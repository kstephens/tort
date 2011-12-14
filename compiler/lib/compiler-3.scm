;; (set! &trace 9)
(load "compiler/lib/environment.scm")
(load "compiler/lib/debug.scm")
(load "compiler/lib/label.scm")
(load "compiler/lib/assembler.scm")

(let ( 
              ;; x68_64
       (arg-regs     '#(%rdi %rsi %rdx %rcx %r8 %r9))
       (word-size    ('get &root 'WORD_SIZE))
       (tag-bits     ('get &root 'TAG_BITS))
       (locative-tag ('get ('get &root 'tagged_mtables) <locative>))
       (fixnum-tag   ('get ('get &root 'tagged_mtables) <fixnum>))
       (unspec `(&q ,(if #f #f)))
       (ops          ('new <map>))
       )
  (for-each (lambda (o) ('set ops (car o) o)) 
    `(
       (&INV (notq ,car))
       (&NEG (negq ,car))
       (&ADD (addq ,cadr ,car))
       (&SUB (subq ,cadr ,car))
       (&MUL (imulq ,cadr ,car))
       (&DIV 
	 (movq ,car (&r %rdx))
	 (sarq (&$ 63) (&r %rdx))
	 (idivq ,cadr))
       (&AND (andq ,cadr ,car))
       (&OR  (orq  ,cadr ,car))
       (&XOR (xorq ,cadr ,car))
       (&MOD 
	 (movq ,car (&r %rdx))
	 (sarq (&$ 63) (&r %rdx))
	 (idivq ,cadr)
	 (movq (&r %rdx) (&r %rax)))
       (&LSH
	 (movl ,cadr (&r %ecx))
	 (movq ,car (&r %rax))
	 (salq (&r %cl) (&r %rax)))
       (&RSH
	 (movl ,cadr (&r %ecx))
	 (movq ,car (&r %rax))
	 (sarq (&r %cl) (&r %rax)))
       (&NOT 
	 (cmpq ($& 0) ,car)
	 (sete (&r %al))
	 (movzbl (&r %al) (&r %eax))
	 )
       ;; LOR, LAND ;; FIXME
       (&EQ
	 (cmpq ,cadr ,car)
	 (sete (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       (&NE
	 (cmpq ,cadr ,car)
	 (setne (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       (&LT
	 (cmpq ,cadr ,car)
	 (setl (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       (&GT
	 (cmpq ,cadr ,car)
	 (setg (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       (&LE
	 (cmpq ,cadr ,car)
	 (setle (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       (&GE
	 (cmpq ,cadr ,car)
	 (setge (&r %al))
	 (movzbl (&r %al) (&r %eax)))
       ))

  ;; 1) Rewrite scheme to canonical forms:
  ;;
  ;; (quote e) => (&q e)
  ;; SYM => (&v SYM): Unbound variable
  ;; (&e env expr): binds expr in env.
  ;; (&_ expr): Void expression, result of expr not used.
  ;; (&extern SYM) => (&g binding): external global binding.
  ;; (&c f . args); Call C function f with args.
  ;; (&p expr): Create boxed pointer for expr value.
  ;; (&P p-expr): Get boxed pointer's value.
  ;; (&L! l-expr v-expr): Set locative's value.
  ;; (&l expr): Create locative for expr value.
  ;; (&L l-expr): Get locative's value.
  ;; (&L! l-expr v-expr): Set locative's value.
  ;; (&stack-alloc size): allocate size bytes on stack.
  ;; (begin) => UNSPEC
  ;; (begin e) => e
  ;; (begin e . exprs) => (&b e . exprs)
  ;; (lambda formals . b) => (&lambda env (begin . b))
  (letrec ((f 
    (lambda (e)
      ;; (display "  1 e = ")(write e)(newline)
      (cond
	((environ? e) e)
	((symbol? e) `(&v ,e))
	((pair? e)
	  (case (car e)
	    ((quote)  `(&q ,(cadr e)))
	    ((set!)   `(&set! ,(cadr e) ,(f (caddr e))))
	    ((eq?)    `(&eq? ,@(map f (cdr e))))
	    ((if)     `(&if ,(f (cadr e)) ,(f (caddr e))
			 ,(if (pair? (cdddr e))
			    (f (cadddr e))
			    unspec)))
	    ((while)  `(&while ,(f (cadr e)) ,(f `(begin ,@(cddr e)))))
	    ((set!)   `(&set! (&v ,(cadr e)) ,(f (caddr e))))
	    ((lambda)   ;; (lambda formals . body) => (&lambda 'formals (begin . body) 'name)
	      `(&lambda  ,`(&q ,(cadr e))  ,(f `(begin ,@(cddr e)))))
	    ((&c-func)  ;; (&c-func name formals . body) => (&&c-func 'formals (begin . body) 'name)
	      `(&&c-func ,`(&q ,(caddr e)) ,(f `(begin ,@(cdddr e))) (&q ,(cadr e))))
	    ((let)      ;; (let bindings . body) => (&let #f bindings (begin . body))
	      `(&let     ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (cadr e))
		            ,(f `(begin ,@(cddr e)))))
	    ((begin)  (cond
			((null? (cdr e))   unspec)
			((null? (cddr e))  (f (cadr e)))
			(else
			  (let* ((c (cons #f '())) (r c))
			    (set! e (cdr e))
			    (while (pair? (cdr e))
			      (set-cdr! c (cons `(&_ ,(f (car e))) '()))
			      (set! c (cdr c)) (set! e (cdr e)))
			    (set-cdr! c (cons (f (car e)) '()))
			    `(&b ,@(cdr r))))))
	    ((or)  (cond
		     ((null? (cdr e))   '(%q #f))
		     ((null? (cddr e))  (f (cadr e)))
		     (else `(&or ,@(map f (cddr e))))))
	    ((and)  (cond
		     ((null? (cdr e))   '(%q #t))
		     ((null? (cddr e))  (f (cadr e)))
		     (else `(&and ,@(map f (cddr e))))))
	    ((&e)      `(&e (&q ,(cadr e)) ,(f (caddr e))))
	    ((&extern &stack-alloc) `(,(car e) (&q ,(cadr e))))
	    (else     (if (and (symbol? (car e)) (equal? (string-ref (symbol->string (car e)) 0) #\&))
			`(,(car e) ,@(map f (cdr e)))
			`(&c    ,@(map f e))))
	    ))
	(else `(&q ,e))))))
    (define (compiler:pass-1 e)
      (f e))
    ) ;; letrec
  
  ;; 2) Rewrite ('sym ...) => (&send 'sym ...)
  ;;    Rewrite ((lambda formals . body) . inits) => (let bindings . body) 
  ;;    Rewrite (let () . body) => (begin . body)
  (letrec ((f 
    (lambda (e)
      ;; (display "  2 e = ")(write e)(newline)
      (cond
	((environ? e) e)
	((eq? (car e) '&q) e)
	((eq? (car e) '&v) e)
	((eq? (car e) '&r) e)
	((eq? (car e) '&&extern) e)
#|
	((and 
	   (eq? (car e) '&eq?)  ;; (&eq? 'x 'y)
	   (eq? (car (cadr e)) '&q)
	   (eq? (car (caddr e)) '&q))
	  (f `(&q ,(eq? (cadr (cadr e)) (cadr (caddr e))))))
	((and 
	   (eq? (car e) '&if)  ;; (&if '??? ...)
	   (eq? (car (cadr e)) '&q))
	  (f (if (cadr (cadr e)) (caddr e) (cadddr e))))
|#
	((eq? (car e) '&lambda)  ;; (&lambda formals body)
	  `(&lambda  ,(cadr e) ,(f (caddr e))))
	((eq? (car e) '&&c-func) ;; (&&c-func formals body name)
	  `(&&c-func ,(cadr e) ,(f (caddr e)) ,(f (cadddr e))))
	((eq? (car e) '&let)     ;; (&let bindings body)
	  (if (null? (cadr e))   ;; (&let () body)
	    (f (caddr e))
	    `(&let ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (cadr e))
	       ,(f (caddr e)))))
	((and                    ;; (&c (&q SYMBOL) ...) => 
	   (eq? (car e) '&c)     ;;   (&send (&q SYMBOL) ...)
	   (pair? (cadr e)) (eq? (car (cadr e)) '&q)
	   (symbol? (cadr (cadr e))))
	  `(&send ,@(map f (cdr e))))
	((and 
	   (eq? (car e) '&c)                          ;; (&c
	   (pair? (cadr e)) (eq? (caadr e) '&lambda)) ;;   (&lambda
	  (let ( (b '()) 
		 (vars  (cadr  (cadr e)))             ;;      vars
		 (body  (caddr (cadr e)))             ;;      body)
		 (inits (cddr e))                     ;;      . inits)
		 )
	    (while (or (pair? vars) (pair? inits))
	      (set! b (cons (list (car vars) (f (car inits))) b))
	      (set! vars (cdr vars))
	      (set! inits (cdr inits)))
	    `(&let ,b ,(f body))))
	(else
	  ;; (debug "  2 Default" e)
	  `(,(car e) ,@(map f (cdr e)))))
      )))
    (define (compiler:pass-2 e)
      (f e))
    ) ;; letrec

  ;; 3) Create environments.
  (letrec ( 
    (f 
      (lambda (env e)
	;;(display "  3 e = ")(write e)(newline)
	(case (car e)
	  ((&q &v &&extern) )
	  ((&lambda &&c-func) ; (&lambda  'formals body) => (&lambda env body) 
	    (let ((subenv ('subenv env)))
	      ('closure= subenv e)
	      ('name= subenv (and (eq? (car e) '&&c-func) (cadr (cadddr e))))
	      ('add-formals subenv (cadr (car (cdr e))))
	      (set-car! (cdr e) subenv)
	      (f subenv (caddr e)) ;; body
	      ))
	  ((&let)  ; (&let bindings body) => (&let env body)
	    (let ((subenv ('subenv env)))
	      ('_alloc-env= subenv ('alloc-env env)) ;; allocate from
	      ;; same closure as parent environment, this is just a separate namespace. 
	      ('closure= subenv ('closure env)) 
	      (for-each (lambda (b)
			  ('add subenv ('new env-binding 
					 'name (car b) 
					 'init (f env (cadr b))
					 )))
		(car (cdr e)))
	      (set-car! (cdr e) subenv)
	      (f subenv (caddr e)) ;; body
	      ))					
	  ((&e)  ;; (&e (&q env) expr) =>  
	    (f (cadr (cadr e)) (caddr e)))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:pass-3 env e)
      (f env e))
    ) ;; letrec

  ;; 4) Find referenced variables.
  ;; (&v SYM) => (&v ENV-BINDING): Bound variable
  (letrec (
    (f 
      (lambda (env e)
	;; (display "  4 e = ")(write e)(newline)
	(case (car e)
	  ((&q &g) )
	  ((&v &set!)
	    (let ((b ('lookup-or-add-global env (cadr e))))
	      ;; (display "   b = ")(write b)(newline)
	      (set-car! (cdr e) b)
	      ('referenced-from! b env)
	      ))
	  ((&lambda &&c-func)  ;; (&lambda formals body)
	    (f (cadr e) (caddr e)))
	  ((&let)  ;; (&let env body) 
	    (for-each (lambda (b) (f env ('init b)))
	      ('binding-list (cadr e)))
	      (f (cadr e) (caddr e)))
	  ((&e)  ;; (&e (&q env) expr) =>  
	    (f (cadr (cadr e)) (caddr e)))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:pass-4 env e)
      (f env e))
    ) ;; letrec

  ;; 5) Allocate referenced variables.
  (letrec ( 
    (f 
      (lambda (env e)
	;; (display "  5 e = ")(write e)(newline)
	(case (car e)
	  ((&q &v &g) )
	  ((&lambda &&c-func)  
	    ('allocate-bindings (cadr e))
	    (f (cadr e) (caddr e)))
	  ((&let)
	    (let ((subenv (cadr e))
		  (alloc-offset ('alloc-offset ('alloc-env env))))
	      ;; (debug "&let " (cadr e))
	      ('allocate-bindings subenv)
	      (for-each (lambda (b) (f env ('init b)))
		('binding-list subenv))
	      (f subenv (caddr e))
	      ('alloc-offset= ('alloc-env env) alloc-offset)
	      ))
	  ((&e)  ;; (&e (&q env) expr) =>
	    (f (cadr (cadr e)) (caddr e)))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:pass-5 env e)
      (f env e))
    ) ;; letrec

  ;; 6) Emit preliminary instructions.
  (letrec (
    (literal (lambda (x) `(&& ,('_word x))))
    (loc 
      (lambda (o b)
	(let ((l ('loc b)))
	  (cond
	    ((locative? l) ;; a known global.
	      (begin 
		('emit o `(movq ,(literal ('_to_c_ptr l)) (&r %rdx)))
		`(&o 0 (&r %rdx))))
#|
	    (('export-index b)
	      `(&o ,(- locative-tag) ,l))
|#
	    (('closed-over? b)
	      `(&o ,(- locative-tag) ,l))
	    (else
	      l)))))
    (f 
      (lambda (env o e)
	;; (display "  6 e = ")(write e)(newline)
	(case (car e)
	  ((&_) 
	    (case (car (cadr e))
	      ((&q &v)  )
	      (else     (f env o (cadr e)))))
	  ((&q) ('emit o `(movq ,(literal (cadr e)) (&r %rax))))
	  ((&b) (for-each (lambda (e) (f env o e)) (cdr e)))
	  ((&&extern) ('emit o `(movq ,('label o (cadr (cadr e))) (&r %rax))))
	  ((&v) 
	    ('emit o `(movq ,(loc o (cadr e)) (&r %rax))))
	  ((&&)  ;; get the address of a variable.
	    ('emit o `(leaq ,('loc (cadr (cadr e))) (&r %rax))))
	  ((&i)  ;; box an int.
	    (f env o (cadr e))
	    ('emit o 
	      '(salq (&$ 2) (&r %rax))
	      '(orq  (&$ 1) (&r %rax))))
	  ((&I)  ;; unbox an int.
	    (f env o (cadr e))
	    ('emit o '(sarq (&$ 2) (&r %rax))))
	  ((&p)  ;; box a pointer.
	    (f env o (cadr e))
	    ('emit o 
	      '(movq (&r %rax) (&r %rdi))
	      '(callq _tort_ptr_new)))
	  ((&P)  ;; unbox a pointer.
	    (f env o (cadr e))
	    ('emit o '(movq (&o 0 (&r %rax)) (&r %rax))))
	  ((&L)  ;; get locative's value.
	    (f env o (cadr e))
	    ('emit o '(movq (&o ,(- locative-tag) (&r %rax)) (&r %rax))))
	  ((&l!) ;; set locative's value.
	    (f env o (cadr e))
	    ('emit o '(pushq (&r %rax)))
	    (f env o (caddr e))
	    ('emit o '(popq (&r %rdx)))
	    ('emit o '(movq (&r %rax) (&o ,(- locative-tag) (&r %rdx)))))
	  ((&l) ;; create new locative to value.
	    (f env o (cadr e))
	    ('emit o 
	      '(movq (&r %rax) (&r &rdi))
	      '(callq _tort_locative_new_value)))
	  ((&s!) ;; non-locative set! used for initializers.
	    (f env o (caddr e))
	    ('emit o `(movq %rax ,('loc (cadr e)))))
	  ((&set!)
	    (f env o (caddr e))
	    (cond
	      ((and (pair? (cadr e)) (eq? (car (cadr e)) '&r))
		('emit o `(movq %rax ,(cadr (cadr e))))))
	      (else
		('emit o `(movq %rax ,(loc o (cadr e))))))
	  ((&eq?) 
	    (let ((Lf ('label o))
		  (Le ('label o)))
	      (f env o (caddr e))
	      ('emit o '(pushq (&r %rax)))
	      (f env o (cadr e))
	      ('emit o
		'(pop %rdx)
		'(cmpq (&r %rdx) (&r %rax))
		`(jne ,Lf)
		`(movq ,(literal #t) (&r %rax))
		`(jmp ,Le)
		'(.align 4) `(.label: ,Lf)
		`(movq ,(literal #f) (&r %rax))
		'(.align 4) `(.label: ,Le))))
	  ((&if)
	    (let ((Lf ('label o))
		  (Le ('label o)))
	      (f env o (cadr e))
	      ('emit o 
		`(movq ,(literal #f) (&r %rdx))
		'(cmpq (&r %rdx) (&r %rax))
		`(je ,Lf))
	      (f env o (caddr e))
	      ('emit o `(jmp ,Le) '(.align 4) `(.label: ,Lf))
	      (f env o (cadddr e))
	      ('emit o `(.label: ,Le))
	      ))
	  ((&while)
	    (let ((Lb ('label o))
		  (Le ('label o)))
	      ('emit o '(.align 4) `(.label: ,Lb))
	      (f env o (cadr e))
	      ('emit o
		`(movq ,(literal #f) (&r %rdx))
		'(cmpq (&r %rdx) (&r %rax))
		`(je ,Le))
	      (f env o (caddr e))
	      ('emit o `(jmp ,Lb))
	      ('emit o '(.align 4) `(.label: ,Le))
	      ))
	  ((&or)
	    (let ((Le ('label o)))
	      (set! e (cdr e))
	      (while (not (null? e))
		(f env o (car e))
		('emit o 
		  `(movq ,(literal #f) (&r %rdx))
		  '(cmpq (&r %rdx) (&r %rax))
		  `(jne ,Le))
		(set! e (cdr e)))
	      ('emit o '(.align 4) `(.label: ,Le))
	      ))
	  ((&and)
	    (let ((Le ('label o)))
	      (set! e (cdr e))
	      (while (not (null? e))
		(f env o (car e))
		('emit o 
		  `(movq ,(literal #f) (&r %rdx))
		  '(cmpq (&r %rdx) (&r %rax))
		  `(je ,Le))
		(set! e (cdr e)))
	      ('emit o '(.align 4) `(.label: ,Le))
	      ))
	  ((&lambda &&c-func)
	    (let* ((env (cadr e))
		   (this   (or ('name env) ('name o)))
		   (start  ('label o))
		   (return ('label o))
		   (end    ('label o))
		   (old-isns ('isns o)))
	      ;; Start new isn vector.
	      ('isns= o (vector))
	      ('emit o
		'(.text)
		'(.align 4))
	      (if this
		(begin
		  (set! this ('label o this))
		  ('emit o
		    `(.globl  ,this)
		    `(.label: ,this))))
	      ('emit o
		`(.label: ,start)
		'(pushq (&r %rbp))
		'(movq  (&r %rsp) (&r %rbp)))
	      ;; Allocate stack space.
	      (let ((alloc-offset ('alloc-offset-max env)))
		(if (< alloc-offset 0)
		  (begin
		    ;; Align to 16-byte boundary.
		    (set! alloc-offset (* (/ (+ (- alloc-offset) 15) 16) 16))
		    ;; (debug "  6 " alloc-offset)
		    ('alloc-offset-max= env (- alloc-offset))
		    ('emit o `(subq (&$ ,alloc-offset) (&r %rsp))))))
	      ;; Move register arguments to stack.
	      (for-each 
		(lambda (b)
		  (let ((reg ('reg b)) (loc ('loc b)))
		    ;; If binding is in register and there is a stack location allocated, 
		    ;;   move it to stack.
		    (if (and reg loc)
		      ('emit o `(movq ,reg ,loc)))))
		('binding-list env))
	      ;; Make locatives for closed-over arguments.
	      (for-each
		(lambda (b)
		  (if ('closed-over? b)
		    (begin
		      (f env o `(&s! ,b (&l ,('loc b))))))
		  )
		('binding-list env))
	      ;; Emit body instructions.
	      (f env o (caddr e))
	      ('emit o
		`(.label: ,return)
		'(leave)
		'(ret)
		`(.label: ,end))
	      ;; Append parent instructions after this function.
	      ('append ('isns o) old-isns)
	      ;; Emit ptr expression for this function.
	      ('emit o 
		`(movq ,start (&r %rdi))
		'(callq _tort_ptr_new))
	      ))
	  ((&let)
	    (let ((subenv (cadr e)))
	      (for-each 
		(lambda (b)
		  (if ('init b)
		    (if ('referenced? b)
		      (let ((init 
			      (if ('closed-over? b) `(&l ,('init b)) ('init b))))
			(f subenv o `(&s! ,b ,init)))
		      (f subenv o ('init b)))))
		('binding-list subenv))
	      (f subenv o (caddr e))
	      (if (> ('stack-alloc subenv) 0)
		('emit o `(addq (&$ ,('stack-alloc subenv)) (&r %rsp))))))
	  ((&stack-alloc)
	    (let ((size (cadr (cadr e))))
	      (set! size (* (/ (+ size (- word-size 1)) word-size) word-size)) ;; align
	      ('stack-alloc= env (+ ('stack-alloc env) size))
	      ('emit o
		'(movq (&r %rsp)  (&r %rax))
		`(subq (&$ ,size) (&r %rsp)))))
	  ((&c) ;; C function call.
	    (let ((stack-args-size 0) (nargs 0))
	      ;; Push args onto stack.
	      (for-each
		(lambda (e)
		  (f env o e)
		  ('emit o '(pushq (&r %rax)))
		  (set! nargs (+ nargs 1))
		  (set! stack-args-size (+ stack-args-size word-size))) 
		(reverse (cddr e)))
	      ;; Load function into %rax.
	      (f env o (cadr e))
	      ;; Pull register args off stack.
	      (let ((i 0))
		(while (and (< i nargs) (< i (vector-length arg-regs)))
		  ('emit o `(popq ,(vector-ref arg-regs i)))
		  (set! stack-args-size (- stack-args-size word-size))
		  (set! i (+ i 1))))
	      ;; Load %rbx with argc.
	      ('emit o `(movq (&$ ,nargs) (&r %rbx)))
	      ;; Call function in %rax.
	      ('emit o '(callq* (&r %rax)))
	      ;; Pop remaining stack args.
	      (if (> stack-args-size 0)
		('emit o `(addq (&$ ,stack-args-size) (&r %rsp))))))
	  ((&e) (f (cadr (cadr e)) o (caddr e)))
	  ((&o)
	    (f env o (cadr e))
	    ('emit o '(pushq (&r %rax)))
	    (f env o (caddr e))
	    ('emit o 
	      '(popq (&r %rdx)) 
	      '(addq (&r %rdx) (&r %rax))
	      '(movq (&o 0 (&r %rax)) (&r %rax))))
	  ((&r)
	    ('emit o '(movq ,e (&r %rax))))
	  (else
	    (for-each (lambda (e) (f env o e)) (cdr e))))
	e)))
    (define (compiler:pass-6 env e)
      (let* ( (isns ('new isns)))
	(f env isns e)
	isns))
    ) ;; letrec

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tests
  ;;

  (define (compile expr env)
    (let ((result expr)
	  (asm ('new assembler)))
      (if (not env) (set! env ('new environ 'arg-regs arg-regs 'word-size word-size)))
      (display "\nexpr     = ")(write expr)(newline)
      (set! result (macro-expand result))
      (display "mac-xpnd = ")(write result)(newline)
      (set! result `(&e ,env
		      (&c-func ,('init-symbol asm) () ,result)))
      (display "result 0 = ")(write result)(newline)
      ;; (set! &trace 1)
      (set! result (compiler:pass-1 result))
      ;; (set! &trace 0)
      (display "result 1 = ")(write result)(newline)
      (set! result (compiler:pass-2 result))
      (display "result 2 = ")(write result)(newline)
      (set! result (compiler:pass-3 env result))
      (display "result 3 = ")(write result)(newline)
      ;; (set! &trace 1)
      (set! result (compiler:pass-4 env result))
      ;; (set! &trace 0)
      (display "result 4 = ")(write result)(newline)
      (set! result (compiler:pass-5 env result))
      (display "result 5 = ")(write result)(newline)
      
      ;; ('output-name= asm "_tort_x_test") ;; TESTING
      (set! result (compiler:pass-6 env result))
      (display "result 6 = ")(write result)(newline)
      ('isns= asm result)
      ('assemble asm)

      (display "isns = ")(newline)
      (vector-for-each (lambda (x)
			 (display "    | ")(display x)(newline))
	('isns ('isns asm)))

      (display "asm = \n")(display ('asm asm))(newline)

      (set! result ('load asm))
      result))

  (let
    ((t 
       (lambda (expr expect)
	 (let ((result expr) 
		(func #f))
	   (display "\n===================================================================\n");
	   (set! func (compile result #f))
	   (set! result ('_ccall func))
	   (display "expr     = ")(write expr)(newline)
	   (display "result   = ")(write result)(newline)
	   (if (not (equal? result expect))
	     (display "expr     = ")(write expr)(newline)
	     (display "result   = ")(write result)(newline)
	     (display "expect   = ")(write expect)(newline)
	     (error "result != expect"))
	   )
	 #f)))
    (define (void x))
    (void ('load <dynlib> "/usr/lib/libc")) ;; printf
    (t '1 '1)
    (t '(quote (a b))  '(a b))
    (t '(begin)        (if #f #f))
    (t '(begin 1)      '1)
    (t '(begin 1 'y)   'y)
    (t '(begin 1 2 3)  '3)
    (t '(eq? 1 2)      '#f)
    (t '(eq? 1 1)      '#t)
    (t '(if 1 2)       '2)
    (t '(if #f #f)     (if #f #f))
    (t '(if #f 2 3)    '3)
    (t '(let ((a 1) (b 2)) 3 a)
      '1)
    (t '(let ((a (&p (&stack-alloc 15))) (b 2)) a b)
      '2)
    (t '(case 'a ((a 1) '1) (else 2))
      '1)
    (t '(lambda (a b) 1 a)
      '(&lambda #f (a b) (&b (&_ (&q 1)) (&v a))))
    (t '(&i 1) '5)
    (t '(&I 5) '1)
    (t '(&i ((&extern tort_prints) (&P "Hello, World!\n")))
      '14)
    (t '(&i ((&extern printf) (&P "Hello, World!\n")))
      '14)
    (t '(foo 1 2)     '(&c (&v foo) (&q 1) (&q 2)))
    (t '(&primitive 1 2) '(&primitive (&q 1) (&q 2)))
    (t '(let ((a 1)) (lambda (x) (+ a x)))
      'HUH?)
    ;; rewrite-2
    (t '((lambda (p1 p2) b) a1 a2)
      '(&let #f ((p2 (&v a2)) (p1 (&v a1))) (&v b)))
    (t '('+ 1 2)
      '(&send (&q +) (&q 1) (&q 2)))
    (t '(&c-func myfunc (a b c) (+ a c))
      #f)
  )
)

