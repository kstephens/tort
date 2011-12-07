;; (set! &trace 9)
(load "compiler/lib/environment.scm")
(load "compiler/lib/debug.scm")
(load "compiler/lib/label.scm")
(load "compiler/lib/assembler.scm")

(let ( 
              ;; x68_64
       (arg-regs   '#(%rdi %rsi %rdx %rcx %r8 %r9))
       (word-size    ('get &root 'WORD_SIZE))
       (tag-bits     ('get &root 'TAG_BITS))
       (locative-tag ('get ('get &root 'tagged_mtables) <locative>))
       (fixnum-tag   ('get ('get &root 'tagged_mtables) <fixnum>))
       (unspec '(&q ()))
       
       (set-cons! (lambda (dst src)
		    (set-car! dst (car src))
		    (set-cdr! dst (cdr src))
		    dst))
       ;; (caddddr (lambda (x) (car (cdr (cdr (cdr (cdr x)))))))
       )

  ;; 1) Rewrite scheme to canonical forms:
  ;;
  ;; (quote e) => (&q e)
  ;; SYM => (&v SYM): Unbound variable
  ;; (&_ expr): Void expression, result not used.
  ;; (&extern SYM): external linkage.
  ;; (&c f . args); Call f with args.
  ;; (&L expr): Create locative for expr value.
  ;; (&l l-expr): Get locative's value.
  ;; (&l! l-expr v-expr): Set locative's value.
  ;; (begin) => UNSPEC
  ;; (begin e) => e
  ;; (begin e . exprs) => (&b e . exprs)
  ;; (lambda formals . b) => (&lambda env (begin . b))
  (letrec ((f 
    (lambda (e)
      (cond
	((symbol? e) `(&v ,e))
	((pair? e)
	  (case (car e)
	    ((quote)  `(&q ,(cadr e)))
	    ((set!)   `(&set! ,(cadr e) ,(f (caddr e))))
	    ((if)     `(&if ,(f (cadr e)) ,(f (caddr e))
			 ,(if (pair? (cdddr e))
			    (f (cadddr e))
			    unspec)))
	    ((set!)   `(&set! (&v ,(cadr e)) ,(f (caddr e))))
	    ((lambda)   ; (lambda formals . body) => (&lambda 'formals (begin . body) 'name)
	      `(&lambda  ,`(&q ,(cadr e))  ,(f `(begin ,@(cddr e)))))
	    ((&c-func)  ; (&c-func name formals . body) => (&&c-func 'formals (begin . body) 'name)
	      `(&&c-func ,`(&q ,(caddr e)) ,(f `(begin ,@(cdddr e))) (&q ,(cadr e))))
	    ((let)      ; (let bindings . body) => (&let #f bindings (begin . body))
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
	((eq? (car e) '&extern) e)
	((eq? (car e) '&lambda)  ; (&lambda formals body)
	  `(&lambda  ,(cadr e) ,(f (caddr e))))
	((eq? (car e) '&&c-func) ; (&&c-func formals body name)
	  `(&&c-func ,(cadr e) ,(f (caddr e)) ,(f (cadddr e))))
	((eq? (car e) '&let)     ; (&let bindings body)
	  (if (null? (cadr e))   ; (&let () body)
	    (f (caddr e))
	    `(&let ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (cadr e))
	       ,(f (caddr e)))))
	((and                    ; (&c (&q SYMBOL) ...) => 
	   (eq? (car e) '&c)  ; (&send (&q SYMBOL) ...)
	   (pair? (cadr e)) (eq? (car (cadr e)) '&q)
	   (symbol? (cadr (cadr e))))
	  `(&send ,@(map f (cdr e))))
	((and 
	   (eq? (car e) '&c)                       ; (&c
	   (pair? (cadr e)) (eq? (caadr e) '&lambda)) ;   (&lambda
	  (let ( (b '()) 
		 (vars  (cadr  (cadr e)))            ;     vars
		 (body  (caddr (cadr e)))            ;     body)
		 (inits (cddr e))                     ;  . inits)
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
	  ((&q &v &extern) )
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
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e))
      )
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
	  ((&q &extern) )
	  ((&v &set!)
	    (let ((b ('lookup-or-add-global env (cadr e))))
	      ;; (display "   b = ")(write b)(newline)
	      (set-car! (cdr e) b)
	      ('referenced?= b #t)  ;; TODO: add reference count.
	      ;; If this binding's closure is not in our closure,
	      ;; mark the binding as closed-over.
	      ;; b is exported from its closure and
	      ;; imported into this.
	      (if (not (eq? ('closure env) ('closure ('env b))))
		(begin
		  ('closed-over?= b #t)
		  ('set ('exports ('env b)) b b)
		  ('set ('imports env) b b))
	      )))
	  ((&lambda &&c-func)  ;; (&lambda formals body)
	    (f (cadr e) (caddr e)))
	  ((&let)  ;; (&let env body) 
	    (for-each (lambda (b) (f env ('init b)))
	      ('binding-list (cadr e)))
	      (f (cadr e) (caddr e)))
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
	  ((&q &v &extern) )
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
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:pass-5 env e)
      (f env e))
    ) ;; letrec

  ;; 6) Emit preliminary instructions.
  (define-struct isns
    (name      #f)
    (isns      (vector))
    (labels    ('new <map>))
    (label-id  0)
    (literals '())
    (pass      'unknown)
    (env       #f)
    (main      #f)
    )
  (define-method isns ('lisp_write self port)
    (display "#<isns >" port))
  (define-method isns ('emit self . isns)
    (let ((out ('isns self)))
      (for-each 
	(lambda (x)
	  (if (not (pair? x)) (error "emit: not a pair: %O" x))
	  (display "    | ")(display x)(newline)
	  ('add out x)) 
	isns))
    self)
  (define-method isns ('label self . name)
    (set! name (if (pair? name) (car name) #f))
    ;; (debug "isns.label" name)
    (let ((id ('label-id self))
	   (label ('new label)))
      ('label-id= self (+ id 1))
      ('id= label id)
      (if name
	(begin
	  ('scope= label 'global)
	  (set! name name))
	(set! name (string->symbol (string-append "L" (number->string id)))))
      ('name= label name)
      ('set ('labels self) name label)
      label))
  
  (letrec (
    (literal (lambda (x) `(&$ ,('_to_c_literal x))))
    (loc 
      (lambda (o b)
	(let ((l ('loc b)))
	  (if (locative? l)
	    (begin 
	      ('emit o `(movq ,(literal ('_to_c_ptr l)) (&r %rdx)))
	      `(&o 0 (&r %rdx)))
	    (if ('closed-over? b)
	      `(&o ,(- locative-tag) ,l)
	      l)))))
    (f 
      (lambda (o e)
	(display "  6 e = ")(write e)(newline)
	(case (car e)
	  ((&_) 
	    (case (car (cadr e))
	      ((&q &v)  )
	      (else     (f o (cadr e)))))
	  ((&q) ('emit o `(movq ,(literal (cadr e)) (&r %rax))))
	  ((&b) (for-each (lambda (e) (f o e)) (cdr e)))
	  ((&v) 
	    ('emit o `(movq ,(loc o (cadr e)) (&r %rax))))
	  ((&l)  ;; get locative's value.
	    (f o (cadr e))
	    ('emit o '(movq (&o ,(- locative-tag) (&r %rax)) (&r %rax))))
	  ((&l!) ;; set locative's value.
	    (f o (cadr e))
	    ('emit o '(movq (&r %rax) (&r %rdx)))
	    (f o (caddr e))
	    ('emit o '(movq (&r %rax) (&o ,(- locative-tag) (&r %rdx)))))
	  ((&L) ;; create new locative to value.
	    (f o (cadr e))
	    ('emit o 
	      '(movq (&r %rax) (&r &rdi))
	      '(call _tort_locative_new_value)))
	  ((&s!) ;; non-locative set! used for initializers.
	    (f o (caddr e))
	    ('emit o `(movq %rax ,('loc (cadr e)))))
	  ((&set!)
	    (f o (caddr e))
	    ('emit o `(movq %rax ,(loc o (cadr e)))))
	  ((&if)
	    (let ((Lf ('label o))
		  (Le ('label o)))
	      (f o (cadr e))
	      ('emit o `(cmpq ,(literal #f) (&r %rax)) `(je ,Lf))
	      (f o (caddr e))
	      ('emit o `(jmp ,Le) '(.align 4) `(.label: ,Lf))
	      (f o (cadddr e))
	      ('emit o `(.label: ,Le))
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
		    `(.globl ,this)
		    `(.label: ,this))))
	      ('emit o
		`(.label: ,start)
		'(pushq (&r %rsp))
		'(movq  (&r %rsp) (&r %rbp)))
	      ;; Allocate stack space.
	      (let ((alloc-offset ('alloc-offset-max env)))
		(if (< alloc-offset 0)
		  (begin
		    ;; Align to 16-byte boundary.
		    (set! alloc-offset (* (/ (+ (- alloc-offset) 15) 16) 16))
		    ;; (debug "  6 " alloc-offset)
		    ('alloc-offset-max= env (- alloc-offset))
		    ('emit o `(subq ,alloc-offset (&r %rsp))))))
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
		    (f o `(&s! ,b (&L ,('loc b))))
		    ))
		('binding-list env))
	      ;; Emit body instructions.
	      (f o (caddr e))
	      ('emit o
		`(.label: ,return)
		'(leave)
		'(ret)
		`(.label: ,end))
	      ;; Append parent instructions after this function.
	      ('append ('isns o) old-isns)
	      ;; FIXME: ('emit o `(movq ,start (&r %rax)))
	      ))
	  ((&let)
	    (for-each 
	      (lambda (b)
		(if ('init b)
		  (if ('referenced? b)
		    (let ((init 
			    (if ('closed-over? b) `(&L ,('init b)) ('init b))))
		      (f o `(&s! ,b ,init)))
		    (f o ('init b)))))
	      ('binding-list (cadr e)))
	    (f o (caddr e)))
	  ((&c)
	    (let ((stack-args-size 0) (nargs 0))
	      ;; Push args onto stack.
	      (for-each
		(lambda (e)
		  (f o e)
		  ('emit o '(pushq (&r %rax)))
		  (set! nargs (+ nargs 1))
		  (set! stack-args-size (+ stack-args-size word-size))) 
		(reverse (cddr e)))
	      ;; Load function into %rax.
	      (f o (cadr e))
	      ;; Pull register args off stack.
	      (let ((i 0))
		(while (and (< i nargs) (< i (vector-length arg-regs)))
		  ('emit o `(popq ,(vector-ref arg-regs i)))
		  (set! stack-args-size (- stack-args-size word-size))
		  (set! i (+ i 1))))
	      ;; Load %rbx with argc.
	      ('emit o `(movq (&$ ,nargs) (&r %rbx)))
	      ;; Call function in %rax.
	      ('emit o '(call* (&r %rax)))
	      ;; Pop remaining stack args.
	      (if (> stack-args-size 0)
		('emit o `(addq ,stack-args-size %rsp)))))
	  (else
	    (for-each (lambda (e) (f o e)) (cdr e))))
	e)))
    (define (compiler:pass-6 env e)
      (let* ((o ('new isns)))
	(set! e `(&let ,env ,e))
	(f o e)
	o))
    ) ;; letrec

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tests
  ;;

  (define (compile expr env)
    (let ((result expr))
      (if (not env) (set! env ('new environ)))
      (display "\nexpr     = ")(write expr)(newline)
      
      (set! result (compiler:pass-1 result))
      (display "result 1 = ")(write result)(newline)
      (set! result (compiler:pass-2 result))
      (display "result 2 = ")(write result)(newline)
      (set! result (compiler:pass-3 env result))
      (display "result 3 = ")(write result)(newline)
      (set! result (compiler:pass-4 env result))
      (display "result 4 = ")(write result)(newline)
      (set! result (compiler:pass-5 env result))
      (display "result 5 = ")(write result)(newline)
      
      (set! result (compiler:pass-6 env result))
      (display "result 6 = ")(write result)(newline)
      (let ((asm ('new assembler)))
	('isns= asm result)
	('output-name= asm "_tort_x_test")
	('body asm)
	('assemble asm 'verbose)
	)

      result))

  (let
    ((t 
       (lambda (expr expect)
	 (let ((result expr))
	   (set! result (compile result #f))
	   (display "code = ")(newline)
	   (vector-for-each (lambda (x)
			      (display "    | ")(display x)(newline))
	     ('isns result))
	   (if (and #f (not (equal? result expect)))
	     (display "expr     = ")(write expr)(newline)
	     (display "result   = ")(write result)(newline)
	     (display "expect   = ")(write expect)(newline)
	     (error "result != expect"))
	   )
	 #f)))
    (t '1 '(&q 1))
    (t '(quote (a b))  '(&q (a b)))
    (t '(begin)        '(&q ()))
    (t '(begin 1)      '(&q 1))
    (t '(begin 1 'y)   '(&b (&_ (&q 1)) (&q y)))
    (t '(begin 1 2 3) '(&b (&_ (&q 1)) (&_ (&q 2)) (&q 3)))
    (t '(if 1 2)      '(&if (&q 1) (&q 2) (&q ())))
    (t '(if 1 2 3)    '(&if (&q 1) (&q 2) (&q 3)))
    (t '(lambda (a b) 1 a)
      '(&lambda #f (a b) (&b (&_ (&q 1)) (&v a))))
    (t '(let ((a 1) (b 1)) 1 a)
      '(&let #f ((a 1) (b 1)) (&b (&_ (&q 1)) (&v a))))
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

