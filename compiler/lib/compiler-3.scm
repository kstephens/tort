; (set! &trace 9)
(load "compiler/lib/environment.scm")
(load "compiler/lib/debug.scm")

(let ( 
              ;; x68_64
       (arg-regs   '#(%rdi %rsi %rdx %rcx %r8 %r9))
       (SP         '%rsp)
       (BP         '%rbp)
       (IP         '%rip)
       (RESULT     '%rax)
       (TMP0       '%rbx)
       (ops '( (MOV movq 2) (SUB subq 2) (ADD addq 2) (OR orq 2) (AND andq 2)
	       (SAR sarq 2) (SAL salq 2)
	       (CMP cmpq 2)
	       (PUSH pushq 1) (POP popq 1)
	       (CALL call 1) (JMP jmp 1) (JNE jne 1) (JE je 1)
	       (LEAVE leave 0) (RET ret 0)))

       (word-size    ('get &root 'WORD_SIZE))
       (tag-bits     ('get &root 'TAG_BITS))
       (locative-tag ('get ('get &root 'tagged_mtables) <locative>))
       (fixnum-tag   ('get ('get &root 'tagged_mtables) <fixnum>))
       (UNSPEC '(&q ()))
       
       (set-cons! (lambda (dst src)
		    (set-car! dst (car src))
		    (set-cdr! dst (cdr src))
		    dst))
       (caddddr (lambda (x) (car (cdr (cdr (cdr (cdr x)))))))
       )
  (set! SP `(&r ,SP))
  (set! BP `(&r ,BP))
  (set! IP `(&r ,IP))
  (map! (lambda (r) `(&r ,r)) arg-regs)

  ;; (&v SYM): Unbound variable
  ;; (&v ENV-BINDING): Bound variable
  ;; (&_ expr): Void expression, result not used.
  ;; (&extern SYM): external linkage.
  ;;
  ;; 1) Rewrite scheme to canonical forms.
  (letrec ((f 
    (lambda (e)
      (cond
	((symbol? e) `(&v ,e))
	((pair? e)
	  (case (car e)
	    ((quote)  `(&q ,(cadr e)))
	    ;; ((set!)   `(&set! ,(cadr e) ,(f (cadr e))))
	    ((if)     `(&if ,(f (cadr e)) ,(f (caddr e))
			 ,(if (pair? (cdddr e))
			    (f (cadddr e))
			    UNSPEC)))
	    ((lambda)  ; ; (lambda formals . body) => (&lambda 'formals (begin . body) 'name)
	      `(&lambda  ,`(&q ,(cadr e))  ,(f `(begin ,@(cddr e)))))
	    ((&c-func) ; (&c-func name formals . body) => (&&c-func 'formals (begin . body) 'name)
	      `(&&c-func ,`(&q ,(caddr e)) ,(f `(begin ,@(cdddr e))) (&q ,(cadr e))))
	    ((let)     ; (let bindings . body) => (&let #f bindings (begin . body))
	      `(&let     ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (cadr e))
		            ,(f `(begin ,@(cddr e)))))
	    ((begin)  (cond
			((null? (cdr e))   UNSPEC)
			((null? (cddr e))  (f (cadr e)))
			(else
			  (let* ((c (cons #f '())) (r c))
			    (set! e (cdr e))
			    (while (pair? (cdr e))
			      (set-cdr! c (cons `(&_ ,(f (car e))) '()))
			      (set! c (cdr c))
			      (set! e (cdr e)))
			    (set-cdr! c (cons (f (car e)) '()))
			    `(&b ,@(cdr r))))))
	    (else     (if (and (symbol? (car e)) (equal? #\& (string-ref (symbol->string (car e)) 0)))
			`(,(car e) ,@(map f (cdr e)))
			`(&call    ,@(map f e))))
	    ))
	(else `(&q ,e))))))
    (define (compiler:rewrite-1 e)
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
	((and                    ; (&call (&q SYMBOL) ...) => 
	   (eq? (car e) '&call)  ; (&send (&q SYMBOL) ...)
	   (pair? (cadr e)) (eq? (car (cadr e)) '&q)
	   (symbol? (cadr (cadr e))))
	  `(&send ,@(map f (cdr e))))
	((and 
	   (eq? (car e) '&call)                       ; (&call
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
	  (debug "  2 Default" e)
	  `(,(car e) ,@(map f (cdr e)))))
      )))
    (define (compiler:rewrite-2 e)
      (f e))
    ) ;; letrec

  ;; 3) Create environments.
  (letrec ( 
    (f 
      (lambda (env e)
	;; (display "  3 e = ")(write e)(newline)
	(case (car e)
	  ((&q &v &extern) )
	  ((&lambda &&c-func) ; (&lambda  'formals body) => (&lambda env body) 
	    (let ((subenv ('subenv env)))
	      ('closure= subenv e)
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
    (define (compiler:rewrite-3 env e)
      (f env e))
    ) ;; letrec

  ;; 4) Find referenced variables.
  (letrec (
    (f 
      (lambda (env e)
	(display "  4 e = ")(write e)(newline)
	(case (car e)
	  ((&q &extern) )
	  ((&v)
	    (let ((b ('lookup-or-add-global env (cadr e))))
	      ;; (display "   b = ")(write b)(newline)
	      (set-car! (cdr e) b)
	      ('referenced?= b #t)
	      ;; If this binding's closure is not in our closure,
	      ;; mark the binding as closed-over.
	      ;; b is exported from its closure and
	      ;; imported into this.
	      (if (not (eq? ('closure env) ('closure ('env b))))
		(begin
		  ('closed-over= b #t)
		  ('set ('exports ('env b)) b b)
		  ('set ('imports env) b b)
		  )
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
    (define (compiler:rewrite-4 env e)
      (f env e))
    ) ;; letrec

  ;; 5) Allocate referenced variables.
  (letrec ( 
    (f 
      (lambda (env e)
	(display "  5 e = ")(write e)(newline)
	(case (car e)
	  ((&q &v &extern) )
	  ((&lambda &&c-func)  
	    ('allocate-bindings (cadr e))
	    (f (cadr e) (caddr e)))
	  ((&let) 
	    (let ((subenv (cadr e))
		  (alloc-offset ('alloc-offset ('alloc-env env))))
	      (debug "&let " (cadr e))
	      ('allocate-bindings subenv)
	      (for-each (lambda (b) (f env ('init b)))
		('binding-list subenv))
	      (f subenv (caddr e))
	      ('alloc-offset= ('alloc-env env) alloc-offset)
	      ))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:rewrite-5 env e)
      (f env e)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tests
  ;;

  (let
    ((t 
       (lambda (expr expect)
	 (let ((result expr) (env ('new environ)))
	   (display "\nexpr     = ")(write expr)(newline)

	   (set! result (compiler:rewrite-1 result))
	   (display "result 1 = ")(write result)(newline)
	   (set! result (compiler:rewrite-2 result))
	   (display "result 2 = ")(write result)(newline)
	   (set! result (compiler:rewrite-3 env result))
	   (display "result 3 = ")(write result)(newline)
	   (set! result (compiler:rewrite-4 env result))
	   (display "result 4 = ")(write result)(newline)
	   (set! result (compiler:rewrite-5 env result))
	   (display "result 5 = ")(write result)(newline)

	   (display "expect   = ")(write expect)(newline)
	   (if (and #f (not (equal? result expect)))
	     (display "expr     = ")(write expr)(newline)
	     (display "result   = ")(write result)(newline)
	     (display "expect   = ")(write expect)(newline)
	     (error "result != expect"))
	   )
	 #f)))
    (t '1 '(&q 1))
    (t '(quote (a b)) '(&q (a b)))
    (t '(begin)       '(&q ()))
    (t '(begin 1)     '(&q 1))
    (t '(begin 1 'y)   '(&b (&_ (&q 1)) (&q y)))
    (t '(begin 1 2 3) '(&b (&_ (&q 1)) (&_ (&q 2)) (&q 3)))
    (t '(if 1 2)      '(&if (&q 1) (&q 2) (&q ())))
    (t '(if 1 2 3)    '(&if (&q 1) (&q 2) (&q 3)))
    (t '(lambda (a b) 1 a)
      '(&lambda #f (a b) (&b (&_ (&q 1)) (&v a))))
    (t '(let ((a 1) (b 1)) 1 a)
      '(&let #f ((a 1) (b 1)) (&b (&_ (&q 1)) (&v a))))
    (t '(foo 1 2)     '(&call (&v foo) (&q 1) (&q 2)))
    (t '(&primitive 1 2) '(&primitive (&q 1) (&q 2)))
    (t '(let ((a 1)) (lambda (x) (+ a x)))
      'HUH?)
    ;; rewrite-2
    (t '((lambda (p1 p2) b) a1 a2)
      '(&let #f ((p2 (&v a2)) (p1 (&v a1))) (&v b)))
    (t '('+ 1 2)
      '(&send (&q +) (&q 1) (&q 2)))
    (t '(&c-func #f (a b c) (+ a c))
      #f)
  )
)

