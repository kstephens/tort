; (set! &trace 9)
(load "compiler/lib/environment.scm")

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
       (UNSPEC '(&quote ()))
       (caddddr (lambda (x) ('car ('cdr ('cdr ('cdr '(cdr x)))))))
       )
  (set! SP `(&reg ,SP))
  (set! BP `(&reg ,BP))
  (set! IP `(&rep ,IP))
  (map! (lambda (r) `(&reg ,r)) arg-regs)

  ;; Rewrite canonical forms.
  (letrec ((f 
    (lambda (e)
      (cond
	((symbol? e) `(&var ,e))
	((pair? e)
	  (case (car e)
	    ((quote)  `(&quote ,(cadr e)))
	    ;; ((set!)   `(&set! ,(cadr e) ,(f (cadr e))))
	    ((if)     `(&if ,(f (cadr e)) ,(f (caddr e))
			 ,(if (pair? (cdddr e))
			    (f (cadddr e))
			    UNSPEC)))
	    ((lambda)
	      `(&lambda  #f ,(cadr e)  ,(f `(begin ,@(cddr e)))))
	    ((&c-func) ; (&c-func name formals . body) => (&&c-func formals (begin . body) name)
	      (display "  cdddr = ")(write (cdddr e))(newline)
	      `(&&c-func #f ,(caddr e) ,(f `(begin ,@(cdddr e))) ,(cadr e)))
	    ((let)     ; (let bindings . body) => (&let #f bindings (begin . body))
	      `(&let     #f ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (cadr e))
		            ,(f `(begin ,@(cddr e)))))
	    ((begin)  (cond
			((null? (cdr e))  UNSPEC)
			((null? (cddr e)) (f (cadr e)))
			(else
			  (let* ((c (cons #f '())) (r c))
			    (set! e (cdr e))
			    (while (pair? (cdr e))
			      (set-cdr! c (cons `(&void ,(f (car e))) '()))
			      (set! c (cdr c))
			      (set! e (cdr e)))
			    (set-cdr! c (cons (f (car e)) '()))
			    `(&begin ,@(cdr r))))))
	    (else     (if (and (symbol? (car e)) (equal? #\& (string-ref (symbol->string (car e)) 0)))
			`(,(car e) ,@(map f (cdr e)))
			`(&call    ,@(map f e))))
	    ))
	(else `(&quote ,e))))))
    (define (compiler:rewrite-1 e)
      (f e))
    ) ;; letrec
  
  ;; Rewrite ('sym ...) => (&send 'sym ...)
  ;; Rewrite ((lambda formals . body) . inits) => (&let #f bindings body) 
  (letrec ((f 
    (lambda (e)
      (display "  2 e = ")(write e)(newline)
      (cond
	((eq? (car e) '&quote) e)
	((eq? (car e) '&var) e)
	((eq? (car e) '&extern) e)
	((eq? (car e) '&let)     ; (&let #f bindings body)
	  `(&let ,(cadr e) ,(map (lambda (e) `(,(car e) ,(f (cadr e)))) (caddr e))
	     ,(f (cadddr e))))
	((eq? (car e) '&lambda)  ; (&lambda #f formals body)
	  `(&lambda  ,(cadr e) ,(caddr e) ,(f (cadddr e))))
	((eq? (car e) '&&c-func) ; (&&c-func #f formals body name)
	  `(&&c-func ,(cadr e) ,(caddr e) ,(f (cadddr e)) ,(caddddr e)))
	((and                    ; (&call (&quote SYMBOL) ...) => 
	   (eq? (car e) '&call)  ; (&send (&quote SYMBOL) ...)
	   (pair? (cadr e)) (eq? (car (cadr e)) '&quote)
	   (symbol? (cadr (cadr e))))
	  `(&send ,@(map f (cdr e))))
	((and 
	   (eq? (car e) '&call)                       ; (&call
	   (pair? (cadr e)) (eq? (caadr e) '&lambda)) ;   (&lambda ?
	  (let ( (b '()) 
		 (vars  (caddr  (cadr e)))            ;     vars
		 (body  (cadddr (cadr e)))            ;     body)
		 (inits (cddr e))                     ;  . inits)
		 )
	    (while (or (pair? vars) (pair? inits))
	      (set! b (cons (list (car vars) (f (car inits))) b))
	      (set! vars (cdr vars))
	      (set! inits (cdr inits)))
	    `(&let ,(cadr (cadr e)) ,b ,(f body))))
	(else `(,(car e) ,@(map f (cdr e)))))
      )))
    (define (compiler:rewrite-2 e)
      (f e))
    ) ;; letrec

  ;; Create environments.
  (letrec ( 
    (f 
      (lambda (env e)
	(display "  3 e = ")(write e)(newline)
	(case (car e)
	  ((&quote &var &extern) )
					; (&let #f bindings body)
	  ((&let)  
	    (let ((subenv ('subenv env)))
	      (set-car! (cdr e) subenv)
	      ('closure= subenv ('closure env))
	      (for-each (lambda (b)
			  ('add subenv ('new env-binding 'name (car b)))
			  (f env (cadr b)))
		(caddr e))
	      (f subenv (cadddr e))
	      ))
					; (&lambda  #f formals body)
					; (&&c-func #f formals body name)
	  ((&lambda &&c-func)  
	    (let ((subenv ('subenv env)))
	      (set-car! (cdr e) subenv)
	      ('closure= subenv e)
	      (add-formals! subenv (caddr e))
	      (f subenv (cadddr e))
	      ))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e))

    (add-formals!
      (lambda (env params)
	(let ( (l params)
	       (arg-i -1)
	       (arg-bp-offset (+ word-size word-size)) ; BP -> #(prev-BP rtn-addr)
	       (arg #f)
	       (rest-arg #f)
	       (reg #f)
	       (loc #f)
	       (binding #f)
	       )
	  (while (not (null? l))
	    (set! arg-i (+ arg-i 1))
	    (set! reg #f)
	    (set! loc #f)
	    (cond
	      ((symbol? l) ; rest arg
		(set! arg l)
		(set! rest-arg arg-i)
		(set! l '()))
	      (else
		(set! arg (car l))
		(set! l (cdr l))
		(cond
		  ((< arg-i (vector-length arg-regs))
		    ;; allocate temporary on stack for argument register
		    (set! reg (vector-ref arg-regs arg-i)))
		  (else
		    ;; location is relative to BP.
		    (set! loc `(&offset ,BP ,arg-bp-offset))
		    (set! arg-bp-offset (+ arg-bp-offset word-size))))))
	    (set! binding ('new env-binding 
			    'name arg
			    'loc loc
			    'reg reg
			    'rest-arg rest-arg))
	    ;; (debug binding)
	    ('add env binding)
	    )
	  ))
      ))
    (define (compiler:rewrite-3 env e)
      (f env e))
    ) ;; letrec

  ;; Find variables.
  (letrec (
    (f 
      (lambda (env e)
	(display "  4 e = ")(write e)(newline)
	(case (car e)
	  ((&quote) )
	  ((&var)
	    (let ((b ('lookup-or-add-global env (cadr e))))
	      (display "   b =")(write b)(newline)
	      (if (null? b) (error "variable %O is unbound" (cadr e)))
	      (set-car! (cdr e) b)
	      ('referenced?= b #t)
	      (if (not (eq? ('closure env) ('closure ('env b))))
		('closed-over= b #t))
	      ))
	  ;; (&let #f bindings body)
	  ((&let) 
	    (let ((subenv (cadr e)))
	      (for-each (lambda (b)
			  (f env (cadr b)))
		(caddr e))
	      (f subenv (cadddr e))
	      ))
	  ;; (&lambda #f formals body)
	  ;; (&&c-func #f formals body name)
	  ((&lambda &&c-func)  
	    (let ((subenv (cadr e)))
	      (f subenv (cadddr e))
	      ))
	  (else
	    (for-each (lambda (e) (f env e)) (cdr e))))
	e)))
    (define (compiler:rewrite-4 env e)
      (f env e))
    ) ;; letrec

  (let
    ((t 
       (lambda (expr expect)
	 (let ((result expr) (env ('new environ)))
	   (display "expr     = ")(write expr)(newline)

	   (set! result (compiler:rewrite-1 result))
	   (display "result 1 = ")(write result)(newline)
	   (set! result (compiler:rewrite-2 result))
	   (display "result 2 = ")(write result)(newline)
	   (set! result (compiler:rewrite-3 env result))
	   (display "result 3 = ")(write result)(newline)
	   (set! result (compiler:rewrite-4 env result))
	   (display "result 4 = ")(write result)(newline)

	   (display "expect   = ")(write expect)(newline)
	   (if (and #f (not (equal? result expect)))
	     (display "expr     = ")(write expr)(newline)
	     (display "result   = ")(write result)(newline)
	     (display "expect   = ")(write expect)(newline)
	     (error "result != expect"))
	   )
	 #f)))
    (t '1 '(&quote 1))
    (t '(quote (a b)) '(&quote (a b)))
    (t '(begin)       '(&quote ()))
    (t '(begin 1)     '(&quote 1))
    (t '(begin 1 'y)   '(&begin (&void (&quote 1)) (&quote y)))
    (t '(begin 1 2 3) '(&begin (&void (&quote 1)) (&void (&quote 2)) (&quote 3)))
    (t '(if 1 2)      '(&if (&quote 1) (&quote 2) (&quote ())))
    (t '(if 1 2 3)    '(&if (&quote 1) (&quote 2) (&quote 3)))
    (t '(lambda (a b) 1 a)
      '(&lambda #f (a b) (&begin (&void (&quote 1)) (&var a))))
    (t '(let ((a 1) (b 1)) 1 a)
      '(&let #f ((a 1) (b 1)) (&begin (&void (&quote 1)) (&var a))))
    (t '(foo 1 2)     '(&call (&var foo) (&quote 1) (&quote 2)))
    (t '(&primitive 1 2) '(&primitive (&quote 1) (&quote 2)))
    (t '(let ((a 1)) (lambda (x) (+ a x)))
      'HUH?)
    ;; rewrite-2
    (t '((lambda (p1 p2) b) a1 a2)
      '(&let #f ((p2 (&var a2)) (p1 (&var a1))) (&var b)))
    (t '('+ 1 2)
      '(&send (&quote +) (&quote 1) (&quote 2)))
    (t '(&c-func #f (a b c) (+ a c))
      #f)
  )
)

