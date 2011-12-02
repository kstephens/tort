(define-macro (debug expr)
  `(begin
     (display " #| DEBUG: ")
     (write ',expr)
     (display " = ")
     (write ,expr)
     (display " |#")
     (newline)))

(let (
       (ops '())
       (word-size  8)
       (arg-regs   '#(%rdi %rsi %rdx %rcx %r8 %r9))
       (SP         '%rsp)
       (BP         '%rbp)
       (RESULT     '%rax)
       (binary-ops '((MOV movq) (SUB subq) (ADD addq) (OR orq) (AND andq)))
       (unary-ops  '((PUSH pushq) (POP popq) (CALL call)))
       (nonary-ops '((LEAVE leave) (RTN rtn)))
       (compiled-forms ('new <map>))
       )

  (define-struct reg
    (name  'UNKNOWN)
    (width word-size))
  (define-method reg ('_emit self stream)
    ('emit stream ('name self)))
  (set! SP (reg:new 'name SP))
  (set! BP (reg:new 'name BP))
  (set! RESULT (reg:new 'name RESULT))

  (define-struct reg-offset
    (reg    'UNKNOWN-REG)
    (offset 0))
  (define-method reg-offset ('_emit self stream) 
    ('emit stream ('offset self) "(" ('reg self) ")"))

  (define-struct opcode
    (name    'UNKNOWN)
    (n-param 2))
  (define-method opcode ('new self name n-param)
    ('name= self name)
    ('n-param= self n-param))
  (define-method opcode ('_emit self stream)
    ('emit stream ('name self) " "))
  
  (define-struct isn
    (opcode 'UNKNOWN)
    (args   '()))
  (define-method isn ('new self opcode args)
    ('opcode= self opcode)
    ('args= self args))
  (define-method isn ('_emit self stream)
    (let ((sep " ") (arg-i 0) (opcode ('opcode self)))
      ('emit stream opcode)
      (for-each
	(lambda (arg)
	  ('emit stream sep arg)
	  (set! arg-i (+ arg-i 1))
	  (set! sep 
	    (cond
	      ((< arg-i ('n-param opcode))
		", ")
	      ((= arg-i ('n-param opcode))
		" // ")
	      (else
		" "))))
	('args self)))
    ('emit stream "\n")
    #t)
  
  (define-struct label
    (name #f)
    (position #f)
    (references '())
    )
  (define-method label ('_emit self stream)
    (let ((id ('label stream)))
      ('label-id= stream (+ id 1))
      ('name= self (string-append "L" (number->string id))))
    ('emit stream ('name self) ":\n")
    ('labels= stream (cons self ('labels stream))))
  
  (define-struct constant
    (value #f)
    )
  (define-method constant ('_emit self stream)
    (let ((v ('value self)))
      (cond 
	((number? v)
	  ('emit stream "$" v))
      )))

  (define-struct environ
    (parent           #f)
    (alloc-env        #f)
    (alloc-offset     0)
    (alloc-offset-max 0)
    (bindings      ('new <map>))
    (subenvs       ('new <map>))
    )
  (define-method environ ('add self binding)
    ('set ('bindings self) ('name binding) binding)
    binding)
  (define-method environ ('binding-list self)
    (vector->list ('values ('bindings self))))
  (define-method environ ('lookup self name)
    ('get ('bindings self) name))
  (define-method environ ('subenv self obj)
    (let ((env ('get ('subenvs self) obj)))
      (if (null? env)
	(begin
	  (set! env ('new environ 'parent self 'alloc-env self))
	  ('set ('subenvs self) obj env)
	  ))
      env))

  (define-struct env-binding
    (name        #f)
    (referenced? #f)
    (set?        #f)
    (loc         #f)
    (size        #f)
    (reg         #f)
    (rest-arg    #f)
    (restore-reg #f)
    (closed-over #f)
    (exported    #f)
    )

  (define-struct isn-stream
    (body     (string-new))
    (labels   '())
    (label-id  0)
    (mode      nil)
    (env       #f)
    )
  (define-method isn-stream ('emit self . objs)
    (for-each 
      (lambda (obj)
	(cond
	  ((or (string? obj) (symbol? obj) (number? obj))
	    (display obj ('body self)))
	  ((null? obj)
	    )
	  ((pair? obj)
	    (for-each (lambda (obj) ('emit self obj)) obj))
	  (else
	    ;; (debug obj)
	    ('_emit obj self))))
      objs)
    self #t)

  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self src dst . rest)
	('emit self (cadr x) " " src ", " dst " // " rest "\n")))
  binary-ops)
  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self src . rest)
	('emit self (cadr x) " " src " // " rest "\n")))
    unary-ops)
  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self . rest)
	('emit self (cadr x) " // " rest "\n")))
    nonary-ops)

  (let-macro (
	       ((PUSH . args)  `('PUSH self  ,@args))
	       ((POP . args)   `('POP self  ,@args))
	       ((MOV . args)   `('MOV self   ,@args))
	       ((SUB . args)   `('SUB self   ,@args))
	       ((ADD . args)   `('ADD self   ,@args))
	       ((LEAVE . args) `('LEAVE self ,@args))
	       ((RTN . args)   `('RTN self   ,@args))
	       ((OFFSET reg offset) `('new reg-offset 'reg ,reg 'offset ,offset))
	       ((CONST val) `('new constant 'value ,val))
	       )

    (define-method environ ('allocate-binding env binding)
      (if (and ('referenced? binding) (not ('loc binding)))
	(let* ((alloc-env (or ('alloc-env env) env))
		(alloc-offset ('alloc-offset alloc-env))
		(alloc-size (or ('size binding) word-size)))
	  ('size= binding alloc-size)
	  ;; Align to word boundary.
	  (set! alloc-size (* (/ (+ alloc-size (- word-size 1)) word-size) word-size))
	  (set! alloc-offset (- alloc-offset alloc-size))
	  ('alloc-offset= alloc-env alloc-offset)
	  (if (> ('alloc-offset-max alloc-env) alloc-offset)
	    ('alloc-offset-max= alloc-env alloc-offset))
	  ('loc= binding (OFFSET BP alloc-offset))
	  (debug 'allocate-binding)(debug binding)
	)))
    (define-method environ ('allocate-bindings env)
      (for-each (lambda (b) ('allocate-binding env b))
	('binding-list env)))

    (define-method environ ('bind-params env params)
      (let ( (l params)
	     (arg-i -1)
	     (arg-bp-offset (+ word-size word-size)) ; BP -> #(prev-BP rtn-addr)
	     (arg #f)
	     (rest-arg #f)
	     (reg #f)
	     (loc #f)
	     (binding #f)
	     ) 
	;; (debug args)
	;; (debug (vector-length arg-regs))
	(while (not (null? l))
	  (set! arg-i (+ arg-i 1))
	  (set! reg #f)
	  (set! loc #f)
	  ;; (debug l)
	  ;; (debug arg-i)
	  (cond
	    ((symbol? l) ; rest arg
	      (set! arg l)
	      (set! rest-arg l)
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
		  (set! loc (OFFSET BP arg-bp-offset))
		  (set! arg-bp-offset (+ arg-bp-offset word-size))))))
	  ;; (debug arg)
	  ;; (debug loc)
	  ;; (debug reg)
	  (set! binding ('new env-binding 
			  'name arg
			  'loc loc
			  'reg reg
			  'rest-arg rest-arg))
	  ;; (debug binding)
	  ('add env binding)
	  ;; FIXME: handle rest-arg.
	  )
	))


    (define-method isn-stream ('emit-binding-preamble self binding)
      ;; FIXME: handle rest-arg.
      (let ((reg ('reg binding))
	     (loc ('loc binding)))
	;; If binding is in register, and there is a stack location allocated, move it to stack.
	(if (and reg loc)
	  (MOV reg loc '<= ('name binding))
	  )))

    (define-method isn-stream ('emit-stack-space self env)
      ;; Save stack space for bindings.
      (let ((alloc-offset ('alloc-offset-max env)))
	(if (< alloc-offset 0)
	  (begin
	    ;; Align to 16-byte boundary.
	    (set! alloc-offset (* (/ (+ (- alloc-offset) 15) 16) 16))
	    ('alloc-offset-max= env (- alloc-offset))
	    (SUB (CONST alloc-offset) SP)))))

    (define-method isn-stream ('cfunc self env params body)
      (if (null? env)
        (set! env ('new environ)))
      ('env= self env)

      ;; Preamble
      (PUSH  BP)
      (MOV   SP BP)
      ;; Bind params.
      ('bind-params env params)
      ;; Find variable contours in body.
      ('mode= self 'variable-contour)
      (for-each (lambda (e) ('expr self env RESULT e)) body)
      ;; Allocate space for bindings.
      ('mode= self 'allocate-bindings)
      ('allocate-bindings env)
      (for-each (lambda (e) ('expr self env RESULT e)) body)
      ;; Emit param preamble.
      (for-each (lambda (b) ('emit-binding-preamble self b)) ('binding-list env))
      ;; Reserve stack space for bindings.
      ('emit-stack-space self env)
      ;; Emit body exprs.
      ('mode= self 'emit)
      (for-each (lambda (e) ('expr self env RESULT e)) body)
      ;; Postamble
      (LEAVE)
      (RTN)
      self
      #t
      )

    (define-method isn-stream ('ccall self env dst func args)
      #f
      )
    
    (define-method isn-stream ('method self env dst params)
      #f
      )
    
    (define-method isn-stream ('expr-call self env dst obj)
      #f)

    (define-method isn-stream ('expr self env dst obj)
      (cond
	((eq? dst obj) #f)
	((symbol? obj)
	  ('expr-var self env dst obj))
	((pair? obj)
	  ('expr-pair self env dst obj))
	(else
	  (debug obj)
	  (error "unknown object"))))
    
    (define-method isn-stream ('expr-var self env dst obj)
      (let ((b ('lookup env obj)))
	(debug ('mode self))(debug obj)(debug b)
	(if (null? b) (error "variable %O is unbound" obj))
	(case ('mode self)
	  ((variable-contour)
	    ('referenced?= b #t))
	  ((emit)
	    (MOV ('loc b) dst obj)
	    ))))

    (define-method isn-stream ('expr-set! self env dst obj)
      (let ((b ('lookup env obj)))
	(debug ('mode self))(debug obj)
	(if (null? b) (error "variable %O is unbound" obj))
	(case ('mode self)
	  ((emit)
	    (if ('referenced? b)
	      (MOV dst ('loc b) '=> obj))
	    ))))

    (define-method isn-stream ('expr-pair self env dst obj)
      (let ((form ('get compiled-forms (car obj))))
	(if (null? form)
	  (case (car obj)
	    ((let)  ('expr-let self env dst obj))
	    ((set!)
	      ('expr self self env dst (caddr obj))
	      ('expr-set! self env dst (cadr obj)))
	    (else 
	      ('expr-call self env dst obj)))
	  (form self env dst . args)
	  )))

    (define-method isn-stream ('expr-let self env dst obj)
      (debug ('mode self))(debug obj)
      (let ((bindings (cadr obj))
	     (body (cddr obj)))
	(let ((subenv ('subenv env obj)))
	  (case ('mode self)
	    ((variable-contour)
	      (for-each (lambda (b)
			  ('add subenv ('new env-binding 'name (car b))))
		bindings)
	      ))

	  (for-each (lambda (b) 
		      ('expr      self env    RESULT (cadr b))
		      ('expr-set! self subenv RESULT (car  b)))
	    bindings)

	  (for-each (lambda (e) ('expr self subenv dst e)) body)

	  (display "  env for ")(write obj)(display ":\n")
	  (for-each (lambda (b)
		      (write b)(newline))
	    ('binding-list subenv))(display "\n")
	  (newline)

	  (case ('mode self)
	    ((allocate-bindings) ('allocate-bindings subenv)))
	  )))

#|
    (let-macro (
		 ((form name-args . body)  
		   `('set compiled-forms ,(car name-args) (lambda (self env dst ,@(cdr name-args)) ,@body))))
      (form (if test t . rest) #f)
      (form (while test . body) #f)
      (form (lambda args . body) #f)
      ) ;; let-macro
|#
    (debug compiled-forms)

    ) ;; let-macro
  
  ) ;; let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (display "\nenviron=")(write environ)(newline)
;; (define e ('new environ))
;; (display "\ne=\n")(write e)(newline)
(define s ('new isn-stream))
;; (display "\ns=\n")(write s)(newline)
('emit s "// hello, world\n")
('cfunc s nil '(a b c d e f g h i) '(a c e g h i (let ((x a) (y b)) x y)))
(display "\nCode:\n")(display ('body s))(display "\n")
(display "\nEnv:\n")
(for-each (lambda (b)
	    (write b)(newline))
	  ('binding-list ('env s)))(display "\n")
