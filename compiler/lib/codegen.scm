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
    ('emit stream "\n"))
  
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

=  (define-struct environ
    (parent        nil)
    (bp-offset    0) ; (list 0))
    ;; (bp-offset-max 0)
    (saves         nil)
    (bindings      ('new <map>))
    )
  (define-method environ ('add self binding)
    ('set ('bindings self) ('name binding) binding)
    binding)
  (define-method environ ('lookup self name)
    ('get ('bindings self) name))
    
  (define-struct env-binding
    (name        #f)
    (getted      #f)
    (setted      #f)
    (loc         nil)
    (bp-offset   #f)
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
    self)

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

    (define-method environ ('bind env name . bp-offset)
      (let ( (loc #f)
	     (binding #f))
	;; Allocate offset?
	;; (debug (pair? bp-offset))
	;; (debug (and (pair? bp-offset) (car bp-offset)))
	(set! bp-offset (and (pair? bp-offset) (car bp-offset)))
	;; (debug bp-offset)
	(if (or (not bp-offset) (null? bp-offset))
	  (begin
	    (set! bp-offset (- ('bp-offset env) word-size))
	    ('bp-offset= env bp-offset)))
	(if (number? bp-offset)
	  (set! loc (OFFSET BP bp-offset))
	  (set! loc bp-offset))
	(set! binding ('new env-binding 
			'name name
			'loc loc
			'bp-offset bp-offset))
	;;(debug name)
	;;(debug loc)
	;;(debug bp-offset)
	;;(debug binding)
	('add env binding)))
    
    (define-method isn-stream ('emit-params self env params)
      ;; Save parameter registers as unnamed bindings:
      (let ( (l params)
	     (rest-arg #f)
	     (arg nil)
	     (loc nil)
	     (binding nil)
	     (arg-i -1)
	     (reg #f)
	     (arg-offset (+ word-size word-size))) ; BP -> #(prev-BP rtn-addr)
	;; (debug args)
	;; (debug (vector-length arg-regs))
	(while (not (null? l))
	  (set! arg-i (+ arg-i 1))
	  ;; (debug l)
	  ;; (debug arg-i)
	  (cond
	    ((symbol? l) ; rest arg
	      (set! arg l)
	      (set! rest-arg l)
	      (set! l '())
	      (set! loc nil)
	      (set! reg #f))
	    (else
	      (set! arg (car l))
	      (set! l (cdr l))
	      (cond
		((< arg-i (vector-length arg-regs))
		  (set! loc #f) ;; allocate temporary on stack.
		  (set! reg (vector-ref arg-regs arg-i)))
		(else
		  (set! loc arg-offset) ;; location is relative to BP.
		  (set! reg #f)
		  (set! arg-offset (+ arg-offset word-size))))))
	  ;; (debug arg)
	  ;; (debug loc)
	  ;; (debug reg)
	  (set! binding ('bind env arg loc))
	  (debug binding)
	  ;; save reg to stack binding.
	  (if reg
	    (MOV reg ('loc binding) '<= arg))
	  ;; FIXME: handle rest-arg.
	  )
	))

    (define-method isn-stream ('emit-stack-space self env)
      ;; Save stack space for args.
      (let ((bp-offset ('bp-offset env)))
	(if (< bp-offset 0)
	  (begin
	    ;; Align to 16-byte boundarys
	    (set! bp-offset (* (/ (+ (- bp-offset) 15) 16) 16))
	    (SUB (CONST bp-offset) SP)))))

    (define-method isn-stream ('cfunc self env params body)
      (if (null? env)
        (set! env ('new environ)))
      ('env= self env)
      (PUSH  BP)
      (MOV   SP BP)
      ('emit-params self env params)

      ('mode= self 'variable-contour)
      (for-each (lambda (expr)
		  ('expr self env expr RESULT))
	body)
      ('emit-stack-space self env)

      ('mode= self 'emit)
      (for-each (lambda (expr)
		  ('expr self env expr RESULT))
	body)

      (LEAVE)
      (RTN)
      self
      )

    (define-method isn-stream ('ccall self env func args)
      #f
      )
    
    (define-method isn-stream ('method self env params)
      #f
      )
    
    (define-method isn-stream ('expr-send self env sel args dst)
      #f
      )
    
    (define-method isn-stream ('expr self env obj dst)
      (cond
	((symbol? obj)
	  ('expr-var self env obj dst))
	((pair? obj)
	  ('expr-pair self env obj dst))
	(else
	  (error "unknown object"))))
    
    (define-method isn-stream ('expr-var self env obj dst)
      (let ((b ('lookup env obj)))
	;; (debug env)
	(debug obj)(debug b)(debug ('mode self))
	(case ('mode self)
	  ((variable-contour)
	   (if (null? b) (error "variable %O is unbound" obj)
	       ('getted= b #t)))
	  ((emit)
	   (if (null? b) (error "variable %O is unbound" obj)
	       (MOV ('loc b) dst obj)
	       )))))
    (define-method isn-stream ('expr-pair self env obj dst)
      (let ((form ('get compiled-forms (car obj)))
	     (args (cdr obj)))
	(if (null? form)
	  ('expr-send self env (car obj) args dst)
	  (form self env dst . args)
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
(display "\ns=\n")(write s)(newline)
('emit s "// hello, world\n")
('cfunc s nil '(a b c d e f g h i) '(a c e g h i))
(display "\nCode:\n")(display ('body s))(display "\n")
(display "\nEnv:\n")
(for-each (lambda (b)
	    (write b)(newline))
	  (vector->list ('values ('bindings ('env s)))))(display "\n")
