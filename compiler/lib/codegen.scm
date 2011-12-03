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
       (nonary-ops '((LEAVE leave) (RET ret)))
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
		"\t// ")
	      (else
		" "))))
	('args self)))
    ('emit stream "\n")
    #t)
  
  (define-struct constant (value #f))
  (define-method constant ('_emit self stream)
    (let ((v ('value self)))
      (cond 
	((number? v)
	  ('emit stream "$" v))
	(else (error "unknown constant type %O" v)))
      ))

  (define-struct literal (value #f))
  (define-method literal ('_emit self stream)
    (let ((v ('value self)))
      ('literals= stream (cons v ('literals stream)))
      (display '$ ('body stream))
      (display ('_to_c_literal v) ('body stream))))

  (define-struct quote (value #f))
  (define-method quote ('_emit self stream)
    (let ((v ('value self)))
      (write v ('body stream))))

  (define-struct environ
    (parent           #f)
    (alloc-env        #f)
    (alloc-offset     0)
    (alloc-offset-max 0)
    (bindings      ('new <map>))
    (macros        ('new <map>))
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
    (type        #f)
    (size        #f)
    (reg         #f)
    (rest-arg    #f)
    (restore-reg #f)
    (closed-over #f)
    (exported    #f)
    )

  (define-struct isn-stream
    (name     #f)
    (_output-name #f)
    (body     (string-new))
    (labels   '())
    (label-id  0)
    (literals '())
    (mode      nil)
    (env       #f)
    )
  (define-method isn-stream ('output-name self)
    (string-append "_tort_x_" (or ('name self) ('_to_string ('_object_ptr self)))))

  (define-struct label
    (id #f)
    (name #f)
    (position #f)
    (references '())
    )
  (define-method label ('_emit self stream)
    (display ('name self) ('body stream)))
  
  (define-method isn-stream ('label self)
    (let ((id ('label-id self))
	   (label ('new label)))
      ('label-id= self (+ id 1))
      ('id= label id)
      ('name= label (string-append "L" (number->string id)))
      ('labels= self (cons label ('labels self)))
      label))

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
	    ('_emit obj self))))
      objs)
    self #t)

  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self src dst . rest)
	('emit self (cadr x) " " src ", " dst rest "\n")))
    binary-ops)
  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self src . rest)
	('emit self (cadr x) " " src rest "\n")))
    unary-ops)
  (for-each 
    (lambda (x)
      (define-method isn-stream ((car x) self . rest)
	('emit self (cadr x) rest "\n")))
    nonary-ops)

  (let-macro (
	       ((PUSH . args)  `('PUSH self  ,@args))
	       ((POP . args)   `('POP self   ,@args))
	       ((MOV_ . args)  `('MOV self   ,@args))
	       ((MOV . args)   `('mov self   ,@args))
	       ((SUB . args)   `('SUB self   ,@args))
	       ((ADD . args)   `('ADD self   ,@args))
	       ((LEAVE . args) `('LEAVE self ,@args))
	       ((RET . args)   `('RET self   ,@args))
	       ((CALL . args)  `('CALL self  ,@args))
	       ((OFFSET reg offset) `('new reg-offset 'reg ,reg 'offset ,offset))
	       ((CONST val)    `('new constant 'value ,val))
	       ((LITERAL val)  `('new literal  'value ,val))
	       ((QUOTE val)    `('new quote    'value ,val))
	       ((LABEL_ obj . args) `('emit self ,obj ":" ,@args "\n"))
	       ((S op . args)  `(,op self env dst ,@args))
	       )

    (define-method isn-stream ('mov self src dst . args)
      (cond
       ((eq? src dst) #f)
       ((eq? dst 'STACK)
	(PUSH src))
       ((eq? dst 'CALL)
	(CALL "*" src))
       (else (MOV_ src dst args))))

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
	  ;; (debug 'allocate-binding)(debug binding)
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
	  (MOV_ reg loc "\t// <= " ('name binding))
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

    (define-method isn-stream ('c-func self env params body)
      (let ((dst RESULT) 
	     (start ('label self)) 
	     (return ('label self)) 
	     (end ('label self)))
	(if (null? env)
	    (set! env ('new environ)))
	('env= self env)
	
	;; Preamble
	(LABEL_ ('output-name self))
	('emit self "/* " (QUOTE params) " */\n")
	(LABEL_ start)
	(PUSH  BP)
	(MOV   SP BP)
	;; Bind params.
	('bind-params env params)
	;; Find variable contours in body.
	('mode= self 'variable-contour)
	('expr-body self env body)
	;; Allocate space for bindings.
	('mode= self 'allocate-bindings)
	('allocate-bindings env)
	('expr-body self env body)
	;; Emit param preamble.
	(for-each (lambda (b) ('emit-binding-preamble self b)) ('binding-list env))
	;; Reserve stack space for bindings.
	('emit-stack-space self env)
	;; Emit body exprs.
	('mode= self 'emit)
	('expr-body self env body)
	;; Postamble
	(LABEL_ return)
	(LEAVE)
	(RET)
	(LABEL_ end)
	self
	#t
	))

    (define-method isn-stream ('expr-body self env body)
      ;; (debug 'pass=)(debug ('mode self))
      (for-each (lambda (e) ('expr self env RESULT e)) body))

    (define-method isn-stream ('method-func self env dst params)
      ('cfunc self env dst (cons '&msg params)))
    
    (define-method isn-stream ('expr-call self env dst obj)
      (debug 'expr-call)(debug ('mode self))(debug obj)
      (case ('mode self)
	((emit)
	 (let ((func (car obj)) (args (cdr obj))
	       (arg-i #f) (nargs -1) (sp-offset 0))
	   (set! arg-i (reverse
			(map (lambda (e) 
			       (set! nargs (+ nargs 1))
			       (cons e nargs)
			       ) args)))
	   (for-each (lambda (ei) 
		       ('expr self env 
			      (if (< (cdr ei) (vector-length arg-regs))
				  (vector-ref arg-regs (cdr ei))
				  (begin
				    (set! sp-offset (+ sp-offset word-size))
				    'STACK))
			      (car ei))
		       ) arg-i)
	   (debug arg-i)(debug sp-offset)
	   ('expr self env 'CALL func)
	   (if (> sp-offset 0)
	       (ADD (CONST sp-offset) SP))
	   (MOV RESULT dst)))
	(else
	 (debug 'handle-other)
	 ;; FIXME:
	 ;; (for-each (lambda (e) (S 'expr e)) obj)
	 )
	))

    (define-method isn-stream ('expr self env dst obj)
      (cond
	((eq? dst obj) #f)
	((symbol? obj)
	  ('expr-var self env dst obj))
	((pair? obj)
	  ('expr-pair self env dst obj))
	(else
	  ; (debug obj)
	  (S 'expr-constant obj))))

    (define-method isn-stream ('expr-constant self env dst obj)
      (MOV (LITERAL obj) dst))

    (define-method isn-stream ('expr-var self env dst obj)
      (let ((b ('lookup env obj)))
	;; (debug ('mode self))(debug obj)(debug b)
	(if (null? b) (error "variable %O is unbound" obj))
	(case ('mode self)
	  ((variable-contour)
	    ('referenced?= b #t))
	  ((emit)
	    (MOV ('loc b) dst "\t// " obj)
	    ))))

    (define-method isn-stream ('expr-set! self env dst obj)
      (let ((b ('lookup env obj)))
	;; (debug ('mode self))(debug obj)
	(if (null? b) (error "variable %O is unbound" obj))
	(case ('mode self)
	  ((emit)
	    (if ('referenced? b)
	      (MOV dst ('loc b) "\t// => " obj))
	    ))))

    (define-method isn-stream ('expr-pair self env dst obj)
      (let ((form ('get compiled-forms (car obj))))
	(if (null? form)
	  (case (car obj)
	    ((quote) (S 'expr-constant (cadr obj)))
	    ((let)   (S 'expr-let obj))
	    ((set!)
	      (S 'expr      (caddr obj))
	      (S 'expr-set! (cadr obj)))
	    (else 
	      (S 'expr-call obj)))
	  (S form . args)
	  )))

    (define-method isn-stream ('expr-let self env dst obj)
      (debug ('mode self))(debug obj)
      (let ((bindings (cadr obj))
	     (body (cddr obj))
	     (subenv ('subenv env obj)))
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
	
	(if (eq? ('mode self) 'allocate-bindings) ('allocate-bindings subenv))
	))

#|
    (let-macro (
		 ((form name-args . body)  
		   `('set compiled-forms ',(car name-args) (lambda (self env dst ,@(cdr name-args)) ,@body))))
      (form (if test t . body) #f)
      (form (while test . body) #f)
      (form (lambda args . body) #f)
      ) ;; let-macro
|#
    (debug compiled-forms)

    (define-method isn-stream ('assemble c . options)
      (let ((name ('output-name c))
	     (verbose (not (null? options)))
	     (fname nil))
	(set! verbose #t)
	(set! fname (string-append "tmp/" name))
	(let ((sfile (string-append fname ".s"))
	       (ofile (string-append fname ".o"))
	       (dfile (string-append fname ".dylib"))
	       )
	  ;;(display "sfile ")(write sfile)(newline)
	  ;;(display "ofile ")(write ofile)(newline)
	  (let ((name-sym (string->symbol name))
		 (st nil)
		 (func-ptr nil)
		 (result #f))
	    (call-with-output-file sfile (lambda (f)
					   (display ('body c) f)))
	    (if verbose
	      (begin
		(display "Assembly:\n") 
		(display ('body c))
		(display "=====\n")))
	    
	    (posix:system (string-append "gcc "
			    ;; (if verbose "--verbose" "")
			    " -D__DYNAMIC__ -fPIC -DPIC "
			    " -export-dynamic -fno-common -c -o " ofile " " sfile))
	    ;; (if verbose (posix:system (string-append "otool -tv " ofile)))
	    (posix:system (string-append "gcc "
			    ;; (if verbose "--verbose" "")
			    " -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o " dfile " " ofile " -compatibility_version 1 -current_version 1.0 -Wl,-single_module"))
	    ;; (if verbose (posix:system (string-append "otool -tv " dfile)))
	    (set! result dfile)
	    (display "'assemble => ")(write result)(newline)
	    result
	    ))))

    (define-method isn-stream ('load c . options)
      (let ((name ('output-name c))
	 (verbose (not (null? options)))
	 (fname nil))
	(set! verbose #t)
	(set! fname (string-append "tmp/" name))
	(let ((sfile (string-append fname ".s"))
	       (ofile (string-append fname ".o"))
	       (dfile (string-append fname ".dylib"))
	       )
	  (let ((name-sym (string->symbol name))
		 (st nil)
		 (func-ptr nil)
		 (result nil))
	    (set! st ('load <dynlib> (string-append "./" dfile)))
	    ;;(display "st = ")(write st)(newline)
	    ;;(display "name-sym = ")(write name-sym)(newline)
	    ;;(display "name-sym class = ")(write (%get-type name-sym))(newline)
	    ;;('__debugger st) (set! &trace 1)
	    (set! func-ptr ('get st name-sym))
	    (if (or #t verbose) (begin (display "  func-ptr = ")(write func-ptr)(newline)))
	    (set! result func-ptr)
	    result
	    ))))

    ) ;; let-macro
  
  ) ;; let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (display "\nenviron=")(write environ)(newline)
;; (define e ('new environ))
;; (display "\ne=\n")(write e)(newline)
(define s ('new isn-stream))
;; (display "\ns=\n")(write s)(newline)
;; ('emit s "// hello, world\n")
('c-func s nil 
  '(a b c d e f g h i)
  '(a c e g h i
    (let ((x a) (y b)) x y)
    (a 1 2 3 4 5 6 7 8 9 10)
    ))
(display "\nCode:\n")(display ('body s))(display "\n")
(display "\nEnv Bindings:\n")
(for-each (lambda (b)
	    (write b)(newline))
	  ('binding-list ('env s)))(display "\n")
(display "\nLiterals:\n")
(for-each (lambda (b)
	    (write b)(newline))
  ('literals s))(display "\n")
('assemble s)
('load s)

