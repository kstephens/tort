;; (set! &trace 1)

(define-macro (debug expr)
  `(begin
     (display " #| DEBUG: ")
     (write ',expr)
     (display " = ")
     (write ,expr)
     (display " |#")
     (newline)))

(let (
       ;; x68_64
       (word-size  8)
       (arg-regs   '#(%rdi %rsi %rdx %rcx %r8 %r9))
       (SP         '%rsp)
       (BP         '%rbp)
       (IP         '%rip)
       (RESULT     '%rax)
       (TMP0       '%rbx)
       (binary-ops '((MOV movq) (SUB subq) (ADD addq) (OR orq) (AND andq)
		     (CMP cmpq)))
       (unary-ops  '((PUSH pushq) (POP popq)
		      (CALL call) (JMP jmp) (JNE jne) (JE je)))
       (nonary-ops '((LEAVE leave) (RET ret)))

       ;;
       (locative-tag ('get ('get &root 'tagged_mtables) <locative>))
       (fixnum-tag   ('get ('get &root 'tagged_mtables) <fixnum>))
       (compiled-forms ('new <map>))
       )

  (define-struct reg
    (name  'UNKNOWN)
    (width word-size))
  (define-method reg ('_emit self stream)
    ('emit stream ('name self)))

  (let ((regs ('new <map>)))
    (define-method reg ('initialize-struct self)
      (let ((obj ('get regs ('name self))))
	(if (null? obj)
	  ('set regs ('name self) self)
	  (set! self obj))
	;; (debug regs)
	self)))

  ('map! arg-regs (lambda (r) ('new reg 'name r))) 
  (set! SP ('new reg 'name SP))
  (set! BP ('new reg 'name BP))
  (set! IP ('new reg 'name IP))
  (set! RESULT ('new reg 'name RESULT))
  (set! TMP0 ('new reg 'name TMP0))

  (define-struct reg-offset
    (reg    'UNKNOWN-REG)
    (offset 0))
  (define-method reg-offset ('_emit self stream) 
    ('emit stream ('offset self) "(" ('reg self) ")"))

  (define-struct opcode
    (name    'UNKNOWN)
    (n-param 2))
  (define-method opcode ('_emit self stream)
    ('emit stream ('name self) " "))
  
  (define-struct isn
    (opcode 'UNKNOWN)
    (args   '()))
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
  (define-method environ ('lisp_write self port)
    (display "#<environ " port)
    (write ('keys ('bindings self)) port)
    (display " >" port)
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
    (labels   ('new <map>))
    (label-id  0)
    (literals '())
    (pass      'unknown)
    (env       #f)
    )
  (define-method isn-stream ('output-name self)
    (string-append "_tort_x_" (or ('name self) ('_to_string ('_object_ptr self)))))

  (define-struct label
    (id #f)
    (name #f)
    (scope 'local)
    (position #f)
    (references '())
    )
  (define-method label ('_emit self stream)
    (case ('scope self)
      ((global)
	('emit stream "_" ('name self)))
      (else
	('emit stream ('name self)))))
  
  (define-method isn-stream ('label self . name)
    (let ((id ('label-id self))
	   (label ('new label)))
      ('label-id= self (+ id 1))
      ('id= label id)
      (if (null? name)
	(set! name (string->symbol (string-append "L" (number->string id))))
	(begin
	  ('scope= label 'global)
	  (set! name (car name))))
      ('name= label name)
      ('set ('labels self) name label)
      label))

  ;; Emits GCC assembly:
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
  (define-method isn-stream ('emit-isn self opcode . args)
    ('emit self opcode args))
  (define-method isn-stream ('emit-label self l . objs)
    ;; (set! &trace 1)
    (let ((name ('name l)))
      (if (eq? ('scope l) 'global)
	(begin
	  (set! name (string-append "_" ('_to_string name)))
	  ('emit self "  .globl " name "\n")
	  ))
      ('emit self "  " name ":\n")))
  (define-method isn-stream ('emit-align self amount)
    ('emit self "  .align " amount ",0x90\n") ;; ?? 0x90
    )

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
	       ((JMP . args)   `('JMP self  ,@args))
	       ((JE  . args)   `('JE self   ,@args))
	       ((JNE . args)   `('JNE self  ,@args))
	       ((CMP . args)   `('CMP self  ,@args))
	       ((OFFSET reg offset) `('new reg-offset 'reg ,reg 'offset ,offset))
	       ((CONST val)    `('new constant 'value ,val))
	       ((LITERAL val)  `('new literal  'value ,val))
	       ((QUOTE val)    `('new quote    'value ,val))
	       ((LABEL . args) `('label self ,@args))
	       ((LABEL_ obj . args) `('emit-label self ,obj))
	       ((ALIGN size . args) `('emit-align self ,size))
	       ((COMMENT . args) `('emit self "  /* " ,@args " */\n"))
	       ((TEXT . args)  `('emit self "  .text" ,@args "\n"))
	       ((S op . args)  `(,op self env dst ,@args))
	       ((EXPR . args)  `('expr self env dst ,@args))
	       )

    (define-method isn-stream ('mov self src dst . args)
      (cond
       ((eq? src dst)    #f)
       ((eq? dst 'STACK) (PUSH src))
       ((eq? dst 'CALL)  (CALL "*" src))
       (else             (MOV_ src dst args))))

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
	     (this (LABEL ('output-name self)))
	     (start (LABEL)) 
	     (return (LABEL)) 
	     (end (LABEL)))
	(if (null? env)
	    (set! env ('new environ)))
	('env= self env)
	
	;; Bind params.
	('pass= self 'bind-params)
	('bind-params env params)
	;; Find variable contours in body.
	('pass= self 'variable-contour)
	('expr-body self env body)
	;; Allocate space for bindings.
	('pass= self 'allocate-bindings)
	('allocate-bindings env)
	('expr-body self env body)

	;; Preamble
	('pass= self 'emit)
	(TEXT)
	(ALIGN 4) ; ???
	(LABEL_ this)
	(COMMENT (QUOTE params))
	(LABEL_ start)
	(PUSH  BP)
	(MOV   SP BP)
	;; Reserve stack space for bindings.
	(COMMENT "Reserve stack space:")
	('emit-stack-space self env)
	;; Emit param preamble.
	(COMMENT "Save param regs:")
	(for-each (lambda (b) ('emit-binding-preamble self b)) ('binding-list env))
	(COMMENT "Body:")
	;; Emit body exprs.
	('expr-body self env body)
	;; Postamble
	(LABEL_ return)
	(LEAVE)
	(RET)
	(LABEL_ end)
	))
    
    (define-method isn-stream ('expr-body self env body)
      ;; (debug 'pass=)(debug ('pass self))
      (for-each (lambda (e) ('expr self env RESULT e)) body))

    (define-method isn-stream ('method-func self env dst params)
      ('c-func self env dst (cons '&msg params)))
   
    (define-method isn-stream ('expr-call self env dst e)
      (debug 'expr-call)(debug ('pass self))(debug e)
      (case ('pass self)
	((emit)
	 (let ((func (car e)) (args (cdr e))
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
	   ;; (debug arg-i)(debug sp-offset)
	   ('expr self env 'CALL func)
	   (if (> sp-offset 0)
	       (ADD (CONST sp-offset) SP))
	   (MOV RESULT dst)))
	(else
	  (debug 'handle-other)
	  (for-each (lambda (e) (EXPR e)) e)
	 )
	))

    (define-method isn-stream ('expr self env dst e)
      (cond
	((eq? dst e)  #f)
	((symbol? e)  ('expr-var self env dst e))
	((pair? e)    ('expr-pair self env dst e))
	(else         (S 'expr-literal e))))

    (define-method isn-stream ('expr-literal self env dst e)
      (case ('pass self)
	((emit)
	  (MOV (LITERAL e) dst))))

    (define-method isn-stream ('expr-ptr self env dst e)
      ('expr self env RESULT e)
      (case ('pass self)
	((emit)
	  (MOV (OFFSET RESULT 0) dst "\t// *tort_P(" RESULT ")")) ;; dst = ptr->data 
	))

    (define-method isn-stream ('expr-contents self env dst e)
      ('expr self env RESULT e)
      (case ('pass self)
	((emit)
	  (SUB (CONST locative-tag) RESULT "\t// tort_L(" RESULT ")")) ;; RESULT  = locative 
	  (MOV (OFFSET RESULT 0) dst "\t// *" RESULT)
	))

    (define-method isn-stream ('expr-set-contents! self env dst e)
      ('expr self env TMP0 e)
      (case ('pass self)
	((emit)
	  (SUB (CONST locative-tag) TMP0 "\t// tort_L(" TMP0 ")")) ;; RESULT  = locative 
	  (MOV dst (OFFSET TMP0 0) "\t// *" TMP0 " = " dst)
	))

    (define-method isn-stream ('expr-var self env dst e)
      (let ((b ('lookup env e)))
	;; (debug ('pass self))(debug e)(debug b)
	(if (null? b) (error "variable %O is unbound" e))
	(case ('pass self)
	  ((variable-contour)
	    ('referenced?= b #t))
	  ((emit)
	    (MOV ('loc b) dst "\t// " e)
	    ))))

    (define-method isn-stream ('expr-set! self env dst e)
      (let ((b ('lookup env e)))
	;; (debug ('pass self))(debug e)
	(if (null? b) (error "variable %O is unbound" e))
	(case ('pass self)
	  ((emit)
	    (if ('referenced? b)
	      (MOV dst ('loc b) "\t// => " e))
	    ))))

    (define-method isn-stream ('expr-pair self env dst e)
      (if (eq? ('pass self) 'emit) (COMMENT (QUOTE e)))
      (let ((form ('get compiled-forms (car e))))
	(if (null? form)
	  (case (car e)
	    ((quote) (S 'expr-literal (cadr e)))
	    ((if)    (S 'expr-if e))
	    ((let)   (S 'expr-let e))
	    ((set!)
	      (EXPR         (caddr e))
	      (S 'expr-set! (cadr e)))
	    ((lambda)
	      (S 'expr-lambda (cadr e) (caddr e)))
	    ((&ptr)      (S 'expr-ptr (cadr e)))
	    ((&contents) (S 'expr-contents (cadr e)))
	    ((&set-contents!) 
	      (EXPR                  (caddr e))
	      (S 'expr-set-contents! (cadr e)))
	    (else 
	      (S 'expr-call e)))
	  (S form . args)
	  )))

    (define-method isn-stream ('expr-let self env dst e)
      ;; (debug ('pass self))(debug e)
      (let ((bindings (cadr e))
	     (body (cddr e))
	     (subenv ('subenv env e)))
	(case ('pass self)
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
	
	(if (eq? ('pass self) 'allocate-bindings) ('allocate-bindings subenv))
	))

    (define-method isn-stream ('expr-if self env dst e)
      (case ('pass self)
	((emit)
	  (let ((Lfalse (LABEL)) (Lend (LABEL))
		 (test-expr  (cdr e))
		 (true-expr  (cddr e))
		 (false-expr (cdddr e)))
	    (EXPR (car test-expr))
	    (MOV (LITERAL #f) TMP0)
	    (CMP TMP0 dst)
	    (JE Lfalse)
	    (EXPR (car true-expr))
	    (cond 
	      ((pair? false-expr)	
		(JMP Lend)
		(ALIGN 4)
		(LABEL_ Lfalse)
		(EXPR (car false-expr)))
	      (else
		(ALIGN 4)
		(LABEL_ Lfalse)))
	    (ALIGN 4)
	    (LABEL_ Lend)))
	(else
	  (for-each (lambda (e) (EXPR e)) (cdr e))))
      )

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
	    (posix:unlink sfile)
	    (posix:unlink ofile)
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
	    ;; (posix:unlink dfile)
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
     (if #t 1 2)
    (a (&ptr b) 2 3 4 5 6 7 8 9 10)
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

(let ((func ('load s)))
  (debug func)
  ('load <dynlib> "/usr/lib/libc") ;; printf
  ('_ccallv func (vector (&extern 'printf) ('_to_c_ptr "Hello World!\n")))
  )

