;; (set! &trace 1)
(load "compiler/lib/rewrite.scm")

(define-macro (debug expr)
  `(begin
     (display " #| DEBUG: ")
     (write ',expr)
     (display " = ")
     (write ,expr)
     (display " |#")
     (newline)))

(let* (
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

       ;;
       (word-size    ('get &root 'WORD_SIZE))
       (tag-bits     ('get &root 'TAG_BITS))
       (locative-tag ('get ('get &root 'tagged_mtables) <locative>))
       (fixnum-tag   ('get ('get &root 'tagged_mtables) <fixnum>))
       (compiled-forms ('new <map>))
       (unspec '())
       (UNSPEC ''())
       (VOID 'VOID)
       (STK 'STK)
       (TOP #f)
       (gensym (lambda () (make-symbol '())))
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
    (globals       #f)
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
    (main      #f)
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
    (case ('pass self)
      ((emit)
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
      objs)))
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
    (lambda (op)
      (let ((sel (car op)) (opcode (cadr op)))
	(case (caddr op)
	  ((0) (define-method isn-stream (sel self . r)     ('emit self opcode              r "\n")))
	  ((1) (define-method isn-stream (sel self a . r)   ('emit self opcode " " a        r "\n")))
	  ((2) (define-method isn-stream (sel self a b . r) ('emit self opcode " " a ", " b r "\n")))
	  )))
    ops)
  
  (let-macro (
	       ((PUSH . args)  `('PUSH self  ,@args))
	       ((POP_ . args)  `('POP self   ,@args))
	       ((POP . args)   `('pop self   ,@args))
	       ((MOV_ . args)  `('MOV self   ,@args))
	       ((MOV . args)   `('mov self   ,@args))
	       ((SUB . args)   `('SUB self   ,@args))
	       ((ADD . args)   `('ADD self   ,@args))
	       ((SAR . args)   `('SAR self   ,@args))
	       ((SAL . args)   `('SAL self   ,@args))
	       ((OR  . args)   `('OR self   ,@args))
	       ((AND . args)   `('AND self   ,@args))
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
	       ((LABEL: obj . args) `('emit-label self ,obj))
	       ((ALIGN size . args) `('emit-align self ,size))
	       ((COMMENT . args) `('emit self "  /* " ,@args " */\n"))
	       ((TEXT . args)  `('emit self "  .text" ,@args "\n"))
	       ((S op . args)  `(,op self env dst ,@args))
	       ((ED   . args)  `('expr self env ,@args))
	       ((E    . args)  `('expr self env dst ,@args))
	       )
    
    (set! STK (OFFSET SP 0))

    (define-method isn-stream ('pop self . args)
      (if (null? args)
	(ADD (CONST word-size) SP)
	(POP_ (car args))))

    (define-method isn-stream ('mov self src dst . args)
      (cond
       ((eq? dst src)    #f)
       ((eq? dst 'VOID)  #f)
       ((eq? dst  STK)  (PUSH src))
       ((eq? dst 'CALL)
	 (cond
	   ((label? src) (CALL src))
	   (else         (CALL "*" src))))
       (else
	 (if (label? src)
	   (set! src (string-append "_" ('_to_string ('name src)) "@GOTPCREL(%rip)"))) ;; FIXME
	 (MOV_ src dst args))))

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

    (define-method isn-stream ('&c-func self env name params body)
      (let ((dst RESULT) 
	     (this (LABEL (or name ('output-name self))))
	     (start (LABEL)) 
	     (return (LABEL)) 
	     (end (LABEL)))
	(if (null? body)
	  (set! body `(,UNSPEC)))

	;; Bind params.
	('pass= self 'bind-params)
	('bind-params env params)
	;; Find variable contours in body.
	('pass= self 'variable-contour)
	('expr-body self env RESULT body)
	;; Allocate space for bindings.
	('pass= self 'allocate-bindings)
	('allocate-bindings env)
	('expr-body self env RESULT body)

	;; Preamble
	('pass= self 'emit)
	(TEXT)
	(ALIGN 4) ; ???
	(LABEL: this)
	(COMMENT (QUOTE params))
	(LABEL: start)
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
	('expr-body self env RESULT body)
	;; Postamble
	(LABEL: return)
	(LEAVE)
	(RET)
	(LABEL: end)
	(if (not ('main self))
	  ('main= self this))
	this
	))
    
    (define-method isn-stream ('expr-c-func self env dst e)
      (let ((name (cadr e))
	     (formals (caddr e))
	     (body    (cdddr e)))
	('c-func self env name formals body)))

    (define-method isn-stream ('expr-c-extern self env dst e)
      (MOV (LABEL e) dst))

    (define-method isn-stream ('expr-body self env dst body)
      (while (pair? (cdr body))
	(ED VOID (car body))
	(set! body (cdr body)))
      (ED dst (car body)))

    (define-method isn-stream ('method-func self env dst params)
      ('c-func self env dst (cons '&msg params)))
   
    (define-method isn-stream ('expr-c-call self env dst e)
      ;; (debug 'expr-c-call)(debug ('pass self))(debug e)
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
		       (ED (if (< (cdr ei) (vector-length arg-regs))
			    (vector-ref arg-regs (cdr ei))
			    (begin
			      (set! sp-offset (+ sp-offset word-size))
			      STK))
			 (car ei))
		       ) arg-i)
	   ;; (debug arg-i)(debug sp-offset)
	   (ED 'CALL func)
	   (if (> sp-offset 0)
	       (ADD (CONST sp-offset) SP))
	   (MOV RESULT dst)))
	(else
	  (for-each (lambda (e) (E e)) e)
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

    (define-method isn-stream ('expr-c-ptr<- self env dst e)
      (ED RESULT e)
      (case ('pass self)
	((emit)
	  (MOV (OFFSET RESULT 0) dst "\t// *tort_P(" RESULT ")")) ;; dst = ptr->data 
	))

    (define-method isn-stream ('expr-c-ptr-> self env dst e)
      (set! e `(&c-call (&c-extern tort_ptr_new) ,e))
      (E e))

    (define-method isn-stream ('expr-contents self env dst e)
      (ED RESULT e)
      (case ('pass self)
	((emit)
	  (MOV (OFFSET RESULT (- locative-tag)) dst "\t// *tort_L(" RESULT ")"))))

    (define-method isn-stream ('expr-set-contents! self env dst e)
      (ED TMP0 e)
      (case ('pass self)
	((emit)
	  (MOV dst (OFFSET TMP0 (- locative-tag)) "\t// *tort_L(" TMP0 ") = " dst))))

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
	    ((set!)  (E            (caddr e))
	             (S 'expr-set! (cadr e)))
	    ((lambda)    (S 'expr-lambda (cadr e) (caddr e)))
	    ((&c-func)   (S 'expr-c-func e))
	    ((&c-extern) (S 'expr-c-extern (cadr e)))
	    ((&c-call)   (S 'expr-c-call (cdr e)))
	    ((&c-ptr<-)  (S 'expr-c-ptr<- (cadr e)))
	    ((&c-ptr->)  (S 'expr-c-ptr-> (cadr e)))
	    ((&c-int<-)  (E (cadr e))
	                 (SAR (CONST tag-bits) dst))
	    ((&c-int->)  (E (cadr e)) 
	                 (SAL (CONST tag-bits) dst)
	                 (OR  (CONST fixnum-tag) dst))
	    ((&+)
	      (let ((a (gensym)) (b (gensym)))
		(E `(let ((,a ,(cadr e)) (,b ,(caddr e)))
		      (MOV ,a ,dst)
		      (ADD ,b ,dst)
		      ))))
	    ((&contents) (S 'expr-contents (cadr e)))
	    ((&set-contents!) 
	      (E                     (caddr e))
	      (S 'expr-set-contents! (cadr e)))
	    (else
	      (cond
		((and (pair? (car e)) (eq? (caar e) 'lambda))
		  ;; FIXME!
		  (E `(let ,(map (lambda (list (car e) (cadr e))) (cadar e))
			@,(cdr e))))
		(else
		  (S 'expr-c-call e)))))
	  (S form . args)
	  )))

    (define-method isn-stream ('expr-let self env dst e)
      ;; (debug ('pass self))(debug e)
      (let ((bindings (cadr e)) (body (cddr e))
	     (subenv ('subenv env e))
	     (alloc-offset ('alloc-offset env)))
	(case ('pass self)
	  ((variable-contour)
	   (for-each (lambda (b)
		       ('add subenv ('new env-binding 'name (car b))))
		     bindings)
	   ))
	
	(for-each (lambda (b) 
		    (ED                     RESULT (cadr b))
		    ('expr-set! self subenv RESULT (car  b)))
		  bindings)
	
	(for-each (lambda (e) ('expr self subenv dst e)) body)
	
	(if (eq? ('pass self) 'allocate-bindings) ('allocate-bindings subenv))
	('alloc-offset= env alloc-offset)
	))

    (define-method isn-stream ('expr-if self env dst e)
      (case ('pass self)
	((emit)
	  (let ((Lfalse (LABEL)) (Lend (LABEL))
		 (test-expr  (cdr e))
		 (true-expr  (cddr e))
		 (false-expr (cdddr e)))
	    (E (car test-expr))
	    (MOV (LITERAL #f) TMP0)
	    (CMP TMP0 dst)
	    (JE Lfalse)
	    (E (car true-expr))
	    (JMP Lend)
	    (ALIGN 4)
	    (LABEL: Lfalse)
	    (E (if (pair? false-expr) 
		 (car false-expr) UNSPEC))
	    ;; (ALIGN 4)
	    (LABEL: Lend)))
	(else
	  (for-each (lambda (e) (E e)) (cdr e))))
      )

#|
    (let-macro (
		 ((form name-args . body)  
		   `('set compiled-forms ',(car name-args) (lambda (self env dst ,@(cdr name-args)) ,@body))))
      (form (if test t . body) #f)
      (form (while test . body) #f)
      (form (lambda args . body) #f)
      ) ;; let-macro
    (debug compiled-forms)
|#

    (define-method isn-stream ('compile self env e)
      (if (null? env)
	(set! env ('new environ)))
      ('env= self env)
      (set! e (compiler:rewrite-1 e))
      (set! e (compiler:rewrite-2 e))
      (ED RESULT e)
      self)

    (define-method isn-stream ('assemble c . options)
      (let ((name ('output-name c))
	     (verbose (not (null? options)))
	     (fname '()))
	;; (set! verbose #t)
	(set! fname (string-append "tmp/" name))
	(let ((sfile (string-append fname ".s"))
	       (ofile (string-append fname ".o"))
	       (dfile (string-append fname ".dylib"))
	       )
	  ;;(display "sfile ")(write sfile)(newline)
	  ;;(display "ofile ")(write ofile)(newline)
	  (let ((name-sym (string->symbol name))
		 (st '())
		 (func-ptr '())
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
	 (fname '()))
	(set! verbose #t)
	(set! fname (string-append "tmp/" name))
	(let ((sfile (string-append fname ".s"))
	       (ofile (string-append fname ".o"))
	       (dfile (string-append fname ".dylib"))
	       )
	  (let ((name-sym (string->symbol name))
		 (st '())
		 (func-ptr '())
		 (result '()))
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

(define (void x))

(void ('load <dynlib> "/usr/lib/libc")) ;; printf
(define (test-compiler expr args expect)
  (let ((s ('new isn-stream)))
    (display "# ==============================================\n")
    (debug expr)
    ('compile s '() expr)
    (display "\nCode:\n")(display ('body s))
    (display "\nEnv Bindings:\n")
    (for-each (lambda (b)
		(write b)(newline))
      ('binding-list ('env s)))
    (display "\nLiterals:\n")
    (for-each (lambda (b)
		(write b)(newline))
      ('literals s))
    ('assemble s)
    (let ((func ('load s))
	   (result #f))
      (debug func)(debug args)
      (set! result ('_ccallv func args))
      (debug result)
      (if expect
	(begin
	  (set! expect (car expect))
	  (debug expect)
	  (if (not (equal? result expect))
	    (error "result != expect"))))
      result
      )
    ))

(test-compiler
  '(&c-func #f ()
     1)
  (vector)
  '(1))

(test-compiler
  '(&c-func #f ()
     1
     2
     3)
  (vector)
  '(3))

(test-compiler
  '(&c-func #f ()
     (if #t 'true))
  (vector)
  '(true))

(test-compiler
  '(&c-func #f ()
     (if #f 'true))
  (vector)
  '(())) ;; unspec

(test-compiler
  '(&c-func #f ()
     (&c-int-> (&c-call (&c-extern tort_hello_world))))
  (vector)
  '(42))

(test-compiler
  '(&c-func #f ()
     (&c-call (&c-extern tort_prints) (&c-ptr<- "123\n"))
     'ok)
  (vector)
  '(ok))

(test-compiler
  '(&c-func #f (func str)
     (&c-call (&c-ptr<- func) (&c-ptr<- str))
     'ok)
  (vector (%extern 'tort_prints) "456\n")
  '(ok))

(test-compiler
  '(&c-func #f ()
     (&c-call (&c-extern tort_string_new_cstr) (&c-call (&c-extern tort_tlisp_version))))
  (vector)
  '("tlisp 1.0"))

(test-compiler
  '(&c-func #f (a b)
     (&c-int-> (&+ (&c-int<- a) (&c-int<- b))))
  (vector 2 3)
  '(5))

(test-compiler
  '(&c-func #f (a b c d)
     (&c-int-> (&+ 
		 (&+ (&c-int<- a) (&c-int<- b)) 
		 (&+ (&c-int<- c) (&c-int<- d)))))
  (vector 1 2 3 4)
  '(10))

(test-compiler
  '(&c-func #f ()
     (&c-ptr-> (&c-extern tort_prints)))
  (vector)
  (list (%extern 'tort_prints)))

(test-compiler
  '(&c-func #f (a b c d e f g h i)
     a c e g h i
     (let ((x a) (y b)) x y)
     (if #t 
       (let ((x1 a) (y1 b)) x1 y1)
       (let ((x2 b) (y2 a) (z2 a)) x2 y2))
     (a (&c-ptr b) 2 3 4 5 6 7 8 9 10)
     'ok
     )
  (vector (%extern 'printf) ('_to_c_ptr "Hello World!\n"))
  '(ok))


