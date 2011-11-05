(define-struct compiler
  (stream        (string-new))
  (output-name   nil)
  (label-id      0)
  (ar-offset-    (list 0))
  (ar-offset-max 0)
  (saves         nil)
  (env           nil))
(define compiler:make compiler:new) ;; alias FIXME

(define (compiler:ar-offset c) 
  (car (compiler:ar-offset- c)))
(define (compiler:ar-offset= c v)
  (if (< v (compiler:ar-offset-max c))
      (compiler:ar-offset-max= c v))
  (set-car! (compiler:ar-offset- c) v))
(define (compiler:ar-offset:push c) 
  (compiler:ar-offset-= c 
			    (cons (compiler:ar-offset c) (compiler:ar-offset- c))))
(define (compiler:ar-offset:push c) 
  (compiler:ar-offset-= c 
			    (cdr (compiler:ar-offset- c))))
(define (compiler:env:push c b)
  (compiler:env= c
		     (cons b (compiler:env c))))

(define (compiler:label c)
  (let ((id (compiler:label-id c)))
    (let ((label (string-append "L" (number->string id))))
      (compiler:label-id= c (+ id 1))
      label)))

(define (compiler:emit c . args)
  (set! c (compiler:stream c))
  (for-each (lambda (x)
	      (display x c)) 
	    args)
  (newline c))

(define (compiler:global-symbol c o)
  (if (symbol? o)
      (set! o (symbol->string o)))
  (string->symbol 
   (string-append "_" o)))

(define (compiler:method:label c o)
  (string-append "_tort_x_" ('_to_string ('_object_ptr o))))

(define compiler:object:header-size ('_object_header_size '()))
(define compiler:message:alloc-size (+ compiler:object:header-size ('_alloc_size ('new <message>))))
(define (compiler:type:slot-offset type slot)
  ((string->symbol (string-append "_offset_" (symbol->string slot))) type))

(define (compiler:reg:offset reg type slot)
  (string-append
   (object->string (compiler:type:slot-offset type slot))
   "(" reg ")"))

(let ((word-size 8)
      (_msg     "%rbx")
      (_rcvr    "%r12")
      (ar-reg   "%rbp")
      (sp-reg   "%rsp")
      (tmp0-reg "%r14")
      (msg      "%r15")
      (meth     "%rax")
      (rtn-reg  "%rax")
      (arg-regs '#("%rdi" "%rsi" "%rdx" "%rcx" "%r8" "%r9"))
      (callee-regs '#()))
  (let (
	(_msg->selector        (compiler:reg:offset _msg <message> 'selector))
	(_msg->argc            (compiler:reg:offset _msg <message> 'argc))
	(_msg->mtable          (compiler:reg:offset _msg <message> 'mtable))
	(_msg->method          (compiler:reg:offset _msg <message> 'method))
	(arg0-reg (vector-ref arg-regs 0))
	(arg1-reg (vector-ref arg-regs 1))
	(arg2-reg (vector-ref arg-regs 2))
	(msg->previous_message (compiler:reg:offset msg <message> 'previous_message))
	(msg->selector         (compiler:reg:offset msg <message> 'selector))
	(msg->receiver         (compiler:reg:offset msg <message> 'receiver))
	(msg->argc             (compiler:reg:offset msg <message> 'argc))
	(msg->method           (compiler:reg:offset msg <message> 'method))
	(meth->applyf          (compiler:reg:offset meth <method> 'applyf))
	)

    (define (compiler:bind c name . ar-offset)
      (let ((loc #f)
	    (binding #f))
	;; Allocate offset?
	(set! ar-offset (and (pair? ar-offset) (car ar-offset)))
	(if (or (not ar-offset) (null? ar-offset))
	    (begin
	      (set! ar-offset (- (compiler:ar-offset c) word-size))
	      (compiler:ar-offset= c ar-offset)))
	(if (number? ar-offset)
	    (set! loc (string-append (number->string ar-offset) "(" ar-reg ")"))
	    (set! loc ar-offset))
	    
	(set! binding 
	      (vector 
	       name      ; 0 env-name
	       loc       ; 1 reg or dst
	       ar-offset ; 2 ar-offset
	       #f        ; 3 restore reg
	       ))
	(compiler:env:push c binding)
	;;(display "  binding = ")(write binding)(newline)
	binding))

    (define (compiler:env:binding c n)
      (let ((l (compiler:env c))
	    (e #f)
	    (b #f))
	(while (not (null? l))
	       (set! e (car l))
	       (if (eq? n (vector-ref e 0))
		   (begin
		     (set! b e)
		     (set! l '()))
		   (set! l (cdr l))))
	b))

    (define (compiler:compile:method c o)
      (let ((mname (compiler:method:label c o))
	    (args (car o))
	    (save-regs (list _msg _rcvr tmp0-reg)))
	(set! args (cons '&msg args))
	(compiler:output-name= c mname)
	(set! mname (compiler:global-symbol c mname))
	(compiler:emit c "  .text")
	(compiler:emit c "  .align 4,0x90") ; ???
	(compiler:emit c "  .globl " mname)
	(compiler:emit c "  " mname ':)
	;; C method(_tort_message:%rdi->%rbx, rcvr:%rsi->%r12, ...args)

	;; save caller's frame pointer:
	(compiler:emit c "pushq " ar-reg)
	;; set frame pointer to current stack pointer:
	(compiler:emit c "movq  " sp-reg ", " ar-reg)

	;; Save parameter registers as unnamed bindings:
	(let ((a args)
	      (rest-arg #f)
	      (arg nil)
	      (loc nil)
	      (binding nil)
	      (arg-i -1)
	      (reg #f)
	      (arg-offset (+ word-size word-size))) ; prev-ar rtn-addr
	  ;; (display "args = ")(write args)(newline)
	  ;; (display "  (vector-length arg-regs) = ")(write (vector-length arg-regs))(newline)
	  (while (not (null? a))
	    (set! arg-i (+ arg-i 1))
	    ;; (display "  a = ")(write a)(newline)
	    ;; (display "  arg-i = ")(write arg-i)(newline)
	    (cond
	     ((symbol? a)
	      (set! arg a)
	      (set! a '())
	      (set! rest-arg a)
	      (set! loc nil)
	      (set! reg #f))
	     (else
	      (set! arg (car a))
	      (set! a (cdr a))
	      (cond
	       ((< arg-i (vector-length arg-regs))
		(set! loc #f) ;; allocate temporary on stack.
		(set! reg (vector-ref arg-regs arg-i)))
	       (else
		(set! loc arg-offset) ;; location is relative to ar-reg.
		(set! reg #f)
		(set! arg-offset (+ arg-offset word-size))))))
	    ;; (display "    arg = ")(write arg)(newline)
	    ;; (display "    loc = ")(write loc)(newline)
	    ;; (display "    reg = ")(write reg)(newline)
	    (set! binding (compiler:bind c arg loc))
	    ;; (display "      binding = ")(write binding)(newline)
	    ;; save reg to stack binding.
	    (if reg
		(compiler:emit c "movq  " reg ", " (vector-ref binding 1) " \t// -> " binding)
	        (compiler:emit c "                      \t// -> " binding))
	    )
	  ;; FIXME: handle rest-arg.
	  )
	
	;; Save registers as unnamed bindings:
	(for-each (lambda (reg)
		    (let ((binding (compiler:bind c nil)))
		      (vector-set! binding 3 (string-append "movq  " (vector-ref binding 1) ", " reg))
		      ;; (display "  binding = ")(write binding)(newline)
		      (compiler:emit c "movq  " reg ", " (vector-ref binding 1))))
		  save-regs)

	;; Space for register saves:
	(let ((ar-offset (compiler:ar-offset-max c)))
	  ;; (display "ar-offset = ")(write ar-offset)(newline) 
	  ;; align to 16-byte stack frames.
	  (set! ar-offset (* (/ (+ ar-offset -15) 16) 16))
	  ;; (display "ar-offset aligned = ")(write ar-offset)(newline) 
	  (compiler:emit c "subq  $" (- ar-offset) ", " sp-reg))

	;; Move _tort_message (arg0) to _msg reg:
	(compiler:emit c "movq  " arg0-reg ", " _msg " // _tort_message")
	;; Move rcvr (arg1) to _rcvr reg:
	(compiler:emit c "movq  " arg1-reg ", " _rcvr " // _tort_rcvr")
	
	;; Compile method body:
	(compiler:compile:method:body c o)

	;; Restore bindings:
	(for-each (lambda (binding)
		    (let ((restore-reg (vector-ref binding 3)))
		      (if restore-reg
			  (compiler:emit c restore-reg))))
		  (compiler:env c))

	;; Restore caller's frame pointer and return.
	(compiler:emit c "leave")
	(compiler:emit c "ret")
	c
	))

    (define (compiler:compile:method:body c o)
      (let ((args (car o))
	    (body (cdr o)))
	(for-each (lambda (stmt)
		    (compiler:compile:expr c stmt))
		  body)))

    (define (compiler:compile:expr c o)
      (compiler:compile:expr-dst c o rtn-reg))

    (define (compiler:compile:expr-dst c o dst)
      (cond
       ((number? o)
	(compiler:compile:number c o dst))
       ((pair? o)
	(compiler:compile:pair c o dst))
       ((symbol? o)
	(compiler:compile:symbol c o dst))
       (else
	(compiler:compile:reference c o dst)))
      )

    (define (compiler:compile:send c sel rcvr args)
      ;; (compiler:emit c "  // (send " sel " " rcvr " " args ")")
      ;; Create message object on stack:
      ;;   
      ;;   Save msg reg:
      (compiler:emit c "pushq " msg)
      ;;   Allocate new message object on stack:
      (compiler:emit c "subq  $" compiler:message:alloc-size ", " sp-reg)
      (compiler:emit c "movq  " sp-reg "," msg)
      ;;   Add object header offset:
      (compiler:emit c "addq  $" compiler:object:header-size ", " msg)
      
      (set! args
	    (let ((arg-i 1)) ; skip first real args: (_msg rcvr)
	      (map (lambda (arg-expr)
		     (set! arg-i (+ arg-i 1))
		     (list arg-i arg-expr))
		   args)))
      
      ;; Initialize message object:
      ;;
      ;;   previous_message:
      (compiler:emit c "movq  " _msg ", " msg->previous_message)
      ;;   selector:
      (compiler:compile:expr c sel)
      (compiler:emit c "movq  " rtn-reg ", " msg->selector)
      ;;   argc:
      (let ((argc (+ (length args) 1))) ; + 1 for rcvr
	(compiler:compile:expr-dst c argc msg->argc))
      ;;   receiver:
      (compiler:compile:expr c rcvr)
      (compiler:emit c "movq  " rtn-reg ", " msg->receiver)

      ;; Invoke _tort_lookup:
      ;; message = _tort_lookup(_tort_message, rcvr, message)
      (compiler:emit c "movq  " rtn-reg ", " arg1-reg " \t// <- rcvr")
      (compiler:emit c "movq  " msg     ", " arg2-reg " \t// <- message")
      (compiler:emit c "movq  " _msg    ", " arg0-reg " \t// <- _tort_message")
      (compiler:emit c "call  " (compiler:global-symbol c '_tort_lookup))
      (compiler:emit c "movq  " rtn-reg ", " msg " \t// -> message")

      ;; Invoke method->func(message, rcvr, args ...):
      ;;
      ;; Emit arguments in reverse order.
      (let ((stack-arg-count 0))
	(for-each (lambda (arg)
		    (if (eq? 'STACK (compiler:compile:caller-arg c arg))
			(set! stack-arg-count (+ stack-arg-count 1))
			))
		  (reverse args))
	;; receiver:
	(compiler:emit c "movq  " msg->receiver ", " arg1-reg " \t// <- rcvr")
	;; message:
	(compiler:emit c "movq  " msg ", " arg0-reg " \t// <- msg")
	
	;; msg->method->applyf(msg, rcvr, ...) 
	(compiler:emit c "movq  " msg->method ", " meth " \t// msg->method => meth")
	(compiler:emit c "call  *" meth->applyf "      \t// meth->apply(msg, rcvr, args...) ")
	
	;; Pop args sp:
	(if (> stack-arg-count 0)
	    (compiler:emit c "addq  $" (* 8 stack-arg-count) ", " sp-reg "    \t// pop args"))
	)

      ;; Reclaim message space:
      (compiler:emit c "addq  $" compiler:message:alloc-size ", " sp-reg "    \t// pop *msg")
      ;;   Restore msg reg:
      (compiler:emit c "popq  " msg)
      )

    (define (compiler:compile:caller-arg c arg)
      (let ((arg-i    (car arg))
	    (arg-expr (cadr arg))
	    (arg-type #f))
					; (compiler:emit c "  // arg #" arg-i)
	(set! arg-type (if (< arg-i (vector-length arg-regs))
			   (vector-ref arg-regs arg-i)
			   'STACK))
	(compiler:compile:expr-dst c arg-expr arg-type)
	arg-type))

    (define (compiler:compile:pair c o dst)
      (compiler:emit c "// pair => " dst)
      (cond
       ((eq? (car o) 'quote)
	(compiler:compile:quote c (cadr o) dst))
       ((eq? (car o) 'set!)
	(compiler:compile:set! c (cadr o) (caddr o) dst))
       ((eq? (car o) '&root)
	(compiler:compile:pair c (list ''get &root (cadr o)) dst))
       ((eq? (car o) 'if)
	(compiler:compile:if c o dst))
       ((eq? (car o) 'while)
	(compiler:compile:while c o dst))
       ((eq? (car o) 'or)
	(compiler:compile:or c o dst))
       ((eq? (car o) 'and)
	(compiler:compile:and c o dst))
       ((eq? (car o) 'begin)
	(for-each (lambda (stmt)
		    (compiler:compile:expr-dst c stmt dst))
		  (cdr o)))
       (else
	(let ((sel (car o))
	      (rcvr (cadr o))
	      (args (cddr o)))
	  (cond
	   ((and (pair? sel) (eq? (car sel) 'quote) (symbol? (cadr sel)))
	    (compiler:compile:send c sel rcvr args))
	   (else
	    (compiler:compile:send c sel rcvr args))))
	;; send always emits to dst=rtn-reg.
	(cond
	 ((eq? dst 'STACK)
	  (compiler:emit c "pushq " rtn-reg))
	 ((eq? dst rtn-reg))
	 (else
	  (compiler:emit c "movq  " rtn-reg ", " dst)))
	)
       ))

    (define (compiler:compile:or c o dst)
      (let ((Lend (compiler:label c))
	    (exprs (cdr o)))
	(while (not (null? exprs))
	       ;; cmpq expects 16-bit constant, so use a register for #f value.
	       (compiler:compile:expr-dst c #f tmp0-reg)
	       (compiler:compile:expr-dst c (car exprs) dst)
	       (compiler:emit c "cmpq  " tmp0-reg ", " dst)
	       (compiler:emit c "jne   " Lend)
	       (set! exprs (cdr exprs)))
	(compiler:compile:expr-dst c #f dst)
	(compiler:emit c "  .align 4,0x90")
	(compiler:emit c "  " Lend ":")))

    (define (compiler:compile:and  c o dst)
      (let ((Lend (compiler:label c))
	    (exprs (cdr o)))
	(compiler:compile:expr-dst c #f dst)
	(while (not (null? exprs))
	       ;; cmpq expects 16-bit constant, so use a register for #f value.
	       (compiler:compile:expr-dst c #f tmp0-reg)
	       (compiler:compile:expr-dst c (car exprs) dst)
	       (compiler:emit c "cmpq  " tmp0-reg ", " dst)
	       (compiler:emit c "je    " Lend)
	       (set! exprs (cdr exprs)))
	(compiler:emit c "  .align 4,0x90")
	(compiler:emit c "  " Lend ":")))

    (define (compiler:compile:if c o dst)
      (let ((Lfalse (compiler:label c))
	    (Lend   (compiler:label c))
	    (test-expr  (cdr o))
	    (true-expr  (cddr o))
	    (false-expr (cdddr o))
	    (false-value (compiler:constant:object c #f))
	    )
	;; cmpq expects 16-bit constant, so use a register for #f value.
	(compiler:compile:expr-dst c #f tmp0-reg)
	(set! false-value tmp0-reg)
	(compiler:compile:expr-dst c (car test-expr) dst)
	(compiler:emit c "cmpq  " false-value ", " dst)
	(compiler:emit c "je    " Lfalse)
	(compiler:compile:expr-dst c (car true-expr) dst)
	(cond 
	 ((pair? false-expr)	
	  (compiler:emit c "jmp   " Lend)
	  (compiler:emit c "  .align 4,0x90")
	  (compiler:emit c "  " Lfalse ":")
	  (compiler:compile:expr-dst c (car false-expr) dst))
	 (else
	  (compiler:emit c "  .align 4,0x90")
	  (compiler:emit c "  " Lfalse ":")))
	(compiler:emit c "  " Lend ":")))

    (define (compiler:compile:while c o dst)
      (let ((Lend (compiler:label c))
	    (Lagain   (compiler:label c))
	    (test-expr  (cdr o))
	    (body-expr  (cddr o))
	    (false-value (compiler:constant:object c #f))
	    )
	(compiler:emit c "  .align 4,0x90")
	(compiler:emit c "  " Lagain ":")
	;; cmpq expects 16-bit constant, so use a register for #f value.
	(compiler:compile:expr-dst c #f tmp0-reg)
	(set! false-value tmp0-reg)
	(compiler:compile:expr-dst c (car test-expr) dst)
	(compiler:emit c "cmpq  " false-value ", " dst)
	(compiler:emit c "je    " Lend)
	(for-each (lambda (stmt)
		    (compiler:compile:expr-dst c stmt dst))
		  body-expr)
	(compiler:emit c "jmp   " Lagain)
	(compiler:emit c "  .align 4,0x90")
	(compiler:emit c "  " Lend ":")))

    (define (compiler:compile:symbol c o dst)
      (cond
       ((eq? o '&root)
	(compiler:compile:reference c &root dst))
       (else
	(let ((b (compiler:env:binding c o)))
	  (if b
	      (cond
	       ((eq? dst 'STACK)
		(compiler:emit c "pushq " (vector-ref b 1) "       \t// <- " b))
	       (else
		(compiler:emit c "movq  " (vector-ref b 1) ", " dst " \t// <- " b)))
	      (compiler:compile:global c o dst)
	      )))))

    (define (compiler:compile:set! c o v dst)
      (compiler:emit c "  // (set! " o " " v ") using " dst)
      (compiler:compile:expr-dst c v dst)
      (let ((b (compiler:env:binding c o)))
	(if b
	    (compiler:emit c "movq  " dst ", " (vector-ref b 1) " \t// -> " b)
	    (compiler:compile:set!:global c o dst) ; FIXME: global?
	    ))
      (cond
       ((eq? dst 'STACK)
	 (compiler:emit c "// dst = " dst)
	 ;; (compiler:emit c "pushq " dst)
	 )))

    (define (compiler:compile:global c o dst)
      (compiler:compile:reference c o dst) ; FIXME: global?
      )

    (define (compiler:compile:set!:global c o dst)
      (compiler:compile:reference c o dst) ; FIXME: global?
      )

    (define (compiler:compile:quote c o dst)
      (cond
       ((number? o)
	(compiler:compile:number c o dst))
       (else
	(compiler:compile:reference c o dst))))

    (define (compiler:compile:number c o dst)
      (compiler:compile:literal c (compiler:constant:number c o) dst))

    (define (compiler:compile:reference c o dst)
      (compiler:compile:literal c (compiler:constant:reference c o) dst))

    (define (compiler:compile:literal c o dst)
      (if (eq? dst 'STACK)
	  (compiler:emit c "pushq " o)
	  (compiler:emit c "movq  " o ", " dst)))

    (define (compiler:constant:object c o)
      (cond
       ((number? o)
	(compiler:constant:number c o))
       (else
	(compiler:constant:reference c o))))

    (define (compiler:constant:number c o)
      (string-append "$" (object->string (| (+ o o) 1)))) ; |

    (define (compiler:constant:reference c o)
      (string-append "$0x" ('_to_string ('_object_ptr o))))

)) ; let) let)


	(define (compiler:compile:literal:string c o)
	  (let ((v   (compiler:label c))
		 (s   (compiler:label c)))
	    (compiler:emit c "  .data")
	    (compiler:emit c "  " v ":")
	    (compiler:emit c "  .asciiz" (object->string o))
	    (compiler:emit c "  " s ":")
	    (compiler:emit c "  .qword $0")
	    (compiler:emit c "  .text")
	    ))
	
		     (define (compiler:box:int c dst)
					; int expression is in dst,
		       (compiler:emit c "addq  " dst ", " dst)
		       (compiler:emit c "orq   $1, " dst))

		     (define (compiler:unbox:int c dst)
					; int expression is in dst,
		       (compiler:emit c "sarq  " dst))

		     (define (compiler:assemble c . options)
		       (let ((name (compiler:output-name c))
			     (verbose (not (null? options)))
			     (fname nil))
			 (set! fname (string-append "tmp/" name))
			 (let ((sfile (string-append fname ".s"))
			       (ofile (string-append fname ".o"))
			       (dfile (string-append fname ".dylib"))
			       )
					;(display "sfile ")(write sfile)(newline)
					;(display "ofile ")(write ofile)(newline)
			   (let ((name-sym (string->symbol name))
				 (st nil)
				 (func-ptr nil)
				 (result #f))
			     (call-with-output-file sfile (lambda (f)
							    (display (compiler:stream c) f)))
			     (if verbose
				 (begin
				   (display "Assembly:\n") 
				   (display (compiler:stream c))
				   (display "=====\n")))

			     (posix:system (string-append "gcc "
					; (if verbose "--verbose" "")
							  " -D__DYNAMIC__ -fPIC -DPIC "
							  " -export-dynamic -fno-common -c -o " ofile " " sfile))
					; (if verbose (posix:system (string-append "otool -tv " ofile)))
			     (posix:system (string-append "gcc "
					; (if verbose "--verbose" "")
							  " -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o " dfile " " ofile " -compatibility_version 1 -current_version 1.0 -Wl,-single_module"))
					; (if verbose (posix:system (string-append "otool -tv " dfile)))
			     (set! result dfile)
			     (display "compile:assemble => ")(write result)(newline)
			     result
			     ))))

		     (define (compiler:load c . options)
		       (let ((name (compiler:output-name c))
			     (verbose (not (null? options)))
			     (fname nil))
			 (set! fname (string-append "tmp/" name))
			 (let ((sfile (string-append fname ".s"))
			       (ofile (string-append fname ".o"))
			       (dfile (string-append fname ".dylib"))
			       )
			   (let ((name-sym (string->symbol name))
				 (st nil)
				 (func-ptr nil)
				 (result nil))
			     (set! st ('_dlopen (string-append "./" dfile)))
			     ;;(display "st = ")(write st)(newline)
			     ;;(display "name-sym = ")(write name-sym)(newline)
			     ;;(display "name-sym class = ")(write (%get-type name-sym))(newline)
			     ;;('__debugger st) (set! &trace 1)
			     (set! func-ptr ('get st name-sym))
			     (if (or #t verbose) (begin (display "  func-ptr = ")(write func-ptr)(newline)))
			     (set! result func-ptr)
			     result
			     ))))

;;;;
