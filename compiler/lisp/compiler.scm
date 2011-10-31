(define compiler:make
  (lambda args 
    (list 
     ""   ; output stream
     nil  ; output name
     0    ; next label id
     0    ; saves-ar-offset
     nil  ; saves
     )))

(define compiler:output-name 
  (lambda (c)
    (car (cdr c))))

(define compiler:set-output-name!
  (lambda (c v)
    (set-car! (cdr c) v)))

(define compiler:stream
  (lambda (c)
    (car c)))

(define compiler:set-stream!
  (lambda (c s)
    (set-car! c s)))

(define compiler:label-id
  (lambda (c)
    (caddr c)))

(define compiler:set-label-id!
  (lambda (c v)
    (set-car! (cddr c) v)))

(define compiler:saves-ar-offset
  (lambda (c)
    (car (cdddr c))))

(define compiler:set-saves-ar-offset!
  (lambda (c v)
    (set-car! (cdddr c) v)))

(define compiler:label
  (lambda (c)
    (let ((id (compiler:label-id c)))
      (let ((label (string-append "L" (number->string id))))
	(compiler:set-label-id! c (+ id 1))
	label))))

(define compiler:emit 
  (lambda (c . args)
    (set! c (compiler:stream c))
    (for-each (lambda (x)
		(display x c)) 
	      args)
    (newline c)
    ))

(define compiler:global-symbol
  (lambda (c o)
    (if (symbol? o)
	(set! o (symbol->string o)))
    (string->symbol 
     (string-append "_" o))))

(define compiler:method:label
  (lambda (c o)
     (string-append "_tort_x_" ('_to_string ('_object_ptr o)))))

(define compiler:object:header-size ('_object_header_size '()))
(define compiler:message:alloc-size (+ compiler:object:header-size ('_alloc_size ('new <message>))))
(define compiler:type:slot-offset
  (lambda (type slot)
    ((string->symbol (string-append "_offset_" (symbol->string slot))) type)))

(define compiler:reg:offset 
  (lambda (reg type slot)
    (string-append
     (object->string (compiler:type:slot-offset type slot))
     "(" reg ")"
     )))

(let ((_msg "%rbx")
      (_rcvr "%r12")
      (ar-reg "%rbp")
      (sp-reg "%rsp")
      (tmp0-reg "%r14")
      (msg "%r15")
      (meth "%rax")
      (rtn-reg  "%rax")
      (arg-regs '#("%rdi" "%rsi" "%rdx" "%rcx" "%r8" "%r9"))
      )
  (let (
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

(define compiler:save-reg
  (lambda (c reg)
    (let ((offset (compiler:saves-frame-offset c)))
      (set! offset (- offset 8))
      (compiler:emit c "movq  " reg ", $" offset "(" ar-reg ")")
      (compiler:set-saves-frame-offset! offset)
    )))

(define compiler:compile:method 
  (lambda (c o)
    (let ((mname (compiler:method:label c o)))
      (compiler:set-output-name! c mname)
      (set! mname (compiler:global-symbol c mname))
      (compiler:emit c ".text")
      (compiler:emit c ".globl " mname)
      (compiler:emit c mname ':)
      ;; C method(_tort_message:%rdi->%rbx, rcvr:%rsi->%r12, ...args)

      ;; save caller's frame pointer:
      (compiler:emit c "pushq " ar-reg)
      ;; set frame pointer to current stack pointer:
      (compiler:emit c "movq  " sp-reg ", " ar-reg)

      ; Save registers:
      (compiler:emit c "movq  " _msg ", -8(" ar-reg ")")
      (compiler:emit c "movq  " _rcvr ", -16(" ar-reg ")")
      (compiler:emit c "movq  " tmp0-reg ", -24(" ar-reg ")")

      ; Space for register saves:
      (compiler:emit c "subq  $24, " sp-reg)
      ;(compiler:emit c "subq $xxx, " sp-reg)         ; Save space for local variables.

      ; Move _tort_message to %rbx
      (compiler:emit c "movq  " arg0-reg ", " _msg " // _tort_message")
      ; Move rcvr to %r12:
      (compiler:emit c "movq  " arg1-reg ", " _rcvr " // _tort_rcvr")

      (compiler:compile:method:body c o)

      ;; Restore registers:
      (compiler:emit c "movq   -8(" ar-reg "), " _msg)
      (compiler:emit c "movq  -16(" ar-reg "), " _rcvr)
      (compiler:emit c "movq  -24(" ar-reg "), " tmp0-reg)

      ;; Restore caller's frame pointer and return.
      (compiler:emit c "leave")
      (compiler:emit c "ret")
      c
      )))

(define compiler:compile:method:body 
  (lambda (c o)
    (let ((args (car o))
	  (body (cdr o)))
      (for-each (lambda (stmt)
		  (compiler:compile:expr c stmt))
		body))))

(define compiler:compile:expr
  (lambda (c o)
    (compiler:compile:expr-dst c o rtn-reg)))

(define compiler:compile:expr-dst
  (lambda (c o dst)
    ; (compiler:emit c "// expr-dst: " (object->string o) " type:" ('_name (%get-type o)) " => " dst)
    ; (set! &trace 1)
     (cond
     ((number? o)
      (compiler:compile:number c o dst))
     ((pair? o)
      (compiler:compile:pair c o dst))
     (else
      (compiler:compile:constant c o dst)))
    ; (set! &trace 0)
    ))

(define compiler:compile:send
   (lambda (c sel rcvr args)
     (set! args
	   (let ((arg-i 1))
	     (map (lambda (arg-expr)
		    (set! arg-i (+ arg-i 1))
		    (list arg-i arg-expr))
		  args)))
     (compiler:emit c "// (send " sel " " rcvr " " args ")")
      ;; Create message object on stack:
      ;;   
      ;;   Save msg reg:
      (compiler:emit c "pushq " msg)
      ;;   Allocate new message object on stack:
      (compiler:emit c "subq  $" compiler:message:alloc-size ", " sp-reg)
      (compiler:emit c "movq  " sp-reg "," msg)
      ;;   Add object header offset:
      (compiler:emit c "addq  $" compiler:object:header-size ", " msg)

      ;; Initialize message object:
      ;;
      ;;   previous_message:
      (compiler:emit c "// _msg => msg->previous_message")
      (compiler:emit c "movq  " _msg ", " msg->previous_message)
      ;;   selector:
      (compiler:emit c "// sel " (object->string sel) " => msg->selector")
      (compiler:compile:expr c sel)
      (compiler:emit c "movq  " rtn-reg ", " msg->selector)
      ;(compiler:compile:expr-dst c sel msg->selector)

      ;;   argc:
      (let ((argc (+ (length args) 1))) ; + 1 for rcvr
	(compiler:emit c "// (length args) " argc " => msg->argc")
	(compiler:compile:expr-dst c argc msg->argc))

      ;;   receiver:
      (compiler:emit c "// rcvr " (object->string rcvr) " => msg->receiver")
      (compiler:compile:expr c rcvr)
      (compiler:emit c "movq  " rtn-reg ", " msg->receiver)

      ;; Invoke _tort_lookup:
      ;; message = _tort_lookup(_tort_message, rcvr, message)
      (compiler:emit c "movq  " rtn-reg ", " arg1-reg " // rcvr")
      (compiler:emit c "movq  " msg     ", " arg2-reg " // message")
      (compiler:emit c "movq  " _msg    ", " arg0-reg " // _tort_message")
      (compiler:emit c "call  " (compiler:global-symbol c '_tort_lookup))
      (compiler:emit c "movq  " rtn-reg ", " msg " // -> message")

      ;; Invoke method->func(message, rcvr, args ...):
      ;;
      ;; Emit arguments in reverse order.
      (map (lambda (arg)
	     (compiler:compile:caller-arg c arg))
	   (reverse args))

      ;; receiver:
      (compiler:emit c "movq  " msg->receiver ", " arg1-reg " // => rcvr")
      ;; message:
      (compiler:emit c "movq  " msg ", " arg0-reg " // => msg")

      ;; msg->method->applyf(msg, rcvr, ...) 
      (compiler:emit c "movq  " msg->method ", " meth " // msg->method => meth")
      ;;   Restore msg reg:
      (compiler:emit c "popq  " msg)
      (compiler:emit c "call  *" meth->applyf " // apply() ")

      ;;
      ;;   movq tort_message.method(%rbx), %rax
      ;;   movq tort_method.func(%rax), %rax
      ;;   call *(%rax)
      ;; 
      ;; Pop args sp:

      ;; Reclaim message space:
      ;;   Pop message space:
      (compiler:emit c "addq  $" compiler:message:alloc-size ", " sp-reg)

      (compiler:emit c "// (send ...): END")
    ))

(define compiler:compile:caller-arg 
  (lambda (c arg)
    (let ((arg-i    (car arg))
	  (arg-expr (cadr arg)))
      (compiler:emit c "// arg #" arg-i " => " (object->string arg-expr))
      (compiler:compile:expr-dst c arg-expr
				 (if (< arg-i (vector-length arg-regs))
				     (vector-ref arg-regs arg-i)
				     'STACK)))))

(define compiler:compile:pair
  (lambda (c o dst)
    (cond
     ((eq? (car o) 'quote)
      (compiler:compile:constant (cadr o) dst))
     ((eq? (car o) 'if)
      (let ((Lfalse (compiler:label c))
	    (Lend   (compiler:label c))
	    (test-expr  (cdr o))
	    (true-expr  (cddr o))
	    (false-expr (cdddr o)))
	(compiler:compile:expr-dst c (car test-expr) rtn-reg)
	(compiler:compile:expr-dst c #f tmp0-reg)
	(compiler:emit c "cmpq  " rtn-reg ", " tmp0-reg)
	(compiler:emit c "je    " Lfalse)
	(compiler:compile:expr-dst c (car true-expr) dst)
	(cond 
	 ((pair? false-expr)	
	  (compiler:emit c "jmp   " Lend)
	  (compiler:emit c Lfalse ":")
	  (compiler:compile:expr-dst c (car false-expr) dst))
	 (else
	  (compiler:emit c Lfalse ":")))
	(compiler:emit c Lend ":")
	))
     (else
      (compiler:compile:send c (car o) (cadr o) (cddr o))
      ;; send always emits to dst=rtn-reg.
      (cond
       ((eq? dst 'STACK)
	(compiler:emit c "pushq " rtn-reg))
       ((eq? dst rtn-reg))
       (else
	(compiler:emit c "movq  " rtn-reg ", " dst)))
      ))
     ))

(define compiler:compile:number
  (lambda (c o dst)
    ;; (compiler:emit c "// number " o)
    (compiler:compile:literal c (string-append "$" (object->string (| (+ o o) 1))) dst))) ;|

(define compiler:compile:constant
  (lambda (c o dst)
    ;; (compiler:emit c "// constant " (object->string o))
    (compiler:compile:literal c (compiler:constant c o) dst)))

(define compiler:constant
  (lambda (c o)
    (string-append "$0x" ('_to_string ('_object_ptr o)))))

(define compiler:compile:literal
  (lambda (c o dst)
    (if (eq? dst 'STACK)
	(compiler:emit c "pushq " o)
        (compiler:emit c "movq  " o ", " dst))))

)) ; let) let)


(define compiler:compile:literal:string
  (lambda (c o)
    (let ((v   (compiler:label c))
	  (s   (compiler:label c)))
      (compiler:emit c ".data")
      (compiler:emit c v ":")
      (compiler:emit c ".asciiz" (object->string o))
      (compiler:emit c s ":")
      (compiler:emit c ".qword $0")
      (compiler:emit c ".text")
      )))

(define compiler:compile:expr:from-word
  (lambda (c o)
    ; int expression is in %rax,
    ; (compiler:emit c "movq $42, %rax")
    (compiler:emit c "addq  %rax, %rax")
    (compiler:emit c "orq   $1, %rax")))

(define compiler:assemble
  (lambda (c . options)
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

	  (posix:system (string-append "gcc "
				       (if verbose "--verbose" "")
				       " -export-dynamic -fno-common -DPIC -c -o " ofile " " sfile))
	  ;; (if verbose (posix:system (string-append "otool -tv " ofile)))
	  (posix:system (string-append "gcc "
				       (if verbose "--verbose" "")
				       " -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o " dfile " " ofile " -compatibility_version 1 -current_version 1.0 -Wl,-single_module"))
	  ;; (if verbose (posix:system (string-append "otool -tv " dfile)))
	  (set! result dfile)
	  (display "compile:assemble => ")(write result)(newline)
	  result
	)))))

(define compiler:load 
  (lambda (c . options)
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
	  (if verbose (begin (display "func-ptr = ")(write func-ptr)(newline)))
	  (set! result ('_ccall func-ptr))
	  (display "result = ")(write result)(newline)
	  result
    )))))

;;;;
