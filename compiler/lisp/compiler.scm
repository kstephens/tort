(define compiler:make
  (lambda args 
    (list 
     (string-new)   ; output stream
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
      (compiler:emit c "  .text")
      (compiler:emit c "  .align 4,0x90") ; ???
      (compiler:emit c "  .globl " mname)
      (compiler:emit c "  " mname ':)
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
      (compiler:emit c "subq  $32, " sp-reg) ; align to 16-byte stack frames.
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
     (cond
     ((number? o)
      (compiler:compile:number c o dst))
     ((pair? o)
      (compiler:compile:pair c o dst))
     ((symbol? o)
      (compiler:compile:symbol c o dst))
     (else
      (compiler:compile:reference c o dst)))
    ))

(define compiler:compile:send
   (lambda (c sel rcvr args)
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
      (compiler:emit c "movq  " rtn-reg ", " arg1-reg " // rcvr")
      (compiler:emit c "movq  " msg     ", " arg2-reg " // message")
      (compiler:emit c "movq  " _msg    ", " arg0-reg " // _tort_message")
      (compiler:emit c "call  " (compiler:global-symbol c '_tort_lookup))
      (compiler:emit c "movq  " rtn-reg ", " msg " // -> message")

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
	(compiler:emit c "movq  " msg->receiver ", " arg1-reg " // => rcvr")
	;; message:
	(compiler:emit c "movq  " msg ", " arg0-reg " // => msg")
	
	;; msg->method->applyf(msg, rcvr, ...) 
	(compiler:emit c "movq  " msg->method ", " meth " // msg->method => meth")
	(compiler:emit c "call  *" meth->applyf " // apply() ")
	
	;; Pop args sp:
	(if (> stack-arg-count 0)
	    (compiler:emit c "addq $" (* 8 stack-arg-count) ", " sp-reg))
	)

      ;; Reclaim message space:
      (compiler:emit c "addq  $" compiler:message:alloc-size ", " sp-reg)
      ;;   Restore msg reg:
      (compiler:emit c "popq  " msg)
    ))

(define compiler:compile:caller-arg 
  (lambda (c arg)
    (let ((arg-i    (car arg))
	  (arg-expr (cadr arg))
	  (arg-type #f))
      ; (compiler:emit c "  // arg #" arg-i)
      (set! arg-type (if (< arg-i (vector-length arg-regs))
				     (vector-ref arg-regs arg-i)
				     'STACK))
      (compiler:compile:expr-dst c arg-expr arg-type)
      arg-type)))

(define compiler:compile:pair
  (lambda (c o dst)
    (cond
     ((eq? (car o) 'quote)
      (compiler:compile:quote c (cadr o) dst))
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
     )))

(define compiler:compile:or
  (lambda (c o dst)
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
      (compiler:emit c "  " Lend ":"))))

(define compiler:compile:and
  (lambda (c o dst)
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
      (compiler:emit c "  " Lend ":"))))
 
(define compiler:compile:if
  (lambda (c o dst)
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
      (compiler:emit c "  " Lend ":"))))
  
(define compiler:compile:while
  (lambda (c o dst)
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
      (compiler:emit c "  " Lend ":"))))

(define compiler:compile:symbol
  (lambda (c o dst)
    (cond
     ((eq? o '&root)
      (compiler:compile:reference c &root dst))
     (else
      (compiler:compile:reference c o dst)))))

(define compiler:compile:quote
  (lambda (c o dst)
    (cond
     ((number? o)
      (compiler:compile:number c o dst))
     (else
      (compiler:compile:reference c o dst)))))

(define compiler:compile:number
  (lambda (c o dst)
    (compiler:compile:literal c (compiler:constant:number c o) dst)))

(define compiler:compile:reference
  (lambda (c o dst)
    (compiler:compile:literal c (compiler:constant:reference c o) dst)))

(define compiler:compile:literal
  (lambda (c o dst)
    (if (eq? dst 'STACK)
	(compiler:emit c "pushq " o)
        (compiler:emit c "movq  " o ", " dst))))

(define compiler:constant:object
  (lambda (c o)
    (cond
     ((number? o)
      (compiler:constant:number c o))
     (else
      (compiler:constant:reference c o)))))

(define compiler:constant:number
  (lambda (c o)
    (string-append "$" (object->string (| (+ o o) 1))))) ; |

(define compiler:constant:reference
  (lambda (c o)
    (string-append "$0x" ('_to_string ('_object_ptr o)))))

)) ; let) let)


(define compiler:compile:literal:string
  (lambda (c o)
    (let ((v   (compiler:label c))
	  (s   (compiler:label c)))
      (compiler:emit c "  .data")
      (compiler:emit c "  " v ":")
      (compiler:emit c "  .asciiz" (object->string o))
      (compiler:emit c "  " s ":")
      (compiler:emit c "  .qword $0")
      (compiler:emit c "  .text")
      )))

(define compiler:box:int
  (lambda (c dst)
    ; int expression is in dst,
    (compiler:emit c "addq  " dst ", " dst)
    (compiler:emit c "orq   $1, " dst)))

(define compiler:unbox:int
  (lambda (c dst)
    ; int expression is in dst,
    (compiler:emit c "sarq  " dst)))

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
