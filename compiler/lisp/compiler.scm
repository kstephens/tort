(define compiler:make
  (lambda args 
    (list 
     ""   ; output stream
     nil  ; output name
     0    ; next label id
     )))

(define compiler:output-name 
  (lambda (c)
    (car (cdr c))))

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

(define compiler:label
  (lambda (c)
    (let* ((id (compiler:label-id c))
	   (label (string-concat "L" (number->string id))))
      (compiler:set-label-id! c (+ id 1))
      label)))

(define compiler:emit 
  (lambda (c . args)
    (set! c (compiler:stream c))
    (for-each (lambda (x)
		(display x c)) 
	      args)
    (newline c)
    ))

(define compiler:method 
  (lambda (c o)
    (let ((mname (compiler:method:name c o)))
      (set-car! (cdr c) mname) ; HACK compiler:output-name
      (compiler:emit c ".text")
      (compiler:emit c ".globl " '_ mname)
      (compiler:emit c '_ mname ':)
      (compiler:emit c "pushq %rbp")               ; save caller's frame pointer.
      (compiler:emit c "movq %rsp, %rbp")          ; set frame pointer to current stack pointer.
      ;(compiler:emit c "subq $xxx, %rsp")         ; Save space for local variables.
      (compiler:method:body c o)
      (compiler:emit c "leave")
      (compiler:emit c "ret")
      c
      )))

(define compiler:method:name
  (lambda (c o)
    (string-append "_tort_x_" (send '_to_string ('_object_ptr o)))
    ))

(define compiler:method:body 
  (lambda (c o)
    (compiler:expr c o)
    ;; (compiler:expr c 42)
    ))

(define compiler:expr
  (lambda (c o)
    (if (tagged? o)
	(compiler:literal:tagged c o)
        (compiler:literal:constant c o))
    ))

(define compiler:literal:tagged
  (lambda (c o)
    (compiler:emit c "movq $" (| (+ o o) 1) ", %rax" 
                     " // literal " (object->string o))
    ))

(define compiler:literal:constant
  (lambda (c o)
    (let ((ptr (send '_object_ptr o)))
      (compiler:emit c "movq $0x" (send '_to_string ptr) ", %rax"
                     " // constant object ref " ptr))
    ))

(define compiler:compile-call
  (lambda (c sel rcvr args)
    ;; %rbx contains current message.
    ;;
    ;; Create message object on stack:
    ;;   pushq %r15
    ;;   subq  $(sizeof(tort_message)), %rsp
    ;;   movq  %rsp, %r15 # %r15 now has address of message object
    ;;   
    ;; Initialize message object on stack:
    ;;
    ;;   movq  %rbx, tort_message.previous_message(%r15)
    ;;   (comp sel) => %eax
    ;;   movq  %eax, tort_message.selector(%r15)
    ;;   (comp rcvr) => %eax
    ;;   movq  %eax, tort_message.receiver(%r15)
    ;;   movq  $(size args), tort_message.argc(%r15)
    ;;
    ;; %r15 = _tort_lookup(_tort_message, rcvr, message)
    ;;
    ;;   movq  %r15, %rdx ;
    ;;   movq  %rax, %rsi ;
    ;;   movq  %rbx, %rdi ;
    ;;   call  _tort_lookup ; (_tort_message:%rdi, rcvr:%rsi, message:%rdx)
    ;;   movq  %rax, %r15
    ;;
    ;; Invoke method->func(message, rcvr, args ...):
    ;;
    ;; Emit arguments into (%rdi, %rsi, %rdx, %rcx, %r8,  %r9,  push sp, etc)
    ;;                      msg,  rcvr, arg0, arg1, arg2, arg3, arg4, etc)
    ;; 
    ;;   (comp arg4) => %rax
    ;;   push %rax
    ;;   (comp arg3) => %rax
    ;;   movq %rax, %r9 ; arg3
    ;;   (comp arg2) => %rax
    ;;   movq %rax, %r8 ; arg2
    ;;   (comp arg1) => %rax
    ;;   movq %rax, %rcx ; arg1
    ;;   (comp arg0) => %rax
    ;;   movq %rax, %rdx ; arg0
    ;;   movq tort_message.receiver(%r15), %rsi ; rcvr
    ;;   movq %r15, %rdi ; msg
    ;;
    ;;   movq tort_message.method(%rbx), %rax
    ;;   movq tort_method.func(%rax), %rax
    ;;   call *(%rax)
    ;; 
    ;; Pop args sp:
    ;; Pop message space:
    ;;
    ;; Restore %r15:
    ;;    popq %r15:
    ))

(define compiler:literal:string
  (lambda (c o)
    (let ((v   (compiler:label c))
	  (s   (compiler:label c)))
      (compiler:emit c ".data")
      (compiler:emit c v ":")
      (compiler:emit c ".asciiz" ('escape o))
      (compiler:emit c s ":")
      (compiler:emit c ".qword $0")
      (compiler:emit c ".text")
      )))

(define compiler:expr:from-word
  (lambda (c o)
    ; int expression is in %rax,
    ; (compiler:emit c "movq $42, %rax")
    (compiler:emit c "addq %rax, %rax")
    (compiler:emit c "orq  $1, %rax")))

(define compiler:assemble
  (lambda (c . options)
    (let ((name (compiler:output-name c))
	  (verbose (not (null? options))))
      (let ((sfile (string-append name ".s"))
	    (ofile (string-append name ".o"))
	    (dfile (string-append name ".dylib"))
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
	  (if verbose (posix:system (string-append "otool -tv " ofile)))
	  (posix:system (string-append "gcc "
				       (if verbose "--verbose" "")
				       " -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o " dfile " " ofile " -compatibility_version 1 -current_version 1.0 -Wl,-single_module"))
	  (if verbose (posix:system (string-append "otool -tv " dfile)))
	  (set! result dfile)
	  (display "compile:assemble => ")(write result)(newline)
	  result
	)))))

(define compiler:load 
  (lambda (c . options)
    (let ((name (compiler:output-name c))
	  (verbose (not (null? options))))
      (let ((sfile (string-append name ".s"))
	    (ofile (string-append name ".o"))
	    (dfile (string-append name ".dylib"))
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
