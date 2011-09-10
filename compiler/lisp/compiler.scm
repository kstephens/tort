(define compiler:make
  (lambda args 
    (list 
     ""   ; output stream
     nil  ; output name
     )))

(define compiler:stream
  (lambda (c)
    (car c)))

(define compiler:set-stream!
  (lambda (c s)
    (set-car! c s)))

(define compiler:emit 
  (lambda (c . args)
    (set! c (compiler:stream c))
    (for-each args (lambda (x)
		(display x c)))
    (newline c)
    ))

(define compiler:method 
  (lambda (c o)
    (let ((mname (compiler:method:name c o)))
      (set-car! (cdr c) mname)
      (compiler:emit c ".text")
      (compiler:emit c ".globl " '_ mname)
      (compiler:emit c '_ mname ':)
      (compiler:emit c "pushq %rbp")               ; save caller's frame pointer.
      (compiler:emit c "movq %rsp, %rbp")          ; set frame pointer to current stack pointer.
      ;(compiler:emit c "subq $xxx, %rsp")         ; Save space for local variables.
      (compiler:method:body c o)
      (compiler:emit c "leave")
      (compiler:emit c "ret") 
      )))

(define compiler:method:name
  (lambda (c o)
    (string-append "_tort_x_" (send '_to_string ('_object_ptr o)))
    ))

(define compiler:method:body 
  (lambda (c o)
    (compiler:emit c "movq $42, %rax")
    (compiler:emit c "addq %rax, %rax")
    (compiler:emit c "orq  $1, %rax")
    ))

(define compiler:assemble
  (lambda (c)
    (let ((name (car (cdr c))))
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

	  (posix:system (string-append "gcc --verbose -export-dynamic -fno-common -DPIC -c -o " ofile " " sfile))
	  (posix:system (string-append "otool -tv " ofile))
	  (posix:system (string-append "gcc --verbose -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o " dfile " " ofile " -compatibility_version 1 -current_version 1.0 -Wl,-single_module"))
	  (posix:system (string-append "otool -tv " dfile))

	  
	  (set! st ('_dlopen (string-append "./" dfile)))
	  (display "st = ")(write st)(newline)
	  (display "name-sym = ")(write name-sym)(newline)
	  ;;(display "name-sym class = ")(write (%get-type name-sym))(newline)
	  ;;('__debugger st) (set! &trace 1)
	  (set! func-ptr ('get st name-sym))
	  (display "func-ptr = ")(write func-ptr)(newline)
	  (set! result ('_ccall func-ptr))
	  (display "result = ")(write result)(newline)
	  result
	)))))

;;;;;

(define m ('get ('_mtable 'symbol) '_inspect))
(define c (compiler:make))
(compiler:method c m)
(compiler:stream c)
(compiler:assemble c)

