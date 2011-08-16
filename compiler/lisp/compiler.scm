(define compile:make
  (lambda args 
    '()
    ))

(define compile:emit 
  (lambda (c . args)
    (map args (lambda (x)
		(write x)))
    (newline)
    ))

(define compile:method 
  (lambda (c o)
    (compile:emit c '_ (compile:method:name o) ':)
    (compile:emit c "pushq %rbp")               ; save caller's frame pointer.
    (compile:emit c "movq %rsp, %rbp")          ; set frame pointer to current stack pointer.
    ;(compile:emit c "subq $xxx, %rsp")         ; Save space for local variables.
    (compile:method:body c o)
    (compile:emit c "leave")
    (compile:emit c "ret") 
    ))

(define compile:method:name
  (lambda (c o)
    (string-append "_" (send 'name o))
    ))

(define compile:method:body 
  (lambda (c o)
    (compile:emit c "movq $42, %rax")
    (compile:emit c "addq %rax, %rax")
    ))
