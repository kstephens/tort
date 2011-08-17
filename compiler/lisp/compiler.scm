(define compiler:make
  (lambda args 
    (list "")
    ))

(define compiler:stream
  (lambda (c)
    (car c)))

(define compiler:set-stream!
  (lambda (c s)
    (set-car! c s)))

(define compiler:emit 
  (lambda (c . args)
    (set! c (compiler:stream c))
    (map args (lambda (x)
		(display x c)))
    (newline c)
    ))

(define compiler:method 
  (lambda (c o)
    (compiler:emit c '_ (compiler:method:name c o) ':)
    (compiler:emit c "pushq %rbp")               ; save caller's frame pointer.
    (compiler:emit c "movq %rsp, %rbp")          ; set frame pointer to current stack pointer.
    ;(compiler:emit c "subq $xxx, %rsp")         ; Save space for local variables.
    (compiler:method:body c o)
    (compiler:emit c "leave")
    (compiler:emit c "ret") 
    ))

(define compiler:method:name
  (lambda (c o)
    (string-append "_tort_x_" (send '_to_string ('_object_ptr o)))
    ))

(define compiler:method:body 
  (lambda (c o)
    (compiler:emit c "movq $42, %rax")
    (compiler:emit c "addq %rax, %rax")
    ))

;;;;;

(define m ('get ('_mtable 'symbol) '_inspect))
(define c (compiler:make))
(compiler:method c m)
(compiler:stream c)
