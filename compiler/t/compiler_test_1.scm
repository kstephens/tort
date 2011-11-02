;;;;;
; (load "compiler/lisp/compiler")

(for-each 
 (lambda (case)
   (let ((c (compiler:make))
	 (m (car case))
	 (expected (cadr case))
	 )
     (display "===========================\n  Testing ")(write m)(newline)
     ;; (set! &trace 1)
     (compiler:compile:method c m)
     ;; (set! &trace 0)
     ;; (compiler:stream c)
     (compiler:assemble c 'verbose)
     (compiler:load c)
     ; (read)
     ))
 '(
   (
    (() ; args 
     1)
    1)
   (
    (() ; args 
     'two)
    'two)
   (
    (() ; args 
     (if #t 'three))
    'three)
   (
    (() ; args 
     (if #t 'four 'not-ok))
    'four)
   (
    (() ; args 
     (if #f 'not-ok 'five))
    'five)
   (
    (() ; args 
     ('- -1 5))
    -6)
   (
    (() ; args 
     (if #f (+ 1 2) (+ 3 4)))
    7)
   (
    (() ; args 
     (begin #f (+ 1 2) (+ 3 5)))
    8)
   (
    (() ; args 
     (or #f 9 10))
    9)
   (
    (() ; args 
     (and 9 10))
    10)
   (
    (() ; args 
     &root)
    &root)
   (
    (() ; args 
     (&root stdout))
    *standard-output*)
   (
    (() ; args 
     ('_write (&root stdout) "Hello World!\n"))
    #f)
   (
    (() ; args 
     (quote (while #t ('_write *standard-output* "Hello, World!\n"))))
    #f
    )
   (
    ((a) ; args 
     (if #f ('list a))
     (set! a 1))
    #f
    )
   (
    ((a b) ; args 
     (if #f ('list a b))
     (set! b 2))
    #f
    )
   (
    ((a b c) ; args 
     (if #f ('list a b c))
     (set! c 3))
    #f
    )
   (
    ((a b c d) ; args 
     (if #f ('list a b c d))
     (set! d 4))
    #f
    )
   (
    ((a b c d e) ; args 
     (if #f ('list a b c d e))
     (set! e 5))
    #f
    )
   (
    ((a b c d e f) ; args 
     (if #f ('list a b c d e f))
     (set! f 6))
    #f
    )
   (
    ((a b c d e f g) ; args 
     (if #f ('list a b c d e f g))
     (set! g 7))
    #f
    )
   ))

