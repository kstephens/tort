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
   ))

