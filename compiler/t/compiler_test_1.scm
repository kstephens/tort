;;;;;
; (load "compiler/lisp/compiler")

(for-each 
 (lambda (meth)
   (let ((c (compiler:make))
	 (m meth))
     (display "Testing ")(write m)(newline)
     ;; (set! &trace 1)
     (compiler:compile:method c m)
     ;; (set! &trace 0)
     ;; (compiler:stream c)
     (compiler:assemble c 'verbose)
     (compiler:load c)
     (read)
     )
   )
 '(
   (
    () ; args 
    1
    )
   (
    () ; args 
    'symbol
    )
   (
    () ; args 
    (if #t 'ok)
    )
   (
    () ; args 
    (if #t 'ok 'not-ok)
    )
   (
    () ; args 
    (if #f 'not-ok 'ok)
    )
   (
    () ; args 
    (if #f (+ 1 2) (+ 3 4))
    )
   (
    () ; args 
    (begin #f (+ 1 2) (+ 3 4))
    )
   (
    () ; args 
    &root
    )
   (
    () ; args 
    (&root stdout)
    )
   (
    () ; args 
    ;; body
    ('_write (&root stdout) "Hello World!\n")
    )
   (
    () ; args 
    (quote (while #t ('_write *standard-output* "Hello, World!\n")))
    )
   ))

