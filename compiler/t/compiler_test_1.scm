;;;;;
; (load "compiler/lisp/compiler")

(for-each 
  (lambda (case)
    (let ((c (compiler:new))
	   (m (car case))
	   (expected (cadr case))
	   (func-ptr nil)
	   (result nil)
	   )
      (if expected
	(begin
	  (display "===========================\n  Testing ")(write m)(newline)
	  ;; (set! &trace 1)
	  (compiler:compile:method c m)
	  ;; (set! &trace 0)
	  ;; (compiler:stream c)
	  (compiler:assemble c 'verbose)
	  (set! func-ptr (compiler:load c))
	  (set! result ('_ccallv func-ptr (vector &msg 1 2 3 4 5 6 7 8 9)))
	  (display "  params   = ")(write (car m))(newline)
	  (display "  body     = ")(write (cdr m))(newline)
	  (display "  result   = ")(write result)(newline)
	  (display "  expected = ")(write expected)(newline)
	  (if (not (equal? result expected)) ('_error "result does not equal? expected"))
	  ;; (read)
	  ))))
    ;; Cases:
 `(
   (
    (() ; args
     1) ; body
    1)  ; expected
   (
    (() ; args 
     'two)
    two)
   (
    (() ; args 
     (if #t 'three))
    three)
   (
    (() ; args 
     (if #t 'four 'not-ok))
    four)
   (
    (() ; args 
     (if #f 'not-ok 'five))
    five)
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
     ('get &root 'stdout))
    ,*standard-output*)
   (
    (() ; args 
     ('_write (&root stdout) "Hello World!\n"))
    13)
   (
    (() ; args 
     (quote (foo bar)))
    (foo bar))
   (
    (() ; args 
     (&root 'stdout)) ;; optional syntax
    ,*standard-output*)

   (
    ((a) ; args 
     a)
    1)
   (
    ((a b) ; args 
      b)
    2)
   (
    ((a b c) ; args 
      c)
     3)
   (
    ((a b c d) ; args 
      d)
     4)
   (
    ((a b c d e) ; args 
      e)
     5)
   (
    ((a b c d e f) ; args 
     f)
     6)
   (
    ((a b c d e f g) ; args 
     g)
     7)
   (
    ((a b c d e f g h) ; args 
     h)
     8)


   (
    ((a) ; args 
      (set! a 'a)
      a)
     a)
   (
    ((a b) ; args 
      (set! b 'b)
      b)
    b)
   (
    ((a b c) ; args 
     (set! c 'c))
     c)
   (
    ((a b c d) ; args 
      (set! d 'd)
      d)
     d)
   (
    ((a b c d e) ; args 
      (set! e 'e)
      e)
     e)
   (
    ((a b c d e f) ; args 
     (set! f 'f))
    f)
   (
    ((a b c d e f g) ; args 
     (set! g 'g))
    g)
   (
    ((a b c d e f g h) ; args 
     (set! h 'h))
    h)
   )

