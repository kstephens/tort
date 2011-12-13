(load "pattern.scm")

;; Simplify simple unary and binary expressions.
(let ()
  (define (r? x)
    (and (pair? x) (eq? (car x) '&r)))
  (define (!r? x)
    (not (r? x)))
  (define *peephole-rules*
    (map 
      (lambda (r) (rule:make (car r) (caddr r)))
      `(
	 (
	   ( (movq (? s1) (?? d1 ,r?))
	     (movq (? s2) (?? d1 ,r?))
	     ) 
	   =>
	   ( (movq (? s2) (? d1)))
	   )

	 (
	   ( (movq (?? s1 ,r?) (?? d1 ,r?))
	     (movq (? d1)      (?? d2 ,r?))
	     ) 
	   =>
	   ( (movq (? s1) (? d2)))
	   )
	 
	 (
	   ( (movq (?? s1 ,!r?) (?? d1 ,r?))
	     (movq (? d1)       (?? d2 ,r?))
	     ) 
	   =>
	   ( (movq (? s1) (? d2)))
	   )
	 
	 (
	   ( (movq (?? s1 ,r?) (?? d1 ,r?))
	     (movq (? d1)      (?? d2 ,!r?))
	     )
	   =>
	   ( (movq (? s1) (? d2)))
	   )

	 (
	   ( (pushq (? s))
	     (popq  (? s))
	     )
	   =>
	   ( )
	   )

	 (
	   ( (pushq (?? s ,r?))
	     (popq  (?? d ,r?))
	     )
	   =>
	   ( (movq (? s) (? d)))
	   )

	 (
	   ( (pushq (?? s ,r?))
	     (popq  (?? d ,!r?))
	     )
	   =>
	   ( (movq (? s) (? d)))
	   )

	 (
	   ( (pushq (?? s ,!r?))
	     (popq  (?? d ,r?))
	     )
	   =>
	   ( (movq (? s) (? d)))
	   )

	 (
	   ( (pushq (?? s ,!r?))
	     (popq  (?? d ,!r?))
	     )
	   =>
	   ( (movq (? s)     (&r %rdx))
	     (movq (&r %rdx) (? d)))
	   )

	 )))

  (display "rules = ")(write *peephole-rules*)(newline)
  (for-each
    (lambda (in)
      (let ((out #f))
	(set! out (rule:applyn *peephole-rules* in))
	(display "in  = ")(write in)(newline)
	(display "out = ")(write out)(newline)
	))
    '(
       ( (movq  (&o  -8 (&r %rbp)) (&r %rax))
	 (movq  (&o -16 (&r %rbp)) (&r %rax)))
       ( (movq  (&o  -8 (&r %rbp)) (&r %rax))
	 (movq  (&r %rax)          (&r %rsi)))
       ( (pushq (&r %rsi))
	 (popq  (&r %rsi)))
       ( (pushq (&o -8 (&r %rbp)))
	 (popq  (&r %rsi)))
       ( (pushq (&o  -8 (&r %rbp)))
	 (popq  (&o -16 (&r %rbp))))

       ))
  )

  

