(load "lisp/lib/pattern.scm")

(define assembler:peephole
;; Simplify simple unary and binary expressions.

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

  

