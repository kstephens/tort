(load "lisp/lib/pattern.scm")

(letrec 
  (
    (r? (lambda (x)
	  (and (pair? x) (eq? (car x) '&r))))
    (!r? (lambda (x)
	   (not (r? x))))
    (!r-rsp? (lambda (x)
	   (not (and (r? x) (eq? (cadr x) '%rsp)))))
    (r-!rax? (lambda (x)
	     (and (r? x) (not (eq? (cadr x) '%rax)))))

    (rules
    (map 
      (lambda (r) (rule:make (car r) (caddr r)))
      `(
	 (
	   ( (movq (?? d1 ,r?) (? d1))
	     )
	   =>
	   (
	   )
	   )

	 (
	   ( (pushq (? s1))
	     (movq  (? s2) (&r %rax))
	     (movq  (? s3) (&r %rax))
	     (popq  (?? d2 ,r-!rax?))
	     )
	   =>
	   ( (movq (? s1) (? d2))
	     (movq (? s2) (&r %rax))
	     (movq (? s3) (&r %rax))
	     )
	   )

	 (
	   ( (pushq (? s1))
	     (.nop)
	     (movq  (? s2) (&r %rax))
	     (popq  (?? d2 ,r-!rax?))
	     )
	   =>
	   ( (movq (? s1) (? d2))
	     (movq (? s2) (&r %rax))
	     )
	   )

	 (
	   ( (pushq (? s1))
	     (movq  (? s2) (&r %rax))
	     (popq  (?? d2 ,r-!rax?))
	     )
	   =>
	   ( (movq (? s1) (? d2))
	     (movq (? s2) (&r %rax))
	     )
	   )

	 (
	   ( (pushq (?? s1 ,!r?))
	     (movq  (?? s2 ,!r-rsp?) (?? d2 ,!r-rsp?))
	     )
	   =>
	   ( (movq  (? s2) (? d2))
	     (pushq (? s1))
	     )
	   )

	 (
	   ( (movq (? s1) (?? d1 ,r?))
	     (movq (&o (? o) (? d1)) (?? d1 ,r?))
	     ) 
	   =>
	   (
	     (movq (? s1) (? d1))
	     (.nop)
	     (movq (&o (? o) (? d1)) (? d1))
	   )
	   )

	 (
	   ( (movq (?? s1 ,r?) (?? d1 ,!r?))
	     (movq (? d1)      (? s1))
	     )
	   =>
	   ( (movq (? s1)      (? d1))
	     )
	   )

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
	   ( (movq (?? s1 ,!r?) (?? d1 ,r?))
	     (pushq (? d1))
	     )
	   =>
	   ( (pushq (? s1))
	     )
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
	     (movq (&r %rdx) (? d))
	     )
	   )

	 (
	   ( (movq   (? s1)    (&r %rax))
	     (movq   (? s2)    (?? d1 ,r-!rax?))
	     (callq* (&r %rax))
	     )
	   =>
	   ( 
	     (movq   (? s2)    (? d1))
	     (movq   (? s1)    (&r %rax))
	     (callq* (&r %rax))
	     )
	   )

	 )))
    )

  (define assembler:peephole:debug #f)
  (define (assembler:peephole in)
    (let ((old #f) (new in) (replaced? #t))
      (while replaced?
	(set! replaced? #f)
	(for-each 
	  (lambda (rule)
	    (set! old new)
	    (set! new (vector))
	    (let* ((i 0)
		   (rule-size (list-length (rule:pattern rule))))
	      ;; (display "rule-size = ")(write rule-size)(display " rule = ")(write rule)(newline)
	      (while (< i (vector-length old)) 
		(let* (
			(peephole (vector->list (subvector old i rule-size)))
			(match (pattern:match (rule:pattern rule) peephole (pattern:dictionary:make)))
			(replace #f))
		  (if match
		    (begin
		      (set! replaced? #t)
		      (set! replace (pattern-dictionary:replace match (rule:pattern rule) (rule:replacement rule)))
		      (if assembler:peephole:debug
			(begin
			  (display "rule-size = ")(write rule-size)(display " rule = ")(write rule)(newline)
			  (display "  i = ")(write i)(newline)
			  (display "  peephole = ")(write peephole)(newline)
			  (display "  match = ")(write match)(newline)
			  (display "  replace  = ")(write replace)(newline)
			  ))
		      (vector-append! new (list->vector replace))
		      (set! i (+ i rule-size)))
		    (begin
		      (vector-add! new (vector-ref old i))
		      (set! i (+ i 1))))
		  ))))
	  rules))
      new))

  ) ;; let

  

