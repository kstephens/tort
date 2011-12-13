(let ((me *top-level-macro-environment* ))
  (let* ( (e '(define-macro (foo a b) `('foo ,a ,b 1 2)))
	 (e-prime 
	   (begin (set! &trace 0)
		    ('expand me e))))
    (display "expr   = ")(write e)(newline)
    (display "expr'  = ")(write e-prime)(newline)))


(let ((e '(case xyz ((1) 1) ((foo bar) 2) (else 3))))
  (display "e  = ")(write e)(newline)
  (set! *macro-expand-trace* #t)
  (display "e' = ")(write (macro-expand e))(newline)
  (set! *macro-expand-trace* #f)
)

