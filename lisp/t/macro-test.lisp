;(set! &macro-trace 1)
(let-macro (((foo a b) `(list 'foo ,a ,b))
	    ((bar) `(list 'bar 'baz)))
  (display "macros in let-macro")(write ('macros &env))(newline)
  (write (foo 1 2))(newline)
  (write (bar))(newline))
;(set! &macro-trace 0)
(display "macros outside let-macro")(write ('macros &env))(newline)

(let ((e '(case 1)))
  (display "e =  ")(write e)(newline)
  (display "  => ")(write (macro-expand e))(newline))

(let ((e '(case 1 (else 'c))))
  (display "e =  ")(write e)(newline)
  (display "  => ")(write (macro-expand e))(newline))

(let ((e '(case 1 ((1 2 3) 'a))))
  (display "e =  ")(write e)(newline)
  (display "  => ")(write (macro-expand e))(newline))

(let ((e '(case 1 ((1 2 3) 'a) ((4 5) 'b) (else 'c))))
  (display "e =  ")(write e)(newline)
  (display "  => ")(write (macro-expand e))(newline))

(let ((e '(macro-bind ((a (+ 1 2)) (b 3)) `(+ ,a ,a ,b))))
  (display "e =  ")(write e)(newline)
  (display "  => ")(write (macro-expand e))(newline))

