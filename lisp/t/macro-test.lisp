;(set! &macro-trace 1)
(let-macro (((foo a b) `(list 'foo ,a ,b))
	    ((bar) `(list 'bar 'baz)))
  (display "macros in let-macro")(write ('macros &env))(newline)
  (write (foo 1 2))(newline)
  (write (bar))(newline))
;(set! &macro-trace 0)
(display "macros outside let-macro")(write ('macros &env))(newline)

