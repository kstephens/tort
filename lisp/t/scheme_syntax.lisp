(set! &macro-trace 1)
(cond (else 1 'else))
(cond (#t 1 'this) (else 2 'else))
(set! &macro-trace 0)
