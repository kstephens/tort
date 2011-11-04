(let ((a 1) (b 2))
  ;; (set! &trace 1)
  `(a b)
  `(a ,b)
  ;; (set! &trace 0)
  )

