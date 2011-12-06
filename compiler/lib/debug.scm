(define-macro (debug msg expr)
  `(begin
     (display " #| DEBUG: ")
     (display ,msg)(display " ")
     (write ',expr)
     (display " = ")
     (write ,expr)
     (display " |#")
     (newline)))

