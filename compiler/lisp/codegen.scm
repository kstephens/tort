(define-struct reg
  (name  "")
  (width 8))
(define-method <reg> (emit self stream)
  ('emit stream name))

(define-struct reg-off
  (reg nil)
  (offset nil))
(define-method <reg-off> (emit self stream) 
  ('emit stream ('offset self) "(" ('reg self) ")"))

(define-struct isn
  (name "")
  (nargs 2))
(define-method <isn> (emit self stream args)
  ('emit stream ('name self) " ")
  (let ((sep " "))
    (for-each (lambda (arg)
		('emit stream sep arg)
		(set! sep ", "))
      args))
  ('emit stream "\n"))

