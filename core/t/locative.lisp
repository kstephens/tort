(let ((c (cons 0 1))
      (l #f)
      (o #f)
      (sym (make-symbol '())))
  (write c)(newline)
  (set! l ('_slot_locative_at c 0))
  (write l)(newline)
  ('value l)
  ('value= l 'changed-by-locative-value=)
  (write c)(newline)
  (set! o (cons 2 3))
  ;; Give object its own mtable.
  ; ('_mtable= o ('new <mtable> ('_mtable o)))
  (write sym)(newline)
  ('add_method ('_mtable o) sym l)
  (set! &trace 2)
  (write (sym o))(newline)
  (set! &trace 0)
  )
