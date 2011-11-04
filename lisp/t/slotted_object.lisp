(let ((o ('new <slotted-object>)))
  ('_add_slot o 'a 1)
  ('_add_slot o 'b 2)
  (display "o.a = ")(write ('a o))(newline)
  (display "o.b = ")(write ('b o))(newline)
  ;;(display "o._names = ")(write ('_names o))(newline) ; FIXME
  ;;(display "o._values = ")(write ('_values o))(newline) ; FIXME
  )