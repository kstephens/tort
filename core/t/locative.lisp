(define c (cons 0 1))
c
(define l0 ('_slot_locative_at c 0))
l0
('value l0)
('value= l0 'a)
c
(define o ('new <object>))
o
('_set_mtable o ('new <mtable> ('_mtable o)))
('add_method ('_mtable o) 'foo l0)
('foo o)

