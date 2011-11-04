(define-struct reg
  (name  "")
  (width 8))
(set! &trace 1)
(set! &macro-trace 1)
(define-method reg ('_emit self stream args)
  ('emit stream name))
(set! &macro-trace 0)
(set! &trace 0)

(define-struct reg-off
  (reg nil)
  (offset nil))
(define-method reg-off ('_emit self stream args) 
  ('emit stream ('offset self) "(" ('reg self) ")"))

(define-struct isn
  (name #f)
  (nargs 2))
(define-method isn ('new self name args)
  ('name= self name)
  ('nargs= self args))

(define-method isn ('_emit self stream args)
  ('emit stream ('name self) " ")
  (let ((sep " ") (arg-i 0))
    (for-each (lambda (arg)
		('emit stream sep arg)
		(set! arg-i (+ arg-i 1))
		(cond
		  ((= arg-i (isn-nargs self))
		    (set! sep " // "))
		  ((> arg-i (isn-nargs self))
		    (set! sep " "))
		  (else
		    (set! sep ", "))))
      args))
  ('emit stream "\n"))

(define-struct label
  (name #f)
  (position #f)
  (references '())
  )
(define-method label ('_emit self stream args)
  ('emit stream (label:name self))
  ('emit stream ":\n" self))

(define-struct isn-stream
  (body (string-new))
  (labels '()))
(define-method isn-stream ('emit self obj . args)
  (cond
    ((string? obj)
      (display obj (isn-stream:body self)))
    ((or (symbol? obj) (number? obj))
      (display obj (isn-stream:body self)))
    (else
      ('_emit obj self args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s ('new isn-stream))
('emit s "// hello, world\n")
(display "\nCode:\n")(display ('body s))(display "\n")
