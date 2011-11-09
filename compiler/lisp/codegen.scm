(define-struct reg
  (name  'UNKNOWN)
  (width 8))
(define-method reg ('_emit self stream)
  ('emit stream ('name self)))

(define-struct reg-off
  (reg    'UNKNOWN-REG)
  (offset 0))
(define-method reg-off ('_emit self stream) 
  ('emit stream ('offset self) "(" ('reg self) ")"))

(define-struct opcode
  (name    'UNKNOWN)
  (n-param 2))
(define-method opcode ('new self name n-param)
  ('name= self name)
  ('n-param= self n-param))

(define-method opcode ('_emit self stream)
  ('emit stream ('name self) " "))

(define-struct isn
  (opcode 'UNKNOWN)
  (args   '()))
(define-method isn ('new self opcode args)
  ('opcode= self opcode)
  ('args= self args))
(define-method isn ('_emit self stream)
  (let ((sep " ") (arg-i 0) (opcode ('opcode self)))
    ('emit stream opcode)
    (for-each
      (lambda (arg)
	('emit stream sep arg)
	(set! arg-i (+ arg-i 1))
	(set! sep 
	  (cond
	    ((< arg-i ('n-param opcode))
	      ", ")
	    ((= arg-i ('n-param opcode))
	      " // ")
	    (else
	      " "))))
      ('args self)))
  ('emit stream "\n"))

(define-struct label
  (name #f)
  (position #f)
  (references '())
  )
(define-method label ('_emit self stream)
  ('emit stream ('name self) ":\n")
  ('labels= stream (cons self ('labels stream))))

(define-struct isn-stream
  (body    (string-new))
  (labels '()))
(define-method isn-stream ('emit self . objs)
  (for-each 
    (lambda (obj)
      (cond
	((or (string? obj) (symbol? obj) (number? obj))
	  (display obj ('body self)))
	(else
	  ('_emit obj self))))
    objs)
  self)

(for-each 
  (lambda (x)
    (define-method isn-stream ((car x) self src dst)
      ('emit self (cadr x) " " src ", " dst "\n")))
  '((MOV movq) (SUB subq) (ADD addq) (OR orq) (AND andq)))

(for-each 
  (lambda (x)
    (define-method isn-stream ((car x) self src)
      ('emit self (cadr x) " " src "\n")))
  '((PUSH pushq) (POP popq) (CALL call))

(define-method isn-stream ('cfunc self env func args)
  #f
)

(define-method isn-stream ('ccall self env func args)
  #f
)

(define-method isn-stream ('send self env func args)
  #f
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s ('new isn-stream))
(display "\ns=\n")(write s)(newline)
('emit s "// hello, world\n")
(display "\nCode:\n")(display ('body s))(display "\n")
