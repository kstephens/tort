(define-struct macro-environment
  (bindings ('new <map>))
  (parent #f)
  )
(define-method macro-environment ('get-macro self car-expr)
  (let ((x ('get ('bindings self) car-expr)))
    (if (null? x) 
      (if ('parent self)
	('get-macro ('parent self) car-expr)
	#f)
      x)))
(define-method macro-environment ('set-macro self symbol macro)
  ('set ('bindings self) symbol macro))
(define-method macro-environment ('apply-macro self macro e)
  (set! e (cdr e))
  (display "  apply-macro ")(write macro)(display " to ")(write e)(newline)
  (set! &trace 1)
  (set! e (macro . e))
  (set! &trace 0)
  e)
(define-method macro-environment ('expand-expr self e)
  (display "  expand-expr ")(write e)(newline)
  (cond 
    ((pair? e)
      (case (car e)
	((quote) e)
	((let! lambda) 
	  (,(car e) (cadr e) ,@('expand-args self (cddr e))))
	(else 
	  (let ((macro ('get-macro self (car e))))
	    (display "  macro for ")(display (car e))(display " = ")(write macro)(newline)
	    (if macro
	      ('apply-macro self macro e)
	      ('expand-args self e))))))
    (else e)))
(define-method macro-environment ('expand-args self e)
  (display "  expand-args ")(write e)(newline)
  (cond 
    ((pair? e) (cons 
		 ('expand-expr self (car e)) 
		 ('expand-args self (cdr e))))
    (else      e)))

(define-method macro-environment ('expand self e)
  (let ((e-last #f) (e-next e))
    (while (not (equal? e-next e-last))
      (set! e-last e-next)
      (set! e-next ('expand-expr self e-last)))
    e-next))

(define *top-level-macro-environment* ('new macro-environment))
(set! &trace 0)
('set-macro *top-level-macro-environment* 'define-macro 
  (lambda (name-formals . body)
    `('set-macro *top-level-macro-environment* ,(car name-formals) (lambda ,(cdr name-formals) ,@body))))

(let ((me *top-level-macro-environment* ))
  (let* ( (e '(define-macro (foo a b) `('foo ,a ,b 1 2)))
	 (e-prime 
	   (begin (set! &trace 0)
		    ('expand me e))))
    (display "expr   = ")(write e)(newline)
    (display "expr'  = ")(write e-prime)(newline)))

