;; -*- scheme -*-

(define <struct> ('new_class <mtable> <vector>))

(let ((slot-name
       (lambda (slot) (if (pair? slot) (car slot) slot)))
      (slot-default
       (lambda (slot) (if (and (pair? slot) (not (null? (cdr slot))))
			  (cadr slot)
			  ''())))
      (slot-offset  
       (lambda (slot) (caddr slot)))
      (struct-name
       (lambda (obj) (vector-ref obj 1)))
      (struct-slots 
       (lambda (obj) (vector-ref obj 2)))
      )

  (define-method <struct> ('lisp_write obj port)
    (display "#<struct " port)
    (write (struct-name obj) port)
    (display " " port)
    (for-each (lambda (slot)
		(write (slot-name slot) port)(display " " port)
		(write (vector-ref obj (slot-offset slot)))(display " " port)
		) (struct-slots obj))
    (display ">" port)
    )
  (define-method <struct> ('initialize-struct obj)
    obj)
  (define-method <struct> ('_inspect obj port)
    ('lisp_write obj port))

(define (%define-struct name slots)
  (let* ((name-s (symbol->string name))
	(name- (string-append name-s ":"))
	(nslots 2)
	(slot-getter-proc #f)
	(slot-setter-sel  #f)
	(slot-setter-proc #f)
	(new_lambda_name #f)
	  ;; Create a class-oriented mtable delegating to <vector>.
	(mtable ('new_class <mtable> <struct>)))
    ;; (display "\nname- = ")(write name-)(newline)
    ;; (display "struct ")(write name)(display " slots = ")(write slots)(newline)
    (set! slots
      (map (lambda (slot)
	     ;; (display "\nslot =")(write slot)(newline)
	     (set! nslots (+ nslots 1))
	     (list (slot-name slot) (slot-default slot) nslots))
	slots))
    ;; (display "struct ")(write name)(display " slots = ")(write slots)(newline)
    (set! slot-getter-proc
      (lambda (slot)
	(string->symbol
	  (string-append name- (symbol->string (slot-name slot))))))
    (set! slot-setter-sel
      (lambda (slot)
	(string->symbol
	  (string-append (symbol->string (slot-name slot)) "="))))
    (set! slot-setter-proc
      (lambda (slot)
	(string->symbol
	  (string-append name- (symbol->string (slot-name slot)) "="))))
    (set! new_lambda_name (string->symbol (string-append name- "new")))

  `(begin 
     ;; (set! &trace 1)
     ,@(map (lambda (slot)
	      (let ( (gp (slot-getter-proc slot))
		     (sp (slot-setter-proc slot))
		     (sg (slot-name slot))
		     (ss (slot-setter-sel slot))
		     (so (slot-offset slot))
		     )
	      ;; (display "\nprocessing ")(write slot)(newline)
	      `(begin
		 (define (,gp obj)     (vector-ref  obj ,so))
	         ('add_method ',mtable ',sg ,gp)
		 (define (,sp obj val) (vector-set! obj ,so val))
	         ('add_method ',mtable ',ss ,sp)
		 )))
	 slots)
     (define ,name ',mtable)
     (define (,(string->symbol (string-append name-s "?")) o) (eq? ('_mtable o) ',mtable))
     (define (,new_lambda_name . inits)
       (let ((instance ('_mtable= (vector '<struct> ',name ',slots ,@(map slot-default slots)) ',mtable)))
	 (while (pair? inits)
	   (let ((n (car inits))
		 (v (cadr inits)))
	     (cond
	       ,@(map (lambda (slot)
			`((eq? n ',(slot-name slot))
			   (vector-set! instance ,(slot-offset slot) v)))
		   slots)
	       (else (error "invalid slot name")))
	     (set! inits (cddr inits))))
	 ('initialize-struct instance)))
     ;; (set! &trace 0)
     ('add_method ',('_mtable mtable) 'new 
       (lambda (mtable . args)
	 (,new_lambda_name . args)))
     ;; (display `(struct ,name))(display " => ")(write ',mtable)(newline)
     ',mtable
     )))

)

(define-macro (define-struct name . slots) (%define-struct name slots))

