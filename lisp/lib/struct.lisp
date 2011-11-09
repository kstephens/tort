;; -*- scheme -*-

(define (%define-struct name slots)
  (let* ((name-s (symbol->string name))
	(name- (string-append name-s ":"))
	(nslots 2)
	(slot-name #f) 
	(slot-default #f)
	(slot-offset #f)
	(slot-getter-proc #f)
	(slot-setter-sel  #f)
	(slot-setter-proc #f)
	(new_lambda_name #f)
	(mtable ('new <mtable> <vector>)))
    ;; (display "\nname- = ")(write name-)(newline)
    (set! slot-name    
      (lambda (slot) (if (pair? slot) (car slot) slot)))
    (set! slot-default 
      (lambda (slot) (if (and (pair? slot) (not (null? (cdr slot))))
			 (cadr slot)
			 ''())))
    ;; (display "struct ")(write name)(display " slots = ")(write slots)(newline)
    (set! slots
      (map (lambda (slot)
	     ;; (display "\nslot =")(write slot)(newline)
	     (set! nslots (+ nslots 1))
	     (list (slot-name slot) (slot-default slot) nslots))
	slots))
    ;; (display "struct ")(write name)(display " slots = ")(write slots)(newline)
    (set! slot-offset
      (lambda (slot) (caddr slot)))
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
     (define (,new_lambda_name . args)
       ;; (set! &trace 1)
       (set! args ('_set_mtable (vector '<struct> ',name ',slots ,@(map slot-default slots)) ',mtable))
       ;; (set! &trace 0)
       args)
     ;; (set! &trace 0)
     ('add_method ',('_mtable mtable) 'new 
       (lambda (mtable . args)
	 (,new_lambda_name)))
     ;; (display `(struct ,name))(display " => ")(write ',mtable)(newline)
     ',mtable
     )))

(define-macro (define-struct name . slots) (%define-struct name slots))

(if #f
  (begin
    (write (%define-struct 'test-struct '(a (b) (c 'foo))))(newline)
    (define-struct test-struct compiler a (b) (c 'foo))
    (write (test-struct:new))(newline)
    )
