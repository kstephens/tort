;; -*- scheme -*-
(set! &macro-trace 1)
(define (%define-struct name slots)
  (let ((name-s (symbol->string name))
	(name- (string-append (symbol->string name) ":"))
	(nslots 2)
	(slot-name #f) 
	(slot-default #f)
	(slot-offset #f)
	(slot-getter #f)
	(slot-setter #f)
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
    (set! slots (map (lambda (slot)
		       ;; (display "\nslot =")(write slot)(newline)
		       (set! nslots (+ nslots 1))
		       (list (slot-name slot) (slot-default slot) nslots))
		     slots))
    ;; (display "struct ")(write name)(display " slots = ")(write slots)(newline)
    (set! slot-offset  (lambda (slot) (caddr slot)))
    (set! slot-getter  (lambda (slot) 
			 (string->symbol 
			  (string-append name- (symbol->string (slot-name slot))))))
    (set! slot-setter  (lambda (slot) 
			 (string->symbol 
			  (string-append name- "set-" (symbol->string (slot-name slot)) "!"))))
    (set! new_lambda_name (string->symbol (string-append name- "new")))

  `(begin 
     ;; (set! &trace 1)
     ,@(map (lambda (slot)
	      ;; (display "\nprocessing ")(write slot)(newline)
	      `(begin
		 (define (,(slot-getter slot) obj)     (vector-ref  obj ,(slot-offset slot)))
		 (define (,(slot-setter slot) obj val) (vector-set! obj ,(slot-offset slot) val))))
	    slots)
     (define ,(string->symbol (string-append "<" name-s ">")) ',mtable)
     (define (,(string->symbol (string-append name-s "?")) o) (eq? ('%mtable o) ',mtable))
     (define (,new_lambda_name . args)
       ('_set_mtable (vector '<struct> ',name ',slots ,@(map slot-default slots)) ',mtable))
     ;; (set! &trace 0)
     ('add_method ',mtable 'new 'new_lambda_name) 
     ',mtable
     )))

(define-macro (define-struct name . slots) (%define-struct name slots))

(if #f
  (begin
    (write (%define-struct 'test-struct '(a (b) (c 'foo))))(newline)
    (define-struct test-struct compiler a (b) (c 'foo))
    (write (test-struct:new))(newline)
    )
