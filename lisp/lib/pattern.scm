
(define (pattern:dictionary:make) '())
(define (pattern:dictionary:slots d) d)
(define pattern:dictionary:slot:make cons)
(define pattern:dictionary:slot:key car)
(define pattern:dictionary:slot:value cdr)
(define pattern:dictionary:slot:set-key! set-car!)
(define pattern:dictionary:slot:set-value! set-cdr!)
(define (pattern:dictionary:add! d k v)
  (cons (pattern:dictionary:slot:make k v) 
        (pattern:dictionary:slots d)))
(define (pattern:dictionary:get-slot d k)
  (define (scan slots k)
    (if (null? slots)
        #f
        (let ((slot (car slots)))
          (if (equal? (pattern:dictionary:slot:key slot) k)
              slot
              (scan (cdr slots) k)))))
  (scan (pattern:dictionary:slots d) k))
(define (pattern:dictionary:set! d k v)
  (let ((slot (pattern:dictionary:get-slot d k)))
    (if slot
        (begin
          (pattern:dictionary:slot:set-value! slot v)
          d)
          (pattern:dictionary:add! d k v))))
(define (pattern:dictionary:get d k . not-found)
  (let ((slot (pattern:dictionary:get-slot d k)))
    (if slot
	(pattern:dictionary:slot:value slot)
	(if (pair? not-found) (car not-found)))))

(define pattern:*not-found* (vector 'not-found))
(define (pattern:match pattern datum dictionary)
  (cond
   ((not dictionary)
    #f)
   ((pattern:variable? pattern)
    (pattern:variable:match? pattern datum dictionary))
   ((pattern-predicate? pattern)
    (pattern-predicate:match? pattern datum dictionary))
   ((and (pair? pattern) (pair? datum))
    ;; (display "pattern:match: ")(write pattern)(display " ")(write datum)(newline)
    (pattern:match (cdr pattern) (cdr datum) 
		   (pattern:match (car pattern) (car datum) dictionary)))
   ((and (vector?       pattern) (vector?       datum)
      (= (vector-length pattern) (vector-length datum)))
     (let ((i 0))
       (while (and dictionary (< i (vector-length pattern)))
	 (set! dictionary 
	   (pattern:match 
	     (vector-ref pattern i)
	     (vector-ref datum i)
	     dictionary))
	 (set! i (+ i 1)))
       dictionary))
   (else
    (if (equal? pattern datum)
	dictionary
	#f))))
(define (pattern:replace pattern datum replacement dictionary)
  ;; (display "pattern:replace ")(write pattern)(write datum)(write replacement)(write dictionary)(newline)
  (cond 
   ((procedure? replacement)
    (replacement pattern datum dictionary))
   ((pattern:variable? pattern)
    (pattern:variable:replace pattern datum replacement))
   ((pattern-predicate? pattern)
    (pattern-predicate:replace pattern datum replacement))
   (else
    replacement)))
(define (pattern:variable? pattern)
  (and (pair? pattern) (eq? (car pattern) '?)))
(define pattern:variable cadr)
(define (pattern:variable:match? pattern datum dictionary)
  (let ((dv (pattern:dictionary:get dictionary pattern pattern:*not-found*)))
    (if (eq? pattern:*not-found* dv)
	(pattern:dictionary:set! dictionary pattern datum)
	(if (equal? dv datum)
	    dictionary
	    #f))))
(define (pattern:variable:replace pattern datum replacement)
  replacement)

;; (pattern:match `(?? a ,number?) 12 '()) => (((? a) . 12))
(define (pattern-predicate? pattern)
  (and (pair? pattern) (eq? (car pattern) '??)))
(define pattern-predicate caddr)
(define (pattern-predicate:match? pattern datum dictionary)
  (let ((pred (pattern-predicate pattern)))
    (if (cond
	 ((procedure? pred)
	  (pred datum))
	 (else
	  (eval `(,@pred datum))))
	(pattern:variable:match? `(? ,(pattern:variable pattern)) datum dictionary)
	#f)))
(define (pattern-predicate:replace pattern datum replacement)
  (let ((pred (pattern-predicate pattern)))
    (cond
     ((procedure? pred)
      (pred datum))
     (else
      (eval `(,@pred datum))))))

(define pattern:*debug* #f)
(define (pattern-dictionary:replace d pattern datum)
  (let ((slot (pattern:dictionary:get-slot d datum)))
    (if pattern:*debug*
	(begin (display "  pattern-dictionary:replace ")
	       (write datum)(display " ")
	       (write slot)(display " ")
	       (write d)(display " ")(newline)))
    (cond
     (slot
      (pattern:replace (pattern:dictionary:slot:key slot) datum (pattern:dictionary:slot:value slot) d))
     ((procedure? datum)
      (datum pattern datum d))
     ((pair? datum)
      (cons (pattern-dictionary:replace d pattern (car datum))
	    (pattern-dictionary:replace d pattern (cdr datum))))
     ((vector? datum)
       (let ((v make-vector (vector-length datum))
	     (i 0))
	 (while (< i 0)
	   (vector-set! v i (pattern-dictionary:replace d pattern (vector-ref datum i)))
	   (set! i (+ i 1)))
	 v))
     (else
      datum))))

(define (pattern:compile:top-level pattern)
  (let ((datum 'datum)
	(dictionary 'dictionary))
    `(lambda (pattern ,datum ,dictionary)
       ,(pattern:compile pattern datum dictionary))))
(define (pattern:compile pattern datum dictionary)
   (cond 
    ((pattern:variable? pattern)
     (pattern:variable:compile pattern datum dictionary))
    ((pair? pattern)
     `(and (pair ,datum) 
	   (let ((a (car ,datum))
		 (d (cdr ,datum)))
	     ,(pattern:compile (car pattern) 'a
			       (pattern:compile (cdr pattern) 'd
						dictionary)))))
    (else
     `(and (equal? (quote ,pattern) ,datum)
           ,dictionary))))
(define (pattern:variable:compile pattern datum dictionary)
   `(pattern:variable:match? ,pattern ,datum ,dictionary))
 
 (define (rule:make pattern replacement)
   (vector 'rule pattern '=> replacement))
 (define (rule:pattern rule)
   (vector-ref rule 1))
 (define (rule:replacement rule)
   (vector-ref rule 3))  
 (define (rule:apply rule datum)
   (let ((dict (pattern:dictionary:make))
	 (result #f))
     ;; (display "\nrule:apply ")(write rule)(display " ")(write datum)(newline)
     (set! dict (pattern:match (rule:pattern rule) datum dict))
     (if dict
         (begin
           (if pattern:*debug*
               (begin
                 (display "\n  rule:apply: matched\n")
                 (write datum)(display "\n    ")
                 (write rule) (display "\n    ")
                 (write dict) (display "\n    ")
                 (newline)
                 )
	     )
           (set! result (pattern-dictionary:replace dict (rule:pattern rule) (rule:replacement rule)))
           (if pattern:*debug*
	     (begin
	       (display "\n  rule:apply: result ")
	       (write result)(display " ")
	       (newline)
	       ))
           result
           )
       datum)))
(define (rule:applyn rules datum)
  (if (null? rules)
    datum
    (rule:applyn (cdr rules) 
      (rule:apply (car rules) datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (while-change-func f)
  (letrec ((wcf (lambda (x)
		  (let ((x-prime (f x)))
		    ;; (display "x  => ")(write x)(newline)
		    ;; (display "x' => ")(write x-prime)(newline)
		    (if (equal? x-prime x)
			x
			(wcf x-prime))))))
    wcf))

 (define (recursive-func f)
   (letrec ((rf (lambda (x)
                  (set! x (f x))
                  (cond
                    ((pair? x)
                     (cons (rf (car x))
                           (rf (cdr x))))
                    (else
                     x)))))
     rf))
 
