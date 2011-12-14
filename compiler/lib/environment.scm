(let ( (env-id 0) )
  (define-struct environ
    (id               (let ((id (+ env-id 1))) (set! env-id id) id))
    (name             #f)
    (parent           #f)
    (_alloc-env       #f)
    (alloc-offset     0)
    (alloc-offset-max 0)
    (closure          #f)
    (formals          #f)
    (bindings         ('new <map>))
    (macros           ('new <map>))
    (_global          #f)
    (exports          ('new <map>))
    (export-vector    (vector))
    (imports          ('new <map>)) ; maps env-binding to env vector offset.
    (arg-regs         '#())
    (word-size        #f)
    )

  (define-method environ ('lisp_write self port)
    (display "#<e " port)
    ;; (write ('keys ('bindings self)) port)
    (write ('id self) port)
    (if ('formals self)
      (begin
	(display " " port)
	(write ('formals self) port)))
    (display " >" port)
    )
  (define-method environ ('add self binding)
    ('set ('bindings self) ('name binding) binding)
    ('env= binding self)
    binding)
  (define-method environ ('binding-list self)
    (vector->list ('values ('bindings self))))
  (define-method environ ('lookup self name)
    (let ((b ('get ('bindings self) name)))
      (if (and (null? b) ('parent self))
	('lookup ('parent self) name))
      b))
  (define-method environ ('lookup-or-add-global self name)
    (let ((b ('lookup self name)))
      (if (null? b)
	('add ('global self) ('new env-binding 'name name 'loc ('new_value <locative> '()) ))
	b)))
  (define-method environ ('global self)
    (or ('_global self) self))
  (define-method environ ('alloc-env self)
    ;; (debug "  alloc-env " self)
    (or ('_alloc-env self) self))
  (define-method environ ('subenv self)
    (let ((env ('new environ 'parent self)));; 'loc ???
      ('arg-regs=  env ('arg-regs self))
      ('word-size= env ('word-size self))
      ('_global= env ('global self))
      env))
  (define-method environ ('add-formals env params)
    ('formals= env params)
    (let* ((word-size ('word-size env))
	   (arg-regs  ('arg-regs env))
	   (l params)
	   (arg-i -1)
	   (arg-bp-offset (+ word-size word-size)) ; BP -> #(prev-BP rtn-addr)
	   (arg #f)
	   (rest-arg #f)
	   (reg #f)
	   (loc #f)
	   (binding #f)
	   )
      (while (not (null? l))
	(set! arg-i (+ arg-i 1))
	(set! reg #f)
	(set! loc #f)
	(cond
	  ((symbol? l) ; rest arg
	    (set! arg l)
	    (set! rest-arg arg-i)
	    (set! l '()))
	  (else
	    (set! arg (car l))
	    (set! l (cdr l))
	    (cond
	      ((< arg-i (vector-length arg-regs))
		;; allocate temporary on stack for argument register
		(set! reg `(&r ,(vector-ref arg-regs arg-i))))
	      (else
		;; location is relative to BP.
		(set! loc `(&o ,arg-bp-offset (%r %rbp)))
		(set! arg-bp-offset (+ arg-bp-offset word-size))))))
	(set! binding ('new env-binding 
			'name arg
			'loc loc
			'reg reg
			'rest-arg rest-arg))
	;; (debug "" binding)
	('add env binding)
	)))
  
  (define-method environ ('allocate-binding env binding)
    (if (and ('referenced? binding) (not ('loc binding)))
      (let* ( (word-size ('word-size env))
	      (alloc-env (or ('alloc-env env) env))
	      (alloc-offset ('alloc-offset alloc-env))
	      (alloc-size (or ('size binding) word-size)))
	('size= binding alloc-size)
	;; If it's exported, it's locative in the &export-vector.
	(if ('export-index binding)
	  ('loc= binding `(&o ,('export-index binding) &export-vector))
	  ;; Else, it's locative is on the stack.
	  (let* (
		  (alloc-env (or ('alloc-env env) env))
		  (alloc-offset ('alloc-offset alloc-env)))
	    ;; Align to word boundary.
	    (set! alloc-size (* (/ (+ alloc-size (- word-size 1)) word-size) word-size))
	    (set! alloc-offset (- alloc-offset alloc-size))
	    ('alloc-offset= alloc-env alloc-offset)
	    (if (> ('alloc-offset-max alloc-env) alloc-offset)
	      ('alloc-offset-max= alloc-env alloc-offset))
	    ('loc= binding `(&o ,alloc-offset (&r %rbp)))
	    ;; (debug 'allocate-binding)(debug binding)
	    )))))
  (define-method environ ('allocate-bindings self)
    ;; (debug "allocate-bindings" env)
    (for-each (lambda (b) ('allocate-binding self b))
      ('binding-list self)))

  (define-method environ ('add-export! self b)
    (if (null? ('get ('exports self)))
      (let ((export-index ('size ('export-vector self))))
	(if (= export-index 0)
	  (begin
	    ;; Create a name for the export-vector.
	    ('add self ('new env-binding 'name '&export-vector))
	    ;; Space for export-vector itself.
	    ('add ('export-vector self) nil) 
	    (set! export-index 1)))
	('set ('exports self) b export-index)
	('add ('export-vector self) b)
	)))

  (define-struct env-binding
    (name        #f)
    (init        #f)
    (env         #f)
    (constant?    #f)
    (referenced?  #f)
    (closed-over? #f)
    (set?        #f)
    (loc         #f)
    (type        #f)
    (size        #f)
    (reg         #f)
    (rest-arg    #f)
    (restore-reg #f)
    (export-index #f)
    )
  (define-method env-binding ('referenced-from! b env)
    ;; If this binding's closure is not in our closure,
    ;; mark the binding as closed-over.
    ;; b is exported from its origin closure and
    ;; imported into this.
    (let ((origin ('env b)))
      ('referenced?= b #t)  ;; TODO: add reference count.
      (if (not (eq? ('closure env) ('closure origin)))
	;; Mark b as exported in all closure environments between here
	;; and its origination.	
	(let ((exporter ('parent env)))
	  ('closed-over?= b #t)
	  ('add-export! origin b)
	  (while (not (eq? exporter origin))
	    (let ((parent ('parent exporter)))
	      (if (not (eq? ('closure exporter) ('closure parent)))
		('add-export! exporter b))
	      (set! exporter parent)))))))

  (define-method env-binding ('lisp_write self port)
    (display "#<b " port)
    (write ('id ('env self)) port) (display " " port)
    (write ('name self) port)      (display " " port)
    (if ('init self) 
      (begin
	(display "=" port)
	(write ('init self) port)
	(display " " port)))
    (if ('closed-over? self) (display "* " port))
    (if ('loc self) 
      (begin 
	(display "@" port)
	(write ('loc self) port))
	(display " " port))
    (display ">" port)
    )

) ;; let
