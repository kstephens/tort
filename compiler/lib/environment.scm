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
	;; Align to word boundary.
	(set! alloc-size (* (/ (+ alloc-size (- word-size 1)) word-size) word-size))
	(set! alloc-offset (- alloc-offset alloc-size))
	('alloc-offset= alloc-env alloc-offset)
	(if (> ('alloc-offset-max alloc-env) alloc-offset)
	  ('alloc-offset-max= alloc-env alloc-offset))
	('loc= binding `(&o ,alloc-offset (&r %rbp)))
	;; (debug 'allocate-binding)(debug binding)
	)))
  (define-method environ ('allocate-bindings env)
    ;; (debug "allocate-bindings" env)
    (for-each (lambda (b) ('allocate-binding env b))
      ('binding-list env)))

  (define-struct env-binding
    (name        #f)
    (init        #f)
    (env         #f)
    (referenced?  #f)
    (closed-over? #f)
    (set?        #f)
    (loc         #f)
    (type        #f)
    (size        #f)
    (reg         #f)
    (rest-arg    #f)
    (restore-reg #f)
    (exported     #f)
    )
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
