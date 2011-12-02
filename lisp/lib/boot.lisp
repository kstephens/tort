;; -*- scheme -*-
(define (eq? a b) ('eq? a b))
(define equal? 'equal?)
(define eqv? 'equal?)

(define nil '())

(define (current-environment) &globals)
(define (eval o e) ('lisp_eval o e))

(define (%root sym) ('get &root sym))
(define (%mtable-by-name sym) ('get (%root 'mtable) sym))
(define (%mtable o) ('_mtable o))

(define (make mt . args) ('new mt . args))

(define <object> (%mtable-by-name 'object))

(define (not o) (if o #f #t))

(define (null? o) (eq? o '()))
(define <cons> (%mtable-by-name 'cons))
(define (pair? o) (eq? ('_mtable o) <cons>)) ;; bootstrap
(define (cons a d) ('new <cons> a d))
(define (car o) ('car o))
(define (cdr o) ('cdr o))
(define (set-car! o v) ('set-car! o v))
(define (set-cdr! o v) ('set-cdr! o v))
(define (list . args) args)

(define (%define-macro name transformer)
  ('define_macro &env name transformer))

(%define-macro 'define-macro 
  (lambda (name . body)
    (if (pair? name)
      (list '%define-macro (list 'quote (car name)) (cons 'lambda (cons (cdr name) body)))
      (cons '%define-macro (cons (list 'quote name) body)))))

(define-macro (send sel rcvr . args) `(,sel ,rcvr ,@args))

(define (length o) ('size o))
(define list-length length)
(define (reverse l)
  (let ((r nil))
    (while (pair? l)
      (set! r (cons (car l) r))
      (set! l (cdr l)))
    r))

(define (memv v l)
  (let ((r #f))
    (while (pair? l)
      (if (eqv? v (car l))
	  (begin
	    (set! r l)
	    (set! l '()))
	  (set! l (cdr l))))
    r))

(define (map f l)
  (let ((result (cons #f '()))
	 (r #f)
	 (c '()))
    (set! r result)
    (while (pair? l)
      (set! c (cons (f (car l)) '()))
      (set-cdr! r c)
      (set! r c)
      (set! l (cdr l)))
    (cdr result)))

(define (map! f l)
  (while (pair? l)
    (set-car! l (f (car l)))
    (set! l (cdr l))))

(define (for-each f l)
  (while (pair? l)
    (f (car l))
    (set! l (cdr l))))

(define (append . lists)
  (let ((result (cons #f '())))
    (if (pair? lists)
      (let ((r result) (l '()) (c '()))
	(while (not (null? (cdr lists)))
	  (set! l (car lists))
	  (set! lists (cdr lists))
	  (while (not (null? l))
	    (set! c (cons (car l) '()))
	    (set-cdr! r c)
	    (set! r c)
	    (set! l (cdr l))
	    ))
	(set-cdr! r (car lists))
	))
    (cdr result)))

(define (caar o) ('car ('car o)))
(define (cadr o) ('car ('cdr o)))
(define (cdar o) ('cdr ('car o)))
(define (cddr o) ('cdr ('cdr o)))

(define (caaar o) ('car ('car ('car o))))
(define (caadr o) ('car ('car ('cdr o))))
(define (cadar o) ('car ('cdr ('car o))))
(define (caddr o) ('car ('cdr ('cdr o))))
(define (cdaar o) ('cdr ('car ('car o))))
(define (cdadr o) ('cdr ('car ('cdr o))))
(define (cddar o) ('cdr ('cdr ('car o))))
(define (cdddr o) ('cdr ('cdr ('cdr o))))

(define &quasiquote
  (let ((qq-list #f) (qq-element #f) (qq-object #f))
    (set! qq-list (lambda (l)
		    (if (pair? l)
		      (let ((obj (car l)))
			(if (and (pair? obj) (eq? (car obj) 'unquote-splicing))
			  (if (cdr l)
			    (list 'append (cadr obj) (qq-list (cdr l)))
			    (cadr obj))
			  (list 'cons (qq-object obj) (qq-list (cdr l)))))
		      (list 'quote l))))
    (set! qq-element (lambda (l)
		       (let ((head (car l)))
			 (if (eq? head 'unquote)
			   (cadr l)
			   (qq-list l)))))
    (set! qq-object (lambda (object)
		      (if (pair? object)
			(qq-element object)
			(list 'quote object))))
    (lambda (expr)
      (qq-object expr))))

(define-macro quasiquote &quasiquote)

(define-macro (let-macro bindings . body)
  `(&let ()
     ,@(map (lambda (binding)
	      `('set_macro &env ',(caar binding) (lambda ,(cdar binding) ,@(cdr binding))))
	 bindings)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (begin . body)
  (if (null? body) ''() ; undef
    (if (null? (cdr body)) (car body)
      `(let () ,@body))))

(define-macro (cond case . cases)
  (if (null? cases)
    `(begin ,@(cdr case))
    `(if ,(car case) 
       (begin ,@(cdr case))
       (cond ,@cases))))

(define-macro (let* bindings . body)
  (cond
    ((null? bindings) `(let () ,@body))
    ((pair? bindings)
      `(let (,(car bindings)) (let* (,@(cdr bindings)) ,@body)))))
 
(define (macro-expand expr . env)
  (set! env (if (pair? env) (car env) &env))
  (let ((result ('lisp_macro_expand expr env)))
    (if (null? result) expr result)))

(define-macro (macro-bind bindings . body)
  (let ((anon-bindings (map (lambda (b) (cons (make-symbol '()) b)) bindings)))
   `(let ,(map (lambda (b) `(,(cadr b) ,(car b))) anon-bindings)
     (let ,(map (lambda (b) `(,(car b) ,(caddr b))) anon-bindings)
       ,@body))))

(define-macro (letrec bindings . body)
  `(let ,(map (lambda (binding) `(,(car binding) #f)) bindings)
     ,@(map (lambda (binding) (set! ,(car binding) ,@(cdr binding))) bindings)
     ,@(body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-method mtable name-and-args . body)
  (let ((name (car name-and-args))
	 (args (cdr name-and-args)))
    `(begin
       ('add_method ,mtable ,name (lambda ,args ,@body))
       ;; (display `(method ,mtable ,name))(newline)
       (list ,mtable ,name))))

(define (%reduce f l)
  (let ((a (car l)))
    (set! l (cdr l))
    (while (not (null? l))
      ;; (write 'a=)(write a)(write "\n")
      ;; (write 'l=)(write l)(write "\n")
      (set! a (f a (car l)))
      (set! l (cdr l))
      )
    a))

(define (string->symbol s) ('new <symbol> s))
(define (symbol->string s) 
  (let ((s ('name s)))
    (if (null? s)
      s
      ('clone s))))
(define (string-append . args)
  (set-car! args ('clone (car args)))
  (%reduce 'append args))
(define <symbol> (%mtable-by-name 'symbol)) ;; bootstrap
(define-macro (define-mtable-class name)
  (let ((name-s (symbol->string name))
	 (mtable (%mtable-by-name name)))
    `(begin
       (define ,(string->symbol (string-append "<" name-s ">")) ',mtable)
       (define (,(string->symbol (string-append name-s "?")) o) (eq? ('_mtable o) ',mtable)))))
(define-mtable-class string)
(define (string-new . size) ('new <string> (if (pair? size) (car size) 0)))
(define (string-length s) ('size s))
(define (string-ref s i) ('get s i))
(define (string-set! s i v) ('set s i v))

(define-mtable-class vector)
(define (vector . vals)
  (let ((v ('new <vector> (list-length vals)))
	 (i 0))
    (while (not (null? vals))
      (vector-set! v i (car vals))
      (set! i (+ i 1))
      (set! vals (cdr vals)))
    v))
(define (vector-length s) ('size s))
(define (vector-ref s i) ('get s i))
(define (vector-set! s i v) ('set s i v))
(define (vector->list v)
  (let* ((l (cons '() '()))
	 (r l)
	 (i 0))
    (while (< i (vector-length v))
      (set-cdr! r (cons (vector-ref v i) '()))
      (set! r (cdr r))
      (set! i (+ i 1)))
    (cdr l)))

(define-mtable-class symbol)
(define (make-symbol s) ('new <symbol> s))

(define-mtable-class boolean)

(define *standard-input*  (%root 'stdin))
(define *standard-output* (%root 'stdout))
(define *standard-error*  (%root 'stderr))
(define-mtable-class io)
(define (open-output-file fname)
  ('open ('create <io>) fname "w+"))
(define (close-output-file f) ('close f))
(define (open-input-file fname)
  ('open ('create <io>) fname "r"))
(define (close-input-file f) ('close f))
(define (call-with-input-file file proc)
  (let ((f (open-input-file file))
	 (r nil))
    (set! r (proc f))
    (close-input-file f)
    r))
(define (call-with-output-file file proc)
  (let ((f (open-output-file file))
	 (r nil))
    (set! r (proc f))
    (close-output-file f)
    r))

(define (newline . port)
  ('_write (if (pair? port) (car port) *standard-output*) "\n"))

(define (write obj . port)
  ('lisp_write obj 
    (if (pair? port) (car port) *standard-output*)))

(define (display obj . port)
  (set! port (if (pair? port) (car port) *standard-output*)) 
  (if (string? obj)
    ('_write port obj)
    (write obj port)))

(define (read . port)
  (set! port (if (pair? port) (car port) *standard-input*))
  ('lisp_read port))

(define-mtable-class tagged)
(define tagged? (lambda (x) ('!= ('_tag x) 0)))
(define-mtable-class fixnum)
(define <integer> <fixnum>)
(define integer? fixnum?)
(define <rational> <fixnum>)
(define rational? fixnum?)
(define <number> <fixnum>)
(define number? fixnum?)

(define (+ . args)
  (if (null? args)
    0
    (%reduce (lambda (a b) ('+ a b)) args)))

(define (* . args)
  (if (null? args)
    1
    (%reduce (lambda (a b) ('* a b)) args)))

(define (- first . args)
  (if (null? args)
    ('@- first)
    ('- first (%reduce (lambda (a b) ('+ a b)) args))))

(define (/ first . args)
  (if (null? args)
    ('/ 1 first)
    ('/ first (%reduce (lambda (a b) ('* a b)) args))))

;; BITWISE OPERATORS.
(define-macro (define-binary-operator op)
  `(define (,op first . args)
     (',op first (%reduce (lambda (a b) (',op a b)) args))))
(define-binary-operator |)
(define-binary-operator &)
(define-binary-operator ^)

(define (%bin-op op)
  (lambda (a b) (op a b)))

(define = eq?)
(define < (%bin-op '<))
(define > (%bin-op '>))
(define <= (%bin-op '<=))
(define >= (%bin-op '>=))

(define *load-debug* #f)
(define-mtable-class lisp_repl)
(define <lisp-repl> <lisp_repl>) ; FIXME
(define (load fname . env)
  (let ((out (if *load-debug* *standard-error* nil))
	 (env (if (pair? env) (car env) &env))
	 (repl nil))
    (set! repl ('new <lisp-repl>))
    ('output= repl out)
    ('prompt= repl out)
    ('env= repl env)
    (call-with-input-file fname 
      (lambda (f)
	('input= repl f)
	('run repl)))
    ;; ('result repl) ; FIXME
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (object->string o)
  (let ((s (string-new)))
    ('_inspect o s)
    s))
(define number->string object->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mtable-class mtable)
(define-mtable-class map)
(define-mtable-class message)
(define-mtable-class method)
(define-mtable-class slotted_object)
(define <slotted-object> <slotted_object>) ; FIXME
(define-mtable-class catch)
(define-mtable-class locative)
(define-mtable-class value)
(define-mtable-class dynlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-mtable-class posix)
(define posix ('allocate <posix>))
(define (posix:system str) ('system posix str))
(define (posix:exit code) ('exit posix code))
;; (posix:system "hostname")

;; Lambdas as methods:
;; ('add_method <tagged> '+ (lambda (a b) (+ a b)))
('add_method <string> '+ (lambda (a b) ('append ('clone a) b)))

(display ";; boot.lisp complete!")(newline)
;; ('_inspect ('get &root '_printf_dispatch) *standard-output*)(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set! *load-debug* #t)
; (set! &trace 1)
(load "lisp/lib/struct.lisp")
(load "compiler/lib/compiler.scm")
