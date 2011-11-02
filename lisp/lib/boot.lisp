;; -*- scheme -*-
(define (eq? a b) ('eq? a b))

(define nil '())
(define (send sel rcvr . args) (sel rcvr . args))

(define (current-environment) &globals)
(define (eval o e) ('lisp_eval o e))

(define (%root sym) ('get &root sym))
(define (%mtable-by-name sym) ('get (%root 'mtable) sym))

(define (%get-type o) ('_mtable o))

(define (make mt . args) ('new mt . args))

(define <object> (%mtable-by-name 'object))

(define (not o) (if o #f #t))

(define (null? o) (eq? o '()))
(define <cons> (%mtable-by-name 'cons))
(define (pair? o) (eq? (%get-type o) <cons>))
(define (cons a d) ('new <cons> a d))
(define (car o) ('car o))
(define (cdr o) ('cdr o))
(define (set-car! o v) ('set-car! o v))
(define (set-cdr! o v) ('set-cdr! o v))
(define (list . args) args)
(define (length o) ('size o))
(define (reverse l)
  (let ((r nil))
    (while (pair? l)
	   (set! r (cons (car l) r))
	   (set! l (cdr l))
	   )
    r))

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
	    (map f (cdr l)))))
(define (for-each f l)
  (if (null? l)
      l
      (begin
	(f (car l))
	(for-each f (cdr l)))))

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
			      (list 'concat-list (cadr obj)      (qq-list (cdr l)))
			      (list   'cons        (qq-object obj) (qq-list (cdr l)))))
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

(let ((a 1) (b 2))
  (set! &trace 1)
  `(a b)
  `(a ,b)
  (set! &trace 0)
  )

(define <vector> (%mtable-by-name 'string))
(define (vector? o) (eq? (%get-type o) <vector>))
(define (vector-length s) ('size s))
(define (vector-ref s i) ('get s i))
(define (vector-set! s i v) ('set s i v))

(define <string> (%mtable-by-name 'string))
(define (string? o) (eq? (%get-type o) <string>))
(define (string-new . size) ('new <string> (if (pair? size) (car size) 0)))
(define (string-length s) ('size s))
(define (string-ref s i) ('get s i))
(define (string-set! s i v) ('set s i v))
(define (string-append . args)
  (set-car! args ('clone (car args)))
  (%reduce 'append args))

(define <symbol> (%mtable-by-name 'symbol))
(define (symbol? o) (eq? (%get-type o) <symbol>))
(define (make-symbol s) ('_create <symbol> s))
(define (string->symbol s) ('new <symbol> s))
(define (symbol->string s) 
  (let ((s ('name s)))
    (if (null? s)
	s
	('clone s))))

(define *standard-input*  (%root 'stdin))
(define *standard-output* (%root 'stdout))
(define *standard-error*  (%root 'stderr))
(define <io> (%mtable-by-name 'io))
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

(define <tagged> (%mtable-by-name 'tagged))
(define (tagged? o) (eq? (%get-type o) <tagged>))
(define number? tagged?)

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
(define (| first . args)
    ('| first (%reduce (lambda (a b) ('| a b)) args)))

(define (& first . args)
  ('& first (%reduce (lambda (a b) ('& a b)) args)))

(define (^ first . args)
  ('^ first (%reduce (lambda (a b) ('^ a b)) args)))

(define (%bin-op op)
  (lambda (a b) (op a b)))

(define = eq?)
(define < (%bin-op '<))
(define > (%bin-op '>))
(define <= (%bin-op '<=))
(define >= (%bin-op '>=))

(define *load-debug* #f)
(define (load fname . env)
  (let ((out (if *load-debug* *standard-error* nil))
	(env (if (null? env) &env (car env))))
    (call-with-input-file fname (lambda (f) ('lisp_repl f out out env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (object->string o)
  (let ((s (string-new)))
    ('_inspect o s)
    s))
(define number->string object->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <mtable> (%mtable-by-name 'mtable))
(define (mtable? o) (eq? (%get-type o) <mtable>))

(define <map> (%mtable-by-name 'map))
(define (map? o) (eq? (%get-type o) <map>))

(define <message> (%mtable-by-name 'message))
(define (message? o) (eq? (%get-type o) <message>))

(define <method> (%mtable-by-name 'method))
(define (method? o) (eq? (%get-type o) <method>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <posix> (%mtable-by-name 'posix))
(define posix ('allocate <posix>))
(define (posix:system str) ('system posix str))
;; (posix:system "hostname")

;; ('add_method <tagged> '+ (lambda (a b) (+ a b)))
('add_method <string> '+ (lambda (a b) ('append ('clone a) b)))

(display "boot.lisp complete!")(newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set! *load-debug* #t)
(load "compiler/lisp/compiler.scm")

