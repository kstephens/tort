(define eq? (lambda (a b) ('eq? a b)))

(define nil '())
(define send (lambda (sel rcvr . args) (sel rcvr . args)))

(define current-environment (lambda () &globals))
(define eval (lambda (o e) ('lisp_eval o e)))

(define %root (lambda (sym) ('get &root sym)))
(define %mtable-by-name (lambda (sym) ('get (%root 'mtable) sym)))

(define %get-type (lambda (o) ('_mtable o)))

(define make (lambda (mt . args) ('new mt . args)))

(define <object> (%mtable-by-name 'object))

(define not (lambda (o) (if o #f #t)))

(define null? (lambda (o) (eq? o '())))
(define <cons> (%mtable-by-name 'cons))
(define pair? (lambda (o) (eq? (%get-type o) <cons>)))
(define cons (lambda (a d) ('new <cons> a d)))
(define car (lambda (o) ('car o)))
(define cdr (lambda (o) ('cdr o)))
(define set-car! (lambda (o v) ('set-car! o v)))
(define set-cdr! (lambda (o v) ('set-cdr! o v)))
(define list (lambda args args))
(define length (lambda (o) ('size o)))
(define reverse 
  (lambda (l)
    (let ((r nil))
      (while (pair? l)
	(set! r (cons (car l) r))
	(set! l (cdr l))
	)
      r)))

(define map 
  (lambda (f l)
    (if (null? l)
	l
      (cons (f (car l))
	    (map f (cdr l))))))
(define for-each
  (lambda (f l)
    (if (null? l)
	l
      (begin
       (f (car l))
       (for-each f (cdr l))))))

(define caar (lambda (o) ('car ('car o))))
(define cadr (lambda (o) ('car ('cdr o))))
(define cdar (lambda (o) ('cdr ('car o))))
(define cddr (lambda (o) ('cdr ('cdr o))))

(define caaar (lambda (o) ('car ('car ('car o)))))
(define caadr (lambda (o) ('car ('car ('cdr o)))))
(define cadar (lambda (o) ('car ('cdr ('car o)))))
(define caddr (lambda (o) ('car ('cdr ('cdr o)))))
(define cdaar (lambda (o) ('cdr ('car ('car o)))))
(define cdadr (lambda (o) ('cdr ('car ('cdr o)))))
(define cddar (lambda (o) ('cdr ('cdr ('car o)))))
(define cdddr (lambda (o) ('cdr ('cdr ('cdr o)))))

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

(define a 1)
(define b 1)
(set! &trace 1)
`(a b)
`(a ,b)
(set! &trace 0)

(define <vector> (%mtable-by-name 'string))
(define vector? (lambda (o) (eq? (%get-type o) <vector>)))
(define vector-length (lambda (s) ('size s)))
(define vector-ref (lambda (s i) ('get s i)))
(define vector-set! (lambda (s i v) ('set s i v)))

(define <string> (%mtable-by-name 'string))
(define string? 
  (lambda (o) (eq? (%get-type o) <string>)))
(define string-new (lambda a ('new <string> (if (pair? a) (car a) 0))))
(define string-length (lambda (s) ('size s)))
(define string-ref (lambda (s i) ('get s i)))
(define string-set! (lambda (s i v) ('set s i v)))
(define string-append 
  (lambda args
    (set-car! args ('clone (car args)))
    (%reduce 'append args)))

(define <symbol> (%mtable-by-name 'symbol))
(define symbol?
  (lambda (o) (eq? (%get-type o) <symbol>)))
(define make-symbol 
  (lambda (s) ('_create <symbol> s)))
(define string->symbol 
  (lambda (s) ('new <symbol> s)))
(define symbol->string
  (lambda (s) 
    (let ((s ('name s)))
      (if (null? s)
	  s
	('clone s)))))

(define *standard-input*  (%root 'stdin))
(define *standard-output* (%root 'stdout))
(define *standard-error*  (%root 'stderr))
(define <io> (%mtable-by-name 'io))
(define open-output-file
  (lambda (fname)
    ('open ('create <io>) fname "w+")))
(define close-output-file (lambda (f) ('close f)))
(define open-input-file
  (lambda (fname)
    ('open ('create <io>) fname "r")))
(define close-input-file (lambda (f) ('close f)))
(define call-with-input-file
  (lambda (file proc)
    (let ((f (open-input-file file))
	  (r nil))
      (set! r (proc f))
      (close-input-file f)
      r)))
(define call-with-output-file
  (lambda (file proc)
    (let ((f (open-output-file file))
	  (r nil))
      (set! r (proc f))
      (close-output-file f)
      r)))

(define newline
  (lambda port
    ('_write (if (pair? port) (car port) *standard-output*) "\n")))

(define write 
  (lambda (obj . port)
    ('lisp_write obj 
		 (if (pair? port) (car port) *standard-output*))))

(define display
  (lambda (obj . port)
    (set! port (if (pair? port) (car port) *standard-output*)) 
    (if (string? obj)
	('_write port obj)
        (write obj port))))

(define read
  (lambda port
    (set! port (if (pair? port) (car port) *standard-input*))
    ('lisp_read port)))

(define %reduce 
  (lambda (f l)
    (let ((a (car l)))
      (set! l (cdr l))
      (while (not (null? l))
	; (write 'a=)(write a)(write "\n")
	; (write 'l=)(write l)(write "\n")
	(set! a (f a (car l)))
	(set! l (cdr l))
	)
      a)))

(define <tagged> (%mtable-by-name 'tagged))
(define tagged? (lambda (o) (eq? (%get-type o) <tagged>)))
(define number? tagged?)

(define +
  (lambda args
    (if (null? args)
	0
      (%reduce (lambda (a b) ('+ a b)) args))))

(define * 
  (lambda args
    (if (null? args)
	1
      (%reduce (lambda (a b) ('* a b)) args))))

(define -
  (lambda (first . args)
    (if (null? args)
	('@- first)
      ('- first (%reduce (lambda (a b) ('+ a b)) args)))))

(define /
  (lambda (first . args)
    (if (null? args)
	('/ 1 first)
      ('/ first (%reduce (lambda (a b) ('* a b)) args)))))

;; BITWISE OPERATORS.
(define |
  (lambda (first . args)
    ('| first (%reduce (lambda (a b) ('| a b)) args))))

(define &
  (lambda (first . args)
    ('& first (%reduce (lambda (a b) ('& a b)) args))))

(define ^
  (lambda (first . args)
    ('^ first (%reduce (lambda (a b) ('^ a b)) args))))

(define %bin-op 
  (lambda (op)
    (lambda (a b) (op a b))))

(define = eq?)
(define < (%bin-op '<))
(define > (%bin-op '>))
(define <= (%bin-op '<=))
(define >= (%bin-op '>=))

(define *load-debug* #f)
(define load 
  (lambda (fname . env)
    (let ((out (if *load-debug* *standard-error* nil))
	  (env (if (null? env) &env (car env))))
      (call-with-input-file fname (lambda (f) ('lisp_repl f out out env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define object->string
  (lambda (o)
    (let ((s (string-new)))
      ('_inspect o s)
      s)))
(define number->string object->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <mtable> (%mtable-by-name 'mtable))
(define mtable? (lambda (o) (eq? (%get-type o) <mtable>)))

(define <map> (%mtable-by-name 'map))
(define map? (lambda (o) (eq? (%get-type o) <map>)))

(define <message> (%mtable-by-name 'message))
(define message? (lambda (o) (eq? (%get-type o) <message>)))

(define <method> (%mtable-by-name 'method))
(define method? (lambda (o) (eq? (%get-type o) <method>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <posix> (%mtable-by-name 'posix))
(define posix ('allocate <posix>))
(define posix:system (lambda (str) ('system posix str)))
;; (posix:system "hostname")

; ('add_method <tagged> '+ (lambda (a b) (+ a b)))
('add_method <string> '+ (lambda (a b) ('append ('clone a) b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (set! *load-debug* #t)
(load "compiler/lisp/compiler.scm")

