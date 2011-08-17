(define nil '())
(define send (lambda (sym rcvr . args) (sym rcvr . args)))

(define %root (lambda (sym) ('get &root sym)))
(define %mtable-by-name (lambda (sym) ('get (%root 'mtable) sym)))

(define %get-type (lambda (o) ('_mtable o)))
(define eq? (lambda (a b) ('eq? a b)))

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
(define map 
  (lambda (l f)
    (if (null? l)
	l
      (cons (f (car l))
	    (map (cdr l) f)))))
(define for-each
  (lambda (l f)
    (if (null? l)
	l
      (begin
       (f (car l))
       (for-each (cdr l) f)))))


(define <string> (%mtable-by-name 'string))
(define string? 
  (lambda (o) (eq? (%get-type o) <string>)))
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

(define *standard-input*  (%root 'stdin))
(define *standard-output* (%root 'stdout))
(define *standard-error*  (%root 'stderr))
(define <io> (%mtable-by-name 'io))
(define open-output-file
  (lambda (fname)
    ('open ('create <io>) fname "w+")))
(define close-output-file 
  (lambda (f)
    ('close f)))
(define open-input-file
  (lambda (fname)
    ('open ('create <io>) fname "r")))
(define close-input-file 
  (lambda (f)
    ('close f)))

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

(define = eq?)

(define *load-debug* #f)
(define load 
  (lambda (fname . env)
    (let ((f (open-input-file fname))
	  (out (if *load-debug* *standard-error* nil))
	  (env (if (null? env) &env (car env))))
      ; (display "opened ")(write fname)(display " => ")(write f)(newline)
      (let ((r ('lisp_repl f out out env)))
	(close-input-file f)
	(r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <posix> (%mtable-by-name 'posix))
(define posix ('allocate <posix>))
(define posix:system (lambda (str) ('system posix str)))

(posix:system "hostname")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! *load-debug* #t)
(load "compiler/lisp/compiler.scm")

