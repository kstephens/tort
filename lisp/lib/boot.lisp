(define nil '())
(define %root (lambda (sym) ('get &root sym)))
(define %mtable-by-name (lambda (sym) ('get (%root 'mtable) sym)))
(define *standard-output* (%root 'stdout))

(define %get-type (lambda (o) ('_mtable o)))
(define eq? (lambda (a b) ('eq? a b)))

(define make (lambda (mt . args) ('new mt . args)))

(define null? (lambda (o) (eq? o '())))
(define <cons> (%mtable-by-name 'cons))
(define pair? (lambda (o) (eq? (%get-type o) <cons>)))
(define cons (lambda (a d) ('new <cons> a d)))
(define car (lambda (o) ('car o)))
(define cdr (lambda (o) ('cdr o)))
(define set-car! (lambda (o v) ('set-car! o v)))
(define set-cdr! (lambda (o v) ('set-cdr! o v)))

(define list (lambda args args))

(define string-ref (lambda (s i) ('get s i)))
(define string-set! (lambda (s i v) ('set s i v)))

(define +
  (lambda args
    (if (null? args)
	0
      (%reduce (lambda (a b) ('+ a b)) args))))

(define * 
  (lambda args
    (if (null? args)
	1
      (%reduce (lambda (a b) ('+ a b)) args))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


