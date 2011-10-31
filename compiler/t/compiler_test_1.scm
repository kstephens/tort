;;;;;
; (load "compiler/lisp/compiler")

(define m 
  '(
    ;; args
    () 
    ;; body
    ('_inspect "Hello World!" *standard-output*) ; FAILS
    ('_write *standard-output* "\n") ; FAILS
    ))

(define m 
  '(
    ;; args
    () 
    ;; body
    (if #f (+ 1 2) (+ 3 4))
    ))

(define m 
  '(
    ;; args
    () 
    ;; body
    (while #t ('_write *standard-output* "Hello, World!\n"))
    ))

(define c (compiler:make))
;; (set! &trace 1)
(compiler:compile:method c m)
;; (set! &trace 0)
;; (compiler:stream c)
(compiler:assemble c)
(compiler:load c)

