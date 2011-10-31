;;;;;
; (load "compiler/lisp/compiler")

(define m (list '() (list 'write "Hello World" *standard-output*)))
(define c (compiler:make))
;; (set! &trace 1)
(compiler:compile:method c m)
;; (set! &trace 0)
;; (compiler:stream c)
(compiler:assemble c)
(compiler:load c)

