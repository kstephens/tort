;;;;;
(load "compiler")

(define m ('get ('_mtable 'symbol) '_inspect))
(define c (compiler:make))
(compiler:method c m)
;; (compiler:stream c)
(compiler:assemble c)
(compiler:load c)

