(load "lisp/lib/struct.lisp")

(begin
  (define-struct test-struct a (b) (c 'foo))
  (write (test-struct:new))(newline)
  (write (test-struct:new 'a 1))(newline)
  (write (test-struct:new 'a 2 'b 3 'c 'bar))(newline)
  (write ('new test-struct 'a 4 'c 5))(newline)
)