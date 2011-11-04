; -*- scheme -*-

(let ((the-catch ('new <catch>)))
  ('begin the-catch 
    (lambda (c)
      (display "testing catch/throw\n")
      ('unwind_protect <catch> 
	(lambda () (display "  throwing!!!\n")))
      ('throw c 'thrown)
      'not-thrown))))
