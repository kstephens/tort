(let* ( 
              ;; x68_64
	(arg-regs     '#(%rdi %rsi %rdx %rcx %r8 %r9))
	(object-header-size (send 'get &root 'object_header_size))
	(word-size    (send 'get &root 'word_size))
	(tag-bits     (send 'get &root 'tag_bits))
	(locative-tag (send 'get (send 'get &root 'tagged_mtables) <locative>))
	(fixnum-tag   (send 'get (send 'get &root 'tagged_mtables) <fixnum>))
	(unspec       `(&q ,(if #f #f)))
	(rtn          '(&r %rax))
	(arg0         rtn)
	(arg1         `(&r ,(vector-ref arg-regs 1)))
	(arg2         `(&r ,(vector-ref arg-regs 2)))
	(inline-asm    ('new <map>))
	(macro (lambda (n t)
		 (send 'set inline-asm n t)))
       )
  (for-each 
    (lambda (o) 
      (let ((transformer 
	      (case (cadr o)
		((0) (lambda ()    `((&asm ,@(cddr o)))))
		((1) (lambda (a)   `((&asm ,@(cddr o)) ,a)))
		((2) (lambda (a b) `((&asm ,@(cddr o)) ,a ,b)))
		(else (error "too many arguments in inline-asm %O" o)))))
	(macro (car o) transformer)))
    `(
       (&&~  1 (notq ,rtn))
       (&&@- 1 (negq ,rtn))
       (&&+ 2 (addq ,arg1 ,rtn))
       (&&- 2 (subq ,arg1 ,rtn))
       (&&* 2 (imulq ,arg1 ,rtn))
       (&&/ 2
	 (movq ,rtn (&r %rdx))
	 (sarq (&$ 63) (&r %rdx))
	 (idivq ,arg1))
       (&&& 2 (andq ,arg1 ,rtn))
       (&&| 2 (orq  ,arg1 ,rtn))
       (&&^ 2 (xorq ,arg1 ,rtn))
       (&&% 2
	 (movq ,rtn (&r %rdx))
	 (sarq (&$ 63) (&r %rdx))
	 (idivq ,arg1)
	 (movq (&r %rdx) ,rtn))
       (&&<< 2
	 (movl ,arg1 (&r %ecx))
	 (salq (&r %cl) ,rtn))
       (&&>> 2
	 (movl ,arg1 (&r %ecx))
	 (sarq (&r %cl) ,rtn))
       (&&! 1
	 (cmpq ($& 0) ,rtn)
	 (sete (&r %al))
	 (movzbl (&r %al) ,rtn))
       ;; LOR, LAND ;; FIXME
       (&&==  2
	 (cmpq ,arg1 ,rtn)
	 (sete (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&&!=  2
	 (cmpq ,arg1 ,rtn)
	 (setne (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&&<  2
	 (cmpq ,arg1 ,rtn)
	 (setl (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&&>  2
	 (cmpq ,arg1 ,rtn)
	 (setg (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&&<=  2
	 (cmpq ,arg1 ,rtn)
	 (setle (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&&>=  2
	 (cmpq ,arg1 ,rtn)
	 (setge (&r %al))
	 (movzbl (&r %al) ,rtn))
       (&i 1 ;; box a fixnum
	  (salq (&$ 2) ,rtn)
	  (orq  (&$ 1) ,rtn))
       (&I 1  ;; unbox an int.
	  (sarq (&$ 2) ,rtn))
       (&P 1 ;; unbox a pointer.
          (movq (&o ,arg0 0) ,rtn))
       (&L 1  ;; get locative's value.
	  (movq (&o ,rtn ,(- locative-tag)) ,rtn))
       (&l! 2  ;; set locative's value.
	  (movq ,arg1 (&o ,arg0 ,(- locative-tag))))
   ))

  (macro '&p ;; box a pointer.
    (lambda (x) `((&extern tort_ptr_new) ,x)))
  (macro '&nl ;; create new locative to value
    (lambda (x) `((&extern tort_locative_new_value) ,x)))

  (define (compiler:inline-asm-macros)
    inline-asm)
)

