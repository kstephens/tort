#include "tort/tort.h"

static ssize_t the_answer = 42;
tort_v _tort_m_class__return_42(tort_tp tort_v rcvr)
{
  return tort_i(the_answer);
}

static tort_v selector_1;
static tort_v rcvr_1;
static tort_v arg_1;
tort_v _tort_m_class__call_other(tort_tp tort_v rcvr, tort_v sel)
{
  tort_send(selector_1, rcvr_1, arg_1);
  return_tort_send(sel, rcvr);
}

tort_v _tort_plain_c(void *arg0, tort_v arg1, tort_v arg2, tort_v arg3, tort_v arg4, tort_v arg5, tort_v arg6)
{
  extern void a7(void *arg0, tort_v arg1, tort_v arg2, tort_v arg3, tort_v arg4, tort_v arg5, tort_v arg6);
  extern void ip(long long arg0, void *arg1);
  extern void pp(void *arg0, void *arg1);
  a7(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  ip(0, arg0);
  ip(1, arg1);
  ip(2, arg2);
  ip(3, arg3);
  ip(4, arg4);
  ip(5, arg5);
  pp((void*) 0, arg0);
  pp((void*) 1, arg1);
  pp((void*) 2, arg2);
  pp((void*) 3, arg3);
  pp((void*) 4, arg4);
  pp((void*) 5, arg5);
  return arg0;
}


/* 
IA64: calling sequence:

At Entry into callee:

(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
 %rdi  %rsi  %rdx  %rcx  %r8   %r9   16(%rbp)

Callee saves %rbp:

	pushq	%rbp
	movq	%rsp, %rbp

???? prbp arg6
0    8    16
^
|
rsp
rbp

Callee saves registers:

	movq	%rbx, -40(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -24(%rbp)
	movq	%r14, -16(%rbp)
	movq	%r15, -8(%rbp)
	subq	$64, %rsp // extra space for %r9

Callee saves arguments on stack:
	movq    %r9,  -56(%rbp)

????  or9   ???? orbx or12 or13 or14 or15 orbp
-64  -56    -48  -40  -32  -24  -16  -8   0
^                                         ^
|                                         |
%rsp                                      %rbp

... DO STUFF...

Callee restores registers:

	movq	-40(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r14
	movq	-8(%rbp), %r15

Callee restores %rbp and %rsp:

	leave

Returns result in %rax:

	ret
 
*/

