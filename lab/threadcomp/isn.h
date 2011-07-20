ISN(LIT, 1,
    push(arg0)
    )

ISN(POP, 0,
    pop()
    )

ISN(ADD, 0, 
    ({
      sp[1] = sp[0] + (size_t) sp[1];
      pop();
    }))

ISN(PRINT, 0, 
    printf("%ld\n", (long) (size_t) pop());
    )

ISN(CALL_, 1,
    run((word_t**) &pc[-1], &sp)
    )

ISN(CALL_TAIL_, 1,
    pc_p = (word_t**) &pc[-1];
    pc = *pc_p
    )

ISN(CALL, 0,
    {
      pc = pop();
      run(&pc, &sp);
    })

ISN(CALL_TAIL, 0,
    {
      pc_p = 0;
      pc = pop();
    })

ISN(RTN, 0, 
    {
      *sp_p = sp;
      return;
    }
    )

#undef ISN
