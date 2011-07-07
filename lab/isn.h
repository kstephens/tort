ISN(LIT, 1,
    push(arg0)
    )

ISN(POP, 0,
    pop()
    )

ISN(ADD, 0, 
    ({
      top[1] = top[0] + (size_t) top[1];
      pop();
    }))

ISN(PRINT, 0, 
    printf("%ld\n", (long) (size_t) pop());
    )

ISN(RTN, 0, 
    return
    )

#undef ISN
