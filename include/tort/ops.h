#ifdef UOP
#ifndef BOP_NO_INT
UOP(INV,~)
#endif
#ifndef UOP_NO_NEG
UOP(NEG,-)
#undef  UOP_NO_NEG
#endif
#endif

#ifdef BOP
BOP(ADD,+)
BOP(SUB,-)
BOP(MUL,*)
BOP(DIV,/)
#ifndef BOP_NO_INT
BOP(AND,&)
BOP(OR,|)
BOP(XOR,^)
BOP(MOD,%)
BOP(LSH,<<)
BOP(RSH,>>)
#endif
#endif

#ifdef LUP
LUP(NOT,!)
#endif

#ifdef LOP
LOP(LAND,&&)
LOP(LOR,||)
#endif

#ifdef ROP
ROP(EQ,==)
ROP(NE,!=)
ROP(LT,<)
ROP(GT,>)
ROP(LE,<=)
ROP(GE,>=)
#endif

#ifdef UOP
#undef UOP
#endif
#ifdef BOP
#undef BOP
#endif
#ifdef LOP
#undef LOP
#endif
#ifdef LUP
#undef LUP
#endif
#ifdef ROP
#undef ROP
#endif


