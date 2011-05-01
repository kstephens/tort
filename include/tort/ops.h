#ifdef UOP
UOP(NOT,!)
#ifndef BOP_NO_INT
UOP(INV,~)
#endif
#ifndef UOP_NO_NEG
UOP(NEG,-)
#undef  UOP_NO_NEG
#endif
#undef UOP
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
#undef BOP
#endif

#ifdef ROP
ROP(LAND,&&)
ROP(LOR,||)
ROP(EQ,==)
ROP(NE,!=)
ROP(LT,<)
ROP(GT,>)
ROP(LE,<=)
ROP(GE,>=)
#undef ROP
#endif

