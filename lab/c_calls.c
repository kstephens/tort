
typedef void *voidP;
typedef long long long_long;
typedef long double long_double;

#define F(RT) \
  RT RT##_func(char char_, short short_, int int_, long long_, long long long_long_, float float_, double double_, long double long_double_, void *voidP_) \
  {									\
    int void_ = 0;							\
    printf("i = %d\n", int_);						\
    return (RT) RT##_;							\
  }
F(void);
F(char);
F(short);
F(int);
F(long);
F(long_long);
F(float);
F(double);
F(long_double);
F(voidP);

/*
ia64 return values:
void : nothing
char, short, int : %eax
long, long long, void* : %rax 
float, double: %xmm0
long double: via fldt instruction.
*/
