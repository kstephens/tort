#include <stdio.h>
#include <stddef.h>
#include <sys/mman.h>

typedef void (*new_stack_func)(void *arg);

static 
void new_stack(new_stack_func func, void *data)
{
  size_t size = 16 * 1024;
  void *ptr;

  ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (ptr == (void*) -1LL) {
    perror("can't alloc machine stacks");
  }
#if 0
  if (mprotect(ptr,
	       1024, PROT_READ | PROT_WRITE) < 0) {
    perror("mprotect failed");
  }
#endif
  
  /*
    +----------------+                    -------------------+
    |                |                                       |
    +----------------+                    -------------------+
    |                 |                    |    |
    ptr               ptr + size           sp   fp
  */
  void *sp = __builtin_alloca((size_t) 0); /* (void*) &ptr; */
  long long dist = sp - (ptr + size);
  fprintf(stderr, "  new_stack\n        sp = %018p\n       ptr = %018p\n      dist = 0x%016llx\n",
	  sp, ptr, (long long) dist);
  size_t pad = sizeof(void*);
  void *new_sp = __builtin_alloca((dist + pad) & ~pad);
  fprintf(stderr, "    new_sp = %018p\n", new_sp);
  func(data);
}

static
void a(void *arg)
{
  fprintf(stderr, "a(%s) @ %p\n", arg, &arg);
}

int main(int argc, char **argv)
{
  fprintf(stderr, "main() @ %p\n", &argv);
  a("from main()");
  new_stack(a, "from new_stack()");  
  return 0;
}
