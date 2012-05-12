#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  printf("%d\n", (int) (sizeof(tort_v) * 8));
  return 0;
}

