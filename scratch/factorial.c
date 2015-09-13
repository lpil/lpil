#include <stdio.h>


int factorial(int n)
{
  int fac = 1;
  while (n > 0) {
    fac *= n;
    n--;
  }
  return fac;
}


int main(void)
{
  int i;
  for (i = -10; i <= 10; ++i) {
    printf("%d! == %d\n", i, factorial(i));
  }
  return 0;
}
