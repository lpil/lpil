#include <stdio.h>

char* foo(char arr[])
{
  return arr;
}

int main(void)
{
  char bar[] = {1, 2, 3, 4};
  foo(bar);
  return 0;
}
