#include <stdio.h>

int main(void)
{
  char name[] = "Louis Adam Pilfold.";
  int i = sizeof(name) / sizeof(name[0]);

  while(i--) {
    printf("%c", name[i]);
  }
  printf("\n");

  return 0;
}
