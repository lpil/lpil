#include <stdio.h>

int main(void)
{
  // Take an int from user input and store it in i
  int i;
  puts("Gimmie an int");
  scanf("%d", &i);
  printf("You gave me a %d\n", i);

  // Take a float from user input and store it in f
  float f;
  puts("Gimmie a float");
  scanf("%f", &f);
  printf("You gave me a %f\n", f);

  return 0;
}
