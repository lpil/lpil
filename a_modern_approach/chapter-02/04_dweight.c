// Dimensional weight is something in the shipping industry. If the dimensional
// weight is larger than the actual weight, they use that instead.

#include <stdio.h>

int main(int argc, char *argv[])
{
  int height;
  int length;
  int width;

  puts("Height?");
  scanf("%d", &height);
  puts("Length?");
  scanf("%d", &length);
  puts("Width?");
  scanf("%d", &width);

  int volume = height * length * width;
  int weight = (volume + 165) / 166;

  printf("Dimensions: %dx%dx%d\n", length, width, height);
  printf("Volume (cubic inches): %d\n", volume);
  printf("Dimensional weight (pounds): %d\n", weight);

  return 0;
}

