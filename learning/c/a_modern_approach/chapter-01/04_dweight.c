// Dimensional weight is something in the shipping industry. If the dimensional
// weight is larger than the actual weight, they use that instead.

#include <stdio.h>

int main(void)
{
  int height = 8;
  int length = 12;
  int width  = 10;
  int volume = height * length * width;
  int weight = (volume + 165) / 166;

  printf("Dimensions: %dx%dx%d\n", length, width, height);
  printf("Volume (cubic inches): %d\n", volume);
  printf("Dimensional weight (pounds): %d\n", weight);

  return 0;
}
