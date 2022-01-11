// Dimensional weight is something in the shipping industry. If the dimensional
// weight is larger than the actual weight, they use that instead.

#include <stdio.h>

#define CUBIC_IN_PER_LB 166

int main(void)
{
  int height = 4;
  int length = 15;
  int width  = 55;

  int volume = height * length * width;

  printf("Dimensions: %dx%dx%d\n", length, width, height);
  printf("Volume (cubic inches): %d\n", volume);
  printf(
      "Dimensional weight (pounds): %d\n",
      (volume + CUBIC_IN_PER_LB - 1) / CUBIC_IN_PER_LB
      );

  return 0;
}
