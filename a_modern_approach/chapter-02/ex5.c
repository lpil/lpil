#include <stdio.h>
#include <math.h>

// Calculate the volume of a sphere with a 10 meter radius.
// v = 4/3 pi r^3

int main(int argc, char *argv[])
{
  int   radius = 10.0;
  float volume = (4.0 / 3.0) * M_PI * pow(radius, 3);

  printf("%f\n", volume);

  return 0;
}
