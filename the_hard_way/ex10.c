#include <stdio.h>

int main(int argc, char *argv[])
{
  int i = 0;

  for (i = 1; i < argc; ++i) {
    printf("arg %d: %s\n", i, argv[i]);
  }

  // Let's create an array of strings
  char *countries[] = {
    "England",
    "Ireland",
    "Scotland",
    "Wales"
  };
  int num_states = 4;

  for (i = 0; i < num_states; ++i) {
    printf("country %d: %s\n", i, countries[i]);
  }

  return 0;
}
