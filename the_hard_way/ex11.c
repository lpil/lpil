#include <stdio.h>

int main(int argc, char *argv[])
{
  int i = 0;
  while(i < argc) {
    printf("arg %d: %s\n", i, argv[i]);
    ++i;
  }

  char *countries[] = {
    "England",
    "Ireland",
    "Scotland",
    "Wales"
  };
  int num_states = 4;

  i = 0;
  while(i < num_states) {
    printf("country %d: %s\n", i, countries[i]);
    ++i;
  }

  return 0;
}
