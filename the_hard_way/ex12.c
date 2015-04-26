#include <stdio.h>

int main(int argc, char *argv[])
{
  int i;

  if (argc == 1) {
    puts("You only have one argument. You suck.");
  } else if (argc > 1 && argc < 4) {
    
    puts("Here's your args!");
    for (i = 1; i < argc; ++i) {
      printf("%s ", argv[i]);
    }
    printf("\n");

  } else {
    puts("Too many args! :(");
  }

  return 0;
}
