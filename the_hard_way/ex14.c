#include <stdio.h>
#include <ctype.h>

// Forward function declarations
// Allows use of functions before their body is defined
void print_letters(char arg[]);


int can_print(char ch)
{
  return isalpha(ch) || isblank(ch);
}

void print_arguments(int argc, char *argv[])
{
  int i;

  for (i = 0; i < argc; ++i) {
    print_letters(argv[i]);
  }
}

void print_letters(char arg[])
{
  int i = 0;
  while (arg[i] != '\0') {

    char ch = arg[i];

    if (can_print(ch)) {
      printf("'%c' == %d\n", ch, ch);
    }
    ++i;
  }
}


int main(int argc, char *argv[])
{
  print_arguments(argc, argv);
  return 0;
}
