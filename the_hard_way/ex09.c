#include <stdio.h>

int main(int argc, char *argv[]) {

  // If we don't specify all the elements C will set them to \0
  int numbers[4] = {0};
  char name[4] = {'a'};

  printf("numbers: %d %d %d %d\n",
      numbers[0], numbers[1], numbers[2], numbers[3]);
  printf("name chars: %c %c %c %c\n",
      name[0], name[1], name[2], name[3]);
  printf("name: %s\n", name);

  numbers[0] = 1;
  numbers[1] = 2;
  numbers[2] = 3;
  numbers[3] = 4;

  name[0] = 'F';
  name[1] = 'o';
  name[2] = 'o';
  name[3] = '\0';

  printf("numbers: %d %d %d %d\n",
      numbers[0], numbers[1], numbers[2], numbers[3]);
  printf("name chars: %c %c %c %c\n",
      name[0], name[1], name[2], name[3]);
  printf("name: %s\n", name);

  // Another, apparently more common way to make strings
  char *another = "Bar";

  printf("another chars: %c %c %c %c\n",
      another[0], another[1], another[2], another[3]);
  printf("another: %s\n", another);

  return 0;
}
