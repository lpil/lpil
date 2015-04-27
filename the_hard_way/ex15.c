#include <stdio.h>

/***********************************************
 *  All these loops result in the same thing!  *
 ***********************************************/


int main(int argc, char *argv[])
{
  int ages[] = {23, 42, 12, 89, 2};
  char *names[] = {
    "Alan", "Frank", "Mary", "John", "Lisa"
  };

  // We can safely get the number of elems in ages
  int count = sizeof(ages) / sizeof(int);
  int i;

  // Array indexing
  for (i = 0; i < count; ++i) {
    printf("%s has %d years alive.\n", names[i], ages[i]);
  }
  puts("---");

  // Set up pointers at the start of the arrays
  int *cur_age   = ages;
  char **cur_name = names; // Pointer to a pointer- names is a 2D array

  // Pointer arithmetic
  for (i = 0; i < count; ++i) {
    printf("%s has %d years alive.\n", *(cur_name + i), *(cur_age + i));
  }
  puts("---");

  // Pointer indexing
  for (i = 0; i < count; ++i) {
    printf("%s has %d years alive.\n", cur_name[i], cur_age[i]);
  }
  puts("---");

  
  // A really silly method
  for(
      cur_name = names, cur_age = ages;
      (cur_age - ages) < count;
      ++cur_age, ++cur_name
     )
  {
    printf("%s has %d years alive.\n", *cur_name, *cur_age);
  }

  return 0;
}
