#include <stdlib.h>
#include <stdio.h>

struct cons_cell {
  int value;
  struct cons_cell *next;
};

struct cons_cell *List_new(int n)
{
  struct cons_cell *cell = malloc( sizeof(struct cons_cell) );
  *cell = (struct cons_cell) { .value = n, .next = NULL };
  return cell;
}


int main(void)
{
  struct cons_cell *cell = List_new(2);
  printf("Value is %d!\n", cell->value);
  return 0;
}
