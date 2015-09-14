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

struct cons_cell *List_prepend(struct cons_cell *list, int n)
{
  struct cons_cell *head = malloc( sizeof(struct cons_cell) );
  *head = (struct cons_cell) { .value = n, .next = list };
  return head;
}


void List_print(struct cons_cell *head)
{
  printf("(");
  struct cons_cell *next;
  next = head->next;
  while(next != NULL) {
    printf("%d, ", head->value);
    head = next;
    next = head->next;
  }
  printf("%d)\n", head->value);
}

int main(void)
{
  struct cons_cell *list = List_new(1);
  list = List_prepend(list, 2);
  list = List_prepend(list, 3);
  list = List_prepend(list, 4);
  list = List_prepend(list, 5);
  list = List_prepend(list, 6);
  List_print(list);
  return 0;
}
