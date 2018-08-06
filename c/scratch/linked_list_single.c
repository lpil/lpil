#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


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
  struct cons_cell *head = List_new(n);
  head->next = list;
  return head;
}


void List_print(struct cons_cell *head)
{
  printf("(");
  struct cons_cell *next;
  next = head->next;
  while (next != NULL) {
    printf("%d, ", head->value);
    head = next;
    next = head->next;
  }
  printf("%d)\n", head->value);
}


void List_free(struct cons_cell *head)
{
  struct cons_cell *next;
  next = head->next;
  do {
    free(head);
    head = next;
    next = head->next;
  } while (next != NULL);
  free(head);
}


int List_count(struct cons_cell *head)
{
  int size = 1;
  struct cons_cell *next;
  next = head->next;
  do {
    size++;
    head = next;
    next = head->next;
  } while (next != NULL);
  return size;
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
  printf("Size: %d\n", List_count(list));
  List_free(list);
  return 0;
}
