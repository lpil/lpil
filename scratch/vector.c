#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

struct vector {
  char *contents;
  int size;
  int capacity;
};

struct vector *Vector_new(int capacity)
{
  char *contents     = malloc( capacity * sizeof(char) );
  struct vector *vec = malloc( sizeof(struct vector) );
  *vec = (struct vector) {
    .contents = contents,
    .capacity = capacity,
    .size = 0,
  };
  return vec;
}


void Vector_expand(struct vector *vec)
{
  vec->capacity *= 2;
  char *contents = malloc( vec->capacity * sizeof(char) );
  char *old = vec->contents;
  int i = vec->size;
  while (i--) {
    contents[i] = old[i];
  }
  vec->contents = contents;
  free(old);
}


void Vector_push(struct vector *vec, char elem)
{
  if (vec->size >= vec->capacity) {
    Vector_expand(vec);
  }
  vec->contents[vec->size] = elem;
  vec->size++;
}


void Vector_print(struct vector *vec)
{
  int i;
  printf("[");
  for (i = 0; i < vec->size; ++i) {
    printf("'%c', ", vec->contents[i]);
  }
  printf("\b\b]\n");
}


void Vector_set(struct vector *vec, int i, char elem)
{
  assert(i < vec->size);
  vec->contents[i] = elem;
}


void Vector_free(struct vector *vec)
{
  free( vec->contents );
  free( vec );
}


int main(void)
{
  int i;
  struct vector *v = Vector_new(1);
  Vector_push(v, 'H');
  Vector_push(v, 'e');
  Vector_push(v, 'l');
  Vector_push(v, 'l');
  Vector_push(v, 'o');
  Vector_push(v, '!');
  Vector_print(v);
  Vector_set(v, 1, 'u');
  Vector_print(v);
  printf("Vector size: %d\n", v->size);
  printf("Vector capacity: %d\n", v->capacity);
  for (i = 0; i < v->size; ++i) {
    printf("%c", v->contents[i]);
  }
  printf("\n");
  Vector_free(v);
  return 0;
}
