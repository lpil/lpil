#include <stdlib.h>
#include <stdio.h>

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


struct vector *Vector_expand(struct vector *vec)
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
  return vec;
}


struct vector *Vector_push(struct vector *vec, char data)
{
  if (vec->size >= vec->capacity) {
    vec = Vector_expand(vec);
  }
  vec->contents[vec->size] = data;
  vec->size++;
  return vec;
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


void Vector_free(struct vector *vec)
{
  free( vec->contents );
  free( vec );
}


int main(void)
{
  int i;
  struct vector *v = Vector_new(1);
  v = Vector_push(v, 'H');
  v = Vector_push(v, 'e');
  v = Vector_push(v, 'l');
  v = Vector_push(v, 'l');
  v = Vector_push(v, 'o');
  v = Vector_push(v, '!');
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
