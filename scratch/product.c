#include <stdio.h>
#include <assert.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))


// This can totally overflow.
//
int product(int arr[], int arr_size)
{
  assert(arr_size > 0);

  int acc = 1;
  while(arr_size--) {
    acc *= arr[arr_size];
  }
  return acc;
}


int main(void)
{
  int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  int size = ARRAY_SIZE(arr);
  int prod = product(arr, size);
  printf("%d\n", prod);
  return 0;
}
