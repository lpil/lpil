#define MIN(x, y) (((x) < (y)) ? (x) : (y))

int compute(char *x, int x_size, char *y, int y_size)
{
  int distance = 0;
  int i = MIN(x_size, y_size);
  while (i--) {
    if (x[i] != y[i]) { distance++; }
  }
  return distance;
}
