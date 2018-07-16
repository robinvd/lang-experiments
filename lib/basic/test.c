#include <stdlib.h>
#include <stdio.h>

extern int8_t* alloc(size_t);
extern void enterGC();

int main() {
  int* num = (int*) alloc(sizeof(num));
  *num = 5;
  printf("%d\n", *num);
  enterGC();
  printf("%d\n", *num);
}
