#include <stdlib.h>
#include <stdio.h>

#define SIZE 200

int8_t* mem;
int8_t* curr;
int8_t* max;

void initGC() {
  mem = malloc(SIZE * sizeof(int8_t));
  curr = mem;
  max = mem + SIZE;
  return;
}

// s is the amount in bytes (int8_t)
int8_t* alloc(size_t s) {
  if (curr + s > max) {
    puts("GCERR: out of mem");
    exit(3);
  }
  int8_t* space = curr;
  curr += s;
  return space;
}
