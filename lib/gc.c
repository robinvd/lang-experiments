#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>

#include "./statepoint.h"

extern uint8_t __LLVM_StackMaps[];
extern void callGC();
void* stackmap = (void*)&__LLVM_StackMaps;

uint8_t* heapPtr;
uint8_t* basePtr;
size_t heapSize;
size_t startSize = 20300 * sizeof(char);

uint64_t gid = 1;

typedef struct {
  uint64_t tag;
  uint64_t id;
  uint64_t* newLoc;
} header;

uint8_t* secondHeap;
statepoint_table_t* table;

void initGC () {
  heapSize = startSize;
  heapPtr = malloc(startSize);
  basePtr = heapPtr;

  secondHeap = malloc(startSize);
  memset(secondHeap, 0x7F, heapSize); 

  /* printf("printing the table...\n"); */
  table = generate_table(stackmap, 0.5);
  /* print_table(stdout, table, true); */
  /* printf("\n\n"); */
  assert(lookup_return_address(table, 0) == NULL);
}

uint64_t* relocate(uint64_t** slot, char* heapPtr) {
  header* h = (header*) (slot - sizeof(header));
  printf("headerLoc: %p, tag: %x, id: %x, newLoc: %p, val %llu\n",
      h, h -> tag, h -> id, h -> newLoc, **slot);

  if (h -> tag == 1) {
    *slot = h -> newLoc;    // update slot in stack frame
    return heapPtr;
  }

  uint64_t val = **slot; // read val

  header* nh = (header*) heapPtr;
  nh -> tag = 0;
  nh -> id = gid++;
  nh -> newLoc = 0;
  heapPtr += sizeof(header);

  *((uint64_t*)heapPtr) = val;  // write val
  *slot = heapPtr;    // update slot in stack frame

  h -> tag = 1;
  h -> newLoc = heapPtr;

  return heapPtr + sizeof(uint64_t); // bump heap ptr
}

void collectGarbage(uint8_t* stackPtr) {
  uint64_t retAddr = *((uint64_t*)stackPtr);

  stackPtr += sizeof(void*);
  frame_info_t* frame = lookup_return_address(table, retAddr);
  assert(frame != NULL);

  int startUsage = heapPtr - basePtr;
  uint8_t* newBase = secondHeap;
  uint8_t* newHeapPtr = secondHeap;


  int count = 0;
  while(frame != NULL) {
        
    uint16_t i;
    for(i = 0; i < frame->numSlots; i++) {
    printf("frame2: %p\n", frame);
      /* printf("poinderslot: %p\n", ) */
      pointer_slot_t ptrSlot = frame->slots[i];
      if(ptrSlot.kind >= 0) {
        // our example does not use derived pointers
        assert(false && "unexpected derived pointer\n");
      }

      uint64_t** ptr = (uint64_t**)(stackPtr + ptrSlot.offset);
      assert(*ptr != NULL);
      printf("ptr: %p, val:%llu\n", *ptr, **ptr);
      count++;
      newHeapPtr = relocate(ptr, newHeapPtr);
    }

    // move to next frame. seems we have to add one pointer size to
    // reach the next return address? NOTE
    printf("frame: %p\n", frame);
    stackPtr = stackPtr + frame->frameSize;

    // grab return address of the frame
    retAddr = *((uint64_t*)stackPtr);
    stackPtr += sizeof(void*); // step into frame
    frame = lookup_return_address(table, retAddr);
  }
    
    // swap spaces
  secondHeap = basePtr;
  basePtr = newBase;
  heapPtr = newHeapPtr;
  
  // overwrite old space with 1's to 
  // cause weird results if something's wrong.
  memset(secondHeap, 0x7F, heapSize); 

  int endUsage = heapPtr - basePtr;
  printf("moved %d items\n", count);
  printf("heap usage: %lld -> %lld\n", startUsage, endUsage);
}

__attribute__((noinline)) uint8_t* alloc(size_t size) {
  puts("start alloc");
  /* if (heapPtr + size + sizeof(header) > basePtr + heapSize) { */
    puts("start GC");
    collectGarbage( (uint64_t*) __builtin_frame_address(0) + 1);
    puts("end GC");
  /* } */

  puts("make header");
  header* h = (header*) heapPtr;
  h -> tag = 0;
  h -> id = gid++;
  h -> newLoc = 0;
  printf("h: %p", h);
  heapPtr += sizeof(header);

  uint8_t* ptr = heapPtr;
  heapPtr += size;

  puts("end alloc");
  return ptr;
}
