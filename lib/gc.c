
#include <stdio.h>
#include "./statepoint.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

// for PRIu and PRId
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

extern uint8_t __LLVM_StackMaps[];
extern uint64_t heapSizeB;
extern uint8_t* heapBase;
extern uint8_t* heapPtr; // points at the first free spot in the heap
uint8_t* auxHeap;

// #define PRINT_STUFF

typedef union {
  void* newLoc;
  uint64_t size;
} body;

typedef struct {
  uint64_t tag;
  body body;
} header;

void initHeader(header* h, uint64_t s) {
  h -> tag = 0;
  h -> body.size = s;
}

bool tableBuilt = false;
statepoint_table_t* table;

void initGC() {
  heapPtr = malloc(heapSizeB);
  heapBase = heapPtr;
}

uint32_t* relocate_uint32star(uint8_t** slot, uint8_t* heapPtr) {
  header* h = (header*) (*slot - sizeof(header));

  if (h -> tag == 1) {
    *slot = h -> body.newLoc;
    return heapPtr;
  }

  uint8_t* valLoc = *slot;

  header* nh = (header*) heapPtr;
  initHeader(nh, h -> body.size);
  heapPtr += sizeof(header);

  memcpy(heapPtr, *slot, h -> body.size); // cpy val
  h -> tag = 1;
  h -> body.newLoc = heapPtr;
  *slot = heapPtr;    // update slot in stack frame
  return heapPtr + nh->body.size; // bump heap ptr
}


void doGC(uint8_t* stackPtr) {
    void* stackmap = (void*)&__LLVM_StackMaps;
    int before = (int) (heapPtr - heapBase);
    
    if(!tableBuilt) {
        printf("stackPtr = 0x%" PRIX64 "\n", (uint64_t)stackPtr);
        
        // setup aux heap
        printf("aux heap size = %llu bytes\n", heapSizeB);
        auxHeap = (uint32_t*) malloc(heapSizeB);
        memset(auxHeap, 0x7F, heapSizeB); 
        
        printf("printing the table...\n");
        table = generate_table(stackmap, 0.5);
        print_table(stdout, table, true);
        printf("\n\n");
        assert(lookup_return_address(table, 0) == NULL);
        // destroy_table(table);
        tableBuilt = true;
    }
    
    
    
    uint64_t retAddr = *((uint64_t*)stackPtr);
    stackPtr += sizeof(void*); // step into frame
    frame_info_t* frame = lookup_return_address(table, retAddr);
    
    // we'll be moving live stuff to the current aux heap
    uint32_t* newBase = auxHeap;
    uint32_t* newHeapPtr = auxHeap;
    
#ifdef PRINT_STUFF
    printf("\n\n--- starting to scan the stack for gc ---\n");
    printf("frame return address: 0x%" PRIX64 "\n", retAddr);
#endif
    
    while(frame != NULL) {
        
        uint16_t i;
        for(i = 0; i < frame->numSlots; i++) {
            pointer_slot_t ptrSlot = frame->slots[i];
            if(ptrSlot.kind >= 0) {
                // our example does not use derived pointers
                assert(false && "unexpected derived pointer\n");
            }
            
            uint32_t** ptr = (uint32_t**)(stackPtr + ptrSlot.offset);
            newHeapPtr = relocate_uint32star(ptr, newHeapPtr);
        }
        
        // printf("\trelocated %" PRIu16 " pointer(s).\n", i);
        
        // move to next frame. seems we have to add one pointer size to
        // reach the next return address? NOTE
        stackPtr = stackPtr + frame->frameSize;
        
        // grab return address of the frame
        retAddr = *((uint64_t*)stackPtr);
        stackPtr += sizeof(void*); // step into frame
        frame = lookup_return_address(table, retAddr);
        
#ifdef PRINT_STUFF
        printf("frame return address: 0x%" PRIX64 "\n", retAddr);
#endif
    }
    
#ifdef PRINT_STUFF
    printf("Reached the end of the stack.\n\n");
#endif
    
    // swap spaces
    auxHeap = heapBase;
    heapBase = newBase;
    heapPtr = newHeapPtr;
    
    // overwrite old space with 1's to 
    // cause weird results if something's wrong.
    memset(auxHeap, 0x7F, heapSizeB); 

    int after = (int) (heapPtr - heapBase);
    printf("gc result %d -> %d\n", before, after);
}

__attribute__((noinline)) uint8_t* alloc(int size) {
  /* printf("%p %p\n", heapPtr + 4, (char)heapBase + heapSizeB - 32); */
  if (heapPtr + sizeof(header) + size > heapBase + heapSizeB) {
    doGC( (uint64_t*) __builtin_frame_address(0) + 1);
  }

  if (heapPtr + sizeof(header) + size > heapBase + heapSizeB) {
    free(auxHeap);
    auxHeap = malloc(heapSizeB * 2);
    doGC( (uint64_t*) __builtin_frame_address(0) + 1);

    free(auxHeap);
    auxHeap = malloc(heapSizeB * 2);

    heapSizeB *= 2;
  }

  header* h = (header*) heapPtr;
  initHeader(h, size);
  heapPtr += sizeof(header);

  char* ptr = heapPtr;
  heapPtr += size;

  return ptr;
}

void prInt(int* x) {
  printf("%d\n", *x);
}
