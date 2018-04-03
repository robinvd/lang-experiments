#define PRINT_STUFF

#include <stdio.h>
#include "./llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

// for PRIu and PRId
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

void pr(uint64_t *x) {
  printf("val is: %d\n", *x);
  return;
}

extern void enterGC();

extern uint8_t __LLVM_StackMaps[];
size_t heapSizeB = 500;
uint8_t* heapBase;
uint8_t* heapPtr; // points at the first free spot in the heap

// #define PRINT_STUFF

bool tableBuilt = false;
statepoint_table_t* table;
uint8_t* auxHeap;

void initGC() {
  uint8_t* mem = malloc(heapSizeB * sizeof(uint8_t));
  heapBase = mem;
  heapPtr = mem;
}

uint8_t* relocate_uint32star(uint8_t** slot, uint8_t* heapPtr) {
    uint64_t** sl = (uint64_t**) slot;
    uint64_t* hp = (uint64_t*) heapPtr;

    uint64_t val = (uint64_t) **sl; // read val
    *hp = val;  // write val
    *sl = hp;    // update slot in stack frame
    return heapPtr + 1; // bump heap ptr
}


void doGC(uint8_t* stackPtr) {
    void* stackmap = (void*)&__LLVM_StackMaps;
    
    if(!tableBuilt) {
        printf("stackPtr = 0x%" PRIX64 "\n", (uint64_t)stackPtr);
        
        // setup aux heap
        printf("aux heap size = %llu bytes\n", heapSizeB);
        auxHeap = (uint8_t*) malloc(heapSizeB * sizeof(uint8_t));
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
    frame_info_t* frame = lookup_return_address(table, retAddr);

    stackPtr += sizeof(void*); // step into frame
    
    // we'll be moving live stuff to the current aux heap
    uint8_t* newBase = auxHeap;
    uint8_t* newHeapPtr = auxHeap;
    
#ifdef PRINT_STUFF
    printf("\n\n--- starting to scan the stack for gc ---\n");
    printf("frame return address: 0x%" PRIX64 "\n", retAddr);
    printf("frame: %lld\n", frame);
#endif
    int copied=0;
    
    while(frame != NULL) {
        
        printf("nslots: %d\n", frame->numSlots);
        uint16_t i;
        for(i = 0; i < frame->numSlots; i++) {
            pointer_slot_t ptrSlot = frame->slots[i];
            if(ptrSlot.kind >= 0) {
                // our example does not use derived pointers
                assert(false && "unexpected derived pointer\n");
            }
            
            printf("reloc\n");
            uint8_t** ptr = (uint8_t**)(stackPtr + ptrSlot.offset);
            newHeapPtr = relocate_uint32star(ptr, newHeapPtr);
            copied += 1;
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
    printf("copied %d elements\n", copied);
    printf("heapBase: %llu, heapPtr %llu\n", newBase, newHeapPtr);
    printf("Reached the end of the stack.\n\n");
#endif
    
    // swap spaces
    auxHeap = heapBase;
    heapBase = newBase;
    heapPtr = newHeapPtr;
    
    // overwrite old space with 1's to 
    // cause weird results if something's wrong.
    memset(auxHeap, 0x7F, heapSizeB); 
}

uint8_t* alloc(size_t s) {
  if (heapPtr + s > heapBase + heapSizeB - 8) {
    /* enterGC(); */
    /* __asm__ ( "mov %rsp, %rdi;" */
    /*           "jmp doGC;" */
    /* ); */
    __asm__ ( "callq enterGC ;"
    );
  }

  uint8_t* mem = heapPtr;
  heapPtr += s;

#ifdef PRINT_STUFF
  /* printf("heapBase: %llu, heapPtr %llu\n", heapBase, heapPtr); */
#endif
  return mem;
}
