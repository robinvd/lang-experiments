# running
run with `clang -g -Wall -O0 test.s gc.c statepoint.c && ./a.out`

# results
- lots of debug output
- segfault, changes like adding a printf also changes the segfault location
- maybe stack corruption?

# rebuild
test.s is build using `cabal run` in the root folder, this generates the test.ll test.bc and test.s files

can also be compiled from the .ll file by
```
opt -rewrite-statepoints-for-gc test.ll
llc lest.bc
clang test.s gc.c statepoint.c
```
