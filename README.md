# lang-experiments

A functional language experiment using haskell and llvm.

It has a functional, basic gc written in c.

# example program
```
// 0 is false
// 1 is true
let
  x = 42
  isOdd = (x) -> case x {
    0 -> 0  
    x -> isEven(x - 1)
  }
  isEven = (x) -> case x {
    0 -> 1
    x -> isOdd(x - 1)
  }
  f = (x) -> 50
  g = (x) -> x + 2 + 5 - x
  fib = (x) -> case x {
      0 -> 0
      1 -> 1
      x -> fib(x - 1) + fib(x - 2)
    }
  // half = (x) -> fdiv(x,2.0)
in fib(20)
```
