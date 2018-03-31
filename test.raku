let
  isOdd = (x) -> case x {
    0 -> 0  
    x -> isEven(sub(x,1))
  }
  isEven = (x) -> case x {
    0 -> 1
    x -> isOdd(x - 1)
  }
  f = (x) -> 50
  half = (x) -> fdiv(x,2.0)
in isEven(11)
