# todo/ideas
- ADT/structs (patterns) + kind type system
- better infix support (priority, user def)
- better renamer, no bound
- modules + naming
- inline asm
- lower expr data type
  - almost llvm but without registers
    so alloc/store/load/call/br
    (without metadata?)

syntax (uses bound) -> (renamer) Core () Id -> (typechecker) Core Type Id -> (?) Lower -> (Emit) LLVM
