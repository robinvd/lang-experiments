target datalayout = "e-m:e-i32:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
; ModuleID = 'main'

@heapSizeB = local_unnamed_addr global i32 400, align 8
@heapBase = common local_unnamed_addr global i8 addrspace(1)* null, align 8
@heapPtr = common local_unnamed_addr global i8 addrspace(1)* null, align 8
