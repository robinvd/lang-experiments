target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
; ModuleID = 'main'


declare external ccc void @puts(i8*)

declare external ccc void @initGC()

declare external ccc void @exit()

declare external ccc void @fflush(i8*)

declare external ccc i8 addrspace(1)* @alloc(i64) gc "statepoint-example"

declare external ccc void @prInt(i64 addrspace(1)*) gc "statepoint-example"

define private fastcc i64 addrspace(1)* @"+"(i64 addrspace(1)*, i64 addrspace(1)*) alwaysinline gc "statepoint-example"{
  %3 = load i64, i64 addrspace(1)* %0, align 8
  %4 = load i64, i64 addrspace(1)* %1, align 8
  %5 = add i64 %3, %4
  %6 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %7 = bitcast i8 addrspace(1)* %6 to i64 addrspace(1)*
  store i64 %5, i64 addrspace(1)* %7, align 8
  ret i64 addrspace(1)* %7
}

define private fastcc i64 addrspace(1)* @-(i64 addrspace(1)*, i64 addrspace(1)*) alwaysinline gc "statepoint-example"{
  %3 = load i64, i64 addrspace(1)* %0, align 8
  %4 = load i64, i64 addrspace(1)* %1, align 8
  %5 = sub i64 %3, %4
  %6 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %7 = bitcast i8 addrspace(1)* %6 to i64 addrspace(1)*
  store i64 %5, i64 addrspace(1)* %7, align 8
  ret i64 addrspace(1)* %7
}

define external ccc i64 @main(){
  call fastcc void @initGC()
  %1 = call fastcc i64 addrspace(1)* @userMain()
  call ccc void @fflush(i8* inttoptr (i8 0 to i8*))
  call fastcc void @prInt(i64 addrspace(1)* %1)
  ret i64 0
}

define private fastcc i64 addrspace(1)* @isOdd0(i64 addrspace(1)* %x) gc "statepoint-example"{
; <label>:0:
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 0, i64 addrspace(1)* %2, align 8
  %3 = load i64, i64 addrspace(1)* %x, align 8
  %4 = load i64, i64 addrspace(1)* %2, align 8
  %5 = icmp eq i64 %3, %4
  br i1 %5, label %cond, label %cond1
cond:
  %6 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %7 = bitcast i8 addrspace(1)* %6 to i64 addrspace(1)*
  store i64 0, i64 addrspace(1)* %7, align 8
  br label %end
cond1:
  br label %cond2
cond2:
  %8 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %9 = bitcast i8 addrspace(1)* %8 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %9, align 8
  %10 = call fastcc i64 addrspace(1)* @-(i64 addrspace(1)* %x, i64 addrspace(1)* %9)
  %11 = call fastcc i64 addrspace(1)* @isEven1(i64 addrspace(1)* %10)
  br label %end
end:
  %12 = phi i64 addrspace(1)* [%7, %cond], [%11, %cond2]
  ret i64 addrspace(1)* %12
}

define private fastcc i64 addrspace(1)* @isEven1(i64 addrspace(1)* %x) gc "statepoint-example"{
; <label>:0:
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 0, i64 addrspace(1)* %2, align 8
  %3 = load i64, i64 addrspace(1)* %x, align 8
  %4 = load i64, i64 addrspace(1)* %2, align 8
  %5 = icmp eq i64 %3, %4
  br i1 %5, label %cond, label %cond1
cond:
  %6 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %7 = bitcast i8 addrspace(1)* %6 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %7, align 8
  br label %end
cond1:
  br label %cond2
cond2:
  %8 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %9 = bitcast i8 addrspace(1)* %8 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %9, align 8
  %10 = call fastcc i64 addrspace(1)* @-(i64 addrspace(1)* %x, i64 addrspace(1)* %9)
  %11 = call fastcc i64 addrspace(1)* @isOdd0(i64 addrspace(1)* %10)
  br label %end
end:
  %12 = phi i64 addrspace(1)* [%7, %cond], [%11, %cond2]
  ret i64 addrspace(1)* %12
}

define private fastcc i64 addrspace(1)* @f2(i8* %x) gc "statepoint-example"{
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 50, i64 addrspace(1)* %2, align 8
  ret i64 addrspace(1)* %2
}

define private fastcc i64 addrspace(1)* @g3(i64 addrspace(1)* %x) gc "statepoint-example"{
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 2, i64 addrspace(1)* %2, align 8
  %3 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %4 = bitcast i8 addrspace(1)* %3 to i64 addrspace(1)*
  store i64 5, i64 addrspace(1)* %4, align 8
  %5 = call fastcc i64 addrspace(1)* @-(i64 addrspace(1)* %4, i64 addrspace(1)* %x)
  %6 = call fastcc i64 addrspace(1)* @"+"(i64 addrspace(1)* %2, i64 addrspace(1)* %5)
  %7 = call fastcc i64 addrspace(1)* @"+"(i64 addrspace(1)* %x, i64 addrspace(1)* %6)
  ret i64 addrspace(1)* %7
}

define private fastcc i64 addrspace(1)* @fib4(i64 addrspace(1)* %x) gc "statepoint-example"{
; <label>:0:
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 0, i64 addrspace(1)* %2, align 8
  %3 = load i64, i64 addrspace(1)* %x, align 8
  %4 = load i64, i64 addrspace(1)* %2, align 8
  %5 = icmp eq i64 %3, %4
  br i1 %5, label %cond, label %cond1
cond:
  %6 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %7 = bitcast i8 addrspace(1)* %6 to i64 addrspace(1)*
  store i64 0, i64 addrspace(1)* %7, align 8
  br label %end
cond1:
  %8 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %9 = bitcast i8 addrspace(1)* %8 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %9, align 8
  %10 = load i64, i64 addrspace(1)* %x, align 8
  %11 = load i64, i64 addrspace(1)* %9, align 8
  %12 = icmp eq i64 %10, %11
  br i1 %12, label %cond2, label %cond3
cond2:
  %13 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %14 = bitcast i8 addrspace(1)* %13 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %14, align 8
  br label %end
cond3:
  br label %cond4
cond4:
  %15 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %16 = bitcast i8 addrspace(1)* %15 to i64 addrspace(1)*
  store i64 1, i64 addrspace(1)* %16, align 8
  %17 = call fastcc i64 addrspace(1)* @-(i64 addrspace(1)* %x, i64 addrspace(1)* %16)
  %18 = call fastcc i64 addrspace(1)* @fib4(i64 addrspace(1)* %17)
  %19 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %20 = bitcast i8 addrspace(1)* %19 to i64 addrspace(1)*
  store i64 2, i64 addrspace(1)* %20, align 8
  %21 = call fastcc i64 addrspace(1)* @-(i64 addrspace(1)* %x, i64 addrspace(1)* %20)
  %22 = call fastcc i64 addrspace(1)* @fib4(i64 addrspace(1)* %21)
  %23 = call fastcc i64 addrspace(1)* @"+"(i64 addrspace(1)* %18, i64 addrspace(1)* %22)
  br label %end
end:
  %24 = phi i64 addrspace(1)* [%7, %cond], [%14, %cond2], [%23, %cond4]
  ret i64 addrspace(1)* %24
}

define private fastcc i64 addrspace(1)* @userMain() gc "statepoint-example"{
  %x = call ccc i8 addrspace(1)* @alloc(i64 8)
  %x1 = bitcast i8 addrspace(1)* %x to i64 addrspace(1)*
  store i64 42, i64 addrspace(1)* %x1, align 8
  %1 = call ccc i8 addrspace(1)* @alloc(i64 8)
  %2 = bitcast i8 addrspace(1)* %1 to i64 addrspace(1)*
  store i64 20, i64 addrspace(1)* %2, align 8
  %3 = call fastcc i64 addrspace(1)* @fib4(i64 addrspace(1)* %2)
  ret i64 addrspace(1)* %3
}