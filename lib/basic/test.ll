; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nounwind sspstrong uwtable
define i32 @main() local_unnamed_addr #0 gc "statepoint-example" {
  call void @initGC()
  %1 = tail call i8 addrspace(1)* @alloc(i64 8) #2
  %2 = bitcast i8 addrspace(1)* %1 to i32 addrspace(1)*
  store i32 5, i32 addrspace(1)* %2, align 4, !tbaa !3
  call void @prp(i32 addrspace(1)* %2)
  %3 = load i32, i32 addrspace(1)* %2
  call void @pr(i32 %3)
  tail call void (...) @enterGC() #2
  call void @prp(i32 addrspace(1)* %2)
  %4 = load i32, i32 addrspace(1)* %2
  call void @pr(i32 %4)
  ret i32 0
}

declare i8 addrspace(1)* @alloc(i64) local_unnamed_addr #1

declare void @pr(i32)
declare void @prp(i32 addrspace(1)*)

declare void @enterGC(...) local_unnamed_addr #1
 
declare void @initGC()

attributes #0 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
!3 = !{!4, !4, i64 0}
!4 = !{!"int", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
