; ModuleID = '<stdin>'
source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nounwind sspstrong uwtable
define i32 @main() local_unnamed_addr #0 gc "statepoint-example" {
  %statepoint_token = call token (i64, i32, void ()*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidf(i64 2882400000, i32 0, void ()* @initGC, i32 0, i32 0, i32 0, i32 0)
  %statepoint_token4 = tail call token (i64, i32, i8 addrspace(1)* (i64)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_p1i8i64f(i64 2882400000, i32 0, i8 addrspace(1)* (i64)* @alloc, i32 1, i32 0, i64 8, i32 0, i32 0) #3
  %1 = call i8 addrspace(1)* @llvm.experimental.gc.result.p1i8(token %statepoint_token4)
  %2 = bitcast i8 addrspace(1)* %1 to i32 addrspace(1)*
  store i32 5, i32 addrspace(1)* %2, align 4, !tbaa !3
  %statepoint_token5 = call token (i64, i32, void (i32 addrspace(1)*)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidp1i32f(i64 2882400000, i32 0, void (i32 addrspace(1)*)* @prp, i32 1, i32 0, i32 addrspace(1)* %2, i32 0, i32 0, i8 addrspace(1)* %1)
  %3 = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token %statepoint_token5, i32 8, i32 8) ; (%1, %1)
  %.remat = bitcast i8 addrspace(1)* %3 to i32 addrspace(1)*
  %4 = load i32, i32 addrspace(1)* %.remat
  %statepoint_token6 = call token (i64, i32, void (i32)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidi32f(i64 2882400000, i32 0, void (i32)* @pr, i32 1, i32 0, i32 %4, i32 0, i32 0, i8 addrspace(1)* %3)
  %5 = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token %statepoint_token6, i32 8, i32 8) ; (%3, %3)
  %.remat1 = bitcast i8 addrspace(1)* %5 to i32 addrspace(1)*
  %statepoint_token7 = tail call token (i64, i32, void (...)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidvarargf(i64 2882400000, i32 0, void (...)* @enterGC, i32 0, i32 0, i32 0, i32 0, i8 addrspace(1)* %5) #3
  %6 = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token %statepoint_token7, i32 7, i32 7) ; (%5, %5)
  %.remat2 = bitcast i8 addrspace(1)* %6 to i32 addrspace(1)*
  %statepoint_token8 = call token (i64, i32, void (i32 addrspace(1)*)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidp1i32f(i64 2882400000, i32 0, void (i32 addrspace(1)*)* @prp, i32 1, i32 0, i32 addrspace(1)* %.remat2, i32 0, i32 0, i8 addrspace(1)* %6)
  %7 = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token %statepoint_token8, i32 8, i32 8) ; (%6, %6)
  %.remat3 = bitcast i8 addrspace(1)* %7 to i32 addrspace(1)*
  %8 = load i32, i32 addrspace(1)* %.remat3
  %statepoint_token9 = call token (i64, i32, void (i32)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidi32f(i64 2882400000, i32 0, void (i32)* @pr, i32 1, i32 0, i32 %8, i32 0, i32 0)
  ret i32 0
}

declare i8 addrspace(1)* @alloc(i64) local_unnamed_addr #1

declare void @pr(i32)

declare void @prp(i32 addrspace(1)*)

declare void @enterGC(...) local_unnamed_addr #1

declare void @initGC()

declare void @__tmp_use(...)

declare token @llvm.experimental.gc.statepoint.p0f_isVoidf(i64, i32, void ()*, i32, i32, ...)

declare token @llvm.experimental.gc.statepoint.p0f_p1i8i64f(i64, i32, i8 addrspace(1)* (i64)*, i32, i32, ...)

; Function Attrs: nounwind readonly
declare i8 addrspace(1)* @llvm.experimental.gc.result.p1i8(token) #2

declare token @llvm.experimental.gc.statepoint.p0f_isVoidp1i32f(i64, i32, void (i32 addrspace(1)*)*, i32, i32, ...)

; Function Attrs: nounwind readonly
declare i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token, i32, i32) #2

declare token @llvm.experimental.gc.statepoint.p0f_isVoidi32f(i64, i32, void (i32)*, i32, i32, ...)

declare token @llvm.experimental.gc.statepoint.p0f_isVoidvarargf(i64, i32, void (...)*, i32, i32, ...)

attributes #0 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind readonly }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
!3 = !{!4, !4, i64 0}
!4 = !{!"int", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
