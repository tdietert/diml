; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc double @nest() #0 {
entry:
  %0 = tail call fastcc double @nested(double 1.000000e+00)
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @nested(double %x1) #0 {
entry:
  %0 = fadd double %x1, 1.000000e+00
  ret double %0
}

define double @main() {
entry:
  %0 = tail call fastcc double @nest()
  %1 = fptosi double %0 to i64
  tail call void @printInt(i64 %1)
  ret double 1.000000e+00
}

declare i32 @printf(i8* noalias nocapture, ...)

define void @printInt(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
