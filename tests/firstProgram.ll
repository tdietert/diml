; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc double @f([2 x double] %z) #0 {
entry:
  %0 = tail call fastcc double @lambda(double 1.100000e+01, [2 x double] %z)
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @lambda(double %x, [2 x double] %z) #0 {
entry:
  %0 = fadd double %x, 2.000000e+00
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @fib(double %x1, [2 x double] %z) #0 {
entry:
  %0 = fcmp ult double %x1, 2.000000e+00
  br i1 %0, label %if.exit, label %if.else

if.else:                                          ; preds = %entry
  %1 = fadd double %x1, -1.000000e+00
  %2 = tail call fastcc double @fib(double %1, [2 x double] %z)
  %3 = fadd double %x1, -2.000000e+00
  %4 = tail call fastcc double @fib(double %3, [2 x double] %z)
  %5 = fadd double %2, %4
  ret double %5

if.exit:                                          ; preds = %entry
  ret double 1.000000e+00
}

define double @main() {
entry:
  %0 = tail call fastcc double @fib(double 1.000000e+01, [2 x double] [double 1.000000e+00, double 2.000000e+00])
  %1 = tail call fastcc double @f([2 x double] [double 1.000000e+00, double 2.000000e+00])
  %2 = fadd double %0, %1
  %3 = fptosi double %2 to i64
  tail call void @printInt(i64 %3)
  ret double 1.000000e+00
}

declare i32 @printf(i8* noalias nocapture, ...)

define double @fst([2 x double] %tuple) {
entry:
  %0 = extractvalue [2 x double] %tuple, 0
  ret double %0
}

define double @snd([2 x double] %tuple) {
entry:
  %0 = extractvalue [2 x double] %tuple, 1
  ret double %0
}

define void @printInt(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
