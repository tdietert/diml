; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

define double @main() {
entry:
  %0 = tail call double @fst([2 x double] [double 1.000000e+00, double 2.000000e+00])
  %1 = tail call double @snd([2 x double] [double 1.000000e+00, double 2.000000e+00])
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
