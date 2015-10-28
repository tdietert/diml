; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc double @lambda(double %x, <2 x double> %z, double %y) #0 {
entry:
  %0 = fadd double %x, %y
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @s(double %y1, <2 x double> %z, double %y) #0 {
entry:
  ret double %y1
}

; Function Attrs: nounwind readnone
define internal fastcc double @fib(double %x1, <2 x double> %z, double %y) #0 {
entry:
  %0 = fcmp oeq double %x1, 0.000000e+00
  br i1 %0, label %if.exit, label %if.else

if.else:                                          ; preds = %entry
  %1 = fcmp ult double %x1, 3.000000e+00
  br i1 %1, label %if.exit, label %if.else1

if.exit:                                          ; preds = %if.else, %entry
  %2 = phi double [ 0.000000e+00, %entry ], [ 1.000000e+00, %if.else ]
  ret double %2

if.else1:                                         ; preds = %if.else
  %3 = fadd double %x1, -1.000000e+00
  %4 = tail call fastcc double @fib(double %3, <2 x double> %z, double %y)
  %5 = fadd double %x1, -2.000000e+00
  %6 = tail call fastcc double @fib(double %5, <2 x double> %z, double %y)
  %7 = fadd double %4, %6
  ret double %7
}

; Function Attrs: nounwind readnone
define double @main() #0 {
entry:
  %0 = tail call fastcc double @fib(double 5.000000e+00, <2 x double> <double 1.000000e+00, double 2.000000e+00>, double 6.000000e+00)
  %1 = tail call fastcc double @lambda(double 6.000000e+00, <2 x double> <double 1.000000e+00, double 2.000000e+00>, double 6.000000e+00)
  %2 = fadd double %0, %1
  %3 = tail call fastcc double @s(double 6.000000e+00, <2 x double> <double 1.000000e+00, double 2.000000e+00>, double 6.000000e+00)
  %4 = fadd double %2, %3
  ret double %4
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #1

; Function Attrs: nounwind
define void @printInt(i64 %this) #1 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
attributes #1 = { nounwind }
