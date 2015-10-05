; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc double @lambda(double %y) #0 {
entry:
  %0 = fadd double %y, 5.000000e+00
  ret double %0
}

; Function Attrs: nounwind readnone
define internal fastcc double @fib(double %x, double %y) #0 {
entry:
  %0 = fcmp oeq double %x, 0.000000e+00
  br i1 %0, label %if.exit, label %if.else

if.else:                                          ; preds = %entry
  %1 = fcmp ult double %x, 3.000000e+00
  br i1 %1, label %if.exit, label %if.else1

if.exit:                                          ; preds = %if.else, %entry
  %2 = phi double [ 0.000000e+00, %entry ], [ 1.000000e+00, %if.else ]
  ret double %2

if.else1:                                         ; preds = %if.else
  %3 = fadd double %x, -1.000000e+00
  %4 = tail call fastcc double @fib(double %3, double %y)
  %5 = fadd double %x, -2.000000e+00
  %6 = tail call fastcc double @fib(double %5, double %y)
  %7 = fadd double %4, %6
  ret double %7
}

; Function Attrs: nounwind
define double @main() #1 {
entry:
  %0 = tail call fastcc double @fib(double 5.000000e+00, double 6.000000e+00)
  %1 = tail call fastcc double @lambda(double 6.000000e+00)
  %2 = fadd double %0, %1
  %3 = fptosi double %2 to i64
  tail call void @print.tinteger(i64 %3)
  ret double 1.000000e+00
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #1

; Function Attrs: nounwind
define void @print.tinteger(i64 %this) #1 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
attributes #1 = { nounwind }
