; ModuleID = 'dimlProgram'

; Function Attrs: nounwind readnone
define double @lambda(double %x, double %y) #0 {
entry:
  %0 = fadd double %x, %y
  ret double %0
}

; Function Attrs: nounwind readnone
define double @fib(double %x, double %y) #0 {
entry:
  %0 = fcmp ult double %x, 2.000000e+00
  br i1 %0, label %if.exit, label %if.else

if.else:                                          ; preds = %entry
  %1 = fadd double %x, -1.000000e+00
  %2 = tail call double @fib(double %1, double %y)
  %3 = fadd double %x, -2.000000e+00
  %4 = tail call double @fib(double %3, double %y)
  %5 = fadd double %2, %4
  ret double %5

if.exit:                                          ; preds = %entry
  ret double 1.000000e+00
}

; Function Attrs: nounwind readnone
define double @main() #0 {
entry:
  %0 = tail call double @fib(double 6.000000e+00, double 6.000000e+00)
  %1 = tail call double @lambda(double 5.000000e+00, double 6.000000e+00)
  %2 = fadd double %0, %1
  ret double %2
}

attributes #0 = { nounwind readnone }
