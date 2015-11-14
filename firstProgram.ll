; ModuleID = 'dimlProgram'

@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define double @main() #0 {
entry:
  %0 = alloca <2 x double>
  ret <2 x double>* %0
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
