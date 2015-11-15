declare i32 @printf(i8* noalias nocapture, ...)

@.fmti = private unnamed_addr constant [3 x i8] c "%d\00"

define void @printInt(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64  %this)
  ret void
}
