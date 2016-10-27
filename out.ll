; ModuleID = 'ConstantC codegen'

@0 = private unnamed_addr constant [5 x i8] c"vaer\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %calltmp = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i32 0, i32 0))
  ret i32 %calltmp
}
