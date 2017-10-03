; ModuleID = 'sort.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.smack_value = type { i8* }

; Function Attrs: nounwind uwtable
define i32 @sort2(i32* %out2, i32* %in2) #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i32*, align 8
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  store i32* %out2, i32** %1, align 8
  store i32* %in2, i32** %2, align 8
  %3 = load i32*, i32** %2, align 8
  %4 = getelementptr inbounds i32, i32* %3, i64 0
  %5 = load i32, i32* %4, align 4
  store i32 %5, i32* %a, align 4
  %6 = load i32*, i32** %2, align 8
  %7 = getelementptr inbounds i32, i32* %6, i64 1
  %8 = load i32, i32* %7, align 4
  store i32 %8, i32* %b, align 4
  %9 = load i32, i32* %a, align 4
  %10 = load i32, i32* %b, align 4
  %11 = icmp slt i32 %9, %10
  br i1 %11, label %12, label %23

; <label>:12                                      ; preds = %0
  %13 = load i32*, i32** %2, align 8
  %14 = getelementptr inbounds i32, i32* %13, i64 0
  %15 = load i32, i32* %14, align 4
  %16 = load i32*, i32** %1, align 8
  %17 = getelementptr inbounds i32, i32* %16, i64 0
  store i32 %15, i32* %17, align 4
  %18 = load i32*, i32** %2, align 8
  %19 = getelementptr inbounds i32, i32* %18, i64 1
  %20 = load i32, i32* %19, align 4
  %21 = load i32*, i32** %1, align 8
  %22 = getelementptr inbounds i32, i32* %21, i64 1
  store i32 %20, i32* %22, align 4
  br label %34

; <label>:23                                      ; preds = %0
  %24 = load i32*, i32** %2, align 8
  %25 = getelementptr inbounds i32, i32* %24, i64 1
  %26 = load i32, i32* %25, align 4
  %27 = load i32*, i32** %1, align 8
  %28 = getelementptr inbounds i32, i32* %27, i64 0
  store i32 %26, i32* %28, align 4
  %29 = load i32*, i32** %2, align 8
  %30 = getelementptr inbounds i32, i32* %29, i64 0
  %31 = load i32, i32* %30, align 4
  %32 = load i32*, i32** %1, align 8
  %33 = getelementptr inbounds i32, i32* %32, i64 1
  store i32 %31, i32* %33, align 4
  br label %34

; <label>:34                                      ; preds = %23, %12
  %35 = load i32, i32* %a, align 4
  %36 = load i32, i32* %b, align 4
  %37 = icmp slt i32 %35, %36
  %38 = zext i1 %37 to i32
  ret i32 %38
}

; Function Attrs: nounwind uwtable
define void @sort3(i32* %conds, i32* %out3, i32* %in3) #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i32*, align 8
  %3 = alloca i32*, align 8
  store i32* %conds, i32** %1, align 8
  store i32* %out3, i32** %2, align 8
  store i32* %in3, i32** %3, align 8
  %4 = load i32*, i32** %2, align 8
  %5 = load i32*, i32** %3, align 8
  %6 = call i32 @sort2(i32* %4, i32* %5)
  %7 = load i32*, i32** %1, align 8
  %8 = getelementptr inbounds i32, i32* %7, i64 0
  store i32 %6, i32* %8, align 4
  %9 = load i32*, i32** %2, align 8
  %10 = getelementptr inbounds i32, i32* %9, i64 1
  %11 = load i32, i32* %10, align 4
  %12 = load i32*, i32** %3, align 8
  %13 = getelementptr inbounds i32, i32* %12, i64 1
  store i32 %11, i32* %13, align 4
  %14 = load i32*, i32** %2, align 8
  %15 = getelementptr inbounds i32, i32* %14, i64 1
  %16 = load i32*, i32** %3, align 8
  %17 = getelementptr inbounds i32, i32* %16, i64 1
  %18 = call i32 @sort2(i32* %15, i32* %17)
  %19 = load i32*, i32** %1, align 8
  %20 = getelementptr inbounds i32, i32* %19, i64 1
  store i32 %18, i32* %20, align 4
  %21 = load i32*, i32** %2, align 8
  %22 = getelementptr inbounds i32, i32* %21, i64 0
  %23 = load i32, i32* %22, align 4
  %24 = load i32*, i32** %3, align 8
  %25 = getelementptr inbounds i32, i32* %24, i64 0
  store i32 %23, i32* %25, align 4
  %26 = load i32*, i32** %2, align 8
  %27 = getelementptr inbounds i32, i32* %26, i64 1
  %28 = load i32, i32* %27, align 4
  %29 = load i32*, i32** %3, align 8
  %30 = getelementptr inbounds i32, i32* %29, i64 1
  store i32 %28, i32* %30, align 4
  %31 = load i32*, i32** %2, align 8
  %32 = load i32*, i32** %3, align 8
  %33 = call i32 @sort2(i32* %31, i32* %32)
  %34 = load i32*, i32** %1, align 8
  %35 = getelementptr inbounds i32, i32* %34, i64 2
  store i32 %33, i32* %35, align 4
  ret void
}

; Function Attrs: nounwind uwtable
define i32* @sort3_wrapper(i32* %conds, i32* %out, i32* %in) #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i32*, align 8
  %3 = alloca i32*, align 8
  store i32* %conds, i32** %1, align 8
  store i32* %out, i32** %2, align 8
  store i32* %in, i32** %3, align 8
  %4 = load i32*, i32** %1, align 8
  %5 = getelementptr inbounds i32, i32* %4, i64 12
  %6 = load i32*, i32** %2, align 8
  %7 = icmp ult i32* %5, %6
  br i1 %7, label %13, label %8

; <label>:8                                       ; preds = %0
  %9 = load i32*, i32** %2, align 8
  %10 = getelementptr inbounds i32, i32* %9, i64 12
  %11 = load i32*, i32** %1, align 8
  %12 = icmp ult i32* %10, %11
  br label %13

; <label>:13                                      ; preds = %8, %0
  %14 = phi i1 [ true, %0 ], [ %12, %8 ]
  %15 = zext i1 %14 to i32
  call void @__VERIFIER_assume(i32 %15)
  %16 = load i32*, i32** %1, align 8
  %17 = getelementptr inbounds i32, i32* %16, i64 12
  %18 = load i32*, i32** %3, align 8
  %19 = icmp ult i32* %17, %18
  br i1 %19, label %25, label %20

; <label>:20                                      ; preds = %13
  %21 = load i32*, i32** %3, align 8
  %22 = getelementptr inbounds i32, i32* %21, i64 12
  %23 = load i32*, i32** %1, align 8
  %24 = icmp ult i32* %22, %23
  br label %25

; <label>:25                                      ; preds = %20, %13
  %26 = phi i1 [ true, %13 ], [ %24, %20 ]
  %27 = zext i1 %26 to i32
  call void @__VERIFIER_assume(i32 %27)
  %28 = load i32*, i32** %2, align 8
  %29 = getelementptr inbounds i32, i32* %28, i64 12
  %30 = load i32*, i32** %3, align 8
  %31 = icmp ult i32* %29, %30
  br i1 %31, label %37, label %32

; <label>:32                                      ; preds = %25
  %33 = load i32*, i32** %3, align 8
  %34 = getelementptr inbounds i32, i32* %33, i64 12
  %35 = load i32*, i32** %2, align 8
  %36 = icmp ult i32* %34, %35
  br label %37

; <label>:37                                      ; preds = %32, %25
  %38 = phi i1 [ true, %25 ], [ %36, %32 ]
  %39 = zext i1 %38 to i32
  call void @__VERIFIER_assume(i32 %39)
  %40 = load i32*, i32** %1, align 8
  %41 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %40)
  call void @public_in(%struct.smack_value* %41)
  %42 = load i32*, i32** %2, align 8
  %43 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %42)
  call void @public_in(%struct.smack_value* %43)
  %44 = load i32*, i32** %3, align 8
  %45 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %44)
  call void @public_in(%struct.smack_value* %45)
  %46 = load i32*, i32** %1, align 8
  %47 = bitcast i32* %46 to i8*
  %48 = call %struct.smack_value* @__SMACK_values(i8* %47, i32 3)
  call void @declassified_out(%struct.smack_value* %48)
  %49 = load i32*, i32** %1, align 8
  %50 = bitcast i32* %49 to i8*
  %51 = call %struct.smack_value* @__SMACK_values(i8* %50, i32 3)
  call void @public_in(%struct.smack_value* %51)
  %52 = load i32*, i32** %1, align 8
  %53 = bitcast i32* %52 to i8*
  %54 = call %struct.smack_value* @__SMACK_values(i8* %53, i32 3)
  call void @public_out(%struct.smack_value* %54)
  %55 = call %struct.smack_value* @__SMACK_return_value()
  call void @public_out(%struct.smack_value* %55)
  %56 = load i32*, i32** %1, align 8
  %57 = load i32*, i32** %2, align 8
  %58 = load i32*, i32** %3, align 8
  call void @sort3(i32* %56, i32* %57, i32* %58)
  %59 = load i32*, i32** %1, align 8
  ret i32* %59
}

declare void @__VERIFIER_assume(i32) #1

declare void @public_in(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_value(...) #1

declare void @declassified_out(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_values(i8*, i32) #1

declare void @public_out(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_return_value() #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}
