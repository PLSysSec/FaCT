; ModuleID = 'sort.c'
source_filename = "sort.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.smack_value = type { i8* }

; Function Attrs: nounwind uwtable
define i32 @sort2(i32*, i32*) #0 {
  %3 = alloca i32*, align 8
  %4 = alloca i32*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32* %0, i32** %3, align 8
  store i32* %1, i32** %4, align 8
  %7 = load i32*, i32** %4, align 8
  %8 = getelementptr inbounds i32, i32* %7, i64 0
  %9 = load i32, i32* %8, align 4
  store i32 %9, i32* %5, align 4
  %10 = load i32*, i32** %4, align 8
  %11 = getelementptr inbounds i32, i32* %10, i64 1
  %12 = load i32, i32* %11, align 4
  store i32 %12, i32* %6, align 4
  %13 = load i32, i32* %5, align 4
  %14 = load i32, i32* %6, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %16, label %27

; <label>:16:                                     ; preds = %2
  %17 = load i32*, i32** %4, align 8
  %18 = getelementptr inbounds i32, i32* %17, i64 0
  %19 = load i32, i32* %18, align 4
  %20 = load i32*, i32** %3, align 8
  %21 = getelementptr inbounds i32, i32* %20, i64 0
  store i32 %19, i32* %21, align 4
  %22 = load i32*, i32** %4, align 8
  %23 = getelementptr inbounds i32, i32* %22, i64 1
  %24 = load i32, i32* %23, align 4
  %25 = load i32*, i32** %3, align 8
  %26 = getelementptr inbounds i32, i32* %25, i64 1
  store i32 %24, i32* %26, align 4
  br label %38

; <label>:27:                                     ; preds = %2
  %28 = load i32*, i32** %4, align 8
  %29 = getelementptr inbounds i32, i32* %28, i64 1
  %30 = load i32, i32* %29, align 4
  %31 = load i32*, i32** %3, align 8
  %32 = getelementptr inbounds i32, i32* %31, i64 0
  store i32 %30, i32* %32, align 4
  %33 = load i32*, i32** %4, align 8
  %34 = getelementptr inbounds i32, i32* %33, i64 0
  %35 = load i32, i32* %34, align 4
  %36 = load i32*, i32** %3, align 8
  %37 = getelementptr inbounds i32, i32* %36, i64 1
  store i32 %35, i32* %37, align 4
  br label %38

; <label>:38:                                     ; preds = %27, %16
  %39 = load i32, i32* %5, align 4
  %40 = load i32, i32* %6, align 4
  %41 = icmp slt i32 %39, %40
  %42 = zext i1 %41 to i32
  ret i32 %42
}

; Function Attrs: nounwind uwtable
define void @sort3(i32*, i32*, i32*) #0 {
  %4 = alloca i32*, align 8
  %5 = alloca i32*, align 8
  %6 = alloca i32*, align 8
  store i32* %0, i32** %4, align 8
  store i32* %1, i32** %5, align 8
  store i32* %2, i32** %6, align 8
  %7 = load i32*, i32** %5, align 8
  %8 = load i32*, i32** %6, align 8
  %9 = call i32 @sort2(i32* %7, i32* %8)
  %10 = load i32*, i32** %4, align 8
  %11 = getelementptr inbounds i32, i32* %10, i64 0
  store i32 %9, i32* %11, align 4
  %12 = load i32*, i32** %5, align 8
  %13 = getelementptr inbounds i32, i32* %12, i64 1
  %14 = load i32, i32* %13, align 4
  %15 = load i32*, i32** %6, align 8
  %16 = getelementptr inbounds i32, i32* %15, i64 1
  store i32 %14, i32* %16, align 4
  %17 = load i32*, i32** %5, align 8
  %18 = getelementptr inbounds i32, i32* %17, i64 1
  %19 = load i32*, i32** %6, align 8
  %20 = getelementptr inbounds i32, i32* %19, i64 1
  %21 = call i32 @sort2(i32* %18, i32* %20)
  %22 = load i32*, i32** %4, align 8
  %23 = getelementptr inbounds i32, i32* %22, i64 1
  store i32 %21, i32* %23, align 4
  %24 = load i32*, i32** %5, align 8
  %25 = getelementptr inbounds i32, i32* %24, i64 0
  %26 = load i32, i32* %25, align 4
  %27 = load i32*, i32** %6, align 8
  %28 = getelementptr inbounds i32, i32* %27, i64 0
  store i32 %26, i32* %28, align 4
  %29 = load i32*, i32** %5, align 8
  %30 = getelementptr inbounds i32, i32* %29, i64 1
  %31 = load i32, i32* %30, align 4
  %32 = load i32*, i32** %6, align 8
  %33 = getelementptr inbounds i32, i32* %32, i64 1
  store i32 %31, i32* %33, align 4
  %34 = load i32*, i32** %5, align 8
  %35 = load i32*, i32** %6, align 8
  %36 = call i32 @sort2(i32* %34, i32* %35)
  %37 = load i32*, i32** %4, align 8
  %38 = getelementptr inbounds i32, i32* %37, i64 2
  store i32 %36, i32* %38, align 4
  ret void
}

; Function Attrs: nounwind uwtable
define i32* @sort3_wrapper(i32*, i32*, i32*) #0 {
  %4 = alloca i32*, align 8
  %5 = alloca i32*, align 8
  %6 = alloca i32*, align 8
  store i32* %0, i32** %4, align 8
  store i32* %1, i32** %5, align 8
  store i32* %2, i32** %6, align 8
  %7 = load i32*, i32** %4, align 8
  %8 = getelementptr inbounds i32, i32* %7, i64 12
  %9 = load i32*, i32** %5, align 8
  %10 = icmp ult i32* %8, %9
  br i1 %10, label %16, label %11

; <label>:11:                                     ; preds = %3
  %12 = load i32*, i32** %5, align 8
  %13 = getelementptr inbounds i32, i32* %12, i64 12
  %14 = load i32*, i32** %4, align 8
  %15 = icmp ult i32* %13, %14
  br label %16

; <label>:16:                                     ; preds = %11, %3
  %17 = phi i1 [ true, %3 ], [ %15, %11 ]
  %18 = zext i1 %17 to i32
  call void @__VERIFIER_assume(i32 %18)
  %19 = load i32*, i32** %4, align 8
  %20 = getelementptr inbounds i32, i32* %19, i64 12
  %21 = load i32*, i32** %6, align 8
  %22 = icmp ult i32* %20, %21
  br i1 %22, label %28, label %23

; <label>:23:                                     ; preds = %16
  %24 = load i32*, i32** %6, align 8
  %25 = getelementptr inbounds i32, i32* %24, i64 12
  %26 = load i32*, i32** %4, align 8
  %27 = icmp ult i32* %25, %26
  br label %28

; <label>:28:                                     ; preds = %23, %16
  %29 = phi i1 [ true, %16 ], [ %27, %23 ]
  %30 = zext i1 %29 to i32
  call void @__VERIFIER_assume(i32 %30)
  %31 = load i32*, i32** %5, align 8
  %32 = getelementptr inbounds i32, i32* %31, i64 12
  %33 = load i32*, i32** %6, align 8
  %34 = icmp ult i32* %32, %33
  br i1 %34, label %40, label %35

; <label>:35:                                     ; preds = %28
  %36 = load i32*, i32** %6, align 8
  %37 = getelementptr inbounds i32, i32* %36, i64 12
  %38 = load i32*, i32** %5, align 8
  %39 = icmp ult i32* %37, %38
  br label %40

; <label>:40:                                     ; preds = %35, %28
  %41 = phi i1 [ true, %28 ], [ %39, %35 ]
  %42 = zext i1 %41 to i32
  call void @__VERIFIER_assume(i32 %42)
  %43 = load i32*, i32** %4, align 8
  %44 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %43)
  call void @public_in(%struct.smack_value* %44)
  %45 = load i32*, i32** %5, align 8
  %46 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %45)
  call void @public_in(%struct.smack_value* %46)
  %47 = load i32*, i32** %6, align 8
  %48 = call %struct.smack_value* (i32*, ...) bitcast (%struct.smack_value* (...)* @__SMACK_value to %struct.smack_value* (i32*, ...)*)(i32* %47)
  call void @public_in(%struct.smack_value* %48)
  %49 = load i32*, i32** %4, align 8
  %50 = bitcast i32* %49 to i8*
  %51 = call %struct.smack_value* @__SMACK_values(i8* %50, i32 3)
  call void @declassified_out(%struct.smack_value* %51)
  %52 = load i32*, i32** %4, align 8
  %53 = bitcast i32* %52 to i8*
  %54 = call %struct.smack_value* @__SMACK_values(i8* %53, i32 3)
  call void @public_in(%struct.smack_value* %54)
  %55 = load i32*, i32** %4, align 8
  %56 = bitcast i32* %55 to i8*
  %57 = call %struct.smack_value* @__SMACK_values(i8* %56, i32 3)
  call void @public_out(%struct.smack_value* %57)
  %58 = call %struct.smack_value* @__SMACK_return_value()
  call void @public_out(%struct.smack_value* %58)
  %59 = load i32*, i32** %4, align 8
  %60 = load i32*, i32** %5, align 8
  %61 = load i32*, i32** %6, align 8
  call void @sort3(i32* %59, i32* %60, i32* %61)
  %62 = load i32*, i32** %4, align 8
  ret i32* %62
}

declare void @__VERIFIER_assume(i32) #1

declare void @public_in(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_value(...) #1

declare void @declassified_out(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_values(i8*, i32) #1

declare void @public_out(%struct.smack_value*) #1

declare %struct.smack_value* @__SMACK_return_value() #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.9.1-4ubuntu3~16.04.2 (tags/RELEASE_391/rc2)"}
