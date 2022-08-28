; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@0 = private constant [13 x i8] c"Hello world!\00"
@1 = private constant [4 x i8] c"ls \00"
@2 = private constant [13 x i8] c"good bye JIT\00"

declare i32 @puts(ptr)

declare i32 @system(ptr)

define i32 @main(i32 %0, ptr %1) {
  %3 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %4 = alloca ptr, align 8
  store ptr %1, ptr %4, align 8
  %5 = call i32 @puts(ptr @0)
  %6 = call i32 @system(ptr @1)
  %7 = call i32 @puts(ptr @2)
  ret i32 0
}
