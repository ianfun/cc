; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%Foo = type { i32, i32 }

@Z = global %Foo zeroinitializer

define i32 @main() {
  %1 = load %Foo, ptr @Z, align 4
  %2 = extractvalue %Foo %1, 0
  ret i32 %2
}
