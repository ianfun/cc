; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@foo = internal global i32 0

define i32 @main() {
entry:
  %load = load i32, ptr @foo, align 4
  ret i32 %load
}
