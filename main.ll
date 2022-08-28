; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

module asm "mov $10, %eax"

; Function Attrs: nounwind
define i32 @main() local_unnamed_addr #0 {
  tail call void asm sideeffect "mov $$10, %eax", ""() #0
  ret i32 0
}

attributes #0 = { nounwind }
