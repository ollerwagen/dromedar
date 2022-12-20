; ModuleID = 'test.c'
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() {
  %1 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %2 = call i32 @f()
  ret i32 %2
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @f() {
  ret i32 3
}