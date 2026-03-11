// RUN: %clang_cc1 -triple x86_64-unknown-unknown -emit-llvm -disable-llvm-passes %s -o - | FileCheck %s

extern "C" [[clang::red]] void red_target(int);

[[clang::green]] void invoke_red() {
  __builtin_red_call(red_target, 7);
}

// CHECK: @__green_fiber_system_stack = external thread_local global ptr

// CHECK-LABEL: define {{.*}}void @_Z10invoke_redv()
// CHECK: %[[SAVE:.*]] = call ptr @llvm.stacksave
// CHECK: %[[SYS:.*]] = load ptr, ptr @__green_fiber_system_stack
// CHECK: call void @llvm.stackrestore{{.*}}(ptr %[[SYS]])
// CHECK: call void @red_target(i32 {{.*}}7)
// CHECK: call void @llvm.stackrestore{{.*}}(ptr %[[SAVE]])

