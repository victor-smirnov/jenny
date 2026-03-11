// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -emit-llvm -o - %s | FileCheck %s

// Test that red function calls from green functions are automatically wrapped

[[clang::red]] void red_function(int x);

[[clang::green]] void green_function() {
    // This should be automatically wrapped in __builtin_red_call
    red_function(42);
}

// CHECK-LABEL: define{{.*}} void @_Z14green_functionv()
// CHECK: call void @llvm.stacksave
// CHECK: call void @_Z12red_functioni(i32 42)
// CHECK: call void @llvm.stackrestore

