// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

[[clang::green]] void green_func() {}
// CHECK: define {{.*}}void @_Z10green_funcv() [[ATTR:#[0-9]+]]

namespace [[clang::green]] NS {
  void nested() {}
// CHECK: define {{.*}}void @_ZN2NS6nestedEv() [[ATTR]]

  [[clang::red]] void nested_red() {}
// CHECK: define {{.*}}void @_ZN2NS10nested_redEv() [[RED_ATTR:#[0-9]+]]
}

struct [[clang::green]] S {
  void method() {}
// CHECK: define {{.*}}void @_ZN1S6methodEv({{.*}}) [[ATTR]]

  [[clang::red]] void red_method() {}
// CHECK: define {{.*}}void @_ZN1S10red_methodEv({{.*}}) [[RED_ATTR]]
};

void red_func() {
  S s;
}
// CHECK: define {{.*}}void @_Z8red_funcv() [[RED_ATTR]]

// CHECK: attributes [[ATTR]] = { {{.*}}"split-stack"{{.*}} }
// CHECK-NOT: attributes [[RED_ATTR]] = { {{.*}}"split-stack"{{.*}} }
