// RUN: %clang_cc1 -fsyntax-only -verify %s

[[clang::green]] void green_func();
[[clang::red]] void red_func_explicit();

void red_func() {
  green_func(); // expected-error {{function with 'green' attribute cannot be called from a non-green function}}
}

[[clang::green]] void green_caller() {
  green_func();
}

namespace [[clang::green]] GreenNS {
  void implicit_green() {}
  
  [[clang::red]] void explicit_red() {
    implicit_green(); // expected-error {{function with 'green' attribute cannot be called from a non-green function}}
  }
}

void red_caller_ns() {
  GreenNS::implicit_green(); // expected-error {{function with 'green' attribute cannot be called from a non-green function}}
}

[[clang::green]] void green_caller_ns() {
  GreenNS::implicit_green();
}

struct [[clang::green]] GreenStruct {
  void implicit_green_method() {}
  
  [[clang::red]] void explicit_red_method() {
    implicit_green_method(); // expected-error {{function with 'green' attribute cannot be called from a non-green function}}
  }
};

void red_caller_struct() {
  GreenStruct s;
  s.implicit_green_method(); // expected-error {{function with 'green' attribute cannot be called from a non-green function}}
  s.explicit_red_method(); // OK
}
