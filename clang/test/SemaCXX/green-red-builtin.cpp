// RUN: %clang_cc1 -fsyntax-only -verify %s

[[clang::red]] void red_noargs();
[[clang::red]] void red_one(int);

[[clang::green]] void ok_calls() {
  __builtin_red_call(red_noargs);
  __builtin_red_call(red_one, 42);
}

[[clang::green]] void too_few_args() {
  __builtin_red_call(red_one); // expected-error {{too few arguments}}
}

[[clang::green]] void too_many_args() {
  __builtin_red_call(red_noargs, 1); // expected-error {{too many arguments}}
}

[[clang::green]] void not_a_function() {
  int value = 0;
  __builtin_red_call(value); // expected-error {{called object type 'int' is not a function or function pointer}}
}

[[clang::green]] void no_arguments() {
  __builtin_red_call(); // expected-error {{too few arguments}}
}

