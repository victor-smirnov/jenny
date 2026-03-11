// RUN: %clang_cc1 -fsyntax-only -verify %s

// Test automatic wrapping of red function calls from green functions

[[clang::red]] void red_function(int x);
[[clang::green]] void green_function();

[[clang::green]] void test_auto_wrap() {
    // This should be automatically wrapped in __builtin_red_call
    red_function(42);  // expected-no-error
    
    // Explicit __builtin_red_call should still work
    __builtin_red_call(red_function, 42);  // expected-no-error
}

// Test that green -> green calls are not wrapped
[[clang::green]] void green_callee() {}
[[clang::green]] void test_green_to_green() {
    green_callee();  // expected-no-error
}

// Test that red -> red calls are not wrapped
[[clang::red]] void red_callee() {}
void test_red_to_red() {
    red_callee();  // expected-no-error
}

// Test that red -> green calls are still forbidden
[[clang::green]] void green_callee2() {}
void test_red_to_green() {
    green_callee2();  // expected-error {{cannot call green function from red context}}
}

