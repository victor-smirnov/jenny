// RUN: %clang_cc1 -freflection -std=c++2a %s

template<int x>
class nested_foo {
  consteval -> fragment struct {
    int get() {
      return x;
    }
  };
};

class foo {
  consteval {
    int i = 0;
    -> fragment struct {
      int unqualid("get_value_", %{i})() {
        return nested_foo<%{i}> { }.get();
      }
    };
  };
};

int main() {
  foo f;
  return f.get_value_0();
}
