// RUN: %clang_cc1 -freflection -std=c++2a %s

#define assert(E) if (!(E)) __builtin_abort();

// test injection in a non-constexpr function
int f() {
  consteval -> fragment {
    return 9;
  };
}

int g() {
  constexpr auto frag1 = fragment {
    return 9;
  };

  consteval -> frag1;
}

constexpr auto frag = fragment {
};

template<typename T>
T h() {
  consteval -> fragment {
    return T();
  };
}

template<int x>
constexpr auto templ_frag = fragment {
  return x;
};

constexpr int temple_frag_inst(bool bigNum) {
  if (bigNum) {
    consteval -> templ_frag<10>;
  }
  consteval -> templ_frag<0>;
}

int main() {
  // MetaprogramDecl
  consteval {
    -> frag;
  }
  // InjectionDecl
  consteval -> frag;

  assert(f() == 9);
  assert(g() == 9);
  assert(h<int>() == int());
  assert(temple_frag_inst(true) > temple_frag_inst(false));
}
