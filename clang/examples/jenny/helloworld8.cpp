#include "hw_metalib.h"

int main() {
  consteval {
    //char data[5] = {'a', 'b', 'c', 'd', 0};
    for (int i = 0; i < 3; i++) {
      int val = meta_call metaFn("Hello World", 0);
	    //int val = meta_call metaFn(data);
	    meta_call printf("Result: %d\n", val);

	    //StrView view("Hello world!", 0);
	    //Helper hh{&view};

	    //meta_call metaFn(hh);
    }
  }

  return 0;
}
