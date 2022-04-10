#include "hw_metalib.h"

#include <stdio.h>

int main() {
    constexpr int iii = meta_call metaFn("HW!", 0);
    (void)meta_call printf("Result: %d\n", iii);
    return 0;
}
