#include "hw_metalib.h"

#include <stdio.h>


int metaFn(const char* arg, int v) {
    printf("Metafuntion1: %s :: %d\n", arg, v);
    return 12345 + v;
}


int metaFn(StrView view) {
    printf("Metafuntion2: %s :: %d ::: %s\n", view.data, view.size, view.extra.c);
    return 54321 + view.size;
}


int metaFn(const char* arg) {
    printf("Metafuntion3: %s\n", arg);
    return 345;
}


void metaFn(Helper helper) {
    printf("Metafuntion4: %s\n", helper.str->data);
}