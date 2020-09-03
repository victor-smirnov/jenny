#include <meta/compiler.h>


#pragma once

extern "C" int printf(const char*, ...);

int metaFn(const char* arg, int v);

struct StrView;

struct Helper {
    const StrView* str;
};

struct StrView {
    const char* data;
    int size;

    struct {
	int a;
	int b;
	const char* c;
    } extra;

    constexpr StrView(): data(), size(), extra{} {}
    constexpr StrView(const char* dd, int ss): data(dd), size(ss), extra{1, 2, "Booo!!!"} {}
    constexpr StrView(const StrView& other) = default;
};

int metaFn(StrView view);

int metaFn(const char* view);

void metaFn(Helper helper);