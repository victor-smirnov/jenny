//===- JennyCxxProcessor.cpp ----------------------------------------------===//
//
// Part of the Jenny Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Jenny/Common.h"

#include <iostream>

namespace jenny {
namespace reflect {

class ClassImpl: public Class {
    std::string name_;
public:
    ClassImpl(const std::string& name) noexcept :
        name_(name)
    {}

    virtual const char* name() const {
        return name_.c_str();
    }
};


class CompilerImpl: public Compiler {

    Class* make_class(const char* name)
    {
        return new ClassImpl(name);
    }

    void free(CompilerObject* obj) noexcept {
        delete obj;
    }

    void print(const char* message) noexcept {
        std::cout << message << std::endl;
    }
};


Compiler& compiler() noexcept{
    static CompilerImpl compiler_impl;
    return compiler_impl;
}

CompilerObject::~CompilerObject() noexcept {}
Compiler::~Compiler() noexcept {}

}}
