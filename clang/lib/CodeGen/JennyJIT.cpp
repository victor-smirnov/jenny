//===- JennyJIT.cpp - A simple JIT for Jenny --------------------*- C++ -*-===//
//
// Part of the LLVM/Jenny Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//============================================================================//

#include "clang/Jenny/JennyJIT.h"

#include <clang/AST/DeclGroup.h>
#include <clang/AST/Decl.h>

#include <clang/Sema/Sema.h>

#include "../Headers/compiler/reflect.h"

#include <memory>
#include <iostream>

using namespace llvm;
using namespace llvm::orc;
using namespace clang;

namespace clang {

Error processJennyMetaprograms(Sema&) {
    std::cout << "Inside Jenny processor!" << std::endl;
    return Error::success();
}

}
