//===- JennyProcessor.h - Jenny interface for Clang Sema layer --*- C++ -*-===//
//
// Part of the LLVM/Jenny Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//============================================================================//

#pragma once

#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"

#include "clang/Sema/SemaConsumer.h"

#include <functional>
#include <memory>

namespace clang {

class Sema;
class CompilerInstance;

struct JennyProcessor: SemaConsumer {
    virtual ~JennyProcessor() noexcept;
    static std::unique_ptr<JennyProcessor> create(CompilerInstance&, std::unique_ptr<ASTConsumer> target);
};



}

