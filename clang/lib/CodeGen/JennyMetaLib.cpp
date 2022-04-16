//===-- JennyMetaLib.cpp - Jenny Metaprogramming Support LIB -- -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "../Headers/jenny/meta/compiler.h"

#include "clang/AST/Type.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/APValue.h"
#include "clang/AST/PrettyPrinter.h"

#include <cstdlib>
#include <cstring>
#include <assert.h>

namespace jenny {

clang::QualType MetaInfo::qual_type() const {
  assert_type(TYPE);

  return ::clang::QualType::getFromOpaquePtr(payload_.type);
}

void MetaInfo::dump() const {
  switch (type_) {
  case Type::TYPE: qual_type().dump(); break;
  case Type::DECLARATION: decl()->dump(); break;
  case Type::EXPRESSION: expr()->dump(); break;
  case Type::BASE_SPECIFIER: base_specifier()->getType().dump(); break;
  default:
    llvm::errs() << "Unknown MetaInfo type: " << type_ << "\n";
  }
}

void MetaInfo::print() const {
  switch (type_) {
  //case Type::TYPE: qual_type().print(llvm::errs(), clang::PrintingPolicy{clang::LangOptions{}}); break;
  case Type::TYPE: qual_type().dump(); break;
  case Type::DECLARATION: decl()->print(llvm::errs()); break;
  case Type::EXPRESSION: expr()->printPretty(llvm::errs(), nullptr, clang::PrintingPolicy{clang::LangOptions{}}, 2); break;
  case Type::BASE_SPECIFIER: base_specifier()->getType().print(llvm::errs(), clang::PrintingPolicy{clang::LangOptions{}}); break;
  default:
    llvm::errs() << "Unknown MetaInfo type: " << type_ << "\n";
  }
}


void MetaInfo::assert_type(Type type) const {
  if (type != this->type_) {
    assert(type == this->type_ && "Unexpected MetaInfo type");
  }
}

}
