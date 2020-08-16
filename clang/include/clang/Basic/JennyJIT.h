//===--- JennyJIT.h - Jenny Metacall JIT API -------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#pragma once

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/ArrayRef.h"

#include "clang/AST/Type.h"

#include <string>
#include <memory>
#include <vector>

namespace __jy {
struct JennyMetaCallAdapter;
}

namespace clang {

class CallExpr;
class FunctionDecl;
class ASTContext;
class Sema;
class HeaderSearchOptions;
class PreprocessorOptions;
class CodeGenOptions;
class DiagnosticsEngine;

struct JennyJIT {
  virtual ~JennyJIT() noexcept;

  using AdapterFn = void (*) (::__jy::JennyMetaCallAdapter&);

  virtual FunctionDecl* CreateAdapter(const CallExpr* call, llvm::ArrayRef<QualType> args) noexcept = 0;
  virtual std::string compile(FunctionDecl* adapter) noexcept  = 0;

  virtual void* GetSymbol(llvm::StringRef name) noexcept = 0;

  static std::shared_ptr<JennyJIT> Create(
      ASTContext& Ctx,
      const std::vector<std::string>& jitLibs,
      DiagnosticsEngine& diagnostics,
      const HeaderSearchOptions& header_search_options,
      const PreprocessorOptions& preprocessor_options,
      const CodeGenOptions& codegen_options
  );
};

}
