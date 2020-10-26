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


#include "llvm/Support/Error.h"

#include "clang/AST/Type.h"
#include "clang/AST/APValue.h"
#include "clang/AST/Expr.h"

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
class PCHContainerReader;
class FrontendOptions;
class FileManager;

namespace interp {
class State;
}

struct MetacallArgsAdapter {
  virtual ~MetacallArgsAdapter() noexcept;
  virtual ::__jy::JennyMetaCallAdapter* call_adapter() noexcept = 0;

  virtual bool addParam(const APValue& value, QualType type) noexcept = 0;
  virtual ArrayRef<QualType> types() const              = 0;
  virtual const std::string* exception() const noexcept = 0;
  virtual Optional<APValue> getResult() const           = 0;
};

std::string strErrorAndConsume(llvm::Error&& error);

struct JennyJIT {
  virtual ~JennyJIT() noexcept;

  using AdapterFn = void (*) (::__jy::JennyMetaCallAdapter&);

  virtual Expected<FunctionDecl*> CreateAdapter(const CallExpr* call, llvm::ArrayRef<QualType> args) noexcept = 0;
  virtual Expected<std::string> compile(FunctionDecl* adapter) noexcept = 0;

  virtual Expected<void*> GetSymbol(llvm::StringRef name) noexcept = 0;

  virtual std::unique_ptr<MetacallArgsAdapter> CreateMetacallArgsAdapter(
      QualType returnType, QualType calleeReturnType,
      Expr::EvalContext& Ctx0, interp::State& Info,
      SourceLocation SLoc
  ) noexcept = 0;

  static Expected<std::shared_ptr<JennyJIT>> Create(
      Sema& S,
      ASTContext& Ctx,
      const PCHContainerReader& PCHCtrReader,
      const FrontendOptions& FEOptions,
      const std::vector<std::string>& jitLibs,
      DiagnosticsEngine& diagnostics,
      const HeaderSearchOptions& header_search_options,
      const PreprocessorOptions& preprocessor_options,
      const CodeGenOptions& codegen_options,
      const LangOptions& LangOpts
  );
};

std::string strErrorAndConsume(llvm::Error&& error);

}
