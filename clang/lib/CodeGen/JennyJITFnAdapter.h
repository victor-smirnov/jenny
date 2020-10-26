//===---- JennyJITFnAdapter.h - Jenny JIT Adapter for metacalls -*- C++ -*-===//
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

#include "clang/AST/APValue.h"
#include "clang/AST/Type.h"
#include "clang/AST/ASTContext.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"

#include "../AST/Interp/State.h"

#include "../Headers/__clang_jenny_metacall.h"
#include "../Headers/jenny/meta/compiler.h"

#include "clang/Basic/JennyJIT.h"

#include <tuple>
#include <unordered_map>

namespace clang {


class FileManager;
class SourceManager;
class DiagnosticsEngine;
class Preprocessor;
class NamedDecl;

namespace jenny {
using MetaCallAllocator = llvm::BumpPtrAllocatorImpl<llvm::MallocAllocator, 512>;
using MetaCallAllocationMap = std::unordered_map<void*, void*>;
}

class JennyMetaCallAdapterImpl final: public MetacallArgsAdapter, public __jy::JennyMetaCallAdapter {

  struct MemTy {
    void* Ptr;
  };

  using ParamsTy = llvm::SmallVector<MemTy, 6>;
  using ParamsTypesTy = llvm::SmallVector<QualType, 6>;

  ParamsTy Params;
  ParamsTypesTy ParamTypes;

  QualType ReturnType;
  QualType CalleeReturnType;

  Optional<APValue> Result;

  jenny::MetaCallAllocator Allocator;
  jenny::MetaCallAllocationMap AllocationMap;

  Expr::EvalContext& Ctx;


  interp::State& Info;
  SourceLocation SLoc;

  std::string* Exception;

public:

  struct ParamDescr {
    bool isConst;
  };

  JennyMetaCallAdapterImpl(
      QualType returnType, QualType calleeReturnType,
      Expr::EvalContext& Ctx0, interp::State& Info,
      SourceLocation SLoc
  ) noexcept :
    ReturnType(returnType), CalleeReturnType(calleeReturnType), Result(),
    Ctx(Ctx0), Info(Info), SLoc(SLoc), Exception(nullptr)
  {
  }

  virtual ~JennyMetaCallAdapterImpl() noexcept {
    if (Exception) delete Exception;
  }

  ::__jy::JennyMetaCallAdapter* call_adapter() noexcept override {
    return this;
  }

  const std::string* exception() const noexcept override {
    return Exception;
  }

  ArrayRef<QualType> types() const override {
      return ParamTypes;
  }

  size_t params() const {return Params.size();}

  const void* param_const(int num) const noexcept override {
    return Params[num].Ptr;
  }

  void* param(int num) const noexcept override {
    return Params[num].Ptr;
  }

  bool addParam(const APValue& value, QualType type) noexcept override;

  Optional<APValue> getResult() const override {
    return Result;
  }

  void result(void* value) noexcept override;

  void except(::jenny::MetaExceptionBase& exception) noexcept override;
  void except_unknown() noexcept override;
};


struct CxxDeclImplBase: public ::jenny::ICxxDecl {
  virtual const NamedDecl* namedDecl() const noexcept = 0;
  virtual ASTContext& context() noexcept              = 0;
  virtual Preprocessor& preprocessor() noexcept       = 0;
  virtual SourceManager& sourceManager() noexcept     = 0;
  virtual FileManager& fileManager() noexcept         = 0;
  virtual DiagnosticsEngine& diag() noexcept          = 0;
};



}
