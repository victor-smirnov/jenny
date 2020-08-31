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

#include "Interp/State.h"

#include "../Headers/__clang_jenny_metacall.h"

#include <tuple>
#include <unordered_map>

namespace clang {

namespace jenny {
using MetaCallAllocator = llvm::BumpPtrAllocatorImpl<llvm::MallocAllocator, 512>;
using MetaCallAllocationMap = std::unordered_map<void*, void*>;
}


class JennyMetaCallAdapterImpl final: public __jy::JennyMetaCallAdapter {

  struct MemTy {
    void* Ptr;
  };

  using ParamsTy = llvm::SmallVector<MemTy, 6>;
  using ParamsTypesTy = llvm::SmallVector<QualType, 6>;

  ParamsTy Params;
  ParamsTypesTy ParamTypes;
  MemTy Result;

  QualType ReturnType;
  size_t data_size_;


  jenny::MetaCallAllocator Allocator;
  jenny::MetaCallAllocationMap AllocationMap;

  Expr::EvalContext& Ctx;


  interp::State& Info;
  SourceLocation SLoc;

public:

  struct ParamDescr {
    bool isConst;
  };

  JennyMetaCallAdapterImpl(QualType returnType, Expr::EvalContext& Ctx0, interp::State& Info, SourceLocation SLoc):
    Result{nullptr}, ReturnType(returnType), data_size_(), Ctx(Ctx0), Info(Info), SLoc(SLoc)
  {
  }

  ArrayRef<QualType> types() const {
      return ParamTypes;
  }

  size_t params() const {return Params.size();}

  const void* param_const(int num) const noexcept override {
    return Params[num].Ptr;
  }

  void* param(int num) const noexcept override {
    return Params[num].Ptr;
  }

  bool addParam(const APValue& value, QualType type);
  Optional<APValue> getAPValueResult() const;

  void result(const void* value) noexcept override
  {
      data_size_ = Ctx.ASTCtx.getTypeSizeInChars(ReturnType).getQuantity();
      Result.Ptr = Allocator.Allocate(data_size_, llvm::Align(Ctx.ASTCtx.getTypeAlignInChars(ReturnType).getQuantity()));

      std::memcpy(Result.Ptr, value, data_size_);
  }

  const void* getResult() const {
    return Result.Ptr;
  }
};

}
