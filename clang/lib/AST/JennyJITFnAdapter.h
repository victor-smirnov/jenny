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

#include "../Headers/__clang_jenny_metacall.h"

#include <tuple>

namespace clang {

class JennyMetaCallAdapterImpl: public __jy::JennyMetaCallAdapter {

  struct MemTy {
    void* Ptr;
    bool External;
    size_t element_size;
    size_t array_size;
  };

  using ParamsTy = llvm::SmallVector<MemTy, 6>;
  using ParamsTypesTy = llvm::SmallVector<QualType, 6>;

  ParamsTy Params;
  ParamsTypesTy ParamTypes;
  MemTy Result;

  llvm::BumpPtrAllocatorImpl<llvm::MallocAllocator, 512> Allocator;

  ASTContext& Ctx;
  QualType ReturnType;

public:

  struct ParamDescr {
    bool isConst;
  };

  JennyMetaCallAdapterImpl(size_t resultSize, size_t alignment, ASTContext& Ctx0):
    Result{nullptr, true, resultSize, 1}, Ctx(Ctx0)
  {
    if (resultSize > 0) {
      Result.Ptr = Allocator.Allocate(resultSize, llvm::Align(alignment));
      Result.External = false;
    }
  }

  JennyMetaCallAdapterImpl(QualType returnType, ASTContext& Ctx0):
    Result{nullptr, true, 0, 1}, Ctx(Ctx0), ReturnType(returnType)
  {
    bool isVoid = returnType.getCanonicalType() == Ctx.VoidTy;
    if (!isVoid)
    {
      Result.element_size = Ctx.getTypeSizeInChars(returnType).getQuantity();
      Result.Ptr = Allocator.Allocate(Result.element_size, llvm::Align(Ctx.getTypeAlignInChars(returnType).getQuantity()));
      Result.External = false;
    }
  }

  llvm::ArrayRef<QualType> types() const {
      return ParamTypes;
  }

  void addParam(void* mem, size_t element_size, size_t array_size = 1) {
    Params.push_back(MemTy{mem, true, element_size, array_size});
  }

  void* addParam(size_t element_size, size_t array_size = 1, size_t alignment = 0) {
    void* ptr = Allocator.Allocate(element_size, llvm::Align(alignment));
    Params.push_back(MemTy{ptr, false, element_size, array_size});
    return ptr;
  }

  template<typename T>
  void addTypedParam(const T& value) {
      *reinterpret_cast<T*>(addParam(sizeof(value), 1, alignof (T))) = value;
  }

  size_t params() const {return Params.size();}

  const void* param_const(int num) const noexcept {
    return Params[num].Ptr;
  }

  void* param(int num) const noexcept {
    return Params[num].Ptr;
  }

  void addParam(const APValue& value);
  APValue getAPValueResult() const;

  void result(const void* value) const noexcept {
    std::memcpy(Result.Ptr, value, Result.array_size * Result.element_size);
  }

  const void* getResult() const {
    return Result.Ptr;
  }
};

}
