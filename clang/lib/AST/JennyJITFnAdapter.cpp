//===-- JennyJITFnAdapter.cpp - Jenny JIT Adapter for metacalls -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/APValue.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecordLayout.h"
#include "clang/Basic/TargetInfo.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"

#include "Interp/State.h"

#include "JennyJITFnAdapter.h"


#include <memory>
#include <cstdint>
#include <cstring>

namespace clang {

using namespace llvm;

namespace {


class ValueBuffer {
  char* Bytes;
  CharUnits Size;
public:
  ValueBuffer(): Bytes(), Size(CharUnits::Zero()) {}
  ValueBuffer(char* Bytes, CharUnits Size): Bytes(Bytes), Size(Size) {}

  LLVM_NODISCARD
  bool readObject(CharUnits Offset, CharUnits Width,
                  SmallVectorImpl<unsigned char> &Output) const {
    for (CharUnits I = Offset, E = Offset + Width; I != E; ++I) {
      Output.push_back(Bytes[I.getQuantity()]);
    }
    return true;
  }

  void writeObject(CharUnits Offset, SmallVectorImpl<unsigned char> &Input) {
    size_t Index = 0;
    for (unsigned char Byte : Input) {
      assert(!Bytes[Offset.getQuantity() + Index] && "overwriting a byte?");
      Bytes[Offset.getQuantity() + Index] = Byte;
      ++Index;
    }
  }

  size_t size() const { return Size.getQuantity(); }

  char* data() {
    return Bytes;
  }

  const char* data() const {
    return Bytes;
  }
};

/// Traverse an APValue to produce an BitCastBuffer, emulating how the current
/// target would represent the value at runtime.
class APValueToMemBufferConverter final {
  Expr::EvalContext& Ctx;
  interp::State& Info;
  jenny::MetaCallAllocator& Allocator;

  ValueBuffer Buffer;
  SourceLocation SLoc;

private:
  APValueToMemBufferConverter(Expr::EvalContext& Ctx,
                              interp::State& state0,
                              jenny::MetaCallAllocator& Allocator,
                              SourceLocation SLoc)
    : Ctx(Ctx),
      Info(state0),
      Allocator(Allocator),
      SLoc(SLoc)
  {}

public:
  static Optional<ValueBuffer> convert(Expr::EvalContext& Ctx,
                                       interp::State& Info,
                                       jenny::MetaCallAllocator& Allocator,
                                       const APValue& value,
                                       QualType type,
                                       SourceLocation SLoc) {
    APValueToMemBufferConverter cvt(Ctx, Info, Allocator, SLoc);

    cvt.allocateBuffer(type);

    if (cvt.visit(value, type, CharUnits::fromQuantity(0))) {
      return cvt.Buffer;
    }

    return None;
  }

private:

  bool visit(const APValue &Val, QualType Ty) {
    return visit(Val, Ty, CharUnits::fromQuantity(0));
  }

  Optional<ValueBuffer> convert(const APValue& value, QualType type) {
    return convert(value, type, SLoc);
  }

  Optional<ValueBuffer> convert(const APValue& value, QualType type, SourceLocation SLoc) {
    return convert(Ctx, Info, Allocator, value, type, SLoc);
  }


  bool isBufferAllocated() const {
    return Buffer.size() > 0;
  }

  void allocateBuffer(CharUnits Size, CharUnits Alignment) {
    Buffer = ValueBuffer(
      (char*)Allocator.Allocate(Size.getQuantity(), Align(Alignment.getQuantity())),
      Size
    );
    std::memset(Buffer.data(), 0, Buffer.size());
  }

  void allocateBuffer(QualType type) {
    CharUnits DstSize   = Ctx.ASTCtx.getTypeSizeInChars(type);
    CharUnits Alignment = Ctx.ASTCtx.getTypeAlignInChars(type);
    allocateBuffer(DstSize, Alignment);
  }

  void allocateArrayBuffer(QualType type) {
    CharUnits DstSize   = Ctx.ASTCtx.getTypeSizeInChars(type);
    CharUnits Alignment = Ctx.ASTCtx.getTypeAlignInChars(type);
    allocateBuffer(DstSize, Alignment);
  }



  // Write out Val with type Ty into Buffer starting at Offset.
  bool visit(const APValue &Val, QualType Ty, CharUnits Offset) {
    assert((size_t)Offset.getQuantity() <= Buffer.size());

    // As a special case, nullptr_t has an indeterminate value.
    if (Ty->isNullPtrType())
      return true;

    // Dig through Src to find the byte at SrcOffset.
    switch (Val.getKind()) {
    case APValue::Indeterminate:
    case APValue::None:
      return true;

    case APValue::Int:
      return visitInt(Val.getInt(), Ty, Offset);
    case APValue::Float:
      return visitFloat(Val.getFloat(), Ty, Offset);
    case APValue::Array:
      return visitArray(Val, Ty, Offset);
    case APValue::Struct:
      return visitRecord(Val, Ty, Offset);

    case APValue::ComplexInt:
    case APValue::ComplexFloat:
    case APValue::Vector:
    case APValue::FixedPoint:
      // FIXME: We should support these.

    case APValue::Reflection:
    case APValue::Fragment:
    case APValue::Union:
    case APValue::MemberPointer:
    case APValue::AddrLabelDiff: {
      Info.FFDiag(SLoc,
                  diag::note_constexpr_bit_cast_unsupported_type)
          << Ty;
      return false;
    }

    case APValue::LValue:
      if (const Expr* expr = Val.getLValueBase().dyn_cast<const Expr*>()) {
        if (const StringLiteral* str = dyn_cast_or_null<const StringLiteral>(expr)) {
          return visitStringLiteral(str, Offset);
        }
      }
      else if (const ValueDecl* vdecl = Val.getLValueBase().dyn_cast<const ValueDecl*>()) {
        return visitValueDecl(vdecl, Offset, Ty);
      }
    }

    Info.FFDiag(SLoc,
                diag::note_metacall_unsupported_type)
        << Ty;

    return false;
  }

  bool visitValueDecl(const ValueDecl* vdecl, CharUnits Offset, QualType type) {
    if (const VarDecl* varDecl = dyn_cast_or_null<const VarDecl>(vdecl)) {
      if (const Expr* init = varDecl->getInit()) {
        Expr::EvalResult Result;
        if (init->EvaluateAsRValue(Result, Ctx, true)) {
          if (Optional<ValueBuffer> buf = convert(Result.Val, init->getType(), init->getBeginLoc())) {
            uint64_t ptrIntValue = (uint64_t) buf.getValue().data();
            APSInt ival = APSInt::getUnsigned(ptrIntValue);
            return visitInt(ival, Ctx.ASTCtx.UnsignedLongLongTy, Offset);
          }
        }
      }
    }


    Info.FFDiag(SLoc,
                diag::note_metacall_unsupported_type)
        << type;

    return false;
  }

  bool visitStringLiteral(const StringLiteral* str, CharUnits Offset)
  {
    StringRef ss = str->getString();
    char* ptr = allocateString(ss);
    uint64_t ptr_val = reinterpret_cast<uint64_t>(ptr);
    APSInt uval = APSInt::getUnsigned(ptr_val);
    return visitInt(uval, Ctx.ASTCtx.UnsignedLongLongTy, Offset);
  }

  bool visitRecord(const APValue &Val, QualType Ty, CharUnits Offset) {
    const RecordDecl *RD = Ty->getAsRecordDecl();
    const ASTRecordLayout &Layout = Ctx.ASTCtx.getASTRecordLayout(RD);

    // Visit the base classes.
    if (auto *CXXRD = dyn_cast<CXXRecordDecl>(RD)) {
      for (size_t I = 0, E = CXXRD->getNumBases(); I != E; ++I) {
        const CXXBaseSpecifier &BS = CXXRD->bases_begin()[I];
        CXXRecordDecl *BaseDecl = BS.getType()->getAsCXXRecordDecl();

        if (!visitRecord(Val.getStructBase(I), BS.getType(),
                         Layout.getBaseClassOffset(BaseDecl) + Offset))
          return false;
      }
    }

    // Visit the fields.
    unsigned FieldIdx = 0;
    for (FieldDecl *FD : RD->fields()) {
      if (FD->isBitField()) {
        Info.FFDiag(SLoc,
                    diag::note_constexpr_bit_cast_unsupported_bitfield);
        return false;
      }

      uint64_t FieldOffsetBits = Layout.getFieldOffset(FieldIdx);

      assert(FieldOffsetBits % Ctx.ASTCtx.getCharWidth() == 0 &&
             "only bit-fields can have sub-char alignment");
      CharUnits FieldOffset =
          Ctx.ASTCtx.toCharUnitsFromBits(FieldOffsetBits) + Offset;
      QualType FieldTy = FD->getType();
      if (!visit(Val.getStructField(FieldIdx), FieldTy, FieldOffset))
        return false;
      ++FieldIdx;
    }

    return true;
  }

  bool visitArray(const APValue &Val, QualType Ty, CharUnits Offset) {
    const auto *CAT =
        dyn_cast_or_null<ArrayType>(Ty->getAsArrayTypeUnsafe());
    if (!CAT) {
      Ty.dump();
      return false;
    }

    CharUnits ElemWidth = Ctx.ASTCtx.getTypeSizeInChars(CAT->getElementType());
    unsigned NumInitializedElts = Val.getArrayInitializedElts();
    unsigned ArraySize = Val.getArraySize();
    // First, initialize the initialized elements.
    for (unsigned I = 0; I != NumInitializedElts; ++I) {
      const APValue &SubObj = Val.getArrayInitializedElt(I);
      if (!visit(SubObj, CAT->getElementType(), Offset + I * ElemWidth))
        return false;
    }

    // Next, initialize the rest of the array using the filler.
    if (Val.hasArrayFiller()) {
      const APValue &Filler = Val.getArrayFiller();
      for (unsigned I = NumInitializedElts; I != ArraySize; ++I) {
        if (!visit(Filler, CAT->getElementType(), Offset + I * ElemWidth))
          return false;
      }
    }

    return true;
  }

  bool visitInt(const APSInt &Val, QualType Ty, CharUnits Offset) {
    CharUnits Width = Ctx.ASTCtx.getTypeSizeInChars(Ty);
    SmallVector<unsigned char, 8> Bytes(Width.getQuantity());
    llvm::StoreIntToMemory(Val, &*Bytes.begin(), Width.getQuantity());
    Buffer.writeObject(Offset, Bytes);
    return true;
  }

  bool visitFloat(const APFloat &Val, QualType Ty, CharUnits Offset) {
    APSInt AsInt(Val.bitcastToAPInt());
    return visitInt(AsInt, Ty, Offset);
  }

  char* allocateString(StringRef ss) {
    char* ptr = reinterpret_cast<char*>(Allocator.Allocate(ss.size() + 1, 1));
    ptr[ss.size()] = 0;
    std::memcpy(ptr, ss.data(), ss.size());
    return ptr;
  }
};

/// Write an BitCastBuffer into an APValue.
class MemBufferToAPValueConverter {
  ASTContext& ASTCtx;
  interp::State& Info;
  const ValueBuffer &Buffer;
  SourceLocation SLoc;

  MemBufferToAPValueConverter(ASTContext& ASTCtx0,
                              interp::State &Info,
                              const ValueBuffer &Buffer,
                              SourceLocation SLoc)
    : ASTCtx(ASTCtx0), Info(Info),
      Buffer(Buffer),
      SLoc(SLoc)
  {}


  // Emit an unsupported bit_cast type error. Sema refuses to build a bit_cast
  // with an invalid type, so anything left is a deficiency on our part (FIXME).
  // Ideally this will be unreachable.
  llvm::NoneType unsupportedType(QualType Ty) {
    Info.FFDiag(SLoc,
                diag::note_metacall_unsupported_type)
        << Ty;
    return None;
  }

  Optional<APValue> visit(const BuiltinType *T, CharUnits Offset,
                          const EnumType *EnumSugar = nullptr) {
    if (T->isNullPtrType()) {
      uint64_t NullValue = ASTCtx.getTargetNullPointerValue(QualType(T, 0));
      return APValue((Expr *)nullptr,
                     /*Offset=*/CharUnits::fromQuantity(NullValue),
                     APValue::NoLValuePath{}, /*IsNullPtr=*/true);
    }

    CharUnits SizeOf = ASTCtx.getTypeSizeInChars(T);
    SmallVector<uint8_t, 8> Bytes;
    if (!Buffer.readObject(Offset, SizeOf, Bytes)) {
      // If this is std::byte or unsigned char, then its okay to store an
      // indeterminate value.
      bool IsStdByte = EnumSugar && EnumSugar->isStdByteType();
      bool IsUChar =
          !EnumSugar && (T->isSpecificBuiltinType(BuiltinType::UChar) ||
                         T->isSpecificBuiltinType(BuiltinType::Char_U));
      if (!IsStdByte && !IsUChar) {
        QualType DisplayType(EnumSugar ? (const Type *)EnumSugar : T, 0);
        Info.FFDiag(SLoc,
                    diag::note_constexpr_bit_cast_indet_dest)
            << DisplayType << ASTCtx.getLangOpts().CharIsSigned;
        return None;
      }

      return APValue::IndeterminateValue();
    }

    APSInt Val(SizeOf.getQuantity() * ASTCtx.getCharWidth(), true);
    llvm::LoadIntFromMemory(Val, &*Bytes.begin(), Bytes.size());

    if (T->isIntegralOrEnumerationType()) {
      Val.setIsSigned(T->isSignedIntegerOrEnumerationType());
      return APValue(Val);
    }

    if (T->isRealFloatingType()) {
      const llvm::fltSemantics &Semantics =
          ASTCtx.getFloatTypeSemantics(QualType(T, 0));
      return APValue(APFloat(Semantics, Val));
    }

    return unsupportedType(QualType(T, 0));
  }

  Optional<APValue> visit(const RecordType *RTy, CharUnits Offset) {
    const RecordDecl *RD = RTy->getAsRecordDecl();
    const ASTRecordLayout &Layout = ASTCtx.getASTRecordLayout(RD);

    unsigned NumBases = 0;
    if (auto *CXXRD = dyn_cast<CXXRecordDecl>(RD))
      NumBases = CXXRD->getNumBases();

    APValue ResultVal(APValue::UninitStruct(), NumBases,
                      std::distance(RD->field_begin(), RD->field_end()));

    // Visit the base classes.
    if (auto *CXXRD = dyn_cast<CXXRecordDecl>(RD)) {
      for (size_t I = 0, E = CXXRD->getNumBases(); I != E; ++I) {
        const CXXBaseSpecifier &BS = CXXRD->bases_begin()[I];
        CXXRecordDecl *BaseDecl = BS.getType()->getAsCXXRecordDecl();
        if (BaseDecl->isEmpty() ||
            ASTCtx.getASTRecordLayout(BaseDecl).getNonVirtualSize().isZero())
          continue;

        Optional<APValue> SubObj = visitType(
              BS.getType(), Layout.getBaseClassOffset(BaseDecl) + Offset);
        if (!SubObj)
          return None;
        ResultVal.getStructBase(I) = *SubObj;
      }
    }

    // Visit the fields.
    unsigned FieldIdx = 0;
    for (FieldDecl *FD : RD->fields()) {
      // FIXME: We don't currently support bit-fields. A lot of the logic for
      // this is in CodeGen, so we need to factor it around.
      if (FD->isBitField()) {
        Info.FFDiag(SLoc,
                    diag::note_constexpr_bit_cast_unsupported_bitfield);
        return None;
      }

      uint64_t FieldOffsetBits = Layout.getFieldOffset(FieldIdx);
      assert(FieldOffsetBits % ASTCtx.getCharWidth() == 0);

      CharUnits FieldOffset =
          CharUnits::fromQuantity(FieldOffsetBits / ASTCtx.getCharWidth()) +
          Offset;
      QualType FieldTy = FD->getType();
      Optional<APValue> SubObj = visitType(FieldTy, FieldOffset);
      if (!SubObj)
        return None;
      ResultVal.getStructField(FieldIdx) = *SubObj;
      ++FieldIdx;
    }

    return ResultVal;
  }

  Optional<APValue> visit(const EnumType *Ty, CharUnits Offset) {
    QualType RepresentationType = Ty->getDecl()->getIntegerType();
    assert(!RepresentationType.isNull() &&
           "enum forward decl should be caught by Sema");
    const auto *AsBuiltin =
        RepresentationType.getCanonicalType()->castAs<BuiltinType>();
    // Recurse into the underlying type. Treat std::byte transparently as
    // unsigned char.
    return visit(AsBuiltin, Offset, /*EnumTy=*/Ty);
  }

  Optional<APValue> visit(const ConstantArrayType *Ty, CharUnits Offset) {
    size_t Size = Ty->getSize().getLimitedValue();
    CharUnits ElementWidth = ASTCtx.getTypeSizeInChars(Ty->getElementType());

    APValue ArrayValue(APValue::UninitArray(), Size, Size);
    for (size_t I = 0; I != Size; ++I) {
      Optional<APValue> ElementValue =
          visitType(Ty->getElementType(), Offset + I * ElementWidth);
      if (!ElementValue)
        return None;
      ArrayValue.getArrayInitializedElt(I) = std::move(*ElementValue);
    }

    return ArrayValue;
  }

  Optional<APValue> visit(const Type *Ty, CharUnits Offset) {
    return unsupportedType(QualType(Ty, 0));
  }

  Optional<APValue> visitType(QualType Ty, CharUnits Offset) {
    QualType Can = Ty.getCanonicalType();

    switch (Can->getTypeClass()) {
#define TYPE(Class, Base)                                                      \
    case Type::Class:                                                            \
  return visit(cast<Class##Type>(Can.getTypePtr()), Offset);
#define ABSTRACT_TYPE(Class, Base)
#define NON_CANONICAL_TYPE(Class, Base)                                        \
    case Type::Class:                                                            \
  llvm_unreachable("non-canonical type should be impossible!");
#define DEPENDENT_TYPE(Class, Base)                                            \
    case Type::Class:                                                            \
  llvm_unreachable(                                                          \
  "dependent types aren't supported in the constant evaluator!");
#define NON_CANONICAL_UNLESS_DEPENDENT(Class, Base)                            \
    case Type::Class:                                                            \
  llvm_unreachable("either dependent or not canonical!");
#include "clang/AST/TypeNodes.inc"
    }
    llvm_unreachable("Unhandled Type::TypeClass");
  }

public:
  // Pull out a full value of type DstType.
  static Optional<APValue> convert(ASTContext& ASTCtx, interp::State& Info, const ValueBuffer &Buffer,
                                   QualType type, SourceLocation SLoc) {
    MemBufferToAPValueConverter Converter(ASTCtx, Info, Buffer, SLoc);
    return Converter.visitType(type, CharUnits::fromQuantity(0));
  }
};


}


bool JennyMetaCallAdapterImpl::addParam(const APValue& value, QualType type) {
  Optional<ValueBuffer> buf = APValueToMemBufferConverter::convert(Ctx, Info, Allocator, value, type, SLoc);
  if (buf) {
    Params.push_back(MemTy{buf->data()});
    ParamTypes.push_back(type);
    return true;
  }

  return false;
}

Optional<APValue> JennyMetaCallAdapterImpl::getAPValueResult() const {
  if (ReturnType == Ctx.ASTCtx.VoidTy) {
    return APValue{};
  }
  else {
    ValueBuffer Buffer((char*)Result.Ptr, CharUnits::fromQuantity(data_size_));
    return MemBufferToAPValueConverter::convert(Ctx.ASTCtx, Info, Buffer, ReturnType, SLoc);
  }
}

}

namespace __jy {
JennyMetaCallAdapter::~JennyMetaCallAdapter() noexcept {}
}
