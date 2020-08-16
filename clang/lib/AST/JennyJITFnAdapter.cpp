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

#include "JennyJITFnAdapter.h"


#include <memory>
#include <cstdint>
#include <cstring>

namespace clang {

void JennyMetaCallAdapterImpl::addParam(const APValue& value) {
    if (value.isInt()) {
        unsigned bits = value.getInt().getBitWidth();
        if (bits <= 64)
        {
            bool isSigned = value.getInt().isSigned();

            if (isSigned) {
                auto intValue = value.getInt().getSExtValue();

                switch (bits) {
                    case 8:  addTypedParam((signed char)intValue); ParamTypes.push_back(Ctx.CharTy); break;
                    case 16: addTypedParam((short)intValue); ParamTypes.push_back(Ctx.ShortTy); break;
                    case 32: addTypedParam((int)intValue); ParamTypes.push_back(Ctx.IntTy); break;
                    case 64: addTypedParam((long long int)intValue); ParamTypes.push_back(Ctx.LongLongTy); break;
                    default: llvm_unreachable("unsupported bit width");
                }
            }
            else {
                auto intValue = value.getInt().getZExtValue();

                switch (bits) {
                    case 8:  addTypedParam((unsigned char)intValue); ParamTypes.push_back(Ctx.UnsignedCharTy); break;
                    case 16: addTypedParam((unsigned short)intValue); ParamTypes.push_back(Ctx.UnsignedShortTy); break;
                    case 32: addTypedParam((unsigned int)intValue); ParamTypes.push_back(Ctx.UnsignedIntTy); break;
                    case 64: addTypedParam((unsigned long long int)intValue); ParamTypes.push_back(Ctx.UnsignedLongLongTy); break;
                    default: llvm_unreachable("unsupported bit width");
                }
            }
        }
        else {
            llvm_unreachable("Long integer is not supported");
        }
    }
    else if (value.isFloat()) {
        const llvm::APFloat& fltValue = value.getFloat();
        unsigned bits = fltValue.getSizeInBits(fltValue.getSemantics());

        if (bits == 32){
            addTypedParam(fltValue.convertToFloat());
            ParamTypes.push_back(Ctx.FloatTy);
        }
        else if (bits == 64){
            addTypedParam(fltValue.convertToDouble());
            ParamTypes.push_back(Ctx.DoubleTy);
        }
        else {
            llvm_unreachable("Long float is not supported");
        }
    }
    else if (value.isLValue())
    {
        APValue::LValueBase lvb = value.getLValueBase();
        if (const Expr* expr = lvb.dyn_cast<const Expr*>()) {
            if (const StringLiteral* str = dyn_cast<StringLiteral>(expr)) {
                StringRef ss = str->getString();
                addParam(const_cast<char*>(ss.data()), 1, ss.size());
                ParamTypes.push_back(Ctx.getPointerType(Ctx.CharTy));
            }
            else {
                llvm_unreachable("Unsupported LValue expression type");
            }
        }
        else {
            llvm_unreachable("Unsupported LValue");
        }
    }
}

APValue JennyMetaCallAdapterImpl::getAPValueResult() const
{
    if (ReturnType == Ctx.VoidTy) {
        return APValue{};
    }
    else if (ReturnType == Ctx.IntTy) {
        int value = *(int*)Result.Ptr;
        return APValue{llvm::APSInt(llvm::APInt(32, value), false)};
    }
    else if (ReturnType == Ctx.LongTy) {
        long value = *(long*)Result.Ptr;
        return APValue{llvm::APSInt(llvm::APInt(sizeof(long) * 8, value), false)};
    }
    else if (ReturnType == Ctx.LongLongTy) {
        long long value = *(long long*)Result.Ptr;
        return APValue{llvm::APSInt(llvm::APInt(sizeof(long long) * 8, value), false)};
    }
    else if (ReturnType == Ctx.UnsignedLongLongTy) {
        unsigned long long value = *(unsigned long long*)Result.Ptr;
        return APValue{llvm::APSInt(llvm::APInt(sizeof(unsigned long long) * 8, value), true)};
    }
    else if (ReturnType == Ctx.BoolTy) {
        bool value = *(bool*)Result.Ptr;
        return APValue{llvm::APSInt(llvm::APInt(sizeof(bool) * 8, value), true)};
    }
    else if (ReturnType == Ctx.FloatTy) {
        float value = *(float*)Result.Ptr;
        return APValue{llvm::APFloat(value)};
    }
    else if (ReturnType == Ctx.DoubleTy) {
        double value = *(double*)Result.Ptr;
        return APValue{llvm::APFloat(value)};
    }

    llvm::errs() << "ReturnType: \n";
    ReturnType.print(llvm::errs(), Ctx.getPrintingPolicy(), Twine(), 2);
    llvm::errs() << "\n";
    llvm::errs().flush();
    llvm_unreachable("Unsupported return type for metacall");
}

}

namespace __jy {
JennyMetaCallAdapter::~JennyMetaCallAdapter() noexcept {}
}
