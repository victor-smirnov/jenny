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


#include "../Headers/__clang_jenny_metacall.h"
#include "../Headers/meta/compiler.h"


#include <cstdlib>
#include <cstring>

namespace jenny {

Compiler& compiler() noexcept {
  static thread_local Compiler cc;
  return cc;
}

//const char* MetaException::reason() const noexcept {return reason_;}

}
