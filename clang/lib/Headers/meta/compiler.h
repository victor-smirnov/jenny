/*===---- meta/compiler.h - Jenny Metaprogramming Support ------------------===
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===-----------------------------------------------------------------------===
 */

#pragma once

#include "../__clang_jenny_metacall.h"
#include "cstr.h"

#include <cstdlib>
#include <cstring>
#include <string>
#include <memory>

namespace jenny {

class MetaException: public MetaExceptionBase {
  char* reason_;
public:
  MetaException(const char* reason) noexcept {
    size_t len = std::strlen(reason);
    reason_ = new char[len + 1];
    std::memcpy(reason_, reason, len + 1);
  }

  ~MetaException() noexcept {
    delete[] reason_;
  }

  const char* reason() const noexcept {return reason_;}
};




struct Compiler {
  virtual ~Compiler() noexcept {}
};

Compiler& compiler() noexcept;



}
