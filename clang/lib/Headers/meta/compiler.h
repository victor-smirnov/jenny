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

struct ICxxDecl {
  virtual ~ICxxDecl() noexcept {}

  virtual std::string name() const noexcept = 0;
  virtual std::string to_string() const noexcept = 0;
  virtual std::string to_ast_string() const noexcept = 0;

  virtual void pretty_print() const noexcept = 0;
  virtual void ast_dump() const noexcept = 0;
};


class PCxxDecl {
  ICxxDecl* decl_;
public:
  PCxxDecl(): decl_() {}
  ~PCxxDecl() noexcept;

  PCxxDecl(ICxxDecl* decl): decl_(decl) {}

  PCxxDecl(const PCxxDecl&) = delete;
  PCxxDecl& operator=(const PCxxDecl&) = delete;

  PCxxDecl(PCxxDecl&&) noexcept;
  PCxxDecl& operator=(PCxxDecl&&) noexcept;

  bool operator==(const PCxxDecl& other) noexcept {
    return decl_ == other.decl_;
  }

  ICxxDecl& operator*() noexcept {return *decl_;}
  ICxxDecl& operator*() const noexcept {return *decl_;}

  ICxxDecl* operator->() noexcept {return decl_;}
  ICxxDecl* operator->() const noexcept {return decl_;}

  ICxxDecl* get() noexcept {return decl_;}
  ICxxDecl* get() const noexcept {return decl_;}

  ICxxDecl* release() noexcept {
    ICxxDecl* tmp = decl_;
    decl_ = nullptr;
    return tmp;
  }

  void reset();
};

void swap(PCxxDecl& one, PCxxDecl& two) noexcept;

PCxxDecl parse_cxx_top_level_decl(const char* decl_name, const char* code);

struct Compiler {
  virtual ~Compiler() noexcept {}
};

Compiler& compiler() noexcept;



}
