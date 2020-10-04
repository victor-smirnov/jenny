/*===---- __clang_jenny_metacall.h - Jenny metacall runtime support --------===
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===-----------------------------------------------------------------------===
 */

/**
 * __jy_meta_call operator support types. You don't need to include this header
 * directly.
 */

#ifndef __CLANG_JENNY_METACALL_H
#define __CLANG_JENNY_METACALL_H

namespace clang {
class Decl;
class Expr;
class CXXBaseSpecifier;
class CXXFragment;
class ASTContext;
class QualType;
}

namespace jenny {

struct MetaExceptionBase {
  virtual ~MetaExceptionBase() noexcept {};
  virtual const char* reason() const noexcept = 0;
};



class MetaInfo {
public:
  enum Type {EXPRESSION, DECLARATION, BASE_SPECIFIER, TYPE, FRAGMENT};
private:
  clang::ASTContext& ctx_;

  union Payload {
    const clang::Expr* expr;
    const clang::Decl* decl;
    const clang::CXXBaseSpecifier* baseSpecifier;
    const void* type;
    const clang::CXXFragment* cxx_fragment;

    Payload(const clang::Expr* expr): expr(expr) {}
    Payload(const clang::Decl* decl): decl(decl) {}
    Payload(const clang::CXXBaseSpecifier* baseSpecifier): baseSpecifier(baseSpecifier) {}
    Payload(const void* type): type(type) {}
    Payload(const clang::CXXFragment* cxx_fragment): cxx_fragment(cxx_fragment) {}
  };

  Payload payload_;
  Type type_;

public:
  MetaInfo(clang::ASTContext& ctx, Payload payload, Type type):
    ctx_(ctx), payload_(payload), type_(type)
  {}

  Type type() const {
    return type_;
  }

  const clang::Decl* decl() const {
    assert_type(Type::DECLARATION);
    return payload_.decl;
  }

  const clang::Expr* expr() const {
    assert_type(Type::EXPRESSION);
    return payload_.expr;
  }

  const clang::CXXBaseSpecifier* base_specifier() const {
    assert_type(Type::BASE_SPECIFIER);
    return payload_.baseSpecifier;
  }

  const clang::CXXFragment* cxx_fragment() const {
    assert_type(Type::FRAGMENT);
    return payload_.cxx_fragment;
  }

  clang::QualType qual_type() const;

  clang::ASTContext& ctx() {return ctx_;}
  const clang::ASTContext& ctx() const {return ctx_;}

  void dump() const;
  void print() const;

private:
  void assert_type(Type type) const;
};

}

namespace __jy {

struct JennyMetaCallAdapter {
  virtual ~JennyMetaCallAdapter() noexcept;

  virtual const void* param_const(int num) const noexcept = 0;
  virtual void* param(int num) const noexcept = 0;

  virtual void result(void* value) noexcept = 0;
  virtual void except(jenny::MetaExceptionBase& exception) noexcept = 0;
  virtual void except_unknown() noexcept = 0;
};

}


#endif //__CLANG_JENNY_METACALL_H
