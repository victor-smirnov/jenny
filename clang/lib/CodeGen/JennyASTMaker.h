//===--- JennyASTMaker.cpp - Jenny Metacall JIT Implementation --*- C++ -*-===//
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

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"


#include <cstdint>

namespace clang {

class JennyASTMaker {
public:
  JennyASTMaker(ASTContext &C) : C(C) {}

  /// Create a new BinaryOperator representing a simple assignment.
  BinaryOperator *makeAssignment(const Expr *LHS, const Expr *RHS, QualType Ty);

  /// Create a new BinaryOperator representing a comparison.
  BinaryOperator *makeComparison(const Expr *LHS, const Expr *RHS,
                                 BinaryOperator::Opcode Op);

  /// Create a new compound stmt using the provided statements.
  CompoundStmt *makeCompound(ArrayRef<Stmt*>);

  /// Create a new DeclRefExpr for the referenced variable.
  DeclRefExpr *makeDeclRefExpr(const VarDecl *D,
                               bool RefersToEnclosingVariableOrCapture = false);

  /// Create a new UnaryOperator representing a dereference.
  UnaryOperator *makeDereference(const Expr *Arg, QualType Ty);

  /// Create an implicit cast for an integer conversion.
  Expr *makeIntegralCast(const Expr *Arg, QualType Ty);

  /// Create an implicit cast to a builtin boolean type.
  ImplicitCastExpr *makeIntegralCastToBoolean(const Expr *Arg);

  /// Create an implicit cast for lvalue-to-rvaluate conversions.
  ImplicitCastExpr *makeLvalueToRvalue(const Expr *Arg, QualType Ty);

  /// Make RValue out of variable declaration, creating a temporary
  /// DeclRefExpr in the process.
  ImplicitCastExpr *
  makeLvalueToRvalue(const VarDecl *Decl,
                     bool RefersToEnclosingVariableOrCapture = false);

  /// Create an implicit cast of the given type.
  ImplicitCastExpr *makeImplicitCast(const Expr *Arg, QualType Ty,
                                     CastKind CK = CK_LValueToRValue);


  /// Create a Return statement.
  ReturnStmt *makeReturn(const Expr *RetVal);

  /// Create an integer literal expression of the given type.
  IntegerLiteral *makeIntegerLiteral(std::uint64_t Value, QualType Ty);

  /// Create a member expression.
  MemberExpr *makeMemberExpression(Expr *base, ValueDecl *MemberDecl,
                                   bool IsArrow = false,
                                   ExprValueKind ValueKind = VK_LValue);

  /// Returns a *first* member field of a record declaration with a given name.
  /// \return an nullptr if no member with such a name exists.
  ValueDecl *findMemberField(const RecordDecl *RD, StringRef Name);

  /// Returns a *first* member field of a record declaration with a given name.
  /// \return an nullptr if no member with such a name exists.
  CXXMethodDecl *findMemberMethod(const CXXRecordDecl *RD, StringRef Name);

  CXXMemberCallExpr *makeCXXMemberCall(Expr *Fn,
                                       ArrayRef<Expr *> Args, QualType Ty,
                                       ExprValueKind VK);

  CStyleCastExpr *makeCStyleCastExpr(QualType T,
                                     ExprValueKind VK, CastKind K,
                                     Expr *Op);

private:
  ASTContext &C;
};

}
