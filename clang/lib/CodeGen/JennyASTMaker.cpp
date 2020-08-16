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

#include "clang/AST/Decl.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"

#include "JennyASTMaker.h"

using namespace clang;

BinaryOperator *JennyASTMaker::makeAssignment(const Expr *LHS, const Expr *RHS,
                                         QualType Ty) {
  return BinaryOperator::Create(
      C, const_cast<Expr *>(LHS), const_cast<Expr *>(RHS), BO_Assign, Ty,
      VK_RValue, OK_Ordinary, SourceLocation(), FPOptionsOverride());
}

BinaryOperator *JennyASTMaker::makeComparison(const Expr *LHS, const Expr *RHS,
                                         BinaryOperator::Opcode Op) {
  assert(BinaryOperator::isLogicalOp(Op) ||
         BinaryOperator::isComparisonOp(Op));
  return BinaryOperator::Create(
      C, const_cast<Expr *>(LHS), const_cast<Expr *>(RHS), Op,
      C.getLogicalOperationType(), VK_RValue, OK_Ordinary, SourceLocation(),
      FPOptionsOverride());
}

CompoundStmt *JennyASTMaker::makeCompound(ArrayRef<Stmt *> Stmts) {
  return CompoundStmt::Create(C, Stmts, SourceLocation(), SourceLocation());
}

DeclRefExpr *JennyASTMaker::makeDeclRefExpr(
    const VarDecl *D,
    bool RefersToEnclosingVariableOrCapture)
{
  QualType Type = D->getType().getNonReferenceType();

  DeclRefExpr *DR = DeclRefExpr::Create(
      C, NestedNameSpecifierLoc(), SourceLocation(), const_cast<VarDecl *>(D),
      RefersToEnclosingVariableOrCapture, SourceLocation(), Type, VK_LValue);
  return DR;
}

UnaryOperator *JennyASTMaker::makeDereference(const Expr *Arg, QualType Ty) {
  return UnaryOperator::Create(C, const_cast<Expr *>(Arg), UO_Deref, Ty,
                               VK_LValue, OK_Ordinary, SourceLocation(),
                               /*CanOverflow*/ false, FPOptionsOverride());
}

ImplicitCastExpr *JennyASTMaker::makeLvalueToRvalue(const Expr *Arg, QualType Ty) {
  return makeImplicitCast(Arg, Ty, CK_LValueToRValue);
}

ImplicitCastExpr *
JennyASTMaker::makeLvalueToRvalue(const VarDecl *Arg,
                             bool RefersToEnclosingVariableOrCapture) {
  QualType Type = Arg->getType().getNonReferenceType();
  return makeLvalueToRvalue(makeDeclRefExpr(Arg,
                                            RefersToEnclosingVariableOrCapture),
                            Type);
}

ImplicitCastExpr *JennyASTMaker::makeImplicitCast(const Expr *Arg, QualType Ty,
                                             CastKind CK) {
  return ImplicitCastExpr::Create(C, Ty,
                                  /* CastKind=*/ CK,
                                  /* Expr=*/ const_cast<Expr *>(Arg),
                                  /* CXXCastPath=*/ nullptr,
                                  /* ExprValueKind=*/ VK_RValue);
}

Expr *JennyASTMaker::makeIntegralCast(const Expr *Arg, QualType Ty) {
  if (Arg->getType() == Ty)
    return const_cast<Expr*>(Arg);

  return ImplicitCastExpr::Create(C, Ty, CK_IntegralCast,
                                  const_cast<Expr*>(Arg), nullptr, VK_RValue);
}

ImplicitCastExpr *JennyASTMaker::makeIntegralCastToBoolean(const Expr *Arg) {
  return ImplicitCastExpr::Create(C, C.BoolTy, CK_IntegralToBoolean,
                                  const_cast<Expr*>(Arg), nullptr, VK_RValue);
}



ReturnStmt *JennyASTMaker::makeReturn(const Expr *RetVal) {
  return ReturnStmt::Create(C, SourceLocation(), const_cast<Expr *>(RetVal),
                            /* NRVOCandidate=*/nullptr);
}

IntegerLiteral *JennyASTMaker::makeIntegerLiteral(std::uint64_t Value, QualType Ty) {
  llvm::APInt APValue = llvm::APInt(C.getTypeSize(Ty), Value);
  return IntegerLiteral::Create(C, APValue, Ty, SourceLocation());
}

MemberExpr *JennyASTMaker::makeMemberExpression(Expr *base, ValueDecl *MemberDecl,
                                           bool IsArrow,
                                           ExprValueKind ValueKind) {

  DeclAccessPair FoundDecl = DeclAccessPair::make(MemberDecl, AS_public);
  return MemberExpr::Create(
      C, base, IsArrow, SourceLocation(), NestedNameSpecifierLoc(),
      SourceLocation(), MemberDecl, FoundDecl,
      DeclarationNameInfo(MemberDecl->getDeclName(), SourceLocation()),
      /* TemplateArgumentListInfo=*/ nullptr, MemberDecl->getType(), ValueKind,
      OK_Ordinary, NOUR_None);
}

ValueDecl *JennyASTMaker::findMemberField(const RecordDecl *RD, StringRef Name) {

  CXXBasePaths Paths(
      /* FindAmbiguities=*/false,
      /* RecordPaths=*/false,
      /* DetectVirtual=*/ false);
  const IdentifierInfo &II = C.Idents.get(Name);
  DeclarationName DeclName = C.DeclarationNames.getIdentifier(&II);

  DeclContextLookupResult Decls = RD->lookup(DeclName);
  for (NamedDecl *FoundDecl : Decls)
    if (!FoundDecl->getDeclContext()->isFunctionOrMethod())
      return cast<ValueDecl>(FoundDecl);

  return nullptr;
}

CXXMethodDecl *JennyASTMaker::findMemberMethod(const CXXRecordDecl *RD, StringRef Name) {

  CXXBasePaths Paths(
      /* FindAmbiguities=*/false,
      /* RecordPaths=*/false,
      /* DetectVirtual=*/ false);
  const IdentifierInfo &II = C.Idents.get(Name);
  DeclarationName DeclName = C.DeclarationNames.getIdentifier(&II);

  DeclContextLookupResult Decls = RD->lookup(DeclName);

  for (NamedDecl *FoundDecl : Decls) {
      return dyn_cast<CXXMethodDecl>(FoundDecl);
  }

  return nullptr;
}

CXXMemberCallExpr *JennyASTMaker::makeCXXMemberCall(Expr *Fn,
                                                    ArrayRef<Expr *> Args, QualType Ty,
                                                    ExprValueKind VK)
{
    CXXMemberCallExpr* memberCall = CXXMemberCallExpr::Create(
        C, Fn, Args, Ty, VK, SourceLocation{}, FPOptionsOverride()

    );
    return memberCall;
}

CStyleCastExpr *JennyASTMaker::makeCStyleCastExpr(QualType T,
                                                  ExprValueKind VK, CastKind K,
                                                  Expr *Op)
{
    return CStyleCastExpr::Create(C, T, VK, K, Op,
                                  nullptr, C.getTrivialTypeSourceInfo(T),
                                  SourceLocation{}, SourceLocation{});
}
