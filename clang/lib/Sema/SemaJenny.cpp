//===----- SemaJenny.cpp - Semantic Analysis for Reflection ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for Jenny C++ extensions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/ParsedReflection.h"
#include "clang/Sema/ParserLookupSetup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaInternal.h"
#include "TypeLocBuilder.h"

#include <unordered_map>

using namespace clang;
using namespace sema;

namespace {

QualType MapMetacallType(ASTContext& Ctx, QualType srcType) noexcept {

  std::string rtnTypeName = srcType.getCanonicalType().getAsString();

  if (rtnTypeName.find("basic_string<char, struct std::char_traits<char>, class std::allocator<char>") != std::string::npos) {
    QualType ctt = Ctx.getPointerType(Ctx.getConstType(Ctx.CharTy));
    return ctt;
  }
  else if (rtnTypeName == "class jenny::PCxxDecl") {
    return Ctx.MetaInfoTy;
  }

  return srcType;
}

}

ExprResult Sema::ActOnJennyMetaCallExpr(
    SourceLocation KWLoc, CallExpr *E,
    SourceLocation StartLoc,
    SourceLocation EndLoc)
{
    JennyMetaCallExpr* CE = JennyMetaCallExpr::Create(
                Context,
                MapMetacallType(Context, E->getType()),
                KWLoc,
                E,
                StartLoc, EndLoc);
    return CE;
}
