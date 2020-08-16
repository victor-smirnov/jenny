//===--- Parser.cpp - C Language Family Parser ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements parsing for reflection facilities.
//
//===----------------------------------------------------------------------===//

#include "clang/Parse/Parser.h"
#include "clang/AST/ASTContext.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/ParsedReflection.h"

using namespace clang;

/// Parse a Jemmy meta call expression.
///
/// \verbatim
///       jenny-meta-call:
///         '__jy_meta_call' '(' call-expr ')'
///         '__jy_meta_call' call-expr
///
/// \endverbatim
ExprResult Parser::ParseJennyMetaCallExpression() {

    assert(Tok.is(tok::kw___jy_meta_call) && "expected '__jy_meta_call'");
    SourceLocation StartLoc = ConsumeToken();

    BalancedDelimiterTracker T(*this, tok::l_paren);
    const bool hasParens = Tok.is(tok::l_paren);
    if (hasParens) {
        T.expectAndConsume(diag::err_expected_lparen_after, "__jy_meta_call");
    }

    EnterExpressionEvaluationContext ConstantEvaluated(
        Actions, Sema::ExpressionEvaluationContext::ConstantEvaluated,
        Sema::ReuseLambdaContextDecl);

    ExprResult Operand = Actions.CorrectDelayedTyposInExpr(
                ParseCastExpression(CastParseKind::UnaryExprOnly));

    if (Operand.isInvalid()) {
        return ExprError();
    }

    SourceLocation sLoc;
    SourceLocation eLoc{};

    if (hasParens) {
        if (T.consumeClose())
            return ExprError();

        sLoc = T.getOpenLocation();
        eLoc = T.getCloseLocation();
    }
    else {
        sLoc = StartLoc;
    }

    if (CallExpr* callExpr = dyn_cast_or_null<CallExpr>(Operand.get())) {
        return Actions.ActOnJennyMetaCallExpr(StartLoc, callExpr, sLoc, eLoc);
    }
    else {
        Diag(sLoc, diag::err_expected_call_expression) << tok::kw___jy_meta_call;
        return ExprError();
    }
}
