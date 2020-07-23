
#include <clang/AST/Type.h>
#include <clang/AST/Expr.h>
#include <clang/AST/TypeLoc.h>
#include <clang/AST/ASTContext.h>

namespace clang {


JennyTypeOfExprType::JennyTypeOfExprType(Expr *E, QualType resolvedType0, QualType can)
    : Type(JennyTypeOfExpr, can,
             toTypeDependence(E->getDependence()) |
                 (E->getType()->getDependence() &
                  TypeDependence::VariablyModified)),
        TOExpr(E), resolvedType(resolvedType0)
{}

DependentJennyTypeOfExprType::DependentJennyTypeOfExprType(const ASTContext &Context, Expr *E)
    : JennyTypeOfExprType(E, Context.DependentTy), Context(Context) {}

bool JennyTypeOfExprType::isSugared() const {
  return !TOExpr->isTypeDependent();
}

QualType JennyTypeOfExprType::desugar() const {
  if (isSugared())
    return getUnderlyingExpr()->getType();

  return QualType(this, 0);
}

void DependentJennyTypeOfExprType::Profile(llvm::FoldingSetNodeID &ID,
                                      const ASTContext &Context, Expr *E) {
  E->Profile(ID, Context, true);
}


// Reimplemented to account for Jenny/C++ extension
//     typeof unary-expression
// where there are no parentheses.
SourceRange JennyTypeOfExprTypeLoc::getLocalSourceRange() const {
  if (getRParenLoc().isValid())
    return SourceRange(getTypeofLoc(), getRParenLoc());
  else
    return SourceRange(getTypeofLoc(),
                       getUnderlyingExpr()->getSourceRange().getEnd());
}


/// getJennyTypeOfExprType - Unlike many "get<Type>" functions, we can't unique
/// JennyTypeOfExprType AST's (since expression's are never shared). For example,
/// multiple declarations that refer to "jy_typeof(x)" all contain different
/// DeclRefExpr's. This doesn't effect the type checker, since it operates
/// on canonical type's (which are always unique).
QualType ASTContext::getJennyTypeOfExprType(Expr *tofExpr) const {
  JennyTypeOfExprType *toe;
  if (tofExpr->isTypeDependent()) {
    llvm::FoldingSetNodeID ID;
    DependentJennyTypeOfExprType::Profile(ID, *this, tofExpr);

    void *InsertPos = nullptr;
    DependentJennyTypeOfExprType *Canon
      = DependentJennyTypeOfExprTypes.FindNodeOrInsertPos(ID, InsertPos);
    if (Canon) {
      // We already have a "canonical" version of an identical, dependent
      // typeof(expr) type. Use that as our canonical type.
      toe = new (*this, TypeAlignment) JennyTypeOfExprType(tofExpr,
                                          QualType((JennyTypeOfExprType*)Canon, 0));
    } else {
      // Build a new, canonical typeof(expr) type.
      Canon
        = new (*this, TypeAlignment) DependentJennyTypeOfExprType(*this, tofExpr);
      DependentJennyTypeOfExprTypes.InsertNode(Canon, InsertPos);
      toe = Canon;
    }
  } else {
    //QualType Canonical = getCanonicalType(tofExpr->getType());
    toe = new (*this, TypeAlignment) JennyTypeOfExprType(tofExpr, this->DependentTy, this->DependentTy);
  }
  Types.push_back(toe);
  return QualType(toe, 0);
}


}
