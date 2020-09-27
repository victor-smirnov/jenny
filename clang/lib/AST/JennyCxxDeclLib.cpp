//===-- JennyCStrLib.cpp - Jenny Metaprogramming Support LIB -- -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Sema/Sema.h"


#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"


#include "../Headers/meta/compiler.h"

#include "JennyJITFnAdapter.h"

#include <cstring>
#include <memory>

using namespace clang;
using namespace llvm;

namespace jenny {

class CxxDeclImpl: public ::clang::CxxDeclImplBase {
  std::unique_ptr<DiagnosticsEngine> Diag;
  std::unique_ptr<FileManager> FM;
  std::unique_ptr<SourceManager> SM;
  std::unique_ptr<HeaderSearch> HS;
  std::unique_ptr<Preprocessor> PP;
  std::unique_ptr<ASTContext> PCtx;
  const NamedDecl* Decl;
public:
  CxxDeclImpl(
      std::unique_ptr<DiagnosticsEngine> Diag,
      std::unique_ptr<FileManager> FM,
      std::unique_ptr<SourceManager> SM,
      std::unique_ptr<HeaderSearch> HS,
      std::unique_ptr<Preprocessor> PP,
      std::unique_ptr<ASTContext> PCtx
  ) noexcept:
    Diag(std::move(Diag)),
    FM(std::move(FM)),
    SM(std::move(SM)),
    HS(std::move(HS)),
    PP(std::move(PP)),
    PCtx(std::move(PCtx)),
    Decl(nullptr)
  {}

  const NamedDecl* namedDecl() const noexcept {
    return Decl;
  }

  void setNamedDecl(const NamedDecl* Decl) noexcept {
    this->Decl = Decl;
  }

  ASTContext& context() noexcept {
    return *PCtx;
  }

  Preprocessor& preprocessor() noexcept {
    return *PP;
  }

  SourceManager& sourceManager() noexcept {
    return *SM;
  }

  FileManager& fileManager() noexcept {
    return *FM;
  }

  DiagnosticsEngine& diag() noexcept {
    return *Diag;
  }

  virtual std::string name() const noexcept {
    return Decl->getQualifiedNameAsString();
  }

  virtual std::string to_string() const noexcept {
    std::string buf;
    llvm::raw_string_ostream ss(buf);
    if (Decl) {
      PrintingPolicy PP(LangOptions{});
      PP.Indentation = 1;
      Decl->print(ss, PP);
    }
    else {
      llvm::outs() << "nullptr";
    }
    return buf;
  }

  virtual std::string to_ast_string() const noexcept {
    std::string buf;
    llvm::raw_string_ostream ss(buf);
    if (Decl) {
      Decl->dump(ss);
    }
    else {
      llvm::outs() << "nullptr";
    }
    return buf;
  }

  virtual void pretty_print() const noexcept {
    if (Decl) {
      PrintingPolicy PP(LangOptions{});
      PP.Indentation = 1;
      Decl->print(llvm::outs(), PP);
    }
    else {
      llvm::outs() << "nullptr\n";
    }
  }

  virtual void ast_dump() const noexcept {
    if (Decl) {
      Decl->dump(llvm::outs());
    }
    else {
      llvm::outs() << "nullptr\n";
    }
  }
};


std::unique_ptr<CxxDeclImpl> createCxxDeclImpl(Preprocessor& MainPP, TranslationUnitKind TUKind) {
  const PreprocessorOptions &PPOpts = MainPP.getPreprocessorOpts();

  auto& MainDiag = MainPP.getDiagnostics();
  auto Diag = std::make_unique<DiagnosticsEngine>(
        MainDiag.getDiagnosticIDs(),
        &MainDiag.getDiagnosticOptions(),
        nullptr, false
  );

  auto FM = std::make_unique<FileManager>(
        MainPP.getFileManager().getFileSystemOpts(),
        &MainPP.getFileManager().getVirtualFileSystem()
  );

  auto SM = std::make_unique<SourceManager>(
        *Diag,
        *FM,
        false
  );

  auto HS = std::make_unique<HeaderSearch>(
    MainPP.getHeaderSearchInfo().getHeaderSearchOptsPtr(),
    *SM, *Diag, MainPP.getLangOpts(), nullptr
  );

  std::unique_ptr<Preprocessor> PP = std::make_unique<Preprocessor>(
                                      MainPP.getPreprocessorOptsPtr(),
                                      MainPP.getDiagnostics(), MainPP.getLangOpts(),
                                      *SM, *HS,
                                      MainPP.getModuleLoader(),
                                      /*IdentifierInfoLookup=*/nullptr,
                                      /*OwnsHeaderSearch=*/false,
                                      TUKind);

  PP->Initialize(MainPP.getTargetInfo(), MainPP.getAuxTargetInfo());
  PP->setPredefines(MainPP.getPredefines());

  // Initialize the header search object.  In CUDA compilations, we use the aux
  // triple (the host triple) to initialize our header search, since we need to
  // find the host headers in order to compile the CUDA code.
  const llvm::Triple *HeaderSearchTriple = &PP->getTargetInfo().getTriple();
  if (PP->getTargetInfo().getTriple().getOS() == llvm::Triple::CUDA &&
      PP->getAuxTargetInfo())
    HeaderSearchTriple = &PP->getAuxTargetInfo()->getTriple();

  ApplyHeaderSearchOptions(PP->getHeaderSearchInfo(), PP->getHeaderSearchInfo().getHeaderSearchOpts(),
                           PP->getLangOpts(), *HeaderSearchTriple);

  if (PPOpts.DetailedRecord)
    PP->createPreprocessingRecord();

  std::unique_ptr<ASTContext> PCtx = std::make_unique<ASTContext>(
        PP->getLangOpts(), PP->getSourceManager(), PP->getIdentifierTable(),
        PP->getSelectorTable(), PP->getBuiltinInfo()
  );

  PCtx->InitBuiltinTypes(PP->getTargetInfo(), PP->getAuxTargetInfo());

  return std::make_unique<CxxDeclImpl>(std::move(Diag), std::move(FM),
                                       std::move(SM), std::move(HS),
                                       std::move(PP),
                                       std::move(PCtx));
}

namespace {
struct EmptyASTConsumer: ASTConsumer {};
}

PCxxDecl parse_cxx_top_level_decl(const char* decl_name, const char* code) {
  using namespace ast_matchers;

  CompilerInstance& CI = currentThreadLocalCompilerInstance();

  auto Decl = createCxxDeclImpl(CI.getPreprocessor(), TU_Complete);

  std::unique_ptr<MemoryBuffer> buffer = MemoryBuffer::getMemBufferCopy(code, "<parse_cxx_top_level_decl>");
  FileID fid = Decl->sourceManager().createFileID(std::move(buffer));
  Decl->sourceManager().setMainFileID(fid);

  EmptyASTConsumer Consumer;
  ParseAST(Decl->preprocessor(), &Consumer, Decl->context(), false, TU_Complete, nullptr, false);

  auto Matcher = namedDecl(eachOf(
        recordDecl(hasName(decl_name), isDefinition()),
        functionDecl(hasName(decl_name), isDefinition())
  )).bind("NDecl");
  auto res = match(Matcher, Decl->context());

  if (res.size() > 0) {
    Decl->setNamedDecl(res[0].getNodeAs<NamedDecl>("NDecl"));
  }

  return PCxxDecl(Decl.release());
}

PCxxDecl::~PCxxDecl() noexcept {
  if (decl_) {
    delete decl_;
  }
}


PCxxDecl::PCxxDecl(PCxxDecl&& other) noexcept:
  decl_(other.decl_)
{
  other.decl_ = nullptr;
}

PCxxDecl& PCxxDecl::operator=(PCxxDecl&& other) noexcept {
  if (&other != this) {
    std::swap(decl_, other.decl_);
  }

  return *this;
}

void PCxxDecl::reset() {
  if (decl_) {
    delete decl_;
    decl_ = nullptr;
  }
}

}
