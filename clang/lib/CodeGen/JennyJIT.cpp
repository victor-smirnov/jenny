//===--- JennyJIT.cpp - Jenny Metacall JIT Implementation -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//


#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/FormatVariadic.h>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Sema/Sema.h"

#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/PreprocessorOptions.h"
#include <clang/Parse/Parser.h>
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/Utils.h"


#include <clang/AST/DeclGroup.h>
#include <clang/AST/Decl.h>
#include <clang/Sema/Sema.h>



#include <llvm/Object/Archive.h>
#include <llvm/Object/Binary.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/CrashRecoveryContext.h>

#include "clang/Basic/JennyJIT.h"

#include "../Headers/__clang_jenny_metacall.h"
#include "JennyASTMaker.h"

#include <memory>
#include <iostream>
#include <atomic>

using namespace llvm;
using namespace llvm::orc;


namespace clang {
namespace {


class DummyASTConsumer: public ASTConsumer {};

class JennyJITImpl: public JennyJIT {

  class SymbolGenerator: public JITDylib::DefinitionGenerator {
    JennyJITImpl& consumer_;
    std::unique_ptr<DynamicLibrarySearchGenerator> inprocess_;

  public:

    SymbolGenerator(JennyJITImpl& consumer):
      consumer_(consumer)
    {
      auto jtmb = cantFail(llvm::orc::JITTargetMachineBuilder::detectHost());
      auto target_machine = cantFail(jtmb.createTargetMachine());
      auto data_layout = target_machine->createDataLayout();

      inprocess_ = cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
                              data_layout.getGlobalPrefix()
                              ));
    }

    Error tryToGenerate(LookupKind K, JITDylib &JD,
                        JITDylibLookupFlags JDLookupFlags,
                        const SymbolLookupSet &LookupSet)
    {
      size_t cnt = 0;
      for (auto sym: LookupSet) {
        auto str = (*sym.first).str();
        auto ii = consumer_.functions_.find(str);
        if (ii != consumer_.functions_.end())
        {
          cnt++;
        }
      }

      if (cnt < LookupSet.size()) {
        return inprocess_->tryToGenerate(K, JD, JDLookupFlags, LookupSet);
      }
      else {
        return Error::success();
      }
    }
  };

  friend class SymbolGenerator;

  IntrusiveRefCntPtr<ASTContext> ast_context_;
  ASTContext& Ctx;

  std::vector<std::string> JitLibs;

  IntrusiveRefCntPtr<DiagnosticsEngine> diagnostics_;
  HeaderSearchOptions header_search_options_;
  PreprocessorOptions preprocessor_options_;
  CodeGenOptions codegen_options_;

  std::unique_ptr<ASTConsumer> target_;

  std::unique_ptr<LLLazyJIT> jit_;
  ASTNameGenerator name_gen_;
  std::unique_ptr<LLVMContext> llvm_ctx_;
  LangOptions mainLangOpts;
  LangOptions jitLangOpts;
  std::unique_ptr<CodeGenerator> code_generator_;
  std::function<void ()> module_builder_;
  std::unordered_map<std::string, FunctionDecl*> functions_;

  std::atomic<unsigned> fn_cnt_{0};

  bool types_configured_{false};
  QualType AdapterClassTy;
  QualType FnArgTy;
  QualType FunctionTy;
  QualType MetaExceptionBaseTy;

  std::unique_ptr<Preprocessor> PP0;
  std::unique_ptr<Sema> SM0;

  DummyASTConsumer Consumer;

public:
  JennyJITImpl(
      Sema& S,
      ASTContext& AstCtx,
      const PCHContainerReader& PCHCtrReader,
      const FrontendOptions& FEOptions,
      const std::vector<std::string>& jitLibs,
      DiagnosticsEngine& diagnostics,
      HeaderSearchOptions header_search_options,
      PreprocessorOptions preprocessor_options,
      CodeGenOptions codegen_options,
      LangOptions lang_options,
      Error& error
  ):
    ast_context_(&AstCtx),
    Ctx(AstCtx),
    JitLibs(jitLibs),
    diagnostics_(&diagnostics),
    header_search_options_(header_search_options),
    codegen_options_(process(codegen_options)),
    name_gen_(*ast_context_),
    llvm_ctx_(std::make_unique<LLVMContext>()),
    jitLangOpts(lang_options),
    code_generator_(CreateLLVMCodeGen(
                      *diagnostics_,
                      "CodeGenModule_0",
                      header_search_options_,
                      preprocessor_options_,
                      codegen_options_,
                      jitLangOpts,
                      *llvm_ctx_
                      ))
  {
    (void)PCHCtrReader;
    (void)FEOptions;

    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

    jit_ = cantFail(LLLazyJITBuilder().create());
    jit_->getMainJITDylib().addGenerator(std::make_unique<SymbolGenerator>(*this));

    for (auto fileName: JitLibs) {
      ErrorOr<std::unique_ptr<MemoryBuffer>> buf = MemoryBuffer::getFile(fileName);
      if (buf) {
        file_magic magic = identify_magic(buf.get()->getBuffer());
        switch (magic) {
        case file_magic::archive : {
          Error err = addArchive(std::move(buf.get()));
          if (err) {
            error = std::move(err);
            return;
          }
          break;
        }
        case file_magic::elf_shared_object : {
          std::string errStr;
          if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(fileName.c_str(), &errStr)) {
              error = createStringError(
                    std::errc::not_supported,
                    "Can't load shared metalib: %s. Reason: %s", fileName.c_str(), errStr.c_str());
          }
          break;
        }
        default: {
          Error err = addObjectFile(std::move(buf.get()));
          if (err) {
            error = std::move(err);
            return;
          }
          break;
        }
        }
      }
      else {
        error = createStringError(buf.getError(), "Can't load metalib file %s", fileName.c_str());
        break;
      }
    }

    Preprocessor& PP = S.getPreprocessor();
    mainLangOpts = PP.getLangOpts();

    PP0 = std::make_unique<Preprocessor>(
          std::make_shared<PreprocessorOptions>(PP.getPreprocessorOpts()),
          PP.getDiagnostics(),
          mainLangOpts,
          S.getSourceManager(),
          PP.getHeaderSearchInfo(),
          PP.getModuleLoader(),
          nullptr, false, TU_Prefix, &PP
    );

    PP0->Initialize(PP.getTargetInfo(), PP.getAuxTargetInfo());
    InitializePreprocessor(*PP0.get(), PP.getPreprocessorOpts(), PCHCtrReader, FEOptions);

    SM0 = std::make_unique<Sema>(
          *PP0.get(),
          Ctx, Consumer,
          TU_Prefix, nullptr, &S
    );

    PP0->InitPredefines();

    Parser P(*PP0.get(), *SM0.get(), false);
    P.Initialize();

    Parser::DeclGroupPtrTy ADecl;
    for (bool AtEOF = P.ParseFirstTopLevelDecl(ADecl);
         !AtEOF;
         AtEOF = P.ParseTopLevelDecl(ADecl))
    {}
  }

  static CodeGenOptions& process(CodeGenOptions& options) {
    options.setDebugInfo(codegenoptions::DebugInfoKind::NoDebugInfo);
    return options;
  }

  Error addObjectFile(std::unique_ptr<MemoryBuffer> buf) {
    return jit_->addObjectFile(std::move(buf));
  }

  Error addArchive(std::unique_ptr<MemoryBuffer> buf) {
    Expected<std::unique_ptr<object::Archive>> arr = object::Archive::create(*buf.get());
    if (arr) {
      Error err = Error::success();
      for (auto child: arr.get()->children(err)) {
        if (!err) {
          Expected<MemoryBufferRef> bb = child.getMemoryBufferRef();
          if (bb) {
            if (Error bbErr = addObjectFile(MemoryBuffer::getMemBuffer(bb.get()))) {
              return bbErr;
            }
          }
          else {
            return bb.takeError();
          }
        }
        else {
          return err;
        }
      }
      return err;
    }
    else {
      return arr.takeError();
    }
  }

  Expected<Decl*> parseTopLevelDecl(const std::string& data) noexcept
  {
    std::unique_ptr<MemoryBuffer> buffer = MemoryBuffer::getMemBufferCopy(data, "<JennyJIT>");
    FileID fid = Ctx.getSourceManager().createFileID(std::move(buffer));

    Parser P(*PP0.get(), *SM0.get(), false);

    Sema::ContextRAII CtxRAII(*SM0.get(), Ctx.getTranslationUnitDecl());

    PP0->EnterSourceFile(fid, PP0->GetCurDirLookup(), SourceLocation{});

    P.Initialize();

    Decl* result = nullptr;
    Parser::DeclGroupPtrTy ADecl;
    for (bool AtEOF = P.ParseFirstTopLevelDecl(ADecl); !AtEOF;
         AtEOF = P.ParseTopLevelDecl(ADecl)) {
      // If we got a null return and something *was* parsed, ignore it.  This
      // is due to a top-level semicolon, an action override, or a parse error
      // skipping something.
      if (ADecl) {
        result = ADecl.get().getSingleDecl();
        if (result && !dyn_cast<EmptyDecl>(result)) {
          break;
        }
        else {
          result = nullptr;
          break;
        }
      }
    }

    return result;
  }

  Expected<FunctionDecl*> CreateAdapter(const CallExpr* call, llvm::ArrayRef<QualType> args) noexcept override {
    if (!types_configured_) {
      ConfigureTypes();
      types_configured_ = true;
    }

    const FunctionDecl* callee = call->getDirectCallee();
    assert(callee && "callee is null");

    std::string calleName = callee->getName().str();
    std::string fnName = "__metacall_" + calleName + "__" + std::to_string(fn_cnt_.fetch_add(1));

    std::string buffer = "void " + fnName + "(__jy::JennyMetaCallAdapter& adapter) {\n";
    buffer += "  try {\n";

    std::string calleQName = callee->getQualifiedNameAsString();

    if (callee->getReturnType() == Ctx.VoidTy) {
      buffer += "    " + calleQName + "(\n";
      AppendArgs(buffer, args);
      buffer += "    );\n";
    }
    else {
      buffer += "    auto res = " + calleQName + "(\n";
      AppendArgs(buffer, args);
      buffer += "    );\n";

      buffer += "    adapter.result(&res);\n";
    }

    buffer += R"(  }
  catch (::jenny::MetaExceptionBase& ex) {
    adapter.except(ex);
  }
  catch (...) {
    adapter.except_unknown();
  }
})";

    Expected<Decl*> decl = parseTopLevelDecl(buffer);
    if (decl) {
      if (decl.get()) {
        assert(isa<FunctionDecl>(decl.get()) && "Metacall adapter is not a FunctionCall");
        return dyn_cast<FunctionDecl>(decl.get());
      }
      else {
        return llvm::make_error<llvm::StringError>(
            llvm::formatv("Cannot compile metacall adapter for: {0}, the adapter is null.", calleQName),
            llvm::inconvertibleErrorCode());
      }
    }
    else {
      return decl.takeError();
    }
  }


  Expected<std::string> compile(FunctionDecl* adapter) noexcept override
  {
    std::string fn_name = name_gen_.getName(adapter);

    code_generator_->Initialize(Ctx);
    code_generator_->HandleTopLevelDecl(DeclGroupRef(adapter));

    ThreadSafeModule mm(
          std::unique_ptr<llvm::Module>(code_generator_->ReleaseModule()),
          std::move(llvm_ctx_)
          );

    //mm.getModuleUnlocked()->dump();

    Function* fn = mm.getModuleUnlocked()->getFunction(fn_name);
    if (!fn) {
      return llvm::make_error<llvm::StringError>(
          llvm::formatv("Cannot compile metacall adapter for: {0}", fn_name),
          llvm::inconvertibleErrorCode());
    }

    llvm_ctx_ = std::make_unique<LLVMContext>();
    code_generator_->StartModule(std::string("CodeGenModule_") + std::to_string(++fn_cnt_), *llvm_ctx_);

    if (Error err = jit_->addLazyIRModule(std::move(mm))) {
      return std::move(err);
    }

    return fn_name;
  }

  Expected<void*> GetSymbol(llvm::StringRef name) noexcept override {
    Expected<JITEvaluatedSymbol> sym = jit_->lookup(name);
    if (sym) {
      return (void*)sym->getAddress();
    }
    else {
      return sym.takeError();
    }
  }

private:
  void AppendArgs(std::string& buffer, llvm::ArrayRef<QualType> argTypes) {
    size_t idx{};
    for (QualType valType: argTypes) {
      buffer += std::string("    *(") + valType.getAsString() + "*) adapter.param(" + std::to_string(idx) + ")";

      if (idx < argTypes.size() - 1) {
        buffer += ",";
      }

      buffer += "\n";

      idx++;
    }
  }

  void ConfigureTypes() {
    AdapterClassTy = Ctx.getCanonicalType(getFnArgTy());
    FnArgTy = Ctx.getLValueReferenceType(AdapterClassTy);

    SmallVector<QualType, 1> ArgTys;
    ArgTys.push_back(FnArgTy);
    FunctionTy = Ctx.getFunctionType(Ctx.VoidTy, ArgTys, {});

    MetaExceptionBaseTy = Ctx.getCanonicalType(getMetaExceptionBaseTy());
  }

  QualType getFnArgTy() const {
    for (Type* tt: Ctx.getTypes()){
      QualType type(tt, 0);
      if (type.getAsString() == "struct __jy::JennyMetaCallAdapter") {
        return type;
      }
    }

    llvm_unreachable("No __clang_jenny_metacall.h is included");
  }

  QualType getMetaExceptionBaseTy() const {
    for (Type* tt: Ctx.getTypes()){
      QualType type(tt, 0);
      if (type.getAsString() == "struct jenny::MetaExceptionBase") {
        return type;
      }
    }

    llvm_unreachable("No __clang_jenny_metacall.h is included");
  }
};

}

JennyJIT::~JennyJIT() noexcept {}

Expected<std::shared_ptr<JennyJIT>> JennyJIT::Create(
    Sema& S,
    ASTContext& Ctx,
    const PCHContainerReader& PCHCtrReader,
    const FrontendOptions& FEOptions,
    const std::vector<std::string>& jitLibs,
    DiagnosticsEngine& diagnostics,
    const HeaderSearchOptions& header_search_options,
    const PreprocessorOptions& preprocessor_options,
    const CodeGenOptions& codegen_options,
    const LangOptions& lang_options
) {
  Error error = Error::success();
  (void)(bool)error;
  auto ptr = std::make_shared<JennyJITImpl>(        
        S, Ctx, PCHCtrReader, FEOptions,
        jitLibs, diagnostics, header_search_options,
        preprocessor_options, codegen_options, lang_options,
        error
  );

  if (!error) {
    consumeError(std::move(error));
    return ptr;
  }
  else {
    return std::move(error);
  }
}



}
