//===- JennyJIT.cpp - A simple JIT for Jenny --------------------*- C++ -*-===//
//
// Part of the LLVM/Jenny Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//============================================================================//


#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ADT/StringMap.h>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Sema/Sema.h"

#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Frontend/CompilerInstance.h"

#include "clang/Jenny/JennyTypeOfVisitor.h"
#include "clang/Jenny/JennyJIT.h"
#include "clang/Jenny/JennyProcessor.h"

#include <clang/AST/DeclGroup.h>
#include <clang/AST/Decl.h>
#include <clang/Sema/Sema.h>

#include "../Headers/compiler/reflect.h"

#include <memory>
#include <iostream>

using namespace llvm;
using namespace llvm::orc;
using namespace clang;

namespace clang {

class JennyProcessorImpl: public JennyProcessor {

protected:
    IntrusiveRefCntPtr<ASTContext> ast_context_;

    IntrusiveRefCntPtr<DiagnosticsEngine> diagnostics_;
    HeaderSearchOptions header_search_options_;
    PreprocessorOptions preprocessor_options_;
    CodeGenOptions codegen_options_;

    //CompilerInstance& compiler_instance_;
    Sema* sema_;
    std::unique_ptr<ASTConsumer> target_;

    std::unique_ptr<LLLazyJIT> jit_;
    ASTNameGenerator name_gen_;
    std::unique_ptr<LLVMContext> llvm_ctx_;
    std::unique_ptr<CodeGenerator> code_generator_;
    std::function<void ()> module_builder_;
    std::unordered_map<std::string, FunctionDecl*> functions_;

    int fn_cnt_{};

    std::vector<std::function<void(ASTConsumer&)>> producer_history_;

    class Visitor: public RecursiveASTVisitor<Visitor> {
        JennyProcessorImpl& consumer_;
    public:
        Visitor(JennyProcessorImpl& consumer): consumer_(consumer) {}

        bool VisitFunctionDecl(FunctionDecl* fd)
        {
            if (fd->hasBody())
            {
                std::string name = consumer_.name_gen_.getName(fd);
                consumer_.functions_[name] = fd;
            }

            return true;
        }

        bool VisitJennyTypeOfExprType(JennyTypeOfExprType* type)
        {


            return false;
        }


    };



    class SymbolGenerator: public JITDylib::DefinitionGenerator {
        JennyProcessorImpl& consumer_;
        std::unique_ptr<DynamicLibrarySearchGenerator> inprocess_;

    public:

        SymbolGenerator(JennyProcessorImpl& consumer):
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
                //std::cout << "Look for: " << str << std::endl;

                auto ii = consumer_.functions_.find(str);
                if (ii != consumer_.functions_.end())
                {
                    //std::cout << "Found: " << str << std::endl;
                    cantFail(consumer_.materialize(ii->second));
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

    friend class Visitor;
    friend class SymbolGenerator;

public:
    JennyProcessorImpl(CompilerInstance& compiler_instance, std::unique_ptr<ASTConsumer> target):
        ast_context_(&compiler_instance.getASTContext()),
        diagnostics_(&compiler_instance.getDiagnostics()),
        header_search_options_(compiler_instance.getHeaderSearchOpts()),
        preprocessor_options_(compiler_instance.getPreprocessorOpts()),
        codegen_options_(compiler_instance.getCodeGenOpts()),

        sema_(nullptr),
        target_(std::move(target)),
        name_gen_(compiler_instance.getASTContext()),
        llvm_ctx_(std::make_unique<LLVMContext>()),
        code_generator_(CreateLLVMCodeGen(
            *diagnostics_,
            "CodeGenModule",
            header_search_options_,
            preprocessor_options_,
            codegen_options_,
            *llvm_ctx_
        ))
    {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

        jit_ = cantFail(LLLazyJITBuilder().create());
        jit_->getMainJITDylib().addGenerator(std::make_unique<SymbolGenerator>(*this));
    }

    void HandleTranslationUnit(ASTContext& ctx)
    {
        auto dd = ctx.getTranslationUnitDecl();
        Visitor visitor(*this);

        visitor.TraverseDecl(dd);

        using FnSig = void (*)(...);
        FnSig fn = (FnSig)cantFail(jit_->lookup("main")).getAddress();
        fn();

        for (auto& fn: producer_history_) {
            fn(*target_);
        }

        target_->HandleTranslationUnit(ctx);
    }

    Error materialize(FunctionDecl* fn_ast)
    {
        code_generator_->Initialize(*ast_context_);
        code_generator_->HandleTopLevelDecl(DeclGroupRef(fn_ast));

        ThreadSafeModule mm(
            std::unique_ptr<llvm::Module>(code_generator_->ReleaseModule()),
            std::move(llvm_ctx_)
        );

        llvm_ctx_ = std::make_unique<LLVMContext>();
        code_generator_->StartModule(std::string("CodeGenModule_") + std::to_string(fn_cnt_++), *llvm_ctx_);

        return jit_->addLazyIRModule(std::move(mm));
    }


    virtual void InitializeSema(Sema& sema) {
        if (SemaConsumer* sc = dyn_cast<SemaConsumer>(target_.get())) {
            sc->InitializeSema(sema);
        }
        sema_ = &sema;
    }

    virtual void ForgetSema() {
        if (SemaConsumer* sc = dyn_cast<SemaConsumer>(target_.get())) {
            sc->ForgetSema();
        }
        sema_ = nullptr;
    }



    /// Initialize - This is called to initialize the consumer, providing the
    /// ASTContext.
    virtual void Initialize(ASTContext &Context) {
        target_->Initialize(Context);
    }

    /// HandleTopLevelDecl - Handle the specified top-level declaration.  This is
    /// called by the parser to process every top-level Decl*.
    ///
    /// \returns true to continue parsing, or false to abort parsing.
    virtual bool HandleTopLevelDecl(DeclGroupRef D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleTopLevelDecl(D);
        });
        return true;
    }

    /// This callback is invoked each time an inline (method or friend)
    /// function definition in a class is completed.
    virtual void HandleInlineFunctionDefinition(FunctionDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleInlineFunctionDefinition(D);
        });
    }



    /// HandleTagDeclDefinition - This callback is invoked each time a TagDecl
    /// (e.g. struct, union, enum, class) is completed.  This allows the client to
    /// hack on the type, which can occur at any point in the file (because these
    /// can be defined in declspecs).
    virtual void HandleTagDeclDefinition(TagDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleTagDeclDefinition(D);
        });
    }

    /// This callback is invoked the first time each TagDecl is required to
    /// be complete.
    virtual void HandleTagDeclRequiredDefinition(const TagDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleTagDeclRequiredDefinition(D);
        });
    }

    /// Invoked when a function is implicitly instantiated.
    /// Note that at this point point it does not have a body, its body is
    /// instantiated at the end of the translation unit and passed to
    /// HandleTopLevelDecl.
    virtual void HandleCXXImplicitFunctionInstantiation(FunctionDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target_->HandleCXXImplicitFunctionInstantiation(D);
        });
    }

    /// Handle the specified top-level declaration that occurred inside
    /// and ObjC container.
    /// The default implementation ignored them.
    virtual void HandleTopLevelDeclInObjCContainer(DeclGroupRef D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleTopLevelDeclInObjCContainer(D);
        });
    }

    /// Handle an ImportDecl that was implicitly created due to an
    /// inclusion directive.
    /// The default implementation passes it to HandleTopLevelDecl.
    virtual void HandleImplicitImportDecl(ImportDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleImplicitImportDecl(D);
        });
    }

    /// CompleteTentativeDefinition - Callback invoked at the end of a translation
    /// unit to notify the consumer that the given tentative definition should be
    /// completed.
    ///
    /// The variable declaration itself will be a tentative
    /// definition. If it had an incomplete array type, its type will
    /// have already been changed to an array of size 1. However, the
    /// declaration remains a tentative definition and has not been
    /// modified by the introduction of an implicit zero initializer.
    virtual void CompleteTentativeDefinition(VarDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.CompleteTentativeDefinition(D);
        });
    }

    /// CompleteExternalDeclaration - Callback invoked at the end of a translation
    /// unit to notify the consumer that the given external declaration should be
    /// completed.
    virtual void CompleteExternalDeclaration(VarDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.CompleteExternalDeclaration(D);
        });
    }

    /// Callback invoked when an MSInheritanceAttr has been attached to a
    /// CXXRecordDecl.
    virtual void AssignInheritanceModel(CXXRecordDecl *RD) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.AssignInheritanceModel(RD);
        });
    }

    /// HandleCXXStaticMemberVarInstantiation - Tell the consumer that this
    // variable has been instantiated.
    virtual void HandleCXXStaticMemberVarInstantiation(VarDecl *D) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleCXXStaticMemberVarInstantiation(D);
        });
    }

    /// Callback involved at the end of a translation unit to
    /// notify the consumer that a vtable for the given C++ class is
    /// required.
    ///
    /// \param RD The class whose vtable was used.
    virtual void HandleVTable(CXXRecordDecl *RD) {
        producer_history_.push_back([=](ASTConsumer& target){
            target.HandleVTable(RD);
        });
    }

    /// If the consumer is interested in entities getting modified after
    /// their initial creation, it should return a pointer to
    /// an ASTMutationListener here.
    virtual ASTMutationListener *GetASTMutationListener() { return nullptr; }

    /// If the consumer is interested in entities being deserialized from
    /// AST files, it should return a pointer to a ASTDeserializationListener here
    virtual ASTDeserializationListener *GetASTDeserializationListener() {
      return nullptr;
    }

    /// PrintStats - If desired, print any statistics.
    virtual void PrintStats() {}

    /// This callback is called for each function if the Parser was
    /// initialized with \c SkipFunctionBodies set to \c true.
    ///
    /// \return \c true if the function's body should be skipped. The function
    /// body may be parsed anyway if it is needed (for instance, if it contains
    /// the code completion point or is constexpr).
    virtual bool shouldSkipFunctionBody(Decl *D) { return true; }
};

JennyProcessor::~JennyProcessor() noexcept {}

std::unique_ptr<JennyProcessor> JennyProcessor::create(CompilerInstance& ci, std::unique_ptr<ASTConsumer> target) {
    return std::make_unique<JennyProcessorImpl>(ci, std::move(target));
}

}
