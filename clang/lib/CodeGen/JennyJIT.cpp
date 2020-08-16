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
#include "clang/Frontend/CompilerInstance.h"

//#include "clang/Jenny/JennyTypeOfVisitor.h"
//#include "clang/Jenny/JennyJIT.h"
//#include "clang/Jenny/JennyProcessor.h"

#include <clang/AST/DeclGroup.h>
#include <clang/AST/Decl.h>
#include <clang/Sema/Sema.h>

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
                //std::cout << "Look for: " << str << std::endl;

                auto ii = consumer_.functions_.find(str);
                if (ii != consumer_.functions_.end())
                {
                    //std::cout << "Found: " << str << std::endl;
                    //cantFail(consumer_.materialize(ii->second));
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
    std::unique_ptr<CodeGenerator> code_generator_;
    std::function<void ()> module_builder_;
    std::unordered_map<std::string, FunctionDecl*> functions_;

    std::atomic<unsigned> fn_cnt_{0};

    bool types_configured_{false};
    QualType AdapterClassTy;
    QualType FnArgTy;
    QualType FunctionTy;


public:
    JennyJITImpl(
        ASTContext& AstCtx,
        const std::vector<std::string>& jitLibs,
        DiagnosticsEngine& diagnostics,
        HeaderSearchOptions header_search_options,
        PreprocessorOptions preprocessor_options,
        CodeGenOptions codegen_options
    ):  ast_context_(&AstCtx),
        Ctx(AstCtx),
        JitLibs(jitLibs),
        diagnostics_(&diagnostics),
        header_search_options_(header_search_options),
        codegen_options_(codegen_options),
        name_gen_(*ast_context_),
        llvm_ctx_(std::make_unique<LLVMContext>()),
        code_generator_(CreateLLVMCodeGen(
            *diagnostics_,
            "CodeGenModule_0",
            header_search_options_,
            preprocessor_options_,
            codegen_options_,
            *llvm_ctx_
        ))
    {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

        jit_ = cantFail(LLLazyJITBuilder().create());
        jit_->getMainJITDylib().addGenerator(std::make_unique<SymbolGenerator>(*this));

        for (auto fileName: JitLibs) {
            cantFail(jit_->addObjectFile(std::move(MemoryBuffer::getFile(fileName).get())));
        }
    }





    FunctionDecl* CreateAdapter(const CallExpr* call, llvm::ArrayRef<QualType> args) noexcept override
    {
        if (!types_configured_) {
            ConfigureTypes();
            types_configured_ = true;
        }

        const FunctionDecl* callee = call->getDirectCallee();
        assert(callee && "callee is null");

        std::string name = callee->getName().str();
        name = "__metacall_" + name + "__" + std::to_string(fn_cnt_.fetch_add(1));

        const IdentifierInfo* fnNameInfo = &ast_context_->Idents.get(name);

        FunctionDecl* fn = FunctionDecl::Create(
            Ctx,
            Ctx.getTranslationUnitDecl(),
            SourceLocation{},
            SourceLocation{},
            fnNameInfo,
            FunctionTy,
            nullptr,
            SC_None
        );

        Ctx.getTranslationUnitDecl()->addDecl(fn);

        IdentifierInfo* adapterII = &Ctx.Idents.get("adapter");
        llvm::SmallVector<ParmVarDecl*, 1> ParamVars;
        ParamVars.push_back(ParmVarDecl::Create(
            Ctx,
            fn,
            SourceLocation(), SourceLocation(),
            adapterII,
            FnArgTy,
            nullptr,
            SC_None,
            nullptr
        ));

        fn->setParams(ParamVars);

        JennyASTMaker maker(Ctx);

        SmallVector<Stmt*, 2> statements;
        SmallVector<Expr*, 8> Args;

        DeclRefExpr* adapterRefCast = maker.makeDeclRefExpr(ParamVars[0]);

        CXXRecordDecl *AdapterClassDecl = AdapterClassTy->getAsCXXRecordDecl();
        CXXMethodDecl *paramMethod = maker.findMemberMethod(AdapterClassDecl, "param");
        MemberExpr* memberExpr = maker.makeMemberExpression(adapterRefCast, paramMethod);

        size_t idx{};
        size_t callee_params = callee->parameters().size();
        for (QualType valType: args)
        {
            Expr* expr;
            IntegerLiteral* paramNum = maker.makeIntegerLiteral(idx, Ctx.IntTy);
            CXXMemberCallExpr* memberCall = maker.makeCXXMemberCall(memberExpr, paramNum, Ctx.VoidPtrTy, VK_RValue);

            if (idx < callee_params)
            {
                ParmVarDecl* arg = callee->parameters()[idx];
                QualType tgtType = Ctx.getCanonicalParamType(arg->getType());

                if (tgtType->isPointerType()) {
                    expr = maker.makeCStyleCastExpr(tgtType, VK_RValue, CK_BitCast, memberCall);
                }
                else {
                    CStyleCastExpr* castExpr = maker.makeCStyleCastExpr(Ctx.getPointerType(tgtType), VK_RValue, CK_BitCast, memberCall);
                    UnaryOperator *deref = maker.makeDereference(castExpr, tgtType);
                    expr = maker.makeLvalueToRvalue(deref, tgtType);
                }
            }
            else {
                QualType tgtType = valType;

                if (tgtType->isPointerType()) {
                    expr = maker.makeCStyleCastExpr(tgtType, VK_RValue, CK_BitCast, memberCall);
                }
                else {
                    CStyleCastExpr* castExpr = maker.makeCStyleCastExpr(Ctx.getPointerType(tgtType), VK_RValue, CK_BitCast, memberCall);
                    UnaryOperator *deref = maker.makeDereference(castExpr, tgtType);
                    expr = maker.makeLvalueToRvalue(deref, tgtType);
                }
            }

            Args.push_back(expr);
            ++idx;
        }

        CallExpr* tgtCallee = CallExpr::Create(
            Ctx,
            const_cast<Expr*>(call->getCallee()),
            Args,
            callee->getType(),
            VK_RValue,
            SourceLocation{},
            call->getFPFeatures()
        );

        QualType returnType = callee->getReturnType();
        bool isNonVoid = Ctx.getCanonicalType(returnType) != Ctx.VoidTy;
        if (isNonVoid)
        {
            VarDecl* retVal = VarDecl::Create(
                Ctx, fn, SourceLocation{}, SourceLocation{},
                &Ctx.Idents.get("res"),
                returnType, nullptr, StorageClass::SC_None);

            retVal->setInit(tgtCallee);

            DeclStmt* retValStmt = new (Ctx) DeclStmt(DeclGroupRef{retVal}, SourceLocation{}, SourceLocation{});
            statements.push_back(retValStmt);

            DeclRefExpr* retValRef = maker.makeDeclRefExpr(retVal);

            MemberExpr* resultMemberExpr;
            Expr* resultArg;

            if (returnType->isPointerType())
            {
                CXXMethodDecl *resultMethod  = maker.findMemberMethod(AdapterClassDecl, "result_ptr");
                resultMemberExpr = maker.makeMemberExpression(adapterRefCast, resultMethod);

                resultArg = maker.makeLvalueToRvalue(retValRef, returnType);
            }
            else {
                CXXMethodDecl *resultMethod  = maker.findMemberMethod(AdapterClassDecl, "result_val");
                resultMemberExpr = maker.makeMemberExpression(adapterRefCast, resultMethod);

                resultArg = UnaryOperator::Create(Ctx, retValRef, UO_AddrOf,
                                                  Ctx.getPointerType(returnType),
                                                  VK_RValue, OK_Ordinary,
                                                  SourceLocation{},
                                                  false, FPOptionsOverride{});
            }

            QualType constVoidPtr = Ctx.getPointerType(Ctx.VoidTy.withConst());
            ImplicitCastExpr* cast2 = maker.makeImplicitCast(resultArg, constVoidPtr, CK_BitCast);
            CXXMemberCallExpr* reslutExpr = maker.makeCXXMemberCall(resultMemberExpr, cast2, Ctx.VoidTy, VK_RValue);
            statements.push_back(reslutExpr);
        }
        else {
            statements.push_back(tgtCallee);
        }

        fn->setBody(maker.makeCompound(statements));

        //fn->print(llvm::outs(), 2);
        //llvm::outs().flush();

        return fn;
    }

    std::string compile(FunctionDecl* adapter) noexcept override
    {
        std::string fn_name = name_gen_.getName(adapter);

        code_generator_->Initialize(Ctx);
        code_generator_->HandleTopLevelDecl(DeclGroupRef(adapter));

        ThreadSafeModule mm(
            std::unique_ptr<llvm::Module>(code_generator_->ReleaseModule()),
            std::move(llvm_ctx_)
        );

        //mm.getModuleUnlocked()->dump();

        llvm_ctx_ = std::make_unique<LLVMContext>();
        code_generator_->StartModule(std::string("CodeGenModule_") + std::to_string(++fn_cnt_), *llvm_ctx_);

        cantFail(jit_->addLazyIRModule(std::move(mm)));

        return fn_name;
    }

    void* GetSymbol(llvm::StringRef name) noexcept override {
        return (void*)cantFail(jit_->lookup(name)).getAddress();
    }

private:
    void ConfigureTypes() {
        AdapterClassTy = Ctx.getCanonicalType(getFnArgTy());
        FnArgTy = Ctx.getLValueReferenceType(AdapterClassTy);

        SmallVector<QualType, 1> ArgTys;
        ArgTys.push_back(FnArgTy);
        FunctionTy = Ctx.getFunctionType(Ctx.VoidTy, ArgTys, {});
    }

    QualType getFnArgTy() const {
        for (Type* tt: Ctx.getTypes()){
            QualType type(tt, 0);
            //std::cout << type.getAsString() << std::endl;
            if (type.getAsString() == "struct __jy::JennyMetaCallAdapter") {
                return type;
            }
        }

        llvm_unreachable("No __clang_jenny_metacall.h is included");
        return QualType(static_cast<Type*>(nullptr), 0);
    }
};

}

JennyJIT::~JennyJIT() noexcept {}

std::shared_ptr<JennyJIT> JennyJIT::Create(
    ASTContext& Ctx,
    const std::vector<std::string>& jitLibs,
    DiagnosticsEngine& diagnostics,
    const HeaderSearchOptions& header_search_options,
    const PreprocessorOptions& preprocessor_options,
    const CodeGenOptions& codegen_options
) {
    return std::make_shared<JennyJITImpl>(
        Ctx, jitLibs, diagnostics, header_search_options,
        preprocessor_options, codegen_options
    );
}

}
