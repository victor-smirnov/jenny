/*===---- compiler.h - Jenny Compiler API ----------------------------------===
 *
 * Part of the Jenny Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===-----------------------------------------------------------------------===
 */

#pragma once

namespace jenny {
namespace reflect {

struct CompilerObject;
struct Class;
struct Compiler;
struct Function;
struct Argument;
struct Type;
struct Template;
struct Runtime;

struct CompilerObject {
    virtual ~CompilerObject() noexcept;
};

struct Type: CompilerObject {
    virtual const char* name() const = 0;
};

struct Class: Type {

};

struct Template: CompilerObject {

};

struct Function: CompilerObject {
};

struct Runtime: CompilerObject {
};


struct Compiler {
public:
    virtual ~Compiler() noexcept;
    virtual Class* make_class(const char* name) = 0;
    virtual void free(CompilerObject*) noexcept = 0;

    virtual void print(const char* message) noexcept = 0;
};

Compiler& compiler() noexcept;

Runtime& runtime() noexcept;

}}
