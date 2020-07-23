# Jenny Platform

Jenny is an _experimental_ metaprogramming Platform basen on Clang and
[Memoria Framework](https://bitbucket.org/vsmirnov/memoria/wiki/Home) for
data-intensive applications.

In a few words, Memoria is a database engine development framework providing
high-performance mutimodel persistent (functional) data representation
for transactional and analytical applications. Currently, it's heavily
relying on template metaprogramming to assemble complex data containers
out of reusable primitives. But to unleash its full potential, Memoria
needs more capable metaprogramming than C++ is currently providing.

Jenny Platform is mostly focused on *tooling*, it extends Clang and
C++20 in the following ways:

1. Reflection/Metaprogramming: making compiler internals accessible to
metaprograms at the type level and at the AST/syntax level. The whole idea
is pretty similar to proposals for
[Reflection TS](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1240r1.pdf).

1. Embedded domain specific languages for data structures authoring
and querying. [SDN](https://bitbucket.org/vsmirnov/memoria/wiki/String%20Data%20Notation)
and its type system is integrated natively with the language as a
core data represenation (SDN is something like JSON for JavaScript
but with types). Other examples of DSLs are SQL and JMESpath.

1. Full JIT-enabled lazy C++ interpreter available at compile time. Metaprograms
can be compiled separately, can do IO, access databases, spawn threads
and do heavy computations like theorem proving with no significant
performance penalties relative to AOT-compiled code.

1. Memoria-based code model. Parsed Clang's AST files can be stored in
Memoria's containers togather with additional data and metadata,
enabling immediately accessible (zero serialization) embedded databases
for run-time reflection metadata and arbitrary code annotations.
Memoria's containers can be linked with executable files.
Such code representain enables efficient JITting of C++ at run-time,
because there is no need to parse huuuuge files anymore.

1. Metaprogramming-aware build system (Jenny Built Tool, JBT) and
packaging tool based on the C++ interpreter. Metaprograms are
aware of the project structure _and_ compilation process. JBT is
built on top of Memoria-provided high-performance transactional
versioned (like Git) database enabling every resource (including
intermediate state) to be accessible by the clients(IDEs,
metaprograms). Compilation in Jenny is project-based, not TU-based.

1. JBT exposes everything (code model, building process' state)
via elaborate RESTful API for external clients to integrate with.
IDE developers can focus on feature, usability and UX, leaving
everything else to JBT that can be run either locally (stdin/stdout)
or remotely (HTTPS). Interested third parties can provide Kubernetes
integration for JBT (to scale-out compilation of very large projects)
and cloud-native storage options for Memoria.

1. Memoria has sophisticated cross-platform runtime library providing
advanced data types, custom memory allocators, fibers (forked
from Boost) and high-performance asynchronous IO for decent storage
and networking. Core and essential parts of this runtime will be
ported into Jenny and supported at the level of the programming language
(via C++ extensions).

When Jenny is mature enough, Memoria will be rewritten in its
dialect of C++ leveraging the ultimate metaprogramming capabilities
the Platform is intended to provide.

# The LLVM Compiler Infrastructure

This directory and its sub-directories contain source code for LLVM,
a toolkit for the construction of highly optimized compilers,
optimizers, and run-time environments.

The README briefly describes how to get started with building LLVM.
For more information on how to contribute to the LLVM project, please
take a look at the
[Contributing to LLVM](https://llvm.org/docs/Contributing.html) guide.

## Getting Started with the LLVM System

Taken from https://llvm.org/docs/GettingStarted.html.

### Overview

Welcome to the LLVM project!

The LLVM project has multiple components. The core of the project is
itself called "LLVM". This contains all of the tools, libraries, and header
files needed to process intermediate representations and converts it into
object files.  Tools include an assembler, disassembler, bitcode analyzer, and
bitcode optimizer.  It also contains basic regression tests.

C-like languages use the [Clang](http://clang.llvm.org/) front end.  This
component compiles C, C++, Objective-C, and Objective-C++ code into LLVM bitcode
-- and from there into object files, using LLVM.

Other components include:
the [libc++ C++ standard library](https://libcxx.llvm.org),
the [LLD linker](https://lld.llvm.org), and more.

### Getting the Source Code and Building LLVM

The LLVM Getting Started documentation may be out of date.  The [Clang
Getting Started](http://clang.llvm.org/get_started.html) page might have more
accurate information.

This is an example work-flow and configuration to get and build the LLVM source:

1. Checkout LLVM (including related sub-projects like Clang):

     * ``git clone https://github.com/llvm/llvm-project.git``

     * Or, on windows, ``git clone --config core.autocrlf=false
    https://github.com/llvm/llvm-project.git``

2. Configure and build LLVM and Clang:

     * ``cd llvm-project``

     * ``mkdir build``

     * ``cd build``

     * ``cmake -G <generator> [options] ../llvm``

        Some common build system generators are:

        * ``Ninja`` --- for generating [Ninja](https://ninja-build.org)
          build files. Most llvm developers use Ninja.
        * ``Unix Makefiles`` --- for generating make-compatible parallel makefiles.
        * ``Visual Studio`` --- for generating Visual Studio projects and
          solutions.
        * ``Xcode`` --- for generating Xcode projects.

        Some Common options:

        * ``-DLLVM_ENABLE_PROJECTS='...'`` --- semicolon-separated list of the LLVM
          sub-projects you'd like to additionally build. Can include any of: clang,
          clang-tools-extra, libcxx, libcxxabi, libunwind, lldb, compiler-rt, lld,
          polly, or debuginfo-tests.

          For example, to build LLVM, Clang, libcxx, and libcxxabi, use
          ``-DLLVM_ENABLE_PROJECTS="clang;libcxx;libcxxabi"``.

        * ``-DCMAKE_INSTALL_PREFIX=directory`` --- Specify for *directory* the full
          path name of where you want the LLVM tools and libraries to be installed
          (default ``/usr/local``).

        * ``-DCMAKE_BUILD_TYPE=type`` --- Valid options for *type* are Debug,
          Release, RelWithDebInfo, and MinSizeRel. Default is Debug.

        * ``-DLLVM_ENABLE_ASSERTIONS=On`` --- Compile with assertion checks enabled
          (default is Yes for Debug builds, No for all other build types).

      * ``cmake --build . [-- [options] <target>]`` or your build system specified above
        directly.

        * The default target (i.e. ``ninja`` or ``make``) will build all of LLVM.

        * The ``check-all`` target (i.e. ``ninja check-all``) will run the
          regression tests to ensure everything is in working order.

        * CMake will generate targets for each tool and library, and most
          LLVM sub-projects generate their own ``check-<project>`` target.

        * Running a serial build will be **slow**.  To improve speed, try running a
          parallel build.  That's done by default in Ninja; for ``make``, use the option
          ``-j NNN``, where ``NNN`` is the number of parallel jobs, e.g. the number of
          CPUs you have.

      * For more information see [CMake](https://llvm.org/docs/CMake.html)

Consult the
[Getting Started with LLVM](https://llvm.org/docs/GettingStarted.html#getting-started-with-llvm)
page for detailed information on configuring and compiling LLVM. You can visit
[Directory Layout](https://llvm.org/docs/GettingStarted.html#directory-layout)
to learn about the layout of the source code tree.
