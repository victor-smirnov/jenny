/*===---- __clang_jenny_metacall.h - HIP runtime support ---------------===
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===-----------------------------------------------------------------------===
 */

/**
 * __jy_meta_call operator support types. You don't need to include this header
 * directly.
 */

namespace __jy {

struct JennyMetaCallAdapter {
  virtual ~JennyMetaCallAdapter() noexcept;

  virtual const void* param_const(int num) const noexcept = 0;
  virtual void* param(int num) const noexcept = 0;

  virtual void result(const void* value) const noexcept = 0;
};

}
