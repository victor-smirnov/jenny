/*===---- meta/cstr.h - Jenny Metaprogramming Support ----------------------===
 *
 * Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
 * See https://llvm.org/LICENSE.txt for license information.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 *===-----------------------------------------------------------------------===
 */

#include <cstdlib>
#include <string>

namespace jenny {

class CStr;
void swap(CStr&, CStr&) noexcept;
CStr operator+(const CStr& one, const CStr& two) noexcept;

class CStr {
  size_t length_;
  char* data_;
public:
  friend void ::jenny::swap(CStr&, CStr&) noexcept;
  friend CStr operator+(const CStr&, const CStr&) noexcept;

  CStr() noexcept;
  CStr(const char*) noexcept;
  CStr(const char*, size_t len) noexcept;
  CStr(const CStr&) noexcept;
  CStr(CStr&&) noexcept;

  CStr(std::string) noexcept;
  ~CStr() noexcept;

  const char* data() const noexcept {
    return data_;
  }

  size_t length() const noexcept {
    return length_;
  }

  operator const char*() const noexcept {
    return data_;
  }

  char operator[](size_t idx) const noexcept {
    return data_[idx];
  }

  bool operator==(const CStr& other) const noexcept;
  CStr& operator=(const CStr& other) noexcept;
  CStr& operator=(CStr&& other) noexcept;
  CStr& operator=(const char*) noexcept;
  CStr& operator=(std::string str) noexcept;

  CStr& operator+=(const CStr& other) noexcept;
  CStr& operator+=(const char* str) noexcept;
  CStr& operator+=(const std::string& str) noexcept;

  void clear() noexcept;

  bool is_null() const noexcept {
    return data_ == nullptr;
  }

  bool is_empty() const noexcept {
    return is_null() || length_ == 0;
  }

  operator bool() const noexcept {
    return !is_empty();
  }
};


}
