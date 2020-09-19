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

#include "../Headers/meta/cstr.h"

#include <cstring>

namespace jenny {

namespace  {

char EMPTY[1] = {0};

char* MergeData(const char* one, size_t one_len, const char* two, size_t two_len) {
  size_t len = one_len + two_len + 1;
  char* data = new char[len];

  if (one)
  {
    std::memcpy(data, one, one_len);
  }

  if (two)
  {
    std::memcpy(data + one_len, two, two_len);
  }

  data[len] = 0;

  return data;
}

}


CStr::CStr() noexcept :
  length_(0), data_(EMPTY)
{}

CStr::CStr(const char* str) noexcept
{
  if (str) {
    size_t len = std::strlen(str);
    data_ = new char[len + 1];
    std::memcpy(data_, str, len + 1);
    length_ = len;
  }
  else {
    data_ = nullptr;
    length_ = 0;
  }
}

CStr::CStr(const char* str, size_t len) noexcept {
  if (str) {
    data_ = new char[len + 1];
    std::memcpy(data_, str, len + 1);
    length_ = len;
  }
  else {
    data_ = nullptr;
    length_ = 0;
  }
}

CStr::CStr(const CStr& str) noexcept
{
  if (str) {
    if (!str.is_empty()) {
      data_ = new char[str.length() + 1];
      std::memcpy(data_, str.data(), str.length() + 1);
      length_ = str.length();
    }
    else {
      data_ = EMPTY;
      length_ = 0;
    }
  }
  else {
    data_ = nullptr;
    length_ = 0;
  }
}

CStr::CStr(CStr&& other) noexcept:
  length_(other.length_),
  data_(other.data_)
{
  other.data_ = nullptr;
  other.length_ = 0;
}

CStr::CStr(std::string str) noexcept:
  length_(0), data_(nullptr)
{
  operator=(str);
}

CStr::~CStr() noexcept {
  clear();
}

void CStr::clear() noexcept {
  if (data_ && data_ != EMPTY) {
    delete[] data_;
    length_ = 0;
  }
}

bool CStr::operator==(const CStr& other) const noexcept
{
  if (length_ == other.length_) {
    if (data_ && other.data_) {
      return std::strcmp(data_, other.data_);
    }
  }

  return false;
}

CStr& CStr::operator=(const CStr& other) noexcept
{
  size_t len = other.length();
  data_ = new char[len + 1];
  std::memcpy(data_, other.data(), len + 1);
  return *this;
}

CStr& CStr::operator=(CStr&& other) noexcept
{
  if (&other != this) {
    clear();
    data_ = other.data_;
    length_ = other.length_;

    other.data_ = nullptr;
    other.length_ = 0;
  }

  return *this;
}


CStr& CStr::operator=(const char* str) noexcept
{
  clear();
  size_t len = std::strlen(str);
  data_ = new char[len + 1];
  std::memcpy(data_, str, len + 1);

  return *this;
}


CStr& CStr::operator=(std::string str) noexcept {
  return operator=(CStr(str.data(), str.length()));
}

CStr& CStr::operator+=(const CStr& other) noexcept {
  *this = operator+(*this, other);
  return *this;
}

CStr& CStr::operator+=(const char* str) noexcept {
  if (str) {
    size_t str_len = std::strlen(str);
    if (str_len) {
      size_t len = length_;
      char* data = MergeData(data_, length_, str, str_len);
      clear();
      data_ = data;
      length_ = len + str_len;
    }
  }

  return *this;
}

CStr& CStr::operator+=(const std::string& str) noexcept {
  if (!str.empty()) {
    size_t len = length_;
    char* data = MergeData(data_, length_, str.data(), str.length());
    clear();
    data_ = data;
    length_ = len + str.length();
  }

  return *this;
}

CStr operator+(const CStr& one, const CStr& two) noexcept {
  CStr res(nullptr);

  if (one.is_null() && two.is_null()) {
    return res;
  }

  res.data_ = MergeData(one.data(), one.length(), two.data(), two.length());
  res.length_ = one.length() + two.length() + 1;

  return res;
}

void swap(CStr& one, CStr& two) noexcept {
  using std::swap;

  swap(one.data_, two.data_);
  swap(one.length_, two.length_);
}

}
