/* vim: set ts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#ifndef _include_auto_string_h_
#define _include_auto_string_h_

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <amtl/am-string.h>

namespace ke {

class AutoString
{
public:
  AutoString() : ptr_(nullptr), length_(0)
  {
  }
  AutoString(AutoString &&other)
    : ptr_(other.ptr_),
    length_(other.length_)
  {
    other.ptr_ = nullptr;
    other.length_ = 0;
  }
  AutoString(const char *ptr)
  {
    assign(ptr);
  }
  AutoString(const AString &str)
  {
    assign(str.chars(), str.length());
  }
  AutoString(const char *ptr, size_t len)
  {
    assign(ptr, len);
  }
  AutoString(const AutoString &other)
  {
    assign(other.ptr(), other.length());
  }
  ~AutoString()
  {
    free(ptr_);
  }

  AutoString &operator =(const char *ptr) {
    free(ptr_);
    assign(ptr);
    return *this;
  }
  AutoString &operator =(const AutoString &other) {
    free(ptr_);
    assign(other.ptr(), other.length());
    return *this;
  }
  AutoString &operator =(AutoString &&other) {
    Swap(other.ptr_, ptr_);
    Swap(other.length_, length_);
    return *this;
  }

  AutoString operator +(const AutoString &other) const {
    size_t len = length() + other.length();
    char *buf = (char *)malloc(len + 1);
    memcpy(buf, ptr(), length());
    memcpy(buf + length(), other.ptr(), other.length());
    buf[len] = '\0';

    AutoString r;
    r.ptr_ = buf;
    r.length_ = len;
    return r;
  }

  AutoString operator +(const char *other) const {
    size_t other_length = strlen(other);
    size_t len = length() + other_length;
    char *buf = (char *)malloc(len + 1);
    memcpy(buf, ptr(), length());
    memcpy(buf + length(), other, other_length);
    buf[len] = '\0';

    AutoString r;
    r.ptr_ = buf;
    r.length_ = len;
    return r;
  }

  AutoString operator +(unsigned val) const {
    char buffer[24];
    _snprintf(buffer, sizeof(buffer), "%d", val);
    return *this + buffer;
  }

  size_t length() const {
    return length_;
  }

  bool operator !() const {
    return !length_;
  }

  const char *ptr() const {
    return ptr_ ? ptr_ : "";
  }
  operator const char *() const {
    return ptr();
  }

private:
  void assign(const char *ptr) {
    if (!ptr) {
      ptr_ = nullptr;
      length_ = 0;
      return;
    }
    assign(ptr, strlen(ptr));
  }
  void assign(const char *ptr, size_t length) {
    if (!ptr) {
      ptr_ = nullptr;
      length_ = 0;
      return;
    }
    length_ = length;
    ptr_ = (char *)malloc(length_ + 1);
    memcpy(ptr_, ptr, length_);
    ptr_[length_] = '\0';
  }

private:
  char *ptr_;
  size_t length_;
};

};

#endif // _include_spcomp_auto_string_h_
