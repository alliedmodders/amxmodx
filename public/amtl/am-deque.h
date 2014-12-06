// vim: set sts=8 ts=2 sw=2 tw=99 et:
//
// Copyright (C) 2014, David Anderson and AlliedModders LLC
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//  * Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//  * Neither the name of AlliedModders LLC nor the names of its contributors
//    may be used to endorse or promote products derived from this software
//    without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
#ifndef _INCLUDE_KEIMA_TPL_CPP_DEQUE_H_
#define _INCLUDE_KEIMA_TPL_CPP_DEQUE_H_

#include <new>
#include <stdlib.h>
#include <assert.h>
#include <am-cxx.h>
#include <am-allocator-policies.h>
#include <am-utility.h>
#include <am-moveable.h>

namespace ke {

template <typename T, typename AllocPolicy = SystemAllocatorPolicy>
class Deque : public AllocPolicy
{
  static const size_t kInvalidIndex = ~size_t(0);

 public:
  Deque(AllocPolicy = AllocPolicy())
   : buffer_(NULL),
     maxlength_(0),
     first_(0),
     last_(0)
  {
  }
  Deque(Deque &&other)
   : buffer_(other.buffer_),
     maxlength_(other.maxlength_),
     first_(other.first_),
     last_(other.last_)
  {
    other.reset();
  }
  ~Deque() {
    zap();
  }

  Deque &operator =(Deque &&other) {
    zap();
    buffer_ = other.buffer_;
    maxlength_ = other.maxlength_;
    first_ = other.first_;
    last_ = other.last_;
    other.reset();
  }

  bool empty() const {
    return first_ == last_;
  }

  template <typename U>
  bool append(U &&other) {
    size_t next = ensureCanAppend();
    if (next == kInvalidIndex)
      return false;
    new (&buffer_[last_]) T(ke::Forward<U>(other));
    last_ = next;
    return true;
  }

  template <typename U>
  bool prepend(U &&other) {
    size_t prev = ensureCanPrepend();
    if (prev == kInvalidIndex)
      return false;
    first_ = prev;
    new (&buffer_[first_]) T(ke::Forward<U>(other));
    return true;
  }

  void popFront() {
    assert(!empty());
    buffer_[first_].~T();
    if (first_ == maxlength_ - 1)
      first_ = 0;
    else
      first_++;
  }
  void popBack() {
    assert(!empty());
    if (last_ == 0)
      last_ = maxlength_ - 1;
    else
      last_--;
    buffer_[last_].~T();
  }

  T popFrontCopy() {
    T t = front();
    popFront();
    return t;
  }
  T popBackCopy() {
    T t = back();
    popBack();
    return t;
  }

  const T &front() const {
    assert(!empty());
    return buffer_[first_];
  }
  T &front() {
    assert(!empty());
    return buffer_[first_];
  }
  const T &back() const {
    assert(!empty());
    if (last_ == 0)
      return buffer_[maxlength_ - 1];
    return buffer_[last_ - 1];
  }
  T &back() {
    assert(!empty());
    if (last_ == 0)
      return buffer_[maxlength_ - 1];
    return buffer_[last_ - 1];
  }

  size_t length() const {
    if (first_ == last_)
      return 0;
    return first_ < last_
           ? (last_ - first_)
           : (last_ + (maxlength_ - first_));
  }
  size_t capacity() const {
    return maxlength_;
  }

 private:
  Deque(const Deque<T> &other) KE_DELETE;
  Deque &operator =(const Deque<T> &other) KE_DELETE;

  // Return the next value of first_.
  size_t ensureCanPrepend() {
    if (first_ == 0) {
      if (maxlength_ && (last_ != maxlength_ - 1))
        return maxlength_ - 1;
    } else if (first_ - 1 != last_) {
      return first_ - 1;
    }

    // The ring is full.
    if (!growByOne())
      return kInvalidIndex;
    return maxlength_ - 1;
  }

  // Return the next value of last_.
  size_t ensureCanAppend() {
    if (last_ < first_) {
      if (last_ + 1 != first_)
        return last_ + 1;
    } else{
      if (last_ + 1 < maxlength_)
        return last_ + 1;
      if (first_ != 0)
        return 0;
    }

    // The ring is full.
    if (!growByOne())
      return kInvalidIndex;
    return last_ + 1;
  }

  bool growByOne() {
    if (!IsUintPtrMultiplySafe(maxlength_, 2)) {
      this->reportAllocationOverflow();
      return false;
    }

    size_t new_maxlength = maxlength_ ? maxlength_ * 2 : 8;
    T *new_buffer = (T *)this->malloc(sizeof(T) * new_maxlength);
    if (!new_buffer)
      return false;

    // Move everything to the bottom of the new buffer, and reset our indices
    // so that first is at 0.
    if (first_ < last_) {
      MoveRange(new_buffer, buffer_ + first_, last_ - first_);
      last_ = last_ - first_;
      first_ = 0;
    } else {
      MoveRange(new_buffer, buffer_ + first_, maxlength_ - first_);
      MoveRange(new_buffer + (maxlength_ - first_), buffer_, last_);
      last_ = last_ + (maxlength_ - first_);
      first_ = 0;
    }
    this->free(buffer_);

    buffer_ = new_buffer;
    maxlength_ = new_maxlength;
    return true;
  }

  void reset() {
    buffer_ = NULL;
    maxlength_ = 0;
    first_ = 0;
    last_ = 0;
  }

  void zap() {
    if (first_ < last_) {
      for (size_t i = first_; i < last_; i++)
        buffer_[i].~T();
    } else {
      for (size_t i = first_; i < maxlength_; i++)
        buffer_[i].~T();
      for (size_t i = 0; i < last_; i++)
        buffer_[i].~T();
    }
    this->free(buffer_);
  }

 private:
  T *buffer_;
  size_t maxlength_;

  // Always points to the first readable item.
  size_t first_;

  // Always points to where the next item can be appended.
  size_t last_;
};

}

#endif // _INCLUDE_KEIMA_TPL_CPP_DEQUE_H_
