// vim: set sts=8 ts=2 sw=2 tw=99 et:
//
// Copyright (C) 2013-2014, David Anderson and AlliedModders LLC
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

#ifndef _include_amtl_utility_h_
#define _include_amtl_utility_h_

#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#if defined(_MSC_VER)
# include <intrin.h>
#else
# include <inttypes.h>
#endif
#include <am-moveable.h>
#include <am-cxx.h>
#include <am-algorithm.h>

#if defined(_MSC_VER)
// Mac file format warning.
# pragma warning(disable:4355)
#endif

namespace ke {

static const size_t kMallocAlignment = sizeof(void *) * 2;

static const size_t kKB = 1024;
static const size_t kMB = 1024 * kKB;
static const size_t kGB = 1024 * kMB;

template <typename T> T
ReturnAndVoid(T &t)
{
    T saved = t;
    t = T();
    return saved;
}

// Wrapper that automatically deletes its contents. The pointer can be taken
// to avoid destruction.
template <typename T>
class AutoPtr
{
 public:
 AutoPtr()
   : t_(nullptr)
 {
 }
 explicit AutoPtr(T *t)
   : t_(t)
 {
 }
 AutoPtr(AutoPtr &&other)
 {
     t_ = other.t_;
     other.t_ = nullptr;
 }
 ~AutoPtr() {
     delete t_;
 }
 T *get() {
   return t_;
 }
 T *take() {
     return ReturnAndVoid(t_);
 }
 T *forget() {
     return ReturnAndVoid(t_);
 }
 T *operator *() const {
     return t_;
 }
 T *operator ->() const {
     return t_;
 }
 operator T *() const {
     return t_;
 }
 T *operator =(T *t) {
     delete t_;
     t_ = t;
     return t_;
 }
 T **address() {
   return &t_;
 }
 T *operator =(AutoPtr &&other) {
     delete t_;
     t_ = other.t_;
     other.t_ = nullptr;
     return t_;
 }
 bool operator !() const {
     return !t_;
 }

 private:
  AutoPtr(const AutoPtr &other) KE_DELETE;
  AutoPtr &operator =(const AutoPtr &other) KE_DELETE;

 private:
  T *t_;
};

// Wrapper that automatically deletes its contents. The pointer can be taken
// to avoid destruction.
template <typename T>
class AutoArray
{
    T *t_;

  public:
    AutoArray()
      : t_(nullptr)
    {
    }
    explicit AutoArray(T *t)
      : t_(t)
    {
    }
    ~AutoArray() {
        delete [] t_;
    }
    T *take() {
        return ReturnAndVoid(t_);
    }
    T *forget() {
        return ReturnAndVoid(t_);
    }
    T **address() {
      return &t_;
    }
    T &operator *() const {
        return t_;
    }
    operator T *() const {
        return t_;
    }
    void operator =(T *t) {
        delete [] t_;
        t_ = t;
    }
    bool operator !() const {
        return !t_;
    }
};

static inline size_t
Log2(size_t number)
{
    assert(number != 0);

#ifdef _MSC_VER
    unsigned long rval;
# ifdef _M_IX86
    _BitScanReverse(&rval, number);
# elif _M_X64
    _BitScanReverse64(&rval, number);
# endif
    return rval;
#else
    size_t bit;
    asm("bsr %1, %0\n"
        : "=r" (bit)
        : "rm" (number));
    return bit;
#endif
}

static inline size_t
FindRightmostBit(size_t number)
{
    assert(number != 0);

#ifdef _MSC_VER
    unsigned long rval;
# ifdef _M_IX86
    _BitScanForward(&rval, number);
# elif _M_X64
    _BitScanForward64(&rval, number);
# endif
    return rval;
#else
    size_t bit;
    asm("bsf %1, %0\n"
        : "=r" (bit)
        : "rm" (number));
    return bit;
#endif
}

static inline bool
IsPowerOfTwo(size_t value)
{
    if (value == 0)
        return false;
    return !(value & (value - 1));
}

static inline size_t
Align(size_t count, size_t alignment)
{
    assert(IsPowerOfTwo(alignment));
    return count + (alignment - (count % alignment)) % alignment;
}

static inline bool
IsUint32AddSafe(unsigned a, unsigned b)
{
    if (!a || !b)
        return true;
    size_t log2_a = Log2(a);
    size_t log2_b = Log2(b);
    return (log2_a < sizeof(unsigned) * 8) &&
           (log2_b < sizeof(unsigned) * 8);
}

static inline bool
IsUintPtrAddSafe(size_t a, size_t b)
{
    if (!a || !b)
        return true;
    size_t log2_a = Log2(a);
    size_t log2_b = Log2(b);
    return (log2_a < sizeof(size_t) * 8) &&
           (log2_b < sizeof(size_t) * 8);
}

static inline bool
IsUint32MultiplySafe(unsigned a, unsigned b)
{
    if (a <= 1 || b <= 1)
        return true;

    size_t log2_a = Log2(a);
    size_t log2_b = Log2(b);
    return log2_a + log2_b <= sizeof(unsigned) * 8;
}

static inline bool
IsUintPtrMultiplySafe(size_t a, size_t b)
{
    if (a <= 1 || b <= 1)
        return true;

    size_t log2_a = Log2(a);
    size_t log2_b = Log2(b);
    return log2_a + log2_b <= sizeof(size_t) * 8;
}

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof(array[0]))
#define IS_ALIGNED(addr, alignment)    (!(uintptr_t(addr) & ((alignment) - 1)))

template <typename T>
static inline bool
IsAligned(T addr, size_t alignment)
{
    assert(IsPowerOfTwo(alignment));
    return !(uintptr_t(addr) & (alignment - 1));
}

static inline void *
AlignedBase(void *addr, size_t alignment)
{
    assert(IsPowerOfTwo(alignment));
    return reinterpret_cast<void *>(uintptr_t(addr) & ~(alignment - 1));
}

template <typename T>
class StorageBuffer
{
 public:
  T *address() {
    return reinterpret_cast<T *>(buffer_);
  }
  const T *address() const {
    return reinterpret_cast<const T *>(buffer_);
  }

 private:
  union {
    char buffer_[sizeof(T)];
    uint64_t aligned_;
  };
};

template <typename T>
class SaveAndSet
{
 public:
  SaveAndSet(T *location, const T &value)
   : location_(location),
     old_(*location)
  {
    *location_ = value;
  }
  ~SaveAndSet() {
    *location_ = old_;
  }

 private:
  T *location_;
  T old_;
};

template <typename T>
class Maybe
{
 public:
  Maybe()
   : initialized_(false)
  {}
  ~Maybe() {
    if (initialized_)
      t_.address()->~T();
  }

  void init() {
    new (t_.address()) T();
    initialized_ = true;
  }
  template <typename U>
  void init(U &&u) {
    new (t_.address()) T(Forward<U>(u));
    initialized_ = true;
  }
  bool initialized() const {
    return initialized_;
  }

 private:
  bool initialized_;
  StorageBuffer<T> t_;
};

template <typename T>
class StackLinked
{
 public:
  StackLinked<T>(T **prevp)
   : prevp_(prevp),
     prev_(*prevp)
  {
    *prevp_ = static_cast<T *>(this);
  }
  virtual ~StackLinked() {
    assert(*prevp_ == this);
    *prevp_ = prev_;
  }

 protected:
  T **prevp_;
  T *prev_;
};

#if defined(_MSC_VER)
# define KE_FMT_SIZET           "Iu"
# define KE_FMT_I64             "I64d"
# define KE_FMT_U64             "I64u"
#elif defined(__GNUC__)
# define KE_FMT_SIZET           "zu"
# define KE_FMT_I64             PRId64
# define KE_FMT_U64             PRIu64
#else
# error "Implement format specifier string"
#endif

#if defined(__GNUC__)
# define KE_CRITICAL_LIKELY(x)  __builtin_expect(!!(x), 1)
#else
# define KE_CRITICAL_LIKELY(x)  x
#endif

#if defined(_WIN32)
# define KE_IMPORT __declspec(dllimport)
# define KE_EXPORT __declspec(dllexport)
#else
# define KE_IMPORT
# define KE_EXPORT __attribute__((visibility("default")))
#endif

#if defined(KE_EXPORTING)
# define KE_LINK KE_EXPORT
#elif defined(KE_IMPORTING)
# define KE_LINK KE_IMPORT
#else
# define KE_LINK
#endif

} // namespace ke

#endif // _include_amtl_utility_h_
