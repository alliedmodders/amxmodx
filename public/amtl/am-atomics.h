// vim: set sts=8 ts=2 sw=2 tw=99 et:
//
// Copyright (C) 2013, David Anderson and AlliedModders LLC
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

#ifndef _include_amtl_atomics_h_
#define _include_amtl_atomics_h_

#include <am-utility.h>

namespace ke {

#if defined(_MSC_VER)
extern "C" {
  long __cdecl _InterlockedIncrement(long volatile *dest);
  long __cdecl _InterlockedDecrement(long volatile *dest);
  long long __cdecl _InterlockedIncrement64(long long volatile *dest);
  long long __cdecl _InterlockedDecrement64(long long volatile *dest);
  long __cdecl _InterlockedCompareExchange(long volatile *dest, long exchange, long comparand);
# if _MSC_VER > 1600 || (_MSC_VER == 1600 && !defined(_M_IX86))
  void * __cdecl _InterlockedCompareExchangePointer(
     void * volatile *Destination,
     void * Exchange,
     void * Comparand
  );
#else
  static inline void * _InterlockedCompareExchangePointer(
     void * volatile *Destination,
     void * Exchange,
     void * Comparand)
  {
    return (void *)_InterlockedCompareExchange((long volatile *)Destination, (long)Exchange, (long)Comparand);
  }
#endif
}
# pragma intrinsic(_InterlockedIncrement)
# pragma intrinsic(_InterlockedDecrement)
# pragma intrinsic(_InterlockedCompareExchange)
# if _MSC_VER > 1600 || (_MSC_VER == 1600 && !defined(_M_IX86))
#  pragma intrinsic(_InterlockedCompareExchangePointer)
# endif
# if defined(_WIN64)
#  pragma intrinsic(_InterlockedIncrement64)
#  pragma intrinsic(_InterlockedDecrement64)
# endif
#endif

#if defined(__GNUC__)
# if defined(i386) || defined(__x86_64__)
#  if defined(__clang__)
    static inline void YieldProcessor() { asm("pause"); }
#  else
#   if KE_GCC_AT_LEAST(4, 7)
#    define YieldProcessor() __builtin_ia32_pause()
#   else
    static inline void YieldProcessor() { asm("pause"); }
#   endif
#  endif
# else
#  define YieldProcessor()
# endif
#elif defined(_MSC_VER)
# if !defined(YieldProcessor)
#  define YieldProcessor _mm_pause
# endif
#endif

#if defined(_MSC_VER)
static inline void *
CompareAndSwapPtr(void *volatile *Destination, void *Exchange, void *Comparand)
{
  return _InterlockedCompareExchangePointer(Destination, Exchange, Comparand);
}
#else
static inline void *
CompareAndSwapPtr(void *volatile *dest, void *newval, void *oldval)
{
  return __sync_val_compare_and_swap(dest, oldval, newval);
}
#endif

template <size_t Width>
struct AtomicOps;

template <>
struct AtomicOps<4>
{
#if defined(_MSC_VER)
  typedef volatile long Type;

  static Type Increment(Type *ptr) {
    return _InterlockedIncrement(ptr);
  }
  static Type Decrement(Type *ptr) {
    return _InterlockedDecrement(ptr);
  };
#elif defined(__GNUC__)
  typedef volatile int Type;

  // x86/x64 notes: When using GCC < 4.8, this will compile to a spinlock.
  // On 4.8+, or when using Clang, we'll get the more optimal "lock addl"
  // variant.
  static Type Increment(Type *ptr) {
    return __sync_add_and_fetch(ptr, 1);
  }
  static Type Decrement(Type *ptr) {
    return __sync_sub_and_fetch(ptr, 1);
  }
#endif
};

template <>
struct AtomicOps<8>
{
#if defined(_MSC_VER)
  typedef volatile long long Type;

  static Type Increment(Type *ptr) {
    return _InterlockedIncrement64(ptr);
  }
  static Type Decrement(Type *ptr) {
    return _InterlockedDecrement64(ptr);
  };
#elif defined(__GNUC__)
  typedef volatile int64_t Type;

  // x86/x64 notes: When using GCC < 4.8, this will compile to a spinlock.
  // On 4.8+, or when using Clang, we'll get the more optimal "lock addl"
  // variant.
  static Type Increment(Type *ptr) {
    return __sync_add_and_fetch(ptr, 1);
  }
  static Type Decrement(Type *ptr) {
    return __sync_sub_and_fetch(ptr, 1);
  }
#endif
};

class KE_LINK AtomicRefcount
{
  typedef AtomicOps<sizeof(uintptr_t)> Ops;

 public:
  AtomicRefcount(uintptr_t value)
   : value_(value)
  {
  }

  void increment() {
    Ops::Increment(&value_);
  }

  // Return false if all references are gone.
  bool decrement() {
    return Ops::Decrement(&value_) != 0;
  }

 private:
  Ops::Type value_;
};

}

#endif // _include_amtl_atomics_h_

