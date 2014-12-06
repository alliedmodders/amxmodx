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

#ifndef _include_amtl_ts_refcounting_h_
#define _include_amtl_ts_refcounting_h_

#include <am-refcounting.h>
#include <am-atomics.h>

namespace ke {

// See the comment above Refcounted<T> for more information. This class is
// identical, except changing the reference count is guaranteed to be atomic
// with respect to other threads changing the reference count.
template <typename T>
class KE_LINK RefcountedThreadsafe
{
  public:
    RefcountedThreadsafe()
     : refcount_(0)
    {
    }

    void AddRef() {
      refcount_.increment();
    }
    bool Release() {
        if (!refcount_.decrement()) {
            delete static_cast<T *>(this);
            return false;
        }
        return true;
    }

  protected:
    ~RefcountedThreadsafe() {
    }

  private:
    AtomicRefcount refcount_;
};

// Use this to forward to ke::Refcounted<X>, when implementing IRefcounted.
#define KE_IMPL_REFCOUNTING_TS(classname)                                     \
   void AddRef() {                                                            \
     ke::RefcountedThreadsafe<classname>::AddRef();                           \
   }                                                                          \
   void Release() {                                                           \
     ke::RefcountedThreadsafe<classname>::Release();                          \
   }

// Classes may be multiply-inherited may wish to derive from this Refcounted
// instead.
class VirtualRefcountedThreadsafe : public IRefcounted
{
 public:
  VirtualRefcountedThreadsafe() : refcount_(0)
  {
#if !defined(NDEBUG)
    destroying_ = false;
#endif
  }
  virtual ~VirtualRefcountedThreadsafe()
  {}
  void AddRef() KE_OVERRIDE {
    assert(!destroying_);
    refcount_.increment();
  }
  void Release() KE_OVERRIDE {
    if (!refcount_.decrement()) {
#if !defined(NDEBUG)
      destroying_ = true;
#endif
      delete this;
    }
  }

 private:
  AtomicRefcount refcount_;
#if !defined(NDEBUG)
  bool destroying_;
#endif
};

// This is a specialized version of Ref<> that is safe to read and write from
// multiple threads. It is not recommended for general use, since it imposes
// a CAS spin-lock on every read/write.
//
// Normally, assigning Ref<> to Ref<> has a race condition where in between
// the read and incref, another thread can assign, decref, and ultimately
// destroy the left-hand side. This prevents such a scenario by making reads
// atomic with respect to the incref operation.
//
// Pointers stored in an AtomicRef<> must be at least sizeof(void *) aligned.
template <typename T>
class AtomicRef
{
 public:
  AtomicRef()
   : thing_(nullptr)
  {}
  AtomicRef(T *thing)
   : thing_(thing)
  {
    assert(IsAligned(thing, sizeof(void *)));
    if (thing)
      thing->AddRef();
  }
  ~AtomicRef() {
    assert(thing_ == untagged(thing_));
    if (thing_)
      reinterpret_cast<T *>(thing_)->Release();
  }

  // Atomically retrieve and add a reference to the contained value.
  AlreadyRefed<T> get() {
    T *value = lock();
    if (value)
      value->AddRef();
    unlock(value);
    return AdoptRef(value);
  }

  // Atomically incref the new value and replace the old value.
  void operator =(T *other) {
    T *value = lock();
    if (other)
      other->AddRef();
    unlock(other);
    if (value)
      value->Release();
  }

 private:
  AtomicRef(const AtomicRef &other) KE_DELETE;
  void operator =(const AtomicRef &other) KE_DELETE;

 private:
  // We represent a locked state with a tag bit.
  void *tagged(void *ptr) {
    return reinterpret_cast<void *>(uintptr_t(ptr) | 1);
  }
  void *untagged(void *ptr) {
    return reinterpret_cast<void *>(uintptr_t(ptr) & ~uintptr_t(1));
  }

  T *lock() {
    // Spin until we can replace an untagged ptr with the tagged version.
    void *oldval = untagged(thing_);
    while (CompareAndSwapPtr(&thing_, tagged(oldval), oldval) != oldval) {
      YieldProcessor();
      oldval = untagged(thing_);
    }
    return reinterpret_cast<T *>(oldval);
  }
  void unlock(T *ptr) {
    // Nothing should have mutated the value, and the new value should be
    // untagged.
    assert(thing_ == tagged(thing_));
    assert(ptr == untagged(ptr));
    thing_ = ptr;
  }

 private:
  void * volatile thing_;
};

} // namespace ke

#endif // _include_amtl_ts_refcounting_h_
