// vim: set sts=8 ts=2 sw=2 tw=99 et:
//
// Copyright (C) 2013, David Anderson and AlliedModders LLC
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//  * Redistributions of source code must retain the above copyright notice, this
//  list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above copyright notice,
//  this list of conditions and the following disclaimer in the documentation
//  and/or other materials provided with the distribution.
//  * Neither the name of AlliedModders LLC nor the names of its contributors
//  may be used to endorse or promote products derived from this software
//  without specific prior written permission.
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

#ifndef _include_amtl_hashmap_h_
#define _include_amtl_hashmap_h_

#include <am-hashtable.h>

namespace ke {

// Template parameters:
//
// K - Key type.
// V - Value type.
// HashPolicy - A struct with a hash and comparator function for each lookup type:
//     static uint32_t hash(const Type &value);
//     static bool matches(const Type &value, const K &key);
//
// All types that match a given key, must compute the same hash.
//
// Note that like HashTable, a HashMap is not usable until init() has been called.
template <typename K,
          typename V,
          typename HashPolicy,
          typename AllocPolicy = SystemAllocatorPolicy>
class HashMap : public AllocPolicy
{
 private:
  struct Entry
  {
    K key;
    V value;

    Entry()
    {
    }
    Entry(Moveable<Entry> other)
      : key(Moveable<K>(other->key)),
        value(Moveable<V>(other->value))
    {
    }

    Entry(const K &aKey, const V &aValue)
     : key(aKey),
       value(aValue)
    { }
    Entry(const K &aKey, Moveable<V> aValue)
     : key(aKey),
       value(aValue)
    { }
    Entry(Moveable<K> aKey, const V &aValue)
     : key(aKey),
       value(aValue)
    { }
    Entry(Moveable<K> aKey, Moveable<V> aValue)
     : key(aKey),
       value(aValue)
    { }
  };

  struct Policy
  {
    typedef Entry Payload;

    template <typename Lookup>
    static uint32_t hash(const Lookup &key) {
      return HashPolicy::hash(key);
    }

    template <typename Lookup>
    static bool matches(const Lookup &key, const Payload &payload) {
      return HashPolicy::matches(key, payload.key);
    }
  };

  typedef HashTable<Policy, AllocPolicy> Internal;

 public:
  HashMap(AllocPolicy ap = AllocPolicy())
    : table_(ap)
  {
  }

  // capacity must be a power of two.
  bool init(size_t capacity = 16) {
    return table_.init(capacity);
  }

  typedef typename Internal::Result Result;
  typedef typename Internal::Insert Insert;
  typedef typename Internal::iterator iterator;

  template <typename Lookup>
  Result find(const Lookup &key) {
    return table_.find(key);
  }

  template <typename Lookup>
  Insert findForAdd(const Lookup &key) {
    return table_.findForAdd(key);
  }

  template <typename Lookup>
  void removeIfExists(const Lookup &key) {
    return table_.remove(key);
  }

  void remove(Result &r) {
    table_.remove(r);
  }

  // The map must not have been mutated in between findForAdd() and add().
  // The Insert object is still valid after add() returns, however.
  bool add(Insert &i, const K &key, const V &value) {
    return table_.add(i, Entry(key, value));
  }
  bool add(Insert &i, Moveable<K> key, const V &value) {
    return table_.add(i, Entry(key, value));
  }
  bool add(Insert &i, const K &key, Moveable<V> value) {
    return table_.add(i, Entry(key, value));
  }
  bool add(Insert &i, Moveable<K> key, Moveable<V> value) {
    return table_.add(i, Entry(key, value));
  }
  bool add(Insert &i, Moveable<K> key) {
    return table_.add(i, Entry(key, V()));
  }

  // This can be used to avoid compiler constructed temporaries, since AMTL
  // does not yet support move semantics. If you use this, the key and value
  // must be set after.
  bool add(Insert &i) {
    return table_.add(i);
  }

  iterator iter() {
    return iterator(&table_);
  }

  void clear() {
    table_.clear();
  }

  size_t elements() const {
    return table_.elements();
  }

  size_t estimateMemoryUse() const {
    return table_.estimateMemoryUse();
  }

 private:
  Internal table_;
};

template <typename T>
struct PointerPolicy
{
  static inline uint32_t hash(T *p) {
    return HashPointer(p);
  }
  static inline bool matches(T *p1, T *p2) {
    return p1 == p2;
  }
};

}

#endif // _include_amtl_hashmap_h_
