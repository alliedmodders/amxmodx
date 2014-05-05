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

#ifndef _include_amtl_inline_list_h_
#define _include_amtl_inline_list_h_

#include <stddef.h>
#include <assert.h>

namespace ke {

template <typename T> class InlineList;

// Objects can recursively inherit from InlineListNode in order to have
// membership in an InlineList<T>.
template <typename T>
class InlineListNode
{
  friend class InlineList<T>;

  public:
  InlineListNode()
   : next_(NULL),
     prev_(NULL)
  {
  }

  InlineListNode(InlineListNode *next, InlineListNode *prev)
   : next_(next),
     prev_(prev)
  {
  }

  protected:
  InlineListNode *next_;
  InlineListNode *prev_;
};

// An InlineList is a linked list that threads link pointers through objects,
// rather than allocating node memory. A node can be in at most one list at
// any time.
//
// Since InlineLists are designed to be very cheap, there is no requirement
// that elements be removed from a list once the list is destructed. However,
// for as long as the list is alive, all of its contained nodes must also
// be alive.
template <typename T>
class InlineList
{
  typedef InlineListNode<T> Node;

  Node head_;

  // Work around a clang bug where we can't initialize with &head_ in the ctor.
  inline Node *head() {
    return &head_;
  }

 public:
  InlineList()
    : head_(head(), head())
  {
  }

  ~InlineList()
  {
#if !defined(NDEBUG)
    // Remove all items to clear their next/prev fields.
    while (begin() != end())
      remove(*begin());
#endif
  }

 public:
  class iterator
  {
    friend class InlineList;
    Node *iter_;

   public:
    iterator(Node *iter)
      : iter_(iter)
    {
    }

    iterator & operator ++() {
      iter_ = iter_->next;
      return *this;
    }
    iterator operator ++(int) {
      iterator old(*this);
      iter_ = iter_->next_;
      return old;
    }
    T * operator *() {
      return static_cast<T *>(iter_);
    }
    T * operator ->() {
      return static_cast<T *>(iter_);
    }
    bool operator !=(const iterator &where) const {
      return iter_ != where.iter_;
    }
    bool operator ==(const iterator &where) const {
      return iter_ == where.iter_;
    }
  };

  iterator begin() {
    return iterator(head_.next_);
  }

  iterator end() {
    return iterator(&head_);
  }

  iterator erase(iterator &at) {
    iterator next = at;
    next++;

    remove(at.iter_);

    // Iterator is no longer valid.
    at.iter_ = NULL;

    return next;
  }

  bool empty() const {
    return head_.next_ == &head_;
  }

  void remove(Node *t) {
    t->prev_->next_ = t->next_;
    t->next_->prev_ = t->prev_;

#if !defined(NDEBUG)
    t->next_ = NULL;
    t->prev_ = NULL;
#endif
  }

  void append(Node *t) {
    assert(!t->next_);
    assert(!t->prev_);

    t->prev_ = head_.prev_;
    t->next_ = &head_;
    head_.prev_->next_ = t;
    head_.prev_ = t;
  }
};

}

#endif // _include_amtl_inline_list_h_

