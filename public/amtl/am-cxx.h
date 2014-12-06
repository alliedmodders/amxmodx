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
#ifndef _include_amtl_cxx_support_h_
#define _include_amtl_cxx_support_h_

#if defined(__clang__)
# if !(defined(__clang_major__) && defined(__clang_minor__))
#  define KE_CLANG_MAJOR 1
#  define KE_CLANG_MINOR __GNUC_MINOR__
# else
#  if defined(__apple_build_version__) && clang_major__ > 3
    // 4.0 => 3.1, 4.1 => 3.2
#   if __clang_major__ == 4
#    define KE_CLANG_MAJOR 3
#    if __clang_minor__ == 0
#     define KE_CLANG_MINOR 1
#    else
#     define KE_CLANG_MINOR 2
#    endif
    // 5.0 => 3.3, 5.1 => 3.4
#   elif __clang_major__ == 5
#    define KE_CLANG_MAJOR 3
#    if __clang_minor__ == 0
#     define KE_CLANG_MINOR 3
#    else
#     define KE_CLANG_MINOR 4
#    endif
#   elif __clang_major__ == 6
#    define KE_CLANG_MAJOR 3
#    define KE_CLANG_MINOR 5
#   endif
#  endif
#  if !defined(KE_CLANG_MAJOR)
#   define KE_CLANG_MAJOR __clang_major__
#  endif
#  if !defined(KE_CLANG_MINOR)
#   define KE_CLANG_MINOR __clang_minor__
#  endif
# endif

// Done with horrible clang version detection.
# define KE_CLANG_AT_LEAST(x, y) \
   ((__clang_major__ > (x)) || (__clang_major__ == x && __clang_minor__ >= y))

# if KE_CLANG_AT_LEAST(2, 9)
#  define KE_CXX_HAS_RVAL_REFS 30
#  define KE_CXX_HAS_DELETE
#  define KE_CXX_HAS_STATIC_ASSERT
#  define KE_CXX_HAS_DOUBLE_GT
#  define KE_CXX_HAS_ENUM_CLASS
# endif
# if KE_CLANG_AT_LEAST(3, 0)
#  define KE_CXX_HAS_OVERRIDE
#  define KE_CXX_HAS_EXPLICIT_BOOL
#  define KE_CXX_HAS_NULLPTR
#  define KE_CXX_HAS_NOEXCEPT
# endif
# if KE_CLANG_AT_LEAST(3, 1)
#  define KE_CXX_HAS_CONSTEXPR
# endif

#elif defined(__GNUC__)
# define KE_GCC_AT_LEAST(x, y) ((__GNUC__ > (x)) || (__GNUC__ == x && __GNUC_MINOR__ >= y))

# if KE_GCC_AT_LEAST(4, 3)
#  define KE_CXX_HAS_RVAL_REFS 10
#  define KE_CXX_HAS_STATIC_ASSERT
#  define KE_CXX_HAS_DOUBLE_GT
# endif
# if KE_GCC_AT_LEAST(4, 4)
#  define KE_CXX_HAS_DELETE
#  define KE_CXX_HAS_ENUM_CLASS
# endif
# if KE_GCC_AT_LEAST(4, 5)
#  define KE_CXX_HAS_EXPLICIT_BOOL
#  undef KE_CXX_HAS_RVAL_REFS
#  define KE_CXX_HAS_RVAL_REFS 21
# endif
# if KE_GCC_AT_LEAST(4, 6)
#  define KE_CXX_HAS_NULLPTR
#  define KE_CXX_HAS_NOEXCEPT
#  define KE_CXX_HAS_CONSTEXPR
#  undef KE_CXX_HAS_RVAL_REFS
#  define KE_CXX_HAS_RVAL_REFS 30
# endif
# if KE_GCC_AT_LEAST(4, 7)
#  define KE_CXX_HAS_OVERRIDE
# endif

#elif defined(_MSC_VER)
# if _MSC_VER >= 1600
#  define KE_CXX_HAS_RVAL_REFS 20
#  define KE_CXX_HAS_STATIC_ASSERT
#  define KE_CXX_HAS_DOUBLE_GT
#  define KE_CXX_HAS_NULLPTR
# endif
# if _MSC_VER >= 1700
#  undef KE_CXX_HAS_RVAL_REFS
#  define KE_CXX_HAS_RVAL_REFS 21
#  define KE_CXX_HAS_OVERRIDE
#  define KE_CXX_HAS_ENUM_CLASS
# endif
# if _MSC_VER >= 1800
#  define KE_CXX_HAS_DELETE
#  define KE_CXX_HAS_EXPLICIT_BOOL
# endif
# if _MSC_VER == 1800 && _MSC_FULL_VER == 180021114
#  define KE_CXX_HAS_CONSTEXPR
# endif
#else
# error Unrecognized compiler.
#endif

// Done with compiler feature detection.

#if defined(KE_CXX_HAS_OVERRIDE)
# define KE_OVERRIDE override
#else
# define KE_OVERRIDE
#endif

#if defined(KE_CXX_HAS_DELETE)
# define KE_DELETE = delete
#else
# define KE_DELETE
#endif

#if defined(KE_CXX_HAS_NOEXCEPT)
# define KE_NOEXCEPT noexcept
#else
# define KE_NOEXCEPT
#endif

#if defined(KE_CXX_HAS_CONSTEXPR)
# define KE_CONSTEXPR constexpr
#else
# define KE_CONSTEXPR
#endif

#if defined(KE_CXX_HAS_STATIC_ASSERT)
# define KE_STATIC_ASSERT(cond) static_assert(cond, #cond)
#else
# define KE_STATIC_ASSERT(cond) extern int static_assert_f(int a[(cond) ? 1 : -1])
#endif

#if !defined(KE_CXX_HAS_RVAL_REFS) || KE_CXX_HAS_RVAL_REFS < 21
//# error AMTL requires rvalue reference 2.1 support (N2844+)
#endif
#if !defined(KE_CXX_HAS_DOUBLE_GT)
# error AMTL requires support for >> in template names
#endif
#if !defined(KE_CXX_HAS_NULLPTR)
# if defined(__GNUC__) && !defined(__clang__)
#  define nullptr __null
#  define KE_CXX_HAS_NULLPTR
# else
#  error AMTL requires nullptr support
# endif
#endif

#define KE_DEFINE_ENUM_OPERATORS(EnumName)                                          \
  static inline EnumName operator |(const EnumName &left, const EnumName &right) {  \
    return EnumName(uint32_t(left) | uint32_t(right));                              \
  }                                                                                 \
  static inline EnumName operator &(const EnumName &left, const EnumName &right) {  \
    return EnumName(uint32_t(left) & uint32_t(right));                              \
  }                                                                                 \
  static inline EnumName operator ^(const EnumName &left, const EnumName &right) {  \
    return EnumName(uint32_t(left) ^ uint32_t(right));                              \
  }                                                                                 \
  static inline EnumName operator ~(const EnumName &flags) {                        \
    return EnumName(~uint32_t(flags));                                              \
  }                                                                                 \
  static inline EnumName & operator |=(EnumName &left, const EnumName &right) {     \
    return left = left | right;                                                     \
  }                                                                                 \
  static inline EnumName & operator &=(EnumName &left, const EnumName &right) {     \
    return left = left & right;                                                     \
  }                                                                                 \
  static inline EnumName & operator ^=(EnumName &left, const EnumName &right) {     \
    return left = left ^ right;                                                     \
  }                                                                                 \
  static inline bool operator !(const EnumName &obj) {                              \
    return uint32_t(obj) == 0;                                                      \
  }

#endif // _include_amtl_cxx_support_h_
