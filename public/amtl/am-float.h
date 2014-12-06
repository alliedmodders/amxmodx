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

#ifndef _include_amtl_float_h_
#define _include_amtl_float_h_

#include <math.h>
#include <float.h>

namespace ke {

static const uint32_t kFloat32ExponentMask = 0x7F800000;
static const uint64_t kFloat64ExponentMask = 0x7FFF000000000000ULL;

struct float32_bits
{
  static const uint32_t kExponentMask = kFloat32ExponentMask;

  typedef uint32_t Bits;

  union layout {
    uint32_t bits;
    float value;
  };

  static layout to_layout(float value) {
    layout impl;
    impl.value = value;
    return impl;
  }
};

struct float64_bits
{
  static const uint64_t kExponentMask = kFloat64ExponentMask;

  typedef uint64_t Bits;

  union layout {
    uint64_t bits;
    float value;
  };

  static layout to_layout(float value) {
    layout impl;
    impl.value = value;
    return impl;
  }
};

template <typename T>
struct float_bits;
template <>
struct float_bits<float> : public float32_bits {};
template <>
struct float_bits<double> : public float64_bits {};

template <typename T>
static inline bool
IsNaN(T v)
{
#ifdef _MSC_VER
  return !!_isnan(v);
#else
  return isnan(v);
#endif
}

template <typename T> static inline bool
IsInfinite(T value)
{
  typedef float_bits<T> Properties;
  typedef typename Properties::Bits Bits;
  Bits bits = Properties::to_layout(value).bits;
  return (bits & Properties::kExponentMask) == Properties::kExponentMask;
};

// Performs the operation (x % y) where x and y are floating-point values.
//
// To compute a floating point modulus, this function returns "r", where r
// satisfies the following equation:
//
//  x = (I * y) + r
//
// Where I is an integer <= x, and r is a value less than y. If no such
// integer I exists, the result is NaN.
//
// If x or y are NaN, the result is NaN.
// If x is +/-Infinity, the result is NaN.
// If y is 0, the result is NaN (as a divide by zero is implied).
//
// If y is Infinity, then r = x (and I = 0).
// If x is +/-0, then r = +/-0.
template <typename T> static inline T
FloatModulo(T left, T right)
{
#if defined(KE_WINDOWS)
  // Windows fmod() does not follow the contract above, in that:
  //  42 % Infinity => NaN, instead of 42, and
  //  -0 % -N => 0, instead of -0.
  if ((!IsInfinite(left) && IsInfinite(right)) ||
      (left == 0 && !IsInfinite(right)))
  {
    return left;
  }
#endif

  return fmod(left, right);
}

} // namespace ke

#endif // _include_amtl_float_h_
