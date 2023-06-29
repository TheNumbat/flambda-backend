
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <smmintrin.h>
#include <emmintrin.h>

int64_t int64x2_low_int64(__m128i v) 
{
  return _mm_extract_epi64(v, 0);
}

int64_t int64x2_high_int64(__m128i v) 
{
  return _mm_extract_epi64(v, 1);
}

__m128i int64x2_of_int64s(int64_t low, int64_t high) 
{
  return _mm_set_epi64x(high, low); 
}

// These symbols need to be defined for linking to work,
// but they will never be called - all correspond to builtins.

#define BUILTIN(name) void name(){}

BUILTIN(caml_int64x2_of_int32x4)
BUILTIN(caml_int64x2_of_int16x8)
BUILTIN(caml_int64x2_of_int8x16)
BUILTIN(caml_int64x2_of_float32x4)
BUILTIN(caml_int64x2_of_float64x2)
BUILTIN(caml_int32x4_of_int64x2)
BUILTIN(caml_int32x4_of_int16x8)
BUILTIN(caml_int32x4_of_int8x16)
BUILTIN(caml_int32x4_of_float32x4)
BUILTIN(caml_int32x4_of_float64x2)
BUILTIN(caml_int16x8_of_int64x2)
BUILTIN(caml_int16x8_of_int32x4)
BUILTIN(caml_int16x8_of_int8x16)
BUILTIN(caml_int16x8_of_float32x4)
BUILTIN(caml_int16x8_of_float64x2)
BUILTIN(caml_int8x16_of_int64x2)
BUILTIN(caml_int8x16_of_int32x4)
BUILTIN(caml_int8x16_of_int16x8)
BUILTIN(caml_int8x16_of_float32x4)
BUILTIN(caml_int8x16_of_float64x2)
BUILTIN(caml_float32x4_of_int64x2)
BUILTIN(caml_float32x4_of_int32x4)
BUILTIN(caml_float32x4_of_int16x8)
BUILTIN(caml_float32x4_of_int8x16)
BUILTIN(caml_float32x4_of_float64x2)
BUILTIN(caml_float64x2_of_int64x2)
BUILTIN(caml_float64x2_of_int32x4)
BUILTIN(caml_float64x2_of_int16x8)
BUILTIN(caml_float64x2_of_int8x16)
BUILTIN(caml_float64x2_of_float32x4)

// SSE2 / Float32x4

BUILTIN(caml_sse_float32x4_eq)
BUILTIN(caml_sse_float32x4_neq)
BUILTIN(caml_sse_float32x4_gt)
BUILTIN(caml_sse_float32x4_lt)
BUILTIN(caml_sse_float32x4_ge)
BUILTIN(caml_sse_float32x4_le)
BUILTIN(caml_sse_float32x4_both_nan)
BUILTIN(caml_sse_float32x4_either_nan)
BUILTIN(caml_sse_float32x4_add)
BUILTIN(caml_sse_float32x4_sub)
BUILTIN(caml_sse_float32x4_mul)
BUILTIN(caml_sse_float32x4_div)
BUILTIN(caml_sse_float32x4_max)
BUILTIN(caml_sse_float32x4_min)
BUILTIN(caml_sse_float32x4_rcp)
BUILTIN(caml_sse_float32x4_rsqrt)
BUILTIN(caml_sse_float32x4_sqrt)
BUILTIN(caml_sse_float32x4_set4)
BUILTIN(caml_sse_float32x4_set1)
BUILTIN(caml_sse_float32x4_zero)
BUILTIN(caml_sse_float32x4_move_high)
BUILTIN(caml_sse_float32x4_move_low)
BUILTIN(caml_sse_float32x4_shuffle_high)
BUILTIN(caml_sse_float32x4_shuffle_low)
BUILTIN(caml_sse_float32x4_shuffle)
