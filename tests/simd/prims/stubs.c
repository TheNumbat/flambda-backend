
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
