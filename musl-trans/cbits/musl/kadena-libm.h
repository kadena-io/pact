#ifndef _KADENA_LIBM_H
#define _KADENA_LIBM_H

#include <stdint.h>
#include <float.h>
#include <math.h>
#include "endian.h"

/* Support non-nearest rounding mode.  */
#define KADENA_WANT_ROUNDING 1
/* Support signaling NaNs.  */
#define KADENA_WANT_SNAN 0

#if WANT_SNAN
#error SNaN is unsupported
#else
#define issignalingf_inline(x) 0
#define issignaling_inline(x) 0
#endif

#ifndef KADENA_TOINT_INTRINSICS
#define KADENA_TOINT_INTRINSICS 0
#endif

#if KADENA_TOINT_INTRINSICS
/* Round x to nearest int in all rounding modes, ties have to be rounded
   consistently with converttoint so the results match.  If the result
   would be outside of [-2^31, 2^31-1] then the semantics is unspecified.  */
static double_t kadena_roundtoint(double_t);

/* Convert x to nearest int in all rounding modes, ties have to be rounded
   consistently with roundtoint.  If the result is not representible in an
   int32_t then the semantics is unspecified.  */
static int32_t kadena_converttoint(double_t);
#endif

/* Helps static branch prediction so hot path can be better optimized.  */
#ifdef __GNUC__
#define kadena_predict_true(x) __builtin_expect(!!(x), 1)
#define kadena_predict_false(x) __builtin_expect(x, 0)
#else
#define kadena_predict_true(x) (x)
#define kadena_predict_false(x) (x)
#endif

/* Evaluate an expression as the specified type. With standard excess
   precision handling a type cast or assignment is enough (with
   -ffloat-store an assignment is required, in old compilers argument
   passing and return statement may not drop excess precision).  */

static inline float kadena_eval_as_float(float x)
{
	float y = x;
	return y;
}

static inline double kadena_eval_as_double(double x)
{
	double y = x;
	return y;
}

/* kadena_fp_barrier returns its input, but limits code transformations
   as if it had a side-effect (e.g. observable io) and returned
   an arbitrary value.  */

#ifndef kadena_fp_barrier
#define kadena_fp_barrier kadena_fp_barrier
static inline double kadena_fp_barrier(double x)
{
	volatile double y = x;
	return y;
}
#endif

/* kadena_fp_force_eval ensures that the input value is computed when that's
   otherwise unused.  To prevent the constant folding of the input
   expression, an additional kadena_fp_barrier may be needed or a compilation
   mode that does so (e.g. -frounding-math in gcc). Then it can be
   used to evaluate an expression for its fenv side-effects only.   */

#ifndef kadena_fp_force_eval
#define kadena_fp_force_eval kadena_fp_force_eval
static inline void kadena_fp_force_eval(double x)
{
	volatile double y;
	y = x;
}
#endif

#define kadena_asuint(f) ((union{float _f; uint32_t _i;}){f})._i
#define kadena_asuint64(f) ((union{double _f; uint64_t _i;}){f})._i
#define kadena_asdouble(i) ((union{uint64_t _i; double _f;}){i})._f

/* error handling functions */
float __kadena_math_uflowf(uint32_t);
float __kadena_math_oflowf(uint32_t);
float __kadena_math_divzerof(uint32_t);
double __kadena_math_uflow(uint32_t);
double __kadena_math_oflow(uint32_t);
double __kadena_math_divzero(uint32_t);
double __kadena_math_invalid(double);

#endif
