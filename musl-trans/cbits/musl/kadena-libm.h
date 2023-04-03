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

#if KADENA_WANT_SNAN
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

static inline float eval_as_float(float x)
{
	float y = x;
	return y;
}

static inline double eval_as_double(double x)
{
	double y = x;
	return y;
}

/* fp_barrier returns its input, but limits code transformations
   as if it had a side-effect (e.g. observable io) and returned
   an arbitrary value.  */

#ifndef fp_barrier
#define fp_barrier fp_barrier
static inline double fp_barrier(double x)
{
	volatile double y = x;
	return y;
}
#endif

/* fp_force_eval ensures that the input value is computed when that's
   otherwise unused.  To prevent the constant folding of the input
   expression, an additional fp_barrier may be needed or a compilation
   mode that does so (e.g. -frounding-math in gcc). Then it can be
   used to evaluate an expression for its fenv side-effects only.   */

#ifndef fp_force_eval
#define fp_force_eval fp_force_eval
static inline void fp_force_eval(double x)
{
	volatile double y;
	y = x;
}
#endif

#define kadena_asuint(f) ((union{float _f; uint32_t _i;}){f})._i
#define kadena_asuint64(f) ((union{double _f; uint64_t _i;}){f})._i
#define kadena_asdouble(i) ((union{uint64_t _i; double _f;}){i})._f

/* ************************************************************************** */
/* __math_xflow.c */

static inline double __math_xflow(uint32_t sign, double y)
{
	return eval_as_double(fp_barrier(sign ? -y : y) * y);
}

/* ************************************************************************** */
/* __math_oflow.c */

static inline double __math_oflow(uint32_t sign)
{
	return __math_xflow(sign, 0x1p769);
}

/* ************************************************************************** */
/* __math_uflow.c */

static inline double __math_uflow(uint32_t sign)
{
	return __math_xflow(sign, 0x1p-767);
}

/* ************************************************************************** */
/* __math_invalid.c */

static inline double __math_invalid(double x)
{
	return (x - x) / (x - x);
}

/* ************************************************************************** */
/* __math_divzero.c */

static inline double __math_divzero(uint32_t sign)
{
	return fp_barrier(sign ? -1.0 : 1.0) / 0.0;
}

#endif
