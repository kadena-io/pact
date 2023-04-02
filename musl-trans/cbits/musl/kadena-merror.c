#include "kadena-libm.h"

/* ************************************************************************** */
/* __math_xflow.c */

double __kadena_math_xflow(uint32_t sign, double y)
{
	return kadena_eval_as_double(kadena_fp_barrier(sign ? -y : y) * y);
}

/* ************************************************************************** */
/* __math_oflow.c */

double __kadena_math_oflow(uint32_t sign)
{
	return __kadena_math_xflow(sign, 0x1p769);
}

/* ************************************************************************** */
/* __math_uflow.c */

double __kadena_math_uflow(uint32_t sign)
{
	return __kadena_math_xflow(sign, 0x1p-767);
}

/* ************************************************************************** */
/* __math_invalid.c */

double __kadena_math_invalid(double x)
{
	return (x - x) / (x - x);
}

/* ************************************************************************** */
/* __math_divzero.c */

double __kadena_math_divzero(uint32_t sign)
{
	return kadena_fp_barrier(sign ? -1.0 : 1.0) / 0.0;
}
