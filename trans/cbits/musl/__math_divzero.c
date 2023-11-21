#include "libm.h"

double __kadena_math_divzero(uint32_t sign)
{
	return fp_barrier(sign ? -1.0 : 1.0) / 0.0;
}
