#include "libm.h"

double __kadena_math_uflow(uint32_t sign)
{
	return __kadena_math_xflow(sign, 0x1p-767);
}
