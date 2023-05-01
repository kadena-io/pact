#include "libm.h"

double __kadena_math_invalid(double x)
{
	return (x - x) / (x - x);
}
