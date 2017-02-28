/*
 *	nu_gammq.h: --- 
 *
 *	DESCRIPTION:
 *		
 *		
 */


#include <math.h>
#include "headas_stdio.h"
#include <headas_utils.h>
#include "nu_termio.h"

#define ITMAX 100
#define EPS   3.0e-7
#define FPMIN 1.0e-30 
int nu_gcf(float *gammcf, float a, float x, float *gln);
int nu_gammq(float a, float x, float * gammcf);
float nu_gammln(float xx);
int nu_gser(float *gamser, float a, float x, float *gln);



