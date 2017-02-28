/*****************************************************************************
 * 
 * File name: xrt_gammq.h
 * 
 *
 * Author/Date: Barbara Saija March 2004
 *
 * History:
 *         v. 0.1.0 - adapted from NUMERICAL RECIPES
 *
 ****************************************************************************/
#include <math.h>
#include "headas_stdio.h"
#include <headas_utils.h>
#include "xrt_termio.h"

#define ITMAX 100
#define EPS   3.0e-7
#define FPMIN 1.0e-30 
int xrt_gcf(float *gammcf, float a, float x, float *gln);
int xrt_gammq(float a, float x, float * gammcf);
float xrt_gammln(float xx);
int xrt_gser(float *gamser, float a, float x, float *gln);



