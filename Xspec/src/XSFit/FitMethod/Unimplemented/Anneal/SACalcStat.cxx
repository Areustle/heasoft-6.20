
#include "xsTypes.h"


extern "C" void sacalcstat_(int npar, Real* par, Real& ftstat);

//Routine that the simulated annealing code calls to get the
// function value.
// npar: number of parameters 
// par: parameter values
// ftstat: output statistic value
// fortran callable as SACalcStat. C++ implementation of annfcn.f


void sacalcstat_(int npar, Real* par, Real& ftstat)
{


}
