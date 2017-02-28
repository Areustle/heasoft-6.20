#include <iostream>
#include "spibkg_common.h"

extern "C" void spibkgHi(const Real* energy, int Nflux, const Real* parameter, 
	    int spectrum, Real* flux, Real* fluxError, const char* init)
{
    static bool firstCall = true;
    int numRows;

    if(firstCall && !(firstCall = false))
	numRows = initialize(energy, Nflux);
    
    int nParams = 50;

    for(int i = 0; i < Nflux; ++i)
	flux[i] = bkg[spectrum][i];
      //	flux[i] = bkg[getRowNumber(spectrum) - 1][i];

    int j;

    for(int i = 0; i < nParams; ++i)
    {
	j = sorted_indices[i];
	flux[j] *= (1 + parameter[i]);
    }    
}

