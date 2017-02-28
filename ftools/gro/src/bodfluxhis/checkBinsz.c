
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"

void checkBinsz(int *nbin, long *noutrows, int *binsz)
{
      *nbin = (*noutrows)/(*binsz);

       if (*nbin < 2) 
	{
          printf ("Please input smaller bin size!");
          exit (1);
	  }
}
