/*----------------------------------------------------------------
File: getmedian.c

Description:
    Calculates the median value of an input 1-D array.
    Calculations are done in DOUBLE.

Author:
       Banashree M Seifert (August 1997)
 
Modifications:
 
 
Functions called:
     qsort  -- to sort the input array X
      cmp   -- compares the two values.  This functions is required by qsort


Variables used:
      x     double    input array
      n     int       no. of elements in array x
      xmed  double    returned median value
 
------------------------------------------------------------------*/
#include <stdlib.h>
#include "cfortran.h"
#include "getmedian.h"

void getmedian(double *x, int n, double *xmed)
{
  int n2, n2p;
  int i;
  
  qsort(x,n, sizeof(double), cmp);
  n2p=(n2=n/2)+1;
  *xmed = (n%2? x[n2p] : 0.5*(x[n2]+x[n2p]));

  return;
}

FCALLSCSUB3(getmedian,GETMEDIAN,getmedian,DOUBLEV,INT,PDOUBLE)

int cmp(const void *x1, const void *x2)
{
  if(*((double *)x1) >  *((double *)x2)) return 1;
  else if (*((double *)x1) == *((double *)x2)) return 0;
  else return -1;
}





