#ifndef CALTOOLS_LOADED
#define CALTOOLS_LOADED

#include "cfortran.h"
#include "pctype.h"

/******************* Cspawn ********************/

#define Cspawn(cbuf,lbuf,ierr)\
 CCALLSFSUB3(CSPAWN,cspawn,STRING,INT,PCINT,cbuf,lbuf,ierr)

#endif

