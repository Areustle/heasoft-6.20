/* This routine is a replacement for performing a call to Fcecho internal
 to the C code since this causes OSF to barf and gag due to the number of
 expansions that cfortran.h inserts. Basically, the code runs out of memory.
*/
#include <stdio.h>
#include <string.h>
#include "cfortran.h"
#include "pctype.h"
#include "ftools.h"

void XTE_Fcecho(output)
  char *output;
{
  Fcecho(output);
}

