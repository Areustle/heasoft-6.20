/* RCS: $Id: closetable.c,v 1.2 1997/04/01 23:21:35 miket Exp $ */
#include <stdio.h>
#include <string.h>
#include "fitsio.h"

int closeTable (fitsfile *unit)
/*----------------------------------------------------------------------
 *  Close a FITS file
 *
 *  Input:  unit           Fitsio unit number for file
 *
 *  Return: int            Success?
 *
 ----------------------------------------------------------------------*/
{
  int status=0;

  ffpcks (unit, &status) ;
  ffclos (unit, &status) ;

  return (1);
}
