char *xccrcsid = "RCS: $Id: xCC.c,v 1.2 2001/10/15 19:25:25 zpan Exp $" ;
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "bary.h"

int xCC (double t, double *tZero, double *tcorr)
{

  FILE *tdc ;

  double subday, m0, m1, m2, end, t0=0.0 ;
  double modt=0.0, corr = -999999.0 ;
  int error = 0 ;

  if ( !( tdc = openAFile (XTECC) ) ) {
    error = -1 ;
    corr = 0 ;
  }

  while ( !error && ( fscanf (tdc, "%lg %lg %lg %lg", &m0, &m1, &m2, &end) ) ) {
    if ( end < 0.0 ) {
      if ( m0 < 0.0 ) {
	error = 2 ;
	break ;
      }
      else {
	subday = m0 ;
	modt = t / 86400.0 - subday ;
	t0 = m1 ;
      }
    }
    else {
      if ( modt < end ) {
	corr = m0 + m1 * modt + m2 * modt * modt ;
	break ;
      }
    }
  }

  if ( error || ( corr == -999999.0 ) ) {
    corr = 0.0 ;
    error-- ;
  }
/*  else {
 *    printf ("TimeZero: %f seconds\n", t0) ;
 *    printf ("PCA: %d microseconds; HEXTE %d microseconds\n",
 *	    (long) corr - 16, (long) corr) ;
 *  }
 */

  *tZero = t0 ;
  *tcorr = corr ;
  fclose (tdc) ;

  return error ;
}
