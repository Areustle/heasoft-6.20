#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define SWIFTCC "swco.dat"

int sCC (double t, double *tZero, double *tcorr, char *clockFile)
{
  /*
  char *swiftClockFile;
  FILE *tdc ;

  double subday, m0, m1, m2, end, t0=0.0 ;
  double modt=0.0, corr = -999999.0 ;*/
  int error = 0 ;
  /*
  FILE *openAFile (const char *) ;

  // Use clock file name if requested, otherwise default to SWIFTCC
  swiftClockFile = clockFile ? clockFile : SWIFTCC;
  if ( !( tdc = openAFile (swiftClockFile) ) ) {
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

  *tZero = t0 ;
  *tcorr = -1*corr ; /* corr is measured offset so have to reverse sign *//*
  fclose (tdc) ;
*/
  /* OK, since axBary only calculates the clock correction for the midpoint of
     the file to be barycentered and applies that across the whole file (!)
     we've decided to apply it in the Perl wrapper routine (barycorr) for every
     point. This no-op routine is being left in for uniformity.
     M.Tripicco 16Dec2004 */

  *tZero = 0.0;
  *tcorr = 0.0; 

  return error ;
}
