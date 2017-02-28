/* $Id: haxBary.c,v 1.1 2001/02/21 21:32:25 miket Exp $ */
/*--------------------------------------------------------------------
 *
 *    File:        axBarym.c
 *    Programmer:  Arnold Rots, ASC
 *    Date:        3 September 1998
 *
 *  axBary is a multi-mission function that applies barycenter
 *  corrections to HFWG-compliant FITS files.
 *
 *  This file contains a simple main wrapper for axBary that allows
 *  running it from the command line.
 *--------------------------------------------------------------------*/

#include "bary.h"

/*           A Simple Main          */
int main (int argc, char **argv)
{
  char progName[80] = "" ;
  char inFile[1024], outFile[1024], orbitFile[1024], refFrame[32] ;
  double ra, dec ;
  int debug = 0 ;
  int error = 0 ;

  int getopts (int, char**, char*, char*, char*, char*, double*, double*,
	     char*, int*) ;

  if ( getopts (argc, argv, progName, orbitFile, inFile, outFile,
		&ra, &dec, refFrame, &debug) )
    exit (1) ;

  error = axBary (inFile, orbitFile, outFile, ra, dec, refFrame, debug) ;

  exit (error) ;
}

/*-----------------------------------------------------------------------
 *
 *  getopts decodes command line options for axBary.
 *
 *----------------------------------------------------------------------*/

int getopts (int argc, char **argv, char *progName, char *orbitFile,
	     char *inFile, char *outFile, double *ra, double *dec,
	     char *refFrame, int *debug)
{
/*
 *  Initialize ----------------------------------------------------------
 */
  *refFrame = 0 ;
  *orbitFile = 0 ;
  *inFile = 0 ;
  *outFile = 0 ;
  *ra = *dec = -1000.0 ;
  strcpy (progName, *argv) ;
  argv++ ;
  argc-- ;

/*
 *  Decode --------------------------------------------------------------
 */
  while ( argc ) {
    if ( !strcmp (*argv, "-debug") )
      *debug = 1 ;
    else if ( !strncmp (*argv, "-ra", 3) ) {
      *ra = atof (*(++argv)) ;
      argc-- ;
    }
    else if ( !strncmp (*argv, "-dec", 4) ) {
      *dec = atof (*(++argv)) ;
      argc-- ;
    }
    else if ( !strcmp (*argv, "-i") ) {
      strcpy (orbitFile, *(++argv)) ;
      argc-- ;
    }
    else if ( !strcmp (*argv, "-o") ) {
      strcpy (outFile, *(++argv)) ;
      argc-- ;
    }
    else if ( !strcmp (*argv, "-f") ) {
      strcpy (inFile, *(++argv)) ;
      argc-- ;
    }
    else if ( !strncmp (*argv, "-ref", 4) ) {
      strcpy (refFrame, *(++argv)) ;
      argc-- ;
    }
    argv++ ;
    argc-- ;
  }

/*
 *  Check and return ----------------------------------------------------
 */
  if ( !orbitFile[0] || ( !inFile[0] && !outFile[0] ) ) {
    fprintf (stderr, "axBary - Usage:\n    %s ",  progName) ;
    fprintf (stderr, "-i orbitFile -f inputDataFile [-o outFile]\n") ;
    fprintf (stderr, "                      [-ra RA] [-dec DEC] [-ref refFrame]\n") ;
    return 1 ;
  }
  else
    return 0 ;
}
