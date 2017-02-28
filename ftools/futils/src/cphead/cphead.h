/******************************************************************************

File name: cphead.h

******************************************************************************/

#include "fitsio.h"

/****** functions for copying keywords in the header *************************/
#define MAXCOPYKEYS 	500
int c_copyhead(fitsfile *infits, fitsfile *outfits, int scale, ...);
int c_copyheadn(fitsfile *infits, fitsfile *outfits,
                int scale, int numkeys,char** list );
int fit_comment(char *comment);


