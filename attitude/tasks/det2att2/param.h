#ifndef DET2ATT_PARAM_H
#define DET2ATT_PARAM_H

#include <stdio.h>
#include "fitsio.h"


#define FILENAME_DIMEN 1024


typedef struct
{
  char infile[FILENAME_DIMEN];
  char outfile[FILENAME_DIMEN];
  char teldeffile[FILENAME_DIMEN];
  char startsys[FLEN_VALUE];
  int debug;

} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);


#endif
