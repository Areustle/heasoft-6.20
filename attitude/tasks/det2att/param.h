#ifndef DET2ATT_PARAM_H
#define DET2ATT_PARAM_H

#include <stdio.h>


#define FILENAME_DIMEN 1024


typedef struct
{
char infile[FILENAME_DIMEN];
char outfile[FILENAME_DIMEN];
char teldef[FILENAME_DIMEN];

} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);


#endif
