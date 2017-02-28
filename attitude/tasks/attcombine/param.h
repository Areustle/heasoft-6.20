#include <stdio.h>
#define FILENAME_DIMEN 128

#include "align.h"


typedef struct {

char first[FILENAME_DIMEN];
char second[FILENAME_DIMEN];
char combined[FILENAME_DIMEN];

ALIGN * align;
int interpolation;
int history;


} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);
