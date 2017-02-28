#include <stdio.h>
#include "align.h"
#define FILENAME_DIMEN 128


typedef struct {

char infile[FILENAME_DIMEN];
char outfile[FILENAME_DIMEN];

char format[FILENAME_DIMEN];

ALIGN* align;

FILE* fpout;

int history;

} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);
