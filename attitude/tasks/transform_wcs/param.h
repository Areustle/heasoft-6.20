
#define FILENAME_LENGTH 128

#include "comboxform.h"
#include "coordfits.h"
#include "coordwcs.h"


typedef struct {


WCS* wcs;
COMBOXFORM* combo;

char outfile[FILENAME_LENGTH];

char  in_suffix[2];
char out_suffix[2];

int history;

} PARAM;

/**************************************************************************
***************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void);
