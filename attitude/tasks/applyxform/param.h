
#define FILENAME_LENGTH 128

#include "comboxform.h"
#include "coordfits.h"
#include "coordwcs.h"


typedef struct {

COMBOXFORM* combo;

FILE* fpin;
FILE* fpout;

char format[FILENAME_LENGTH];

int history;


} PARAM;

/**************************************************************************
***************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void);
