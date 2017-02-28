#include <stdio.h>
#define FILENAME_DIMEN 128

#include "coordfits.h"
#include "points.h"

typedef struct {

    TELDEF* teldef;
    POINTS* points;
    char outfile[FILENAME_DIMEN];

    double tolerance;
    int max_iterations;
    
    int decimals;
    
    int history;


} PARAM;


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void);
