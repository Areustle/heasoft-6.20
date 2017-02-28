
#define FILENAME_LENGTH 128
#define COORD_NAME_LENGTH 4

#include "coordfits.h"
#include "coordwcs.h"




typedef struct {

char coord_name[COORD_NAME_LENGTH];

WCS* wcs;

char outfile[FILENAME_LENGTH];

char suffix[2];

int history;


} PARAM;

/**************************************************************************
***************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void);


/**************************************************************************
**************************************************************************
* determine what transform we should use 
**************************************************************************/
int coord_type(TELDEF* teldef, char* name);


/******************************************************************************
*
******************************************************************************/
int get_segment(TELDEF* teldef);

/***************************************************************************
* Get the COORDDEF structure for a given coordinate integer code
***************************************************************************/
COORDDEF* get_coorddef(TELDEF* teldef, int coord);



