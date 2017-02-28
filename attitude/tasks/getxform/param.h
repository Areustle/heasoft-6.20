#ifndef XIMAGEXFORM_PARAM_INCLUDED

#define FILENAME_LENGTH 128
#define COORD_NAME_LENGTH 4

#include "coordfits.h"
#include "comboxform.h"



typedef struct {



TELDEF* teldef;

char from_coord[COORD_NAME_LENGTH];
char   to_coord[COORD_NAME_LENGTH];

char outfile[FILENAME_LENGTH];

COMBOXFORM* combo;

double time;
QUAT * quat;

int history;

} PARAM;

/**************************************************************************
***************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void);

/******************************************************************************
* read and interpret the transform method from the parameter file
******************************************************************************/
int determineMethod();


/**************************************************************************
**************************************************************************
* determine what transform we should use 
**************************************************************************/
int coord_type(TELDEF* teldef, char* name);

/**************************************************************************
**************************************************************************
* returns >0 if the first coordinate index is "lower" than the second.
**************************************************************************/
int is_upward(int from, int to);

/******************************************************************************
*
******************************************************************************/
int get_segment(TELDEF* teldef);

/***************************************************************************
* Get the COORDDEF structure for a given coordinate integer code
***************************************************************************/
COORDDEF* get_coorddef(TELDEF* teldef, int coord);

int parstamp_path (const char * path);

#define XIMAGEXFORM_PARAM_INCLUDED
#endif /* XIMAGEXFORM_PARAM_INCLUDED */
