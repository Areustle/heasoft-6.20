#ifndef COORDINATOR_PARAM_INCLUDED

#include "fitsio.h"
#include "coord.h"
#define FILENAME_LENGTH 128

/****************************
* input paramater structure 
****************************/
typedef struct {

char event_file[FILENAME_LENGTH];
char event_extension[FLEN_VALUE]; /* name of event file events extension */

char time_col_name[FLEN_VALUE]; /* name of time column */

char att_file[FILENAME_LENGTH];
char cal_file[FILENAME_LENGTH];

int do_aberration;
int follow_sun;

double ra,dec;

int randomize;
int seed;
int interpolation;

int skyx_null_value;
int skyy_null_value;

int do_sky;

double time_margin;

} PARAM;


/**************************************************************************
**************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void);

/****************************************************************************
*****************************************************************************
* free all the memory associated with a param structure
****************************************************************************/
void destroyParam(PARAM* param);

#define COORDINATOR_PARAM_INCLUDED
#endif /* COORDINATOR_PARAM_INCLUDED */
