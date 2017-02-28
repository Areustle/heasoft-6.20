#ifndef COORDINATOR_INFO_INCLUDED
#include "coordfits.h"
#include "param.h"

/**********************************************************************
* a structure of information useful to the update_coordinate function *
**********************************************************************/
typedef struct {

PARAM* param;

ATTFILE* att;
TELDEF* cal;

/*************************************************************
* these things do not transport information. They are
* just in here so that we don't have to create and destroy
* these structures withing the iterated function 
*************************************************************/
QUAT* q; /* current pointing */


/************************************************************
* abberation information used only when param->follow_sun=0 *
************************************************************/
double v;       /* magnitude of earth's velocity */
double vhat[3]; /* direction of earths velocity (normalized to unity)  */

double mjdref; /* value of MJDREF keyword in event file */


/********************
* FITS column stuff *
********************/
iteratorCol* time_col;
iteratorCol* seg_col;
iteratorCol* rawx_col;
iteratorCol* rawy_col;
iteratorCol** detx_col;
iteratorCol** dety_col;
iteratorCol* skyx_col;
iteratorCol* skyy_col;

int** detx; 
int** dety;


int seg_value;
int rawx_value;
int rawy_value;

int missing_attitude_count; /* number of events not covered by attitude file */

} INFO;


/*****************************************************
* set up misc info structure to hand to the iterator 
* the fitsfile pointer argument is for the event file
*****************************************************/
INFO* createInfo(PARAM* param, fitsfile* fp);


/**************************************************************************
***************************************************************************
* function to set iterator FITS column strucutres
**************************************************************************/
iteratorCol* setIteratorColumns(INFO* info, fitsfile* fp, int* ncols);

/******************************************************************************
*******************************************************************************
* free all the memory associated with an INFO structure
* Note this even destroys the associated PARAM structure even though
* that structure was created separately.
******************************************************************************/
void destroyInfo(INFO* info);

#define COORDINATOR_INFO_INCLUDED
#endif /* COORDINATOR_INFO_INCLUDED */
