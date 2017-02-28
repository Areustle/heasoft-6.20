#include "param.h"
#define MAX_MODEL_PHA 40

/****************************************************************************
* This structure contains information to be communicated to and
* from the FITS iterator
****************************************************************************/

typedef struct {

PARAM* param;

double model[4][2*MAX_MODEL_PHA+1];
int max_pha;
int model_dimen;

int ccd_on[4];

} INFO;


/***************************************************************************
****************************************************************************
* create a new INFO structure 
* This routine also reads a bunch of things from the FITS header and
* initializes the model RDD function 
***************************************************************************/
INFO* allocateInfo(PARAM* param, fitsfile* fp);

/****************************************************************************
*****************************************************************************
* Initialize the RDD model
****************************************************************************/
void setRDDmodelInfo(INFO* info, double time, int sis, int ccdmode);
