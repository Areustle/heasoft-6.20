#include "coordfits.h"
#include "gtis.h"

#define FILENAME_DIMEN 256

typedef struct {

ATTFILE* att;
GTIS* gti;
ALIGN* align;

int nbins;
double bin_size;
double max_rotation;

int max_iterations;

char alignfile[FILENAME_DIMEN];
char newattfile[FILENAME_DIMEN];

int bound_gtis;
int history;


} PARAM;

/********************************************************
* read the parameters 
********************************************************/
PARAM* readParam(void);

/****************************************************************************
*****************************************************************************
* The following functions write the results to the parfile.
* the ra, dec and roll parameters are the mean telescope pointing
* and the Euler angles give the mean position of the spacecraft axes. 
* These functions don't use the PARAM structure but are in this file because
* they use the xpi wrappers.
****************************************************************************/
void writeRaDecRollToParfile(double ra, double dec, double roll);
void writeEulerToParfile(EULER* e);

