/*****************************************************************************
* this C structure contains a set of bins for determining the nominal aspect 
* point
*****************************************************************************/
typedef struct {

QUAT* q0;

float** q[4];
float** dur;
float out; /* duration falling outside the bin structure */

int i0; /* these two variables hold the */
int j0; /* coordinate of the center bin */

double size; /* width of a single bin in radians */
int dimen;   /* number of bins in each dimension */

XFORM2D* trans; /* dummy used for binning */
QUAT* q_offset; 

} ASPECTBINS;

#include "gtis.h"


/*******************************************************************
* allocate storage space for a set of aspect bins
* and initialize the structure
*********************************************************************/
ASPECTBINS* allocateAspectBins(int nbins);

/****************************************************
* free the memory for a set of aspect bins 
*********************************************************/
void destroyAspectBins(ASPECTBINS* bin);

/**********************************************************************
* change the center pointing and bin size of a set of aspect bins
* and then reset all the bin values to zero.
**********************************************************************/
void resetAspectBins(ASPECTBINS* bin, QUAT* q0, double size);

/**************************************************************************
* add a pointing to the correct aspect bin
***************************************************************************/
void addToAspectBins(ASPECTBINS* bin, QUAT* q, double duration);

/**************************************************************************
* find the peak of a set of aspect bins and calculate the nominal pointing
* returns 1 if the peak is definitely valid
* returns 0 if there may be a higher peak outside the bin structure
***************************************************************************/
int findPeakOfAspectBins(QUAT* q, ASPECTBINS* bin);

/*************************************************
* print the non-zero duration bins 
**************************************************/
void printAspectBins(ASPECTBINS* bin);


/*********************************************************************
* fill a set of aspect bins with the aspect solution in a given file 
* The routine interpolates between adjecent attitude records which 
* are more than max_rotation apart
*********************************************************************/
void fillAspectBins(ASPECTBINS* bin, ATTFILE* file, double max_rotation,
                    GTIS* gti);
