#ifndef ALIGN_INCLUDED

#include "fitsio.h"
#include "coord.h"

#define ALIGN_ROUNDOFF_ERROR 1e-10

/******************************************************************************
*******************************************************************************
* An ALIGN structure specifies a rotation and a roll anfgle convention.
* The intent is to use this for conversions between spacecraft axies and
* some other set of axes.
******************************************************************************/
typedef struct {

QUAT* q;
QUAT* q_inverse;

int roll_sign;
double roll_offset;


} ALIGN;

/****************************************************************************
* Allocate storage for an ALIGN object and set it's values to the defaults
* Specificly an identity rotation, positive roll angle with zero offset.
*****************************************************************************/
ALIGN* allocateDefaultAlign();


/************************************************************************
* destructor
************************************************************************/
void destroyAlign(ALIGN* align);

/* Copy an ALIGN structure. */
void copyAlign(ALIGN* dest, ALIGN* source);

/******************************************************************************
* read alignment information from the current HDU of an already opened FITS
* file. This function is useful for reading the alignment embedded in a
* teldef file.
******************************************************************************/
ALIGN* readAlignFromFITSfile(fitsfile* fp);

/***************************************************************************
* read a standalone alignment file. If the filename is "none" (case
* insensitive) or empty, then no file will be read and a default ALIGN
* structure will be returned.
***************************************************************************/
ALIGN* readAlign(char* filename);

/************************************************************************
* Given a quaternion, calculate the corresponding RA Dec and Roll,
* applying the given misalignment and roll angle convention.
************************************************************************/
void convertQuatToRADecRoll(ALIGN* align, QUAT* q,
                       double* ra, double* dec, double* roll);
                       
/************************************************************************
* Given RA Dec and roll, calculate the corresponding quaternion,
* applying the given misalignment and roll angle convention.
************************************************************************/
void convertRADecRollToQuat(ALIGN* align, QUAT* q,
                       double ra, double dec, double roll);

/****************************************
 * Print an Align structure to a stream.
 ****************************************/
void printAlign(ALIGN* align, FILE* stream);


#define ALIGN_INCLUDED
#endif /* ALIGN_INCLUDED */
