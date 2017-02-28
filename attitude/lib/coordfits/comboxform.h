#ifndef COMBOXFORM_INCLUDED

#include "xform2d.h"
#include "mapxform.h"
#include "fitsio.h"

typedef struct {

XFORM2D* trans;
MAPXFORM* map;

} COMBOXFORM;

/******************************************************************************
* create a new comboxform set to the identity transform
******************************************************************************/
COMBOXFORM* allocateComboXform();

/****************************************************************************
*
****************************************************************************/
void destroyComboXform(COMBOXFORM* combo);

/****************************************************************************
*
****************************************************************************/
void applyComboXform(COMBOXFORM* combo, double* newx, double* newy,
                                        double  oldx, double  oldy );

/******************************************************************************
*
******************************************************************************/
void writeComboXform(COMBOXFORM* combo, char* filename);


/******************************************************************************
*
******************************************************************************/
COMBOXFORM*  readComboXform(char* filename);

/**************************************************************************
*
**************************************************************************/
void applyXform2dBeforeComboXform(XFORM2D* trans, COMBOXFORM* combo);

/**************************************************************************
*
**************************************************************************/
void applyXform2dAfterComboXform(COMBOXFORM* combo, XFORM2D* trans);

/**************************************************************************
* return a new COMBOXFORM which is the inverse of this one.
* Note that if there is a non-linear part to the transform, the
* inverse is not reversable. i.e. two inverses do not give the original
* transform.
**************************************************************************/
COMBOXFORM* invertComboXform(COMBOXFORM* combo);
#define COMBOXFORM_INCLUDED
#endif /* COMBOXFORM_INCLUDED */
