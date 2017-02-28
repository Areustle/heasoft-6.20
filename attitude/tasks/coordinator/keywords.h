#include "fitsio.h"
#include "longnam.h"
#include "info.h"
#include "param.h"
/************************************************************************
* this file contains a few routines for updating the header keywords
* for a coordinator run 
************************************************************************/

/**************************************************************************
**************************************************************************
* update the keywords for a single set of coordinates 
**************************************************************************/
void update_coord_keywords(COORDDEF* coord,
                            iteratorCol* colx, iteratorCol* coly,
                            double crvalx, double crvaly,
                            int nullx, int nully );

/****************************************************************************
***************************************************************************** 
* Write keywords to document aspecting calculation
****************************************************************************/
void writeAspectingKeywords(fitsfile* fp, PARAM* param);

/****************************************************************************
***************************************************************************** 
* add HISTORY comments to header
****************************************************************************/
void add_history_comments(fitsfile* fp, INFO* info);

/****************************************************************************
***************************************************************************** 
* Update column keywords and add HISTORY comments 
****************************************************************************/
void update_keywords(INFO* info, iteratorCol* fits_col);
