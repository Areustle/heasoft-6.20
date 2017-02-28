#include "methods.h"



static void determine_quad(COMBOXFORM* combo, Quad * q, int i, int j) {

    /**************************************************************
    * create the quadrilateral with the transformed pixel corners *
    **************************************************************/

    applyComboXform(combo, &q->a.x, &q->a.y, i - 0.5, j - 0.5);

    applyComboXform(combo, &q->b.x, &q->b.y, i + 0.5, j - 0.5);

    applyComboXform(combo, &q->c.x, &q->c.y, i + 0.5, j + 0.5);

    applyComboXform(combo, &q->d.x, &q->d.y, i - 0.5, j + 0.5);

} /* end of distribute_value function */


/*******************************************************************************
*
*******************************************************************************/
void distribute_value(IMAGE* image, COMBOXFORM* combo, int i, int j,
                             double value) {
    Quad q;


    /************************************************
    * avoid the hard work if it will have no affect *
    ************************************************/
    if (!isnan(value) && value == 0 && !image->weight) return;


	determine_quad(combo, &q, i, j);

    /*****************************************************************
    * either increment thepixels or set them null depending on the
    * input value
    *****************************************************************/
    if(isnan(value)) setQuadOnImageNull(image, &q);
    else           incrementQuadOnImage(image, &q, value);

} /* end of distribute_value function */



/*******************************************************************************
*
*******************************************************************************/
void distribute_flags (IMAGE* image, COMBOXFORM* combo, int i, int j,
                             long flags) {
    Quad q;

    /************************************************
    * avoid the hard work if it will have no affect *
    ************************************************/
    if (flags == 0) return;

	determine_quad(combo, &q, i, j);

    applyFlagsToImage(image, &q, flags);

} /* end of distribute_value function */

