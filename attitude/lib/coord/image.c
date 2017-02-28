/*******************************************************************************
* This file contains functions to determine the transform between the pixels of
* an image and a coordinate system described by a COORDDEF structure. There are
* a million different arbitrary conventions involved in doing this. So the idea
* is to pick one set of conventions and encapsulate it in these two functions.
* That way all tools can prompt for input values in a consistent way, and we
* can document the exact conventions used here.
*
* The general issues involved in doing this are:
* - differences in the first pixel coordinates in the image and in the 
*   coorddef space
*
* - "windowing" of the image - e.g. it may not occupy the full coordef space

* - binning of the image - i.e. a single image pixel could cover multiple pixels
*   in the coorddef space.
*
*
* Here are the conventions:
* - The center of the first image pixel is at (0, 0)
*
* - For windowing we specify an offset of the first pixel in the image from
*   the first pixel in the coorddef space. An offset of (0, 0) means that the
*   first pixel of the image corresponds to the first pixel in the coorddef space.
*   This should be the default for the user interface.
*   A negative offset means the first pixel of the image is outside the 
*   coorddef coordinate space.
*
* - The offset is in the units of the coorddef space. In other words the 
*   offset is applied to unbinned pixels.
* 
* - Binning is specified as the number of coorddef pixels which correspond to
*   a sigle image pixel. For example a binning factor of "2" means one image
*   pixel covers two coorddef pixels.
*
* - The binning transformation leaves the left edge of the first pixel aligned
*   with the left edge of pixel (first_pixel+offset) in the coorddef space.
*
* - An image may have different binning in the X and Y directions.
*
* - The binning factor does not have to be an integer.
*
* - Images are never rotated or "skewed" with respect to the coorddef coordinate
*   system.
*
*
* To summarize, in one dimension:
* image = ((coord - first - offset) + 0.5 ) /bin - 0.5
*
* where image is the image coordinate
*       coord is the coorddef coordinate
*       first is the coordinate of the center of the first pixel in coorddef space
*       offset is the offset parameter
*       bin is the binning parameter
*
* The 0.5 terms are because the coordinates refer to the centers of pixels and
* the left edges of the pixels align when binning.
*
* For example if the first pixel in the coordef space is "1", and you have 
* an offset of "3" and a binning factor of "2" it would look like this:
*
*   4   5   6   7   8   9
* |---|---|---|---|---|---| coorddef
* |-------|-------|-------| image
*     0       1       2
*
* Note how the left edge of the first pixel is at 3.5 in the coordef space,
* which corresponds to -0.5 in the image space.
*
* Also, a coorddef coordinate of "9" corresponds to an image coordinate of
* 2.25 = ((9 - 1 -3) + 0.5 ) /2 - 0.5
*
*
* A note on image size:
* The above is all you need to specify the transformation between image and 
* coordef space. If you have a tool which needs to create an output image,
* the the user interface should always specify the image size in binned
* pixels. In other words the user should specify the number of pixels which
* will actually be in the image regardless of the number of coordef pixels
* that would correspond to. A negative size dimension should default to the
* number of image pixels needed to span the coordef space. in other words
* to (coord->dimen/bin), where coord->dimen is the number of pixels in the
* coordef space and bin is the binning factor.
*
*******************************************************************************/

#include "coorddef.h"


/**********************************************************************************
* returns the transform from image coordinates to coordef space.
*********************************************************************************/
void setImageToCoordDefXform2D(XFORM2D* trans, COORDDEF* coord,
                               double binx, double biny,
			       double offset_x, double offset_y) {
XFORM2D* bin;
XFORM2D* unbin;
XFORM2D* offset;

bin    = allocateXform2d();
unbin  = allocateXform2d();
offset = allocateXform2d();

/********************************
* create the binning transforms *
********************************/
setXform2dToScaling(bin, 1./binx, 1./biny, -0.5, -0.5);
invertXform2d(unbin, bin);

/******************************
* create the offset transform *
******************************/
setXform2dToTranslation(offset, offset_x + coord->first_pixel_x,
                                offset_y + coord->first_pixel_y);
				
/*****************************
* combine the two transforms *
*****************************/
combineXform2ds(trans, unbin, offset);

/**********
* cleanup *
**********/
destroyXform2d(bin);
destroyXform2d(unbin);
destroyXform2d(offset);

} /* end of setImageToCoordXform2D function */

/**********************************************************************************
* returns the transform from coorddef space to pixel coordinates
*********************************************************************************/
void setCoordDefToImageXform2D(XFORM2D* trans, COORDDEF* coord,
                            double binx, double biny,
			    double offset_x, double offset_y) {
XFORM2D* bin;
XFORM2D* offset;

bin    = allocateXform2d();
offset = allocateXform2d();

/********************************
* create the binning transforms *
********************************/
setXform2dToScaling(bin, 1./binx, 1./biny, -0.5, -0.5);


/******************************
* create the offset transform *
******************************/
setXform2dToTranslation(offset, -(offset_x + coord->first_pixel_x),
                                -(offset_y + coord->first_pixel_y));
				
/*****************************
* combine the two transforms *
*****************************/
combineXform2ds(trans, offset, bin);

/**********
* cleanup *
**********/
destroyXform2d(bin);
destroyXform2d(offset);

} /* end of setImageToCoordXform2D function */
