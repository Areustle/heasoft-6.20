/// \file ahfits_image.h
/// \brief ahfits: Read/Write images from FITS files 
/// \author Andy Sargent
/// \date $Date: 2015/08/17 20:46:05 $

/// \addtogroup mod_ahfits
/// \section ahfits_image File Access Library - ahfits_image
///
/// This library contains functions to read and write images of the
/// active extension.  Functions exist for each value type, e.g. double.
///

#ifndef AHFITS_AHFITS_IMAGE_H
#define AHFITS_AHFITS_IMAGE_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_IMAGE,"$Id: ahfits_image.h,v 1.6 2015/08/17 20:46:05 asargent Exp $")

#include "ahfits/ahfits_base.h"

#include <vector>

/// \ingroup mod_ahfits
namespace ahfits {

/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief type definitions of each 2D vector image types
typedef std::vector<std::vector<double> > Img2dDbl;
typedef std::vector<std::vector<long> > Img2dLng;
typedef std::vector<std::vector<long long> > Img2dLngLng;
typedef std::vector<std::vector<float> > Img2dFlt;
typedef std::vector<std::vector<short> > Img2dShr;
typedef std::vector<std::vector<char> > Img2dByt;

/// \brief FITS file access function for writing images
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
/// \param[in] nx         Image x-axis length {NAXIS1}
/// \param[in] ny         image y-axis length {NAXIS2}
/// \param[in] bitpix     Type definition of image elements
/// \param[out] status    Return true if error present
void writeImageCommon(FilePtr & ahffp, const std::string extname, double bzero,
                       double bscale, long * bnull, int nx, int ny, int bitpix,
                       int & status);

/// \brief Write images as type double to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dDbl img, 
                double bzero, double bscale, long * bnull);


/// \brief Write images as type long to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dLngLng img, 
                double bzero, double bscale, long * bnull);

/// \brief Write images as type long to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dLng img, 
                double bzero, double bscale, long * bnull);

/// \brief Write images as type float to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dFlt img, 
                double bzero, double bscale, long * bnull);

/// \brief Write images as type short to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dShr img, 
                double bzero, double bscale, long * bnull);

/// \brief Write images as type char to FITS file
/// \param[in,out] ahffp  The FITS file object
/// \param[in] extname    Desired extension name to write FITS image to
/// \param[in] img        2D image vector to write to FITS image
/// \param[in] bzero      0th point if pixel values are not true physical values (Default 1.0)
/// \param[in] bscale     Scale if pixel values are not true physical values (Default 0.0)
/// \param[in] bnull      Null value keyword in image
void writeImage(FilePtr & ahffp, const std::string extname, Img2dByt img, 
                double bzero, double bscale, long * bnull);

/// \brief FITS file access function for reading images
/// \param[in,out] ahffp  The FITS file object
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
/// \param[out] bnull     Null value keyword in image
/// \param[out] fpixel    Pointer to first pixels in each axis
/// \param[out] lpixel    Pointer to last pixels in each axis
/// \param[out] inc       Pointer to read every n'th pixel (Default 1) for each axis
void readImageCommon(FilePtr & ahffp, long & nx, long & ny, long * bnull,
                      long * fpixel, long * lpixel, long * inc);

/// \brief Read images as type double from FITS file
/// \param[in] ahffp      The FITS file object
/// \param[out] img       2D image vector to write to FITS image
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
void readImage(FilePtr & ahffp, Img2dDbl & img, long & nx, long & ny);

/// \brief Read images as type long from FITS file
/// \param[in] ahffp      The FITS file object
/// \param[out] img       2D image vector to write to FITS image
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
void readImage(FilePtr & ahffp, Img2dLng & img, long & nx, long & ny);

/// \brief Read images as type float from FITS file
/// \param[in] ahffp      The FITS file object
/// \param[out] img       2D image vector to write to FITS image
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
void readImage(FilePtr & ahffp, Img2dFlt & img, long & nx, long & ny);

/// \brief Read images as type short from FITS file
/// \param[in] ahffp      The FITS file object
/// \param[out] img       2D image vector to write to FITS image
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
void readImage(FilePtr & ahffp, Img2dShr & img, long & nx, long & ny);

/// \brief Read images as type char from FITS file
/// \param[in] ahffp      The FITS file object
/// \param[out] img       2D image vector to write to FITS image
/// \param[out] nx        Image x-axis length {NAXIS1}
/// \param[out] ny        image y-axis length {NAXIS2}
void readImage(FilePtr & ahffp, Img2dByt & img, long & nx, long & ny);

/** @} */

} // namespace ahfits

#endif   /* AHFITS_AHFITS_IMAGE_H */

/* Revision Log
   $Log: ahfits_image.h,v $
   Revision 1.6  2015/08/17 20:46:05  asargent
   Added image long long writing capabilities

   Revision 1.5  2014/04/30 15:49:26  asargent
   Changed function names from readImageGeneric and writeImageGeneric to readImageCommon and writeImageCommon to avoid misrepresentation.

   Revision 1.4  2014/02/20 19:20:45  asargent
   Added in comments for each function declaration.

*/
