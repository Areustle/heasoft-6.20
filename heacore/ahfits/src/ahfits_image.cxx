/// \file ahfits_image.cxx
/// \brief ahfits: FITS file IMAGE operations
/// \author Andy Sargent
/// \date $Date: 2016/01/28 19:17:29 $

#define AHLABEL ahfits_ahfits_image
#define AHCVSID "$Id: ahfits_image.cxx,v 1.13 2016/01/28 19:17:29 rshill Exp $"

#include "ahfits/ahfits_image.h"
#include "ahfits/ahfits_header.h"
#include "ahfits/ahfits_file.h"
#include "ahfits/ahfits_router.h"

#include "ahlog/ahlog.h"

#include "fitsio.h"

namespace ahfits {

// -----------------------------------------------------------------------------

void writeImageCommon(FilePtr & ahffp, const std::string extname, double bzero,
                       double bscale, long * bnull, int nx, int ny, int bitpix,
                       int & status) {

  long naxes[2] = {0};           // 2-element array with NAXIS1 and NAXIS2 values

  // ahffp must point to an opened FITS file
  if (0 == ahffp)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"destination AHFITS pointer is NULL, but should not be");
  if (0 == ahffp->m_cfitsfp)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"destination CFITS pointer is NULL, but should not be");

  // flush any buffers that may exist for HDU ahffp is pointing to; disconnect routers
  ahfits::flushAndClearBuffers(ahffp);
  ahffp->m_router.clear();

  // extension name for image must not already exist
  if(!extname.empty()) {
    if(ahfits::HDUExists(ahffp,extname))
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"HDU "+extname+" already exists.");
  }

  // input image must have a non-zero size
  if(0 == nx)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"Vector img is empty");

  // Create image extension
  naxes[0] = nx;
  naxes[1] = ny;   // Assuming image is a valid 2D image

  if(extname.empty()) {
    if(0 != fits_resize_img(ahffp->m_cfitsfp,bitpix,2,naxes,&status)) 
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"failed to resize primary image extension: "+extname+statusMsg(status));
  } else {
    if(0 != fits_create_img(ahffp->m_cfitsfp,bitpix,2,naxes,&status))
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"failed to create IMAGE extension: "+extname+statusMsg(status));
    ahfits::writeKeyValStr(ahffp,"EXTNAME",extname,"");
    ahfits::move(ahffp,extname);     // Move to new extension
  }

  // write BZERO, BSCALE, and BNULL keywords
  ahfits::writeKeyValDbl(ahffp,"BZERO",bzero,"");  
  ahfits::writeKeyValDbl(ahffp,"BSCALE",bscale,"");
  if (bnull != 0) ahfits::writeKeyValLLong(ahffp,"BNULL",*bnull,"");

}

// -----------------------------------------------------------------------------

void writeImage(FilePtr & ahffp, const std::string extname, Img2dDbl img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  double * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,DOUBLE_IMG,status);
  
  imgarr = new double[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TDOUBLE,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void writeImage(FilePtr & ahffp, const std::string extname, Img2dLng img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  long * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,LONG_IMG,status);
  
  imgarr = new long[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TLONG,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void writeImage(FilePtr & ahffp, const std::string extname, Img2dLngLng img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  long long * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,LONGLONG_IMG,status);
  
  imgarr = new long long[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TLONGLONG,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------
void writeImage(FilePtr & ahffp, const std::string extname, Img2dFlt img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  float * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,FLOAT_IMG,status);
  
  imgarr = new float[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TFLOAT,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void writeImage(FilePtr & ahffp, const std::string extname, Img2dShr img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  short * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,SHORT_IMG,status);
  
  imgarr = new short[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TSHORT,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void writeImage(FilePtr & ahffp, const std::string extname, Img2dByt img, 
                double bzero, double bscale, long * bnull) {

  int status = 0;                // status of cfitsio calls
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  int nx = 0;
  int ny = 0;
  int rowLength = 0;
  char * imgarr;
  
  nx = img.size();
  ny = img[0].size();    // Assuming image is a valid 2D image

  // Verify that all rows are same length
  for(int ii = 0; ii < nx; ++ii) {
    rowLength = img[ii].size();
    if(rowLength != ny) {
      AH_THROW_RUNTIME("Failed to write IMAGE: Invalid image-vector size in one or more rows");
    }
  }

  writeImageCommon(ahffp,extname,bzero,bscale,bnull,nx,ny,BYTE_IMG,status);
  
  imgarr = new char[nx*ny];
  
  // copy input image 2D vector to temporary 2D array
  for(int ii = 0; ii < nx; ++ii) {
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      imgarr[kk] = img[ii][jj];
    }
  }

  // define initial and final image positions so that whole image is written
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  // write image
  if(0 != fits_write_subset(ahffp->m_cfitsfp,TBYTE,fpixel,lpixel,imgarr,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write IMAGE"+statusMsg(status));
  }

  // clean-up
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void readImageCommon(FilePtr & ahffp, long & nx, long & ny, long * bnull,
                      long * fpixel, long * lpixel, long * inc) {

  // +++ 2014-01-02 AS: Is there always a bnull?
  // +++ 2014-01-06 MW Is BNULL relevant for TDOUBLE images?
  
  // get size of 2D image
  nx = ahfits::getKeyValLLong(ahffp,"NAXIS1");
  ny = ahfits::getKeyValLLong(ahffp,"NAXIS2");
  
  // define initial/final image positions and skips so that whole image is read
  fpixel[0]=1;
  fpixel[1]=1;
  lpixel[0]=nx;
  lpixel[1]=ny;
  inc[0]=1;       // a value of 2 would only read every other X value
  inc[1]=1;
  
  // ahffp must point to an image HDU of an opened FITS file
  if (0 == ahffp)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"AHFITS pointer is NULL, but should not be");
  if (0 == ahffp->m_cfitsfp)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"CFITS pointer is NULL, but should not be");
  if(!ahfits::isImage(ahffp))
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to open IMAGE");

  if(nx < 1 || ny < 1) AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"FITS IMAGE is empty");
  
  // get NULL value, if present
  if(ahfits::keywordExists(ahffp,"BNULL")) bnull = new long(ahfits::getKeyValLLong(ahffp,"BNULL"));
  
}

// -----------------------------------------------------------------------------

void readImage(FilePtr & ahffp, Img2dDbl & img, long & nx, long & ny) {

  int status = 0;                // status of cfitsio calls
  double * imgarr;               // temporary array for writing data
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  long inc[2] = {0};             // do not skip any points in image
  long * bnull = 0;              // NULL value

  readImageCommon(ahffp,nx,ny,bnull,fpixel,lpixel,inc);
  
  // allocate memory for temporary array
  imgarr = new double[nx*ny];

  // read image from FITS file into temporary array
  if(0 != fits_read_subset(ahffp->m_cfitsfp,TDOUBLE,fpixel,lpixel,inc,bnull,imgarr,0,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read IMAGE"+statusMsg(status));
  }

  // copy image from temporary array to output 2D vector  
  img.resize(nx); // Allocate memory x-direction
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny); // Allocate memory y-direction
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      img[ii][jj] = imgarr[kk];
    }
  }
 
  // clean-up  
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void readImage(FilePtr & ahffp, Img2dLng & img, long & nx, long & ny) {

  int status = 0;              // status of cfitsio calls
  long * imgarr;               // temporary array for writing data
  long fpixel[2] = {0};        // 2D array holding initial position of image to write
  long lpixel[2] = {0};        // 2D array holding final position of image to write
  long inc[2] = {0};           // do not skip any points in image
  long * bnull = 0;            // NULL value

  readImageCommon(ahffp,nx,ny,bnull,fpixel,lpixel,inc);
  
  // allocate memory for temporary array
  imgarr = new long[nx*ny];

  // read image from FITS file into temporary array
  if(0 != fits_read_subset(ahffp->m_cfitsfp,TLONG,fpixel,lpixel,inc,bnull,imgarr,0,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read IMAGE"+statusMsg(status));
  }

  // copy image from temporary array to output 2D vector  
  img.resize(nx); // Allocate memory x-direction
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny); // Allocate memory y-direction
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      img[ii][jj] = imgarr[kk];
    }
  }
 
  // clean-up  
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void readImage(FilePtr & ahffp, Img2dFlt & img, long & nx, long & ny) {

  int status = 0;               // status of cfitsio calls
  float * imgarr;               // temporary array for writing data
  long fpixel[2] = {0};         // 2D array holding initial position of image to write
  long lpixel[2] = {0};         // 2D array holding final position of image to write
  long inc[2] = {0};            // do not skip any points in image
  long * bnull = 0;             // NULL value

  readImageCommon(ahffp,nx,ny,bnull,fpixel,lpixel,inc);
  
  // allocate memory for temporary array
  imgarr = new float[nx*ny];

  // read image from FITS file into temporary array
  if(0 != fits_read_subset(ahffp->m_cfitsfp,TFLOAT,fpixel,lpixel,inc,bnull,imgarr,0,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read IMAGE"+statusMsg(status));
  }

  // copy image from temporary array to output 2D vector  
  img.resize(nx); // Allocate memory x-direction
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny); // Allocate memory y-direction
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      img[ii][jj] = imgarr[kk];
    }
  }
 
  // clean-up  
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void readImage(FilePtr & ahffp, Img2dShr & img, long & nx, long & ny) {

  int status = 0;               // status of cfitsio calls
  short * imgarr;               // temporary array for writing data
  long fpixel[2] = {0};         // 2D array holding initial position of image to write
  long lpixel[2] = {0};         // 2D array holding final position of image to write
  long inc[2] = {0};            // do not skip any points in image
  long * bnull = 0;             // NULL value

  readImageCommon(ahffp,nx,ny,bnull,fpixel,lpixel,inc);
  
  // allocate memory for temporary array
  imgarr = new short[nx*ny];

  // read image from FITS file into temporary array
  if(0 != fits_read_subset(ahffp->m_cfitsfp,TSHORT,fpixel,lpixel,inc,bnull,imgarr,0,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read IMAGE"+statusMsg(status));
  }

  // copy image from temporary array to output 2D vector  
  img.resize(nx); // Allocate memory x-direction
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny); // Allocate memory y-direction
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      img[ii][jj] = imgarr[kk];
    }
  }
 
  // clean-up  
  delete [] imgarr;

}

// -----------------------------------------------------------------------------

void readImage(FilePtr & ahffp, Img2dByt & img, long & nx, long & ny) {

  int status = 0;                // status of cfitsio calls
  char * imgarr;               // temporary array for writing data
  long fpixel[2] = {0};          // 2D array holding initial position of image to write
  long lpixel[2] = {0};          // 2D array holding final position of image to write
  long inc[2] = {0};             // do not skip any points in image
  long * bnull = 0;            // NULL value

  readImageCommon(ahffp,nx,ny,bnull,fpixel,lpixel,inc);
  
  // allocate memory for temporary array
  imgarr = new char[nx*ny];

  // read image from FITS file into temporary array
  if(0 != fits_read_subset(ahffp->m_cfitsfp,TBYTE,fpixel,lpixel,inc,bnull,imgarr,0,&status)) {
    delete [] imgarr;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read IMAGE"+statusMsg(status));
  }

  // copy image from temporary array to output 2D vector  
  img.resize(nx); // Allocate memory x-direction
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny); // Allocate memory y-direction
    for(int jj = 0; jj < ny; ++jj) {
      int kk = jj+ny*ii;
      img[ii][jj] = imgarr[kk];
    }
  }
 
  // clean-up  
  delete [] imgarr;

}

// -----------------------------------------------------------------------------


} // namespace ahfits

/* Revision Log
   $Log: ahfits_image.cxx,v $
   Revision 1.13  2016/01/28 19:17:29  rshill
   Edited wording of error message that was Astro-H SXI-specific.

   Revision 1.12  2015/08/17 20:46:36  asargent
   Added image long long writing capabilities

   Revision 1.11  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.10  2014/11/26 15:12:45  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.9  2014/09/22 14:40:10  asargent
   Added functionality to write to primary extension for images.

   Revision 1.8  2014/04/30 15:50:44  asargent
   Changed function names from readImageGeneric and writeImageGeneric to readImageCommon and writeImageCommon to avoid misrepresentation. Added in verification during image writing to check that all rows are the same length.

   Revision 1.7  2014/04/17 15:38:27  mwitthoe
   ahfits: fix bug in buffer which sometimes causes an d an extra row to be added to the end of an appended file (issue 379); simplified bug fix from issue 344

   Revision 1.6  2014/02/24 15:21:00  asargent
   Fixed bug where open router connections were causing an excess number of rows to be written to the output file. Closed any open router connections. See issue 344.

   Revision 1.5  2014/02/20 19:22:43  asargent
   Added in comments to image functions.

   Revision 1.4  2014/02/14 17:08:43  asargent
   Updated unit tests for ahfits image reading/writing

   Revision 1.3  2014/01/14 16:35:37  mwitthoe
   ahfits: now flush buffers of binary tables before making a new image HDU; skip updating connections and cleaining up buffering if in image extension (this stuff happens automatically when closing a file or moving between HDUs)

   Revision 1.2  2014/01/07 17:30:16  asargent
   Added functions for integer reading/writing of images

   Revision 1.1  2014/01/06 16:22:59  asargent
   Initial version of ahfits_image for reading and writing images to FITS files


*/
