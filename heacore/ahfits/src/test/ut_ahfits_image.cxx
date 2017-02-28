/// \file ut_ahfits_image.cxx
/// \brief unit test for ahfits_image
/// \author Andy Sargent
/// \date $Date: 2014/09/22 14:43:23 $
 
#define AHLABEL ahtime_ut_ahfits_image
#define AHCVSID "$Id: ut_ahfits_image.cxx,v 1.6 2014/09/22 14:43:23 asargent Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

// ---------------------------------------------------------------------------

void ut_write_read_image_dbl(void) {

  LABEL_TEST("Test writing/reading of a FITS image as double");

  ahfits::FilePtr ahffp;

  START_TEST("clone input file") {
    ahfits::clone("input/event_file_1.fits","!output/image_dbl.fits",&ahffp,true);
  } END_TEST

  ahfits::Img2dDbl img;
  int nx=10;
  int ny=5;

  img.resize(nx);
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny);
    for(int jj = 0; jj < ny; ++jj) {
      img[ii][jj] = jj+ny*ii;
    }
  }

  START_TEST("write image to file") {
    long * bnull = 0;
    ahfits::writeImage(ahffp,"TIMAGE",img,0.,1.,bnull);
  } END_TEST

  START_TEST("read image from file and check values") {
    ahfits::move(ahffp,"TIMAGE");
    long nx2=0;
    long ny2=0;
    ahfits::Img2dDbl img2;
    ahfits::readImage(ahffp,img2,nx2,ny2);
    if (nx2 != nx || ny2 != ny) FAIL;
    for (int ii=0; ii < nx; ++ii) {
      for (int jj=0; jj < ny; ++jj) {
        if (img2[ii][jj] != img[ii][jj]) FAIL;
      }
    }
  } END_TEST

  START_TEST("Move to image extension with an open router connection") {

    int l_segment=0;
    long * bnull = 0;
    ahfits::move(ahffp,"EVENTS");
    ahfits::Router router1(ahffp);
    router1.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);
    ahfits::writeImage(ahffp,"TIMAGE2",img,0.,1.,bnull);
    if(ahffp->m_router.size() > 0) FAIL;

  } END_TEST


  ahfits::close(ahffp);

} 

// -----------------------------------------------------------------------------

void ut_write_read_image_lng(void) {

  LABEL_TEST("Test writing/reading of a FITS image as long");

  ahfits::FilePtr ahffp;
  
  START_TEST("clone input file") {
    ahfits::clone("input/event_file_1.fits","!output/image_lng.fits",&ahffp,true);
  } END_TEST

  ahfits::Img2dLng img;
  int nx=10;
  int ny=5;

  img.resize(nx);
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny);
    for(int jj = 0; jj < ny; ++jj) {
      img[ii][jj] = jj+ny*ii;
    }
  }

  START_TEST("write image to primary") {
    long * bnull = 0;
    ahfits::FilePtr ahffp2 = 0;
    ahfits::create("!output/image_primary.fits","",&ahffp2);
    ahfits::writeImage(ahffp2,"",img,0.,1.,bnull);
    ahfits::close(ahffp2);
  } END_TEST

  START_TEST("write image to file") {
    long * bnull = 0;
    ahfits::writeImage(ahffp,"TIMAGE",img,0.,1.,bnull);
  } END_TEST

  START_TEST("read image from file and check values") {
    ahfits::move(ahffp,"TIMAGE");
    long nx2=0;
    long ny2=0;
    ahfits::Img2dLng img2;
    ahfits::readImage(ahffp,img2,nx2,ny2);
    if (nx2 != nx || ny2 != ny) FAIL;
    for (int ii=0; ii < nx; ++ii) {
      for (int jj=0; jj < ny; ++jj) {
        if (img2[ii][jj] != img[ii][jj]) FAIL;
      }
    }
  } END_TEST

  ahfits::close(ahffp);

} 

// -----------------------------------------------------------------------------

void ut_write_read_image_flt(void) {

  LABEL_TEST("Test writing/reading of a FITS image as float");

  ahfits::FilePtr ahffp;
  
  START_TEST("clone input file") {
    ahfits::clone("input/event_file_1.fits","!output/image_flt.fits",&ahffp,true);
  } END_TEST

  ahfits::Img2dFlt img;
  int nx=10;
  int ny=5;

  img.resize(nx);
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny);
    for(int jj = 0; jj < ny; ++jj) {
      img[ii][jj] = jj+ny*ii;
    }
  }

  START_TEST("write image to file") {
    long * bnull = 0;
    ahfits::writeImage(ahffp,"TIMAGE",img,0.,1.,bnull);
  } END_TEST

  START_TEST("read image from file and check values") {
    ahfits::move(ahffp,"TIMAGE");
    long nx2=0;
    long ny2=0;
    ahfits::Img2dFlt img2;
    ahfits::readImage(ahffp,img2,nx2,ny2);
    if (nx2 != nx || ny2 != ny) FAIL;
    for (int ii=0; ii < nx; ++ii) {
      for (int jj=0; jj < ny; ++jj) {
        if (img2[ii][jj] != img[ii][jj]) FAIL;
      }
    }
  } END_TEST

  ahfits::close(ahffp);

} 

// -----------------------------------------------------------------------------

void ut_write_read_image_shr(void) {

  LABEL_TEST("Test writing/reading of a FITS image as short");

  ahfits::FilePtr ahffp;
  
  START_TEST("clone input file") {
    ahfits::clone("input/event_file_1.fits","!output/image_shr.fits",&ahffp,true);
  } END_TEST

  ahfits::Img2dShr img;
  int nx=10;
  int ny=5;

  img.resize(nx);
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny);
    for(int jj = 0; jj < ny; ++jj) {
      img[ii][jj] = jj+ny*ii;
    }
  }

  START_TEST("write image to file") {
    long * bnull = 0;
    ahfits::writeImage(ahffp,"TIMAGE",img,0.,1.,bnull);
  } END_TEST

  START_TEST("read image from file and check values") {
    ahfits::move(ahffp,"TIMAGE");
    long nx2=0;
    long ny2=0;
    ahfits::Img2dShr img2;
    ahfits::readImage(ahffp,img2,nx2,ny2);
    if (nx2 != nx || ny2 != ny) FAIL;
    for (int ii=0; ii < nx; ++ii) {
      for (int jj=0; jj < ny; ++jj) {
        if (img2[ii][jj] != img[ii][jj]) FAIL;
      }
    }
  } END_TEST

  ahfits::close(ahffp);

} 

// -----------------------------------------------------------------------------

void ut_write_read_image_byt(void) {

  LABEL_TEST("Test writing/reading of a FITS image as char");

  ahfits::FilePtr ahffp;
  
  START_TEST("clone input file") {
    ahfits::clone("input/event_file_1.fits","!output/image_byt.fits",&ahffp,true);
  } END_TEST

  ahfits::Img2dByt img;
  int nx=10;
  int ny=5;

  img.resize(nx);
  for(int ii = 0; ii < nx; ++ii) {
    img[ii].resize(ny);
    for(int jj = 0; jj < ny; ++jj) {
      img[ii][jj] = jj+ny*ii;
    }
  }

  START_TEST("write image to file") {
    long * bnull = 0;
    ahfits::writeImage(ahffp,"TIMAGE",img,0.,1.,bnull);
  } END_TEST

  START_TEST("read image from file and check values") {
    ahfits::move(ahffp,"TIMAGE");
    long nx2=0;
    long ny2=0;
    ahfits::Img2dByt img2;
    ahfits::readImage(ahffp,img2,nx2,ny2);
    if (nx2 != nx || ny2 != ny) FAIL;
    for (int ii=0; ii < nx; ++ii) {
      for (int jj=0; jj < ny; ++jj) {
        if (img2[ii][jj] != img[ii][jj]) FAIL;
      }
    }
  } END_TEST

  ahfits::close(ahffp);

} 

// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ut_ahfits_image.cxx,v $
 Revision 1.6  2014/09/22 14:43:23  asargent
 New test for writing image to primary extension

 Revision 1.5  2014/02/24 15:58:20  asargent
 Added in new unit test for moving to an image extension with an open router connection.

 Revision 1.4  2014/02/20 19:25:31  asargent
 Removed cout from testing.

 Revision 1.3  2014/02/14 17:07:24  asargent
 Updated unit tests for ahfits image reading/writing

 Revision 1.2  2014/01/07 17:33:04  asargent
 Test functions for ahfits_image

 Revision 1.1  2014/01/06 16:22:03  asargent
 Test functions for ahfits_image

*/
