/// \file pixgti.cxx
/// \brief Load and use pixel-dependent GTI files
/// \author Mike Witthoeft
/// \date $Date: 2016/08/10 21:52:49 $

#define AHLABEL ahsxs_pixgti
#define AHCVSID "$Id: pixgti.cxx,v 1.2 2016/08/10 21:52:49 mwitthoe Exp $"

#include "ahsxs/pixgti.h"
#include "ahgen/ahgen.h"

namespace ahsxs {


// ---------------------------------------------------------------------------

void loadGTIFile(const std::string& filename, GTIFile& gti) {

  // loop over all binary extensions and figure out what kind of GTI it is
  gti.m_havepixcol=false;
  ahfits::FilePtr fptmp=0;
  ahfits::open(filename,"",&fptmp);
  bool extended=false;
  if (!ahfits::isPrimary(fptmp)) {                   // extended syntax used; only use this extension
    extended=true;
  } else {
    ahfits::firstHDU(fptmp,ahfits::e_BINARY_TBL);    // may have multiple extensions; loop through all
  }
  do {
    std::string extname;
    std::string fileext;     // filename[extension]
    if (extended) {
      extname="";            // ahfits::open() will ignore this value if extended syntax used
      fileext=filename;      // extended syntax already included in filename (from user)
    } else {
      extname=ahfits::getKeyValStr(fptmp,"EXTNAME");
      fileext=filename+"["+extname+"]";
    }
    if (!ahfits::keywordExists(fptmp,"DETNAM")) {    // treated as general GTI if no DETNAM
      if (gti.m_havegen) {
        AH_INFO(ahlog::LOW) << fileext << ": already have general GTI; skipping extension" << std::endl;
        continue;   // this condition never met if extended syntax used
      }
      AH_INFO(ahlog::LOW) << fileext << ": no DETNAM defined, opening as general GTI" << std::endl;
      ahfits::open(filename,extname,&gti.m_fpgen);
      gti.m_havegen=true;
    } else {
      std::string detnam=ahgen::strtoupper(ahfits::getKeyValStr(fptmp,"DETNAM"));
      if (detnam == "PIXEL") {
        if (ahfits::haveColumn(fptmp,"PIXEL")) {    // extension has START,STOP,PIXEL
          if (gti.m_haveanypix) {
            AH_INFO(ahlog::LOW) << fileext << ": already have a pixel GTI; skipping extension" << std::endl;
            continue;   // this condition never met if extended syntax used
          }
          // open FITS pointer to extension for each pixel
          AH_INFO(ahlog::LOW) << fileext << ": opening extension for each pixel" << std::endl;
          for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {
            ahfits::open(filename,extname,&gti.m_fppix[ipix]);
            gti.m_havepix[ipix]=true;
          }
          gti.m_haveanypix=true;
          gti.m_havepixcol=true;
        } else {
          if (gti.m_havegen) {
            AH_INFO(ahlog::LOW) << fileext << ": already have general GTI; skipping extension" << std::endl;
            continue;   // this condition never met if extended syntax used
          }
          AH_INFO(ahlog::LOW) << fileext << ": opening as general GTI" << std::endl;
          ahfits::open(filename,extname,&gti.m_fpgen);
          gti.m_havegen=true;
        }
      } else {      // look for DETNAM in the form of PIXnn where nn is the pixel number
        int pixel=-1;
        if ((detnam.size() == 5) && ahgen::isNumber(detnam.substr(3,2))) {
          pixel=atoi((detnam.substr(3,2)).c_str());
          if (pixel < 0 || pixel >= ahsxs::NPIXEL) pixel=-1;   // invalid pixel number
        }
        if (pixel >= 0) {
          if (gti.m_havepix[pixel]) {
            AH_INFO(ahlog::LOW) << fileext << ": already have GTI for pixel " << pixel << "; skipping extension" << std::endl;
            continue;   // this condition never met if extended syntax used
          }
          AH_INFO(ahlog::LOW) << fileext << ": opening as pixel " << pixel << " GTI" << std::endl;
          ahfits::open(filename,extname,&gti.m_fppix[pixel]);
          gti.m_havepix[pixel]=true;
          gti.m_haveanypix=true;
        } else {
          AH_INFO(ahlog::LOW) << fileext << ": not valid GTI extension; skipping" << std::endl;
        }
      }
    }
    if (extended) break;    // extended syntax used; only processing one extension
  } while (ahfits::nextHDU(fptmp,ahfits::e_BINARY_TBL));
  ahfits::close(fptmp);

  // set up router connections
  if (gti.m_havegen) {
    gti.m_routgen=new ahfits::Router(gti.m_fpgen);
    gti.m_routgen->connectScalar(ahfits::e_READONLY,"START",gti.m_startgen,&gti.m_startgen_null);
    gti.m_routgen->connectScalar(ahfits::e_READONLY,"STOP",gti.m_stopgen,&gti.m_stopgen_null);
  }
  for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {
    if (gti.m_havepix[ipix]) {
      gti.m_routpix[ipix]=new ahfits::Router(gti.m_fppix[ipix]);
      gti.m_routpix[ipix]->connectScalar(ahfits::e_READONLY,"START",gti.m_startpix[ipix],&gti.m_startpix_null[ipix]);
      gti.m_routpix[ipix]->connectScalar(ahfits::e_READONLY,"STOP",gti.m_stoppix[ipix],&gti.m_stoppix_null[ipix]);
      if (gti.m_havepixcol) gti.m_routpix[ipix]->connectScalar(ahfits::e_READONLY,"PIXEL",gti.m_pixel[ipix]);
      ahsxs::readFirstPixelGTI(gti,ipix);
    }
  }

  // attempt to read first GTI; if no GTIs are found then close extension
  if (gti.m_havegen) {
    bool ok=ahsxs::readFirstGeneralGTI(gti);
    if (!ok) {
      gti.m_havegen=false;
      if (gti.m_routgen != 0) delete gti.m_routgen, gti.m_routgen=0;
      if (gti.m_fpgen != 0) ahfits::close(gti.m_fpgen);
    }
  }
  bool haveanypix=false;
  for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {
    if (gti.m_havepix[ipix]) {
      bool ok=ahsxs::readFirstPixelGTI(gti,ipix);
      if (!ok) {
        gti.m_havepix[ipix]=false;
        if (gti.m_routpix[ipix] != 0) delete gti.m_routpix[ipix], gti.m_routpix[ipix]=0;
        if (gti.m_fppix[ipix] != 0) ahfits::close(gti.m_fppix[ipix]);
      } else {
        haveanypix=true;
      }
    }
  }
  gti.m_haveanypix=haveanypix;

}

// ---------------------------------------------------------------------------

bool readNextGeneralGTI(GTIFile& gti) {
  if (!gti.m_havegen) return false;

  // save current START/STOP in case EOF is reached
  double start=gti.m_startgen;
  double stop=gti.m_stopgen;

  ahfits::nextRow(gti.m_fpgen);
  while (ahfits::readOK(gti.m_fpgen)) {
    ahfits::readRow(gti.m_fpgen);
    if (gti.m_startgen_null == 0 && gti.m_stopgen_null == 0) return true;
    ahfits::nextRow(gti.m_fpgen);
  }

  // can only reach this point if at EOF of GTI file and no valid match was 
  // found; reset current GTI values to last good GTI
  gti.m_startgen=start;
  gti.m_stopgen=stop;
  return false;
}

// ---------------------------------------------------------------------------

bool readNextPixelGTI(GTIFile& gti, int pixel) {
  if (!gti.m_havepix[pixel]) return false;

  // save current START/STOP in case EOF is reached
  double start=gti.m_startpix[pixel];
  double stop=gti.m_stoppix[pixel];

  ahfits::nextRow(gti.m_fppix[pixel]);
  while (ahfits::readOK(gti.m_fppix[pixel])) {
    ahfits::readRow(gti.m_fppix[pixel]);
    if (!gti.m_havepixcol || (gti.m_pixel[pixel] == pixel)) {
      if (gti.m_startpix_null[pixel] == 0 && gti.m_stoppix_null[pixel] == 0) return true;
    }
    ahfits::nextRow(gti.m_fppix[pixel]);
  }

  // can only reach this point if at EOF of GTI file and no valid match was 
  // found; reset current GTI values to last good GTI
  gti.m_startpix[pixel]=start;
  gti.m_stoppix[pixel]=stop;
  return false;
}

// ---------------------------------------------------------------------------

bool readFirstGeneralGTI(GTIFile& gti) {
  if (!gti.m_havegen) return false;
  ahfits::firstRow(gti.m_fpgen);
  if (!ahfits::readOK(gti.m_fpgen)) return false;
  ahfits::readRow(gti.m_fpgen);

  // in case row has a null START or STOP, keep reading rows
  if (gti.m_startgen_null == 1 || gti.m_stopgen_null == 1) {
    return ahsxs::readNextGeneralGTI(gti);
  }
  return true;
}

// ---------------------------------------------------------------------------

bool readFirstPixelGTI(GTIFile& gti, int pixel) {
  if (!gti.m_havepix[pixel]) return false;
  ahfits::firstRow(gti.m_fppix[pixel]);
  if (!ahfits::readOK(gti.m_fppix[pixel])) return false;
  ahfits::readRow(gti.m_fppix[pixel]);

  // Want to keep reading rows if
  //  1. have pixel-dependent GTI with PIXEL column and current row has the
  //     wrong pixel
  //  2. either START or STOP is NULL
  if (gti.m_havepixcol && (gti.m_pixel[pixel] != pixel)) {
    return ahsxs::readNextPixelGTI(gti,pixel);
  } else if (gti.m_startpix_null[pixel] == 1 || gti.m_stoppix_null[pixel] == 1) {
    return ahsxs::readNextPixelGTI(gti,pixel);
  }
  return true;
}

// ---------------------------------------------------------------------------

}  // namespace ahsxs

/* Revision Log
 $Log: pixgti.cxx,v $
 Revision 1.2  2016/08/10 21:52:49  mwitthoe
 bug-fix in SXS GTI library: correct handling of end-of-file case for pixel-dependent GTI extensions with a PIXEL column

 Revision 1.1  2016/08/10 16:27:32  mwitthoe
 ahsxs: add library for reading two types of SXS pixel-dependent GTI files: 1) one extension per pixel or 2) one extension with PIXEL column


*/
