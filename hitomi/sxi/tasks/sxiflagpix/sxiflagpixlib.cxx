/// \file sxiflagpixlib.cxx
/// \brief functions to act on the CALDB and FITS files containing the SXI pixel indices
/// \author Andy Sargent
/// \date $Date: 2016/02/10 19:53:28 $

#define AHLABEL tool_sxiflagpix_sxiflagpixlib
#define AHCVSID "$Id: sxiflagpixlib.cxx,v 1.28 2016/02/10 19:53:28 mdutka Exp $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "sxiflagpixlib.h"
#include <sstream> 

namespace sxiflagpixlib {

void loadPix(const std::string & filename, PixArray & dat, double tstart_in, double tstop_in) {
  
  std::string inst; // Instrument variable string
 
  // Local variables for router connection
  double l_time;
  int l_ccdid;
  int l_actx;
  int l_acty;
  int numpix = 0;
 
  clearPix(dat);
  // open file
  ahfits::FilePtr fptr; // File pointer for passed FITS file
  ahfits::open(filename,"PIXELS",&fptr);
  
  // Check for correct instrument
  inst = ahfits::getKeyValStr(fptr, "INSTRUME");
  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in " + filename + " should be SXI, not " + inst);
  }

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file
  
  // make connections to local variables
  router.connectScalar(ahfits::e_READONLY, "START", l_time);
  router.connectScalar(ahfits::e_READONLY, "ACTX", l_actx);
  router.connectScalar(ahfits::e_READONLY, "ACTY", l_acty);
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);

  // read table
  for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
 
    ahfits::readRow(fptr);
    
    if(l_time >= tstart_in && l_time <= tstop_in) {
      dat.m_actx.push_back(l_actx);
      dat.m_acty.push_back(l_acty);
      dat.m_ccdid.push_back(l_ccdid);
      numpix++;

      // Verify values in input file
      if (l_ccdid > 3 || l_ccdid < 0) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of CCD_ID in file " + filename + 
                             " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }
      if (l_actx > 640 || l_actx  < 1) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of ACTX in file " + filename + 
                              " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }
      if (l_acty > 640 || l_acty < 1) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of ACTY in file " + filename + 
                             " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }
   
    } // end if within observation time
  
  } // end loop events

  // Set total number of pixels
  dat.m_numpix = numpix;

 

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadBadPix(const std::string & filename, double tstartEvt, PixArray & dat) {

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  std::string inst; // Instrument variable string

  // Local variables for router connection
  double l_time = 0.;
  double chunkTime = 0.;
  int l_ccdid = 0;
  int l_actx = 0;
  int l_acty = 0;
  int l_yextend = 0;
  int numpix = 0; 
  int startRow = 0;

  clearPix(dat);
  // open file

  ahfits::open(filename,"BADPIX",&fptr);

  // Check for correct instrument
  inst = ahfits::getKeyValStr(fptr, "INSTRUME");

  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in " + filename + " should be SXI, not " + inst);
  }

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY, "TIME", l_time);
  router.connectScalar(ahfits::e_READONLY, "ACTX", l_actx);
  router.connectScalar(ahfits::e_READONLY, "ACTY", l_acty);
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);
  router.connectScalar(ahfits::e_READONLY, "YEXTEND", l_yextend);

  //read first row
  ahfits::firstRow(fptr); 
  ahfits::readRow(fptr);
  if(tstartEvt < l_time) { 
    AH_THROW_RUNTIME("First time in " + filename + " should not be greater than TSTART in event file"); 
  }

  chunkTime = l_time;
  startRow = ahfits::currentRow(fptr);
  while (tstartEvt > l_time && ahfits::readOK(fptr)) {
    if (chunkTime != l_time) {
      chunkTime = l_time;
      startRow = ahfits::currentRow(fptr);
    }
    ahfits::nextRow(fptr);
    ahfits::readRow(fptr);
  }

  ahfits::gotoRow(fptr, startRow);
  AH_DEBUG << "Starting row of time block in " << filename << " = " << startRow << std::endl; 
  while (chunkTime == l_time && ahfits::readOK(fptr)) {
    dat.m_actx.push_back(l_actx);
    dat.m_acty.push_back(l_acty);
    dat.m_ccdid.push_back(l_ccdid);
    dat.m_yextend.push_back(l_yextend);
    numpix++;
    
    // Verify values in input file
    if (l_ccdid > 3 || l_ccdid < 0) {
      std::stringstream ss;
      ss << ahfits::currentRow(fptr);
      std::string errMsg = "Invalid value of CCD_ID in file " + filename + 
                           " at row " + ss.str() + 
                           " Aborting...";
      AH_THROW_RUNTIME(errMsg);
    }
    if (l_actx > 640 || l_actx  < 1) {
      std::stringstream ss;
      ss << ahfits::currentRow(fptr);
      std::string errMsg = "Invalid value of ACTX in file " + filename + 
                           " at row " + ss.str() + 
                           " Aborting...";
      AH_THROW_RUNTIME(errMsg);
    }
    if (l_acty > 640 || l_acty < 1) {
      std::stringstream ss;
      ss << ahfits::currentRow(fptr);
      std::string errMsg = "Invalid value of ACTY in file " + filename + 
                           " at row " + ss.str() + 
                           " Aborting...";
      AH_THROW_RUNTIME(errMsg);
    }

    ahfits::nextRow(fptr);
    ahfits::readRow(fptr);
  }  

  // Set total number of pixels
  dat.m_numpix = numpix;

  // close FITS file
  ahfits::close(fptr);

}

// ****************************************************************************

void loadHotPix(const std::string & filename, double tstartEvt, double tstopEvt, PixArray & dat) {

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  std::string inst; // Instrument variable string

  // Local variables for router connection
  double l_time = 0.;
  int l_ccdid = 0;
  int l_actx = 0;
  int l_acty = 0;
  int numpix = 0; 
  int startRow = 0;

  clearPix(dat);

  // Search for hot pixels +/- 1 day around observation
  double hot_start = tstartEvt-24*60*60;
  double hot_stop = tstopEvt+24*60*60;

  // open file
  ahfits::open(filename,"HOTPIX",&fptr);

  // Check for correct instrument
  inst = ahfits::getKeyValStr(fptr, "INSTRUME");

  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in " + filename + " should be SXI, not " + inst);
  }

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file

  router.connectScalar(ahfits::e_READONLY, "TIME", l_time);
  router.connectScalar(ahfits::e_READONLY, "ACTX", l_actx);
  router.connectScalar(ahfits::e_READONLY, "ACTY", l_acty);
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);

  AH_DEBUG << "Looking for hot pixels " << filename << " = " << startRow << std::endl; 
  for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    
    ahfits::readRow(fptr);
    
    // Skip hot pixels outside of time range
    if(l_time >= hot_start && l_time <= hot_stop ) {
      dat.m_actx.push_back(l_actx);
      dat.m_acty.push_back(l_acty);
      dat.m_ccdid.push_back(l_ccdid);
      numpix++;

      // Verify values in input file
      if (l_ccdid > 3 || l_ccdid < 0) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of CCD_ID in file " + filename + 
                             " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }
      if (l_actx > 640 || l_actx  < 1) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of ACTX in file " + filename + 
                              " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }
      if (l_acty > 640 || l_acty < 1) {
        std::stringstream ss;
        ss << ahfits::currentRow(fptr);
        std::string errMsg = "Invalid value of ACTY in file " + filename + 
                             " at row " + ss.str() + 
                             " Aborting...";
        AH_THROW_RUNTIME(errMsg);
      }

    }
  }  

  // Set total number of pixels
  dat.m_numpix = numpix;

  // close FITS file
  ahfits::close(fptr);
}

// ****************************************************************************

void clearPix(PixArray & dat) {

    // Clear all data from dat
    dat.m_actx.clear();
    dat.m_acty.clear();
    dat.m_ccdid.clear();
    dat.m_yextend.clear();
}

// ****************************************************************************

} // namespace sxiflagpixlib

/* Revision Log
 $Log: sxiflagpixlib.cxx,v $
 Revision 1.28  2016/02/10 19:53:28  mdutka
 Adding check to see if input pixel files are valid

 Revision 1.27  2015/08/17 23:45:03  asargent
 Bug fixes: changed image writing to long long, fixed yextend boundary, forced LL with hex values. Cleaned up mask file reading. Added +/- 1 day for hot pixels

 Revision 1.26  2015/08/10 13:44:04  mwitthoe
 sxiflagpix: conform to standard header; add parameter stamping to log file; make yes/no lowercase in parameter file; general clean-up

 Revision 1.25  2015/06/08 14:13:26  mdutka
 changing extname of hotpix file pixel to hotpix

 Revision 1.24  2015/06/02 20:23:47  mdutka
 fixed bug with hotpix reading

 Revision 1.23  2015/06/02 20:06:10  mdutka
 fixed bug with loading hotpix file

 Revision 1.22  2015/06/02 15:57:58  mdutka
 fixing bug with reading badpix file

 Revision 1.21  2015/06/02 15:32:20  mdutka
 corrected reading of sxi badpix file

 Revision 1.20  2015/05/29 12:43:02  mdutka
 debugged CALDB query

 Revision 1.19  2015/05/20 21:03:05  mdutka
 commiting change from hiroya, chages detailed in trf 04-27

 Revision 1.18  2015/05/12 18:50:43  mdutka
 checking in incremental changes after firt report from eric miller issue #490

 Revision 1.17  2015/03/18 15:04:38  mdutka
 sxi tool changes see issue #490

 Revision 1.16  2014/12/11 21:16:36  asargent
 Bug fixes: -1 was being written to good elements in output image file. Bug fix is in maskAreaDiscrimination routine. Added constraints to segment and uses the active readnode to determine which coordinates to flag. Sped up routine checkBad3x3 by passing in statusMask by reference. New parameter copyphas to copy phas from telemetry before updating.

 Revision 1.15  2014/09/12 20:06:22  asargent
 Revamped sxiflagpix tool: statusMask is now stored in ACT coordinates rather than DET.

 Revision 1.14  2014/06/23 13:56:13  asargent
 Updated output bad pixel file to include entire header information from input event file. Changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

 Revision 1.13  2014/05/14 20:44:56  asargent
 Removed extraneous functions

 Revision 1.12  2014/02/14 17:18:18  asargent
 Updated ahfits image reading/writing

 Revision 1.11  2014/02/12 15:11:35  asargent
 General housecleaning to better match other tools. Cleaned up and reorganized comments and variables, converted flagged pixels data to vectors with better memory allocation.

 Revision 1.10  2014/01/17 21:36:38  asargent
 Removed commented out coordinate check

 Revision 1.9  2014/01/17 21:30:03  asargent
 Added in a validation check for pixel DET coordinates

 Revision 1.8  2014/01/09 21:58:24  asargent
 Added in variable comments

 Revision 1.7  2014/01/07 18:01:56  asargent
 Added in image read/write access from ahfits. Fixed passing around of arrays and changed statusMask to type ahfits::Img2dLng.

 Revision 1.6  2013/12/19 20:45:30  asargent
 Condensed variable connections to only necessary variables. Added in some placeholders for mask array CALDB FITS IMAGE file.

 Revision 1.5  2013/12/12 22:15:07  asargent
 Various changes to function parameters and combination of flagged pixel input files

 Revision 1.4  2013/11/22 14:17:06  asargent
 New placeholder function for getting time based bad pixel arrays

 Revision 1.3  2013/11/18 18:07:23  asargent
 More function call optionssxiflagpixlib.cxx

 Revision 1.2  2013/11/15 22:30:01  asargent
 Added checks to verify GTI. Added options to retrieve array data from get_(detx,dety,yextend,segment) functions.

 Revision 1.1  2013/11/14 21:42:38  asargent
 "Initial version of sxiflagpixlib.cxx"


*/
