/// \file sxipi.cxx
/// \brief Calculate pulse invariant (PI) value exactly proportional to X-Ray
///  energy for each event
/// \author Andy Sargent
/// \date $Date: 2016/07/27 22:39:11 $
/// \version 1.0

/**

\defgroup tool_sxipi SXI PI (sxipi)
@ingroup mod_sxi_tasks

Calculate a pulse invariant (PI), a value exactly proportional to the X-ray 
energy (1 ch = 6 eV), for each event, by applying several corrections and 
grade assignment. This task consists of 5 steps: (1) even/odd correction, 
(2) charge trail correction, (3) charge transfer inefficiency (CTI) 
correction, (4) Grade assignment, and (5) PHA to PI conversion (gain 
correction). Each CCD has two related analog ASICs where detected events are 
processed. One of the ASICs processes charges from even-RAWX pixels, whereas 
the other processes those from odd-RAWX pixels. 

STATUS bits set:

 Bit (zero-based)  Category     Description
     25            general       PHAS[0] < event threshold
     26            vtevnodd      Video temperature is out of range
     27            vtevnodd      Lack of video temp HK at time close to event
     28            chtrail/CTI   Correction value is negative
     29            general       Null value by correction process

Source files:

  sxipi.cxx
  sxipilib.h
  sxipilib.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-31   MSD    Clean-up code

*/

#define AHLABEL tool_sxipi
#define AHCVSID "$Id: sxipi.cxx,v 1.93 2016/07/27 22:39:11 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sxipilib.h"
#include "ahmission/ahmission.h" // procstatus
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"         // for random numbers
#include "ahmission/caldb.h"


// Regional includes
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "headas_stdio.h"        // for debugging of hdgtcalf only
#include "fitsio.h"              // fitsio functions 
#include "headas.h"              // expand_item_list  

#include <cmath>                 // define NAN
#include <sstream>               // create a stream for a changing variable name


/** \addtogroup tool_sxipi
 *  @{
 */

/// \brief Get parameter values
/// \param[out] param         Structure containing data from parameter file
void getPar(sxipilib::Params & param);

/// \brief Copy contents of infile to outfile; initialize CALDB data into structures
/// \param[in,out] param      Structure containing data from parameter file
/// \param[out] fpout         File pointer to event output file
/// \param[out] hkdat         Data structure to hold information from HK file 
/// \param[out] eodat         Data structure holding information from even odd CALDB file
/// \param[out] chtrdat       Data structure holding information from charge trail CALDB file
/// \param[out] ctidat        Data structure holding information from CTI CALDB file
/// \param[out] spthdat       Data structure holding information from split threshold CALDB file
/// \param[out] gradedat      Data structure holding information from hit pattern CALDB file
/// \param[out] gaindat       Data structure holding information from gain CALDB file
/// \param[out] keydat        Data structure to hold keyword information from input file
void initialize(sxipilib::Params & param, ahfits::FilePtr & fpout, sxipilib::HKData & hkdat,
                sxipilib::EvenOddData & eodat, sxipilib::ChargeTrailData & chtrdat,
                sxipilib::CTIData & ctidat, sxipilib::AnomRegData & AnomRegData,
                sxipilib::SpThData & spthdat, sxipilib::GradeData & gradedat, 
                sxipilib::GainData & gaindat, sxipilib::Keywords & keydat,
                int * evtThresInt);

/// \brief Run event loop through corrections and retrieve hit pattern data
/// \param[in] param          Structure containing data from parameter file
/// \param[in,out] fpout      File pointer to event output file
/// \param[in] hkdat          Data structure to hold information from HK file 
/// \param[in] eodat          Data structure holding information from even odd CALDB file
/// \param[in] chtrdat        Data structure holding information from charge trail CALDB file
/// \param[in] ctidat         Data structure holding information from CTI CALDB file
/// \param[in] spthdat        Data structure holding information from split threshold CALDB file
/// \param[in] gradedat       Data structure holding information from hit pattern CALDB file
/// \param[in] gaindat        Data structure holding information from gain CALDB file
/// \param[in] keydat         Data structure to hold keyword information from input file
void doWork(const sxipilib::Params& param, ahfits::FilePtr & fpout, 
            sxipilib::HKData & hkdat, const sxipilib::EvenOddData& eodat,
            const sxipilib::ChargeTrailData& chtrdat,  const sxipilib::CTIData& ctidat, sxipilib::AnomRegData & AnomRegData,
            const sxipilib::SpThData& spthdat, const sxipilib::GradeData& gradedat,
            const sxipilib::GainData& gaindat, const sxipilib::Keywords& keydat,
            const int * evtThresInt);

/// \brief Close file pointers
/// \param[out] fpout         File pointer to event output file
void finalize(ahfits::FilePtr & fp);

// ****************************************************************************

/// \brief sxipi tool
int main(int argc, char** argv) {

  sxipilib::Params param;            // Structure to store parameters for par file
  
  ahfits::FilePtr fpout = 0;         // File pointer to event output file

  sxipilib::HKData hkdat;
  sxipilib::EvenOddData eodat;       // Structure to store even/odd data from CALDB
  sxipilib::ChargeTrailData chtrdat; // Structure to store charge trail data from CALDB
  sxipilib::CTIData ctidat;          // Structure to store CTI data from CALDB
  sxipilib::AnomRegData AnomRegData; // Structure to store anomaly region data 
  sxipilib::SpThData spthdat;        // Structure to store split threshold data from CALDB
  sxipilib::GradeData gradedat;      // Structure to store grade data from CALDB
  sxipilib::GainData gaindat;        // Structure to store gain data from CALDB

  // Keyword EVENTTHR
  int evtThresInt[8] = { 0 };                // Event threshold array, one for each segment

  sxipilib::Keywords keydat;
  
  int status = ahapp::startUp(argc, argv, TOOLTAG);
  HDIO_init(); // for debugging of HDgtcalf only 

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(param);
      ahapp::writeParametersToLog();
      initialize(param, fpout, hkdat, eodat, chtrdat, ctidat, AnomRegData, spthdat, gradedat, gaindat, keydat, evtThresInt);
      doWork(param, fpout, hkdat, eodat, chtrdat, ctidat, AnomRegData, spthdat, gradedat, gaindat, keydat, evtThresInt);
      finalize(fpout);
      ahapp::shutDown();
    } else {
      try {
        getPar(param);
        initialize(param, fpout, hkdat, eodat, chtrdat, ctidat, AnomRegData, spthdat, gradedat, gaindat, keydat, evtThresInt);
        doWork(param, fpout, hkdat, eodat, chtrdat, ctidat, AnomRegData, spthdat, gradedat, gaindat, keydat, evtThresInt);
      } catch (const std::exception & x) {
          ahapp::writeParametersToLog();
          status = 1;
          AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout);
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;

} // end main

// ****************************************************************************

void getPar(sxipilib::Params & param) {
  
  param.m_infile = ahapp::getParString("infile");
  param.m_outfile = ahapp::getParString("outfile");
  param.m_hkfile = ahapp::getParString("hkfile");
  
  param.m_hkext = ahapp::getParString("hkext");
  param.m_hkcol = ahapp::getParString("hkcolstem");
  param.m_hkvideoid = ahapp::getParString("hkvideoid");

  param.m_vtevnodd = ahapp::getParString("vtevnoddfile");   // CALDB
  param.m_chtrailfile = ahapp::getParString("chtrailfile"); // CALDB
  param.m_ctifile = ahapp::getParString("ctifile");         // CALDB
  param.m_spthfile = ahapp::getParString("spthfile");       // CALDB
  param.m_gainfile = ahapp::getParString("gainfile");       // CALDB
  param.m_patternfile = ahapp::getParString("patternfile"); // CALDB

  param.m_startcol = ahapp::getParString("startcol");

  param.m_evnoddcor = ahapp::getParBool("evnoddcor");
  param.m_chtrailcor = ahapp::getParBool("chtrailcor");
  param.m_cticor = ahapp::getParBool("cticor");
  param.m_gaincor = ahapp::getParBool("gaincor");
  param.m_ctigrade = ahapp::getParBool("ctigrade");
  param.m_copygrade = ahapp::getParBool("copygrade");
  param.m_phcut = ahapp::getParString("phcut");

  param.m_badpixopt = ahapp::getParInt("badpixopt");

  param.m_spthiter = ahapp::getParBool("spthiter");
  param.m_spthcaldb = ahapp::getParBool("spthcaldb");
  
  param.m_spthoffset = ahapp::getParDouble("spthoffset");
  param.m_spthslope = ahapp::getParDouble("spthslope");

  param.m_evtthre = ahapp::getParString("evtthre");
  param.m_negthre = ahapp::getParInt("negthre");

  param.m_deltatime = ahapp::getParInt("deltatime");
  
  param.m_debugcol = ahapp::getParBool("debugcol");

  param.m_randomize = ahapp::getParBool("randomize");
  param.m_seed = ahapp::getParInt("seed");

  // convert hkvideoid into a vector of chars                     
  char* hkvideoid_char=(char*)param.m_hkvideoid.c_str();
  char** items=0;
  int nitems=0;
  int status=0;
  int trim=1;         // trim spaces                                                                                         
  int skip=1;         // exclude empty items                                                                                                 
  int guard=0;        // do not protect against commas in parentheses                                                                        
  items=expand_item_list(hkvideoid_char, &nitems, ',',trim,skip,guard,&status);
  if (status != 0)
    AH_THROW_RUNTIME("invalid value for eminin parameter; expect list of values separated by commas");
  for (int ii = 0; ii < nitems; ++ii) {
    param.m_hkvid_list.push_back(*items[ii]);
  }
  
  if (param.m_startcol != "PHAS" && !param.m_debugcol) {
    AH_THROW_RUNTIME("Debug columns are turned off and starting column is not PHAS");
  } 

} // end getPar

// ****************************************************************************

void initialize(sxipilib::Params & param, ahfits::FilePtr & fpout, sxipilib::HKData & hkdat,
                sxipilib::EvenOddData & eodat, sxipilib::ChargeTrailData & chtrdat,
                sxipilib::CTIData & ctidat, sxipilib::AnomRegData & AnomRegData, sxipilib::SpThData & spthdat,
                sxipilib::GradeData & gradedat, sxipilib::GainData & gaindat,
                sxipilib::Keywords & keydat, int * evtThresInt) {

  // extension name for input file; strings to store INSTRUME keyword value. 
  std::string extname = "EVENTS"; // Extension name for input/output file
  std::string inst;               // Name of current instrument
  std::string startcol;
  std::string datamode;           // DATAMODE header keyword in event file  
  std::string dmode;              // first 7 characters of dmode

  double tstart = 0.;             // Observation start time
  double tstop = 0.;              // Observation stop time

  std::string telescop;           // TELESCOP header keyword event file

  bool hastnull = false;          // Boolean check for TNULL

  std::string huclegthstr = "";
  std::string vucheghtstr = "";
  std::string imgheghtstr = "";
  std::string actvnodestr = "";
  
  // construct expr argument for boundary parameters for CALDB searches
  std::string expr;
  std::stringstream cistatus_ss;
  std::string l_evtthres;       // event threshold from parameter

  // copy contents of infile to outfile and return opened output file (fpout);
  // if editing input file in-place (allowed with last argument = true),
  // fpout will point to the opened input file.
  ahfits::clone(param.m_infile, param.m_outfile, &fpout, true);
  if (ahfits::isPrimary(fpout)) ahfits::move(fpout, extname);   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fpout,param.m_infile);
  
  // Check if FITS file pointer is valid
  if(0==ahfits::numRows(fpout)) {
    AH_THROW_RUNTIME("No rows in input event file");
  }
  
  // Check for correct instrument
  inst = ahfits::getKeyValStr(fpout, "INSTRUME");
  
  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in inputfile should be SXI, not " + inst);
  }
  
  // Read keywords from event file
  tstart = ahfits::getKeyValDbl(fpout, "TSTART");
  tstop = ahfits::getKeyValDbl(fpout, "TSTOP");
  telescop = ahfits::getKeyValStr(fpout, "TELESCOP");
  datamode = ahfits::getKeyValStr(fpout, "DATAMODE");
  huclegthstr = ahfits::getKeyValStr(fpout, "HUCLEGTH");
  vucheghtstr = ahfits::getKeyValStr(fpout, "VUCHEGHT");
  imgheghtstr = ahfits::getKeyValStr(fpout, "IMGHEGHT");
  actvnodestr = ahfits::getKeyValStr(fpout, "ACTVNODE");
  keydat.m_winSt = ahfits::getKeyValLLong(fpout, "WIN_ST");
  keydat.m_winSize = ahfits::getKeyValLLong(fpout, "WIN_SIZE");
  keydat.m_ciStatus = ahfits::getKeyValLLong(fpout, "CISTATUS");
  keydat.m_ciPeriod = ahfits::getKeyValLLong(fpout, "CIPERIOD");
  keydat.m_ciOffset = ahfits::getKeyValLLong(fpout, "CIOFFSET");
  keydat.m_ciFirst = ahfits::getKeyValLLong(fpout, "CIFIRST");

  if(ahgen::strtoupper(param.m_evtthre) == "DEFAULT") {
    l_evtthres = ahfits::getKeyValStr(fpout,"EVENTTHR");
    sxipilib::parseStringInt(l_evtthres, 8, evtThresInt);
  } else {
    sxipilib::parseStringInt(param.m_evtthre, 1, evtThresInt);
    for(int ii = 1; ii < 8; ++ii ) evtThresInt[ii] = evtThresInt[0];
  }

  cistatus_ss << keydat.m_ciStatus;
  
  for (int ii=0; ii<7; ++ii) {
    dmode += datamode.at(ii);
  }

  expr = "DATAMODE.eq." + dmode + 
    ".and.CISTATUS.eq."+cistatus_ss.str();

  
  for(int ii = 0; ii < 8; ++ii) {
    keydat.m_huclegth[ii] = 0;
    keydat.m_vucheght[ii] = 0;
    keydat.m_imgheght[ii] = 0;
    keydat.m_actvnode[ii] = 0;
  }

  sxipilib::parseStringInt(huclegthstr, 8, keydat.m_huclegth);
  sxipilib::parseStringInt(vucheghtstr, 8, keydat.m_vucheght);
  sxipilib::parseStringInt(imgheghtstr, 8, keydat.m_imgheght);
  sxipilib::parseStringInt(actvnodestr, 8, keydat.m_actvnode);

  // Capture TNULL value for PHAS
  hastnull = ahfits::columnNull(fpout, "CCD_ID", keydat.m_ccdbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("CCD_ID column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "SEGMENT", keydat.m_segbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("SEGMENT column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "READNODE", keydat.m_readnodebad);
  if(!hastnull) {
    AH_THROW_RUNTIME("READNODE column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "RAWX", keydat.m_rawxbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("RAWX column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "ACTY", keydat.m_actybad);
  if(!hastnull) {
    AH_THROW_RUNTIME("ACTY column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "ADCAVE", keydat.m_adcavebad);
  if(!hastnull) {
    AH_THROW_RUNTIME("ADCAVE column lacks TNULL");
  }
  if(param.m_startcol == "PHA_SPTH") {
    hastnull = ahfits::columnNull(fpout, "PHA_SPTH", keydat.m_startcolbad);
    if(!hastnull) {
      std::stringstream msg;
      msg << param.m_startcol << " column lacks TNULL.";
      AH_THROW_RUNTIME(msg.str());
    }
  }

  if (param.m_startcol == "PHAS") {
    hastnull = ahfits::columnNull(fpout, "PHAS", keydat.m_startcolbad);
    if(!hastnull) {
      std::stringstream msg;
      msg << param.m_startcol << " column lacks TNULL.";
      AH_THROW_RUNTIME(msg.str());
    }
  }

  if(param.m_startcol == "GRADE_SPTH") {
    // Need to check both PHA and GRADE debug columns
    hastnull = ahfits::columnNull(fpout, "PHA_SPTH", keydat.m_startcolbad);
    hastnull = ahfits::columnNull(fpout, "GRADE_SPTH", keydat.m_startcolbad);
    if(!hastnull) {
      std::stringstream msg;
      msg << param.m_startcol << " column lacks TNULL.";
      AH_THROW_RUNTIME(msg.str());
    }
  }

  // Get min/max values of CCD_ID, ACTX, ACTY and SEGMENT
  if(!ahfits::columnRange(fpout,"CCD_ID",keydat.m_ccdmin,keydat.m_ccdmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column CCD_ID");
  }
  if(!ahfits::columnRange(fpout,"SEGMENT",keydat.m_segmin,keydat.m_segmax)){
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column SEGMENT");
  }
  if(!ahfits::columnRange(fpout,"READNODE",keydat.m_readnodemin,keydat.m_readnodemax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column READNODE");
  }
  if(!ahfits::columnRange(fpout,"RAWX",keydat.m_rawxmin,keydat.m_rawxmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column RAWX");
  }
  if(!ahfits::columnRange(fpout,"ACTY",keydat.m_actymin,keydat.m_actymax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column ACTY");
  }

  AH_DEBUG << "CCD_ID MIN: " << keydat.m_ccdmin << " MAX: " << keydat.m_ccdmax << " NULL: " << keydat.m_ccdbad  << std::endl;
  AH_DEBUG << "SEGMENT MIN: " << keydat.m_segmin << " MAX: " << keydat.m_segmax << " NULL: " << keydat.m_segbad  << std::endl;
  AH_DEBUG << "READNODE MIN: " << keydat.m_readnodemin << " MAX: " << keydat.m_readnodemax << " NULL: " << keydat.m_readnodebad  << std::endl;
  AH_DEBUG << "RAWX MIN: " << keydat.m_rawxmin << " MAX: " << keydat.m_rawxmax << " NULL: " << keydat.m_rawxbad  << std::endl;
  AH_DEBUG << "ACTY MIN: " << keydat.m_actymin << " MAX: " << keydat.m_actymax << " NULL: " << keydat.m_actybad  << std::endl;
  AH_DEBUG << "ADCAVE NULL: " << keydat.m_adcavebad  << std::endl;
  AH_DEBUG << param.m_startcol << " NULL: " << keydat.m_startcolbad << std::endl;

  // Move to EVENTS extension
  ahfits::move(fpout, extname);

  // Verify valid starting column
  startcol = param.m_startcol;
  if(startcol != "PHAS" &&
     startcol != "PHAS_EVENODD" &&
     startcol != "PHAS_TRAILCORR" &&
     startcol != "PHAS_CTICORR" &&
     startcol != "PHA") {
    AH_THROW_RUNTIME("Invalid starting column. Must be PHAS, PHAS_EVENODD, PHAS_TRAILCORR, PHAS_CTICORR, or PHA");
  }

  // Check if starting column exists
  if(0 == ahfits::haveColumn(fpout,startcol)) AH_THROW_RUNTIME("Start column does not exist");

  // Add columns in output event file if keeping debug columns
  // Only add debug columns if starting column is PHAS
  // if debug col is off delete existing columns
  if(param.m_debugcol) {
    if (!ahfits::haveColumn(fpout, "PHAS_EVENODD")) {
      ahfits::insertColAfter(fpout,"PHAS_EVENODD","9D","");
      ahfits::setColumnDescription(fpout,"PHAS_EVENODD","PHAS after sxipi even/odd correction");
    }
    if (!ahfits::haveColumn(fpout, "PHAS_TRAILCORR")) {
      ahfits::insertColAfter(fpout,"PHAS_TRAILCORR","9D","");
      ahfits::setColumnDescription(fpout,"PHAS_TRAILCORR","PHAS after sxipi charge trail correction");
    }
    if (!ahfits::haveColumn(fpout, "PHAS_CTICORR")) {
      ahfits::insertColAfter(fpout,"PHAS_CTICORR","9D","");
      ahfits::setColumnDescription(fpout,"PHAS_CTICORR","PHAS after sxipi CTI correction");
    }
    if (!ahfits::haveColumn(fpout, "PHA_SPTH")) {
      ahfits::insertColAfter(fpout,"PHA_SPTH","1I","");
      ahfits::setColumnDescription(fpout,"PHA_SPTH","PHA after sxipi first grade iteration");
    }
    if (!ahfits::haveColumn(fpout, "GRADE_SPTH")) {
      ahfits::insertColAfter(fpout,"GRADE_SPTH","1I","");
      ahfits::setColumnDescription(fpout,"GRADE_SPTH","GRADE after sxipi first grade iteration");
    }
  } else if (!param.m_debugcol) { //if debug column is set to false, delete existing debug columns     
    int errorStatus = 0;
    char extnameCfits[7] = "EVENTS";
    fitsfile * fpoutCfits = NULL;
    fits_open_file(&fpoutCfits,param.m_outfile.c_str(),READWRITE,&errorStatus);
    fits_movnam_hdu(fpoutCfits,ANY_HDU,extnameCfits,0,&errorStatus);
    if (ahfits::haveColumn(fpout, "PHAS_EVENODD")) {
      int colnum = ahfits::name2Num(fpout, "PHAS_EVENODD");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    if (ahfits::haveColumn(fpout, "PHAS_TRAILCORR")) {
      int colnum = ahfits::name2Num(fpout, "PHAS_TRAILCORR");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    if (ahfits::haveColumn(fpout, "PHAS_CTICORR")) {
      int colnum = ahfits::name2Num(fpout, "PHAS_CTICORR");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    if (ahfits::haveColumn(fpout, "PHA_SPTH")) {
      int colnum = ahfits::name2Num(fpout, "PHA_SPTH");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    if (ahfits::haveColumn(fpout, "GRADE_SPTH")) {
      int colnum = ahfits::name2Num(fpout, "GRADE_SPTH");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    fits_close_file(fpoutCfits, &errorStatus);
    if (errorStatus != 0) {
      AH_ERR << "cfitio error status: " << errorStatus << std::endl;
      AH_THROW_RUNTIME("failed to close file");
    }
  }

  //Add GRADE_COPY and PHA_COPY columns to output event file if copygrade=yes
  if (param.m_copygrade) {
    if (ahfits::haveColumn(fpout, "PHA_COPY")) {
      AH_INFO(ahlog::HIGH) << "WARNING: Copygrade is yes, but PHA_COPY column already exists. It will be overwritten." << std::endl;
    } else {
      ahfits::insertColAfter(fpout,"PHA_COPY","1I","");
      ahfits::setColumnDescription(fpout,"PHA_COPY","PHA before sxipi calculation");
    }
    if (ahfits::haveColumn(fpout, "GRADE_COPY")) {
      AH_INFO(ahlog::HIGH) << "WARNING: Copygrade is yes, but GRADE_COPY column already exists. It will be overwritten." << std::endl;
    } else {
      ahfits::insertColAfter(fpout,"GRADE_COPY","1I","");
      ahfits::setColumnDescription(fpout,"GRADE_COPY","GRADE before sxipi calculation");
    }
  //Delete GRADE_COPY and PHA_COPY columns fromt output event file if copygrade=no and they exist
  } else if (!param.m_copygrade) {
    int errorStatus = 0;
    char extnameCfits[7] = "EVENTS";
    fitsfile * fpoutCfits = NULL;
    fits_open_file(&fpoutCfits,param.m_outfile.c_str(),READWRITE,&errorStatus);
    fits_movnam_hdu(fpoutCfits,ANY_HDU,extnameCfits,0,&errorStatus);
    if (ahfits::haveColumn(fpout, "PHA_COPY")) {
      int colnum = ahfits::name2Num(fpout, "PHA_COPY");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    if (ahfits::haveColumn(fpout, "GRADE_COPY")) {
      int colnum = ahfits::name2Num(fpout, "GRADE_COPY");
      fits_delete_col(fpoutCfits, colnum, &errorStatus);
      if (errorStatus != 0) {
        AH_ERR << "cfitio error status: " << errorStatus << std::endl;
        AH_THROW_RUNTIME("failed to delete column");
      }
    }
    fits_close_file(fpoutCfits, &errorStatus);
    if (errorStatus != 0) {
      AH_ERR << "cfitio error status: " << errorStatus << std::endl;
      AH_THROW_RUNTIME("failed to close file");
    }
  }

  if(ahgen::strtoupper(param.m_vtevnodd) == "NONE" && param.m_evnoddcor) {
    AH_THROW_RUNTIME("Parameter evnoddcor set to true but no even odd file given");
  }
  if(param.m_chtrailfile == "NONE" && param.m_chtrailcor) {
    AH_THROW_RUNTIME("Parameter chtrailcor set to true but no charge trail file given");
   }
  if(param.m_ctifile == "NONE" && param.m_cticor) {
    AH_THROW_RUNTIME("Parameter cticor set to true but no CTI file given");
   }
  if(param.m_spthfile == "NONE" && param.m_spthcaldb) {
    AH_THROW_RUNTIME("Parameter spthcaldb set to true but no split threshold file given");
  }
  if(param.m_gainfile == "NONE" && param.m_gaincor) {
    AH_THROW_RUNTIME("Parameter gaincor set to true but no gain file given");
  }

  if(param.m_hkfile != "NONE" && param.m_evnoddcor) {
    // Open HK1 File
    sxipilib::loadHK(param.m_hkfile, hkdat, param.m_hkext, param.m_hkcol, tstart, tstop, param.m_hkvid_list);
  }

  // Load even odd data
  if(param.m_evnoddcor) {
    sxipilib::loadEvenOdd(param.m_vtevnodd, eodat, tstart, tstop, telescop, inst);            
  }

  // Load charge trail data
  if(param.m_chtrailcor) {
    sxipilib::loadChargeTrail(param.m_chtrailfile, chtrdat, tstart, tstop, keydat, telescop, inst, expr);
  }

  // Load CTI data
  if(param.m_cticor) {
    sxipilib::loadCTI(param.m_ctifile, ctidat, AnomRegData, tstart, tstop, keydat, telescop, inst, expr);
  }

  // Load split threshold data
  if(param.m_spthcaldb) {
    sxipilib::loadSpTh(param.m_spthfile, spthdat, tstart, telescop, inst);
  }

  // Load gain data
  if(param.m_gaincor) {
    sxipilib::loadGain(param.m_gainfile, gaindat, tstart, keydat, telescop, inst, expr);
  }

  // Load grade data
  if(startcol != "PHA") {
    if(param.m_patternfile == "NONE") AH_THROW_RUNTIME("No grade file input.");
    sxipilib::loadGrade(param.m_patternfile, gradedat, telescop, inst);
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

} // end initialize

// ****************************************************************************

void doWork(const sxipilib::Params& param, ahfits::FilePtr & fpout, 
            sxipilib::HKData & hkdat, const sxipilib::EvenOddData& eodat,
            const sxipilib::ChargeTrailData& chtrdat,  const sxipilib::CTIData& ctidat, sxipilib::AnomRegData & AnomRegData,
            const sxipilib::SpThData& spthdat, const sxipilib::GradeData& gradedat,
            const sxipilib::GainData& gaindat, const sxipilib::Keywords& keydat, const int * evtThresInt) {

  int errorStatus = 0;      // Error status. 0: OK; 1: Continue, next row; 2: Stop program
  int iccdseg = 0;
  double r = 0;             // Random number for PHA          
     
  double timeEvt = 0.;      // Event time from event file
  char timenull = 0;        // null flag for time    
  int ccdidEvt = 0;         // CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4) 
  int segmentEvt = 0;       // Segment ID (0: AB, 1: CD)
  int readnodeEvt = 0;      // Readout node (0: A or C, 1: B or D)
  int rawxEvt = 0;          // RAWX coordinate for specified event
  int actxEvt = 0;          // ACTX coordinate for specified event
  int actyEvt = 0;              // ACTY coordinate for specified event
  int pOuterMostEvt = 0;        // 0: pixel PH < split threshold, 1: pixel PH >= split threshold
  int adcAveEvt = 0;            // ADC setting (0, 1, 2, or -1)
  double phaEvt = 0.;           // column pha value
  double pha=0.;                //PHA output from grade pha
  double pha_tmp=0.;            //temporary pha value
  double piEvt = 0.;            // Final event PI calculation
  int gradeEvt = 0;             // Final grade of event
  int grade_copy = 0;
  double pha_copy = 0.;

  bool setnull = false;     // Boolean to set null row
  bool continue_flag = false;

  char statusEvt[sxipilib::LENSTATUS];                 // Check if event is on a CI row
  ahfits::IndexType numStatus = sxipilib::LENSTATUS;   // Length of status bits
  for (int iii=0; iii < sxipilib::LENSTATUS; iii++) statusEvt[iii]=0;

  double phasEvt[sxipilib::LENPHAS5x5];    // PHAS for specified event
  double phasEvenOdd[sxipilib::LENPHAS];   // Even odd corrected PHAS
  double phasChTrail[sxipilib::LENPHAS];   // Charge trail corrected PHAS
  double phasCTI[sxipilib::LENPHAS];       // CTI corrected PHAS
  double phasTmp[sxipilib::LENPHAS];       // CTI corrected PHAS
  int phasMask[sxipilib::LENPHAS];         // Mask events with bad pixel
  for (int iii=0; iii < sxipilib::LENPHAS5x5; iii++) phasEvt[iii]=0.;
  for (int iii=0; iii < sxipilib::LENPHAS; iii++) {
    phasEvenOdd[iii]=0.;
    phasChTrail[iii]=0.;
    phasCTI[iii]=0.;
    phasTmp[iii]=0.;
    phasMask[iii]=0;
  }

  double phaSpTh;
  int gradeSpTh;

  char procStatus[sxipilib::LENSTATUS];           // column: PROC_STATUS (1st bit=1 => BAD)
  ahfits::IndexType numProcStatus;                // number of PROC_STATUS bits found
  for (int iii=0; iii < sxipilib::LENSTATUS; iii++) procStatus[iii]=0;

  ahfits::Router router(fpout);

  //Events counters for output logging
  long long num_events = 0;          //total number of events read
  long long num_good = 0;            //number of events with PI assigned
  long long num_null_rows = 0;       //total number of output events filled with NULLs
  long long num_bad_procstatus = 0;  //num. events skipped because of bad PROC_STATUS
  long long num_bad_time = 0;        //num. events skipped because TIME=NULL
  long long num_bad_phas0 = 0;       //num. events skipped with PHAS[0] < event threshold
  long long num_bad_ccdid = 0;       //num. events skipped with CCD_ID out of range
  long long num_bad_segment = 0;     //num. events skipped with SEGMENT out of range
  long long num_bad_readnode = 0;    //num. events skipped with READNODE out of range
  long long num_bad_rawx = 0;        //num. events skipped with RAWX out of range
  long long num_bad_acty = 0;        //num. events skipped with ACTY out of range
  long long num_bad_adcave = 0;      //num. events skipped with ADCAVE = NULL
  long long num_bad_startcol = 0;    //num. events skipped with input startcol = NULL
  long long num_bad_phas = 0;        //num. events skipped with PHAS[0-8] < par.negativep
  long long num_null_evenodd = 0;    //num. events which had NULL set by even/odd correction
  long long num_null_chtrail = 0;    //num. events which had NULL set by charge trail correction
  long long num_null_cti = 0;        //num. events which had NULL set by CTI correction
  long long num_null_grade = 0;      //num. events which had NULL set by grade/PHA calculation
  long long num_null_pi = 0;         //num. events which had NULL set by gain (PHA->PI) correction

  //Log which corrections will be performed
  if (param.m_startcol == "PHAS") {
    AH_INFO(ahlog::HIGH) << "Starting with PHAS." << std::endl; 
    if (param.m_randomize) {   
      AH_INFO(ahlog::HIGH) << "Randomization will be applied to input PHAS." << std::endl; 
      ahgen::seedRandom(param.m_seed);
    } else {
      AH_INFO(ahlog::HIGH) << "No randomization will be applied to input PHAS." << std::endl;
    }
  } else if (param.m_startcol == "PHAS_EVENODD") {
    // phas_evenodd is already float, no randomization
    AH_INFO(ahlog::HIGH) << "Starting with PHAS_EVENODD." << std::endl; 
    if (param.m_evnoddcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_EVENODD but even-odd correction is turned on.  Correction may have already been applied." << std::endl;
    }
  } else if (param.m_startcol == "PHAS_TRAILCORR") {
    // phas_trailcorr is already float, no randomization
    AH_INFO(ahlog::HIGH) << "Starting with PHAS_TRAILCORR." << std::endl;
    if (param.m_evnoddcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_TRAILCORR but even-odd correction is turned on.  Correction may have already been applied." << std::endl;
    }
    if (param.m_chtrailcor) { 
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_TRAILCORR but charge trail correction is turned on.  Correction may have already been applied." << std::endl;
    }
  } else if (param.m_startcol == "PHAS_CTICORR") {
    // phas_cticorr is already float, no randomization
    AH_INFO(ahlog::HIGH) << "Starting with PHAS_CTICORR." << std::endl; 
    if (param.m_evnoddcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_CTICORR but even-odd correction is turned on.  Correction may have already been applied." << std::endl;
    }
    if (param.m_chtrailcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_CTICORR but charge trail correction is turned on.  Correction may have already been applied." << std::endl;
    }
    if (param.m_cticor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHAS_CTICORR but CTI correction is turned on.  Correction may have already been applied." << std::endl;
    }
  } else if (param.m_startcol == "PHA") {
    AH_INFO(ahlog::HIGH) << "Starting with PHA." << std::endl; 
    // start with PHAS column if startcol=PHA
    // phas is an integer, so here need randomization of decimal part
    if(param.m_randomize) {   
      AH_INFO(ahlog::HIGH) << "Randomization will be applied to input PHAS." << std::endl; 
      ahgen::seedRandom(param.m_seed);
    } else {
      AH_INFO(ahlog::HIGH) << "Randomization will not be applied to input PHAS." << std::endl; 
    }
    if (param.m_evnoddcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHA but even-odd correction is turned on.  Correction will be applied to PHAS but not used to calculate PHA." << std::endl;
    }
    if (param.m_chtrailcor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHA but charge trail correction is turned on.  Correction will be applied to PHAS but not used to calculate PHA." << std::endl;
    }
    if (param.m_cticor) {
      AH_INFO(ahlog::HIGH) << "WARNING:  Starting column is PHA but CTI correction is turned on.  Correction will be applied to PHAS but not used to calculate PHA." << std::endl;
    }
  } 

  if (!param.m_ctigrade && param.m_cticor) { 
    AH_INFO(ahlog::HIGH) << "Grade-dependent CTI correction will not be applied ." << std::endl;
  }

  if (param.m_phcut != "CALDB" && param.m_cticor) {
    AH_INFO(ahlog::HIGH) << "PH_CUT = " << param.m_phcut << "will be used in CTI correction, not CALDB value." << std::endl;
  }


  router.connectScalar(ahfits::e_READONLY, "TIME", timeEvt, &timenull);
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", ccdidEvt);
  router.connectScalar(ahfits::e_READONLY, "SEGMENT", segmentEvt);
  router.connectScalar(ahfits::e_READONLY, "READNODE", readnodeEvt);
  router.connectScalar(ahfits::e_READONLY, "RAWX", rawxEvt);
  router.connectScalar(ahfits::e_READONLY, "ACTX", actxEvt);
  router.connectScalar(ahfits::e_READONLY, "ACTY", actyEvt);
  router.connectScalar(ahfits::e_READONLY, "P_OUTER_MOST", pOuterMostEvt);
  router.connectScalar(ahfits::e_READONLY, "ADCAVE", adcAveEvt);
  router.connectScalar(ahfits::e_READWRITE, "PHA", phaEvt);
  router.connectScalar(ahfits::e_READWRITE, "PI", piEvt);
  router.connectScalar(ahfits::e_READWRITE, "GRADE", gradeEvt);
  if (param.m_copygrade) {
    router.connectScalar(ahfits::e_READWRITE, "GRADE_COPY", grade_copy);
    router.connectScalar(ahfits::e_READWRITE, "PHA_COPY", pha_copy);
  }
  router.connectFixedLengthArray(ahfits::e_READONLY, "PHAS", phasEvt);
  router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS_MASK", phasMask);
  router.connectBit(ahfits::e_READWRITE, "STATUS", statusEvt, numStatus);
  router.connectBit(ahfits::e_READONLY,"PROC_STATUS", procStatus, numProcStatus);
  if(param.m_debugcol) {
    router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS_EVENODD", phasEvenOdd);
    router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS_TRAILCORR", phasChTrail);
    router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS_CTICORR", phasCTI);
    router.connectScalar(ahfits::e_READWRITE, "PHA_SPTH", phaSpTh);
    router.connectScalar(ahfits::e_READWRITE, "GRADE_SPTH", gradeSpTh);
  }

  // Loop through events
  for(ahfits::firstRow(fpout); ahfits::readOK(fpout); ahfits::nextRow(fpout)) {
    continue_flag = false;
    ahfits::readRow(fpout);
    num_events++;

    AH_DEBUG << "######## Processing event " << num_events << std::endl;

    // -- copy GRADE, PHA if copygrade=yes
    if (param.m_copygrade) {
      grade_copy = gradeEvt;
      pha_copy = phaEvt;
    }

    // figure out which PHAS* column to read as phas_tmp, on which
    // all the corrections will be performed 
    if (param.m_startcol == "PHAS") {
      if (param.m_randomize) { 
        for (int ii=0; ii<sxipilib::LENPHAS; ++ii) {
          r = ahgen::getRandom();
          phasTmp[ii] = phasEvt[ii]+r;
        } 
      } else {
        for (int ii=0; ii<sxipilib::LENPHAS; ++ii) { 
          phasTmp[ii] = phasEvt[ii];                
        }
      }
    } else if (param.m_startcol == "PHAS_EVENODD") { 
      for (int ii=0; ii<sxipilib::LENPHAS; ++ii) {
        phasTmp[ii] =  phasEvenOdd[ii];
      }
    }else if (param.m_startcol == "PHAS_TRAILCORR") { 
      for (int ii=0; ii<sxipilib::LENPHAS; ++ii) {
        phasTmp[ii] =  phasChTrail[ii];
      }
    }else if (param.m_startcol == "PHAS_CTICORR") { 
      for (int ii=0; ii<sxipilib::LENPHAS; ++ii) {
        phasTmp[ii] =  phasCTI[ii];
      }
    }else if (param.m_startcol == "PHA") { 
      if (param.m_randomize) { 
        for (int ii=0; ii<sxipilib::LENPHAS; ++ii) {
          r = ahgen::getRandom();
          phasTmp[ii] = phasEvt[ii]+r;
        } 
      } else {
        for (int ii=0; ii<sxipilib::LENPHAS; ++ii) { 
          phasTmp[ii] = phasEvt[ii];                
        }
      }
    }

    if (!procstatus::processRow(procStatus)) {
      AH_INFO(ahlog::LOW) << "PROC_STATUS false at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
      num_bad_procstatus++;
      num_null_rows++;
      if(param.m_debugcol) {
        sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                               sxipilib::LENPHAS, phaSpTh, gradeSpTh);
      }
      sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
      ahfits::writeRow(fpout);
      continue;
    } else if (timenull==1) {
      AH_INFO(ahlog::LOW) << "TIME is NULL at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
      num_bad_time++;
      num_null_rows++;
      if(param.m_debugcol) {
        sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                               sxipilib::LENPHAS, phaSpTh, gradeSpTh);
      }
      sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
      statusEvt[26] = 1;
      ahfits::writeRow(fpout);
      continue;
    } else {

      // Check for negative PHAS (All negative, or more negative than positive)
      // Null value for phas: -32768
      if(phasEvt[0] < evtThresInt[2*ccdidEvt+segmentEvt]) {
        AH_INFO(ahlog::LOW) << "PHAS is below event threshold at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
        AH_INFO(ahlog::LOW) << "PHAS[0] " << phasEvt[0] << " < " << evtThresInt[2*ccdidEvt+segmentEvt] << std::endl;
        num_bad_phas0++;
        num_null_rows++;
        // Fill null values to all of the columns written by this task and go to the next event
        if(param.m_debugcol) {
          sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                                 sxipilib::LENPHAS, phaSpTh, gradeSpTh);
        }
        
        sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
        statusEvt[25] = 1;
        ahfits::writeRow(fpout);
        continue;
      }

      // Verify within boundary for CCD_ID, SEGMENT, READNODE, RAWX, ACTY and ADCAVE
      // Set null can only be set to true
      if(ccdidEvt < keydat.m_ccdmin || ccdidEvt > keydat.m_ccdmax || ccdidEvt == keydat.m_ccdbad) {
        AH_INFO(ahlog::LOW) << "CCD_ID out of range at row  " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
        num_bad_ccdid++;
        setnull = true;
      }
      if(segmentEvt < keydat.m_segmin || segmentEvt > keydat.m_segmax || segmentEvt == keydat.m_segbad) {
        AH_INFO(ahlog::LOW) << "SEGMENT out of range at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
        num_bad_segment++;
        setnull = true;
      }
      if(readnodeEvt < keydat.m_readnodemin || readnodeEvt > keydat.m_readnodemax || readnodeEvt == keydat.m_readnodebad) {
       AH_INFO(ahlog::LOW) << "READNODE out of range at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
       num_bad_readnode++;
       setnull = true;
      }
      if(rawxEvt < keydat.m_rawxmin || rawxEvt > keydat.m_rawxmax || rawxEvt == keydat.m_rawxbad) {
        AH_INFO(ahlog::LOW) << "RAWX out of range at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
        num_bad_rawx++;
        setnull = true;
      }
      if(actyEvt < keydat.m_actymin || actyEvt > keydat.m_actymax || actyEvt == keydat.m_actybad) {
        AH_INFO(ahlog::LOW) << "ACTY out of range at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
        num_bad_acty++;
        setnull = true;
      }
      if(adcAveEvt == keydat.m_adcavebad) {
        AH_INFO(ahlog::LOW) << "ADCAVE is null at row " << ahfits::currentRow(fpout) << ". Skipping event." << std::endl; 
        num_bad_adcave++;
        setnull = true;
      }

      // Verify no null values in startcol
      if(param.m_startcol == "PHAS") {
        for(int jj = 0; jj < sxipilib::LENPHAS; ++jj) { 
          if(phasEvt[jj] == keydat.m_startcolbad) {
            AH_INFO(ahlog::LOW) << "PHAS[" << jj << "] bad at row" << ahfits::currentRow(fpout) << ". Skipping event." << std::endl; 
            setnull = true;
            num_bad_startcol++;
          } 
        }
      } else if(param.m_startcol == "PHAS_EVENODD") {
        for(int jj = 0; jj < sxipilib::LENPHAS; ++jj) { 
          if(phasEvenOdd[jj] == keydat.m_startcolbad) {
            AH_INFO(ahlog::LOW) << "PHAS_EVENODD[" << jj << "] bad at row" << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;
            setnull = true;
            num_bad_startcol++;
          } 
        }
      } else if(param.m_startcol == "PHAS_TRAILCORR") {
        for(int jj = 0; jj < sxipilib::LENPHAS; ++jj) { 
          if(phasChTrail[jj] == keydat.m_startcolbad) {
            AH_INFO(ahlog::LOW) << "PHAS_TRAILCORR[" << jj << "] bad at row" << ahfits::currentRow(fpout) << ". Skipping event." << std::endl; 
            setnull = true;
            num_bad_startcol++;
          } 
        }
      } else if(param.m_startcol == "PHAS_CTICORR") {
        for(int jj = 0; jj < sxipilib::LENPHAS; ++jj) { 
          if(phasCTI[jj] == keydat.m_startcolbad) {
            AH_INFO(ahlog::LOW) << "PHAS_CTICORR[" << jj << "] bad at row" << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;  
            setnull = true; 
            num_bad_startcol++;
          }
        }
      } else if(param.m_startcol == "PHA") {
        if(phaEvt == keydat.m_startcolbad) {
          AH_INFO(ahlog::LOW) << "PHA bad at row" << ahfits::currentRow(fpout) << ". Skipping event." << std::endl;  
          setnull = true;
          num_bad_startcol++;
        }
      }

      if(setnull) {
        // Fill null values to all of the columns written by this task and go to the next event
        if(param.m_debugcol) {
          sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                                 sxipilib::LENPHAS, phaSpTh, gradeSpTh);
        }
        sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
        statusEvt[29] = 1;
        ahfits::writeRow(fpout);
        setnull = false;
        num_null_rows++;
        continue;
      }

      for(int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
        if(phasEvt[ii] < param.m_negthre) {
          AH_INFO(ahlog::LOW) << "PHAS below minimum pha threshold at row " << ahfits::currentRow(fpout) << " Skipping event." << std::endl;
          // Fill null values to all of the columns written by this task and go to the next event
          if(param.m_debugcol) {
            sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                                   sxipilib::LENPHAS, phaSpTh, gradeSpTh);
          }
          sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
          ahfits::writeRow(fpout);
          continue_flag = true;
        }
      }
      if (continue_flag) {
        num_bad_phas++;
        num_null_rows++;
        continue;
      }
   
      if(param.m_evnoddcor) {
        // Calculate index for huclegth, vucheght and imgheght
        iccdseg = 2*ccdidEvt+segmentEvt;
 
        AH_DEBUG << "Performing even odd correction phasTmp, phasEvenOdd: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasEvenOdd[ii] << std::endl;
        }  

        // Step 1: Even/Odd correction
        // Subroutine to determine the even odd for each 3x3 pixel and calculate
        // the correction
        // Event column info: PHAS, TIME, RAWX, CCD_ID, SEGMENT, READNODE
        sxipilib::correctEvenOdd(phasTmp, timeEvt, rawxEvt, actxEvt, ccdidEvt, segmentEvt,
                                 readnodeEvt, adcAveEvt, keydat.m_actvnode, param.m_evnoddcor, param.m_deltatime,
                                 eodat, phasEvenOdd, errorStatus, hkdat, param, statusEvt);

        AH_DEBUG << "After even odd correction phasTmp, phasEvenOdd: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasEvenOdd[ii] << std::endl;
        }
        //update phasTmp with value computed by Even Odd
        for(int ii = 0; ii < sxipilib::LENPHAS; ++ii) { phasTmp[ii] = phasEvenOdd[ii]; }

        if(errorStatus == 2) {
          std::stringstream msg;
          msg << "Error during even/odd correction at row " << ahfits::currentRow(fpout) << 
                 ". HK file is out of time order. Error status " << errorStatus;
          AH_THROW_RUNTIME(msg.str());
        }
        if(errorStatus == 1) {
          // Fill null values to all of the columns written by this task and go to the next event
          if(param.m_debugcol) {
            sxipilib::setDebugNull(phasEvenOdd, phasChTrail, phasCTI, 
                                   sxipilib::LENPHAS, phaSpTh, gradeSpTh);
          }
          sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
          statusEvt[29] = 1;
          AH_INFO(ahlog::LOW) << "Even/odd correction resulted in null row " << ahfits::currentRow(fpout) << std::endl;
          num_null_evenodd++;
          num_null_rows++;
          ahfits::writeRow(fpout);
          continue;
        }
      } else if (param.m_debugcol) {
        for(int ii=0; ii < sxipilib::LENPHAS; ++ii) { 
          phasEvenOdd[ii] = phasTmp[ii];
        }
      } //end even odd correction

      if (param.m_chtrailcor) {
    
        AH_DEBUG << "Performing charge trail correction phasTmp, phasEvenOdd: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasChTrail[ii] << std::endl;
        }  

        // Step 2: Charge trail correction
        // Event column info: (PHAS_EVENODD), TIME, RAWX, ACTY, CCD_ID, SEGMENT, READNODE
        sxipilib::correctChargeTrail(phasTmp, rawxEvt, actxEvt, actyEvt, ccdidEvt, segmentEvt, 
                                     readnodeEvt, statusEvt, keydat.m_huclegth[iccdseg],
                                     keydat.m_vucheght[iccdseg], keydat.m_imgheght[iccdseg],
                                     keydat.m_ciStatus, keydat.m_ciPeriod, keydat.m_ciOffset,
                                     keydat.m_ciFirst, param.m_chtrailcor, chtrdat,
                                     phasChTrail, errorStatus);

        AH_DEBUG << "After charge trail correction phasTmp, phasChTrail: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasChTrail[ii] << std::endl;
        }  

        //update phasTmp with value computed by Charge trail
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) { phasTmp[ii] = phasChTrail[ii]; } 

        if(errorStatus == 2) {
          std::stringstream msg;
          msg << "Bad event during charge trail correction at row " << ahfits::currentRow(fpout) << ". Error status " << errorStatus;
          AH_THROW_RUNTIME(msg.str());
        }
        if(errorStatus == 1) {
          // Fill null values to all of the columns written by this task and go to the next event
          if(param.m_debugcol) {
            // cannot use sxipilib::setDebugNull since phasEvenOdd should be kept
            for(int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
              phasChTrail[ii] = NAN;
              phasCTI[ii] = NAN;
            }
            phaSpTh = sxipilib::PHA_TNULL;
            gradeSpTh = sxipilib::GRADE_TNULL;
          }
          sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
          statusEvt[29] = 1;
          AH_INFO(ahlog::LOW) << "Charge trail correction resulted in null row " << ahfits::currentRow(fpout) << std::endl;
          num_null_chtrail++;
          num_null_rows++;
          ahfits::writeRow(fpout);
          continue;
        }
      } else if (param.m_debugcol) {
        for(int ii=0; ii < sxipilib::LENPHAS; ++ii) { 
          phasChTrail[ii] = phasTmp[ii];
        }
      }

      if (param.m_cticor) {
       
        AH_DEBUG << "Performing CTI correction phasTmp, phasEvenOdd: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasCTI[ii] << std::endl;
        }  

        // Step 3: CTI correction
        // Event column info: (PHAS_TRAILCORR), TIME, RAWX, ACTY, CCD_ID, SEGMENT, READNODE
        sxipilib::correctCTI(phasTmp, gradeEvt, phaEvt, rawxEvt, actxEvt, actyEvt, param, 
                             ccdidEvt, segmentEvt, readnodeEvt, keydat.m_actvnode,
                             keydat.m_huclegth[iccdseg], keydat.m_vucheght[iccdseg], 
                             keydat.m_imgheght[iccdseg], keydat.m_winSt, keydat.m_winSize,
                             keydat.m_ciStatus, keydat.m_ciPeriod, keydat.m_ciOffset,
                             keydat.m_ciFirst, param.m_cticor, ctidat, AnomRegData, phasCTI, errorStatus);
 

        AH_DEBUG << "After CTI correction phasTmp, phasCTI: " << std::endl;
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
          AH_DEBUG << phasTmp[ii] << ", " << phasCTI[ii] << std::endl;
        }  

        //update phasTmp with value computed by CTI correction
        for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) { phasTmp[ii] = phasCTI[ii]; }

        if(errorStatus == 2) {
          std::stringstream msg;
          msg << "Bad event during CTI correction at row " << ahfits::currentRow(fpout) << ". Error status " << errorStatus<< std::endl;
          AH_THROW_RUNTIME(msg.str());
        }
        if(errorStatus == 1) {
          // Fill null values to all of the columns written by this task and go to the next event
          if(param.m_debugcol) {
            // cannot use sxipilib::setDebugNull since phasEvenOdd, phasChTrail should be kept
            for(int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
              phasCTI[ii] = NAN;
            }
            phaSpTh = sxipilib::PHA_TNULL;
            gradeSpTh = sxipilib::GRADE_TNULL;
          }
          sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
          statusEvt[29] = 1;
          AH_INFO(ahlog::LOW) << "CTI correction resulted in null row " << ahfits::currentRow(fpout) << std::endl;
          num_null_cti++;
          num_null_rows++;
          ahfits::writeRow(fpout);
          continue;
        }
      } else if (param.m_debugcol) {
        for(int ii=0; ii < sxipilib::LENPHAS; ++ii) { 
          phasCTI[ii] = phasTmp[ii];
        }        
      } //end if CTI
 
  
      AH_DEBUG << "Performing GRADE and PHA assigment on phasTmp" << std::endl;
      for (int ii = 0; ii < sxipilib::LENPHAS; ++ii) {
        AH_DEBUG << phasTmp[ii] << std::endl;
      }  

      // Step 4: Grade assignment and PHA calculation
      // Event column info: (PHAS_CTICORR), P_OUTER_MOST, CCD_ID, SEGMENT, READNODE
      sxipilib::gradePHA(phasTmp, phasMask, param.m_badpixopt, pOuterMostEvt, 
                         ccdidEvt, segmentEvt, readnodeEvt, param.m_spthiter,
                         param.m_spthcaldb, param.m_spthoffset, param.m_spthslope,
                         gradedat, spthdat, pha, gradeEvt, phaSpTh, gradeSpTh,
                         errorStatus);
      if(errorStatus == 2) {
        std::stringstream msg;
        msg << "Bad event during grade at row " << ahfits::currentRow(fpout) << ". Error status " << errorStatus << std::endl;
        AH_THROW_RUNTIME(msg.str());
      }
      if(errorStatus == 1) {
        // Fill null values to all of the columns written by this task and go to the next event
        AH_INFO(ahlog::LOW) << "Grade and PHA determination resulted in null row " << ahfits::currentRow(fpout) << std::endl;
        num_null_grade++; 
        num_null_rows++;
        if(param.m_debugcol) {
          phaSpTh = sxipilib::PHA_TNULL;
          gradeSpTh = sxipilib::GRADE_TNULL;
        }
        sxipilib::setOutputNull(phaEvt,gradeEvt,piEvt);
        statusEvt[29] = 1;
        ahfits::writeRow(fpout);
        continue;
      }
      

      // Step 5: PHA to PI conversion
      // Event column info: PHA, TIME, CCD_ID, SEGMENT, READNODE
      if (param.m_gaincor) {
        //if startcol=PHA, use the PHA value from the event list ("phaevt")
        if (param.m_startcol == "PHA") {
          pha_tmp = phaEvt;   
        } else { //otherwise use the PHA value from step 4 ("pha")
          pha_tmp = pha;
          phaEvt = pha_tmp;
        }
       
        sxipilib::correctGain(pha_tmp, ccdidEvt, segmentEvt, readnodeEvt, gaindat, 
                              piEvt, errorStatus);
        if(errorStatus == 2) { 
          std::stringstream msg;
          msg << "Bad event during gain correction at row " << ahfits::currentRow(fpout) << ". Error status " << errorStatus << std::endl;
          AH_THROW_RUNTIME(msg.str());
        }
        if(errorStatus == 1) {
          // Fill null values to all of the columns written by this task and go to the next event
          AH_INFO(ahlog::LOW) << "Bad event at row " << ahfits::currentRow(fpout) << " Skipping event." << std::endl;
          num_null_pi++;
          num_null_rows++;
          piEvt = sxipilib::PI_TNULL;
          statusEvt[29] = 1;
          ahfits::writeRow(fpout);
          continue;
        } 
      } else {
        //no gain correction, just copy PHA->PI
        piEvt = pha;
        phaEvt = pha;
      }

      num_good++;
      ahfits::writeRow(fpout);
    } // end procstatus
  } // end event loop

  // Fill in min, max and null value keywords
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TLMIN",ahfits::name2Num(fpout,"PHA")),sxipilib::PHA_TLMIN,""); 
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TLMAX",ahfits::name2Num(fpout,"PHA")),sxipilib::PHA_TLMAX,""); 
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TLMIN",ahfits::name2Num(fpout,"PI")),sxipilib::PI_TLMIN,""); 
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TLMAX",ahfits::name2Num(fpout,"PI")),sxipilib::PI_TLMAX,""); 

  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"PHA")),sxipilib::PHA_TNULL,""); 
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"PI")),sxipilib::PI_TNULL,""); 
  ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"GRADE")),sxipilib::GRADE_TNULL,""); 
  if(param.m_debugcol) {
    ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"PHA_SPTH")),sxipilib::PHA_TNULL,""); 
    ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"GRADE_SPTH")),sxipilib::GRADE_TNULL,""); 
  }
  if (param.m_copygrade) { 
    ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"PHA_COPY")),sxipilib::PHA_TNULL,""); 
    ahfits::writeKeyValLLong(fpout,sxipilib::concatenate("TNULL",ahfits::name2Num(fpout,"GRADE_COPY")),sxipilib::GRADE_TNULL,"");
  } 

  AH_INFO(ahlog::HIGH) << "Number of events with special status:      " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events read:                   " << num_events << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number successfully processed:           " << num_good << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number filled with NULLs:                " << num_null_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "    bad PROC_STATUS:                       " << num_bad_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "    TIME=NULL:                             " << num_bad_time << std::endl;
  AH_INFO(ahlog::HIGH) << "    PHAS[0] < event threshold:             " << num_bad_phas0 << std::endl;
  AH_INFO(ahlog::HIGH) << "    CCD_ID out of range:                   " << num_bad_ccdid << std::endl;
  AH_INFO(ahlog::HIGH) << "    SEGMENT out of range:                  " << num_bad_segment << std::endl;
  AH_INFO(ahlog::HIGH) << "    READNODE out of range:                 " << num_bad_readnode << std::endl;
  AH_INFO(ahlog::HIGH) << "    RAWX out of range:                     " << num_bad_rawx << std::endl;
  AH_INFO(ahlog::HIGH) << "    ACTY out of range:                     " << num_bad_acty << std::endl;
  AH_INFO(ahlog::HIGH) << "    ADCAVE = NULL:                         " << num_bad_adcave << std::endl;
  AH_INFO(ahlog::HIGH) << "    input startcol = NULL:                 " << num_bad_startcol << std::endl;
  AH_INFO(ahlog::HIGH) << "    PHAS[0-8] < par.negativeph:            " << num_bad_phas << std::endl;
  AH_INFO(ahlog::HIGH) << "    NULL set by even/odd correction:       " << num_null_evenodd << std::endl;
  AH_INFO(ahlog::HIGH) << "    NULL set by charge trail correction:   " << num_null_chtrail << std::endl;
  AH_INFO(ahlog::HIGH) << "    NULL set by CTI correction:            " << num_null_cti << std::endl;
  AH_INFO(ahlog::HIGH) << "    NULL set by grade/PHA calculation:     " << num_null_grade << std::endl;
  AH_INFO(ahlog::HIGH) << "    NULL set by gain (PHA->PI) correction: " << num_null_pi << std::endl;

  AH_INFO(ahlog::HIGH) << "Finished." << std::endl;

} // end doWork

// ****************************************************************************

void finalize(ahfits::FilePtr & fpout) {
  
  // Close all FITS file pointers
  ahfits::close(fpout);
  AH_INFO(ahlog::HIGH) << "Finished." << std::endl;

} // end finalize

// ****************************************************************************

/** @} */

/* Revision Log
 $Log: sxipi.cxx,v $
 Revision 1.93  2016/07/27 22:39:11  rshill
 Corrected parsing of EVTTHRE to detect error conditions:
 (1) non-numeric string; (2) wrong number of values specified.

 Revision 1.92  2016/04/06 19:58:45  rshill
 Added STATUS bits section to prologue.

 Revision 1.91  2016/03/24 18:56:39  mdutka
 Adding parameter logging to standard main

 Revision 1.90  2016/01/29 19:26:27  mdutka
 Updating sxipi to move to the correct extension depending on the value of DATAMODE

 Revision 1.89  2016/01/15 14:50:22  mdutka
 removing tnull from array columns, this causes ftverify to fail.  Also using multiple routers to connect to video temperature file, fixes bug with hkvideoid parameter

 Revision 1.88  2015/12/29 18:14:14  mwitthoe
 sxipi: throw error if there are no rows in the input file

 Revision 1.87  2015/12/29 13:44:28  mdutka
 Changing handling of of CALDB files when the CALDB query is not performed

 Revision 1.86  2015/12/04 14:21:59  asargent
 Changed default clobber value to no, removed extra print statement.

 Revision 1.85  2015/12/04 05:57:22  asargent
 Read the EVENTTHR keyword and check events against threshold

 Revision 1.84  2015/11/20 18:29:49  mwitthoe
 sxipi: add column comments

 Revision 1.83  2015/11/12 17:51:14  mdutka
 Updating sxipi to account for inverse echo effect

 Revision 1.82  2015/11/09 13:47:13  mdutka
 commiting updated version of sxipi from erci miller

 Revision 1.81  2015/11/02 13:52:58  mdutka
 phaEvt is now set even if gaincor is off so the pha column is written

 Revision 1.80  2015/10/29 22:13:30  mdutka
 corrected bug with updating PHA values

 Revision 1.79  2015/10/29 17:54:08  mdutka
 correcting debugging output

 Revision 1.78  2015/10/29 16:46:43  mdutka
 adding debugging output

 Revision 1.77  2015/10/28 20:48:20  mdutka
 Commiting sxipi after bug fix

 Revision 1.76  2015/10/21 19:36:18  mdutka
 Checking in sxipi after adding updated CTI correction routine

 Revision 1.75  2015/09/16 15:00:06  klrutkow
 Use the ahmission calbd resolve query for all CALDB files ; remove redundant if-blocks

 Revision 1.74  2015/08/19 15:52:46  mwitthoe
 sxipi: change TNULL from 4095 to 4096; create function to set NULL to reduce duplication; create constants for TLMIN, TLMAX, and TNULL for PHA, PI, and GRADE

 Revision 1.73  2015/08/17 23:53:10  asargent
 Updated FITS file checking just after opening

 Revision 1.72  2015/08/17 16:31:53  mwitthoe
 sxipi: move a couple log statements outside of the row loop

 Revision 1.71  2015/08/17 15:54:24  mwitthoe
 sxipi: add counters to total events read and succesfully processed; fixed cases where counters were never incremented

 Revision 1.70  2015/08/10 15:40:40  mwitthoe
 sxipi: add standard prologue, stamp parameters to log file; move non-standard functions from sxipi.cxx to sxipilib.h/cxx; general clean-up

 Revision 1.69  2015/07/31 21:37:06  mdutka
 Adding logging to sxipi

 Revision 1.68  2015/06/09 20:16:34  mdutka
 cleaning up screen output info and waring streams

 Revision 1.67  2015/06/08 20:11:37  mdutka
 adding handling time = null in event and hk file

 Revision 1.66  2015/06/08 16:13:27  mdutka
 changing warnings to errors

 Revision 1.65  2015/06/08 15:55:03  mdutka
 changing conditionals for loading caldb data

 Revision 1.64  2015/06/08 15:29:31  mdutka
 adding debug info

 Revision 1.63  2015/06/08 15:16:50  mdutka
 debugging

 Revision 1.62  2015/06/08 14:58:20  mdutka
 debugging...

 Revision 1.61  2015/06/08 14:14:19  mdutka
 updating logic of dowork routine

 Revision 1.60  2015/06/05 20:36:32  mdutka
 moved deleting columns to end of DoWork

 Revision 1.59  2015/06/05 20:20:13  mdutka
 adding hkvideoid par

 Revision 1.58  2015/06/05 16:29:30  mdutka
 fixed typo

 Revision 1.57  2015/06/05 16:13:41  mdutka
 hkcoltem -> hkcolstem, support for correction on but starting at later column, delete debug cols if they are present and if debucol = no

 Revision 1.56  2015/06/04 21:27:29  mdutka
 debug

 Revision 1.55  2015/06/04 21:13:13  mdutka
 PHA startcol behavior debug

 Revision 1.54  2015/06/04 21:07:46  mdutka
 debug

 Revision 1.53  2015/06/04 21:02:07  mdutka
 debug

 Revision 1.52  2015/06/04 20:57:42  mdutka
 debug

 Revision 1.51  2015/06/04 20:38:03  mdutka
 debug

 Revision 1.50  2015/06/04 20:04:39  mdutka
 debug

 Revision 1.49  2015/06/04 19:56:38  mdutka
 debug

 Revision 1.48  2015/06/04 19:42:02  mdutka
 fixing issue searchVT

 Revision 1.47  2015/06/04 19:38:21  mdutka
 Adding debug info

 Revision 1.46  2015/06/04 19:30:19  mdutka
 adding more debug info

 Revision 1.45  2015/06/04 19:25:28  mdutka
 adding debug info to search VT

 Revision 1.44  2015/06/04 19:13:40  mdutka
 ccdid char ->int

 Revision 1.43  2015/06/04 18:47:27  mdutka
 adding debug evenodd

 Revision 1.42  2015/06/04 18:23:07  mdutka
 fixed writing of uncorrected columns

 Revision 1.41  2015/06/04 18:00:01  mdutka
 add pha startcol

 Revision 1.40  2015/06/04 17:53:07  mdutka
 fixed randomization for startcol == phas

 Revision 1.39  2015/06/04 17:30:54  mdutka
 fixed misspelling

 Revision 1.38  2015/06/04 17:16:56  mdutka
 fixed bugs

 Revision 1.37  2015/06/04 15:44:26  mdutka
 Changing logic of the tool, there were a few bugs

 Revision 1.36  2015/06/04 13:36:27  mdutka
 removing randomization if starting col is nt PHAS

 Revision 1.35  2015/06/03 20:56:45  mdutka
 fixed randomization

 Revision 1.34  2015/06/03 19:57:22  mdutka
 adding tnull support for PHAS column

 Revision 1.33  2015/06/03 19:22:16  mdutka
 adding chatter

 Revision 1.32  2015/06/03 18:27:18  mdutka
 correct bug in code for case hk file == none and evenodcor = yes

 Revision 1.31  2015/06/03 14:32:40  mdutka
 changed status bit numbers after adding bit 24 to sxiphas

 Revision 1.30  2015/06/02 19:33:49  mdutka
 fixed bug with status column now readwrite instead of readonly

 Revision 1.29  2015/05/27 20:21:45  mdutka
 fixed bug when reading hkfile

 Revision 1.28  2015/05/26 17:41:15  mdutka
 Changed error message for error status 2 in correct even odd subroutine, now will report that the HK file is out of time order

 Revision 1.27  2015/05/22 17:03:58  mdutka
 change keyword code looks for in CTI caldb file

 Revision 1.26  2015/05/22 14:53:15  mdutka
 reordered remaining status bits not caught in first pass

 Revision 1.25  2015/05/22 13:13:34  mdutka
 Chaging flag bit order and implementing CALDB

 Revision 1.24  2015/04/13 16:27:42  mdutka
 fixing memory bug for status bit column and issues Eric identified in first report on sxipi Apr9_2015

 Revision 1.23  2015/03/18 14:43:26  mdutka
 sxi tool change see Issue #490

 Revision 1.22  2015/01/06 21:29:10  mwitthoe
 sxipi: change parameter hkhdu to hkext; see issue 472

 Revision 1.21  2014/12/30 22:09:28  asargent
 Updated parameter descriptions

 Revision 1.20  2014/12/30 21:41:54  mwitthoe
 sxipi: update parameters; see issue 472

 Revision 1.19  2014/12/11 21:46:33  asargent
 Added check during even odd correction that PHAS is greater than 0

 Revision 1.18  2014/12/11 21:19:27  asargent
 Bug fix: during even odd correction, an integer array of the event PHAS was passed into correctEvenOdd when it needed to be a double. Added randomization parameters. Added in option for NONE for certain input files. Updated the HK file reading to store as a data structure for faster access.

 Revision 1.17  2014/11/05 19:41:32  asargent
 Removed debug column TNULL keywords, replaced null values (-32768) with NaN

 Revision 1.16  2014/10/29 21:41:20  asargent
 Added boundary checking for each event and TNULL,TLMIN,TLMAX keyword writing.

 Revision 1.15  2014/09/30 19:47:25  asargent
 New extension and column name parameters for HK file. Moved HK router to doWork function for speed increase.

 Revision 1.14  2014/09/18 20:07:20  asargent
 Changed event file keyword from CISTART to CIFIRST. Made extension explicit in HK file.

 Revision 1.13  2014/09/15 20:59:41  mwitthoe
 sxipi: add support for extended syntax on the input file; issue 179

 Revision 1.12  2014/09/15 14:42:37  mwitthoe
 sxipi: remove instrument argument for procstatus::processRow(); see issue 412

 Revision 1.11  2014/09/08 19:35:10  asargent
 Updates to sxipi to include changes in split threshold parameters, more thorough checks in start columns.

 Revision 1.10  2014/07/17 14:21:46  asargent
 Changed instances of float to double for better precision between different architectures.

 Revision 1.9  2014/06/23 13:59:50  asargent
 Updated sxipi to include more debugging options for starting PHAS column. Also changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

 Revision 1.8  2014/05/14 20:52:38  asargent
 Updated file pointer names.

 Revision 1.7  2014/04/02 16:07:14  asargent
 Updated length of phasEvt to represent length from FITS file. Incorrect allocation was causing a segmentation fault on 32-bit machines

 Revision 1.6  2014/04/01 15:02:56  asargent
 Updates to algorithms in searchVT(), searchSPTH() and several other changes in accordance with updated TRF v20140328. General cleanup.

 Revision 1.5  2014/02/27 19:21:12  asargent
 Added in more to the correction algorithm in correctEvenOdd(), fixed typos.

 Revision 1.4  2014/02/20 19:15:41  asargent
 First working version of sxipi. Changes with reading of keywords as strings. Added function to convert strings arrays. Minor comment changes.

 Revision 1.3  2014/02/11 16:43:17  asargent
 Changed debug columns to correct types.

 Revision 1.2  2014/02/11 15:34:54  asargent
 Fixed typo

 Revision 1.1  2014/02/11 15:33:31  asargent
 First version of sxipi

*/
