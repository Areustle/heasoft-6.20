/// \file ahtime.cxx
/// \brief Assign time to housekeeping and event files.
/// \author Mike Witthoeft
/// \date $Date: 2016/04/21 18:24:06 $
/// \version 1.0

/** 

\defgroup tool_ahtime Time Assignment (ahtime)
@ingroup mod_mission_tasks

Assign the time to all science (SXS , SXI, HXI, SGD, Shield-HXI, Shield-SGD, 
CAMS, MXS and attitude and special GTI) and HK data.

Source files:

  ahtime.cxx
  ahtimelib.cxx
  ahtimelib.h

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahtime

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-27   MCW    Clean-up code

*/
 
#define AHLABEL tool_ahtime
#define AHCVSID "$Id: ahtime.cxx,v 1.132 2016/04/21 18:24:06 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahtimelib.h"
#include "ahapp/ahapp.h"
#include "ahmath/ahmath.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahtime/ahtime.h"
#include "ahmission/ahmission.h"    // procstatus::processRow
#include "ahmission/caldb.h"
#include "ahmission/delay.h"
#include "ahmission/camstoffset.h"
#include "ahgen/ahgen.h"


#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas.h"    // expand_item_list
#include "hdcal.h"     // HDgtcalf

#include <iostream>
#include <sstream>
#include <cmath>
#include <exception>

/** \addtogroup tool_ahtime
 *  @{
 */

/// \brief Get parameter values.
/// \param[out] infile name of input HK or event file
/// \param[out] outfile name of output file
/// \param[out] timfile name of TIM file
/// \param[out] lookupfile name HK file with local times
/// \param[out] coldeffile name of column definition FITS file (or CALDB)
/// \param[out] delayfile name of instrument delay FITS file (or CALDB)
/// \param[out] offsetfile name of CAMS offset file (or CALDB)
/// \param[out] leapsecfile leap second FITS file location (or CALDB)
/// \param[out] timecol name of column holding assigned times
/// \param[out] startcol name of START column for SXS lost events
/// \param[out] stopcol name of STOP column for SXS lost events
/// \param[out] calctime true to assign TIME column
/// \param[out] calcutc true to calculate UTC columns (HK only)
/// \param[out] writekeys true to write output timing keywords
/// \param[out] interp interpolation type
void getPar(std::string& infile, std::string& outfile, std::string& timfile, 
            std::string& lookupfile, std::string& coldeffile, 
            std::string& delayfile, std::string& offsetfile,
            std::string& leapsecfile, std::string& timecol, 
            std::string& startcol, std::string& stopcol, bool& calctime, 
            bool& calcutc, bool& writekeys, int& interp);

/// \brief Prepare for tool operation: 1) copy contents of input file to output
///        file; 2) load leap second FITS file; 3) load column definitions; 
///        and 4) load TIM file data.
/// \param[in] infile name of input HK or event file
/// \param[in] outfile name of output file (initialize will check (and remove)
///            a starting ! character indicating clobber
/// \param[in] timecol name of column holding assigned times
/// \param[in] timfile name of TIM file
/// \param[in] calctime true to assign TIME column
/// \param[in] leapsecfile location of leap second FITS file (or CALDB)
/// \param[in,out] coldeffile name of column definition FITS file (or CALDB)
/// \param[out] fptr FITS file pointer to input file
/// \param[out] timdat look up tables between L32TI and TIME from TIM file
/// \param[out] leapsecdat leap second data
/// \param[out] coldat contains column name data from column definitions file
/// \param[out] datetime  date and time used to query CALDB
void initialize(const std::string& infile, std::string& outfile,
                const std::string& timecol, const std::string& timfile,
                bool calctime, const std::string& leapsecfile,
                std::string& coldeffile, ahfits::FilePtr & fptr, 
                ahmission::timfile::TimFileData& timdat, 
                ahtime::leapsec::LeapSecTable & leapsecdat,
                timecoldef::ColDefInfo& coldat, std::string & datetime);

/// \brief Calculate and fill TIME column.
/// \param[in] fptr FITS file pointer to input file
/// \param[in] timecol name of TIME column to be updated
/// \param[in] startcol name of START column for SXS lost events
/// \param[in] stopcol name of STOP column for SXS lost events
/// \param[in] calctime true to assign TIME column
/// \param[in] calcutc true to calculate UTC columns (HK only)
/// \param[in] writekeys true to write output timing keywords
/// \param[in] interp interpolation type enumeration
/// \param[in] lookupfile name of local HK file needed for science data
/// \param[in] offsetfile name of CAMS offset FITS file (or CALDB)
/// \param[in] delayfile name of instrument delay FITS file (or CALDB)
/// \param[in] coldat contains column name data from column definitions file
/// \param[in] leapsecdat leap second data
/// \param[in] timdat look up tables between L32TI and TIME from TIM file
/// \param[in] datetime date and time used to query CALDB
void doWork(ahfits::FilePtr fptr, const std::string& timecol, 
            const std::string& startcol, const std::string& stopcol,
            bool calctime, bool calcutc, bool writekeys, int interp, 
            const std::string& lookupfile, std::string offsetfile, 
            std::string delayfile, timecoldef::ColDefInfo& coldat, 
            ahtime::leapsec::LeapSecTable & leapsecdat, 
            ahmission::timfile::TimFileData& timdat, std::string & datetime);

/// \brief close open FITS files
/// \param[in] fptr FITS file pointer to input file
void finalize(ahfits::FilePtr fptr);


// ****************************************************************************

/// \brief ahtime tool
int main(int argc, char** argv) {

  std::string infile;              // name of HK or event FITS file
  std::string outfile;             // name of output file (will copy input)
  std::string timfile;             // name of TIM file
  std::string lookupfile;          // name of HK local time file
  std::string coldeffile;          // name of column definitions FITS file
  std::string delayfile;           // name of instrument delay FITS file
  std::string offsetfile;          // name of CAMS offset FITS file
  std::string leapsecfile;         // name for leap second FITS file
  std::string timecol;             // name of column to be populated with time
  std::string startcol;            // name of START column for SXS lost events
  std::string stopcol;             // name of STOP column for SXS lost events;
  bool calctime=false;             // true to calculate TIME column
  bool calcutc=false;              // true to calculate UTC columns
  bool writekeys=false;            // true to write timing keywords
  int interp=0;                    // interpolation type string
  std::string datetime;            // time used to look up caldb

  ahfits::FilePtr fptr=0;                     // FITS file pointer to input file
  ahmission::timfile::TimFileData timdat;     // look up tables from TIM file
  timecoldef::ColDefInfo coldat;              // data from column definitions file
  ahtime::leapsec::LeapSecTable leapsecdat;   // leap second data from CALDB

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(infile,outfile,timfile,lookupfile,coldeffile,delayfile,offsetfile,
             leapsecfile,timecol,startcol,stopcol,calctime,calcutc,writekeys,interp);
      ahapp::writeParametersToLog(); 
      initialize(infile,outfile,timecol,timfile,calctime,leapsecfile,coldeffile,
                 fptr,timdat,leapsecdat,coldat,datetime);
      doWork(fptr,timecol,startcol,stopcol,calctime,calcutc,writekeys,interp,
             lookupfile,offsetfile,delayfile,coldat,leapsecdat,timdat,datetime);
      finalize(fptr);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile,outfile,timfile,lookupfile,coldeffile,delayfile,offsetfile,
               leapsecfile,timecol,startcol,stopcol,calctime,calcutc,writekeys,interp);
        initialize(infile,outfile,timecol,timfile,calctime,leapsecfile,coldeffile,
                   fptr,timdat,leapsecdat,coldat,datetime);
        doWork(fptr,timecol,startcol,stopcol,calctime,calcutc,writekeys,interp,
               lookupfile,offsetfile,delayfile,coldat,leapsecdat,timdat,datetime);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fptr);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unanble to start up tool." << std::endl;
  }

  return status;

}

// ****************************************************************************

void getPar(std::string& infile, std::string& outfile, std::string& timfile, 
            std::string& lookupfile, std::string& coldeffile, 
            std::string& delayfile, std::string& offsetfile,
            std::string& leapsecfile, std::string& timecol, 
            std::string& startcol, std::string& stopcol, bool& calctime, 
            bool& calcutc, bool& writekeys, int& interp) {

  infile=ahapp::getParString("infile");
  outfile=ahapp::getParString("outfile");
  timfile=ahapp::getParString("timfile");
  lookupfile=ahapp::getParString("lookupfile");
  coldeffile=ahapp::getParString("coldeffile");
  delayfile=ahapp::getParString("delayfile");
  offsetfile=ahapp::getParString("offsetfile");
  leapsecfile=ahapp::getParString("leapsecfile");
  timecol=ahapp::getParString("timecol");
  calctime=ahapp::getParBool("calctime");
  calcutc=ahapp::getParBool("calcutc");
  writekeys=ahapp::getParBool("writekeys");

  // Read interpolation parameter and convert it into the ahmath
  // enumerated value.
  std::string tinterp=ahgen::strtoupper(ahapp::getParString("interp"));
  interp=ahmath::interpolation_type(tinterp);

  // Read gticolumns parameter and get two column names
  std::string tgticolumns=ahgen::strtoupper(ahapp::getParString("gticolumns"));
  char* instr=(char*)tgticolumns.c_str();
  char** items=0;
  int nitems=0;
  int status=0;
  int trim=1;         // trim spaces
  int skip=1;         // exclude empty items
  int guard=0;        // do not protect against commas in parentheses
  items=expand_item_list(instr, &nitems, ',',trim,skip,guard,&status);
  if (status != 0 || nitems != 2)
    AH_THROW_RUNTIME("invalid value for gticolumns parameter; expect two values separated by a comma");
  startcol=items[0];
  stopcol=items[1];

  // calctime and calcutc cannot both be false.
  if (!calctime && !calcutc)
    AH_THROW_RUNTIME("calctime and calcutc both false; nothing to do!");

}

// ****************************************************************************

void initialize(const std::string& infile, std::string& outfile,
                const std::string& timecol, const std::string& timfile,
                bool calctime, const std::string& leapsecfile,
                std::string& coldeffile, ahfits::FilePtr & fptr, 
                ahmission::timfile::TimFileData& timdat, 
                ahtime::leapsec::LeapSecTable & leapsecdat,
                timecoldef::ColDefInfo& coldat, std::string& datetime) { 

  std::string colfilename;    // name of column definitions FITS file (CALDB)
  std::string leapfilename;   // name of leap second FITS file (CALDB)

  // Copy contents of input to output and return opened output file (fptr);
  // if editing input file in-place (allowed with last argument = true), 
  // fptr will point to the opened input file.
  ahfits::clone(infile,outfile,&fptr,true);
  AH_INFO(ahlog::HIGH) << "Cloned input file, " << infile << ", to output file, " << outfile << "." << std::endl;

  // Perform CALDB query for column definition file, if necessary
  if (calctime) {

    //get tstart in MET, using first row of S_TIME columne
    bool foundTime = false;  
    double tstart = 0.;

    //go to first extension that has S_TIME column and has data 
    while (1) {
      if (!ahfits::haveColumn(fptr,"S_TIME") || ahfits::numRows(fptr) == 0) {
        if (ahfits::nextHDU(fptr)) {
          continue;
        } else {
          break;
        }
      } else { 
        foundTime = true;
        break;
      }
    }

    //if S_TIME with data was found read the first first and use that 
    //time to search CALDB
    if (foundTime) {      
      ahfits::Router router_tstart(fptr); 
      router_tstart.connectScalar(ahfits::e_READONLY,"S_TIME",tstart);
      ahfits::firstRow(fptr);
      ahfits::readRow(fptr);
      router_tstart.clearConnections();
    } else {
      std::stringstream err;
      err << "No S_TIME column found in " << infile 
          << " can not perform CALDB query";
      AH_THROW_RUNTIME(err.str());
    }

    //Obtain epoch
    long long  mjdrefi=(int)ahfits::getKeyValLLong(fptr,"MJDREFI");
    double mjdreff=ahfits::getKeyValDbl(fptr,"MJDREFF");

    ahtime::AhDateTime epoch;   // epoch as terrestrial time
    ahtime::AhMJDTime epoch_mjd(mjdrefi,mjdreff);
    ahtime::reformatMJDAsDateTime(epoch_mjd,epoch);
    ahtime::AhDateTime dateobs;
    ahtime::convertMissionTimeToTT(tstart, epoch, dateobs);
    datetime = dateobs.getDateTimeAsStr();

    colfilename=ahmission::caldb::resolve(coldeffile,"column definitions","GEN","-","TIMECOLDEF","-");
    AH_INFO(ahlog::HIGH) << "CALDB File found: " << coldeffile << std::endl;

    // Load column definitions from CALDB (or local file).
    ape_trad_set_string("coldeffile",colfilename.c_str());   // to record actual file path in history
    timecoldef::load(colfilename,coldat);
  }

  // read leapsecond data
  leapfilename=ahmission::caldb::resolve(leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
  AH_INFO(ahlog::LOW) << "Using leapsecond file: " << leapfilename <<std::endl;
  ahtime::leapsec::load(leapfilename,leapsecdat);
  
  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("leapsecfile",leapfilename.c_str());

  // Read TIM data: TIME, L32TI, and STATUS; construct two lookup tables:
  //  1. between L32TI and TIME including illegal STATUS rows
  //  2. between L32TI and TIME excluding illegal STATUS rows
  // Construct mapping of indices between first and second lookup tables
  if (calctime) {
    ahmission::timfile::loadTimFile(timfile,timdat);
    AH_INFO(ahlog::HIGH) << "Loaded TIM file: " << timfile << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of total rows: " << timdat.m_size1 << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of non-NULL rows: " << timdat.m_size2 << std::endl;
  }

}

// ****************************************************************************

void doWork(ahfits::FilePtr fptr, const std::string& timecol, 
            const std::string& startcol, const std::string& stopcol,
            bool calctime, bool calcutc, bool writekeys, int interp, 
            const std::string& lookupfile, std::string offsetfile, 
            std::string delayfile, timecoldef::ColDefInfo& coldat, 
            ahtime::leapsec::LeapSecTable & leapsecdat, 
            ahmission::timfile::TimFileData& timdat, std::string & datetime) {

  // maximum value of L32TI
  unsigned long l32timask=0xFFFFFFFF;
  double l32timax=(double)l32timask;

  // Data structure to hold data from the CAMS time offset CALDB file.
  ahmission::camstoffset::DataType camsoffdat;  // CAMS time offset data from CALDB

  // Store keyword values from active HDU
  std::string key_extname;
  std::string key_instrume;
  std::string key_detnam;
  std::string key_datamode;

  // Identify type of active HDU
  std::string optype;       // Type of operation to perform
  std::string sysname;      // SYSTEM column value in col def CALDB file
  std::string delayname;    // HDU name in delay file
  bool isinstgti=false;     // true if instrument-specific GTI extension

  // Store delays from instrument delay CALDB file
  double delaytime1=0.;
  double delaytime2=0.;

  // Variables needed for the SXI fine-TI calculation; full description
  // of these quantities is given later
  SXIParams sxipars;

  // Lookup tables
  // 1 table is needed for HXI/SGD; 4 tables needed for SXS
  LookupTable lutables[4];

  // Fine-TI value; SXS_GTILOST requires two fine-TIs
  double fineti=0.;          // fine-TI
  double fineti2=0.;         // for GTI and SXS event files

  // Report if extrapolation was required to compute fine-TI from the
  // lookup table or TIME from the TIM file.  This can happen when the 
  // search position in the lookup table or TIM file is next to a 
  // discontinuity of the sawtooth function.
  bool extrap=false;

  // Report if TIM lookup fails.  This occurs when the search position
  // for interpolation is adjacent to a bad-STATUS TIM row.
  bool badtim=false;

  // Keep track of current row of TIM file for faster searching; this gets
  // reset to zero at the start of each HDU.  This index refers to the 
  // row number of the complete TIM file (including invalid rows).
  unsigned long timidx=0;

  // Variables used to get epoch in DateTime format.
  int mjdrefi=0;
  double mjdreff=0.;
  ahtime::AhDateTime ttepoch;   // epoch as terrestrial time
  ahtime::AhDateTime epoch;     // epoch as UTC

  // Variable of struct to hold row data from input file.
  EventDat rowdat;

  // Variables used in row loop.
  double time0=-1.;               // first non-NULL TIME (to be written as TSTART in output)
  double time1=-1.;               // last non-NULL TIME (to be written as TSTOP in output)
  std::string tutcstr;            // used to construct TSTART/TSTOP output keywords

  // TSTART and TSTOP keywords in primary HDU should have the smallest
  // and largest TSTART/TSTOP values over all extensions where TIME is
  // assigned
  double pri_tstart=-1.;
  double pri_tstop=-1.;

  // row counters for file
  Counters count_tot;

  // loop over HDUs
  ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);  // move to first binary table
  do {

    // row counters for extension
    Counters count_ext;

    // save current parameter stamping state
    bool parstampstate=ahfits::isParameterStamping(fptr);

    // skip extension if TIME column not present
    if (!ahfits::haveColumn(fptr,"TIME")) continue;

    // Reset search index of TIM file
    timidx=0;

    // Reset the search indices for the HK lookup tables.
    for (int ilu=0; ilu < 4; ilu++) lutables[ilu].m_luidx=0;

    // Need to read keywords to determine which operation to perform.
    key_extname=ahgen::strtoupper(ahfits::getKeyValStr(fptr,"EXTNAME"));
    if (ahfits::keywordExists(fptr,"INSTRUME"))
      key_instrume=ahgen::strtoupper(ahfits::getKeyValStr(fptr,"INSTRUME"));
    if (ahfits::keywordExists(fptr,"DETNAM")) 
      key_detnam=ahgen::strtoupper(ahfits::getKeyValStr(fptr,"DETNAM"));
    if (ahfits::keywordExists(fptr,"DATAMODE")) 
      key_datamode=ahgen::strtoupper(ahfits::getKeyValStr(fptr,"DATAMODE"));
    AH_INFO(ahlog::LOW) << "Processing EXTNAME: " << key_extname << std::endl;
    AH_INFO(ahlog::LOW) << "  INSTRUME, DETNAM, DATAMODE: " << key_instrume 
                        << ", " << key_detnam << ", " << key_datamode << std::endl;

    // Determine operation type from header keywords; also system
    // name used in column definitions file and extension in delay file.
    optype="BAD";
    sysname="BAD";
    delayname="BAD";
    isinstgti=false;       // used for instrument-specific GTI extensions
    if (key_extname.substr(0,3) == "GTI") isinstgti=true;    // extension is GTIxxxx
    if (key_extname.substr(0,5) == "GTIHK") {                // extension is GTIHKxxxx
      isinstgti=false;
      sysname="HK";
      optype="GTI";
    } else if (key_extname == "ATTITUDE") {
      sysname="HK";
      optype="HK";
    } else if (key_extname.find("HK_") == 0) {
      if (key_extname.find("FWE") != std::string::npos) {
        sysname="HK";
        delayname="FW";
        optype="MXS";
      } else if (key_extname == "HK_HXI1_CAM_SCL" || key_extname == "HK_HXI2_CAM_SCL") {
        sysname="HK_HXI#_CAM_SCL";
        optype=key_instrume;    // HXI1 or HXI2
      } else if (key_extname == "HK_SGD1_CC1_SCL" || key_extname == "HK_SGD2_CC1_SCL" ||
                 key_extname == "HK_SGD1_CC2_SCL" || key_extname == "HK_SGD2_CC2_SCL" ||
                 key_extname == "HK_SGD1_CC3_SCL" || key_extname == "HK_SGD2_CC3_SCL") {
        sysname="HK_SGD#_CC#_SCL";
        optype=key_instrume;    // SGD1 or SGD2
      } else {
        ahfits::disableParameterStamping(fptr);
        sysname="HK";
        optype="HK";
      }
    } else if (key_instrume == "HXI1") {
      delayname="HXI";
      if (key_detnam == "CAMERA") {
        sysname="HXI_CAMERA";
        optype="HXI1";
      } else if (key_detnam == "SHIELD") {
        if (key_datamode == "GRB") {
          sysname="HXI_SHIELDGRB";
          optype="HXI1";
        } else if (key_datamode == "SCALAR") {
          sysname="HXI_SHIELD1RATE";
          optype="HXI1_SCALAR";
        } else if (key_datamode == "HISTOGRAM") {
          sysname="HXI_SHIELD1HIST";
          optype="HXI1_SCALAR";
        }
      }
    } else if (key_instrume == "HXI2") {
      delayname="HXI";
      if (key_detnam == "CAMERA") {
        sysname="HXI_CAMERA";
        optype="HXI2";
      } else if (key_detnam == "SHIELD") {
        if (key_datamode == "GRB") {
          sysname="HXI_SHIELDGRB";
          optype="HXI2";
        } else if (key_datamode == "SCALAR") {
          sysname="HXI_SHIELD1RATE";   // This is not a typo; should be Shield1RATE
          optype="HXI2_SCALAR";
        } else if (key_datamode == "HISTOGRAM") {
          sysname="HXI_SHIELD1HIST";
          optype="HXI2_SCALAR";
        }
      }
    } else if (key_instrume == "SGD1") {
      delayname="SGD";
      if (key_detnam == "CC1" || key_detnam == "CC2" || key_detnam == "CC3") {
        sysname="SGD_CC";
        optype="SGD1";
      } else if (key_detnam == "SHIELD1" || key_detnam == "SHIELD2") {
        if (key_datamode == "GRB") {
          optype="SGD1";
          sysname="SGD_SHIELD1GRB";
        } else if (key_datamode == "SCALAR") {
          optype="SGD1_SCALAR";
          sysname="SGD_SHIELD1RATE";
        } else if (key_datamode == "HISTOGRAM") {
          sysname="SGD_SHIELD1HIST";
          optype="SGD1_SCALAR";
        }
      }
    } else if (key_instrume == "SGD2") {
      delayname="SGD";
      if (key_detnam == "CC1" || key_detnam == "CC2" || key_detnam == "CC3") {
        sysname="SGD_CC";
        optype="SGD2";
      } else if (key_detnam == "SHIELD1" || key_detnam == "SHIELD2") {
        if (key_datamode == "GRB") {
          optype="SGD2";
          sysname="SGD_SHIELD2GRB";
        } else if (key_datamode == "SCALAR") {
          optype="SGD2_SCALAR";
          sysname="SGD_SHIELD2RATE";   // This is not a typo; SGD2 has SHIELD2RATE; HXI2 has SHIELD1RATE
        } else if (key_datamode == "HISTOGRAM") {
          sysname="SGD_SHIELD2HIST";
          optype="SGD2_SCALAR";
        }
      }
    } else if (key_instrume == "SXI") {
      sysname="SXI";
      delayname="SXI";
      optype="SXI";
      if (key_datamode == "EXPOSURE" || key_datamode == "DFRAME" || 
          key_datamode == "IFRAME" || key_datamode == "RFRAME" ||
          key_datamode == "HOTPIX") {
        optype="SXI_NOWIN";
      }
    } else if (key_instrume == "SXS") {
      delayname="SXS";
      if (key_detnam == "ANTICO") {
        optype="SXS";
        sysname="SXS_ANTICO";
      } else if (key_detnam == "PIXEL") {
        optype="SXS_TRIG";
        if (key_datamode == "PX_PULSEREC") {
          sysname="SXS_PULSEREC";
        } else if (key_datamode == "PX_NOISEREC") {
          sysname="SXS_NOISEREC";
        } else if (key_datamode == "PX_WFRB") {
          optype="SXS";
          sysname="SXS_ANTICO";    // treated the same way as antico files
        } else {
          sysname="SXS_PIXEL";
        }
      }
      if (isinstgti) optype="SXS";   // make sure not to try SXS_TRIG operation
    } else if (key_instrume == "CAMS1" || key_instrume == "CAMS2") {
      sysname="CAMS";
      delayname="CAMS";
      if (key_instrume == "CAMS1")
        optype="CAMS1";
      else
        optype="CAMS2";
    }
    if (sysname == "BAD" || optype == "BAD") {
      AH_THROW_RUNTIME("Could not determine extension type: "+key_extname);
    }

    AH_INFO(ahlog::LOW) << "  Operation type: " << optype << std::endl;
    AH_INFO(ahlog::LOW) << "  System type: " << sysname << std::endl;
    if (delayname == "BAD")
      AH_INFO(ahlog::LOW) << "  Delay: NONE" << std::endl;
    else
      AH_INFO(ahlog::LOW) << "  Delay: " << delayname << std::endl;

    // get epoch from header values: MJDREFI & MJDREFF
    mjdrefi=(int)ahfits::getKeyValLLong(fptr,"MJDREFI");
    mjdreff=ahfits::getKeyValDbl(fptr,"MJDREFF");
    ahtime::AhMJDTime ttepoch_mjd(mjdrefi,mjdreff);
    ahtime::reformatMJDAsDateTime(ttepoch_mjd,ttepoch);
    ahtime::convertTTToUTC(ttepoch,leapsecdat,epoch);
    if (epoch.year() > 3000) {
      AH_THROW_RUNTIME("something is wrong with the input epoch (MJDREFI); the UTC year is greater than 3000");
    }

    // Get column names from column definitions CALDB file; output is colnames
    timecoldef::ColumnNames colnames;
    if (calctime) timecoldef::get(coldat,sysname,colnames);

    // load CAMS offset information from CALDB (or local file)
    if (calctime) {
      if (optype == "CAMS1" || optype == "CAMS2") {
        offsetfile=ahmission::caldb::resolve(offsetfile,"CAMS offset","CAMS","-","TIME_OFFSET",datetime);
        ape_trad_set_string("offsetfile",offsetfile.c_str());   // to record actual file path in history
        ahmission::camstoffset::load(offsetfile,camsoffdat);
      }
    }

    // Load instrument delays from CALDB (or local file).
    // Read the delay from the row with the largest TIME smaller than
    // the S_TIME value of the 1st row of the input HDU.
    if (calctime) {
      if (delayname != "BAD") {
        // Read S_TIME from 1st row
        double s_time0=0.;
        ahfits::firstRow(fptr);
        ahfits::Router tmprouter(fptr);
        tmprouter.connectScalar(ahfits::e_READONLY,"S_TIME",s_time0);
        ahfits::readRow(fptr);

        std::string codename="DELAY_"+delayname;
        delayfile=ahmission::caldb::resolve(delayfile,"Time delays","GEN","-",codename,datetime);  
        ape_trad_set_string("delayfile",delayfile.c_str());   // to record actual file path in history
        ahmission::delay::loadAndGet(delayfile,delayname,s_time0,delaytime1,delaytime2);
        AH_INFO(ahlog::HIGH) << "  Loaded delay times: delay1, delay2= " << delaytime1 << ", " << delaytime2 << std::endl;
      } else {
        delaytime1=0.;
        delaytime2=0.;
      }
    }

    // get lookup table from HXI or SGD HK file (parameter lookupfile)
    if (calctime) {
      if (optype == "HXI1" || optype == "HXI2" || optype == "SGD1" || optype == "SGD2") {
        AH_INFO(ahlog::HIGH) <<  "Load lookup table for HXI/SGD" << std::endl;
        ahfits::FilePtr fphk=NULL;  // ahfits FilePtr for HK file with look-up table
        long long u32ti_val=0;      // local variable for L32TI column in HK file
        long long ltime_val=0.;     // local variable for local time column in HK file
        double l32ti=0.;            // to hold L32TI value converted from U32TI
        char lu_proc_status[32];    // PROC_STATUS column for lookup table
        ahfits::IndexType lu_num_proc_status=32;
        for (int ips=0; ips < 32; ips++) lu_proc_status[ips]=0;
  
        // determine extension and column names of lookup table
        std::string name_hkextname=colnames.m_hkextname;
        std::string name_ti1hk=colnames.m_ti1hk;
        std::string name_ltime1hk=colnames.m_ltime1hk;
        std::string firsthash="1";      // what to fill in for 1st '#' character
        if (optype == "HXI2" || optype == "SGD2") firsthash="2";
        hashReplace(name_hkextname,firsthash);
        hashReplace(name_ti1hk,firsthash);
        hashReplace(name_ltime1hk,firsthash);
  
        // also need to update column name for LTIME1EVT for some cases
        if (sysname == "HK_HXI#_CAM_SCL") {
          hashReplace(colnames.m_ltime1evt,firsthash);
        } else if (sysname == "HK_SGD#_CC#_SCL") {
          std::string secondhash;
          if (key_detnam == "CC1")
            secondhash="1";
          else if (key_detnam == "CC2")
            secondhash="2";
          else if (key_detnam == "CC3")
            secondhash="3";
          else {
            std::stringstream msg;
            msg << "Expected DETNAM = CC1, CC2, or CC3 in extension " << key_extname << "; have DETNAM = " << key_detnam;
            AH_THROW_RUNTIME(msg.str());
          }
          hashReplace(colnames.m_ltime1evt,firsthash);
          hashReplace(colnames.m_ltime1evt,secondhash);
        }
  
        AH_INFO(ahlog::HIGH) << "  filename, extension: " << lookupfile << ", " << name_hkextname << std::endl;
        AH_INFO(ahlog::HIGH) << "  TI, LOCAL_TIME column names: " << name_ti1hk << ", " << name_ltime1hk << std::endl;
  
        // open HK file with lookup table
        ahfits::open(lookupfile,name_hkextname,&fphk);
        unsigned long nrows=ahfits::numRows(fphk);
  
        // make connections between lookup table columns and local variables
        ahfits::Router routhk(fphk);
        routhk.connectScalar(ahfits::e_READONLY,name_ti1hk,u32ti_val);
        routhk.connectScalar(ahfits::e_READONLY,name_ltime1hk,ltime_val);
        routhk.connectBit(ahfits::e_READONLY,"PROC_STATUS",lu_proc_status,
                        lu_num_proc_status);
  
        // Create temporary arrays to hold lookup table read from file.  It is
        // allocated to the size of the HK extension.  However, some of the 
        // HK rows may have a bad PROC_STATUS and are skipped so that the final
        // lookup table will have fewer rows.
        unsigned long lusize=0;
        double* luxdat=new double[nrows];
        double* luydat=new double[nrows];
  
        // Need to convert HK LOCAL_TIME column to same units as the event
        // LOCAL_TIME.  The conversion factor is stored in the column definitions
        // file as 2**ltfactbits.
        int ltfactbits=colnames.m_ltfactbits;
  
        // read lookup table
        ahfits::firstRow(fphk);
        unsigned long jrow=0;
        for (unsigned long irow=0; irow < nrows; irow++) {
          ahfits::readRow(fphk);
  //        if (procstatus::processRow(lu_proc_status)) {
          if (lu_proc_status[1] == 0) {              // do not skip lookup rows with Astro-H bit flagged as bad
            l32ti=(double)u32tol32(u32ti_val);
            luxdat[jrow]=l32ti;
            luydat[jrow]=(double)(ltime_val>>ltfactbits);
            jrow++;
          }
          ahfits::nextRow(fphk);
        }
        ahfits::close(fphk);
        lusize=jrow;
  
        // set up final lookup table (only first lookup table is needed)
        double luymax=pow(2.,colnames.m_ltime1bits-ltfactbits);
        initLookupTable(lutables[0],lusize,luymax);
        for (unsigned long irow=0; irow < lusize; irow++) {
          lutables[0].m_xdat[irow]=luxdat[irow];
          lutables[0].m_ydat[irow]=luydat[irow];
        }
  
        // free temporary lookup table
        delete [] luxdat; luxdat=0;
        delete [] luydat; luydat=0;
  
        AH_INFO(ahlog::HIGH) << "  Number of total lookup rows: " << nrows << std::endl;
        AH_INFO(ahlog::HIGH) << "  Number of lookup rows with good PROC_STATUS: " << lutables[0].m_size << std::endl;
        AH_INFO(ahlog::HIGH) << "  Range of L32TI:      " << ahlog::setprecision(15) << lutables[0].m_xdat[0] << " : " << lutables[0].m_xdat[jrow-1] << std::endl;
        AH_INFO(ahlog::HIGH) << "  Range of LOCAL_TIME: " << ahlog::setprecision(15) << lutables[0].m_ydat[0] << " : " << lutables[0].m_ydat[jrow-1] << std::endl;
  
      }
    }

    // Get the four lookup tables from SXS HK file (lookupfile)
    if (calctime) {
      if (optype == "SXS" || optype == "SXS_GTILOST" || optype == "SXS_TRIG") {
        AH_INFO(ahlog::HIGH) <<  "Load lookup table for SXS" << std::endl;
        ahfits::FilePtr fphk=NULL;  // ahfits FilePtr for HK file with look-up table
        long long u32ti_val=0;      // local variable for L32TI column in HK file
        double ltime_val=0.;        // local variable for local time column in HK file
        double l32ti=0.;            // to hold L32TI value converted from U32TI
        int pspid=0;                // local variable for PSP_ID column in HK file
        char lu_proc_status[32];    // PROC_STATUS column for lookup table
        ahfits::IndexType lu_num_proc_status=32;
        for (int ips=0; ips < 32; ips++) lu_proc_status[ips]=0;
  
        // since 4 lookup tables are in a single extension, need to read entire
        // table into temporary arrays and determine the size of each lookup 
        // table before storing them into the struct
        int nrows=0;        // store number of rows in FITS file
        double* tmp_ltime=0;
        double* tmp_l32ti=0;
        int* tmp_pspid=0;
        unsigned long count[4]={0,0,0,0};
        unsigned long idx[4]={0,0,0,0};
    
        AH_INFO(ahlog::HIGH) << "  filename, extension: " << lookupfile << ", " << colnames.m_hkextname << std::endl;
        AH_INFO(ahlog::HIGH) << "  TI, LOCAL_TIME column names: " << colnames.m_ti1hk << ", " << colnames.m_ltime1hk << std::endl;
  
        // open HK file with lookup tables
        ahfits::open(lookupfile,colnames.m_hkextname,&fphk);
    
        // make connections between lookup table columns and local variables    
        ahfits::Router routhk(fphk);
        routhk.connectScalar(ahfits::e_READONLY,colnames.m_ti1hk,u32ti_val);
        routhk.connectScalar(ahfits::e_READONLY,colnames.m_ltime1hk,ltime_val);
        routhk.connectScalar(ahfits::e_READONLY,colnames.m_ltime2hk,pspid);
        routhk.connectBit(ahfits::e_READONLY,"PROC_STATUS",lu_proc_status,
                          lu_num_proc_status);
  
        // allocate size of temporary arrays based on number of rows in file
        nrows=ahfits::numRows(fphk);
        tmp_ltime=new double[nrows];
        tmp_l32ti=new double[nrows];
        tmp_pspid=new int[nrows];
  
        // arrays to store previous row info per PSP_ID
        double last_ltime[4]={0.,0.,0.,0.};
        double last_u32ti[4]={0.,0.,0.,0.};
        unsigned long nskip[4]={0,0,0,0};
  
        // Read lookup tables into temporary arrays and count size of each table.
        // Rows that have the same L32TI or LOCAL_TIME as the previous row with
        // the same PSP_ID are skipped.
        ahfits::firstRow(fphk);
        long jrow=0;
        for (long irow=0; irow < nrows; irow++) {
          ahfits::readRow(fphk);
          if (lu_proc_status[1] == 0) {              // do not skip lookup rows with Astro-H bit flagged as bad

            // skip rows with same times as previous row with same PSP_ID
            if (ltime_val == last_ltime[pspid] || u32ti_val == last_u32ti[pspid]) {
              nskip[pspid]++;
            } else {
              count[pspid]++;     // count how many valid rows for each PSP
              l32ti=(double)u32tol32(u32ti_val);
              tmp_ltime[jrow]=ltime_val;
              tmp_l32ti[jrow]=l32ti;
              tmp_pspid[jrow]=pspid;
              jrow++;
            }
            last_ltime[pspid]=ltime_val;
            last_u32ti[pspid]=u32ti_val;
          }    // end if PROC_STATUS okay

          ahfits::nextRow(fphk);
        }
        ahfits::close(fphk);

        // allocate sizes of lookup tables based on counts above
        double ymax=pow(2.,colnames.m_ltime1bits);
        for (int ilu=0; ilu < 4; ilu++) {
          initLookupTable(lutables[ilu],count[ilu],ymax);
        }
    
        // construct lookup tables from temporary arrays
        // Note: look-up points that have the same L32TI or SampleCnt as the 
        //       previous point (from the same PSP) are skipped
        for (long irow=0; irow < jrow; irow++) {    // jrow is the number of non-skipped rows read from FITS file
          int pspid=tmp_pspid[irow];
          lutables[pspid].m_xdat[idx[pspid]]=tmp_l32ti[irow];
          lutables[pspid].m_ydat[idx[pspid]]=tmp_ltime[irow];
          idx[pspid]++;
        }
  
        AH_INFO(ahlog::LOW) << "  SXS lookup tables: " << std::endl;
        AH_INFO(ahlog::LOW) << "   PSP      lookup size         # rejected" << std::endl;
        for (int ilu=0; ilu < 4; ilu++) {
          AH_INFO(ahlog::LOW) << "   " << ilu << "         " << lutables[ilu].m_size << "                 " << nskip[ilu] << std::endl;
        }
        AH_INFO(ahlog::LOW) << std::endl;
        AH_INFO(ahlog::LOW) << "   PSP               L32TI range                    LOCAL_TIME range" << std::endl;
        for (int ilu=0; ilu < 4; ilu++) {
          unsigned int ilast=lutables[ilu].m_size-1;
          double xmin=lutables[ilu].m_xdat[0];
          double xmax=lutables[ilu].m_xdat[ilast];
          double ymin=lutables[ilu].m_ydat[0];
          double ymax=lutables[ilu].m_ydat[ilast];
          AH_INFO(ahlog::LOW) << "   " << ilu << "         " << ahlog::setprecision(15) 
                              << xmin << " : " << xmax << "        "<< ymin << " : " << ymax << std::endl;
        }
  
        // clean up temporary arrays
        delete [] tmp_ltime;
        delete [] tmp_l32ti;
        delete [] tmp_pspid;
      }
    }

    // Define constants for SXI time-assignment operation.
    if (calctime) {
      if (optype == "SXI" || optype == "SXI_NOWIN") {
        AH_INFO(ahlog::HIGH) <<  "Define constants needed for fine-TI calculation for SXI" << std::endl;
        // timask: mask to keep only leading 8 binary digits of L32TI
        // seqfac: conversion factor for SEQ_START_TIME into L32TI units
        sxipars.m_timask=0xFF000000;
        sxipars.m_seqfac=pow(0.5,8);
        AH_INFO(ahlog::HIGH) << "  fine L32TI = L32TI & timask + seqfac * SEQ_START_TIME" << std::endl;
        AH_INFO(ahlog::HIGH) << "  where timask, seqfac = " << sxipars.m_timask << ", " << sxipars.m_seqfac << std::endl;
    
        // The SXI instrument operates in fixed-length time cycles given by the
        // keyword, NOMEXPO (i.e. 4s).  The total cycle is further divided into
        // subcycles based on the DATAMODE keyword.  A subcycle is divided in the
        // following way:
        //
        // [---EXPDEADB---|---TIMTRANB----|---FLUSHIMB----|====TIMEDEL====|---TIMTRANA----|---EXPDEADA---]
        //
        // where events are only recorded during the TIMEDEL interval.  While the
        // number of sub-cycles depends on the DATAMODE keyword, it can also be
        // calculated by dividing the total cycle time (NOMEXPO) by the subcycle
        // time illustrated above.  The time assigned to events is within the
        // TIMEDEL interval based on the TIMEPIXR keyword.  For TIMEPIXR=0.5, the
        // time is assigned at the halfway point in the interval as shown below:
        // 
        // [---EXPDEADB---|---TIMTRANB----|---FLUSHIMB----|====TIMEDEL====|---TIMTRANA----|---EXPDEADA---]
        // |----------------SUBCYCLE TIME SHIFT-------------------->
        //
        // The total time shift from the beginning of SEQ_START_TIME is the subcycle
        // time shift shown above plus the total time of all subcycles prior to the
        // active subcycle:
        //
        //  time shift = SUBCYCLE_SHIFT + (icycle * SUBCYCLE_TIME)
        //
        // where SUBCYCLE_SHIFT = EXPDEADB+TIMTRANB+FLUSHIMB+(TIMEPIXR*TIMEDEL), 
        //       SUBCYCLE_TIME = EXPDEADB+TIMTRANB+FLUSHIMB+TIMEDEL+TIMTRANA+EXPDEADA,
        // and icycle ranges from 0 to NEXP-1, where NEXP is the number of
        // nexposures in one complete cycle (e.g. 4s).
        //
        // The SUBCYCLE_SHIFT is constant for all subcycles except the last where
        // TIMEDEL and EXPDEADA can be extended.  For time assignment, the adjusted
        // EXPDEADA can be ignored since it occurs after the exposure window, but
        // an additional shift is needed based on the final TIMEDEL size:
        //
        //  ADJLASTTIME = TIMEPIXR * (LASTDEL - TIMEDEL)
        //
        // In the last subcycle, this value must be added to the time shift.
        //
    
        if (optype == "SXI") {      // this stuff is not needed for SXI_NOWIN
          // read timing constants from header
          double timedel=ahfits::getKeyValDbl(fptr,"TIMEDEL");
          double timtranb=ahfits::getKeyValDbl(fptr,"TIMTRANB");
          double expdeadb=ahfits::getKeyValDbl(fptr,"EXPDEADB");
          double timtrana=ahfits::getKeyValDbl(fptr,"TIMTRANA");
          double expdeada=ahfits::getKeyValDbl(fptr,"EXPDEADA");
          double flushimb=ahfits::getKeyValDbl(fptr,"FLUSHIMB");
          double lastdead=ahfits::getKeyValDbl(fptr,"LASTDEAD");
          double lastdel=ahfits::getKeyValDbl(fptr,"LASTDEL");
          double nomexpo=ahfits::getKeyValDbl(fptr,"NOMEXPO");
          double timepixr=ahfits::getKeyValDbl(fptr,"TIMEPIXR");
          long ccdsize=ahfits::getKeyValLLong(fptr,"CCDSIZE");      // max RAWY value y: [0 - CCDSIZE]
  
          // for calculating number of exposures in SXI time frame
          double bintol=0.001;   // tolerance for detecting if number is close-enough to integer (floating imprecision)
          double extratime=0.;   // extra size of last exposure frame
          double nbins_dbl=0.;   // number of bins as a double
          int nbins=0;           // ... rounded to an integer
      
          // for time assignment, the above keyword values can be reduced to the 
          // following:
          //  - subcycshift: time from start of the subcycle to TIME reference
          //  - subcyctime: total time of a single subcycle
          //  - adjlasttime: additional shift if in last subcycle
          //  - nexp: number of exposures in cycle (assigned below)
          //  - dely: number of Y pixels for each exposure (assigned below)
          sxipars.m_subcycleshift=expdeadb+timtranb+flushimb+timepixr*timedel;
          sxipars.m_subcycletime=expdeadb+timtranb+flushimb+timedel+expdeada+timtrana;
          sxipars.m_adjlasttime=timepixr*(lastdel-timedel);
  
          // determine number of exposures in nominal time frame (this will be 
          // constant within a file)
          // note: rounding nbins and then check if close enough to integer
          if (lastdel > 0.) 
            extratime=(lastdead-expdeada)+(lastdel-timedel);
          else
            extratime=0.;
          nbins_dbl=(nomexpo-extratime)/sxipars.m_subcycletime;
          nbins=(int)(nbins_dbl+0.5);         // +0.5 for rounding
          if (std::abs(nbins_dbl-(double)nbins) > bintol) {
            AH_INFO(ahlog::HIGH) << " *** did not find an integral number of bins within tolerance" << std::endl;
            AH_INFO(ahlog::HIGH) << " -- Num bins:  " << nbins_dbl << std::endl;
            AH_INFO(ahlog::HIGH) << " -- Tolerance: " << bintol << std::endl;
          }
      
          // check that number of bins divides into RAWY 
          int dely=ccdsize/nbins;   // should divide equally
          if (dely*nbins != ccdsize) {
            AH_INFO(ahlog::LOW) << " *** SXI operation: number of exposure bins does not divide equally into RAWY size" << std::endl;
            AH_INFO(ahlog::LOW) << " -- num bins:      " << nbins << std::endl;
            AH_INFO(ahlog::LOW) << " -- CCD size:      " << ccdsize << std::endl;
            AH_INFO(ahlog::LOW) << " -- num RAWY bins: " << dely << std::endl;
          }
      
          // number of Y pixels corresponding to single exposure
          sxipars.m_nexp=nbins;
          sxipars.m_dely=dely;
  
          AH_INFO(ahlog::HIGH) << "SXI windowing constants:" << std::endl;
          AH_INFO(ahlog::HIGH) << ahlog::setprecision(15) << "  subcycleshift: " << sxipars.m_subcycleshift << std::endl;
          AH_INFO(ahlog::HIGH) << ahlog::setprecision(15) << "  subcycletime:  " << sxipars.m_subcycletime << std::endl;
          AH_INFO(ahlog::HIGH) << ahlog::setprecision(15) << "  adjlasttime:   " << sxipars.m_adjlasttime << std::endl;
          AH_INFO(ahlog::HIGH) << "  nexp:          " << sxipars.m_nexp << std::endl;
          AH_INFO(ahlog::HIGH) << ahlog::setprecision(15) << "  dely:          " << sxipars.m_dely << std::endl;
        }
      }
    }

    // Make connections to columns given in the column definition CALDB
    // file.  If the column is a non-empty string, make the connection.
    // For SXS_GTILOST files, need to connect to two different SampleCnt
    // columns; the names of these columns are derived from the LTIME1EVT
    // entry in the coldef file with a '1' or '2' appended.
    ahfits::Router router(fptr);
    std::vector<std::string> colread;     // list of columns being read
    std::vector<std::string> colwrite;    // list of columns being written
    std::vector<std::string> coladd;      // list of columns added
    if (ahgen::strtoupper(timecol) != "TIME") {    // add output time column if not "TIME"
      if (!ahfits::haveColumn(fptr,timecol)) {
        ahfits::insertColAfter(fptr,timecol,"D","TIME");
        ahfits::setColumnDescription(fptr,timecol,"ahtime");
        coladd.push_back(timecol);
      }
    }
    router.connectScalar(ahfits::e_READWRITE,timecol, rowdat.m_time, &rowdat.m_timenull);
    colwrite.push_back(timecol);
    if (calctime) {
      router.connectScalar(ahfits::e_READONLY,colnames.m_tisc,rowdat.m_tisc);
      router.connectScalar(ahfits::e_READONLY,colnames.m_sendtime,rowdat.m_sendtime);
      colread.push_back(colnames.m_tisc);
      colread.push_back(colnames.m_sendtime);

      // GTI extensions and SXS Lost GTI files nominally fill START and STOP
      // columns.  If user specifies different names, then insert those columns.
      if (optype == "GTI" || isinstgti) {
        if (startcol != "START") {
          if (!ahfits::haveColumn(fptr,startcol)) {
            ahfits::insertColAfter(fptr,startcol,"D","START");    // inserting column after START (which must be present)
            ahfits::setColumnDescription(fptr,startcol,"ahtime");
            coladd.push_back(startcol);
          }
        }
        if (startcol != "STOP") {   // add output stop column in not "STOP"
          if (!ahfits::haveColumn(fptr,stopcol)) {
            ahfits::insertColAfter(fptr,stopcol,"D","STOP");    // inserting column after STOP (which must be present)
            ahfits::setColumnDescription(fptr,stopcol,"ahtime");
            coladd.push_back(stopcol);
          }
        }
      }

      // Expect to have S_TIMESP and L32TISP columns for GTI extensions 
      if (optype == "GTI") {
        router.connectScalar(ahfits::e_WRITEONLY,startcol, rowdat.m_start, &rowdat.m_startnull);
        router.connectScalar(ahfits::e_WRITEONLY,stopcol, rowdat.m_stop, &rowdat.m_stopnull);
        router.connectScalar(ahfits::e_READONLY,colnames.m_tisc+"SP",rowdat.m_tiscsp);
        router.connectScalar(ahfits::e_READONLY,colnames.m_sendtime+"SP",rowdat.m_sendtimesp);
        colwrite.push_back(startcol);
        colwrite.push_back(stopcol);
        colread.push_back(colnames.m_tisc);
        colread.push_back(colnames.m_sendtime);
      }

      // Instrument GTI extensions have two local times
      if (isinstgti) {
        if (colnames.m_ltime1evt == "NA") {
          AH_INFO(ahlog::HIGH) << "Received instrument GTI with no local time" << std::endl;
          AH_INFO(ahlog::HIGH) << "  EXTNAME:   " << key_extname << std::endl;
          AH_INFO(ahlog::HIGH) << "  INSTRUME:  " << key_instrume << std::endl;
          AH_INFO(ahlog::HIGH) << "  DETNAM:    " << key_detnam << std::endl;
          AH_INFO(ahlog::HIGH) << "  DATAMODE:  " << key_datamode << std::endl;
          AH_INFO(ahlog::HIGH) << "  sysname:   " << sysname << std::endl;
          AH_INFO(ahlog::HIGH) << "  optype:    " << optype << std::endl;
          AH_INFO(ahlog::HIGH) << "  delayname: " << delayname << std::endl; 
          AH_THROW_RUNTIME("Received instrument GTI with no local time... stopping");
        }

        // make connections to both local times and START/STOP
        router.connectScalar(ahfits::e_WRITEONLY,startcol, rowdat.m_start, &rowdat.m_startnull);
        router.connectScalar(ahfits::e_WRITEONLY,stopcol, rowdat.m_stop, &rowdat.m_stopnull);
        router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt+"1",rowdat.m_ltime1evt,&rowdat.m_ltime1evtnull);
        router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt+"2",rowdat.m_ltime1evt_2,&rowdat.m_ltime1evt_2null);
        colwrite.push_back(startcol);
        colwrite.push_back(stopcol);
        colread.push_back(colnames.m_ltime1evt+"1");
        colread.push_back(colnames.m_ltime1evt+"2");

      // SXS event files need to fill two output columns: TIME & TIMETRIG using
      // two different local times: SAMPLECNT & SAMPLECNTTRIG
      } else if (optype == "SXS_TRIG") {
        std::string timetrigcol="TRIG"+timecol;
        std::string samcnttrigcol=colnames.m_ltime1evt+"TRIG";
        if (!ahfits::haveColumn(fptr,timetrigcol)) {
          ahfits::insertColAfter(fptr,timetrigcol,"D",timecol);
          ahfits::setColumnDescription(fptr,timetrigcol,"ahtime");
          coladd.push_back(timetrigcol);
        }
        router.connectScalar(ahfits::e_WRITEONLY,timetrigcol,rowdat.m_timetrig,&rowdat.m_timetrignull);
        router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt,rowdat.m_ltime1evt,&rowdat.m_ltime1evtnull);        // used for TIME
        router.connectScalar(ahfits::e_READONLY,samcnttrigcol,rowdat.m_ltime1evt_2,&rowdat.m_ltime1evt_2null);            // used for TIMETRIG
        colwrite.push_back(timetrigcol);
        colread.push_back(colnames.m_ltime1evt);
        colread.push_back(samcnttrigcol);

      // SXS event files need to use the double-type version of the local time column
      } else if (optype == "SXS") {
        router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt,rowdat.m_ltime1evt,&rowdat.m_ltime1evtnull);
        colread.push_back(colnames.m_ltime1evt);

      // All other file types need only connect to one local time column
      } else {
        if (colnames.m_ltime1evt != "NA") {
          long long tmpnullval=0;     // not used but needed when checking if column has TNULL defined
          if (ahfits::columnNull(fptr,colnames.m_ltime1evt,tmpnullval)) {    // only connect null value if TNULL defined
            router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt,rowdat.m_ltime1evt,&rowdat.m_ltime1evtnull);
            colread.push_back(colnames.m_ltime1evt);
          } else {
            router.connectScalar(ahfits::e_READONLY,colnames.m_ltime1evt,rowdat.m_ltime1evt);
            colread.push_back(colnames.m_ltime1evt);
          }
        }
      }
      if (colnames.m_ltime2evt != "NA") {
        router.connectScalar(ahfits::e_READONLY,colnames.m_ltime2evt,rowdat.m_ltime2evt);
        colread.push_back(colnames.m_ltime2evt);
      }
      if (optype == "SXI") {     // SXI also needs RAWY column
        router.connectScalar(ahfits::e_READONLY,"RAWY",rowdat.m_rawy);
        colread.push_back("RAWY");
      }
    }
    if (calcutc && optype == "HK") {       // calcutc only true if HK file
      router.connectScalar(ahfits::e_WRITEONLY,"YYYY",rowdat.m_yyyy,&rowdat.m_yyyynull);
      router.connectScalar(ahfits::e_WRITEONLY,"DDD",rowdat.m_ddd,&rowdat.m_dddnull);
      router.connectScalar(ahfits::e_WRITEONLY,"HH",rowdat.m_hh,&rowdat.m_hhnull);
      router.connectScalar(ahfits::e_WRITEONLY,"MM",rowdat.m_mm,&rowdat.m_mmnull);
      router.connectScalar(ahfits::e_WRITEONLY,"SS",rowdat.m_ss,&rowdat.m_ssnull);
      router.connectScalar(ahfits::e_WRITEONLY,"US",rowdat.m_us,&rowdat.m_usnull);
      colwrite.push_back("YYYY");
      colwrite.push_back("DDD");
      colwrite.push_back("HH");
      colwrite.push_back("MM");
      colwrite.push_back("SS");
      colwrite.push_back("US");
    }
    router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",rowdat.m_proc_status,
                      rowdat.num_proc_status);
    colread.push_back("PROC_STATUS");

    // write list of columns added/read/written to log file
    std::stringstream sscoladd;
    sscoladd <<   "Columns added:   ";
    for (std::vector<std::string>::iterator astr=coladd.begin(); astr != coladd.end(); astr++) 
      sscoladd << *astr << "  ";      
    AH_INFO(ahlog::HIGH) << sscoladd.str() << std::endl;
    std::stringstream sscolread;
    sscolread <<  "Columns read:    ";
    for (std::vector<std::string>::iterator astr=colread.begin(); astr != colread.end(); astr++) 
      sscolread << *astr << "  ";
    AH_INFO(ahlog::HIGH) << sscolread.str() << std::endl;
    std::stringstream sscolwrite;
    sscolwrite << "Columns written: ";
    for (std::vector<std::string>::iterator astr=colwrite.begin(); astr != colwrite.end(); astr++) 
      sscolwrite << *astr << "  ";
    AH_INFO(ahlog::HIGH) << sscolwrite.str() << std::endl;

    // loop over rows in FITS file
    long long irow=0;
    for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
      ahfits::readRow(fptr);
      irow++;
      AH_DEBUG << "Row number: irow = " << irow << std::endl;
      AH_DEBUG << "  L32TI, S_TIME = " << ahlog::setprecision(15) << rowdat.m_tisc << ", " << rowdat.m_sendtime << std::endl;

      // Reset output NULL flags
      if (calctime) rowdat.m_timenull=0;     // do not want to reset if only calculating UTC columns (from TIME column)
      rowdat.m_startnull=0;
      rowdat.m_stopnull=0;
      rowdat.m_yyyynull=0;
      rowdat.m_dddnull=0;
      rowdat.m_hhnull=0;
      rowdat.m_mmnull=0;
      rowdat.m_ssnull=0;
      rowdat.m_usnull=0;

      // If calculating TIME, need to reset Astro-H PROC_STATUS bits
      // +++ 2015-08-20 MCW need to make a function to reset these bits
      if (calctime) {
        for (int iii=16; iii < 32; iii++) rowdat.m_proc_status[iii]=0;
      }

      // If bad PROC_STATUS, write NULL
      // Note: no if-statements for calcutc or calctime are needed since
      // connections are only made to the appropriate columns.
      if (!procstatus::processRow(rowdat.m_proc_status)) {
        AH_DEBUG << "  row " << irow << ": Bad PROC_STATUS; setting TIME=NULL" << std::endl;
        count_ext.num_badprocstatus++;
        count_ext.num_timenull++;
        rowdat.m_timenull=1;
        rowdat.m_startnull=1;
        rowdat.m_stopnull=1;
        rowdat.m_yyyynull=1;
        rowdat.m_dddnull=1;
        rowdat.m_hhnull=1;
        rowdat.m_mmnull=1;
        rowdat.m_ssnull=1;
        rowdat.m_usnull=1;
        ahfits::writeRow(fptr);
        continue;      // process next row
      }

      if (calctime) {

        // ============ START FINE-TI CALCULATION ==============================

        // Compute default fine-TI from L32TI value; this is valid for HK, CAMS,
        // HXI/SGD scalar, and SXS Template
        fineti=(double)rowdat.m_tisc;
        fineti2=fineti;

        // For SXI, fine-TI is calculated by combining the leading bits
        // of L32TI with SEQ_START_TIME.
        if (optype == "SXI" || optype == "SXI_NOWIN") {

          // compute fine-TI for TIME (and START, if GTI file)
          calcFineTI_SXI(irow,optype,sxipars,rowdat.m_tisc,rowdat.m_sendtime,
                         rowdat.m_rawy,rowdat.m_ltime1evt,rowdat.m_ltime1evtnull,
                         rowdat.m_timenull,fineti,count_ext);

          // compute fine-TI for STOP, if GTI file
          if (isinstgti) {
            calcFineTI_SXI(irow,optype,sxipars,rowdat.m_tisc,rowdat.m_sendtime,
                           rowdat.m_rawy,rowdat.m_ltime1evt_2,rowdat.m_ltime1evt_2null,
                           rowdat.m_stopnull,fineti2,count_ext);

            // if both START/STOP are NULL, then remove double-counting of row
            if (rowdat.m_timenull ==1 && rowdat.m_stopnull == 1) count_ext.num_timenull--;
          }
        }

        // To get fine-TI for HXI/SGD, use the single lookup table.
        if (optype == "HXI1" || optype == "HXI2" || 
            optype == "SGD1" || optype == "SGD2") {

          calcFineTI_Lookup(irow,rowdat.m_ltime1evt,rowdat.m_ltime1evtnull,
                            rowdat.m_timenull,lutables[0],interp,fineti,
                            count_ext);

          if (isinstgti) {
            calcFineTI_Lookup(irow,rowdat.m_ltime1evt_2,rowdat.m_ltime1evt_2null,
                              rowdat.m_stopnull,lutables[0],interp,fineti2,
                              count_ext);

            // if both START/STOP are NULL, then remove double-counting of row
            if (rowdat.m_timenull ==1 && rowdat.m_stopnull == 1) count_ext.num_timenull--;
          }
        }

        // Fine TI calculation for SXS
        if (optype == "SXS" || optype == "SXS_TRIG" || optype == "SXS_GTILOST") {

          // get PSP_ID for selecting which lookup table to use
          int pspid=rowdat.m_ltime2evt;

          calcFineTI_Lookup(irow,rowdat.m_ltime1evt,rowdat.m_ltime1evtnull,
                            rowdat.m_timenull,lutables[pspid],interp,fineti,
                            count_ext);

          if (isinstgti || optype == "SXS_TRIG") {
            calcFineTI_Lookup(irow,rowdat.m_ltime1evt_2,rowdat.m_ltime1evt_2null,
                              rowdat.m_stopnull,lutables[pspid],interp,fineti2,
                              count_ext);

            // if both START/STOP are NULL, then remove double-counting of row
            if (rowdat.m_timenull == 1) rowdat.m_startnull=1;
            if (rowdat.m_timenull == 1 && rowdat.m_stopnull == 1) count_ext.num_timenull--;
          }
        }      // end if for SXS fine-TI calculations

        AH_DEBUG << "  Fine TI = " << ahlog::setprecision(15) << fineti << std::endl;

        // ============ START TIME CALCULATION ==============================

        if (rowdat.m_timenull == 0) {
          // check that current S_TIME (rowdat.m_sendtime) is larger than current 
          // position in TIM file; if not, then set PROC_STATUS and reset TIM 
          // search (timidx=0)
          if (rowdat.m_sendtime < timdat.m_time1[timidx]) {
            AH_DEBUG << "Row: " << irow << " S_TIME smaller than current TIM TIME; set out-of-order flag in PROC_STATUS and reset TIM index" << std::endl;
            AH_DEBUG << "  event S_TIME, TIM TIME (tim row) = " << rowdat.m_sendtime << ", " << timdat.m_time1[timidx] << " (" << timidx << ")" << std::endl;
            setProcStatusOutOfOrder(rowdat.m_proc_status);
            timidx=0;
          }

          // perform TIM lookup
          extrap=false;
          badtim=false;
          try {
            rowdat.m_time=lookupTIM(timdat,rowdat.m_sendtime,fineti,l32timax,
                                    interp,timidx,extrap,badtim,rowdat.m_proc_status);
          } catch (std::exception& e) {
            std::stringstream msg;
            msg << "Error in TIM lookup for row " << irow << ": " << e.what();
            AH_THROW_RUNTIME(msg.str());
          }
          if (extrap) {
            count_ext.num_extrap++;
            AH_DEBUG << "  *** row " << irow << "; extrapolating to get TIME from TIM file" << std::endl;
          }
          if (badtim) {
            setProcStatusGSFCBad(rowdat.m_proc_status);
            rowdat.m_timenull=1;
            count_ext.num_timenull++;
            AH_DEBUG << "  row " << irow << "; TIME assignment failed, setting TIME=NULL" << std::endl;
          } else {
            AH_DEBUG << "  TIM lookup okay; interpolated TIME = " << ahlog::setprecision(15) << rowdat.m_time << std::endl;
          }

          // Need to compute STOP time for GTI extensions; only compute 2nd
          // time if TIME (from above) is not NULL
          if (optype == "GTI") {
            if (rowdat.m_timenull == 1) {
              AH_DEBUG << "  TIME=NULL so skipping GTI STOP calculation" << std::endl;
              rowdat.m_startnull=1;
              rowdat.m_stopnull=1;
            } else {
              rowdat.m_startnull=0;
              rowdat.m_stopnull=0;
              rowdat.m_start=rowdat.m_time;   // START=TIME

              // calculate STOP time
              fineti2=(double)rowdat.m_tiscsp;
              unsigned long timidx_tmp=timidx;   // do not want to overwrite first search position
              extrap=false;
              badtim=false;
              rowdat.m_stop=lookupTIM(timdat,rowdat.m_sendtimesp,fineti2,l32timax,
                                      interp,timidx_tmp,extrap,badtim,0);        // 0 => do not fill PROC_STATUS
              if (extrap) {
                AH_DEBUG << "  *** row " << irow << "; extrapolating to get GTI STOP from TIM file" << std::endl;
              }
              if (badtim) {
                rowdat.m_stopnull=1;
                AH_DEBUG << "  row " << irow << "; TIME assignment failed, setting GTI STOP=NULL" << std::endl;
              } else {
                AH_DEBUG << "  TIM lookup okay; interpolated START, STOP = " << ahlog::setprecision(15) << rowdat.m_start << ", " << rowdat.m_stop << std::endl;
              }
            }
          }
  
          // Need to compute STOP time for instrument GTI files; only compute
          // 2nd time if TIME (from above) is not NULL
          if (isinstgti) {
            if (rowdat.m_timenull == 1) {
              AH_DEBUG << "  TIME=NULL so skipping SXS GTILOST STOP calculation" << std::endl;
              rowdat.m_startnull=1;
              rowdat.m_stopnull=1;
            } else {
              rowdat.m_startnull=0;
              rowdat.m_start=rowdat.m_time;   // START=TIME

              if (rowdat.m_stopnull == 0) {      // only want to compute STOP if fineti2 calculation was okay

                // calculate STOP time
                unsigned long timidx_tmp=timidx;   // do not want to overwrite first search position
                extrap=false;
                badtim=false;
                rowdat.m_stop=lookupTIM(timdat,rowdat.m_sendtime,fineti2,l32timax,
                                        interp,timidx_tmp,extrap,badtim,0);        // 0 => do not fill PROC_STATUS
                if (extrap) {
                  AH_DEBUG << "  *** row " << irow << "; extrapolating to get SXS GTILOST STOP time" << std::endl;
                }
                if (badtim) {
                  rowdat.m_stopnull=1;
                  AH_DEBUG << "  row " << irow << "; TIME assignment failed, setting SXS GTILOST STOP=NULL" << std::endl;
                } else {
                  AH_DEBUG << "  TIM lookup okay; interpolated SXS GTI LOST START, STOP = " << ahlog::setprecision(15) << rowdat.m_start << ", " << rowdat.m_stop << std::endl;
                }
              }
            }
          }    // end if instrument GTI 

          // Need to compute TIMETRIG for SXS_TRIG; only compute 2nd time
          // if TIME (from above) is not NULL
          if (optype == "SXS_TRIG") {

            // The fine-TI for TIMETRIG was computed at the same time as the
            // second local times for instrument GTIs.  For convenience, 
            // stoptimenull was used to record a failure in the calculation.
            rowdat.m_timetrignull=rowdat.m_stopnull;

            if (rowdat.m_timenull == 1) {
              AH_DEBUG << "  TIME=NULL so skipping SXS TIMETRIG calculation" << std::endl;
              rowdat.m_timetrignull=1;
            } else if (rowdat.m_timetrignull == 0) {      // only want to compute TIMETRIG if fineti2 calculation was okay

              // calculate TIMETRIG time
              unsigned long timidx_tmp=timidx;   // do not want to overwrite first search position
              extrap=false;
              badtim=false;
              rowdat.m_timetrig=lookupTIM(timdat,rowdat.m_sendtime,fineti2,l32timax,
                                          interp,timidx_tmp,extrap,badtim,0);        // 0 => do not fill PROC_STATUS
              if (extrap) {
                AH_DEBUG << "  *** row " << irow << "; extrapolating to get SXS TIMETRIG" << std::endl;
              }
              if (badtim) {
                rowdat.m_timetrignull=1;
                AH_DEBUG << "  row " << irow << "; TIME assignment failed, setting SXS TIMETRIG=NULL" << std::endl;
              } else {
                AH_DEBUG << "  TIM lookup okay; interpolated SXS TIMETRIG = " << ahlog::setprecision(15) << rowdat.m_timetrig << std::endl;
              }
            }
          }    // end if SXS_TRIG


        }     // end if TIME != NULL


        // ============ START DELAY CALCULATION ==============================

        // adjust TIME by instrument delay or CAMS TIME_CODE
        if (rowdat.m_timenull == 0) {      // only if TIME could be assigned above

          if (optype == "CAMS1" || optype == "CAMS2") {
             // time delay depends on CAMS1 or CAMS2
             double delay=delaytime1;
             if (optype == "CAMS2") delay=delaytime2;

             // adjust according to TIME_CODE
             double camsoffset=ahmission::camstoffset::get_offset(camsoffdat,rowdat.m_ltime1evt);
             double camsperiod=ahmission::camstoffset::get_period(camsoffdat);
             rowdat.m_time+=camsoffset-camsperiod+delay;

             AH_DEBUG << "  adjust CAMS TIME with offset-period+delay = " << camsoffset << " - " << camsperiod << " + " << delay << std::endl;
             AH_DEBUG << "                                            = " << ahlog::setprecision(15) << (camsoffset-camsperiod+delay) << std::endl;

           } else if (optype == "SXI") {
             rowdat.m_time+=delaytime1;
             if (isinstgti) addDelayToGTI(rowdat,delaytime1);
             AH_DEBUG << "  adjust SXI TIME with delay = " << delaytime1 << std::endl;

           } else if (optype == "HXI1" || optype == "SGD1" || 
                      optype == "HXI1_SCALAR" || optype == "SGD1_SCALAR") {
             rowdat.m_time+=delaytime1;
             if (isinstgti) addDelayToGTI(rowdat,delaytime1);
             AH_DEBUG << "  adjust HXI/SGD TIME with delay = " << delaytime1 << std::endl;

           } else if (optype == "HXI2" || optype == "SGD2" || 
                      optype == "HXI2_SCALAR" || optype == "SGD2_SCALAR") {
             rowdat.m_time+=delaytime2;
             if (isinstgti) addDelayToGTI(rowdat,delaytime2);
             AH_DEBUG << "  adjust HXI/SGD TIME with delay = " << delaytime2 << std::endl;

           } else if (optype == "SXS") {
             double delay=delaytime1;                        // PSP_ID = 0 or 1
             if (rowdat.m_ltime2evt > 1) delay=delaytime2;   // PSP_ID = 2 or 3
             rowdat.m_time+=delay;
             if (isinstgti) addDelayToGTI(rowdat,delay);
             AH_DEBUG << "  adjust SXS TIME with delay = " << delay << std::endl;

           } else if (optype == "SXS_TRIG") {
             double delay=delaytime1;                        // PSP_ID = 0 or 1
             if (rowdat.m_ltime2evt > 1) delay=delaytime2;   // PSP_ID = 2 or 3
             rowdat.m_time+=delay;
             rowdat.m_timetrig+=delay;
             AH_DEBUG << "  adjust SXS TIME & TIMETRIG with delay = " << delay << std::endl;

           } else if (optype == "SXS_GTILOST") {
             double delay=delaytime1;                        // PSP_ID = 0 or 1
             if (rowdat.m_ltime2evt > 1) delay=delaytime2;   // PSP_ID = 2 or 3
             rowdat.m_time+=delay;
             if (isinstgti) addDelayToGTI(rowdat,delay);
             AH_DEBUG << "  adjust SXS GTI LOST TIME/START/STOP with delay = " << delay << std::endl;

           } else if (optype == "MXS") {
             rowdat.m_time+=delaytime1;
             AH_DEBUG << "  adjust MXW TIME with delay = " << delaytime1 << std::endl;
           }

           AH_DEBUG << "  Time calculation finished; final TIME = " << ahlog::setprecision(15) << rowdat.m_time << std::endl;

        }    // end if (timenull == 0)

        // store the first time written to header
        if (time0 < 0. && rowdat.m_timenull == 0) time0=rowdat.m_time;

      }   // end if(calctime)

      // calculate UTC
      // +++ 2013-03-18 MCW how to calculate if skip_adj = true? (not if TRF)
      if (calcutc && optype == "HK") {

        if (rowdat.m_timenull == 0) {
          ahtime::AhDateTime tutc;   // dummy var to hold UTC time
          ahtime::convertMissionTimeToUTC(rowdat.m_time,epoch,leapsecdat,tutc);
          rowdat.m_yyyy=tutc.year();
          rowdat.m_ddd=tutc.daysInYear();
          rowdat.m_hh=tutc.hour();
          rowdat.m_mm=tutc.minute();
          rowdat.m_ss=tutc.second();
          rowdat.m_us=tutc.getSubsecondAsInt(6);   // number of microseconds
          AH_DEBUG << "  YYYY = " << rowdat.m_yyyy << std::endl;
          AH_DEBUG << "   DDD = " << rowdat.m_ddd << std::endl;
          AH_DEBUG << "    HH = " << rowdat.m_hh << std::endl;
          AH_DEBUG << "    MM = " << rowdat.m_mm << std::endl;
          AH_DEBUG << "    SS = " << rowdat.m_ss << std::endl;
          AH_DEBUG << "    US = " << rowdat.m_us << std::endl;
        } else {
          rowdat.m_yyyynull=1;
          rowdat.m_dddnull=1;
          rowdat.m_hhnull=1;
          rowdat.m_mmnull=1;
          rowdat.m_ssnull=1;
          rowdat.m_usnull=1;
          AH_DEBUG << "  TIME=NULL; setting YYYY,DDD,HH,MM,SS,US to NULL" << std::endl;
        }
      }

      // write TIME and UTC
      if (rowdat.m_timenull == 0) time1=rowdat.m_time;   // keep last non-NULL TIME for TSTOP keyword
      ahfits::writeRow(fptr);

    }   // end loop over rows

    // Correct header times
    if (calctime && writekeys) {
      if (time0 >= 0.) {    // only if time0 has been set
        tutcstr=ahtime::calcDateObs(time0,epoch,leapsecdat);
        ahfits::writeKeyValDbl(fptr,"TSTART",time0,"Start time");
        ahfits::writeKeyValStr(fptr,"DATE-OBS",tutcstr,"Start Date");
        AH_INFO(ahlog::HIGH) << "TSTART, DATE-OBS = " << time0 << ", " << tutcstr << std::endl;
      } else {
        AH_INFO(ahlog::HIGH) << "No valid TIMEs computed; TSTART & DATE-OBS keywords not set" << std::endl;
      }

      if (time1 >= 0.) {    // only if time1 has been set
        tutcstr=ahtime::calcDateObs(time1,epoch,leapsecdat);
        ahfits::writeKeyValDbl(fptr,"TSTOP",time1,"Stop time");
        ahfits::writeKeyValStr(fptr,"DATE-END",tutcstr,"Stop Date");
        AH_INFO(ahlog::HIGH) << "TSTOP, DATE-END =  " << time1 << ", " << tutcstr << std::endl;
      } else {
        AH_INFO(ahlog::HIGH) << "No valid TIMEs computed; TSTOP & DATE-END keywords not set" << std::endl;
      }
    }

    // update primary tstart/tstop keyword values
    if (pri_tstart < 0.) {
      pri_tstart=time0;
    } else {
      if (time0 < pri_tstart) pri_tstart=time0;
    }
    if (pri_tstop < 0.) {
      pri_tstop=time1;
    } else {
      if (time1 > pri_tstop) pri_tstop=time1;
    }

    // restore parameter stamping state
    ahfits::setParameterStamping(fptr,parstampstate);

    count_ext.num_rows=irow;
    AH_INFO(ahlog::HIGH) << "Finished processing extension" << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of rows:                      " << count_ext.num_rows << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of extrapolated TIMEs:        " << count_ext.num_extrap << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of rows with bad PROC_STATUS: " << count_ext.num_badprocstatus << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of rows with TIME = NULL:     " << count_ext.num_timenull << std::endl;

    // add current extension counters to total
    count_tot.num_rows+=count_ext.num_rows;
    count_tot.num_extrap+=count_ext.num_extrap;
    count_tot.num_badprocstatus+=count_ext.num_badprocstatus;
    count_tot.num_timenull+=count_ext.num_timenull;

  } while (ahfits::nextHDU(fptr, ahfits::e_BINARY_TBL));

  // write TSTART/TSTOP to primary HDU
  if (writekeys) {
    ahfits::move(fptr,"");    // move to primary HDU
    if (pri_tstart >= 0.) {
      tutcstr=ahtime::calcDateObs(pri_tstart,epoch,leapsecdat);
      ahfits::writeKeyValDbl(fptr,"TSTART",pri_tstart,"Start time");
      ahfits::writeKeyValStr(fptr,"DATE-OBS",tutcstr,"Start Date");
      AH_INFO(ahlog::HIGH) << "Primary TSTART, DATE-OBS = " << time0 << ", " << tutcstr << std::endl;
    } else {
      AH_INFO(ahlog::HIGH) << "No valid TIMEs computed; TSTART & DATE-OBS keywords not set in Primary header" << std::endl;
    }
    if (pri_tstop >= 0.) {
      tutcstr=ahtime::calcDateObs(pri_tstop,epoch,leapsecdat);
      ahfits::writeKeyValDbl(fptr,"TSTOP",pri_tstop,"Stop time");
      ahfits::writeKeyValStr(fptr,"DATE-END",tutcstr,"Stop Date");
      AH_INFO(ahlog::HIGH) << "Primary TSTOP, DATE-END =  " << time1 << ", " << tutcstr << std::endl;
    } else {
      AH_INFO(ahlog::HIGH) << "No valid TIMEs computed; TSTOP & DATE-END keywords not set in Primary header" << std::endl;
    }
  }

  // Write list of parameters to log file
  // Note: we are stamping the parameters at the end of the log file because
  // some CALDB files are only resolved in the HDU loop.
  ahapp::writeParametersToLog(); 

  // Deallocate lookup tables, if necessary
  for (int ilu=0; ilu < 4; ilu++) clearLookupTable(lutables[ilu]);

  AH_INFO(ahlog::HIGH) << "Finished processing file" << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of rows:                      " << count_tot.num_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of extrapolated TIMEs:        " << count_tot.num_extrap << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of rows with bad PROC_STATUS: " << count_tot.num_badprocstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of rows with TIME = NULL:     " << count_tot.num_timenull << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr fptr) {

  ahfits::close(fptr);

}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: ahtime.cxx,v $
 Revision 1.132  2016/04/21 18:24:06  rshill
 Corrected typo.

 Revision 1.131  2016/04/21 18:20:25  rshill
 Added 2 writeParametersToLog() calls; made some string
 tests case insensitive.

 Revision 1.130  2016/04/07 17:43:27  mwitthoe
 ahtime tool: change INFO log messages to DEBUG inside the row loop; add a counter for extrapolated TIMEs

 Revision 1.129  2016/03/28 08:51:30  mwitthoe
 ahtime tool: fix bug where START was not set to NULL for SXS lost GTI files when SampleCnt=NULL

 Revision 1.128  2016/03/18 15:09:58  asargent
 Replaced AH_INFO with AH_WARN

 Revision 1.127  2016/03/02 15:40:13  mwitthoe
 ahtime: fix bug where duplicate SXS lookup table rows were not correctly being ignored

 Revision 1.126  2015/10/27 21:02:18  mwitthoe
 ahtime tool: the DATAMODE values for SXS PIXEL data should be prepended by PX_

 Revision 1.125  2015/10/27 14:06:18  mwitthoe
 ahtime tool: add FITS comments for following keywords: TSTART, TSTOP, DATE-OBS, and DATE-END

 Revision 1.124  2015/10/13 21:07:42  mwitthoe
 ahtime: if TIM lookup fails in the index search, now set TIME=NULL instead of throwing an error

 Revision 1.123  2015/10/02 18:10:30  mwitthoe
 ahtime: switch CALDB queries to use general function

 Revision 1.122  2015/09/25 18:05:22  mwitthoe
 ahtime tool: fix bug where UTC columns were being computed from NULL TIMEs when calctime=no

 Revision 1.121  2015/08/24 01:33:49  mwitthoe
 ahtime: 1) do not load column definitions file, delay file, or lookup tables if calctime is false; 2) re-read column names from column definitions file for each extension so that multiple SGD extensions in the HK file (CC1, CC2, CC3) can be processed correctly

 Revision 1.120  2015/08/20 20:26:36  mwitthoe
 ahtime tool: fix error in log message when resetting TIM search index

 Revision 1.119  2015/08/20 19:19:44  mwitthoe
 ahtime: 1) fix bug in reading lookup tables when there is a row with a bad PROC_STATUS; 2) do not check Astro-H PROC_STATUS bit when reading lookup tables; 3) do not set Astro-H PROC_STATUS bit when S_TIME is out-of-order; reset Astro-H PROC_STATUS bits when performing time assignment

 Revision 1.118  2015/08/19 14:45:17  mwitthoe
 ahtime tool: add support for HXI/SGD HISTOGRAM files; DETNAM for SGDshield files can be SHIELD1 or SHIELD2 (not just SHIELD)

 Revision 1.117  2015/08/11 02:04:55  mwitthoe
 ahtime tool: the HK and event LOCAL_TIME columns for HXI and SGD have different precisions, ahtime now converts the HK LOCAL_TIME into the units of the vent LOCAL_TIME using information in the column definitions CALDB file (new columns added); the conversion of the HK LOCAL_TIME also lowers the maximum allowed value of the event LOCAL_TIME (the height of the sawtooth function is smaller), so the event LOCAL_TIME values also need to be modified by taking the modulus with the new maximum value; these changes have been tested with both HXI and SGD test data from funcd

 Revision 1.116  2015/08/06 14:22:40  mwitthoe
 ahtime: add missing semicolon

 Revision 1.115  2015/08/05 21:01:09  mwitthoe
 ahtime tool: add support for general, instrument GTI

 Revision 1.114  2015/07/31 20:53:12  mwitthoe
 ahtime: fix bug where SXS GTI Lost files were not getting the sysname variable assigned (needed for the coldef lookup)

 Revision 1.113  2015/07/30 16:24:34  mwitthoe
 ahtime: add parameter stamping to log file

 Revision 1.112  2015/07/28 16:43:38  mwitthoe
 ahtime tool: perform tool clean-up; see issues 532, 533, and 534

 Revision 1.111  2015/07/09 16:52:32  mdutka
 Replacing DATE_OBS/DATE_END with DATE-OBS/DATE-END

 Revision 1.110  2015/07/07 17:58:26  mdutka
 adding caldb query to ahtime

 Revision 1.109  2015/07/02 18:30:58  mdutka
 adding CALDB queries for coldef file and offset

 Revision 1.108  2015/06/12 18:01:13  mwitthoe
 ahtime: disable parameter stamping for HK extensions to save eeve time when processing large HK files

 Revision 1.107  2015/06/05 19:04:01  mwitthoe
 ahtime: add support for SXI HOTPIX files (runs using SXI-NOWIN mode); add some AH_INFO and AH_DEBUG statements

 Revision 1.106  2015/06/03 23:06:27  mwitthoe
 ahtime: make compatible with latest coldef CALDB file; do not add delay for HXI/SGD HK files

 Revision 1.105  2015/04/28 18:52:22  mwitthoe
 ahtime: add writekeys parameter; change detection method of MXS files; see issue 515

 Revision 1.104  2015/04/03 17:59:39  mwitthoe
 ahtime: change boolean parameters to use lowercase yes/no; convert coldeffile parameter to uppercase before checking if CALDB

 Revision 1.103  2015/03/23 20:35:31  mwitthoe
 ahtime tool: implement all items for ahtime in issue 496: 1) change TIMETRIG output column to TRIGTIME, 2) add new SXS WFRB file type, 3) SXS Lost GTI files now have an extension named GTILOST, 4) skip extensions with no TIME column, 5) add new time assignment operation for GTI extensions; fix bug where PROC_STATUS was not being filled by the TIM STATUS values

 Revision 1.102  2015/03/18 17:41:41  asargent
 Updated DETNAME to DETNAM

 Revision 1.101  2015/03/12 15:37:46  mwitthoe
 ahtime tool: add check for valid year number after converting MJDREF epoch to UTC (issue 477); fix bug where epoch was only being computed for HK files but used by all file types

 Revision 1.100  2015/01/09 22:49:01  mwitthoe
 ahtime: remove duplicate points from SXS lookup table to avoid divide-by-zero error when performing lookup

 Revision 1.99  2015/01/08 18:43:20  mwitthoe
 ahtime tool: improve some error messages involving lookup tables

 Revision 1.98  2015/01/06 22:28:24  mwitthoe
 ahtime: update parameters; see issue 472

 Revision 1.97  2014/12/24 16:45:25  mwitthoe
 ahtime: hash characters have been introduced to some column names which get replaced by 1,2,or 3 for HXI/SGD; no issue, but changes are in the TRF for 2014-12-22

 Revision 1.96  2014/12/16 21:39:07  mwitthoe
 ahtime: now read SXS event LOCALTIMEs (SampleCnt) as a double instead of a long long in order to preserve precision; no redmine issue

 Revision 1.95  2014/12/05 21:55:00  mwitthoe
 ahtime: bug fix for SXI time assignment in window mode

 Revision 1.94  2014/12/02 21:09:53  mwitthoe
 ahtime tool: fix bugs in SXI time assignment

 Revision 1.93  2014/11/21 18:34:11  mwitthoe
 ahtime: now compute TIMETRIG column for SXS event files; fix bugs in SXI time assignment; see issue 457

 Revision 1.92  2014/11/05 21:37:20  mwitthoe
 ahtime: update task after change to argument list of ahmath::search_sawY2X(); see issue 458

 Revision 1.91  2014/11/04 20:23:54  mwitthoe
 ahtime: add parameter, gticolumns, for specifying the names of t the GTI columns for SXS lost events; insert column given by the timecolumn parameter if the column is not present in the input file; fix bug when reading SXS look-up tables; check for NULL values of localtime when TNULL is defined (if local time is NULL set TIME=NULL); see issue 421

 Revision 1.90  2014/09/10 03:58:17  mwitthoe
 ahtime tool: fix bug related to switchover to new TIM file library

 Revision 1.89  2014/09/10 02:45:52  mwitthoe
 ahtime tool: update tool to reflect new locations of timfile and leapsec CALDB libraries

 Revision 1.88  2014/08/19 17:19:31  mwitthoe
 ahtime: add gticolumns parameter to specify START/STOP columns for SXS lost event files; if timecolumn != TIME and refers to a column which is not present in the output file, then insert the column (similar behavior for gticolumns)

 Revision 1.87  2014/08/05 12:13:29  mwitthoe
 ahtime tool: bug-fix - was using tahfits::getKeyValDbl to read an integer-type keyword instead of ahfits::getKeyValLLong

 Revision 1.86  2014/07/18 17:16:06  mwitthoe
 ahtime tool: write TSTART/TSTOP to primary HDU which are the smallest/largest TSTART/TSTOP values from all HDUs where time assignment is performed

 Revision 1.85  2014/07/08 21:23:23  mwitthoe
 ahtime tool: make changes described in issue 400; primary changes include allowing science data in HK files (instead of assuming event data in a single-extension FITS file) and adding support for SXS_GTI files which have two TIMEs to assign

 Revision 1.84  2014/05/29 21:01:25  mwitthoe
 ahtime tool: fix bug in constructing SXS lookup tables based on PSP_ID column values; was using wrong variable for PSP_ID

 Revision 1.83  2014/05/29 20:58:16  mwitthoe
 ahtime tool: fix bugs: PROC_STATUS bit numbers and use correct lookup table for HXI/SGD fine-TI calculation

 Revision 1.82  2014/01/22 19:04:27  mwitthoe
 ahtime tool: update documentation

 Revision 1.81  2014/01/21 21:47:05  mwitthoe
 ahtime tool: revise according to code review; issue 331

 Revision 1.80  2014/01/14 19:12:50  rshill
 Code review comments

 Revision 1.79  2014/01/13 21:48:42  mwitthoe
 ahtime tool: update documentation; change a couple constant names for the SXI time assignment operation for clarity

 Revision 1.78  2014/01/13 20:49:43  mwitthoe
 ahtime: update SXI time assignment based on latest version of TRF (2013-01-13)

 Revision 1.77  2014/01/13 15:37:04  klrutkow
 code review: comments

 Revision 1.76  2014/01/03 21:14:20  mwitthoe
 ahtime tool: update standard main, see issue 327

 Revision 1.75  2013/12/31 15:27:23  mwitthoe
 ahtime tool: update SXI time assignment to adjust TIME based on windowing mode; these changes are not complete, waiting on final list of required keywords in the TRF

 Revision 1.74  2013/12/05 21:30:33  mwitthoe
 ahtime tool: move time assignment algorithm into 3 functions: calculateFineTI, calculateTimeFromTIMFile, and addTimeDelay in order to reduce code duplication in the new SXI algorithm yet to be implemented; see issue 321

 Revision 1.73  2013/12/05 19:42:31  mwitthoe
 ahtime tool: collect local variables connecting with input file columns into a single structure: EventDat; this is related to issue #321

 Revision 1.71  2013/11/21 16:44:18  mwitthoe
 ahtime tool: free memory allocated in TimeAssignDat structure

 Revision 1.70  2013/11/20 23:12:04  mwitthoe
 ahtime tool: replace code reading the TIM file with new functions in the ahtime library; switch the STATUS column in the TIM file from I-type to 10X-type; move variable declarations to top of scope; rename the timecoldef library to ahtimelib; add tests for ahtimelib

 Revision 1.69  2013/10/07 15:43:32  mwitthoe
 ahtime tool: switch to use new ahfits connect functions (issue 270)

 Revision 1.68  2013/09/17 19:29:34  mwitthoe
 ahtime tool: switch over to using new ahtime library functions (see issue 290)

 Revision 1.67  2013/09/11 19:55:21  mwitthoe
 ahtime tool: switch how HDUs are accessed to use new defintion of empty string to represent the primary HDU instead of the first, non-primary HDU

 Revision 1.66  2013/08/30 20:19:13  mwitthoe
 ahtime tool: switch over to new version of ahmission/delay (issue 236)

 Revision 1.65  2013/08/24 01:35:16  mwitthoe
 fix two bugs in the ahtime tool: 1. tim map array was of unsiged longs was assigned a negative value; 2. when checking PROC_STATUS of TIM file, it was possible to get an array-out-of-bounds error (this was causing the segmentation fault on Tahir's Mac OSX 10.5.8 machine

 Revision 1.64  2013/07/30 18:51:30  mwitthoe
 ahtime: remove unnecessary include statement to ahgen

 Revision 1.63  2013/07/22 20:01:41  mwitthoe
 ahtime: implement PROC_STATUS; now copies bits from STATUS column from TIM file

 Revision 1.62  2013/07/19 14:57:46  mwitthoe
 ahtime: fix bug affecting 32-bit machines; a bit mask of all ones was being converted to a double with a value of -1.0 because I was using an siged long instead of unsigned.

 Revision 1.61  2013/04/19 18:10:21  mwitthoe
 ahtime tool: value of the timask constant needed for SXI EVENT files was not correct; the value did agree with the TRF, which was based on a faulty figure from an old version of SCT 021

 Revision 1.60  2013/04/18 21:02:24  mwitthoe
 use new ahtime::calcDateObs() function in the ahtime tool to calculate DATE-OBS/DATE-END

 Revision 1.59  2013/04/18 19:26:21  mwitthoe
 ahtime tool: fill-in placeholder function for determining if STATUS is legal or illegal

 Revision 1.58  2013/04/16 19:53:42  mwitthoe
 ahtime tool: use sawtooth search function prior to interpolation instead of the non-sawtooth search function (this was a bug)

 Revision 1.57  2013/04/16 19:08:34  mwitthoe
 ahtime tool: add NULL support; improve warning messages when extrapolating

 Revision 1.56  2013/04/12 13:46:22  mwitthoe
 update ahtime tool to use new versions of leap second, delay, and CAMS time offset libraries (now in ahmission)

 Revision 1.55  2013/04/10 21:31:58  mwitthoe
 initialize all ahfits FilePtrs to NULL in the ahtime tool

 Revision 1.54  2013/04/04 21:35:41  mwitthoe
 ahtime tool: linked with local version timecoldef CALDB library; updated ahfits connects to use read/write flags

 Revision 1.53  2013/04/04 21:16:19  mwitthoe
 ahtime tool: update task to match latest TRF (several changes); update test input files to match TRF; update local CALDB files for column definitions and delay times

 Revision 1.52  2013/03/08 21:26:39  mwitthoe
 add todo comment in ahtime source

 Revision 1.51  2013/01/31 16:05:17  mwitthoe
 edit Doxygen in ahtime tool: fixed typo and some keyword descriptions

 Revision 1.50  2013/01/19 18:58:48  mwitthoe
 update ahtime according to 2013-01-17 checklist; too many changes to list here, see checklist

 Revision 1.49  2012/12/17 20:03:44  mwitthoe
 change how CALDB files are temporarily defined in the ahtime tool to allow aht unit tests to work; CALDB files accessed by these parameters do not yet exist in CALDB, so special treatment is needed in the short term

 Revision 1.48  2012/12/10 19:27:22  mwitthoe
 make timing tools up-to-date with recent changes to ahfits

 Revision 1.47  2012/12/07 22:10:14  mwitthoe
 ahtime: after cloning input file, moving to 1st HDU; change name of column definitions file from columndef.fits to timecoldef.fits

 Revision 1.46  2012/12/03 20:28:20  mwitthoe
 in doxygen description of ahtime and ahmktime, change tags surrounding parameter list from code/endcode to verbatim/endverbatim in order to prevent accidentally syntax highlighting

 Revision 1.45  2012/11/28 20:51:03  mwitthoe
 modify ahtime tool to reflect changes in ahmission, namely how HK file are defined

 Revision 1.44  2012/11/26 20:21:17  mwitthoe
 update doxygen description for ahtime tool

 Revision 1.43  2012/11/15 03:01:37  mwitthoe
 change timing tools to use new ahcaldb libraries

 Revision 1.42  2012/11/13 18:34:50  mwitthoe
 change timing tools to use new open/create/clone functions from ahtime

 Revision 1.41  2012/11/07 20:35:02  mwitthoe
 put updated timing test data into ahtime tool directory; change ahtime tool to use these data and check that TSTART/TSTOP ranges between the input and TIM files are compatible

 Revision 1.40  2012/11/04 23:28:04  mwitthoe
 make timing tools (ahtime, ahmktim, ahtrendtemp) consistent with new ahfits clone functions

 Revision 1.39  2012/11/01 20:52:21  mwitthoe
 conform timing tools to new version of ahfits: mainly change to how clobber to handled

 Revision 1.38  2012/11/01 14:39:37  mwitthoe
 time tools now access clobber state from ahgen, not ahapp

 Revision 1.37  2012/11/01 00:11:18  mwitthoe
 ahtime: now open files in initialize instead of doWork()

 Revision 1.36  2012/10/31 18:15:55  mwitthoe
 change ahtime tool to use new location of ahlookup library (now ahtime, not ahmath)

 Revision 1.35  2012/10/25 16:47:22  mwitthoe
 use new parameter retrieval functions from ahapp

 Revision 1.34  2012/10/25 01:38:59  mwitthoe
 ahtime tool now uses ahlookup instead of old ahtimfile

 Revision 1.33  2012/10/24 20:00:46  mwitthoe
 tweaks to ahtime tool

 Revision 1.32  2012/10/18 17:38:10  mwitthoe
 align ahtime tool with changes in ahtime library: namely, single ahtime header and new CAMS CALDB file

 Revision 1.31  2012/10/12 22:53:37  mwitthoe
 add a couple comments

 Revision 1.30  2012/10/11 18:08:29  mwitthoe
 convert ahtime tool over to new ahfits

 Revision 1.29  2012/09/26 00:39:08  mwitthoe
 for the ahtime tool: add test files for CAMS and SXI; add delay and column definition FITS files; change TI to L32TI

 Revision 1.28  2012/09/13 21:21:00  mwitthoe
 change to struct field name

 Revision 1.27  2012/09/13 20:57:15  mwitthoe
 changed ahtime tool to use new argument list of time_assign()

 Revision 1.26  2012/09/13 18:11:14  mwitthoe
 switch ahtime tool to new version scheme

 Revision 1.25  2012/09/11 18:53:56  mwitthoe
 now single time assignment function in ahtime tool instead of the obsolete detector-specific functions

 Revision 1.24  2012/09/05 18:31:47  mwitthoe
 ahtime tool now uses new version of ahcolumndef

 Revision 1.23  2012/08/31 20:57:38  mwitthoe
 use ahmission in ahtime tool

 Revision 1.22  2012/08/23 21:31:21  mwitthoe
 change from ahgen to ahapp in ahtime and ahtimeconv tools

 Revision 1.21  2012/08/21 19:53:32  mwitthoe
 ahtime tool now uses new version of instrument delay library, ahdelay

 Revision 1.20  2012/08/18 02:39:27  mwitthoe
 make ahtime and ahtimeconv tools work with new version of ahleapsec library

 Revision 1.19  2012/08/17 21:24:15  mwitthoe
 apply standards to ahtime tool

 Revision 1.18  2012/08/16 18:21:18  mwitthoe
 ahtime tool can now deal with CAMS event files

 Revision 1.17  2012/08/15 16:55:53  mwitthoe
 put finalize() in try-block in main() of ahtime and ahtimeconv

 Revision 1.16  2012/08/15 16:25:03  mwitthoe
 add versioning to ahtime and ahtimeconv tools

 Revision 1.15  2012/07/26 14:33:15  mwitthoe
 implement clobber in ahtime

 Revision 1.14  2012/07/20 14:57:32  mwitthoe
 add Doxygen to ahtime tool

 Revision 1.13  2012/07/17 22:36:36  mwitthoe
 allow ahtime tool to access leap second file from REFDATA

 Revision 1.12  2012/07/17 19:01:36  mwitthoe
 ahtime tool: edit par file descriptions, always ask for localtimehk parameter, check if calctime and calcutc are both false

 Revision 1.11  2012/07/16 21:02:53  mwitthoe
 update ahtime library/tool header comments

 Revision 1.10  2012/07/16 20:10:20  mwitthoe
 move reading of TIM data outside of HDU loop in doWork() of ahtime tool

 Revision 1.9  2012/07/12 20:39:32  mwitthoe
 update ahtime tool to use new time assignment function and ahtime classes

 Revision 1.8  2012/06/28 23:21:03  mwitthoe
 ahtime library and tool: add support for calctime, calcutc, and interpolation parameters; moved HDU loop from library to tool


*/

