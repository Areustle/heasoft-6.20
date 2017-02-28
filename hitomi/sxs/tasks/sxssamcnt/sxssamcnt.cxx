/// \file sxssamcnt.cxx
/// \brief Combine columns from SXS event or SXS-specific HK file and fill the
///  SampleCnt column needed for time assignment.
/// \author Mike Witthoeft
/// \date $Date: 2016/07/27 15:38:41 $
/// \version 1.0

/** 

\defgroup tool_sxssamcnt Determine SampleCnt for SXS files (sxssamcnt)
@ingroup mod_sxs_tasks

The time assignment for some SXS files requires a look-up table in the 
HK_ALLUSR extension of the SXS HK file.  This task computes the Local Time 
in both the SXS files and the lookup table so it can be used in the ahtime 
task.

Source files:

  sxssamcnt.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahtime
  astroh/sxs/lib/ahsxs

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-27   MCW    Clean-up code

*/
 
#define AHLABEL tool_sxssamcnt
#define AHCVSID "$Id: sxssamcnt.cxx,v 1.66 2016/07/27 15:38:41 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahlog/ahlog.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahapp/ahapp.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahtime/ahtime.h"
#include "ahsxs/ahsxs.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <iomanip>
#include <cmath>

/** \addtogroup tool_sxssamcnt
 *  @{
 */

struct Par {
  Par(): m_timever1val(0), m_timever2val(0) {}

  std::string m_infile;          // name of input file
  std::string m_outfile;         // name of output file (clone of input)
  std::string m_coeftime;        // name of CALDB file with coefficients
  std::string m_col1;            // TRIG_LP or LATCH_SAMPLE_CNT or LOST_#_LP
  std::string m_col2;            // TIME_VERNIER or LATCH_BASE_CNT
  std::string m_col3;            // WFRB_WRITE_LP
  std::string m_col4;            // WFRB_SAMPLE_CNT
  std::string m_col5;            // FLG_EVENT_LOST
  std::string m_timever1;        // input TIME_VERNIER for antico or start of GTI
  std::string m_timever2;        // input TIME_VERNIER for stop of GTI
  int m_timever1val;             // integer value of timever1 (if not DEFAULT)
  int m_timever2val;             // integer value of timever2 (if not DEFAULT)
  std::string m_outcol;          // name of output column: SAMPLECNT or SAMPLECNT#
};


/// \brief Get parameter values
/// \param[out] par  set of parameter values
void getPar(Par& par);

/// \brief Copy contents of infile to outfile; resolve default values of columns
/// \param[in,out] par  set of parameter values
/// \param[out] fp            FITS file pointer to output file
/// \param[out] filetype      type of input file (see anonymous enumeration in this file)
/// \param[out] columnout1    name of first output column
/// \param[out] columnout2    name of second output column
/// \param[out] coeff         set of coefficients from coeftime
void initialize(Par& par, ahfits::FilePtr & fp, std::string& filetype, 
                std::string& columnout1, std::string& columnout2,
                ahsxs::samcntcoeff::Coefficients& coeff);

/// \brief Fill in 2nd extension of output TIM file
/// \param[in] par  set of parameter values
/// \param[in] fp FITS file pointer to output file
/// \param[in] filetype type of input file (see anonymous enumeration in this file)
/// \param[in] columnout1    name of first output column
/// \param[in] columnout2    name of second output column
/// \param[in] coeff     set of coefficients from coeftime
void doWork(const Par& par, ahfits::FilePtr fp, const std::string& filetype, 
            const std::string& columnout1, const std::string& columnout2,
            ahsxs::samcntcoeff::Coefficients& coeff);

/// \brief close open FITS files
/// \param[in] fp FITS file pointer to output file
void finalize(ahfits::FilePtr fp);

/// \brief contruct a column name by replacing the character "#" with a
///  user-specified string
/// param[in] colname name of the column
/// param[in] repl string to replace '#' with
/// return column name
std::string formColNameStr(const std::string& colname, const std::string& repl);

/// \brief contruct a column name by replacing the character "#" with a
///  user-specified integer
/// param[in] colname name of the column
/// param[in] idx index number which will be inserted into the column name
/// return column name
std::string formColNameInt(const std::string & colname,int idx);


// ****************************************************************************

/// \brief ahmktim tool
///
int main(int argc, char** argv) {

  Par par;                                   // set of parameter values
  ahfits::FilePtr fp=0;                      // FITS file pointer to output file
  std::string filetype="";                   // type of SXS file; e.g. Antico
  std::string columnout1;                    // name of first output column
  std::string columnout2;                    // name of second output column
  ahsxs::samcntcoeff::Coefficients coeff;    // set of coefficients to use in SampleCnt calculation

  int status = ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par, fp, filetype, columnout1, columnout2, coeff);
      doWork(par, fp, filetype, columnout1, columnout2, coeff);
      finalize(fp);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fp, filetype, columnout1, columnout2, coeff);
        doWork(par, fp, filetype, columnout1, columnout2, coeff);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fp);
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
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_coeftime=ahapp::getParString("coeftime");
  par.m_col1=ahapp::getParString("col1");
  par.m_col2=ahapp::getParString("col2");
  par.m_col3=ahapp::getParString("col3");
  par.m_col4=ahapp::getParString("col4");
  par.m_col5=ahapp::getParString("col5");
  par.m_outcol=ahapp::getParString("outcol");
  par.m_timever1=ahapp::getParString("timever1");
  par.m_timever2=ahapp::getParString("timever2");

}

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr & fp, std::string& filetype, 
                std::string& columnout1, std::string& columnout2,
                ahsxs::samcntcoeff::Coefficients& coeff) {

  // declare variables
  std::string extname;    // name of HDU to access in input file

  // Copy contents of infile to outfile and return opened output file (fp);
  // if editing input file in-place (allowed with last argument = true), 
  // fp will point to the opened input file.
  // Extended syntax is allowed so that GTILOST files can be procesed.  These
  // files have two extensions (pixel & antico) with the same extension name.
  // This tool should be run twice on these files using extended syntax by
  // HDU number, not extension name (e.g. filename+2).
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) {
    ahfits::firstHDU(fp,ahfits::e_BINARY_TBL);    // only move to first binary table if extended syntax has not been used
  }
  ahmission::checkEmptyTable(fp,par.m_infile);

  // check that is SXS event of SXS-specific HK file
  if ("SXS" != ahfits::getKeyValStr(fp,"INSTRUME"))
    AH_THROW_RUNTIME("input file is not SXS event or HK file");

  // get file type and extension name
  // if HK file, then all extensions will be HK
  // if EVENTS file, then may be a non-EVENTS extension (e.g. gtilost)
  if (ahfits::keywordExists(fp,"HDUCLAS2") && "HKP" == ahfits::getKeyValStr(fp,"HDUCLAS2")) {
    filetype="HK";
    extname="HK_ALLUSR";
    ahfits::move(fp,"HK_ALLUSR");      // the extension name must match the value in the column definitions CALDB file (see ahtime)
  } else { //Not HK file; Science Data
    if ("GTILOST" == ahfits::getKeyValStr(fp,"EXTNAME")) {
      extname="GTILOST";
      filetype = "GTILOST";
    } else if ("EVENTS" == ahfits::getKeyValStr(fp,"EXTNAME")) {
      extname="EVENTS";
      std::string detstr=ahfits::getKeyValStr(fp,"DETNAM");
      std::string datamode=ahfits::getKeyValStr(fp,"DATAMODE");
      if ("ANTICO" == detstr || "PX_WFRB" == datamode)      // pixel WFRB files treated as antico
        filetype="ANTICO";
      else if ("PIXEL" == detstr)
        filetype="PIXEL";
    }
  }
  if (filetype == "") AH_THROW_RUNTIME("input file not SXS event or SXS-specific HK file");

  AH_INFO(ahlog::HIGH) << "Filename, extension, file type = " << par.m_infile << ", " << extname << ", " << filetype << std::endl;

  // sort out columns names if given "DEFAULT"
  if (filetype == "HK") { // note: col3/4 not used with HK
    if (ahgen::strtoupper(par.m_col1) == "DEFAULT") par.m_col1="LATCH_SAMPLE_CNT";
    if (ahgen::strtoupper(par.m_col2) == "DEFAULT") par.m_col2="LATCH_BASE_CNT";
    if (ahgen::strtoupper(par.m_outcol) == "DEFAULT") par.m_outcol="SAMPLECNT"; 
    AH_INFO(ahlog::HIGH) << "Input columns: " << par.m_col1 << ", " << par.m_col2 << std::endl;
    AH_INFO(ahlog::HIGH) << "Output column: " << par.m_outcol << std::endl;
  } else if (filetype == "GTILOST"){  // Lost GTI
    if (ahgen::strtoupper(par.m_col1) == "DEFAULT") par.m_col1="EL_#_LP";
    if (ahgen::strtoupper(par.m_col3) == "DEFAULT") par.m_col3="WFRB_WRITE_LP";
    if (ahgen::strtoupper(par.m_col4) == "DEFAULT") par.m_col4="WFRB_SAMPLE_CNT";
    if (ahgen::strtoupper(par.m_outcol) == "DEFAULT") par.m_outcol="SAMPLECNT#";    
    AH_INFO(ahlog::HIGH) << "Input columns: " << par.m_col1 << ", " << par.m_col3 << ", " << par.m_col4 << std::endl;
    AH_INFO(ahlog::HIGH) << "Output column: " << par.m_outcol << std::endl;
  } else if (filetype == "PIXEL") {   // PIXEL
    if (ahgen::strtoupper(par.m_col1) == "DEFAULT") par.m_col1="TRIG_LP";
    if (ahgen::strtoupper(par.m_col2) == "DEFAULT") par.m_col2="TIME_VERNIER";
    if (ahgen::strtoupper(par.m_col3) == "DEFAULT") par.m_col3="WFRB_WRITE_LP";
    if (ahgen::strtoupper(par.m_col4) == "DEFAULT") par.m_col4="WFRB_SAMPLE_CNT";
    if (ahgen::strtoupper(par.m_outcol) == "DEFAULT") par.m_outcol="SAMPLECNT#";
    AH_INFO(ahlog::HIGH) << "Input columns: " << par.m_col1 << ", " << par.m_col2 << ", " << par.m_col3 << ", " << par.m_col4 << std::endl;
    AH_INFO(ahlog::HIGH) << "Output column: " << par.m_outcol << std::endl;
  } else {       // ANTICO
    if (ahgen::strtoupper(par.m_col1) == "DEFAULT") par.m_col1="TRIG_LP";
    if (ahgen::strtoupper(par.m_col3) == "DEFAULT") par.m_col3="WFRB_WRITE_LP";
    if (ahgen::strtoupper(par.m_col4) == "DEFAULT") par.m_col4="WFRB_SAMPLE_CNT";
    if (ahgen::strtoupper(par.m_col5) == "DEFAULT") par.m_col5="FLG_EVENT_LOST";
    if (ahgen::strtoupper(par.m_outcol) == "DEFAULT") par.m_outcol="SAMPLECNT";
    AH_INFO(ahlog::HIGH) << "Input columns: " << par.m_col1 << ", " << par.m_col3 << ", " << par.m_col4 << ", " << par.m_col5 << std::endl;
    AH_INFO(ahlog::HIGH) << "Output column: " << par.m_outcol << std::endl;
  }

  // create output column if it is not present in FITS file
  if (filetype == "PIXEL") {
    //Form SampleCnt and SampleCntTrig column names from SampleCnt#
    columnout1 = formColNameStr(par.m_outcol, "");
    columnout2 = formColNameStr(par.m_outcol, "TRIG");
    if (!ahfits::haveColumn(fp,columnout1)){
      std::string colformat="D";
      ahfits::insertColAfter(fp,columnout1,colformat,par.m_col4);
      ahfits::setColumnDescription(fp,columnout1,"Used to calculate TIME");
      AH_INFO(ahlog::HIGH) << "Creating output column: " << columnout1 << std::endl;
    }
    if (!ahfits::haveColumn(fp,columnout2)){
      std::string colformat="D";
      ahfits::insertColAfter(fp,columnout2,colformat,columnout1);
      ahfits::setColumnDescription(fp,columnout2,"Used to calculate TRIGTIME");
      AH_INFO(ahlog::HIGH) << "Creating output column: " << columnout2 << std::endl;
    }

  } else if (filetype == "GTILOST"){
    //Form SampleCnt1 and SampleCnt2 column names from SampleCnt#
    columnout1 = formColNameInt(par.m_outcol, 1);
    columnout2 = formColNameInt(par.m_outcol, 2); 
    if (!ahfits::haveColumn(fp,columnout1)){
      std::string colformat="D";
      ahfits::insertColAfter(fp,columnout1,colformat,par.m_col4);
      ahfits::setColumnDescription(fp,columnout1,"Used to calculte START");
      AH_INFO(ahlog::HIGH) << "Creating output column: " << columnout1 << std::endl;
    }
    if (!ahfits::haveColumn(fp,columnout2)){
      std::string colformat="D";
      ahfits::insertColAfter(fp,columnout2,colformat,columnout1);
      ahfits::setColumnDescription(fp,columnout2,"Used to calculte STOP");
      AH_INFO(ahlog::HIGH) << "Creating output column: " << columnout2 << std::endl;
    }

    // Form LOST_START_LP and LOST_STOP_LP column names from LOST_#_LP (SXS_GTILOST)
    par.m_col2=formColNameStr(par.m_col1,"STOP");
    par.m_col1=formColNameStr(par.m_col1,"START");

  } else {  // ANTICO or HK
    if (!ahfits::haveColumn(fp,par.m_outcol)){ 
     std::string colformat="D";
     ahfits::insertColAfter(fp,par.m_outcol,colformat,par.m_col1);
      ahfits::setColumnDescription(fp,par.m_outcol,"Used to calculate TIME");
      AH_INFO(ahlog::HIGH) << "Creating output column: " << par.m_outcol << std::endl;
    }
  }

  // set values of timever1 and timever2 for GTI LOST or ANTICO files
  if (filetype == "GTILOST") {
    if (par.m_timever1 == "DEFAULT") {
      par.m_timever1val=+23;
    } else {
      par.m_timever1val=atoi(par.m_timever1.c_str());
    }
    if (par.m_timever2 == "DEFAULT") {
      par.m_timever2val=-8;
    } else {
      par.m_timever2val=atoi(par.m_timever2.c_str());
    }
    AH_INFO(ahlog::HIGH) << "Using timever1, timever2 = " << par.m_timever1val << ", " << par.m_timever2val << std::endl;
  }
  if (filetype == "ANTICO") {
    if (par.m_timever1 == "DEFAULT") {
      par.m_timever1val=0;
    } else {
      par.m_timever1val=atoi(par.m_timever1.c_str());
    }
    AH_INFO(ahlog::HIGH) << "Using timever1 = " << par.m_timever1val << std::endl;
  }

  // need to get coefficients from coeftime if SXS_PIXEL
  if (filetype == "PIXEL") {
    
    // tstart is actually the first S_TIME value. mxstime is run before ahtime,
    // so the TSTART keyword is not set yet.  Open the infile to get the first 
    // S_TIME value

    // open infile to first extension
    //get tstart in MET, using first row of S_TIME columne 
    ahfits::Router router(fp); 
    double stime0=0.0;
    router.connectScalar(ahfits::e_READONLY,"S_TIME",stime0);
    ahfits::firstRow(fp);
    ahfits::readRow(fp);
    router.clearConnections();
    
    // we need the datetime in order to look up CALDB
    // use MJDREFI to get UTC epoch in date/time format
    long long mjdrefi=(int)ahfits::getKeyValLLong(fp,"MJDREFI");
    ahtime::AhDateTime epoch;     // epoch as UTC
    ahtime::AhMJDTime epoch_mjd(mjdrefi);
    ahtime::reformatMJDAsDateTime(epoch_mjd,epoch);

    // add S_TIME from first row to epoch to approximate DATE-OBS
    // (leap seconds are neglected)
    ahtime::AhDateTime dateobs;
    ahtime::convertMissionTimeToTT(stime0,epoch,dateobs);
    std::string queryDate = dateobs.getDateTimeAsStr();
    
    // resolve the actual coeftime file, in case it's CALDB
    par.m_coeftime = ahmission::caldb::resolve(par.m_coeftime, "SXS time interval", "SXS", "PIXEL", "ARRCOEFFS", queryDate);
    // to record actual file path in par file, and in history keywords
    ape_trad_set_string("coeftime",par.m_coeftime.c_str());
    
    ahsxs::samcntcoeff::loadCoefficients(par.m_coeftime,stime0,coeff);

    AH_INFO(ahlog::LOW) << std::endl;
    AH_INFO(ahlog::LOW) << "Table of time coefficients:" << std::endl;
    AH_INFO(ahlog::LOW) << "     PIXEL      A_HIGH      B_HIGH      C_HIGH      A_MID       B_MID       C_MID       A_LOW       B_LOW       C_LOW" << std::endl;
    for (int ip=0; ip < 36; ip++) {
      int wid=12;
      std::stringstream line;
      line << std::setw(10) << ip;
      line << std::fixed << std::setprecision(4) << std::setw(wid) << coeff.m_ah[ip] << std::setw(wid) << coeff.m_bh[ip] << std::setw(wid) << coeff.m_ch[ip];
      line << std::fixed << std::setprecision(4) << std::setw(wid) << coeff.m_am[ip] << std::setw(wid) << coeff.m_bm[ip] << std::setw(wid) << coeff.m_cm[ip];
      line << std::fixed << std::setprecision(4) << std::setw(wid) << coeff.m_al[ip] << std::setw(wid) << coeff.m_bl[ip] << std::setw(wid) << coeff.m_cl[ip];
      AH_INFO(ahlog::LOW) << line.str() << std::endl;
    }
    AH_INFO(ahlog::LOW) << std::endl;
  } // end-if SXS_PIXEL file 

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(const Par& par, ahfits::FilePtr fp, const std::string& filetype, 
            const std::string& columnout1, const std::string& columnout2,
            ahsxs::samcntcoeff::Coefficients& coeff) {

  // set up local variables and router for input HK file with GPS data
  long col1=0;           // input col1
  long col2=0;           // input col2
  long col3=0;           // input col3
  long col4=0;           // input col4
  char lostcnt[1]={0};   // input column with lost count flag
  ahfits::IndexType num_lostcnt=1;        // size of FLG_LOST_EVENT for antico
  double colout=0.0;     // output column
  double colout1=0.0;    // output column 1 for SXS_GTILOST or SXS_PIXEL
  double colout2=0.0;    // output column 2 for SXS_GTILOST or SXS_PIXEL
  char outnull=0;        // set to 1 if output column should be NULL  
  char outnull1=0;       // null flag for SXS_GTILOST columnout1
  char outnull2=0;       // null flag SXS_GTILOST columnout2
  char proc_status[32];  // PROC_STATUS column
  ahfits::IndexType num_proc_status=32;   // size of PROC_STATUS column
  for (int ii=0; ii < num_proc_status; ii++) proc_status[ii]=0;
  int itype=0;           // ITYPE column (needed for SXS_PIXEL calculation)
  int rise_time=0;       // RISE_TIME column (needed for SXS_PIXEL calculation)
  int deriv_max=0;       // DERIV_MAX column (needed for SXS_PIXEL calculation)
  int pixel=0;           // PIXEL column (needed for SXS_PIXEL calculation)
  ahfits::Router router(fp);
  router.connectScalar(ahfits::e_READONLY,par.m_col1,col1);
  if (filetype != "ANTICO") router.connectScalar(ahfits::e_READONLY,par.m_col2,col2);
  if (filetype != "HK") router.connectScalar(ahfits::e_READONLY,par.m_col3,col3);
  if (filetype != "HK") router.connectScalar(ahfits::e_READONLY,par.m_col4,col4);
  if (filetype == "ANTICO" && ahfits::haveColumn(fp,par.m_col5)) {     // if FLG_EVENT_LOST not present, just use value of zero
    router.connectBit(ahfits::e_READONLY,par.m_col5,lostcnt,num_lostcnt);
  }
  if (filetype == "GTILOST" || filetype == "PIXEL") {
    router.connectScalar(ahfits::e_WRITEONLY,columnout1,colout1,&outnull1);
    router.connectScalar(ahfits::e_WRITEONLY,columnout2,colout2,&outnull2);
  } else {
    router.connectScalar(ahfits::e_WRITEONLY,par.m_outcol,colout,&outnull);
  }
  router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",proc_status,num_proc_status);
  if (filetype == "PIXEL") {
    router.connectScalar(ahfits::e_READONLY,"ITYPE",itype);
    router.connectScalar(ahfits::e_READONLY,"RISE_TIME",rise_time);
    router.connectScalar(ahfits::e_READONLY,"DERIV_MAX",deriv_max);
    router.connectScalar(ahfits::e_READONLY,"PIXEL",pixel);
  }

  long long irow=0;
  long long cntnull=0;       // number of events where NULL is assigned to samplecnt
  long long cntevtlost=0;    // number of antico events with FLG_EVENT_LOST flagged
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp) ) {
    irow++;
    ahfits::readRow(fp);

    // reset output NULL flag
    outnull = 0;
    outnull1 = 0;
    outnull2 = 0;

    // If bad PROC_STATUS, write NULL.  In order to allow for re-running of the
    // time assignment tools, we only check the Japanese bits of PROC_STATUS 
    // since the Astro-H bits of PROC_STATUS can be set bad by ahtime.
    if (proc_status[1] == 1) {
      outnull = 1;
      outnull1 = 1;
      outnull2 = 1;
      cntnull++;
    } else {

      if (filetype == "ANTICO") col2=par.m_timever1val;  //  TIME_VERNIER for ANTICO

      if (filetype == "HK") {
        // col1 = LATCH_SAMPLE_CNT
        // col2 = LATCH_BASE_CNT

        colout=(double)col1+(double)col2/400.;

      } else if (lostcnt[0] != 0) {       // ANTICO: FLAG_EVENT_LOST = yes

        outnull=1;
        outnull1 = 1;
        outnull2 = 1;
        cntnull++;
        cntevtlost++;

      } else if (filetype == "GTILOST") {

        // col1 = EL_START_LP
        // col2 = EL_STOP_LP
        // col3 = WFRB_WRITE_LP
        // col4 = WFRB_SAMPLE_CNT

        // The final mask, 0x3F, in the computations below ensure that the
        // wrapping from lap 63 to lap 0 is done correctly.  Without the
        // final mask the deltalap for this case would be -63, but after
        // applying the mask, deltalap=1.
        long deltalap1 = ( ((col3>>18)&0x3f)-((col1>>18)&0x3f) )&0x3F;
        long deltalap2 = ( ((col3>>18)&0x3f)-((col2>>18)&0x3f) )&0x3F;
        long lap1=(col1>>18)&0x3f;
        long lap2=(col2>>18)&0x3f;

        if ((deltalap1 > 4 && deltalap1 < 63) || (deltalap2 > 4 && deltalap2 < 63)) {
          cntnull++;
          outnull1=1;
          outnull2=1;
        } else {
          int dtimevernier=(par.m_timever1val-par.m_timever2val)/16+1;
          if ((((col2&0x3FFFF)-(col1&0x3FFFF))&0x3FFFF) < dtimevernier) {   // if time_vernier will cause the interval to be negative
            if (deltalap2 == 63) deltalap2=-1;
            colout2=(double)((col4-deltalap2*0x40000)&0xffffffff) + (double)(col2 & 0x3ffff);
            colout1=colout2;
          } else if ((col2 < col1) && (lap1<61 || lap2>2)) {      // illegal negative interval with no wrapping
            cntnull++;
            outnull1=1;
            outnull2=1;
          } else {

            // if deltalap == 63, then EL_START/STOP_LP trails WFRB_WRITE_LP
            if (deltalap1 == 63) deltalap1=-1;
            if (deltalap2 == 63) deltalap2=-1;

            // The mask, 0xffffffff, ensures that small WFRB_SAMPLE_CNT (col4)
            // values do not become negative when deltalap=1; instead the mask
            // wraps the values to be near to 2^32.
            colout1=(double)((col4-deltalap1*0x40000)&0xffffffff) + (double)(col1 & 0x3ffff) 
                   + (double)par.m_timever1val/16.;
            colout2=(double)((col4-deltalap2*0x40000)&0xffffffff) + (double)(col2 & 0x3ffff) 
                   + (double)par.m_timever2val/16.;
          }
        }

      } else {                        // PIXEL or ANTICO; FLG_EVENT_LOST = no

        // col1 = TRIG_LP
        // col2 = TIME_VERNIER   (PIXEL only)
        // col3 = WFRB_WRITE_LP
        // col4 = WFRB_SAMPLE_CNT
        // col5 = FLG_EVENT_LOST   (ANTICO only)

        // Get the difference between first 6 bits of two columns which 
        // represents the number of laps taken in the ring buffer.
        // The final mask, 0x3F, in the computations below ensure that the
        // wrapping from lap 63 to lap 0 is done correctly.  Without the
        // final mask the deltalap for this case would be -63, but after
        // applying the mask, deltalap=1.
        long deltalap=( ((col3>>18)&0x3f)-((col1>>18)&0x3f) )&0x3F;

        // check if WFRB_WRITE_LP < TRIG_LP which is an illegal condition
        if (deltalap > 4 && deltalap < 63) {
          AH_INFO(ahlog::HIGH) << "DeltaLap for row " << irow << " is invalid: " << deltalap << "; setting SampleCnt=NULL" << std::endl;
          outnull=1;
          cntnull++;
        } else {
          // in case TRIG_LP gets ahead of WFRB_WRITE_LP
          if (deltalap == 63) deltalap=-1;

          // Get sample count by finding location in ring buffer and adding
          // the lap amount.  The mask, 0xffffffff, ensures that small
          // WFRB_SAMPLE_CNT (col4) values do not become negative when 
          // deltalap=1; instead the mask wraps the values to be near to 2^32.
          colout=(double)((col4-deltalap*0x40000)&0xffffffff) + (double)(col1 & 0x3ffff) 
                 + (double)col2/16.;

          // if PIXEL case, then need to fill SAMPLECNTTRIG column too
          if (filetype == "PIXEL") {
            if (outnull == 1) {
              outnull1=1;       // SAMPLECNT=NULL
              outnull2=1;       // SAMPLECNTTRIG=NULL
            } else {
              colout2=colout;   // SAMPLECNTTRIG is not adjusted using coefficients
              double a=0.;
              double b=0.;
              double c=0.;
              if (itype == 0) {                           // Hp
                a=coeff.m_ah[pixel];
                b=coeff.m_bh[pixel];
                c=coeff.m_ch[pixel];
              } else if (itype == 1 || itype == 2) {      // Mp, Ms
                a=coeff.m_am[pixel];
                b=coeff.m_bm[pixel];
                c=coeff.m_cm[pixel];
              } else if (itype == 3 || itype == 4) {      // Lp, Ls
                a=coeff.m_al[pixel];
                b=coeff.m_bl[pixel];
                c=coeff.m_cl[pixel];
              } else {                                    // BL, LO, RJ
                a=0.;
                b=0.;
                c=0.;
              }
              int rise_time7=rise_time%128;                       // only use trailing 7-bits of RISE_TIME
              colout1=colout-(0.25*a*rise_time7+b*deriv_max+c);   // SAMPLECNT is adjusted using coefficients
            }
          }   // end if PIXEL
        }

      }

    }    // end else PROC_STATUS is okay

    // write to file
    ahfits::writeRow(fp);

  } // end for loop

  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Row counts " << std::endl;
  AH_INFO(ahlog::HIGH) << "  num rows:           " << irow << std::endl;
  AH_INFO(ahlog::HIGH) << "  num NULL:           " << cntnull << std::endl;
  if (filetype != "HK") {
    if (filetype == "ANTICO") AH_INFO(ahlog::HIGH) << "  num lost:           " << cntevtlost << std::endl;
  }

}

// ****************************************************************************

void finalize(ahfits::FilePtr fp) {

  ahfits::close(fp);
}

// ****************************************************************************

std::string formColNameStr(const std::string& colname, const std::string& repl) {

  // copy input column name and get location of replacement character
  std::string out=colname;
  size_t loc=out.find('#');

  // copy column name to output and do replacement
  out.replace(loc,1,repl);
  return out;
}

// ****************************************************************************

std::string formColNameInt(const std::string & colname, int idx) {

  // copy input column name and get location of replacement character
  std::string out=colname;
  size_t loc=out.find('#');

  // turn valid integer index into replacement character
  std::stringstream ch;
  ch << idx;

  // copy column name to output and do replacement
  out.replace(loc,1,ch.str());
  return out;
}


// ****************************************************************************


/** @} */


/* Revision Log
 $Log: sxssamcnt.cxx,v $
 Revision 1.66  2016/07/27 15:38:41  mwitthoe
 sxssamcnt: add some comments to explain the samplecnt computation

 Revision 1.65  2016/07/26 19:17:58  mwitthoe
 sxssamcnt: fix error in algorithm for case where WFRB_WRITE_LP is small and on the first lap and TRIG_LP is on the last lap leading to a negative SAMPLECNT

 Revision 1.64  2016/07/26 15:20:02  mwitthoe
 sxssamcnt: perform normal SampleCnt calculation for pixel lost events (ITYPE=6) instead of setting SampleCnt=NULL

 Revision 1.63  2016/06/01 20:55:41  mwitthoe
 sxssamcnt: compute samplecnt for negative-interval lost GTIs where the lap value has been reset

 Revision 1.62  2016/04/05 18:40:16  mwitthoe
 sxssamcnt: for lost GTI, set SampleCnt1/2 to NULL if EL_STOP_LP is less than EL_START_LP

 Revision 1.61  2016/03/30 04:34:47  mwitthoe
 sxssamcnt: previous fix for negative GTI in lost events was not correct

 Revision 1.60  2016/03/28 14:05:44  mwitthoe
 sxssamcnt: change samplecnt algorithm to handle case where TRIG_LP (or EL_START/STOP_LP) is larger than WFRB_WRITE_LP; new method from Sawada

 Revision 1.59  2016/03/24 14:23:48  mdutka
 addressing items listed in issue #610

 Revision 1.58  2015/12/29 16:39:37  mwitthoe
 sxssamcnt: throw error if no rows in input file

 Revision 1.57  2015/12/03 16:21:27  mwitthoe
 sxssamcnt: treat pixel WFRB files the same way as antico files

 Revision 1.56  2015/11/19 18:23:49  mwitthoe
 sxssamcnt: add column comments

 Revision 1.55  2015/10/27 21:34:44  mwitthoe
 sxssamcnt: 1) for antico files, if the FLAG_EVENT_LOST column is absent from the file (e.g. noiserec), assume a value of zero; 2) allow for extended syntax to that both extensions in the LOSTGTI files can be processed

 Revision 1.54  2015/10/07 15:00:27  mwitthoe
 sxssamcnt: only use the 7 least-significant bits of RISE_TIME in the SAMPLECNT calculation for pixel events

 Revision 1.53  2015/09/10 17:51:44  mwitthoe
 sxssamcnt: 1) only check Japanese bits of PROC_STATUS and ignore Astro-H bits; 2) change default column name for Lost GTI files: TRIG_#_LP to EL_#_LP

 Revision 1.52  2015/07/30 16:32:58  mwitthoe
 sxssamcnt: add parameter stamping to log file

 Revision 1.51  2015/07/29 15:38:25  mwitthoe
 sxssamcnt: clean tool; see issues 532, 533, and 534

 Revision 1.50  2015/07/22 03:03:02  klrutkow
 call ahmission::caldb resolve instead of local CALBD query

 Revision 1.49  2015/07/06 15:55:09  klrutkow
 changed DATE_OBS to DATE-OBS in comment

 Revision 1.48  2015/07/06 04:02:39  klrutkow
 fixed typos

 Revision 1.47  2015/07/02 20:09:39  klrutkow
 added time to CALDB search

 Revision 1.46  2015/07/01 17:39:43  klrutkow
 added CALDB query for coeftime

 Revision 1.45  2015/06/05 19:59:16  mwitthoe
 sxssamcnt: now read FLG_EVENT_LOST instead of FLAG_EVENT_LOST; the column is read as a 1X instead of 1B

 Revision 1.44  2015/06/03 23:04:44  mwitthoe
 sxssamcnt: only check HDUCLAS2 keyword if present in header; change HK calculation factor from 160 to 400

 Revision 1.43  2015/04/28 16:43:10  mwitthoe
 sxssamcnt: remove '1' from default HK column names; see issue 515

 Revision 1.42  2015/04/03 18:44:45  mwitthoe
 sxssamcnt: update boolean parameters; convert parameters to uppercase before checking for DEFAULT

 Revision 1.41  2015/03/23 19:43:15  mwitthoe
 sxssamcnt: change extension name of Lost GTI from GTI to GTILOST; see issue 496

 Revision 1.40  2015/03/23 18:14:50  mwitthoe
 sxssamcnt: coefficients applied to science events to calculate SAMPLECNT are now pixel-dependent; see issue 496

 Revision 1.39  2015/03/20 19:43:41  mwitthoe
 sxssamcnt: remove obsolete BASELINE run mode (baseline data is now part of the PIXEL file)

 Revision 1.38  2015/03/20 19:18:07  mwitthoe
 sxssamcnt: change FLAG_LOST_CNT column name to FLAG_EVENT_LOST; do not apply coefficients to SAMPLECNT for lost or baseline events; see issue 496

 Revision 1.37  2015/03/18 20:46:00  asargent
 Changed DETNAME to DETNAM

 Revision 1.36  2015/01/08 18:41:54  mwitthoe
 sxssamcnt: for Antico files, add SampleCnt column after TRIG_LP instead of TIME_VERNIER since the latter may not be present

 Revision 1.35  2014/12/29 19:32:11  klrutkow
 updated tool with new parameters, per issue 472

 Revision 1.34  2014/11/20 19:09:27  mwitthoe
 sxssamcnt: update according to issue 457: 1) remove check on DATAMODE keyword, 2) compute SAMPLECNTTRIG column in event files, 3) get coeficients needed for event SAMPLECNT calculation from CALDB file

 Revision 1.33  2014/11/04 20:34:40  mwitthoe
 sxssamcnt: send AH_INFO message with row number when the output SAMPLECNT column is set to NULL; capitalize all column names; see issue 421

 Revision 1.32  2014/08/19 13:52:13  mwitthoe
 sxssamcnt: the timever1 parameter is now used to set TIME_VERNIER for antico input files

 Revision 1.31  2014/07/23 15:15:48  mwitthoe
 sxssamcnt: clean up source code a bit

 Revision 1.30  2014/07/21 18:21:58  mdutka
 Added changes to sxssamcnt based on Issue #374 in redmine

 Revision 1.29  2014/03/26 18:06:15  mwitthoe
 sxssamcnt: change ahfits::connect() to ahfits::connectScalar()

 Revision 1.28  2014/01/22 21:32:18  mwitthoe
 sxssamcnt: update according to code review; issue 331

 Revision 1.27  2014/01/22 19:20:41  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.26  2014/01/09 20:05:48  rshill
 Deleted stale comment

 Revision 1.25  2014/01/03 22:12:05  mwitthoe
 sxssamcnt: update standard main, issue 327

 Revision 1.24  2013/12/02 23:00:21  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.23  2013/09/19 14:43:08  mwitthoe
 sxssamcnt: old version was checking a header keyword before moving to the first binary table extension causing the unit tests to fail... fixed

 Revision 1.22  2013/07/31 20:18:23  mwitthoe
 sxssamcnt: remove unnecessary include for ahgen

 Revision 1.21  2013/05/15 19:53:15  mwitthoe
 sxssamcnt tool: add new ring buffer wrapping check for WFRB_WRITE_LP and TRIG_LP; check for illegal condition between these columns

 Revision 1.20  2013/04/18 19:28:22  mwitthoe
 sxssamcnt tool: remove unnecessary +++ comomment from source

 Revision 1.19  2013/04/08 17:32:59  mwitthoe
 allow for more than one extension in SXS science input file by checking if not HK before attempting to move to EVENTS extension; this change was made to account for a gti extension in the sample data file providing by Japan via Mike Loewenstein (ah20121020_1658_1714_sxs_ff.evt)

 Revision 1.18  2013/04/04 20:32:59  mwitthoe
 sxssamcnt: remove dependency on ahmission; implemented NULL via ahfits; add read/write status in ahfits connections

 Revision 1.17  2013/03/28 18:53:44  mwitthoe
 changed sxssamcnt.cxx to match latest TRF

 Revision 1.16  2013/03/22 16:27:53  mwitthoe
 add TOOLTAG macro to sxssamcnt

 Revision 1.15  2013/03/21 20:13:22  mwitthoe
 sxssamcnt: remove TOOLTAG macro since the CVS macro, Name, does not work as hoped; TOOLTAG is now defined in ahapp.h

 Revision 1.14  2013/03/20 15:42:58  mwitthoe
 sxssamcnt: CVS keyword, Tag, appears incorrect, trying Name

 Revision 1.13  2013/03/20 15:41:17  mwitthoe
 sxssamcnt: change standard main to conform to new standard; add TOOLTAG giving the tool version to be reported in the parameter dump of output FITS files

 Revision 1.12  2013/03/08 22:06:01  mwitthoe
 sxssamcnt: add note about possible problem when calculating SampleCnt for science data; identified by Mike L

 Revision 1.11  2013/02/11 17:45:53  peachey
 Force DNULL keyword to be treated as a double to avoid wrapping -2^31 to *positive* 2^31.

 Revision 1.10  2013/02/11 15:57:23  mwitthoe
 change sxssamcnt.cxx and make_test_sxssamcnt.cxx to use 32-bit DNULL

 Revision 1.9  2013/02/09 05:27:57  mwitthoe
 sxssamcnt: add NULL support for science and HK data (in addition to antico)

 Revision 1.8  2013/02/09 04:08:11  mwitthoe
 change sxssamcnt to use the DNULL keyword instead of NAN when setting the SampleCnt column to NULL

 Revision 1.7  2013/01/29 19:33:06  mwitthoe
 sxssamcnt: change output SampleCnt column from type J to D, change calculation of SampleCnt accordingly; add program under sxssamcnt/testdata which will create sample data files for 3 cases: HK, Science, and Antico

 Revision 1.6  2013/01/15 20:38:13  mwitthoe
 sxssamcnt: fixed problems highlighted in 2013-01-07 checklist: changed while-loop over FITS rows to standard for-loop; insert output column into FITS file if not present

 Revision 1.5  2013/01/07 20:17:30  mwitthoe
 update parameter listing in the sxssamcnt tool

 Revision 1.4  2012/12/10 19:28:33  mwitthoe
 make sxssamcnt tools up-to-date with recent changes to ahfits

 Revision 1.3  2012/11/28 21:10:43  mwitthoe
 sxssamcnt: add parameter for FLAG_LOST_CNT; FLAG_LOST_CNT now applies to Pixel and Antico; corrected extension names for science and HK files; correctly identify HK or science data

 Revision 1.2  2012/11/26 20:19:04  mwitthoe
 add algorithm to sxssamcnt; update doxygen description of tool

 Revision 1.1  2012/11/16 20:47:39  mwitthoe
 add task sxssamcnt; builds but missing algorithm


*/

