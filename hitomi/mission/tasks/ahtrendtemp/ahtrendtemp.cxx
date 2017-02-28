/// \file ahtrendtemp.cxx
/// \brief Calculate frequency vs temperature data from trend data in HK file.
/// \author Mike Witthoeft
/// \date $Date: 2015/12/29 19:20:54 $
/// \version 1.0

/** 

\defgroup tool_ahtrendtemp Produce freq v. temp data (ahtrendtemp)
@ingroup mod_mission_tasks

To assign time Astro-H uses the GPS as well as an onboard clock. The onboard 
clock stability is related to temperature variation. The relation between 
frequency and temperature has been measured on ground. ahtrendtemp 
calculates the same frequency/temperature relation for the onboard clock 
using the inflight data. 

Source files:

  ahtrendtemp.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahtime

Modification history:

  Ver     Date        Author      Description
  1.0   2015-07-07    MCW & AJS   Clean-up code

*/

// +++ 2013-02-28 MCW need meaningful default/min/max values for tempresol and stimemax



#define AHLABEL tool_ahtrendtemp
#define AHCVSID "$Id: ahtrendtemp.cxx,v 1.58 2015/12/29 19:20:54 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahtime/ahtime.h"
#include "ahmath/ahmath.h"
#include "ahgen/ahgen.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <limits>
#include <vector>
#include <utility>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <fstream>

/** \addtogroup tool_ahtrendtemp
 *  @{
 */

/// \brief Get parameter values
/// \param[out] infile input quartz trend file
/// \param[out] frqtemfile existing freq vs. temp CALDB file
/// \param[out] outtemp file name to dump input frequencies and temperatures
/// \param[out] outfile output file with frequency and temperature
/// \param[out] starttime time to start processing HK file
/// \param[out] stoptime time to stop processing HK file
/// \param[out] quartzext name of extension containing quartz clock count
/// \param[out] tempext name of extension containing temperature
/// \param[out] quartzcol column with quartz clock count
/// \param[out] l32ticol column with L32TI in quartz extension
/// \param[out] u32ticol column with Quartz_U32TI in quartz extension
/// \param[out] tempcol column containing quartz temperature
/// \param[out] stimecol column containing S_TIME
/// \param[out] leapsecfile location of leap second table
/// \param[out] tempresol temperature resolution where measurements are averaged
/// \param[out] stimemax maximum value of delta S_TIME
/// \param[out] averagemode mode to average frequency: 1) simple average, 2) TBD
void getPar(std::string& infile, std::string& frqtemfile, 
            std::string& outtemp, std::string& outfile, std::string& starttime,
            std::string& stoptime, std::string& quartzext,
            std::string& tempext, std::string& quartzcol,
            std::string& l32ticol, std::string& u32ticol, std::string& tempcol, 
            std::string& stimecol, std::string& leapsecfile, 
            double& tempresol, double& stimemax, int& averagemode);

/// \brief Copy contents of frqtemfile to outfile.  Load leap second data.
/// \param[in] infile input quartz trend file
/// \param[in] frqtemfile existing freq vs. temp CALDB file
/// \param[in] quartzext name of extension containing quartz clock count
/// \param[in] leapsecfile location of leap second table
/// \param[in] outfile output file with frequency and temperature
/// \param[out] outcolfreq column name in output file for frequencies
/// \param[out] outcoltemp column name in output file for temperatures
/// \param[out] outcolnpt column name in output file for number of points in bin
/// \param[in,out] averagemode mode to average frequency
/// \param[out] infilelist list of input file names (from @filelist)
/// \param[out] fpout FITS file pointer to outfile (being cloned from frqtemfile)
/// \param[out] leapsecdat leap second data
void initialize(const std::string& infile, const std::string& frqtemfile, 
                const std::string& quartzext,
                const std::string& leapsecfile, const std::string& outfile, 
                std::string& outcolfreq, std::string& outcoltemp, 
                std::string& outcolnpt, int& averagemode, 
                ahfits::ListStringType& infilelist,
                ahfits::FilePtr & fpout, 
                ahtime::leapsec::LeapSecTable & leapsecdat);


/// \brief Use quartz trend file to calculate frequency vs temperature data
/// \param[in] infilelist list of input file names
/// \param[in] fpout FITS file pointer to outfile (being cloned from 
/// \param[in] outtemp file name to dump input frequencies and temperatures
/// \param[in] leapsecdat leap second data
/// \param[in] starttime time to start processing HK file
/// \param[in] stoptime time to stop processing HK file
/// \param[in] quartzext name of extension containing quartz clock count
/// \param[in] tempext name of extension containing temperature
/// \param[in] quartzcol column with quartz clock count
/// \param[in] l32ticol column with L32TI in quartz extension
/// \param[in] u32ticol column with Quartz_U32TI in quartz extension
/// \param[in] tempcol column containing quartz temperature
/// \param[in] stimecol column containing R_TIME
/// \param[in] outcolfreq column name in output file for frequencies
/// \param[in] outcoltemp column name in output file for temperatures
/// \param[in] outcolnpt column name in output file for number of points in bin
/// \param[in] tempresol temperature resolution where measurements are averaged
/// \param[in] stimemax maximum value of delta S_TIME
/// \param[in] averagemode mode to average frequency
void doWork(const ahfits::ListStringType& infilelist, ahfits::FilePtr & fpout, 
            const std::string& outtemp, 
            ahtime::leapsec::LeapSecTable & leapsecdat,
            const std::string& starttime, 
            const std::string& stoptime, const std::string& quartzext, 
            const std::string& tempext, const std::string& quartzcol, 
            const std::string& l32ticol, const std::string& u32ticol, 
            const std::string& tempcol, const std::string& stimecol, 
            const std::string& outcolfreq, const std::string& outcoltemp,
            const std::string& outcolnpt,
            double tempresol, double stimemax, int averagemode);

/// \brief close open FITS files
/// \param[in] fpout FITS file pointer to outfile (being cloned from 
void finalize(ahfits::FilePtr fpout);


// ****************************************************************************

/// \brief ahtrendtemp main
int main(int argc, char** argv) {

  std::string infile;         // input quartz trend file
  std::string frqtemfile;     // existing freq vs. temp CALDB file
  std::string outtemp;        // file name to dump input frequencies and temperatures
  std::string outfile;        // output file with frequency and temperature
  std::string starttime;      // time to start processing HK file
  std::string stoptime;       // time to stop processing HK file
  std::string quartzext;      // name of extension containing quartz clock count
  std::string tempext;        // name of extension containing temperature
  std::string quartzcol;      // column with quartz clock count
  std::string l32ticol;       // column with L32TI in quartz extension
  std::string u32ticol;       // column with Quartz_U32TI in quartz extension
  std::string tempcol;        // column containing quartz temperature
  std::string stimecol;       // column containing S_TIME
  std::string outcolfreq;     // column in output file for frequency
  std::string outcoltemp;     // column in output file for temperature
  std::string outcolnpt;      // column in output file for number of points per bin
  std::string leapsecfile;    // location of leap second FITS file
  double tempresol=0.;        // temperature resolution where measurements are averaged
  double stimemax=0.;         // maximum value of delta S_TIME
  int averagemode=1;          // mode to average frequency: 1) simple average, 2) TBD

  ahfits::ListStringType infilelist;          // list of input file names
  ahfits::FilePtr fpout=NULL;                 // FITS file pointer of output file

  ahtime::leapsec::LeapSecTable leapsecdat;   // leap second data

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(infile, frqtemfile, outtemp, outfile,  starttime, stoptime, 
             quartzext, tempext, quartzcol, l32ticol, u32ticol, tempcol, 
             stimecol, leapsecfile, tempresol, stimemax, averagemode);
      initialize(infile, frqtemfile, quartzext, leapsecfile, outfile, 
                 outcolfreq, outcoltemp, outcolnpt, averagemode, infilelist, 
                 fpout, leapsecdat);
      doWork(infilelist, fpout, outtemp, leapsecdat, starttime, stoptime, 
             quartzext, tempext, quartzcol, l32ticol, u32ticol, 
             tempcol, stimecol, outcolfreq, outcoltemp, outcolnpt, 
             tempresol, stimemax, averagemode);
      finalize(fpout);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile, frqtemfile, outtemp, outfile,  starttime, stoptime, 
               quartzext, tempext, quartzcol, l32ticol, u32ticol, tempcol, 
               stimecol, leapsecfile, tempresol, stimemax, averagemode);
        initialize(infile, frqtemfile, quartzext, leapsecfile, outfile, 
                   outcolfreq, outcoltemp, outcolnpt, averagemode, infilelist, 
                   fpout, leapsecdat);
        doWork(infilelist, fpout, outtemp, leapsecdat, starttime, stoptime, 
               quartzext, tempext, quartzcol, l32ticol, u32ticol, 
               tempcol, stimecol, outcolfreq, outcoltemp, outcolnpt, 
               tempresol, stimemax, averagemode);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout);
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

void getPar(std::string& infile, std::string& frqtemfile, 
            std::string& outtemp, std::string& outfile, std::string& starttime,
            std::string& stoptime, std::string& quartzext,
            std::string& tempext, std::string& quartzcol,
            std::string& l32ticol, std::string& u32ticol, std::string& tempcol, 
            std::string& stimecol, std::string& leapsecfile, 
            double& tempresol, double& stimemax, int& averagemode) {

  infile=ahapp::getParString("infile");
  frqtemfile=ahapp::getParString("frqtemfile");
  outtemp=ahapp::getParString("outtemp");
  outfile=ahapp::getParString("outfile");
  starttime=ahapp::getParString("starttime");
  stoptime=ahapp::getParString("stoptime");
  quartzext=ahapp::getParString("quartzext");
  tempext=ahapp::getParString("tempext");
  quartzcol=ahapp::getParString("quartzcol");
  l32ticol=ahapp::getParString("l32ticol");
  u32ticol=ahapp::getParString("u32ticol");
  tempcol=ahapp::getParString("tempcol");
  stimecol=ahapp::getParString("stimecol");
  leapsecfile=ahapp::getParString("leapsecfile");
  tempresol=ahapp::getParDouble("tempresol");
  stimemax=ahapp::getParDouble("stimemax");
  averagemode=ahapp::getParInt("averagemode");

}

// ****************************************************************************

void initialize(const std::string& infile, const std::string& frqtemfile, 
                const std::string& quartzext,
                const std::string& leapsecfile, const std::string& outfile, 
                std::string& outcolfreq, std::string& outcoltemp, 
                std::string& outcolnpt, int& averagemode,
                ahfits::ListStringType& infilelist,
                ahfits::FilePtr & fpout,
                ahtime::leapsec::LeapSecTable & leapsecdat) {

  // declare variables
  std::string leapfilename;   // name of leap second file (from CALDB)
  std::string hduout;         // name of output HDU
  std::string frqtemfilename; // name of freq vs temp file (from CALDB)

  // read leapsecond data
  leapfilename=ahmission::caldb::resolve(leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
  AH_INFO(ahlog::LOW) << "Using leapsecond file: " << leapfilename <<std::endl;
  ahtime::leapsec::load(leapfilename,leapsecdat);
  
  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("leapsecfile",leapfilename.c_str());

  // check for legal values of averagemode
  if (averagemode != 1)
    AH_THROW_RUNTIME("only averagemode=1 currently supported");

  // get list of input files
  ahfits::expandFileList(infile, infilelist);
  if (infilelist.size() == 0)
    AH_THROW_RUNTIME("no file list found in infile: "+infile);
  AH_INFO(ahlog::HIGH) << "Processing " << infilelist.size() << " input files" << std::endl;

  // get the DATE-OBS AND SMUUNIT keyword from the quartz extension of the 
  // first infile, for the frqtem CALDB query
  std::string firstinfile=*infilelist.begin();
  ahfits::FilePtr fpq=NULL;
  ahfits::open(firstinfile,quartzext,&fpq);
  if (!ahfits::readOK(fpq))
    AH_THROW_RUNTIME("failed to open input file: "+firstinfile+"at HDU: "+quartzext);
  std::string smuunit=ahfits::getKeyValStr(fpq,"SMUUNIT");
  std::string datetime=ahfits::getKeyValStr(fpq,"DATE-OBS");
  ahfits::close(fpq);
  AH_INFO(ahlog::HIGH) << "Input file is for SMU = " << smuunit << std::endl;
  AH_INFO(ahlog::HIGH) << "Input file has DATE-OBS = " << datetime << std::endl;

  
  // get the freq vs temp filename
  std::string expr_str = "SMUUNIT.eq." + smuunit;
  frqtemfilename=ahmission::caldb::resolve(frqtemfile, "freq vs temp", "GEN", "-", "TIME_FREQ", datetime, expr_str);
  AH_INFO(ahlog::LOW) << "Using frequency v. temperature file: " << frqtemfilename <<std::endl;

  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("frqtemfile",frqtemfilename.c_str());
  
  // clone input CALDB file to output file
  ahfits::clone(frqtemfilename,outfile,&fpout);

  // create new extension in output file based on header from 1st HDU
  hduout="TEMPORARY";    // will be renamed after data added
  ahfits::addHDU(fpout,"FVT",fpout,hduout);
  ahfits::move(fpout,"TEMPORARY");

  // names of columns in output file
  outcolfreq="FREQ";
  outcoltemp="TEMP";
  outcolnpt="NPTTEMP";

  // create NPTTEMP column, if not present
  if (!ahfits::haveColumn(fpout,outcolnpt)) {
    ahfits::insertColAfter(fpout,outcolnpt,"I",outcoltemp);
    ahfits::setColumnDescription(fpout,outcolnpt,"ahtrendtemp");
    AH_INFO(ahlog::HIGH) << "Adding column: " << outcolnpt << std::endl;
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(const ahfits::ListStringType& infilelist, ahfits::FilePtr & fpout, 
            const std::string& outtemp, 
            ahtime::leapsec::LeapSecTable & leapsecdat,
            const std::string& starttime, 
            const std::string& stoptime, const std::string& quartzext, 
            const std::string& tempext, const std::string& quartzcol, 
            const std::string& l32ticol, const std::string& u32ticol, 
            const std::string& tempcol, const std::string& stimecol, 
            const std::string& outcolfreq, const std::string& outcoltemp,
            const std::string& outcolnpt,
            double tempresol, double stimemax, int averagemode) {

  // temperature vs. clock data will be stored as a std::vector of std::pairs 
  // (temp,freq); this list will be sorted by temperature before further
  // processing
  // note: the iterator, tvcit, is needed when printing the list of points
  // to the ASCII file (named by parameter outtemp)
  std::vector< std::pair<double, double> > tvc;
  std::vector< std::pair<double, double> >::iterator tvcit;

  // PERIODCL is a keyword in the quartz clock extension which is used to
  //  convert clock count into frequency; it is assumed to have the same value
  //  in all input files
  std::string keyname_pcl="PERIODCL";
  double periodcl=0;

  // the START_TIME and STOP_TIME keywords indicate the range of S_TIME
  //  values encountered in the input files
  double key_tstart=-1.;
  double key_tstop=0.0;

  // Astro-H epoch will be read from each extension, but needed to set
  // DATE-OBS/DATE-END keywords for output file after following for-loop
  ahtime::AhDateTime epoch;

  // local variables for output file
  double temp_out=0.;
  double freq_out=0.;
  int npt_out=0;

  // loop variables going over sorted temperatures vs. clock count
  int i=0;                          // current index in tvc vector
  int istart=0;                     // first index of active bin
  int imax=0;                       // largest index

  // total number of rows read/skipped
  int nskip_tot=0;            // total number of rows skipped
  int nrows_tot=0;            // total number of rows read

  // DATE-OBS, DATE-END, and SMUUNIT keyword values
  std::string dateobs;
  std::string dateend;
  std::string smuunit="";     // empty string means undefined

  // loop over input files
  ahfits::ListStringType::const_iterator fit;
  double stime_qq=0.;     // conversion of Quartz_U32TI to S_TIME-like value
  double tstart=0.;
  bool copykeywords=true;   // copy keywords from Quartz_U32TI extension if true
  for (fit=infilelist.begin(); fit != infilelist.end(); fit++) {
    std::string infile=*fit;
    AH_INFO(ahlog::HIGH) << "Processing file: " << infile << std::endl;

    ahfits::FilePtr fpq=NULL;   // ahfits file pointer for quartz clock HDU
    ahfits::FilePtr fpt=NULL;   // ahfits file pointer for temperature HDU
    int mjdrefi=0;              // integer part of MJD epoch
    double mjdreff=0.;          // fractional part of MJD epoch
    double tstop=0.;            // stop time in seconds
    double stime_t_prev=0.;     // store S_TIME from previous row
    double temp_prev=0.;        // store TEMP from previous row
    int irow=0;                 // current row index
    int nskip=0;                // number of rows skipped in current file

    // local variables connecting to quartz clock extension
    long l32ti_q=0;
    long qu32ti_q=0;
    double clkcnt=0.;

    // local variables connecting to temperature extension
    double stime_t=0.;
    double temp=0.;

    // Open file to quartz clock and temperature extensions in input file.
    // Skip file if either extension is missing.
    ahfits::open(infile,"",&fpq);
    if (!ahfits::HDUExists(fpq,quartzext)) {
      AH_INFO(ahlog::HIGH) << "  Skipping file because quartz clock extension is missing: " << infile << std::endl;
      continue;
    }
    ahfits::move(fpq,quartzext);
    if (0 == ahfits::numRows(fpq)) {
      AH_INFO(ahlog::HIGH) << "  Skipping file because there are no rows in the quartz clock extension: " << infile << std::endl;
      continue;
    }

    ahfits::open(infile,"",&fpt);
    if (!ahfits::HDUExists(fpt,tempext)) {
      AH_INFO(ahlog::HIGH) << "  Skipping file because temperature extension is missing: " << infile << std::endl;
      continue;
    }
    ahfits::move(fpt,tempext);
    if (0 == ahfits::numRows(fpt)) {
      AH_INFO(ahlog::HIGH) << "  Skipping file because there are no rows in the temperature extension: " << infile << std::endl;
      continue;
    }

    // copy keywords from Quartz extension to output
    if (copykeywords) {
      ahmission::keyword::copyAllKeywords(fpq,fpout,ahmission::keyword::e_HK);
      copykeywords=false;
    }

    // read SMUUNIT from quartz clock extension, and
    //  if 1st file: write SMUUNIT keyword to output file
    //  else: check that SMUUNIT is consistent with previous value
    // +++ 2013-12-19 MCW should an inconsistency in SMUUNIT be a warning or error?
    if (smuunit == "") {
      smuunit=ahfits::getKeyValStr(fpq,"SMUUNIT");
      ahfits::writeKeyValStr(fpout,"SMUUNIT",smuunit,"");
    } else {
      if (smuunit != ahfits::getKeyValStr(fpq,"SMUUNIT"))
        AH_THROW_RUNTIME("  inconsistent SMUUNIT keyword in input file list");
    }

    // Read GPSOFFET keyword which is used to get the time of the quartz
    // clock measurement.
    double gpsoffet=ahfits::getKeyValDbl(fpq,"GPSOFFET");

    // get Astro-H epoch from header of input file (quartz extension)
    mjdrefi=(int)ahfits::getKeyValLLong(fpq,"MJDREFI");
    mjdreff=ahfits::getKeyValDbl(fpq,"MJDREFF");
    ahtime::AhMJDTime ttepoch_mjd(mjdrefi,mjdreff);
    ahtime::AhDateTime ttepoch;
    ahtime::reformatMJDAsDateTime(ttepoch_mjd,ttepoch);
    ahtime::convertTTToUTC(ttepoch,leapsecdat,epoch);

    // get PERIODCL value
    periodcl=ahfits::getKeyValDbl(fpq,keyname_pcl);

    // convert starttime and stoptime from UTC to seconds to allow comparison
    //  with S_TIME
    tstart=0.;
    tstop=std::numeric_limits<double>::max();
    if (ahgen::strtoupper(starttime) != "ALL") {

      // convert starttime to UTC and check if starttime < epoch
      ahtime::AhDateTime tmp1(starttime);
      if (tmp1.compare(epoch) < 0) {
        AH_THROW_RUNTIME("  starttime parameter cannot be earlier than epoch; file:"
                         +infile);
      }

      // set number of seconds between starttime and epoch
      tstart=numSecInUTCInterval(epoch,tmp1,leapsecdat,0);
    }
    if (ahgen::strtoupper(stoptime) != "ALL") {

      // convert stoptime to UTC and check if stoptime < epoch
      ahtime::AhDateTime tmp1(stoptime);
      if (tmp1.compare(epoch) < 0) {
        AH_THROW_RUNTIME("  stoptime parameter cannot be earlier than epoch; file:"
                         +infile);
      }

      // set number of seconds between stoptime and epoch
      tstop=numSecInUTCInterval(epoch,tmp1,leapsecdat,0);
    }
    if (tstart >= tstop)
      AH_THROW_RUNTIME("  starttime/stoptime parameters define invalid range");

    // connect columns from quartz clock extension to local variables
    ahfits::Router routq(fpq);
    routq.connectScalar(ahfits::e_READONLY,l32ticol,l32ti_q);
    routq.connectScalar(ahfits::e_READONLY,u32ticol,qu32ti_q);
    routq.connectScalar(ahfits::e_READONLY,quartzcol,clkcnt);

    // connect columns from temperature extension to local variables
    ahfits::Router routt(fpt);
    routt.connectScalar(ahfits::e_READONLY,stimecol,stime_t);
    routt.connectScalar(ahfits::e_READONLY,tempcol,temp);

    // read first two temperatures to prepare for searching
    ahfits::readRow(fpt);
    stime_t_prev=stime_t;
    temp_prev=temp;
    ahfits::nextRow(fpt);
    ahfits::readRow(fpt);

    // loop over rows of quartz extension
    nskip=0;
    irow=0;          // current row index
    for (ahfits::firstRow(fpq); ahfits::readOK(fpq); ahfits::nextRow(fpq)) {


      // read row data from clock-quartz extension: L32TI, S_TIME,
      //  Quartz_U32TI, and RAW_QUARTZ_CLOCK
      ahfits::readRow(fpq);
      irow++;

      // skip rows where Quartz_U32TI is zero
      if (qu32ti_q == 0) {
        AH_INFO(ahlog::LOW) << "Row " << irow << ": skip row with Quartz_U32TI = 0" << std::endl;
        nskip++;
        continue;
      }

      // skip will indicate whether there is a problem with operating on the
      // current row of the quartz clock extension.  a value of zero means
      // everything is okay; non-zero values are defined here
      //  skip     meaning
      //   0        okay
      //   1        S_TIME from quartz clock extension outside range of
      //             temperature extension
      //   2        adjacent S_TIME values from temperature extension (T1 and T2)
      //             which bracket the active S_TIME (T3) from the quartz clock
      //             extension are both further from the active S_TIME than stimemax,
      //             i.e. [(T3-T1) > stimemax] AND [(T2-T3) > stimemax]
      int skip=0;

      double temp_q=0.;    // temperature interpolated onto quartz clock S_TIME

      // Convert Quartz_U32TI to Seconds since AstroH epoch (we call stime_qq)
      // by subtracting the GPS offset (seconds between the GPS and Astro-H
      // seconds).
      stime_qq=(double)qu32ti_q-gpsoffet;

      // update TSTART/TSTOP
      if (key_tstart < 0. || stime_qq < key_tstart) key_tstart=stime_qq;
      if (stime_qq > key_tstop) key_tstop=stime_qq;

      // check if in starttime/stoptime range
      if (stime_qq > tstop) {
        if (irow == 1) {
          AH_INFO(ahlog::HIGH) << "  *** input time range does not overlap with starttime "
                               << "  and stoptime parameters (stoptime < 1st input time)"
                               << std::endl;
        }
        break;
      }
      if (stime_qq < tstart) continue;

      // check if first S_TIME from quartz clock extension is inside range
      //  of temperature extension
      if (stime_qq < stime_t_prev) skip=1;

      // find position in temperature extension for interpolation
      //  result will have  stime_t_prev < stime_qq < stime_t
      if (skip == 0) {
        while (stime_t < stime_qq) {
          if (!ahfits::readOK(fpt)) break;
          stime_t_prev=stime_t;
          temp_prev=temp;
          ahfits::nextRow(fpt);
          ahfits::readRow(fpt);
        }
        if (stime_t < stime_qq) skip=1;    // temperature S_TIME range exceeded
        if ( (stime_qq-stime_t_prev) > stimemax && (stime_t-stime_qq) > stimemax) {
          skip=2;
        }
      }
      if (skip == 1) {
        AH_INFO(ahlog::LOW) << "  Quartz S_TIME outside range of temperature extension; skipping row " << irow << std::endl;
        nskip++;
      } else if (skip == 2) {
        AH_INFO(ahlog::HIGH) << "  Quartz S_TIME in large gap of temperature extension; skipping row " << irow << std::endl;
        AH_INFO(ahlog::LOW) << "  time to previous/next temperature row: " << (stime_qq-stime_t_prev) << " / " << (stime_t-stime_qq) << std::endl;
        nskip++;
      } else {
        AH_INFO(ahlog::LOW) << "  Row " << irow << ": time(quartz), time(temp) = " << stime_qq << ", " << stime_t << std::endl;
      }

      // check if stimemax is exceeded for first row
      if (irow == 1 && skip == 2) {
        AH_INFO(ahlog::HIGH) << "  *** stimemax parameter exceeded in first row of " 
                             << infile << std::endl;
      }

      // interpolate temperature using linear method
      temp_q=0.;
      if (skip == 0) {
        temp_q=ahmath::interpolate_point_twopoint(stime_qq,stime_t_prev,temp_prev,
                                                  stime_t,temp);
      }

      // write to output list (also converting clkcnt to frequency)
      if (skip == 0) tvc.push_back( std::make_pair(temp_q,clkcnt/periodcl) );

    }   // end loop over rows

    // summarize points read/skipped
    AH_INFO(ahlog::LOW) << "File: " << infile << "  points kept/skipped: "
                        << tvc.size() << "/" << nskip << std::endl;

    // close input file
    ahfits::close(fpq);
    ahfits::close(fpt);

    nskip_tot+=nskip;
    nrows_tot+=irow;

  }    // end loop over files

  // check if any valid points found
  if (tvc.size() == 0) {
    if (stime_qq < tstart) {
      AH_INFO(ahlog::HIGH) << "*** input time range does not overlap with starttime "
                           << "and stoptime parameters (starttime > last input time)" 
                           << std::endl;
    }

    AH_INFO(ahlog::HIGH) << "*** No valid points read; nothing to be done" << std::endl;
    return;
  }

  // sort temperature vs clock count data by temperature
  sort(tvc.begin(),tvc.end());

  // dump list of frequencies and temperatures to ASCII file: outtemp
  std::ofstream fd;
  fd.open(outtemp.c_str());   // terse way to remove const-ness
  fd << "# " << std::setw(12) << "Temperature" << std::setw(12) << "Frequency"
     << std::endl;
  for (tvcit=tvc.begin(); tvcit != tvc.end(); tvcit++) {
    fd << std::setw(12) << std::scientific << std::setprecision(4) << tvcit->first
       << std::setw(20) << std::scientific << std::setprecision(8) << tvcit->second
       << std::endl;
  }
  fd.close();

  // set up local variables and router for output file
  temp_out=0.;
  freq_out=0.;
  npt_out=0;
  ahfits::Router rout_out(fpout);
  rout_out.connectScalar(ahfits::e_WRITEONLY,outcolfreq,freq_out);
  rout_out.connectScalar(ahfits::e_WRITEONLY,outcoltemp,temp_out);
  rout_out.connectScalar(ahfits::e_WRITEONLY,outcolnpt,npt_out);

  // loop over sorted temperature vs. clock count data
  i=0;                          // current index in tvc vector
  istart=i;                     // first index of active bin
  imax=tvc.size()-1;            // largest index
  while (1) {

    int binsize=0;              // size of temperature bin

    // find end of bin   (  .first gives temperature)
    while (tvc[i].first-tvc[istart].first < tempresol) {
      i++;
      if (i > imax) break;    // see note below
    }
    binsize=i-istart;

    // note: upon reaching the end of the tvc vector, the index, i, will
    // be equal to the vector size and therefore greater than index of the
    // last element by 1.  this is okay since the averaging operation below
    // only goes up to i-1.

    // average temperatures and frequencies according to averagemode parameter
    switch (averagemode) {

      case 1:      // normal averaging
        temp_out=0.;
        freq_out=0.;
        for (int j=istart; j < i; j++) {
          temp_out+=tvc[j].first;
          freq_out+=tvc[j].second;
        }
        npt_out=binsize;
        temp_out/=(double)binsize;
        freq_out/=(double)binsize;
        break;

      default:  // shouldn't be here since averagemode is checked in initialize
        AH_THROW_LOGIC("illegal value of averagemode");
    }

    // print out bin information: start, end, average
    {
      std::stringstream inf_t1,inf_t2,inf_i1,inf_i2,inf_np,inf_ta,inf_fa;
      inf_t1 << std::setw(6) << std::setprecision(4) << tvc[istart].first;
      inf_t2 << std::setw(6) << std::setprecision(4) << tvc[i-1].first;
      inf_i1 << std::setw(3) << std::setprecision(4) << istart;
      inf_i2 << std::setw(3) << std::setprecision(4) << i-1;
      inf_np << std::setw(4) << binsize;
      inf_ta << std::setw(18) << std::setprecision(6) << temp_out;
      inf_fa << std::setw(18) << std::setprecision(10) << freq_out;
      AH_INFO(ahlog::LOW) << "BIN TEMP1,TEMP2,IDX1,IDX2,NPTS,TEMPAVG,FREQAVG: " 
                          << inf_t1.str() << "  " << inf_t2.str() << "  " 
                          << inf_i1.str() << "  " << inf_i2.str() << "  " 
                          << inf_np.str() << "  " << inf_ta.str() << "  " 
                          << inf_fa.str() << std::endl;
    }

    // write values to output file and reset bin start position
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);
    istart=i;

    // end of tvc vector?
    if (i >= imax) break;
  }
  AH_INFO(ahlog::HIGH) << "Total number of averaged quantities: " << i << std::endl;
  AH_INFO(ahlog::HIGH) << "Total number of input files:         " << infilelist.size() << std::endl;
  AH_INFO(ahlog::HIGH) << "Total number of rows read:           " << nrows_tot << std::endl;
  AH_INFO(ahlog::HIGH) << "Total number of rows skipped:        " << nskip_tot << std::endl;

  // update header keywords: TSTART & TSTOP
  ahfits::writeKeyValDbl(fpout,"TSTART",key_tstart,"");
  ahfits::writeKeyValDbl(fpout,"TSTOP",key_tstop,"");

  // convert TSTART (TAI) to DATE-OBS (UTC) : YYYY-MM-DDThh:mm:ss.xxxxxx
  // convert TSTOP (TAI) to DATE-END (UTC)
  if (key_tstart > 0.) {
    dateobs=ahtime::calcDateObs(key_tstart,epoch,leapsecdat);
    ahfits::writeKeyValStr(fpout,"DATE-OBS",dateobs,"");
  } else {
    AH_INFO(ahlog::HIGH) << "Cannot calculate DATE-OBS since TSTART < 0" << std::endl;
  }
  if (key_tstop > 0.) {
    dateend=ahtime::calcDateObs(key_tstop,epoch,leapsecdat);
    ahfits::writeKeyValStr(fpout,"DATE-END",dateend,"");
  } else {
    AH_INFO(ahlog::HIGH) << "Cannot calculate DATE-END since TSTOP < 0" << std::endl;
  }

  // write other keywords
  ahfits::writeKeyValLLong(fpout,"NUMCLQRZ",tvc.size(),"");
  ahfits::writeKeyValDbl(fpout,"TEMPBIN",tempresol,"");
  ahfits::writeKeyValDbl(fpout,"STIMEMAX",stimemax,"");
  ahfits::writeKeyValLLong(fpout,"AVGMODE",averagemode,"");

  // update extension name
  ahfits::writeKeyValStr(fpout,"EXTNAME","FVT","");

}

// ****************************************************************************

void finalize(ahfits::FilePtr fpout){
  ahfits::close(fpout);
}

// ****************************************************************************


/** @} */


/* Revision Log
 $Log: ahtrendtemp.cxx,v $
 Revision 1.58  2015/12/29 19:20:54  mwitthoe
 ahtrendtemp: add check on number of rows in the input extensions

 Revision 1.57  2015/11/02 19:07:22  mwitthoe
 ahtrendtemp: fix log message for number of events skipped in event file

 Revision 1.56  2015/10/07 19:49:25  mwitthoe
 ahtrendtemp: change to the quartz clock time (stime_qq) expression given by Yuki; change default values of the the extension/column parameters to the values given by Yuki

 Revision 1.55  2015/07/29 20:28:13  mwitthoe
 ahtrendtemp: remove +++ comment

 Revision 1.54  2015/07/29 20:26:19  mwitthoe
 ahtrendtemp: add description for added column; copy keywords from input quartz extension to output extension

 Revision 1.53  2015/07/22 16:14:11  klrutkow
 changed local CALDB queries for frqtem and leapsec to use ahmission caldb resolve query

 Revision 1.52  2015/07/14 20:52:50  klrutkow
 added closed first fits file in initialize, after getting DATE-OBS

 Revision 1.51  2015/07/07 16:27:06  mwitthoe
 ahtrendtemp: add several log statements to tool as part of clean-up procedure; see issue 533

 Revision 1.50  2015/07/07 14:07:08  klrutkow
 fixed error when calling ape_trad_set_str

 Revision 1.49  2015/07/06 15:49:56  klrutkow
 changed DATE_OBS to DATE-OBS and DATE_END to DATE-END

 Revision 1.48  2015/07/06 04:11:06  klrutkow
 added quartzetx to initialize, for CALDB query ; added call to new frqtem::resolve function to get CALDB filename ; changed extension name from PRELAUNCH to FVT for extension to add

 Revision 1.47  2015/04/03 18:10:13  mwitthoe
 ahtrendtemp: change boolean parameters to use lowercase yes/no; convert starttime and stoptime parameters to uppercase before checking if ALL

 Revision 1.46  2015/01/05 20:56:18  mwitthoe
 ahtrendtemp: update parameters; see issue 472

 Revision 1.45  2014/09/10 03:56:33  mwitthoe
 ahtrendtemp: fix bug related to switchover to new leap second library

 Revision 1.44  2014/09/10 02:46:42  mwitthoe
 ahtrendtemp tool: update tool to reflect new locations of timfile and leapsec CALDB libraries

 Revision 1.43  2014/08/06 14:13:57  mwitthoe
 ahtrendtemp: fix bug where integer variable was being initialized with a double

 Revision 1.42  2014/08/05 12:21:37  mwitthoe
 ahtrendtemp: change type of a variable to avoid compiler warning when an implicit type conversion took place

 Revision 1.41  2014/01/22 19:17:03  mwitthoe
 ahtrentemp: update documentation

 Revision 1.40  2014/01/21 21:50:51  mwitthoe
 ahtrendtemp: revise according to code review; issue 331

 Revision 1.39  2014/01/14 19:18:47  rshill
 Code review comments

 Revision 1.38  2014/01/03 21:53:20  mwitthoe
 ahtrendtemp: issue 327... forgot to add include for iostream

 Revision 1.37  2014/01/03 21:48:59  mwitthoe
 ahtrendtemp: update standard main, see issue 327

 Revision 1.36  2013/12/19 16:07:02  mwitthoe
 ahtrendtemp: copy SMUUNIT keyword from input file to output file; add SMUUNIT keyword to sample test files

 Revision 1.35  2013/12/02 22:54:46  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.34  2013/11/13 22:50:18  mwitthoe
 ahtrendtemp: move variable declarations to top of scope

 Revision 1.33  2013/10/07 13:47:03  mwitthoe
 ahtrendtemp: switch to using new ahfits connect functions (see issue 270)

 Revision 1.32  2013/09/17 19:30:33  mwitthoe
 ahtrendtemp tool: switch over to using new ahtime library functions (see issue 290)

 Revision 1.31  2013/09/03 15:45:08  mwitthoe
 ahtrendtemp: fixed sign error associated with uses the stimemax parameter; this will change the results from the unit test, ut02

 Revision 1.30  2013/08/12 19:05:50  mwitthoe
 ahtrendtemp: process checklist submitted by Reggie on 2013-07-31

 Revision 1.29  2013/07/30 19:00:24  mwitthoe
 ahtrendtemp: remove unnecessary include for ahgen

 Revision 1.28  2013/07/24 17:23:35  mwitthoe
 ahtrendtemp tool: fix tool based on checklist performed by KR on 2013-07-23: fix Doxygen section name; update parameter list in tool with parameter file

 Revision 1.27  2013/04/17 17:31:45  mwitthoe
 use ahmath interpolation library in ahtrendtemp tool

 Revision 1.26  2013/04/12 13:18:29  mwitthoe
 update ahtrendtemp tool to use new version of leap second library (now in ahmission)

 Revision 1.25  2013/04/10 14:41:31  mwitthoe
 ahtrendtemp: add missing ahfits FilePtr initializations

 Revision 1.24  2013/04/10 14:39:58  mwitthoe
 ahtrendtemp: fix comment; initialize ahfits FilePtr to NULL

 Revision 1.23  2013/04/04 17:23:32  mwitthoe
 ahtrendtemp: remove ahmission dependency, change to new connect functions which specify read/write only

 Revision 1.22  2013/03/26 14:32:24  mwitthoe
 update standard main for ahtrendtemp tool

 Revision 1.21  2013/03/05 21:52:26  mwitthoe
 ahtrendtemp: improve comments

 Revision 1.20  2013/03/05 10:56:02  mwitthoe
 ahtrendtemp: remove finished TODO items from source; fix listing of FITS parts read/written

 Revision 1.19  2013/03/05 10:36:24  mwitthoe
 ahtrendtemp: update code based on discussion in Japan (Feb 2013): edit parameters, correct U32TI to S_TIME conversion, write extra keywords to output, add output column NPTTEMP, change test condition with parameter stimemax, fix output column names for FREQ and TEMP, change default extension names, change test data to match new standards

 Revision 1.18  2013/02/19 18:32:45  mwitthoe
 fix typo in error message in ahtrendtemp

 Revision 1.17  2013/01/30 20:09:03  mwitthoe
 add several checks for the starttime/stoptime parameters compared against the range of U32TI in the QUARTZ HDU of the input file: 1) starttime/stoptime range invalid and 2) ranges do not overlap; any of these problems result in an error

 Revision 1.16  2013/01/30 18:48:40  mwitthoe
 fix bug in ahtrendtemp causing seg fault when stimemax parameter set too small (issue 198)

 Revision 1.15  2013/01/16 02:33:57  mwitthoe
 modify ahtrendtemp based on checklist: fix extension names in parameter file; fix a couple parameter types; fix some doxygen

 Revision 1.14  2013/01/03 21:29:31  mwitthoe
 change misleading name of variable colname_pcl to keyname_pcl

 Revision 1.13  2012/12/18 21:18:12  mwitthoe
 ahtrendtemp: add test data in tool directory; modify parameter file to match test data files and extensions; debug tool (TSTART/TSTOP, averagin, output extension name); add low priority information statements to detail temperature binning

 Revision 1.12  2012/12/11 19:27:42  mwitthoe
 ahtrendtemp: get output column names from cloned extension; dump sorted list of temperatures and frequencies derived from all input files; write DATE_OBS and DATE_END keywords, read U32TI from quartz HDU instead of S_TIME; misc fixed to get tool to build

 Revision 1.11  2012/12/10 19:27:22  mwitthoe
 make timing tools up-to-date with recent changes to ahfits

 Revision 1.10  2012/12/03 20:25:26  mwitthoe
 add doxygen to ahtrendtemp tool; fix bug where the quartz clock count was not being converted to frequency using the PERIODCL keyword

 Revision 1.9  2012/11/29 21:00:12  mwitthoe
 major changes to ahtrendtemp to match algorithm in latest TRF; data is read from two extensions instead of one and frequency calculation has changed; new parameters as well

 Revision 1.8  2012/11/15 03:01:38  mwitthoe
 change timing tools to use new ahcaldb libraries

 Revision 1.7  2012/11/13 18:34:50  mwitthoe
 change timing tools to use new open/create/clone functions from ahtime

 Revision 1.6  2012/11/04 23:28:05  mwitthoe
 make timing tools (ahtime, ahmktim, ahtrendtemp) consistent with new ahfits clone functions

 Revision 1.5  2012/11/01 20:52:21  mwitthoe
 conform timing tools to new version of ahfits: mainly change to how clobber to handled

 Revision 1.4  2012/11/01 14:39:37  mwitthoe
 time tools now access clobber state from ahgen, not ahapp

 Revision 1.3  2012/11/01 00:04:04  mwitthoe
 change ahtrendtemp tool to use new version of ahlookup

 Revision 1.2  2012/10/31 18:59:53  mwitthoe
 ahtrendtemp: open files in initialize() and pass ahfits FilePtrs to doWork

 Revision 1.1  2012/10/25 20:17:45  mwitthoe
 add the ahtrendtemp tool


*/

