/// \file ahmktim.cxx
/// \brief Create TIM_LOOKUP table for time assignment
/// \author Mike Witthoeft
/// \date $Date: 2016/07/11 19:48:39 $
/// \version 0.0

/**

\defgroup tool_ahmktim Produce TIM file (ahmktim)
@ingroup mod_mission_tasks

+++ 2015-07-08 AS/MW: Need short description from TRF

Source files:

  ahmktim.cxx
  ahmktimlib.cxx
  ahmktimlib.h

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahtime

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-07    MCW & AJS   Clean-up code

*/ 

#define AHLABEL tool_ahmktim
#define AHCVSID "$Id: ahmktim.cxx,v 1.66 2016/07/11 19:48:39 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahmktimlib.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahtime/ahtime.h"
#include "ahgen/ahgen.h"
#include "ahmission/ahmission.h"
#include "ahmission/keyword.h"
#include "ahmission/caldb.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <cmath>
#include <sstream>
#include <iomanip>     // std::setprecision
#include <vector>


/** \addtogroup tool_ahmktim
 *  @{
 */

/// \brief Get parameter values
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief Copy contents of timfile to outtimfile.  Read in freq vs. temp data
///  from CALDB file
/// \param[in,out] par structure with parameter values
/// \param[out] fphk1 FITS file pointer to input HK file: GPS HDU
/// \param[out] fptim FITS file pointer to output TIM file
/// \param[out] gp FITS file pointer for output GTI file
/// \param[out] hcedat structure used to search HK_HCE extension
/// \param[out] freqtempdata data from freq v. temp CALDB file
/// \param[out] leapsecdat leap second data from CALDB file
void initialize(Par& par, ahfits::FilePtr & fphk1,
                ahfits::FilePtr & fptim, ahfits::FilePtr & gp, 
                hce::HCE& hcedat, freqtemp::DataType& freqtempdata,
                ahtime::leapsec::LeapSecTable & leapsecdat,
                AnchorData& timepackets);

/// \brief Fill in 2nd extension of output TIM file
/// \param[in] par structure with parameter values
/// \param[in] fphk1 FITS file pointer to input HK file: GPS HDU
/// \param[in] fptim FITS file pointer to output TIM file
/// \param[in] gp FITS file pointer for output GTI file
/// \param[in] hcedat structure used to search HK_HCE extension
/// \param[in] freqtempdata data from freq v. temp CALDB file
/// \param[in] leapsecdat leap second data from CALDB file
void doWork(const Par& par, ahfits::FilePtr fphk1, ahfits::FilePtr fptim, 
            ahfits::FilePtr & gp, hce::HCE& hcedat,
            freqtemp::DataType& freqtempdata, 
            ahtime::leapsec::LeapSecTable & leapsecdat,
            AnchorData& timepackets);

/// \brief close open FITS files
/// \param[in] fphk1 FITS file pointer to input HK file: GPS HDU
/// \param[in] fptim FITS file pointer to output TIM file
/// \param[in] gp FITS file pointer for output GTI file
/// \param[in] hcedat structure used to search HK_HCE extension
/// \param[in] freqtempdata data from freq v. temp CALDB file
void finalize(ahfits::FilePtr fphk1, ahfits::FilePtr fptim, ahfits::FilePtr gp,
              hce::HCE& hcedat, freqtemp::DataType& freqtempdata);

// ****************************************************************************

/// \brief ahmktim tool
int main(int argc, char** argv) {

  Par par;                                   // structure with parameter values

  freqtemp::DataType freqtempdata;           // data from freq v. temp CALDB file
  ahtime::leapsec::LeapSecTable leapsecdat;  // leap second data
  AnchorData timepackets;                    // store TIME_PACKETS data
  hce::HCE hcedat;                           // used to search HK_HCE extension

  ahfits::FilePtr fphk1=0;                   // FITS file pointer to input HK file: GPS HDU
  ahfits::FilePtr fptim=0;                   // FITS file pointer to output TIM file
  ahfits::FilePtr gp=0;                      // GTI file structure

  int status=ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par,fphk1,fptim,gp,hcedat,freqtempdata,leapsecdat,timepackets);
      doWork(par,fphk1,fptim,gp,hcedat,freqtempdata,leapsecdat,timepackets);
      finalize(fphk1,fptim,gp,hcedat,freqtempdata);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fphk1,fptim,gp,hcedat,freqtempdata,leapsecdat,timepackets);
        doWork(par,fphk1,fptim,gp,hcedat,freqtempdata,leapsecdat,timepackets);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fphk1,fptim,gp,hcedat,freqtempdata);
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

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_frqtemfile=ahapp::getParString("frqtemfile");
  par.m_timfile=ahapp::getParString("timfile");
  par.m_outtimfile=ahapp::getParString("outtimfile");
  par.m_outgtifile=ahapp::getParString("outgtifile");
  par.m_leapsecfile=ahapp::getParString("leapsecfile");
  par.m_timext=ahapp::getParString("timext");
  par.m_hkgpsext=ahapp::getParString("hkgpsext");
  par.m_hktempext=ahapp::getParString("hktempext");
  par.m_gaptime=64.*ahapp::getParDouble("gaptime");   // convert gaptime from seconds to L32TI units
  par.m_stimecol=ahapp::getParString("stimecol");
  par.m_l32ticol=ahapp::getParString("l32ticol");
  par.m_tempcol=ahapp::getParString("tempcol");
  par.m_packheadcol=ahapp::getParString("packheadcol");
  par.m_gpsacol=ahapp::getParString("gpsacol");
  par.m_gpsbcol=ahapp::getParString("gpsbcol");
  par.m_gpsccol=ahapp::getParString("gpsccol");
  par.m_gpsdcol=ahapp::getParString("gpsdcol");
  par.m_suzdrifttime=ahapp::getParDouble("suzdrifttime");

}

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr & fphk1,
                ahfits::FilePtr & fptim, ahfits::FilePtr & gp, 
                hce::HCE& hcedat, freqtemp::DataType& freqtempdata,
                ahtime::leapsec::LeapSecTable & leapsecdat,
                AnchorData& timepackets) {

  // declare variables
  std::string leapfilename;        // leap second filename from CALDB
  std::string fn_frqtemfile;       // freqtemp filename
  std::string gps_smuunit;         // SMUUNIT keyword from GPS HDU - A or B
  long long mjdrefi = 0;
  double mjdreff = 0.; 
  double tstart = 0.; 
 
  // open HK file to GPS extension
  ahfits::open(par.m_infile,par.m_hkgpsext,&fphk1);
  AH_INFO(ahlog::HIGH) << "Opened GPS HDU: " << par.m_infile << "[" << par.m_hkgpsext << "]" << std::endl;
  ahmission::checkEmptyTable(fphk1,par.m_infile);

  // read SMUUNIT from GPS HDU
  gps_smuunit=ahfits::getKeyValStr(fphk1,"SMUUNIT");
  AH_INFO(ahlog::HIGH) << "SMUUNIT from GPS HDU = " << gps_smuunit << std::endl;
  
  // prepare structure used to search for temperature extension
  hce::setupHCE(par.m_infile,par.m_hktempext,par.m_stimecol,par.m_tempcol,
                hcedat);
  AH_INFO(ahlog::HIGH) << "Opened temperature HDU: " << par.m_infile << "[" << par.m_hktempext << "]" << std::endl;

  // read leapsecond data
  leapfilename=ahmission::caldb::resolve(par.m_leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
  AH_INFO(ahlog::LOW) << "Using leapsecond file: " << leapfilename <<std::endl;
  ahtime::leapsec::load(leapfilename,leapsecdat);
  
  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("leapsecfile",leapfilename.c_str());

  //get tstart in MET, using first row of S_TIME columne 
  ahfits::Router router_hk(fphk1); 
  router_hk.connectScalar(ahfits::e_READONLY,"S_TIME",tstart);
  ahfits::firstRow(fphk1);
  ahfits::readRow(fphk1);
  router_hk.clearConnections();
  AH_INFO(ahlog::HIGH) << "S_TIME from first row of GPS HDU: " << tstart << std::endl;
        
  //Obtain epoch
  mjdrefi=(int)ahfits::getKeyValLLong(fphk1,"MJDREFI");
  mjdreff=ahfits::getKeyValDbl(fphk1,"MJDREFF");
  ahtime::AhDateTime ttepoch;   // epoch as terrestrial time
  ahtime::AhDateTime epoch;     // epoch as UTC
  ahtime::AhMJDTime ttepoch_mjd(mjdrefi,mjdreff);
  ahtime::reformatMJDAsDateTime(ttepoch_mjd,ttepoch);
  ahtime::convertTTToUTC(ttepoch,leapsecdat,epoch);
  std::string datetime = ahtime::calcDateObs(tstart, epoch, leapsecdat);

  // load data from freq vs. temp CALDB file
  fn_frqtemfile=ahmission::caldb::resolve(par.m_frqtemfile,"Freq v Temp","GEN","-","TIME_FREQ",datetime);
  ape_trad_set_string("frqtemfile",fn_frqtemfile.c_str()); // store path in history
  AH_INFO(ahlog::HIGH) << "Using freq vs temp file: " << fn_frqtemfile <<std::endl;
  freqtemp::load(fn_frqtemfile,freqtempdata);

  AH_INFO(ahlog::HIGH) << "All input files opened." <<std::endl;

  // create output GPS GTI file
  create_gti_file(par.m_outgtifile,gp);
  ahmission::keyword::copyAllKeywords(fphk1,gp,ahmission::keyword::e_GTI);

  // If no input file provided, create a new file from scratch; otherwise 
  // clone input file to output file.  If editing input file in-place (allowed
  // with last argument = true), fptim will point to the opened input file.
  if (par.m_timfile == "" || ahgen::strtoupper(par.m_timfile) == "NONE") {
    ahfits::create(par.m_outtimfile,"",&fptim);
    AH_INFO(ahlog::HIGH) << "No input TIM file; Creating new file: " << par.m_outtimfile <<std::endl;
  } else {
    ahfits::clone(par.m_timfile,par.m_outtimfile,&fptim,true);
    AH_INFO(ahlog::HIGH) << "Appending TIM lookup table to existing file: " << par.m_outtimfile <<std::endl;
  }

  // If TIME_PACKETS extension exists in TIM file, read whole table of 
  // R_TIME vs L32TI used to anchor Suzaku-mode points when GPS is
  // unsynchronized for long periods of time.  If the TIME_PACKETS
  // extension is not found, then the tool will still run, but no
  // anchoring will be done.
  if (ahfits::HDUExists(fptim,"TIME_PACKETS")) {
    ahfits::move(fptim,"TIME_PACKETS");

    timepackets.m_tstart=ahfits::getKeyValDbl(fptim,"TSTART");
    timepackets.m_l32tistart=ahfits::getKeyValDbl(fptim,"LTISTART");
    timepackets.m_tstop=ahfits::getKeyValDbl(fptim,"TSTOP");
    timepackets.m_l32tistop=ahfits::getKeyValDbl(fptim,"LTISTOP");

    // Read table
    double l_stime=0.;
    double l_l32ti=0.;
    ahfits::Router rout(fptim);
    rout.connectScalar(ahfits::e_READONLY,"S_TIME",l_stime);
    rout.connectScalar(ahfits::e_READONLY,"L32TI",l_l32ti);
    for (ahfits::firstRow(fptim); ahfits::readOK(fptim); ahfits::nextRow(fptim)) {
      ahfits::readRow(fptim);
      timepackets.m_stime.push_back(l_stime);
      timepackets.m_l32ti.push_back(l_l32ti);
    }
    timepackets.m_nrows=timepackets.m_stime.size();
  }

  // Add output extension from scratch: TIM_LOOKUP
  add_tim_extension(fptim,par.m_timext,gps_smuunit);
  ahmission::keyword::copyAllKeywords(fphk1,fptim,ahmission::keyword::e_HK);  

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(const Par& par, ahfits::FilePtr fphk1, ahfits::FilePtr fptim, 
            ahfits::FilePtr & gp, hce::HCE& hcedat,
            freqtemp::DataType& freqtempdata, 
            ahtime::leapsec::LeapSecTable & leapsecdat,
            AnchorData& timepackets) {

  // variables:
  // 
  // INPUT from HK1 (GPS extension)
  //  hk1_gpsa
  //  hk1_gpsb
  //  hk1_gpsc
  //  hk1_gpsd
  //  hk1_ti
  //  hk1_stime
  //  hk1_pheader      packet_header
  //  hk1_seqnum       seqnum (bits 18-31 of packet_header)
  //
  // OUTPUT to TIM file
  //  tim_ti
  //  tim_time
  //  tim_status
  //
  // The algorithm operates on two rows at a time, the variables
  // starting with prev_ will be the next written to the output
  // TIM file or are the previously read values from HK1:
  //  prev_time         -- to be output to TIME of TIM file (calculated)
  //  prev_status       -- to be output to GPS_STATUS of TIM file
  //  prev_gpsmode      -- determined from GPS A-D flags
  //  prev_ti           -- hk1_ti from previous row
  //  prev_stime        -- hk1_stime from previous row
  //

  // local variables to connect with columns in GPS HDU of HK file
  long long hk1_ti=0;              // L32TI
  double hk1_stime=0.;             // S_TIME
  char hk1_gpsa=0;                 // GPS flag A
  char hk1_gpsb=0;                 // GPS flag B
  char hk1_gpsc=0;                 // GPS flag C
  char hk1_gpsd=0;                 // GPS flag D
  int hk1_seqnum=0;                // sequence number (extracted from packet header)

  // +++ 2014-10-03 MCW change this temporarily for testing with Suzaku
  double tiunit_factor=1./64.;     // factor to convert TI value to seconds
//  double tiunit_factor=1./4096.;     // factor to convert TI value to seconds

  long long offset;                // GPS offset keyword value in HK file
  int ticycle=0;                   // tmppow rounded to integer
  double starti=-1.;               // starting time of GPS-on interval
  double stopi=-1.;                // stopping time of GPS-on interval (inclusive)
  long long irow=1;                // row loop counter
  long long nrow=0;                // number of rows in input HK file
  long long prev_ti=0;             // previous L32TI in GPS HDU
  double prev_stime=0;             // previous S_TIME in GPS HDU
  CharVector prev_status(LENSTATUS);   // previous STATUS in GPS HDU
  for (int is=0; is < LENSTATUS; is++) prev_status[is]=0;
  int prev_seqnum=0;               // previous sequence number
  int prev_gpsmode=GPS_UNDEF;      // GPS mode
  double prev_time=0.;             // previous TIME value
  unsigned long ftidx=0;           // search position in freq vs temp CALDB file
  int mjdrefi=0;                   // MJDREFI keyword value
  double mjdreff=0.;               // MJDREFF keyword value
  double tstart=0.;                // output TSTART value for TIM file
  double tstop=0.;                 // output TSTOP value for TIM file
  std::string dateobs;             // string for DATE-OBS keyword
  std::string dateend;             // string for DATE-END keyword

  // local variables to connect with columns in TIM file
  double tim_ti=0;                            // L32TI
  double tim_time=0.;                         // TIME
  CharVector tim_status(LENSTATUS);           // STATUS
  for (int is=0; is < LENSTATUS; is++) tim_status[is]=0;
  std::string timecolumn="TIME";              // name of TIME column
  std::string statuscol="STATUS";             // name of STATUS column
  ahfits::IndexType num_status=LENSTATUS;     // length of STATUS column

  // local variables to connect with GTI columns
  double gti_start=0.;
  double gti_stop=0.;

  // initialize counters
  int rowno = 0;
  int syncno = 0;
  int unsyncno = 0;
  int suzregno = 0;
  int illegalno = 0;
  int gtino = 0;

  // The packet_header column is 20 bytes which is too long to fit into
  // a single data type.  However, only bits 18-31 are needed (giving the
  // sequence number).  
  unsigned char hk1_ph_ptr[20];
  for (int ii=0; ii < 20; ii++) hk1_ph_ptr[ii]=0;

  // set up local variables and router for input HK file with GPS data
  nrow=ahfits::numRows(fphk1);
  ahfits::Router rout_hk1(fphk1);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_l32ticol,hk1_ti);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_stimecol,hk1_stime);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_gpsacol,hk1_gpsa);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_gpsbcol,hk1_gpsb);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_gpsccol,hk1_gpsc);
  rout_hk1.connectScalar(ahfits::e_READONLY,par.m_gpsdcol,hk1_gpsd);
  rout_hk1.connectFixedLengthArray(ahfits::e_READONLY,par.m_packheadcol,hk1_ph_ptr);

  // set up local variables and router for output TIM file
  ahfits::Router rout_tim(fptim);
  rout_tim.connectScalar(ahfits::e_WRITEONLY,par.m_l32ticol,tim_ti);
  rout_tim.connectScalar(ahfits::e_READWRITE,timecolumn,tim_time);
  rout_tim.connectBit(ahfits::e_WRITEONLY,statuscol,&tim_status[0],num_status);

  // set up local variables for writing GTI intervals
  ahfits::Router rout_gti(gp);
  rout_gti.connectScalar(ahfits::e_WRITEONLY,"START",gti_start);
  rout_gti.connectScalar(ahfits::e_WRITEONLY,"STOP",gti_stop);

  // get Astro-H/GPS offset time (in seconds) from keyword
  offset=(long long)ahfits::getKeyValDbl(fphk1,"GPSOFFET");
  ticycle=26;    // +++ 2014-10-03 MCW  =26 for testing Astro-H data
//  ticycle=20;    // +++ 2014-10-03 MCW temporarily =20 for testing Suzaku data
  AH_INFO(ahlog::LOW) << "GPSOFFET: " << offset << std::endl;

  // loop over HK (GPS) file rows
  ftidx=0;                        // search position in freq vs temp CALDB file
  ahfits::firstRow(fphk1);
  while (ahfits::readOK(fphk1)) {

    double time=0.;                    // TIME
    CharVector status(LENSTATUS);      // STATUS
    for (int is=0; is < LENSTATUS; is++) status[is]=0;
    int gpsmode=GPS_UNDEF;             // GPS mode determined from GPS columns

    ahfits::readRow(fphk1);
    rowno++;

    // Get sequence number from packet header by extracting bits 18-31
    hk1_seqnum=((hk1_ph_ptr[2]&0x3F)<<8)+hk1_ph_ptr[3];

    // determine GPS state; determine STATUS
    gpsmode=getGPSMode(hk1_gpsa,hk1_gpsb,hk1_gpsc,hk1_gpsd,status);
    std::string sgpsmode="";
    if (gpsmode == GPS_SYNCH) 
      sgpsmode=" synchronized";
    else if (gpsmode == GPS_SYNCHTRAN) 
      sgpsmode=" synchronized, transition";
    else if (gpsmode == GPS_SUZAKU) 
      sgpsmode=" unsynchronized (Suzaku)";
    else if (gpsmode == GPS_TRAN) 
      sgpsmode=" transition";
    else if (gpsmode == GPS_ILLEGAL) 
      sgpsmode=" illegal";
    else
      sgpsmode="undefined";
    AH_DEBUG << "Row, GPS mode: " << ahfits::currentRow(fphk1) << ", " << sgpsmode << std::endl;

    // Close active GTI if previous row is GPS_SYNCH/SYNCHTRAN
    if (starti >= 0.) {          // valid GTI started
      if (!(gpsmode == GPS_SYNCH || gpsmode == GPS_SYNCHTRAN)) {
        stopi=prev_time;
        gti_start=starti;
        gti_stop=stopi;
        ahfits::writeRow(gp);
        ahfits::nextRow(gp);
        starti=-1.;
      }
    }

    //  =====  GPS SYNCHRONIZED =====
    if (gpsmode == GPS_SYNCH || gpsmode == GPS_SYNCHTRAN) {

      syncno++;

      // Calculate TIME
      time=calc_time(hk1_ti, hk1_stime, offset, ticycle, tiunit_factor);

      // Set bad STATUS if TIME was not calculated (in case of S_TIME and
      // L32TI being on different cycles).
      if (time < 0.) set_status_illegal(status);

      // Starting new GTI if on row 1 or previous row was not SYNCH/SYNCHTRAN
      if (time > 0.) {      // only start GTI if TIME was calculated
        if (starti < 0.) {
          starti=time;
          gtino++;
        } else if (prev_gpsmode != GPS_SYNCH && prev_gpsmode != GPS_SYNCHTRAN) {
          starti=time;
          gtino++;
        }
      }

      // Check for duplicate point; update STATUS for 2nd point (status)
      if (irow > 1 && hk1_ti == prev_ti) {
        AH_DEBUG << "  Current row has same L32TI value as previous row; flagging as duplicate" << std::endl;
        AH_DEBUG << "  Previous row: S_TIME, L32TI = " << prev_stime << ", " << prev_ti << std::endl;
        AH_DEBUG << "  Current row: S_TIME, L32TI =  " << hk1_stime << ", " << hk1_ti << std::endl;
        set_status_duplicate(status);
        set_status_illegal(status);
      }

      // Write to TIM file
      tim_ti=hk1_ti;
      tim_time=time;
      for (int is=0; is < LENSTATUS; is++) tim_status[is]=status[is];
      ahfits::writeRow(fptim);
      ahfits::nextRow(fptim);

      // Store current row values as PREV
      prev_ti=hk1_ti;
      prev_stime=hk1_stime;
      for (int is=0; is < LENSTATUS; is++) prev_status[is]=status[is];
      prev_time=time;
      prev_gpsmode=gpsmode;
      prev_seqnum=hk1_seqnum;

      // goto next row and continue to next WHILE iteration
      ahfits::nextRow(fphk1);
      irow++;
      continue;

    //  =====  GPS ILLEGAL =====
    } else if (gpsmode == GPS_ILLEGAL) {

      illegalno++;

      // If the GPS flags have an illegal configuration, we set TIME=S_TIME
      // and flag the STATUS accordingly.  The prev_* variables are not 
      // updated to this row.


      // set TIME and STATUS
      time=hk1_stime;
      set_status_illegal(status);

      // write to TIM file
      tim_ti=hk1_ti;
      tim_time=time;
      for (int is=0; is < LENSTATUS; is++) tim_status[is]=status[is];
      ahfits::writeRow(fptim);
      ahfits::nextRow(fptim);

      // goto next row and continue to next WHILE iteration
      ahfits::nextRow(fphk1);
      irow++;
      continue;

    //  =====  GPS SUZAKU =====
    } else if (gpsmode == GPS_SUZAKU || gpsmode == GPS_TRAN) {

      suzregno++;

      // The Suzaku method consists of the following steps:
      //
      // 1. Collect sequence of consecutive points from the GPS extension where
      //    the SMU is not synchronized with GPS.  This sequence is stored in the
      //    arrays: dat_stime[] & dat_l32ti[].  The length of the arrays is stored
      //    in ndat.
      //
      //    a) If possible, the first point of the sequence should be the last
      //       point where the SMU is synchronized.
      //    b) If possible, the last point of the sequence should be the first
      //       point where GPS synchronization is re-established.
      //
      // 2. Look for skip or duplicate regions in the sequence.  The flagging is
      //    recorded in the array: dat_status[], also of length, ndat.  Duplicate
      //    points will not be included in the drift-adjustment calculation.
      //
      // 3. Get list of anchor points.  The first/last anchor point is chosen
      //    to be before/after the sequence of Suzaku-mode points.  Interior
      //    anchor points are taken from the L32TI vs S_TIME table in the 
      //    TIME_PACKETS extension of the input TIM file.  Interior anchor
      //    points are required when the duration of the unsynchronized period is
      //    greater than a time given by the suzdrifttime parameter.  The interior
      //    anchor points will be separated from each other by at least suzdrift
      //    time.
      //
      //    The leading anchor point of the first region will be
      //
      //    d) last point where SMU is sychronized (see 1a), or
      //    e) if the point from 3d has an illegal STATUS, the row in the
      //       TIME_PACKETS table with the largest S_TIME smaller than the first
      //       unsychronized point, or
      //    f) if file begins with SMU not synchronized or 3e fails, the TSTART &
      //       LTISTART keywords from the TIME_PACKETS extension of the input
      //       TIM file, or
      //    g) if all the above fail, the first point of the sequence with a valid
      //       status
      //
      //    Similarly, the trailing anchor point of the last region is
      //
      //    h) first point where SMU is re-sychronized (see 1b), or
      //    i) if the point from 3h has an illegal STATUS, the row in the
      //       TIME_PACKETS table with the smallest S_TIME larger than the last
      //       unsychronized point, or
      //    j) if file ends with SMU not synchronized or 3i fails, the TSTOP & 
      //       LTISTOP keywords from the TIME_PACKETS extension of the input TIM
      //       file, or
      //    k) if all the above fail, the last point of the sequence with a valid
      //       status
      //
      // 4. Split Suzaku-mode points into regions using the anchor points as
      //    boundaries.
      //
      // 5. Process each region in two parts:
      //
      //    a. Compute the running drift correction of the L32TI values based on
      //       the quartz clock frequency variation with temperature.  In this
      //       step, the HK_HCE extension of the input file is searched for the
      //       temperature corresponding to S_TIME.  Then, that temperature is 
      //       used to search the frequency vs. temperature CALDB table to get a
      //       clock frequency.
      //    b. Linearly shift the region so that the first and last region points
      //       lie on the line between the leading and trailing anchor points.
      //


      // STEP 1 - collect sequence of unsynchronized points
      
      AH_DEBUG << "Suzaku mode step 1: Collect unsynchronized points" <<std::endl;

      // initialize arrays to hold collection of Suzaku-mode rows
      // note: ideally first and last rows will contain GPS_SYNCH data
      int ndat=0;                            // length of sequence
      DblVector dat_l32ti;
      DblVector dat_stime;
      IntVector dat_gpsmode;
      IntVector dat_seqnum;
      StatusVector dat_status;
      CharVector blank_status(LENSTATUS);      // empty status for single event
      for (int is=0; is < LENSTATUS; is++) blank_status[is]=0;

      // if PREV is GPS_SYNCH, store it as first point
      if (prev_gpsmode == GPS_SYNCH || prev_gpsmode == GPS_SYNCHTRAN) {
        ndat++;
        dat_l32ti.push_back((double)prev_ti);
        dat_stime.push_back((double)prev_time);
        dat_gpsmode.push_back(prev_gpsmode);
        dat_seqnum.push_back(prev_seqnum);
        dat_status.push_back(blank_status);
        for (int is=0; is < LENSTATUS; is++) dat_status[ndat-1][is]=prev_status[is];
      }

      // store current row in sequence
      unsyncno++;
      ndat++;
      dat_l32ti.push_back((double)hk1_ti);
      dat_stime.push_back((double)hk1_stime);
      dat_gpsmode.push_back(gpsmode);
      dat_seqnum.push_back(hk1_seqnum);
      dat_status.push_back(blank_status);
      for (int is=0; is < LENSTATUS; is++) dat_status[ndat-1][is]=status[is];

      // read rows until non-Suzaku row found
      while(1) {

        // set previous
        prev_ti=hk1_ti;
        prev_stime=hk1_stime;
        for (int is=0; is < LENSTATUS; is++) prev_status[is]=status[is];
        prev_gpsmode=gpsmode;
        prev_seqnum=hk1_seqnum;

        // read next row; get GPS mode and set status
        ahfits::nextRow(fphk1);
        irow++;
        if (!ahfits::readOK(fphk1)) break;   // EOF
        ahfits::readRow(fphk1);
        gpsmode=getGPSMode(hk1_gpsa,hk1_gpsb,hk1_gpsc,hk1_gpsd,status);

        // Get sequence number from packet header by extracting bits 18-31
        hk1_seqnum=((hk1_ph_ptr[2]&0x3F)<<8)+hk1_ph_ptr[3];

        // store current
        ndat++;
        dat_l32ti.push_back((double)hk1_ti);
        dat_stime.push_back((double)hk1_stime);
        dat_gpsmode.push_back(gpsmode);
        dat_seqnum.push_back(hk1_seqnum);
        dat_status.push_back(blank_status);
        for (int is=0; is < LENSTATUS; is++) dat_status[ndat-1][is]=status[is];

        // no longer Suzaku-mode?
        if (gpsmode != GPS_SUZAKU && gpsmode != GPS_TRAN && gpsmode != GPS_ILLEGAL) break;
        unsyncno++;
      }

      AH_DEBUG << "Size of Suzaku-mode group: " << ndat << std::endl;

      // set STATUS flags if first or last point is not GPS_SYNCH
      if (has_status_illegal(dat_status[0]) || 
          (dat_gpsmode[0] != GPS_SYNCH && dat_gpsmode[0] != GPS_SYNCHTRAN)) {
        AH_DEBUG << "Suzaku-mode set of did not start with valid GPS_SYNCH" << std::endl;
        set_status_badend(dat_status[0]);
      }
      if (has_status_illegal(dat_status[ndat-1]) ||
          (dat_gpsmode[ndat-1] != GPS_SYNCH && dat_gpsmode[ndat-1] != GPS_SYNCHTRAN)) {
        AH_DEBUG << "Suzaku-mode set of did not end with valid GPS_SYNCH; cannot adjust slope" << std::endl;
        set_status_badend(dat_status[ndat-1]);
      }

      // if last point is a valid GPS synch point, then it will be the last
      // anchor point.
      bool have_last_anchor=false;
      double last_anchor_time=-1.;
      double last_anchor_l32ti=-1.;
      if (!has_status_illegal(dat_status[ndat-1]) &&
          (dat_gpsmode[ndat-1] == GPS_SYNCH || dat_gpsmode[ndat-1] == GPS_SYNCHTRAN)) {
        last_anchor_l32ti=dat_l32ti[ndat-1];
        last_anchor_time=calc_time(last_anchor_l32ti, dat_stime[ndat-1], offset, ticycle, tiunit_factor);
        if (last_anchor_time > 0.) {
          have_last_anchor=true;
          AH_DEBUG << "Last point of Suzaku region is synchronized with GPS" << std::endl;
          AH_DEBUG << "  S_TIME, L32TI = " << dat_stime[ndat-1] << ", " << dat_l32ti[ndat-1] << std::endl;
          AH_DEBUG << "  calculated TIME = " << last_anchor_time << std::endl;
        }
      }


      // STEP 2 - flag skip and duplicate points
      
      AH_DEBUG << "Suzaku mode step 2: Flag skip and duplicate regions" <<std::endl;

      // check for skip region, update STATUS as necessary
      // note: cannot operate on 1st row
      int prev_i=0;           // keep track of previous legal i value
      for (int i=1; i < ndat; i++) {
        if (has_status_illegal(dat_status[i])) continue;
        // calculate difference in sequence number; if large negative, then
        // assume that max(seqnum) was reached add add offset
        int dseqnum=dat_seqnum[i]-dat_seqnum[prev_i];
        int MAXSEQNUM=16383;     // 2**14 - 1     // +++ 2014-12-04 MCW move this constant somewhere else
        while (dseqnum < 0) dseqnum+=MAXSEQNUM;   // if sequence number resets, then dseqnum will be negative   // +++ not in TRF
        double chk=(dat_l32ti[i]-dat_l32ti[prev_i]);
        if (chk > par.m_gaptime*dseqnum) {
          set_status_skip(dat_status[i]);
          AH_DEBUG << "Skip region found between Suzaku points " << prev_i << " and " << i << std::endl;
          AH_DEBUG << "  seqnum[i], seqnum[i+1] = " << dat_seqnum[prev_i] << ", " << dat_seqnum[i] << std::endl;
          AH_DEBUG << "  dseqnum, MAXSEQNUM = " << dseqnum << ", " << MAXSEQNUM << std::endl;
          AH_DEBUG << "  L32TI i, i+1: " << dat_l32ti[prev_i] << ", " << dat_l32ti[i] << std::endl;
          AH_DEBUG << "  delta-L32TI, gaptime*dseqnum: " << chk << ", " << par.m_gaptime*dseqnum << std::endl;
        }
        prev_i=i;
      }

      // check for duplication region, update STATUS as necessary
      int duplicate_start=-1;    // mark if in duplicate region; -1 = not duplicate region, >0 idx of suzaku-mode points at start of region
      prev_i=0;                  // keep track of previous legal i value
      for (int i=1; i < ndat; i++) {
        if (has_status_illegal(dat_status[i])) continue;
        if (duplicate_start < 0) {
          if (dat_l32ti[i] < dat_l32ti[prev_i]) duplicate_start=prev_i;
        } else {
          if (dat_l32ti[i] > dat_l32ti[duplicate_start]) {
            set_status_duplicate(dat_status[duplicate_start]);
            for (int j=duplicate_start+1; j < i; j++) {
              if (has_status_illegal(dat_status[j])) continue;
              set_status_duplicate(dat_status[j]);
              set_status_illegal(dat_status[j]);
            }
            AH_DEBUG << "Duplicate region found between " << duplicate_start << " and " << i << std::endl;
            duplicate_start=-1;
          }
        }
        prev_i=i;
      }

      // STEP 3 - get list of anchor points

      AH_DEBUG << "Suzaku mode step 3: Get list of anchor points" <<std::endl;

      // leading anchor variables
      double first_time=0.;
      double first_l32ti=0.;

      // decide where the leading anchor point comes from 
      //
      //  have TIM?      row#       1st STATUS       1st synch?       method
      //     no           any          any             any              3g - first point
      //     yes            1          any             any              3f - TIM header
      //     yes           >1          bad             any              3e - TIM table
      //     yes           >1         good             yes              3d - first point
      //     yes           >1         good              no              3e - TIM table
      //
      std::string method="";
      if (timepackets.m_nrows == 0) {   // TIME_PACKETS HDU empty or not present in TIM file
        method="FIRSTPOINT";            // 3g
        AH_DEBUG << "No rows in TIME_PACKETS HDU; use first point as leading anchor point" << std::endl;
      } else if (irow == 1) {           // if input starts in Suzaku-mode, use header
        method="TIMHEADER";             // 3f
        AH_DEBUG << "HK GPS starts in Suzaku-mode; use TSTART/TSTOP in TIM header as leading anchor point" << std::endl;
      } else if (has_status_illegal(dat_status[0])) {
        method="TIMTABLE";              // 3e
        AH_DEBUG << "First point has an illegal status; get leading anchor point from TIME_PACKETS table" << std::endl;
      } else if (dat_gpsmode[0] == GPS_SYNCH || dat_gpsmode[0] == GPS_SYNCHTRAN) {
        method="FIRSTPOINT";            // 3g
        AH_DEBUG << "First point has GPS synchronized; use first group point as leading anchor" << std::endl;
      } else {
        method="TIMTABLE";              // 3e
        AH_DEBUG << "First point is not GPS synchronized; get leading anchor point from TIME_PACKETS table" << std::endl;
      }

      // Note: Three separate if-statements are used below for the method types
      // instead of a single if-else block.  The reason is that the TIMTABLE
      // method needs to be able to fallback to the TIMHEADER method upon
      // failure.

      // use first point with a valid status
      if (method == "FIRSTPOINT") {
        int i=0;
        for (i=0; i < ndat; i++) {
          if (!has_status_illegal(dat_status[i])) break;
        }
        if (i >= ndat-1) AH_THROW_RUNTIME("could not find valid starting point for region");
        first_time=dat_stime[i];
        first_l32ti=dat_l32ti[i];
      }

      if (method == "TIMTABLE") {
        // Search the TIME_PACKETS table for the row with the largest S_TIME
        // below dat_stime[0]; if no rows qualify, then use TIME_PACKETS header.
        if (timepackets.m_stime[0] > dat_stime[0]) {  // no rows in TIME_PACKETS before start of region
          method="TIMHEADER";
          AH_DEBUG << "No TIME_PACKETS rows before start of Suzaku-mode group; use TSTART/TSTOP from TIM header instead" << std::endl;
        } else {
          int i=0;
          while (i < timepackets.m_nrows && (timepackets.m_stime[i] < dat_stime[0])) i++;
          first_time=timepackets.m_stime[i-1];
          first_l32ti=timepackets.m_l32ti[i-1];
        }
      }

      if (method == "TIMHEADER") {
        // Take TSTART and LTISTART keyword values from TIME_PACKETS
        first_time=timepackets.m_tstart;
        first_l32ti=timepackets.m_l32tistart;
      }

      AH_DEBUG << "Leading anchor point: S_TIME, L32TI = " << first_time << ", " << first_l32ti << std::endl;

      // Determine remaining anchor points
      AH_DEBUG << "Determine remaining anchor points based on the suzdrifttime parameter." <<std::endl;
      DblVector anchor_time;
      DblVector anchor_l32ti;
      BoolVector anchor_skiplastadj;
      anchor_time.push_back(first_time);
      anchor_l32ti.push_back(first_l32ti);
      anchor_skiplastadj.push_back(false);
      int iseq=0;                             // current index in sequence
      while (iseq < ndat-1) {

        double last_time=0.;
        double last_l32ti=0.;

        // proceed through sequence until end or anchor point needed
        double stimefirst=dat_stime[iseq];
        while (iseq < ndat-1) {
          iseq++;
          if (dat_stime[iseq] - stimefirst >= par.m_suzdrifttime) break;
        }
        if (ndat-1-iseq < 10) iseq=ndat-1;      // +++2014-11-26 MCW prevent small group from happening - define 10 as a constant
        AH_DEBUG << "  Look for anchor point near iseq, S_TIME[iseq]: " << iseq << ", " << dat_stime[iseq] << std::endl;

        // determine method for getting trailing anchor point
        //
        //  have TIM?      EOF?      last STATUS      last synch?       method
        //     no           any          any             any              3k - last point
        //     yes          yes          any             any              3j - TIM header
        //     yes           no          bad             any              3i - TIM table
        //     yes           no         good             yes              3h - last point
        //     yes           no         good              no              3i - TIM table
        //
        method="";
        if (timepackets.m_nrows == 0) {
          method="LASTPOINT";
          AH_DEBUG << "  No rows in TIME_PACKETS HDU; use last point in group as trailing anchor point" << std::endl;
        } else if (iseq == nrow) {
          method="TIMHEADER";
          AH_DEBUG << "  Last point in group is the last point in HK GPS; use TSTART/TSTOP in TIM header as leading anchor point" << std::endl;
        } else if (has_status_illegal(dat_status[iseq])) {
          method="TIMTABLE";
          AH_DEBUG << "  Last point in group has an illegal value; get trailing anchor point from TIME_PACKETS table" << std::endl;
        } else if (dat_gpsmode[iseq] == GPS_SYNCH || dat_gpsmode[iseq] == GPS_SYNCHTRAN) {
          method="LASTPOINT";
          AH_DEBUG << "  Last point in group is synchronized; use last point as trailing anchor point" << std::endl;
        } else {
          method="TIMTABLE";
          AH_DEBUG << "  Last point in group is unsynchronized; get trailing anchor point from TIME_PACKETS table" << std::endl;
        }

        // Modify LASTPOINT method if file ends in Suzaku-mode
        bool skiplastadj=false;      // if true, skip slope adjustment to end point (still adjust based on first point)
        if ((method == "LASTPOINT") && (iseq == nrow)) {
          if (dat_gpsmode[iseq] != GPS_SYNCH && dat_gpsmode[iseq] != GPS_SYNCHTRAN) {
            skiplastadj=true;
            AH_DEBUG << "  Last row in HK GPS is in Suzaku-mode; only adjust slope of Suzaku-mode points to leading anchor point (not trailing anchor)" << std::endl;
          }
        }

        // Note: Three separate if-statements are used below for the method types
        // instead of a single if-else block.  The reason is that the TIMTABLE
        // method needs to be able to fallback to the TIMHEADER method upon
        // failure.

        if (method == "LASTPOINT") {
          last_time=dat_stime[iseq];
          last_l32ti=dat_l32ti[iseq];
          AH_DEBUG << "  Assign trailing anchor point using last valid Suzaku-mode point" << std::endl;

          // if last point is GPS-synch, need to do the GPS time calculation
          if (dat_gpsmode[iseq] == GPS_SYNCH || dat_gpsmode[iseq] == GPS_SYNCHTRAN) {
            AH_DEBUG << "  Trailing point is GPS-synchronized; perform GPS time calculation" << std::endl;
            last_time=calc_time(dat_l32ti[iseq], dat_stime[iseq], offset, ticycle, tiunit_factor);
            if (last_time > 0.) {
              AH_DEBUG << "  S_TIME, L32TI, TIME: " << dat_stime[iseq] << ", " << last_l32ti << ", " << last_time << std::endl;
            } else {
              AH_DEBUG << "  Error in TIME calculation (S_TIME/L32TI cycle mis-match); try to get last point from TIME_PACKETS table in TIM file" << std::endl;
              method="TIMTABLE";
            }
          }
        }
  
        if (method == "TIMTABLE") {
          // Search the TIME_PACKETS table for the row with the smallest S_TIME
          // above dat_stime[iseq]; if no rows qualify, then use TIME_PACKETS header.
          if (timepackets.m_stime[timepackets.m_nrows-1] < dat_stime[iseq]) {  // no rows in TIME_PACKETS after end of region
            method="TIMHEADER";
            AH_DEBUG << "  No TIME_PACKETS rows after start of Suzaku-mode group; use TSTART/TSTOP from TIM header instead" << std::endl;
          } else {
            int i=0;
            while (i < timepackets.m_nrows && (timepackets.m_stime[i] < dat_stime[iseq])) i++;
            last_time=timepackets.m_stime[i];
            last_l32ti=timepackets.m_l32ti[i];
          }
        }
  
        if (method == "TIMHEADER") {
          // Take TSTOP and LTISTOP keyword values from TIME_PACKETS
          last_time=timepackets.m_tstop;
          last_l32ti=timepackets.m_l32tistop;
        }

        // if last anchor point is greater in time than the GPS anchor point,
        // then substitute the latter.
        bool done=false;
        if (have_last_anchor) {
          if (last_time > last_anchor_time) {
            std::stringstream msg;
            msg << "  Changing last anchor from (TIME,L32TI) =  (" << std::setprecision(15)  << last_time << ", " << last_anchor_l32ti << ") to (" << last_anchor_time << ", " << last_anchor_l32ti << ")" << std::endl;
            AH_DEBUG << msg.str();
            last_time=last_anchor_time;
            last_l32ti=last_anchor_l32ti;
            done=true;         // if setting anchor to final GPS anchor, then no other anchors are needed
          }
        }

        // store anchor point
        {
          std::stringstream msg;
          msg << "Adding anchor point: TIME, L32TI, adjslope = " << std::setprecision(15)  << last_time << ", " << last_l32ti << ", " << skiplastadj << std::endl;
          AH_DEBUG << msg.str();
        }
        anchor_time.push_back(last_time);
        anchor_l32ti.push_back(last_l32ti);
        anchor_skiplastadj.push_back(skiplastadj);

        if (done) break;

        // move to first sequence point past anchor point
        while (iseq < ndat-1) {
          iseq++;
          if (dat_stime[iseq] > last_time) break;
        }
      }
      AH_DEBUG << "Number of anchor points: " << anchor_time.size() << std::endl;


      // STEP 4 - define Suzaku-mode regions

      AH_DEBUG << "Suzaku mode step 4: Define regions based on anchor points" <<std::endl;

      // variables defining regions
      IntVector reg_ifirst;          // dat_* index for start of region
      IntVector reg_ilast;           // dat_* index for stop of region

      // define regions until sequence finished
      int ifirst=0;
      for (int ianchor=1; ianchor < (int)anchor_time.size(); ianchor++) {
        int ilast=ifirst+1;
        while ((ilast < ndat) && (dat_stime[ilast] < anchor_time[ianchor])) ilast++;
        ilast--;
        reg_ifirst.push_back(ifirst);
        reg_ilast.push_back(ilast);
        AH_DEBUG << "Sub-region: ifirst, ilast, size = " << ifirst << ", " << ilast << ", " << ilast-ifirst+1 << std::endl;
        ifirst=ilast+1;        
      }
      reg_ilast[reg_ilast.size()-1]=ndat-1;     // ensure that last region contains last point

      // +++ MCW How to handle small regions?


      // STEP 5 - Process each region
      
      AH_DEBUG << "Suzaku mode step 5: Process each region" <<std::endl;

      //    a. Compute the running drift correction of the L32TI values based on
      //       the quartz clock frequency variation with temperature.  In this
      //       step, the HK_HCE extension of the input file is searched for the
      //       temperature corresponding to S_TIME.  Then, that temperature is 
      //       used to search the frequency vs. temperature CALDB table to get a
      //       clock frequency.
      //    b. Linearly shift the region so that the first and last region points
      //       lie on the line between the leading and trailing anchor points.

      for (unsigned int ireg=0; ireg < reg_ifirst.size(); ireg++) {
        std::stringstream msg;
        msg << "Perform drift integral for group " << ireg+1 << std::endl;
        AH_DEBUG << msg.str();
        msg.str("");   // clear
        msg << "  first point: index, TIME, L32TI = " << reg_ifirst[ireg] << std::setprecision(15) << ", " << dat_stime[reg_ifirst[ireg]] << ", " << dat_l32ti[reg_ifirst[ireg]] << std::endl;
        AH_DEBUG << msg.str();
        msg.str("");   // clear
        msg << "  last point: index, TIME, L32TI = " << reg_ilast[ireg] << std::setprecision(15)  << ", " << dat_stime[reg_ilast[ireg]] << ", " << dat_l32ti[reg_ilast[ireg]] << std::endl;
        AH_DEBUG << msg.str();
        msg.str("");   // clear
        msg << "  leading anchor: TIME, L32TI = " << std::setprecision(15)  << anchor_time[ireg] << ", " << anchor_l32ti[ireg] << std::endl;
        AH_DEBUG << msg.str();
        msg.str("");   // clear
        msg << "  trailing anchor: TIME, L32TI = " << std::setprecision(15)  << anchor_time[ireg+1] << ", " << anchor_l32ti[ireg+1] << std::endl;
        AH_DEBUG << msg.str();
        msg.str("");   // clear
        msg << "  skip trailing slope adjustment? " << anchor_skiplastadj[ireg+1] << std::endl;
        AH_DEBUG << msg.str();
        drift_integral(dat_l32ti,dat_stime,dat_status,
                       reg_ifirst[ireg],reg_ilast[ireg],
                       anchor_l32ti[ireg],anchor_time[ireg],
                       anchor_l32ti[ireg+1],anchor_time[ireg+1],
                       anchor_skiplastadj[ireg+1],hcedat,freqtempdata,ftidx);
        AH_DEBUG << std::endl;
      }

      // write Suzaku-mode points to output TIM file (skip first/last point if GPS_SYNCH)
      AH_DEBUG << "Writing processed Suzaku-mode points to output TIM file" <<std::endl;
      int ibegin=0;
      if (dat_gpsmode[0] == GPS_SYNCH || dat_gpsmode[0] == GPS_SYNCHTRAN) ibegin=1;
      int iend=ndat-1;
      if (dat_gpsmode[iend] == GPS_SYNCH || dat_gpsmode[iend] == GPS_SYNCHTRAN) iend--;
      for (int i=ibegin; i <= iend; i++) {
        tim_time=dat_stime[i];
//        tim_ti=(long long)dat_l32ti[i];
//        tim_ti=tim_ti%4294967296LL;           // +++ 2014-10-03 MCW addition to algorithm; need to put into TRF
        tim_ti=dat_l32ti[i];
        while (tim_ti > 4294967296.) tim_ti-=4294967296.;
        for (int is=0; is < LENSTATUS; is++) tim_status[is]=dat_status[i][is];
        ahfits::writeRow(fptim);
        ahfits::nextRow(fptim);
      }
      AH_DEBUG << "Finished processing Suzaku-mode group" <<std::endl;
    }      // end if (suzaku mode)

  }        // end while loop over rows in input file (HK GPS)
  AH_INFO(ahlog::HIGH) << "Finished processing HK GPS rows" <<std::endl;

  // Write last GTI if necessary
  if (starti >= 0.) {          // valid GTI started
    stopi=prev_time;
    gti_start=starti;
    gti_stop=stopi;
    ahfits::writeRow(gp);
    ahfits::nextRow(gp);
    starti=-1.;
  }

  // get epoch from header of HK file
  mjdrefi=ahfits::getKeyValLLong(fphk1,"MJDREFI");
  mjdreff=ahfits::getKeyValDbl(fphk1,"MJDREFF");
  ahtime::AhMJDTime ttepoch_mjd(mjdrefi,mjdreff);
  ahtime::AhDateTime ttepoch;
  ahtime::reformatMJDAsDateTime(ttepoch_mjd,ttepoch);
  ahtime::AhDateTime epoch;      // as UTC
  ahtime::convertTTToUTC(ttepoch,leapsecdat,epoch);

  // write epoch to TIM and GPS GTI files
  ahfits::writeKeyValLLong(fptim,"MJDREFI",mjdrefi,"");
  ahfits::writeKeyValDbl(fptim,"MJDREFF",mjdreff,"");
  ahfits::writeKeyValLLong(gp,"MJDREFI",mjdrefi,"");
  ahfits::writeKeyValDbl(gp,"MJDREFF",mjdreff,"");

  // write TSTART/TSTOP to TIM and GPS GTI files
  ahfits::firstRow(fptim);
  ahfits::readRow(fptim);
  tstart=tim_time;
  ahfits::lastRow(fptim);
  ahfits::readRow(fptim);
  tstop=tim_time;
  ahfits::writeKeyValDbl(fptim,"TSTART",tstart,"");
  ahfits::writeKeyValDbl(fptim,"TSTOP",tstop,"");
  ahfits::writeKeyValDbl(gp,"TSTART",tstart,"");
  ahfits::writeKeyValDbl(gp,"TSTOP",tstop,"");

  // calculate DATE-OBS/END and write to TIM and GPS GTI files
  // requires leap second table
  dateobs=ahtime::calcDateObs(tstart,epoch,leapsecdat);
  ahfits::writeKeyValStr(fptim,"DATE-OBS",dateobs,"");
  ahfits::writeKeyValStr(gp,"DATE-OBS",dateobs,"");
  dateend=ahtime::calcDateObs(tstop,epoch,leapsecdat);
  ahfits::writeKeyValStr(fptim,"DATE-END",dateend,"");
  ahfits::writeKeyValStr(gp,"DATE-END",dateend,"");

  AH_INFO(ahlog::LOW) << "Output TIM keywords:" <<std::endl;
  AH_INFO(ahlog::LOW) << "  TSTART/TSTOP     : " << tstart << " / " << tstop <<std::endl;
  AH_INFO(ahlog::LOW) << "  DATE-OBS/DATE-END: " << dateobs << " / " << dateend <<std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of HK GPS rows processed:       " << rowno <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of GPS synchronized rows:       " << syncno <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of GPS unsynchronized rows:     " << unsyncno <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows with illegal GPS flags: " << illegalno <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of Suzaku-mode regions:         " << suzregno <<std::endl;
  AH_INFO(ahlog::HIGH) << "Number of GPS synch GTIs:              " << gtino <<std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr fphk1, ahfits::FilePtr fptim, ahfits::FilePtr gp,
              hce::HCE& hcedat, freqtemp::DataType& freqtempdata) {

  ahfits::close(fphk1);
  ahfits::close(fptim);
  ahfits::close(gp);
  hce::closeHCE(hcedat);
  freqtemp::clean(freqtempdata);
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: ahmktim.cxx,v $
 Revision 1.66  2016/07/11 19:48:39  mwitthoe
 ahmktim: add correction for case where the S_TIME and L32TI values in the HK GPS extension are in different L32TI cycles

 Revision 1.65  2016/04/21 17:48:47  rshill
 Added two calls to writeParametersToLog() to main.

 Revision 1.64  2016/04/07 19:40:21  mwitthoe
 ahmktim: change many INFO statements to DEBUG to reduce log file size

 Revision 1.63  2015/12/29 19:53:53  klrutkow
 throw error if no rows in the input file(s)

 Revision 1.62  2015/10/08 18:52:03  mwitthoe
 ahmktim: do not throw error if input HK GPS extension is empty, tool just makes an empty TIM file

 Revision 1.61  2015/10/02 19:33:18  mwitthoe
 ahmktim: switch over to general CALDB resolve function for freq vs temp file

 Revision 1.60  2015/10/02 18:28:15  mwitthoe
 ahmktim: switch CALDB query over to general function

 Revision 1.59  2015/07/30 16:20:19  mwitthoe
 ahmktim: stamp parameters to log file

 Revision 1.58  2015/07/29 21:01:02  mwitthoe
 ahmktim: add column descriptions and copy keywords to new output extensions

 Revision 1.57  2015/07/08 16:03:20  asargent
 Clean up of code, new log statements, new counters

 Revision 1.56  2015/07/06 17:20:17  mdutka
 updated to use common frqtem library function

 Revision 1.55  2015/07/01 15:19:47  mdutka
 using extended syntax now

 Revision 1.54  2015/06/30 20:38:23  mdutka
 Implementing CALDB query

 Revision 1.53  2015/06/22 17:01:16  mwitthoe
 ahmktim: change L32TI offset constant to double to avoid type overflow build errors on 32-bit machines: L32TI max = 4294967296.

 Revision 1.52  2015/06/12 17:09:37  mwitthoe
 ahmktim: add Suzaku-mode comment that was accidentally removed during the last revision

 Revision 1.51  2015/06/10 18:09:45  mwitthoe
 ahmktim: fix Suzaku-mode operation to remove discontinuties in TIME vs L32TI curve; changed output L32TI column from J to D

 Revision 1.49  2015/06/03 23:09:30  mwitthoe
 ahmktim: debugging from funcd testing

 Revision 1.48  2015/05/12 18:51:44  mwitthoe
 ahmktim: 1) fix seg fault when number of Suzaku mode points exceed the internal dimension... a error is now thrown; 2) fix infinite loop when GPS times are larger than largest HCE time... now return last HCE row in that case

 Revision 1.47  2015/04/03 17:54:52  mwitthoe
 ahmktim: change boolean parameters to use lowercase yes/no; convert timfile parameter to uppercase before checking if NONE

 Revision 1.46  2015/03/03 15:22:45  mwitthoe
 ahmktim: 1) fix bug where AnchorData struct was not initialized properly; 2) fix bug where anchor points are sought even when no input TIM file is present; 3) fix bug in slope adjustment when no TIM file is present; 4) fix bug where same anchor point was being used twice; remove debug print statements

 Revision 1.45  2015/01/06 20:49:52  mwitthoe
 ahmktim: parameter changes; see issue 472

 Revision 1.44  2014/12/22 16:11:54  mwitthoe
 ahmktim: update tool to include anchor points from the TIME_PACKETS extension of the input TIM file when computing TIME in Suzaku-mode; see issue 457

 Revision 1.43  2014/09/10 03:57:24  mwitthoe
 ahmktim: switch to new leap second library

 Revision 1.42  2014/08/19 21:14:10  mwitthoe
 ahmktim: do not check units of L32TI column; do not use 1st extension of input tim file as template for output extension; create new output file from scratch if no input TIM file provided; see issue 421

 Revision 1.41  2014/08/05 12:18:07  mwitthoe
 ahmktim: make a few implicit type conversions explicit to avoid compiler error

 Revision 1.40  2014/01/22 18:28:23  mwitthoe
 ahmktim: update documentation

 Revision 1.39  2014/01/21 21:31:17  mwitthoe
 ahmktim: revise according to code review; issue 331

 Revision 1.38  2014/01/09 20:09:55  asargent
 Added CR comments pertaining to std main()

 Revision 1.37  2013/12/02 22:55:22  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.36  2013/11/21 16:59:43  mwitthoe
 ahmktim: add (and call) destructor for freqtemp structure

 Revision 1.35  2013/11/14 22:57:43  mwitthoe
 ahmktim: rename freqtemp library to ahmktimlib; move variable declarations to top of scope; change STATUS column from I type to 10X

 Revision 1.34  2013/10/07 13:57:26  mwitthoe
 ahmktim: switch to using new ahfits connection functions (issue 270)

 Revision 1.33  2013/09/17 19:31:30  mwitthoe
 ahmktim tool: switch over to using new ahtime library functions (see issue 290)

 Revision 1.32  2013/09/11 15:25:23  mwitthoe
 ahmktim: remove references to empty strings to go to the first extension of a FITS file; this is in preparation for the redefinition of empty string to represent the primary HDU

 Revision 1.31  2013/07/30 18:50:44  mwitthoe
 ahmktime: remove unnecessary include statement to ahgen

 Revision 1.30  2013/07/18 18:19:25  mwitthoe
 ahmktim: add code to create output GTI file from scratch

 Revision 1.29  2013/07/01 13:59:00  mwitthoe
 ahmktim tool: switch to using version of ahfits::cloneSingleHDU which does not return a FITS pointer; instead use the function which has the output pointer listed as an argument

 Revision 1.28  2013/04/18 21:04:38  mwitthoe
 use new ahtime::calcDateObs() function in the ahmktim tool to calculate DATE-OBS/DATE-END

 Revision 1.27  2013/04/17 15:40:29  mwitthoe
 ahmktim tool: remove dependence on ahgti library (tool cannot yet create GTI files, though); add check for EOF for writing final GTI; add check for TSTART/TSTOP ranges not overlapping between input TIM and HK files

 Revision 1.26  2013/04/12 13:31:43  mwitthoe
 update ahmktim tool to use new version of leap second library (now in ahmission)

 Revision 1.25  2013/04/08 21:05:35  mwitthoe
 ahmktim tool: modify freqtemp CALDB library to remove staticly-loaded data, finish load() function, and add getFrequency() function; re-order functions in main tool source to put standard functions at top

 Revision 1.24  2013/04/05 15:40:35  mwitthoe
 ahmktim: incorporate local freqtemp CALDB library; use read/write flags in ahfits connections

 Revision 1.23  2013/03/26 14:38:12  mwitthoe
 updated standard main for ahmktim tool (issue 230)

 Revision 1.22  2013/03/08 21:25:30  mwitthoe
 ahmktim: extensive changes throughout to bring code into alignment with TRF from Japanese workshop in Feb/Mar (LA, YT, MW)

 Revision 1.21  2013/02/19 19:58:30  mwitthoe
 change gti to GTI in comments of ahmktim

 Revision 1.20  2013/01/20 20:44:45  mwitthoe
 update ahmktim according to checklist; tosee checklist for numerous changes

 Revision 1.19  2012/12/28 18:05:12  mwitthoe
 change ahmktim task to use new ahgti library

 Revision 1.18  2012/12/18 14:43:25  mwitthoe
 fix typos in ahmktim tool; missing semi-colon and missing/wrong arguments for ahfits::columnRange()

 Revision 1.17  2012/12/17 21:12:53  mwitthoe
 ahmktim: add timextname to parameter file; use new ahmission/ahgti library to create GTI file; get AstroH-GPS offset time and max L32TI values from header values instead of library constants

 Revision 1.16  2012/12/10 19:27:22  mwitthoe
 make timing tools up-to-date with recent changes to ahfits

 Revision 1.15  2012/12/03 20:28:20  mwitthoe
 in doxygen description of ahtime and ahmktime, change tags surrounding parameter list from code/endcode to verbatim/endverbatim in order to prevent accidentally syntax highlighting

 Revision 1.14  2012/11/29 20:58:09  mwitthoe
 add algorithm for GPS-transition-mode in ahmktim; tweak how GPS gti file is created

 Revision 1.13  2012/11/26 20:55:34  mwitthoe
 update doxygen description for the ahmktim tool

 Revision 1.12  2012/11/15 03:01:37  mwitthoe
 change timing tools to use new ahcaldb libraries

 Revision 1.11  2012/11/14 18:38:51  mwitthoe
 ahmktim: now use 4 GPS flags to determine GPS state; read GPS and temperature data from different extensions in HK file

 Revision 1.10  2012/11/13 18:34:50  mwitthoe
 change timing tools to use new open/create/clone functions from ahtime

 Revision 1.9  2012/11/04 23:28:05  mwitthoe
 make timing tools (ahtime, ahmktim, ahtrendtemp) consistent with new ahfits clone functions

 Revision 1.8  2012/11/01 20:52:20  mwitthoe
 conform timing tools to new version of ahfits: mainly change to how clobber to handled

 Revision 1.7  2012/11/01 14:39:37  mwitthoe
 time tools now access clobber state from ahgen, not ahapp

 Revision 1.6  2012/11/01 00:04:38  mwitthoe
 change ahmktim to use new version of ahlookup

 Revision 1.5  2012/10/31 19:57:32  mwitthoe
 ahmktim: open files in initialize() instead of doWork()

 Revision 1.4  2012/10/25 16:48:00  mwitthoe
 use new parameter retrieval functions from ahapp

 Revision 1.3  2012/10/25 01:40:07  mwitthoe
 removed cloneTIMFile() from ahtime library, but code from that function directly into the ahmktim tool

 Revision 1.2  2012/10/24 18:59:35  mwitthoe
 finished preparing ahmktim for Japanese visit

 Revision 1.1  2012/10/22 21:13:38  mwitthoe
 add ahmktim tool (incomplete and untested)


*/

