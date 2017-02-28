/// \file ahmktimlib.cxx
/// \brief Functions for ahmktim
/// \author Mike Witthoeft
/// \date $Date: 2016/12/19 17:16:01 $
 
#define AHLABEL tool_ahmktim_ahmktimlib
#define AHCVSID "$Id: ahmktimlib.cxx,v 1.18 2016/12/19 17:16:01 mwitthoe Exp $"

#include "ahmktimlib.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"

#include <cmath>        // std::abs
#include <sstream>
#include <iomanip>     // std::setprecision

// =============================================================================

int getGPSMode(char gpsa, char gpsb, char gpsc, char gpsd, CharVector& status) {

  // reset status bits                                           STATUS
  for (int is=0; is < LENSTATUS; is++) status[is]=0;

  if (gpsb && gpsc && gpsa && gpsd) {                        // 0000000000
    return GPS_SYNCH;                                      
                                                           
  } else if (!gpsa && !gpsb && !gpsc) {                      // 0010000000
    status[2]=1;                                           
    return GPS_SUZAKU;                                     
                                                           
  } else if (!gpsa && gpsb && gpsc && !gpsd) {               // 0010000000
    status[2]=1;                                           
    return GPS_SUZAKU;                                     
                                                           
  } else if (!gpsa && !gpsb && gpsc) {                       // 0100000000
    status[1]=1;                                           
    return GPS_TRAN;                                       
                                                           
  } else if (!gpsa && gpsb && gpsc && gpsd) {                // 0000000000
    return GPS_SYNCHTRAN;                                  
                                                           
  } else if (!gpsa && gpsb && !gpsc) {                       // 1000000001
    status[0]=1;                                           
    status[9]=1;                                           
    return GPS_ILLEGAL;                                    
                                                           
  } else if (!gpsa && gpsb && gpsc && !gpsd) {               // 1000000010
    status[0]=1;                                           
    status[8]=1;                                           
    return GPS_ILLEGAL;                                    
                                                           
  } else if (gpsa && !(gpsb && gpsc && gpsd) ) {             // 1000000100
    status[0]=1;
    status[7]=1;
    return GPS_ILLEGAL;
  }

  // should not get here
  std::stringstream msg;
  msg << "missing GPSMode for gpsa, gpsb, gpsc, gpsd: " << gpsa << ", "
      << gpsb << ", " << gpsc << ", " << gpsd;
  AH_THROW_LOGIC(msg.str());

}

// =============================================================================

void set_status_illegal(CharVector& status) {
  status[0]=1;      // set 1xxxxxxxxx
}

// =============================================================================

bool has_status_illegal(CharVector& status) {
  return (status[0] == 1);
}

// =============================================================================

void set_status_skip(CharVector& status) {
  status[3]=1;    // set xxx1xxxxxx
  status[4]=0;    // set xxxx0xxxxx
}

// =============================================================================

void set_status_duplicate(CharVector& status) {
  status[3]=1;    // set xxx1xxxxxx
  status[4]=1;    // set xxxx1xxxxx
}

// =============================================================================

void set_status_badend(CharVector& status) {
  status[5]=1;    // set xxxxx1xxxx
}

// =============================================================================

double calc_time(long long l32ti, double stime, long long offset,
                int cycle, double tifactor) {

  long long tmp=(long long)(offset+stime);
  tmp=tmp>>cycle;    // number of complete TI cycles
  tmp=tmp<<cycle;    // convert back into seconds
  double time=(double)(tmp)-(double)offset+tifactor*(double)l32ti;

  if ( (long long)(time-stime)>>(cycle-1) > 0) {   // S_TIME in later cycle than L32TI
    tmp=(long long)(offset+stime);
    tmp=(tmp>>cycle)-1L;    // correct for cycle mis-match
    tmp=tmp<<cycle;
    time=(double)(tmp)-(double)offset+tifactor*(double)l32ti;
  } else if ((long long)(stime-time)>>(cycle-1) > 0) {   // L32TI in later cycle than S_TIME
    tmp=(long long)(offset+stime);
    tmp=(tmp>>cycle)+1L;    // correct for cycle mis-match
    tmp=tmp<<cycle;
    time=(double)(tmp)-(double)offset+tifactor*(double)l32ti;
  }

  // Check if TIME and S_TIME are still further than half of a cycle apart
  // indicating that L32TI and S_TIME at least two cycles apart.  Do not try
  // to correct, just return an illegal TIME.
  if ( (long long)std::abs(time-stime)>>(cycle-1) > 0) time=-1.;

  return time;
}

// =============================================================================

void drift_integral(DblVector& dat_l32ti, DblVector& dat_stime,
                    StatusVector& dat_status, int ifirst, int ilast,
                    double first_l32ti, double first_time, 
                    double last_l32ti, double last_time, bool skiplastadj,
                    hce::HCE& hcedat, freqtemp::DataType& freqtempdata, 
                    unsigned long ftidx) {

  // 64 Hz is the nominal frequency of TI (counts/sec) +++ 2014-12-12 MCW should be defined elsewhere
  double freq0=64.;

  // other variables
  double dtisum=0.;           // store integration result
  double hk1_temp=0.;         // temperature assigned to GPS row (current)
  double prev_temp1=0.;       // temperature assigned to GPS row (previous)
  int prev_i=-1;              // keep track of last legal i value
  int istart=-1;              // first legal i value
  double temp_avg=0.;         // average temperature from two HK GPS rows
  double freq=0.;             // frequency corresponding to temp_avg
  double dt=0.;               // delta S_TIME used in drift integral
  double dat_l32ti_first=0.;  // store first L32TI value before slope adjustment
  double slope1=0.;           // slope from drift-corrected points
  double slope2=0.;           // slope from anchor points
  double oldline=0.;          // single value on drift-corrected line
  double newline=0.;          // single value on anchor line

  // perform drift integral
  // note: dat_l32ti values will be replaced with the TI_drift values
  for (int i=ifirst; i <= ilast; i++) {

    // skip if illegal point
    if (has_status_illegal(dat_status[i])) {
      AH_DEBUG << "  idx = " << i << ": skipping point with illegal STATUS" << std::endl;
      continue;
    }

    // set first legal i value -- istart
    if (istart < 0) istart=i;

    // search for temperature of point using HK_HCE extension
    hk1_temp=hce::searchHCE(hcedat,dat_stime[i]);
    AH_DEBUG << "  idx = " << i << ": search HCE HDU for temperature: S_TIME, TEMP = " << dat_stime[i] << ", " << hk1_temp << std::endl;

    // do integral for all points except the 1st
    if (i > ifirst) {
      temp_avg=0.5*(hk1_temp+prev_temp1);
      freq=freqtemp::getFrequency(temp_avg,ftidx,freqtempdata);
      AH_DEBUG << "    Lookup frequency: AVG TEMP, FREQ = " << temp_avg << ", " << freq << std::endl;
      dt=dat_stime[i]-dat_stime[prev_i];
      dtisum+=dt*(freq-freq0);
      AH_DEBUG << "    L32TI0, dL32TI, new L32TI = " << dat_l32ti[i] << ", " << dtisum << ", " << dat_l32ti[i]+dtisum << std::endl;
      dat_l32ti[i]=dat_l32ti[i]+dtisum;
    }

    // store temperature as PREV and end loop
    prev_temp1=hk1_temp;
    prev_i=i;
  }

  // if skiplastadj is set, then only adjust slope based on first_* arguments, not
  // last_* arguments
  if (skiplastadj) {
    last_l32ti=dat_l32ti[ilast];
    last_time=dat_stime[ilast];
  }

  // adjust slope if last point is GPS_SYNCH (and STATUS is legal)
  dat_l32ti_first=dat_l32ti[ifirst];   // need to copy this since dat_l32ti is being updated
  double dat_stime_first=dat_stime[ifirst];
  std::stringstream msg;
  msg << "  Data points for slope calculation: L32TI first/last, S_TIME first/last = " << std::setprecision(15) << dat_l32ti_first << ", " << dat_l32ti[ilast] << ", " << dat_stime[ifirst] << ", " << dat_stime[ilast] << std::endl;
  AH_DEBUG << msg.str();
  msg.str("");    // clear stream
  msg << "  Anchor points for slope calculation: L32TI first/last, S_TIME first/last = " << std::setprecision(15) << first_l32ti << ", " << last_l32ti << ", " << first_time << ", " << last_time << std::endl;
  AH_DEBUG << msg.str();
  slope1=(dat_stime[ilast]-dat_stime[ifirst])/(dat_l32ti[ilast]-dat_l32ti_first);
  slope2=(last_time-first_time)/(last_l32ti-first_l32ti);
  for (int i=ifirst; i <= ilast; i++) {
    if (has_status_illegal(dat_status[i])) continue;
    oldline=dat_stime_first+slope1*(dat_l32ti[i]-dat_l32ti[ifirst]);
    newline=first_time+slope2*(dat_l32ti[i]-first_l32ti);
    dat_stime[i]=dat_stime[i]-oldline+newline;
    std::stringstream msg;
    msg << "i, S_TIME, adj L32TI: " << i << ", " << std::setprecision(15) << dat_stime[i] << ", " << dat_l32ti[i] << std::endl;
    AH_DEBUG << msg.str();
  }
  AH_DEBUG << "Adjust slope of group: initial/final slope = " << slope1 << ", " << slope2 << std::endl;

}

// =============================================================================

void create_gti_file(const std::string& filename, ahfits::FilePtr& gp) {
  ahfits::create(filename,"",&gp);
  ahfits::addEmptyTbl(gp,"GTIGPS");
  ahfits::writeKeyValStr(gp,"TELESCOP",ahmission::getTELESCOPString(),"");
  ahfits::insertColAfter(gp,"START","1D","");
  ahfits::setColumnDescription(gp,"START","ahmktim");
  ahfits::insertColAfter(gp,"STOP","1D","");
  ahfits::setColumnDescription(gp,"STOP","ahmktim");
}

// =============================================================================

void add_tim_extension(ahfits::FilePtr fp, const std::string& extname,
                       const std::string& smuunit) {
  ahfits::addEmptyTbl(fp,extname);
  ahfits::insertColAfter(fp,"TIME","1D","");
  ahfits::setTUnit(fp,"TIME","s");
  ahfits::setColumnDescription(fp,"TIME","ahmktim");
  ahfits::insertColAfter(fp,"L32TI","1D","");
  ahfits::setColumnDescription(fp,"L32TI","ahmktim");
  ahfits::insertColAfter(fp,"STATUS","10X","");
  ahfits::setColumnDescription(fp,"STATUS","ahmktim");

  // add SMUUNIT keyword
  ahfits::writeKeyValStr(fp,"SMUUNIT",smuunit,"");

}

// =============================================================================

namespace hce {

// ---------------------------------------------------------------------------

void setupHCE(const std::string& filename, const std::string& extname, 
              const std::string& stimecolumn, const std::string& tempcolumn,
              HCE& hcedat) {

  // reset structure, if already being used
  hce::closeHCE(hcedat);

  // open FITS file and make connections to local variables
  ahfits::open(filename,extname,&hcedat.m_ahffp);
  hcedat.m_routptr=new ahfits::Router(hcedat.m_ahffp);
  hcedat.m_routptr->connectScalar(ahfits::e_READONLY,stimecolumn,hcedat.m_stime);
  hcedat.m_routptr->connectScalar(ahfits::e_READONLY,tempcolumn,hcedat.m_temp);

  // read first two rows to prepare for search
  ahfits::firstRow(hcedat.m_ahffp);
  ahfits::readRow(hcedat.m_ahffp);
  hcedat.m_prev_stime=hcedat.m_stime;
  hcedat.m_prev_temp=hcedat.m_temp;
  ahfits::nextRow(hcedat.m_ahffp);
  ahfits::readRow(hcedat.m_ahffp);
  ahfits::nextRow(hcedat.m_ahffp);   // prepare for reading next row

}

// ---------------------------------------------------------------------------

double searchHCE(HCE& hcedat, double stime) {

  // check if set-up
  if (hcedat.m_routptr == 0 || hcedat.m_ahffp == 0)
    AH_THROW_LOGIC("cannot search for TEMPERATURE; HCE structure not set up");

  // want to read rows until hcedat.m_prev_stime <= stime <= hcedat.m_stime
  while (hcedat.m_stime < stime) {
    hcedat.m_prev_stime=hcedat.m_stime;
    hcedat.m_prev_temp=hcedat.m_temp;
    ahfits::readRow(hcedat.m_ahffp);    
    ahfits::nextRow(hcedat.m_ahffp);
    if (!ahfits::readOK(hcedat.m_ahffp)) break;
  }

  if (stime > hcedat.m_stime)
    AH_DEBUG << "HK GPS S_TIME (" << stime << ") larger than last HK HCE S_TIME (" << hcedat.m_stime << ")" << std::endl;

  // return TEMPERATURE with closer S_TIME
  double out=hcedat.m_temp;
  if ( std::abs(stime-hcedat.m_prev_stime) < std::abs(hcedat.m_stime-stime) )
    out=hcedat.m_prev_temp;
  return out;

}

// ---------------------------------------------------------------------------

void closeHCE(HCE& hcedat) {
  if (hcedat.m_routptr != 0) delete hcedat.m_routptr, hcedat.m_routptr=0;
  if (hcedat.m_ahffp != 0) ahfits::close(hcedat.m_ahffp);
}

// ---------------------------------------------------------------------------

}  // namespace hce


// =============================================================================


namespace freqtemp {

// ---------------------------------------------------------------------------

/// \callgraph
void load(const std::string & filename, DataType & dat) {

  // store column values from CALDB file
  double l_freq=0.;
  double l_temp=0.;

  // make sure dataset is empty
  clean(dat);
  
  // open file
  ahfits::FilePtr fptr;
  
  ahfits::open(filename,"",&fptr);
  if (ahfits::isPrimary(fptr)) {
    // move to first binary table if extended syntax not used in filename
    ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);  
  }

  if (!ahfits::readOK(fptr)) {
    ahfits::close(fptr);
    AH_THROW_RUNTIME("failed to open frequency vs. temperature FITS file: "+filename);
  }

  // allocate memory
  dat.m_size=ahfits::numRows(fptr);
  dat.m_temp=new double[dat.m_size];
  dat.m_freq=new double[dat.m_size];

  // make connections between FITS columns and local variables
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"FREQ",l_freq);
  router.connectScalar(ahfits::e_READONLY,"TEMP",l_temp);

  // read file
  long irow=0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    dat.m_freq[irow]=l_freq;
    dat.m_temp[irow]=l_temp;
    irow++;
  }

  // close FITS file
  ahfits::close(fptr);

  // print result
  AH_INFO(ahlog::LOW) << "Frequency vs. temperature CALDB file loaded with "
                      << dat.m_size << " points." << std::endl;

}

// ---------------------------------------------------------------------------

void clean(DataType & dat) {
  if (dat.m_temp != 0) delete [] dat.m_temp, dat.m_temp=0;
  if (dat.m_freq != 0) delete [] dat.m_freq, dat.m_freq=0;
  dat.m_size=0;
}

// ---------------------------------------------------------------------------

double getFrequency(double temp, unsigned long idx, DataType & dat) {
  bool extrap=false;
  idx=ahmath::search(temp,dat.m_temp,dat.m_size,extrap,idx);
  if (extrap)
    AH_INFO(ahlog::LOW) << "*** extrapolating freq vs. temp data with a "
                        << "temperature of " << temp << std::endl;
  return ahmath::interpolate(temp,dat.m_temp,dat.m_freq,dat.m_size,
                             ahmath::TWOPOINT,idx);
}

// ---------------------------------------------------------------------------

}  // namespace freqtemp


// =============================================================================


/* Revision Log
 $Log: ahmktimlib.cxx,v $
 Revision 1.18  2016/12/19 17:16:01  mwitthoe
 ahmktim: fix double-free failure which sometimes occurs when building with gcc 5.4 with optimization turned on

 Revision 1.17  2016/07/11 19:48:39  mwitthoe
 ahmktim: add correction for case where the S_TIME and L32TI values in the HK GPS extension are in different L32TI cycles

 Revision 1.16  2016/04/07 19:40:21  mwitthoe
 ahmktim: change many INFO statements to DEBUG to reduce log file size

 Revision 1.15  2015/07/29 21:01:02  mwitthoe
 ahmktim: add column descriptions and copy keywords to new output extensions

 Revision 1.14  2015/07/08 19:00:57  asargent
 Changed AH_WARN to AH_INFO

 Revision 1.13  2015/07/08 16:03:20  asargent
 Clean up of code, new log statements, new counters

 Revision 1.12  2015/07/01 15:19:47  mdutka
 using extended syntax now

 Revision 1.11  2015/06/30 20:38:23  mdutka
 Implementing CALDB query

 Revision 1.10  2015/05/12 19:46:44  mwitthoe
 ahmktim: add warning message when a GPS S_TIME value is larger than the last HCE S_TIME value

 Revision 1.9  2015/05/12 18:51:44  mwitthoe
 ahmktim: 1) fix seg fault when number of Suzaku mode points exceed the internal dimension... a error is now thrown; 2) fix infinite loop when GPS times are larger than largest HCE time... now return last HCE row in that case

 Revision 1.8  2014/12/22 16:11:54  mwitthoe
 ahmktim: update tool to include anchor points from the TIME_PACKETS extension of the input TIM file when computing TIME in Suzaku-mode; see issue 457

 Revision 1.7  2014/09/12 20:56:06  mwitthoe
 ahmktim: allow extended syntax for frequency/temperature CALDB file

 Revision 1.6  2014/04/01 17:37:45  mwitthoe
 ahmktim: change connect() to connectScalar() in ahmktimlib.cxx

 Revision 1.5  2014/01/21 21:31:17  mwitthoe
 ahmktim: revise according to code review; issue 331

 Revision 1.4  2014/01/14 18:48:10  rshill
 Code review comments

 Revision 1.3  2013/11/21 16:59:43  mwitthoe
 ahmktim: add (and call) destructor for freqtemp structure

 Revision 1.2  2013/11/20 23:53:14  mwitthoe
 ahmktim: update ahmktimlib to use new version of ahmath library which uses C arrays instead of std::vectors (see issue 315)

 Revision 1.1  2013/11/14 22:57:43  mwitthoe
 ahmktim: rename freqtemp library to ahmktimlib; move variable declarations to top of scope; change STATUS column from I type to 10X



 BELOW IS THE REVISION LOG FOR freqtemp.cxx BEFORE RENAMING TO ahmktimlib.cxx

 Revision 1.5  2013/09/11 18:44:45  mwitthoe
 ahmktim: remove references to empty strings to go to the first extension of a FITS file; this is in preparation for the redefinition of empty string to represent the primary HDU

 Revision 1.4  2013/04/08 21:05:35  mwitthoe
 ahmktim tool: modify freqtemp CALDB library to remove staticly-loaded data, finish load() function, and add getFrequency() function; re-order functions in main tool source to put standard functions at top

 Revision 1.3  2013/04/05 15:40:35  mwitthoe
 ahmktim: incorporate local freqtemp CALDB library; use read/write flags in ahfits connections

 Revision 1.2  2012/12/10 18:13:32  mwitthoe
 in ahcaldb, revert to using old versions of open/create which return void instead of FilePtr

 Revision 1.1  2012/11/15 02:24:23  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.2  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.1  2012/10/24 18:02:30  mwitthoe
 add CALDB libary for frequency vs. temperature data (missing algorithm for loading/retrieving data


*/
