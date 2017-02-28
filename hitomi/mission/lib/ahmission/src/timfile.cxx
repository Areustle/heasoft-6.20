/// \file timfile.cxx
/// \brief read lookup tables from TIM file
/// \author Mike Witthoeft
/// \date $Date: 2015/06/03 22:57:01 $

#define AHLABEL ahmission_timfile
#define AHCVSID "$Id: timfile.cxx,v 1.2 2015/06/03 22:57:01 mwitthoe Exp $"

#include "ahmission/timfile.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <string>

namespace ahmission {

namespace timfile {

// ---------------------------------------------------------------------------

bool isTimStatusIllegal(char* status) {
  // STATUS is a 10 bit quantity where the leading bit indicates if the STATUS
  // is legal (=0) or illegal (=1)
  if (status[0] == 1) return true;
  return false;
}

// ---------------------------------------------------------------------------

void initTimFileData1(TimFileData& timdat, long size1) {
  // delete any allocated data
  cleanTimFileData(timdat);

  timdat.m_size1=size1;
  timdat.m_time1=new double[size1];
  timdat.m_l32ti1=new double[size1];
  timdat.m_map=new long[size1];

  timdat.m_status1=new char*[size1];
  for (int is=0; is < size1; is++) 
    timdat.m_status1[is]=new char[LENTIMSTATUS];
}

// ---------------------------------------------------------------------------

void initTimFileData2(TimFileData& timdat, long size2) {
  // delete table 2 data, if allocated
  if (timdat.m_time2 != 0) delete [] timdat.m_time2;
  if (timdat.m_l32ti2 != 0) delete [] timdat.m_l32ti2;
  timdat.m_time2=0;
  timdat.m_l32ti2=0;

  timdat.m_size2=size2;
  timdat.m_time2=new double[size2];
  timdat.m_l32ti2=new double[size2];
}

// ---------------------------------------------------------------------------

void cleanTimFileData(TimFileData& timdat) {
  timdat.~TimFileData();   // call destructor
}

// ---------------------------------------------------------------------------

void loadTimFile(const std::string& filename, TimFileData& timdat) {

  // extension and column names in TIM file
  std::string timhdu="TIM_LOOKUP";
  std::string time_col="TIME";
  std::string l32ti_col="L32TI";
  std::string status_col="STATUS";

  // local variables to connect to TIM file columns
  double l_l32ti=0.;                            // L32TI column
  double l_time=0.;                             // TIME column
  char l_status[LENTIMSTATUS]={0};              // STATUS column
  ahfits::IndexType num_status=LENTIMSTATUS;    // number of STATUS bits 

  // other variables
  ahfits::FilePtr fptim=0;     // ahfits FilePtr
  long nrows=0;                // number of rows in file
  long size2=0;                // size of second lookup table

  // open TIM file and get number of rows (allow for extended syntax in 
  // filename to override TIME_PACKETS)
  ahfits::open(filename,"",&fptim);
  if (ahfits::isPrimary(fptim)) ahfits::move(fptim,timhdu);
  nrows=ahfits::numRows(fptim);
  if (nrows < 4) 
    AH_THROW_RUNTIME("need at least 4 points in TIM file: "+filename);

  // read tstart and tstop keywords
  timdat.m_tstart=ahfits::getKeyValDbl(fptim,"TSTART");
  timdat.m_tstop=ahfits::getKeyValDbl(fptim,"TSTOP");

  // allocate space for TIM data in structure
  initTimFileData1(timdat,nrows);

  // make connections between local variables and FITS columns
  ahfits::Router routtim(fptim);
  routtim.connectScalar(ahfits::e_READONLY,time_col,l_time);
  routtim.connectScalar(ahfits::e_READONLY,l32ti_col,l_l32ti);
  routtim.connectBit(ahfits::e_READONLY,status_col,l_status,num_status);

  // read TIM data into structure
  ahfits::firstRow(fptim);
  for (long irow=0; irow < nrows; irow++) {
    ahfits::readRow(fptim);
    timdat.m_time1[irow]=l_time;
    timdat.m_l32ti1[irow]=l_l32ti;
    for (int is=0; is < LENTIMSTATUS; is++)
      timdat.m_status1[irow][is]=l_status[is];
    ahfits::nextRow(fptim);
  }
  ahfits::close(fptim);

  // Construct new set of TIM data that omits any rows with an illegal
  // STATUS.  Create a map of indices from the whole set (tim1_time) to 
  // the legal subset (tim2_time).  An index not appearing in the subset
  // will be assigned a value of zero.  This 2nd set of TIM data will be 
  // used in the interpolation operation.

  // Step 1: count how many rows there are in the second lookup table
  //  and create index map between two lookup tables
  size2=0;
  for (long irow=0; irow < nrows; irow++) {
    if (isTimStatusIllegal(timdat.m_status1[irow])) {
      timdat.m_map[irow]=-1;
    } else {
      timdat.m_map[irow]=size2;
      size2++;
    }
  }

  // Step 2: allocate second lookup table
  initTimFileData2(timdat,size2);

  // Step 3: fill second lookup table using map
  for (long irow=0; irow < nrows; irow++) {
    long idx2=timdat.m_map[irow];
    if (idx2 < 0) continue;    // illegal status
    timdat.m_time2[idx2]=timdat.m_time1[irow];
    timdat.m_l32ti2[idx2]=timdat.m_l32ti1[irow];
  }

}

// ---------------------------------------------------------------------------

}  // namespace timefile

}  // namespace ahmission


/* Revision Log
 $Log: timfile.cxx,v $
 Revision 1.2  2015/06/03 22:57:01  mwitthoe
 use TIM_LOOKUP extension instead of TIME_PACKETS in TIM file for time assignment

 Revision 1.1  2014/09/10 02:30:35  mwitthoe
 ahmission library: add timfile CALDB library (moved from ahtime library); remove leap second library (moved to ahtime library)

 Revision 1.2  2013/11/21 15:38:57  mwitthoe
 ahtime library: timfile: make a destructor for TIM data struct

 Revision 1.1  2013/11/20 23:03:49  mwitthoe
 ahtime library: add library for reading the TIM file needed for time assignment (ahtime and mxstime tools); remove obsolete testing code for old (ancient) TIM file library


*/
