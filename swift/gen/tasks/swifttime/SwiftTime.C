//----------------------------------------------------------------------
//
//  File:        SwiftTime.C
//  Programmer:  Arnold Rots  -  USRA
//  Date:        31 October 1995
//  Description: Code for SwiftTime, SwiftTimeRange, STRList classes
//    modified 24Feb2004 by M. Tripicco for Swift from XTETime.C
//
//----------------------------------------------------------------------
//

#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include "SwiftTime.h"
#define TAIUTC "tai-utc.dat" // link to /FTP/xte/calib_data/clock/tai-utc.dat"

using namespace std;

const double SwiftTime::MJD0        = 2400000.5 ;  // JD - MJD
const double SwiftTime::MJDREF      = 51910.0 ;    // MJD at 2001.0
const double SwiftTime::TT2UT       = 64.184 ;     // UTC - TT at 2001.0
int    SwiftTime::NUMLEAPSECS       = 0 ;
double SwiftTime::LEAPSECS[]        = {} ;
const int    SwiftTime::REFYEAR     = 2001 ;       // generalizing the MET zeropoint
const int    SwiftTime::LEAPYR1     = 2004 ;       // 1st leap yr after REFYEAR

static int daymonth[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} ;
static const char * const month[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"} ;

//
//   ---------------------------------------------------------
// -- SwiftTime::SwiftTime (double tt, TimeSys ts, TimeFormat tf) --
//   ---------------------------------------------------------
//

// Description:
// Constructor: most general constructor
// Default for tf is SECS
SwiftTime::SwiftTime (double tt, TimeSys ts, TimeFormat tf)
  : t (tt), timeZero (0.0)
{
  setleaps() ;
  set (tt, ts, tf) ;
  return ;
}

//
//   ----------------------------------------------------------
// -- SwiftTime::SwiftTime (char *date, TimeSys ts, TimeFormat tf) --
//   ----------------------------------------------------------
//

// Description:
// Constructor: create from a date string
// Default for ts is UTC, for tf is DATE
SwiftTime::SwiftTime (const char *date, TimeSys ts, TimeFormat tf)
  : t (0.0), timeZero (0.0)
{
  setleaps() ;
  set (date, ts, tf) ;
  return ;
}
//
//   --------------------------
// -- SwiftTime::setleaps (void) --
//   --------------------------
//

// Description:
// Function to set leap second table
void SwiftTime::setleaps (void)
{
  if ( !NUMLEAPSECS ) {
    FILE *FF ;
    static char taiutc[1024];
    if ( getenv("TIMING_DIR") ) {
      sprintf(taiutc,"%s/%s",getenv("TIMING_DIR"),TAIUTC);
    }else if ( getenv("LHEA_DATA") ) {
      sprintf(taiutc,"%s/%s",getenv("LHEA_DATA"),TAIUTC);
    }else{
      cerr << "TIMING_DIR and LHEA_DATA are undefined - ignoring leap seconds" << endl;
    }
    if ( FF = fopen (taiutc, "r") ) {
      long leapsMD ;
      while ( fscanf (FF, "%*d %*s  1 =JD 24%ld.5 %*s %*lg S + (MJD - %*lg) X %*lg %*s",
		      &leapsMD) == 1 ) {
	leapsMD -= (long) (MJDREF + 0.1) ;

	if ( leapsMD > 0 ) {
	  LEAPSECS[NUMLEAPSECS] = leapsMD * 86400 + 1 + NUMLEAPSECS ;
	  NUMLEAPSECS++ ;
	}
      }
      fclose (FF) ;
    }
    else{
      cerr << "Failed to open " << taiutc << " - ignoring leap seconds" << endl;
      NUMLEAPSECS = 0 ;          // Leap seconds prior to MJDREF are in TT2UT 
    }
  }
  return ;
}

//
//   -----------------------------------------------------
// -- SwiftTime::set (double tt, TimeSys ts, TimeFormat tf) --
//   -----------------------------------------------------
//

// Description:
// General set function
void SwiftTime::set (double tt, TimeSys ts, TimeFormat tf)
{
  switch (tf) {
  case JD:
    tt -= MJD0 ;
  case MJD:
    tt -= MJDREF ;
    tt *= 86400.0 ;
  case SECS:
    break ;
  default:                //  Error; do nothing
    return ;
  }

  switch (ts) {
  case MET:
    break ;
  case TT:
    tt -= TT2UT ;
    break ;
  case UTC:
    {
      for (int i=0; i<NUMLEAPSECS; i++)
        if ( tt >= LEAPSECS[i] )
          tt++ ;
    }
    break ;
  default:
    return ;
  }
  t = tt ;
  return ;
}

//
//   ------------------------------------------------------
// -- SwiftTime::set (char *date, TimeSys ts, TimeFormat tf) --
//   ------------------------------------------------------
//

// Description:
// General set function from a date string
void SwiftTime::set (const char *date, TimeSys ts, TimeFormat tf)
{
  int year, day, hour, minute;
  double second ;
  int n ;
  int m = 0 ;
  char mn[4] ;

  switch (tf) {
  case DATE:
    n = sscanf (date, "%d:%d:%d:%d:%lg", &year, &day, &hour, &minute, &second) ;
    if ( n != 5 )
      return ;
    break ;
  case CALDATE:
    n = sscanf (date, "%d%c%c%c%d at %d:%d:%lg",
    &year, mn, mn+1, mn+2, &day, &hour, &minute, &second) ;
    if ( n != 8 )
      return ;
    if ( year%4 )
      daymonth[1] = 28 ;
    else
      daymonth[1] = 29 ;
    mn[0] = toupper(mn[0]) ;
    mn[1] = tolower(mn[1]) ;
    mn[2] = tolower(mn[2]) ;
    mn[3] = 0 ;
    while ( strcmp(mn, month[m]) ) {
      if ( m > 11 )
  return ;
      day += daymonth[m++] ;
    }
    break ;
  default:
    return ;
  }
  day += (year - REFYEAR) * 365 - 1 ;
  if (year < REFYEAR) {
    day += (year - LEAPYR1) / 4 ;
  }else{
    day += (year - (LEAPYR1 - 3)) / 4 ;
  }
  second = (double) day * 86400 + hour * 3600 + minute * 60 + second ;
  set (second, ts, SECS) ;

  return ;
}

//
//   -----------------
// -- SwiftTime::monDay --
//   -----------------
//

// Description:
// Convert UTC or TT date string to calendar date string
const char *SwiftTime::monDay (const char *date) {
  char d[32] ;
  int year, day ;
  int m = 0 ;

  strcpy (d, date) ;
  sscanf (d, "%d:%d", &year, &day) ;
  if ( year%4 )
    daymonth[1] = 28 ;
  else
    daymonth[1] = 29 ;

  while ( day > daymonth[m] ) {
    day -= daymonth[m] ;
    m++ ;
  }
  sprintf (tdate, "%04d%s%02d at ", year, month[m], day) ;
  strcpy (tdate+13, d+9) ;
  return ( tdate ) ;
}

//
//   ------------------------------------------
// -- SwiftTime::get (TimeSys ts, TimeFormat tf) --
//   ------------------------------------------
//

// Description:
// Generalized time return function
double SwiftTime::get (TimeSys ts, TimeFormat tf) const {
  double tt ;
  switch (ts) {
  case UTC:
    tt = getUTC () ;
    break ;
  case TT:
    tt = getTT () ;
    break ;
  case MET:
    tt = getMET () ;
    break ;
  }
  switch (tf) {
  case MJD:
    tt = tt / 86400.0 + MJDREF ;
    break ;
  case JD:
    tt = tt / 86400.0 + MJDREF + MJD0 ;
    break ;
  default:
    break ;
  }
  return tt ;
}


//
//   ------------------------
// -- SwiftTime::getUTC (void) --
//   ------------------------
//

// Description:
// Return UTC seconds
double SwiftTime::getUTC (void) const {
  double tt = t + timeZero ;
  for (int i=NUMLEAPSECS-1; i>=0; i--)
    if ( tt >= LEAPSECS[i] )
      tt-- ;
  return tt ;
}

//
//   ----------------------------------------------
// -- SwiftTime::getDate (TimeSys ts, TimeFormat tf) --
//   ----------------------------------------------
//

// Description:
// Generalized date string return function
const char *SwiftTime::getDate (TimeSys ts, TimeFormat tf) {
  double tt = get (ts, SECS) ;
  int year, day, hour, minute, second, millisec ;
  int i = (4 - (LEAPYR1 - REFYEAR)) % 4; // Good grief!

  second = (int) (tt + 0.0005) ; // Note: floor() rounding assumed
  millisec = (int) ((tt + 0.0005 - second) * 1000.0) ;
  if (millisec < 0)
    millisec = 0 ;
  else if (millisec >= 1000)
    millisec = 999;
  day = second / 86400 ;
  second %= 86400 ;
  hour = second / 3600 ;
  second %= 3600 ;
  minute = second / 60 ;
  second %= 60 ;
  year = REFYEAR ;
  day++ ;
  while ( day > 365 ) {
    if ( !i ) {
      if ( day == 366 )
  break ;
      else
  day-- ;
    }
    day -= 365 ;
    year++ ;
    i = (i+1)%4 ;
  }

  sprintf (tdate, "%4d:%03d:%02d:%02d:%02d.%03d",
	   year, day, hour, minute, second, millisec) ;
  switch (ts) {
  case TT:
    strcat (tdate, "TT") ;
    break ;
  case UTC:
    strcat (tdate, "UTC") ;
    break ;
  case MET:
    strcat (tdate, "MET") ;
    break ;
  default:
    break ;
  }

  if ( tf == CALDATE )
    return ( monDay (tdate) ) ;
  else
    return tdate ;
}

//
//   ---------------------------
// -- SwiftTimeRange::setEmpty () --
//   ---------------------------
//

// Description:
// Determine whether range is empty
void SwiftTimeRange::setEmpty (void) {
  double t1=start.getMET() ;
  double t2=stop.getMET() ;
  if ( ( t1 >= t2 ) || ( t1 <= 0.0 ) || ( t2 <= 0.0 ) )
    empty = 1 ;
  else
    empty = 0 ;
  return ;
}

//
//   -----------------------------
// -- SwiftTimeRange::printRange () --
//   -----------------------------
//

// Description:
// A two-liner in UTC date format
void SwiftTimeRange::printRange (void) {
  cout << "---SwiftTimeRange - Empty: " << empty
       << ", Start: " << start.getMET() << " (" << UTStartDate () << ")\n"
       << "                       "
       << "  Stop:  " << stop.getMET() << " (" << UTStopDate () << ")\n" ;
  return ;
}
//
//   --------------------------------
// -- SwiftTimeRange::printRangeCal () --
//   --------------------------------
//

// Description:
// A two-liner in UTC calendar date format
void SwiftTimeRange::printRangeCal (void) {
  cout << "---SwiftTimeRange - Empty: " << empty
       << ", Start: " << start.getMET() << " (" << start.UTCalDate () << ")\n"
       << "                       "
       << "  Stop:  " << stop.getMET() << " (" << stop.UTCalDate () << ")\n" ;
  return ;
}

//
//   -----------------------
// -- STRList::printList () --
//   -----------------------
//

// Description:
// Print list contents in UTC calendar date format
void STRList::printList (void) {
  cout << "\nSTRList - Empty: " << empty << ", Number of ranges:: " << numSTRs
       << ", List range:\n" ;
  listRange.printRange () ;
  if ( numSTRs ) {
    cout << "Member ranges:\n" ;
    for (int i=0;i<numSTRs;i++)
      tr[i].printRange () ;
  }
  return ;
}

//
//   --------------------------
// -- STRList::printListCal () --
//   --------------------------
//

// Description:
// Print list contents in UTC calendar date format
void STRList::printListCal (void) {
  cout << "\nSTRList - Empty: " << empty << ", Number of ranges:: " << numSTRs
       << ", List range:\n" ;
  listRange.printRangeCal () ;
  if ( numSTRs ) {
    cout << "Member ranges:\n" ;
    for (int i=0;i<numSTRs;i++)
      tr[i].printRangeCal () ;
  }
  return ;
}

//
//   ----------------------------
// -- STRList::STRList (STRList) --
//   ----------------------------
//

// Description:
// Copy constructor for a new TR list
STRList::STRList (const STRList &trl)
{
  numSTRs = trl.numSTRs ;
  listRange = trl.listRange ;
  empty = trl.empty ;
  tr = new SwiftTimeRange[numSTRs] ;
  for (int i=0; i<numSTRs; i++)
    tr[i] = trl.tr[i] ;
  return ;
}

//
//   ------------------------------
// -- STRList::operator= (STRList) --
//   ------------------------------
//

// Description:
// Copy operator for a TR list
STRList& STRList::operator= (const STRList &trl)
{
  delete [] tr ;
  numSTRs = trl.numSTRs ;
  listRange = trl.listRange ;
  empty = trl.empty ;
  tr = new SwiftTimeRange[numSTRs] ;
  for (int i=0; i<numSTRs; i++)
    tr[i] = trl.tr[i] ;
  return *this ;
}


//
//   -------------------------------------
// -- STRList::STRList (STRList, STRList) --
//   -------------------------------------
//

// Description:
// Construct a new TR list by "AND"ing two existing lists
STRList::STRList (const STRList &trl1, const STRList &trl2)
  : numSTRs (1), empty (1), tr (0) {

//  Trivial cases: if one of them is empty, the result is empty

  if ( trl1.isEmpty() || trl2.isEmpty() ) {
    numSTRs = 1 ;
    empty = 1 ;
    tr = new SwiftTimeRange () ;
    listRange = *tr ;
    return ;
  }

//  To minimize work, make sure the second one is the shortest

  const STRList *list1 = &trl1 ;
  const STRList *list2 = &trl2 ;
  int nlist1 = list1->numSTRs ;
  int nlist2 = list2->numSTRs ;
  if ( nlist1 < nlist2 ) {
    int i = nlist2 ;
    nlist2 = nlist1 ;
    nlist1 = i ;
    list1 = list2 ;
    list2 = &trl1 ;
  }

//  Simple case: second list has only one member

  if ( nlist2 == 1 ) {
    STRList scratchlist (*list1) ;
    scratchlist.andRange ( list2->tr[0] ) ;
    numSTRs = scratchlist.numSTRs ;
    listRange = scratchlist.listRange ;
    empty = scratchlist.empty ;
    tr = new SwiftTimeRange[numSTRs] ;
    for (int i=0; i<numSTRs; i++)
      tr[i] = scratchlist.tr[i] ;
    return ;
  }

//  The full works: AND each range in list2 with all of list1
//                  OR the resulting lists

  STRList buildlist ;
  int i ;
  for (i=0;i<nlist2;i++) {
    STRList scratchlist (*list1) ;
    scratchlist.andRange ( list2->tr[i] ) ;
    buildlist.orList (scratchlist) ;
  }
  numSTRs = buildlist.numSTRs ;
  listRange = buildlist.listRange ;
  empty = buildlist.empty ;
  tr = new SwiftTimeRange[numSTRs] ;
  for (i=0; i<numSTRs; i++)
    tr[i] = buildlist.tr[i] ;
  return ;
}

//
//   -------------------------------
// -- STRList::isInRange (SwiftTime&) --
//   -------------------------------
//

// Description:
// Return 0 if in range
int STRList::isInRange (const SwiftTime &T) const {
  for (int i=0;i<numSTRs;i++)
    if ( !tr[i].isInRange (T) )
      return 0 ;
  return 1 ;
}

//
//   -----------------------------
// -- STRList::isInRange (double) --
//   -----------------------------
//

// Description:
// Return 0 if in range
int STRList::isInRange (double t) const {
  for (int i=0;i<numSTRs;i++)
    if ( !tr[i].isInRange (t) )
      return 0 ;
  return 1 ;
}

//
//   ------------------------------
// -- STRList::getRange (SwiftTime&) --
//   ------------------------------
//

// Description:
// Return range in which SwiftTime object <T> falls
const SwiftTimeRange *STRList::getRange (const SwiftTime &T) const {
  for (int i=0;i<numSTRs;i++)
    if ( !tr[i].isInRange (T) )
      return tr+i ;
  return NULL ;
}

//
//   ----------------------------
// -- STRList::getRange (double) --
//   ----------------------------
//

// Description:
// Return range in which MET time <t> falls
const SwiftTimeRange *STRList::getRange (double t) const {
  for (int i=0;i<numSTRs;i++)
    if ( !tr[i].isInRange (t) )
      return tr+i ;
  return NULL ;
}

//
//   -----------------------
// -- STRList::totalTime () --
//   -----------------------
//

// Description:
// Return total time (in seconds), covered by the list
double STRList::totalTime (void) const {
  double tt = 0.0 ;
  if ( !empty )
    for (int i=0;i<numSTRs;i++)
      tt += tr[i].totalTime () ;
  return tt ;
}

//
//   -----------------
// -- STRList::orList --
//   -----------------
//

// Description:
// "OR" in another SwiftTime Range List
void STRList::orList (const STRList &trl) {

//  Do nothing if trl is empty

  if ( trl.empty )
    return ;

//  If *this is empty, replace it by trl

  if ( empty ) {
    delete [] tr ;
    numSTRs = trl.numSTRs ;
    listRange = trl.listRange ;
    empty = trl.empty ;
    tr = new SwiftTimeRange[numSTRs] ;
    for (int i=0; i<numSTRs; i++)
      tr[i] = trl.tr[i] ;
  }

//  Do the full thing

  else {
    int n = trl.numSTRs ;
    for (int i=0;i<n;i++)
      orRange ( trl.tr[i] ) ;
  }
  return ;
}

//
//   -----------------
// -- STRList::notList --
//   -----------------
//

// Description:
// Negate a SwiftTime Range List over a specified time range
void STRList::notList (const SwiftTimeRange &T) {

//  If the list was empty, the answer is just T ...

  if ( empty ) {

//  ... unless, of course, T was empty, too, in which case nothing changes

    if ( !T.isEmpty() ) {
      *tr = T ;
      listRange = T ;
      numSTRs = 1 ;
      empty = 0 ;
    }
  }

//  "Regular" case

  else {
    SwiftTimeRange *ntr = new SwiftTimeRange[numSTRs+1] ;
    ntr[0].setStart(1000.0) ;
    for (int i=0; i<numSTRs; i++) {
      ntr[i].setStop(tr[i].TStart()) ;
      ntr[i+1].setStart(tr[i].TStop()) ;
    }
    ntr[numSTRs].setStop(1.0e20) ;
    numSTRs++ ;
    delete [] tr ;
    tr = ntr ;
    setListRange () ;
    andRange (T) ;
  }
  return ;
}


//
//   -------------------
// -- STRList::andRange --
//   -------------------
//

// Description:
// "AND" in an extra SwiftTime Range
void STRList::andRange (const SwiftTimeRange &T) {
  int startin=0, stopin=0 ;
  int startafter=0, stopafter=0 ;
  int zap=0 ;
  int istart, istop ;
  int i ;
  double tstart = T.METStart () ;
  double tstop = T.METStop () ;
//
//  First the trivial cases
//
  if ( empty )
    return ;
  else if ( T.isEmpty () )
    zap = 1 ;
  else if ( ( tstart <= listRange.METStart () ) &&
      ( tstop >= listRange.METStop () ) )
    return ;
  else if ( tstop < listRange.METStart () )
    zap = 1 ;
  else if ( tstart > listRange.METStop () )
    zap = 1 ;
//
//  See where the start and stop times fall in the existing list
//  (add 1 to the indices)
  else {
    for (i=0;i<numSTRs;i++) {
      if ( !startin ) {
	istart = tr[i].isInRange (tstart) ;
	if ( !istart )
	  startin = i + 1 ;
	else if ( istart > 0 )
	  startafter = i + 1 ;
      }
      if ( !stopin ) {
	istop = tr[i].isInRange (tstop) ;
	if ( !istop )
	  stopin = i + 1 ;
	else if ( istop > 0 )
	  stopafter = i + 1 ;
      }
    }
//
//  Now figure out what to do
//    Which range do we start in?
//
    if ( startin ) {
      startin -- ;                     // Correct the index
      tr[startin].setStart (tstart) ;  // Adjust the time
    }

//      In between
    else if ( !stopin && ( startafter == stopafter ) )
      zap = 1 ;

//      Start after
    else
      startin = startafter ;

//    Which range do we stop in?
    if ( !zap ) {
      if ( stopin ) {
	stopin-- ;
	tr[stopin].setStop (tstop) ;
      }

//      Stop after
      else
	stopin = stopafter - 1 ;
    }
  }

//
//  Calculate the new length
  int newNumSTRs ;
  if ( zap ) {
    newNumSTRs = 1 ;
    empty = 1 ;
  }
  else
    newNumSTRs = stopin - startin + 1 ;

//  No change in number of ranges: done
  if ( numSTRs == newNumSTRs ) {
    if ( zap )
      tr->resetRange (0.0, 0.0) ;
    setListRange () ;
    return ;
  }

//
//  Make a new set of ranges
  SwiftTimeRange *newSTR = new SwiftTimeRange[newNumSTRs] ;

//
//    Rearrange the ranges
  if ( !zap ) {
//      Now copy the remaining ones
    int j=0 ;
    for (i=startin;i<=stopin;i++,j++)
      newSTR[j] = tr[i] ;
  }
  else
    newSTR->resetRange (0.0, 0.0) ;

//
//  Exchange the two lists
  delete [] tr ;
  tr = newSTR ;
  numSTRs = newNumSTRs ;
  setListRange () ;

  return ;
}

//
//   ------------------
// -- STRList::orRange --
//   ------------------
//

// Description:
// "OR" in an extra SwiftTime Range
void STRList::orRange (const SwiftTimeRange &T) {
  int startin=0, stopin=0 ;
  int startafter=0, stopafter=0 ;
  int before=0, after=0, between=0, straddle=0 ;
  int istart, istop ;
  int i ;
  double tstart = T.METStart () ;
  double tstop = T.METStop () ;

//
//  Handle the empties first
//
  if ( T.isEmpty () )
    return ;
  if ( empty ) {
    numSTRs = 1 ;
    empty = 0 ;
    tr[0] = T ;
    listRange = T ;
    return ;
  }

//
//  First the trivial cases
//
  if ( ( tstart <= listRange.METStart () ) &&
      ( tstop >= listRange.METStop () ) )
    straddle = 1 ;
  else if ( tstop < listRange.METStart () )
    before = 1 ;
  else if ( tstart > listRange.METStop () )
    after = 1 ;

//
//  See where the start and stop times fall in the existing list
//  (add 1 to the indices)
  else {
    for (i=0;i<numSTRs;i++) {
      if ( !startin ) {
	istart = tr[i].isInRange (tstart) ;
	if ( !istart )
	  startin = i + 1 ;
	else if ( istart > 0 )
	  startafter = i + 1 ;
      }
      if ( !stopin ) {
	istop = tr[i].isInRange (tstop) ;
	if ( !istop )
	  stopin = i + 1 ;
	else if ( istop > 0 )
	  stopafter = i + 1 ;
      }
    }
//
//  Now figure out what to do
//    Which range do we start in?
//
    if ( startin ) {
      if ( startin == stopin )  // If we're stopping in the same one, return
	return ;
      startin -- ;              // Correct the index
    }

//      In between
    else if ( !stopin && ( startafter == stopafter ) )
      between = stopafter ;

//      Somebody's start time needs to be adjusted
    else {
      startin = startafter ;
      tr[startin].setStart (tstart) ;
    }

//    Which range do we stop in?
    if ( stopin )
      stopin-- ;
//      Somebody's stop time needs to be adjusted
    else
      if ( stopafter ) {
	stopin = stopafter - 1 ;
	tr[stopin].setStop (tstop) ;
      }
  }

//
//  The range list must now be non-empty
  empty = 0 ;

//
//  Calculate the new length
  int newNumSTRs ;
  if ( before + after + between )
    newNumSTRs = numSTRs + 1 ;
  else if ( straddle )
    newNumSTRs =  1;
  else
    newNumSTRs = numSTRs - stopin + startin ;

//  No change in number of ranges: done
  if ( numSTRs == newNumSTRs ) {
    if ( straddle )
      tr[0] = T ;
    setListRange () ;
    return ;
  }

//
//  Make a new set of ranges
  SwiftTimeRange *newSTR = new SwiftTimeRange[newNumSTRs] ;

//
//    Extra range before
  if ( before ) {
    newSTR[0] = T ;
    for (i=0;i<numSTRs;i++)
      newSTR[i+1] = tr[i] ;
  }

//
//    Extra range after
  else if ( after ) {
    for (i=0;i<numSTRs;i++)
      newSTR[i] = tr[i] ;
    newSTR[numSTRs] = T ;
  }

//
//    Straddling range
  else if ( straddle )
    newSTR[0] = T ;

//
//    Extra range in between
  else if ( between ) {
    for (i=0;i<between;i++)
      newSTR[i] = tr[i] ;
    newSTR[between] = T ;
    for (i=between;i<numSTRs;i++)
      newSTR[i+1] = tr[i] ;
  }

//
//    Rearrange the ranges
  else {
//      Cover the new part in a single range
    tr[stopin].setStart (tr[startin].METStart()) ;
//      Now copy the remaining ones
    int j=0 ;
    for (i=0;i<startin;i++,j++)
      newSTR[j] = tr[i] ;
    for (i=stopin;i<numSTRs;i++,j++)
      newSTR[j] = tr[i] ;
  }

//
//  Exchange the two lists
  delete [] tr ;
  tr = newSTR ;
  numSTRs = newNumSTRs ;
  setListRange () ;

  return ;
}

//
//   -----------------------
// -- STRList::setListRange --
//   -----------------------
//

// Description:
// Update the list range
void STRList::setListRange (void) {
  int i, j, remove=0 ;

  if ( numSTRs )
    empty = 0 ;
  for (i=0; i<numSTRs; i++)
    if ( tr[i].isEmpty() )
      remove++ ;
  if ( remove ) {
    if ( remove >= numSTRs ) {
      numSTRs = 0 ;
      empty = 1 ;
    }
    else {
      SwiftTimeRange *newSTR = new SwiftTimeRange[numSTRs - remove] ;
      for (i=0, j=0; i<numSTRs; i++)
	if ( !tr[i].isEmpty() )
	  newSTR[j++] = tr[i] ;
      delete [] tr ;
      tr = newSTR ;
      numSTRs -= remove ;
      empty = 0 ;
    }
  }

  if ( !empty )
    listRange.resetRange (tr[0].METStart(), tr[numSTRs-1].METStop()) ;
  else
    listRange.resetRange (0.0, -1.0) ;
}
