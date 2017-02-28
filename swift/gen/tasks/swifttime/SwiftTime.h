//----------------------------------------------------------------------
//
// File Name   : SwiftTime.h
// Programmer  : Arnold Rots, USRA
// Description : SwiftTime, SwiftTimeRange, and STRList classes
//
// .NAME    SwiftTime - SwiftTime, SwiftTimeRange, and STRList classes
// .LIBRARY Util
// .HEADER  Time transformations and manipulations
// .INCLUDE SwiftTime.h
// .FILE    SwiftTime.C
//
// .SECTION Author
//  Arnold Rots,
//  USRA,
//  <arots@xebec.gsfc.nasa.gov>
//     modified 24Feb2004 by M.Tripicco for Swift from XTETime.h
//
// .SECTION Description
// SwiftTime allows transformations between three time systems in five
// formats.  The time systems are MET, TT, and UTC.  The formats:
// seconds (double), MJD (double), JD (double), date string
// ("yyyy:ddd:hh:mm:ssTTT"), and calendar date string
// ("yyyyMondd at hh:mm:ssTTT"); TTT may be any of the time systems.
// A time correction term is optional.
//
// SwiftTimeRange is an aggregate of two SwiftTime objects and an EMPTY indicator.
// A SwiftTimeRange is considered empty if either the start or stop MET is
// non-positive, or if the start time is later than the stop time.
//
// A STRList is a list of SwiftTimeRanges; it includes methods to perform
// logical AND and OR operations between STRLists and between STRLists
// and SwiftTimeRanges.
//
// Note that all instances of SwiftTime will have one private member that
// holds the date strings.  It is the user's responsibility to copy
// the string returned by any of the date string methods, since the
// next call will overwrite it.  Especially, avoid generating more
// than one date string for the same object in a single statement, such as:
//
// cout <<
// "Time : " <<
// t.TTDate <<
// " or " <<
// t.TTCalDate <<
// "\n" ;
//
// Leap seconds are taken from tai-utc.dat.  If not available:
// nothing is done since leapsecs through 1999.0 are in TT2UT already
//
//----------------------------------------------------------------------
//

#ifndef SwiftTIME_H
#define SwiftTIME_H


//
//   ---------
// -- SwiftTime --
//   ---------
//

class SwiftTime {

//*  Private attributes
 
  double t ;                                     // Time is kept in MET seconds
  double timeZero ;                              // Time correction term
  char tdate[32] ;

  static const double MJD0        ;  // JD - MJD
  static const double MJDREF      ;  // MJD at 2001.0 (REFYEAR)
  static const double TT2UT       ;  // TT - UTC at 2001.0 (REFYEAR)
  static int    NUMLEAPSECS       ;  // Number of leap seconds
  static double LEAPSECS[100]     ;  // Leap seconds
  static const int    REFYEAR     ;  // Reference year (2001.0)
  static const int    LEAPYR1     ;  // 1st leap yr after REFYEAR

//*  Private methods

  const char *monDay (const char *date) ;

//*  Public methods

 public:

//*    Enumeration types

  enum TimeSys {MET, TT, UTC} ;
  enum TimeFormat {SECS, JD, MJD, DATE, CALDATE} ;

//*    Constructors

  SwiftTime (void) ;
  SwiftTime (double tt) ;
  SwiftTime (double tt, TimeSys ts, TimeFormat tf=SECS) ;
  SwiftTime (const char *date, TimeSys ts=UTC, TimeFormat tf=DATE) ;

//*    Set methods

  void setleaps (void) ;
  void set (double tt, TimeSys ts=MET, TimeFormat tf=SECS) ;
  void set (const char *date, TimeSys ts=UTC, TimeFormat tf=DATE) ;
  void setTZero (double tz) ;

//*    Get methods

  double get (TimeSys ts=TT, TimeFormat tf=SECS) const ;
  double getMET (void) const ;
  double getTT (void) const ;
  double getUTC (void) const ;
  double getTZero (void) const ;
  const char *getDate (TimeSys ts=UTC, TimeFormat tf=DATE) ;
  const char *UTDate (void) ;
  const char *TTDate (void) ;
  const char *UTCalDate (void) ;
  const char *TTCalDate (void) ;
  double UTmjd (void) const ;
  double TTmjd (void) const ;
  double UTjd (void) const ;
  double TTjd (void) const ;

} ;

// Description:
// Constructor: default constructor; set to zero
inline SwiftTime::SwiftTime (void)
  : t (0.0), timeZero (0.0) { setleaps() ; }

// Description:
// Constructor: create from MET seconds
inline SwiftTime::SwiftTime (double tt)
  : t (tt), timeZero (0.0) { setleaps() ; }

// Description:
// Set the time correction term
inline void SwiftTime::setTZero (double tz) {
  timeZero = tz ;
}

// Description:
// Return MET seconds
inline double SwiftTime::getMET (void) const {
  return t+timeZero ;
}

// Description:
// Return TT seconds since REFYEAR (TT)
inline double SwiftTime::getTT (void) const {
  return t+timeZero+TT2UT ;
}

// Description:
// Return time zero point correction
inline double SwiftTime::getTZero (void) const {
  return timeZero ;
}

// Description:
// Return time as UTC date string
inline const char *SwiftTime::UTDate (void) {
  return ( getDate (UTC, DATE) ) ;
}

// Description:
// Return time as TT date string
inline const char *SwiftTime::TTDate (void) {
  return ( getDate (TT, DATE) ) ;
}

// Description:
// Return time as UTC calendar date string
inline const char *SwiftTime::UTCalDate (void) {
  return ( monDay (UTDate()) ) ;
}

// Description:
// Return time as TT calendar date string
inline const char *SwiftTime::TTCalDate (void) {
  return ( monDay (TTDate()) ) ;
}

// Description:
// Return time as MJD(UTC)
inline double SwiftTime::UTmjd (void) const {
  return ( getUTC() / 86400.0 + MJDREF ) ;
}

// Description:
// Return time as MJD(TT)
inline double SwiftTime::TTmjd (void) const {
  return ( getTT() / 86400.0 + MJDREF ) ;
}

// Description:
// Return time as JD(UTC)
inline double SwiftTime::UTjd (void) const {
  return ( UTmjd() + MJD0 ) ;
}

// Description:
// Return time as JD(TT)
inline double SwiftTime::TTjd (void) const {
  return ( TTmjd() + MJD0 ) ;
}

//
//   --------------
// -- SwiftTimeRange --
//   --------------
//

class SwiftTimeRange {

//*  Private attributes

  SwiftTime start ;
  SwiftTime stop ;
  int empty ;                    // Empty defined as:
                                   // start or stop <= 0.0, or start >= stop
                                   // start == stop > 0.0 is empty!

//*  Private method

  void setEmpty (void) ;

//*  Public methods

 public:

//*    Constructors

  SwiftTimeRange (void) ;
  SwiftTimeRange (const SwiftTime &T1, const SwiftTime &T2) ;
  SwiftTimeRange (double t1, double t2) ;

//*    Set Methods

  void setStart (const SwiftTime &T1) ;      // Set start as SwiftTime object
  void setStop (const SwiftTime &T2) ;       // Set stop as SwiftTime object
  void resetRange (const SwiftTime &T1, const SwiftTime &T2) ;
  void setStart (double t1) ;              // Set start as MET seconds
  void setStop (double t2) ;               // Set stop as MET seconds
  void resetRange (double t1, double t2) ;

//*    Get methods

  SwiftTime TStart (void) const ;            // Return start as SwiftTime object
  SwiftTime TStop (void) const ;             // Return stop as SwiftTime object
  double METStart (void) const ;           // Return start in MET seconds
  double METStop (void) const ;            // Return stop in MET seconds
  const char *UTStartDate (void) ;         // Return start as UTC date string
  const char *UTStopDate (void) ;          // Return stop as UTC date string
  const char *TTStartDate (void) ;         // Return start as TT date string
  const char *TTStopDate (void) ;          // Return stop as TT date string
  int isInRange (const SwiftTime &T) const ;
  int isInRange (double t) const ;
  double totalTime (void) const ;          // Return total seconds
  int isEmpty (void) const ;               // Empty range?
  void printRange (void) ;                 // A two-liner in UTC date format
  void printRangeCal (void) ;              // A two-liner in UTC calendar format

} ;

// Description:
// Empty constructor
inline SwiftTimeRange::SwiftTimeRange (void)
  : start (0.0), stop (0.0), empty (1) { }

// Description:
// Constructor using SwiftTime objects
inline SwiftTimeRange::SwiftTimeRange (const SwiftTime &T1, const SwiftTime &T2) 
  : start (T1), stop (T2) {
  setEmpty () ;
}

// Description:
// Constructor using MET seconds
inline SwiftTimeRange::SwiftTimeRange (double t1, double t2)
  : start (t1), stop (t2) {
  setEmpty () ;
}

// Description:
// Set start as SwiftTime object
inline void SwiftTimeRange::setStart (const SwiftTime &T1) {
  start = T1 ;
  setEmpty () ;
}

// Description:
// Set stop as SwiftTime object
inline void SwiftTimeRange::setStop (const SwiftTime &T2) {
  stop = T2 ;
  setEmpty () ;
}

// Description:
// Reset range with SwiftTime objects
inline void SwiftTimeRange::resetRange (const SwiftTime &T1, const SwiftTime &T2) {
  start = T1 ;
  stop = T2 ;
  setEmpty () ;
}

// Description:
// Set start as MET seconds
inline void SwiftTimeRange::setStart (double t1) {
  start.set (t1) ;
  setEmpty () ;
}

// Description:
// Set start as MET seconds
inline void SwiftTimeRange::setStop (double t2) {
  stop.set (t2) ;
  setEmpty () ;
}

// Description:
// Reset range in MET seconds
inline void SwiftTimeRange::resetRange (double t1, double t2) {
  start.set (t1) ;
  stop.set (t2) ;
  setEmpty () ;
}

// Description:
// Return start as SwiftTime object
inline SwiftTime SwiftTimeRange::TStart (void) const {
  return start ;
}

// Description:
// Return start as SwiftTime object
inline SwiftTime SwiftTimeRange::TStop (void) const {
  return stop ;
}

// Description:
// Return start in MET seconds
inline double SwiftTimeRange::METStart (void) const {
  return start.getMET () ;
}

// Description:
// Return stop in MET seconds
inline double SwiftTimeRange::METStop (void) const {
  return stop.getMET () ;
}

// Description:
// Return start as UTC date string
inline const char *SwiftTimeRange::UTStartDate (void) {
  return start.UTDate () ;
}

// Description:
// Return stop as UTC date string
inline const char *SwiftTimeRange::UTStopDate (void) {
  return stop.UTDate () ;
}

// Description:
// Return start as TT date string
inline const char *SwiftTimeRange::TTStartDate (void) {
  return start.TTDate () ;
}

// Description:
// Return stop as TT date string
inline const char *SwiftTimeRange::TTStopDate (void) {
  return stop.TTDate () ;
}

// Description:
// Return -1 if before, 0 if in range, 1 if after
inline int SwiftTimeRange::isInRange (double t) const {
  if ( t < start.getMET() )
    return -1 ;
  else if ( t > stop.getMET() )
    return 1 ;
  else if ( empty )
    return 1 ;
  else
    return 0 ;
}

// Description:
// Return -1 if before, 0 if in range, 1 if after
inline int SwiftTimeRange::isInRange (const SwiftTime &T) const {
  return isInRange (T.getMET()) ;
}

// Description:
// Return total seconds
inline double SwiftTimeRange::totalTime (void) const {
  return ( empty ? 0.0 : ( stop.getMET() - start.getMET() ) ) ;
}

// Description:
// Empty range?
inline int SwiftTimeRange::isEmpty (void) const {
  return empty ;
}

//
//   ---------
// -- STRList --
//   ---------
//

class STRList {

//*  Private attributes

  SwiftTimeRange listRange ;
  int numSTRs ;
  SwiftTimeRange *tr ;
  int empty ;

//*  Public methods

 public:

//*    Constructors

  STRList (void) ;
  STRList (const SwiftTimeRange &T) ;
  STRList (const STRList &trl) ;
  STRList (const STRList &trl1, const STRList &trl2) ;

//*    Destructor

  ~STRList () ;

//*    Operators

  STRList& operator=(const STRList &trl) ;

//*    Processing (modification) methods

  void orList (const STRList &trl) ;
  void notList (const SwiftTimeRange &T) ;
  void andRange (const SwiftTimeRange &T) ;
  void orRange (const SwiftTimeRange &T) ;

//*    Get methods

  int isInRange (const SwiftTime &T) const ;         //  Return 0 if in range
  int isInRange (double t) const ;
  int getNumSTRs (void) const ;
  const SwiftTimeRange *getRange (int i) const ;
  const SwiftTimeRange *getRange (const SwiftTime &T) const ;
  const SwiftTimeRange *getRange (double t) const ;
  void setListRange (void) ;
  int isEmpty (void) const ;
  double totalTime (void) const ;
  void printList (void) ;
  void printListCal (void) ;
} ;

// Description:
// Default constructor for a single SwiftTimeRange List
inline STRList::STRList (void)
  : empty(1), numSTRs (1) {
  tr = new SwiftTimeRange () ;
  listRange = *tr ;
}

// Description:
// Constructor for a single SwiftTimeRange List
inline STRList::STRList (const SwiftTimeRange &T)
  : listRange (T), numSTRs (1) {
  tr = new SwiftTimeRange (T) ;
  empty = T.isEmpty () ;
}

// Description:
// Destructor
inline STRList::~STRList () {
  delete [] tr ;
}

// Description:
// Return number of ranges in list
inline int STRList::getNumSTRs (void) const {
  return numSTRs ;
}

// Description:
// Return range no. ,i>
inline const SwiftTimeRange *STRList::getRange (int i) const {
  if ( ( i >= 0 ) && ( i < numSTRs ) )
    return tr+i ;
  else
    return NULL ;
}

// Description:
// Empty list?
inline int STRList::isEmpty (void) const {
  return empty ;
}

#endif             // SwiftTIME_H
