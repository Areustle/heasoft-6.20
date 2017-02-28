//----------------------------------------------------------------------
//
//  File:        sTime2.C
//  Programmer:  Arnold Rots  -  USRA
//  Date:        15 March 1996
//  Description: Time conversion utility using SwiftTime.
//
// .NAME    sTime - Time conversion utility for Swift
// .LIBRARY Util
// .HEADER  Swift Time Converter
//
// .SECTION SYNOPSIS
//  .RI sTime " time " [ timeSystem " [" timeFormat ]]
//
// .SECTION Author
//  Arnold Rots,
//  USRA,
//  <arots@xebec.gsfc.nasa.gov>
//    modified 24Feb2004 by M.Tripicco for Swift
//
// .SECTION DESCRIPTION
//  sTime2 is the web version of sTime.
//  sTime is a utility that performs conversions between different
//  time systems and formats.  A time is input on the command line
//  in any system and format, and may subsequently be retrieved in
//  any system and format.
//
//  The supported time systems are: MET, TT, and UTC.
//
//  The supported time formats are: seconds since 2001.0 in decimal
//  and hexadecimal format, Mission Day Number (ddd:hh:mm:ss.sss), Julian Day,
//  Modified Julian Day (=JD-2400000.5), Date (yyyy:ddd:hh:mm:ss),
//  and Calendar Date (yyyyMondd at hh:mm:ss).
//
//  The user is expected to provide the time to be converted on the
//  command line, optionally with a time system and a time format
//  specification.  If the time system is not specified, MET is assumed.
//  If the format is not specified, sTime tries to figure it out for
//  itself; in that case, numbers under 100000 are assumed to be MJD,
//  numbers under 2500000 JD, and anything larger SECS; mission day
//  number and date are distinguished by the value of the first number:
//  if between 367 and 1900, mission day number (code "N") is assumed;
//  hexadecimal seconds are not supported in this automatic recognition mode,
//  since it can be ambiguous.
//
//  Time system and time format codes may be abbreviated to single
//  characters and are case-insensitive.
//
//  During execution, the user is asked for time system and time
//  format codes that (s)he wants the time converted to, until the
//  user indicates the desire to quit.
//
//----------------------------------------------------------------------
//

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <limits.h>
#include "SwiftTime.h"

using namespace std;

SwiftTime *getinput (char *, int, char **) ;
int getsys (SwiftTime::TimeSys *) ;
int readsys (char *, SwiftTime::TimeSys *) ;
int getform (SwiftTime::TimeFormat *, int *, int *) ;
int readform (char *, SwiftTime::TimeFormat *, int *, int *) ;
extern "C" { int sCC (double, double *, long *) ; }

//
//   -------
// -- sTime --
//   -------
//

int main (int argc, char *argv[]) {
  char progName[256] ;
  SwiftTime *T ;
  int error = 0 ;
  SwiftTime::TimeSys tSys ;
  SwiftTime::TimeFormat tForm ;
  int hexfmt=0 ;
  int nmday=0 ;

//    Get the time
  if ( ( T = getinput(progName, argc, argv) ) == NULL ) {
    cout << "sTime2: Incorrect time format; try again" << endl ; 

//         << "sTime2 - Usage:\n    " << progName
//         << " <t> <tsys in> <tform in> <tsys out> <tform out>" << endl
//	 << " <t> is the input time," << endl
//	 << " <tsys> = MET (default), TT, or UTC" << endl
//	 << " <tform> = SECS, HEXSECS, NUMDAY, JD, MJD, DATE, CALDATE"
//	 << endl ;
    error = 1 ;
  }

//    Conversion and output loop
  if ( !error ) {

//      Get desired time system
    if ( readsys (argv[argc-2], &tSys) ) {
      error = 2 ;
      cout << "sTime2: Failed readsys" << endl ;
    }

//      Get desired time format
    if ( readform (argv[argc-1], &tForm, &hexfmt, &nmday) ) {
      error = 3 ;
      cout << "sTime2: Failed readform" << endl ;
    }

//      Convert and print the result
    if ( !error ) {
      switch (tForm) {
        case SwiftTime::SECS : case SwiftTime::JD : case SwiftTime::MJD : {
	  double t = T->get(tSys, tForm) ;
	  if ( hexfmt ) {
	    unsigned long jt = (unsigned long) t ;
	    cout << "0x" << setw(7) << hex << jt << endl ;
	  }
	  else if ( nmday > 0 ) {
	    int day = (int) t / 86400 ;
	    t -= day * 86400 ;
	    int h = (int) t / 3600 ;
	    t -= h * 3600 ;
	    int m = (int) t / 60 ;
	    t -= m * 60 ;
	    cout << dec << day << ":" << h << ":" << m << ":"
	         << setprecision(10) << t << endl ;
	  }
	  else if ( nmday < 0 )
	    cout << setprecision(15) << t/86400.0 << endl ;
	  else 
	    cout << setprecision(18) << t << endl ;
	  break ;
	}
	case SwiftTime::DATE : case SwiftTime::CALDATE : {
	  const char *s = T->getDate(tSys, tForm) ;
	  cout << s << endl ;
	  break ;
	}
      }
    }
  }

  double t0 ;
  long tc ;
  //  MJT 09Feb2007: Swift clock correction file is indexed by UTC, not MET
  if ( sCC (T->getUTC(), &t0, &tc) )
    cout << "Clock correction for this time is not available" << endl ;
  else
    cout << "Clock correction: " << setprecision(6) << -1e-6*tc << " s" << endl;

  exit ( error ) ;
}

//
//   ----------
// -- getinput --
//   ----------
//

// Description:
// Parse the input time on the command line.
SwiftTime *getinput (char *progName, int argc, char *argv[])
{
  SwiftTime *tt = NULL ;
  char str[256] ;
  double t ;
  unsigned long jt = 0 ;
  SwiftTime::TimeSys tSys = SwiftTime::MET ;
  SwiftTime::TimeFormat tForm = SwiftTime::SECS ;
  int hexfmt = 0 ;
  int ch = 0 ;
  int getform = 0 ;
  int error = 0 ;
  int nmday = 0 ;
  int day = 0 ;
  int h = 0 ;
  int m = 0 ;

//    Program name
  strcpy (progName, argv[0]) ;

//    No time argument
  if ( argc < 2 )
    error = 1 ;

  if ( !error ) {

//    Only time provided
    if ( argc == 2 ) {
      if ( strstr (argv[1], ":") ) {
	sscanf (argv[1], "%d:", &day) ;
	if ( ( day < 1900 ) && ( day > 366 ) ) {
	  nmday = 1 ;
	  tForm = SwiftTime::SECS ;
	}
	else {
	  strcpy (str, argv[1]) ;
	  tForm = SwiftTime::DATE ;
	  ch = 1 ;
	}
      }
      else
	getform = 1 ;
    }
    else {
      int istrt = 2 ;

//      Caldate format?
      if ( ( argc >= 4 ) && ( !strcmp(argv[2], "at")
			     || !strcmp(argv[2], "AT") ) ) {
	istrt = 4 ;
	sprintf (str, "%s %s %s", argv[1], argv[2], argv[3]) ;
	tForm = SwiftTime::CALDATE ;
	ch = 1 ;
      }

//      Date format?
      else if ( strstr (argv[1], ":") ) {
	sscanf (argv[1], "%d:", &day) ;
	if ( ( day < 1900 ) && ( day > 366 ) ) {
	  nmday = 1 ;
	  tForm = SwiftTime::SECS ;
	}
	else {
	  strcpy (str, argv[1]) ;
	  tForm = SwiftTime::DATE ;
	  ch = 1 ;
	}
      }

//        Get time system
      if ( argc > istrt )
	error = readsys (argv[istrt], &tSys) ;
      
//        Get time format
      if ( !error ) {
	if ( argc > istrt+1 ) {
	  error = readform (argv[istrt+1], &tForm, &hexfmt, &nmday) ;
//           MJT 22Nov99: don't rely on assumptions when we have specifier!
          if ( *argv[istrt+1] == 'n' )
            ch = 0;
        }
	else if ( !ch )
	  getform = 1 ;
      }
    }
  }

  if ( !error ) {

//    Number input
    if ( !ch ) {
      if ( hexfmt ) {
	sscanf (argv[1], "%x", &jt) ;
	t = jt ;
      }
      else if ( nmday > 0 ) {
	sscanf (argv[1], "%d:%d:%d:%lg", &day, &h, &m, &t) ;
	t += 86400 * day + 3600 * h + 60 * m ;
      }
      else if ( nmday < 0 )
	t = 86400.0 * atof (argv[1]) ;
      else
	t = atof (argv[1]) ;
//      If format has to be deduced ...
      if ( getform ) {
	if ( t < 100000.0 )
	  tForm = SwiftTime::MJD ;
	else if ( t < 2500000.0 )
	  tForm = SwiftTime::JD ;
	else
	  tForm = SwiftTime::SECS ;
      }
      tt = new SwiftTime (t, tSys, tForm) ;
    }

//    Character string input
    else
      tt = new SwiftTime(str, tSys, tForm) ;
  }

//  Done
  return tt ;
}

//
//   --------
// -- getsys --
//   --------
//

// Description:
// Get time system code from the user.
int getsys (SwiftTime::TimeSys *tSys)
{
  int cont = 1 ;
  char tsys[32] ;

  while ( cont > 0 ) {
    cout << "Conversion to Time System MET, TT, UTC, or Quit: " ;
    fgets (tsys, 10, stdin) ;
    cont = readsys (tsys, tSys) ;
  }
  return cont ;
}

//
//   ---------
// -- readsys --
//   ---------
//

// Description:
// Interpret the time system code provided by the user.
// Return 0 if valid code, -1 for quit, +1 for unrecognized code.
int readsys (char *tsys, SwiftTime::TimeSys *tSys)
{
  int quit = 0 ;
  switch (*tsys) {
  case 'm': case 'M':
    *tSys = SwiftTime::MET ;
    break ;
  case 't': case 'T':
    *tSys = SwiftTime::TT ;
    break ;
  case 'u': case 'U':
    *tSys = SwiftTime::UTC ;
    break ;
  case 'q': case 'Q': case 'x': case 'X':
    quit = -1 ;
    break ;
  default:
    quit = 1 ;
  }
  return quit ;
}

//
//   ---------
// -- getform --
//   ---------
//

// Description:
// Get time format code from the user.
int getform (SwiftTime::TimeFormat *tForm, int *hex, int *nmday)
{
  int cont = 1 ;
  char tform[32] ;

  while ( cont > 0 ) {
    cout << "Print in Format SECS, HEXSECS, NUMDAY, JD, MJD, DATE, CALDATE, or Quit: " ;
    fgets (tform, 10, stdin) ;
    cont = readform (tform, tForm, hex, nmday) ;
  }
  return cont ;
}

//
//   ----------
// -- readform --
//   ----------
//

// Description:
// Interpret the time format code provided by the user.
// Return 0 if valid code, -1 for quit, +1 for unrecognized code.
int readform (char *tform, SwiftTime::TimeFormat *tForm, int *hex, int *nmday)
{
  int quit = 0 ;
  *hex = 0 ;
  *nmday = 0 ;

  switch (*tform) {
  case 's': case 'S':
    *tForm = SwiftTime::SECS ;
    break ;
  case 'j': case 'J':
    *tForm = SwiftTime::JD ;
    break ;
  case 'm': case 'M':
    *tForm = SwiftTime::MJD ;
    break ;
  case 'd': case 'D':
    *tForm = SwiftTime::DATE ;
    break ;
  case 'c': case 'C':
    *tForm = SwiftTime::CALDATE ;
    break ;
  case 'h': case 'H':
    *tForm = SwiftTime::SECS ;
    *hex = 1 ;
    break ;
  case 'n': case 'N':
    *tForm = SwiftTime::SECS ;
    *nmday = 1 ;
    break ;
  case 'f': case 'F':
    *tForm = SwiftTime::SECS ;
    *nmday = -1 ;
    break ;
  case 'q': case 'Q': case 'x': case 'X':
    quit = -1 ;
    break ;
  default:
    quit = 1 ;
  }
  return quit ;
}
