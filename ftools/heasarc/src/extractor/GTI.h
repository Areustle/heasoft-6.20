// GTI and GTIs class definitions

#define HAVE_GTI 1

#include <fstream>
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <cstdio>
#include <sstream>
#include <string>
#include <stdexcept>
#include <ctime>
#include <valarray>
#include <vector>

#include <CCfits/CCfits>

typedef int Integer;
typedef double Real;

using namespace std;
using namespace CCfits;

// a single GTI consists of arrays of start and stop times in seconds
// and an Integer and Real defining the MJD reference from which these
// times are measured.

class GTI{
 public:

  vector<Real> Start;
  vector<Real> End;
  Integer MJDREF_days;
  Real MJDREF_seconds;

  // constructor
  GTI();

  // destructor
  ~GTI();

  // deep copy

  GTI& operator= (const GTI&);

  // clear the gti

  void clear();

  // check whether a time or set of (ordered) times falls within the GTI

  bool isInGTI(Real time, Integer mjdrefi, Real mjdrefr);
  vector<bool> isInGTI(vector<Real> time, Integer mjdrefi, Real mjdrefr);

  // order the GTI - remove overlaps and arrange in increasing order of time

  void order();

  // dump the contents to the screen for diagnostic purposes

  void dump();

  // size of Start and End arrays

  size_t size();

  // merge with another GTI - either 'and'ing or 'or'ing

  void merge(const GTI&, bool andmode);

  // return the exposure time between t1 and t2 within gti

  Real exposure(const Real t1, const Real t2);

  // change the MJDREF used for this gti

  void changeMJDREF(Integer MJDREF_days_in, Real MJDREF_seconds_in);

  // read from a file

  Integer read(string filename, string gtiname);

  // read from a FITS file

  Integer readFITS(string filename, string gtiname);

  // read from a XRONOS window file

  Integer readXRONOS(string filename);

  // read from a text file

  Integer readText(string filename);

  // write to a FITS file

  Integer writeFITS(string filename, string gtiname);

};

// the GTIs class is a vector of single GTI objects

class GTIs{
 public:

  vector<GTI>   gti;

  // constructor
  GTIs();

  // destructor
  ~GTIs();

  // deep copy

  GTIs& operator= (const GTIs&);

  // clear the gtis

  void clear();

  // unify - set all GTIs on the same MJDREF

  void unify();

};



// Notes on matching to fortran routines in times.f
//
// ADDGTI   loops round time filter files and ANDs the resulting GTIs.
// ADDGTI1  reads from a FITS time filter file
// ADDGTI2  reads from a text time filter file
// CALCBINSIZE calculates the good time in a bin
// CHECKGTI checks for overlap between GTIs read from a file and those passed
// EXTMGTI  sort/merge a GTI.
// FINGTI   ANDs two GTIs (both have been sorted, merged and filtered).
// GETGTI   reads GTIS from a FITS file
// SELECT_PHASE_SETUP read Xronos window file to set up phase filtering
// SELECT_PHASE check whether time is in phase bin
// CALC_EXP calculate phase-corrected exposure
// SELECT_TIME check whether time is in GTI
// ADJUST_GTI correct GTI to frame boundary.
// EXSORT2  heapsort of one array, switching second array at same time
// EXSORT3  heapsort of 2-D array on first index, switching second index at same time
// EXPCORR  corrects exposure time for phase windowing
// SUBSET   string manipulation routine
// SPLITGTI split a GTI for multiple detectors into separate GTIs for each detector
