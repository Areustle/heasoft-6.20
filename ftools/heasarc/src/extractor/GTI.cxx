// Code for GTI and GTIs classes

#ifndef HAVE_GTI
#include "GTI.h"
#endif

// Class GTI

//default constructor
GTI::GTI()
{
}

//default destructor
GTI::~GTI()
{
}

// deep copy

GTI& GTI::operator= (const GTI& a)
{
  MJDREF_days = a.MJDREF_days;
  MJDREF_seconds = a.MJDREF_seconds;

  Start.resize(a.Start.size());
  End.resize(a.End.size());

  for (size_t i=0; i<Start.size(); i++) {
    Start[i] = a.Start[i];
    End[i] = a.End[i];
  }

  return *this;
}

// clear the gti

void GTI::clear()
{
  Start.clear();
  End.clear();
  MJDREF_days = 0;
  MJDREF_seconds = 0.0;
  return;
}

// check whether a time or set of (ordered) times falls within the GTI
// need to work out how to do the adjust_gti fix for aligning with bin boundaries

bool GTI::isInGTI(Real time, Integer mjdrefi, Real mjdrefr)
{
  vector<bool> result(1);
  vector<Real> inTime;
  inTime.push_back(time);

  result = this->isInGTI(inTime, mjdrefi, mjdrefr);

  return result[0];
}

// note the following assumes that both input times and GTIs are ordered
// might want to change this to output a pointer to avoid copying long vectors

vector<bool> GTI::isInGTI(vector<Real> intime, Integer mjdrefi, Real mjdrefr)
{
  static size_t index;
  static bool first;
  vector<bool> inGTI(time.size(),false);

  // first need to map input times onto the MJDREF in the GTI

  vector<Real> time(intime);
  Real diff = (mjdrefr - MJDREF_seconds) + 86400.0d0*(mjdrefi - MJDREF_days);
  for (size_t i=0; i<time.size(); i++) time[i] += diff;

  // initialize the current gti to the first

  if ( first ) {
    index = 0;
    first = false;
  }

  // usually times will be tested in increasing order but if an input time
  // is before the current gti (as defined by index) then do a reset

  if ( time[0] < Start[index] ) index = 0;

  if ( Start.size() == 0 ) return inGTI;

  if ( time[0] > End[End.size()-1] ||
       time[time.size()-1] < Start[0] ) return inGTI;


  for (size_t i=0; i<time.size(); i++) {

    if ( time[i] > End[End.size()-1] ) return inGTI;

    if ( time[i] < Start[index] ) {
    // if time is before current start then it cannot be an interval
      inGTI[i] = false;

    } else if ( time[i] >= Start[index] && time[i] <= End[index] ) {
    // check if time is in current interval
      inGTI[i] = true;

    } else {
      // do O(logN) search for next relevant GTI interval

      bool foundit(false);
      size_t lasttry = Start.size()/2;
      size_t step = lasttry;

      while ( !foundit ) {
	size_t itry = lasttry;

	if ( itry == Start.size()-1 ) {
	  // special case if we are testing last interval
	  index = itry;
	  foundit = true;
	  if ( time[i] >= Start[itry] && time[i] <= End[itry] ) inGTI[i] = true;
	} else if ( time[i] >= Start[itry] && time[i] <= Start[itry+1] ) {
	  // found the relevant interval
	  index = itry;
	  foundit = true;
	  if ( time[i] <= End[itry] ) inGTI[i] = true;
	} else {
	  // not the right interval so try again
	  step = step/2;
	  if ( step < 1 ) step = 1;
	  if ( time[i] > Start[itry] ) {
	    lasttry += step;
	  } else {
	    lasttry -= step;
	  }
	}
      }
    }

    // end loop over times

  }

  return inGTI;

}

// order the GTI - remove overlaps and arrange in increasing order of time

void GTI::order()
{

  size_t N(Start.size());

  // heapsort on Start while switching End at the same time.

  if ( N < 2 ) return;

  size_t l(N/2);
  size_t ir(N-1);
  Real stemp, etemp;

  bool done(false);
  while (!done) {

    if ( l > 0 ) {
      l--;
      stemp = Start[l];
      etemp = End[l];
    } else {
      stemp = Start[ir];
      etemp = End[ir];
      Start[ir] = Start[0];
      End[ir] = End[0];
      ir--;
      if ( ir == 0 ) {
	Start[0] = stemp;
	End[0] = etemp;
	done = true;
      }
    }

    if (!done) {
      size_t i(l);
      size_t j(l+1);

      bool done2(false);
      while (!done2) {
	if ( j <= ir ) {
	  if ( j < ir ) {
	    if ( Start[j] < Start[j+1] ) j++;
	  }
	  if ( stemp < Start[j] ) {
	    Start[i] = Start[j];
	    End[i] = End[j];
	    i = j;
	    j = j + j;
	  } else {
	    j = ir + 1;
	  }
	} else {
	  Start[i] = stemp;
	  End[i] = etemp;
	  done2 = true;
	}
      }
    }

  }

  // Intervals now in increasing order of Start. Merge the overlaps

  bool found(true);
  while ( found ) {
    found = false;

    for (size_t i=0; i<N-1; i++) {
      if ( !found ) {
	if ( End[i] >= Start[i+1] ) {
	  found = true;
	  if ( End[i+1] > End[i] ) End[i] = End[i+1];
	  Start.erase(Start.begin()+i+1);
	  End.erase(End.begin()+i+1);
	  N--;
	}
      }
    }
  }

  // Clean up the "bad" GTIs where Start > End.

  size_t i = 0;
  while ( i < N ) {
    if ( Start[i] > End[i] ) {
      Start.erase(Start.begin()+i);
      End.erase(End.begin()+i);
      N--;
    } else {
      i++;
    }
  }

  return;

}

// dump the contents to the screen for diagnostic purposes

void GTI::dump()
{
  cout << "Detector number = " << Detector << endl;
  cout << "TimeZero = " << TimeZero << endl;
  for (size_t i=0; i<Start.size(); i++) {
    cout << i << " " << Start[i] << " " << End[i] << endl;
  }
  return;
}

// return the size of the Start and End arrays

size_t GTI::size()
{
  return Start.size();
}

// merge with another GTI - either 'and'ing or 'or'ing

void GTI::merge(const GTI& a, bool andmode)
{
  // first map Start and Stop from a onto the same MJDREF as this.

  vector<Real> inStart(a.Start);
  vector<Real> inEnd(a.End);

  Real diff = (a.MJDREF_seconds - MJDREF_seconds) 
    + 86400.0d0*(a.MJDREF_days - MJDREF_days);
  for (size_t i=0; i<inStart.size(); i++) {
    inStart[i] += diff;
    inEnd[i] += diff;
  }

  size_t N1(this->size());
  size_t N2(inStart.size());

  // for 'OR' mode just need to merge the two sets of good time
  // intervals and run order().

  if ( !andmode ) {

    for (size_t k=0; k<N2; k++) {
      Start.push_back(inStart[k]);
      End.push_back(inEnd[k]);
    }
    this->order();
    return;

  }

  // what is left is the 'AND' mode for which we have to work through 
  // the intervals finding overlaps

  // trap special cases of no intervals in one of the gtis

  if ( N2 == 0 ) return;

  if ( N1 == 0 ) {
    Start.resize(N2);
    End.resize(N2);
    for (size_t i=0; i<N2; i++) {
      Start[i] = inStart[i];
      End[i] = inEnd[i];
    }
    return;
  }

  // now the general case

  vector<Real> tStart, tEnd;

  size_t i1(0);
  size_t i2(0);
  bool done(false);
  bool active(false);

  while ( !done ) {

    // if active find the next end time then set to inactive

    if ( active ) {

      if ( End[i1] > inEnd[i2] ) {
	tEnd.push_back(inEnd[i2]);		  
	i2++;
      } else {
	tEnd.push_back(End[i1]);
	i1++;
      }
      active = false;

      // eliminate this interval if it is zero length

      if ( tEnd[tEnd.size()-1] <= tStart[tStart.size()-1] ) {
	tStart.pop_back();
	tEnd.pop_back();
      }

      if ( i1 >= N1 || i2 >= N2 ) done = true;
      
    } else {

      // if not active then find the next time which is valid and set to active

      while ( End[i1] < inStart[i2] && !done ) {
	i1++;
	if ( i1 >= N1 ) done = true;
      }

      while ( inEnd[i2] < Start[i1] && !done ) {
	i2++;
	if ( i2 >= N2 ) done = true;
      }

      if ( !done ) {
	if ( Start[i1] >= inStart[i2] ) {
	  tStart.push_back(Start[i1]);
	} else {
	  tStart.push_back(inStart[i2]);
	}
	active = true;
      }
    }
  }

  // copy temp arrays back into this

  Start.resize(tStart.size());
  End.resize(tEnd.size());
  for (size_t i=0; i<Start.size(); i++) {
    Start[i] = tStart[i];
    End[i] = tEnd[i];
  }

  return;
}

// return the exposure time between t1 and t2 within gti

Real GTI::exposure(const Real t1, const Real t2)
{
  Real texp = 0.0;

  for(size_t i=0; i<this->size(); i++) {

    Real g1 = Start[i];
    Real g2 = End[i];

    if ( t2 < g1 ) return texp;

    if ( g1 <= t1 && g2 >= t1 && g2 <= t2 ) {
      // partial overlap with g2 <= t2
      //   g1-------------g2
      //       t1------------t2
      texp += g2 - t1;
    } else if ( g2 <= t1 ) {
      // no overlap
      //  g1-----g2
      //             t1-------t2
    } else if ( t2 <= g1 ) {
      // no overlap
      //             g1-----g2
      //  t1-----t2
    } else if ( g1 >= t1 && g1 <= t2 && g2 >= t2 ) {
      // partial overlap with g1 >= t1
      //       g1-------------g2
      //   t1------------t2
      texp += t2 - g1;
    } else if ( g1 >= t1 && g1 <= t2 && g2 >= t1 && g2 <= t2 ) {
      // gti totally within time range
      //       g1---g2
      //   t1------------t2
      texp += g2 - g1;
    } else if ( g1 <= t1 && g2 >= t2 ) {
      // time range totall within gti
      //   g1-----------g2
      //      t1-----t2
      texp += t2 - t1;
    }

  }

  return texp;
}

// change the MJDREF used for this gti

void changeMJDREF(Integer MJDREF_days_in, Real MJDREF_seconds_in) 
{

  Real diff = (MJDREF_seconds - MJDREF_seconds_in)
    + 86400.0d0*(MJDREF_days - MJDREF_days_in);
  for (size_t i=0; i<Start.size(); i++) {
    Start[i] += diff;
    End[i] += diff;
  }

  return;
}


// read from a file

Integer GTI::read(string filename, string gtiname)
{
  Integer status(0);
  return status;
}

// read from a FITS file

Integer GTI::readFITS(string filename, string gtiname)
{
  //  bool verbosity = FITS::verboseMode();
  const vector<string> hduKeys;
  const vector<string> primaryKey;

  auto_ptr<FITS> pInfile(0);

  if ( index(filename.c_str(),'[') != 0 ) {

    // if an extension is specified as part of the filename then open the file
    // and move to it

    try {
      pInfile.reset(new FITS(filename,Read,false));
    } catch (...) {
      return 1;
    }
       
  } else {

    // if the extension was not specified then try to find it - first try gtiname

    try {
      pInfile.reset(new FITS(filename,Read,gtiname,false,hduKeys,primaryKey));
    } catch (...) {

      // if that didn't work try STDGTI as the HDU name.

      try {
	pInfile.reset(new FITS(filename,Read,"STDGTI",false,hduKeys,primaryKey));
      } catch (...) {

	// if none of these work then just read the second HDU

	try {
	  pInfile.reset(new FITS(filename,Read,2,false,hduKeys,primaryKey));
	} catch (...) {
	  return 1;
	}
      }
    }
  }

  ExtHDU& gtiext = pInfile->currentExtension();

  // get TIMEZERO and MJDREF keywords - note the different ways that
  // MJDREF data may be stored

  Real TimeZero;
  try {
    gtiext.readKey("TIMEZERO", TimeZero);
  } catch (CCfits::HDU::NoSuchKeyword&) {
    TimeZero = 0.0;
  }

  try {
    Real mjdref;
    gtiext.readKey("MJDREF", mjdref);
    MJDREF_days = (Integer)mjdref;
    MJDREF_seconds = mjdref - MJDREF_days;
  } catch (CCfits::HDU::NoSuchKeyword&) {
    try {
      gtiext.readKey("MJDREFI", MJDREF_days);
      gtiext.readKey("MJDREFF", MJDREF_seconds);
    } catch (CCfits::HDU::NoSuchKeyword&) {
      try {
	gtiext.readKey("XS-MJDRD", MJDREF_days);
	gtiext.readKey("XS-MJDRF", MJDREF_seconds);
      } catch (CCfits::HDU::NoSuchKeyword&) {
	return 2;
      }
    }
  }
  MJDREF_seconds *= 86400;

  Integer Nrows;
  gtiext.readKey("NAXIS2", Nrows);

  try {

    // get columns for START and STOP

    Column& StartCol = gtiext.column("START");
    Column& StopCol = gtiext.column("STOP");

    // read START and STOP columns

    StartCol.read(Start,1,Nrows);
    StopCol.read(End,1,Nrows);

  } catch(...) {
    return 3;
  }

  // if TimeZero is non-zero then add to the Start and End

  if ( TimeZero != 0.0 ) {
    for (size_t i=0; i<Start.size(); i++) {
      Start += TimeZero;
      End += TimeZero;
    }
  }

  return 0;
}

// read from a XRONOS window file

Integer GTI::readXRONOS(string filename)
{
  Integer status(0);

  // open file

  ifstream xrfile;
  xrfile.open(filename.c_str());
  if ( !xrfile.is_open() ) {
    return 1;
  }

  // jump over first line

  string line;
  getline(xrfile, line);

  // read number of time intervals

  getline(xrfile, line);

  stringstream lstream;
  size_t N;
  lstream << line;
  lstream >> N;

  // read time intervals

  Real t1, t2;
  for (size_t i=0; i<N; i++) {
    getline(xrfile, line);
    lstream.clear();
    lstream << line;
    lstream >> t1 >> t2;
    Start.push_back(t1*86400.0);
    End.push_back(t2*86400.0);
  }

  xrfile.close();

  // at the moment MJDREFs are initialized to -1. Need to work
  // out how to get this information from a XRONOS window file

  MJDREF_days = -1;
  MJDREF_seconds = -1.0;

  return status;
}

// read from a simple text file

Integer GTI::readText(string filename)
{
  Integer status(0);

  // open file

  ifstream txtfile;
  txtfile.open(filename.c_str());
  if ( !txtfile.is_open() ) {
    return 1;
  }

  string line;
  stringstream lstream;

  // read time intervals

  Real t1, t2;
  while( txtfile.good() ) {
    getline(txtfile, line);
    if ( line.length() > 0 ) {
      lstream.clear();
      lstream << line;
      lstream >> t1 >> t2;
      Start.push_back(t1);
      End.push_back(t2);
    }
  }

  txtfile.close();

  // at the moment MJDREFs are initialized to -1. Need to work
  // out how to get this information from a text file

  MJDREF_days = -1;
  MJDREF_seconds = -1.0;

  return status;
}

// write to a FITS file

Integer GTI::writeFITS(string filename, string gtiname)
{
  Integer status(0);

  vector<string> ttype;
  vector<string> tform;
  vector<string> tunit;

  // Create a new FITS file instance

  auto_ptr<FITS> pFits(0);
  try{
    pFits.reset(new FITS(filename, Write));
  } catch (FITS::CantCreate) {
    return 1;
  }

  // set up column descriptors

  ttype.push_back("START");
  tform.push_back("D");
  tunit.push_back("Seconds");

  ttype.push_back("STOP");
  tform.push_back("D");
  tunit.push_back("Seconds");

  // Create the new extension

  Table* pgtiext = pFits->addTable(gtiname, Start.size(), ttype, tform, tunit, BinaryTbl);
  Table& gtiext = *pgtiext;

  // Write the timezero keyword

  gtiext.addKey("TIMEZERO", 0.0, " ");

  // Write the MJDREF keywords

  gtiext.addkey("MJDREFI, MJDREF_days, "Modified Julian date in days (integer part)");
  gtiext.addkey("MJDREFF, MJDREF_seconds/86400.0, "Modified Julian date in days (fractional part)");

  // Write the columns

  gtiext.column("START").write(Start, 1);
  gtiext.column("STOP").write(End, 1);

  return status;
}



//***********************************************************************
// Methods for GTIs.

//default constructor
GTIs::GTIs()
{
}

//default destructor
GTIs::~GTIs()
{
}

// deep copy

GTIs& GTIs::operator= (const GTIs& a)
{
  gti.resize(a.gti.size());
  for(size_t i=0; i<gti.size(); i++) {
    gti[i] = a.gti[i];
  }
  return *this;
}

// clear the gtis

void GTIs::clear()
{
  gti.clear();
  return;
}

// unify - set all GTIs on the same MJDREF

void GTIs::unify()
{
  // find minimum MJDREF

  Integer MJDREF_days_min = gti[0].MJDREF_days;
  Real MJDREF_seconds_min = gti[0].MJDREF_seconds;

  for (size_t i=1; i<gti.size(); i++) {

    if ( gti[i].MJDREF_days < MJDREF_days_min ) {
      MJDREF_days_min = gti[i].MJDREF_days;
      MJDREF_seconds_min = gti[i].MJDREF_seconds;
    } else if ( gti[i].MJDREF_days == MJDREF_days_min ) {
      if ( gti[i].MJDREF_seconds < MJDREF_seconds_min ) {
	MJDREF_seconds_min = gti[i].MJDREF_seconds;
      }
    }

  }

  // now reset all MJDREF to the minimum

  for (size_t i=0; i<gti.size(); i++) {
    gti[i].changeMJDREF(MJDREF_days_min, MJDREF_seconds_min);
  }

  return;

}


