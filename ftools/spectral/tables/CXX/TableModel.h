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

using namespace std;
using namespace CCfits;

typedef int Integer;
typedef float Real;
typedef valarray<Integer> IntegerArray;
typedef valarray<Real> RealArray;

#define HAVE_TABLEMODEL 1

// class definition for individual parameters within the table model

class TableParameter{
 public:

  string Name;                  // Parameter name
  int InterpolationMethod;      // 0==linear, 1==log
  Real InitialValue;            // Initial value for fit
  Real Delta;                   // Delta for fit
  Real Minimum;                 // Hard lower-limit (should correspond to first tabulated value)
  Real Bottom;                  // Soft lower-limit
  Real Top;                     // Soft upper-limit
  Real Maximum;                 // Hard upper-limit (should correspond to last tabulated value)
  RealArray TabulatedValues;    // Tabulated parameter values

  //constructor

  TableParameter();

  // destructor

  ~TableParameter();

};

// class definition for individual spectra (and additional spectra) within the table model

class TableSpectrum{
 public:

  RealArray Flux;
  RealArray ParameterValues;
  vector<RealArray> addFlux;

  //constructor

  TableSpectrum();

  // destructor

  ~TableSpectrum();

};


// class definition for table

class TableModel{
 public:

  vector <TableParameter> Parameters;
  vector <TableSpectrum> Spectra;
  string ModelName;
  string ModelUnits;
  int NumIntParams;
  int NumAddParams;
  bool isError;
  bool isRedshift;
  bool isAdditive;
  RealArray Energies;

  // constructor

  TableModel();

  // destructor

  ~TableModel();

  // write to a FITS file

  void write(string filename);

};
