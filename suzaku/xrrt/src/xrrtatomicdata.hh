// xrrtatomicdata.hh
//
// Class definition for Atomic Data Factors
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/18 Moved error codes form xrrt_types.hh to here. R. Fink
// 1997/09/19 Improved documentation. R. Fink
// 1997/09/19 Replaced error returns by exceptions. R. Fink
// 
// 2005/12/24 Y.ISHISAKI	version 6.2.4
//	change char * -> string in loadTableFromFits()

#ifndef XRRTATOMICDATA_HH
#define XRRTATOMICDATA_HH

//
// XrrtAtomicData system interfaces used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <string.h>
#include <cstring>
#include <string>
// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
// #include <stl.h>
#include <vector>
//
// XrrtAtomicData XRRT interfaces used
//
#include "xrrt_types.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// XrrtAtomicData Ftools/FITSIO interfaces used
//
extern "C" {
#include "fitsio.h"
}

//
// Local enums
//
enum XrrtAtomicDataErrorCode
        {
        XrrtAtomicDataErrors,
        noSuchADTExtension,     // Requested FITS extension not found
        noFitsZ2Col,            // No column named z in FITS extension
        noFitsSymbolCol,        // No column named symbol in FITS extension
        noFitsNameCol,          // No column named name in FITS extension
        noFitsWeightCol,        // No column named weight in FITS extension
        noFitsDensityCol,       // No column named density in FITS extension
        XrrtAtomicDataErrorsEnd
        };


//
// XrrtAtomicData stores selected atomic data for an element
//
class  XrrtAtomicData
{
    public:
          // Constructor
          XrrtAtomicData(); 
          // Copy Constructor
          XrrtAtomicData( const XrrtAtomicData& atomicData ); 
          //
          // Accessor functions
          //
          int getAtomicNumber() const;
          void         setAtomicNumber(const int& number);

          double getAtomicWeight() const;
          void   setAtomicWeight(const double& weight);

          double getNominalCGSDensity() const;
          void   setNominalCGSDensity(const double& cgsDensity);

          string getAtomicSymbol() const;
          void   setAtomicSymbol(const string& symbol);

          string getAtomicName() const;
          void   setAtomicName(const string& name);


    private:
          //
          // Member data
          //
          int atomicNumber;
          double       atomicWeight;
          double       nominalCGSDensity;
          string       atomicSymbol;
          string       atomicName;
};


//
// XrrtAtomicDataTable stores atomic data for all the elements by atomic number
//
class XrrtAtomicDataTable
{
    //
    // theAtomicDataTable() is a friend since it needs to access the private
    // constructor to create the static table
    //
    friend XrrtAtomicDataTable& theAtomicDataTable();

    public:
          //
          // Accessor functions
          //
          int getAtomicNumber(const int& atomicNumber) const;
          void         setAtomicNumber(const int& atomicNumber,
                                       const int& number);

          double getAtomicWeight(const int& atomicNumber) const;
          void   setAtomicWeight(const int& atomicNumber,
                                 const double& weight);

          double getNominalCGSDensity(const int& atomicNumber) const;
          void   setNominalCGSDensity(const int& atomicNumber,
                                      const double& cgsDensity);

          string getAtomicSymbol(const int& atomicNumber) const;
          void   setAtomicSymbol(const int& atomicNumber,
                                 const string& symbol);

          //
          // Reserve space for the required number of elements
          //
          void reserve(int elements);
          //
          // Add an element to the table
          // Note that elements MUST be loaded in Atomic Number order
          //
          void addEntry(XrrtAtomicData& row);
          //
          // Load an entire Atomic Data Table from a FITS file
          //
          void loadTableFromFits(string fitsFileName, string extensionName);
          //
          // Convert error codes to messages
          //
          string errorMessage(XrrtAtomicDataErrorCode errorCode);

    private:
          // Constructor
          XrrtAtomicDataTable();
          // Copy Constructor
          XrrtAtomicDataTable(const XrrtAtomicDataTable& adTable);
          //
          // Elements stored and access by their atomic number
          //
          vector<XrrtAtomicData> atomicData;

};

XrrtAtomicDataTable& theAtomicDataTable();

#endif
