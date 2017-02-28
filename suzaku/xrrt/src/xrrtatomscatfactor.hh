// xrrtatomscatfactor.hh
//
// Class definition for Atomic Scattering Factors
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink
// 1997/09/19 Replaced error returns by exceptions. R. Fink
// 
// 2005/12/24 Y.ISHISAKI	version 6.2.4
//	change char * -> string in loadTableFromFits()

#ifndef XRRTATOMSCATFACTOR_HH
#define XRRTATOMSCATFACTOR_HH

//
// System interfaces used
//
#include <exception>
#include <string.h>
#include <string>
#include <algorithm>
#include <vector>
using namespace std;

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

// 
// FITSIO interfaces used
//
extern "C" {
#include "fitsio.h"
}

//
// Local enums
//
enum XrrtAtomicScatterFactorsErrorCode
        {
        XrrtAtomicScatterFactorsErrors,
        noSuchExtension,    // FITS extension could not be found
        noFitsZCol,         // No column named z found in FITS extension
        noFitsEnergy2Col,   // No column named energy found in FITS extension
        noFitsF1RealCol,    // No column named f1real found in FITS extension
        noFitsF2ImgCol,     // No column named f2img found in FITS extension
        XrrtAtomicScatterFactorsErrorsEnd
        };

//
// XrrtAtomicScatterFactors stores the Henke type Atomic Scatter Factors
// for a single energy
// See: "Soft X-Ray Optics" by Eberhard Spiller ISBN 0-8194-1654-1
//      Page 11 and forward.
//
class  XrrtAtomicScatterFactors
{
    public:
          // Constructor
          XrrtAtomicScatterFactors(); 
          // Copy Constructor
          XrrtAtomicScatterFactors( const XrrtAtomicScatterFactors& scatter ); 
          //
          // Accessor functions
          //
          double getF1() const;
          void   setF1(const double& f1);

          double getF2() const;
          void   setF2(const double& f2);

          // Note this is in electron volts as per the Henke table design
          double getEnergy() const;
          void   setEnergy(const double& energyIneV);

          //
          // Operators
          //
          bool operator< (const XrrtAtomicScatterFactors& rhs) const;
          bool operator< (const double& rhs) const;
          bool operator== (const XrrtAtomicScatterFactors& rhs) const;
          bool operator== (const double& rhs) const;

    private:
          //
          // Member data
          //
          double energyIneV;
          // double_complex atomicScatterFactor;
          complex<double> atomicScatterFactor;
};

//
// XrrtASFByAtom stores Atomic Scatter Factors at all energies for a single
// atom
//
class XrrtASFByAtom
{
    public:
          // Constructor
          XrrtASFByAtom();
          // Copy Constructor
          XrrtASFByAtom(const XrrtASFByAtom& asfTable);

          //
          // Add a single energy to the table
          //
          void addEntry(const double& energy, const double& f1real,
                        const double& f2img);

          //
          // Since values are stored for specific energies, interpolate for
          // any energy
          double interpolateRealF1(const double& energyIneV);
          double interpolateImgF2(const double& energyIneV);
          //
          // How many energies are stored for this atom?
          //
          int getNumberOfEnergies() const;
          //
          // What energy is a specific row/entry?
          // Since XrrtAtomicScatterFactors stores electron volts, that is
          // what this returns.
          //
          double getTableEnergy(const int& tableEntry) const;

    private:
          //
          // Individual rows (energies) are combined to form a full table
          // for a single atom
          //
          vector<XrrtAtomicScatterFactors> asfByEnergy;

};

//
// XrrtASFTable stores Atomic Scatter Factor tables for all the elements/atoms
// This code is designed to handle atoms 1-92 ONLY.
//
class XrrtASFTable
{
    //
    // theAtomicScatterFactorTable() is a friend since it needs to access the
    // the private constructor since there is only 1 table.
    //
    friend XrrtASFTable& theAtomicScatterFactorTable();

    public:
          //
          // Accessor functions
          //
          double getRealF1(const int& atomicNumber, 
                           const double& energyIneV);
          double getImgF2(const int& atomicNumber, 
                          const double& energyIneV);

          // How many energies for a specific atom?
          int getNumberOFEnergies(const int& atomicNumber);
          // Return energy in electron volts for a specific atom & row
          double getEnergy(const int& atomicNumber, 
                           const int& tableEntry);
          //
          // Load a complete table of all atoms from a FITS file
          //
          void loadTableFromFITS(string inputFile, string extname);
          //
          // Add an atom to the table
          //
          void addTable(const int atomicNumber, XrrtASFByAtom* table);
          //
          // Convert an error code to an error message
          //
          string errorMessage(XrrtAtomicScatterFactorsErrorCode errorCode);

    private:
          // Constructor
          XrrtASFTable();
          // Copy Constructor
          XrrtASFTable(const XrrtASFTable& asfTable);
          
          //
          // Only pointers are stored which implies someone has to do
          // destructors at some point. This code is not implimented since the
          // table is expected to live forever.
          vector<XrrtASFByAtom*> asfTable;
};

XrrtASFTable& theAtomicScatterFactorTable();

#endif
