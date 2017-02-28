// xrrtmolecular.hh
//
// Class definition for molecular formulas
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTMOLECULE_HH
#define XRRTMOLECULE_HH

// 
// XrrtMolecule system interfaces used
//
#include <stdio.h>
#include <string>
#include <cctype>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <exception>
using namespace std;

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

//
// Local enums
//
enum XrrtMoleculeErrorCode
        {
        XrrtMoleculeErrors,
        trashCharInFormula,
        XrrtMoleculeErrorsEnd
        };

//
// XrrtMolecule provides functions to store formulas for chemical molecules
// and convert them to useable forms.
//
class  XrrtMolecule
{
    public:
          // Constructor
          XrrtMolecule(); 
          // Copy Constructor
          XrrtMolecule( const XrrtMolecule& molecule ); 
          //
          // Create a complete molecule from a formula.
          //
          void createMolecule(const string& formula);
          //
          // Accessors for molecule contents
          //
          int getNumberOfElements() const;

          string getSymbolFormula() const;

          void getElement(const int& elementNumber,
                          int& atomicNumber,
                          int& numberOfTimesOccurs) const;
          //
          // Convert error code to error message
          //
          string errorMessage(XrrtMoleculeErrorCode errorCode);

    private:
          //
          // Adds an element to a molecule
          void addElement(const int& atomicNumber,
                          const int& numberOfTimesOccurs);
          //
          // Data members
          //
          // Chemical formula as in C6O12H6
          string               symbolFormula;
          //
          // Arrays with the symbolFormula broken down into atomic numbers
          // and the number of times each occurs.
          vector<int> atomicNumberOfElement;
          vector<int> numberOfTimesElementOccurs;
};
#endif
