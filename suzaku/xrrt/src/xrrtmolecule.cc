// xrrtmolecule.cc
//
// Member definition for molecular formulas
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/24 Upgrade documentation. R. Fink

//#include <iostream.h>
#include <cstring>
#include "xrrtmolecule.hh"

XrrtMolecule::XrrtMolecule():
    symbolFormula(" "),
    atomicNumberOfElement(),
    numberOfTimesElementOccurs()
{
// A very simple constructor
}

XrrtMolecule::XrrtMolecule(const XrrtMolecule& molecule):
    symbolFormula(" "),
    atomicNumberOfElement(),
    numberOfTimesElementOccurs()
{
// 
// Copy constructor
//
    symbolFormula = molecule.symbolFormula;
    // 
    // Reserve enough space in the vectors to hold the old vector contents
    //
    atomicNumberOfElement.reserve(molecule.atomicNumberOfElement.size());
    numberOfTimesElementOccurs.reserve(molecule.numberOfTimesElementOccurs.size());
    //
    // Use the STL to copy each vector
    //
    copy(molecule.atomicNumberOfElement.begin(),
         molecule.atomicNumberOfElement.end(),
         atomicNumberOfElement.begin());
    copy(molecule.numberOfTimesElementOccurs.begin(),
         molecule.numberOfTimesElementOccurs.end(),
         numberOfTimesElementOccurs.begin());
}

string
XrrtMolecule::errorMessage(XrrtMoleculeErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorString;

    char number[255];
    switch (errorCode)
        {
        case trashCharInFormula:
            errorString = 
            "Part of the surface formula can not be interpreted; which part I can not tell you because g++ exceptions don't work right.";
            break;
        default:
            errorString = 
            "XrrtMolecule::errorMessage has no text for error code = ";
            sprintf(number, "%d", errorCode);
            errorString.append(number);
            break;
        }
    return errorString;
}

void
XrrtMolecule::createMolecule(const string& formula)
{
//
// Convert a character based formula in to an full XrrtMolecule
//
//
// All the allowed elemental symbols
//
string atomicSymbols[93] = { " ",
"H",  "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne", "Na", "Mg", "Al",
"Si", "P",  "S",  "Cl", "Ar", "K",  "Ca", "Sc", "Ti", "V",  "Cr", "Mn", "Fe",
"Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",
"Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te",
"I",  "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb",
"Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt",
"Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa",
"U"};

string* atomicSymbolsPtr;

    //
    // Decode a formula in the form AAnBBnCCn where AA,BB,CC are atomic
    // symbols and n is the number of times that atom occurs
    //

    //
    // Scan over the formula, removing each element as it is found along
    // with a numeric repeat value if supplied
    string localFormula = formula;
    string elementSymbol;
    string numberString;
    int atomicNumber = 0;
    int elementRepeats = 0;
    // Note that localFormula is destroyed in this code
    while (localFormula.length() != 0)
        {
        // Check for a space
        if (localFormula[0] == ' ')
           {
           // Destroy the space and continue
           localFormula.erase(0,1);
           continue;
           }
        // Check for an upper case letter which begins an element symbol
        if (isupper(localFormula[0]))
           {
           // Save the upper case letter as the beginning of element symbol
           elementSymbol = localFormula.substr(0,1);
           localFormula.erase(0,1);
           // Check for a following lower case as part of the symbol
           if (islower(localFormula[0]))
              {
              // save the 2nd part of the symbol
              elementSymbol.append(localFormula.substr(0,1));
              localFormula.erase(0,1);
              }
           // Does a numeric value follow?
           if (isdigit(localFormula[0]))
              {
              // extract the numeric value
              numberString = localFormula.substr(0,1);
              localFormula.erase(0,1);
              while (localFormula.length() != 0)
                  {
                  if (isdigit(localFormula[0]))
                     {
                     // extract the numeric value
                     numberString.append(localFormula.substr(0,1));
                     localFormula.erase(0,1);
                     continue;
                     }
                  else
                     {
                     // We reached the end of the number
                     break;
                     }
                  }
              // convert the numeric value
              elementRepeats = atoi(numberString.c_str());
              }
           else
              {
              // Set a default of 1 for the number of element occurances
              elementRepeats = 1;
              }
           // We now have both a symbol and a repeat number   
           // Search for the symbol in the table
           // find is broken in gcc 2.7.2.2
           char charElementSymbol[255];
           strcpy(charElementSymbol, elementSymbol.c_str());
           atomicSymbolsPtr = find(&atomicSymbols[0], 
                                   &atomicSymbols[92], 
                                   charElementSymbol);
           atomicNumber = atomicSymbolsPtr - &atomicSymbols[0];
           if (atomicNumber > 0)
              {
              // Add this element to the molecule
              addElement(atomicNumber, elementRepeats);
              continue;
              }
           }
        // We have found found trash in the formula
        throw trashCharInFormula;
        }
   symbolFormula = formula;
}


int 
XrrtMolecule::getNumberOfElements() const
{
     return atomicNumberOfElement.size();
}

string
XrrtMolecule::getSymbolFormula() const
{
    return symbolFormula;
}

void
XrrtMolecule::addElement(const int& atomicNumber,
                         const int& numberOfTimesOccurs)
{
     atomicNumberOfElement.push_back(atomicNumber);
     numberOfTimesElementOccurs.push_back(numberOfTimesOccurs);
}

void
XrrtMolecule::getElement(const int& elementNumber,
                         int& atomicNumber,
                         int& numberOfTimesOccurs) const
{
    if (elementNumber < 0 || (unsigned int)elementNumber > atomicNumberOfElement.size())
       {
       atomicNumber = 0;
       numberOfTimesOccurs = 0;
       }
    else
       {
       atomicNumber = atomicNumberOfElement[elementNumber];
       numberOfTimesOccurs = numberOfTimesElementOccurs[elementNumber];
       }
}

