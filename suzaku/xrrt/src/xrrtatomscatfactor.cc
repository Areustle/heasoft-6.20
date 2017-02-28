// xrrtatomscatfactor.cc
//
// Member definition for Atomic Scattering Factors
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/19 Replaced error returns by exceptions. R. Fink
// 1997/09/24 Upgraded documentation. R. Fink
// 
// 2005/12/24 Y.ISHISAKI	version 6.2.4
//	change char * -> string in loadTableFromFits()

//#include <iostream.h>
#include "xrrtatomscatfactor.hh"

XrrtASFTable& theAtomicScatterFactorTable()
{
//
// Provide static access to the only Atomic Scatter Factor table in the
// program
//
static XrrtASFTable theTable;

     return theTable;
}

XrrtASFTable::XrrtASFTable():
    asfTable()
{
// A simple constructor; there is nothing to do.
}

XrrtASFByAtom::XrrtASFByAtom():
    asfByEnergy()
{
// A simple constructor; there is nothing to do.
}

XrrtAtomicScatterFactors::XrrtAtomicScatterFactors():
    energyIneV(0),
    atomicScatterFactor()
{
// A simple constructor
}

XrrtAtomicScatterFactors::XrrtAtomicScatterFactors(const XrrtAtomicScatterFactors& scatterData):
    energyIneV(0),
    atomicScatterFactor()
{
//
// A simple copy constructor
//
    energyIneV = scatterData.energyIneV;
    atomicScatterFactor = scatterData.atomicScatterFactor;
}

double 
XrrtASFTable::getRealF1(const int& atomicNumber, 
                           const double& energyIneV)
{
       return (asfTable[atomicNumber])->interpolateRealF1(energyIneV);
}

double 
XrrtASFTable::getImgF2(const int& atomicNumber, 
                          const double& energyIneV)
{
    return (asfTable[atomicNumber])->interpolateImgF2(energyIneV);
}

double
XrrtASFByAtom::interpolateRealF1(const double& energyIneV)
{
// WARNING!!! DO NOT CHANGE THIS CODE WITHOUT DETAILED TESTING. gcc 2.7.2.2
// GENERATED VERY WEIRD (INCORRECT) CODE FOR SLIGHT CHANGES IN THIS CODE!!!

// The atomic scatter factors, F1 and F2, which are stored as their
// complex form F1 + iF2, are provided in the Henke tables as values
// at specific energy points. In between these energies, they must
// be interpolated. This code does that for the F1 value. Because
// the values can jump by large steps at atomic lines, only simple
// bin to bin interpolation is used.

vector<XrrtAtomicScatterFactors>::iterator upper;

    // lower_bound returns the 1st place in the table that energyIneV
    // could be inserted and still maintain sequential order. Thus it
    // returns a pointer to the bin >= energyIneV.
    upper = lower_bound(asfByEnergy.begin(), asfByEnergy.end(), energyIneV);

    // If the (>= bin) >  the last bin, then return the last bin
    // If the (>= bin) == the last bin, then return the last bin 
    // If the (>= bin) == the 1st  bin, then return the 1st  bin
    // else interpolate with the next bin DOWN
    if (upper == asfByEnergy.end())
       {
       // Beyond the table range
       return  (asfByEnergy.back()).getF1();
       }
    else if (*upper == asfByEnergy.back())
       {
       // The last bin
       return (*upper).getF1();
       }
    else if (*upper == asfByEnergy.front())
       {
       // The first bin
       return (*upper).getF1();
       }
    else
       {
       // Interpolate with the next bin down
       vector<XrrtAtomicScatterFactors>::iterator lower = upper;
       lower--;
       double fraction;
       double realF1;
       fraction = (energyIneV - (*lower).getEnergy()) / 
                  ((*upper).getEnergy() - (*lower).getEnergy());
       realF1 = (*lower).getF1() + 
                 fraction * ((*upper).getF1() - (*lower).getF1());
       return realF1;
       }
}

double
XrrtASFByAtom::interpolateImgF2(const double& energyIneV)
{
// WARNING!!! DO NOT CHANGE THIS CODE WITHOUT DETAILED TESTING. gcc 2.7.2.2
// GENERATED VERY WEIRD (INCORRECT) CODE FOR SLIGHT CHANGES IN THIS CODE!!!

// The atomic scatter factors, F1 and F2, which are stored as their
// complex form F1 + iF2, are provided in the Henke tables as values
// at specific energy points. In between these energies, they must
// be interpolated. This code does that for the F2 value. Because
// the values can jump by large steps at atomic lines, only simple
// bin to bin interpolation is used.

vector<XrrtAtomicScatterFactors>::iterator upper;

    // lower_bound returns the 1st place in the table that energyIneV
    // could be inserted and still maintain sequential order. Thus it
    // returns a pointer to the bin >= energyIneV.
    upper = lower_bound(asfByEnergy.begin(), asfByEnergy.end(), energyIneV);

    // If the (>= bin) >  the last bin, then return the last bin
    // If the (>= bin) == the last bin, then return the last bin 
    // If the (>= bin) == the 1st  bin, then return the 1st  bin
    // else interpolate with the next bin DOWN
    if (upper == asfByEnergy.end())
       {
       // Beyond the table range
       return (asfByEnergy.back()).getF2();
       }
    else if (*upper == asfByEnergy.back())
       {
       // The last bin
       return (asfByEnergy.back()).getF2();
       }
    else if (*upper == asfByEnergy.front())
       {
       // The first bin
       return (*upper).getF1();
       }
    else
       {
       // Interpolate with the next bin down
       vector<XrrtAtomicScatterFactors>::iterator lower = upper;
       lower--;
       double fraction;
       double imgF2;
       fraction = (energyIneV - (*lower).getEnergy()) / 
                  ((*upper).getEnergy() - (*lower).getEnergy());
       imgF2 = (*lower).getF2() + 
                 fraction * ((*upper).getF2() - (*lower).getF2());
       return imgF2;
       }
}


void 
XrrtASFTable::loadTableFromFITS(string atomicScatterFactorsFileName,
                                string extensionName)
{
//
// This function loads the Atomic Scatter Factors f1 and f2 from a FITS file
// extension.
//

//
// CFITSIO API calls by FITS column. If the column type changes, these
// must change.
//
#define FITS_READ_COL_Z       fits_read_col_int
#define FITS_READ_COL_ENERGY  fits_read_col_dbl
#define FITS_READ_COL_F1REAL  fits_read_col_dbl
#define FITS_READ_COL_F2IMG   fits_read_col_dbl
//
// The Names of the FITS columns.
//
char * const FITS_COLNAME_Z      = "z";
char * const FITS_COLNAME_ENERGY = "energy";
char * const FITS_COLNAME_F1REAL = "f1real";
char * const FITS_COLNAME_F2IMG  = "f2img";

// Local variables


// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return
char     fitsChar[256];     // Dummy catch for FITS return string
char     fitsComment[256];  // Dummy catch for FITS Comment return
long     fitsRowLimit;
int      fitsRow;
int      fitsAnyNull;


// The file name should be clean (i.e. a unix file name) but Ftools
// use an appended [number] to signify which extension to use. I
// never use that format so I have to make sure it is stripped if
// someone used it out of habit.
//
// Determine whether the file names are "clean"; i.e. lack [#]
//     strip them if so, and convert to char
const char *charAtomicScatterFactorsFile= atomicScatterFactorsFileName.c_str();

//
// Open the Atomic Scatter Factors file for INPUT only 
//
fitsfile* fitsAtomicScatterFactors;    // FITSIO FILE* pointer
fitsStatus = 0;
fitsReturn = fits_open_file( &fitsAtomicScatterFactors, 
                             charAtomicScatterFactorsFile,
                             READONLY,       &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

// Find the requested extension in the FITS file
// Scan all possible extentions looking for the requested extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
const char *charExtensionName = extensionName.c_str();
for (int i=2; i<10000; i++)
    {
    //
    // Skip to the indicated extension and determine its existance
    fitsReturn = fits_movabs_hdu(fitsAtomicScatterFactors, i, &fitsHduType, 
                                 &fitsStatus);
    //
    // If EOF, we never found the requested extension
    if (fitsStatus == END_OF_FILE)
       {
       throw noSuchExtension;
       }
    //
    // On any other error, abort
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Read the contents of the EXTNAME keyword
    fitsReturn = fits_read_key_str(fitsAtomicScatterFactors, "EXTNAME", 
                                   fitsChar, fitsComment, &fitsStatus);
    //
    // If anything except keyword found occurs, skip to the next extension
    if (fitsStatus != NO_ERROR)
       {
       continue;
       }
    //
    // Use case insensitive compare to compare the contents of the EXTNAME
    // keyword and our extension name.
    if ((strcasecmp(fitsChar, charExtensionName)) == 0)
       {
       // requested extension found
       break;
       }
    }
// The only way to fall thru here is the break above. Since we  examine
// all possible extensions, the EOF will trip if our extension is
// not found.

// We now have the requested extension as the file position

// Find the FITS file matching columns for the required column names

// FITSIO column references
int      fitsColZ;
int      fitsColEnergy;
int      fitsColF1Real;
int      fitsColF2Img;

fitsReturn = fits_get_colnum(fitsAtomicScatterFactors, FALSE, FITS_COLNAME_Z,
                             &fitsColZ, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsZCol;
   }

fitsReturn = fits_get_colnum(fitsAtomicScatterFactors, CASEINSEN, 
                             FITS_COLNAME_ENERGY,
                             &fitsColEnergy, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsEnergy2Col;
   }

fitsReturn = fits_get_colnum(fitsAtomicScatterFactors, CASEINSEN, 
                             FITS_COLNAME_F1REAL,
                             &fitsColF1Real, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsF1RealCol;
   }

fitsReturn = fits_get_colnum(fitsAtomicScatterFactors, CASEINSEN, 
                             FITS_COLNAME_F2IMG,
                             &fitsColF2Img, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsF2ImgCol;
   }

//
// Find out how many rows we have in the FITS table
//
fitsReturn = fits_read_key_lng(fitsAtomicScatterFactors, "NAXIS2", 
                               &fitsRowLimit,
                               fitsComment, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

//
// Begin loading the atomic scatter factors into the ASF class class
//
// FITS column return values
int    atomicNumber;
double energy;
double f1Real;
double f2Img;
//
int lastAtomicNumber = 0;
XrrtASFByAtom* atomicData = 0;
// 
// Insert a dummy entry for the zero atomic number
theAtomicScatterFactorTable().addTable(lastAtomicNumber, atomicData);
//
// For all rows in the FITS table
for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++)
   {
   //
   // Atomic number column
   //
   fitsReturn = FITS_READ_COL_Z(fitsAtomicScatterFactors, fitsColZ,
                                fitsRow, 1L, 1L, 0, &atomicNumber,
                                &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   
   // If the atomic number changes, add a new entry to the table to hold
   // its atomic scatter factors
   if (atomicNumber != lastAtomicNumber)
      {
      if (lastAtomicNumber > 0)
         {
         theAtomicScatterFactorTable().addTable(lastAtomicNumber, atomicData);
         }
      lastAtomicNumber = atomicNumber;
      // Obtain an atomic data object
      atomicData = new XrrtASFByAtom;
      }

   //
   // Energy column
   //
   fitsReturn = FITS_READ_COL_ENERGY(fitsAtomicScatterFactors, fitsColEnergy,
                                     fitsRow, 1L, 1L, 0, 
                                     &energy,
                                     &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      return;
      }

   //
   // Real part of the Atomic Scatter Factor
   //
   fitsReturn = FITS_READ_COL_F1REAL(fitsAtomicScatterFactors, fitsColF1Real,
                                       fitsRow, 1L, 1L, 0, 
                                       &f1Real,
                                       &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }

   //
   // Imaginary part of the Atomic Scatter Factor
   //
   fitsReturn = FITS_READ_COL_F2IMG(fitsAtomicScatterFactors, fitsColF2Img,
                                       fitsRow, 1L, 1L, 0,
                                       &f2Img,
                                       &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }

   //
   // Add the scatter factor entry to the table
   //
   atomicData->addEntry(energy, f1Real, f2Img);

   }

//
// Close the ASF file
//
fitsReturn = fits_close_file(fitsAtomicScatterFactors,  &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
}

string
XrrtASFTable::errorMessage(XrrtAtomicScatterFactorsErrorCode errorCode)
{
//
// Convert erro codes to error messages
//
string errorMessage = "Bug in XrrtASFTable::errorMessage()";

    switch (errorCode)
        {
        case noFitsZCol:
             errorMessage = 
             "Missing atomic number (z) column in Atomic Scattering Factor FITS file.";
             break;
        case noSuchExtension:
             errorMessage = 
              "There is no extension in the Atomic Scattering Factor FITS file with the requested name.";
             break;
        case noFitsEnergy2Col:
             errorMessage = 
             "Missing energy (energy) column in Atomic Scattering Factor FITS file.";
             break;
        case noFitsF1RealCol:
             errorMessage = 
             "Missing F1Real (f1real) column in Atomic Scattering Factor FITS file.";
             break;
        case noFitsF2ImgCol:
             errorMessage = 
             "Missing F2Img (f2img) column in Atomic Scattering Factor FITS file.";
             break;
        default:
             char number[255];
             sprintf(number, "%d", errorCode);
             errorMessage = "XrrtASFTable::errorMessage has no message for error code = ";
             errorMessage.append(number);
             break;
        }
    return errorMessage;
}



double
XrrtAtomicScatterFactors::getEnergy() const
{
     return energyIneV;
}

void
XrrtAtomicScatterFactors::setEnergy(const double& energy)
{
    energyIneV = energy;
}

double
XrrtAtomicScatterFactors::getF1() const
{
     return atomicScatterFactor.real();
}

void
XrrtAtomicScatterFactors::setF1(const double& f1)
{
  // Modified by H. Mori (2005/09/14)
  // double_complex newValue(f1,atomicScatterFactor.imag());
  complex<double> newValue(f1,atomicScatterFactor.imag());
     atomicScatterFactor = newValue;
}

double
XrrtAtomicScatterFactors::getF2() const
{
     return atomicScatterFactor.imag();
}

void
XrrtAtomicScatterFactors::setF2(const double& f2)
{
  // Modified by H. Mori (2005/09/14)
  // double_complex newValue(atomicScatterFactor.real(), f2);
  complex<double> newValue(atomicScatterFactor.real(), f2);
     atomicScatterFactor = newValue;
}

bool
XrrtAtomicScatterFactors::operator< (const XrrtAtomicScatterFactors& rhs) const
{
    return (energyIneV < rhs.energyIneV);
}

bool
XrrtAtomicScatterFactors::operator< (const double& rhs) const
{
    return (energyIneV < rhs);
}

bool
XrrtAtomicScatterFactors::operator== (const XrrtAtomicScatterFactors& rhs) const
{
    return (energyIneV == rhs.energyIneV);
}

bool
XrrtAtomicScatterFactors::operator== (const double& rhs) const
{
    return (energyIneV == rhs);
}


void
XrrtASFByAtom::addEntry(const double& energy, const double& f1real,
                        const double& f2img)
{
XrrtAtomicScatterFactors atomicData;
     atomicData.setEnergy(energy);
     atomicData.setF1(f1real);
     atomicData.setF2(f2img);
     asfByEnergy.push_back(atomicData);
}

int 
XrrtASFByAtom::getNumberOfEnergies() const
{
    return asfByEnergy.size();
}

double
XrrtASFByAtom::getTableEnergy(const int& tableEntry) const
{
    return (asfByEnergy[tableEntry]).getEnergy();
}



void
XrrtASFTable::addTable(const int atomicNumber, XrrtASFByAtom* table)
{
    // Reserve entries for elements 1-92 plus a dummy 0 on 1st entry.
    if (asfTable.size() < 93)
       {
       for (int i=0;i<93;i++)
          {
          asfTable.push_back((XrrtASFByAtom*) 0);
          }
       }
    asfTable[atomicNumber]=table;
}

int 
XrrtASFTable::getNumberOFEnergies(const int& atomicNumber)
{
    return (asfTable[atomicNumber])->getNumberOfEnergies();
}

double 
XrrtASFTable::getEnergy(const int& atomicNumber, 
                        const int& tableEntry)
{
    return (asfTable[atomicNumber])->getTableEnergy(tableEntry);
}

