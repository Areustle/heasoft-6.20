// xrrtatomicdata.cc
//
// Member definition for Atomic Data Factors
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/18 Replaced error returns by exceptions. R. Fink
// 1997/09/24 Upgrade documentation. R. Fink
// 
// 2005/12/24 Y.ISHISAKI	version 6.2.4
//	change char * -> string in loadTableFromFits()

//#include <iostream.h>
#include "xrrtatomicdata.hh"

XrrtAtomicData::XrrtAtomicData():
    atomicNumber(0),
    atomicWeight(0),
    nominalCGSDensity(0),
    atomicSymbol(""),
    atomicName("")
{
// A simple constructor; nothing special.
}

XrrtAtomicDataTable::XrrtAtomicDataTable():
    atomicData()
{
    //
    // Create a dummy entry for atomic number 0 (zero) which does not exist
    // but which C++ vector design needs as a filler
    //
    XrrtAtomicData numberZero;
    //
    // Save room for all the standard elements
    //
    atomicData.reserve(93);
    //
    // Insert the dummy entry in the table of the elements
    //
    atomicData.push_back(numberZero);
}

XrrtAtomicDataTable& theAtomicDataTable()
{
//
// Create a single static AtomicDataTable that can be accessed by all parts
// of the program.
//
static XrrtAtomicDataTable table;

    return table;
}

XrrtAtomicData::XrrtAtomicData(const XrrtAtomicData& atomicData):
    atomicNumber(0),
    atomicWeight(0),
    nominalCGSDensity(0),
    atomicSymbol(""),
    atomicName("")
{
//
// A simple copy constructor; nothing special here.
//
    atomicNumber = atomicData.atomicNumber;
    atomicWeight = atomicData.atomicWeight;
    nominalCGSDensity = atomicData.nominalCGSDensity;
    atomicSymbol = atomicData.atomicSymbol;
    atomicName   = atomicData.atomicName;
}

string
XrrtAtomicDataTable::errorMessage(XrrtAtomicDataErrorCode errorCode)
{
//
// Convert error codes to error messages
//

string errorMessage = "Bug in XrrtAtomicData::errorMessage()";

    switch (errorCode)
        {
        case noSuchADTExtension:
             errorMessage = 
             "No extension with the requested name was found in the Atomic Data File";
             break;
        case noFitsZ2Col:
             errorMessage = 
             "Missing atomic number (z) column in atomic data file";
             break;
        case noFitsSymbolCol:
             errorMessage = 
             "Missing atomic symbol (symbol) column in atomic data file";
             break;
        case noFitsNameCol:
             errorMessage = 
             "Missing atomic name (name) column in atomic data file";
             break;
        case noFitsWeightCol:
             errorMessage = 
             "Missing atomic weight (weight) column in atomic data file";
             break;
        case noFitsDensityCol:
             errorMessage = 
             "Missing density (density) column in atomic data file";
             break;
        default:
             char number[255];
             sprintf(number, "%d", errorCode);
             errorMessage = "XrrtAtomicDataTable::errorMessage has no message for error code = ";
             errorMessage.append(number);
             break;
        }
    return errorMessage;
}

void
XrrtAtomicDataTable::loadTableFromFits(string atomicDataFileName,
                                       string extensionName)
{
//
// This function loads an Atomic Data table from a FITS file using
// CFITSIO API. The defines below are local to this function.

//
// Function calls in the CFITSIO API
// These must be changed if the data type of the column changes.
//
#define FITS_READ_COL_Z       fits_read_col_int
#define FITS_READ_COL_SYMBOL  fits_read_col_str
#define FITS_READ_COL_NAME    fits_read_col_str
#define FITS_READ_COL_WEIGHT  fits_read_col_dbl
#define FITS_READ_COL_DENSITY fits_read_col_dbl
//
// The names of the columns in the FITS file.
//
char * const FITS_COLNAME_Z       = "z";
char * const FITS_COLNAME_SYMBOL  = "symbol";
char * const FITS_COLNAME_NAME    = "name";
char * const FITS_COLNAME_WEIGHT  = "weight";
char * const FITS_COLNAME_DENSITY = "density";

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

// FITSIO column references
int      fitsColZ;
int      fitsColWeight;
int      fitsColSymbol;
int      fitsColName;
int      fitsColDensity;



// It is standard in Ftools to give FITS file extensions as [number] on the
// end of the file name. I try never to obey that standard, prefering
// to use EXTNAME keywords and ID extensions by name.
//
// Determine whether the file name is "clean"; i.e. lacks [#]
//     strip it if not, and convert to char

const char *charAtomicDataFileName = atomicDataFileName.c_str();

//
// Open the Atomic Data file for input ONLY
//
fitsfile* fitsAtomicData;    // FITSIO FILE* pointer
fitsStatus = 0;
fitsReturn = fits_open_file( &fitsAtomicData, 
                             charAtomicDataFileName,
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
    // Move the indicated extension in the FITS file
    fitsReturn = fits_movabs_hdu(fitsAtomicData, i, &fitsHduType, 
                                 &fitsStatus);
    //
    // If we hit EOF, we never found the requested extension
    if (fitsStatus == END_OF_FILE)
       {
       throw noSuchADTExtension;
       }
    //
    // If any error occurs, bail out.
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    //
    // Retrieve the contents of the EXTNAME keyword in the extension
    fitsReturn = fits_read_key_str(fitsAtomicData, "EXTNAME", 
                                   fitsChar, fitsComment, &fitsStatus);
    // 
    // Assume that any return other than keyword found means the
    // keyword was missing; thus we ignore ALL errors here.
    if (fitsStatus != NO_ERROR)
       {
       // The extension did not have an EXTNAME keyword
       continue;
       }
    //
    // Compare the contents of the EXTNAME keyword to our extension name
    // without respect to case
    if ((strcasecmp(fitsChar, charExtensionName)) == 0)
       {
       // requested extension found
       break;
       }
    }
// The only way we can fall out the bottom of this loop is via the break above.
// This is because we check far more extensions than a legal FITS file can have.
// Thus the EOF case will always trip.

// We now have the requested extension as the file position

//
// Find the FITS file matching columns for the required column names
//
fitsReturn = fits_get_colnum(fitsAtomicData, CASEINSEN, FITS_COLNAME_Z,
                             &fitsColZ, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsZ2Col;
   }

fitsReturn = fits_get_colnum(fitsAtomicData, CASEINSEN, 
                             FITS_COLNAME_SYMBOL,
                             &fitsColSymbol, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsSymbolCol;
   }

fitsReturn = fits_get_colnum(fitsAtomicData, CASEINSEN, 
                             FITS_COLNAME_NAME,
                             &fitsColName, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsNameCol;
   }

fitsReturn = fits_get_colnum(fitsAtomicData, CASEINSEN, 
                             FITS_COLNAME_WEIGHT,
                             &fitsColWeight, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsWeightCol;
   }

fitsReturn = fits_get_colnum(fitsAtomicData, CASEINSEN, 
                             FITS_COLNAME_DENSITY,
                             &fitsColDensity, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noFitsDensityCol;
   }

//
// Find out how many rows we have in the FITS table
//
fitsReturn = fits_read_key_lng(fitsAtomicData, "NAXIS2", 
                               &fitsRowLimit,
                               fitsComment, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

//
//  Based on the number of rows in the FITS table, reserve enough space 
//  in the table for all the elements.
//  Use the function because it accounts for the zero element.
//
theAtomicDataTable().reserve((int) fitsRowLimit);

//
// Begin loading the atomic data into the AtomicData class
//
//
// FITS column return values
// These match up with the column names/contents
//
int    atomicNumber;
char   charAtomicSymbol[255];
char   charAtomicName[255];
char*  charArray[2];
charArray[0] = 0;
charArray[1] = 0;
char** charArrayPtr = &charArray[0];
string atomicSymbol = "NONE";
string atomicName   = "NONE";
double atomicWeight;
double density;
//
// An object to hold element data
//
XrrtAtomicData atomicData;
//
// For each row, read each column and store its contents
// We ignore the possibility of NULL values in the FITS file columns.
//
for (fitsRow=1; fitsRow <= fitsRowLimit; fitsRow++)
   {
   //
   // Atomic Number
   //
   fitsReturn = FITS_READ_COL_Z(fitsAtomicData, fitsColZ,
                                fitsRow, 1L, 1L, 0, &atomicNumber,
                                &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }

   atomicData.setAtomicNumber(atomicNumber);
   
   //
   // Atomic symbol (e.g. He, C, Fe)
   //
   charArray[0] = charAtomicSymbol;
   strcpy(charAtomicSymbol, "FAILURE");
   fitsReturn = FITS_READ_COL_SYMBOL(fitsAtomicData, fitsColSymbol,
                                     fitsRow, 1L, 1L, " ", 
                                     charArrayPtr,
                                     &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   atomicSymbol = charAtomicSymbol;
   atomicData.setAtomicSymbol(atomicSymbol);

   //
   // Atomic Name (e.g. Carbon, Iron)
   //
   charArray[0] = charAtomicName;
   fitsReturn = FITS_READ_COL_NAME(fitsAtomicData, fitsColName,
                                       fitsRow, 1L, 1L, " ", 
                                       charArrayPtr,
                                       &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   atomicName = charAtomicName;
   atomicData.setAtomicName(atomicName);

   //
   // Atomic Weight
   //
   fitsReturn = FITS_READ_COL_WEIGHT(fitsAtomicData, fitsColWeight,
                                       fitsRow, 1L, 1L, 0,
                                       &atomicWeight,
                                       &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   atomicData.setAtomicWeight(atomicWeight);

   //
   // Specific density
   //
   fitsReturn = FITS_READ_COL_DENSITY(fitsAtomicData, fitsColDensity,
                                       fitsRow, 1L, 1L, 0,
                                       &density,
                                       &fitsAnyNull, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   atomicData.setNominalCGSDensity(density);

   // 
   // Push the atomic data onto the table
   //
   theAtomicDataTable().addEntry(atomicData);
   }

//
// Close the ASF file
//
fitsReturn = fits_close_file(fitsAtomicData,  &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

}



int
XrrtAtomicData::getAtomicNumber() const
{
     return atomicNumber;
}

void
XrrtAtomicData::setAtomicNumber(const int& number)
{
    atomicNumber = number;
}

double
XrrtAtomicData::getAtomicWeight() const
{
    return atomicWeight;
}

void
XrrtAtomicData::setAtomicWeight(const double& weight)
{
    atomicWeight = weight;
}

double
XrrtAtomicData::getNominalCGSDensity() const
{
    return nominalCGSDensity;
}

void
XrrtAtomicData::setNominalCGSDensity(const double& cgsDensity)
{
    nominalCGSDensity = cgsDensity;
}

string 
XrrtAtomicData::getAtomicSymbol() const
{
     return atomicSymbol;
}

void 
XrrtAtomicData::setAtomicSymbol(const string& symbol)
{
    atomicSymbol = symbol;
}

string 
XrrtAtomicData::getAtomicName() const
{
     return atomicName;
}

void 
XrrtAtomicData::setAtomicName(const string& name)
{
    atomicName = name;
}



int
XrrtAtomicDataTable::getAtomicNumber(const int& atomicNumber) const
{
    return atomicData[atomicNumber].getAtomicNumber();
}

void
XrrtAtomicDataTable::setAtomicNumber(const int& atomicNumber,
                                     const int& number)
{
    atomicData[atomicNumber].setAtomicNumber(number);
}

double
XrrtAtomicDataTable::getAtomicWeight(const int& atomicNumber) const
{
    return atomicData[atomicNumber].getAtomicWeight();
}

void
XrrtAtomicDataTable::setAtomicWeight(const int& atomicNumber,
                                     const double& weight)
{
    atomicData[atomicNumber].setAtomicWeight(weight);
}

double
XrrtAtomicDataTable::getNominalCGSDensity(const int& atomicNumber) const
{
    return atomicData[atomicNumber].getNominalCGSDensity();
}

void
XrrtAtomicDataTable::setNominalCGSDensity(const int& atomicNumber,
                                          const double& cgsDensity)
{
    atomicData[atomicNumber].setNominalCGSDensity(cgsDensity);
}

string 
XrrtAtomicDataTable::getAtomicSymbol(const int& atomicNumber) const
{
     return atomicData[atomicNumber].getAtomicSymbol();
}

void 
XrrtAtomicDataTable::setAtomicSymbol(const int& atomicNumber,
                                     const string& symbol)
{
    atomicData[atomicNumber].setAtomicSymbol(symbol);
}

void
XrrtAtomicDataTable::addEntry(XrrtAtomicData& data)
{
    atomicData.push_back(data);
}

void
XrrtAtomicDataTable::reserve(int elements)
{
    atomicData.reserve(elements++);
}
