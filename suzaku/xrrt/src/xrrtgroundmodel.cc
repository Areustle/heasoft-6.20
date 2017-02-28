// xrrtgroundmodel.cc
//
// Member definition for photon source (Ground model)
// Richard L Fink GSFC/631
// 1997/09/16
// 1997/09/24 Upgrade documentation. R. Fink

#include "xrrtgroundmodel.hh"

XrrtGroundModel::XrrtGroundModel():
    groundModelOpen(false),
    groundModelFile(0),
    energyFitsColNum(0),
    xFitsColNum(0),
    yFitsColNum(0),
    zFitsColNum(0),
    unitxFitsColNum(0),
    unityFitsColNum(0),
    unitzFitsColNum(0),
    maxFitsRowNumber(0),
    fitsRowNumber(0)
{
// A simple constructor
}

string
XrrtGroundModel::errorMessage(XrrtGroundModelErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case groundFileAlreadyOpen:
            errorMessage = 
            "Attempt to open a second Ground Model FITS file";
            break;
        case noGroundModelExtension:
            errorMessage = 
            "The requested Ground Model extension could not be found";
            break;
        case noGroundEnergyFitsCol:
            errorMessage =
            "The Ground Model FITS file has no energy column";
            break;
        case noGroundXFitsCol:
            errorMessage =
            "The Ground Model FITS file has no x column";
            break;
        case noGroundYFitsCol:
            errorMessage =
            "The Ground Model FITS file has no y column";
            break;
        case noGroundZFitsCol:
            errorMessage =
            "The Ground Model FITS file has no z column";
            break;
        case noGroundUnitxFitsCol:
            errorMessage =
            "The Ground Model FITS file has no unitx column";
            break;
        case noGroundUnityFitsCol:
            errorMessage =
            "The Ground Model FITS file has no unity column";
            break;
        case noGroundUnitzFitsCol:
            errorMessage =
            "The Ground Model FITS file has no unitz column";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                          "XrrtGroundModel::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}
void
XrrtGroundModel::openGroundFitsFile(string fileName, string extensionName)
{
//
// Open a Ground-Type FITS file model and leave it read to be read
//

//
// FITS extension keyword name
//
char * const FITS_EXTENSION_NAME = "EXTNAME";
//
// FITS extension column names
//
char * const FITS_COLNAME_ENERGY = "energy";
char * const FITS_COLNAME_X      = "x";
char * const FITS_COLNAME_Y      = "y";
char * const FITS_COLNAME_Z      = "z";
char * const FITS_COLNAME_UNITX  = "unitx";
char * const FITS_COLNAME_UNITY  = "unity";
char * const FITS_COLNAME_UNITZ  = "unitz";

// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return
char     fitsChar[256];     // Dummy catch for FITS return string
char     fitsComment[256];  // Dummy catch for FITS Comment return
long     fitsRowLimit;


//
// 1st check that a ground file is not already open
//
if (groundModelOpen)
   {
   throw groundFileAlreadyOpen;
   }

//
// Strip any unneeded [number] appended on the file name by user force of habit
// Clean up the fileName string
size_t cleanLength = fileName.find_last_not_of('[');
cleanLength++;
fileName.resize(cleanLength);


//
// Open the ground file for INPUT only
//
fitsStatus = 0;
fitsReturn = fits_open_file( &groundModelFile, (char*) fileName.c_str(),
                             READONLY,       &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

// Find the requested extension
// Scan all possible extentions looking for the extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
for (int i=2; i<10000; i++)
    {
    fitsReturn = fits_movabs_hdu(groundModelFile, i, &fitsHduType, &fitsStatus);
    if (fitsStatus == END_OF_FILE)
       {
       throw noGroundModelExtension;
       }
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    fitsReturn = fits_read_key_str(groundModelFile, FITS_EXTENSION_NAME, 
                                   fitsChar, fitsComment, &fitsStatus);
    if ((strcasecmp(fitsChar, extensionName.c_str())) == 0)
       {
       // extension found
       break;
       }
    }

// We now have the extension as the file position

//
// Find the FITS file matching columns for the required column names
//
fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_ENERGY,
                             &energyFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundEnergyFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_X,
                             &xFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundXFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_Y,
                             &yFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundYFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_Z,
                             &zFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundZFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_UNITX,
                             &unitxFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundUnitxFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_UNITY,
                             &unityFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundUnityFitsCol;
   }

fitsReturn = fits_get_colnum(groundModelFile, FALSE, FITS_COLNAME_UNITZ,
                             &unitzFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noGroundUnitzFitsCol;
   }


//
// Find out how many rows  we have in the FITS table
//
fitsReturn = fits_read_key_lng(groundModelFile, "NAXIS2", &fitsRowLimit,
                               fitsComment, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

//
// record the number of rows in the table
//
maxFitsRowNumber = fitsRowLimit;
//
// Default the current row to before the table start
//
fitsRowNumber = 0;

// File is open
groundModelOpen = true;
}

void
XrrtGroundModel::getGroundModelPhoton(double& energyInKeV,
                                      double& xInMM,
                                      double& yInMM,
                                      double& zInMM,
                                      double& unitX,
                                      double& unitY,
                                      double& unitZ)
{
//
// Read the next row in the Ground model FITS table
// If the table max row is exceeded, return zeros
//
// Standard FITSIO calling sequence
int      fitsStatus = 0;    // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsAnyNull;       // Standard return for null data present

//
// Get next FITS row
//
fitsRowNumber++;

if (fitsRowNumber > maxFitsRowNumber)
   {
   // We are at table end
   energyInKeV = 0.0e0;
   xInMM = 0.0e0;
   yInMM = 0.0e0;
   zInMM = 0.0e0;
   unitX = 0.0e0;
   unitY = 0.0e0;
   unitZ = 0.0e0;
   return;
   }

// Get row contents
//
// energy
//
fitsReturn = fits_read_col_dbl(groundModelFile, energyFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &energyInKeV,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// xInMM
//
fitsReturn = fits_read_col_dbl(groundModelFile, xFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &xInMM,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// yInMM
//
fitsReturn = fits_read_col_dbl(groundModelFile, yFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &yInMM,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// zInMM
//
fitsReturn = fits_read_col_dbl(groundModelFile, zFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &zInMM,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// unitX
//
fitsReturn = fits_read_col_dbl(groundModelFile, unitxFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &unitX,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// unitY
//
fitsReturn = fits_read_col_dbl(groundModelFile, unityFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &unitY,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
//
// unitZ
//
fitsReturn = fits_read_col_dbl(groundModelFile, unitzFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &unitZ,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
}

void
XrrtGroundModel::closeFITSFile()
{
//
// Close the file and mark internally that no file is open
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function

     groundModelOpen = false;
     fitsStatus = 0;
     fitsReturn = fits_close_file(groundModelFile, &fitsStatus);
     if (fitsStatus != NO_ERROR)
        {
        throw fitsStatus;
        }
}


long
XrrtGroundModel::getFitsModelRow() const
{
    return fitsRowNumber;
}

long
XrrtGroundModel::getMaxFitsModelRow() const
{
    return maxFitsRowNumber;
}
