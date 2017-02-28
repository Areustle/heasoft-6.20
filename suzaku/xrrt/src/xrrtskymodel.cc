// xrrtskymodel.cc
//
// Member definition for photon source (Sky model)
// Richard L Fink GSFC/631
// 1997/09/16
// 1997/09/25 Upgrade documentation. R. Fink


#include "xrrtskymodel.hh"

XrrtSkyModel::XrrtSkyModel():
    skyModelOpen(false),
    skyModelFile(0),
    energyFitsColNum(0),
    thetaFitsColNum(0),
    phiFitsColNum(0),
    maxFitsRowNumber(0),
    fitsRowNumber(0)
{
// A simple constructor
}

string
XrrtSkyModel::errorMessage(XrrtSkyModelErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case skyFileAlreadyOpen:
            errorMessage = 
            "Attempt to open a second Sky Model FITS file";
            break;
        case noSkyModelExtension:
            errorMessage = 
            "The requested Sky Model extension could not be found";
            break;
        case noSkyEnergyFitsCol:
            errorMessage = 
            "The Sky Model FITS file has no energy column";
            break;
        case noSkyThetaFitsCol:
            errorMessage = 
            "The Sky Model FITS file has no theta column";
            break;
        case noSkyPhiFitsCol:
            errorMessage = 
            "The Sky Model FITS file has no phi column";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                              "XrrtSkyModel::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

void
XrrtSkyModel::openSkyFitsFile(string fileName, string extensionName)
{
//
// Open a SKY MODEL FITS file and save its parameters.
//
char * const FITS_EXTENSION_NAME = "EXTNAME";
char * const FITS_COLNAME_ENERGY = "energy";
char * const FITS_COLNAME_THETA  = "theta";
char * const FITS_COLNAME_PHI    = "phi";

// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return
char     fitsChar[256];     // Dummy catch for FITS return string
char     fitsComment[256];  // Dummy catch for FITS Comment return
long     fitsRowLimit;


// 1st check that a Sky file is not already open
if (skyModelOpen)
   {
   throw skyFileAlreadyOpen;
   }

// Clean up the fileName string
size_t cleanLength = fileName.find_last_not_of('[');
cleanLength++;
fileName.resize(cleanLength);


// Open the sky file
fitsStatus = 0;
fitsReturn = fits_open_file( &skyModelFile, (char*) fileName.c_str(),
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
    fitsReturn = fits_movabs_hdu(skyModelFile, i, &fitsHduType, &fitsStatus);
    if (fitsStatus == END_OF_FILE)
       {
       throw noSkyModelExtension;
       }
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    fitsReturn = fits_read_key_str(skyModelFile, FITS_EXTENSION_NAME, 
                                   fitsChar, fitsComment, &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    if ((strcasecmp(fitsChar, extensionName.c_str())) == 0)
       {
       // extension found
       break;
       }
    }
//
// We now have the extension as the file position
//
// Find the FITS file matching columns for the required column names
//
fitsReturn = fits_get_colnum(skyModelFile, FALSE, FITS_COLNAME_ENERGY,
                             &energyFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noSkyEnergyFitsCol;
   }

fitsReturn = fits_get_colnum(skyModelFile, FALSE, FITS_COLNAME_THETA,
                             &thetaFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noSkyThetaFitsCol;
   }

fitsReturn = fits_get_colnum(skyModelFile, FALSE, FITS_COLNAME_PHI,
                             &phiFitsColNum, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw noSkyPhiFitsCol;
   }

//
// Find out how many rows  we will have
//
fitsReturn = fits_read_key_lng(skyModelFile, "NAXIS2", &fitsRowLimit,
                               fitsComment, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

// record the number of rows
maxFitsRowNumber = fitsRowLimit;
fitsRowNumber = 0;

// File is open
skyModelOpen = true;
}

void
XrrtSkyModel::getSkyModelPhoton(double& energyInKeV,
                                double& thetaRadians,
                                double& phiRadians)
{
// 
// Read a Sky model photon from the FITS file row
// Return all zeros if we go beyond the table limits
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsAnyNull;       // Standard return for null data present

// get next FITS row
fitsRowNumber++;

if (fitsRowNumber > maxFitsRowNumber)
   {
   // We are at table end
   energyInKeV = 0.0e0;
   thetaRadians = 0.0e0;
   phiRadians = 0.0e0;
   return;
   }

// get row contents
// energy
fitsStatus = 0;
fitsReturn = fits_read_col_dbl(skyModelFile, energyFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &energyInKeV,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
// theta
fitsReturn = fits_read_col_dbl(skyModelFile, thetaFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &thetaRadians,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
// phi
fitsReturn = fits_read_col_dbl(skyModelFile, phiFitsColNum,
                               fitsRowNumber, 1L, 1L, 0, &phiRadians,
                               &fitsAnyNull, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
   
}



long
XrrtSkyModel::getFitsModelRow() const
{
    return fitsRowNumber;
}

long
XrrtSkyModel::getMaxFitsModelRow() const
{
    return maxFitsRowNumber;
}
