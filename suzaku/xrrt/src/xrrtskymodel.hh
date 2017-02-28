// xrrtskymodel.hh
//
// Class definition for photon source (Sky model)
// Richard L Fink GSFC/631
// 1997/09/16
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/23 Upgraded documentation. R. Fink

#ifndef XRRTSKYMODEL_HH
#define XRRTSKYMODEL_HH

//
// System interfaces used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <string.h>
#include <cstring>
#include <string>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// FITSIO interfaces used
//
extern "C"
{
#include "fitsio.h"
}

//
// Local enums
//
enum XrrtSkyModelErrorCode
        {
        XrrtSkyModelErrors,
        skyFileAlreadyOpen,   // Request to open a sky model FITS file when one
                              // is already open
        noSkyModelExtension,  // Could not find the requested extension in
                              // the supplied FITS file
        noSkyEnergyFitsCol,   // Missing energy FITS column in extension
        noSkyThetaFitsCol,    // Missing theta  FITS column in FITS extension
        noSkyPhiFitsCol,      // Missing phi    FITS column in FITS extension
        XrrtSkyModelErrorsEnd
        };

//
// XrrtSkyModel supports a general mode of inputing any type astronmical
// object model into ray tracing.
//
class  XrrtSkyModel
{
    public:
          // Constructor
          XrrtSkyModel(); 
          // Copy Constructor
          XrrtSkyModel( const XrrtSkyModel& skyModel); 

          // 
          // FITS file/binary extension that will contain the Sky model
          //
          void openSkyFitsFile(string fileName, string extensionName);
          //
          // The sky model parameters from each table row; returns 0.0
          // for all values if called beyond the end of the table
          //
          void getSkyModelPhoton(double& energyInKeV,
                                 double& thetaRadian,
                                 double& phiRadian);
          //
          // Return the current row number in the table
          //
          long getFitsModelRow() const;
          //
          // Return the maximum table row number
          //
          long getMaxFitsModelRow() const;

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtSkyModelErrorCode errorCode);

    private:
          //
          // Is the FITS file open?
          bool      skyModelOpen;
          //
          // CFITSIO FITS file pointer
          fitsfile* skyModelFile;
          //
          // FITS table column numbers for the data values
          int       energyFitsColNum;
          int       thetaFitsColNum;
          int       phiFitsColNum;
          //
          // The size of the table (NAXIS2 value)
          long      maxFitsRowNumber;
          //
          // The current row in the table
          long      fitsRowNumber;
}; 

#endif
