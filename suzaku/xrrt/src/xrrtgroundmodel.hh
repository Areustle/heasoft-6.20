// xrrtgroundmodel.hh
//
// Class definition for photon source (Ground model)
// Richard L Fink GSFC/631
// 1997/09/16
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTGROUNDMODEL_HH
#define XRRTGROUNDMODEL_HH

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
// FITSIO iterfaces used
//
extern "C"
{
#include "fitsio.h"
}

//
// Local enums
//
enum XrrtGroundModelErrorCode
        {
        XrrtGroundModelErrors,
        groundFileAlreadyOpen,   // A ground FITS file is already open
        noGroundModelExtension,  // Requested FITS extension could not be found
        noGroundEnergyFitsCol,   // No FITS column named energy was found
        noGroundXFitsCol,        // No FITS column named x was found
        noGroundYFitsCol,        // No FITS column named y was found
        noGroundZFitsCol,        // No FITS column named z was found
        noGroundUnitxFitsCol,    // No FITS column named unitx was found
        noGroundUnityFitsCol,    // No FITS column named unity was found
        noGroundUnitzFitsCol,    // No FITS column named unitz was found
        XrrtGroundModelErrorsEnd
        };


//
// XrrtGroundModel provides support for an external FITS file that contains
// exact positions for each photon to be ray traced.
//
class  XrrtGroundModel
{
    public:
          // Constructor
          XrrtGroundModel(); 
          // Copy Constructor
          XrrtGroundModel( const XrrtGroundModel& groundModel); 

          //
          // Prepare the FITS file to be read
          //
          void openGroundFitsFile(string fileName, string extensionName);
          // 
          // Close the Ground FITS file
          //
          void closeFITSFile();

          // 
          // Read in the next photon from the FITS file
          //
          void getGroundModelPhoton(double& energyInKeV,
                                    double& xInMM,
                                    double& yInMM,
                                    double& zInMM,
                                    double& unitX,
                                    double& unitY,
                                    double& unitZ);
 
          //
          // Return the current FITS row number
          //
          long getFitsModelRow() const;
          //
          // Return the maximum FITS row number = NAXIS2 = row count
          long getMaxFitsModelRow() const;
          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtGroundModelErrorCode errorCode);

    private:
          //
          // Is a file currently open?
          bool      groundModelOpen;
          //
          // FITSIO pointer to the FITS file structure
          fitsfile* groundModelFile;
          //
          // FITS column number for energy data
          int       energyFitsColNum;
          //
          // FITS column number for x-axis location of photon
          int       xFitsColNum;
          //
          // FITS column number for y-axis location of photon
          int       yFitsColNum;
          //
          // FITS column number for z-axis location of photon
          int       zFitsColNum;
          //
          // FITS column number for x-axis direction of photon
          int       unitxFitsColNum;
          //
          // FITS column number for y-axis direction of photon
          int       unityFitsColNum;
          //
          // FITS column number for z-axis direction of photon
          int       unitzFitsColNum;
          //
          // Maximum row number in the FITS table
          long      maxFitsRowNumber;
          //
          // Current row number in FITS table
          long      fitsRowNumber;
}; 

#endif
