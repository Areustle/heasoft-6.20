// xrrtreflectivity.hh
//
// Class definition for Building Reflectivity Tables
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997.09/22 Upgraded documentation. R. Fink

#ifndef XRRTREFLECTIVITY_HH
#define XRRTREFLECTIVITY_HH

//
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <string.h>
#include <cstring>
// These included to fix gcc template bug in complex var via xrrtoptical.hh
#include <complex>
// Modified by H. Mori (2005/09/14)
// #include <std/dcomplex.h>
//
// #include <stl.h>

// 
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtmolecule.hh"
#include "xrrtoptical.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

// 
// CFITSIO interface
//
extern "C" {
#include "fitsio.h"
}

//
// Local enums
//
enum  XrrtReflectivityErrorCode
        {
        XrrtReflectivityErrors,
        noReflectivityParams,   // Requested reflectivity to be calculated
                                // when needed params not set
        duplicateExtensionName, // Specified writing a new relectivity table
                                // and the name already exists in the output
                                // FITS file
        tableTypeRedefined,     // Once a table type is specified, it can not
                                // be redefined part way thru creation
        tableTypeNotSet,        // You must set the table type before creation
        XrrtReflectivityErrorsEnd
        };

//
// XrrtReflectByEnergy are reflection table rows with a specific energy
//
class  XrrtReflectByEnergy
{
    public:
          // Constructor
          XrrtReflectByEnergy(); 
          // Copy Constructor
          XrrtReflectByEnergy( const XrrtReflectByEnergy& table ); 
          
          //
          // Control the size of the table
          //
          void reserveTableBins(const int& numberOfBins);
          int getTableSize() const;

          //
          // Add a bin to the table
          void addEntry(const double& angle, const double& reflectCoeff);
          
          //
          // Accessor functions
          //
          double getEnergyInKeV() const;
          void   setEnergyInKeV(const double& energy);

          //
          double getBinAngle(const int& index);

          //
          double getReflectivity(const int& index);

    private:
           // Energy that the table is calculated for
           double energyInKeV;

           //
           // Table contents
           //
           // Surface incident angle in radians of the beginning of the bin
           vector<double> binAngle;
           // Reflection probability 0-1 for this angle
           vector<double> reflectivity;
};

//
// XrrtReflectivity combines the multiple energy rows to form the whole table
// and stores the parameters that are global to the table.
//
class XrrtReflectivity
{
    public:
          // Constructor
          XrrtReflectivity();
          // Copy Constructor
          XrrtReflectivity( const XrrtReflectivity& reflectivity);

          //
          // Set the global table parameters
          //
          void setTableParameters(const XrrtMolecule& compound,
                                  const double& cgsDensity,
                                  const double& roughness);

          //
          // Have the table global params been set?
          //
          bool getParametersSet() const;

          //
          // Accessors for the global table parameters
          //
          XrrtMolecule getSurfaceCompound() const;
          //
          double getCGSDensity() const;
          //
          double getSurfaceRoughness() const;

          //
          // Set the type of table to use
          //
          void setFixedTable(double& fixedStartAngleInRadians,
                             double& fixedAngleBinSize, 
                             int& numberOfFixedBins);
          void setVariableTable(int& maximumNumberOfBins,
                                double& variableStartAngleInRadians,
                                double& variableMinimumReflectivity);
          // Creates a table that matchs the input table but uses the
          // access count by bin angle to stretch the table bins in areas
          // with high usage and compress the bins when angles are little used
          // NOTE: the output table will ALWAYS be a Variable table
          void feedbackMode(string& inputfeedbackFitsFile,
                            string& inputTableFile,
                            string& inputTableName,
                            int& maximumNumberOfBins);
         

          //
          // Accessors for type of table info
          //
          // Fixed table accessors
          //
          bool   getUseFixedAngleBinSize() const;
          //
          double getFixedStartAngleInRadians() const;
          //
          double getFixedAngleBinSize() const;
          //
          int getNumberOfFixedBins() const;
          //
          // Variable table accessors
          //
          bool   getUseVariableBinSize() const;
          //
          int getMaximumNumberOfBins() const;
          //
          double getVariableStartAngleInRadians() const;
          //
          double getVariableMinimumReflectivity() const;
          //
          // Feedback table accessors
          //
          bool   getUseFeedbackImage() const;
          //
          string getFeedbackFileName() const;
          //
          string getFeedBackOriginalTableFile() const;
          //
          string getFeedbackOriginalTable() const;

          //
          // Adds a table with the given energy
          //
          void addTable(double& energyInKeV);

          //
          // Write the current table to a FITS file
          //
          void writeFITSTable(string& outputFileName,
                              string& outputExtensionName,
                              string& caldbTelescope,
                              string& caldbInstrument,
                              string& caldbValidDate,
                              string& caldbValidTime,
                              string& caldbDescription,
                              string& caldbEnerg,
                              bool&   destroyExistingFile);

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtReflectivityErrorCode errorCode);

    private:
          //
          // All the accessor that set status are private
          //
          void   setUseFixedAngleBinSize(const bool& trueFalse);
          void   setFixedStartAngleInRadians(const double& angle);
          void   setFixedAngleBinSize(const double& binSizeInRadians);
          void   setNumberOfFixedBins(const int& bins);

          void   setUseVariableBinSize(const bool& trueFalse);
          void   setMaximumNumberOfBins(const int& bins);
          void   setVariableStartAngleInRadians(const double& angle);
          void   setVariableMinimumReflectivity(const double& reflectivity);

          void   setUseFeedbackImage(const bool& trueFalse);
          void   setFeedbackFileName(const string& fileName);
          void   setFeedBackOriginalTableFile(const string& fileName);
          void   setFeedbackOriginalTable(const string& tableName);

           //
           // Variables that are parameters of the table
           bool         parametersSet;
           bool         tableTypeSet;
           XrrtMolecule surfaceCompound;
           double       cgsDensity;

           //
           // variables used when fixed bin sizes are selected
           bool   useFixedAngleBinSize;
           double fixedStartAngleInRadians;
           double fixedAngleBinSize;
           int numberOfFixedBins;

           //
           // Variables used when variable bin sizes are used
           bool   useVariableBinSize;
           int maximumNumberOfBins;
           double variableStartAngleInRadians;
           double variableMinimumReflectivity;

           //
           // Variables used in feedback mode
           bool   useFeedbackImage;
           string feedbackFileName;
           string feedBackOriginalTableFile;
           string feedbackOriginalTable;

           // This is placed here because the roughness calculation accounts
           // for the energy variation.
           double       surfaceRoughness;
           XrrtOpticalConstants optical;

           //
           // The (by energy) rows that make up the whole table
           vector<XrrtReflectByEnergy*> reflectTables;
};

#endif
