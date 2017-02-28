// xrrtreflectivity.cc
//
// Member definition for Reflectivity
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/25 Upgrade documentation. R. Fink

//#include <iostream.h>
#include "xrrtreflectivity.hh"

XrrtReflectByEnergy::XrrtReflectByEnergy():
    energyInKeV(0),
    binAngle(),
    reflectivity()
{
// A simple constructor
}

XrrtReflectByEnergy::XrrtReflectByEnergy(const XrrtReflectByEnergy& table ):
    energyInKeV(0),
    binAngle(),
    reflectivity()
{
//
// A simple copy constructor
//
    energyInKeV = table.energyInKeV;
    binAngle.reserve(table.binAngle.size());
    copy(table.binAngle.begin(), table.binAngle.end(), binAngle.begin());
    reflectivity.reserve(table.reflectivity.size());
    copy(table.reflectivity.begin(), table.reflectivity.end(),
         reflectivity.begin());
}

XrrtReflectivity::XrrtReflectivity():
    parametersSet(false),
    tableTypeSet(false),
    surfaceCompound(),
    cgsDensity(0),
    useFixedAngleBinSize(false),
    fixedStartAngleInRadians(0),
    fixedAngleBinSize(0),
    numberOfFixedBins(0),
    useVariableBinSize(false),
    maximumNumberOfBins(0),
    variableStartAngleInRadians(0),
    variableMinimumReflectivity(0),
    useFeedbackImage(false),
    feedbackFileName(" "),
    feedBackOriginalTableFile(" "),
    feedbackOriginalTable(" "),
    surfaceRoughness(0),
    optical(),
    reflectTables()
{
// A simple constructor
}

XrrtReflectivity::XrrtReflectivity(const XrrtReflectivity& reflectivity):
    parametersSet(false),
    tableTypeSet(false),
    surfaceCompound(),
    cgsDensity(0),
    useFixedAngleBinSize(false),
    fixedStartAngleInRadians(0),
    fixedAngleBinSize(0),
    numberOfFixedBins(0),
    useVariableBinSize(false),
    maximumNumberOfBins(0),
    variableStartAngleInRadians(0),
    variableMinimumReflectivity(0),
    useFeedbackImage(false),
    feedbackFileName(" "),
    feedBackOriginalTableFile(" "),
    feedbackOriginalTable(" "),
    surfaceRoughness(0),
    optical(),
    reflectTables()
{
//
// A somewhat more complex copy constructor
//
    parametersSet = reflectivity.parametersSet;
    tableTypeSet = reflectivity.tableTypeSet;
    cgsDensity = reflectivity.cgsDensity;
    surfaceRoughness = reflectivity.surfaceRoughness;
    surfaceCompound = reflectivity.surfaceCompound;
    useFixedAngleBinSize = reflectivity.useFixedAngleBinSize;
    fixedStartAngleInRadians = reflectivity.fixedStartAngleInRadians;
    fixedAngleBinSize = reflectivity.fixedAngleBinSize;
    numberOfFixedBins = reflectivity.numberOfFixedBins;
    useVariableBinSize = reflectivity.useVariableBinSize;
    maximumNumberOfBins = reflectivity.maximumNumberOfBins;
    variableStartAngleInRadians = reflectivity.variableStartAngleInRadians;
    variableMinimumReflectivity = reflectivity.variableMinimumReflectivity;
    useFeedbackImage = reflectivity.useFeedbackImage;
    feedbackFileName = reflectivity.feedbackFileName;
    feedBackOriginalTableFile = reflectivity.feedBackOriginalTableFile;
    feedbackOriginalTable = reflectivity.feedbackOriginalTable;
    optical = reflectivity.optical;
    reflectTables.reserve(reflectivity.reflectTables.size());
    copy(reflectivity.reflectTables.begin(),
         reflectivity.reflectTables.end(),
         reflectTables.begin());
}

string
XrrtReflectivity::errorMessage(XrrtReflectivityErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case noReflectivityParams:
            errorMessage = 
            "You need to set reflectivity params before calling table create";
            break;
        case duplicateExtensionName:
            errorMessage = 
            "Requested output table already exists!";
            break;
        case tableTypeRedefined:
            errorMessage = 
            "Attempt to redefine type type once set";
            break;
        case tableTypeNotSet:
            errorMessage = 
            "The table type was not set before attempting to create a table";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                         "XrrtReflectivity::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

void
XrrtReflectivity::addTable(double& energyInKeV)
{
//
// Create a new reflectivity table with the given energy and parameters
//
const double xrrtPI = 3.1415926535897932385e0;

     // 1st check thet this table has it parameters set
     if (!parametersSet)
        {
        throw noReflectivityParams;
        }
     if (!tableTypeSet)
        {
        throw tableTypeNotSet;
        }

     // Allocate a new table
     XrrtReflectByEnergy* reflectTable = new XrrtReflectByEnergy;

        
     // Store the appropriate variables
     reflectTable->setEnergyInKeV(energyInKeV);

     if (useVariableBinSize)
        {
        fixedStartAngleInRadians = variableStartAngleInRadians;
        fixedAngleBinSize =
                   (xrrtPI/2.0e0 - variableStartAngleInRadians) /
                   (maximumNumberOfBins-1);
        numberOfFixedBins = maximumNumberOfBins;
        }

     // Generate a fixed table 
     double angle;
     double variableEndAngleInRadians;
     double reflectivity;
     variableEndAngleInRadians = variableStartAngleInRadians;
     reflectTable->reserveTableBins(numberOfFixedBins);
     for (int bin=0; bin < numberOfFixedBins; bin++)
         {
         angle = fixedStartAngleInRadians + (double) bin * fixedAngleBinSize;
         reflectivity = optical.computeSingleLayerReflect(surfaceCompound, 
                                                          cgsDensity,
                                                          energyInKeV, 
                                                          surfaceRoughness,
                                                          angle);
         reflectTable->addEntry(angle, reflectivity);
         if (useVariableBinSize)
            {
            if (reflectivity > variableMinimumReflectivity)
               {
               variableEndAngleInRadians = angle + fixedAngleBinSize;
               }
            }
         }
      // Propagate the last reflectivity value for all higher angles
      angle = 2.0e0*xrrtPI;
      reflectTable->addEntry(angle, reflectivity);

     // If we needed a fixed table, we are done
     if (useFixedAngleBinSize)
        {
        // We are done
        reflectTables.push_back(reflectTable);
        return;
        }
// We must regenerate the table for a variable bin size
     fixedStartAngleInRadians = variableStartAngleInRadians;
     fixedAngleBinSize =
                   (variableEndAngleInRadians - variableStartAngleInRadians) /
                   (maximumNumberOfBins-1);
     numberOfFixedBins = maximumNumberOfBins;
     delete reflectTable;
     reflectTable = new XrrtReflectByEnergy;
// Generate a new table
     // Store the appropriate variables
     reflectTable->setEnergyInKeV(energyInKeV);

     // Generate a fixed table 
     reflectTable->reserveTableBins(numberOfFixedBins);
     for (int bin=0; bin < numberOfFixedBins; bin++)
         {
         angle = fixedStartAngleInRadians + (double) bin * fixedAngleBinSize;
         reflectivity = optical.computeSingleLayerReflect(surfaceCompound, 
                                                          cgsDensity,
                                                          energyInKeV, 
                                                          surfaceRoughness,
                                                          angle);
         reflectTable->addEntry(angle, reflectivity);
         }
      // Propagate the last reflectivity value for all higher angles
      angle = 2.0e0*xrrtPI;
      reflectTable->addEntry(angle, reflectivity);

      // Save the table
      reflectTables.push_back(reflectTable);
}


void
XrrtReflectivity::feedbackMode(string& inputFeedbackFitsFile,
                               string& inputTableFile,
                               string& inputTableName,
                               int& maximumNumberOfBins)
{
//
// This code is incomplete so don't use it!!!!!!!!!
//

// convert strings for internal use

string stringInputFeedbackFitsFile = inputFeedbackFitsFile;
char charFeedbackFITS[255];
strcpy(charFeedbackFITS, stringInputFeedbackFitsFile.c_str());

string stringInputTableFile = inputTableFile;
char charTableFile[255]; 
strcpy(charTableFile, stringInputTableFile.c_str());

char charTableName[255];
strcpy(charTableName, inputTableName.c_str());


}

void
XrrtReflectivity::writeFITSTable(string& outputFileName,
                                 string& outputExtensionName,
                                 string& caldbTelescope,
                                 string& caldbInstrument,
                                 string& caldbValidDate,
                                 string& caldbValidTime,
                                 string& caldbDescription,
                                 string& caldbEnerg,
                                 bool&   destroyExistingFITSFiles)
{
//
// Write a reflectivity table to a FITS extension
//
// Local variables
//
// Standard FITSIO calling sequence
int      fitsStatus;        // Standard error code return from FITSIO
int      fitsReturn;        // Standard return type from a FITSIO function
int      fitsHduType;       // Std FITS HDU type return
char     fitsChar[256];     // Dummy catch for FITS return string
char     fitsComment[256];  // Dummy catch for FITS Comment return

//
// Determine whether the file name is "clean"; i.e. lack [#]
//     strip it if not, and convert to char
string localFileName = outputFileName;
size_t cleanLength = localFileName.find_last_not_of('[');
cleanLength++;
localFileName.resize(cleanLength);


//
// Open file with new capability to destroy existing files
//
fitsfile* fitsOutputFile;    // FITSIO FILE* pointer
fitsStatus = 0;

string openFITSName;
if (destroyExistingFITSFiles == true)
   {
   // Magic value to force CFITSIO to destroy existing file
   openFITSName = "!" + localFileName;
   }
else
   {
   openFITSName = localFileName;
   }
fitsReturn = fits_create_file(&fitsOutputFile,
                             (char*) openFITSName.c_str(), &fitsStatus);
//
// The file may not already exist; if so create it
//
if (fitsStatus == NO_ERROR)
   {
   // Create a dummy header
   int bitpix = LONG_IMG;
   int naxis = 0;
   long* naxes = 0;
   fitsReturn = fits_write_imghdr(fitsOutputFile, bitpix, naxis, naxes,
                                  &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   }
else if (fitsStatus == FILE_NOT_CREATED)
   {
   //
   // The file already exists
   //
   fitsStatus = 0;
   fitsReturn = fits_open_file(&fitsOutputFile,
                              (char*) openFITSName.c_str(), 
                              READWRITE, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   }

//
// See if this header is present already; after all, we don't want headers
// with the same name

// Find the requested extension in the FITS file
// Scan all possible extentions looking for the requested extension
// 10000 is an arbitary large number
// 2 is the 1st extension of a FITS file
for (int i=2; i<10000; i++)
    {
    fitsReturn = fits_movabs_hdu(fitsOutputFile, i, &fitsHduType, 
                                 &fitsStatus);
    if (fitsStatus == END_OF_FILE)
       {
       // Good; we didn't find an extension with the same name
       fitsStatus = 0;
       break;
       }
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    fitsReturn = fits_read_key_str(fitsOutputFile, "EXTNAME", 
                                   fitsChar, fitsComment, &fitsStatus);
    if ((strcasecmp(fitsChar, outputExtensionName.c_str())) == 0)
       {
       // requested extension found
       throw duplicateExtensionName;
       }
    }

//
// construct the binary extension
//
// How many elements to write?
// Add up the table sizes per energy
//
int naxis2 = 0;
for (vector<XrrtReflectByEnergy*>::iterator table = reflectTables.begin();
     table < reflectTables.end();
     table++)
    {
    naxis2 = naxis2 + (*table)->getTableSize();
    }
//
// How many fields?
//
int tfields = 3;
//
// What names do they have?
//
char* ttype[3];
string energy = "energy";
ttype[0] = (char*) energy.c_str();
string bangle = "bangle";
ttype[1] = (char*) bangle.c_str();
string refprob = "refprob";
ttype[2] = (char*) refprob.c_str();
//
// What format do they have?
//
char* tform[3];
string oneD = "1D";
tform[0] = (char*) oneD.c_str();
tform[1] = (char*) oneD.c_str();
tform[2] = (char*) oneD.c_str();
//
// What  units?
//
char* tunit[3];
string kev = "KeV";
tunit[0] = (char*) kev.c_str();
string radians = "radians";
tunit[1] = (char*) radians.c_str();
string none = "none";
tunit[2] = (char*) none.c_str();
//
// Create the table header
//
fitsReturn = fits_create_tbl(fitsOutputFile, BINARY_TBL, naxis2, tfields,
                             ttype, tform, tunit, 
                             (char*) outputExtensionName.c_str(),
                             &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
// Replace the comment lines on the TTYPEn lines
fitsReturn = fits_modify_comment(fitsOutputFile, "TTYPE1",
                                 "Energy for reflection probability",
                                 &fitsStatus);
fitsReturn = fits_modify_comment(fitsOutputFile, "TTYPE2",
                                 "Reflection angle for reflection probability",
                                 &fitsStatus);
fitsReturn = fits_modify_comment(fitsOutputFile, "TTYPE3",
                                 "Reflection probability",
                                 &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

//
// Write header cards in order by:
// 1) CALDB headers
// 2) Headers for the whole table
// 3) headers for individual sub-tables

//
// Write CALDB headers
//
fitsReturn = fits_write_key_str(fitsOutputFile, "TELESCOP",
                                (char*) caldbTelescope.c_str(),
                                "mission name", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "INSTRUME", 
                                (char*) caldbInstrument.c_str(),
                                "instrument name", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CCLS0001", "BCF",
                                "Basic Calibration File", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CCNM0001", "REFLECTIVITY",
                               "OGIP Class - Mirror reflectivity", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CDTP0001", "DATA",
                                "Real data, not subroutine", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CVSD0001", 
                                (char*) caldbValidDate.c_str(),
                                "Validity Start date", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CVST0001", 
                                (char*) caldbValidTime.c_str(),
                                "Validity Start time", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CDES0001", 
                                (char*) caldbDescription.c_str(),
                                "Description", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_str(fitsOutputFile, "CBD10001", 
                                (char*) caldbEnerg.c_str(),
                                "Energy Bounds", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

//
// Write whole table headers
//
fitsReturn = fits_write_date(fitsOutputFile, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
strcpy(fitsComment, "Reflectivity table major history");
fitsReturn = fits_write_history(fitsOutputFile, fitsComment, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
strcpy(fitsChar, surfaceCompound.getSymbolFormula().c_str());
fitsReturn = fits_write_key_str(fitsOutputFile, "FORMULA", fitsChar, 
                                "surface formula", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_dbl(fitsOutputFile, "DENSITY", cgsDensity,
                                10, "surface cgs density", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
fitsReturn = fits_write_key_dbl(fitsOutputFile, "ROUGH", surfaceRoughness,
                                10, "surface roughness param", &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }
if (useFixedAngleBinSize)
   {
   char keyword[9];
   // Fixed bin size sub table
   strcpy(keyword, "TABLE");
   fitsReturn = fits_write_key_str(fitsOutputFile, keyword, "F",
                                   "Fixed bin size table", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword, "SANGLE");
   double startAngle = fixedStartAngleInRadians;
   fitsReturn = fits_write_key_dbl(fitsOutputFile, keyword, startAngle,
                            10, "Start angle in radians", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword,"FIXBIN");
   double binSize = fixedAngleBinSize;
   fitsReturn = fits_write_key_dbl(fitsOutputFile, keyword, binSize,
                            10, "Bin size in radians", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   int noOfBins = numberOfFixedBins;
   strcpy(keyword, "NOBINS");
   fitsReturn = fits_write_key_lng(fitsOutputFile, keyword, (long) noOfBins,
                                   "number of fixed bins", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   }
if (useVariableBinSize)
   {
   char keyword[9];
   // Variable bin size sub table
   strcpy(keyword, "TABLE");
   fitsReturn = fits_write_key_str(fitsOutputFile, keyword, "V",
                                   "Variable bin size table", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword, "SANGLE");
   double startAngle = variableStartAngleInRadians;
   fitsReturn = fits_write_key_dbl(fitsOutputFile, keyword, startAngle,
                            10, "Start angle in radians", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword, "MINREF");
   double minReflectivity = variableMinimumReflectivity;
   fitsReturn = fits_write_key_dbl(fitsOutputFile, keyword, minReflectivity,
                            10, "Minimum reflectivity", &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   int noOfBins = maximumNumberOfBins;
   strcpy(keyword, "NOBINS");
   fitsReturn = fits_write_key_lng(fitsOutputFile, keyword, (long) noOfBins,
                                   "max number of variable bins", 
                                   &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   }
if (useFeedbackImage)
   {
   char keyword[9];
   strcpy(fitsComment,"The bins in this table were stretched to magnify");
   fitsReturn = fits_write_history(fitsOutputFile, fitsComment, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(fitsComment,"the accuracy in high angle use regions using");
   fitsReturn = fits_write_history(fitsOutputFile, fitsComment, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(fitsComment,"feedback info from the following files:");
   fitsReturn = fits_write_history(fitsOutputFile, fitsComment, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }

   // Note in file we are using long string features
   fitsReturn = ffplsw(fitsOutputFile, &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   string feedbackFileName = feedbackFileName;
   string originalTableFile = feedBackOriginalTableFile;
   string originalTableName  = feedbackOriginalTable;
   strcpy(keyword, "FFNAME");
   // const char * cast away since the function does not modify it
   fitsReturn = fits_write_key_longstr(fitsOutputFile, keyword, 
                                       (char*) feedbackFileName.c_str(),
                                       "FeedBack File Name", 
                                       &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword, "OTFNAME");
   // const char * cast away since the function does not modify it
   fitsReturn = fits_write_key_longstr(fitsOutputFile, keyword, 
                                       (char*) originalTableFile.c_str(),
                                       "Original Table File Name", 
                                       &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   strcpy(keyword, "OTNAME");
   // const char * cast away since the function does not modify it
   fitsReturn = fits_write_key_longstr(fitsOutputFile, keyword, 
                                       (char*) originalTableName.c_str(),
                                       "Original Table Name", 
                                       &fitsStatus);
   if (fitsStatus != NO_ERROR)
      {
      throw fitsStatus;
      }
   
   }

//
// Write history by sub-table
// i.e. for each energy in the table
//
int index = 0;
for (vector<XrrtReflectByEnergy*>::iterator table = reflectTables.begin();
     table < reflectTables.end();
     table++)
    {
    index++;
    double energy = (*table)->getEnergyInKeV();
    char keyword[9];
    char number[10];
    strcpy(keyword, "KEV");
    sprintf(number,"%d",index);
    strcat(keyword, number);
    fitsReturn = fits_write_key_dbl(fitsOutputFile, keyword, energy,
                                10, "sub-table energy in KeV", &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    strcpy(keyword, "BIN");
    strcat(keyword, number);
    long localBins  = (*table)->getTableSize();
    fitsReturn = fits_write_key_lng(fitsOutputFile, keyword, localBins,
                                    "sub-table bin count", &fitsStatus);
    if (fitsStatus != NO_ERROR)
       {
       throw fitsStatus;
       }
    }



//
// write the table as rows in the FITS file
//
double fitsArray[1]; // FITSIO always is writing arrays 
long row = 0;
for (vector<XrrtReflectByEnergy*>::iterator table = reflectTables.begin();
     table < reflectTables.end();
     table++)
    {
    double energy = (*table)->getEnergyInKeV();
    for (int i = 0; i < (int) (*table)->getTableSize(); i++)
        {
        double bAngle = (*table)->getBinAngle(i);
        double refprob = (*table)->getReflectivity(i);
        row++;
        // Write energy
        fitsArray[0] = energy;
        fitsReturn = fits_write_col_dbl(fitsOutputFile, 1, row, 1L, 1L,
                                        fitsArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
    
        // Write angle
        fitsArray[0] = bAngle;
        fitsReturn = fits_write_col_dbl(fitsOutputFile, 2, row, 1L, 1L,
                                        fitsArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
    
        // Write reflection probability
        fitsArray[0] = refprob;
        fitsReturn = fits_write_col_dbl(fitsOutputFile, 3, row, 1L, 1L,
                                        fitsArray, &fitsStatus);
        if (fitsStatus != NO_ERROR)
           {
           throw fitsStatus;
           }
        }
    }
// Close the table
fitsReturn = fits_close_file(fitsOutputFile, &fitsStatus);
if (fitsStatus != NO_ERROR)
   {
   throw fitsStatus;
   }

}


double
XrrtReflectByEnergy::getEnergyInKeV() const
{
    return energyInKeV;
}

void
XrrtReflectByEnergy::setEnergyInKeV(const double& energy)
{
    energyInKeV = energy;
}


void 
XrrtReflectByEnergy::addEntry(const double& angle, const double& reflectCoeff)
{
     binAngle.push_back(angle);
     reflectivity.push_back(reflectCoeff);
}

void 
XrrtReflectByEnergy::reserveTableBins(const int& numberOfBins)
{
    binAngle.reserve(numberOfBins);
    reflectivity.reserve(numberOfBins);
}

double
XrrtReflectByEnergy::getBinAngle(const int& index)
{
     return binAngle[index];
}

double 
XrrtReflectByEnergy::getReflectivity(const int& index)
{
     return reflectivity[index];
}

int
XrrtReflectByEnergy::getTableSize() const
{
    return binAngle.size();
}

bool 
XrrtReflectivity::getParametersSet() const
{
     return parametersSet;
}

XrrtMolecule 
XrrtReflectivity::getSurfaceCompound() const
{
     return surfaceCompound;
}

double 
XrrtReflectivity::getCGSDensity() const
{
     return cgsDensity;
}

double 
XrrtReflectivity::getSurfaceRoughness() const
{
     return surfaceRoughness;
}

void 
XrrtReflectivity::setTableParameters(const XrrtMolecule& compound,
                                     const double& density,
                                     const double& roughness)
{
    surfaceCompound = compound;
    cgsDensity = density;
    surfaceRoughness = roughness;
    parametersSet = true;
}

void 
XrrtReflectivity::setFixedTable(double& startAngleInRadians,
                                double& angleBinSize, 
                                int& numberOfBins)
{
     if (tableTypeSet)
        {
        throw tableTypeRedefined;
        }
     useFixedAngleBinSize = true;
     fixedStartAngleInRadians = startAngleInRadians;
     fixedAngleBinSize = angleBinSize;
     numberOfFixedBins = numberOfBins;
     tableTypeSet = true;
}

void 
XrrtReflectivity::setVariableTable(int& maxNumberOfBins,
                                   double& varStartAngleInRadians,
                                   double& varMinReflectivity)
{
     if (tableTypeSet)
        {
        throw tableTypeRedefined;
        }
     useVariableBinSize = true;
     maximumNumberOfBins = maxNumberOfBins;
     variableStartAngleInRadians = varStartAngleInRadians;
     variableMinimumReflectivity = varMinReflectivity;
     tableTypeSet = true;
}

 bool
XrrtReflectivity::getUseFixedAngleBinSize() const
{
    return useFixedAngleBinSize;
}

void 
XrrtReflectivity::setUseFixedAngleBinSize(const bool& trueFalse)
{
    useFixedAngleBinSize = trueFalse;
}

double
XrrtReflectivity::getFixedStartAngleInRadians() const
{
    return fixedStartAngleInRadians;
}

void
XrrtReflectivity::setFixedStartAngleInRadians(const double& angle)
{
    fixedStartAngleInRadians = angle;
}

double
XrrtReflectivity::getFixedAngleBinSize() const
{
     return fixedAngleBinSize;
}

void 
XrrtReflectivity::setFixedAngleBinSize(const double& binSizeInRadians)
{
    fixedAngleBinSize = binSizeInRadians;
}

int 
XrrtReflectivity::getNumberOfFixedBins() const
{
     return numberOfFixedBins;
}

 void 
XrrtReflectivity::setNumberOfFixedBins(const int& bins)
{
     numberOfFixedBins = bins;
}

bool   
XrrtReflectivity::getUseVariableBinSize() const
{
    return useVariableBinSize;
}

void   
XrrtReflectivity::setUseVariableBinSize(const bool& trueFalse)
{
    useVariableBinSize = trueFalse;
}

int 
XrrtReflectivity::getMaximumNumberOfBins() const
{
    return maximumNumberOfBins;
}

void   
XrrtReflectivity::setMaximumNumberOfBins(const int& bins)
{
    maximumNumberOfBins = bins;
}

double 
XrrtReflectivity::getVariableStartAngleInRadians() const
{
    return variableStartAngleInRadians;
}

void   
XrrtReflectivity::setVariableStartAngleInRadians(const double& angleInRadians)
{
    variableStartAngleInRadians = angleInRadians;
}

double 
XrrtReflectivity::getVariableMinimumReflectivity() const
{
    return variableMinimumReflectivity;
}

void   
XrrtReflectivity::setVariableMinimumReflectivity(const double& reflectivity)
{
     variableMinimumReflectivity = reflectivity;
}


bool   
XrrtReflectivity::getUseFeedbackImage() const
{
    return useFeedbackImage;
}

void   
XrrtReflectivity::setUseFeedbackImage(const bool& trueFalse)
{
     useFeedbackImage = trueFalse;
}

string 
XrrtReflectivity::getFeedbackFileName() const
{
    return feedbackFileName;
}

void   
XrrtReflectivity::setFeedbackFileName(const string& fileName)
{
     feedbackFileName = fileName;
}

string 
XrrtReflectivity::getFeedBackOriginalTableFile() const
{
    return feedBackOriginalTableFile;
}

void   
XrrtReflectivity::setFeedBackOriginalTableFile(const string& fileName)
{
     feedBackOriginalTableFile = fileName;
}

string 
XrrtReflectivity::getFeedbackOriginalTable() const
{
    return feedbackOriginalTable;
}

void   
XrrtReflectivity::setFeedbackOriginalTable(const string& tableName)
{
     feedbackOriginalTable = tableName;
}

