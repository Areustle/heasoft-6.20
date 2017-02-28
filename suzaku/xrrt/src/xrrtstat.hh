// xrrtstat.hh
//
// Class definition for XRRT statistics kept on ray tracing photon results
// Richard L Fink GSFC/631
// 1997/08/11
// 1997/09/18 Moved error codes from xrrt_types.hh to here and upgraded
//            documentation. R. Fink
// 1997/09/23 More documentation work. R. Fink
// 1998/10/08 Fix bug in collectStatistics routine that allowed ALL photons 
//            into the output image rather than just those that reached the
//            focal plane.
//            Reported by Yoshihiro UEDA ueda@astro.isas.ac.jp of ISAS
//


#ifndef XRRTSTAT_HH
#define XRRTSTAT_HH

//
// XrrtStat system interface used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <math.h>
#include <cmath>
#include <numeric>
#include <iterator>


//
// XrrtStat XRRT interface use
//
#include "xrrt_types.hh"
#include "xrrtphoton.hh"
#include "xrrtpolygon.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// XrrtStat Ftool/FITSIO interface use
//
extern "C" {
#include "fitsio.h"
}

//
// Local enums
//
enum XrrtStatErrorCode
        {
        XrrtStatErrors,
        fitsFileNotOpen,       // Request to write keywords to non-open file
        imageFileMayNotExist,  // When writing an image to a primary array,
                               // the ENTIRE FITS file may not already exist.
        unknownPhotonStatus,   // A photon status value from ray tracing
                               // was not programmed for
        unknownPhotonReflectionClass,  // A photon reflection class from ray
                               // tracing was not programmed for
        psfCoreIsZero,         // there are no photons in the PSF core
        XrrtStatErrorsEnd
        };
//
// XrrtStat collects all ray tracing statistics together. Collection
// and writing to FITS.
//
class  XrrtStat
{
    public:
          // Constructor
          XrrtStat(); 
          // Copy Constructor
          XrrtStat( const XrrtStat& stat ); 

          //
          // Error message converter
          //
          string errorMessage(XrrtStatErrorCode errorCode) const;

          //
          // Main call of photon statistics collection
          //
          void collectStatistics(const XrrtPhoton& photon);

          //
          // Cause all existing FITS files to be destroyed on open
          //
          void setONDestroyExistingFITSFiles();
          
          //
          // Focal plane scaling function
          //
          void setFocalPlaneScale(const double& sourceOffsetAngleRadian,
                                  const double& sourceRotationAngleRadian,
                                  const double& nominalFocalLengthMM);

          //
          // Specify the small mask/important area for focal plane
          //
          void setFocalPlaneMask(XrrtPolygon& mask);

          //
          // Functions to turn ON/OFF data collection
          //
          void setResultCollectionON();
          void setResultCollectionOFF();
          bool getResultCollectionStatus() const;
          //
          void setResultStatFITSOutputON();
          void setResultStatFITSOutputOFF();
          void suppressByEnergyResultStatOutput();
          bool getResultStatFITSOutputStatus() const;
          //
          void setPhotonHistoryON();
          void setPhotonHistoryOFF();
          bool getPhotonHistoryStatus() const;
          //
          void setImageCollectionON();
          void setImageCollectionOFF();
          bool getImageCollectionStatus() const;
          void setImageSaveByEnergyON();
          void setImageSaveByEnergyOFF();
          bool getImageSaveByEnergyStatus() const;

          //
          // Functions to set parameter data
          //
          void setResultStatFileData(const string& fileName, 
                                     const string& tableName);
          void setPhotonHistoryFileData(const string& fileName, 
                                        const string& tableName);
          void setImageFileData(const string& fileName, 
                                const string& tableName);
          void setPsfCoreRadiusLimitRadian(const double& angle);
          void setAreaInMMSqr(const double& area);
          void setRADEC(const double& ra, const double& dec);

          //
          // Functions to Start/Stop statistics
          //     start* opens files and prepares collections
          //     stop*  writes data/closes files/zeros collection totals
          //
          void startResultCollection();
          void stopResultCollection();
          void startPhotonHistory();
          void stopPhotonHistory();
          void startImageCollection();
          void stopImageCollection();

          //
          // Access to the FITS file pointers
          //
          fitsfile* getPhotonHistoryFilePtr();
          fitsfile* getResultStatFilePtr();
          fitsfile* getImageFilePtr();

          //
          // Image parameter functions
          //
          void setImageSize(const int& xyBins,
                            const double&       xyMinMM,
                            const double&       xyMaxMM);
          //
          // Functions to recover statistics data
          //
          // Return the current number of photons that have fallen inside
          // the radius specified by setPsfCoreRadiusLimitRadian()
          int getCoreTraceCount() const;
          //
          // Return the geometrical area in mm^2
          double getGeometricalAreaMM2();
          //
          // return the cumulative photon count within a given radius
          long int getCumPhotonCntByRadius(double& arcminRadius);

          // 
          // Return the size in mm/arcmin of a PSF radial bin
          double getPsfRadialBinSizeMM();
          double getPsfRadialBinSizeArcmin();

          //
          // Compute the encircled energy function point spread function
          // and return it
          void calculatePSF(vector<double>& psfArray);

          //
          // Functions to write keywords to FITS files
          //
          void fitsFileHistoryWrite(fitsfile* fitsFile, string& comment);
          void fitsFileCommentWrite(fitsfile* fitsFile, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    unsigned int& value, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    int& value, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    float& value, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    double& value, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    bool& value, string& comment);
          void fitsFileKeywordWrite(fitsfile* fitsFile, string& keyword, 
                                    string& value, string& comment);

          void resultStatHistoryWrite(string& comment);
          void resultStatCommentWrite(string& comment);
          void resultStatKeywordWrite(string& keyword, unsigned int& value, 
                                      string& comment);
          void resultStatKeywordWrite(string& keyword, int& value, 
                                      string& comment);
          void resultStatKeywordWrite(string& keyword, float& value, 
                                      string& comment);
          void resultStatKeywordWrite(string& keyword, double& value, 
                                      string& comment);
          void resultStatKeywordWrite(string& keyword, bool& value, 
                                      string& comment);
          void resultStatKeywordWrite(string& keyword, string& value, 
                                      string& comment);

          void photonHistoryHistoryWrite(string& comment);
          void photonHistoryCommentWrite(string& comment);
          void photonHistoryKeywordWrite(string& keyword, unsigned int& value, 
                                         string& comment);
          void photonHistoryKeywordWrite(string& keyword, int& value, 
                                         string& comment);
          void photonHistoryKeywordWrite(string& keyword, float& value, 
                                         string& comment);
          void photonHistoryKeywordWrite(string& keyword, double& value, 
                                         string& comment);
          void photonHistoryKeywordWrite(string& keyword, bool& value, 
                                         string& comment);
          void photonHistoryKeywordWrite(string& keyword, string& value, 
                                         string& comment);

          void imageHistoryWrite(string& comment);
          void imageCommentWrite(string& comment);
          void imageKeywordWrite(string& keyword, unsigned int& value, 
                                 string& comment);
          void imageKeywordWrite(string& keyword, int& value, 
                                 string& comment);
          void imageKeywordWrite(string& keyword, float& value, 
                                 string& comment);
          void imageKeywordWrite(string& keyword, double& value, 
                                 string& comment);
          void imageKeywordWrite(string& keyword, bool& value, 
                                 string& comment);
          void imageKeywordWrite(string& keyword, string& value, 
                                 string& comment);


    private:
           //
           // private member functions for XrrtStat
           //
           // Dump all XrrtStat metadata to a FITS header
           void writeHistoricalKeywords(fitsfile* fitsFile);

           //
           void collectResultStatistics(const XrrtPhoton& photon);
           void writeResultStatEnergyRow();
           void writeResultStatTotalRow();
           void addByEnergyToTotal();
           void zeroByEnergy();
           void smoothPSF(double* indexArray, double* smoothArray, 
                          int numberOfArrayElements, double indexArrayPosition,
                          double* smoothedReturnValue, int smoothingMode);

           //
           // The version of XrrtStat and for now the version of the XRRT
           // release.
           string XrrtStatVersion;

           //
           // How to handle pre-existing files
           bool destroyExistingFITSFiles;

           //
           // Variables to support Result Statistics Collection
           //
           bool collectResultStat;
           bool outputResultStat;
           bool suppressEnergyResultRows;
           string resultStatFileName;
           string resultStatTableName;
           fitsfile* resultStatFile;
           long resultStatRowsWritten;

           //
           // Variables to store metainformation necessary to tracking of
           // photon positions on focal plane
           //
           double sourceFocalPlaneXCenterMM;
           double sourceFocalPlaneYCenterMM;
           double designFocalLengthMM;
           double focalPlaneMM2Arcmin;
           double psfCoreRadiusLimitRadian;
           double areaInMMSqr;
           XrrtPolygon focalPlaneMask;
           int sourceRadialProfileBins;
           int psfRadialProfileBins;
           double sourceRadialBinsPerArcmin;
           double psfRadialProfileBinsPerMM;

           //
           // Variables to support Photon History Collection
           //
           void outputPhotonData(const XrrtPhoton& photon);
           bool outputPhotonHistory;
           string photonHistoryFileName;
           string photonHistoryTableName;
           fitsfile* photonHistoryFile;
           long photonHistoryRowsWritten;

           //
           // Variables to support Image Collection
           //
           void addPhotonToImage(const XrrtPhoton& photon);
           bool collectImage;
           bool saveImageByEnergy;
           string imageFileName;
           string imageTableName;
           fitsfile* imageFile;
           int xyAxisBins;
           double xyAxisMinMM;
           double xyAxisMaxMM;
           double nominalRAAtCenter;
           double nominalDECAtCenter;

           //
           // Running totals for results of ray tracing on photons
           //
           struct resultTotals {
               double       energy;
               int tracedPhotonCount;
               int tracedWithinPsfCoreRadiusCount;
               int impactOnFocalPlaneCount;
               int fpOnePerLayerCount;
               int fpOneLayerOnlyCount;
               int fpLayerOneOnlyCount;
               int fpLayerTwoOnlyCount;
               int fpNoReflectionCount;
               int fpAbnormalPathCount;
               int impactOnFPMaskCount;
               int fpMaskOnePerLayerCount;
               int fpMaskOneLayerOnlyCount;
               int fpMaskLayerOneOnlyCount;
               int fpMaskLayerTwoOnlyCount;
               int fpMaskNoReflectionCount;
               int fpMaskAbnormalPathCount;
               int impactOnMirrorTop;
               int impactOnObstruction;
               int impactOnOuterHousingCount;
               int impactOnInnerHousingCount;
               int absorbedOnFrontMirrorFaceCount;
               int absorbedOnBackMirrorFaceCount;
               int photonCanNotLeaveMirrorCount;
               int photonErrorCount;
	       // Add the case of collimator reflection
	       // (added by HIDEYUKI MORI)
	       int impactOnCollimatorTop;
	       int absorbedOnFrontCollimatorFaceCount;
	       int absorbedOnBackCollimatorFaceCount;
	       int photonCanNotLeaveCollimatorCount;
               // impactBySourceRadialBin is in fractional  arcmin bins
               // See the constructor code
               vector<int> impactBySourceRadialBin;
               // psfRadialBin is in millimeters on the focal plane
               vector<int> psfRadialBin;
               vector< vector<int> > image;
               resultTotals();
           } statByEnergy, total;

}; 

inline void 
XrrtStat::suppressByEnergyResultStatOutput()
{
    suppressEnergyResultRows = true;
}
#endif
