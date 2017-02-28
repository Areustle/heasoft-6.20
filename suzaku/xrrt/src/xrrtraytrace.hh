// xrrtraytrace.hh
//
// Class definition for XrrtRaytrace class
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Upgraded documentation and moved error codes from xrrt_types.hh
//            to here. R. Fink
// 1997/09/22 More documentation work. R. Fink
// 1999/02/04 Added code from ISAS to support Astro-E scatter function. R Fink
// 2006/07/17 Y.ISHISAKI	version 6.4.1
//	comment out #include "callib.h"
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	remove getDefaultScatterObject(), getDefaultPhotonObject()
//	remove setFrontRemap(),setBackRemap(),setPreCollimatorRemap()
//	remove setAscaParams(), setAstroeParams(), setAstroeBackParams()
//	remove setScatterIndexFileName(), getScatterIndexFileName()
//	private -> public defaultPhoton, defaultReflection, defaultScatterInfo
//	define XRRT_NAME,XRRT_CREDIT,XRRT_VERSION_MAJOR/MINOR/REVISION
//	modify arguments of loadMirrorDesc()
//	rename preCollimator -> pcol

#ifndef XRRTRAYTRACE_HH
#define XRRTRAYTRACE_HH

#define XRRT_NAME	"XRRT"
#define XRRT_CREDIT	"The XRRT X-Ray Ray-Tracing library Version %d.%d.%d"
#define XRRT_VERSION_MAJOR	6
#define XRRT_VERSION_MINOR	5
#define XRRT_VERSION_REVISION	5

// 
// XrrtRayTrace system interfaces used
//
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stl.h>
#include <exception>
// #include <string.h>
#include <cstring>
// #include <values.h>
// #include <stdlib.h>
#include <cstdlib>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtphoton.hh"
#include "xrrtstructure.hh"
#include "xrrtreflection.hh"
#include "xrrtscatter.hh"
#include "xrrtremapname.hh"
#include "xrrtmolecule.hh"
#include "xrrtatomicdata.hh"
#include "xrrtatomscatfactor.hh"
// Add for Quadrant-level ray-tracing
// Added by Hideyuki MORI : date 2006/01/27)
#include "xrrtquadrant.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

// 
// Ftools library interfaces
//
extern "C" {
#include "fitsio.h"
// CALDB interface routines
// #include "callib.h"
void c_fcerr(const char *errmsg);
}

// LOCAL defines
enum XrrtRayTraceErrorCode {
        XrrtRaytraceErrors,
        ASCA_PARAMS_PREVIOUSLY_SET, // Code defends against setting ASCA
                               // scatter param values more than once
        noMirrorExtension,     // The requested mirror extension was not found
                               // in the telescope description file
        noObstructionExtension, // The requested obstruction extension was not
                                // found in the telscope description file
	// Add the case of missing the quadrant extension
	// (added by Hideyuki MORI : date 2006/03/04)
	noQuadrantExtension,    // The requested quadrant extension was not
                                // found in the telscope description file
        missingReflectionTables, // A requested/needed reflection table was
                                 // not found in the reflection table file
	noPreCollimatorExtension, // The requested collimator extension was not found
                                  // in the collimator description file 
	// (modified by Hideyuki MORI)
        noFitsLayerCol,        // Missing layer column in mirror extension
        noFitsAssemblyCol,     // Missing assembly column in mirror extension
        noFitsNumberCol,       // Missing number column in mirror extension
        noFitsFragmentCol,     // Missing fragment column in mirror extension
        noFitsFunctionCol,     // Missing function column in mirror extension
        noFitsScatterCol,      // Missing scatter column in mirror extension
        noFitsFrelectCol,      // Missing freflect column in mirror extension
        noFitsBreflectCol,     // Missing breflect column in mirror extension
        noFitsFstartCol,       // Missing fstart column in mirror extension
        noFitsFendCol,         // Missing fend column in mirror extension
        noFitsScrossCol,       // Missing scross column in mirror extension
        noFitsEcrossCol,       // Missing ecross column in mirror extension
        noFitsTopinrCol,       // Missing topinr column in mirror extension
        noFitsTopoutrCol,      // Missing topoutr column in mirror extension
        noFitsBotinrCol,       // Missing botinr column in mirror extension
        noFitsBotoutrCol,      // Missing botoutr column in mirror extension
        noFitsTopdCol,         // Missing topd column in mirror extension
        noFitsBotdCol,         // Missing botd column in mirror extension
        noFitsPlayerCol,       // Missing layer column in obstruction extension
        noFitsPolynumCol,      // Missing polynum column in obstruction extension
        noFitsDistanceCol,     // Missing distance column in obstruction extension
        noFitsXvertexCol,      // Missing xvertex column in obstruction extension
        noFitsYvertexCol,      // Missing yvertex column in obstruction extension
	// Add error types for the quadrant informations
	// (added by Hideyuki MORI : date 2006/03/04)
	noFitsQuadrantCol,     // Missing quadrant column in quadrant extension
	noFitsQlayerCol,       // Missing quadrant layer column in quadrant extension
	noFitsDeltaxCol,       // Missing deltax column in quadrant extension
	noFitsDeltayCol,       // Missing deltay column in quadrant extension
	noFitsDeltazCol,       // Missing deltaz column in quadrant extension
	noFitsDeltaTxCol,      // Missing deltaTx column in quadrant extension
	noFitsDeltaTyCol,      // Missing deltaTy column in quadrant extension
	noFitsDeltaTzCol,      // Missing deltaTz column in quadrant extension
	// End of the modification
        noFitsEnergyCol,       // Missing energy column in reflection table 
                               // extension
        noFitsBangleCol,       // Missing bangle column in reflection table 
                               // extension
        noFitsRefprobCol,      // Missing refprob column in reflection table 
                               // extension
	// Add the pre-collimator error type (modified by Hideyuki Mori)
	noFitsColLayerCol,     // Missing layer column in collimator extension
        noFitsColAssemblyCol,  // Missing assembly column in collimator extension
        noFitsColNumberCol,    // Missing number column in collimator extension
        noFitsColFragmentCol,  // Missing fragment column in collimator extension
        noFitsColFunctionCol,  // Missing function column in collimator extension
        noFitsColScatterCol,   // Missing scatter column in collimator extension
        noFitsColFrelectCol,   // Missing freflect column in collimator extension
        noFitsColBreflectCol,  // Missing breflect column in collimator extension
        noFitsColFstartCol,    // Missing fstart column in collimator extension
        noFitsColFendCol,      // Missing fend column in collimator extension
        noFitsColScrossCol,    // Missing scross column in collimator extension
        noFitsColEcrossCol,    // Missing ecross column in collimator extension
        noFitsColTopinrCol,    // Missing topinr column in collimator extension
        noFitsColTopoutrCol,   // Missing topoutr column in collimator extension
        noFitsColBotinrCol,    // Missing botinr column in collimator extension
        noFitsColBotoutrCol,   // Missing botoutr column in collimator extension
        noFitsColTopdCol,      // Missing topd column in collimator extension
        noFitsColBotdCol,      // Missing botd column in collimator extension

        invalidFormatVersionString,      // invalid 'FORMAT_VERSION(n)' string
        unknownFormatVersion,  // unknown FORMAT_VERSION

         // Add error types for the reflection table of the Astro-E2 pre-collimator blade  
         // (modified by Hideyuki MORI : date 2003/01/14)
        missingPreCollimatorReflectionTables, // A requested/needed reflection table was
                                              // not found in the Pre-Collimator reflection table file
        noPreCollimatorFitsEnergyCol,  // Missing energy column in Pre-Collimator blade
                                       // reflection table extension
        noPreCollimatorFitsBangleCol,  // Missing bangle column in Pre-Collimator blade
                                       // reflection table extension
        noPreCollimatorFitsRefprobCol, // Missing refprob column in Pre-Collimator blade
                                       // reflection table extension
	// End of the modification
        
	noTelescopeDefined,    // A ray trace was requested before a telescope
                               // description file was loaded
        noPhotonDefined,       // A ray trace was requested when no photon
                               // had been defined
        badStructureLayerType, // A layer type that was not programmed for
                               // was detected in code
        unsupportedMirrorType, // A mirror type that is not programmed for was
                               // requested

        // Add error type for the pre-collimator (added by Hideyuki MORI)
	unsupportedCollimatorType, // A collimator type that is not programmed 
	                           // for was requested
        invalidScross,         // The scross FITS column returned something
                               // other than true or false
        invalidEcross,         // The ecross FITS column returned something
                               // other than true or false

	// Add error type for the pre-collimator (added by Hideyuki MORI)
        invalidCollimatorScross, // The scross FITS column returned something
	                         // other than true or false
        invalidCollimatorEcross, // The ecross FITS column returned something
                                 // other than true or false

        requestedFitsFileNotOpen, // A keyword write was requested to a non-open
                               // FITS file pointer
        missingCALDBVariable,  // CALDB enevironment variable not available at
                               // run time.
        caldbError,            // some general CALDB error
        errorLoadingReflectionTables, // pass down error from loadMirrorDesc()
        invalidChemicalFormula, // xrrtexternal.cc access of XrrtMolecule
        outOfMemory,            // out of memory
        XrrtRaytraceErrorsEnd
};

// END LOCAL defines

//
// Support for ray tracing photons thru telescopes
//
class  XrrtRaytrace {
    //
    // theRaytraceInfo() needs to be a friend to access the private
    // constructor for the static XrrtRayTrace object.
    //
    friend XrrtRaytrace& theRaytraceInfo();

    public:
          // Mount a pre-collimator for removing stray lights?
          // (modified by H. Mori : date 2003/01/14)
          bool            mountPreCollimator;

          //
          // Functions to allow class users to set up conditions before
          // ray tracing
          //
          // Return Telescope Description Files and Reflection tables file
          // names from the CALDB (obsolete)
          // string findTDFByMission(string& missionName, string& telescopeName);
          // string findREFLECTByMission(string& missionName, string& telescopeName);
          // Function to load a telescope & collimator description
          // (modified by H. Mori : date 2003/01/14)
          void loadMirrorDesc(string mirrorFileName, 
                              string mirrorExtension,
                              string obstructExtension,
                              string quadrantExtension,
			      string pcolExtension,
                              string reflectFileName,
	                      string backprofFileName);

          void loadReflect(fitsfile* fitsReflectFile, XrrtTable* reflectTable);

          //
          // Define parameters for internal calculation of reflection values
          // in place of external table files
          void defineInternalReflectionTables(string& frontSurfaceTableName,
                                              XrrtMolecule* frontSurface,
                                              double& frontSurfaceCGSDensity,
                                              double& frontSurfaceRoughness,
                                              string& backSurfaceTableName,
                                              XrrtMolecule* backSurface,
                                              double& backSurfaceCGSDensity,
                                              double& backSurfaceRoughness);
          // Routines to find files in the CALDB (obsolete)
          // string findAtomScatFactorFileName();
          // string findAtomDataFileName();

          // Force the ray tracing to read local Atomic Data Files
          // rather than access them from the CALDB
          void setAtomicDataFileNames(string& atomicScatterFileName,
                                      string& atomicInfoFileName);

          // Function to override all scatter modes from the mirror file
          // to a single scatter mode.
          void setOverrideScatterMode(const  SurfaceScatterMode& mode);

          // Function to set the state of the photon BEFORE it is traced
          void initPhoton(const double& energy, 
                          const double& radius, 
                          const double& rotationangle,
                          const double& zdistance,
                          const double& photonUnitRadial,  
                          const double& photonPhiAngle,  
                          const double& photonUnitZ);

          // Function to override the normal location of the focal plane 
          // (zero millimeters on the z-axis) to a new value.
          void setFocalPlanePosition(const double& focalPlaneZ);

          //
          // Functions to carry out a ray trace on a photon
          //
          PhotonStatusCodes rayTrace();

          //
          // Functions to provide access to information about the current
          // state information used in ray tracing
          //
          // Function to return whether ASCA Scatter Params have been set
          bool getAscaParamStatus();
          // Function to return whether ASTROE Scatter Params have been set
          bool getAstroeParamStatus();

          // Function to return whether any mirror had NONE as a scatter option
          bool scatterNoneExists() const;

          // Function to return whether any mirror had ASCA as a scatter option
          bool scatterAscaExists() const;

          // Function to return whether any mirror had Astro-E as scatter option
          bool scatterAstroeExists() const;

          // Function to return the smallest usable radius for the telescope
          double getMinimumTelescopePhotonRadiusMM() const;

          // Dunction to return the largest usable radisu for the telescope
          double getMaximumTelescopePhotonRadiusMM() const;

          // 
          // Function to write information only known by XrrtRayTrace
          // to a FITS file header
          //
          void writeHistoricalKeywords(fitsfile* fitsFilePtr);

          //
          // Function to convert error codes to messages
          //
          string errorMessage(const XrrtRayTraceErrorCode& errorCode);

          //
          // Function to access the telescope name of the Astro-E2 XRTs
          // (added by Hideyuki MORI : date 2006/01/31)
          //
          void setTelescopeName(string& telescopeName);
          string getTelescopeName() const;

          // Pointer to static photon info object
          XrrtPhoton&     defaultPhoton;
          // Pointer to static reflection off mirror surface info object
          XrrtReflection& defaultReflection;
          // Pointer to static mirror scattering info object
          XrrtScatter&    defaultScatterInfo;

    private:
          // Constructor
          // The constructors are made private because we use a static
          // XrrtRayTrace object for ray tracing.
          XrrtRaytrace(); 
          // Copy Constructor
          XrrtRaytrace( const XrrtRaytrace& rayTrace ); 

          // 
          // Functions only used by XrrtRayTrace
          //

          // Function to trace a photon thru a mirror layer
          void mirrorLayerTrace(const Count& layer);

          // Function to trace a photon thru an obstruction layer
          void obstructionLayerTrace(const Count& layer);

          // Add the case of Pre-Collimator (added by H. Mori)
          // Function to trace a photon thru an Pre-Collimator layer
          void collimatorLayerTrace(const Count& layer);

          // Function to return a pointer to a new or existing
          // reflection table
          XrrtTable* getFrontTableEntry( const string& reflectTableString);

          // Function to return a pointer to a new or existing
          // reflection table
          XrrtTable* getBackTableEntry( const string& reflectTableString);

          // Function to return a pointer to a new or existing Pre-Collimator 
          // blade reflection table (added by H. Mori : date 2003/01/14)
          XrrtTable* getPcolTableEntry( const string& reflectTableString);

          //
          // Data members
          //
          // Control of possible text output common to FTOOLS
          int raytraceChatter;

          // Atomic data file names
          bool overrideCALDBAtomicFiles;
          string atomicScatterFactorsFileName;
          string atomicDataFileName;

          // Storage for when Reflection tables are dynamically calculated
          bool dynamicReflectionTable;
          string frontSurfaceTableName;
          XrrtMolecule* frontSurface;
          double frontSurfaceCGSDensity;
          double frontSurfaceRoughness;
          string backSurfaceTableName;
          XrrtMolecule* backSurface;
          double  backSurfaceCGSDensity;
          double backSurfaceRoughness;

          // Array of front of mirror reflection table names
          vector<string> frontReflectTables;
          // Mappings from old table names to new table names
          XrrtRemapName frontRemap;

          // Array of back of mirror reflection table names
          vector<string> backReflectTables;
          // Mappings from old table names to new table names
          XrrtRemapName backRemap;

          // Array of Pre-Collimator blade reflection table names
          // (modified by H. Mori : date 2003/01/14)
          vector<string> pcolReflectTables;
          // Mappings from old table names to new table names
          // (modified by H. Mori : date 2003/01/14)
          XrrtRemapName pcolRemap;

          //
          // References to static objects used by XrrtRayTrace
          //
          // Pointer to static telescope structure info object
          XrrtStructure&  structure;
          //
          // For the ray-tracing at the quadrant level
          // (added by Hideyuki MORI : date 2005/12/17)
          //
          // Pointer to static quadrant structure info object
          XrrtQuadrant& quadrant;

          //
          // XrrtRayTrace status variables
          //
          // Has a telescope been loaded yet?
          bool            telescopeDefined;
          // Has a photon been defined yet?
          bool            photonDefined;
          // During telescope loading, did a mirror use NONE scattering?
          bool            scatterNoneFound;
          // During telescope loading, did a mirror use ASCA scattering?
          bool            scatterAscaFound;
          // During telescope loading, did a mirror use Astro-E scattering?
          bool            scatterAstroeFound;

          // 
          // XrrtRayTrace limits variables
          //
          // Where is the focal plane along the z-axis in mm?
          double          assumedFocalPlaneZ;
          // What is the minimum usable telescope radius in millimeters?
          double          minimumTelescopePhotonRadiusMM;
          // What is the maximum usable telescope radius in millimeters?
          double          maximumTelescopePhotonRadiusMM;

          // 
          // Save the telescope name of Astro-E2 XRTs
          // (added by Hideyuki MORI : date 2006/01/31)
          //
          string astroeXrtTelescopeName;
}; 

//
// Function prototype for global access to the static XrrtRayTrace object
//
XrrtRaytrace& theRaytraceInfo();

#endif
