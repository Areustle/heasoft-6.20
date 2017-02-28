/// \file xrtraytrace.cxx
/// \brief Calculate ray tracing
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2017/01/17 15:07:20 $

/** 

\defgroup tool_xrtraytrace Trace a photon ray through an X-ray telescope

The xrtraytrace tool calculates the path of a photon through an X-ray 
telescope. 

Source files:

  xrtraytrace.cxx
  xrtraytrace_lib.cxx
  xrtraytrace_lib.h

Library dependencies:

  heacore/ape
  heacore/heaapp
  heacore/heautils
  heacore/cfitsio
  heacore/ahlog

Modification history:

  Ver   Date         Author  Description
  1.0   2015-08-10   KLR    Clean-up code


\internal
\note All the vector elements in this code is accessed using the [] operator, 
      rather than at(), in order to keep the code as fast as possible.


*/


#define AHLABEL tool_raytrace
#define AHCVSID "$Id: "
#define TOOLTAG "$Name: Hitomi_18Jan2017_V005a $"

#include "xrtraytrace_lib.h"

// heaapp access for start-up and shut-down.
#include "heaapp/heaapp.h"

// needed by the startUp and shutDown functions
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

// heautils utilities
#include "headas_utils.h"   // for get_history, headas_clobpar, etc


/** \addtogroup tool_raytrace
 *  @{
 */


/**********************************************
 * ********************************************
 * 		Declare the standard functions
 * ********************************************
**********************************************/


// include startup and shutdown functions from heaapp
int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);
int shutDown(HeaAppData * appdata);

/// \brief Get parameter values from raytrace.par file and command input
void getPar(Param & param, Photons & photons);

/// \brief Setup tool operation by, for example: copying files, loaded
///  data sets, allocating memory, etc
/// \param[out] topExtObjects in each of these ExternalObjectsStructure is a vector of the individual parts for all the top/bottom ExternalObjects, the number of those parts, and the z coordinate of the top/bottom that is closest to the telescope
void initialize(Param & param, 
                Photons & photons,  
                PSF_EEF & psf, 
                EA & ea, 
                ReflectTransGrids & reflectTransGrids, 
                Scattering & scat, 
                bool & has3DObjects, 
                double & entryRad, 
                double & entryAngle, 
                long & entrysegment,
                bool & doHistoryFile, 
                double & plateScaleArcmin, 
                double & plateScaleArcsec, 
                std::vector<HousingGeometry> & housings, 
                double & housingHardLimit,
                long & numxrtzlevels, 
                std::vector<double> & xrtzlevels, 
                CartesianCoord & initialPhotonPos, 
                long & numXRTObjects, 
                std::vector<XRTObject> & XRTObjects, 
                std::vector<int> & obstructionIntervalLowIndex, 
                std::vector<int> & obstructionIntervalHighIndex, 
                std::vector<long> & zminsortxrtobjectindex, 
                std::vector<long> & zmaxsortxrtobjectindex,  
                std::vector<long> & zmaxsortxrtobjectindexAll, 
                int & numRoughSurfaces, 
                long & numGroups, 
                bool & preinterpolateGrids, 
                RemappedReflectTransGrids & remappedGrids, 
                std::map<std::string, int> & historyColumns, 
                bool & doTransmission, 
                Aperture & aperture, 
                Transforms & transforms, 
                SectorsShells & sectorsShells, 
                ExternalObjectsStructure & topExtObjects, 
                ExternalObjectsStructure & bottomExtObjects, 
                GenTelescope & genTelescope);

/// \brief Do the actual work of the tool
void doWork(Param & param, 
            Photons & photons, 
            PSF_EEF & psf, 
            EA & ea, 
            const ReflectTransGrids & reflectTransGrids, 
            RemappedReflectTransGrids & remappedGrids, 
            const Scattering & scat, 
            long numFrontAngles, 
            long numRoughAngles, 
            long numPcolAngles, 
            long numGroups, 
            int numRoughSurfaces, 
            bool preinterpolateGrids, 
            long numXRTObjects, 
            const std::vector<XRTObject> & XRTObjects, 
            const std::vector<HousingGeometry> & housings, 
            double housingHardLimit,
            const std::vector<long> & zmaxsortxrtobjectindexAll, 
            long numxrtzlevels, 
            const std::vector<double> & xrtzlevels, 
            const std::vector<int> & obstructionIntervalLowIndex, 
            const std::vector<int> & obstructionIntervalHighIndex, 
            bool doTransmission, 
            bool has3DObjects, 
            double & entryRad, 
            double & entryAngle, 
            long & entrysegment,
            bool doHistoryFile, 
            double plateScaleArcmin, 
            double plateScaleArcsec, 
            CartesianCoord & initialPhotonPos, 
            std::map<std::string, int> & historyColumns, 
            const Aperture & aperture, 
            const Transforms & transforms, 
            const SectorsShells & sectorsShells, 
            const ExternalObjectsStructure & topExtObjects, 
            const ExternalObjectsStructure & bottomExtObjects, 
            const GenTelescope & genTelescope);

/// \brief Close open fits file, free any objects
void finalize(Param & param);




/**********************************************
 * ********************************************
 * 		Declare initialize() helper functions
 * ********************************************
**********************************************/

void initializePhotons(Param & param, 
                       Photons & photons, PSF_EEF & psf,
                       bool & preinterpolateGrids);
void getInputEnergies(std::string & energyInput, 
                      Photons & photons, 
                      Param & param);

void setupHistoryFile(Param & param, 
                      std::map<std::string, int> & historyColumns);
void setupPSFFile(Param & param);
void setupEAFile(Param & param);

double op_cos(double angle);
double op_sin(double angle);
double op_abs(double angle);
double op_fmod360(double angle);



/**********************************************
 * ********************************************
 * 		Define main() function
 * ********************************************
**********************************************/

/// \brief raytrace tool main function
int main(int argc, char** argv) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // variables needed as part of this main() pattern.
  int finalstatus = 0;        // Global status of tool run. Set to non-0 at the 
                              // first error and remains non-0 thereafter.
  int status = 0;             // Status of an individual function call. Reused 
                              // for the next function call. 0=normal
  HeaAppData appdata = { 0 }; // Application data, used by startUp/shutDown to 
                              // initialize support libraries.
  
//  int status = 0;             // headas status (0 = normal)
  
  // declare structs to hold data
  Param param;
  Photons photons;
  PSF_EEF psf;
  EA ea;
  ReflectTransGrids reflectTransGrids;
  RemappedReflectTransGrids remappedGrids;
  Scattering scat;
  Aperture aperture;
  Transforms transforms;
  SectorsShells sectorsShells;
  GenTelescope genTelescope;
  
  // this is a keyword in the TDF
  long numGroups = 0;           // in SURFACE extension
  
  // this will be at least 1 (back surface), maybe 2 (precollimator)
  int numRoughSurfaces = 0;
  
  // flag to tell us whether to preinterpolate the reflectivity, etc grids, 
  // or to do them on the fly
  bool preinterpolateGrids = false;
  
  long numXRTObjects = 0;
  std::vector<XRTObject> XRTObjects;
  
  // Create an array of indices that point to elements in the bounding box array xrtobjectboundingbox (and related arrays) that correspond to sorting on zmin (zmin is the lowest z-coordinate of an xrt object). The sort order is descending, starting from the highest zmin. It is ok to sort on zmin using the bounding boxes before the coordinate transformations are applied because we only need crude z-coordinate divisions in the telescope structure to implement a scheme that improves run time by restricting the number of candidate photon/xrt object interactions. We also do not need to distinguish between XRT object types because the routine that finds candidate XRT objects for a photon to interact with operates on all object types.  make a similar index array sorted for zmax
  std::vector<long> zminsortxrtobjectindex;   // +++ why pass these in?  Only initialize() uses them
  std::vector<long> zmaxsortxrtobjectindex;   // +++ why pass these in?  Only initialize() uses them
  std::vector<long> zmaxsortxrtobjectindexAll;
  
  long numxrtzlevels = 0;
  std::vector<double> xrtzlevels;
  bool doTransmission = false;        // whether to treat transmission, or just reflection
  bool has3DObjects = false;          // flag to alert that we have 3D objects. currently, only 2D are supported
  double entryAngle = 0.0;
  double entryRad = 0.0;
  long entrysegment = 0;              // Entry segment number used in diagnostic mode  

  bool doHistoryFile = false;       // +++ this should be a struct about the history file, 
                                    // along with the columns map, fitsfile *, etc
  
  // the plate scale on the focal plane
  double plateScaleArcmin = 0.0;
  double plateScaleArcsec = 0.0;
  
  CartesianCoord initialPhotonPos;
  
  // This is a three element vector of HousingGeometry structs.  
  //      housings[0] = precollimator 
  //      housings[1] = primary mirrors 
  //      housings[2] = secondary mirrors
  // even if there is no precollimator, we still require a three element vector
  // +++ use a map?
  std::vector<HousingGeometry> housings;
  
  // crude bound to size of photon bounding box limited by telescope housing 
  // (i.e., largest outer radius + largest X-offset)
  double housingHardLimit = 0.0;
  
  // associate column names and numbers
  std::map<std::string, int> historyColumns;
  
  // in each of these ExternalObjectsStructure is a vector of the individual 
  // parts for all the top/bottom ExternalObjects, the number of those parts, 
  // and the z coordinate of the top/bottom that is closest to the telescope
  ExternalObjectsStructure topExtObjects;
  ExternalObjectsStructure bottomExtObjects;
  
  
  // These are set in xrtSetup() and needed in raytraceOnePhoton()
  // Pointer to array element in for the xrtobject* arrays for the 1st member (low index) and last object (high index) of a group of obstructions: since obstructions start with an xrtobjectid of 0, it is equal to the xrtobjectid of the 1st obstruction in a group (interval). 
  // We define 3 groups: (use enum obstructionInterval_e)
  // 0 ABOVE   = above pre-collimator, above and touching the primaries
  // 1 BETWEEN = between primaries and secondaries
  // 2 BELOW   = below the secondaries
  
  std::vector<int> obstructionIntervalLowIndex(s_numObstrIntervals, -1); // Initialize to -1
  std::vector<int> obstructionIntervalHighIndex(s_numObstrIntervals); // Initialize to 0 
  
  // -------------------------------------
  
  
  // Global initializations. Set up logging streams, open parameter file,
  // handle universal parameters like "debug" and "chatter", etc.
  status = startUp(argc, argv, TOOLTAG, &appdata);
  if (0 != status) {
    // +++ ahlog_err or AH_ERR ?
    ahlog_err("main", "startUp returned status %d, not 0 as expected.\n", status);
    finalstatus = 1; // Latch error status
  }

  if (0 == status) {
    
    if (ahlog::get_debug()) {
      
      AH_DEBUG << "Entering debug mode." << std::endl;
      
      getPar(param, photons);
      
      // Write all parameters to the log file.  When debug=yes, the params will 
      // be written twice: here, and again at the end of initialize.  That is 
      // to ensure against a possible failure in initialize().
      writeParametersToLog();
      
      initialize(param, photons, psf, ea, reflectTransGrids, scat, has3DObjects, entryRad, entryAngle, entrysegment, doHistoryFile, plateScaleArcmin, plateScaleArcsec, housings, housingHardLimit, numxrtzlevels, xrtzlevels, initialPhotonPos, numXRTObjects, XRTObjects, obstructionIntervalLowIndex, obstructionIntervalHighIndex, zminsortxrtobjectindex, zmaxsortxrtobjectindex, zmaxsortxrtobjectindexAll, numRoughSurfaces, numGroups, preinterpolateGrids, remappedGrids, historyColumns, doTransmission, aperture, transforms, sectorsShells, topExtObjects, bottomExtObjects, genTelescope);
      
      doWork(param, photons, psf, ea, reflectTransGrids, remappedGrids, scat, reflectTransGrids.m_numFrontAngles, reflectTransGrids.m_numRoughAngles, reflectTransGrids.m_numPcolAngles, numGroups, numRoughSurfaces, preinterpolateGrids, numXRTObjects, XRTObjects, housings, housingHardLimit, zmaxsortxrtobjectindexAll, numxrtzlevels, xrtzlevels, obstructionIntervalLowIndex, obstructionIntervalHighIndex, doTransmission, has3DObjects, entryRad, entryAngle, entrysegment, doHistoryFile, plateScaleArcmin, plateScaleArcsec, initialPhotonPos, historyColumns, aperture, transforms, sectorsShells, topExtObjects, bottomExtObjects, genTelescope);
      
      finalize(param);
      
    } else {
      // not in debug mode
      
      try {
        getPar(param, photons);
        initialize(param, photons, psf, ea, reflectTransGrids, scat, has3DObjects, entryRad, entryAngle, entrysegment, doHistoryFile, plateScaleArcmin, plateScaleArcsec, housings, housingHardLimit, numxrtzlevels, xrtzlevels, initialPhotonPos, numXRTObjects, XRTObjects, obstructionIntervalLowIndex, obstructionIntervalHighIndex, zminsortxrtobjectindex, zmaxsortxrtobjectindex, zmaxsortxrtobjectindexAll, numRoughSurfaces, numGroups, preinterpolateGrids, remappedGrids, historyColumns, doTransmission, aperture, transforms, sectorsShells, topExtObjects, bottomExtObjects, genTelescope);
        doWork(param, photons, psf, ea, reflectTransGrids, remappedGrids, scat, reflectTransGrids.m_numFrontAngles, reflectTransGrids.m_numRoughAngles, reflectTransGrids.m_numPcolAngles, numGroups, numRoughSurfaces, preinterpolateGrids, numXRTObjects, XRTObjects, housings, housingHardLimit, zmaxsortxrtobjectindexAll, numxrtzlevels, xrtzlevels, obstructionIntervalLowIndex, obstructionIntervalHighIndex, doTransmission, has3DObjects, entryRad, entryAngle, entrysegment, doHistoryFile, plateScaleArcmin, plateScaleArcsec, initialPhotonPos, historyColumns, aperture, transforms, sectorsShells, topExtObjects, bottomExtObjects, genTelescope);
      } catch (const std::exception &x) {
        
        // Write all parameters to the log file.  This may be the second time 
        // they are written (the first was at the end of initialize()), just 
        // in case there was a failure in initialize() or doWork().
        writeParametersToLog();
        
        finalstatus = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(param);
      } catch (const std::exception &x) {
        finalstatus = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      
    }
    
  }
  
  // +++ surround with a try-catch block?
  // Perform global clean-up in parallel to startUp function. Close parameter file, close
  // logging streams, etc.
  status = shutDown(&appdata);
  if (0 != status) {
    // Note that after shutDown, the usual error message streams no longer 
    // print anything. Thus, in the unlikely case that shutDown signaled an 
    // error, it must be reported here with appdata's printerr function. This 
    // will still print to the user's terminal even after the streams have been 
    // shut down. Also note this message is not strictly necessary because 
    // shutDown will log its own errors, but this is included just to 
    // demonstrate how heaapp's message functions work.
    appdata.printerr("main", "shutDown returned status %d, not 0 as expected.\n", status);
    finalstatus = 1;
  }

  // Return the final status
  return finalstatus;
  
}   // end main()


// ****************************************************************************


/**********************************************
 * ********************************************
 * 		Define the standard functions
 * ********************************************
**********************************************/

// get the values from the .par file and store them in a Param structure
void getPar(Param & param, Photons & photons) {
  
  // -------------------------------------
  // get the input parameters
  // -------------------------------------
  
  // input/output filenames and extensions
  param.m_mirrorfile      = getParString("mirrorfile");   
  param.m_obstructfile    = getParString("obstructfile");  
  param.m_frontreffile    = getParString("frontreffile");   
  param.m_backreffile     = getParString("backreffile");    
  param.m_pcolreffile     = getParString("pcolreffile");    
  param.m_scatterfile     = getParString("scatterfile");
  
  param.m_numphoton       =  getParInt("numphoton");
  param.m_energy          =  getParString("energy");
  param.m_seed            =  getParInt("seed");
  param.m_misalign        =  getParString("misalign");
  
  param.m_transmode       =  getParString("transmode");
  param.m_scattermode     =  getParString("scattermode");
  param.m_source          =  getParString("source");
  param.m_betapars        =  getParString("betapars");
  param.m_flatradius      =  getParDouble("flatradius");
  param.m_psrcfile        =  getParString("psrcfile");
  param.m_diagpars        =  getParString("diagpars");
  param.m_offaxis         =  getParString("offaxis");
  param.m_roll            =  getParString("roll");
  param.m_annulus         =  getParString("annulus");
  param.m_rectangle       =  getParString("rectangle");
  
  param.m_outeafile       =  getParString("outeafile");
  param.m_outpsffile      =  getParString("outpsffile");
  param.m_psfpars         =  getParString("psfpars");
  param.m_resultsplanez   =  getParDouble("resultsplanez");
  
  param.m_resplaneonly    =  getParBool("resplaneonly");
  param.m_outphistfile    =  getParString("outphistfile");
  param.m_phisttype       =  getParString("phisttype");
  param.m_externobjects   =  getParString("externobjects");
  param.m_fastmode        =  getParString("fastmode");
  
  param.m_telescop        =  getParString("telescop");
  param.m_instrume        =  getParString("instrume");
  param.m_validdate       =  getParString("validdate");
  param.m_validtime       =  getParString("validtime");
  
  AH_DEBUG << "phistfn = " << param.m_outphistfile << std::endl;
  
  
  // Allow the input parameter numphoton to be negative to indicate that we 
  // want to print a message to the screen every time 1000 more photons have 
  // been traced, otherwise the increment for the message is calculated 
  // later. Then make numphoton positive.
  // NOTE: Because the par file has a min=1 for numphoton, APE will throw a
  // warning if numphoton<0.  To get around this, remove the 'min' value
  // in the syspfiles/xrtraytrace.par file.
  if (param.m_numphoton < 0) {
    photons.m_photonPrintIncrement = 1000;
    param.m_numphoton = std::abs(param.m_numphoton);
  } else {
    // the 0 value will indicate later that it needs to be calculated
    photons.m_photonPrintIncrement = 0;
  }


  
  // output to the user, after we queried for the parameters
  AH_OUT << "************************************" << std::endl;
  AH_OUT << "         START XRTRAYTRACE          " << std::endl;
  AH_OUT << "************************************" << std::endl;

  
  // -------------------------------------
  // store source param in a more convenient type
  // -------------------------------------
  
  // store the source param as an enum, to minimuze string searches in code
  if (isEqualCaseInsens(param.m_source, "point")) {
    param.m_sourceEnum = POINT;
  } else if (isEqualCaseInsens(param.m_source, "flatcircle")) {
    param.m_sourceEnum = FLATCIRCLE;
  } else if (isEqualCaseInsens(param.m_source, "betamodel")) {
    param.m_sourceEnum = BETAMODEL;
  } else if (isEqualCaseInsens(param.m_source, "photonlist")) {
    param.m_sourceEnum = PHOTONLIST;
  } else if (isEqualCaseInsens(param.m_source, "groundmodel")) {
    param.m_sourceEnum = GROUNDMODEL;
  } else if (isEqualCaseInsens(param.m_source, "diagnosticmode")) {
    param.m_sourceEnum = DIAGNOSTICMODE;
  }
  
  
  // -------------------------------------
  // preliminary error checking
  // -------------------------------------
  
  // A minimum of one output file must be specified
  if ( !isInputFileGiven(param.m_outeafile) &&
       !isInputFileGiven(param.m_outpsffile) && 
       !isInputFileGiven(param.m_outphistfile) ) {
    AH_THROW_RUNTIME("You must specify at least one output file");
  }
  
  // make sure a photon file is provided if needed
  if ( ( param.m_sourceEnum == PHOTONLIST || 
         param.m_sourceEnum == GROUNDMODEL ) &&
       !isInputFileGiven(param.m_psrcfile) ) { 
    AH_THROW_RUNTIME("A photon source file must be provided in the 'psrcfile' parameter.");
  }
  
  
  // -------------------------------------
  // decide some flags
  // -------------------------------------
  
  // flag if there is a precollimator
  param.m_pcolExists = isInputFileGiven(param.m_pcolreffile);
  
  // flag if tool should write a history file
  param.m_phistory = isInputFileGiven(param.m_outphistfile);
  
  // flag if we should write all the coordinates to the history file
  if ( isEqualCaseInsens(param.m_phisttype, "full") ) {
    param.m_writeCoords = true;
  }
  
  // flag if tool should treat any objects in AZIMUTHALSTRUCT extension of TDF
  // defaults of all these are false
  if ( !isEqualCaseInsens(param.m_externobjects, "none") ) {
    param.m_doExternalObjects = true;
    if ( isEqualCaseInsens(param.m_externobjects, "all") ) {
      param.m_doTopExternalObjects = true;
      param.m_doBottomExternalObjects = true;
    } else if ( isEqualCaseInsens(param.m_externobjects, "top") ) {
      param.m_doTopExternalObjects = true;
    } else if ( isEqualCaseInsens(param.m_externobjects, "bottom") ) {
      param.m_doBottomExternalObjects = true;
    }
  } // end-if check for m_externobjects
  
  // flag what kind of fastmode we should use
  if ( isEqualCaseInsens(param.m_fastmode, "yes") ) {
    param.m_doFastMode = true;
    AH_DEBUG << "fastmode = yes" << std::endl;
  } else if ( isEqualCaseInsens(param.m_fastmode, "super") ) {
    // +++ 20141215 this is for an unadvertised version of the code, using 
    //              raytracePhotonFast()
    param.m_doSuperFastMode = true;
    AH_DEBUG << "fastmode = super" << std::endl;
  } else if ( isEqualCaseInsens(param.m_fastmode, "no") ) {
    // keep default values of m_doFastMode and m_doSuperFastMode = false
    AH_DEBUG << "fastmode = no" << std::endl;
  } else {
    // +++ or AH_ERR?
    AH_THROW_RUNTIME("A value of 'yes' or 'no' must be entered for the param fastmode");
  }
  
  // +++ TODO make sure each list of numbers (annulus, rectangle, etc) has the correct number of numbers
  // +++ TODO validate the validdate and validtime params
  
  
  // -------------------------------------
  // store input string params in more 
  // convenient type
  // -------------------------------------
  
  // extract the vectors from other string lists
  listStringsToDoubles(param.m_offaxis, param.m_offaxisVec);
  listStringsToDoubles(param.m_roll, param.m_rollVec);
  listStringsToDoubles(param.m_psfpars, param.m_psfparsVec);
  
  // Make sure none of the input off-axis angles are 90 degrees
  for (uint iOffAx = 0 ; iOffAx < param.m_offaxisVec.size() ; ++iOffAx) {
    if (param.m_offaxisVec[iOffAx] >= s_90DegInArcmin) {
      AH_THROW_RUNTIME("Offaxis angles must be less than 90 degrees (5400 arcmin).");
    }
  }
  
  // +++ 20150914 store the theta and roll here, and check their sizes equal if in pair mode?
  // (right now it's in initializePhotons I think)
  
  // -------------------------------------
  // resolve CALDB parameters  
  // -------------------------------------
  
  // resolve them, then record actual path in par file and in history keywords
  // +++ 20150721 KLR use m_validdate and m_validtime for CALDB query?
  
  // mirrorfile
  param.m_mirrorfile = resolve(param.m_mirrorfile, "mirror", param.m_instrume, "-", "MIRROR", "-", "-", param.m_telescop);
  ape_trad_set_string("mirrorfile",param.m_mirrorfile.c_str());
  
  // obstructfile
  param.m_obstructfile = resolve(param.m_obstructfile, "obstruction", param.m_instrume, "-", "OBSTRUCT", "-", "-", param.m_telescop);
  ape_trad_set_string("obstructfile",param.m_obstructfile.c_str());
  
  // frontreffile
  param.m_frontreffile = resolve(param.m_frontreffile, "front reflectivity", param.m_instrume, "-", "REFLECT-TRANS", "-", "-", param.m_telescop);
  ape_trad_set_string("frontreffile",param.m_frontreffile.c_str());
  
  // backreffile
  param.m_backreffile = resolve(param.m_backreffile, "back reflectivity", param.m_instrume, "-", "BACKSIDE-REFLECT", "-", "-", param.m_telescop);
  ape_trad_set_string("backreffile",param.m_backreffile.c_str());
  
  // pcolreffile
  param.m_pcolreffile = resolve(param.m_pcolreffile, "precollimator reflectivity", param.m_instrume, "-", "COLLIMATOR-REFLECT", "-", "-", param.m_telescop);
  ape_trad_set_string("pcolreffile",param.m_pcolreffile.c_str());
  
  // scatterfile
  if (!isEqualCaseInsens(param.m_scattermode, "NONE")) {
    param.m_scatterfile = resolve(param.m_scatterfile, "scattering", param.m_instrume, "-", "SEGMENT1-SCATTERING", "-", "-", param.m_telescop);
    ape_trad_set_string("scatterfile",param.m_scatterfile.c_str());
  }
  
  // Write all parameters to the log file.  This is placed here so that any 
  // CALDB filenames are resolved.
  writeParametersToLog();
  
  // -------------------------------------
  // make sure we're going to correct extensions, 
  // else code would just go to the first extension
  // (this is only a concern if user supplied files)
  // -------------------------------------
  
  if (param.m_mirrorfile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the mirror file.  Manually assigning extension 'MIRROR'" << std::endl; 
    param.m_mirrorfile.append("[MIRROR]");
  }
  if (param.m_obstructfile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the obstruction file.  Manually assigning extension 'OBSTRUCT'" << std::endl; 
    param.m_obstructfile.append("[OBSTRUCT]");
  }
  if (param.m_frontreffile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the front reflectivity file.  Manually assigning first extension" << std::endl; 
    param.m_frontreffile.append("[1]");
  }
  if (param.m_backreffile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the back reflectivity file.  Manually assigning extension 'REFPROBBACK'" << std::endl; 
    param.m_backreffile.append("[REFPROBBACK]");
  }
  if (param.m_pcolreffile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the precollimator reflectivity file.  Manually assigning extension 'REFPROBPCOL'" << std::endl; 
    param.m_pcolreffile.append("[REFPROBPCOL]");
  }
  
  
}   // end getPar()


// ----------------------------------------------------------------------------


void initialize(Param & param, 
                Photons & photons, 
                PSF_EEF & psf, 
                EA & ea, 
                ReflectTransGrids & reflectTransGrids, 
                Scattering & scat, 
                bool & has3DObjects, 
                double & entryRad, 
                double & entryAngle,
                long & entrysegment, 
                bool & doHistoryFile, 
                double & plateScaleArcmin, 
                double & plateScaleArcsec,
                std::vector<HousingGeometry> & housings, 
                double & housingHardLimit, 
                long & numxrtzlevels, 
                std::vector<double> & xrtzlevels, 
                CartesianCoord & initialPhotonPos, 
                long & numXRTObjects, 
                std::vector<XRTObject> & XRTObjects, 
                std::vector<int> & obstructionIntervalLowIndex, 
                std::vector<int> & obstructionIntervalHighIndex, 
                std::vector<long> & zminsortxrtobjectindex, 
                std::vector<long> & zmaxsortxrtobjectindex, 
                std::vector<long> & zmaxsortxrtobjectindexAll, 
                int & numRoughSurfaces, 
                long & numGroups, 
                bool & preinterpolateGrids, 
                RemappedReflectTransGrids & remappedGrids, 
                std::map<std::string, int> & historyColumns, 
                bool & doTransmission, 
                Aperture & aperture, 
                Transforms & transforms, 
                SectorsShells & sectorsShells, 
                ExternalObjectsStructure & topExtObjects, 
                ExternalObjectsStructure & bottomExtObjects, 
                GenTelescope & genTelescope) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // misalignments[0] holds info for the pre-collimator foils (pcol) 
  // misalignments[1] holds info for the mirror foils
//  std::vector<Misalignment> misalignments;
  
  // flag if the telescope surface is multilayer or not
  // from keyword in refl file
  bool isMultiLayerSurface = false;
  
  // for looping through energies, making sure we have same grid as refl file
  bool foundEnergyMatch = false;
  
  // set a maximum number of energies for which to calculate the EEF/PSF, otherwise the resulting output files will be too large. If the input energy grid is larger than this maximum number of energies, only one EEF and PSF (image) is calculated per off-axis angle per azimuthal angle, summed over all energies in the input grid. A flag for this is set later, once the final size of the energy grid is determined.
  const int maxNumEEFEnergies = 10;
   
  // xrtregion2xrtobjectid(maxnumsets, maxmirrorsegment, sectorspersegment, numberofshells) 
  // This array returns the XRT object ID given the location in the telescope 
  // in terms of the object type and "set" (i.e. pre-collimators, primaries, 
  // or secondaries), the segment in that set, the sector in that segment, and 
  // the radial shell number. The dimensions of the array are obtained as 
  // follows:
  //    maxnumsets = total number "vertically arranged" groups of objects. 
  //        This number is normally 3 (pre-collimator, primaries, secondaries), 
  //        and could be 3 even if there is no pre-collimator and/or no 
  //        secondary mirror set. 
  //    maxmirrorsegment = maximum value in the ASSEMBLY column of the MIRORR 
  //        extension of the TDF (the number of segments for each mirror set 
  //        should be the same as the number of segments in the pre-collimator).
  //    sectorspersegment = maximum value in the SECTORNUMBER column of the 
  //        MIRROR extension in the TDF. Note that the SECTORNUMBER column 
  //        value is different to the xrtobjectsectorid() array because the 
  //        former is a number unique only to a segment whereas the latter is 
  //        a number unique to the entire telescope.
  //    numberofshells = maximum value in the NUMBER column of the MIRROR 
  //        extension in the TDF.
  vector4Dint xrtRegionToObjectID;
  
  // index of the array that corresponds to the array element for the first 
  // primary mirror (which comes after the last obstruction).  This is set 
  // inside xrtSetup().
  long firstMirrorIndex = 0;
  // index of the array that corresponds to the array element for the first 
  // pre-collimator foil (which comes after the last secondary mirror).  This 
  // is set inside xrtSetup().
  long lastMirrorIndex = 0;
  
  // Maps a shell (first shell, shell 1, is at index 0) to the group it belongs
  // in.  This is used to map the tauPermmIndex to the correct tauPermm for an 
  // XRTObject.  Different groups may have different thicknesses, so each 
  // taupermm must be calculated separately.
  // This is set up in readReflTrans() 
  //          (-> getMassAbsorptionData() -> setupTDFSurface())
  std::vector<int> xrtShellToGroupNumber;
  
  // -------------------------------------
  
  // seed the random number (to be freed in finalize())
  seedRandom(param.m_seed);
  
  
  // -------------------------------------
  // Consolidate foil tilt and twist misalignment parameters into one array
  // -------------------------------------
  
  // 140923 We will no longer have a random tilt and twist component, only the systematic components in the TDF. The pivot axis fractions for the primary, secondary and pre-collimator units will be read from keywords TLTPVPRI, TLTPVSEC, TLTPVPCL respectively, in the TDF (this will be done later in the code after the TDF is opened). The input parameters malignmode, ftilt, ptilt, ftwist, ptwist, and tiltpivot are therefore retired. A new input parameter, misalign, is introduced that switches on and off the six misalignments (tilt, twist, each applied to primary, secondary, and pre-collimator units).
  
  // Read the input parameter misalign from the input.par file: it should be a 
  // list of six numbers as indicated below. If the value of one of the numbers 
  // is negative then the tilt misalignment for the corresponding telescope 
  // component is switched off, otherwise a 0 or positive value indicates that 
  // the misalignment for that component should be switched on.
  Misalignment misalignment_struct;
  
  // extract the numbers from the misalign string
  std::vector<double> misalignmentFlags;
  listStringsToDoubles(param.m_misalign, misalignmentFlags);
  
  // make sure there are the correct number of flags
  // +++ 20150914 KLR This would fit better in getPar, with the others
  if (misalignmentFlags.size() != s_numMisalignmentFlags) {
    AH_THROW_RUNTIME("A flag is needed for each of the misalignments in the 'misalign' paramter: (pcol tilt, primary tilt, secondary tilt, pcol twist, primary twist, secondary twist)");
  }
  
  // now store the flags
  misalignment_struct.m_tiltSwitch[PCOL]       = (misalignmentFlags[0] >= 0 ? true : false);
  misalignment_struct.m_tiltSwitch[PRIMARY]    = (misalignmentFlags[1] >= 0 ? true : false);
  misalignment_struct.m_tiltSwitch[SECONDARY]  = (misalignmentFlags[2] >= 0 ? true : false);
  misalignment_struct.m_twistSwitch[PCOL]      = (misalignmentFlags[3] >= 0 ? true : false);
  misalignment_struct.m_twistSwitch[PRIMARY]   = (misalignmentFlags[4] >= 0 ? true : false);
  misalignment_struct.m_twistSwitch[SECONDARY] = (misalignmentFlags[5] >= 0 ? true : false);
  
  AH_DEBUG << "do pcol tilt = " << (misalignment_struct.m_tiltSwitch[PCOL] ? "true" : "false") << std::endl;
  AH_DEBUG << "do prim tilt = " << (misalignment_struct.m_tiltSwitch[PRIMARY] ? "true" : "false") << std::endl;
  AH_DEBUG << "do secon tilt = " << (misalignment_struct.m_tiltSwitch[SECONDARY] ? "true" : "false") << std::endl;
  AH_DEBUG << "do pcol twist = " << (misalignment_struct.m_twistSwitch[PCOL] ? "true" : "false") << std::endl;
  AH_DEBUG << "do prim twist = " << (misalignment_struct.m_twistSwitch[PRIMARY] ? "true" : "false") << std::endl;
  AH_DEBUG << "do secon twist = " << (misalignment_struct.m_twistSwitch[SECONDARY] ? "true" : "false") << std::endl;
  
  
  // -------------------------------------
  // Open the input photon file if specified, and set up some initial photon 
  // parameters for other input photon spatial distribution models
  // -------------------------------------
  
  initializePhotons(param, photons, psf, preinterpolateGrids);
  
  
  // -------------------------------------
  // Read the scattering file and set up the scattering probability tables
  // -------------------------------------
  
  //  The scattering file needs to be read before xrtsetup() because xrtsetup() will take the column names from the input scattering file and match them with references in the TDF, as well as create a unified index that will link every XRT object to a set of scattering probability tables, also allowing for different links for the front-side and back-side mirror surfaces.
  
  //  Only the arrays numscatcolumns and scatcolnames are passed to xrtseup()
  if (!isEqualCaseInsens(param.m_scattermode, "none")) {
    readScatteringFile(param.m_scatterfile, param.m_telescop, param.m_instrume, scat);
  }
  // Inputs:
  //  -scatterfile (string): scattering file name from input .par file
  //  -telescop (string): telescop from input .par file
  //  -instrume (string): instrume from input .par file
  // Outputs:
  //  -scat (struct): struct containing information about the scattering:
  //  -numscextensions (long): number of extensions found in the scattering file (should be 1 per segment)
  //  -numscatenergies (long): number of energies in the Energy column in the scattering file 
  //  -scatEnergy (long): array of energies for the scattering tables
  //  -numscatincidentangles (long): Number of incident angles for which scattering probability is tabulated
  //  -scatincidentangles (double): incident angle grid
  //  -numscatteringangles (long): Number of scattering angles for which scattering probability is tabulated
  //  -scatteringangles (double): scattering angle grid
  //  -maxnumscatcolumns (long): maximum number of scattering data columns in any extension of the scattering file
  //  -numscatcolumns (long): array containing number of scattering data columns in each extension 
  //  -scatcolnames (string): array of names for every scattering data column, organized by extension (=segment), and column id number
  //  -scatteringdistribution (double) array of cumulative scattering probabilities: see description in readscatteringfile() for details

  
  // -------------------------------------
  // set up the telescope structure
  // -------------------------------------
  
  std::vector<std::string> frontColumnNames;
  numxrtzlevels = 1;
  
  // create unified arrays of the properties of all XRT objects (obstructions, mirror foils, pre-collimator foils), including indexing for reflection, transmission, and scattering, as well as the creation of bounding boxes for each object, and net coordinate transformationsobjects
  xrtSetup(param, scat, misalignment_struct, genTelescope, housings, XRTObjects, numXRTObjects, firstMirrorIndex, lastMirrorIndex, zminsortxrtobjectindex, zmaxsortxrtobjectindex, zmaxsortxrtobjectindexAll, frontColumnNames, xrtzlevels, numxrtzlevels, obstructionIntervalLowIndex, obstructionIntervalHighIndex, xrtRegionToObjectID, has3DObjects, numGroups, transforms, topExtObjects, bottomExtObjects, sectorsShells);
  
  AH_DEBUG << "after xrtsetup" << std::endl;
  
  // Set the bound to the size of the photon bounding box
  // (largest outer radius + largest X-offset)
  std::vector<double> allrOuter(s_numHousings);
  std::vector<double> allxOffset(s_numHousings);
  
  for (int i = 0 ; i < s_numHousings ; i++ ) {
    allrOuter[i] = housings[i].m_rOuter;
    allxOffset[i] = housings[i].m_xShift;
    //+++ or just do the max in here, since I'm already going through loops
  }
  housingHardLimit = *std::max_element(allrOuter.begin(), allrOuter.end()) + 
                     *std::max_element(allxOffset.begin(), allxOffset.end());
  
  // calculate the plate scale on the focal plane
  plateScaleArcmin = genTelescope.m_focalLength / s_radianToArcmin;
  plateScaleArcsec = genTelescope.m_focalLength / s_radianToArcsec;
  
  // z=0 means focal plane.  the initial z should be same as the aperture
  // The variable ztelescopeaperture is the z-coordinate of the plane of 1st 
  // interaction between the photon and telescope. If there are external 
  // objects above the main telescope components (e.g. thermal shield, top), 
  // then this z-coordinate is that of the plane of the “top” external objects 
  // (which are currently treated as 2-D, all in the same plane), otherwise it 
  // is equal to the highest z-coordinate in xrtzlevels
  if (topExtObjects.m_numExtObjectParts > 0) {
    aperture.m_ztelescopeaperture = topExtObjects.m_zCoord;
  } else {
    aperture.m_ztelescopeaperture = xrtzlevels.at(0);
  }
  
  // If the input "source" parameter in the .par file was "diagnosticmode" then 
  // calculate the actual position of input photons on the telescope aperture.
  if (param.m_sourceEnum == DIAGNOSTICMODE) {
    
    // Get the parameters from the input .par file: the parameter diagpars is 
    // a string containing a mixed list of integers and double-precision 
    // types which must be extracted:
    // +++ 20150914 KLR This would fit better in getPar, with the others
    std::vector<double> diagnosticParsArray;
    listStringsToDoubles(param.m_diagpars, diagnosticParsArray);
    entrysegment = static_cast<long>(diagnosticParsArray[0]);
    long entrysector = static_cast<long>(diagnosticParsArray[1]);
    long entryshell = static_cast<long>(diagnosticParsArray[2]);
    double shellgapfraction = diagnosticParsArray[3];
    double entryAngleFraction = diagnosticParsArray[4];
    // The above parameters specify a unqiue position on the telescope aperture 
    // at which photons will be injected. The position is located in the 
    // sector entrysector, in the segment entrysegment, between the shells 
    // entryshell and entryshell+1 (where the first, innermost shell in the 
    // telescope has entryshell=1), at a radial distance shellgapfraction 
    // times the intershell gap, from the innermost shell. The angular 
    // position in the sector is a fraction entryanglefraction of the angular 
    // width of the sector, anticlockwise from the sector boundary defined 
    // by STARTANGLE in the TDF.
    
    // if the entrysegment is negative, interpret the 4th and 5th numbers as the
    // initial x and y
    if (entrysegment < 0) {
      initialPhotonPos.m_x = shellgapfraction;
      initialPhotonPos.m_y = entryAngleFraction;
      cartesianToPolar(initialPhotonPos, entryRad, entryAngle);
      // the code needs these to be in radians.  It's set to degrees before 
      // writing to history file
    } else {

      // Identify the two XRT objects in adjacent shells that bracket the 
      // specified photon entry position – we will then use the radii of the 
      // tops of those foils to calculate the actual photon entry position. 
      // However, we do need to specify whether the telescope has a 
      // pre-collimator or not because it affects which objects are first 
      // encountered by photons
      int entryset = ( param.m_pcolExists ? 2 : 0);

      if (entryshell <= 1) {
        AH_THROW_RUNTIME("Diagnostic mode cannot inject photons between outermost shell and outer housing wall: Try again with a different shell number");
      }

      // The TDF numbers shells from the outermost to the innermost, so 
      // entryobjectidone and entryobjectidtwo are the innermost and outermost 
      // shells respectively of the pair bracketing the entry position
      // Remember that entryshell-1 is the index for shell number entryshell, 
      // so entryshell is the index for shell number entryshell+1
      long entryobjectidone = xrtRegionToObjectID[entryset][entrysegment-1][entrysector-1][entryshell-1];
      long entryobjectidtwo = xrtRegionToObjectID[entryset][entrysegment-1][entrysector-1][entryshell];

      // Now, for the pair of foils, get the top outer radius of the innermost 
      // foil (i.e. radial distance from the telescope axis to the backside of 
      // the foil), and the top outer radius of the outermost foil. Note that 
      // this interval in radial length of the "inter-shell gap" includes the 
      // thickness of the outermost foil, enabling diagnostic runs that inject 
      // photons into the top edge of a foil (as opposed to the front or 
      // back faces):
      double rad_objectone = XRTObjects[entryobjectidone].m_geoParams[10];
      double rad_objecttwo = XRTObjects[entryobjectidtwo].m_geoParams[10];
      entryAngle = std::abs(XRTObjects[entryobjectidone].m_sideWalls[0]) + 
                          (entryAngleFraction * 
                          (std::abs(XRTObjects[entryobjectidone].m_sideWalls[1]) - 
                            std::abs(XRTObjects[entryobjectidone].m_sideWalls[0])) );
      entryRad = rad_objectone + ( shellgapfraction * (rad_objecttwo - rad_objectone) );

      // Set the photon initial coordinates
      initialPhotonPos.m_x = entryRad * std::cos(entryAngle);
      initialPhotonPos.m_y = entryRad * std::sin(entryAngle);
      
    } // end-if checking if user input initial x and y (entrysegment<0)
    
    // Set the z-coordinate to that of the aperture. It is simply equal to the 
  // largest z-coordinate of any xrt object (usually this will correspond to 
  // the tops of the pre-collimator foils).
    initialPhotonPos.m_z = aperture.m_ztelescopeaperture;
    
    // The initial direction of these photons was already calculated earlier 
  // for all modes that do not involve getting input photons from a file (a 
  // direction was generated for each of the off-axis and azimuthal angles 
  // specified in the input .par file).
    
  } // end if-block "source == diagnosticmode"

  
  // -------------------------------------
  // set up reflectivity and transmission data
  // -------------------------------------
  
  // If the flag preinterpolategrids was set earlier to True then 
  // pre-interoplate and re-map the reflectivity, transmission, and optical 
  // depth grids onto the energy grid specified in the input parameter file. 
  // (The case for which we don’t do this and interpolate inside the main 
  // ray-tracing loop is if the photons are from a file and no unique energy 
  // grid is specified).

  // frontColumnNames must be same size as numGroups
  // frontColumnNames must be in order of group
  
  readReflectTrans(param, photons, numGroups, frontColumnNames, reflectTransGrids, xrtShellToGroupNumber, doTransmission, isMultiLayerSurface);
  
  // If readReflectTrans didn't abort, numBackAngles=numPcolAngles is satisfied 
  // (kept independent for the possibility of generalizing later). The rough 
  // surface materials will be unified and will be a function of either of 
  // these angle grids
  reflectTransGrids.m_numRoughAngles = reflectTransGrids.m_numBackAngles;
  
  
  // now that we've read the reflectivity file (with the mass absorption 
  // extension), make sure that the max mass absorption index we found in the 
  // TDF AZIMUTHALSTRUT extension is valid.
  if (doTransmission || param.m_doExternalObjects) {
    if ( (topExtObjects.m_maxMabsIndex > reflectTransGrids.m_numMaterials) || 
         (bottomExtObjects.m_maxMabsIndex > reflectTransGrids.m_numMaterials) ){
      // +++ testing both top and bottom is redundant, since they have the same value
      AH_THROW_RUNTIME("The values of MABSINDEX in the AZIMUTHALSTRUCT extension of the TDF exceed the number of materials in the reflectivity file, given by the NMATERIA keyword in the front reflection extension");
    }
  }
        
  // In the case that the input .par file specified that the energy grid 
  // should be obtained from the reflectivity file, the final energy grid 
  // for ray-tracing has not yet been set up because the reflectivity file 
  // had not been opened earlier. The energy range for which to extract 
  // the energy grid was specified in the input .par file (searchenergymin 
  // to searchenergymax).
  // In the cases that the energy grid is not extracted from the reflectivity 
  // file, we have to check that if the front-side mirrors have a multilayer 
  // coating (multilayersurf = "True"), every single energy in the energy 
  // grid should have an exact match in the reflectivity file energy grid 
  // because it is not possible to interpolate multilayer reflectivity 
  // sufficiently accurately, due to large-amplitude peaks and valleys in 
  // the reflectivity functions.
  // Thus, if multilayersurf = "True" and energiesfromreflectivity == "False", 
  // loop over all input energies to find matches in the reflectivity file 
  // grid, otherwise if multilayersurf= "True", simply transfer the portion of 
  // FrontReflEnergies that is in range, to Photonenergies. In the latter 
  // case, the routine remapReflectTrans will take care of transferring the 
  // actual reflectivity, transmission, and optical depth grids.

  const double maxRelDiff = 1e-3;     // max relative difference, when comparing energies later
  if (isMultiLayerSurface && !param.m_energiesfromreflectivity) {
    
    // make sure all the input photon energies are in the reflectivity file
    for (long i = 0 ; i < photons.m_numPhotonEnergies ; ++i) {
      for (uint j = 0 ; j < reflectTransGrids.m_frontReflEnergies.size() ; ++j) {
        if (doublesAreEqual(photons.m_photonEnergies[i], reflectTransGrids.m_frontReflEnergies[j], maxRelDiff)) {
          foundEnergyMatch = true;
          // we found a match in the refl grid; can go to the next photon energy
          break;
        } else {
          foundEnergyMatch = false;
        }
      }
    }
    if (!foundEnergyMatch) {
      AH_THROW_RUNTIME("For multilayer reflectivity all ray-trace energies must exactly match one of the energies in the reflectivity file grid. At least one of your ray-trace energies does not match. Please re-define your ray-trace energy grid and start over. (For example, you could make a custom reflectivity file using xrtreftable based on your current energy grid, or you could specify '-1 minEnergy maxEnergy' in the .par file energy grid line to select a subset of the current reflectivity file grid.");
    }
          
          
  // If energies from the reflectivity file were requested, regardless of multilayer="True"
  } else if (param.m_energiesfromreflectivity) {
    
    // +++ 20140701 I'm changing this slightly
    // +++ test that floating point comparison works as expected here
    for (long i = 0 ; i < reflectTransGrids.m_numFrontEnergies ; ++i) {
      if ( (reflectTransGrids.m_frontReflEnergies[i] >= param.m_searchenergymin) && 
           (reflectTransGrids.m_frontReflEnergies[i] <= param.m_searchenergymax) ) {
        photons.m_photonEnergies.push_back(reflectTransGrids.m_frontReflEnergies[i]);
      }
    }

    // Over-ride previously tentative minimum and maximum energies for the case of energy grid read from the reflectivity file
    photons.m_minPhotonEnergy = getMinDouble(photons.m_photonEnergies);
    photons.m_maxPhotonEnergy = getMaxDouble(photons.m_photonEnergies);
    photons.m_numPhotonEnergies = photons.m_photonEnergies.size();

  AH_DEBUG << "photons.m_numPhotonEnergies = " << photons.m_numPhotonEnergies << std::endl;
  
  } // End of if-block that checks for multilayer=True and energiesfromreflectivity= "False"  
  
  // Set up the indexing array for thick transmission components. The index 
  // array points to the optical depth array roughtaupermm(energy bin, group#). 
  // The group# for a given mirror foil is as defined in the TDF SURFACE 
  // extension, and group 0 is reserved for the pre-collimator. 
  // Currently the transmission is the same for front-side to back-side and 
  // vice-versa but the flexibility is built in for future possibilities.
  if (doTransmission || param.m_doExternalObjects) {
    int currGroup = 0;
    for (int iObj = firstMirrorIndex ; iObj < lastMirrorIndex ; ++iObj) {
      currGroup = xrtShellToGroupNumber.at(XRTObjects.at(iObj).m_shellNumber-1);
      // back to front
      XRTObjects[iObj].m_tauPermmIndexBack  = currGroup;
      // front to back
      XRTObjects[iObj].m_tauPermmIndexFront = currGroup;
    } // end-for through mirror foils
    // +++ so this is only needed for mirror foils?  the pcol doesn't use roughtaupermm?  or we just know that it's roughtaupermm[energy][0]?
  }
  
  
  AH_DEBUG << "topExtObjects.m_numExtObjectParts = " << topExtObjects.m_numExtObjectParts << std::endl;
  
  // +++ error checking: that refl file has numnber of mass abs columns to match mabsCoeffIndex 
  // (when I used wrong refl file, I got a seg fault)
  
  // Calculate transmission probabilities as a function of energy for the 
  // external objects such as thermal shield etc. The energy grid used for 
  // this is the same as the one specified for front-side reflectivity.
  // Transmission probability = thickness in mm x 
  //                            0.1 (to convert to cm) x 
  //                            density (g/cm^3) x 
  //                            mass-absorption coefficient (mabscoef), which is from ReadReflectTrans()
  
  reflectTransGrids.m_numTopExtObjectParts = topExtObjects.m_numExtObjectParts;
  if (topExtObjects.m_numExtObjectParts > 0) {
    
    AH_DEBUG << "reflectTransGrids.m_numFrontEnergies = " << reflectTransGrids.m_numFrontEnergies << std::endl;
    
    reflectTransGrids.m_topExtObjectsTransProb.resize(reflectTransGrids.m_numFrontEnergies);
    for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
      reflectTransGrids.m_topExtObjectsTransProb.at(iEnergy).resize(topExtObjects.m_numExtObjectParts);
    }
  
    for (int iObject = 0 ; iObject < topExtObjects.m_numExtObjectParts ; ++iObject) {
      AH_DEBUG << "iObject = " << iObject << std::endl;
      // Only calculate the transmission probability if the mass-absorption 
      // coefficient index is positive; a negative value indicates that the 
      // object is to be treated as an obstruction (with transmission 
      // probability=0.0). Since all of the transmission probabilities are 
      // initialized to zero we don’t need to do any "else" for negative 
      // mass-absorption index.
      int mabsCoeffIndex = topExtObjects.m_mabsIndex.at(iObject);
      AH_DEBUG << "mabsCoeffIndex = " << mabsCoeffIndex << std::endl;
      if (mabsCoeffIndex > 0) {
        for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
          AH_DEBUG << "iEnergy = " << iEnergy << std::endl;
          reflectTransGrids.m_topExtObjectsTransProb.at(iEnergy).at(iObject) = 
                  exp( -1.0 * topExtObjects.m_thickness.at(iObject) * s_mmTocm * 
                       topExtObjects.m_density.at(iObject) * 
                       reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) );
          
          AH_DEBUG << "topExtObjects.m_thickness.at(iObject) = " << topExtObjects.m_thickness.at(iObject) << std::endl;
          AH_DEBUG << "topExtObjects.m_density.at(iObject) = " << topExtObjects.m_density.at(iObject) << std::endl;
          AH_DEBUG << "reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) = " << reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) << std::endl;
          AH_DEBUG << "topExtObjectsTransProb.at(iEnergy).at(iObject) = " << reflectTransGrids.m_topExtObjectsTransProb.at(iEnergy).at(iObject) << std::endl;
          
          //+++ save optical depth (this value, before the exp()) somewhere, for diagnostic
          
        }
      }
    }
    
  } // end-of doing top part transmission probability
  
  AH_DEBUG << "bottomExtObjects.m_numExtObjectParts = " << bottomExtObjects.m_numExtObjectParts << std::endl;
  
  // Now do bottom objects
  reflectTransGrids.m_numBottomExtObjectParts = bottomExtObjects.m_numExtObjectParts;
  if (bottomExtObjects.m_numExtObjectParts > 0) {
    
    reflectTransGrids.m_bottomExtObjectsTransProb.resize(reflectTransGrids.m_numFrontEnergies);
    for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
      reflectTransGrids.m_bottomExtObjectsTransProb.at(iEnergy).resize(bottomExtObjects.m_numExtObjectParts);
    }

    for (int iObject = 0 ; iObject < bottomExtObjects.m_numExtObjectParts ; ++iObject) {
      // Only calculate the transmission probability if the mass-absorption 
      // coefficient index is positive; a negative value indicates that the 
      // object is to be treated as an obstruction (with transmission 
      // probability=0.0). Since all of the transmission probabilities are 
      // initialized to zero we don’t need to do any "else" for negative 
      // mass-absorption index.
      int mabsCoeffIndex = bottomExtObjects.m_mabsIndex.at(iObject);
      AH_DEBUG << "mabsCoeffIndex = " << mabsCoeffIndex << std::endl;
      
      if (mabsCoeffIndex > 0) {
        for (int iEnergy = 0 ; iEnergy < reflectTransGrids.m_numFrontEnergies ; ++iEnergy) {
          AH_DEBUG << "massAbsCoeff.at(iEnergy).size() = " << reflectTransGrids.m_massAbsCoeff.at(iEnergy).size() << std::endl;
          AH_DEBUG << "iObject = " << iObject << std::endl;
          AH_DEBUG << "bottomExtObjectsTransProb.at(iEnergy).size() = " << reflectTransGrids.m_bottomExtObjectsTransProb.at(iEnergy).size() << std::endl;
          AH_DEBUG << "bottomExtObjects.m_density.at(iObject) = " << bottomExtObjects.m_density.at(iObject) << std::endl;
          AH_DEBUG << "reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) = " << reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) << std::endl;
          reflectTransGrids.m_bottomExtObjectsTransProb.at(iEnergy).at(iObject) = 
                  exp( -1.0 * bottomExtObjects.m_thickness.at(iObject) * s_mmTocm * 
                       bottomExtObjects.m_density.at(iObject) * 
                       reflectTransGrids.m_massAbsCoeff.at(iEnergy).at(mabsCoeffIndex-1) );
          AH_DEBUG << "bottomExtObjectsTransProb.at(iEnergy).at(iObject) = " << reflectTransGrids.m_bottomExtObjectsTransProb.at(iEnergy).at(iObject) << std::endl;
        }
      }
    }
    
  } // end-of doing bottom part transmission probability
  
  
  
  // The pre-interpolation and re-mapping results in a considerable reduction 
  // in run time of the main ray-tracing loop because all of the energy 
  // interpolation is done outside of the loop.
  // Note that the reflectivity for the front-side of mirrors, the back-sides, 
  // and the pre-collimator can all have different energy grids to begin with, 
  // but they will all be mapped onto the same energy grid.
  
  // initialize numRoughSurfaces to 1: there is at least a back side to mirror
  numRoughSurfaces = 1;
  
  if (param.m_pcolExists) { 
    numRoughSurfaces++;
  }
  
  AH_DEBUG << std::endl;
  
  
  if (preinterpolateGrids) {
    remapReflectTrans(reflectTransGrids, photons.m_numPhotonEnergies, photons.m_photonEnergies, numGroups, param.m_pcolExists, doTransmission, remappedGrids);
  }
  
  AH_DEBUG << std::endl;
  // -------------------------------------
  // set up EEF and EA arrays
  // -------------------------------------
  // only if input photons are not from a file and if the filename selected 
  // for EEF and/or EA was not "none"
   
  if (!photons.m_photonsFromFile) {
    // If there are more than 10 energies, the EEF or PSF will be averaged over 
    // the entire energy grid
    if (photons.m_numPhotonEnergies > maxNumEEFEnergies) { 
      psf.m_numEEFens = 1;
      psf.m_summedEEFPSF = true;
    } else {
      psf.m_numEEFens = photons.m_numPhotonEnergies;
      psf.m_summedEEFPSF = false;
    }
    
    // +++ this is done in doWork, before and at the start of the main energy loop.  does it need to be done here?
    // set up the PSF image
    psf.m_psfImage.resize(psf.m_numpsfxvals);
    for (int iX = 0 ; iX < psf.m_numpsfxvals ; ++iX) {
      psf.m_psfImage[iX].resize(psf.m_numpsfyvals);
    }

    // set up the EEF
    // EEF value versus radial distance is required for each off-axis angle, each roll angle, and each energy.
    psf.m_eef.resize(psf.m_numradeef);
    for (int iRad = 0 ; iRad < psf.m_numradeef ; ++iRad) {
      psf.m_eef[iRad].resize(photons.m_nrollin);
      for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
        psf.m_eef[iRad][iRoll].resize(photons.m_nthetain);
        for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
          psf.m_eef[iRad][iRoll][iTheta].resize(psf.m_numEEFens);
        }
      }
    }
    
    // The following is a loop variable used later which in this case is equal 
    // to numPhotonEnergies if photons are not from a file but 1 if they are
    // Also, if the photons are not read from a file, we set the number of 
    // photons, nphotons, equal to numphoton, from the input .par file 
    // (otherwise it is set elsewhere to be equal to the number of rows in the 
    // input photon file).
    // we're currently in an if-block photons are not from a file
    // if it was from a file, we read in the number of rows into 
    // photons.m_nphotons inside initializePhotons(), where we also set m_nens
    photons.m_nens = photons.m_numPhotonEnergies;
    photons.m_nphotons = param.m_numphoton;
    
    // Set up effective area (EA) array if "outeafile" in the input parameter file 
    // is not "none" OR if photons not read from a file. An effective area vs. 
    // energy point is required for each off-axis angle, each azimuthal angle, 
    // and each energy. An additional array is set up for the total number of 
    // photons per energy that impact the results plane (usually the focal 
    // plane), from which the effective area is calculated. (The photon numbers 
    // are counted separately for each off-axis angle, each azimuthal angle.)
    ea.m_ea.resize(photons.m_nrollin);
    ea.m_resultsplanephotons.resize(photons.m_nrollin);
    ea.m_effareaphotons.resize(photons.m_nrollin);
    for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
      ea.m_ea[iRoll].resize(photons.m_nthetain);
      ea.m_resultsplanephotons[iRoll].resize(photons.m_nthetain);
      ea.m_effareaphotons[iRoll].resize(photons.m_nthetain);
      for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
        ea.m_ea[iRoll][iTheta].resize(photons.m_numPhotonEnergies);
        ea.m_resultsplanephotons[iRoll][iTheta].resize(photons.m_nens);
        ea.m_effareaphotons[iRoll][iTheta].resize(photons.m_nens);
      }
    }
    
  } else {
    
    ea.m_resultsplanephotons.resize(photons.m_nrollin);
    for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
      ea.m_resultsplanephotons[iRoll].resize(photons.m_nthetain);
      for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
        ea.m_resultsplanephotons[iRoll][iTheta].resize(photons.m_nens);
      }
    }
    
  }// end-if photons not from a file
  
  // as of now, the numbers of photon/energy/theta/roll should be stored
  // now store the total number of photons we will be raytracing
  photons.m_totalNumPhotons = photons.m_nphotons * photons.m_nens * photons.m_nthetain * photons.m_nrollin;
  
  AH_DEBUG << "photons.m_nens = " << photons.m_nens << std::endl;
  AH_DEBUG << "photons.m_nphotons = " << photons.m_nphotons << std::endl;
  AH_DEBUG << "Total number of photons to be raytraced: " << photons.m_totalNumPhotons << std::endl;
  
  // -------------------------------------
  // set up telescope aperture and geometric area
  // -------------------------------------
  
  // +++ put all this in a sub-function
  
  // Either a partial annulus or a rectangle is selected for the telescope aperture, but the selection is rather complicated because a rectangle that is specified has to lie completely inside the full annulus of the telescope aperture. If it does not, the annulus is selected regardless of what the user specified (the code does not handle a partial rectangle).
  // The variable apertureannulus=True means annulus was finally selected, and apertureannulus=False means that a rectangular aperture was finally selected.
  // Action:
  // 1. If either the maximum radius in the input parameter file is negative,
  // then tentatively turn off annulus (tentatively use rectangle).
  // 2. If either the height or width of the rectangle in the input parameter file is negative then turn off the rectangle (use annulus).
  // 3. However, if any of the corners of the rectangle lie outside the radii of the aperture then use the annulus with the radii specified in the input parameter file. 
  // 4. If both annulus and rectangle are turned off, use full annulus with the full aperture radii of the telescope (and the full angle range).
  // 5. If both annulus and rectangle are turned on, use the annulus, with the radii and angles specified in the input parameter file.
  // 6. If annulus is finally selected then check if the minimum radius is negative. If it is, it tells the code to set the lower aperture radius equal to the inner housing radius (which is obtained from the TDF). If the upper radius of the annulus is greater than the outer housing radius, then the upper radius of the annulus is forced to be equal to the radius of the outer housing.

  // The logic is then as follows:
  // if (rmax)>0 {choose annulus regardless of rectangle pars}
  // 	apertureannulus=True
  // else if {rmax -ve}
  //   if (height & width)>0 {choose rectangle but need to check corners}
  // 	if (all corners inside) then
  // 		apertureannulus=False
  // 	else
  // 		apertureannulus=True
  // 	endif
  //   else {either or height or width -ve -select annulus}
  // 	apertureannulus=True
  // end if
  // Then if apertureannulus=True test if: 
    // (a)	rmin<0 – if so, set lower radius of aperture=inner housing radius;
    // (b)	rmax>outer housing radius – if so, set upper radius of aperture=outer housing radius.

  // Note that if the rectangle is selected then photons are allowed to enter *only* the rectangluar area, and the geometric area is then simply the area of the rectangle. Otherwise, the geometric area is equal to the area of the partial annulus.

  
  // Extract the four numbers from the string input parameter "annulus" and store them as follows
  std::vector<double> annulusVals;
  listStringsToDoubles(param.m_annulus, annulusVals);
  double apertureinput_rmin = annulusVals[0];
  double apertureinput_rmax = annulusVals[1];
  double aperture_minangle = annulusVals[2];
  double aperture_maxangle = annulusVals[3];
  // +++ add a check that there are indeed only 4 doubles in this list
  
  aperture.m_minAngleDeg = aperture_minangle;
  aperture.m_maxAngleDeg = aperture_maxangle;
  aperture.m_minAngleRad = aperture_minangle * s_degreesToRadian;
  aperture.m_maxAngleRad = aperture_maxangle * s_degreesToRadian;
  // Following is the central angle enclosed by the annulus radial boundaries
  aperture.m_deltaAngleRad = aperture.m_maxAngleRad - aperture.m_minAngleRad;

  
  // Extract the four numbers from the string input parameter "rectangle" and store them as follows
  std::vector<double> rectangleVals;
  listStringsToDoubles(param.m_rectangle, rectangleVals);
  double aperture_rectangle_xc = rectangleVals[0];
  double aperture_rectangle_yc = rectangleVals[1];
  double apertureinput_rectangle_xwid = rectangleVals[2];
  double apertureinput_rectangle_ywid = rectangleVals[3];
  // +++ add a check that there are indeed only 4 doubles in this list
  
  // if both apertureinput_rmin and apertureinput_rmax are >0 choose annulus
  // regardless of rectangle parameters (annulus trumps rectangle)
  
  // if both apertureinput_rmin and apertureinput_rmax are >0 choose annulus
  // regardless of rectangle parameters (annulus trumps rectangle)
  const int numCorners = 4;
  std::vector<double> xcorner(numCorners);
  std::vector<double> ycorner(numCorners);
  std::vector<double> rcorner(numCorners);
  std::vector<double> phicorner(numCorners);
  double hxwid = 0.0;
  double hywid = 0.0;
  
  // we need to allow the photons to enter the central "hole" of the telescope, 
  // so only check rmax
  if (apertureinput_rmax > 0.0) {
    aperture.m_isAnnulus = true;
  } else {
    // check if both height and width of rectangle are positive.
    // If so, check that all the corners lie inside the full telescope
    // aperture. If not, choose annulus again
    if ( (apertureinput_rectangle_xwid > 0.0) && 
         (apertureinput_rectangle_ywid > 0.0) ) {
      hxwid = 0.5 * apertureinput_rectangle_xwid;
      hywid = 0.5 * apertureinput_rectangle_ywid;
      xcorner[0] = aperture_rectangle_xc + hxwid;
      xcorner[1] = aperture_rectangle_xc + hxwid;
      xcorner[2] = aperture_rectangle_xc - hxwid;
      xcorner[3] = aperture_rectangle_xc - hxwid;
      ycorner[0] = aperture_rectangle_yc + hywid;
      ycorner[1] = aperture_rectangle_yc + hywid;
      ycorner[2] = aperture_rectangle_yc - hywid;
      ycorner[3] = aperture_rectangle_yc - hywid;
      int cornersinside = 0; // counts corners inside telescope
      for (int i = 0 ; i < numCorners ; ++i) {
        rcorner[i] = std::sqrt((xcorner[i]*xcorner[i]) + (ycorner[i]*ycorner[i]));
        if (ycorner[i] > 0.0) {
          phicorner[i] = s_radianToDegrees * std::acos(xcorner[i] / rcorner[i]);
        } else {
          phicorner[i] = 360.0 - (s_radianToDegrees * std::acos(xcorner[i]/rcorner[i]));
        }
        if ( (rcorner[i] >= genTelescope.m_innerhousingradius) && 
             (rcorner[i] <= genTelescope.m_outerhousingradius) && 
             (phicorner[i] >= aperture_minangle) && 
             (phicorner[i] <= aperture_maxangle) ) {
            cornersinside++;
        }
      } // end for-loop
      if (cornersinside == numCorners) {
        aperture.m_isAnnulus = false;
      } else {
        aperture.m_isAnnulus = true;
      }
    } else {
      aperture.m_isAnnulus = true;
    }
  }
  
  // If annular aperture was selected, check that the inner and outer radii lie 
  // within the actual telescope aperture. If not, force them to do so.
  // actual minimum and maximum radius:
  if (aperture.m_isAnnulus) {
    
    // we tell the code to make the inner aperture radius = housing inner 
    // radius by specifying a negative number for the inner aperture radius
    if (apertureinput_rmin < 0.0) {
      aperture.m_rMin = genTelescope.m_innerhousingradius;
    } else {
      aperture.m_rMin = apertureinput_rmin;
    }
    if ( (apertureinput_rmax > genTelescope.m_outerhousingradius) || 
         (apertureinput_rmax <= 0.0) ) {
      aperture.m_rMax = genTelescope.m_outerhousingradius;
    } else {
      aperture.m_rMax = apertureinput_rmax;
    }
    
    // check that user entered an rmax that is lrgr than rmin
    if (aperture.m_rMax <= aperture.m_rMin) {
      AH_THROW_RUNTIME("Cannot have outer radius less than inner radius.");
    }
    
    // squares of radii, and geometric area
    aperture.m_rMinSq = aperture.m_rMin * aperture.m_rMin;
    aperture.m_rMaxSq = aperture.m_rMax * aperture.m_rMax;
    aperture.m_geometricAreaSqmm = s_pi * 
                    (aperture.m_rMaxSq - aperture.m_rMinSq) * 
                    ((aperture_maxangle-aperture_minangle) / 360.0);
    
    
    
//  std::cout << std::setprecision(15) << "s_pi = " << s_pi << std::endl;
  AH_DEBUG << "s_pi = " << s_pi << std::endl;
  AH_DEBUG << "apertureinput_rmin = " << apertureinput_rmin << std::endl;
  AH_DEBUG << "innerhousingradius = " << genTelescope.m_innerhousingradius << std::endl;
  AH_DEBUG << "apertureinput_rmax = " << apertureinput_rmax << std::endl;
  AH_DEBUG << "outerhousingradius = " << genTelescope.m_outerhousingradius << std::endl;
  AH_DEBUG << "m_rMax = " << aperture.m_rMax << std::endl;
  AH_DEBUG << "m_rMaxSq = " << aperture.m_rMaxSq << std::endl;
  AH_DEBUG << "m_rMin = " << aperture.m_rMin << std::endl;
  AH_DEBUG << "m_rMinSq = " << aperture.m_rMinSq << std::endl;
  AH_DEBUG << "m_rMaxSq -  m_rMinSq= " << aperture.m_rMaxSq - aperture.m_rMinSq << std::endl;
  AH_DEBUG << "aperture_maxangle = " << aperture_maxangle << std::endl;
  AH_DEBUG << "aperture_minangle = " << aperture_minangle << std::endl;
  AH_DEBUG << "aperture_maxangle-aperture_minangle = " << aperture_maxangle-aperture_minangle << std::endl;
  AH_DEBUG << "(aperture_maxangle-aperture_minangle) / 360.0 = " << (aperture_maxangle-aperture_minangle) / 360.0 << std::endl;
  AH_DEBUG << "(aperture.m_rMaxSq - aperture.m_rMinSq) * (aperture_maxangle-aperture_minangle) / 360.0 = " << (aperture.m_rMaxSq - aperture.m_rMinSq) * (aperture_maxangle-aperture_minangle) / 360.0 << std::endl;
//  std::cout << std::setprecision(8) << "s_pi * (aperture.m_rMaxSq - aperture.m_rMinSq) * (aperture_maxangle-aperture_minangle) / 360.0 = " << s_pi * (aperture.m_rMaxSq - aperture.m_rMinSq) * (aperture_maxangle-aperture_minangle) / 360.0 << std::endl;
//  std::cout << std::setprecision(8) << "geometricAreaSqmm = " << aperture.m_geometricAreaSqmm << std::endl;
  
  
  } else {
    // the aperture is not an annulus
    aperture.m_xWidth = apertureinput_rectangle_xwid;
    aperture.m_yWidth = apertureinput_rectangle_ywid;
    aperture.m_xMin = xcorner[2];
    aperture.m_yMin = ycorner[2];
    aperture.m_geometricAreaSqmm = aperture.m_xWidth * aperture.m_yWidth;
  }
  
  AH_DEBUG << "geometricAreaSqmm = " << aperture.m_geometricAreaSqmm << std::endl;
  
  aperture.m_geometricAreaSqcm = aperture.m_geometricAreaSqmm * 0.01;
  aperture.m_fullGeometricAreaSqmm = s_pi * 
          ( (genTelescope.m_outerhousingradius * genTelescope.m_outerhousingradius) - 
            (genTelescope.m_innerhousingradius * genTelescope.m_innerhousingradius) );
  
  // convert the number of photons that impact the results plane for a given 
  // energy into an effective area
  aperture.m_effAreaFactor = aperture.m_geometricAreaSqcm / photons.m_nphotons;

  AH_DEBUG << "photons.m_nphotons = " << photons.m_nphotons << std::endl;
  AH_DEBUG << "m_effAreaFactor = " << aperture.m_effAreaFactor << std::endl;
  
  
  // -------------------------------------
  // Create photon history file if requested
  // -------------------------------------
  
  // Open a new FITS file for writing results to, only if a photon history 
  // file was requested.  
  if ( param.m_phistory ) {
    doHistoryFile = true;
    setupHistoryFile(param, historyColumns);
    // The header will be written by writehistoryfileheader() after a complete 
    // ray-trace 
    // The results will be written row-by-row to a buffer later, in doWork(), 
    // after each call to raytraceonephoton()
  } else {
    doHistoryFile = false;
  }
  
  // -------------------------------------
  // Create PSF and EA files if requested
  // -------------------------------------
  
  if (!photons.m_photonsFromFile) {
    if ( isInputFileGiven(param.m_outpsffile) ) {
      setupPSFFile(param);
    }
    if ( isInputFileGiven(param.m_outeafile) ) {
      setupEAFile(param);
    }
  }
  
  
}   // end initialize()


// ----------------------------------------------------------------------------


void doWork(Param & param, 
            Photons & photons, 
            PSF_EEF & psf, 
            EA & ea, 
            const ReflectTransGrids & reflectTransGrids, 
            RemappedReflectTransGrids & remappedGrids, 
            const Scattering & scat, 
            long numFrontAngles, 
            long numRoughAngles, 
            long numPcolAngles, 
            long numGroups, 
            int numRoughSurfaces, 
            bool preinterpolateGrids, 
            long numXRTObjects, 
            const std::vector<XRTObject> & XRTObjects, 
            const std::vector<HousingGeometry> & housings, 
            double housingHardLimit, 
            const std::vector<long> & zmaxsortxrtobjectindexAll, 
            long numxrtzlevels, 
            const std::vector<double> & xrtzlevels, 
            const std::vector<int> & obstructionIntervalLowIndex, 
            const std::vector<int> & obstructionIntervalHighIndex, 
            bool doTransmission, 
            bool has3DObjects, 
            double & entryRad, 
            double & entryAngle,
            long & entrysegment, 
            bool doHistoryFile, 
            double plateScaleArcmin, 
            double plateScaleArcsec, 
            CartesianCoord & initialPhotonPos, 
            std::map<std::string, int> & historyColumns, 
            const Aperture & aperture, 
            const Transforms & transforms, 
            const SectorsShells & sectorsShells, 
            const ExternalObjectsStructure & topExtObjects, 
            const ExternalObjectsStructure & bottomExtObjects, 
            const GenTelescope & genTelescope) {
  
      AH_DEBUG << "start of doWork()" << std::endl;
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  fitsfile * photon_fp = 0;             // photon data file
  
  int m = 0;                            // energy index if preinterpolategrids
  
  // input* are the values the user specified in the input parameters 'offaxis'
  // and 'roll'
  // We want to write these input* values (in arcmin and deg) to the OFFAXIS
  // and AZIMUTH keywords in the output PSF and EA files
  double inputOffAxisThetaRad     = 0.0;
  double inputOffAxisThetaArcmin  = 0.0;
  double inputAzimuthDeg          = 0.0;
  
  // initial* is the actual initial theta, which in most cases is the same as
  // input*.  For flatcircle and betamodel, though, the initial* is different
  // than the input*.  We want to write these initial* to the "initialtheta"
  // and "initialazimdir" columns in the output history file.
  double initialThetaArcmin       = 0.0;
  double initialAzimuthDeg        = 0.0;
  
  // +++ this is never used.  Strictly speaking the thick transmission 
  //     probabilities should be divided by cosinputoffaxistheta but for the 
  //     moment we're not doing that (the value is close to 1 even for off-axis 
  //     angles as large as 1 degree). Let's leave the code as is in case we 
  //     decide to use it.
  // The cosine of the off-axis angle is copied to cosinputoffaxistheta here 
  // because elsewhere (e.g. for input photons from a file) it is obtained 
  // from the corresponding value in the input file (it will be used to 
  // calculate transmission in the external xrt components such as the 
  // thermal shield):
  double cosInputOffaxisTheta           = 0.0;
  double oneOverCosInputOffaxisTheta    = 0.0;
  
  // focal length * tan(theta)
  double fltantheta               = 0.0;
  
  double totaleefenergy           = 0.0;
  long totalpsfphotons            = 0;
  
  // converting the focal plane from mm to arcsec
  double fpmmToArcsec = s_radianToArcsec / genTelescope.m_focalLength;
  
  StatResults stats;  // counter for results after tracing each photon
  
  // following will hold sub-arrays of reflectivity, transmission, optical 
  // depths, and scattering for single energies
  // front-side mirror reflectivity, #angle bins x #groups
  std::vector< std::vector<double> > frefeslice(numFrontAngles);
  // front-side mirror thin-film transmission, #angle bins x #groups
  std::vector< std::vector<double> > ftranseslice(numFrontAngles);
  for (int i = 0 ; i < numFrontAngles ; ++i) {
    frefeslice[i].resize(numGroups);
    ftranseslice[i].resize(numGroups);
  }
  // reflection from rough surfaces, #angle bins x #of unique rough surfaces
  std::vector< std::vector<double> > roughrefeslice(numRoughAngles);
  for (int i = 0 ; i < numRoughAngles ; ++i) {
    roughrefeslice[i].resize(numRoughSurfaces);
  }
  // optical depth per mm of thick XRT components
  std::vector<double> fronttaupermmeslice(numGroups+1);
  // optical depth per mm of thick XRT components
  std::vector<double> roughtaupermmeslice(numGroups+1);
  
  // single-energy slices of transmission for each of the external objects
  std::vector<double> topextobjectstranseslice(topExtObjects.m_numExtObjectParts);
  std::vector<double> bottomextobjectstranseslice(bottomExtObjects.m_numExtObjectParts);
  std::vector<double> finaltopextobjectstranseslice(topExtObjects.m_numExtObjectParts);
  std::vector<double> finalbottomextobjectstranseslice(bottomExtObjects.m_numExtObjectParts);
  
  // variables for inital photon direction
  DirectionVector initialPhotonDir;
  double initialphotonradius = 0.0;
  double initialphotonphi = 0.0;
  
  // for looping through photon file later
  double rowenergy = 0.0;
  
  double offaxisfromfile = 0.0;     // input to sphericalToCartesianDir() [rad]
  double rollfromfile = 0.0;        // input to sphericalToCartesianDir()
  DirectionVector tempDirVec;       // output from sphericalToCartesianDir()
  double extendedtheta = 0.0;       // both are output from getBetaModelPhoton()
  double extendedphi = 0.0;         //    or getFlatCirclePhoton()
                                    //    and input to sourceLocalToAbsolutePos
  
  // Getting the input positions from a 'groundmodel' source
  CartesianCoord groundPos;
  CartesianCoord newGroundPos;
  double tgroundproj = 0.0;
  
  // input to raytraceonephoton()
  CartesianCoord initialphotonpos = initialPhotonPos;
  
  // max number of path coordinates to record in the history file
  const int maxNumCoordsToRecord = 7;
  //+++ this caused an error in main loop in raytraceonephoton (see question to Tahir for explanation)
  
  // input to raytraceonephoton(): maximum number of interactions of a single 
  // photon including the initial position/direction and the final results 
  // plane impact if there is one.
  const int maxNumPathCoords = 20;
  
  
  // outputs for raytraceonephoton
  int numInteractions = 0;
  std::vector<CartesianCoord> pathCoords(maxNumPathCoords);
  std::vector<DirectionVector> pathDirs(maxNumPathCoords);
  std::vector< std::vector<int> > pathCode(maxNumPathCoords);
  for (int iInteraction = 0 ; iInteraction < maxNumPathCoords ; ++iInteraction) {
    pathCode[iInteraction].resize(s_numPathCodeInts);
  }
  std::vector<int> pathXRTObjectID(maxNumPathCoords);
  CartesianCoord finalPhotonPos;
  DirectionVector finalPhotonDir;
  bool resultsPlaneWasImpacted = false;
  std::vector<int> pathErrorCode(maxNumPathCoords);
  
  // +++ 20140918 just for temp testing
  std::vector<double> pathIncidentAngle(maxNumPathCoords);
  std::vector<double> pathScatteringAngle(maxNumPathCoords);

  CartesianCoord finalPSFwOffset;     // psf coords, w offset, to write to history file column
  CartesianCoord finalpsf;            // psf coords, wo offset, to write to psf file
  double radialpsfdistmm = 0.0;
  double psfazimangle = 0.0;
  double radialpsfdistarcsec = 0.0;

  double finalradialdir = 0.0;
  double finaldirazimuth = 0.0;
  CartesianCoord finalphotondirCoord;
  
  // the pathcode for a photon, as a string, to print to the history file
  std::string combinedPathCodeString;
  
  // the row number to which to write data to the history file
  int rowNum = 0;
  
  // for writing the energy range in the header keyword of the PSF or EEF file
  std::string energyidstr;
  
  // the PSF/EEF and EA files will be numbered sequentially
  int psfExtNum = 0;
  int eaExtNum = 0;
  
  vector1Ddbl eefSlice;   // a slice of the EEF to pass to writePSFType2Ext

  // counter for photon number traced per off-axis angle per roll angle
  long photonCount = 0;
  
  int sumpatherrorcode = 0;   // output from updateEventCounters() 
  
  // for reading from input photon file, if provided
  std::map<std::string, int> photonColumns;
  int columnNum = 0;
  int row = 0;
  
  // Determine sector and gapnumber loop variables to pass to raytraceonephoton
  // the values changes in each loop in doWork, but the size doesn't
  std::vector<int> loopSectorNumbers(sectorsShells.m_numLoopSectors);

  
  // -------------------------------------
  
  AH_OUT << "Beginning raytracing" << std::endl;
  
  // if the photons are from a file, get all the column numbers before we enter
  // the loops below.
  if (photons.m_photonsFromFile) {
    
    fits_open_file(&photon_fp, (param.m_psrcfile).c_str(), READONLY, &status);
    checkForFITSError(status, "opening", param.m_psrcfile);
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_psrcfile << std::endl;
    
    // the psrcfile photon file should still be open 
    fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("ENERGY"), &columnNum, &status);
    photonColumns["ENERGY"] = columnNum;
    checkForFITSError(status, "getting ENERGY column numbers in", param.m_psrcfile);
    // +++ Make sure it's in keV
    
    if (param.m_sourceEnum == PHOTONLIST) {
      // get columns THETA, ROLL 
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("THETA"), &columnNum, &status);
      photonColumns["THETA"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("PHI"), &columnNum, &status);
      photonColumns["PHI"] = columnNum;
    } else if (param.m_sourceEnum == GROUNDMODEL) {
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("X"), &columnNum, &status);
      photonColumns["X"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("Y"), &columnNum, &status);
      photonColumns["Y"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("Z"), &columnNum, &status);
      photonColumns["Z"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("UNITX"), &columnNum, &status);
      photonColumns["UNITX"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("UNITY"), &columnNum, &status);
      photonColumns["UNITY"] = columnNum;
      fits_get_colnum(photon_fp, CASEINSEN, const_cast<char *>("UNITZ"), &columnNum, &status);
      photonColumns["UNITZ"] = columnNum;
    }
    checkForFITSError(status, "getting column numbers in", param.m_psrcfile);
  }       
  
  // Counter for total photon number traced (all energies, off-axis angles, 
  // roll angles)
  photonCount = 0;
  
  AH_DEBUG << "photons.m_nrollin = " << photons.m_nrollin << std::endl;
  AH_DEBUG << "photons.m_nthetain = " << photons.m_nthetain << std::endl;

  // -------------------------------------
  // Begin actual work loops
  // -------------------------------------
 
  // Calculate the increment for which to print photon counter to the 
  // screen such that we only print 10 messages. For this we have to 
  // calculate the total number of photons for the different situations. 
  // Still allow the increment to be 1000 if numphoton was initially 
  // negative
  long int finaltotphotons = 0;
  int percentcomplete = 0;
  if (photons.m_photonPrintIncrement == 0) {
    if (photons.m_photonsFromFile) {
      finaltotphotons = photons.m_nphotons;
    } else {
      if (param.m_doThetaPhiPairs) {
        finaltotphotons =  photons.m_numPhotonEnergies * photons.m_nthetain * photons.m_nphotons;
      } else {
        finaltotphotons = photons.m_numPhotonEnergies * photons.m_nthetain * photons.m_nrollin * photons.m_nphotons;
      } // end-if checking if doing theta/phi pairs
    } // end-if checking if photons from a file
    photons.m_photonPrintIncrement = finaltotphotons / 10	;
  } // end-if checking if photonPrintIncrement had to be calculated.
  
  // loop over azimuthal (or roll) angle
  for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
    
    // loop over off-axis (theta) angle
    for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
      
      // If the input photons are not from a file and the azimuthal (roll) 
      // angle mode is to only do diagonal pairs in the theta,phi matrix, 
      // then skip the non-diagonal loops
      if ( (!photons.m_photonsFromFile) && (param.m_doThetaPhiPairs) ) { 
        if (iTheta != iRoll) {
          continue;
        }
      }
      
      AH_DEBUG << "param.m_offaxisVec.size() = " << param.m_offaxisVec.size() << std::endl;
      AH_DEBUG << "photons.m_rollAnglesDeg.size() = " << photons.m_rollAnglesDeg.size() << std::endl;
      
      // off axis (theta) angle (it was input in arcmin)
      inputOffAxisThetaArcmin = param.m_offaxisVec[iTheta];
      
      // default: the initial theta is the input theta.  This will change if 
      // photons are from a file or if source is flatcircle or betamodel
      // +++ 20150331 KLR set the initial for photons from a file
      initialThetaArcmin = inputOffAxisThetaArcmin;
        
      if (!photons.m_photonsFromFile) {
        
        // there are only entries in m_rollAnglesDeg if photons aren't from file
        inputAzimuthDeg    = photons.m_rollAnglesDeg[iRoll];
        initialAzimuthDeg  = inputAzimuthDeg;
        
        // print output to user about which theta and roll we're at
        AH_OUT << "Starting run for azimuthal angle = "  << inputAzimuthDeg << " degrees, " <<
                  "off-axis angle = "  << inputOffAxisThetaArcmin << " arcmin" << std::endl;
      
        AH_DEBUG << "photons.m_offaxisThetaRad.size() = " << photons.m_offaxisThetaRad.size() << std::endl;
        AH_DEBUG << "photons.m_rollAnglesDeg.size() = " << photons.m_rollAnglesDeg.size() << std::endl;
        AH_DEBUG << "param.m_offaxisVec.size() = " << param.m_offaxisVec.size() << std::endl;
        
        // The cosine of the off-axis angle is copied to another variable here 
        // because elsewhere (e.g. for input photons from a file) it is 
        // obtained from the corresponding value in the input file (it will be 
        // used to calculate transmission in the external xrt components such 
        // as the thermal shield)
        cosInputOffaxisTheta = photons.m_cosOffaxisTheta[iTheta];
        oneOverCosInputOffaxisTheta = 1. / cosInputOffaxisTheta;
        
        // Calculate psfx(j,k) & psfy(j,k), the x and y coordinates respectively 
        // of the position of the source center on the results/focal plane. The 
        // PSF is calculated as photons/arcmin^2 in radial annuli centered on 
        // psfx,psfy. The units of psfx and psfy are mm (or the same units as 
        // those that the focal length is given in).
        // +++ add this to comments in header
        // Note that we need psfx and psfy even if a psf file will not be created 
        // because some columns in the photon history file need them.
        inputOffAxisThetaRad = photons.m_offaxisThetaRad[iTheta];
        fltantheta = genTelescope.m_focalLength * std::tan(inputOffAxisThetaRad);
        psf.m_psfx[iTheta][iRoll] = fltantheta * photons.m_cosRollAngles[iRoll];
        psf.m_psfy[iTheta][iRoll] = fltantheta * photons.m_sinRollAngles[iRoll];

      } // end-if photons not from a file 
      // if the photons are from a file, all of psf.m_psfx and psfy are 
      // initialized to zero, leave them
      
      // For each photon we need to know its initial direction in Cartesian 
      // coordinates. For the "groundmodel" we already have this explicitly. 
      // We have also already calculated the initial direction of every 
      // point-source photon (it is the same for all point-source photons for a 
      // given theta and roll-angle position).

      // For the other input photon model choices, we *generate* the off-axis 
      // angle and the roll angle to calculate the initial Cartesian direction 
      // vector. At this point we know the off-axis angle and roll angle of the 
      // center of the source, and it will not change with energy.

      // For extended sources, each photon inside the energy and photon number 
      // loops that follow will get their own off-axis angle and roll angle, but 
      // we still need to know these angles for the source center.

      // For a point source, assign the previously calculated Cartesian initial 
      // photon direction for the current roll angle and off-axis angle. For a 
      // point source, the photon direction is the same for every photon so this 
      // can be outside of the energy loop and the photon loop.
      if ( (param.m_sourceEnum == POINT) || 
           (param.m_sourceEnum == DIAGNOSTICMODE) ) {
        //+++ change m_xPtSrcInitPhotonDir to a DirectionVector
        initialPhotonDir.m_xDir = photons.m_xPtSrcInitPhotonDir[iTheta][iRoll];
        initialPhotonDir.m_yDir = photons.m_yPtSrcInitPhotonDir[iTheta][iRoll];
        initialPhotonDir.m_zDir = photons.m_zPtSrcInitPhotonDir[iTheta][iRoll];
      }
      
      // If the EEF or PSF image is to be calculated averaged over all 
      // energies, the normalization factors and psf image array need to be 
      // reset to zero before entering the energy loop
      if ( !photons.m_photonsFromFile && psf.m_summedEEFPSF ) {
        totaleefenergy = 0.0;
        totalpsfphotons = 0;
        // reset the psf image
        psf.m_psfImage.clear();
        psf.m_psfImage.resize(psf.m_numpsfxvals);
        for (int i = 0 ; i < psf.m_numpsfxvals ; ++i) {
          psf.m_psfImage[i].resize(psf.m_numpsfyvals);
        }
      }

      // don't reset all the stats counters here because the history file can 
      // now have results from multiple angles. However, we still need to reset 
      // number of double reflections because it is used to calculate the 
      // effective area for each theta and phi. 
      stats.numrpiDoubleRefl = 0;
      
      AH_DEBUG << "photons.m_nens = " << photons.m_nens << std::endl;
      
      
      // ---------------------------
      // Loop over photon energy 
      // (if input photons are from a file this loop 
      // effectively does not exist because nens=1 in that case)
      // ---------------------------
      
      for (int iEnergy = 0 ; iEnergy < photons.m_nens ; ++iEnergy) {
        
        // reset numrpiDoubleReflPerEnergy for each energy, theta, and 
        // azimuthal angle. this is used in the effective area calculation
        stats.numrpiDoubleReflPerEnergy = 0;

        AH_DEBUG << "photons.m_photonEnergies.size() = " << photons.m_photonEnergies.size() << std::endl;

        // +++ why have this again?? Can't we just have it here, and don't need it above?  even if m_summedEEFPSF, we'll nens is at least 1, right?
        // If the EEF or PSF image is to be calculated separately for each energy, the normalization factors and psf image array need to be reset to zero before each new energy run
        if ( !photons.m_photonsFromFile && !psf.m_summedEEFPSF ) {
          totaleefenergy = 0.0;
          totalpsfphotons = 0;
          // reset the psf image
          psf.m_psfImage.clear();
          psf.m_psfImage.resize(psf.m_numpsfxvals);
          for (int i = 0 ; i < psf.m_numpsfxvals ; ++i) {
            psf.m_psfImage[i].resize(psf.m_numpsfyvals);
          }
        }

        // Extract slices of the grids for reflectivity, transmission, 
        // taupermm, and scattering, if the input photons are not from a file 
        // or if an energy grid was specified for file photon input, in which 
        // case these arrays were pre-interpolated earlier and the flag 
        // preinterpolategrids was set. (Otherwise interpolation on the fly 
        // will be dealt with inside the next loop). These energy slices will 
        // be part of the input to the single-photon ray-tracing routine, 
        // raytraceonephoton().
        if (preinterpolateGrids) {

          for (int iAngle = 0 ; iAngle < numFrontAngles ; ++iAngle) {
            for (int iGrp = 0 ; iGrp < numGroups ; ++iGrp) {
              frefeslice[iAngle][iGrp] = remappedGrids.m_frontRefl[iEnergy][iAngle][iGrp];
              ftranseslice[iAngle][iGrp] = remappedGrids.m_frontTran[iEnergy][iAngle][iGrp];
            }
          }
          // +++ changed numBackAngles to numRoughAngles
          for (int iAngle = 0 ; iAngle < numRoughAngles ; ++iAngle) {
            roughrefeslice[iAngle][0] = remappedGrids.m_roughRefl[iEnergy][iAngle][0];
          }
          if (param.m_pcolExists) {
            for (int iAngle = 0 ; iAngle < numPcolAngles ; ++iAngle) {
              //+++ we're just copying same as roughrefeslice[iAngle][0]?
              //+++ refactor this?  or note that [1] means precol?
              roughrefeslice[iAngle][1] = remappedGrids.m_roughRefl[iEnergy][iAngle][0];
            }
          }
          for (int iGrp = 0 ; iGrp <= numGroups ; ++iGrp) {
            fronttaupermmeslice[iGrp] = remappedGrids.m_frontTauPermm[iEnergy][iGrp];
            roughtaupermmeslice[iGrp] = remappedGrids.m_roughTauPermm[iEnergy][iGrp];
          }
          // single-energy slices of transmission for each top external object
          for (int iObj = 0 ; iObj < topExtObjects.m_numExtObjectParts ; ++iObj) {
            topextobjectstranseslice[iObj] = remappedGrids.m_topExtObjectTransProb[iEnergy][iObj];
          }
          // single-energy slices of transmission for each bot. external object
          for (int iObj = 0 ; iObj < bottomExtObjects.m_numExtObjectParts ; ++iObj) {
            bottomextobjectstranseslice[iObj] = remappedGrids.m_bottomExtObjectTransProb[iEnergy][iObj];
          }

        } // end-if preinterpolateGrids

        // ---------------------------
        // Loop over photon number 
        // (for file photon input this loops over row of the file)
        // ---------------------------
  
        AH_DEBUG << "photons.m_nphotons = " << photons.m_nphotons << std::endl;
        for (int iPhoton = 0 ; iPhoton < photons.m_nphotons ; ++iPhoton) {

          // update photon counter
          photonCount++;
          
          // output to user an incremental (every 10%) message
          if ( (photonCount % photons.m_photonPrintIncrement) == 0) {
            percentcomplete = (100.0 * photonCount) / finaltotphotons;
            // Note: % can be rounded to 1% regardless
            AH_OUT << "Completed " << percentcomplete << "% (" << photonCount << " photons)" << std::endl;
          }
          
          if (photons.m_photonsFromFile) {
            row = iPhoton+1;    // for reading data from photon file

            // open the photon source file.  This will either be from 
            // source=photonlist or source=groundmodel.

            // read the energy value for this row, same column name for both types of file
            fits_read_col_dbl(photon_fp, photonColumns["ENERGY"], row, 1, 1, 0, &rowenergy, 0, &status);
            checkForFITSError(status, "getting ENERGY data from", param.m_psrcfile);
            
            AH_DEBUG << "rowenergy = " << rowenergy << " for row " << row << std::endl;

            //+++ I don't understand - wasn't this just done above, in if (preinterpolateGrids)? 
            //+++ how did we know we weren't in if (photons.m_photonsFromFile) ?
            // Call the remapping and interpolation routine: if the reflectivity 
            // and transmission grids etc. were already pre-interpolated onto a 
            // smaller energy grid, use the remapping routine on pre-interpolated 
            // arrays to extract slices of functions for a single energy. 
            // Otherwise, use the original reflecitivity and transmission grids 
            // to extract slices of functions for a single energy.
            if (preinterpolateGrids) {
              
              AH_DEBUG << "preinterpolateGrids" << std::endl;

              // Loop over energy grid to find which one matches "rowenergy" and 
              // pull out array slices for this energy
              //+++ is this if we had a unique energy grid?
              // +++ if photons are from a file, then m_photonEnergies is size 0!
              for (int ematchindex = 0 ; ematchindex < photons.m_numPhotonEnergies ; ++ematchindex) {
                if (rowenergy == photons.m_photonEnergies[ematchindex]) {
                  m = ematchindex;
                }
              }
              for (int iAngle = 0 ; iAngle < numFrontAngles ; ++iAngle) {
                for (int iGrp = 0 ; iGrp < numGroups ; ++iGrp) {
                  frefeslice[iAngle][iGrp] = remappedGrids.m_frontRefl[m][iAngle][iGrp];
                  ftranseslice[iAngle][iGrp] = remappedGrids.m_frontTran[m][iAngle][iGrp];
                }
              }
              // +++ changed numBackAngles to numRoughAngles
              for (int iAngle = 0 ; iAngle < numRoughAngles ; ++iAngle) {
                roughrefeslice[iAngle][0] = remappedGrids.m_roughRefl[m][iAngle][0];
              }
              if (param.m_pcolExists) {
                for (int iAngle = 0 ; iAngle < numPcolAngles ; ++iAngle) {
                  //+++ we're just copying same as roughrefeslice[iAngle][0]?
                  //+++ refactor this?  or note that [1] means precol?
                  roughrefeslice[iAngle][1] = remappedGrids.m_roughRefl[m][iAngle][0];
                }
              }
              for (int iGrp = 0 ; iGrp <= numGroups ; ++iGrp) {
                fronttaupermmeslice[iGrp] = remappedGrids.m_frontTauPermm[m][iGrp];
                roughtaupermmeslice[iGrp] = remappedGrids.m_roughTauPermm[m][iGrp];
              }
              // The following are single-energy slices of transmission for each of the external objects
              for (int iObj = 0 ; iObj < topExtObjects.m_numExtObjectParts ; ++iObj) {
                topextobjectstranseslice[iObj] = remappedGrids.m_topExtObjectTransProb[m][iObj];
              }
              for (int iObj = 0 ; iObj < bottomExtObjects.m_numExtObjectParts ; ++iObj) {
                bottomextobjectstranseslice[iObj] = remappedGrids.m_bottomExtObjectTransProb[m][iObj];
              }

            } else {
              
              
              AH_DEBUG << "don't preinterpolateGrids" << std::endl;

              // If the reflectivity/transmission grids were not 
              // pre-interpolated, do the interpolation now, using the original 
              // grids, and simply specifiying one target energy for the 
              // remapping, for which a temporary energy array of size 1 is 
              // created
              int numtargetenergy = 1;
              std::vector<double> targetenergy(numtargetenergy);
              targetenergy[0] = rowenergy;    //+++ is push_back appropriate after initializing to 1?
              remapReflectTrans(reflectTransGrids, numtargetenergy, targetenergy, numGroups, param.m_pcolExists, doTransmission, remappedGrids);

              // assign the 3D remapped grids to 2D frefeslice, etc to pass 
              // into raytraceonephoton
              // there's only entry in first (energy) dimension
              frefeslice = remappedGrids.m_frontRefl[0];
              ftranseslice = remappedGrids.m_frontTran[0];
              roughrefeslice = remappedGrids.m_roughRefl[0];
              fronttaupermmeslice = remappedGrids.m_frontTauPermm[0];
              roughtaupermmeslice = remappedGrids.m_roughTauPermm[0];
              topextobjectstranseslice = remappedGrids.m_topExtObjectTransProb[0];
              bottomextobjectstranseslice = remappedGrids.m_bottomExtObjectTransProb[0];

              // also put the input energy from the file into m_photonEnergies
              photons.m_photonEnergies.push_back(rowenergy);
              
            } // end if-block preinterpolateGrids

            for (int iObj = 0 ; iObj < topExtObjects.m_numExtObjectParts ; ++iObj) {
              AH_DEBUG << "topextobjectstranseslice["<<iObj<<"] = " << topextobjectstranseslice[iObj] << std::endl;
            }
            
            // Read the theta and phi columns from the input file if it is of 
            // type "photonlist"
            if (param.m_sourceEnum == PHOTONLIST) {

              // photonlist model, just 3 columns: energy, theta, phi (phi=roll)
              fits_read_col_dbl(photon_fp, photonColumns["THETA"], row, 1, 1, 0, &offaxisfromfile, 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["PHI"], row, 1, 1, 0, &rollfromfile, 0, &status);
              checkForFITSError(status, "getting THETA and PHI data", param.m_psrcfile);
              // Note the units of both angles in the original file format are 
              // radians. We may need to redefine the format for units of 
              // arcmin, which are more convenient.
              
              // Convert photon direction from spherical to Cartesian coords
              sphericalToCartesianDir(offaxisfromfile, rollfromfile, tempDirVec);
              // Direction vector to source points away from origin and photon 
              // direction points in the opposite direction so multiply by -1:
              initialPhotonDir.m_xDir = -1.0 * tempDirVec.m_xDir;
              initialPhotonDir.m_yDir = -1.0 * tempDirVec.m_yDir;
              initialPhotonDir.m_zDir = -1.0 * tempDirVec.m_zDir;

              // the off-axis angle is copied to the variable that is written 
              // to the history file (it is set for other source types as well)
              cosInputOffaxisTheta = std::cos(offaxisfromfile);
              oneOverCosInputOffaxisTheta = 1. / cosInputOffaxisTheta;
              
            } else if (param.m_sourceEnum == GROUNDMODEL) {
              
              // for the ground model, read photon initial x,y,z position and 
              // initial direction vector from the input file itself (name 
              // psrcfile, 1st extension). The remaining columns are positions
              fits_read_col_dbl(photon_fp, photonColumns["X"], row, 1, 1, 0, &(groundPos.m_x), 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["Y"], row, 1, 1, 0, &(groundPos.m_y), 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["Z"], row, 1, 1, 0, &(groundPos.m_z), 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["UNITX"], row, 1, 1, 0, &(initialPhotonDir.m_xDir), 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["UNITY"], row, 1, 1, 0, &(initialPhotonDir.m_yDir), 0, &status);
              fits_read_col_dbl(photon_fp, photonColumns["UNITZ"], row, 1, 1, 0, &(initialPhotonDir.m_zDir), 0, &status);
              checkForFITSError(status, "getting X,Y,Z,UNITX,UNITY,UNITZ data", param.m_psrcfile);
              
              // in the input file for groundmodel, the initial x and y are 
              // not initial x and y at telescope aperture (the first z level). 
              // However, in fastmode the code that works out which shell to 
              // start the photon assumes that this x and y is at the aperture 
              // +++ 20150126 addition - add to TRF
              newGroundPos.m_z = aperture.m_ztelescopeaperture;
              getXYForNewZ(groundPos, initialPhotonDir, newGroundPos, tgroundproj);
              initialphotonpos = newGroundPos;
              
              // The following is needed to calculate transmission through 
              // external xrt objects (e.g. thermal shield)
              cosInputOffaxisTheta = std::abs(initialPhotonDir.m_zDir);
              oneOverCosInputOffaxisTheta = 1. / cosInputOffaxisTheta;
            
            }
            
          } // end of if-block for input photons are from a file

          // Set the initial photon direction if this has not already been 
          // done, otherwise assign it to previously calculated value.

          // For the "groundmodel" mode, the initial photon positions were 
          // already given in the input photon file; for both "groundmodel" 
          // and "photonlist" mode, the initial photon directions were already 
          // set. For all other source modes, random initial photon positions 
          // on the telescope aperture are generated using the appropriate 
          // probability distributions.
          if (param.m_sourceEnum == FLATCIRCLE) {

            // Call routine to generate a random photon position (and hence 
            // direction) in an extended circular source with a uniform photon 
            // flux per unit area. First, the source is place at the pole 
            // (z-axis) and random theta and phi in spherical polar coordinates 
            // are generated:
            getFlatCirclePhoton(param.m_seed, param.m_flatradius, extendedtheta, extendedphi);
            // The source is then rotated so that its center is at the actual 
            // position given by theta(j) and roll(k) later
            
            
          } else if (param.m_sourceEnum == BETAMODEL) {
            // Call routine to generate a random photon position (and hence 
            // direction) in an extended circular source with a beta model 
            // spatial flux distribution.
            // Extract the following three numerical values from the string 
            // variable "betapars" in the input parameter file.
            std::vector<double> betapars;
            listStringsToDoubles(param.m_betapars, betapars);

            double betacore = betapars[0];        // core radius in arcmin
            double betaindex = betapars[1];       // power-law index of beta model
            double betamaxradius = betapars[2];   // maximum radius in arcmin
            //  First, the source is place at the pole (z-axis) and random 
            // theta and phi in spherical polar coordinates are generated:
            getBetaModelPhoton(param.m_seed, betacore, betaindex, betamaxradius, extendedtheta, extendedphi);
            // The source is then rotated so that its center is at the actual 
            // position given by theta(j) and roll(k) later

            AH_DEBUG << "extendedtheta = " << extendedtheta << std::endl;
            AH_DEBUG << "extendedphi = " << extendedphi << std::endl;
            
          } // end if-block for extended photon source models

          // If any either of the extended source models was selected, 
          // calculate the actual position of the random photon and its 
          // direction, after rotating the source to its actual position.
          if ( (param.m_sourceEnum == FLATCIRCLE) || 
               (param.m_sourceEnum == BETAMODEL) ) {

            CartesianCoord coordFinal;      // output from sourceLocalToAbsolutePos
            // The source is rotated so that its center is at the actual 
            // position given by theta(j) and roll(k), and the new position of 
            // the photon in spherical and Cartesian coordinates is computed:
            sourceLocalToAbsolutePos(photons.m_cosOffaxisTheta[iTheta], photons.m_sinOffaxisTheta[iTheta], photons.m_cosRollAngles[iRoll], photons.m_sinRollAngles[iRoll], extendedtheta, extendedphi, coordFinal);
            AH_DEBUG << "coordFinal = " << coordFinal << std::endl;
            // Precalculated trig. values are used in the above in the interest 
            // of speed 
            
            // Calculate the photon direction Cartesian components:
            initialPhotonDir.m_xDir = -1.0 * coordFinal.m_x;
            initialPhotonDir.m_yDir = -1.0 * coordFinal.m_y;
            initialPhotonDir.m_zDir = -1.0 * coordFinal.m_z;

            // Convert the actual off-axis angle of an individual photon to 
            // arcmin and the its azimuthal angle to degrees
            initialThetaArcmin  = extendedtheta * s_radianToArcmin;
            initialAzimuthDeg   = extendedphi * s_radianToDegrees;
            
//            inputOffAxisThetaArcmin = s_radianToArcmin * extendedtheta;   // +++ 20150331 KLR 
//            inputAzimuthDeg = s_radianToDegrees * extendedphi;  // +++ 20150331 KLR 
            oneOverCosInputOffaxisTheta = 1. / std::cos(extendedtheta);
            
          }
          
          // For input modes for photons from a file or a point source, the 
          // photon direction will already have been calculated.

          // For all except the groundmodel, generate initial position 
          // (x, y, z) of the photon, corresponding to an entry point on the 
          // aperture. For these source modes, random initial photon 
          // positions on the telescope aperture are generated using the 
          // appropriate probability distributions.
          // If there is any new mode in the future that has pre-determined 
          // x, y, z entry points, this if-block will need to be modified.

          if (param.m_sourceEnum == GROUNDMODEL) {
            
            cartesianToPolar(initialphotonpos, initialphotonradius, initialphotonphi);
            
            // for groundmodel, calculate the initialtheta and initialazimdir
            // columns for the history file from the initialPhotonDir columns
            // in the groundmodel file.  They must be in radians here, because 
            // they're converted before being written
            CartesianCoord initialPhotonDirCoord(initialPhotonDir.m_xDir,
                                                 initialPhotonDir.m_yDir, 
                                                 initialPhotonDir.m_zDir);
            cartesianToPolar(initialPhotonDirCoord, offaxisfromfile, rollfromfile);

          } else if (param.m_sourceEnum == DIAGNOSTICMODE) {
            initialphotonradius = entryRad;
            initialphotonphi = entryAngle;

          } else {

            initialphotonpos.m_z = aperture.m_ztelescopeaperture;

            if (aperture.m_isAnnulus) {
              // If the aperture is a partial annulus generate a random r^2 and 
          // phi in cylindrical coordinates from a uniform distribution 
          // (r^2 not r because we need equal numbers of photons in equal areas)
              initialphotonradius = std::sqrt( aperture.m_rMinSq + (getRandom() * (aperture.m_rMaxSq - aperture.m_rMinSq)) );
              // Following is the rotation angle of the entry point in radians
              initialphotonphi = aperture.m_minAngleRad + (aperture.m_deltaAngleRad * getRandom());
              initialphotonpos.m_x = initialphotonradius * std::cos(initialphotonphi);
              initialphotonpos.m_y = initialphotonradius * std::sin(initialphotonphi);
            } else {
              // else if the aperture is a rectangle
              initialphotonpos.m_x = aperture.m_xMin + (aperture.m_xWidth * getRandom());
              initialphotonpos.m_y = aperture.m_yMin + (aperture.m_yWidth * getRandom());
              // Convert the initial position to cylindrical coordinates
              cartesianToPolar(initialphotonpos, initialphotonradius, initialphotonphi);

            }  // end if-block testing if aperture is annulus or rectangle

          }  // end if-block checking source (groundmodel, diagnostic, or other)
          
          // Determine which energy in the scattering probability energy grid is most appropriate for the energy of the current photon. Regular interpolation is inadequate for the scattering profiles because the shapes of the probability distributions are "peculiar" and often vary erratically with input parameters. For the moment we simply choose the nearest grid energy until a more sophisticated approach is developed.
          int scatenergyindex = 0;
          double mindeltascatenergy = 1.0e30;
          double deltascatenergy = 0.0;
          int ies = 0;
          if (!isEqualCaseInsens(param.m_scattermode, "none")) {
            if (photons.m_photonEnergies[iEnergy] < scat.m_scatEnergies[0]) { 
              scatenergyindex = 0;
            } else if (photons.m_photonEnergies[iEnergy] > scat.m_scatEnergies[scat.m_numscatenergies-1]) {
              scatenergyindex = scat.m_numscatenergies-1;
            } else {
              mindeltascatenergy = 1.0e30;
              while (ies < scat.m_numscatenergies) {
                deltascatenergy = std::abs(photons.m_photonEnergies[iEnergy] - scat.m_scatEnergies[ies]);
                if (deltascatenergy < mindeltascatenergy) {
                  mindeltascatenergy = deltascatenergy;
                  scatenergyindex = ies;
                }
                ies++;
              }
            }
          }
          
          
          // variables that will be passed to raytraceonephoton which passes 
          // them to the new intercept routine, getimpactcandidates()
          // Determine the gap number, shell numbers that enclose the photon 
          // initial position, and the sector number corresponding to that 
          // position
          // A gap is defined as the interval between the front-side faces of two adjacent foils (or between the inner housing wall and the front-side of the first foil; or between the front-side of the last foil and the outer housing radius).
          // Thus, a gap is not completely empty but includes the thickness of the innermost foil of a pair (except for the first gap).
          // The gapnumber goes from 1 for the gap between the inner housing radius and the 1st shell, to numgapradialboundaries-1, or (number of foil shells)+1.
          // The routine will utilize gapradialboundaries which will already have taken into account whether the first objects encountered are pre-collimator or mirror foils. (The shell positions for the pre-collimator are not exactly the same as the primary mirror positions of the top edges of the foils).
          // Use a bisection method to locate the gap number and radii corresponding photon initial radius. The variables numgapradialboundaries and gapradialboundaries were calculated in xrtsetup().
          // Note that the gap & sector that are located do not account for misalignments so could be wrong at this stage. However, fastmode overcomes this by selecting a gap and sector range that covers likely misalignment ranges, and then subsequent impact testing routines will identify the correct photon-object impacts. The purpose of this initial gap and sector location is simply to serve as an anchor around which a range of sub-foils will be selected for testing. 
          
          int gapindex1 = 0;            // finding current radial gap
          int gapindex2 = 0;            // finding current radial gap
          int sectorgapindex1 = 0;      // finding current sector gap
          int sectorgapindex2 = 0;      // finding current sector gap
          int currentgapnumber = 0;
          int currentloshell = 0;
          int currenthishell = 0;
          int currentsectornumber = 0;
          
          // Determine sector and gapnumber loop variables to pass to raytraceonephoton()
          int lowShell = 0;     // trf: shellsearchlo
          int highShell = 0;    // trf: highShell
          int lowSector = 0;   // trf: lowSector
          int highSector = 0;   // trf: highSector
          
          AH_DEBUG << "sectorsShells.m_numgapradialboundaries = " << sectorsShells.m_numgapradialboundaries << std::endl;
          AH_DEBUG << "initialphotonradius = " << initialphotonradius << std::endl;
          
          // Transfrom the photon initial
          // position and direction into the TDF frame, except for 
          // source=DIAGNOSTICMODE, diagnosticParsArray.entrysegment>0 
          // (which is already in the TDF frame)
        
          CartesianCoord initialTDFPhotonPos;
          DirectionVector initialTDFPhotonDir;
          double initialTDFPhotonPhi = 0.0;
          CartesianCoord finalTDFPhotonPos;
          DirectionVector finalTDFPhotonDir;
          if ((param.m_sourceEnum == DIAGNOSTICMODE) && (entrysegment>0)) {
            initialTDFPhotonPos = initialphotonpos;
            initialTDFPhotonDir = initialPhotonDir;
            initialTDFPhotonPhi = initialphotonphi;
          } else {
            initialTDFPhotonPos.m_x= genTelescope.m_costelfprot*initialphotonpos.m_x+genTelescope.m_sintelfprot*initialphotonpos.m_y;
            initialTDFPhotonPos.m_y= genTelescope.m_costelfprot*initialphotonpos.m_y-genTelescope.m_sintelfprot*initialphotonpos.m_x;
            initialTDFPhotonPos.m_z= initialphotonpos.m_z;
            initialTDFPhotonDir.m_xDir= genTelescope.m_costelfprot*initialPhotonDir.m_xDir+genTelescope.m_sintelfprot*initialPhotonDir.m_yDir;
            initialTDFPhotonDir.m_yDir= genTelescope.m_costelfprot*initialPhotonDir.m_yDir-genTelescope.m_sintelfprot*initialPhotonDir.m_xDir;
            initialTDFPhotonDir.m_zDir= initialPhotonDir.m_zDir;
            cartesianToPolar(initialTDFPhotonPos, initialphotonradius, initialTDFPhotonPhi);
          }


          if (initialphotonradius > genTelescope.m_innerhousingradius) {
            bisectionLocate(sectorsShells.m_gapradialboundaries, sectorsShells.m_numgapradialboundaries, initialphotonradius, gapindex1, gapindex2);
            currentgapnumber = gapindex1 + 1;
            currentloshell = gapindex1;
            currenthishell = currentgapnumber;

            AH_DEBUG << "after call to bisectionLocate: " << std::endl;
            AH_DEBUG << "gapindex1 = " << gapindex1 << std::endl;
            AH_DEBUG << "gapindex2 = " << gapindex2 << std::endl;

            // Determine the sector number that the initial photon position 
            // lies in. The sector number goes from 
            // 1 to # of segments X # sectors per segment (note that the 
            // "sectornumber" in the TDF resets to 1 at the start of each 
            // segment so is not what is required here). We call it the 
            // "aperture sector number" and an interpolation grid for this was 
            // calculated in xrtsetup(). The interpolation grid goes from 
            // 0 to 360 degrees and contains an artificial sector if one of 
            // the original sectors crossed the +ve x-axis (0 degree line).
            // The units of the angle boundaries are radians so the initial 
            // photon phi also has to be in radians
            bisectionLocate(sectorsShells.m_mirrorsectorangleboundaries, sectorsShells.m_nummirrorsectorangleboundaries, initialTDFPhotonPhi, sectorgapindex1, sectorgapindex2);
            
            currentsectornumber = sectorgapindex1 + sectorsShells.m_zerosectorid + 1;
            if (currentsectornumber > sectorsShells.m_numApertureSectors) { 
              currentsectornumber = currentsectornumber - sectorsShells.m_numApertureSectors;
              // (The routine should never give a sector number<1)
            } // +++ add an else and throw an error, to be safe
            
            // Determine the sector and gapnumber loop variables to pass to 
            // raytraceonephoton
            // loopSectorNumbers is always the same size, 
            // sectorsShells.m_numLoopSectors
            // we need to clear it in each loop, and refill with new numbers 
            // based on currentsectornumber
            // +++ do I need to clear it each loop iteration if I'm refilling each entry?
            loopSectorNumbers.clear();
            loopSectorNumbers.resize(sectorsShells.m_numLoopSectors);
            lowShell = currentloshell - sectorsShells.s_deltaShells;
            highShell = currenthishell + sectorsShells.s_deltaShells;
            lowSector = currentsectornumber - sectorsShells.s_deltaSectors;
            highSector = currentsectornumber + sectorsShells.s_deltaSectors;

            if (lowShell < 1) {
              lowShell = 1;
            }
            if (highShell > sectorsShells.m_numShells) {
              highShell = sectorsShells.m_numShells;
            }
            if (lowSector < 1) {
              lowSector = sectorsShells.m_numApertureSectors - lowSector;
            }
            if (highSector > sectorsShells.m_numApertureSectors) {
              highSector = highSector - sectorsShells.m_numApertureSectors;
            }
            for (int iSector = 0 ; iSector < sectorsShells.m_numLoopSectors ; ++iSector) {
              loopSectorNumbers.at(iSector) = currentsectornumber - sectorsShells.m_numLoopSectors + 1 + iSector;
              if (loopSectorNumbers.at(iSector) < 1) {
                loopSectorNumbers.at(iSector) += sectorsShells.m_numApertureSectors;
              }
              if (loopSectorNumbers.at(iSector) > sectorsShells.m_numApertureSectors) {
                loopSectorNumbers.at(iSector) -= sectorsShells.m_numApertureSectors;
              }
            }
          
          } // end-if photon radial position is > inner housing radius

          AH_DEBUG << "oneOverCosInputOffaxisTheta = " << oneOverCosInputOffaxisTheta << std::endl;

          // Apply the cos(theta) factors to external object transmission 
          // factors if we are doing either:
          //   * an extended source, or 
          //   * the very first iterations of both the energy and photon number 
          //     loops for all other input source types
          if ( photons.m_isExtendedSource || photons.m_photonsFromFile || (iPhoton == 0) ) {
            for (int iObj = 0 ; iObj < topExtObjects.m_numExtObjectParts ; ++iObj) {
              finaltopextobjectstranseslice[iObj] = std::pow(topextobjectstranseslice[iObj], oneOverCosInputOffaxisTheta);
              AH_DEBUG << "topextobjectstranseslice[iObj] = " << topextobjectstranseslice[iObj] << std::endl;
            } 
            for (int iObj = 0 ; iObj < bottomExtObjects.m_numExtObjectParts ; ++iObj) {
              finalbottomextobjectstranseslice[iObj] = bottomextobjectstranseslice[iObj];
            }
          } // end-if checking for extended source or 1st iterations of E and N loops
          


          AH_DEBUG << "before raytraceOnePhoton" << std::endl;
          
          
          // ---------------------------
          // Now call the subroutine that traces one photon from the intial 
          // position to its final resting place in the telescope.
          // ---------------------------

          if (param.m_doSuperFastMode) {
            raytracePhotonFast(has3DObjects, param.m_pcolExists, initialTDFPhotonPos, initialTDFPhotonDir, initialphotonradius, initialTDFPhotonPhi, reflectTransGrids.m_numFrontAngles, reflectTransGrids.m_frontAngles, frefeslice, ftranseslice, fronttaupermmeslice, reflectTransGrids.m_numBackAngles, reflectTransGrids.m_backAngles, roughrefeslice, roughtaupermmeslice, topExtObjects, bottomExtObjects, finaltopextobjectstranseslice, finalbottomextobjectstranseslice, genTelescope, housings, xrtzlevels, param.m_resultsplanez, maxNumPathCoords, XRTObjects, scatenergyindex, scat, sectorsShells, obstructionIntervalLowIndex, obstructionIntervalHighIndex, transforms, numInteractions, pathCoords, pathDirs, pathCode, pathXRTObjectID, finalTDFPhotonPos, finalTDFPhotonDir, resultsPlaneWasImpacted, pathErrorCode);
          } else {
            raytraceOnePhoton(has3DObjects, param.m_pcolExists, param.m_doFastMode, initialTDFPhotonPos, initialTDFPhotonDir, initialphotonradius, initialTDFPhotonPhi, reflectTransGrids.m_numFrontAngles, reflectTransGrids.m_frontAngles, frefeslice, ftranseslice, fronttaupermmeslice, reflectTransGrids.m_numBackAngles, reflectTransGrids.m_backAngles, roughrefeslice, roughtaupermmeslice, topExtObjects, bottomExtObjects, finaltopextobjectstranseslice, finalbottomextobjectstranseslice, scatenergyindex, scat, sectorsShells, loopSectorNumbers, lowShell, highShell, genTelescope, housings, housingHardLimit, numxrtzlevels, xrtzlevels, param.m_resultsplanez, maxNumPathCoords, transforms, zmaxsortxrtobjectindexAll, numXRTObjects, XRTObjects, obstructionIntervalLowIndex, obstructionIntervalHighIndex, numInteractions, pathCoords, pathDirs, pathCode, pathXRTObjectID, finalTDFPhotonPos, finalTDFPhotonDir, resultsPlaneWasImpacted, pathErrorCode, pathIncidentAngle, pathScatteringAngle);
          }
          // See raytraceonephoton() for further details of the input and output parameters.
          // Brief summary repeated here for the outputs of raytraceonephoton()
          // numInteractions: total number of photon/telescope interactions
          // pathcoordinates(numInteractions+1,3): Cartesian coordinates at each interaction (0th entry is the initial position)
          // pathphotondir(numInteractions+1,3): Cartesian components of the photon unit direction vector prior to each interaction (0th entry is the initial direction)
          // finalphotonpos(3), finalphotondir(3): the position and direction vector of the last interaction (copied from the final interactions in pathcoordinates, pathphotondir, since we do not know the number of interactions in advance).
          // pathcode(numInteractions,3): integers recording various outcomes and attributes, three integers per interaction
          // pathxrtobjectid(numInteractions): integer specifiying which XRT object was impacted at each interaction.
          // resultsPlaneWasImpacted: Boolean that is true if the photon survived to impact the results plane (focal plane by default).
          // patherrorcode: Integer detailing any anomalies or errors that were produced in the photon path calculations.

          // Now that 1 photon has been completely ray-traced, calculate some derived quantities for this photon, and update some statistical quantities that collect results on multiple photons.

          //Transform final photon position and direction back to physical telescope coordinate system (rotation around z-axis
          // of +telfprot)
          finalPhotonPos.m_x = genTelescope.m_costelfprot*finalTDFPhotonPos.m_x - genTelescope.m_sintelfprot*finalTDFPhotonPos.m_y;
          finalPhotonPos.m_y = genTelescope.m_sintelfprot*finalTDFPhotonPos.m_x + genTelescope.m_costelfprot*finalTDFPhotonPos.m_y;
          finalPhotonPos.m_z = finalTDFPhotonPos.m_z;
          finalPhotonDir.m_xDir = genTelescope.m_costelfprot*finalTDFPhotonDir.m_xDir - genTelescope.m_sintelfprot*finalTDFPhotonDir.m_yDir;
          finalPhotonDir.m_yDir = genTelescope.m_sintelfprot*finalTDFPhotonDir.m_xDir + genTelescope.m_costelfprot*finalTDFPhotonDir.m_yDir;
          finalPhotonDir.m_zDir = finalTDFPhotonDir.m_zDir; 
          
          AH_DEBUG << "after raytraceonephoton" << std::endl;
          AH_DEBUG << "finalPhotonPos = " << finalPhotonPos << std::endl;
          
          // Update the event counters regardless of whether a history file is 
          // going to be written (in particular statresults.numRpiDoubleRefl is 
          // now going to be used to calculate the effective area)
          updateEventCounters(numInteractions, pathCode, resultsPlaneWasImpacted, stats, pathErrorCode, sumpatherrorcode);
          
          // Join the individual pathcodestring values to form a single string, combinedPathCodeString. Limit the length of the string combinedPathCodeString to 32 characters, or 8 interactions, including the final one, which may or may not be a focal plane impact (each interaction is described by 4 characters). The first 4 characters of pathcodestring correspond to the first interaction. If there are more than 8 interactions, include the first 8 consecutive interactions. If there are less than 8 interactions, fill the unused characters with trailing blanks (presumably this is done automatically).
          createCombinedPathCodeString(numInteractions, pathCode, combinedPathCodeString);
          
          AH_DEBUG << "pathcode = " << combinedPathCodeString << std::endl;
          
          // these need to be set for the history file, whether or not 
          // photons are from a file
          finalphotondirCoord.m_x = finalPhotonDir.m_xDir;
          finalphotondirCoord.m_y = finalPhotonDir.m_yDir;
          // finalradialdir is the final photon direction radial component in 
          // the x-y plane, and finaldirazimuth is the final photon position 
          // azimuthal angle (relative to x-axis passing through optical axis
          cartesianToPolar(finalphotondirCoord, finalradialdir, finaldirazimuth);
          finaldirazimuth *= s_radianToDegrees;

          // Calculate some quantities that are needed only if a PSF file was requested, or if a history file is to be written
          if ( !photons.m_photonsFromFile && ( doHistoryFile || isInputFileGiven(param.m_outpsffile) ) ) {

            AH_DEBUG << std::endl;
            
            // Final photon position relative to the source center theoretical 
            // position on the results plane, in terms of arcseconds radial 
            // "distance" and rotation angle relative to an axis centered on 
            // the source position and parallel to the x-axis.

            // +++ 20140820
            // we checked the orientation of the image by plotting finalPhotonPos 
            // from history file and from the image (temporarily replacing 
            // finalpsf x and y with finalPhotonPos x and y).  Currently 
            // centering of the psf* coords only works for roll=0.  For nonzero 
            // roll, the current location of the image is not correct.  when a 
            // more accurate centering formula is used, the psf image should
            // always be centered on the origin.
            // The PSF with an offset is written to the history file
            finalPSFwOffset.m_x = finalPhotonPos.m_x + psf.m_psfx[iTheta][iRoll];
            finalPSFwOffset.m_y = finalPhotonPos.m_y + psf.m_psfy[iTheta][iRoll];
            // 20140929
            // The coordinates written to the psf image are not to be offset
            // 20141001
            // I'm using finalPSFwOffset to get psfxindex anyway, to get transmission working
            finalpsf.m_x = finalPhotonPos.m_x;
            finalpsf.m_y = finalPhotonPos.m_y;
            cartesianToPolar(finalPSFwOffset, radialpsfdistmm, psfazimangle);
            radialpsfdistarcsec = radialpsfdistmm / plateScaleArcsec;

            long eefbinindex = 0;
            long psfxindex = 0;
            long psfyindex = 0;

            // Now specialize only on events that impacted the focal plane and 
            // no abnormal error flag
            if ( resultsPlaneWasImpacted && (sumpatherrorcode == 0) ) {
              
              AH_DEBUG << "eefbinindex = " << eefbinindex << std::endl;
              AH_DEBUG << "radialpsfdistarcsec = " << radialpsfdistarcsec << std::endl;
              AH_DEBUG << "psf.m_delta_radeef = " << psf.m_delta_radeef << std::endl;
              
              // find which radial bin of the EEF the position corresponds to 
              // (truncate to long)
              eefbinindex = static_cast<long>(radialpsfdistarcsec / psf.m_delta_radeef);
              
              AH_DEBUG << "eefbinindex = " << eefbinindex << std::endl;
              AH_DEBUG << "psf.m_numradeef = " << psf.m_numradeef << std::endl;
              AH_DEBUG << "psf.m_radeef.size() = " << psf.m_radeef.size() << std::endl;
              AH_DEBUG << "psf.m_summedEEFPSF = " << ( psf.m_summedEEFPSF ? "yes" : "no") << std::endl;
              
              
              // Update the EEF & PSF image: but first check the flag 
              // summedEEFPSF (T/F) to see if we are creating one EEF summed 
              // over all energies or one EEF for each energy
              for (int ir = eefbinindex ; ir < psf.m_numradeef ; ++ir) {
                // only increment if our radius is inside
                if (radialpsfdistarcsec <= psf.m_radeef[ir]) {
                  if (psf.m_summedEEFPSF) {
                    psf.m_eef[ir][iRoll][iTheta][0] += photons.m_photonEnergies[iEnergy];
                  } else {
                    psf.m_eef[ir][iRoll][iTheta][iEnergy] += photons.m_photonEnergies[iEnergy];
                  }
                }
              }

              psfxindex = static_cast<int>( (fpmmToArcsec*finalPSFwOffset.m_x - psf.m_psfLowerXCorner) / psf.m_delta_radeef );
              psfyindex = static_cast<int>( (fpmmToArcsec*finalPSFwOffset.m_y - psf.m_psfLowerYCorner) / psf.m_delta_radeef );

              if ( (psfxindex >=0) && (psfxindex < psf.m_numpsfxvals) && 
                   (psfyindex >=0) && (psfyindex < psf.m_numpsfyvals) ) {
                psf.m_psfImage[psfxindex][psfyindex] += 1.0;
                // Following will be used for normalizing the PSF image (do it 
                // just before writing each psf image extension)
                totalpsfphotons++;
              }

              // Following will be used for normalizing the EEF (do it just 
              // before writing each eef extension)
              totaleefenergy += photons.m_photonEnergies[iEnergy];

            } // end if-block checking if photon impacted the results plane

          } // end-if checking (psf file = yes and photons from a file = no) OR doHistory
          
          // Update cumulative array that counts number of photons that hit the
          // results plane for each energy, off-axis angle, and azimuthal angle
          if (resultsPlaneWasImpacted && (sumpatherrorcode == 0) ) { 
            ea.m_resultsplanephotons[iRoll][iTheta][iEnergy]++;

            // Currently, for the effective area, we only count doubly-
            // reflected photons (1 on primary mirror, 1 on secondary mirror) 
            // for the contribution to the effective area. This automatically 
            // excludes photons that passed through gaps in the system but it 
            // is uncertain whether once- reflected photons in the region 
            // expected for doubly-reflected rays. Note that this also means 
            // that the effective area for events involving transmission is not 
            // correctly calculated.
            
            // Improved method for calculating effective area that includes 
            // double reflections (exactly one primary and one secondary), 
            // regardless of pre-collimator reflection and transmission in any 
            // XRT component will use the counter statresults.numRpiDoubleRefl 
            // and is assigned to effareaphotons(iRoll,iTheta,iEnergy) at the end of each loop 
            // over energy, theta, and phi.
            
            // +++ 20150108 I think this has changed slightly. 
            
          }
          
          // Write one row of the photon history file columns to the file, 
          // corresponding to results for one traced photon, whatever the 
          // outcome of that ray-trace was. (The header of the history file is 
          // written later after the end of the photon number and energy loops, 
          // since it contains some aggregate and statistical quantities).
          // only write this row if the input .par file specified to only write 
          // events that survived to impact the results plane.
          if ( doHistoryFile && 
               !(param.m_resplaneonly && !resultsPlaneWasImpacted) ) {
            
            // write the row for this photon and energy
            rowNum++;

            // put the local variables in datatypes necessary for cfitsio
            long numInteractionsLng = numInteractions;
            long sumpatherrorcodesLng = sumpatherrorcode;
            char * c_pathcode = const_cast<char*>(combinedPathCodeString.c_str());
            char c_resultsplaneimpact[1] = {resultsPlaneWasImpacted ? 1 : 0};
            std::string currColName;      // for getting event1x, etc column names
            double initalAzimDeg = initialphotonphi * s_radianToDegrees;
            
            double thetaToWrite = 0.0; 
            double rollToWrite = 0.0; 
            double energyToWrite = 0.0;
            
            // +++ comment
            if (photons.m_photonsFromFile) {
              AH_DEBUG << "photons from a file" << std::endl;
              thetaToWrite  = offaxisfromfile * s_radianToArcmin;
              rollToWrite   = rollfromfile * s_radianToDegrees;
              energyToWrite = rowenergy;
            } else {
              AH_DEBUG << "photons not from a file" << std::endl;
              thetaToWrite  = initialThetaArcmin;
              rollToWrite   = initialAzimuthDeg;
              energyToWrite = photons.m_photonEnergies[iEnergy];
              
//              thetaToWrite = inputOffAxisThetaArcmin; // +++ 20150331 change these, becuase flatcircle and betamodel have different input and initial
//              rollToWrite = inputAzimuthDeg;    // +++ 20150331 
            }
            
            
            AH_DEBUG << "thetaToWrite = " << thetaToWrite << std::endl;
            AH_DEBUG << "rollToWrite = " << rollToWrite << std::endl;
            AH_DEBUG << "photons.m_photonEnergies.size() = " << photons.m_photonEnergies.size() << std::endl;
            AH_DEBUG << "energyToWrite = " << energyToWrite << std::endl;
            
            // columns to write if user requested full or basic history file
            if ( isEqualCaseInsens(param.m_phisttype, "full") || 
                 isEqualCaseInsens(param.m_phisttype, "basic") ) {
              
              fits_write_col_dbl(param.m_history_fp, historyColumns["energy"],         rowNum, 1, 1, &energyToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialrad"],     rowNum, 1, 1, &initialphotonradius, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialazimpos"], rowNum, 1, 1, &initalAzimDeg, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialx"],       rowNum, 1, 1, &initialphotonpos.m_x, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialy"],       rowNum, 1, 1, &initialphotonpos.m_y, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialtheta"],   rowNum, 1, 1, &thetaToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialazimdir"], rowNum, 1, 1, &rollToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalxpos"],      rowNum, 1, 1, &finalPhotonPos.m_x, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalypos"],      rowNum, 1, 1, &finalPhotonPos.m_y, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalxpsf"],      rowNum, 1, 1, &finalPSFwOffset.m_x, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalypsf"],      rowNum, 1, 1, &finalPSFwOffset.m_y, &status);
              fits_write_col_str(param.m_history_fp, historyColumns["pathcode"],       rowNum, 1, 1, &c_pathcode, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["radialdir"],      rowNum, 1, 1, &finalradialdir, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["azimuthdir"],     rowNum, 1, 1, &finaldirazimuth, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalxdir"],      rowNum, 1, 1, &finalPhotonDir.m_xDir, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalydir"],      rowNum, 1, 1, &finalPhotonDir.m_yDir, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalzdir"],      rowNum, 1, 1, &finalPhotonDir.m_zDir, &status);
              fits_write_col_lng(param.m_history_fp, historyColumns["numint"],         rowNum, 1, 1, &numInteractionsLng, &status);
              if (param.m_resplaneonly) {
                fits_write_col_lng(param.m_history_fp, historyColumns["rowindex"],     rowNum, 1, 1, &photonCount, &status);
              } else {
                fits_write_col_log(param.m_history_fp, historyColumns["resultsplane"], rowNum, 1, 1, c_resultsplaneimpact, &status);
              }
              fits_write_col_lng(param.m_history_fp, historyColumns["errorcode"],      rowNum, 1, 1, &sumpatherrorcodesLng, &status);
            
            } else if (isEqualCaseInsens(param.m_phisttype, "brief")) {
              // columns to write if user only wanted brief history file
              
              fits_write_col_dbl(param.m_history_fp, historyColumns["energy"],         rowNum, 1, 1, &energyToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialx"],       rowNum, 1, 1, &initialphotonpos.m_x, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialy"],       rowNum, 1, 1, &initialphotonpos.m_y, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialtheta"],   rowNum, 1, 1, &thetaToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["initialazimdir"], rowNum, 1, 1, &rollToWrite, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalxpos"],      rowNum, 1, 1, &finalPhotonPos.m_x, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalypos"],      rowNum, 1, 1, &finalPhotonPos.m_y, &status);
              fits_write_col_str(param.m_history_fp, historyColumns["pathcode"],       rowNum, 1, 1, &c_pathcode, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalxdir"],      rowNum, 1, 1, &finalPhotonDir.m_xDir, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalydir"],      rowNum, 1, 1, &finalPhotonDir.m_yDir, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["finalzdir"],      rowNum, 1, 1, &finalPhotonDir.m_zDir, &status);
              if (param.m_resplaneonly) {
                fits_write_col_lng(param.m_history_fp, historyColumns["rowindex"],     rowNum, 1, 1, &photonCount, &status);
              } else {
                fits_write_col_log(param.m_history_fp, historyColumns["resultsplane"], rowNum, 1, 1, c_resultsplaneimpact, &status);
              }
              
            } // end-if which type of history file
            
            if (param.m_writeCoords) {
              // write all the coords for each interaction (up to a set number 
              // of interactions).  This is set true if phisttype=full
              // +++ the upper limit of this loop could be more efficient maybe.  up to numinteractions if numinteractions is less than max?
              for (int iInteraction = 1 ; iInteraction < maxNumCoordsToRecord ; ++iInteraction) {
                currColName = "event";
                currColName.append(intToString(iInteraction));
                currColName.append("x");
                fits_write_col_dbl(param.m_history_fp, historyColumns[currColName],  rowNum, 1, 1, &pathCoords[iInteraction].m_x, &status);
                currColName.replace(currColName.length()-1,1,"y");
                fits_write_col_dbl(param.m_history_fp, historyColumns[currColName],  rowNum, 1, 1, &pathCoords[iInteraction].m_y, &status);
                currColName.replace(currColName.length()-1,1,"z");
                fits_write_col_dbl(param.m_history_fp, historyColumns[currColName],  rowNum, 1, 1, &pathCoords[iInteraction].m_z, &status);
              }

              // +++ 20140918 the following are for testing
              bool foundPri = false;
              bool foundSec = false;
              long priobjid = -1;
              long secobjid = -1;
              double priincangle = 0.0;
              double secincangle = 0.0;
              double priscatangle = 0.0;
              double secscatangle = 0.0;
              for (int iInteraction = 0 ; iInteraction <= numInteractions ; ++iInteraction) {
                if ( (pathCode[iInteraction][1] == 06) && !foundPri) {
                  priobjid = pathXRTObjectID[iInteraction];
                  priincangle = pathIncidentAngle[iInteraction];
                  priscatangle = pathScatteringAngle[iInteraction];
                  foundPri = true;
                }
                if ( (pathCode[iInteraction][1] == 7) && !foundSec) {
                  secobjid = pathXRTObjectID[iInteraction];
                  secincangle = pathIncidentAngle[iInteraction];
                  secscatangle = pathScatteringAngle[iInteraction];
                  foundSec = true;
                }
              }
              fits_write_col_lng(param.m_history_fp, historyColumns["priobjid"],       rowNum, 1, 1, &priobjid, &status);
              fits_write_col_lng(param.m_history_fp, historyColumns["secobjid"],       rowNum, 1, 1, &secobjid, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["priincangle"],    rowNum, 1, 1, &priincangle, &status);
              fits_write_col_dbl(param.m_history_fp, historyColumns["secincangle"],    rowNum, 1, 1, &secincangle, &status);

            }
            checkForFITSError(status, "writing data to", param.m_outphistfile);

          } // end-if doHistoryFile

        } // end for-loop over photon number
        
        // improved method for calculating effective area that includes double 
        // reflections (exactly one primary and one secondary), regardless of 
        // pre-collimator reflection and transmission in any XRT component.  
        // will use the counter statresults.numRpiDoubleRefl and is assigned to 
        // effareaphotons(iRoll,iTheta,iEnergy) at end of each loop over energy, theta, and phi
        
        // +++ comment
        if (!photons.m_photonsFromFile) {
          // Calculate effective area for this energy, off-axis angle, and azimuthal angle
          ea.m_effareaphotons[iRoll][iTheta][iEnergy] = stats.numrpiDoubleReflPerEnergy;
          // Keep intermediate array effareaphotons(iRoll,iTheta,iEnergy) for diagnostic purposes
          ea.m_ea[iRoll][iTheta][iEnergy] = aperture.m_effAreaFactor * ea.m_effareaphotons[iRoll][iTheta][iEnergy];
          
          // if we're summing the PSF energies, there is only one extension for 
          // all the energies, but still need different extensions for each 
          // offaxis/theta combination
          if (psf.m_summedEEFPSF) {
            psfExtNum = (iRoll * photons.m_nthetain) + iTheta + 1;
          } else {
            psfExtNum = (iRoll * photons.m_nthetain * psf.m_numEEFens) + (iTheta * psf.m_numEEFens) + iEnergy + 1;
          }
        }

        // if the EEF or PSF image is to be calculated separately for each 
        // energy, the EEF or PSF image need to written to a unique extension 
        // before starting the next energy run. (the eef is not summed, so 
        // we're doing for each energy)
        if ( isInputFileGiven(param.m_outpsffile) && !photons.m_photonsFromFile && !psf.m_summedEEFPSF ) {
          
          energyidstr = doubleToString(photons.m_photonEnergies[iEnergy]);  // +++ Truncate to nearest 0.001 keV

          if (psf.m_psfType == PSF) {
            writePSFType1Ext(param, psfExtNum, photons, genTelescope, plateScaleArcmin, energyidstr, inputOffAxisThetaArcmin, inputAzimuthDeg, totalpsfphotons,  psf.m_psfx[iTheta][iRoll], psf.m_psfy[iTheta][iRoll], psf.m_psfLowerXCorner, psf.m_psfLowerYCorner, psf.m_delta_radeef, psf.m_psfImage);
          }
          if (psf.m_psfType == EEF) {
            // get a slice of the eef
            eefSlice.clear();
            eefSlice.resize(psf.m_numradeef);
            for (int iRad = 0 ; iRad < psf.m_numradeef ; ++iRad) {
              eefSlice[iRad] = psf.m_eef[iRad][iRoll][iTheta][iEnergy];
            }

            writePSFType2Ext(param, psfExtNum, genTelescope, plateScaleArcmin, energyidstr, inputOffAxisThetaArcmin, inputAzimuthDeg, totalpsfphotons, psf.m_psfx[iTheta][iRoll], psf.m_psfy[iTheta][iRoll], psf.m_radeef, psf.m_delta_radeef, totaleefenergy, eefSlice);
          }
        } // end-if writing the psf extension headers

      } // end for-loop over photon energies, iEnergy

      // Truncate energyidstr components to nearest 0.001 keV because it is to be a keyword value +++ do this
      energyidstr = doubleToString(photons.m_minPhotonEnergy);
      energyidstr.append(" to ");
      energyidstr.append(doubleToString(photons.m_maxPhotonEnergy));

      // If the EEF or PSF image is to be calculated averaged over all energies, the EEF or PSF image need to be written to one extension after completion of the energy and photon number loops (and the eef is summed, so we're doing for one energy bin)
      if ( isInputFileGiven(param.m_outpsffile) && !photons.m_photonsFromFile && psf.m_summedEEFPSF ) {
        

        if (psf.m_psfType == PSF) {
          writePSFType1Ext(param, psfExtNum, photons, genTelescope, plateScaleArcmin, energyidstr, inputOffAxisThetaArcmin, inputAzimuthDeg, totalpsfphotons, psf.m_psfx[iTheta][iRoll], psf.m_psfy[iTheta][iRoll], psf.m_psfLowerXCorner, psf.m_psfLowerYCorner, psf.m_delta_radeef, psf.m_psfImage);
        }

        if (psf.m_psfType == EEF) {
          // get a slice of the eef
          eefSlice.clear();
          eefSlice.resize(psf.m_numradeef);
          for (int iRad = 0 ; iRad < psf.m_numradeef ; ++iRad) {
            eefSlice[iRad] = psf.m_eef[iRad][iRoll][iTheta][0];
          }
          writePSFType2Ext(param, psfExtNum, genTelescope, plateScaleArcmin, energyidstr, inputOffAxisThetaArcmin, inputAzimuthDeg, totalpsfphotons, psf.m_psfx[iTheta][iRoll], psf.m_psfy[iTheta][iRoll], psf.m_radeef, psf.m_delta_radeef, totaleefenergy, eefSlice);
        }
      }

      // +++ combine this with above if-block.
      // Write the effective area (EA) for this pair of off-axis and azimuthal 
      // angles (theta(j), roll(k), respectively) to a unqiue extension in the 
      // output EA file
      if ( isInputFileGiven(param.m_outeafile) && !photons.m_photonsFromFile ) {
        eaExtNum = (iRoll*photons.m_nthetain) + iTheta + 1;
        vector1Ddbl effareaslice = ea.m_ea[iRoll][iTheta];
        writeEFFAreaExt(param, eaExtNum, genTelescope, energyidstr, inputOffAxisThetaArcmin, inputAzimuthDeg, photons.m_photonEnergies, effareaslice);
      }

    } // end for-loop over iTheta, off-axis angle
  } // end for-loop over iRoll, phi, azimuthal angle
  
  // if we wrote a psf and/or ea file, now go back to primary extension to 
  // write some keywords
  if (!photons.m_photonsFromFile) {
    // for type 1 (psf image), we wrote the keywords inside writePSFType1Ext()
    if ( isInputFileGiven(param.m_outpsffile) && (psf.m_psfType != PSF) ) {
      writePrimaryExtKeywords(&param.m_psf_fp, param.m_outpsffile, photons);
    }
    
    if ( isInputFileGiven(param.m_outeafile) ) {
      writePrimaryExtKeywords(&param.m_ea_fp, param.m_outeafile, photons);
    }
  }
  
  if (doHistoryFile) {
    
    AH_DEBUG << "photonCount = " << photonCount << std::endl;
    
    // Write header for 1st extension
    writeHistoryFileHeader(param, stats, photons, genTelescope, genTelescope.m_focalLength, plateScaleArcmin, aperture.m_fullGeometricAreaSqmm, aperture.m_geometricAreaSqcm, aperture.m_ztelescopeaperture, aperture.m_isAnnulus, genTelescope.m_innerhousingradius, genTelescope.m_outerhousingradius, transforms.m_doTransforms, photons.m_numPhotonEnergies, photonCount);
    
    // Write 2nd extension
    if (!photons.m_photonsFromFile) {
      writeHistoryFile2ndExtension(param, photons, genTelescope);
    }
    
  }

  // close photon_fp
  if (photon_fp != 0) {
    fits_close_file(photon_fp, &status);
    checkForFITSError(status, "closing", "photon file");
  }
  
  AH_DEBUG << "end of doWork()" << std::endl;
  
  
  // output to the user
  AH_OUT << "************************************" << std::endl;
  AH_OUT << "          END XRTRAYTRACE           " << std::endl;
  AH_OUT << "************************************" << std::endl;

}   // end doWork()


// ----------------------------------------------------------------------------


void finalize(Param & param) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  
  // -------------------------------------
  
  // free the random number pointer that was created in initialize()
  freeRandom();
  
  // close history, PSF, and EA files
  if (param.m_history_fp != 0) {
    fits_close_file(param.m_history_fp, &status);
    checkForFITSError(status, "closing", param.m_outphistfile);
  }
  if (param.m_psf_fp != 0) {
    fits_close_file(param.m_psf_fp, &status);
    checkForFITSError(status, "closing", param.m_outpsffile);
    //+++ I've had lots of errors regarding closing the psf file.
    // +++ valgrind  at line 1607 (fits_close)
//    
//==1468== Syscall param write(buf) points to uninitialised byte(s)
//==1468==    at 0x6FF1BA: write$NOCANCEL (in /usr/lib/system/libsystem_kernel.dylib)
//==1468==    by 0x5B959D: __sflush (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0x5E4F0C: __sfvwrite (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0x5E4CA4: fwrite (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0xBFB24: file_write (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB33A2: ffwrite (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB105E: ffbfwt (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB1223: ffflsh (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB34F7: ffclos (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0x100010CA2: finalize(Param&) (xrtraytrace.cxx:1607)
//==1468==    by 0x100001CAC: main (xrtraytrace.cxx:170)
//==1468==  Address 0x1051354c8 is 2,904 bytes inside a block of size 4,096 alloc'd
//==1468==    at 0xC658: malloc (vg_replace_malloc.c:295)
//==1468==    by 0x5B83F7: __smakebuf (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0x5AED19: __swsetup (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0x5E4D2D: __sfvwrite (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0x5E4CA4: fwrite (in /usr/lib/system/libsystem_c.dylib)
//==1468==    by 0xBFB24: file_write (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB33A2: ffwrite (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB105E: ffbfwt (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB1223: ffflsh (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0xB34F7: ffclos (in /Users/kristinrutkowskiwork/Documents/headas_astroh_untagged/heacore/x86_64-apple-darwin11.4.2/lib/libcfitsio_3.36.dylib)
//==1468==    by 0x100010CA2: finalize(Param&) (xrtraytrace.cxx:1607)
//==1468==    by 0x100001CAC: main (xrtraytrace.cxx:170)
//==1468==  Uninitialised value was created by a stack allocation
//==1468==    at 0x1000701ED: writePSFType1Ext(Param&, int, int, int, std::string const&, std::string const&, std::string const&, double, double, long, double, double, long, double, std::vector<std::vector<double, std::allocator<double> >, std::allocator<std::vector<double, std::allocator<double> > > > const&) (xrtraytrace_lib.cxx:7928)
  }
  if (param.m_ea_fp != 0) {
    fits_close_file(param.m_ea_fp, &status);
    checkForFITSError(status, "closing", param.m_outeafile);
  }
  
}   // end finalize()


// ----------------------------------------------------------------------------



/**********************************************
 * ********************************************
 * 		initialize() helper functions
 * ********************************************
**********************************************/



void initializePhotons(Param & param, Photons & photons, PSF_EEF & psf,
                       bool & preinterpolateGrids) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * photon_fp;                 // photon data file
  int status = 0;                       // for cfitsio function calls

  // flag if we're provided a unique energy list
  bool uniqueEnergyListExists = false;
  
  DirectionVector dirVec;               // output for sphericalToCartesianDir()
  
  // -------------------------------------
  
  // For input photon modes that require reading a file we must force 
  // nthetain (number of offaxis-angle points) and nrollin (number of 
  // azimuthal or roll angle points) both to be 1 because the photons in the 
  // file already have their own off-axis and roll angles and setting 1 for 
  // both will prohibit looping over these angles in the main ray-tracing 
  // block. The photon energies are also already in the file so we do not use 
  // the energy grid in the input parameter file. Also, since we are tracing 
  // only one photon per row in the input FITS file, the number of photons to 
  // trace is equal to 1 per row (over-riding the photon number in the input 
  // parameter file).

  // decide if photons are from a file
  if ( param.m_sourceEnum == PHOTONLIST || 
       param.m_sourceEnum == GROUNDMODEL ) { 
    photons.m_photonsFromFile = true;
  }
  
  if (photons.m_photonsFromFile) {
    
    // open the file
    fits_open_file(&photon_fp, (param.m_psrcfile).c_str(), READONLY, &status);
    checkForFITSError(status, "opening", param.m_psrcfile);
    //+++ make sure I close this
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_psrcfile << std::endl;
    
    // store how many rows/photons in this extension
    fits_get_num_rows(photon_fp, &(photons.m_nphotons), &status);
    checkForFITSError(status, "getting number of rows in", param.m_psrcfile);
    
    // For input photon modes that do not involve reading an input file, 
    // m_nens will be equal to the number of photon energies to be ray-traced; 
    // but for a file input, this will be a dummy loop variable so we set it 
    // equal to 1 
    photons.m_nens = 1;

    // The actual photon energy will be read row-by-row from the input file,
    // but we will read a keyword that will indicate whether to use the 
    // regular form of energy input in the .par file to specify a grid of unique
    // energies. This will avoid unnecessary interpolation of the 
    // reflectivity (etc.) grids when reading energies from a file.
    //+++ make sure this works, and this keyword is correct unit
    fits_read_key(photon_fp, TLOGICAL, const_cast<char *>("UNQELIST"), &uniqueEnergyListExists, 0, &status);
    checkForFITSError(status, "reading keyword UNQELIST in", param.m_psrcfile);
    
    // read keywords for the minimum and maximum energies in order to keep
    // the size of reflectivity (etc.) grids under control
    fits_read_key_dbl(photon_fp, const_cast<char *>("MINENERG"), &(photons.m_minPhotonEnergy), 0, &status);
    fits_read_key_dbl(photon_fp, const_cast<char *>("MAXENERG"), &(photons.m_maxPhotonEnergy), 0, &status);
    checkForFITSError(status, "reading keywords in", param.m_psrcfile);
  
    // For input photon modes that require reading a file we must force nthetain (number of offaxis-angle points) and nrollin (number of azimuthal or roll angle points) both to be 1 because the photons in the file already have their own off-axis and roll angles and setting 1 for both will prohibit looping over these angles in the main ray-tracing block. The photon energies are also already in the file so we do not use the energy grid in the input parameter file. Also, since we are tracing only one photon per row in the input FITS file, the number of photons to trace is equal to 1 per row (over-riding the photon number in the input parameter file).
    photons.m_nthetain = 1;
    photons.m_nrollin = 1;
  
  } // end if-block if photons from a file
  
  // If the photons are from a file AND a unique energy grid was specified, 
  // OR if the photons are not from a file then read the input energy grid 
  // from the .par file or an energy grid file
  if (uniqueEnergyListExists || !photons.m_photonsFromFile) {
    AH_DEBUG << "photons are not from a file, or there is a unique energy" << std::endl;
  
    // read the input energies from the .par file, setting min and max too
    getInputEnergies(param.m_energy, photons, param);
    
    if ( (photons.m_energyislist) && (param.m_energies[0] < 0.0) ) { 
      photons.m_minPhotonEnergy = param.m_searchenergymin;
      photons.m_maxPhotonEnergy = param.m_searchenergymax;
    } else {
      photons.m_minPhotonEnergy = getMinDouble(photons.m_photonEnergies);
      photons.m_maxPhotonEnergy = getMaxDouble(photons.m_photonEnergies);
    }

    // If the condition in this if-block is satisfied, we can pre-interpolate
    // the reflectivity (etc.) grids on energy before entering the main 
    // ray-tracing loop, otherwise the grids have to be interpolated (later) 
    // on the fly. Set a flag for this
    preinterpolateGrids = true;
    
  } else {
    
    AH_DEBUG << std::endl;
    
    preinterpolateGrids = false;
    
    // read keywords for the minimum and maximum energies in order to keep
    // the size of reflectivity (etc.) grids under control
    fits_read_key_dbl(photon_fp, const_cast<char *>("MINENERG"), &(photons.m_minPhotonEnergy), 0, &status);
    fits_read_key_dbl(photon_fp, const_cast<char *>("MAXENERG"), &(photons.m_maxPhotonEnergy), 0, &status);
    checkForFITSError(status, "reading keywords in", param.m_psrcfile);

  }
  
  AH_DEBUG << "photons.m_minPhotonEnergy = " << photons.m_minPhotonEnergy << std::endl;
  AH_DEBUG << "photons.m_maxPhotonEnergy = " << photons.m_maxPhotonEnergy << std::endl;
  
  if (!photons.m_photonsFromFile) {
    
    // Set up off-axis angle and roll angle grids:
    // From the input parameter offaxis (type string) in the input parameter file,
    // extract the numerical values of off-axis angles (units arcmin), convert to
    // radians, and store the values
    listStringsToDoubles(param.m_offaxis, photons.m_offaxisThetaArcmin);
    photons.m_nthetain = photons.m_offaxisThetaArcmin.size();
    
    // convert them all from arcmin to radian
    photons.m_offaxisThetaRad.resize(photons.m_nthetain);
    std::transform(photons.m_offaxisThetaArcmin.begin(), photons.m_offaxisThetaArcmin.end(), 
                   photons.m_offaxisThetaRad.begin(),
                   std::bind2nd(std::multiplies<double>(), s_arcminToRadian));
    
    // Get the roll angles
    listStringsToDoubles(param.m_roll, photons.m_rollAnglesDeg);
    photons.m_nrollin = photons.m_rollAnglesDeg.size();
    
    if (photons.m_rollAnglesDeg.at(0) < 0.0) {
      if (photons.m_nthetain != photons.m_nrollin) {
        AH_THROW_RUNTIME("In this mode, the number of off-axis angles ('offaxis' parameter) must be equal to the number of azimuthal angles('roll' parameter)");
      }
      param.m_doThetaPhiPairs = true;
    } else {
      param.m_doThetaPhiPairs = false;
    }
    
    // now that we've decided which roll mode, store the mod(abs,360) values
    std::transform(photons.m_rollAnglesDeg.begin(), photons.m_rollAnglesDeg.end(), 
                   photons.m_rollAnglesDeg.begin(), op_abs);
    std::transform(photons.m_rollAnglesDeg.begin(), photons.m_rollAnglesDeg.end(), 
                   photons.m_rollAnglesDeg.begin(), op_fmod360);
    
    // store the roll angles in radians, from degrees
    photons.m_rollAnglesRad.resize(photons.m_nrollin);
    std::transform(photons.m_rollAnglesDeg.begin(), photons.m_rollAnglesDeg.end(), 
                   photons.m_rollAnglesRad.begin(),
                   std::bind2nd(std::multiplies<double>(), s_degreesToRadian));
    
    
    // pre-calculate trig quantities for both angles (for speed later)
    photons.m_cosOffaxisTheta.resize(photons.m_nthetain);
    photons.m_sinOffaxisTheta.resize(photons.m_nthetain);
    photons.m_cosRollAngles.resize(photons.m_nrollin);
    photons.m_sinRollAngles.resize(photons.m_nrollin);
    std::transform(photons.m_offaxisThetaRad.begin(), photons.m_offaxisThetaRad.end(), 
                   photons.m_cosOffaxisTheta.begin(), op_cos);
    std::transform(photons.m_offaxisThetaRad.begin(), photons.m_offaxisThetaRad.end(), 
                   photons.m_sinOffaxisTheta.begin(), op_sin);
    std::transform(photons.m_rollAnglesRad.begin(), photons.m_rollAnglesRad.end(), 
                   photons.m_cosRollAngles.begin(), op_cos);
    std::transform(photons.m_rollAnglesRad.begin(), photons.m_rollAnglesRad.end(), 
                   photons.m_sinRollAngles.begin(), op_sin); 
    
    // Pre-calculate photon directions for a point-source
    // The center of the source is located at an off-axis angle of theta(i) and a
    // rotation (roll) angle of roll(j). If the photons are from a point source we can
    // pre-calculate the directions but it will be an array because of the possibility of
    // multiple theta and phi (=roll) values. We also do this for extended sources, in
    // which case the direction corresponds to photons from the center of the source
    // only (directions from other points on the source will be calculated later)
    photons.m_xPtSrcInitPhotonDir.resize(photons.m_nthetain);
    photons.m_yPtSrcInitPhotonDir.resize(photons.m_nthetain);
    photons.m_zPtSrcInitPhotonDir.resize(photons.m_nthetain);
    for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
      photons.m_xPtSrcInitPhotonDir[iTheta].resize(photons.m_nrollin);
      photons.m_yPtSrcInitPhotonDir[iTheta].resize(photons.m_nrollin);
      photons.m_zPtSrcInitPhotonDir[iTheta].resize(photons.m_nrollin);
    }
    
    // remember that here, the photons are not from a file
    for (int iRoll = 0 ; iRoll < photons.m_nrollin ; ++iRoll) {
      for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
        // Call routine to convert direction in spherical polar coordinates to Cartesian
        //coordinates. The vector to the source position points radially outwards, but the
        //photon direction points radially inward, towards the origin, so every Cartesian
        //component is effectively negated.
        sphericalToCartesianDir(photons.m_offaxisThetaRad[iTheta], 
                                photons.m_rollAnglesRad[iRoll], dirVec);
        photons.m_xPtSrcInitPhotonDir[iTheta][iRoll] = -1.0 * dirVec.m_xDir;
        photons.m_yPtSrcInitPhotonDir[iTheta][iRoll] = -1.0 * dirVec.m_yDir;
        photons.m_zPtSrcInitPhotonDir[iTheta][iRoll] = -1.0 * dirVec.m_zDir;
      }
    }
    
    // Set a flag to indicate if we are doing a point source or extended 
    // source; the parameter source is from the input .par file
    if ( (param.m_sourceEnum == FLATCIRCLE) || 
         (param.m_sourceEnum == BETAMODEL) ) { 
      photons.m_isExtendedSource = true;
    }
    
      
    // We setup EEF/PSF and EA arrays inside this loop because these outputs 
    // should be suppressed if the input photon source was a file because the 
    // photons in the file may originate in very different positions.
    
    
    // set up radial bins for EEF array, if the input parameter "outpsffile" was not "none" in the input parameter file
//    if (isInputFileGiven(param.m_outpsffile)) {

      // Get these from the string in the input .par file called "psfpars":
      switch (static_cast<int>(param.m_psfparsVec[0])) {
        case 1:
          psf.m_psfType = PSF;
          break;
        case 2:
          psf.m_psfType = EEF;
          break;
        default:
          AH_THROW_RUNTIME("Only values of 1 (for PSf image file) and 2 (for EEF file) are supported for the first value of the 'psfpars' parameter"); 
      }
      psf.m_numradeef = static_cast<long>(param.m_psfparsVec[1]);
      psf.m_delta_radeef = param.m_psfparsVec[2];

      // Following will hold the eef radial values
      psf.m_radeef.resize(psf.m_numradeef);
      for (int ir = 0 ; ir < psf.m_numradeef ; ++ir) {
        psf.m_radeef[ir] = ir * psf.m_delta_radeef;
      }

      // Following defines image pixels and size for the psftype=1 (images) option
      psf.m_numpsfxvals = (2 * psf.m_numradeef) + 1;
      psf.m_numpsfyvals = psf.m_numpsfxvals;
      psf.m_psfImageCenterPixel = psf.m_numradeef + 1;
      psf.m_psfLowerXCorner = -0.5 * psf.m_delta_radeef * (2.0*psf.m_numradeef + 1);
      psf.m_psfLowerYCorner = psf.m_psfLowerXCorner;
      
      // +++ why not for psftype=2?
      // +++ also, why isn't m_numpsfxvals the same as m_psfx.size()?  (which is set just below)
      
//    } // end-if outpsffile given
      
    //The center of the source has a corresponding x,y position on the focal plane and this x,y position (referred to as psfx,psfy) is the position of the peak of the photon distribution on the results/focal plane. The following, psfx and psfy, are the x and y coordinates respectively of the position of the source center on the results/focal plane (will be filled in later after the ray-tracing is complete).
    // Note that we need psfx and psfy even if a psf file will not be created 
    // because the some columns in the photon history file need them.
    psf.m_psfx.resize(photons.m_nthetain);
    psf.m_psfy.resize(photons.m_nthetain);
    for (int iTheta = 0 ; iTheta < photons.m_nthetain ; ++iTheta) {
      psf.m_psfx[iTheta].resize(photons.m_nrollin);
      psf.m_psfy[iTheta].resize(photons.m_nrollin);
    }

    
  } // end if-block !m_photonsFromFile
  
  AH_DEBUG << "photons.m_nthetain = " << photons.m_nthetain << std::endl;
  AH_DEBUG << "photons.m_nrollin = " << photons.m_nrollin << std::endl;
  AH_DEBUG << "photons.m_nens = " << photons.m_nens << std::endl;
  
  
} // end initializePhotons()




/*** 
 * helpers for initializePhotons()
 ***/
 
 // this is for getting either the unique energy list, or 
 // if the input photons aren't from a file
void getInputEnergies(std::string & energyInput, Photons & photons, Param & param) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * fitsfp;                    // reflectivity data file
  int status = 0;                       // for cfitsio function calls
  int fileExists = 0;
  std::string errorMsg;
  
  int energyColNum = 0;
  energyUnits_e energyUnit;
  
  // -------------------------------------
  
  // see if the energy param was a valid file
  fits_file_exists(energyInput.c_str(), &fileExists, &status);
  
  // the file does exist
  if (fileExists == 1) {

    // open the file
    fits_open_file(&fitsfp, energyInput.c_str(), READONLY, &status);
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << energyInput << std::endl;

    // if the file was opened, but the extension name was incorrect
    if (status == BAD_HDU_NUM) {
      errorMsg= "The extension name specificed for the energy file '";
      errorMsg.append(energyInput);
      errorMsg.append("' does not match an extension in the file");
      AH_THROW_RUNTIME(errorMsg);
    }

    // see if there were any other errors opening the file
    checkForFITSError(status, "opening", energyInput);
  
    photons.m_energyislist = false;
    
    // see how many energies are in the grid
    fits_get_num_rows(fitsfp, &(photons.m_numPhotonEnergies), &status);
    checkForFITSError(status, "getting number of energy rows in", energyInput);
    photons.m_photonEnergies.resize(photons.m_numPhotonEnergies);
    
    fits_get_colnum(fitsfp, CASEINSEN, const_cast<char *>("ENERGY"), &energyColNum, &status);
    checkForFITSError(status, "getting energy column number in", energyInput);
    
    // get energy data 
    for (long long i = 0 ; i < photons.m_numPhotonEnergies ; ++i) {
      fits_read_col_dbl(fitsfp, energyColNum, i+1, 1, 1, 0, &(photons.m_photonEnergies[i]), 0, &status);
    }
    checkForFITSError(status, "getting data from", energyInput);
  
    energyUnit = getEnergyUnit(fitsfp, energyInput, energyColNum);
    storeEnergyInkeV(energyUnit, photons.m_photonEnergies);
  
    // close energy file
    fits_close_file(fitsfp, &status);
    checkForFITSError(status, "closing", energyInput);

    // +++ do this within the row loop above, to save time?
    photons.m_minPhotonEnergy = getMinDouble(photons.m_photonEnergies);
    photons.m_maxPhotonEnergy = getMaxDouble(photons.m_photonEnergies);
    
    
  } else {
    // the file doesn't exist.  This should be a string of energies.
    
    photons.m_energyislist = true;
    // energy should be a list of numbers in a string; extract those numbers
    listStringsToDoubles(energyInput, param.m_energies);
    
    // If energy is a list of numbers AND the first value is negative then the next two numbers represent the minimum and maximum energies respectively of the energy range to retrieve later from the energy grid in the reflectivity file.
    if (param.m_energies[0] < 0.0) {
      
      /// make sure there are three numbers here
      if (param.m_energies.size() < 3) {
        AH_THROW_RUNTIME("If the first value in the 'energy' parameter is negative, there must be two more numbers, representing the min and max energy values to search for inside the reflectivty file");
      }
      
      param.m_energiesfromreflectivity = true;
      param.m_searchenergymin = param.m_energies[1];
      param.m_searchenergymax = param.m_energies[2];
      
    } else {
      // energies are not from a reflectivity file.  The energies are 
      // coming from the param list
      
      param.m_energiesfromreflectivity = false;
      photons.m_photonEnergies = param.m_energies;
      photons.m_numPhotonEnergies = photons.m_photonEnergies.size();
      // +++ do this within the row loop above, to save time?
      photons.m_minPhotonEnergy = getMinDouble(photons.m_photonEnergies);
      photons.m_maxPhotonEnergy = getMaxDouble(photons.m_photonEnergies);
      
    }
    
    // +++ shouldn't this only happen if energies are not from a reflectivity file?
    photons.m_numPhotonEnergies = photons.m_photonEnergies.size();
    
  }

  /* testing that I got energy data */
  #ifdef DEBUG
  AH_DEBUG << std::endl;
  AH_DEBUG << "*** testing that I got energy grid data ***" << std::endl;
  for (int i = 0 ; i < photons.m_numPhotonEnergies ; ++i) { 
    AH_DEBUG << "energy[" << i << "] = " << photons.m_photonEnergies[i] << std::endl;
  }
  #endif
  
} // end getInputEnergies()




void setupHistoryFile(Param & param, std::map<std::string, int> & historyColumns) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  int fileExists = 0;                   // check if file already exists
  
  int tfields = 0;        // how many columns in the extension
  // the following define the column names, formats, units; are keywords in header
  std::vector<std::string> ttype;       // column name in fits file
  std::vector<std::string> tform;       // data format for column in fits file
  std::vector<std::string> tunit;       // physical unit for column in fits file
  char ** c_ttype;        // convert ttype to c string for fits call
  char ** c_tform;        // convert tform to c string for fits call
  char ** c_tunit;        // convert tunit to c string for fits call
  std::string currColName;              // creating all the event1x etc columns
  
  const int maxNumInteractions = 7;     // how many interactions to record
  
  std::string historyFileName = param.m_outphistfile;
  
  // -------------------------------------
  
  // check if file already exists
  fits_file_exists(historyFileName.c_str(), &fileExists, &status);
  checkForFITSError(status, "checking for existence of", historyFileName);
  
  // if file exists
  if (fileExists == 1) {
    
    // open file before I can delete it
    fits_open_file(&param.m_history_fp, historyFileName.c_str(), READWRITE, &status);
    checkForFITSError(status, "opening", historyFileName);
    
    // if clobber was set, delete this file to make a new one
    if ( headas_clobpar ) {
      fits_delete_file(param.m_history_fp, &status);
      checkForFITSError(status, "deleting an older version of", historyFileName);
    } else {
      // clobber wasn't set but the file already exists.  Throw an error.
      AH_THROW_RUNTIME("File already exists, but clobber was set to no.  Either delete the file or set clobber to yes.");
    }
    
  } else if (fileExists == -1) {
    AH_THROW_RUNTIME("input file name is not a disk file");
  }

  // now create it (we either deleted it if clobber was set, or it didn't exist)
  fits_create_file(&param.m_history_fp, historyFileName.c_str(), &status);
  checkForFITSError(status, "creating", historyFileName);

  AH_INFO(ahlog::HIGH) << "Creating file: " << historyFileName << std::endl;
  
  // create the columns for extension (make sure they're all lowercase, to 
  // look up in the std::map later when writing data)
  
  // columns to write if user requested full or basic history file
  if ( isEqualCaseInsens(param.m_phisttype, "full") || 
       isEqualCaseInsens(param.m_phisttype, "basic") ) {
    
    ttype.push_back("energy");
    tform.push_back("1D");
    tunit.push_back("keV");
    ttype.push_back("initialrad");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("initialazimpos");
    tform.push_back("1D");
    tunit.push_back("deg");
    ttype.push_back("initialx");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("initialy");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("initialtheta");
    tform.push_back("1D");
    tunit.push_back("arcmin");
    ttype.push_back("initialazimdir");
    tform.push_back("1D");
    tunit.push_back("deg");
    ttype.push_back("finalxpos");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("finalypos");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("finalxpsf");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("finalypsf");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("pathcode");
    tform.push_back("32A");
    tunit.push_back("");
    ttype.push_back("radialdir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("azimuthdir");
    tform.push_back("1D");
    tunit.push_back("deg");
    ttype.push_back("finalxdir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("finalydir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("finalzdir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("numint");
    tform.push_back("1J");
    tunit.push_back("");
    if (param.m_resplaneonly) {
      // if we're only writing resultsplane photons, then also write a column
      // that gives the row number this would be if we were writing all photons
      ttype.push_back("rowindex");
      tform.push_back("1J");
      tunit.push_back("");
    } else {
      // if we're writing all rows, then write the boolean column that tells
      // if this photon reached the results plane
      ttype.push_back("resultsplane");
      tform.push_back("1L");
      tunit.push_back("");
    }
    ttype.push_back("errorcode");
    tform.push_back("1J");
    tunit.push_back("");  

  } else if (isEqualCaseInsens(param.m_phisttype, "brief")) {
    
    ttype.push_back("energy");
    tform.push_back("1D");
    tunit.push_back("keV");
    ttype.push_back("initialx");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("initialy");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("initialtheta");
    tform.push_back("1D");
    tunit.push_back("arcmin");
    ttype.push_back("initialazimdir");
    tform.push_back("1D");
    tunit.push_back("deg");
    ttype.push_back("finalxpos");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("finalypos");
    tform.push_back("1D");
    tunit.push_back("mm");
    ttype.push_back("pathcode");
    tform.push_back("32A");
    tunit.push_back("");
    ttype.push_back("finalxdir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("finalydir");
    tform.push_back("1D");
    tunit.push_back("");
    ttype.push_back("finalzdir");
    tform.push_back("1D");
    tunit.push_back("");
    if (param.m_resplaneonly) {
      // if we're only writing resultsplane photons, then also write a column
      // that gives the row number this would be if we were writing all photons
      ttype.push_back("rowindex");
      tform.push_back("1J");
      tunit.push_back("");
    } else {
      // if we're writing all rows, then write the boolean column that tells
      // if this photon reached the results plane
      ttype.push_back("resultsplane");
      tform.push_back("1L");
      tunit.push_back("");
    }
    
  } // end-if which columns to create, based on type of history file
  
  // +++ this if-block and boolean could be absorbed into above
  // This is only true if phisttype=full
  if (param.m_writeCoords) {
    for (int iInteraction = 0 ; iInteraction < maxNumInteractions ; ++iInteraction) {
      // we're only recording 7 interactions, so I don't need to worry about padding with 0s
      currColName = "event";
      currColName.append(intToString(iInteraction+1));
      
      currColName.append("x");
      ttype.push_back(currColName);
      tform.push_back("1D");
      tunit.push_back("mm");
      
      currColName.replace(currColName.length()-1,1,"y");
      ttype.push_back(currColName);
      tform.push_back("1D");
      tunit.push_back("mm");
      
      currColName.replace(currColName.length()-1,1,"z");
      ttype.push_back(currColName);
      tform.push_back("1D");
      tunit.push_back("mm"); 
    }
  
    // +++ 20140918 the following are for testing
    ttype.push_back("priobjid");
    tform.push_back("1J");
    tunit.push_back("");
    ttype.push_back("secobjid");
    tform.push_back("1J");
    tunit.push_back("");
    ttype.push_back("priincangle");
    tform.push_back("1D");
    tunit.push_back("deg");
    ttype.push_back("secincangle");  
    tform.push_back("1D");    
    tunit.push_back("deg");
  } // end-if coordHistory
  
  // +++ add comments to columns
  
  // see how many columns we now have
  tfields = ttype.size();
  // make the tunit vector the same size
  tunit.resize(tfields);
      
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; ++i) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
    
    // also throw the column names into a map, so that we can reference columns
    // by their names and not depend on their numbers (columns are 1-indexed)
    historyColumns.insert(std::make_pair(ttype[i], i+1));
  }
  
  // create the extension
  fits_create_tbl(param.m_history_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform, c_tunit, s_historyExtName.c_str(), &status);
  checkForFITSError(status, "creating extension in", historyFileName);
  
  // move to the extension
  fits_movnam_hdu(param.m_history_fp, ANY_HDU, const_cast<char *>(s_historyExtName.c_str()), 0, &status);
  checkForFITSError(status, "moving to extension in", historyFileName);
  
  // delete the vectors for c strings
  delete [] c_ttype;
  delete [] c_tform;
  delete [] c_tunit;
 
  
  
  // ---------------------------------------------------------------------------
  // update TTYPEn keywords (column names), based on which columns are present
  // ---------------------------------------------------------------------------
  
  
  std::string TTYPE = "TTYPE";
  std::string currTTYPE = "";
  if ( isEqualCaseInsens(param.m_phisttype, "full") || 
       isEqualCaseInsens(param.m_phisttype, "basic") ) {
    
    currTTYPE = TTYPE + intToString(historyColumns["energy"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "energy", "Photon energy", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialrad"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialrad", "Initial photon aperture radial position", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialazimpos"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialazimpos", "Initial photon aperture azimuthal position", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialx"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialx", "Initial x position of photon at aperture", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialy"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialy", "Initial y position of photon at aperture", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialtheta"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialtheta", "Intial photon direction off-axis angle", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialazimdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialazimdir", "Intial photon direction azimuthal angle", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalxpos"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalxpos", "Final x position of photon on results plane", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalypos"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalypos", "Final y position of photon on results plane", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalxpsf"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalxpsf", "Final x position relative to PSF peak", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalypsf"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalypsf", "Final y position relative to PSF peak", &status);
    currTTYPE = TTYPE + intToString(historyColumns["pathcode"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "pathcode", "Code describing photon path through telescope", &status);
    currTTYPE = TTYPE + intToString(historyColumns["radialdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "radialdir", "Final radial direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["azimuthdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "azimuthdir", "Final azimuthal direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalxdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalxdir", "Final x direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalydir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalydir", "Final y direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalzdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalzdir", "Final z direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["numint"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "numint", "Num interactions btwn photon and telescope", &status);
    if (param.m_resplaneonly) {
      currTTYPE = TTYPE + intToString(historyColumns["rowindex"]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "rowindex", "Row number in full event list", &status);
    } else {
      currTTYPE = TTYPE + intToString(historyColumns["resultsplane"]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "resultsplane", "Whether photon reached results plane", &status);
    }
    currTTYPE = TTYPE + intToString(historyColumns["errorcode"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "errorcode", "Code describing any errors encountered", &status);
    
  } else if (isEqualCaseInsens(param.m_phisttype, "brief")) {
    
    currTTYPE = TTYPE + intToString(historyColumns["energy"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "energy", "Photon energy", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialx"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialx", "Initial x position of photon at aperture", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialy"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialy", "Initial y position of photon at aperture", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialtheta"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialtheta", "Intial photon direction off-axis angle", &status);
    currTTYPE = TTYPE + intToString(historyColumns["initialazimdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "initialazimdir", "Intial photon direction azimuthal angle", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalxpos"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalxpos", "Final x position of photon on results plane", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalypos"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalypos", "Final y position of photon on results plane", &status);
    currTTYPE = TTYPE + intToString(historyColumns["pathcode"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "pathcode", "Code describing photon path through telescope", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalxdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalxdir", "Final x direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalydir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalydir", "Final y direction of photon", &status);
    currTTYPE = TTYPE + intToString(historyColumns["finalzdir"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "finalzdir", "Final z direction of photon", &status);
    if (param.m_resplaneonly) {
      currTTYPE = TTYPE + intToString(historyColumns["rowindex"]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "rowindex", "Row number in full event list", &status);
    } else {
      currTTYPE = TTYPE + intToString(historyColumns["resultsplane"]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "resultsplane", "Whether photon reached results plane", &status);
    }
  }
  std::string colCommBase = " position of photon at interaction ";
  std::string colComm;
  // This is only true if phisttype=full
  if (param.m_writeCoords) {
    for (int iInteraction = 1 ; iInteraction <= maxNumInteractions ; ++iInteraction) {
      
      // we're only recording 7 interactions, so I don't need to worry about padding with 0s
      currColName = "event";
      currColName.append(intToString(iInteraction));
      
      currColName.append("x");
      colComm = "x" + colCommBase + intToString(iInteraction);
      currTTYPE = TTYPE + intToString(historyColumns[currColName]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), currColName.c_str(), colComm.c_str(), &status);   // event1x, event2x, etc 
      
      currColName.replace(currColName.length()-1,1,"y");
      colComm = "y" + colCommBase + intToString(iInteraction);
      currTTYPE = TTYPE + intToString(historyColumns[currColName]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), currColName.c_str(), colComm.c_str(), &status);   // event1y, event2y, etc
      
      currColName.replace(currColName.length()-1,1,"z");
      colComm = "z" + colCommBase + intToString(iInteraction);
      currTTYPE = TTYPE + intToString(historyColumns[currColName]);
      fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), currColName.c_str(), colComm.c_str(), &status);   // event1z, event2z, etc
      
    }  // end-loop through interactions
    
    currTTYPE = TTYPE + intToString(historyColumns["priobjid"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "priobjid", "ID of primary mirror if impacted", &status);
    currTTYPE = TTYPE + intToString(historyColumns["secobjid"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "secobjid", "ID of secondary mirror if impacted", &status);
    currTTYPE = TTYPE + intToString(historyColumns["priincangle"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "priincangle", "Incident angle w.r.t. primary mirror", &status);
    currTTYPE = TTYPE + intToString(historyColumns["secincangle"]);
    fits_update_key_str(param.m_history_fp, currTTYPE.c_str(), "secincangle", "Incident angle w.r.t. secondary mirror", &status);
    
  } // end-if writing coords columns
  
  
} // end setupHistoryFile()




void setupPSFFile(Param & param) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  int fileExists = 0;                   // check if file already exists
  
  // -------------------------------------
  
  // check if file already exists
  fits_file_exists(param.m_outpsffile.c_str(), &fileExists, &status);
  checkForFITSError(status, "checking for existence of", param.m_outpsffile);
  
  // if file exists
  if (fileExists == 1) {
    
    // open file before I can delete it
    fits_open_file(&param.m_psf_fp, param.m_outpsffile.c_str(), READWRITE, &status);
    checkForFITSError(status, "opening", param.m_outpsffile);
    
    // if clobber was set, delete this file to make a new one
    if (headas_clobpar) {
      fits_delete_file(param.m_psf_fp, &status);
      checkForFITSError(status, "deleting an older version of", param.m_outpsffile);
    } else {
      // clobber wasn't set but the file already exists.  Throw an error.
      AH_THROW_RUNTIME("File already exists, but clobber was set to no.  Either delete the file or set clobber to yes.");
    }
    
  } else if (fileExists == -1) {
    AH_THROW_RUNTIME("input file name is not a disk file");
  }

  // now create it (we either deleted it if clobber was set, or it didn't exist)
  fits_create_file(&param.m_psf_fp, param.m_outpsffile.c_str(), &status);
  checkForFITSError(status, "creating", param.m_outpsffile);

  AH_INFO(ahlog::HIGH) << "Creating file: " << param.m_outpsffile << std::endl;
  
} // end setupPSFFile()



void setupEAFile(Param & param) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;                       // for cfitsio function calls
  int fileExists = 0;                   // check if file already exists
  
  // -------------------------------------
  
  // check if file already exists
  fits_file_exists(param.m_outeafile.c_str(), &fileExists, &status);
  checkForFITSError(status, "checking for existence of", param.m_outeafile);
  
  // if file exists
  if (fileExists == 1) {
    
    // open file before I can delete it
    fits_open_file(&param.m_ea_fp, param.m_outeafile.c_str(), READWRITE, &status);
    checkForFITSError(status, "opening", param.m_outeafile);
    
    // if clobber was set, delete this file to make a new one
    if (headas_clobpar) {
      fits_delete_file(param.m_ea_fp, &status);
      checkForFITSError(status, "deleting an older version of", param.m_outeafile);
    } else {
      // clobber wasn't set but the file already exists.  Throw an error.
      AH_THROW_RUNTIME("File already exists, but clobber was set to no.  Either delete the file or set clobber to yes.");
    }
    
  } else if (fileExists == -1) {
    AH_THROW_RUNTIME("input file name is not a disk file");
  }

  // now create it (we either deleted it if clobber was set, or it didn't exist)
  fits_create_file(&param.m_ea_fp, param.m_outeafile.c_str(), &status);
  checkForFITSError(status, "creating", param.m_outeafile);
  
  AH_INFO(ahlog::HIGH) << "Creating file: " << param.m_outeafile << std::endl;
  // +++ I should also resize .m_eef, etc in here
  
  
} // end setupEAFile()


double op_cos (double angle) { return std::cos(angle); }
double op_sin (double angle) { return std::sin(angle); }
double op_abs (double angle) { return std::abs(angle); }
double op_fmod360 (double angle) { return std::fmod(angle,360.); }


int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata) {
  int status = 0;

  /* Check arguments. Use stdio functions for reporting problems at this stage, since no other message streams are set up yet. */
  if (0 >= argc) { fprintf(stderr, "startUp: logic (programming) error; argc == %d; must be positive.\n", argc); status = 1; }
  if (0 == argv) { fprintf(stderr, "startUp: logic (programming) error; argv is null.\n"); status = 1; }
  else if (0 == *argv) { fprintf(stderr, "startUp: logic (programming) error; *argv is null.\n"); status = 1; }
  /* if (0 == tooltag) no problem; tooltag is optional. */
  if (0 == appdata) { fprintf(stderr, "startUp: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  if (0 == status) {
    /** Initialize the application data structure. */
    *appdata = heaapp_construct_appdata(argc, argv); /* TODO: add tooltag when heaapp has it. */
  }

  /** From here on, use I/O functions in appdata to report errors. These will function correctly even if
      all else fails below. */

  if (0 == status) {
    /** Connect ape. Note this does not actually initialize ape, but it connects code that will initialize ape.
        The ape initialization code will read standard parameters, including chatter, logfile, history and
        clobber and store them in the application data structure. */
    status = heaapp_ape_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect ape.\n");
  }

  if (0 == status) {
    /** Connect ahlog. Note this does not actually initialize ahlog, but it connects code that will initialize ahlog.
        The ahlog initialization code will pull the name of the tool, chatter and logfile parameters, etc., from the
        application data structure when it *does* run. */
    status = heaapp_ahlog_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect ahlog.\n");
  }

  if (0 == status) {
    /** Connect heautils. Note this does not actually initialize heautils, but it connects code that will initialize heautils.
        The heautils initialization code will pull history, clobber, etc., from the
        application data structure when it *does* run. */
    status = heaapp_heautils_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect heautils.\n");
  }

  if (0 == status) {
    /** Finally, run all the initialization codes that were connected above. */
    status = heaapp_initialize(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to initialize application.\n");
  }

  return status;
}

int shutDown(HeaAppData * appdata) {
  int status = 0;
  /* Check arguments. Use stdio functions (only) for reporting null appdata, since no other option. */
  if (0 == appdata) { fprintf(stderr, "shutDown: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  /* Do finalize operations. */
  if (0 == status) {
    /** This will shut down the libraries in reverse order that were started up in the startUp function. */
    status = heaapp_finalize(appdata);
    /* Report error using appdata IO, which is valid even if there was an error. */
    if (0 != status) { appdata->printerr("shutDown", "heaapp_finalize returned an error.\n"); }
  }
  return status;
}




// end addtogroup
/** @} */



/* Revision Log
 $Log: xrtraytrace.cxx,v $
 Revision 1.34  2017/01/17 15:07:20  mdutka
 Correcting bug running in IMAGE mode changed line 2414

 Revision 1.33  2017/01/13 13:45:19  mdutka
 Adding bug correction for final x and final y position

 Revision 1.32  2016/11/10 20:32:33  mdutka
 Xrtraytrace will now account for rotations in the telescop definition file

 Revision 1.31  2016/05/26 14:07:22  klrutkow
 added a comment about incremental logging

 Revision 1.30  2016/03/31 06:14:43  klrutkow
 fixed bug when checking 'source' and 'psrcfile' params, discovered when someone ran on Lion

 Revision 1.29  2016/03/25 17:51:03  klrutkow
 removed debugging output line

 Revision 1.28  2016/03/25 17:24:14  klrutkow
 implemented new way to output progress, only printing 10 messages to user on how many photons processed

 Revision 1.27  2015/11/02 17:32:50  klrutkow
 moved location of logging start

 Revision 1.26  2015/10/01 17:24:00  klrutkow
 added op_fmod360 to take mod 360 of abs of roll

 Revision 1.25  2015/09/23 13:12:22  klrutkow
 added some missing oneOverCosInputOffaxisTheta calculcations

 Revision 1.24  2015/09/15 17:21:07  klrutkow
 added writeParametersToLog ; use enum to check param source values instead of string comparisons throughout ; implemented new 'roll' mode, adding op_abs function ; implement new cosinputtheta algorithm for transmission through external objects

 Revision 1.23  2015/09/10 02:34:14  klrutkow
 add check that telescop and instrume params match keywords in all infiles ; add telescop and instrume params to readScatteringFile() ; log statements as each file is opened ; manually assign extensions if not provided ; added extra output line at end of run ; added column rowindex if resplaneonly=yes

 Revision 1.22  2015/08/18 04:31:10  klrutkow
 changed CCNM value to look for scattering file

 Revision 1.21  2015/08/17 03:39:39  klrutkow
 changed CALDB CCNM search terms for back refl and pcol refl

 Revision 1.20  2015/08/13 03:39:01  klrutkow
 shortened prologue (issue 534) ; change CALDB queries to use new resolve() function ; ensure that mirror, obstruct, and refl files have extension specified (commented out for now) ; add genTelescope as argument to writeHistoryFile2ndExtension ; edit TTYPEn (column name) keyword comments

 Revision 1.19  2015/06/29 15:26:51  klrutkow
 adding CALDB queries to getPar

 Revision 1.18  2015/06/18 17:54:40  klrutkow
 update param prompts; testing CALDB queries

 Revision 1.17  2015/04/30 23:39:33  klrutkow
 in doWork(), organized theta and roll variables with inputOffAxisThetaArcmin,   inputAzimuthDeg, initialThetaArcmin, initialAzimuthDeg, depending on if variable came from par file or was calculated for flatcircle or betamodel ; in doWork(), changed k and j loop variables to iTheta and iRoll ; in doWork(), updated psfExtNum when summing energies ; moved telescop, instrume, detnam function params (when writing extensions) to struct genTelescope ; adding more comments, documentation ; changed fits_get_col(TDOUBLE) to fits_get_col_dbl

 Revision 1.16  2015/03/23 14:37:10  klrutkow
 moved telescop, instrume, detnam function params to struct genTelescope

 Revision 1.15  2015/02/26 16:34:47  klrutkow
 updated top and bottomextobjectstranseslice loop variable

 Revision 1.14  2015/02/09 02:18:35  klrutkow
 corrected the value written to FGEOAREA keyword in history file

 Revision 1.13  2015/01/29 03:17:42  klrutkow
 updated with new params, per issue 472

 Revision 1.12  2015/01/29 03:06:00  klrutkow
 updated initial position calculation for groundmodel sources ; error checking: input offaxis angle must be less than 90 degrees

 Revision 1.11  2015/01/20 15:03:26  klrutkow
 updated EEF calculcation for offaxis

 Revision 1.10  2015/01/16 16:03:51  klrutkow
 updated VERSION keyword to v1.06 from v1.05; corrected psf extension numbering; added top and bottomextobjectstranseslice assignment before call to raytraceOnePhoton() if preinterpolateGrids=false and photonsfromfile=true; added new variable stats.numrpiDoubleReflPerEnergy for effective area calculation; size pathCoords, etc arrays in doWork instead of in raytraceOnePhoton();

 Revision 1.9  2014/12/29 17:20:00  klrutkow
 new params writeonlyresultsplane, phisttype, externalobjects, fastmode; removed param coordhist; fixed error with TDF, extension SEGMENT, columns DELTATX Y and Z: store in rad not arcmin; new structs to hold telescope information: sectorsShells, ExternalObjects, GenTelescope; added variables to existing structs (topextobjectstrans, etc); added keywords NOFFAXIS, NAZIMUTH, NORMRAD to PSF and EEF primary extensions; added an else to housing check, if photon is inside innerhousing radius; added second extension, INPUTPHOTONS, to history file; updated raytrace standard output

 Revision 1.8  2014/11/17 18:01:24  klrutkow
 created PSF_EEF and EA structs, moved vars from Photons struct; created enum psfType_e; removed zminsortxrtobjectindexAll (since we now use zmaxsort...)

 Revision 1.7  2014/11/07 15:24:50  klrutkow
 initialized all bools; updated offaxis and roll variable names to include units; updated extension number for PSF when energies are summed; moved housingHardLimit calculation from raytraceOnePhoton() to initialize(); implement heaapp for startup() and shutdown()

 Revision 1.6  2014/10/06 22:47:33  klrutkow
 updated code for xrtraytrace_suite.v1.03: add scattering implementation; changed misalignment implementation to remove random misalignment; allow user to go below inner housing radius; fix bugs in setupXRTTransform; fix keywords in output files for offaxis, psfcenter, fix offset for psf image, added 4 columns to end of history file; debugged betamodel, flatcircle, and photon list modes; if annulus and rectangle are turned off, aperture defaults to annulus

 Revision 1.5  2014/10/03 16:48:56  klrutkow
 code as of v1.02 Sept 11 2014: ensure NGROUPS matches between reflectivity files and TDF; update getPhotonObjectImpactCoords() to correctly hit top of foils, removed last 'killPhoton' flag, better calculate front and back boundaries, better calculate top and bottom boundaries; updated probability calculcations in raytraceonephoton() for absorption, transmission, or reflection; added ability in diagnosticmode to input initial x and y; small edit to updateEventCounters(); abort if user enters malign=gauss but wrong number of doubles for ftilt etc; added .clear() to refl and transm grids before bisectionInterp() in raytraceonephoton(); if numInteractions goes above maxNumPathCoords, stop that photon with an error code instead of aborting program

 Revision 1.4  2014/09/04 16:24:06  klrutkow
 updates for v1.01 delivery

 Revision 1.3  2014/08/12 18:19:31  klrutkow
 fixed error when converting from double to int

 Revision 1.2  2014/08/12 15:12:48  klrutkow
 added headas_utils.h to get headas params

*/
