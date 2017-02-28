/// \file raytrace_lib.h
/// \brief Declares the supporting functions for raytrace task
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2016/11/10 20:32:34 $
/// 
/// This file contains the declarations for the supporting functions for the 
/// raytrace calculations.  It also contains the data structures that the
/// code uses.

  
#ifndef RAYTRACE_RAYTRACE_LIB_H
#define RAYTRACE_RAYTRACE_LIB_H


#include "fitsio.h"           // cfitsio (functions, enums, READONLY, FLEN_CARD)
#include "headas_rand.h"      // random number generator (in heautils)

#include "ahlog/ahlog.h"      // Logging/messaging, st_stream

#include <algorithm>          // std::min_element, std::max_element, std::transform, std:fill
#include <cmath>              // std::fmod, std::cos, std::hypot, std::abs, pow, etc
#include <functional>         // std::multiplies, std::divides
#include <iomanip>            // std::setfill, std::setw, std::setprecision
#include <iostream>
#include <map>                // std::map
#include <sstream>            // to_string(), stringstream, <<
#include <string>             // std::string
#include <vector>             // std::vector


/* +++ uncomment this #define in order to print out all the statements that
   ensure that data was read and stored properly.  Each new loop starts with
   a statement "**** test that I've ...".  It might be a good idea to redirect 
   output to a log file ('./raytrace mode=h > xrtreftable-output.log') 
   because the output is quite long (for example, it prints all 4000 from the 
   angle grid).  */
//#define DEBUG



/** \addtogroup tool_raytrace
 * @{
 */


/// physical constants
static const double s_piovertwo       = 1.570796370506286;
static const double s_pi              = 3.141592653589793;
static const double s_threepiovertwo  = 4.712389111518859;
static const double s_twopi           = 6.283185482025146;
static const double s_sqrttwo         = 1.41421356237309514547;

/// converting angles between units
static const double s_radianToDegrees = 57.2957779186820488;
static const double s_degreesToRadian = 0.01745329300562540;
static const double s_radianToArcmin  = 3437.7466751209231006;
static const double s_arcminToRadian  = 2.908882167604234e-04;
static const double s_radianToArcsec  = 206264.8005072553933132;
static const double s_arcsecToRadian  = 4.848136946007058e-06;
static const double s_90DegInArcmin   = 5400.0;

/// converting misc
static const double s_eVTokeV         = 1e-03;
static const double s_angstromTomm    = 1e-07;
static const double s_mmTocm          = 1e-01;
static const double s_mabsToTau       = 1e-08;

/// extension names hardwired into code
static const std::string s_tdfPrecolExtName     = "COLLIMATOR";
static const std::string s_tdfSegExtName        = "SEGMENT";
static const std::string s_tdfSurfExtName       = "SURFACE";
static const std::string s_tdfExternalExtName   = "AZIMUTHALSTRUCT";
static const std::string s_massAbsExtName       = "MASS_ABSORPTION";
static const std::string s_historyExtName       = "PHOTONHISTORY";
static const std::string s_historyExtName2      = "INPUTPHOTONS";

/// multidimensional vectors
typedef std::vector<double>         vector1Ddbl;
typedef std::vector< vector1Ddbl >  vector2Ddbl;
typedef std::vector< vector2Ddbl >  vector3Ddbl;
typedef std::vector< vector3Ddbl >  vector4Ddbl ;
typedef std::vector< vector4Ddbl >  vector5Ddbl;
typedef std::vector< std::vector<long> > vector2Dlng;
typedef std::vector< std::vector<int> > vector2Dint;
typedef std::vector< vector2Dint > vector3Dint;
typedef std::vector< vector3Dint > vector4Dint;

/// miscellaneous enumerated lists
enum energyUnits_e {e_eV, e_keV};
enum misalignmentType_e {UNIFORM, GAUSS};
enum objectTypes_e {OBSTRUCTION, FOIL};

// There should be 6 misalignment flags in the parameter 'misalign'
static const size_t s_numMisalignmentFlags = 6;

/// OBSTRUCT : 0 for flag to ignore geometry
/// CYLINDER : 1 for precollimators
/// CONE     : 2 for mirror foils
enum surfaceGeometryTypes_e {OBSTRUCT, CYLINDER, CONE};

/// housing array has three elements:
/// PCOL      : array index 0
/// PRIMARY   : array index 1
/// SECONDARY : array index 2
/// there must be three elements, even if there is no pcol
static const int s_numHousings = 3;
enum housingSets_e {PCOL, PRIMARY, SECONDARY};

enum faces_e {BACK, FRONT};
enum housingSides_e {INNER, OUTER};

// enums to describe parameter values
enum psfType_e {PSF, EEF};
enum source_e {POINT, FLATCIRCLE, BETAMODEL, PHOTONLIST, GROUNDMODEL, DIAGNOSTICMODE};

/// applyXRTTransform() will tranform either a position, a direction, or both.
enum transformType_e {BOTH, POSITION, DIRECTION};

/// enum for pathCode vector entries.  See input params for raytraceOnePhoton 
/// for details on what each entry value means.
///  pathcode[0] = A  = interaction type
///  pathcode[1] = BC = impacted object
///  pathcode[2] = D  = impacted face
static const int s_numPathCodeInts = 3;    // A, BC, D
enum pathCodeEntries_e {A, BC, D};

/// Three groups/intervals of obstructions:
/// 0 ABOVE   = above pre-collimator, above and touching the primaries
/// 1 BETWEEN = between primaries and secondaries
/// 2 BELOW   = below the secondaries
static const int s_numObstrIntervals = 3;
enum obstructionInterval_e {ABOVE, BETWEEN, BELOW};

//+++ add enums to refer to indices in arrays (like geoParams, etc)



/**********************************************
 * ********************************************
 * 		structs
 * ********************************************
**********************************************/


/// \brief Input and derived parameters used in the raytrace task.
///
/// Param is a structure storing the input parameters and various
/// related/derived parameters needed in the raytrace task.
struct Param {
  
  /// default constructor
  Param() : 
    m_numphoton(0), 
    m_seed(0), 
    m_flatradius(0.0), 
    m_resultsplanez(0.0), 
    m_resplaneonly(false), 
    m_doThetaPhiPairs(false),
    m_pcolExists(false), 
    m_phistory(false), 
    m_writeCoords(false), 
    m_doExternalObjects(false), 
    m_doTopExternalObjects(false), 
    m_doBottomExternalObjects(false),
    m_doSuperFastMode(false),
    m_doFastMode(false),
    m_energiesfromreflectivity(false),
    m_searchenergymin(0.0), 
    m_searchenergymax(0.0), 
    m_history_fp(0), 
    m_psf_fp(0), 
    m_ea_fp(0), 
    m_errorOccurred(false) { }
  
  /// destructor
  ~Param() {
    m_history_fp = 0;
    m_psf_fp = 0;
    m_ea_fp = 0;
  }
   
  /// tool parameters, from .par file
  std::string m_mirrorfile;       ///< Input telescope description file for mirror (or CALDB)
  std::string m_obstructfile;     ///< Input telescope description file for obstructions (or CALDB)
  std::string m_frontreffile;     ///< Input reflectivity/transmission file for front-side (or CALDB)
  std::string m_backreffile;      ///< Input reflectivity/transmission file for back-side (or CALDB)
  std::string m_pcolreffile;      ///< Input reflectivity/transmission file for pre-collimator (or CALDB)
  std::string m_scatterfile;      ///< Input scattering file for reflecting surfaces (front, back, pre-collimator)
  
  int m_numphoton;                ///< Number of photons per energy per off-axis per roll position
  std::string m_energy;           ///< Input energies [keV] (see help file for options)
  
  int m_seed;                     ///< Random number generator seed (0=use system time)
  
  std::string m_misalign;         ///< Mirror foil tilt and twist misalignment flags
  
  std::string m_transmode;        ///< XRT components to treat with transmission (NONE, ALL, MIRROR, PCOL)
  std::string m_scattermode;      ///< XRT components to treat with scattering (NONE, ALL, MIRROR, PCOL)
  std::string m_source;           ///< Photon source spatial distribution
  std::string m_betapars;         ///< Photon beta pars: core radius [arcmin], beta, max radius [arcmin]
  double m_flatradius;            ///< Photon flat circle model source radius [arcmin]
  std::string m_psrcfile;         ///< Photon source file (if PHOTONLIST or GROUNDMODEL)
  std::string m_diagpars;         ///< Diagnostic mode parameters
  std::string m_offaxis;          ///< Photon source center off-axis angles [arcmin]
  std::string m_roll;             ///< Photon source center azimuthal (roll) angles [deg]
  std::string m_annulus;          ///< Annular aperture pars: min/max radius [mm]; min/max angle [deg]
  std::string m_rectangle;        ///< Rectangular aperature pars: x, y, x-width, y-height [mm]
  
  std::string m_outeafile;        ///< Output effective area (EA) file
  std::string m_outpsffile;       ///< Output PSF image or EEF file
  std::string m_psfpars;          ///< Output PSF parameters
  
  double m_resultsplanez;         ///< Z location of the results plane [mm]
  
  bool m_resplaneonly;            ///< Only write history of photons reaching the results plane (YES, NO)
  std::string m_outphistfile;     ///< Photon history file name
  std::string m_phisttype;        ///< Type of output photon history file (FULL, BASIC, BRIEF)
  std::string m_externobjects;    ///< External objects to include (NONE, ALL, TOP, BOTTOM)
  std::string m_fastmode;         ///< Use fast mode (YES, NO)
  
  std::string m_telescop;         ///< TELESCOP
  std::string m_instrume;         ///< INSTRUME
  std::string m_validdate;        ///< Validity date assigned to output files (yyyy-mm-dd)
  std::string m_validtime;        ///< Validity time assigned to output files (hh:mm:ss)
  
  /// standard parameters from .par file are handled by headas_start_up()
  /// (such as clobber, debug, etc)
  
  // ----------------------------------------------------
  /// DERIVED variables from the input string parameters:
  
  /// these string lists are converted to vectors
  vector1Ddbl m_energies;
  vector1Ddbl m_offaxisVec;
  vector1Ddbl m_rollVec;
  vector1Ddbl m_psfparsVec;
  
  // +++ should I put ALL derived vectors here?  Some are in photons (psf)
  
  /// derived variables from the input parameters:
  
  source_e m_sourceEnum;          ///< enumerated value version of source param
  
  bool m_doThetaPhiPairs;         ///< flag if code is to do theta,phi pairs instead of every phi for every theta
  
  bool m_pcolExists;              ///< flag if a precollimator file was provided
  bool m_phistory;                ///< flag if we're to write a history file  // +++ rename
  bool m_writeCoords;             ///< flag if we should write all impact coordinate columns to history file.  true if FULL history file
  
  bool m_doExternalObjects;       ///< flag if we will be treating any objects in AZIMUTHALSTRUCT extension of TDF
  bool m_doTopExternalObjects;    ///< flag if we will be treating any top objects in AZIMUTHALSTRUCT extension of TDF
  bool m_doBottomExternalObjects; ///< flag if we will be treating any bottom objects in AZIMUTHALSTRUCT extension of TDF
  
  /// The user can enter yes|no for fastmode
  /// no    = use interceptXRTObjects() inside raytraceOnePhoton()
  /// yes   = use getImpactCandidates() inside raytraceOnePhoton()
  //  super = use raytracePhotonFast().  This function is not fully tested, so 
  //          this option is not advertised at the moment. +++ 20141215
  bool m_doSuperFastMode;         ///< flag to use raytracePhotonFast().
  bool m_doFastMode;              ///< flag to use getImpactCandidates() rather than interceptXRTObjects().  used to be param 'fastmode'  +++20141215
  
  bool m_energiesfromreflectivity;  ///< flag if we're to get energy grid directly from reflectivity file
  double m_searchenergymin;         ///< if energies from refl file, min energy to grab
  double m_searchenergymax;         ///< if energies from refl file, max energy to grab
  
  /// output file pointers
  fitsfile * m_history_fp;
  fitsfile * m_psf_fp;
  fitsfile * m_ea_fp;
  
  /// flag to identify if an error was thrown
  bool m_errorOccurred;
  
};  // end struct Param


/// \brief Information about photon source
///
/// Photons is a structure holding information and data about the source of
/// photons to the raytrace tool, including the energies, off-axis and 
/// azimuthal angles of photon source, and numbers of photons.
struct Photons {
  
  /// default constructor
  Photons() :
    m_photonsFromFile(false), 
    m_energyislist(false), 
    m_nens(0),
    m_nphotons(0),
    m_numPhotonEnergies(0),
    m_isExtendedSource(false),
    m_minPhotonEnergy(0.0),
    m_maxPhotonEnergy(0.0),
    m_nthetain(0),
    m_nrollin(0), 
    m_totalNumPhotons(0), 
    m_photonPrintIncrement(0) { }
  
  /// destructor
  ~Photons() { }
  
  /// flag that input photons are read from a file vs created within code
  bool m_photonsFromFile;
  
  /// flag that the param 'energies' is a list or a filename
  // +++ is this appropriate here in Photons?
  bool m_energyislist;
  
  /// For input photon modes that do not involve reading an input file, 
  /// m_nens will be equal to the number of photon energies to be ray-traced; 
  /// but for a file input, this will be a dummy loop variable equal to 1 
  long m_nens;
  
  /// Number of photons.  If photons are from a file, this is the number of 
  /// rows in the file.  If the photons are not read from a file, 
  /// nphotons = numphoton, from the input .par file
  long m_nphotons;
  
  long m_numPhotonEnergies;      // +++ comment
  
  // if user input source = FLATCIRCLE or BETAMODEL, it's an extended source
  // The default is false (point source))
  bool m_isExtendedSource;
  
  /// grid of photon energies to use
  /// if photons are not from a file:
  ///   if param 'energy' is a filename: grid from file
  ///   if param 'energy' is a list: that list is the grid
  ///   if param 'energy' is a list and the first is negative: this isn't 
  ///               populated, energies are from the reflectivity file
  /// if photons are from a file: 
  ///   if unique energy list exists, this is that grid (either as file or list)
  /// this is grid onto which refl, trans, and opticaldepth/mm will be mapped
  /// dimen: (m_numPhotonEnergies)
  vector1Ddbl m_photonEnergies;
  
  /// min and max keep the size of reflectivity (etc.) grids under control
  double m_minPhotonEnergy;
  double m_maxPhotonEnergy;
  
  // if the photons are not from a file:
  
  /// theta: Photon source center off-axis angles
  int m_nthetain;                   ///< number of off-axis theta angles
  vector1Ddbl m_offaxisThetaArcmin; ///< array of off-axis angles, from .par (arcmin)
  vector1Ddbl m_offaxisThetaRad;    ///< array of off-axis angles, converted (rad)
  vector1Ddbl m_cosOffaxisTheta;
  vector1Ddbl m_sinOffaxisTheta;
  /// phi: Photon source center azimuthal (roll) angles
  int m_nrollin;                    ///< number of roll angles
  vector1Ddbl m_rollAnglesDeg;      ///< array of roll angles, from .par (deg)
  vector1Ddbl m_rollAnglesRad;      ///< array of roll angles, converted (rad)
  vector1Ddbl m_cosRollAngles;
  vector1Ddbl m_sinRollAngles;
  
  //+++ make these vector2Ddbl of <DirectionVector>
  vector2Ddbl m_xPtSrcInitPhotonDir;
  vector2Ddbl m_yPtSrcInitPhotonDir;
  vector2Ddbl m_zPtSrcInitPhotonDir;
  
  // +++ This is supposed to be the total number of photons, but I'm 
  //     missing the pairs of theta/roll
  // photons.m_nphotons * photons.m_nens * photons.m_nthetain * photons.m_nrollin;
  int m_totalNumPhotons;
  
  // Flag to see how many output messages to give user
  long int m_photonPrintIncrement;
  
};  // end struct Photons


/// \brief Information for the Point Spread Function (PSF) or 
///        Encircled Energy Function (EEF)
/// 
/// PSF_EEF is a struct containing variables necessary for creating the PSF 
/// files, both the EEF and the image.
struct PSF_EEF {
  
  /// default constructor
  PSF_EEF() : 
    m_psfType(PSF),
    m_numradeef(0), 
    m_delta_radeef(0.0), 
    m_numpsfxvals(0), 
    m_numpsfyvals(0), 
    m_psfImageCenterPixel(0),
    m_psfLowerXCorner(0.0), 
    m_psfLowerYCorner(0.0), 
    m_summedEEFPSF(false), 
    m_numEEFens(0) {}
  
  /// destructor
  ~PSF_EEF() { }
  
  
  // from input param psfpars, 1st number.  1=PSF, 2=EEF
  psfType_e m_psfType;
  
  // from input param psfpars, 2nd number. Number of PSF radial bins
  long m_numradeef;
  
  // from input param psfpars, 3rd number. Radial bin step size (arcsec)
  double m_delta_radeef;
  
  /// eef radial values
  /// dimen: (m_numradeef)
  vector1Ddbl m_radeef;
  
  long m_numpsfxvals;
  long m_numpsfyvals;
  
  long m_psfImageCenterPixel;
  
  double m_psfLowerXCorner;   // state units
  double m_psfLowerYCorner;   // 
  
  /// If there are more than 10 energies, the EEF or PSF will be averaged over 
  /// the entire energy grid
  bool m_summedEEFPSF;
  
  int m_numEEFens;
  
  /// The center of the source has a corresponding x,y position on the focal 
  /// plane and this x,y position (referred to as psfx,psfy) is the position of 
  /// the peak of the photon distribution on the results/focal plane. The 
  /// following, psfx and psfy, are the x and y coordinates respectively of the 
  /// position of the source center on the results/focal plane
  /// dimen: (nthetain,nrollin)
  vector2Ddbl m_psfx;
  vector2Ddbl m_psfy;
  
  /// the output PSF array: PSF value versus radial distance is required for 
  /// each off-axis angle, each roll angle, and each energy.
  /// dimen: (numpsfxvals,numpsfyvals)
  vector2Ddbl m_psfImage;
  
  /// The EEFs for each energy, off-axis angle, and azimuthal angle. For the 
  /// case that we have to create an EEF summed over all energies, the energy 
  /// index will always be 0 and this will be done by a conditional based on 
  /// the Boolean summedEEFPSF
  /// dimen: (numradeef,nrollin,nthetain,numEEFens)
  vector4Ddbl m_eef;          
  
};  // end struct PSF


/// \brief Effective Area information
struct EA {
  
  /// default constructor
  EA() {}
  
  /// destructor
  ~EA() { }
  
  
  /// An effective area vs. energy point is required for each off-axis angle, 
  /// each roll angle, and each energy.
  /// dimen: (nrollin,nthetain,m_numPhotonEnergies)
  vector3Ddbl m_ea;
  
  // +++ this is never used.  It's use was decided against (or forgotten), but 
  //     we're keeping it for now just in case, and for debugging
  /// the total number of photons per energy that impact the results plane 
  /// (usually the focal plane), from which the effective area is calculated.
  /// dimen: (nrollin,nthetain,nens)
  vector3Dint m_resultsplanephotons;
  
  // Following will count the photons for the effective area calculation
  /// dimen: (nrollin,nthetain,nens)
  vector3Dint m_effareaphotons;
  
};  // end struct EA


/// \brief Information about the reflectivity and transmission properties 
///
/// ReflectTransGrids is a structure holding information from the reflectivity
/// file (output from xrtreftable tool).  It includes the angle and energy 
/// grids for reflectivity and transmission for the front and backs of the
/// foils, including the precollimator, and the optical depth of the mirrors. 
/// The reflectivity and transmission grids are the probabilities that a ray 
/// is reflected or transmitted at that energy and angle for that group.
/// Includes transmission grids for top and bottom external objects, if they 
/// exist.
struct ReflectTransGrids {
  
  /// default constructor
  ReflectTransGrids() : 
    m_numMaterials(0), 
    m_numFrontEnergies(0), 
    m_numFrontAngles(0), 
    m_numBackEnergies(0), 
    m_backAngleStart(0.0), 
    m_backAngleDelta(0.0), 
    m_numBackAngles(0), 
    m_numPcolEnergies(0), 
    m_pcolAngleStart(0.0), 
    m_pcolAngleDelta(0.0), 
    m_numPcolAngles(0), 
    m_numRoughAngles(0), 
    m_numTopExtObjectParts(0), 
    m_numBottomExtObjectParts(0) { }
  
  /// destructor
  ~ReflectTransGrids() { }
  
  
  /// number of materials with mass absorption coefficients
  long m_numMaterials;
  
  /// Number of energies in energy grid for mirror front-side refl and trans
  long m_numFrontEnergies;
  /// Energy grid for the mirror front-side reflectivity and transmission
  vector1Ddbl m_frontReflEnergies;
  /// Number of incident angles for mirror front-side refl and trans
  long m_numFrontAngles;
  /// Incident angle grid for mirror front-side reflectivity and transmission
  vector1Ddbl m_frontAngles;
  /// Reflectivity grid corresponding for the front-side mirrors
  /// size: m_frontRefl[m_numFrontEnergies][m_numFrontAngles][nGroups]
  vector3Ddbl m_frontRefl;
  /// Thin film transmission grid corresponding for the front-side mirrors
  /// size: m_frontRefl[m_numFrontEnergies][m_numFrontAngles][nGroups]
  vector3Ddbl m_frontTran;
  
  /// Number of energies in energy grid for refl of back-side of mirror foils
  long m_numBackEnergies;
  /// Energy grid for the reflectivity of back-side of mirror foils
  vector1Ddbl m_backEnergies;
  /// start of angle grid for back-side of mirror foils
  double m_backAngleStart;
  /// step of angle grid for back-side of mirror foils
  double m_backAngleDelta;
  /// Number of incident angles for back-side of mirror foil reflectivity
  long m_numBackAngles;
  /// Incident angle grid for the back-side of mirror foil reflectivity
  vector1Ddbl m_backAngles;
  /// Reflectivity grid for the back-side of mirror foils
  /// size: m_backRefl[m_numBackEnergies][m_numBackAngles]
  vector2Ddbl m_backRefl;
  
  /// Number of energies in energy grid for refl of pre-collimator foils
  long m_numPcolEnergies;
  /// Energy grid for the reflectivity of pre-collimator foils
  vector1Ddbl m_pcolEnergies;
  /// start of angle grid for pre-collimator
  double m_pcolAngleStart;
  /// step of angle grid for pre-collimator
  double m_pcolAngleDelta;
  /// Number of incident angles for pre-collimator foil reflectivity
  long m_numPcolAngles;
  /// Incident angle grid for pre-collimator foil reflectivity
  vector1Ddbl m_pcolAngles;
  /// Reflectivity grid for pre-collimator foils
  /// size: m_backRefl[m_numPcolEnergies][m_numPcolAngles]
  vector2Ddbl m_pcolRefl;
  
  // numBackAngles should = numPcolAngles (kept independent for the 
  // possibility of generalizing later). The rough surface materials will be 
  // unified and will be a function of either of these angle grids
  long m_numRoughAngles;
  
  /// Thin-film optical depth per mm grid corresponding to the energy array for 
  /// the front-side mirrors, for each group of foils. Used to calculate the 
  /// transmission from the bottom to the top of the coated front-side mirror 
  /// surface. Note that the 0th element is reserved for the pre-collimator 
  /// (but is a dummy with optical depth=0) so the mirror elements start from 1
  /// size: m_frontTauPermm[m_numFrontEnergies][nGroups+1]
  vector2Ddbl m_frontTauPermm;
  
  /// Grid for optical depth per mm for foil bodies (i.e. not including the 
  /// front surface). The energy grid is the same as that for the front-side 
  /// reflectivity. Note that the 0th element is reserved for the precollimator 
  /// so the mirror elements start from 1.
  /// size: m_roughTauPermm[m_numFrontEnergies][nGroups+1]
  vector2Ddbl m_roughTauPermm;
  
  /// Mass absorption coefficients as a function of energy and material number
  /// [cm^2/g]
  /// size: m_massAbsCoeff[m_numFrontEnergies][nmaterials]
  vector2Ddbl m_massAbsCoeff;
  
  
  /// number of top and bottom external object parts.  Convenient to store 
  /// here, as well as in ExternalObjectsStructure, for calculating and 
  /// remapping the transmission probability.
  int m_numTopExtObjectParts;
  int m_numBottomExtObjectParts;
  
  // Transmission probabilities as a function of energy for the 
  // external objects such as thermal shield etc. The energy grid used for 
  // this is the same as the one specified for front-side reflectivity.
  // Transmission probability = thickness in mm x 
  //                            0.1 (to convert to cm) x 
  //                            density (g/cm^3) x 
  //                            mass-absorption coefficient (mabscoef), which is from ReadReflectTrans()
  /// size: [m_numFrontEnergies][m_numExtObjectParts]
  vector2Ddbl m_topExtObjectsTransProb;
  vector2Ddbl m_bottomExtObjectsTransProb;
  
};  // end struct ReflectTransGrids


/// \brief The reflectivity and transmission properties remapped onto new 
///        energy grid
///
/// RemappedReflectTransGrids holds the remapped reflectivity, transmission, 
/// and optical depth grids using a new energy grid.  This includes 
/// transmission for the top and bottom external objects, if they are present 
/// in the struct ExternalObjectsStructure.
// +++ add comments from remap function in trf
struct RemappedReflectTransGrids {
  
  /// default constructor
  RemappedReflectTransGrids() { }
  
  /// destructor
  ~RemappedReflectTransGrids() { }
  
  /// use numEnergies and Energies grid from Photons struct
  
  /// Reflectivity grid corresponding for the front-side mirrors
  /// size: m_frontRefl[m_numEnergies][m_numFrontAngles][nGroups]
  vector3Ddbl m_frontRefl;
  /// Thin film transmission grid corresponding for the front-side mirrors
  /// size: m_frontRefl[m_numEnergies][m_numFrontAngles][nGroups]
  vector3Ddbl m_frontTran;
  
  /// Thin-film optical depth per mm grid for the front-side mirrors. Used to 
  /// calculate transmission from the bottom to the top of the coated 
  /// front-side mirror surface.)
  /// size: m_frontTauPermm[m_numFrontEnergies][nGroups+1]
  vector2Ddbl m_frontTauPermm;
  
  /// Reflectivity grid corresponding for the rough surfaces
  /// size: m_frontRefl[m_numEnergies][m_numBackAngles][numRoughSurfaces]
  vector3Ddbl m_roughRefl;
  
  /// Grid for optical depth per mm for the rough-surface materials
  /// size: m_roughTauPermm[m_numFrontEnergies][nGroups+1]
  vector2Ddbl m_roughTauPermm;

  /// size: m_pcolRefl[numPhotonEnergies][numBackAngles]
  vector2Ddbl m_pcolRefl;
  
  /// size: numOutEnergies, numTopExtObjectParts
  vector2Ddbl m_topExtObjectTransProb;
  
  /// size: numOutEnergies, numbottomExtObjectParts
  vector2Ddbl m_bottomExtObjectTransProb;
  

};  // end struct RemappedReflectTransGrids


/// \brief Scattering information
///
/// Scattering holds all the information from the scattering file.  This 
/// consists of the scattering and incident angle grids for the front of the 
/// mirrors and the rough surfaces (back of mirror and precollimator), the 
/// column names in the file (which are referenced from the TDF), and the 
/// probability of scattering at a certain angle for each incident angle in 
/// the grid. The entire file, each cell in each extension, is stored in the 
/// five dimensional vector m_cumScatDist.
struct Scattering {
  
  /// default constructor
  Scattering() : 
    m_numSegments(0), 
    m_numColumns(0), m_numscatenergies(0), 
    m_firstScatteringAngle(0.0), 
    m_deltaScatteringAngle(0.0), 
    m_numScatteringAngles(0), 
    m_firstIncidentAngle(0.0), 
    m_deltaIncidentAngle(0.0), 
    m_numIncidentAngles(0), 
    m_roughScatID(0), 
    m_firstRoughScatteringAngle(0.0), 
    m_deltaRoughScatteringAngle(0.0), 
    m_numRoughScatteringAngles(0), 
    m_firstRoughIncidentAngle(0.0), 
    m_deltaRoughIncidentAngle(0.0), 
    m_numRoughIncidentAngles(0) { }
  
  /// destructor
  ~Scattering() { }
  
  
  /// number of extensions in the file (one per telescope segment)
  int m_numSegments;  
  
  /// how many columns in each extension. 
  /// dimen: (m_numSegments)
  std::vector<int> m_numColumns;
  /// Array holding the names of all the scattering probability data columns in 
  /// the input scattering file, organized by segment, where each segment 
  /// corresponds to a unique extension in the file.
  /// dimen: (m_numSegments, m_numColumns)
  std::vector< std::vector<std::string> > m_scatcolnames;
  
  //  ENERGY
  /// Number of tabulated energies in the Energy column. 
  long m_numscatenergies;
  /// Array of tabulated energies in the Energy column. For the moment it is 
  /// assumed that the ENERGY column in the 1st extension is the same as that 
  /// in every other extension (and therefore XRT segment).  The code does not 
  /// check for this.
  /// dimen: (numscatenergies)
  vector1Ddbl m_scatEnergies;
  
  //  SCATTERING ANGLES
  double m_firstScatteringAngle;
  double m_deltaScatteringAngle;
  /// Number of scattering angles in the front-side scattering angle grid
  long m_numScatteringAngles;
  /// Array of scattering angles for front-side mirror scattering prob. grid
  /// dimen: (m_numScatteringAngles)
  vector1Ddbl m_scatteringAngles;
  
  //  INCIDENT ANGLES
  /// First incident angle in front-side mirror incident angle grid (radians)
  double m_firstIncidentAngle;
  /// interval between incident grid points of front-side mirror incident 
  /// angles (radians)
  double m_deltaIncidentAngle;
  /// Number of front-side mirror incident angles
  long m_numIncidentAngles;
  /// Array of front surface incident angles for the scattering probability grid
  /// dimen: (m_numIncidentAngles)
  vector1Ddbl m_incidentAngles;
  
  /// ROUGH SURFACES (back and pcol)
  
  /// The index pointing to the column position holding the first back-side 
  /// mirror scattering, or the first pre-collimator scattering (whichever 
  /// comes first).  It is used to access the different angle grids associated 
  /// with rough surface scattering. A value of -1 indicates that arrays were 
  /// not found for back-side scattering or pre-collimator scattering.
  int m_roughScatID;
  
  /// store indices (not column numbers) of the back columns, for each extension
  /// dimen: (m_numSegments)
  std::vector<int> m_backColumnScatID;
  /// store indices (not column numbers) of the pcol columns, for each extension
  /// dimen: (m_numSegments)
  std::vector<int> m_pcolColumnScatID;
  
  //  ROUGH SCATTERING ANGLES
  double m_firstRoughScatteringAngle;
  double m_deltaRoughScatteringAngle;
  /// Number of scattering angles in the rough material scattering angle grid
  int m_numRoughScatteringAngles;
  /// Array of scattering angles for rough material scattering probability grid
  /// dimen: (m_numRoughScatteringAngles)
  vector1Ddbl m_roughScatteringAngles;
  
  //  ROUGH INCIDENT ANGLES
  /// First incident angle in the rough surface incident angle grid (radians)
  double m_firstRoughIncidentAngle;
  /// The interval between rough material incident grid points (radians)
  double m_deltaRoughIncidentAngle;
  /// Number of rough-surface incident angles
  int m_numRoughIncidentAngles;
  /// Array of rough surface incident angles for the scattering probability grid
  /// dimen: (m_numRoughIncidentAngles)
  vector1Ddbl m_roughIncidentAngles;
  
  /// PROBABILITY DATA
  
  /// Cumulative scattering probability for front-side mirror surfaces as well 
  /// as back-side and pre-collimators, as a function of segment id, column in 
  /// the scattering file, photon energy, scattering angle bin, and incident 
  /// angle bin. The column numbering in the input scattering file begins after 
  /// the Energy column, so a value of 0 for the second index in cumscatprob 
  /// refers to column number 2 in the FITS file. The probability is a 
  /// cumulative probability because the value of cumscatprob at a given value 
  /// of the scattering angle, is the probability of scattering through any 
  /// angle from the minimum tabulated up to that value of the scattering 
  /// angle. This allows easy inversion to generate a scattering angle from a 
  /// random number generated from a uniform distribution.
  /// size: (numsegments, numcolumns, numenergies, numscatteringangles, numincidentangles)
  ///   numsegments is the number of extensions, not counting the primary extension
  ///   numcolumns does not count the energy column
  ///   numenergies is how many rows in the file
  ///   numscatteringangles is the first number in TDIMn for this column
  ///   numincidentangles is the second number in TDIMn for this column
  /// Remember that the vector is 0-based.  For example, SXT has 4 segments in 
  /// the telescope, so there are 4 extensions after the primary extension.  So 
  /// the first dimension of m_cumScatDist has size 4.  But m_cumScatDist[0] 
  /// refers to the first extension, to SEGMENT001.  Similar numbering applies 
  /// to all the other dimension
  vector5Ddbl m_cumScatDist;
  
  /// Indices to scattering probability arrays that point to the first non-zero 
  /// probability value for a given segment, column, and incident angle.
  /// dimen: (numsegments, numcolumns, numscatenergies, numincidentangles)
  vector4Dint m_firstNonZeroScatProb;
  
  /// Indices to scattering probability arrays that point to the last non-zero 
  /// probability value for a given segment, column, and incident angle.
  /// dimen: (numsegments, numcolumns, numscatenergies, numincidentangles)
  vector4Dint m_lastNonZeroScatProb;
  
};  // end struct Scattering


/// \brief Counters for various outcomes of ray-tracing more than one photon
struct StatResults {
  
  /// default constructor
  StatResults() :
    numresultsplaneimpacts(0), 
    numrpiPerEnergy(0), 
    numphwitherror(0), 
    numrpiphwitherror(0), 
    numrpiPrimaryRefl(0), 
    numrpiSecondaryRefl(0), 
    numrpiDoubleRefl(0), 
    numrpiDoubleReflPerEnergy(0),
    numrpiPriSecRefl(0), 
    numrpiMoreThanTwoRefl(0), 
    numrpiNoFrontRefl(0), 
    numrpiPreColReflNoMirror(0), 
    numrpiPreColReflOneMirror(0), 
    numrpiPreColReflTwoMirror(0), 
    numrpiPreColReflMoreThanTwoMirror(0), 
    numrpiBackSideOneRefl(0), 
    numrpiBackSideMoreThanOne(0), 
    numrpiScatter(0), 
    numrpiFrntMScatter(0), 
    numTransmission(0), 
    numPreColTransmission(0), 
    numMirrorTransmission(0), 
    numrpiTransmission(0), 
    numrpiPreColTransmission(0), 
    numrpiMirrorTransmission(0), 
    numTotalAbsorbed(0), 
    numPreColEdgeImpact(0), 
    numPreColEdgeImpactAbsorbed(0), 
    numMirrorEdgeImpact(0), 
    numEdgeMirrorAbsorbed(0), 
    numPreColFaceAbsorbed(0), 
    numFrontMirrorAbsorbed(0), 
    numBackMirrorAbsorbed(0), 
    numObstructionAbsorbed(0), 
    numSectorWallAbsorbed(0), 
    numInnerHousingAbsorbed(0), 
    numOuterHousingAbsorbed(0),
    numAllResultsPlaneImpacts(0), 
    numrpiNoScatter(0) { }
  
  
  /// destructor
  ~StatResults() { }
  
  long numresultsplaneimpacts;      ///< Number of photons that impacted the results plane
  long numrpiPerEnergy;             ///< Number of photons that impacted the results plane for a given energy (this counter is set to zero at the start of each new unique energy is selected, in order to calculate the effective area as a function of energy)
  long numphwitherror;              ///< Number of photons that resulted in an error condition (non-zero error), out of all photons input
  long numrpiphwitherror;           ///<  Number of results plane photon impacts with a non-zero error code
  long numrpiPrimaryRefl;           ///< Number of results-plane photons that incurred a front-side primary-mirror reflection and no secondary-mirror reflection, no pre-collimator reflection
  long numrpiSecondaryRefl;         ///< Number of results-plane photons that incurred a front-side secondary-mirror reflection and no primary-mirror reflection, no pre-collimator reflection
  /// NOTE: number of front-side mirror single reflections is the sum of the previous two counters
  long numrpiDoubleRefl;            ///< Number of front-side double reflections (exactly one primary and one secondary), regardless of scattering or pre-collimator reflection, will be used for effective area calculation.  It is also used in the MNEFAREA keyword in the history file.  This value is reset for each theta and azimuthal angle in doWork().
  
  // +++
  long numrpiDoubleReflPerEnergy;   ///< Number of front-side double reflections (exactly one primary and one secondary), regardless of scattering or pre-collimator reflection, will be used for effective area calculation.  This value is reset for each energy, theta, and azimuthal angle in doWork().
  
  long numrpiPriSecRefl;            ///< Number of results-plane photons that incurred exactly 1 primary and 1 secondary mirror reflection, no pre-col reflection
  long numrpiMoreThanTwoRefl;       ///< Number of results-plane photons that underwent more than two front-side mirror reflections, no pre-collimator reflection
  long numrpiNoFrontRefl;           ///< Number of results plane photons that underwent no front-side mirror reflections
  long numrpiPreColReflNoMirror;    ///< Number of results-plane photons that underwent at least one pre-collimator reflection and no mirror reflections
  long numrpiPreColReflOneMirror;   ///< Number of results-plane photons that had at least one pre-collimator reflection and only one front-side mirror reflection (either primary or secondary)
  long numrpiPreColReflTwoMirror;   ///< Number of results-plane photons that had at least one pre-collimator reflection and exactly two front-side mirror reflections
  long numrpiPreColReflMoreThanTwoMirror;  ///< Number of results-plane photons that had at least one pre-collimator reflection and more than two front-side mirror reflections
  long numrpiBackSideOneRefl;       ///< Number of results-plane photons that underwent exactly one reflection on the back-side of a mirror foil (regardless of other interactions)
  long numrpiBackSideMoreThanOne;   ///< Number of results-plane photons that underwent more than one reflection on the back-side of a mirror foil (regardless of other interactions)
  long numrpiScatter;               ///< Number of results-plane photons that underwent scattering after at least one reflection
  long numrpiFrntMScatter;          ///< Number of results-plane photons that underwent scattering after a front-side mirror reflection
  long numTransmission;             ///< Number of photons that underwent at least one transmission event
  long numPreColTransmission;       ///< Number of photons that underwent at least one pre-collimator transmission event
  long numMirrorTransmission;       ///< Number of photons that underwent at least one mirror transmission event
  long numrpiTransmission;          ///< Number of results-plane photons that underwent at least one transmission event
  long numrpiPreColTransmission;    ///< Number of results-plane photons that underwent at least one pre-collimator transmission event
  long numrpiMirrorTransmission;    ///< Number of results-plane photons that underwent at least one mirror transmission event
  long numTotalAbsorbed;            ///< Total number of photons absorbed before reaching the results plane
  long numPreColEdgeImpact;         ///< Number of photons that impacted one of the edges of a pre-collimator foil
  long numPreColEdgeImpactAbsorbed; ///< Number of photons that were absorbed after impacting one of the edges of a pre-collimator foil
  long numMirrorEdgeImpact;         ///< Number of photons that impacted one of the edges of a mirror foil
  long numEdgeMirrorAbsorbed;       ///< Number of photons that were absorbed after impacting one of the edges of a mirror foil
  long numPreColFaceAbsorbed;       ///< Number of photons absorbed after impacting a pre-collimator front or back face
  long numFrontMirrorAbsorbed;      ///< Number of photons absorbed after impacting the front-side of a mirror foil
  long numBackMirrorAbsorbed;       ///< Number of photons absorbed after impacting the back-side of a mirror foil
  long numObstructionAbsorbed;      ///< Number of photons absorbed by a support structure (obstruction)
  long numSectorWallAbsorbed;       ///< Number of photons absorbed by a sector side wall
  long numInnerHousingAbsorbed;     ///< Number of photons absorbed by an inner housing unit
  long numOuterHousingAbsorbed;     ///< Number of photons absorbed by an outer housing unit
  long numAllResultsPlaneImpacts;   ///< Number of results plane photon impacts regardless of error condition
  long numrpiNoScatter;             ///< Number of results-plane photons that underwent no scattering
  
};


/// \brief struct representing the three values in a Cartesian coordinate
struct CartesianCoord {
  
  /// default constructor
  CartesianCoord() :
    m_x(0.), m_y(0.), m_z(0.) { }
  
  /// constructor
  CartesianCoord(double x, double y, double z) :
    m_x(x), m_y(y), m_z(z) { }
  
  /// destructor
//  ~CartesianCoord() { }
  
  double m_x;            ///< x-component of Cartesian coordinate
  double m_y;            ///< y-component of Cartesian coordinate
  double m_z;            ///< z-component of Cartesian coordinate
  
};  // end struct CartesianCoord


/// struct representing the three values (x, y, and z) in a Direction vector
/// The direction vector (“velocity”) should be normalized to unity if we want 
/// to equate time and distance variables without another constant.
struct DirectionVector {
  
  /// default constructor
  DirectionVector() :
    m_xDir(0.), m_yDir(0.), m_zDir(0.) { }
  
  /// constructor
  DirectionVector(double x, double y, double z) :
    m_xDir(x), m_yDir(y), m_zDir(z) { }
  
  /// destructor
//  ~DirectionVector() { }
  
  double m_xDir;            ///< x-component of direction vector
  double m_yDir;            ///< y-component of direction vector
  double m_zDir;            ///< z-component of direction vector
  
};  // end struct DirectionVector


/// in an array of Misalignment structs, there should be two dimensions:
///     [0] for pre-collimator foils
///     [1] for mirror foils
struct Misalignment {
  
  //static const int m_numTelescopeSets = 3;
  
  /// default constructor
  Misalignment() { 
    m_tiltSwitch.resize(s_numHousings);
    m_twistSwitch.resize(s_numHousings);
    m_tiltPivot.resize(s_numHousings);
  }
  
  /// destructor
  ~Misalignment() { }
  
  std::vector<bool> m_tiltSwitch;   ///< 
  std::vector<bool> m_twistSwitch;  ///< 
  std::vector<double> m_tiltPivot;  ///< 
  
};  // end struct Misalignment


/// Based on input parameters annulus and rectangle, store the properties of 
/// the telescope aperture.
struct Aperture {
  
  /// default constructor
  Aperture() : 
    m_ztelescopeaperture(0.0), 
    m_isAnnulus(false),
    m_rMin(0.0), 
    m_rMax(0.0), 
    m_rMinSq(0.0), 
    m_rMaxSq(0.0), 
    m_minAngleDeg(0.0), 
    m_minAngleRad(0.0), 
    m_maxAngleDeg(0.0), 
    m_maxAngleRad(0.0), 
    m_deltaAngleRad(0.0), 
    m_xWidth(0.0), 
    m_yWidth(0.0), 
    m_xMin(0.0), 
    m_yMin(0.0),
    m_geometricAreaSqmm(0.0), 
    m_geometricAreaSqcm(0.0), 
    m_fullGeometricAreaSqmm(0.0), 
    m_effAreaFactor(0.0) { }
  
  /// destructor
  ~Aperture() { }
  
  /// z level of the aperture
  double m_ztelescopeaperture;
  
  /// The aperture is either an annulus or a rectangle
  bool m_isAnnulus;
  
  /// if the aperture is an annulus:
  double m_rMin;
  double m_rMax;
  double m_rMinSq;
  double m_rMaxSq;
  double m_minAngleDeg;
  double m_minAngleRad;
  double m_maxAngleDeg;
  double m_maxAngleRad;
  double m_deltaAngleRad;   ///< central angle enclosed by the annulus radial boundaries
  
  /// if the aperture is a rectangle:
  double m_xWidth;
  double m_yWidth;
  double m_xMin;
  double m_yMin;
  
  double m_geometricAreaSqmm;     ///< aperture geometric area in sq mm
  double m_geometricAreaSqcm;     ///< aperture geometric area in sq cm
  double m_fullGeometricAreaSqmm; ///< Full telescope geometric area in sq mm
  
  /// convert the number of photons that impact the results plane for a given energy into an effective area
  double m_effAreaFactor;       // geometricareacmsq / photons.m_nphotons
  
};  // end struct Aperture


/// struct containing general and miscellaneous telescope paramters, such as 
/// TELESCO keyword, the inner housing radius, number of central external 
/// object parts, etc.
struct GenTelescope {
  
  /// default constructor
  GenTelescope() : 
    m_focalLength(0.0),
    m_telfprot(0.0),
    m_telfprotRadians(0.0),
    m_costelfprot(0.0),
    m_sintelfprot(0.0),
    m_innerhousingradius(0.0), 
    m_outerhousingradius(0.0), 
    m_holewalltopz(0.0), 
    m_holesideinnerradius(0.0),
    m_numcentralextobjectparts(0) { }
  
  /// destructor
  ~GenTelescope() { }
  
  std::string m_telescop;             ///< TELESCOP keyword in TDF
  std::string m_instrume;             ///< INSTRUME keyword in TDF
  std::string m_detnam;               ///< DETNAM keyword in TDF
  // TELESCOP, INSTRUME keywords must match between: 
  //    - input param file
  //    - TDF file(s)
  //    - reflectivity file(s)
  //    - scattering file (if scattering != none)
  
  double m_focalLength;               ///< FOCALLEN keyword in TDF [mm]
  double m_telfprot;                  ///< TELFPROT keyword in TDF [degrees]
  double m_telfprotRadians;           ///< TELFPROT in radians [radians]
  double m_costelfprot;               ///< cosine of TELFPROT
  double m_sintelfprot;               ///< sin of TELFPROT
  double m_innerhousingradius;        ///< [mm] 
  double m_outerhousingradius;        ///< [mm] 
  double m_holewalltopz;              ///< inner radius [0]
  double m_holesideinnerradius;       ///< inner radius [0]
  int m_numcentralextobjectparts;      ///< inner radius [0]
  
};  // end struct GenTelescope


/// \brief Data about how the foils are spaced, including sector numbers and 
///        gap boundaries
///
/// SectorsShells holds data about the spacing and alignment of the foils.  
/// This includes the radial bounds of the gaps between shells, and the angular 
/// boundaries enclosing sectors in one layer of the telescope foils
struct SectorsShells {
  
  
  /// Thesedetermine how wide a net to cast as we search for 
  /// potential impacts between a photon and the telescope.  These are not 
  /// controlled by the user.  The numbers may change with testing to find the 
  /// most efficient values.
  static const int s_deltaShells  = 1;
  static const int s_deltaSectors = 1;
  
  /// default constructor
  SectorsShells() : 
    m_numShells(0), 
    m_numSectors(0), 
    m_sectorsPerSegment(0), 
    m_numSegments(0), 
    m_numGaps(0), 
    m_numgapradialboundaries(0), 
    m_nummirrorsectorangleboundaries(0), 
    m_zerosectorid(0), 
    m_numApertureSectors(0),  
    m_numLoopSectors((2 * s_deltaSectors) + 1) { }
  
  /// destructor
  ~SectorsShells() { }
  
  /// Number of foil shells in the telescope (maximum value in the NUMBER 
  /// column of the MIRROR or COLLIMATOR extension in the TDF)
  int m_numShells;
  
  /// Total number of sectors in the telescope.  Differs from sectorspersegment 
  /// in that m_sectorsPerSegment is a number unique only to a segment whereas 
  /// m_numSectors is counts all the sectors in the telescope.  Numbering 
  /// starts at 1 for the primary mirrors, then counts the secondary, then the 
  /// precollimator.  This number is the max XRTObject.m_sectorID
  int m_numSectors;
  
  
  /// maximum value in the SECTORNUMBER column of the MIRROR extension in the 
  /// TDF. Note that the SECTORNUMBER column value is different to the 
  /// xrtobjectsectorid() array because the former is a number unique only to a 
  /// segment whereas the latter is a number unique to the entire telescope
  int m_sectorsPerSegment;
  
  /// Number of telescope segments per "layer" of the telescope (i.e. per set of primaries, or per set of secondaries, or per set of pre-collimators).  This is the maximum value in the SEGMENT column in the SEGMENT extension of the TDF. Hitomi HXT has 3 segments, and SXT has 4.  This also corresponds to how many extensions are in the scattering file (one extension per segment).
  int m_numSegments;
  
  /// 
  int m_numGaps;
  
  /// The number of boundaries enclosing gaps in the mirror shells. The number 
  /// includes the inner and outer housing radii as gap boundaries.
  int m_numgapradialboundaries;
  
  /// The radial boundaries enclosing gaps in the mirror shells. The number 
  /// includes the inner and outer housing radii as gap boundaries.
  vector1Ddbl m_gapradialboundaries;
  
  /// The number of boundaries enclosing sectors in one layer of the telescope 
  /// foils (i.e. it is the number of primary mirror sectors, or the number of 
  /// secondary mirror sectors).
  int m_nummirrorsectorangleboundaries;
  
  /// The angular boundaries enclosing sectors in one layer of the telescope 
  /// foils (i.e. it is the number of primary mirror sectors, or the number of 
  /// secondary mirror sectors).
  vector1Ddbl m_mirrorsectorangleboundaries;
  
  /// The id number of the sector that crosses the positive x-axis (i.e. the 
  /// sector whose boundaries include 0 degrees)
  int m_zerosectorid;
  
  /// number of sectors in the aperture layer of foils (but it should be the 
  /// same for all layers- it is really the number of sectors per layer)
  int m_numApertureSectors;
  
  /// For a given objectset (0=p-col, 1=primaries, 2=secondaries), sector 
  /// number (1 to # sectors per segment x #segments), and shell number, the 
  /// array returns the XRT object id.  This is used in raytraceonphoton(), and 
  /// for getImpactCandidates().
  /// This is different to xrtobject2regionid because this does not refer to 
  /// segment number.  If we want bisectionlocate to find the sector 
  /// corresponding to a particular azimuthal angle we must have a continuous 
  /// sector numbering between 0 and 360 degrees
  /// dimen: (s_numHousings or set number, numApertureSectors, numShells)
  vector3Dint m_setsectorshell2xrtobjectid;
  
  /// Number of sectors that will be searched for potential photon-object 
  /// impacts, by the routine getimpactcandidates().  This number depends on 
  /// s_deltaSectors.
  int m_numLoopSectors;
  
  /// loopSectorNumbers is not in SectorsShells because it changes with every 
  /// loop iteration in doWork, but the size is always m_numLoopSectors, which 
  /// is constant.
  
  
};  // end struct SectorsShells


/// \brief Struct representing an obstruction in the telescope
struct Obstruction {
  
  /// default constructor
  Obstruction() :
    m_set(0), m_idNumber(0), m_zCoord(0.0), m_numVertices(0) { }
  
  /// destructor
  ~Obstruction() { }
  
  /// data filled from TDF OBSTRUCT extension
  int m_set;              ///< column LAYER (ie, 1-4)
  int m_idNumber;         ///< column POLYNUM (ie, 1-126)
  double m_zCoord;        ///< column DISTANCE currently, each set is a certain
                          ///  distance from the bottom, and only 2D is 
                          ///  supported, so each obstruction has one zcoord
  std::vector<CartesianCoord> m_vertices; ///< columns XVERTEX,YVERTEX,DISTANCE
                          /// the vertices must be in order around the 
                          /// obstruction.  Since only 2D obstructions are 
                          /// supported, all vertices have the same zcoord.
  int m_numVertices;      ///< number of vertices per obstruction
  
};  // end struct Obstruction


/// \brief The bounding coordinations of an XRTObject
struct ObjectBoundingBox {
  
  /// default constructor
  ObjectBoundingBox() : 
    m_xMin(0.), m_xMax(0.), 
    m_yMin(0.), m_yMax(0.), 
    m_zMin(0.), m_zMax(0.) { }
  
  /// destructor
  ~ObjectBoundingBox() { }
  
  double m_xMin;      ///< minimum x coord of bounding box
  double m_xMax;      ///< maximum x coord of bounding box
  double m_yMin;      ///< minimum y coord of bounding box
  double m_yMax;      ///< maximum y coord of bounding box
  double m_zMin;      ///< minimum z coord of bounding box
  double m_zMax;      ///< maximum z coord of bounding box
  
  //+++ add a vector of coords for the vertices?
  
};  // end struct ObjectBoundingBox


struct PhotonBoundingBox {
  
  /// default constructor
  PhotonBoundingBox() : 
    m_xMin(0.), m_xMax(0.), 
    m_yMin(0.), m_yMax(0.), 
    m_zMin(0.), m_zMax(0.), 
    m_rMin(0.), m_rMax(0.) { }
  
  /// destructor
  ~PhotonBoundingBox() { }
  
  double m_xMin;      ///< minimum x coord of bounding box
  double m_xMax;      ///< maximum x coord of bounding box
  double m_yMin;      ///< minimum y coord of bounding box
  double m_yMax;      ///< maximum y coord of bounding box
  double m_zMin;      ///< minimum z coord of bounding box
  double m_zMax;      ///< maximum z coord of bounding box
  double m_rMin;      ///< minimum radial distance of the photon path in the x-y 
                      ///  plane from the origin (i.e. closest distance of any 
                      ///  bounding box corner to the z-axis)
  double m_rMax;      ///< maximum radial distance of the photon path in the x-y 
                      ///  plane from the origin (i.e. largest distance of any 
                      ///  bounding box corner to the z-axis)
  
  //+++ add a vector of coords for the vertices?
  
};  // end struct PhotonBoundingBox


/// Quantities related to the slopes (and their squares) of XRT objects, if 
/// relevant. For cone-shaped foils the slope is kc, where the equation of the 
/// cone is z^2 = kc^2 (x^2+y^2). The front surface and back surface of an XRT 
/// foil are treated as two cones that can have their own parameters
struct Slopes {
  
  /// default constructor
  Slopes() : 
    m_back(0.), m_front(0.), m_backSq(0.), m_frontSq(0.) { }
  
  /// destructor
  ~Slopes() { }
  
  double m_back;           ///< slope of back side
  double m_front;          ///< slope of front side
  double m_backSq;         ///< slope of back side squared
  double m_frontSq;        ///< slope of front side squared
  
}; // end struct Slopes


/// Various geometrical parameters and some pre-calculated derived quantities 
/// (in the interest of speed) describing an XRT object. The members are 
/// calculated in xrtsetup(). For mirror and pre-collimator foils (currently 
/// not relevant for obstructions).
//+++ get rid of this?  Tahir wanted an array instead
struct GeoParams {
  
  /// default constructor
  GeoParams() : 
    m_zTop(0.), m_zBottom(0.), m_zBottomSq(0.),
    m_rBottomInner(0.), m_rBottomOuter(0.), 
    m_rBottomInnerSq(0.), m_rBottomOuterSq(0.), 
    m_zBottomrBottomInner(0.), m_zBottomrBottomOuter(0.),
    m_rTopInner(0.), m_rTopOuter(0.), m_rTopInnerSq(0.), m_rTopOuterSq(0.), 
    m_slopeBackrBottomOuter(0.), m_slopeFrontrBottomInner(0.), 
    m_zBottomShiftBack(0.), m_zBottomShiftFront(0.), 
    m_zTopShiftBack(0.), m_zTopShiftFront(0.) { }
  
  /// destructor
  ~GeoParams() { }
  
  double m_zTop;                    ///< 00 z-coordinate of the top of the object
  double m_zBottom;                 ///< 01 z-coordinate of the bottom of the object
  double m_zBottomSq;               ///< 02 z-coordinate of the bottom of the object, squared
  double m_rBottomInner;            ///< 03 Inner bottom radius
  double m_rBottomOuter;            ///< 04 Outer bottom radius
  double m_rBottomInnerSq;          ///< 05 Inner bottom radius squared
  double m_rBottomOuterSq;          ///< 06 Outer bottom radius squared
  double m_zBottomrBottomInner;     ///< 07 Bottom z-coordinate times bottom inner radius
  double m_zBottomrBottomOuter;     ///< 07 Bottom z-coordinate times bottom outer radius
  double m_rTopInner;               ///< 09 Top inner radius
  double m_rTopOuter;               ///< 10 Top outer radius
  double m_rTopInnerSq;             ///< 11 Top inner radius squared
  double m_rTopOuterSq;             ///< 12 Top outer radius squared
  double m_slopeBackrBottomOuter;   ///< 13 Back slope times outer bottom radius
  double m_slopeFrontrBottomInner;  ///< 14 Front slope times inner bottom radius
  double m_zBottomShiftBack;        ///< 15 Bottom z-coordinate minus m_slopeBackrBottomOuter (for shift to standard position)
  double m_zBottomShiftFront;       ///< 16 Bottom z-coordinate minus m_slopeFrontrBottomInner (for shift to standard position)
  double m_zTopShiftBack;           ///< 17 Top z-coordinate minus m_zBottomShiftBack
  double m_zTopShiftFront;          ///< 18 Top z-coordinate minus m_zBottomShiftFront
  
}; // end struct GeoParams


/// in an array of HousingGeometry objects, there should be three dimensions:
///     [0] for pre-collimator
///     [1] for primary mirrors
///     [2] for secondary mirrors
/// This is irrespective to if there actually is a precollimator.
struct HousingGeometry {
  
  /// default constructor
  HousingGeometry() : 
    m_rInner(0.0),   m_rInnerSq(0.0), 
    m_rOuter(0.0),   m_rOuterSq(0.0),
    m_zLower(0.0),   m_zUpper(0.0),
    m_xShift(0.0),   m_yShift(0.0) { }
  
  /// destructor
  ~HousingGeometry() { }
  
  double m_rInner;      ///< [mm] inner radius [0]
  double m_rInnerSq;    ///< inner radius squared
  double m_rOuter;      ///< [mm] outer radius [1]
  double m_rOuterSq;    ///< outer radius squared
  double m_zLower;      ///< [mm] lower z-distance from focal plane [2]
  double m_zUpper;      ///< [mm] upper z-distance from focal plane [3]
  double m_xShift;      ///< [mm] x-coordinate shift of center [4]
  double m_yShift;      ///< [mm] y-coordinate shift of center [5]
  
}; // end struct HousingGeometry


/// \brief Struct holding information about coordinate transformations
struct Transforms {
  
  /// default constructor
  Transforms() : 
    m_doTransforms(true) { }
  
  /// destructor
  ~Transforms() { }
  
  /// number of numerical values to completely describe net shifts and 
  /// rotations of XRT objects
  static const int m_maxTransformXYZ = 14;
  
  // +++ 12 or 14
  
  /// Set up arrays that will hold coordinate transformations for each XRT 
  /// object that are appropriate for a number of situations as described 
  /// below. In each case, the transformations consist of 12 numbers that 
  /// describe a net translational shift and a net rotation.
  /// In the following, "XRT frame" refers to the frame of reference in which 
  /// the results plane (by default, the focal plane) contains the x and y 
  /// Cartesian coordinate axes, and the z-axis coincides with central axis of 
  /// the cylindrical housing units and foil arrangements, when no 
  /// translational or rotational offsets are applied. The "object" frame 
  /// refers to a frame of reference in which a given XRT object is not shifted 
  /// or rotated. Therefore, each XRT object is associated with a unique 
  /// transformation that transforms the position and direction of a photon 
  /// from the XRT frame to the object frame. A further transformation can be 
  /// applied such that the object will appear to be in a "standard position" 
  /// from the point of view of the photon (for example the cone that gives 
  /// rise to a conical foil surface could be placed with its apex at the 
  /// origin of coordinates, and its axis coinciding with the z-axis).
  /// The intercept points for a ray interecting an XRT object are found by 
  /// transforming the photon into the object frame first. The interaction 
  /// (e.g. reflection, transmission, and scattering) is treated in the object 
  /// frame, and the resulting photon is transformed back into the XRT frame. 
  /// These transformations considerably reduce the burden on run-time of the 
  /// main ray-tracing loop because they significantly simplify the math, so 
  /// the transformations are critical for keeping the run-time low.
  
  /// flag whether or not to apply transformations.  It allows all 
  /// misalignments, shifts, and rotational offsets of XRT objects to be turned 
  /// off (i.e. no coordinate transformations will be applied, and the 
  /// components of the telescope will all be at their default (design) 
  /// positions and will have their default dimensions)
  /// for diagnostic purposes only, set internally (not controllable by the 
  /// general user)
  bool m_doTransforms;
  
  /// All of the transformation arrays have a similar format, as follows: 
  /// array(*,0:2) = 1st row of rotation matrix
  /// array(*,3:5) = 2nd row of rotation matrix
  /// array(*,6:8) = 3rd row of rotation matrix
  /// array(*,9)   = shift parallel to x-axis
  /// array(*,10)  = shift parallel to y-axis
  /// array(*,11)  = shift parallel to z-axis;

  /// arrays to hold coordinate transformations for each XRT object
  /// populated in xrtsetup, used in raytraceonephoton
  /// dimen [][]
  ///  xrt frame to object frame in TDF (default) position
  vector2Ddbl m_xrt2objectframe;
  ///  object frame in TDF (default) position to xrt frame
  vector2Ddbl m_object2xrtframe;
  ///  xrt frame to sector sidewall frame
  vector2Ddbl m_xrt2sectorframe;
  ///  sector sidewall frame to xrt frame
  vector2Ddbl m_sector2xrtframe;
  
};  // end struct Transforms


/// \brief Struct holding all information describing an individual XRTObject
///
/// An XRTObject is a part of the telescope, either a mirror or precollimator 
/// foil or an obstruction.  The struct hold the geometric data about that 
/// XRTObject, including indices indicating in which part of the telescope this 
/// object sits, any misalignments this object has, and reflectivity properties.
struct XRTObject {
  
  // +++ overload the << operator so I can print an object
  
  //+++ is this acceptable?? (static)
  static const int m_numSideWalls = 6;
  static const int m_numGeoParams = 19;
  
  /// default constructor
  XRTObject() { 
    m_objectIdx = 0;
    m_type = FOIL;
    m_set = 0;
    m_geometry = CYLINDER;
    m_reflIndexFront = 0;
    m_reflIndexBack = 0;
    m_doScattering = false;
    m_scatterIndexBack = 0;
    m_scatterIndexFront = 0; 
    m_startAngle = 0.0; 
    m_endAngle = 0.0;
    m_zBotTimesrInnerBot = 0.0;
    m_zBotTimesrOuterBot = 0.0;
    m_numVertices = 0;
    m_sectorID = 0;
    m_sectorNumber = 0;
    m_segmentID = 0;
    m_shellNumber = 0; 
    m_interval = ABOVE;
    m_rSectorShift = 0.0;
    m_tiltAngle = 0.0;
    m_twistAngle = 0.0;
    m_xSegmentShift = 0.0;
    m_ySegmentShift = 0.0;
    m_zSegmentShift = 0.0;
    m_xSegmentRot = 0.0;
    m_ySegmentRot = 0.0;
    m_zSegmentRot = 0.0;
    m_xHousingShift = 0.0;
    m_yHousingShift = 0.0;
    m_lowerRadialBound = 0.0;
    m_upperRadialBound = 0.0;
    m_sideWalls.resize(m_numSideWalls);
    m_geoParams.resize(m_numGeoParams);
    m_doTransmission = false;
    m_tauPermmIndexBack = 0;
    m_tauPermmIndexFront = 0;
  }
  
  /// destructor
  ~XRTObject() { }
  
  // +++ get comments for all this from the top of xrtsetup() in TRF (and top of raytraceonephoton?)
  
  // this is for internal reference
  // it's the index of this current object in the XRTObjects vector
  // this is zero-based, so it will range from 0 to numXRTObjects-1
  int m_objectIdx;
  
  
  objectTypes_e m_type;     ///< OBSTRUCTION (0) or FOIL (1)
  
  /// if OBSTRUCT: 1,2,3... for groups of obstructions at similar distances 
  ///              along optical axis, with 1 being the set farthest from the 
  ///              optical axis.
  /// if FOIL: 
  ///     0 = precol
  ///     1 = primary mirror
  ///     2 = secondary mirror
  /// -1 = This XRTObject is 'turned off': don't consider it in raytracing.
  ///      This is useful for creating files for testing the ray-tracing code 
  ///      and can be done by changing the value of LAYER in the relevant row 
  ///      or rows of the LAYER column in the MIRROR, COLLIMATOR, or OBSTRUCT 
  ///      extensions.
  int m_set;                
                            
  
  /// functional form of the surface of an XRT object; will tell the 
  /// ray-tracing code which routines to call to evaluate ray-object 
  /// interaction. For mirror and pre-collimator foils the value will 
  /// eventually be read from the TDF column "FUNCTION" but that value does 
  /// not directly correspond to the value of here, which must also account 
  /// for obstructions. 
  /// Currently we set xrtobjectgeometry()=value of “FUNCTION” column +2.
  /// Then, we should have for xrtobjectgeometry
  ///     0 = obstruction
  ///     1 = cylindrical foil (pre-collimator)
  ///     2 = foil whose surface is part of a cone
  ///     3+ ... additional functions can be defined for future/other missions
  surfaceGeometryTypes_e m_geometry;

  
  /// For the thin-film front surfaces of mirror foils, the refl index points 
  /// to the correct group number in the reflectivity array frontrefl, and is 
  /// used to access the correct thin-film transmission array (up to the top 
  /// of the substrate).  For rough surfaces, the index points to the correct 
  /// slice of the reflectivity array roughrefinterp (see call to 
  /// remapReflectTrans).  Note that for obstructions both indices are zero 
  /// and for pre-collimator foils both indices are the same.
  int m_reflIndexFront;
  int m_reflIndexBack;
  
  
  /// Flag whether to treat scattering for an XRT object, based on the input 
  /// parameter scattermode.
  bool m_doScattering;
  /// used to access the correct scattering probability distribution for each 
  /// xrt object, appropriate for which face of the object was impacted by a 
  /// photon. These are not used for obstructions.
  /// The scattering probability arrays are set-up by the routine 
  /// setupxrtscattering() and are: 
  ///     totscatprob(index, incident angle bin) (total scattering probability)
  ///     scatterdistribution(index, incident angle bin, scattering angle bin)
  int m_scatterIndexBack;   // pathfacehit[numinteractions] == 0
  int m_scatterIndexFront;  // pathfacehit[numinteractions] == 1
  
  /// geometrical parameters of foils.  
  /// Some of the quantities are directly from the TDF, others are derived in 
  /// order to reduce run time. The entries for obstructions are given zeros 
  /// and are not used.
  
  double m_startAngle;
  double m_endAngle;
  
  /// product of z-coordinate of bottom of a foil and inner bottom radius
  double m_zBotTimesrInnerBot;
  /// product of z-coordinate of bottom of a foil and outer bottom radius
  double m_zBotTimesrOuterBot;
  
  /// vertices of xrtobjects
  /// vector size 8 is needed to represent a 6-sided 3-D object
  //+++ why have numVertices?  Why isn't it always 8?
  //+++ move this down by the BBox?
  std::vector<CartesianCoord> m_vertices;
  int m_numVertices;
  
  /// The ID of the sector for this XRTObject.  This is ranges the entire 
  /// telescope and does not reset for each segment.  It identifies a sector 
  /// with a single number without having to refer to segment or set.  For 
  /// example, in Hitomi, the SXT has 8 sectors per segment, 4 segments, and 3 
  /// sets, so this number will be between 1 and 96.
  /// Not used for obstructions.
  int m_sectorID;
  
  /// Sector number straight from TRF, MIRROR or COLLIMATOR extension, 
  /// SECTORNUMBER column.  This number resets at each segment, and tells to 
  /// which sector within a segment this XRTObject belongs.  For example, in 
  /// Hitomi, the SXT has 8 sectors per segment, so this number will be between 
  /// 1 and 8.
  /// Not used for obstructions.
  int m_sectorNumber;   
  
  /// The segment in which this XRTObject resides.  For example, in Hitomi, the 
  /// SXT has 4 segments, so this number will be between 1 and 4.
  /// Not used for obstructions.
  int m_segmentID;
  
  /// Shell identification number that an XRT object belongs to if it is of 
  /// type foil (the value is just 0 for obstructions). The shell id number 
  /// makes it easy to locate the two foils immediately adjacent in radius to 
  /// (i.e. surround) a given foil.
  int m_shellNumber;
  
  /// Denotes in which 'interval' this obstruction belongs.   
  /// There are three (s_numObstrIntervals) groups/intervals of obstructions:
  /// 0 ABOVE   = above pre-collimator, above and touching the primaries
  /// 1 BETWEEN = between primaries and secondaries
  /// 2 BELOW   = below the secondaries
  /// These are set in the enum obstructionInterval_e {ABOVE, BETWEEN, BELOW}
  /// Not used for foils, only for obstructions
  // in trf: obstructioninterval
  obstructionInterval_e m_interval;
  
  /// sector radial shift appropriate for the XRT object 
  /// Not used for obstructions.
  double m_rSectorShift;
  
  /// actual tilt and twist angles for each XRT object, in arcsec
  /// (not used for obstructions)
  /// a systematic part is obtained from information in the TDF, to which a 
  /// random part is added from information in the input parameter file.
  double m_tiltAngle;
  double m_twistAngle;
  
  /// coordinate translational offsets in the x,y,z directions that apply to 
  /// entire segments (values will be from the SEGMENT extension in the TDF).
  double m_xSegmentShift;
  double m_ySegmentShift;
  double m_zSegmentShift;
  
  /// rotational offsets around the x,y,z axes that apply to entire segments 
  /// (values from the SEGMENT extension in the TDF).
  double m_xSegmentRot;
  double m_ySegmentRot;
  double m_zSegmentRot;
  
  /// "housing" shifts and rotations apply to entire groups of xrtobjects. 
  /// Note that the primary and secondary mirror units are each associated 
  /// with two layers of obstructions.
  /// The rotations are simple rotations about each fixed cartesian axis, just as for segments.
  /// For the time being, we will likely only be using *shiftx and *shifty.
  /// Note that these simply use information in housinggeometry(3,6), 
  /// assigning appropriate values according to the particular XRT object. 
  /// The z-shift and the rotations are all commented out because they are 
  /// not used at the moment but could be in the future.
  double m_xHousingShift;
  double m_yHousingShift;
  
  /// a BoundingBox holds the 6 parameters for bounding boxes for each XRT 
  /// object. The bounding boxes will have sides that are parallel to the 
  /// Cartesian coordinate axes and the 6 parameters will be the mininum and 
  /// maximum values of x, y, and z, in that order. These bounding boxes 
  /// correspond to xrt objects with their default positions and no 
  /// translational or rotational offsets applied. Note that for 2-D 
  /// obstructions the bounding boxes have equal minimum and maximum values of 
  /// the z-coordinate so the boxes are also 2-D.
  ObjectBoundingBox m_BBox;
  /// bounding boxes for each XRT object WITH all translational and 
  /// rotational transformations applied. Note that the sides of the bounding 
  /// boxes are still parallel to the coordinate axes but the boxes completely 
  /// contain the transformed XRT objects.
  ObjectBoundingBox m_BBoxTrnsfrmd;
  /// The following will hold a radial lower and upper bound for each XRT 
  /// object that correspond to the radius of the adjacent shell of foils 
  /// closer to and further from the optical axis respectively. They will be 
  /// used in conjunction with the bounding boxes to quickly eliminate 
  /// possible XRT objects that cannot be intercepted by a given photon.
  
  /// The following will hold a radial lower and upper bound for each XRT 
  /// object that correspond to the radius of the adjacent shell of foils 
  /// closer to and further from the optical axis respectively. They will be 
  /// used in conjunction with the bounding boxes to quickly eliminate 
  /// possible XRT objects that cannot be intercepted by a given photon
  double m_lowerRadialBound;      ///< 0 outer radius of the adjacent foil closest to the telescope axis
  double m_upperRadialBound;      ///< 1 inner radius of the adjacent foil furthest from the telescope axis
  //+++ radian bounds inside bbox?
  
  /// Label for coordinate quadrant occupied by XRT object. Indicates which 
  /// Cartesian coordinate quadrant or quadrants an xrt object occupies. 
  ///   [0] is the start quadrant number
  ///   [1] is the end quadrant number
  ///   [2] is the number of quadrants occupied
  /// Note that currently this array is only filled for mirror and 
  /// pre-collimator foils, not obstructions.
  std::vector<int> m_coordQuads;
  
  /// Boundaries of sector sidewalls.  dimen: 6 (set above)
  /// For each XRT object that lies in a sector (i.e. a mirror foil or 
  /// pre-collimator foil, not an obstruction), angular boundary information 
  /// about the sector is as follows: 
  ///   [0] is the sector start angle in radians
  ///   [1] is the sector end angle in radians
  ///   [2] is the tangent of the start angle
  ///   [3] is the tangent of the end angle
  ///   [4] is abs(start angle) mod 2pi
  ///   [5] is abs(end angle) mod 2pi
  /// The tangents and mods are pre-calculated before the main ray-tracing loop 
  /// in the interest of runtime speed.
  /// For sidewalls that can be crossed (i.e. virtual) or for XRT objects for 
  /// which sidewalls are not relevant, the start and/or end angles have a 
  /// negative value.
  vector1Ddbl m_sideWalls;
  // +++ why not make a struct for sideWalls?  then could have a flag 'virtual'
  
  //+++ add comment
  Slopes m_slopes;
  
  //+++ add long comment
  //+++ create enums for these indices
  /// NOTE: (Per TRF) In general, different mirror geometries will not be 
  /// characterized by the same parameters. To retain generality, the data 
  /// structure m_geoParams should not have named members that are specific to 
  /// these particular parameters. This enables future telescopes to use the 
  /// same vector, but have each member refer to a different physical quantity.
  /// Definitions for Suzaku and Hitomi telescopes (foil refers to mirror or 
  /// pre-collimator foil):
  /// m_geoParams[0]  = z-coordinate of the top edge of a foil
  /// m_geoParams[1]  = z-coordinate of the bottom edge of a foil
  /// m_geoParams[2]  = square of z-coordinate of the bottom edge of a foil
  /// m_geoParams[3]  = radial distance of bottom of inner foil surface to central axis
  /// m_geoParams[4]  = radial distance of bottom of outer foil surface to central axis
  /// m_geoParams[5]  = square of inner-bottom foil radius
  /// m_geoParams[6]  = square of outer-bottom foil radius
  /// m_geoParams[7]  = product of the z-coordinate of the bottom of a foil and the inner bottom radiu
  /// m_geoParams[8]  = product of the z-coordinate of the bottom of a foil and the inner bottom radius //+++ outer ?
  /// m_geoParams[9]  = radial distance of top of inner foil surface to central axis
  /// m_geoParams[10] = radial distance of top of outer foil surface to central axis
  /// m_geoParams[11] = square of top-inner foil radius
  /// m_geoParams[12] = square of top-outer foil radius
  /// m_geoParams[13] = slope of back foil surface times bottom outer radius
  /// m_geoParams[14] = slope of front foil surface times bottom inner radius
  /// m_geoParams[15] = foil bottom z-coordinate minus xrtobjectgeoparams(*,13)
  /// m_geoParams[16] = foil bottom z-coordinate minus xrtobjectgeoparams(*,14)
  /// m_geoParams[17] = foil top z-coordinate minus xrtobjectgeoparams(*,15)
  /// m_geoParams[18] = foil top z-coordinate minus xrtobjectgeoparams(*,16)
  vector1Ddbl m_geoParams;
  
  /// Flag whether to treat transmission for an XRT object based on the input 
  /// parameter transmode which can have values none/all/mirrors/pcol.
  bool m_doTransmission;
  
  /// Pointers to optical depth per mm (functions of energy) of XRT objects 
  /// needed for transmission calculations.  Index maps this object to the 
  /// correct tauPermm (optical depth per mm for various materials or 
  /// combinations of materials of telescope components, as a function of 
  /// energy) in:
  ///     reflectTransGrids.m_frontTauPermm and 
  ///     reflectTransGrids.m_roughTauPermm.
  /// The optical depth is required to calculate 
  /// transmission through "rough" materials (as opposed to thin-films). The 
  /// thin-film, front-surface transmission is already calculated and stored 
  /// in the reflectvity/transmission file. 
  ///   m_tauPermmIndexBack points to the optical depth of photons impacting 
  ///     the back of the object, emerging from the front
  ///   m_tauPermmIndexFront points to the optical depth for photons impacting 
  ///     the front of the object, emerging from the back. 
  /// In most cases the two optical depths are the same but for mirror foils 
  /// the optical depth for the thin-film on the front side, impacted from 
  /// the front is not included in roughtaupermm. It is, however, included in 
  /// roughtaupermm for photons impacting the back side.
  /// Note that the optical depth array roughtaupermm is created by the 
  /// readReflectTrans() routine.
  /// Different groups may have different thicknesses, so each taupermm must be 
  /// calculated separately.
  int m_tauPermmIndexBack;    ///< back to front: xrtobjecttaupermmindex(i,0)
  int m_tauPermmIndexFront;   ///< front to back: xrtobjecttaupermmindex(i,1)
  
};  // end struct XRTObject


/// \brief Struct containing aggregate data for an external object.
///
/// It is intended that there will be two of these structs: one for the top, 
/// above the telescope, and one for below the telescope.
/// Any external objects that are not above or below the telescope are ignored 
/// and not included.  This data comes from the AZIMUTHALSTRUCT extension of 
/// the TDF, gathered from the struct TDF.
struct ExternalObjectsStructure {
  
  /// default constructor
  ExternalObjectsStructure() :
    m_numExtObjectParts(0), 
    m_doExternalObjects(false),
    m_zCoord(0.0), 
    m_maxMabsIndex(0) { }
  
  /// destructor
  ~ExternalObjectsStructure() { }
  
  /// These vectors are data from the AZIMUTHALSTRUCT extension of the TDF, 
  /// gathered from the struct TDF_External. Each vector will be the same size.  
  /// dimen: m_numExtObjectParts
  std::vector<int> m_object;           ///< object id number
  std::vector<int> m_subObject;        ///< sub-object (part number)
  std::vector<double> m_rMin;          ///< inner radius of object component or part [mm]
  std::vector<double> m_rMax;          ///< outer radius of object component or part [mm]
  std::vector<double> m_startAngleDeg; ///< start angle of object component or part [deg]
  std::vector<double> m_endAngleDeg;   ///< end angle of object component or part [deg]
  std::vector<double> m_startAngleRad; ///< start angle of object component or part [rad]
  std::vector<double> m_endAngleRad;   ///< end angle of object component or part [rad]
  std::vector<double> m_zMin;          ///< minimum z-coord of object component or part [mm]
  std::vector<double> m_zMax;          ///< maximum z-coord of object component or part [mm]
  std::vector<double> m_thickness;     ///< thickness in z-direction of object component or part [mm]
  std::vector<double> m_density;       ///< density [g/cm^3]
  std::vector<int> m_mabsIndex;        ///< mass-absorption index of object component or part
  
  /// NOTES:
  /// (1) z-axis is perpendicular to focal plane and z=0 on focal plane
  /// (2) mass-absorption index points to the cardinal number of the material 
  ///     for which mass-absorption values exist in the 2nd extension of the 
  ///     reflectivity file created by xrtreftable. It is equal to the column 
  ///     number less 1 because 1 is the energy.
  
  /// number of external object parts in this ExternalObjectsStructure 
  /// (top or bottom)
  /// This number is found by going through each row in the AZIMUTHALSTRUCT 
  /// extension and determining which object parts lie either above or below 
  /// the main telescope structure.
  int m_numExtObjectParts;
  // +++ trf: numtopextobjectparts, numbottomextobjectparts
  
  ///< flag if we will be treating any of these (top or bottom) objects from AZIMUTHALSTRUCT extension of TDF
  bool m_doExternalObjects;
  
  /// z-coord closest to telescope
  /// For a TOP ExternalObjectsStructure, this is the lowest z-coord of all 
  ///   external components above main telescope (e.g. thermal shield top)
  /// For a BOTTOM ExternalObjectsStructure, this is the lowest z-coord of all 
  ///   external components below main telescope (e.g. thermal shield bottom)
  double m_zCoord;
  // +++ trf: extobjectstopzcoord, extobjectsbottomzcoord
  
  /// largest value of mabsindex, to double check agains the reflectivity file 
  /// that there are indeed the required number of columns for mass absorption 
  /// coefficients
  int m_maxMabsIndex;
  
};  // end struct ExternalObjectsStructure


// structure containing data from the MIRROR extension of the TRF
struct TDF_Foil {
  
  /// default constructor
  TDF_Foil() : 
    m_numRows(0), 
    m_maxSegment(0), 
    m_sectorsPerSegment(0), 
    m_numberofshells(0) { }
  
  /// destructor
  ~TDF_Foil() { }
  
  /// \brief columns from TDF file extension (MIRROR or COLLIMATOR)
  std::vector<long> m_set;                 ///< LAYER
  std::vector<long> m_segment;             ///< ASSEMBLY
  std::vector<long> m_number;              ///< NUMBER
  std::vector<long> m_sectorNumber;        ///< SECTORNUMBER
  std::vector<double> m_sectorShift;       ///< SECTORSHIFT
  std::vector<double> m_sysTilt;           ///< SYSTILT
  std::vector<double> m_sysTwist;          ///< SYSTWIST
  std::vector<long> m_function;            ///< FUNCTION
  std::vector<std::string> m_scatter;      ///< SCATTER
  std::vector<std::string> m_fRelfect;     ///< FREFLECT
  std::vector<std::string> m_bRelfect;     ///< BREFLECT
  std::vector<double> m_startAngle;        ///< FSTART
  std::vector<double> m_endAngle;          ///< FEND
  std::vector<double> m_topInnerRadius;    ///< TOPINR
  std::vector<double> m_topOuterRadius;    ///< TOPOUTR
  std::vector<double> m_botInnerRadius;    ///< BOTINR
  std::vector<double> m_botOuterRadius;    ///< BOTOUTR
  std::vector<double> m_topZ;              ///< TOPD
  std::vector<double> m_botZ;              ///< BOTD
  //+++ the bools for scross and ecross had "warning: taking address of temporary" when compiled
  std::vector<bool> m_startCross;         ///< SCROSS
  std::vector<bool> m_endCross;           ///< ECROSS
  
  long m_numRows;           ///< there is 1 row per smallest mirror component
  int m_maxSegment;         ///< max segment id number of any segment
  int m_sectorsPerSegment;  ///< max value in the SECTORNUMBER column of the MIRROR extension in the TDF. 
  int m_numberofshells;     ///< maximum value in the NUMBER column of the MIRROR extension in the TDF
  
  /// pre-calc trig quantities in the interest of run-time speed for ray-tracing
  std::vector<double> m_tanStartAngle;
  std::vector<double> m_tanEndAngle;

}; // end struct TDF_Foil


// structure containing data from the OBSTRUCT extension of the TRF
struct TDF_Obstruct {

  /// default constructor
  TDF_Obstruct() : 
    m_numRows(0), 
    m_numObst(0), 
    m_numObstGrps(0), 
    m_obstructions3d(0) { }
  
  /// destructor
  ~TDF_Obstruct() { }
  
  
  /// \brief columns from TDF file extension OBSTRUCT
  std::vector<long> m_set;              ///< LAYER group num of support structures same z distance
  std::vector<long> m_idNumber;         ///< POLYNUM polygon to which this row (vertex) belongs
  std::vector<double> m_zCoord;         ///< DISTANCE z distance (mm) from focal plane
  std::vector<double> m_xVertex;        ///< XVERTEX
  std::vector<double> m_yVertex;        ///< YVERTEX
  
  long m_numRows;   ///< total num vertices in OBSTRUCT ext (one vertex per row)
  
  // Number of unique IDs associated with groups of support structures 
  // (i.e. groups characterized by a similar distance of its members from the 
  // focal plane).
  long m_numObst;       ///< Number of unique obstructions in ext.
  long m_numObstGrps;

  int m_obstructions3d;                ///< Number of 3D obstructions
  
  std::vector<int> m_verticesPerObst;  ///< num vertices for each obstruction
  
  std::vector<long> m_idSortedIdx;     ///< index to sorted m_idNumber
  std::vector<long> m_grpSortedIdx;    ///< index to sorted m_set
  
};  // end struct TDF_obstruct


// structure containing data from the SEGMENT extension of the TRF
struct TDF_Segment {

  /// default constructor
  TDF_Segment() : 
    m_numRows(0), 
    m_maxSegment(0), 
    m_minLayer(0), 
    m_maxLayer(0), 
    m_sumofsegmentoffsets(0.0) { }
  
  /// destructor
  ~TDF_Segment() { }
  
  
  /// \brief columns from TDF file extension (SEGMENT)
  std::vector<long> m_segment;        ///< SEGMENT 
  std::vector<double> m_startAngle;   ///< STARTANGLE [rad]
  std::vector<double> m_endAngle;     ///< ENDANGLE [rad]
  std::vector<long> m_layer;          ///< LAYER 0=pcol, 1=primaries, 2=secondaries
  std::vector<double> m_deltax;       ///< DELTAX offset parallel to the Cartesian x-axis [mm]
  std::vector<double> m_deltay;       ///< DELTAY offset parallel to the Cartesian y-axis [mm]
  std::vector<double> m_deltaz;       ///< DELTAZ offset parallel to the Cartesian z-axis [mm]
  std::vector<double> m_deltatxArcmin;///< DELTATX rotational offset about the x-axis [arcmin]
  std::vector<double> m_deltatyArcmin;///< DELTATY rotational offset about the y-axis [arcmin]
  std::vector<double> m_deltatzArcmin;///< DELTATZ rotational offset about the z-axis [arcmin]
  std::vector<double> m_deltatxRad;   ///< DELTATX rotational offset about the x-axis [rad]
  std::vector<double> m_deltatyRad;   ///< DELTATY rotational offset about the y-axis [rad]
  std::vector<double> m_deltatzRad;   ///< DELTATZ rotational offset about the z-axis [rad]
  /// Each of the three rotations is around a fixed axis; e.g. the DELTATY 
  /// rotation is around the original y-axis, not the new one that was obtained 
  /// after rotating around the x-axis
  /// the DELTATX, DELTATY, DELTATZ columns are in arcmin in the TDF, and 
  /// converted to radian here to be used throughout the code
  
  long m_numRows;   ///< total num segments (rows) in SEGMENT ext
  
  int m_minSegment; ///< min segment number
  int m_maxSegment; ///< max segment number
  int m_minLayer;   ///< min layer number
  int m_maxLayer;   ///< max layer number
  
  // reorganize shifts and rotations into 2-D arrays
  vector2Ddbl m_xShift;
  vector2Ddbl m_yShift;
  vector2Ddbl m_zShift;
  vector2Ddbl m_xRotation;
  vector2Ddbl m_yRotation;
  vector2Ddbl m_zRotation;
  
  double m_sumofsegmentoffsets;
  
};  // end struct TDF_segment


// structure containing data from the SURFACE extension of the TRF
struct TDF_Surface {

  /// default constructor
  TDF_Surface() : 
    m_numRows(0) { }
  
  /// destructor
  ~TDF_Surface() { }
  
  
  /// \brief columns from TDF file extension (SURFACE)
  std::vector<long> m_layerGroupNumber;         ///< GROUP 
  std::vector<long> m_layerFirstShell;          ///< FIRSTSHELL
  std::vector<long> m_layerLastShell;           ///< LASTSHELL
  std::vector<long> m_layerNumber;              ///< LAYER
  std::vector<std::string> m_layerMaterial;     ///< MATERIAL
  std::vector<double> m_layerSurfaceDensity;    ///< DENSITY
  std::vector<double> m_layerSurfaceThickness;  ///< THICKNESS
  
  long m_numRows;   ///< total num rows in SURFACE ext
  
  /// number of mirror shells: value of the last element of layerlastshell
  int m_numMirrorShells;
  
  /// total thickness of mirror foil bodies for each group
  /// dimen: [ngroups]
  std::vector<double> m_totalThickness;
  
  /// the group number that an XRT foil in a particular shell belongs to and 
  /// will be used later to access the correct optical depth for a given XRT 
  /// object via xrtobjecttaupermmindex
  /// dimen: [nummirrorshells]
  std::vector<int> m_xrtShellToGroupNumber;
  
};  // end struct TDF_Surface


// Struct containing data from the AZIMUTHALSTRUCT extension of the TDF
struct TDF_External {

  /// default constructor
  TDF_External() : 
    m_numExtObjectParts(0),
    m_numTopExtObjectParts(0),
    m_numBottomExtObjectParts(0),
    m_minMabsIndex(0) { }
  
  /// destructor
  ~TDF_External() { }
  
  
  /// \brief columns from TDF file AZIMUTHALSTRUCT extension 
  std::vector<long> m_objectNumber;   ///< OBJECT 
  std::vector<long> m_partNumber;     ///< SUBOBJECT 
  std::vector<double> m_radMin;       ///< RMIN 
  std::vector<double> m_radMax;       ///< RMAX
  std::vector<double> m_startAngle;   ///< STARTANGLE
  std::vector<double> m_endAngle;     ///< ENDANGLE
  std::vector<double> m_zMin;         ///< ZMIN
  std::vector<double> m_zMax;         ///< ZMAX
  std::vector<double> m_density;      ///< EFFDENSITY
  std::vector<long> m_mabsIndex;      ///< MABSINDEX
  
  long m_numExtObjectParts;   ///< num rows in AZIMUTHALSTRUCT ext
  
  /// The number of external object parts that lie above the aperture of the 
  /// main telescope (i.e. above the pre-collimator or primaries)
  long m_numTopExtObjectParts;
  long m_numBottomExtObjectParts; /// Num external object parts below telescope
  
  /// The smallest positive value of mabsindex will be used by the routine that 
  /// reads the mass-absorption data from the FITS file to separate the data 
  /// for mirror foil bodies and the data for external objects.
  // +++ this isn't currently used in the code, but a need for it may arise
  //     to shorten the loops through materials
  int m_minMabsIndex;   // trf: extmabsindexmin
  
};  // end struct TDF_External




/**********************************************
 * ********************************************
 * 		function declarations
 * ********************************************
**********************************************/


/// \brief Set up the data structures relating the telescope
/// \param[in] param Struct holding the TDF filenames, boolean flags for doing 
///               transformations and scattering
/// \param[in] scat Struct holding scattering column names
/// \param[out] misalignment_struct Struct holding consolidated information 
///               about misalignment parameters
/// \param[out] genTelescope struct containing general telescope paramters, 
///               such as TELESCOP, INSTRUME, DETNAME keywords, and focal length
/// \param[out] housings Struct holding consolidated information about the 
///               telescope housing structure, obtained from the MIRROR 
///               extension header in the TDF.
/// \param[out] XRTObjects Array of XRTObject Structs, each holding information 
///               about an individual XRTObject, such as geometry, segmentID, 
///               geoparams, etc.
///               dimen: (numXRTObjects)
/// \param[out] numXRTObjects Total number of XRT objects found in TDF 
///               (obstructions plus mirrors plus pre-collimator foils).
/// \param[out] firstMirrorIndex Index of the XRTObjects array that corresponds 
///               to the array element for the first primary mirror (which 
///               comes after the last obstruction).
/// \param[out] lastMirrorIndex Index of the XRTObjects array that corresponds 
///               to the array element for the last primary mirror (which 
///               comes before the first precollimator).
/// \param[out] zminsortxrtobjectindex Array of indices that point to 
///               XRTObjects array sorted by ZMIN, which, for a given XRT 
///               object is the lowest bound of its z-coordinate in default 
///               (design) position. The array points to XRT objects in 
///               descending order of ZMIN. It only has as many entries as 
///               zlevels.
/// \param[out] zmaxsortxrtobjectindex Array of indices that point to 
///               XRTObjects array sorted by ZMAX, which, for a given XRT 
///               object is the highest bound of its z-coordinate in default 
///               (design) position. The array points to XRT objects in 
///               descending order of ZMAX.
/// \param[out] zmaxsortxrtobjectindexAll Array of indices that point to 
///               XRTObjects array sorted by ZMAX, which, for a given XRT 
///               object is the highest bound of its z-coordinate in default 
///               (design) position. The array points to XRT objects in 
///               descending order of ZMAX. Contains entries for every 
///               XRTObject.
/// \param[out] uniqueFrontNames List of unique columns names from the 
///               reflectivity file
/// \param[out] xrtzlevels The z-coordinates of the numxrtzlevels planes 
///               dividing the telescope.
///               dimen: (numxrtzlevels)
/// \param[out] numxrtzlevels The ray-tracing code divides the telescope into 
///               several planes (with normals parallel to the z-axis); 
///               numxrtzlevels is number of such planes
/// \param[out] obstructionIntervalLowIndex Pointer to array element in for the 
///               xrtobject* arrays for the 1st member of a group of 
///               obstructions: since obstructions start with an xrtobjectid 
///               of 0, it is equal to the xrtobjectid of the 1st obstruction 
///               in a group (interval). We define 3 groups:
///                   0 = above and touching the primaries
///                   1 = between primaries and secondaries
///                   2 = below the secondaries
/// \param[out] obstructionIntervalHighIndex The pointer to the array element 
///               for the last object in an obstruction group/interval, 
///               corresponding to obstructionIntervalLowIndex
/// \param[out] xrtRegionToObjectID This array returns the XRT object ID given 
///               the location in the telescope in terms of the object type and 
///               "set" (i.e. pre-collimators, primaries, or secondaries), the 
///               segment in that set, the sector in that segment, and the 
///               radial shell number.
///               dimen: (maxnumsets, maxmirrorsegment, sectorspersegment, 
///                       numberofshells)
///                   * maxnumsets = total number "vertically arranged" groups 
///                       of objects. This number is normally 3 
///                       (pre-collimator, primaries, secondaries), and could 
///                       be 3 even if there is no pre-collimator and/or no
///                        secondary mirror set.
///                   * maxmirrorsegment = maximum value in the ASSEMBLY column 
///                       of the MIRORR extension of the TDF (the number of 
///                       segments for each mirror set should be the same as 
///                       the number of segments in the pre-collimator).
///                   * sectorspersegment = maximum value in the SECTORNUMBER 
///                       column of the MIRROR extension in the TDF. Note that 
///                       the SECTORNUMBER column value is different to the 
///                       xrtobjectsectorid() array because the former is a 
///                       number unique only to a segment whereas the latter is 
///                       a number unique to the entire telescope.
///                   * numberofshells = maximum value in the NUMBER column of 
///                       the MIRROR extension in the TDF.
/// \param[out] has3DObjects
/// \param[out] numGroups number of groups of foils that share the same 
///               front-mirror reflectivity and transmission data
/// \param[out] transforms Struct containing information about transforming an 
///               XRTObject due to misalignments.
/// \param[out] topExtObjects Struct containing all information about the top 
///               external objects, such as the thermal shield or cover.  This 
///               information comes from the AZIMUTHALSTRUCT extension in the 
///               TDF
/// \param[out] bottomExtObjectsStruct containing all information about bottom 
///               external objects, such as the thermal shield or cover.  This 
///               information comes from the AZIMUTHALSTRUCT extension in the 
///               TDF
/// \param[out] sectorsShells Struct containing data about how the foils are 
///               spaced, including sector numbers and gap boundaries
///
/// Sets up the geometrical structure and other properties of the X-ray 
/// telescope to prepare for ray-tracing. The concept is to take information 
/// about the support structures, mirror foils, and pre-collimator foils from 
/// the telescope definition file (TDF) and create arrays that merge and unify 
/// the different "XRT objects" so that they can all be treated by the 
/// ray-tracing on a par with each other. The ray-tracing code will then view 
/// the components of the telescope as geometrical objects which can be treated 
/// in as uniform a way as possible. This routine, xrtsetup(), returns 
/// structures with names of the form xrtobject*** that hold various 
/// attributes of the XRT objects. The ray-tracing code then uses these 
/// structures to treat the interaction of input photons with the XRT objects. 
/// The routine also returns, for every XRT object, unique sets of numbers that 
/// describe coordinate transformations that result from various misalignments 
/// (translational and rotational), relative to the ideal design case 
/// (some of the misalignments include randomization so the transformation for 
/// each object is in general unique). The ray-tracing code then uses this 
/// transformation information to perform coordinate conversions that 
/// considerably simplify the math of photon-object interactions, because fr
/// keeping the run-time of the main ray-tracing loop low is critical (note 
/// that photons that are absorbed do not have to be transformed back to any 
/// frame of reference). 
/// Note that xrtsetup() does not prepare the reflectivity and transmission 
/// data (this is done elsewhere), although it does set up some critical 
/// indexing that associates each XRT object with the correct reflectivity and 
/// transmission data.
void xrtSetup(Param & param, 
              const Scattering & scat, 
              Misalignment & misalignment_struct,
              GenTelescope & genTelescope, 
              std::vector<HousingGeometry> & housings,
              std::vector<XRTObject> & XRTObjects, 
              long & numXRTObjects,
              long & firstMirrorIndex, 
              long & firstPcolIndex,
              std::vector<long> & zminsortxrtobjectindex,
              std::vector<long> & zmaxsortxrtobjectindex,
              std::vector<long> & zmaxsortxrtobjectindexAll,
              std::vector<std::string> & uniqueFrontNames,
              std::vector<double> & xrtzlevels, 
              long & numxrtzlevels,
              std::vector<int> & obstructionIntervalLowIndex, 
              std::vector<int> & obstructionIntervalHighIndex, 
              vector4Dint & xrtRegionToObjectID,
              bool & has3DObjects, 
              long & numGroups, 
              Transforms & transforms, 
              ExternalObjectsStructure & topExtObjects,
              ExternalObjectsStructure & bottomExtObjects, 
              SectorsShells & sectorsShells);

/// \brief Read in information from TDF, MIRROR extension 
/// \param[in] param Struct holding the TDF filename
/// \param[out] mirror 
/// \param[out] housings 
/// \param[out] misalignment_struct 
/// \param[out] frontreflindex 
/// \param[out] uniqueFrontNames 
/// \param[out] numUniqueFrontNames 
/// \param[out] uniqueRoughNames 
/// \param[out] numUniqueRoughNames 
/// \param[out] backmirrorreflindex 
/// \param[out] numGroups Number of mirror foil groups
/// \param[out] genTelescope Contains TELESCOP, INSTRUME, etc keyword values
/// 
/// The MIRROR extension of the TDF is read, and variables stored describing
/// the location and orientation of the mirror foils.  The housings array is 
/// set up for the precollimator, primary mirror, and secondary mirror.
void setupTDFMirror(Param & param, 
                    TDF_Foil & mirror, 
                    std::vector<HousingGeometry> & housings, 
                    Misalignment & misalignment_struct, 
                    std::vector<int> & frontreflindex, 
                    std::vector<std::string> & uniqueFrontNames, 
                    int numUniqueFrontNames, 
                    std::vector<std::string> & uniqueRoughNames, 
                    int numUniqueRoughNames, 
                    std::vector<int> & backmirrorreflindex, 
                    long & numGroups, 
                    GenTelescope & genTelescope);


/// \brief Read in information from TDF, COLLIMATOR extension 
/// \param[in] param Struct holding the TDF filename, and boolean pcolExists
/// \param[out] pcol 
/// \param[out] housings 
/// \param[out] misalignment_struct 
void setupTDFPrecol(Param & param, 
                    TDF_Foil & pcol, 
                    std::vector<HousingGeometry> & housings, 
                    Misalignment & misalignment_struct, 
                    std::vector<int> & pcolFrontReflIndex, 
                    std::vector<int> & pcolBackReflIndex, 
                    std::vector<std::string> & uniqueRoughNames, 
                    int numUniqueRoughNames);


void setupTDFObstruct(Param & param, TDF_Obstruct & tdfobstruct, 
                      std::vector<Obstruction> & obstructions, 
                      bool & has3DObjects);


void setupTDFSegment(Param & param, 
                     TDF_Segment & seg);


void setupTDFSurface(Param & param, 
                     int numMaterials, 
                     int numGroups, 
                     int pcolMaterialIndex, 
                     const vector1Ddbl & materialDensity, 
                     const std::vector<std::string> & materialNames,
                     const vector2Ddbl & massAbs,
                     TDF_Surface & surf, 
                     ReflectTransGrids & reflectTransGrids,
                     std::vector<int> & xrtShellToGroupNumber);


void setupTDFExternalObjects(Param & param, 
                             TDF_External & externalStructures);

/// \brief read input scattering file, organize information into a struct
/// \param[in] scatterfilename scattering filename, from user input parameter
/// \param[in] telescop telescop keyword, from user input parameter  
/// \param[in] instrume instrume keyword, from user input parameter  
/// \param[out] scat struct of scattering information
/// 
/// Reads an XRT scattering file and returns an array of the probability of 
/// scattering as a function of the incident angle and the scattering angle, 
/// for a number of photon energies, and a number of physical regions in the 
/// telescope.
void readScatteringFile(const std::string & scatterfilename, 
                        const std::string & telescop,
                        const std::string & instrume,
                        Scattering & scat);


/// \brief Trace one photon through telescope until absorption or results plane 
/// \param[in] has3DObjects If true the support structures are treated as 
///               3-dimensional objects, and 2-dimensional if false. Currently 
///               only has3DObjects=False is supported.
/// \param[in] pcolExists
/// \param[in] doFastMode
/// \param[in] initialPhotonCoord Cartesian components of the initial photon 
///               position
/// \param[in] initialPhotonDir Cartesian components of the initial photon 
///               direction
/// \param[in] initialPhotonRadius
/// \param[in] initialPhotonPhi
/// \param[in] numIncidentAngles Number of incident angles for front-side 
///               mirror angle grid
/// \param[in] incidentAngles Incident angle grid for front-side mirror 
///               surfaces (dimen: numIncidentAngles)
/// \param[in] frontRefl Front-side mirror reflection probability 
///                      (dimen: numincidentangles, ngroups)
/// \param[in] frontTran Front-side mirror transmission probability 
///                      (dimen: numincidentangles, ngroups)
/// \param[in] frontTauPermm Optical depth per mm of the coating layers on the 
///               front side of the mirror foils (dimen: ngroups)
/// \param[in] numRoughAngles Number of incident angles values in the angle 
///               grid for the rough surfaces
/// \param[in] roughAngles Incident angle grid for the rough surfaces: back of 
///               mirrors and pcol (dimen: numRoughAngles)
/// \param[in] roughRefl Rough surface reflection probability grid as a 
///               function of incident angle and unique rough surfaces 
///                      (dimen: numroughangles, numroughsurf)
/// \param[in] roughtaupermm  Optical depth per mm for each unique rough 
///               surface (dimen: numroughsurf)
/// \param[in] topExtObjects Reference to ExternalObjectsStructure struct, which holds 
/// \param[in] bottomExtObjects 
/// \param[in] topTransProb Slice of reflTranGrid.m_topExtObjectsTransProb, for a single energy
/// \param[in] bottomTransProb Slice of reflTranGrid.m_bottomExtObjectsTransProb, for a single energy
/// \param[in] scatenergyindex The index for the energy dimension of the input 
///               scattering probability array
/// \param[in] scat Struct containing information about scattering.  Used when 
///               this fn calls getScatteredDirection()
/// \param[in] sectorsShells 
/// \param[in] loopSectorNumbers 
/// \param[in] lowShell 
/// \param[in] highShell 
/// \param[in] genTelescope Contains general information about the telescope, 
///               such as the inner and outer housing radii, and if there are 
///               central object in telescope
/// \param[in] housings Consolidated information about the telescope housing 
///               structure, obtained from the MIRROR extension header in the 
///               TDF. (dimen: 3 for pre-collimator, primary mirrors, and 
///               secondary mirrors respectively)
/// \param[in] housingHardLimit 
/// \param[in] numxrtzlevels The ray-tracing code divides the telescope into 
///               several planes (with normals parallel to the z-axis); 
///               numxrtzlevels is number of such planes
/// \param[in] xrtzlevels The z-coordinates of the numxrtzlevels planes 
///               dividing the telescope.  (dimen: numxrtzlevels)
/// \param[in] resultsplanez The z-coordinate of the plane for which to 
///               collect final ray-tracing results. It is obtained from the 
///               input parameter file and the default value is 0.0, 
///               corresponding to the focal plane. There are no spatial 
///               (e.g. instrument) masks used to filter the results.
/// \param[in] maxNumPathCoords Maximum number of interactions between the ray 
///               and the telescope components that will be considered for 
///               saving key attributes (such as impact coordinates).
/// \param[in] transforms 
/// \param[in] zmaxsortxrtobjectindexAll Array of indices that point to 
///               XRTObjects array sorted by ZMAX, which, for a given XRT 
///               object is the highest bound of its z-coordinate in default 
///               (design) position. The array points to XRT objects in 
///               descending order of ZMAX.  Contains entries for every 
///               XRTObject.
/// \param[in] numXRTObjects
/// \param[in] XRTObjects Array of all the XRTObjects in telescope 
///               (dimen: numXRTObjects)
/// \param[in] obstructionIntervalLowIndex
/// \param[in] obstructionIntervalHighIndex
/// \param[out] numinteractions Total number of interactions between the 
///               photon and XRT components, including the final focal-plane 
///               impact if there is one. The "zeroth" interaction corresponds 
///               to the initial photon position and direction.
/// \param[out] pathCoords Cartesian coordinates of each photon-object 
///               interaction point, in XRTframe 
///               (dimen: numinteractions+1)
/// \param[out] pathDirs Cartesian components of the direction vector of the 
///               photon at each interaction point (if the photon direction 
///               changes at the interaction point, the direction is that just 
///               before the interaction takes place). 
///               (dimen: numinteractions+1)
/// \param[out] pathCode array of three integers per interaction that record 
///               various attributes of the interaction. The three integers are 
///               referred to as A, BC, and D, and are defined as follows:
///                  pathcode[0] = A = interaction type
///                    1 = absorption or end of path on the results plane
///                    2 = reflection only
///                    3 = reflection followed by scattering
///                    4 = transmission
///                    9 = error
///                  pathcode[1] = BC = impacted object
///                    01 = results plane
///                    02 = inner housing
///                    03 = outer housing
///                    04 = segment/sector sidewall
///                    05 = pre-collimator foil
///                    06 = primary mirror foil
///                    07 = secondary mirror foil
///                    08 = support structure
///                    09 = top external object (above main telescope)
///                    10 = bottom external object (below main telescope)
///                  pathcode[2] = D = impacted face
///                     1 = back (mirrors, pre-collimator foils)
///                         i.e. the face oriented towards outer housing
///                         for obstructions & focal plane, it is the top
///                     2 = front (mirrors, pre-collimator foils)
///                         i.e. the face oriented towards inner housing
///                         for obstructions it is the bottom.
///                     3 = mirror, pre-collimator foils: top face/edge 
///                     4 = mirror, pre-collimator foils: bottom face/edge
///                     5 = mirror, pre-collimator foils: sides
///                     6 = Undetermined (but not back or front)
/// \param[out] pathXRTObjectID The XRT object ID number for each ray-object 
///               interaction. For impacts on XRT components that do not have 
///               an object ID, the value is -1 (this will always be a final 
///               interaction since photons will always be absorbed at sites 
///               that are not classed as XRT objects).
/// \param[out] finalPhotonPos Cartestian coordinates of the last recorded 
///               photon position (if the results plane was intercepted then 
///               the coordinates are those on the results plane): they are 
///               simply copied from the final coordinates in pathcoordinates().
/// \param[out] finalPhotonDir Cartestian components of the last recorded 
///               photon direction (if the results plane was intercepted then 
///               the unit vector components are those just prior to impact on 
///               the results plane): they are simply copied from the final 
///               components in pathphotondir().
/// \param[out] resultsPlaneWasImpacted True if results plane (focal plane by 
///               default) was impacted.
/// \param[out] pathErrorCode Photon error codes.
/// \param[out] pathIncidentAngle 
/// \param[out] pathScatteringAngle 
///
/// Follows a photon through an X-ray telescope until it is absorbed by an XRT 
/// object or until it intercepts the results plane (which by default is the 
/// focal plane), given the initial position and initial direction of the 
/// photon.
void raytraceOnePhoton(bool has3DObjects, 
                       bool pcolExists,
                       bool doFastMode,
                       const CartesianCoord & initialPhotonCoord,
                       const DirectionVector & initialPhotonDir, 
                       double initialphotonradius, 
                       double initialphotonphi,
                       int numIncidentAngles,
                       const vector1Ddbl & incidentAngles, 
                       const vector2Ddbl & frontRefl, 
                       const vector2Ddbl & frontTran, 
                       const vector1Ddbl & frontTauPermm, 
                       int numRoughAngles, 
                       const vector1Ddbl & roughAngles,
                       const vector2Ddbl & roughRefl,
                       const vector1Ddbl & roughtaupermm, 
                       const ExternalObjectsStructure & topExtObjects, 
                       const ExternalObjectsStructure & bottomExtObjects, 
                       const vector1Ddbl & topTransProb, 
                       const vector1Ddbl & bottomTransProb, 
                       int scatenergyindex,
                       const Scattering & scat, 
                       const SectorsShells & sectorsShells, 
                       const std::vector<int> & loopSectorNumbers, 
                       int shellsearchlo, 
                       int shellsearchhi,
                       const GenTelescope & genTelescope,
                       const std::vector<HousingGeometry> & housings, 
                       double housingHardLimit,
                       long numxrtzlevels, 
                       const vector1Ddbl & xrtzlevels,
                       double resultsplanez, 
                       int maxNumPathCoords, 
                       const Transforms & transforms, 
                       const std::vector<long> & zmaxsortxrtobjectindexAll, 
                       int numXRTObjects, 
                       const std::vector<XRTObject> & XRTObjects, 
                       const std::vector<int> & obstructionIntervalLowIndex, 
                       const std::vector<int> & obstructionIntervalHighIndex, 
                       int & numInteractions, 
                       std::vector<CartesianCoord> & pathCoords, 
                       std::vector<DirectionVector> & pathDirs,
                       vector2Dint & pathCode, 
                       std::vector<int> & pathXRTObjectID,
                       CartesianCoord & finalPhotonPos,
                       DirectionVector & finalPhotonDir, 
                       bool & resultsPlaneWasImpacted, 
                       std::vector<int> & pathErrorCode,
                        
                       std::vector<double> & pathIncidentAngle,
                       std::vector<double> & pathScatteringAngle);


/// \brief 
/// \param[in] 
/// \param[out] 
/// 
/// Follows a photon through an X-ray telescope until it is absorbed by an 
/// XRT object or until it is otherwise terminated. This routine is a fast 
/// version of raytraceOnePhoton() that is designed to quickly calculate the 
/// effective area at the expense of not fully calculating paths that do not 
/// contribute to the effective area (or the contribution is negligible).
void raytracePhotonFast(bool has3DObjects,  
                        bool pcolExists,
                        const CartesianCoord & initialPhotonCoord,
                        const DirectionVector & initialPhotonDir, 
                        double initialPhotonRadius, 
                        double initialPhotonPhi,
                        int numIncidentAngles,
                        const vector1Ddbl & incidentAngles, 
                        const vector2Ddbl & frontRefl, 
                        const vector2Ddbl & frontTran, 
                        const vector1Ddbl & frontTauPermm,
                        int numRoughAngles, 
                        const vector1Ddbl & roughAngles, 
                        const vector2Ddbl & roughRefl,
                        const vector1Ddbl & roughtaupermm, 
                        const ExternalObjectsStructure & topExtObjects, 
                        const ExternalObjectsStructure & bottomExtObjects, 
                        const vector1Ddbl & topTransProb, 
                        const vector1Ddbl & bottomTransProb,
                        const GenTelescope & genTelescope,
                        const std::vector<HousingGeometry> & housings, 
                        const vector1Ddbl & xrtzlevels,
                        double resultsplanez, 
                        int maxNumPathCoords, 
                        const std::vector<XRTObject> & XRTObjects,
                        int scatenergyindex,
                        const Scattering & scat, 
                        const SectorsShells & sectorsShells, 
                        const std::vector<int> & obstructionIntervalLowIndex, 
                        const std::vector<int> & obstructionIntervalHighIndex, 
                        const Transforms & transforms, 
         
                        int & numInteractions, 
                        std::vector<CartesianCoord> & pathCoords, 
                        std::vector<DirectionVector> & pathDirs,
                        vector2Dint & pathCode, 
                        std::vector<int> & pathXRTObjectID,
                        CartesianCoord & finalPhotonPos,
                        DirectionVector & finalPhotonDir, 
                        bool & resultsPlaneWasImpacted, 
                        std::vector<int> & pathErrorCode);


/// \brief Create a string with the entire pathcode for a single photon's path
/// \param[in] numInteractions the number of interactions for this photon
/// \param[in] pathCode an array with the codes for each interaction 
///               (size: numInteractions+1)
/// \param[out] combinedPathCodeString the end result string of path codes
/// 
/// Join the individual pathcodestring values to form a single string, 
/// combinedPathCodeString. Limit the length of the string 
/// combinedPathCodeString to 32 characters, or 8 interactions, including the 
/// final one, which may or may not be a focal plane impact (each interaction 
/// is described by 4 characters). The first 4 characters of pathcodestring 
/// correspond to the first interaction. If there are more than 8 interactions, 
/// include the first 8 consecutive interactions.  The details of what each
/// pathcode means is described in the declaration of raytraceOnePhoton().
/// \internal
/// \note We pass in numInteractions, rather than finding the size of pathCode,
///       in order to maximize speed.  This is called inside of the nested loops
///       in doWork, called for each theta, roll, energy.
void createCombinedPathCodeString(int numInteractions, 
                                  const std::vector< std::vector<int> > & pathCode, 
                                  std::string & combinedPathCodeString);



/// \brief Make a short list of candidate XRT objects that can potentially be 
///        intercepted by the photon
/// \param[in] photonBBox 
/// \param[in] idxLow 
/// \param[in] numXRTObjects Number of XRT objects in XRT object arrays. Since 
///               speed is particularly critical for this routine, 
///               numXRTObjects is passed as an input.
/// \param[in] zmaxsortxrtobjectindex vector of indices that refer to XRT 
///               object bounding boxes, sorted on zMax, the highest z coord of 
///               an XRTobject (descending order). 
///               size = numXRTObjects
/// \param[in] XRTObjects Vector of structure XRTObjects.  This function will 
///               be using the object type and transformed bounding boxes of 
///               the XRTObjects. 
///               size = numXRTObjects
/// \param[in] excludeID
/// \param[in] doRadialRejection If true, use the radial bounds of an XRT 
///               object (as well as Cartesian bounding boxes) to determine 
///               whether an XRT object is a candidate for interaction with 
///               the photon
/// \param[out] newIdxLow Upon exit this index points to the z-sorted index 
///               array element that corresponds to the first bounding box in 
///               the z-sorted array that was not rejected because of its zmin 
///               compared to the current photon z. If all bounding boxes are 
///               rejected the exit value is -1 indicating that the there are 
///               no more photon-object impacts to search for given the current 
///               photon position and direction.
/// \param[out] numCandidates Number of candidate XRT objects qualifying for
///               a potential interaction with the photon.
/// \param[out] candidateList List of XRT object IDs pointing to the XRT 
///               objects qualifying for a potential interaction with the 
///               photon (these are the array indices that directly point to 
///               the XRT object properties in the xrtobject* arrays)
///               size = numCandidates
///
/// Given a photon bounding box and a list of XRT objects, make a short list of 
/// candidate XRT objects that can potentially be intercepted by the photon. An 
/// index array is also passed that points to members of the bounding box array 
/// sorted in order of decreasing z-distance from the focal plane (i.e. from 
/// top to bottom of telescope). The start and stop indices for the sorted 
/// array are supplied for searching for potential photon-object impacts. A 
/// dynamic indexing system allows XRT objects that were rejected on a previous 
/// run (because their minimum z-coordinate was higher than the current photon 
/// position) not to be tested for impact again. This assumes that the photon 
/// will never have any component of motion in the direction of increasing z 
/// (away from the focal plane) so it is optional to invoke this (it is 
/// implemented by means of the variable newIdxLow which is updated on each 
/// run and the sortedIdxLow variable can be set equal to newIdxLow).
/// \note This function has been analyzed and rewritten in order to increase 
///       speed. 
/// \note This function is called from raytraceOnePhoton() when the user enters
///       the param fastmode=no.  It is more exact than getImpactCandidates(), 
///       but not quite as fast.
/// \internal 
void interceptXRTObjects(const PhotonBoundingBox & photonBBox, 
                         int idxLow, 
                         int numXRTObjects, 
                         std::vector<long> zmaxsortxrtobjectindex,
                         const std::vector<XRTObject> & XRTObjects,
                         long excludedID,
                         bool doRadialRejection,
                         int & newIdxLow, 
                         int & numCandidates,
                         std::vector<int> & candidateList);



/// \brief Find XRTObjects that could be impacted by photon
/// \param[in] photonBBox Cartesian components of a bounding box that 
///               completely encloses the photon path between the current 
///               position and the projected z-coordinate of the end of the 
///               path. Also includes the inner and outer radial bounds of the 
///               photon path endpoints (in other words, in the x-y plane the 
///               photon path is a line segment that lies entirely within the 
///               radial bounds).
/// \param[in] numXRTObjects Number of XRT objects in XRT object arrays.
///               Since speed is particularly critical for this routine, 
///               numXRTObjects is passed as an input.
/// \param[in] XRTObjects Vector of structure XRTObjects.  This function will 
///               be using the object type and transformed bounding boxes of 
///               the XRTObjects. The members that this routine uses are:
///                * m_set
///                * m_BBox
///               size = numXRTObjects
/// \param[in] excludeID Unconditionally reject the XRT object with id equal to 
///               this parameter (exlcudedid) as a candidate for intercepting 
///               the photon. For example, if the photon has just been 
///               reflected from an XRT object we don’t want to include that 
///               object for consideration as the next most likely XRT object 
///               to be intercepted otherwise we would be stuck in a loop in 
///               which the photon can never leave the XRT object.
/// \param[in] obstructionIntervalLowIndex Pointer to array element in for the 
///               xrtobject* arrays for the 1st member of a group of 
///               obstructions: since obstructions start with an xrtobjectid of 
///               0, it is equal to the xrtobjectid of the 1st obstruction in a 
///               group (interval). We define 3 groups:
///                 0 = above and touching the primaries
///                 1 = between primaries and secondaries
///                 2 = below the secondaries
/// \param[in] obstructionIntervalHighIndex The pointer to the array element 
///               for the last object in an obstruction group/interval, 
///               corresponding to obstructionintervalloindex
/// \param[in] zGroupPointer Routine will loop over relevant obstructions, 
///               using zGroupPointer to exclude the group of obstructions 
///               that cannot be impacted:
///                 0 means all obstructions can be impacted
///                 1 means a primary mirror has been impacted so 1st group of 
///                   obstructions cannot be impacted;
///                 2 means a secondary mirror has been impacted so 1st and 2nd 
///                   group of obstructions cannot be impacted
/// \param[in] numLoopSectors Number of sectors that will be searched for 
///               potential photon-object impacts
/// \param[in] loopSectorNumbers For each sector that is searched, the elements 
///               of this array are equal to the sector number. This is needed 
///               because the sectors are cyclicly numbered so the upper sector 
///               could have a lower sector number than the lower sector of the 
///               consecutive range of sectors searched.
/// \param[in] lowShell Lower shell number that could be used to search for 
///               potential photon-object impact candidates (subtract 1 for 
///               index)
/// \param[in] highShell Upper shell number that could be used to search for 
///               potential photon-object impact candidates (subtract 1 for 
///               index)
/// \param[in] sectorsShells struct containing information about the telescope 
///               sectors and shells.  This routine uses the array 
///               setsectorshell2xrtobjectid
/// \param[out] numCandidates Number of candidate XRT objects qualifying for
///               a potential interaction with the photon.
/// \param[out] candidateList List of XRT object IDs pointing to the XRT 
///               objects qualifying for a potential interaction with the photon 
///               (these are the array indices that directly point to the XRT 
///               object properties in the xrtobject* arrays)
///               size = numCandidates
///
/// Given a photon bounding box and a list of XRT objects, make a short list of 
/// candidate XRT objects that can potentially be intercepted by the photon.
/// \internal
/// \note This routine is meant to replace interceptXRTObjects()
void getImpactCandidates(const PhotonBoundingBox & photonBBox, 
                         int numXRTObjects, 
                         const std::vector<XRTObject> & XRTObjects,
                         long excludedID,
                         const std::vector<int> & obstructionIntervalLowIndex, 
                         const std::vector<int> & obstructionIntervalHighIndex, 
                         long zGroupPointer, 
                         int numLoopSectors,
                         const std::vector<int> & loopSectorNumbers, 
                         int lowShell, 
                         int highShell, 
                         const SectorsShells & sectorsShells, 
                         int & numCandidates,
                         std::vector<int> & candidateList);




/// \brief Calculate the impact points between a single ray (photon) and a 
///        single cone
/// \param[in] coordOrig Cartesian coordinates of the photon initial position 
///               in the original frame
/// \param[in] dirVecOrig Cartesian components of the photon direction unit 
///               vector in the original frame
/// \param[in] coordTran Cartesian coordinates of the photon initial position
///               in the transformed frame (cone in standard position)
/// \param[in] dirVecTran Cartesian components of the photon direction unit 
///               vector in the transformed frame (cone in standard position)
/// \param[in] kc Slope of the cone in the equation z^2 = (kc)^2 (x^2 + y^2)
/// \param[in] kcSq The square of kc
/// \param[out] phInitPos Indicates whether the photon initial position was: 
///                 0: inside the cone
///                 1: outside the cone
///                 2: on the surface of the cone
///               Note that there is no redundancy in information compared to 
///               rayConeResult, because phInitPos is assigned a value 
///               regardless of whether there is a ray-cone impact or not.
/// \param[out] rayConeResult Indicates the outcome of the ray-cone interaction:
///                 = neg: no impact possible
///                  -1: discriminator in quadratic negative
///                  -2: both solutions for impact are behind the 
///                      photon
///                  -3: both solutions are for impact with the 
///                      ghost cone so are not valid 
///                  -4: ray parallel to surface and impact point 
///                      is behind the photon
///                  -5: ray parallel to surface but intercepts 
///                      the ghost cone.
///                 = 0: impact on the inside of the cone
///                 = 1: impact on the outside of the cone
///                 = 2: impact on inside but linear approximation 
///                      for solution used. 
///                 = 3: impact on outside but linear approximation 
///                      for solution used. 
///                 = 4: initial position is on the surface of the 
///                      cone
/// \param[out] tImpact Time elapsed between the photon at the initial position 
///               to the impact point if there is one. Since the photon 
///               direction vector (“velocity”) is a unit vector, the impact 
///               time is identical to the impact distance (path length between 
///               initial position and impact position)
/// \param[out] coordImpactOrig Cartesian coordinates of the ray-cone impact 
///               position, if there is one, in the original telescope frame
/// \param[out] coordImpactTran Cartesian coordinates of the ray-cone impact 
///               position, if there is one, in the frame in which the cone is 
///               in standard position
/// 
/// Calculate the impact points between a single ray (photon) and a single 
/// cone. In the interest of speed, the interaction is performed with the cone 
/// in "standard position" (axis parallel to the z-axis, apex at the origin, 
/// pointing towards negative z). In the telescope frame, the photon position 
/// and direction are transformed instead of the cone. In the interest of speed 
/// both the original and transformed position and direction of the photon must 
/// be supplied. Further in the interest of speed, the square of the (z/r) 
/// slope that appears in the equation of the cone must be supplied 
/// pre-calculated. The routine returns the time between the initial photon 
/// position and impact (if there is one), and the impact coordinates in the 
/// original frame and in the frame in which the cone is in standard position 
/// (the impact time is the same in any frame). In the case that a solution is 
/// not found, the routine returns some details about why. It also tells 
/// whether an interaction took place inside or outside the cone. 
/// Solutions of the ray-cone equations can return up to two impact points: 
/// however this actually includes two cones which are joined at the apex and 
/// share a common axis (the second cone which has its apex pointing the +z 
/// direction is the "ghost cone"). Since the routine is specifically designed 
/// for finding interactions between photons and components in an X-ray 
/// telescope, the routine never returns more than one impact point, selecting 
/// the solution that is in the direction of motion of the photon, and 
/// rejecting interactions with the ghost cone. In the case that there are 
/// still two solutions, the one with the shortest distance between the photon 
/// initial position and the impact point is selected (equivalent to the one 
/// with the shortest impact time).
void rayConeIntercept(const CartesianCoord & coordOrig, 
                      const DirectionVector & dirVecOrig,
                      const CartesianCoord & coordTran,
                      const DirectionVector & dirVecTran,
                      double kc, 
                      double kcSq, 
                      int & phInitPos, 
                      int & rayConeResult, 
                      double & tImpact, 
                      CartesianCoord & coordImpactOrig, 
                      CartesianCoord & coordImpactTran);


/// \brief Calculate the impact points between a single ray (photon) and a 
///        single cylinder
/// \param[in] coordOrig Cartesian coordinates of the photon initial position 
///                      in the original frame
/// \param[in] dirVecOrig Cartesian components of the photon direction unit 
///                      vector in the original frame
/// \param[in] coordTran Cartesian coordinates of the photon initial position 
///                      in the transformed frame
/// \param[in] dirVecTran Cartesian components of the photon direction unit 
///                      vector in the transformed frame
/// \param[in] r0Sq square of radius of cylinder.
/// \param[out] phInitPos Indicates whether the photon initial position was: 
///                         0: inside the cylinder
///                         1: outside the cylinder
///                         2: on the surface of the cylinder
///                       Note that there is no redundancy in information 
///                       compared to rayCylResult, because phInitPos is 
///                       assigned a value regardless of whether there is a 
///                       ray-cone impact or not.
/// \param[out] rayCylResult Indicates the outcome of the ray-cylinder 
///                          interaction as follows:
///                          = neg: no impact possible
///                          = -1: discriminator in quadratic negative
///                          = -2: both solutions for impact are behind the 
///                                photon
///                          = -3: both solutions are for impact with below the 
///                                focal plane (z is neg) so are not valid 
///                          = -4: ray parallel to surface
///                          = 0: impact on the inside of the cylinder
///                          = 1: impact on the outside of the cylinder
///                          = 2: impact on inside but linear approximation for 
///                               solution used (not yet implemented). 
///                          = 3: impact on outside but linear approximation 
///                               for solution used (not yet implemented). 
///                          = 4: initial position is on the surface of the cylinder
/// \param[out] tImpact Time elapsed between the photon at the initial position 
///                     to the impact point if there is one. Since the photon 
///                     direction vector (“velocity”) is a unit vector, the 
///                     impact time is identical to the impact distance (path 
///                     length between initial position and impact position)
/// \param[out] coordImpactOrig Cartesian coordinates of the ray-cylinder 
///                             impact position, if there is one, in the 
///                             original telescope frame
/// \param[out] coordImpactTran Cartesian coordinates of the ray-cylinder 
///                             impact position, if there is one, in the frame 
///                             in which the cone is in standard position
/// 
/// Calculate the impact points between a single ray (photon) and a single 
/// cylinder. In the telescope frame, the photon position and direction are 
/// transformed instead of the cylinder. In the interest of speed both the 
/// original and transformed position and direction of the photon must be 
/// supplied. Further in the interest of speed, the square of the radius of the 
/// cylinder must be supplied. The routine returns the time between the initial 
/// photon position and impact (if there is one), and the impact coordinates in 
/// the original frame and in the frame in transformed (the impact time is the 
/// same in any frame). In the case that a solution is not found, the routine 
/// returns some details about why. It also tells whether an interaction took 
/// place inside or outside the cylinder.
/// Solutions of the ray-cylinder equations can return up to two impact points. 
/// Since the routine is specifically designed for finding interactions between 
/// photons and components in an X-ray telescope, the routine never returns 
/// more than one impact point, selecting the shortest distance solution that 
/// is in the direction of motion of the photon.
/// This routine is very similar to rayConeIntercept, except we're calculating 
/// the impact point of a ray with a cylinder instead of a cone.
void rayCylinderIntercept(const CartesianCoord & coordOrig, 
                          const DirectionVector & dirVecOrig,
                          const CartesianCoord & coordTran,
                          const DirectionVector & dirVecTran,
                          double r0Sq, 
                          int & phInitPos, 
                          int & rayCylResult, 
                          double & tImpact, 
                          CartesianCoord & coordImpactOrig, 
                          CartesianCoord & coordImpactStd);


/// \brief Determine if a point lies inside a polygon
/// \param[in] point The 2 dimensional point
/// \param[in] numVertices Number of vertices
/// \param[in] vertices The 2 dim points of each vertex 
/// \return Returns true if the given point is inside the polygon, on an edge,
///         or on a vertex; false if not
///
/// Determine whether a point (xp,yp) lies inside or outside a polygon. The 
/// routine works only in 2 dimensions. It is critical that the order of the 
/// supplied vertices of polygon is sequential around the polygon (either 
/// clockwise or anti-clockwise) 
bool isPointInsidePolygon(const CartesianCoord & point, 
                          int numVertices,
                          const std::vector<CartesianCoord> & vertices);


/// \brief Determines whether a photon path intercepts a telescope object and 
///        calculates the results of the interaction if there is one.
/// \param[in] objIndex index of object, used for testing
/// \param[in] objectType Type of object (obstruction or foil)
/// \param[in] surfaceGeometry Type of geometry (obstruction, cylinder, or cone)
/// \param[in] treatAs3D If true the support structures are treated as 
///                      3-dimensional objects, and 2- dimensional if false. 
///                      Currently only obs3d=False is supported.
/// \param[in] slopes Quantities related to the slopes (and their squares) of 
///                   XRT objects, if relevant. For cone-shaped foils the slope 
///                   is kc, where the equation of the cone is 
///                   z^2 = kc^2 (x^2+y^2). The front surface and back surface 
///                   of an XRT foil are treated as two cones that can have 
///                   their own parameters.
/// \param[in] geoParams Various geometrical parameters and some pre-calculated 
///                      derived quantities (in the interest of speed) 
///                      describing an XRT object (currently not relevant for 
///                      obstructions). These are calculated in xrtsetup()
/// \param[in] numVertices Number of vertices of the XRT object
/// \param[in] vertices The Cartesian coordinates of up to 8 vertices
/// \param[in] objectBBox The XRT object’s bounding box parameters, calculated 
///                       in xrtsetup()
/// \param[in] startAngle If relevant, the start angle position that will 
///                       completely contain the XRT object
/// \param[in] endAngle If relevant, the end angle position that will 
///                       completely contain the XRT object
/// \param[in] startAngleMod 
/// \param[in] endAngleMod 
/// \param[in] origCoord Cartesian coordinates of the photon initial position 
///                      in the original frame
/// \param[in] origDirVec Cartesian components of the photon direction unit 
///                       vector in the original frame
/// \param[in] transformedCoord Cartesian coordinates of the photon initial 
///                             position in the transformed frame (note: the 
///                             routine places the XRT object in standard 
///                             position, so it should not be done outside)
/// \param[in] transformedDirVec Cartesian components of the photon direction 
///                              unit vector in the transformed frame (note: 
///                              the routine places the XRT object in standard 
///                              position, so it should not be done outside)
/// \param[out] faceHit Coded information about the ray-object interaction with 
///                     respect to which face of an object was hit first:
///                       = -2 if initial photon position was inside the 
///                            foil/object (this should never happen but if it 
///                            does the results are invalid
///                       = -1 if no impact possible
///                       =  0 for back-face impact for foil, bottom for 
///                            obstruction
///                       =  1 for front-face impact for foil, top for 
///                            obstruction 
///                       =  2 for impact on top face/edge of foil
///                       =  3 for impact on bottom face/edge of foil
///                       =  4 for impact on sides of foil
///                       =  5 impact face unknown (not needed in some 
///                            situations...e.g. entry on side, exit from side)
/// \param[out] exitFace Coded information about the ray-object interaction 
///                      with respect to which face of an XRT object the 
///                      emerging ray exits from (it does not mean the photon 
///                      actually exits, however):
///                        = 0: back face of foil
///                        = 1: front face of foil
///                        = 2: top face/edge of foil
///                        = 3: bottom face/edge of foil 
///                        = 4: side of foil
///                        = 5: unknown
/// \param[out] impactCoordOrig If there is an impact between the photon and 
///                             XRT object these are the Cartesian coordinates 
///                             of the impact in the original (untransformed) 
///                             XRT frame.
/// \param[out] impactCoordStd If there is an impact between the photon and XRT 
///                            object these are the Cartesian coordinates of 
///                            the impact in the frame in which the impacted 
///                            surface is in “standard position.” For a cone 
///                            with axis parallel to the z-axis this means the 
///                            apex is at the origin. Note that the standard 
///                            positions for front and back surfaces of 
///                            cone-shaped foils are not identical.
/// \param[out] distance If there is an impact, this is the distance between 
///                      the photon initial position and the impact point. 
///                      Since the direction vector (“velocity”) should be 
///                      normalized to unity, the impact distance is identical 
///                      to the impact time.
/// \param[out] pathLength The distance between the entry and exit points of a 
///                        ray along the photon direction through an XRT object 
///                        (if it is intercepted). It is used later to 
///                        calculate transmission probability and has the same 
///                        units as the length parameters describing the XRT 
///                        object. It is not calculated for all scenarios, in 
///                        which case it is assigned a value of -1. For example 
///                        a ray entering the bottom face of a foil and exiting 
///                        from a front/back face is not treated (in the 
///                        interest of speed) since such a situation is 
///                        unlikely to occur in a real telescope (the photon is 
///                        flagged to be terminated later).
/// \param[out] errorCode Coded information about any error conditions that 
///                       arose, as follows:
///                         = 0 if everything ok (including in impact result)
///                         = 1 initial photon position was inside the foil 
///                             (results invalid)
///                         = 2 initial photon position was on the surface of a 
///                             foil (photon will be destroyed) 
///                         = 3 identical impact times to front and back of 
///                             foil (photon will be destroyed)
///                         = 4 path between front/back face and one of the 
///                             sides: photon will be terminated
/// \param[out] killPhoton If this variable is False on exit, the photon 
///                        continues propagating through the telescope. 
///                        However, if it is True, the photon path is flagged 
///                        for termination later after returning from the 
///                        routine. This could happened if the photon hits a 
///                        support structure, or is deemed to be absorbed under 
///                        certain circumstances (e.g. if we know the photon 
///                        path entry point is on the top edge of a foil and 
///                        the exit point is on the bottom edge, we assume it 
///                        is absorbed without doing a detailed calculation). 
///                        The scenarios that can terminate the photon path are 
///                        as follows:
///                          -obstruction hit
///                          -path from top to bottom face of a foil
///                          -any path that involves the side or sides of a 
///                           foil (the sides are not real for sub-foils, and 
///                           for full foils an entry or exit on a side is 
///                           computationally expensive to calculate and is 
///                           very unlikely to occur in a telescope and if it 
///                           does, is likely to result in a photon being 
///                           terminated due to absorption later anyway)
///
/// Determines whether a photon path intercepts a telescope object and 
/// calculates the results of the interaction if there is one. In the case of 
/// an impact, for mirror or pre-collimator foils, the path of the ray to 
/// 'the other side' of the object is also calculated in order for downstream 
/// routines to calculate the transmission probability. The interaction 
/// between ray and object is treated with the object in a standardized 
/// position with respect to the coordinate axes (in the interest of run-time 
/// speed). The coordinate transformations are done outside the routine (the 
/// photon initial position and direction are transformed instead of the 
/// object). However, the routine needs both the untransformed and transformed 
/// photon parameters and it returns the impact coordinates (if there is an 
/// impact) in both frames.
void getPhotonObjectImpactCoords(long objIndex, 
                                 objectTypes_e objectType, 
                                 surfaceGeometryTypes_e surfaceGeometry,
                                 bool treatAs3D, 
                                 const Slopes & slopes,
                                 const std::vector<double> & geoParams, 
                                 int numVertices, 
                                 const std::vector<CartesianCoord> & vertices,
                                 const ObjectBoundingBox & objectBBox, 
                                 double startAngle, 
                                 double endAngle,
                                 double startAngleMod, 
                                 double endAngleMod, 
                                 const CartesianCoord & origCoord,
                                 const DirectionVector & origDirVec,
                                 const CartesianCoord & transformedCoord,
                                 const DirectionVector & transformedDirVec,
                                 int & faceHit, 
                                 int & exitFace, 
                                 CartesianCoord & impactCoordOrig,
                                 CartesianCoord & impactCoordStd,
                                 double & distance, 
                                 double & pathLength,
                                 int & errorCode, 
                                 bool & killThePhoton);


/// \brief Determine whether a sector sidewall is impacted
/// \param[in] coordIn Cartesian coordinates of the initial photon position
/// \param[in] dirVec Cartesian components of the photon direction unit vector
/// \param[in] coordFoil Cartesian components of the potential impact point of 
///               the photon on the sector sidewall.
/// \param[in] checkFoilImpact True if there is a candidate XRT foil to be 
///               tested that could be intercepted instead of the sector 
///               sidewalls
/// \param[in] tanThetaWall1 The tangent of the angles (relative to the x-axis) 
///               of the sector sidewalls/boundaries
/// \param[in] tanThetaWall2 The tangent of the angles (relative to the x-axis) 
///               of the sector sidewalls/boundaries
/// \param[out] foilImpactedFirst True if a foil was impacted before the 
///               sidewall, False if the sidewall was impacted before the foil 
///               was reached
/// \param[out] sidewallOutcome Code indicating the final outcome as follows:
///               0 = normal outcome with one robust solution.
///               1 = There were 2 positive time solutions 
///                   corresponding to impacting a side wall with
///                   the same tan(theta) but in a different quadrant. 
///                   The values returned for the impact results 
///                   correspond to the smallest of these times.
///              -1 = photon traveling parallel to sidewall #1; 
///                   no impact possible
///              -2 = photon traveling parallel to sidewall #2; 
///                   no impact possible
///              -3 = times to impact the adjacent sidewalls are both negative
///              -4 = times to impact the adjacent sidewalls are both zero: 
///                   this is also an error condition.
///              Thus all events with sidewalloutcome=-3,-4,-5 should be 
///                   dropped as anomalous.
/// \param[out] tWallImpact Time lapsed between photon initial position and 
///               sidewall if there is one
/// \param[out] coordImpact Cartesian coordinates of the impact point on a 
///               sidewall if there is one
/// \param[out] wallID If there is an impact with a sidewall, wallid indicates 
///               which one (1 or 2)
///
/// Given a photon initial position (coordIn) and direction vector (dirVec)
/// determine whether a sector sidewall is impacted. The routine takes the 
/// sidewall boundary positions enclosing the mirror/pre-collimator foil from 
/// the two tangents of the angles (and it is assumed that the foil extends to 
/// these boundaries). Note that the routine can be used with or without a 
/// foil; for the latter the intercept between the photon and foil has to be 
/// pre-calculated.
void sidewallImpact(const CartesianCoord & coordIn, 
                    const DirectionVector & dirVec, 
                    const CartesianCoord & coordFoil, 
                    bool checkFoilImpact,
                    double tanThetaWall1, 
                    double tanThetaWall2, 
                    bool & foilImpactedFirst, 
                    int & sidewallOutcome,
                    double & tWallImpact, 
                    CartesianCoord & coordImpact,
                    int & wallID);



/// \brief Calculate the impact point (if there is oone) between a single ray 
///        (photon) and the telescope inner and outer housing units
/// \param[in] housings Struct containing information about the telescope 
///               housings
/// \param[in] coordInit Initial cartesian coord of photon
/// \param[in] dirVecInit Initial direction vector of photon
/// \param[out] impactResult 
/// \param[out] innerOuter 
/// \param[out] tImpact 
/// \param[out] coordImpact Cartesian coordinates of impact, if there is one
///
/// Calculate the impact point (if there is oone) between a single ray (photon) 
/// and the telescope inner and outer housing units. The units of all lengths 
/// and distances are mm. There are several housing units corresponding to the 
/// pre-collimator, primary mirror, and secondary mirror assemblies, and they 
/// may be offset relative to each other. Although Hitomi has all three units, 
/// in general an X-ray telescope may have only one or two of them; the 
/// routine accounts for such possibilities. A missing unit is indicated by its 
/// inner and outer radii being zero.
void housingImpactPositions(const std::vector<HousingGeometry> & housings,
                            const CartesianCoord & coordInit,
                            const DirectionVector & dirVecInit,
                            int & impactResult, 
                            int & innerOuter,
                            double & tImpact, 
                            CartesianCoord & coordImpact);


/// \brief 
/// \param[in] initialDir Cartesian components of the unit direction vector of 
///               the initial (incident) ray
/// \param[in] normal Cartesian components of the unit normal vector at the 
///               point at which reflection is taking place
/// \param[in] isSpecular True if only specular reflection is being considered, 
///               in which case the reflected angle is equal to the incident 
///               angle. False if scattering is considered in addition to 
///               specular reflection. The scattering angle and the effective 
///               net reflected angle are calculated outside the routine
/// \param[in] sinIncident The sine of the grazing incident angle (i.e. 90 deg
///               minus the angle of the incident ray with respect to the 
///               normal). If sinincident angle has a value lt -1, it is a 
///               flag for the routine to calculate sinIncident from the 
///               incident ray direction vector and the normal vector.
/// \param[in] sinReflected The sine of the grazing reflected angle (i.e. 
///               90 degrees minus the angle of the incident ray with respect 
///               to the normal)
/// \param[out] reflectedDir The unit direction vector of the reflected ray 
///               (which may or may not include scattering)
/// \param[out] reflError Error code for this routine: 
///                         0 = OK
///                         1 = incident angle was 90 degrees or more
///
/// Calculate the direction vector of a reflected ray given:
/// (a) Initial direction vector of ray
/// (b) Normal vector at point of impact on the reflecting/scattering surface
/// (c) The sine of the grazing angle of incidence
/// (d) The sine of the grazing reflected angle (may or may not be equal to the 
///     incident angle, depending on whether scattering is included or not).
/// The Boolean isSpecular specifies whether specular reflection is being 
/// considered or not (in the interest of speed, different calculation paths 
/// are taken for the two cases).
void getReflectionDirection(const DirectionVector & initialDir, 
                            const DirectionVector & normal, 
                            bool isSpecular, 
                            double & sinIncident, 
                            double & sinReflected,
                            DirectionVector & reflectedDir, 
                            int & reflError);


/// \brief Generates a scattering angle around the specular direction after an 
///        X-ray reflects from a mirror or pre-collimator foil.
/// \param[in] incAngle Grazing incident angle for which a scattering angle is 
///               desired (radians)
/// \param[in] segmentID The index that accesses the scattering probability 
///               array, cumscatprob from the Scattering struct, by the 
///               segment number (not index) of the X-ray telescope.
/// \param[in] scatColIdx Index of column containing appropriate scattering data
/// \param[in] energyIdx The index for the energy dimension of the input 
///               scattering probability array, cumscatprob from the 
///               Scattering struct.
/// \param[in] scat Scattering struct containing all information from 
///               scattering file
/// \param[in] isRoughScattering True if the surface is either the back-side of 
///               a mirror or if it is a pre-collimator.
/// \param[out] scatteredAngle The scattered angle relative to the specular 
///               direction, generated randomly according to the 
///               measured scattering profiles tabulated in the 
///               original input scattering file. (radians)
///
/// Generates a scattering angle around the specular direction after an X-ray 
/// reflects from a mirror or pre-collimator foil. The routine takes an array 
/// of scattering probability distributions from a file read by the routine 
/// readscatteringfile(). In addition to a dependence on the deviations of a 
/// foil from the perfect form, the scattering probability is a function of 
/// photon energy, incident angle, scattering angle, and is obtained from 
/// measured reflectivity profiles.
/// One of the inputs to the routine is an index that selects a subarray of the 
/// scattering full probability distribution for a particular energy.
/// NOTE: The appropriate scattering probability versus scattering angle 
/// distribution, for a particular mirror or pre-collimator foil, for a given 
/// energy and incident angle, should be obtained by interpolation on energy 
/// and incident angle. However, since the probability distributions have 
/// erratic and often peculiar forms, regular interpolation cannot be relied 
/// upon to produce correct results. For the moment, the calling routine simply 
/// specifies an energy index that selects profiles closest in energy to the 
/// desired energy. In a more sophisticated approach, the scattering 
/// probability array should be re-mapped (outside of this routine) on to the 
/// ray-tracing energy grid, the re-mapping using a technique that would 
/// produce reliable scattering profiles between the measured ones, but such a 
/// technique has yet to be established. When that is achieved, we would still 
/// specify the energy slice by the same mechanism as done now in this routine, 
/// and what would be different is the scattering probability array itself 
/// (which would then be customized to the exact ray-trace energies outside of 
/// this routine).
/// This routine does not interpolate on incident angle either: it currently 
/// selects a slice of the scattering probability array simply be choosing the 
/// index of the incident angle grid that has the closest incident angle to the 
/// one that is input to the routine.
void getScatteredDirection(double incidentAngle, 
                           int segmentID, 
                           int scatColIdx, 
                           int energyIdx,
                           const Scattering & scat, 
                           bool isRoughScattering,
                           double & scatteredAngle);

/// \brief 
/// \param[in] 
/// \param[out] 
///
/// Calculate the normal vector at a given point on a surface with specified 
/// geometry. The normal vector is normalized to unity. Currently, cylindrical 
/// and cone surfaces are supported.
void getSurfaceNormal(surfaceGeometryTypes_e surfaceGeometry, 
                      const std::vector<double> & geoParams, 
                      objectTypes_e objectType, 
                      const Slopes & slopes, 
                      int face, 
                      const CartesianCoord & coordIn, 
                      DirectionVector & normal,
                      int normalError);


/// \brief
/// \param[in] 
/// \param[in] 
/// \param[in] 
/// \param[out] 
/// \param[out] 
///
/// Given a set of pairs of rotation matrices and linear shift vectors, combine 
/// all of the sets to produce an equivalent single rotation matrix and single 
/// shift vector
void calcNetTransformation(long numSteps, 
                           const vector3Ddbl & inputRotations,
                           const vector2Ddbl & inputShifts,
                           vector2Ddbl & netRotation, 
                           vector1Ddbl & netShift,
        
                           long objIndex);


/// \brief
/// \param[in] transformType Whether to transform position, direction, or both 
/// \param[in] coordIn Input (untransformed) Cartesian coordinates
/// \param[in] dirVecIn Input (untransformed) Cartesian components of direction 
///               vector
/// \param[in] xrtobjecttransform The parameters describing a single rotation 
///               and a single shift (appropriate for one XRT object), applied 
///               to the input coordinates and/or direction vector components.
/// \param[out] transformedCoord Transformed Cartesian coordinates 
///               (corresponding to the transformation applied to coordIn)
/// \param[out] transformedDirVec Transformed Cartesian dir. vector components 
///               (corresponding to the transformation applied to dirVecIn)
///
/// Applies coordinate transformations pertaining to a given XRT object to 
/// photon positions, direction vectors, and bounding boxes. The transformation 
/// parameters are supplied as input so the routine can be used to transform in 
/// both directions from one frame to another.
/// An input variable, transformtype, specifies whether to apply the transform 
/// to a position, a direction, or both. To transform photon or bounding box 
/// vertex positions, both the rotation and shift should be applied, whereas 
/// for photon directions, only the rotation should be applied.
void applyXRTTransform(transformType_e transformType, 
                       const CartesianCoord & coordIn,
                       const DirectionVector & dirVecIn,
                       const vector1Ddbl & xrtobjecttransform,
                       CartesianCoord & transformedCoord,
                       DirectionVector & transformedDirVec,
        long objIndex);


/// \brief Create transform (rotation and shift) matrices for XRT objects
/// \param[in] XRTObjects Array of telescope parts (foils and obstructions)
/// \param[in] numXRTObjects Total number of XRT objects found in TDF 
///               (obstructions plus mirrors plus pre-collimator foils)
/// \param[in] segmentRotationZOffset distance which defines the z-coordinate 
///               of the plane parallel to the x-y plane that contains the axes 
///               about which the segment misalignment rotation angles 
///               (xrtsegmentrotx and xrtsegmentroty) are defined. [mm] Not 
///               controllable by the user and assigned to its default value of 
///               the focallength of the telescope in xrtsetup() 
/// \param[in] misalignment_struct Stuct containing misalignment information
/// \param[out] transforms struct that contains the four tranforms matrices
///
/// Set up the individual steps for transforming XRT objects (composed of 
/// various misalignment parameters), and combine them into a single rotation 
/// and a single shift. Transformation arrays are created for each XRT object. 
/// The same is done for the reverse transformations. A subset of 
/// transformations (and their inverse) are also created for going from the XRT 
/// frame to a sector side wall and vice versa.
void setupXRTTransform(const std::vector<XRTObject> & XRTObjects, 
                       long numXRTObjects, 
                       double segmentRotationZOffset,
                       const Misalignment & misalignment_struct, 
                       Transforms & transforms);


/// \brief Transform a bounding box
/// \param[in] numXRTObjects Total number of XRT objects found in TDF 
///               (obstructions plus mirrors plus pre-collimator foils)
/// \param[in] xrttransform array that describes transformations to apply
/// \param[in/out] XRTObjects Array of telescope parts (foils and obstructions)
///                we're using XRTObjects members: 
///                 * m_BBox as the input bounding box, and
///                 * m_BBoxTrnsfrmd as the output transformed bounding box
///
/// 
void transformBBoxes(long numXRTObjects, 
                     int maxTransformXYZ, 
                     const vector2Ddbl & xrttransform, 
                     std::vector<XRTObject> & XRTObjects);


/// \brief reset all event counters to zero
/// \param[in/out] stats the struct containing all the event counters
///
/// All the event counters keep track of how many of each type of event.
void resetEventCounters(StatResults & stats);



/// \brief Updates the counters according to various raytrace outcome
/// \param[out] numinteractions Total number of interactions between the 
///               photon and XRT components, including the final focal-plane 
///               impact if there is one.
/// \param[out] pathCode Array of three integers per interaction that records 
///               what was impacted by the photon. 
/// \param[in] resultsPlaneWasImpacted
/// \param[in/out] stats Struct containing counters for different event types
/// \param[out] pathErrorCode Photon error codes
/// \param[out] sumpatherrorcode Sum of numinteractions values of patherrorcode
/// 
/// Cumulatively updates the appropriate counters according to various outcomes 
/// after each photon is ray-traced from beginning to termination.
void updateEventCounters(long numInteractions, 
                         const std::vector< std::vector<int> > & pathCode, 
                         bool resultsPlaneWasImpacted, 
                         StatResults & stats, 
                         std::vector<int> & pathErrorCode, 
                         int & sumpatherrorcode);



/**********************************************
 * 		write output functions
**********************************************/


/// \brief Writes one extension of PSF image FITS file
/// \param[in] param Struct containing the FITS file pointer to the PSF file.
/// \param[in] psfExtNum Extension number of current extension
/// \param[in] photons Struct containing number of offaxis and roll angles for 
///               keywords written to primary extension
/// \param[in] genTelescope Contains TELESCOP, INSTRUME, etc keyword values
/// \param[in] plateScale Value used to calculate FPMM2AM keyword
/// \param[in] energyidstr Energy or energy range, for Energy keyword
/// \param[in] offaxis Offaxis angle for this extension, written in OFFAXIS keyword
/// \param[in] azimuthangle Roll angle for this extension, written in AZIMUTH keyword
/// \param[in] totalpsfphotons Value written in TOTCTS keyword
/// \param[in] psfxcen Value written in XCENTER keyword
/// \param[in] psfycen Value written in YCENTER keyword
/// \param[in] psfLowerXCorner Value written in CRVAL1 keyword
/// \param[in] psfLowerYCorner Value written in CRVAL2 keyword
/// \param[in] deltacoord  Value written in CDELT1 and CDELT2 keywords
/// \param[in] psfimage Two-dim array containing PSF image
///
/// Writes one extension of a FITS file containing an image suitable for use as 
/// a PSF. The FITS file should already be open. Each extension contains data 
/// for a unique energy, or a single range of energy, and for a unique pair of 
/// values for the off-axis angle and azimuthal angle of source photons 
/// relative to the XRT optical axis.
void writePSFType1Ext(Param & param, 
                      int psfExtNum,
                      const Photons & photons,
                      const GenTelescope & genTelescope, 
                      double plateScale, 
                      const std::string & energyidstr, 
                      double offaxis, 
                      double azimuthangle, 
                      long totalpsfphotons, 
                      double psfxcen, 
                      double psfycen, 
                      double psfLowerXCorner, 
                      double psfLowerYCorner, 
                      double deltacoord, 
                      const vector2Ddbl & psfimage);



/// \brief Writes one extension of EEF FITS file
/// \param[in] param Struct containing the FITS file pointer to the EEF file.
/// \param[in] psfExtNum Extension number of current extension
/// \param[in] genTelescope Contains TELESCOP, INSTRUME, etc keyword values
/// \param[in] plateScale Value used to calculate FPMM2AM keyword
/// \param[in] energyidstr Energy or energy range, for Energy keyword
/// \param[in] offaxis Offaxis angle for this extension, written in OFFAXIS keyword
/// \param[in] azimuthangle Roll angle for this extension, written in AZIMUTH keyword
/// \param[in] totalpsfphotons Value written in TOTCTS keyword
/// \param[in] psfxcen Value written in XCENTER keyword
/// \param[in] psfycen Value written in YCENTER keyword
/// \param[in] radeef Array of radius values, for first column
/// \param[in] delta_radeef Value written in 1CDLT2 keyword
/// \param[in] totaleefenergy Value to normalize EEF
/// \param[in] eef Array of EEF values, for second column
///
/// Writes one extension of a FITS file containing the encircled energy 
/// fraction (EEF) as a function of radial distance from the central peak of 
/// the PSF. The FITS file should already be open. Each extension contains data 
/// for a unique energy, or a single range of energy, and for a unqiue pair of 
/// values for the off-axis angle and azimuthal angle of source photons 
/// relative to the XRT optical axis
void writePSFType2Ext(Param & param, 
                      int psfExtNum,
                      const GenTelescope & genTelescope, 
                      double plateScale, 
                      const std::string & energyidstr, 
                      double offaxis, 
                      double azimuthangle, 
                      long totalpsfphotons, 
                      double psfxcen, 
                      double psfycen, 
                      vector1Ddbl radeef, 
                      double delta_radeef, 
                      double totaleefenergy, 
                      const vector1Ddbl & eef);


/// \brief Writes one extension of EA FITS file
/// \param[in] param Struct containing the FITS file pointer to the EA file.
/// \param[in] eaExtNum Extension number of current extension
/// \param[in] genTelescope Contains TELESCOP, INSTRUME, etc keyword values
/// \param[in] energyidstr Energy or energy range, for Energy keyword
/// \param[in] offaxis Offaxis angle for this extension, written in OFFAXIS keyword
/// \param[in] azimuthangle Roll angle for this extension, written in AZIMUTH keyword
/// \param[in] energies Array of energies for first column
/// \param[in] effarea Array of effective area for second column
///
/// Writes one extension of a FITS file containing effective area versus energy 
/// data for a telescope. The FITS file should already be open. Each extension 
/// contains data for a unqiue pair of values for the off-axis angle and 
/// azimuthal angle of source photons relative to the XRT optical axis.
void writeEFFAreaExt(Param & param, 
                     int eaExtNum,
                     const GenTelescope & genTelescope, 
                     const std::string & energyidstr,
                     double offaxis, 
                     double azimuthangle, 
                     const vector1Ddbl & energies, 
                     const vector1Ddbl & effarea);


/// \brief Write the header to the first extension of the output history file
/// \param[in] param Struct containing the FITS file pointer to the history file
/// \param[in] stats Struct containing statistical information about photons
/// \param[in] photons Struct containing number of offaxis and roll angles 
/// \param[in] genTelescope Contains TELESCOP, INSTRUME, etc keyword values
/// \param[in] focalLength Focal length of telescope, obtained from the MIRROR 
///               extension header in the TDF (mm)
/// \param[in] plateScale Value used to calculate FPMM2AM keyword
/// \param[in] fullGeometricArea 
/// \param[in] apertureGeometricArea 
/// \param[in] ztelescopeaperture 
/// \param[in] apertureIsAnnulus 
/// \param[in] innerHousingRadius 
/// \param[in] outerHousingRadius 
/// \param[in] doTransforms 
/// \param[in] numPhotonEnergies 
/// \param[in] photonCountIn 
///
/// Writes numerous keywords to the ray-tracing code output event history file, 
/// from the statresults structure, which holds various counters pertaining to 
/// detailed results of the paths and interactions of a group of ray-traced 
/// photons. Many of the keyword values summarize various aggregate and 
/// statistical outcomes of the ray-tracing code.
void writeHistoryFileHeader(Param & param, 
                            const StatResults & stats, 
                            const Photons & photons, 
                            const GenTelescope & genTelescope, 
                            double focalLength, 
                            double plateScale, 
                            double fullGeometricArea, 
                            double apertureGeometricArea, 
                            double ztelescopeaperture, 
                            bool apertureIsAnnulus, 
                            double innerHousingRadius, 
                            double outerHousingRadius, 
                            bool doTransforms, 
                            long numPhotonEnergies,   // +++ this is straight from photons struct.  redundant param
                            long photonCountIn);


/// \brief Write the second extension of the output history file
/// \param[in] param Struct containing the FITS file pointer to the history file
/// \param[in] photons Struct containing the offaxis and roll angles 
///
/// Writes the second extension of the history file, INPUTPHOTONS, which lists 
/// different energies, roll angles, and off axis angles of input photons.
void writeHistoryFile2ndExtension(Param & param, 
                                  Photons & photons,
                                  const GenTelescope & genTelescope);


/// \brief Write common standard keywords to active HDU.
/// \param[in] param Param struct that holds all the parameters from the .par
/// \param[in] fitsfp the fits file pointer to the file to edit
///
/// Writes keywords that need to go into each active extension, such as 
/// DATE, VERSION, and HISTORY.
/// The DATE keyword writes the system date, to give date of file creation.
/// The VERSION keywords writes the version number for the raytracing code.
/// All the input params are stamped to the output file as HISTORY keywords, 
/// if the user selected "history=yes" in the input parameter.  The default 
/// behaviour is "history=yes". 
void writeCommonKeywords(Param & param, fitsfile ** fitsfp);


/// \brief 
/// \param[in] fitsfp 
/// \param[in] filename 
/// \param[in] photons 
///
/// 
void writePrimaryExtKeywords(fitsfile ** fitsfp,
                          const std::string & filename,
                          const Photons & photons);


/**********************************************
 * 		main work functions
**********************************************/


/// \brief Read a reflectivity/transmission FITS file created by xrtreftable
/// \param[in] param struct containing refl file FITS pointer, and user inputs 
///               about the collimator and transmission 
/// \param[in] photons struct containing information about the energy range 
///               needed
/// \param[in] numGroups Number of foil groups with similar front-side surface 
///               coatings. It is obtained from the SURFACE extension in TDF.
/// \param[in] frontRefColNames List of unique names of front-side reflectivity 
///               columns in the reflectivity file. It is derived from the 
///               freflect column in the MIRROR ext in the TDF.
/// \param[out] reflectTransGrids struct containing all the grids and 
///               information for the reflectivity and transmission
/// \param[out] doTransmission flag if transmission should be applied to 
///               raytracing.  If no mass absorption data is found in the 
///               reflect file, this is set to false.
/// \param[out] isMultiLayerSurface flag if the mirror surface is multi-layer.  
///               Found in the MULTLYR keyword in the reflect file.
///
/// The routine allows different energy grids for the front-side mirror data, 
/// the mass-absorption coefficient data, and the rough-surface material data.
/// The back-side of mirror foils, and the front and back of pre-collimator 
/// foils are referred to here as “rough-surface materials” (the surfaces are 
/// too rough for geometrical optics theory to apply and the reflectivity must 
/// be obtained empirically).
/// The back-side reflectivity and the pre-collimator reflectivity will 
/// eventually (outside this routine) be merged into a unified array called 
/// outRoughRefl. This requires that both the back-side and pre-collimator 
/// reflectivity have identical angle grids. The program will abort if this 
/// condition is not met. Even though m_backAngles and m_pcolAngles must be 
/// identical, they are treated as independent arrays for the sake of future 
/// generalization. 
/// Currently the code assumes that there is just one extension giving the 
/// reflectivity of the back-side of all mirror foils, and the file name and 
/// extension for this is in the parameter backreffile from raytrace.par.
/// For the purpose of transmission through the back of mirror foils to the 
/// bottom of the front-side thin-film coating the code will look for 
/// substrate information in the front-side reflectivity extension header and 
/// use the mass-absorption coefficients to calculate the optical depth per mm.
/// If the MASS_ABSORPTION extension is not found, the code will not abort, 
/// but instead it will set the boolean doTransmission=false, forcing the 
/// ray-tracing code to perform the simulations without treating transmission.
/// The pre-collimator extension is optional since the ray-tracing code should 
/// work for telescopes that don’t have a pre-collimator. 
/// The code assumes that the first extension in the reflectivity file is for 
/// the front-side of the mirror foils.
/// Note that at the moment the output array frontTaupermm is a place-holder 
/// and the data to fill it does not yet exist in the FITS file. It will simply 
/// contain all zeros for the moment.
void readReflectTrans(Param & param, 
                      const Photons & photons,
                      long numGroups, 
                      const std::vector<std::string> & frontRefColNames,
                      ReflectTransGrids & reflectTransGrids, 
                      std::vector<int> & xrtShellToGroupNumber,
                      bool & doTransmission, 
                      bool & isMultiLayerSurface);



/// \brief Interpolate and remap reflectivity, transmission, etc arrays onto a 
///        new energy array
/// \param[in] reflGrids struct containing the original reflectivity arrays, 
///               read from the reflectivity file in readReflectTrans
/// \param[in] numOutEnergies The number of energies in the energy grid onto 
///               which the reflectivity, transmission, and optical depth per 
///               mm grids will be remapped
/// \param[in] outEnergies The energy grid onto which the reflectivity, 
///               transmission, and optical depth per mm grids will be remapped
///               size = numOutEnergies
/// \param[in] numGroups Number of foil groups with similar front-side surface 
///               coatings
/// \param[in] pcolExists flag if precollimator exists 
/// \param[in] doTransmission flag if tool should treat transmission
/// \param[out] remappedGrids struct containing the output grids remapped onto 
///               the energy array OutEnergies
///
/// Interpolate and remap several arrays onto a new energy array. The input 
/// arrays are for reflectivity, transmission, and optical depth per mm as a 
/// function of energy, incident angle, and foil group number.
/// ALL of the input arrays are mapped on to a common energy grid.
/// The back-side reflectivity and the pre-collimator reflectivity are merged 
/// into a unified array called OutRoughRefl. This requires that both the back-
/// side and pre-collimator reflectivity have IDENTICAL angle grids. Even 
/// though BackAngles and PcolAngles must be identical, they are input as 
/// independent arrays for the sake of future generalization; they should be 
/// checked for equality in readReflectTrans. (Note that the optical depths of 
/// rough materials are also unified, in OutRoughTaupermm, but these arrays do 
/// not depend on the incident angle grids).
void remapReflectTrans(const ReflectTransGrids & reflGrids,
                       long numOutEnergies, 
                       const std::vector<double> & outEnergies, 
                       long numGroups, 
                       bool pcolExists, 
                       bool doTransmission, 
                       RemappedReflectTransGrids & remappedGrids);


/// \brief interpolate a return value for an input value, based on arrays of 
///        input and output
/// \param[in] numGridPts Size of the input arrays. Specified explicitly in the 
///               interest of run-time speed so that the routine does not have 
///               to deduce the array size each time.
/// \param[in] xGrid Array of 'x' values
/// \param[in] yGrid Array of corresponding 'y' values
/// \param[in] xIn The 'x' value for which we would like to find a 
///               corresponding 'y'
/// \param[out] yOut The interpolated value
///
/// Linearly interpolate on an input grid of values, using bisection to quickly 
/// locate the two interpolation positions.
/// Note that the routine does no range checking
void bisectionInterp(long numGridPts, 
                     const std::vector<double> & xGrid, 
                     const std::vector<double> & yGrid, 
                     double xIn, double & yOut);


/// \brief Locate the pair of grid points that enclose the input value
/// \param[in] numGridPts Size of the input arrays. Specified explicitly in the 
///               interest of run-time speed so that the routine does not have 
///               to deduce the array size each time.
/// \param[in] xGrid Array of 'x' values
/// \param[in] xIn The 'x' value for which we would like to find a 
///               corresponding 'y'
/// \param[out] index1 index pointing to the array element in xGrid that is 
///               closest to xIn but is less than xin
/// \param[out] index2 index pointing to the array element in xGrid that is 
///               closest to xIn but is greater than xin
///
/// Given an array of x values (size numgridpts, values in gridx) and an input 
/// value, xin, locate the pair of grid points that enclose the input value. 
/// The outputs are two indices, xindex1 and xindex2 which point to the array 
/// elements in gridx (the 1st value in gridx has an index of 0).  Although 
/// xindex2=xindex1+ 1 always, both indices are returned for convenience (also, 
/// special treatment is required when xin is near or equal to one of the end 
/// points of gridx)
/// Note that the routine does no range checking
void bisectionLocate(const std::vector<double> & xGrid, 
                     long numGridPts, 
                     double xin, 
                     int & index1, 
                     int & index2);


/// readReflectTrans helper functions:


/// \brief Read and store mass absorption data from the reflectivity file
/// \param[in] refl_fp FITS file pointer to the relectivity file
/// \param[in] param struct containing information from input parameters.  this 
///               function uses the name of the refl file and precollimator 
///               reflecitivity file
/// \param[in] numMaterials Number of materials that have mass absorption 
///               coefficients in the file, from the keyword NMATERIA in the 
///               front reflectivity extension
/// \param[in] numSubstrates Number of subtrates in the file, from the keyword 
///               NSUBSTRA in the front reflectivity extension
/// \param[in] substrateIndices array of indices for each substrate, pointing 
///               to column number, from the keyword SUBSTRnn in the front 
///               reflectivity extension
/// \param[in] substrateThickness array of thicknesses for each substrate, from 
///               the keyword SUBTHKnn in the front reflectivity extension
/// \param[in] totalSubstrateThickness sum total thickness of all substrates
/// \param[in] pcolMaterialIndex index of the precollimator material, from the 
///               keyword PCOLMTRL in the front reflectivity extension
/// \param[in] firstNeededRow first row to get from MASS_ABSORPTION extension, 
///               according to which energies we want
/// \param[out] reflectTransGrids struct containing all the grids and 
///               information for the reflectivity and transmission  
/// \param[out] xrtShellToGroupNumber 
///
/// Stores information about mass absorption, such as tau per mm for each of 
/// the substrates. +++ add comments from function
void getMassAbsorptionData(fitsfile * refl_fp, 
                           Param & param, 
                           int numMaterials, 
                           long numGroups, 
                           int numSubstrates, 
                           const std::vector<int> & substrateIndices, 
                           const std::vector<double> & substrateThickness,
                           double totalSubstrateThickness, 
                           int pcolMaterialIndex, 
                           long firstNeededRow, 
                           ReflectTransGrids & reflectTransGrids, 
                           std::vector<int> & xrtShellToGroupNumber);


/// \brief Read and store data about the back side of the mirror
/// \param[in] param
/// \param[in] photons
/// \param[out] reflectTransGrids
///
/// Called from inside readReflectTrans, if user selects to do transmission.
void getMirrorBackData(Param & param, 
                       const Photons & photons, 
                       ReflectTransGrids & reflectTransGrids);


/// \brief Read and store data about the pre-collimator
/// \param[in] param
/// \param[in] photons
/// \param[out] reflectTransGrids
/// 
/// Called from inside readReflectTrans, if user selects to do transmission
/// and if a pre-collimator file is provided.
void getMirrorPcolData(Param & param, 
                       const Photons & photons,
                       ReflectTransGrids & reflectTransGrids);
                          


/**********************************************
 * 		misc work functions
**********************************************/


/// \brief Create a bounding box for an obstruction
/// \param[in] vertices The coords of each vertex of the structure
/// \param[out] obstructBBoxCoords defining coordinates of bounding box:
///             [0] = min x value   [1] = max x value
///             [2] = min y value   [3] = max y value
///             [4] = min z value   [5] = max z value
/// \param[out] obstructVertices actual x,y,z coords of the eight vertices:
///             [0][*] = eight x coords
///             [1][*] = eight corresponding y coords
///             [2][*] = eight corresponding z coords
///
/// Calculate the six Cartesian parameters of the bounding box for a telescope
/// support structure (in general any object that can be defined as a 2-D 
/// polygon, or a 3-D structure given only the coordinates of its vertices). 
/// The routine does not need to know whether the structure is 2-D or 3-D: 
/// A 2-D structure will give a 2-D bounding box.
/// The units of length do not matter; the output bounding values of x, y, z 
/// will be given in the same units as the inputs.
void makeObstructBBox(const std::vector<CartesianCoord> & vertices, 
                      ObjectBoundingBox & obstructBBoxCoords, 
                      std::vector<CartesianCoord> & obstructBBoxVertices);


/// \brief Create a bounding box for a mirror foil
/// \param[in] rBottomInner Radial distance of the bottom (lowest z-coord) 
///            inner circular edge from the z-axis
/// \param[in] rBottomOuter Radial distance of the bottom (lowest z-coord) 
///            outer circular edge from the z-axis
/// \param[in] rTopInner Radial distance of the top (highest z-coord) 
///            inner circular edge from the z-axis
/// \param[in] rTopOuter Radial distance of the top (highest z-coord) 
///            outer circular edge from the z-axis
/// \param[in] angleStart Azimuthal angle start boundary of foil edge (radians)
/// \param[in] angleEnd zimuthal angle end boundary of foil edge (radians)
/// \param[in] zBottom z-coord of all points on the bottom edges of the foil
/// \param[in] zTop z-coord of all points on the top edges of the foil
/// \param[out] foilBBoxCoords The defining coordinates of the bounding box:
///             [0] = min x value   [1] = max x value
///             [2] = min y value   [3] = max y value
///             [4] = min z value   [5] = max z value
/// \param[out] foilVertices The actual x,y,z coords of the eight vertices:
///             [0][*] = eight x coords
///             [1][*] = eight corresponding y coords
///             [2][*] = eight corresponding z coords
/// \param[out] foilCoordQuad Integers between 1 and 4 specifying the Cartesian 
///             coordinate quadrants occupied by the foil (this can be used to 
///             reduce run-time speed in calculating angles from inverse trig 
///             functions). 
///             Note that Cartesian quadrant is not to be confused with 
///             telescope quadrant- the former strictly refers to the Cartesian 
///             coordinate axes.
///             foilCoordQuad[0] = quadrant in which foil begins 
///                                (i.e. where angleStart is located)
///             foilcoordquad[1] = quadrant in which foil begins 
///                                (i.e. where angleEnd is located)
///             foilcoordquad[3] = number of quadrants spanned by foil 
///
/// Calculate the six Cartesian parameters of the bounding box for a mirror or 
/// pre-collimator foil (in general any object that can be defined as a solid 
/// formed by a quadrilateral rotated around the z-axis between two specified 
/// angles).
/// The units of length do not matter; the output bounding values of x, y, z 
/// will be given in the same units as the inputs.
void makeFoilBBox(double rBottomInner, 
                  double rBottomOuter, 
                  double rTopInner, 
                  double rTopOuter, 
                  double angleStartIn, 
                  double angleEndIn, 
                  double zBottom, 
                  double zTop,
                  std::vector<double> & foilBBoxCoords,
                  ObjectBoundingBox & foilBBox, 
                  std::vector< std::vector<double> > & foilVertices,
                  std::vector<int> & foilCoordQuad);


/// \brief 
/// \param[in] 
/// \param[out] photonBBox The resulting boudning box
///
/// Create a bounding box (with sides parallel to Cartesian coordinates axes) 
/// around a photon path, given the photon initial position, its direction unit 
/// vector, and the target z-coordinate on which the path ends.
void makePhotonBBox(const CartesianCoord & photonPos, 
                    const DirectionVector & photonDir, 
                    double zEnd, 
                    double housingHardLimit, 
                    PhotonBoundingBox & photonBBox, 
                    int & photonBBoxError);


/// \brief 
/// \param[in] randseed Random number seed
/// \param[in] flatRadius radius of the source in arcmin 
///            (input parameter flatradius from raytrace.par file)
/// \param[out] flatCircleTheta Polar angle of photon position relative to 
///             center, in spherical polar coords, with source center at pole
///             (rad)
/// \param[out] flatCirclePhi The azimuthal angle of photon position in 
///             spherical polar coords, with source center at pole (rad)
///
/// Generate a random photon position in a circular source with a spatially 
/// uniform photon number distribution. The source center is located at the 
/// pole in spherical coordinates, and the ouput of the routine is the position 
/// of the photon in spherical polar coordinates. The actual photon position 
/// given the actual source center relative to the telescope coordinate system 
/// should be computed outside the routine.
void getFlatCirclePhoton(long randSeed, 
                         double flatRadius, 
                         double & flatCircleTheta, 
                         double & flatCirclePhi);


/// \brief 
/// \param[in] randseed Random number seed
/// \param[in] coreRadius Core radius parameter of the beta model in arcmin 
///            (1st value in input parameter betapars from raytrace.par file)
/// \param[in] beta The beta parameter of the beta model 
///            (2nd value in input parameter betapars from raytrace.par file)
/// \param[in] betaMaxRadius Maximum radius parameter of beta model in arcmin 
///            (3rd value in input parameter betapars from raytrace.par file)
/// \param[out] betaModelTheta Polar angle of the photon position relative to 
///             center, in spherical polar coords, with source center at pole
/// \param[out] betaModelPhi The azimuthal angle of the photon position in 
///             spherical polar coords, with source center at pole
///
/// Generate a random photon position in a circular source with a spatial 
/// distribution described by the beta model. The source center is located at 
/// the pole in spherical coordinates, and the ouput of the routine is the 
/// position of the photon in spherical polar coordinates. The actual photon 
/// position given the actual source center relative to the telescope 
/// coordinate system should be computed outside the routine.
void getBetaModelPhoton(long randseed, 
                        double coreRadius, 
                        double beta, 
                        double betaMaxRadius, 
                        double & betaModelTheta, 
                        double & betaModelPhi);


/// \brief 
/// \param[in] centerCosTheta Cosine of polar angle of actual center of source
/// \param[in] centerSinTheta Sine of polar angle of actual center of source
/// \param[in] centerCosPhi Cosine of azimuthal angle of actual center of source
/// \param[in] centerSinPhi Sine of azimuthal angle of actual center of source
/// \param[in] offsetTheta Polar angle of desired position relative to center 
///             of source
/// \param[in] offsetPhi Azimuthal angle of desired position if center of 
///            source is at the pole
/// \param[out] coordFinal Actual x,y,z coords of desired position if center of 
///             source is at (centertheta, centerphi)
///
/// Given the position in spherical polar coordinates in a source whose center 
/// is at the pole, calculate the Cartesian coordinates of the same position if 
/// the center of the source is not at the pole, but at spherical polar angles 
/// centertheta and centerphi.
/// Note that instead of actually specifying centertheta and centerphi as 
/// input, we pre-calculate and input the cosines and sines of these angles in 
/// the interest of run-time speed since the center of the source will have 
/// fixed coordinates while we select photon positions within the source.
/// All angles are in radians.
void sourceLocalToAbsolutePos(double centerCosTheta, 
                              double centerSinTheta, 
                              double centerCosPhi, 
                              double centerSinPhi, 
                              double offsetTheta, 
                              double offsetPhi,
                              CartesianCoord & coordFinal);


/// \brief Conve
/// \param[in] coordIn Initial position, with initial x, y, z
/// \param[in] dirVec Initial direction vector
/// \param[out] coordOut Final position.  z is set to the desired value before
///               entereing this function
/// \param[out] tImpact Time between initial and final positions; this is equal 
///               to the distance traveled between the two points if the 
///               direction vector is a unit vector
///
/// Given an inital position (xi,yi,zi in Cartesian coordinates), and an 
/// initial direction vector (Cartesian components vx,vy,vz), and a projected 
/// new z coordinate (znew), find the new x and y coordinates (xnew, ynew) of 
/// the new projected position, corresponding to the new z coordinate. Note 
/// that the direction vector ("velocity") should be normalized to unity if we 
/// want to equate time and distance variables without another constant.
/// coordIn
void getXYForNewZ(const CartesianCoord & coordIn, 
                  const DirectionVector & dirVec, 
                  CartesianCoord & coordOut, 
                  double & tImpact);


/// \brief Convert spherical vector to Cartesian
/// \param[in] theta Polar angle in spherical coordinates (radians)
/// \param[in] phi Azimuthal angle in spherical coordinates (radians)
/// \param[out] dirVec The resulting unit vector, with x,y,z components
///
/// Convert a unit vector in spherical polar coordinates to Cartesian 
/// coordinates.
void sphericalToCartesianDir(double theta, 
                             double phi, 
                             DirectionVector & dirVec);


// +++ I should make this output a PolarCoord struct
/// \brief Convert a 2-D cartesian vector to cylindrical polar coordinates
/// \param[in] coordIn Cartesian coordinate, with desired x and y coords
/// \param[out] radius The radial component in cylindrical coordinates
/// \param[out] phi The azimuthal angle in cylindrical coordinates (radian)
///
/// 
void cartesianToPolar(const CartesianCoord & coordIn, 
                      double & radius, 
                      double & phi);


/// \brief 
/// \param[in] 
/// \param[out] 
/// 
/// This function takes an input string array and creates a smaller array 
/// containing unique strings from the input array, and an index mapping each 
/// element position of the input array onto the unique string array. The 
/// routine may be called more than once, with a different input array each 
/// time, but cumulatively building on the same unique string array (which is 
/// therefore both an input and an output). An index array is created for each 
/// input string each time the routine is called.
/// On the very first call (or when a fresh unique string array is desired), 
/// the parameter input nunique is set to 0. Upon excecution, the value of 
/// nunique is updated to the total number of unique strings found and this 
/// value of nunique is used on the next call to the subroutine.
void indexUniqueNames(const std::vector<std::string> namesIn, 
                      int firstIndex,
                      int & numUniqueNames, 
                      std::vector<std::string> & uniqueNames, 
                      std::vector<int> & nameIndex);




/**********************************************
 * 		parameter retrieval functions
**********************************************/


/// \brief Report an error with getting a parameter
/// \param[in] parname name of parameter that caused an error
/// \param[in] status the error status that was encountered
/// \return message string if an error occurred getting a parameter
/// \internal
/// \note this code is copied from hitomi/ahapp
std::string reportGetParErr(const std::string & parname, int status);

/// \brief Get string parameter
/// \param[in] parname name of parameter
/// \return string value of parameter
/// \internal
/// \note this code is copied from hitomi/ahapp
std::string getParString(const std::string & parname);

/// \brief Get boolean parameter
/// \param[in] parname name of parameter
/// \return boolean value of parameter
/// \internal
/// \note this code is copied from hitomi/ahapp
bool getParBool(const std::string & parname);

/// \brief Get double parameter
/// \param[in] parname name of parameter
/// \return double value of parameter
/// \internal
/// \note this code is copied from hitomi/ahapp
double getParDouble(const std::string & parname);

/// \brief Get integer parameter
/// \param[in] parname name of parameter
/// \return int value of parameter
/// \internal
/// \note this code is copied from hitomi/ahapp
int getParInt(const std::string & parname);

/// \brief converts a char array to a string
/// \param[in] input character array to turn into a string
/// \return C++ string value of the input C style string
std::string convertCharToString(char * input);

/// \brief returns if an actual input file (or caldb) was provided
/// \param[in] input parameter variable name
/// \return true if a file was provided; false if none was provided
/// 
/// Examines the input parameter from the user to determine if the user provided
/// a filename.  Function does not check if it was a valid file, only if a 
/// string was provided, and the string is not "none"
bool isInputFileGiven(const std::string & infile);

/// \brief convert a string (list of numbers) into a vector of doubles
/// \param[in] stringList The string that holds the list of numbers
/// \param[out] doubleList The vector of numbers, as doubles
///
/// Convert a string which is a list of numbers, from an input parameter such 
/// as offaxis, into a one dimensional vector of doubles.  
/// douhbleList should not be resized before entering (it uses push_back)
void listStringsToDoubles(const std::string & stringList, 
                          vector1Ddbl & doubleList);

/// \brief return resolved filename: from CALBD or REFDATA or the original
/// \param[in] filename either the name of FITS file, or CALDB, or REFDATA
/// \param[in] filetype one- or two-word description of file, for info messages
/// \param[in] instrume INSTRUME keyword from par file.
/// \param[in] detnam DETNAM keyword
/// \param[in] codename keyword for searching CALDB CODENAM keywords
/// \param[in] datetime DATE-OBS keyword
/// \param[in] expression optional expression, ie for CBD keywords. default="-"
/// \param[in] telescop TELESCOP keyword from input file, optional. For 
///                   leapsec file, this would be 'gen'. Every other tool would 
///                   use the TELESCOP keyword, which should always be 
///                   "HITOMI".  default="HITOMI"
/// \return actual name of file
/// \internal
/// this is copied from hitomi/mission/lib/ahmission/ahmission/caldb.h
std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression="-",
                    const std::string & telescop="HITOMI");


/**********************************************
 * 		random number generator functions
**********************************************/


HDmt_state* & getRandSeedState();
void seedRandom(unsigned long int seed);
double getRandom(void);   // +++ make sure this is: uniform random number between 0.0 and 1.0 using (seed)

///  Generate a random number from a Gaussian distribution with a mean of 0 and a sigma of 1
/// \internal
/// adapted from http://en.literateprograms.org/index.php?title=Special:DownloadCode/Box-Muller_transform_%28C%29&oldid=7011
double getRandomGaussian(void);

void freeRandom(void);



/**********************************************
 * 		misc functions
**********************************************/


void storeEnergyInkeV(energyUnits_e energyUnit, vector1Ddbl & energyGrid);


void getRowsInEnergyRange(fitsfile * fits_fp, 
                          const std::string & filename,
                          const Photons & photons, 
                          energyUnits_e energyUnit, 
                          int energyColNum, 
                          long & firstNeededRow, 
                          long & lastNeededRow);


energyUnits_e getEnergyUnit(fitsfile * fits_fp, const std::string & filename,
                          int energyColNum);

double getMinDouble(std::vector<double> & vec);
double getMaxDouble(std::vector<double> & vec);
int getMinInt(std::vector<int> & vec);
int getMaxInt(std::vector<int> & vec);
long getMinLong(std::vector<long> & vec);
long getMaxLong(std::vector<long> & vec);

/// 
/// \internal
/// http://www.altdev.co/2012/02/22/comparing-floating-point-numbers-2012-edition/#sthash.e8rxzk5h.dpuf
bool doublesAreEqual(double inA, double inB, double maxRelDiff);


/// \brief returns a string version of an double
/// \param[in] value the double that needs to be a string
/// \return a string version of the double
std::string doubleToString(double value);

/// \brief returns a string version of an int
/// \param[in] value the int that needs to be a string
/// \return a string version of the int
std::string intToString(int value);

/// \brief returns a string version of a long
/// \param[in] value the long that needs to be a string
/// \return a string version of the long
std::string longToString(long value);


/// \brief string comparison, case insensitive
/// \param[in] str1 the first string to compare
/// \param[in] str2 the second string to compare
/// \return true if the strings are equal, case insensitive; false if not
bool isEqualCaseInsens(const std::string & str1, const std::string & str2);


/// \brief searches for a substring in longer string, returns true if found
/// \input mainStr the string in which we want to search for str2
/// \input subStr the string that we want to search for in str1
/// \return true if subStr was found in mainStr, otherwise false
bool foundCaseInsens(const std::string & mainStr, const std::string & subStr);

/// \brief return a filename, stripped of any path
/// \param[in] the entire filename, possibly including the path
/// \return the sole filename, without path
std::string getFilename(const std::string & str);

/// \brief Checks that the CFITSIO status is 0.  If not, it throws an error.
/// \param[in] status the status to check, returned from a prior CFITSIO call 
/// \param[in] doing what the prior CFITSIO call was doing 
/// \param[in] filename The file currently being accessed 
/// \internal
/// \note this code is adapted from attconvert function reportFITSError()
void checkForFITSError(const int status, const std::string & doing, const std::string & filename);

/// \brief overload the output operator to print a CartesianCoord struct
st_stream::OStream & operator<<(st_stream::OStream & os, const CartesianCoord & coord);

/// \brief overload the output operator to print a DirectionVector struct
st_stream::OStream & operator<<(st_stream::OStream & os, const DirectionVector & vec);

/// \brief Writes the input parameters to the log file
/// \internal
/// \note This code copied from hitomi/ahapp.cxx
void writeParametersToLog();

// end addtogroup
/** @} */


#endif // RAYTRACE_RAYTRACE_LIB_H




/* Revision Log
 $Log: xrtraytrace_lib.h,v $
 Revision 1.23  2016/11/10 20:32:34  mdutka
 Xrtraytrace will now account for rotations in the telescop definition file

 Revision 1.22  2016/03/25 17:25:03  klrutkow
 implemented new way to output progress, only printing 10 messages to user on how many photons processed (using new variable m_photonPrintIncrement)

 Revision 1.21  2016/03/24 21:59:57  klrutkow
 added variable m_totalNumPhotons to Photons struct ; changed any instance of mission name to Hitomi

 Revision 1.20  2016/02/18 21:54:12  klrutkow
 changed default TELESCOP to HITOMI in resolve() function

 Revision 1.19  2015/09/15 17:20:18  klrutkow
 added writeParametersToLog ; use enum to check param source values

 Revision 1.18  2015/09/10 02:35:09  klrutkow
 add telescop and instrume params to readScatteringFile()

 Revision 1.17  2015/08/13 03:32:11  klrutkow
 add genTelescope as argument to writeHistoryFile2ndExtension ; renamed writeParamHistoryKeywords to writeCommonKeywords ; added resolve() function to query CALDB

 Revision 1.16  2015/06/29 15:27:48  klrutkow
 added telescop and instrume to Param struct ; added longToString function

 Revision 1.15  2015/04/30 23:39:41  klrutkow
 adding more comments, documentation ; updated signature for remapReflectTrans ; removed my testing createRemappedFITSFile ; write DETNAM keyword to all output files

 Revision 1.14  2015/03/23 14:34:53  klrutkow
 update documentation; moved telescop, instrume, detnam function params to struct genTelescope

 Revision 1.13  2015/02/09 02:18:53  klrutkow
 corrected the value written to FGEOAREA keyword in history file: added
   m_fullGeometricAreaSqmm to Aperture struct

 Revision 1.12  2015/01/29 03:17:42  klrutkow
 updated with new params, per issue 472

 Revision 1.11  2015/01/29 03:06:47  klrutkow
 temporarily output scattering angle from raytraceOnePhoton()

 Revision 1.10  2015/01/16 15:59:43  klrutkow
 added stats.numrpiDoubleReflPerEnergy variable; changed s_deltaShells to 1; commented out default destructors for CartesianCoord and DirectionVector

 Revision 1.9  2014/12/29 17:20:15  klrutkow
 new params writeonlyresultsplane, phisttype, externalobjects, fastmode; removed param coordhist; fixed error with TDF, extension SEGMENT, columns DELTATX Y and Z: store in rad not arcmin; set doTransforms default to true instead of false; made transformType an enum; changed m_maxTransformXYZ from 12 to 14; new function getImpactCandidates(); new structs to hold telescope information: sectorsShells, ExternalObjects, GenTelescope; added variables to existing structs (topextobjectstrans, etc)

 Revision 1.8  2014/11/17 18:01:38  klrutkow
 created PSF_EEF and EA structs, moved vars from Photons struct; created enum psfType_e; removed zminsortxrtobjectindexAll (since we now use zmaxsort...); made some function input params const (getReflTrans, etc)

 Revision 1.7  2014/11/07 15:25:46  klrutkow
 initialized variables to 0 in structs; initialized all bools; updated offaxis and roll variable names to include units; new, faster algorithm for interceptXRTObjects; implement heaapp for startup() and shutdown()

 Revision 1.6  2014/10/06 22:47:33  klrutkow
 updated code for xrtraytrace_suite.v1.03: add scattering implementation; changed misalignment implementation to remove random misalignment; allow user to go below inner housing radius; fix bugs in setupXRTTransform; fix keywords in output files for offaxis, psfcenter, fix offset for psf image, added 4 columns to end of history file; debugged betamodel, flatcircle, and photon list modes; if annulus and rectangle are turned off, aperture defaults to annulus

 Revision 1.5  2014/10/03 16:48:56  klrutkow
 code as of v1.02 Sept 11 2014: ensure NGROUPS matches between reflectivity files and TDF; update getPhotonObjectImpactCoords() to correctly hit top of foils, removed last 'killPhoton' flag, better calculate front and back boundaries, better calculate top and bottom boundaries; updated probability calculcations in raytraceonephoton() for absorption, transmission, or reflection; added ability in diagnosticmode to input initial x and y; small edit to updateEventCounters(); abort if user enters malign=gauss but wrong number of doubles for ftilt etc; added .clear() to refl and transm grids before bisectionInterp() in raytraceonephoton(); if numInteractions goes above maxnumpathcoords, stop that photon with an error code instead of aborting program

 Revision 1.4  2014/09/04 16:24:06  klrutkow
 updates for v1.01 delivery

 Revision 1.3  2014/08/12 18:19:31  klrutkow
 fixed error when converting from double to int

 Revision 1.2  2014/08/12 15:12:48  klrutkow
 added headas_utils.h to get headas params

*/
