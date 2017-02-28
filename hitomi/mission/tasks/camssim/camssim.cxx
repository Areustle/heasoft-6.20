/// \file camssim.cxx
/// \brief Simulates motion of the extensible optical bench (EOB).
/// \author Andy Sargent
/// \date $Date: 2016/04/14 19:56:05 $

/**

/// \defgroup tool_camssim CAMS simulation (camssim)
/// @ingroup mod_mission_tasks

This tool simulates the motion of the extensible optical bench (EOB).  It produces
simulated CAMS displacement data files.  It modifies HXI event files by generating
RAW coordinates that are a function of EOB motion.

Source files:

  camssim.cxx

Library depenencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  attitude/lib/coordfits
  attitude/lib/atFunctions
  hitomi/gen/lib/ahapp
  hitomi/mission/lib/ahmission

Modification history:
 1.0   2015-09-04  RSH     Initial implementation


*/

#define AHLABEL tool_camssim
#define AHCVSID "$Id: camssim.cxx,v 1.41 2016/04/14 19:56:05 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ape/ape_trad.h"
#include "ape/ape_error.h"

#include "ahgen/ahgen.h"          // random number generator
#include "ahmission/ahmission.h"  // getTELESCOPString
#include "ahmission/hxiteldef.h"  // HXI TelDef struct
#include "ahmission/camsteldef.h" // CAMS TelDef struct
#include "ahmission/caldb.h"      // CALDB filename resolver
#include "ahmission/keyword.h"    // copy keywords
#include "atFunctions.h"          // Attitude vector/matrix functions
#include "atError.h"              // Attitude vector/matrix errors
#include "coordfits2.h"           // Points to various TelDef Libraries 

#include <cmath> // used for trigonometric functions.

// Determine sign of a variable
#define sign(X) ( ( (X) > 0 ) - ( (X) < 0 ) )

// atFunctions rotation matrix has 9 elements.
// Set all elements:
// A B C
// D E F
// G H I
// (1,0,0) transforms to (A,D,G)
// (0,1,0) transforms to (B,E,H)
// (0,0,1) transforms to (C,F,I)
#define setMatrix(M,A,B,C,D,E,F,G,H,I) \
((M)[0][0] = (A), \
 (M)[0][1] = (B), \
 (M)[0][2] = (C), \
 (M)[1][0] = (D), \
 (M)[1][1] = (E), \
 (M)[1][2] = (F), \
 (M)[2][0] = (G), \
 (M)[2][1] = (H), \
 (M)[2][2] = (I) \
)

//  Set matrix to [1].
#define setMatrixIdentity(M) \
((M)[0][0] = 1, \
 (M)[0][1] = 0, \
 (M)[0][2] = 0, \
 (M)[1][0] = 0, \
 (M)[1][1] = 1, \
 (M)[1][2] = 0, \
 (M)[2][0] = 0, \
 (M)[2][1] = 0, \
 (M)[2][2] = 1 \
)

//  Copy matrix N into matrix M.
#define copyMatrix(M,N) \
((M)[0][0] = (N)[0][0], \
 (M)[0][1] = (N)[0][1], \
 (M)[0][2] = (N)[0][2], \
 (M)[1][0] = (N)[1][0], \
 (M)[1][1] = (N)[1][1], \
 (M)[1][2] = (N)[1][2], \
 (M)[2][0] = (N)[2][0], \
 (M)[2][1] = (N)[2][1], \
 (M)[2][2] = (N)[2][2] \
)

/** \addtogroup tool_camssim
 *  @{
 */

const int ISIZE = 16384;


struct Params {

  std::string m_infile1;          ///< Input event file name
  std::string m_infile2;          ///< Input event file name

  std::string m_outroothxi;   ///< Output file name root for HXI
  std::string m_outrootcams;  ///< Output file name root for CAMS  
  
  std::string m_teldefhxi1;       ///< Input CALDB HXI1 teledef file
  std::string m_teldefhxi2;       ///< Input CALDB HXI2 teledef file
  std::string m_teldefcams1;      ///< Input CALDB CAMS1 teledef file
  std::string m_teldefcams2;      ///< Input CALDB CAMS2 teledef file
  
  double m_freq1;         ///< Frequency of the 1st sinusoid in the motion (Hz)
  double m_freq2;         ///< Frequency of the 2nd sinusoid in the motion (Hz)
  
  double m_xamp1;         ///< Amplitude of  X-motion (translation) for 1st sinusoid (mm) 
  double m_yamp1;         ///< Amplitude of  Y-motion (translation) for 1st sinusoid (mm)
  double m_xamp2;         ///< Amplitude of  X-motion (translation) for 2nd sinusoid (mm)
  double m_yamp2;         ///< Amplitude of  Y-motion (translation) for 2nd sinusoid (mm)
  double m_ramp1;         ///< Amplitude of  twist motion for 1st sinusoid (mrad)
  double m_ramp2;         ///< Amplitude of  twist motion for 2nd sinusoid (mrad)
  
  int m_typemotion;       ///< Type of motion 1=XYin phase, 2=XYout of phase  3=XY 90deg out of phase
  
  double m_deltatcams;    ///< Time interval in which the CAMS records 5 measurements (d/f 1s)
  
  double m_randerr;       ///< Magnitude of  CAMS error: random component (mm)
  double m_consterr;      ///< Magnitude of  CAMS error: random component (mm)
  double m_sinerr;        ///< Magnitude of  CAMS error: random component (mm)
  double m_freqerr;       ///< Frequency of the sinusoidal error component (Hz)
  double m_seed;          ///< Seed for random number. Time variable is used if seed is set to 0 (default). 
  
};

// ****************************************************************************
// Function declarations

/// \brief Read parameters and store related quantities
/// \param[in,out] param  Parameter structure
void getPar(Params & param);

/// \brief Read TelDef files and open input and output data files.
/// \param[in,out] param  Parameter structure
/// \param[out] cams1     CAMS1 TelDef structure
/// \param[out] cams2     CAMS2 TelDef structure
/// \param[out] hxi1      HXI1 TelDef structure
/// \param[out] hxi2      HXI2 TelDef structure
/// \param[out] hxi_std   HXI standard TelDef structure
/// \param[out] tstart    Earliest start time from HXI files
/// \param[out] tstop     Latest stop time from HXI files
/// \param[out] fph1      File pointer to HXI1 file
/// \param[out] fph2      File pointer to HXI2 file
/// \param[out] fpc1      File pointer to CAMS1 file
/// \param[out] fpc2      File pointer to CAMS2 file
void initialize(Params& param, ahmission::teldef::CAMSTelDef& cams1, 
                ahmission::teldef::CAMSTelDef& cams2, ahmission::teldef::HXITelDef& hxi1,
                ahmission::teldef::HXITelDef& hxi2, TELDEF2*& hxi_std,
                double & tstart, double & tstop,
                ahfits::FilePtr & fph1, ahfits::FilePtr & fph2,
                ahfits::FilePtr & fpc1, ahfits::FilePtr & fpc2);

/// \brief Process the CAMS position data files and produce the output offsets file.
/// \param[in,out]param   Parameter structure
/// \param[in] cams1      CAMS1 TelDef structure
/// \param[in] cams2      CAMS2 TelDef structure
/// \param[in] hxi1       HXI1 TelDef structure
/// \param[in] hxi2       HXI2 TelDef structure
/// \param[in] hxi_std    HXI standard TelDef structure
/// \param[in] tstart     Earliest start time from HXI files
/// \param[in] tstop      Latest stop time from HXI files
/// \param[in] fph1       File pointer to HXI1 file
/// \param[in] fph2       File pointer to HXI2 file
/// \param[in] fpc1       File pointer to CAMS1 file
/// \param[in] fpc2       File pointer to CAMS2 file
void doWork(Params& param, const ahmission::teldef::CAMSTelDef& cams1, 
            const ahmission::teldef::CAMSTelDef& cams2, 
            const ahmission::teldef::HXITelDef& hxi1, 
            const ahmission::teldef::HXITelDef& hxi2, 
            TELDEF2*& hxi_std, double tstart, double tstop,
            ahfits::FilePtr fph1, ahfits::FilePtr fph2,
            ahfits::FilePtr fpc1, ahfits::FilePtr fpc2);

/// \brief Close files and free memory.
/// \param[in] hxi_std HXI standard TelDef structure
/// \param[in] fph1       File pointer to HXI1 file
/// \param[in] fph2       File pointer to HXI2 file
/// \param[in] fpc1       File pointer to CAMS1 file
/// \param[in] fpc2       File pointer to CAMS2 file
void finalize(ahfits::FilePtr fph1, ahfits::FilePtr fph2, 
              ahfits::FilePtr fpc1, ahfits::FilePtr fpc2,
              TELDEF2*& hxi_std);

/// \brief Generate sinusoidal motions of EOB
/// \param[in] time       Simulated time calculated from CAMS event file
/// \param[in] AX1        Amplitude of  X-motion (translation) for 1st sinusoid (mm)
/// \param[in] AX2        Amplitude of  X-motion (translation) for 2nd sinusoid (mm)
/// \param[in] AY1        Amplitude of  Y-motion (translation) for 1st sinusoid (mm)
/// \param[in] AY2        Amplitude of  Y-motion (translation) for 2nd sinusoid (mm)
/// \param[in] Agamma1    Amplitude of  twist motion for 1st sinusoid (mrad)
/// \param[in] Agamma2    Amplitude of  twist motion for 2nd sinusoid (mrad)
/// \param[in] omega1     Frequency of the 1st sinusoid  in the motion (Hz)
/// \param[in] omega2     Frequency of the 2nd sinusoid  in the motion (Hz)
/// \param[in] type       Type of motion 1=XYin phase, 2=XYout of phase  3=XY 90deg out of phase
/// \param[out] delta_x   Generated motion in the X direction
/// \param[out] delta_y   Generated motion in the Y direction
/// \param[out] gamma     Generated rotation angle
void EOBmotion(double time, double AX1, double AX2, double AY1, double AY2, 
               double Agamma1, double Agamma2, double omega1, double omega2, 
               int type, double & delta_x, double & delta_y, double & gamma);

/// \brief Invert simulated CAMS measurements
/// \param[in] time       Simulated time calculated from CAMS event file
/// \param[in] delta_x    Generated motion in the X direction
/// \param[in] delta_y    Generated motion in the Y direction
/// \param[in] gamma      Generated rotation angle
/// \param[in] r_cams1    Coordinate position (mm) of CAMS-1 (teldef CAM_XLOC, CAM_YLOC)
/// \param[in] r_cams2    Coordinate position (mm) of CAMS-2 (teldef CAM_XLOC, CAM_YLOC)
/// \param[in] error_amp  Magnitude of  CAMS error (Random, Constant, Sinusoidal)
/// \param[in] seed       Seed for generating random number
/// \param[out] X1        Inverted X coordinate for CAMS1
/// \param[out] Y1        Inverted Y coordinate for CAMS1
/// \param[out] X2        Inverted X coordinate for CAMS2
/// \param[out] Y2        Inverted Y coordinate for CAMS2
/// \param[out] tnull1    Null flag, set to true if invalid X or Y coordinate.
/// \param[out] tnull2    Null flag, set to true if invalid X or Y coordinate.
void invCAMS(double time, double delta_x, double delta_y, double gamma, 
             AtVect r_cams1, AtVect r_cams2, AtVect error_amp, 
             double freqerr, double cams_pix_size, double seed, 
             int & X1, int & Y1, int & X2, int &  Y2, char & tnull1,
             char & tnull2);

/// \brief Generate RAW coordinates as the satellite would record for given motion using ACT coordinates
/// \param[in] time       Simulated time calculated from CAMS event file
/// \param[in] actx       Given ACT cooridnate in the X-axis
/// \param[in] acty       Given ACT cooridnate in the Y-axis
/// \param[out] rawx      Simulated RAW coordinate in the X-axis
/// \param[out] rawy      Simulated RAW coordinate in the Y-axis
/// \param[in] AX1        Amplitude of  X-motion (translation) for 1st sinusoid (mm)
/// \param[in] AX2        Amplitude of  X-motion (translation) for 2nd sinusoid (mm)
/// \param[in] AY1        Amplitude of  Y-motion (translation) for 1st sinusoid (mm)
/// \param[in] AY2        Amplitude of  Y-motion (translation) for 2nd sinusoid (mm)
/// \param[in] Agamma1    Amplitude of  twist motion for 1st sinusoid (mrad)
/// \param[in] Agamma2    Amplitude of  twist motion for 2nd sinusoid (mrad)
/// \param[in] omega1     Frequency of the 1st sinusoid  in the motion (Hz)
/// \param[in] omega2     Frequency of the 2nd sinusoid  in the motion (Hz)
/// \param[in] type       Type of motion 1=XYin phase, 2=XYout of phase  3=XY 90deg out of phase
/// \param[in] r_hxi      Coordinates (mm) of HXI (teldef HXI_XLOC, HXI_YLOC)
/// \param[in] beta       Rotation angles (degrees) of HXI  (teldef HXI_ROTD).
/// \param[in] r_off_hxi    Half-value of r_hxi
/// \param[in] hxi_pix_size HXI pixel size (mm) (teldef RAW_XSCL or RAW_YSCL)
/// \param[in] r_size_hxi   HXI detector size (mm) (teldef HXI_XPHY, HXI_YPHY)
/// \param[in] r_act_size_hxi  HXI ACT coordinate size (teldef HXI_XSIZ, HXI_YSIZ)
/// \param[in] height_factor
/// \param[in] seed         Seed for generating random number
/// \param[out] tnull       Null flag, set to true if invalid RAWX or RAWY coordinate.
void processEvent(double time, long actx, long acty, long & rawx, long & rawy, 
                  double AX1, double AX2, double AY1, double AY2, double Agamma1, 
                  double Agamma2, double omega1, double omega2, int type, 
                  AtVect r_hxi, double beta, AtVect r_off_hxi, 
                  double hxi_pix_size, AtVect r_size_hxi, AtVect r_act_size_hxi,
                  double height_factor, double seed, char & tnull);

/// \brief Initialize rotation matrix around axis
/// \param[in,out] R      Matrix to rotate
/// \param[in] axis       Axis to rotate around
/// \param[in] angle      Rotation angle
void rot2(AtRotMat & R, int axis, double angle);

/// \brief Create empty CAMS FITS output files
/// \param[in] outcams1   Name of first CAMS output file
/// \param[in] outcams2   Name of second CAMS output file
/// \param[in] fphxi      File pointer to HXI file to copy keywords from
/// \param[in,out] fpc1   File pointer to first CAMS file
/// \param[in,out] fpc2   File pointer to second CAMS file
/// \param[in] extname    Extension name to write to
void create_cams_files(const std::string & outcams1, const std::string & outcams2,
                       ahfits::FilePtr & fphxi, ahfits::FilePtr & fpc1, 
                       ahfits::FilePtr & fpc2, std::string extname);

// ****************************************************************************

int main(int argc, char** argv) {

  Params param;             // Structure to store parameters from par file
  ahmission::teldef::CAMSTelDef cams1;  // Structure to hold data from CAMS TelDef
  ahmission::teldef::CAMSTelDef cams2;  // Structure to hold data from CAMS TelDef
  ahmission::teldef::HXITelDef hxi1;    // Structure to hold data from HXI TelDef
  ahmission::teldef::HXITelDef hxi2;    // Structure to hold data from HXI TelDef
  TELDEF2* hxi_std = NULL;

  double tstart = 0;            // Event file earliest start time
  double tstop = 0;             // Event file latest stop time

  ahfits::FilePtr fph1 = 0; // FITS file pointer to HXI1 output file
  ahfits::FilePtr fph2 = 0; // FITS file pointer to HXI2 output file
  ahfits::FilePtr fpc1 = 0; // FITS file pointer to CAMS1 output file
  ahfits::FilePtr fpc2 = 0; // FITS file pointer to CAMS2 output file

  int status = ahapp::startUp(argc, argv, TOOLTAG);

  if(status == 0) {
    if(ahlog::get_debug()) {
      getPar(param);
      ahapp::writeParametersToLog(); 
      initialize(param, cams1, cams2, hxi1, hxi2, hxi_std, 
                 tstart, tstop, fph1, fph2, fpc1, fpc2);
      doWork(param, cams1, cams2, hxi1, hxi2, hxi_std, 
             tstart, tstop, fph1, fph2, fpc1, fpc2);
      finalize(fph1, fph2, fpc1, fpc2, hxi_std);
      ahapp::shutDown();
    } else {
      try {
        getPar(param);
        initialize(param, cams1, cams2, hxi1, hxi2, hxi_std, 
                   tstart, tstop, fph1, fph2, fpc1, fpc2);
        doWork(param, cams1, cams2, hxi1, hxi2, hxi_std, 
               tstart, tstop, fph1, fph2, fpc1, fpc2);
      } catch (const std::exception& x) {
        ahapp::writeParametersToLog(); 
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fph1, fph2, fpc1, fpc2, hxi_std);
      } catch (const std::exception& x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception& x) {
        status = 1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }  
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }
  
  return status;
  
}

// ****************************************************************************
void getPar(Params & param) {

  param.m_infile1 = ahapp::getParString("infile1");
  param.m_infile2 = ahapp::getParString("infile2"); 
  
  param.m_outroothxi = ahapp::getParString("outroothxi"); 
  param.m_outrootcams = ahapp::getParString("outrootcams"); 
  
  param.m_teldefhxi1 = ahapp::getParString("hxi1teldef");   // CALDB
  param.m_teldefhxi2 = ahapp::getParString("hxi2teldef");   // CALDB
  param.m_teldefcams1 = ahapp::getParString("cams1teldef"); // CALDB
  param.m_teldefcams2 = ahapp::getParString("cams2teldef"); // CALDB
  
  param.m_freq1 = ahapp::getParDouble("freq1");
  param.m_freq2 = ahapp::getParDouble("freq2"); 
  
  param.m_xamp1 = ahapp::getParDouble("xamp1"); 
  param.m_yamp1 = ahapp::getParDouble("yamp1"); 
  param.m_xamp2 = ahapp::getParDouble("xamp2"); 
  param.m_yamp2 = ahapp::getParDouble("yamp2");
  param.m_ramp1 = ahapp::getParDouble("ramp1"); 
  param.m_ramp2 = ahapp::getParDouble("ramp2"); 
  
  param.m_typemotion = ahapp::getParInt("typemotion");
  
  param.m_deltatcams = ahapp::getParDouble("deltatcams"); 
  
  param.m_randerr = ahapp::getParDouble("randerr");
  param.m_consterr = ahapp::getParDouble("consterr");
  param.m_sinerr = ahapp::getParDouble("sinerr");
  param.m_freqerr = ahapp::getParDouble("freqerr");
  
  param.m_seed = ahapp::getParDouble("seed");

}

// ****************************************************************************

void initialize(Params& param, ahmission::teldef::CAMSTelDef& cams1, 
                ahmission::teldef::CAMSTelDef & cams2, ahmission::teldef::HXITelDef & hxi1,
                ahmission::teldef::HXITelDef & hxi2, TELDEF2* & hxi_std,
                double & tstart, double & tstop, 
                ahfits::FilePtr & fph1, ahfits::FilePtr & fph2,
                ahfits::FilePtr & fpc1, ahfits::FilePtr & fpc2) {
  
  int status = 0;   // Status check for throwing errors
  
  // extension name for input file; strings to store INSTRUME keyword value. 
  std::string extname = "EVENTS";         // Extension to access from input files
  std::string inst1 = "";                 // Name of instrument from first HXI output file
  std::string inst2 = "";                 // Name of instrument from second HXI output file
  std::string outhxi1;                    // Name of first HXI output file
  std::string outhxi2;                    // Name of second HXI output file
  std::string outcams1;                   // Name of first CAMS output file
  std::string outcams2;                   // Name of second CAMS output file
  std::string infile1 = param.m_infile1;  // Name of first HXI input file
  std::string infile2 = param.m_infile2;  // Name of second HXI input file
  std::string actual_cams1teldef = "";    // Real name of CAMS1 TelDef file
  std::string actual_cams2teldef = "";    // Real name of CAMS2 TelDef file
  std::string actual_hxi1teldef = "";     // Real name of HXI1 TelDef file
  std::string actual_hxi2teldef = "";     // Real name of HXI2 TelDef file
  bool onefile = false;                   // Process only one file if specified

  // Check if input files are both NONE
  if(ahgen::strtoupper(infile1) == "NONE" && ahgen::strtoupper(infile2) == "NONE") AH_THROW_RUNTIME("No input HXI files.");

  // Check if input files are the same
  if(infile1 == infile2) {
    if(ahgen::strtoupper(infile1) != "NONE") AH_THROW_RUNTIME("HXI FITS input files are the same.");
  }

  // Open input files, check if being used and valid FITS files.
  if(ahgen::strtoupper(infile1) != "NONE") { // open infile1 if desired
    ahfits::open(infile1, "", &fph1);
    if (ahfits::isPrimary(fph1)) ahfits::move(fph1,extname);  // if extended syntax not given, move to EVENT extension
    
    // make sure file isn't empty
    ahmission::checkEmptyTable(fph1,infile1);
    
    // Check for HXI instrument.
    inst1 = ahgen::strtoupper(ahfits::getKeyValStr(fph1, "INSTRUME"));
    if("HXI1" != inst1 && "HXI2" != inst1) {
      AH_THROW_RUNTIME("INSTRUME keyword in inputfile should be HXI1 or HXI2, not " + inst1 + ".");
    }
  } 
  
  if(ahgen::strtoupper(infile2) != "NONE") { // Open infile2 if desired
    ahfits::open(infile2, "", &fph2);
    if (ahfits::isPrimary(fph2)) ahfits::move(fph2,extname);  // if extended syntax not given, move to EVENT extension
    
    // make sure file isn't empty
    ahmission::checkEmptyTable(fph2,infile2);
    
    inst2 = ahgen::strtoupper(ahfits::getKeyValStr(fph2, "INSTRUME"));
    if("HXI1" != inst2 && "HXI2" != inst2) {
      AH_THROW_RUNTIME("INSTRUME keyword in inputfile should be HXI1 or HXI2, not " + inst2 + ".");
    }
  }  
  
  if(inst1 == inst2) {
    if(inst1 == "HXI2") ahfits::close(fph1);
    if(inst2 == "HXI1") ahfits::close(fph2);
  }

  // Only one input file is being used.
  if(fph1 == 0 || fph2 == 0) {
    onefile = true;
    AH_INFO(ahlog::HIGH) << "Only one HXI input file" << std::endl;
  } 
      
  // copy contents of infile to outfile and return opened output file;
  // if editing input file in-place (allowed with last argument = true),
  // fp will point to the opened input file.
  // Open output files and check for columns.
  // If ACTX, ACTY or TIME columns do not exist, throw error
  // If RAWX or RAWY do not exist, create columns.
  if(fph1 != 0) {

    // Verify that HXI1 TelDef file is called for.
    if(ahgen::strtoupper(infile1) == "NONE" && ahgen::strtoupper(param.m_teldefhxi1) == "NONE") 
      AH_THROW_RUNTIME("No input HXI1 TelDef file.");

    // Resolve CALDB reference.
    std::string actual_hxi1teldef = ahmission::caldb::resolve(param.m_teldefhxi1,
    "TelDef", "HXI1", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

    // Load HXI TelDef file into a structure.
    AH_INFO(ahlog::HIGH) << "Reading HXI1 TelDef file " << actual_hxi1teldef << "." << std::endl;
    hxi1 = ahmission::teldef::HXITelDef(actual_hxi1teldef, 1);
    AH_INFO(ahlog::LOW) << "\n\nHXI1 TelDef structure:\n" << hxi1 << std::endl;

    ape_trad_set_string("hxi1teldef", actual_hxi1teldef.c_str());
    param.m_teldefhxi1 = actual_hxi1teldef;

    outhxi1 = param.m_outroothxi+"1.fits";
    AH_INFO(ahlog::HIGH) << "Creating output file " << outhxi1 <<  std::endl;
    ahfits::clone(infile1, outhxi1, &fph1, true);
    ahfits::move(fph1, extname);
    // Check for columns ACTX, ACTY, TIME, RAWX, RAWY. If RAWX/Y don't exist, create them.
    if(!ahfits::haveColumn(fph1, "ACTX")) AH_THROW_RUNTIME("FITS file lacks ACTX column");
    if(!ahfits::haveColumn(fph1, "ACTY")) AH_THROW_RUNTIME("FITS file lacks ACTY column");
    if(!ahfits::haveColumn(fph1, "TIME")) AH_THROW_RUNTIME("FITS file lacks TIME column");
    if(!ahfits::haveColumn(fph1, "RAWX")) {
      AH_INFO(ahlog::HIGH) << "Adding column to " << outhxi1 << ": RAWX" << std::endl;
      ahfits::insertColBefore(fph1,"RAWX","1I","ACTX");
    } else {
      AH_INFO(ahlog::HIGH) << "Overwrite " << outhxi1 <<" column RAWX" << std::endl;
    }
    if(!ahfits::haveColumn(fph1, "RAWY")) {
      AH_INFO(ahlog::HIGH) << "Adding column to " << outhxi1 << ": RAWY" << std::endl;
      ahfits::insertColAfter(fph1,"RAWY","1I","RAWX");
    } else {
      AH_INFO(ahlog::HIGH) << "Overwrite " << outhxi1 << " column RAWY" << std::endl;
    }
    ahfits::setTNull(fph1,"RAWX",-1000);
    ahfits::setTNull(fph1,"RAWY",-1000);
  
  }
  if(fph2 != 0) {

    // Verify that HXI2 TelDef file is called for.
    if(ahgen::strtoupper(infile2) == "NONE" && ahgen::strtoupper(param.m_teldefhxi2) == "NONE") 
      AH_THROW_RUNTIME("No input HXI2 TelDef file.");

    // Resolve CALDB reference.
    std::string actual_hxi2teldef = ahmission::caldb::resolve(param.m_teldefhxi2,
    "TelDef", "HXI2", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

    ape_trad_set_string("hxi2teldef", actual_hxi2teldef.c_str());
    param.m_teldefhxi2 = actual_hxi2teldef;

    outhxi2 = param.m_outroothxi+"2.fits";
    ahfits::clone(infile2, outhxi2, &fph2, true);
    ahfits::move(fph2, extname);
    // Check for columns ACTX, ACTY, TIME, RAWX, RAWY. If RAWX/Y don't exist, create them.
    if(!ahfits::haveColumn(fph2, "ACTX")) AH_THROW_RUNTIME("FITS file lacks ACTX column");
    if(!ahfits::haveColumn(fph2, "ACTY")) AH_THROW_RUNTIME("FITS file lacks ACTY column");
    if(!ahfits::haveColumn(fph2, "TIME")) AH_THROW_RUNTIME("FITS file lacks TIME column");
    if(!ahfits::haveColumn(fph2, "RAWX")) {
      AH_INFO(ahlog::HIGH) << "Adding column to " << outhxi2 << ": RAWX" << std::endl;
      ahfits::insertColBefore(fph2,"RAWX","1I","ACTX");
    } else {
      AH_INFO(ahlog::HIGH) << "Overwrite " << outhxi2 <<" column RAWX" << std::endl;
    }
    if(!ahfits::haveColumn(fph2, "RAWY")) {
      AH_INFO(ahlog::HIGH) << "Adding column to " << outhxi2 << ": RAWY" << std::endl;
      ahfits::insertColAfter(fph2,"RAWY","1I","RAWX");
    } else {
      AH_INFO(ahlog::HIGH) << "Overwrite " << outhxi2 << " column RAWY" << std::endl;
    }
    ahfits::setTNull(fph2,"RAWX",-1000);
    ahfits::setTNull(fph2,"RAWY",-1000);

    // Load HXI TelDef file into structure.
    AH_INFO(ahlog::HIGH) << "Reading HXI2 TelDef file " << actual_hxi2teldef << "." << std::endl;
    hxi2 = ahmission::teldef::HXITelDef(actual_hxi2teldef, 2);
    AH_INFO(ahlog::LOW) << "\n\nHXI2 TelDef structure:\n" << hxi2 << std::endl;

  }
  if(fph1 && fph2) {
    // Determine earliest start time, latest stop time if
    // both input files are in use
    double tstart1_in = ahfits::getKeyValDbl(fph1, "TSTART");
    double tstop1_in = ahfits::getKeyValDbl(fph1, "TSTOP");
    double tstart2_in = ahfits::getKeyValDbl(fph2, "TSTART");
    double tstop2_in = ahfits::getKeyValDbl(fph2, "TSTOP");
    tstart = std::max(tstart1_in, tstart2_in);
    tstop = std::min(tstop1_in, tstop2_in);
  } else if (fph1) {
    // infile2 was not input. Use infile1 TSTART and TSTOP
    tstart = ahfits::getKeyValDbl(fph1, "TSTART");
    tstop = ahfits::getKeyValDbl(fph1, "TSTOP");
  } else {
    // infile1 was not input. Use infile2 TSTART and TSTOP
    tstart = ahfits::getKeyValDbl(fph2, "TSTART");
    tstop = ahfits::getKeyValDbl(fph2, "TSTOP");
  }

  if(0 == param.m_seed) ahgen::seedRandom((long)tstop);
  else ahgen::seedRandom((long)param.m_seed);

  if(fph1) {
    status = readTelDef2(param.m_teldefhxi1.c_str(), &hxi_std);
    if(status) AH_THROW_RUNTIME("Cannot read HXI TelDef file " + param.m_teldefhxi1 + ".");
  } else {
    status = readTelDef2(param.m_teldefhxi2.c_str(), &hxi_std);
    if(status) AH_THROW_RUNTIME("Cannot read HXI TelDef file " + param.m_teldefhxi2 + ".");
  }

  // Print the HXI standard TelDef structure if in debug mode.
  if(ahlog::get_debug()) printTelDef2(hxi_std, stderr);

  actual_cams1teldef = ahmission::caldb::resolve(param.m_teldefcams1,
    "TelDef", "CAMS1", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

  ape_trad_set_string("cams1teldef", actual_cams1teldef.c_str());

  actual_cams2teldef = ahmission::caldb::resolve(param.m_teldefcams2,
    "TelDef", "CAMS2", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

  ape_trad_set_string("cams2teldef", actual_cams2teldef.c_str());

  AH_INFO(ahlog::HIGH) << "Reading CAMS1 TelDef file " << actual_cams1teldef << "." << std::endl;
  cams1 = ahmission::teldef::CAMSTelDef(actual_cams1teldef, 1);
  AH_INFO(ahlog::LOW) << "\n\nCAMS1 TelDef structure:\n" << cams1 << std::endl;
  
  AH_INFO(ahlog::HIGH) << "Reading CAMS2 TelDef file " << actual_cams2teldef << "." << std::endl;
  cams2 = ahmission::teldef::CAMSTelDef(actual_cams2teldef, 2);
  AH_INFO(ahlog::LOW) << "\n\nCAMS2 TelDef structure:\n" << cams2 << std::endl;
  
  // Name the CAMS output files
  outcams1 = param.m_outrootcams+"1.fits";
  outcams2 = param.m_outrootcams+"2.fits";

  // Create CAMS output files
  // Note: we will copy keywords from the first HXI file; it is assumed that
  // the two HXI input files have consistent keywords.
  create_cams_files(outcams1,outcams2,fph1,fpc1,fpc2,"CAMS_DATA");
  ahfits::move(fpc1,"CAMS_DATA");
  ahfits::move(fpc2,"CAMS_DATA");
  
  // Write list of parameters to log file
  //
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(Params & param, const ahmission::teldef::CAMSTelDef & cams1, 
            const ahmission::teldef::CAMSTelDef & cams2, 
            const ahmission::teldef::HXITelDef & hxi1, 
            const ahmission::teldef::HXITelDef & hxi2, TELDEF2* & hxi_std,
            double tstart, double tstop,
            ahfits::FilePtr fph1, ahfits::FilePtr fph2,
            ahfits::FilePtr fpc1, ahfits::FilePtr fpc2) {

  double delta_x = 0;               // Shift in motion in the x direction
  double delta_y = 0;               // Shift in motion in the y direction
  double gamma = 0;                 // Generated Rotation angle
  double time_n = 0;                // generated time within event observation time
  int n = 0;                        // Generic counter for CAMS simulations
  char tnull = 0, tnull1 = 0, tnull2 = 0;    // Null flags for setting TNULL

  // Par file variables
  double AX1 = param.m_xamp1; // Amplitude of X-motion (translation) for 1st sinusoid (mm)
  double AX2 = param.m_xamp2; // Amplitude of X-motion (translation) for 2nd sinusoid (mm)
  double AY1 = param.m_yamp1; // Amplitude of Y-motion (translation) for 1st sinusoid (mm)
  double AY2 = param.m_yamp2; // Amplitude of Y-motion (translation) for 2nd sinusoid (mm)
  double Agamma1 = param.m_ramp1;       // Amplitude of  twist motion for 1st sinusoid (mrad)
  double Agamma2 = param.m_ramp2;       // Amplitude of  twist motion for 2nd sinusoid (mrad)
  double omega1 = param.m_freq1*2.*M_PI; // Frequency of the 1st sinusoid  in the motion (Hz)
  double omega2 = param.m_freq2*2.*M_PI; // Frequency of the 2nd sinusoid  in the motion (Hz)
  int type = param.m_typemotion;    // Type of motion 1=XY in phase, 2=XY out of phase  3=XY 90deg out of phase
  double deltatcams = param.m_deltatcams;     // Time interval in which the CAMS records 5 measurements (d/f 1s)
  double freqerr = param.m_freqerr; // Frequency of the sinusoidal error component (Hz)
  double seed = param.m_seed;       // Seed for random number. Time variable is used if seed is set to 0 (default).
  double error_amp[3] = { 0 };                        // Magnitude of  CAMS error (Random, Constant, Sinusoidal)

  // HXI1/2 out file variables
  double l_timeh1 = 0, l_timeh2 = 0;  // Value of time from TIME column
  long l_actxh1 = 0, l_actxh2 = 0;    // ACTX column
  long l_actyh1 = 0, l_actyh2 = 0;    // ACTY column
  long l_rawxh1 = 0, l_rawxh2 = 0;    // Output raw x-coordinate for RAWX column
  long l_rawyh1 = 0, l_rawyh2 = 0;    // Output raw y-coordinate for RAWY column
  
  // CAMS1/2 out file variables
  double l_time = 0;                  // Output CAMS time value. Same for both files
  int l_x1 = 0, l_x2 = 0;             // Output x coordinates: X_RAW
  int l_y1 = 0, l_y2 = 0;             // Output y coordinates: Y_RAW
  double l_x1_float = 0.0, l_x2_float = 0.0;  // Output x coordinates: X
  double l_y1_float = 0.0, l_y2_float = 0.0;  // Output y coordinates: Y
  int l_quality = 0;                  // Output quality value. Always 0 for both CAMS files.
  char l_proc_status[32] = { 0 };     // Output PROC_STATUS. Always, all bits are 0 for both CAMS files.
  ahfits::IndexType len_proc_status = 32;  // Length of PROC_STATUS bit array

  // HXI TelDef variables
  int raw_sys = 0;                    // Check if RAW coordinates
  int raw_min_seg = 0;                // Check minimum segments to use
  double hxi_pix_size = 0;            // HXI pixel size (mm) (teldef RAW_XSCL or RAW_YSCL; default 0.250)
  double focallen = 0;                // HXI focal length (mm) (teldef FOCALLEN; default 12000.0)
  double beta[2] = { 0 };             // Rotation angles (degrees) of HXI  (teldef HXI_ROTD).
  double z_hxi1 = 0;
  double z_hxi2 = 0;
  double z_cams = 0;
  double z_cams1 = 0;
  double z_cams2 = 0;
  double height_factor1 = 1;
  double height_factor2 = 1;
  AtVect r_hxi = { 0 };                    // Coordinates (mm) of HXI (teldef HXI_XLOC, HXI_YLOC)
  AtVect r_hxi1 = { 0 };                   // Coordinates (mm) of HXI1 (teldef HXI_XLOC, HXI_YLOC)
  AtVect r_hxi2 = { 0 };                   // Coordinates (mm) of HXI2 (teldef HXI_XLOC, HXI_YLOC)
  AtVect r_size_hxi = { 0 };               // HXI detector size (mm) (teldef HXI_XPHY, HXI_YPHY)
  AtVect r_off_hxi = { 0 };                // Half-value of r_hxi
  AtVect r_act_size_hxi = { 0 };           // Lenght of detector pixels in ACT
  
  // CAMS TelDef variables
  double t_cams[5] = { 0 };           // Subtimes of 1 deltatcams interval
  AtVect r_cams1 = { 0 };                  // Coordinate position (mm) of CAMS1 (teldef CAM_XLOC, CAM_YLOC)
  AtVect r_cams2 = { 0 };                  // Coordinate position (mm) of CAMS2 (teldef CAM_XLOC, CAM_YLOC)
  double cams_pix_size = cams1.m_scale; // CAMS pixel size (mm) (teldef CAM_SCAL; default 0.001)
  if(0 == cams_pix_size)
    AH_THROW_RUNTIME("Invalid CAMS pixel size.");
  
  // Read coordinate system "RAW" from HXI TelDef
  raw_sys = getCoordSystemNumberFromName(hxi_std, "RAW");
  if(raw_sys < 0)
    AH_THROW_RUNTIME("Cannot find RAW coordinate system in HXI TelDef file.");
  
  // Read minimum segment value from HXI TelDef
  raw_min_seg = hxi_std->min_segment[raw_sys];
  
  // Assign HXI Pixel size from HXI TelDef
  hxi_pix_size = hxi_std->coordsys[raw_sys][raw_min_seg]->scale_x;
  if(0 == hxi_pix_size) AH_THROW_RUNTIME("Invalid HXI pixel size.");

  // Assign Focal Length from HXI TelDef
  focallen = hxi_std->skyattparam[raw_sys]->focal_length;

  // Get ACT_XSIZ/ACT_YSIZ from TelDef file for pixel size of the detector in ACT coordinates
  r_act_size_hxi[0] = hxi_std->coordsys[raw_sys+1][hxi_std->min_segment[raw_sys+1]]->npixels_x; 
  r_act_size_hxi[1] = hxi_std->coordsys[raw_sys+1][hxi_std->min_segment[raw_sys+1]]->npixels_y; 
 
  if(fph1) {
    beta[0] = hxi1.m_rot_angle*M_PI/180;  // Rotation angle of HXI-1  (teldef HXI_ROTD; default 157.5 degrees).
    r_hxi1[0] = hxi1.m_x_location;        // X-position (mm) of HXI-1 (teldef HXI_XLOC; default 465.0)
    r_hxi1[1] = hxi1.m_y_location;        // Y-position (mm) of HXI-1 (teldef HXI_YLOC; default 195.0)
    z_hxi1 = hxi1.m_z_location;           // Z-position (mm) of HXI-1 (teldef HXI_ZLOC; default -5491.0)
  }
  
  if(fph2) {
    beta[1] = hxi2.m_rot_angle*M_PI/180;  // Rotation angle of HXI-2  (teldef HXI_ROTD; default 202.5 degrees).
    r_hxi2[0] = hxi2.m_x_location;        // X-position (mm) of HXI-2 (teldef HXI_XLOC; default -465.0)
    r_hxi2[1] = hxi2.m_y_location;        // Y-position (mm) of HXI-2 (teldef HXI_YLOC; default 195.0)
    z_hxi2 = hxi2.m_z_location;           // Z-position (mm) of HXI-2 (teldef HXI_ZLOC; default -5491.0)
  }

  if(fph1) {
    r_size_hxi[0] = hxi1.m_x_phys_size;   // HXI detector size in X (mm) (teldef HXI_XPHY; default 32.0)
    r_size_hxi[1] = hxi1.m_y_phys_size;   // HXI detector size in Y (mm) (teldef HXI_YPHY; default 32.0)
  } else {
    r_size_hxi[0] = hxi2.m_x_phys_size;   // HXI detector size in X (mm) (teldef HXI_XPHY; default 32.0)
    r_size_hxi[1] = hxi2.m_y_phys_size;   // HXI detector size in Y (mm) (teldef HXI_YPHY; default 32.0)
  }

  // This is the same for both instruments HXI1 and HXI2
  r_off_hxi[0] = r_size_hxi[0]/2;     // HXI X coordinate offset (default 16.0)
  r_off_hxi[1] = r_size_hxi[1]/2;     // HXI Y coordinate offset (default 16.0)

  AH_INFO(ahlog::LOW) << "HXI X and Y offsets = " << r_off_hxi[0] << ", " << r_off_hxi[1] << std::endl;

  r_cams1[0] = cams1.m_x_location;      // X-position (mm) of CAMS-1 (teldef CAM_XLOC; default 300.0)
  r_cams1[1] = cams1.m_y_location;      // Y-position (mm) of CAMS-1 (teldef CAM_YLOC; default 480.0)
  z_cams1 = cams1.m_z_location;         // Z-position (mm) of CAMS-1 (teldef CAM_ZLOC; default -5723.0)

  r_cams2[0] = cams2.m_x_location;      // X-position (mm) of CAMS-2 (teldef CAM_XLOC; default -300.0)
  r_cams2[1] = cams2.m_y_location;      // Y-position (mm) of CAMS-2 (teldef CAM_XLOC; default 480.0)
  z_cams2 = cams2.m_z_location;         // Z-position (mm) of CAMS-1 (teldef CAM_ZLOC; default -5723.0)
  
  z_cams = (z_cams1 + z_cams2)/2.0;
  height_factor1 = z_hxi1/z_cams;
  height_factor2 = z_hxi2/z_cams;

  AH_INFO(ahlog::LOW) << "Mean Z(CAMS) = " << z_cams << std::endl;
  AH_INFO(ahlog::LOW) << "HXI1 height factor = " << height_factor1 << std::endl;
  AH_INFO(ahlog::LOW) << "HXI2 height factor = " << height_factor2 << std::endl;

  // Initialize simulation errors
  error_amp[0] = param.m_randerr;       // Magnitude of  CAMS error: random component (mm)
  error_amp[1] = param.m_consterr;      // Magnitude of  CAMS error: constant component (mm)
  error_amp[2] = param.m_sinerr;        // Magnitude of  CAMS error: sinusoidal component (mm)
    
  // prepare columns TIME X Y Quality for cams output 1 & 2
  // initialize outrootcams1,2 columns : TIME X Y QUALITY
  ahfits::Router routerc1(fpc1);
  ahfits::Router routerc2(fpc2);

  // TIME columns in both CAMS outfiles are the same.
  routerc1.connectScalar(ahfits::e_WRITEONLY, "TIME", l_time);
  routerc1.connectScalar(ahfits::e_WRITEONLY, "X_RAW", l_x1, &tnull1);
  routerc1.connectScalar(ahfits::e_WRITEONLY, "Y_RAW", l_y1, &tnull1);
  routerc1.connectScalar(ahfits::e_WRITEONLY, "X", l_x1_float);
  routerc1.connectScalar(ahfits::e_WRITEONLY, "Y", l_y1_float);
  routerc1.connectScalar(ahfits::e_WRITEONLY, "QUALITY", l_quality);
  routerc1.connectBit(ahfits::e_WRITEONLY, "PROC_STATUS", l_proc_status, len_proc_status);
  
  routerc2.connectScalar(ahfits::e_WRITEONLY, "TIME", l_time);
  routerc2.connectScalar(ahfits::e_WRITEONLY, "X_RAW", l_x2, &tnull2);
  routerc2.connectScalar(ahfits::e_WRITEONLY, "Y_RAW", l_y2, &tnull2);
  routerc2.connectScalar(ahfits::e_WRITEONLY, "X", l_x2_float);
  routerc2.connectScalar(ahfits::e_WRITEONLY, "Y", l_y2_float);
  routerc2.connectScalar(ahfits::e_WRITEONLY, "QUALITY", l_quality);
  routerc2.connectBit(ahfits::e_WRITEONLY, "PROC_STATUS", l_proc_status, len_proc_status);

  AH_INFO(ahlog::LOW) << "EOBmotion constants: AX1=" << AX1 << ", AX2=" << AX2
    << ", AY1=" << AY1 << ", Agamma1=" << Agamma1 << ", Agamma2=" << Agamma2
    << ", omega1=" << omega1 << ", omega2=" << omega2 << ", type=" << type << std::endl;

  AH_INFO(ahlog::LOW) << "invCAMS constants: r_cams1[0]=" << r_cams1[0] << ", r_cams1[1]=" 
    << r_cams1[1] << ", r_cams1[2]=" << r_cams1[2] << std::endl;
  AH_INFO(ahlog::LOW) << "r_cams1[0]=" << r_cams2[0] << ", r_cams2[1]=" 
    << r_cams2[1] << ", r_cams2[2]=" << std::endl;
  AH_INFO(ahlog::LOW) << "error_amp[0]=" << error_amp[0] << ", error_amp[1]=" << error_amp[1] 
    << ", error_amp[2]=" << error_amp[2] << std::endl;
  AH_INFO(ahlog::LOW) << "freqerr=" << freqerr << ", cams_pix_size="
    << cams_pix_size << ", seed=" << seed << std::endl;  

  while(time_n < tstop + deltatcams) {
    
    // Set time within tstart/tstop
    time_n = int(tstart) + n*deltatcams;
    n++;

    //set the 5 CAMs subtimes of 1 deltatcams  interval 
    t_cams[0] = 0*deltatcams/64+time_n;
    t_cams[1] = 13*deltatcams/64+time_n;
    t_cams[2] = 26*deltatcams/64+time_n;
    t_cams[3] = 39*deltatcams/64+time_n;
    t_cams[4] = 52*deltatcams/64+time_n;
    
    // calculate X, Y for each CAMS subtime 
    for(int ii = 0; ii < 5; ++ii) {

      // Reset motion variables
      delta_x = 0;
      delta_y = 0;
      gamma = 0;

      // Reset coordinates
      l_x1 = 0;
      l_y1 = 0;
      l_x2 = 0;
      l_y2 = 0;
        
      // Call EOBmotion to calculate the motion in satellite coordinates
      // invCAMS calculates the coordinates in CAMS coordinates 
      l_time = t_cams[ii];
      
      // Calculate Extensible Optical Bench motion
      EOBmotion(l_time, AX1, AX2, AY1, AY2, Agamma1, Agamma2, omega1, omega2, 
                type, delta_x, delta_y, gamma);

      AH_INFO(ahlog::LOW) << "EOBmotion: time=" << l_time << ", delta_x=" << delta_x
        << ", delta_y=" << delta_y << ", gamma=" << gamma << std::endl;

      // Invert for simulated CAMS measurements
      invCAMS(l_time, delta_x, delta_y, gamma, r_cams1, r_cams2, error_amp,
              freqerr, cams_pix_size, seed, l_x1, l_y1, l_x2, l_y2,
              tnull1, tnull2);

      AH_INFO(ahlog::LOW) << "invCAMS:   time=" << l_time << ", X1=" << l_x1
        << ", Y1=" << l_y1 << ", X2=" << l_x2 << ", Y2=" << l_y2 << ", tnull1=" << (int)tnull1
        << ", tnull2=" << (int)tnull2 << std::endl;

      l_x1_float = l_x1;
      l_y1_float = l_y1;
      l_x2_float = l_x2;
      l_y2_float = l_y2;
      
      //Write outrootcams1 TIME, X, Y, Quality, move to next row
      ahfits::writeRow(fpc1);
      ahfits::nextRow(fpc1);
    
      //Write outrootcams2 TIME, X, Y, Quality, move to next row
      ahfits::writeRow(fpc2);
      ahfits::nextRow(fpc2);

    }
    
    // Quality columns are always zero for this simulation.    
  }
  
  if(fph1 != 0) {
    AH_INFO(ahlog::HIGH) << "Processing hxi 1" << std::endl;
    // Set up routers for HXI output files
    ahfits::Router routerh1(fph1);

    // Process event data
    // HX1 and/or 2 is in toprocess
    routerh1.connectScalar(ahfits::e_READONLY, "TIME", l_timeh1);
    routerh1.connectScalar(ahfits::e_READONLY, "ACTX", l_actxh1);
    routerh1.connectScalar(ahfits::e_READONLY, "ACTY", l_actyh1);
    routerh1.connectScalar(ahfits::e_WRITEONLY, "RAWX", l_rawxh1, &tnull);
    routerh1.connectScalar(ahfits::e_WRITEONLY, "RAWY", l_rawyh1, &tnull);
    
    // set r_hxi to HXI2 values
    r_hxi[0] = r_hxi1[0];
    r_hxi[1] = r_hxi1[1];

    AH_INFO(ahlog::LOW) << "processEvent constants for HXI1:" << std::endl;
    AH_INFO(ahlog::LOW) << "r_hxi[0]=" << r_hxi[0] << ", r_hxi[1]=" << r_hxi[1]
      << ", beta[1]=" << beta[1] << std::endl;
    AH_INFO(ahlog::LOW) << "r_off_hxi[0]=" << r_off_hxi[0]
      << ", r_off_hxi[1]=" << r_off_hxi[1] << ", hxi_pix_size="
      << hxi_pix_size << std::endl;
    AH_INFO(ahlog::LOW) << "r_size_hxi[0]=" << r_size_hxi[0]
      << ", r_size_hxi[1]=" << r_size_hxi[1] << ", r_act_size_hxi[0]="
      << r_act_size_hxi[0] << ", r_act_size_hxi[1]=" << r_act_size_hxi[1]
      << ", height_factor1=" << height_factor1 << std::endl;
     
    // Read  infile_n TIME, ACTX ACTY
    // Event loop: write rawx, rawy
    for(ahfits::firstRow(fph1); ahfits::readOK(fph1); ahfits::nextRow(fph1)) {
        
      // Read rows from event file
      ahfits::readRow(fph1);

      // Generate event 
      processEvent(l_timeh1, l_actxh1, l_actyh1, l_rawxh1, l_rawyh1, AX1, AX2, AY1, AY2,
                   Agamma1, Agamma2, omega1, omega2, type, r_hxi, beta[0], r_off_hxi,
                   hxi_pix_size, r_size_hxi, r_act_size_hxi, height_factor1, seed, tnull);

      AH_INFO(ahlog::LOW) << "processEvent: l_timeh1=" << l_timeh1 << ", l_actxh1=" <<
        l_actxh1 << ", l_actyh1=" << l_actyh1 << ", l_rawxh1=" << l_rawxh1 << ", l_rawyh1"
        << l_rawyh1 << ", tnull=" << (int)tnull << std::endl;
        
      // Write rows to output
      ahfits::writeRow(fph1);

    } // End event loop
  }

  // Process file 2 if available
  if(fph2 != 0) {
    AH_INFO(ahlog::HIGH) << "Processing hxi 2" << std::endl;
    ahfits::Router routerh2(fph2);
    routerh2.connectScalar(ahfits::e_READONLY, "TIME", l_timeh2);
    routerh2.connectScalar(ahfits::e_READONLY, "ACTX", l_actxh2);
    routerh2.connectScalar(ahfits::e_READONLY, "ACTY", l_actyh2);
    routerh2.connectScalar(ahfits::e_WRITEONLY, "RAWX", l_rawxh2, &tnull);
    routerh2.connectScalar(ahfits::e_WRITEONLY, "RAWY", l_rawyh2, &tnull);
    
    // set r_hxi to HXI2 values
    r_hxi[0] = r_hxi2[0];
    r_hxi[1] = r_hxi2[1];

    AH_INFO(ahlog::LOW) << "processEvent constants for HXI2:" << std::endl;
    AH_INFO(ahlog::LOW) << "r_hxi[0]=" << r_hxi[0] << ", r_hxi[1]=" << r_hxi[1]
      << ", beta[1]=" << beta[1] << std::endl;
    AH_INFO(ahlog::LOW) << "r_off_hxi[0]=" << r_off_hxi[0]
      << ", r_off_hxi[1]=" << r_off_hxi[1] << ", hxi_pix_size="
      << hxi_pix_size << std::endl;
    AH_INFO(ahlog::LOW) << "r_size_hxi[0]=" << r_size_hxi[0]
      << ", r_size_hxi[1]=" << r_size_hxi[1] << ", r_act_size_hxi[0]="
      << r_act_size_hxi[0] << ", r_act_size_hxi[1]=" << r_act_size_hxi[1]
      << ", height_factor2=" << height_factor2 << std::endl;
    
    // Read  infile_n TIME, ACTX ACTY
    for(ahfits::firstRow(fph2); ahfits::readOK(fph2); ahfits::nextRow(fph2)) {
      
      // Read rows from event file
      ahfits::readRow(fph2);
      
      // Generate event 
      processEvent(l_timeh2, l_actxh2, l_actyh2, l_rawxh2, l_rawyh2, AX1, AX2, AY1, AY2,
                   Agamma1, Agamma2, omega1, omega2, type, r_hxi, beta[1], r_off_hxi,
                   hxi_pix_size, r_size_hxi, r_act_size_hxi, height_factor2, seed, tnull);

      AH_INFO(ahlog::LOW) << "processEvent: l_timeh2=" << l_timeh2 << ", l_actxh2=" <<
        l_actxh2 << ", l_actyh2=" << l_actyh2 << ", l_rawxh2=" << l_rawxh2 << ", l_rawyh2"
        << l_rawyh2 << ", tnull=" << (int)tnull << std::endl;

      // Write rows to output
      ahfits::writeRow(fph2);

    } // End event loop
  }

}

// ****************************************************************************

void finalize(ahfits::FilePtr fph1, ahfits::FilePtr fph2, 
              ahfits::FilePtr fpc1, ahfits::FilePtr fpc2,
              TELDEF2* & hxi_std) {

  // Close all FITS file pointers
  ahfits::close(fph1);
  ahfits::close(fph2);
  ahfits::close(fpc1);
  ahfits::close(fpc2);
  destroyTelDef2(hxi_std);
  
  AH_INFO(ahlog::HIGH) << "Finished." << std::endl;

}

// ****************************************************************************

void EOBmotion(double time, double AX1, double AX2, double AY1, double AY2, 
               double Agamma1, double Agamma2, double omega1, double omega2, 
               int type, double & delta_x, double & delta_y, double & gamma) {

  if ( type == 1 ) { // Linear motion in all phases
    delta_x = AX1*cos(omega1*time) + AX2*sin(omega2*time); // (mm)
    delta_y = AY1*cos(omega1*time) + AY2*sin(omega2*time); // (mm)
    gamma = Agamma1*cos(omega1*time) + Agamma2*sin(omega2*time); //(mrad)
    // Convert Gamma from mrad to rad:
    gamma /= 1000;
  } else if ( type == 2 ) { // Linear motion X out of phase with Y and gamma
      delta_x = -AX1*cos(omega1*time) - AX2*sin(omega2*time);
      delta_y = AY1*cos(omega1*time) + AY2*sin(omega2*time);
      gamma = Agamma1*cos(omega1*time) + Agamma2*sin(omega2*time);
      // Convert Gamma from mrad to rad:
      gamma /= 1000;
  } else if ( type == 3 ) { // Circular Motion. X and Y are 90 degrees out of phase
      delta_x = AX1*cos(omega1*time) + AX2*cos(omega2*time);
      delta_y = AY1*sin(omega1*time) + AY2*sin(omega2*time);
      gamma = Agamma1*cos(omega1*time) + Agamma2*sin(omega2*time);
      // Convert Gamma from mrad to rad:
      gamma /= 1000;
  }

} // end EOBmotion

// ****************************************************************************

void invCAMS(double time, double delta_x, double delta_y, double gamma, 
             AtVect r_cams1, AtVect r_cams2, AtVect error_amp, 
             double freqerr, double cams_pix_size, double seed, 
             int & X1, int & Y1, int & X2, int &  Y2, char & tnull1,
             char & tnull2) {

  // This function simulates CAMS measurements.
  // Input is time, delta_x, delta_y, gamma
  // output is X, Y for CAMS1 and CAMS2

  double orb_freq = 0.;               // Example orbital period
  double randn = 0;                   // Generated random number 
  double error_X1 = 0.;               // Generated X1 error (CAMS1)
  double error_Y1 = 0.;               // Generated Y1 error (CAMS1)
  double error_X2 = 0.;               // Generated X2 error (CAMS2)
  double error_Y2 = 0.;               // Generated Y2 error (CAMS2)
  double floatX1 = 0., floatX2 = 0.;  // Temporary X coordinates  
  double floatY1 = 0., floatY2 = 0.;  // Temporary Y coordinates
  int conv_factor = 1000;             // Conversion factor for mm to microns
  AtVect delta_r = { 0 };             // Change in direction vector
  AtVect r1_sat = { 0 };              // Vector to convert SAT1 to CAMS1
  AtVect r2_sat = { 0 };              // Vector to convert SAT2 to CAMS2
  AtVect r1 = { 0 };                  // Generated change in direction for CAMS1
  AtVect r2 = { 0 };                  // Generated change in direction for CAMS2
  AtRotMat Rz_gamma = { { 0 } };      // Rotation matrix, with gamma rotation angle
  AtRotMat Rz_90 = { { 0 } };         // Rotation matrix, with -90 degree rotation angle
  AtRotMat r_C1 = { { 0 } };          // Generated rotation matrix (CAMS1)
  AtRotMat r_C2 = { { 0 } };          // Generated rotation matrix (CAMS2)

  AtVect temp_vec1 = { 0 };           // Vector to hold temp coordinates
  AtVect temp_vec2 = { 0 };           // Vector to hold temp coordinates

  // reset X/Y null flag
  tnull1 = 0;
  tnull2 = 0;

  orb_freq = freqerr*2*M_PI;   // Example for 6000s orbital period

  // Initialize rotation matrices
  //setMatrix(Ry_pi,-1,0,0,0,1,0,0,0,-1);
  setMatrix(Rz_90,0,-1,0,1,0,0,0,0,1);

  // Set Rotation matrix rz_gamma
  rot2(Rz_gamma, 3, gamma);

  // Rotation matrices from SAT to CAMS 1 and 2
  // For version 0.7 and earlier of CAMS document:  copyMatrix(r_C1,Ry_pi);
  // For version 0.8 and later of CAMS document:
  copyMatrix(r_C1,Rz_90);
  // Sense of rotation is opposite that in CAMS Teldef file (here SAT->CAMS,
  // in teldef CAMS->SAT).

  // ATRMProd function is "reverse" of what is expected:
  // product of two rotation matrices 
  // atRMProd(rm0, rm1, rm2): rm2 = rm1 rm0 
  // For version 0.7 and earlier of CAMS document:  ATRMProd(Ry_pi,Rz_90,r_C2);
  // For version 0.8 and later of CAMS document:
  setMatrixIdentity(r_C2);

  // Initialize delta_r
  delta_r[0] = delta_x;
  delta_r[1] = delta_y;

  // Set r1_sat
  // r1_sat = delta_r + Rz_gamma*r_cams1 - r_cams1
  ATSubVect(delta_r,r_cams1,temp_vec1);
  ATRotVect(Rz_gamma,r_cams1,temp_vec2);
  ATAddVect(temp_vec1,temp_vec2,r1_sat);

  // Set r2_sat
  // r2_sat = delta_r + Rz_gamma*r_cams2 - r_cams2
  //ATSubVect(test_delta_r,test_r_cams2,temp_vec1);
  ATSubVect(delta_r,r_cams2,temp_vec1);
  ATRotVect(Rz_gamma,r_cams2,temp_vec2);
  ATAddVect(temp_vec1,temp_vec2,r2_sat);

  // r1 = r_C1*r1_sat
  ATRotVect(r_C1,r1_sat,r1);
  // r2 = r_C2*r2_sat
  ATRotVect(r_C2,r2_sat,r2);

  // randn is a normally-distributed random number
  // Generate random number with time as seed for each calculation
  randn = ahgen::getRandom();
  error_X1 = error_amp[0]*randn + error_amp[1]*sin(orb_freq*time) + error_amp[2];
  
  randn = ahgen::getRandom();
  error_Y1 = error_amp[0]*randn + error_amp[1]*sin(orb_freq*time) + error_amp[2];
  
  randn = ahgen::getRandom();
  error_X2 = error_amp[0]*randn + error_amp[1]*sin(orb_freq*time) + error_amp[2];
  
  randn = ahgen::getRandom();
  error_Y2 = error_amp[0]*randn + error_amp[1]*sin(orb_freq*time) + error_amp[2];

  // Apply calculated coordinates with errors
  floatX1 = r1[0] + error_X1;
  floatY1 = r1[1] + error_Y1;
  floatX2 = r2[0] + error_X2;
  floatY2 = r2[1] + error_Y2;
  
  // Here the "sign" function is sign(x) = abs(x)/x;
  floatX1 = int(floatX1/cams_pix_size)*cams_pix_size + sign(floatX1)*cams_pix_size/2;
  floatY1 = int(floatY1/cams_pix_size)*cams_pix_size + sign(floatY1)*cams_pix_size/2;
  floatX2 = int(floatX2/cams_pix_size)*cams_pix_size + sign(floatX2)*cams_pix_size/2;
  floatY2 = int(floatY2/cams_pix_size)*cams_pix_size + sign(floatY2)*cams_pix_size/2;
  
  // These are the final coordinates
  X1 = int(floatX1*conv_factor);
  X2 = int(floatX2*conv_factor);
  Y1 = int(floatY1*conv_factor);
  Y2 = int(floatY2*conv_factor);

  // Verify X/Y are within range of size of int
  if((X1 < -ISIZE || X1 > ISIZE-1) || (Y1 < -ISIZE || Y1 > ISIZE-1)) {
    tnull1 = 1;
  }
  
  if((X2 < -ISIZE || X2 > ISIZE-1) || (Y2 < -ISIZE || Y2 > ISIZE-1)) {
    tnull2 = 1;
  }


} // end invCAMS

// ****************************************************************************

void processEvent(double time, long actx, long acty, long & rawx, long & rawy, 
                  double AX1, double AX2, double AY1, double AY2, double Agamma1, 
                  double Agamma2, double omega1, double omega2, int type, 
                  AtVect r_hxi, double beta, AtVect r_off_hxi, 
                  double hxi_pix_size, AtVect r_size_hxi, AtVect r_act_size_hxi, 
                  double height_factor, double seed, char & tnull) {

  double delta_x_sat = 0;           // Change in SAT x coordinate
  double delta_y_sat = 0;           // Change in SAT y coordinate
  double x_ideal = 0;               // Generated ideal value for x coordinate
  double y_ideal = 0;               // Generated ideal value for y coordinate
  double gamma = 0;                 // Generated rotation angle
  double randn = 0;                 // Generated random number

  AtVect delta_r = { 0 };           // Change in direction vector
  AtVect r_raw = { 0 };             // Generated change in RAW coordinates vector
  AtVect r_ideal = { 0 };           // Vector to hold ideal coordinates
  AtRotMat Rz_gamma_neg = { { 0 } };// Rotation matrix, with -gamma rotation angle
  AtRotMat Rz_beta = { { 0 } };     // Rotation matrix, with beta rotation angle

  AtVect temp_vec1 = { 0 };         // Vector to hold temp coordinates
  AtVect temp_vec2 = { 0 };         // Vector to hold temp coordinates
  AtVect temp_vec3 = { 0 };         // Vector to hold temp coordinates

  // Initialize rotation matrices to identity.
  setMatrixIdentity(Rz_gamma_neg);
  setMatrixIdentity(Rz_beta);

  // reset RAWX/RAWY null flag
  tnull = 0;

  // Simulate relative motion
  EOBmotion(time, AX1, AX2, AY1, AY2, Agamma1, Agamma2, omega1, omega2, type, delta_x_sat, delta_y_sat, gamma);

  // r_ideal is now the RAW with no motion, but randomized within the pixel. So this is an intermediate
  // coordinate. Here "rand" uniformly distributed pseudorandom number.
  randn = ahgen::getRandom(); // Generate random number with time as seed
  x_ideal = hxi_pix_size*(actx-r_act_size_hxi[0]/2) + hxi_pix_size*randn - hxi_pix_size/2;
  randn = ahgen::getRandom(); // Generate random number with time as seed
  y_ideal = hxi_pix_size*(acty-r_act_size_hxi[1]/2) + hxi_pix_size*randn - hxi_pix_size/2;
  r_ideal[0] = x_ideal;
  r_ideal[1] = y_ideal;

  // Assign relative motion to delta_r vector (mm). These are delta_x, delta_y in Figure 16 of CAMS Document
  delta_r[0] = delta_x_sat;
  delta_r[1] = delta_y_sat;
  
  // Define rotation matrix for +/- gamma rotation about z axis.
  // Substitute rot2 with atfunction.
  rot2(Rz_gamma_neg,3,-gamma);
  
  // Calculate the corrupted position vector. Now changed to Rz_beta
  // Convert beta from radians to degrees
  rot2(Rz_beta,3,beta);

  // gamma is the twist angle, delta_r is the EOB translation.
  // The below equation is actually a matrix and vector equation
  // r_raw = Rz_beta*(Rz_gamma_neg*(-delta_r+r_hxi)-r_hxi) + Rz_gamma_neg*r_ideal + r_off_hxi
  ATSubVect(r_hxi,delta_r,temp_vec1);          // Subtract delta_r from r_hxi
  ATRotVect(Rz_gamma_neg,temp_vec1,temp_vec2); // apply Rz_gamma_neg to r_raw
  ATSubVect(temp_vec2,r_hxi,temp_vec1);        // subtract r_hxi from r_raw
  ATRotVect(Rz_beta,temp_vec1,temp_vec2);      // Apply Rz_beta to r_raw
  ATRotVect(Rz_gamma_neg,r_ideal,temp_vec1);   // Apply Rz_gamma_neg to r_ideal
  ATAddVect(temp_vec1,temp_vec2,temp_vec3);    // Sum r_raw and r_temp
  ATAddVect(temp_vec3,r_off_hxi,r_raw);        // Sum r_raw and r_off_hxi for final vector

  // Consider only points within HXI FOV
  if( (r_raw[0] < r_size_hxi[0] + hxi_pix_size/2) && (r_raw[0] > 0 + hxi_pix_size/2) && 
      (r_raw[1] < r_size_hxi[1] + hxi_pix_size/2) && (r_raw[1] > 0 + hxi_pix_size/2) ) {
    
    // Take the RAW HXI, calculate the integer and center in the middle of the pixel.
    // Convert from mm to pixels and round to nearest pixel.
    // Cast r_raw as an int, set to RAW coordinate

    r_raw[0] *= height_factor;
    r_raw[1] *= height_factor;
    
    // These are now the perturbed RAWX coordinates.
    rawx = int(r_raw[0]/hxi_pix_size+0.5);

    // These are now the perturbed RAWY coordinates.
    rawy = int(r_raw[1]/hxi_pix_size+0.5);

  } else {
    tnull = 1;
  }

} // end processEvent

// ****************************************************************************

void rot2(AtRotMat & R, int axis, double angle) {

  if(axis == 1) {         // rotation around x axis by angle
    setMatrix(R,1,0,0,0,cos(angle),-sin(angle),0,sin(angle),cos(angle));
  } else if(axis == 2) {  // rotation around y axis by angle
    setMatrix(R,cos(angle),0,sin(angle),0,1,0,-sin(angle),0,cos(angle));
  } else if(axis == 3) {  // rotation around z axis by angle
    setMatrix(R,cos(angle),-sin(angle),0,sin(angle),cos(angle),0,0,0,1);
  } else {
    AH_THROW_RUNTIME("Only rotate around X, Y or Z (x=1,y=2,z=3)");
  }

} // end rot2

// ****************************************************************************

void create_cams_files(const std::string & outcams1, const std::string & outcams2,
                       ahfits::FilePtr & fphxi, ahfits::FilePtr & fpc1, 
                       ahfits::FilePtr & fpc2, std::string extname) {
  
  //Open outrootcam1
  ahfits::create(outcams1, "", &fpc1);
  ahfits::addEmptyTbl(fpc1,extname);
  
  //Write keywords outrootcam1 
  ahmission::keyword::copyAllKeywords(fphxi,fpc1,ahmission::keyword::e_EVENT);
  ahfits::writeKeyValStr(fpc1,"INSTRUME","CAMS1", "");

  // Prepare columns TIME, X_RAW, Y_RAW, X, Y, QUALITY in outrootcam1
  ahfits::insertColAfter(fpc1,"TIME","1D","");  
  ahfits::insertColAfter(fpc1,"X_RAW","1I","");  
  ahfits::insertColAfter(fpc1,"Y_RAW","1I","");  
  ahfits::insertColAfter(fpc1,"X","1E","");  
  ahfits::insertColAfter(fpc1,"Y","1E","");  
  ahfits::insertColAfter(fpc1,"QUALITY","1I","");
  ahfits::insertColAfter(fpc1,"PROC_STATUS","32X","");
  ahfits::setTNull(fpc1,"X_RAW",32767); // Set up TNULL value for column X
  ahfits::setTNull(fpc1,"Y_RAW",32767); // Set up TNULL value for column Y

  //Open outrootcam2
  ahfits::create(outcams2, "", &fpc2);
  ahfits::addEmptyTbl(fpc2,extname);

  //Write keywords outrootcam2 
  ahmission::keyword::copyAllKeywords(fphxi,fpc2,ahmission::keyword::e_EVENT);
  ahfits::writeKeyValStr(fpc2,"INSTRUME","CAMS2", "");
  
  // Prepare columns TIME, X_RAW, Y_RAW, X, Y, QUALITY in outrootcam2
  ahfits::insertColAfter(fpc2,"TIME","1D","");  
  ahfits::insertColAfter(fpc2,"X_RAW","1I","");  
  ahfits::insertColAfter(fpc2,"Y_RAW","1I","");  
  ahfits::insertColAfter(fpc2,"X","1E","");  
  ahfits::insertColAfter(fpc2,"Y","1E","");  
  ahfits::insertColAfter(fpc2,"QUALITY","1I","");  
  ahfits::insertColAfter(fpc2,"PROC_STATUS","32X","");
  ahfits::setTNull(fpc2,"X_RAW",32767); // Set up TNULL value for column X
  ahfits::setTNull(fpc2,"Y_RAW",32767); // Set up TNULL value for column Y

}

// ****************************************************************************

/** @} */

/* Revision Log
  $Log: camssim.cxx,v $
  Revision 1.41  2016/04/14 19:56:05  rshill
  Additional calls to writeParametersToLog(); case-insensitive
  string comparisons; prologue tweak.

  Revision 1.40  2016/02/19 00:44:04  klrutkow
  use ahmission::getTELESCOPString() to set TELESCOP

  Revision 1.39  2015/12/29 19:32:11  klrutkow
  camssim.cxx throw error if no rows in the input file(s), using checkEmptyTable instead of readOK

  Revision 1.38  2015/11/17 20:24:56  mwitthoe
  camssim: copy keywords to output CAMS files

  Revision 1.37  2015/09/04 21:10:33  rshill
  Added stamping of parameters to log.

  Revision 1.36  2015/08/07 19:20:47  rshill
  Add PROC_STATUS to CAMS output.

  Revision 1.35  2015/08/06 15:35:23  rshill
  Corrected rotation angle of r_C2 in invCAMS; more cleanup.

  Revision 1.34  2015/08/04 23:50:57  rshill
  Corrected a term of Ry_pi matrix (not used, but done for clarity).

  Revision 1.33  2015/08/03 15:35:12  rshill
  Task cleanup; message output.

  Revision 1.32  2015/07/29 21:58:14  rshill
  Copy actual teldef file names back into param struct.

  Revision 1.31  2015/07/21 21:48:01  rshill
  Added +++ comments to be addressed, concerning rotation matrices.

  Revision 1.30  2015/07/16 22:13:15  rshill
  Added CALDB filename resolution.

  Revision 1.29  2015/04/02 17:42:08  asargent
  Utilization of ahmission::getTELESCOPString during CAMS file creation.

  Revision 1.28  2015/03/25 21:52:11  asargent
  New TelDef keywords with X-location for instruments. Allowed use of "none" in file input

  Revision 1.27  2015/03/03 16:02:20  asargent
  Use of attitude library functions for vector and matrix algebra

  Revision 1.26  2015/02/04 16:53:58  asargent
  Bug fix in sign function. Function was truncating value prematurely if value was less than 0. Simplified routine by simple if statements.

  Revision 1.25  2015/01/07 16:58:11  asargent
  Updated parameters. See issue #472

  Revision 1.24  2014/09/15 14:17:47  mwitthoe
  camssim: add extended syntax support for the opening of the EVENTS files

  Revision 1.23  2014/08/05 14:01:33  asargent
  Updated sign function to accept integers rather than doubles to fix possible compiler issues.

  Revision 1.22  2014/02/20 19:49:29  asargent
  Updated algorithm for processing HXI events. Updated comments.

  Revision 1.21  2014/01/29 19:24:57  asargent
  Removed boolean check for seeding.

  Revision 1.20  2014/01/29 17:53:01  asargent
  Added new custom seed options, define in par file. Fixed an error where simerrorfreq was set equal to simerror3.

  Revision 1.19  2014/01/29 17:09:30  asargent
  Fixed a bug where invCAMS was not generating a new random number for each error calculation.

  Revision 1.18  2014/01/29 14:56:28  asargent
  Fixed typo in invCAMS, moved writing of CAMS files to include all subtimes.

  Revision 1.17  2014/01/28 22:06:06  asargent
  Corrected an offset error in the final algorithm in processEvents

  Revision 1.16  2014/01/28 18:43:09  asargent
  Moved null flags to processEvent function.

  Revision 1.15  2014/01/28 17:40:07  asargent
  Added reset for second HXI file null variables.

  Revision 1.14  2014/01/28 14:44:17  asargent
  Added check to verify different input files.

  Revision 1.13  2014/01/28 14:24:06  asargent
  Added in additional offset to r_ideal to keep the coordinates centered. Made integer casting more explicit in calculation of RAW coordinates.

  Revision 1.12  2014/01/27 21:52:57  asargent
  Changed column names to be the same in both CAMS output files. Added in divide-by-zero check in sign function.

  Revision 1.11  2014/01/27 19:10:37  asargent
  Fixed debugging error

  Revision 1.10  2014/01/27 18:34:06  asargent
  Added doxygen comments

  Revision 1.9  2014/01/27 15:51:59  asargent
  Removed a debugging check.

  Revision 1.8  2014/01/27 15:48:44  asargent
  Fixed a conversion error for gamma from mrad to rad. Fixed errors during EOBmotion function where the wrong matrix was used.

  Revision 1.7  2014/01/23 19:20:32  asargent
  Added in checks for TNULL during HXI event loop(s)

  Revision 1.6  2014/01/23 15:29:32  asargent
  Added in TNULL column for RAWX and RAWY null values.

  Revision 1.5  2014/01/22 21:25:51  asargent
  Fixed a typo during HXI event loop. Added option to not include one input file.

  Revision 1.4  2014/01/22 02:39:00  klrutkow
  CR: comments

  Revision 1.3  2014/01/17 18:12:20  asargent
  Fixed random number generator, and added several comments

  Revision 1.2  2014/01/16 23:11:40  asargent
  Fixed a bug where the outputfiles weren't closing. Fixed order of certain FITS event loops

  Revision 1.1  2014/01/15 21:59:27  asargent
  Initial version of camssim.cxx

*/
