/// \file hxisgdpha.cxx
/// \brief Tool to compute calibrated pulse height for HXI/SGD.
/// \author Robert S. Hill
/// \date $Date: 2016/07/12 15:27:01 $

/**

\defgroup tool_hxisgdpha Compute Calibrated Pulse Height EPI from Raw PHA (hxisgdpha)
@ingroup mod_hxisgd_tasks

This tool populates the EPI column in the SFF for the HXI and SGD instruments.
Common-mode noise is subtracted from each PHA value and a gain correction curve
is applied to give EPI.  Signals from bad or noisy readouts or outside the
domain of the gain correction are set to null.

Source files:

  hxisgdpha.cxx
  hxisgdphalib.cxx
  hxisgdphalib.h

Library dependencies:

  astroh/hxisgd/lib/hxisgdevtid/
  astroh/mission/lib/ahmission/ahmission
  astroh/mission/lib/ahmission/caldb
  astroh/gen/lib/ahapp
  heacore/ahgen
  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils

Status bits changed:

  PROC_STATUS - not changed

  STATUS
   Bit Description
    0   Signal marked as bad or noisy by bad pixel CALDB file
    1   PHA < 0 or PHA > 1023
    2   PHA - (common mode noise) < 0
    3   PHA - (common mode noise) is outside domeain of gain spline
    4   Always cleared
    5   SGD only - LCHK bit is set
    6   SGD only - alternative version of gain spline used (for certain
        ASICs if corresponding TRIGPAT bit is clear)
    7   Always cleared

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-06  RSH     initial version, after cleaning code

*/

#define AHLABEL tool_hxisgdpha
#define AHCVSID "$Id: hxisgdpha.cxx,v 1.72 2016/07/12 15:27:01 rshill Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "hxisgdphalib.h"
#include "hxisgdevtid/badpix.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahapp/ahapp.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "hdcal.h"

#include <cmath>
#include <sstream>
#include <cstring>
#include <stdexcept>

const int PHAMIN=0;                 //  Minimum valid PHA.
const int PHAMAX=1023;              //  Maximum valid PHA.

const int SGD_MAX_ASICS = 208;
const int HXI_MAX_READOUTS = 1280;
const int SGD_MAX_READOUTS = 13312;
const int SGD_TRIGPAT_SIZE = 31;    //  Number of bits in FLAG_TRIGPAT
const int SGD_LCHK_SIZE = 1;        //  Number of bits in FLAG_LCHK
const int STATUS_SIZE=8;            //  Number of bits in output STATUS.
const int PROC_STATUS_SIZE = 32;    //  Number of bits in input PROC_STATUS.

/** \addtogroup tool_hxisgdpha
 *   @{
 *   */

/// \brief Get parameter values
/// \param[out] infile name of input SFF
/// \param[out] outfile name of output SFF with EPI filled in
/// \param[out] gainfile name of FITS file with table of PHA calibration info,
///    or CALDB if the file exists in the calibration database
/// \param[out] badpixfile name of FITS file with table of active readout channels
///    flags, or CALDB if the file exists in the calibration database
/// \param[out] outnsubcol write the PHA_NSUB column
/// \param[out] datamode overrides datamode keyword in event file header
/// \param[out] randomize randomize fractional part of PHA
/// \param[out] seed random number seed
void getPar(std::string & infile, std::string & outfile, 
  std::string & gainfile, std::string & badpixfile, bool & outnsubcol,
  std::string & datamode, bool & randomize, int & seed);
  
/// \brief Open output file and get instrument identifier
/// \param[in] infile name of input SFF
/// \param[in] outfile name of output SFF with EPI filled in
/// \param[in] gainfile name of CALDB file for PHA calibration
/// \param[in] badpixfile name of CALDB file for active readout channel flags
/// \param[in] outnsubcol write the PHA_NSUB column
/// \param[in] datamode overrides datamode keyword in event file header
/// \param[out] instrume "HXI" or "SGD"
/// \param[out] gain_table struct with gain functions
/// \param[out] altgain_table struct with alternative gain functions
/// \param[out] activechan_table struct with active channel information
/// \param[out] fp_out pointer to output file (destination)
void initialize(const std::string infile, const std::string outfile, 
  const std::string gainfile, const std::string badpixfile, 
  const bool outnsubcol, const std::string datamode,
  const bool randomize, int & seed, std::string & instrume, 
  ahmission::spline::SplineSetMap & gain_table, 
  ahmission::spline::SplineSetMap & altgain_table, 
  hxisgdevtid::badpix::ActiveChanTable & activechan_table,
  ahfits::FilePtr & fp_out);

/// \brief Invoke the PHA calibration algorithm to produce EPI
/// \param[in] instrume "HXI" or "SGD"
/// \param[in] gain_table struct with gain functions
/// \param[in] altgain_table struct with alternative gain functions
/// \param[in] activechan_table struct with active channel information
/// \param[in] outnsubcol write the PHA_NSUB column
/// \param[in] randomize randomize fractional part of PHA
/// \param[in] seed random number seed
/// \param[in] fp_out pointer to output file (destination)
void doWork(const std::string infile,
  ahmission::spline::SplineSetMap & gain_table, 
  ahmission::spline::SplineSetMap & altgain_table, 
  hxisgdevtid::badpix::ActiveChanTable & activechan_table,
  const bool outnsubcol,
  const bool randomize, int seed,
  ahfits::FilePtr fp_out);

/// \brief Close files, free data structures.
/// \param[in] activechan_table struct with active channel information
/// \param[in] outnsubcol write the PHA_NSUB column
/// \param[in] fp_out pointer to output file (destination)
void finalize(
  ahmission::spline::SplineSetMap & gain_table, 
  ahmission::spline::SplineSetMap & altgain_table, 
  hxisgdevtid::badpix::ActiveChanTable & activechan_table,
  ahfits::FilePtr & fp_out);

/// \brief Set status flags OK.
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusOK(char* status, long& count);

/// \brief Set status flag for bad/noisy pixel. 
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusBadNoisy(char* status, long& count);

/// \brief set status flag for pha out of valid range
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusPHARange(char* status, long& count);

/// \brief set status flag for pha-common mode noise less than zero
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusPHANeg(char* status, long& count);

/// \brief set status flag for pha-common mode noise outside domain of gain function
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusPHAGainDomain(char* status, long& count);

/// \brief set status flag for proc_status != 0
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
// void setStatusProcStatus(char* status, long& count);

/// \brief set status flag for length-check flag set
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusLengthCheck(char* status, long& count);

/// \brief set status flag for alternative gain
/// \param[in,out] status status flags, one per element
/// \param[in,out] count incremented for each execution
void setStatusAltGain(char* status, long& count);

// ****************************************************************************

int main(int argc, char** argv) {

  std::string infile;          // Input Second Fits File (SFF).
  std::string outfile;         // Output Second Fits File (SFF).
  std::string gainfile;        // Input gain calibration CALDB file.
  std::string badpixfile;      // Input active channel CALDB file.
  std::string instrume;        // "HXI" or "SGD".
  std::string datamode;        // DATAMODE to override value in event file header.
  ahfits::FilePtr fp_out = 0;  // File pointer to output file.
  bool randomize;              // Flag whether to provide random fractional part for PHA.
  int seed;                    // Random number seed.
  bool outnsubcol;             // Flag to select whether to fill the PHA_NSUB column.
  ahmission::spline::SplineSetMap gain_table;             // Structure for gain calibration data.
  ahmission::spline::SplineSetMap altgain_table;          // Structure for alternative gain calibration data.
  hxisgdevtid::badpix::ActiveChanTable activechan_table;  // Structure for active channel data.

  int status = ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      ahapp::writeParametersToLog();
      getPar(infile, outfile, gainfile, badpixfile, outnsubcol, 
             datamode, randomize, seed);
      initialize(infile, outfile, gainfile, badpixfile, outnsubcol, datamode,
                 randomize, seed, instrume, gain_table, altgain_table,
                 activechan_table, fp_out);
      doWork(instrume, gain_table, altgain_table, activechan_table, outnsubcol, 
             randomize, seed, fp_out);
      finalize(gain_table, altgain_table, activechan_table, fp_out);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile, outfile, gainfile, badpixfile, outnsubcol,
              datamode, randomize, seed);
        initialize(infile, outfile, gainfile, badpixfile, outnsubcol, datamode,
                   randomize, seed, instrume, gain_table, altgain_table,
                   activechan_table, fp_out);
        doWork(instrume, gain_table, altgain_table, activechan_table, outnsubcol, 
               randomize, seed, fp_out);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(gain_table, altgain_table, activechan_table, fp_out);
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During shutDown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(std::string & infile, std::string & outfile, 
  std::string & gainfile, std::string & badpixfile,
  bool & outnsubcol, std::string & datamode, 
  bool & randomize, int & seed) {

  infile = ahapp::getParString("infile");
  outfile = ahapp::getParString("outfile");
  gainfile = ahapp::getParString("gainfile");
  badpixfile = ahapp::getParString("badpixfile");
  outnsubcol = ahapp::getParBool("outnsubcol");
  datamode = ahapp::getParString("datamode");
  randomize = ahapp::getParBool("randomize");
  seed = ahapp::getParInt("seed");
}

// ----------------------------------------------------------------------------

void initialize(const std::string infile, const std::string outfile, 
  const std::string gainfile, const std::string badpixfile, 
  bool outnsubcol, const std::string datamode, bool randomize, int & seed,
  std::string & instrume,
  ahmission::spline::SplineSetMap & gain_table, 
  ahmission::spline::SplineSetMap & altgain_table, 
  hxisgdevtid::badpix::ActiveChanTable & activechan_table,
  ahfits::FilePtr & fp_out) {

  std::string badpixcaldbfile = ""; // Active channel/bad pixel CALDB filename to be used.
  std::string actualgainfile = "";  // Gain CALDB filename to be used.
  std::string ev_telescop = "";     // Value of keyword TELESCOP in FFF.
  std::string ev_instrume = "";     // Value of keyword INSTRUME in FFF.
  std::string ev_detnam = "";       // Value of keyword DETNAM in FFF.
  std::string ev_datamode = "";     // Value of keyword DATAMODE in FFF.
  std::string ev_date_obs = "";     // Value of keyword DATE-OBS in FFF.
  std::string badpix_datamode = ""; // Value of keyword DATAMODE in bad pixel CALDB file.

  int maxreadout = 0;
  std::size_t i = 0;                  // Location of underscore in DATAMODE
  std::string tdatamode = "";         // Temporary string for DATAMODE parsing

  // Create the output SFF.
  //
  AH_INFO(ahlog::HIGH) << "Input file:   " << infile << std::endl;
  AH_INFO(ahlog::HIGH) << "Output file:  " << outfile << std::endl;
   ahfits::clone(infile, outfile, &fp_out, true);
  if (ahfits::isPrimary(fp_out)) ahfits::move(fp_out, "EVENTS");    // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fp_out, infile);

  ev_telescop = ahfits::getKeyValStr(fp_out, "TELESCOP");
  ev_instrume = ahfits::getKeyValStr(fp_out, "INSTRUME");
  ev_detnam   = ahfits::getKeyValStr(fp_out, "DETNAM");
  ev_datamode = ahfits::getKeyValStr(fp_out, "DATAMODE");

  AH_INFO(ahlog::HIGH) << "TELESCOP:  " << ev_telescop << "   INSTRUME:  " << ev_instrume
    << "   DETNAM:  " << ev_detnam << "   DATAMODE:  " << ev_datamode << std::endl;
  
  if (!ahmission::isValidTELESCOP(ev_telescop)) {
    AH_THROW_RUNTIME("Bad TELESCOP keyword in event file");
  }

  if (ahgen::strtoupper(ev_instrume) == "HXI1" || 
      ahgen::strtoupper(ev_instrume) == "HXI2" ) {
    instrume = "HXI";
    maxreadout = HXI_MAX_READOUTS;
    if (ahgen::strtoupper(ev_detnam) != "CAMERA") {
      AH_THROW_RUNTIME("DETNAM must be CAMERA for HXI");
    }
  } else if (ahgen::strtoupper(ev_instrume) == "SGD1" || 
             ahgen::strtoupper(ev_instrume) == "SGD2" ) {
    instrume = "SGD";
    maxreadout = SGD_MAX_READOUTS;
    if (ahgen::strtoupper(ev_detnam) != "CC1" && 
        ahgen::strtoupper(ev_detnam) != "CC2" && 
        ahgen::strtoupper(ev_detnam) != "CC3") {
      AH_THROW_RUNTIME("DETNAM must be CC1, CC2, or CC3 for SGD");
    }
  } else {
    AH_THROW_RUNTIME("valid INSTRUME not found in FITS file header");
  }

  // this will be used in the CALDB query
  ev_date_obs = ahfits::getKeyValStr(fp_out, "DATE-OBS");

  // Add the PHA_NSUB column if called for.
  //
  if (outnsubcol) {
    std::stringstream ss;
    ss.str("");
    ss << "1PI(" << maxreadout << ")";
    ahfits::insertColAfter(fp_out, "PHA_NSUB", ss.str(), "PHA");
    ahfits::setColumnDescription(fp_out,"PHA_NSUB","PHA with common mode noise subtracted");
  }

  // Load the PHA gain calibration file as a function of instrument, time, and detector.
  //
  actualgainfile=ahmission::caldb::resolve(gainfile, "gain", ev_instrume, ev_detnam, "GAIN", ev_date_obs);
  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("gainfile",actualgainfile.c_str());   
  pha::loadGainTable(actualgainfile, ev_instrume, ev_detnam, gain_table);
  if (ahgen::strtoupper(instrume) == "SGD") {
    pha::loadAltGainTable(actualgainfile, ev_instrume, ev_detnam, altgain_table);
  }

  //  Load the active/dead readout calibration file as a function of instrument and detector.
  //
  badpixcaldbfile=ahmission::caldb::resolve(badpixfile, "bad pixel", ev_instrume, ev_detnam, "BADPIX", ev_date_obs);
  AH_DEBUG << "Resolved CALDB file = " << badpixcaldbfile << std::endl;
  ape_trad_set_string("badpixfile",badpixcaldbfile.c_str());   // to record actual file path in history

  //  Get the active/dead channel flags as a function of the effective
  //  DATAMODE (header value with possible override by parameter value).
  //
  if (!datamode.empty() && ahgen::strtoupper(datamode) != "NONE") {
    tdatamode = datamode;
  } else {
    tdatamode = ev_datamode;
  }

  // Take portion of DATAMODE after the underscore.
  //
  i = tdatamode.rfind("_");   // locate underscore character
  if (i == std::string::npos) 
    AH_THROW_RUNTIME("expecting underscore character in input DATAMODE, e.g. CAMERA_NORMAL1");
  // Substring starts one character past the underscore.
  badpix_datamode=tdatamode.substr(++i);
  hxisgdevtid::badpix::loadActiveChan(badpixcaldbfile, ev_instrume, ev_detnam,
                                      badpix_datamode, activechan_table);
  
  // Seed the random number generator used to convert integral PHA to float.
  //
  if (randomize) ahgen::seedRandom(seed);

  // Write list of parameters to log file
  //
  ahapp::writeParametersToLog(); 

}

// ----------------------------------------------------------------------------

void doWork(const std::string instrume, 
  ahmission::spline::SplineSetMap & gain_table, 
  ahmission::spline::SplineSetMap & altgain_table, 
  hxisgdevtid::badpix::ActiveChanTable & activechan_table, bool outnsubcol,
  bool randomize, int seed, 
  ahfits::FilePtr fp_out) {

  //  Arrays to receive data from input FITS table.
  //  The same for both HXI and SGD.
  //
  char proc_status[PROC_STATUS_SIZE];        //  PROC_STATUS flags
  char status[STATUS_SIZE];                  //  STATUS flags
  ahfits::IndexType num_status=STATUS_SIZE;  //  Store number of STATUS flags in a variable
  short num_asic=0;                          //  Number of ASICs represented in telemetry
  short num_readout[SGD_MAX_ASICS];          //  Number of readouts represented for each ASIC
  short asic_id[SGD_MAX_ASICS];              //  ASIC identifier
  short asic_ref[SGD_MAX_ASICS];             //  ASIC reference level
  short asic_cmn[SGD_MAX_ASICS];             //  ASIC common mode noise
  short readout_id_rmap[SGD_MAX_READOUTS];   //  Sequential identifier of readout
  short pha[SGD_MAX_READOUTS];               //  Raw PHA from telemetry
  short pha_nsub[SGD_MAX_READOUTS];          //  PHA with common mode noise subtracted
  float epi[SGD_MAX_READOUTS];               //  Gain calibrated PHA (rounded to integral value)
  char epi_nulls[SGD_MAX_READOUTS];          //  Null value flags for EPI
  char trigpat[SGD_TRIGPAT_SIZE];            //  Trigger pattern flags FLAG_TRIGPAT
  char lchk[SGD_LCHK_SIZE];                  //  Length check flag FLAG_LCHK

  long n_rows = 0;
  std::string n_rows_keyword = "NAXIS2";
  bool altgain = false;

  //  Counts of valid or invalid rows in various categories.
  //
  long num_good_rows=0;           //  Good rows.
  long num_bad_rows=0;            //  Bad rows, any category.
  long num_bad_proc_status=0;     //  Rows skipped due to proc_status.
  long num_output_rows=0;         //  Output rows.
  long num_status_ok=0;           //  STATUS is OK (no bits set).
  long num_status_bad_noisy=0;    //  Bad/noisy readout.
  long num_status_pha_range=0;    //  PHA out of range.
  long num_status_pha_neg=0;      //  PHA < zero.
  long num_status_gain_domain=0;  //  PHA outside domain of spline.
  long num_status_lchk=0;         //  SGD length check (FLAG_LCHK).
  long num_status_alt_gain=0;     //  SGD alternative gain used
  long num_ordinary_gain=0;       //  SGD ordinary gain used

  bool row_ok = true;             //  Current row is OK.
 
  //  Counts of elements in variable-length columns.
  //
  ahfits::IndexType asic_length=0, readout_length=0, proc_status_length=0;
  ahfits::IndexType trigpat_length=0, lchk_length=0;

  //  FITS binary table setup.
  //
  ahfits::Router update_router(fp_out);

  AH_DEBUG << "Beginning router connections" << std::endl;
  update_router.connectScalar(ahfits::e_READONLY,"NUM_ASIC", num_asic);
  if (ahgen::strtoupper(instrume) == "SGD") {
    update_router.connectBit(ahfits::e_READONLY, "FLAG_TRIGPAT", trigpat, trigpat_length);
    update_router.connectBit(ahfits::e_READONLY, "FLAG_LCHK", lchk, lchk_length);
  }
  update_router.connectBit(ahfits::e_READONLY,"PROC_STATUS", proc_status, proc_status_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"NUM_READOUT", num_readout, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_ID", asic_id, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_REF", asic_ref, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_CMN", asic_cmn, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP", readout_id_rmap, readout_length);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"PHA", pha, readout_length);
  update_router.connectBit(ahfits::e_WRITEONLY, "STATUS", status, num_status);
  if (outnsubcol) {
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"PHA_NSUB", pha_nsub, readout_length);
  }
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"EPI", epi, readout_length, epi_nulls);


  //  Process row by row.
  //
  n_rows = ahfits::getKeyValLLong(fp_out, n_rows_keyword);
  row_ok = true;
  altgain = false;
  for (long i=0; i<n_rows; ++i) {

    int x=0;                                 // PHA with common mode noise subtracted.
    int retcode=0;                           // Return code from spline coefficient lookup.
    bool at_boundary = false;                // Is x at a node of the spline.
    double coeff_set1[4], coeff_set2[4];     // Coefficients returned by spline lookup.
    double depi1=0.0, depi2=0.0;             // Intermediate results of gain calibration.
    double xr=0.0;                           // May be either x or x+(random fractional part).

    int common_mode_noise = 0;               // A value from ASIC_CMN column.
    int n_readouts_this_asic = 0;            // Count of readouts to limit inner loop.
    int prev_readouts = 0, k=0;              // Markers to manage progress through readouts.

    // Active channel flag for current signal.
    hxisgdevtid::badpix::ActiveChanVal activechan_current = hxisgdevtid::badpix::e_BAD_PIXEL;

    if (i == 0) {
      ahfits::firstRow(fp_out);
    } else {
      ahfits::nextRow(fp_out);
    }
    ahfits::readRow(fp_out);    

    AH_DEBUG << "Row # " << i << " " << "num_asic = " << (int) num_asic << std::endl;

    for (int n=0; n<SGD_MAX_READOUTS; ++n) {
      epi_nulls[n] = 0;
      epi[n] = 0.0;
      if (outnsubcol) pha_nsub[n] = 0;
    }

    //  Initialize the row flags.
    //
    row_ok = true;
    setStatusOK(status, num_status_ok);

    //  Check PROC_STATUS to determine whether to process the row.
    //
    if (!procstatus::processRow(proc_status)) {

      num_bad_proc_status++;

    } else{

      if (ahgen::strtoupper(instrume) == "SGD") {
        std::string bits="";
        for (int ibit=0; ibit<SGD_TRIGPAT_SIZE; ++ibit) {
          bits += (0 != trigpat[ibit] ? "1" : "0");
        }
      }
      prev_readouts = 0;
      k=0;
      for (int j=0; j<num_asic; ++j) {

        AH_DEBUG << "ASIC #" << j << std::endl;

        n_readouts_this_asic = num_readout[j];
        common_mode_noise = asic_cmn[j];

        // For SGD only, evaluate FLAG_TRIGPAT to determine whether the
        // regular or the alternative gain should be used.
        //
        //     ASIC ID (hex)       ASIC_ID (decimal)        READOUT_ID_RMAP
        //     Min        Max        Min       Max          Min         Max
        //     0x0        0x7          0         7            1         512
        //    0x10       0x17         16        23          513        1024
        //    0x20       0x27         32        39         1025        1536
        //    0x30       0x37         48        55         1537        2048
        //   0x100      0x107        256       263         3329        3840
        //   0x110      0x117        272       279         3841        4352
        //   0x120      0x127        288       295         4353        4864
        //   0x130      0x137        304       311         4865        5376
        //   0x200      0x207        512       519         6657        7168
        //   0x210      0x217        528       535         7169        7680
        //   0x220      0x227        544       551         7681        8192
        //   0x230      0x237        560       567         8193        8704
        //   0x300      0x307        768       775         9985       10496
        //   0x310      0x317        784       791        10497       11008
        //   0x320      0x327        800       807        11009       11520
        //   0x330      0x337        816       823        11521       12032
        altgain = false;
        if (ahgen::strtoupper(instrume) == "SGD") {
          if (0x0 <= asic_id[j] && asic_id[j] <= 0x7 && trigpat[0] == 0) {
            altgain = true;
          } else if (0x10 <= asic_id[j] && asic_id[j] <= 0x17 && trigpat[1] == 0) {
            altgain = true;
          } else if (0x20 <= asic_id[j] && asic_id[j] <= 0x27 && trigpat[2] == 0) {
            altgain = true;
          } else if (0x30 <= asic_id[j] && asic_id[j] <= 0x37 && trigpat[3] == 0) {
            altgain = true;
          } else if (0x100 <= asic_id[j] && asic_id[j] <= 0x107 && trigpat[7] == 0) {
            altgain = true;
          } else if (0x110 <= asic_id[j] && asic_id[j] <= 0x117 && trigpat[8] == 0) {
            altgain = true;
          } else if (0x120 <= asic_id[j] && asic_id[j] <= 0x127 && trigpat[9] == 0) {
            altgain = true;
          } else if (0x130 <= asic_id[j] && asic_id[j] <= 0x137 && trigpat[10] == 0) {
            altgain = true;
          } else if (0x200 <= asic_id[j] && asic_id[j] <= 0x207 && trigpat[14] == 0) {
            altgain = true;
          } else if (0x210 <= asic_id[j] && asic_id[j] <= 0x217 && trigpat[15] == 0) {
            altgain = true;
          } else if (0x220 <= asic_id[j] && asic_id[j] <= 0x227 && trigpat[16] == 0) {
            altgain = true;
          } else if (0x230 <= asic_id[j] && asic_id[j] <= 0x237 && trigpat[17] == 0) {
            altgain = true;
          } else if (0x300 <= asic_id[j] && asic_id[j] <= 0x307 && trigpat[21] == 0) {
            altgain = true;
          } else if (0x310 <= asic_id[j] && asic_id[j] <= 0x317 && trigpat[22] == 0) {
            altgain = true;
          } else if (0x320 <= asic_id[j] && asic_id[j] <= 0x327 && trigpat[23] == 0) {
            altgain = true;
          } else if (0x330 <= asic_id[j] && asic_id[j] <= 0x337 && trigpat[24] == 0) {
            altgain = true;
          }
          AH_DEBUG << "Row: " << i << "  ASIC_ID: " << asic_id[j] 
                   << "  altgain: " << (altgain?"true":"false") << std::endl;
        }

        for (int k1=0; k1<n_readouts_this_asic; ++k1) {

          k = k1 + prev_readouts;
          activechan_current = activechan_table[readout_id_rmap[k]];
          
          //  If readout is bad/noisy, then skip the PHA calibration 
          //  and flag EPI[k] to be set to TNULL.
          //        
          //if (activechan_table[readout_id_rmap[k]] != 1) {
          if (activechan_current == hxisgdevtid::badpix::e_BAD_PIXEL) {
            AH_DEBUG << "Bad channel " << k << " " << readout_id_rmap[k] 
                     << " " << activechan_current << std::endl;
            epi_nulls[k] = 1;     //  Output value of EPI[k] is TNULL.
            row_ok = false;
            setStatusBadNoisy(status, num_status_bad_noisy);
            continue;
          }

          //  If PHA is out of range, then skip the PHA calibration 
          //  and flag EPI[k] to be set to TNULL.
          //        
          if (pha[k] < PHAMIN || pha[k] > PHAMAX) {
            epi_nulls[k] = 1;     //  Output value of EPI[k] is TNULL.
            row_ok = false;
            setStatusPHARange(status, num_status_pha_range);
            continue;
          }

          //  For SGD only, LCHK == 1 requires special treatment.
          //
          if (ahgen::strtoupper(instrume) == "SGD") {

            if (lchk[0] == 1 || pha[k] == 1022 || pha[k] == 1023) {
              setStatusLengthCheck(status, num_status_lchk);
            }
          }

          //  Subtract ASIC_CMN = common mode noise. 
          //  (ASIC_REF = reference channel is not used.)
          //
          x = pha[k] - common_mode_noise;
          pha_nsub[k] = x;

          //  Negative energy:  set status, but still process.
          //
          if (x < 0) {
            row_ok = false;
            setStatusPHANeg(status, num_status_pha_neg);
          }

          //  Add random fractional part, if necessary.
          //
          xr = (double) x;
          if (randomize) xr += ahgen::getRandom() - 0.5;

          if (altgain) {  //  SGD only, select alternative gain if necessary
            retcode = ahmission::spline::get_coeff(
              altgain_table, readout_id_rmap[k], xr, at_boundary, 
              coeff_set1, coeff_set2);
            setStatusAltGain(status, num_status_alt_gain);
          } else {        //  Regular gain for all HXI and most SGD readouts
            retcode = ahmission::spline::get_coeff(
              gain_table, readout_id_rmap[k], xr, at_boundary, 
              coeff_set1, coeff_set2);
            num_ordinary_gain++;
          }

          //  x is outside the domain of the gain calibration.
          //
          if (retcode != 0) {
            epi_nulls[k] = 1;
            row_ok = false;
            setStatusPHAGainDomain(status, num_status_gain_domain);
            continue;
          }

          AH_DEBUG << "At boundary?  " << at_boundary << std::endl;
          AH_DEBUG << coeff_set1[3] << "   "<< coeff_set1[2] << "   "
                   << coeff_set1[1] << "   "<< coeff_set1[0] << std::endl;
          AH_DEBUG << coeff_set2[3] << "   "<< coeff_set2[2] << "   "
                   << coeff_set2[1] << "   "<< coeff_set2[0] << std::endl;

          //  Evaluate spline for interval containing x.
          //
          depi1 = coeff_set1[3]*pow(xr,3) + coeff_set1[2]*pow(xr,2) + coeff_set1[1]*xr + coeff_set1[0];

          //  If x is at an interval boundary, evaluate spline for
          //  the second interval containing x, and average the results
          //  of the two splines.
          //
          AH_DEBUG << "k=" << k << " pha[k]=" << pha[k] << " asic_cmn[j]=" 
                   << asic_cmn[j] << " x=" << x << " xr=" << xr<< std::endl;

          if (at_boundary) {
            depi2 = coeff_set2[3]*pow(xr,3) + coeff_set2[2]*pow(xr,2) + coeff_set2[1]*xr + coeff_set2[0];
            epi[k] = 0.5*(depi1 + depi2);
            AH_DEBUG << "epi1=" << depi1 << " epi2=" << depi2 << " epi[k]=" << epi[k] << std::endl;
          } else {
            epi[k] = depi1;
            AH_DEBUG << "epi1=" << depi1 << " epi[k]=" << epi[k] << std::endl;
          }
          
          //  Good readout.
          //
          epi_nulls[k] = 0;

        }

        prev_readouts += n_readouts_this_asic;
      }

      if (prev_readouts != readout_length) {
        AH_ERR << "Row " << i << " failed length check on readout-indexed vectors." << std::endl; 
        AH_ERR << prev_readouts << " (from NUM_READOUT) != " << readout_length 
            << " (vector dimension)" << std::endl;
        row_ok = false;
      }

      AH_DEBUG << "asic_length = " << asic_length << std::endl;
      AH_DEBUG << "readout_length = " << readout_length << std::endl;

      if (row_ok) {
        ++num_good_rows;
      } else {
        ++num_bad_rows;
      }
    }

    //  Writes all connected columns (including PHA_NSUB if outnsub parameter
    //  set to true specified).
    //
    ahfits::writeRow(fp_out);
    ++num_output_rows;

  }

  AH_INFO(ahlog::HIGH) << "Table row counts:" << std::endl;
  AH_INFO(ahlog::HIGH) << "   Input rows:   " << n_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "      Processed: " << num_good_rows + num_bad_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "         Good:   " << num_good_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "         Bad:    " << num_bad_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "      Skipped due to PROC_STATUS: " << num_bad_proc_status << std::endl;
  AH_INFO(ahlog::HIGH) << "   Output rows:  " << num_output_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of signals with the following conditions:" << std::endl;
  AH_INFO(ahlog::HIGH) << "   Readout is bad or noisy:                           " << num_status_bad_noisy << std::endl;
  AH_INFO(ahlog::HIGH) << "   PHA out of valid range:                            " << num_status_pha_range << std::endl;
  AH_INFO(ahlog::HIGH) << "   Noise-subtracted PHA less than zero:               " << num_status_pha_neg << std::endl;
  AH_INFO(ahlog::HIGH) << "   Noise-subtracted PHA outside gain function domain: " << num_status_gain_domain << std::endl;
  if (ahgen::strtoupper(instrume) == "SGD") {
    AH_INFO(ahlog::HIGH) << "   Length check:          " << num_status_lchk << std::endl;
    AH_INFO(ahlog::HIGH) << "   Alternative gain used: " << num_status_alt_gain << std::endl;
    AH_INFO(ahlog::HIGH) << "   Ordinary gain used:    " << num_ordinary_gain << std::endl;
  }

  if (num_status_ok != n_rows) AH_THROW_LOGIC("Number of times status cleared does not match number of rows.");

}

// ----------------------------------------------------------------------------

void finalize(ahmission::spline::SplineSetMap & gain_table, 
              ahmission::spline::SplineSetMap & altgain_table, 
              hxisgdevtid::badpix::ActiveChanTable & activechan_table,
              ahfits::FilePtr & fp_out) {
  gain_table.clear();
  altgain_table.clear();
  activechan_table.clear();
  ahfits::close(fp_out);
}

// ----------------------------------------------------------------------------

void setStatusOK(char* status, long& count) {
  for (int i=0; i<STATUS_SIZE; ++i) status[i] = 0;
  ++count;
}

// ----------------------------------------------------------------------------

void setStatusBadNoisy(char* status, long& count) {
  status[0] = 1;
  ++count;
}

// ----------------------------------------------------------------------------

void setStatusPHARange(char* status, long& count) {
  status[1] = 1;
  ++count;
}

// ----------------------------------------------------------------------------

void setStatusPHANeg(char* status, long& count) {
  status[2] = 1;
  ++count;
}

// ----------------------------------------------------------------------------

void setStatusPHAGainDomain(char* status, long& count) {
  status[3] = 1;
  ++count;
}

// ----------------------------------------------------------------------------

// void setStatusProcStatus(char* status, long& count) {
//  status[4] = 1;
//  ++count;
// }

// ----------------------------------------------------------------------------

void setStatusLengthCheck(char* status, long& count) {
  status[5] = 1;
  ++count;
}

// ----------------------------------------------------------------------------

void setStatusAltGain(char* status, long& count) {
  status[6] = 1;
  ++count;
}

/** @} */


/* Revision Log
 $Log: hxisgdpha.cxx,v $
 Revision 1.72  2016/07/12 15:27:01  rshill
 Corrections to log message output.

 Revision 1.71  2016/04/18 15:19:51  rshill
 Added status bits section to prologue.

 Revision 1.70  2016/04/01 20:06:07  mdutka
 adding parameter logging to standard mainand correcting checking of DETNAM and INSTRUME keywords so that it is case insensitive

 Revision 1.69  2016/01/28 01:39:30  mwitthoe
 hxisgdpha: remove log message sending binary output to the log file; convert some log messages to debug messages in order to reduce the log file size

 Revision 1.68  2015/12/29 16:50:05  rshill
 Added checkEmptyTable().

 Revision 1.67  2015/11/20 18:13:01  mwitthoe
 hxisgdpha: add column comment for PHA_NSUB column

 Revision 1.66  2015/10/27 15:12:26  rshill
 Change pha randomization so that random number range is -0.5 to +0.5
 rather than 0 to 1.

 Revision 1.65  2015/10/22 22:27:40  rshill
 Fixed two bugs reported by Hiro:  (1) make instrume
 pass-by-reference in initialize; (2) change 330 to 337 in ASIC_ID check.
 Also, incorporated instrument team's prescription for lchk and signal=1022 or 1023.

 Revision 1.64  2015/09/04 16:04:13  rshill
 Added parameter stamping to log.

 Revision 1.63  2015/08/18 22:03:09  rshill
 Corrected mistakes in log output.

 Revision 1.62  2015/08/13 01:29:46  rshill
 Additional logging.

 Revision 1.61  2015/08/06 19:36:52  rshill
 Condensed comment prologue.

 Revision 1.60  2015/07/15 18:52:23  klrutkow
 added CALDB queries with ahmission query

 Revision 1.59  2015/07/13 17:30:38  klrutkow
 removed unused CALDB variables in initialize()

 Revision 1.58  2015/07/13 00:16:59  klrutkow
 call resolve() to get gainfile and badpixfile CALDB files ; edited if-block to ensure INSTRUME has the 1 or 2, so it can't just be HXI or SGD, it must be HXI1, HXI2, SGD1, or SGD2

 Revision 1.57  2015/04/23 22:42:04  rshill
 For SGD flag length check in STATUS only if PHA=1022 or 1023.

 Revision 1.56  2015/03/18 19:56:04  asargent
 Changed DETNAME to DETNAM

 Revision 1.55  2015/03/16 17:57:42  mwitthoe
 hxisgdpha: move active channel structure and load function into the hxisgdevtid library (these are now needed by sgdevtid)

 Revision 1.54  2015/03/13 22:05:39  rshill
 Delete strange pixel processing; use ASIC_ID to select nontrigger signals.

 Revision 1.53  2015/03/03 20:28:03  rshill
 Strange SGD readouts; correction to randomization.

 Revision 1.52  2015/01/20 18:39:51  rshill
 Corrected bug whereby PHA to EPI conversion was skipped
 for PHA < ASIC_CMN.

 Revision 1.51  2015/01/09 23:00:00  mwitthoe
 hxisgdpha: default value for datamode parameter is now NONE instead of an empty string (although an empty string still works)

 Revision 1.50  2014/12/31 16:04:56  rshill
 Backed out of check of standard telescope name - should be coordinated.

 Revision 1.49  2014/12/24 16:03:18  rshill
 Moved DATAMODE underscore split to initialize.

 Revision 1.48  2014/12/23 21:18:48  mwitthoe
 hxisgdpha: update parameter definitions; see issue 472

 Revision 1.47  2014/12/18 15:50:13  rshill
 Standard TELESCOP string enforced.

 Revision 1.46  2014/12/01 21:56:37  rshill
 Deleted the line setting EPI=NULL for negative PHA_NSUB.

 Revision 1.45  2014/09/15 20:34:11  mwitthoe
 hxisgdpha: add extended syntax support for input file; see issue 179

 Revision 1.44  2014/09/12 21:29:40  mwitthoe
 hxisgdpha: remove instrument argument in procstatus::processRow() -- there is no instrument dependency

 Revision 1.43  2014/07/22 21:00:51  mwitthoe
 hxisgdpha: bad ACTIVE_FLAG value in badpix CALDB file was changed from -1 to 0

 Revision 1.42  2014/07/22 17:59:06  mwitthoe
 hxisgdpha: add checks for INSTRUME/DETNAME to CALDB load functions; add datamode parameter to parameter file; remove compiler warnings

 Revision 1.41  2014/06/05 22:51:24  rshill
 Response to TRF review.  CALDB formats updated,
 documentation improved.

 Revision 1.40  2014/05/01 19:17:56  rshill
 Partially fixed up while editing the TRF.  Does not build.

 Revision 1.39  2014/03/13 22:49:59  rshill
 Add randomization of PHA.

 Revision 1.38  2014/02/27 23:03:44  mwitthoe
 hxisgdpha: stop rounding EPI to nearest integer

 Revision 1.37  2014/02/07 23:13:57  rshill
 Changed double connected to E FITS column to a float.

 Revision 1.36  2014/02/07 22:02:02  rshill
 Fixed initialization error.

 Revision 1.35  2014/02/01 03:43:36  rshill
 Changed control totals.

 Revision 1.34  2014/01/31 20:41:50  rshill
 Upgraded proc_status processing. Improved const correctness.

 Revision 1.33  2014/01/31 00:07:25  rshill
 Floating EPI, moved misplaced getKeyValStr.

 Revision 1.32  2014/01/17 14:33:33  rshill
 Completed handling of PHA_NSUB column

 Revision 1.31  2014/01/17 04:21:14  rshill
 Revised to current TRF; dealt with code review comments.

 Revision 1.30  2014/01/16 18:37:15  rshill
 Added PHA_NSUB column and outnsubcol;
 changed EPI to double.

 Revision 1.29  2014/01/09 19:56:57  klrutkow
 klrutkow: code review: comments

 Revision 1.28  2014/01/09 19:44:33  mwitthoe
 hxisgdpha: code review: variable declarations

 Revision 1.27  2014/01/09 19:36:15  asargent
 Added CR comments pertaining to main() structure

 Revision 1.26  2014/01/03 22:11:49  rshill
 Standard main and consolidated function files.

 Revision 1.25  2013/12/02 22:47:23  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.24  2013/10/17 21:09:40  rshill
 Corrected head comment.

 Revision 1.23  2013/10/17 20:42:09  rshill
 Edited comments about unimplemented features that are in fact
 implemented.

 Revision 1.22  2013/10/16 17:10:46  rshill
 Converted connects to new calls.

 Revision 1.21  2013/06/18 00:36:34  rshill
 Additional debug statements

 Revision 1.20  2013/06/14 18:28:38  rshill
 Standardized the statement order; added more comments;
 introduced rounding of results.

 Revision 1.19  2013/04/10 21:33:13  rshill
 Got rid of hxisgdpha:: namespace.

 Revision 1.18  2013/04/04 22:58:48  rshill
 Move CALDB code out of old ahcaldb area. Got rid of static global
 data structures. Streamlined data structures containing CALDB data.
 Revamped CALDB format per discussions with Japan.

 Revision 1.17  2013/04/01 18:44:27  rshill
 Delete AhFitsHeaderInfo variable.

 Revision 1.16  2013/04/01 17:53:43  rshill
 Active channel CALDB file.

 Revision 1.15  2013/03/20 22:50:25  rshill
 FITS column read/write flags.

 Revision 1.14  2013/02/21 19:27:07  rshill
 Added code to read in active channel flags (but not to use them).

 Revision 1.13  2013/01/24 21:34:23  rshill
 Update doxygen.

 Revision 1.12  2013/01/19 02:06:07  rshill
 Debug statements.

 Revision 1.11  2013/01/19 00:13:31  rshill
 Cleaned up code.  Move polynomial computation into main program.
 Added comments indicating future development:  time-dependent CALDB,
 dead channel flagging, detector-dependent CALDB.

 Revision 1.10  2013/01/16 22:03:27  rshill
 Cleaned up code.

 Revision 1.9  2013/01/14 04:43:09  rshill
 Initialized Fileptr to 0.

 Revision 1.8  2013/01/11 23:02:23  rshill
 Lengthened arrays for SGD; added a \todo.

 Revision 1.7  2013/01/11 00:16:10  rshill
 Added code to really use a proposed PHA CALDB file format.

 Revision 1.6  2013/01/09 18:43:54  rshill
 Cleaned up doxygen markup.

 Revision 1.5  2013/01/06 03:05:10  rshill
 Change extension name to EVENTS; change stub computation.

 Revision 1.4  2012/12/11 21:42:57  rshill
 Scrubbed for current ahfits & for coding standards.

 Revision 1.3  2012/11/30 20:30:34  rshill
 Added some AH_DEBUG output.

 Revision 1.2  2012/11/30 01:01:38  rshill
 Stub version that at least builds and should execute, but not yet tested.

 Revision 1.1  2012/11/30 00:20:29  rshill
 Initial writing of hxisgdpha, to fill calibrated pulse-height column in SFF.


*/
