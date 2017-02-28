/// \file sgdevtid.cxx
/// \brief Reconstruct SGD events
/// \author Robert S. Hill
/// \date $Date: 2016/04/18 20:08:27 $

/** 

\defgroup tool_sgdevtid Reconstruct SGD events (sgdevtid)
@ingroup mod_hxisgd_tasks

This task reconstructs photon events from SGD data.  Detection of a
high-energy photon can produce several secondary signals in SGD pixels
as a result of the interaction of the photon with the instrument.  Such
signals are grouped into occurrences.  Event reconstruction means
tracing all the signals in an occurrence back to the initial detection
and computing the incident energy of the photon.

Source files:
  sgdevtid.cxx
  sgdevtidlib.h
  sgdevtidlib.cxx
  sgdevtidcaldb.h
  sgdevtidcaldb.cxx

Library dependencies:

  astroh/hxisgd/lib/hxisgdevtid/
  astroh/mission/lib/ahmission/
  astroh/gen/lib/ahapp
  heacore/ahgen/ahgen
  heacore/ahgen/BitBuf
  heacore/ahfits
  heacore/ahlog
  heacore/ape

Status bits changed:

PROC_STATUS - not changed
STATUS - not changed
 ------(Event reconstruction outcome)---------------------------------

RECO_STATUS - Bit field written for each occurrence (SFFa row):
 Bit  Description
 ------(Occurrence mode identification)-------------------------------
  0    not used
  1    Mode is PSEUDO
  2    Mode is CALMODE 
  3    Mode is READALL
  4    Mode is NOTSURE
 ------(Event reconstruction skipped for this occurrence)-------------
  5    Bad PROC_STATUS
  6    Mode is READALL or CALMODE and SKIPRECO option is YES
  7    Occurrence has no signals
  8    FASTBGO or HITPAT flag is set and REJECTBGO option is YES
  9    Occurrence has too many signals to reconstruct
 10    All signals in the occurrence are below threshold
 ------(Event could not be reconstructed)-----------------------------
 11    Signal cluster has too many signals
 12    Signal cluster has invalid shape
 13    Too many signals in a group within Si layers
 14    Occurrence has too many hits to reconstruct
 15    All hits are bad by F test for Compton scattering (2 or fewer hits)
 16    All hits are bad by F test for Compton scattering (3 or more hits)
 17    All hits are bad by G test for Compton scattering 
 18    All hits are bad because they have low probability (2 or fewer hits)
 19    All hits are bad because they have low probability (3 or more hits)
 20    Singularity in escape energy computation
 ------(Trivial reconstruction)---------------------------------------
 21    One signal above threshold, and one signal in the occurrence
 22    One signal above threshold, and multiple signals in the occurrence
 ------(Complex reconstruction succeeded)-----------------------------
 ------(  reconstruction by merging signals into one hit)-------------
 23    One hit is left after merging signal clusters
 24    One hit is left after merging CdTe fluorescence within a CdTe layer
 25    One hit is left after merging CdTe fluorescence from a different CdTe layer
 26    One hit is left after merging CdTe fluorescence from Si layer
 27    One hit is left after merging scattered electron energy from Si layer to Si layer
 ------(  reconstruction by evaluating possible hit sequences)--------
 28    One hit sequence is permitted by F test (cosine of scattering angle valid)
 29    One hit sequence is permitted by G test (scattering angle consistent with energies)
 30    One hit sequence is permitted by probability threshold
 31    One hit sequence is left after tie-breaking based on figure of merit (FOM)
 32    not used
 ------(Reason for escape energy calculation, if one was done)--------
 33    Because all sequences initially ruled out by F test
 34    Because all sequences initially ruled out by G test
 35    Because all sequences initially ruled out by probability test
 ------(Whether escape energy was calculated)-------------------------
 36    Escape energy calculation was done
 37    not used
 38    not used
 39    not used

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-06  RSH     initial version, after cleaning code

*/
 
#define AHLABEL tool_sgdevtid
#define AHCVSID "$Id: sgdevtid.cxx,v 1.95 2016/04/18 20:08:27 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sgdevtidlib.h"
#include "sgdevtidcaldb.h"
#include "hxisgdevtid/hxisgdevtid.h"   // struct for reading input SFF
#include "hxisgdevtid/badpix.h"
#include "hxisgdevtid/fluor.h"
#include "hxisgdevtid/remap.h"

#include "ahapp/ahapp.h"
#include "ahgen/ahgen.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <string>
#include <cmath>
#include <iomanip>
#include <sstream>
#include <algorithm>     // for std::transform (to transform string to lowercase)

/** \addtogroup tool_sgdevtid
 *  @{
 */

/// \brief Get parameter values
/// \param[out] par                 // Par structure holding parameters
void getPar(sgdevtidlib::Par& par);

/// \brief open input file; create empty output file; load CALDB files
/// \param[in]  par                Structure holding parameters
/// \param[out] fpin               ahfits file pointer to infile
/// \param[out] fpout              ahfits file pointer to outfile
/// \param[out] fpextra            ahfits file pointer to outtracefile
/// \param[out] epithre_table      Threshold data from badpixfile
/// \param[out] activechan_table   Active channel data from badpixfile
/// \param[out] fluor_table        Data from fluorefile; event thresholds
/// \param[out] remap_table        Data from remapfile 
/// \param[out] geom               Critical distances and CAMERA coord errors
/// \param[out] probseq_table      Data from probseqfile
/// \param[out] probfov_table      Data from probfovfile
void initialize(sgdevtidlib::Par& par, ahfits::FilePtr& fpin,
                ahfits::FilePtr& fpout, ahfits::FilePtr& fpextra,
                hxisgdevtid::badpix::ThresholdTable & epithre_table,
                hxisgdevtid::badpix::ActiveChanTable & activechan_table,
                int& alive_asic, 
                hxisgdevtid::fluor::DataType & fluor_table,
                hxisgdevtid::remap::DataType & remap_table,
                hxisgdevtid::remap::GeomKeywords & geom,
                sgdprobseq::DataType & probseq_table,
                sgdprobfov::DataType & probfov_table);

/// \brief do the event reconstruction
/// \param[in] par                Structure holding parameters
/// \param[in] fpin               ahfits file pointer to infile
/// \param[in] fpout              ahfits file pointer to outfile
/// \param[in] fpextra            ahfits file pointer to outtracefile
/// \param[in] epithre_table      Data from badpixfile; event thresholds
/// \param[in] activechan_table   Active channel data from badpixfile
/// \param[in] fluor_table        Data from fluorefile; event thresholds
/// \param[in] remap_table        Data from remapfile 
/// \param[in] geom               Critical distances and CAMERA coord errors
/// \param[in] probseq_table      Data from probseqfile
/// \param[in] probfov_table      Data from probfovfile
void doWork(sgdevtidlib::Par &par, ahfits::FilePtr& fpin,
            ahfits::FilePtr& fpout, ahfits::FilePtr& fpextra,
            hxisgdevtid::badpix::ThresholdTable & epithre_table, 
            hxisgdevtid::badpix::ActiveChanTable & activechan_table,
            int& alive_asic,
            hxisgdevtid::fluor::DataType & fluor_table,
            hxisgdevtid::remap::DataType & remap_table,
            hxisgdevtid::remap::GeomKeywords & geom,
            sgdprobseq::DataType & probseq_table,
            sgdprobfov::DataType & probfov_table);

/// \brief close FITS files; clear allocated memory
/// \param[in]  par              Structure holding parameters
/// \param[in] fpin              ahfits file pointer to infile
/// \param[in] fpout             ahfits file pointer to outfile
/// \param[in] fpextra           ahfits file pointer to outtracefile
/// \param[in] epithre_table     Data from badpixfile; event thresholds
/// \param[in] activechan_table  Active channel data from badpixfile
/// \param[in] remap_table       Data from remapfile 
void finalize(sgdevtidlib::Par& par, ahfits::FilePtr fpin, 
              ahfits::FilePtr fpout, ahfits::FilePtr fpextra,
              hxisgdevtid::badpix::ThresholdTable & epithre_table,
              hxisgdevtid::badpix::ActiveChanTable & activechan_table,
              hxisgdevtid::remap::DataType & remap_table);

/// \brief create output file based on EVENTS extension of input file
/// \param[in] fpsrc ahfits file pointer with EVENTS extension to use as a template
/// \param[in] outfile name of output file
/// \param[out] fpdest ahfits::file pointer where to add new extension
void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      ahfits::FilePtr& fpdest);

/// \brief create extra output file with reconstruction intermediate steps
/// \param[in] fpsrc ahfits file pointer with EVENTS extension to use as a template
/// \param[in] outfile name of output file
/// \param[out] fpdest ahfits::file pointer where to add new extension
void createExtraOutputFile(ahfits::FilePtr fpsrc, const std::string& outtracefile,
                      ahfits::FilePtr& fpdest);

/// \brief initialize output row data copy values from input row
/// \param[in] inrow input row data
/// \param[in] outrow output row data
void prepareOutputData(hxisgdevtid::SFFRowData& inrow, sgdevtidlib::OutputRowData& outrow);

/// \brief initialize extra output row data and copy values from
///  input row
/// \param[in] inrow input row data
/// \param[in] outrow extra output row data
void prepareExtraOutputData(hxisgdevtid::SFFRowData& inrow, sgdevtidlib::ExtraOutputRowData& extraoutrow);

/// \brief return true if there are any non-zero bits in FLAG_HITPAT
/// \param[in] inrow input row data
/// \return true if FLAG_HITPAT has any non-zero bit
bool hasNonZeroHitPat(hxisgdevtid::SFFRowData& inrow);

/// \brief return true if there are any non-zero bits in FLAG_FASTBGO
/// \param[in] inrow input row data
/// \return true if FLAG_FASTBGO has any non-zero bit
bool hasNonZeroFastBGO(hxisgdevtid::SFFRowData& inrow);

/// \brief print the histogram of RECO_STATUS that is kept during the run
/// \param[in] outrow output row data
void prReco(sgdevtidlib::OutputRowData& outrow);

/// \brief print the chosen sequence
/// \param[in] signals  All the signals in the table row
/// \param[in] hits Hits reconstructed from signals
void prSeq(sgdevtidlib::RowSignals & signals, sgdevtidlib::Hits & hits);

// ****************************************************************************

/// \brief sgdevtid tool
///
/// Long description
int main(int argc, char** argv) {

  //  Structure to hold parameter values.
  //
  sgdevtidlib::Par par;

  //  FITS file pointers.
  //
  ahfits::FilePtr fpin=0;
  ahfits::FilePtr fpout=0;
  ahfits::FilePtr fpextra=0;

  //  Variables to store CALDB data.
  //
  hxisgdevtid::badpix::ThresholdTable epithre_table;
  hxisgdevtid::badpix::ActiveChanTable activechan_flags;
  hxisgdevtid::fluor::DataType fluor_table;
  hxisgdevtid::remap::DataType remap_table;
  hxisgdevtid::remap::GeomKeywords geom;
  sgdprobseq::DataType probseq_table;
  sgdprobfov::DataType probfov_table;
  int alive_asic = 0;

  int status = ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par, fpin, fpout, fpextra, epithre_table, activechan_flags,
                 alive_asic, fluor_table, remap_table, geom, probseq_table, probfov_table);
      doWork(par, fpin, fpout, fpextra, epithre_table, activechan_flags,
             alive_asic, fluor_table, remap_table, geom, probseq_table, probfov_table);
      finalize(par, fpin, fpout, fpextra, epithre_table, activechan_flags, remap_table);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fpin, fpout, fpextra, epithre_table, activechan_flags,
                   alive_asic, fluor_table, remap_table, geom, probseq_table, probfov_table);
        doWork(par, fpin, fpout, fpextra, epithre_table, activechan_flags,
               alive_asic, fluor_table, remap_table, geom, probseq_table, probfov_table);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(par, fpin, fpout, fpextra, epithre_table, activechan_flags, remap_table);
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
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

void getPar(sgdevtidlib::Par& par) {
  par.infile=ahapp::getParString("infile");
  par.outfile=ahapp::getParString("outfile");
  par.occurrenceid=ahapp::getParInt("occurrenceid");
  par.remapfile=ahapp::getParString("remapfile");
  par.fluorefile=ahapp::getParString("fluorefile");
  par.badpixfile=ahapp::getParString("badpixfile");
  par.probseqfile=ahapp::getParString("probseqfile");
  par.probfovfile=ahapp::getParString("probfovfile");
  par.rejectbgo=ahapp::getParBool("rejectbgo");
  par.skipreco=ahapp::getParBool("skipreco");
  par.strangepix=ahapp::getParInt("strangepix");
  par.numsignal=ahapp::getParInt("numsignal");
  par.datamode=ahapp::getParString("datamode");
  par.d10_as_read=ahapp::getParDouble("d10");
  par.d1a1a_as_read=ahapp::getParDouble("d1a1a");
  par.d1a1b_as_read=ahapp::getParDouble("d1a1b");
  par.d1a2_as_read=ahapp::getParDouble("d1a2");
  par.d1a3_as_read=ahapp::getParDouble("d1a3");
  par.d10=par.d10_as_read;
  par.d1a1a=par.d1a1a_as_read;
  par.d1a1b=par.d1a1b_as_read;
  par.d1a2=par.d1a2_as_read;
  par.d1a3=par.d1a3_as_read;
  par.a=ahapp::getParDouble("a");
  par.b=ahapp::getParDouble("b");
  par.probaccept2=ahapp::getParDouble("probaccept2");
  par.probaccept3=ahapp::getParDouble("probaccept3");
  par.probaccept4=ahapp::getParDouble("probaccept4");
  par.distz=ahapp::getParDouble("distz");
  par.paraoffset0=ahapp::getParDouble("paraoffset0");
  par.paraoffset1=ahapp::getParDouble("paraoffset1");
  par.paraoffset2=ahapp::getParDouble("paraoffset2");
  par.weight0=ahapp::getParDouble("weight0");
  par.weight1=ahapp::getParDouble("weight1");
  par.weight2=ahapp::getParDouble("weight2");
  par.weight3=ahapp::getParDouble("weight3");
  par.outtracefile=ahapp::getParString("outtracefile");
  par.seed=ahapp::getParInt("seed");

  // do not know how to handle 'strange' signals yet
  if (par.strangepix == 2)
    AH_THROW_RUNTIME("do not know how to handle 'strange' signals yet; set strangepix parameter to 0 (bad) or 1 (good)");

  // Check output trace file parameter.
  if (par.outtracefile.find_first_not_of(' ') == std::string::npos) {
    AH_THROW_RUNTIME("outtracefile cannot be blank or null: use NONE or give filename");
  }
  par.extrainfo = ahgen::strtoupper(par.outtracefile) != "NONE";

//  if (par.paraoffset0 == 0. || par.paraoffset1 == 0. || par.paraoffset2 == 0.)
//    AH_THROW_RUNTIME("paraoffset parameters cannot be zero");

  std::string tdelgmethod=ahapp::getParString("delgmethod");
  std::transform(tdelgmethod.begin(), tdelgmethod.end(), tdelgmethod.begin(), ::tolower);
  if (tdelgmethod == "analytic")
    par.delgmethod=sgdevtidlib::e_ANALYTIC;
  else if(tdelgmethod == "corner")
    par.delgmethod=sgdevtidlib::e_CORNER;
  else
    AH_THROW_RUNTIME("invalid value of delgmethod parameter; must be 'analytic' or 'corner'");

}

// ****************************************************************************

void initialize(sgdevtidlib::Par& par, ahfits::FilePtr& fpin,
                ahfits::FilePtr& fpout, ahfits::FilePtr& fpextra,
                hxisgdevtid::badpix::ThresholdTable & epithre_table,
                hxisgdevtid::badpix::ActiveChanTable & activechan_table,
                int& alive_asic, 
                hxisgdevtid::fluor::DataType & fluor_table,
                hxisgdevtid::remap::DataType & remap_table,
                hxisgdevtid::remap::GeomKeywords & geom,
                sgdprobseq::DataType & probseq_table,
                sgdprobfov::DataType & probfov_table) {

  // If storeprobfov=true, then store all FOV probabilities in memory
  // +++ 2015-01-22 MCW this is currently for testing only, if we want to keep
  // +++ 2015-01-22 MCW this option in the future, a parameter or size test
  // +++ 2015-01-22 MCW should be performed.
  bool storeprobfov=false;


  //  Open input file; check INSTRUME/DETNAM.
  //
  ahfits::open(par.infile,"",&fpin);
  if (ahfits::isPrimary(fpin)) ahfits::move(fpin,"EVENTS");   // move to EVENTS extension if extended syntax not provided
  std::string instrume=ahfits::getKeyValStr(fpin,"INSTRUME");
  std::string instrume_shrt=instrume.substr(0,3);
  std::string detnam=ahfits::getKeyValStr(fpin,"DETNAM");
  std::string dateobs=ahfits::getKeyValStr(fpin,"DATE-OBS");
  AH_INFO(ahlog::HIGH) << "Input file:  " << par.infile << std::endl;
  if (ahgen::strtoupper(instrume) != "SGD1" && ahgen::strtoupper(instrume) != "SGD2")
    AH_THROW_RUNTIME("input file EVENTS HDU has wrong INSTRUME value; should be SGD1 or SGD2");
  if (ahgen::strtoupper(detnam) != "CC1" && ahgen::strtoupper(detnam) != "CC2" && ahgen::strtoupper(detnam) != "CC3")
    AH_THROW_RUNTIME("input file EVENTS HDU has wrong DETNAM value; should be CC1, CC2, or CC3");

  //  Read DATAMODE keyword, apply user override if necessary,
  //    and extract portion relevant to CALDB access.
  //
  std::string tdatamode = "";         // Temporary strings for parsing DATAMODE
  std::string ev_datamode = ahfits::getKeyValStr(fpin,"DATAMODE");
  std::string dmtype = "";
  std::string normaln = "";
  std::size_t i = 0;                  // Location of underscore in DATAMODE
  AH_INFO(ahlog::HIGH) << "INSTRUME:  " << instrume << "  DETNAM:  " << detnam << std::endl;
  if (!par.datamode.empty() && ahgen::strtoupper(par.datamode) !=  "NONE") {
    tdatamode = par.datamode;
    AH_INFO(ahlog::HIGH) << "DATAMODE in event file=" << ev_datamode << "; overridden with parameter: " << tdatamode << std::endl;
  } else {
    tdatamode = ev_datamode;
    AH_INFO(ahlog::HIGH) << "DATAMODE taken from event file:  " << ev_datamode << std::endl;
  }
  i = tdatamode.rfind("_");           // locate underscore character
  if (i == std::string::npos) 
    AH_THROW_RUNTIME("expecting underscore character in input DATAMODE, e.g. CAMERA_NORMAL1");
  // Substring starts one character past the underscore.
  dmtype = tdatamode.substr(0, i);
  normaln = tdatamode.substr(++i, 7);

  // Note:  Deleted constraint that datamode must be NORMALn
  // if (normaln.substr(0, 6) != "NORMAL") {
  //   AH_THROW_RUNTIME("effective DATAMODE value (piece after underscore): "+tdatamode);
  // }

  //  Load data from remapfile.
  // Note: the 2nd argument of load() ("SGD") checks that the given remap file 
  //       has INSTRUME=SGD
  //
  std::string actual_remap = ahmission::caldb::resolve(par.remapfile, "remapping", instrume_shrt, "-", "REMAPPING", dateobs);
  ape_trad_set_string("remapfile",actual_remap.c_str());   // to record actual file path in history
  hxisgdevtid::remap::load(actual_remap, "SGD", remap_table);
  hxisgdevtid::remap::loadGeomSGD(actual_remap, "SGD", geom);

  //  Revise critical distance values.
  //
  if (par.d10 < geom.m_distan01) {
    par.d10 = geom.m_distan01;
    AH_INFO(ahlog::HIGH) << "Effective value of d10 parameter changed to " << par.d10 << 
    " to match minimum given by DISTAN01 in remapping CALDB file." << std::endl;
  }
  if (par.d1a1b < geom.m_distan02) {
    par.d1a1a = geom.m_distan02;
    AH_INFO(ahlog::HIGH) << "Effective value of d1a1a parameter changed to " << par.d1a1a << 
    " to match minimum given by DISTAN02 in remapping CALDB file." << std::endl;
  }
  if (par.d1a1b < geom.m_distan03) {
    par.d1a1b = geom.m_distan03;
    AH_INFO(ahlog::HIGH) << "Effective value of d1a1b parameter changed to " << par.d1a1b << 
    " to match minimum given by DISTAN03 in remapping CALDB file." << std::endl;
  }
  if (par.d1a2 < geom.m_distan04) {
    par.d1a2 = geom.m_distan04;
    AH_INFO(ahlog::HIGH) << "Effective value of d1a2 parameter changed to " << par.d1a2 << 
    " to match minimum given by DISTAN04 in remapping CALDB file." << std::endl;
  }
  if (par.d1a3 < geom.m_distan05) {
    par.d1a3 = geom.m_distan05;
    AH_INFO(ahlog::HIGH) << "Effective value of d1a3 parameter changed to " << par.d1a3 << 
    " to match minimum given by DISTAN05 in remapping CALDB file." << std::endl;
  }

  //  Load data from fluorefile.
  //  Load data from file.
  // Note: the 2nd argument of load() ("SGD") checks that the given fluorescence file 
  //       has INSTRUME=SGD
  //
  std::string actual_fluor=ahmission::caldb::resolve(par.fluorefile, "fluorescence", instrume_shrt, "-", "LINE_ENERGY", dateobs);
  ape_trad_set_string("fluorefile",actual_fluor.c_str());   // to record actual file path in history
  hxisgdevtid::fluor::load(actual_fluor,"SGD",fluor_table);

  //  Load data from badpixelfile.
  // Note: the 2nd argument of load() (instrume) checks that the given bad pixel 
  //       file has INSTRUME=SGD
  // Note: the 3rd argument of load() (detnam) locates the correct extension
  //       in the badpix file via DETNAM
  //
  std::string actual_badpix=ahmission::caldb::resolve(par.badpixfile, "bad pixel", instrume, detnam, "BADPIX", dateobs);
  ape_trad_set_string("badpixfile",actual_badpix.c_str());   // to record actual file path in history
  hxisgdevtid::badpix::loadThreshold(actual_badpix,instrume,detnam,normaln,
                                     epithre_table,alive_asic);
  hxisgdevtid::badpix::loadActiveChan(actual_badpix, instrume, detnam,
                                      normaln, activechan_table);

  //  Load data from probseqfile.
  //
  std::string actual_probseq=ahmission::caldb::resolve(par.probseqfile, "sequence probability", instrume_shrt, "-", "PROB_SEQUENCE", dateobs);
  ape_trad_set_string("probseqfile",actual_probseq.c_str());   // to record actual file path in history
  sgdprobseq::load(actual_probseq,probseq_table);

  //  Load data from probfovfile.
  //
  // Note: since probabilities are read in a random-access manner, we will
  // disable buffering for this file only.
  std::string actual_probfov=ahmission::caldb::resolve(par.probfovfile, "FOV probability", instrume_shrt, "-", "LIKELIHOOD", dateobs);
  ape_trad_set_string("probfovfile",actual_probfov.c_str());   // to record actual file path in history
  int bufferval=ahfits::getBuffer();
  ahfits::setBuffer(0);
  sgdprobfov::load(actual_probfov,storeprobfov,probfov_table);
  ahfits::setBuffer(bufferval);
//  sgdprobfov::enableInterpolation("ENE_TOTAL",probfov_table);
//  sgdprobfov::enableInterpolation("COMPTON_TH",probfov_table);

  // create output file
  createOutputFile(fpin,par.outfile,fpout);

  // create extra output file
  if (par.extrainfo) createExtraOutputFile(fpin,par.outtracefile,fpextra);

  // seed random number generator
  ahgen::seedRandom(par.seed);

  // Write list of parameters to log file
  //
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(sgdevtidlib::Par &par, ahfits::FilePtr& fpin,
            ahfits::FilePtr& fpout, ahfits::FilePtr& fpextra,
            hxisgdevtid::badpix::ThresholdTable & epithre_table, 
            hxisgdevtid::badpix::ActiveChanTable & activechan_table,
            int& alive_asic,
            hxisgdevtid::fluor::DataType & fluor_table,
            hxisgdevtid::remap::DataType & remap_table,
            hxisgdevtid::remap::GeomKeywords & geom,
            sgdprobseq::DataType & probseq_table,
            sgdprobfov::DataType & probfov_table) {


  // Reconstruction summary counters.
  long long num_rows_no_reco_attempt = 0;
  long long num_rows_reco_trivial = 0;
  long long num_rows_reco_nontrivial = 0;

  // store row data from input and output files
  hxisgdevtid::SFFRowData inrow("SGD");
  sgdevtidlib::OutputRowData outrow;
  sgdevtidlib::ExtraOutputRowData extraoutrow;

  sgdevtidlib::RowSignals signals;              // All signals from current row
  sgdevtidlib::IntVectorIndex sigLayer;         // Signals sorted by LAYER_ID
  //sgdevtidlib::IntVectorIndex sigSensor;      // Signals sorted by SENSOR_ID

  sgdevtidlib::SeqInfo sequence_type;           // Description of hit sequence
  bool escape_flag = false;                     //  True if escape energy was used in reconstruction
  sgdevtidlib::Hits hits;                       //  Tracks hits: contains hitarray, epiarray, ee, delta_ee

  // escape energies, calculated during F & G tests are needed by the tie-breaking routine
  double escape_en[sgdevtidlib::MAX_PERM];
  for (int i=0; i < sgdevtidlib::MAX_PERM; i++) escape_en[i]=0.;

  //  State codes indicating what to do next.
  //
  bool write_row = false;
 
  //  Test results.
  //
  double g[sgdevtidlib::MAX_PERM][sgdevtidlib::MAX_NUMHITS];    // calculated in performFGProbTests; needed in performTieBreaking
  bool test_f[sgdevtidlib::MAX_PERM];
  bool test_g[sgdevtidlib::MAX_PERM];
  for (int i=0; i < sgdevtidlib::MAX_PERM; i++) {
    for (int j=0; j < sgdevtidlib::MAX_NUMHITS; j++) g[i][j]=0.;
    test_f[i]=false;
    test_g[i]=false;
  }

  //  Sequence probabilities
  //
  double prob[sgdevtidlib::MAX_PERM];   // computed in performFGProbTests, used in performTieBreaking
  for (int i=0; i < sgdevtidlib::MAX_PERM; i++) prob[i]=0.;

  //  Routers.
  //
  ahfits::Router routin(fpin);
  ahfits::Router routout(fpout);
  ahfits::Router* routextraout=0;

  int numhit = 0;     //  Number of hits after merging

  //  Dimension from table header.
  //
  ahfits::IndexType naxis2 = ahfits::getKeyValLLong(fpin, "NAXIS2");

  // set up connections between local variables and input SFF columns
  //
  AH_DEBUG << "Connecting input columns" << std::endl;
  routin.connectScalar(ahfits::e_READONLY,"TIME",inrow.m_time, &inrow.m_time_null);
  routin.connectScalar(ahfits::e_READONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routin.connectScalar(ahfits::e_READONLY,"CATEGORY",inrow.m_category);
  routin.connectScalar(ahfits::e_READONLY,"LIVETIME",inrow.m_livetime);
  routin.connectScalar(ahfits::e_READONLY,"NUM_ASIC",inrow.m_num_asic);
  routin.connectBit(ahfits::e_READONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routin.connectBit(ahfits::e_READONLY,"STATUS",inrow.m_status,inrow.num_status);
  routin.connectBit(ahfits::e_READONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routin.connectBit(ahfits::e_READONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routin.connectBit(ahfits::e_READONLY,"FLAG_CALMODE",inrow.m_flag_calmode,inrow.num_flag_calmode);
  routin.connectScalar(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig[0]);
  routin.connectBit(ahfits::e_READONLY,"FLAG_LCHKMIO",inrow.m_flag_lchkmio,inrow.num_flag_lchkmio);
  routin.connectBit(ahfits::e_READONLY,"FLAG_CCBUSY",inrow.m_flag_ccbusy,inrow.num_flag_ccbusy);
  routin.connectBit(ahfits::e_READONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT_CC",inrow.m_flag_hitpat_cc,inrow.num_flag_hitpat_cc);
  routin.connectBit(ahfits::e_READONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",inrow.m_readout_id_rmap,inrow.num_readout_id_rmap);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"PHA",inrow.m_pha,inrow.num_pha);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"EPI",inrow.m_epi,inrow.num_epi,inrow.m_epi_null);

  // set up connections between local variables and output SFFa columns
  //
  AH_DEBUG << "Connecting main output columns" << std::endl;
  routout.connectScalar(ahfits::e_WRITEONLY,"TIME",inrow.m_time, &inrow.m_time_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routout.connectScalar(ahfits::e_WRITEONLY,"CATEGORY",inrow.m_category);
  routout.connectScalar(ahfits::e_WRITEONLY,"LIVETIME",inrow.m_livetime);
  routout.connectBit(ahfits::e_READONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routout.connectBit(ahfits::e_WRITEONLY,"STATUS",inrow.m_status,inrow.num_status);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CALMODE",inrow.m_flag_calmode,inrow.num_flag_calmode);
  routout.connectScalar(ahfits::e_WRITEONLY,"FLAG_TRIG",inrow.m_flag_trig[0]);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHKMIO",inrow.m_flag_lchkmio,inrow.num_flag_lchkmio);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CCBUSY",inrow.m_flag_ccbusy,inrow.num_flag_ccbusy);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT_CC",inrow.m_flag_hitpat_cc,inrow.num_flag_hitpat_cc);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routout.connectScalar(ahfits::e_WRITEONLY, "PI", outrow.m_pi, &outrow.m_pi_null);
  routout.connectBit(ahfits::e_WRITEONLY,"RECO_STATUS",outrow.m_reco_status,outrow.num_reco_status);
  routout.connectScalar(ahfits::e_WRITEONLY,"MATTYPE",outrow.m_mattype, &outrow.m_mattype_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "ENE_TOTAL", outrow.m_ene_total, &outrow.m_ene_total_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "NUMSIGNAL", outrow.m_numsignal);
  routout.connectBit(ahfits::e_WRITEONLY, "NUMHITS", outrow.m_numhits, outrow.num_numhits);
  routout.connectScalar(ahfits::e_WRITEONLY, "SEQ_HITS", outrow.m_seq_hits,&outrow.m_seq_hits_null);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY, "DELCOMPTON", outrow.m_delcompton);
  routout.connectScalar(ahfits::e_WRITEONLY, "COMPTON_TH", outrow.m_compton_th, &outrow.m_compton_th_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "COMPTON_PH", outrow.m_compton_ph, &outrow.m_compton_ph_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "DISTANCE0", outrow.m_distance0, &outrow.m_distance0_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "OFFAXIS", outrow.m_offaxis, &outrow.m_offaxis_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"CAMERAX",outrow.m_camerax,&outrow.m_camerax_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"CAMERAY",outrow.m_cameray,&outrow.m_cameray_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"CAMERAZ",outrow.m_cameraz,&outrow.m_cameraz_null);
  routout.connectScalar(ahfits::e_WRITEONLY, "LIKELIHOOD",outrow.m_probability,&outrow.m_probability_null);

  // set up connections between local variables and output SFFa columns
  //
  AH_DEBUG << "Connecting extra output columns" << std::endl;
  if (par.extrainfo) {
    routextraout = new ahfits::Router(fpextra);
    routextraout->connectScalar(ahfits::e_WRITEONLY, "TIME", inrow.m_time, &inrow.m_time_null);
    routextraout->connectScalar(ahfits::e_WRITEONLY, "OCCURRENCE_ID", inrow.m_occurrence_id);
    routextraout->connectScalar(ahfits::e_WRITEONLY, "NUMSIGNAL", extraoutrow.m_numsignal);
    routextraout->connectScalar(ahfits::e_WRITEONLY, "M", extraoutrow.m_m);
    routextraout->connectScalar(ahfits::e_WRITEONLY, "NUMPERM", extraoutrow.m_numperm);
    routextraout->connectBit(ahfits::e_WRITEONLY, "ESCAPE_FLAG", &extraoutrow.m_escape_flag, extraoutrow.num_escape_flag);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY0", extraoutrow.m_sigarray0, extraoutrow.num_sigarray0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY1_0", extraoutrow.m_sigarray1_0, extraoutrow.num_sigarray1_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY1A_1A", extraoutrow.m_sigarray1a_1a, extraoutrow.num_sigarray1a_1a);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY1A_1B", extraoutrow.m_sigarray1a_1b, extraoutrow.num_sigarray1a_1b);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY1A_2", extraoutrow.m_sigarray1a_2, extraoutrow.num_sigarray1a_2);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "SIGARRAY1A_3", extraoutrow.m_sigarray1a_3, extraoutrow.num_sigarray1a_3);
    routextraout->connectFixedLengthArray(ahfits::e_WRITEONLY, "HITARRAY", extraoutrow.m_hitarray, extraoutrow.m_hitarray_null);
    routextraout->connectFixedLengthArray(ahfits::e_WRITEONLY, "EPIARRAY", extraoutrow.m_epiarray, extraoutrow.m_epiarray_null);
    routextraout->connectFixedLengthArray(ahfits::e_WRITEONLY, "DELTAEPIARRAY", extraoutrow.m_deltaepiarray, extraoutrow.m_deltaepiarray_null);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "E", extraoutrow.m_e, extraoutrow.num_e);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "DELTA_E", extraoutrow.m_delta_e, extraoutrow.num_delta_e);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "F", extraoutrow.m_f, extraoutrow.num_f);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "DELTA_F", extraoutrow.m_delta_f, extraoutrow.num_delta_f);
    routextraout->connectBit(ahfits::e_WRITEONLY, "CHECK_F", extraoutrow.m_check_f, extraoutrow.num_check_f);
    routextraout->connectBit(ahfits::e_WRITEONLY, "TEST_F", extraoutrow.m_test_f, extraoutrow.num_test_f);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "C_THETA_G", extraoutrow.m_cos_theta_g, extraoutrow.num_cos_theta_g);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "D_C_THETA_G", extraoutrow.m_delta_cos_theta_g, extraoutrow.num_delta_cos_theta_g);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "C_THETA_K", extraoutrow.m_cos_theta_k, extraoutrow.num_cos_theta_k);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "D_C_THETA_K", extraoutrow.m_delta_cos_theta_k, extraoutrow.num_delta_cos_theta_k);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "G", extraoutrow.m_g, extraoutrow.num_g);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "DELTA_G", extraoutrow.m_delta_g, extraoutrow.num_delta_g);
    routextraout->connectBit(ahfits::e_WRITEONLY, "CHECK_G", extraoutrow.m_check_g, extraoutrow.num_check_g);
    routextraout->connectBit(ahfits::e_WRITEONLY, "TEST_G", extraoutrow.m_test_g, extraoutrow.num_test_g);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "PROB", extraoutrow.m_prob, extraoutrow.num_prob);
    routextraout->connectBit(ahfits::e_WRITEONLY, "TEST_PROB", extraoutrow.m_test_prob, extraoutrow.num_test_prob);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "FOM", extraoutrow.m_fom, extraoutrow.num_fom);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "C_THETA_G_0", extraoutrow.m_cos_theta_g_0, extraoutrow.num_cos_theta_g_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "D_C_THETA_G_0", extraoutrow.m_delta_cos_theta_g_0, extraoutrow.num_delta_cos_theta_g_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "C_THETA_K_0", extraoutrow.m_cos_theta_k_0, extraoutrow.num_cos_theta_k_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "D_C_THETA_K_0", extraoutrow.m_delta_cos_theta_k_0, extraoutrow.num_delta_cos_theta_k_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "G_0", extraoutrow.m_g_0, extraoutrow.num_g_0);
    routextraout->connectVariableLengthArray(ahfits::e_WRITEONLY, "DELTA_G_0", extraoutrow.m_delta_g_0, extraoutrow.num_delta_g_0);

    routextraout->connectScalar(ahfits::e_WRITEONLY, "CLSTRSHAPE", extraoutrow.m_clstrshape);
    routextraout->connectBit(ahfits::e_WRITEONLY, "MERGE1_0", &extraoutrow.m_merge1_0, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "MERGE1A_1A", &extraoutrow.m_merge1a_1a, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "MERGE1A_1B", &extraoutrow.m_merge1a_1b, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "MERGE1A_2", &extraoutrow.m_merge1a_2, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "MERGE1A_3", &extraoutrow.m_merge1a_3, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "RAND1_0", &extraoutrow.m_rand1_0, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "RAND1A_3", &extraoutrow.m_rand1a_3, extraoutrow.num_merge_flag);
    routextraout->connectBit(ahfits::e_WRITEONLY, "RAND_FOM", &extraoutrow.m_rand_fom, extraoutrow.num_merge_flag);
  }

  //  Row counters.
  //
  inrow.m_row_count = 0;
  outrow.m_row_count = 0;
  extraoutrow.m_row_count = 0;
 
  // event row loop
  ahfits::firstRow(fpout);
  write_row = false;
  sgdevtidlib::clearRecoStatusHist(outrow);  // Histogram of RECO_STATUS bits is kept
  //
  AH_DEBUG << "Starting event loop" << std::endl;
  AH_INFO(ahlog::HIGH) << "Processing input BINTABLE of " << naxis2 << " rows." << std::endl;
  //
  for (ahfits::firstRow(fpin); ahfits::readOK(fpin); ahfits::nextRow(fpin)) {
    //
    //AH_DEBUG << "Head of event loop, inside loop" << std::endl;
    //
 
    // Status indicator that controls early exit from loop
    //
    bool reco_complete = false;

    int n_good_sigrow = 0;                    //  Number of good signals in row
    int n_bad_sigrow = 0;                     //  Number of bad signals in row
    int maxsignal = 0;                        //  Maximum number of signals in valid row
    int nsignal = 0;                          //  Total number of signals in row
//    int best_k = -1;                        //  Subscript for permutation with best theta_K; < 0 => no selection
    long long rownumber=0;                    //  current row number
    int datamode_occ=hxisgdevtid::e_NOTSURE;  //  Datamode of one occurrence

    //  This is positioned at the head of the row loop in order to be reached
    //  by C++ continue statements (which skip to the very end of the loop).
    //
    //  The possibility of skipping the write is maintained because for 
    //  some non-science data, each signal is written out as a separate event,
    //  in an interior loop.  In that case, it is wrong to have a duplicate
    //  write here.
    //
    if (write_row) {

      // print sequence information if occurrenceid set
      if (par.occurrenceid > 0 && hits.m_m > 1) prSeq(signals, hits);

      //  Build the part of the primary output row that applies
      //  to all occurrences.
      //
      outrow.m_numsignal = signals.m_nsignal;
      extraoutrow.m_numsignal = signals.m_nsignal;

      // fill SEQ_HITS if PI was assigned
      if (outrow.m_pi_null == 0) {
        outrow.m_seq_hits_null=0;
        outrow.m_seq_hits = sgdprobseq::lookupMechanism(sequence_type.m_sequence,
                                                        escape_flag,probseq_table);
      }

      AH_DEBUG << "outrow.m_pi:            " << outrow.m_pi << " (NULL: " << (int)outrow.m_pi_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_compton_th:    " << outrow.m_compton_th << " (NULL: " << (int)outrow.m_compton_th_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_compton_ph:    " << outrow.m_compton_ph << " (NULL: " << (int)outrow.m_compton_ph_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_distance0:     " << outrow.m_distance0 << " (NULL: " << (int)outrow.m_distance0_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_offaxis:       " << outrow.m_offaxis << " (NULL: " << (int)outrow.m_offaxis_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_camerax:       " << outrow.m_camerax << " (NULL: " << (int)outrow.m_camerax_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_cameray:       " << outrow.m_cameray << " (NULL: " << (int)outrow.m_cameray_null << ")" << std::endl;
      AH_DEBUG << "outrow.m_cameraz:       " << outrow.m_cameraz << " (NULL: " << (int)outrow.m_cameraz_null << ")" << std::endl;

      if (outrow.m_ene_total_null == 1)
        AH_DEBUG << "--EVENT_EPI: NULL" << std::endl;
      else
        AH_DEBUG << "--EVENT_EPI: " << outrow.m_ene_total << std::endl;
      if (outrow.m_pi_null == 1)
        AH_DEBUG << "--EVENT_PI: NULL" << std::endl;
      else
        AH_DEBUG << "--EVENT_PI: " << outrow.m_pi << std::endl;

      AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      ++outrow.m_row_count;
      if (outrow.m_pi_null == 1) outrow.m_count_pi_null++;
      if (outrow.m_row_count < inrow.m_row_count) {
        std::stringstream ss;
        ss.str("");
        ss << "Fewer output than input rows:  inrow.m_row_count = " << inrow.m_row_count << "; outrow.m_row_count = " << outrow.m_row_count;
        AH_THROW_LOGIC(ss.str());
      }

      if (par.extrainfo) {
        AH_DEBUG << "Writing extra output row " << extraoutrow.m_row_count+1 << std::endl;
        ahfits::writeRow(fpextra);
        ahfits::nextRow(fpextra);
        ++extraoutrow.m_row_count;
        if (extraoutrow.m_row_count < inrow.m_row_count) {
          std::stringstream ss;
          ss.str("");
          ss << "Fewer extraoutput than input rows:  inrow.m_row_count = " << inrow.m_row_count << "; extraoutrow.m_row_count = " << extraoutrow.m_row_count;
          AH_THROW_LOGIC(ss.str());
        }
      }
      write_row = false;   //  Reset
    }

    if (par.occurrenceid > 0 && outrow.m_row_count > 0) break;
    ahfits::readRow(fpin);
    if (par.occurrenceid > 0 && inrow.m_occurrence_id != par.occurrenceid) {
      write_row = false;
      continue;
    }
    ++inrow.m_row_count;
    rownumber=ahfits::currentRow(fpin);
    nsignal=inrow.num_readout_id_rmap;

    if (naxis2 > 25000 && (rownumber % 25000) == 1) {
      AH_OUT << "Processing row " << rownumber << "/" << naxis2 << " of input SFF" << std::endl;
    }

    AH_DEBUG << "--PROCESS_ROW: " << rownumber << std::endl;
    AH_DEBUG << "--OCCURRENCE_ID: " << inrow.m_occurrence_id << std::endl;
    {
      std::stringstream msg;
      msg << "--TIME: " << std::setprecision(15) << inrow.m_time;
      AH_DEBUG << msg.str() << std::endl;
    }

    // initializations for next row
    escape_flag=false;
    sgdevtidlib::reInitHits(hits);
    sgdevtidlib::reInitSignals(signals);
    sgdevtidlib::clearRecoStatus(outrow);   

    //  Copy row data from input to output (reset other column values).
    //
    prepareOutputData(inrow,outrow);
    if (par.extrainfo) prepareExtraOutputData(inrow, extraoutrow);

    // Determine whether this occurrence is CC, CALMODE, READALL, or PSEUDO
    // (occurrence-by-occurrence datamode, which is distinct from DATAMODE keyword).
    // Note: last argument is only needed for HXI
    datamode_occ=hxisgdevtid::getOccurrenceMode("SGD",inrow,alive_asic,0);

    //  Set RECO_STATUS bits based on datamode of occurrence.
    //
    switch (datamode_occ) {
      case hxisgdevtid::e_CAMERA:  break;
      case hxisgdevtid::e_PSEUDO:  sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_PSEUDO); break;
      case hxisgdevtid::e_CALMODE:  sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_CALMODE); break;
      case hxisgdevtid::e_READALL:  sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_READALL); break;
      case hxisgdevtid::e_NOTSURE:  sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_NOTSURE); break;
    }

    //  Check for bad PROC_STATUS.
    //
    if (!procstatus::processRow(inrow.m_proc_status)) {
      AH_INFO(ahlog::LOW) << "Finished: Bad PROC_STATUS in row " << rownumber << " (RECO_BAD_PROC_STATUS)" << std::endl;
      AH_INFO(ahlog::LOW) << "Writing output row " << outrow.m_row_count+1 << std::endl;
      sgdevtidlib::setRecoStatusBit(outrow,sgdevtidlib::RECO_BAD_PROC_STATUS);
      sgdevtidlib::setOutputEventNull(outrow);    // set PI and other SFFa output columns to NULL
      ++num_rows_no_reco_attempt;
      write_row = true; 
      continue;   // write and go to next row in SFF
    }

    // Check if no signals in occurrence
    if (nsignal == 0) {
      AH_DEBUG << "Finished: no signals in row " << rownumber << " (RECO_NO_SIGNALS)" << std::endl;
      AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
      sgdevtidlib::setRecoStatusBit(outrow,sgdevtidlib::RECO_NO_SIGNALS);
      sgdevtidlib::setOutputEventNull(outrow);    // set PI and other SFFa output columns to NULL
      ++num_rows_no_reco_attempt;
      write_row = true; 
      continue;   // write and go to next row in SFF
    }

    //  Check BGO.  If rejecting, set output columns to NULL.
    //
    if (par.rejectbgo) {
      if (hasNonZeroHitPat(inrow) || hasNonZeroFastBGO(inrow)) {
        AH_DEBUG << "Finished: have non-zero HITPAT or FASTBGO in row number " << rownumber 
          << " (RECO_FASTBGO_HITPAT)" << std::endl;
        AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
        sgdevtidlib::setRecoStatusBit(outrow,sgdevtidlib::RECO_FASTBGO_HITPAT);
        sgdevtidlib::setOutputEventNull(outrow);     // set PI and other SFFa output columns to NULL
        ++num_rows_no_reco_attempt;
        write_row = true;
        continue;   // write and go to next row in SFF
      }
    }

    //  Skip READALL and CALMODE occurrences if skipreco == true.
    //
    if (par.skipreco) {
      if (datamode_occ == hxisgdevtid::e_READALL || datamode_occ == hxisgdevtid::e_CALMODE) {
        AH_DEBUG << "Finished: READALL or CALMODE in row number " << rownumber 
          << " (RECO_SKIP_RECO)" << std::endl;
        AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
        sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_SKIP_RECO);
        sgdevtidlib::setOutputEventNull(outrow);    // set PI and other SFFa output columns to NULL
        ++num_rows_no_reco_attempt;
        write_row = true;
        continue;   // write and go to next row in SFF
      }
    }

    if (par.numsignal > 0) {
      maxsignal = par.numsignal;
    } else {
      maxsignal = sgdevtidlib::MAX_NUMSIGNAL;
    }

    if (nsignal > maxsignal) {
      AH_DEBUG << "Finished: Too many signals in row " << rownumber << " (RECO_TOO_MANY_SIGNALS)" << std::endl;
      AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
      sgdevtidlib::setRecoStatusBit(outrow,sgdevtidlib::RECO_TOO_MANY_SIGNALS);
      sgdevtidlib::setOutputEventNull(outrow);
      ++num_rows_no_reco_attempt;
      signals.m_nsignal = nsignal;
      write_row = true;
      continue;   // write and go to next row in SFF
    }

    AH_DEBUG << nsignal << " signals in row " << rownumber << std::endl;
    AH_DEBUG << "          signal    readout_id_rmap    rawx    rawy    sensor_id     asic     readout         epi" << std::endl;
    for (int i=0; i < nsignal; ++i) {
      int ri=inrow.m_readout_id_rmap[i];
      std::stringstream msg;
      msg << "--SIGNAL: "
          << std::setw(4) << i
          << std::setw(16) << ri
          << std::setw(13) << hxisgdevtid::remap::getRAWX(remap_table,ri)
          << std::setw(8) << hxisgdevtid::remap::getRAWY(remap_table,ri)
          << std::setw(10) << hxisgdevtid::remap::getSENSOR_ID(remap_table,ri)
          << std::setw(12) << hxisgdevtid::remap::getASIC_ID(remap_table,ri)
          << std::setw(12) << hxisgdevtid::remap::getREADOUT_ID(remap_table,ri);
      if (inrow.m_epi_null[i] == 0)
        msg << std::setw(12) << inrow.m_epi[i];
      else
        msg << std::setw(12) << "NULL";
      msg << std::endl;
      AH_DEBUG << msg.str();
    }

    //  At this point, the number of signals < maxsignal (default 48).
    //
    //  Store the signal information in arrays for use in the functions.
    //  These arrays are grouped in a structure but otherwise are simple 1-D
    //  native C++ arrays.
    //
    for (int i=0; i < nsignal; ++i) {

      //
      AH_DEBUG << "Processing signal " << i << " in row " << rownumber << std::endl;
      //

      int ri = inrow.m_readout_id_rmap[i];
      double epi_i = inrow.m_epi[i];
      bool epi_i_null = inrow.m_epi_null[i];
      bool sig_okay=true;                     // true if signal should be used

      signals.m_readout_id_rmap[i] = ri;
      signals.m_epi[i] = epi_i;
      signals.m_epi_current[i] = epi_i;

      signals.m_rawx[i] = hxisgdevtid::remap::getRAWX(remap_table,ri);
      signals.m_rawy[i] = hxisgdevtid::remap::getRAWY(remap_table,ri);
      signals.m_sensor_id[i] = hxisgdevtid::remap::getSENSOR_ID(remap_table,ri);
      signals.m_camerax[i] = hxisgdevtid::remap::getCAMERAX(remap_table,ri);
      signals.m_cameray[i] = hxisgdevtid::remap::getCAMERAY(remap_table,ri);
      signals.m_cameraz[i] = hxisgdevtid::remap::getCAMERAZ(remap_table,ri);
      signals.m_layer[i] = hxisgdevtid::remap::getLAYER(remap_table,ri);
      signals.m_layer_type[i] = hxisgdevtid::SGDLayerType(signals.m_layer[i]);

      // Discount signal if EPI=NULL or EPI below threshold or based on
      // active channel flags
      if (epi_i < epithre_table[ri]) {
        AH_DEBUG << "ignore signal " << i << " which is below threshold" << std::endl;
        sig_okay=false;
      }
      if (epi_i_null) {
        AH_DEBUG << "ignore signal " << i << " which is NULL" << std::endl;
        sig_okay=false;
      }
      if (activechan_table[ri] == hxisgdevtid::badpix::e_BAD_PIXEL) {
        AH_DEBUG << "ignore signal " << i << " which has a bad active channel flag" << std::endl;
        sig_okay=false;
      }
      if (activechan_table[ri] == hxisgdevtid::badpix::e_STRANGE_PIXEL) {
        if (par.strangepix == 0) {
          AH_DEBUG << "ignore signal " << i << " which is strange (and strangepix parameter = 0)" << std::endl;
          sig_okay=false;
        }
      }

      //  The following information is used below to control and track
      //  signal merging.
      //
      if (!sig_okay) {
        //
        AH_DEBUG << "In row " << rownumber << " epi_i=" << epi_i << "; epithre_table[ri]=" << epithre_table[ri] << std::endl;
        signals.m_flag[i] = false;
        signals.m_merge_survivor[i] = false;
        signals.m_sigarray_0[i] = -1;
        ++n_bad_sigrow;
      } else {
        signals.m_flag[i] = true;
        signals.m_merge_survivor[i] = true;
        signals.m_sigarray_0[i] = i;
        ++n_good_sigrow;
      }
      ++signals.m_nsignal;
    }    // end loop over signals
    if (par.extrainfo) {
      for (int i=0; i < signals.m_nsignal; ++i) {
        extraoutrow.m_sigarray0[i] = signals.m_sigarray_0[i];
      }
      extraoutrow.num_sigarray0 = signals.m_nsignal;
    }
    AH_DEBUG << "Num signals/good/bad: " << signals.m_nsignal << ", " << n_good_sigrow << ", " << n_bad_sigrow << std::endl;

    if (n_good_sigrow <= 0) {
      AH_DEBUG << "Finished: No good signals in row " << rownumber << " (RECO_ALL_SIGNALS_LOW)" << std::endl;
      AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
      sgdevtidlib::setOutputEventNull(outrow);
      sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_ALL_SIGNALS_LOW);
      ++num_rows_no_reco_attempt;
      write_row = true;
      continue;   // write and go to next row in SFF
    }

    //  There is at least one good signal at this point.
    //
    if (nsignal == 1) {   //  One signal total (a good one).
      AH_DEBUG << "Finished: only one signal (RECO_ONE_SIGNAL)" << std::endl;
      sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_ONE_SIGNAL);

      //  Get event layer and that it is photoabsorption.
      //
      getSequenceOneHitNoEscape(signals, sequence_type);

      //  Build output row.
      //
      buildOutputRowOneHit(signals, outrow);
      ++num_rows_reco_trivial;

      write_row = true;
      continue;   // write and go to next row in SFF
    }

    //  There are several signals at this point (nsignal > 1).
  
    //  Exactly one signal above threshold, but more than 1 signal
    //  so some were below threshold.
    //
    if (n_good_sigrow == 1) {
      AH_DEBUG << "Finished: only one signal above threshold (RECO_ONE_GOOD_SIGNAL)" << std::endl;
      sgdevtidlib::setRecoStatusBit(outrow, sgdevtidlib::RECO_ONE_GOOD_SIGNAL);

      //  Get event layer and that it is photoabsorption.
      //
      getSequenceOneHitNoEscape(signals, sequence_type);

      buildOutputRowOneHit(signals, outrow);
      ++num_rows_reco_trivial;

      write_row = true;
      continue;   // write and go to next row in SFF
    }
    
    ++num_rows_reco_nontrivial;   // Count non-trivial event reconstructions.

    AH_DEBUG << "About to call mergeAdjacentSignals in row " << rownumber << std::endl;
    sgdevtidlib::mergeAdjacentSignals (par, fluor_table, signals,
      outrow, extraoutrow, sequence_type, reco_complete);

    if (reco_complete) {
      write_row = true;
      continue;   // write and go to next row in SFF
    }

    //  At this point, we have merged adjacent signals in the
    //  same sensor.  Now we go through the hit processing
    //  involving non-adjacent signals from the same or different layers.
    AH_DEBUG << "About to call mergeFluorAndScatter in row " << rownumber << std::endl;
    sgdevtidlib::mergeFluorAndScatter(par, signals, fluor_table, 
      geom, outrow, extraoutrow, sequence_type, reco_complete);

    if (reco_complete) {
      write_row = true;
      continue;  // write and go to next row in SFF
    }

    //  Steps 2, 3, 4, and 5:  Analyze hit pattern.
    //  Each non-zero epi is either (a) the result of merging a pair of
    //  signals, or (b) a good signal with no associated secondary.  These 
    //  epi and their associated CAMERAXYZ positions are now called hits 
    //  instead of signals. This section of code analyzes the pattern
    //  of hits.

    //  The list of signals has been whittled down by merging scattering
    //  and fluorescence signals into the originating signals.  The
    //  remaining signals are "hits."  These are flagged (inside the
    //  called functions) using the
    //  field m_possible_hit in the OneSig structure.

    //  Gather up the hits found by preceding function calls.
    //
    numhit = 0;
    hits.m_best_k=-1;     // reset selected sequence (best_k) for this occurrence
    for (int i=0; i < nsignal; ++i) {
      if (signals.m_merge_survivor[i]) {
        hits.m_hitarray[numhit] = i;
        hits.m_epiarray[numhit] = signals.m_epi_current[i];
        hits.m_deltaepiarray[numhit] = std::sqrt(signals.m_var_current[i]);
        ++numhit;
      }
    }
    hits.m_m = numhit;

    AH_DEBUG << "Number of hits after merging: " << hits.m_m << std::endl;
    AH_DEBUG << "         Hit   Signal       EPI                     delta-EPI" << std::endl;
    for (int i=0; i < hits.m_m; i++) {
      std::stringstream msg;
      msg << "--HIT: "
          << std::setw(4) << i
          << std::setw(8) << hits.m_hitarray[i]
          << std::setw(20) << std::setprecision(15) << hits.m_epiarray[i]
          << std::setw(20) << std::setprecision(15) << hits.m_deltaepiarray[i];
      msg << std::endl;
      AH_DEBUG << msg.str();
    }


    //  Copy hits to extra output.
    //
    if (par.extrainfo) {
      for (int i=0; i<numhit; ++i) {
        extraoutrow.m_hitarray[i] = hits.m_hitarray[i];
        extraoutrow.m_epiarray[i] = hits.m_epiarray[i];
        extraoutrow.m_deltaepiarray[i] = hits.m_deltaepiarray[i];
        extraoutrow.m_hitarray_null[i] = 0;
        extraoutrow.m_epiarray_null[i] = 0;
        extraoutrow.m_deltaepiarray_null[i] = 0;
      }
    }

    //  Check.  (This test should never pass in this context.)
    //
    if (numhit < 2 || numhit > sgdevtidlib::MAX_NUMHITS) {
      AH_THROW_LOGIC("hitarray has too few or too many hits.");
    }
  
    //  At this point, 2 <= m <= 4, by definition.

    //  The following function combines all the steps that
    //  may require an extra iteration due to the escape energy
    //  calculation.
    //
    //  Step 2:  F and G tests of scattering angle to
    //           reject unphysical sequences.
    //  Step 3:  Reject low-probability sequences 
    //           according to layer types.
    //
    AH_DEBUG << "About to call performFGProbTest in row " << rownumber << std::endl;
    sgdevtidlib::performFGProbTests (par, signals, hits,
      fluor_table, probseq_table, geom, prob, test_f, test_g, g, escape_flag, escape_en,
      outrow, fpextra, extraoutrow, sequence_type, reco_complete);

    if (reco_complete && hits.m_best_k < 0) {    // performFGProbTests finished with a reconstruction failure
      write_row = true;
      continue;   // write and go to next row in SFF
    }

    //
    //  Step 4.  Tie-breaking using figure of merit (FOM), which
    //  is a function of several variables and user parameters
    //  to choose only one sequence
    //
    AH_DEBUG << "About to call performTieBreaking in row " << rownumber << std::endl;
    sgdevtidlib::performTieBreaking (par, signals, hits,
      geom, prob, test_f, test_g, g, escape_flag, escape_en, 
      outrow, extraoutrow, sequence_type, reco_complete);

    if (!reco_complete) {
      AH_THROW_LOGIC("reconstruction not complete after tie-breaking");
    }

    //  There is no condition test here.  Tie breaking cannot
    //  result in a failed reconstruction, and it always
    //  chooses a single result.

    //  Step 5.  FOV probability (background discrimination).
    //
    //  No accept/reject algorithm here -- obtain from CALDB file
    //  the probability that the source is in the FOV, and store
    //  in the output table row, along with dist0 and phi.
    //
    AH_DEBUG << "About to call getProbFOV in row " << rownumber << std::endl;
    sgdevtidlib::getProbFOV(probfov_table, signals, hits, escape_en, 
      outrow, extraoutrow);

    //  There is no condition test here.  Obtaining the FOV
    //  probability cannot result in a failed reconstruction.
    write_row = true;
  }

  //  For all but the last row, writing is done at the head of the loop just
  //  before reading.  This block is necessary in order to pick up the last row.
  //
  if (write_row) {

    // print sequence information if occurrence_id set
    if (par.occurrenceid > 0 && hits.m_m > 1) prSeq(signals, hits);

    //  Build the part of the primary output row that applies
    //  to all occurrences.
    //
    outrow.m_numsignal = signals.m_nsignal;
    extraoutrow.m_numsignal = signals.m_nsignal;
    if (outrow.m_pi_null == 0) {    // only fill SEQ_HITS if PI was assigned
      outrow.m_seq_hits_null=0;
      outrow.m_seq_hits = sgdprobseq::lookupMechanism(sequence_type.m_sequence,
                                                    escape_flag,
                                                    probseq_table);
    }

    AH_DEBUG << "outrow.m_pi_null          = " << (int)outrow.m_camerax_null << std::endl;
    AH_DEBUG << "outrow.m_delcomption_null = " << (int)outrow.m_cameray_null << std::endl;
    AH_DEBUG << "outrow.m_compton_th_null  = " << (int)outrow.m_cameraz_null << std::endl;
    AH_DEBUG << "outrow.m_compton_ph_null  = " << (int)outrow.m_cameraz_null << std::endl;
    AH_DEBUG << "outrow.m_distance0_null   = " << (int)outrow.m_cameraz_null << std::endl;
    AH_DEBUG << "outrow.m_offaxis_null     = " << (int)outrow.m_cameraz_null << std::endl;
    AH_DEBUG << "outrow.m_camerax_null     = " << (int)outrow.m_camerax_null << std::endl;
    AH_DEBUG << "outrow.m_cameray_null     = " << (int)outrow.m_cameray_null << std::endl;
    AH_DEBUG << "outrow.m_cameraz_null     = " << (int)outrow.m_cameraz_null << std::endl;
    AH_DEBUG << "outrow.m_occurrence_id    = " << outrow.m_occurrence_id << std::endl;
    AH_DEBUG << "Writing output row " << outrow.m_row_count+1 << std::endl;
    //
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);
    ++outrow.m_row_count;
    if (outrow.m_pi_null == 1) outrow.m_count_pi_null++;
    if (outrow.m_row_count < inrow.m_row_count) {
        std::stringstream ss;
        ss.str("");
        ss << "Fewer output than input rows:  inrow.m_row_count = " << inrow.m_row_count << "; outrow.m_row_count = " << outrow.m_row_count;
        AH_THROW_LOGIC(ss.str());
    }
    if (par.extrainfo) {
      //
      AH_DEBUG << "extraoutrow.num_escape_flag = " << extraoutrow.num_escape_flag << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray0 = " << extraoutrow.num_sigarray0 << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray1_0 = " << extraoutrow.num_sigarray1_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray1a_1a = " << extraoutrow.num_sigarray1a_1a << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray1a_1b = " << extraoutrow.num_sigarray1a_1b << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray1a_2 = " << extraoutrow.num_sigarray1a_2 << std::endl;
      AH_DEBUG << "extraoutrow.num_sigarray1a_3 = " << extraoutrow.num_sigarray1a_3 << std::endl;
      AH_DEBUG << "extraoutrow.num_e = " << extraoutrow.num_e << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_e = " << extraoutrow.num_delta_e << std::endl;
      AH_DEBUG << "extraoutrow.num_f = " << extraoutrow.num_f << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_f = " << extraoutrow.num_delta_f << std::endl;
      AH_DEBUG << "extraoutrow.num_check_f = " << extraoutrow.num_check_f << std::endl;
      AH_DEBUG << "extraoutrow.num_test_f = " << extraoutrow.num_test_f << std::endl;
      AH_DEBUG << "extraoutrow.num_cos_theta_g = " << extraoutrow.num_cos_theta_g << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_cos_theta_g = " << extraoutrow.num_delta_cos_theta_g << std::endl;
      AH_DEBUG << "extraoutrow.num_cos_theta_k = " << extraoutrow.num_cos_theta_k << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_cos_theta_k = " << extraoutrow.num_delta_cos_theta_k << std::endl;
      AH_DEBUG << "extraoutrow.num_g = " << extraoutrow.num_g << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_g = " << extraoutrow.num_delta_g << std::endl;
      AH_DEBUG << "extraoutrow.num_check_g = " << extraoutrow.num_check_g << std::endl;
      AH_DEBUG << "extraoutrow.num_test_g = " << extraoutrow.num_test_g << std::endl;
      AH_DEBUG << "extraoutrow.num_prob = " << extraoutrow.num_prob << std::endl;
      AH_DEBUG << "extraoutrow.num_test_prob = " << extraoutrow.num_test_prob << std::endl;
      AH_DEBUG << "extraoutrow.num_fom = " << extraoutrow.num_fom << std::endl;
      AH_DEBUG << "extraoutrow.num_cos_theta_g_0 = " << extraoutrow.num_cos_theta_g_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_cos_theta_g_0 = " << extraoutrow.num_delta_cos_theta_g_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_cos_theta_k_0 = " << extraoutrow.num_cos_theta_k_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_cos_theta_k_0 = " << extraoutrow.num_delta_cos_theta_k_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_g_0 = " << extraoutrow.num_g_0 << std::endl;
      AH_DEBUG << "extraoutrow.num_delta_g_0 = " << extraoutrow.num_delta_g_0 << std::endl;

      AH_DEBUG << "extraoutrow.m_occurrence_id    = " << extraoutrow.m_occurrence_id << std::endl;
      AH_DEBUG << "Writing extra output row " << extraoutrow.m_row_count+1 << std::endl;
      ahfits::writeRow(fpextra);
      ahfits::nextRow(fpextra);
      ++extraoutrow.m_row_count;
      if (extraoutrow.m_row_count < inrow.m_row_count) {
        std::stringstream ss;
        ss.str("");
        ss << "Fewer extraoutput than input rows:  inrow.m_row_count = " << inrow.m_row_count << "; extraoutrow.m_row_count = " << extraoutrow.m_row_count;
        AH_THROW_LOGIC(ss.str());
      }
    }
  }

  AH_INFO(ahlog::HIGH) << "Input SFF rows:                             " << inrow.m_row_count << std::endl;
  AH_INFO(ahlog::HIGH) << "   Counts accumulated during processing:" << std::endl;
  AH_INFO(ahlog::HIGH) << "   No reconstruction attempted:                 " << num_rows_no_reco_attempt << std::endl;
  AH_INFO(ahlog::HIGH) << "   Trival reconstruction (1 signal):            " << num_rows_reco_trivial << std::endl;
  AH_INFO(ahlog::HIGH) << "   Multiple-signal reconstruction attempted:    " << num_rows_reco_nontrivial << std::endl;
  AH_INFO(ahlog::HIGH) << "   TOTAL:                             " 
    << num_rows_no_reco_attempt + num_rows_reco_trivial + num_rows_reco_nontrivial << std::endl;
  AH_INFO(ahlog::HIGH) << "Output event file rows:  " << outrow.m_row_count << std::endl;
  AH_INFO(ahlog::HIGH) << "   PI set to NULL on output: " << outrow.m_count_pi_null << std::endl;
  AH_INFO(ahlog::HIGH) << "Extra output file rows: " << extraoutrow.m_row_count << std::endl;

  prReco(outrow);

  if (par.extrainfo) delete routextraout;

}

// ****************************************************************************

void finalize(sgdevtidlib::Par& par, ahfits::FilePtr fpin, 
              ahfits::FilePtr fpout, ahfits::FilePtr fpextra,
              hxisgdevtid::badpix::ThresholdTable & epithre_table,
              hxisgdevtid::badpix::ActiveChanTable & activechan_table,
              hxisgdevtid::remap::DataType & remap_table) {
  ahfits::close(fpin);
  ahfits::close(fpout);
  if (par.extrainfo) ahfits::close(fpextra);
  epithre_table.clear();
  remap_table.clear();
}

// ****************************************************************************

void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      ahfits::FilePtr& fpdest) {

  // create empty output file with EVENTS HDU
  ahfits::create(outfile,"",&fpdest);
  ahfits::addEmptyTbl(fpdest,"EVENTS");

  // copy keywords from input file
  ahmission::keyword::copyAllKeywords(fpsrc,fpdest,ahmission::keyword::e_EVENT);

  // copied from input
  ahfits::insertColAfter(fpdest,"TIME","1D");
  ahfits::setTUnit(fpdest,"TIME","s");
  ahfits::setColumnDescription(fpdest, "TIME", "Seconds from 01 Jan 2014 00:00:00");
  ahfits::insertColAfter(fpdest,"OCCURRENCE_ID","1J");
  ahfits::setColumnDescription(fpdest, "OCCURRENCE_ID", "Sequential number for occurrence");
  ahfits::insertColAfter(fpdest,"CATEGORY","1B");
  ahfits::setColumnDescription(fpdest, "CATEGORY", "Data recorder priority");
  ahfits::insertColAfter(fpdest,"FLAG_LCHKMIO",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_LCHKMIO,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_LCHKMIO", "0=ok 1=error MIO received data");
  ahfits::insertColAfter(fpdest,"FLAG_CCBUSY",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_CCBUSY,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_CCBUSY", "1=CC busy 0=CC not busy");
  ahfits::insertColAfter(fpdest,"FLAG_HITPAT_CC",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_HITPAT_CC,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_HITPAT_CC", "CC hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_HITPAT",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_HITPAT,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_HITPAT", "BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_FASTBGO",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_FASTBGO,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_FASTBGO", "Fast BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_SEU",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_SEU,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_SEU", "0=ok 1=single event upset");
  ahfits::insertColAfter(fpdest,"FLAG_LCHK",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_LCHK,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_LCHK", "0=ok 1=length check error");
  ahfits::insertColAfter(fpdest,"FLAG_CALMODE",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_CALMODE,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_CALMODE", "1=calibration mode 0=other");
  ahfits::insertColAfter(fpdest,"FLAG_TRIGPAT",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_TRIGPAT,"X"));
  ahfits::setColumnDescription(fpdest, "FLAG_TRIGPAT", "Trigger pattern");
  ahfits::insertColAfter(fpdest,"FLAG_TRIG",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_TRIG,"B"));
  ahfits::setColumnDescription(fpdest, "FLAG_TRIG", "Trigger origin");
  ahfits::insertColAfter(fpdest,"LIVETIME","1V");
  ahfits::setColumnDescription(fpdest, "LIVETIME", "Time since previous occurrence");
  ahfits::insertColAfter(fpdest,"PROC_STATUS",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_PROC_STATUS,"X"));
  ahfits::setColumnDescription(fpdest, "PROC_STATUS", "Record bad telemetry or bad values");
  ahfits::insertColAfter(fpdest,"STATUS",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_STATUS,"X"));
  ahfits::setColumnDescription(fpdest, "STATUS", "Occurrence status flags");

  // new columns
  ahfits::insertColAfter(fpdest,"PI","I");
  ahfits::setColumnDescription(fpdest, "PI", "Pulse Invariant");
  ahfits::setTNull(fpdest,"PI",-999);
  ahfits::setTLmin(fpdest,"PI",0);
  ahfits::setTLmax(fpdest,"PI",2047);
  ahfits::insertColAfter(fpdest,"ENE_TOTAL","E");
  ahfits::setColumnDescription(fpdest, "ENE_TOTAL", "Sum of EPI for occurrence");
  ahfits::insertColAfter(fpdest,"NUMSIGNAL","I");
  ahfits::setColumnDescription(fpdest, "NUMSIGNAL", "Number of signals in occurrence");
  ahfits::insertColAfter(fpdest,"NUMHITS","5X");
  ahfits::setColumnDescription(fpdest, "NUMHITS", "Hit distribution");
  ahfits::insertColAfter(fpdest,"SEQ_HITS","I");
  ahfits::setColumnDescription(fpdest, "SEQ_HITS", "Sequence of hits from CALDB");
  ahfits::setTNull(fpdest,"SEQ_HITS",-999);
  ahfits::insertColAfter(fpdest,"DELCOMPTON","2E");
  ahfits::setColumnDescription(fpdest, "DELCOMPTON", "Value of DeltaG (M>2)");
  ahfits::insertColAfter(fpdest,"COMPTON_TH","E");
  ahfits::setColumnDescription(fpdest, "COMPTON_TH", "Value of Compton ThetaK(0)");
  ahfits::insertColAfter(fpdest,"COMPTON_PH","E");
  ahfits::setColumnDescription(fpdest, "COMPTON_PH", "Value of Compton Phi");
  ahfits::insertColAfter(fpdest,"DISTANCE0","E");
  ahfits::setColumnDescription(fpdest, "DISTANCE0", "(mm) Distance, 1st two hits");
  ahfits::insertColAfter(fpdest,"OFFAXIS","E");
  ahfits::setColumnDescription(fpdest, "OFFAXIS", "Offaxis angle");
  ahfits::insertColAfter(fpdest,"CAMERAX","E");
  ahfits::setColumnDescription(fpdest, "CAMERAX", "1st hit coord camerax");
  ahfits::setTLmin(fpdest,"CAMERAX",-39.0);
  ahfits::setTLmax(fpdest,"CAMERAX",39.0);
  ahfits::insertColAfter(fpdest,"CAMERAY","E");
  ahfits::setColumnDescription(fpdest, "CAMERAY", "1st hit coord cameray");
  ahfits::setTLmin(fpdest,"CAMERAY",-39.0);
  ahfits::setTLmax(fpdest,"CAMERAY",39.0);
  ahfits::insertColAfter(fpdest,"CAMERAZ","E");
  ahfits::setColumnDescription(fpdest, "CAMERAZ", "1st hit coord cameraz");
  ahfits::setTLmin(fpdest,"CAMERAZ",-77.0);
  ahfits::setTLmax(fpdest,"CAMERAZ",3.0);
  ahfits::insertColAfter(fpdest,"LIKELIHOOD","E");
  ahfits::setColumnDescription(fpdest, "LIKELIHOOD", "Likelihood of event");
  ahfits::insertColAfter(fpdest,"RECO_STATUS",hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_RECO_STATUS,"X"));
  ahfits::setColumnDescription(fpdest, "RECO_STATUS", "Reconstruction status");
  ahfits::insertColAfter(fpdest,"MATTYPE","I");
  ahfits::setColumnDescription(fpdest, "MATTYPE", "Material type");
  ahfits::setTNull(fpdest,"MATTYPE",-999);
  ahfits::setTLmin(fpdest,"MATTYPE",1);
  ahfits::setTLmax(fpdest,"MATTYPE",3);
}

// ****************************************************************************


void createExtraOutputFile(ahfits::FilePtr fpsrc, const std::string& outtracefile, 
                      ahfits::FilePtr& fpdest) {

  std::string tformvare, tformvare4;   //  Used to build "1P()" variable-length FITS descriptors
  std::string tformvari;
  std::string tformvarx, tformvarx4;
  std::stringstream str;

  // create empty output file with EVENTS HDU
  ahfits::create(outtracefile,"",&fpdest);
  ahfits::addEmptyTbl(fpdest,"EVENTS");

  // copy keywords from input file
  ahmission::keyword::copyAllKeywords(fpsrc,fpdest,ahmission::keyword::e_EVENT);

  // copied from input
  ahfits::insertColAfter(fpdest,"TIME","1D");
  ahfits::setTUnit(fpdest,"TIME","s");
  ahfits::setColumnDescription(fpdest, "TIME", "Seconds from 01 Jan 2014 00:00:00");
  ahfits::insertColAfter(fpdest,"OCCURRENCE_ID","1J");
  ahfits::setColumnDescription(fpdest, "OCCURRENCE_ID", "Sequential number for occurrence");

  // New columns generated by the task (all variable-length).
  //
  str.str("");
  str << "1PI(" << sgdevtidlib::MAX_PERM << ")" ;   // For dimension = number of permutations
  tformvari = str.str();

  str.str("");
  str << "1PE(" << sgdevtidlib::MAX_PERM << ")" ;   // For dimension = number of permutations
  tformvare = str.str();

  str.str("");
  str << "1PE(" << sgdevtidlib::MAX_PERM*sgdevtidlib::MAX_NUMHITS << ")" ; // For dimension = 4*(number of perms)
  tformvare4 = str.str();

  str.str("");
  str << sgdevtidlib::MAX_PERM << "X" ;   // For dimension = number of permutations
  tformvarx = str.str();

  str.str("");
  str << sgdevtidlib::MAX_PERM*sgdevtidlib::MAX_NUMHITS << "X" ; // For dimension = 4*(number of perms)
  tformvarx4 = str.str();

  ahfits::insertColAfter(fpdest, "NUMSIGNAL", "1I");
  ahfits::setColumnDescription(fpdest, "NUMSIGNAL", "Number of signals in occurrence");
  ahfits::insertColAfter(fpdest, "M", "1I");
  ahfits::setColumnDescription(fpdest, "M", "Number of hits in occurrence");
  ahfits::insertColAfter(fpdest, "NUMPERM", "1I");
  ahfits::setColumnDescription(fpdest, "NUMPERM", "Number of permutations");
  ahfits::insertColAfter(fpdest, "ESCAPE_FLAG", "1X");
  ahfits::setColumnDescription(fpdest, "ESCAPE_FLAG", "Esc energy computed 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "SIGARRAY0", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY0", "Signal IDs before merging");
  ahfits::setTNull(fpdest, "SIGARRAY0", -1);
  ahfits::insertColAfter(fpdest, "SIGARRAY1_0", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY1_0", "Merged signals step 1_0");
  ahfits::setTNull(fpdest, "SIGARRAY1_0", -1);
  ahfits::insertColAfter(fpdest, "SIGARRAY1A_1A", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY1A_1A", "Merged signals step 1A_1A");
  ahfits::setTNull(fpdest, "SIGARRAY1A_1A", -1);
  ahfits::insertColAfter(fpdest, "SIGARRAY1A_1B", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY1A_1B", "Merged signals step 1A_1B");
  ahfits::setTNull(fpdest, "SIGARRAY1A_1B", -1);
  ahfits::insertColAfter(fpdest, "SIGARRAY1A_2", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY1A_2", "Merged signals step 1A_2");
  ahfits::setTNull(fpdest, "SIGARRAY1A_2", -1);
  ahfits::insertColAfter(fpdest, "SIGARRAY1A_3", tformvari);
  ahfits::setColumnDescription(fpdest, "SIGARRAY1A_3", "Merged signals step 1A_3");
  ahfits::setTNull(fpdest, "SIGARRAY1A_3", -1);
  ahfits::insertColAfter(fpdest, "HITARRAY", "4I");
  ahfits::setColumnDescription(fpdest, "HITARRAY", "Hit IDs");
  ahfits::setTNull(fpdest, "HITARRAY", -1);
  ahfits::insertColAfter(fpdest, "EPIARRAY", "4E");
  ahfits::setColumnDescription(fpdest, "EPIARRAY", "Hit energies");
  ahfits::insertColAfter(fpdest, "DELTAEPIARRAY", "4E");
  ahfits::setColumnDescription(fpdest, "DELTAEPIARRAY", "Hit energy errors");
  ahfits::insertColAfter(fpdest, "E", tformvare4);
  ahfits::setColumnDescription(fpdest, "E", "Cumulative hit energies");
  ahfits::insertColAfter(fpdest, "DELTA_E", tformvare4);
  ahfits::setColumnDescription(fpdest, "E", "Error in cumulative hit energies");
  ahfits::insertColAfter(fpdest, "F", tformvare4);
  ahfits::setColumnDescription(fpdest, "F", "F for Compton energy test");
  ahfits::insertColAfter(fpdest, "DELTA_F", tformvare4);
  ahfits::setColumnDescription(fpdest, "DELTA_F", "Error in F");
  ahfits::insertColAfter(fpdest, "CHECK_F", tformvarx4);
  ahfits::setColumnDescription(fpdest, "CHECK_F", "F test per hit 1=pass 0=fail");
  ahfits::insertColAfter(fpdest, "TEST_F", tformvarx);
  ahfits::setColumnDescription(fpdest, "TEST_F", "F test 1=pass 0=fail");
  ahfits::insertColAfter(fpdest, "C_THETA_G", tformvare4);
  ahfits::setColumnDescription(fpdest, "C_THETA_G", "Cos theta_G for kinematic test");
  ahfits::insertColAfter(fpdest, "D_C_THETA_G", tformvare4);
  ahfits::setColumnDescription(fpdest, "D_C_THETA_G", "Error in cos theta_G");
  ahfits::insertColAfter(fpdest, "C_THETA_K", tformvare4);
  ahfits::setColumnDescription(fpdest, "C_THETA_K", "Cos theta_K for kinematic test");
  ahfits::insertColAfter(fpdest, "D_C_THETA_K", tformvare4);
  ahfits::setColumnDescription(fpdest, "D_C_THETA_K", "Error in cos theta_K");
  ahfits::insertColAfter(fpdest, "G", tformvare4);
  ahfits::setColumnDescription(fpdest, "G", "G for kinematic test");
  ahfits::insertColAfter(fpdest, "DELTA_G", tformvare4);
  ahfits::setColumnDescription(fpdest, "DELTA_G", "Error in G");
  ahfits::insertColAfter(fpdest, "CHECK_G", tformvarx4);
  ahfits::setColumnDescription(fpdest, "CHECK_G", "G test per hit 1=pass 0=fail");
  ahfits::insertColAfter(fpdest, "TEST_G", tformvarx);
  ahfits::setColumnDescription(fpdest, "TEST_G", "G test 1=pass 0=fail");
  ahfits::insertColAfter(fpdest, "PROB", tformvare4);
  ahfits::setColumnDescription(fpdest, "PROB", "Seq prob per permutation");
  ahfits::insertColAfter(fpdest, "TEST_PROB", tformvarx);
  ahfits::setColumnDescription(fpdest, "TEST_PROB", "Seq prob test 1=pass 0=fail");
  ahfits::insertColAfter(fpdest, "FOM", tformvare4);
  ahfits::setColumnDescription(fpdest, "FOM", "Tie-breaking FOM per permutation");
  ahfits::insertColAfter(fpdest, "C_THETA_G_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "C_THETA_G_0", "Cos theta_g(0) per perm");
  ahfits::insertColAfter(fpdest, "D_C_THETA_G_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "D_C_THETA_G_0", "Error in cos theta_g(0)");
  ahfits::insertColAfter(fpdest, "C_THETA_K_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "C_THETA_K_0", "Cos theta_k(0) per perm");
  ahfits::insertColAfter(fpdest, "D_C_THETA_K_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "D_C_THETA_K_0", "Error in cos theta_k(0)");
  ahfits::insertColAfter(fpdest, "G_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "G_0", "Cos theta_g(0) - cos theta_k(0)");
  ahfits::insertColAfter(fpdest, "DELTA_G_0", tformvare4);
  ahfits::setColumnDescription(fpdest, "DELTA_G_0", "Error in G_0");

  ahfits::insertColAfter(fpdest, "CLSTRSHAPE", "1I");
  ahfits::setColumnDescription(fpdest, "CLSTRSHAPE", "Cluster shape code in step 1_0");
  ahfits::insertColAfter(fpdest, "MERGE1_0", "1X");
  ahfits::setColumnDescription(fpdest, "MERGE1_0", "Merging in step 1_0 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "MERGE1A_1A", "1X");
  ahfits::setColumnDescription(fpdest, "MERGE1A_1A", "Merging in step 1a_1a 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "MERGE1A_1B", "1X");
  ahfits::setColumnDescription(fpdest, "MERGE1A_1B", "Merging in step 1a_1b 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "MERGE1A_2", "1X");
  ahfits::setColumnDescription(fpdest, "MERGE1A_2", "Merging in step 1a_2 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "MERGE1A_3", "1X");
  ahfits::setColumnDescription(fpdest, "MERGE1A_3", "Merging in step 1a_3 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "RAND1_0", "1X");
  ahfits::setColumnDescription(fpdest, "RAND1_0", "Max signal chosen randomly 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "RAND1A_3", "1X");
  ahfits::setColumnDescription(fpdest, "RAND1A_3", "Max signal chosen randomly 1=yes 0=no");
  ahfits::insertColAfter(fpdest, "RAND_FOM", "1X");
  ahfits::setColumnDescription(fpdest, "RAND_FOM", "Max FOM chosen randomly 1=yes 0=no");
}

// ****************************************************************************

void prepareOutputData(hxisgdevtid::SFFRowData& inrow, sgdevtidlib::OutputRowData& outrow) {

  // initialize output columns
  outrow.m_pi=0;
  outrow.m_pi_null=0;
  outrow.m_ene_total = 0.0;
  outrow.m_ene_total_null = 1;
  outrow.m_numsignal = 0;
  for (int i=0; i<5; ++i) {
    outrow.m_numhits[i] = 0;
  }
  outrow.m_seq_hits = 0;
  outrow.m_seq_hits_null=0;
  outrow.m_delcompton[0] = -1.;
  outrow.m_delcompton[1] = -1.;
  outrow.m_compton_th = 0.0;
  outrow.m_compton_th_null = 1;
  outrow.m_compton_ph = 0.0;
  outrow.m_compton_ph_null = 1;
  outrow.m_distance0 = 0.0; 
  outrow.m_distance0_null = 1;
  outrow.m_offaxis = 0.0;
  outrow.m_offaxis_null = 1;
  outrow.m_camerax=0.0;
  outrow.m_camerax_null=0;
  outrow.m_cameray=0.0;
  outrow.m_cameray_null=0;
  outrow.m_cameraz=0.0;
  outrow.m_cameraz_null=0;
  outrow.m_probability = 0;
  outrow.m_probability_null = 1;
  outrow.m_mattype=0;
  outrow.m_mattype_null=1;
  clearRecoStatus(outrow);
}

// ****************************************************************************

void prepareExtraOutputData(hxisgdevtid::SFFRowData& inrow, 
  sgdevtidlib::ExtraOutputRowData& extraoutrow) {

  extraoutrow.m_numsignal = 0;
  extraoutrow.m_m = 0;
  extraoutrow.m_numperm = 0;
  extraoutrow.m_escape_flag = 0;

  for (ahfits::IndexType ii=0; ii < sgdevtidlib::MAX_NUMSIGNAL; ii++) {
    extraoutrow.m_sigarray0[ii] = -1;
    extraoutrow.m_sigarray1_0[ii] = -1;
    extraoutrow.m_sigarray1a_1a[ii] = -1;
    extraoutrow.m_sigarray1a_1b[ii] = -1;
    extraoutrow.m_sigarray1a_2[ii] = -1;
    extraoutrow.m_sigarray1a_3[ii] = -1;
  }
  for (ahfits::IndexType ii=0; ii < sgdevtidlib::MAX_NUMHITS; ii++) {
    extraoutrow.m_hitarray[ii] = -1;
    extraoutrow.m_epiarray[ii] = 0.0;
    extraoutrow.m_deltaepiarray[ii] = 0.0;
    extraoutrow.m_hitarray_null[ii] = 1;
    extraoutrow.m_epiarray_null[ii] = 1;
    extraoutrow.m_deltaepiarray_null[ii] = 1;
  }
  for (ahfits::IndexType ii=0; ii < sgdevtidlib::MAX_PERM*sgdevtidlib::MAX_NUMHITS; ii++) {
    extraoutrow.m_e[ii] = 0.0;
    extraoutrow.m_delta_e[ii] = 0.0;
    extraoutrow.m_f[ii] = 0.0;
    extraoutrow.m_delta_f[ii] = 0.0;
    extraoutrow.m_check_f[ii] = 0;
    extraoutrow.m_cos_theta_g[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_g[ii] = 0.0;
    extraoutrow.m_cos_theta_k[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_k[ii] = 0.0;
    extraoutrow.m_g[ii] = 0.0;
    extraoutrow.m_delta_g[ii] = 0.0;
    extraoutrow.m_check_g[ii] = 0;
  }
  extraoutrow.num_e = 1;
  extraoutrow.num_delta_e = 1;
  extraoutrow.num_f = 1;
  extraoutrow.num_delta_f = 1;
  extraoutrow.num_cos_theta_g = 1;
  extraoutrow.num_delta_cos_theta_g = 1;
  extraoutrow.num_cos_theta_k = 1;
  extraoutrow.num_delta_cos_theta_k = 1;
  extraoutrow.num_g = 1;
  extraoutrow.num_delta_g = 1;
  for (ahfits::IndexType ii=0; ii < sgdevtidlib::MAX_PERM; ii++) {
    extraoutrow.m_test_f[ii] = 0;
    extraoutrow.m_test_g[ii] = 0;
    extraoutrow.m_prob[ii] = 0.0;
    extraoutrow.m_test_prob[ii] = 0;
    extraoutrow.m_fom[ii] = 0.0;
    extraoutrow.m_cos_theta_g_0[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_g_0[ii] = 0.0;
    extraoutrow.m_cos_theta_k_0[ii] = 0.0;
    extraoutrow.m_delta_cos_theta_k_0[ii] = 0.0;
    extraoutrow.m_g_0[ii] = 0.0;
    extraoutrow.m_delta_g_0[ii] = 0.0;
  }
  extraoutrow.num_prob = 1;
  extraoutrow.num_test_prob = 1;
  extraoutrow.num_fom = 1;
  extraoutrow.num_cos_theta_g_0 = 1;
  extraoutrow.num_delta_cos_theta_g_0 = 1;
  extraoutrow.num_cos_theta_k_0 = 1;
  extraoutrow.num_delta_cos_theta_k_0 = 1;
  extraoutrow.num_g_0 = 1;
  extraoutrow.num_delta_g_0 = 1;

  extraoutrow.m_clstrshape=1;     // initialize as single signal cluster
  extraoutrow.m_merge1_0=0;
  extraoutrow.m_merge1a_1a=0;
  extraoutrow.m_merge1a_1b=0;
  extraoutrow.m_merge1a_2=0;
  extraoutrow.m_merge1a_3=0;
  extraoutrow.m_rand1_0=0;
  extraoutrow.m_rand1a_3=0;
  extraoutrow.m_rand_fom=0;
}

// ****************************************************************************

bool hasNonZeroHitPat(hxisgdevtid::SFFRowData& inrow) {
  if (inrow.m_flag_hitpat[0] != 0) return true;
  if (inrow.m_flag_hitpat[1] != 0) return true;
  return false;
}

// ****************************************************************************

bool hasNonZeroFastBGO(hxisgdevtid::SFFRowData& inrow) {
  if (inrow.m_flag_fastbgo[0] != 0) return true;
  if (inrow.m_flag_fastbgo[1] != 0) return true;
  return false;
}

// ****************************************************************************

void prReco(sgdevtidlib::OutputRowData& outrow) {
  long long tot_occ = 0;              // Number of occurrences
  long long tot_rec = 0;              // Number of occurrences with reconstruction
  long long tot_no_rec = 0;           // Number of occurrences without reconstruction
  long long tot_trivial = 0;          // Number of trivial reconstructions (1 signal) 
  long long tot_nontriv = 0;          // Number of nontrivial reconstructions (>1 signal) 
  long long tot_success = 0;          // Number of non-trivial reconstructions that succeed
  long long tot_failure = 0;          // Number of non-trivial reconstructions that fail
  long long tot_escape_attempted = 0; // Number of escape energy calculations
  long long tot_escape_resolved = 0;  // Number of escape energy calculations that
                                      //   result in successful reconstruction
  AH_INFO(ahlog::HIGH) << "Histogram of RECO_STATUS bits: " << std::endl;
  for (int i=0; i < hxisgdevtid::SGD_SIZE_RECO_STATUS; ++i) {
    std::stringstream ss;
    ss.str("");
    ss << "     Bit " << std::setw(2) << i << ":  " 
       << std::setw(44) << std::left << sgdevtidlib::reco_string[i] 
       << std::setw(10) << std::right << outrow.m_reco_hist[i];
    AH_INFO(ahlog::HIGH)  << ss.str() << std::endl;
    if (i >= sgdevtidlib::BIT_MIN_NO_RECO && i <= sgdevtidlib:: BIT_MAX_NO_RECO) {
      tot_occ += outrow.m_reco_hist[i];
      tot_no_rec += outrow.m_reco_hist[i];
    }
    if (i >= sgdevtidlib::BIT_MIN_RECO_FAILED && i <= sgdevtidlib:: BIT_MAX_RECO_FAILED) {
      tot_failure += outrow.m_reco_hist[i];
      tot_nontriv += outrow.m_reco_hist[i];
      tot_rec += outrow.m_reco_hist[i];
      tot_occ += outrow.m_reco_hist[i];
    }
    if (i >= sgdevtidlib::BIT_MIN_RECO_TRIVIAL && i <= sgdevtidlib:: BIT_MAX_RECO_TRIVIAL) {
      tot_trivial += outrow.m_reco_hist[i];
      tot_rec += outrow.m_reco_hist[i];
      tot_occ += outrow.m_reco_hist[i];
    }
    if (i >= sgdevtidlib::BIT_MIN_RECO_OK && i <= sgdevtidlib:: BIT_MAX_RECO_OK) {
      tot_success += outrow.m_reco_hist[i];
      tot_nontriv += outrow.m_reco_hist[i];
      tot_rec += outrow.m_reco_hist[i];
      tot_occ += outrow.m_reco_hist[i];
    }
    if (i == sgdevtidlib::RECO_GOTO_ESCAPE_CALC_VIA_F_FAILURE ||
        i == sgdevtidlib::RECO_GOTO_ESCAPE_CALC_VIA_G_FAILURE ||
        i == sgdevtidlib::RECO_GOTO_ESCAPE_CALC_VIA_PROBSEQ_FAILURE) {
      tot_escape_attempted += outrow.m_reco_hist[i];
    }
    if (i == sgdevtidlib::RECO_SINGULARITY_IN_ESCAPE_CALC || i == sgdevtidlib::RECO_PERFORM_ESCAPE_CALC) 
      tot_escape_resolved += outrow.m_reco_hist[i];
  }
  AH_INFO(ahlog::HIGH) << "SUMMARY OF RECO_STATUS COUNTS" << std::endl;
  AH_INFO(ahlog::HIGH) << "Occurrences processed:        " << tot_occ << std::endl;
  AH_INFO(ahlog::HIGH) << "   No reconstruction attempted:   " << tot_no_rec << std::endl;
  AH_INFO(ahlog::HIGH) << "   Reconstruction attempted:      " << tot_rec << std::endl;
  AH_INFO(ahlog::HIGH) << "      Trivial reconstruction (1 signal):        " << tot_trivial << std::endl;
  AH_INFO(ahlog::HIGH) << "      Multiple-signal reconstruction attempted: " << tot_nontriv << std::endl;
  AH_INFO(ahlog::HIGH) << "                                        Succeeded: " << tot_success << std::endl;
  AH_INFO(ahlog::HIGH) << "                                           Failed: " << tot_failure << std::endl;
  AH_INFO(ahlog::HIGH) << "Escape energy calculations attempted:                      " 
    << tot_escape_attempted << std::endl;
  AH_INFO(ahlog::HIGH) << "Escape energy calculations completed or found impossible:  " 
    << tot_escape_resolved << std::endl;
}

// ****************************************************************************

void prSeq(sgdevtidlib::RowSignals & signals, sgdevtidlib::Hits & hits) { 

// print sequence information if occurrence_id set

  int k_sel = 0;
  int n_perm = 0;
  const int* pr = sgdevtidlib::selectPermArray(hits.m_m, n_perm);

  if (hits.m_best_k < 0) {
    AH_INFO(ahlog::HIGH) << "No best_k value (set negative); permutation 0 printed" << std::endl;
    k_sel = 0;
  } else {
    AH_INFO(ahlog::HIGH) << "best_k (favored premutation) = " << hits.m_best_k << std::endl;
    k_sel = hits.m_best_k;
  }

  AH_INFO(ahlog::HIGH) << "Hit sequence for permutation " << k_sel << ":" << std::endl;
  AH_INFO(ahlog::HIGH) << "#  RIM    SENSOR    EPI     CAMERAX     CAMERAY     CAMERAZ          DX          DY          DZ" << std::endl;
  for (int j=0; j < hits.m_m; j++) {
    int p = pr[hits.m_m*k_sel+j];          // Permuted index for j
    int hp=hits.m_hitarray[p];                     // Permuted index in signals
    int pn = pr[hits.m_m*k_sel+j+1];       // Permuted index for j
    int hpn=hits.m_hitarray[pn];                   // Permuted index in signals

    // distances to next point in sequence
    double dx=0.;
    double dy=0.;
    double dz=0.;
    if (j != hits.m_m-1) {
      dx=signals.m_camerax[hpn]-signals.m_camerax[hp];
      dy=signals.m_cameray[hpn]-signals.m_cameray[hp];
      dz=signals.m_cameraz[hpn]-signals.m_cameraz[hp];
    }

    std::stringstream msg;
    msg << std::setw(7) << signals.m_readout_id_rmap[hp]
        << std::setw(8) << signals.m_sensor_id[hp]
        << std::setw(8) << hits.m_epiarray[p]
        << std::setw(12) << signals.m_camerax[hp]
        << std::setw(12) << signals.m_cameray[hp]
        << std::setw(12) << signals.m_cameraz[hp]
        << std::setw(12) << dx
        << std::setw(12) << dy
        << std::setw(12) << dz
        << std::endl;
    AH_INFO(ahlog::HIGH) << msg.str();
  }
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: sgdevtid.cxx,v $
 Revision 1.95  2016/04/18 20:08:27  rshill
 Added status bits section to prologue.

 Revision 1.94  2016/04/13 21:32:29  rshill
 Added progress indicators.

 Revision 1.93  2016/04/08 17:48:34  mdutka
 adding caseinsesitivity to checks on instrume and detnam

 Revision 1.92  2016/04/07 20:48:32  mdutka
 Moving high volumne log statements to debug

 Revision 1.91  2016/03/18 22:26:44  rshill
 Changed misplaced label of DISTANCE0 column in output file.

 Revision 1.90  2016/03/07 23:05:02  rshill
 Added TLMIN and TLMAX to MATTYPE and CAMERA[XYZ] output columns.

 Revision 1.89  2015/12/09 15:44:18  mwitthoe
 sgdevtid: fix typo in log -> COUHTS to COUNTS

 Revision 1.88  2015/11/17 18:57:00  mwitthoe
 sgdevtid: copy all keywords from input file to output files

 Revision 1.87  2015/10/30 17:55:15  rshill
 Added datamode parameter to override datamode in event file;
 deleted check for value=*NORMALn.  For conformity with hxisgdpha.

 Revision 1.86  2015/09/04 16:04:13  rshill
 Added parameter stamping to log.

 Revision 1.85  2015/08/18 22:26:00  rshill
 Clarified counts summaries.

 Revision 1.84  2015/08/13 00:40:54  rshill
 Additional logging output and counts.

 Revision 1.83  2015/08/10 22:56:32  rshill
 Code cleanup: +++ cleanup, logging, comments for output table columns.

 Revision 1.82  2015/08/07 02:56:13  rshill
 Added DISTANxx being a minimum value for the corresponding parameter.

 Revision 1.81  2015/07/15 18:43:35  klrutkow
 added CALDB queries with ahmission query

 Revision 1.80  2015/04/28 20:25:47  mwitthoe
 sgdevtid: fix bug where escape energy was not included in event energy in FOV lookup routine; remove obsolete +++ comments

 Revision 1.79  2015/04/06 19:38:32  rshill
 Corrected a comment.

 Revision 1.78  2015/04/03 18:19:40  mwitthoe
 sgdevtid: update boolean parameters

 Revision 1.77  2015/04/01 19:31:11  mwitthoe
 sgdevtid: fix warning from 32-bit linux compiler

 Revision 1.76  2015/04/01 18:01:49  mwitthoe
 sgdevtid: remove datamode variable which is no longer used

 Revision 1.75  2015/03/19 22:21:54  rshill
 Changed PROBABILITY output column to LIKELIHOOD.

 Revision 1.74  2015/03/18 19:58:21  asargent
 Changed DETNAME to DETNAM

 Revision 1.73  2015/03/16 18:15:22  mwitthoe
 sgdevtid: throw error if strangepix parameter is set to 2 since that behavior is not yet defined

 Revision 1.72  2015/03/16 18:00:24  mwitthoe
 sgdevtid: add strangepix parameter which specifies how 'strange' signals should be treated

 Revision 1.71  2015/03/03 18:37:33  mwitthoe
 sgdevtid: update after Feb 2015 Japan meeting: store delta-F in F test; remove randomization in fluorescence merging; remove expand mode; add energy-dependent energy uncertainties

 Revision 1.67  2015/01/23 17:51:08  mwitthoe
 sgdevtid: use sequence probabilities instead of FOV probabilities in the figure-of-merit calculation

 Revision 1.66  2015/01/23 17:06:17  mwitthoe
 sgdevtid: 1) disable buffering for the FOV probability file since file is not read sequentially; 2) remove useless check on number of hits in FOV lookup function

 Revision 1.65  2015/01/22 21:55:23  mwitthoe
 sgdevit: implement FOV probability; see issue 482

 Revision 1.64  2015/01/12 20:57:14  mwitthoe
 sgdevtid: only create outtracefile if expand=no; in expand mode, do not require number of input rows to equal number of output rows

 Revision 1.63  2014/12/31 15:53:09  rshill
 Corrected bug in copying PROC_STATUS from input to output.

 Revision 1.62  2014/12/30 22:06:20  rshill
 Corrected treatment of null EPI and PI.

 Revision 1.61  2014/12/30 19:04:57  rshill
 Deleted unused function prototype hadBadStatus.

 Revision 1.60  2014/12/30 18:20:05  rshill
 Incorporated occurrence-by-occurrence datamode requirements.

 Revision 1.59  2014/12/24 20:53:40  rshill
 Modified for parameter standards update

 Revision 1.58  2014/12/24 18:31:33  rshill
 Updated for parameter standards (Redmine issue #472)

 Revision 1.57  2014/12/17 22:25:07  rshill
 Fix build error due to new badpix::load.

 Revision 1.56  2014/11/13 20:47:50  rshill
 Reconciled with new SFFa template.

 Revision 1.55  2014/09/15 20:46:10  mwitthoe
 sgdevtid: add support for extended syntax; issue 179

 Revision 1.54  2014/09/15 17:19:06  mwitthoe
 sgdevtid: add MATTYPE column to output, to be used in gain fitting; see issue 432

 Revision 1.53  2014/08/11 21:00:35  mwitthoe
 sgdevtid: fix doxygen

 Revision 1.52  2014/07/24 17:40:14  mwitthoe
 sgdevtid: update probm CALDB file format; bug-fix: there were two expand variables being used; bug-fix: there was a mix-up between layer and sensor index when determining location of a signal; bug-fix: extrainfo was not being checked before trying to write the extra output file in sgdevtidlib

 Revision 1.51  2014/06/24 17:50:26  rshill
 Split loading SGD geom keywords into a separate call.

 Revision 1.50  2014/06/13 00:39:45  rshill
 Geometry in remap header.  expand parameter and
 outfileextra=NONE.  Corrected error computations.

 Revision 1.49  2014/05/09 19:53:26  mwitthoe
 sgdevtid: update tool to use new versions of the common CALDB accaccess routines used also by hxievtid (remap, fluor, and badpix)

 Revision 1.48  2014/03/27 19:52:31  rshill
 Moved printing of sequence summary to a function called prSeq,
 which is now invoked in both "if (write_row)" blocks if occurrence_id
 is specified as a user parameter.

 Revision 1.47  2014/03/21 18:14:33  mwitthoe
 sgdevtid: in probm CALDB file 1) change type of probhit from int to double, 2) rename columns to match test file sent by Hiro on Mar 20; add best_k to hits structure; print hit sequence to log file (with AH_DEBUG) when occurrence_id is used; initialize test_f/g/prob to false instead of true

 Revision 1.46  2014/03/19 15:33:28  mwitthoe
 sgdevtid: fix sign error in Delta-cos(theta_k); change F-test to exclude Delta-F

 Revision 1.45  2014/03/14 21:55:04  mwitthoe
 sgdevtid: ensure that all output columns are written to; update RECO_STATUS values

 Revision 1.44  2014/03/13 21:03:43  mwitthoe
 sgdevtid: add random selection for Step 1a-2; change FOM expression; include extension name in CALDB lookup function for the badpix file; if single F test passes with M>2, continue to G test; add lookup function for ProbM file to return the MECHANISM index which gets output in the SEQHITS column; print number of occurrences which have PI=NULL to end of log file; remove some old, commented-out code

 Revision 1.43  2014/03/11 21:53:09  mwitthoe
 sgdevtid: only include signals above threshold in the PI calculation

 Revision 1.42  2014/03/11 20:24:37  mwitthoe
 sgdevtid: completed SGD items 3 & 4 listed in issue #360; these items involved adding random selection in steps 1-0, 1a-1a, 1a-1b, and tie-breaking; now take absolute value of delta-g in the G test; fixed bug in FOM calculation which was omitting the escape energy

 Revision 1.41  2014/03/05 14:03:50  mwitthoe
 sgdevtid: add a couple more columns to the extra output file to keep track of when merging occurred and the cluster shape (step 1-0); fix bug where total EPI was not calculated properly; in escape loop, automatically fail F test if escape energy is negative; if the calculated PI is out-of-range, set it to the max value: 2048

 Revision 1.40  2014/03/04 14:10:18  mwitthoe
 sgdevtid: change algorithm for Steps 1-0 and 1a-3; in Step 1-0, certain clusters up to size 5 are allowed; in Step 1a-3, up to 3 signals can now be merged (instead of only 2)

 Revision 1.39  2014/03/01 17:22:55  mwitthoe
 sgdevtid: update tool based on testing results on 2/28 between Hiro, Ichinohe, and MCW; the updates include bug fixes and algorithm changes; details are in the Japan notes under Build 5/Sprint 1 on the redmine wiki

 Revision 1.38  2014/02/21 20:51:42  rshill
 Fixed failure to propagate variances through signal merges;
 added occurrence_id parameter to process one selected row.

 Revision 1.37  2014/02/20 22:58:40  rshill
 Bug fixes in writing output rows.

 Revision 1.36  2014/02/15 00:35:41  rshill
 Bugfixes: G test subscripting; for-loop initialization.

 Revision 1.35  2014/02/13 13:11:56  rshill
 Corrected some printed row counts.

 Revision 1.33  2014/02/12 23:31:07  rshill
 Made some progress debugging.

 Revision 1.32  2014/02/12 01:09:44  rshill
 Debugging in progress.

 Revision 1.31  2014/02/07 01:06:50  rshill
 Debugged several FITS in output and extra output files.

 Revision 1.30  2014/02/06 00:42:15  rshill
 Fixed extra output handling and delta E.

 Revision 1.29  2014/02/05 01:05:48  rshill
 Corrected bug in EPI initialization.

 Revision 1.28  2014/02/05 00:55:15  rshill
 Added a call to setOutputEventNull for case of one low signal.

 Revision 1.27  2014/02/05 00:45:15  rshill
 Updated RECO_STATUS handling.

 Revision 1.26  2014/02/03 22:15:25  rshill
 Greatly simplified flow on return from functions;
 rationalized RECO_STATUS bit assignments.

 Revision 1.25  2014/02/03 19:14:04  rshill
 Partway through reconciliation with TRF.  Still builds.

 Revision 1.24  2014/02/03 02:41:46  rshill
 Removed needless string conversions.

 Revision 1.23  2014/02/03 00:36:31  rshill
 Debugging continues; still builds.

 Revision 1.22  2014/02/02 22:56:16  rshill
 Partway through debugging, but builds.

 Revision 1.21  2014/01/30 20:42:13  rshill
 Added in actually using the probm CALDB file.

 Revision 1.20  2014/01/30 03:51:52  rshill
 Revised for closer TRF conformance.

 Revision 1.19  2014/01/29 22:12:53  rshill
 Interim version with callouts from TRF meeting.

 Revision 1.18  2014/01/29 01:41:49  rshill
 Completed some version of all routines

 Revision 1.17  2014/01/28 23:39:53  rshill
 Revsions following walkthrough with MW.

 Revision 1.16  2014/01/28 20:31:46  rshill
 Building output row data; improved set of values for condition.

 Revision 1.15  2014/01/27 23:58:32  rshill
 Added extra output file management.  Improved permutation
 management.  Moved many declarations from sgdevtid to sgdevtidlib.

 Revision 1.14  2014/01/25 01:37:07  rshill
 Brought closer to TRF with differences called out.

 Revision 1.13  2014/01/24 19:47:42  rshill
 Major restructuring.  Factored out the steps in the TRF into separate
 functions in another source file.  Control and status information
 passed via a condition code rather than being directly acted upon
 using goto statements.

 Revision 1.12  2014/01/16 22:22:02  rshill
 Includes comments following walkthrough by RSH & MW.

 Revision 1.11  2014/01/16 00:47:49  rshill
 Partial cleanup after walkthrough with MW

 Revision 1.10  2014/01/15 18:36:05  rshill
 Correction of compilation errors in progress.

 Revision 1.9  2014/01/15 16:24:21  rshill
 Continued file processing;
 added extra output file.

 Revision 1.8  2014/01/14 00:30:11  rshill
 Started on the file formatting/reading/writing.

 Revision 1.7  2014/01/10 22:57:32  rshill
 Added functions to compute thetaG and error.Needs cleanup and reading/writing files.

 Revision 1.6  2014/01/10 01:04:57  rshill
 Starting to take shape now.  Still not finished, however.

 Revision 1.5  2014/01/09 23:12:23  rshill
 Basic structure in place; many details to be fixed.

 Revision 1.4  2014/01/08 01:52:54  rshill
 Need to finish G test and escape energy calculation.

 Revision 1.3  2014/01/08 00:09:20  rshill
 Further along on initial coding -- about to start the G test.

 Revision 1.2  2014/01/04 01:05:58  rshill
 A slightly more developed (but still very rough) version
 uploaded mainly for backup.

 Revision 1.1  2013/12/28 00:29:35  rshill
 First checkin - woefully inchoate and incomplete.



*/

