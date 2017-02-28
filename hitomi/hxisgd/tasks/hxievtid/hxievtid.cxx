/// \file hxievtid.cxx
/// \brief Reconstruct HXI events
/// \author Mike Witthoeft
/// \date $Date: 2016/04/18 18:43:38 $
/// \version 1.0

/** 

\defgroup tool_hxievtid Reconstruct HXI events (hxievtid)
@ingroup mod_hxisgd_tasks

Reconstruct events from the HXI SFF.  The reconstruction method employed 
depends on the DATAMODE keyword value in the input file.

Source files:

  hxievtid.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission
  astroh/hxisgd/lib/hxisgdevtid

Status bits changed:

PROC_STATUS - not changed
STATUS - not changed

RECO_STATUS - Bit field written for each occurrence (SFFa row):
 Bit  Description
 ------(Occurrence mode identification)-------------------------------
  0    Mode is Am 241
  1    Mode is PSEUDO
  2    Mode is CALMODE 
  3    Mode is READALL
  4    Mode is NOTSURE
 ------(Event reconstruction skipped for this occurrence)-------------
  5    Bad PROC_STATUS
  6    Mode is READALL or CALMODE and SKIPRECO option is YES
  7    Occurrence has no signals
  8    FASTBGO or HITPAT flag is set and REJECTBGO option is YES
 ------(Event reconstruction outcome)---------------------------------
  9    Event could not be reconstructed

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-12   MCW    Clean-up code

*/
 
#define AHLABEL tool_hxievtid
#define AHCVSID "$Id: hxievtid.cxx,v 1.60 2016/04/18 18:43:38 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "hxisgdevtid/hxisgdevtid.h"
#include "hxisgdevtid/remap.h"
#include "hxisgdevtid/badpix.h"
#include "hxisgdevtid/fluor.h"

#include "ahapp/ahapp.h"
#include "ahgen/ahgen.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahmission/spline.h"
#include "ahmission/keyword.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <string>
#include <sstream>
#include <cstdlib>       // std::abs


/** \addtogroup tool_hxievtid
 *  @{
 */

// constants defining array sizes
const int SIZE_STATUS=8;              ///< num bits in STATUS column
const int SIZE_PROC_STATUS=32;        ///< num bits in PROC_STATUS column
const int SIZE_FLAG_SEU=5;            ///< num bits in FLAG_SEU column
const int SIZE_FLAG_LCHK=5;           ///< num bits in FLAG_LCHK column
const int SIZE_FLAG_TRIG=8;           ///< num bits in FLAG_TRIG column
const int SIZE_FLAG_TRIGPAT=8;        ///< num bits in FLAG_TRIGPAT column
const int SIZE_FLAG_HITPAT=2;         ///< num bits in FLAG_HITPAT column
const int SIZE_FLAG_FASTBGO=2;        ///< num bits in FLAG_FASTBGO column
const int SIZE_CHANNEL=1280;          ///< num elements in READOUT_ID_RMAP and EPI columns

const int NLAYER=5;                   ///< number of HXI layers
const int NSIDE=10;                   ///< number of HXI sides (2 sides per layer)
const int NSIGNALPERSIDE=128;         ///< number of signals per HXI side

/// \brief structure to hold input parameters
struct Par {

  Par(): m_occurrenceid(0), m_rejectbgo(false), m_skipreco(false) {}

  std::string m_infile;             ///< Input FITS file name
  std::string m_outfile;            ///< Output FITS file name
  std::string m_remapfile;          ///< FITS file to convert READOUT_ID_RMAP to RAWX, RAWY, and LAYER
  std::string m_fluorefile;         ///< FITS file containing energy ranges for fluorescence
  std::string m_badpixfile;         ///< FITS file containing energy thresholds
  std::string m_enecutfile;         ///< FITS file containing energy cut coefficients
  long long m_occurrenceid;         ///< If >= 0, process only occurrence with this ID
  bool m_rejectbgo;                 ///< Reject events for which BGO trigger occurred
  std::string m_outcalfile;         ///< Signal event calibration output file
  std::string m_datamode;           ///< Substitute DATAMODE in place of event value (or NONE)
  bool m_skipreco;                  ///< Do not reconstruct READALL & CALMODE events
};


/// \brief structure to hold a single row of data of the output SFFa file; 
///  columns referring to either a bit type or a variable-length array, require
///  two members in the struct: one for data and one for the number of elements.
///  Columns which can be set to NULL also require a separate member.  This
///  structure is also used to store data for the extra output file (containing
///  calibration information), however, not all elements of the struct are
///  needed.
struct OutputRowData {

  OutputRowData(): m_occurrence_id(0), m_evtcat(0), m_layer(0), m_layer_null(0),
                   m_readout_id_index(0), m_readout_id_index_null(0), m_rawx(0),
                   m_rawx_null(0), m_rawy(0), m_rawy_null(0), m_ene_total(0.),
                   m_ene_total_null(0), m_pi(0), m_pi_null(0),
                   num_reco_status(0), m_actx(0), m_actx_null(0),
                   m_acty(0), m_acty_null(0), m_focx(0), m_focx_null(0), m_focy(0),
                   m_focy_null(0), m_x(0), m_x_null(0), m_y(0), m_y_null(0),
                   m_side(0) {
    for (ahfits::IndexType ii=0; ii < NSIDE; ii++) m_goodbad[ii]=0;
    for (ahfits::IndexType ii=0; ii < NSIDE; ii++) m_signal[ii]=0;
    for (ahfits::IndexType ii=0; ii < NSIDE; ii++) m_sigpos[ii]=0;
    for (ahfits::IndexType ii=0; ii < NSIDE; ii++) m_sigepi[ii]=0.;
    for (ahfits::IndexType ii=0; ii < NLAYER; ii++) m_epitop[ii]=0.;
    for (ahfits::IndexType ii=0; ii < NLAYER; ii++) m_epibot[ii]=0.;
    for (ahfits::IndexType ii=0; ii < NLAYER; ii++) m_epicut[ii]=0;
    for (ahfits::IndexType ii=0; ii < NLAYER; ii++) m_validhits[ii]=0;
  }

  // The following columns are copied from the input file without modification:
  long m_occurrence_id;                        ///< OCCURRENCE_ID column - 1J

  // The remaining columns are computed by this task:
  int m_evtcat;                                ///< EVTCAT column - 1B
  int m_layer;                                 ///< LAYER column - 1B
  char m_layer_null;                           ///< LAYER NULL flag; 0=not NULL, 1=NULL
  int m_readout_id_index;                      ///< READOUT_ID_INDEX column - 1I
  char m_readout_id_index_null;                ///< READOUT_ID_INDEX NULL flag; 0=not NULL, 1=NULL
  int m_rawx;                                  ///< RAWX column - 1I
  char m_rawx_null;                            ///< RAWX NULL flag; 0=not NULL, 1=NULL
  int m_rawy;                                  ///< RAWY column - 1I
  char m_rawy_null;                            ///< RAWY NULL flag; 0=not NULL, 1=NULL
  double m_ene_total;                          ///< ENE_TOTAL column - 1D
  char m_ene_total_null;                       ///< ENE_TOTAL NULL flag; 0=not NULL, 1=NULL
  int m_pi;                                    ///< PI column - 1I
  char m_pi_null;                              ///< PI NULL flag; 0=not NULL, 1=NULL
  char m_reco_status[hxisgdevtid::HXI_SIZE_RECO_STATUS];        ///< RECO_STATUS column - 3X
  ahfits::IndexType num_reco_status;           ///< number of RECO_STATUS bits
  int m_goodbad[NSIDE];                        ///< GOODBAD column - 10I

  // The next arrays are dimensioned to the number of sides in the HXI 
  // instrument; 10 sides (2 per each of 5 layers).  m_signal stores how many
  // signals are found for each side, m_sidpos gives the position of the hit
  // on each side, and m_sigepi gives the sum of the signal EPI values.
  int m_signal[NSIDE];                         ///< SIGNAL column - 10I
  int m_sigpos[NSIDE];                         ///< SIGPOS column - 10I
  double m_sigepi[NSIDE];                      ///< SIGEPI column - 10D

  // Arrays to hold information related to the energy cut.  Each array is
  // dimensioned per layer.  The EPITOP and EPIBOT arrays will contain the
  // same values as stored in the SIGEPI column.  The EPICUT column gives
  // the result of the energy cut test: 0=okay; 1=p-side energy too high;
  // 2=p-side eneregy too low; 3=no signals; 4=at least one signal in layer
  // but one or both sides are invalid (no signal, NULL, below threshold).
  double m_epitop[NLAYER];                    ///< EPITOP column - 5E
  double m_epibot[NLAYER];                    ///< EPIBOT column - 5E
  int m_epicut[NLAYER];                       ///< EPICUT column - 5I
  char m_epicut_null[NLAYER];                 ///< EPICUT NULL flags

  // This array stores a status code for the kind of hit found for each of
  // 5 layers: 0: layer not processed, 1: good hit, 2: bad hit.
  int m_validhits[NLAYER];                     ///< VALIDHITS column - 5I

  // These columns are to be filled by a later task.  Their values are only set
  // to 0 or NULL by this task.
  int m_actx;                                  ///< ACTX column - 1I
  char m_actx_null;                            ///< ACTX NULL flag; 0=not NULL, 1=NULL
  int m_acty;                                  ///< ACTY column - 1I
  char m_acty_null;                            ///< ACTY NULL flag; 0=not NULL, 1=NULL
  int m_detx;                                  ///< DETX column - 1I
  char m_detx_null;                            ///< DETX NULL flag; 0=not NULL, 1=NULL
  int m_dety;                                  ///< DETY column - 1I
  char m_dety_null;                            ///< DETY NULL flag; 0=not NULL, 1=NULL
  int m_focx;                                  ///< FOCX column - 1I
  char m_focx_null;                            ///< FOCX NULL flag; 0=not NULL, 1=NULL
  int m_focy;                                  ///< FOCY column - 1I
  char m_focy_null;                            ///< FOCY NULL flag; 0=not NULL, 1=NULL
  int m_x;                                     ///< X column - 1I
  char m_x_null;                               ///< X NULL flag; 0=not NULL, 1=NULL
  int m_y;                                     ///< Y column - 1I
  char m_y_null;                               ///< Y NULL flag; 0=not NULL, 1=NULL

  // This column is only used for the extra output file containing calibration
  // data; it will be left empty for the regular output file
  int m_side;                                  ///< SIDE column - 1B (outcalfile only)

};

/// \brief This structure holds data needed for a single layer from a single 
///  occurrence (single row).
struct SignalDataLayer {

  SignalDataLayer(): m_nsigx(0), m_nsigy(0), m_nsig(0), m_nbelowthreshx(0),
                     m_nnullx(0), m_nokayx(0), m_nbelowthreshy(0), m_nnully(0),
                     m_nokayy(0), m_validhit(true), m_hitx(0), m_hity(0),
                     m_hitpi(0) {
    for (int ii=0; ii < NSIGNALPERSIDE; ii++) {
      m_rawx[ii]=0;
      m_epix[ii]=0.;
      m_statusx[ii]=false;
      m_rawy[ii]=0;
      m_epiy[ii]=0.;
      m_statusy[ii]=false;
    }
  }

  int m_nsigx;                         ///< number of signals in X side
  int m_nsigy;                         ///< number of signals in Y side
  int m_nsig;                          ///< total number of signals

  int m_rawx[NSIGNALPERSIDE];          ///< list of RAWX values for X side
  double m_epix[NSIGNALPERSIDE];       ///< list of EPI values for X side
  bool m_statusx[NSIGNALPERSIDE];      ///< list of STATUS values for X side
  int m_nbelowthreshx;                 ///< number of signals below threshold; X side
  int m_nnullx;                        ///< number of NULL signals; X side
  int m_nokayx;                        ///< number of okay signals; X side

  int m_rawy[NSIGNALPERSIDE];          ///< list of RAWY values for Y side
  double m_epiy[NSIGNALPERSIDE];       ///< list of EPI values for Y side
  bool m_statusy[NSIGNALPERSIDE];      ///< list of STATUS values for Y side
  int m_nbelowthreshy;                 ///< number of signals below threshold; Y side
  int m_nnully;                        ///< number of NULL signals; Y side
  int m_nokayy;                        ///< number of okay signals; Y side

  bool m_validhit;                     ///< true if signals correspond to a hit

  int m_hitx;                          ///< Hit: RAWX
  int m_hity;                          ///< Hit: RAWY
  double m_hitpi;                      ///< Hit: PI

};

/// \brief This structure to hold signal data for each layer from a single
///  occurrence (single row).
struct SignalDataRow {

  SignalDataLayer m_layerdat[NLAYER];    ///< signal data split over layer
};


/// \brief Get all parameter values.
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief 1) Open and check input file; 2) load data from all CALDB files;
///  3) create empty output file.
/// \param[in] par               structure with parameter values
/// \param[out] fpin             ahfits file pointer to infile
/// \param[out] fpout            ahfits file pointer to outfile
/// \param[out] fpextra          ahfits file pointer to outcalfile
/// \param[out] alive_asic       value of ALIVE_ASIC column from badpix file
/// \param[out] remap_table      Data from remapfile 
/// \param[out] epithre_table    Data from badpixfile; event thresholds
/// \param[out] fluor_table      Data from fluorescence CALDB file
/// \param[out] lower_cut        Spline coefficients for lower energy cut
/// \param[out] upper_cut        Spline coefficients for upper energy cut
void initialize(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
                ahfits::FilePtr& fpextra, int& alive_asic,
                hxisgdevtid::remap::DataType& remap_table,
                hxisgdevtid::badpix::ThresholdTable& epithre_table,
                hxisgdevtid::fluor::DataType& fluor_table,
                ahmission::spline::SplineSetMap& lower_cut, 
                ahmission::spline::SplineSetMap& upper_cut);

/// \brief Perform event reconstruction algorithm.
/// \param[in] par               structure with parameter values
/// \param[in] fpin              ahfits file pointer to infile
/// \param[in] fpout             ahfits file pointer to outfile
/// \param[in] fpextra           ahfits file pointer to outcalfile
/// \param[in] alive_asic        value of ALIVE_ASIC column from badpix file
/// \param[in] remap_table       Data from remapfile 
/// \param[in] epithre_table     Data from badpixfile; event thresholds
/// \param[in] fluor_table       Data from fluorescence CALDB file
/// \param[in] lower_cut         Spline coefficients for lower energy cut
/// \param[in] upper_cut         Spline coefficients for upper energy cut
void doWork(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
            ahfits::FilePtr& fpextra, int alive_asic,
            hxisgdevtid::remap::DataType& remap_table, 
            hxisgdevtid::badpix::ThresholdTable& epithre_table, 
            hxisgdevtid::fluor::DataType& fluor_table,
            ahmission::spline::SplineSetMap& lower_cut, 
            ahmission::spline::SplineSetMap& upper_cut);

/// \brief Close FITS files; clear allocated memory.
/// \param[in] fpin              ahfits file pointer to infile
/// \param[in] fpout             ahfits file pointer to outfile
/// \param[in] fpextra           ahfits file pointer to outcalfile
/// \param[in] remap_table       Data from remapfile 
void finalize(ahfits::FilePtr fpin, ahfits::FilePtr fpout, ahfits::FilePtr fpextra,
              hxisgdevtid::remap::DataType & remap_table);

/// \brief Create output file.
/// \param[in] fpsrc ahfits file pointer to EVENTS extension of input file
/// \param[in] outfile name of output file
/// \param[out] fpdest output ahfits file pointer to output file
void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      ahfits::FilePtr& fpdest);

/// \brief Create extra output file which contains calibration information.
/// \param[in] fpsrc ahfits file pointer to EVENTS extension of input file
/// \param[in] outfile name of output file
/// \param[out] fpdest output ahfits file pointer to output file
void createExtraOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      ahfits::FilePtr& fpdest);

/// \brief Initialize output row data and copy values from input row.  This
///  function will be used to prepare output data for the SFFa file and the
///  extra output file with calibration data.
/// \param[in] inrow input row data
/// \param[in] outrow output row data
void prepareOutputData(hxisgdevtid::SFFRowData& inrow, OutputRowData& outrow);

/// \brief Set RECO_STATUS bit for Am241 mode
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusAm241(char* reco_status);

/// \brief Set RECO_STATUS bits for occurrence type
/// \param[in] mode e_CAMERA, e_AM241, e_PSEUDO, e_CALMODE, e_READALL, or e_NOTSURE
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusMode(int mode, char* reco_status);

/// \brief Set bad status bit of RECO_STATUS
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusBadStatus(char* reco_status);

/// \brief Set fast BGO bit of RECO_STATUS
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusFastBGO(char* reco_status);

/// \brief Set bit of RECO_STATUS for READALL or CALMODE occurrences which are
///  skipped.
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusSkipReco(char* reco_status);

/// \brief Set bad-reconstruction bit of RECO_STATUS.
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusBadReco(char* reco_status);

/// \brief Set no-signal bit of RECO_STATUS.
/// \param[in,out] reco_status RECO_STATUS array of bits
void setRecoStatusNoSignal(char* reco_status);

/// \brief Return true if there are any non-zero bits in FLAG_HITPAT.
/// \param[in] inrow input row data
/// \return true if FLAG_HITPAT has any non-zero bit
bool hasNonZeroHitPat(hxisgdevtid::SFFRowData& inrow);

/// \brief Return true if there are any non-zero bits in FLAG_FASTBGO.
/// \param[in] inrow input row data
/// \return true if FLAG_FASTBGO has any non-zero bit
bool hasNonZeroFastBGO(hxisgdevtid::SFFRowData& inrow);

/// \brief Set appropriate output columns to NULL to indicate invalid event.
/// \param[out] outrow struct with output data for single row
void setOutputToNull(OutputRowData& outrow);


// ****************************************************************************

/// \brief sxspi tool
int main(int argc, char** argv) {

  // variable to hold parameter values
  Par par;      

  // FITS file pointers
  ahfits::FilePtr fpin=0;        // ahfits file structure for input file
  ahfits::FilePtr fpout=0;       // ahfits file structure for output file
  ahfits::FilePtr fpextra=0;     // ahfits file structure for extra output file (calibration data)

  // variables to store CALDB data
  hxisgdevtid::remap::DataType remap_table;             // convert READOUT_ID_RMAP to RAWX, RAWY, and LAYER
  hxisgdevtid::badpix::ThresholdTable epithre_table;    // EPI threshold values
  hxisgdevtid::fluor::DataType fluor_table;             // Fluorescence energies
  int alive_asic=0;                                     // ALIVE_ASIC value from badpix CALDB file
  ahmission::spline::SplineSetMap lower_cut;            // spline coefficients for lower energy cut
  ahmission::spline::SplineSetMap upper_cut;            // spline coefficients for upper energy cut

  int status = ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par,fpin,fpout,fpextra,alive_asic,remap_table,epithre_table,
                 fluor_table,lower_cut,upper_cut);
      doWork(par,fpin,fpout,fpextra,alive_asic,remap_table,epithre_table,
             fluor_table,lower_cut,upper_cut);
      finalize(fpin,fpout,fpextra,remap_table);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fpin,fpout,fpextra,alive_asic,remap_table,epithre_table,
                   fluor_table,lower_cut,upper_cut);
        doWork(par,fpin,fpout,fpextra,alive_asic,remap_table,epithre_table,
               fluor_table,lower_cut,upper_cut);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpin,fpout,fpextra,remap_table);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_remapfile=ahapp::getParString("remapfile");
  par.m_fluorefile=ahapp::getParString("fluorefile");
  par.m_badpixfile=ahapp::getParString("badpixfile");
  par.m_enecutfile=ahapp::getParString("enecutfile");
  par.m_occurrenceid=ahapp::getParInt("occurrenceid");
  par.m_rejectbgo=ahapp::getParBool("rejectbgo");
  par.m_outcalfile=ahapp::getParString("outcalfile");
  par.m_datamode=ahapp::getParString("datamode");
  par.m_skipreco=ahapp::getParBool("skipreco");

  if (par.m_outcalfile.find_first_not_of(' ') == std::string::npos) {
    AH_THROW_RUNTIME("outcalfile cannot be blank or null: use NONE or give filename");
  }
}

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
                ahfits::FilePtr& fpextra, int& alive_asic,
                hxisgdevtid::remap::DataType& remap_table,
                hxisgdevtid::badpix::ThresholdTable& epithre_table,
                hxisgdevtid::fluor::DataType& fluor_table,
                ahmission::spline::SplineSetMap& lower_cut,
                ahmission::spline::SplineSetMap& upper_cut) {

  // local variables
  std::string instrume;                 // value of INSTRUME keyword from infile
  std::string detnam;                   // value of DETNAM keyword from infile
  std::string datamode;                 // value of DATAMODE keyword from infile
  std::string dateobs;                  // value of DATE-OBS keyword from infile
  std::string instrume_shrt;            // first three letters of INSTRUME keyword ("HXI")
  std::string actual_remap;             // name of remap file to read (result from CALDB query)
  std::string actual_fluorefile;        // name of fluorescence file to read (result from CALDB query)
  std::string actual_badpix;            // name of badpix file to read (result from CALDB query)
  std::string actual_enecut;            // name of energy cut file to read (result from CALDB query)

  // open input SFF file; check INSTRUME/DETNAM
  ahfits::open(par.m_infile,"",&fpin);
  if (ahfits::isPrimary(fpin)) ahfits::move(fpin,"EVENTS");  // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fpin,par.m_infile);
  instrume=ahfits::getKeyValStr(fpin,"INSTRUME");
  detnam=ahfits::getKeyValStr(fpin,"DETNAM");
  if (instrume != "HXI1" && instrume != "HXI2")
    AH_THROW_RUNTIME("input file EVENTS HDU has wrong INSTRUME value; should be HXI1 or HXI2");
  if (detnam != "CAMERA")
    AH_THROW_RUNTIME("input file EVENTS HDU has wrong DETNAM value; should be CAMERA");
  instrume_shrt=instrume.substr(0,3);
  
  // read DATAMODE from input file which is used to get bad pixel thresholds
  // from the badpix CALDB file; the DATAMODE will be of the form CAMERA_NORMAL#,
  // but only the NORMAL# will be retained (where # is a number).
  std::string ev_datamode=ahfits::getKeyValStr(fpin,"DATAMODE");
  if (!par.m_datamode.empty() && ahgen::strtoupper(par.m_datamode) != "NONE") {
    datamode=par.m_datamode;
    AH_INFO(ahlog::HIGH) << "DATAMODE in event file=" << ev_datamode << "; overridden with datamode parameter: " << par.m_datamode << std::endl;
  } else {
    datamode=ev_datamode;
    AH_INFO(ahlog::HIGH) << "DATAMODE taken from event file: " << ev_datamode << std::endl;
  }
  std::size_t found=datamode.rfind("_");   // locate underscore character
  if (found == std::string::npos) 
    AH_THROW_RUNTIME("expecting underscore character in input DATAMODE, e.g. CAMERA_NORMAL1");
  found++;                           // move start position of substring to after underscore
  datamode=datamode.substr(found);   // keep only part of datamode after underscore

  // For CALDB query
  dateobs=ahfits::getKeyValStr(fpin,"DATE-OBS");
  
  // Load data from remapping CALDB file.  This structure will provide mappings
  // from READOUT_ID_RMAP to RAWX, RAWY, and LAYER_INDEX.  Uses library in
  // astroh/hxisgd/lib/hxisgdevtidlib/remap.h
  // Note: the 2nd argument of load() ("HXI") checks that the given remap file 
  //       has INSTRUME=HXI
  actual_remap=ahmission::caldb::resolve(par.m_remapfile, "remapping", instrume_shrt, "-", "REMAPPING", dateobs);
  ape_trad_set_string("remapfile",actual_remap.c_str());   // to record actual file path in history
  hxisgdevtid::remap::load(actual_remap, "HXI", remap_table);

  // Load data from fluorescence CALDB file.  The structure will allow us to
  // check if an EPI corresponds to a fluorescence line by checking against the
  // energy ranges listed in the CALDB file.  Uses library in
  // astroh/hxisgd/lib/hxisgdevtidlib/fluor.h
  // Note: the 2nd argument of load() ("HXI") checks that the given fluorescence file 
  //       has INSTRUME=HXI
  actual_fluorefile=ahmission::caldb::resolve(par.m_fluorefile, "fluorescence", instrume_shrt, "-", "LINE_ENERGY", dateobs);
  ape_trad_set_string("fluorefile",actual_fluorefile.c_str());
  hxisgdevtid::fluor::load(actual_fluorefile,"HXI",fluor_table);

  // Load data from badpixel CALDB file.  The structure contains a EPI threshold
  // for each READOUT_ID_RMAP value.  Signals with an EPI value below these
  // thresholds are ignored.  Uses library in
  // astroh/hxisgd/lib/hxisgdevtidlib/badpix.h
  // Note: the 2nd argument of load() (instrume) checks that the given remap 
  //       file has INSTRUME=HXI
  // Note: the 3rd argument of load() ("CAMERA") locates the correct extension
  //       in the badpix file via DETNAM
  actual_badpix=ahmission::caldb::resolve(par.m_badpixfile, "bad pixel", instrume, detnam, "BADPIX", dateobs);
  ape_trad_set_string("badpixfile",actual_badpix.c_str());   // to record actual file path in history
  hxisgdevtid::badpix::loadThreshold(actual_badpix,instrume,"CAMERA",datamode,epithre_table,alive_asic);

  // Load data from energy cut CALDB file.  The structure contains spline
  // coefficients covering the EPI energy range for each layer.  
  // Note: the resolve function will return the extension for ECUT_LOWER,
  //       but spline::load() will go to the appropriate extension
  actual_enecut=ahmission::caldb::resolve(par.m_enecutfile, "energy cut", instrume, detnam, "ECUT_LOWER", dateobs);
  ape_trad_set_string("enecutfile",actual_enecut.c_str());   // to record actual file path in history
  ahmission::spline::load(actual_enecut,"LOWER","LAYER",instrume,"CAMERA",lower_cut);
  ahmission::spline::load(actual_enecut,"UPPER","LAYER",instrume,"CAMERA",upper_cut);

  // Create output file from scratch.  The output file will also copy the
  // following keyword values from the input file: TELESCOP, INSTRUME, DETNAM,
  // and DATAMODE.
  createOutputFile(fpin,par.m_outfile,fpout);

  // If the outcalfile parameter is not "NONE", then create the extra output
  // file from scratch.  The ahfits file pointer for the input file is provided
  // to supply keywords: TELESCOP, INSTRUME, DETNAM, and DATAMODE.
  if (ahgen::strtoupper(par.m_outcalfile) != "NONE") 
    createExtraOutputFile(fpin,par.m_outcalfile,fpextra);

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
            ahfits::FilePtr& fpextra, int alive_asic,
            hxisgdevtid::remap::DataType& remap_table, 
            hxisgdevtid::badpix::ThresholdTable& epithre_table, 
            hxisgdevtid::fluor::DataType& fluor_table,
            ahmission::spline::SplineSetMap& lower_cut,
            ahmission::spline::SplineSetMap& upper_cut) {

  // variable declarations
  std::string feature;              // name of feature detected (from fluorescence file)
  long long numrows=0;              // number of rows in input SFF file
  long long krow=0;                 // index of current row in input SFF file
  int mode=hxisgdevtid::e_NOTSURE;  // type of occurrence: CAMERA, AM241, CALMODE, READALL, PSEUDO, or NOTSURE
                                    // see Datamodes enum in hxisgdevtid library

  // variables to store row data from input and output files
  hxisgdevtid::SFFRowData inrow("HXI");
  OutputRowData outrow;
  OutputRowData extrarow;           // to be written to outcalfile (calibration data)

  // counters which get reported at end of log file
  long long count_rows=0;                  // number of rows read from input file
  long long count_badstatus=0;             // number of occurrences with a bad STATUS
  long long count_rejectbgo=0;             // number of occurrences rejected due to FAST_BGO or FLAG_HITPAT when rejectbgo parameter is true
  long long count_nohits=0;                // number of occurrences with no valid, reconstructed hits -> unsuccessful reconstruction
  long long count_toomanyhits=0;           // number of occurrences with too many hits for algorithm to handle -> unsuccessful reconstruction
  long long count_invalidhits=0;           // number of occurrences with 1 or 2 valid hits, but at least one invalid hit -> unsuccessful reconstruction
  long long count_onehit=0;                // number of occurrences with 1 valid hit -> successful reconstruction
  long long count_twohits_bothsi=0;        // number of occurrences with 2 valid hits in Si layers -> unsuccessful reconstruction
  long long count_twohits_goodenergy=0;    // number of occurrences with 2 valid hits, 1 Si & 1 CdTe, where the fluorescence energy condition is satisfied -> successful reconstruction
  long long count_twohits_badenergy=0;     // number of occurrences with 2 valid hits, 1 Si & 1 CdTe, where the fluorescence energy condition is NOT satisfied -> unsuccessful reconstruction
  long long count_layer[5]={0,0,0,0,0};    // count how many events occur per layer; only successful reconstructions are considered
  long long count_skipreco=0;              // count how many occurrences are skipped when skipreco=yes
  long long count_nosignal=0;              // count how many occurrences have zero signals
  long long count_epicut_null=0;           // number of times EPICUT is set to NULL
  long long count_epicut[5]={0,0,0,0,0};   // histogram of (non-NULL) EPICUT values


  // set up connections between local variables and input SFF columns
  ahfits::Router routin(fpin);
  routin.connectScalar(ahfits::e_READONLY,"TIME",inrow.m_time);
  routin.connectScalar(ahfits::e_READONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routin.connectScalar(ahfits::e_READONLY,"CATEGORY",inrow.m_category);
  routin.connectScalar(ahfits::e_READONLY,"LIVETIME",inrow.m_livetime);
  routin.connectScalar(ahfits::e_READONLY,"NUM_ASIC",inrow.m_num_asic);
  routin.connectBit(ahfits::e_READONLY,"STATUS",inrow.m_status,inrow.num_status);
  routin.connectBit(ahfits::e_READONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routin.connectBit(ahfits::e_READONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routin.connectBit(ahfits::e_READONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routin.connectBit(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
  routin.connectBit(ahfits::e_READONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",inrow.m_readout_id_rmap,inrow.num_readout_id_rmap);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"EPI",inrow.m_epi,inrow.num_epi,inrow.m_epi_null);

  // set up connections between local variables and output SFFa columns
  ahfits::Router routout(fpout);
  routout.connectScalar(ahfits::e_WRITEONLY,"TIME",inrow.m_time);
  routout.connectScalar(ahfits::e_WRITEONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routout.connectScalar(ahfits::e_WRITEONLY,"CATEGORY",inrow.m_category);
  routout.connectScalar(ahfits::e_WRITEONLY,"LIVETIME",inrow.m_livetime);
  routout.connectScalar(ahfits::e_WRITEONLY,"LAYER",outrow.m_layer,&outrow.m_layer_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"ENE_TOTAL",outrow.m_ene_total,&outrow.m_ene_total_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"PI",outrow.m_pi,&outrow.m_pi_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"RAWX",outrow.m_rawx,&outrow.m_rawx_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"RAWY",outrow.m_rawy,&outrow.m_rawy_null);
  routout.connectBit(ahfits::e_WRITEONLY,"STATUS",inrow.m_status,inrow.num_status);
  routout.connectBit(ahfits::e_WRITEONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routout.connectBit(ahfits::e_WRITEONLY,"RECO_STATUS",outrow.m_reco_status,outrow.num_reco_status);
  routout.connectScalar(ahfits::e_WRITEONLY,"EVTCAT",outrow.m_evtcat);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SIGNAL",outrow.m_signal);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SIGPOS",outrow.m_sigpos);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SIGEPI",outrow.m_sigepi);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"EPITOP",outrow.m_epitop);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"EPIBOT",outrow.m_epibot);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"EPICUT",outrow.m_epicut,outrow.m_epicut_null);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"GOODBAD",outrow.m_goodbad);
  routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"VALIDHITS",outrow.m_validhits);
  routout.connectScalar(ahfits::e_WRITEONLY,"ACTX",outrow.m_actx,&outrow.m_actx_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"ACTY",outrow.m_acty,&outrow.m_acty_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"DETX",outrow.m_detx,&outrow.m_detx_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"DETY",outrow.m_dety,&outrow.m_dety_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"FOCX",outrow.m_focx,&outrow.m_focx_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"FOCY",outrow.m_focy,&outrow.m_focy_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"X",outrow.m_x,&outrow.m_x_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"Y",outrow.m_y,&outrow.m_y_null);

  // set up connections between local variables the extra output file
  ahfits::Router* routextra=0;
  if (fpextra != 0) {     // no extra output file if fpextra==0
    routextra=new ahfits::Router(fpextra);
    routextra->connectScalar(ahfits::e_WRITEONLY,"TIME",inrow.m_time);
    routextra->connectScalar(ahfits::e_WRITEONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
    routextra->connectScalar(ahfits::e_WRITEONLY,"CATEGORY",inrow.m_category);
    routextra->connectScalar(ahfits::e_WRITEONLY,"LIVETIME",inrow.m_livetime);
    routextra->connectScalar(ahfits::e_WRITEONLY,"LAYER",extrarow.m_layer,&extrarow.m_layer_null);
    routextra->connectScalar(ahfits::e_WRITEONLY,"SIDE",extrarow.m_side);
    routextra->connectScalar(ahfits::e_WRITEONLY,"ENE_TOTAL",extrarow.m_ene_total,&extrarow.m_ene_total_null);
    routextra->connectScalar(ahfits::e_WRITEONLY,"PI",extrarow.m_pi,&extrarow.m_pi_null);
    routextra->connectScalar(ahfits::e_WRITEONLY,"RAWX",extrarow.m_rawx,&extrarow.m_rawx_null);
    routextra->connectScalar(ahfits::e_WRITEONLY,"RAWY",extrarow.m_rawy,&extrarow.m_rawy_null);
    routextra->connectBit(ahfits::e_WRITEONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
    routextra->connectBit(ahfits::e_WRITEONLY,"STATUS",inrow.m_status,inrow.num_status);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
    routextra->connectBit(ahfits::e_WRITEONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
    routextra->connectBit(ahfits::e_WRITEONLY,"RECO_STATUS",extrarow.m_reco_status,extrarow.num_reco_status);
    routextra->connectScalar(ahfits::e_WRITEONLY,"EVTCAT",extrarow.m_evtcat);
  }

  // event row loop
  numrows = ahfits::numRows(fpin); 
  ahfits::firstRow(fpout);                                                      // make sure to start writing from first row of output file
  if (fpextra != 0) ahfits::firstRow(fpextra);                                  // make sure to start writing from first row of extra output file
  for (ahfits::firstRow(fpin); ahfits::readOK(fpin); ahfits::nextRow(fpin)) {   // loop over all input rows
    krow++;
    ahfits::readRow(fpin);                     // read input row data into inrow structure
    int nsignal=inrow.num_readout_id_rmap;     // number of signals found in occurrence

    // when occurence_id parameter >= 0; process single occurrence
    if (par.m_occurrenceid >= 0 && inrow.m_occurrence_id != par.m_occurrenceid) continue;

    if (numrows > 25000 && (krow % 25000) == 1) {
      AH_OUT << "Processing row " << krow << "/" << numrows << " of input SFF" << std::endl;
    }

    AH_DEBUG << "Processing row: " << krow << std::endl;
    AH_DEBUG << "Num signals: " << nsignal << std::endl;

    // This structure stores signal data for each layer.  It contains output and
    // diagnostic information about the reconstruction process.
    SignalDataRow sigdat;

    // Prepare output data structure
    // 1. Copy row data from input file to output file: TIME, OCCURRENCE_ID,
    //    CATEGORY, LIVETIME, FLAG_SEU, FLAG_LCHK, FLAG_TRIG, FLAG_TRIGPAT,
    //    FLAG_HITPAT, FLAG_FASTBGO, and STATUS.
    // 2. Initialize other output columns: PI, RAWX, RAWY, LAYER, 
    //    READOUT_ID_INDEX, EVTCAT, RECO_STATUS, SIGNAL, SIGEPI, GOODBAD, and
    //    VALIDHITS.
    // 3. Initialize following columns if NORMAL or AM241: ACTX/Y, DETX/Y,
    //    FOCX/Y, and X/Y.
    prepareOutputData(inrow,outrow);
    count_rows++;

    // If occurrence has a bad STATUS, set bit of RECO_STATUS, set the
    // output to NULL, write row, and continue to next occurrence.
    if (!procstatus::processRow(inrow.m_proc_status)) {
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": bad PROC_STATUS; set output to NULL" << std::endl;
      setRecoStatusBadStatus(outrow.m_reco_status);
      setOutputToNull(outrow);
      outrow.m_evtcat=11;
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      count_badstatus++;
      count_epicut_null+=5;       // increase count for each layer
      continue;                    // read next input row
    }

    // Determine if occurrence is CAMERA/AM241, CALMODE, READALL, PSEUDO, or
    // NOTSURE.
    bool isAm241=false;                 // true if Am241 flag set
    mode=hxisgdevtid::getOccurrenceMode("HXI",inrow,alive_asic,&isAm241);

    // Set RECO_STATUS bits based on mode (all bits have been initialized to zero)
    if (isAm241) setRecoStatusAm241(outrow.m_reco_status);
    setRecoStatusMode(mode,outrow.m_reco_status);

    // If no signals in occurrence, then set bit of RECO_STATUS, set the output 
    // to NULL, write output row, and continue to next occurrence.
    if (nsignal == 0) {
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": no signals in occurrence; set output to NULL" << std::endl;
      setRecoStatusNoSignal(outrow.m_reco_status);
      setOutputToNull(outrow);
      outrow.m_evtcat=11;
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      count_nosignal++;
      for (int ii=0; ii < NLAYER; ii++) {
        outrow.m_epicut_null[ii]=0;
        outrow.m_epicut[ii]=3;             // 3 => no signals
        count_epicut[3]++;                 // increase count for each layer
      }
      continue;                            // read next input row
    }

    // If the rejectbgo parameter is set to 'yes', check if any bits of 
    // FLAG_HITPAT or FLAG_FASTBGO are set.  If so, then set bit of 
    // RECO_STATUS, set the output to NULL, write output row, and continue to 
    // next occurrence.
    if (par.m_rejectbgo) {
      if (hasNonZeroHitPat(inrow) || hasNonZeroFastBGO(inrow)) {
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": flagged as FASTBGO or HITPAT; set output to NULL" << std::endl;
        setRecoStatusFastBGO(outrow.m_reco_status);
        setOutputToNull(outrow);
        outrow.m_evtcat=11;
        ahfits::writeRow(fpout);
        ahfits::nextRow(fpout);
        count_rejectbgo++;
        count_epicut_null+=5;      // increase count for each layer
        continue;                  // read next input row
      }
    }

    // Skip READALL and CALMODE occurrences if skipreco is yes
    if (par.m_skipreco) {
      if (mode == hxisgdevtid::e_READALL || mode == hxisgdevtid::e_CALMODE) {
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": READALL or CALMODE; set output to NULL" << std::endl;
        setRecoStatusSkipReco(outrow.m_reco_status);
        setOutputToNull(outrow);
        outrow.m_evtcat=11;
        ahfits::writeRow(fpout);
        ahfits::nextRow(fpout);
        count_skipreco++;
        count_epicut_null+=5;      // increase count for each layer
        continue;                  // read next input row
      }
    }

    // Loop over all signals and
    // 1. find RAWX, RAWY, LAYER using remapping CALDB data
    // 2. check if the EPI value is above threshold (from badpix CALDB data)
    // 3. store signal data in signal data structure (sigdat) organized by layer
    //    and side of layer (top or bottom).
    //
    // If the EPI value is below threshold, then the signal is designated as
    // "BAD" (via laydat->m_statusx/y).  However this does not disallow a hit
    // to exist on the side with the BAD signal, unless all signals on that 
    // side are BAD.
    AH_DEBUG << "Separate signals by layer" << std::endl;
    for (int isig=0; isig < nsignal; isig++) {
      int readout_id_rmap=inrow.m_readout_id_rmap[isig];
      double epi=inrow.m_epi[isig];

      // get signal position from remap CALDB file
      int rawx=hxisgdevtid::remap::getRAWX(remap_table,readout_id_rmap);
      int rawy=hxisgdevtid::remap::getRAWY(remap_table,readout_id_rmap);
      int layer=hxisgdevtid::remap::getLAYER(remap_table,readout_id_rmap);

      AH_DEBUG << "Signal: " << isig << std::endl;
      AH_DEBUG << "READOUT_ID_RMAP: " << readout_id_rmap << std::endl;
      AH_DEBUG << "EPI: " << epi << std::endl;
      AH_DEBUG << "RAWX,RAWY,LAYER: " << rawx << ", " << rawy << ", " << layer << std::endl;

      SignalDataLayer* laydat=&sigdat.m_layerdat[layer];   // for brevity: signal data for this layer

      // determine which side the signal is on and check the EPI threshold
      laydat->m_nsig++;    // increase total number of signals for layer
      if (rawx > 0) {      // bottom side
        AH_DEBUG << "Is bottom-side signal" << std::endl;
        AH_DEBUG << "RAWX, EPI, threshold: " << rawx << ", " << epi << ", " << epithre_table[readout_id_rmap] << std::endl;
        laydat->m_nsigx++;
        int j=laydat->m_nsigx-1;   // index to store signal values
        laydat->m_rawx[j]=rawx;
        if (inrow.m_epi_null[isig] == 1) {    // EPI is NULL?
          AH_DEBUG << "EPI is NULL; set bad status" << std::endl;
          laydat->m_statusx[j]=true;
          laydat->m_nnullx++;
        } else if (epi < epithre_table[readout_id_rmap]) {
          AH_DEBUG << "EPI below threshold; set bad status" << std::endl;
          laydat->m_statusx[j]=true;
          laydat->m_nbelowthreshx++;
        } else {
          laydat->m_nokayx++;
          laydat->m_epix[j]=epi;
        }
      } else {           // top side
        AH_DEBUG << "Is top-side signal" << std::endl;
        AH_DEBUG << "RAWY, EPI, threshold: " << rawy << ", " << epi << ", " << epithre_table[readout_id_rmap] << std::endl;
        laydat->m_nsigy++;
        int j=laydat->m_nsigy-1;   // index to store signal values
        laydat->m_rawy[j]=rawy;
        if (inrow.m_epi_null[isig] == 1) {    // EPI is NULL?
          AH_DEBUG << "EPI is NULL; set bad status" << std::endl;
          laydat->m_statusy[j]=true;
          laydat->m_nnully++;
        } else if (epi < epithre_table[readout_id_rmap]) {
          AH_DEBUG << "EPI below threshold; set bad status" << std::endl;
          laydat->m_statusy[j]=true;
          laydat->m_nbelowthreshy++;
        } else {
          laydat->m_nokayy++;
          laydat->m_epiy[j]=epi;
        }
      }
    }    // end loop over signals

    // Check for valid hits in each layer using following steps:
    // 1. require at least one good signal on each side
    // 2. calculate EPI value for each side by summing signal EPIs
    // 3. assign the hit EPI based on the layer type: top side for SI, bottom
    //    side for CdTe
    // 4. check that signals on each side have legal configuration: A) one
    //    signal only, B) two adjacent signals
    // 5. Record position of hit based on signals with largest EPI value for
    //    each side
    AH_DEBUG << "Check for valid hits in each layer" << std::endl;
    int nhit=0;                 // number of confirmed hits over all layers
    bool invalidstatus=false;   // true if any layer has invalid hit
    for (int ilay=0; ilay < NLAYER; ilay++) {
      SignalDataLayer* laydat=&sigdat.m_layerdat[ilay];   // for brevity; look at signals in current layer

      AH_DEBUG << "Layer: " << ilay << std::endl;
      AH_DEBUG << "Num signal X: " << laydat->m_nsigx << std::endl;
      AH_DEBUG << "Num signal Y: " << laydat->m_nsigy << std::endl;

      // side indices of current layer
      int sidy=ilay;      // top sides:    0,1,2,3,4
      int sidx=ilay+5;    // bottom sides: 5,6,7,8,9

      // nothing to do if no signals present; skip to next layer
      if (laydat->m_nsig == 0) {
        outrow.m_goodbad[sidx]=0;          // good
        outrow.m_goodbad[sidy]=0;          // good
        outrow.m_epicut_null[ilay]=0;
        outrow.m_epicut[ilay]=3;           // 3 => no signals
        continue;
      } 

      // Step 1: skip layer if all signals are BAD for any side
      // note: two if-statements are required since the GOODBAD column is
      //       assigned per side
      if (laydat->m_nokayx == 0) {
        outrow.m_validhits[ilay]=2;                   // 2 => bad layer

        if (laydat->m_nsigx == 0) {                   // no signals on side
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") no signals on bottom side" << std::endl;
          outrow.m_goodbad[sidx]=3;
        } else if (laydat->m_nnullx == 0) {           // all signals NULL
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all bottom-side signals below threshold" << std::endl;
          outrow.m_goodbad[sidx]=1;
        } else if (laydat->m_nbelowthreshx == 0) {    // all signals below threshold
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all bottom-side signals are NULL" << std::endl;
          outrow.m_goodbad[sidx]=2;
        } else {                                      // both NULL and below threshold
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all bottom-side signals are NULL or below threshold" << std::endl;
          outrow.m_goodbad[sidx]=4;
        }
      }
      if (laydat->m_nokayy == 0) {
        outrow.m_validhits[ilay]=2;                   // 2 => bad layer

        if (laydat->m_nsigy == 0) {                   // no signals on side
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") no signals on top side" << std::endl;
          outrow.m_goodbad[sidy]=3;
        } else if (laydat->m_nnully == 0) {           // all signals NULL
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all top-side signals below threshold" << std::endl;
          outrow.m_goodbad[sidy]=1;
        } else if (laydat->m_nbelowthreshy == 0) {    // all signals below threshold
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all top-side signals are NULL" << std::endl;
          outrow.m_goodbad[sidy]=2;
        } else {                                      // both NULL and below threshold
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": (layer " << ilay << ") all top-side signals are NULL or below threshold" << std::endl;
          outrow.m_goodbad[sidy]=4;
        }
      }

      // if no signals on one side or other, just skip to next layer
      if (laydat->m_nokayx == 0 || laydat->m_nokayy == 0) {
        outrow.m_epicut_null[ilay]=0;
        outrow.m_epicut[ilay]=4;           // 4 => at least one signal, but one side or both invalid
        continue;
      }

      // Step 2: get EPI value for both sides of layer
      outrow.m_sigepi[sidx]=0.;
      for (int jj=0; jj < laydat->m_nsigx; jj++) {
        if (!laydat->m_statusx[jj]) outrow.m_sigepi[sidx]+=laydat->m_epix[jj];
      }
      outrow.m_sigepi[sidy]=0.;
      for (int jj=0; jj < laydat->m_nsigy; jj++) {
        if (!laydat->m_statusy[jj]) outrow.m_sigepi[sidy]+=laydat->m_epiy[jj];
      }

      // fill EPITOP and EPIBOT columns containing same information as SIGEPI
      outrow.m_epitop[ilay]=outrow.m_sigepi[sidy];
      outrow.m_epibot[ilay]=outrow.m_sigepi[sidx];

      // Check if top-side EPI is within tolerance of bottom-side EPI 
      // using energy cut CALDB file.
      // Note: due to CALDB format, do not use nside EPI for lookup with the
      // CdTe side, still use EPI from bottom side.  (2015-04-20 Lorella, Hiro, Mike W telecon)
      double epi_nside=outrow.m_sigepi[sidx];
      double epi_pside=outrow.m_sigepi[sidy];
      double ecutlower=0.;
      double ecutupper=0.;
      int status1=ahmission::spline::evaluate(lower_cut,ilay,epi_nside,ecutlower);
      int status2=ahmission::spline::evaluate(upper_cut,ilay,epi_nside,ecutupper);
      if (status1 != 0 || status2 != 0) {
        outrow.m_validhits[ilay]=2;                   // 2 => bad layer
        outrow.m_epicut_null[ilay]=1;
        std::stringstream msg;
        msg << "Occurrence " << inrow.m_occurrence_id << ": bottom-side EPI for layer "
            << ilay << " out-of-range of energy cut; "
            << "rejecting occurrence; bottom-side EPI = " << epi_nside;
        AH_DEBUG << msg.str() << std::endl;
        continue;
      } else if (epi_pside < ecutlower) {
        outrow.m_validhits[ilay]=2;                   // 2 => bad layer
        outrow.m_epicut_null[ilay]=0;
        outrow.m_epicut[ilay]=2;                      // 2 => p-side energy too low
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": top-side EPI lower than energy cut limit" << std::endl;
        continue;
      } else if (epi_pside > ecutupper) {
        outrow.m_validhits[ilay]=2;                   // 2 => bad layer
        outrow.m_epicut_null[ilay]=0;
        outrow.m_epicut[ilay]=1;                      // 1 => p-side energy too high
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": top-side EPI higher than energy cut limit" << std::endl;
        continue;
      } else {
        outrow.m_epicut_null[ilay]=0;
        outrow.m_epicut[ilay]=0;                      // 0 => p-side energy is okay
      }

      // Step 3: assign PI
      //  for Si, take sum of top signals
      //  for CdTe, take sum of bottom signals
      if (ilay == NLAYER-1) {    // last layer is CdTe
        AH_DEBUG << "Hit EPI from bottom side (X)" << std::endl;
        laydat->m_hitpi+=outrow.m_sigepi[sidx];
      } else {                      // other layers are Si
        AH_DEBUG << "Hit EPI from top side (Y)" << std::endl;
        laydat->m_hitpi+=outrow.m_sigepi[sidy];
      }
      AH_DEBUG << "EPI: " << laydat->m_hitpi << std::endl;

      // Step 4/5 (bottom side): check for legal X combinations and set RAWX
      //  X side is legal if:
      //   A) one signal, or
      //   B) two adjacent signals
      // Note: number of good signals is determined by the number of total
      //       signals minus the number of bad signals
      if (laydat->m_nokayx == 1) {     // number of good signals (not NULL, above threshold)
        AH_DEBUG << "X side has valid hit: 1 signal" << std::endl;
        for (int jj=0; jj < laydat->m_nsigx; jj++) {        // find single valid signal
          if (!laydat->m_statusx[jj]) {
            laydat->m_hitx=laydat->m_rawx[jj];
            break;
          }
        }
      } else if(laydat->m_nokayx == 2) {
        // find the two valid signals
        int jj1=0;
        int jj2=0;
        for (jj1=0; jj1 < laydat->m_nsigx; jj1++) {        // find first valid signal
          if (!laydat->m_statusx[jj1]) break;
        }
        for (jj2=jj1+1; jj2 < laydat->m_nsigx; jj2++) {    //find second valid signal
          if (!laydat->m_statusx[jj2]) break;
        }
        // check adjacency
        if (std::abs(laydat->m_rawx[jj1]-laydat->m_rawx[jj2]) == 1) {
          AH_DEBUG << "X side has valid hit: 2 adjacent signals" << std::endl;
          if (laydat->m_epix[jj2] > laydat->m_epix[jj1]) {
            AH_DEBUG << "Take position from 2nd signal of hit (based on EPI)" << std::endl;
            laydat->m_hitx=laydat->m_rawx[jj2];
          } else {
            AH_DEBUG << "Take position from 1st signal of hit (based on EPI)" << std::endl;
            laydat->m_hitx=laydat->m_rawx[jj1];
          }
        } else {   // signals not adjacent
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": Invalid hit - bottom side of layer " << ilay << " has two, non-adjacent signals" << std::endl;
          laydat->m_validhit=false;
        }
      } else {     // more than 2 signals
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": Invalid hit - bottom side of layer " << ilay << " has more than two signals" << std::endl;
        laydat->m_validhit=false;
      }

      // Step 4/5 (top side): check for legal Y combinations and set RAWY
      //  Y side is legal if:
      //   A) one signal, or
      //   B) two adjacent signals
      // Note: number of good signals is determined by the number of total
      //       signals minus the number of bad signals
      if (laydat->m_nokayy == 1) {
        AH_DEBUG << "Y side has valid hit: 1 signal" << std::endl;
        for (int jj=0; jj < laydat->m_nsigy; jj++) {        // find single valid signal
          if (!laydat->m_statusy[jj]) {
            laydat->m_hity=laydat->m_rawy[jj];
            break;
          }
        }
      } else if(laydat->m_nokayy == 2) {
        // find the two valid signals
        int jj1=0;
        int jj2=0;
        for (jj1=0; jj1 < laydat->m_nsigy; jj1++) {        // find first valid signal
          if (!laydat->m_statusy[jj1]) break;
        }
        for (jj2=jj1+1; jj2 < laydat->m_nsigy; jj2++) {    //find second valid signal
          if (!laydat->m_statusy[jj2]) break;
        }
        // check adjacency
        if (std::abs(laydat->m_rawy[jj1]-laydat->m_rawy[jj2]) == 1) {
          AH_DEBUG << "Y side has valid hit: 2 adjacent signals" << std::endl;
          if (laydat->m_epiy[jj2] > laydat->m_epiy[jj1]) {
            AH_DEBUG << "Take position from 2nd signal of hit (based on EPI)" << std::endl;
            laydat->m_hity=laydat->m_rawy[jj2];
          } else {
            AH_DEBUG << "Take position from 1st signal of hit (based on EPI)" << std::endl;
            laydat->m_hity=laydat->m_rawy[jj1];
          }
        } else {   // signals not adjacent
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": Invalid hit - top side of layer " << ilay << " has two, non-adjacent signals" << std::endl;
          laydat->m_validhit=false;
        }
      } else {     // more than 2 signals
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": Invalid hit - top side of layer " << ilay << " has more than two signals" << std::endl;
        laydat->m_validhit=false;
      }

      // Set output SIGNAL & SIGPOS column values for both sides of current layer.
      // SIGNAL stores the number of non-NULL signals above threshold.  SIGPOS
      // stores the position of the X-side and Y-side hits corresponding to the
      // X-side and Y-side EPI values in the SIGEPI column.
      outrow.m_signal[sidx]=laydat->m_nokayx;
      outrow.m_signal[sidy]=laydat->m_nokayy;
      outrow.m_sigpos[sidx]=laydat->m_hitx;
      outrow.m_sigpos[sidy]=laydat->m_hity;

      // Update number of hits in occurrence and VALIDHITS output column.
      if (laydat->m_validhit) {
        AH_DEBUG << "Layer has valid hit" << std::endl;
        nhit++;
        outrow.m_validhits[ilay]=1;
      } else {
        AH_DEBUG << "Layer has invalid hit" << std::endl;
        outrow.m_validhits[ilay]=2;
        invalidstatus=true;
      }

    }   // end loop over layers


    // Check for valid event based on number of hits found across all layers.
    // Only 1 or 2 hit occurrences can result in an event.  If any layer has
    // an invalid hit, then the reconstruction is unsuccessful.  For two-hit
    // occurrences, an event only results if one hit is from a Si layer and
    // the other is from the CdTe layer and the fluorescence energy condition
    // is satisfied for the Si-layer hit.  In order to analyze the event
    // reconstruction process, each successful and unsuccessful case has its
    // own event category (evtcat).  If the reconstruction is unsuccessful, 
    // the first bit of RECO_STATUS is set to 1.
    if (nhit == 0) {
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction failed - no valid hits" << std::endl;
      outrow.m_evtcat=9;
      setOutputToNull(outrow);
      setRecoStatusBadReco(outrow.m_reco_status);
      count_nohits++;
    } else if (nhit > 2) {
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction failed - more than 2 hits" << std::endl;
      outrow.m_evtcat=10;
      setOutputToNull(outrow);
      setRecoStatusBadReco(outrow.m_reco_status);
      count_toomanyhits++;
    } else if (invalidstatus) {    // 1 or 2 hits, but other invalid hits
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction failed - 1 or 2 hits, but invalid layer" << std::endl;
      outrow.m_evtcat=8;
      setOutputToNull(outrow);
      setRecoStatusBadReco(outrow.m_reco_status);
      count_invalidhits++;
    } else if (nhit == 1) {
      AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction successful - single valid hit" << std::endl;
      outrow.m_evtcat=1;
      bool found=false;
      for (int ilay=0; ilay < NLAYER; ilay++) {
        SignalDataLayer* laydat=&sigdat.m_layerdat[ilay];   // for brevity
        if (laydat->m_nsig > 0 && laydat->m_hitpi > 0) {
          AH_DEBUG << "Hit in layer " << ilay << std::endl;
          outrow.m_rawx=laydat->m_hitx;
          outrow.m_rawy=laydat->m_hity;
          outrow.m_ene_total=laydat->m_hitpi;
          hxisgdevtid::convertEPIToPI("HXI",outrow.m_ene_total,outrow.m_pi,outrow.m_pi_null);
          outrow.m_layer=ilay;
          AH_DEBUG << "RAWX, RAWY, LAYER: " << outrow.m_rawx << ", " << outrow.m_rawy << ", " << outrow.m_layer << std::endl;
          AH_DEBUG << "Event category: " << outrow.m_evtcat << std::endl;
          AH_DEBUG << "EPI, PI: " << laydat->m_hitpi << ", " << outrow.m_pi << std::endl;
          found=true;
          break;
        }
      }
      if (!found) AH_THROW_LOGIC("failed to locate single, valid hit");
      count_onehit++;
    } else {                        // nvalidhit == 2

      AH_DEBUG << "Valid event with 2 hits" << std::endl;

      // pointers to signals from different layers; for brevity
      SignalDataLayer* laydat1=0;
      SignalDataLayer* laydat2=0;

      // get layer indices of valid layers: ilay1, ilay2, where ilay2 > ilay1
      int ilay1=-1;
      int ilay2=-1;
      for (int ilay=0; ilay < NLAYER; ilay++) {
        SignalDataLayer* laydat=&sigdat.m_layerdat[ilay];   // for brevity
        if (laydat->m_nsig > 0 && laydat->m_hitpi > 0) {
          if (ilay1 < 0) 
            ilay1=ilay;
          else
            ilay2=ilay;
        }
        if (ilay2 > 0) break;
      }
      if (ilay2 < 0) AH_THROW_LOGIC("2-hit case, but 2nd hit layer not identified");
      AH_DEBUG << "Hits in layers " << ilay1 << " and " << ilay2 << std::endl;
      laydat1=&sigdat.m_layerdat[ilay1];    // has smaller layer index
      laydat2=&sigdat.m_layerdat[ilay2];    // has larger layer index

      // figure out if valid event
      // note: NLAYER-1 is index of last later which is CdTe
      if (ilay2 != NLAYER-1) {         // two Si hits
        AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction failed - 2 hits, both in a Si layer" << std::endl;
        outrow.m_evtcat=7;
        setOutputToNull(outrow);
        setRecoStatusBadReco(outrow.m_reco_status);
        count_twohits_bothsi++;
      } else {                            // one Si, one CdTe
        AH_DEBUG << "One hit in CdTe layer" << std::endl;

        // perform energy test based on Si EPI
        feature="";
        if (hxisgdevtid::fluor::hasMatch("Si",laydat1->m_hitpi,fluor_table,feature)) {    // fluorescence test passes
          outrow.m_ene_total=laydat1->m_hitpi+laydat2->m_hitpi;
          hxisgdevtid::convertEPIToPI("HXI",outrow.m_ene_total,outrow.m_pi,outrow.m_pi_null);
          outrow.m_rawx=laydat2->m_hitx;
          outrow.m_rawy=laydat2->m_hity;
          outrow.m_layer=ilay2;           // CdTe layer index
          outrow.m_evtcat=2+ilay1;        // 2 + Si layer index
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction successful - 2-hit event" << std::endl;
          AH_DEBUG << "Energy test passes => valid event" << std::endl;
          AH_DEBUG << "RAWX, RAWY, LAYER: " << outrow.m_rawx << ", " << outrow.m_rawy << ", " << outrow.m_layer << std::endl;
          AH_DEBUG << "Event category: " << outrow.m_evtcat << std::endl;
          AH_DEBUG << "EPI1, EPI2, EPI(total), PI: " << laydat1->m_hitpi << ", " << laydat2->m_hitpi << ", " << laydat1->m_hitpi+laydat2->m_hitpi << ", " << outrow.m_pi << std::endl;
          count_twohits_goodenergy++;
        } else {                         // fluorescence test fails
          AH_DEBUG << "Occurrence " << inrow.m_occurrence_id << ": reconstruction failed - 2 hits (1 Si, 1 CdTe), but energy test fails" << std::endl;
          outrow.m_evtcat=6;
          setOutputToNull(outrow);
          setRecoStatusBadReco(outrow.m_reco_status);
          count_twohits_badenergy++;
        }

      }      // end if over 2-hit events
    }        // end if over all event types
    if (outrow.m_layer_null == 0) count_layer[outrow.m_layer]++;    // count successful events per layer

    // increment count for EPICUT
    for (int ilay=0; ilay < NLAYER; ilay++) {
      if (outrow.m_epicut_null[ilay] == 1) {
        count_epicut_null++;
      } else {
        count_epicut[outrow.m_epicut[ilay]]++;
      }
    }

    // Write output row.
    AH_DEBUG << "Write to output file" << std::endl;
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);

    // Write calibration data to extra output file
    // 1. Copy row data from input file to output file: TIME, OCCURRENCE_ID,
    //    CATEGORY, LIVETIME, FLAG_SEU, FLAG_LCHK, FLAG_TRIG, FLAG_TRIGPAT,
    //    FLAG_HITPAT, FLAG_FASTBGO, and STATUS.
    // 2. For each layer with a hit, populate PI, RAWX, RAWY, LAYER, EVTCAT, and
    //    RECO_STATUS and write to outcalfile
    if (fpextra != 0) {
      prepareOutputData(inrow,extrarow);    // Step 1: only needs to be done once per occurrence
      for (int ilay=0; ilay < NLAYER; ilay++) {
        SignalDataLayer* laydat=&sigdat.m_layerdat[ilay];           // for brevity
        if (laydat->m_nsig > 0 && laydat->m_validhit) {             // only output for layers with a valid hit

          // EVTCAT and RECO_STATUS are the same for the top and bottom layers;
          // take values from output file structure
          extrarow.m_evtcat=outrow.m_evtcat;
          for (int ir=0; ir < hxisgdevtid::HXI_SIZE_RECO_STATUS; ir++)
            extrarow.m_reco_status[ir]=outrow.m_reco_status[ir];

          // top side - RAWY > 0
          extrarow.m_layer=ilay;                                             // layer index
          extrarow.m_side=ilay;                                              // sides 0-4 denote top layer
          extrarow.m_ene_total=outrow.m_sigepi[extrarow.m_side];
          hxisgdevtid::convertEPIToPI("HXI",extrarow.m_ene_total,extrarow.m_pi,extrarow.m_pi_null);    // PI of hit in side from sum of all signals in side
          extrarow.m_rawx=0;                                                 // RAWX not valid for top layer
          extrarow.m_rawy=laydat->m_hity;                                    // set RAWY - top layer position
          ahfits::writeRow(fpextra);
          ahfits::nextRow(fpextra);

          // bottom side - RAWX > 0
          extrarow.m_layer=ilay;                                             // layer index
          extrarow.m_side=ilay+5;                                            // sides 5-9 denote bottom layer
          extrarow.m_ene_total=outrow.m_sigepi[extrarow.m_side];
          hxisgdevtid::convertEPIToPI("HXI",extrarow.m_ene_total,extrarow.m_pi,extrarow.m_pi_null);    // PI of hit in side from sum of all signals in side
          extrarow.m_rawx=laydat->m_hitx;                                    // set RAWX - bottom layer position
          extrarow.m_rawy=0;                                                 // RAWY not valid for bottom layer
          ahfits::writeRow(fpextra);
          ahfits::nextRow(fpextra);

        }  // end if valid hit
      }    // end loop over layers
    }      // end if fpextra != 0

    // if occurrenceid parameter >= 0, then only want to process a single row
    if (par.m_occurrenceid >= 0) break;

  }   // end row loop over input

  // Destroy router to extra output file.  This step is necessary since the
  // ahfits router for the extra output file is a declared as a pointer since
  // it is needed only when fpextra != 0.
  if (fpextra != 0 && routextra != 0) {
    delete routextra;
    routextra=0;
  }

  // report counts
  AH_INFO(ahlog::HIGH) << "OCCURRENCE COUNTS: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  TOTAL PROCESSED (ROWS):                  " << count_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "  BAD STATUS:                              " << count_badstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  NO SIGNALS:                              " << count_nosignal << std::endl;
  AH_INFO(ahlog::HIGH) << "  REJECT BGO:                              " << count_rejectbgo << std::endl;
  AH_INFO(ahlog::HIGH) << "  SKIP RECONSTRUCTION:                     " << count_skipreco << std::endl;
  AH_INFO(ahlog::HIGH) << "  NO HITS FOUND (EVTCAT 9):                " << count_nohits << std::endl;
  AH_INFO(ahlog::HIGH) << "  INVALID HIT(S) (EVTCAT 8):               " << count_invalidhits << std::endl;
  AH_INFO(ahlog::HIGH) << "  TOO MANY HITS (EVTCAT 10):               " << count_toomanyhits << std::endl;
  AH_INFO(ahlog::HIGH) << "  ONE HIT, GOOD (EVTCAT 1):                " << count_onehit << std::endl;
  AH_INFO(ahlog::HIGH) << "  TWO HITS, GOOD (EVTCAT 2-5):             " << count_twohits_goodenergy << std::endl;
  AH_INFO(ahlog::HIGH) << "  TWO HITS, ENERGY TEST FAILED (EVTCAT 6): " << count_twohits_badenergy << std::endl;
  AH_INFO(ahlog::HIGH) << "  TWO HITS, BOTH SI LAYER (EVTCAT 7):      " << count_twohits_bothsi << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "EPICUT COUNTS (TOTAL = 5 x ROWS): " << std::endl;
  AH_INFO(ahlog::HIGH) << "  NULL:                                    " << count_epicut_null << std::endl;
  AH_INFO(ahlog::HIGH) << "  0 = OKAY:                                " << count_epicut[0] << std::endl; 
  AH_INFO(ahlog::HIGH) << "  1 = TOP SIDE TOO HIGH:                   " << count_epicut[1] << std::endl;
  AH_INFO(ahlog::HIGH) << "  2 = TOP SIDE TOO LOW:                    " << count_epicut[2] << std::endl;
  AH_INFO(ahlog::HIGH) << "  3 = NO SIGNALS IN LAYER:                 " << count_epicut[3] << std::endl;
  AH_INFO(ahlog::HIGH) << "  4 = OTHER PROBLEM:                       " << count_epicut[4] << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "  COUNTS PER LAYER" << std::endl;
  for (int ilayer=0; ilayer < 5; ilayer++)
    AH_INFO(ahlog::HIGH) << "    LAYER " << ilayer << ":  " << count_layer[ilayer] << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr fpin, ahfits::FilePtr fpout, ahfits::FilePtr fpextra,
              hxisgdevtid::remap::DataType & remap_table) {
  ahfits::close(fpin);
  ahfits::close(fpout);
  if (fpextra != 0) ahfits::close(fpextra);
  remap_table.clear();
}

// ****************************************************************************

void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile, 
                      ahfits::FilePtr& fpdest) {

  AH_INFO(ahlog::HIGH) << "Creating output file: " << outfile << std::endl;

  // create empty output file with EVENTS HDU
  ahfits::create(outfile,"",&fpdest);
  ahfits::addEmptyTbl(fpdest,"EVENTS");

  // copy keywords from input file
  ahmission::keyword::copyAllKeywords(fpsrc,fpdest,ahmission::keyword::e_EVENT);

  // copied from input
  ahfits::insertColAfter(fpdest,"TIME","1D");
  ahfits::setTUnit(fpdest,"TIME","s");
  ahfits::setColumnDescription(fpdest,"TIME","Seconds from 01 Jan 2014 00:00:00");
  ahfits::insertColAfter(fpdest,"OCCURRENCE_ID","1J");
  ahfits::setColumnDescription(fpdest,"OCCURRENCE_ID","Sequential Number for occurrence");
  ahfits::insertColAfter(fpdest,"CATEGORY","1B");
  ahfits::setColumnDescription(fpdest,"CATEGORY","Data recorder priority");
  ahfits::insertColAfter(fpdest,"FLAG_SEU","5X");
  ahfits::setColumnDescription(fpdest,"FLAG_SEU","1-4 Si 5 CdTE SEU flag");
  ahfits::insertColAfter(fpdest,"FLAG_LCHK","5X");
  ahfits::setColumnDescription(fpdest,"FLAG_LCHK","1-4 Si 5 CdTE Length Chk flag");
  ahfits::insertColAfter(fpdest,"FLAG_TRIG","8X");
  ahfits::setColumnDescription(fpdest,"FLAG_TRIG","Trigger origin");
  ahfits::insertColAfter(fpdest,"FLAG_TRIGPAT","8X");
  ahfits::setColumnDescription(fpdest,"FLAG_TRIGPAT","Trigger pattern");
  ahfits::insertColAfter(fpdest,"FLAG_HITPAT","2X");
  ahfits::setColumnDescription(fpdest,"FLAG_HITPAT","BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_FASTBGO","2X");
  ahfits::setColumnDescription(fpdest,"FLAG_FASTBGO","Fast BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"LIVETIME","1J");
  ahfits::setTZero(fpdest,"LIVETIME",2147483648LL);
  ahfits::setColumnDescription(fpdest,"LIVETIME","Time since previous occurrence");
  ahfits::insertColAfter(fpdest,"PROC_STATUS","32X");
  ahfits::setColumnDescription(fpdest,"PROC_STATUS","Record bad telemetry or bad values");
  ahfits::insertColAfter(fpdest,"STATUS","8X");
  ahfits::setColumnDescription(fpdest,"STATUS","Occurrence status Flags");

  // added from reconstruction (this tool)
  ahfits::insertColAfter(fpdest,"EVTCAT","1B");
  ahfits::setColumnDescription(fpdest,"EVTCAT","Event category");
  ahfits::insertColAfter(fpdest,"SIGNAL","10I");
  ahfits::setColumnDescription(fpdest,"SIGNAL","Number of Signals per side");
  ahfits::insertColAfter(fpdest,"SIGPOS","10I");
  ahfits::setColumnDescription(fpdest,"SIGPOS","Signal position per side (X or Y)");
  ahfits::insertColAfter(fpdest,"SIGEPI","10E");
  ahfits::setColumnDescription(fpdest,"SIGEPI","Sum of EPI per side");
  ahfits::insertColAfter(fpdest,"GOODBAD","10I");
  ahfits::setColumnDescription(fpdest,"GOODBAD","0=good 1=bad per side");
  ahfits::insertColAfter(fpdest,"VALIDHITS","5I");
  ahfits::setColumnDescription(fpdest,"VALIDHITS","Hit validity per layer");
  ahfits::insertColAfter(fpdest,"LAYER","1B");
  ahfits::setTNull(fpdest,"LAYER",99);
  ahfits::setColumnDescription(fpdest,"LAYER","Layer number 0-3 Si 4 CdTe");
  ahfits::insertColAfter(fpdest,"ENE_TOTAL","D");
  ahfits::setColumnDescription(fpdest,"ENE_TOTAL","Sum of EPI for occurrence");
  ahfits::insertColAfter(fpdest,"PI","I");
  ahfits::setTNull(fpdest,"PI",-999);
  ahfits::setTLmin(fpdest,"PI",0);
  ahfits::setTLmax(fpdest,"PI",2047);
  ahfits::setColumnDescription(fpdest,"PI","Pulse Invariant");
  ahfits::insertColAfter(fpdest,"RECO_STATUS","16X");
  ahfits::setColumnDescription(fpdest,"RECO_STATUS","Reconstruction Status");
  ahfits::insertColAfter(fpdest,"RAWX","1I");
  ahfits::setTNull(fpdest,"RAWX",-1);
  ahfits::setTLmin(fpdest,"RAWX",1);
  ahfits::setTLmax(fpdest,"RAWX",128);
  ahfits::setColumnDescription(fpdest,"RAWX","Pixel X on RAW-coordinate");
  ahfits::insertColAfter(fpdest,"RAWY","1I");
  ahfits::setTNull(fpdest,"RAWY",-1);
  ahfits::setTLmin(fpdest,"RAWY",1);
  ahfits::setTLmax(fpdest,"RAWY",128);
  ahfits::setColumnDescription(fpdest,"RAWY","Pixel Y on RAW-coordinate");

  // columns related to energy cut test
  ahfits::insertColAfter(fpdest,"EPITOP","5E");
  ahfits::setColumnDescription(fpdest,"EPITOP","EPI total top layers");
  ahfits::insertColAfter(fpdest,"EPIBOT","5E");
  ahfits::setColumnDescription(fpdest,"EPIBOT","EPI total bottom layers");
  ahfits::insertColAfter(fpdest,"EPICUT","5I");   // 0=okay; 1=exclude high; 2=exclude low; 3=no signal; 4=below thresh or NULL
  ahfits::setTNull(fpdest,"EPICUT",-999);
  ahfits::setColumnDescription(fpdest,"EPICUT","Energy cut test code for layer");

  // to be filled by coordevt
  ahfits::insertColAfter(fpdest,"ACTX","1I");
  ahfits::setTNull(fpdest,"ACTX",-1);
  ahfits::setTLmin(fpdest,"ACTX",1);
  ahfits::setTLmax(fpdest,"ACTX",256);
  ahfits::setColumnDescription(fpdest,"ACTX","Pixel X on ACT-coordinate");
  ahfits::insertColAfter(fpdest,"ACTY","1I");
  ahfits::setTNull(fpdest,"ACTY",-1);
  ahfits::setTLmin(fpdest,"ACTY",1);
  ahfits::setTLmax(fpdest,"ACTY",256);
  ahfits::setColumnDescription(fpdest,"ACTY","Pixel Y on ACT-coordinate");
  ahfits::insertColAfter(fpdest,"DETX","1I");
  ahfits::setTNull(fpdest,"DETX",-1);
  ahfits::setTLmin(fpdest,"DETX",1);
  ahfits::setTLmax(fpdest,"DETX",256);
  ahfits::setColumnDescription(fpdest,"DETX","Pixel X on DET-coordinate");
  ahfits::insertColAfter(fpdest,"DETY","1I");
  ahfits::setTNull(fpdest,"DETY",-1);
  ahfits::setTLmin(fpdest,"DETY",1);
  ahfits::setTLmax(fpdest,"DETY",256);
  ahfits::setColumnDescription(fpdest,"DETY","Pixel Y on DET-coordinate");
  ahfits::insertColAfter(fpdest,"FOCX","1I");
  ahfits::setTNull(fpdest,"FOCX",-1);
  ahfits::setTLmin(fpdest,"FOCX",1);
  ahfits::setTLmax(fpdest,"FOCX",1810);
  ahfits::setColumnDescription(fpdest,"FOCX","Pixel X on FOC-coordinate");
  ahfits::insertColAfter(fpdest,"FOCY","1I");
  ahfits::setTNull(fpdest,"FOCY",-1);
  ahfits::setTLmin(fpdest,"FOCY",1);
  ahfits::setTLmax(fpdest,"FOCY",1810);
  ahfits::setColumnDescription(fpdest,"FOCY","Pixel Y on FOC-coordinate");
  ahfits::insertColAfter(fpdest,"X","1I");
  ahfits::setTNull(fpdest,"X",-1);
  ahfits::setTLmin(fpdest,"X",1);
  ahfits::setTLmax(fpdest,"X",1810);
  ahfits::setColumnDescription(fpdest,"X","Pixel X on SKY-coordinate");
  ahfits::insertColAfter(fpdest,"Y","1I");
  ahfits::setTNull(fpdest,"Y",-1);
  ahfits::setTLmin(fpdest,"Y",1);
  ahfits::setTLmax(fpdest,"Y",1810);
  ahfits::setColumnDescription(fpdest,"Y","Pixel Y on SKY-coordinate");

}

// ****************************************************************************

void createExtraOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile, 
                           ahfits::FilePtr& fpdest) {

  // create empty output file with EVENTS HDU
  ahfits::create(outfile,"",&fpdest);
  ahfits::addEmptyTbl(fpdest,"EVENTS");

  // copy keywords from input file
  ahmission::keyword::copyAllKeywords(fpsrc,fpdest,ahmission::keyword::e_EVENT);

  // copied from input
  ahfits::insertColAfter(fpdest,"TIME","1D");
  ahfits::setTUnit(fpdest,"TIME","s");
  ahfits::setColumnDescription(fpdest,"TIME","Seconds from 01 Jan 2014 00:00:00");
  ahfits::insertColAfter(fpdest,"OCCURRENCE_ID","1J");
  ahfits::setColumnDescription(fpdest,"OCCURRENCE_ID","Sequential Number for occurrence");
  ahfits::insertColAfter(fpdest,"CATEGORY","1B");
  ahfits::setColumnDescription(fpdest,"CATEGORY","Data recorder priority");
  ahfits::insertColAfter(fpdest,"FLAG_SEU","5X");
  ahfits::setColumnDescription(fpdest,"FLAG_SEU","1-4 Si 5 CdTE SEU flag");
  ahfits::insertColAfter(fpdest,"FLAG_LCHK","5X");
  ahfits::setColumnDescription(fpdest,"FLAG_LCHK","1-4 Si 5 CdTE Length Chk flag");
  ahfits::insertColAfter(fpdest,"FLAG_TRIG","8X");
  ahfits::setColumnDescription(fpdest,"FLAG_TRIG","Trigger origin");
  ahfits::insertColAfter(fpdest,"FLAG_TRIGPAT","8X");
  ahfits::setColumnDescription(fpdest,"FLAG_TRIGPAT","Trigger pattern");
  ahfits::insertColAfter(fpdest,"FLAG_HITPAT","2X");
  ahfits::setColumnDescription(fpdest,"FLAG_HITPAT","BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_FASTBGO","2X");
  ahfits::setColumnDescription(fpdest,"FLAG_FASTBGO","Fast BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"LIVETIME","1J");
  ahfits::setTZero(fpdest,"LIVETIME",2147483648LL);
  ahfits::setColumnDescription(fpdest,"LIVETIME","Time since previous occurrence");
  ahfits::insertColAfter(fpdest,"PROC_STATUS","32X");
  ahfits::setColumnDescription(fpdest,"PROC_STATUS","Record bad telemetry or bad values");
  ahfits::insertColAfter(fpdest,"STATUS","8X");
  ahfits::setColumnDescription(fpdest,"STATUS","Occurrence status Flags");

  // calibration data added by this tool
  ahfits::insertColAfter(fpdest,"LAYER","1B");
  ahfits::setTNull(fpdest,"LAYER",99);
  ahfits::setColumnDescription(fpdest,"LAYER","Layer number 0-3 Si 4 CdTe");
  ahfits::insertColAfter(fpdest,"SIDE","1B");
  ahfits::setColumnDescription(fpdest,"SIDE","Side number");
  ahfits::insertColAfter(fpdest,"ENE_TOTAL","D");
  ahfits::setColumnDescription(fpdest,"ENE_TOTAL","Sum of EPI for occurrence");
  ahfits::insertColAfter(fpdest,"PI","I");
  ahfits::setTNull(fpdest,"PI",-999);
  ahfits::setTLmin(fpdest,"PI",0);
  ahfits::setTLmax(fpdest,"PI",2047);
  ahfits::setColumnDescription(fpdest,"PI","Pulse Invariant");
  ahfits::insertColAfter(fpdest,"EVTCAT","1B");
  ahfits::setColumnDescription(fpdest,"EVTCAT","Event category");
  ahfits::insertColAfter(fpdest,"RAWX","1I");
  ahfits::setTNull(fpdest,"RAWX",-1);
  ahfits::setTLmin(fpdest,"RAWX",1);
  ahfits::setTLmax(fpdest,"RAWX",128);
  ahfits::setColumnDescription(fpdest,"RAWX","Pixel X on RAW-coordinate");
  ahfits::insertColAfter(fpdest,"RAWY","1I");
  ahfits::setTNull(fpdest,"RAWY",-1);
  ahfits::setTLmin(fpdest,"RAWY",1);
  ahfits::setTLmax(fpdest,"RAWY",128);
  ahfits::setColumnDescription(fpdest,"RAWY","Pixel Y on RAW-coordinate");
  ahfits::insertColAfter(fpdest,"RECO_STATUS","16X");
  ahfits::setColumnDescription(fpdest,"RECO_STATUS","Reconstruction Status");

}

// ****************************************************************************

void prepareOutputData(hxisgdevtid::SFFRowData& inrow, OutputRowData& outrow) {
  outrow.m_occurrence_id=inrow.m_occurrence_id;

  outrow.m_ene_total=0.;
  outrow.m_ene_total_null=0;
  outrow.m_pi=0;
  outrow.m_pi_null=0;
  outrow.m_rawx=0;
  outrow.m_rawx_null=0;
  outrow.m_rawy=0;
  outrow.m_rawy_null=0;
  outrow.m_layer=0;
  outrow.m_layer_null=0;
  outrow.m_readout_id_index=0;
  outrow.m_readout_id_index_null=0;
  outrow.m_evtcat=0;
  for (int ii=0; ii < hxisgdevtid::HXI_SIZE_RECO_STATUS; ii++) outrow.m_reco_status[ii]=0;
  for (int ii=0; ii < NSIDE; ii++) outrow.m_signal[ii]=0;
  for (int ii=0; ii < NSIDE; ii++) outrow.m_sigpos[ii]=0;
  for (int ii=0; ii < NSIDE; ii++) outrow.m_sigepi[ii]=0.;
  for (int ii=0; ii < NLAYER; ii++) outrow.m_epitop[ii]=0.;
  for (int ii=0; ii < NLAYER; ii++) outrow.m_epibot[ii]=0.;
  for (int ii=0; ii < NLAYER; ii++) outrow.m_epicut[ii]=0;
  for (int ii=0; ii < NLAYER; ii++) outrow.m_epicut_null[ii]=1;    // default EPICUT[i] = NULL
  for (int ii=0; ii < NSIDE; ii++) outrow.m_goodbad[ii]=0;
  for (int ii=0; ii < NLAYER; ii++) outrow.m_validhits[ii]=0;

  outrow.m_actx=0;
  outrow.m_actx_null=0;
  outrow.m_acty=0;
  outrow.m_acty_null=0;
  outrow.m_detx=0;
  outrow.m_detx_null=0;
  outrow.m_dety=0;
  outrow.m_dety_null=0;
  outrow.m_focx=0;
  outrow.m_focx_null=0;
  outrow.m_focy=0;
  outrow.m_focy_null=0;
  outrow.m_x=0;
  outrow.m_x_null=0;
  outrow.m_y=0;
  outrow.m_y_null=0;

}

// ****************************************************************************

void setRecoStatusAm241(char* reco_status) {
  reco_status[0]=1;
}

// ****************************************************************************

void setRecoStatusMode(int mode, char* reco_status) {
  if (mode == hxisgdevtid::e_PSEUDO) reco_status[1]=1;
  if (mode == hxisgdevtid::e_CALMODE) reco_status[2]=1;
  if (mode == hxisgdevtid::e_READALL) reco_status[3]=1;
  if (mode == hxisgdevtid::e_NOTSURE) reco_status[4]=1;
}

// ****************************************************************************

void setRecoStatusBadStatus(char* reco_status) {
  reco_status[5]=1;
}

// ****************************************************************************

void setRecoStatusFastBGO(char* reco_status) {
  reco_status[8]=1;
}

// ****************************************************************************

void setRecoStatusSkipReco(char* reco_status) {
  reco_status[6]=1;
}

// ****************************************************************************

void setRecoStatusBadReco(char* reco_status) {
  reco_status[9]=1;
}

// ****************************************************************************

void setRecoStatusNoSignal(char* reco_status) {
  reco_status[7]=1;
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

void setOutputToNull(OutputRowData& outrow) {
  outrow.m_rawx_null=1;
  outrow.m_rawy_null=1;
  outrow.m_ene_total_null=1;
  outrow.m_pi_null=1;
  outrow.m_layer_null=1;
  outrow.m_actx_null=1;
  outrow.m_acty_null=1;
  outrow.m_detx_null=1;
  outrow.m_dety_null=1;
  outrow.m_focx_null=1;
  outrow.m_focy_null=1;
  outrow.m_x_null=1;
  outrow.m_y_null=1;
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: hxievtid.cxx,v $
 Revision 1.60  2016/04/18 18:43:38  rshill
 Changed wording on bit description.

 Revision 1.59  2016/04/18 17:29:31  rshill
 Added status bits section to prologue.

 Revision 1.58  2016/04/13 21:31:37  rshill
 Added progress indicators.

 Revision 1.57  2016/04/07 20:32:31  mdutka
 Moving high volumne log messages to debug stream

 Revision 1.56  2016/04/01 16:47:04  mdutka
 adding parameter logging to standard main

 Revision 1.55  2015/12/29 16:53:40  rshill
 Added checkEmptyTable().

 Revision 1.54  2015/11/19 22:09:06  mwitthoe
 hxievtid: add column comments

 Revision 1.53  2015/11/04 21:18:08  mwitthoe
 hxievtid: add datamode parameter allowing user to override DATAMODE keyword in event file

 Revision 1.52  2015/08/23 23:47:32  mwitthoe
 hxievtid: add ENE_TOTAL column to main and extra output files; in both cases, ENE_TOTAL contains the energy used in the PI calculation

 Revision 1.51  2015/08/13 04:47:32  mwitthoe
 hxievtid: add standard prologue; stamp parameters to log file; add FITS column descriptions to output file; add log statements; see issues 532 & 534

 Revision 1.50  2015/07/30 14:32:25  mwitthoe
 hxievtid bug fixes: 1) change goodbad variable type to match FITS column type (10I instead of 10X); 2) fix epicut counter

 Revision 1.49  2015/07/14 17:01:17  klrutkow
 added caldb resolve queries

 Revision 1.48  2015/04/20 14:40:52  mwitthoe
 hxievtid: during a telecon between Lorella, Hiro, and Mike W on 2015-Apr-20, Hiro informed us that the energy-cut CALDB file is always with respect to the bottom-side EPI, instead of the n-side EPI; therefore, it is not necessary to test the layer index when selecting which EPI to use in the CALDB lookup.

 Revision 1.47  2015/04/03 18:16:39  mwitthoe
 hxievtid: update boolean parameters; remove some commented-out code

 Revision 1.46  2015/04/01 19:26:54  mwitthoe
 hxievtid: fix some compiler warnings from the 32-bit linux compiler

 Revision 1.45  2015/03/18 19:52:59  mwitthoe
 hxievtid: change DETNAME to DETNAM

 Revision 1.44  2015/03/17 16:21:57  mwitthoe
 hxievtid: switch to updated badpix library (hxisgdevtid); change ENER* columns to EPI*

 Revision 1.43  2015/03/03 18:28:04  mwitthoe
 hxievtid: changes after Japan trip (Feb 2015): 1) remove expand mode; 2) reorder RECO_STATUS flags; 3) add requirement that top and bottom layer EPI values must be consistent (new CALDB)

 Revision 1.41  2014/12/30 16:30:59  mwitthoe
 hxievtid: use new form of load() function for the badpix CALDB file; now we pass only a portion of the DATAMODE keyword (e.g. NORMAL1, not CAMERA_NORMAL1)

 Revision 1.40  2014/12/23 20:24:50  mwitthoe
 hxievtid: make fixes to parameter format; issue 472

 Revision 1.39  2014/12/23 19:54:44  mwitthoe
 hxievtid: update parameter names/descriptions; see issue 472

 Revision 1.38  2014/12/16 15:45:46  mwitthoe
 hxievtid: change badpix CALDB search from using TSTART to DATAMODE; see issue 466

 Revision 1.37  2014/12/10 20:04:21  mwitthoe
 hxievtid: update tool according to issue 466; main change is to not reject occurrences which have a bad layer due to NULL or below-threshold signals

 Revision 1.36  2014/12/04 18:39:19  mwitthoe
 hxievtid: add include statement for ahgen

 Revision 1.35  2014/11/19 16:09:10  mwitthoe
 hxievtid: fix bug discovered by Hiro where signal PHA values were not being output when the column parameter was set to PHA

 Revision 1.34  2014/11/19 14:44:52  mwitthoe
 hxievtid: fix bug identified by Andy and Hiro where the layers of 2-hit events were not correctly identified when one of the hits occured in the first layer (layer=0); this caused the EVTCAT output column to be set to the wrong value

 Revision 1.33  2014/11/12 16:38:58  mwitthoe
 hxievtid: copy TSTART/TSTOP keywords from input file to output and extra output files

 Revision 1.32  2014/11/12 16:26:02  mwitthoe
 hxievtid: copy TSTART/TSTOP keywords from input file to output and extra output files

 Revision 1.31  2014/11/12 14:29:00  mwitthoe
 hxievtid: fix bug - some occurrences with invalid hits were being categorized as a valid event; add new event category for bad occurrences (bad STATUS, PROC_STATUS, or FAST_BGO)

 Revision 1.30  2014/11/05 17:00:28  mwitthoe
 hxievtid: bring output file formats up-to-date with latest FITS templates; see issue 459

 Revision 1.29  2014/10/28 20:22:24  mwitthoe
 hxievtid: fix case where invalid hits were being written to the extra output file

 Revision 1.28  2014/09/15 20:41:01  mwitthoe
 hxievtid: add support for extended syntax; issue 179

 Revision 1.27  2014/08/14 18:14:44  mwitthoe
 hxievtid: doxygen correction so that the tool appears in the documentatioon

 Revision 1.26  2014/07/24 20:14:30  mwitthoe
 hxievtid: fix expand option

 Revision 1.25  2014/07/23 18:36:29  mwitthoe
 hxievtid: change how outfileextra parameter is handled to match sgdevtid

 Revision 1.24  2014/07/23 18:15:34  mwitthoe
 hxievtid: remove extrainfo parameter and add expand and column parameters: trf_hxievtid_14-06-09

 Revision 1.23  2014/07/22 21:22:14  mwitthoe
 hxievtid: add support for DATAMODE=NORMAL1, NORMAL2, etc

 Revision 1.22  2014/05/23 19:57:09  mwitthoe
 hxievtid: add new, optional output file which will contain calibration data in the format expected by the general gain fitting tool (see issue 385)

 Revision 1.21  2014/05/09 20:26:21  mwitthoe
 hxievtid: correct FITS types for the EVTCAT and LAYER columns

 Revision 1.20  2014/05/09 19:52:26  mwitthoe
 hxievtid: update tool to use new versions of the CALDB access routines

 Revision 1.19  2014/05/08 18:47:24  mwitthoe
 hxievtid: add documentation; bring variable names into better alignment with TRF; move variable declarations to top of scope

 Revision 1.18  2014/05/07 20:34:21  mwitthoe
 hxievtid: improve comments

 Revision 1.17  2014/03/18 20:26:40  mwitthoe
 hxievtd: print number of events per layer at the end of the log file

 Revision 1.16  2014/03/12 20:58:19  mwitthoe
 hxievtid: intermediate stage of calculation was storing EPI as an integer, now is a double; RAWX/Y was being associated with the top/bottom side of the detector, switched to bottom/top; exclude signals below threshold when checking the signal pattern within a layer; add extension name argument to load() routine for badpixel CALDB file

 Revision 1.15  2014/02/27 01:59:24  mwitthoe
 hxievtid: fix 2 bugs: 1) SIGNAL column wasn't being filled; 2) SIGEPI did not sum EPI over all signals in layer

 Revision 1.14  2014/02/23 02:46:22  mwitthoe
 hxievtid: add occurrence_id parameter to restrict operation on a single row; add debug statements; add operation counts; minor debugging

 Revision 1.13  2014/02/04 14:33:02  mwitthoe
 hxievtid: switch to including cstdlib to get integer version of abs(); cmath only has float versions of abs()

 Revision 1.12  2014/02/04 14:21:24  mwitthoe
 hxievtid: fix absolute value call; the abs() function wants a float-type, not an integer type

 Revision 1.11  2014/02/03 16:32:08  mwitthoe
 hxievtid: tweak code and add comments to bring alignment of source closer to TRF

 Revision 1.10  2014/01/22 01:43:50  klrutkow
 klrutkow: CR: comments

 Revision 1.9  2014/01/21 21:10:06  mwitthoe
 hxievtid: revise according to code review; issue 331

 Revision 1.8  2014/01/16 18:24:29  mwitthoe
 hxievtid tool: remove local CALDB access libraries; use those in the hxisgdevtid library instead

 Revision 1.7  2014/01/14 19:25:21  rshill
 Code review comments

 Revision 1.6  2014/01/09 21:00:47  driethmi
 Added comment in main to maybe combine extra files into lib.cxx and lib.h

 Revision 1.5  2014/01/09 19:56:28  asargent
 More comments for CR

 Revision 1.4  2014/01/03 22:31:12  mwitthoe
 hxievtid: update standard main, see issue 327

 Revision 1.3  2013/12/18 22:27:29  mwitthoe
 hxievtid: added LAYER_INDEX column to test remap CALDB file; use LAYER_INDEX instead of SENSOR_ID in hxisgdmap library; change EPI from I-type to E-type; add EPI-to-PI conversion; set STATUS column to 8 bits; fixed fastbgo check; see issue 322 for details

 Revision 1.2  2013/12/16 17:43:57  mwitthoe
 hxievtid: got a build error on some systems from the std::abs() calls where the argument was a int leading to ambiguity on how to convert it to a floating-point value; added explicit typecasting of the argument to double to resolve the problem

 Revision 1.1  2013/12/12 21:38:19  mwitthoe
 new tool hxievtid: incomplete, but builds


*/

