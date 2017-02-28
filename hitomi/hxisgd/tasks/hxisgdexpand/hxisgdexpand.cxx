/// \file hxisgdexpand.cxx
/// \brief Expand HXI and SGD SFF files
/// \author Kristin Rutkowski
/// \date $Date: 2016/04/07 21:23:39 $

/** 

\defgroup tool_hxisgdexpand Expand HXI and SGD SFF files (hxisgdexpand)
@ingroup mod_hxisgd_tasks

Expand events from the HXI SFF.  Each signal for an occurrence is expanded 
into a separate row in the output file.

STATUS bits set:  none

RECO_STATUS (reconstruction status) bits set:

 Bit number     Instrument     Description
  0              HXI            AM241
  1              HXI & SGD      PSEUDO
  2              HXI & SGD      CALMODE
  3              HXI & SGD      READALL 
  4              HXI & SGD      NOTSURE
  5              HXI & SGD      bad PROC_STATUS
  7              HXI & SGD      no signals

Source files:

  hxisgdexpand.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  hitomi/gen/lib/ahapp
  hitomi/hxisgd/lib/hxisgdevtid
  hitomi/mission/lib/ahmission
  hitomi/sxs/lib/ahsxs

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-12  KLR     Clean-up code



*/
 
#define AHLABEL tool_hxisgdexpand
#define AHCVSID "$Id: hxisgdexpand.cxx,v 1.16 2016/04/07 21:23:39 mdutka Exp $"

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
#include "ahmission/keyword.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <string>
#include <cstdlib>       // std::abs


/** \addtogroup tool_hxisgdexpand
 *  @{
 */



/// \brief RECO_STATUS bits
const int AM241_BIT = 0;
const int PSEUDO_BIT = 1;
const int CALMODE_BIT = 2;
const int READALL_BIT = 3;
const int NOTSURE_BIT = 4;
const int BAD_STATUS_BIT = 5;
const int NO_SIGNAL_BIT = 7;

// enum listing two possible instruments for this tool
enum instrument_e {e_HXI, e_SGD};


/// \brief structure to hold input parameters
struct Par {

  Par() :
    m_occurrenceid(0) {}

  std::string m_infile;             ///< Input FITS file name
  std::string m_outfile;            ///< Output FITS file name
  std::string m_remapfile;          ///< FITS file to convert READOUT_ID_RMAP to RAWX, RAWY, and LAYER
  std::string m_badpixfile;         ///< FITS file containing energy thresholds
  long m_occurrenceid;              ///< If >= 0, process only occurrence with this ID
  std::string m_expandcol;          ///< Name of column to expand
  
};


/// \brief structure to hold a single row of data of the output file; 
///  columns referring to either a bit type or a variable-length array, require
///  two members in the struct: one for data and one for the number of elements.
///  Columns which can be set to NULL also require a separate member.
struct OutputRowData {

  OutputRowData(const std::string& inst) :
                m_time(0), m_occurrence_id(0), m_category(0), m_livetime(0),
                m_readout_id_index(0),
                m_readout_id_index_null(0), 
                m_pi(0), m_pi_null(0), num_reco_status(0), 
                m_layer(0), m_layer_null(0), 
                m_rawx(0), m_rawx_null(0), 
                m_rawy(0), m_rawy_null(0),
                m_mattype(0), m_mattype_null(0)
  {
    
    // choose size based on instrument
    if (ahgen::strtoupper(inst) == "SGD") {
      num_status        = hxisgdevtid::SGD_SIZE_STATUS;
      num_proc_status   = hxisgdevtid::SGD_SIZE_PROC_STATUS;
      num_flag_seu      = hxisgdevtid::SGD_SIZE_FLAG_SEU;
      num_flag_lchk     = hxisgdevtid::SGD_SIZE_FLAG_LCHK;
      num_flag_trig     = hxisgdevtid::SGD_SIZE_FLAG_TRIG;
      num_flag_trigpat  = hxisgdevtid::SGD_SIZE_FLAG_TRIGPAT;
      num_flag_hitpat   = hxisgdevtid::SGD_SIZE_FLAG_HITPAT;
      num_flag_fastbgo  = hxisgdevtid::SGD_SIZE_FLAG_FASTBGO;
      num_reco_status   = hxisgdevtid::SGD_SIZE_RECO_STATUS;
      num_flag_lchkmio  = hxisgdevtid::SGD_SIZE_FLAG_LCHKMIO;
      num_flag_ccbusy   = hxisgdevtid::SGD_SIZE_FLAG_CCBUSY;
      num_flag_hitpat_cc= hxisgdevtid::SGD_SIZE_FLAG_HITPAT_CC;
      num_flag_calmode  = hxisgdevtid::SGD_SIZE_FLAG_CALMODE;
    } else if (ahgen::strtoupper(inst) == "HXI") {
      num_status        = hxisgdevtid::HXI_SIZE_STATUS;
      num_proc_status   = hxisgdevtid::HXI_SIZE_PROC_STATUS;
      num_flag_seu      = hxisgdevtid::HXI_SIZE_FLAG_SEU;
      num_flag_lchk     = hxisgdevtid::HXI_SIZE_FLAG_LCHK;
      num_flag_trig     = hxisgdevtid::HXI_SIZE_FLAG_TRIG;
      num_flag_trigpat  = hxisgdevtid::HXI_SIZE_FLAG_TRIGPAT;
      num_flag_hitpat   = hxisgdevtid::HXI_SIZE_FLAG_HITPAT;
      num_flag_fastbgo  = hxisgdevtid::HXI_SIZE_FLAG_FASTBGO;
      num_reco_status   = hxisgdevtid::HXI_SIZE_RECO_STATUS;
    } else {
      AH_THROW_LOGIC("invalid inst value: "+inst+"; should be HXI or SGD");
    }
    
    
    for (ahfits::IndexType ii=0; ii < num_status; ii++) m_status[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_seu; ii++) m_flag_seu[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_lchk; ii++) m_flag_lchk[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_trig; ii++) m_flag_trig[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_trigpat; ii++) m_flag_trigpat[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_hitpat; ii++) m_flag_hitpat[ii]=0;
    for (ahfits::IndexType ii=0; ii < num_flag_fastbgo; ii++) m_flag_fastbgo[ii]=0;

  }
  
  // For the sizes of all the arrays below, use the constant SGD size for 
  // each, unless the HXI constant larger than SGD

  // The following columns are copied from the input file without modification:
  
  // both types of file: HXI and SGD
  double m_time;                                            ///< TIME column - 1D
  long m_occurrence_id;                                     ///< OCCURRENCE_ID column - 1J
  int m_category;                                           ///< CATEGORY column - 1I
  unsigned long m_livetime;                                 ///< LIVETIME column - 1J
  char m_status[hxisgdevtid::SGD_SIZE_STATUS];              ///< STATUS column - 8X
  ahfits::IndexType num_status;                             ///< number of STATUS bits
  char m_proc_status[hxisgdevtid::SGD_SIZE_PROC_STATUS];    ///< STATUS column - 32X
  ahfits::IndexType num_proc_status;                        ///< number of PROC_STATUS bits
  char m_flag_seu[hxisgdevtid::HXI_SIZE_FLAG_SEU];          ///< FLAG_SEU column - 5X
  ahfits::IndexType num_flag_seu;                           ///< number of FLAG_SEU bits
  char m_flag_lchk[hxisgdevtid::HXI_SIZE_FLAG_LCHK];        ///< FLAG_LCHK column - 5X
  ahfits::IndexType num_flag_lchk;                          ///< number of FLAG_LCHK bits
  char m_flag_trig[hxisgdevtid::HXI_SIZE_FLAG_TRIG];        ///< FLAG_TRIG column - 8X (HXI)
  ahfits::IndexType num_flag_trig;                          ///< number of FLAG_TRIG bits/bytes
  char m_flag_trig_sgd;                                     ///< FLAG_TRIG column - 1B (SGD)
  char m_flag_trigpat[hxisgdevtid::SGD_SIZE_FLAG_TRIGPAT];  ///< FLAG_TRIGPAT column - 8X
  ahfits::IndexType num_flag_trigpat;                       ///< number of FLAG_TRIG_PAT bits
  char m_flag_hitpat[hxisgdevtid::SGD_SIZE_FLAG_HITPAT];    ///< FLAG_HITPAT column - 2X
  ahfits::IndexType num_flag_hitpat;                        ///< number of FLAG_HITPAT bits
  char m_flag_fastbgo[hxisgdevtid::SGD_SIZE_FLAG_FASTBGO];  ///< FLAG_FASTBGO column - 2X
  ahfits::IndexType num_flag_fastbgo;                       ///< number of FLAG_FASTBGO bits

  // only SGD copies these columns:
  char m_flag_lchkmio[hxisgdevtid::SGD_SIZE_FLAG_LCHKMIO];      // 1X
  ahfits::IndexType num_flag_lchkmio;
  char m_flag_ccbusy[hxisgdevtid::SGD_SIZE_FLAG_CCBUSY];        // 3X
  ahfits::IndexType num_flag_ccbusy;
  char m_flag_hitpat_cc[hxisgdevtid::SGD_SIZE_FLAG_HITPAT_CC];  // 3X
  ahfits::IndexType num_flag_hitpat_cc;
  char m_flag_calmode[hxisgdevtid::SGD_SIZE_FLAG_CALMODE];      // 1X
  ahfits::IndexType num_flag_calmode;
  
  // The remaining columns are computed by this task:
  
  // both types of file: HXI and SGD
  int m_readout_id_index;                                   ///< READOUT_ID_INDEX column - 1I
  char m_readout_id_index_null;                             ///< READOUT_ID_INDEX NULL flag; 0=not NULL, 1=NULL 
  int m_pi;                                                 ///< PI column - 1I
  char m_pi_null;                                           ///< PI NULL flag; 0=not NULL, 1=NULL
  char m_reco_status[hxisgdevtid::SGD_SIZE_RECO_STATUS];    ///< RECO_STATUS column - 3X
  ahfits::IndexType num_reco_status;                        ///< number of RECO_STATUS bits
  
  // HXI only
  int m_layer;                                              ///< LAYER column - 1B
  char m_layer_null;                                        ///< LAYER NULL flag; 0=not NULL, 1=NULL
  int m_rawx;                                               ///< RAWX column - 1I
  char m_rawx_null;                                         ///< RAWX NULL flag; 0=not NULL, 1=NULL
  int m_rawy;                                               ///< RAWY column - 1I
  char m_rawy_null;                                         ///< RAWY NULL flag; 0=not NULL, 1=NULL
  
  // SGD only
  // specifies the material type where the event was 
  // triggered; 1 - Si layer, 2 - CdTe layer, 3 - multiple layers.  MATTYPE
  // is NULL when PI is NULL.
  int m_mattype;                                            ///< MATTYPE column - 1I
  char m_mattype_null;                                      ///< MATTYPE NULL flag;
  
};  // end struct OutputRowData


/// \brief Get all parameter values.
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief 1) Open and check input file; 2) load data from all CALDB files;
///  3) create empty output file.
/// \param[in] par               structure with parameter values
/// \param[out] fpin             ahfits file pointer to infile
/// \param[out] instrument       instrument of events file: HXI or SGD
/// \param[out] fpout            ahfits file pointer to outfile
/// \param[out] alive_asic       value of ALIVE_ASIC column from badpix file
/// \param[out] remap_table      Data from remapfile 
void initialize(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
                instrument_e& instrument, int& alive_asic,
                hxisgdevtid::remap::DataType& remap_table);

/// \brief Perform event expansion algorithm.
/// \param[in] par               structure with parameter values
/// \param[in] fpin              ahfits file pointer to infile
/// \param[in] fpout             ahfits file pointer to outfile
/// \param[in] instrument        instrument of events file: HXI or SGD
/// \param[in] alive_asic        value of ALIVE_ASIC column from badpix file
/// \param[out] remap_table       Data from remapfile
void doWork(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
            instrument_e instrument, int alive_asic,
            hxisgdevtid::remap::DataType& remap_table);

/// \brief Close FITS files; clear allocated memory.
/// \param[in] fpin              ahfits file pointer to infile
/// \param[in] fpout             ahfits file pointer to outfile
/// \param[in] remap_table       Data from remapfile 
void finalize(ahfits::FilePtr fpin, ahfits::FilePtr fpout,
              hxisgdevtid::remap::DataType & remap_table);

/// \brief Create output file.
/// \param[in] fpsrc             ahfits file pointer to EVENTS extension of input file
/// \param[in] outfile           name of output file
/// \param[in] instrument        instrument of events file: HXI or SGD
/// \param[out] fpdest           output ahfits file pointer to output file
void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      instrument_e instrument, ahfits::FilePtr& fpdest);

/// \brief Initialize output row data and copy values from input row.  This
///  function will be used to prepare output data for the file and the
///  extra output file with calibration data.
/// \param[in] instrument        instrument of events file: HXI or SGD
/// \param[in] inrow             input row data
/// \param[in] outrow            output row data
void prepareOutputData(instrument_e instrument, hxisgdevtid::SFFRowData& inrow, 
                       OutputRowData& outrow);

/// \brief Set RECO_STATUS bits for occurrence type
/// \param[in] mode              CAMERA, AM241, PSEUDO, CALMODE, READALL, or NOTSURE
/// \param[in,out] reco_status   RECO_STATUS array of bits
void setRecoStatusMode(int mode, char* reco_status);

/// \brief Set status bit of RECO_STATUS
/// \param[in,out] reco_status   RECO_STATUS array of bits
/// \param[in] bit               which bit to set
void setRecoStatusBit(int bit, char* reco_status);

/// \brief Set appropriate output columns to NULL to indicate invalid event.
/// \param[out] outrow           struct with output data for single row
void setOutputToNull(OutputRowData& outrow);


// ****************************************************************************

/// \brief sxspi tool
int main(int argc, char** argv) {

  // variable to hold parameter values
  Par par;      

  // FITS file pointers
  ahfits::FilePtr fpin=0;        // ahfits file structure for input file
  ahfits::FilePtr fpout=0;       // ahfits file structure for output file
  
  // 
  instrument_e instrument;
  
  // variables to store CALDB data
  hxisgdevtid::remap::DataType remap_table;       // convert READOUT_ID_RMAP to RAWX, RAWY, and LAYER
  int alive_asic=0;                               // ALIVE_ASIC value from badpix CALDB file

  int status = ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      
      // Write all parameters to the log file.  When debug=yes, the params will 
      // be written twice: here, and again at the end of initialize.  That is 
      // to insure against a possible failure in initialize().
      ahapp::writeParametersToLog();
      
      initialize(par,fpin,fpout,instrument,alive_asic,remap_table);
      doWork(par,fpin,fpout,instrument,alive_asic,remap_table);
      finalize(fpin,fpout,remap_table);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fpin,fpout,instrument,alive_asic,remap_table);
        doWork(par,fpin,fpout,instrument,alive_asic,remap_table);
      } catch (const std::exception &x) {
        // Write all parameters to the log file.  This may be the second time 
        // they are written (the first was at the end of initialize()), just 
        // in case there was a failure in initialize() or doWork().
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpin,fpout,remap_table);
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
  
} // end main()

// ****************************************************************************

void getPar(Par& par) {

  par.m_infile=ahapp::getParString("infile");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_remapfile=ahapp::getParString("remapfile");
  par.m_badpixfile=ahapp::getParString("badpixfile");
  par.m_occurrenceid=ahapp::getParInt("occurrenceid");
  par.m_expandcol=ahapp::getParString("expandcol");

} // end getPar()

// ****************************************************************************

void initialize(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
                instrument_e& instrument, int& alive_asic,
                hxisgdevtid::remap::DataType& remap_table) {

  // local variables
  std::string instrume;                 // value of INSTRUME keyword from infile
  std::string detnam;                   // value of DETNAM keyword from infile
  std::string datamode;                 // value of DATAMODE keyword from infile
  std::string datetime;                 // value of DATE-OBS keyword from infile, for CALDB query
  std::string actual_remap;             // name of remap file to read (result from CALDB query)
  std::string actual_badpix;            // name of badpix file to read (result from CALDB query)
  std::string instrument_str;           // instrument string for remap file
  
  // EPI threshold values.  hxisgdevtid::badpix::loadThreshold loads this table from 
  // the badpix file, along with the alive_asic.  This tool will only use 
  // alive_asic.
  hxisgdevtid::badpix::ThresholdTable epithre_table;    
  
  // open input SFF file; check INSTRUME/DETNAM
  ahfits::open(par.m_infile,"",&fpin);
  if (ahfits::isPrimary(fpin)) ahfits::move(fpin,"EVENTS");  // move to EVENTS extension if extended syntax not used
  instrume=ahfits::getKeyValStr(fpin,"INSTRUME");
  detnam=ahfits::getKeyValStr(fpin,"DETNAM");
          
  // Flag which instrument this is
  if ( ahgen::strtoupper(instrume) == "HXI1" || 
       ahgen::strtoupper(instrume) == "HXI2" ) {
    
    instrument = e_HXI;
    instrument_str = "HXI";
    if (ahgen::strtoupper(detnam) != "CAMERA")
      AH_THROW_RUNTIME("input file EVENTS HDU has wrong DETNAM value; should be CAMERA");
    
  } else if ( ahgen::strtoupper(instrume) == "SGD1" || 
              ahgen::strtoupper(instrume) == "SGD2" ) {
    
    instrument = e_SGD;
    instrument_str = "SGD";
    if ( ahgen::strtoupper(detnam) != "CC1" && 
         ahgen::strtoupper(detnam) != "CC2" && 
         ahgen::strtoupper(detnam) != "CC3" ) {
      AH_THROW_RUNTIME("input file EVENTS HDU has wrong DETNAM value; should be CC1, CC2, or CC3");
    }
    
  } else {
    AH_THROW_RUNTIME("input file EVENTS HDU has wrong INSTRUME value; should be HXI1, HXI2, SGD1, or SGD2");
  }
  
  // for CALDB queries
  datetime=ahfits::getKeyValStr(fpin,"DATE-OBS");
  
  // for bad-pixel file
  datamode = ahfits::getKeyValStr(fpin,"DATAMODE");
  
  AH_INFO(ahlog::HIGH) << "INSTRUME = "<< instrume << std::endl;
  AH_INFO(ahlog::HIGH) << "DETNAM = "<< detnam << std::endl;
  AH_INFO(ahlog::HIGH) << "DATAMODE = "<< datamode << std::endl;
  AH_INFO(ahlog::HIGH) << "DATE-OBS = "<< datetime << std::endl;
  
  // Load data from remapping CALDB file.  This structure will provide mappings
  // from READOUT_ID_RMAP to RAWX, RAWY, and LAYER_INDEX.  Uses library in
  // hitomi/hxisgd/lib/hxisgdevtidlib/remap.h
  // Note: the 2nd argument of load() checks that the given remap file has 
  //       INSTRUME=HXI or SGD; the remap file does not include the 1 or 2
  actual_remap = ahmission::caldb::resolve(par.m_remapfile, "remapping", instrument_str, "-", "REMAPPING", datetime);
  ape_trad_set_string("remapfile",actual_remap.c_str());   // to record actual file path in history
  hxisgdevtid::remap::load(actual_remap, instrument_str, remap_table);
  AH_INFO(ahlog::HIGH) << "Remapping file read" << std::endl;

  // read DATAMODE from input file which is used to get bad pixel thresholds
  // from the badpix CALDB file; the DATAMODE will be of the form CAMERA_NORMAL#,
  // but only the NORMAL# will be retained (where # is a number).
  std::size_t found = datamode.rfind("_");   // locate underscore character
  if (found == std::string::npos) 
    AH_THROW_RUNTIME("expecting underscore character in input DATAMODE, e.g. CAMERA_NORMAL1");
  found++;                           // move start position of substring to after underscore
  datamode=datamode.substr(found);   // keep only part of datamode after underscore

  // Load data from badpixel CALDB file.  The structure contains a EPI threshold
  // for each READOUT_ID_RMAP value.  Signals with an EPI value below these
  // thresholds are ignored.  Uses library in
  // hitomi/hxisgd/lib/hxisgdevtidlib/badpix.h
  // Note: the 2nd argument of load() (instrume) checks that the given badpix 
  //       file has the correct INSTRUME keyword
  // Note: the 3rd argument of load() ("CAMERA") locates the correct extension
  //       in the badpix file via DETNAM
  actual_badpix = ahmission::caldb::resolve(par.m_badpixfile, "bad pixel", instrume, detnam, "BADPIX", datetime);
  ape_trad_set_string("badpixfile",actual_badpix.c_str());   // to record actual file path in history
  hxisgdevtid::badpix::loadThreshold(actual_badpix,instrume,detnam,datamode,epithre_table,alive_asic);
  AH_INFO(ahlog::HIGH) << "Bad pixel file read" << std::endl;
  
  // Create output file from scratch.  Will also add additional, expanded,
  // columns.  The output file will also copy the following keyword values 
  // from the input file: TELESCOP, INSTRUME, DETNAM, and DATAMODE.
  createOutputFile(fpin,par.m_outfile,instrument,fpout);
  
  // Write all parameters to the log file
  ahapp::writeParametersToLog();

} // end initialize()

// ****************************************************************************

void doWork(Par& par, ahfits::FilePtr& fpin, ahfits::FilePtr& fpout,
            instrument_e instrument, int alive_asic,
            hxisgdevtid::remap::DataType& remap_table) {

  // variable declarations
  std::string instrument_str; // instrument string
  std::string epicol;         // energy column to connect to: EPI or PHA
  bool epiflag=false;         // true if connecting to EPI column (instead of PHA)
  int nsignal=0;              // Total number of signals in row
  long long krow=0;           // index of current row in input SFF file
  
  if (instrument == e_HXI) {
    instrument_str = "HXI";
  } else if (instrument == e_SGD) {
    instrument_str = "SGD";
  }
  
  // variables to store row data from input and output files
  hxisgdevtid::SFFRowData inrow(instrument_str);
  OutputRowData outrow(instrument_str);
  
  // counters which get reported at end of log file
  long long count_rows=0;                  // number of rows read from input file
  long long count_badstatus=0;             // number of occurrences with a bad STATUS
  long long count_nosignal=0;              // number of occurrences with no signals
  long long count_expand=0;                // number of signals (not occurrences) written
  long long count_layer[5]={0,0,0,0,0};    // count how many events occur per layer; only successful reconstructions are considered
  
  // Determine which energy column we are connecting to.
  if (ahgen::strtoupper(par.m_expandcol) == "EPI") {
    epicol="EPI";
    epiflag=true;
  } else if (ahgen::strtoupper(par.m_expandcol) == "PHA") {
    epicol="PHA";
    epiflag=false;
  } else {
    // not EPI or PHA
    epicol="EPI";
    epiflag=true;
    AH_INFO(ahlog::LOW) << "Invalid value of expandcol parameter; expanding on EPI column" << std::endl;
  }
  AH_OUT << "Output expanded on "<<epicol<<" columns" << std::endl;
  
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
  routin.connectBit(ahfits::e_READONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",inrow.m_readout_id_rmap,inrow.num_readout_id_rmap);
  if (epicol == "EPI") {
    routin.connectVariableLengthArray(ahfits::e_READONLY,epicol,inrow.m_epi,inrow.num_epi,inrow.m_epi_null);
  } else if (epicol == "PHA") {
    // PHA (which are int) columns don't have TNULL
    routin.connectVariableLengthArray(ahfits::e_READONLY,epicol,inrow.m_epi,inrow.num_epi);
  }
  // HXI and SGD SFF files have different formats for the FLAG_TRIP column,
  if (instrument == e_HXI) {
    routin.connectBit(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
  }
  if (instrument == e_SGD) {
    routin.connectScalar(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig[0]);
    // SGD also copies some extra columns
    routin.connectBit(ahfits::e_READONLY,"FLAG_LCHKMIO",inrow.m_flag_lchkmio,inrow.num_flag_lchkmio);
    routin.connectBit(ahfits::e_READONLY,"FLAG_CCBUSY",inrow.m_flag_ccbusy,inrow.num_flag_ccbusy);
    routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT_CC",inrow.m_flag_hitpat_cc,inrow.num_flag_hitpat_cc);
    routin.connectBit(ahfits::e_READONLY,"FLAG_CALMODE",inrow.m_flag_calmode,inrow.num_flag_calmode);
  }
  
  AH_OUT << "All input files read" << std::endl;
  
  // set up connections between local variables and output columns
  ahfits::Router routout(fpout);
  routout.connectScalar(ahfits::e_WRITEONLY,"TIME",outrow.m_time);
  routout.connectScalar(ahfits::e_WRITEONLY,"OCCURRENCE_ID",outrow.m_occurrence_id);
  routout.connectScalar(ahfits::e_WRITEONLY,"CATEGORY",outrow.m_category);
  routout.connectScalar(ahfits::e_WRITEONLY,"LIVETIME",outrow.m_livetime);
  routout.connectBit(ahfits::e_WRITEONLY,"STATUS",outrow.m_status,outrow.num_status);
  routout.connectBit(ahfits::e_WRITEONLY,"PROC_STATUS",outrow.m_proc_status,outrow.num_proc_status);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_SEU",outrow.m_flag_seu,outrow.num_flag_seu);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHK",outrow.m_flag_lchk,outrow.num_flag_lchk);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIGPAT",outrow.m_flag_trigpat,outrow.num_flag_trigpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT",outrow.m_flag_hitpat,outrow.num_flag_hitpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_FASTBGO",outrow.m_flag_fastbgo,outrow.num_flag_fastbgo);
  routout.connectScalar(ahfits::e_WRITEONLY,"PI",outrow.m_pi,&outrow.m_pi_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"READOUT_ID_INDEX",outrow.m_readout_id_index,&outrow.m_readout_id_index_null);
  routout.connectBit(ahfits::e_WRITEONLY,"RECO_STATUS",outrow.m_reco_status,outrow.num_reco_status);
  // HXI and SGD SFF files have different formats for the FLAG_TRIG column
  // and they output different columns
  if (instrument == e_HXI) {
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIG",outrow.m_flag_trig,outrow.num_flag_trig);
    routout.connectScalar(ahfits::e_WRITEONLY,"LAYER",outrow.m_layer,&outrow.m_layer_null);
    routout.connectScalar(ahfits::e_WRITEONLY,"RAWX",outrow.m_rawx,&outrow.m_rawx_null);
    routout.connectScalar(ahfits::e_WRITEONLY,"RAWY",outrow.m_rawy,&outrow.m_rawy_null);
  }
  if (instrument == e_SGD) {
    routout.connectScalar(ahfits::e_WRITEONLY,"FLAG_TRIG",outrow.m_flag_trig[0]);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHKMIO",outrow.m_flag_lchkmio,outrow.num_flag_lchkmio);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CCBUSY",outrow.m_flag_ccbusy,outrow.num_flag_ccbusy);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT_CC",outrow.m_flag_hitpat_cc,outrow.num_flag_hitpat_cc);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CALMODE",outrow.m_flag_calmode,outrow.num_flag_calmode);
    routout.connectScalar(ahfits::e_WRITEONLY,"MATTYPE",outrow.m_mattype, &outrow.m_mattype_null);
  }
  
  // if occurrenceid parameter given, will only process single occurrence
  if (par.m_occurrenceid >= 0) {
    AH_OUT << "Only processing occurrence ID " << par.m_occurrenceid << std::endl;
  }

  if (instrument == e_HXI) {
    AH_INFO(ahlog::LOW) << "Copying columns: TIME, OCCURRENCE_ID, CATEGORY, " << 
                           "FLAG_SEU, FLAG_LCHK, FLAG_TRIG, FLAG_TRIGPAT, " <<
                           "FLAG_HITPAT, FLAG_FASTBGO, LIVETIME, STATUS, " << 
                           "PROC_STATUS" << std::endl;
  }
  if (instrument == e_SGD) {
     AH_INFO(ahlog::LOW) << "Copying columns: TIME, OCCURRENCE_ID, CATEGORY, " << 
                           "FLAG_SEU, FLAG_LCHK, FLAG_TRIG, FLAG_TRIGPAT, " << 
                           "FLAG_HITPAT, FLAG_FASTBGO, LIVETIME, STATUS, " << 
                           "PROC_STATUS, FLAG_LCHKMIO, FLAG_CCBUSY, " << 
                           "FLAG_HITPAT_CC, FLAG_CALMODE" << std::endl;
  }

  // event row loop
  // make sure to start writing from first row of output file
  ahfits::firstRow(fpout);                                                      
  // loop over all input rows
  for (ahfits::firstRow(fpin); ahfits::readOK(fpin); ahfits::nextRow(fpin)) {   
    krow++;
    ahfits::readRow(fpin);                 // read input row data into inrow structure
    
    nsignal=inrow.num_readout_id_rmap;     // number of signals found in occurrence
    
    // when occurence_id parameter >= 0; process single occurrence
    if (par.m_occurrenceid >= 0 && inrow.m_occurrence_id != par.m_occurrenceid) continue;
    
    AH_DEBUG << "Processing row: " << krow << "   Num signals: " << nsignal << std::endl;

    // Prepare output data structure
    // 1. Copy row data from input file to output file: TIME, OCCURRENCE_ID,
    //    CATEGORY, LIVETIME, FLAG_SEU, FLAG_LCHK, FLAG_TRIG, FLAG_TRIGPAT,
    //    FLAG_HITPAT, FLAG_FASTBGO, and STATUS.
    // 2. Initialize other output columns: PI, RAWX, RAWY, LAYER, 
    //    READOUT_ID_INDEX, RECO_STATUS, MATTYPE.
    prepareOutputData(instrument,inrow,outrow);
    count_rows++;

    // If occurrence has a bad STATUS, set last bit of RECO_STATUS, set the
    // output to NULL, write row, and continue to next occurrence.
    if (!procstatus::processRow(inrow.m_proc_status)) {
      AH_DEBUG << "Found bad PROC_STATUS in row " << krow << std::endl;
      setRecoStatusBit(BAD_STATUS_BIT, outrow.m_reco_status);
      setOutputToNull(outrow);
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      count_badstatus++;
      continue;                    // read next input row
    }
    
    // Check if no signals in occurrence
    if (nsignal == 0) {
      AH_DEBUG << "No signals in row " << krow << std::endl;
      setRecoStatusBit(NO_SIGNAL_BIT, outrow.m_reco_status);
      setOutputToNull(outrow);
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      count_nosignal++;
      continue;   // go to next row in SFF
    }
    
    // Set RECO_STATUS bits based on mode (all bits have been initialized to zero)
    bool isAm241=false;                 // will be true if Am241 flag set
    int mode=hxisgdevtid::getOccurrenceMode(instrument_str, inrow, alive_asic, &isAm241);
    if (isAm241) setRecoStatusBit(AM241_BIT, outrow.m_reco_status);    // HXI only
    setRecoStatusMode(mode,outrow.m_reco_status);
    
    // Treat each signal as an event and convert its EPI to PI.  
    // Each signal is then written to a separate row in the output file.
    for (int isig=0; isig < nsignal; isig++) {
      
      if (instrument == e_HXI) {
        outrow.m_rawx=hxisgdevtid::remap::getRAWX(remap_table,inrow.m_readout_id_rmap[isig]);
        outrow.m_rawy=hxisgdevtid::remap::getRAWY(remap_table,inrow.m_readout_id_rmap[isig]);
        outrow.m_layer=hxisgdevtid::remap::getLAYER(remap_table,inrow.m_readout_id_rmap[isig]);
        count_layer[outrow.m_layer]++;    // counts are reported in log file
      }
      
      if (inrow.m_epi_null[isig] == 1) {
        outrow.m_pi_null=1;
      } else if (epiflag) {
        // convert to PI if reading EPI column
        hxisgdevtid::convertEPIToPI(instrument_str,inrow.m_epi[isig],outrow.m_pi,outrow.m_pi_null);
      } else {
        // no conversion if reading PHA column
        outrow.m_pi = static_cast<int>(inrow.m_epi[isig]);
      }
      
      outrow.m_readout_id_index=inrow.m_readout_id_rmap[isig];
      
      if (instrument == e_SGD) {
        // set material type based on layer
        int layer=hxisgdevtid::remap::getLAYER(remap_table,inrow.m_readout_id_rmap[isig]);
        outrow.m_mattype_null=0;
        if (hxisgdevtid::SGDLayerIsSi(layer))
          outrow.m_mattype=1;
        else                      // multiple layers not applicable here
          outrow.m_mattype=2;
      }
      
      ahfits::writeRow(fpout);
      ahfits::nextRow(fpout);
      count_expand++;
      
    } // end-loop over signals
    
  }   // end row loop over input

  AH_INFO(ahlog::HIGH) << "Finished processing SFF file "<< par.m_infile << std::endl;
  
  // report counts
  AH_INFO(ahlog::HIGH) << "OCCURRENCE COUNTS: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  TOTAL PROCESSED (ROWS):                  " << count_rows << std::endl;
  AH_INFO(ahlog::HIGH) << "  BAD STATUS:                              " << count_badstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  NO SIGNALS:                              " << count_nosignal << std::endl;
  AH_INFO(ahlog::HIGH) << "  EXPANDED SIGNALS:                        " << count_expand << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  if (instrument == e_HXI) {
    // counting per layer only really makes sense for HXI, since SGD has too 
    // many layers to bother with this
    AH_INFO(ahlog::HIGH) << "  COUNTS PER LAYER" << std::endl;
    for (int ilayer=0; ilayer < 5; ilayer++)
      AH_INFO(ahlog::HIGH) << "    LAYER " << ilayer << ":  " << count_layer[ilayer] << std::endl;
  }
  
} // end doWork()

// ****************************************************************************

void finalize(ahfits::FilePtr fpin, ahfits::FilePtr fpout,
              hxisgdevtid::remap::DataType & remap_table) {
  ahfits::close(fpin);
  ahfits::close(fpout);
  remap_table.clear();
} // end finalize()

// ****************************************************************************

void createOutputFile(ahfits::FilePtr fpsrc, const std::string& outfile,
                      instrument_e instrument, ahfits::FilePtr& fpdest) {

  // HXI and SGD files have different formats for the columns
  std::string flag_seu_format;
  std::string flag_lchk_format;
  std::string flag_trig_format;
  std::string flag_trigpat_format;
  std::string flag_hitpat_format;
  std::string flag_fastbgo_format;
  std::string reco_status_format;
  std::string proc_status_format;
  std::string status_format;
  std::string flag_lchkmio_format;
  std::string flag_ccbusy_format;
  std::string flag_hitpat_cc_format;
  std::string flag_calmode_format;
  
  if (instrument == e_HXI) {
    flag_seu_format         = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_SEU,"X");
    flag_lchk_format        = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_LCHK,"X");
    flag_trig_format        = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_TRIG,"X");
    flag_trigpat_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_TRIGPAT,"X");
    flag_hitpat_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_HITPAT,"X");
    flag_fastbgo_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_FLAG_FASTBGO,"X");
    reco_status_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_RECO_STATUS,"X");
    proc_status_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_PROC_STATUS,"X");
    status_format           = hxisgdevtid::getColumnFormat(hxisgdevtid::HXI_SIZE_STATUS,"X");
  }
  if (instrument == e_SGD) {
    flag_seu_format         = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_SEU,"X");
    flag_lchk_format        = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_LCHK,"X");
    flag_trig_format        = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_TRIG,"B");
    flag_trigpat_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_TRIGPAT,"X");
    flag_hitpat_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_HITPAT,"X");
    flag_fastbgo_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_FASTBGO,"X");
    reco_status_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_RECO_STATUS,"X");
    proc_status_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_PROC_STATUS,"X");
    status_format           = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_STATUS,"X");
    flag_lchkmio_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_LCHKMIO,"X");
    flag_ccbusy_format      = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_CCBUSY,"X");
    flag_hitpat_cc_format   = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_HITPAT_CC,"X");
    flag_calmode_format     = hxisgdevtid::getColumnFormat(hxisgdevtid::SGD_SIZE_FLAG_CALMODE,"X");
  }
  
  // create empty output file with EVENTS HDU
  ahfits::create(outfile,"",&fpdest);
  ahfits::addEmptyTbl(fpdest,"EVENTS");

  // copy keywords from input file
  ahmission::keyword::copyAllKeywords(fpsrc,fpdest,ahmission::keyword::e_EVENT);

  // copied columns from input
  ahfits::insertColAfter(fpdest,"TIME","1D");
  ahfits::setTUnit(fpdest,"TIME","s");
  ahfits::setColumnDescription(fpdest,"TIME","Seconds from 01 Jan 2014 00:00:00");
  ahfits::insertColAfter(fpdest,"OCCURRENCE_ID","1J");
  ahfits::setColumnDescription(fpdest,"OCCURRENCE_ID","Sequential Number for occurrence");
  ahfits::insertColAfter(fpdest,"CATEGORY","1B");
  ahfits::setColumnDescription(fpdest,"CATEGORY","Data recorder priority");
  ahfits::insertColAfter(fpdest,"FLAG_SEU",flag_seu_format);
  if (instrument == e_HXI) {
    ahfits::setColumnDescription(fpdest,"FLAG_SEU","1-4 Si 5 CdTE SEU flag");
  } else {
    ahfits::setColumnDescription(fpdest,"FLAG_SEU","0=ok 1=single event upset");
  }
  ahfits::insertColAfter(fpdest,"FLAG_LCHK",flag_lchk_format);
  if (instrument == e_HXI) {
    ahfits::setColumnDescription(fpdest,"FLAG_LCHK","1-4 Si 5 CdTE Length Chk flag");
  } else {
    ahfits::setColumnDescription(fpdest,"FLAG_LCHK","0=ok 1=length error");
  }
  ahfits::insertColAfter(fpdest,"FLAG_TRIG",flag_trig_format);
  ahfits::setColumnDescription(fpdest,"FLAG_TRIG","Trigger origin");
  ahfits::insertColAfter(fpdest,"FLAG_TRIGPAT",flag_trigpat_format);
  ahfits::setColumnDescription(fpdest,"FLAG_TRIGPAT","Trigger pattern");
  ahfits::insertColAfter(fpdest,"FLAG_HITPAT",flag_hitpat_format);
  ahfits::setColumnDescription(fpdest,"FLAG_HITPAT","BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"FLAG_FASTBGO",flag_fastbgo_format);
  ahfits::setColumnDescription(fpdest,"FLAG_FASTBGO","Fast BGO shield hit pattern");
  ahfits::insertColAfter(fpdest,"LIVETIME","1V");
  ahfits::setColumnDescription(fpdest,"LIVETIME","Time since previous occurrence");
  ahfits::insertColAfter(fpdest,"PROC_STATUS",proc_status_format);
  ahfits::setColumnDescription(fpdest,"PROC_STATUS","Record bad telemetry or bad values");
  ahfits::insertColAfter(fpdest,"STATUS",status_format);
  ahfits::setColumnDescription(fpdest,"STATUS","Occurrence status Flags");
  if (instrument == e_SGD) {
    ahfits::insertColAfter(fpdest,"FLAG_LCHKMIO",flag_lchkmio_format);
    ahfits::setColumnDescription(fpdest,"FLAG_LCHKMIO","0=ok 1=error MIO received data");
    ahfits::insertColAfter(fpdest,"FLAG_CCBUSY",flag_ccbusy_format);
    ahfits::setColumnDescription(fpdest,"FLAG_CCBUSY","1=CC busy 0=CC not busy");
    ahfits::insertColAfter(fpdest,"FLAG_HITPAT_CC",flag_hitpat_cc_format);
    ahfits::setColumnDescription(fpdest,"FLAG_HITPAT","CC hit pattern");
    ahfits::insertColAfter(fpdest,"FLAG_CALMODE",flag_calmode_format);
    ahfits::setColumnDescription(fpdest,"FLAG_CALMODE","1=calibration mode 0=other");
  }
  
  // columns added from expansion (this tool)
  ahfits::insertColAfter(fpdest,"READOUT_ID_INDEX","1I");
  ahfits::setTNull(fpdest,"READOUT_ID_INDEX",-999);
  ahfits::setColumnDescription(fpdest,"READOUT_ID_INDEX","Readout index");
  if (instrument == e_HXI) {
    ahfits::insertColAfter(fpdest,"LAYER","1B");
    ahfits::setTNull(fpdest,"LAYER",99);
    ahfits::setColumnDescription(fpdest,"LAYER","Layer number 0-3 Si 4 CdTe");
  }
  ahfits::insertColAfter(fpdest,"PI","I");
  ahfits::setTNull(fpdest,"PI",-999);
  ahfits::setTLmin(fpdest,"PI",0);
  ahfits::setTLmax(fpdest,"PI",2047);
  ahfits::setColumnDescription(fpdest,"PI","Pulse Invariant");
  ahfits::insertColAfter(fpdest,"RECO_STATUS",reco_status_format);
  ahfits::setColumnDescription(fpdest,"RECO_STATUS","Reconstruction Status");
  if (instrument == e_HXI) {
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
  }
  if (instrument == e_SGD) {
    ahfits::insertColAfter(fpdest,"MATTYPE","I");
    ahfits::setTNull(fpdest,"MATTYPE",-999);
    ahfits::setColumnDescription(fpdest,"MATTYPE","Material type");
  }
  
} // end createOutputFile()

// ****************************************************************************

void prepareOutputData(instrument_e instrument, hxisgdevtid::SFFRowData& inrow, 
                       OutputRowData& outrow) {
  
  // copy the input into the output
  outrow.m_time=inrow.m_time;
  outrow.m_occurrence_id=inrow.m_occurrence_id;
  outrow.m_category=inrow.m_category;
  outrow.m_livetime=inrow.m_livetime;

  if (instrument == e_HXI) {
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_SEU; ii++)       outrow.m_flag_seu[ii]=inrow.m_flag_seu[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_LCHK; ii++)      outrow.m_flag_lchk[ii]=inrow.m_flag_lchk[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_TRIG; ii++)      outrow.m_flag_trig[ii]=inrow.m_flag_trig[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_TRIGPAT; ii++)   outrow.m_flag_trigpat[ii]=inrow.m_flag_trigpat[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_HITPAT; ii++)    outrow.m_flag_hitpat[ii]=inrow.m_flag_hitpat[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_FLAG_FASTBGO; ii++)   outrow.m_flag_fastbgo[ii]=inrow.m_flag_fastbgo[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_STATUS; ii++)         outrow.m_status[ii]=inrow.m_status[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_PROC_STATUS; ii++)    outrow.m_proc_status[ii]=inrow.m_proc_status[ii];
    for (int ii=0; ii < hxisgdevtid::HXI_SIZE_RECO_STATUS; ii++)    outrow.m_reco_status[ii]=0;
  }
  if (instrument == e_SGD) {
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_SEU; ii++)       outrow.m_flag_seu[ii]=inrow.m_flag_seu[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_LCHK; ii++)      outrow.m_flag_lchk[ii]=inrow.m_flag_lchk[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_TRIG; ii++)      outrow.m_flag_trig[ii]=inrow.m_flag_trig[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_TRIGPAT; ii++)   outrow.m_flag_trigpat[ii]=inrow.m_flag_trigpat[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_HITPAT; ii++)    outrow.m_flag_hitpat[ii]=inrow.m_flag_hitpat[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_FASTBGO; ii++)   outrow.m_flag_fastbgo[ii]=inrow.m_flag_fastbgo[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_STATUS; ii++)         outrow.m_status[ii]=inrow.m_status[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_PROC_STATUS; ii++)    outrow.m_proc_status[ii]=inrow.m_proc_status[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_LCHKMIO; ii++)   outrow.m_flag_lchkmio[ii]=inrow.m_flag_lchkmio[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_CCBUSY; ii++)    outrow.m_flag_ccbusy[ii]=inrow.m_flag_ccbusy[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_HITPAT_CC; ii++) outrow.m_flag_hitpat_cc[ii]=inrow.m_flag_hitpat_cc[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_FLAG_CALMODE; ii++)   outrow.m_flag_calmode[ii]=inrow.m_flag_calmode[ii];
    for (int ii=0; ii < hxisgdevtid::SGD_SIZE_RECO_STATUS; ii++)    outrow.m_reco_status[ii]=0;
  }
  
  // prepare new columns to be written
  
  // both HXI and SGD
  outrow.m_pi=0;
  outrow.m_pi_null=0;
  outrow.m_readout_id_index=0;
  outrow.m_readout_id_index_null=0;
  
  // HXI only
  outrow.m_rawx=0;
  outrow.m_rawx_null=0;
  outrow.m_rawy=0;
  outrow.m_rawy_null=0;
  outrow.m_layer=0;
  outrow.m_layer_null=0;
  
  // SGD only
  outrow.m_mattype=0;
  outrow.m_mattype_null=0;

} // end prepareOutputData()

// ****************************************************************************

void setRecoStatusMode(int mode, char* reco_status) {
  if (mode == hxisgdevtid::e_AM241)   reco_status[AM241_BIT]=1;
  if (mode == hxisgdevtid::e_PSEUDO)  reco_status[PSEUDO_BIT]=1;
  if (mode == hxisgdevtid::e_CALMODE) reco_status[CALMODE_BIT]=1;
  if (mode == hxisgdevtid::e_READALL) reco_status[READALL_BIT]=1;
  if (mode == hxisgdevtid::e_NOTSURE) reco_status[NOTSURE_BIT]=1;
} // end setRecoStatusMode()

// ****************************************************************************

void setRecoStatusBit(int bit, char* reco_status) {
  reco_status[bit]=1;
} // end setRecoStatusBit()

// ****************************************************************************

void setOutputToNull(OutputRowData& outrow) {
  outrow.m_rawx_null=1;
  outrow.m_rawy_null=1;
  outrow.m_pi_null=1;
  outrow.m_layer_null=1;
  outrow.m_readout_id_index_null=1;
  outrow.m_mattype_null=1;
} // end setOutputToNull()

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: hxisgdexpand.cxx,v $
 Revision 1.16  2016/04/07 21:23:39  mdutka
 Moving high volumne logging statements to debug stream

 Revision 1.15  2016/04/06 21:25:27  rshill
 Added STATUS and RECO_STATUS sections to prologue.

 Revision 1.14  2016/03/22 18:47:12  klrutkow
 per issue 610: added writeParametersToLog to two other spots, made string comparisons case-insensitive ; changed library path to hitomi

 Revision 1.13  2015/11/19 22:17:19  mwitthoe
 hxisgdexpand: add column comments

 Revision 1.12  2015/11/17 18:51:06  mwitthoe
 hxisgdexpand: copy all keywords from input file to output

 Revision 1.11  2015/08/13 03:55:41  klrutkow
 tool cleanup (issue 532): shortened prologue (issue 534), added logging

 Revision 1.10  2015/07/15 18:34:11  klrutkow
 use ahmission caldb query instead of local

 Revision 1.9  2015/07/13 00:17:37  klrutkow
 call resolve() to get remap and badpixfile CALDB files

 Revision 1.8  2015/05/01 02:57:52  klrutkow
 updated to write params to log file (issue 485)

 Revision 1.7  2015/04/28 20:19:08  klrutkow
 removed commented section, with different order of columns for comparison with SGD

 Revision 1.6  2015/04/02 14:54:44  klrutkow
 casting inrow PI to an int, to match outrow variable type

 Revision 1.5  2015/03/26 14:56:15  klrutkow
 only report counts per layer for HXI, not SGD

 Revision 1.4  2015/03/23 13:27:08  klrutkow
 added occurrenceid param

 Revision 1.3  2015/03/18 19:52:10  asargent
 Changed DETNAME to DETNAM

 Revision 1.2  2015/03/17 16:29:23  mwitthoe
 hxisgdexpand: remove unused constants; fix dimension bug; switch to new version of badpix library (in hxisgdevtid)

 Revision 1.1  2015/03/03 21:23:21  klrutkow
 created new tool to handle expansion, instead of expanding in hxievtid or sgdevtid

 

*/
