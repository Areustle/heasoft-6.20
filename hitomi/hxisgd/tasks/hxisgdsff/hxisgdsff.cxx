/// \file hxisgdsff.cxx
/// \brief Tool to translate HXI/SGD FFF files to SFF format
/// \author Robert S. Hill
/// \date $Date: 2016/04/18 14:59:48 $

/**

\defgroup tool_hxisgdsff Convert First to Second FITS file (hxisgdsff)
@ingroup mod_hxisgd_tasks

This task converts the FFF for the HXI and SGD into the SFF.  Science
data are unpacked from telemetry format into variable-length
array columns.  For each occurrence, some of the columns have an 
element for each ASIC that has data, and some have an element
for each readout that has data.

Source files:

  hxisgdsff.cxx
  hxisgdsfflib.cxx
  hxisgdsfflib.h

Library dependencies:

  astroh/hxisgd/lib/hxisgdevtid/
  astroh/mission/lib/ahmission/ahmission
  astroh/mission/lib/ahmission/caldb
  astroh/gen/lib/ahapp
  heacore/ahgen/ahgen
  heacore/ahgen/BitBuf
  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils

Status bits updated:  none

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-06  RSH     initial version, after cleaning code

*/

/** \addtogroup tool_hxisgdsff
 *   @{
 *   */

#define AHLABEL tool_hxisgdsff
#define AHCVSID "$Id: hxisgdsff.cxx,v 1.85 2016/04/18 14:59:48 rshill Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "hxisgdsfflib.h"
#include "hxisgdevtid/remap.h"
#include "ahfits/ahfits.h"
#include "ahgen/BitBuf.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahapp/ahapp.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <cstring>
#include <stdexcept>

/// \brief Get parameter values
/// \param[out] infile name of input file, the FFF
/// \param[out] outfile name of output file, the SFF
/// \param[out] remapfile name of FITS file with table of remapped 
///    ASIC and readout IDs, or CALDB if the file exists in the calibration database
void getPar(std::string & infile, std::string & outfile, std::string & remapfile);
  
/// \brief Open files and get instrument identifier
/// \param[in] infile name of input file, the FFF
/// \param[in] outfile name of output file, the SFF
/// \param[in] remapfile name of CALDB file for remapping ASIC and readout IDs
/// \param[out] remap_table structure holding CALDB table data
/// \param[out] fp_out ahfits descriptor for output file, the SFF
/// \param[out] instrume instrument name
void initialize(const std::string & infile, const std::string & outfile, 
    const std::string & remapfile, hxisgdsfflib::RemapTable & remap_table, 
    ahfits::FilePtr & fp_out, std::string & instrume);

/// \brief Perform appropriate FFF-to-SFF conversion for HXI or SGD
/// \param[in] fp_out ahfits descriptor for output file, the SFF
/// \param[in] instrume instrument name
void doWork(hxisgdsfflib::RemapTable remap_table, const ahfits::FilePtr fp_out, 
  const std::string & instrume);

/// \brief Close the output file
/// \param[in] fp_out ahfits descriptor for output file, the SFF
void finalize(hxisgdsfflib::RemapTable & remap_table, ahfits::FilePtr & fp_out);

// ****************************************************************************

int main(int argc, char** argv) {

  std::string infile;       // Name of input FFF
  std::string outfile;      // Name of output SFF
  std::string remapfile;    // Name of remapping CALDB file
  std::string instrume;     // Name of instrument
  hxisgdsfflib::RemapTable remap_table;  // Structure to contain ASIC and readout remapping
  ahfits::FilePtr fp_out = 0;          // Output file descriptor

  int status = ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(infile, outfile, remapfile);
      ahapp::writeParametersToLog(); 
      initialize(infile, outfile, remapfile, remap_table, fp_out, instrume);
      doWork(remap_table, fp_out, instrume);
      finalize(remap_table, fp_out);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile, outfile, remapfile);
        initialize(infile, outfile, remapfile, remap_table, fp_out, instrume);
        doWork(remap_table, fp_out, instrume);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(remap_table, fp_out);
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

void getPar(std::string & infile, std::string & outfile, std::string & remapfile) {

  infile = ahapp::getParString("infile");
  outfile = ahapp::getParString("outfile");
  remapfile = ahapp::getParString("remapfile");

}

// ----------------------------------------------------------------------------

void initialize(const std::string & infile, const std::string & outfile, 
    const std::string & remapfile, hxisgdsfflib::RemapTable & remap_table, 
    ahfits::FilePtr & fp_out, std::string & instrume) {
    
  std::string caldbfile = "";          // Remap file name to be used.
  ahfits::FilePtr fp_caldb = 0;        // File descriptor for remap file.
  std::string ev_telescop = "";        // Value of keyword TELESCOP in FFF.
  std::string ev_instrume = "";        // Value of keyword INSTRUME in FFF.
  std::string ev_detnam = "";          // Value of keyword DETNAM in FFF.
  std::string ev_date_obs = "";        // Value of keyword DATE-OBS in FFF.
  std::string caldb_instrume = "";     // Value of keyword INSTRUME in CALDB file.
  std::string caldb_detnam = "";       // Value of keyword DETNAM in CALDB file.

  int total_columns_found = 0;                   // Actual number of required columns found.
  int n_found = 0;                               // Number of columns matching a string.
  int n_columns_needed = 0;                      // Total number of required columns.
  ahfits::ColumnList output_collist;             // Needed for column name search.
  const int n_req_cols_hxi = 19;                 // Number of required columns, HXI.
  const int n_req_cols_sgd = 23;                 // Number of required columns, SGD.
  std::string columns_needed[n_req_cols_sgd];    // FFF columns required for both HXI and SGD.
  columns_needed[0] = "TIME";
  columns_needed[1] = "S_TIME";
  columns_needed[2] = "ADU_CNT";
  columns_needed[3] = "L32TI";
  columns_needed[4] = "OCCURRENCE_ID";
  columns_needed[5] = "LOCAL_TIME";
  columns_needed[6] = "CATEGORY";
  columns_needed[7] = "FLAGS";
  columns_needed[8] = "FLAG_SEU";
  columns_needed[9] = "FLAG_LCHK";
  columns_needed[10] = "FLAG_TRIG";
  columns_needed[11] = "FLAG_TRIGPAT";
  columns_needed[12] = "FLAG_HITPAT";
  columns_needed[13] = "FLAG_FASTBGO";
  columns_needed[14] = "LIVETIME";
  columns_needed[15] = "NUM_ASIC";
  columns_needed[16] = "RAW_ASIC_DATA";
  columns_needed[17] = "PROC_STATUS";
  columns_needed[18] = "STATUS";
  columns_needed[19] = "FLAG_LCHKMIO";         // SGD only.
  columns_needed[20] = "FLAG_CCBUSY";          // SGD only.
  columns_needed[21] = "FLAG_HITPAT_CC";       // SGD only.
  columns_needed[22] = "FLAG_CALMODE";         // SGD only.
   
  //  Create the FITS file.
  //
  AH_INFO(ahlog::HIGH) << "Input file:   " << infile << std::endl;
  AH_INFO(ahlog::HIGH) << "Output file:  " << outfile << std::endl;
  ahfits::clone(infile, outfile, &fp_out, true);
  if (ahfits::isPrimary(fp_out)) ahfits::move(fp_out, "EVENTS");   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fp_out, infile);

  //  Get the required keywords that identify the hardware
  //  used for the observation.
  //
  ev_telescop = ahfits::getKeyValStr(fp_out, "TELESCOP");
  ev_instrume = ahfits::getKeyValStr(fp_out, "INSTRUME");
  ev_detnam = ahfits::getKeyValStr(fp_out, "DETNAM");
  ev_date_obs = ahfits::getKeyValStr(fp_out,"DATE-OBS");

  AH_INFO(ahlog::HIGH) << "TELESCOP:  " << ev_telescop << "   INSTRUME:  " << ev_instrume
    << "   DETNAM:  " << ev_detnam << std::endl;

  //  Check for the correct spacecraft.
  //
  if (!ahmission::isValidTELESCOP(ev_telescop)) {
    AH_THROW_RUNTIME("Bad TELESCOP keyword in event file");
  }

  //  Make sure the instrument and detector names are correct and consistent.
  //
  if (ahgen::strtoupper(ev_instrume) == "HXI1" || ahgen::strtoupper(ev_instrume) == "HXI2" ) {
    instrume = "HXI";
    if (ev_detnam != "CAMERA") {
      AH_THROW_RUNTIME("DETNAM must be CAMERA for HXI");
    }
    n_columns_needed = n_req_cols_hxi;
  } else if (ahgen::strtoupper(ev_instrume) == "SGD1" || ahgen::strtoupper(ev_instrume) == "SGD2" ) {
    instrume = "SGD";
    if (ahgen::strtoupper(ev_detnam) != "CC1" && ahgen::strtoupper(ev_detnam) != "CC2" && ahgen::strtoupper(ev_detnam) != "CC3") {
      AH_THROW_RUNTIME("DETNAM must be CC1, CC2, or CC3 for SGD");
    }
    n_columns_needed = n_req_cols_sgd;
  } else {
    AH_THROW_RUNTIME("valid INSTRUME not found in FITS file header");
  }
  
  // for CALDB queries
  ev_date_obs = ahfits::getKeyValStr(fp_out,"DATE-OBS");

  //  Check for presence of all required table columns in the EVENTS extension.
  //
  for (int j=0; j<n_columns_needed; ++j) {

    //  Case-insensitive search for columns with a given name.
    //
    n_found = ahfits::searchColumn(fp_out, columns_needed[j], output_collist, false);
    if (n_found == 1) {
      ++total_columns_found;
    } else {
      AH_INFO(ahlog::HIGH) << "Required column not found in FFF: " << columns_needed[j] << std::endl;
      AH_THROW_RUNTIME("Input FFF not valid.");
    }
  }

  //  This should never get tripped.  The maximum number of columns that can 
  //  be found is equal to n_columns_needed, but if fewer are found, the error
  //  will already have been thrown.
  //
  if (total_columns_found != n_columns_needed) {
    AH_THROW_LOGIC ("Column search inconsistency.");
  }

  // Open the CALDB file and check keywords.
  //
  caldbfile = ahmission::caldb::resolve(remapfile, "remapping", instrume, "-", "REMAPPING", ev_date_obs);
  ahfits::open(caldbfile, "", &fp_caldb);          // Opens the file at primary HDU
  ahfits::firstHDU(fp_caldb,ahfits::e_BINARY_TBL); // Moves to first binary table
  if (0 == ahfits::numRows(fp_caldb)) {
    AH_THROW_RUNTIME("CALDB table with ASIC and readout remapping is empty: "+caldbfile);
  }

  // in case the user supplied a remapping file, make sure it's for the 
  // correct instrument
  caldb_instrume = ahfits::getKeyValStr(fp_caldb, "INSTRUME");
  if (ahgen::strtoupper(caldb_instrume) != ahgen::strtoupper(instrume)) {
    AH_THROW_RUNTIME("CALDB INSTRUME (" + caldb_instrume + ") does not match FFF INSTRUME");
  }
  
  // Load the CALDB file data.
  //
  ape_trad_set_string("remapfile",caldbfile.c_str());   // to record actual file path in history
  hxisgdsfflib::loadRemapping(fp_caldb, remap_table);

  // Close the CALDB file.
  //
  ahfits::close(fp_caldb);

  // Write list of parameters to log file
  //
  ahapp::writeParametersToLog(); 

}

// ----------------------------------------------------------------------------

void doWork(hxisgdsfflib::RemapTable remap_table, const ahfits::FilePtr fp_out, 
  const std::string & instrume) {

  const int HXI_MAX_ASICS = 40;
  const int SGD_MAX_ASICS = 208;
  const int HXI_MAX_READOUTS = 1280;
  const int SGD_MAX_READOUTS = 13312;
  const int PROC_STATUS_SIZE = 32;
  const int FLAG_SEU_SIZE = 5;

  //
  //  Storage for existing FFF columns.
  //
  unsigned char num_asic=0;
  char proc_status[PROC_STATUS_SIZE];
  char flag_seu[FLAG_SEU_SIZE];
  ahfits::IndexType num_proc_status = 0;
  ahfits::IndexType num_flag_seu = 0;

  //
  //  Storage for SFF columns.
  //
  //  These have the same names as the SFF columns, except those that
  //  include the tag _short, which designate a 16-bit quantity that applies 
  //  to the SGD.  In these cases, the corresponding HXI quantity is a char.
  //
  short asic_id_short[SGD_MAX_ASICS];            // ASIC ID, SGD.
  char asic_id[HXI_MAX_ASICS];                   // ASIC ID, HXI.

  unsigned char asic_id_rmap[SGD_MAX_ASICS];     // Remapped ASIC ID, HXI & SGD.
  char asic_chip[SGD_MAX_ASICS];                 // ASIC_CHIP bit flags,  HXI & SGD.
  char asic_trig[SGD_MAX_ASICS];                 // ASIC_TRIG bit flags, HXI & SGD.
  char asic_seu[SGD_MAX_ASICS];                  // ASIC_SEU bit flags, HXI & SGD.
  short num_readout[SGD_MAX_ASICS];              // Number of readouts for current ASIC.
  short asic_cmn[SGD_MAX_ASICS];                 // Zero level for each ASIC.
  short asic_ref[SGD_MAX_ASICS];                 // Alternative zero level for each ASIC.

  long long readout_flag_ll[SGD_MAX_ASICS];      // Bit mask of readouts that have data, SGD.
  unsigned long readout_flag[HXI_MAX_ASICS];     // Bit mask of readouts that have data, HXI.

  short readout_asic_id_short[SGD_MAX_READOUTS]; // ASIC identifier for each READOUT, SGD.
  char readout_asic_id[HXI_MAX_READOUTS];        // ASIC identifier for each READOUT, HXI.

  char readout_id[SGD_MAX_READOUTS];             // Readout identifier, HXI & SGD.
  short readout_id_rmap[SGD_MAX_READOUTS];       // Remapped (sequential) readout ID, HXI & SGD.
  short pha[SGD_MAX_READOUTS];                   // Pulse height, HXI & SGD.
  //float epi[SGD_MAX_READOUTS];                 // Calibrated pulse height, HXI & SGD.

  //
  //  Bit fields without data (filler and sentinels)
  //
  unsigned long filler_bits=0, start_bit=0, dm_bit=0, cm_bit=0, stop_bit=0;
  unsigned long end_filler=0;

  //
  //  Control totals
  //
  long row_total_active_readouts=0;    // Number of readouts with a signal, in the current row.
  long row_total_processed_bits=0;     // Number of bits processed for the current row.
  long num_words=0;                    // Number of 32-bit words processed for the current row.
  long num_good_rows=0;                // Number of good rows in the FFF.
  long num_bad_rows=0;                 // Number of bad rows in the FFF.
  long num_do_not_process=0;           // Number of rows skipped in the FFF due to PROC_STATUS.
  long num_format_bad=0;               // Number of rows in the FFF with a bad telemetry format.
  long num_remapping_bad=0;                // Number of instances of failed remapping CALDB lookups.

  //
  //  Status variables
  //
  bool format_ok=true;                 // Telemetry format of current row is OK.
  bool remapping_ok=true;              // Remapping CALDB lookup for current row is OK.

  //
  //  Variables to hold temporary values.
  //
  int current_asic_id=0;               // ASIC_ID for data in process of being unpacked.
  int current_readout_id=0;            // READOUT_ID for current PHA value.
  int current_asic_id_rmap=0;          // ASIC_ID_RMAP for current PHA value.
  int current_readout_id_rmap=0;       // READOUT_ID_RMAP for current PHA value.
  int current_layer=0;                 // LAYER for current PHA value.
  unsigned long long readout_flag_test=0;   // Result of anding READOUT_FLAG with mask.

  //
  // Set up buffer for unpacking bits.  BitBuf contains the routines to
  // strip out bit fields, as well as an internal buffer to hold the
  // telemetry bits that are being processed.
  //
  ahgen::BitBuf raw_asic_data(19552, 8);  // Allocate with 19552*8 bits, which is room
                                          //  enough for the biggest possible SGD telemetry.
  
  //
  // Provide a local variable that will allow ahfits to access the
  // BitBuf internal buffer.
  //
  unsigned char * raw_asic_data_array = (unsigned char *)raw_asic_data.m_buf;

  long n_rows;                            // Number of rows in the FFF binary table.
  std::string n_rows_keyword = "NAXIS2";  // Keyword holding the number of rows.

  long long readout_bitmasks[64];
  
  short readout_id_list_this_asic[64];
  short num_active_readouts = 0;

  ahfits::IndexType asic_data_length=0, asic_length=0, readout_length=0;

  //
  //  Variables characterizing instrument (most are set within the loops below).
  //
  int max_readouts=0, num_bit_asic_id=0, n_filler_bits=0;

  ahfits::Router update_router(fp_out);
  
  //
  //  Set up bitmasks for testing readout flags.
  //
  for (int ibits=0; ibits<64; ++ibits) {
    readout_bitmasks[ibits] = 1ull << ibits;
  }

  //
  // FFF columns that are needed.
  //
  update_router.connectScalar(ahfits::e_READONLY,"NUM_ASIC", num_asic);
  update_router.connectVariableLengthArray(ahfits::e_READONLY,"RAW_ASIC_DATA", raw_asic_data_array, asic_data_length);
  update_router.connectBit(ahfits::e_READWRITE,"PROC_STATUS", proc_status, num_proc_status);
  update_router.connectBit(ahfits::e_READONLY,"FLAG_SEU", flag_seu, num_flag_seu);

  //
  // SFF columns to be filled.
  //
  if (ahgen::strtoupper(instrume) == "HXI") {
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_ID", asic_id, asic_length);
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_FLAG", readout_flag, asic_length);
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ASIC_ID", readout_asic_id, readout_length);
  } else {
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_ID", asic_id_short, asic_length);
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_FLAG", readout_flag_ll, asic_length);
    update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ASIC_ID", readout_asic_id_short, readout_length);
  }
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_ID_RMAP", asic_id_rmap, asic_length);
  update_router.connectBit(ahfits::e_WRITEONLY,"ASIC_CHIP", asic_chip, asic_length);
  update_router.connectBit(ahfits::e_WRITEONLY,"ASIC_TRIG", asic_trig, asic_length);
  update_router.connectBit(ahfits::e_WRITEONLY,"ASIC_SEU", asic_seu, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"NUM_READOUT", num_readout, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_REF", asic_ref, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_CMN", asic_cmn, asic_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ID", readout_id, readout_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ID_RMAP", readout_id_rmap, readout_length);
  update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"PHA", pha, readout_length);
  // Do not connect EPI:  column should be empty, not zero-filled.
  // update_router.connectVariableLengthArray(ahfits::e_WRITEONLY,"EPI", epi, readout_length);

  //  Obtain the number of table rows to be processed.
  //
  n_rows = ahfits::getKeyValLLong(fp_out, n_rows_keyword);

  //  Loop through rows.
  //
  for (long i=0; i<n_rows; ++i) {
    format_ok = true;
    remapping_ok  = true;

    //AH_DEBUG << "HXI/SGD row " << i << std::endl;

    if (i == 0) {
      ahfits::firstRow(fp_out);
    } else {
      ahfits::nextRow(fp_out);
    }

    //  Initialize storage for FFF columns.
    //
    num_asic = 0;
    std::memset(raw_asic_data_array, '\0', 19552);
    for (int j=0; j<32; ++j) proc_status[j] = 0;
    
    //  Initialize storage for SFF columns.
    //
    if (ahgen::strtoupper(instrume) == "HXI") {
      std::memset(asic_id, '\0', sizeof(asic_id));
      std::memset(readout_flag, '\0', sizeof(readout_flag));
      std::memset(readout_asic_id, '\0', sizeof(readout_asic_id));
    } else {
      std::memset(asic_id_short, '\0', sizeof(asic_id_short));
      std::memset(readout_flag_ll, '\0', sizeof(readout_flag_ll));
      std::memset(readout_asic_id_short, '\0', sizeof(readout_asic_id_short));
    }
    std::memset(asic_id_rmap, '\0', sizeof(asic_id_rmap));
    std::memset(asic_chip, '\0', sizeof(asic_chip));
    std::memset(asic_trig, '\0', sizeof(asic_trig));
    std::memset(asic_seu, '\0', sizeof(asic_seu));
    std::memset(asic_ref, '\0', sizeof(asic_ref));
    std::memset(asic_cmn, '\0', sizeof(asic_cmn));
    std::memset(num_readout, '\0', sizeof(num_readout));
    std::memset(readout_id, '\0', sizeof(readout_id));
    std::memset(readout_id_rmap, '\0', sizeof(readout_id_rmap));
    std::memset(pha, '\0', sizeof(pha));
    // std::memset(epi, '\0', sizeof(epi));

    //  Initialize storage for auxiliary array.
    //
    std::memset(readout_id_list_this_asic, '\0', sizeof(readout_id_list_this_asic));

    //  Read FFF data.  This fills the variables connected to FITS table columns above.
    //
    ahfits::readRow(fp_out);    

    //AH_DEBUG << "Row # " << i << " " << "num_asic = " << (int) num_asic << std::endl;
    //AH_DEBUG << "asic_data_length =  " << asic_data_length << std::endl;

    //  Process the row only if proc_status says to do so.
    //
    if (!procstatus::processRow(proc_status)) {

      ++num_do_not_process;   // Failed PROC_STATUS check
      ++num_bad_rows;

    } else {

      //  PROC_STATUS check succeeded - process the row.

      //
      //  Precondition the telemetry buffer: convert 32-bit big-endian word format
      //  to little-endian.
      //
      raw_asic_data.bufByteSwap32(asic_data_length);

      row_total_active_readouts = 0;  // Number of signals for all ASICs, this occurrence.
      row_total_processed_bits = 0;   // Number of bits extracted, this occurrence.
      unsigned long long buf1 = 0ull, readout_flag_buf = 0ull;
      ahfits::IndexType bitcount, fillcount, one_asic_length_bits, one_asic_length_shorts;

      //
      //  The structure of the HXI & SGD electronics is such that readouts (i.e., detection
      //  elements -- which may be either pixels or strips) are associated with ASICs
      //  (i.e., special chips -- Application Specific Integrated Circuits).  There are
      //  many readouts connected in a fixed way with each ASIC.  However, in any given
      //  row, only a varying subset of readouts will have any data.  
      //  
      //  In the FFF, the detected signals are grouped by ASIC identifier.  This outer
      //  loop processes each ASIC.  An inner loop (below) processes in turn each readout
      //  having a signal for that ASIC.
      //
      for (int j=0; j<num_asic; ++j) {

        //AH_DEBUG << "ASIC #" << j << std::endl;

        bitcount = 0;  // Number of bits extracted, this ASIC
       
        //
        //  The raw_asic_data.io function sequentially strips bits out of the telemetry.
        //  This function also stores the bits into a variable, which then is stored
        //  in an output SFF column.  The fields that are processed include science
        //  data.  Also some single bits (CM, DM, start bit, stop bit) are by
        //  definition always set to 1; in the present code, these bits are checked in
        //  order to verify the telemetry format.
        //

        //  Extract ASIC_ID, 8 bits if HXI, 12 bits if SGD.
        //
        if (ahgen::strtoupper(instrume) == "HXI") {
          num_bit_asic_id = 8;
        } else {
          num_bit_asic_id = 12;
        }
        raw_asic_data.io(buf1, num_bit_asic_id, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        bitcount += num_bit_asic_id;
          if (ahgen::strtoupper(instrume) == "HXI") {
          asic_id[j] = buf1;
        } else {
          asic_id_short[j] = buf1;
        }

        //AH_DEBUG << "ASIC_ID = " << std::hex << buf1 << " (hex)" << std::dec << std::endl;

        //
        //  Extract 3 filler bits if HXI.
        //
          if (ahgen::strtoupper(instrume) == "HXI") {
          n_filler_bits = 3;
          raw_asic_data.io(filler_bits, n_filler_bits, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
          if (filler_bits != 0) {
            format_ok = false;  // Filler bits must be zero.
            AH_INFO(ahlog::HIGH) << "Row " << i << ": filler bit bad" << std::endl;
          }
        } else {
          n_filler_bits = 0;
        }
        bitcount += n_filler_bits;

        //
        //  Extract start bit.
        //
        raw_asic_data.io(start_bit, 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (start_bit != 1) {
          format_ok = false;  // Start bit must be one.
          AH_INFO(ahlog::HIGH) << "Row " << i << ": start bit bad" << std::endl;
        }
        bitcount += 1;

        //
        //  Extract ASIC_CHIP bit.
        //
        raw_asic_data.io(asic_chip[j], 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (asic_chip[j] != 1) {
          format_ok = false;  // asic_chip bit must be one.
          AH_INFO(ahlog::HIGH) << "Row " << i << ": asic_chip[" << j << "] bit bad" << std::endl;
        }
        bitcount += 1;
        //AH_DEBUG << "asic_chip[j] = " << std::hex << (int)asic_chip[j] << std::dec << std::endl;

        //
        //  Extract ASIC_TRIG bit.
        //
        raw_asic_data.io(asic_trig[j], 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        bitcount += 1;
        //AH_DEBUG << "asic_trig[j] = " << std::hex << (int)asic_trig[j] << std::dec << std::endl;

        //
        //  Extract ASIC_SEU bit.
        //
        raw_asic_data.io(asic_seu[j], 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if ((int)asic_seu[j] == 1) {
          if (ahgen::strtoupper(instrume) == "SGD") {
            if (flag_seu[0] != 1) {
              format_ok = false;
              AH_INFO(ahlog::HIGH) << "Row " << i << ": (SGD) asic_seu[" << j << "] bit is set" << std::endl;
              AH_INFO(ahlog::HIGH) << "but flag_seu is not set" << std::endl;
            }
          } else {
            hxisgdsfflib::lookupRemapping(remap_table, asic_id[j], 0,
              current_asic_id_rmap, current_readout_id_rmap, current_layer);
            if (flag_seu[current_layer] != 1) {
              format_ok = false;
              AH_INFO(ahlog::HIGH) << "Row " << i << ": (HXI) asic_seu[" << j << "] bit is set" << std::endl;
              AH_INFO(ahlog::HIGH) << "but flag_seu[" << current_layer << "] is not set" << std::endl;
            }
          }
        }

        bitcount += 1;
        //AH_DEBUG << "asic_seu[j] = " << std::hex << (int)asic_seu[j] << std::dec << std::endl;

        //
        //  Extract DM bit.
        //
        raw_asic_data.io(dm_bit, 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (dm_bit != 1) {
          format_ok = false;  // DM bit must be one
          AH_INFO(ahlog::HIGH) << "Row " << i << ": DM bit bad" << std::endl;
        }
        bitcount += 1;

        //
        //  Extract flags for readouts having signals: 32 bits for HXI, 64 bits for SGD.
        //
        if (ahgen::strtoupper(instrume) == "HXI") {
          max_readouts = 32;
        } else {
          max_readouts = 64;
        }
        raw_asic_data.io(readout_flag_buf, max_readouts, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (ahgen::strtoupper(instrume) == "HXI") {
          readout_flag[j] = readout_flag_buf;
        } else {
          readout_flag_ll[j] = readout_flag_buf;
        }
        bitcount += max_readouts;
        //AH_DEBUG << "Readout flags = " << std::hex << readout_flag_ll[j] << std::dec << std::endl;

        //
        //  Extract CM bit.
        //
        raw_asic_data.io(cm_bit, 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (cm_bit != 1) {
          format_ok = false;  // CM bit must be one
          AH_INFO(ahlog::HIGH) << "Row " << i << ": CM bit bad" << std::endl;
        }
        bitcount += 1;

        //
        //  Extract ASIC_REF: 10 bits.
        //
        raw_asic_data.io(asic_ref[j], 10, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        bitcount += 10;

        // Extract number of pulse heights for this ASIC.
        //
        num_active_readouts = 0;
        for (int k=0; k<max_readouts; ++k) {
          readout_flag_test = readout_flag_buf & readout_bitmasks[k];
          if (readout_flag_test != 0) {
              readout_id_list_this_asic[num_active_readouts++] = k;
          }
        }
        if (num_active_readouts > max_readouts) {
          AH_THROW_LOGIC("number of active readouts exceeds maximum possible");
        }
        num_readout[j] = num_active_readouts;
        //AH_DEBUG << "num_active_readouts = " << num_active_readouts << std::endl;

        //
        //  This inner loop processes each readout that has a signal in the occurrence,
        //  for the current ASIC.
        //
        for (int m1=0, m=0; m1<num_active_readouts; ++m1) {

          //AH_DEBUG << "Readout #" << m1 << std::endl;
        
          m = row_total_active_readouts + m1;

          //  Fill "current" variables for CALDB table lookup.
          //
          if (ahgen::strtoupper(instrume) == "HXI") {
            current_asic_id = asic_id[j];
            readout_asic_id[m] = asic_id[j];
          } else {
            current_asic_id = asic_id_short[j];
            readout_asic_id_short[m] = asic_id_short[j];
          }
          current_readout_id = readout_id_list_this_asic[m1]; 
          readout_id[m] = readout_id_list_this_asic[m1];

          //AH_DEBUG << "hxisgdsfflib::lookupRemapping  " << current_asic_id << "  " << current_readout_id << std::endl;

          // Remap the ASIC_ID and READOUT_ID using CALDB table.
          //
          remapping_ok = 
              hxisgdsfflib::lookupRemapping(remap_table, current_asic_id, current_readout_id, 
                current_asic_id_rmap, current_readout_id_rmap, current_layer);

          if (!remapping_ok) {
            AH_INFO(ahlog::HIGH) << "Row " << i << ": remapping info retrieval bad" << std::endl;
            break;
          }

          //AH_DEBUG << "hxisgdsfflib::get  " << current_asic_id_rmap << "  " << current_readout_id_rmap << std::endl;

          asic_id_rmap[j] = current_asic_id_rmap;
          readout_id_rmap[m] = current_readout_id_rmap;

          // Extract the PHA: 10 bits.
          //
          raw_asic_data.io(pha[m], 10, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
          bitcount += 10;

          // EPI is the next field in the SFF, but it is not set by this program.

        }

        //
        // Get out if CALDB lookup failed.
        //
        if (!remapping_ok) break;

        //
        // Keep track of the total number of readouts with signals, across all ASICs.
        //
        row_total_active_readouts += num_active_readouts;

        //
        // Extract ASIC_CMN: 10 bits.
        //
        raw_asic_data.io(asic_cmn[j], 10, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        bitcount += 10;

        //AH_DEBUG << "asic_cmn[j] = " << std::hex << asic_cmn[j] << std::dec << std::endl;

        //
        // Extract stop bit.
        //
        raw_asic_data.io(stop_bit, 1, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
        if (stop_bit != 1) {
          AH_INFO(ahlog::HIGH) << "Row " << i << ": stop bit[" << j << "] bad" << std::endl;
          format_ok = false;  // stop bit must be set
        }
        bitcount += 1;

        //
        // Extract the filler bits at the end of the data for this ASIC (varying number to fill out
        // an integral number of 16-bit halfwords).
        //
        fillcount = 16 - (bitcount % 16);
        if (fillcount < 16) {
          raw_asic_data.io(end_filler, fillcount, ahgen::BitBuf::e_UNPACK, ahgen::BitBuf::e_ORDER_NORMAL);
          if (end_filler != 0) {
            AH_ERR << "bitcount = " << bitcount << "       fillcount = " << fillcount << std::endl;
            AH_ERR << "end_filler = " << std::hex << end_filler << std::dec << std::endl;
            AH_THROW_RUNTIME ("one or more end-of-ASIC filler bits is set");
          }
          bitcount += fillcount;
        }
        row_total_processed_bits += bitcount;

        //
        // Make sure the counters of data items processed for the current ASIC are all
        // mutually consistent.
        //
        // How many bits should we have processed, based on the number of readouts with
        // signals?
        //
        if (ahgen::strtoupper(instrume) == "HXI") {
          one_asic_length_bits = (59 + num_active_readouts*10 + 11);
        } else {
          one_asic_length_bits = (92 + num_active_readouts*10 + 11);
        }
        if (one_asic_length_bits % 16 == 0) {
          one_asic_length_shorts = one_asic_length_bits/16;
        } else {
          one_asic_length_shorts = one_asic_length_bits/16 + 1; 
        }

        //
        // Compare the expected number of bits processed with the actual bit count
        // processed.
        //
        if (bitcount != 16*one_asic_length_shorts) {
          AH_THROW_LOGIC("number of processed bits for an ASIC is inconsistent with expectation");
        }

      }
      asic_length = num_asic;
      readout_length = row_total_active_readouts;

      AH_DEBUG << "Row " << i << ": # active ASICS=" << asic_length
        << "  # active readouts=" << readout_length << std::endl;

      //
      // For the entire row, make sure the amount of data processed is equal to the
      // length of the telemetry block.
      //
      if (remapping_ok) {  // We might have got here via an error - if so, skip this check.
        if (row_total_processed_bits % 32 == 0) {
          num_words = row_total_processed_bits/32;
        } else {
          num_words = row_total_processed_bits/32 + 1; 
        }
        if (4*num_words != asic_data_length) {
          AH_INFO(ahlog::HIGH) << "Number of processed bytes for this row is wrong:" << std::endl;
          AH_INFO(ahlog::HIGH) << "Row # " << i <<  ":   Processed = " << 4*num_words 
                              << "   Expected = " << asic_data_length << std::endl;
          format_ok = false;
        }
      }

      // Track the counts for the various categories of good and bad rows.
      //
      if (format_ok && remapping_ok) {
        ++num_good_rows;      // Good
      } else {
        //
        // TBD:  Do we update PROC_STATUS or STATUS?
        // Example:
        // proc_status |= 2;
        ++num_bad_rows;       // All bad categories
        if (!format_ok) {
          ++num_format_bad;   // Bad format
        }
        if (!remapping_ok) {
          ++num_remapping_bad;  // Remapping lookup failed
        }
      }
    }    
    ahfits::writeRow(fp_out);
    raw_asic_data.rewind();              // Reinitialize telemetry buffer for next row.
  }

  AH_INFO(ahlog::HIGH) << "Number of good table rows = " << num_good_rows 
     << "; number of bad table rows = " << num_bad_rows << std::endl;
  if (num_bad_rows > 0) {
    AH_INFO(ahlog::HIGH) << "Number of rows with bad format                      = " << num_format_bad << std::endl;
    AH_INFO(ahlog::HIGH) << "Number of rows with remapping problem               = " << num_remapping_bad << std::endl;
  }

}

// ----------------------------------------------------------------------------

void finalize(hxisgdsfflib::RemapTable & remap_table, ahfits::FilePtr & fp_out) {
  ahfits::close(fp_out);  // Close the SFF.
  remap_table.clear();    // Free the CALDB table storage.
}

/** @} */

/* Revision Log
 $Log: hxisgdsff.cxx,v $
 Revision 1.85  2016/04/18 14:59:48  rshill
 Added status bits section to prologue.

 Revision 1.84  2016/04/11 14:46:01  mdutka
 Adding parameter stamping to standard main and making instrume and detnam checks case insensitive

 Revision 1.83  2016/04/07 21:04:55  mdutka
 Moving high volumn log statements to debug stream

 Revision 1.82  2015/12/29 16:49:40  rshill
 Added checkEmptyTable().

 Revision 1.81  2015/09/04 16:04:13  rshill
 Added parameter stamping to log.

 Revision 1.80  2015/08/18 21:44:11  rshill
 Corrected mistake in log output; was printing input instead of output file.

 Revision 1.79  2015/08/18 00:04:49  mwitthoe
 hxisgdsff: fix error message when remapping CALDB file has no rows

 Revision 1.78  2015/08/13 00:51:09  rshill
 Added loggin of INSTRUME, TELESCOP, DETNAM.

 Revision 1.77  2015/08/13 00:48:18  rshill
 Corrected compilation error.

 Revision 1.76  2015/08/13 00:47:20  rshill
 Added logging output for input and output files.

 Revision 1.75  2015/08/06 19:51:47  rshill
 Cleanup up comment prologue; fixed one message.

 Revision 1.74  2015/08/06 15:01:01  rshill
 After code and TRF cleanup.

 Revision 1.73  2015/07/15 18:58:52  klrutkow
 added CALDB queries with ahmission query

 Revision 1.72  2015/07/13 17:31:33  klrutkow
 call resolve() to get remapfile CALDB file ; edited if-block to ensure INSTRUME has the 1 or 2, so it can't just be HXI or SGD, it must be HXI1, HXI2, SGD1, or SGD2 ; changed comparison between CALDB and infile INSTRUME to check against 'instrume' string instead of using substr, since 'instrume' is already just the first three letters

 Revision 1.71  2015/03/18 19:55:55  mwitthoe
 hxisgdsff: change DETNAME to DETNAM

 Revision 1.70  2015/02/12 19:47:19  rshill
 Checked ASIC_SEU handling vs. TRF; +++ comment asking for this check deleted.

 Revision 1.69  2015/02/05 18:13:04  rshill
 readout_flag array changed to unsigned long to match current sff template.

 Revision 1.68  2014/12/31 16:05:15  rshill
 Backed out of check of standard telescope name - should be coordinated.

 Revision 1.67  2014/12/24 19:37:04  klrutkow
 updated tool with new parameters, per issue 472

 Revision 1.66  2014/12/17 23:27:03  rshill
 Changed DETNAM back to DETNAME; should be coordinated.

 Revision 1.65  2014/12/17 16:54:46  rshill
 Commented out the check for DETNAM in remap file;
 hxievtid does not make this check and latest remap file does not have DETNAM.

 Revision 1.64  2014/12/15 20:55:12  rshill
 EPI column (variable-length vector) no longer filled, but rather,
 has no elements.

 Revision 1.63  2014/09/15 20:43:29  mwitthoe
 hxisgdsff: add support for extended syntax with the input file; issue 179

 Revision 1.62  2014/09/12 21:27:47  mwitthoe
 hxisgdsff: remove instrument argument in procstatus::processRow() -- there is no instrument dependency

 Revision 1.61  2014/07/25 19:20:21  mwitthoe
 hxisgdsff: remove column count check, since the number of FITS columns was being compared to the number of input columns, not the number of input+output columns; added an if-statement outside the asic_seu/flag_seu warning message since only flag_seu was being checked (not asic_seu), this is a deviation from the TRF and should be checked by Bob/Lorella (+++ comment added in source)

 Revision 1.60  2014/07/23 17:16:45  mwitthoe
 hxisgdsff: fix an array out-of-bounds bug causing the tool to hang on Ubuntu v14

 Revision 1.59  2014/06/05 22:48:44  rshill
 In response to TRF review.  Checking streamlined, SEU bit check
 added. Comments improved.

 Revision 1.58  2014/05/28 22:46:05  rshill
 Recoded variable declarations for greater clarity.

 Revision 1.57  2014/03/05 01:40:53  rshill
 Commented out AH_DEBUG statements to improve timing.

 Revision 1.56  2014/02/07 23:15:50  rshill
 Changed double connected to E FITS column to a float.

 Revision 1.55  2014/02/01 03:45:29  rshill
 Fixed initialization bug.

 Revision 1.54  2014/01/31 19:51:14  rshill
 Two-bit proc_status check.

 Revision 1.53  2014/01/31 00:08:30  rshill
 Changed EPI to float, PROC_STATUS to bit type.

 Revision 1.52  2014/01/09 20:14:48  klrutkow
 klrutkow: code review: comments

 Revision 1.51  2014/01/09 20:02:05  mwitthoe
 hxisgdsff: code review: variable declarations

 Revision 1.50  2014/01/09 19:40:53  asargent
 Added CR comments pertaining to main()

 Revision 1.49  2014/01/03 21:14:37  rshill
 Converted to standard main and to standard configuration of multiple
 source code files.

 Revision 1.48  2014/01/03 20:47:10  rshill
 New standard main.

 Revision 1.47  2013/12/02 22:52:03  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.46  2013/10/16 17:08:24  rshill
 Converted connects to new calls.

 Revision 1.45  2013/05/06 13:43:24  rshill
 More informative message for bad stop bit.

 Revision 1.44  2013/04/19 22:41:30  rshill
 More information printed for error detection cases.

 Revision 1.43  2013/04/10 21:31:05  rshill
 Got rid of hxisgdsff:: namespace.

 Revision 1.42  2013/04/04 22:56:50  rshill
 Moved remap code out of the old ahcaldb.  Got rid of static
 global storage area.  Streamlined the CALDB struct.

 Revision 1.41  2013/04/02 16:01:06  rshill
 Clarified code by eliminating an intermediate boolean.

 Revision 1.40  2013/04/01 18:40:02  rshill
 Move instrument (etc.) keyword processing into task.  More carefully
 match variable types with FITS column types (needed for TNULL processing).

 Revision 1.39  2013/03/20 22:50:33  rshill
 FITS column read/write flags.

 Revision 1.38  2013/02/13 21:22:54  rshill
 Tweaked doxygen in response to tester comments.

 Revision 1.37  2013/01/24 01:29:52  rshill
 Cleaned up doxygen prologue.

 Revision 1.36  2013/01/16 22:06:21  rshill
 Changed some long long to long.

 Revision 1.35  2013/01/15 21:36:45  rshill
 Added row format checks, placeholders for bounds checks, and a non-fatal
 check for failure to find the right CALDB remap record.  Placeholder
 for checks based on multiple columns added.  Exit message added.

 Revision 1.34  2013/01/15 00:22:40  rshill
 Added telemetry block length test; improved some variable names.

 Revision 1.33  2013/01/14 23:21:01  rshill
 More transparent variable names; placeholder range checks.

 Revision 1.32  2013/01/14 22:06:29  rshill
 Unified doWork routine for HXI and SGD.

 Revision 1.31  2013/01/14 04:42:47  rshill
 Initialized Fileptr to 0.

 Revision 1.30  2013/01/11 22:24:36  rshill
 Added a \todo.

 Revision 1.29  2013/01/09 18:47:16  rshill
 Cleaned up doxygen markup.

 Revision 1.28  2013/01/09 18:44:32  rshill
 Cleaned up doxygen markup.


 Revision 1.27  2013/01/04 16:05:53  rshill
 Use X-type bit flags; undo byte swap before output.

 Revision 1.26  2012/12/27 17:11:11  rshill
 changed extension name to EVENTS; commented out FLAG setting until ahfits X format done

 Revision 1.25  2012/12/18 21:19:07  rshill
 With byte swap of 32-bit words on both instruments.

 Revision 1.24  2012/12/18 15:38:53  rshill
 SGD format of 2012-12-18.

 Revision 1.23  2012/12/18 01:37:10  rshill
 Last commit before SGD format of 2012-12-04, using big-endian
 32-bit words.  Capability of implementing previous FFF/SFF format
 retained.

 Revision 1.22  2012/12/11 21:42:58  rshill
 Scrubbed for current ahfits & for coding standards.

 Revision 1.21  2012/12/04 15:15:34  rshill
 Added ull to bitmask constants.

 Revision 1.20  2012/11/30 20:31:42  rshill
 Fixed some bugs in ASIC and READOUT ID remapping, especially wrong
 declarations of variable lengths in SGD case.

 Revision 1.19  2012/11/30 00:17:41  rshill
 Further corrections to proc_status updating.

 Revision 1.18  2012/11/30 00:06:57  rshill
 Corrected misplaced setting of PROC_STATUS.

 Revision 1.17  2012/11/29 20:40:14  rshill
 With FFF+SFF format and remapping of ASIC_ID and READOUT_ID.

 Revision 1.16  2012/11/27 23:52:18  rshill
 Best current idea of HXI+SGD formats; CALDB lookup of ASIC_ID and
 READOUT_ID remapping.  SGD is now implemented.

 Revision 1.15  2012/11/16 00:43:43  rshill
 Got rid of unneeded ape headers.

 Revision 1.14  2012/11/15 23:07:11  rshill
 Reflecting identity of FFF and SFF FITS file formats, and changes
 made in team meetings.  Note, the FITS format can't be regarded
 as stable yet.

 Revision 1.13  2012/11/15 00:35:14  rshill
 Rationalized HXI and SGD telem formats after meetings of ~1 Nov
 and of 9 Nov 2012.

 Revision 1.12  2012/10/26 21:20:40  rshill
 Rewrite of task from scratch using BitBuf to treat problem narrowly as
 a set of bit transfers with little to no parsing.

*/
