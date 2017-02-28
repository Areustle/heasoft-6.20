/// \file hxisgdmerge.cxx
/// \brief Tool to merge two HXI or SGD SFF files
/// \author Michael Witthoeft
/// \date $Date: 2016/04/08 18:48:09 $

/**

\defgroup tool_hxisgdmerge Merge two HXI or SGD SFF files (hxisgdmerge)
@ingroup mod_hxisgd_tasks

Merge slew and pointing event files for HXI or SGD.

Source files:

  hxisgdmerge.cxx

Library dependencies:

  astroh/hxisgd/lib/hxisgdevtid/
  astroh/gen/lib/ahapp
  heacore/ahgen
  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils

Modification history:

  Ver   Date        Author  Description
  1.0   2016-12-05  MCW     initial version

*/

#define AHLABEL tool_hxisgdmerge
#define AHCVSID "$Id: hxisgdmerge.cxx,v 1.4 2016/04/08 18:48:09 mdutka Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "hxisgdevtid/hxisgdevtid.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"
#include "ahapp/ahapp.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <cstdio>             // std::remove()

/** \addtogroup tool_hxisgdmerge
 *   @{
 *   */

/// \brief structure to hold input parameters
struct Par {

  std::string m_infile1;            ///< First input FITS file name
  std::string m_infile2;            ///< Second input FITS file name
  std::string m_outfile;            ///< Output FITS file name
};

/// \brief Get parameter values.
/// \param[out] par structure with parameter values
void getPar(Par& par);
  
/// \brief Clone infile1 to outfile and open infile2.
/// \param[in] par structure with parameter values
/// \param[out] ahffp1 ahfits file pointer to outfile
/// \param[out] ahffp2 ahfits file pointer to infile2
/// \param[out] inst instrument of input files: HXI or SGD
/// \param[out] clean if true, then error occurred and output should be deleted
void initialize(const Par& par, ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2,
                std::string& inst, bool& clean);

/// \brief Copy rows from infile2 to end of outfile table.
/// \param[in] ahffp1 ahfits file pointer to outfile
/// \param[in] ahffp2 ahfits file pointer to infile2
/// \param[in] inst instrument of input files: HXI or SGD
/// \param[in] clean if true, then error occurred and output should be deleted
void doWork(ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2,
            const std::string& inst, bool clean);

/// \brief Close files.
/// \param[in] ahffp1 ahfits file pointer to outfile
/// \param[in] ahffp2 ahfits file pointer to infile2
/// \param[in] clean if true, then error occurred and output should be deleted
void finalize(ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2, bool clean);


// ****************************************************************************

int main(int argc, char** argv) {

  Par par;                     // parameter values
  ahfits::FilePtr ahffp1=0;    // ahfits file pointer to outfile
  ahfits::FilePtr ahffp2=0;    // ahfits file pointer to infile2
  std::string inst;            // type of instrument: HXI or SGD
  bool clean;                  // true: delete output file (upon error)

  int status = ahapp::startUp(argc,argv,TOOLTAG);
  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog();
      initialize(par,ahffp1,ahffp2,inst,clean);
      doWork(ahffp1,ahffp2,inst,clean);
      finalize(ahffp1,ahffp2,clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,ahffp1,ahffp2,inst,clean);
        doWork(ahffp1,ahffp2,inst,clean);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(ahffp1,ahffp2,clean);
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

void getPar(Par& par) {

  par.m_infile1 = ahapp::getParString("infile1");
  par.m_infile2 = ahapp::getParString("infile2");
  par.m_outfile = ahapp::getParString("outfile");
}

// ----------------------------------------------------------------------------

void initialize(const Par& par, ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2,
                std::string& inst, bool& clean) {

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

  // Clone infile1 to output file.
  ahfits::clone(par.m_infile1, par.m_outfile, &ahffp1, true);
  if (ahfits::isPrimary(ahffp1)) ahfits::move(ahffp1, "EVENTS");    // move to EVENTS extension if extended syntax not used

  // If infile1 contains no rows, then just clone infile2 as outfile
  if (0 == ahfits::numRows(ahffp1)) {
    std::remove(par.m_outfile.c_str());    // delete previously cloned infile1
    ahfits::clone(par.m_infile2,par.m_outfile,&ahffp1);
    return;
  }

  // By default, ahfits::clone assumes that you will not be adding rows
  // to the FITS file so, when buffering, it will not add extra rows to
  // the end of the file causing a slow-down.  However, in this tool we
  // want to add rows to the end of the cloned file, so we enable this
  // feature.
  ahfits::enableBufferPadding(ahffp1);

  // Open infile2.
  ahfits::open(par.m_infile2,"",&ahffp2);
  if (ahfits::isPrimary(ahffp2)) ahfits::move(ahffp2, "EVENTS");    // move to EVENTS extension if extended syntax not used

  // Check that INSTRUME from both input files match.
  std::string instrume1=ahfits::getKeyValStr(ahffp1,"INSTRUME");
  std::string instrume2=ahfits::getKeyValStr(ahffp2,"INSTRUME");
  if (ahgen::strtoupper(instrume1) != ahgen::strtoupper(instrume2)) {
    clean=true;
    AH_THROW_RUNTIME("INSTRUME keyword values do not match.");
  }
  inst=ahgen::strtoupper(instrume1.substr(0,3));

  // Check that DETNAM from both input files match.
  std::string detnam1=ahfits::getKeyValStr(ahffp1,"DETNAM");
  std::string detnam2=ahfits::getKeyValStr(ahffp2,"DETNAM");
  if (ahgen::strtoupper(detnam1) != ahgen::strtoupper(detnam2)) {
    clean=true;
    AH_THROW_RUNTIME("DETNAM keyword values do not match.");
  }

  // Check that TIME range of files do not over lap (warning message only)
  double tstop1=ahfits::getKeyValDbl(ahffp1,"TSTOP");
  double tstart2=ahfits::getKeyValDbl(ahffp2,"TSTART");
  if (tstart2 < tstop1) {
    AH_INFO(ahlog::HIGH) << "Input files overlap or are out of TIME order; output file is not sorted in TIME" << std::endl << std::endl;
  }

}

// ----------------------------------------------------------------------------

void doWork(ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2,
            const std::string& inst, bool clean) {

  // If infile1 is empty, then infile2 was cloned to outfile in initialize
  // and ahffp2 was not set up.  Nothing left for the tool to do.
  if (0 == ahffp2) return;

  // Set up row data structure based on instrument.
  hxisgdevtid::SFFRowData inrow(inst);

  // Set up connections between local variables and infile2 SFF columns.
  ahfits::Router routin(ahffp2);
  routin.connectScalar(ahfits::e_READONLY,"TIME",inrow.m_time,&inrow.m_time_null);
  routin.connectScalar(ahfits::e_READONLY,"S_TIME",inrow.m_s_time);
  routin.connectScalar(ahfits::e_READONLY,"ADU_CNT",inrow.m_adu_cnt);
  routin.connectScalar(ahfits::e_READONLY,"L32TI",inrow.m_l32ti);
  routin.connectScalar(ahfits::e_READONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routin.connectScalar(ahfits::e_READONLY,"LOCAL_TIME",inrow.m_local_time);
  routin.connectScalar(ahfits::e_READONLY,"CATEGORY",inrow.m_category);
  routin.connectBit(ahfits::e_READONLY,"FLAGS",inrow.m_flags,inrow.num_flags);
  routin.connectBit(ahfits::e_READONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routin.connectBit(ahfits::e_READONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routin.connectBit(ahfits::e_READONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routin.connectBit(ahfits::e_READONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routin.connectScalar(ahfits::e_READONLY,"LIVETIME",inrow.m_livetime);
  routin.connectScalar(ahfits::e_READONLY,"NUM_ASIC",inrow.m_num_asic);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"RAW_ASIC_DATA",inrow.m_raw_asic_data,inrow.num_raw_asic_data);
  routin.connectBit(ahfits::e_READONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routin.connectBit(ahfits::e_READONLY,"STATUS",inrow.m_status,inrow.num_status);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_ID",inrow.m_asic_id,inrow.num_asic_id);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_ID_RMAP",inrow.m_asic_id_rmap,inrow.num_asic_id_rmap);
  routin.connectBit(ahfits::e_READONLY,"ASIC_CHIP",inrow.m_asic_chip,inrow.num_asic_chip);
  routin.connectBit(ahfits::e_READONLY,"ASIC_TRIG",inrow.m_asic_trig,inrow.num_asic_trig);
  routin.connectBit(ahfits::e_READONLY,"ASIC_SEU",inrow.m_asic_seu,inrow.num_asic_seu);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_FLAG",inrow.m_readout_flag,inrow.num_readout_flag);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"NUM_READOUT",inrow.m_num_readout,inrow.num_num_readout);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_REF",inrow.m_asic_ref,inrow.num_asic_ref);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"ASIC_CMN",inrow.m_asic_cmn,inrow.num_asic_cmn);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ASIC_ID",inrow.m_readout_asic_id,inrow.num_readout_asic_id);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID",inrow.m_readout_id,inrow.num_readout_id);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",inrow.m_readout_id_rmap,inrow.num_readout_id_rmap);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"PHA",inrow.m_pha,inrow.num_pha);
  routin.connectVariableLengthArray(ahfits::e_READONLY,"EPI",inrow.m_epi,inrow.num_epi,inrow.m_epi_null);
  if (ahgen::strtoupper(inst) == "SGD") {
    routin.connectBit(ahfits::e_READONLY,"FLAG_LCHKMIO",inrow.m_flag_lchkmio,inrow.num_flag_lchkmio);
    routin.connectBit(ahfits::e_READONLY,"FLAG_CCBUSY",inrow.m_flag_ccbusy,inrow.num_flag_ccbusy);
    routin.connectBit(ahfits::e_READONLY,"FLAG_HITPAT_CC",inrow.m_flag_hitpat_cc,inrow.num_flag_hitpat_cc);
    routin.connectBit(ahfits::e_READONLY,"FLAG_CALMODE",inrow.m_flag_calmode,inrow.num_flag_calmode);
    routin.connectScalar(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig[0]);
  } else {     // HXI
    routin.connectBit(ahfits::e_READONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
  }

  // Set up connections between local variables and output SFF columns.
  ahfits::Router routout(ahffp1);
  routout.connectScalar(ahfits::e_WRITEONLY,"TIME",inrow.m_time,&inrow.m_time_null);
  routout.connectScalar(ahfits::e_WRITEONLY,"S_TIME",inrow.m_s_time);
  routout.connectScalar(ahfits::e_WRITEONLY,"ADU_CNT",inrow.m_adu_cnt);
  routout.connectScalar(ahfits::e_WRITEONLY,"L32TI",inrow.m_l32ti);
  routout.connectScalar(ahfits::e_WRITEONLY,"OCCURRENCE_ID",inrow.m_occurrence_id);
  routout.connectScalar(ahfits::e_WRITEONLY,"LOCAL_TIME",inrow.m_local_time);
  routout.connectScalar(ahfits::e_WRITEONLY,"CATEGORY",inrow.m_category);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAGS",inrow.m_flags,inrow.num_flags);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_SEU",inrow.m_flag_seu,inrow.num_flag_seu);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHK",inrow.m_flag_lchk,inrow.num_flag_lchk);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIGPAT",inrow.m_flag_trigpat,inrow.num_flag_trigpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT",inrow.m_flag_hitpat,inrow.num_flag_hitpat);
  routout.connectBit(ahfits::e_WRITEONLY,"FLAG_FASTBGO",inrow.m_flag_fastbgo,inrow.num_flag_fastbgo);
  routout.connectScalar(ahfits::e_WRITEONLY,"LIVETIME",inrow.m_livetime);
  routout.connectScalar(ahfits::e_WRITEONLY,"NUM_ASIC",inrow.m_num_asic);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"RAW_ASIC_DATA",inrow.m_raw_asic_data,inrow.num_raw_asic_data);
  routout.connectBit(ahfits::e_WRITEONLY,"PROC_STATUS",inrow.m_proc_status,inrow.num_proc_status);
  routout.connectBit(ahfits::e_WRITEONLY,"STATUS",inrow.m_status,inrow.num_status);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_ID",inrow.m_asic_id,inrow.num_asic_id);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_ID_RMAP",inrow.m_asic_id_rmap,inrow.num_asic_id_rmap);
  routout.connectBit(ahfits::e_WRITEONLY,"ASIC_CHIP",inrow.m_asic_chip,inrow.num_asic_chip);
  routout.connectBit(ahfits::e_WRITEONLY,"ASIC_TRIG",inrow.m_asic_trig,inrow.num_asic_trig);
  routout.connectBit(ahfits::e_WRITEONLY,"ASIC_SEU",inrow.m_asic_seu,inrow.num_asic_seu);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_FLAG",inrow.m_readout_flag,inrow.num_readout_flag);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"NUM_READOUT",inrow.m_num_readout,inrow.num_num_readout);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_REF",inrow.m_asic_ref,inrow.num_asic_ref);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_CMN",inrow.m_asic_cmn,inrow.num_asic_cmn);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ASIC_ID",inrow.m_readout_asic_id,inrow.num_readout_asic_id);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ID",inrow.m_readout_id,inrow.num_readout_id);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"READOUT_ID_RMAP",inrow.m_readout_id_rmap,inrow.num_readout_id_rmap);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"PHA",inrow.m_pha,inrow.num_pha);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"EPI",inrow.m_epi,inrow.num_epi,inrow.m_epi_null);
  if (ahgen::strtoupper(inst) == "SGD") {
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_LCHKMIO",inrow.m_flag_lchkmio,inrow.num_flag_lchkmio);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CCBUSY",inrow.m_flag_ccbusy,inrow.num_flag_ccbusy);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_HITPAT_CC",inrow.m_flag_hitpat_cc,inrow.num_flag_hitpat_cc);
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_CALMODE",inrow.m_flag_calmode,inrow.num_flag_calmode);
    routout.connectScalar(ahfits::e_WRITEONLY,"FLAG_TRIG",inrow.m_flag_trig[0]);
  } else {     // HXI
    routout.connectBit(ahfits::e_WRITEONLY,"FLAG_TRIG",inrow.m_flag_trig,inrow.num_flag_trig);
  }

  // Move to end of output file to prepare to adding rows.
  ahfits::lastRow(ahffp1);

  // Append infile2 rows to outfile
  for (ahfits::firstRow(ahffp2); ahfits::readOK(ahffp2); ahfits::nextRow(ahffp2)) {
    ahfits::readRow(ahffp2);

    ahfits::nextRow(ahffp1);
    ahfits::writeRow(ahffp1);
  }

}

// ----------------------------------------------------------------------------

void finalize(ahfits::FilePtr& ahffp1, ahfits::FilePtr& ahffp2, bool clean) {

  std::string filename;
  if (0 != ahffp1) filename=ahffp1->m_filename;
  ahfits::close(ahffp1);
  if (clean && "" != filename && ahgen::fileExists(filename)) std::remove(filename.c_str());

  ahfits::close(ahffp2);
}

// ----------------------------------------------------------------------------

/** @} */


/* Revision Log
 $Log: hxisgdmerge.cxx,v $
 Revision 1.4  2016/04/08 18:48:09  mdutka
 adding case insesitivity and parameter stamping to standard main

 Revision 1.3  2016/01/27 15:45:58  mwitthoe
 hxisgdmerge: delete output file upon input file incompatibility error

 Revision 1.2  2016/01/06 19:20:58  mwitthoe
 hxisgdmerge: (per Andy's suggestion) if infile1 is empty, just clone infile2 into outfile instead of copying row-by-row

 Revision 1.1  2016/01/06 16:18:36  mwitthoe
 add new tool, hxisgdmerge, which merges two HXI/SGD SFF files; see issue 574


*/
