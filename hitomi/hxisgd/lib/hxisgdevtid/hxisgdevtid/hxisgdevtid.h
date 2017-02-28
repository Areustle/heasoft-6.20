/// \file hxisgdevtid.h
/// \brief structures and functions common to HXI/SGD reconstruction tasks.
/// \author Mike Witthoeft
/// \date $Date: 2016/06/17 19:11:23 $

/// \addtogroup mod_hxisgdevtid
/// \section hxisgdevtid Structures and functions common to HXI/SGD reconstruction tasks - hxisgdevtid
///
/// Description...
///

#ifndef HXISGDEVTID_H
#define HXISGDEVTID_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDEVTID,"$Id: hxisgdevtid.h,v 1.5 2016/06/17 19:11:23 rshill Exp $")

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <cmath>
#include <string>
#include <iostream>

/// \ingroup mod_hxisgdevtid

namespace hxisgdevtid {

/** \addtogroup mod_hxisgdevtid
 *  @{
 */

// maximum size of RAW_ASIC_DATA column in bytes
const int SIZE_RAW_ASIC_DATA=19552;

// array sizes for SGD
const int SGD_SIZE_STATUS=8;              ///< num bits in STATUS column
const int SGD_SIZE_PROC_STATUS=32;        ///< num bits in PROC_STATUS column
const int SGD_SIZE_FLAGS=64;              ///< num bits in FLAGS column
const int SGD_SIZE_FLAG_SEU=1;            ///< num bits in FLAG_SEU column
const int SGD_SIZE_FLAG_LCHK=1;           ///< num bits in FLAG_LCHK column
const int SGD_SIZE_FLAG_TRIG=1;           ///< num bytes in FLAG column
const int SGD_SIZE_FLAG_TRIGPAT=31;       ///< num bits in FLAG_TRIGPAT column
const int SGD_SIZE_FLAG_HITPAT=4;         ///< num bits in FLAG_HITPAT column
const int SGD_SIZE_FLAG_FASTBGO=4;        ///< num bits in FLAG_FASTBGO column
const int SGD_SIZE_FLAG_LCHKMIO=1;        ///< num bits in FLAG_LCHKMIO column
const int SGD_SIZE_FLAG_CCBUSY=3;         ///< num bits in FLAG_CCBUSY column
const int SGD_SIZE_FLAG_HITPAT_CC=3;      ///< num bits in FLAG_HITPAT_CC column
const int SGD_SIZE_FLAG_CALMODE=1;        ///< num bits in FLAG_CALMODE column
const int SGD_SIZE_CHANNEL=13312;         ///< num elements in READOUT_ID_RMAP and EPI columns
const int SGD_MAX_ASICS=208;              ///< maximum number of SGD ASICS

// array sizes for HXI
const int HXI_SIZE_STATUS=8;              ///< num bits in STATUS column
const int HXI_SIZE_PROC_STATUS=32;        ///< num bits in PROC_STATUS column
const int HXI_SIZE_FLAGS=32;              ///< num bits in FLAGS column
const int HXI_SIZE_FLAG_SEU=5;            ///< num bits in FLAG_SEU column
const int HXI_SIZE_FLAG_LCHK=5;           ///< num bits in FLAG_LCHK column
const int HXI_SIZE_FLAG_TRIG=8;           ///< num bits in FLAG_TRIG column
const int HXI_SIZE_FLAG_TRIGPAT=8;        ///< num bits in FLAG_TRIGPAT column
const int HXI_SIZE_FLAG_HITPAT=2;         ///< num bits in FLAG_HITPAT column
const int HXI_SIZE_FLAG_FASTBGO=2;        ///< num bits in FLAG_FASTBGO column
const int HXI_SIZE_FLAG_LCHKMIO=1;        ///< not used for HXI
const int HXI_SIZE_FLAG_CCBUSY=1;         ///< not used for HXI
const int HXI_SIZE_FLAG_HITPAT_CC=1;      ///< not used for HXI
const int HXI_SIZE_FLAG_CALMODE=1;        ///< not used for HXI
const int HXI_SIZE_CHANNEL=1280;          ///< num elements in READOUT_ID_RMAP and EPI columns
const int HXI_MAX_ASICS=40;               ///< maximum number of HXI ASICS

// SGD layer types.
const int SGD_SI_STACK=0;                               ///< internal index for Si layers
const int SGD_CDTE_BOTTOM=1;                            ///< internal index for CdTe bottom layers
const int SGD_CDTE_SIDE=2;                              ///< internal index for CdTe side layers
const int SGD_SI_STACK_MIN=100*SGD_SI_STACK;            ///< smallest layer index of Si stack
const int SGD_SI_STACK_MAX=SGD_SI_STACK_MIN+99;         ///< largest layer index of Si stack
const int SGD_CDTE_BOTTOM_MIN=100*SGD_CDTE_BOTTOM;      ///< smallest layer index of CdTe bottom stack
const int SGD_CDTE_BOTTOM_MAX=SGD_CDTE_BOTTOM_MIN+99;   ///< largest layer index of CdTe bottom stack
const int SGD_CDTE_SIDE_MIN=100*SGD_CDTE_SIDE;          ///< smallest layer index of CdTe side stacks
const int SGD_CDTE_SIDE_MAX=SGD_CDTE_SIDE_MIN+99;       ///< largest layer index of CdTe side stacks

// RECO_STATUS size for each instrument
const int HXI_SIZE_RECO_STATUS=16;        ///< number of RECO_STATUS bits for HXI
const int SGD_SIZE_RECO_STATUS=40;        ///< number of RECO_STATUS bits for SGD


/// \brief Enumerate occurrence DATAMODEs.
enum Datamodes {
  e_CAMERA,      // also CC for SGD
  e_AM241,       // HXI only
  e_CALMODE,
  e_PSEUDO,
  e_READALL,
  e_NOTSURE
};


/// \brief Structure to hold a single row of data from the input SFF file
///  for either HXI or SGD.
///
/// Columns which are bit type or a variable-length array need two members in
/// the struct: one for data and one for the number of elements.
struct SFFRowData {

  SFFRowData(const std::string& inst):
             m_time(0.), m_time_null(0), m_s_time(0.), m_adu_cnt(0), m_l32ti(0),
             m_occurrence_id(0), m_local_time(0), m_category(0),
             m_livetime(0), m_num_asic(0), m_row_count(0) {

    // choose size based on instrument
    if (inst == "SGD") {
      num_raw_asic_data=SIZE_RAW_ASIC_DATA;
      num_asic_id=SGD_MAX_ASICS;
      num_asic_id_rmap=SGD_MAX_ASICS;
      num_asic_chip=SGD_MAX_ASICS;
      num_asic_trig=SGD_MAX_ASICS;
      num_asic_seu=SGD_MAX_ASICS;
      num_readout_flag=SGD_MAX_ASICS;
      num_num_readout=SGD_MAX_ASICS;
      num_asic_ref=SGD_MAX_ASICS;
      num_asic_cmn=SGD_MAX_ASICS;
      num_readout_asic_id=SGD_SIZE_CHANNEL;
      num_readout_id=SGD_SIZE_CHANNEL;
      num_proc_status=SGD_SIZE_PROC_STATUS;
      num_status=SGD_SIZE_STATUS;
      num_flags=SGD_SIZE_FLAGS;
      num_flag_seu=SGD_SIZE_FLAG_SEU;
      num_flag_lchk=SGD_SIZE_FLAG_LCHK;
      num_flag_trig=SGD_SIZE_FLAG_TRIG;
      num_flag_trigpat=SGD_SIZE_FLAG_TRIGPAT;
      num_flag_hitpat=SGD_SIZE_FLAG_HITPAT;
      num_flag_fastbgo=SGD_SIZE_FLAG_FASTBGO;
      num_flag_lchkmio=SGD_SIZE_FLAG_LCHKMIO;
      num_flag_ccbusy=SGD_SIZE_FLAG_CCBUSY;
      num_flag_hitpat_cc=SGD_SIZE_FLAG_HITPAT_CC;
      num_flag_calmode=SGD_SIZE_FLAG_CALMODE;
      num_readout_id_rmap=SGD_SIZE_CHANNEL;
      num_pha=SGD_SIZE_CHANNEL;
      num_epi=SGD_SIZE_CHANNEL;
    } else if (inst == "HXI") {
      num_raw_asic_data=SIZE_RAW_ASIC_DATA;
      num_asic_id=HXI_MAX_ASICS;
      num_asic_id_rmap=HXI_MAX_ASICS;
      num_asic_chip=HXI_MAX_ASICS;
      num_asic_trig=HXI_MAX_ASICS;
      num_asic_seu=HXI_MAX_ASICS;
      num_readout_flag=HXI_MAX_ASICS;
      num_num_readout=HXI_MAX_ASICS;
      num_asic_ref=HXI_MAX_ASICS;
      num_asic_cmn=HXI_MAX_ASICS;
      num_readout_asic_id=HXI_SIZE_CHANNEL;
      num_readout_id=HXI_SIZE_CHANNEL;
      num_proc_status=HXI_SIZE_PROC_STATUS;
      num_status=HXI_SIZE_STATUS;
      num_flags=HXI_SIZE_FLAGS;
      num_flag_seu=HXI_SIZE_FLAG_SEU;
      num_flag_lchk=HXI_SIZE_FLAG_LCHK;
      num_flag_trig=HXI_SIZE_FLAG_TRIG;
      num_flag_trigpat=HXI_SIZE_FLAG_TRIGPAT;
      num_flag_hitpat=HXI_SIZE_FLAG_HITPAT;
      num_flag_fastbgo=HXI_SIZE_FLAG_FASTBGO;
      num_flag_lchkmio=HXI_SIZE_FLAG_LCHKMIO;
      num_flag_ccbusy=HXI_SIZE_FLAG_CCBUSY;
      num_flag_hitpat_cc=HXI_SIZE_FLAG_HITPAT_CC;
      num_flag_calmode=HXI_SIZE_FLAG_CALMODE;
      num_readout_id_rmap=HXI_SIZE_CHANNEL;
      num_pha=HXI_SIZE_CHANNEL;
      num_epi=HXI_SIZE_CHANNEL;
    } else {
      AH_THROW_LOGIC("invalid inst value: "+inst+"; should be HXI or SGD");
    }

    // initialize arrays
    for (ahfits::IndexType ii=0; ii < SIZE_RAW_ASIC_DATA; ii++) m_raw_asic_data[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_id[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_id_rmap[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_chip[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_trig[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_seu[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_readout_flag[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_num_readout[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_ref[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_MAX_ASICS; ii++) m_asic_cmn[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_readout_asic_id[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_readout_id[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_PROC_STATUS; ii++) m_proc_status[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_STATUS; ii++) m_status[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAGS; ii++) m_flags[ii]=0;
    for (ahfits::IndexType ii=0; ii < HXI_SIZE_FLAG_SEU; ii++) m_flag_seu[ii]=0;
    for (ahfits::IndexType ii=0; ii < HXI_SIZE_FLAG_LCHK; ii++) m_flag_lchk[ii]=0;
    for (ahfits::IndexType ii=0; ii < HXI_SIZE_FLAG_TRIG; ii++) m_flag_trig[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_TRIGPAT; ii++) m_flag_trigpat[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_HITPAT; ii++) m_flag_hitpat[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_FASTBGO; ii++) m_flag_fastbgo[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_LCHKMIO; ii++) m_flag_lchkmio[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_CCBUSY; ii++) m_flag_ccbusy[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_HITPAT_CC; ii++) m_flag_hitpat_cc[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_FLAG_CALMODE; ii++) m_flag_calmode[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_readout_id_rmap[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_pha[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_epi[ii]=0;
    for (ahfits::IndexType ii=0; ii < SGD_SIZE_CHANNEL; ii++) m_epi_null[ii]=0;
  }

  double m_time;
  char m_time_null;
  double m_s_time;
  long m_adu_cnt;
  long m_l32ti;
  long m_occurrence_id;
  long m_local_time;
  char m_category;
  long long m_livetime;
  char m_num_asic;

  char m_raw_asic_data[SIZE_RAW_ASIC_DATA];
  ahfits::IndexType num_raw_asic_data;

  short m_asic_id[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_id;

  char m_asic_id_rmap[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_id_rmap;

  char m_asic_chip[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_chip;

  char m_asic_trig[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_trig;

  char m_asic_seu[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_seu;

  long long m_readout_flag[SGD_MAX_ASICS];
  ahfits::IndexType num_readout_flag;

  long m_num_readout[SGD_MAX_ASICS];
  ahfits::IndexType num_num_readout;

  long m_asic_ref[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_ref;

  long m_asic_cmn[SGD_MAX_ASICS];
  ahfits::IndexType num_asic_cmn;

  short m_readout_asic_id[SGD_SIZE_CHANNEL];
  ahfits::IndexType num_readout_asic_id;

  char m_readout_id[SGD_SIZE_CHANNEL];
  ahfits::IndexType num_readout_id;

  char m_proc_status[SGD_SIZE_PROC_STATUS];
  ahfits::IndexType num_proc_status;

  char m_status[SGD_SIZE_STATUS];
  ahfits::IndexType num_status;

  char m_flag_seu[HXI_SIZE_FLAG_SEU];        // HXI constant larger than SGD
  ahfits::IndexType num_flag_seu;

  char m_flag_lchk[HXI_SIZE_FLAG_LCHK];      // HXI constant larger than SGD
  ahfits::IndexType num_flag_lchk;

  char m_flag_trig[HXI_SIZE_FLAG_TRIG];      // HXI constant larger than SGD
  ahfits::IndexType num_flag_trig;

  char m_flag_trigpat[SGD_SIZE_FLAG_TRIGPAT];
  ahfits::IndexType num_flag_trigpat;

  char m_flag_hitpat[SGD_SIZE_FLAG_HITPAT];
  ahfits::IndexType num_flag_hitpat;

  char m_flag_fastbgo[SGD_SIZE_FLAG_FASTBGO];
  ahfits::IndexType num_flag_fastbgo;

  char m_flag_lchkmio[SGD_SIZE_FLAG_LCHKMIO];
  ahfits::IndexType num_flag_lchkmio;

  char m_flag_ccbusy[SGD_SIZE_FLAG_CCBUSY];
  ahfits::IndexType num_flag_ccbusy;

  char m_flag_hitpat_cc[SGD_SIZE_FLAG_HITPAT_CC];
  ahfits::IndexType num_flag_hitpat_cc;

  char m_flag_calmode[SGD_SIZE_FLAG_CALMODE];
  ahfits::IndexType num_flag_calmode;

  int m_readout_id_rmap[SGD_SIZE_CHANNEL];
  ahfits::IndexType num_readout_id_rmap;

  int m_pha[SGD_SIZE_CHANNEL];
  ahfits::IndexType num_pha;

  double m_epi[SGD_SIZE_CHANNEL];
  char m_epi_null[SGD_SIZE_CHANNEL];
  ahfits::IndexType num_epi;

  char m_flags[SGD_SIZE_FLAGS];
  ahfits::IndexType num_flags;

  ahfits::IndexType m_row_count;
};


/// \brief Determine operation mode of occurrence.
/// \param[in] inst name of instrument: HXI or SGD
/// \param[in] inrow input row data with FLAGs
/// \param[in] alive_asic number of alive ASICs as read from badpix CALDB file
/// \param[out] isAm241 true if Am241 flag was set (HXI only)
/// \return mode occurrence mode enumerated value (see Datamodes enum)
int getOccurrenceMode(const std::string& inst, hxisgdevtid::SFFRowData& inrow,
                      int alive_asic, bool* isAm241=0);

/// \brief Convert EPI in keV to PI, which is a channel number.
/// \param[in] inst name of instrument: HXI or SGD
/// \param[in] epi energy in keV
/// \param[out] pi output PI value
/// \param[out] pi_null PI NULL flag (1 => NULL)
void convertEPIToPI(const std::string& inst, double epi, int& pi, char& pi_null);

/// \brief Return type of layer in SGD detector.
/// \param[in] layer_num         Official designator of layer
/// \return code for Si, CdTe bottom, or CdTe side
int SGDLayerType(int layer_num);

/// \brief Test whether a layer in the SGD detector is CdTe
/// \param[in] layer_num         Official designator of layer
/// \return True if the layer is CdTe
bool SGDLayerIsCdTe(int layer_num);

/// \brief Test whether a layer in the SGD detector is Si
/// \param[in] layer_num         Official designator of layer
/// \return True if the layer is Si
bool SGDLayerIsSi(int layer_num);

/// Construct FITS column format string for array of the given length and type.
/// \param[in] length length of array
/// \param[in] type FITS column type, e.g. I
/// \return FITS column format, e.g. 10I
std::string getColumnFormat(int length, const std::string& type);


/** @} */

}  // namespace hxisgdevtid

#endif /* HXISGDEVTID_H */

/* Revision Log

 $Log: hxisgdevtid.h,v $
 Revision 1.5  2016/06/17 19:11:23  rshill
 Changed SFFRowData.m_asic_id and m_readout_asic_id from char to short to accommodate SGD.

 Revision 1.4  2016/01/12 20:35:50  mwitthoe
 hxisgdevtid library: change type of m_livetime from unsigned long to long long

 Revision 1.3  2016/01/06 16:13:31  mwitthoe
 hxisgdevtid library: add missing columns from SFF template to SFFRowData structure for use in hxisgdmerge

 Revision 1.2  2015/07/31 19:38:48  rshill
 Changed m_livetime to unsigned.

 Revision 1.1  2015/03/03 18:24:45  mwitthoe
 hxisgdevtid library: add common structures/functions for reconstruction and expand tools; add new keywords to fluorescence CALDB file for SGD energy test


*/
