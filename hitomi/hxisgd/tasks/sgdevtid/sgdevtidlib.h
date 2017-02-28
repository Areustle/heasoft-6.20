/// \brief Functions to form and test hit sequences in SGD event reconstruction.
/// \author Robert S. Hill
/// \date $Date: 2015/10/30 17:55:15 $

/// \addtogroup tool_sgdevtid
/// \section sgdevtid_sgdevtidlib SGD Event Reconstruction: Hit Sequence Processing - sgdevtidlib
///
/// Functions are provided to merge signals into hits, and to test hit sequences
/// for physical likelihood.  These functions are called by the primary doWork routine
/// in sgdevtid.

/// \ingroup tool_sgdevtid

#ifndef SGDEVTID_SGDEVTIDLIB_H
#define SGDEVTID_SGDEVTIDLIB_H

#include "ahgen/ahversion.h"
AHVERSION(SGDEVTID_SGDEVTIDLIB,"$Id: sgdevtidlib.h,v 1.46 2015/10/30 17:55:15 rshill Exp $")

#include <string>
#include <vector>
#include <map>
#include <cmath>

#include "ahfits/ahfits.h"
#include "hxisgdevtid/hxisgdevtid.h"
#include "hxisgdevtid/fluor.h"
#include "hxisgdevtid/remap.h"
#include "sgdevtidcaldb.h"


/// \ingroup tool_sgdevtid

namespace sgdevtidlib {

/** \addtogroup tool_sgdevtid
 *  @{
 */

// precision definition
// +++ RSH 2014-01-14 Value arbitrary at this point
const double FLOAT_EPSILON=1e-12;     ///< precision of a trig function

// bit and max array sizes columns
const int SIZE_STATUS=8;              ///< num bits in STATUS column
const int SIZE_PROC_STATUS=32;        ///< num bits in PROC_STATUS column
const int SIZE_FLAG_SEU=1;            ///< num bits in FLAG_SEU column
const int SIZE_FLAG_LCHK=1;           ///< num bits in FLAG_LCHK column
const int SIZE_FLAG_TRIGPAT=31;       ///< num bits in FLAG_TRIGPAT column
const int SIZE_FLAG_HITPAT=4;         ///< num bits in FLAG_HITPAT column
const int SIZE_FLAG_FASTBGO=4;        ///< num bits in FLAG_FASTBGO column
const int SIZE_FLAG_LCHKMIO=1;        ///< num bits in FLAG_LCHKMIO column
const int SIZE_FLAG_CCBUSY=3;         ///< num bits in FLAG_CCBUSY column
const int SIZE_FLAG_HITPAT_CC=3;      ///< num bits in FLAG_HITPAT_CC column
const int SIZE_FLAG_CALMODE=1;        ///< num bits in FLAG_CALMODE column
const int SIZE_CHANNEL=13312;         ///< num elements in READOUT_ID_RMAP and EPI columns
const int SIZE_NUMHITS=5;             ///< num bits in NUMHITS column  (maximum number of hits + 1)
const int NUM_LAYER=48;               ///< number of SGD layers
const int NUM_SI_STACK=32;            ///< number of SGD layers in the Si stack
const int NUM_CDTE_STACK=18;          ///< number of SGD layers in the CdTe stack
const int NUM_CDTE_SIDES=8;           ///< number of SGD layers in the CdTe sides
const int MAX_NUMSIGNAL=100;          ///< number of signals to allocate for safety
const int MAX_NUMHITS=4;              ///< maximum number of hits allowed

  
//  Permutation table for F and G tests.
//
const int PERM_2[] = {0, 1, 1, 0};
const int PERM_3[] = 
{ 0, 1, 2,
  0, 2, 1,
  1, 0, 2,
  1, 2, 0,
  2, 0, 1,
  2, 1, 0 
  };
const int PERM_4[] = 
{ 0, 1, 2, 3,
  0, 1, 3, 2,
  0, 2, 1, 3,
  0, 2, 3, 1,
  0, 3, 1, 2,
  0, 3, 2, 1,
  1, 0, 2, 3,
  1, 0, 3, 2,
  1, 2, 0, 3,
  1, 2, 3, 0,
  1, 3, 0, 2,
  1, 3, 2, 0,
  2, 0, 1, 3,
  2, 0, 3, 1,
  2, 1, 0, 3,
  2, 1, 3, 0,
  2, 3, 0, 1,
  2, 3, 1, 0,
  3, 0, 1, 2,
  3, 0, 2, 1,
  3, 1, 0, 2,
  3, 1, 2, 0,
  3, 2, 0, 1,
  3, 2, 1, 0 
  };

/// \brief Maximum number of permutations (for 4 items, in this case).
const int MAX_PERM = 24;

/// \brief Electron mass time speed of light squared.
/// From physics.nist.gov 2014-01-28: 0.510998928(11) Mev
const double MEC2 = 510.998928;  //  keV

const double PI=2.*std::asin(1.0);
const double RADTODEG=180./PI;

/// \brief Enumerate DATAMODEs.
enum Datamodes {
  e_CC,
  e_CALMODE,
  e_PSEUDO,
  e_READALL,
  e_NOTSURE
};

/// \brief methods to calculate Delta cos(ThetaG)
enum DeltaGMethod {
  e_ANALYTIC,       ///< using analytic formula (Ichinohe)
  e_CORNER          ///< approximation by check all sensor corner combinations
};

/// \brief cluster shapes
enum ClusterShapes {
  e_NOSHAPE=0,
  e_SINGLE=1,          //  *  single     ** double   *** line
  e_DOUBLE=2,          //
  e_LINE=3,            //  ** elbow      *** tee      *  cross
  e_ELBOW=4,           //   *             *          ***
  e_TEE=5,             //                             *
  e_CROSS=6,           //  ** square
  e_SQUARE=7           //  **
};

const std::string cluster_shape_string[8] = {
  "INVALID",
  "SINGLE",
  "DOUBLE",
  "LINE",
  "ELBOW",
  "TEE",
  "CROSS",
  "SQUARE"
};


///  \brief RECO_STATUS (reconstruction status) bits.  These encode
///  the outcome of event reconstruction in the current row.
///  The numbers denote individual bits in FITS X-type column.
///  

//  Bits 0-4 datamode of single occurrence
//
const int RECO_BIT_UNASSIGNED_1 = 0;   // Note: Am241 for HXI instrument
const int RECO_PSEUDO = 1;
const int RECO_CALMODE = 2;
const int RECO_READALL = 3;
const int RECO_NOTSURE = 4;

//  Bits 5-10 no reconstruction
//
const int RECO_BAD_PROC_STATUS=5;
const int RECO_SKIP_RECO=6;
const int RECO_NO_SIGNALS = 7;
const int RECO_FASTBGO_HITPAT=8;
const int RECO_TOO_MANY_SIGNALS=9;
const int RECO_ALL_SIGNALS_LOW=10;

//  Bits 11-20 reconstruction failed
//
const int RECO_CLUSTER_TOO_MANY_SIGNALS=11;
const int RECO_CLUSTER_WRONG_SHAPE=12;
const int RECO_SI_SI_GROUP_TOO_LARGE=13;
const int RECO_TOO_MANY_HITS=14;
const int RECO_ALL_BAD_F_M_EQ_2=15;
const int RECO_ALL_BAD_F_M_GT_2=16;
const int RECO_ALL_BAD_G=17;
const int RECO_ALL_LOW_PROB_M_EQ_2=18;
const int RECO_ALL_LOW_PROB_M_GT_2=19;
const int RECO_SINGULARITY_IN_ESCAPE_CALC=20;

//  Bits 21-22 trivial reconstruction succeeded
//
const int RECO_ONE_SIGNAL = 21;
const int RECO_ONE_GOOD_SIGNAL = 22;

//  Bits 23-32 complex reconstruction succeeded
//
const int RECO_ONE_HIT_REMAINING_1_0 = 23;
const int RECO_ONE_HIT_REMAINING_1A_1A = 24;
const int RECO_ONE_HIT_REMAINING_1A_1B = 25;
const int RECO_ONE_HIT_REMAINING_1A_2 = 26;
const int RECO_ONE_HIT_REMAINING_1A_3 = 27;
const int RECO_ONE_SEQUENCE_REMAINING_F = 28;
const int RECO_ONE_SEQUENCE_REMAINING_G = 29;
const int RECO_ONE_SEQUENCE_REMAINING_PROBSEQ = 30;
const int RECO_ONE_SEQUENCE_REMAINING_TIE_BREAK = 31;
const int RECO_BIT_UNASSIGNED_2 = 32;

//  Bits 33-36 escape energy calculated
//
const int RECO_GOTO_ESCAPE_CALC_VIA_F_FAILURE = 33;
const int RECO_GOTO_ESCAPE_CALC_VIA_G_FAILURE = 34;
const int RECO_GOTO_ESCAPE_CALC_VIA_PROBSEQ_FAILURE = 35;
const int RECO_PERFORM_ESCAPE_CALC = 36;

//  Bits 37-39 not used
const int RECO_BIT_UNASSIGNED_3 = 37;
const int RECO_BIT_UNASSIGNED_4 = 38;
const int RECO_BIT_UNASSIGNED_5 = 39;

//  Constants for bit ranges
const int BIT_MIN_NO_RECO = RECO_BAD_PROC_STATUS;
const int BIT_MAX_NO_RECO = RECO_ALL_SIGNALS_LOW;

const int BIT_MIN_RECO_FAILED = RECO_CLUSTER_TOO_MANY_SIGNALS;
const int BIT_MAX_RECO_FAILED = RECO_SINGULARITY_IN_ESCAPE_CALC;

const int BIT_MIN_RECO_TRIVIAL = RECO_ONE_SIGNAL;
const int BIT_MAX_RECO_TRIVIAL = RECO_ONE_GOOD_SIGNAL;

const int BIT_MIN_RECO_OK = RECO_ONE_HIT_REMAINING_1_0;
const int BIT_MAX_RECO_OK = RECO_ONE_SEQUENCE_REMAINING_TIE_BREAK;

const std::string reco_string[40] = {
"RECO_BIT_UNASSIGNED_1",
"RECO_PSEUDO",
"RECO_CALMODE",
"RECO_READALL",
"RECO_NOTSURE",
"RECO_BAD_PROC_STATUS",
"RECO_SKIP_RECO",
"RECO_NO_SIGNALS",
"RECO_FASTBGO_HITPAT",
"RECO_TOO_MANY_SIGNALS",
"RECO_ALL_SIGNALS_LOW",
"RECO_CLUSTER_TOO_MANY_SIGNALS",
"RECO_CLUSTER_WRONG_SHAPE",
"RECO_SI_SI_GROUP_TOO_LARGE",
"RECO_TOO_MANY_HITS",
"RECO_ALL_BAD_F_M_EQ_2",
"RECO_ALL_BAD_F_M_GT_2",
"RECO_ALL_BAD_G",
"RECO_ALL_LOW_PROB_M_EQ_2",
"RECO_ALL_LOW_PROB_M_GT_2",
"RECO_SINGULARITY_IN_ESCAPE_CALC",
"RECO_ONE_SIGNAL",
"RECO_ONE_GOOD_SIGNAL",
"RECO_ONE_HIT_REMAINING_1_0",
"RECO_ONE_HIT_REMAINING_1A_1A",
"RECO_ONE_HIT_REMAINING_1A_1B",
"RECO_ONE_HIT_REMAINING_1A_2",
"RECO_ONE_HIT_REMAINING_1A_3",
"RECO_ONE_SEQUENCE_REMAINING_F",
"RECO_ONE_SEQUENCE_REMAINING_G",
"RECO_ONE_SEQUENCE_REMAINING_PROBSEQ",
"RECO_ONE_SEQUENCE_REMAINING_TIE_BREAK",
"RECO_BIT_UNASSIGNED_2",
"RECO_GOTO_ESCAPE_CALC_VIA_F_FAILURE",
"RECO_GOTO_ESCAPE_CALC_VIA_G_FAILURE",
"RECO_GOTO_ESCAPE_CALC_VIA_PROBSEQ_FAILURE",
"RECO_PERFORM_ESCAPE_CALC",
"RECO_BIT_UNASSIGNED_3",
"RECO_BIT_UNASSIGNED_4",
"RECO_BIT_UNASSIGNED_5"};

/// \brief SGD layer types.
//const int SI_STACK=0;
//const int CDTE_BOTTOM=1;
//const int CDTE_SIDE=2;
//const int SI_STACK_MIN=100*SI_STACK;
//const int SI_STACK_MAX=SI_STACK_MIN+99;
//const int CDTE_BOTTOM_MIN=100*CDTE_BOTTOM;
//const int CDTE_BOTTOM_MAX=CDTE_BOTTOM_MIN+99;
//const int CDTE_SIDE_MIN=100*CDTE_SIDE;
//const int CDTE_SIDE_MAX=CDTE_SIDE_MIN+99;

/// \brief LAYER_INDEX values for CdTe layers that are near
///   Si layers (sides or top one of CdTe stack).
//  const int CDTE_NEAR_SI[] = {100, 200, 202, 204, 206};

/// \brief Dimension of CDTE_NEAR_SI.
const int NUM_CDTE_NEAR_SI = 5;

/// \brief Structure to characterize a unique pair of signals 
///   which is a merger candidate.
struct SignalPair {
  int m_j;                    ///< Subscript of first signal
  int m_k;                    ///< Subscript of second signal
  double m_dist;              ///< Distance between signals [mm]
  bool m_flag;                ///< Flag if pair already used
};

struct Par {

  Par(): occurrenceid(0), rejectbgo(false), skipreco(false), strangepix(0),
         numsignal(0), d10(0.), d1a1a(0.), d1a1b(0.), d1a2(0.), d1a3(0.),
         a(0.), b(0.), probaccept2(0.), probaccept3(0.), probaccept4(0.),
         distz(0.), paraoffset0(0.), paraoffset1(0.), paraoffset2(0.),
         weight0(0.), weight1(0.), weight2(0.), weight3(0.), delgmethod(0),
         seed(0), extrainfo(false) {}

  std::string infile;                 ///< Input FITS file name
  std::string outfile;                ///< Output FITS file name
  std::string remapfile;              ///< Definition of ASIC and READOUT remapping
  std::string fluorefile;             ///< File containing energy ranges for fluorescence
  std::string badpixfile;             ///< File containing energy threshold
  std::string probseqfile;            ///< File containing sequence probabilities
  std::string probfovfile;            ///< File containing angular distance probabilities
  long long occurrenceid;             ///< If > 0, process that one occurrence
  bool rejectbgo;                     ///< Reject events for which BGO trigger occurred
  bool skipreco;                      ///< Reconstruction skipped for CALMODE and READALL
  int strangepix;                     ///< Treatment of strange pixel (0=bad, 1=normal, 2=strange)
  std::string outtracefile;           ///< Output file for extra information
  int numsignal;                      ///< Maximum number of signals to analyze
  std::string datamode;               ///< User override of DATAMOD keyword in input file
  double d10_as_read;                 ///< [mm] Shortest distance between two adjacent pixels
  double d1a1a_as_read;               ///< [mm] Diagonal distance between two adjacent pixels in same layer
  double d1a1b_as_read;               ///< [mm] Distance between two layers (CdTe-Cdte fluorescence)
  double d1a2_as_read;                ///< [mm] Distance for combining Si-CdTe fluorescence
  double d1a3_as_read;                ///< [mm] Distance for combining Si-Si electron scattering
  double d10;                         ///< [mm] Shortest distance between two adjacent pixels
  double d1a1a;                       ///< [mm] Diagonal distance between two adjacent pixels in same layer
  double d1a1b;                       ///< [mm] Distance between two layers (CdTe-Cdte fluorescence)
  double d1a2;                        ///< [mm] Distance for combining Si-CdTe fluorescence
  double d1a3;                        ///< [mm] Distance for combining Si-Si electron scattering
  double a;                           ///< [mm] Acceptance tolerance for test of F in step 2
  double b;                           ///< [mm] Acceptance tolerance for test of G in step 2
  double probaccept2;                 ///< Probability threshold for acceptance for M=2 in step 3
  double probaccept3;                 ///< Probability threshold for acceptance for M=3 in step 3
  double probaccept4;                 ///< Probability threshold for acceptance for M=4 in step 3
  double distz;                       ///< [mm] Very large distance for target object in step 4
  double paraoffset0;                 ///< Parameter used in calculating G[k,0] for FOM
  double paraoffset1;                 ///< Parameter used in calculating G[k,1] for FOM
  double paraoffset2;                 ///< Parameter used in calculating G[k,2] for FOM
  double weight0;                     ///< Parameter used in calculating G[k,0] for FOM
  double weight1;                     ///< Parameter used in calculating G[k,1] for FOM
  double weight2;                     ///< Parameter used in calculating G[k,2] for FOM
  double weight3;                     ///< Parameter used in calculating Prob[k] for FOM
  int delgmethod;                     ///< Method to calculate Delta cos(ThetaG); note: parameter is string, but converted to enumerated value: DeltaGMethod
  int seed;                           ///< Seed to use for random number generator (0 for system time)

  // the following are not parameters; just here for convenience
  bool extrainfo;                     ///< Set to false if outtracefile=="NONE"
};

/// \brief structure to hold a single row of data of the output SFFa file; 
///  columns referring to either a bit type or a variable-length array, require
///  two members in the struct: one for data and one for the number of elements
struct OutputRowData {

  OutputRowData(): m_occurrence_id(0), m_pi(0), m_pi_null(0), m_ene_total(0.), 
    m_ene_total_null(0), num_numhits(SIZE_NUMHITS), 
    num_reco_status(hxisgdevtid::SGD_SIZE_RECO_STATUS), m_mattype(0), 
    m_mattype_null(0), m_row_count(0), m_count_pi_null(0) {
                  
    for (ahfits::IndexType ii=0; ii < SIZE_NUMHITS; ii++) m_numhits[ii]=0;
    for (ahfits::IndexType ii=0; ii < hxisgdevtid::SGD_SIZE_RECO_STATUS; ii++) m_reco_status[ii]=0;
    for (ahfits::IndexType ii=0; ii < hxisgdevtid::SGD_SIZE_RECO_STATUS; ii++) m_reco_hist[ii]=0;
  }

  long m_occurrence_id;                        // 1J
 
  int m_pi;                                    // 1I
  char m_pi_null;
  
  float m_ene_total;                          // 1E
  char m_ene_total_null;
  
  int m_numsignal;                             // 1I
  
  char m_numhits[SIZE_NUMHITS];                // 5X
  ahfits::IndexType num_numhits;
  
  int m_seq_hits;                              // 1I
  char m_seq_hits_null;
  
  float m_delcompton[2];                      // 2E
  
  float m_compton_th;                         // 1E
  char m_compton_th_null;
  
  float m_compton_ph;                         // 1E
  char m_compton_ph_null;
  
  float m_distance0;                          // 1E
  char m_distance0_null;
  
  float m_offaxis;                            // 1E
  char m_offaxis_null;
  
  float m_camerax;                            // 1E
  char m_camerax_null;
  
  float m_cameray;                            // 1E
  char m_cameray_null;
  
  float m_cameraz;                            // 1E
  char m_cameraz_null;
  
  float m_probability;                        // 1E
  char m_probability_null;
  
  char m_reco_status[hxisgdevtid::SGD_SIZE_RECO_STATUS];        // 40X
  ahfits::IndexType num_reco_status;
  
  // Added 2014-09-15: specifies the material type where the event was 
  // triggered; 1 - Si layer, 2 - CdTe layer, 3 - multiple layers.  MATTYPE
  // is NULL when PI is NULL.
  int m_mattype;                             // 1I
  char m_mattype_null;

  // The following are not written to the output file; they merely count
  // statistics
  ahfits::IndexType m_reco_hist[hxisgdevtid::SGD_SIZE_RECO_STATUS];
  ahfits::IndexType m_row_count;
  ahfits::IndexType m_count_pi_null;

};
/// \brief structure to hold a single row of data of the extra output (debugging) file.
struct ExtraOutputRowData {

  ExtraOutputRowData(): m_occurrence_id(0), m_numsignal(0), m_m(0), m_numperm(0),
    m_escape_flag(0), num_escape_flag(1),
    num_sigarray0(1), num_sigarray1_0(1), 
    num_sigarray1a_1a(1), num_sigarray1a_1b(1),
    num_sigarray1a_2(1), num_sigarray1a_3(1),
    num_e(1), num_delta_e(1), num_f(1),
    num_delta_f(1), num_check_f(1), num_test_f(1),
    num_cos_theta_g(1), num_delta_cos_theta_g(1), num_cos_theta_k(1),
    num_delta_cos_theta_k(1), num_g(1), num_delta_g(1),
    num_check_g(1), num_test_g(1), num_prob(1), 
    num_test_prob(1), num_fom(1), num_cos_theta_g_0(1), 
    num_delta_cos_theta_g_0(1), num_cos_theta_k_0(1),
    num_delta_cos_theta_k_0(1), num_g_0(1), num_delta_g_0(1), m_row_count(0), 
    m_clstrshape(1),   // initialize cluster shape as single signal (see ClusterShapes enumeration)
    num_merge_flag(1), m_merge1_0(0), 
    m_merge1a_1a(0), m_merge1a_1b(0), m_merge1a_2(0), m_merge1a_3(0), 
    m_rand1_0(0), m_rand1a_3(0), m_rand_fom(0) {
                  
    for (ahfits::IndexType ii=0; ii < MAX_NUMSIGNAL; ii++) {
      m_sigarray0[ii] = -1;
      m_sigarray1_0[ii] = -1;
      m_sigarray1a_1a[ii] = -1;
      m_sigarray1a_1b[ii] = -1;
      m_sigarray1a_2[ii] = -1;
      m_sigarray1a_3[ii] = -1;
    }
    for (ahfits::IndexType ii=0; ii < MAX_NUMHITS; ii++) {
      m_hitarray[ii] = -1;
      m_epiarray[ii] = 0.0;
      m_deltaepiarray[ii] = 0.0;
      m_hitarray_null[ii] = 1;
      m_epiarray_null[ii] = 1;
      m_deltaepiarray_null[ii] = 1;
    }
    for (ahfits::IndexType ii=0; ii < MAX_PERM*4; ii++) {
      m_check_f[ii] = 0;
      m_check_g[ii] = 0;
      m_e[ii] = 0.0;
      m_delta_e[ii] = 0.0;
      m_f[ii] = 0.0;
      m_delta_f[ii] = 0.0;
      m_cos_theta_g[ii] = 0.0;
      m_delta_cos_theta_g[ii] = 0.0;
      m_cos_theta_k[ii] = 0.0;
      m_delta_cos_theta_k[ii] = 0.0;
      m_g[ii] = 0.0;
      m_delta_g[ii] = 0.0;
    }
    for (ahfits::IndexType ii=0; ii < MAX_PERM; ii++) {
      m_test_f[ii] = 0;
      m_test_g[ii] = 0;
      m_test_prob[ii] = 0;
      m_prob[ii] = 0.0;
      m_fom[ii] = 0.0;
      m_cos_theta_g_0[ii] = 0.0;
      m_delta_cos_theta_g_0[ii] = 0.0;
      m_cos_theta_k_0[ii] = 0.0;
      m_delta_cos_theta_k_0[ii] = 0.0;
      m_g_0[ii] = 0.0;
      m_delta_g_0[ii] = 0.0;
    }
  }

  long m_occurrence_id;                        // 1J

  int m_numsignal;        // Original number of signals

  int m_m;                // Number of hits after merging

  int m_numperm;          // Number of hit sequence permutations

  char m_escape_flag;     // 1 if escape energy applied to this row
  ahfits::IndexType num_escape_flag;

  int m_sigarray0[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray0;
  
  int m_sigarray1_0[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray1_0;
  
  int m_sigarray1a_1a[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray1a_1a;
  
  int m_sigarray1a_1b[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray1a_1b;
  
  int m_sigarray1a_2[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray1a_2;

  int m_sigarray1a_3[MAX_NUMSIGNAL];
  ahfits::IndexType num_sigarray1a_3;

  int m_hitarray[MAX_NUMHITS];
  char m_hitarray_null[MAX_NUMHITS];

  float m_epiarray[MAX_NUMHITS];
  char m_epiarray_null[MAX_NUMHITS];

  float m_deltaepiarray[MAX_NUMHITS];
  char m_deltaepiarray_null[MAX_NUMHITS];

  float m_e[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_e;

  float m_delta_e[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_delta_e;

  float m_f[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_f;

  float m_delta_f[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_delta_f;

  char m_check_f[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_check_f;

  char m_test_f[MAX_PERM];
  ahfits::IndexType num_test_f;

  float m_cos_theta_g[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_cos_theta_g;

  float m_delta_cos_theta_g[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_delta_cos_theta_g;

  float m_cos_theta_k[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_cos_theta_k;

  float m_delta_cos_theta_k[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_delta_cos_theta_k;

  float m_g[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_g;

  float m_delta_g[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_delta_g;

  char m_check_g[MAX_PERM*MAX_NUMHITS];
  ahfits::IndexType num_check_g;

  char m_test_g[MAX_PERM];
  ahfits::IndexType num_test_g;

  float m_prob[MAX_PERM];
  ahfits::IndexType num_prob;

  char m_test_prob[MAX_PERM];
  ahfits::IndexType num_test_prob;

  float m_fom[MAX_PERM];
  ahfits::IndexType num_fom;

  float m_cos_theta_g_0[MAX_PERM];
  ahfits::IndexType num_cos_theta_g_0;

  float m_delta_cos_theta_g_0[MAX_PERM];
  ahfits::IndexType num_delta_cos_theta_g_0;

  float m_cos_theta_k_0[MAX_PERM];
  ahfits::IndexType num_cos_theta_k_0;

  float m_delta_cos_theta_k_0[MAX_PERM];
  ahfits::IndexType num_delta_cos_theta_k_0;

  float m_g_0[MAX_PERM];
  ahfits::IndexType num_g_0;

  float m_delta_g_0[MAX_PERM];
  ahfits::IndexType num_delta_g_0;

  ahfits::IndexType m_row_count;

  // From Step 1-0, give the cluster shape of one of the clusters.  The values
  // come from the ClusterShapes enumeration.  There may be more than one 
  // cluster for a given occurrence, but only listing one here since you cannot
  // easily select from an array.
  int m_clstrshape;

  // The following are of type 1X; set to 1 if any merging was done in the step.
  ahfits::IndexType num_merge_flag;
  char m_merge1_0;
  char m_merge1a_1a;
  char m_merge1a_1b;
  char m_merge1a_2;
  char m_merge1a_3;

  // The following are of type 1X; set to 1 if a random selection occurred in 
  // the step.
  char m_rand1_0;
  char m_rand1a_3;
  char m_rand_fom;

};

/// \brief structure to hold data during event reconstruction
struct RowSignals {
  
  RowSignals(): m_nsignal(0) {
    for (ahfits::IndexType i=0; i<MAX_NUMSIGNAL; ++i) {
      m_readout_id_rmap[i] = 0;
      m_rawx[i] = 0;
      m_rawy[i] = 0;
      m_sensor_id[i] = 0;
      m_camerax[i] = 0.0;
      m_cameray[i] = 0.0;
      m_cameraz[i] = 0.0;
      m_epi[i] = 0.0;
      m_layer[i] = 0;
      m_layer_type[i] = 0;
      m_flag[i] = false;

      m_cluster[i] = 0;

      m_sigarray_0[i] = 0;
      m_sigarray_1_0[i] = 0;
      m_sigarray_1a_1a[i] = 0;
      m_sigarray_1a_1b[i] = 0;
      m_sigarray_1a_2[i] = 0;
      m_sigarray_1a_3[i] = 0;

      m_epi_current[i] = 0.0;
      m_var_current[i] = 0.0;

      m_epi_merge_1_0[i] = 0.0;
      m_epi_merge_1a_1a[i] = 0.0;
      m_epi_merge_1a_1b[i] = 0.0;
      m_epi_merge_1a_2[i] = 0.0;
      m_epi_merge_1a_3[i] = 0.0;

      m_var_merge_1_0[i] = 0.0;
      m_var_merge_1a_1a[i] = 0.0;
      m_var_merge_1a_1b[i] = 0.0;
      m_var_merge_1a_2[i] = 0.0;
      m_var_merge_1a_3[i] = 0.0;

      m_merge_survivor[i] = false;
    }
  }

  int m_nsignal;                          ///< Number of signals actually present
  int m_readout_id_rmap[MAX_NUMSIGNAL];   ///< READOUT_ID_RMAP (key value for pixel locations)
  int m_rawx[MAX_NUMSIGNAL];              ///< RAWX
  int m_rawy[MAX_NUMSIGNAL];              ///< RAWY
  int m_sensor_id[MAX_NUMSIGNAL];         ///< SENSOR_ID
  double m_camerax[MAX_NUMSIGNAL];        ///< CAMERAX
  double m_cameray[MAX_NUMSIGNAL];        ///< CAMERAY
  double m_cameraz[MAX_NUMSIGNAL];        ///< CAMERAZ
  double m_epi[MAX_NUMSIGNAL];            ///< EPI as read in
  int m_layer[MAX_NUMSIGNAL];             ///< Layer number
  int m_layer_type[MAX_NUMSIGNAL];        ///< Layer type
  bool m_flag[MAX_NUMSIGNAL];             ///< True if EPI above threshold

  int m_cluster[MAX_NUMSIGNAL];            ///< Tracks cluster membership in step 1-0

  int m_sigarray_0[MAX_NUMSIGNAL];         ///< Original index for valid signals
  int m_sigarray_1_0[MAX_NUMSIGNAL];       ///< Membership in cluster of adjacent pixels
  int m_sigarray_1a_1a[MAX_NUMSIGNAL];     ///< Membership in same-layer CdTe-CdTe fluoresence
  int m_sigarray_1a_1b[MAX_NUMSIGNAL];     ///< Membership in cross-layer CdTe-CdTe fluoresence
  int m_sigarray_1a_2[MAX_NUMSIGNAL];      ///< Membership in Si-CdTe fluoresence
  int m_sigarray_1a_3[MAX_NUMSIGNAL];      ///< Membership in Si-Si scattering

  double m_epi_current[MAX_NUMSIGNAL];        ///< Merged energy from current step
  double m_var_current[MAX_NUMSIGNAL];        ///< Variance in merged energy from current step

  double m_epi_merge_1_0[MAX_NUMSIGNAL];      ///< Merged energy after clusters
  double m_epi_merge_1a_1a[MAX_NUMSIGNAL];    ///< Merged energy after same-layer CdTe-CdTe fluoresence
  double m_epi_merge_1a_1b[MAX_NUMSIGNAL];    ///< Merged energy after cross-layer CdTe-CdTe fluoresence
  double m_epi_merge_1a_2[MAX_NUMSIGNAL];     ///< Merged energy after Si-CdTe fluoresence
  double m_epi_merge_1a_3[MAX_NUMSIGNAL];     ///< Merged energy after Si-Si scattering

  double m_var_merge_1_0[MAX_NUMSIGNAL];      ///< Merged variance after clusters
  double m_var_merge_1a_1a[MAX_NUMSIGNAL];    ///< Merged variance after same-layer CdTe-CdTe fluoresence
  double m_var_merge_1a_1b[MAX_NUMSIGNAL];    ///< Merged variance after cross-layer CdTe-CdTe fluoresence
  double m_var_merge_1a_2[MAX_NUMSIGNAL];     ///< Merged variance after Si-CdTe fluoresence
  double m_var_merge_1a_3[MAX_NUMSIGNAL];     ///< Merged variance after Si-Si scattering

  bool m_merge_survivor[MAX_NUMSIGNAL];      ///< True if signal still represents an independent EPI after last merge
};

/// \brief structure to track the hit sequence.
struct Hits {
  Hits(): m_m(0), m_best_k(-1) {
    for (int i=0; i<MAX_NUMHITS; ++i) {
      m_hitarray[i] = -1; 
      m_epiarray[i] = 0.0; 
      m_deltaepiarray[i] = 0.0; 
    }
  }
      
  int m_m;
  int m_best_k;      // selected permutation index (-1 = none selected)
  int m_hitarray[MAX_NUMHITS];
  double m_epiarray[MAX_NUMHITS];
  double m_deltaepiarray[MAX_NUMHITS];
};

/// \brief structure to track the sequence description.
struct SeqInfo {
  int m_sequence[MAX_NUMHITS];        ///< 0=Si, 1=CdTe bottom, 2=CdTeside, 3=escape, -1=not used
  std::string m_mechanism1; ///< P=photoabsortion, C=Compton, E=escape
};

/// \brief structure to enable sorting ints using a double as a key
///  (in this instance, subscripts of an array are to be sorted by an 
///  associated distance).  std::sort requires this setup.
struct IntDouble {
  int m_int;
  double m_double;
};

typedef std::map<int, std::vector<int> > IntVectorIndex;

/// \brief Reinitialize the signals.
/// \param[in] signals Structure containing signals in an input row
void reInitSignals(RowSignals& signals);

/// \brief Reinitialize the hits.
/// \param[in] hits Structure containing hits in an input row
void reInitHits(Hits& hits);

/// \brief Merge signals that are in adjacent pixels.
/// \param[in] par User input parameters in a structure
/// \param[in] fluor_table Structure containing energy resolution information
/// \param[in] signals Structure containing signals in an input row
/// \param[in,out] outrow Structure containing output file record
/// \param[in,out] extraoutrow Structure containing extra output file record
/// \param[in,out] sequence_type Structure containing description of hit sequence
/// \param[out] reco_complete True if reconstruction finished

void mergeAdjacentSignals (Par & par, hxisgdevtid::fluor::DataType& fluor_table,
  RowSignals& signals, 
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete);

/// \brief Merge signals that are connected by a fluorescence or scattering process.
/// \param[in] par User input parameters in a structure
/// \param[in] signals Structure containing signals in an input row
/// \param[in] fluor_table CALDB fluoresence data
/// \param[in,out] outrow Structure containing output file record
/// \param[in,out] extraoutrow Structure containing extra output file record
/// \param[in,out] sequence_type Structure containing description of hit sequence
/// \param[out] reco_complete True if reconstruction finished
void mergeFluorAndScatter(Par & par, RowSignals& signals,
  hxisgdevtid::fluor::DataType& fluor_table, 
  hxisgdevtid::remap::GeomKeywords geom, 
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete);

/// \brief Merge signals for step 1a_1a (CdTe same layer)
/// \param[in] signals Structure containing signals in an input row
/// \param[in] src Subscript of source signal: dest=dest+src
/// \param[in] dest Subscript of destination signal: dest=dest+src
void merge_signals_1a_1a (RowSignals& signals, int src, int dest);

/// \brief Merge signals for step 1a_1b (CdTe different layers)
/// \param[in] signals Structure containing signals in an input row
/// \param[in] src Subscript of source signal: dest=dest+src
/// \param[in] dest Subscript of destination signal: dest=dest+src
void merge_signals_1a_1b (RowSignals& signals, int src, int dest);

/// \brief Merge signals for step 1a_2 (Si-CdTe)
/// \param[in] signals Structure containing signals in an input row
/// \param[in] src Subscript of source signal: dest=dest+src
/// \param[in] dest Subscript of destination signal: dest=dest+src
void merge_signals_1a_2 (RowSignals& signals, int src, int dest);

/// \brief Merge signals for step 1a_3 (Si-Si)
/// \param[in] signals Structure containing signals in an input row
/// \param[in] group vector group indices to merge together
/// \param[in] dest Subscript of destination signal
void merge_signals_1a_3 (RowSignals& signals, std::vector<int> group, int dest);

/// \brief Test whether two signals are in edge-adjacent pixels.
/// \param[in] signals Structure containing signals in an input row
/// \param[in] j Subscript of first signal
/// \param[in] k Subscript of second signal
/// \param[in] d1_0 Distance defining edge-adjacency
bool pixelsAdjacent(RowSignals& signals, int j, int k, double d1_0);

/// \brief Compare two doubles in order to sort associated ints
/// \param[in] item1 Structure with an int member and a double member
/// \param[in] item2 Structure with an int member and a double member
/// \return True if item1.m_double is less then item2.m_double
bool IntDoubleCompare (const IntDouble& item1, const IntDouble& item2);

/// \brief Perform the F and G tests of scattering angle,
///   together with the low-probability sequence test
/// \param[in] par Structure containing user input parameters
/// \param[in] signals Structure containing signals in an input row
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] fluor_table Structure containing fluoresncefile CALDB data
/// \param[in] probseq_table Structure containing probseqfile CALDB data
/// \param[in] prob array of sequence probabilities for each sequence
/// \param[out] test_f Outcome of F tests on all permutations
/// \param[out] test_g Outcome of G tests on all permutations
/// \param[out] g g[k][j] values (needed form tie-breaking
/// \param[out] escape_flag True if escape energy calculation was necessary
/// \param[in,out] outrow Structure containing output file record
/// \param[in] fpextra File pointer to extra output file
/// \param[in,out] extraoutrow Structure containing extra output file record
/// \param[in,out] sequence_type Structure containing description of hit sequence
/// \param[out] reco_complete True if reconstruction finished
void performFGProbTests (Par& par, RowSignals& signals, Hits& hits,
  hxisgdevtid::fluor::DataType& fluor_table, sgdprobseq::DataType& probseq_table,
  hxisgdevtid::remap::GeomKeywords& geom, double prob[],
  bool test_f[], bool test_g[], double g[][MAX_NUMHITS], bool & escape_flag, double escape_en[MAX_PERM],
  OutputRowData& outrow, ahfits::FilePtr fpextra, ExtraOutputRowData& extraoutrow, 
  SeqInfo& sequence_type, bool& reco_complete);

/// \brief Perform the tie breaking based on FOM
/// \param[in] par Structure containing user input parameters
/// \param[in] signals Structure containing signals in an input row
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] probfov_table Angular distance probability CALDB table
/// \param[in] test_f Outcome of F tests on all permutations
/// \param[in] test_g Outcome of G tests on all permutations
/// \param[in] g g[k][j] values
/// \param[in] escape_flag True if escape energy calculation was necessary
/// \param[in] escape_en Array of escape energies for each permutation
/// \param[in,out] outrow Structure containing output file record
/// \param[in,out] extraoutrow Structure containing extra output file record
/// \param[in,out] sequence_type Structure containing description of hit sequence
/// \param[out] reco_complete True if reconstruction finished
void performTieBreaking (Par& par, RowSignals& signals, Hits& hits,
  hxisgdevtid::remap::GeomKeywords& geom,
  double prob[], bool test_f[], bool test_g[], double g[][MAX_NUMHITS], 
  bool escape_flag, double escape_en[MAX_PERM], OutputRowData& outrow, 
  ExtraOutputRowData& extraoutrow, SeqInfo& sequence_type, bool& reco_complete);

/// \brief Compute the escape energy if needed to make a sequence of hits physical
/// \param[in] ee Stepwise summed energies of all the possible sequences
/// \param[in] var_ee Variance in ee
/// \param[in] delta_ee Error in ee = sqrt(variance)
/// \param[in] cos_tht_g_m_minus_2 The last Comption scattering angle in the sequence
/// \param[in] n_perm Actual number of permutations being tested
/// \param[in] m Actual number of hits in sequence
/// \param[in] mat Material matrix (Si, CdTe bottom, or CdTe side)
/// \paramp[out] escape_en calcuated escape energy for each sequence
/// \return Condition code flagging numerical validity
bool computeEscapeEnergy (double ee[MAX_PERM][MAX_NUMHITS], 
  double var_ee[MAX_PERM][MAX_NUMHITS], double delta_ee[MAX_PERM][MAX_NUMHITS], 
  double cos_tht_g_m_minus_2[MAX_PERM], 
  int n_perm, int m, int mat[MAX_PERM][MAX_NUMHITS],
  hxisgdevtid::fluor::DataType& fluor_table, double escape_en[MAX_PERM]);

/// \brief Make sure m is consistent with signals.m_merge_survivor.  Throws
/// if not.
/// \param[in] signals Signals from current row
/// \param[in] m Number of hits
void assertNumHitsCorrect(RowSignals& signals, int m);

/// \brief Reinitialize the parts of the extra output data row
///   that are computed in F, G, and subsequent computations.
/// \param[in,out] outrow Structure containing output file record
void partialReInitExtraOutputData(ExtraOutputRowData& extraoutrow);

/// \brief For the chosen hit sequence, format the information corresponding
/// to the SEQUENCE and MECHANISM1 columns in the CALDB file specified by
/// the probseqfile parameter.
/// \param[in] signals Structure containing signals in an input row
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] pr Vector of all permutations for m
/// \param[in] ksel Selected permutation
/// \param[in] escape_flag True if escape energy is in effect
/// \param[out] sequence_type Description of the sequence
void getHitSequence(RowSignals& signals, Hits& hits,
  const int* pr, int ksel, bool escape_flag, SeqInfo& sequence_type);

/// \brief Fill in hit sequence information for the case of only one hit.
/// \param[in] signals Structure containing signals in an input row
/// \param[in] escape_flag True if escape energy is in effect
/// \param[out] sequence_type Description of the sequence
void getSequenceOneHit(RowSignals& signals, bool escape_flag, SeqInfo& sequence_type);

/// \brief Fill in hit sequence information for the case of only one hit,
///   in early stages before escape energy calculation is a possibility.
/// \param[in] signals Structure containing signals in an input row
/// \param[out] sequence_type Description of the sequence
void getSequenceOneHitNoEscape(RowSignals& signals, SeqInfo& sequence_type);

/// \brief Fill in CAMERAX, CAMERAY, and CAMERAZ coordinates in
///   output data row for the first hit in the chosen sequence.
/// \param[in] signals Structure containing signals in an input row
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] pr Vector of all permutations for m
/// \param[in] ksel Selected permutation
/// \param[in] escape_en zero or escape energy to add to total EPI
/// \param[in,out] outrow Structure containing output file record
void buildOutputRowFirstHit(RowSignals& signals, Hits& hits, 
    const int* pr, int ksel, double escape_en, OutputRowData& outrow);

/// \brief Fill in PI, CAMERAX, CAMERAY, and CAMERAZ coordinates in
///   output data row for just one hit.
/// \param[in] signals Structure containing signals in an input row
/// \param[in,out] outrow Structure containing output file record
void buildOutputRowOneHit(RowSignals& signals, OutputRowData& outrow);

/// \brief Fill in CAMERAX, CAMERAY, and CAMERAZ coordinates in
///   output data row with null flags, along with PI.
/// \param[in,out] outrow Structure containing output file record
void setOutputEventNull(OutputRowData& outrow);

/// \brief select the array of permutations for 2, 3, or 4 hits
/// \param[in] m Number of hits
/// \param[out] n_perm Number of permutations
/// \return Pointer to n_perm*m elements composing the list of permutations
const int* selectPermArray(int m, int& n_perm);

/// \brief compute cosine of angle subtended at the middle one of 3 points
/// \param[in] ra                position vector of first point
/// \param[in] rb                position vector of middle point
/// \param[in] rc                position vector of last point
double computeCosThetaG(double ra[], double rb[], double rc[]);

/// \brief estimate the error of theta_g by computing maximum difference between
///  the nominal value (obtained by computeCosThetaG) and the angles computed
///  using all combinations of the detector corner positions.
/// \param[in] ra                position vector of first point
/// \param[in] rb                position vector of middle point
/// \param[in] rc                position vector of last point
/// \param[in] epsx              perturbation in CAMERAX
/// \param[in] epsy              perturbation in CAMERAY
/// \param[in] epsz              perturbation in CAMERAZ
double computeDeltaCosThetaG_corner
  (double ra[], double rb[], double rc[], double epsa[], double epsb[], double epsc[]);

/// \brief calculate the  error of theta_g by applying the analytic formula
///  resulting from the derivative of theta_g.
/// \param[in] ra                position vector of first point
/// \param[in] rb                position vector of middle point
/// \param[in] rc                position vector of last point
/// \param[in] epsx              perturbation in CAMERAX
/// \param[in] epsy              perturbation in CAMERAY
/// \param[in] epsz              perturbation in CAMERAZ
double computeDeltaCosThetaG_analytic
  (double ra[], double rb[], double rc[], double epsa[], double epsb[], double epsc[]);

/// \brief compute cosine of angle subtended at the middle one of 3 points
/// \param[in] probfov_table       structure with FOV probability from CALDB
/// \param[in] signals Structure containing signals in an input row
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] escape_en Array of escape energies for each permutation
/// \param[in,out] outrow Structure containing output file record
/// \param[in,out] extraoutrow Structure containing extra output file record
void getProbFOV(sgdprobfov::DataType& probfov_table,
  RowSignals& signals, Hits& hits, double escape_en[MAX_PERM],
  OutputRowData& outrow, ExtraOutputRowData& extraoutrow);

///// \brief Return type of layer in SGD detector.
///// \param[in] layer_num         Official designator of layer
///// \return code for Si, CdTe bottom, or CdTe side
//int layerType(int layer_num);
//
///// \brief Test whether a layer in the SGD detector is CdTe
///// \param[in] layer_num         Official designator of layer
///// \return True if the layer is CdTe
//bool layerIsCdTe(int layer_num);
//
///// \brief Test whether a layer in the SGD detector is Si
///// \param[in] layer_num         Official designator of layer
///// \return True if the layer is Si
//bool layerIsSi(int layer_num);

/// \brief Test whether two layers in the SGD detector are adjacent
///   Not currently implemented (dummy routine).
/// \param[in] layer1         Official designator of layer
/// \param[in] layer2         Official designator of layer
/// \return True if the layers are adjacent
bool layersAdjacent(int layer1, int layer2);

/// \brief Test whether an energy is in the range for CdTe fluorescence
/// \param[in] epi  Energy
/// \param[in] fluor_table  CALDB fluorescence table
bool energyIsCdTeFluor(double epi, hxisgdevtid::fluor::DataType& fluor_table);

/// \brief Test whether an energy is in the range for Si fluorescence
/// \param[in] epi  Energy
/// \param[in] fluor_table  CALDB fluorescence table
bool energyIsSiFluor(double epi, hxisgdevtid::fluor::DataType& fluor_table);

/// \brief Check where a given layer is one of the  CdTe layers
///    immediately near the Si stack.
/// \param[in] layer         Official designator of layer
/// \return True if the layer is in the given set
bool layerIsCdTeNearSi(int layer, 
  hxisgdevtid::remap::GeomKeywords& geom);

bool layerIsCdTeNorthSouth(int layer, 
  hxisgdevtid::remap::GeomKeywords geom);

bool layerIsCdTeEastWest(int layer, 
  hxisgdevtid::remap::GeomKeywords geom);

void camCoordErrors(int layer, int layer_type, 
  hxisgdevtid::remap::GeomKeywords geom, double delta_xyz[]);

/// \brief Set one bit of the RECO_STATUS column
/// \param[in,out] outrow Output SFFa row data
/// \param[in] bit Bit number in X column
void setRecoStatusBit(sgdevtidlib::OutputRowData& outrow, int bit);

/// \brief Zero out the histogram of RECO_STATUS bits
/// \param[in,out] outrow Output SFFa row data (only m_reco_hist field affected)
void clearRecoStatusHist(sgdevtidlib::OutputRowData& outrow);

/// \brief Clear one bit of the RECO_STATUS column
/// \param[in,out] outrow Output SFFa row data
/// \param[in] bit Bit number in X column
void clearRecoStatusBit(sgdevtidlib::OutputRowData& outrow, int bit);

/// \brief Clear all bits of the RECO_STATUS column
/// \param[in,out] outrow Output SFFa row data
void clearRecoStatus(sgdevtidlib::OutputRowData& outrow);

/// \brief calculate the distance between two signals using the CAMERA coordinates
/// \param[in] signals Structure containing signals in an input row
/// \param[in] index of first signal
/// \param[in] index of second signal
/// \return absolute value of distance
double calcDistance(RowSignals& signals, int isig, int jsig);

/// \brief fill the NUMHITS column of the output file.  This column has type,
///  5x, where one of the first 4 bits will be set depending on if there are
///  1, 2, 3, or 4 total hits in the occurrence.  The last bit is set if 
///  an escape energy was calculated.
/// \param[in] hits Structure containing the hits in the current row
/// \param[in] escape_en zero or escape energy to add to total EPI
/// \param[out] outrow Structure containing output file record
void setNumHits(Hits& hits, double escape_en, OutputRowData& outrow);



/** @} */

}

#endif /* SGDEVTID_SGDEVTIDLIB_H */

/* Revision log
 $Log: sgdevtidlib.h,v $
 Revision 1.46  2015/10/30 17:55:15  rshill
 Added datamode parameter to override datamode in event file;
 deleted check for value=*NORMALn.  For conformity with hxisgdpha.

 Revision 1.45  2015/08/13 00:40:54  rshill
 Additional logging output and counts.

 Revision 1.44  2015/08/10 22:55:29  rshill
 Code cleanup.

 Revision 1.43  2015/08/07 16:09:59  rshill
 Removed obsolete InputRowData struct; added Par members for critical distances.

 Revision 1.42  2015/04/28 20:25:47  mwitthoe
 sgdevtid: fix bug where escape energy was not included in event energy in FOV lookup routine; remove obsolete +++ comments

 Revision 1.41  2015/03/16 18:00:25  mwitthoe
 sgdevtid: add strangepix parameter which specifies how 'strange' signals should be treated

 Revision 1.40  2015/03/03 18:37:33  mwitthoe
 sgdevtid: update after Feb 2015 Japan meeting: store delta-F in F test; remove randomization in fluorescence merging; remove expand mode; add energy-dependent energy uncertainties

 Revision 1.37  2015/01/23 17:51:08  mwitthoe
 sgdevtid: use sequence probabilities instead of FOV probabilities in the figure-of-merit calculation

 Revision 1.36  2015/01/22 21:55:24  mwitthoe
 sgdevit: implement FOV probability; see issue 482

 Revision 1.35  2014/12/30 22:06:20  rshill
 Corrected treatment of null EPI and PI.

 Revision 1.34  2014/12/30 18:20:05  rshill
 Incorporated occurrence-by-occurrence datamode requirements.

 Revision 1.33  2014/12/24 20:53:40  rshill
 Modified for parameter standards update

 Revision 1.32  2014/12/24 18:31:33  rshill
 Updated for parameter standards (Redmine issue #472)

 Revision 1.31  2014/11/13 20:47:50  rshill
 Reconciled with new SFFa template.

 Revision 1.30  2014/09/15 17:19:06  mwitthoe
 sgdevtid: add MATTYPE column to output, to be used in gain fitting; see issue 432

 Revision 1.29  2014/07/24 17:40:14  mwitthoe
 sgdevtid: update probm CALDB file format; bug-fix: there were two expand variables being used; bug-fix: there was a mix-up between layer and sensor index when determining location of a signal; bug-fix: extrainfo was not being checked before trying to write the extra output file in sgdevtidlib

 Revision 1.28  2014/06/13 00:39:46  rshill
 Geometry in remap header.  expand parameter and
 outfileextra=NONE.  Corrected error computations.

 Revision 1.27  2014/03/21 18:14:33  mwitthoe
 sgdevtid: in probm CALDB file 1) change type of probhit from int to double, 2) rename columns to match test file sent by Hiro on Mar 20; add best_k to hits structure; print hit sequence to log file (with AH_DEBUG) when occurrence_id is used; initialize test_f/g/prob to false instead of true

 Revision 1.26  2014/03/14 21:55:04  mwitthoe
 sgdevtid: ensure that all output columns are written to; update RECO_STATUS values

 Revision 1.25  2014/03/13 21:03:43  mwitthoe
 sgdevtid: add random selection for Step 1a-2; change FOM expression; include extension name in CALDB lookup function for the badpix file; if single F test passes with M>2, continue to G test; add lookup function for ProbM file to return the MECHANISM index which gets output in the SEQHITS column; print number of occurrences which have PI=NULL to end of log file; remove some old, commented-out code

 Revision 1.24  2014/03/11 21:06:57  mwitthoe
 sgdevtid: fix items 10,13,14,15 in issue #360; this involved removing an unused constant, changing the name of a variable, and updating default parameter values; a new function was added to check if a given energy was consistent with the Si fluorescence energy (the previous version incorrectly used the CdTe energies in Step 1a-2)

 Revision 1.23  2014/03/11 20:24:37  mwitthoe
 sgdevtid: completed SGD items 3 & 4 listed in issue #360; these items involved adding random selection in steps 1-0, 1a-1a, 1a-1b, and tie-breaking; now take absolute value of delta-g in the G test; fixed bug in FOM calculation which was omitting the escape energy

 Revision 1.22  2014/03/05 14:03:50  mwitthoe
 sgdevtid: add a couple more columns to the extra output file to keep track of when merging occurred and the cluster shape (step 1-0); fix bug where total EPI was not calculated properly; in escape loop, automatically fail F test if escape energy is negative; if the calculated PI is out-of-range, set it to the max value: 2048

 Revision 1.21  2014/03/04 14:10:18  mwitthoe
 sgdevtid: change algorithm for Steps 1-0 and 1a-3; in Step 1-0, certain clusters up to size 5 are allowed; in Step 1a-3, up to 3 signals can now be merged (instead of only 2)

 Revision 1.20  2014/03/01 17:22:55  mwitthoe
 sgdevtid: update tool based on testing results on 2/28 between Hiro, Ichinohe, and MCW; the updates include bug fixes and algorithm changes; details are in the Japan notes under Build 5/Sprint 1 on the redmine wiki

 Revision 1.19  2014/02/21 20:51:42  rshill
 Fixed failure to propagate variances through signal merges;
 added occurrence_id parameter to process one selected row.

 Revision 1.18  2014/02/20 22:58:40  rshill
 Bug fixes in writing output rows.

 Revision 1.17  2014/02/12 23:31:07  rshill
 Made some progress debugging.

 Revision 1.16  2014/02/12 01:09:44  rshill
 Debugging in progress.

 Revision 1.15  2014/02/07 01:06:50  rshill
 Debugged several FITS in output and extra output files.

 Revision 1.14  2014/02/06 00:42:15  rshill
 Fixed extra output handling and delta E.

 Revision 1.13  2014/02/05 00:45:15  rshill
 Updated RECO_STATUS handling.

 Revision 1.12  2014/02/03 22:15:25  rshill
 Greatly simplified flow on return from functions;
 rationalized RECO_STATUS bit assignments.

 Revision 1.11  2014/02/03 19:14:04  rshill
 Partway through reconciliation with TRF.  Still builds.

 Revision 1.10  2014/02/03 00:36:31  rshill
 Debugging continues; still builds.

 Revision 1.9  2014/02/02 22:56:16  rshill
 Partway through debugging, but builds.

 Revision 1.8  2014/01/30 20:42:13  rshill
 Added in actually using the probm CALDB file.

 Revision 1.7  2014/01/30 03:51:52  rshill
 Revised for closer TRF conformance.

 Revision 1.6  2014/01/29 01:41:49  rshill
 Completed some version of all routines

 Revision 1.5  2014/01/28 23:39:53  rshill
 Revsions following walkthrough with MW.

 Revision 1.4  2014/01/28 20:31:46  rshill
 Building output row data; improved set of values for condition.

 Revision 1.3  2014/01/27 23:58:32  rshill
 Added extra output file management.  Improved permutation
 management.  Moved many declarations from sgdevtid to sgdevtidlib.

 Revision 1.2  2014/01/25 01:37:07  rshill
 Brought closer to TRF with differences called out.

 Revision 1.1  2014/01/24 19:44:16  rshill
 First commit of SGD event reconstruction functions.

*/
