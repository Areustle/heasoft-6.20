/// \file sxsbranchlib.h
/// \brief functions for sxsbranch
/// \author Mike Dutka
/// \date $Date: 2015/08/19 21:03:26 $

/// \addtogroup tool_sxsbranch
/// \section tool_sxsbranch_sxsbranchlib Supplementary functions for 
///          sxsbranch
///
///  
/// \subsection tool_sxsbranch
/// 
/// This tools computes the effective exposure time from the branching ratios 
/// of different event grades

#ifndef TOOL_SXSSIMBRANCH_SXSSIMBRANCHLIB_H
#define TOOL_SXSSIMBRANCH_SXSSIMBRANCHLIB_H

#include "ahgen/ahversion.h"
AHVERSION(TOOL_SXSSIMBRANCH_SXSSIMBRANCHLIB,"$Id: sxsbranchlib.h,v 1.8 2015/08/19 21:03:26 mwitthoe Exp $")

#include "ahsxs/ahsxs.h"
#include "ahmission/caldb.h"
#include "ahmission/ahmission.h"
#include "ahmath/ahmath.h"

#include <string>

/// \addtogroup tool_sxsrmf
namespace branchlib {

/// \brief the number of pixels
const int NPIXELS=36;

/// \brief conversion factor from milliseconds to seconds
const double MILTOSEC = 0.001;

// -----------------------------------------------------------------------------

/// \brief structure to store input parameters
struct Par {
  Par(): m_countrate(0.), m_dtprimary(0.), m_dtmidhigh(0.), m_dtlowmid(0.), 
         m_debin(0.), m_rtmax(0.), m_dmmin(0.), m_enrgthr(0.), m_ctphafrac1(0.),
         m_ctphafrac2(0.), m_calpixrate(0.), m_ratburst(0.), m_tburst(0.),
         m_trise(0.), m_tdecay(0.), m_flagmerge(false), m_dtmerge(0.),
         m_maxenergy(0.), m_dtmax(0.), m_seed(0) {}

  std::string m_infile;         ///< Input event file
  std::string m_outfile;        ///< output file contianing branching ratios
  std::string m_filetype;       ///< Type of file real or sim(ulated)
  double m_countrate;           ///< Count rate (per sec) when infile=none 
                                ///< (single pixel case)
  double m_exposure;            ///< exposure time when infile=none 
                                ///< (single pixel case)
  std::string m_dtprimary_str;  ///< Time interval used to distinguish evt as 
                                ///< primary or secondary (ms)
  std::string m_dtmidhigh_str;  ///< Time interval used to distinguish evt as 
                                ///< mid-or high-resolution (ms)
  std::string m_dtlowmid_str;   ///< Time interval used to distinguish evt as 
                                ///< low- or mid-resolution (ms)
  double m_dtprimary;           ///< Time interval used to distinguish evt as 
                                ///< primary or secondary (ms)
  double m_dtmidhigh;           ///< Time interval used to distinguish evt as 
                                ///< mid-or high-resolution (ms)
  double m_dtlowmid;            ///< Time interval used to distinguish evt as 
                                ///< low- or mid-resolution (ms)
  double m_debin;               ///< Energy width of PI channel (ev)
  double m_rtmax;               ///< Maximum risetime for good events
  double m_dmmin;               ///< Minimum deriv_max for good events
  double m_enrgthr;             ///< Energy threshold for event triggering (eV)
  double m_ctphafrac1;          ///< Ratio of nearest-neighbor crosstalk energy 
                                ///< to original
  double m_ctphafrac2;          ///< Ratio of next-nearest-neighbor crosstalk 
                                ///< energy to original
  double m_calpixrate;          ///< Counts per second in the cal-pixel 
                                ///< (pixel 12)
  std::string m_pixfrac;        ///< pixel count-rate fraction table
  std::string m_pixmask;        ///< pixel mask table
  std::string m_ctelpixfile;    ///< SXS electrical pixel map
  bool m_flagburst;             ///< Burst-like lightcurve?
  double m_ratburst;            ///< Ratio of burst count rate at peak to 
                                ///< quiescent rate
  double m_tburst;              ///< Burst start time in seconds after beginning
                                ///< of exposure
  double m_trise;               ///< Burst (linear) rise-time in seconds
  double m_tdecay;              ///< Burst (exponential) decay-time in seconds
  bool m_flagmerge;             ///< Are events within dtmerge merged?
  double m_dtmerge;             ///< Time interval within which event cannot be separated (ms)
  
  double m_maxenergy;           ///< Discard merged events with greater that  this value (MeV) 
  double m_dtmax;               ///< Maximum time span for merging events
  int m_seed;                   ///< Seed for random number generator

};

// -----------------------------------------------------------------------------

/// \brief input file column and keyword values, real and simulated data is stored here
///        also contains sxs specific qunatities for convertSkyXYtoPIXEL for sim data 
struct infileData {
  infileData(): m_time(0.), m_pixel(0), m_x(0.), m_y(0.), m_quick_double(0),
                m_num_quick_double(0), m_slope_differ(0), m_num_slope_differ(0),
                m_pi(0.), m_rise_time(0.), m_itype(0), m_deriv_max(0.), 
                m_el_reason(0), m_el_lost_cnt(0), m_num_status(0), m_itype_sim(0),
                m_router(0), m_nevents(0), m_exposure(0.), m_roll_nom(0.),
                m_tcrpx2(0.), m_tcrpx3(0.), m_tcdlt2(0.), m_tcdlt3(0.), m_evtrate(0.) 
                {
                  for (int ii=0; ii!=ahsxs::LEN_SXSSTATUS; ++ii) m_status[ii]=0;
                }  
  
  //Column data             
  double m_time;                         ///< TIME column (Real and Sim)
  int m_pixel;                           ///< PIXEL column (Real) 
  long m_x;                              ///< X position of source (Sim)
  long m_y;                              ///< Y position of source (Sim)
  char m_quick_double;                   ///< QUICK_DOUBLE column (Real)
  ahfits::IndexType m_num_quick_double;  ///< size of the quick double column
  char m_slope_differ;                   ///< SLOPE_DIFFER column (Real)
  ahfits::IndexType m_num_slope_differ;  ///< size of SLOPE_DIFFER_COLUMN
  long m_pi;                             ///< PI column (Real and Sim)
  double m_rise_time;                    ///< RISE_TIME column (Real)
  int m_itype;                           ///< ITYPE column (Real)
  double m_deriv_max;                    ///< DERIV_MAX column (Real)
  int m_el_reason;                       ///< EL_REASON column (Real)
  int m_el_lost_cnt;                     ///< EL_LOST_CNT column (Real)
  char m_status[ahsxs::LEN_SXSSTATUS];   ///< STATUS column
  ahfits::IndexType m_num_status;        ///< length of STATUS column

  //Output Column data
  int m_itype_sim;                       ///< output itype column for simulated data

  //ahfits router
  ahfits::Router * m_router;             ///< router to connect local varibles to 
                                         ///< input file columns

  //keyword values
  long long m_nevents;                   ///< total number of events (rows in event file)
  double m_exposure;                     ///< total exposure time when infile = real or sim
  double m_roll_nom;                     ///< roll angle of the spacecraft
  double m_tcrpx2;                       ///< X-axis reference pixel of projected image 
  double m_tcrpx3;                       ///< Y-axis reference pixel of projected image
  double m_tcdlt2;                       ///< X increment (deg/pixel) at ref pixel
  double m_tcdlt3;                       ///< Y increment (deg/pixel) at ref pixel
  std::string m_instrume;                ///< INSTRUME keyword value
  std::string m_telescop;                ///< DETECTOR keyword value

  //event rate computed from exposure and nevents 
  //this is not used in the single pixel case
  double m_evtrate;                      ///< event rate

};

// -----------------------------------------------------------------------------

/// \brief struct contains the number of events in each grade for each pixel
struct Nevtgr {
  Nevtgr() {
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_ntotal[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_ngood[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nbad[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nct[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nhp[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nmp[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nlp[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nms[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nls[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nbl[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nlo[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nrj[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nhp_ct[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nmp_ct[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nms_ct[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nlp_ct[ii]=0; 
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nls_ct[ii]=0; 
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_ncounts[ii]=0;
    for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_nquick[ii]=0;
  }
  int m_ntotal[branchlib::NPIXELS];    ///< total number of events
  int m_ngood[branchlib::NPIXELS];     ///< number of good events
  int m_nbad[branchlib::NPIXELS];      ///< number of bad events
  int m_nct[branchlib::NPIXELS];       ///< number of cross-talk events
  int m_nhp[branchlib::NPIXELS];       ///< number of HP events
  int m_nmp[branchlib::NPIXELS];       ///< number of MP events
  int m_nlp[branchlib::NPIXELS];       ///< number of LP events
  int m_nms[branchlib::NPIXELS];       ///< number of MS events
  int m_nls[branchlib::NPIXELS];       ///< number of LS events
  int m_nbl[branchlib::NPIXELS];       ///< number of BL events 
  int m_nlo[branchlib::NPIXELS];       ///< number of LOST events
  int m_nrj[branchlib::NPIXELS];       ///< number of Rejected events
  int m_nhp_ct[branchlib::NPIXELS];    ///< number of HP CT events
  int m_nmp_ct[branchlib::NPIXELS];    ///< number of MP CT events
  int m_nms_ct[branchlib::NPIXELS];    ///< number of MS CT events
  int m_nlp_ct[branchlib::NPIXELS];    ///< number of LP CT events
  int m_nls_ct[branchlib::NPIXELS];    ///< number of LS CT events
  int m_ncounts[branchlib::NPIXELS];   ///< number of counts (> 1 counts per quick-double/lost evt)
  int m_nquick[branchlib::NPIXELS];    ///< number of quick double events
};  

// -----------------------------------------------------------------------------

///Brief contains output varibles which are written to outfile 
struct outputVars { 
  outputVars(): m_branch_hp_est(0.), m_branch_mp_est(0.), m_branch_ms_est(0.),
                m_branch_lp_est(0.), m_branch_ls_est(0.), m_branch_hp(0.), 
                m_branch_mp(0.), m_branch_ms(0.), m_branch_lp(0.), 
                m_branch_ls(0.), m_rate_hp_arr_est(0.), m_rate_mp_arr_est(0.),  
                m_rate_lp_arr_est(0.), m_rate_ms_arr_est(0.), 
                m_rate_ls_arr_est(0.), m_rate_hp_ct_arr_est(0.), 
                m_rate_mp_ct_arr_est(0.), m_rate_lp_ct_arr_est(0.),
                m_rate_ms_ct_arr_est(0.), m_rate_ls_ct_arr_est(0.),
                m_rate_good_arr_est(0.), m_rate_ct_arr_est(0.),
                m_rate_avg_sub_est(0.), m_rate_total_arr_est(0.), 
                m_rate_good_arr(0.), m_rate_bad_arr(0.), m_rate_bl_arr(0.),
                m_rate_lo_arr(0.), m_rate_rj_arr(0.), m_rate_ct_arr(0.),
                m_rate_total_arr(0.), m_rate_hp_arr(0.), m_rate_mp_arr(0.),
                m_rate_ms_arr(0.), m_rate_lp_arr(0.), m_rate_ls_arr(0.),
                m_rate_hp_ct_arr(0.), m_rate_mp_ct_arr(0.), 
                m_rate_ms_ct_arr(0.), m_rate_lp_ct_arr(0.),m_rate_ls_ct_arr(0.),
                m_rate_avg_sub(0.) {
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_hp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_mp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_ms[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_lp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_ls[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_hp_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_mp_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_ms_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_lp_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_expose_ls_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_good_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_total_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_hp_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_mp_est[ii]=0.;   
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_lp_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ms_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ls_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_hp_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_mp_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_lp_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ms_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ls_ct_est[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_good[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_bad[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_bl[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_lo[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_rj[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ct[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_total[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_hp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_mp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ms[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_lp[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ls[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_hp_ct[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_mp_ct[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ms_ct[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_lp_ct[ii]=0.;
                  for (int ii=0; ii!=branchlib::NPIXELS; ++ii) m_rate_ls_ct[ii]=0.;
                }

  //branching ratios
  double m_branch_hp_est;       ///< branching ratio estimate high-primary events
  double m_branch_mp_est;       ///< branching ratio estimate mid-primary events
  double m_branch_ms_est;       ///< branching ratio estimate mid-secondary events
  double m_branch_lp_est;       ///< branching ratio estimate low-primary events
  double m_branch_ls_est;       ///< branching ratio estimate low-secondary events
  
  double m_branch_hp;           ///< branching ratio high-primary events 
  double m_branch_mp;           ///< branching ratio mid-primary events
  double m_branch_ms;           ///< branching ratio mid-secondary events
  double m_branch_lp;           ///< branching ratio low-primary events
  double m_branch_ls;           ///< branching ratio low-secondary events

  double m_rate_hp_arr_est;     ///< average rate estimate high-primary events 
  double m_rate_mp_arr_est;     ///< average rate estimate mid-primary events
  double m_rate_lp_arr_est;     ///< average rate estimate low-primary events
  double m_rate_ms_arr_est;     ///< average rate estimate mid-secondary events
  double m_rate_ls_arr_est;     ///< average rate estimate low-secondary events
  
  double m_rate_hp_ct_arr_est;  ///< average rate estimate high-primary 
                                ///< cross-talk events
  double m_rate_mp_ct_arr_est;  ///< average rate estimate mid-primary 
                                ///< cross-talk events
  double m_rate_lp_ct_arr_est;  ///< average rate estimate low-primary 
                                ///< cross-talk events
  double m_rate_ms_ct_arr_est;  ///< average rate estimate mid-secondard 
                                ///< cross-talk events
  double m_rate_ls_ct_arr_est;  ///< average rate estimate low-secondary 
                                ///< cross-talk events

  double m_rate_good_arr_est;   ///< average rate estimate good events
  double m_rate_ct_arr_est;     ///< average rate estimate cross-talk events 
  double m_rate_avg_sub_est;    ///< average rate estimate good events
  double m_rate_total_arr_est;  ///< average rate estimate good events

  double m_rate_good_arr;       ///< average rate good events
  double m_rate_bad_arr;        ///< average rate bad events
  double m_rate_bl_arr;         ///< average rate baseline events
  double m_rate_lo_arr;         ///< average rate lost events
  double m_rate_rj_arr;         ///< average rate rejected events 
  double m_rate_ct_arr;         ///< average rate cross talk events
  double m_rate_total_arr;      ///< average rate total
  double m_rate_hp_arr;         ///< average rate high-primary
  double m_rate_mp_arr;         ///< average rate mid-primary
  double m_rate_ms_arr;         ///< average rate mid-secondary
  double m_rate_lp_arr;         ///< average rate low-primary
  double m_rate_ls_arr;         ///< average rate low-secondary
  
  //crosstalk events
  double m_rate_hp_ct_arr;      ///< average rate high-primary cross-talk
  double m_rate_mp_ct_arr;      ///< average rate mid primary cross-talk
  double m_rate_ms_ct_arr;      ///< average rate mid secondary cross-talk
  double m_rate_lp_ct_arr;      ///< average rate low primary cross-talk
  double m_rate_ls_ct_arr;      ///< average rate low secondary cross-talk

  double m_rate_avg_sub;        ///< average rate for all pixels

  //effective exposure 
  double m_expose_hp[branchlib::NPIXELS];      ///< effective exposure per pixel high primary
  double m_expose_mp[branchlib::NPIXELS];      ///< effective exposure per pixel mid primary
  double m_expose_ms[branchlib::NPIXELS];      ///< effective exposure per pixel mid secondary
  double m_expose_lp[branchlib::NPIXELS];      ///< effective exposure per pixel low primary
  double m_expose_ls[branchlib::NPIXELS];      ///< effective exposure per pixel low secondary
   
  double m_expose_hp_est[branchlib::NPIXELS];  ///< effective exposure estimate per pixel high primary
  double m_expose_mp_est[branchlib::NPIXELS];  ///< effective exposure estimate per pixel mid primary
  double m_expose_ms_est[branchlib::NPIXELS];  ///< effective exposure estimate per pixel mid secondary
  double m_expose_lp_est[branchlib::NPIXELS];  ///< effective exposure estimate per pixel low primary
  double m_expose_ls_est[branchlib::NPIXELS];  ///< effective exposure estimate per pixel low secondary

  double m_rate_good_est[branchlib::NPIXELS];  ///< estimated rate good events per pixel
  double m_rate_ct_est[branchlib::NPIXELS];    ///< estimated rate cross talk events per pixel
  double m_rate_total_est[branchlib::NPIXELS]; ///< estimated rate total events per pixel

  //estimates for semi analytice calculation
  double m_rate_hp_est[branchlib::NPIXELS];    ///< estimated rate high-primary events per pixel
  double m_rate_mp_est[branchlib::NPIXELS];    ///< estimated rate mid-primary events per pixel
  double m_rate_lp_est[branchlib::NPIXELS];    ///< estimated rate low-primary events per pixel
  double m_rate_ms_est[branchlib::NPIXELS];    ///< estimated rate mid-secondary events per pixel
  double m_rate_ls_est[branchlib::NPIXELS];    ///< estimated rate high-primary events per pixel
  double m_rate_hp_ct_est[branchlib::NPIXELS]; ///< estimated rate high-primary events per pixel
  double m_rate_mp_ct_est[branchlib::NPIXELS]; ///< estimated rate high-primary events per pixel
  double m_rate_lp_ct_est[branchlib::NPIXELS]; ///< estimated rate high-primary events per pixel
  double m_rate_ms_ct_est[branchlib::NPIXELS]; ///< estimated rate high-primary events per pixel
  double m_rate_ls_ct_est[branchlib::NPIXELS]; ///< estimated rate high-primary events per pixel

  //using real and simulated events
  double m_rate_good[branchlib::NPIXELS];      ///< rate good events per pixel
  double m_rate_bad[branchlib::NPIXELS];       ///< rate bad events per pixel
  double m_rate_bl[branchlib::NPIXELS];        ///< rate base line events per pixels
  double m_rate_lo[branchlib::NPIXELS];        ///< rate lost events per pixel
  double m_rate_rj[branchlib::NPIXELS];        ///< rate rejected events per pixel
  double m_rate_ct[branchlib::NPIXELS];        ///< rate cross-talk events per pixel
  double m_rate_total[branchlib::NPIXELS];     ///< rate total per pixel
  double m_rate_hp[branchlib::NPIXELS];        ///< rate high-primary event per pixel
  double m_rate_mp[branchlib::NPIXELS];        ///< rate mid-primary events per pixel
  double m_rate_ms[branchlib::NPIXELS];        ///< rate mid-secondary events per pixel
  double m_rate_lp[branchlib::NPIXELS];        ///< rate low primary evetns per pixel
  double m_rate_ls[branchlib::NPIXELS];        ///< rate low secondary events per pixel
  
  //crosstalk events 
  double m_rate_hp_ct[branchlib::NPIXELS];     ///< rate high primary cross talk events per pixel
  double m_rate_mp_ct[branchlib::NPIXELS];     ///< rate mid-primary cross talk events per pixel
  double m_rate_ms_ct[branchlib::NPIXELS];     ///< rate mid-secondary cross talk events per pixel
  double m_rate_lp_ct[branchlib::NPIXELS];     ///< rate mid-secondary cross talk events per pixel
  double m_rate_ls_ct[branchlib::NPIXELS];     ///< rate mid-secondary cross talk events per pixel
};

// -----------------------------------------------------------------------------

/// \brief structure containing characteristics of output simulated events
///        some events may be merged for high count rate sources
struct simevents {
  simevents(): m_evtnum(0) {}

  std::vector<double> m_time; ///< time of merged event
  std::vector<long> m_x;      ///< x coordinate of simulated event (SKY) 
  std::vector<long> m_y;      ///< y coordinate of simulated event (SKY)
  std::vector<long> m_pi;     ///< pi of simulated event
  std::vector<int> m_pixel;   ///< pixel of simulated event
  std::vector<int> m_pileup;  ///< pileup of simulated event
 
  int m_evtnum;               ///< number of simulated events after merging

};

// -----------------------------------------------------------------------------

/// \brief Assign grade to a list of events
/// \param[in] nevents Number of events
/// \param[in] evtime vector of event times
/// \param[in] evtpix vector of event pixel numbers
/// \param[in] dtprimary delta-time constant for identifying primary events
/// \param[in] dtmidhigh delta-time constant distinguishing mid- and high-res events
/// \param[in] dtlowmid delta-time constant distinguishing low- and mid-res events
/// \param[out] grade vector of grade assignments
void grade(long long nevents, std::vector<double> &evtime, std::vector<int> &evtpix, 
           double dtprimary, double dtmidhigh, double dtlowmid, std::vector<int> &grade);  

// -----------------------------------------------------------------------------

/// \brief Convert sky coordinates to pixel
/// \param[in] xsky_pix discrete input sky coordinate in x-direction
/// \param[in] ysky_pix discrete input sky coordinate in y-direction
/// \param[in] roll_pnt pointing roll angle from HEASIM event FITS header
/// \param[in] crpxx_sky sky x reference pixel from HEASIM event FITS header
/// \param[in] crpxy_sky sky y reference pixel from HEASIM event FITS header
int convertSkyXYtoPIXEL(double xsky_pix, double ysky_pix, double roll_pnt, 
                        double crpxx_sky, double crpxy_sky);  

// -----------------------------------------------------------------------------

/// \brief Return index of the smallest array value larger than (or equal to)
///  the search value.
/// \param[in] pos value to search for
/// \param[in] nvec size of 1D array vec
/// \param[in] vec 1D array to be searched
///
/// Given a monotonically-increasing, ordered vector vec of size nvec, find
/// the smallest index such that vec[index] >= pos; 
/// i.e. vec[index-1] < pos <= vec[index].  The routine is based on the
/// bisection method in Numerical Recipes (subroutine "locate").                                                                  
int find_index(double pos, int nvec, double* vec);

// -----------------------------------------------------------------------------

/// \brief Assign times according to the light-curve of a single burst with 
///   linear risetime and exponential decay.
/// \param[in] nn number of events
/// \param[in] expose simulation exposure time
/// \param[in] ratburst ratio of burst count rate at peak to quiescent rate
/// \param[in] tburst burst start time in seconds after beginning of exposure
/// \param[in] trise burst (linear) rise-time in seconds
/// \param[in] tdecay burst (exponential) decay-time in seconds
/// \param[out] evt_time output vector of event times
/// 
/// Assign times, according to the light-curve of a single burst with linear
/// risetime and exponential decay, to nn events and returns these in a one-
/// dimensional vector (evt_time) of size nn. This algorithm works by mapping
/// the lightcurve onto a uniform distribution using the integral of the
/// lightcurve.  Because the light curve is only piecewise-continuous, a lookup
/// table is employed.
///
/// This function was taken from heasim, with the following modifications:
///  1. use random number function from the Astro-H library
///  2. change output from 1D array to a C++ vector
int timeset_burst(int nn, double expose, double ratburst, double tburst,
                  double trise, double tdecay, std::vector<double>& evt_time);

// -----------------------------------------------------------------------------

/// \brief converts dt parameters to doubles or retreives dtparamters
///        from a CALDB file
/// \param[out] par       structure to store input parameters
/// \param[out] dtfile    file which contains dt parmaters
/// \param[in] datetime   DATE-OBS keyword in input file
void getdt(branchlib::Par& par, const double tstart, std::string datetime);

// -----------------------------------------------------------------------------

/// \brief Opens the event file, reads the neccesary keywords and column names,
///        it does not read the data this is done one line at a time in the 
///        do work block
/// \param[out] fpInput   Fits file pointer to input event file 
/// \param[out] datIn     structure which holds column and keyword data from 
///                       input file 
/// \param[in] par        structure to store input parameters
void open_evtfile(ahfits::FilePtr& fpInput, branchlib::infileData & datIn, 
                  branchlib::Par& par);

// -----------------------------------------------------------------------------

/// \brief  Open the pixel definition file used for sxsflagpix, 
///         reads the neccesary columns and the determines which
///         pixel potentially crosstalk.  
/// \param[in] ctelpixfile name of pixmap file
/// \param[out] prox_arr array containing pixel proximity information
/// \param[out] quad     array contianing pixel proximity information
void mkproxmat(std::string ctelpixfile, int (&prox_arr)[branchlib::NPIXELS][4],
               int (&quad)[branchlib::NPIXELS]);

// -----------------------------------------------------------------------------

/// \brief Branch estimate array will perfrom a semi-analytic calculation to
///        estimate the branching ratios for the SXS array
/// \param[in] rarray       average count rate for all pixels
/// \param[in] pixfrac      name of pixfrac file
/// \param[in] dt1          Time interval used to distinguish evt as 
///                         mid-or high-resolution (ms)
/// \param[in] dt2          Time interval used to distinguish evt as 
///                         low- or mid-resolution (ms) 
/// \param[in] rate_calpix  Counts per second in the cal-pixel 
///                         (pixel 12)
/// \param[in] m_ctphafrac1 Ratio of nearest-neighbor crosstalk energy 
///                         to original
/// \param[in] m_ctphafrac2 Ratio of next-nearest-neighbor crosstalk 
///                         energy to original
/// \param[in] frac_arr     array indicating the fraction of counts in each 
///                         based on the point spread function
/// \param[in] prox_arr     array containing pixel proximity information
/// \param[out] out_arr     array containing semi-analytic estimates for
///                         output quantities  
void branch_est_arr(double& rarray, std::string pixfrac, double& dt1, double& dt2, 
                    double& rate_calpix, double ctphafrac1, double ctphafrac2, 
                    double * frac_arr, int (&prox_arr)[branchlib::NPIXELS][4], 
                    double (&out_arr)[7][branchlib::NPIXELS]);
                 
// -----------------------------------------------------------------------------

/// \brief    timeset assigns times, according to the temporal characteristics 
///           of a source, to nn events and returns these in a one-dimensional 
///           array (evtime) of size nn.
/// \param[in] expose     exposure time
/// \param[in] period     period for sinusoidal 
/// \param[in] pulsed     component of light curve describing a source with 
///                       non-constant bringtness
/// \param[in] nn         number of events
/// \param[out] evtime    vector containing the times of each event
void timeset(double& expose, double period, double pulsed, long long& nn, 
             std::vector<double> &evtime);

// -----------------------------------------------------------------------------

/// \brief Merges events which are within dtmerge of the previous event
///        and create output file with merged events only for sim data
/// \param[in] fpInput  Fits file pointer to input event file
/// \param[in] datIn    structure which holds column and keyword data from 
///                      input file
/// \param[in] par       structure to store input parameters
/// \param[out] simevt   output simulated event file column data
void mergeSim(ahfits::FilePtr& fpInput, branchlib::infileData& datIn, 
              branchlib::Par& par, branchlib::simevents& simevt); 

// -----------------------------------------------------------------------------

/// \brief Merges events which are within dtmerge of the previous event
///        in case where infile==NONE.
/// \param[in] par       structure to store input parameters
/// \param[in] nevents   number of events
/// \param[in] evtime    vector of event times
/// \param[in] evtpix    vector of event pixel numbers
/// \param[out] simevt   output simulated event file column data
void mergeNone(branchlib::Par& par, long long nevents, std::vector<double> &evtime, 
               std::vector<int> &evtpix, branchlib::simevents& simevt); 

// -----------------------------------------------------------------------------

/// \brief sorts simulated event in time order
/// \param[out] simevt   output simulated event file column data
void sortSim(branchlib::simevents& simevt);

// -----------------------------------------------------------------------------

/// \brief writeSim will write the simulated event file 
/// \param[out] fpSim   file pointer to simulated file
/// \param[in] par       structure to store input parameters
/// \param[in] datIn    structure which holds column and keyword data from 
///                      input file
/// \param[in] simevt   output simulated event file column data
void writeSim(ahfits::FilePtr& fpSim, branchlib::Par& par,
              branchlib::infileData& datIn, branchlib::simevents& simevt);

// -----------------------------------------------------------------------------

/// \brief calcBranch determines all of the quanties which will be written to 
///        the final output file  
/// \param[in] par       structure to store input parameters
/// \param[in] exposure  structure which holds column and keyword data from 
///                      input file
/// \param[in] out_arr  array containing semi-analytic estimates for
///                      output quantities  
/// \param[in] mask_arr array identifying which pixels are masked
/// \param[in] nvg      struct containing the numbers of every type of event
/// \param[out] outVars struct containing output variables
void calcBranch(branchlib::Par& par, double exposure, 
                double (&out_arr)[7][branchlib::NPIXELS], 
                int (&mask_arr)[branchlib::NPIXELS], branchlib::Nevtgr &nvg, 
                branchlib::outputVars &outVars); 

// -----------------------------------------------------------------------------

/// \brief Creates output file and write all keywords and column values, close 
///        output file when done writing
/// \param[in] par       structure to store input parameters
/// \param[in] frac_arr array indicating the fraction of counts in each 
///                     based on the point spread function
/// \param[in] prox_arr array containing pixel proximity information
/// \param[out] outVars struct containing output variables
void writeOutput(branchlib::Par &par, double * frac_arr,
                 int (&prox_arr)[branchlib::NPIXELS][4], 
                 branchlib::outputVars &outVars);

// -----------------------------------------------------------------------------

/// \brief interprets the status column returns BAD, CROSSTALK or GOOD depending
///        upon which bits are set in the status column
/// \param[in] status   values of status column from real event files 
std::string readStatus(char* status);

// -----------------------------------------------------------------------------

}//end namespace branchlib


#endif /* TOOL_SXSBRANCH_SXSBRANCHLIB_H */

/* Revision Log
 $Log: sxsbranchlib.h,v $
 Revision 1.8  2015/08/19 21:03:26  mwitthoe
 sxsbranch: doxygen label fixes

 Revision 1.7  2015/08/13 18:12:33  mwitthoe
 sxsbranch: add standard tool prologue, move non-standard functions/structs from tool source to lib file; clean-up code; remove compiler warnings; remove unused functions

 Revision 1.6  2015/07/09 18:11:42  mdutka
 updating branch, fixed issue with lack of preciasion in pixel calculation, fixed writing extra rows in sim out file as well

 Revision 1.5  2015/06/18 17:41:25  mdutka
 fixed issues with exposure time for simulated file

 Revision 1.4  2015/06/18 14:36:18  mdutka
 updating doxygen comments

 Revision 1.3  2015/05/26 19:08:36  mwitthoe
 sxsbranch: 1) bug-fixes from code restructuring; 2) add new burst time calculation from heasim; 3) add merging and cross-talk changes for infile=NONE; 4) update mergeSim() function; 5) fix output column names and types

 Revision 1.2  2015/01/22 20:08:58  mdutka
 updating sxsbranch in process of debugging...

 Revision 1.1  2015/01/06 18:33:09  mdutka
 changed name of sxssimbranch to sxsbranch

 Revision 1.1  2014/12/22 20:46:52  mdutka
 adding new tool sxssimbranch

 Revision 1.3  2014/08/04 17:19:36  mdutka
 Fixed converting double to integer warnings

 Revision 1.2  2014/07/10 18:21:11  mdutka
 all dynamically sized arrays are now initialized to zero, added dynamic sizing to containers which store data from the CALDB information

 Revision 1.1  2014/06/30 19:02:36  mdutka
 Added sxsrmf tool

 Revision 1.4  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.3  2014/01/09 20:33:50  rshill
 Code review comments

 Revision 1.2  2013/11/14 18:44:41  mwitthoe
 sxsflagpix: correct Doxygen section labels in sxsflagpixlib.h

 Revision 1.1  2013/11/12 17:45:03  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)



 BELOW IS THE REVISION LOG FOR sxspixelmap.h BEFORE RENAMING TO sxsflagpixlib.h

 Revision 1.4  2013/07/25 19:47:33  mwitthoe
 fix doxygen tags

 Revision 1.3  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.2  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.1  2013/01/02 19:09:52  mwitthoe
 add new ahcaldb library for the SXS pixel map


*/
