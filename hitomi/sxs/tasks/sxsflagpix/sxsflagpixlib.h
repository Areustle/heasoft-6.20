/// \file sxsflagpixlib.h
/// \brief functions for sxsflagpix
/// \author Mike Witthoeft
/// \date $Date: 2016/08/22 23:20:35 $

/// \addtogroup tool_sxsflagpix
/// \section tool_sxsflagpix_sxsflagpixlib Supplementary functions for sxsflagpix
///
/// \subsection tool_sxsflagpix_sxspixelmap SXS Pixel Map - sxspixelmap
///
/// Mapping between various SXS pixel indices: chip, side, and quadrant.
///

#ifndef TOOL_SXSFLAGPIX_SXSFLAGPIXLIB_H
#define TOOL_SXSFLAGPIX_SXSFLAGPIXLIB_H

#include "ahgen/ahversion.h"
AHVERSION(TOOL_SXSFLAGPIX_SXSFLAGPIXLIB,"$Id: sxsflagpixlib.h,v 1.17 2016/08/22 23:20:35 mwitthoe Exp $")

#include "ahfits/ahfits.h"
#include "ahsxs/sxsstatus.h"
#include "ahsxs/sxsdt.h"

#include <string>
#include <map>
#include <set>


/// \brief data type of PIXEL column
typedef int type_pixel;

/// \brief data type of row number
typedef long long type_row;

/// \brief data type of TIME column
typedef double type_time;

/// \brief data type of PHA column
typedef long type_pha;

/// \brief data type of PI column
typedef long type_pi;

/// \brief data type of EPI column
typedef double type_epi;

/// \brief data type of element in STATUS column
typedef char type_status;

/// \brief data type of element in PROC_STATUS column
typedef char type_proc_status;

/// \brief data type of CTMULT column
typedef int type_mult;

/// \brief data type of FLG_* columns
typedef char type_flag;

/// \brief data type of DURATION column
typedef double type_duration;

/// \brief data type of the itype column
typedef int type_itype;

/// \brief data type of PSP_ID column
typedef int type_pspid;

/// \brief number of MXS LEDs
const int NUMLED=4;

/// \brief number of bit in PROC_STATUS column
const int LENPROC_STATUS=32;

/// \brief max number of rows allowed in a single group
const int MAXGROUPSIZE=1000;

/// \brief units of the antico DURATION column in seconds
const double ANTDURUNITS=80.e-6;

/// \brief enumerated values for types of flagging performed in this task
enum FlagTypes {
  e_PROX,                      ///< proximity check
  e_CTEL,                      ///< electrical cross talk
  e_CTEL2,                     ///< 2nd electrical cross talk
  e_CTREC,                     ///< recoil cross talk
  e_ANT,                       ///< antico
  e_GTI,                       ///< GTI
  e_MXS                        ///< MXS GTI
};


/// \brief structure to store input parameters
struct Par {
  Par(): m_calcant(false),m_antswitch(0),m_antphathr(0),m_calcctrec(false),
         m_calcprox(false),m_calcctel(false),m_ctelnear(1),m_calcctel2(false),
         m_ctelnear2(1),m_pxpithr(0),m_calcmxs(false),m_kalow(0.),m_kahigh(0.),
         m_kbeta(0.),m_dtflag(false),m_ckrisetime(false) {}

  std::string m_infile;        ///< name of input file
  std::string m_inantfile;     ///< name of antico file
  std::string m_outfile;       ///< name of output file
  std::string m_antpsp;        ///< PSP to use for coincidence (A=PSPA B=PSPB)
  std::string m_antshift;      ///< Shift to apply to the TIME of an antico event(s) or CALDB
  std::string m_gtifile;       ///< GTI input file to include event 
  bool m_calcant;              ///< flag antico pixels
  std::string m_antdtpre;      ///< delta-t before event to check for antico
  std::string m_antdtfol;      ///< delta-t after event to check for antico
  int m_antswitch;             ///< 0 delta time (follow) from file; 1 use parameter 
  int m_antphathr;             ///< PHA threshold for antico
  int m_antdurthr;             ///< DURATION threshold for antico
  bool m_calcctrec;            ///< flag recoil cross-talk
  std::string m_ctrecdt;       ///< delta-t for recoil cross-talk
  bool m_calcprox;             ///< flag event for time proximity
  std::string m_proxdt;        ///< delta time in sec for event the occurs close in time
  bool m_calcctel;             ///< flag electrical cross-talk
  std::string m_pixdeffile;    ///< name of CALDB pixel map file name
  std::string m_cteldt;        ///< delta-t for electrical cross-talk
  int m_ctelnear;              ///< neighbor radius for electrical cross-talk
  bool m_calcctel2;            ///< flag electrical cross-talk 2 (different delta-time)
  std::string m_cteldt2;       ///< delta-t for electrical cross-talk 2
  int m_ctelnear2;             ///< neighbor radius for electrical cross-talk 2
  int m_pxpithr;               ///< Events with PI below this value are skipped (see usepxpithr)
  std::string m_usepxpithr;    ///< Flagging types which skip events based on pxpithr
  bool m_calcmxs;              ///< flag MXS
  std::string m_mxsdt;         ///< delta-t for MXS GTI afterglow
  std::string m_inmxsgti;      ///< name of MXS GTI file
  double m_kalow;              ///< Lower energy limit of Mn K-alpha for recoil test
  double m_kahigh;             ///< Upper energy limit of Mn K-alpha for recoil test
  double m_kbeta;              ///< Energy of Mn K-beta for recoil test
  bool m_dtflag;               ///< True if writing delta-time columns for cross-talk and antico
  bool m_ckrisetime;           ///< True to skip events with RISE_TIME > 127
  std::string m_resetflags;    ///< True to reset all STATUS bits before operations

  std::set<int> m_usepxpithr_values;
  std::set<int> m_resetflags_values;
};

/// \brief structure to information from a single row for cross-talk
struct CTRow {
  CTRow(): m_row(0), m_time(0.), m_time_null(0), m_pixel(0), m_pixel_null(0),
           m_pha(0), m_pha_null(0), m_pi(0), m_pi_null(0), m_epi(0.), m_epi_null(0.),
           m_rise_time(0), m_mult(0), m_itype(0), num_status(ahsxs::LEN_SXSSTATUS),
           num_proc_status(LENPROC_STATUS), m_dtctel(0.), m_dtctel_null(0),
           m_dtctrec(0.), m_dtctrec_null(0), m_isel(false)
           {
             for (int i=0; i < ahsxs::LEN_SXSSTATUS; i++) m_status[i]=0;
             for (int i=0; i < LENPROC_STATUS; i++) m_proc_status[i]=0;
           }

  long long m_row;
  double m_time;
  char m_time_null;
  int m_pixel;
  char m_pixel_null;
  long m_pha;
  char m_pha_null;
  long m_pi;
  char m_pi_null;
  double m_epi;
  char m_epi_null;
  int m_rise_time;
  int m_mult;
  int m_itype;
  char m_status[ahsxs::LEN_SXSSTATUS];
  ahfits::IndexType num_status;
  char m_proc_status[LENPROC_STATUS];
  ahfits::IndexType num_proc_status;
  double m_dtctel;
  char m_dtctel_null;
  double m_dtctrec;
  char m_dtctrec_null;

  // true if event has electrical cross-talk with some other event
  bool m_isel;
};


/// \addtogroup tool_sxsflagpix
namespace sxspixelmap {

/** \addtogroup tool_sxsflagpix
 *  @{
 */

typedef std::map<int, int> pixmap;

/// \brief Describe location of SXS detector pixels.
///
/// The SXS detector has 36 pixels split into two sides of 18 pixels each
/// and four quadrants with 9 pixels each.  This structure will map the 
/// global pixel index (0-35) to each of 1) side number (0,1); 2) side pixel,
/// i.e. XM pixel (0-17); 3) quadrant number (0-3); and 4) quadrant pixel,
/// i.e. SPC pixel (0-8).
typedef struct {
  pixmap m_xmside;             ///< convert pixel to XM side index (0-1)
  pixmap m_xmpixel;            ///< convert pixel to XM pixel (0-17)
  pixmap m_spcquad;            ///< convert pixel to SPC quadrant (0-3)
  pixmap m_spcpixel;           ///< convert pixel to SPC pixel (0-8)
} DataType;

/// \brief read data file and load data into global data map
/// \param[in] filename name of data file
/// \param[out] dat data from CALDB file
void load(const std::string & filename, DataType & dat);

/// \brief clear data
/// \param[in,out] dat data from CALDB file
void clear(DataType & dat);

/// \brief return the XM side number for the given global pixel number
/// \param[in] dat data from CALDB file
/// \return XM side number
double get_xmside(DataType & dat, int pixel);

/// \brief return the XM pixel number for the given global pixel number
/// \param[in] dat data from CALDB file
/// \return XM pixel number
double get_xmpixel(DataType & dat, int pixel);

/// \brief return the SPC quadrant number for the given global pixel number
/// \param[in] dat data from CALDB file
/// \return SPC quadrant number
double get_spcquad(DataType & dat, int pixel);

/// \brief return the SPC pixel number for the given global pixel number
/// \param[in] dat data from CALDB file
/// \return SPC pixel number
double get_spcpixel(DataType & dat, int pixel);


/** @} */

}  // namespace sxspixelmap


/// \brief This function is used to get the value or file name from a time 
///        interval parameter, e.g. antdtpre.  An error is thrown if the value 
///        is negative or the file name does not equal the previously-specified 
///        file name.
/// \param[in] parval         name of the parameter
/// \param[in] value          value of the parameter
/// \parma[in] dtfile         dt filename
/// \parampin] allowneg       do not throw error if value is negative
void getdtpar(const std::string& parval,double& value,std::string& dtfile,
              bool allowneg=false);

/// \brief Parse parameter string containing a list of flagging types.
/// \param[in] parname name of parameter (for error message only)
/// \param[in] parval input parameter string
/// \param[out] types output list of enumerated types
///
/// Valid flagging types:
///  ALL - include all types
///  NONE - include no types
///  PROX - proximity
///  CTEL - electrical cross-talk
///  CTEL2 - 2nd electrical cross-talk 
///  CTREC - recoil cross-talk
///  ANT - antico
///  GTI - GTI
///  MXS - MXS GTI
void parse_flagging_types(const std::string& parname, const std::string& parval,
                          std::set<int>& types);

/// \brief reset STATUS bits
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] types       flagging types to reset STATUS bits for
/// \param[out] clean      if yes, delete output files
void reset_status(ahfits::FilePtr fp, const std::set<int>& types, bool& clean);

/// \brief flag recoil and electrical cross-talk
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] ckrisetime  if true, skip events with RISE_TIME > 127
/// \param[in] ctrecdt     delta time in sec for event cross-talk due to recoil electrons
/// \param[in] cteldt      delta time in sec for electrical cross-talk between pixels
/// \param[in] ctelnear    number of nearby pixels to check for electrical cross-talk
/// \param[in] proxdt      delta-time used to define simultaneous events
/// \param[in] calcctrec   flag recoil cross-talk
/// \param[in] calcctel    flag electrical cross talk
/// \param[in] calcprox    flag simultaneous events
/// \param[in] dtflag      write delta-time output columns
/// \param[in] kalow       PHA comparison value for lowest K-alpha
/// \param[in] kahigh      PHA comparison value for highest K-alpha
/// \param[in] kbeta       PHA comparison value for K-beta
/// \param[in] ctelidx     which electrical cross-talk delta-time to use:
///                          1 - DTCTEL
///                          2 - DTCTEL2
/// \param[in] usepxpithr  true: apply pxpithr to events
/// \param[in] pxpithr     skip events with PI less than this threshold
/// \param[in] pixmapdat   data from SXS pixel map CALDB file
/// \param[out] clean      if yes, delete output files
void process_crosstalk(ahfits::FilePtr fp, bool ckrisetime, double ctrecdt,
                       double cteldt, int ctelnear, double proxdt, bool calcctrec,
                       bool calcctel, bool calcprox, bool dtflag, double kalow,
                       double kahigh, double kbeta, int ctelidx, bool usepxpithr,
                       int pxpithr, sxspixelmap::DataType& pixmapdat,bool& clean);

/// \brief flag antico pixels
/// \param[in] fp          ahfits object of output file (clone of input)
/// \param[in] fpa         ahfits object of antico file (if needed)
/// \param[in] ckrisetime  if true, skip events with RISE_TIME > 127
/// \param[in] antpsp      which PSP to check for antico coincidence
/// \param[in] antshift    Shift to apply to the TIME of an antico event (s) or CALDB
/// \param[in] antdtpre    delta time in sec preceding an antico event
/// \param[in] antdtfol    delta time in sec following an antico event
/// \param[in] antswitch   set antdtfol: if 0 read delta time from file; if 1 use parameter
/// \param[in] antphathr   PHA threshold for antico
/// \param[in] antdurthr   DURATION threshold for antico
/// \param[in] dtflag      write delta-time output columns
/// \param[out] clean      if yes, delete output files
void process_antico(ahfits::FilePtr fp, ahfits::FilePtr fpa, bool ckrisetime,
                    const std::string& antpsp, double antshift, double antdtpre,
                    double antdtfol, int antswitch, int antphathr, int antdurthr,
                    bool dtflag, bool& clean);

/// \brief flag MXS events
/// \param[in] fp            ahfits object of output file (clone of input)
/// \param[in] gp            ahfits object of MXS GTI file (if needed)
/// \param[in] ckrisetime    if true, skip events with RISE_TIME > 127
/// \param[in] mxsdt         afterglow time
/// \param[out] extnumarray  number of rows for each MXS GTI extension
/// \param[out] clean        if yes, delete output files
void process_mxs(ahfits::FilePtr fp, ahfits::FilePtr gp, bool ckrisetime,
                 double mxsdt, long* extnumarray, bool& clean);

/// \brief      Will obtain flagging information for time intervals from gti file
/// \param[in] fp            ahfits object of output file (clone of input)     
/// \param[in] gtifile       GTI input file
/// \param[in] ckrisetime    if true, skip events with RISE_TIME > 127
/// \param[out] clean        if yes, delete output files
void flag_gti(ahfits::FilePtr fp, const std::string & gtifile, bool ckrisetime,
              bool& clean);


#endif /* TOOL_SXSFLAGPIX_SXSFLAGPIXLIB_H */

/* Revision Log
 $Log: sxsflagpixlib.h,v $
 Revision 1.17  2016/08/22 23:20:35  mwitthoe
 sxsflagpix: fix bug where negative values for antshift were not permitted when entered manually (CALDB was OK); now antshift can be positive or negative

 Revision 1.16  2016/08/16 18:12:20  mwitthoe
 sxsflagpix: remove constraint on group size when flagging cross-talk; previously the tool threw an error if there were more than 1000 in a group, now there can be any number

 Revision 1.15  2016/08/11 20:07:01  mwitthoe
 sxsflagpix: 1) switch meaning on 0/1 for GTI STATUS bits - 0 means invalid event or not in GTI, 1 means valid event in GTI; 2) allow negative pxpithr values

 Revision 1.14  2016/07/25 18:46:47  mwitthoe
 sxsflagpix: 1) skip events with PHA/PI/EPI=NULL; 2) do not include pixel 12 events in proximity check; 3) add parameters pxpithr/usepxpithr for setting a PI threshold for including events in the proximity and cross-talk checks; 4) change meaning of resetflags parameter to be a list of flagging types to reset the STATUS bits for

 Revision 1.13  2016/05/24 19:40:12  mwitthoe
 sxsflagpix: add new parameter, resetflags, to set all STATUS bits to zero before flagging new bits

 Revision 1.12  2015/12/08 15:52:27  mwitthoe
 sxsflagpix: convert antico DURATION value to seconds when constructing the antico search interval

 Revision 1.11  2015/12/03 19:36:14  mwitthoe
 sxsflagpix: perform a 2nd electrical cross-talk check using a different delta-time

 Revision 1.10  2015/10/07 16:53:21  mwitthoe
 sxsflagpix: add ckrisetime parameter to skip events with RISE_TIME>127

 Revision 1.9  2015/10/02 20:44:05  mwitthoe
 sxsflagpix: remove pixmap include statement from sxsflagpixlib.h

 Revision 1.8  2015/09/25 20:02:10  mwitthoe
 sxsflagpix: add parameter, antdurthr, which is the DURATION threshold for accepting antico events; if an antico event has a DURATION less than antdurthr, then it is ignored

 Revision 1.7  2015/09/23 14:22:15  mwitthoe
 sxsflagpix: delete output file if there is an error before processing

 Revision 1.6  2015/09/23 14:11:24  mwitthoe
 sxsflagpix: 1) add dtflag parameter which switches on new delta-time output columns for cross-talk and antico (diagnostics); 2) throw error if discovered that TIME column is not sorted; 3) perform recoil cross-talk energy check with EPI instead of PHA; 4) fix parameter description for calcprox; 5) fix afterglow counter for MXS flagging and clarify log messages

 Revision 1.5  2015/07/30 16:07:14  mwitthoe
 sxsflagpix: clean up tool; see issues 532, 534, and 543

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
