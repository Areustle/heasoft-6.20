/// \file sxipilib.h
/// \brief CALDB access structures and routines for sxipi
/// \author Andy Sargent 
/// \date $Date: 2015/12/29 13:44:28 $
///
/// This header file declares the task-specific enumerations and functions needed for use in the 
/// sxipi task. The functions are defined in sxipilib.cxx.

#ifndef TASKS_SXIPI_SXIPILIB_H
#define TASKS_SXIPI_SXIPILIB_H

#include "ahgen/ahversion.h"
AHVERSION(TASKS_SXIPI_SXIPLIB,"$Id: sxipilib.h,v 1.26 2015/12/29 13:44:28 mdutka Exp $")

#include "ahlog/ahlog.h"
#include "ahfits/ahfits.h"


/// \addtogroup tool_sxipi
namespace sxipilib {

/** \addtogroup tool_sxipi
 * @{
 */

/// \brief Length of PHA arrays
const int LENPHAS = 9;
const int LENPHAS5x5 = 25;
const int LENSTATUS = 48;

const int PHA_TLMIN=0;         ///< TLMIN of PHA column
const int PHA_TLMAX=4095;      ///< TLMAX of PHA column
const int PHA_TNULL=4096;      ///< TNULL of PHA column

const int PI_TLMIN=0;          ///< TLMIN of PI column
const int PI_TLMAX=4095;       ///< TLMAX of PI column
const int PI_TNULL=4096;       ///< TNULL of PI column

const int GRADE_TNULL=-1;      ///< TNULL of GRADE column

typedef std::vector<int> vecInt;       ///< vector typedef of integers
typedef std::vector<char> vecChar;     ///< vector typedef of integers
typedef std::vector<double> vecDbl;    ///< vector typedef of doubles
typedef std::vector<vecDbl> vec2dDbl;  ///< 2D vector typedef of double
typedef std::vector<vecInt> vec2dInt;  ///< 2D vector typedef of integers

/// \brief Define parameter file holding information
struct Params {
  
  std::string m_infile;       ///< Input event file name
  std::string m_outfile;      ///< Output event file name
  std::string m_hkfile;       ///< Input SXI hk file name
  std::string m_hkext;        ///< HK extension name where video temperature is recorded
  std::string m_hkcol;        ///< Column name stem for video temperature
  std::string m_hkvideoid;    ///< Video id keywords
  std::string m_phcut;        ///< Pulseheight cut for CTI correction 

  vecChar m_hkvid_list;

  std::string m_vtevnodd;     ///< CALDB for evenodd correction
  std::string m_chtrailfile;  ///< CALDB for chargetrail correction
  std::string m_ctifile;      ///< CALDB for CTI correction
  std::string m_spthfile;     ///< CALDB for SPLITH correction
  std::string m_gainfile;     ///< CALDB for gain correction
  std::string m_patternfile;  ///< CALDB for grade hit patterns

  std::string m_startcol;     ///< Initial PHAS or PHA for correction
  
  bool m_evnoddcor;           ///< Flag to enable even-odd correction
  bool m_chtrailcor;          ///< Flag to eable charge trail correction
  bool m_cticor;              ///< Flag to enable CTI correction
  bool m_spthiter;            ///< Flag for split threshold iteration 
  bool m_spthcaldb;           ///< Flag to use CALDB values for split threshold
  bool m_gaincor;             ///< Flag to enable gain correction
  bool m_ctigrade;            ///< Flag to use grade info in CTI correction
  bool m_copygrade;           ///< flag to copy existing GRADE and PHA columns

  double m_spthoffset;        ///< Split offset value
  double m_spthslope;         ///< Split slope value

  std::string m_evtthre;              ///< Event threshold value
  int m_negthre;              ///< Minimum PHAS value for normal event
  int m_deltatime;            ///< Time interval to search temperature in HK file
  int m_badpixopt;            ///< Defines behavior for events with bad pixels

  bool m_debugcol;            ///< Keep the debug columns (yes/no)

  bool m_randomize;           ///< Randomize PHA values (yes/no)
  int m_seed;                 ///< Random number generator seed (0 - use system time)

};

struct Keywords {

  Keywords(): m_winSt(0),m_winSize(0),m_ciStatus(0),m_ciPeriod(0),m_ciOffset(0),m_ciFirst(0),
              m_ccdbad(0),m_segbad(0),m_readnodebad(0),m_rawxbad(0),m_actybad(0),m_adcavebad(0),
              m_startcolbad(0),m_ccdmin(0),m_ccdmax(0),m_rawxmin(0),m_rawxmax(0),m_actymin(0),
              m_actymax(0),m_segmin(0),m_segmax(0),m_readnodemin(0),m_readnodemax(0) {}

  int m_huclegth[8];           ///< Length of horizontal underclock region
  int m_vucheght[8];           ///< Height of vertical underclock region
  int m_imgheght[8];           ///< Height of imaging area
  int m_actvnode[8];           ///< Active read out node for each segment
  int m_winSt;                 ///< Window start position in ACTY
  int m_winSize;               ///< Window size/height in ACTY
  int m_ciStatus;              ///< Charge injection status (0: off, 1: on)
  int m_ciPeriod;              ///< Spacing of charge injection rows in ACTY
  int m_ciOffset;              ///< Offset from the first injected row in ACTY
  int m_ciFirst;               ///< First injected row in ACTY

  // Null values of CCD_ID, ACTY, RAWX, SEGMENT, READNODE, ADCAVE, STARTCOL
  long long m_ccdbad;
  long long m_segbad;
  long long m_readnodebad;
  long long m_rawxbad;
  long long m_actybad;
  long long m_adcavebad;
  long long m_startcolbad;

  // Min/Max values of CCD_ID, ACTY, RAWX, SEGMENT, READNODE
  long long m_ccdmin;
  long long m_ccdmax;
  long long m_rawxmin;
  long long m_rawxmax;
  long long m_actymin;
  long long m_actymax;
  long long m_segmin;
  long long m_segmax;
  long long m_readnodemin;
  long long m_readnodemax;

};

struct HKData {
HKData(): m_irowHK0(1), m_numrows(0), m_firstHKrow(0), m_lastHKrow(0) {}

  std::vector<double> m_timeHK;                  ///< HK time from HK file
  vecChar m_timeHK_nullflgs;                     ///< HK time null flags
  std::vector<double> m_vt0;                     ///< video temperature of ASIC PQ on video board A (CCD1)
  std::vector<double> m_vt1;                     ///< video temperature of ASIC RS on video board A (CCD2)
  std::vector<double> m_vt2;                     ///< video temperature of ASIC PQ on video board B (CCD3)
  std::vector<double> m_vt3;                     ///< video temperature of ASIC RS on video board B (CCD4)

  long m_irowHK0; 
  int m_numrows;
  long m_firstHKrow;                        ///< first row with time != null in hk data
  long m_lastHKrow;                         ///< last row with time != null in hk data
 
};

/// \brief define even odd file holding information
struct EvenOddData {

  // Keyword values
  double m_keyVtMin;          ///< Minimum allowable temperature (deg C)
  double m_keyVtMax;          ///< Maximum allowable temperature (deg C)
  double m_keyVtRef;          ///< Reference video temperature (deg C)
  double m_keyPHRefEvenOdd;   ///< Reference PHAS corresponding to 6 keV

  // Column values
  vecInt m_ccdid;             ///< CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
  vecInt m_segment;           ///< Segment ID (0: AB, 1:CD)
  vecInt m_readnode;          ///< Readout node (0: A or C, 1: B or D)
  vecInt m_adcAve;            ///< ADC setting (0, 1, 2, or -1)
  vecInt m_evenOdd;           ///< Even (0) or odd (1)
  vecDbl m_vtGainNorm;        ///< Normalization of video temperature gain function
  vecDbl m_vtGainIndex;       ///< Power-law index for video temperature gain function
  vecDbl m_vtGainOffset;      ///< Offset parameter value for video temperature gain function
  vecDbl m_gainOffset;        ///< Offset parameter value for gain function
  vecDbl m_gainLinr;          ///< Linear coefficient for the gain function
  vecDbl m_gainQuad;          ///< Quadratic coefficient for the gain function

};

/// \brief define charge trail file holding information
struct ChargeTrailData {

  // Keyword values
  double m_keyPHRefChTrail;   ///< Reference PHAS corresponding to 6 keV

  // Column Values
  vecInt m_ccdid;             ///< CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
  vecInt m_segment;           ///< Segment ID (0: AB, 1:CD)
  vecInt m_readnode;          ///< Readout node (0: A or C, 1: B or D)
  vecDbl m_normH;             ///< Power-law normalization for horizontal trail correction
  vecDbl m_normV;             ///< Power-law normalization for vertical trail correction
  vecDbl m_indexH;            ///< Power-law index for horizontal trail correction
  vecDbl m_indexV;            ///< Power-law index for vertical trail  correction
  vecDbl m_offsetH;           ///< Offset parameter value for horizontal trail correction
  vecDbl m_offsetV;           ///< Offset parameter value for vertical trail correction
  vecDbl m_ph_cut;            ///< pulseheight above which energy-dependent CTI correction is used
  vecDbl m_eslope;              ///< energy slope
  vecDbl m_eoffset;             ///< energy offset

};

/// \brief define CTI file holding information
struct CTIData {

  // Keyword Values
  double m_keyCIStatus;       ///< Status of charge injection (0: off, 1: on)
  double m_keyPHRefCTI;       ///< Reference PHAS corresponding to 6 keV

  // Column Values
  vecInt m_ccdid;               ///< CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
  vecInt m_segment;             ///< Segment ID (0: AB, 1:CD)
  vecInt m_readnode;            ///< Readout node (0: A or C, 1: B or D)
  vec2dDbl m_paraFNorm;         ///< Normalization for fast parallel CTI
  vec2dDbl m_paraFProb;         ///< Probability for fast parallel CTI
  vec2dDbl m_paraFScale;        ///< Scale factor for fast parallel CTI
  vec2dDbl m_paraFIndex;        ///< Power-law index for fast parellel CTI
  vec2dDbl m_paraSNorm;         ///< Normalization for slow parallel CTI
  vec2dDbl m_paraSProb;         ///< Probability for slow parallel CTI
  vec2dDbl m_paraSScale;        ///< Scale factor for slow parallel CTI
  vec2dDbl m_paraSIndex;        ///< Power-law index for slow parellel CTI
  vec2dDbl m_paraANorm;         ///< Normalization for anomalous parallel CTI
  vec2dDbl m_paraAProb;         ///< Probability for anomalous parallel CTI
  vec2dDbl m_paraAScale;        ///< Scale factor for anomalous parallel CTI
  vec2dDbl m_paraAIndex;        ///< Power-law index for anomalous parellel CTI
  vec2dDbl m_gfactor;           ///< for each grade 0-11
  vec2dDbl m_goffset_al0;       ///< polynomial coefficient for low energy range
  vec2dDbl m_goffset_al1;       ///< polynomial coefficient for low energy range
  vec2dDbl m_goffset_al2;       ///< polynomial coefficient for low energy range
  vec2dDbl m_goffset_al3;       ///< polynomial coefficient for low energy range
  vec2dDbl m_goffset_am0;       ///< polynomial coefficient for mid energy range
  vec2dDbl m_goffset_am1;       ///< polynomial coefficient for mid energy range
  vec2dDbl m_goffset_am2;       ///< polynomial coefficient for mid energy range
  vec2dDbl m_goffset_am3;       ///< polynomial coefficient for mid energy range
  vec2dDbl m_goffset_ah0;       ///< polynomial coefficient for high energy range
  vecDbl m_goffset_ebl;       ///< pulseheight (energy) bounds for GOFFSET
  vecDbl m_goffset_ebh;       ///< pulseheight (energy) bounds for GOFFSET
  vecDbl m_ph_cut;              ///< pulseheight above which energy-dependent CTI correction is used

};

struct AnomRegData {

  vecInt m_ccdid;
  vec2dInt m_actx;
  vec2dInt m_anomaly_min;
  vec2dInt m_anomaly_max;

};
  

/// \brief define split threshold file holding information
struct SpThData {
  
  vecInt m_ccdid;             ///< CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
  vecInt m_segment;           ///< Segment ID (0: AB, 1:CD)
  vecInt m_readnode;          ///< Readout node (0: A or C, 1: B or D)
  vecDbl m_spthOffset;        ///< Offset for the split threshold function
  vecDbl m_spthLinr;          ///< Linear coefficient for the split threshold function

};

/// \brief define gain file holding information
struct GainData {

  vecInt m_ccdid;             ///< CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
  vecInt m_segment;           ///< Segment ID (0: AB, 1:CD)
  vecInt m_readnode;          ///< Readout node (0: A or C, 1: B or D)
  vecDbl m_offsetLow;         ///< Offset of the gain function in the low energy band
  vecDbl m_linrLow;           ///< Linear coefficient of the gain in the low energy band
  vecDbl m_quadLow;           ///< Quadratic coefficient of the gain in the low energy band
  vecDbl m_offsetMed;         ///< Offset of the gain function in the medium energy band
  vecDbl m_linrMed;           ///< Linear coefficient of the gain in the medium energy band
  vecDbl m_quadMed;           ///< Quadratic coefficient of the gain in the medium energy band
  vecDbl m_offsetHi;          ///< Offset of the gain function in the high energy band
  vecDbl m_linrHi;            ///< Linear coefficient of the gain in the high energy band
  vecDbl m_quadHi;            ///< Quadratic coefficient of the gain in the high energy band
  vecDbl m_boundLM;           ///< Boundary between the low and medium energy bands
  vecDbl m_boundMH;           ///< Boundary between the medium and high energy bands

};

/// \brief define grade file holding information
struct GradeData {

  vecInt m_grade;             ///< Grade of PHAS, defined in TRF
  vec2dInt m_pattern;         ///< 9-digit integer

};

/// \brief read FITS HK file and load data
/// \param[in] hkfile name of even odd data file
/// \param[out] hkdat data from FITS file
/// \param[in]  extname Extension name to move to
/// \param[in]  hkcolstem Column stem to use when reading rows
/// \param[in] tstart observation start time
/// \param[in] tstop  observation stop time
/// \param[in] hkvideoid ???                    // +++ 2015-08-10 MCW need description
 void loadHK(const std::string & hkfile, HKData & hkdat, const std::string& extname,
             const std::string& hkcolstem, double tstart, double tstop,
             const std::vector<char>& hkvideoid);

/// \brief read FITS even odd file and load data
/// \param[in,out] eofile name of even odd data file
/// \param[out] eodat data from FITS file
/// \param[in] tstart observation start time
/// \param[in] tstop observation stop time
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
void loadEvenOdd(std::string & eofile, EvenOddData & eodat, double tstart, 
                 double tstop, const std::string & telescop, 
                 const std::string & instrume);

/// \brief read FITS charge trail file and load data
/// \param[in,out] chtrfile name of charge trail data file
/// \param[out] chtrdat data from FITS file
/// \param[in] tstart   observation start time
/// \param[in] tstop observation stop time
/// \param[in] keydat   structure which contains boundary keywords for CALDB query
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
/// \param[in] expr     CALDB query expression
void loadChargeTrail(std::string & chtrfile, ChargeTrailData & chtrdat, 
                     double tstart, double tstop, sxipilib::Keywords &  keydat, 
                     const std::string & telescop, const std::string & instrume, 
                     const std::string & expr);

/// \brief read FITS CTI file and load data
/// \param[in,out] ctifile name of data file
/// \param[out] ctidat data from FITS file
/// \param[in] tstart observation start time
/// \param[in] tstop observation stop time
/// \param[in] keydat structure which contains boundary keywords for CALDB query
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
/// \param[in] expr     CALDB query expression
void loadCTI(std::string & ctifile, CTIData & ctidat, AnomRegData & AnomRegData,
             double tstart, double tstop, const sxipilib::Keywords & keydat, 
             const std::string & telescop, const std::string & instrume, 
             const std::string & expr);

/// \brief read FITS split threshold file and load data
/// \param[in,out] spthfile name of data file
/// \param[out] spthdat data from FITS file
/// \param[in] tstart   observation start time
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
void loadSpTh(std::string & spthfile, SpThData & spthdat, double tstart, 
              const std::string & telescop, const std::string & instrume);

/// \brief read FITS gain file and load data
/// \param[in,out] gainfile name of data file
/// \param[out] gaindat data from FITS file
/// \param[in] tstart   observation start time
/// \param[in] keydat structure which contains boundary keywords for CALDB query
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
/// \param[in] expr     CALDB query expression
void loadGain(std::string & gainfile, GainData & gaindat, 
              double tstart, const sxipilib::Keywords & keydat, 
              const std::string & telescop, const std::string & instrume, 
              const std::string & expr);

/// \brief read FITS grade file and load data
/// \param[in,out] gradefile name of data file
/// \param[out] gradedat data from FITS file
/// \param[in] telescop TELESCOP keyword used in query
/// \param[in] instrume INSTRUME keyword used in query
void loadGrade(std::string & gradefile, GradeData & gradedat,
               const std::string & telescop, const std::string & instrume);

/// \brief clear data from even odd data structure 
/// \param[in, out] eodat   data from FITS file
void clearEvenOdd(EvenOddData & eodat);

/// \brief clear data from charge trail data structure
/// \param[in, out] chtrdat data from FITS file
void clearChargeTrail(ChargeTrailData & chtrdat);

/// \brief clear data from CTI data structure
/// \param[in, out] ctidat  data from FITS file
void clearCTI(CTIData & ctidat);

/// \brief clear data from split threshold data structure
/// \param[in, out] spthdat data from FITS file
void clearSpTh(SpThData & spthdat);

/// \brief clear data from gain data structure
/// \param[in, out] gaindat data from FITS file
void clearGain(GainData & gaindat);

/// \brief clear data from grade data structure
/// \param[in, out] gradedat  data from FITS file
void clearGrade(GradeData & gradedat);

/// \brief Search for first match within even odd data
/// \param[in] eodat            Data structure holding even odd file information
/// \param[in] ccdid            CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segment          Segment ID (0: AB, 1:CD)
/// \param[in] readnode         Readout node (0: A or C, 1: B or D)
/// \param[in] adcAve           ADC setting (0, 1, 2, or -1)
/// \param[in] evenOdd          Even: 0, odd: 1
/// \param[out] vtGainNorm      Normalization of video temperature gain function
/// \param[out] vtGainIndex     Power-law index for video temperature gain function
/// \param[out] vtGainOffset    Offset parameter value for video temperature gain function
/// \param[out] gainOffset      Offset parameter value for gain function
/// \param[out] gainLinr        Linear coefficient for the gain function
/// \param[out] gainQuad        Quadratic coefficient for the gain function
void searchEvenOdd(const sxipilib::EvenOddData& eodat, char ccdid, char segment,
                   char readnode, int adcAve, int evenOdd, double & vtGainNorm,
                   double & vtGainIndex, double & vtGainOffset, double & gainOffset,
                   double & gainLinr, double & gainQuad);

/// \brief Search for first match within charge trail data
/// \param[in] chtrdat          Data structure holding charge trail file information
/// \param[in] ccdid            CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segment          Segment ID (0: AB, 1:CD)
/// \param[in] readnode         Readout node (0: A or C, 1: B or D)
/// \param[out] normH           Power-law normalization for horizontal trail correction
/// \param[out] normV           Power-law normalization for vertical trail correction
/// \param[out] indexH          Power-law index for horizontal trail correction
/// \param[out] indexV          Power-law index for vertical trail correction
/// \param[out] offsetH         Offset parameter value for horizontal trail correction
/// \param[out] offsetV         Offset parameter value for vertical trail correction
void searchChargeTrail(const sxipilib::ChargeTrailData& chtrdat, char ccdid, char segment, 
                       char readnode, double & normH, double & normV, double & indexH, 
                       double & indexV, double & offsetH, double & offsetV, double & ph_cut);

/// \brief Search for first match within CTI data
/// \param[in] ctidat           Data structure holding CTI file information
/// \param[in] ccdid            CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segment          Segment ID (0: AB, 1:CD)
/// \param[in] readnode         Readout node (0: A or C, 1: B or D)
/// \param[out] paraFNorm       Normalization for fast parallel CTI
/// \param[out] paraFProb       Probability for fast parallel CTI
/// \param[out] paraFScale      Scale factor for fast parallel CTI
/// \param[out] paraFIndex      Power-law index for fast parellel CTI
/// \param[out] paraSNorm       Normalization for slow parallel CTI
/// \param[out] paraSProb       Probability for slow parallel CTI
/// \param[out] paraSScale      Scale factor for slow parallel CTI
/// \param[out] paraSIndex      Power-law index for slow parellel CTI
/// \param[out] paraANorm       Normalization for anomalous parallel CTI
/// \param[out] paraAProb       Probability for  parallel CTI
/// \param[out] paraAScale      Scale factor for anomalous parallel CTI
/// \param[out] paraAIndex      Power-law index for anomalous parellel CTI
/// \param[out] gfactor         for each grade 0-11
/// \param[out] goffset_al0     polynomial coefficient for low energy range
/// \param[out] goffset_al1     polynomial coefficient for low energy range
/// \param[out] goffset_al2     polynomial coefficient for low energy range
/// \param[out] goffset_al3     polynomial coefficient for low energy range
/// \param[out] goffset_am0     polynomial coefficient for mid energy range
/// \param[out] goffset_am1     polynomial coefficient for mid energy range
/// \param[out] goffset_am2     polynomial coefficient for mid energy range
/// \param[out] goffset_am3     polynomial coefficient for mid energy range
/// \param[out] goffset_ah0     polynomial coefficient for high energy range
/// \param[out] goffset_ebl     pulseheight (energy) bounds for GOFFSET
/// \param[out] goffset_ebh     pulseheight (energy) bounds for GOFFSET
/// \param[out] ph_cut          pulseheight above which energy-dependent CTI correction is used

void searchCTI(const sxipilib::CTIData& ctidat, char ccdid, char segment, 
               char readnode, vecDbl & paraFNorm, vecDbl & paraFProb, 
               vecDbl & paraFScale, vecDbl & paraFIndex, vecDbl & paraSNorm, 
               vecDbl & paraSProb, vecDbl & paraSScale, vecDbl & paraSIndex,
               vecDbl & paraANorm, vecDbl & paraAProb, vecDbl & paraAScale, 
               vecDbl & paraAIndex, vecDbl & gfactor, vecDbl & goffset_al0, 
               vecDbl & goffset_al1, vecDbl & goffset_al2, vecDbl & goffset_al3,
               vecDbl & goffset_am0, vecDbl & goffset_am1, vecDbl & goffset_am2,
               vecDbl & goffset_am3, vecDbl & goffset_ah0, double & goffset_ebl,
               double & goffset_ebh, double & ph_cut);


/// \param[in] anomregdata   struct contain anomly region data
/// \param[in] ccd_id        CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] actx          pixel actx coordinate
/// \param[out] anomaly_min  
/// \param[out] anomaly_max
void searchAnomReg(AnomRegData anomregdat, char & ccd_id, int & actx, 
                   vecInt & anomaly_min, vecInt & anomaly_max);  

/// \brief Search for first match within split threshold data
/// \param[in] spthdat          Data structure holding split threshold file information
/// \param[in] ccdid            CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segment          Segment ID (0: AB, 1:CD)
/// \param[in] readnode         Readout node (0: A or C, 1: B or D)
/// \param[out] spthOffset      Offset for the split threshold function
/// \param[out] spthLinr        Linear coefficient for the split threshold function
void searchSPTH(const sxipilib::SpThData& spthdat, char ccdid, char segment,
                char readnode, double & spthOffset, double & spthLinr);

/// \brief Search for first match within gain data
/// \param[in] gaindat          Data structure holding gain file information
/// \param[in] ccdid            CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segment          Segment ID (0: AB, 1:CD)
/// \param[in] readnode         Readout node (0: A or C, 1: B or D)
/// \param[out] offsetLow       Offset of the gain function in the low energy band
/// \param[out] linrLow         Linear coefficient of the gain in the low energy band
/// \param[out] quadLow         Quadratic coefficient of the gain in the low energy band
/// \param[out] offsetMed       Offset of the gain function in the medium energy band
/// \param[out] linrMed         Linear coefficient of the gain in the medium energy band
/// \param[out] quadMed         Quadratic coefficient of the gain in the medium energy band
/// \param[out] offsetHi        Offset of the gain function in the high energy band
/// \param[out] linrHi          Linear coefficient of the gain in the high energy band
/// \param[out] quadHi          Quadratic coefficient of the gain in the high energy band
/// \param[out] boundLM         Boundary between the low and medium energy bands
/// \param[out] boundMH         Boundary between the medium and high energy bands
void searchGain(const sxipilib::GainData& gaindat, char ccdid, char segment, char readnode,
                double & offsetLow, double & linrLow, double & quadLow,
                double & offsetMed, double & linrMed, double & quadMed,
                double & offsetHi, double & linrHi, double & quadHi,
                double & boundLM, double & boundMH);

/// \brief search for first match within grade data
/// \param[in] gradedat         Data structure holding grade file information
/// \param[in] pattern3x3Evt    Grade of PHAS, defined in TRF
/// \param[out] grade3x3        9-digit integer
void searchGradeData(const GradeData& gradedat, int * pattern3x3Evt, int & grade3x3);

/// \brief parse keyword string value from CALDB file to obtain double value 
/// \param[in] keystr           Input string of format KEYWORDSTR(0) to parse
/// \param[out] keydbl          Output value from string
void parseCALDBKeywordDbl(const std::string& keystr, double & keydbl); 

/// \brief Get temperature at the closest HK time before event time
/// \param[in] timeEvt        Time of event from event file
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] deltatime      Time interval to search temperature in HK file
/// \param[out] vt            Video temperature
/// \param[out] vtStatus      If closest time has valid temperature status: 0, else -1
/// \param[in] vtmin          Minimum VT from even odd data
/// \param[in] vtmax          Maximum VT from even odd data
/// \param[in,out] hkdat      Data structure to hold information from HK file (search index modified by this function)
void searchVT(double timeEvt, char ccdidEvt, double deltatime,
              double & vt, int & vtStatus, double vtmin, double vtmax, 
              sxipilib::HKData & hkdat);

/// \brief Even odd correction 
/// \param[in] phasTmp        PHAS for specified event
/// \param[in] timeEvt        Event time from event file
/// \param[in] rawxEvt        RAWX coordinate for specified event
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segmentEvt     Segment ID (0: AB, 1:CD)
/// \param[in] readnodeEvt    Readout node (0: A or C, 1: B or D)
/// \param[in] adcAveEvt      ADC setting (0, 1, 2, or -1)
/// \param[in] evenOddCor     Flag to enable even-odd correction
/// \param[in] deltatime      Time interval to search temperature in HK file
/// \param[in] eodat          Data structure holding information from even odd CALDB file
/// \param[out] phasEvenOdd   Corrected copy of event PHAS
/// \param[out] errorStatus   Status 0: OK, 1: Continue to next row, 2: stop program
/// \param[in] hkdat          Data structure to hold information from HK file 
/// \param[in] param          Data structure containing data from parameter file
/// \param[out] statusEvt     event status
void correctEvenOdd(double * phasTmp, double timeEvt, int rawxEvt, int actxEvt, char ccdidEvt, 
                    char segmentEvt, char readnodeEvt, int adcAveEvt, const int * actvnode, int evenOddCor,
                    double deltatime, const sxipilib::EvenOddData& eodat,
                    double * phasEvenOdd, int & errorStatus, sxipilib::HKData & hkdat,
                    const sxipilib::Params& param, char * statusEvt);

/// \brief Charge trail correction for lost charges during transfer
/// \param[in] phasTmp        Corrected copy of event PHAS
/// \param[in] rawxEvt        RAWX coordinate for specified event
/// \param[in] actyEvt        ACTY coordiante for specified event
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segmentEvt     Segment ID (0: AB, 1:CD)
/// \param[in] readnodeEvt    Readout node (0: A or C, 1: B or D)
/// \param[in] statusEvt      Check if event is on a CI row
/// \param[in] huclegth       Length of horizontal underclock region
/// \param[in] vucheght       Height of vertical underclock region
/// \param[in] imgheght       Height of imaging area
/// \param[in] ciStatus       Charge injection status (0: off, 1: on)
/// \param[in] ciPeriod       Spacing of charge injection rows in ACTY
/// \param[in] ciOffset       First row of CI in ACTY
/// \param[in] ciFirst        First injected row in ACTY
/// \param[in] chTrailCor     Flag to eable charge trail correction
/// \param[in] chtrdat        Data structure holding information from charge trail CALDB file
/// \param[out] phasChTrail   Corrected copy of even odd PHAS
/// \param[out] errorStatus   Status 0: OK, 1: Continue to next row, 2: stop program
void correctChargeTrail(double * phasTmp, int rawxEvt, int actxEvt, int actyEvt, 
                        char ccdidEvt, char segmentEvt, char readnodeEvt, 
                        char * statusEvt, int huclegth, int vucheght, int imgheght,
                        int ciStatus, int ciPeriod, int ciOffset, int ciFirst,
                        int chTrailCor, const sxipilib::ChargeTrailData& chtrdat,
                        double * phasChTrail, int & errorStatus);

/// \brief CTI correction for lost charges during transfer
/// \param[in] phasTmp        Corrected copy of even odd PHAS
/// \param[in] rawxEvt        RAWX coordinate for specified event
/// \param[in] actyEvt        ACTY coordiante for specified event
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segmentEvt     Segment ID (0: AB, 1:CD)
/// \param[in] readnodeEvt    Readout node (0: A or C, 1: B or D)
/// \param[in] huclegth       Length of horizontal underclock region
/// \param[in] vucheght       Height of vertical underclock region
/// \param[in] imgheght       Height of imaging area
/// \param[in] winSt          Window start position in ACTY
/// \param[in] winSize        Window size/height in ACTY
/// \param[in] ciStatus       Charge injection status (0: off, 1: on)
/// \param[in] ciPeriod       Spacing of charge injection rows in ACTY
/// \param[in] ciOffset       First row of CI in ACTY
/// \param[in] ciFirst        First injected row in ACTY
/// \param[in] ctiCor         Flag to enable CTI correction
/// \param[in] ctidat         Data structure holding information from CTI CALDB file
/// \param[out] phasCTI       Corrected copy of charge trail PHAS
/// \param[out] errorStatus   Status 0: OK, 1: Continue to next row, 2: stop program
void correctCTI(double * phasTmp, int gradeEvt, double phaEvt, int rawxEvt, int actxEvt, 
                int actyEvt, const sxipilib::Params& param, 
                char ccdidEvt, char segmentEvt, char readnodeEvt, const int * actvnode, int huclegth,
                int vucheght, int imgheght, int winSt, int winSize, 
                int ciStatus, int ciPeriod, int ciOffset, int ciFirst, 
                int ctiCor, const sxipilib::CTIData& ctidat, 
                const sxipilib::AnomRegData & AnomRegData, 
                double * phasCTI, int & errorStatus);

/// \brief Grade assignment and PHA calculation
/// \param[in] phasTmp        Corrected copy of charge trail PHAS
/// \param[in] phasMask       Mask events with bad pixel
/// \param[in] badpixopt      Defines behavior for events with bad pixels      
/// \param[in] pOuterMostEvt  0: pixel PH < split threshold, 1: pixel PH >= split threshold
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segmentEvt     Segment ID (0: AB, 1:CD)
/// \param[in] readnodeEvt    Readout node (0: A or C, 1: B or D)
/// \param[in] spthiter       Flag to use constant split threshold in CALDB
/// \param[in] spthcaldb      Flag to use CALDB values for split threshold
/// \param[in] spthoff        Split offset value (if flag spthcaldb is no)
/// \param[in] spthslope      Split slope value (if flag spthcaldb is no)
/// \param[in] gradedat       Data structure holding information from hit pattern CALDB file
/// \param[in] spthdat        Data structure holding information from split threshold CALDB file
/// \param[out] phaEvt        PHA from event file
/// \param[out] gradeEvt      Grade from event file
/// \param[out] phaSpTh       Event PHA with split theshold correction
/// \param[out] gradeSpTh     Event grade with split threshold correction
/// \param[out] errorStatus   Status 0: OK, 1: Continue to next row, 2: stop program
void gradePHA(double * phasTmp, int * phasMask, int badpixopt, short pOuterMostEvt, 
              char ccdidEvt, char segmentEvt, char readnodeEvt, bool spthiter, 
              bool spthcaldb, int spthoff, int spthslope,
              const sxipilib::GradeData& gradedat, const sxipilib::SpThData& spthdat,
              double & phaEvt, int & gradeEvt, double & phaSpTh, int & gradeSpTh,
              int & errorStatus);

/// \brief Iterate over the split threshold
/// \param[in] phas           Corrected PHAS
/// \param[in] pOuterMostEvt  0: pixel PH < split threshold, 1: pixel PH >= split threshold
/// \param[in] gradedat       Data structure holding information from hit pattern CALDB file
/// \param[in] spth           Energy dependent split threshold
/// \param[out] phaEvt        Final PHA of event
/// \param[out] gradeEvt      Final grade of event
void gradeIter(double * phas, short pOuterMostEvt, const sxipilib::GradeData& gradedat,
               double spth, double & phaEvt, int & gradeEvt);

/// \brief Check the two abutting neighbors of a corner pixel
/// \param[in] pattern3x3Evt    3x3 hit pattern; 1: pixel PH >= thresh, 2: pixel PH < thresh
/// \param[in] cornerPixelIndex Index variable for checking corners of 3x3 hit pattern
/// \param[in] checkPixInd1     Adjacent pixels next to pixel corners
/// \param[in] checkPixInd2     Adjacent pixels next to pixel corners
/// \param[in] phas             PHAS array
/// \return Return 1 if corner PH is greater than both neighbor PH values, 0 otherwise
int checkCorner(int * pattern3x3Evt, int cornerPixelIndex, int checkPixInd1, 
                int checkPixInd2, double * phas);

/// \brief Check the outer 5x5 hit pattern against the 3x3 hit pattern
/// \param[in] pattern3x3Evt  3x3 hit pattern; 1: pixel PH >= thresh, 2: pixel PH < thresh
/// \param[in] pattern5x5Evt  Outer 16 elements of hit pattern
/// \return Return 1 if there is any bordering pixel, 0 otherwise
int checkOuter(int * pattern3x3Evt, int * pattern5x5Evt);

/// \brief PHA to PI conversion, final gain correction
/// \param[in] phaEvt         Final PHA of event
/// \param[in] ccdidEvt       CCD ID (0: SXI1, 1: SXI2, 2:SXI3, 3:SXI4)
/// \param[in] segmentEvt     Segment ID (0: AB, 1:CD)
/// \param[in] readnodeEvt    Readout node (0: A or C, 1: B or D)
/// \param[in] gaindat        Data structure holding information from gain CALDB file
/// \param[out] piEvt         Final PI of event
/// \param[out] errorStatus   Status 0: OK, 1: Continue to next row, 2: stop program
void correctGain(double phaEvt, char ccdidEvt, char segmentEvt, char readnodeEvt, 
                 const sxipilib::GainData& gaindat, double & piEvt, int & errorStatus);

/// \brief parse a string of the form (1,2,3,4,5) into an array
/// \param[in] str             string to be parsed, delimited by commas
/// \param[in] length          length of output string
/// \param[out] outarr         output array containing delimited contents of str
void parseStringInt(std::string str, int length, int * outarr);

/// \brief helper function to create TNULL, TLMIN, TLMAX keywords
/// \param[in] keyroot         Keyword root such as TNULL, TLMIN, TLMAX
/// \param[in] colnum          Column number to concatenate with keyword root
/// \return Concatenated string (e.g. TLMIN11)
std::string concatenate(std::string const & keyroot, int colnum);

/// \brief set debug columns to NULL
/// \param[in,out] phasEvenOdd   Corrected copy of event PHAS
/// \param[in,out] phasChTrail   Corrected copy of even odd PHAS
/// \param[in,out] phasCTI       Corrected copy of charge trail PHAS
/// \param[in,out] len           length of phasEvenOdd, phasChTrail, and phasCTI arrays
/// \param[in,out] phaSpTh       Event PHA with split theshold correction
/// \param[in,out] gradeSpTh     Event grade with split threshold correction
void setDebugNull(double* phasEvenOdd, double* phasChTrail, double* phasCTI,
                  int len, double& phaSpTh, int& gradeSpTh);

/// \brief set output columns to NULL
/// \param[in,out] phaEvt event PHA variable
/// \param[in,out] gradeEvt event grade variable
/// \param[in,out] piEvt event PI variable
void setOutputNull(double& phaEvt, int& gradeEvt, double& piEvt);

/** @} */

} // namespace sxipilib

#endif /* TOOL_SXIPI_SXIPILIB_H */

/* Revision Log
 $Log: sxipilib.h,v $
 Revision 1.26  2015/12/29 13:44:28  mdutka
 Changing handling of of CALDB files when the CALDB query is not performed

 Revision 1.25  2015/12/04 05:57:22  asargent
 Read the EVENTTHR keyword and check events against threshold

 Revision 1.24  2015/11/12 17:51:14  mdutka
 Updating sxipi to account for inverse echo effect

 Revision 1.23  2015/10/28 20:47:43  mdutka
 Commiting sxipi after bug fix

 Revision 1.22  2015/10/21 19:36:18  mdutka
 Checking in sxipi after adding updated CTI correction routine

 Revision 1.21  2015/08/19 15:52:46  mwitthoe
 sxipi: change TNULL from 4095 to 4096; create function to set NULL to reduce duplication; create constants for TLMIN, TLMAX, and TNULL for PHA, PI, and GRADE

 Revision 1.20  2015/08/19 13:54:56  asargent
 Fixed bug when indexing the hkdata array. Bug showed up on 32-bit Mac and caused GCC to segfault due to using a long long as an index

 Revision 1.19  2015/08/10 15:40:40  mwitthoe
 sxipi: add standard prologue, stamp parameters to log file; move non-standard functions from sxipi.cxx to sxipilib.h/cxx; general clean-up

 Revision 1.18  2015/06/08 20:11:37  mdutka
 adding handling time = null in event and hk file

 Revision 1.17  2015/06/05 20:20:13  mdutka
 adding hkvideoid par

 Revision 1.16  2015/06/04 19:04:49  mdutka
 fixed bug with data structs bit->ints

 Revision 1.15  2015/05/27 20:21:45  mdutka
 fixed bug when reading hkfile

 Revision 1.14  2015/05/22 13:13:34  mdutka
 Chaging flag bit order and implementing CALDB

 Revision 1.13  2015/03/18 14:43:26  mdutka
 sxi tool change see Issue #490

 Revision 1.12  2015/01/06 21:29:10  mwitthoe
 sxipi: change parameter hkhdu to hkext; see issue 472

 Revision 1.11  2014/12/30 21:41:54  mwitthoe
 sxipi: update parameters; see issue 472

 Revision 1.10  2014/12/11 21:27:08  asargent
 New data structure to hold HK information. Fixed CALDB time verification to correct check.

 Revision 1.9  2014/10/29 21:41:20  asargent
 Added boundary checking for each event and TNULL,TLMIN,TLMAX keyword writing.

 Revision 1.8  2014/09/30 19:47:25  asargent
 New extension and column name parameters for HK file. Moved HK router to doWork function for speed increase.

 Revision 1.7  2014/09/18 20:07:20  asargent
 Changed event file keyword from CISTART to CIFIRST. Made extension explicit in HK file.

 Revision 1.6  2014/09/08 19:35:10  asargent
 Updates to sxipi to include changes in split threshold parameters, more thorough checks in start columns.

 Revision 1.5  2014/07/17 14:21:46  asargent
 Changed instances of float to double for better precision between different architectures.

 Revision 1.4  2014/06/23 13:59:50  asargent
 Updated sxipi to include more debugging options for starting PHAS column. Also changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

 Revision 1.3  2014/04/01 15:04:54  asargent
 Updates to include observation time validation in load functions. New function to parse keyword for CALDB files.

 Revision 1.2  2014/02/10 21:03:45  asargent
 Updated descriptions of functions and included cmath for pow() use

 Revision 1.1  2014/02/10 20:16:32  asargent
 First version of sxipilib header file

*/
