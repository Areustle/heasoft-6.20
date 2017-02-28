/// \file sxspha2pilib.h
/// \brief Functions for sxspi
/// \author Kristin Rutkowski
/// \date $Date: 2016/11/03 15:51:38 $

/// \addtogroup tool_sxspha2pi
/// \section tool_sxspha2pi_sxspha2pilib Supplementary functions for sxspha2pi
///
/// 

#ifndef TOOL_SXSPHA2PI_SXSPHA2PILIB_H
#define TOOL_SXSPHA2PI_SXSPHA2PILIB_H

#include "ahgen/ahversion.h"
AHVERSION(SXSPHA2PI_SXSPHA2PILIB,"$Id: sxspha2pilib.h,v 1.10 2016/11/03 15:51:38 mwitthoe Exp $")

#include "ahsxs/engain.h"

#include <set>


namespace ahsxs {

// ****************************************************************************

/// \brief structure to hold parameter values
struct Par {
    Par(): m_calcupi(true), m_calcpi(true), m_tempidx(0), m_pxphaoffset(0.),
           m_scaleepi(false), m_extended(false), m_binwidth(0.), m_offset(0.),
           m_gapdt(0.), m_ntemp(0), m_randomize(false), m_seed(0) {};

  std::string m_infile;           ///< name of input file
  std::string m_outfile;          ///< name of output file
  bool m_calcupi;                 ///< calculate UPI column
  bool m_calcpi;                  ///< calculate PI column
  std::string m_driftfile;        ///< Input drift correction file (or CALDB)
  std::string m_gainfile;         ///< Input gain file (or CALDB)
  std::string m_scalefile;        ///< Input EPI scale correction file (or CALDB)
  int m_tempidx;                  ///< Temperature index of gain coefficients to use (or -1)
  double m_pxphaoffset;           ///< Average offset to add to PHA before applying gain
  std::string m_secphacol;        ///< Input PHA column to use in secondary correction
  std::string m_addepicol;        ///< Output energy column with secondary correction
  std::string m_method;           ///< Correction method (FIT or AVERAGE)
  bool m_scaleepi;                ///< Scale EPI values using scalefile (yes/[no])
  std::string m_scalegrade;       ///< List of grades to apply scale factors
  std::string m_itypecol;         ///< Column containing event grade
  bool m_extended;                ///< Operate on extended energy region
  double m_binwidth;              ///< PI bin width when using the extended energy range (eV)
  double m_offset;                ///< energy offset of first PI channel when using the extended energy range (eV)
  int m_tlmax;                    ///< Maximum PI channel when using the extended energy range
  double m_gapdt;                 ///< Time [s] between events to define a gap (or <0)
  int m_ntemp;                    ///< Number of temperatures from gain file to use in interpolation
  bool m_writetemp;               ///< True to output temperature for each event
  bool m_extrap;                  ///< True to allow extrapolation when determining drift temperature
  bool m_randomize;               ///< True if adding a random number to PHA before applying the gain
  int m_seed;                     ///< Random number generator seed (0=use system time)

  // derived parameters
  std::set<int> m_gradeset;       ///< Set of integer grades to apply EPI scale factor to
  std::string m_epi2colname;      ///< name of EPI2 column (derived from addepicol)

};

// ****************************************************************************

/// \brief store temperature data for a single pixel (from sxsgain)
struct TempDataOnePixel {
  
  // initialize values
  TempDataOnePixel(): m_size(0) { };
  
  int m_size;                        ///< number of temperatures
  std::vector<double> m_time;        ///< vector of times where temperature is measured
  std::vector<double> m_temp;        ///< vector of temperatures
  
};

// ****************************************************************************

/// \brief store temperature data for all pixels (from sxsgain)
struct TempDataAllPixels {
  
  // initialize values
  TempDataAllPixels(): m_tempidx(0) { 
    m_tempdatpix.resize(ahsxs::NPIXEL);
  };
  
  std::string m_caltype;                        ///< calibration type used in the temperature calculations
  long m_tempidx;                               ///< temperature index from gain file used in constructing the profile in sxsgain
  std::set<long> m_pixels;                      ///< set of pixels found in drift file (output from sxsgain)
  std::vector<TempDataOnePixel> m_tempdatpix;   ///< temperature vs time data for each pixel read from sxsgain output
          
};

// ****************************************************************************

/// \brief store EPI scale factors per pixel
struct EPIScaleFactors {
  
  // initialize values
  EPIScaleFactors() { 
    for (int i=0; i < ahsxs::NPIXEL; i++) m_hpscale[i]=0.;
    for (int i=0; i < ahsxs::NPIXEL; i++) m_mscale[i]=0.;
    for (int i=0; i < ahsxs::NPIXEL; i++) m_lscale[i]=0.;
  };

  bool m_haveallgrades;               // true if CALDB file has all three scale columns
  double m_hpscale[ahsxs::NPIXEL];    // scale factors for Hp grades
  double m_mscale[ahsxs::NPIXEL];     // scale factors for Mp and Ms grades
  double m_lscale[ahsxs::NPIXEL];     // scale factors for Lp and Ls grades
          
};

// ****************************************************************************

/// \brief load temperature vs time data from sxsgain output
/// \param[in] filename name of sxsgain output file
/// \param[in] method select which temperatures to read from filename (FIT or AVERAGE)
/// \param[out] tempdat structure to hold data
void loadTempGainData(const std::string & filename, 
                      const std::string & method,
                      ahsxs::TempDataAllPixels & tempdat);

// ****************************************************************************

/// \brief load EPI scale factors from FITS file
/// \param[in] filename name of input file with two columns: PIXEL and SCALE
/// \param[out] scaledat structure to hold data
void loadEPIScaleFactors(const std::string & filename, 
                         ahsxs::EPIScaleFactors & scaledat);

// ****************************************************************************

/// \brief Set all output column values to NULL
/// \param[out] l_upinull UPI or UPIE column
/// \param[out] l_epinull EPI or EPIE column
/// \param[out] l_pinull PI or PIE column
/// \param[out] l_epi2null EPI2 column
/// \param[out] l_tempnull TEMP column
///
/// There are multiple places in algorithm where all output columns should be 
/// set to NULL.  This helper function is called each time.
void setValuesToNull(char & l_upinull, 
                     char & l_epinull, 
                     char & l_pinull, 
                     char & l_epi2null,
                     char & l_tempnull);

// ****************************************************************************

/// \brief Compute EPI from PHA value
/// \param[in] par parameter values
/// \param[in] pha input PHA value
/// \param[in] pixel event pixel number
/// \param[in] itype event ITYPE
/// \param[in] temp temperature for interpolating gain
/// \param[in] temp_itype ITYPE to use in gain look-up (not always same as
///            event ITYPE)
/// \param[in] gaindat structure containing energy gain coefficients for each
///            pixel and temperature
/// \param[in] scaledat structure containing EPI scale factors
/// \param[out] epi output EPI value
void computeEPI(const Par& par, double pha, int pixel, int itype, double temp,
                int temp_itype, ahsxs::engain::AllEnergyGainData& gaindat, 
                const ahsxs::EPIScaleFactors& scaledat,
                double& epi);

// ****************************************************************************

} // end namespace ahsxs
        
#endif /* TOOL_SXSPHA2PI_SXSPHA2PILIB_H */

/* Revision Log
 $Log: sxspha2pilib.h,v $
 Revision 1.10  2016/11/03 15:51:38  mwitthoe
 sxspha2pi: fix bug affecting only extended energy mode where the tool failed if the EPI2E column already existed in the event file

 Revision 1.9  2016/06/02 15:13:08  mwitthoe
 sxspha2pi: 1) add parameter, pxphaoffset, which is added to each event PHA along with a random number between -0.5 and +0.5 before the gain is applied; 2) now support a 2nd format for the scalefile which has columns for all three grades instead of just Hp

 Revision 1.8  2016/04/20 21:31:02  mwitthoe
 sxspha2pi: 1) add option to compute EPI2 using PHA2 (secondary correction); 2) if driftfile is cal-pix, add option to scale EPI values by pixel-dependent factors stored in a CALDB file

 Revision 1.7  2015/11/20 17:58:03  mwitthoe
 sxspha2pi: write PIEOFFST and PIEWIDTH keywords when in extended energy move; add extrap parameter to allow extrapolation when determining drift temperature

 Revision 1.6  2015/11/13 21:14:34  mwitthoe
 sxspha2pi: 1) add parameter, writetemp, to output TEMP column with drift temperature for each event; 2) fix drift file lookup - old version was retrieving the first row after the event time instead of the row immediately before the event; 3) skip reading the INDEX and GROUPS columns if the allprimary parameter is yes (these columns are only to be used in the PI correction for secondary events

 Revision 1.5  2015/09/17 18:32:16  mwitthoe
 sxspha2pi: 1) add parameters, calcupi and calcpi, specifying which calculations to perform; 2) add parameter tempidx giving the gain temperature index to use in the UPI calculation (or -1 to use the index in the drift file); 3) add itypecol parameter giving the ITYPE column to use with the event grade information; 4) delete output file if there is an early error

 Revision 1.4  2015/08/19 21:47:26  mwitthoe
 sxspha2pi: fix Doxygen labels

 Revision 1.3  2015/08/13 15:29:10  mwitthoe
 sxspha2pi: move all structs and non-standard functions from sxspha2pi to sxspha2pilib, add standard prologue, implement extended energy mode, general clean-up; see issues 532 & 534

 Revision 1.2  2015/04/29 22:56:22  klrutkow
 edited comments; added line to test loading gain data (commented out)

 Revision 1.1  2015/04/21 19:24:26  klrutkow
 new tool sxspha2pi


*/

 
