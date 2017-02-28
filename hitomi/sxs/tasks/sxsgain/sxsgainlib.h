/// \file sxsgainlib.h
/// \brief Functions for sxsgain
/// \author Michael Witthoeft
/// \date $Date: 2016/10/13 16:04:09 $

/// \addtogroup tool_sxsgain
/// \section tool_sxsgain_sxsgainlib Supplementary functions for sxsgain
///

#ifndef TOOL_SXSGAIN_SXSGAINLIB_H
#define TOOL_SXSGAIN_SXSGAINLIB_H

#include "ahgen/ahversion.h"
AHVERSION(SXSGAIN_SXSGAINLIB,"$Id: sxsgainlib.h,v 1.13 2016/10/13 16:04:09 mwitthoe Exp $")


#include "ahgain/callines.h"
#include "ahgain/ahgain.h"
#include "ahfits/ahfits.h"
#include "ahsxs/ahsxs.h"
#include "ahsxs/engain.h"
#include "ahmath/ahmath.h"

#include <string>
#include <sstream>

/// \brief minimum number of bin points
const int MINPT=5;

/// \brief maximum number of bin points
const int MAXPT=5000;

/// \brief length of PROC_STATUS column in Astro-H FITS files
const int LENPROCSTATUS=32;

/// \brief Store a single GTI interval: start & stop
typedef std::pair<double, double> GTIInterval;

/// \brief Store a vector of GTI intervals
typedef std::vector<GTIInterval> GTIIntervals;

/// \brief Map of GTIIntervals per pixel
typedef std::map<int, GTIIntervals> GTIIntervalsMap;


/// \brief structure to hold parameter values
struct Par {
  Par(): tempidx(0), ntemp(0), numevent(0), minevent(0), gapdt(0.),
         grpoverlap(0.), startenergy(0.), stopenergy(0.), extraspread(0.),
         pxphaoffset(0.), broadening(0.), gridprofile(false), fitwidth(false),
         background(0), spangti(false), usemp(false), ckrisetime(false),
         calcerr(false), writeerrfunc(false), ckant(false), ckctrec(false),
         ckctel(false), extrap(false), avgwinrad(0.), minwidth0(0.), 
         maxitcycle(0), r2tol(0.), searchstepshift(0.), maxdshift(0.), 
         bisectolshift(0.), searchstepwidth(0.), maxdwidth(0.), 
         bisectolwidth(0.), minwidth(0.), nerrshift(0), nerrwidth(0), 
         shifterrfac(0.), widtherrfac(0.), gainitype(0), centerprof(false), 
         useevst(true) {};

  std::string infile;           // name of input file
  std::string outfile;          // name of output file
  std::string gainfile;         // name of CALDB gain file
  int tempidx;                  // temperature index to use from gain file (starting from 1)
  std::string gaincoeff;        // type of gain coefficients to use (H, M, L)
  std::string linefitfile;      // name of CALDB file with calibration line data
  std::string linetocorrect;    // name of calibration line to use; e.g. Mnka
  std::string itypecol;         // column containing event ITYPE
  int ntemp;                    // Number of temperatures from gain file to use in interpolation
  std::string calmethod;        // calibration type, e.g. cal-pix
  int numevent;                 // nominal number of events to group
  int minevent;                 // minimum nunber of events allowed in group
  std::string gtifile;          // name of gti file
  double gapdt;                 // largest allowed separation of two consecutive events in the same group
  double grpoverlap;            // percentage of points shared between two consecutive groups
  double startenergy;           // start of selection region in eV
  double stopenergy;            // end of selection region
  double extraspread;           // enlarge line spread in eV
  double pxphaoffset;           // amount to shift PHA bin values
  double broadening;            // Gaussian FWHM to convolve profile with
  bool gridprofile;             // true to exit tool after writing profile
  bool fitwidth;                // true to fit convolution width
  int background;               // type of background to fit - none, constant, or sloped (as enumerated value; see ahgain library)
  bool spangti;                 // ignore GTI in the data accumulation
  bool usemp;                   // true if including Mp events in fitting
  bool ckrisetime;              // true to skip events with RISE_TIME > 127
  bool calcerr;                 // true if calculating uncertainties on shift and width
  bool writeerrfunc;            // output likelihood functions used in shift/width uncertainty (if calcerr)
  bool ckant;                   // true to exclude antico events from groups
  bool ckctrec;                 // true to exclude recoil cross-talk events from groups
  bool ckctel;                  // true to exclude electrical cross-talk events from groups
  bool extrap;                  // allow extrapolation when determining temperature
  double avgwinrad;             // radius of interval used to update binned average (channel units)
  double minwidth0;             // smallest allowed initial value in width fitting (eV)
  int maxitcycle;               // maximum number of fitting iterations
  double r2tol;                 // convergence criterion for R^2
  double searchstepshift;       // step size when fitting shift (channel units)
  double maxdshift;             // largest allowed deviation from initial shift (channel units)
  double bisectolshift;         // tolerance of shift in bisection method (channel units)
  double searchstepwidth;       // step size when fitting width (channel units)
  double maxdwidth;             // largest allowed deviation from initial width (channel units)
  double bisectolwidth;         // tolerance of width in bisection method (channel units)
  double minwidth;              // smallest width to allow in fitting (channel units)
  int nerrshift;                // number of shifts in shift likelihood function
  int nerrwidth;                // number of widths in width likelihood function
  double shifterrfac;           // used to determine domain of shift likelihood function
  double widtherrfac;           // used to determine domain of width likelihood function

  // derived parameters
  std::string splitcolumn;      // column name used to separate the data  <<< This is forced to PIXEL in sxsgain
  int gainitype;                // which gain coefficients to use as an ITYPE [H=0, M=1 (primary), L=3 (primary)]
  bool centerprof;              // center profile in energy range if startenergy or stopenergy is specified
  bool useevst;                 // if true, use the energy vs temperature table to get temperature instead of PHA vs temperature table
};


/// \brief Create a new FITS file to be filled with calibration fitting results.
/// \param[in] filename name of new file
/// \param[in] splitcolumn name of column used to split event data; empty
///  string if not used
/// \param[in] telescop value of TELESCOP keyword; or empty string to skip
/// \param[in] instrume value of INSTRUME keyword; or empty string to skip
/// \param[in] detnam value of DETNAM keyword; or empty string to skip
/// \param[in] gridprofile if yes, only create Grid_profile extension
/// \param[in] calmethod Calibration method: Cal-pix, MXS, or Fe55
/// \param[in] tempidx Input temperature index for selecting gain (starting from 1)
/// \param[in] gaintemp Temperature corresponding to tempidx
/// \param[in] energref which line to use for calibration (e.g. Cuka)
/// \param[in] chstart start of PHA binning mesh
/// \param[in] chlast end of PHA binning mesh
/// \param[in] chwidth PHA bin width
/// \param[in] picolumn name of energy column used in fitting (e.g. PHA)
/// \param[in] maxnmesh number of bin mesh points
/// \param[in] calcerr if yes, create uncertainty columns
/// \param[in] writeerrfunc if yes (and calcerr=yes), create likelihood and Chi^2 array columns
/// \param[in] nshift size of likelihood array for shift (used if calcerr/writeerrfunc=yes)
/// \param[in] nwidth size of likelihood array for width (used if calcerr/writeerrfunc=yes)
/// \param[out] fpout ahfits FilePtr for output file
void createGainOutput(const std::string& filename, const std::string& splitcolumn,
                      const std::string& telescop, const std::string& instrume,
                      const std::string& detnam, bool gridprofile, 
                      const std::string& calmethod, int tempidx, double gaintemp,
                      const std::string& energref, double chfirst, double chlast,
                      double chwidth, const std::string& picolumn, int maxnmesh,
                      bool calcerr, bool writeerrfunc, int nshift, int nwidth, 
                      ahfits::FilePtr& fpout);


/// \brief Obtain the temperature corresponding to the given PHA
/// \param[in] avgpha input PHA value from fit
/// \param[in] pixel pixel of fitted group
/// \param[in] itype value from the ITYPE column in the event file
/// \param[in] useevst use energy vs temperature table to interpolate
///  temperature, otherwise PHA vs temperature is used
/// \param[in] gaindat gain coefficients to convert PHA to energy
/// \param[in] profavgpha average PHA of profile
/// (on set of coefficients for each temperature and each pixel)
/// \param[in] linedat calibration line data (using average energy of feature
/// \param[in] parextrap true to allow extrapolation in temperature calculation
/// \return temperature corresponding to the given PHA
double computeTemperature(double avgpha, int pixel, int itype, bool useevst,
                          ahsxs::engain::AllEnergyGainData & gaindat, 
                          double profavgpha, bool parextrap);

/// \brief Set the columns as NULL for a non-fitted profile
/// \param[in] profdat   ProfDat item to set to copy non-fitted profile from
/// \param[out] results  FitResults item to copy non-fitted profile to
/// \param[out] nullcol  Flag to set columns to null
void setNullRow(const ahgain::CalProfile & profdat, ahgain::FitResults & results, 
                char * nullcol);

#endif /* TOOL_SXSGAIN_SXSGAINLIB_H */

/* Revision Log
 $Log: sxsgainlib.h,v $
 Revision 1.13  2016/10/13 16:04:09  mwitthoe
 sxsgain: use tabulated profile average instead of theoretical average when determining the temperature from the fitted shift

 Revision 1.12  2016/08/10 16:29:57  mwitthoe
 sxsgain: now support pixel-dependent GTI files in two formats

 Revision 1.11  2016/06/01 21:36:02  mwitthoe
 sxsgain: add new parameter, pxphaoffset, to shift the PHA bin values

 Revision 1.10  2016/05/10 16:59:08  mwitthoe
 sxsgain: for the input GTI file: 1) if DETNAM not defined in an extension, treat it as a general GTI (pixel-independent); 2) if extended syntax is used, only read that single extension which can be pixel-dependent or not

 Revision 1.9  2015/11/19 21:14:04  mwitthoe
 sxsgain: now tool will use energy vs temperature table by default when computing the group temperature, but the PHA vs temperature table can be used instead when tempidx < 0

 Revision 1.8  2015/11/17 16:30:59  mwitthoe
 sxsgain: 1) remove centerprof parameter (behavior now enabled when startenergy or stopenergy is specified); 2) make PHA bins centered on half-integers; 3) use PHA vs temperature table to calculate temperature (make ntemp parameter negative to use energy vs temperature table instead); 4) write same keywords to 1st and 2nd extensions

 Revision 1.7  2015/10/27 19:11:23  mwitthoe
 sxsgain: move resetUncertainties() function to ahgain library

 Revision 1.6  2015/10/27 18:07:14  mwitthoe
 sxsgain: change names of parameters: nlikeshift, nlikewidth, likeshiftfac, likewidthfac to nerrshift, nerrwidth, shifterrfac, widtherrfac since these parameters are used for both the likelihood and Chi^2 uncertainty calculations

 Revision 1.5  2015/10/27 17:26:11  mwitthoe
 sxsgain: change name of writelike parameter to writeerrfunc; this parameter now causes the tool to write both the likelihood and Chi^2 functions

 Revision 1.4  2015/10/27 14:33:06  mwitthoe
 sxsgain: add Chi^2 uncertainties

 Revision 1.3  2015/10/16 20:33:04  mwitthoe
 sxsgain: 1) set default value of ckrisetime to yes; 2) increase MAXPT to 5000; 3) set default value of tempidx to 2; 4) new parameter extrap to allow for extrapolating the temperature

 Revision 1.2  2015/10/13 20:01:58  mwitthoe
 sxsgain: 1) option to skip events with RISE_TIME > 127; 2) allow calculation of uncertainties on fitted shift and width using the likelihood method

 Revision 1.1  2015/10/01 21:14:23  mwitthoe
 sxsgain: now can accept either a GTI file with a single, general GTI extension or a GTI file with 36, pixel-specific extensions


*/

 
