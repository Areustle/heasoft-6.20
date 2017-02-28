/// \file ahgain.h
/// \brief Calculate gain corrections from calibration features.
/// \author Mike Witthoeft
/// \date $Date: 2016/08/25 17:32:00 $
 
/// \addtogroup mod_ahgain
/// \section ahgain_ahgain Calculate gain corrections - ahgain
///
/// This library provides functions to calculate gain corrections by fitting
/// a theoretical profile to a single calibration feature.  The library uses
/// a least-squares approach to fit the energy shift, scale factor, background,
/// and Gaussian convolution width.
///

#ifndef AHGAIN_AHGAIN_H
#define AHGAIN_AHGAIN_H

#include "ahgen/ahversion.h"
AHVERSION(AHGAIN_AHGAIN,"$Id")

#include "ahgain/callines.h"
#include "ahfits/ahfits.h"

/// \brief library namespace
/// \ingroup mod_ahgain
namespace ahgain {

/** \addtogroup mod_ahgain
 *  @{
 */

// ****************************************************************************

/// \brief Enumeration giving allowed background types in shift fitting.
enum BackgroundType {
  e_NOBACKGROUND,           ///< no background
  e_CONSTBACKGROUND,        ///< constant background
  e_SLOPEBACKGROUND         ///< background with constant (linear) slope
};

// ****************************************************************************

/// \brief Structure containing theoretical calibration profile.
struct CalProfile {
  CalProfile(): m_nmesh(0), m_mesh(0), m_prof(0), m_dprof(0), m_avg_prof(0.),
                m_var_prof(0.), m_offset(0.) {};
  ~CalProfile() {
    if (0 != m_mesh) delete [] m_mesh;
    if (0 != m_prof) delete [] m_prof;
    if (0 != m_dprof) delete [] m_dprof;
  };
  int m_nmesh;               ///< number of mesh points
  double* m_mesh;            ///< energy mesh array (channel units)
  double* m_prof;            ///< array of profile values
  double* m_dprof;           ///< array of profile derivative values
  double m_avg_prof;         ///< average energy of profile
  double m_var_prof;         ///< variance of profile
  double m_offset;           ///< offset between original profile position and position on mesh (when centerprof=yes)
};

// ****************************************************************************

/// \brief single row data gain fitting
struct EventData {
  EventData(): m_time(0.), m_timenull(0), m_split(0), m_pi(0.), m_pinull(0) {};

  double m_time;                         ///< time when event occurs
  char m_timenull;                       ///< set to 1 if TIME is NULL
  int m_split;                           ///< value of splitcolumn
  double m_pi;                           ///< column to bin (e.g. PI, PHA)
  char m_pinull;                         ///< set to 1 if PI is NULL
};

// ****************************************************************************

/// \brief vector of EventRow instances; used to store group of points to 
///  bin and fit
typedef std::vector<EventData> EventDataVec;

// ****************************************************************************

/// \brief map of integers to EventRowVec instances; used to store group of
///  events for different values of splitcolumn
typedef std::map<int, EventDataVec> EventVecMap;


// ****************************************************************************

/// \brief Structure containing output of calibration line fitting
struct FitResults {

  FitResults(): m_nevent(0), m_splitval(0), m_fit_shift(0.), m_fit_width(0.), 
                m_fit_scale(0.), m_fit_bgrnd(0.), m_fit_slope(0.), m_chisq(0.),
                m_avgunbin(0.), m_avgbin(0.), m_avgfit(0.), m_nmesh(0), 
                m_mesh(0), m_bindat(0), m_fitdat(0), m_cor_fit(0.), 
                m_cor_ave(0.), m_time(0.), m_telapse(0.), m_exposure(0.),
                m_peakshlike(0.), m_sigshlike(0.), m_peakshchi2(0.),
                m_sig1shchi2(0.), m_sig2shchi2(0.), m_sigshchi2(0.),
                m_nshift(0), m_shifts(0), m_shlike(0), m_shchi2(0),
                m_peakwdlike(0.), m_sigwdlike(0.), m_peakwdchi2(0.),
                m_sig1wdchi2(0.), m_sig2wdchi2(0.), m_sigwdchi2(0.),
                m_nwidth(0), m_widths(0), m_wdlike(0), m_wdchi2(0) {};

  ~FitResults() {
    m_nmesh=0;
    if (0 != m_mesh) delete [] m_mesh;
    if (0 != m_bindat) delete [] m_bindat;
    if (0 != m_fitdat) delete [] m_fitdat;
    if (0 != m_shifts) delete [] m_shifts;
    if (0 != m_shlike) delete [] m_shlike;
    if (0 != m_shchi2) delete [] m_shchi2;
    if (0 != m_widths) delete [] m_widths;
    if (0 != m_wdlike) delete [] m_wdlike;
    if (0 != m_wdchi2) delete [] m_wdchi2;
  };

  int m_nevent;          //< number of input events
  int m_splitval;        //< value of split column for input event group

  double m_fit_shift;    //< fitted shift
  double m_fit_width;    //< fitted width
  double m_fit_scale;    //< fitted scale factor
  double m_fit_bgrnd;    //< fitted background constant
  double m_fit_slope;    //< fitted background slope
  double m_chisq;        //< Chi^2 of fit

  double m_avgunbin;     //< unbinned average energy
  double m_avgbin;       //< binned average energy
  double m_avgfit;       //< average energy from fit

  int m_nmesh;           //< number of energy mesh points
  double* m_mesh;        //< energy mesh array (channel units)
  double* m_bindat;      //< array of binned events
  double* m_fitdat;      //< array of profile values with fitted parameters applied

  // energy corrections to shift binned peak to calibration positions
  double m_cor_fit;      //< correction based on fit
  double m_cor_ave;      //< correction based on event average

  // store average time of events and time span
  double m_time;         //< center of time interval of events
  double m_telapse;      //< time difference between first and last events
  double m_exposure;     //< exposure time of results

  // shift uncertainties
  double m_peakshlike;   //< shift at peak of likelihood function
  double m_sigshlike;    //< sigma of shift likelihood function
  double m_peakshchi2;   //< shift at peak Chi^2 (minimum value)
  double m_sig1shchi2;   //< sigma of shift Chi^2 function to lower values
  double m_sig2shchi2;   //< sigma of shift Chi^2 function to higher values
  double m_sigshchi2;    //< sigma of shift Chi^2 function (average of above 2)
  int m_nshift;          //< number of shift values for computing likelihood/Chi^2
  double* m_shifts;      //< array of shift values for likelihood/Chi^2 functions
  double* m_shlike;      //< array of likelihood values over shift
  double* m_shchi2;      //< array of Chi^2 values over shift

  // width uncertainties
  double m_peakwdlike;   //< width at peak of likelihood function
  double m_sigwdlike;    //< sigma of width likelihood function
  double m_peakwdchi2;   //< width at peak of Chi^2 (minimum value)
  double m_sig1wdchi2;   //< sigma of width Chi^2 function to lower values
  double m_sig2wdchi2;   //< sigma of width Chi^2 function to higher values
  double m_sigwdchi2;    //< sigma of width Chi^2 function (average of above 2)
  int m_nwidth;          //< number of width values for computing likelihood/Chi^2
  double* m_widths;      //< array of width values for likelihood/Chi^2 functions
  double* m_wdlike;      //< array of likelihood values over width
  double* m_wdchi2;      //< array of Chi^2 values over width

};

// ****************************************************************************

/// \brief Construct calibration profile on energy mesh.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] nmesh number of mesh points
/// \param[in] evchannel number of eV per unit of mesh representation
/// \param[in] callines calibration line data from CALDB file
/// \param[in] centerprof if true, then center profile in mesh range
/// \param[in] sigma width of Gaussian as standard deviation used to broaden
///  the profile (in units of mesh)
/// \param[out] profdat constructed profile on mesh including its deriviative,
///   average, and variance
///
/// If the mesh spacing is coarse compared with the natural lines widths
/// in callines, then treat each line as a delta-function and use Gaussian
/// profiles (width of sigma) to construct the profile. 
void constructCalibrationProfile(double* mesh, int nmesh, double evchannel,
                                 const callines::CalLines& callines, 
                                 bool centerprof, double sigma,
                                 CalProfile& profdat);


/// \brief Convolve profile and its derivative with a Gaussian.
/// \param[in] profdatin input profile (unconvolved)
/// \param[in] sigma width of Gaussian as standard deviation
/// \param[out] profdatout output profile (convolved)
void convolveCalibrationProfile(const CalProfile& profdatin, double sigma,
                                CalProfile& profdatout);


/// \brief Calculate the average energy and variance of profile.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[out] avg average bin position
/// \param[out] var variance of bin positions
void calcAverageAndVariance(double* mesh, double* prof, int nmesh,
                            double& avg, double& var);


/// \brief Compute an energy in eV for the interval radius to use when updating
///  the binned event average which is used as an initial guess of the energy
///  shift.
/// \param[in] callines Calibration line data from CALDB file
/// \param[in] broadening FWHM of Gaussian used to convolve profile
/// \param[in] nvoigt number of Voigt widths to use as radius
/// \return interval radius
///
/// Define the radius of the window used to calculate updated binned average
/// when fitting the theoretical profile to a group of event data; the updated
/// average is the initial condition of the fitting method; the window is
/// defined by [bavg1-avgwinrad : bavg1+avgwinrad] where bavg1 is the initial
/// binned average of all points in the group.  Considering that we broaden
/// the theoretical profile (consisting of Lorentzians) with a Gaussian using
/// the broadening parameter, each line in the profile will have a Voigt
/// profile.  The value of avgwinrad will be chosen to that the individual 
/// line furtherst from the profile average will be covered to a number of
/// Voigt widths.  The expression for the Voigt width is an approximation
/// given here: http://en.wikipedia.org/wiki/Voigt_profile.  The original 
/// source of this expression is Journal of Quantitative Spectroscopy and 
/// Radiative Transfer 17 (2): 233–236 (1977).
double computeAverageWindowRadius(const callines::CalLines& callines,
                                  double broadening, int nvoigt);


/// \brief Set mesh of fit results structure and allocate arrays.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] nmesh number of mesh points
/// \param[in] nshift number of shifts in uncertainty calculation
/// \param[in] nwidth number of widths in uncertainty calculation
/// \param[out] results FitResults item to initialize
void initializeFitResults(double* mesh, int nmesh, int nshift, int nwidth,
                          FitResults& results);


/// \brief Shift data along mesh by given amount.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] shift amount to shift profile along the mesh
/// \param[out] profout profile with parameters applied
///
/// The function works by interpolating each point onto its new position on
/// the mesh after shifting.  The output array, out, must be properly sized
/// before this function is called.  It is assumed that the mesh has fixed
/// spacings.  Function is assumed to be constant beyond mesh end points.
void shiftData(double* mesh, double* prof, int nmesh, double shift,
                  double* profout);


/// \brief Apply fitted shift parameters to profile.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] shift amount to shift profile along the mesh
/// \param[in] scale profile values are scaled by this factor
/// \param[in] bgrnd this value is added to each profile value
/// \param[in] slope the product of this value and the i'th mesh point is
///            added to the i'th profile value
/// \param[out] profout profile with parameters applied
///
/// The output array, out, must be properly sized before this function is
/// called.  It is assumed that the mesh has fixed spacings.  Function is 
/// assumed to be constant beyond mesh end points.
void applyShiftParameters(double* mesh, double* prof, int nmesh, double shift,
                          double scale, double bgrnd, double slope, double* profout);



/// \brief Calculate least-squares functional value for fitting an energy
///  shift.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of energy shifted profile values on mesh (the 
///  output functional value for for the shift applied to this profile)
/// \param[in] dprof array with derivatives of the shifted profile
/// \param[in] nmesh number of mesh points
/// \param[in] backtype type of background fitting (BackgroundType enum)
/// \param[out] z computed value of functional
/// \param[out] scale fitted scale factor
/// \param[out] bgrnd fitted background value
/// \param[out] slope fitted background slope
void calcFunctionalShift(double* mesh, double* bindat, double* prof,
                         double* dprof, int nmesh, int backtype, double& z,
                         double& scale, double& bgrnd, double& slope);


/// \brief Calculate least-squares functional value for width fitting.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] width width to apply before computing functional value
/// \param[out] z output value of functional
/// \param[out] scale output fitted scale factor
/// \param[out] profout output profile with width and scale factor applied
void calcFunctionalWidth(double* mesh, double* bindat, double* prof, int nmesh,
                         double width, double& z, double& scale, double* profout);


/// \brief Seek the best-fit shift in one direction from estimate.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of profile values on mesh
/// \param[in] dprof array of profile derivative values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] backtype type of background fitting (BackgroundType enum)
/// \param[in] shift0 shift to start search with
/// \param[in] searchstepsize direction and magnitude of search step size
/// \param[in] maxdshift stop search if seeking this far past shift0
/// \param[in] bisectol convergence of shift where bisection method stops
/// \param[out] shift fitted shift
/// \param[out] scale fitted scale factor
/// \param[out] bgrnd fitted background constant
/// \param[out] slope fitted background slope
/// \param[out] r2 coefficient of determination of final fit
/// \param[out] profout profile with fitted parameters applied
/// \param[out] status if non-zero, then no solution found
///
/// This routine looks for an energy shift, scale factor, and background 
/// parameters which yield the best agreement between a binned data set and
/// theoretical profile.  The routine uses a least-squares method where the
/// zero of a functional is sought.  Starting from the initial shift (shift0),
/// the functional is calculated in steps in the direction and size of
/// searchstepsize until a change of sign is found.  Once bracketed, the
/// bisection method is used to get the best-fit shift to the desired tolerance
/// given by bisectol.  The remaining parameters are analytically computed
/// using the fitted shift.
void findZeroShift(double* mesh, double* bindat, double* prof, double* dprof,
                   int nmesh, int backtype, double shift0, double searchstepsize, 
                   double maxdshift, double bisectol, double& shift, 
                   double& scale, double& bgrnd, double& slope, double& r2,
                   double* profout, int& status);


/// \brief Seek the best-fit convolution width in one direction from estimate.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] width0 width to start search with
/// \param[in] searchstepsize direction and magnitude of search step size
/// \param[in] minwidth stop search for widths below this value
/// \param[in] maxdwidth stop search if seeking this far past width0
/// \param[in] bisectol convergence of width where bisection method stops
/// \param[out] width fitted width
/// \param[out] scale fitted scale factor
/// \param[out] r2 coefficient of determination of final fit
/// \param[out] profout profile with width and scale factor applied
/// \param[out] status if non-zero, then no solution found
///
/// This routine looks for a convolution width and scale factor which yield the
/// best agreement between a binned data set and theoretical profile.  The
/// routine uses a least-squares method where the zero of a functional is 
/// sought.  Starting from the initial width (width0), the functional is
/// calculated in steps in the direction and size of searchstepsize until a
/// change of sign is found.  Once bracketed, the bisection method is used to
/// get the best-fit width to the desired tolerance given by bisectol.  The
/// scale factor is then computed analytically using the fitted width.
void findZeroWidth(double* mesh, double* bindat, double* prof, int nmesh,
                   double width0, double searchstepsize, double minwidth,
                   double maxdwidth, double bisectol, double& width, 
                   double& scale, double& r2, double* profout, int& status);


/// \brief Fit energy shift between binned data set and profile.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of profile values on mesh
/// \param[in] dprof array of profile derivative values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] backtype type of background fitting (BackgroundType enum)
/// \param[in] shift0 shift to start search with
/// \param[in] searchstepsize magnitude of search step size
/// \param[in] maxdshift stop search if seeking this far past shift0
/// \param[in] bisectol convergence of shift where bisection method stops
/// \param[out] shift fitted shift
/// \param[out] scale fitted scale factor
/// \param[out] bgrnd fitted background constant
/// \param[out] slope fitted background slope
/// \param[out] r2 coefficient of determination of final fit
/// \param[out] profout profile with shift, scale factor, and background applied
/// \param[out] status if non-zero, then no solution found
///
/// This routine will search at smaller and larger shifts from shift0 to find
/// at most two solutions.  The solution with the best R^2 value will be
/// returned as the final fit.
void fitShift(double* mesh, double* bindat, double* prof, double* dprof, 
              int nmesh, int backtype, double shift0, double searchstepsize,
              double maxdshift, double bisectol, double& shift, double& scale,
              double& bgrnd, double& slope, double& r2, double* profout, int& status);


/// \brief Fit Gaussian convolution width between binned data set and profile.
/// \param[in] mesh array of mesh points representing the center of each bin
/// \param[in] bindat array of binned events on represented on mesh
/// \param[in] prof array of profile values on mesh
/// \param[in] nmesh number of mesh points
/// \param[in] width0 width to start search with
/// \param[in] searchstepsize magnitude of search step size
/// \param[in] minwidth do not search below this width
/// \param[in] maxdwidth stop search if seeking this far past width0
/// \param[in] bisectol convergence of width where bisection method stops
/// \param[out] width fitted width
/// \param[out] scale fitted scale factor
/// \param[out] r2 coefficient of determination of final fit
/// \param[out] profout profile with width and scale factor applied
/// \param[out] status if non-zero, then no solution found
///
/// This routine will search at smaller and larger widths from width0 to find
/// at most two solutions.  The solution with the best R^2 value will be
/// returned as the final fit.  Since the fitting functional has no value for
/// width=0, a minimum search width must be provided.
void fitWidth(double* mesh, double* bindat, double* prof, int nmesh, 
              double width0, double searchstepsize, double minwidth,
              double maxdwidth, double bisectol, double& width, double& scale,
              double& r2, double* profout, int& status);


/// \brief Create a new FITS file to be filled with calibration fitting results.
/// \param[in] filename name of new file
/// \param[in] splitcolumn name of column used to split event data; empty
///  string if not used
/// \param[in] telescop value of TELESCOP keyword; or empty string to skip
/// \param[in] instrume value of INSTRUME keyword; or empty string to skip
/// \param[in] detnam value of DETNAM keyword; or empty string to skip
/// \param[in] gridprofile if yes, only create Grid_profile extension
/// \param[in] energref which line to use for calibration (e.g. Cuka)
/// \param[in] chstart start of PHA binning mesh
/// \param[in] chlast end of PHA binning mesh
/// \param[in] chwidth PHA bin width
/// \param[in] picolumn name of energy column used in fitting (e.g. PHA)
/// \param[in] maxnmesh number of bin mesh points
/// \param[in] calcerr if yes, create uncertainty columns
/// \param[in] writeerrfunc if yes (and calcerr=yes), create likelihood array columns
/// \param[in] nshift size of likelihood array for shift (used if calcerr/writelike=yes)
/// \param[in] nwidth size of likelihood array for width (used if calcerr/writelike=yes)
/// \param[out] fpout ahfits FilePtr for output file
void createGainOutput(const std::string& filename, const std::string& splitcolumn,
                      const std::string& telescop, const std::string& instrume,
                      const std::string& detnam, bool gridprofile, 
                      const std::string& energref, double chfirst, double chlast,
                      double chwidth, const std::string& picolumn, int maxnmesh,
                      bool calcerr, bool writeerrfunc, int nshift, int nwidth, 
                      ahfits::FilePtr& fpout);

/// \brief Bin a collection of events and fit the following parameters to
///  the calibration profile: energy shift, convolution width, scale factor,
///  background constant, and background slope.
/// \param[in] events vector of event row data
/// \param[in] nevents number of events to bin from events vector
/// \param[in] profdat constructed profile on mesh including its deriviative,
///   average, and variance
/// \param[in] profwidth width of Gaussian (in channel units, not eV!) used to
///  convolve the theoretical profile prior to fitting; this value is combined 
///  with the fitted width to obtain the final convolution width
/// \param[in] fitwidth true if fitting width in addition to energy shift
/// \param[in] backtype type of background fitting (enumerated value)
/// \param[in] avgwinrad energy radius of box to use when updating binned
///  event average
/// \param[in] minwidth0 smallest allowed initial guess of the width
/// \param[in] maxitcycle maximum number of shift/width fitting iterations
/// \param[in] r2tol stop fitting procedure when R^2 of fit converges within
///  this amount (only relevant when fitwidth=true)
/// \param[in] searchstepshift magnitude of search step size for shift fitting
/// \param[in] maxdshift stop shift search if seeking this far past shift0
/// \param[in] bisectolshift convergence of shift where bisection method stops
/// \param[in] searchstepwidth magnitude of search step size for width fitting
/// \param[in] minwidth do not search below this width
/// \param[in] maxdwidth stop width search if seeking this far past width0
/// \param[in] bisectolwidth convergence of width where bisection method stops
/// \param[out] results structure containing data for output file
/// \param[out] status non-zero if the fit was unsuccessful
void fitEvents(const EventDataVec& events, int nevents, const CalProfile& profdat,
               double profwidth, bool fitwidth, int backtype, double avgwinrad,
               double minwidth0, int maxitcycle, double r2tol, 
               double searchstepshift, double maxdshift, double bisectolshift, 
               double searchstepwidth, double minwidth, double maxdwidth, 
               double bisectolwidth, FitResults& results, int& status);

/// \brief Reset uncertainty values
/// \param[out] results FitResults structure after successful fitting
void resetUncertainties(FitResults& results);

/// \brief Construct array of shift values in the results structure for 
///  uncertainty calculations.
/// \param[in,out] results FitResults structure after successful fitting
/// \param[in] natwidth maximum natural width of profile feature (single line)
/// \param[in] rfactor used to determine range of shifts for likelihood 
/// \return true if construction was successful
///
/// This function constructs an array of shift values to use in the likelihood
/// and Chi^2 uncertainty routines.  The range of shift values is determined
/// from the the fitted and natural widths of the a set of results.  If both
/// of these widths are zero, then this routine will fail.  The mininum and
/// maximum values of the shift array are:
///
///  smin = -rfactor * fwidth
///  smax = +rfactor * fwidth
///
/// where
///
///  fwidth = sqrt( natwidth**2 + fitwidth**2 )
///
/// Note: this function expects the output shift array to be allocated to size
/// nshift and initialized to zero.
bool constructShiftArray(FitResults& results, double natwidth, double rfactor);

/// \brief Compute the likelihood function for energy shift of the fitted
///  profile on the binned data set.  Compute the peak and variance of the
///  likelihood function.
/// \param[in,out] results FitResults structure after successful fitting
///
/// The following elements of results are used: mesh, bindat, fitdat, nmesh, 
/// nshift.  The following elements of results are filled: shifts, shlike,
/// peakshlike, sigshlike.
///
/// This function computes the likelihood (shlike) in the following way:
///
///  Likelihood(s) = Sum_i f(x_i - s)**d_i
///
/// where
///
///  s - energy shift
///  i - index over nmesh
///  x - energies in mesh
///  d - counts in bindat
///  f - fitted profile in fitdat
///
/// This function is evaluated over the input array of shift values (shifts).
///
/// The likelihood function is computed at nshift energy shifts in this range.
/// The output, peakshlike, gives the shift where the likelihood is largest; if
/// two or more points have the same, peak value then only the first is returned
/// and a message is printed to the log file.  The standard deviation of the
/// likelihood function is stored in sigshlike.
///
/// Note: this function expects the output likelihood array to be allocated to
/// size nshift and initialized to zero.
void computeLikelihoodShift(FitResults& results);

/// \brief Compute an uncertainty on the fitted shift by finding the shift
///  values where Chi^2 is one greater than the minimum (peak) value.
/// \param[in,out] results FitResults structure after successful fitting
///
/// This function will compute the Chi^2 function into the m_shchi2 array
/// in results.  The shift at the peak of Chi^2 is stored in m_peakshchi2.
/// Two sigma are calculated: the first to lower shift values is stored in
/// m_sig1shchi2, and the second to larger shift values is stored in 
/// m_sig2shchi2.  The average of these two values is stored in m_sigshchi2.
///
/// Note: this function expects the output arrays, shifts and likelihood, to
/// be allocated to size nshift and initialized to zero.
void computeChi2ShiftUncertainty(FitResults& results);


/// \brief Construct array of width values for uncertainty calculations.
/// \param[in,out] results FitResults structure after successful fitting
/// \param[in] profwidth convolution width used in profile (profdat)
/// \param[in] rfactor used to determine range of widths for likelihood 
/// \return true if construction was successful
///
/// This function fills an array of width values in the results structure to
/// use in the likelihood and Chi^2 uncertainty routines.  The range of width
/// values is determined from the the fitted and initial widths of the fitting
/// profile.  If the width range is zero, then this routine will fail.  The
/// range is given by
///
///  wmin = fwidth/rfactor
///  wmax = fwidth*rfactor
///
/// where
///
///  fwidth = sqrt( natwidth**2 + fitwidth**2 ).
///
/// Note: this function expects the output width array to be allocated to size
/// nwidth and initialized to zero.
bool constructWidthArray(FitResults& results, double profwidth, double rfactor);


/// \brief Compute the likelihood function for width of the fitted profile on
///  the binned data set.  Compute the peak and variance of the likelihood
///  function.
/// \param[in,out] results FitResults structure after successful fitting
/// \param[in] profdat theoretical profile
/// \param[in] profwidth convolution width used in profile (profdat)
///
/// The following elements of results are used: mesh, bindat, nmesh, shift,
/// width, scale, bgrng, and slope.  The following elements are filled:
/// wdlike, peakwdlike, sigwdlike.
///
/// This function computes the likelihood (wdlike) in the following way:
///
///  Likelihood(w) = Sum_i f(w; x_i)**d_i
///
/// where
///
///  f(w; x_i) - theoretical profile with fitted shift, scale, and background
///              applied and then convolved with width, w, and evaluated at
///              energy, x_i.
///  w - convolution width
///  i - index over nmesh
///  x - energies in mesh
///  d - counts in bindat
///
/// This function is evaluated for all values in the widths array.  The output,
/// peakwdlike, gives the shift where the likelihood is largest; if two or more
/// points have the same, peak value then only the first is returned and a
/// message is printed to the log file.  The standard deviation of the 
/// likelihood is stored in sigwdlike.
///
/// Note: this function expects the output likelihood array to be allocated to
/// size nwidth and initialized to zero.
void computeLikelihoodWidth(FitResults& results, const CalProfile& profdat,
                            double profwidth);

/// \brief Compute an uncertainty on the fitted width by finding the width
///  values where Chi^2 is one greater than the minimum (peak) value.
/// \param[in] results FitResults structure after successful fitting
/// \param[in] profdat theoretical profile
/// \param[in] profwidth convolution width used in profile (profdat)
///
/// Note: this function expects the output arrays, shifts and likelihood, to
/// be allocated to size nshift and initialized to zero.
void computeChi2WidthUncertainty(FitResults& results, const CalProfile& profdat,
                                 double profwidth);

/** @} */

}  // namespace ahgain


#endif /* AHGAIN_AHGAIN_H */

/* Revision Log
 $Log: ahgain.h,v $
 Revision 1.18  2016/08/25 17:32:00  mwitthoe
 ahgain library: 1) fix memory leak; 2) fix array overrun in unit test

 Revision 1.17  2015/11/17 16:46:19  mwitthoe
 ahgain library: write keywords to both output extensions of drift file

 Revision 1.16  2015/11/17 15:43:07  mwitthoe
 ahgain library: fix shift and correction factor values when centerprof is enabled

 Revision 1.15  2015/10/28 18:26:36  mwitthoe
 ahgain library refactoring: include all uncertainty variables into the FitResults structure to simplify gain fitting tools

 Revision 1.14  2015/10/27 19:05:47  mwitthoe
 ahgain library: add function for resetting uncertainty variables

 Revision 1.13  2015/10/27 16:32:36  mwitthoe
 ahgain library: create separate routines to construct the shift and width arrays used in the uncertainty functions; this to reduce code duplication since the likelihood and Chi^2 methods use the same arrays

 Revision 1.12  2015/10/27 14:30:16  mwitthoe
 ahgain library: add functions for computing shift/width uncertainties using Chi^2

 Revision 1.11  2015/10/13 19:39:12  mwitthoe
 ahgain library: add functions for computing the likelihood functions for the fitted shift and width parameters

 Revision 1.10  2015/03/18 17:36:05  mwitthoe
 ahgain library: change DETNAME to DETNAM

 Revision 1.9  2015/03/11 16:01:18  mwitthoe
 ahgain library: minor documentation improvements

 Revision 1.8  2015/03/10 14:45:17  mwitthoe
 ahgain library: add sloped background fitting to gain library; generalized fitting routines to support 3 background modes: none, constant, sloped

 Revision 1.7  2015/01/21 16:10:13  mwitthoe
 ahgain library: add argument to constructCalibrationProfile() to allow the theoretical profile to be centered in the energy mesh; add functions for fitting with a sloped background (these are for testing purposes only)

 Revision 1.6  2014/11/12 13:12:21  mwitthoe
 ahgain library: in createGainOutput(), add parameter to specify whether or not the Drift_energy extension is made

 Revision 1.5  2014/10/02 01:18:29  mwitthoe
 ahgain library: add NULL flag for event TIME column; check if TIME is NULL before trying to fit events; see issue 445

 Revision 1.4  2014/08/07 17:38:44  mwitthoe
 ahgain library: modify constructCalibrationProfile() to also perform the convolution; this function will check if the mesh is too coarse to accurately represent the natural line widths and, if so, construct the profile using only the Gaussian width

 Revision 1.3  2014/08/06 20:34:54  mwitthoe
 ahgain library: correct keywords in output file; compute EXPOSURE column; change DELTATIME column to TELAPSE; write group averages to output file

 Revision 1.2  2014/07/21 15:36:53  mwitthoe
 ahgain: correct the calculation of the fitted width; previous version was using the FWHM in eV from the broadening parameter instead of the standard deviation in channel units

 Revision 1.1  2014/07/17 19:47:20  mwitthoe
 add ahgain library which contains routines for fitting event data with a calibration feature


*/
