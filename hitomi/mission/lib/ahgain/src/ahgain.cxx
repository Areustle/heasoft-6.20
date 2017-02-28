/// \file ahgain.cxx
/// \brief Calculate gain corrections from calibration features.
/// \author Mike Witthoeft
/// \date $Date: 2016/08/18 15:12:55 $
 
#define AHLABEL ahgain_ahgain
#define AHCVSID "$Id: ahgain.cxx,v 1.27 2016/08/18 15:12:55 mwitthoe Exp $"

#include "ahgain/ahgain.h"
#include "ahmath/ahmath.h"
#include "ahlog/ahlog.h"

#include <cmath>
#include <sstream>
#include <iomanip>    // std::setprecision   +++ temporary!

namespace ahgain {

// ****************************************************************************

void constructCalibrationProfile(double* mesh, int nmesh, double evchannel,
                                 const callines::CalLines& callines, 
                                 bool centerprof, double sigma,
                                 CalProfile& profdat) {

  AH_INFO(ahlog::LOW) << "--------------------------------------------------" << std::endl;
  AH_INFO(ahlog::LOW) << "Constructing calibration profile" << std::endl;

  // If this variable is true, then the mesh is too coarse to construct
  // the Lorentzian lines directly.  Instead, each line will be treated as
  // a delta-function and Gaussians (of width sigma) will be used.
  bool coarsemesh=false;

  // This constant is used to define a coarse mesh (see coarsemesh above).
  // If there are fewer than this many mesh points in the span of the largest
  // line width, then then mesh is considered coarse.
  int ptperwidth=3;

  // Mesh spacing is assumed to be constant.
  double dmesh=mesh[1]-mesh[0];

  // Allocate memory
  // Note: the memory will be freed by the destructor of CalProfile.
  profdat.m_nmesh=nmesh;
  if (0 != profdat.m_mesh) { delete [] profdat.m_mesh; profdat.m_mesh=0; }
  if (0 != profdat.m_prof) { delete [] profdat.m_prof; profdat.m_prof=0; }
  if (0 != profdat.m_dprof) { delete [] profdat.m_dprof; profdat.m_dprof=0; }
  profdat.m_mesh=new double[nmesh];
  profdat.m_prof=new double[nmesh];
  profdat.m_dprof=new double[nmesh];

  // Copy mesh in structure and initialize output arrays
  for (int j=0; j < nmesh; j++) {
    profdat.m_mesh[j]=mesh[j];
    profdat.m_prof[j]=0.;
    profdat.m_dprof[j]=0.;
  }

  // If centering profile in mesh, then need the energy shift for each
  // theoretical line.  Note: using evchannel to convert profile average
  // from eV to channel.
  double shift=0.;
  if (centerprof) {
    double meshcenter=0.5*(mesh[0]+mesh[nmesh-1]);
    shift=meshcenter-callines.m_avenergy/evchannel;
    profdat.m_offset=shift;
    AH_INFO(ahlog::LOW) << "Centering profile on mesh:" << std::endl;
    AH_INFO(ahlog::LOW) << "  Center of mesh:    " << meshcenter << " [channel]" << std::endl;
    AH_INFO(ahlog::LOW) << "  Center of profile: " << callines.m_avenergy << " [eV]" << std::endl;
    AH_INFO(ahlog::LOW) << "  Center of profile: " << callines.m_avenergy/evchannel << " [channel]" << std::endl;
    AH_INFO(ahlog::LOW) << "  Shift:             " << shift << " [channel]" << std::endl;
  }

  // Determine if mesh is coarse.
  double maxwidth=0.;
  for (int k=0; k < callines.m_nlines; k++) {
    if (callines.m_width[k]/evchannel > maxwidth) 
      maxwidth=callines.m_width[k]/evchannel;
  }
  if ( (int)(maxwidth/dmesh) < ptperwidth ) coarsemesh=true;

  // If Gaussian width is also small compared to the mesh, give a warning
  if ( coarsemesh && (int)(sigma/dmesh) < ptperwidth ) {
    // +++ all these are warnings
    AH_INFO(ahlog::HIGH) << "Mesh is coarse compared to profile lines and "
                         << "Gaussian width; profile may be poorly represented."
                         << std::endl;
    AH_INFO(ahlog::HIGH) << "Max natural line width: " << maxwidth << " [channel]" << std::endl;
    AH_INFO(ahlog::HIGH) << "Gaussian sigma:         " << sigma << " [channel]" << std::endl;
    AH_INFO(ahlog::HIGH) << "Mesh spacing:           " << dmesh << " [channel]" << std::endl;
    AH_INFO(ahlog::HIGH) << "Suggested pts per bin:  " << ptperwidth << std::endl;
  }

  // If have a coarse mesh and Gaussian width is zero, then revert to
  // natural widths
  if (coarsemesh && sigma == 0.) coarsemesh=false;

  // Write profile construction method to log file
  if (coarsemesh) 
    AH_INFO(ahlog::HIGH) << "Profile constructed from Gaussians, not Voigt profiles" << std::endl;
  else
    AH_INFO(ahlog::HIGH) << "Profile constructed from Voigt profiles" << std::endl;

  // Construct profile.  
  // Note: we are converting from eV to channel with evchannel.
  if (coarsemesh) {    // construct profile with Gaussians
    for (int k=0; k < callines.m_nlines; k++) {
      double pos=callines.m_energy[k]/evchannel+shift;   // shift != 0 if centering profile
      double amp=callines.m_amplitude[k];
      for (int j=0; j < nmesh; j++) {
        double tmp=(mesh[j]-pos)/sigma;
        profdat.m_prof[j]+=amp*exp(-0.5*tmp*tmp);
      }
    }
  } else {     
    // construct profile with Lorentzians
    for (int k=0; k < callines.m_nlines; k++) {
      double pos=callines.m_energy[k]/evchannel+shift;   // shift != 0 if centering profile
      double wid=callines.m_width[k]/evchannel;
      double amp=callines.m_amplitude[k];
      for (int j=0; j < nmesh; j++) {
        double tmp=2.*(mesh[j]-pos)/wid;
        profdat.m_prof[j]+=amp/(1.+tmp*tmp);
      }
    }

    // convolve with Gaussian
    double* cprof=new double[nmesh];   // temporarily store convolved profile
    ahmath::convolveWithGaussianFixed(mesh,profdat.m_prof,nmesh,sigma,cprof);
    for (int ii=0; ii < nmesh; ii++) profdat.m_prof[ii]=cprof[ii];  // copy result
    delete [] cprof;    // clear memory
  }

  // Calculate derivative
  ahmath::calcFirstDerivative(profdat.m_mesh,profdat.m_prof,profdat.m_nmesh,
                              profdat.m_dprof);

  // calculate average and variance
  calcAverageAndVariance(profdat.m_mesh,profdat.m_prof,profdat.m_nmesh,
                         profdat.m_avg_prof,profdat.m_var_prof);

  AH_INFO(ahlog::LOW) << "Finished constructing calibration profile" << std::endl;
  AH_INFO(ahlog::LOW) << "--------------------------------------------------" << std::endl;

}

// ****************************************************************************

void convolveCalibrationProfile(const CalProfile& profdatin, double sigma,
                                CalProfile& profdatout) {

  // allocate memory
  if (0 != profdatout.m_mesh) { delete [] profdatout.m_mesh; profdatout.m_mesh=0; }
  if (0 != profdatout.m_prof) { delete [] profdatout.m_prof; profdatout.m_prof=0; }
  if (0 != profdatout.m_dprof) { delete [] profdatout.m_dprof; profdatout.m_dprof=0; }
  profdatout.m_mesh=new double[profdatin.m_nmesh];
  profdatout.m_prof=new double[profdatin.m_nmesh];
  profdatout.m_dprof=new double[profdatin.m_nmesh];

  // Copy mesh and convolve profile & derivative.
  // Note: using convolution routine optimized for a mesh with fixed spacing.
  profdatout.m_nmesh=profdatin.m_nmesh;
  for (int is=0; is < profdatin.m_nmesh; is++) profdatout.m_mesh[is]=profdatin.m_mesh[is];
  ahmath::convolveWithGaussianFixed(profdatin.m_mesh,profdatin.m_prof,profdatin.m_nmesh,sigma,profdatout.m_prof);
  ahmath::convolveWithGaussianFixed(profdatin.m_mesh,profdatin.m_dprof,profdatin.m_nmesh,sigma,profdatout.m_dprof);

  // calculate average and variance
  calcAverageAndVariance(profdatout.m_mesh,profdatout.m_prof,profdatout.m_nmesh,
                         profdatout.m_avg_prof,profdatout.m_var_prof);
}

// ****************************************************************************

void calcAverageAndVariance(double* mesh, double* prof, int nmesh,
                            double& avg, double& var) {

  double asum1=0.;         // weighted sum of energy
  double bsum=0.;          // unweighted sum of amplitude
  for (int j=0; j < nmesh; j++) {
    asum1+=mesh[j]*prof[j];
    bsum+=prof[j];
  }
  avg=asum1/bsum;

  // Performing variance calculation as separate loop to avoid possibility
  // of taking the difference of two large numbers.
  double asum2=0.;         // weighted sum of (energy-avg)**2
  for (int j=0; j < nmesh; j++) {
    asum2+=(mesh[j]-avg)*(mesh[j]-avg)*prof[j];
  }
  var=asum2/bsum;
}

// ****************************************************************************

double computeAverageWindowRadius(const callines::CalLines& callines,
                                  double gaufwhm, int nvoigt) {

  // find width of line furthest from the profile average
  double maxdiffenergy=std::abs(callines.m_energy[0]-callines.m_avenergy);
  double maxdiffwidth=callines.m_width[0];
  for (int k=1; k < callines.m_nlines; k++) {
    double diff=std::abs(callines.m_energy[k]-callines.m_avenergy);
    if (diff > maxdiffenergy) {
      maxdiffenergy=diff;
      maxdiffwidth=callines.m_width[k];
    }
  }

  // compute cumulative width; see header description of this function
  double cumwidth=0.5346*maxdiffwidth+std::sqrt(0.2166*maxdiffwidth*maxdiffwidth+
                                                gaufwhm*gaufwhm);

  return maxdiffenergy+0.5*nvoigt*cumwidth;    // 0.5 => want half of FWHM
}

// ****************************************************************************

void initializeFitResults(double* mesh, int nmesh, int nshift, int nwidth,
                          FitResults& results) {

  if (nmesh == 0) AH_THROW_LOGIC("cannot initialize FitResults with nmesh=0");
  if (nshift == 0) AH_THROW_LOGIC("cannot initialize FitResults with nshift=0");
  if (nwidth == 0) AH_THROW_LOGIC("cannot initialize FitResults with nwidth=0");

  if (results.m_mesh != 0) AH_THROW_LOGIC("can only initialize FitResults once");
  results.m_nmesh=nmesh;
  results.m_mesh=new double[nmesh];
  results.m_bindat=new double[nmesh];
  results.m_fitdat=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    results.m_mesh[j]=mesh[j];
    results.m_bindat[j]=0.;
    results.m_fitdat[j]=0.;
  }

  results.m_nshift=nshift;
  results.m_shifts=new double[nshift];
  results.m_shlike=new double[nshift];
  results.m_shchi2=new double[nshift];
  for (int j=0; j < nshift; j++) {
    results.m_shifts[j]=0.;
    results.m_shlike[j]=0.;
    results.m_shchi2[j]=0.;
  }

  results.m_nwidth=nwidth;
  results.m_widths=new double[nwidth];
  results.m_wdlike=new double[nwidth];
  results.m_wdchi2=new double[nwidth];
  for (int j=0; j < nwidth; j++) {
    results.m_widths[j]=0.;
    results.m_wdlike[j]=0.;
    results.m_wdchi2[j]=0.;
  }
}

// ****************************************************************************

void shiftData(double* mesh, double* prof, int nmesh, double shift,
                  double* profout) {

  int jp=0;
  for (int j=0; j < nmesh; j++) {
    double xp=mesh[j]-shift;

    if (xp <= mesh[0])
      profout[j]=prof[0];          // constant background to left
    else if (xp >= mesh[nmesh-1])
      profout[j]=prof[nmesh-1];    // constant background to right
    else {
      while (mesh[jp] < xp) jp++;
      profout[j]=ahmath::interpolate_point_twopoint(xp,mesh[jp-1],prof[jp-1],
                                                    mesh[jp],prof[jp]);
    }
  }
}

// ****************************************************************************

void applyShiftParameters(double* mesh, double* prof, int nmesh, double shift,
                          double scale, double bgrnd, double slope, double* profout) {
  ahgain::shiftData(mesh,prof,nmesh,shift,profout);
  for (int j=0; j < nmesh; j++) profout[j]=scale*profout[j]+bgrnd+slope*mesh[j];
}

// ****************************************************************************

void calcFunctionalShift(double* mesh, double* bindat, double* prof,
                         double* dprof, int nmesh, int backtype, double& z,
                         double& scale, double& bgrnd, double& slope) {

  // summation variables
  double sum_F00=0.;     // sum f(x_i-mu)^2               [all background types]
  double sum_F01=0.;     // sum f(x_i-mu)                 [const and sloped backgrounds]
  double sum_F11=0.;     // sum 1                         [const and sloped backgrounds]
  double sum_F02=0.;     // sum x_i f(x_i-mu)             [sloped background]
  double sum_F12=0.;     // sum x_i                       [sloped background]
  double sum_F22=0.;     // sum x_i^2                     [sloped background]
  double sum_G0=0.;      // sum g_i f(x_i-mu)             [all background types]
  double sum_G1=0.;      // sum g_i                       [const and sloped backgrounds]
  double sum_G2=0.;      // sum x_i g_i                   [sloped background]
  double sum_F00p=0.;    // sum f(x_i-mu) df(x_i-mu)/dmu  [all background types]
  double sum_F01p=0.;    // sum df(x_i-mu)/dmu            [const and sloped backgrounds]
  double sum_F02p=0.;    // sum x_i df(x_i-mu)/dmu        [sloped background]
  double sum_G0p=0.;     // sum g_i df(x_i-mu)/dmu        [all background types]

  // compute sums needed by all background types
  for (int j=0; j < nmesh; j++) {
    sum_F00+=prof[j]*prof[j];
    sum_G0+=bindat[j]*prof[j];
    sum_F00p+=prof[j]*dprof[j];
    sum_G0p+=bindat[j]*dprof[j];
  }

  // compute sums needed by constant and sloped background types
  if (backtype != e_NOBACKGROUND) {
    for (int j=0; j < nmesh; j++) {
      sum_F01+=prof[j];
      sum_F11+=1.;
      sum_G1+=bindat[j];
      sum_F01p+=dprof[j];
    }
  }

  // compute sums needed by sloped background
  if (backtype == e_SLOPEBACKGROUND) {
    for (int j=0; j < nmesh; j++) {
      sum_F02+=mesh[j]*prof[j];
      sum_F12+=mesh[j];
      sum_F22+=mesh[j]*mesh[j];
      sum_G2+=mesh[j]*bindat[j];
      sum_F02p+=mesh[j]*dprof[j];
    }
  }

  // compute fitted parameters based on background type
  if (backtype == e_NOBACKGROUND) {
    scale=sum_G0/sum_F00;
    bgrnd=0.;
    slope=0.;
  } else if (backtype == e_CONSTBACKGROUND) {   // invert 2x2 matrix
    double factor=1./(sum_F00*sum_F11-sum_F01*sum_F01);
    scale=factor*(sum_F11*sum_G0-sum_F01*sum_G1);
    bgrnd=factor*(-sum_F01*sum_G0+sum_F00*sum_G1);
    slope=0.;
  } else if (backtype == e_SLOPEBACKGROUND) {   // invert 3x3 matrix
    // b variables hold inverse 3x3 matrix
    double b11=0.;
    double b12=0.;
    double b13=0.;
    double b21=0.;
    double b22=0.;
    double b23=0.;
    double b31=0.;
    double b32=0.;
    double b33=0.;
    bool okay=true;
    ahmath::inverse3x3(sum_F00, sum_F01, sum_F02,     // input row 1
                       sum_F01, sum_F11, sum_F12,     // input row 2
                       sum_F02, sum_F12, sum_F22,     // input row 3
                       b11,b12,b13,b21,b22,b23,b31,b32,b33,okay);
    if (!okay) AH_THROW_RUNTIME("failed to determine fitting parameters with sloped background");

    scale=b11*sum_G0+b12*sum_G1+b13*sum_G2;
    bgrnd=b21*sum_G0+b22*sum_G1+b23*sum_G2;
    slope=b31*sum_G0+b32*sum_G1+b33*sum_G2;
  } else {
    AH_THROW_LOGIC("invalid background type");
  }

  // compute functional value
  z=sum_G0p-scale*sum_F00p;
  if (backtype != e_NOBACKGROUND) z-=bgrnd*sum_F01p;
  if (backtype == e_SLOPEBACKGROUND) z-=slope*sum_F02p;
}

// ****************************************************************************

void calcFunctionalWidth(double* mesh, double* bindat, double* prof, int nmesh,
                         double width, double& z, double& scale, double* profout) {

  if (width <= 0.) AH_THROW_LOGIC("functional can only be computed for a positive, non-zero width");

  // Ratio of a circle's circumference to its diameter
  const double pi=2.0*std::asin(1.);

  // Initialize output profile to zero and prepare array to hold 
  // the derivative of the convolved profile with width.
  double* dcprof=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    profout[j]=0.;
    dcprof[j]=0.;
  }

  // Convolve profile with Gaussian of given width; using fixed-mesh-spacing
  // function to improve speed.
  ahmath::convolveWithGaussianFixed(mesh,prof,nmesh,width,profout);

  // Calculate deriviative of convolved function with respect to width: 2 steps

  // Step 1: pre-calculate exponential terms for efficiency; instead of 
  // calculating exponentials nmesh^2 times, we can calculate all nmesh
  // terms at the beginning.
  double dmesh=mesh[1]-mesh[0];      // assuming constant mesh spacing
  double* fexp=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    double xp=dmesh*j/width;
    fexp[j]=(xp*xp-1.)*std::exp(-0.5*xp*xp);
  }

  // Step 2: perform convolution integral using the trapezoidal method.
  // Note: main body of integral is split into two parts, j<i and j>i, to 
  // avoid call to the absolute value function (improves performance).
  double factor=dmesh/width/width/std::sqrt(2.*pi);    // factor outside integral
  for (int j=0; j < nmesh; j++) {
    double asum=0.;     // integration sum variable

    // first point has factor of 1/2 (trapezoidal method)
    int i=0;
    asum+=0.5*prof[i]*fexp[j-i];

    // middle points with i < j
    for (i=1; i < j; i++) asum+=prof[i]*fexp[j-i];

    // middle points with i >= j
    // Note: need to correct starting index for when j==0
    int istart=std::max(1,j);
    for (i=istart; i < nmesh-1; i++) asum+=prof[i]*fexp[i-j];

    // last point has factor of 1/2 (trapezoidal method)
    i=nmesh-1;
    asum+=0.5*prof[i]*fexp[i-j];

    dcprof[j]=factor*asum;
  }

  // The functional is computed from a bunch of sums
  double sum_hF=0.;     // sum[ bindat * profout ]
  double sum_F2=0.;     // sum[ profout * profout ]
  double sum_FG=0.;     // sum[ profout * dcprof ]
  double sum_hG=0.;     // sum[ bindat * dcprof ]
  for (int j=0; j < nmesh; j++) {
    sum_hF+=bindat[j]*profout[j];
    sum_F2+=profout[j]*profout[j];
    sum_FG+=profout[j]*dcprof[j];
    sum_hG+=bindat[j]*dcprof[j];
  }
  scale=sum_hF/sum_F2;
  z=scale*sum_FG-sum_hG;

  // Appy scale factor to output profile
  for (int j=0; j < nmesh; j++) profout[j]*=scale;

  // Delete temporary arrays
  delete [] dcprof;
  delete [] fexp;

}

// ****************************************************************************

void findZeroShift(double* mesh, double* bindat, double* prof, double* dprof,
                   int nmesh, int backtype, double shift0, double searchstepsize, 
                   double maxdshift, double bisectol, double& shift, 
                   double& scale, double& bgrnd, double& slope, double& r2,
                   double* profout, int& status) {

  const double NOSOL=1000000.;     // indicate that no solution was found

  // Prepare arrays to hold shifted profile and its derivative
  double* sprof=new double[nmesh];
  double* dsprof=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    sprof[j]=0.;
    dsprof[j]=0.;
  }

  // Need to keep track of two sets of solutions
  double z1=0.;
  double scale1=0.;
  double bgrnd1=0.;
  double slope1=0.;
  double z2=0.;
  double scale2=0.;
  double bgrnd2=0.;
  double slope2=0.;

  // Get value of functional at initial shift
  ahgain::shiftData(mesh,prof,nmesh,shift0,sprof);    // shift profile
  ahgain::shiftData(mesh,dprof,nmesh,shift0,dsprof);  // shift profile derivative
  ahgain::calcFunctionalShift(mesh,bindat,sprof,dsprof,nmesh,backtype,
                              z1,scale1,bgrnd1,slope1);

  // Step in direction of searchstepsize until functional (z) changes sign
  // or the search range is exhausted (maxdshift)
  z2=NOSOL;
  scale2=1.;
  bgrnd2=0.;
  slope2=0.;
  shift=shift0;
  while (1) {
    shift+=searchstepsize;
    if (std::abs(shift-shift0) > maxdshift) break;  // search range exhausted

    // Calculate functional at new shift
    ahgain::shiftData(mesh,prof,nmesh,shift,sprof);
    ahgain::shiftData(mesh,dprof,nmesh,shift,dsprof);
    ahgain::calcFunctionalShift(mesh,bindat,sprof,dsprof,nmesh,backtype,
                        z2,scale2,bgrnd2,slope2);

    // Check if done with search
    if (z1*z2 < 0.) break;    // found a zero!

    // Prepare for next step iteration
    z1=z2;
    z2=NOSOL;
  }

  // If no solution, then return non-zero status
  if (z2 == NOSOL) {
    status=1;
    shift=0.;
    scale=1.;
    bgrnd=0.;
    slope=0.;
    r2=0.;
    for (int j=0; j < nmesh; j++) profout[j]=prof[j];
  }

  // Use bisection method to get shift within desired tolerance (bisecttol)
  if (status == 0) {     // solution exists

    // shifts to use in bisection method
    double s1=0.;
    double s2=0.;

    // Set up initial conditions for bisection method; the sign of
    // searchstepsize tells us which initial condition has the lower energy
    if (searchstepsize > 0.) {
      s1=shift-searchstepsize;
      s2=shift;
    } else {
      s1=shift;
      s2=shift-searchstepsize;
      double tz=z1;      // swap functional values: z1,z2 = z2,z1
      z1=z2;
      z2=tz;
    }

    // If tolerance condition is already satisfied (s2-s1 < bisectol) then 
    // use second point as the output
    scale=scale2;
    bgrnd=bgrnd2;
    slope=slope2;

    // Perform bisection method
    while ( (s2-s1) > bisectol) {
      double z=0.;
      shift=0.5*(s1+s2);
      ahgain::shiftData(mesh,prof,nmesh,shift,sprof);
      ahgain::shiftData(mesh,dprof,nmesh,shift,dsprof);
      ahgain::calcFunctionalShift(mesh,bindat,sprof,dsprof,nmesh,backtype,
                                  z,scale,bgrnd,slope);

      // if current z is really close to zero, then stop
      if (std::abs(z) < 1.e-9) break;

      // reset range to where z changes sign
      if (z*z1 < 0.) {
        s2=shift;
        z2=z;
      } else {
        s1=shift;
        z1=z;
      }
    }

    // Apply scale/background parameter to shifted profile for output
    applyShiftParameters(mesh,sprof,nmesh,0.0,scale,bgrnd,slope,profout);

    // Calculate R^2 for output
    r2=ahmath::calcR2(bindat,profout,nmesh);

  }  // end if status==0

  // free memory
  delete [] sprof;
  delete [] dsprof;

}

// ****************************************************************************

void findZeroWidth(double* mesh, double* bindat, double* prof, int nmesh,
                   double width0, double searchstepsize, double minwidth,
                   double maxdwidth, double bisectol, double& width, 
                   double& scale, double& r2, double* profout, int& status) {

  const double NOSOL=1000000.;     // indicate that no solution was found

  // Set up two sets of solutions
  double z1=0.;
  double scale1=0.;
  double* bprof1=new double[nmesh];
  double z2=0.;
  double scale2=0.;
  double* bprof2=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    bprof1[j]=0.;
    bprof2[j]=0.;
  }

  // Get value of functional at initial shift
  ahgain::calcFunctionalWidth(mesh,bindat,prof,nmesh,width0,z1,scale1,bprof1);

  // Step in direction of searchstepsize until functional (z) changes sign
  // or the search range is exhausted (maxdwidth)
  z2=NOSOL;
  width=width0;
  double widthlast=width;   // keep track of the width from the previous step
  scale2=1.;
  while (1) {
    if (width+searchstepsize < 0.) {     // do not allow negative widths
      width=0.5*width;
    } else {
      width+=searchstepsize;
    }
    if (width < minwidth) break;                    // search range exhausted
    if (std::abs(width-width0) > maxdwidth) break;  // search range exhausted

    // Calculate functional at new width
    calcFunctionalWidth(mesh,bindat,prof,nmesh,width,z2,scale2,bprof2);

    // Check if done with search
    if (z1*z2 < 0.) break;    // found a zero!

    // Prepare for next step iteration
    z1=z2;
    z2=NOSOL;
  }

  // If no solution, then return non-zero status
  if (z2 == NOSOL) {
    status=1;
    width=width0;
    scale=1.;
    r2=0.;
    for (int j=0; j < nmesh; j++) profout[j]=bprof1[j];
  }

  // Use bisection method to get width within desired tolerance (bisecttol)
  if (status == 0) {     // solution exists

    // widths to use for bisection method
    double w1=0.;
    double w2=0.;

    // Set up initial conditions for bisection method; the sign of
    // searchstepsize tells us which initial condition has the lower energy
    if (searchstepsize > 0.) {
      w1=widthlast;
      w2=width;
    } else {
      w1=width;
      w2=widthlast;
      double tz=z1;      // swap functional values: z1,z2 = z2,z1
      z1=z2;
      z2=tz;
    }

    // If tolerance condition is already satisfied (w2-w1 < bisectol) then 
    // use second point as the output
    scale=scale2;
    for (int j=0; j < nmesh; j++) profout[j]=bprof2[j];

    // Perform bisection method
    while ( (w2-w1) > bisectol) {
      double z=0.;
      width=0.5*(w1+w2);
      calcFunctionalWidth(mesh,bindat,prof,nmesh,width,z,scale,profout);

      // if current z is really close to zero, then stop
      if (std::abs(z) < 1.e-9) break;

      // reset range to where z changes sign
      if (z*z1 < 0.) {
        w2=width;
        z2=z;
      } else {
        w1=width;
        z1=z;
      }
    }

    // Calculate R^2 for output
    r2=ahmath::calcR2(bindat,profout,nmesh);

  }  // end if status==0

  // free memory
  delete [] bprof1;
  delete [] bprof2;

}

// ****************************************************************************

void fitShift(double* mesh, double* bindat, double* prof, double* dprof, 
              int nmesh, int backtype, double shift0, double searchstepsize,
              double maxdshift, double bisectol, double& shift, double& scale,
              double& bgrnd, double& slope, double& r2, double* profout, int& status) {

  // Solution at smaller shifts
  double shift1=0.;
  double scale1=0.;
  double bgrnd1=0.;
  double slope1=0.;
  double r2_1=0.;
  int status1=0;
  double* prof1=new double[nmesh];
  for (int j=0; j < nmesh; j++) prof1[j]=0.;

  // Solution at larger shifts
  double shift2=0.;
  double scale2=0.;
  double bgrnd2=0.;
  double slope2=0.;
  double r2_2=0.;
  int status2=0;
  double* prof2=new double[nmesh];
  for (int j=0; j < nmesh; j++) prof2[j]=0.;

  // Search for solution at smaller shifts: -searchstepsize
  ahgain::findZeroShift(mesh,bindat,prof,dprof,nmesh,backtype,shift0,
                        -searchstepsize,maxdshift, bisectol,shift1,scale1,
                        bgrnd1,slope1,r2_1,prof1,status1);

  // Search for solution at larger shifts: +searchstepsize
  ahgain::findZeroShift(mesh,bindat,prof,dprof,nmesh,backtype,shift0,
                        +searchstepsize,maxdshift,bisectol,shift2,scale2,
                        bgrnd2,slope2,r2_2,prof2,status2);

  // Result: failure if both status1 and status2 are bad
  //         shift1 if status2 is bad (and vice versa)
  //         shift with better R^2 if both status1 and status2 are OK
  if (status1 > 0 && status2 > 0) {
    status=1;
  } else if (status1 > 0) {
    status=0;
    shift=shift2;
    scale=scale2;
    bgrnd=bgrnd2;
    slope=slope2;
    r2=r2_2;
    for (int j=0; j < nmesh; j++) profout[j]=prof2[j];
  } else if (status2 > 0) {
    status=0;
    shift=shift1;
    scale=scale1;
    bgrnd=bgrnd1;
    slope=slope1;
    r2=r2_1;
    for (int j=0; j < nmesh; j++) profout[j]=prof1[j];
  } else if (r2_2 > r2_1) {
    status=0;
    shift=shift2;
    scale=scale2;
    bgrnd=bgrnd2;
    slope=slope2;
    r2=r2_2;
    for (int j=0; j < nmesh; j++) profout[j]=prof2[j];
  } else {
    status=0;
    shift=shift1;
    scale=scale1;
    bgrnd=bgrnd1;
    slope=slope1;
    r2=r2_1;
    for (int j=0; j < nmesh; j++) profout[j]=prof1[j];
  }

  // free memory
  delete [] prof1;
  delete [] prof2;
}

// ****************************************************************************

void fitWidth(double* mesh, double* bindat, double* prof, int nmesh, 
              double width0, double searchstepsize, double minwidth,
              double maxdwidth, double bisectol, double& width, double& scale,
              double& r2, double* profout, int& status) {

  // Solution at smaller widths
  double width1=0.;
  double scale1=0.;
  double r2_1=0.;
  double* prof1=new double[nmesh];
  for (int j=0; j < nmesh; j++) prof1[j]=0.;
  int status1=0;

  // Solution at larger widths
  double width2=0.;
  double scale2=0.;
  double r2_2=0.;
  double* prof2=new double[nmesh];
  for (int j=0; j < nmesh; j++) prof2[j]=0.;
  int status2=0;

  // Search for solution at smaller widths: -searchstepsize
  findZeroWidth(mesh,bindat,prof,nmesh,width0,-searchstepsize,minwidth,maxdwidth,
                bisectol,width1,scale1,r2_1,prof1,status1);

  // Search for solution at larger widths: +searchstepsize
  findZeroWidth(mesh,bindat,prof,nmesh,width0,+searchstepsize,minwidth,maxdwidth,
                bisectol,width2,scale2,r2_2,prof2,status2);

  // Result: failure if both status1 and status2 are bad
  //         shift1 if status2 is bad (and vice versa)
  //         shift with better R^2 if both status1 and status2 are OK
  if (status1 > 0 && status2 > 0) {
    status=1;
  } else if (status1 > 0) {
    status=0;
    width=width2;
    scale=scale2;
    r2=r2_2;
    for (int j=0; j < nmesh; j++) profout[j]=prof2[j];
  } else if (status2 > 0) {
    status=0;
    width=width1;
    scale=scale1;
    r2=r2_1;
    for (int j=0; j < nmesh; j++) profout[j]=prof1[j];
  } else if (r2_2 > r2_1) {
    status=0;
    width=width2;
    scale=scale2;
    r2=r2_2;
    for (int j=0; j < nmesh; j++) profout[j]=prof2[j];
  } else {
    status=0;
    width=width1;
    scale=scale1;
    r2=r2_1;
    for (int j=0; j < nmesh; j++) profout[j]=prof1[j];
  }

  // free memory
  delete [] prof1;
  delete [] prof2;
}

// ****************************************************************************

void createGainOutput(const std::string& filename, const std::string& splitcolumn,
                      const std::string& telescop, const std::string& instrume,
                      const std::string& detnam, bool gridprofile, 
                      const std::string& energref, double chfirst, double chlast,
                      double chwidth, const std::string& picolumn, int maxnmesh,
                      bool calcerr, bool writeerrfunc, int nshift, int nwidth, 
                      ahfits::FilePtr& fpout) {

  ahfits::create(filename,"",&fpout);

  // construct data type for arrays in drift table
  std::stringstream ssdtype;
  ssdtype << maxnmesh << "D";
  std::string dtype=ssdtype.str();

  if (!gridprofile) {
    // The Drift_energy extension contains table of drift corrections.
    ahfits::addEmptyTbl(fpout,"Drift_energy");
    if (telescop != "") ahfits::writeKeyValStr(fpout,"TELESCOP",telescop,"");
    if (instrume != "") ahfits::writeKeyValStr(fpout,"INSTRUME",instrume,"");
    if (detnam != "") ahfits::writeKeyValStr(fpout,"DETNAM",detnam,"");
    ahfits::insertColBefore(fpout,"TIME","1D","");
    ahfits::setTUnit(fpout,"TIME","sec");
    if (splitcolumn != "") ahfits::insertColAfter(fpout,splitcolumn,"1I","");
    ahfits::insertColAfter(fpout,"COR_FIT","1D","");
    ahfits::insertColAfter(fpout,"COR_AVE","1D","");
    ahfits::insertColAfter(fpout,"CHISQ","1D","");
    ahfits::insertColAfter(fpout,"AVGUNBIN","1D","");  // unbinned average
    ahfits::insertColAfter(fpout,"AVGBIN","1D","");    // binned average
    ahfits::insertColAfter(fpout,"AVGFIT","1D","");    // average from fit
    ahfits::insertColAfter(fpout,"SHIFT","1D","");     // fitted shift
    ahfits::insertColAfter(fpout,"SCALE","1D","");     // fitted scale factor
    ahfits::insertColAfter(fpout,"BGRND","1D","");     // fitted background
    ahfits::insertColAfter(fpout,"SLOPE","1D","");     // fitted background slope
    ahfits::insertColAfter(fpout,"WIDTH","1D","");     // fitted width
    ahfits::insertColAfter(fpout,"TELAPSE","1D","");
    ahfits::insertColAfter(fpout,"EXPOSURE","1D","");
    ahfits::insertColAfter(fpout,"NEVENT","1I","");
    ahfits::insertColAfter(fpout,"SPECTRUM",dtype,"");  // binned events
    ahfits::insertColAfter(fpout,"FITPROF",dtype,"");   // theoretical profile with fitted parameters applied

    ahfits::writeKeyValStr(fpout,"ENERGREF",energref,"");
    ahfits::writeKeyValDbl(fpout,"CH_START",chfirst,"");
    ahfits::writeKeyValDbl(fpout,"CH_STOP",chlast,"");
    ahfits::writeKeyValDbl(fpout,"CH_WIDTH",chwidth,"");
    ahfits::writeKeyValStr(fpout,"PICOLUMN",picolumn,"");

    if (calcerr) {
      ahfits::insertColAfter(fpout,"SIGSHLIKE","1D","");    // sigma of shift likelihood function
      ahfits::insertColAfter(fpout,"SIGWDLIKE","1D","");    // sigma of width likelihood function
      ahfits::insertColAfter(fpout,"SIGSHCHI2","1D","");    // sigma of shift from Chi^2
      ahfits::insertColAfter(fpout,"SIGWDCHI2","1D","");    // sigma of width from Chi^2

      if (writeerrfunc) {
        std::stringstream shform;         // format specifier for shift likelihood
        shform << nshift << "D";
        ahfits::insertColAfter(fpout,"SHIFTS",shform.str(),"");   // array of shift values for shift error functions
        ahfits::insertColAfter(fpout,"SHLIKE",shform.str(),"");   // array of likelihood values over shifts
        ahfits::insertColAfter(fpout,"SHCHI2",shform.str(),"");   // array of Chi^2 values over shifts

        std::stringstream wdform;         // format specifier for width likelihood
        wdform << nwidth << "D";
        ahfits::insertColAfter(fpout,"WIDTHS",wdform.str(),"");   // array of width values for width error functions
        ahfits::insertColAfter(fpout,"WDLIKE",wdform.str(),"");   // array of likelihood values over widths
        ahfits::insertColAfter(fpout,"WDCHI2",wdform.str(),"");   // array of Chi^2 values over widths
      }
    }
  }

  // The Grid_profile extension contains the theoretical calibration profile
  // on the binned energy grid used in the fitting.
  ahfits::addEmptyTbl(fpout,"Grid_profile");
  if (telescop != "") ahfits::writeKeyValStr(fpout,"TELESCOP",telescop,"");
  if (instrume != "") ahfits::writeKeyValStr(fpout,"INSTRUME",instrume,"");
  if (detnam != "") ahfits::writeKeyValStr(fpout,"DETNAM",detnam,"");
  ahfits::insertColBefore(fpout,"ENERGY","1D","");
  ahfits::setTUnit(fpout,"ENERGY","eV");
  ahfits::insertColAfter(fpout,"AMPLITUDE","1D","");
  ahfits::writeKeyValStr(fpout,"ENERGREF",energref,"");
  ahfits::writeKeyValDbl(fpout,"CH_START",chfirst,"");
  ahfits::writeKeyValDbl(fpout,"CH_STOP",chlast,"");
  ahfits::writeKeyValDbl(fpout,"CH_WIDTH",chwidth,"");
  ahfits::writeKeyValStr(fpout,"PICOLUMN",picolumn,"");

}

// ****************************************************************************

void fitEvents(const EventDataVec& events, int nevents, const CalProfile& profdat,
               double profwidth, bool fitwidth, int backtype, double avgwinrad,
               double minwidth0, int maxitcycle, double r2tol, 
               double searchstepshift, double maxdshift, double bisectolshift, 
               double searchstepwidth, double minwidth, double maxdwidth, 
               double bisectolwidth, FitResults& results, int& status) {

  // check if profile defined
  if (profdat.m_nmesh == 0) AH_THROW_LOGIC("calibration profile not defined");

  // check if first or last TIMEs are NULL
  if (events[0].m_timenull == 1 || events[nevents-1].m_timenull == 1)
    AH_THROW_LOGIC("some event TIMEs are NULL; should not be included in group");

  // reset output status
  status=0;

  // for brevity
  int nmesh=profdat.m_nmesh;
  double* mesh=profdat.m_mesh;
  double dmesh=mesh[1]-mesh[0];      // assume mesh spacing is constant

  // shift fitting results
  double shift=0.;
  double scale1=0.;
  double bgrnd=0.;
  double slope=0.;

  // width fitting results
  double width=0.;
  double scale2=0.;

  // fitting is done when R^2 is converged; keep track of R^2 for last
  // two fitting attempts
  double r2=0.;
  double r2lastshift=0.;
  double r2lastwidth=0.;

  // allocate memory for binned data and fit
  double* bindat=new double[nmesh];          // binned event data
  double* fitprof=new double[nmesh];         // output fitted profile
  for (int j=0; j < nmesh; j++) {
    bindat[j]=0.;
    fitprof[j]=0.;
  }

  // Working versions of the profile and derivative arrays; these are used
  // in the fitting procedure
  double* tprof=new double[nmesh];
  double* tdprof=new double[nmesh];
  for (int j=0; j < nmesh; j++) {
    tprof[j]=0.;
    tdprof[j]=0.;
  }

  // Some other variables
  double avg_nobin=0.;     // unbinned average
  double asum1=0.;         // used to compute average of binned data
  double asum2=0.;         // used to compute variance of binned data
  double bsum=0.;          // used to computer average/variance of binned data
  double avg_bin=0.;       // binned average
  double var_bin=0.;       // varaince of binned data
  double shift0=0.;        // estimate of energy shift
  double width0=0.;        // estimate of convolution width

  // Bin data.  For j'th mesh point, find all events in range
  // (mesh[j]-0.5*dmesh : mesh[j]+0.5*dmesh)
  // Note: assumed that all points are in range of mesh
  for (int i=0; i < nevents; i++) {
    int j=0;
    while (events[i].m_pi >= mesh[j]+0.5*dmesh) j++;
    bindat[j]++;
  }

  // Calculate unbinned average
  avg_nobin=0.;
  for (int i=0; i < nevents; i++) {
    avg_nobin+=events[i].m_pi;
  }
  avg_nobin/=(double)nevents;
  AH_DEBUG << "Unbinned average:" << avg_nobin << std::endl;

  // Calculate average and variance of binned data set
  asum1=0.;     // weighted sum of energy
  asum2=0.;     // weighted sum of energy**2
  bsum=0.;      // unweighted sum of counts
  for (int j=0; j < nmesh; j++) {
    asum1+=mesh[j]*bindat[j];
    asum2+=mesh[j]*mesh[j]*bindat[j];
    bsum+=bindat[j];
  }
  avg_bin=asum1/bsum;
  var_bin=asum2/bsum-avg_bin*avg_bin;
  AH_DEBUG << "Initial binned average:" << avg_bin << std::endl;
  AH_DEBUG << "Initial binned variance:" << var_bin << std::endl;

  // Update binned average by creating smaller window around first value
  asum1=0.;
  asum2=0.;
  bsum=0.;
  for (int j=0; j < nmesh; j++) {
    if (mesh[j] < (avg_bin-avgwinrad) || mesh[j] > (avg_bin+avgwinrad)) continue;
    asum1+=mesh[j]*bindat[j];
    asum2+=mesh[j]*mesh[j]*bindat[j];
    bsum+=bindat[j];
  }
  if (0. == bsum) {
    AH_DEBUG << "No data found in the updated fitting window.  This may" << std::endl;
    AH_DEBUG << "be due to multiple features being present in the" << std::endl;
    AH_DEBUG << "search window; try reducing size of search window" << std::endl;
    AH_DEBUG << "using the extraspread parameter or the startenergy &" << std::endl;
    AH_DEBUG << "stopenergy parameters.  For this group, we will use " << std::endl;
    AH_DEBUG << "the full-window average for the output AVGBIN and" << std::endl;
    AH_DEBUG << "COR_AVE columns, but they may not be accurate." << std::endl;
  } else {
    avg_bin=asum1/bsum;
    var_bin=asum2/bsum-avg_bin*avg_bin;
    AH_DEBUG << "Updated binned average: " << avg_bin << std::endl;
    AH_DEBUG << "Updated binned variance: " << var_bin << std::endl;
  }

  // Estimate of shift based on binned average
  shift0=avg_bin-profdat.m_avg_prof;
  AH_DEBUG << "Estimated energy shift: " << shift0 << std::endl;

  // If fitting the width, get estimate of the width using the difference
  // between the variances of the binned data and the theoretical profile.
  // If the estimate is too small, set it to minwidth0.
  width0=0.;
  if (fitwidth) {
    double dvar=var_bin-profdat.m_var_prof;
    double minsigma0=ahmath::convertFWHM2sigma(minwidth0);
    if (dvar > minsigma0*minsigma0)
      width0=std::sqrt(dvar);
    else
      width0=minsigma0;
  } else {      // not fitting width
    width0=0.;
  }
  AH_DEBUG << "Estimated width: " << width0 << std::endl;

  // Loop over shift/width fitting cycles; fitting is finished when
  // R^2 changes by less than r2tol.
  scale2=1.;            // initial scale factor for width fitting
  width=width0;
  int k=0;              // iteration counter
  while (1) {
    k++;
    if (fitwidth && k > maxitcycle) {
      // +++ warning
      AH_INFO(ahlog::LOW) << "Fitting failed: max iterations of " << maxitcycle << "reached" << std::endl;
      status=1;
      break;
    }

    // Convolve base profile and derivative with width, if necessary.  We
    // start each iteration with the original profile so that the fitted
    // shift is the total shift and not an additional shift from the previous
    // iteration.  If not fitting the width, then there will be only one k
    // iteration and we can just use the original, inconvolved profile.
    // Note: the convolution will not change the average energy of the profile.
    // Note: using convolveWithGaussianFixed() for speed improvement.
    if (fitwidth) {
      ahmath::convolveWithGaussianFixed(mesh,profdat.m_prof,nmesh,width,tprof);
      ahmath::calcFirstDerivative(mesh,tprof,nmesh,tdprof);
    } else {    // if not fitting width, just copy original profile
      for (int j=0; j < nmesh; j++) {
        tprof[j]=profdat.m_prof[j];
        tdprof[j]=profdat.m_dprof[j];
      }
    }

    // Fit energy shift between binned data and profile (tprof)
    fitShift(mesh,bindat,tprof,tdprof,nmesh,backtype,shift0,searchstepshift,
             maxdshift,bisectolshift,shift,scale1,bgrnd,slope,r2,fitprof,status);
    if (status != 0) {
      // +++ warning
      AH_INFO(ahlog::LOW) << "Shift fitting failed in iteration " << k << std::endl;
      break;
    }

    // We are done fitting if:
    //  1. not fitting width, or
    //  2. r2 changed by less than r2tol
    if (!fitwidth || std::abs(r2-r2lastshift) < r2tol) break;
    r2lastshift=r2;

    // Fit width.  Need to use the original profile with shift parameters
    // applied, and not the profile convolved with the initial width 
    // (otherwise, we could not fit a smaller width).
    applyShiftParameters(mesh,profdat.m_prof,nmesh,shift,scale1,bgrnd,slope,tprof);
    fitWidth(mesh,bindat,tprof,nmesh,width0,searchstepwidth,minwidth,
             maxdwidth,bisectolwidth,width,scale2,r2,fitprof,status);
    if (status != 0) {
      // +++ warning
      AH_INFO(ahlog::LOW) << "Width fitting failed in iteration " << k << std::endl;
      break;
    }

    // We are done if R^2 has changed by less than the tolerance
    if (std::abs(r2-r2lastwidth) < r2tol) break;
    r2lastwidth=r2;

  }  // end while loop (k)


  // Prepare output

  // Initialize array output to zero.  This is done before copying because
  // it is possible that the mesh size may be different for different split
  // values.  The effect of this initialization is that unneeded array values
  // will be set to zero at the end of the array.
  for (int j=0; j < results.m_nmesh; j++) {
    results.m_mesh[j]=0.;
    results.m_bindat[j]=0.;
    results.m_fitdat[j]=0.;
  }

  // Output TIME = average of first and last event times.
  // Output TELASPE = difference between first and last event times.
  results.m_time=0.5*(events[nevents-1].m_time+events[0].m_time);
  results.m_telapse=events[nevents-1].m_time-events[0].m_time;

  // Set averages
  results.m_avgunbin=avg_nobin;
  results.m_avgbin=avg_bin;

  // Correction from binned average
  results.m_cor_ave=profdat.m_avg_prof/avg_bin;

  // Assign number of events in group and splitcolumn value
  results.m_nevent=nevents;
  results.m_splitval=events[0].m_split;

  // Binned data event
  for (int j=0; j < nmesh; j++) {
    results.m_mesh[j]=mesh[j];
    results.m_bindat[j]=bindat[j];
    results.m_fitdat[j]=fitprof[j];   // if fitting failed, this contains original profile
  }

  if (status == 0) {

    // The output scale and background parameters are calculated using
    // the fitted parameters from both the shift and width fitting
    results.m_fit_scale=scale1*scale2;
    results.m_fit_bgrnd=bgrnd*scale2;
    results.m_fit_slope=slope*scale2;

    // The output width must combine the fitted width and the profwidth
    // parameter and be reported as a FWHM (note: width=0 if not fitting width).
    results.m_fit_width=ahmath::convertsigma2FWHM(sqrt(width*width+profwidth*profwidth));

    // The output shift is just the fitted value
    // If centerprof=yes, then also need to add the offset of the profile
    results.m_fit_shift=shift+profdat.m_offset;        // shift relative to original profile position, so offset is added
    results.m_avgfit=profdat.m_avg_prof+shift;         // average is an absolute value, so computed from current position of profile and fitted shift

    // Compute Chi^2 between the fitted profile and binned data
    int nparam=3;              // shift, scale, background
    if (fitwidth) nparam=4;    // + width
    results.m_chisq=ahmath::calcReducedChi2(bindat,fitprof,nmesh,nparam);

    // Compute the gain corrections based on the fitted shift
    // Gain correction is ration of original profile position (so include offset
    // and the fitted average)
    results.m_cor_fit=(profdat.m_avg_prof+profdat.m_offset)/(results.m_avgfit);


    AH_INFO(ahlog::LOW) << "Fitting successful" << std::endl;
    AH_INFO(ahlog::LOW) << "Shift, Scale, Background, Slope, Width: " << results.m_fit_shift
                        << ", " << results.m_fit_scale << ", " << results.m_fit_bgrnd
                        << ", " << results.m_fit_slope << ", " << results.m_fit_width << std::endl;
  }

  // free memory
  delete [] bindat;
  delete [] fitprof;
  delete [] tprof;
  delete [] tdprof;
}

// ****************************************************************************

void resetUncertainties(FitResults& results) {

  results.m_peakshlike=0.;
  results.m_sigshlike=0.;
  results.m_peakshchi2=0.;
  results.m_sig1shchi2=0.;
  results.m_sig2shchi2=0.;
  results.m_sigshchi2=0.;
  for (int ii=0; ii < results.m_nshift; ii++) {
    results.m_shifts[ii]=0.;
    results.m_shlike[ii]=0.;
    results.m_shchi2[ii]=0.;
  }

  results.m_peakwdlike=0.;
  results.m_sigwdlike=0.;
  results.m_peakwdchi2=0.;
  results.m_sig1wdchi2=0.;
  results.m_sig2wdchi2=0.;
  results.m_sigwdchi2=0.;
  for (int ii=0; ii < results.m_nwidth; ii++) {
    results.m_widths[ii]=0.;
    results.m_wdlike[ii]=0.;
    results.m_wdchi2[ii]=0.;
  }
}

// ****************************************************************************

bool constructShiftArray(FitResults& results, double natwidth, double rfactor) {

  // Confirm that output arrays have been allocated.
  if (results.m_shifts == 0) AH_THROW_LOGIC("Output results.m_shift array needs to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nshift; i++) results.m_shifts[i]=0.;

  // Calculate base width used to get shift range
  double fitsigma=ahmath::convertFWHM2sigma(results.m_fit_width);
  double fwidth=std::sqrt(natwidth*natwidth+fitsigma*fitsigma);

  // If fwidth is zero, then there is no basis for determining the range of
  // shifts to calculate the likelihood function for.
  if (fwidth == 0.) {
    AH_INFO(ahlog::HIGH) << "Zero shift range; cannot compute likelihood.  Must provide a natural line width." << std::endl;
    return false;
  }

  // Determine range of shift values to evaluate likelihood function at
  double firstshift=-rfactor*fwidth;
  double lastshift=rfactor*fwidth;
  double dshift=(lastshift-firstshift)/(results.m_nshift-1);

  // Fill array of shift values
  for (int i=0; i < results.m_nshift; i++) results.m_shifts[i]=firstshift+dshift*i;
  return true;
}

// ****************************************************************************

void computeLikelihoodShift(FitResults& results) {

  // Confirm that output arrays have been allocated.
  if (results.m_shifts == 0 || results.m_shlike == 0)
    AH_THROW_LOGIC("Output arrays in results, m_shifts & m_shlike, need to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nshift; i++) results.m_shlike[i]=0.;

  // Create working arrays
  double* tprof=new double[results.m_nmesh];
  for (int i=0; i < results.m_nmesh; i++) tprof[i]=0.;

  // The results array may be dimensioned larger than the data it contains, so
  // the m_nmesh value should not be used.  Determine the number of valid mesh
  // points by ignoring all trailing mesh values equal to zero.  The approach
  // here looks for the mesh point whose value is less than the previous.
  int nmesh=1;
  while (nmesh < (results.m_nmesh-1) && results.m_mesh[nmesh] > results.m_mesh[nmesh-1]) {
    nmesh++;
  }

  // Compute log likelihood function
  int ipeak=0;      // index of peak likelihood
  for (int i=0; i < results.m_nshift; i++) {
    // shift fitted profile
    ahgain::shiftData(results.m_mesh,results.m_fitdat,nmesh,results.m_shifts[i],tprof);

    double asum=0.;
    for (int j=0; j < nmesh; j++) {
      if (tprof[j] < 0.) continue;
      asum+=results.m_bindat[j]*std::log(tprof[j]);
    }
    results.m_shlike[i]=asum;
    if (results.m_shlike[i] > results.m_shlike[ipeak]) ipeak=i;
  }
  results.m_peakshlike=results.m_shifts[ipeak];

  // If no peak likelihood found, send log message and return 0
  if (ipeak == 0 || ipeak == (results.m_nshift-1)) {

    AH_DEBUG << "No peak likelihood found; returning zero error.  Try increasing likelihood shift range." << std::endl;
    for (int i=0; i < results.m_nshift; i++) results.m_shlike[i]=0.;
    results.m_peakshlike=0.;
    results.m_sigshlike=0.;

  } else {

    // Normalize log likelihood
    double dshift=results.m_shifts[1]-results.m_shifts[0];   // spacing of shifts array (assumed constant)
    double peakloglike=results.m_shlike[ipeak];              // maximum value of log likelihood
    double asum=0.;
    for (int i=0; i < results.m_nshift; i++) asum+=exp(results.m_shlike[i]-peakloglike)*dshift;
    for (int i=0; i < results.m_nshift; i++) results.m_shlike[i]=(results.m_shlike[i]-peakloglike)-std::log(asum);

    // Compute variance and sigma
    asum=0.;
    for (int i=0; i < results.m_nshift; i++) {
      double ds=results.m_shifts[i]-results.m_peakshlike;
      asum+=exp(results.m_shlike[i])*ds*ds;
    }
    double var=asum*dshift;
    results.m_sigshlike=sqrt(var);

    // convert log(likelihood) to likelihood
    for (int i=0; i < results.m_nshift; i++) {
      results.m_shlike[i]=exp(results.m_shlike[i]);
    }
  }

  // free working array
  delete [] tprof;
  tprof=0;
}

// ****************************************************************************

void computeChi2ShiftUncertainty(FitResults& results) {

  // Confirm that output arrays have been allocated.
  if (results.m_shifts == 0 || results.m_shchi2 == 0)
    AH_THROW_LOGIC("Output results arrays, m_shifts & m_shchi2, need to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nshift; i++) results.m_shchi2[i]=0.;

  // Create working arrays
  double* tprof=new double[results.m_nmesh];
  for (int i=0; i < results.m_nmesh; i++) tprof[i]=0.;

  // The results array may be dimensioned larger than the data it contains, so
  // the m_nmesh value should not be used.  Determine the number of valid mesh
  // points by ignoring all trailing mesh values equal to zero.  The approach
  // here looks for the mesh point whose value is less than the previous.
  int nmesh=1;
  while (nmesh < (results.m_nmesh-1) && results.m_mesh[nmesh] > results.m_mesh[nmesh-1]) {
    nmesh++;
  }

  // Compute Chi^2 values
  int ipeak=0;      // index of peak Chi^2
  for (int i=0; i < results.m_nshift; i++) {
    // shift fitted profile
    ahgain::shiftData(results.m_mesh,results.m_fitdat,nmesh,results.m_shifts[i],tprof);
    results.m_shchi2[i]=ahmath::calcChi2(results.m_bindat,tprof,nmesh);
    if (results.m_shchi2[i] < results.m_shchi2[ipeak]) ipeak=i;
  }
  results.m_peakshchi2=results.m_shifts[ipeak];

  // If no peak Chi^2 found, send log message and return 0
  if (ipeak == 0 || ipeak == (results.m_nshift-1)) {

    AH_DEBUG << "No peak Chi^2 found; returning zero error.  Try increasing Chi^2 shift range." << std::endl;
    for (int i=0; i < results.m_nshift; i++) results.m_shchi2[i]=0.;
    results.m_peakshlike=0.;
    results.m_sig1shchi2=0.;
    results.m_sig2shchi2=0.;
    results.m_sigshchi2=0.;

  } else {

    // find intercepts at chi2[ipeak]+1 which corresponds to 1 sigma
    double chi2chk=results.m_shchi2[ipeak]+1.;

    // to smaller shifts
    int idx=ipeak-1;
    while (results.m_shchi2[idx] < chi2chk) idx--;
    double slope=(results.m_shchi2[idx+1]-results.m_shchi2[idx])/(results.m_shifts[idx+1]-results.m_shifts[idx]);
    results.m_sig1shchi2=results.m_peakshchi2-((chi2chk-results.m_shchi2[idx])/slope+results.m_shifts[idx]);

    // to larger shifts
    idx=ipeak+1;
    while (results.m_shchi2[idx] < chi2chk) idx++;
    slope=(results.m_shchi2[idx]-results.m_shchi2[idx-1])/(results.m_shifts[idx]-results.m_shifts[idx-1]);
    results.m_sig2shchi2=(chi2chk-results.m_shchi2[idx-1])/slope+results.m_shifts[idx-1]-results.m_peakshchi2;

    // average
    results.m_sigshchi2=0.5*(results.m_sig1shchi2+results.m_sig2shchi2);
  }

  // free working array
  delete [] tprof;
  tprof=0;
}

// ****************************************************************************

bool constructWidthArray(FitResults& results, double profwidth, double rfactor) {

  // Confirm that output arrays have been allocated.
  if (results.m_widths == 0) AH_THROW_LOGIC("Output results.m_width array needs to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nwidth; i++) results.m_widths[i]=0.;

  // Determine fitted width by removing intrinsic profile width
  double outsigma=ahmath::convertFWHM2sigma(results.m_fit_width);
  double fwidth=std::sqrt(outsigma*outsigma-profwidth*profwidth);
  if (fwidth <= 0.) {
    AH_DEBUG << "Zero width range; cannot compute likelihood." << std::endl;
    return false;
  }

  // Determine range of width values to evaluate likelihood function at
  double firstwidth=fwidth/rfactor;
  double lastwidth=fwidth*rfactor;
  double dwidth=(lastwidth-firstwidth)/(results.m_nwidth-1);

  // Fill array of width values
  for (int i=0; i < results.m_nwidth; i++) results.m_widths[i]=firstwidth+dwidth*i;
  return true;
}

// ****************************************************************************

void computeLikelihoodWidth(FitResults& results, const CalProfile& profdat,
                            double profwidth) {

  // Confirm that output arrays have been allocated.
  if (results.m_widths == 0 || results.m_wdlike == 0)
    AH_THROW_LOGIC("Output results arrays, m_widths & m_wdlike, need to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nwidth; i++) results.m_wdlike[i]=0.;

  // Create working arrays
  double* t1prof=new double[results.m_nmesh];
  double* t2prof=new double[results.m_nmesh];
  for (int i=0; i < results.m_nmesh; i++) {
    t1prof[i]=0.;
    t2prof[i]=0.;
  }

  // The results array may be dimensioned larger than the data it contains, so
  // the m_nmesh value should not be used.  Determine the number of valid mesh
  // points by ignoring all trailing mesh values equal to zero.  The approach
  // here looks for the mesh point whose value is less than the previous.
  int nmesh=1;
  while (nmesh < (results.m_nmesh-1) && results.m_mesh[nmesh] > results.m_mesh[nmesh-1]) {
    nmesh++;
  }

  // Compute log likelihood.
  int ipeak=0;                          // index of peak likelihood
  for (int i=0; i < results.m_nwidth; i++) {

    // get profile with sample width
    double width=std::sqrt(profwidth*profwidth+results.m_widths[i]*results.m_widths[i]);
    double shift=results.m_fit_shift-profdat.m_offset;       // if centerprof=yes, remove profile offset from fitted shift so it can be represented on the mesh
    ahmath::convolveWithGaussianFixed(profdat.m_mesh,profdat.m_prof,nmesh,width,t1prof);
    ahgain::applyShiftParameters(profdat.m_mesh,t1prof,nmesh,
            shift,results.m_fit_scale,results.m_fit_bgrnd, results.m_fit_slope,
            t2prof);

    double asum=0.;
    for (int j=0; j < nmesh; j++) {
      if (t2prof[j] < 0.) continue;
      asum+=results.m_bindat[j]*std::log(t2prof[j]);
    }
    results.m_wdlike[i]=asum;
    if (results.m_wdlike[i] > results.m_wdlike[ipeak]) ipeak=i;
  }
  results.m_peakwdlike=results.m_widths[ipeak];

  // If no peak likelihood found, send log message and return 0
  if (ipeak == 0 || ipeak == (results.m_nwidth-1)) {

    AH_DEBUG << "No peak likelihood found; returning zero error.  Try increasing likelihood width range." << std::endl;
    for (int i=0; i < results.m_nwidth; i++) results.m_wdlike[i]=0.;
    results.m_peakwdlike=0.;
    results.m_sigwdlike=0.;

  } else {

    // Normalize log likelihood
    double dwidth=results.m_widths[1]-results.m_widths[0];        // spacing of widths array (assumed constant)
    double peakloglike=results.m_wdlike[ipeak];                   // maximum value of log likelihood
    double asum=0.;
    for (int i=0; i < results.m_nwidth; i++) asum+=exp(results.m_wdlike[i]-peakloglike)*dwidth;
    for (int i=0; i < results.m_nwidth; i++) {
      results.m_wdlike[i]=(results.m_wdlike[i]-peakloglike)-std::log(asum);
    }

    // Compute variance and sigma
    asum=0.;
    for (int i=0; i < results.m_nwidth; i++) {
      double dw=results.m_widths[i]-results.m_peakwdlike;
      asum+=exp(results.m_wdlike[i])*dw*dw;
    }
    double var=asum*dwidth;
    results.m_sigwdlike=sqrt(var);

    // The returned peak width should include the width of the profile.
    results.m_peakwdlike=std::sqrt(profwidth*profwidth+results.m_peakwdlike*results.m_peakwdlike);

    // convert log(likelihood) to likelihood
    for (int i=0; i < results.m_nwidth; i++) {
      results.m_wdlike[i]=exp(results.m_wdlike[i]);
    }
  }

  // free working array
  delete [] t1prof;
  delete [] t2prof;
  t1prof=0;
  t2prof=0;
}

// ****************************************************************************

void computeChi2WidthUncertainty(FitResults& results, const CalProfile& profdat,
                                 double profwidth) {

  // Confirm that output arrays have been allocated.
  if (results.m_widths == 0 || results.m_wdchi2 == 0)
    AH_THROW_LOGIC("Output results arrays, m_widths & m_wdchi2, need to be pre-allocated.");

  // Reset output arrays
  for (int i=0; i < results.m_nwidth; i++) results.m_wdchi2[i]=0.;

  // Create working arrays
  double* t1prof=new double[results.m_nmesh];
  double* t2prof=new double[results.m_nmesh];
  for (int i=0; i < results.m_nmesh; i++) {
    t1prof[i]=0.;
    t2prof[i]=0.;
  }

  // The results array may be dimensioned larger than the data it contains, so
  // the m_nmesh value should not be used.  Determine the number of valid mesh
  // points by ignoring all trailing mesh values equal to zero.  The approach
  // here looks for the mesh point whose value is less than the previous.
  int nmesh=1;
  while (nmesh < (results.m_nmesh-1) && results.m_mesh[nmesh] > results.m_mesh[nmesh-1]) {
    nmesh++;
  }

  // Compute Chi^2 values
  int ipeak=0;                          // index of peak Chi^2
  for (int i=0; i < results.m_nwidth; i++) {

    // get profile with sample width
    double width=std::sqrt(profwidth*profwidth+results.m_widths[i]*results.m_widths[i]);
    double shift=results.m_fit_shift-profdat.m_offset;       // if centerprof=yes, remove profile offset from fitted shift so it can be represented on the mesh
    ahmath::convolveWithGaussianFixed(profdat.m_mesh,profdat.m_prof,nmesh,width,t1prof);
    ahgain::applyShiftParameters(profdat.m_mesh,t1prof,nmesh,
            shift,results.m_fit_scale,results.m_fit_bgrnd,results.m_fit_slope,
            t2prof);

    results.m_wdchi2[i]=ahmath::calcChi2(results.m_bindat,t2prof,nmesh);
    if (results.m_wdchi2[i] < results.m_wdchi2[ipeak]) ipeak=i;
  }
  results.m_peakwdchi2=results.m_widths[ipeak];

  // If no peak likelihood found, send log message and return 0
  if (ipeak == 0 || ipeak == (results.m_nwidth-1)) {

    AH_DEBUG << "No peak Chi^2 found; returning zero error.  Try increasing Chi^2 width range." << std::endl;
    for (int i=0; i < results.m_nwidth; i++) results.m_wdchi2[i]=0.;
    results.m_peakwdchi2=0.;
    results.m_sig1wdchi2=0.;
    results.m_sig2wdchi2=0.;
    results.m_sigwdchi2=0.;

  } else {

    // find intercepts at chi2[ipeak]+1 which corresponds to 1 sigma
    double chi2chk=results.m_wdchi2[ipeak]+1.;

    // to smaller widths
    int idx=ipeak-1;
    while (idx > 0 && results.m_wdchi2[idx] < chi2chk ) idx--;
    double slope=(results.m_wdchi2[idx+1]-results.m_wdchi2[idx])/(results.m_widths[idx+1]-results.m_widths[idx]);
    results.m_sig1wdchi2=results.m_peakwdchi2-((chi2chk-results.m_wdchi2[idx])/slope+results.m_widths[idx]);

    // to larger widths
    idx=ipeak+1;
    while (idx > results.m_nwidth && results.m_wdchi2[idx] < chi2chk) idx++;
    slope=(results.m_wdchi2[idx]-results.m_wdchi2[idx-1])/(results.m_widths[idx]-results.m_widths[idx-1]);
    results.m_sig2wdchi2=(chi2chk-results.m_wdchi2[idx-1])/slope+results.m_widths[idx-1]-results.m_peakwdchi2;

    // average
    results.m_sigwdchi2=0.5*(results.m_sig1wdchi2+results.m_sig2wdchi2);

    // The returned peak width should include the width of the profile.
    results.m_peakwdchi2=std::sqrt(profwidth*profwidth+results.m_peakwdchi2*results.m_peakwdchi2);
  }

  // free working array
  delete [] t1prof;
  delete [] t2prof;
  t1prof=0;
  t2prof=0;
}

// ****************************************************************************

}  // namespace ahgain


/* Revision Log
 $Log: ahgain.cxx,v $
 Revision 1.27  2016/08/18 15:12:55  mwitthoe
 ahgain library: fix array-overrun bug

 Revision 1.26  2016/05/10 17:32:06  asargent
 Added boundary condition checks to SIGWDCHI2 calculation

 Revision 1.25  2016/04/07 21:28:53  mwitthoe
 ahgain library: change some INFO messages to DEBUG

 Revision 1.24  2015/11/17 16:46:19  mwitthoe
 ahgain library: write keywords to both output extensions of drift file

 Revision 1.23  2015/11/17 15:43:08  mwitthoe
 ahgain library: fix shift and correction factor values when centerprof is enabled

 Revision 1.22  2015/10/28 18:26:36  mwitthoe
 ahgain library refactoring: include all uncertainty variables into the FitResults structure to simplify gain fitting tools

 Revision 1.21  2015/10/27 19:05:47  mwitthoe
 ahgain library: add function for resetting uncertainty variables

 Revision 1.20  2015/10/27 16:32:36  mwitthoe
 ahgain library: create separate routines to construct the shift and width arrays used in the uncertainty functions; this to reduce code duplication since the likelihood and Chi^2 methods use the same arrays

 Revision 1.19  2015/10/27 14:30:16  mwitthoe
 ahgain library: add functions for computing shift/width uncertainties using Chi^2

 Revision 1.18  2015/10/16 20:01:00  mwitthoe
 ahgain library bug-fix: when the search window is so large as to allow multiple features within the search window, the updated search window may contain zero counts (as the result of being placed in between the features).  This causes a divide-by-zero computation leading to an infinite loop.  Now, when no counts are found in the updated search window, the output AVGBIN and COR_AVE columns contain the full-window values.  When this occurrs, a log message is written describing the situation and suggesting a workaround.

 Revision 1.17  2015/10/13 19:39:12  mwitthoe
 ahgain library: add functions for computing the likelihood functions for the fitted shift and width parameters

 Revision 1.16  2015/09/23 20:26:59  mwitthoe
 ahgain library: write results from average method even if fitting failed (fitEvents routine); see issue 554

 Revision 1.15  2015/08/12 13:59:43  klrutkow
 changed AH_WARN to AH_INFO

 Revision 1.14  2015/08/11 18:37:25  mwitthoe
 ahgain library: remove a couple commented-out std::cout statements

 Revision 1.13  2015/03/18 17:36:05  mwitthoe
 ahgain library: change DETNAME to DETNAM

 Revision 1.12  2015/03/11 16:01:18  mwitthoe
 ahgain library: minor documentation improvements

 Revision 1.11  2015/03/10 14:45:17  mwitthoe
 ahgain library: add sloped background fitting to gain library; generalized fitting routines to support 3 background modes: none, constant, sloped

 Revision 1.10  2015/01/21 16:10:13  mwitthoe
 ahgain library: add argument to constructCalibrationProfile() to allow the theoretical profile to be centered in the energy mesh; add functions for fitting with a sloped background (these are for testing purposes only)

 Revision 1.9  2014/11/12 13:12:21  mwitthoe
 ahgain library: in createGainOutput(), add parameter to specify whether or not the Drift_energy extension is made

 Revision 1.8  2014/10/02 01:18:29  mwitthoe
 ahgain library: add NULL flag for event TIME column; check if TIME is NULL before trying to fit events; see issue 445

 Revision 1.7  2014/10/02 00:52:09  mwitthoe
 ahgain library: add FITPROF column to output file which contains the theoretical profile with the fitted parameters applied; this column can be plotted against the SPECTRUM column to visually see how good the fit was; see issue 445

 Revision 1.6  2014/10/02 00:12:07  mwitthoe
 ahgain library: change output fitted width from sigma to FWHM; see issue 445

 Revision 1.5  2014/08/18 20:40:12  mwitthoe
 ahgain library: change how variance is calculated in order to improve precision agreement between systems; see issue 423

 Revision 1.4  2014/08/07 17:38:44  mwitthoe
 ahgain library: modify constructCalibrationProfile() to also perform the convolution; this function will check if the mesh is too coarse to accurately represent the natural line widths and, if so, construct the profile using only the Gaussian width

 Revision 1.3  2014/08/06 20:34:55  mwitthoe
 ahgain library: correct keywords in output file; compute EXPOSURE column; change DELTATIME column to TELAPSE; write group averages to output file

 Revision 1.2  2014/07/21 15:36:53  mwitthoe
 ahgain: correct the calculation of the fitted width; previous version was using the FWHM in eV from the broadening parameter instead of the standard deviation in channel units

 Revision 1.1  2014/07/17 19:47:20  mwitthoe
 add ahgain library which contains routines for fitting event data with a calibration feature


*/
