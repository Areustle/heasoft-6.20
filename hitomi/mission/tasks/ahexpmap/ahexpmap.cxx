/// \file ahexpmap.cxx
/// \brief Make exposure maps for SXS and SXI
/// \author Robert Hill
/// \date $Date: 2016/08/30 19:03:50 $

/**

\defgroup tool_ahexpmap Make SXI exposure map (sxsexpmap)
@ingroup mod_mission_tasks

File name: ahexpmap.cxx
Author:    Robert S. Hill
$Date: 2016/08/30 19:03:50 $
Version: 1.0

This program creates an output file containing (1) the histogram of 
offaxis angle and (2) the exposure maps for a given Astro-H observation.

Library dependencies:

  astroh/mission/lib/imagetranslib
  astroh/mission/lib/arfgenlib
  attitude/lib/coord
  attitude/lib/coordfits
  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahgain

Modification history:

 Ver   Date        Author  Description
 1.0   2016-01-20  RSH     Initial implementation including FOC and SKY transform
                           and efficiency and exposure

*/

#define AHLABEL tool_ahexpmap
#define AHCVSID "$Id: ahexpmap.cxx,v 1.62 2016/08/30 19:03:50 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

extern "C" {
#include "imagetranslib/image.h"
#include "imagetranslib/methods.h"
#include "imagetranslib/param.h"
#include "comboxform.h"
#include "coordfits.h"
}
#include "arfgenlib/arfgenlib.h"
#include "ahgen/ahgen.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"
#include "ahmission/ahmission.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <vector>  // standard container
#include <cmath>   // math functions
#include <iomanip> // setprecision for streams
#include <fstream> // for reading text files
#include <sstream> // for formatting strings
#include <cstdlib>

/** \addtogroup tool_ahexpmap
 *  @{
 */

namespace ahexpmap {

  const int ROLL_SIGN = -1;

  const int RAD2ARCMIN = 180.0*60.0/M_PI;

  const std::string DEFGTI = "GTI";
  const std::string DEFPIXGTI = "PIXELS";
  const std::string SXSDEFPIXGTI = "GTIPIXELOFF";

  // User parameters

  struct Params {
    std::string m_ehkfile;
    std::string m_gtifile;
    std::string m_instrume;
    std::string m_badimgfile;
    std::string m_pixgtifile;
    std::string m_outfile;
    std::string m_outmaptype;
    double m_delta;
    long m_numphi;
    std::string m_stopsys;
    std::string m_instmap;
    std::string m_qefile;
    std::string m_contamifile;
    std::string m_vigfile;
    std::string m_obffile;
    std::string m_fwfile;
    std::string m_gvfile;
    bool m_maskcalsrc;
    std::string m_fwtype;
    std::string m_actual_fwtype;
    std::string m_specmode;
    std::string m_specform;
    std::string m_specfile;
    std::string m_energy;
    std::string m_evperchan;
    std::string m_abund;
    std::string m_cols;
    std::string m_covfac;
    Params() : m_ehkfile(""), m_gtifile(""), m_instrume(""), m_badimgfile(""), 
      m_pixgtifile(""), m_outfile(""), m_outmaptype(""), m_delta(0.0), 
      m_numphi(0), m_stopsys(""), m_instmap(""), m_qefile(""), m_contamifile(""), 
      m_vigfile(""), m_obffile(""), m_fwfile(""), m_gvfile(""),  
      m_maskcalsrc(false), m_fwtype(""), m_actual_fwtype(""), m_specmode(""), m_specform(""), 
      m_specfile(""), m_energy(""), m_evperchan(""), 
      m_abund(""), m_cols("") {}
  };

  // OFFAXISHIST keywords

  struct KeywordValues {
    std::string m_telescop;
    std::string m_instrume;
    std::string m_detnam;
    std::string m_dateobs;
    double m_tstart;
    double m_tstop;
    double m_ra_src;
    double m_dec_src;
    double m_ra_pnt;
    double m_dec_pnt;
    double m_ra_nom;
    double m_dec_nom;
    double m_pa_nom;
    double m_optaxisx;
    double m_optaxisy;
    double m_opt_rotd;
    double m_raw_xscl;
    double m_raw_yscl;
    double m_act_xscl;
    double m_act_yscl;
    double m_det_xscl;
    double m_det_yscl;
    double m_focallen;
    double m_foc_xscl;
    double m_foc_yscl;
    long m_raw_xsiz;
    long m_act_xsiz;
    long m_act_ysiz;
    long m_det_xsiz;
    long m_det_ysiz;
    long m_detxflip;
    long m_detyflip;
    long m_foc_xsiz;
    long m_foc_ysiz;
    double m_foc_xoff;
    double m_foc_yoff;
    double m_foc_rotd;
    long m_optxflip;
    long m_optyflip;
    double m_cendet;           // DET coordsys center
    double m_cenfoc;           // FOC coordsys center
    double m_detscaleratio;    // Ratio of ACT to DET pixel sizes
    double m_focscaleratio;    // Ratio of DET to FOC pixel sizes
    double m_censky;           // SKY coordsys center, X
    double m_pixels_per_radian; // SKY inverse plate scale
    double m_xinc_sky;         // SKY plate scale, X [deg/pixel]
    double m_yinc_sky;         // SKY plate scale, Y [deg/pixel]
    long m_numtheta;           // Number of unique theta annuli
    std::string m_ctype1;
    std::string m_cunit1;
    double m_crval1;
    double m_crpix1;
    double m_cdelt1;
    std::string m_ctype2;
    std::string m_cunit2;
    double m_crval2;
    double m_crpix2;
    double m_cdelt2;
    double m_sxspxwid;
    double m_sxspxgap;
    std::string m_gatevalv;
    std::string m_filter;
    KeywordValues() :
      m_telescop(""), m_instrume(""), m_detnam(""), m_dateobs(""),
      m_tstart(0.0), m_tstop(0.0), 
      m_ra_src(0.0), m_dec_src(0.0), m_ra_pnt(0.0), m_dec_pnt(0.0), 
      m_ra_nom(0.0), m_dec_nom(0.0), m_pa_nom(0.0),
      m_optaxisx(0.0), m_optaxisy(0.0), m_opt_rotd(0.0),
      m_raw_xscl(0.0), m_raw_yscl(0.0),
      m_act_xscl(0.0), m_act_yscl(0.0),
      m_det_xscl(0.0), m_det_yscl(0.0), m_focallen(0.0),
      m_foc_xscl(0.0), m_foc_yscl(0.0),
      m_raw_xsiz(0), 
      m_act_xsiz(0), m_act_ysiz(0), m_det_xsiz(0), m_det_ysiz(0),
      m_detxflip(0), m_detyflip(0),
      m_foc_xsiz(0), m_foc_ysiz(0),
      m_foc_xoff(0.0), m_foc_yoff(0.0), m_foc_rotd(0.0), 
      m_optxflip(0), m_optyflip(0),
      m_cendet(0.0), m_cenfoc(0.0), 
      m_detscaleratio(0.0),
      m_focscaleratio(0.0),
      m_censky(0.0), m_pixels_per_radian(0.0),
      m_xinc_sky(0.0), m_yinc_sky(0.0), m_numtheta(0),
      m_ctype1(""), m_cunit1(""), m_crval1(0.0), m_crpix1(0.0), m_cdelt1(0.0),
      m_ctype2(""), m_cunit2(""), m_crval2(0.0), m_crpix2(0.0), m_cdelt2(0.0),
      m_sxspxwid(0.0), m_sxspxgap(0.0),
      m_gatevalv(""), m_filter("") {}
  };

  // Pointing data from EHK file

  struct EHKrow {
    double m_time;
    double m_ra;
    double m_dec;
    double m_roll;
    double m_ra_pnt;
    double m_dec_pnt;
    EHKrow() : 
      m_time(0), m_ra(0), m_dec(0), m_roll(0), m_ra_pnt(0), 
      m_dec_pnt(0) {}
  };

  // GTIs from overall GTI file

  struct GTIData {
    std::vector<double> m_start;
    std::vector<double> m_stop;
    long m_num_gti;
    GTIData() : m_num_gti(0) { }
  };

  // GTIs to be written for each offaxis bin

  struct OffAxisGTI {
    std::vector<double> m_theta_bin;
    std::vector<double> m_start;
    std::vector<double> m_stop;
    long m_num_gti;
    OffAxisGTI() : m_num_gti(0) { }
  };

  // Status indicators for transition from one offaxis
  // bin or GTI to the next

  enum PreviewGTI {
    e_PREVIEW_NOT_IN_GTI,
    e_PREVIEW_IN_GTI
  };

  enum GTIStatus {
    e_NOT_IN_GTI,
    e_IN_GTI_AND_OFFAXIS_BIN,
    e_NEW_OFFAXIS_BIN_SAME_GTI
  };

  const std::string bin_status_msg[3] = {
    "NOT_IN_GTI",
    "IN_GTI_AND_OFFAXIS_BIN",
    "NEW_OFFAXIS_BIN_SAME_GTI"
  };

  // Flags for good/bad/inactive pixels
  enum PixFlag {
    e_PIX_INACT,
    e_PIX_GOOD,
    e_PIX_BAD,
    e_PIX_PARTIAL
  };
}

/// \brief Get parameter values
/// \param[out] par   Structure containing user parameters
void getPar(ahexpmap::Params & par);

/// \brief Initialize the run
/// \param[in]   par     Structure containing user parameters
/// \param[out]  fpehk   Pointer to EHK file
/// \param[out]  rehk    Column router for EHK file
/// \param[out]  ehkrow  Structure containing EHK row data
/// \param[out]  keywd   Coordinate keywords
/// \param[out]  gti_data     Data from GTI file
/// \param[out]  min_theta_bin Lowest bin number for offaxis angle
/// \param[out]  ngrid_theta  Number of offaxis bins
/// \param[out]  ngrid_wedge  Total number of wedge bins (theta and phi)
/// \param[out]  npix    Number of pixels with GTI info
/// \param[out]  expo    Exposure map array
/// \param[out]  ave_ra       Mean RA of pointing per bin
/// \param[out]  ave_dec      Mean Dec of pointing per bin
/// \param[out]  ave_roll     Mean roll per bin
/// \param[out]  ave_ra_pnt   Mean optical axis RA per bin
/// \param[out]  ave_dec_pnt  Mean optical axis Dec per bin
/// \param[out]  ave_theta    Mean offaxis angle per bin
/// \param[out]  ave_phi      Mean azimuth per bin
/// \param[out]  do_efficiency If true, output map includes efficiencies
/// \param[out]  effimage     DET efficiency image, computed once
void initialize(ahexpmap::Params & par,
  ahfits::FilePtr& fpehk, ahfits::Router*& rehk, ahexpmap::EHKrow& ehkrow,
  ahexpmap::KeywordValues& keywd, ahexpmap::GTIData& gti_data, 
  long& min_theta_bin, long& ngrid_theta, long& ngrid_wedge, long& npix,
  std::vector<std::vector<double> >& expo,
  std::vector<double>& ave_ra, std::vector<double>& ave_dec,
  std::vector<double>& ave_theta, std::vector<double>& ave_phi, std::vector<double>& ave_roll, 
  std::vector<double>& ave_ra_pnt, std::vector<double>& ave_dec_pnt,
  bool& do_efficiency, std::string & actual_instmap,
  std::vector<std::vector<double> > & effimage);

/// \brief Do the work of the run
/// \param[in]   par     Structure containing user parameters
/// \param[in]   fpehk   Pointer to EHK file
/// \param[in]   rehk    Column router for EHK file
/// \param[in]   ehkrow  Structure containing EHK row data
/// \param[in]   keywd   Coordinate keywords
/// \param[out]  fpbad   Pointer to instrument map
/// \param[out]  fpout   Pointer to instrument map
/// \param[in]   gti_data     Data from GTI file
/// \param[in]  min_theta_bin Lowest bin number for offaxis angle
/// \param[in]   ngrid_theta  Number of offaxis bins
/// \param[out]  ngrid_wedge  Total number of wedge bins (theta and phi)
/// \param[in]   npix    Number of pixels with GTI info
/// \param[in]   expo    Exposure map array
/// \param[in]   ave_ra       Mean RA of pointing per bin
/// \param[in]   ave_dec      Mean Dec of pointing per bin
/// \param[in]   ave_roll     Mean roll per bin
/// \param[in]   ave_ra_pnt   Mean optical axis RA per bin
/// \param[in]   ave_dec_pnt  Mean optical axis Dec per bin
/// \param[in]   ave_theta    Mean offaxis angle per bin
/// \param[in]   ave_phi      Mean azimuth per bin
/// \param[in]   do_efficiency If true, output map includes efficiencies
/// \param[in]   effimage     DET efficiency image, computed once
void doWork(ahexpmap::Params & par,
  ahfits::FilePtr fpehk, ahfits::Router* rehk, ahexpmap::EHKrow& ehkrow,
  ahexpmap::KeywordValues& keywd, 
  ahfits::FilePtr& fpbad, ahfits::FilePtr& fpout, ahexpmap::GTIData& gti_data, 
  long min_theta_bin, long ngrid_theta, long ngrid_wedge, long npix,
  std::vector<std::vector<double> >& expo,
  std::vector<double>& ave_ra, std::vector<double>& ave_dec, std::vector<double>& ave_roll, 
  std::vector<double>& ave_ra_pnt, std::vector<double>& ave_dec_pnt, 
  std::vector<double>& ave_theta, std::vector<double>& ave_phi, 
  const bool do_efficiency, const std::string & actual_instmap, 
  std::vector<std::vector<double> > & effimage);

/// \brief Clean up from the run
/// \param[in]   fpehk   Pointer to EHK file
/// \param[in]   rehk    Column router for EHK file
/// \param[in]   ehkrow  Structure containing EHK row data
/// \param[in]   fpbad   Pointer to instrument map
/// \param[in]   fpout   Pointer to instrument map
void finalize(ahfits::FilePtr fpehk, ahfits::Router* rehk, 
  ahfits::FilePtr fpbad, ahfits::FilePtr fpout);

/// \brief Calculate the efficiency map in DET
/// \param[in]   keywd   Coordinate keywords
/// \param[out]  effimage  DET efficiency image
void calcEfficiencyMap(ahexpmap::Params& par, ahexpmap::KeywordValues& keywd, 
  std::vector<std::vector<double> >& effimage);

/// \brief Calculate the theta and phi for EHK row
/// \param[in]   ra_src  RA of observation target
/// \param[in]   ra_pnt  RA of optical axis
/// \param[in]   ra_nom  RA of pointing
/// \param[in]   dec_src Dec of observation target
/// \param[in]   dec_pnt Dec of optical axis
/// \param[in]   dec_nom Dec of pointing
/// \param[in]   ra_nom  Roll of pointing
/// \param[out]  theta   Offaxis angle
/// \param[out]  phi     Aximuth angle
void calcDeltaAttitude(double ra_src, double ra_pnt, double ra_nom, 
  double dec_src, double dec_pnt, double dec_nom, double pa_nom,
  double& theta, double& phi);

/// \brief       Get the wedge array index given theta and phi
/// \param[in]   theta   Offaxis angle
/// \param[in]   phi     Aximuth angle
/// \param[in]   min_theta_bin Lowest theta bin represented in data
/// \param[in]   delta   Theta bin width
/// \param[in]   numphi  Number of phi bins for innermost annulus
/// \param[out]  index   (theta,phi) bin index
void getIndexFromWedge(double theta, double phi, long min_theta_bin, 
  double m_delta, long m_numphi, long& index);

/// \brief       Get the (theta,phi) bin bounds from wedge index
/// \param[in]   index   (theta,phi) bin index
/// \param[in]   min_theta_bin Lowest theta bin represented in data
/// \param[in]   delta   Theta bin width
/// \param[in]   numphi  Number of phi bins for innermost annulus
/// \param[in]   theta_min  Lowest offaxis angle of bin
/// \param[in]   theta_max  Highest offaxis angle of bin
/// \param[in]   phi_min Lowest azimuthal angle of bin
/// \param[in]   phi_max Highest azimuthal angle of bin
void getWedgeFromIndex(long index, long min_theta_bin, double delta, long numphi, 
  double& theta_min, double& theta_max, double& phi_min, double& phi_max);

/// \brief  Compute the image transformation from DET to FOC or SKY
/// \param[in]  stopsys            "FOC" or "SKY"
/// \param[out] xform              pointer to Xform2d describinng transform
/// \param[in]  cendet             DET center (X and Y)
/// \param[in]  foc_xoff           FOC_XOFF from teldef
/// \param[in]  foc_yoff           FOC_YOFF from teldef
/// \param[in]  focscaleratio      Ratio of DET to FOC scales
/// \param[in]  foc_rotd           FOC_ROTD from teldef
/// \param[in]  cenfoc             FOC center (X and Y)
/// \param[in]  roll_sign          Roll sign convention
/// \param[in]  ra_nom             RA center of SKY
/// \param[in]  dec_nom            DEC center of SKY
/// \param[in]  ra_exp             RA_NOM of exposure map component
/// \param[in]  dec_exp            DEC_NOM of exposure map component
/// \param[in]  pa_exp             PA_NOM of exposure map component
/// \param[in]  censky             SKY center (X and Y)
/// \param[in]  pixels_per_radian  SKY inverse plate scale
void computeXform2dDetToFocOrSky (std::string & stopsys, XFORM2D* xform, 
  const double cendet, const double foc_xoff, const double foc_yoff,
  const double focscaleratio, const double foc_rotd, const double cenfoc,
  const double roll_sign, const double ra_nom, const double dec_nom,
  const double ra_exp, const double dec_exp, const double pa_exp,
  const double censky, const double pixels_per_radian);

// ****************************************************************************

/// \brief ahexpmap tool
int main(int argc, char** argv) {

  ahexpmap::Params par;
  
  //  FITS file pointers

  ahfits::FilePtr fpehk = 0;   // Attitude
  ahfits::FilePtr fpbad = 0;   // Bad pixel image
  ahfits::FilePtr fpout = 0;   // Output exposure map
  ahfits::Router* rehk = 0;    // Router to manage attitude

  ahexpmap::KeywordValues keywd;   // Coordinate keywords
  ahexpmap::GTIData gti_data;      // Data from GTI file
  std::vector<std::vector<double> > expo;   // Exposure time array
  std::vector<std::vector<double> > effimage;  // Efficiency image
  std::vector<double> ave_ra;      // Mean RA per bin
  std::vector<double> ave_dec;     // Mean Dec per bin
  std::vector<double> ave_roll;    // Mean roll per bin
  std::vector<double> ave_ra_pnt;  // Mean optical axis RA per bin
  std::vector<double> ave_dec_pnt; // Mean optical axi Dec per bin
  std::vector<double> ave_theta;   // Mean offaxis angle per bin
  std::vector<double> ave_phi;     // Mean azimuth per bin
  ahexpmap::EHKrow ehkrow;         // Attitude data from row of EHK file

  bool do_efficiency;              // Flag to apply QE, contamination, vignetting
  std::string actual_instmap = ""; // Instrument map after CALDB resolution

  long ngrid_theta=0;              // Number of offaxis bins
  long ngrid_wedge=0;              // Number of offaxis bins
  long min_theta_bin=0;            // Bin number of minimum offaxis angle 
  long npix=0;                     // Number of flickering pixels

  int status = ahapp::startUp(argc, argv, TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par, fpehk, rehk, ehkrow, keywd, gti_data, 
        min_theta_bin, ngrid_theta, ngrid_wedge, npix,
        expo, ave_ra, ave_dec, 
        ave_theta, ave_phi, ave_roll, ave_ra_pnt, ave_dec_pnt,
        do_efficiency, actual_instmap, effimage);
      doWork(par, fpehk, rehk, ehkrow, keywd, fpbad, fpout, gti_data, 
        min_theta_bin, ngrid_theta, ngrid_wedge, npix, expo, ave_ra, ave_dec,
        ave_roll, ave_ra_pnt, ave_dec_pnt, ave_theta, ave_phi,
        do_efficiency, actual_instmap, effimage);
      finalize(fpehk, rehk, fpbad, fpout);        
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fpehk, rehk, ehkrow, keywd, gti_data, 
          min_theta_bin, ngrid_theta, ngrid_wedge, npix,
          expo, ave_ra, ave_dec, 
          ave_theta, ave_phi, ave_roll, ave_ra_pnt, ave_dec_pnt,
          do_efficiency, actual_instmap, effimage);
        doWork(par, fpehk, rehk, ehkrow, keywd, fpbad, fpout, gti_data, 
          min_theta_bin, ngrid_theta, ngrid_wedge, npix, expo, ave_ra, ave_dec,
          ave_roll, ave_ra_pnt, ave_dec_pnt, ave_theta, ave_phi,
          do_efficiency, actual_instmap, effimage);
      } catch (const std::exception & x) {
          ahapp::writeParametersToLog(); 
          status = 1;
          AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpehk, rehk, fpbad, fpout);
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;

} // end main

// ****************************************************************************

void getPar(ahexpmap::Params & par) {

  //  Step 1:  Get parameters

  par.m_ehkfile = ahapp::getParString("ehkfile");
  par.m_gtifile = ahapp::getParString("gtifile");
  par.m_instrume = ahapp::getParString("instrume");
  par.m_badimgfile = ahapp::getParString("badimgfile");
  par.m_pixgtifile = ahapp::getParString("pixgtifile");
  par.m_outfile = ahapp::getParString("outfile");
  par.m_outmaptype = ahapp::getParString("outmaptype");
  par.m_delta = ahapp::getParDouble("delta"); 
  par.m_numphi = ahapp::getParDouble("numphi"); 
  par.m_stopsys = ahapp::getParString("stopsys");
  par.m_instmap = ahapp::getParString("instmap");
  par.m_qefile = ahapp::getParString("qefile");
  par.m_contamifile = ahapp::getParString("contamifile");
  par.m_vigfile = ahapp::getParString("vigfile");
  par.m_obffile = ahapp::getParString("obffile");
  par.m_fwfile = ahapp::getParString("fwfile");
  par.m_gvfile = ahapp::getParString("gvfile");
  par.m_maskcalsrc = ahapp::getParBool("maskcalsrc");
  par.m_fwtype = ahapp::getParString("fwtype");
  par.m_specmode = ahapp::getParString("specmode");
  par.m_specform = ahapp::getParString("specform");
  par.m_specfile = ahapp::getParString("specfile");
  par.m_energy = ahapp::getParString("energy");
  par.m_evperchan = ahapp::getParString("evperchan");
  par.m_abund = ahapp::getParString("abund");
  par.m_cols = ahapp::getParString("cols");
  par.m_covfac = ahapp::getParString("covfac");
}

// ****************************************************************************

void initialize(ahexpmap::Params & par,
  ahfits::FilePtr& fpehk, ahfits::Router*& rehk, ahexpmap::EHKrow& ehkrow,
  ahexpmap::KeywordValues& keywd, ahexpmap::GTIData& gti_data, 
  long& min_theta_bin, long& ngrid_theta, long& ngrid_wedge, long& npix,
  std::vector<std::vector<double> >& expo,
  std::vector<double>& ave_ra, std::vector<double>& ave_dec,
  std::vector<double>& ave_theta, std::vector<double>& ave_phi, std::vector<double>& ave_roll, 
  std::vector<double>& ave_ra_pnt, std::vector<double>& ave_dec_pnt,
  bool& do_efficiency, std::string & actual_instmap, 
  std::vector<std::vector<double> > & effimage) {

  long naxis=0, naxis1=0, naxis2=0;   // Image format
  bool image_ok = false;              // Flag for valid image format
  std::string tele = "";              // Telescope string
  std::string inst = "";              // Instrument string
  std::string defgtiext = "";         // Default extension name for GTI
  std::string defpixgtiext = "";      // Default extension name for pixel GTI

  ahfits::FilePtr fpins = 0;          // File pointer for instrumenet map
  ahfits::FilePtr fpbad = 0;          // File pointer for bad pixel map
  ahfits::FilePtr fppix = 0;          // File pointer for pixel GTI file

  ahfits::FilePtr fpgti = 0;          // File pointer for GTI file
  ahfits::Router* rgti = 0;           // FITS data router for GTI file
  double l_start = 0.0;               // Local START variable
  double l_stop = 0.0;                // Local STOP variable

  ahfits::IndexType i_gti = 0;        // Counter for GTI file rows
  ahfits::IndexType i_ehk = 0;        // Counter for EHK file rows

  double plate_scale = 0.0;           // Plate scale in focal plane [deg/pixel]
  double theta = 0.0, phi = 0.0;      // Offaxis angle and azimuth
  double max_theta = 0.0;             // Upper limit of offaxis angle
  double min_theta = 0.0;             // Lower limit of offaxis angle
  ahexpmap::PreviewGTI gti_status = ahexpmap::e_PREVIEW_NOT_IN_GTI;  // Used in prechecking offaxis limits

  std::stringstream ss;               // Used to build throw messages

  bool read_badimg = false;           // Using bad pixel image
  bool read_pixgti = false;           // Using flickering pixel GTI
  bool read_row = true;               // Read the current table row
  bool compute_theta_phi = false;     // Compute theta,phi in preview

  // Counters for paths through EHK preview

  ahfits::IndexType num_iter=0, num_preview_path0=0, num_preview_path1=0,
    num_preview_path2=0, num_preview_path3=0, num_preview_path4=0,
    num_preview_path5=0, num_preview_path6=0;

  // Step 2:  Validate parameters and input files

  // Check instrument

  tele = ahmission::getTELESCOPString();
  inst = ahgen::strtoupper(par.m_instrume);

  if (inst != "SXI" && inst != "SXS" && inst != "HXI1" && inst != "HXI2") {
    AH_THROW_RUNTIME("INSTRUME parameter " + par.m_instrume + "is invalid.  Exiting.");
  }

  // Flags for pixel characterization

  if (inst == "HXI1" || inst == "HXI2") {
    read_badimg = false;
    read_pixgti = false;
  } else {
    read_badimg = ( ahgen::strtoupper(par.m_badimgfile) != "NONE" );
    read_pixgti = ( ahgen::strtoupper(par.m_pixgtifile) != "NONE" );
  }

  // Query CALDB for instrument map

  if (inst == "HXI1" || inst == "HXI2") {
    actual_instmap = 
      ahmission::caldb::resolve(par.m_instmap, "instrument map", inst, "-", "INSTMAP_RAW", 
        "-", "-", tele);
  } else {
    actual_instmap = 
      ahmission::caldb::resolve(par.m_instmap, "instrument map", inst, "-", "INSTMAP_DET", 
        "-", "-", tele);
  }

  // Check if primary HDU of badimgfile contains two-dimensional array

  if (read_badimg) {
    ahfits::open(par.m_badimgfile, "", &fpbad);   // Checks existence

    AH_INFO(ahlog::HIGH) << "Opened bad pixel image file " << par.m_badimgfile << std::endl;
    naxis = ahfits::getKeyValLLong(fpbad, "NAXIS");

    image_ok = false;
    if (naxis == 2) {
      naxis1 = ahfits::getKeyValLLong(fpbad, "NAXIS1");
      naxis2 = ahfits::getKeyValLLong(fpbad, "NAXIS2");
      if (0 < naxis1 && 0 < naxis2) {
        image_ok = true;
      }
    }
    AH_INFO(ahlog::HIGH) << "Bad pixel image format NAXIS=" << naxis << " NAXIS1=" << naxis1
      << " NAXIS2=" << naxis2 << std::endl;
    ahfits::close(fpbad);
    AH_INFO(ahlog::HIGH) << "Closed bad pixel image file" << std::endl;
    if (!image_ok) {
      AH_THROW_RUNTIME("badimgfile primary HDU does not contain a correctly sized image.");
    }
  } else {
    AH_INFO(ahlog::HIGH) << "No bad pixel image file" << std::endl;
  }
  
  ahfits::open(actual_instmap, "", &fpins);   // Checks existence
  ahfits::move(fpins, 2);  // Move to first extension (second HDU)
  AH_INFO(ahlog::HIGH) << "Opened instrument map " << actual_instmap << std::endl;

  // Obtain required keywords (fails if keyword absent)

  keywd.m_telescop = tele;
  keywd.m_instrume = inst;
  keywd.m_act_xsiz = ahfits::getKeyValLLong(fpins, "ACT_XSIZ");
  keywd.m_act_ysiz = ahfits::getKeyValLLong(fpins, "ACT_YSIZ");
  keywd.m_act_xscl = ahfits::getKeyValDbl(fpins, "ACT_XSCL");
  keywd.m_act_yscl = ahfits::getKeyValDbl(fpins, "ACT_YSCL");
  keywd.m_det_xsiz = ahfits::getKeyValLLong(fpins, "DET_XSIZ");
  keywd.m_det_ysiz = ahfits::getKeyValLLong(fpins, "DET_YSIZ");
  keywd.m_det_xscl = ahfits::getKeyValDbl(fpins, "DET_XSCL");
  keywd.m_det_yscl = ahfits::getKeyValDbl(fpins, "DET_YSCL");
  keywd.m_detxflip = ahfits::getKeyValLLong(fpins, "DETXFLIP");
  keywd.m_detyflip = ahfits::getKeyValLLong(fpins, "DETYFLIP");
  keywd.m_foc_xsiz = ahfits::getKeyValLLong(fpins, "FOC_XSIZ");
  keywd.m_foc_ysiz = ahfits::getKeyValLLong(fpins, "FOC_YSIZ");
  keywd.m_foc_xscl = ahfits::getKeyValDbl(fpins, "FOC_XSCL");
  keywd.m_foc_yscl = ahfits::getKeyValDbl(fpins, "FOC_YSCL");
  keywd.m_foc_xoff = ahfits::getKeyValDbl(fpins, "FOC_XOFF");
  keywd.m_foc_yoff = ahfits::getKeyValDbl(fpins, "FOC_YOFF");
  keywd.m_foc_rotd = ahfits::getKeyValDbl(fpins, "FOC_ROTD");
  keywd.m_optaxisx = ahfits::getKeyValDbl(fpins, "OPTAXISX");
  keywd.m_optaxisy = ahfits::getKeyValDbl(fpins, "OPTAXISY");
  keywd.m_optxflip = ahfits::getKeyValLLong(fpins, "OPTXFLIP");
  keywd.m_optyflip = ahfits::getKeyValLLong(fpins, "OPTYFLIP");
  keywd.m_opt_rotd = ahfits::getKeyValDbl(fpins, "OPT_ROTD");
  keywd.m_focallen = ahfits::getKeyValDbl(fpins, "FOCALLEN");

  if (inst == "SXI" || inst == "SXS") {
    keywd.m_detnam = "SXT";
  } else if (inst == "HXI1" || inst == "HXI2") {
    keywd.m_detnam = "HXT";
    keywd.m_raw_xsiz = ahfits::getKeyValLLong(fpins, "NAXIS1");
  }
  
  if (inst == "HXI1" || inst == "HXI2" || inst == "SXI") {
    keywd.m_raw_xscl = ahfits::getKeyValDbl(fpins, "RAW_XSCL");
    keywd.m_raw_yscl = ahfits::getKeyValDbl(fpins, "RAW_YSCL");
  }

  if (inst == "SXS") {
    keywd.m_sxspxwid = ahfits::getKeyValDbl(fpins, "SXSPXWID");
    keywd.m_sxspxgap = ahfits::getKeyValDbl(fpins, "SXSPXGAP");
  }

  if (keywd.m_det_xsiz != keywd.m_det_ysiz) {
    AH_THROW_RUNTIME("DET not square");
  }
  if (keywd.m_foc_xsiz != keywd.m_foc_ysiz) {
    AH_THROW_RUNTIME("FOC not square");
  }
  keywd.m_cendet = (1.0 + keywd.m_det_xsiz)/2.0;
  keywd.m_cenfoc = (1.0 + keywd.m_foc_xsiz)/2.0;
  keywd.m_censky = keywd.m_cenfoc;
  keywd.m_focscaleratio = keywd.m_det_xscl/keywd.m_foc_xscl;
  keywd.m_detscaleratio = keywd.m_act_xscl/keywd.m_det_xscl;
  keywd.m_pixels_per_radian = keywd.m_focallen/keywd.m_foc_xscl;  // (focal len)/(pixel pitch) [mm]/[mm/px]
  plate_scale = (1.0/keywd.m_pixels_per_radian)*180.0/M_PI;
  keywd.m_xinc_sky = -plate_scale;  // RA increases right to left
  keywd.m_yinc_sky = +plate_scale;

  // These are from the instrument map, so should be appropriate
  // for DET (SXI/SXS) or RAW (HXI).
  keywd.m_ctype1 = ahfits::getKeyValStr(fpins, "CTYPE1");
  keywd.m_cunit1 = ahfits::getKeyValStr(fpins, "CUNIT1");
  keywd.m_crval1 = ahfits::getKeyValDbl(fpins, "CRVAL1");
  keywd.m_crpix1 = ahfits::getKeyValDbl(fpins, "CRPIX1");
  keywd.m_cdelt1 = ahfits::getKeyValDbl(fpins, "CDELT1");
  keywd.m_ctype2 = ahfits::getKeyValStr(fpins, "CTYPE2");
  keywd.m_cunit2 = ahfits::getKeyValStr(fpins, "CUNIT2");
  keywd.m_crval2 = ahfits::getKeyValDbl(fpins, "CRVAL2");
  keywd.m_crpix2 = ahfits::getKeyValDbl(fpins, "CRPIX2");
  keywd.m_cdelt2 = ahfits::getKeyValDbl(fpins, "CDELT2");

  AH_INFO(ahlog::HIGH) << "Keywords obtained from instrument map:" << std::endl;
  AH_INFO(ahlog::HIGH) << "TELESCOP = " << keywd.m_telescop << std::endl;
  AH_INFO(ahlog::HIGH) << "INSTRUME = " <<keywd.m_instrume << std::endl;
  AH_INFO(ahlog::HIGH) << "DETNAM   = " << keywd.m_detnam << std::endl;
  AH_INFO(ahlog::HIGH) << "OPTAXISX = " << keywd.m_optaxisx << std::endl;
  AH_INFO(ahlog::HIGH) << "OPTAXISY = " << keywd.m_optaxisy << std::endl;
  AH_INFO(ahlog::HIGH) << "FOCALLEN = " << keywd.m_focallen << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC_XSCL = " << keywd.m_foc_xscl << std::endl;
  AH_INFO(ahlog::HIGH) << "DET_XSIZ = " << keywd.m_det_xsiz << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC_XSIZ = " << keywd.m_foc_xsiz << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC_XOFF = " << keywd.m_foc_xoff << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC_YOFF = " << keywd.m_foc_yoff << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC_ROTD = " << keywd.m_foc_rotd << std::endl;
  AH_INFO(ahlog::HIGH) << "OPT_ROTD = " << keywd.m_opt_rotd << std::endl;
  AH_INFO(ahlog::HIGH) << "OPTXFLIP = " << keywd.m_optxflip << std::endl;
  AH_INFO(ahlog::HIGH) << "OPTYFLIP = " << keywd.m_optyflip << std::endl;
  
  if (inst == "HXI1" || inst == "HXI2") {
    AH_INFO(ahlog::HIGH) << "RAW_XSCL = " << keywd.m_raw_xscl << std::endl;
    AH_INFO(ahlog::HIGH) << "RAW_YSCL = " << keywd.m_raw_yscl << std::endl;
  } else {
    AH_INFO(ahlog::HIGH) << "DET_XSCL = " << keywd.m_det_xscl << std::endl;
    AH_INFO(ahlog::HIGH) << "DET_YSCL = " << keywd.m_det_yscl << std::endl;
  }

  AH_INFO(ahlog::HIGH) << "From instrument map:" << std::endl;
  AH_INFO(ahlog::HIGH) << "CTYPE1 = " << keywd.m_ctype1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CUNIT1 = " << keywd.m_cunit1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRVAL1 = " << keywd.m_crval1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRPIX1 = " << keywd.m_crpix1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CDELT1 = " << keywd.m_cdelt1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CTYPE2 = " << keywd.m_ctype2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CUNIT2 = " << keywd.m_cunit2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRVAL2 = " << keywd.m_crval2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRPIX2 = " << keywd.m_crpix2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CDELT2 = " << keywd.m_cdelt2 << std::endl;

  if (inst == "SXS") {
    AH_INFO(ahlog::HIGH) << "SXSPXWID = " << keywd.m_sxspxwid << std::endl;
    AH_INFO(ahlog::HIGH) << "SXSPXGAP = " << keywd.m_sxspxgap << std::endl;
  }

  AH_INFO(ahlog::HIGH) << "Values computed from the keyword values:" << std::endl;
  AH_INFO(ahlog::HIGH) << "DET center (X and Y) = " << keywd.m_cendet << std::endl;
  AH_INFO(ahlog::HIGH) << "FOC center (X and Y) = " << keywd.m_cenfoc << std::endl;
  AH_INFO(ahlog::HIGH) << "SKY center (X and Y) = " << keywd.m_censky << std::endl;
  AH_INFO(ahlog::HIGH) << "Pixels per radian = " << keywd.m_pixels_per_radian << std::endl;
  AH_INFO(ahlog::HIGH) << "SKYX increment [deg/pixel] = " << keywd.m_xinc_sky << std::endl;
  AH_INFO(ahlog::HIGH) << "SKYY increment [deg/pixel] = " << keywd.m_yinc_sky << std::endl;

  ahfits::close(fpins);
  AH_INFO(ahlog::HIGH) << "Closed instrument map" << std::endl;

  //  Set the parameter for the instrument map

  ape_trad_set_string("instmap", actual_instmap.c_str());   

  //  Validate DELTA

  if (par.m_delta <= 0) {
    ss.str("");
    ss << par.m_delta;
    AH_THROW_RUNTIME("DELTA parameter = " + ss.str() + "; should be positive.  Exiting.");
  }

  //  Validate NUMPHI

  if (par.m_numphi < 0) {
    ss.str("");
    ss << par.m_numphi;
    AH_THROW_RUNTIME("NUMPHI parameter = " + ss.str() + "; should be positive.  Exiting.");
  }

  // Step 3: read GTI into memory (required)

  defgtiext = ahexpmap::DEFGTI;
  ahfits::open(par.m_gtifile, "", &fpgti);
  // Move to default extension for instrument if
  // extended syntax not used.
  if (ahfits::isPrimary(fpgti)) ahfits::move(fpgti, defgtiext);
  AH_INFO(ahlog::HIGH) << "Opened GTI file " << par.m_gtifile << std::endl;

  // Obtain required keywords (fails if keyword absent)

  keywd.m_tstart = ahfits::getKeyValDbl(fpgti, "TSTART");
  keywd.m_tstop = ahfits::getKeyValDbl(fpgti, "TSTOP");

  AH_INFO(ahlog::HIGH) << "Keywords obtained from GTI file (GTI extension):" << std::endl;
  ss.str("");
  ss << std::setprecision(15) << "TSTART   = " << keywd.m_tstart;
  AH_INFO(ahlog::HIGH) << ss.str() << std::endl;
  ss.str("");
  ss << std::setprecision(15) << "TSTOP    = " << keywd.m_tstop;
  AH_INFO(ahlog::HIGH) << ss.str() << std::endl;

  // Read GTI columns

  rgti = new ahfits::Router(fpgti);
  rgti->connectScalar(ahfits::e_READONLY, "START", l_start);
  rgti->connectScalar(ahfits::e_READONLY, "STOP" , l_stop);

  gti_data.m_num_gti = 0;
  for (ahfits::firstRow(fpgti); ahfits::readOK(fpgti); ahfits::nextRow(fpgti)) {
    ahfits::readRow(fpgti);
    gti_data.m_start.push_back(l_start);
    gti_data.m_stop.push_back(l_stop);
    ++gti_data.m_num_gti;
  }
  AH_INFO(ahlog::HIGH) << "Read " << gti_data.m_num_gti << " GTI rows" << std::endl;

  // Get special SXS keywords

  if (inst == "SXS") {
    ahfits::move(fpgti, "EVENTS");
    keywd.m_gatevalv = ahfits::getKeyValStr(fpgti, "GATEVALV");
    keywd.m_filter = ahfits::getKeyValStr(fpgti, "FILTER");
    AH_INFO(ahlog::HIGH) << "Keywords obtained from GTI file (EVENTS extension):" << std::endl;
    AH_INFO(ahlog::HIGH) << "GATEVALV = " << keywd.m_gatevalv << std::endl;
    AH_INFO(ahlog::HIGH) << "FILTER   = " << keywd.m_filter << std::endl;
    if (ahgen::strtoupper(par.m_fwtype) == "DEFAULT") {
      par.m_actual_fwtype = keywd.m_filter;
    } else {
      par.m_actual_fwtype = par.m_fwtype;
    }
  }

  delete rgti;
  ahfits::close(fpgti);
  AH_INFO(ahlog::HIGH) << "Closed GTI file" << std::endl;

  // Step 4: read attitude data (required)

  ahfits::open(par.m_ehkfile, "", &fpehk);  //  Includes existence check
  AH_INFO(ahlog::HIGH) << "Opened EHK file " << par.m_ehkfile << std::endl;

  if (ahfits::isPrimary(fpehk)) ahfits::move(fpehk, "EHK");   // move to EHK extension if extended syntax not used

  // Check that file has data
  ahmission::checkEmptyTable(fpehk, par.m_ehkfile);

  // Check for attitude column existence

  rehk = new ahfits::Router(fpehk);

  // Step 4b: determine min/max theta

  // Read keywords in ehkfile

  keywd.m_ra_src = ahfits::getKeyValDbl(fpehk, "RA_OBJ");
  keywd.m_dec_src = ahfits::getKeyValDbl(fpehk, "DEC_OBJ");
  keywd.m_ra_nom = ahfits::getKeyValDbl(fpehk, "RA_NOM");
  keywd.m_dec_nom = ahfits::getKeyValDbl(fpehk, "DEC_NOM");
  keywd.m_pa_nom = ahfits::getKeyValDbl(fpehk, "PA_NOM");

  AH_INFO(ahlog::HIGH) << "Keywords obtained from EHK file:" << std::endl;
  AH_INFO(ahlog::HIGH) << "RA_OBJ   = " << keywd.m_ra_src << std::endl;
  AH_INFO(ahlog::HIGH) << "DEC_OBJ  = " << keywd.m_dec_src << std::endl;
  AH_INFO(ahlog::HIGH) << "RA_NOM   = " << keywd.m_ra_nom << std::endl;
  AH_INFO(ahlog::HIGH) << "DEC_NOM  = " << keywd.m_dec_nom << std::endl;
  AH_INFO(ahlog::HIGH) << "PA_NOM   = " << keywd.m_pa_nom << std::endl;

  // Connect EHK file columns

  rehk = new ahfits::Router(fpehk);
  rehk->connectScalar(ahfits::e_READONLY, "TIME", ehkrow.m_time);
  rehk->connectScalar(ahfits::e_READONLY, "RA", ehkrow.m_ra);
  rehk->connectScalar(ahfits::e_READONLY, "DEC", ehkrow.m_dec);
  rehk->connectScalar(ahfits::e_READONLY, "ROLL", ehkrow.m_roll);
  AH_INFO(ahlog::HIGH) << "Reading columns TIME, RA, DEC, ROLL from EHK file" << std::endl;

  if (inst == "HXI1") {
    rehk->connectScalar(ahfits::e_READONLY, "HX1_RA_PNT", ehkrow.m_ra_pnt);
    rehk->connectScalar(ahfits::e_READONLY, "HX1_DEC_PNT", ehkrow.m_dec_pnt);
    AH_INFO(ahlog::HIGH) << "Reading columns HX1_RA_PNT, HX1_DEC_PNT from EHK file" << std::endl;
  } else if (inst == "HXI2") {
    rehk->connectScalar(ahfits::e_READONLY, "HX2_RA_PNT", ehkrow.m_ra_pnt);
    rehk->connectScalar(ahfits::e_READONLY, "HX2_DEC_PNT", ehkrow.m_dec_pnt);
    AH_INFO(ahlog::HIGH) << "Reading columns HX2_RA_PNT, HX2_DEC_PNT from EHK file" << std::endl;
  } else if (inst == "SXS") {
    rehk->connectScalar(ahfits::e_READONLY, "SXS_RA_PNT", ehkrow.m_ra_pnt);
    rehk->connectScalar(ahfits::e_READONLY, "SXS_DEC_PNT", ehkrow.m_dec_pnt);
    AH_INFO(ahlog::HIGH) << "Reading columns SXS_RA_PNT, SXS_DEC_PNT from EHK file" << std::endl;
  } else if (inst == "SXI") {
    rehk->connectScalar(ahfits::e_READONLY, "SXI_RA_PNT", ehkrow.m_ra_pnt);
    rehk->connectScalar(ahfits::e_READONLY, "SXI_DEC_PNT", ehkrow.m_dec_pnt);
    AH_INFO(ahlog::HIGH) << "Reading columns SXI_RA_PNT, SXI_DEC_PNT from EHK file" << std::endl;
  } else {
    AH_THROW_RUNTIME("Bad INSTRUME parameter = " + par.m_instrume + ".  Exiting.");
  }
  
  max_theta = -1e9;
  min_theta = 1e9;
  i_gti = 0;
  i_ehk = 0;
  gti_status = ahexpmap::e_PREVIEW_NOT_IN_GTI;
  read_row = true;
  ahfits::firstRow(fpehk);
  num_iter = 0;
  num_preview_path0 = 0;
  num_preview_path1 = 0;
  num_preview_path2 = 0;
  num_preview_path3 = 0;
  num_preview_path4 = 0;
  num_preview_path5 = 0;
  num_preview_path6 = 0;
  AH_INFO(ahlog::HIGH) << "Number of rows in EHK file = " << ahfits::numRows(fpehk) << std::endl;

  while (ahfits::readOK(fpehk)) {

  // for (firstRow(fpehk); readOK(fpehk); nextRow(fpehk)) 

    num_iter++;

    if (read_row) {
      readRow(fpehk);   // Obtain columns set up above
      i_ehk++;
    }

    if (gti_status == ahexpmap::e_PREVIEW_NOT_IN_GTI) {

      if (i_gti >= gti_data.m_num_gti) {
        if (ahlog::get_debug()) {
          ss.str("");
          ss << std::setprecision(15) << "GTI subscript >= number of GTI, i_gti=" << i_gti 
            << " gti_data.m_num_gti=" << gti_data.m_num_gti 
            << "EHK TIME=" << ehkrow.m_time << " i_ehk=" << i_ehk;
          AH_DEBUG << ss.str() << std::endl;
        }
        num_preview_path1++;
        break;
      } else {
        // ss.str("");
        // ss << std::setprecision(15) << "GTI subscript < number of GTI, i_gti=" << i_gti 
        //   << " number of gti=" << gti_data.m_num_gti 
        //   << " gti start=" << gti_data.m_start[i_gti]
        //   << " gti stop=" << gti_data.m_stop[i_gti]
        //   << "EHK TIME=" << ehkrow.m_time << " i_ehk=" << i_ehk;
        // AH_INFO(ahlog::LOW) << ss.str() << std::endl;
        num_preview_path2++;
      }
    }

    if (ehkrow.m_time < gti_data.m_start[i_gti]) {
      gti_status = ahexpmap::e_PREVIEW_NOT_IN_GTI;
      ahfits::nextRow(fpehk);
      read_row = true;
      compute_theta_phi = false;
      num_preview_path3++;
    } else if (ehkrow.m_time > gti_data.m_stop[i_gti]) {
      if (gti_status == ahexpmap::e_PREVIEW_IN_GTI) {
        if (ahlog::get_debug()) {
          ss.str("");
          ss << std::setprecision(15) << "EHK passed end of GTI: i_gti=" << i_gti 
            << ", number of gti=" << gti_data.m_num_gti 
            << ", gti start=" << gti_data.m_start[i_gti]
            << ", gti stop=" << gti_data.m_stop[i_gti]
            << ", EHK TIME=" << ehkrow.m_time << ", i_ehk=" << i_ehk;
          AH_DEBUG << ss.str() << std::endl;
        }
        compute_theta_phi = false;
      } else {
        compute_theta_phi = true;
      }
      i_gti++;
      gti_status = ahexpmap::e_PREVIEW_NOT_IN_GTI;
      read_row = false;
      num_preview_path4++;
    } else {
      if (gti_status == ahexpmap::e_PREVIEW_NOT_IN_GTI) {
        if (ahlog::get_debug()) {
          ss.str("");
          ss << std::setprecision(15) << "EHK at start of GTI: i_gti=" << i_gti 
            << ", number of gti=" << gti_data.m_num_gti 
            << ", gti start=" << gti_data.m_start[i_gti]
            << ", gti stop=" << gti_data.m_stop[i_gti]
            << ", EHK TIME=" << ehkrow.m_time << ", i_ehk=" << i_ehk;
          AH_DEBUG << ss.str() << std::endl;
        }
      }
      gti_status = ahexpmap::e_PREVIEW_IN_GTI;
      compute_theta_phi = true;
      ahfits::nextRow(fpehk);
      read_row = true;
      num_preview_path5++;
    }

    if (compute_theta_phi) {
      calcDeltaAttitude(keywd.m_ra_src, ehkrow.m_ra_pnt, ehkrow.m_ra, 
        keywd.m_dec_src, ehkrow.m_dec_pnt, ehkrow.m_dec, ehkrow.m_roll, theta, phi);
      if (theta > max_theta) max_theta = theta;
      if (theta < min_theta) min_theta = theta;
      num_preview_path6++;
    }

  }
  
  if (gti_status == ahexpmap::e_PREVIEW_IN_GTI) {  // GTI open at last EHK row
    i_gti++;
  }

  AH_INFO(ahlog::HIGH) << "Read " << i_ehk << " row" << (i_ehk>1?"s":"") << " of EHK file" << std::endl;
  AH_INFO(ahlog::HIGH) << "Processed " << i_gti << " GTI" << std::endl;
  AH_INFO(ahlog::LOW) << "Number of preview loop iterations = " << num_iter << " >= number of EHK rows processed" << std::endl;
  AH_DEBUG << "Execution counts for preview code paths: " << std::endl;
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  0: %10d  previous EHK time not in GTI; GTI pointer advanced past EHK time gap\n", num_preview_path0); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  1: %10d  previous EHK time not in GTI; GTI pointer has gone out of range\n", num_preview_path1); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  2: %10d  previous EHK time not in GTI; GTI pointer is in range\n", num_preview_path2); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  3: %10d  current EHK time is before current GTI start:  go read next EHK row\n", num_preview_path3);
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  4: %10d  current EHK time is after current GTI stop:  need to find next GTI\n", num_preview_path4); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  5: %10d  current EHK time is within current GTI\n", num_preview_path5); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  6: %10d  theta and phi are computed\n", num_preview_path6); 

  // Step 4c: determine the dimension of the offaxis angle grid

  // Determine bin number of min theta (need not be central theta bin)
  min_theta_bin = min_theta/par.m_delta;

  // Determine the number of theta bins
  ngrid_theta = max_theta/par.m_delta + 1 - min_theta_bin;
  // For each theta bin n > 0, there will be (par.phi x i) phi bins, plus 1 in the first theta bin

  // If the central theta bin is used, it is not split by phi
  if (min_theta_bin == 0) {
    ngrid_wedge = 1;
  } else {
    ngrid_wedge = min_theta_bin*par.m_numphi;
  }  
  // For each theta bin besides central, split it into phi bins
  for (long i=min_theta_bin+1; i<(min_theta_bin+ngrid_theta); ++i) {
    // If numphi is 0, no azimuthal splitting
    if (par.m_numphi == 0) {
      ngrid_wedge += 1;
    } else {
      ngrid_wedge += par.m_numphi * i;
    }
  }

  AH_INFO(ahlog::HIGH) << "Minimum OFFAXIS angle (theta) = " << min_theta << 
    ", first theta bin = " << min_theta_bin << std::endl;
  AH_INFO(ahlog::HIGH) << "Maximum OFFAXIS angle (theta) = " << max_theta << 
    ", number of theta bins = " << ngrid_theta << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of phi bins per theta bin = " << par.m_numphi << 
    ", total number of bins = " << ngrid_wedge << std::endl;

  if (ngrid_wedge > 20) { 
    AH_INFO(ahlog::HIGH) << "Large number of theta,phi bins=" << ngrid_wedge
      << "; consider increasing DELTA parameter, which is currently "
      << par.m_delta << " arcmin" << std::endl;
    AH_INFO(ahlog::HIGH) 
      << "Or consider decreasing NUMPHI parameter, which is currently "
      << par.m_numphi << std::endl;
  }

  // Step 5: get dimension of exposure array

  if (read_pixgti) {
    if (inst == "SXS") {
      defpixgtiext = ahexpmap::SXSDEFPIXGTI;
    } else {
      defpixgtiext = ahexpmap::DEFPIXGTI;
    }
    ahfits::open(par.m_pixgtifile, "", &fppix);

    // Move to default extension for instrument if
    // extended syntax not used.
    if (ahfits::isPrimary(fppix)) ahfits::move(fppix, defpixgtiext);
    npix = ahfits::getKeyValLLong(fppix, "NAXIS2");
    ahfits::close(fppix);
    AH_INFO(ahlog::HIGH) << "Reading pixel GTI data from file " << par.m_pixgtifile << "; npix = " << npix << std::endl;
  } else {
    npix = 0;
    AH_INFO(ahlog::HIGH) << "No pixel GTI data; npix = " << npix << std::endl;
  }

  // expo[theta][0] is total exposure of offaxis grid[theta]
  // expo[theta][i] (1 <= i <= n) is exposure of pixel i in offaxis grid[theta]

  //  Allocate storage for working arrays

  expo.resize(ngrid_wedge);
  for (long i=0; i<ngrid_wedge; ++i) {
    expo[i].resize(npix+1);
  }

  ave_ra.resize(ngrid_wedge);
  ave_dec.resize(ngrid_wedge);
  ave_theta.resize(ngrid_wedge);
  ave_phi.resize(ngrid_wedge);
  ave_roll.resize(ngrid_wedge);
  ave_ra_pnt.resize(ngrid_wedge);
  ave_dec_pnt.resize(ngrid_wedge);

  for (long i=0; i<ngrid_wedge; ++i) {
    for (long j=0; j<npix+1; ++j) expo[i][j] = 0.0;
    ave_ra[i] = 0.0;
    ave_dec[i] = 0.0;
    ave_theta[i] = 0.0;
    ave_phi[i] = 0.0;
    ave_roll[i] = 0.0;
    ave_ra_pnt[i] = 0.0;
    ave_dec_pnt[i] = 0.0;
  }

  // Step 6:  Set up for efficiency mode.

  do_efficiency = ( ahgen::strtoupper(par.m_outmaptype) == "EFFICIENCY" );

  if (inst == "SXI" || inst == "SXS") {
    if (do_efficiency) {
      calcEfficiencyMap(par, keywd, effimage);
    }
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(ahexpmap::Params & par,
  ahfits::FilePtr fpehk, ahfits::Router* rehk, ahexpmap::EHKrow& ehkrow,
  ahexpmap::KeywordValues& keywd, 
  ahfits::FilePtr& fpbad, ahfits::FilePtr& fpout, ahexpmap::GTIData& gti_data, 
  long min_theta_bin, long ngrid_theta, long ngrid_wedge, long npix,
  std::vector<std::vector<double> >& expo,
  std::vector<double>& ave_ra, std::vector<double>& ave_dec, std::vector<double>& ave_roll, 
  std::vector<double>& ave_ra_pnt, std::vector<double>& ave_dec_pnt, 
  std::vector<double>& ave_theta, std::vector<double>& ave_phi, const bool do_efficiency, 
  const std::string & actual_instmap, 
  std::vector<std::vector<double> > & effimage) {

  ahfits::Router* rpix;    // Manages variables for pixel GTI file
  long l_detx=0, l_dety=0;          // Local variables for FITS columns
  double l_start=0.0, l_stop=0.0;   // Local variables for FITS columns

  ahfits::Img2dShr badpix_map;     // Bad pixel or instrument map
  ahfits::Img2dShr flag_image;     // Result of combining bad pixel, exposure

  ahfits::FilePtr fppix = 0;          // File pointer for pixel GTI file

  ahfits::Router* rout;       // Manages variables for offaxis histogram
  ahfits::Router* routgti;    // Manages variables for GTIEXP extensions
  ahfits::Router* routpartial;   // Manages variables for PARTIALEXP extensions
  int l_detx_out=0, l_dety_out;
  float l_fraction_out=0.0;
  double l_offaxislo=0.0, l_offaxishi=0.0, l_offaxisval=0.0;
  double l_azimuthlo=0.0, l_azimuthhi=0.0, l_azimuthval=0.0;
  double l_timeinterval=0.0, l_fraction=0.0, l_ranomxp=0.0, l_decnomxp=0.0;
  double l_pa_nomxp=0.0, l_ra_pnt=0.0, l_dec_pnt=0.0;       // Local variables for FITS columns

  std::string inst = "";          // Uppercase version of INSTRUME
  std::string defpixgtiext = "";  // Default pixel GTI extension name

  ahfits::IndexType i_ehk = 0;   // Row counter for EHK file
  ahfits::IndexType i_gti = 0;   // Row counter for GTI file
  ahfits::IndexType k_gti = 0;   // Counter for GTI bins in output file

  //  Counters for execution paths through EHK and GTI processing

  ahfits::IndexType num_iter=0, num_ehk_path0=0, num_ehk_path1=0, 
    num_ehk_path2=0, num_ehk_path3=0, num_ehk_path4=0, num_ehk_path5=0, 
    num_ehk_path6=0, num_ehk_path7=0, num_ehk_path8=0, num_ehk_path9=0, 
    num_ehk_path10=0, num_ehk_path11=0, num_ehk_path12=0, num_ehk_path13=0, 
    num_ehk_path14=0, num_ehk_path15=0, num_ehk_path16=0, num_ehk_path17=0,
    num_ehk_path18=0, num_ehk_path19=0, num_ehk_path20=0, num_ehk_path21=0;

  ahexpmap::GTIStatus gti_status = ahexpmap::e_NOT_IN_GTI;
  long theta_j=0, theta_k=0;       // Offaxis angle bins

  double theta = 0.0;
  double phi = 0.0;
  double total_exp = 0.0;
  double this_ave_theta = 0.0;
  double this_ave_phi = 0.0;
  double this_ave_ra = 0.0;
  double this_ave_dec = 0.0;
  double this_ave_roll = 0.0;
  double this_ave_ra_pnt = 0.0;
  double this_ave_dec_pnt = 0.0;
  double sum_theta = 0.0;
  double sum_phi = 0.0;
  double sum_ra = 0.0;
  double sum_dec = 0.0;
  double sum_roll = 0.0;
  double sum_ra_pnt = 0.0;
  double sum_dec_pnt = 0.0;
  double grand_ave_ra_pnt = 0.0;
  double grand_ave_dec_pnt = 0.0;
  ahfits::IndexType n_row = 0;
  long length_x=0, length_y=0;      // Image dimensions
  long num_expo=0;                    // Number of exposure maps with non-zero exposure to write out.

  ahexpmap::OffAxisGTI offaxis_gti; // Structure: offaxis GTI array

  double offaxis_gti_start = 0.0;   // Start of current GTI
  double offaxis_gti_stop = 0.0;    // End of current GTI
  double offaxis_gti_len = 0.0;     // Length of current GTI
  double offaxis_gti_min_start = 0.0;    // For computing GTI header TSTART
  double offaxis_gti_max_stop = 0.0;     // For computing GTI header TSTOP

  long theta_bin = 0;               // Bin number for theta only
  std::set<long> unique_theta_bin;  // Count unique offaxis angle bins
  std::vector<double> fraction;     // Array of relative exposure times
  ahfits::Img2dDbl offaxis_image;   // To hold individual exposure maps
  ahfits::Img2dFlt output_image;    // To be written to outfile
  double timeinterval = 0.0;        // To hold actual time interval per exposure map
  double max_oi = 0.0;              // Maximum of offaxis image, for messages
  double sum_max_oi = 0.0;          // Sum of maxima of offaxis images, for messages
  double max_out = 0.0;             // Maximum of output image, for messages
  double bad_time_fraction = 0.0;            // Fraction of current pixel's exp time in "bad GTI"
  double additional_bad_time_fraction = 0.0; // Fractional amount to be subtracted from current 
                                             //   pixel's exposure time
  double new_good_time_fraction = 0.0;       // Resultant fractional exposure time for pixel
  const double frac_eps = 1.0e-10;           // Leeway for testing validity of frac exp time
  IMAGETRANS* trans_param = 0;      // params for routines borrowed from imagetrans
  IMAGE* original = 0;              // Image before transformation 
  IMAGE* transformed = 0;           // Image after transformation 
  //std::string output_img_extname = "EXPOSURE";  // Extension name of output SKY image
  double pixelarearatio = 0.0;      // Ratio of output to DET pixel area

  std::stringstream ss;             // Used to build EXTNAMEs

  bool read_row = true;             // Read the current EHK table row
  bool extra_iteration = false;     // Add on a fake EHK table row to close last GTI

  //  Flags for whether bad pixel image or pixel GTI table are used

  bool read_badimg = ( ahgen::strtoupper(par.m_badimgfile) != "NONE" );
  bool read_pixgti = ( ahgen::strtoupper(par.m_pixgtifile) != "NONE" );

  inst = ahgen::strtoupper(par.m_instrume);

  // Set up variable connections for pixel GTI file

  if (read_pixgti) {
    if (inst == "SXS") {
      defpixgtiext = ahexpmap::SXSDEFPIXGTI;
    } else {
      defpixgtiext = ahexpmap::DEFPIXGTI;
    }
    ahfits::open(par.m_pixgtifile, "", &fppix);

    // Move to default extension for instrument if
    // extended syntax not used.
    if (ahfits::isPrimary(fppix)) ahfits::move(fppix, defpixgtiext);
    rpix = new ahfits::Router(fppix);
    rpix->connectScalar(ahfits::e_READONLY, "DETX", l_detx);
    rpix->connectScalar(ahfits::e_READONLY, "DETY", l_dety);
    rpix->connectScalar(ahfits::e_READONLY, "START", l_start);
    rpix->connectScalar(ahfits::e_READONLY, "STOP", l_stop);
    AH_INFO(ahlog::HIGH) << "Opened pixel GTI file " << par.m_pixgtifile 
      << " before processing EHK file" << std::endl;
  }

  // Step 7:  Main part: Loop through EHK file to calculate histogram

  ahfits::move(fpehk, "EHK");
  AH_INFO(ahlog::HIGH) << "Rereading EHK file (moved to EHK extension)" << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows in EHK file = " << ahfits::numRows(fpehk) << std::endl;

  i_ehk = 0;
  i_gti = 0;
  gti_status = ahexpmap::e_NOT_IN_GTI;
  read_row = true;
  extra_iteration = false;   // Extra iterations may be needed to clean up last GTI
  ahfits::firstRow(fpehk);
  num_iter = 0;
  num_ehk_path0 = 0; num_ehk_path1 = 0; num_ehk_path2 = 0; num_ehk_path3 = 0;
  num_ehk_path4 = 0; num_ehk_path5 = 0; num_ehk_path6 = 0; num_ehk_path7 = 0; 
  num_ehk_path8 = 0; num_ehk_path9 = 0; num_ehk_path10 = 0; num_ehk_path11 = 0; 
  num_ehk_path12 = 0; num_ehk_path13 = 0; num_ehk_path14 = 0; num_ehk_path15 = 0;
  num_ehk_path16 = 0;
  while (ahfits::readOK(fpehk) || extra_iteration) {

    num_iter++;

    // Read columns

    if (read_row) {
      readRow(fpehk);   // Gets connected columns TIME and attitude
      i_ehk++;
      if (i_ehk % 5000 == 1) {
        AH_OUT << "Read " << i_ehk << " rows of EHK file" << std::endl;
      }
    }

    // AH_INFO(ahlog::LOW) << "EHK current row=" << fpehk->m_currow 
    //   << ", EHK number of rows=" << ahfits::numRows(fpehk) << " i_gti=" << i_gti 
    //   << " i_ehk=" << i_ehk << std::endl;

    // Step 7a:  Check if the TIME is within GTI

    if (gti_status == ahexpmap::e_NOT_IN_GTI) {

      // In old versions of this program, GTI not containing an EHK time
      // were skipped over here.  However, time in these GTI has to be counted.
      // The time is attributed to the next EHK attitude.

      if (i_gti < gti_data.m_num_gti && ehkrow.m_time > gti_data.m_stop[i_gti]) {
        if (ahlog::get_debug()) {
          ss.str("");
          ss << "time " << std::setprecision(15) << ehkrow.m_time << " after gti stop time " 
            << gti_data.m_stop[i_gti] << " i_gti=" << i_gti;
          AH_DEBUG << ss.str() << std::endl;
        }
        num_ehk_path0++;
      }

      if (i_gti >= gti_data.m_num_gti) {
        AH_DEBUG << "GTI subscript >= number of GTI, i_gti=" << i_gti 
          << " gti_data.m_num_gti=" << gti_data.m_num_gti << " i_ehk=" << i_ehk << std::endl;
        num_ehk_path1++;
        break;
      } else {
        // AH_INFO(ahlog::LOW) << "GTI subscript < number of GTI, i_gti=" << i_gti 
        //   << " gti_data.m_num_gti=" << gti_data.m_num_gti << " i_ehk=" << i_ehk << std::endl;
        num_ehk_path2++;
      }
    }

    // First conditional block:  check for GTI boundary or offaxis bin boundary

    if (ehkrow.m_time < gti_data.m_start[i_gti]) {

      if (ahlog::get_debug()) {
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time << " before GTI start time " 
          << gti_data.m_start[i_gti] << " i_gti=" << i_gti << " i_ehk=" << i_ehk;
        AH_DEBUG << ss.str() << std::endl;
      }
      ahfits::nextRow(fpehk);
      read_row = true;
      gti_status = ahexpmap::e_NOT_IN_GTI;
      num_ehk_path3++;
      continue;

    } else if (gti_status == ahexpmap::e_NOT_IN_GTI) {

      // First row after GTI start

      if (ahlog::get_debug()) {
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time;
        AH_DEBUG << ss.str() << std::endl;
      }
      offaxis_gti_start = gti_data.m_start[i_gti];
      calcDeltaAttitude(keywd.m_ra_src, ehkrow.m_ra_pnt, ehkrow.m_ra, 
        keywd.m_dec_src, ehkrow.m_dec_pnt, ehkrow.m_dec, ehkrow.m_roll, theta, phi);
      // Now need to get the wedge index, nut simply the annulus index
      //theta_j = int(theta/par.m_delta) - min_theta_bin;
      getIndexFromWedge(theta, phi, min_theta_bin, par.m_delta, par.m_numphi, theta_j);
      AH_DEBUG << "getIndexFromWedge 1: theta=" << theta << " phi=" << phi
        << " min_theta_bin=" << min_theta_bin << " par.m_delta=" << par.m_delta 
        << " par.m_numphi=" << par.m_numphi << " theta_j=" << theta_j << std::endl;

      gti_status = ahexpmap::e_IN_GTI_AND_OFFAXIS_BIN;

      if (ahlog::get_debug()) {
        ss.str("");
        ss << std::setprecision(8) << "ra_src=" << keywd.m_ra_src << " ra_pnt=" << ehkrow.m_ra_pnt 
          << " ra_nom=" <<  ehkrow.m_ra 
          << " dec_src=" << keywd.m_dec_src << " dec_pnt=" << ehkrow.m_dec_pnt << " dec_nom=" << ehkrow.m_dec
          << " roll_nom=" << ehkrow.m_roll << " theta=" << theta << " phi=" << phi;
        AH_DEBUG << ss.str() << std::endl;
        AH_DEBUG << "gti_status set to IN_GTI_AND_OFFAXIS_BIN, theta_j=" << theta_j << " i_ehk=" << i_ehk << std::endl;
      }
      num_ehk_path4++;

    } else if (extra_iteration) {

      // Last GTI includes last EHK row

      //offaxis_gti_stop = ehkrow.m_time;  // This was a bug: GTI define the observation, not EHK
      offaxis_gti_stop = gti_data.m_stop[i_gti];
      gti_status = ahexpmap::e_NOT_IN_GTI;

      if (ahlog::get_debug()) {
        AH_DEBUG << "Extra iteration to trigger summation over last GTI" << std::endl;
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time << " within last GTI "
          << gti_data.m_start[i_gti] << " - " << gti_data.m_stop[i_gti] 
          << " i_gti=" << i_gti << ", gti_status set to NOT_IN_GTI";
        AH_DEBUG << ss.str() << std::endl;
      }
      num_ehk_path5++;

    } else if (ehkrow.m_time > gti_data.m_stop[i_gti]) {

      // First row after GTI stop

      offaxis_gti_stop = gti_data.m_stop[i_gti];
      gti_status = ahexpmap::e_NOT_IN_GTI;

      if (ahlog::get_debug()) {
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time << " after GTI stop time " 
          << gti_data.m_stop[i_gti] << " i_gti=" << i_gti << ", gti_status set to NOT_IN_GTI";
        AH_INFO(ahlog::LOW) << ss.str() << std::endl;
      }
      num_ehk_path6++;

    } else {

      // Step 7b: check if this row falls in the same offaxis bin
      // as the previous row

      if (ahlog::get_debug()) {
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time
           << " gti_status=" << ahexpmap::bin_status_msg[gti_status]
           << " gti=" << gti_data.m_start[i_gti] << "-" << gti_data.m_stop[i_gti];
        AH_DEBUG << ss.str() << std::endl;
      }

      calcDeltaAttitude(keywd.m_ra_src, ehkrow.m_ra_pnt, ehkrow.m_ra, 
        keywd.m_dec_src, ehkrow.m_dec_pnt, ehkrow.m_dec, ehkrow.m_roll, theta, phi);
      // Now need to get the wedge index, nut simple the annulus index
      //theta_k = int(theta/par.m_delta) - min_theta_bin;
      getIndexFromWedge(theta, phi, min_theta_bin, par.m_delta, par.m_numphi, theta_k);
      AH_DEBUG << "getIndexFromWedge 2: theta=" << theta << " phi=" << phi
        << " min_theta_bin=" << min_theta_bin << " par.m_delta=" << par.m_delta 
        << " par.m_numphi=" << par.m_numphi << " theta_k=" << theta_k << std::endl;

      // Detect start of new offaxis bin.  Note that if the spacecraft attitude
      // crosses a bin boundary at the very end of a GTI, the attitude change
      // takes precedence, because the GTI end is not detected until the next
      // EHK row is processed.  This is an infrequent occurrence.
      if (theta_k != theta_j) {
        offaxis_gti_stop = ehkrow.m_time;
        gti_status = ahexpmap::e_NEW_OFFAXIS_BIN_SAME_GTI;
        AH_DEBUG << "gti_status set to NEW_OFFAXIS_BIN_SAME_GTI, theta_j=" << theta_j << " i_ehk=" << i_ehk << std::endl;
        num_ehk_path7++;
      }
      num_ehk_path8++;
      if (ahlog::get_debug()) {
        ss.str("");
        ss << std::setprecision(8) << "ra_src=" << keywd.m_ra_src << " ra_pnt=" << ehkrow.m_ra_pnt 
          << " ra_nom=" <<  ehkrow.m_ra 
          << " dec_src=" << keywd.m_dec_src << " dec_pnt=" << ehkrow.m_dec_pnt << " dec_nom=" << ehkrow.m_dec
          << " roll_nom=" << ehkrow.m_roll << " theta=" << theta << " phi=" << phi;
        AH_DEBUG << ss.str() << std::endl;
      }

    }  // End first conditional block

    // Second conditional block: accumulate results based on status of current row
    // in relation to GTI and bin boundaries

    if (gti_status == ahexpmap::e_IN_GTI_AND_OFFAXIS_BIN) {

      // Attitude stays in the same offaxis bin. Count up the attitude values

      sum_theta += theta;
      sum_phi += phi;
      sum_ra += ehkrow.m_ra;
      sum_dec += ehkrow.m_dec;
      sum_roll += ehkrow.m_roll;
      sum_ra_pnt += ehkrow.m_ra_pnt;
      sum_dec_pnt += ehkrow.m_dec_pnt;
      if (ahlog::get_debug()) {
        ss.str("");
        ss << std::setprecision(8) 
          << " sum_ra=" << sum_ra << " sum_dec=" << sum_dec
          << " sum_roll=" << sum_roll
          << " sum_ra_pnt=" << sum_ra_pnt << " sum_dec_pnt=" << sum_dec_pnt
          << " sum theta=" << sum_theta << " sum_phi=" << sum_phi;
        AH_DEBUG << ss.str() << std::endl;
      }


      AH_DEBUG << "gti_status=IN_GTI_AND_OFFAXIS_BIN theta_j=" << theta_j << " i_ehk=" << i_ehk << std::endl;
      num_ehk_path9++;
      n_row ++;

      if (ehkrow.m_time > gti_data.m_stop[i_gti]) {

        // Current EHK time is past the end of the current GTI, meaning the
        // GTI has been skipped and is not sampled by the EHK.  It has
        // been counted above like any other GTI.

        offaxis_gti_stop = gti_data.m_stop[i_gti];
        gti_status = ahexpmap::e_NOT_IN_GTI;
        num_ehk_path19++;

      } else {

        ahfits::nextRow(fpehk);
        read_row = true;
        num_ehk_path20++;
      }

    }

    if (gti_status != ahexpmap::e_IN_GTI_AND_OFFAXIS_BIN) {

      // At this point, one of the following is true:
      //  (1) the current point is in a new offaxis bin, or 
      //  (2) a GTI has just closed
      // If both of these conditions occur together, then an offaxis GTI
      //  length of zero is generated.

      // Step 7c:  Calculate quantities for the bin

      offaxis_gti_len = offaxis_gti_stop - offaxis_gti_start;

      if (0 == offaxis_gti_len) {
        ss.str("");
        ss << "time " << std::setprecision(15) << ehkrow.m_time
           << " offaxis_gti_len is zero; "
           << " gti_status=" << ahexpmap::bin_status_msg[gti_status]
           << " gti=" << gti_data.m_start[i_gti] << "-" << gti_data.m_stop[i_gti];
        AH_INFO(ahlog::HIGH) << ss.str() << std::endl;
        num_ehk_path21++;

      } else {

        // Calculate average attitude in this interval
        this_ave_ra = sum_ra / n_row;
        this_ave_dec = sum_dec / n_row;
        this_ave_roll = sum_roll / n_row;
        this_ave_ra_pnt = sum_ra_pnt / n_row;
        this_ave_dec_pnt = sum_dec_pnt / n_row;
        this_ave_theta = sum_theta / n_row;
        this_ave_phi = sum_phi / n_row;
        if (ahlog::get_debug()) {
          ss.str("");
          ss << std::setprecision(15) 
            << "offaxis_gti_start=" << offaxis_gti_start << " offaxis_gti_stop=" << offaxis_gti_stop
            << " offaxis_gti_len=" << offaxis_gti_len << " n_row=" << n_row << std::endl;
          ss << std::setprecision(8) 
            << "this_ave_ra=" << this_ave_ra << " this_ave_dec=" << this_ave_dec
            << " this_ave_roll=" << this_ave_roll
            << " this_ave_ra_pnt=" << this_ave_ra_pnt << " this_ave_dec_pnt=" << this_ave_dec_pnt
            << " this ave_theta=" << this_ave_theta << " this_ave_phi=" << this_ave_phi;
          AH_DEBUG << ss.str() << std::endl;
        }

        // Keep track of unique bins
        theta_bin = this_ave_theta/par.m_delta;
        unique_theta_bin.insert(theta_bin);

        // Update average attitude

        ave_theta[theta_j] = 
          (ave_theta[theta_j]*expo[theta_j][0] + this_ave_theta*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_phi[theta_j] = 
          (ave_phi[theta_j]*expo[theta_j][0] + this_ave_phi*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_ra[theta_j] = 
          (ave_ra[theta_j]*expo[theta_j][0] + this_ave_ra*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_dec[theta_j] = 
          (ave_dec[theta_j]*expo[theta_j][0] + this_ave_dec*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_roll[theta_j] = 
          (ave_roll[theta_j]*expo[theta_j][0] + this_ave_roll*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_ra_pnt[theta_j] = 
          (ave_ra_pnt[theta_j]*expo[theta_j][0] + this_ave_ra_pnt*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);
        ave_dec_pnt[theta_j] = 
          (ave_dec_pnt[theta_j]*expo[theta_j][0] + this_ave_dec_pnt*offaxis_gti_len)/(expo[theta_j][0] + offaxis_gti_len);

        //std::cout << "+++++ " << offaxis_gti_len << " " << this_ave_ra << " " << ave_ra[theta_j] << " " << theta_j << std::endl;

        grand_ave_ra_pnt = 
          (grand_ave_ra_pnt*total_exp + this_ave_ra_pnt*offaxis_gti_len)/(total_exp + offaxis_gti_len);
        grand_ave_dec_pnt = 
          (grand_ave_dec_pnt*total_exp + this_ave_dec_pnt*offaxis_gti_len)/(total_exp + offaxis_gti_len);

        // Add this offaxis interval time to exposure array

        expo[theta_j][0] += offaxis_gti_len;
        AH_DEBUG << "Accum expos time: theta_j=" << theta_j << " expo[theta_j][0]="
          << expo[theta_j][0] << " offaxis_gti_len=" << offaxis_gti_len << std::endl;

        // Add interval to list of GTI labeled by theta bin
        
        offaxis_gti.m_theta_bin.push_back(theta_j);
        offaxis_gti.m_start.push_back(offaxis_gti_start);
        offaxis_gti.m_stop.push_back(offaxis_gti_stop);

        ++k_gti;

        if (ahlog::get_debug()) {
          ss.str("");
          ss << "Added offaxis GTI k_gti=" << k_gti << " theta_j=" << theta_j 
            << " offaxis_gti_start=" << std::setprecision(15)
            << offaxis_gti_start << " offaxis_gti_stop=" << offaxis_gti_stop;
          AH_DEBUG << ss.str() << std::endl;
        }

        // Count up total exposure as well

        total_exp += offaxis_gti_len;

        AH_DEBUG << "theta_j=" << theta_j << " total_exp=" << total_exp 
          << " expo[theta_j][0]=" << expo[theta_j][0] << std::endl;

        // Reset sum parameters

        sum_theta = 0.0;
        sum_phi = 0.0;
        sum_ra = 0.0;
        sum_dec = 0.0;
        sum_roll = 0.0;
        sum_ra_pnt = 0.0;
        sum_dec_pnt = 0.0;
        n_row = 0;

        // Step 7d:  process pixel GTI (e.g., flickering pixel)

        if (read_pixgti) {
          //if (inst == "SXS") {
          //  defpixgtiext = ahexpmap::SXSDEFPIXGTI;
          //} else {
          //  defpixgtiext = ahexpmap::DEFPIXGTI;
          //}
          //ahfits::close(fppix);
          //ahfits::open(par.m_pixgtifile, "", &fppix);

          // Move to default extension for instrument if
          // extended syntax not used.
          //if (ahfits::isPrimary(fppix)) ahfits::move(fppix, defpixgtiext);
          
          ahfits::IndexType i_pix = 0;   // Pixel GTI row counter
          double t_start = 0.0, t_stop = 0.0, t_pixel_gti = 0.0;

          // Loop through per-pixel GTI file

          for (ahfits::firstRow(fppix); ahfits::readOK(fppix); ahfits::nextRow(fppix)) {
            ahfits::readRow(fppix);

            // Only the overlap between pixel GTI and overall GTI is kept

            t_start = l_start;
            t_stop = l_stop;
            if (t_start < offaxis_gti_start) t_start = offaxis_gti_start;
            if (t_stop > offaxis_gti_stop) t_stop = offaxis_gti_stop;
            t_pixel_gti = offaxis_gti_len - (t_stop - t_start);
            if (t_start >= offaxis_gti_stop) t_pixel_gti = offaxis_gti_len;
            if (t_stop <= offaxis_gti_start) t_pixel_gti = offaxis_gti_len;
            expo[theta_j][i_pix + 1] += t_pixel_gti;
            if (ahlog::get_debug()) {
              ss.str("");
              ss << std::setprecision(15) << "Adjust expo i_pix=" << i_pix << " l_start=" << l_start 
                << " l_stop=" << l_stop << " offaxis_gti_start=" << offaxis_gti_start 
                << " offaxis_gti_stop=" << offaxis_gti_stop << " offaxis_gti_len=" << offaxis_gti_len
                << " t_start=" << t_start << " t_stop=" << t_stop << " t_pixel_gti=" << t_pixel_gti;
              AH_DEBUG << ss.str() << std::endl;
            }
            ++i_pix;
          }
          num_ehk_path10++;
        }
        num_ehk_path11++;
      }

      if (gti_status == ahexpmap::e_NEW_OFFAXIS_BIN_SAME_GTI) {

        // Current row becomes the first row of a new interval

        AH_DEBUG << "gti_status=NEW_OFFAXIS_BIN_SAME_GTI theta_j=" << theta_j << " i_ehk=" << i_ehk << std::endl;
        
        // Update gtistart, attitude, etc.

        // Reinitialization was done above.

        offaxis_gti_start = offaxis_gti_stop;
        theta_j = theta_k;
        sum_theta += theta;
        sum_phi += phi;
        sum_ra += ehkrow.m_ra;
        sum_dec += ehkrow.m_dec;
        sum_roll += ehkrow.m_roll;
        sum_ra_pnt += ehkrow.m_ra_pnt;
        sum_dec_pnt += ehkrow.m_dec_pnt;
        n_row++;
        gti_status = ahexpmap::e_IN_GTI_AND_OFFAXIS_BIN;
        AH_INFO(ahlog::LOW) << "gti_status changed to IN_GTI_AND_OFFAXIS_BIN" << std::endl;

        ahfits::nextRow(fpehk);
        read_row = true;
        num_ehk_path12++;
    
      } else {

        AH_DEBUG << "gti_status=NOT_IN_GTI i_ehk=" << i_ehk << std::endl;

        // In this case, GTI start and attitude will be updated for
        // the first row within the next GTI, so no need to update them now

        read_row = false;
        i_gti++;
        num_ehk_path13++;
      }
    } // End second conditional block

    // Permit extra iterations until GTI list exhausted

    if (extra_iteration) {
      if (i_gti >= gti_data.m_num_gti) {
        extra_iteration = false;
        num_ehk_path14++;
      } else {
        i_gti++;
        num_ehk_path17++;
      }
    } else {
      
      // It's a bug to make the first condition ahfits::atLastRow(fpehk), because
      // it will be satisfied after the NEXT to last row is processed, not the
      // last row.

      if (i_ehk == ahfits::numRows(fpehk)) {
        if (gti_status == ahexpmap::e_IN_GTI_AND_OFFAXIS_BIN) {
          extra_iteration = true;
          num_ehk_path15++;
        } else if (i_gti < (gti_data.m_num_gti-1)) {
          extra_iteration = true;
          i_gti++;
          num_ehk_path18++;
        }
        read_row = false;
      }
      num_ehk_path16++;
    }

  } // End loop EHK file and GTI
  AH_OUT << "Processed " << i_ehk << " row" << (i_ehk>1?"s":"") << " of EHK file" << std::endl;
  AH_INFO(ahlog::HIGH) << "Processed " << i_gti << " GTI" << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of EHK rows in GTI = " << num_ehk_path9 << ": these rows contribute to exposure time histogram" << std::endl;
  AH_INFO(ahlog::LOW) << "Number of computation loop iterations = " << num_iter << " >= number of EHK rows processed" << std::endl;
  AH_DEBUG << "Execution counts for code paths: " << std::endl;
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  0: %10d  previous EHK time not in GTI; GTI pointer advanced past EHK time gap\n", num_ehk_path0); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  1: %10d  previous EHK time not in GTI; GTI pointer has gone out of range\n", num_ehk_path1); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  2: %10d  previous EHK time not in GTI; GTI pointer is in range\n", num_ehk_path2); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  3: %10d  current EHK time is before current GTI start:  go read next EHK row\n", num_ehk_path3);
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  4: %10d  current EHK time is first one after current GTI start\n", num_ehk_path4); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  5: %10d  fake EHK row used to terminate final GTI\n", num_ehk_path5); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  6: %10d  current EHK time is first one after current GTI stop\n", num_ehk_path6); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  7: %10d  current EHK attitude is in different phi, theta bin from previous\n", num_ehk_path7); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  8: %10d  current EHK time is within current GTI\n", num_ehk_path8); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path  9: %10d  current EHK time is in current GTI and attitude is in current phi, theta bin\n", num_ehk_path9); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 10: %10d  pixel GTI file processed\n", num_ehk_path10); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 11: %10d  current EHK time is in new GTI or attitude is in new phi, theta bin\n", num_ehk_path11); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 12: %10d  current EHK time is in current GTI and attitude is in new phi, theta bin\n", num_ehk_path12); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 13: %10d  current EHK time is outside current GTI\n", num_ehk_path13); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 14: %10d  fake EHK time terminating final GTI has been processed\n", num_ehk_path14); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 15: %10d  final EHK time is within a GTI\n", num_ehk_path15); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 16: %10d  current EHK row is a normal EHK row, not the fake terminating one\n", num_ehk_path16); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 17: %10d  GTI pointer advanced for second or subsequent extra iterations\n", num_ehk_path17); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 18: %10d  GTI pointer advanced for first extra iteration\n", num_ehk_path18); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 19: %10d  GTI skipped over by EHK times\n", num_ehk_path19); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 20: %10d  at least one EHK time in GTI\n", num_ehk_path20); 
  ahlog::debug(__func__, __FILE__, __LINE__,
    "   path 21: %10d  zero-length time interval\n", num_ehk_path21); 

  if (read_pixgti) {
    ahfits::close(fppix);
    AH_INFO(ahlog::HIGH) << "Closed pixel GTI file after processing EHK." << std::endl;
  }

  offaxis_gti.m_num_gti = k_gti;
  keywd.m_ra_pnt = grand_ave_ra_pnt;
  keywd.m_dec_pnt = grand_ave_dec_pnt;
  keywd.m_numtheta = unique_theta_bin.size();

  // Step 8:  Get stable bad pixel data from input file

  // Open file to Primary HDU.

  if (read_badimg) {
    ahfits::open(par.m_badimgfile, "", &fpbad);
    AH_INFO(ahlog::HIGH) << "Opened bad pixel image file " << par.m_badimgfile << std::endl;
    AH_INFO(ahlog::HIGH) << "  (not using instrument map except for keywords)" << std::endl;
  } else {
    ahfits::open(actual_instmap, "INSTMAP", &fpbad);
    AH_INFO(ahlog::HIGH) << "Opened instrument map " << actual_instmap << std::endl;
    AH_INFO(ahlog::HIGH) << "  (not using bad pixel image file)" << std::endl;
  }

  // Read the bad pixel or instrument map image.

  ahfits::readImage(fpbad, badpix_map, length_x, length_y);
  AH_INFO(ahlog::LOW) << "X dim=" << length_x << "; Y dim=" << length_y << std::endl;
  flag_image.resize(length_y);
  for (long cy=0; cy<length_y; ++cy) flag_image[cy].resize(length_x);

  // Make flag pixel image

  for (long cx=0; cx<length_x; ++cx) {
    for (long cy=0; cy<length_y; ++cy) {
      long tmp = badpix_map[cy][cx];

      flag_image[cy][cx] = ahexpmap::e_PIX_INACT;

      // Save image pixel value into tmp.

      if (read_badimg) {

        // Input badpix values:
        // 0: good pixel
        // 1: calibration source pixel
        // 2: bad pixel
        // -1: null, out of detector or area discrimination

        if (tmp == 0) {
          flag_image[cy][cx] = ahexpmap::e_PIX_GOOD;
        } else if (tmp == 1) {
          if (par.m_maskcalsrc) {
            flag_image[cy][cx] = ahexpmap::e_PIX_BAD;  // cal src is removed
          } else {
            flag_image[cy][cx] = ahexpmap::e_PIX_GOOD; // cal src is not removed
          }
        } else if (tmp == 2) {
          flag_image[cy][cx] = ahexpmap::e_PIX_BAD;
        } else if (tmp == -1) {
          flag_image[cy][cx] = ahexpmap::e_PIX_INACT;
        } else {
          flag_image[cy][cx] = ahexpmap::e_PIX_INACT;
          AH_INFO(ahlog::HIGH) << "image value is supposed to be 0, 1, 2, or -1." << std::endl;
        }
      } else {

        // Input instrument map values:
        // 0: out of FoV
        // 1: in FoV

        if (tmp == 0) {
          flag_image[cy][cx] = ahexpmap::e_PIX_INACT;
        } else if (tmp == 1) {
          flag_image[cy][cx] = ahexpmap::e_PIX_GOOD;
        } else {
          flag_image[cy][cx] = ahexpmap::e_PIX_INACT;
          AH_INFO(ahlog::HIGH) << "image value is supposed to be 0 or 1." << std::endl;
        }
      }
    }
  }

  // Step 9:  Get location of flickering pixel (any pixels which have partial bad time
  // interval) and update the values to 0
  // ahfits::FilePtr fpdbg=0;
  // ahfits::create("debug_image.fits", "", &fpdbg);
  // ahfits::writeImage(fpdbg, "", flag_image, 0.0, 1.0, 0);
  // ahfits::close(fpdbg);

  if (read_pixgti) {
    if (inst == "SXS") {
      defpixgtiext = ahexpmap::SXSDEFPIXGTI;
    } else {
      defpixgtiext = ahexpmap::DEFPIXGTI;
    }
    ahfits::open(par.m_pixgtifile, "", &fppix);

    // Move to default extension for instrument if
    // extended syntax not used.
    if (ahfits::isPrimary(fppix)) ahfits::move(fppix, defpixgtiext);
    AH_INFO(ahlog::HIGH) << "Opened pixel GTI file " << par.m_pixgtifile 
      << " to set up flag_image" << std::endl;
    rpix = new ahfits::Router(fppix);
    rpix->connectScalar(ahfits::e_READONLY, "DETX", l_detx);
    rpix->connectScalar(ahfits::e_READONLY, "DETY", l_dety);
    for (ahfits::firstRow(fppix); ahfits::readOK(fppix); ahfits::nextRow(fppix)) {
      readRow(fppix);

      AH_INFO(ahlog::LOW) << "flag_image[" << l_dety-1 << "][" << l_detx-1 << "]=" << flag_image[l_dety-1][l_detx-1] << std::endl;
      if (ahexpmap::e_PIX_INACT == flag_image[l_dety-1][l_detx-1]) {
        AH_INFO(ahlog::HIGH) << "Pixel " << l_detx << " " << l_dety << " is both INACTIVE and present in PIXEL_GTI" << std::endl;
        AH_INFO(ahlog::HIGH) << "INACTIVE status prevails - pixel flagged INACTIVE " << std::endl;
      } else {
        if (ahexpmap::e_PIX_BAD == flag_image[l_dety-1][l_detx-1]) {
          AH_INFO(ahlog::HIGH) << "Pixel " << l_detx << " " << l_dety << " is both BAD and present in PIXEL_GTI" << std::endl;
          AH_INFO(ahlog::HIGH) << "BAD status prevails - pixel flagged BAD " << std::endl;
        } else {
          AH_INFO(ahlog::LOW) << "Pixel " << l_detx << " " << l_dety << " flagged for partial exposure" << std::endl;
          flag_image[l_dety-1][l_detx-1] = ahexpmap::e_PIX_PARTIAL;  // Partial exposure in pixel
        }
      }
    }
    delete rpix;
    ahfits::close(fppix);
    AH_INFO(ahlog::HIGH) << "Closed pixel GTI file" << std::endl;
  }

  // Open output file

  ahfits::create(par.m_outfile, "", &fpout);
  AH_INFO(ahlog::HIGH) << "Created output file " << par.m_outfile << std::endl;

  // Write header keywords

  // This call selects primary-header keywords, a superset of those
  // specifically required.

  ahmission::keyword::copyAllKeywords(fpehk, fpout, ahmission::keyword::e_PRIMARY);

  ahfits::writeKeyValStr(fpout, "TELESCOP", keywd.m_telescop, "Telescope mission name");
  ahfits::writeKeyValStr(fpout, "INSTRUME", keywd.m_instrume, "Instrument name");
  ahfits::writeKeyValStr(fpout, "DETNAM", keywd.m_detnam, "Detector subsystem");

  // Instrument map keywords - these will be retained for HXI, but
  //   overwritten later for SXI or SXS
  ahfits::writeKeyValStr(fpout, "CTYPE1", keywd.m_ctype1, "Coordinate axis");
  ahfits::writeKeyValStr(fpout, "CUNIT1", keywd.m_cunit1, "Axis unit");
  ahfits::writeKeyValDbl(fpout, "CRVAL1", keywd.m_crval1, "Reference value");
  ahfits::writeKeyValDbl(fpout, "CRPIX1", keywd.m_crpix1, "Reference point");
  ahfits::writeKeyValDbl(fpout, "CDELT1", keywd.m_cdelt1, "Coordinate increment");
  ahfits::writeKeyValStr(fpout, "CTYPE2", keywd.m_ctype2, "Coordinate axis");
  ahfits::writeKeyValStr(fpout, "CUNIT2", keywd.m_cunit1, "Axis unit");
  ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_crval2, "Reference value");
  ahfits::writeKeyValDbl(fpout, "CRPIX2", keywd.m_crpix2, "Reference point");
  ahfits::writeKeyValDbl(fpout, "CDELT2", keywd.m_cdelt2, "Coordinate increment");

  // Step 10:  Create and output histogram to 1st extension

  // Create new extension
  
  ahfits::addEmptyTbl(fpout, "OFFAXISHIST");

  // Write header keywords

  // This call selects event-oriented keywords, a superset of those
  // specifically required.

  ahmission::keyword::copyAllKeywords(fpehk, fpout, ahmission::keyword::e_EVENT);

  // Make sure the keywords specifically required are set correctly.

  ahfits::writeKeyValStr(fpout, "TELESCOP", keywd.m_telescop, "Telescope mission name");
  ahfits::writeKeyValStr(fpout, "INSTRUME", keywd.m_instrume, "Instrument name");
  ahfits::writeKeyValStr(fpout, "DETNAM", keywd.m_detnam, "Detector subsystem");
  ahfits::writeKeyValDbl(fpout, "RA_PNT", keywd.m_ra_pnt, "[deg] Mean RA of optical axis");
  ahfits::writeKeyValDbl(fpout, "DEC_PNT", keywd.m_dec_pnt, "[deg] Mean Dec of optical axis");
  ahfits::writeKeyValDbl(fpout, "RA_NOM", keywd.m_ra_nom, "[deg] Mean RA of pointing");
  ahfits::writeKeyValDbl(fpout, "DEC_NOM", keywd.m_dec_nom, "[deg] Mean Dec of pointing");
  ahfits::writeKeyValDbl(fpout, "PA_NOM", keywd.m_pa_nom, "[deg] Mean roll of pointing");
  ahfits::writeKeyValDbl(fpout, "RA_OBJ", keywd.m_ra_src, "[deg] RA of target object");
  ahfits::writeKeyValDbl(fpout, "DEC_OBJ", keywd.m_dec_src, "[deg] Dec of target object");


  if (inst == "HXI1" || inst == "HXI2" || inst == "SXI") {
    ahfits::writeKeyValDbl(fpout, "RAW_XSIZ", keywd.m_raw_xsiz, "[pixels] RAW address space X size");
    ahfits::writeKeyValDbl(fpout, "RAW_XSCL", keywd.m_raw_xscl, "[mm/pixel] RAW X scale");
    ahfits::writeKeyValDbl(fpout, "RAW_YSCL", keywd.m_raw_yscl, "[mm/pixel] RAW Y scale");
  }
  ahfits::writeKeyValLLong(fpout, "ACT_XSIZ", keywd.m_act_xsiz, "[pixels] ACT address space X size");
  ahfits::writeKeyValLLong(fpout, "ACT_YSIZ", keywd.m_act_ysiz, "[pixels] ACT address space Y size");
  ahfits::writeKeyValDbl(fpout, "ACT_XSCL", keywd.m_act_xscl, "[mm/pixel] ACT scale, X");
  ahfits::writeKeyValDbl(fpout, "ACT_YSCL", keywd.m_act_yscl, "[mm/pixel] ACT scale, Y)");
  ahfits::writeKeyValLLong(fpout, "DET_XSIZ", keywd.m_det_xsiz, "[pixels] DET address space X size");
  ahfits::writeKeyValLLong(fpout, "DET_YSIZ", keywd.m_det_ysiz, "[pixels] DET address space Y size");
    ahfits::writeKeyValDbl(fpout, "DET_XSCL", keywd.m_det_xscl, 
      "DET address space mm per x det unit (mm/pixel)");
    ahfits::writeKeyValDbl(fpout, "DET_YSCL", keywd.m_det_yscl, 
      "DET address space mm per y det unit (mm/pixel)");
  ahfits::writeKeyValLLong(fpout, "DETXFLIP", keywd.m_detxflip, "flip of DET X axis relative to ACT");
  ahfits::writeKeyValLLong(fpout, "DETYFLIP", keywd.m_detyflip, "flip of DET Y axis relative to ACT");
  ahfits::writeKeyValLLong(fpout, "FOC_XSIZ", keywd.m_foc_xsiz, "[pixels] FOC address space X size");
  ahfits::writeKeyValLLong(fpout, "FOC_YSIZ", keywd.m_foc_ysiz, "[pixels] FOC address space Y size");
  ahfits::writeKeyValDbl(fpout, "FOC_XSCL", keywd.m_foc_xscl, "[mm/pixel] FOC X scale");
  ahfits::writeKeyValDbl(fpout, "FOC_YSCL", keywd.m_foc_yscl, "[mm/pixel] FOC Y scale");
  ahfits::writeKeyValDbl(fpout, "FOC_XOFF", keywd.m_foc_xoff, "[pixels] DETX offset to the FOC center position");
  ahfits::writeKeyValDbl(fpout, "FOC_YOFF", keywd.m_foc_yoff, "[pixels] DETY offset to the FOC center position");
  ahfits::writeKeyValDbl(fpout, "FOC_ROTD", keywd.m_foc_rotd, "[deg] DET rotation angle in FOC coordinates");
  ahfits::writeKeyValDbl(fpout, "OPTAXISX", keywd.m_optaxisx, 
    "[pixels] optical axis x in DET coordinates");
  ahfits::writeKeyValDbl(fpout, "OPTAXISY", keywd.m_optaxisy, 
    "[pixels] optical axis y in DET coordinates");
  ahfits::writeKeyValLLong(fpout, "OPTXFLIP", keywd.m_optxflip, "flip of telescope axes relative to DETX/Y");
  ahfits::writeKeyValLLong(fpout, "OPTYFLIP", keywd.m_optyflip, "flip from (look-down) to (look-up)");
  ahfits::writeKeyValDbl(fpout, "OPT_ROTD", keywd.m_opt_rotd, "rotation of telescope output system wrt DET");
  ahfits::writeKeyValDbl(fpout, "FOCALLEN", keywd.m_focallen, "Instrument focal length (mm)");

  if (par.m_instrume == "SXS") {
    ahfits::writeKeyValDbl(fpout, "SXSPXWID", keywd.m_sxspxwid, 
      "pixel width (mm)");
    ahfits::writeKeyValDbl(fpout, "SXSPXGAP", keywd.m_sxspxgap, 
      "gap between pixels (mm)");
    ahfits::writeKeyValStr(fpout, "GATEVALV", keywd.m_gatevalv, "Gatevalve state");
    ahfits::writeKeyValStr(fpout, "FILTER", keywd.m_filter, "Filter state");
  }

  // Instrument map keywords:  RAW for HXI, DET for SXI/SXS.
  // These are needed downstream by ahsxtarfgen or hxirspeffimg.
  ahfits::writeKeyValStr(fpout, "CTYPE1", keywd.m_ctype1, "Coordinate axis");
  ahfits::writeKeyValStr(fpout, "CUNIT1", keywd.m_cunit1, "Axis unit");
  ahfits::writeKeyValDbl(fpout, "CRVAL1", keywd.m_crval1, "Reference value");
  ahfits::writeKeyValDbl(fpout, "CRPIX1", keywd.m_crpix1, "Reference point");
  ahfits::writeKeyValDbl(fpout, "CDELT1", keywd.m_cdelt1, "Coordinate increment");
  ahfits::writeKeyValStr(fpout, "CTYPE2", keywd.m_ctype2, "Coordinate axis");
  ahfits::writeKeyValStr(fpout, "CUNIT2", keywd.m_cunit2, "Axis unit");
  ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_crval2, "Reference value");
  ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_crval2, "Reference value");
  ahfits::writeKeyValDbl(fpout, "CRPIX2", keywd.m_crpix2, "Reference point");
  ahfits::writeKeyValDbl(fpout, "CDELT2", keywd.m_cdelt2, "Coordinate increment");

  AH_INFO(ahlog::HIGH) << "CTYPE1 = " << keywd.m_ctype1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CUNIT1 = " << keywd.m_cunit1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRVAL1 = " << keywd.m_crval1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRPIX1 = " << keywd.m_crpix1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CDELT1 = " << keywd.m_cdelt1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CTYPE2 = " << keywd.m_ctype2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CUNIT2 = " << keywd.m_cunit2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRVAL2 = " << keywd.m_crval2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CRPIX2 = " << keywd.m_crpix2 << std::endl;
  AH_INFO(ahlog::HIGH) << "CDELT2 = " << keywd.m_cdelt2 << std::endl;

  ahfits::insertColAfter(fpout, "OFFAXISLO", "1D");
  ahfits::setColumnDescription(fpout, "OFFAXISLO", "Lower bound of theta bin");
  ahfits::insertColAfter(fpout, "OFFAXISHI", "1D");
  ahfits::setColumnDescription(fpout, "OFFAXISHI", "Upper bound of theta bin");
  ahfits::insertColAfter(fpout, "OFFAXISVAL", "1D");
  ahfits::setColumnDescription(fpout, "OFFAXISVAL", "Average theta in bin");
  ahfits::insertColAfter(fpout, "AZIMUTHLO", "1D");
  ahfits::setColumnDescription(fpout, "AZIMUTHLO", "Lower bound of phi bin");
  ahfits::insertColAfter(fpout, "AZIMUTHHI", "1D");
  ahfits::setColumnDescription(fpout, "AZIMUTHHI", "Upper bound of phi bin");
  ahfits::insertColAfter(fpout, "AZIMUTH", "1D");
  ahfits::setColumnDescription(fpout, "AZIMUTH", "Average phi in bin");
  ahfits::insertColAfter(fpout, "TIMEINTERVAL", "1D");
  ahfits::setColumnDescription(fpout, "TIMEINTERVAL", "Time with pointing in bin");
  ahfits::insertColAfter(fpout, "FRACTION", "1D");
  ahfits::setColumnDescription(fpout, "FRACTION", "Fraction of time in bin");
  ahfits::insertColAfter(fpout, "RANOMXP", "1D");
  ahfits::setColumnDescription(fpout, "RANOMXP", "Average spacecraft RA in bin");
  ahfits::insertColAfter(fpout, "DECNOMXP", "1D");
  ahfits::setColumnDescription(fpout, "DECNOMXP", "Average spacecraft Dec in bin");
  ahfits::insertColAfter(fpout, "PA_NOMXP", "1D");
  ahfits::setColumnDescription(fpout, "PA_NOMXP", "Average spacecraft Roll in bin");
  ahfits::insertColAfter(fpout, "RA_PNT", "1D");
  ahfits::setColumnDescription(fpout, "RA_PNT", "Average optical axis RA in bin");
  ahfits::insertColAfter(fpout, "DEC_PNT", "1D");
  ahfits::setColumnDescription(fpout, "DEC_PNT", "Average optical axis Dec in bin");

  rout = new ahfits::Router(fpout);

  rout->connectScalar(ahfits::e_WRITEONLY, "OFFAXISLO", l_offaxislo);
  rout->connectScalar(ahfits::e_WRITEONLY, "OFFAXISHI", l_offaxishi);
  rout->connectScalar(ahfits::e_WRITEONLY, "OFFAXISVAL", l_offaxisval);
  rout->connectScalar(ahfits::e_WRITEONLY, "AZIMUTHLO", l_azimuthlo);
  rout->connectScalar(ahfits::e_WRITEONLY, "AZIMUTHHI", l_azimuthhi);
  rout->connectScalar(ahfits::e_WRITEONLY, "AZIMUTH", l_azimuthval);
  rout->connectScalar(ahfits::e_WRITEONLY, "TIMEINTERVAL", l_timeinterval);
  rout->connectScalar(ahfits::e_WRITEONLY, "FRACTION", l_fraction);
  rout->connectScalar(ahfits::e_WRITEONLY, "RANOMXP", l_ranomxp);
  rout->connectScalar(ahfits::e_WRITEONLY, "DECNOMXP", l_decnomxp);
  rout->connectScalar(ahfits::e_WRITEONLY, "PA_NOMXP", l_pa_nomxp);
  rout->connectScalar(ahfits::e_WRITEONLY, "RA_PNT", l_ra_pnt);
  rout->connectScalar(ahfits::e_WRITEONLY, "DEC_PNT", l_dec_pnt);

  ahfits::firstRow(fpout);
  num_expo = 0;
  for (long i=0; i<ngrid_wedge; ++i) {
    l_timeinterval = expo[i][0];
    // Go to the next wedge if this one has no exposure
    if (l_timeinterval <= 0) {
      AH_INFO(ahlog::LOW) << "Wedge bin " << i << " contains no exposure, will not write histogram row." << std::endl;
      continue;
    }
    // Increment the exposure map number.  This is used rather than i since we might skip some.
    num_expo++;
    l_fraction = l_timeinterval/total_exp;
    fraction.push_back(l_fraction);  // Save into argument for exposure map scaling.
    // Get the min,max of theta,phi from the array index.
    getWedgeFromIndex(i, min_theta_bin, par.m_delta, par.m_numphi, l_offaxislo, l_offaxishi, l_azimuthlo, l_azimuthhi);
    // Store theta and phi corresponding to the mean coordinates in the bin
    // (do not store mean theta and phi).
    calcDeltaAttitude(keywd.m_ra_src, ave_ra_pnt[i],  ave_ra[i], 
      keywd.m_dec_src, ave_dec_pnt[i], ave_dec[i], ave_roll[i],
      l_offaxisval, l_azimuthval);
    // l_offaxisval = ave_theta[i];
    // l_azimuthval = ave_phi[i];
    l_ranomxp = ave_ra[i];
    l_decnomxp = ave_dec[i];
    l_pa_nomxp = ave_roll[i];
    l_ra_pnt = ave_ra_pnt[i];
    l_dec_pnt = ave_dec_pnt[i];
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);
  }

  ahfits::writeKeyValLLong(fpout, "NUMEXMAP", num_expo, "Number of exposure maps");
  ahfits::writeKeyValLLong(fpout, "NUMTHETA", keywd.m_numtheta, "Number of unique theta bins");
  ahfits::writeKeyValDbl(fpout, "EXPOSURE", total_exp, "Total exposure");

  delete rout;

  // Step 11:  Create and coadd output expomap images for primary HDU; in same loop, create
  // and write GTI extensions.

  if (inst == "SXI" || inst == "SXS") {
    
    // Allocate storage for image corresponding to (theta, phi) bin (DET size).
    offaxis_image.resize(length_y);
    for (long cy=0; cy<length_y; ++cy) {
      offaxis_image[cy].resize(length_x);
      for (long cx=0; cx<length_x; ++cx) {
        offaxis_image[cy][cx] = 0.0;
      }
    }

    // Allocate storage for output image (DET or FOC/SKY size).
    if (ahgen::strtoupper(par.m_stopsys) == "DET") {
      output_image.resize(length_y);
      for (long cy=0; cy<length_y; ++cy) {
        output_image[cy].resize(length_x);
        for (long cx=0; cx<length_x; ++cx) {
          output_image[cy][cx] = 0.0;
        }
      }
    } else {
      output_image.resize(keywd.m_foc_ysiz);
      for (long cy=0; cy<keywd.m_foc_ysiz; ++cy) {
        output_image[cy].resize(keywd.m_foc_xsiz);
        for (long cx=0; cx<keywd.m_foc_xsiz; ++cx) {
          output_image[cy][cx] = 0.0;
        }
      }
    }

    // Allocate temporary image and parameter structures for imagetranslib
    // routines.

    trans_param = (IMAGETRANS*)std::calloc(1, sizeof(IMAGETRANS));  // imagetrans params
    trans_param->combo = allocateComboXform();

    original = allocateImage(length_x, length_y, DOUBLE_IMG);
    transformed = allocateImage(keywd.m_foc_xsiz, keywd.m_foc_ysiz, DOUBLE_IMG);

    // Zero.
    for (long cx=0; cx<length_x; cx++) {
      for (long cy=0; cy<length_y; cy++) {
        setImagePixel(original, cx, cy, 0.0);
      }
    }
    for (long cx=0; cx<keywd.m_foc_xsiz; cx++) {
      for (long cy=0; cy<keywd.m_foc_ysiz; cy++) {
        setImagePixel(transformed, cx, cy, 0.0);
      }
    }

  }  // End if inst == SXI or SXS

  // Loop through the wedges and save GTI and partial pixel exposure tables.
  num_expo = 0;

  sum_max_oi = 0.0;
  for (long i=0; i<ngrid_wedge; i++) {

    timeinterval = expo[i][0];
    max_oi = -1e99;

    // Go to the next wedge if this one has no exposure
    if (timeinterval <= 0) {
      AH_INFO(ahlog::LOW) << "Wedge bin " << i << " contains no exposure, will not write GTIEXP or PARTIALEXP." << std::endl;
      continue;
    }

    // Increment the exposure map number.  This is used rather than i since we might skip some.
    num_expo++;

    if (inst == "SXI" || inst == "SXS") {

      for (long cx=0; cx<length_x; ++cx) {
        for (long cy=0; cy<length_y; ++cy) {
          if (flag_image[cy][cx] == ahexpmap::e_PIX_BAD) {  // Bad pixel
            offaxis_image[cy][cx] = 0.0;    // Zero because bad
          } else if (flag_image[cy][cx] == ahexpmap::e_PIX_PARTIAL) {  // Partial exposure
            offaxis_image[cy][cx] = 0.0;    // Zero for accumulation
          } else if (flag_image[cy][cx] == ahexpmap::e_PIX_GOOD) {  // Good pixel
            offaxis_image[cy][cx] = 1.0;    // One
          } else if (flag_image[cy][cx] == ahexpmap::e_PIX_INACT) { // Outside active area
            offaxis_image[cy][cx] = 0.0;    // Zero
          } else { 
            AH_THROW_LOGIC("Improper flag_image value; exiting");
          }
        }
      }

      // Account for partial exposure of pixels via pixel GTI.
      if (read_pixgti && (timeinterval > 0)) {
        if (inst == "SXS") {
          defpixgtiext = ahexpmap::SXSDEFPIXGTI;
        } else {
          defpixgtiext = ahexpmap::DEFPIXGTI;
        }
        ahfits::open(par.m_pixgtifile, "", &fppix);

        // Move to default extension for instrument if
        // extended syntax not used.
        if (ahfits::isPrimary(fppix)) ahfits::move(fppix, defpixgtiext);
        AH_INFO(ahlog::HIGH) << "Opened pixel GTI file " << par.m_pixgtifile 
          << " to calculate exposure map values" << std::endl;
        rpix = new ahfits::Router(fppix);
        rpix->connectScalar(ahfits::e_READONLY, "DETX", l_detx);
        rpix->connectScalar(ahfits::e_READONLY, "DETY", l_dety);
        ahfits::IndexType j = 1;
        for (ahfits::firstRow(fppix); ahfits::readOK(fppix); ahfits::nextRow(fppix)) {
          readRow(fppix);
          if (flag_image[l_dety-1][l_detx-1] == ahexpmap::e_PIX_PARTIAL) {
            if (offaxis_image[l_dety-1][l_detx-1] == 0.0) {
              offaxis_image[l_dety-1][l_detx-1] = expo[i][j]/timeinterval;
              AH_INFO(ahlog::LOW) << "pixel=0:  Stored " << expo[i][j]/timeinterval << " in DET pixel " << l_detx 
                << " " << l_dety << " in offaxis_image; expo[i][j]=" << expo[i][j]
                << " timeinterval=" << timeinterval 
                << " wedge bin=" << i << " pixel list entry=" << j << std::endl;
            } else if (offaxis_image[l_dety-1][l_detx-1] > 0.0 && offaxis_image[l_dety-1][l_detx-1] <= 1.0) {
              bad_time_fraction = 1.0 - offaxis_image[l_dety-1][l_detx-1];
              additional_bad_time_fraction = 1.0 - expo[i][j]/timeinterval;
              new_good_time_fraction = 1.0 - (bad_time_fraction + additional_bad_time_fraction);
              AH_DEBUG << "bad_time_fraction=" << bad_time_fraction << " additional_bad_time_fraction=" <<
                additional_bad_time_fraction << std::endl;
              offaxis_image[l_dety-1][l_detx-1] = new_good_time_fraction;
              AH_INFO(ahlog::LOW) << "pixel!=0: Stored " << new_good_time_fraction << " in DET pixel " << l_detx 
                << " " << l_dety << " in offaxis_image; expo[i][j]=" << expo[i][j]
                << " timeinterval=" << timeinterval 
                << " wedge bin=" << i << " pixel list entry=" << j << std::endl;
              AH_INFO(ahlog::LOW) << "Note: multiple GTI for one pixel must be disjoint or this result is invalid." << std::endl;
            }
            if (offaxis_image[l_dety-1][l_detx-1] < 0.0 - frac_eps || offaxis_image[l_dety-1][l_detx-1] > 1.0 + frac_eps) {
              AH_THROW_RUNTIME("Partial pixel total is less than 0 or greater than 1.  Exiting.");
            }
          }
          ++j;
        }
        delete rpix;
        ahfits::close(fppix);
        AH_INFO(ahlog::HIGH) << "Closed pixel GTI file" << std::endl;
      }
    } // End if SXI or SXS

    if (!do_efficiency) {  // Do only in EXPOSURE mode.

      // Output GTI BINTABLE

      // Create BINTABLE column structure: START, STOP

      ss.str("");
      ss << "GTIEXP" << std::setw(3) << std::setfill('0') << num_expo;
      ahfits::addEmptyTbl(fpout, ss.str());

      ahfits::writeKeyValStr(fpout, "TELESCOP", keywd.m_telescop, "Telescope mission name");
      ahfits::writeKeyValStr(fpout, "INSTRUME", keywd.m_instrume, "Instrument name");
      ahfits::writeKeyValStr(fpout, "DETNAM", keywd.m_detnam, "Detector subsystem");
      ahfits::writeKeyValLLong(fpout, "GTINUMBR", num_expo, "Number of GTI table");

      ahfits::insertColAfter(fpout, "START", "1D");
      ahfits::setColumnDescription(fpout, "START", "Lower bound of GTI");
      ahfits::setTUnit(fpout, "START", "s");
      ahfits::insertColAfter(fpout, "STOP", "1D");
      ahfits::setColumnDescription(fpout, "STOP", "Upper bound of GTI");
      ahfits::setTUnit(fpout, "STOP", "s");

      routgti = new ahfits::Router(fpout);
      routgti->connectScalar(ahfits::e_WRITEONLY, "START", l_start);
      routgti->connectScalar(ahfits::e_WRITEONLY, "STOP", l_stop);

      ahfits::firstRow(fpout);
      offaxis_gti_min_start = 1e25;
      offaxis_gti_max_stop = -1e25;
      for (long k=0; k<offaxis_gti.m_num_gti; ++k) {

        if (i == offaxis_gti.m_theta_bin[k]) {

          AH_INFO(ahlog::LOW) << "Selecting offaxis GTI i=" << i << " k=" << k
            << "  offaxis_gti.m_theta_bin[k]=" << offaxis_gti.m_theta_bin[k] << std::endl;

          l_start = offaxis_gti.m_start[k];
          if (l_start < offaxis_gti_min_start) {
            offaxis_gti_min_start = l_start;
          }
          l_stop = offaxis_gti.m_stop[k];
          if (l_stop > offaxis_gti_max_stop) {
            offaxis_gti_max_stop = l_stop;
          }
          ahfits::writeRow(fpout);
          ss.str("");
          ss << std::setprecision(15) << "Wrote offaxis GTI row l_start=" 
            << l_start << " l_stop=" << l_stop;
          AH_INFO(ahlog::LOW) << ss.str() << std::endl;
          ahfits::nextRow(fpout);
        }
      }

      ahfits::writeKeyValDbl(fpout, "TSTART", offaxis_gti_min_start, "Starting time of all GTI");
      ahfits::writeKeyValDbl(fpout, "TSTOP", offaxis_gti_max_stop, "Ending time of all GTI");

      delete routgti;

      ahfits::close(fpout);

      if (inst == "SXI" || inst == "SXS") {

        // Output partial pixel BINTABLE.
        // Create BINTABLE column structure: DETX, DETY, FRACTION.
        ahfits::open(par.m_outfile, "", &fpout);

        ss.str("");
        ss << "PARTIALEXP" << std::setw(3) << std::setfill('0') << num_expo;
        ahfits::addEmptyTbl(fpout, ss.str());

        ahfits::writeKeyValStr(fpout, "TELESCOP", keywd.m_telescop, "Telescope mission name");
        ahfits::writeKeyValStr(fpout, "INSTRUME", keywd.m_instrume, "Instrument name");
        ahfits::writeKeyValStr(fpout, "DETNAM", keywd.m_detnam, "Detector subsystem");
        ahfits::writeKeyValLLong(fpout, "GTINUMBR", num_expo, "Number of GTI table");

        ahfits::insertColAfter(fpout, "DETX", "1I");
        ahfits::setColumnDescription(fpout, "DETX", "DET X coordinate");
        ahfits::insertColAfter(fpout, "DETY", "1I");
        ahfits::setColumnDescription(fpout, "DETY", "DET Y coordinate");
        ahfits::insertColAfter(fpout, "FRACTION", "1E");
        ahfits::setColumnDescription(fpout, "FRACTION", "Fractional exposure time");

        routpartial = new ahfits::Router(fpout);
        routpartial->connectScalar(ahfits::e_WRITEONLY, "DETX", l_detx_out);
        routpartial->connectScalar(ahfits::e_WRITEONLY, "DETY", l_dety_out);
        routpartial->connectScalar(ahfits::e_WRITEONLY, "FRACTION", l_fraction_out);

        ahfits::firstRow(fpout);

        // First, write zero fraction rows for bad/inactive pixels.
        for (long cx=0; cx<length_x; cx++) {
          for (long cy=0; cy<length_y; cy++) {
            // Both bad and inactive pixels have to be tabulated
            // for ARF normalization downstream.
            if (flag_image[cy][cx] == ahexpmap::e_PIX_BAD || flag_image[cy][cx] == ahexpmap::e_PIX_INACT) {
              // Bad pixel, zero exposure
              l_detx_out = cx + 1;
              l_dety_out = cy + 1;
              l_fraction_out = 0.0;
              ahfits::writeRow(fpout);
              ahfits::nextRow(fpout);
              //AH_INFO(ahlog::LOW) << "Wrote bad pixel " << cx << "," << cy << " fraction=" << l_fraction_out << std::endl;
            } else if (flag_image[cy][cx] == ahexpmap::e_PIX_PARTIAL) {
              // The corresponding offaxis_image
              // pixel already contains the fractional exposure
              // time relative to exposure time of wedge.
              AH_DEBUG << "For partial pixel, offaxis_image[" << cy << "][" << cx << "]=" << offaxis_image[cy][cx] << std::endl;
              if (offaxis_image[cy][cx] < 1.0) {
                l_detx_out = cx + 1;
                l_dety_out = cy + 1;
                l_fraction_out = offaxis_image[cy][cx];
                ahfits::writeRow(fpout);
                ahfits::nextRow(fpout);
                AH_INFO(ahlog::LOW) << "Wrote partial pixel " << cx << "," << cy << " fraction=" << l_fraction_out << std::endl;
              }
            } // Fully good pixels not treated.
          }
        }

        delete routpartial;
        ahfits::close(fpout);

      }  // End if SXI or SXS

    } // End if EXPOSURE mode

    if (inst == "SXI" || inst == "SXS") {
      if (do_efficiency) {

        // Apply the efficiency conversion (e.g., QE, contamination, vignetting),
        // together with fractional time scaling.
        for (long cx=0; cx<keywd.m_det_xsiz; cx++) {
          for (long cy=0; cy<keywd.m_det_ysiz; cy++) {
            offaxis_image[cy][cx] *= fraction[num_expo-1]*effimage[cx][cy];
            if (offaxis_image[cy][cx] > max_oi) {
              max_oi = offaxis_image[cy][cx];
            }
          }
        }
        //output_img_extname = "EFFICIENCY"; 
      } else {
        // Scale by exposure time to get units of seconds.
        for (long cx=0; cx<keywd.m_det_xsiz; cx++) {
          for (long cy=0; cy<keywd.m_det_ysiz; cy++) {
            offaxis_image[cy][cx] *= timeinterval;
            if (offaxis_image[cy][cx] > max_oi) {
              max_oi = offaxis_image[cy][cx];
            }
          }
        }
        //output_img_extname = "EXPOSURE"; 
      }

      sum_max_oi += max_oi;
      AH_INFO(ahlog::LOW) << "i=" << i << " num_expo=" << num_expo << " max(offaxis_image)=" << max_oi 
        << " sum=" << sum_max_oi << std::endl;

      if (ahgen::strtoupper(par.m_stopsys) == "DET") {

        pixelarearatio = 1.0;

        // For DET image, accumulate only; no geometric transform.

        for (long cx=0; cx<keywd.m_det_xsiz; cx++) {
          for (long cy=0; cy<keywd.m_det_ysiz; cy++) {
            output_image[cy][cx] += offaxis_image[cy][cx];
          }
        }

      } else { // stopsys == FOC or SKY
        
        pixelarearatio = keywd.m_focscaleratio*keywd.m_focscaleratio;

        // Transform the component exposure or efficiency map to the
        // FOC or SKY frame and accumulate into total image.
        //
        // This computation is done using coordfits library routines together 
        // with routines copied from the task imagetrans.  

        // Compute the required transform.
        computeXform2dDetToFocOrSky (par.m_stopsys, 
          trans_param->combo->trans,
          keywd.m_cendet, keywd.m_foc_xoff, keywd.m_foc_yoff,
          keywd.m_focscaleratio, keywd.m_foc_rotd, keywd.m_cenfoc,
          ahexpmap::ROLL_SIGN, keywd.m_ra_nom, keywd.m_dec_nom,
          ave_ra[i], ave_dec[i], ave_roll[i],
          keywd.m_censky, keywd.m_pixels_per_radian);

        // Copy offaxis image into the required input structure.
        for (long cx=0; cx<length_x; cx++) {
          for (long cy=0; cy<length_y; cy++) {
            setImagePixel(original, cx, cy, offaxis_image[cy][cx]);
            //std::cout << i << " " << cx << " " << cy << " " << offaxis_image[cy][cx] << std::endl;
          }
        }

        // This call accumulates values.
          
        transform_by_area(original, transformed, trans_param);

      } // End if stopsys == FOC or SKY

      AH_INFO(ahlog::HIGH) << "pixelarearatio=" << pixelarearatio << std::endl;

    } // End if SXI or SXS

    ahfits::open(par.m_outfile, "", &fpout);

  } // End loop through wedges


  if (inst == "SXI" || inst == "SXS") {

    // The "transformed" structure contains accumulated values and needs
    // to be copied to the write form for writing.
    if (ahgen::strtoupper(par.m_stopsys) != "DET") {
      for (long cx=0; cx<keywd.m_foc_xsiz; cx++) {
        for (long cy=0; cy<keywd.m_foc_ysiz; cy++) {
          output_image[cy][cx] += pixelarearatio*getImagePixel(transformed, cx, cy);
          if (output_image[cy][cx] > max_out) {
            max_out = output_image[cy][cx];
          }
        }
      }
    }

    AH_INFO(ahlog::LOW) << "max(output_image)=" << max_out << std::endl;

    // Free temporary image and parameter structures.
    destroyImage(original);
    destroyImage(transformed);
    destroyComboXform(trans_param->combo);
    free(trans_param);

    // Step 12:  Write image into primary HDU.

    ahfits::writeImage(fpout, "", output_image, 0.0, 1.0, 0);

    // Set keywords.

    ahfits::writeKeyValStr(fpout, "TELESCOP", keywd.m_telescop, "Telescope mission name");
    ahfits::writeKeyValStr(fpout, "INSTRUME", keywd.m_instrume, "Instrument name");
    ahfits::writeKeyValStr(fpout, "DETNAM", keywd.m_detnam, "Detector subsystem");
    ahfits::writeKeyValDbl(fpout, "RA_NOM", keywd.m_ra_nom, "[deg] Avg aspect point R.A.");
    ahfits::writeKeyValDbl(fpout, "DEC_NOM", keywd.m_dec_nom, "[deg] Avg aspect point Dec");
    ahfits::writeKeyValDbl(fpout, "PA_NOM", keywd.m_pa_nom, "[deg] Avg position angle (roll)");
    ahfits::writeKeyValDbl(fpout, "RA_PNT", keywd.m_ra_pnt, "[deg] Avg Optical axis R.A.");
    ahfits::writeKeyValDbl(fpout, "DEC_PNT", keywd.m_dec_pnt, "[deg] Avg Optical axis Dec");
    ahfits::writeKeyValLLong(fpout, "NUMEXMAP", num_expo, "Number of component maps");
    ahfits::writeKeyValLLong(fpout, "NUMTHETA", num_expo, "Number of unique theta bins");

    // WCS keywords overwrite previously set ones that were from instrument map
    if (ahgen::strtoupper(par.m_stopsys) == "SKY") {
      ahfits::writeKeyValStr(fpout, "CTYPE1", "RA---TAN", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT1", "deg", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL1", keywd.m_ra_nom, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX1", keywd.m_censky, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT1", keywd.m_xinc_sky, "Coordinate increment");
      ahfits::writeKeyValStr(fpout, "CTYPE2", "DEC--TAN", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT2", "deg", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_dec_nom, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX2", keywd.m_censky, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT2", keywd.m_yinc_sky, "Coordinate increment");
    } else if (ahgen::strtoupper(par.m_stopsys) == "FOC") {
      ahfits::writeKeyValStr(fpout, "CTYPE1", "FOCX", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT1", "pixel", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL1", keywd.m_cenfoc, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX1", keywd.m_cenfoc, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT1", 1, "Coordinate increment");
      ahfits::writeKeyValStr(fpout, "CTYPE2", "FOCY", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT2", "pixel", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_cenfoc, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX2", keywd.m_cenfoc, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT2", 1, "Coordinate increment");
    } else {  // DET
      ahfits::writeKeyValStr(fpout, "CTYPE1", "DETX", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT1", "pixel", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL1", keywd.m_cendet, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX1", keywd.m_cendet, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT1", 1, "Coordinate increment");
      ahfits::writeKeyValStr(fpout, "CTYPE2", "DETY", "Coordinate axis");
      ahfits::writeKeyValStr(fpout, "CUNIT2", "pixel", "Axis unit");
      ahfits::writeKeyValDbl(fpout, "CRVAL2", keywd.m_cendet, "Reference value");
      ahfits::writeKeyValDbl(fpout, "CRPIX2", keywd.m_cendet, "Reference point");
      ahfits::writeKeyValDbl(fpout, "CDELT2", 1, "Coordinate increment");
    }
  }  // End if SXI or SXS

}

// ****************************************************************************

void finalize(ahfits::FilePtr fpehk, ahfits::Router* rehk, 
  ahfits::FilePtr fpbad, ahfits::FilePtr fpout) {

  delete rehk;
  ahfits::close(fpehk);
  AH_INFO(ahlog::HIGH) << "Closed EHK file" << std::endl;
  ahfits::close(fpbad);
  AH_INFO(ahlog::HIGH) << "Closed bad pixel file or instrument map after EHK processing" << std::endl;
  ahfits::close(fpout);
  AH_INFO(ahlog::HIGH) << "Closed output FITS file" << std::endl;
  
}

// ****************************************************************************

void calcEfficiencyMap(ahexpmap::Params& par, ahexpmap::KeywordValues& keywd, 
  std::vector<std::vector<double> >& effimage) {

  std::stringstream ss;  // For formatting messages and numbers

  std::string inst;                   // Uppercase version of instrument
  std::vector<double> energy_vec;     // Energies specified in par
  std::vector<double> ene_cen;        // Energies used
  std::vector<double> ene_counts;     // Counts for each energy in FITS spectrum file
  std::vector<double> detxlo, detxhi, detylo, detyhi;  // DET pixel boundaries
  double ene_min=0.0, ene_max=0.0;    // Min and max energy specified for efficiencies
  double ene=0.0;                     // Energy used for efficiencies, MONO mode
  std::vector<std::vector<double> > effimage_ene;  // Efficiency image for one energy
  std::string specm="", specf="";     // Upper-case strings
  long num_ebin=0;                    // Number of energy bins
  long sum_counts=0;                  // Total counts in FITS spectrum file
  const double sxi_default_evperchan=6.0;    // Default eV per channel for SXI
  const double sxs_default_evperchan=0.5;    // Default eV per channel for SXS
  const double hxi_default_evperchan=100.0;  // Default eV per channel for HXI1 and HXI2
  double actual_evperchan=0.0;        // eV per channel after processing user parameter value
  double pi_to_kev=0.0;               // PI to keV conversion factor
  long pi_min=0.0, pi_max=0;          // Min and max channel for ene_min, ene_max
  long l_channel=0;                   // Local variable for reading CHANNEL column
  double l_counts=0.0;                // Local variable for reading COUNTS column
  double fpmm2arcmin = 0.0;           // Conversion facter, mm to arcmin in focal plane
  double detpixx2arcmin = 0.0;        // Arcmin per DETX pixel
  double detpixy2arcmin = 0.0;        // Arcmin per DETY pixel

  ahfits::FilePtr fpspc;              // FITS file pointer for spectrum file
  ahfits::Router* rspc;               // FITS data router for spectrum file

  std::string actual_obffile = "NONE";      // Actual filename of optical blocking filter file
  std::string actual_fwfile = "NONE";       // Actual filename of filter wheel file
  std::string actual_gvfile = "NONE";       // Actual filename of gatevalve file
  std::string actual_qefile = "NONE";       // Actual filename of quantum efficiency file
  std::string actual_contamifile = "NONE";  // Actual filename of contamination file
  std::string actual_vigfile = "NONE";      // Actual filename of vignetting file

  // Variables related to reading the efficiency files:

  // Number of unique QE vs. E functions for different spatial regions 
  // on the detector or detector array

  long numqefunc;

  // Index array that points to a unique QE*(optical blocking filter 
  // transmission) vs. energy function in qebcftrans(*,index), as a 
  // function of x and y position on the detector
  std::vector< std::vector<long> > qetransindex;  

  // For SXI:  Collection of unique, combined QE*(optical blocking filter transmission) 
  // vs. energy functions.
  // For SXS:  QE vs. energy (no optical blocking filter).
  std::vector< std::vector<double> > qecbftrans;  

  // Number of unique contamination transmission vs. E functions for 
  // different spatial regions on the detector or detector array
  long numcontamfunc;

  // Consolidated, net transmission (efficiency array)
  std::vector< std::vector<double> > qetrans;  

  // Index array that points to a unique contamination transmission vs. 
  // energy function in contamtrans(*,index), as a function of x and y 
  // position on the detector
  std::vector< std::vector<long> > contamtransindex;  

  // Collection of unique contamination transmission vs. energy functions
  std::vector< std::vector<double> > contamtrans;  

  // Height and radius of filterwheel (SXS only)
  double filterheight;
  double filterradius;

  // Fractional occupation of each pixel by the gatevalve and the
  // filter wheel, respectively (SXS only)
  std::vector< std::vector<double> > gvfracimage; 
  std::vector< std::vector<double> > fwfracimage; 

  // Flag to get gatevalve efficiency
  bool do_gatevalve = false;

  // Dimensions of images related to filter wheel (SXS only)
  long fwnx;
  long fwny;

  // Central coordinates of pixels in images related to filter wheel
  // (SXS only)
  std::vector<double> fwx;
  std::vector<double> fwy;

  // Edge coordinates of pixels in images related to filter wheel
  // (SXS only)
  std::vector<double> fwxbnds;
  std::vector<double> fwybnds; 

  // Geometric image of filter wheel and support structure
  std::vector< std::vector<double> > fwgeoimage; 

  // Type of filter transmission: 1=simple, 2=complex
  int trantype;

  // Vignetting coefficients
  int numvignetcoeff;
  double** vignetcoeff;

  // Filter wheel designations
  std::string fwcodename = "";
  std::string fwtype = "";

  // Intermediate variables for vignetting calculation
  double drx_arcmin = 0.0;     // Distance from optical center, X
  double dry_arcmin = 0.0;     // Distance from optical center, Y
  double pixeltheta = 0.0;     // Total distance from optical center
  double vignetfactor = 0.0;   // Computed vignetting factor

  std::vector<double> abund;  // Contamination abundances
  std::vector<double> cols;   // Contamination column densities
  std::vector<double> covfac; // Contamination covering factors

  // Parse contamination tweaking parameters.
  arfgenlib::listStringsToDoubles(par.m_abund, abund);
  arfgenlib::listStringsToDoubles(par.m_cols, cols);
  arfgenlib::listStringsToDoubles(par.m_covfac, covfac);

  // Parse energy vector.
  arfgenlib::listStringsToDoubles(par.m_energy, energy_vec);
  if (energy_vec.size() < 1 || energy_vec.size() > 2) {
    AH_ERR << "ENERGY parameter = " << par.m_energy << std::endl;
    ss.str("");
    ss << "Bad ENERGY parameter; has " << energy_vec.size() 
      << " values; must have 1 or 2 values; exiting";
    AH_THROW_RUNTIME(ss.str());
  }
  ene_min = energy_vec[0];
  ene_max = (energy_vec.size() > 1 ? energy_vec[1] : energy_vec[0]);

  // Configure spectral or monochrome mode.
  specm = ahgen::strtoupper(par.m_specmode);
  specf = ahgen::strtoupper(par.m_specform);
  if (specm == "MONO") {
    if (energy_vec.size() == 1) {
      ene = ene_min;
    } else {
      ene = (ene_min + ene_max)/2.0;
    }
    ene_cen.push_back(ene);
    ene_counts.push_back(1.0);
    num_ebin++;
    sum_counts = 1;
  } else if (specm == "SPEC") {
    if (ene_min > ene_max) {
      ss.str("");
      ss << std::setprecision(15) << "Energy min " << ene_min 
         << " is greater than energy max " << ene_max 
         << "; exiting";
      AH_THROW_RUNTIME(ss.str());
    }
    sum_counts = 0;
    if (specf == "FITS") {

      if (ahgen::strtoupper(par.m_evperchan) == "DEFAULT") {
        if (ahgen::strtoupper(ahfits::getKeyValStr(fpspc, "INSTRUME")) != "SXI") {
          actual_evperchan = sxi_default_evperchan;
        } else if (ahgen::strtoupper(ahfits::getKeyValStr(fpspc, "INSTRUME")) != "SXS") {
          actual_evperchan = sxs_default_evperchan;
        } else {
          actual_evperchan = hxi_default_evperchan;
        }
      } else {  // eV per channel is specified in parameter
        ss.str(par.m_evperchan);
        ss >> actual_evperchan;
      }

      pi_to_kev = actual_evperchan*0.001;
      pi_min = ene_min/pi_to_kev;
      pi_max = ene_max/pi_to_kev;
      num_ebin = pi_max - pi_min + 1;
      sum_counts = 0;

      ahfits::open(par.m_specfile, "", &fpspc);
      AH_INFO(ahlog::HIGH) << "Moving to SPECTRUM extension in specfile" << std::endl;
      ahfits::move(fpspc, "SPECTRUM");
      rspc = new ahfits::Router(fpspc);
      rspc->connectScalar(ahfits::e_READONLY, "CHANNEL", l_channel);
      rspc->connectScalar(ahfits::e_READONLY, "COUNTS", l_counts);
      ahfits::gotoRow(fpspc, pi_min); 
      for (long i=0; i<num_ebin; i++) {
        if (!ahfits::readOK(fpspc)) break;
        ahfits::readRow(fpspc);
        ene_cen.push_back(l_channel/pi_to_kev);
        ene_counts.push_back(l_counts);
        sum_counts += l_counts;
      }
      delete rspc;
      ahfits::close(fpspc);

    } else if (specf == "ASCII") {
      
      // Spectrum file is xspec or sherpa model output
      // Column 1:  energy
      // Column 2:  flux proportional to photons/cm2/s/keV

      std::ifstream sps(par.m_specfile.c_str(), std::ifstream::in);
      double t_energy=0.0, t_flux=0.0;  // Temporary variables
      if (sps.is_open()) {
        num_ebin = 0;
        while (sps >> t_energy >> t_flux) {
          if (t_energy >= ene_min && t_energy <= ene_max) {
            ene_cen.push_back(t_energy);
            ene_counts.push_back(t_flux);
            sum_counts += t_flux;
            num_ebin++;
          }
        }
        sps.close();
      } else {
        AH_THROW_RUNTIME("Could not open spectrum file "+par.m_specfile+"; exiting");
      }
    } else {
      AH_THROW_RUNTIME("SPECFORM="+specf+" is incorrect for SPECMODE="+specm
        +" (must be FITS or ASCII); exiting");
    }
  } else {
    AH_THROW_RUNTIME("SPECMODE="+specm+" is incorrect (must be MONO or SPEC); exiting");
  }

  // Retrieve efficiencies for time of observation
  AH_DEBUG << "About to read efficiencies " << std::endl;

  detxlo.resize(keywd.m_det_xsiz);
  detxhi.resize(keywd.m_det_xsiz);
  detylo.resize(keywd.m_det_ysiz);
  detyhi.resize(keywd.m_det_ysiz);
  for (long i=0; i<keywd.m_det_xsiz; i++) {
    detxlo[i] = i - 0.5;
    detxhi[i] = i + 0.5;
  }
  for (long i=0; i<keywd.m_det_ysiz; i++) {
    detylo[i] = i - 0.5;
    detyhi[i] = i + 0.5;
  }

  inst = ahgen::strtoupper(par.m_instrume);
  if (inst == "SXI") {

    // Resolve CALDB files (SXI)

    actual_qefile = ahmission::caldb::resolve(
      par.m_qefile, "QE", inst, "-", "QE_OBL", "-", 
      "-", keywd.m_telescop);

    actual_contamifile = ahmission::caldb::resolve(
      par.m_contamifile, "contamination", inst, "-", "COLUMN_TRANS", "-", 
      "-", keywd.m_telescop);

    actual_vigfile = ahmission::caldb::resolve(par.m_vigfile, 
      "vignetting coefficients", "GEN", "-", "VIGNCOEF", "-", "-", keywd.m_telescop);

    arfgenlib::sxiefficiencies(keywd.m_tstart, actual_qefile, 
      actual_contamifile, keywd.m_det_xsiz, keywd.m_det_ysiz, 
      detxlo, detxhi, detylo, detyhi,
      num_ebin, ene_cen,
      numqefunc, qetransindex, qecbftrans, 
      numcontamfunc, qetrans, 
      contamtransindex, contamtrans, trantype, 
      abund, cols, covfac);

  } else if (inst == "SXS") {

    // Resolve CALDB files (SXS)

    actual_qefile = ahmission::caldb::resolve(
      par.m_qefile, "QE", inst, "-", "QE", "-", 
      "-", keywd.m_telescop);

    actual_contamifile = ahmission::caldb::resolve(
      par.m_contamifile, "contamination", inst, "-", "COLUMN_DENSITY", "-", 
      "-", keywd.m_telescop);

    actual_vigfile = ahmission::caldb::resolve(par.m_vigfile, 
      "vignetting coefficients", "GEN", "-", "VIGNCOEF", "-", "-",keywd.m_telescop);

    actual_obffile = ahmission::caldb::resolve(
      par.m_obffile, "optical blocking filter", inst, "-", "TRANSBLOCKING", "-", 
      keywd.m_dateobs, keywd.m_telescop);

    // Form filter designations.
    fwtype = ahgen::strtoupper(par.m_actual_fwtype);
    if (fwtype.substr(0,4) == "OPEN") {
      fwtype = "OPEN";
    }
    if (fwtype == "OPEN") {
      fwcodename = "-";
    } else if (fwtype == "FE55") {
      fwcodename = "FE55TRANS";
    } else if (fwtype == "BE") {
      fwcodename = "BETRANS";
    } else if (fwtype == "ND") {
      fwcodename = "ND25TRANS";
    } else if (fwtype == "POLY") {
      fwcodename = "POLYTRANS";
    } else if (fwtype == "UNDEF") {
      fwcodename = "-";
    } else {
      AH_THROW_RUNTIME("Bad FWTYPE parameter = "+par.m_fwtype+"; exiting");
    }

    if (fwtype == "OPEN" || fwtype == "UNDEF" || ahgen::strtoupper(par.m_fwfile) == "NONE") {
      actual_fwfile = "NONE";
    } else {
      actual_fwfile = ahmission::caldb::resolve(
        par.m_fwfile, "filter wheel", inst, "-", fwcodename, "-", 
        keywd.m_dateobs, keywd.m_telescop);
    }

    if (ahgen::strtoupper(keywd.m_gatevalv.substr(0,5)) == "CLOSE") {
      actual_gvfile = ahmission::caldb::resolve(
        par.m_gvfile, "gatevalve", inst, "-", "GATEVALVTRANS", "-", 
        keywd.m_dateobs, keywd.m_telescop);
    } else {
      actual_gvfile = "NONE";
    }

    do_gatevalve = (actual_gvfile != "NONE");

    arfgenlib::sxsefficiencies(keywd.m_tstart, actual_qefile, actual_obffile,
      actual_fwfile, actual_contamifile, do_gatevalve, actual_gvfile,
      fwtype, keywd.m_det_xsiz, keywd.m_det_ysiz,  
      detxlo, detxhi, detylo, detyhi, 
      num_ebin, ene_cen,
      numqefunc, numcontamfunc, 
      qecbftrans, qetransindex, 
      contamtrans, contamtransindex,
      filterheight, filterradius, 
      gvfracimage, fwfracimage, 
      fwnx, fwny, fwx, fwy, fwxbnds, fwybnds,
      fwgeoimage, trantype, abund, cols, covfac);

  } else {

    AH_THROW_RUNTIME("Efficiency maps supported only for SXI or SXS; exiting");

  }

  // Call routine to read the vignetting file, returning the coefficient required
  // for using the analytic formulas.
  if (ahgen::strtoupper(actual_vigfile) != "NONE") {
    arfgenlib::readahvignetfile(actual_vigfile, numvignetcoeff, &vignetcoeff);
  }

  // Store resolved CALDB file names back into parameters

  ape_trad_set_string("qefile", actual_qefile.c_str());
  ape_trad_set_string("contamifile", actual_contamifile.c_str());
  ape_trad_set_string("vigfile", actual_vigfile.c_str());
  ape_trad_set_string("obffile", actual_obffile.c_str());
  ape_trad_set_string("fwfile", actual_fwfile.c_str());
  ape_trad_set_string("gvfile", actual_gvfile.c_str());

  if (ahgen::strtoupper(actual_vigfile) != "NONE") {  // Only needed for vignetting
    fpmm2arcmin = ahexpmap::RAD2ARCMIN*std::atan(1.0/keywd.m_focallen);
    detpixx2arcmin = keywd.m_det_xscl/fpmm2arcmin;  // DET_XSCL = [mm/pixel]
    detpixy2arcmin = keywd.m_det_yscl/fpmm2arcmin;  // DET_YSCL = [mm/pixel]
    AH_INFO(ahlog::LOW) << "Conversion factor, mm to arcmin: " << fpmm2arcmin << std::endl;
    AH_INFO(ahlog::LOW) << "Conversion factor, DETX pix to arcmin: " << detpixx2arcmin << std::endl;
    AH_INFO(ahlog::LOW) << "Conversion factor, DETY pix to arcmin: " << detpixy2arcmin << std::endl;
  }

  effimage.resize(keywd.m_det_xsiz);
  effimage_ene.resize(keywd.m_det_xsiz);
  for (long cx=0; cx<keywd.m_det_xsiz; cx++) {
    effimage[cx].resize(keywd.m_det_ysiz);
    effimage_ene[cx].resize(keywd.m_det_ysiz);
    for (long cy=0; cy<keywd.m_det_ysiz; cy++) {
      effimage[cx][cy] = 0.0;
      effimage_ene[cx][cy] = 1.0;
    }
  }

  for (long cx=0; cx<keywd.m_det_xsiz; cx++) {
    for (long cy=0; cy<keywd.m_det_ysiz; cy++) {
      for (long i=0; i<num_ebin; i++) {
        if (ahgen::strtoupper(par.m_qefile) != "NONE") {
          effimage_ene[cx][cy] *= qecbftrans[0][qetransindex[cx][cy]];
        }
        if (ahgen::strtoupper(par.m_qefile) != "NONE") {
          effimage_ene[cx][cy] *= contamtrans[0][contamtransindex[cx][cy]];
        }
        if (ahgen::strtoupper(par.m_vigfile) != "NONE") {
          // Temporary variables used only here
          drx_arcmin = (cx + 1 - keywd.m_optaxisx)*detpixx2arcmin;
          dry_arcmin = (cy + 1 - keywd.m_optaxisy)*detpixy2arcmin;
          pixeltheta = std::sqrt(drx_arcmin*drx_arcmin + dry_arcmin*dry_arcmin);
          arfgenlib::ahvignetfactor(const_cast<char*>(inst.c_str()), 
            ene_cen[i], pixeltheta, numvignetcoeff, vignetcoeff, vignetfactor);
          // std::cout << pixeltheta << " " << vignetfactor << std::endl;
          // AH_INFO(ahlog::LOW) << "evig=" << evig << " pixeltheta=" << pixeltheta 
          //   << " vignetfactor=" << vignetfactor << std::endl;
          effimage_ene[cx][cy] *= vignetfactor;
        }
        effimage[cx][cy] += effimage_ene[cx][cy]*ene_counts[i]/sum_counts;
      } // End energy loop
    } // End cy
  } // End cx
} // End function calcEfficiencyMap

// ****************************************************************************

void calcDeltaAttitude(double ra_src, double ra_pnt, double ra_nom, 
  double dec_src, double dec_pnt, double dec_nom, double pa_nom,
  double& theta, double& phi) {

  // Use haversine formula to compute offaxis angle
  // Taken from idlastro routine gcirc
  // http://idlastro.gsfc.nasa.gov/ftp/pro/gcirc.pro
  // see also http://en.wikipedia.org/Great-circle_distance

  double ra_rad_pnt = ra_pnt*M_PI/180.0;
  double ra_rad_src = ra_src*M_PI/180.0;
  double dec_rad_pnt = dec_pnt*M_PI/180.0;
  double dec_rad_src = dec_src*M_PI/180.0;
  double d_ra_rad_2 = (ra_rad_src - ra_rad_pnt)/2.0;
  double d_dec_rad_2 = (dec_rad_src - dec_rad_pnt)/2.0;
  double sin_theta = 
    std::sqrt( std::pow(std::sin(d_dec_rad_2),2) 
      + std::cos(dec_rad_src)*std::cos(dec_rad_pnt)*std::pow(std::sin(d_ra_rad_2),2) );

  theta = 2*std::asin(sin_theta)*(180.0*60.0/M_PI);   // Convert to arcmin

  // Simplified planar formula with cosine(declination) factor

  double pa_rad_nom = pa_nom*M_PI/180.0;
  double sinroll = std::sin(pa_rad_nom);
  double cosroll = std::cos(pa_rad_nom);
  double cosdec = std::cos(dec_rad_src);
  double num = (ra_rad_src - ra_rad_pnt)*sinroll*cosdec + (dec_rad_src - dec_rad_pnt)*cosroll;
  double den = (ra_rad_src - ra_rad_pnt)*cosroll*cosdec - (dec_rad_src - dec_rad_pnt)*sinroll;
  if (num != 0.0 && den != 0.0) {
    // This choice of sign in num and den conforms to xrtraytrace.
    phi = std::atan2(num, den)*180.0/M_PI;
    if (phi < 0.0) {
      phi += 360.0;
    }
  } else { // phi undefined if num=den=0, so may as well make it zero
    phi = 0.0;
  }
  AH_DEBUG << "From calcDeltaAttitude: theta=" << theta <<", phi numerator=" << num << ", denom=" << den << ", phi=" << phi << std::endl;

  //double num = std::sin(pa_rad_nom + ra_rad_src - ra_rad_nom);
  //double den_term1 = std::sin(dec_rad_pnt - dec_rad_nom)*std::cos(pa_rad_nom + ra_rad_src - ra_rad_nom);
  //double den_term2_num = std::sin(dec_rad_src - dec_rad_nom)*std::cos(dec_rad_pnt - dec_rad_nom);
  //double den_term2_den = std::cos(dec_rad_src - dec_rad_nom);
  ////double den = den_term1 + den_term2_num/den_term2_den;
  //double den = den_term1 - den_term2_num/den_term2_den;
  //phi = std::atan2(num, den)*180/M_PI;   // Convert to deg
  //AH_DEBUG << "From calcDeltaAttitude: theta=" << theta <<", phi numerator=" << num << ", denom=" << den << ", phi=" << phi << std::endl;
  //double num_term1 = std::sin(pa_rad_nom + ra_rad_src - ra_rad_nom);
  //double den_term1 = std::sin(dec_rad_pnt - dec_rad_nom)*std::cos(pa_rad_nom + ra_rad_src - ra_rad_nom);
  //double num_term2 = std::sin(dec_rad_src - dec_rad_nom)*std::cos(dec_rad_pnt - dec_rad_nom);
  //double den_term2 = std::cos(dec_rad_src - dec_rad_nom);
  //double atan_arg = (num_term1 / den_term1) + (num_term2 / den_term2);
  //phi = std::atan(atan_arg)*180/M_PI;   // Convert to deg
  //AH_DEBUG << "From calcDeltaAttitude: theta=" << theta <<", phi atan_arg=" << atan_arg << ", phi=" << phi << std::endl;

  if (phi < 0) {
    phi += 360.0;
  }
}

// ****************************************************************************

void getIndexFromWedge(double theta, double phi, long min_theta_bin, 
  double delta, long numphi, long& index) {

  // Returns index into the wedge array given theta and phi.

  long theta_bin=0;
  long num_through_prev_annulus=0;
  long num_this_annulus=0;
  double phi_bin_width=0.0;
  long interior_wedge_count=0;
  long wedge_count=0;

  theta_bin = theta/delta;

  // Handle the trivial case first, then the general case.
  // Theta bins are numbered 0 ... (number of theta bins - 1).
  // "Wedges" are first counted (one-based) then the
  // count is converted to a zero-based subscript at the end.

  if (theta_bin == 0) {
    // Answer is trivial for central bin.
    index = 0;
  } else {
    // For other bins, figure out the index.
    // First count all the wedges up to and including the one that
    // includes (theta, phi).
    if (theta_bin == 1) {
      num_through_prev_annulus = 1;
    } else {
      // Add up arithmetic series for bins 1 ... theta_bin-1, then
      // add one for the central bin.
      num_through_prev_annulus = ((theta_bin - 1)*theta_bin*numphi)/2 + 1;
    }
    phi_bin_width = 360.0/(numphi*theta_bin);
    num_this_annulus = (phi + phi_bin_width)/phi_bin_width;  // Count, starting from 1
    wedge_count = num_through_prev_annulus + num_this_annulus;

    // Second, count the wedges from annulus 0 (=central disc) through annulus
    // min_theta_bin - 1.
    if (min_theta_bin == 1) {
      // Central bin only
      interior_wedge_count = 1;
    } else if (min_theta_bin > 1) {
      // Add up arithmetic series for bins 1 ... min_theta_bin-1, then
      // add one for the central bin.
      interior_wedge_count = ((min_theta_bin - 1)*min_theta_bin*numphi)/2 + 1;
    } else { // min_theta_bin == 0
      interior_wedge_count = 0;
    }

    // Correct the wedge count for missing interior bins, then
    // convert to a subscript.
    index = wedge_count - interior_wedge_count - 1;
  }

  AH_DEBUG << "From getIndexFromWedge: delta=" << delta << ", numphi=" << numphi << std::endl;
  AH_DEBUG << "From getIndexFromWedge: theta=" << theta << ", phi=" << phi << ", theta_bin=" 
    << theta_bin << ", min_theta_bin=" << min_theta_bin 
    << ", num_through_prev_annulus=" << num_through_prev_annulus 
    << ", num_this_annulus=" << num_this_annulus << ", index=" << index << std::endl;

}

// ****************************************************************************

void getWedgeFromIndex(long index, long min_theta_bin, double delta, 
  long numphi, double& theta_min, double& theta_max, 
  double& phi_min, double& phi_max) {

  // Returns the min and max theta and phi of a wedge given the array index.
  // Theta values are in arcmin, phi values are in degrees.

  long theta_bin = 0;
  long count = 0;
  long previous_count = 0;
  double phi_bin_width = 0.0;
  long phi_bin_number = 0;

  // Theta bins are numbered 0 ... (number of theta bins - 1).
  // By contrast, "wedges" are counted (one-based).

  if (index == 0 && min_theta_bin == 0) {
    // Answer is trivial for central disc.
    theta_min = 0.;
    theta_max = delta;
    phi_min = 0.;
    phi_max = 360.;
  } else if (numphi == 0) {
    // If numphi is 0, no azimuthal splitting, answer is also trivial.
    theta_min = (index + min_theta_bin) * delta;
    theta_max = theta_min + delta;
    phi_min = 0.;
    phi_max = 360.;
  } else {

    // Accumulate the number of wedges in successive annuli
    // until we have passed index + 1; we add one to index because
    // it is a zero-based subscript and we need a count.

    // Count min_theta_bin outside the loop, since it might be
    // special.
    if (min_theta_bin == 0) {
      count = 1;
    } else {
      count = min_theta_bin*numphi;
    }
    previous_count = 0;
    theta_bin = min_theta_bin;

    // Loop throuth the rest of the theta bins.
    while (count < index + 1) {
      previous_count = count;
      theta_bin++;
      count += theta_bin*numphi;
    }
      
    theta_min = theta_bin*delta;
    theta_max = theta_min + delta;

    phi_bin_width = 360.0/(numphi*theta_bin);
    phi_bin_number = index + 1 - previous_count;  // Count, starting from 1
    phi_min = (phi_bin_number - 1)*phi_bin_width;
    phi_max = phi_min + phi_bin_width;
  }
}

// ****************************************************************************

void computeXform2dDetToFocOrSky (std::string& stopsys, XFORM2D* xform, 
  const double cendet, const double foc_xoff, const double foc_yoff,
  const double focscaleratio, const double foc_rotd, const double cenfoc,
  const double roll_sign, const double ra_nom, const double dec_nom,
  const double ra_exp, const double dec_exp, const double pa_exp,
  const double censky, const double pixels_per_radian) {

  std::stringstream ss;  // For formatting strings

  double sinang=0.0, cosang=0.0;  // Intermediate variables

  // Allocate 2-D transformations (rot, shift)
  XFORM2D* xform_det_to_foc = allocateXform2d();
  XFORM2D* xform_exp_to_nom = allocateXform2d();

  XFORM2D* xf1 = allocateXform2d();
  XFORM2D* xf2 = allocateXform2d();
  XFORM2D* xf3 = allocateXform2d();
  XFORM2D* xf4 = allocateXform2d();
  XFORM2D* xf5 = allocateXform2d();
  XFORM2D* xf6 = allocateXform2d();

  // Allocate quaternions
  QUAT* quat_nom = allocateQuat();
  QUAT* quat_exp = allocateQuat();
  QUAT* deltaq = allocateQuat();

  // Allocate alignment descriptor
  ALIGN* align_identity = allocateDefaultAlign();

  // Compute xform2d to convert DET to FOC

  // Make center of FOC the origin
  setXform2dToTranslation(xf1, -cendet - foc_xoff + 1, -cendet - foc_yoff + 1);

  AH_INFO(ahlog::LOW) << "Xform2d to move FOC center in DET to origin:" << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][0] = " << xf1->rot[0][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][0] = " << xf1->rot[1][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][1] = " << xf1->rot[0][1] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][1] = " << xf1->rot[1][1] << std::endl;
  AH_INFO(ahlog::LOW) << "xshift    = " << xf1->xshift << std::endl;
  AH_INFO(ahlog::LOW) << "yshift    = " << xf1->yshift << std::endl;

  // Scale DET to FOC
  setXform2dToScaling(xf2, focscaleratio, focscaleratio, 0.0, 0.0);

  // Combine the operations
  combineXform2ds(xf3, xf1, xf2);

  // Rotate
  sinang = std::sin(foc_rotd*M_PI/180.0);
  cosang = std::cos(foc_rotd*M_PI/180.0);
  setXform2dToRotation(xf4, sinang, cosang, 0.0, 0.0);

  // Combine the operations.
  combineXform2ds(xf5, xf3, xf4);

  // Make center of FOC the origin.
  setXform2dToTranslation(xf6, cenfoc - 1, cenfoc - 1);

  // Combine the operations.
  combineXform2ds(xform_det_to_foc, xf5, xf6);

  // Deallocate scratch space.
  destroyXform2d(xf1);
  destroyXform2d(xf2);
  destroyXform2d(xf3);
  destroyXform2d(xf4);
  destroyXform2d(xf5);
  destroyXform2d(xf6);

  AH_INFO(ahlog::LOW) << "Xform2d for DET to FOC:" << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][0] = " << xform_det_to_foc->rot[0][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][0] = " << xform_det_to_foc->rot[1][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][1] = " << xform_det_to_foc->rot[0][1] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][1] = " << xform_det_to_foc->rot[1][1] << std::endl;
  AH_INFO(ahlog::LOW) << "xshift    = " << xform_det_to_foc->xshift << std::endl;
  AH_INFO(ahlog::LOW) << "yshift    = " << xform_det_to_foc->yshift << std::endl;

  // Decide if the computation should go further.
  if (ahgen::strtoupper(stopsys) == "FOC") {

    copyXform2d(xform, xform_det_to_foc);

  } else if (ahgen::strtoupper(stopsys) == "SKY") {

    // Compute xform2d to convert FOC to SKY:
    //  (1) FOC coordinates are for a particular image (EXP suffix)
    //  (2) Quaternions are used to compute the EXP-to-SKY transformation
    //  (3) EXP-to-SKY is combined with DET-to-FOC

    // Convert NOM pointing to quaternion

    align_identity->roll_sign = roll_sign;
    invertQuat(align_identity->q_inverse, align_identity->q);

    ss.str("");
    ss << std::setprecision(15) << "NOM pointing RA=" << ra_nom
      << " Dec=" << dec_nom << " Roll=" <<  0.0;
    AH_INFO(ahlog::LOW) << ss.str() << std::endl;
    ss.str("");
    ss << std::setprecision(15) << "EXP pointing RA=" << ra_exp
      << " Dec=" << dec_exp << " Roll=" <<  pa_exp;
    AH_INFO(ahlog::LOW) << ss.str() << std::endl;

    // Roll set to 0 for sky
    convertRADecRollToQuat(align_identity, quat_nom, 
      ra_nom, dec_nom, 0.0);

    // Convert EXP pointing to quaternion
    convertRADecRollToQuat(align_identity, quat_exp, 
      ra_exp, dec_exp, pa_exp);

    AH_INFO(ahlog::LOW) << "nom. pointing q0    = [" 
      << quat_nom->p[0] << " "
      << quat_nom->p[1] << " "
      << quat_nom->p[2] << " "
      << quat_nom->p[3] << "]" << std::endl;

    AH_INFO(ahlog::LOW) << "att q               = [" 
      << quat_exp->p[0] << " "
      << quat_exp->p[1] << " "
      << quat_exp->p[2] << " "
      << quat_exp->p[3] << "]" << std::endl;

    // Find quat of change
    getQuatOfChange(deltaq, quat_nom, quat_exp);
    AH_INFO(ahlog::LOW) << "delta q             = [" 
      << deltaq->p[0] << " "
      << deltaq->p[1] << " "
      << deltaq->p[2] << " "
      << deltaq->p[3] << "]" << std::endl;

    // Convert quat of change to xform2d
    convertQuatToXform2d(xform_exp_to_nom, deltaq,
      cenfoc - 1, cenfoc - 1, censky - 1, censky - 1,
      pixels_per_radian);

    AH_INFO(ahlog::LOW) << "Xform2d for EXP to NOM:" << std::endl;
    AH_INFO(ahlog::LOW) << "rot[0][0] = " << xform_exp_to_nom->rot[0][0] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[1][0] = " << xform_exp_to_nom->rot[1][0] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[0][1] = " << xform_exp_to_nom->rot[0][1] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[1][1] = " << xform_exp_to_nom->rot[1][1] << std::endl;
    AH_INFO(ahlog::LOW) << "xshift    = " << xform_exp_to_nom->xshift << std::endl;
    AH_INFO(ahlog::LOW) << "yshift    = " << xform_exp_to_nom->yshift << std::endl;

    // Combine with DET-to-FOC transformation
    combineXform2ds(xform, xform_det_to_foc, xform_exp_to_nom);

    AH_INFO(ahlog::LOW) << "Final Xform2d:" << std::endl;
    AH_INFO(ahlog::LOW) << "rot[0][0] = " << xform->rot[0][0] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[1][0] = " << xform->rot[1][0] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[0][1] = " << xform->rot[0][1] << std::endl;
    AH_INFO(ahlog::LOW) << "rot[1][1] = " << xform->rot[1][1] << std::endl;
    AH_INFO(ahlog::LOW) << "xshift    = " << xform->xshift << std::endl;
    AH_INFO(ahlog::LOW) << "yshift    = " << xform->yshift << std::endl;

    destroyQuat(deltaq);
    destroyQuat(quat_exp);
    destroyQuat(quat_nom);

  } else {

    AH_ERR << "stopsys=" << stopsys << std::endl;
    AH_THROW_RUNTIME("stopsys must be FOC or SKY; exiting");

  }

}


/** @} */

/* Revision Log

 $Log: ahexpmap.cxx,v $
 Revision 1.62  2016/08/30 19:03:50  rshill
 Corrected the WCS keywords in the primary HDU so that they match
 the map type, DET, FOC, or SKY.

 Revision 1.61  2016/08/29 20:49:46  rshill
 Correct off-by-one error in computeXform2dDetToFocOrSky(),
 which was caused by the fact the 0-based coordinates are necessary for the image
 transformation routines.

 Revision 1.60  2016/08/10 17:40:07  rshill
 Comment out std::cout message.

 Revision 1.59  2016/08/02 19:19:37  rshill
 (1) Changed task so that all event file GTI are counted even
 if GTI are skipped over by EHK time grid; (2) changed handling of last EHK time so that if it is
 inside an event GTI, the whole GTI is counted; (3) detects zero-length time interval resulting
 from coincidence of EHK time with end of a GTI and transition to a new theta,phi bin and skips
 accumulation step that resulted previously in a NaN.

 Revision 1.58  2016/06/22 01:03:45  rshill
 Fixed bug that was introduced by needlessly closing and reopening the pixel GTI file
 for every transition to a new offaxis bin or close of a normal GTI during EHK processing.
 The pixel GTI file can be repeatedly read in a loop without reopening.

 Revision 1.57  2016/06/17 21:06:21  rshill
 Close pixel GTI file before reopening.

 Revision 1.56  2016/06/01 15:37:00  rshill
 Corrected bad CALDB resolution.  In course of previously correcting
 a non-standard calling sequence, had forgotten to initialize TELESCOP value.

 Revision 1.55  2016/06/01 15:03:52  rshill
 Corrected bug whereby pixel GTI extension name was always
 PIXELS in computing exposure time of offaxis bin, per pixel (step 7d).  Now the

 Revision 1.54  2016/05/25 21:44:55  rshill
 Corrected CALDB resolution call for INSTMAP.

 Revision 1.53  2016/05/06 22:01:06  rshill
 Corrected accumulation of multiple intervals for partial pixels;
 bad columns and charge injection rows now shown on exposure map image.

 Revision 1.52  2016/04/29 19:54:23  rshill
 Zero bad pixels in expo map image.  Include inactive pixels in
 partial pixel lists.  Resolve +++ comments.

 Revision 1.51  2016/04/08 20:37:21  rshill
 Changed AH_INFO to AH_DEBUG; improved execution path debug messages; added count printout.

 Revision 1.50  2016/03/30 20:35:34  rshill
 Process gatevalve CALDB file only if GATEVALV=CLOSE.

 Revision 1.49  2016/03/30 18:53:06  rshill
 Corrected implementation mistakes:  ene_min
 and ene_max in MONO mode; HXI default eV per channel.

 Revision 1.48  2016/03/22 15:17:29  rshill
 Added writeParametersToLog calls; deleted a redundant CALDB query (issue #610).

 Revision 1.47  2016/03/18 21:25:01  rshill
 If two energies specified for MONO mode, average is used.

 Revision 1.46  2016/03/12 02:03:41  rshill
 Corrected pixel area normalization to exposure map units of seconds.

 Revision 1.45  2016/03/11 20:46:01  rshill
 Changed test for OPEN filter to first 4 char;
 added UNDEF to possibilities allowed in event (GTI) file; changed default of FWTYPE
 parameter to literal DEFAULT in accord with TRF.

 Revision 1.44  2016/03/11 05:27:58  rshill
 Accepts extended syntax for overall GTI extension.

 Revision 1.43  2016/03/04 21:18:46  rshill
 Restore RAW_XSIZ keyword to output, which had been dropped.

 Revision 1.42  2016/03/03 20:36:24  rshill
 Deleted miscoded existence checks for input files,
 because this check is included in ahfits::open.

 Revision 1.41  2016/03/02 16:40:26  rshill
 Fixed lack of pointer init for structures used by imagetrans.

 Revision 1.40  2016/03/01 00:18:59  rshill
 Corrected CALDB queries for HXI inst map, SXS vignetting.
 Corrected raytrace coord conversion.  Correct SXS FILTER keyword/parameter processing.

 Revision 1.39  2016/02/29 20:10:26  rshill
 Enable extended syntax for pixel GTI extension name.

 Revision 1.38  2016/02/25 02:52:55  rshill
 Corrected partial pixel exposure time computation by
 accumulating sub-GTI correctly.

 Revision 1.37  2016/02/24 19:20:49  rshill
 Change method of computing avg theta, phi per bin to
 computing from average positions.

 Revision 1.36  2016/02/23 20:36:23  rshill
 Removed some +++ comments.  Corrected output DETX, DETY for off-by-one error.

 Revision 1.35  2016/02/23 15:24:43  rshill
 Completed the keyword copying to primary output header.

 Revision 1.34  2016/02/22 16:25:48  rshill
 Added CUNIT keywords to output WCS.  Gave BAD flagging
 precedence over PARTIAL EXP flagging.  Explicity copying RA_OBJ, DEC_OBJ to output.
 Changed bin number message to print if more than 20 total bins, not 20 annuli.

 Revision 1.33  2016/02/20 02:55:13  rshill
 More compact format for PARTIALEXPnnn table.

 Revision 1.32  2016/02/20 02:18:55  rshill
 Functionality now agrees with trf_ahexpmap_16-02-11.docx.
 Several updates from TRF in actual implementation.
 Main change is PARTIALEXPnnn table written instead of image
 corresponding to histogram bin.

 Revision 1.31  2016/01/28 23:37:02  rshill
 Added writing of parameters to log file.

 Revision 1.30  2016/01/27 18:27:32  rshill
 Made sure that TELDEF keywords from instrument map are written
 to OFFAXISHIST header; added DEFAULT processing for EVPERCHAN parameter.

 Revision 1.29  2016/01/25 05:48:03  rshill
 Corrected phi calculation in calcDeltaAttitude().

 Revision 1.28  2016/01/21 06:19:39  rshill
 Writes NUMTHETA keyword; puts image in primary HDU.
 Corrected RA* and DEC* keyword names to include underscore.

 Revision 1.27  2016/01/20 20:53:17  rshill
 Both EXPOSURE and EFFICIENCY modes.  Fixed bug of
 failing to account for the fact that imagetranslib accumulates values in the output map.
 Implemented output in DET/FOC/SKY.

 Revision 1.26  2016/01/19 22:00:43  rshill
 1) Phi binning linear with annulus number; 2) planar approximation for phi
 calculatation; 3) TSTART, TSTOP in GTI extensions.

 Revision 1.25  2016/01/16 01:06:39  rshill
 Fixed another bug in bin index computation.

 Revision 1.24  2016/01/16 00:04:07  rshill
 Added image transformation to sky using imagetranslib.
 Fixed some bugs in bin number computation.

 Revision 1.23  2016/01/14 19:07:36  rshill
 Eric Miller's version with both theta and phi binning,
 per trf_ahexpmap_16_01_12.docx.

 Revision 1.21  2015/12/29 19:41:43  rshill
 Added checkEmptyTable().

 Revision 1.20  2015/12/22 19:43:51  rshill
 Shifted range of phi to 0-360 deg.

 Revision 1.19  2015/12/22 19:16:55  rshill
 Corrected the computation of ngrid_theta, which didn't work if
 max_theta was exactly on a bin boundary.

 Revision 1.18  2015/12/11 21:28:38  rshill
 Corrected typo in converting phi to deg.

 Revision 1.17  2015/12/11 21:03:46  rshill
 Units of theta changed to arcmin; units of phi changed to deg.

 Revision 1.16  2015/12/02 16:52:19  rshill
 Added writing of GATEVALV, FILTER, and WCS keywords to output file.

 Revision 1.15  2015/11/25 16:20:14  rshill
 Completed the set of keywords copied from instrument map.

 Revision 1.14  2015/11/12 23:06:19  rshill
 Deleted +++ and TRF-related comments that are obsolete.

 Revision 1.13  2015/11/12 19:13:41  rshill
 Start histogram at min(theta), not theta=0; open INSTMAP correctly
 if opened a second time.

 Revision 1.12  2015/11/09 20:21:40  rshill
 Corrected codename for instmap CALDB file.

 Revision 1.11  2015/11/02 16:58:58  rshill
 Corrected comment header.

 Revision 1.10  2015/10/30 22:52:26  rshill
 Cut down voluminous output somewhat; still very complete.

 Revision 1.9  2015/10/27 23:38:17  rshill
 Fixed a bug in terminating the last GTI when it
 overlaps the end of the EHK file.  Added some code path counts.

 Revision 1.8  2015/10/27 18:39:51  rshill
 Fixed bug that caused a GTI that includes the last EHK
 row to be skipped.


*/


