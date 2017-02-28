///// \file hxirspeffimg.cxx
/// \brief Generate HXI response matrix
/// \author Robert S. Hill
/// \date $Date: 2017/01/13 23:41:38 $
/// \version 1.0

/** 

\defgroup tool_hxirspeffimg HXI response matrix generation (hxirspeffimg)
@ingroup mod_sxs_tasks

This tool uses raytracing event files to generate a response matrix
for HXI.  This is the matrix product of RMF times ARF.

Source files:

  hxirspeffimg.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath


Modification history:

  Ver   Date        Author  Description
  1.0   2015-12-15   RSH    Initial implementation

*/
 
#define AHLABEL tool_hxirspeffimg
#define AHCVSID "$Id: hxirspeffimg.cxx,v 1.52 2017/01/13 23:41:38 rshill Exp $"

#define TOOLTAG "$Name: Hitomi_dev $"

#include "arfgenlib/arfgenlib.h"
#include "rmflib/rmflib.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmath/ahmath.h"
#include "headas.h"              // expand_item_list  
#include "headas_utils.h"
#include "ahmission/keyword.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
extern "C" {
#include "imagetranslib/image.h"
#include "imagetranslib/methods.h"
#include "imagetranslib/param.h"
#include "comboxform.h"
#include "coordfits.h"
#include "headas_polyfit.h" 
}
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <math.h>
#include <algorithm> 
#include <iostream>
#include <iomanip>  //std::setfill
#include <fstream>
#include <sstream>

/** \addtogroup tool_hxirspeffimg
 *  @{
 */

const double DEG2RAD = M_PI/180.0; 
const double ARCMIN2RAD = M_PI/(180.0*60.0); 
const double RAD2ARCMIN = (180.0*60.0)/M_PI; 

// Initialization routines provided below.
typedef std::vector<double> Double1D;
typedef std::vector<Double1D> Double2D;
typedef std::vector<Double2D> Double3D;
typedef std::vector<Double3D> Double4D;
typedef std::vector<long> Long1D;

typedef std::vector<SAORegion*> RegionArray;

struct CRKeywords {    // Coordinate-related keywords
  double m_ra_src;           // RA of source [deg]
  double m_dec_src;          // Dec of source [deg]
  double m_ra_pnt;           // RA of optical axis [deg]
  double m_dec_pnt;          // Dec of optical axis [deg]
  double m_ra_nom;           // RA of pointing [deg]
  double m_dec_nom;          // Dec of pointing [deg]
  double m_pa_nom;           // Roll angle of pointing [deg]
  double m_optaxisx;         // DETX of optical axis
  double m_optaxisy;         // DETY of optical axis
  double m_opt_rotd;         // Rotation angle of optical axis [deg]
  double m_raw_xscl;         // RAW X scale [mm/pixel]
  double m_raw_yscl;         // RAW Y scale [mm/pixel]
  double m_act_xscl;         // ACT X scale [mm/pixel]
  double m_act_yscl;         // ACT Y scale [mm/pixel]
  double m_det_xscl;         // DET X scale [mm/pixel]
  double m_det_yscl;         // DET Y scale [mm/pixel]
  double m_focallen;         // HXT focal length [mm]
  double m_foc_xscl;         // FOC X scale [mm/pixel]
  double m_foc_yscl;         // FOC Y scale [mm/pixel]
  long m_act_xsiz;           // ACT X dimension [pixels]
  long m_act_ysiz;           // ACT Y dimension [pixels]
  double m_act_xcen;         // ACT X center [pixels]
  double m_act_ycen;         // ACT Y center [pixels]
  long m_det_xsiz;           // DET X dimension [pixels]          
  long m_det_ysiz;           // DET Y dimension [pixels]
  long m_detxflip;           // Sign of DET X increase wrt ACT
  long m_detyflip;           // Sign of DET Y increase wrt ACT
  long m_foc_xsiz;           // FOC X dimension [pixels]
  long m_foc_ysiz;           // FOC Y dimension [pixels]
  double m_foc_xoff;         // Offset between FOC X and DET X
  double m_foc_yoff;         // Offset between FOC Y and DET Y
  double m_foc_rotd;         // Rotation angle between FOC and DET
  long m_optxflip;           // Sign of telescope X increase wrt DET X
  long m_optyflip;           // Sign of telescope Y increase wrt DET X
  long m_roll_sign;          // Roll convention
  double m_cendet;           // DET coordsys center (assumed square)
  double m_cenfoc;           // FOC coordsys center (assumed square)
  double m_focscaleratio;    // Ratio of DET to FOC pixel sizes
  double m_censky;           // SKY coordsys center (assumed square)
  double m_pixels_per_radian; // SKY inverse plate scale
  double m_xinc_sky;         // SKY plate scale, X [deg/pixel], for WCS
  double m_yinc_sky;         // SKY plate scale, Y [deg/pixel], for WCS
  double m_crval1;           // WCS X center in world coordinates
  double m_crpix1;           // WCS X center in pixels
  double m_cdelt1;           // WCS X increment
  std::string m_cunit1;      // WCS X units
  double m_crval2;           // WCS Y center in world coordinates
  double m_crpix2;           // WCS Y center in pixels
  double m_cdelt2;           // WCS Y increment
  std::string m_cunit2;      // WCS Y units
};

struct Baffle {
  double m_centerx;    // Center of baffle aperture (X)
  double m_centery;    // Center of baffle aperture (Y)
  double m_toprad;     // Radius of top baffle aperture
  double m_topheight;  // Height of top baffle aperture
  double m_botrad;     // Radius of bottom baffle aperture
  double m_botheight;  // Height of bottom baffle aperture
  double m_centerxmm;  // Center of baffle aperture (X) [mm] - computed
  double m_centerymm;  // Center of baffle aperture (Y) [mm] - computed
};

struct Erange {
  double m_eminimg;    // Minimum energy for output flat
  double m_emaximg;    // Maximum energy for output flat
};

struct Par {
  // Input parameters
  std::string m_telescop;   // TELESCOP keyword
  std::string m_instrume;   // INSTRUME keyword
  std::string m_erange_str;  // Energy min, max for flat field image
  std::string m_dattfile;   // Delta-attitude file
  std::string m_filtoffsetfile;  // CAMS offset correction file from CAMS2ATT
  std::string m_emapfile;   // Exposure map file
  std::string m_qefile;     // QE file
  std::string m_rmffile;    // RMF file or @list
  std::string m_onaxisffile;  // On-axis fine-grid ARF file
  std::string m_onaxiscfile;  // On-axis coarse-grid ARF file
  std::string m_polydeg;      // Polynomial degree
  std::string m_vigfile;      // Vignetting coefficients file
  std::string m_auxtransfile;   // Auxiliary transmission file
  std::string m_outflatfile;    // Output flatfield file
  std::string m_outmaptype;     // Exposure map or efficiency map
  std::string m_stopsys;        // Coordsys of flatfield
  std::string m_outfile;        // Root of output RSP and ARF file names
  std::string m_regionfile; // Region on sky for which RSP applicable
  std::string m_xrtevtfile; // Raytracing event file
  long m_minphoton;  // Number of photons needed for good result
  long m_sampling;   // Sampling interval for photons
  std::string m_baffle_str;  // Baffle X, Y, Rtop, Ztop, Rbot, Zbot
  double m_rmfthresh;        // LO_THRESH for writing RSP
  
  // Parsed or derived parameter values
  Baffle m_baffle;                   // Parsed baffle attributes
  Erange m_erange;                   // Parsed energy range
  std::string m_outrspfile;          // Output RSP file name
  std::string m_outarffile;          // Output ARF file name
  std::string m_actual_qefile;       // QE file
  std::string m_actual_vigfile;      // Vignetting file
  std::string m_actual_auxtransfile; // Auxiliary transmission file
  std::string m_actual_rmffile;      // RMF (LSF) file
  std::string m_actual_onaxisffile;  // Onaxis ARF, fine energy grid
  std::string m_actual_onaxiscfile;  // Onaxis ARF, coarse energy grid
  int  m_numeric_polydeg;            // Integer value of polynomial degree
  long m_numlayers;                  // Number of HXI layers treated
  long m_numrmffiles;                // RMF file
  bool m_doflatfield;                // Flag to compute flat field
  bool m_dorsp;                      // Flag to compute RSP
  bool m_doregion;                   // Flag to select photons by region
};

/// \brief Read user parameters
/// \param[in] par                   Structure holding parameters
void getPar(Par & par);

/// \brief Fill energy grids, optical axis, QE; open raytracing file
/// \param[in]   par                Structure holding parameters
/// \param[out]  rmfdat             RMF data structure used to build output RSP
/// \param[out]  numeoutbins        Number of energy output bins
/// \param[out]  numrmfinputbins    Number of input energy bins in RMF
/// \param[out]  numrmfchan         Number of output energy channels in RMF
/// \param[out]  inputrmfs          RMFs read from file or files
/// \param[out]  rspmatrix          Output RSP matrix, allocated at correct size
/// \param[out]  outauxtrans        Auxiliary transmission
/// \param[out]  geomarea           Geometric area of telescope
/// \param[out]  numebinscoarse     Number of energy bins in coarse ARF
/// \param[out]  ecoarsecen         Energy bin centers for coarse ARF
/// \param[out]  coarsearfeffarea   EA from coarse ARF
/// \param[out]  numebinsfine       Number of energy bins in fine ARF
/// \param[out]  efinecen           Energy bin centers for fine ARF
/// \param[out]  finearfeffarea     EA from fine ARF
/// \param[out]  xmmperpixel        Physical pixel size in X [mm]
/// \param[out]  ymmperpixel        Physical pixel size in Y [mm]
/// \param[out]  fpmm2arcmin        Conversion factor [arcmin/mm] in focal plane
/// \param[out]  optaxisXstdmm      Optical axis X position [mm]
/// \param[out]  optaxisYstdmm      Optical axis Y position [mm]
/// \param[out]  rawxminmm          Lower bound of RAW X [mm]
/// \param[out]  rawxmaxmm          Upper bound of RAW X [mm]
/// \param[out]  rawyminmm          Lower bound of RAW Y [mm]
/// \param[out]  rawymaxmm          Upper bound of RAW Y [mm]
/// \param[out]  numrawx            RAW X dimension
/// \param[out]  numrawy            RAW Y dimension
/// \param[out]  numactx            ACT X dimension
/// \param[out]  numacty            ACT Y dimension
/// \param[out]  rawx               RAW X pixel centers [pixels]
/// \param[out]  rawy               RAW Y pixel centers [pixels]
/// \param[out]  actxlo             ACT X pixel lower bounds [pixels]
/// \param[out]  actylo             ACT Y pixel lower bounds [pixels]
/// \param[out]  qemap              HXI QE map
/// \param[out]  avgqemap           HXI QE map averaged over energies and summed over layers
/// \param[out]  numcoeff           Number of vignetting coefficients
/// \param[out]  vignetcoeff        Vignetting coefficients
/// \param[out]  hxibaffle          HXI baffle mask
/// \param[out]  opticrawx          RAW X coord of optical axis [pixels]
/// \param[out]  opticrawy          RAW Y coord of optical axis [pixels]
/// \param[out]  opticrawxmm        RAW X coord of optical axis [mm]
/// \param[out]  opticrawymm        RAW Y coord of optical axis [mm]
/// \param[out]  numdeltatt         Number of rows used from CAMS file after sampling
/// \param[out]  deltarawx          RAW X CAMS offset [pixels]
/// \param[out]  deltarawy          RAW Y CAMS offset [pixels]
/// \param[out]  deltarawxmm        RAW X CAMS offset [mm]
/// \param[out]  deltarawymm        RAW Y CAMS offset [mm]
/// \param[out]  deltasinangle      Sine of CAMS rotation
/// \param[out]  deltacosangle      Cosine of CAMS rotation
/// \param[out]  rtnumrows          Number of rows in raytracing file
/// \param[out]  rt_min_e           Minimum energy in raytracing file
/// \param[out]  rt_max_e           Maximum energy in raytracing file
/// \param[out]  rtnumoffaxis       Number of offaxis angles in raytracing file
/// \param[out]  rtoffaxis          Array of offaxis angles in raytracing file
/// \param[out]  effgeomarea        Geometric area multiplied by cos(theta) and exp time factors
/// \param[out]  inputgrpoffaxis    Offaxis angle of every input group
/// \param[out]  inputgrpoffaxisindex Index for offaxis angle of every input group
/// \param[out]  deltattbinsperoffaxis Counts delta attitude bins for each offaxis angle
/// \param[out]  tbinexposure       Exposure time per CAMS time bin
/// \param[out]  totalexposure      Summed exposure time
/// \param[out]  offaxisid          Offaxis angle group number index for each time bin
/// \param[out]  gtipointer         GTI number for each CAMS time point
/// \param[out]  inputphotonsperenergy Pseudo-photons per energy for normalization of EA 
/// \param[out]  kevunits           Flag for whether input RMF file is in KeV
/// \param[out]  ekevlo             Lower bounds of RMF energy bins [keV]
/// \param[out]  ekevhi             Upper bounds of RMF energy bins [keV]
/// \param[out]  eoutkev            Centers of RMF energy bins [keV]
/// \param[out]  xrtregions         Event selection regions in telescope coordinates
/// \param[out]  fpemap             Exposure map file pointer
/// \param[out]  fpxrt              Raytracing file pointer
/// \param[out]  actimg             Structure containing image geometry information
void initialize (Par & par, 
  rmflib::RMFData & rmfdat, 
  long & numeoutbins, long & numrmfinputbins,
  long & numrmfchan, Double3D & inputrmfs,
  Double2D & rspmatrix, Double1D & outauxtrans, double & geomarea,
  long & numebinscoarse, Double1D & ecoarsecen, Double1D & coarsearfeffarea,  
  long & numebinsfine, Double1D & efinecen, Double1D & finearfeffarea, 
  double & xmmperpixel, double & ymmperpixel, double & fpmm2arcmin,
  double & optaxisXstdmm, double & optaxisYstdmm,
  double & rawxminmm, double & rawxmaxmm, double & rawyminmm, double & rawymaxmm,
  long & numrawx, long & numrawy, long & numactx, long & numacty,
  Double1D & rawx, Double1D & rawy,
  Double1D & actxlo, Double1D & actylo,
  Double4D & qemap, Double2D & avgqemap,
  int & numcoeff, double *** vignetcoeff, Double2D & hxibaffle,
  Double1D & opticrawx, Double1D & opticrawy, 
  Double1D & opticrawxmm, Double1D & opticrawymm, 
  long & numdeltatt,
  Double1D & deltarawx, Double1D & deltarawy, 
  Double1D & deltarawxmm, Double1D & deltarawymm, 
  Double1D & deltasinangle, Double1D & deltacosangle,
  long & rtnumrows, double & rt_min_e, double & rt_max_e,
  long & rtnumoffaxis, Double1D & rtoffaxis, double & effgeomarea,
  Double1D & inputgrpoffaxis, Long1D & inputgrpoffaxisindex,
  Double1D & deltattbinsperoffaxis,
  Double1D & tbinexposure, double & totalexposure,
  Long1D & offaxisid, Long1D & gtipointer, Double1D & inputphotonsperenergy,
  bool & kevunits, Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev, 
  RegionArray & xrtregions, ahfits::FilePtr & fpemap, 
  ahfits::FilePtr & fpxrt, CRKeywords & actimg);
  
/// \brief Produce flatfield and RSP
/// \param[in]   par                Structure holding parameters
/// \param[out]  insufficientphotons Flag for not enough photons for ARF/RSP
/// \param[in]   numeoutbins        Number of energy output bins
/// \param[in]   numrmfinputbins    Number of input energy bins in RMF
/// \param[in]   numrmfchan         Number of output energy channels in RMF
/// \param[in]   inputrmfs          RMFs read from file or files
/// \param[in]   rspmatrix          Output RSP matrix, allocated at correct size
/// \param[in]   outauxtrans        Auxiliary transmission
/// \param[in]   arfeffarea         Output ARF, allocated at correct size
/// \param[in]   numebinscoarse     Number of energy bins in coarse ARF
/// \param[in]   ecoarsecen         Energy bin centers for coarse ARF
/// \param[in]   coarsearfeffarea   EA from coarse ARF
/// \param[in]   numebinsfine       Number of energy bins in fine ARF
/// \param[in]   efinecen           Energy bin centers for fine ARF
/// \param[in]   finearfeffarea     EA from fine ARF
/// \param[in]   xmmperpixel        Physical pixel size in X [mm]
/// \param[in]   ymmperpixel        Physical pixel size in Y [mm]
/// \param[in]   fpmm2arcmin        Conversion factor [arcmin/mm] in focal plane
/// \param[in]   optaxisXstdmm      Optical axis X position [mm]
/// \param[in]   optaxisYstdmm      Optical axis Y position [mm]
/// \param[in]   rawxminmm          Lower bound of RAW X [mm]
/// \param[in]   rawxmaxmm          Upper bound of RAW X [mm]
/// \param[in]   rawyminmm          Lower bound of RAW Y [mm]
/// \param[in]   rawymaxmm          Upper bound of RAW Y [mm]
/// \param[in]   numrawx            RAW X dimension
/// \param[in]   numrawy            RAW Y dimension
/// \param[in]   numactx            ACT X dimension
/// \param[in]   numacty            ACT Y dimension
/// \param[in]   rawx               RAW X pixel centers [pixels]
/// \param[in]   rawy               RAW Y pixel centers [pixels]
/// \param[in]   actxlo             ACT X pixel lower bounds [pixels]
/// \param[in]   actylo             ACT Y pixel lower bounds [pixels]
/// \param[in]   qemap              HXI QE map
/// \param[in]   avgqemap           HXI QE map averaged over energies and summed over layers
/// \param[in]   numcoeff           Number of vignetting coefficients
/// \param[in]   vignetcoeff        Vignetting coefficients
/// \param[in]   hxibaffle          HXI baffle mask
/// \param[in]   opticrawx          RAW X coord of optical axis [pixels]
/// \param[in]   opticrawy          RAW Y coord of optical axis [pixels]
/// \param[in]   opticrawxmm        RAW X coord of optical axis [mm]
/// \param[in]   opticrawymm        RAW Y coord of optical axis [mm]
/// \param[in]   numdeltatt         Number of rows used from CAMS file after sampling
/// \param[in]   deltarawx          RAW X CAMS offset [pixels]
/// \param[in]   deltarawy          RAW Y CAMS offset [pixels]
/// \param[in]   deltarawxmm        RAW X CAMS offset [mm]
/// \param[in]   deltarawymm        RAW Y CAMS offset [mm]
/// \param[in]   deltasinangle      Sine of CAMS rotation
/// \param[in]   deltacosangle      Cosine of CAMS rotation
/// \param[in]   rtnumrows          Number of rows in raytracing file
/// \param[in]   rt_min_e           Minimum energy in raytracing file
/// \param[in]   rt_max_e           Maximum energy in raytracing file
/// \param[in]   rtnumoffaxis       Number of offaxis angles in raytracing file
/// \param[in]   rtoffaxis          Array of offaxis angles in raytracing file
/// \param[in]   effgeomarea        Geometric area multiplied by cos(theta) and exp time factors
/// \param[in]   inputgrpoffaxis    Offaxis angle of every input group
/// \param[in]   inputgrpoffaxisindex Index for offaxis angle of every input group
/// \param[in]   deltattbinsperoffaxis Counts delta attitude bins for each offaxis angle
/// \param[in]   tbinexposure       Exposure time per CAMS time bin
/// \param[in]   totalexposure      Summed exposure time
/// \param[in]   offaxisid          Offaxis angle group number index for each time bin
/// \param[in]   gtipointer         GTI number for each CAMS time point
/// \param[in]   inputphotonsperenergy Pseudo-photons per energy for normalization of EA 
/// \param[in]   eoutkev            Centers of RMF energy bins [keV]
/// \param[in]   xrtregions   Event selection regions in telescope coordinates
/// \param[in]   fpemap             Exposure map file pointer
/// \param[in]   fpxrt              Raytracing file pointer
/// \param[in]   actimg             Keywords related to coordinates and image geometry
/// \param[in]   outimg             Output flatfield image
void doWork(Par & par, bool & insufficientphotons,
  long numeoutbins, long numrmfinputbins,
  long numrmfchan, Double3D & inputrmfs,
  Double2D & rspmatrix, Double1D & outauxtrans, Double1D & arfeffarea,
  long numebinscoarse, Double1D & ecoarsecen, Double1D & coarsearfeffarea,
  long numebinsfine, Double1D & efinecen, Double1D & finearfeffarea, 
  double xmmperpixel, double ymmperpixel, double fpmm2arcmin,
  double optaxisXstdmm, double optaxisYstdmm,
  double rawxminmm, double rawxmaxmm, double rawyminmm, double rawymaxmm,
  long numrawx, long numrawy, long numactx, long numacty,
  Double1D & rawx, Double1D & rawy,
  Double1D & actxlo, Double1D & actylo,
  Double4D & qemap, Double2D & avgqemap,
  int numcoeff, double ** vignetcoeff, Double2D & hxibaffle,
  Double1D & opticrawx, Double1D & opticrawy,
  Double1D & opticrawxmm, Double1D & opticrawymm, long numdeltatt,
  Double1D & deltarawx, Double1D & deltarawy, 
  Double1D & deltarawxmm, Double1D & deltarawymm, 
  Double1D & deltasinangle, Double1D & deltacosangle,
  long rtnumrows, double rt_min_e, double rt_max_e,
  long rtnumoffaxis, Double1D & rtoffaxis, double effgeomarea,
  Double1D & inputgrpoffaxis, Long1D & inputgrpoffaxisindex,
  Double1D & deltattbinsperoffaxis,
  Double1D & tbinexposure, double & totalexposure,
  Long1D & offaxisid, Long1D & gtipointer, Double1D & inputphotonsperenergy,
  Double1D & eoutkev, RegionArray & xrtregions, ahfits::FilePtr fpemap,
  ahfits::FilePtr fpxrt, CRKeywords & actimg, Double2D & outimg);

/// \brief Write output files, close files, deallocate any C-style arrays
/// \param[in]   par                Structure holding parameters
/// \param[in]   fpemap             Exposure map file pointer
/// \param[in]   fpxrt              Raytracing file pointer
/// \param[in]   insufficientphotons Flag for not enough photons for ARF/RSP
/// \param[in]   geomarea           Geometric area of telescope
/// \param[in]   numeoutbins        Number of energy output bins
/// \param[in]   kevunits           Flag for whether input RMF file is in KeV
/// \param[in]   ekevlo             Lower bounds of RMF energy bins [keV]
/// \param[in]   ekevhi             Upper bounds of RMF energy bins [keV]
/// \param[in]   eoutkev            Centers of RMF energy bins [keV]
/// \param[in]   rmfdat             RMF data structure used to build output RSP
/// \param[in[   rspmatrix          Output RSP matrix
/// \param[in]   arfeffarea         Output ARF
/// \param[in]   actimg             Keywords related to coordinates and image geometry
/// \param[in]   numactx            ACT X dimension
/// \param[in]   numacty            ACT Y dimension
/// \param[in]   outimg             Output flatfield image
/// \param[in]   vignetcoeff        Vignetting coefficients
void finalize(Par & par, ahfits::FilePtr fpemap, ahfits::FilePtr fpxrt,
  bool insufficientphotons, double geomarea, long numeoutbins,
  bool kevunits, Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev, 
  rmflib::RMFData & rmfdat, Double2D & rspmatrix, Double1D & arfeffarea,
  CRKeywords & actimg, long numactx, long numacty, 
  Double2D & outimg, double ** vignetcoeff);

/// \brief Write out flatfield image
/// \param[in]   stopsys            Coordinate system of flatfield
/// \param[in]   fpemap             Exposure map file pointer
/// \param[in]   filename           Name of output flatfield file
/// \param[in]   actimg             Keywords related to coordinates and image geometry
/// \param[in]   xsize              Image X size [pixels]
/// \param[in]   ysize              Image Y size [pixels]
/// \param[in]   image              Image to be written
void writeimg(std::string & stopsys, ahfits::FilePtr fpemap, std::string & filename, 
  CRKeywords & actimg, long xsize, long ysize, Double2D & image);

/// \brief Write out RSP matrix
/// \param[in]   outrspfile         Name of output RSP file
/// \param[in]   numeoutbins        Number of energy output bins
/// \param[in]   ekevlo             Lower bounds of RMF energy bins [keV]
/// \param[in]   ekevhi             Upper bounds of RMF energy bins [keV]
/// \param[in]   eoutkev            Centers of RMF energy bins [keV]
/// \param[in]   rmfdat             RMF data structure used to build output RSP
/// \param[in[   rspmatrix          Output RSP matrix
void writerspfile(std::string outrspfile, long numeoutbins, 
  Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev, 
  rmflib::RMFData & rmfdat, Double2D & rspmatrix);

/// \brief Read the coordinate-related keywords from histogram header
/// \param[in]   ahffp              File pointer for exposure map file
/// \param[out]  keywd              Structure to contain keyword values
void getCoordKeywords(ahfits::FilePtr ahffp, CRKeywords& keywd);

/// \brief Compute transformation for flat field image
void computeXform2dActToDetFocOrSky (std::string& stopsys, XFORM2D* xform, 
  const double act_xcen, const double act_ycen, 
  const double detxflip, const double detyflip,
  const double cendet, const double foc_xoff, const double foc_yoff,
  const double focscaleratio, const double foc_rotd, const double cenfoc,
  const double roll_sign, const double ra_nom, const double dec_nom,
  const double ra_exp, const double dec_exp, const double pa_exp,
  const double censky, const double pixels_per_radian);

/// \brief Apply transformation to flat field image
void applyImageTransToImage(IMAGETRANS* trans_param, Double2D& input_image, 
  Double2D& output_image, double area_factor);

// Initialization routines for multidimensional arrays.

void initDouble1D(Double1D & dbl1d, long xsize);
void initDouble2D(Double2D & dbl2d, long xsize, long ysize);
void initDouble3D(Double3D & dbl3d, long xsize, long ysize, long zsize);
void initDouble4D(Double4D & dbl4d, long xsize, long ysize, long zsize, long psize);
void initLong1D(Long1D & long1d, long xsize);

// ****************************************************************************

/// \brief hxirspeffimg tool
int main(int argc, char** argv) {

  Par par;                         // Structure containing parameter values
  rmflib::RMFData rmfdat;
  ahfits::FilePtr fpemap = 0; // Exposure map
  ahfits::FilePtr fpxrt = 0;  // Raytracing event file

  // Variable names in parentheses in the comments are array dimensions.

  bool insufficientphotons = false;  // Flag for too few raytracing photons for ARF/RSP
  long numeoutbins = 0;       // Size of RMF input energy grid
  long numrmfinputbins = 0;   // Size of RMF input energy grid = numeoutbins
  long numrmfchan = 0;        // Size of RMF output energy grid
  Double3D inputrmfs;         // RMFs (numrmfinputbins x numrmfchan x #layers)
  double geomarea;            // Geometric area of telescope
  bool kevunits = false;      // Flag for whether input RMF file is in keV
  Double1D ekevlo;            // Low edge of each energy bin (numeoutbins)
  Double1D ekevhi;            // High edge of each energy bin (numeoutbins)
  Double1D eoutkev;           // Center of each energy bin (numeoutbins)

  long numebinscoarse = 0;    // Size of coarse energy grid for effective area (EA) 
  Double1D ecoarsecen;        // Center of each coarse energy bin for EA (numebinscoarse)
  Double1D coarsearfeffarea;  // EA for each coarse energy bin (numebinscoarse)
  long numebinsfine = 0;      // Size of fine energy grid for effective area (EA) 
  Double1D efinecen;          // Center of each fine energy bin for EA (numebinsfine)
  Double1D finearfeffarea;    // EA for each fine enrgy bin (numebinsfine)

  double xmmperpixel = 0.0;   // Pixel size in focal plane, X
  double ymmperpixel = 0.0;   // Pixel size in focal plane, Y
  double fpmm2arcmin = 0.0;   // "Plate scale" focal plane

  double optaxisXstdmm = 0.0; // Nominal RAWX coord of optical axis
  double optaxisYstdmm = 0.0; // Nominal RAWY coord of optical axis

  // Boundaries of RAW image space in millimeters in focal plane
  double rawxminmm=0.0, rawxmaxmm=0.0, rawyminmm=0.0, rawymaxmm=0.0;

  // Dimensions of RAW and ACT image spaces in pixels
  long numrawx=0, numrawy=0, numactx=0, numacty=0;

  Double1D rawx, rawy;        // RAW coordinates of pixel centers
  Double1D actxlo, actylo;    // ACT coordinates of lower pixel boundaries
  Double4D qemap;             // Quantum efficiency (QE) map per layer as read from CALDB file
  Double2D avgqemap;          // QE map averaged over energies and summed over layers

  Double1D outauxtrans;       // Auxiliary transmission as a function of energy

  int numcoeff = 0;           // Number of vignetting coefficients
  double** vignetcoeff = 0;   // Values of vignetting coefficients

  Double2D hxibaffle;         // Image of HXI baffle

  long numdeltatt = 0;        // Number of CAMS time points treated, after sampling

  // CAMS coordinates read from CAMS offset file (numdeltatt)
  Double1D deltarawx, deltarawy, deltasinangle, deltacosangle;
  Double1D opticrawx, opticrawy;

  // Equivalent values in millimeters (numdeltatt)
  Double1D deltarawxmm, deltarawymm;
  Double1D opticrawxmm, opticrawymm;

  Double1D tbinexposure;           // Exposure time for each CAMS time point, accounting for GTI
  double totalexposure = 0.0;      // Total exposure time in GTI
  Long1D offaxisid;                // Offaxis (theta) bin number for each CAMS time point
  Long1D gtipointer;               // GTI number for each CAMS time point

  long rtnumrows = 0;         // Number of raytracing (RT) event file rows, first extension
  double rt_min_e = 0.0;      // Lower bound of RT energies processed = min(ecoarscen)
  double rt_max_e = 0.0;      // Upper bound of RT energies processed = max(ecoarscen)
  long rtnumoffaxis = 0;      // Number of offaxis (theta) angles in RT run
  Double1D rtoffaxis;         // Array of offaxis (theta) angles from RT run (rtnumoffaxis)
  double effgeomarea = 0.0;   // Geometric area multiplied by cos(theta) and exposure time factors

  Double1D inputgrpoffaxis;       // RT input offaxis (theta) angle for each group
  Long1D inputgrpoffaxisindex;    // Bin number of input offaxis (theta) angle for each group
  Double1D inputphotonsperenergy;  // Total number of input pseudophotons per energy
  Double1D deltattbinsperoffaxis;  // CAMS time points per offaxis bin

  RegionArray xrtregions;
  CRKeywords actimg;

  Double2D rspmatrix;         // Output response (numrmfinputbins x numrmfchan)
  Double1D arfeffarea;        // Output ARF
  Double2D outimg;            // Output flatfield image in ACT, DET, FOC, or SKY coordinates

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par, rmfdat,
        numeoutbins, numrmfinputbins,
        numrmfchan, inputrmfs, rspmatrix, outauxtrans, geomarea,
        numebinscoarse, ecoarsecen, coarsearfeffarea, numebinsfine, efinecen,
        finearfeffarea, xmmperpixel, ymmperpixel, fpmm2arcmin,
        optaxisXstdmm, optaxisYstdmm,
        rawxminmm, rawxmaxmm, rawyminmm, rawymaxmm,
        numrawx, numrawy, numactx, numacty, rawx, rawy, actxlo, actylo,
        qemap, avgqemap,
        numcoeff, &vignetcoeff, hxibaffle,
        opticrawx, opticrawy,
        opticrawxmm, opticrawymm, numdeltatt,
        deltarawx, deltarawy, deltarawxmm, deltarawymm, 
        deltasinangle, deltacosangle,
        rtnumrows, rt_min_e, rt_max_e,
        rtnumoffaxis, rtoffaxis, effgeomarea,
        inputgrpoffaxis, inputgrpoffaxisindex, 
        deltattbinsperoffaxis,
        tbinexposure, totalexposure,
        offaxisid, gtipointer, inputphotonsperenergy, 
        kevunits, ekevlo, ekevhi, eoutkev,
        xrtregions, fpemap, fpxrt, actimg);
      doWork(par, insufficientphotons, 
        numeoutbins, numrmfinputbins,
        numrmfchan, inputrmfs, rspmatrix, outauxtrans, arfeffarea,
        numebinscoarse, ecoarsecen, coarsearfeffarea, numebinsfine, efinecen,
        finearfeffarea, xmmperpixel, ymmperpixel, fpmm2arcmin,
        optaxisXstdmm, optaxisYstdmm,
        rawxminmm, rawxmaxmm, rawyminmm, rawymaxmm,
        numrawx, numrawy, numactx, numacty, rawx, rawy, actxlo, actylo,
        qemap, avgqemap, numcoeff, vignetcoeff, hxibaffle, 
        opticrawx, opticrawy,
        opticrawxmm, opticrawymm, numdeltatt,
        deltarawx, deltarawy, deltarawxmm, deltarawymm, 
        deltasinangle, deltacosangle,
        rtnumrows, rt_min_e, rt_max_e,
        rtnumoffaxis, rtoffaxis, effgeomarea,
        inputgrpoffaxis, inputgrpoffaxisindex, 
        deltattbinsperoffaxis,
        tbinexposure, totalexposure,
        offaxisid, gtipointer, inputphotonsperenergy, eoutkev, xrtregions, 
        fpemap, fpxrt, actimg, outimg);
      finalize(par, fpemap, fpxrt, 
        insufficientphotons, geomarea, numeoutbins,
        kevunits, ekevlo, ekevhi, eoutkev, 
        rmfdat, rspmatrix, arfeffarea,
        actimg, numactx, numacty, outimg, vignetcoeff);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, rmfdat,
          numeoutbins, numrmfinputbins,
          numrmfchan, inputrmfs, rspmatrix, outauxtrans, geomarea,
          numebinscoarse, ecoarsecen, coarsearfeffarea, numebinsfine, efinecen,
          finearfeffarea, xmmperpixel, ymmperpixel, fpmm2arcmin,
          optaxisXstdmm, optaxisYstdmm,
          rawxminmm, rawxmaxmm, rawyminmm, rawymaxmm,
          numrawx, numrawy, numactx, numacty, rawx, rawy, actxlo, actylo,
          qemap, avgqemap, numcoeff, &vignetcoeff, hxibaffle,
          opticrawx, opticrawy,
          opticrawxmm, opticrawymm, numdeltatt,
          deltarawx, deltarawy, deltarawxmm, deltarawymm, 
          deltasinangle, deltacosangle,
          rtnumrows, rt_min_e, rt_max_e,
          rtnumoffaxis, rtoffaxis, effgeomarea,
          inputgrpoffaxis, inputgrpoffaxisindex, 
          deltattbinsperoffaxis,
          tbinexposure, totalexposure,
          offaxisid, gtipointer, inputphotonsperenergy, 
          kevunits, ekevlo, ekevhi, eoutkev, 
          xrtregions, fpemap, fpxrt, actimg);
        doWork(par, insufficientphotons, numeoutbins, numrmfinputbins,
          numrmfchan, inputrmfs, rspmatrix, outauxtrans, arfeffarea,
          numebinscoarse, ecoarsecen, coarsearfeffarea, numebinsfine, efinecen,
          finearfeffarea, xmmperpixel, ymmperpixel, fpmm2arcmin,
          optaxisXstdmm, optaxisYstdmm,
          rawxminmm, rawxmaxmm, rawyminmm, rawymaxmm,
          numrawx, numrawy, numactx, numacty, rawx, rawy, actxlo, actylo,
          qemap, avgqemap, numcoeff, vignetcoeff, hxibaffle,
          opticrawx, opticrawy,
          opticrawxmm, opticrawymm, numdeltatt,
          deltarawx, deltarawy, deltarawxmm, deltarawymm,
          deltasinangle, deltacosangle,
          rtnumrows, rt_min_e, rt_max_e,
          rtnumoffaxis, rtoffaxis, effgeomarea,
          inputgrpoffaxis, inputgrpoffaxisindex, 
          deltattbinsperoffaxis,
          tbinexposure, totalexposure,
          offaxisid, gtipointer, inputphotonsperenergy, eoutkev, xrtregions, 
          fpemap, fpxrt, actimg, outimg);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(par, fpemap, fpxrt, 
          insufficientphotons, geomarea, numeoutbins,
          kevunits, ekevlo, ekevhi, eoutkev, 
          rmfdat, rspmatrix, arfeffarea,
          actimg, numactx, numacty, outimg, vignetcoeff);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par & par) {
  
  stringstream ss;         // For formatting filenames
  
  std::string dateobs;     // For CALDB queries
  ahfits::FilePtr fpemap;  //

  Double1D baffle_array, erange_array;
  initDouble1D(baffle_array, 6);
  initDouble1D(erange_array, 2);
  
  par.m_telescop=ahapp::getParString("telescop");
  par.m_instrume=ahapp::getParString("instrume");
  par.m_erange_str=ahapp::getParString("erange");
  par.m_dattfile=ahapp::getParString("dattfile");
  par.m_filtoffsetfile=ahapp::getParString("filtoffsetfile");
  par.m_emapfile=ahapp::getParString("emapfile");
  par.m_qefile=ahapp::getParString("qefile");
  par.m_rmffile=ahapp::getParString("rmffile");
  par.m_onaxisffile=ahapp::getParString("onaxisffile");
  par.m_onaxiscfile=ahapp::getParString("onaxiscfile");
  par.m_polydeg=ahapp::getParString("polydeg");
  par.m_vigfile=ahapp::getParString("vigfile");
  par.m_auxtransfile=ahapp::getParString("auxtransfile");
  par.m_outflatfile=ahapp::getParString("outflatfile");
  par.m_outmaptype=ahapp::getParString("outmaptype");
  par.m_stopsys=ahapp::getParString("stopsys");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_regionfile=ahapp::getParString("regionfile");
  par.m_xrtevtfile=ahapp::getParString("xrtevtfile");
  par.m_minphoton=ahapp::getParInt("minphoton");
  par.m_sampling=ahapp::getParInt("sampling");
  par.m_baffle_str=ahapp::getParString("baffle");
  par.m_rmfthresh=ahapp::getParDouble("rmfthresh");

  // Convert string list to vector:  baffle parameters.
  arfgenlib::listStringsToDoubles(par.m_baffle_str, baffle_array);
  par.m_baffle.m_centerx = baffle_array[0];
  par.m_baffle.m_centery = baffle_array[1];
  par.m_baffle.m_toprad = baffle_array[2];
  par.m_baffle.m_topheight = baffle_array[3];
  par.m_baffle.m_botrad = baffle_array[4];
  par.m_baffle.m_botheight = baffle_array[5];

  // Construct filenames for RSP and ARF files.
  if (ahgen::strtoupper(par.m_outfile) != "NONE") {
    par.m_outrspfile = par.m_outfile + ".rsp";
    par.m_outarffile = par.m_outfile + ".arf";
  } else {
    par.m_outrspfile = "NONE";
    par.m_outarffile = "NONE";
  }

  // Convert string list to vector:  energy range parameters.
  arfgenlib::listStringsToDoubles(par.m_erange_str, erange_array);
  par.m_erange.m_eminimg = erange_array[0];
  par.m_erange.m_emaximg = erange_array[1];
  
  // Get DATE-OBS.  (Probably all of this should be moved to initialize.)
  ahfits::open(par.m_emapfile, "OFFAXISHIST", &fpemap);
  dateobs = ahfits::getKeyValStr(fpemap, "DATE-OBS");
  ahfits::close(fpemap);

  // Search CALDB for correct files if CALDB is given instead of file name.
  par.m_actual_qefile = ahmission::caldb::resolve(par.m_qefile, 
    "quantum efficiency", par.m_instrume, "-", "QE", dateobs, "-",par.m_telescop);
  ape_trad_set_string("qefile", par.m_actual_qefile.c_str());

  par.m_actual_vigfile = ahmission::caldb::resolve(par.m_vigfile,
    "vignetting coefficients", "GEN", "-", "VIGNCOEF", "-", "-", par.m_telescop);

  ape_trad_set_string("vigfile", par.m_actual_vigfile.c_str());

  // Specifying nothing for auxtransfile is the same as specifying "NONE".
  if (par.m_auxtransfile == "") {
    par.m_auxtransfile = "NONE";
  }  
  par.m_actual_auxtransfile = ahmission::caldb::resolve(par.m_auxtransfile, "auxiliary transmission",
    par.m_instrume, "-", "AUXTRAN", dateobs, "-", par.m_telescop);

  ape_trad_set_string("auxtransfile", par.m_actual_auxtransfile.c_str());

  // If there is no QE file then only use the first RMF file (dummy
  // QE values of 1.0 will be used).
  if (ahgen::strtoupper(par.m_qefile) == "NONE") {
    par.m_numlayers = 1;
    AH_INFO(ahlog::HIGH) << "QEFILE=NONE is a diagnostic mode because the QE map and LSF file are a matched set" << std::endl;
  } else {
    par.m_numlayers = 5;
  }

  // Temporary variables for stripping extended syntax
  std::string t_str = ahmission::caldb::resolve(par.m_rmffile, "response matrix", par.m_instrume,
    "-", "RMF_LAYER0", dateobs, "-", par.m_telescop);
  std::size_t t_found = t_str.find("[");   // Strip off extended syntax
  par.m_actual_rmffile = t_str.substr(0, t_found);

  // Substitute the first actual filename into the parameter value - only for CALDB.
  ape_trad_set_string("rmffile", par.m_actual_rmffile.c_str());
  
  // Number of input RMF files used (this is the current format    
  // - may change?)
  par.m_numrmffiles = par.m_numlayers;

  t_str = ahmission::caldb::resolve(par.m_onaxisffile, "fine grid effective area", par.m_instrume,
    "-", "EFFAREAFNE", "-", "-", par.m_telescop);
  par.m_actual_onaxisffile = t_str;
  ape_trad_set_string("onaxisffile", par.m_actual_onaxisffile.c_str());

  t_str = ahmission::caldb::resolve(par.m_onaxiscfile, "coarse grid effective area", par.m_instrume,
    "-", "EFFAREACRS", "-", "-", par.m_telescop);
  par.m_actual_onaxiscfile = t_str;
  ape_trad_set_string("onaxiscfile", par.m_actual_onaxiscfile.c_str());

  // Convert POLYDEG to number
  if (ahgen::strtoupper(par.m_polydeg) == "DEFAULT") {
    par.m_numeric_polydeg = -1;
  } else {
    ss.str(par.m_polydeg);
    ss >> par.m_numeric_polydeg;
  }

  // Set flags to control which files are produced.
  if (ahgen::strtoupper(par.m_outflatfile) == "NONE" &&
    ahgen::strtoupper(par.m_outfile) == "NONE") {
    AH_THROW_RUNTIME("Both OUTFLATFILE and OUTFILE are set to NONE; exiting");
  }
  par.m_doflatfield = false;
  par.m_dorsp = false;
  if (ahgen::strtoupper(par.m_outflatfile) != "NONE") {
    par.m_doflatfield = true;
  }
  if (ahgen::strtoupper(par.m_outfile) != "NONE") {
    par.m_dorsp = true;
  }

  par.m_doregion = true;
  if (ahgen::strtoupper(par.m_regionfile) == "NONE" || !par.m_dorsp) {
    par.m_doregion = false;
  }  

}

// ****************************************************************************

void initialize (Par & par, 
  rmflib::RMFData & rmfdat,
  long & numeoutbins, long & numrmfinputbins,
  long & numrmfchan, Double3D & inputrmfs,
  Double2D & rspmatrix, Double1D & outauxtrans, 
  double & geomarea,
  long & numebinscoarse, Double1D & ecoarsecen, Double1D & coarsearfeffarea,  
  long & numebinsfine, Double1D & efinecen, Double1D & finearfeffarea, 
  double & xmmperpixel, double & ymmperpixel, double & fpmm2arcmin,
  double & optaxisXstdmm, double & optaxisYstdmm,
  double & rawxminmm, double & rawxmaxmm, double & rawyminmm, double & rawymaxmm,
  long & numrawx, long & numrawy, long & numactx, long & numacty,
  Double1D & rawx, Double1D & rawy,
  Double1D & actxlo, Double1D & actylo,
  Double4D & qemap, Double2D & avgqemap,
  int & numcoeff, double *** vignetcoeff, Double2D & hxibaffle,
  Double1D & opticrawx, Double1D & opticrawy,
  Double1D & opticrawxmm, Double1D & opticrawymm,
  long & numdeltatt,
  Double1D & deltarawx, Double1D & deltarawy, 
  Double1D & deltarawxmm, Double1D & deltarawymm, 
  Double1D & deltasinangle, Double1D & deltacosangle,
  long & rtnumrows, double & rt_min_e, double & rt_max_e,
  long & rtnumoffaxis, Double1D & rtoffaxis, double & effgeomarea,
  Double1D & inputgrpoffaxis, Long1D & inputgrpoffaxisindex, 
  Double1D & deltattbinsperoffaxis,
  Double1D & tbinexposure, double & totalexposure,
  Long1D & offaxisid, Long1D & gtipointer, Double1D & inputphotonsperenergy,
  bool & kevunits, Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev,
  RegionArray & xrtregions, ahfits::FilePtr & fpemap, ahfits::FilePtr & fpxrt, CRKeywords & actimg) {

  rmflib::RMFData rmfdatin;  // For reading the RMFs
  
  std::stringstream ss;  // For formatting strings using stream I/O

  Double1D eoutlo;       // Lower limits of output energy channels
  Double1D eouthi;       // Upper limits of output energy channels
  Double1D eoutcen;      // Central energies of output energy channels
  Double1D matrixrow;    // Unpacked row of RMF matrix

  // Coarse energy grid from the input coarse on-axis EA file; the EA
  // file has lower and upper energy bounds, but the raytracing has
  // single energies, so we need the center energies of the ARF
  // energy bins.
  Double1D ecoarsecenfull;        // Coarse energy bin centers
  Double1D coarsearfeffareafull;  // EA for coarse energy bins
  std::string coarsearffile = par.m_actual_onaxiscfile;  // Coarse effective area file
  long numebinscoarsefull = 0;    // Number of energy bins in coarse effective area file

  // Fine energy grid from the input fine-grid on-axis EA file; the EA
  // file has lower and upper energy bounds, but the raytracing has
  // single energies, so we need the center energies of the ARF
  // energy bins.
  std::string finearffile = par.m_actual_onaxisffile;

  ahfits::FilePtr fpqe = 0;        // File pointer for QE map file
  std::string qemap_instrume("");  // INSTRUME keyword from QE map

  ahfits::FilePtr fptrans = 0;  // File pointer for auxiliary transmission file
  ahfits::Router *rttrans = 0;   // Data router for auxiliary transmission file
  Double1D axtenergieskev;      // Energy grid of auxiliary transmission file
  Double1D auxtrans;            // Input auxiliary transmission
  long numaxtenergies = 0;      // Number of rows in aux transmission file
  double l_axtenergy = 0.0;     // Local variable to read energy for aux trans
  double l_auxtrans = 0.0;      // Local variable to read auxiliary transmission
  double newtrans = 0.0;        // Interpolated auxiliary transmission
  double axtconvenergy = 1.0;   // Aux trans energy unit conversion
  std::string axtfileunits = "";   // Energy units in aux transmission file

  long numexpmaps = 0;             // Number of component exposure maps
  Double1D fracexptime;            // Fractional exposure time per component map
  Double1D emapoffaxisval;         // Offaxis (theta) bin average value
  Double1D emaptimeintervals;      // Time intervals corresponding to offaxis bins
  Double1D tfractionratio;         // Proportional to weight of each exposure map
  double emaptotexposure = 0.0;    // Total exposure time in all exposure maps
  long emapnumoffaxis = 0;         // Number of entries in OFFAXISHIST
  Long1D numrtpairs;               // Number of (theta,phi) raytracing pairs per bin
  Long1D pairindexlo;              // First member of pair
  Long1D pairindexhi;              // Second member of pair
  ahfits::Router* rtemap;          // FITS column info for off-axis angle historgram
  double l_fracexptime = 0.0;      // Local variable to read fractional exposure time
  double l_offaxislo = 0.0;        // Local variable to read offaxis angle
  double l_offaxishi = 0.0;        // Local variable to read offaxis angle
  double l_timeinterval = 0.0;     // Local variable to read time for offaxis bin
  long igrp = 0;                   // Running group number for groups of offaxis bins
  Long1D igrpstart;                // Start of each group of offaxis bins
  Long1D igrpend;                  // End of each group of offaxis bins
  Double1D grpexpfrac;             // Exposure fraction for each group
  long numgrptheta = 0;            // Number of unique theta bins
  Double1D inputfulloffaxis;       // Complete list of offaxis angles
  Long1D inputfulloffaxisindex;    // Index to complete list of offaxis angles

  double rawmaprefx = 0.0;         // Coordinate reference value, RAW maps, X
  double rawmaprefxpixel = 0.0;    // Coordinate reference pixel, RAW maps, X
  double rawmapdeltax = 0.0;       // Coordinate increment, RAW maps, X

  double rawmaprefy = 0.0;         // Coordinate reference value, RAW maps, Y
  double rawmaprefypixel = 0.0;    // Coordinate reference pixel, RAW maps, Y
  double rawmapdeltay = 0.0;       // Coordinate increment, RAW maps, Y

  // OPTAXISX, OPTAXISY are the (invariant) position in DET 
  // coordinates of the optical axis (ultimately from the teldef file).
  double optaxisx = 0.0;
  double optaxisy = 0.0;
  double focallength = 0.0;        // HXT focal length (mm)

  // Lower and upper bounds of the RAW and ACT pixels.
  Double1D rawxlo, rawxhi, rawylo, rawyhi;
  Double1D actxhi, actyhi, actxcen, actycen;
  
  long totnumgti = 0;              // Number of GTIs
  Double1D gtistart, gtistop;      // Start and stop times of GTIs
  Long1D gtioffaxisindex;          // Offaxis angle index of each GRI
  double l_start=0.0, l_stop=0.0;  // Local variable for GTI start and stop time
  long rtnumoffaxisfull = 0;       // Counter for offaxis bins including duplicates
  std::string gtiextname("");      // Name of GTI file extension

  Double1D camsfiletimes;          // Time vector for CAMS data, full set
  Double1D opticrawxfile;          // Optical axis RAWX
  Double1D opticrawyfile;          // Optical axis RAWY
  Double1D camstimes;              // Time vector for CAMS data subset
  long numcamsfiletimes = 0;       // Number of CAMS times, full set
  ahfits::FilePtr fpdatt = 0;      // FITS file pointer, delta-attitude file
  ahfits::Router* rtdatt;          // Column data router, delta-attitude file

  Double2D rtnorms;                // Raytracing normalization factors
  long rtnumens = 0;               // Number of raytracing energies (NUMENRG keyword)

  ahfits::ListStringType regionfilenames;  // Names of region files
  long numregions = 0;             // Number of region files

  AH_DEBUG << "Entering initialize" << std::endl;

  numrmfinputbins = 0;   // Number of input energy bins (i.e., input bins of the RMF)
  numeoutbins = 0;       // Number of output energy bins (same as previous; see below)
  numrmfchan = 0;        // Number of RMF channels (i.e., output bins of the RMF)

  //                   input                 input      output    output     output
  //arfgenlib::rmfproc(par.m_rmffilelist[0], getmatrix, rmfegrid, rmfmatrix, kevunits);
  rmflib::openRMFFile(&rmfdatin, par.m_actual_rmffile, "RMF_LAYER0"); 

  // Size of the RMF input energy grid, which is equal to the
  // size of the energy grid in the output telescope effective
  // area array and equal to the input energy array of the output
  // .rsp file
  numeoutbins = rmfdatin.m_nmat;

  // To avoid confusion, assign numeoutbins to an additional
  // variable because it refers to both input and output.
  numrmfinputbins = numeoutbins;

  for (long i=0; i<numrmfinputbins; i++) {
    eoutlo.push_back(rmfdatin.m_ematstart[i]);
    eouthi.push_back(rmfdatin.m_ematstop[i]);
    eoutcen.push_back(rmfdatin.m_ematcen[i]);
  }

  // Number of "channels" - i.e., bins of the detected energy
  // array, which are output in relation to the RMF.
  numrmfchan = rmfdatin.m_nchan;

  // Create an energy grid (for interpolation) for mirror output rays
  // that is in units of keV, regardless of the energy units
  // in the RMF.
  kevunits = rmfdatin.m_kevunits;
  if (kevunits) {
    for (long i=0; i<numeoutbins; i++) {
      eoutkev.push_back(eoutcen[i]);
      ekevlo.push_back(eoutlo[i]);
      ekevhi.push_back(eouthi[i]);
    }
  } else {
    for (long i=0; i<numeoutbins; i++) {
      eoutkev.push_back(eoutcen[i]/1000.0);
      ekevlo.push_back(eoutlo[i]/1000.0);
      ekevhi.push_back(eouthi[i]/1000.0);
    }
  }

  // Put the RMF matrix data from all the response matrices into
  // a single array.

  // Set sizes of arrays and initialize to zero.
  initDouble3D(inputrmfs, numrmfinputbins, numrmfchan, par.m_numlayers);
  initDouble1D(matrixrow, numrmfchan);

  for (long k=0; k<par.m_numlayers; k++) {
    // If k==0 use the rmfmatrix structure already obtained from
    // the first file; otherwise, read next matrix and overwrite
    // the previous array.

    // The desired matrix data is in extension k+1 or extension
    // name RMF_LAYER+k.
    if (k>0) {
      //arfgenlib::rmfproc(par.m_rmffilelist[k], getmatrix, rmfegrid, rmfmatrix, kevunits);
      ss.str("");
      ss << "RMF_LAYER" << k;
      rmflib::openRMFFile(&rmfdatin, par.m_actual_rmffile, ss.str()); 
    }

    // +++ 2015-12-17 RSH Loops rearranged from TRF.
    for (long rowidx=0; rowidx<numrmfinputbins; rowidx++) {
      rmflib::readMatrixRow(&rmfdatin, &matrixrow[0], numrmfchan, rowidx);
      for (long j=0; j<numrmfchan; j++) {
        inputrmfs[rowidx][j][k] = matrixrow[j];
      }
    }
    if (k == 0) {
      //  Save RMF setup information from first RMF
      copyRMFData(&rmfdat, &rmfdatin);
    }
    rmflib::closeRMFFile(&rmfdatin);
  }

  AH_INFO(ahlog::LOW) << "Assembled RMFs" << std::endl;

  // Size the output RSP matrix and set to zero.
  initDouble2D(rspmatrix, numrmfinputbins, numrmfchan);  // Parameter of this function

  // Get auxiliary transmission array from input file auxtransfile and remap
  // it onto the output energy grid.  If the units of the energy column
  // in auxtransfile are eV then convert to keV.

  // The auxiliary transmission array is initialized to 1.0 for all energies.
  // If the energy grid in auxtransfile does not cover the whole range of
  // tne output energy grid, then those parts not covered will have
  // values of 1.0.
  initDouble1D(outauxtrans, numeoutbins);  // Parameter of this function
  for (long i=0; i<numeoutbins; ++i) {
    outauxtrans[i] = 1.0;
  }

  // Open auxtransfile (input parameter) if it exists.

  if (ahgen::strtoupper(par.m_actual_auxtransfile) != "NONE") {
    ahfits::open(par.m_actual_auxtransfile, "TRANSMISSION", &fptrans);
    axtfileunits = ahfits::columnUnits(fptrans, "ENERGY");
    axtconvenergy = 1.0;
    if (ahgen::strtoupper(axtfileunits) == "EV") {
      axtconvenergy = 0.001;
    }
    numaxtenergies = ahfits::getKeyValLLong(fptrans, "NAXIS2");
    initDouble1D(axtenergieskev, numaxtenergies);
    initDouble1D(auxtrans, numaxtenergies);
    rttrans = new ahfits::Router(fptrans);
    rttrans->connectScalar(ahfits::e_READONLY, "ENERGY", l_axtenergy);
    rttrans->connectScalar(ahfits::e_READONLY, "TRANSMISSION", l_auxtrans);
    ahfits::firstRow(fptrans);
    for (long i=0; i<numaxtenergies; ++i) {
      ahfits::readRow(fptrans);
      axtenergieskev[i] = l_axtenergy*axtconvenergy;
      auxtrans[i] = l_auxtrans;
      ahfits::nextRow(fptrans);
    }

    // Interpolate the file transmission array onto the output 
    // transmission array.

    for (long i=0; i<numeoutbins; ++i) {
      if (eoutkev[i] >= axtenergieskev[0] && eoutkev[i] <= axtenergieskev[numaxtenergies-1]) {
        arfgenlib::bisectionInterp(numaxtenergies, axtenergieskev, auxtrans, 
          eoutkev[i], newtrans);
        outauxtrans[i] = newtrans;
      }
    } // end loop over output grid energies

    delete rttrans;
    ahfits::close(fptrans);
    
  } // end if auxtransfile != NONE

  // Currently, the ARF/RSP generator does not use a spectral file.

  // Get coarse and find energy grids from input effective area files 
  // and the EA arrays from these files.
  //                   input          output              output          output
  AH_INFO(ahlog::LOW) << "Reading coarse ARF file " << coarsearffile << std::endl;
  arfgenlib::geteadata(coarsearffile, numebinscoarsefull, ecoarsecenfull, coarsearfeffareafull);

  numebinsfine = 0;
  //                   input        output        output    output
  AH_INFO(ahlog::LOW) << "Reading fine ARF file " << finearffile << std::endl;
  arfgenlib::geteadata(finearffile, numebinsfine, efinecen, finearfeffarea);

  AH_INFO(ahlog::LOW) << "Read coarse and fine ARFs" << std::endl;

  // Open QE map file (this part is just for cross-checking instrument
  // name).
  if (ahgen::strtoupper(par.m_qefile) != "NONE") {
    ahfits::open(par.m_actual_qefile, "QE", &fpqe);
    qemap_instrume = ahgen::strtoupper(ahfits::getKeyValStr(fpqe, "INSTRUME"));
    if (qemap_instrume != ahgen::strtoupper(par.m_instrume)) {
      AH_THROW_RUNTIME("MISMATCH between QE map INSTRUME keyword and INSTRUME parameter; exiting");
    }
    ahfits::close(fpqe);
  }

  // Do some other checks here?

  // Open the exposure map file.
  ahfits::open(par.m_emapfile, "OFFAXISHIST", &fpemap);
  numexpmaps = ahfits::getKeyValLLong(fpemap, "NUMEXMAP");
  numgrptheta = numexpmaps;
  emapnumoffaxis = ahfits::getKeyValLLong(fpemap, "NAXIS2");

  // Fill in the coordinate-related keywords.
  getCoordKeywords(fpemap, actimg);

  // Read in the subset of the histogram data that will be used.
  rtemap = new ahfits::Router(fpemap);
  // +++ 2016-01-21 RSH TRF difference - use bin bound, not offaxisval
  //rtemap->connectScalar(ahfits::e_READONLY, "OFFAXISVAL", l_offaxisval);
  rtemap->connectScalar(ahfits::e_READONLY, "OFFAXISLO", l_offaxislo);
  rtemap->connectScalar(ahfits::e_READONLY, "OFFAXISHI", l_offaxishi);
  rtemap->connectScalar(ahfits::e_READONLY, "FRACTION", l_fracexptime);
  rtemap->connectScalar(ahfits::e_READONLY, "TIMEINTERVAL", l_timeinterval);
  for (ahfits::firstRow(fpemap); ahfits::readOK(fpemap); ahfits::nextRow(fpemap)) {
    ahfits::readRow(fpemap);
    emapoffaxisval.push_back(0.5*(l_offaxislo + l_offaxishi));
    fracexptime.push_back(l_fracexptime);
    emaptimeintervals.push_back(l_timeinterval);
  }
  if (fracexptime.size() != static_cast<size_t>(numexpmaps)) {
    AH_THROW_RUNTIME("OFFAXISHIST should have NUMEXMAP rows, but does not");
  }

  initDouble1D(tfractionratio, emapnumoffaxis);
  initLong1D(numrtpairs, emapnumoffaxis);
  initLong1D(pairindexlo, emapnumoffaxis);
  initLong1D(pairindexhi, emapnumoffaxis);
  rtnumoffaxisfull = 0;

  for (long i=0; i<emapnumoffaxis; i++) {  // Loop over offaxis angle bins
    // Accumulate total exposure time.
    // +++ 2016-01-21 RSH emaptotexposure never used
    emaptotexposure += emaptimeintervals[i];
    // Ratio of expfraction to the mean expfraction (=1/numoffaxis).
    // This is essentially the weight of each exposure map, relative
    // to equal weighting for all.
    tfractionratio[i] = fracexptime[i]*emapnumoffaxis;
    // We start duplicating paris of theta, phi if the exposure fraction
    // is more than 1.5x mean.
    numrtpairs[i] = tfractionratio[i] + 0.5;
    if (numrtpairs[i] < 1) {
      numrtpairs[i] = 1;
    }
    pairindexlo[i] = rtnumoffaxisfull;
    pairindexhi[i] = rtnumoffaxisfull + numrtpairs[i] - 1;
    rtnumoffaxisfull = rtnumoffaxisfull + numrtpairs[i];
  }  // End loop over offaxis angle bins

  // We need to create an array of exposure fractions for off-axis angles that
  // contains only unique values of theta. This is because, regardless of what
  // expanded set of theta values was used to run the raytracing, the event file
  // will have rows grouped by the same theta and it is these groups of theta that
  // will be treated together. However, note that the actual off-axis angles in the
  // exposure file are not used directly because the aharfgen script may have run
  // the raytracing for another source in the field. So we get the actual thetas
  // from the raytracing event file but here we just need the corresponding exposure
  // fractions which are the same regardless of the actual values of theta.

  igrp = 0;
  initLong1D(igrpstart, numgrptheta);
  initLong1D(igrpend, numgrptheta);
  igrpstart[0] = 0;
  // +++ 2016-01-27 RSH TRF error - was igrpend[numgrptheta]
  igrpend[numgrptheta-1] = emapnumoffaxis - 1;
  for (long i=0; i<emapnumoffaxis; i++) {
    if (i > 0 && emapoffaxisval[i] != emapoffaxisval[i-1]) {
      igrp++;
      igrpstart[igrp] = i;
      igrpend[igrp-1] = i - 1;
    }
  }
  rtnumoffaxis = numgrptheta;
  //if (rtnumoffaxis != numgrptheta) {
  //  AH_INFO(ahlog::HIGH) << "Number of unique off-axis angles in"
  //    << " exposure map does not match keyword; proceeding anyway" << std::endl;
  //}
  initDouble1D(grpexpfrac, rtnumoffaxis);
  for (long i=0; i<rtnumoffaxis; i++) {
  //  for (long j=igrpstart[i]; j<=igrpend[i]; j++) {
  //    grpexpfrac[i]=grpexpfrac[i]+fracexptime[j];
    grpexpfrac[i]=fracexptime[i];
  //  }
  }

  rawmaprefx = ahfits::getKeyValDbl(fpemap, "CRVAL1");
  rawmaprefxpixel = ahfits::getKeyValDbl(fpemap, "CRPIX1");
  rawmapdeltax = ahfits::getKeyValDbl(fpemap, "CDELT1");

  rawmaprefy = ahfits::getKeyValDbl(fpemap, "CRVAL2");
  rawmaprefypixel = ahfits::getKeyValDbl(fpemap, "CRPIX2");
  rawmapdeltay = ahfits::getKeyValDbl(fpemap, "CDELT2");

  xmmperpixel = ahfits::getKeyValDbl(fpemap, "RAW_XSCL");  // Parameter of this function
  ymmperpixel = ahfits::getKeyValDbl(fpemap, "RAW_YSCL");  // Parameter of this function

  // OPTAXISX, OPTAXISY are the (invariant) position in DET 
  // coordinates of the optical axis (ultimately from the teldef file).

  optaxisx = ahfits::getKeyValDbl(fpemap, "OPTAXISX");
  optaxisy = ahfits::getKeyValDbl(fpemap, "OPTAXISY");

  optaxisXstdmm = xmmperpixel*optaxisx;  // Parameter of this function
  optaxisYstdmm = ymmperpixel*optaxisy;  // Parameter of this function

  numrawx = ahfits::getKeyValLLong(fpemap, "RAW_XSIZ");  // Parameter of this function
  numrawy = numrawx;
  //numrawy = ahfits::getKeyValLLong(fpemap, "RAW_YSIZ");  // Parameter of this function
  numactx = ahfits::getKeyValLLong(fpemap, "ACT_XSIZ");  // Parameter of this function
  numacty = numactx;
  //numacty = ahfits::getKeyValLLong(fpemap, "ACT_YSIZ");  // Parameter of this function

  focallength = ahfits::getKeyValDbl(fpemap, "FOCALLEN");
  fpmm2arcmin = RAD2ARCMIN*std::atan(1.0/focallength);  // Parameter of this function
  
  // Lower and upper bounds of the rawx and rawy pixels:  size the arrays
  // according to the keywords just retrieved.
  initDouble1D(rawxlo, numrawx);
  initDouble1D(rawxhi, numrawx);
  initDouble1D(rawx, numrawx);
  initDouble1D(rawylo, numrawy);
  initDouble1D(rawyhi, numrawy);
  initDouble1D(rawy, numrawy);

  // Now calculate the boundaries.
  for (long i=0; i<numrawx; i++) {
    rawxlo[i] = (i + 0.5 - rawmaprefxpixel)*rawmapdeltax + rawmaprefx;
    rawxhi[i] = (i + 1.5 - rawmaprefxpixel)*rawmapdeltax + rawmaprefx;
    rawx[i] = 0.5*(rawxlo[i] + rawxhi[i]);
  }
  for (long i=0; i<numrawx; i++) {
    rawylo[i] = (i + 0.5 - rawmaprefypixel)*rawmapdeltay + rawmaprefy;
    rawyhi[i] = (i + 1.5 - rawmaprefypixel)*rawmapdeltay + rawmaprefy;
    rawy[i] = 0.5*(rawylo[i] + rawyhi[i]);
  }

  // Lower and upper bounds of the detector
  rawxminmm = rawxlo[0]*xmmperpixel;
  rawxmaxmm = rawxhi[numrawx-1]*xmmperpixel;
  rawyminmm = rawylo[0]*ymmperpixel;
  rawymaxmm = rawyhi[numrawy-1]*ymmperpixel;
  par.m_baffle.m_centerxmm = par.m_baffle.m_centerx*xmmperpixel;
  par.m_baffle.m_centerymm = par.m_baffle.m_centery*ymmperpixel;

  // Set up parameters for ACT images and initialize the ACT image arrays.
  if (par.m_doflatfield) {
    double midactx = 0.5 + (numactx/2.0);  // Temporary variables
    double midacty = 0.5 + (numacty/2.0);
    actimg.m_crval1 = midactx;   // actimg is a parameter of this function
    actimg.m_crpix1 = midactx;
    actimg.m_cdelt1 = 1;
    actimg.m_crval2 = midacty;
    actimg.m_crpix2 = midacty;
    actimg.m_cdelt2 = 1;
    actimg.m_cunit1 = "";
    actimg.m_cunit2 = "";

    // Calculate the pixel boundaries.
    Double1D actxhi, actyhi, actxcen, actycen;
    for (long i=0; i<numactx; i++) {
      actxlo.push_back((i + 0.5 - actimg.m_crpix1)*actimg.m_cdelt1 + actimg.m_crval1);
      actxhi.push_back((i + 1.5 - actimg.m_crpix1)*actimg.m_cdelt1 + actimg.m_crval1);
      actxcen.push_back(0.5*(actxlo[i] + actxhi[i]));
    }
    for (long i=0; i<numacty; i++) {
      actylo.push_back((i + 0.5 - actimg.m_crpix2)*actimg.m_cdelt2 + actimg.m_crval2);
      actyhi.push_back((i + 1.5 - actimg.m_crpix2)*actimg.m_cdelt2 + actimg.m_crval2);
      actycen.push_back(0.5*(actylo[i] + actyhi[i]));
    }

  }

  AH_INFO(ahlog::LOW) << "Read pixel format information" << std::endl;

  // The GTIs will be read from each GTI extension and merged into combined
  // arrays of start and stop times, along with an identifier array equal in 
  // size, the integer being equal to the GTI group number (=extension 
  // number) minus one = offaxis angle index/pointer.  The size of the
  // merged array, however, will not be determined until all of the GTIs
  // have been read, but we will use the variable totmergegti here with
  // the understanding that it will not be determined until later and
  // the arrays have to be built up after reading each GTI.

  for (long i=0; i<numexpmaps; i++) {
    ss.str("");
    ss << std::setw(3) << std::setfill('0') << i+1;
    gtiextname = "GTIEXP" + ss.str();
    ahfits::move(fpemap, gtiextname);
    rtemap->clearConnections();
    rtemap->connectScalar(ahfits::e_READONLY, "START", l_start);
    rtemap->connectScalar(ahfits::e_READONLY, "STOP", l_stop);
    for (ahfits::firstRow(fpemap); ahfits::readOK(fpemap); ahfits::nextRow(fpemap)) {
      ahfits::readRow(fpemap);
      gtistart.push_back(l_start);
      gtistop.push_back(l_stop);
      gtioffaxisindex.push_back(i);
      totnumgti++;
    }
  }

  AH_INFO(ahlog::LOW) << "Read GTIEXPnnn extensions" << std::endl;

  delete rtemap;  // Deallocate column router for exposure map file
  // ahfits::close(fpemap);  // Close exposure map file

  // Read the CAMS delta attitude file and resample the data if
  // requested.

  if (ahgen::strtoupper(par.m_dattfile) != "NONE") {

    // Temporary variables to hold column data
    double l_time=0.0, l_rawx=0.0, l_rawy=0.0;

    ahfits::open(par.m_dattfile, "", &fpdatt);
    ahfits::firstHDU(fpdatt, ahfits::e_BINARY_TBL);
    ahmission::checkEmptyTable(fpdatt, par.m_dattfile);
    numcamsfiletimes = ahfits::getKeyValLLong(fpdatt, "NAXIS2");

    AH_INFO(ahlog::LOW) << "numcamsfiletimes=" << numcamsfiletimes << std::endl;

    rtdatt = new ahfits::Router(fpdatt);
    rtdatt->connectScalar(ahfits::e_READONLY, "TIME", l_time);
    rtdatt->connectScalar(ahfits::e_READONLY, "RAWX_FLOAT", l_rawx);
    rtdatt->connectScalar(ahfits::e_READONLY, "RAWY_FLOAT", l_rawy);
    for (ahfits::firstRow(fpdatt); ahfits::readOK(fpdatt); ahfits::nextRow(fpdatt)) {
      ahfits::readRow(fpdatt);
      camsfiletimes.push_back(l_time);
      opticrawxfile.push_back(l_rawx);
      opticrawyfile.push_back(l_rawy);
    }

    // Process sampling if necessary
    if (par.m_sampling == 1) {
      numdeltatt = numcamsfiletimes;
      for (long i=0; i<numcamsfiletimes; i++) {
        camstimes.push_back(camsfiletimes[i]);
        opticrawx.push_back(opticrawxfile[i]);
        opticrawy.push_back(opticrawyfile[i]);
        opticrawxmm.push_back(opticrawxfile[i]*xmmperpixel);
        opticrawymm.push_back(opticrawyfile[i]*ymmperpixel);
      }
    } else {
      long tindx=0;  // Temporary variable for sampled CAMS time
      numdeltatt = numcamsfiletimes/par.m_sampling;
      for (long i=0; i<numdeltatt; i++) {
        tindx = i*par.m_sampling;
        camstimes.push_back(camsfiletimes[tindx]);
        opticrawx.push_back(opticrawxfile[tindx]);
        opticrawy.push_back(opticrawyfile[tindx]);
        opticrawxmm.push_back(opticrawxfile[tindx]*xmmperpixel);
        opticrawymm.push_back(opticrawyfile[tindx]*ymmperpixel);
      }
    }

    delete rtdatt;
    ahfits::close(fpdatt);

  }  // end if dattfile

  // Read the CAMS optical bench correction factors vs. time
  // and resample the data if requested.

  if (ahgen::strtoupper(par.m_filtoffsetfile) != "NONE") {

    // NOTE: it is assumed that all the quantities in this file have the
    // exact same time stamps as in the other file, and the code does not
    // currently check for this.

    // Temporary variables for column data
    double l_deltarawx=0.0, l_deltarawy=0.0, l_deltasin=0.0, l_deltacos=0.0;
    Double1D deltarawxfile, deltarawyfile, deltasinangfile, deltacosangfile;

    // Temporary variable for filtoffsetfile FITS pointer
    ahfits::FilePtr fpfiltoff;

    // Temporary variable for column router
    ahfits::Router* rtfiltoff;

    ahfits::open(par.m_filtoffsetfile, "", &fpfiltoff);
    ahfits::firstHDU(fpfiltoff, ahfits::e_BINARY_TBL);
    if (numcamsfiletimes != ahfits::numRows(fpfiltoff)) {
      AH_THROW_RUNTIME("CAMS delta attitude file and offset file have different numbers of rows");
    }
    rtfiltoff = new ahfits::Router(fpfiltoff);
    rtfiltoff->connectScalar(ahfits::e_READONLY, "DELTARAWX", l_deltarawx);
    rtfiltoff->connectScalar(ahfits::e_READONLY, "DELTARAWY", l_deltarawy);
    rtfiltoff->connectScalar(ahfits::e_READONLY, "SINANGLE", l_deltasin);
    rtfiltoff->connectScalar(ahfits::e_READONLY, "COSANGLE", l_deltacos);

    for (ahfits::firstRow(fpfiltoff); ahfits::readOK(fpfiltoff); ahfits::nextRow(fpfiltoff)) {
      ahfits::readRow(fpfiltoff);
      deltarawxfile.push_back(l_deltarawx);
      deltarawyfile.push_back(l_deltarawy);
      deltasinangfile.push_back(l_deltasin);
      deltacosangfile.push_back(l_deltacos);
    }

    if (par.m_sampling == 1) {
      numdeltatt = numcamsfiletimes;
      for (long i=0; i<numdeltatt; i++) {
        deltarawx.push_back(deltarawxfile[i]);
        deltarawy.push_back(deltarawyfile[i]);
        deltasinangle.push_back(deltasinangfile[i]);
        deltacosangle.push_back(deltacosangfile[i]);
      }
    } else {
      long tindx=0;  // Temporary variable for sampled CAMS time
      numdeltatt = numcamsfiletimes/par.m_sampling;
      for (long i=0; i<numdeltatt; i++) {
        tindx = i*par.m_sampling;
        deltarawx.push_back(deltarawxfile[tindx]);
        deltarawy.push_back(deltarawyfile[tindx]);
        deltasinangle.push_back(deltasinangfile[tindx]);
        deltacosangle.push_back(deltacosangfile[tindx]);
      }
    }

    delete rtfiltoff;
    ahfits::close(fpfiltoff);

    initDouble1D(deltarawxmm, numdeltatt);
    initDouble1D(deltarawymm, numdeltatt);
    for (long i=0; i<numdeltatt; i++){
      deltarawxmm[i] = deltarawx[i]*xmmperpixel;
      deltarawymm[i] = deltarawy[i]*ymmperpixel;
    }
  }

  if (par.m_dorsp) {
        
    // Read the raytracing event file 2nd extension and create an array of
    // injected photon number that will be used later for normalization to
    // calculate the effective area.  Also read the keyword for the absolute
    // geometric area.

    // Temporary variables
    long evttotnphot = 0;    // TOTNPHOT keyword
    Double1D rtenergygrid;   // Raytrace energies
    double l_energy = 0.0;   // Local variable for energy column data
    ahfits::Router* rtxrt = 0;  // Column data router
    long jesublo=0, jesubhi=0;  // Subscripts marking energy subranges
    numebinscoarse = 0;      // Count of coarse energy bins
    Double1D cosrtoffaxis;   // Vector of offaxis cosines
    double l_initialtheta=0.0;  // Local variable for raytracing theta
    double l_initialazimdir=0.0;  // Local variable for raytracing phi
    long l_numrowphotons=0;  // Local variable for raytracing number of photons 
    long fullrowindex1=0, fullrowindex2=0;  // Loop limits used in saving offaxis angles
    long evtnumoffaxis = 0;  // Number of OFFAXIS intervals in raytracing event file
    Double2D rtnormsfull;    // Normalizations for full set of raytracing bins
    Double1D rtoffaxisfull;  // Full set of offaxis raytracing bins
    Double1D rtazimfull;     // Full set of offaxis raytracing bins
    long grpj = 0;           // Group index
    Long1D uniquethetastartindex;  // Indexes for unique theta array
    Long1D uniquethetaendindex;

    ahfits::open(par.m_xrtevtfile, "", &fpxrt);
    ahfits::move(fpxrt, 2);

    geomarea = ahfits::getKeyValDbl(fpxrt, "GEOMAREA");
    rtnumrows = ahfits::getKeyValLLong(fpxrt, "NAXIS2");  // Parameter of this function
    evttotnphot = ahfits::getKeyValLLong(fpxrt, "TOTNPHOT");

    ahfits::move(fpxrt, 3);
    ahmission::checkEmptyTable(fpxrt, par.m_xrtevtfile);
    rtnumens = ahfits::getKeyValLLong(fpxrt, "NUMENRG");
    rtxrt = new ahfits::Router(fpxrt);
    rtxrt->connectScalar(ahfits::e_READONLY, "ENERGY", l_energy);
    for (ahfits::firstRow(fpxrt); ahfits::readOK(fpxrt); ahfits::nextRow(fpxrt)) {
      ahfits::readRow(fpxrt);
      rtenergygrid.push_back(l_energy);
    }

    // Match the energies in the raytracing event file to the energies in
    // the coarse grid effective area file and hence create the coarse-grid
    // effective area and energy sub-arrays.

    jesublo = 0;
    jesubhi = numebinscoarsefull-1;
    for (long ie=0; ie<numebinscoarsefull-1; ie++) {
      if (rtenergygrid[0] >= ecoarsecenfull[ie] &&
        rtenergygrid[0] <= ecoarsecenfull[ie+1]) {
        jesublo = ie;
      } // end of if-block searching for high energy bin
      if (rtenergygrid[rtnumens-1] >= ecoarsecenfull[ie] &&
        rtenergygrid[rtnumens-1] <= ecoarsecenfull[ie+1]) {
        jesubhi = ie;
      } // end of if-block searching for high energy bin
    } // end loop over full energy grid

    numebinscoarse = jesubhi - jesublo + 1;
    if (numebinscoarse < 2) {
      AH_ERR << "Number of coarse energy bins in raytracing file = "
        << numebinscoarse << std::endl;
      AH_THROW_RUNTIME("At least 2 are required; a wider raytracing energy range is needed; exiting") ;
    }

    for (long j=0; j<numebinscoarse; j++) {
      ecoarsecen.push_back(ecoarsecenfull[jesublo+j]);
      coarsearfeffarea.push_back(coarsearfeffareafull[jesublo+j]);
    }

    // Establish minimum and maximum energy in raytracing event file.

    rt_min_e = ecoarsecen[0];                 // Parameter of this function
    rt_max_e = ecoarsecen[numebinscoarse-1];  // Parameter of this function

    // Now test the number of theta values in the raytracing event file 
    // (which may be repeated) with the number of theta values deduced
    // earlier from the exposure map expanded theta, phi set.

    evtnumoffaxis = getKeyValLLong(fpxrt, "NOFFAXIS");


    if (rtnumoffaxisfull != evtnumoffaxis) {
      AH_THROW_RUNTIME(std::string("Number of off-axis angles in raytracing event file does not match\n")
        + std::string("number of off-axis angles in exposure map file; exiting"));
    }

    // (Note: the above includes only basic check that the right raytracing
    // event file has been supplied with the user chooses the option of
    // using an existing event file instead of making a new one.)

    initDouble2D(rtnorms, rtnumens, rtnumoffaxis);
    // Second normalization array for expanded theta, phi set.
    // +++ 2016-01-21 RSH rtnumoffaxis in TRF, but later loop limit is rtnumoffaxisfull
    // +++ 2016-01-21 RSH rtnormsfull is computed but never used
    initDouble2D(rtnormsfull, rtnumens, rtnumoffaxisfull);
    initDouble1D(rtoffaxis, rtnumoffaxis);     // Parameter of this function
    initDouble1D(cosrtoffaxis, rtnumoffaxis);  // cosine of off-axis angles
    initDouble1D(rtoffaxisfull, rtnumoffaxisfull);
    initDouble1D(rtazimfull, rtnumoffaxisfull);

    // Off-axis angle for each input photon (for extended sources, it is
    // value supplied for the raytracing input, not the actual off-axis angle
    // of each photon leaving the source) - in argument list
    initDouble1D(inputfulloffaxis, evttotnphot);
    initLong1D(inputfulloffaxisindex, evttotnphot);

    // The varialbe with "grp" refer to the array of unique off-axis angles
    // (i.e., may be less than the number of rows in the offaxis histogram).
    initDouble1D(inputgrpoffaxis, evttotnphot);
    initLong1D(inputgrpoffaxisindex, evttotnphot);

    
    // Effective geometrical area after taking into account cosine(offaxis angle)
    // factors and fractional exposure times (in argument list)

    rtxrt->clearConnections();
    rtxrt->connectScalar(ahfits::e_READONLY, "INITIALTHETA", l_initialtheta);
    rtxrt->connectScalar(ahfits::e_READONLY, "INITIALAZIMDIR", l_initialazimdir);
    rtxrt->connectScalar(ahfits::e_READONLY, "NUMPHOTONS", l_numrowphotons);

    fullrowindex1 = 0;
    fullrowindex2 = 0;
    grpj = 0;
    initLong1D(uniquethetastartindex, numgrptheta);
    initLong1D(uniquethetaendindex, numgrptheta);
    uniquethetastartindex[0] = 0;
    uniquethetaendindex[numgrptheta-1] = evttotnphot - 1;

    for (long j=0; j<rtnumoffaxisfull; j++) {
      for (long i=0; i<rtnumens; i++) {
        ahfits::gotoRow(fpxrt, j*rtnumens + i + 1);  // Row nums are 1-based
        if (!ahfits::readOK(fpxrt)) {
          AH_THROW_RUNTIME("Attempt to read past end of raytracing event file; exiting");
        }
        ahfits::readRow(fpxrt);
        fullrowindex2 = fullrowindex1 + l_numrowphotons - 1;
        rtnormsfull[i][j] = l_numrowphotons;
        if (i == 0) {
          rtoffaxisfull[j] = l_initialtheta;
          rtazimfull[j] = l_initialazimdir;
          // Look for change in theta and azimuth for grouping.
          if (j > 0) {
            AH_DEBUG << "j=" << j << " rtoffaxisfull=" << rtoffaxisfull[j] << " " << rtoffaxisfull[j-1]
              << " rtazimfull=" << rtazimfull[j] << " " << rtazimfull[j-1] << std::endl;
	    if (rtoffaxisfull[j] != rtoffaxisfull[j-1]
	      || rtazimfull[j] != rtazimfull[j-1]) {
	      grpj++;
	      uniquethetastartindex[grpj] = fullrowindex1;
              uniquethetaendindex[grpj-1] = fullrowindex1 - 1;
	      rtoffaxis[grpj] = rtoffaxisfull[j];
	    } // End if theta or phi change
          } else if (j == 0) {
            rtoffaxis[0] = rtoffaxisfull[0];
          } // End if j > 0
        } // End if i == 0
        for (long k=fullrowindex1; k<=fullrowindex2; k++) {
          inputfulloffaxisindex[k] = j;  // Parameter of this function
          inputfulloffaxis[k] = rtoffaxisfull[j];
        } // End of k loop
        fullrowindex1 = fullrowindex2 + 1;
        rtnorms[i][grpj] += l_numrowphotons;
      } // End of i loop
    } // End of j loop

    uniquethetaendindex[grpj] = fullrowindex2;

    AH_DEBUG << "k uniquethetastartindex[k] uniquethetaendindex[k]" << std::endl;
    for (long k=0; k<numexpmaps-1; k++) {
      AH_DEBUG << k << "  " << uniquethetastartindex[k] 
        << "                   " << uniquethetaendindex[k] << std::endl;
    }

    // Set up "grp" cos(theta) and pointers.
    // Effective geometrical area after taking into account cos(offaxis angle)
    // factores and fractional exposure times.
    effgeomarea = 0.0;  
    for (long j=0; j<rtnumoffaxis; j++) {
      cosrtoffaxis[j] = std::cos(ARCMIN2RAD*rtoffaxis[j]);
      effgeomarea += grpexpfrac[j]*geomarea*cosrtoffaxis[j];
      for (long k=uniquethetastartindex[j]; k<=uniquethetaendindex[j]; k++) {
        inputgrpoffaxisindex[k] = j;
        inputgrpoffaxis[k] = rtoffaxis[j];
      } // End of k loop
    } // End of j loop

    if (inputfulloffaxis.size() != static_cast<size_t>(evttotnphot) 
      || inputfulloffaxisindex.size() != static_cast<size_t>(evttotnphot)) {
      AH_THROW_RUNTIME("Inconsistency in number of photons in raytracing event file");
    }

    delete rtxrt;

  } // end if dorsp

  // If the RSP is being calculated, then the ecoarsecen energy
  // array has already been created by processing the raytracing
  // event file.  Otherwise, however, ecoarsecen needs to
  // be set up here.
  if (!par.m_dorsp && par.m_doflatfield) {
    // In this case, the raytracing energy range does not come into play,
    // so the user-selected flatfield energy range (ERANGE) needs to be used 
    // to make the array ecoarsecen.

    // Temporary variables for energy indexes
    int e1index=0, e2index=0, ff_elo_index=0, ff_ehi_index=0;

    // Determine the energy indices for summing the QE.
    AH_INFO(ahlog::LOW) << "Searching for lower flatfield energy bound" << std::endl;
    
    arfgenlib::bisectionLocate(ecoarsecenfull, numebinscoarsefull, par.m_erange.m_eminimg, e1index, e2index);
    ff_elo_index = e1index;

    AH_INFO(ahlog::LOW) << "Located lower flatfield energy bound=" << ff_elo_index << std::endl;
    AH_INFO(ahlog::LOW) << "Searching for upper flatfield energy bound" << std::endl;

    arfgenlib::bisectionLocate(ecoarsecenfull, numebinscoarsefull, par.m_erange.m_emaximg, e1index, e2index);
    ff_ehi_index = e1index;
    
    AH_INFO(ahlog::LOW) << "Located upper flatfield energy bound=" << ff_ehi_index << std::endl;

    numebinscoarse = ff_ehi_index - ff_elo_index + 1;
    for (long i=0; i<numebinscoarse; i++) {
      ecoarsecen.push_back(ecoarsecenfull[i + ff_elo_index]);
    }
  }

  // Call routine to read the QE map data and store in a single array
  // for all energies the following:  RAWX, RAWY, layer.  The QE reading
  // routine will remap and return the data array mapped onto a
  // new energy grid (not the one in the file), ecoarsecen, because
  // these are the energies for which the raytracing event file was made.

  // +++ 2016-01-28 RSH When doing only the flat field, this
  //  may have the wrong range of ecoarscen.
  //                       input                input    
  //input    input    input           input       output
  if (ahgen::strtoupper(par.m_qefile) != "NONE") {
    arfgenlib::readhxiqedata(par.m_actual_qefile, par.m_numlayers, 
      numrawx, numrawy, numebinscoarse, ecoarsecen, qemap);
    AH_INFO(ahlog::LOW) << "Read QE data" << std::endl;
  }

  if (par.m_doflatfield) {

    AH_INFO(ahlog::LOW) << "Setting up for ARF energy searches" << std::endl;

    // Temporary variables for energy indexes
    int e1index=0, e2index=0, qe_elo_index=0, qe_ehi_index=0;
    
    // Temporary variable for fine-grid effective area file = baffle file
    ahfits::FilePtr fponf = 0;

    // Temporary variables for baffle image size
    long nx=0, ny=0;

    // Temporary variable for number of QE energies
    double rnumqeenergies = 0.0;

    // Make a QE map that is averaged over the requested energy range and
    // summed over layers: it will be a single image based on RAW coordinates.

    initDouble2D(avgqemap, numrawx, numrawy);  // Parameter of this function

    // Determine the energy indices for summing the QE.  This needs to be
    // done if the RSP was done, because ecoarsecen includes the raytracing
    // energy range.  On the other hand, if RSP is not done, then 
    // ecoarsecen is preselected to have the flatfield energy range.

    if (par.m_dorsp) {
      AH_INFO(ahlog::LOW) << "Searching for lower energy bound" << std::endl;
      
      arfgenlib::bisectionLocate(ecoarsecen, numebinscoarse, par.m_erange.m_eminimg, e1index, e2index);
      qe_elo_index = e1index;

      AH_INFO(ahlog::LOW) << "Located lower energy bound=" << qe_elo_index << std::endl;
      AH_INFO(ahlog::LOW) << "Searching for upper energy bound" << std::endl;

      arfgenlib::bisectionLocate(ecoarsecen, numebinscoarse, par.m_erange.m_emaximg, e1index, e2index);
      qe_ehi_index = e1index;
    
      AH_INFO(ahlog::LOW) << "Located upper energy bound=" << qe_ehi_index << std::endl;
    } else {
      qe_elo_index = 0;
      qe_ehi_index = numebinscoarse-1;
    }

    if (ahgen::strtoupper(par.m_qefile) == "NONE") {
      for (long i=0; i<numrawx; i++) {
        for (long j=0; j<numrawy; j++) {
          avgqemap[i][j] = 1.0;
        }
      }
    } else {
      rnumqeenergies = qe_ehi_index - qe_elo_index + 1;
      for (long i=0; i<numrawx; i++) {
        for (long j=0; j<numrawy; j++) {
          avgqemap[i][j] = 0.0;
          for (long k=0; k<par.m_numlayers; k++) {
            for (long m=qe_elo_index; m<=qe_ehi_index; m++) {
              avgqemap[i][j] += qemap[m][i][j][k];
            }
          }
          avgqemap[i][j] /= rnumqeenergies;
        }
      }
    }

    AH_INFO(ahlog::LOW) << "Computed avgqemap" << std::endl;

    if (ahgen::strtoupper(par.m_vigfile) != "NONE") {
      // Call routine to read the vignetting file, returning the coefficient required
      // for using the analytic formulas.
      arfgenlib::readahvignetfile(par.m_actual_vigfile, numcoeff, vignetcoeff);

      AH_INFO(ahlog::LOW) << "Read vignetting file" << std::endl;
    }

    // Read the HXI baffle file = primary HDU of fine-grid on-axis ARF file.
    ahfits::open(par.m_actual_onaxisffile, "", &fponf);
    ahfits::move(fponf, 1);  // Force move to primary
    ahfits::readImage(fponf, hxibaffle, nx, ny);  // Parameter of this function
    if (nx != numrawx || ny != numrawy) {
      ss.str("");
      ss << "HXI baffle image size " << nx << " X " << ny << " not the expected"
        " RAW image size " << numrawx << " X " << numrawy;
      AH_THROW_RUNTIME(ss.str());
    }
    ahfits::close(fponf);

    AH_INFO(ahlog::LOW) << "Read baffle image" << std::endl;
  } // End if doflatfield

  // Loop over all the delta-attitude time bins and assign an off-axis angle
  // index to each one, based on the GTIs.  Also, count the number of CAMS
  // time bins for each off-axis angle.
  initLong1D(offaxisid, numdeltatt);   // Parameter of this function
  initLong1D(gtipointer, numdeltatt);  // Parameter of this function
  for (long i=0; i<numdeltatt; i++) {
    gtipointer[i] = -1;
    offaxisid[i] = -1;
  }
  initDouble1D(tbinexposure, numdeltatt);  // Parameter of this function
  initDouble1D(deltattbinsperoffaxis, numexpmaps);  // Parameter of this function
  if (ahgen::strtoupper(par.m_dattfile) == "NONE") {
    // Handle case of no CAMS file.  If there is one, it's handled above.
    numdeltatt = totnumgti;
    initDouble1D(camstimes, numdeltatt);
    initDouble1D(deltarawxmm, numdeltatt);
    initDouble1D(deltarawymm, numdeltatt);
    initDouble1D(opticrawxmm, numdeltatt);
    initDouble1D(opticrawymm, numdeltatt);
    initDouble1D(deltarawx, numdeltatt);
    initDouble1D(deltarawy, numdeltatt);
    initDouble1D(deltasinangle, numdeltatt);
    initDouble1D(deltacosangle, numdeltatt);
    for (long ii=0; ii<numdeltatt; ii++) {
      camstimes[ii] = 0.5*(gtistart[ii] + gtistop[ii]);
      deltacosangle[ii] = 1.0;
      opticrawxmm[ii] = optaxisXstdmm;
      opticrawymm[ii] = optaxisYstdmm;
    }
  }

  totalexposure = 0.0;  // Parameter of this function
  for (long i=0; i<numdeltatt; i++) {
    for (long j=0; j<totnumgti; j++) {
      if (i == 0) {
        totalexposure += gtistop[j]-gtistart[j];
      }
      if (camstimes[i] >= gtistart[j] && camstimes[i] <= gtistop[j]) {
        double txlow=0.0, txhigh=0.0;
        offaxisid[i] = gtioffaxisindex[j];
        deltattbinsperoffaxis[gtioffaxisindex[j]]++;
        gtipointer[i] = j;

        // If the time point is not the first or last then calculat the
        // associated exposure time by examining the previous and next time bin.
        if (i > 0 && i < numdeltatt-1 && numdeltatt > 2) {
          if (camstimes[i-1] < gtistart[j]) {
            txlow = camstimes[i] - gtistart[j];
          } else {
            txlow = (camstimes[i] - camstimes[i-1])/2.0;
          }
          if (camstimes[i+1] > gtistop[j]) {
            //txhigh = camstimes[i+1] - gtistop[j];
            txhigh = gtistop[j] - camstimes[i];
          } else {
            txhigh = (camstimes[i+1] - camstimes[i])/2.0;
          }
          tbinexposure[i] = txlow + txhigh;
        } else if (i == 0 && j == 0) {
          txlow = (camstimes[0] - gtistart[0]);
          if (camstimes[1] > gtistop[0]) {
            txhigh = gtistop[0] - camstimes[0];
          } else {
            txhigh = 0.5*(camstimes[1] - camstimes[0]);
          }
          tbinexposure[0] = txlow + txhigh;
        } else if (i == numdeltatt-1 && j == totnumgti-1) {
          txhigh = (gtistop[totnumgti-1] - camstimes[numdeltatt-1]); 
          if (camstimes[numdeltatt-2] < gtistart[totnumgti-1]) {
            txlow = camstimes[numdeltatt-1] - gtistart[totnumgti-1];
          } else {
            txlow = 0.5*(camstimes[numdeltatt-1] - camstimes[numdeltatt-2]);
          }
          tbinexposure[numdeltatt-1] = txlow + txhigh;
        // } else {
          // AH_THROW_LOGIC("Should not happen: numdeltatt == 2 but i neither 0 nor 1");
        } // end if, checking time bin and filling in exposure time per bin
      } // end if time bin inside GTI
    } // end loop over GTI (j)
  } // end loop over CAMS times (i)

  if (par.m_dorsp) {  // +++ 2016-01-04 RSH Added bracketing if-block.
    
    // Calculate the total number of input pseudo-photons per energy
    // for normalization of the effective area later.
    initDouble1D(inputphotonsperenergy, rtnumens);  // Parameter of this function
    for (long i=0; i<rtnumens; i++) {
      for (long j=0; j<numexpmaps; j++) { // +++ 2016-01-07 RSH was rtnumoffaxis
        inputphotonsperenergy[i] += rtnorms[i][j]*deltattbinsperoffaxis[j];
      }
    }
  }
  
  // Determine whether the input parameter REGIONFILES is the name
  // of a file containing a list of files, or whether it is a list
  // of literal filenames.

  // +++ 2015-12-15 RSH Move to getpar?
  if (par.m_doregion) {
    AH_DEBUG << "About to process regions" << std::endl;
    // Temporary variables to set up C routine calling sequence
    char* reg_char = const_cast<char*>(par.m_regionfile.c_str());
    char** items = 0;
    int nitems = 0;
    int status = 0;
    int trim = 1;      // Trim spaces
    int skip = 1;      // Exclude empty items
    char delim = ',';  // Delimiter is comma
    int guard = 0;     // Do not protect against commas in parantheses
    items = expand_item_list(reg_char, &nitems, delim, trim, skip, guard, &status);
    if (status != 0) {
      AH_THROW_RUNTIME(std::string("Invalid format found for REGIONFILE parameter\n")
        + std::string("while parsing possible comma-separated list; exiting"));
    }
    if (nitems == 1) {
      ahfits::expandFileList(par.m_regionfile, regionfilenames);
    } else {
      for (int ii=0; ii<nitems; ii++) {
        regionfilenames.push_back(std::string(items[ii]));
      }
    }
  }
  numregions = regionfilenames.size();

  if (numregions > 0 && numregions != emapnumoffaxis) {
    AH_ERR << "Number of region files = " << numregions << std::endl;
    AH_ERR << "Number of offaxis bins = " << emapnumoffaxis << std::endl;
    AH_THROW_RUNTIME("Number of region files must equal number of offaxis bins.  Exiting.");
  }
  
  // (Note: region files that are input to this routine should already
  // have been converted to telescope coordinates.)

  // Read the region files and set up an array of structures for 
  // each region.

  arfgenlib::setupregionstruct(numregions, regionfilenames, xrtregions);
  // xrtregions is parameter of this function.

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

  AH_DEBUG << "Exiting initialize" << std::endl;
}

// ****************************************************************************


void doWork(Par & par, bool & insufficientphotons,
  long numeoutbins, long numrmfinputbins,
  long numrmfchan, Double3D & inputrmfs,
  Double2D & rspmatrix, Double1D & outauxtrans, Double1D & arfeffarea,
  long numebinscoarse, Double1D & ecoarsecen, Double1D & coarsearfeffarea,
  long numebinsfine, Double1D & efinecen, Double1D & finearfeffarea, 
  double xmmperpixel, double ymmperpixel, double fpmm2arcmin,
  double optaxisXstdmm, double optaxisYstdmm,
  double rawxminmm, double rawxmaxmm, double rawyminmm, double rawymaxmm,
  long numrawx, long numrawy, long numactx, long numacty,
  Double1D & rawx, Double1D & rawy,
  Double1D & actxlo, Double1D & actylo,
  Double4D & qemap, Double2D & avgqemap,
  int numcoeff, double ** vignetcoeff, Double2D & hxibaffle,
  Double1D & opticrawx, Double1D & opticrawy,
  Double1D & opticrawxmm, Double1D & opticrawymm, long numdeltatt,
  Double1D & deltarawx, Double1D & deltarawy, 
  Double1D & deltarawxmm, Double1D & deltarawymm, 
  Double1D & deltasinangle, Double1D & deltacosangle,
  long rtnumrows, double rt_min_e, double rt_max_e,
  long rtnumoffaxis, Double1D & rtoffaxis, double effgeomarea,
  Double1D & inputgrpoffaxis, Long1D & inputgrpoffaxisindex,
  Double1D & deltattbinsperoffaxis,
  Double1D & tbinexposure, double & totalexposure,
  Long1D & offaxisid, Long1D & gtipointer, Double1D & inputphotonsperenergy,
  Double1D & eoutkev, RegionArray & xrtregions, ahfits::FilePtr fpemap,
  ahfits::FilePtr fpxrt, CRKeywords & actimg, Double2D & outimg) {

  AH_DEBUG << "Entering doWork" << std::endl;

  if (par.m_doflatfield) {

    AH_INFO(ahlog::LOW) << "Computing flat field image" << std::endl;

    // Flag for efficiency vs. exposure
    bool doefficiency = ( ahgen::strtoupper(par.m_outmaptype) == "EFFICIENCY" );

    // Energy for use in vignetting function
    double evig = 0.5*(par.m_erange.m_eminimg + par.m_erange.m_emaximg);

    // Coordinate frame centers
    double raw_xcen = 0.5*(1.0 + numrawx);
    double raw_ycen = 0.5*(1.0 + numrawy);
    double act_xcen = 0.5*(1.0 + numactx);
    double act_ycen = 0.5*(1.0 + numacty);
    
    // Ratio of pixel areas
    const double act_raw_area_factor = 1.0;

    // Number of time bins that lie inside GTI
    long numtbin = 0;
    double drx=0.0, dry=0.0, pixeltheta=0.0, vignetfactor=0.0;
    double xefficiency=0.0;
    double tbin_total=0.0;
    Double2D imageraw, imageact;   // Component image in RAW coordinates
    Double2D outimgtmp;  // Output image in ACT coordinates

    // params for routines borrowed from imagetrans
    IMAGETRANS* trans_param = (IMAGETRANS*)std::calloc(1, sizeof(IMAGETRANS));
    trans_param->combo = allocateComboXform();

    // Transformation descriptors for rot, shift
    XFORM2D* xf_actrawcen = allocateXform2d();
    XFORM2D* xf_actrawrot = allocateXform2d();
    XFORM2D* xf_actrawcenrot = allocateXform2d();
    XFORM2D* xf_actrawunshift = allocateXform2d();
      
    std::string ssys="";

    initDouble2D(imageraw, numrawx, numrawy);
    initDouble2D(imageact, numactx, numacty);
    initDouble2D(outimgtmp, numactx, numacty);

    // Compute vignetting if necessary.
    bool use_vig = (ahgen::strtoupper(par.m_vigfile) != "NONE");

    AH_INFO(ahlog::LOW) << "Number of CAMS times to be processed=" << numdeltatt << std::endl;
                
    //ahlog::debug(__func__, __FILE__, __LINE__,
    //  "i deltarawxfine deltarawyfine rawxfine rawyfine actx acty actxpix actypix\n");
    for (long i=0; i<numdeltatt; i++) {
      if (i % 500 == 0) {
        AH_INFO(ahlog::OUT) << "CAMS time index " << i << " of " << numdeltatt << std::endl;
      }
      
      // Skip to next i if time bin not in GTI
      if (gtipointer[i] >= 0) {
        numtbin++;
        tbin_total += tbinexposure[i];

        // Loops through all the pixel centers (1...numrawx) on the RAWX axis
        for (long j=0; j<numrawx-1; j++) {
          // Loops through all the pixel centers (1...numrawy) on the RAWY axis
          for (long k=0; k<numrawy-1; k++) {
            // Accumulate efficiency and exposure images in RAW.
            if (doefficiency) { // EFFICIENCY
              drx = (rawx[j] - opticrawx[i])*xmmperpixel;
              dry = (rawy[k] - opticrawy[i])*ymmperpixel;
              pixeltheta = fpmm2arcmin*std::sqrt(drx*drx + dry*dry);

              if (use_vig) {
                arfgenlib::ahvignetfactor(const_cast<char*>(par.m_instrume.c_str()), 
                  evig, pixeltheta, numcoeff, vignetcoeff, vignetfactor);
              } else {
                vignetfactor = 1.0;
              }

              // Calculate QE*vignetting*(baffle efficiency) for this pixel.
              // +++ 2016-01-28 RSH TRF has [i][j]
              xefficiency = avgqemap[j][k]*vignetfactor*hxibaffle[j][k];
              imageraw[j][k] = xefficiency*tbinexposure[i];
            } else {  // EXPOSURE
              imageraw[j][k] = tbinexposure[i];
            }
          } // end loop over rawy (k)
        } // end loop over rawyx (j)

        // Shift and center.
        setXform2dToTranslation(xf_actrawcen, deltarawx[i] - raw_xcen, deltarawy[i] - raw_ycen);

        // Rotate.
        setXform2dToRotation(xf_actrawrot, deltasinangle[i], deltacosangle[i], 0.0, 0.0);

        // Combine with shift.
        combineXform2ds(xf_actrawcenrot, xf_actrawcen, xf_actrawrot);

        // Recenter in ACT frame.
        setXform2dToTranslation(xf_actrawunshift, act_xcen, act_ycen);

        // Combine with shift.
        combineXform2ds(trans_param->combo->trans, xf_actrawcenrot, xf_actrawunshift);

        // Apply the transformation.
        applyImageTransToImage(trans_param, imageraw, imageact, act_raw_area_factor);

        // Accumulate in ACT.
        for (long j=0; j<numactx-1; j++) {
          for (long k=0; k<numacty-1; k++) {
            outimgtmp[j][k] += imageact[j][k];
          }
        }

      } // end if gtipointer non-negative

    } // end loop over times [i]

    AH_INFO(ahlog::HIGH) << "Total exposure in CAMS time bins used = " << tbin_total << std::endl;
    AH_INFO(ahlog::HIGH) << "Total exposure in GTI                 = " << totalexposure << std::endl;

    // Renormalize the net image array.
    if (ahgen::strtoupper(par.m_outmaptype) == "EFFICIENCY") {
      for (long j=0; j<numactx; j++) {
        for (long k=0; k<numacty; k++) {
          outimgtmp[j][k] /= tbin_total;
        }
      }
    }

    // Transform the flatfield image.

    ssys = ahgen::strtoupper(par.m_stopsys);
    if (ssys == "ACT") {
      initDouble2D(outimg, actimg.m_act_xsiz, actimg.m_act_ysiz);
      for (long i=0; i<actimg.m_act_xsiz; i++) {
        for (long j=0; j<actimg.m_act_ysiz; j++) {
          outimg[i][j] = outimgtmp[i][j];
        }
      }
    } else {

      double area_factor = 1.0;  // Ratio of pixel areas for normalizing

      if (ssys == "DET") {
        initDouble2D(outimg, actimg.m_det_xsiz, actimg.m_det_ysiz);
      } else if (ssys == "FOC" || ssys == "SKY") {
        initDouble2D(outimg, actimg.m_foc_xsiz, actimg.m_foc_ysiz);
      } else {
        AH_ERR << "STOPSYS=" << par.m_stopsys << std::endl;
        AH_THROW_RUNTIME("Invalid value for STOPSYS; exiting");
      }

      // Compute transformation parameters.
      
      computeXform2dActToDetFocOrSky (par.m_stopsys, 
        trans_param->combo->trans, 
        actimg.m_act_xcen, actimg.m_act_ycen, 
        actimg.m_detxflip, actimg.m_detyflip,
        actimg.m_cendet, actimg.m_foc_xoff, actimg.m_foc_yoff,
        actimg.m_focscaleratio, actimg.m_foc_rotd, actimg.m_cenfoc,
        actimg.m_roll_sign, actimg.m_ra_nom, actimg.m_dec_nom,
        actimg.m_ra_nom, actimg.m_dec_nom, actimg.m_pa_nom,
        actimg.m_censky, actimg.m_pixels_per_radian);

      if (ssys == "DET") {
        area_factor = 1.0;
      } else if (ssys == "FOC" || ssys == "SKY" ) {
        area_factor = actimg.m_focscaleratio*actimg.m_focscaleratio;
      } else {
        AH_ERR << "STOPSYS=" << par.m_stopsys << std::endl;
        AH_THROW_RUNTIME("Invalid value for STOPSYS; exiting");
      }

      applyImageTransToImage(trans_param, outimgtmp, outimg, area_factor);
    }

    destroyXform2d(xf_actrawunshift);
    destroyXform2d(xf_actrawcenrot);
    destroyXform2d(xf_actrawrot);
    destroyXform2d(xf_actrawcen);
    destroyComboXform(trans_param->combo);
    free(trans_param);

    // Flat field written out in finalize() function.

  } // end if doflatfield

  if (par.m_dorsp) {
    // Set up array to hold photon number weights by energy (coarse grid)
    // and by off-axis angle.  Note that for each raytracing event, we
    // generate a number of pseudo-events the correspond to each CAMS
    // time bine sampled.
    //
    // For every photon in the raytracing event file that successfully
    // hits the detector array for every CAMS (resampled) time bin, the
    // photonweight element correspongind to the photon energy and incident
    // off-axis angle is incremented.  (Later the results are normalized
    // to compute the correct effective area.)  Any energy-dependent
    // efficiency factors that also depend on RAWX and RAWY are accounted
    // for by the QE.

    Double2D photonweights; 
    Double1D photonsperenergy, xrtphotonsperenergy;
    Double2D qeweights;

    // Surviving pseudo-photon counter with QE
    initDouble2D(photonweights, numebinscoarse, rtnumoffaxis);

    // Surviving pseudo-photon counter (no QE), but including
    // region, detector, baffle accounting
    initDouble1D(photonsperenergy, numebinscoarse);

    // Surviving pseudo-photon counter, telescope only (regardless of
    // region, detector, baffle, etc., but including double
    // reflection check)
    initDouble1D(xrtphotonsperenergy, numebinscoarse);

    // QE for each energy and each layers, already weighted by the
    // number of pseudo-events falling in the detector data extraction
    // region for each energy
    initDouble2D(qeweights, numebinscoarse, par.m_numrmffiles);

    // Arrays for output ARF that includes QE as well as telescope.
    initDouble1D(arfeffarea, numrmfinputbins);  // Parameter of this function

    // Initialize the variables that will give the smallest and largest
    // numbers of photons in any coarse energy bin (summed over off-axis
    // angle) to indicate the statistical quality of the calculated ARF
    // (note that a real is used to count photons, not an integer).

    double minevtphotons = 1.0e30;
    double maxevtphotons = 0.0;

    // Initialize search/interpolation indexes.
    long energyindex = -1;
    int e1index = -1;
    int e2index = -1;
    //long offaxisindex = -1;
    //int th1index = -1;
    //int th2index = -1;

    std::string eunits = "";        // Energy units
    ahfits::Router rtxrt(fpxrt);    // Column data router

    // Local variables to read column data
    double l_rowenergy = 0.0;       // Energy
    long l_originalrownum = 0;      // Row number
    std::string l_pathcode = "";    // Raytracing path code
    double l_xrtx = 0.0;            // Raytracing coords and directions
    double l_xrty = 0.0;
    double l_xrtdirx = 0.0;
    double l_xrtdiry = 0.0;
    double l_xrtdirz = 0.0;
    long i = 0;                     // Raytracing row index
    long photoncounter = 0;
    double currentenergy = 0.0;
    // double currentoffaxis = 0.0;
    long currentoffaxisindex = 0.0;

    // Intermediate variables
    double xdetxrtframe = 0.0;
    double ydetxrtframe = 0.0;
    double xoptmm = 0.0;
    double yoptmm = 0.0;
    double evtrawxmm = 0.0;
    double evtrawymm = 0.0;
    long evtrawxindex = 0;
    long evtrawyindex = 0;
    double bcx = 0.0;
    double bcy = 0.0;
    double xbaftop = 0.0;
    double ybaftop = 0.0;
    double xbafbot = 0.0;
    double ybafbot = 0.0;
    double evttopradsq = 0.0;
    double baffletopradsq = 0.0;
    double evtbotradsq = 0.0;
    double bafflebotradsq = 0.0;
    double rawxcenmm = 0.0;
    double rawycenmm = 0.0;
    double totqe = 0.0;
    double eanormfactor = 0.0;
    // double rowoffaxis = 0.0;
    long rowoffaxisindex = 0;

    // Local variables for working with rotation angle of telescope
    // coordinates
    double detxflip_d = 0.0;
    double detyflip_d = 0.0;
    double cosoptrotd = 0.0;
    double sinoptrotd = 0.0;
    double optxflip_d = 0.0;
    double optyflip_d = 0.0;

    int insideregion = 0;       // Status variable - ray is inside a region
    Double1D xrteacoarse;       // Effective area with region selection
    Double1D xrtonlyeacoarse;   // Effective area without region selection
    Double2D rmfweights;        // RMF weights

    // RMF weights divided by the telescope effective area on the coarse grid
    Double2D rmfweightsoverxrtea;

    // Ratio of telescope EA to the on-axis telescope EA, on the coarse grid
    Double1D offaxistoonaxisearatiocoarse;

    // Ratio of telescope EA to the on-axis telescope EA, on the fine grid,
    // from polynomial fitting to the coarse ratio
    Double1D offaxistoonaxisearatiofine;

    int polyorder = 0;
    Double1D polycoeff;           // Polynomial coefficients

    Double1D fineoffaxiseffarea;  // Fine grid effective area

    Double1D xrtonlyeaout;        // XRT-only effective area
    double outeffarea = 0.0;      // XRT-only effective for one energy

    Double1D xrteaout;            // Effective area (diagnostic version)

    Double2D rspweights;          // RSP weights
    Double1D coarseweights;       // Subset of RSP weights input to interpolation
    double remappedweight = 0.0;  // RSP weight for one energy

    // Counter for theta bins with a photon impact.
    // +++ 2015-12-18 RSH Not used?  One instance in TRF.
    // numtbinimpacts = 0;

    // Go back to the first extension in the raytracing file.
    ahfits::move(fpxrt, 2);

    // Read energy units from header.
    eunits = ahgen::strtoupper(ahfits::getKeyValStr(fpxrt, "TUNIT1"));
    AH_INFO(ahlog::LOW) << "eunits=" << eunits << std::endl;
    rtxrt.connectScalar(ahfits::e_READONLY, "ENERGY", l_rowenergy);
    rtxrt.connectScalar(ahfits::e_READONLY, "ROWINDEX", l_originalrownum);
    rtxrt.connectScalar(ahfits::e_READONLY, "PATHCODE", l_pathcode);
    rtxrt.connectScalar(ahfits::e_READONLY, "FINALXPOS", l_xrtx);
    rtxrt.connectScalar(ahfits::e_READONLY, "FINALYPOS", l_xrty);
    rtxrt.connectScalar(ahfits::e_READONLY, "FINALXDIR", l_xrtdirx);
    rtxrt.connectScalar(ahfits::e_READONLY, "FINALYDIR", l_xrtdiry);
    rtxrt.connectScalar(ahfits::e_READONLY, "FINALZDIR", l_xrtdirz);

    // Select and report actual polynomial order to be used for fit.
    if (par.m_numeric_polydeg < 0) { // -1 means polydeg="DEFAULT"
      if (numebinscoarse < 6) {
        polyorder = numebinscoarse-1;
      } else {
        polyorder = 5;
      }
    } else {  // Use the user-supplied value if possible
      if (par.m_numeric_polydeg < numebinscoarse) {
        polyorder = par.m_numeric_polydeg;
      } else {
        polyorder = numebinscoarse-1;
      }
    }
    AH_INFO(ahlog::LOW) << "Order of off-axis vs. on-axis polynomial fit=" << polyorder << std::endl;
    initDouble1D(polycoeff, polyorder+1);  // Allocate array for fitted polynomial

    rawxcenmm = 0.5*(rawxminmm + rawxmaxmm);
    rawycenmm = 0.5*(rawyminmm + rawymaxmm);

    detxflip_d = actimg.m_detxflip;
    detyflip_d = actimg.m_detyflip;
    cosoptrotd = std::cos(actimg.m_opt_rotd*M_PI/180.0);
    sinoptrotd = std::sin(actimg.m_opt_rotd*M_PI/180.0);
    optxflip_d = actimg.m_optxflip;
    optyflip_d = actimg.m_optyflip;

    AH_INFO(ahlog::LOW) << "Number of rows in raytracing event file=" 
      << ahfits::numRows(fpxrt) << ", should be equal to " << rtnumrows << std::endl;

    i = 0; 
    for (ahfits::firstRow(fpxrt); ahfits::readOK(fpxrt); ahfits::nextRow(fpxrt)) {
      ahfits::readRow(fpxrt);

      if (eunits == "EV") {
        l_rowenergy /= 1000.0;
      }

      // +++ 2016-01-27 RSH TRF error 0-based vs. 1-based indexing
      // rowoffaxis = inputgrpoffaxis[l_originalrownum-1];
      rowoffaxisindex = inputgrpoffaxisindex[l_originalrownum-1];

      // The following will be used to tell when energy and off-axis
      // angle change compared to previous row.
      if (i == 0) {
        currentenergy = l_rowenergy;
        //currentoffaxis = rowoffaxis;
        currentoffaxisindex = rowoffaxisindex;
        //offaxisindex = 0;
        energyindex = 0;
      }

      // Does the event have a pathcode that is acceptable?  Currently
      // only double reflections are valid (but two reflections may occur
      // along with other events that are not reflections, such as
      // transmissions).

      if (!arfgenlib::ChkDblRefl(l_pathcode)) continue;

      // If the energy changes, update the energy index.

      if (i > 0 && l_rowenergy != currentenergy) {
        arfgenlib::bisectionLocate(ecoarsecen, numebinscoarse, l_rowenergy, e1index, e2index);
        if (l_rowenergy > ecoarsecen[0] && l_rowenergy < ecoarsecen[numebinscoarse-1]) {
          energyindex = e2index;
        } else {
          energyindex = e1index;
        }

        // Update the photon counters (used to warn against too few photons
        // in the event file to make a reliable ARF).

        if (photoncounter < minevtphotons) minevtphotons = photoncounter;
        if (photoncounter > maxevtphotons) maxevtphotons = photoncounter;

        photoncounter = 0;
        currentenergy = l_rowenergy;
      }

      // This counts all pseudo-photons that make it through the telescope,
      // and have undergone double reflections, regardless of whether the
      // photons are eventually detected or not.

      xrtphotonsperenergy[energyindex] += deltattbinsperoffaxis[currentoffaxisindex];

      // If the off-axis angle changes, update the off-axis angle index.
      // (For and extended source, thsi needs to be the off-axis angle of
      // a fixed point in the source -- e.g., the center -- not the
      // off-axis angle of entry of individual photons.)

      //long offaxisindex = 0;
      //if (i>0 && rowoffaxisindex != currentoffaxisindex) {
      //  arfgenlib::bisectionLocate(rtoffaxis, rtnumoffaxis, rowoffaxis, th1index, th2index);
      //  offaxisindex = th1index;
      //  currentoffaxisindex = rowoffaxisindex;
      //}
      currentoffaxisindex = rowoffaxisindex;
      // currentoffaxis = rowoffaxis;

      // The regions are in telescope coordinates, and the region does not move,
      // so select on region first.  We need the optical axis at t=0 in RAWX
      // and RAWY for each off-axis theta, which is then converted to mm 
      // because then any movement due to the CAMS can be translated to real
      // positions on the focal plane (and then related to coordinate of raytraced
      // event in the telescope frame).

      // Begin the loop over CAMS time bins here, selecting only time bins
      // that have the same theta for this row.

      // Does the event lie inside the region for this off-axis bin?

      xdetxrtframe = (optaxisXstdmm + ((cosoptrotd*optxflip_d*l_xrtx) - (sinoptrotd*optyflip_d*l_xrty)))/xmmperpixel;
      ydetxrtframe = (optaxisYstdmm + ((sinoptrotd*optxflip_d*l_xrtx) + (cosoptrotd*optyflip_d*l_xrty)))/ymmperpixel;

      // Case of no rotation
      //xdetxrtframe = (optaxisXstdmm + l_xrtx)/xmmperpixel;
      //ydetxrtframe = (optaxisYstdmm + l_xrty)/ymmperpixel;
      insideregion = 0;

      if (par.m_doregion) {

        // fits_in_region is an existing fitsio routine.  NOTE that these
        // region files were create using the standard optical axis position
        // so the same reference is used here.  Whether the photon is in
        // or out of the region does not depend on the CAMS motion because
        // the regions were created at fixed positions on the sky and are
        // fixed in the telescope frame.  The CAMS motion only affects where
        // the region appears on the detector.

        insideregion = fits_in_region(xdetxrtframe, ydetxrtframe,
          xrtregions[currentoffaxisindex]);  
        // xrtregions is a parameter of the current function (doWork)

      } else {
        
        insideregion = 1;

      }

      if (insideregion == 0) continue;  // Skip to next row of event file

      // Reset counter for how many of the delta-attitude bins result
      // in a detector impact.

      // +++ 2015-12-18 RSH Not used?
      // numtbinimpacts = 0;

      // Loop over delta-attitude bins.

      // +++ 2016-01-27 RSH These lines mistakenly deleted from TRF
      baffletopradsq = par.m_baffle.m_toprad*par.m_baffle.m_toprad;
      bafflebotradsq = par.m_baffle.m_botrad*par.m_baffle.m_botrad;
      for (long j=0; j<numdeltatt; j++) {
        
        // Only process this time bin if the off-axis angle index matches
        // the off-axis angle index for the current row in the event file.

        if (offaxisid[j] == currentoffaxisindex) {

          // Optical axis coordinates for this time bin.
          xoptmm = opticrawxmm[j];
          yoptmm = opticrawymm[j];

          // Position of the event in the row in the time-dependent raw
          // coordinates
          evtrawxmm = xoptmm + ((cosoptrotd*detxflip_d*optxflip_d*l_xrtx) - (sinoptrotd*detyflip_d*optyflip_d*l_xrty));
          evtrawymm = yoptmm + ((sinoptrotd*detxflip_d*optxflip_d*l_xrtx) + (cosoptrotd*detyflip_d*optyflip_d*l_xrty));

          // Case of no rotation
          //evtrawxmm = xoptmm + l_xrtx;
          //evtrawymm = yoptmm + l_xrty;

          // If the event lies outside the detector, skip this event and
          // go to the next delta-attitude time bin in j loop.

          if (evtrawxmm < rawxminmm || evtrawxmm > rawxmaxmm 
            || evtrawymm < rawyminmm || evtrawymm > rawymaxmm) continue;

          evtrawxindex = (evtrawxmm - rawxminmm)/xmmperpixel;
          evtrawyindex = (evtrawymm - rawyminmm)/ymmperpixel;

          // Position of the center of the baffle for this time bin
          bcx = (par.m_baffle.m_centerxmm-rawxcenmm)*deltacosangle[j] - (par.m_baffle.m_centerymm-rawycenmm)*deltasinangle[j] + deltarawxmm[j] + rawxcenmm;
          bcy = (par.m_baffle.m_centerxmm-rawxcenmm)*deltasinangle[j] + (par.m_baffle.m_centerymm-rawycenmm)*deltacosangle[j] + deltarawymm[j] + rawycenmm;

          // Event position at the top of the baffle
          xbaftop = evtrawxmm + (l_xrtdirx/l_xrtdirz)*par.m_baffle.m_topheight;
          ybaftop = evtrawymm + (l_xrtdiry/l_xrtdirz)*par.m_baffle.m_topheight;

          // If the event path does not go through the top baffle hole,
          // then skip this time bin and go to the next delta-attitude
          // time bin.
          evttopradsq = (xbaftop - bcx)*(xbaftop - bcx) + (ybaftop - bcy)*(ybaftop - bcy);
          if (evttopradsq > baffletopradsq) continue;

          // If the event path does not go through the bottom baffle hole,
          // then skip this time bin and go to the next delta-attitude
          // time bin.
          xbafbot = evtrawxmm + (l_xrtdirx/l_xrtdirz)*par.m_baffle.m_botheight;
          ybafbot = evtrawymm + (l_xrtdiry/l_xrtdirz)*par.m_baffle.m_botheight;
          evtbotradsq = (xbafbot - bcx)*(xbafbot - bcx) + (ybafbot - bcy)*(ybafbot - bcy);
          if (evtbotradsq > bafflebotradsq) continue;

          // If we have got this far, it means that the event path for
          // this delta-attitude time bin survived and hit the detector.
          // +++ RSH 2015-12-03 Come back and copy some of the TRF commentary.

          // First check if a QE file was specified.
          if (ahgen::strtoupper(par.m_qefile) != "NONE") {
            totqe = 0.0;
            for (long jq=0; jq<par.m_numrmffiles; jq++) {
              totqe += qemap[energyindex][evtrawxindex][evtrawyindex][jq];

              // Weights to be used in adding the RMFs
              qeweights[energyindex][jq] += qemap[energyindex][evtrawxindex][evtrawyindex][jq];

            }
          } else {
            totqe = 1.0;
          }

          photonweights[energyindex][offaxisid[j]] += totqe;

          // photonsperenergy is an array of counters to track the total number
          // of pseudo-photons per energy that impact the detector, regardless
          // of off-axis angle or time bin.  These counts are proportional to
          // the effective area of the telescope by itself, because they do not
          // include QE.  The normalization factor to give a separate ARF would 
          // then be geometricarea/inputphotonsperenergy.

          photonsperenergy[energyindex]++;

        } // End of if-block for off-axis angle equal to off-axis angle for current time bin
      } // End of loop over delta-attitude time bins

      // Update the photon counter that is used to assess the statistical quality
      // of the output.

      photoncounter++;  // Reset to zero for each new energy
      i++;              // Excludes these categories:  (1) wrong pathcode; (2) outside region

      if (i % 10000 == 1) {
        AH_INFO(ahlog::OUT) << "Raytracing event file rows processed = " << i << std::endl;
      }

    } // End of loop over raytracing event file rows

    AH_INFO(ahlog::OUT) << "Read all raytracing event file rows = " << rtnumrows << " rows" << std::endl;
    AH_INFO(ahlog::OUT) << "Of these, " << i << " have correct raytracing path code and are inside region" << std::endl;

    // If minevtphotons is too low, a viable ARF cannot be made.
    if (minevtphotons < par.m_minphoton) {
      AH_INFO(ahlog::OUT) << "Number of raytrace photons is too low; statistical errors" << std::endl;
      AH_INFO(ahlog::OUT) << "in ARF will be too large to make a viable ARF." << std::endl;
      insufficientphotons = true;
    }

    // Report photons per energy.
    ahlog::info(ahlog::LOW, __FUNCTION__, "     i           energy   photons per energy per CAMS time\n");

    for (long i=0; i<numebinscoarse; i++) {
      ahlog::info(ahlog::LOW, __FUNCTION__, "%7d  %15.7f %15.7f\n", 
         i, ecoarsecen[i], photonsperenergy[i]/numdeltatt);
    }

    // Calculate the normalized RMF weights (these inherently include the telescope
    // effective area as a fraction of the geometric area), and calculate the
    // net response matrix.

    // Also calculate the effective area of the telescope alone. on the coarse energy
    // grid, both with and without the effects of region selection, HXI baffle
    // obstruction, and finite detector area.
    initDouble1D(xrteacoarse, numebinscoarse);
    initDouble1D(xrtonlyeacoarse, numebinscoarse);

    // RMF weights on the coarse grid
    initDouble2D(rmfweights, numebinscoarse, par.m_numlayers);

    // RMF weights divided by the telescope effective area on the coarse grid
    initDouble2D(rmfweightsoverxrtea, numebinscoarse, par.m_numlayers);

    // Ratio of telescope EA to the on-axis telescope EA, on the coarse grid
    initDouble1D(offaxistoonaxisearatiocoarse, numebinscoarse);

    // Ratio of telescope EA to the on-axis telescope EA, on the fine grid,
    // from polynomial fitting to the coarse ratio
    initDouble1D(offaxistoonaxisearatiofine, numebinsfine);

    AH_INFO(ahlog::LOW) << "Computing ratios of weights" << std::endl;
    AH_DEBUG << "i ecoarsecen[i] xrtonlyeacoarse[i] xrteacoarse[i] offaxistoonaxisearatiocoarse[i]" << std::endl;
    for (long i=0; i<numebinscoarse; i++) {
      eanormfactor = effgeomarea/inputphotonsperenergy[i];
      xrteacoarse[i] = eanormfactor*photonsperenergy[i];
      xrtonlyeacoarse[i] = eanormfactor*xrtphotonsperenergy[i];
      offaxistoonaxisearatiocoarse[i] = xrtonlyeacoarse[i]/coarsearfeffarea[i];
      for (long j=0; j<par.m_numlayers; j++) {
        rmfweights[i][j] = eanormfactor*qeweights[i][j];
        if (xrtphotonsperenergy[i] > 0) {
          rmfweightsoverxrtea[i][j] = qeweights[i][j]/xrtphotonsperenergy[i];
        } else {
          rmfweightsoverxrtea[i][j] = 0.0;
        }
      }
      AH_DEBUG << i << " " << ecoarsecen[i] << " " << xrtonlyeacoarse[i] << " " << xrteacoarse[i] 
        << " " << offaxistoonaxisearatiocoarse[i] << std::endl;
    }

    // Fit the telescope EA coarse-grid with a polynomial of order 10 or less.
    // Function used is in headas_polyfit.c/.h.
    AH_INFO(ahlog::LOW) << "Fitting polynomial" << std::endl;
    HDpoly_fit(&ecoarsecen[0], &offaxistoonaxisearatiocoarse[0], &polycoeff[0],
      numebinscoarse, polyorder);
    
    for (long ii=0; ii<polyorder+1; ii++) {
      AH_INFO(ahlog::LOW) << "Poly coeff " << ii << " = " << polycoeff[ii] << std::endl;
    }

    // Evaluate the telescope EA ratio on the fine energy grid in the input
    // on-axis ARF file, and then multiply by the on-axis effective area at
    // each of those fine energies to get the fine-bin EA for the requested
    // region, off-axis histogram, and exposure maps (loop over fine energy grid).
    for (long i=0; i<numebinsfine; i++ ) {
      ahmath::evaluatepoly(efinecen[i], offaxistoonaxisearatiofine[i], polyorder, &polycoeff[0]);
    }

    // Calculate the fine-grid telescope EA using input fine-grid on-axis ARF
    // and the above ratio.
    initDouble1D(fineoffaxiseffarea, numebinsfine);
    for (long i=0; i<numebinsfine; i++) {
      fineoffaxiseffarea[i] = finearfeffarea[i]*offaxistoonaxisearatiofine[i];
    }

    // Remap the fine-grid telescope EA onto the output energy grid.

    // Loop over output energies.
    initDouble1D(xrtonlyeaout, numeoutbins);
    AH_DEBUG << "i eoutkev[i] xrtonlyeaout[i]" << std::endl;
    for (long i=0; i<numeoutbins; i++) {
      // Interpolate the telescope-only EA for this energy onto the output
      // energy grid.  This would be the final output effective area for the
      // output ARF file if one were being made.
      arfgenlib::bisectionInterp(numebinsfine, efinecen, fineoffaxiseffarea,
        eoutkev[i], outeffarea);
      xrtonlyeaout[i] = outeffarea*outauxtrans[i];
      AH_DEBUG << i << " " << eoutkev[i] << " " << xrtonlyeaout[i] << std::endl;
    }

    // The following is currently used only as a diagnostic (it is the telescope
    // EA with region selection, detector impact, baffle obstruction but without
    // QE).  If we want to write a separate ARF output, this would have to be
    // calculated on the fine energy grid first.
    initDouble1D(xrteaout, numeoutbins);
    for (long i=0; i<numeoutbins; i++) {
      
      // Interpolate the telescope-only EA for this energy onto the output
      // energy grid.  This would be the final output effective area for the
      // output ARF file if one were being made.
      if (eoutkev[i] >= rt_min_e && eoutkev[i] <= rt_max_e) {
	arfgenlib::bisectionInterp(numebinscoarse, ecoarsecen, xrteacoarse,
	  eoutkev[i], outeffarea);
	xrteaout[i] = outeffarea*outauxtrans[i];
      }
    }

    // Remap RMF weights onto the energy grid of the input RMF files.
    initDouble2D(rspweights, numrmfinputbins, par.m_numlayers);
    initDouble1D(coarseweights, numebinscoarse);
    for (long i=0; i<numrmfinputbins; i++) {
      // Only do the interpolation is the energy is covered by raytracing
      // events; otherwise, leave the weight at zero.
      if (eoutkev[i] >= rt_min_e && eoutkev[i] <= rt_max_e) {
	for (long j=0; j<par.m_numlayers; j++) {
	  for (long kk=0; kk<numebinscoarse; kk++) {
	    coarseweights[kk] = rmfweightsoverxrtea[kk][j];
	  }
	  arfgenlib::bisectionInterp(numebinscoarse, ecoarsecen, coarseweights,
	    eoutkev[i], remappedweight);
	  rspweights[i][j] = remappedweight*xrtonlyeaout[i];
	}
      }
    }

    AH_INFO(ahlog::LOW) << "Energy grids remapped" << std::endl;

    // Calculate the net response matrix.
    for (long i=0; i<numrmfinputbins; i++) {
      for (long j=0; j<numrmfchan; j++) {
        for (long k=0; k<par.m_numlayers; k++) {
          // +++ 2016-01-28 RSH TRF error rspweights[i][j] originally
          rspmatrix[i][j] += rspweights[i][k]*inputrmfs[i][j][k];
        }
      }
    }
    AH_INFO(ahlog::LOW) << "Response matrix calculated" << std::endl;

    // Construct an ARF that includes the QE and not just the telescope.
    for (long i=0; i<numrmfinputbins; i++) {
      for (long j=0; j<numrmfchan; j++) {
        arfeffarea[i] += rspmatrix[i][j];
      }
    }

    // ARF and RSP files written out in finalize() function.

  }

  AH_DEBUG << "Exiting doWork" << std::endl;

}

// ****************************************************************************

void finalize(Par & par, ahfits::FilePtr fpemap, ahfits::FilePtr fpxrt,
  bool insufficientphotons, double geomarea, long numeoutbins,
  bool kevunits, Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev, 
  rmflib::RMFData & rmfdat, Double2D & rspmatrix, Double1D & arfeffarea,
  CRKeywords & actimg, long numactx, long numacty, 
  Double2D & outimg, double ** vignetcoeff) {

  if (par.m_dorsp) {
    if (!insufficientphotons) {
      arfgenlib::arfkeywords arfkeywords;
      arfkeywords.m_instrume = ahgen::strtoupper(par.m_instrume);
      arfkeywords.m_detnam = ahgen::strtoupper(ahfits::getKeyValStr(fpemap, "DETNAM"));
      arfkeywords.m_geomarea = geomarea;
      arfkeywords.m_emapfile = par.m_emapfile;
      ahfits::move(fpemap, "OFFAXISHIST");
      arfkeywords.m_obstime = ahfits::getKeyValDbl(fpemap, "TSTART");
      arfkeywords.m_xrtevtfile = par.m_xrtevtfile;
      arfgenlib::writearffile(par.m_outarffile, kevunits, arfkeywords, numeoutbins,
        ekevlo, ekevhi, arfeffarea);
      AH_INFO(ahlog::HIGH) << "HXI ARF file " << par.m_outarffile << 
        " written" << std::endl;
      rmfdat.m_lo_thresh = par.m_rmfthresh;
      writerspfile(par.m_outrspfile, numeoutbins, ekevlo, ekevhi, eoutkev, rmfdat, rspmatrix);
      AH_INFO(ahlog::HIGH) << "HXI response matrix file " << par.m_outrspfile << 
        " written" << std::endl;
      ahfits::close(fpxrt);
    } else {
      AH_INFO(ahlog::HIGH) << "HXI ARF file " << par.m_outarffile << 
        " and RSP file " << par.m_outrspfile <<
        " not written: insufficient photons" << std::endl;
    }
  }

  if (par.m_doflatfield) {

    // Write out the flatfield image.

    std::string ssys = ahgen::strtoupper(par.m_stopsys);

    writeimg(ssys, fpemap, par.m_outflatfile, actimg, numactx, numacty, outimg);

    if (ahgen::strtoupper(par.m_outmaptype) == "EFFICIENCY") {
      AH_INFO(ahlog::HIGH) << "Flat field (efficiency map) " << par.m_outflatfile << 
        " written" << std::endl;
      AH_INFO(ahlog::HIGH) << "in " << ssys << " coordinate system" << std::endl;
    } else {
      AH_INFO(ahlog::HIGH) << "Exposure map [s] " << par.m_outflatfile << 
        " written" << std::endl;
      AH_INFO(ahlog::HIGH) << "in " << ssys << " coordinate system" << std::endl;
    }

    if (ahgen::strtoupper(par.m_vigfile) != "NONE") {
      arfgenlib::cleanup_vignet(vignetcoeff);
    }
  }

  ahfits::close(fpemap);
}

// ****************************************************************************

void writeimg(std::string & stopsys, ahfits::FilePtr fpemap,
  std::string & filename, CRKeywords & actimg, long xsize, long ysize, Double2D & image) {

  ahfits::Img2dFlt imgbuff;
  long nx = image.size();
  long ny = image[0].size();
  imgbuff.resize(ny);
  for (long j=0; j<ny; j++) {
    imgbuff[j].resize(nx, 0.0);
    for (long i=0; i<nx; i++) {
      imgbuff[j][i] = image[i][j];
    }
  }

  ahfits::FilePtr fpout;
  ahfits::create(filename, "", &fpout);
  // Requires pixels in FITS storage order = transpose of natural
  // C/C++ order.
  ahfits::writeImage(fpout, "", imgbuff, 0.0, 1.0, 0);
  ahfits::move(fpemap, "OFFAXISHIST");
  ahmission::keyword::copyAllKeywords(fpemap, fpout, ahmission::keyword::e_EVENT);
  if (stopsys == "ACT") {
    ahfits::writeKeyValDbl(fpout, "CRVAL1", actimg.m_crval1, "Reference coord X");
    ahfits::writeKeyValDbl(fpout, "CRPIX1", actimg.m_crpix1, "Reference pixel X");
    ahfits::writeKeyValDbl(fpout, "CDELT1", actimg.m_cdelt1, "X increment");
    ahfits::writeKeyValStr(fpout, "CUNIT1", actimg.m_cunit1, "X units");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "ACTX", "Coordinate type");
    ahfits::writeKeyValDbl(fpout, "CRVAL2", actimg.m_crval2, "Reference coord Y");
    ahfits::writeKeyValDbl(fpout, "CRPIX2", actimg.m_crpix2, "Reference pixel Y");
    ahfits::writeKeyValDbl(fpout, "CDELT2", actimg.m_cdelt2, "Y increment");
    ahfits::writeKeyValStr(fpout, "CUNIT2", actimg.m_cunit2, "Y units");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "ACTY", "Coordinate type");
  } else if (stopsys == "DET") {
    ahfits::writeKeyValDbl(fpout, "CRVAL1", 0.5*(1.0 + actimg.m_det_xsiz), "Reference coord X");
    ahfits::writeKeyValDbl(fpout, "CRPIX1", 0.5*(1.0 + actimg.m_det_xsiz), "Reference pixel X");
    ahfits::writeKeyValDbl(fpout, "CDELT1", 1, "X increment");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "DETX", "Coordinate type");
    ahfits::writeKeyValDbl(fpout, "CRVAL2", 0.5*(1.0 + actimg.m_det_ysiz), "Reference coord Y");
    ahfits::writeKeyValDbl(fpout, "CRPIX2", 0.5*(1.0 + actimg.m_det_ysiz), "Reference pixel Y");
    ahfits::writeKeyValDbl(fpout, "CDELT2", 1, "Y increment");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "DETY", "Coordinate type");
  } else if (stopsys == "FOC") {
    ahfits::writeKeyValDbl(fpout, "CRVAL1", 0.5*(1.0 + actimg.m_foc_xsiz), "Reference coord X");
    ahfits::writeKeyValDbl(fpout, "CRPIX1", 0.5*(1.0 + actimg.m_foc_xsiz), "Reference pixel X");
    ahfits::writeKeyValDbl(fpout, "CDELT1", 1, "X increment");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "FOCX", "Coordinate type");
    ahfits::writeKeyValDbl(fpout, "CRVAL2", 0.5*(1.0 + actimg.m_foc_ysiz), "Reference coord Y");
    ahfits::writeKeyValDbl(fpout, "CRPIX2", 0.5*(1.0 + actimg.m_foc_ysiz), "Reference pixel Y");
    ahfits::writeKeyValDbl(fpout, "CDELT2", 1, "Y increment");
    ahfits::writeKeyValStr(fpout, "CTYPE1", "FOCY", "Coordinate type");
  } else if (stopsys == "SKY") {
    ahfits::writeKeyValStr(fpout, "CTYPE1", "RA---TAN", "Coordinate axis");
    ahfits::writeKeyValDbl(fpout, "CRVAL1", actimg.m_ra_nom, "[deg] Reference value");
    ahfits::writeKeyValDbl(fpout, "CRPIX1", actimg.m_censky, "[pixel] Reference point");
    ahfits::writeKeyValDbl(fpout, "CDELT1", actimg.m_xinc_sky, "[deg/pixel] Coordinate increment");
    ahfits::writeKeyValStr(fpout, "CTYPE2", "DEC--TAN", "Coordinate axis");
    ahfits::writeKeyValDbl(fpout, "CRVAL2", actimg.m_dec_nom, "[deg] Reference value");
    ahfits::writeKeyValDbl(fpout, "CRPIX2", actimg.m_censky, "[pixel] Reference point");
    ahfits::writeKeyValDbl(fpout, "CDELT2", actimg.m_yinc_sky, "[deg/pixel] Coordinate increment");
  } else {
    AH_THROW_RUNTIME("Invalid STOPSYS parameter");
  }
  ahfits::writeKeyValDbl(fpout, "RA_NOM", actimg.m_ra_nom, "[deg] Avg aspect point R.A.");
  ahfits::writeKeyValDbl(fpout, "DEC_NOM", actimg.m_dec_nom, "[deg] Avg aspect point Dec");
  ahfits::writeKeyValDbl(fpout, "PA_NOM", actimg.m_pa_nom, "[deg] Avg position angle (roll)");
  ahfits::writeKeyValDbl(fpout, "RA_PNT", actimg.m_ra_pnt, "[deg] Avg Optical axis R.A.");
  ahfits::writeKeyValDbl(fpout, "DEC_PNT", actimg.m_dec_pnt, "[deg] Avg Optical axis Dec");

  ahfits::close(fpout);
}

// ****************************************************************************

void writerspfile(std::string outrspfile, long numeoutbins, 
  Double1D & ekevlo, Double1D & ekevhi, Double1D & eoutkev, 
  rmflib::RMFData & rmfdat, Double2D & rspmatrix) {

  Double1D row;
  initDouble1D(row, rmfdat.m_nchan);
  rmfdat.m_ahffp = 0;
  rmfdat.m_write = true;
  rmflib::setInputEnergies(&rmfdat, &ekevlo[0], &ekevhi[0], &eoutkev[0], numeoutbins);
  if (ahgen::strtoupper(rmfdat.m_tunit_emin) != "KEV") {
    for (long i=0; i<rmfdat.m_nchan; i++) {
      rmfdat.m_echanstart[i] /= 1000.0;
      rmfdat.m_echanstop[i] /= 1000.0;
      rmfdat.m_echancen[i] /= 1000.0;
    }
  }
  rmflib::createRMFFile(&rmfdat, outrspfile);
  rmflib::writeEboundsHDU(&rmfdat);
  rmflib::createMatrixHDU(&rmfdat);
  for (long rowidx=0; rowidx<numeoutbins; rowidx++) {
    for (long j=0; j<rmfdat.m_nchan; j++) {
      row[j] = rspmatrix[rowidx][j];
    }
    rmflib::writeMatrixRow(&rmfdat, &row[0], rmfdat.m_nchan, rowidx);
  }
  rmflib::closeRMFFile(&rmfdat);
}

// ****************************************************************************

void getCoordKeywords(ahfits::FilePtr ahffp, CRKeywords& keywd) {

  double plate_scale = 0.0;

  keywd.m_ra_pnt = ahfits::getKeyValDbl(ahffp, "RA_PNT");
  keywd.m_dec_pnt = ahfits::getKeyValDbl(ahffp, "DEC_PNT");
  keywd.m_ra_nom = ahfits::getKeyValDbl(ahffp, "RA_NOM");
  keywd.m_dec_nom = ahfits::getKeyValDbl(ahffp, "DEC_NOM");
  keywd.m_pa_nom = ahfits::getKeyValDbl(ahffp, "PA_NOM");
  keywd.m_raw_xscl = ahfits::getKeyValDbl(ahffp, "RAW_XSCL");
  keywd.m_raw_yscl = ahfits::getKeyValDbl(ahffp, "RAW_YSCL");
  keywd.m_act_xsiz = ahfits::getKeyValLLong(ahffp, "ACT_XSIZ");
  keywd.m_act_ysiz = ahfits::getKeyValLLong(ahffp, "ACT_YSIZ");
  keywd.m_act_xscl = ahfits::getKeyValDbl(ahffp, "ACT_XSCL");
  keywd.m_act_yscl = ahfits::getKeyValDbl(ahffp, "ACT_YSCL");
  keywd.m_det_xsiz = ahfits::getKeyValLLong(ahffp, "DET_XSIZ");
  keywd.m_det_ysiz = ahfits::getKeyValLLong(ahffp, "DET_YSIZ");
  keywd.m_det_xscl = ahfits::getKeyValDbl(ahffp, "DET_XSCL");
  keywd.m_det_yscl = ahfits::getKeyValDbl(ahffp, "DET_YSCL");
  keywd.m_detxflip = ahfits::getKeyValLLong(ahffp, "DETXFLIP");
  keywd.m_detyflip = ahfits::getKeyValLLong(ahffp, "DETYFLIP");
  keywd.m_foc_xsiz = ahfits::getKeyValLLong(ahffp, "FOC_XSIZ");
  keywd.m_foc_ysiz = ahfits::getKeyValLLong(ahffp, "FOC_YSIZ");
  keywd.m_foc_xscl = ahfits::getKeyValDbl(ahffp, "FOC_XSCL");
  keywd.m_foc_yscl = ahfits::getKeyValDbl(ahffp, "FOC_YSCL");
  keywd.m_foc_xoff = ahfits::getKeyValDbl(ahffp, "FOC_XOFF");
  keywd.m_foc_yoff = ahfits::getKeyValDbl(ahffp, "FOC_YOFF");
  keywd.m_foc_rotd = ahfits::getKeyValDbl(ahffp, "FOC_ROTD");
  keywd.m_focallen = ahfits::getKeyValDbl(ahffp, "FOCALLEN");
  keywd.m_optaxisx = ahfits::getKeyValDbl(ahffp, "OPTAXISX");
  keywd.m_optaxisy = ahfits::getKeyValDbl(ahffp, "OPTAXISY");
  keywd.m_optxflip = ahfits::getKeyValLLong(ahffp, "OPTXFLIP");
  keywd.m_optyflip = ahfits::getKeyValLLong(ahffp, "OPTYFLIP");
  keywd.m_opt_rotd = ahfits::getKeyValDbl(ahffp, "OPT_ROTD");

  keywd.m_act_xcen = 0.5*(keywd.m_act_xsiz + 1.0);
  keywd.m_act_ycen = 0.5*(keywd.m_act_ysiz + 1.0);
  keywd.m_roll_sign = -1;
  keywd.m_focscaleratio = keywd.m_det_xscl/keywd.m_foc_xscl;
  keywd.m_cendet = 0.5*(keywd.m_det_xsiz + 1.0);
  keywd.m_cenfoc = 0.5*(keywd.m_foc_xsiz + 1.0);
  keywd.m_censky = 0.5*(keywd.m_foc_xsiz + 1.0);
  keywd.m_pixels_per_radian = keywd.m_focallen/keywd.m_foc_xscl;  // (focal len)/(pixel pitch) [mm]/[mm/px]
  plate_scale = (1.0/keywd.m_pixels_per_radian)*180.0/M_PI;
  keywd.m_xinc_sky = -plate_scale;  // RA increases right to left
  keywd.m_yinc_sky = +plate_scale;
}

// ****************************************************************************

void computeXform2dActToDetFocOrSky (std::string& stopsys, XFORM2D* xform, 
  const double act_xcen, const double act_ycen, 
  const double detxflip, const double detyflip,
  const double cendet, const double foc_xoff, const double foc_yoff,
  const double focscaleratio, const double foc_rotd, const double cenfoc,
  const double roll_sign, const double ra_nom, const double dec_nom,
  const double ra_exp, const double dec_exp, const double pa_exp,
  const double censky, const double pixels_per_radian) {

  std::stringstream ss;  // For formatting strings

  double sinang=0.0, cosang=0.0;  // Intermediate variables

  // Allocate 2-D transformations (rot, shift)
  XFORM2D* xform_act_to_det = allocateXform2d();
  XFORM2D* xform_det_to_foc = allocateXform2d();
  XFORM2D* xform_act_to_foc = allocateXform2d();
  XFORM2D* xform_exp_to_nom = allocateXform2d();

  XFORM2D* xf_detactcen = allocateXform2d();
  XFORM2D* xf_detactsca = allocateXform2d();
  XFORM2D* xf_detactcensca = allocateXform2d();
  XFORM2D* xf_detactrot = allocateXform2d();
  XFORM2D* xf_detactcenscarot = allocateXform2d();
  XFORM2D* xf_detactunshift = allocateXform2d();

  XFORM2D* xf_focdetcen = allocateXform2d();
  XFORM2D* xf_focdetsca = allocateXform2d();
  XFORM2D* xf_focdetcensca = allocateXform2d();
  XFORM2D* xf_focdetrot = allocateXform2d();
  XFORM2D* xf_focdetcenscarot = allocateXform2d();
  XFORM2D* xf_focdetunshift = allocateXform2d();

  // Allocate quaternions
  QUAT* quat_nom = allocateQuat();
  QUAT* quat_exp = allocateQuat();
  QUAT* deltaq = allocateQuat();

  // Allocate alignment descriptor
  ALIGN* align_identity = allocateDefaultAlign();

  // Make center of ACT the origin
  setXform2dToTranslation(xf_detactcen, -act_xcen + 1, -act_ycen + 1);
  //AH_INFO(ahlog::LOW) << "xf_detactcen:" << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][0] = " << xf_detactcen->rot[0][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][0] = " << xf_detactcen->rot[1][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][1] = " << xf_detactcen->rot[0][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][1] = " << xf_detactcen->rot[1][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "xshift    = " << xf_detactcen->xshift << std::endl;
  //AH_INFO(ahlog::LOW) << "yshift    = " << xf_detactcen->yshift << std::endl;

  // Scale ACT to DET (which is really only flipping)
  setXform2dToScaling(xf_detactsca, detxflip, detyflip, 0.0, 0.0);
  //AH_INFO(ahlog::LOW) << "xf_detactsca:" << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][0] = " << xf_detactsca->rot[0][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][0] = " << xf_detactsca->rot[1][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][1] = " << xf_detactsca->rot[0][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][1] = " << xf_detactsca->rot[1][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "xshift    = " << xf_detactsca->xshift << std::endl;
  //AH_INFO(ahlog::LOW) << "yshift    = " << xf_detactsca->yshift << std::endl;

  // Combine the operations
  combineXform2ds(xf_detactcensca, xf_detactcen, xf_detactsca);
  //AH_INFO(ahlog::LOW) << "xf_detactcensca:" << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][0] = " << xf_detactcensca->rot[0][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][0] = " << xf_detactcensca->rot[1][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][1] = " << xf_detactcensca->rot[0][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][1] = " << xf_detactcensca->rot[1][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "xshift    = " << xf_detactcensca->xshift << std::endl;
  //AH_INFO(ahlog::LOW) << "yshift    = " << xf_detactcensca->yshift << std::endl;
  //AH_INFO(ahlog::LOW) << "cendet    = " << cendet << std::endl;
  
  // Move origin to center of DET.
  setXform2dToTranslation(xf_detactunshift, cendet - 1, cendet - 1);
  //AH_INFO(ahlog::LOW) << "xf_detactunshift:" << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][0] = " << xf_detactunshift->rot[0][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][0] = " << xf_detactunshift->rot[1][0] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[0][1] = " << xf_detactunshift->rot[0][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "rot[1][1] = " << xf_detactunshift->rot[1][1] << std::endl;
  //AH_INFO(ahlog::LOW) << "xshift    = " << xf_detactunshift->xshift << std::endl;
  //AH_INFO(ahlog::LOW) << "yshift    = " << xf_detactunshift->yshift << std::endl;

  // Combine the operations.
  combineXform2ds(xform_act_to_det, xf_detactcensca, xf_detactunshift);

  AH_INFO(ahlog::LOW) << "Xform2d for ACT to DET:" << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][0] = " << xform_act_to_det->rot[0][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][0] = " << xform_act_to_det->rot[1][0] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[0][1] = " << xform_act_to_det->rot[0][1] << std::endl;
  AH_INFO(ahlog::LOW) << "rot[1][1] = " << xform_act_to_det->rot[1][1] << std::endl;
  AH_INFO(ahlog::LOW) << "xshift    = " << xform_act_to_det->xshift << std::endl;
  AH_INFO(ahlog::LOW) << "yshift    = " << xform_act_to_det->yshift << std::endl;

  // Decide if the computation should go further.
  if (ahgen::strtoupper(stopsys) == "DET") {

    copyXform2d(xform, xform_act_to_det);

  } else {

    // Compute xform2d to convert DET to FOC

    // Make center of FOC the origin
    setXform2dToTranslation(xf_focdetcen, -cendet - foc_xoff + 1, -cendet - foc_yoff + 1);

    // Scale DET to FOC
    setXform2dToScaling(xf_focdetsca, focscaleratio, focscaleratio, 0.0, 0.0);

    // Combine the operations
    combineXform2ds(xf_focdetcensca, xf_focdetcen, xf_focdetsca);

    // Rotate
    sinang = std::sin(foc_rotd*M_PI/180.0);
    cosang = std::cos(foc_rotd*M_PI/180.0);
    setXform2dToRotation(xf_focdetrot, sinang, cosang, 0.0, 0.0);

    // Combine the operations.
    combineXform2ds(xf_focdetcenscarot, xf_focdetcensca, xf_focdetrot);

    // Move origin to center of FOC.
    setXform2dToTranslation(xf_focdetunshift, cenfoc - 1, cenfoc - 1);

    // Combine the operations.
    combineXform2ds(xform_det_to_foc, xf_focdetcenscarot, xf_focdetunshift);

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

      // Combine with previous transformations
      combineXform2ds(xform_act_to_foc, xform_act_to_det, xform_det_to_foc);
      combineXform2ds(xform, xform_act_to_foc, xform_exp_to_nom);

      AH_INFO(ahlog::LOW) << "Final Xform2d:" << std::endl;
      AH_INFO(ahlog::LOW) << "rot[0][0] = " << xform->rot[0][0] << std::endl;
      AH_INFO(ahlog::LOW) << "rot[1][0] = " << xform->rot[1][0] << std::endl;
      AH_INFO(ahlog::LOW) << "rot[0][1] = " << xform->rot[0][1] << std::endl;
      AH_INFO(ahlog::LOW) << "rot[1][1] = " << xform->rot[1][1] << std::endl;
      AH_INFO(ahlog::LOW) << "xshift    = " << xform->xshift << std::endl;
      AH_INFO(ahlog::LOW) << "yshift    = " << xform->yshift << std::endl;

    } else {

      AH_ERR << "stopsys=" << stopsys << std::endl;
      AH_THROW_RUNTIME("stopsys must be DET, FOC, or SKY; exiting");

    }
  }
  destroyXform2d(xform_act_to_det);
  destroyXform2d(xform_det_to_foc);
  destroyXform2d(xform_act_to_foc);
  destroyXform2d(xform_exp_to_nom);

  destroyXform2d(xf_detactcen);
  destroyXform2d(xf_detactsca);
  destroyXform2d(xf_detactcensca);
  destroyXform2d(xf_detactrot);
  destroyXform2d(xf_detactcenscarot);
  destroyXform2d(xf_detactunshift);

  destroyXform2d(xf_focdetcen);
  destroyXform2d(xf_focdetsca);
  destroyXform2d(xf_focdetcensca);
  destroyXform2d(xf_focdetrot);
  destroyXform2d(xf_focdetcenscarot);
  destroyXform2d(xf_focdetunshift);

  destroyQuat(deltaq);
  destroyQuat(quat_exp);
  destroyQuat(quat_nom);

}

// ****************************************************************************

void applyImageTransToImage(IMAGETRANS* trans_param, Double2D& input_image, 
  Double2D& output_image, double area_factor) {

  IMAGE* original;                  // Image before transformation 
  IMAGE* transformed;               // Image after transformation 

  long length_x=0, length_y=0;      // Input image dimensions
  long out_xsiz=0, out_ysiz=0;      // Output image dimensions
 
  // Allocate temporary image and parameter structures for imagetranslib
  // routines.

  length_x = input_image.size();
  length_y = input_image[0].size();

  out_xsiz = output_image.size();
  out_ysiz = output_image[0].size();

  original = allocateImage(length_x, length_y, DOUBLE_IMG);
  transformed = allocateImage(out_xsiz, out_ysiz, DOUBLE_IMG);

  // Zero.
  for (long cx=0; cx<length_x; cx++) {
    for (long cy=0; cy<length_y; cy++) {
      setImagePixel(original, cx, cy, 0.0);
    }
  }
  for (long cx=0; cx<out_xsiz; cx++) {
    for (long cy=0; cy<out_ysiz; cy++) {
      setImagePixel(transformed, cx, cy, 0.0);
    }
  }

  // Copy offaxis image into the required input structure.
  for (long cx=0; cx<length_x; cx++) {
    for (long cy=0; cy<length_y; cy++) {
      setImagePixel(original, cx, cy, input_image[cx][cy]);
    }
  }

  transform_by_area(original, transformed, trans_param);

  // Copy to the right data structure for writing
  for (long cx=0; cx<out_xsiz; cx++) {
    for (long cy=0; cy<out_ysiz; cy++) {
      output_image[cx][cy] = area_factor*getImagePixel(transformed, cx, cy);
    }
  }

  // Free temporary image and parameter structures.
  destroyImage(original);
  destroyImage(transformed);

}

// ****************************************************************************

void initDouble1D(Double1D & dbl1d, long xsize) {
  dbl1d.resize(xsize);
  for (long i=0; i<xsize; i++) {
    dbl1d[i] = 0.0;
  }
}

// ****************************************************************************

void initDouble2D(Double2D & dbl2d, long xsize, long ysize) {
  dbl2d.resize(xsize);
  for (long i=0; i<xsize; i++) {
    dbl2d[i].resize(ysize);
    for (long j=0; j<ysize; j++) {
      dbl2d[i][j] = 0.0;
    }
  }
}

// ****************************************************************************

void initDouble3D(Double3D & dbl3d, long xsize, long ysize, long zsize) {
  dbl3d.resize(xsize);
  for (long i=0; i<xsize; i++) {
    dbl3d[i].resize(ysize);
    for (long j=0; j<ysize; j++) {
      dbl3d[i][j].resize(zsize);
      for (long k=0; k<zsize; k++) {
        dbl3d[i][j][k] = 0.0;
      }
    }
  }
}

// ****************************************************************************

void initDouble4D(Double4D & dbl4d, long xsize, long ysize, long zsize, long psize) {
  dbl4d.resize(xsize);
  for (long i=0; i<xsize; i++) {
    dbl4d[i].resize(ysize);
    for (long j=0; j<ysize; j++) {
      dbl4d[i][j].resize(zsize);
      for (long k=0; k<zsize; k++) {
        dbl4d[i][j][k].resize(psize);
        for (long m=0; m<psize; m++) {
          dbl4d[i][j][k][m] = 0.0;
        }
      }
    }
  }
}

// ****************************************************************************

void initLong1D(Long1D & long1d, long xsize) {
  long1d.resize(xsize);
  for (long i=0; i<xsize; i++) {
    long1d[i] = 0;
  }
}

// ****************************************************************************


/** @} */


/* Revision Log
 $Log: hxirspeffimg.cxx,v $
 Revision 1.52  2017/01/13 23:41:38  rshill
 Added polydeg parameter to control fitting of effective
 area ratio on coarse energy grid.

 Revision 1.51  2016/10/27 21:28:45  rshill
 Added CALDB query for auxtransfile parameter.

 Revision 1.50  2016/09/16 17:18:55  rshill
 Added rmfthresh parameter.

 Revision 1.49  2016/09/02 19:27:00  rshill
 Deleted unused variable rowoffaxis.

 Revision 1.48  2016/09/02 19:23:05  rshill
 Put outauxtrans factor into output ARF; deleted unused
 variable currentoffaxis.

 Revision 1.47  2016/09/02 19:10:10  rshill
 Corrected bug of failure to advance read pointer through
 auxiliary transmission file.

 Revision 1.46  2016/08/31 17:44:18  rshill
 Corrected stepping through list of region files.
 Eliminated some unused variables.

 Revision 1.45  2016/08/30 21:29:37  rshill
 Add in the auxiliary transmission file.  Correct image
 transformations for use with 0-based arrays.

 Revision 1.44  2016/07/12 15:13:18  rshill
 Use DATE-OBS for CALDB query on QE and LSF files.

 Revision 1.43  2016/04/14 20:35:32  rshill
 Do not write RSP file if insufficient photons.

 Revision 1.42  2016/04/13 16:30:09  rshill
 Message tweaked.

 Revision 1.41  2016/04/13 16:15:38  rshill
 Clarified progress messages and changed chatter to always appear.

 Revision 1.40  2016/03/31 22:29:59  rshill
 Reverted loop through raytracing photons to previous form
 to eliminate an introduced bug.  Correct subscripting of deltasinangle and deltacosangle.

 Revision 1.39  2016/03/30 19:56:59  rshill
 Corrected shadowing of arfeffarea parameter in doWork.

 Revision 1.38  2016/03/30 18:44:57  rshill
 Some instances of outrspfile changed to outfile.

 Revision 1.37  2016/03/30 15:56:05  rshill
 Make output ARF.  Moved output RSP writing to finalize().

 Revision 1.36  2016/03/22 21:50:21  rshill
 Added writeParametersToLog() to standard main.

 Revision 1.35  2016/03/09 20:58:28  rshill
 Fixed vignetting file CALDB resolution; raised priority of photon
 counter progress message.

 Revision 1.34  2016/03/08 23:53:14  rshill
 Corrected fine and coarse onaxis EA file name handling again.

 Revision 1.33  2016/03/08 23:33:17  rshill
 Corrected fine and coarse onaxis EA file name handling.

 Revision 1.32  2016/03/07 23:40:09  rshill
 Added CALDB queries for on-axis coarse and fine ARFs.
 Deleted an apparently misplaced else-clause.  Raised priority of photon counter progress message.

 Revision 1.31  2016/02/19 00:27:08  rshill
 Take into account -both- DET[XY]FLIP and OPT[XY]FLIP.

 Revision 1.30  2016/02/19 00:20:06  rshill
 Now takes into account rotation of telescope frame with respect to RAW.

 Revision 1.29  2016/02/02 23:37:32  rshill
 Made one correction to group handling; max polynomial fit degree = 5;
 added output messages.

 Revision 1.28  2016/02/02 01:21:48  rshill
 Corrected accounting of GTI time per CAMS time point.
 RAW to ACT transform done with imagetrans routines.

 Revision 1.27  2016/02/01 04:31:12  rshill
 Use map weighting to mitigagte moire pattenrs in subpixel
 sampled ACT output; copy keywords from exposure map file to flat field output.

 Revision 1.26  2016/01/30 00:10:04  rshill
 Corrected centering in image transformations; corrected bug in
 subscripting RAWY in flat field processing; added outmaptype parameter; corrected efficiency and
 exposure map normalization; added WCS keywords to output image.

 Revision 1.25  2016/01/29 17:10:41  rshill
 Implemented vigfile=NONE and qefile=NONE.

 Revision 1.24  2016/01/29 07:38:45  rshill
 Corrected double free in computation of image transform.

 Revision 1.23  2016/01/29 02:51:58  rshill
 Added image transformation for flatfield; corrected the
 if-statement branch target for a ray outside a region.

 Revision 1.22  2016/01/28 20:25:00  rshill
 Corrected upper end of coarse energy grid;  corrected
 small deviations from the TRF in order of statements and in subscripting;  correct
 error of trying to read baffle image from the wrong extension; now adjusts polyorder
 to lower value for fewer then 11 energy grid points; added progress messages to loops.

 Revision 1.21  2016/01/28 03:18:48  rshill
 Corrected baffle radius computation.

 Revision 1.20  2016/01/28 01:45:33  rshill
 Deleted shadowing declaration of effgeomarea;
 fixed 1- vs 0-based subscripting error for array igrpend; moved initialization
 of polycoeff array outside loop; cleaned up some debug messages.

 Revision 1.19  2016/01/27 23:27:06  rshill
 Cleanup of many +++ comments.

 Revision 1.18  2016/01/27 19:45:21  rshill
 Incorporated TY's requested changes of 2016-01-27 to uniquetheta setup loops.

 Revision 1.17  2016/01/27 00:47:32  rshill
 Changed array dimensions and loop limits to avoid overrunning arrays.

 Revision 1.16  2016/01/25 21:20:54  rshill
 Corrected miscellaneous small coding errors.

 Revision 1.15  2016/01/25 05:38:18  rshill
 Fix element order for image writing.

 Revision 1.14  2016/01/25 05:26:27  rshill
 Use code section for not calling imagetrans,
 since that is not yet implemented.

 Revision 1.13  2016/01/25 05:18:46  rshill
 Following TRF reconciliation and revision to trf_arfgen_2016-01-24b.

 Revision 1.12  2016/01/21 21:07:16  rshill
 Many small changes to bring it up to trf_arfgen_2016-01-16.
 Compiles but probably not truly runnable yet.

 Revision 1.11  2016/01/13 22:35:05  rshill
 Added comments to main program variables.

 Revision 1.10  2016/01/13 00:46:08  rshill
 Incorporated changes from TRF trf_arfgen_2016-01-12.docx.
 Builds, not tested.

 Revision 1.9  2016/01/08 22:32:16  rshill
 Corrected order of writing RSP file extensions.

 Revision 1.8  2016/01/08 22:05:06  rshill
 Correct usage of rmflib rowidx.

 Revision 1.7  2016/01/08 02:52:42  rshill
 Corrected errors such as moving to the wrong extension and
 shadowing parameters with local declarations.  Added routine writerspfile.

 Revision 1.6  2016/01/07 03:07:17  rshill
 Added progress messages.  Corrected some FITS file handling.

 Revision 1.5  2016/01/07 01:00:50  rshill
 Corrected erange and corrected QE CALDB resolution.

 Revision 1.4  2016/01/06 20:21:56  rshill
 Changed calls to vignetting functions.

 Revision 1.3  2016/01/04 22:58:54  rshill
 Cleaned up declarations in doWork.

 Revision 1.2  2016/01/04 22:10:29  rshill
 Cleaned variable declarations and scoping; added if-block in initialize
 around computation of inputphotonsperenergy.

 Revision 1.1  2016/01/01 01:20:49  rshill
 Added flatfield code (untested). Renaming ahhxirspgen to hxirspeffimg.

 
 Old Log:
 ahhxirspgen.cxx
 Revision 1.7  2015/12/29 19:49:06  rshill
 Added checkEmptyTable().

 Revision 1.6  2015/12/21 17:48:12  rshill
 Cleaned up units handling in output file.

 Revision 1.5  2015/12/21 17:26:50  rshill
 Changed EBOUNDS CHANNEL to I type to match rmflib (may
 use rmflib itself later).

 Revision 1.4  2015/12/20 04:42:00  rshill
 Corrected compilation errors in writerspmatrix function.

 Revision 1.3  2015/12/19 02:04:57  rshill
 Temporarily added a routine to write the RSP file.  Does not compile yet.

 Revision 1.2  2015/12/18 23:59:44  rshill
 Compiles.  Still lacks a routine to write the output matrix.

 Revision 1.1  2015/12/18 00:40:02  rshill
 Rough implementation - does not compile.

*/
