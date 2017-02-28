// \file ahsxtarfgen.cxx
/// \brief Assign time to interval start and stop times in MXS files
/// \author Mike Dutka
/// \date $Date: 2017/01/13 23:40:07 $
/// \version 1.0

/** 

\defgroup tool_ahsxtargen Creates arf for sxi and sxs
@ingroup mod_mission_tasks

(0) For each satellite pointing bin (i.e. each off-axis angle), we have 
    generated raytracing events for the coarse energy grid. The source spatial 
    distribution will already been taken into account by the raytracing.

(1) Read teldef file; read input RMF file, input ARF files, and set up energy 
    grids

(2) Read exposure map file histogram; calculate normalized histogram; read the 
    first exposure map image

(3) Open the raytracing event file; read data in the 2nd extension and set up 
    the normalization factors.

(4) Set up SAO region structures for each of the region files in telescope 
    coordinates (corresponding to the different off-axis angles).

(5) Read the QE and contamination files; set up arrays of transmission or 
    efficiency (QE, filters, contamination) versus energy and position index 
    (the latter for position-dependent quantities). For the SXS this includes 
    the gate valve geometrical active fractions for each pixel.

(6) Loop over rows in the raytracing event file:

a.     check pathcode of event for double reflection (go to next row if not)

b.     Keep track of photon energy index and off-axis angle index; when the 
       latter changes read a new exposure map image

c.     Check if event lies outside the detector area – go to next row if it does

d.     Check if event lies outside the region area for this off-axis angle – go
       to next row if it does

e.     Update the photon weights array (for photons that successfully make it 
       to the detector inside the extraction region) for this energy and 
       off-axis angle. [The weight accounts for the exposure map value for this
       detector position, the SXS gate valve active fraction, and the SXS pixel
       gaps.

f.     For the landing position on the detector for this event update weights 
       for the position-depdenent transmission functions (of energy).

(7) From the photon weights calculate telescope effective area (EA) on the 
    coarse energy grid, and the ratio of this EA to the pre-calculated (input) 
    coarse-grid on-axis EA.

(8) Fit the above ratio with a 10th order polynomial. Use the fit to calculate 
    the same ratio on the fine-energy grid. Mulitply this by the on-axis 
    fine-grid pre-calculated EA.

(9) Using the weights accumulated for the position-dependent transmission 
    functions, calculated the weighted mean transmission. Multiply by the 
    combined transmission of all position-independent transmission functions.

(10) Multiply the (8) and (9) to get the final, net EA on the fine-energy grid.

(11) Map the final EA in (10) onto the output energy grid.

(12) Write the data in (11) and the energy grid to the output ARF file.


Source files:

  ahsxtarfgen.cxx

Library dependencies:

  hitomi/mission/arfgenlib
  heacore/ahfits
  heacore/ahlog
  heacore/ape
  hitomi/gen/lib/ahapp
  hitomi/gen/lib/ahmath
  hitomi/mission/ahmission
  heacore/heautils/headas_polyfit
  heacore/heautils/headas_utils


Modification history:

  Ver   Date        Author  Description
  1.0   2015-09-11   MSD    New tool ahsxtarfgen

*/
 
#define AHLABEL tool_mxstime
#define AHCVSID "$Id: ahsxtarfgen.cxx,v 1.57 2017/01/13 23:40:07 rshill Exp $"

#define TOOLTAG "$Name: Hitomi_dev $"

#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmath/ahmath.h"
#include "arfgenlib/arfgenlib.h"
#include "headas.h"              // expand_item_list  
#include "headas_utils.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
extern "C" {
#include "headas_polyfit.h" 
}
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <algorithm> 
#include <iostream>
#include <iomanip>  //std::setfill
#include <fstream>
#include <sstream>


//------------------------------------------------------------------------------

//Global constants
const double degrees2radians = M_PI / 180.0;
const double arcmin2radians = M_PI / 10800.0; 
const double radians2arcmin = 3437.7466751209231006;

//------------------------------------------------------------------------------

/// \brief Contains input parameters from .par file
struct Par {
  Par(): m_minphoton(0) {}
  
  std::string m_telescop;       /// < TELESCOP name (value to write in header 
                                /// < keyword for CALDB)
  std::string m_instrume;       /// < SXI, SXS, HXI1, or HXI2
  std::string m_emapfile;       /// < input exposure map filename
  std::string m_qefile;         /// < input quantum efficiency filename
  std::string m_obffile;        /// < input optical blocking filter filename
  std::string m_fwfile;         /// < input filter wheel transmission filename
  std::string m_contamifile;    /// < input contamination filename
  std::string m_abund_str;      /// < string Relative abundances of contaminants
  std::string m_cols_str;       /// < string Additional column densities for 
                                ///   contaminants
  std::string m_covfac_str;     /// < string Partial covering factors for 
                                ///   contaminant materials
  std::vector<double> m_abund;  /// < vector Relative abundances of contaminants
  std::vector<double> m_cols;   /// < vector Additional column densities for 
                                ///   contaminants
  std::vector<double> m_covfac; /// < vector Partial covering factors for 
                                ///   contaminant materials  
  std::string m_gatevalvefile;  /// < input SXS gate valve calibration filename
  std::string m_rmffile;        /// < input rmf file for energy scale
  std::string m_onaxisffile;    /// < input on-axis fine energy-grid ARF 
                                ///   filename
  std::string m_onaxiscfile;    /// < input on-axis coarse energy-grid ARF 
                                ///   filename
  std::string m_polydeg;        /// < Polynomial degree as string (may be "DEFAULT")
  std::string m_outfile;        /// < output ARF or RSP filename
  std::vector<std::string> m_outfiles; /// < output files arf and diagnostic coordinate file
  std::string m_detcoordfile;   /// < detector coordinate output file
  std::string m_regionfile;     /// < region file names or name of list file 
                                ///   (telescope coordinates)
  std::string m_skyregfile;     /// < Sky Region file
  std::vector<std::string> m_regfilelist;  /// < parsed list of region files
  std::string m_xrtevtfile;     /// < Raytracing photon event file
  int m_minphoton;              /// < Minimum number of focal-plane photons per
                                ///   energy for viable ARF
  std::string m_auxtransfile;    /// < Input auxillary transmission file
  int m_numeric_polydeg;         /// < Numeric equivalent of POLYDEG ("DEFAULT" = -1)

};


//------------------------------------------------------------------------------

/// \brief Contains data from the exposure map file
struct emapData {
  emapData(): m_ahffp_emap(0), m_skymap_crval1(0.), m_skymap_crpix1(0),
              m_skymap_cdelt1(0.),  m_skymap_crval2(0.),m_skymap_crpix2(0),
              m_skymap_cdelt2(0.), m_numskyx(0), m_numskyy(0),
              m_skyregionexptime(0), m_skyexpcorrection(0),
              m_doexpomap(true), m_optaxisdetx(0.),
              m_optaxisdety(0.),m_xmmperpixel(0.), m_ymmperpixel(0.),
              m_opt_rotd(0.),m_optxflip(0.),m_optyflip(0.),
              m_sinoptrotd(0.),m_cosoptrotd(0.),
              m_sxspixelgap(0.), m_sxspixelwidth(0.), 
              m_sxspixelactivefraction(0.), m_numexpomaps(0.),
              m_numgrptheta(0),m_obstime(0.), m_tstop(0.), m_exmapxlo(0),
              m_gtitablo(0),m_detmaprefx(0.),
              m_detmaprefxpixel(0),m_detmapdeltax(0.),m_detmaprefy(0.),
              m_detmaprefypixel(0),m_detmapdeltay(0.),m_numdetx(0),m_numdety(0),
              m_emaptotexposure(0.), m_rtnumoffaxisfull(0), m_rtnumoffaxis(0),
              m_detminx(0.), m_detmaxx(0.), m_detminy(0.), m_detmaxy(0.),
              m_azimuthdeg(0.),m_azimuthradians(0.),m_numoffaxis(0),
              m_totaltime(0.) {}
  
  ahfits::FilePtr m_ahffp_emap;    /// < Exposure map file pointer
  
  std::vector < std::vector<double> > m_skyexpimage;  /// < sky exposure map image
  
  // WCS image keywords for the SKY image
  double m_skymap_crval1;
  long m_skymap_crpix1;
  double m_skymap_cdelt1;
  std::string m_skymap_cunit1;
  double m_skymap_crval2;
  long m_skymap_crpix2;
  double m_skymap_cdelt2;
  std::string m_skymap_cunit2;
  
  long m_numskyx; // Number of X bins
  long m_numskyy; // Number of Y bins
  
  double m_skyregionexptime; // value of the above exposure time (in seconds)
  double m_skyexpcorrection; // skyregionexptime/emaptotexposure

  bool m_doexpomap;                  /// < exposure map or a instrument map 
  double m_optaxisdetx;              /// < keyword OPTAXISX  
  double m_optaxisdety;              /// < keyword OPTAXISY
  double m_xmmperpixel;              /// < keyword DET_XSCL size of each x 
                                     ///   pixel in mm
  double m_ymmperpixel;              /// < keyword DET_YSCL size of each y 
                                     ///   pixel in mm
  double m_opt_rotd;                 /// < keyword OPT_ROTD
  double m_optxflip;                 /// < keyword OPTXFLIP
  double m_optyflip;                 /// < keyword OPTYFLIP
  double m_sinoptrotd;               /// < SIN(opt_rotd * degrees2radians)
  double m_cosoptrotd;               /// < COS(opt_rotd * degrees2radians)

  double m_sxspixelgap;              /// < keyword SXSPXGAP width of gap between
                                     ///   pixels sxs instrument
  double m_sxspixelwidth;            /// < keyword SXSPXWID pixel width 
  double m_sxspixelactivefraction;   /// < Active area sxs instrument
  long m_numexpomaps;                /// < keyword NUMEXMAP number of exposure 
                                     ///   maps
  long m_numgrptheta;                /// < number of unique off-axis angles in 
                                     ///   histogram
  double m_obstime;                  /// < keyword TSTART start time of the 
                                     ///   observation
  double m_tstop;                    /// < keyword TSTOP 
  std::string m_filterwheel;         /// < keyword filterwheel specifies state  
                                     ///   of filterwheel
  std::string m_filterwheelsubstr;   /// < first 4 characters of filterwheel 
                                     ///   keyword
  int m_exmapxlo;                    /// < keyword EXMAPXLO *currently not used*
  int m_gtitablo;                    /// < keyword GTITABLO *currently not used*
  std::string m_gatevalv;            /// < GATEVALV keyword

  //x & y dimensions of image parameters in detector coordinates
  double m_detmaprefx;               /// < x dimension detector coordinates 
  double m_detmaprefxpixel;          /// < reference pixel x 
  double m_detmapdeltax;             /// < x scale
  double m_detmaprefy;               /// < y dimension detector coordinates
  double m_detmaprefypixel;          /// < y refeernce pixel
  double m_detmapdeltay;             /// < y scale
 
  long m_numdetx;                    /// < Number of X bins
  long m_numdety;                    /// < Number of y bins
  
  std::vector<double> m_offaxisval; /// < off-axis angle column OFFAXISVAL

  double m_emaptotexposure;         /// < total exposure time in exposure map
  std::vector<double> m_emaptimeintervals; /// < exposure map time intervals
  std::vector<double> m_tfractionratio;    /// < Ratio of expfraction to the 
                                           ///   mean expfraction
  long m_rtnumoffaxisfull;                 /// < Full number of offaxis angles  
  long m_rtnumoffaxis;                     /// < modified number of offaxis 
                                           ///   angels

  std::vector<long> m_numrtpairs;          /// < number of raytracing pairs
  std::vector<long> m_pairindexlo;         /// < pair index low
  std::vector<long> m_pairindexhi;         /// < pair index high
  
  std::vector<double> m_grpexpfrac;        /// < group exposure fraction

  //lower and upper bounds of the detx and dety pixels
  std::vector<double> m_detxlo;     /// < lower x boundary
  std::vector<double> m_detxhi;     /// < upper x boundary
  std::vector<double> m_detylo;     /// < upper y boundary
  std::vector<double> m_detyhi;     /// < upper y boundary
  
  std::vector<double> m_detxhifull; /// < upper x boundary for sxs includes gaps
  std::vector<double> m_detyhifull; /// < upper y boundary for sxs includes gaps
 
  //lower and upper bounds of the detector 
  double m_detminx;                /// < lower detector x boundary
  double m_detmaxx;                /// < upper detector x boundary
  double m_detminy;                /// < lower detector y boundary
  double m_detmaxy;                /// < upper detector y boundary

  std::vector<double> m_offaxisfrac; ///< offaxis fraction
 
  std::vector< std::vector<double> > m_expmapimage;  /// < vector container for
                                                     ///   exposure map image
 
  std::vector< std::vector<long> > m_numimpacts;   /// < recorde if a pixel has been 
                                                     ///   impacted at lesat once
  double m_azimuthdeg;                    /// < *currently not used*
  double m_azimuthradians;                /// < *currently not used*
  long m_numoffaxis;                      /// < number of offaxis elements
  std::string m_detnam;                   /// < keyword DETNAM
  std::vector<double> m_offaxisarcmin;    /// < OFFAXIS column exposure map file
  std::vector<double> m_histotime;        /// < TIME column exposure map file
  double m_totaltime;                     /// < *not used*
  std::vector<double> m_azimuth;          /// < emap azimuth
};

//------------------------------------------------------------------------------

struct evtfileData { 
  evtfileData(): m_geomarea(0.0),m_rtnumrows(0),m_rtnumens(0),
                 m_rtnumazimuth(0),m_evttotnphot(0) {}
 
  ahfits::FilePtr m_ahffp;     /// < ahfits file pointer
  double m_geomarea;           /// < geometric area of detector
  long m_rtnumrows;            /// < number of rows raytracing event file
  long m_rtnumens;             /// < number of energies raytracing event file
  long m_rtnumazimuth;         /// < keyword nazimuth
  long m_evttotnphot;          /// < total number of photons
  std::string m_srcmdl;        /// < Source type in raytracing file
  double m_rt_min_e;           /// < minimum event energy
  double m_rt_max_e;           /// < maximum event energy  
  std::string m_telescop;      /// < TELESCOP keyword 
  std::string m_instrume;      /// < INSTRUME keyword

  long m_evtnumoffaxis;          /// < keyword NOFFAXIS
    
  std::vector< std::vector<double> > m_rtnorms_full;/// < normilization factors 
  std::vector< std::vector<double> > m_rtnorms;     /// < normilization factors 
  std::vector<double> m_evtenergies;                /// < event energies
  std::vector<double> m_rtoffaxis;                  /// < off axis angle of 
                                                    ///   raytracing event file 
  std::vector<double> m_rtoffaxisfull;              /// < off axis angle of 
                                                    ///   raytracing event file 
  std::vector<double> m_cosrtoffaxis;               /// < cosign of rt offaxis
  std::vector<double> m_inputfulloffaxis;           /// < off axis angle of each
                                                    ///   photon
  std::vector<double> m_inputfulloffaxisindex;      /// < theta index 
                                                    ///   corresponding to 
                                                    ///   inputfulloffaxi
  std::vector<double> m_inputgrpoffaxis;            /// < array of unique 
                                                    ///   off-axis angles
  std::vector<long> m_inputgrpoffaxisindex;         /// < index for array of 
                                                    ///   unique off-axis angles

};

//------------------------------------------------------------------------------

/// \brief Obtains input parameters from .par file
/// \param[out] par    structure which contains input parameters
void getPar(Par & par);

//------------------------------------------------------------------------------

/// \brief stroe input data and sets up output data containers
/// \param[in] par           structure which contains input paramters
/// \param[out] doregion     yes/no region correction is performed
/// \param[out] doGatevalve  yes/no gatevalve correction is performed
/// \param[out] getMatrix    yes/no matrix data is obtained
/// \param[out] rmfegrid     energy grid from response file
/// \param[out] rmfmatrix    matrix grid from response file
/// \param[out] kevunits_rmf keV or eV used in response file
/// \param[out] neoutbins    number ofo output energy bins
/// \param[out] eoutlo       lower energy bounds output arf
/// \param[out] eouthi       upper energy bounds output arf
/// \param[out] eoutcen      center energy output arf
/// \param[out] eoutmin      minimum energy output arf
/// \param[out] eoutmax      maximum energy output arf
/// \param[out] numebinscoarse number of energies coarse enegy grid
/// \param[out] ecoarsecen   center energies coase egrid
/// \param[out] coarsearfeffarea coarse effective area
/// \param[out] numebinsfine  numer of energies fine egrid
/// \param[out] efinecen      ncenter energies fine egrid
/// \param[out] finearfeffarea fine efective area
/// \param[out] emap           exposure map input data
/// \param[out] evtData        event file input data
/// \param[out] numregions     number of region files
/// \param[out] xrtregions     region data structures
/// \param[out] obstime        observation time
/// \param[out] numqefunc      numbers of unique transmission functions for QE
/// \param[out] qetransindex QE * OBF*CBF transmission vs. E function index
/// \param[out] qecbftrans      QE * OBF*CBF transmission
/// \param[out] qetrans         QE_OBL curves interpolated onto fine energy grid
/// \param[out] numcontamfunc   Number of unique contamination transmission vs.
///                             E curves
/// \param[out] contamtransindex index unique contamination transmission vs. E 
///                              curve
/// \param[out] contamtrans     Each unique contamination transmission curve
/// \param[out] filterwheel     fitlerwheel keyword
/// \param[out] filterheight    Height of filter in filterwheel
/// \param[out] filterradius    Radius of filter in filterwheel
/// \param[out] gvfracimage     fraction of each pixel that is not obscured by 
///                             the gatevalve
/// \param[out] fwfracimage     fraction of each pixel that is not obscured by 
///                             Fe 55 calibration filter
/// \param[out] fwnx            x-dimension filter structure in the filter wheel
///                             file
/// \param[out] fwny            y-dimension filter structure in the filter 
///                             wheel file
/// \param[out] fwxbnds         Boundary x of pixels in the array describing 
///                             geometry of the filter
/// \param[out] fwybnds         Boundary y of pixels in the array describing 
///                             geometry of the filter
/// \param[out] fwgeoimage      The image array describing geometry of the 
///                             filter in the filter wheel file
/// \param[out] trantype        file file is type 1 (simple transmission) or 
///                             type 2 (complex) 
void initialize(Par par, bool & doRegion, bool & doGatevalve, bool & getMatrix,
                arfgenlib::s_rmfegrid & rmfegrid, 
                arfgenlib::s_rmfmatrix & rmfmatrix, bool & kevunits_rmf, 
                long & numeoutbins, std::vector<double> & eoutlo, 
                std::vector<double> & eouthi, std::vector<double> & eoutcen,
                double & eoutmin, double & eoutmax, std::vector<double> & eoutkev, 
                long & numebinscoarse,std::vector<double> & ecoarsecen, 
                std::vector<double> & coarsearfeffarea, 
                long & numebinsfine, std::vector<double> & efinecen, 
                std::vector<double> & finearfeffarea, emapData & emap, 
                evtfileData & evtData, long & numregions, long & numregphotons,  
                std::vector<SAORegion*> & xrtregions,double & obstime,
                long & numqefunc, 
                std::vector< std::vector<long> > & qetransindex,
                std::vector< std::vector<double> > & qecbftrans,
                std::vector< std::vector<double> > & qetrans,
                long & numcontamfunc, 
                std::vector< std::vector<long> > & contamtransindex,
                std::vector< std::vector<double> > & contamtrans, 
                std::string & filterwheel,
                double & filterheight, double & filterradius, 
                std::vector< std::vector<double> > & gvfracimage, 
                std::vector< std::vector<double> > & fwfracimage, 
                long & fwnx, long & fwny, std::vector<double> & fwx, 
                std::vector<double> & fwy, std::vector<double> & fwxbnds, 
                std::vector<double> & fwybnds,
                std::vector< std::vector<double> > & fwgeoimage, 
                int & trantype,std::vector<double> & outauxtrans);

//------------------------------------------------------------------------------

/// \brief main routine which determine the final effective area
/// \param[in] par           structure which contains input paramters
/// \param[in] doregion     yes/no region correction is performed
/// \param[in] doGatevalve  yes/no gatevalve correction is performed
/// \param[in] getMatrix    yes/no matrix data is obtained
/// \param[in] rmfegrid     energy grid from response file
/// \param[in] rmfmatrix    matrix grid from response file
/// \param[in] kevunits_rmf keV or eV used in response file
/// \param[in] neoutbins    number ofo output energy bins
/// \param[in] eoutlo       lower energy bounds output arf
/// \param[in] eouthi       upper energy bounds output arf
/// \param[in] eoutcen      center energy output arf
/// \param[in] eoutmin      minimum energy output arf
/// \param[in] eoutmax      maximum energy output arf
/// \param[in] numebinscoarse number of energies coarse enegy grid
/// \param[in] ecoarsecen   center energies coase egrid
/// \param[in] coarsearfeffarea coarse effective area
/// \param[in] numebinsfine  numer of energies fine egrid
/// \param[in] efinecen      ncenter energies fine egrid
/// \param[in] finearfeffarea fine efective area
/// \param[in] emap           exposure map input data
/// \param[in] evtData        event file input data
/// \param[in] numregions     number of region files
/// \param[in] xrtregions     region data structures
/// \param[in] obstime        observation time
/// \param[in] numqefunc      numbers of unique transmission functions for QE
/// \param[in] qetransindex QE * OBF*CBF transmission vs. E function index
/// \param[in] qecbftrans      QE * OBF*CBF transmission
/// \param[in] qetrans         QE_OBL curves interpolated onto fine energy grid
/// \param[in] numcontamfunc   Number of unique contamination transmission vs. 
///                            E curves
/// \param[in] contamtransindex index unique contamination transmission vs. E 
///                             curve
/// \param[in] contamtrans     Each unique contamination transmission curve
/// \param[in] filterwheel     fitlerwheel keyword
/// \param[in] filterheight    Height of filter in filterwheel
/// \param[in] filterradius    Radius of filter in filterwheel
/// \param[in] gvfracimage     fraction of each pixel that is not obscured by 
///                            the gatevalve
/// \param[in] fwfracimage     fraction of each pixel that is not obscured by 
///                            Fe 55 calibration filter
/// \param[in] fwnx            x-dimension filter structure in the filter wheel
///                            file
/// \param[in] fwny            y-dimension filter structure in the filter wheel
///                            file
/// \param[in] fwxbnds         Boundary x of pixels in the array describing 
///                            geometry of the filter
/// \param[in] fwybnds         Boundary y of pixels in the array describing 
///                            geometry of the filter
/// \param[in] fwgeoimage      The image array describing geometry of the filter
///                            in the filter wheel file
/// \param[in] trantype        file file is type 1 (simple transmission) or 
///                            type 2 (complex) 
/// \param[out] outputarfarray final output tranmission
void doWork(Par par, bool & doRegion, bool & doGatevalve, bool & getMatrix,
            bool & kevunits_rmf, long & numeoutbins, 
            std::vector<double> & eoutlo, 
            std::vector<double> & eouthi, std::vector<double> & eoutcen,
            double & eoutmin, double & eoutmax, std::vector<double> & eoutkev,
            long & numebinscoarse,
            std::vector<double> & ecoarsecen, 
            std::vector<double> & coarsearfeffarea, long & numebinsfine, 
            std::vector<double> & efinecen, 
            std::vector<double> & finearfeffarea, 
            emapData & emap, evtfileData & evtData, long & numregions, 
            long & numregphotons, 
            std::vector<SAORegion*> & xrtregions,double & obstime,
            long & numqefunc,std::vector< std::vector<long> > & qetransindex,
            std::vector< std::vector<double> > & qecbftrans,
            std::vector< std::vector<double> > & qetrans,
            long & numcontamfunc, 
            std::vector< std::vector<long> > & contamtransindex,
            std::vector< std::vector<double> > & contamtrans,
            std::string & filterwheel, double & filterheight, 
            double & filterradius, 
            std::vector< std::vector<double> > & gvfracimage, 
            std::vector< std::vector<double> > & fwfracimage, 
            long & fwnx, long & fwny, std::vector<double> & fwx, 
            std::vector<double> & fwy, std::vector<double> & fwxbnds, 
            std::vector<double> & fwybnds,
            std::vector< std::vector<double> > & fwgeoimage, 
            int & trantype, std::vector<double> & outputarfarray,
            bool & insufficientphotons,std::vector<long> & photonsperenergy,
            std::vector<double> & outauxtrans);
  
//------------------------------------------------------------------------------

/// \brief writes arf file an close all open files
/// \param[in] emap           exposure map input data
/// \param[in] evtData        event file input data
/// \param[in] par           structure which contains input paramters
/// \param[in] neoutbins    number ofo output energy bins
/// \param[in] eoutlo       lower energy bounds output arf
/// \param[in] eouthi       upper energy bounds output arf
/// \param[in] outputarfarray  final output tranmission
/// \param[in] kevunits_rmf keV or eV used in response file
void finalize(emapData & emap, evtfileData & evtData, Par & par, 
              arfgenlib::arfkeywords & arfkey, long & numeoutbins, 
              std::vector<double> & eoutlo, std::vector<double> & eouthi, 
              std::vector<double> & outputarfarray, bool & kevunits_rmf,
              bool & insufficientphotons, int status);

//------------------------------------------------------------------------------

/// \brief loads exposure map file data
/// \param[in] instume    instrume keyword
/// \param[in] emapfile   exposuremap file name
/// \param[out] emap      exposure map data
/// \param[in] doGatevalve yes/no gatevalve correction is performed
/// \param[in] gatevalvefile gatevalve file name
void processEmap(std::string instrume, std::string telescop, std::string skyregfile,
                 std::string & emapfile, emapData & emap, bool & doGatevalve,
                 std::string & fwfile, std::string gatevalvefile);

//------------------------------------------------------------------------------

/// \brief load event file data
/// \param[in] xrtevtfile  event file name
/// \param[in] numebinscoarsefull number of coarse energy bins from file
/// \param[in] ecoarsecenfull center energies coase energy grid from file
/// \param[in] coarsearfeffareafull coase effective area from file
/// \param[in] emapnumoffaxis number of offaxis angles in exposure map
/// \param[out] evtData  event file data structure
/// \param[out] numebinscoarse matched num grid points with raytracing file
/// \param[out] ecoarsecen matched center energies with raytracing file
/// \param[out] coarsearfeffarea matched coarse grid with raytracing file
void processEvtfile(std::string xrtevtfile, bool & doRegion, long & numregphotons, long & rtnumoffaxisfull, 
                    long & rtnumoffaxis, long & numebinscoarsefull, 
                    std::vector<double> & ecoarsecenfull,
                    std::vector<double> & coarsearfeffareafull, 
                    evtfileData & evtData, long & numebinscoarse,
                    std::vector<double> & ecoarsecen, 
                    std::vector<double> & coarsearfeffarea);

//------------------------------------------------------------------------------
/// \brief load event file data
/// \param[in] ecoarsecen         coarse energy grid
/// \param[in] coarseEffareaRatio ratio of eff area to on-axis eff area
/// \param[in] efinecen           fine energy grid
/// \param[in] fineEffareaRatioInterp  eff area ratio on fine grid, linear interp 
/// \param[in] numebinscoarse     number of coarse grid points
/// \param[in] numebinsfine       number of fine grid points
/// \param[in] maxpolyorder       maximum polynomial order to be tested
/// \param[out] bestpolyorder     selected polynomil order
void optimizePolyOrder(
  std::vector<double> & ecoarsecen, 
  std::vector<double> & coarseEffareaRatio, 
  std::vector<double> & efinecen, 
  std::vector<double> & fineEffareaRatioInterp, 
  double energy_min, double energy_max,
  long numebinscoarse, long numebinsfine, 
  int maxpolyorder, int & bestpolyorder);

//------------------------------------------------------------------------------

/// \brief interpolate a return value for an input value, based on arrays of 
///        input and output
/// \param[in] numGridPts Size of the input arrays. Specified explicitly in the 
///               interest of run-time speed so that the routine does not have 
///               to deduce the array size each time.
/// \param[in] xGrid Array of 'x' values
/// \param[in] yGrid Array of corresponding 'y' values
/// \param[in] xIn The 'x' value for which we would like to find a 
///               corresponding 'y'
/// \param[out] yOut The interpolated value
///
/// Linearly interpolate on an input grid of values, using bisection to quickly 
/// locate the two interpolation positions.
/// Note that the routine does no range checking
//void bisectionInterp(long numGridPts, 
//                     const std::vector<double> & xGrid, 
//                     const std::vector<double> & yGrid, 
//                     double xIn, double & yOut);

//------------------------------------------------------------------------------

/// \brief Locate the pair of grid points that enclose the input value
/// \param[in] numGridPts Size of the input arrays. Specified explicitly in the 
///               interest of run-time speed so that the routine does not have 
///               to deduce the array size each time.
/// \param[in] xGrid Array of 'x' values
/// \param[in] xIn The 'x' value for which we would like to find a 
///               corresponding 'y'
/// \param[out] index1 index pointing to the array element in xGrid that is 
///               closest to xIn but is less than xin
/// \param[out] index2 index pointing to the array element in xGrid that is 
///               closest to xIn but is greater than xin
///
/// Given an array of x values (size numgridpts, values in gridx) and an input 
/// value, xin, locate the pair of grid points that enclose the input value. 
/// The outputs are two indices, xindex1 and xindex2 which point to the array 
/// elements in gridx (the 1st value in gridx has an index of 0).  Although 
/// xindex2=xindex1+ 1 always, both indices are returned for convenience (also, 
/// special treatment is required when xin is near or equal to one of the end 
/// points of gridx)
/// Note that the routine does no range checking
//void bisectionLocate(const std::vector<double> & xGrid, 
//                     long numGridPts, 
//                     double xin, 
//                     int & index1, 
//                     int & index2);

//------------------------------------------------------------------------------

/// \brief returns a string version of an double
/// \param[in] value the double that needs to be a string
/// \return a string version of the double
std::string doubleToString(double value);

//------------------------------------------------------------------------------

int main(int argc, char** argv) {

  Par par;         // Structure containing input parameter values
  bool doRegion = false;   // Process region files? (YES/NO)
  bool doGatevalve = false; // Process gate valve file? (YES/NO)
  bool getMatrix = false;  // Get Matrix extension of rmf file
  // Rmfproc inputs/outputs
  arfgenlib::s_rmfegrid rmfegrid;  // Struct containing rmf energy grid
  arfgenlib::s_rmfmatrix rmfmatrix; // Struct containing rmf rmfmatrix
  bool kevunits_rmf = false;  // Is the RMF file in keV units (YES/NO)
  long numeoutbins = 0; // Number of output energy bins (grid from RMF[EBOUNDS])
  std::vector<double> eoutlo; // Low edge of output energy bins
  std::vector<double> eouthi;  // Hi edge of output energy bins
  std::vector<double> eoutcen; // Center of output energy bins
  std::vector<double> eoutkev;             // output energy grid
  double eoutmin = 0.0;  // Minimum output energy (center bins)
  double eoutmax = 0.0;  // Maximum output energy (center bins)
  // Getarfdata inputs/outputs
  std::vector<double>  arf_energ_lo;  // Low edge input arf energy bins 
  std::vector<double>  arf_energ_hi;  // Hi edge input arf energy bins
  std::vector<double>  arf_ecenter;   // Center input arf energy bins
  std::vector<double>  arfeffarea;  // Effective area from input arf
  // Geteafile inputs/outputs
  long numebinscoarse = 0;      // Number of energy bins coarse energy grid
  std::vector<double> ecoarsecen; // Coarse energy center of bin 
  std::vector<double> coarsearfeffarea; // Coarse arf effective area
  long numebinsfine = 0; // Number of energy bins fine
  std::vector<double> efinecen; // Center of energy bin fine
  std::vector<double> finearfeffarea; // Fine arf effective area
  //Write arffile inputs
  arfgenlib::arfkeywords arfkeywords; // Struct containing output arf keywords 
  std::vector<double> outputarfarray; // Output effective area
  
  //region files paramters
  long numregions = 0;     // Number of input region file (DET Coordinates)
  std::vector<SAORegion*> xrtregions; // Vector of det region data structs 
                                      // (These are read by fits in region)
  
  emapData emap;            // Struct containing exposure map data
  evtfileData evtData;      // Struct containing event file data

  double obstime = 0.;      // OBSTIME keyword
  std::vector< std::vector<double> > qetrans;  // Quantum efficiency component 
                                               // of transmission
  long numqefunc = 0;         // Number of qe entries

  std::vector< std::vector<long> > qetransindex;  // Energy index of qe 
                                                  // functions
  std::vector< std::vector<double> >  qecbftrans; // QE which includes the 
                                                  // blocking filter
  long numcontamfunc = 0;                  // Contimination function entries
  std::vector< std::vector<long> > contamtransindex; // Contamination energy 
                                                     // indicies
  std::vector< std::vector<double> >  contamtrans; // Contamination transmission

  std::string filterwheel;  // Filterwheel keyword
  // Filter dimensions
  double filterheight = 0; // Height of the filter 
  double filterradius = 0; // radius of the fitler

  std::vector< std::vector<double> > gvfracimage; // ratio of SXS pixel 
                                                  // countrate with gatevalve 
                                                  // to that without. 
  std::vector< std::vector<double> > fwfracimage; // ratio of SXS pixel 
                                                  // countrate with filter in 
                                                  // place to that without.
 
  long numregphotons = 0;                         // number of photons within input regions

  std::vector<double> outauxtrans;                // auxillery transmission 

  // x and y arrays associated with fwgeoimage
  long fwnx = 0;  
  long fwny = 0;
  std::vector<double> fwx; 
  std::vector<double> fwy; 
  std::vector<double> fwxbnds; 
  std::vector<double> fwybnds;

  std::vector< std::vector<double> > fwgeoimage;  // Filter wheel geometric 
                                                  // image
  int trantype = 0;  // 1 for type 1 transmission file, =2 for type 2 filter 
                     // transmission file
 
  arfgenlib::arfkeywords arfkey;  // Struct containing output arf keywords

  // Is the min number of photons in an 
  // energy bin less then parameter minphoton 
  bool insufficientphotons=false;

  std::vector<long> photonsperenergy;

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
     
      getPar(par);
      // Write all parameters to the log file.  When debug=yes, the params will 
      // be written twice: here, and again at the end of initialize.  That is 
      // to insure against a possible failure in initialize().
      ahapp::writeParametersToLog();
      initialize(par,doRegion,doGatevalve,getMatrix,rmfegrid,rmfmatrix,
                 kevunits_rmf,numeoutbins,eoutlo,eouthi,eoutcen,eoutmin,
                 eoutmax,eoutkev,numebinscoarse,ecoarsecen,coarsearfeffarea,
                 numebinsfine,efinecen,finearfeffarea,emap,evtData,numregions,
                 numregphotons,
                 xrtregions,obstime,numqefunc,qetransindex,
                 qecbftrans,qetrans,numcontamfunc,contamtransindex,
                 contamtrans,filterwheel,filterheight,filterradius, 
                 gvfracimage,fwfracimage,fwnx,fwny,fwx, 
                 fwy,fwxbnds,fwybnds,fwgeoimage,trantype,outauxtrans);
      doWork(par,doRegion,doGatevalve,getMatrix,kevunits_rmf,numeoutbins,eoutlo,
             eouthi,eoutcen,eoutmin,eoutmax,eoutkev,numebinscoarse,ecoarsecen,
             coarsearfeffarea,numebinsfine,efinecen,finearfeffarea,emap, 
             evtData,numregions,numregphotons,xrtregions,obstime,numqefunc,
             qetransindex, 
             qecbftrans,qetrans,numcontamfunc,contamtransindex,contamtrans,
             filterwheel,filterheight,filterradius,gvfracimage,fwfracimage,fwnx,
             fwny,fwx,fwy,fwxbnds,fwybnds,fwgeoimage,trantype,outputarfarray,
             insufficientphotons, photonsperenergy,outauxtrans);
             finalize(emap,evtData,par,arfkey,numeoutbins,eoutlo,eouthi, 
                      outputarfarray,kevunits_rmf,insufficientphotons,
                      status);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,doRegion,doGatevalve,getMatrix,rmfegrid,rmfmatrix,
                   kevunits_rmf,numeoutbins,eoutlo,eouthi,eoutcen,eoutmin,
                   eoutmax,eoutkev,numebinscoarse,ecoarsecen,coarsearfeffarea,
                   numebinsfine,efinecen,finearfeffarea,emap,evtData,numregions,
                   numregphotons,
                   xrtregions,obstime,numqefunc,qetransindex,
                   qecbftrans,qetrans,numcontamfunc,contamtransindex,
                   contamtrans,filterwheel,filterheight,filterradius, 
                   gvfracimage,fwfracimage,fwnx,fwny,fwx, 
                   fwy,fwxbnds,fwybnds,fwgeoimage,trantype,outauxtrans);
        doWork(par,doRegion,doGatevalve,getMatrix,kevunits_rmf,numeoutbins,
               eoutlo,eouthi,eoutcen,eoutmin,eoutmax,eoutkev,numebinscoarse,
               ecoarsecen,coarsearfeffarea,numebinsfine,efinecen,finearfeffarea,
               emap,evtData,numregions,numregphotons,xrtregions,obstime,
               numqefunc,
               qetransindex,qecbftrans,qetrans,numcontamfunc,contamtransindex,
               contamtrans,filterwheel,filterheight,filterradius,gvfracimage,
               fwfracimage,fwnx,fwny,fwx,fwy,fwxbnds,fwybnds,fwgeoimage,
               trantype,outputarfarray,insufficientphotons,photonsperenergy,outauxtrans);
      } catch (const std::exception &x) {
        // Write all parameters to the log file.  This may be the second time 
        // they are written (the first was at the end of initialize()), just 
        // in case there was a failure in initialize() or doWork().
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(emap,evtData,par,arfkey,numeoutbins,eoutlo,eouthi, 
                 outputarfarray,kevunits_rmf,insufficientphotons,
                 status);
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
  
//------------------------------------------------------------------------------

void getPar(Par & par) {
 
  par.m_telescop=ahapp::getParString("telescop");
  par.m_instrume=ahapp::getParString("instrume");
  par.m_emapfile=ahapp::getParString("emapfile");
  par.m_qefile=ahapp::getParString("qefile");
  par.m_obffile=ahapp::getParString("obffile");
  par.m_fwfile=ahapp::getParString("fwfile");
  par.m_contamifile=ahapp::getParString("contamifile");
  par.m_abund_str=ahapp::getParString("abund");
  par.m_cols_str=ahapp::getParString("cols");
  par.m_covfac_str=ahapp::getParString("covfac");
  par.m_gatevalvefile=ahapp::getParString("gatevalvefile");
  par.m_rmffile=ahapp::getParString("rmffile");
  par.m_onaxisffile=ahapp::getParString("onaxisffile");
  par.m_onaxiscfile=ahapp::getParString("onaxiscfile");
  par.m_polydeg=ahapp::getParString("polydeg");
  par.m_outfile=ahapp::getParString("outfile");
  par.m_regionfile=ahapp::getParString("regionfile");
  par.m_skyregfile=ahapp::getParString("skyregfile");
  par.m_xrtevtfile=ahapp::getParString("xrtevtfile");
  par.m_minphoton=ahapp::getParInt("minphoton");
  par.m_auxtransfile =ahapp::getParString("auxtransfile");

  //covert string lists to vectors
  arfgenlib::listStringsToDoubles(par.m_abund_str, par.m_abund);
  arfgenlib::listStringsToDoubles(par.m_cols_str, par.m_cols);
  arfgenlib::listStringsToDoubles(par.m_covfac_str, par.m_covfac);

  // Parse outfile parameter, comma separated list 
  char** items=0;         // output list
  int nitems=0;           // number of items found
  int status=0;           // output status of expand_item_list
  int trim=1;             // trim spaces
  int skip=1;             // exclude empty items
  int guard=0;            // do not protect against commas in parentheses
  items=expand_item_list((char*)par.m_outfile.c_str(),&nitems,',',trim, skip,guard,&status);
  if (status != 0) {
    AH_THROW_RUNTIME("invalid value for outfile parameter; expect single file, or list of values separated by commas or spaces");
  }
  for (int ii = 0; ii < nitems; ++ii) {
    std::stringstream tmp;
    tmp << items[ii];
    par.m_outfiles.push_back(tmp.str());
  }   

  // Check if outfile parameter was given as space deliminated
  if (par.m_outfiles.size() == 1) {
    // Parse outfile parameter, space separated list 
    items=0;            // output list
    nitems=0;           // number of items found
    status=0;           // output status of expand_item_list
    trim=1;             // trim spaces
    skip=1;             // exclude empty items
    guard=0;            // do not protect against commas in parentheses
    items=expand_item_list((char*)par.m_outfile.c_str(),&nitems,' ',trim, skip,guard,&status);
    if (status != 0) {
      AH_THROW_RUNTIME("invalid value for outfile parameter; expect single file, or list of values separated by commas or spaces");
    }
    // Check is nitems is 2 if it is, outfile parameter is space deliminated
    // so the oufiles vector can be empties and the set to the vaules in 
    // the space deliminated outfile parameter    
    if (nitems >= 2) { 
      par.m_outfiles.clear(); 
      for (int ii = 0; ii < nitems; ++ii) {
        std::stringstream tmp;
        tmp << items[ii];
        par.m_outfiles.push_back(tmp.str());
      }
    }
  }      

  
  //Set the names of outfile and detector coordinates file 

  if (par.m_outfiles.size() == 2) {
    par.m_outfile = par.m_outfiles[0];
    par.m_detcoordfile = par.m_outfiles[1];
  } else if (par.m_outfiles.size() == 1) {
    par.m_outfile = par.m_outfiles[0];
    par.m_detcoordfile = "NONE";
  } else { 
    AH_THROW_RUNTIME("The outfile parameter can only have 1 or 2 files listed. The output arf (neccessary) and the detector coordinates file (optional) \n");
  }
  
 
  

  //Search CALDB for correct files if CALDB is givin instead of file name
  par.m_auxtransfile = ahmission::caldb::resolve(par.m_auxtransfile,
                                                 "auxillery transmission",
                                                 par.m_instrume,"-","AUXTRAN","-","-",
                                                 par.m_telescop);
  ape_trad_set_string("auxtransfile",par.m_auxtransfile.c_str());

  if (ahgen::strtoupper(par.m_instrume) == "SXI") {
    par.m_emapfile = ahmission::caldb::resolve(par.m_emapfile,"instrument map",
                                               par.m_instrume,"-","INSTMAP_DET",
                                               "-","-",par.m_telescop);
    ape_trad_set_string("emapfile", par.m_emapfile.c_str());

    par.m_qefile = ahmission::caldb::resolve(par.m_qefile,"quantum efficiency",
                                             par.m_instrume,"-","QE_OBL","-","-"
                                             ,par.m_telescop);
    ape_trad_set_string("qefile", par.m_qefile.c_str());

    par.m_contamifile = ahmission::caldb::resolve(par.m_contamifile,
                                                  "contamination",
                                                  par.m_instrume,"-",
                                                  "COLUMN_DENSITY","-","-",
                                                  par.m_telescop);
    ape_trad_set_string("contamifile", par.m_contamifile.c_str());
  
    par.m_rmffile = ahmission::caldb::resolve(par.m_rmffile,"response matrix",
                                              par.m_instrume,"-","MATRIX","-",
                                              "-",par.m_telescop);
    ape_trad_set_string("rmffile", par.m_rmffile.c_str());
  
    par.m_onaxisffile = ahmission::caldb::resolve(par.m_onaxisffile,
                                                  "fine grid effective area",
                                                  par.m_instrume,"-",
                                                  "EFFAREAFNE","-",
                                                  "-",par.m_telescop);
    ape_trad_set_string("onaxisffile", par.m_onaxisffile.c_str());
    
    par.m_onaxiscfile = ahmission::caldb::resolve(par.m_onaxiscfile,
                                                  "coarse grid effective area",
                                                  par.m_instrume,"-",
                                                  "EFFAREACRS","-",
                                                  "-",par.m_telescop);
    ape_trad_set_string("onaxiscfile", par.m_onaxiscfile.c_str());

  }

  if (ahgen::strtoupper(par.m_instrume) == "SXS") {  
    par.m_emapfile = ahmission::caldb::resolve(par.m_emapfile,"instrument map",
                                               par.m_instrume,"-","INSTMAP_DET"
                                               ,"-","-",par.m_telescop);
    ape_trad_set_string("emapfile", par.m_emapfile.c_str());
 
    par.m_qefile = ahmission::caldb::resolve(par.m_qefile,"quantum efficiency",
                                             par.m_instrume,"-","QE","-","-",
                                             par.m_telescop);
    ape_trad_set_string("qefile", par.m_qefile.c_str());

    par.m_obffile = ahmission::caldb::resolve(par.m_obffile,"blocking filter",
                                              par.m_instrume,"-","TRANSBLOCKING"
                                              ,"-","-",par.m_telescop);
    ape_trad_set_string("obffile",par.m_obffile.c_str());
  
    par.m_contamifile = ahmission::caldb::resolve(par.m_contamifile,
                                                  "contamination",
                                                  par.m_instrume,"-",
                                                  "COLUMN_DENSITY","-","-",
                                                  par.m_telescop);
    ape_trad_set_string("contamifile", par.m_contamifile.c_str());
 
    par.m_gatevalvefile = ahmission::caldb::resolve(par.m_gatevalvefile,
                                                    "gatevalve",par.m_instrume,
                                                    "-","GATEVALVTRANS","-","-"
                                                    ,par.m_telescop);
    ape_trad_set_string("gatevalvefile", par.m_gatevalvefile.c_str());
   
    par.m_rmffile = ahmission::caldb::resolve(par.m_rmffile,"response matrix",
                                              par.m_instrume,"-","MATRIX","-",
                                              "-",par.m_telescop);
    ape_trad_set_string("rmffile", par.m_rmffile.c_str());

    par.m_onaxisffile = ahmission::caldb::resolve(par.m_onaxisffile,
                                                  "fine grid effective area",
                                                  par.m_instrume,"-",
                                                  "EFFAREAFNE","-",
                                                  "-",par.m_telescop);
    ape_trad_set_string("onaxisffile", par.m_onaxisffile.c_str());

    par.m_onaxiscfile = ahmission::caldb::resolve(par.m_onaxiscfile,
                                                  "coarse grid effective area",
                                                  par.m_instrume,"-",
                                                  "EFFAREACRS","-",
                                                  "-",par.m_telescop);
    ape_trad_set_string("onaxiscfile", par.m_onaxiscfile.c_str());

  }
 
  // Convert POLYDEG to number
  if (ahgen::strtoupper(par.m_polydeg) == "DEFAULT") {
    par.m_numeric_polydeg = -1;
  } else {
    std::stringstream tmp;
    tmp.str(par.m_polydeg);
    tmp >> par.m_numeric_polydeg;
  }
 
}

//------------------------------------------------------------------------------

void initialize(Par par, bool & doRegion, bool & doGatevalve, bool & getMatrix,
                arfgenlib::s_rmfegrid & rmfegrid, 
                arfgenlib::s_rmfmatrix & rmfmatrix, bool & kevunits_rmf, 
                long & numeoutbins, std::vector<double> & eoutlo, 
                std::vector<double> & eouthi, std::vector<double> & eoutcen,
                double & eoutmin, double & eoutmax,std::vector<double> & eoutkev, 
                long & numebinscoarse,std::vector<double> & ecoarsecen, 
                std::vector<double> & coarsearfeffarea, 
                long & numebinsfine, std::vector<double> & efinecen, 
                std::vector<double> & finearfeffarea, emapData & emap, 
                evtfileData & evtData, long & numregions,long & numregphotons, 
                std::vector<SAORegion*> & xrtregions,double & obstime,
                long & numqefunc, 
                std::vector< std::vector<long> > & qetransindex,
                std::vector< std::vector<double> > & qecbftrans,
                std::vector< std::vector<double> > & qetrans,
                long & numcontamfunc, 
                std::vector< std::vector<long> > & contamtransindex,
                std::vector< std::vector<double> > & contamtrans,
                std::string & filterwheel,double & filterheight, 
                double & filterradius, 
                std::vector< std::vector<double> > & gvfracimage, 
                std::vector< std::vector<double> > & fwfracimage, 
                long & fwnx, long & fwny, std::vector<double> & fwx, 
                std::vector<double> & fwy, std::vector<double> & fwxbnds, 
                std::vector<double> & fwybnds,
                std::vector< std::vector<double> > & fwgeoimage, 
                int & trantype,std::vector<double> & outauxtrans){
 
 

  //Full coarse energy grid paramters
  long numebinscoarsefull = 0;   // number of bins raw coarse energy grid 
  std::vector<double> ecoarsecenfull;   // raw coarse energy grid  
  std::vector<double> coarsearfeffareafull; // coarse effective area

  // File pointer for auxtransfile
  ahfits::FilePtr ahffp_auxtransfile = 0;

  // Varibles related to the auxtransfile
  long numaxtenergies = 0;                      // keyword NAXIS2   
  std::string axtfileunits = "";                // keyword TUNIT1 (units of column "ENERGY") 
  double axtconvenergy = 1.0;                   // auxilery transmission conversion factor for energy
  std::vector<double> axtenergieskev;           // auxtrans energy in kex
  std::vector<double> axtenergy;                // column "ENERGY" auxtransfile
  std::vector<double> auxtrans;                 // column "TRANSMISSION" auxtranfile
  double newtrans = 0.;                         // transmission after interpolation

  doRegion = true;
 

  std::string tmp_region;
  if (par.m_regionfile[0] == '@') {
    tmp_region = par.m_regionfile.substr(1);
  } else {
    tmp_region = par.m_regionfile;
  }

  if (ahgen::strtoupper(par.m_regionfile) == "NONE") {
    doRegion = false;
  } else if (!ahgen::fileExists(tmp_region)) {
    AH_THROW_RUNTIME("The specified region file (or list of regions files) does not exists, please check the filename or set regionfile = NONE if you do not intend to use one");
  }
  
 
  doGatevalve = false;
 
  //--------------------------GET RMF ENERGY GRID------------------------------
  //Get energy  grid from input RMF file; NOTE: this will also be the energy 
  //grid for the output ARF file
  
  //Set getMatrix to false
  getMatrix = false; 
   
  arfgenlib::rmfproc(par.m_rmffile, getMatrix, rmfegrid, rmfmatrix, 
                     kevunits_rmf);

  // Outputs needed from rmfproc: rmfegrid.m_energ_lo, rmfegrid.m_energ_hi, 
  // rmfegid.m_ecenter 
  // (center energies of the bin intervals)
  numeoutbins = rmfegrid.m_nebin;
  eoutlo = rmfegrid.m_energ_lo;
  eouthi = rmfegrid.m_energ_hi;
  eoutcen = rmfegrid.m_ecenter;
  // min and max energies
  eoutmin = *std::min_element(eoutcen.begin(),eoutcen.end());
  eoutmax = *std::max_element(eoutcen.begin(),eoutcen.end());


  //Create a mirror output energy grid (for interpolation) that is in units of 
  //keV regardless of the energy units in the rmf
  eoutkev.resize(numeoutbins,0.0);
  if (kevunits_rmf) {
    for (int ii=0; ii<numeoutbins; ++ii) {
      eoutkev[ii] = eoutcen[ii];
    }
  } else {
    for (int ii=0; ii<numeoutbins; ++ii) {
     eoutkev[ii] = eoutcen[ii]/1000.0;
    }  
  }
  
  // Get auxiliary transmission array from input file auxtransfile and remap
  // it onto the output energy grid.  If the units of the energy column
  // in auxtransfile are eV then convert to keV.

  // The auxiliary transmission array is initialized to 1.0 for all energies.
  // If the energy grid in auxtransfile does not cover the whole range of
  // tne output energy grid, then those parts not covered will have
  // values of 1.0.

  
  outauxtrans.resize(numeoutbins);
  for (long i=0; i<numeoutbins; ++i) {
    outauxtrans[i] = 1.0; 
  }

  // Open auxtransfile (input parameter) if it exists.
  if (ahgen::strtoupper(par.m_auxtransfile) !="NONE") {
    ahfits::open(par.m_auxtransfile,"TRANSMISSION",&ahffp_auxtransfile);
    numaxtenergies = ahfits::getKeyValLLong(ahffp_auxtransfile,"NAXIS2");
    axtfileunits = ahfits::getKeyValStr(ahffp_auxtransfile,"TUNIT1"); 
    axtconvenergy = 1.0; 
    if (ahgen::strtoupper(axtfileunits) == "EV") { 
      axtconvenergy = 0.001; 
    }                                                                
    ahfits::Router router_auxtran(ahffp_auxtransfile);
    double l_auxtrans_energy = 0.;
    double l_auxtrans_transmission = 0.;
    router_auxtran.connectScalar(ahfits::e_READONLY,"ENERGY",l_auxtrans_energy);
    router_auxtran.connectScalar(ahfits::e_READONLY,"TRANSMISSION",l_auxtrans_transmission);
    for (ahfits::firstRow(ahffp_auxtransfile); ahfits::readOK(ahffp_auxtransfile);
         ahfits::nextRow(ahffp_auxtransfile)) {
      ahfits::readRow(ahffp_auxtransfile);
      axtenergy.push_back(l_auxtrans_energy);
      axtenergieskev.push_back(l_auxtrans_energy * axtconvenergy); 
      auxtrans.push_back(l_auxtrans_transmission);  
    }
    // Interpolate the file transmission array onto the output 
    // transmission array.                                           
    for (long i=0; i<numeoutbins; ++i) {
      if (eoutkev[i] >= axtenergieskev[0] && eoutkev[i] <= axtenergieskev[numaxtenergies-1]) {
        arfgenlib::bisectionInterp(numaxtenergies, axtenergieskev, auxtrans, eoutkev[i], newtrans);
        outauxtrans[i] = newtrans;
      } 
    } // end loop over output grid energies                                                      
    ahfits::close(ahffp_auxtransfile);  
  } // end if auxtransfile != NONE 

  // Get fine and coarse energy grids from input EA files and the 
  // effective area arrays from these files

  //-------------------------GET COARSE ENERGY GRID----------------------------
  // Coarse energy grid from the input coarse on-axis EA file; the EA file has 
  // lower and upper energy bounds but the raytracing has single energies, so 
  // we need the center energies of the ARF energy bins
  arfgenlib::geteadata(par.m_onaxiscfile, numebinscoarsefull, ecoarsecenfull, 
                       coarsearfeffareafull);
 
  AH_DEBUG << "numebinscoarsefull = " << numebinscoarsefull << std::endl;
  AH_DEBUG << "ecoarsecenfull[0] = " << ecoarsecenfull[0] << std::endl;
  AH_DEBUG << "ecoarsecenfull[numebinscoarsefull-1] = " << 
               ecoarsecenfull[numebinscoarsefull-1] << std::endl;
  AH_DEBUG << "coarsearfeffareafull[0] = " << coarsearfeffareafull[0] 
           << std::endl;
  AH_DEBUG << "coarsearfeffareafull[numebinscoarsefull-1] = " 
           << coarsearfeffareafull[numebinscoarsefull-1] << std::endl;
   
  //-------------------------GET FINE ENERGY GRID-------------------------------
  // Fine energy grid from the input fine-grid on-axis EA file; the EA file has 
  // lower and upper energy bounds but the raytracing has single energies, 
  // so we need the center energies of the EA energy bins
  arfgenlib::geteadata(par.m_onaxisffile, numebinsfine, efinecen, 
                       finearfeffarea);

  AH_DEBUG << "numebinsfine = " << numebinsfine << std::endl; 
  AH_DEBUG << "efinecen[0] = " << efinecen[0] << std::endl;
  AH_DEBUG << "efinecen[numebinsfine-1] = " << efinecen[numebinsfine-1] 
           << std::endl;
  AH_DEBUG << "finearfeffarea[0] = " << finearfeffarea[0] << std::endl;
  AH_DEBUG << "finearfeffarea[numebinsfine-1] = " 
           << finearfeffarea[numebinsfine-1] << std::endl;
  
  //------------------------GET EXPOSURE MAP DATA-------------------------------
  // Open exposure map file; read keywords from primary header and then read the
  // 1st image from the 1st extension (subsequent images will be read later, 
  // inside a loop). Also set up detector image coordinate parameters and 
  // boundaries.
  processEmap(par.m_instrume, par.m_telescop, par.m_skyregfile,
              par.m_emapfile, emap, doGatevalve,
              par.m_fwfile, par.m_gatevalvefile);

  //---------------------PROCESS RAYTRACING EVENT FILE--------------------------
  // Read the raytracing event file 2nd extension and create an array of 
  // injected photon numbers that will be used later for normalization to 
  // calculate the effective area. Also read the keyword for the absolute 
  // geometric area.
  processEvtfile(par.m_xrtevtfile, doRegion, numregphotons, emap.m_rtnumoffaxisfull, emap.m_rtnumoffaxis,
                 numebinscoarsefull, ecoarsecenfull,coarsearfeffareafull,
                 evtData,numebinscoarse,ecoarsecen,coarsearfeffarea);

  //-------------------PROCESS LIST OF REGION FILES-----------------------------
  // Determine whether the input parameter regionfile is the name of a file 
  // containing a list of files or whether it is a list of literal filenames
  
  if (doRegion) {
    if ('@' == par.m_regionfile[0]) {
      ahfits::expandFileList(par.m_regionfile, par.m_regfilelist);
    } else { 
      char** items=0;         // output list
      int nitems=0;           // number of items found
      int status=0;           // output status of expand_item_list
      int trim=1;             // trim spaces
      int skip=1;             // exclude empty items
      int guard=0;            // do not protect against commas in parentheses
      items=expand_item_list((char*)par.m_regionfile.c_str(),&nitems,',',trim, skip,guard,&status);
      if (status != 0) {
        AH_THROW_RUNTIME("invalid value for regionfile parameter; expect single file, or list of values separated by commas, or a file list");
      }
      for (int ii = 0; ii < nitems; ++ii) {
        std::stringstream tmp;
        tmp << items[ii];
        par.m_regfilelist.push_back(tmp.str());
      }   
    }
    numregions = par.m_regfilelist.size();
  
    if (emap.m_rtnumoffaxis != numregions) {
      AH_OUT << "emap.m_rtnumoffaxis = " << emap.m_rtnumoffaxis << std::endl;
      AH_OUT << "numregions = " << numregions << std::endl; 
      AH_THROW_RUNTIME("The number of detector regions does not match the number of off-axis angles.  Aborting...");
    }
    arfgenlib::setupregionstruct(numregions,par.m_regfilelist,xrtregions);
  } //end if doRegion

  // (Note: region files that are input to this routine should already have been
  // converted to telescope coordinates) Read the region files and set up an 
  // array of structures for each region.  
     
  if (par.m_instrume == "SXI") {
    arfgenlib::sxiefficiencies(obstime,par.m_qefile,par.m_contamifile,
                               emap.m_numdetx,emap.m_numdety,emap.m_detxlo,
                               emap.m_detxhi,emap.m_detylo,emap.m_detyhi,
                               numebinsfine,efinecen,numqefunc,qetransindex,
                               qecbftrans,numcontamfunc,qetrans,
                               contamtransindex,contamtrans,trantype,
                               par.m_abund, par.m_cols, par.m_covfac);
  } else if (par.m_instrume == "SXS") {
    arfgenlib::sxsefficiencies(obstime,par.m_qefile,par.m_obffile,par.m_fwfile,
                               par.m_contamifile,doGatevalve, 
                               par.m_gatevalvefile,emap.m_filterwheel,
                               emap.m_numdetx,emap.m_numdety,emap.m_detxlo,
                               emap.m_detxhi,emap.m_detylo,emap.m_detyhi,
                               numebinsfine, efinecen, numqefunc,numcontamfunc,
                               qetrans,qetransindex,contamtrans,
                               contamtransindex,filterheight,filterradius,
                               gvfracimage,fwfracimage,fwnx,fwny, 
                               fwx,fwy,fwxbnds,fwybnds,fwgeoimage,trantype,
                               par.m_abund, par.m_cols, par.m_covfac);
  }
  // Write all parameters to the log file.
  ahapp::writeParametersToLog();


}

//-----------------------------------------------------------------------------

void doWork(Par par, bool & doRegion, bool & doGatevalve, bool & getMatrix,
            bool & kevunits_rmf, long & numeoutbins, 
            std::vector<double> & eoutlo, 
            std::vector<double> & eouthi, std::vector<double> & eoutcen,
            double & eoutmin, double & eoutmax,std::vector<double> & eoutkev,
            long & numebinscoarse,
            std::vector<double> & ecoarsecen, 
            std::vector<double> & coarsearfeffarea, 
            long & numebinsfine, std::vector<double> & efinecen, 
            std::vector<double> & finearfeffarea, emapData & emap, 
            evtfileData & evtData, long & numregions, long & numregphotons, 
            std::vector<SAORegion*> & xrtregions, double & obstime,
            long & numqefunc, std::vector< std::vector<long> > & qetransindex,
            std::vector< std::vector<double> > & qecbftrans,
            std::vector< std::vector<double> > & qetrans,
            long & numcontamfunc, 
            std::vector< std::vector<long> > & contamtransindex,
            std::vector< std::vector<double> > & contamtrans,
            std::string & filterwheel, double & filterheight, 
            double & filterradius, 
            std::vector< std::vector<double> > & gvfracimage, 
            std::vector< std::vector<double> > & fwfracimage, 
            long & fwnx, long & fwny, std::vector<double> & fwx, 
            std::vector<double> & fwy, std::vector<double> & fwxbnds, 
            std::vector<double> & fwybnds,
            std::vector< std::vector<double> > & fwgeoimage, 
            int & trantype, std::vector<double> & outputarfarray,
            bool & insufficientphotons, std::vector<long> & photonsperenergy,
            std::vector<double> & outauxtrans){
  
  filterwheel = emap.m_filterwheel;
  
 
  AH_DEBUG << "Starting DoWork" << std::endl;
  // Set up array to hold photon number weights by energy (coarse grid), and by
  // off-axis angle. For every photon in the raytracing event file that 
  // succesfully hits the detector the array element corresponding to the 
  // photon energy and incident off-axis angle is incremented by  the products 
  // of all energy-independent efficiency factors (the energy in this weights 
  // array is purely for accounting for the spatial dependence of the PSF). 
  // Currently this array takes the value of [exposure fraction for this x,y 
  // pixel]*[gate valve effective geometric factor if SXS]

  std::vector< std::vector<double> > photonweights;
  std::vector<double> photonweights_row;
  photonweights_row.resize(emap.m_rtnumoffaxis,0.0);
  for (int ii=0; ii<numebinscoarse; ii++) {
    photonweights.push_back(photonweights_row);
  }
  
  // Set up an array of weights that will be used to account for the spatial 
  // dependence of energy-dependent efficiency functions by constructing a 
  // weighted mean net efficiency function of energy. Functions such as the QE,
  // contamination etc. should already have been combined and organized into a
  // single 2D efficiency array of all unique net efficiency curves as a 
  // function energy and spatial index. For each event in the raytracing file 
  // that successfully hits the detector, the index corresponding to the event 
  // x & y coordinates is looked up, and the corresponding element in the 
  // weight function is incremented as a counter. At the end the weighted mean 
  // transmission curve is calculated using these weights, and will multiply the
  // final telescope effective area curve on the same, final (output) energy 
  // grid. Any spatial energy dependence of the PSF will automatically be taken
  // into account because the raytracing event file will place photons in 
  // appropriate spatial positions that also depend on energy. Since the spatial
  // PSF energy dependence is weak, the coarse raytracing grid is sufficient 
  // to create the weights array, which is used to weight the transmission/
  // efficiency curves on an arbitrary energy grid.
 
  std::vector<double> qecbftransmissionweights;
  qecbftransmissionweights.resize(numqefunc,0.0);
  
  std::vector<double> contamtransmissionweights;
  contamtransmissionweights.resize(numcontamfunc,0.0);

  photonsperenergy.resize(numebinscoarse,0.0);

  // Initialize the variables that will give the smallest and largest number of
  // photons in any coarse energy bin (summed over off-axis angle) to indicate
  // the statistical quality of the calculated ARF (note that a real is used 
  // to count photons, not an integer) 
  double minevtphotons=1e30;
  double maxevtphotons=0.0;
  long energyindex = -1;
  long offaxisindex = -1; 

  // These arrays are re-used for each exposure map and their size is equal to 
  // the number of rows in the exposure map being processed.
  std::vector<long> pixeldetx;
  std::vector<long> pixeldety;
  std::vector<double> pixelexpfrac;

  // Boolean that will indicate when "theta"  in the raytracing file changes
  bool thetachanged = true;

  // other local varaibles
  std::string eunits;     // Energy unit 
  double rowenergy = 0.;  // Energy in individual row of event file
  long originalrownum;    // row number from raytracing file
  std::string pathcode;   // raytracing pathcode
  double xrtx = 0.;       // x position evtfile
  double xrty = 0.;       // y position evtfile
  double xrtdirx = 0.;    // x direction evtfile
  double xrtdiry = 0.;    // y direction evtfile
  double xrtdirz = 0.;    // z direction evtfile

  double rowoffaxis = 0.;  // offaxis angle of current group
  double  rowoffaxisindex = 0; // index of offaxis angle
  double currentenergy = 0.; // energy in current row of evtfile
  double currentoffaxis = 0.; // offaxis angle in current row
  int e1index = 0;   // energy index
  int e2index = 0;   // energy index

  //long em_x = 0;        //x width of exposure map image
  //long em_y = 0;        //y width of exposure map image
  
  double xrtdetx = 0.;  // detector coordinates x direction
  double xrtdety = 0.;  // detector coordinates y direction
  
  double photoncontribution = 0.;  // Individual photon's contribution to the 
                                   // photon weight
  
  bool InsideRegion;  // Event is inside input detector regions (yes/no)?

  double xatfilter = 0.;  // x coordinate at filter
  double yatfilter = 0.;  // y coordinate at filter
  double ratfilter = 0.;  // r (radial) coordinate ar filter
   
  long qetxi = 0;    // Quantum efficiecy index
  long contamtxi = 0;  // contamination index

  double wgteffarea = 0.0; // effective area weighting factor

  int polyorder = 0;    // polynomial order used to calculate fine effective area from coarse grid
  int maxpolyorder = 0;    // maximum potential polynomial order used in optimizing polyorder
  double * polycoeff = 0;  // polynomial coefficients

  std::vector<double> coarseEffareaRatio;  // Coarse effective area ratio 
  std::vector<double> fineEffareaRatioInterp; // fine effective area ratio, linearly interpolated
  std::vector<double> fineEffareaRatio;    // fine effective area ratio from polynomial fit
  std::vector<double> fineOffAxisEffArea;  // fine effective area at offaxis angle
  std::vector<double> outputtransmission;  // output transmission fraction
  std::vector<double> outputxrteffarea;    // final output effective area

  std::vector<double> effareawithdetector; // effective area with detector
  double outeffarea = 0.;                  // output effective area

  long idx = 0;                            // pixel coordinate
  long idy = 0;                            // pixel coordinate
   
  long rownum = -1;                       // row number
  long photoncounter=0;                   // number of photons
   
  std::vector<double> meantransmission;   // average transmission
  double nettransweightedsum=0.0;         // net weighted transmission sum
  double sumoftransweights=0.0;           // sum of tranmission weights
  double netcontamweightedsum=0.0;        // net contamination wieghted sum
  double sumofcontamweights=0.0;          // sum oof contamination weights
 
  bool xyinsidedetector = true;           // Photon inside outside detector 
                                          // (yes/no)?
  double extrenorm = 0.0;                 // Normalization factor for extended sources

  // Event rejection counters
  long outsidedetCnt = 0;               
  long dblrflctCnt = 0; 
  long negQEtransCnt = 0;
  long outsideRegCnt = 0; 
  long outsidefilterCnt = 0;
  ahfits::Router* router_outdetevtfile=0;

  // File pointer and columns in detcoordfile 
  ahfits::FilePtr ahffp_detcoordfile = 0;
  double l_timehalfway = 0;
  double l_energy = 0;
  double l_xrtdetx = 0;
  double l_xrtdety = 0;

  if (par.m_detcoordfile != "NONE") {
    // Create output file containing the DETX and DETY 
    // coordinates of each individual event as well as nominal average 
    // event time
    // COLUMNS: timefalfway, xrtdetx, xrtdety
 

    ahfits::create(par.m_detcoordfile,"",&ahffp_detcoordfile);
  
    ahfits::addEmptyTbl(ahffp_detcoordfile,"EVTCOORDTIME");
    ahfits::insertColAfter(ahffp_detcoordfile, "TIME","D", "");
    ahfits::insertColAfter(ahffp_detcoordfile, "ENERGY","D","");
    ahfits::setTUnit(ahffp_detcoordfile, "ENERGY","keV");
    ahfits::insertColAfter(ahffp_detcoordfile, "DETX_FLOAT","D", "");
    ahfits::insertColAfter(ahffp_detcoordfile, "DETY_FLOAT","D", "");
  


    router_outdetevtfile = new ahfits::Router(ahffp_detcoordfile);

  
    router_outdetevtfile->connectScalar(ahfits::e_WRITEONLY,"TIME",l_timehalfway);
    router_outdetevtfile->connectScalar(ahfits::e_WRITEONLY,"ENERGY",l_energy);
    router_outdetevtfile->connectScalar(ahfits::e_WRITEONLY,"DETX_FLOAT",l_xrtdetx);  
    router_outdetevtfile->connectScalar(ahfits::e_WRITEONLY,"DETY_FLOAT",l_xrtdety);  
    ahfits::move(ahffp_detcoordfile,"EVTCOORDTIME");
    ahfits::firstRow(ahffp_detcoordfile);
  }
  // Move to first extension in event file
  ahfits::move(evtData.m_ahffp,2);

  // Read "TUNIT1" keyword from header
  eunits = ahfits::getKeyValStr(evtData.m_ahffp,"TUNIT1"); 

  ahfits::Router router_evt(evtData.m_ahffp);
  
  // Connect local variables to columns
  router_evt.connectScalar(ahfits::e_READONLY,"ENERGY",rowenergy);
  router_evt.connectScalar(ahfits::e_READONLY,"ROWINDEX",originalrownum);
  router_evt.connectScalar(ahfits::e_READONLY,"PATHCODE",pathcode);
  router_evt.connectScalar(ahfits::e_READONLY,"FINALXPOS",xrtx);
  router_evt.connectScalar(ahfits::e_READONLY,"FINALYPOS",xrty);
  router_evt.connectScalar(ahfits::e_READONLY,"FINALXDIR",xrtdirx);
  router_evt.connectScalar(ahfits::e_READONLY,"FINALYDIR",xrtdiry);
  router_evt.connectScalar(ahfits::e_READONLY,"FINALZDIR",xrtdirz);
 
 
  // Loop over raytracing event file rows
  AH_DEBUG << "begin loop over rows in event file row" << std::endl;
  for (ahfits::firstRow(evtData.m_ahffp); ahfits::readOK(evtData.m_ahffp);
       ahfits::nextRow(evtData.m_ahffp)) {
    // Increment row number counter
    ++rownum;
   

    // Read each row of the raytracing event file 
    ahfits::readRow(evtData.m_ahffp);
    if (eunits == "eV") {
      rowenergy = rowenergy/1000.0;
    }

    rowoffaxis = evtData.m_inputgrpoffaxis[originalrownum-1];
   
    rowoffaxisindex = evtData.m_inputgrpoffaxisindex[originalrownum-1];
  
    //Following will be used to tell when energy and off-axis angle change 
    //compared to previous row
    if (rownum == 0) {
      currentenergy = rowenergy;
      currentoffaxis = rowoffaxis;
      offaxisindex = 0;
      energyindex = 0;
    }
      
    //Does the event have a pathcode that is acceptable? Currently only double 
    //reflections are valid (but the two reflections may occur along with 
    //other events that are not reflections, such as transmissions). The 
    //routine ChkDblRefl(pathcode) returns a Boolean.

    if (arfgenlib::ChkDblRefl(pathcode) == false) { 
       continue;
    }
    ++dblrflctCnt;
   
    //If the energy changes, update the energy index
  
    if ((rownum>0) && (rowenergy != currentenergy)) {

      photonsperenergy[energyindex] = photoncounter;

      arfgenlib::bisectionLocate(ecoarsecen,numebinscoarse,rowenergy,
                                 e1index,e2index);
      if ((rowenergy > ecoarsecen[0]) && 
          (rowenergy < ecoarsecen[numebinscoarse-1])) {
        energyindex = e2index;
      } else {
        energyindex = e1index;
      }

      //Update the photon counters (used to warn against too few photons in 
      //the event file to make a reliable ARF)
    
      if (photoncounter < minevtphotons) minevtphotons=photoncounter;
      
      if (photoncounter > maxevtphotons) maxevtphotons=photoncounter;
      photoncounter = 0;
      currentenergy = rowenergy;
    }

    //If the off-axis angle changes, update the off-axis angle index. 
    //(For an extended source, this needs to be the off-axis angle of a fixed 
    //point in the source, e.g. center, not the off-axis angle of entry of 
    //individual photons.)
   
    // Bring back the block to detect the change of theta, but we don’t need 
    // to match it to a grid because we already have it indexed:
    if (((rownum > 0) && (rowoffaxisindex != offaxisindex)) && 
        (offaxisindex>=0)) { 
      offaxisindex = rowoffaxisindex;
      thetachanged = true;
    } // End of if-block checking if off-axis angle changed compared 
      // to previous row

    //if ((rownum>0) && (rowoffaxis != currentoffaxis) && (offaxisindex>=0)) {
    //  AH_DEBUG << "off axis angle has changed currentoffaxis angle rowoffaxis = " << rowoffaxis<< std::endl;
    
    //  arfgenlib::bisectionLocate(evtData.m_rtoffaxis,emap.m_rtnumoffaxis,
    //                             rowoffaxis,th1index,th2index);
  //  offaxisindex = th1index;
  //   if ((rowoffaxis != evtData.m_rtoffaxis[0]) || (rowoffaxis != 
  //        evtData.m_rtoffaxis[emap.m_rtnumoffaxis-1])) {
  //      offaxisindex = th2index;
  //    } else {
  //      offaxisindex = th1index;
  //    }
  //    AH_DEBUG << "th1index = " << th1index << std::endl;
  //    AH_DEBUG << "th2index = " << th2index << std::endl;
      //Read the exposure map for this off-axis angle (over-write previous one);
      //Search for the extension by name: the name will be EXMAPnnn where 
      //nnnn=offaxisindex+1, with leading zeros to make 4 chars (i.e. possible 
      //extension names are EXMAP001…EXMAP999).
      //AH_DEBUG << "exposure map element 2,2 = " << emap.m_expmapimage[2][2];
  //    currentoffaxis = rowoffaxis;
  //  }

    currentoffaxis = rowoffaxis; 


    // If theta switched compared to the previous row read a new exposure map 
    // for the new attitude bin 
    if (thetachanged) {
      // Reset the default values of expmapimage(numdetx, numdety) to 1.0
      for (int ii=0; ii<emap.m_numdetx; ++ii) {
        for (int jj=0; jj<emap.m_numdety; ++jj) {
          emap.m_expmapimage[ii][jj] = 1.0;
        }
      }
      // Find and go to extension EXMAPnnn where nnn=offaxisindex+1, 
      // with leading zeros to make 4 chars (i.e. possible
      // extension names are EXMAP001~EXMAP999)
      std::stringstream extn_ss;
      extn_ss << std::setfill('0') << std::setw(3) << offaxisindex+1;
      std::string extn = "PARTIALEXP" + extn_ss.str();
      AH_DEBUG << "Extension name in exposure map file = " << extn; 
      if (ahfits::HDUExists(emap.m_ahffp_emap,extn)) {
        AH_DEBUG << "Extension Exists!" << std::endl;
        ahfits::move(emap.m_ahffp_emap,extn);
        long emapnumrows = ahfits::getKeyValLLong(emap.m_ahffp_emap,"NAXIS2");
        if (emapnumrows > 0) {
          // Read columns DETX DETY EXPOSUREFRAC
          long l_detx;
          long l_dety;
          double l_exposurefrac;
          ahfits::Router router_emap(emap.m_ahffp_emap);
          router_emap.connectScalar(ahfits::e_READONLY,"DETX",l_detx);
          router_emap.connectScalar(ahfits::e_READONLY,"DETY",l_dety);
          router_emap.connectScalar(ahfits::e_READONLY,"FRACTION",l_exposurefrac);
          for (ahfits::firstRow(emap.m_ahffp_emap); ahfits::readOK(emap.m_ahffp_emap);
               ahfits::nextRow(emap.m_ahffp_emap)) {
            ahfits::readRow(emap.m_ahffp_emap);
            pixeldetx.push_back(l_detx);
            pixeldety.push_back(l_dety);
            pixelexpfrac.push_back(l_exposurefrac);
            emap.m_expmapimage[l_detx-1][l_dety-1] = l_exposurefrac;
          }
        } // End of if-block checking if number of rows in exp. map is >0
      } // End of if-block checking if extension EXMAPnnn exists
      thetachanged = false;
    } // End of if-block checking if thetachanged=True            


    // +++MSD in the above section arn't we just reading the image?
    //  ahfits::readImage(emap.m_ahffp_emap,emap.m_expmapimage,em_x,em_y);

    // Now determine if the event position actually lies on the active area of 
    // the detector (because if it doesn’t we don’t need to waste time doing 
    // anything more complicated and can just skip to the next row in the event 
    // file).
  
    // For the SXS we have to check each pixel because there are gaps between 
    // the pixels, but for the SXI we just check the image boundaries because 
    // the gaps between CCD chips will be accounted for in the exposure map.

    // NOTE that for both SXS and SXI we are just checking whether the event 
    // falls inside or outside the detector IMAGE in the exposure map or 
    // instrument map file – this image will be bigger than the actual detector,
    // containing blank space around the edges. This blank space will be taken 
    // care of by the value of qefunctionsindex being negative in those regions
    // AND/OR by the exposure time being zero.
  
    //First convert event telescope x and y coordinates to detector coordinates.
    //(Should there be a “flip” for the y-coordinate?)
    xrtdetx =  emap.m_optaxisdetx + (((emap.m_cosoptrotd*emap.m_optxflip*xrtx) -
                                          (emap.m_sinoptrotd*emap.m_optyflip*xrty))/emap.m_xmmperpixel); 
    xrtdety =  emap.m_optaxisdety + (((emap.m_sinoptrotd*emap.m_optxflip*xrtx) + 
                                          (emap.m_cosoptrotd*emap.m_optyflip*xrty))/emap.m_ymmperpixel);  


      //xrtdetx = emap.m_optaxisdetx + (xrtx/emap.m_xmmperpixel); 
      //xrtdety = emap.m_optaxisdety + (xrty/emap.m_ymmperpixel);
    
    //Then check boundaries
    xyinsidedetector = false;
    if (par.m_instrume == "SXS") { 
      for (int jj = 0; jj<emap.m_numdetx; ++jj) {
        for (int kk=0; kk<emap.m_numdety; ++kk) {
          if  ((xrtdetx >= emap.m_detxlo[jj]) && 
               (xrtdetx <= emap.m_detxhi[jj]) && 
               (xrtdety >= emap.m_detylo[kk]) && 
               (xrtdety <= emap.m_detyhi[kk])) {
             xyinsidedetector = true;
             //save the x & y ids for the pixel found
             idx = jj;
             idy = kk;
             break;
          }
        }
        if (xyinsidedetector) break;
      }
    } else if (par.m_instrume == "SXI") {
      // Here it is assumed that the gaps between chips are accounted for 
      // by the exposure map
      if ((xrtdetx >=emap.m_detminx) && (xrtdetx<=emap.m_detmaxx) && 
          (xrtdety >=emap.m_detminy) && (xrtdety<=emap.m_detmaxy)) {
        xyinsidedetector = true;
        //save the x & y ids for the pixel found
        idx = (long) (xrtdetx-emap.m_detminx);
        idy = (long) (xrtdety-emap.m_detminy);
 
      }
    } //end of if-block checking which instrument to do bounds for

    // TYMOD 2016-10-20 Write xrtdetx xrtdety and timehalfway to file
    l_timehalfway = 0.5*(emap.m_obstime + emap.m_tstop);
    l_energy = currentenergy;
    l_xrtdetx = xrtdetx;
    l_xrtdety = xrtdety;
     
    if (par.m_detcoordfile != "NONE") {
      ahfits::writeRow(ahffp_detcoordfile);
      ahfits::nextRow(ahffp_detcoordfile);
    }

    //Does the event lie on the active detector area? If not, skip to next row 
    //in event file because the photon in this row gets a zero weight so no 
    //need to waste time on region filtering etc.

    if (xyinsidedetector == false) {
      ++outsidedetCnt;
      continue;
    }
    if (qetransindex[idx][idy] < 0) {
      ++negQEtransCnt;
      continue;
    }
   
    //If SXS, does the event fall outside of the filter area (and is thus lost)?
    //If yes, skip to next row in event file because the photon in this row 
    //gets a zero weight so no need to waste time on region filtering etc. 
    //Applies to both type 1 and type 2 filter files. The structure transmission
    //fraction for the type 2 file should not count events falling outside the 
    //filter radius.
    if ((ahgen::strtoupper(par.m_instrume)=="SXS") && 
        (ahgen::strtoupper(par.m_fwfile) != "NONE")
        && (emap.m_filterwheelsubstr != "OPEN")) {
      //Project x and y (telescope coordinates) to height of filter to get 
      //new x & y there
      xatfilter = xrtx-(filterheight*(xrtdirx/xrtdirz));
      yatfilter = xrty-(filterheight*(xrtdiry/xrtdirz));
      ratfilter = sqrt((xatfilter*xatfilter)+(yatfilter*yatfilter));
      if (ratfilter > filterradius) {
        ++outsidefilterCnt; 
        continue;
      }
    }
   
    //Does the event lie inside the region for this off-axis bin? 
    InsideRegion=false;
     
    if (doRegion) {
      int nshapes;
      nshapes = xrtregions[offaxisindex]->nShapes;
      InsideRegion = fits_in_region(xrtdetx,xrtdety,xrtregions[offaxisindex]);
      // 160229a If this pixel already had an impact accept the photon even if 
      // it is not inside the region because we must deal with whole pixels to 
      // compare with real data
      if (emap.m_numimpacts[idx][idy] > 0) {
        InsideRegion = true;
      }
    } else {
      InsideRegion = true;
    }
    //If no, skip this row of the event file because it should add zero weight 
    //for this photon
    if (!InsideRegion) {
      ++outsideRegCnt;
      continue;
    } else {
      //If yes then do the following:
      //    1.  Get the exposure fraction for this xrtdetx, xrtdety and add it to 
      //        the photon weight counter array (but if SXS, first multiply the 
      //        exposure fraction by the geometric gatevalve fraction if the 
      //        gatevalve is turned on);
      //    2.  For this xrtdetx, xrtdety, look up the index to point to the 
      //        correct “net efficiency” array. Using that index, add 1 to the 
      //        appropriate position in the efficiency weights array; 
      //    3.  update the photon counter (for each coarse energy in the evt 
      //        file)
      photoncounter = photoncounter+1;
      // Update numregphtons
      numregphotons = numregphotons+1;   

      // 160229a Update pixel impact counter
      emap.m_numimpacts[idx][idy] = emap.m_numimpacts[idx][idy] + 1;



      if ((ahgen::strtoupper(par.m_instrume) == "SXS") && 
          (doGatevalve == true)) {
        photoncontribution = emap.m_expmapimage[idx][idy]*
                             gvfracimage[idx][idy] *
                             emap.m_sxspixelactivefraction;
      } else if ((ahgen::strtoupper(par.m_instrume) == "SXS") && 
                 (doGatevalve == false)) {
        //SXS without getvalve
        photoncontribution = emap.m_expmapimage[idx][idy]*
                             emap.m_sxspixelactivefraction;
      } else if (ahgen::strtoupper(par.m_instrume) == "SXI") {
        
        photoncontribution = emap.m_expmapimage[idx][idy];
        
      } //end of if-block checking instrument and gatevalve

      if (rownum == 0) {
        AH_DEBUG << "photoncontribution = " << photoncontribution << std::endl;
      }



   
      //If filter file specified AND filter is not OPEN AND transmission 
      //treatment is type 2 then use the image fractions in the 3rd 
      //extension of the filter file
      if (trantype == 2) {
        photoncontribution = photoncontribution * fwfracimage[idx][idy];
      } 
     
      photonweights[energyindex][offaxisindex] = photonweights[energyindex][offaxisindex] + photoncontribution;
      
      //if (rownum < 100) {
        //   AH_DEBUG << "photonweights[energyindex][offaxisindex] = " << 
          //              photonweights[energyindex][offaxisindex] << std::endl;
        //}
      //Now get index for the efficiency/transmission curves and update the 
      //appropriate element. 
      //Note that SXS QE is position-independent so is dealt with elsewhere    
  
      qetxi = qetransindex[idx][idy];
      qecbftransmissionweights[qetxi]=qecbftransmissionweights[qetxi] + 1.0;
      contamtxi = contamtransindex[idx][idy];  
      contamtransmissionweights[contamtxi]=contamtransmissionweights[contamtxi]+1.0;     
    } //end of if-block checking if event is inside or outside region
  }// end of loop over raytrace event file rows

  // Save counter for last bin.
  photonsperenergy[energyindex] = photoncounter;

  // Print fraction of psf to screen
  double noncalpixfrac = 0.;
  if (par.m_instrume == "SXS") {
    noncalpixfrac = (35.0/36.0);
  } else {
    noncalpixfrac=1.0;
  }
  if (dblrflctCnt > 0) { 
    double fracPSFinsideDET = (double)(dblrflctCnt -outsidedetCnt)/(double)(dblrflctCnt);
    AH_OUT << "Fraction of PSF in Detector Area = " 
           << noncalpixfrac*fracPSFinsideDET << std::endl;
    if (doRegion) {
      double fracPSFinsideReg = ((double)(dblrflctCnt - outsideRegCnt))/((double)dblrflctCnt);
      AH_OUT << "Fraction of PSF inside Region = " 
             << noncalpixfrac*fracPSFinsideReg << std::endl;
    }
  }

 
  for (long ii = 0; ii < numebinscoarse; ++ii) {
    for (long jj = 0; jj < emap.m_rtnumoffaxis; ++jj) {
      AH_DEBUG << "photonweights[" << ii << "][" << jj << "] = " << 
                  photonweights[ii][jj] << std::endl;
    }
  }

  //If minevtphotons is too low, abort the program because a viable ARF cannot be made

  if (minevtphotons < par.m_minphoton) {
    AH_OUT << "Number of raytrace photons too low: statistical errors on ARF will be too large to make a viable ARF- aborting";
    insufficientphotons = true;
  }
  
  //Next, calculate the ratio of the EA from the raytracing event file (weighted
  //average over off-axis angle, using the weights histogram in the exposure 
  //map), to the EA in the coarse-grid input on-axis ARF.
   
  //intialize coarseEffareaRatio
  coarseEffareaRatio.resize(numebinscoarse,0.0);

  //Loop over coarse energy grid in raytracing event file
  AH_DEBUG << "COARSE EFF AREA WEIGHT" << std::endl; 
  for (int ii=0; ii<numebinscoarse;++ii) {
    wgteffarea = 0.0;
    //Loop over off-axis angle
    for (int jj=0; jj<emap.m_rtnumoffaxis; ++jj) {
      //Calculate telescope weigthed mean effective area for this (coarse) 
      //energy value (weighting by the off-axis histogram fraction), using the 
      //absolute geometric area and the input numbers of raytracing photons per
      //energy per off-axis angle retrieved earlier from the raytracing event 
      //file. We multiply by the cosine of the off-axis angles because the ARF 
      //should be normalized to give source flux, but the raytracing is done 
      //with fixed numbers of photons at each off-axis angle.
 
      wgteffarea=wgteffarea + (evtData.m_geomarea * emap.m_grpexpfrac[jj] *
                 photonweights[ii][jj] *
                 evtData.m_cosrtoffaxis[jj] / evtData.m_rtnorms[ii][jj]);

      // Checking for division by zero or accesing an out of range vector 
      // element in the above calculation
      if (wgteffarea != wgteffarea) {
        AH_DEBUG << "emap.m_rtnumoffaxis = " << emap.m_rtnumoffaxis 
                  << std::endl;
        AH_DEBUG << "emap.m_numoffaxis = " << emap.m_numoffaxis 
                  << std::endl;
        AH_DEBUG << "wgteffarea = " << wgteffarea << std::endl;
        AH_DEBUG << "evtData.m_geomarea = " << evtData.m_geomarea << std::endl;
        AH_DEBUG << "emap.m_offaxisfrac[" << jj << "] = " 
                  << emap.m_offaxisfrac[jj] << std::endl;
        AH_DEBUG << "photonweights[" << ii << "][" << jj << "] = " 
                  << photonweights[ii][jj] << std::endl;
        AH_DEBUG << "evtData.m_cosrtoffaxis[" << jj << "] = " 
                  << evtData.m_cosrtoffaxis[jj] << std::endl;
        AH_DEBUG << "evtData.m_rtnorms[" << ii << "][" << jj << "] = " 
                  << evtData.m_rtnorms[ii][jj] << std::endl;
        AH_DEBUG << "****************************************" << std::endl;
      }
      if (wgteffarea > 100000) {
        AH_DEBUG << "emap.m_rtnumoffaxis = " << emap.m_rtnumoffaxis 
                  << std::endl;
        AH_DEBUG << "emap.m_numoffaxis = " << emap.m_numoffaxis 
                  << std::endl;
        AH_DEBUG << "wgteffarea = " << wgteffarea << std::endl;
        AH_DEBUG << "evtData.m_geomarea = " << evtData.m_geomarea << std::endl;
        AH_DEBUG << "emap.m_offaxisfrac[" << jj << "] = " 
                  << emap.m_offaxisfrac[jj] << std::endl;
        AH_DEBUG << "photonweights[" << ii << "][" << jj << "] = " 
                  << photonweights[ii][jj] << std::endl;
        AH_DEBUG << "evtData.m_cosrtoffaxis[" << jj << "] = " 
                  << evtData.m_cosrtoffaxis[jj] << std::endl;
        AH_DEBUG << "evtData.m_rtnorms[" << ii << "][" << jj << "] = " 
                  << evtData.m_rtnorms[ii][jj] << std::endl;
        AH_DEBUG << "****************************************" << std::endl;
      }   

    } //end loop over off-axis angle
    //Calculate the ratio of the above EA to the EA in the coarse energy grid 
    //input on-axis ARF file

    coarseEffareaRatio[ii] = wgteffarea/coarsearfeffarea[ii];
  }//End loop over coarse energy bins

  for (int kk=0; kk<numebinscoarse; ++kk) { 
    AH_DEBUG << "ecoarsecen[" << kk << "], coarseEffareaRatio[" << kk 
             << "],  coarsearfeffarea[" << kk << "]= " << ecoarsecen[kk] << 
             ", " << coarseEffareaRatio[kk] << ", " << coarsearfeffarea[kk] 
             << std::endl;
  } 

  // Fit the EA coarse-grid ratio with a 10th-order (or less) polynomial. 
  // [How to decide what order? Number of photons at a particular energy?] 
  
  // The routine HDpoly_fit() is in the headas file headas_polyfit.c 
  // (uses headas_polyfit.h, headas_svdfit.c).

  // Select and report actual polynomial order to be used for fit.
  if (par.m_numeric_polydeg < 0) { // -1 means polydeg="DEFAULT"
    if (numebinscoarse < 11) {
      maxpolyorder = numebinscoarse-1;
    } else {
      maxpolyorder = 10;
    }
    AH_INFO(ahlog::LOW) << "Testing polynomials of order 1 to " << maxpolyorder
      << " to find best representation" << std::endl;
    AH_INFO(ahlog::LOW) << "of coarse effective area ratio." << std::endl;

    // Interpolate input data onto fine grid linearly.
    fineEffareaRatioInterp.resize(numebinsfine,0.0);  // allocate
    for (int ii=0; ii<numebinsfine; ++ii) { 
      // Interpolate the EA ratio onto the fine energy grid.
      if ((efinecen[ii] >= evtData.m_rt_min_e) && (efinecen[ii] <= evtData.m_rt_max_e)) {
        arfgenlib::bisectionInterp(numebinscoarse,
                                   ecoarsecen, coarseEffareaRatio,
                                   efinecen[ii], fineEffareaRatioInterp[ii]);
      } else {
        fineEffareaRatioInterp[ii] = 0.0;
      }
      AH_DEBUG << "fineEffareaRatioInterp[" << ii << "] (" << efinecen[ii] << " keV) = " << fineEffareaRatioInterp[ii] 
               << std::endl;
    } //end loop over grid energies


    // Call routine to figure out best polynomial order based on
    // obtaining a good fit without large excursions in between the fitted
    // points.
    optimizePolyOrder(ecoarsecen, coarseEffareaRatio,
                      efinecen, fineEffareaRatioInterp, 
                      evtData.m_rt_min_e, evtData.m_rt_max_e,
                      numebinscoarse, numebinsfine, 
                      maxpolyorder, polyorder);
  } else {
    if (par.m_numeric_polydeg < numebinscoarse) {
      polyorder = par.m_numeric_polydeg;
      AH_INFO(ahlog::LOW) << "Using input polynomial order = " << par.m_numeric_polydeg << std::endl;
    } else {
      polyorder = numebinscoarse-1;
      AH_INFO(ahlog::LOW) << "Capping polynomial order at " << polyorder << std::endl;
      AH_INFO(ahlog::LOW) << "because not enough points in coarse grid for input value = "
        << par.m_numeric_polydeg <<  std::endl;
    }
  }

  AH_INFO(ahlog::HIGH) << "Using polynomial of order " << polyorder
    << " to fit coarse effective area ratio" << std::endl;

  // OLD CODE: Set value of polyorder
  //if (numebinscoarse >= 2) {
  //  polyorder = std::min<long>(10,numebinscoarse-1);
  //} else {
  //  AH_THROW_RUNTIME("A wider energy range must be selected in order for the arf to be calculated correctly.");
  //}

  polycoeff = new double[polyorder+1];
  
  // Evaluate the EA ratio on the fine energy grid in the input on-axis ARF file
  // , and then multiply by the on-axis effective area at each of those fine 
  // energies to get the fine-bin EA for the requested region, off-axis 
  // histogram, and exposure maps. [Loop over fine energy grid.]
  fineEffareaRatio.resize(numebinsfine,0.0);

  HDpoly_fit(&ecoarsecen[0],&coarseEffareaRatio[0],polycoeff,numebinscoarse,
             polyorder);
  
  for (int ii=0; ii <= polyorder; ++ii) { 
    AH_DEBUG << "Polycoeff[" << ii << "] = " << polycoeff[ii] << std::endl;
  }

  for (int ii=0; ii < numebinsfine; ++ii) {
    ahmath::evaluatepoly(efinecen[ii],fineEffareaRatio[ii],polyorder,polycoeff);
  }

  //Calculate the fine-grid EA using the input fine-grid on-axis ARF and the 
  //above fine-grid ratio
  fineOffAxisEffArea.resize(numebinsfine,0.0);
  AH_DEBUG << "ENERGY FINE, FINE RATIO, FINE OFFAXISAREA" << std::endl;
  for (int ii=0; ii<numebinsfine; ++ii) {
    fineOffAxisEffArea[ii] = finearfeffarea[ii] * fineEffareaRatio[ii];
        AH_DEBUG << "efinecen = " << efinecen[ii] << ", fineEffareaRatio[ii] = " 
        << fineEffareaRatio[ii] << ", fineOffAxisEffArea[ii] = " 
        << fineOffAxisEffArea[ii] << std::endl;
  }
  
  //Calculate the sums of the weights for normalization of the transmission 
  //array and mean transmission array 
  //Loop over energies in the transmission array energies grid
  meantransmission.resize(numebinsfine,0.0);
  for (int ii=0; ii<numebinsfine; ++ii) {
    nettransweightedsum=0.0;
    sumoftransweights=0.0;
    netcontamweightedsum=0.0;
    sumofcontamweights=0.0;
    //If SXI, loop over all QE*OBL unique transmission indices for this 
    //energy, else if SXS, just set transmission = product of all filter-type 
    //transmissions.
    if (par.m_instrume == "SXI") { 
      for (int jj=0; jj<numqefunc; ++jj) {
        //Sum contents of weights array to get normalization for the 
        //weighted mean
        sumoftransweights = sumoftransweights + qecbftransmissionweights[jj];
        nettransweightedsum = nettransweightedsum + qecbftrans[ii][jj] *
                              qecbftransmissionweights[jj];
      } //end loop over transmission array indices for SXI
    } else if (par.m_instrume == "SXS") {
      for (int jj=0; jj<numqefunc; ++jj) {
        sumoftransweights = sumoftransweights + qecbftransmissionweights[jj];
        nettransweightedsum = nettransweightedsum + (qetrans[ii][jj]*
                              qecbftransmissionweights[jj]);
      }
    } //end of if-block for instrument = SXS or SXI
    //AH_DEBUG << "Contamination transmission weights: " << std::endl;
    //Loop over all unique contamination transmission indices for this energy
    for (int jj=0; jj<numcontamfunc; ++jj) {
      //Sum contents of weights array to get normalization for the weighted mean
      sumofcontamweights = sumofcontamweights + contamtransmissionweights[jj];
      netcontamweightedsum = netcontamweightedsum + (contamtrans[ii][jj]*
                             contamtransmissionweights[jj]);
      
    } //end loop over transmission array indices
    if ((sumoftransweights > 0.0) && (sumofcontamweights > 0.0)) {
      meantransmission[ii] = (nettransweightedsum/sumoftransweights) *
                             (netcontamweightedsum/sumofcontamweights);
    }
    AH_DEBUG << "efinecen = " << efinecen[ii] << ", meantransmission[ii] = " 
             << meantransmission[ii] << std::endl;
  } // end loop over transmission array energies
  AH_DEBUG << "QE TRANSMISSION WEIGHTS: " << std::endl;
  for (int ee=0; ee<numqefunc; ++ee) {
    AH_DEBUG << "qecbftransmissionweights[" << ee << "] = " << 
                qecbftransmissionweights[ee] << std::endl;
  }

  //Now multiply the mean transmission curve and the fine-grid EA and 
  //interpolate this product onto the output energy grid to form the final 
  //output EA for the output ARF (the output energy grid is the same as the 
  //rmf energy grid)  
  
  outputtransmission.resize(numeoutbins,0.0);
  outputxrteffarea.resize(numeoutbins,0.0);
  outputarfarray.resize(numeoutbins,0.0);


  
  //Loop over the fine-bin energy grid to multiply the XRT EA by the detector 
  //efficiency (transmission array)
  
  effareawithdetector.resize(numebinsfine,0.0);
  for (int ii=0; ii<numebinsfine; ++ii) {
    effareawithdetector[ii] = meantransmission[ii] * fineOffAxisEffArea[ii];
  }
  
 

  //Loop over output energies in output energy grid
  //The results are only valid between the minimum and maximum raytracing energies
  //so set the EA to zero outside of this range. 
  if (numregphotons > 0) {
    extrenorm = ((double) evtData.m_evttotnphot) / ((double) numregphotons);
  } else {
    AH_INFO(ahlog::HIGH) << "WARINNG: No raytracing phtons passed through region, calculating effective area for no regions" << std::endl;  
    extrenorm = 1.0;
  }    

  for (int ii=0; ii<numeoutbins; ++ii) { 
    // Interpolate the EA for this energy onto the output energy grid. This is 
    //the final output effective area for the output ARF file.
    if ((eoutkev[ii] >= evtData.m_rt_min_e) && (eoutkev[ii] <= evtData.m_rt_max_e)) {
      arfgenlib::bisectionInterp(numebinsfine,efinecen,effareawithdetector,
                                 eoutkev[ii],outeffarea);
      outputarfarray[ii] = outeffarea * outauxtrans[ii];
    } else {
      outputarfarray[ii] = 0.0;
    }
    AH_DEBUG << "outputarfarray[" << ii << "] = " << outputarfarray[ii] 
             << std::endl;
    
  } //end loop over grid energies
 
  //Clean up memory
  delete[] polycoeff;
  arfgenlib::cleanupRegionStruct(xrtregions);


  // Print number of rejected events
  AH_DEBUG << "minevtphotons = " << minevtphotons << 
  " parameter minphton = " << par.m_minphoton << std::endl;
  AH_DEBUG << outsidedetCnt << " events were outside the detector area" 
           << std::endl;
  AH_DEBUG << dblrflctCnt << " events were double reflection events" 
           << std::endl;
  AH_DEBUG << negQEtransCnt << " events had negative QE" << std::endl;
  AH_DEBUG << outsideRegCnt << " events were outside the regions" 
           << std::endl;
  AH_DEBUG << outsidefilterCnt << " events were outside the filter (SXS only)"
           << std::endl;
  AH_DEBUG << "Total number of event rows = " << rownum << std::endl;

  // Close open files
  ahfits::close(emap.m_ahffp_emap);
  ahfits::close(evtData.m_ahffp);
  if (par.m_detcoordfile != "NONE") {
    if (router_outdetevtfile !=0) delete router_outdetevtfile, router_outdetevtfile=0;
    ahfits::close(ahffp_detcoordfile);
  }
  
  // Summarize photon counts
  AH_INFO(ahlog::HIGH) << "ENERGY      PHOTONS PER ENERGY" << std::endl;
  for (int ii=0; ii<numebinscoarse; ++ii) {
    ahlog::info(ahlog::HIGH, __func__, "%6g%24ld\n", ecoarsecen[ii], photonsperenergy[ii]);
  }

}//end of dowork
  
//-----------------------------------------------------------------------------

void finalize(emapData & emap, evtfileData & evtData, Par & par, 
              arfgenlib::arfkeywords & arfkey, long & numeoutbins, 
              std::vector<double> & eoutlo, std::vector<double> & eouthi, 
              std::vector<double> & outputarfarray, bool & kevunits_rmf,
              bool & insufficientphotons, int status){

  if (0 == status) {
    // Set required keywords in the final arf file
    arfkey.m_telescop = evtData.m_telescop;
    arfkey.m_instrume = evtData.m_instrume;
    arfkey.m_detnam = emap.m_detnam;
    arfkey.m_geomarea = evtData.m_geomarea;
    arfkey.m_emapfile = par.m_emapfile;
    arfkey.m_obstime = emap.m_obstime;
    arfkey.m_xrtevtfile = par.m_xrtevtfile;  

    // Only write the arf if there are sufficient photons
    if (insufficientphotons == false) {
      arfgenlib::writearffile(par.m_outfile,kevunits_rmf,arfkey,numeoutbins,eoutlo,
                              eouthi,outputarfarray);
    }
  }

  

}

//------------------------------------------------------------------------------

void processEmap(std::string instrume, std::string telescop, 
                 std::string skyregfile, std::string & emapfile, 
                 emapData & emap, bool & doGatevalve, 
                 std::string & fwfile, std::string gatevalvefile) { 

  // Dimesnions of image should be equal to numdetx and numdety
  long nx = 0;
  long ny = 0;
  double l_fraction = 0;

  bool pixelinsideregion = false;
  long numskypixelsinregion = 0;
  //double skyregionexptime = 0;

  double skyxc = 0; 
  double skyyc = 0;

  bool doskyregion = false;

  // Fits in region inputs
  int status = 0;
  SAORegion* skyregion = 0;
  WCSdata *WCS = (WCSdata*)malloc(sizeof(WCSdata));

  AH_DEBUG << "Begin processEmap" << std::endl;
  std::string emap_instrume;
  std::string fw_cnam;
  
  //open emapfile (Could be instrument map or exposure map)
  ahfits::open(emapfile,"",&emap.m_ahffp_emap);

  // Move to primary extension
  emap.m_numskyx = ahfits::getKeyValLLong(emap.m_ahffp_emap,"NAXIS1");
  emap.m_numskyy = ahfits::getKeyValLLong(emap.m_ahffp_emap,"NAXIS2");
  
  // Read the SKY exposure map file and find the total effective exposure time
  // inside the SKY region
  if (emap.m_numskyx > 0 && emap.m_numskyy > 0) {
    ahfits::readImage(emap.m_ahffp_emap,emap.m_skyexpimage,
                      emap.m_numskyx,emap.m_numskyy);
  
    // Get the WCS image keywords for the SKY image; not to be 
    // confused with the detector image
    emap.m_skymap_crval1 = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRVAL1");
    emap.m_skymap_crpix1 = ahfits::getKeyValLLong(emap.m_ahffp_emap,"CRPIX1");
    emap.m_skymap_cdelt1 = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CDELT1");
    emap.m_skymap_cunit1 = ahfits::getKeyValStr(emap.m_ahffp_emap,"CUNIT1");
    emap.m_skymap_crval2 = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRVAL2");
    emap.m_skymap_crpix2 = ahfits::getKeyValLLong(emap.m_ahffp_emap,"CRPIX2");
    emap.m_skymap_cdelt2 = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CDELT2");
    emap.m_skymap_cunit2 = ahfits::getKeyValStr(emap.m_ahffp_emap,"CUNIT2");
  
    if (ahgen::strtoupper(skyregfile) != "NONE") {
      doskyregion = true;
      // Fill in WCS data using WCS keywords from the exposure map
     
      WCS->exists = 1;
      WCS->xrefval = emap.m_skymap_crval1;
      WCS->yrefval = emap.m_skymap_crval2;
      WCS->xrefpix = emap.m_skymap_crpix1;
      WCS->yrefpix = emap.m_skymap_crpix2;
      WCS->xinc =  emap.m_skymap_cdelt1;
      WCS->yinc =  emap.m_skymap_cdelt2;
      WCS->rot = 0;
      strcpy(WCS->type,"-TAN"); 
     
      fits_read_ascii_region(skyregfile.c_str(), WCS, &skyregion, &status);
      
      //free(WCS); 
      if (status != 0) {
        AH_ERR << "Failed reading region file " << skyregfile << "." 
               << std::endl;
        AH_ERR << "CFITSIO status = " << status << std::endl;
        delete [] skyregion;
        AH_THROW_RUNTIME("Aborting...");
      }
    }
    
    // Calculate effective exposure time inside region defined in the SKY 
    // region file This is done by brute force- pending finding an existing 
    // better routine
    if (doskyregion) {
      for (int ii=0; ii<emap.m_numskyx; ++ii) {
        skyxc = (double)ii + 1.0;
        for (int jj=0; jj<emap.m_numskyy; ++jj) { 
          skyyc = (double)jj + 1.0;
        
          pixelinsideregion=fits_in_region(skyxc,skyyc,skyregion);
        
          if (pixelinsideregion) { 
            numskypixelsinregion = numskypixelsinregion+1;
            emap.m_skyregionexptime = emap.m_skyregionexptime + 
                                      emap.m_skyexpimage[ii][jj];
          } // End if-block for pixelinsideregion=True
        } // End loop over sky-y jj
      } // End loop over sky-x ii
    } else { // End of block for if doskyregion==True, else do the whole image
      for (int ii=0; ii<emap.m_numskyx; ++ii) {
        for (int jj=0; jj<emap.m_numskyy; ++jj) {
          numskypixelsinregion = numskypixelsinregion+1;
          emap.m_skyregionexptime = emap.m_skyregionexptime + 
                                    emap.m_skyexpimage[ii][jj];
        } // End loop over sky-y (j)
      } // End loop over sky-x (i)
    } // End of if-block testing if doskyregion=True or False
  
    // Average exposure time in image; if no pixels were found in 
    // the region then the exposure time correction factor remains 
    // at the default value of 1 and a warning is issued.    
   
    if (numskypixelsinregion > 0) {
      emap.m_skyregionexptime =  emap.m_skyregionexptime / 
                                 ((double)numskypixelsinregion);
    } else {
      AH_INFO(ahlog::HIGH) << "**WARNING** no pixels found inside SKY image region: results may not be correct" << std::endl;
      AH_THROW_RUNTIME("Aborting...");
    }
  } // End of if-block checking if NAXIS1 and NAXIS2 > 0 for primary exp map 
    // image
  
  // Determine if emapfile is an instrument map file or an exposure map 
  // file by looking for an extension called OFFAXISHIST
 
  if (ahfits::HDUExists(emap.m_ahffp_emap,"OFFAXISHIST")) {
    ahfits::move(emap.m_ahffp_emap,"OFFAXISHIST");
    emap.m_doexpomap = true;
    emap.m_numexpomaps = ahfits::getKeyValLLong(emap.m_ahffp_emap,"NUMEXMAP");
    emap.m_numgrptheta = emap.m_numexpomaps;
    if (ahgen::strtoupper(instrume) == "SXS") {
      if (ahfits::keywordExists(emap.m_ahffp_emap,"GATEVALV")) {
        emap.m_gatevalv = ahfits::getKeyValStr(emap.m_ahffp_emap,"GATEVALV");
      } else {
        emap.m_gatevalv = "OPEN";
      }
    }
  
    // Read keyword for start time of observation (in seconds) and convert it, 
    // using an appropriate offset to be compatible with the definition of time 
    // in the TIME column of the SXI contamination CALDB file (extension #1)
   
    emap.m_obstime = ahfits::getKeyValDbl(emap.m_ahffp_emap,"TSTART");
    emap.m_numoffaxis = ahfits::getKeyValLLong(emap.m_ahffp_emap,"NAXIS2");
    emap.m_tstop =  ahfits::getKeyValDbl(emap.m_ahffp_emap,"TSTOP");   

    // read column FRACTION
    // set up router to file
   
    double l_offaxisval = 0.;
    double l_timeinterval = 0.;
    ahfits::Router router(emap.m_ahffp_emap);
    router.connectScalar(ahfits::e_READONLY,"FRACTION",l_fraction);
    router.connectScalar(ahfits::e_READONLY,"OFFAXISVAL",l_offaxisval);
    router.connectScalar(ahfits::e_READONLY,"TIMEINTERVAL",l_timeinterval);
    for (ahfits::firstRow(emap.m_ahffp_emap); ahfits::readOK(emap.m_ahffp_emap);
         ahfits::nextRow(emap.m_ahffp_emap)) {
      ahfits::readRow(emap.m_ahffp_emap);
      emap.m_offaxisval.push_back(l_offaxisval);
      emap.m_offaxisfrac.push_back(l_fraction);
      emap.m_emaptimeintervals.push_back(l_timeinterval);
      
    }
    router.clearConnections();
    
    // Allocate memory to vectors
    emap.m_tfractionratio.resize(emap.m_numoffaxis,0.0);
    emap.m_numrtpairs.resize(emap.m_numoffaxis,0);
    emap.m_pairindexlo.resize(emap.m_numoffaxis,0);
    emap.m_pairindexhi.resize(emap.m_numoffaxis,0);
    for (int ii=0; ii<emap.m_numoffaxis; ++ii) {
      // Accumulate total exposure time
    
      emap.m_emaptotexposure = emap.m_emaptotexposure + 
                               emap.m_emaptimeintervals.at(ii);
      // Ratio of expfraction to the mean expfraction (=1/numoffaxis)
      emap.m_tfractionratio.at(ii) = emap.m_offaxisfrac.at(ii) * 
                                     (double) emap.m_numoffaxis;
      // We start duplicating pairs of theta,phi if the exposure fraction is
      // more than 1.5x mean
      emap.m_numrtpairs.at(ii) = (long)(emap.m_tfractionratio.at(ii) + 0.5);
      // We do not allow numrtpairs to get below 1
      if (emap.m_numrtpairs.at(ii) < 1) {
        emap.m_numrtpairs.at(ii) = 1;
      }
      for (int jj=0; jj<emap.m_numrtpairs[ii]; ++jj) {
        emap.m_pairindexlo.at(ii) = emap.m_rtnumoffaxisfull;
        emap.m_pairindexhi.at(ii) = emap.m_rtnumoffaxisfull + 
                                    emap.m_numrtpairs.at(ii) - 1;
        emap.m_rtnumoffaxisfull = emap.m_rtnumoffaxisfull + 1;
      }
    }// End of loop over off-axis angle bins

    // Set sky exposure correction 
    emap.m_skyexpcorrection = emap.m_skyregionexptime / emap.m_emaptotexposure;
   
    // We need to create an array of exposure fractions for off-axis angles 
    // that contains only unqiue values of theta. This is because, regardless 
    // of what expanded set of theta values was used to run the raytracing, the
    // event file will have rows grouped by the same theta and it is these 
    // groups of theta that will be treated together. However, note that the 
    // actual off-axis angles in the exposure file are not used directly 
    // because the aharfgen script may have run the raytracing for another 
    // source in the field. So we get the actual thetas from the raytracing 
    // event file but here we just need the corresponding exposure fractions 
    // which are the same regardless of the actual values of theta.

    long igrp = 0;
    std::vector<long> igrpstart;
    std::vector<long> igrpend;
      
    igrpstart.resize(emap.m_numgrptheta,0);
    igrpend.resize(emap.m_numgrptheta,0);
  
    igrpstart[0] = 0;
    igrpend[emap.m_numgrptheta-1] = emap.m_numoffaxis-1;
    
    for (int ii=0; ii<emap.m_numoffaxis; ++ii) {
       if ((ii>0) && (emap.m_offaxisval[ii] != emap.m_offaxisval[ii-1])) {
         igrp = igrp+1;
         igrpstart[igrp] = ii;
         igrpend[igrp-1] = ii-1;
       }
    }
    emap.m_rtnumoffaxis = emap.m_numgrptheta;
 
    if (emap.m_rtnumoffaxis != emap.m_numgrptheta) {
      AH_INFO(ahlog::HIGH) << "WARNING: Number of unique off-axis angles in exposure map does not match keyword" << std::endl;
    }
    
    emap.m_grpexpfrac.resize(emap.m_rtnumoffaxis,0.0);
    for (int ii=0; ii<emap.m_rtnumoffaxis; ++ii) {
      emap.m_grpexpfrac[ii] = emap.m_offaxisfrac[ii]; 
    }
    
  } else { // We are reading an instrument map
    emap.m_doexpomap = false;
    emap.m_numexpomaps = 1;
    emap.m_numoffaxis = 1;
    if (ahgen::strtoupper(instrume) == "SXS") { 
      emap.m_gatevalv = "OPEN";
    }
    emap.m_offaxisfrac.push_back(1.0);
    ahfits::move(emap.m_ahffp_emap,"INSTMAP");
  }
    
  emap_instrume = ahfits::getKeyValStr(emap.m_ahffp_emap,"INSTRUME");
  if (emap_instrume != instrume) {
    AH_THROW_RUNTIME("Mismatch between exposure map file and .par file INSTRUME");
  }
  emap.m_optaxisdetx = ahfits::getKeyValDbl(emap.m_ahffp_emap,"OPTAXISX");
  emap.m_optaxisdety = ahfits::getKeyValDbl(emap.m_ahffp_emap,"OPTAXISY");
  emap.m_xmmperpixel = ahfits::getKeyValDbl(emap.m_ahffp_emap,"DET_XSCL");
  emap.m_ymmperpixel = ahfits::getKeyValDbl(emap.m_ahffp_emap,"DET_YSCL");
  emap.m_opt_rotd = ahfits::getKeyValDbl(emap.m_ahffp_emap,"OPT_ROTD");
  emap.m_optxflip = ahfits::getKeyValDbl(emap.m_ahffp_emap,"OPTXFLIP");
  emap.m_optyflip = ahfits::getKeyValDbl(emap.m_ahffp_emap,"OPTYFLIP");
  emap.m_sinoptrotd = sin(emap.m_opt_rotd * degrees2radians);
  emap.m_cosoptrotd = cos(emap.m_opt_rotd * degrees2radians);

  //The SXS pixel gap and width and calculation of the the active area
  if (ahgen::strtoupper(instrume) == "SXS") {
    //emap.m_gatevalv = ahfits::getKeyValStr(emap.m_ahffp_emap,"GATEVALV");
    emap.m_sxspixelgap = ahfits::getKeyValDbl(emap.m_ahffp_emap,"SXSPXGAP");
    emap.m_sxspixelwidth = ahfits::getKeyValDbl(emap.m_ahffp_emap,"SXSPXWID");
    emap.m_sxspixelactivefraction = (emap.m_sxspixelwidth / 
                                    (emap.m_sxspixelwidth+emap.m_sxspixelgap)) *
                                    (emap.m_sxspixelwidth / 
                                    (emap.m_sxspixelwidth+emap.m_sxspixelgap));
    if ((ahgen::strtoupper(gatevalvefile) != "NONE") && 
        (ahgen::strtoupper(emap.m_gatevalv) == "CLOSE"))
    {
      doGatevalve = true;
    }
    if (emap.m_doexpomap) {
      // SXS filter wheel setting: OPEN1, OPEN2 ,FE55, BE, ND, POLYIMIDE
      // If OPEN1 or OPEN2 then the value of filterwheel is OPEN. Otherwise 
      // the value is equal to the name of the extension in the QE file 
      // containing the data.
      emap.m_filterwheel = ahfits::getKeyValStr(emap.m_ahffp_emap,"FILTER");
    } else {
      emap.m_filterwheel = "OPEN1";
    }
 
    // Get the first 4 characters of the FILTER keyword
    if (emap.m_filterwheel.length() > 4) { 
      emap.m_filterwheelsubstr = ahgen::strtoupper(emap.m_filterwheel.substr(0,4));
    } else {
      emap.m_filterwheelsubstr = emap.m_filterwheel;
    } 

    // 03-10-16 If the first four characters of filterwheel (FILTER KEYWORD) are
    // not any of the known values set filterwheel to OPEN1 also first four characters are set 
    // to OPEN. 
    if (emap.m_filterwheelsubstr != "OPEN" && emap.m_filterwheelsubstr != "FE55" && 
        emap.m_filterwheelsubstr != "BE" && emap.m_filterwheelsubstr != "ND" && 
        emap.m_filterwheelsubstr != "POLY") {
      emap.m_filterwheel = "OPEN1";
      emap.m_filterwheelsubstr = "OPEN";
    }
   

    // Obtain correct FILTER wheel file based on the first four character of the FILTER keyword
    if (emap.m_doexpomap) { 
      if (emap.m_filterwheelsubstr != "OPEN") {
        if (emap.m_filterwheelsubstr == "FE55") {
          fw_cnam = "FE55TRANS";
        } else if (emap.m_filterwheelsubstr == "BE") {
          fw_cnam = "BETRANS";
        } else if (emap.m_filterwheelsubstr == "ND") {
          fw_cnam = "ND25TRANS";
        } else if (emap.m_filterwheelsubstr == "POLY") {
          fw_cnam = "POLYTRANS";
        } else {
          AH_ERR << "first 4 characters of FILTER keyword = " 
                 << emap.m_filterwheelsubstr << std::endl;
          AH_THROW_RUNTIME("FILTER keyword in exposure map file not regonized - aborting");
        }
        fwfile = ahmission::caldb::resolve(fwfile,"filter wheel",instrume,"-",
                                           fw_cnam,"-","-",telescop);
        ape_trad_set_string("fwfile", fwfile.c_str());  
      }
    }
  } // end if instrume == SXS 
  
  if (!emap.m_doexpomap) {
    // We still need a detector map image; if doexpomaps==False get it from 
    // the instrument map; otherwise set one up later using keywords

    // Read image in INSTMAP extension (should already be at that extension)
    ahfits::readImage(emap.m_ahffp_emap,emap.m_expmapimage,nx,ny);
  }    

  // Setting up the detector coordinate pixel x,y center values and x,y 
  // boundaries.  This will be done using keywords in the exposure map file.
 	
  // We now have to get all of the following 8 keywords from the OFFAXISHIST 
  // extension. Note that because there are no detector coordinate images we 
  // must get numdetx and numdety from DET_XSIZ and DET_YSIZ respectively. 
  // Update: DET_YSIZ does not exist in the expo map file so for the moment 
  // assume square 

  emap.m_detmaprefx = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRVAL1"); 
  emap.m_detmaprefxpixel = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRPIX1"); 
  emap.m_detmapdeltax = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CDELT1"); 
  emap.m_detmaprefy = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRVAL2");
  emap.m_detmaprefypixel = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CRPIX2");
  emap.m_detmapdeltay = ahfits::getKeyValDbl(emap.m_ahffp_emap,"CDELT2");

  // Number of X and Y  bins
  emap.m_numdetx = ahfits::getKeyValLLong(emap.m_ahffp_emap,"DET_XSIZ");
  emap.m_numdety = emap.m_numdetx;
  AH_DEBUG << "numdetx = " << emap.m_numdetx << std::endl;
  AH_DEBUG << "numdety = " << emap.m_numdety << std::endl;
 
  // If we are using instrument map, set all the pixel values to 1.0
  // Now we set up a dummy detector map image even if doexpomap==True; the 
  // dummy values will simply be 1.0; it is in place in case it is needed.

  std::vector<double> expmapimagerow;
  std::vector<long> numimpactsrow;
  expmapimagerow.resize(emap.m_numdety,0.0);
  numimpactsrow.resize(emap.m_numdety,0.0);
  for (int kk=0; kk<emap.m_numdetx; ++kk) {
    emap.m_expmapimage.push_back(expmapimagerow);
    emap.m_numimpacts.push_back(numimpactsrow);
  }
  for (int ii=0; ii<emap.m_numdetx; ++ii) {
    for (int jj=0; jj<emap.m_numdety; ++jj) {
      emap.m_expmapimage[ii][jj] = 1.0;
      emap.m_numimpacts[ii][jj] = 0;
    }
  }

  // lower and upper bounds of the detx and dety pixels initializing vectors to
  // 0
  emap.m_detxlo.resize(emap.m_numdetx,0.0);
  emap.m_detxhi.resize(emap.m_numdetx,0.0);
  emap.m_detylo.resize(emap.m_numdety,0.0);
  emap.m_detyhi.resize(emap.m_numdety,0.0);

  // For SXS we have additional x and y upper bounds that include the gap (i.e. 
  // in this system: a pixel is pixel+top + "right-hand" gaps). Even though 
  // these coordinates are calculated from the exposure map, the are not 
  // actually used currently. Instead, the loss in EA due to the gaps between 
  // pixels is accounted for by directly reading the pixel corner coordinates 
  // from the teldef file and calculating the loss in geometric area and then 
  // using that to adjust the weight of a photon path in the raytracing that 
  // happens to fall on that pixel+gap area.

  emap.m_detxhifull.resize(emap.m_numdetx,0.0);
  emap.m_detyhifull.resize(emap.m_numdety,0.0);
 
  // For SXS instrument map:
  // CRVAL1=4.5; CRPIX1=4.5; CDELT1=1; same for 2nd set
  // For SXI instrument map:
  // CRVAL1=905.5; CRPIX1=905.5; CDELT1=1; same for 2nd set
  // Now calculate the boundaries
  AH_DEBUG << "Now calculate boundaries" << std::endl;
  for (int ii=0; ii<emap.m_numdetx; ++ii) {
    emap.m_detxlo[ii] = (((double)ii + 0.5 - emap.m_detmaprefxpixel) * 
                        emap.m_detmapdeltax) + emap.m_detmaprefx;
    emap.m_detxhifull[ii] = (((double)ii + 1.5 - emap.m_detmaprefxpixel)
                             * emap.m_detmapdeltax) + emap.m_detmaprefx;
    emap.m_detxhi[ii] = emap.m_detxhifull[ii] - (emap.m_sxspixelgap*(1.0e-6)
                       / emap.m_xmmperpixel);
  } //end loop over setting up detx boundaries
  
 
  for (int ii=0; ii<emap.m_numdety; ++ii) {
    emap.m_detylo[ii] = (((double)ii + 0.5 - emap.m_detmaprefypixel) * 
                        emap.m_detmapdeltay) + emap.m_detmaprefy;
    emap.m_detyhifull[ii] = (((double)ii + 1.5 - emap.m_detmaprefypixel)
                             * emap.m_detmapdeltay) + emap.m_detmaprefy;
    emap.m_detyhi[ii] = emap.m_detyhifull[ii] - (emap.m_sxspixelgap*(1.0e-6)
                       / emap.m_ymmperpixel);
  } // end of loop over dety boundaries


  //lower and upper bounds of the detector
  emap.m_detminx = emap.m_detxlo[0];
  emap.m_detmaxx = emap.m_detxhi[emap.m_numdetx-1];
  emap.m_detminy = emap.m_detylo[0];
  emap.m_detmaxy = emap.m_detyhi[emap.m_numdety-1];

  AH_DEBUG << "End process emap. " << std::endl;  
}


//------------------------------------------------------------------------------

void processEvtfile(std::string xrtevtfile, bool & doRegion,  long & numregphotons,long & rtnumoffaxisfull, 
                    long & rtnumoffaxis, long & numebinscoarsefull, 
                    std::vector<double> & ecoarsecenfull,
                    std::vector<double> & coarsearfeffareafull, 
                    evtfileData & evtData, long & numebinscoarse,
                    std::vector<double> & ecoarsecen, 
                    std::vector<double> & coarsearfeffarea) {

  double l_energy = 0.;
  long jesublo = 0; 
  long jesubhi = 0;
 
  AH_DEBUG << "Begin process event file" << std::endl;

  // Open xrt event file
  ahfits::open(xrtevtfile,"",&evtData.m_ahffp);
   
  // Go to first extension
  ahfits::move(evtData.m_ahffp,2);

  // Check for empty first extension
  ahmission::checkEmptyTable(evtData.m_ahffp,xrtevtfile);

  // Read keyword for absolute geometric area
  evtData.m_geomarea = ahfits::getKeyValDbl(evtData.m_ahffp,"GEOMAREA");
  
  // Read keywords for Instrume and telescope
  evtData.m_instrume = ahfits::getKeyValStr(evtData.m_ahffp,"INSTRUME");
  evtData.m_telescop = ahfits::getKeyValStr(evtData.m_ahffp,"TELESCOP");

  // Number of rows in the raytracing event file
  evtData.m_rtnumrows = ahfits::getKeyValLLong(evtData.m_ahffp,"NAXIS2");
  
  // Total number of input photons, including those that did not make it to 
  // the focal plane
  evtData.m_evttotnphot = ahfits::getKeyValLLong(evtData.m_ahffp,"TOTNPHOT");  

  evtData.m_srcmdl = ahfits::getKeyValStr(evtData.m_ahffp,"SRCMDL");

  if ((doRegion == false) || (ahgen::strtoupper(evtData.m_srcmdl) == "POINT")) {
    numregphotons = evtData.m_evttotnphot;
  } else {
    numregphotons = 0;
  }  

  // Go to second extension
  ahfits::move(evtData.m_ahffp,3);  

  // Read keywords
  evtData.m_rtnumens = ahfits::getKeyValLLong(evtData.m_ahffp,"NUMENRG");
  AH_DEBUG << "Number of energies = " << evtData.m_rtnumens << std::endl;
  
  // Read column ENERGY into variable evtData.m_evtenergies  
  ahfits::Router router1(evtData.m_ahffp);
  router1.connectScalar(ahfits::e_READONLY,"ENERGY",l_energy);
  for (ahfits::firstRow(evtData.m_ahffp); ahfits::readOK(evtData.m_ahffp);
       ahfits::nextRow(evtData.m_ahffp)) {
    ahfits::readRow(evtData.m_ahffp);
    evtData.m_evtenergies.push_back(l_energy);
  }
  
  // Match the energies in the raytracing event file to the energies in the 
  // coarse grid effeactive area file and hence create the coarse-grid 
  // effective area and energy sub-arrays.
  jesublo = 0;
  jesubhi = numebinscoarsefull-1;
    
  for (int ie=0; ie<numebinscoarsefull-2; ++ie) {
    if ((evtData.m_evtenergies[0] >= ecoarsecenfull[ie]) && 
        (evtData.m_evtenergies[0] < ecoarsecenfull[ie+1])) {
      jesublo=ie;
    } // end of if-block searching low energy bin of range
    if ((evtData.m_evtenergies[evtData.m_rtnumens-1] >= ecoarsecenfull[ie]) &&  
        (evtData.m_evtenergies[evtData.m_rtnumens-1] < ecoarsecenfull[ie+1])) {
      jesubhi=ie;
    } // end of if-block searching high energy bin of range
  } // end loop over full energy grid
  numebinscoarse = jesubhi-jesublo+1;
 
 
  for (int jj=0; jj<numebinscoarse; ++jj) {
    ecoarsecen.push_back(ecoarsecenfull[jesublo+jj]);
    coarsearfeffarea.push_back(coarsearfeffareafull[jesublo+jj]);
  } // end loop over new coarse effective area sub-arrays
  
  evtData.m_rt_min_e = ecoarsecen[0];
  evtData.m_rt_max_e = ecoarsecen[numebinscoarse-1];

  // We now test the number of theta values in the raytraceing event file 
  // (which may be repeated) with the number of theta values deduced earlier 
  // from the exposure map expanded theta, phi set; NOTE that 

  evtData.m_evtnumoffaxis = ahfits::getKeyValLLong(evtData.m_ahffp,"NOFFAXIS");

  if (rtnumoffaxisfull != evtData.m_evtnumoffaxis) {
    AH_DEBUG << "rtnumoffaxisfull = " << rtnumoffaxisfull << std::endl;
    AH_DEBUG << "evtData.m_evtnumoffaxis = " << evtData.m_evtnumoffaxis 
             << std::endl;
    AH_THROW_RUNTIME("The number of off-axis angles in the XRT event file does not match the number of off-axis angles in the exposure map file");
  }

  evtData.m_rtnumazimuth = ahfits::getKeyValLLong(evtData.m_ahffp,"NAZIMUTH");

  // Add second normalization array for the expanded theta, phi set
  // NOTE that the definition of rtnumoffaxis compared to previous code 
  // has changed.
  
  // Initialize 2-D vector evtData
  AH_DEBUG << "intialize 2-D vectors for storing event data" << std::endl;

  std::vector<double> rtnorm_row;
  rtnorm_row.resize(rtnumoffaxis,0.0);
  for (int ii=0; ii<evtData.m_rtnumens; ++ii) { 
    evtData.m_rtnorms.push_back(rtnorm_row);
  }   

  std::vector<double> rtnormfull_row;
  rtnormfull_row.resize(rtnumoffaxisfull,0.0);
  for (int ii=0; ii<evtData.m_rtnumens; ++ii) { 
    evtData.m_rtnorms_full.push_back(rtnormfull_row);
  }
  
  evtData.m_rtoffaxisfull.resize(rtnumoffaxisfull);
  evtData.m_rtoffaxis.resize(rtnumoffaxis,0.0);
  evtData.m_cosrtoffaxis.resize(rtnumoffaxis,0.0);

  // Off-axis angle for each input photon (for extended sources it is the value
  // supplied for the raytracing input, not the actual off-axis angle of each 
  // photon leaving the source)
  evtData.m_inputfulloffaxis.resize(evtData.m_evttotnphot,0.0);
  
  // Theta index corresponding to above (starting from 0)
  evtData.m_inputfulloffaxisindex.resize(evtData.m_evttotnphot,0.0);  
  
  // The variables with "grp" refer to the array of unique off-axis angles 
  // (i.e. may be less than the number of rows in the off-axis histogram);
  
  evtData.m_inputgrpoffaxis.resize(evtData.m_evttotnphot,0.0);
  
  evtData.m_inputgrpoffaxisindex.resize(evtData.m_evttotnphot,0);
  
  long grpj = 0;

  std::vector<double> rtazimfull;
  rtazimfull.resize(rtnumoffaxisfull,0.0);
  double l_rtazimfull = 0.0;  

  std::vector<long> uniquethetastartindex;
  uniquethetastartindex.resize(rtnumoffaxis,0);
   
  std::vector<long> uniquethetaendindex;
  uniquethetaendindex.resize(rtnumoffaxis,0);

  uniquethetastartindex[0] = 0;
  uniquethetaendindex[rtnumoffaxis-1] = evtData.m_evttotnphot-1;


  // Read the second extension of the event file
  
  ahfits::Router router(evtData.m_ahffp);
  long rownum = 0;
  long numphotons = 0; 
  double initialtheta = 0.;
  long fullrowindex1 = 0;
  long fullrowindex2 = 0;
  router.connectScalar(ahfits::e_READONLY,"NUMPHOTONS",numphotons);
  router.connectScalar(ahfits::e_READONLY,"INITIALTHETA",initialtheta);
  router.connectScalar(ahfits::e_READONLY,"INITIALAZIMDIR",l_rtazimfull);
  AH_DEBUG << "Begin reading event file" << std::endl;
  for (int jj=0; jj<rtnumoffaxisfull; ++jj) {
    for (int ii=0; ii<evtData.m_rtnumens; ++ii) {
      rownum = (jj * evtData.m_rtnumens)+ii+1;
      ahfits::gotoRow(evtData.m_ahffp,rownum);
      ahfits::readRow(evtData.m_ahffp);
      rtazimfull.at(jj) = l_rtazimfull;
      fullrowindex2 = fullrowindex1 + numphotons - 1;
      evtData.m_rtnorms_full[ii][jj] = (double)numphotons;
      if (ii == 0) { 
        evtData.m_rtoffaxisfull[jj] = initialtheta;
        if (jj > 0) { 
          if ((evtData.m_rtoffaxisfull[jj] != evtData.m_rtoffaxisfull[jj-1]) ||
              (rtazimfull[jj] != rtazimfull[jj-1])) {
            grpj = grpj+1;
            uniquethetastartindex[grpj] = fullrowindex1;
            uniquethetaendindex[grpj-1] = fullrowindex1 - 1;
            evtData.m_rtoffaxis[grpj] = evtData.m_rtoffaxisfull[jj];
          }
        } else if (jj==0) {
          evtData.m_rtoffaxis[0] = evtData.m_rtoffaxisfull[0];
        } //End of if-block checking j>0 & theta change
      } // End of if-block checking i==0
      // Following arrays point to the off-axis angle for every input photon 
      // ("center" value for extended source models)
      for (long kk=fullrowindex1; kk<=fullrowindex2; ++kk) {
        evtData.m_inputfulloffaxisindex[kk] = jj;
        evtData.m_inputfulloffaxis[kk] = evtData.m_rtoffaxisfull[jj];
      }
      fullrowindex1 = fullrowindex2+1;
      evtData.m_rtnorms[ii][grpj] = evtData.m_rtnorms[ii][grpj] + 
                                    (double)numphotons;
    }
  } 

  uniquethetaendindex[grpj]=fullrowindex2;

  // Set up "grp" cos(theta) and  pointers
  for (int jj=0; jj < rtnumoffaxis; ++jj) {
    evtData.m_cosrtoffaxis[jj] = cos(arcmin2radians*evtData.m_rtoffaxis[jj]);
    long numkkvals = uniquethetaendindex[jj]-uniquethetastartindex[jj]+1;
    for (long kk=0; kk < numkkvals; ++kk) {
      long kkindx = uniquethetastartindex[jj] + kk;
      evtData.m_inputgrpoffaxisindex[kkindx] = jj;
      evtData.m_inputgrpoffaxis[kkindx] = evtData.m_rtoffaxis[jj];
    }
  }

  int cnt = 0;
  for (int ii=0; ii<evtData.m_evttotnphot; ++ii) {
    if (evtData.m_inputfulloffaxis[ii] != 0) {
      ++cnt;
    }
  }
  
  AH_DEBUG << "End process event file" << std::endl;
}

//------------------------------------------------------------------------------

void optimizePolyOrder(
  std::vector<double> & ecoarsecen, 
  std::vector<double> & coarseEffareaRatio, 
  std::vector<double> & efinecen, 
  std::vector<double> & fineEffareaRatioInterp, 
  double energy_min, double energy_max,
  long numebinscoarse, long numebinsfine, 
  int maxpolyorder, int & bestpolyorder) {

  const int MAXORD=10;

  std::vector<std::vector<double> > fineEffareaRatio;  // Effective area ratio
                                  // recomputed on fine grid.  2-D array
                                  // as a function of [polynomial order][energy].

  std::vector<double> sumsq;      // Sum of squared deviations on fine grid,
                                  // as a function of polynomial order.

  std::vector<double> maxabsdev;  // Maximum absolute deviation on fine grid,
                                  // as a function of polynomial order.

  double polycoeff[MAXORD+1];     // Coefficients from polynomial fit.

  double deviation=0.0;           // Signed deviation of each point on fine grid.
  double absdeviation=0.0;        // Unsigned deviation of each point on fine grid.
  double minsumsq=0.0, maxsumsq=0.0;  // min and max of sum of squares.
  double minmad=-1.0, maxmad=-1.0;    // min and max of max absolute deviation.

  int degminsumsq=0;              // Polynomial order optimized by sum of squares
  int degminmad=0;                // Polynomial order optimized by max abs dev

  // Allocate and initialize.
  fineEffareaRatio.resize(maxpolyorder);
  for (int ipord=1; ipord<=maxpolyorder; ++ipord) {
    fineEffareaRatio[ipord-1].resize(numebinsfine,0.0);
  }
  sumsq.resize(maxpolyorder,0.0);
  maxabsdev.resize(maxpolyorder,0.0);

  // Loop through the polynomial order values.  Compute the sum of
  // the squared deviations and the max absolute deviation for each
  // order, on the fine energy grid.

  for (int ipord=1; ipord<=maxpolyorder; ++ipord) {

    // ipord:  polynomial being tested

    for (int ii=0; ii<(MAXORD+1); ++ii) {
      polycoeff[ii] = 0.0;
    }
  
    AH_DEBUG << "Testing polynomial order = " << ipord << std::endl;

    // Do the test fit.
    HDpoly_fit(&ecoarsecen[0],&coarseEffareaRatio[0],polycoeff,numebinscoarse,
               ipord);

    for (int ii=0; ii <= maxpolyorder; ++ii) { 
      AH_DEBUG << "Polycoeff[" << ii << "] = " << polycoeff[ii] << std::endl;
    }

    // Evaluate the test fit on the fine grid.
    for (int ii=0; ii < numebinsfine; ++ii) {
      ahmath::evaluatepoly(efinecen[ii],fineEffareaRatio[ipord-1][ii],ipord,polycoeff);
    }

    // Compute the figure(s) of merit, within the energy range in the
    // raytracing file.
    for (int ii=0; ii < numebinsfine; ++ii) {
      if ((efinecen[ii] >= energy_min) && (efinecen[ii] <= energy_max)) {
        deviation = fineEffareaRatio[ipord-1][ii] - fineEffareaRatioInterp[ii];
        AH_DEBUG << "deviation[" << ii << "] (" << efinecen[ii] << " keV) = " << deviation << std::endl;
        sumsq[ipord-1] += deviation*deviation;
        absdeviation = (deviation > 0 ? deviation : -1.0*deviation);
        maxabsdev[ipord-1] = (absdeviation > maxabsdev[ipord-1] ? absdeviation : maxabsdev[ipord-1]);
      } else {
        AH_DEBUG << "skipping deviation[" << ii << "] (" << efinecen[ii] << " keV)" << std::endl;
      }
    }

    maxsumsq = (sumsq[ipord-1] > maxsumsq ? sumsq[ipord-1] : maxsumsq);
    maxmad = (maxabsdev[ipord-1] > maxmad ? maxabsdev[ipord-1] : maxmad);

  }

  // Print table to log.
  ahlog::info(ahlog::HIGH, __func__, "%5s  %15s  %15s\n", 
    "ORDER", "SUM OF SQUARES", "MAX ABS DEV");
  minsumsq = maxsumsq;
  minmad = maxmad;
  for (int ipord=1; ipord<=maxpolyorder; ++ipord) {
    ahlog::info(ahlog::HIGH, __func__, "%5d  %.15g  %.15g\n", 
      ipord, sumsq[ipord-1], maxabsdev[ipord-1]);
    if (sumsq[ipord-1] < minsumsq) {
      minsumsq = sumsq[ipord-1];
      degminsumsq = ipord;
    }
    if (maxabsdev[ipord-1] < minmad) {
      minmad = maxabsdev[ipord-1];
      degminmad = ipord;
    }
  }

  // +++ 2017-01-11 This criterion is temporary until confirmed.
  AH_INFO(ahlog::HIGH) << "Order selected by minimum sum of squares:        " << 
    degminsumsq << std::endl;
  AH_INFO(ahlog::HIGH) << "Order having minimum value of max abs deviation: " << 
    degminmad << std::endl;

  bestpolyorder = degminsumsq;
}

// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ahsxtarfgen.cxx,v $
 Revision 1.57  2017/01/13 23:40:07  rshill
 Added polydeg parameter to control fitting of effective
 area ratio on coarse energy grid.  Corrected bugs in printing out photonweights
 and in subscripting photonsperenergy.

 Revision 1.56  2016/12/13 15:13:34  mdutka
 fixed bug with applying the contamination file, netcontamweightedsum and sumofcontamweights need to be reset to zero during loop

 Revision 1.55  2016/11/23 20:16:58  mdutka
 Now single detector coordinate region file will be porcessed correctly

 Revision 1.54  2016/11/22 16:15:45  mdutka
 Auxillery transmission data can now be applied to the final arf

 Revision 1.53  2016/11/09 22:37:18  mdutka
 Now writing psf fraction to screen and output event coordinate file

 Revision 1.52  2016/07/29 18:48:52  mdutka
 Moving closing of exposure map and raytracing file to doWork, could lead to segfault if the program exited early enough

 Revision 1.51  2016/07/29 17:46:08  mdutka
 Adding check on each individual file in the list of region files, also fixing segfault if program fails in dowork or initialize

 Revision 1.50  2016/07/21 15:37:15  mdutka
 Now checking to see if region file (detector) exists

 Revision 1.49  2016/04/13 14:01:20  mdutka
 Adding telescop keyword to output arf

 Revision 1.48  2016/03/30 20:31:24  mdutka
 correcting ape_trad_set_string for contamination file CALDB lookup

 Revision 1.47  2016/03/29 21:22:12  mdutka
 gatevale = CLOSE instead of CLOSED, the check on minevtphtons will not longer cause the program to crash but the output ark will not be written on failiure

 Revision 1.46  2016/03/22 15:26:36  mdutka
 adding write parameter to all the correct places

 Revision 1.45  2016/03/22 15:16:16  mdutka
 Moving write parameters to end of initialize

 Revision 1.44  2016/03/21 13:55:39  mdutka
 Adding parameter logging

 Revision 1.43  2016/03/18 18:52:33  mdutka
 ahsxtarfgen.cxx

 Revision 1.42  2016/03/12 04:15:09  mdutka
 removing skyexpmap correction from coarseeffarea

 Revision 1.41  2016/03/12 02:19:30  mdutka
 correcting inconsistancy between TRF and code, skyexpcorrection was never applied to the coarse effective area ratio

 Revision 1.40  2016/03/10 23:07:21  mdutka
 Fixing unused variable warning (y_em x_em)

 Revision 1.39  2016/03/10 19:45:38  mdutka
 now if the sxs filter keyword is not recognized it will be set to open by default

 Revision 1.38  2016/03/04 17:31:41  mdutka
 removing renormalizaiton factor for extended source mode

 Revision 1.37  2016/03/03 23:15:37  mdutka
 addressing partial pixel issue and adding mode to handle detector coordinate regions

 Revision 1.36  2016/03/01 19:36:39  mdutka
 adding numimpacts per pixel counter

 Revision 1.35  2016/02/19 18:43:53  mdutka
 Adding reading of new keywords from exposure map file, also minor code cleanup


 Revision 1.34  2016/02/16 20:50:12  mdutka
 Exposure map extension names have changed now PARTIALEXPOnnn also exposure map fraction column is now just called fraction


 Revision 1.33  2016/02/12 19:38:10  mdutka
 Switching back to detector coordinate based exposure maps

 Revision 1.32  2016/02/11 19:05:11  mdutka
 updating logging statements and added a check on the number of dtector region passed as input, this lead to segfault when the number of regions was not correct

 Revision 1.31  2016/02/08 15:49:50  mdutka
 Now using function expand_item_list to parse comma separated list of detector region files

 Revision 1.30  2016/02/02 21:04:44  mdutka
 Code clean, fleshing out documentation/removing extraneous comments

 Revision 1.29  2016/02/02 17:17:02  mdutka
 corrected issue with energy index and repeated thetas in input raytracing file

 Revision 1.28  2016/01/31 19:18:26  mdutka
 correted bug with setting rowoffaxis and rowoffaxis index

 Revision 1.27  2016/01/28 21:12:49  mdutka
 adding check on the polynomial order so it does not exceed the number of coarse energy bins

 Revision 1.26  2016/01/28 15:58:17  mdutka
 aharfgen/aharfgen.pl



revision 1.25
date: 2016/01/28 14:45:14;  author: mdutka;  state: Exp;  lines: +57 -42
Correct reading of second extension in the event file, file parameter description to conform wil existing standards

revision 1.24
date: 2016/01/24 23:18:30;  author: mdutka;  state: Exp;  lines: +122 -178
Adding corrections based on argen TRF from 1-24-2016

revision 1.23
date: 2016/01/23 18:10:30;  author: mdutka;  state: Exp;  lines: +38 -28
Modified logging statements

revision 1.22
date: 2016/01/23 13:25:35;  author: mdutka;  state: Exp;  lines: +836 -356
corrections made to code in places where the code and the trf have diverged

revision 1.21
date: 2015/12/29 21:16:02;  author: mdutka;  state: Exp;  lines: +69 -29
Updating ahsxtarfgen to determine the coarse energy grid using the ray-tracing event file

revision 1.20
date: 2015/12/29 18:45:59;  author: rshill;  state: Exp;  lines: +6 -3
Added checkEmptyTable().

revision 1.19
date: 2015/12/21 18:00:40;  author: mdutka;  state: Exp;  lines: +4 -10
code cleanup removing whitespace

revision 1.18
date: 2015/12/16 15:54:38;  author: mdutka;  state: Exp;  lines: +77 -10
ahsxtarfgen.cxx

revision 1.17
date: 2015/12/09 16:01:30;  author: mdutka;  state: Exp;  lines: +33 -29
includes bug fixes and revised debugging output

revision 1.16
date: 2015/11/30 20:53:45;  author: mdutka;  state: Exp;  lines: +4 -4
changing codename (CCNM0001 keyword in header) for rmf file query

revision 1.15
date: 2015/11/30 18:42:05;  author: mdutka;  state: Exp;  lines: +42 -104
update code name for gatevalve file caldb query, fixed bug with processing fw file

revision 1.14
date: 2015/11/24 21:32:49;  author: mdutka;  state: Exp;  lines: +14 -19
updated region file parsing

revision 1.13
date: 2015/11/24 15:13:20;  author: mdutka;  state: Exp;  lines: +107 -70
checking in bug fixes for sxs processing

revision 1.12
date: 2015/11/16 22:00:35;  author: mdutka;  state: Exp;  lines: +104 -111
commiting bug fixes, fixed issue with contamination file = none and sxs output

revision 1.11
date: 2015/11/13 21:33:46;  author: mdutka;  state: Exp;  lines: +135 -62
arfgen can now function with instrument map instead as well as and exposure map

revision 1.10
date: 2015/11/10 23:13:23;  author: mdutka;  state: Exp;  lines: +208 -82
made several bug fixes involving, reading exposure maps and determining weights

revision 1.9
date: 2015/11/06 15:40:54;  author: mdutka;  state: Exp;  lines: +84 -47
Updating ahsxtarfgen

revision 1.8
date: 2015/11/04 19:04:49;  author: mdutka;  state: Exp;  lines: +202 -1268
Updating based on TRF Change 10-22

revision 1.7
date: 2015/10/14 16:30:55;  author: mdutka;  state: Exp;  lines: +82 -62
checking in first complete version of aharfgen ahsxtarfgen

revision 1.6
date: 2015/10/06 21:16:41;  author: mdutka;  state: Exp;  lines: +227 -114
checking in incremental progress

revision 1.5
date: 2015/10/01 17:28:09;  author: mdutka;  state: Exp;  lines: +275 -154
checking in incrmental progress on ahsxtarfgen

revision 1.4
date: 2015/09/23 21:22:39;  author: mdutka;  state: Exp;  lines: +493 -279
checking in incrmental progress

revision 1.3
date: 2015/09/21 21:09:46;  author: mdutka;  state: Exp;  lines: +763 -35
checking in incremental progress

revision 1.2
date: 2015/09/18 20:41:29;  author: mdutka;  state: Exp;  lines: +244 -1
checking in incrmental progress

revision 1.1
date: 2015/09/17 23:01:27;  author: mdutka;  state: Exp;
checking in new tool ahsxstarfgen


*/
