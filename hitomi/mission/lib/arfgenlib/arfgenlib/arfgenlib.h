/// \file arfgenlib.h
/// \brief Shared library for arf generator tools
/// \author Mike Dutka
/// \date $Date: 2016/04/13 14:00:10 $
 
/// \addtogroup mod_arfgenlib
/// \section arfgenlib_arfgenlib Common arf gen functions for multiple instruments
///
///
///  
/// \subsection mod_arfgenlib
/// 
/// This Library contains common routine which are used by sxi and sxs arfgen tools 

#ifndef ARFGENLIB_ARFGENLIB_H
#define ARFGENLIB_ARFGENLIB_H

#include "ahgen/ahversion.h"
#include "ahfits/ahfits.h"
AHVERSION(ARFGENLIB_ARFGENLIB,"$Id: arfgenlib.h,v 1.10 2016/04/13 14:00:10 mdutka Exp $")



#include "ahfits/ahfits.h"
#include <vector>

extern "C" {
  #include "region.h"
}
#include "fitsio.h"


/// \brief struct representing the three values in a Cartesian coordinate
struct CartesianCoord {
  
  /// default constructor
CartesianCoord() :
  m_x(0.), m_y(0.), m_z(0.) { }
  
  /// constructor
CartesianCoord(double x, double y, double z) :
  m_x(x), m_y(y), m_z(z) { }
  
  /// destructor
  //  ~CartesianCoord() { }
  
  double m_x;            ///< x-component of Cartesian coordinate
  double m_y;            ///< y-component of Cartesian coordinate
  double m_z;            ///< z-component of Cartesian coordinate
  
};  // end struct CartesianCoord

/// \brief 
/// \ingroup mod_arfgenlib
namespace arfgenlib {

// -----------------------------------------------------------------------------
/// \brief struture contains the rmf energy grid

struct s_rmfegrid {

  s_rmfegrid(): m_nebin(0) {}

  long long  m_nebin;               ///< number of energy bins 
  std::vector<double> m_energ_lo;  ///< low boundary of each energy bin 
  std::vector<double> m_energ_hi;  ///< high boundary of each energy bin
  std::vector<double> m_ecenter;    ///< center of each energy bin
  std::vector<double> m_ebounds_lo; ///< ENERG_LO matrix extension
  std::vector<double> m_ebounds_hi; ///< ENERG_HI matrix extension
  

};

// -----------------------------------------------------------------------------

/// \brief This structure contains the rmf matrix extension
struct s_rmfmatrix {
 
  std::vector<int> m_ngrp;                ///< number of groups
  std::vector< std::vector<int> > m_nchan;     ///< number of channels in each group
  std::vector< std::vector<int> > m_fchan;     ///< first channel within each group
  std::vector< std::vector<double> > m_matrix; ///< response matrix elements

};

// -----------------------------------------------------------------------------
/// \brief This structure contains header keywords for the output arf file
struct arfkeywords { 
  
arfkeywords(): m_geomarea(0), m_obstime(0) {}  
 
  std::string m_telescop;   ///< TELESCOP keyword
  std::string m_instrume;   ///< INSTRUME keyword
  std::string m_detnam;     ///< detnam keyword
  double m_geomarea;        ///< geometric area of the telscope
  std::string m_emapfile;   ///< exposue map file
  double m_obstime;         ///< observation time
  std::string m_xrtevtfile; ///< Name of event file from raytracing run

};

// -----------------------------------------------------------------------------

/// \brief opens a single rmf file, reads the necessary keywords and columns 
///        data,  derives and returns the energy scale, and – if rmmfflag is 
///        set  -- reads and return the full rmf data (needed to make HXI rsp) 
/// \param[in] filename     name of input rmf file
/// \param[in] rmfflag      false for energy grid only else full rmf
/// \param[out] rmfegrid    energy grid struct
/// \param[out] rmfmatrix   rmf matrix struct
/// \param[out] kevunits    True if energy grid is in keV units, false if energy
///                         units are eV                            
void rmfproc(std::string & filename, bool & rmfflag, s_rmfegrid & rmfegrid, 
             s_rmfmatrix & rmfmatrix, bool & kevunits);

// -----------------------------------------------------------------------------

/// \brief loads and stores data from arf file in memory
/// \param[in] arffile       string containing name of arf file
/// \param[out] numarfens    number of effective area and energy points
/// \param[out] arf_energ_lo column ENERG_LO, low energy boundary
/// \param[out] arf_energ_hi column ENERG_HI, high energy boundary
/// \param[out] arf_ecenter  center energy, average of energ_lo and energ_hi 
/// \param[out] arfeffarea   effective area in arf file  
void getarfdata(std::string & arffile, long & numarfens, 
                std::vector<double> & arf_energ_lo, 
                std::vector<double> & arf_energ_hi, 
                std::vector<double> & arf_ecenter, 
                std::vector<double> & arfeffarea); 

// -----------------------------------------------------------------------------

/// \brief loads and stores data from effective area file
/// \param[in] eafile effective area file name
/// \param[out] numeaens  number of energy and effective area points
/// \param[out] ea_ecenter effective area center
/// \param[out] effarea    effective area
void geteadata(std::string & eafile, long & numeaens, 
               std::vector<double> & ea_ecenter, 
               std::vector<double> & effarea); 

// -----------------------------------------------------------------------------

/// \brief write effective area to an ARF file, along with some keywords
/// \param[in] outfile     name of output arf file
/// \param[in] kevunits    specifies the unit type
/// \param[in] arfkeywords structure containing header keywords for arf file
/// \param[in] numeoutbins number of output energy bins
/// \param[in] eoutlo      lower edge output energy bins
/// \param[in] eouthi      upper edge output energy bins
/// \param[in] outputarfarray  Array of effective area value
void writearffile(std::string & outfile, bool & kevunits, 
                  arfkeywords & arfkeywords, long & numeoutbins, 
                  std::vector<double> & eoutlo, std::vector<double> & eouthi,
                  std::vector<double> & outputarfarray);

// -----------------------------------------------------------------------------
 
/// \brief sets up array of region strutures
/// \param[in] numregions       number of regions  
/// \param[in] regionfilenames  array of region file names
/// \param[in] structxrtregions array of region structures
void setupregionstruct(long & numregions, 
                       std::vector<std::string> & regionfilenames, 
                       std::vector<SAORegion*> & xrtregions);

// -----------------------------------------------------------------------------

/// \brief cleans up vector pointing to  region structures
/// \param[in] xrtregions vector of pointers to region structures
void cleanupRegionStruct(std::vector<SAORegion*> & xrtregions);


// -----------------------------------------------------------------------------

/// \brief reads a region file and x and y in detector coordianate and 
///        returns 1 (True) or 0 (False) if coordinate is within the 
///        region or not
/// \param[in] file   Input region file in detector coordinate
/// \param[in] detx   input x detector coordinates    
/// \param[in] dety   input y detector coordinates
int InRegion(const char* file, double detx, double dety);

// -----------------------------------------------------------------------------


/// \brief returns true if the photon raytracing pathcode contains a 
///        double reflection event
/// \param[in]  pathcode 32-character string containing up to 8 codes, each code
///             of length 4, indicating the type of raytracing event that occur
///             red. Each pathcode string contains events for 1 complete 
///             raytrace photon path (so the number of events could be any 
///             number between 1 and 8).
bool ChkDblRefl(std::string & pathcode);

// -----------------------------------------------------------------------------

/// \brief Read transmission and energy columns in a FITS file extension 
///        containing transmission data.
/// \param[in] transmissionfile  Name of FITS file containing the transmission 
///                              data
/// \param[in] extnumber         extension number in the fits file that contains
///                              the data
/// \param[out] numenergies      Number of rows (energy values and transmission 
///                              values) read from the file
/// \param[out] energies         Energy array from file
/// \param[out] transmission      Transmission values read from file
void readtrans(std::string & transmissionfile, int extnumber, 
               long & numenergies, std::vector<double> & energies, 
               std::vector<double> & transmission);

// ----------------------------------------------------------------------------

void readtype2trans(std::string & transmissionfile,long& nx1, long& ny1, 
                    std::vector<double> & gx, std::vector<double> & gy,
                    double & deltgx, double & deltgy,
                    std::vector< std::vector<double> > & geoimage, long & nx2,
                    long & ny2, std::vector< std::vector<double> > & fracimage
                    , long & ne1, std::vector<double> & ens1, 
                    std::vector<double> & trans1, long & ne2,
                    std::vector<double> & ens2, std::vector<double> & trans2);

// ----------------------------------------------------------------------------

/// \brief Determine of sxi effiiencies based on qe and contamination
/// \param[in] obstime        observation time
/// \param[in] qefile         quantum efficiency file
/// \param[in] contamifile    contamination file
/// \param[in] numdetx        x-dimension of image detector coordinates
/// \param[in] numdety        y-dimesion of image detector coordinates
/// \param[in] detxlo         pixel boundary det coordinates
/// \param[in] detxhi         pixel boundary det coordinates
/// \param[in] detylo         pixel boundary det coordinates
/// \param[in] detyhi         pixel boundary det coordinates
/// \param[in] numebinsfine   number of fine energy bins
/// \param[in] efinecen       center energy fine egrid
/// \param[out] numqefunc      numbers of unique transmission functions for QE
/// \param[out] qetransindex QE * OBF*CBF transmission vs. E function index
/// \param[out] qecbftrans      QE * OBF*CBF transmission
/// \param[out] qetrans         QE_OBL curves interpolated onto fine energy grid
/// \param[out] numcontamfunc   Number of unique contamination transmission vs. E curves
/// \param[out] contamtransindex index unique contamination transmission vs. E curve
/// \param[out] contamtrans     Each unique contamination transmission curve
void sxiefficiencies(double obstime, std::string qefile, std::string contamifile,
                     long numdetx, long numdety, std::vector<double> detxlo,
                     std::vector<double> detxhi, std::vector<double> detylo, 
                     std::vector<double> detyhi, long numebinsfine, 
                     std::vector<double> efinecen, long & numqefunc, 
                     std::vector< std::vector<long> > & qetransindex,
                     std::vector< std::vector<double> > & qecbftrans,
                     long & numcontamfunc, std::vector< std::vector<double> > & qetrans,
                     std::vector< std::vector<long> > & contamtransindex,
                     std::vector< std::vector<double> > & contamtrans,
                     int & trantype, std::vector<double> & abund, 
                     std::vector<double> & cols, std::vector<double> & covfac);

//------------------------------------------------------------------------------


/// \brief determine sxs efficiences 
/// \param[in] obstime        observation time
/// \param[in] qefile         quantum efficiency file
/// \param[in] obfile         optical blocking filer file
/// \param[in] fwfile         filter wheel file
/// \param[in] contamifile    contamination file
/// \param[in] dogatevalve    yes/no perform gatevalve correction 
/// \param[in] numdetx        x-dimension of image detector coordinates
/// \param[in] numdety        y-dimesion of image detector coordinates
/// \param[in] detxlo         pixel boundary det coordinates
/// \param[in] detxhi         pixel boundary det coordinates
/// \param[in] detylo         pixel boundary det coordinates
/// \param[in] detyhi         pixel boundary det coordinates
/// \param[in] numebinsfine   number of fine energy bins
/// \param[in] efinecen       center energy fine egrid
/// \param[out] numqefunc      numbers of unique transmission functions for QE
/// \param[in] qetransindex QE * OBF*CBF transmission vs. E function index
/// \param[in] qecbftrans      QE * OBF*CBF transmission
/// \param[out] qetrans         QE_OBL curves interpolated onto fine energy grid
/// \param[out] numcontamfunc   Number of unique contamination transmission vs. E curves
/// \param[out] contamtransindex index unique contamination transmission vs. E curve
/// \param[in] filterwheel     fitlerwheel keyword
/// \param[in] filterheight    Height of filter in filterwheel
/// \param[in] filterradius    Radius of filter in filterwheel
/// \param[in] gvfracimage     fraction of each pixel that is not obscured by the gatevalve
/// \param[in] fwfracimage     fraction of each pixel that is not obscured by Fe 55 calibration filter
/// \param[in] fwnx            x-dimension filter structure in the filter wheel file
/// \param[in] fwny            y-dimension filter structure in the filter wheel file
/// \param[in] fwxbnds         Boundary x of pixels in the array describing geometry of the filter
/// \param[in] fwybnds         Boundary y of pixels in the array describing geometry of the filter
/// \param[in] fwgeoimage      The image array describing geometry of the filter in the filter wheel file
/// \param[in] trantype        file file is type 1 (simple transmission) or type 2 (complex) 
void sxsefficiencies(double obstime, std::string qefile, std::string obfile,
                     std::string fwfile, std::string contamifile, bool dogatevalve, 
                     std::string gatevalvefile, std::string filterwheel, 
                     long numdetx, long numdety,  
                     std::vector<double> detxlo, std::vector<double> detxhi, 
                     std::vector<double> detylo, std::vector<double> detyhi, 
                     long numebinsfine, std::vector<double> efinecen, 
                     long & numqefunc, long & numcontamfunc,
                     std::vector< std::vector<double> > & qetrans, 
                     std::vector< std::vector<long> > & qetransindex,
                     std::vector< std::vector<double> > & contamtrans,
                     std::vector< std::vector<long> > & contamtransindex, 
                     double & filterheight, double & filterradius, 
                     std::vector< std::vector<double> > & gvfracimage, 
                     std::vector< std::vector<double> > & fwfracimage, 
                     long & fwnx, long & fwny, std::vector<double>
                     & fwx, std::vector<double> & fwy, std::vector<double> & 
                     fwxbnds, std::vector<double> & fwybnds,
                     std::vector< std::vector<double> > & fwgeoimage, 
                     int & trantype, std::vector<double> & abund,
                     std::vector<double> & cols, std::vector<double> & covfac);

//------------------------------------------------------------------------------

/// \brief load quantum efficiency data
/// \param[in] qefile         quantum efficiency file
/// \param[in] detxlo         pixel boundary det coordinates
/// \param[in] detxhi         pixel boundary det coordinates
/// \param[in] detylo         pixel boundary det coordinates
/// \param[in] detyhi         pixel boundary det coordinates
/// \param[in] numdetx        x-dimension of image detector coordinates
/// \param[in] numdety        y-dimesion of image detector coordinates
/// \param[out] numenergies    number of energy bins qefile
/// \param[out] energies       energy grid qefile
/// \param[out] numregions    number of regions
/// \param[out] qefuntionsindex indicies of qe functions
/// \param[out] qefunctions     vector of qe functions
/// \param[out] numcbfenergies  number of cbf energies
/// \param[out] cbf_energies    cbf energies
/// \param[out] cbffieltrans    cbf file transmission fractions
void getsxiqefunctions(std::string qefile, std::vector<double> detxlo, 
                       std::vector<double> detxhi, std::vector<double> detylo, 
                       std::vector<double> detyhi,  long numdetx, long numdety,
                       long & numenergies, std::vector<double> & energies, 
                       long & numregions, 
                       std::vector< std::vector<long> > & qefunctionsindex,
                       std::vector< std::vector<double> > & qefunctions,
                       long & numcbfenergies, 
                       std::vector<double> & cbf_energies, 
                       std::vector<double> & cbffiletrans);

//------------------------------------------------------------------------------
/// \brief load contamination data
/// \param[in
/// \param[in] detxlo         pixel boundary det coordinates
/// \param[in] detxhi         pixel boundary det coordinates
/// \param[in] detylo         pixel boundary det coordinates
/// \param[in] detyhi         pixel boundary det coordinates
/// \param[in] numdetx        x-dimension of image detector coordinates
/// \param[in] numdety        y-dimesion of image detector coordinates
/// \param[in] obstime        observation time
/// \param[out] numenergies   number of energy bins qefile
/// \param[out] energies      energy grid qefile
/// \param[out] numregions    number of regions

void getsxcontamifunctions(std::string contamifile, std::vector<double> detxlo,
                            std::vector<double> detxhi, 
                            std::vector<double> detylo, 
                            std::vector<double> detyhi, long numdetx, 
                            long numdety,
                            double & obstime, long & numenergies, 
                            std::vector<double> & energies, 
                            long & numregions, std::vector< std::vector<long> >
                            & contamifunctionsindex, 
                            std::vector < std::vector<double> > & 
                            contamifunctions, std::vector<double> & abund,
                            std::vector<double> & cols, 
                            std::vector<double> & covfac);
//------------------------------------------------------------------------------


/// \brief Routine to read QE file and re-map the QE data onto the 
///        raytracing energy grid
/// \param[in] qefile   Name of FITS caldb file containing the quantum 
///                     efficiency (QE) data.
/// \param[in] numlayers Number of HXI layers, each one with its own response 
///                      matrix
/// \param[in] numrawx   Number of x pixels in the QE map images in raw 
///                      coordinates.
/// \param[in] numrawy  Number of y pixels in the QE map images in raw 
///                      coordinates.
/// \param[in] numebinscoarse  Number of energies in the raytracing energy grid
/// \param[in] ecoarsecen      Raytracing energy grid
/// \param[out] qemap          The output QE map as a function of position in 
///                            rawx and rawy coordinates, photon energy, and 
///                            layer number.  
void readhxiqedata(std::string & qefile, long & numlayers,long & numrawx, 
                   long & numrawy, long & numebinscoarse, 
                   std::vector<double> &ecoarsecen, 
                   std::vector < std::vector < std::vector < std::vector<double> > > >  & qemap 
                   );

//------------------------------------------------------------------------------

void bisectionInterp(long numGridPts, 
                     const std::vector<double> & xGrid, 
                     const std::vector<double> & yGrid, 
                     double xIn, double & yOut);

//------------------------------------------------------------------------------

void bisectionLocate(const std::vector<double> & xGrid, 
                     long numGridPts, 
                     double xin, 
                     int & index1, 
                     int & index2);

//------------------------------------------------------------------------------

std::string doubleToString(double value);

//------------------------------------------------------------------------------

bool isPointInsidePolygon(const CartesianCoord & point, 
                          int numVertices,
                          const std::vector<CartesianCoord> & vertices);

//------------------------------------------------------------------------------

void listStringsToDoubles(const std::string & stringList, 
                          std::vector<double> & doubleList);

//------------------------------------------------------------------------------

/// \brief Subroutine to read a FITS file containing coefficients for ASTRO-H 
///        vignetting functions
/// \param[in] vignetfile   Name of input FITS file
/// \param[out] numcoeff  number of vignetting coefficients
/// \param[out] vignetcoeff Vignetting function coefficients for 4 ASTRO-H
void readahvignetfile(const std::string & vignetfile, int & numcoeff, 
                      double *** vignetcoeff); 

/// \brief Compute SXT vignetting.
/// \param[in] energy   energy in keV
/// \param[in] offaxis  offaxis angle in arcmin
/// \param[in] param    array of 5 parameters
/// \return vignetting factore
double sxtvign(double energy, double offaxis, double* param);

/// \brief Compute HXT vignetting.
/// \param[in] energy   energy in keV
/// \param[in] offaxis  offaxis angle in arcmin
/// \param[in] param    array of 5 parameters
/// \return vignetting factore
double hxtvign(double energy, double offaxis, double* param);

/// \brief Subroutine to evaluate vignetting function for a given energy and 
///        ASTRO-H telescope associated with one of the 4 instruments.
/// \param[in] instrume Name of instrument (SXS, SXI, HXI1, or HXI2)
/// \param[in] energy Energy in keV
/// \param[in] offaxisangle Off-axis angle in arcmin. Azimuthal symmetry is 
///                         assumed.
/// \param[in] numcoeff number of coefficients in vignetting function
/// \param[in] vignetcoeff(numcoeff, 4) vignetting function coefficients 
///                                     for each ASTRO-H instrument: 0=SXS, 
///                                     1=SXI, 2=HXI1, 3=HXI2
/// \param[out] vignetfactor Resulting vignetting factor 
int ahvignetfactor(char* instrume, double & energy, double & offaxisangle,
                   int & numcoeff, double ** vignetcoeff, double & vignetfactor);

void cleanup_vignet(double ** vignetcoeff);

} //end namespace arfgenlib

#endif   /* ARFGENLIB_ARFGENLIB_H */

/** Revision Log

$Log: arfgenlib.h,v $
Revision 1.10  2016/04/13 14:00:10  mdutka
Adding Telescop keyword to output arf

Revision 1.9  2016/01/10 19:39:54  mdutka
Checking in function Inregion, runs fits in region on a single region file

Revision 1.8  2016/01/05 23:42:35  rshill
Changed vignetting to use plain C-style interior functions for clarity.
Corrected order of parameters for consistency with CALDB file.



*/
