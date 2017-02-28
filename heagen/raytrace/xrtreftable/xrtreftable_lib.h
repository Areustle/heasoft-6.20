/// \file xrtreftable_lib.h
/// \brief Declares the supporting functions for reflectivity probability task
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2016/02/19 01:22:30 $
/// 
/// This file contains the declarations for the supporting functions for the 
/// reflectivity calculations.  It also contains the data structures that the
/// code uses.


#ifndef XRTREFTABLE_XRTREFTABLE_LIB_H
#define XRTREFTABLE_XRTREFTABLE_LIB_H

#include "fitsio.h"       // calls to cfitsio (functions, enums, READONLY, FLEN_CARD)

#include "ahlog/ahlog.h"  // Logging/messaging

#include <algorithm>      // find()
#include <cctype>         // isupper()
#include <complex>        // complex numbers
#include <iomanip>        // std::setprecision, std::setfill, std::setw
#include <sstream>        // my to_string()
#include <string>         // strings
#include <vector>         // vectors

// +++ for my testing
#include <iostream>       // std::cout, std::fixed


/** \addtogroup tool_xrtreftable
 * @{
 */
 

// convert between eV and keV (multiply!)
static const double s_eVTokeV = 0.0010;
static const double s_keVToeV = 1000.0;

static const double s_angstromTomm = 1.0e-7;

enum energyUnits_e {e_eV, e_keV};


/**********************************************
 * ********************************************
 * 		structs
 * ********************************************
**********************************************/


/// \brief Input and derived parameters used in the xrtreftable task.
///
/// Param is a structure storing the input parameters and various
/// related/derived parameters needed in the xrtreftable task.
struct Param {
  
  /// default constructor
  Param() : 
    m_out_fp(0), m_errorOccurred(false) { }
  
  /// destructor
  ~Param() {
    m_out_fp = 0;
  }
   
  /// tool parameters, from .par file
  std::string m_atomicfile;     ///< Input atomic data file name
  std::string m_atmscafile;     ///< Input atomic scatter factors file
  std::string m_atmsctng;       ///< Extension name in atomic scattering file (updated by code if user supplied file)
  std::string m_energyfile;     ///< Input file containing energy grid
  std::string m_anglefile;      ///< Input file containing angle grid
  std::string m_mirrorfile;     ///< Input file containing mirror structure definitions
  std::string m_outfile;        ///< Output file name for the reflectivity table
  std::string m_outext;         ///< Extension name for the output reflectivity table
  std::string m_roughmodel;     ///< Name of the model to calculate roughness (DW for Debye-Waller, or NC for Nevot-Croce, or none)
  
  // +++ 20150901 KLR this is to be removed
  std::string m_atmedgefile;    ///< Input atomic edge data file name
  
  /// user-supplied values to write to keywords in output header, from .par file
  std::string m_roughkey;
  std::string m_telescop;
  std::string m_instrume;
  std::string m_validdate;
  std::string m_validtime;
  std::string m_desc;
  
  /// standard parameters from .par file are handled by headas_start_up()
  
  /// output file pointer
  fitsfile * m_out_fp;
  
  /// flag to identify if an error was thrown
  bool m_errorOccurred;
  
};


/// \brief Atomic Data structure 
///
/// Holds all the data from the atomic data input file, such as atomic number Z,
/// element name, weight, etc.  This is basically a periodic table as a fits file.
struct AtomicData {
  
  /// default constructor
  AtomicData() :
    m_nelements(0) { }
  
  /// destructor
  ~AtomicData() {};
  
  /// num unique elements in data file
  int m_nelements;
  
  ///\brief store data straight from the atomic data file
  std::vector<int> m_Z;                       ///< Atomic Number Z
  std::vector<std::string> m_elsymbol;        ///< Element Symbol
  std::vector<std::string> m_elname;          ///< Element Name
  std::vector<double> m_elweight;             ///< Element Weight
  std::vector<double> m_eldensity;            ///< Element Density (g/cm^3)

};


/// \brief Atomic Scattering structure 
///
/// Holds all the atomic scattering data from the input scattering file.  Also
/// holds the derived vectors, storing the data in a way to that will be 
/// used in calculations.
struct Scattering {
  
  /// default constructor
  Scattering() :
    m_nscatelements(0) { }
  
  /// destructor
  ~Scattering() {};
   
  ///\brief store data straight from the atomic scattering file
  std::vector<int> m_Z;                       ///< Atomic Number Z
  std::vector<int> m_rowindex;                  ///< Energy bin number for a given element, starting with 1
  std::vector<double> m_energy;               ///< Energy (eV)
  std::vector<double> m_f1real;               ///< real part of complex atomic scattering factor 
  std::vector<double> m_f2imag;               ///< imag part of complex atomic scattering factor
  
  /// num unique elements in scattering file
  int m_nscatelements;
  
  // these will basically be lookup arrays, to keep track of the index of the
  // desired energy/f1/f2 in the data arrays
  std::vector<long> m_znumbers;
  std::vector< std::vector<long> > m_elemscatindex;
  // zscatindex is indexed by Z, so [0][0] and [0][1] are ignored
  // it stores the row number that each new Z starts at in scattering file
  std::vector< std::vector<long> > m_zscatindex;
  
  
  // Calculate effective lower and upper indices for f1 and f2 arrays
  // (in file, some f1 and f2 values don't exist, so have dummy values of -9999)
  // these will be used in interpolatef1f2()
  std::vector< std::vector<long> > m_f1effindex;
  std::vector< std::vector<long> > m_f2effindex;
  
};


// Surface structure contains all the complicated vectors that are storing
// information about the mirror surface, calculated from the raw data arrays 
// in the struct that come from the SURFACE file.
// This also contains any other relevant information from the TDF SURFACE 
// extension, such as the value of the DETNAM keyword
struct Surface {
  
  /// default constructor
  Surface() :
    m_numgroups(0), m_precolmaterial(0), m_nummassabs(0), m_nummaterials(0),
    m_numThickMaterials(0), m_numThinMaterials(0) { }
  
  /// destructor
  ~Surface() {};
  
  ///\brief DETNAM keyword from SURFACE extension
  std::string m_detnam;
  
  ///\brief store data straight from mirror definition SURFACE extension
  std::vector<int> m_groupnumber;           ///< Group number
  std::vector<int> m_firstshell;            ///< First mirror foil shell in this group
  std::vector<int> m_lastshell;             ///< Last mirror foil shell in this group
  std::vector<int> m_layernumber;           ///< Layer number in this group
  std::vector<std::string> m_coatformula;   ///< Chemical formula of material in this layer
  std::vector<double> m_coatdensity;        ///< Density of material in this layer (g/cm^3)
  std::vector<double> m_coatthickness;      ///< Thickness of this layer (Angstroms)
  std::vector<double> m_coatroughness;      ///< Inter-layer roughness parameter (Angstroms)
    
  /// number of groups of coatings on this mirror
  int m_numgroups;
  
  /// column number index of the pre-collimator material in the output file
  // +++ 20140218 KLR Currently it's Al, so just keep track of index for Al
  int m_precolmaterial;
  
  /// number of materials that have mass absorption coefficients
  /// Currently, all materials are written, so this number is the same as 
  /// m_nummaterials
  int m_nummassabs;
  
  // records the layer of the first substrate in each group, layer=N+1.  This is
  // the first negative layer per group. layer N=0 is vacuum. Indexed by group.
  std::vector<int> m_numlayerplusone;    
  /// total layers in each group, including thick layers
  std::vector<int> m_numlayertotal; 
  /// flag, used to write the keyword MULTLAYR in the output file
  bool m_isMultilayer;
  // arrange coating arrays into 2dim vectors, so can easily look up formula,
  // density, etc for a given group/layer
  std::vector< std::vector<std::string> > m_layerformula;
  std::vector< std::vector<double> > m_layerdensity;
  std::vector< std::vector<double> > m_layerthickness;
  std::vector< std::vector<double> > m_layerroughness;
  std::vector< std::vector<double> > m_sigmaroughnesssq;
  
  /// number of unique materials
  int m_nummaterials;
  /// list of all unique materials
  /// dimen: (m_nummaterials
  std::vector<std::string> m_materialformula;
  /// a list of the material density (takes first value seen in fits file)
  /// dimen: (m_nummaterials
  std::vector<double> m_materialdensity;
  /// how many times this material is in a thick layer (substrate) of a group
  /// dimen: (m_nummaterials)
  std::vector<long> m_materialinthicklayer;
  /// how many times this material is in a thin layer of a group
  /// dimen: (m_nummaterials
  std::vector<long> m_materialinthinlayer;
  /// simple count of how many thick materials (written to NSUBSTRA, number of 
  /// substrates) in this surface (value is set in initialize_output())
  int m_numThickMaterials;
  /// simple count of how many thin materials in this surface 
  /// (value is set in initialize_output())
  int m_numThinMaterials;
  /// points to the materialformula index for each layer in each group
  std::vector< std::vector<long> > m_layermaterialindex;
  
  /// matrix that stores Z for each atom in each material
  std::vector< std::vector<long> > m_materialz;
  /// matrix that stores amount of each atom in each material
  std::vector< std::vector<long> > m_materialnumatoms; 
  /// num of unique elements in each material
  std::vector<long> m_materialnumelements;  
  /// total atomic wt of each material
  std::vector<double> m_materialatomicweight; 

};


// structure to hold data, etc from energy grid input file
struct Energy {
  
  /// default constructor
  Energy() :
    m_numenergy(0) { }
  
  /// destructor
  ~Energy() {};
  
  /// store energy grid data straight from input
  std::vector<double> m_photonenergy;            ///< Energy (eV) 
  
  /// number of energy values in input file
  int m_numenergy;
  
  // precalculate wavelength array in angstroms and cm for energy grid
  // so far these are only used in calcsf1sf2materials(), but they may be used
  // in more places in the future, so keep them here
  std::vector<double> m_photonlam;      ///< Wavelength (Angstrom)
  std::vector<double> m_photonlamsq;    ///< Wavelength sq (Angstrom^2)
  std::vector<double> m_photonlamcm;    ///< Wavelength (cm)
  std::vector<double> m_photonlamcmsq;  ///< Wavelength sq (cm^2)
  std::vector<double> m_lambdasqfac;    ///< 8*pi^2/lambda^2, for roughness
  
};


// structure to hold data, etc from angle grid input file
struct Angle {
  
  /// default constructor
  Angle() :
    m_numangles(0) { }
  
  /// destructor
  ~Angle() {};
 
  /// store angle grid data straight from input (rad)
  std::vector<double> m_incidentangle;        ///< Angle (rad)
  
  /// number of angle values in input file
  int m_numangles;

};


// structure to hold calculated vectors about the atomic edge file
struct Edge {
  
  /// default constructor
  Edge() :
    m_nedges(0) { }
  
  /// destructor
  ~Edge() {};
  
  /// store edge data straight from input fits file
  std::vector<int> m_Z;                       ///< Atomic Number Z
  std::vector<int> m_rowindex;                ///< consecutive numbering of edges for each element
  std::vector<std::string> m_edgename;        ///< as for Kn or Ln or Mn where n is a number ex L2
  std::vector<double> m_energy;               ///< Energy (eV)
  std::vector<double> m_wavelength;           ///< Wavelength (Angstroms)
  
  /// total number of edges.  if is 0, then no file was given
  int m_nedges;
  
  /// map each edge to the energy array in the atomic scattering factor file, 
  /// using the atomic number associated with each edge
  std::vector<long> m_edgearray_sbin;
  
};



/**********************************************
 * ********************************************
 * 		function declarations
 * ********************************************
**********************************************/


/**********************************************
 * 		main work functions
**********************************************/

/// \brief breaks down a string coating material description into its
///        constituent elements
/// \param[in] coatingmaterial the coating material description
/// \param[in] atom structure with all atomic data
/// \param[out] nelemcoat number of unique elements in this coating material
/// \param[out] zcoat atomic number of constituent elements
/// \param[out] fcoat count of atoms with atomic number, corresponding to zcoat
/// \param[out] wcoat total atomic weight of this molecule
///
/// Decomposes a molecular formula to the constituent elements and returns 
/// atomic numbers of those elements, and numbers of atoms of each element in 
/// one molecule.
/// \internal
/// \note this code is basically a copy of xrrtmolecule:createMolecule()
///       received in the original raytracing code from the Japanese
///       xrrtray-7.3rev75_with_database.tar
void decomposeMaterial(const std::string & coatingmaterial, AtomicData & atom,
                       int & nelemcoat, std::vector<int> & zcoat, 
                       std::vector<int> & fcoat, double & wcoat);


/// \brief Calculates the scaled atomic scattering factors for each unique 
///        material.
/// \param[in] ephotonev photon energy in eV to calc optical constants
/// \param[in] ephotoncm photon wavelength in cm to calc optical constants
/// \param[in] ephotoncmsq photon wavelength in cm^2 to calc optical constants
/// \param[in] scat structure with all scattering information
/// \param[in] surf structure with all surface information
/// \param[in] atom structure with number of atomic elements
/// \param[in] param structure holding output file information
/// \param[in] edge structure with all atomic edge information
/// \param[out] sf1material vector with scaled f1 for this energy
/// \param[out] sf2material vector with scaled f2 for this energy
/// \param[out] mabsmaterial vector with mass absorption coefficient
/// 
/// Calculates the scaled atomic scattering factors for each unique 
/// material. This will be needed to calculate the reflectivity and transmission 
/// of a set of multilayer coatings, given a photon energy. The scattering 
/// factors for each unique element are obtained by interpolation of the 
/// scattering factors in the file atmscafile, using the interpolation 
/// routine interpolatef1f2. The outputs (scaled and interpolated scattering 
/// factors for each material) can be subsequently passed to the 
/// multilayerRefl() routine to calculate the optical constants and (complex) 
/// refractive indices of all of the layers in all mirror groups.
void calcsf1sf2materials(const double ephotonev, const double ephotoncm, 
                         const double ephotoncmsq, const Scattering & scat, 
                         const AtomicData & atom, Param & param,
                         const Edge & edge, const Surface & surf,
                         std::vector<double> & sf1material,
                         std::vector<double> & sf2material, 
                         std::vector<double> & mabsmaterial);


/// \brief Calculate scattering factors for an energy
/// \param[in] param structure holding output file information
/// \param[in] Z atomic number of element being considered
/// \param[in] ephotonev photon energy in eV to use when interpolating
/// \param[in] scat structure with all scattering information
/// \param[in] edge structure with all atomic edge information
/// \param[out] sf1out interpolated value of sf1 at this energy
/// \param[out] sf2out interpolated value of sf2 at this energy
///
/// Given an input energy and arrays of atomic scattering factors (scaled or 
/// unscaled) provided on the energy grid, calculate values of the scattering 
/// factors at the input energy for an atom specified by its atomic number. 
/// Simple linear interpolation is used, except if numtotaledges>0, absorption 
/// edge energies provided are used to implement a more complex interpolation. 
/// That is, instead of linear interpolation on grid points either side of the 
/// input energy (if those points include any of the specified edge energies), 
/// the slope of the two grid points above or below is linearly extrapolated to 
/// the input energy. (The two grid points are above or below the input energy 
/// depending on whether the input energy is above or below the relevant edge 
/// energy, respectively). The routine is not designed to handle cases in which 
/// there is more than one edge energy between any two points in the energy 
/// grid for the atomic scattering factors.
void interpolatef1f2(Param & param, const int Z, const double ephotonev, 
                     const Scattering & scat, const Edge & edge, 
                     double & sf1out, double & sf2out);


/// \brief Calculates multilayer reflectivity and transmission for given photon 
///        energy, array of incident angles, and multilayer physical parameters.
/// \param[in] photonlam photon wavelength in angstrom to calc optical constants
/// \param[in] lambdaSqFac 8*pi^2/lambda^2
/// \param[in] surf structure with all surface information
/// \param[in] currGroup group number in which to calculate reflectivity
/// \param[in] angle structure with all angle information
/// \param[in] param structure with all parameter information
/// \param[out] sreflect reflectivity array for a mirror group, single energy,
///             and an array of incident angles
/// \param[out] strans transmission array for a mirror group, single energy,
///             and an array of incident angles
///
/// Calculates multilayer reflectivity and transmission for a given photon 
/// energy, array of incident angles, and multilayer physical parameters. The 
/// energy-dependent parts of the optical constants for each unique material 
/// are already calculated in sf1material and sf2material. The transmission is 
/// calculated at the boundary between the multilayer film and the first thick 
/// layer (substrate). Transmission through the substrate and remaining thick 
/// layers is calculated outside this routine during ray-tracing, using the 
/// mass-absorption coefficient for each layer, layer thickness, and photon 
/// direction.
void multilayerRefl(const double photonlam, const double lambdaSqFac, 
                    const int currGroup, const Surface & surf, 
                    const Angle & angle, const Param & param, 
                    const std::vector<double> & sf1material, 
                    const std::vector<double> & sf2material, 
                    std::vector<double> & sreflect, 
                    std::vector<double> & strans);


/// \brief Calculates the reduction in reflectivity due to “roughness”
/// \param[in] roughModel roughness model to use
/// \param[in] sigmaRoughnessSq 
/// \param[in] lambdaSqFac 
/// \param[in] gs1sq 
/// \param[in] g1_s first g value, S polarization
/// \param[in] g1_p first g value, P polarization
/// \param[in] g2_s second g value, S polarization
/// \param[in] g2_p second g value, P polarization
/// \param[in] lambdaSqFac 
/// \param[out] lroughfac_s multiplying factor for modifying the reflectivity
///             due to the roughness, S-polarization
/// \param[out] lroughfac_p multiplying factor for modifying the reflectivity
///             due to the roughness, P-polarization
///
/// Calculates the inter-layer reduction in reflectivity due to “roughness” 
/// (limitation on smoothness due to thermal vibrations). Two models can be 
/// specified: the Névot-Croce model, or the Debye-Waller model. The routine 
/// calculates the real and imaginary components of the multiplying factor for 
/// the reflected amplitude (not reflectivity) for one pair of layers.
/// The roughness factor is now the same for S and Ppolarization for both DW 
/// and NC models, so the g-factor inputs for P-polarization are now redundant. 
/// (No support in the literature was found for a polarization dependence of 
/// roughness factor, and none is expected because the roughness model 
/// statistically accounts for thermal vibrations of the atomic lattice on the 
/// reflecting surface, and the random nature of these fluctuations would not 
/// favor one type polarization over the other.)
void calcRoughness(const std::string & roughModel, const double sigmaRoughnessSq, 
                   const double lambdaSqFac, const std::complex<double> & gs1sq,
                   const std::complex<double> g1_s, 
                   const std::complex<double> g1_p,
                   const std::complex<double> g2_s,
                   const std::complex<double> g2_p,
                   std::complex<double> & lroughfac_s,
                   std::complex<double> & lroughfac_p);



/**********************************************
 * 		parameter retrieval functions
**********************************************/

/// \brief Report an error with getting a parameter
/// \param[in] parname name of parameter that caused an error
/// \param[in] status the error status that was encountered
/// \return message string if an error occurred getting a parameter
/// \internal
/// \note this code is copied from astroh/ahapp
std::string reportGetParErr(const std::string & parname, int status);


/// \brief Get string parameter
/// \param[in] parname name of parameter
/// \return string value of parameter
/// \internal
/// \note this code is copied from astroh/ahapp
std::string getParString(const std::string & parname);


/// \brief Get boolean parameter
/// \param[in] parname name of parameter
/// \return boolean value of parameter
/// \internal
/// \note this code is copied from astroh/ahapp
bool getParBool(const std::string & parname);


/// \brief Get double parameter
/// \param[in] parname name of parameter
/// \return double value of parameter
/// \internal
/// \note this code is copied from astroh/ahapp
double getParDouble(const std::string & parname);


/// \brief Get integer parameter
/// \param[in] parname name of parameter
/// \return int value of parameter
/// \internal
/// \note this code is copied from astroh/ahapp
int getParInt(const std::string & parname);


/// \brief converts a char array to a string
/// \param[in] input character array to turn into a string
/// \return C++ string value of the input C style string
std::string convertCharToString(char * input);


/// \brief return resolved filename: from CALBD or REFDATA or the original
/// \param[in] filename either the name of FITS file, or CALDB, or REFDATA
/// \param[in] filetype one- or two-word description of file, for info messages
/// \param[in] instrume INSTRUME keyword from par file.
/// \param[in] detnam DETNAM keyword
/// \param[in] codename keyword for searching CALDB CODENAM keywords
/// \param[in] datetime DATE-OBS keyword
/// \param[in] expression optional expression, ie for CBD keywords. default="-"
/// \param[in] telescop TELESCOP keyword from input file, optional. For 
///                   leapsec file, this would be 'gen'. Every other tool would 
///                   use the TELESCOP keyword, which should always be 
///                   "HITOMI".  default="HITOMI"
/// \return actual name of file
/// \internal
/// this is copied from astroh/mission/lib/ahmission/ahmission/caldb.h
std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression="-",
                    const std::string & telescop="HITOMI");


/// \brief Writes the input parameters to the log file
/// \internal
/// \note This code copied from astroh/ahapp.cxx
void writeParametersToLog();


/**********************************************
 * 		misc functions
**********************************************/

void cxasq(const std::complex<double> & cxin, std::complex<double> & cxout);
void cxpower(const std::complex<double> & cxin, double xpower, std::complex<double> & cxout);
void cxexponent(const std::complex<double> & cxin, std::complex<double> & cxout);
void cxatimesb(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout);
void cxnumratio(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout);
void cxdiffoversum(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout);
void cxoneplusaoveronepab(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout);
void cxaplusboveronepab(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout);


/// \brief convert a string (list of numbers) into a vector of doubles
/// \param[in] stringList The string that holds the list of numbers
/// \param[out] doubleList The vector of numbers, as doubles
///
/// Convert a string which is a list of numbers, from an input parameter such 
/// as energy, into a one dimensional vector of doubles.
void listStringsToDoubles(const std::string & stringList, std::vector<double> & doubleList);

/// \brief get the energy unit of a fits column, from the TUNIT keyword
/// \param[in] fits_fp the file pointer of the fits file (must be at correct extension)
/// \param[in] filename the name of the file
/// \param[in] energyColNum the energy column
/// \return the enum (e_eV or e_keV) of the energy unit in the fits file
energyUnits_e getEnergyUnit(fitsfile * fits_fp, const std::string & filename,
                          int energyColNum);


/// \brief convert an energy grid into eV, in place
/// \param[in] energyUnit enum stating input energy unit (keV, eV)
/// \param[in/out] energyGrid vector with input energy, will be converted to eV
void storeEnergyIneV(energyUnits_e energyUnit, std::vector<double> & energyGrid);


/// \brief returns a string version of an int
/// \param[in] value the int that needs to be a string
/// \return a string version of the int
std::string intToString(const int value);


/// \brief returns a string version of a double
/// \param[in] value the double that needs to be a string
/// \return a string version of the double
/// \internal
/// \note sets the precision at 15
std::string doubleToString(const double value);


/// \brief returns a string version of a long
/// \param[in] value the long that needs to be a string
/// \return a string version of the long
std::string longToString(long value);


/// \brief string comparison, case insensitive
/// \param[in] str1 the first string to compare
/// \param[in] str2 the second string to compare
/// \return true if the strings are equal, case insensitive; false if not
bool isEqualCaseInsens(const std::string & str1, const std::string & str2);


/// \brief return the maximum int in a one dimensional vector of ints
int getMaxInt(std::vector<int> & vec);


/// \brief pulls out the filename from a string with the entire path
/// \param[in] str the full path and filename of the file
/// \return only the filename, not the path
/// \internal
/// \note this code is adapted from http://www.cplusplus.com/reference/string/string/find_last_of/
std::string getFilename(const std::string & str);


/// \brief Checks that the CFITSIO status is 0.  If not, it throws an error.
/// \param[in] status the status to check, returned from a prior CFITSIO call 
/// \param[in] doing what the prior CFITSIO call was doing 
/// \param[in] filename The file currently being accessed 
/// \internal
/// \note this code is adapted from attconvert function reportFITSError()
void checkForFITSError(const int status, const std::string & doing, const std::string & filename);



//+++ for my testing
void print2Dmatrix(std::vector< std::vector<double> > & matrix);
void print2Dmatrixlong(std::vector< std::vector<long> > & matrix);


// end addtogroup
/** @} */


#endif // XRTREFTABLE_XRTREFTABLE_LIB_H



/* Revision Log
  $Log: xrtreftable_lib.h,v $
  Revision 1.23  2016/02/19 01:22:30  klrutkow
  updated author

  Revision 1.22  2016/02/18 21:54:10  klrutkow
  changed default TELESCOP to HITOMI in resolve() function

  Revision 1.21  2015/09/15 17:18:07  klrutkow
  added writeParametersToLog

  Revision 1.20  2015/09/10 02:36:26  klrutkow
  updated comment

  Revision 1.19  2015/08/25 03:40:46  klrutkow
  added par atmsctng to Par struct

  Revision 1.18  2015/08/13 03:12:21  klrutkow
  added m_detnam to surface struct ; added resolve() function to query CALDB

  Revision 1.17  2015/06/30 00:41:06  klrutkow
  added longToString, for CALDB queries

  Revision 1.16  2015/01/29 20:53:52  klrutkow
  updated params for issue 472

  Revision 1.15  2015/01/24 19:00:53  klrutkow
  updated tool with new parameters, per issue 472

  Revision 1.14  2014/11/07 15:43:59  klrutkow
  implement heaapp for startup() and shutdown()

  Revision 1.13  2014/10/06 14:10:08  klrutkow
  energy param accepts filename or list of energies; only write substrate (thick) keywords to front extension, not all materials; write all materials to the mass absorption extension; fix the mass absorption calculation in calcsf1sf2materials()

  Revision 1.12  2014/09/04 16:50:07  klrutkow
  updates for v1.01 delivery

  Revision 1.11  2014/08/11 20:46:27  klrutkow
  updated default parameters

  Revision 1.10  2014/03/07 14:31:15  klrutkow
  changed signature of cx__ functions to accept a complex number rather than two doubles

  Revision 1.9  2014/02/28 21:08:40  klrutkow
  updating many misc changes

  Revision 1.8  2014/02/25 18:16:10  klrutkow
  misc updates

  Revision 1.7  2014/02/20 20:51:06  klrutkow
  checking in recent updates for new B05 tag

  Revision 1.6  2014/02/03 13:18:04  peachey
  Temporarily add internal implementation of startUp and shutDown functions
  pending complete integration of these into headas.

  Revision 1.5  2014/02/03 02:58:52  klrutkow
  replaced cxaminusboveronepab with cxaplusboveronepab

  Revision 1.4  2014/01/31 15:56:58  klrutkow
  cleaned up code

  Revision 1.3  2014/01/30 16:29:55  klrutkow
  updated tool, single layer seems to work now
 

*/


