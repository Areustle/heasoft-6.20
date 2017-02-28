/// \file engain.h
/// \brief Energy gain information for SXS
/// \author Kristin Rutkowski
/// \date $Date: 2015/05/01 00:07:57 $

#ifndef AHSXS_ENGAIN_H
#define AHSXS_ENGAIN_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_ENGAIN,"$Id: engain.h,v 1.3 2015/05/01 00:07:57 klrutkow Exp $")

#include "ahsxs/ahsxs.h"

#include <vector>                       // std::vector


/// \ingroup mod_ahsxs

namespace ahsxs {

namespace engain {
 
/** \addtogroup mod_ahsxs
 *  @{
 */



// ****************************************************************************

/// shorten the declarations for 2D vectors of doubles, used in EnergyGainData
typedef std::vector< std::vector<double> > dblVector2D;
  

/// \brief struct holding set of polynomial coefficients for each 
///  grade (high, mid, low) for a specific temperature.
///
/// The holds the data from the CALDB file containing the energy gain function
/// as a function of temperature.
/// There is a set of polynomial coefficients for each of 36 pixel and 3 grades,
/// for different temperatures.  This struct will hold the coefficient data for
/// a single temperature.
/// In this struct, each grade will have a separate array of coefficient data.
/// The arrays are two-dimensional; the first dimension has pixel index and
/// the second has the order of the coefficient (e.g. first term has coefficient
/// leading the PHA^0 term): (NPIXEL,NCOEFF)
struct EnergyGainData {
  
  // initialize values
  EnergyGainData(): m_temperature(0.0), m_ncoeff(0) { 
    m_high.resize(ahsxs::NPIXEL);
    m_mid.resize(ahsxs::NPIXEL);
    m_low.resize(ahsxs::NPIXEL);
  };

  // clean up memory 
  ~EnergyGainData(void) { };
  
  double m_temperature;                ///< temperature for this profile
  int m_ncoeff;                        ///< number of coefficients
  
  /// the size of these arrays will be: (NPIXEL,NCOEFF)
  dblVector2D m_high;                  ///< high grade coefficients
  dblVector2D m_mid;                   ///< mid grade coefficients
  dblVector2D m_low;                   ///< low grade coefficients
  
};  // end struct EnergyGainData


// ****************************************************************************


/// \brief vector of EnergyGainData structs; used to store information from
///        energy gain file
typedef std::vector<EnergyGainData> EnergyGainDataVec;


// ****************************************************************************


/// \brief stuct holding vector of individual energy gain structs, and 
///        vector of corresponding temperatures
///
/// Each EnergyGainData struct is for a single temperature from the energy gain
/// file.  the typedef EnergyGainDataVec is a vector of those structs, to hold
/// all the temperatures.  This struct will hold that EnergyGainDataVec, and a 
/// vector of the corresponding temperatures
struct AllEnergyGainData {
  
  // initialize values
  AllEnergyGainData(): m_numTemps(0), m_numCoeff(0) {};
  
  // clean up memory 
  ~AllEnergyGainData() { };
  
  ahsxs::engain::EnergyGainDataVec m_engainVec;    ///< vector of energy gain data structs (each for a single temp)
  std::vector<double> m_temps;      ///< vector of temps, corresponding to each row in file
  int m_numTemps;                   ///< number of temperatures (rows) in file
  int m_numCoeff;                   ///< number of H,M,L coefficients in file, keyword NCOEFF
  
};


// ****************************************************************************


/// \brief read energy gain file and load data into given structure
/// \param[in] filename name of energy gain file
/// \param[out] allDat data from CALDB file
///
/// 
void loadEnergyGainData(const std::string & filename, 
                        AllEnergyGainData & allDat);


// ****************************************************************************


/// \brief clear all data in structure
/// \param[in,out] dat data from CALDB file
///
/// 
void clearEnergyGainData(AllEnergyGainData & allDat);


// ****************************************************************************


/// \brief Convert energy into PHA using polynomial coefficients from gain file
/// \param[in] energy energy for which to calculate a PHA
/// \param[in] pixel Which pixel in SXS, used to access correct profile
/// \param[in] itype value from the ITYPE column in the event file.  Describes 
///               the grade and whether the event is primary or secondary.  
///               The values are:
///                   ITYPE   Description
///                   --------------------
///                    0      High Primary  - Hp
///                    1      Mid Primary   - Mp
///                    2      Mid Secondary - Ms
///                    3      Low Primary   - Lp
///                    4      Los Secondary - Ls
///                    5      Baseline      - BL
///                    6      Lost          - LO
///                    7      Rejected      - Rj 
/// \param[in] gaindat energy gain data from CALDB file for single temperature
/// \param[in] tol optional tolerance for determining when PHA values converge
/// \return The PHA corresonding to this energy, for this pixel/temperature
///
/// Used to convert energy range into PHA units; per pixel.  Takes in all the 
/// polynomial coefficients for the energy gain, and the pixel identifier 
/// (0-based).  Calculates back to determine the PHA for a given energy, for 
/// the temperature at this specific gaindat struct.  The gaindat struct is 
/// basically one row, for a single temperature, from the CALDB energy gain 
/// file.  
double reverseLookup(double energy, int pixel, int itype, 
                     const EnergyGainData & coeff,
                     double tol=1e-10);


// ****************************************************************************


/// \brief Apply the energy gain for a certain pixel and temperature to a PHA
/// \param[in] pha the input PHA, to be converted to energy  
///               The pha was converted from integer to double with random 
///               fractional part before being passed to this function
/// \param[in] pixel Which pixel in SXS, used to access correct profile
/// \param[in] itype value from the ITYPE column in the event file.  Describes 
///               the grade and whether the event is primary or secondary.  
///               The values are:
///                   ITYPE   Description
///                   --------------------
///                    0      High Primary  - Hp
///                    1      Mid Primary   - Mp
///                    2      Mid Secondary - Ms
///                    3      Low Primary   - Lp
///                    4      Los Secondary - Ls
///                    5      Baseline      - BL
///                    6      Lost          - LO
///                    7      Rejected      - Rj 
/// \param[in] gaindat energy gain data from CALDB file for single temperature
/// \param[out] energy the corrected energy
/// \param[out] energynull the null value for the corrected energy
///
/// Calculates the uncorrected PI (UPI) using a gain function 
/// derived from ground calibration  Within the US, it is also known as the 
/// energy scale correction.  Each SXS pixel is characterized by a gain curve 
/// or energy scale curve for each of three primary grades: High, Mid, and Low. 
/// Each curve is also temperature dependent. The gain (energy scale) 
/// correction is stored in a CALDB file.  This correction may change on a 
/// time scale larger than that of the drift correction (see sxsgain). The 
/// CALDB file, therefore, has to accommodate coefficients per pixel, grade, 
/// and time.  Each row in the CALDB file is contains the coefficients for 
/// every pixel for a single temperature.  The correction functions are 
/// polynomial.
void applyGain(double pha, int pixel, int itype, 
               const EnergyGainData & gaindat, 
               double & energy, char & energynull);


// ****************************************************************************





/** @} */
 
} // end namespace engain

} // end namespace ahsxs


#endif   /* AHSXS_AHSXS_H */


/* Revision Log
   $Log
 
*/