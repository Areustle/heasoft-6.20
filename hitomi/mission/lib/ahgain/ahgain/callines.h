/// \file callines.h
/// \brief Load calibration line information from CALDB file.
/// \author Mike Witthoeft
/// \date $Date: 2015/03/11 16:01:18 $

/// \addtogroup mod_ahgain
/// \section ahgain_callines Load calibration line information from CALDB file - callines
///
/// Read CALDB table containing calibration lines for different sources.  Each
/// calibration source occupies a separate extension.  Several line components
/// may be present for any source.  The extension tables contain the position,
/// width, and relative height for each Lorentzian component.
///

#ifndef AHGAIN_CALLINES_H
#define AHGAIN_CALLINES_H

#include "ahgen/ahversion.h"
AHVERSION(AHGAIN_CALLINES,"$Id: callines.h,v 1.2 2015/03/11 16:01:18 mwitthoe Exp $")

#include <string>
#include <vector>

/// \addtogroup mod_ahgain
namespace callines {

/** \addtogroup mod_ahgain
 *  @{
 */

/// \brief For brevity: a vector of doubles.
typedef std::vector<double> vecdbl;


/// \brief Store attributes for all line components of a single calibration
///  source
struct CalLines {
  std::string m_label;         ///< label of calibration source
  int m_nlines;                ///< number of component lines
  double m_avenergy;           ///< center-of-mass energy of all lines
  vecdbl m_energy;             ///< vector of component positions (eV)
  vecdbl m_width;              ///< vector of component Lorentzian widths (eV)
  vecdbl m_amplitude;          ///< vector of component relative heights
};

/// \brief Read data file and load data into CalLines structure.
/// \param[in] filename name of data file
/// \param[in] extname name of calibration source, e.g. Mnka
/// \param[out] dat data from CALDB file
void load(const std::string & filename, const std::string & extname, 
          CalLines & dat);

/// \brief Return smallest line energy.
/// \param[in] dat data from CALDB file
/// \return minimum line energy
double getMinEnergy(CalLines& dat);

/// \brief Return largest line energy.
/// \param[in] dat data from CALDB file
/// \return maximum line energy
double getMaxEnergy(CalLines& dat);

/** @} */

}  // namespace callines

#endif /* AHGAIN_CALLINES_H */

/* Revision Log
 $Log: callines.h,v $
 Revision 1.2  2015/03/11 16:01:18  mwitthoe
 ahgain library: minor documentation improvements

 Revision 1.1  2014/07/17 19:47:20  mwitthoe
 add ahgain library which contains routines for fitting event data with a calibration feature


*/
