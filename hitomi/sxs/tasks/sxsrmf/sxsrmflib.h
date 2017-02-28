/// \file sxsrmflib.h
/// \brief functions for sxsrmf
/// \author Mike Dutka
/// \date $Date: 2015/12/08 18:43:55 $

/// \addtogroup tool_sxsrmf
/// \section tool_sxsrmf_sxsrmflib Supplementary functions for sxsrmf
///
///  
/// \subsection tool_sxsrmf SXS rmf 
/// 
/// The task calculates rmf for each pixel and grade
///

#ifndef TOOL_SXSRMF_SXSRMFLIB_H
#define TOOL_SXSRMF_SXSRMFLIB_H

#include "ahgen/ahversion.h"
AHVERSION(TOOL_SXSRMF_SXSRMFLIB,"$Id: sxsrmflib.h,v 1.5 2015/12/08 18:43:55 mdutka Exp $")

#include <string>
#include <map>
#include <vector>

/// \addtogroup tool_sxsrmf
namespace sxsrmf_sigma {

/** \addtogroup tool_sxsrmf
 *  @{
 */

 
    
/// \brief Store information needed to calculate the response from the CALDB
///  sigma table which contains information about the gaussion componenet and
///  pixel resolution 
/// 
/// This structure will store the center energy, the FWHM of the gaussian and 
/// the value of either RANGEHIG, RANGEMID or RANGELOW for one resolustion, one 
/// pixel, and all energy bins.
struct DataType{
DataType():  m_naxis2(0), m_egrid_tab(0), m_fwhm(0) { 
               for (int ii=0; ii<3; ++ii) m_de_gaus[ii] = 0; 
             }
     
  double m_de_gaus[3];
  long long m_naxis2;
  double * m_egrid_tab;
  double ** m_fwhm;
    
  ~DataType() { }
} ;

void load(const std::string & filename, DataType & dat, long nfrac,
          std::vector<int> pixInfile, std::vector<int> resolInfile);  


void cleanup(DataType & dat,long nfrac);
/** @} */

}  // namespace sxsrmf_sigma


namespace sxsrmf_tau {

/// \brief Structure which stores information needed to calculate the response from the
///  CALDB tau table which contains the information need for the medium a large rmf 
///  
///  \param[in] m_npeak: the number off escape peaks
///  \param[in] m_de_exp: half-energy width over which expon. applies
///  \param[in] m_egrid_tab: (0,ngrid_in-1) table grid energies  
///  \param[in] m_tau: “decay width” for exponential tail
///  \param[in] m_frac_exp: fraction of integrated flux in exponential
///  \param[in] m_frac_cont: fraction of integrated flux in escape continuum
///  \param[in] m_frac_peak: fraction of integrated flux in escape peaks
///  \param[in] m_de_peak array to store the column names for each of the escape peaks
struct DataType{
DataType(): m_npeak(0), m_de_exp(0.), m_naxis2(0), m_egrid_tab(0), m_tau(0), 
    m_frac_exp(0), m_frac_cont(0), m_frac_peak(0), m_de_peak(0) { }
    long long m_npeak;
    double m_de_exp;
    long long m_naxis2;
    double * m_egrid_tab;
    double * m_tau;
    double * m_frac_exp;
    double * m_frac_cont;
    double ** m_frac_peak;
    double * m_de_peak;
};

void load(const std::string & filename, DataType & dat, std::string whichrmf);
    
 void cleanup(DataType & dat);

} // namespace sxsrmf_tau


#endif /* TOOL_SXSFLAGPIX_SXSFLAGPIXLIB_H */

/* Revision Log
 $Log: sxsrmflib.h,v $
 Revision 1.5  2015/12/08 18:43:55  mdutka
 updating sxsrmf to handle multiple pixels and grades

 Revision 1.4  2015/07/31 18:17:54  mwitthoe
 sxsrmf: fix bug where only the small RMF was being computed; updated tool prologue to standard form; general clean-up

 Revision 1.3  2014/08/04 17:19:36  mdutka
 Fixed converting double to integer warnings

 Revision 1.2  2014/07/10 18:21:11  mdutka
 all dynamically sized arrays are now initialized to zero, added dynamic sizing to containers which store data from the CALDB information

 Revision 1.1  2014/06/30 19:02:36  mdutka
 Added sxsrmf tool

 Revision 1.4  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.3  2014/01/09 20:33:50  rshill
 Code review comments

 Revision 1.2  2013/11/14 18:44:41  mwitthoe
 sxsflagpix: correct Doxygen section labels in sxsflagpixlib.h

 Revision 1.1  2013/11/12 17:45:03  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)



 BELOW IS THE REVISION LOG FOR sxspixelmap.h BEFORE RENAMING TO sxsflagpixlib.h

 Revision 1.4  2013/07/25 19:47:33  mwitthoe
 fix doxygen tags

 Revision 1.3  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.2  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.1  2013/01/02 19:09:52  mwitthoe
 add new ahcaldb library for the SXS pixel map


*/
