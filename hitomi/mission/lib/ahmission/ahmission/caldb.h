/// \file caldb.h
/// \brief functions to act on the CALDB input parameters for Astro-H.
/// \author Kristin Rutkowski
/// \date $Date: 2016/02/18 21:54:45 $

/// \addtogroup mod_ahmission
/// \section ahmission_caldb Implement CALDB query
///
/// Many input files for the Astro-H tools can be from CALDB.  With only minor
/// variations, each CALDB query is essentially the same block of code.  
///

#ifndef AHMISSION_CALDB_H
#define AHMISSION_CALDB_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_CALDB,"$Id: caldb.h,v 1.5 2016/02/18 21:54:45 klrutkow Exp $")

#include <string>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief CALDB query
namespace caldb {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief return resolved filename: from CALDB or REFDATA or the original
/// \param[in] filename either the name of FITS file, or CALDB, or REFDATA.  
///                   This should be the user input to a parameter query.
/// \param[in] filetype one- or two-word description of file, for info messages
/// \param[in] instrume INSTRUME keyword from input file.  Depending on file 
///                   desired, this would be the entire keyword (ie: HXI1), or 
///                   just the first three letters (ie: HXI).
/// \param[in] detnam DETNAM keyword, from input file
/// \param[in] codename keyword for searching CALDB CODENAM keywords
/// \param[in] datetime DATE-OBS keyword from input file, or "yyyy-mm-dd", 
///                   or "-", or "now"
/// \param[in] expression optional expression, ie for CBD keywords. default="-"
/// \param[in] telescop TELESCOP keyword from input file, optional. For 
///                   leapsec file, this would be 'gen'. Every other tool would 
///                   use the TELESCOP keyword, which should always be 
///                   "HITOMI".  default="HITOMI"
/// \return If file is retrieved from CALDB or refdata, the function returns 
///                   the name of the file with path and extended syntax, 
///                   for example "/path/file.fits[2]".  If the user input the 
///                   file name, the function returns the same string, unaltered
std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression="-",
                    const std::string & telescop="HITOMI");


/** @} */

}  // namespace caldb

}  // namespace ahmission

#endif /* AHMISSION_CALDB_H */

/* Revision Log
 $Log: caldb.h,v $
 Revision 1.5  2016/02/18 21:54:45  klrutkow
 changed default TELESCOP to HITOMI in resolve() function

 Revision 1.4  2015/10/02 18:04:50  klrutkow
 added better documentation

 Revision 1.3  2015/07/30 00:52:45  klrutkow
 updated doxygen

 Revision 1.2  2015/07/14 18:10:35  klrutkow
 udpated comments

 Revision 1.1  2015/07/14 16:16:47  klrutkow
 added new, general, CALDB query in caldb.h and caldb.cxx

 */
