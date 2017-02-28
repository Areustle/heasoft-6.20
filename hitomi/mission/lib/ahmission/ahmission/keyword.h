/// \file keyword.h
/// \brief functions to copy mission-specific keywords to new files
/// \author Kristin Rutkowski
/// \date $Date: 2015/07/29 18:22:48 $

/// \addtogroup mod_ahmission
/// \section ahmission_keyword Implement keyword-specific function
///
/// General functions to copy a category of keywords from source file to a 
/// destination file
///

#ifndef AHMISSION_KEYWORD_H
#define AHMISSION_KEYWORD_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_KEYWORD,"$Id: keyword.h,v 1.3 2015/07/29 18:22:48 klrutkow Exp $")

#include "ahfits/ahfits.h"

#include <string>
#include <map>                // std::map
#include <list>



/// \ingroup mod_ahmission
namespace ahmission {

/// \brief copy keywords queries
namespace keyword {

/** \addtogroup mod_ahmission
 *  @{
 */
  
  
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
  
/// enumeration of filetpes to which to copy keywords.  This list determines
/// which keywords are copied to the output file
enum Filetype {e_PRIMARY, e_HK, e_EVENT, e_RATE, e_GTI};

/// enumeration of filetpes to which to copy keywords.  This list determines
/// which keywords are copied to the output file
enum Category {e_COORDINATE, e_OBSERVATION, e_PROCESSING, e_TIMING};


// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

/// \brief copy the keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
/// \param[in] category type of keywords to be copied, from enumerated list
/// 
/// The actual logic of copying the appropriate keywords to the appropriate 
/// destination extension type is in this function; all the other functions 
/// work by calling this function.
void copyKeywordsBase(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype, Category category);

// ---------------------------------------------------------------------------

/// \brief copy the coordinate keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
void copyCoordinateKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype);

// ---------------------------------------------------------------------------

/// \brief copy the observation keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
void copyObservationKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype);

// ---------------------------------------------------------------------------

/// \brief copy the processing keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
void copyProcessingKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype);

// ---------------------------------------------------------------------------

/// \brief copy the timing keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
void copyTimingKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype);

// ---------------------------------------------------------------------------

/// \brief copy all the keywords appropriate for the file type
/// \param[in] src ahfits FITS file pointer to source file
/// \param[in] dest ahfits FITS file pointer to destination file
/// \param[in] filetype type of extension in destination file, from enumerated list
///
/// This function calls each of the individual copy*Keywords() functions
void copyAllKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype);

// ---------------------------------------------------------------------------

/// \brief copy the keywords appropriate for the file type
/// \param[in] category which type keyword we want the 
/// \return 
const std::list<std::string>& getKeywordsMap(Filetype filetype, Category category);

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

/** @} */

}  // namespace keyword

}  // namespace ahmission

#endif /* AHMISSION_KEYWORD_H */

/* Revision Log
 $Log: keyword.h,v $
 Revision 1.3  2015/07/29 18:22:48  klrutkow
 implemented full keyword-copying functionality

 Revision 1.2  2015/07/29 01:59:01  klrutkow
 added declarations to other necessary keyword-copying functions (not fully implemented yet)

 Revision 1.1  2015/07/27 17:06:50  klrutkow
 added new files to copy keywords

 
 */