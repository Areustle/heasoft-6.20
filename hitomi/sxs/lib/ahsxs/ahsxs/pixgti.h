/// \file pixgti.h
/// \brief Load and use pixel-dependent GTI files
/// \author Mike Witthoeft
/// \date $Date: 2016/08/10 16:27:32 $

/// \addtogroup mod_ahsxs
/// \section ahsxs_pixgti SXS pixel GTI files - pixgti
///
/// This library allows SXS pixel-dependent GTI files to be loaded in two
/// different formats:
///
///  1. One extension with columns: START, STOP, PIXEL
///  2. 36 extensions (one per pixel) with columns: START, STOP
///
/// The library does not load all GTIs into memory.  Instead FITS pointers
/// are prepared for reading the FITS rows as necessary for checking if a
/// given time is in a GTI.  It is assumed that the input GTI files are
/// sorted by START.
///

#ifndef AHSXS_PIXGTI_H
#define AHSXS_PIXGTI_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_PIXGTI,"$Id: pixgti.h,v 1.1 2016/08/10 16:27:32 mwitthoe Exp $")

#include "ahsxs/ahsxs.h"
#include "ahfits/ahfits.h"

/// \ingroup mod_ahsxs

namespace ahsxs {

/** \addtogroup mod_ahsxs
 *  @{
 */

struct GTIFile {

  GTIFile() : m_fpgen(0), m_routgen(0), m_havegen(false), m_haveanypix(false),
              m_havepixcol(false), m_startgen(0.), m_startgen_null(0),
              m_stopgen(0.), m_stopgen_null(0) {
    for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {
      m_fppix[ipix]=0;
      m_routpix[ipix]=0;
      m_havepix[ipix]=0;
      m_startpix[ipix]=0.;
      m_startpix_null[ipix]=0;
      m_stoppix[ipix]=0.;
      m_stoppix_null[ipix]=0;
      m_pixel[ipix]=0.;
    }
  }

  ~GTIFile() {
    if (m_routgen != 0) delete m_routgen, m_routgen=0;
    if (m_fpgen != 0) ahfits::close(m_fpgen);
    for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {
      if (m_routpix[ipix] != 0) delete m_routpix[ipix], m_routpix[ipix]=0;
      if (m_fppix[ipix] != 0) ahfits::close(m_fppix[ipix]);
    }
  }

  std::string m_filename;                    // name of GTI file
  ahfits::FilePtr m_fpgen;                   // ahfits pointer to general GTI extension
  ahfits::FilePtr m_fppix[ahsxs::NPIXEL];    // ahfits pointers to pixel GTI extensions
  ahfits::Router* m_routgen;                 // ahfits router for general GTI extension
  ahfits::Router* m_routpix[ahsxs::NPIXEL];  // ahfits router for pixel GTI extensions
  bool m_havegen;                            // true if general GTI extension present in file
  bool m_havepix[ahsxs::NPIXEL];             // true if pixel GTI extensions present
  bool m_haveanypix;                         // true if any pixel GTI extensions present
  bool m_havepixcol;                         // true if have a pixel-dependent GTI with a PIXEL column
  double m_startgen;                         // current START for general GTI
  char m_startgen_null;                      // NULL flag for START for general GTI
  double m_stopgen;                          // current STOP for general GTI
  char m_stopgen_null;                       // NULL flag for STOP for general GTI
  double m_startpix[ahsxs::NPIXEL];          // current START for each pixel GTI
  char m_startpix_null[ahsxs::NPIXEL];       // NULL flag for START for each pixel GTI
  double m_stoppix[ahsxs::NPIXEL];           // current STOP for each pixel GTI
  char m_stoppix_null[ahsxs::NPIXEL];        // NULL flag for STOP for each pixel GTI
  double m_pixel[ahsxs::NPIXEL];             // current PIXEL for each pixel GTI
};

/// \brief Open SXS GTI file and prepare for reading.
/// \param[in] filename name of GTI file
/// \param[out] gti structure with GTI state
///
/// This function expects a GTI file with up to 37 GTI extensions.  One 
/// extension contains GTI valid for all pixels (DETNAM = PIXEL).  The other
/// 36 extensions are pixel-dependent (DETNAM=PIXnn where nn is the pixel 
/// number, e.g. PIX03 for pixel 3).  None of the extensions are individually
/// required.  If an extension does not have the DETNAM keyword defined, then
/// it will be assumed to apply to all pixels.  If extended syntax is used to
/// specify a particular extension, then only the extension will be processed.
void loadGTIFile(const std::string& filename, GTIFile& gti);

/// \brief Read general GTI from next non-NULL row in file.
/// \param[in,out] gti structure with GTI state
/// \return true if reading was successful; false if end of file or no GTI present
///
/// Will attempt to read next GTI where both START and STOP are not NULL.  If
/// end of file is reached, this function returns false; otherwise, if a valid
/// GTI was read, then the function returns true.
bool readNextGeneralGTI(GTIFile& gti);

/// \brief Read pixel GTI from next row in file.
/// \param[in,out] gti structure with GTI state
/// \param[in] pixel pixel number to read
/// \return true if reading was successful; false if end of file or no GTI present
///
/// Will attempt to read next GTI for the given pixel where both START and STOP
/// are not NULL.  If end of file is reached, this function returns false;
/// otherwise, if a valid GTI was read, then the function returns true.
bool readNextPixelGTI(GTIFile& gti, int pixel);

/// \brief Read the first non-NULL row in general GTI.
/// \param[in,out] gti structure with GTI state
/// \return true if reading was successful; false if end of file or no GTI present
bool readFirstGeneralGTI(GTIFile& gti);

/// \brief Read the first non-NULL pixel GTI
/// \param[in,out] gti structure with GTI state
/// \param[in] pixel pixel number to read
/// \return true if reading was successful; false if end of file or no GTI present
bool readFirstPixelGTI(GTIFile& gti, int pixel);




/** @} */

}  // namespace ahsxs

#endif /* AHSXS_PIXGTI */

/* Revision Log

 $Log: pixgti.h,v $
 Revision 1.1  2016/08/10 16:27:32  mwitthoe
 ahsxs: add library for reading two types of SXS pixel-dependent GTI files: 1) one extension per pixel or 2) one extension with PIXEL column


*/
