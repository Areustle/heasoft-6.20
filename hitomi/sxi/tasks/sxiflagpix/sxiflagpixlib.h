/// \file sxiflagpixlib.h
/// \brief functions to act on the CALDB and FITS files containing the SXI pixel indices
/// \author Andy Sargent
/// \date $Date: 2016/08/11 22:49:12 $

/// \addtogroup tool_sxiflagpix
/// \section tool_sxiflagpix_sxiflagpixlib SXI bad pixels - sxiflagpixlib
///
/// Flagged pixels in SXI instrument: Node, Segment, CCD

#ifndef TOOL_SXIFLAGPIX_SXIFLAGPIXLIB_H
#define TOOL_SXIFLAGPIX_SXIFLAGPIXLIB_H

#include "ahfits/ahfits.h"
#include "ahgen/ahversion.h"
AHVERSION(TOOL_SXIFLAGPIX_SXIFLAGPIXLIB,"$Id: sxiflagpixlib.h,v 1.24 2016/08/11 22:49:12 rshill Exp $")

#include <string>
#include <vector>

const int SXI_CCD_SIZE = 640;
const int SXI_DET_SIZE = 1810;

/// \addtogroup tool_sxiflagpix
namespace sxiflagpixlib {

/** \addtogroup tool_sxiflagpix
 *  @{
 */

typedef std::vector<int> vecInt;

/// \brief Define parameter file holding information
struct Params {

  std::string m_infile;          ///< name of input file
  std::string m_outfile;         ///< name of output file
  std::string m_outbadpix;       ///< name of flagged pixels output file
  std::string m_outbadimg;       ///< name of flagged pixels image output file

  std::string m_badpixfile;      ///< name of bad pixels CALDB file
  std::string m_hotpixfile;      ///< name of hot pixels input file
  std::string m_flickpixfile;    ///< name of flickering pixels input file
  std::string m_maskfile;        ///< name of mask CALDB file

  int m_npixnbr;                 ///< distance from a hot pixel, bad pixel, flickering pixel, etc.
  int m_nboundnbr;               ///< Pixel distance defining a neighbhor from
  int m_citrailnbr;              ///< distance trailed from a SCI row to flag a neighbor
  int m_ciprenbr;                ///< distance preceding from a SCI row to flag a neighbor
  int m_echonbr;                 ///< distance preceding from a SCI row to flag a neighbor
  int m_echomin;                 ///< minimum events for CR echo fraction calculation
  int m_echospth;                ///< split threshold for CR echo fraction calculation
  double m_echofrac;             ///< minimum fraction of hits defining a cosmic ray echo
  bool m_echoflag;               ///< flag CR echo pixels yes/no
  std::string m_gtifile;         ///< input GTI file for CR echo pixel flagging, or NONE
  bool m_echomap;                ///< output CR echo pixel fraction map, or NONE
  std::string m_bad_status_str;  ///< Comma deliminated list fo Bad status bits

  bool m_copyphas;               ///< Copy PHAS from telemetry before processing (yes/no)
  bool m_resetflags;             ///< resets sxiflagpix STATUS flags  

  std::vector<int> m_bad_status; ///< vector containing bad status bits
};

/// \brief define type holding information
struct PixArray {

  // Column values
  vecInt m_ccdid;         ///< CCD ID pixel
  vecInt m_actx;          ///< ACTX pixel
  vecInt m_acty;          ///< ACTY pixel
  vecInt m_yextend;       ///<YEXTEND pixel badpix only
  int m_numpix;           ///< Number of flagged events
};

/// \brief read FITS file and load data
/// \param[in] filename   name of data file
/// \param[out] dat       data from FITS file
/// \param[in] tstart_in  observation start time
/// \param[in] tstop_in   observation stop time
void loadPix(const std::string & filename, PixArray & dat, double tstart_in, double tstop_in);

/// \brief read badPixfile and load data
/// \param[in] filename   filename of badpix file
/// \param[in] tstartEvt  TSTART keyword from event file
/// \param[out] dat       data struct for badpix
void loadBadPix(const std::string & filename, double tstartEvt, PixArray & dat); 

/// \brief read hotPixfile and load data
/// \param[in] filename   filename of badpix file
/// \param[in] tstartEvt  TSTART keyword from event file
/// \param[in] tstopEvt   observation stop time
/// \param[out] dat       data struct for badpix
void loadHotPix(const std::string & filename, double tstartEvt, double tstopEvt, PixArray & dat);

/// \brief clear vectors in PixArray structure
/// \param[in,out] dat    data struct for badpix
void clearPix(PixArray & dat);

/** @} */

} // namespace sxiflagpixlib

#endif /* TOOL_SXIFLAGPIX_SXIFLAGPIXLIB_H */

/* Revision Log
 $Log: sxiflagpixlib.h,v $
 Revision 1.24  2016/08/11 22:49:12  rshill
 Added echoflag, gtifile, and echomap parameters.

 Revision 1.23  2016/08/05 05:21:37  rshill
 Added cosmic ray echo detection.

 Revision 1.22  2015/08/17 23:45:03  asargent
 Bug fixes: changed image writing to long long, fixed yextend boundary, forced LL with hex values. Cleaned up mask file reading. Added +/- 1 day for hot pixels

 Revision 1.21  2015/08/10 13:44:04  mwitthoe
 sxiflagpix: conform to standard header; add parameter stamping to log file; make yes/no lowercase in parameter file; general clean-up

 Revision 1.20  2015/06/02 20:06:10  mdutka
 fixed bug with loading hotpix file

 Revision 1.19  2015/05/20 21:03:05  mdutka
 commiting change from hiroya, chages detailed in trf 04-27

 Revision 1.18  2015/03/18 15:04:38  mdutka
 sxi tool changes see issue #490

 Revision 1.17  2014/12/30 22:06:24  mdutka
 Updating parameter list see issue #472

 Revision 1.16  2014/12/11 21:16:36  asargent
 Bug fixes: -1 was being written to good elements in output image file. Bug fix is in maskAreaDiscrimination routine. Added constraints to segment and uses the active readnode to determine which coordinates to flag. Sped up routine checkBad3x3 by passing in statusMask by reference. New parameter copyphas to copy phas from telemetry before updating.

 Revision 1.15  2014/10/15 21:19:38  asargent
 Fixed bug where during setBit23 where it was unsetting unneccessary bits. Added global variable SXI_DET_SIZE.

 Revision 1.14  2014/09/12 20:06:22  asargent
 Revamped sxiflagpix tool: statusMask is now stored in ACT coordinates rather than DET.

 Revision 1.13  2014/06/23 13:56:13  asargent
 Updated output bad pixel file to include entire header information from input event file. Changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

 Revision 1.12  2014/05/14 20:44:59  asargent
 Removed extraneous functions

 Revision 1.11  2014/02/14 17:18:18  asargent
 Updated ahfits image reading/writing

 Revision 1.10  2014/02/12 15:13:04  asargent
 General housecleaning to better match other tools. Cleaned up and reorganized most comments. Cleaned up structures and added a new structure for parameters. Converted flagged pixel structure data to vectors.

 Revision 1.9  2014/01/17 21:30:16  asargent
 Added comments

 Revision 1.8  2014/01/09 21:58:36  asargent
 Added in variable comments

 Revision 1.7  2014/01/07 18:02:11  asargent
 Added in image read/write access from ahfits. Fixed passing around of arrays and changed statusMask to type ahfits::Img2dLng.

 Revision 1.6  2013/12/19 20:45:53  asargent
 Added in some placeholders for mask array CALDB FITS IMAGE file.

 Revision 1.5  2013/12/12 22:15:24  asargent
 Various changes to function declaration parameters

 Revision 1.4  2013/11/22 14:16:47  asargent
 New placeholder function for getting time based bad pixel arrays

 Revision 1.3  2013/11/18 18:07:23  asargent
 More function call options sxiflagpixlib.cxx

 Revision 1.2  2013/11/15 22:30:57  asargent
 Changes to some get functions for retrieval of array data (detx,dety,yextend,segment)

 Revision 1.1  2013/11/14 21:43:26  asargent
 Initial version of sxiflagpixlib.h

 
*/
