/// \file sxiflagpix.cxx
/// \brief Flag pixels for SXI event data
/// \author Andy Sargent
/// \date $Date: 2016/11/22 19:19:46 $
/// \version 1.0

/**

\defgroup tool_sxiflagpix Flag SXI pixels (sxsflagpix)
@ingroup mod_sxi_tasks

Flag events in the SXI FFF according to whether they are hot pixels, 
bad pixels or bad columns, in the calibration source region, on the 
boundary of the CCD or segment or window, or neighbors of such pixels.  
The event STATUS column is updated according to this flag.  A list of 
bad pixels is output.

STATUS bits set:

 Bit  Description
  0    All bad events set by 'bad_status' parameter                   
  1    Inside the calibration source region                           
 ------(Out of area)--------------------------------------------------
  2    Out of CCD                                                     
  3    Out of window                                                  
  4    Out of area discrimination                                     
 ------(Pixels)-------------------------------------------------------
  5    CI row                                                         
  6    Bad pixel from CALDB                                           
  7    Bad column from CALDB                                          
  8    Hot pixel from pre-pipeline                                    
  9    Flickering pixel                                               
 ------(Boundaries)---------------------------------------------------
 10    CCD boundary                                                   
 11    Window boundary                                                
 12    Segment boundary                                               
 13    Area discrimination boundary                                   
 14    At least one 3x3 surrounding pixel has a bad status            
 ------(Neighbors)----------------------------------------------------
 15    CI trailing row                                                
 16    CI preceding row                                               
 17    Preceding/following of bad column                              
 18    Neighbors of bad/hot pixel and bad column                      
 19    Neighbors of flickering pixel                                  
 20    Neighbors of preceding/following of bad column                 
 21    Neighbors of CCD/window boundary                               
 22    Neighbors of segment boundary                                  
 ------(Diagnostics)--------------------------------------------------
 30    1st trailing row of the CI rows                                
 31    1st preceding row of the CI rows                               
 32    2nd trailing row of the CI rows                                
 33    2nd preceding row of the CI rows                               
 34    3rd trailing row of the CI rows                                
 35    3rd preceding row of the CI rows                               
 ------(Cosmic Rays)--------------------------------------------------
 36    Cosmic ray echo pixel

Source files:

  sxiflagpix.cxx
  sxiflagpixlib.h
  sxiflagpixlib.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-30   MSD    Clean-up code

*/

#define AHLABEL tool_sxiflagpix
#define AHCVSID "$Id: sxiflagpix.cxx,v 1.84 2016/11/22 19:19:46 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sxiflagpixlib.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h" // proc_status
#include "ahmission/caldb.h"

// Regional includes
#include "ape/ape_error.h"
#include "ape/ape_trad.h"

// ISO includes
#include <map>
#include <vector>
#include <sstream>    // create a stream for a changing variable name
#include <cstdlib>    // used to convert string to long
#include <cmath>      // pow function
#include "hdcal.h"    // CALDB query utilities
#include "headas.h"   // expand_item_list

/** \addtogroup tool_sxiflagpix
 *  @{
 */

/// \brief number of bits in STATUS column
const int LENSTATUS = 48;

/// \brief number of pixels in PHAS column
const int LENPHAS = 9;
  
typedef std::vector<std::vector<double> > vec2dDbl;
typedef std::vector<std::vector<long> > vec2dLng;
typedef std::vector<std::vector<long long> > vec2dLngLng;
typedef std::vector<std::vector<bool> > vec2dBool;

/// \brief Get parameter values
/// \param[out] param           Structure containing data from parameter file
void getPar(sxiflagpixlib::Params & param);

/// \brief Copy contents of infile to outfile; resolve default values of columns
/// \param[out] param           Structure containing data from parameter file
/// \param[out] fpout           ahfits object of output file (clone of input)
/// \param[out] fpbad           ahfits object of flagged bad pixel output file  
/// \param[out] fpmap           optional exposure map output file
/// \param[in] statusMask       CCD pixel bit statuses
/// \param[out] BitBadStatus    Bad status bits hexidecimal
/// \param[out] fracmap         Fraction map from CR echo detection (if done)
void initialize(sxiflagpixlib::Params & param, ahfits::FilePtr & fpout, 
                ahfits::FilePtr & fpbad, ahfits::FilePtr & fpmap, 
                std::vector<vec2dLngLng> & statusMask, long long & BitBadStatus,
                std::vector<vec2dDbl> & fracmap);

/// \brief Fill output event file and output bad pixel list
/// \param[out] param           Structure containing data from parameter file
/// \param[out] fpout           ahfits object of output file (clone of input)
/// \param[out] fpbad           ahfits object of flagged bad pixel output file  
/// \param[out] fpmap           optional exposure map output file
/// \param[in] statusMask       CCD pixel bit statuses
/// \param[in] BitBadStatus     Bad status bits hexidecimal
/// \param[in] fracmap         Fraction map from CR echo detection (if done)
void doWork(sxiflagpixlib::Params param, ahfits::FilePtr & fpout, 
            ahfits::FilePtr & fpbad, ahfits::FilePtr & fpmap, 
            std::vector<vec2dLngLng> & statusMask, long long & BitBadStatus,
            std::vector<vec2dDbl> & fracmap);

/// \brief close open FITS files
/// \param[in] fpout            ahfits object of output file (clone of input)
/// \param[in] fpbad            ahfits object of flagged bad pixel output file  
/// \param[in] fpmap            ahfits object of flagged bad pixel map output file  
void finalize(ahfits::FilePtr fpout, ahfits::FilePtr fpbad, ahfits::FilePtr fpmap);

/// \brief set bits as char[48] arrays
/// \param[out] l_status       array of length 48 storing bit information
/// \param[out] statusCounter  array of length 48 storing bit counting information
/// \param[in] maskStatus      Pixel status
void setBitChar(char * l_status, int * statusCounter, long long maskStatus);

/// \brief Subroutine to read area discrimination keywords and convert to ACT coordinates
/// \param[in] fpout            ahfits object of output file (clone of input)
/// \param[in] aron             Active readnodes from keyword ACTVNODE
/// \param[out] aron            Area discrimination array: Region on/off
/// \param[out] arin            Area discrimination array: Inclusion region
/// \param[out] arou            Area discrimination array: Exclusion regions
void transformAreaDiscrimToAct(ahfits::FilePtr fpout, int * actvnode, vec2dBool & aron,
                               vec2dLng & arin, vec2dLng & arou);

/// \brief Subroutine to convert RAWX coordinates to ACTX coordinates
/// \param[in] segment          Segment ID number (0: AB, 1 CD)
/// \param[in] readnode         Readnode ID number (0: A or C, 1: B or D)
/// \param[in,out] xcoord       Coordinate to convert from rawx to actx
void convertRawxToActx(int segment, int readnode, int & xcoord); 

/// \brief copy mask info to status array
/// \param[out] maskfile         Input CALDB mask file for calibration source and boundary regions
/// \param[in] npixnbr     Distance from a hot pixel, bad pixel, bad column, or boundary to flag a neighbor pixel boundary
/// \param[in] telescop    TELESCOP keyword event file
/// \param[in] instrume    INSTRUME keyword event file 
/// \param[in,out] statusMask   CCD pixel bit statuses
void copyMaskArray(std::string & maskfile, int nboundnbr, const std::string telescop, const std::string instrume, std::vector<vec2dLngLng>  & statusMask);

/// \brief Set all status bits within the area discrimination regions
/// \param[in] aron             Active readnodes from keyword ACTVNODE
/// \param[out] aron            Area discrimination array: Region on/off
/// \param[out] arin            Area discrimination array: Inclusion region
/// \param[out] arou            Area discrimination array: Exclusion regions
/// \param[in,out] statusMask   CCD pixel bit statuses
void maskAreaDiscrimination(int * actvnode, vec2dBool & aron, vec2dLng & arin, vec2dLng & arou, 
                            std::vector<vec2dLngLng> & statusMask);

/// \brief copy window boundary info to status array
/// \param[in] winStart         Starting pixel of window regions
/// \param[in] winSize          Size of window regions
/// \param[in] npixnbr     Distance from a hot pixel, bad pixel, bad column, or boundary to flag a neighbor pixel boundary 
/// \param[in,out] statusMask   CCD pixel bit statuses
void maskWindowBoundary(int winStart, int winSize, int npixnbr, std::vector<vec2dLngLng> & statusMask);

/// \brief copy charge injection pattern info to status array
/// \param[in] ciperiod         Charge Injection: Period - Number of ACTY rows between CI rows
/// \param[in] cioffset         Charge Injection: Offset from first row in ACTY coordinates
/// \param[in] cifirst          Charge Injection: First row of CI
/// \param[in] citrailnbr  Distance trailed from SCI row to flag a neighbor pixel (pixels)
/// \param[in] ciprenbr    Distance preceding from SCI row to flag a neighbor pixel (pixels)
/// \param[in,out] statusMask   CCD pixel bit statuses
void maskChargeInjection(int ciperiod, int cioffset, int cifirst, int citrailnbr,
                         int ciprenbr, std::vector<vec2dLngLng> & statusMask);

/// \brief set status bits for pixels affected by cosmic ray echo
/// \param[in] fpout            Pointer to event file
/// \param[in] echonbr          Pixel distance defining a CR echo neighbor
/// \param[in] echomin          Minimum events for CR echo fraction calculation
/// \param[in] echospth         Split threshold for CR echo fraction calculation
/// \param[in] echofrac         Minimum fraction of hits defining a cosmic ray echo
/// \param[in] gtifile          GTI to select events used in constructing CR echo map
/// \param[in] echomap          Flag to output the fraction map as a bad pixel extension
/// \param[in,out] statusMask   CCD pixel bit statuses
/// \param[out] fracmap         Fraction map from CR echo algorithm
void maskCREcho(ahfits::FilePtr fpout, int echonbr, int echomin, int echospth, 
                double echofrac, std::string gtifile, bool echomap,
                std::vector<vec2dLngLng> & statusMask, std::vector<vec2dDbl> & fracmap);

/// \brief set event status bit for bad, hot and flickering pixels
/// \param[in] pixels           Data from bad pixels, hot pixels or flickering pixels files
/// \param[in] statusBit        Bit to set in mask (Bad: 0; Hot: 1; Flickering: 2)
/// \param[in] statusBitNeighor Neighboring bit to set in mask (Bad: 8; Hot: 9; Flickering: 10)
/// \param[in] npixnbr     Distance from a hot pixel, bad pixel, bad column, or boundary to flag a neighbor pixel boundary 
/// \param[in,out] statusMask   CCD pixel bit statuses
void maskPixels(sxiflagpixlib::PixArray pixels, long long statusBit, long long statusBitNeighbor, 
                int npixnbr, std::vector<vec2dLngLng> & statusMask);

/// \brief status set to 14 if any of the 3x3 surrounding pixels has the b5-b9 or b15-b16 condition 
/// \param[in,out] statusMask   CCD pixel bit statuses
void setBit3x3surround(std::vector<vec2dLngLng> & statusMask);

/// \brief set bitnum in a mask for mask elements within npixnbr pixels of ACTXI, ACTYI
/// \param[in] ccdid            current CCD from event file
/// \param[in] actx             current x coordinate from event file
/// \param[in] acty             current y coordinate from event file
/// \param[in] npixnbr     Distance from a hot pixel, bad pixel, bad column, or boundary to flag a neighbor pixel boundary 
/// \param[in] bitnum           bit to be set for neighbors
/// \param[in,out] statusMask   CCD pixel bit statuses
void setBitNeighbors(int ccdid, int actx, int acty, int npixnbr, long long bitnum, 
                     std::vector<vec2dLngLng> & statusMask);

/// \brief Set PHAS array to tnull if testMask is true for surrounding coordinates.
/// \param[in] ccdid           ccd id number
/// \param[in] readnode        readout node information
/// \param[in] actx            current x coordinate from event file
/// \param[in] acty            current y coordinate from event file
/// \param[in] testMask        status check of bits 0-3
/// \param[in] statusMask      CCD pixel bit statuses
/// \param[in] tbad            tbad variable from PHAS column
/// \param[in,out] l_phas      PHAS from event file
void setBad3x3(char ccdid, char segment, char readnode, int actx, int acty, long long testMask, 
               std::vector<vec2dLngLng> & statusMask, char * phasMask);


/// \brief parse a string of the form (1,2,3,4,5) into an array
/// \param[in] str             string to be parsed, delimited by commas
/// \param[in] length          length of output string
/// \param[out] outarr         output array containing delimited contents of str
void parseStringInt(std::string str, int length, int * outarr);

/// \brief parse a string list of numbers of the form 1:4,7,9:11 -> (1,2,3,4,7,9,10,11)
/// \param[in] instr           input string to be parsed
/// \param[out] outlist        output vector containing parsed numbers
void parseNumberList(const std::string & instr, std::vector<int> & outlist);

/// \brief Column masking
/// \param[in] statusBits           current status bit
/// \param[in] statusBitPre         previous status bit
/// \param[in] statusBitNeighbor    status of neighbhoring pixel
/// \param[in] statusBitPreNeighbor previous bit of neighbhoring pixel
/// \param[in] npixnbr              Pixel distance defining a neighbor
/// \param[out] statusMask          CCD pixel bit statuses
void maskColumns(sxiflagpixlib::PixArray pixels, long long statusBit, long long statusBitPre, 
                 long long statusBitNeighbor, long long statusBitPreNeighbor,
                 int & npixnbr, std::vector<vec2dLngLng> & statusMask);


/// \brief sets bits for ccd boundary and segment boundary neighbhors
/// \param[in] ccd_id               current ccd
/// \param[in] ix                   pixel coordinate x (ACT)
/// \param[in] iy                   pixel coordinate y (ACT)
/// \param[in] nboundnbr            Pixel distance defining neighbhor from CCD/window/segment boundary"  
/// \param[in] bit_ccd              ccd boundary status flag
/// \param[in] bit_seg              segment boundary status flag
/// \param[out] statusMask          CCD pixel bit statuses
void setBitBoundNeighbors(int ccd_id, int ix, int iy, int nboundnbr, int bit_ccd, int bit_seg, std::vector<vec2dLngLng> & statusMask);


// ****************************************************************************

/// \brief sxiflagpix tool
int main(int argc, char** argv) {

  sxiflagpixlib::Params param;
  
  ahfits::FilePtr fpout = 0;            // FITS file pointer to output file
  ahfits::FilePtr fpbad = 0;            // FITS file pointer to bad pixels output file
  ahfits::FilePtr fpmap = 0;            // FITS file pointer to status map image output file 

  std::vector<vec2dLngLng> statusMask(4);    // 2D arrays of statuses, one for each CCD
  std::vector<vec2dDbl> fracmap(4);          // Fraction map from CR echo detection

  long long BitBadStatus = 0;                //Bad Status bits hexadecimal

  int status = ahapp::startUp(argc, argv, TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(param);
      ahapp::writeParametersToLog(); 
      initialize(param, fpout, fpbad, fpmap, statusMask, BitBadStatus, fracmap);
      doWork(param, fpout, fpbad, fpmap, statusMask, BitBadStatus, fracmap);
      finalize(fpout, fpbad, fpmap);
      ahapp::shutDown();
    } else {
      try {
        getPar(param);
        initialize(param, fpout, fpbad, fpmap, statusMask, BitBadStatus, fracmap);
        doWork(param, fpout, fpbad, fpmap, statusMask, BitBadStatus, fracmap);
      } catch (const std::exception & x) {
          ahapp::writeParametersToLog(); 
          status = 1;
          AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout, fpbad, fpmap);
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;

} // end main

// ****************************************************************************

void getPar(sxiflagpixlib::Params & param) {

  param.m_infile = ahapp::getParString("infile");
  param.m_outfile = ahapp::getParString("outfile");
  param.m_outbadpix = ahapp::getParString("outbadpix");
  param.m_outbadimg = ahapp::getParString("outbadimg");

  param.m_badpixfile = ahapp::getParString("badpixfile");          // CALDB  
  param.m_hotpixfile = ahapp::getParString("hotpixfile"); 
  param.m_flickpixfile = ahapp::getParString("flickpixfile"); 
  
  param.m_maskfile = ahapp::getParString("maskfile");              // CALDB

  param.m_npixnbr = ahapp::getParInt("npixnbr");
  param.m_nboundnbr = ahapp::getParInt("nboundnbr");
  param.m_citrailnbr = ahapp::getParInt("citrailnbr");
  param.m_ciprenbr = ahapp::getParInt("ciprenbr");
  param.m_echonbr = ahapp::getParInt("echonbr");
  param.m_echomin = ahapp::getParInt("echomin");
  param.m_echospth = ahapp::getParInt("echospth");
  param.m_echofrac = ahapp::getParDouble("echofrac");
  param.m_echoflag = ahapp::getParBool("echoflag");
  param.m_gtifile = ahapp::getParString("gtifile");
  param.m_echomap = ahapp::getParBool("echomap");
  param.m_bad_status_str = ahapp::getParString("bad_status");

  param.m_copyphas = ahapp::getParBool("copyphas");

  param.m_resetflags = ahapp::getParBool("resetflags");
 
  //convert bad status list string to vector m_bad_status 
  parseNumberList(param.m_bad_status_str, param.m_bad_status);


} // end getPar

// ****************************************************************************

void initialize(sxiflagpixlib::Params & param, ahfits::FilePtr & fpout, 
                ahfits::FilePtr & fpbad, ahfits::FilePtr & fpmap, 
                std::vector<vec2dLngLng> & statusMask, long long & BitBadStatus,
                std::vector<vec2dDbl> & fracmap) {

  std::string extname = "EVENTS";    // Extension name for input/output file
  std::string telescop;              // String to store TELESCOP keyword value
  std::string inst;                  // String to store INSTRUME keyword value
  std::string detnam;                // String to store DETNAM keyword value
  std::string actvnode_str = "";


  double tstart_in = 0.;             // Observation start time for input event file
  double tstop_in = 0.;              // Observation end time for input event file
  long cistatus = 0;                 // Charge Injection: Use charge injection (on/off)
  long ciperiod = 0;                 // Charge Injection: Period - Number of ACTY rows between CI rows 
  long cioffset = 0;                 // Charge Injection: Offset from first row in ACTY coordinates
  long cifirst = 0;                  // Charge Injection: First row of CI
  long winStart = 0;                 // Starting pixel of window regions
  long winSize = 0;                  // Size of window regions
  int actvnode[8] = { 0,0,0,0,0,0,0,0 };
  
  vec2dBool aron;                    // Area discrimination array: Region on/off
  vec2dLng arin;                     // Area discrimination array: Inclusion region
  vec2dLng arou;                     // Area discrimination array: Exclusion regions

  sxiflagpixlib::PixArray badPixDat;      // data from SXI bad pixel CALDB file
  sxiflagpixlib::PixArray hotPixDat;      // data from SXI hot pixel file
  sxiflagpixlib::PixArray flickerPixDat;  // data from SXI flicker pixel file

  int performArray[8] = { 0,0,0,0,0,0,0,0 };   // performArray 1=flag to be done 0= not to be done
                                               // performArray[0] = badpixel
                                               // performArray[1] = hotpixel
                                               // performArray[2] = flickpixel
                                               // performArray[3] = mappixel
                                               // performArray[4] = evtthre
                                               // performArray[5] = area
                                               // performArray[6] = charge
                                               // performArray[7] = window


  // copy contents of infile to outfile and return opened output file (fpout);
  // if editing input file in-place (allowed with last argument = true),
  // fpout will point to the opened input file.
  ahfits::clone(param.m_infile, param.m_outfile, &fpout, true);
  if (ahfits::isPrimary(fpout)) ahfits::move(fpout, extname);
  ahmission::checkEmptyTable(fpout,param.m_infile);

  // Check if FITS file pointer is valid
  if(!ahfits::readOK(fpout)) {
    AH_THROW_RUNTIME("No events in input event file");
  }
  
  // Check for correct instrument
  inst = ahfits::getKeyValStr(fpout, "INSTRUME");
  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in input file should be SXI, not " + inst);
  }
 
  //store detnam and telescop keyword
  detnam = ahfits::getKeyValStr(fpout, "DETNAM");
  telescop = ahfits::getKeyValStr(fpout, "TELESCOP");

  //convert par.m_bad_status to hexidecimal number
  for (unsigned int ii = 0; ii < param.m_bad_status.size(); ++ii) {
    BitBadStatus += pow(2, param.m_bad_status[ii]);
  }

  // Initialize statusMask to 0 
  // +++ 2014-12-10 AS: Get SXI_CCD_SIZE from event file?
  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    statusMask[i_ccd].resize(SXI_CCD_SIZE);
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      statusMask[i_ccd][ix].resize(SXI_CCD_SIZE);
      for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) {
        statusMask[i_ccd][ix][jy] = 0;
      }
    }
  }

  // Write comments of bit values
  ahfits::writeKeyComment(fpout,
"STATUS column is defined as the following:\n"
"=====================================================================\n"
"flag  description                                                    \n"
"=====================================================================\n"
"1     All bad events set by 'bad_status' parameter                   \n"
"2     Inside the calibration source region                           \n"
"------(Out of area)--------------------------------------------------\n"
"3     Out of CCD                                                     \n"
"4     Out of window                                                  \n"
"5     Out of area discrimination                                     \n"
"------(Pixels)-------------------------------------------------------\n"
"6     CI row                                                         \n"
"7     Bad pixel from CALDB                                           \n"
"8     Bad column from CALDB                                          \n"
"9     Hot pixel from pre-pipeline                                    \n"
"10    Flickering pixel                                               \n"
"------(Boundaries)---------------------------------------------------\n"
"11    CCD boundary                                                   \n"
"12    Window boundary                                                \n"
"13    Segment boundary                                               \n"
"14    Area discrimination boundary                                   \n"
"15    At least one 3x3 surrounding pixel has a bad status            \n"
"------(Neighbors)----------------------------------------------------\n"
"16    CI trailing row                                                \n"
"17    CI preceding row                                               \n"
"18    Preceding/following of bad column                              \n"
"19    Neighbors of bad/hot pixel and bad column                      \n"
"20    Neighbors of flickering pixel                                  \n"
"21    Neighbors of preceding/following of bad column                 \n"
"22    Neighbors of CCD/window boundary                               \n"
"23    Neighbors of segment boundary                                  \n"
"------(Others)-------------------------------------------------------\n"
"24    (sxiphas) 3x3 info is present but 5x5 is absent                \n"
"25    (sxiphas) 3x3 is absent                                        \n"
"26    (sxipi - general) PHAS[0] < event threshold                    \n"
"27    (sxipi - vtevnodd) Video temperature is out of range           \n"
"28    (sxipi - vtevnodd) Lack of video temp HK at time close to event\n"
"29    (sxipi - chtrail/CTI) Correction value is negative             \n"
"30    (sxipi - general) Null value by correction process             \n"
"------(Diagnostics)--------------------------------------------------\n"
"31    1st trailing row of the CI rows                                \n"
"32    1st preceding row of the CI rows                               \n"
"33    2nd trailing row of the CI rows                                \n"
"34    2nd preceding row of the CI rows                               \n"
"35    3rd trailing row of the CI rows                                \n"
"36    3rd preceding row of the CI rows                               \n"
"------(Cosmic Rays)--------------------------------------------------\n"
"37    Cosmic ray echo pixel                                          \n"
"---------------------------------------------------------------------\n"
"38-48  Reserved                                                      \n"
"=====================================================================\n"
  );


  // Read Keywords TSTART, TSTOP, CISTATUS, CIPERIOD, CIOFFSET, CIFIRST, WIN_ST, WIN_SIZE
  tstart_in = ahfits::getKeyValDbl(fpout, "TSTART");
  tstop_in = ahfits::getKeyValDbl(fpout, "TSTOP");
  if (!ahfits::keywordExists(fpout, "WIN_ST") ||
      !ahfits::keywordExists(fpout, "WIN_SIZE")) {
    AH_INFO(ahlog::HIGH) << "Window flagging will not be perfomed" << std::endl;
  } else { 
    winStart = ahfits::getKeyValLLong(fpout, "WIN_ST");     // Get window start coordinate
    winSize = ahfits::getKeyValLLong(fpout, "WIN_SIZE");    // Get window size
    performArray[7] = 1;
  } 
  if (!ahfits::keywordExists(fpout, "CISTATUS") || 
      !ahfits::keywordExists(fpout, "CIPERIOD") ||
      !ahfits::keywordExists(fpout, "CIOFFSET") ||
      !ahfits::keywordExists(fpout, "CIFIRST")  ||
      0 == getKeyValLLong(fpout, "CISTATUS")) { 
    AH_INFO(ahlog::HIGH) << "Charge injection flagging will not be performed"<< std::endl;
  } else {  
    cistatus = getKeyValLLong(fpout, "CISTATUS");           // Status value of charge injection 
    ciperiod = getKeyValLLong(fpout, "CIPERIOD");           // Value of charge injection period from FITS file
    cioffset = getKeyValLLong(fpout, "CIOFFSET");           // Value of charge injection offset from FITS file
    cifirst = getKeyValLLong(fpout, "CIFIRST");             // Value of first charge injection from FITS file
    performArray[6] = 1;
  }

  if (!ahfits::keywordExists(fpout, "EVENTTHR")) {
    AH_INFO(ahlog::HIGH) << "Event threshold flagging will not be performed." << std::endl;
  } else {
    performArray[4] = 1;
  }

  // Allocate arrays for area discrimination keywords:
  aron.resize(8);
  arin.resize(8);
  arou.resize(8);
  for(int ii = 0; ii < 8; ++ii) aron[ii].resize(5);
  for(int ii = 0; ii < 8; ++ii) arin[ii].resize(4);
  for(int ii = 0; ii < 8; ++ii) arou[ii].resize(16);

  if(ahgen::strtoupper(param.m_maskfile) == "NONE" && ahgen::strtoupper(param.m_outbadimg) != "NONE") {
    AH_INFO(ahlog::HIGH) << " *** outbadimg parameter not set to NONE but no input maskfile. Skipping image creation." << std::endl;
    param.m_outbadimg = "NONE";
  }

  if(ahgen::strtoupper(param.m_maskfile) != "NONE") {
    
    param.m_maskfile = ahmission::caldb::resolve(param.m_maskfile, "mask", inst, "-", "MASK", "-", "-", telescop);
    ape_trad_set_string("maskfile",param.m_maskfile.c_str());   // to record actual file path in history
    
    // transfer maskArray info to statusMask
    // Set bits 1, 10, 12, 21, 22
    copyMaskArray(param.m_maskfile, param.m_nboundnbr, telescop, inst, statusMask);
    performArray[3] = 1;
  }

  if (!ahfits::keywordExists(fpout, "ACTVNODE")) { 
    AH_INFO(ahlog::HIGH) << "Area discrimination flagging will not be performed." << std::endl;
  } else {
    // Read keyword ACTVNODE into actvnode
    // Default: ACTVNODE = (0,1,0,1,0,1,0,1)
    actvnode_str = ahfits::getKeyValStr(fpout,"ACTVNODE");
    parseStringInt(actvnode_str, 8, actvnode);
    performArray[5] = 1;
    // Read keywords CnSmARON, CnSmARIN, CnSmAROU using function
    // Convert keyword coordinates from RAW to ACT
    transformAreaDiscrimToAct(fpout, actvnode, aron, arin, arou);

    // transfer area discrimination info to statusMask
    // Set bits 4, 13
    maskAreaDiscrimination(actvnode, aron, arin, arou, statusMask);
  } // end if keyword exists ACTVNODE

  if (performArray[7] == 1) {
    // transfer window boundary info to statusMask
    // Set bits 3, 11, 21
    maskWindowBoundary(winStart, winSize, param.m_nboundnbr, statusMask);
  }

  if (performArray[6] == 1) {
    // transfer charge injection row info to statusMask
    // Set bits 5, 15, 16
    if(cistatus) {
      maskChargeInjection(ciperiod, cioffset, cifirst, param.m_citrailnbr, param.m_ciprenbr, statusMask);
    }
  }
  // hot pixel and flickering pixel lists are observation-specific and can be used
  // as-is.

  // Assign status to mask for bad, hot and flickering pixels
  if(ahgen::strtoupper(param.m_badpixfile) != "NONE") {
    performArray[0] = 1;
    
    //set badpix file to file returned by CALDB query 
    param.m_badpixfile = ahmission::caldb::resolve(param.m_badpixfile, "bad pixel", inst, "-", "BADPIX", "-", "-", telescop);
    ape_trad_set_string("badpixfile",param.m_badpixfile.c_str());   // to record actual file path in history
      
    // load bad pixels within observation time
    sxiflagpixlib::loadBadPix(param.m_badpixfile, tstart_in, badPixDat);
    AH_DEBUG << "Number of bad pixels: " << badPixDat.m_numpix << std::endl;

    // Preset bad pixels in mask
    // Set bits 6, 18
    maskPixels(badPixDat, 0x00000040LL, 0x00040000LL, param.m_npixnbr, statusMask);

    //mask columns with bits 7, 17, 18, 20
    maskColumns(badPixDat, 0x00000080LL, 0x00020000LL, 0x00040000LL, 
                0x00100000LL, param.m_npixnbr, statusMask);

  } else {
    AH_INFO(ahlog::HIGH) << "No badpixel status set" << std::endl;
  }// end if badpixfile != "NONE"

  if(ahgen::strtoupper(param.m_hotpixfile) != "NONE") {
    performArray[1] = 1;
    // Load hot  pixel list from pre-pipeline
    sxiflagpixlib::loadHotPix(param.m_hotpixfile, tstart_in, tstop_in, hotPixDat);
    AH_DEBUG << "Number of hot pixels: " << hotPixDat.m_numpix << std::endl;

    // Preset hot pixels in statusMask
    // Set bits 8, 18
    maskPixels(hotPixDat, 0x00000100LL, 0x00040000LL, param.m_npixnbr, statusMask);
  } else {
    AH_INFO(ahlog::HIGH) << "No hotpixel status set." << std::endl;
  }

  if(ahgen::strtoupper(param.m_flickpixfile) != "NONE") {
    // Load hot pixel list from pre-pipeline
    sxiflagpixlib::loadPix(param.m_flickpixfile, flickerPixDat, tstart_in, tstop_in);
    AH_DEBUG << "Number of flickering pixels: " << flickerPixDat.m_numpix << std::endl;
    
    // Preset flickering pixels in statusMask
    // Set bits 9, 19
    maskPixels(flickerPixDat, 0x00000200LL, 0x00080000LL, param.m_npixnbr, statusMask);
  } else {
    AH_INFO(ahlog::HIGH) << "No flickpixel status set." << std::endl;
  }

  // set bit 14 if bits 5-9 or 15-16 are set
  setBit3x3surround(statusMask);

  // Flag the cosmic ray echo pixels
  if (param.m_echoflag) {
    maskCREcho(fpout, param.m_echonbr, param.m_echomin, param.m_echospth, 
      param.m_echofrac, param.m_gtifile, param.m_echomap, statusMask, fracmap);
  }

  // Data structures are no longer needed. Delete.
  //sxiflagpixlib::clearPix(badPixDat);
  sxiflagpixlib::clearPix(hotPixDat);
  sxiflagpixlib::clearPix(flickerPixDat);

  // Create bad pixel list output file
  if(ahgen::strtoupper(param.m_outbadpix) != "NONE") {
    ahfits::create(param.m_outbadpix,"",&fpbad);
    ahfits::addHDU(fpout,"EVENTS",fpbad,"PIXELS");
  }

  // Set up map output file, if desired
  if(ahgen::strtoupper(param.m_outbadimg) != "NONE") {
    ahfits::create(param.m_outbadimg,"",&fpmap);
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
 
} // end initialize

// ****************************************************************************

void doWork(sxiflagpixlib::Params param, ahfits::FilePtr & fpout, ahfits::FilePtr & fpbad, 
            ahfits::FilePtr & fpmap, std::vector<vec2dLngLng> & statusMask, long long & BitBadStatus,
            std::vector<vec2dDbl> & fracmap) {

  bool hastnull = false;        // Boolean check for TNULL
  bool badPix = false;

  // Initialize variables for routers
  char l_ccdid = 0;             // Current CCD ID value from event loop
  char l_segment = 0;           // Current Segment ID value from event loop
  char l_readnode = 0;          // Current Readout Node value from event loop
  int l_actx = 0;               // Current detx value from event loop
  int l_acty = 0;               // Current dety value from event loop
  
  // Other local variables for copying to bad pixel list
  double l_time, l_s_time5x5, l_s_time;
  int l_adu_cnt, l_adu_cnt5x5;
  char l_category, l_category5x5, l_adcave;
  long long l_l32ti, l_l32ti5x5, l_seqstarttime;
  int l_eventnumber, l_eventnumber5x5, l_rawx, l_rawy, l_p_outer_most; 
  int l_sum_outer_most, l_detx, l_dety, l_focx, l_focy, l_x, l_y;
  int l_pha, l_pi, l_grade; 

  int l_phas_inner3x3[LENPHAS] = { 0 };      // PHAS inner 3x3 array 
  int l_phas_outer5x5[16] = { 0 };           // PHAS outer 5x5 array 
  int l_phas[LENPHAS] = { 0 };               // PHAS array from event loop
  int l_phasall[25] = { 0 };                 // PHAS array from event loop
  char l_status[LENSTATUS] = { 0 };          // 48 Bit status array
  char l_procStatus[LENSTATUS] = { 0 };      // column: PROC_STATUS (1st bit=1 => BAD)
  char l_phasMask[LENPHAS] = { 0 };          // column: PHAS_MASK 

  // Null values of PHAS, CCD_ID, ACTX, ACTY, RAWX, RAWY, SEGMENT
  long long tbad = 0;
  long long ccdbad = 0;
  long long actxbad = 0;
  long long actybad = 0;
  long long rawxbad = 0;
  long long rawybad = 0;
  long long segbad = 0;

  // Minimum/Maximum of CCD_ID, ACTX, ACTY, RAWX, RAWY, SEGMENT
  long long ccdmin = 0;
  long long ccdmax = 0;
  long long actxmin = 0;
  long long actxmax = 0;
  long long actymin = 0;
  long long actymax = 0;
  long long rawxmin = 0;
  long long rawxmax = 0;
  long long rawymin = 0;
  long long rawymax = 0;
  long long segmin = 0;
  long long segmax = 0;

  std::string l_evtthres;       // Current event threshold from event loop
  
  int evtThresInt[8] = { 0,0,0,0,0,0,0,0 };  // Event threshold array, one for each segment
  
  ahfits::IndexType numStatus = LENSTATUS;   // Length of status bits
  ahfits::IndexType numProcStatus;           // number of PROC_STATUS bits found

  long long statusbit = 0;                   //Status bit unique to each event

  //Event Counters
  int statusCounter[LENSTATUS] = { 0 };   //number of pixels with each status flag

  // Complete initialization of local arrays
  for (int i=0; i<LENPHAS; ++i) {
    l_phas[i] = 0;
    l_phas_inner3x3[i] = 0;
    l_phasMask[i] = 0;
  }
  for (int i=0; i<LENSTATUS; ++i) {
    l_status[i] = 0;
    l_procStatus[i] = 0;
    statusCounter[i] = 0;
  }

  // Capture TNULL value for PHAS
  hastnull = ahfits::columnNull(fpout, "PHAS", tbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("PHAS column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "CCD_ID", ccdbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("CCD_ID column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "ACTX", actxbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("ACTX column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "ACTY", actybad);
  if(!hastnull) {
    AH_THROW_RUNTIME("ACTY column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "RAWX", rawxbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("RAWX column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "RAWY", rawybad);
  if(!hastnull) {
    AH_THROW_RUNTIME("RAWY column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fpout, "SEGMENT", segbad);
  if(!hastnull) {
    AH_THROW_RUNTIME("SEGMENT column lacks TNULL");
  }

  // set up event threshold array
  l_evtthres = ahfits::getKeyValStr(fpout,"EVENTTHR");
  parseStringInt(l_evtthres, 8, evtThresInt);

  // Get min/max values of CCD_ID, ACTX, ACTY and SEGMENT
  if(!ahfits::columnRange(fpout,"CCD_ID",ccdmin,ccdmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column CCD_ID");
  }
  if(!ahfits::columnRange(fpout,"SEGMENT",segmin,segmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column SEGMENT");
  }
  if(!ahfits::columnRange(fpout,"ACTX",actxmin,actxmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column ACTX");
  }
  if(!ahfits::columnRange(fpout,"ACTY",actymin,actymax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column ACTY");
  }
  if(!ahfits::columnRange(fpout,"RAWX",rawxmin,rawxmax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column RAWX");
  }
  if(!ahfits::columnRange(fpout,"RAWY",rawymin,rawymax)) {
    AH_THROW_RUNTIME("Failed to get TLMIN or TLMAX in column RAWY");
  }

  AH_DEBUG << "CCD_ID MIN: " << ccdmin << " MAX: " << ccdmax << " NULL: " << ccdbad  << std::endl;
  AH_DEBUG << "ACTX MIN: " << actxmin << " MAX: " << actxmax << " NULL: " << actxbad  <<std::endl;
  AH_DEBUG << "ACTY MIN: " << actymin << " MAX: " << actymax << " NULL: " << actybad  <<std::endl;
  AH_DEBUG << "RAWX MIN: " << rawxmin << " MAX: " << rawxmax << " NULL: " << rawxbad  <<std::endl;
  AH_DEBUG << "RAWY MIN: " << rawymin << " MAX: " << rawymax << " NULL: " << rawybad  <<std::endl;
  AH_DEBUG << "SEGMENT MIN: " << segmin << " MAX: " << segmax << " NULL: " << segbad  <<std::endl;

  if(ahgen::strtoupper(param.m_outbadpix) != "NONE") badPix = true;

  // set up local variables and router for reading/writing file
  ahfits::Router router(fpout);     // Router for output event file

  // connections to event output file
  router.connectScalar(ahfits::e_READONLY, "TIME", l_time);
  router.connectScalar(ahfits::e_READONLY, "S_TIME5X5", l_s_time5x5);
  router.connectScalar(ahfits::e_READONLY, "S_TIME", l_s_time);
  router.connectScalar(ahfits::e_READONLY, "ADU_CNT", l_adu_cnt);
  router.connectScalar(ahfits::e_READONLY, "ADU_CNT5X5", l_adu_cnt5x5);
  router.connectScalar(ahfits::e_READONLY, "CATEGORY", l_category);
  router.connectScalar(ahfits::e_READONLY, "CATEGORY5X5", l_category5x5);
  router.connectScalar(ahfits::e_READONLY, "L32TI", l_l32ti);
  router.connectScalar(ahfits::e_READONLY, "L32TI5X5", l_l32ti5x5);
  router.connectScalar(ahfits::e_READONLY, "SEQSTARTTIME", l_seqstarttime);
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);
  router.connectScalar(ahfits::e_READONLY, "SEGMENT", l_segment);
  router.connectScalar(ahfits::e_READONLY, "EVENTNUMBER", l_eventnumber);
  router.connectScalar(ahfits::e_READONLY, "EVENTNUMBER5X5", l_eventnumber5x5);
  router.connectScalar(ahfits::e_READONLY, "READNODE", l_readnode);
  router.connectScalar(ahfits::e_READONLY, "ADCAVE", l_adcave);
  router.connectScalar(ahfits::e_READONLY, "RAWX", l_rawx);
  router.connectScalar(ahfits::e_READONLY, "RAWY", l_rawy);
  router.connectFixedLengthArray(ahfits::e_READONLY, "PHAS_INNER3X3", l_phas_inner3x3);
  router.connectScalar(ahfits::e_READONLY, "P_OUTER_MOST", l_p_outer_most);
  router.connectScalar(ahfits::e_READONLY, "SUM_OUTER_MOST", l_sum_outer_most);
  router.connectFixedLengthArray(ahfits::e_READONLY, "PHAS_OUTER5X5", l_phas_outer5x5);
  router.connectScalar(ahfits::e_READONLY, "ACTX", l_actx);
  router.connectScalar(ahfits::e_READONLY, "ACTY", l_acty);
  router.connectScalar(ahfits::e_READONLY, "DETX", l_detx);
  router.connectScalar(ahfits::e_READONLY, "DETY", l_dety);
  router.connectScalar(ahfits::e_READONLY, "FOCX", l_focx);
  router.connectScalar(ahfits::e_READONLY, "FOCY", l_focy);
  router.connectScalar(ahfits::e_READONLY, "X", l_x);
  router.connectScalar(ahfits::e_READONLY, "Y", l_y);
  router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS", l_phas);
  router.connectFixedLengthArray(ahfits::e_READONLY, "PHASALL", l_phasall);
  router.connectScalar(ahfits::e_READONLY, "PHA", l_pha);
  router.connectScalar(ahfits::e_READONLY, "PI", l_pi);
  router.connectScalar(ahfits::e_READONLY, "GRADE", l_grade);
  router.connectBit(ahfits::e_READWRITE, "PROC_STATUS", l_procStatus, numProcStatus);
  router.connectBit(ahfits::e_READWRITE, "STATUS", l_status, numStatus);
  router.connectFixedLengthArray(ahfits::e_READWRITE, "PHAS_MASK", l_phasMask); 

  ahfits::Router * routerbp = 0;     // Router for output event file
  if(badPix) {
    routerbp = new ahfits::Router(fpbad);
    // Connections to output bad pixel list
    routerbp->connectScalar(ahfits::e_WRITEONLY, "TIME", l_time);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "S_TIME5X5", l_s_time5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "S_TIME", l_s_time);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "ADU_CNT", l_adu_cnt);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "ADU_CNT5X5", l_adu_cnt5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "CATEGORY", l_category);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "CATEGORY5X5", l_category5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "L32TI", l_l32ti);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "L32TI5X5", l_l32ti5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "SEQSTARTTIME", l_seqstarttime);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "CCD_ID", l_ccdid);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "SEGMENT", l_segment);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "EVENTNUMBER", l_eventnumber);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "EVENTNUMBER5X5", l_eventnumber5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "READNODE", l_readnode);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "ADCAVE", l_adcave);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "RAWX", l_rawx);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "RAWY", l_rawy);
    routerbp->connectFixedLengthArray(ahfits::e_WRITEONLY, "PHAS_INNER3X3", l_phas_inner3x3);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "P_OUTER_MOST", l_p_outer_most);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "SUM_OUTER_MOST", l_sum_outer_most);
    routerbp->connectFixedLengthArray(ahfits::e_WRITEONLY, "PHAS_OUTER5X5", l_phas_outer5x5);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "ACTX", l_actx);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "ACTY", l_acty);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "DETX", l_detx);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "DETY", l_dety);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "FOCX", l_focx);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "FOCY", l_focy);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "X", l_x);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "Y", l_y);
    routerbp->connectFixedLengthArray(ahfits::e_WRITEONLY, "PHAS", l_phas);
    routerbp->connectFixedLengthArray(ahfits::e_WRITEONLY, "PHASALL", l_phasall);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "PHA", l_pha);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "PI", l_pi);
    routerbp->connectScalar(ahfits::e_WRITEONLY, "GRADE", l_grade);
    routerbp->connectBit(ahfits::e_WRITEONLY, "PROC_STATUS", l_procStatus, numProcStatus);
    routerbp->connectBit(ahfits::e_WRITEONLY, "STATUS", l_status, numStatus);
    ahfits::firstRow(fpbad);
  }

  // loop through events
  // copy an element from the status array to the status of the event.
  // most status codes are preset above so that this lookup is all that
  // is needed to assign them to the event.
  // Status mask runs from 0-1281, offset from coordinates by one.
  for(ahfits::firstRow(fpout); ahfits::readOK(fpout); ahfits::nextRow(fpout)) {
    // read each event from event file.
    ahfits::readRow(fpout);

    
    if (!procstatus::processRow(l_procStatus)) {
      ahfits::writeRow(fpout);
      AH_INFO(ahlog::HIGH) << " *** PROC_STATUS false at row " << ahfits::currentRow(fpout)  << std::endl;
      AH_INFO(ahlog::HIGH) << "Skipping row." << std::endl;
      continue;
    } else {

      // Verify within boundary for CCD, ACT, RAW and SEGMENT
      if(l_ccdid < ccdmin || l_ccdid > ccdmax || l_ccdid == ccdbad) {
        AH_INFO(ahlog::HIGH) << " *** CCD_ID is null or out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "CCD_ID: " << l_ccdid << "\nSkipping row.";
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout);
        continue;
      }
      if(l_actx < actxmin || l_actx > actxmax || l_actx == actxbad) {
        AH_INFO(ahlog::HIGH) << " *** ACTX is null or out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "ACTX: " << l_actx << "\nSkipping row." << std::endl;
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout);
        continue;
      }
      if(l_acty < actymin || l_acty > actymax || l_acty == actybad) {
        AH_INFO(ahlog::HIGH) << " *** ACTY is null or out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "ACTY: " << l_acty << "\nSkipping row." << std::endl;
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout);
        continue;
      }
      if(l_rawx < rawxmin || l_rawx > rawxmax || l_rawx == rawxbad) {
        AH_INFO(ahlog::HIGH) << " *** RAWX is null or out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "RAWX: " << l_rawx << "\nSkipping row." << std::endl;
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout);
        continue;
      }
      if(l_rawy < rawymin || l_rawy > rawymax || l_rawy == rawybad) {
        AH_INFO(ahlog::HIGH) << " *** RAWY is null or out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "RAWY: " << l_rawy << "\nSkipping row." << std::endl;
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout); 
        continue;
      }
      if(l_segment < segmin || l_segment > segmax || l_segment == segbad) {
        AH_INFO(ahlog::HIGH) << " *** SEGMENT null or is out of range for row " << ahfits::currentRow(fpout) << std::endl;
        AH_INFO(ahlog::HIGH) << "SEGMENT: " << l_segment << "\nSkipping row." << std::endl;
        l_status[2] = 1;
        statusCounter[2]++;
        ahfits::writeRow(fpout);
        continue;
      }

      // Copy PHAS_INNER3X3 from original telemetry
      if(param.m_copyphas) { 
        for(int ii = 0; ii < LENPHAS; ++ii) l_phas[ii] = l_phas_inner3x3[ii];
      }

      // set bad pixel flag in 3x3 array if appropriate conditions are found
      // TBAD = PH value to assign a bad pixel in the 3x3
      // initialize phas_mask, which holds the mask values of whether a 3x3 pixel is bad
      if (param.m_resetflags) {
        // Reset phas array
        for (int ii=0; ii < LENPHAS; ++ii) l_phasMask[ii] = 0; 
        //set STATUS (0-23) & STATUS(29-47)=0 
        for (int ii=0; ii<=22; ++ii) l_status[ii] = 0;
        for (int ii=30; ii<=47; ++ii) l_status[ii] = 0;
      }        
  
      //test 3x3 for statusMask bits (b5-9, 15-16) if set PHAS_MASK=1
      setBad3x3(l_ccdid, l_segment, l_readnode, l_actx-1, l_acty-1, 0x000183e0LL, statusMask, l_phasMask);

      //status bit is unique to each event, Hereafter this values is updated
      //in the for loop, and statusMask values will be unchanged
      statusbit = statusMask[l_ccdid][l_actx-1][l_acty-1];

      //set Bit0 = 1 if bad pixel
      if ((statusbit & BitBadStatus) != 0) {
        statusbit |= 0x000000001LL;
      }

      // copy an element from the status array to the status of the event.
      // most status codes are preset above so that this look up is all that is needed
      // to assign them to the event.
      setBitChar(l_status, statusCounter, statusbit);

      // Write to outfile
      ahfits::writeRow(fpout);

      // Write flagged pixels status mask to fpbad as bad pixel list for debugging
      if(badPix) {
        if((statusbit & 0x37f7dbc1LL) != 0) {
          // write bad pixel list
          ahfits::writeRow(fpbad);
          ahfits::nextRow(fpbad);
        }
      } // End write to bad pixel file
    } // End if proc status
  } // End main event loop
  
  // If outbadimg is set, then create an image for sxiexpomap
  int outofbounds = 0;
  int calsource = 0;
  int badpixel = 0;
  int outoffov = 0;
  int goodpixel = 0;
  bool ccd_off[4] = { 0,0,0,0 };

  // Read the DETNAM keywords from the event file
  // Determine which CCDs we are reading from
  // CCD:all, CCD12: CCD1+2, CCD34: CCD3+4
  std::string detnam = ahfits::getKeyValStr(fpout,"DETNAM");

  if(ahgen::strtoupper(detnam) == "CCD") { 
    ccd_off[0] = 0;
    ccd_off[1] = 0;
    ccd_off[2] = 0;
    ccd_off[3] = 0;
  } else if(ahgen::strtoupper(detnam) == "CCD12") {
    ccd_off[2] = 1;
    ccd_off[3] = 1;
  } else if(ahgen::strtoupper(detnam) == "CCD34") {
    ccd_off[0] = 1;
    ccd_off[1] = 1;
  } else {
    AH_ERR << "In file " << param.m_infile << ", DETNAME keyword not a valid CCD" << std::endl;
    AH_ERR << "Not creating output SXI image file " << param.m_outbadimg << std::endl;
    AH_THROW_RUNTIME("Bad DETNAME: " + detnam);
  }

  if(ahgen::strtoupper(param.m_outbadimg) != "NONE") {

    ahfits::Img2dLng map;     // Exposure map to be output in DET coordinates
    ahfits::Img2dLng mapOut;  // Copy of exposure map with coordinates swapped (for fitsio)
    ahfits::FilePtr fptr;     // File pointer for passed FITS file
    ahfits::Img2dFlt fmapdet;     // Fraction map from CR echo det, DET coords
    ahfits::Img2dFlt fmapdetOut;  // Copy of fmapdet with coordinates swapped (for fitsio)

    // Allocate memory for exposure map arrays
    map.resize(SXI_DET_SIZE);
    mapOut.resize(SXI_DET_SIZE);
    for(int ii = 0; ii < SXI_DET_SIZE; ++ii) {
      map[ii].resize(SXI_DET_SIZE);
      mapOut[ii].resize(SXI_DET_SIZE);
      for(int jj = 0; jj < SXI_DET_SIZE; ++jj) {
        map[ii][jj] = -1;
        mapOut[ii][jj] = 0;
      }
    }
    //Image should have value 0 for good pixel

    // Allocate memory for CR echo fraction map
    if (param.m_echoflag && param.m_echomap) {
      fmapdet.resize(SXI_DET_SIZE);
      fmapdetOut.resize(SXI_DET_SIZE);
      for(int ii = 0; ii < SXI_DET_SIZE; ++ii) {
        fmapdet[ii].resize(SXI_DET_SIZE);
        fmapdetOut[ii].resize(SXI_DET_SIZE);
        for(int jj = 0; jj < SXI_DET_SIZE; ++jj) {
          fmapdet[ii][jj] = 0;
          fmapdetOut[ii][jj] = 0;
        }
      }
    }

    //open mask file
    ahfits::open(param.m_maskfile,"MASK",&fptr);
    if(0 == ahfits::numRows(fptr)) {
      AH_THROW_RUNTIME("SXI mask file has zero rows");
    }
   
    // setup router
    ahfits::Router router(fptr); // Router creation for FITS file
    
    // make connections to local variables
    int mask_ccdid = 0;
    int map_detx = 0;
    int map_dety = 0;
    int mask_actx = 0;
    int mask_acty = 0;
    router.connectScalar(ahfits::e_READONLY, "CCD_ID", mask_ccdid);
    router.connectScalar(ahfits::e_READONLY, "DETX", map_detx);
    router.connectScalar(ahfits::e_READONLY, "DETY", map_dety);
    router.connectScalar(ahfits::e_READONLY, "ACTX", mask_actx);
    router.connectScalar(ahfits::e_READONLY, "ACTY", mask_acty);

    // read table
    for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
      ahfits::readRow(fptr);
      // Skip row if there is a null value
      if(map_detx-1 < 0 || map_dety-1 < 0) continue;
      // Skip row if we are not processing the CCD
      if(ccd_off[mask_ccdid]) continue;

      // Pre-set pixel to zero
      map[map_detx-1][map_dety-1] = 0;

      if((statusMask[mask_ccdid][mask_actx-1][mask_acty-1] & 0x00000002LL) != 0 ) {
        // Cal source
        // Check for bit 1
        map[map_detx-1][map_dety-1] = 1;
        calsource++;
      }
      if((statusMask[mask_ccdid][mask_actx-1][mask_acty-1] & BitBadStatus) != 0 ){
        // bad pixel
        // Check for BitBadStatus
        map[map_detx-1][map_dety-1] = 2;
        badpixel++;
      }
      if((statusMask[mask_ccdid][mask_actx-1][mask_acty-1] & 0x0000001cLL) != 0 ){
        // Out of field of view/discriminator
        // Check for bit 2, 3, 4
        map[map_detx-1][map_dety-1] = -1;
        outoffov++;
      }
      if(map[map_detx-1][map_dety-1]==0) goodpixel++;
      if (param.m_echoflag && param.m_echomap) {
        fmapdet[map_detx-1][map_dety-1] = fracmap[mask_ccdid][mask_actx-1][mask_acty-1];
      }
      
    } // end foreach row

    ahfits::close(fptr);

    // cfitsio follows fortran conventions in dimensionality for writing images.
    //
    // From the FITS documentation:
    // if a FITS image has NAXIS1 = 100 and NAXIS2 = 50, then a 2-D
    // array just large enough to hold the image should be declared as
    // array[50][100] and not as array[100][50]
    //
    // A copy of the detector statusMask is made with inverted coordinates
    // before being written to the output file.
    for(int ix = 0; ix < SXI_DET_SIZE; ++ix) {
      for(int jy = 0; jy < SXI_DET_SIZE; ++jy) { 
        mapOut[jy][ix] = map[ix][jy];
        if(map[ix][jy] == -1) outofbounds++; // Count the number of out of bounds/Out of FOV pixels
      }
    }
    if (param.m_echoflag && param.m_echomap) {
      for(int ix = 0; ix < SXI_DET_SIZE; ++ix) {
        for(int jy = 0; jy < SXI_DET_SIZE; ++jy) { 
          fmapdetOut[jy][ix] = fmapdet[ix][jy];
        }
      }
    }
    // Subtract the out of FOV pixels to get the correct out of bounds pixels
    outofbounds-=outoffov;

    // Write the image to the FITS file
    ahfits::writeImage(fpmap,"",mapOut,0.,1.,0);
    ahfits::writeKeyComment(fpmap,"Image Values\n"
                                  "   0: Good Pixel\n"
                                  "   1: Calibration source pixel\n"
                                  "   2: bad pixels set by parameter 'bad_status\n"
                                  "  -1: Null, out of CCD or area discrimination");

    AH_INFO(ahlog::HIGH) << "Pixel Counts -" << std::endl;
    AH_INFO(ahlog::HIGH) << "      Good Pixels  : " << goodpixel << std::endl;
    AH_INFO(ahlog::HIGH) << "      Cal Source   : " << calsource << std::endl;
    AH_INFO(ahlog::HIGH) << "      Bad Pixels   : " << badpixel << std::endl;
    AH_INFO(ahlog::HIGH) << "      Out of Bounds: " << outofbounds << std::endl;
    AH_INFO(ahlog::HIGH) << "      Out of FOV   : " << outoffov << std::endl;

    if (param.m_echoflag && param.m_echomap) {
      ahfits::writeImage(fpmap,"ECHOMAP",fmapdetOut,0.,1.,0);
      ahfits::writeKeyComment(fpmap,
        "Fraction map from cosmic ray echo detection in sxiflagpix task");
    }

    if(ahlog::get_debug()) {
      // For Testing Purposes:
      ahfits::Img2dLngLng ccd1, ccd2, ccd3, ccd4;
      if(!ccd_off[0]) ccd1.resize(SXI_CCD_SIZE);
      if(!ccd_off[1]) ccd2.resize(SXI_CCD_SIZE);
      if(!ccd_off[2]) ccd3.resize(SXI_CCD_SIZE);
      if(!ccd_off[3]) ccd4.resize(SXI_CCD_SIZE);
      for(int ii = 0; ii < SXI_CCD_SIZE; ++ii) {
        if(!ccd_off[0]) ccd1[ii].resize(SXI_CCD_SIZE);
        if(!ccd_off[1]) ccd2[ii].resize(SXI_CCD_SIZE);
        if(!ccd_off[2]) ccd3[ii].resize(SXI_CCD_SIZE);
        if(!ccd_off[3]) ccd4[ii].resize(SXI_CCD_SIZE);
      }

      // Write CCD images in separate extensions with status information 
      for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
        if(!ccd_off[0]) for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) ccd1[jy][ix] = statusMask[0][ix][jy];
        if(!ccd_off[1]) for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) ccd2[jy][ix] = statusMask[1][ix][jy];
        if(!ccd_off[2]) for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) ccd3[jy][ix] = statusMask[2][ix][jy];
        if(!ccd_off[3]) for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) ccd4[jy][ix] = statusMask[3][ix][jy];
      }
      if(!ccd_off[0]) ahfits::writeImage(fpmap,"CCD0",ccd1,0.,1.,0);
      if(!ccd_off[1]) ahfits::writeImage(fpmap,"CCD1",ccd2,0.,1.,0);
      if(!ccd_off[2]) ahfits::writeImage(fpmap,"CCD2",ccd3,0.,1.,0);
      if(!ccd_off[3]) ahfits::writeImage(fpmap,"CCD3",ccd4,0.,1.,0);
    }
  } // end if status map

  AH_INFO(ahlog::HIGH) << "Total Flags Counted:" << std::endl;
  for(int ii=0; ii<LENSTATUS; ++ii) {
    if(statusCounter[ii]>0) AH_INFO(ahlog::HIGH) << "      Flag " << ii+1 << ": " << statusCounter[ii] << std::endl;
  }

} // end doWork

// ****************************************************************************

void finalize(ahfits::FilePtr fpout, ahfits::FilePtr fpbad, ahfits::FilePtr fpmap) {

  // Close all FITS file pointers
  ahfits::close(fpout);
  ahfits::close(fpbad);
  ahfits::close(fpmap);
  AH_INFO(ahlog::HIGH) << "Finished." << std::endl;

} // end finalize

// ****************************************************************************

void setBitChar(char * l_status, int * statusCounter, long long maskStatus) {

  // Convert status to array of length 48, setting each bit individually.
  // ahfits writes "bit 0" to index 0 and "bit 31" to index 31.
  // Bit 0 is considered the bad status flag
  // Don't set bits 23-29 (flags 24-30), since these are event-specific and not in maskStatus
  for (int ii=0; ii<=22; ++ii) {
    if(0 == ((0x800000000000LL >> ((LENSTATUS-1)-ii)) & maskStatus)) {
      l_status[ii] = 0;
    } else {
      l_status[ii] = 1;
      statusCounter[ii]++;
    }
  }
  for (int ii=30; ii<LENSTATUS; ++ii) {
    if(0 == ((0x800000000000LL >> ((LENSTATUS-1)-ii)) & maskStatus)) {
      l_status[ii] = 0;
    } else {
      l_status[ii] = 1;
      statusCounter[ii]++;
    }
  }

} // end setBitChar

// ****************************************************************************

void transformAreaDiscrimToAct(ahfits::FilePtr fpout, int * actvnode, vec2dBool & aron,
                               vec2dLng & arin, vec2dLng & arou) {

  // Subroutine for reading keywords ACTVNODE, CnSmARON, CnSmARIN, CnSmAROU
  // and converting them from raw coordinates to act coordinates

  // Strings to hold raw area discrimination keywords
  std::string aron_str = "";
  std::string arin_str = "";
  std::string arou_str = "";
  std::string switchKeyword = "";
  std::string inclKeyword = "";
  std::string exclKeyword = "";

  // Arrays to hold values from area discrimination keywords
  int aron_tmp[5] = { 0,0,0,0,0 };
  int arin_tmp[4] = { 0,0,0,0 };
  int arou_tmp[16] = { 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 };

  int readnode = 0;

  // For keyword of the form CnSmAR__:
  // Cn refers to CCD ID (n = 0: CCD1, 1: CCD2, 2: CCD3, 3: CCD4) 
  // Sm refers to segment (m = 0: Segment AB, 1: Segment CD)
  //
  // The keywords are:
  // C0S0ARON C0S0ARIN C0S0AROU
  // C0S1ARON C0S1ARIN C0S1AROU
  // C1S0ARON C1S0ARIN C1S0AROU
  // C1S1ARON C1S1ARIN C1S1AROU
  // C2S0ARON C2S0ARIN C2S0AROU
  // C2S1ARON C2S1ARIN C2S1AROU
  // C3S0ARON C3S0ARIN C3S0AROU
  // C3S1ARON C3S1ARIN C3S1AROU
  //
  // CnSmARON is a boolean array giving the flags(on/off) of inclusion and 
  // exclusion regions
  // Default: CnSmARON = (1,0,0,0,0)
  // aron[0] = true/false to use inclusion area
  // aron[1] = true/false to use first exclusion area
  // aron[2] = true/false to use second exclusion area
  // aron[3] = true/false to use third exclusion area
  // aron[4] = true/false to use fourth exclusion area
  
  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    for(int i_seg = 0; i_seg < 2; ++i_seg) {
      int idx = 2*i_ccd+i_seg;
      // Reset area discrimination arrays to 0
      for(int ii = 0; ii < 5; ++ii) aron[idx][ii] = 0;
      for(int ii = 0; ii < 4; ++ii) arin[idx][ii] = 0;
      for(int ii = 0; ii < 16; ++ii) arou[idx][ii] = 0;

      // create keyword from CCD ID and Segment ID
      // Reset streams with each iteration
      
      // Get keyword value for CnSmARON
      std::stringstream sson; // String stream to get current CnSmARON keyword
      sson << "C" << i_ccd << "S" << i_seg << "ARON";
      switchKeyword = sson.str();
      aron_str = ahfits::getKeyValStr(fpout,switchKeyword);
      parseStringInt(aron_str, 5, aron_tmp);
      for(int ii = 0; ii < 5; ++ii) aron[idx][ii] = aron_tmp[ii];

      // Get keyword value for CnSmARsION
      std::stringstream ssin; // String stream to get current CnSmARON keyword
      ssin << "C" << i_ccd << "S" << i_seg << "ARIN";
      inclKeyword = ssin.str();
      arin_str = ahfits::getKeyValStr(fpout,inclKeyword);
      parseStringInt(arin_str, 4, arin_tmp);

      // Get keyword value for CnSmAROU
      std::stringstream ssou; // String stream to get current CnSmARON keyword
      ssou << "C" << i_ccd << "S" << i_seg << "AROU";
      exclKeyword = ssou.str();
      arou_str = ahfits::getKeyValStr(fpout,exclKeyword);
      parseStringInt(arou_str, 16, arou_tmp);

      // Get readnode from ACTVNODE keyword
      readnode = actvnode[idx];

      if(aron[idx][0] && (arin_tmp[0] == -1 || arin_tmp[1] == -1 ||
         arin_tmp[2] == -1 || arin_tmp[3] == -1)) {
        AH_THROW_RUNTIME(sson.str() + " keyword is set to on for inclusion region, "
                         "but coordinate is set to -1.");
      } else {
        // Convert inclusion regions from RAW to ACT
        // CnSmARIN = (xxx,yyy,zzz,hhh)
        // X start: xxx
        convertRawxToActx(i_seg, readnode, arin_tmp[0]);
        // X end: yyy
        convertRawxToActx(i_seg, readnode, arin_tmp[1]);
        // Y start: zzz
        arin_tmp[2] += 1;
        // Y end: hhh 
        arin_tmp[3] += 1;
        for(int ii = 0; ii < 4; ++ii) arin[idx][ii] = arin_tmp[ii];
      } // end check if coordinates are -1

      // Convert exclusion regions from RAW to ACT
      // CnSmAROU = (xxx,xxx,xxx,xxx,yyy,yyy,yyy,yyy,zzz,zzz,zzz,zzz,hhh,hhh,hhh,hhh)
      // Set each region individually
      for(int i_arou = 0; i_arou < 4; ++i_arou) {
        if(aron[idx][1+i_arou] && (arou_tmp[0] == -1 || arou_tmp[1] == -1 ||
           arou_tmp[2] == -1 || arou_tmp[3] == -1)) {
          AH_THROW_RUNTIME(sson.str() + " keyword is set to on for exclusion region, "
                           "but coordinate is set to -1.");
        } else {
          //if RAWX is opposite in sense to ACTX because of the readout node being used,
          // then the first entry in ARIN is actually xstop in ACT, and the second entry is
          // the xstart in ACT
          convertRawxToActx(i_seg, readnode, arou_tmp[0+4*i_arou]);
          convertRawxToActx(i_seg, readnode, arou_tmp[1+4*i_arou]);
          arou_tmp[2+4*i_arou] += 1;
          arou_tmp[3+4*i_arou] += 1;
        } // end loop exclusion regions
        for(int ii = 0; ii < 16; ++ii) arou[idx][ii] = arou_tmp[ii];
      } // end check if coordinates are -1
    } // end loop segments
  } // end loop ccds
} // end transformAreaDiscrimToAct

// ****************************************************************************

void convertRawxToActx(int segment, int readnode, int & xcoord) {

  // This is a rough sketch of the layout, with the CCDs, READNODE 
  // (Denoted by "oX" or "Xo", where X is the node)
  // And sense of direction of the axes.
  // NOTE: Map below updated on 2014-09-14. Check for latest version.
  //
  // o--> DETX, ACTY
  // |   ______________________________________________________
  // v  |oA-> RAWY            CCD2 | CCD4             RAWY <-Do|
  // D A||                         |                          ||
  // E C|vRAWX                     |                     RAWX V|
  // T T|                          |                           |
  // Y X|oB                     AB | CD                      Co|
  //    |--------------------------|---------------------------|
  //    |oC                     CD | AB                      Bo|
  //    |                          |                           |
  //    |^RAWX                     |               ACTX, RAWX ^|
  //    ||                         |                          ||
  //    |oD-> RAWY                 |            ACTY, RAWY <-Ao|
  //    |======================================================|
  //    |oA-> RAWY, ACTY      CCD1 | CCD3             RAWY <-Do|
  //    ||                         |                          ||
  //    |v RAWX, ACTX              |                     RAWX v|
  //    |                          |                           |
  //    |oB                     AB | CD                      Co|
  //    |--------------------------|---------------------------|
  //    |oC                     CD | AB                      Bo|
  //    |^ RAWX                    |                 ACTX,RAWX^|
  //    ||                         |                          ||
  //    |oD-> RAWY                 |             ACTY, RAWY<-Ao|
  //    |__________________________|___________________________|
  //

  // For all cases we have:
  // Segment AB, Node A: ACTX = RAWX + 1
  // Segment AB, Node B: ACTX = 320 - RAWX
  // Segment CD, Node C: ACTX = 321 + RAWX
  // Segment CD, Node D: ACTX = 640 - RAWX
  // Changed for updated READNODE ACTVNODE settings        EDM 20150120
  // READNODE=0 for segA,D and =1 for seg B,C; ACTVNODE entries are likewise

  if(0 == segment) { // Segment AB
    if(0 == readnode) {
      // Segment AB, Node A: ACTX = RAWX + 1
      xcoord = 1+xcoord;
    } else { // Node B
      // Segment AB, Node B: ACTX = 320 - RAWX
      xcoord = 320-xcoord;
    }
  } else { // Segment CD
    if(1 == readnode) {         //EDM 20150120
      // Segment CD, Node C: ACTX = 321 + RAWX
      xcoord = 321+xcoord;
    } else {
      // Segment CD, Node D: ACTX = 640 - RAWX
      xcoord = 640-xcoord;
    }
  }
} // end convertRawxToActx

// ****************************************************************************

void copyMaskArray(std::string & maskfile, int nboundnbr, const std::string telescop, const std::string instrume, std::vector<vec2dLngLng>  & statusMask) {
  
  // read calibration/boundary mask event
  // Values in mask are defined as:
  // -1     outside FOV
  //  0     within FOV (good pixel)
  //  1     within calibration source region (Bit 1)                
  //  2     on a CCD boundary (Bit 10)
  //  3     on a segment boundary (Bit 12)
  //  4     within calibration source regions and on a CCD boundary (Bits 1 and 10)
  //  5     in the gap between CCDs (Bit 10)
  // Error if open file and quit
 
  // set up associative arrays for mask info
  std::map<int,int> maskBitCode;   // Map of bit codes for the mask
  std::map<int,int> maskBitCodeNb; // Map of bit codes for the mask pixel neighbors

  // Local variables to read from maskfile
  int l_actx = 0;
  int l_acty = 0;
  int l_ccdid = 0;
  int l_value = 0;
  std::string inst = "";
  
  // Bit codes for the pixel
  //
  // everything set
  maskBitCode.insert(std::pair<int,int>(-1,-1));
  // 00000000 00000000 00000000 00000000 -> nothing set, everything OK
  maskBitCode.insert(std::pair<int,int>(0,0));
  // 00000000 00000100 00000000 00000000 -> bit 1 set - cal mask region
  maskBitCode.insert(std::pair<int,int>(1,0x00000002LL));
  // 00000000 00000000 00000000 10000000 -> bit 10 set - CCD boundary
  maskBitCode.insert(std::pair<int,int>(2,0x00000400LL));
  // 00000000 00000000 00000000 01000000 -> bit 12 set - segment boundary
  maskBitCode.insert(std::pair<int,int>(3,0x00001000LL));
  // 00000000 00000100 00000000 10000000 -> bits 1 and 10 set
  maskBitCode.insert(std::pair<int,int>(4,0x00000402LL));
  // 00000000 00000000 00000000 10000000 -> bit 10 set - same as CCD boundry
  maskBitCode.insert(std::pair<int,int>(5,0x00000400LL));

  // Bit codes for the neighbors
  // 00000000 00000000 10000000 00000000 -> bit 21 set - neighbor of CCD boundary
  maskBitCodeNb.insert(std::pair<int,int>(2,0x00200000LL));
  // 00000000 00000000 01000000 00000000 -> bit 22 set - neighbor of segment boundary
  maskBitCodeNb.insert(std::pair<int,int>(3,0x00400000LL));

  ahfits::FilePtr fptr; // File pointer for passed FITS file

  ahfits::open(maskfile,"MASK",&fptr);
  if(!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open MASK CALDB file");
  }
  
  // Check for correct instrument
  inst = ahfits::getKeyValStr(fptr, "INSTRUME");
  
  if("SXI" != inst) {
    AH_THROW_RUNTIME("INSTRUME keyword in MASK file should be SXI, not " + inst);
  }

  // setup router
  ahfits::Router router(fptr); // Router creation for FITS file
  
  // make connections to local variables
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);
  router.connectScalar(ahfits::e_READONLY, "ACTX", l_actx);
  router.connectScalar(ahfits::e_READONLY, "ACTY", l_acty);
  router.connectScalar(ahfits::e_READONLY, "VALUE", l_value);

  // read table
  for(ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    // skip out of bound values
    if(l_value == -1) { continue; } 
    if(l_actx < 1 || l_acty < 1) { continue; }
    
    // set value bit at coordinate and neighboring coordinates 
    statusMask[l_ccdid][l_actx-1][l_acty-1] |= maskBitCode[l_value];
    if(2 == l_value || 3 == l_value || 4 == l_value) {
      setBitBoundNeighbors(l_ccdid,l_actx-1,l_acty-1,nboundnbr,maskBitCodeNb[2],maskBitCodeNb[3],statusMask);
    } // end if neighbors
  } // end foreach row
} // end copyMaskInfo

// ****************************************************************************

void maskAreaDiscrimination(int * actvnode, vec2dBool & aron, vec2dLng & arin, vec2dLng & arou, 
                            std::vector<vec2dLngLng> & statusMask) {

  // loop through ccd, segment and actx/y coordinates in statusMask
  // Check for area discrimination
  int seg_start = 0;
  int seg_stop = 0;
  int xstart = 0;
  int xstop = 0;
  int ystart = 0;
  int ystop = 0;

  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    for(int i_seg = 0; i_seg < 2; ++i_seg) {
      int idx = 2*i_ccd+i_seg;
      // process inclusion region 
      // Constrict region to segment as to not flag good pixels
      if(0 == i_seg) {
        seg_start = 0;
        seg_stop = 320;
      } else {
        seg_start = 320;
        seg_stop = SXI_CCD_SIZE;
      }
      if(aron[idx][0]) {
        // Take the smaller pixel coordinate as xstart
        xstart = 0;
        xstop = 0;
        ystart = 0;
        ystop = 0;
        // Need to swap start and stop positions depending on the readnode
        // if RAWX is opposite in sense to ACTX because of the readout node being used,
        // then the first entry in ARIN is actually xstop in ACT, and the second entry is
        // the xstart in ACT        
        if(actvnode[idx] != i_seg) {
          xstart = (arin[idx][1]-1);
          xstop = (arin[idx][0]-1);
        } else {
          xstart = (arin[idx][0]-1);
          xstop = (arin[idx][1]-1);
        }
        ystart = arin[idx][2]-1;
        ystop =  arin[idx][3]-1;

        for(int ix = seg_start; ix < seg_stop; ++ix) {
          for(int iy = 0; iy < SXI_CCD_SIZE; ++iy) {
            if(!(ix>=xstart&&ix<=xstop&&iy>=ystart&&iy<=ystop)) {
              // Outside of included area
              // set bit 4
              // 00000000 00000000 00000000 00010000
              statusMask[i_ccd][ix][iy] |= 0x00000010LL;
            } 
            if((ix == xstart || ix == xstop) &&
               (iy >= ystart && iy <= ystop)) {
              // On top or bottom side of included area
              // set bit 13
              // 00000000 00000000 00100000 00000000
              statusMask[i_ccd][ix][iy] |= 0x00002000LL;
            }
            if((iy == ystart || iy == ystop) &&
               (ix >= xstart && ix <= xstop)) {
              // On left or right side of included area
              // set bit 13
              // 00000000 00000001 00000000 00000000
              statusMask[i_ccd][ix][iy] |= 0x00002000LL;
            }
          } // end loop y
        } // end loop x
      } // process inclusion regions

      for(int i_exc = 0; i_exc<4; ++i_exc) {
        if(aron[idx][i_exc+1]) {
          xstart = 0;
          xstop = 0;
          ystart = 0;
          ystop = 0;
          if(actvnode[idx] != i_seg) {
            xstart = (arou[idx][1+4*i_exc]-1);
            xstop = (arou[idx][0+4*i_exc]-1);
          } else {
            xstart = (arou[idx][0+4*i_exc]-1);
            xstop = (arou[idx][1+4*i_exc]-1);
          }
          ystart = arou[idx][2+4*i_exc]-1;
          ystop = arou[idx][3+4*i_exc]-1;
          for(int ix = seg_start; ix < seg_stop; ++ix) {
            for(int iy = 0; iy < SXI_CCD_SIZE; ++iy) {
              if(ix >= xstart && ix <= xstop &&
                 iy >= ystart && iy <= ystop) {
                // Inside of excluded area
                // set bit 4
                // 00000000 00000000 00000000 00010000
                statusMask[i_ccd][ix][iy] |= 0x00000010LL;
              }
              if((ix == xstart || ix == xstop) &&
                 (iy >= ystart && iy <= ystop)) {
                // On top or bottom side of excluded area
                // set bit 13
                // 00000000 00000000 00100000 00000000
                statusMask[i_ccd][ix][iy] |= 0x00002000LL;
              } 
              if((iy == ystart || iy == ystop) &&
                 (ix >= xstart && ix <= xstop)) {
                // On left or right side of excluded area
                // set bit 13
                // 00000000 00000000 00100000 00000000
                statusMask[i_ccd][ix][iy] |= 0x00002000LL;
              } // end if in exclusion region
            } // end loop y
          } // end loop x
        } // end if exclusion regions
      } // end loop exclusion regions
    } // end loop segment
  } // end loop ccd
} // end maskAreaDiscrimination

// ****************************************************************************

void maskWindowBoundary(int winStart, int winSize, int nboundnbr, std::vector<vec2dLngLng> & statusMask) {

  // Neighbor coordinates
  int iy_n = 0;

  // adjust winStart to off-by-one statusMask
  winStart -= 1;

  // set bit 3 for pixels before the winStart region
  for(int iy = 0; iy < winStart; ++iy) {
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      // set bit 3 for CCD 1 and 2 in statusMask
      // 00000000 00000000 00000000 00003000
      statusMask[0][ix][iy] |= 0x00000008LL;
      statusMask[1][ix][iy] |= 0x00000008LL;
    } // end loop x
  } // end loop y
  // set bit 3 for pixels after the winStart region
  for(int iy = winStart+winSize; iy < SXI_CCD_SIZE; ++iy) {
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      // set bit 3 for CCD 1 and 2 in statusMask
      // 00000000 00000000 00000000 00003000
      statusMask[0][ix][iy] |= 0x00000008LL;
      statusMask[1][ix][iy] |= 0x00000008LL;
    } // end loop x
  } // end loop y

  // Set boundaries of window region
  for(int iy = winStart; iy < winStart+winSize; iy+=winSize-1) {
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      // set bit 11 for CCD 1 and 2 in statusMask
      // 00000000 00000000 00001000 00000000
      statusMask[0][ix][iy] |= 0x00000800LL;
      statusMask[1][ix][iy] |= 0x00000800LL;
    } // end loop x
    // Set neighboring pixel status bits
    for(int jy=-nboundnbr; jy<=nboundnbr; ++jy) {  
      // set bit 21 for CCD 1 and 2 in statusMask
      // bit 13: 00000000 01000000 00000000 00000000
      iy_n = iy + jy;
      if (jy == 0 || iy_n < 0 || iy_n > SXI_CCD_SIZE-1) continue;
      for (int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
        statusMask[0][ix][iy_n] |= 0x00200000LL;
        statusMask[1][ix][iy_n] |= 0x00200000LL;
      } // end loop neighbor x
    } // end loop neighbor y
  } // end loop iy

} // end maskWindowBoundary

// ****************************************************************************

void maskChargeInjection(int ciperiod, int cioffset, int cifirst, int citrailnbr,
                         int ciprenbr, std::vector<vec2dLngLng> & statusMask) {

  // charge injection rows and preceding, trailing rows
  // the charge injection is coded in the microcode ID
  // the specifying of the CI row is provided by 3 keywords:
  // CISTATUS (CI mode, 1 = on, 0 = off)
  // CIPERIOD (CI period, i.e. number of ACTY rows between CI rows)
  // CIOFFSET (CI offset from the first injected row ACTY coordinate)
  // CIFIRST (CI first injected row ACTY coordinate)

  // Adjust CIOFFSET and CIFIRST for off-by-one in statusMask
  
  //cioffset -= 1;  REMOVED HY 20150407
  
  cifirst -= 1;

  for(int iy = cifirst+cioffset; iy < SXI_CCD_SIZE; iy+=ciperiod) {
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      // set bit5: 00000000 00000000 00100000
      statusMask[0][ix][iy] |= 0x00000020LL;
      statusMask[1][ix][iy] |= 0x00000020LL;
      statusMask[2][ix][iy] |= 0x00000020LL;
      statusMask[3][ix][iy] |= 0x00000020LL;
    } // end loop x
    for(int jy = 1; jy <= citrailnbr; ++jy) {
      // set bit15: 00000000 10000000 00000000
      int iy_n = iy + jy;
      if(iy_n > (SXI_CCD_SIZE-1)) break;
      for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
        statusMask[0][ix][iy_n] |= 0x00008000LL;
        statusMask[1][ix][iy_n] |= 0x00008000LL;
        statusMask[2][ix][iy_n] |= 0x00008000LL;
        statusMask[3][ix][iy_n] |= 0x00008000LL;
      } // end loop trail x neighbor
    } // end loop trail y neighbor
    for(int jy = 1; jy <= ciprenbr; ++jy) {
      // set bit16: 00000001 00000000 00000000
      int iy_n = iy - jy;
      if(iy_n < 0) break;
      for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
        statusMask[0][ix][iy_n] |= 0x00010000LL;
        statusMask[1][ix][iy_n] |= 0x00010000LL;
        statusMask[2][ix][iy_n] |= 0x00010000LL;
        statusMask[3][ix][iy_n] |= 0x00010000LL;
      } // end loop pre x neighbor
    } // end loop pre y neighbor

    // Set 1st trailing and 1st preceding row of the CI rows                      EDM 20150120
    int jy=1;
    // bit30: 00000000 00000000 00000000 00000010 00000000
    int iy_n1 = iy + jy;
    if (iy_n1 > SXI_CCD_SIZE-1) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n1] |= 0x40000000LL;
      statusMask[1][ix][iy_n1] |= 0x40000000LL;
      statusMask[2][ix][iy_n1] |= 0x40000000LL;
      statusMask[3][ix][iy_n1] |= 0x40000000LL;
    }// end loop x
    // bit31: 00000000 00000000 00000000 00000001 00000000
    iy_n1 = iy - jy;
    if (iy_n1 < 0) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n1] |= 0x80000000LL;
      statusMask[1][ix][iy_n1] |= 0x80000000LL;
      statusMask[2][ix][iy_n1] |= 0x80000000LL;
      statusMask[3][ix][iy_n1] |= 0x80000000LL;
    }// end loop x
    // Set 2nd trailing and 2nd preceding row of the CI rows       EDM 20150120
    jy=2;
    // bit32: 00000000 00000000 00000000 00000000 10000000
    int iy_n2 = iy + jy;
    if (iy_n2 > SXI_CCD_SIZE-1) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n2] |= 0x100000000LL;
      statusMask[1][ix][iy_n2] |= 0x100000000LL;
      statusMask[2][ix][iy_n2] |= 0x100000000LL;
      statusMask[3][ix][iy_n2] |= 0x100000000LL;
    }// end loop x
    // bit33: 0000000 00000000 00000000 00000000 01000000
    iy_n2 = iy - jy;
    if (iy_n2 < 0) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n2] |= 0x200000000LL;
      statusMask[1][ix][iy_n2] |= 0x200000000LL;
      statusMask[2][ix][iy_n2] |= 0x200000000LL;
      statusMask[3][ix][iy_n2] |= 0x200000000LL;
    }// end loop x
    // Set 3rd trailing and 3rd preceding row of the CI rows         EDM 20150120
    jy=3;
    // bit34: 00000000 00000000 00000000  00000000 00100000
    int iy_n3 = iy + jy;
    if (iy_n3 > SXI_CCD_SIZE-1) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n3] |= 0x400000000LL;
      statusMask[1][ix][iy_n3] |= 0x400000000LL;
      statusMask[2][ix][iy_n3] |= 0x400000000LL;
      statusMask[3][ix][iy_n3] |= 0x400000000LL;
    }// end loop x
    // bit35: 00000000 00000000 00000000  00000000 00010000
    iy_n3 = iy - jy;
    if (iy_n3 < 0) break;
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      statusMask[0][ix][iy_n3] |= 0x800000000LL;
      statusMask[1][ix][iy_n3] |= 0x800000000LL;
      statusMask[2][ix][iy_n3] |= 0x800000000LL;
      statusMask[3][ix][iy_n3] |= 0x800000000LL;
    }// end loop x
  } // end loop y
} // end maskChargeInjection

// ****************************************************************************

void maskCREcho(ahfits::FilePtr fpout, int echonbr, int echomin, int echospth, 
                double echofrac, std::string gtifile, bool echomap,
                std::vector<vec2dLngLng> & statusMask, std::vector<vec2dDbl> & fracmap) {

  // Add the cosmic ray echo pixels and their neighbors to statusMask.

  // Set the relative delta_RAWX, delta_RAWY for each pixel in the 5x5
  // compared to center.
  // Pixel indexes show how the 5x5 are numbered:
  //
  //  20 21 22 23 24
  //  18  6  7  8 19    ^
  //  16  4  0  5 17    |
  //  14  1  2  3 15    |
  //   9 10 11 12 13  RAW_Y
  //  RAW_X --->      

  int delta_rawx[25] = { 0, -1, 0, 1, -1, 1, -1, 0, 1, -2, -1, 0, 1, 2, -2, 2, -2, 2, -2, 2, -2, -1, 0, 1, 2 };
  int delta_rawy[25] = { 0, -1, -1, -1, 0, 0, 1, 1, 1, -2, -2, -2, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 2, 2, 2 };

  // Local variables for reading the event file

  char l_ccdid = 0;             // Current CCD ID value from event loop
  double l_time=0.0;            // Current event TIME
  char l_segment = 0;           // Current Segment ID value from event loop
  char l_readnode = 0;          // Current Readout Node value from event loop
  int l_rawx=0, l_rawy=0;       // Current event pixel RAW coordinates
  long l_p_outer_most=0;        // P_OUTER_MOST bit array for pixels over split threshold
  int l_phas[LENPHAS] = { 0 };  // PHAS array
  char l_procStatus[LENSTATUS] = { 0 };      // column: PROC_STATUS (1st bit=1 => BAD)
  int l_grade; 
  double previous_event_time=0.0;   // TIME of previous event

  char l_time_null = 0;            // Flag for null event time

  // Variables used in looping through event file

  int pixel_rawx=0, pixel_rawy=0;  // Nearby pixel RAW coordinates
  int pixel_actx=0, pixel_acty=0;  // Nearby pixel ACT coordinates
  long bitmask=0;                  // Mask for testing P_OUTER_MOST
  long long num_inside_gti=0;      // Counter for events inside GTI
  long long num_outside_gti=0;     // Counter for events outside GTI

  // Variables for reading the GTI file

  ahfits::FilePtr fpgti=0;               // File pointer for GTI file
  ahfits::Router* rgti;                  // Pointer to data router for GTI file
  double l_gtistart=0.0, l_gtistop=0.0;  // START and STOP columns
  char l_gtistart_null = 0;              // Flag for null GTI START
  char l_gtistop_null = 0;               // Flag for null GTI STOP
  const std::string DEFAULT_GTI = "GTI";  // Default GTI EXTNAME
  std::string defgtiext = "";            // Default GTI EXTNAME
  bool time_outside_gti=false;           // Flag for current event not in a GTI
  bool gtieof=false;                     // Flag for GTI file at EOF
  double previous_gtistart=0.0;          // START value of previous GTI

  // Complete initialization of local arrays
  for (int i=0; i<LENPHAS; ++i) l_phas[i] = 0;
  for (int i=0; i<LENSTATUS; ++i) l_procStatus[i] = 0;

  // Initialize the fracgion map and normalization map; these should
  // be the same size as statusMask (4 X 640 X 640).

  // fracmap is passed as an argument
  std::vector<vec2dLng> normmap(4);

  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    fracmap[i_ccd].resize(SXI_CCD_SIZE);
    normmap[i_ccd].resize(SXI_CCD_SIZE);
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      fracmap[i_ccd][ix].resize(SXI_CCD_SIZE);
      normmap[i_ccd][ix].resize(SXI_CCD_SIZE);
      for(int jy = 0; jy < SXI_CCD_SIZE; ++jy) {
        fracmap[i_ccd][ix][jy] = 0;
        normmap[i_ccd][ix][jy] = 0;
      }
    }
  }

  // set up local variables and router for reading file
  ahfits::Router router(fpout);     // Router for output event file
  router.connectScalar(ahfits::e_READONLY, "CCD_ID", l_ccdid);
  router.connectScalar(ahfits::e_READONLY, "SEGMENT", l_segment);
  router.connectScalar(ahfits::e_READONLY, "READNODE", l_readnode);
  router.connectScalar(ahfits::e_READONLY, "TIME", l_time, &l_time_null);
  router.connectScalar(ahfits::e_READONLY, "RAWX", l_rawx);
  router.connectScalar(ahfits::e_READONLY, "RAWY", l_rawy);
  router.connectFixedLengthArray(ahfits::e_READONLY, "PHAS", l_phas);
  router.connectScalar(ahfits::e_READONLY, "GRADE", l_grade);
  router.connectScalar(ahfits::e_READONLY, "P_OUTER_MOST", l_p_outer_most);

  if (ahgen::strtoupper(gtifile) != "NONE") {
    defgtiext = DEFAULT_GTI;
    ahfits::open(gtifile, "", &fpgti);
    // Move to default GTI extension if extended syntax not used
    if (ahfits::isPrimary(fpgti)) ahfits::move(fpgti, defgtiext);
    AH_INFO(ahlog::HIGH) << "Opened GTI file " << gtifile << std::endl;

    rgti = new ahfits::Router(fpgti);
    rgti->connectScalar(ahfits::e_READONLY, "START", l_gtistart, &l_gtistart_null);
    rgti->connectScalar(ahfits::e_READONLY, "STOP" , l_gtistop, &l_gtistop_null);
  }

  // step through each event, taking PHAS, central RAWX, RAWY, and P_OUTER_MOST
  for(ahfits::firstRow(fpout); ahfits::readOK(fpout); ahfits::nextRow(fpout)) {
    // read each event from event file.
    ahfits::readRow(fpout);

    if (!procstatus::processRow(l_procStatus)) continue;

    // Check for NULL event time
    if (1 == l_time_null) continue;

    // Check for event times out of order
    if (l_time < previous_event_time) AH_THROW_RUNTIME("Event TIMES are not sorted");
    previous_event_time = l_time;

    // Test if event is within a GTI, if the GTI file was specified
    if (0 == fpgti) {
      time_outside_gti = false;
      num_inside_gti++;
    } else {
      l_gtistart_null = 0;
      l_gtistop_null = 0;
      // Search for next GTI potentially containing current time,
      // skipping those GTI with null start or stop
      while (l_time > l_gtistop && !gtieof) {
        ahfits::nextRow(fpgti);
        if (ahfits::readOK(fpgti)) {
          ahfits::readRow(fpgti);
          if (1 == l_gtistart_null || 1 == l_gtistop_null) continue;
          if (l_gtistart < previous_gtistart) {
            AH_THROW_RUNTIME("GTI START times are not sorted");
          } else {
            previous_gtistart = l_gtistart;
          }
        } else {
          gtieof = true;
        }
      }
      if (!gtieof && l_time >= l_gtistart) {
        time_outside_gti = false;
        num_inside_gti++;
      } else {
        time_outside_gti = true;
        num_outside_gti++;
      }
    }

    // process this event only if within a GTI
    if (time_outside_gti) continue;

    // skip the aimpoint segment
    if (l_ccdid == 1 && l_segment == 1) continue;

    // for each pixel 1-24 in the 5x5 (ignoring central), check to see if it’s above split threshold
    for (int ii=1; ii<25; ii++) {

      // get the RAW coords of that pixel
      pixel_rawx = l_rawx + delta_rawx[ii];
      pixel_rawy = l_rawy + delta_rawy[ii];

      // if the pixel is off the segment, go to next event
      if (pixel_rawx < 0 || pixel_rawx > 319 || pixel_rawy < 0 || pixel_rawy > 639) {
        continue;
      }
      
      // convert to ACT coords
      // this should convert pixel_actx in place:
      pixel_actx = pixel_rawx;   
      convertRawxToActx(l_segment, l_readnode, pixel_actx);
      pixel_acty = pixel_rawy + 1;

      // increment the normalization array in all cases (since the pixel is tested in this event)
      normmap[l_ccdid][pixel_actx-1][pixel_acty-1] += 1;

      // check against split threshold
      // if it’s in the 3x3, we can just check PHAS
      if (ii < 9) {
        if (l_phas[ii] > echospth) {
          // increment the fraction array
          fracmap[l_ccdid][pixel_actx-1][pixel_acty-1] += 1.;
        }
      // if it’s in the outer 5x5, we have to unpack P_OUTER_MOST
      } else {
        // P_OUTER_MOST is a bit mask encoding which outer pixels are above split threshold, 
        // with pixel 9 encoded in the least significant bit and pixel 24 in the most
        // significant bit.  So we left shift 1 by (ii-9) and compare that to P_OUTER_MOST
        bitmask = (l_p_outer_most & ( 1 << (ii-9) ));
        if (bitmask != 0) {
          // increment the fraction array
          fracmap[l_ccdid][pixel_actx-1][pixel_acty-1] += 1.;
        }
      }
    } // done checking 5x5 pixels

  } // done reading EVENT

  AH_INFO(ahlog::HIGH) << "Number of events inside GTI:  " << num_inside_gti << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of events outside GTI: " << num_outside_gti << std::endl;
  
  if (ahgen::strtoupper(gtifile) != "NONE") {
    delete rgti;
    ahfits::close(fpgti);
  }

  for (int i_ccd=0; i_ccd<4; ++i_ccd) {
    for (int ix=0; ix<SXI_CCD_SIZE; ++ix) {
      for (int iy=0; iy<SXI_CCD_SIZE; ++iy) {
        // normalize the fraction map
        fracmap[i_ccd][ix][iy] /= normmap[i_ccd][ix][iy];
        // skip pixels where normmap doesn’t have enough counts
        if (normmap[i_ccd][ix][iy] < echomin) continue;
        // flag the statusMask only where the fraction map is above the minimum fraction
        if (fracmap[i_ccd][ix][iy] > echofrac) {
          statusMask[i_ccd][ix][iy] |= 0x1000000000LL;
          // flag the neighbors with same STATUS bit
          setBitNeighbors (i_ccd, ix, iy, echonbr, 0x1000000000LL, statusMask);
        }
      } // next iy
    } // next ix
  } // next i_ccd

} // done function maskCREcho

// ****************************************************************************

void maskPixels(sxiflagpixlib::PixArray pixels, long long statusBit, long long statusBitNeighbor, 
                int npixnbr, std::vector<vec2dLngLng> & statusMask) {

 
  int numpix = pixels.m_numpix; // Total number of pixels in array
  int pix_ccd = 0;              // CCD from pixel data
  int pix_actx = 0;             // ACTX coordinate from pixel data
  int pix_acty = 0;             // ACTY coordinate from pixel data

  // defensive programming: unset bit and neighbor bit for each pixel
  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    for(int ix = 0; ix < SXI_CCD_SIZE; ++ix) {
      for(int iy = 0; iy < SXI_CCD_SIZE; ++iy) {
        statusMask[i_ccd][ix][iy] &= ~statusBit;
        statusMask[i_ccd][ix][iy] &= ~statusBitNeighbor;
      }
    }
  }

  // Loop over all pixels
  for(int i_pix = 0; i_pix < numpix; ++i_pix) {
    pix_ccd = pixels.m_ccdid[i_pix];
    pix_actx = pixels.m_actx[i_pix];
    pix_acty = pixels.m_acty[i_pix];

    // Set bit statusBit and set neighboring pixels statusBitNeighbor
    statusMask[pix_ccd][pix_actx-1][pix_acty-1] |= statusBit;
    setBitNeighbors(pix_ccd,pix_actx-1,pix_acty-1,npixnbr,statusBitNeighbor,statusMask);
  } // end loop pixels
} // end maskPixels

// ****************************************************************************

void setBit3x3surround(std::vector<vec2dLngLng> & statusMask) {

  // bit description:
  // bit status set to 14 if any of the 3x3 surrounding pixels has the b5-b9 or b15-b16 condition
  // Therefore testMask = 00000000 00000001 10000011 11100000
  
  // check for bit 14 and set neighbors
  for(int i_ccd = 0; i_ccd < 4; ++i_ccd) {
    for(int ix = 1; ix < SXI_CCD_SIZE-1; ++ix) {
      for(int iy = 1; iy < SXI_CCD_SIZE-1; ++iy) {
        //bits 5,6,7,8,9,15,16
        if((statusMask[i_ccd][ix][iy] & 0x000183e0LL) != 0) {
          setBitNeighbors(i_ccd, ix, iy, 1, 0x00004000LL, statusMask);
        } // end bit set neighbor
      } // end loop y 
    } // end loop x
  } // end loop ccd

} // end of setBit3x3surround

// ****************************************************************************

void setBitNeighbors(int ccdid, int actx, int acty, int npixnbr, long long bitnum, 
                     std::vector<vec2dLngLng> & statusMask) {
 
  // loop through neighbors and set bitnum as status
  for(int ix = (actx - npixnbr); ix <= (actx + npixnbr); ++ix) {
    for(int iy = (acty - npixnbr); iy <= (acty + npixnbr); ++iy) {
      if(ix >= 0 && ix < SXI_CCD_SIZE && iy >= 0 && iy < SXI_CCD_SIZE) {
        if((ix != actx) || (iy != acty)) {
          //set bit bitnum at ix, iy;
          statusMask[ccdid][ix][iy] |= bitnum;
        } // end bit set
      } // end boundary check
    } // end y neighbors
  } // end x neighbors
} // end setBitNeighbors

// ****************************************************************************

void setBad3x3(char ccdid, char segment, char readnode, int actx, int acty, long long testMask, 
               std::vector<vec2dLngLng> & statusMask, char * phasMask) {

  int dx = 0;
  int dy = 0;

  // loop through surrounding 3x3 coordinates
  // Determine which pixel to update TNULL
  // if a nonzero status is found, set pixel in 3x3 to tbad
  for(int ii = 0; ii < 9; ++ii) {

    if(1 == ii || 2 == ii || 3 == ii) dx = actx-1;
    if(4 == ii || 5 == ii)            dx = actx;
    if(6 == ii || 7 == ii || 8 == ii) dx = actx+1;

    if(1 == ii || 4 == ii || 6 == ii) dy = acty-1;
    if(2 == ii || 7 == ii)            dy = acty;
    if(3 == ii || 5 == ii || 8 == ii) dy = acty+1;


    //Flip x coordinate if readout node B or D
    if(segment == 0 && readnode == 1) dx *= -1;
    if(segment == 1 && readnode == 0) dx *= -1;

    if(dx > 0 && dx < SXI_CCD_SIZE && dy > 0 && dy < SXI_CCD_SIZE ){
      if((statusMask[ccdid][dx][dy] & testMask) != 0) {
        //l_phas[ii] = tbad;
        phasMask[ii] = 1;
      } // end bit test
    } // end boundary check
  } // end loop 3x3

} // end setBad3x3

// ****************************************************************************

void parseStringInt(std::string str, int length, int * outarr) {
 
  // This function takes in strings of format "(1,2,3,4,5)" and returns an array of ints, separated
  // by commas and ignoring excess beginning/ending whitespace or parentheses.

  std::string tmp_int = ""; // Temporary string to output
  bool trim = true;         // Check whether to continue trimming whitespace/parentheses

  //trim beginning and ending excess:
  while(trim) {
    if((str[0] == '(') || (str[0] == ' ')) { 
      str.erase(0,1);
    } else if((str[str.length()-1] == ')') || (str[str.length()-1] == ' ') || (str[str.length()-1] == '\n')) {
      str.erase(str.length()-1,1);
    } else { trim = false; }
  }

  // parse string with comma as a delimiter into output array:
  std::istringstream iss(str);
  for(int ii = 0; ii < length; ++ii) {
    getline(iss, tmp_int, ',');
    outarr[ii] = std::strtol(tmp_int.c_str(), 0, 10);
  }

}

// ****************************************************************************

void parseNumberList(const std::string & instr, std::vector<int> & outlist) {
  int status=0;
  int trim=1;         // trim spaces
  int skip=1;         // exclude empty items
  int guard=0;        // do not protect against commas in parentheses 

  char* in1=(char*)instr.c_str();
  char** items1=0;
  int nitems1=0;

  //empty output vector
  outlist.clear();
 
  //split string using commas
  items1=expand_item_list(in1, &nitems1, ',',trim,skip,guard,&status);
  if (status != 0) {
    free(items1);
    std::stringstream msg;
    msg << "failure in expand_item_list call: status = " << status;
    AH_THROW_RUNTIME(msg.str());
  }
  
  //get each number or hyphenated range
  for (int ii = 0; ii < nitems1; ++ii) {
    char* in2=items1[ii];
    char** items2=0;
    int nitems2=0;  
    items2=expand_item_list(in2, &nitems2, ':',trim,skip,guard,&status);
    if (status != 0) {
      free(items1);
      free(items2);
      std::stringstream msg;
      msg << "failure in expand_item_list call: status = " << status;  
      AH_THROW_RUNTIME(msg.str());
    }
    if (nitems2 == 1) {
      if (!ahgen::isNumber(items2[0])) {
        std::stringstream msg;
        msg << "non-numeric value found: " << items2[0]; 
        AH_THROW_RUNTIME(msg.str());
      }
      outlist.push_back(atoi(items2[0]));
    } else if (nitems2 == 2) {
      if (!ahgen::isNumber(items2[0])) {
        std::stringstream msg;
        msg << "non-numeric value found: " << items2[0]; 
        AH_THROW_RUNTIME(msg.str());
      }
      if (!ahgen::isNumber(items2[1])) {
        std::stringstream msg;
        msg << "non-numeric value found: " << items2[1]; 
        AH_THROW_RUNTIME(msg.str());
      }
      int jjstart = atoi(items2[0]);
      int jjstop = atoi(items2[1]);
      if (jjstart > jjstop) {
        free(items1);
        free(items2);
        AH_THROW_RUNTIME("Invalid list string: for a-b intervals a must be less than b");
      }
      for (int jj = atoi(items2[0]); jj <= atoi(items2[1]); ++jj) {
        outlist.push_back(jj);
      }
    } else { 
      free(items1);
      free(items2);
      AH_THROW_RUNTIME("FAIL"); 
    }
    free(items2);
  }

  free(items1);

  //decrement all items in outlist by one
  for(std::vector<int>::size_type ii = 0; ii != outlist.size(); ii++) {
    outlist[ii] = outlist[ii] - 1;
  }

}

// ********************************************************************************************************

void maskColumns(sxiflagpixlib::PixArray pixels, long long statusBit, long long statusBitPre, 
                 long long statusBitNeighbor, long long statusBitPreNeighbor,
                 int & npixnbr, std::vector<vec2dLngLng> & statusMask) {

  int numpix = pixels.m_numpix;
  int ccd_id = 0;
  int actx = 0;
  int acty = 0;
  int yextend = 0;

  for (int i_pix=0; i_pix<numpix;++i_pix) {
    ccd_id = pixels.m_ccdid[i_pix];
    actx = pixels.m_actx[i_pix];
    acty = pixels.m_acty[i_pix];
    yextend = pixels.m_yextend[i_pix]; // Y-extend off by one

    // Check for an illegal y-extend value
    if (yextend > SXI_CCD_SIZE) {
      AH_ERR << "for pixel in row " << i_pix << " in badpix file" << std::endl;
      AH_ERR << "(YEXTEND>SXI_CCD_SIZE): " 
             <<  yextend << ">" << SXI_CCD_SIZE << std::endl;
      AH_THROW_RUNTIME("Illegal YEXTEND value");
    }
    // Check that yextend+acty-1 does not exceed the CCD size
    // Subtract 1 to include the acty pixel
    if (yextend+acty-1 > SXI_CCD_SIZE) {
      AH_ERR << "for pixel in row " << i_pix << " in badpix file" << std::endl;
      AH_ERR << "YEXTEND extends beyond CCD size from ACTY" 
             <<  (yextend+acty-1) << ">" << SXI_CCD_SIZE << std::endl;
      AH_THROW_RUNTIME("Illegal YEXTEND value");
    }

    // must extend at least 1 pixel
    if ( yextend != 1 ) {
      // Extend the acty column by [acty-1:yextend-1] and set the yextend status bit
      for (int j=0; j<yextend; j++) {
        if (acty+j > SXI_CCD_SIZE) {
          AH_ERR << "for pixel in row " << i_pix << " in badpix file" << std::endl;
          AH_ERR << "YEXTEND extends beyond CCD size from ACTY" 
                 <<  (yextend+acty-1) << ">" << SXI_CCD_SIZE << std::endl;
          AH_THROW_RUNTIME("Illegal YEXTEND value");
        }
        // set the bad pixel status bit
        statusMask[ccd_id][actx-1][acty+j-1] |= statusBit;
        // Update 2015-09-21 HY:
        // Incorrect neighboring bit setting for columns
        //setBitNeighbors(ccd_id, actx-1, acty+j-1, npixnbr, statusBitNeighbor, statusMask);
        for(int k=1; k<=npixnbr; ++k) {
          int x_lo = actx - 1 - k;
          int x_hi = actx - 1 + k;
          if ( x_lo >= 0 ) statusMask[ccd_id][x_lo][acty+j-1] |= statusBitNeighbor;
          if ( x_hi < SXI_CCD_SIZE ) statusMask[ccd_id][x_hi][acty+j-1] |= statusBitNeighbor;
        }
      }
    } 

    // Partial columns:
    // Flag preceding and trailing pixels in a partial column, and their neighbors
    //
    // Loop through the acty column by [0,acty-1] and set the preceding status bit
    if ( acty > 1 ) { // No preceding pixels if acty is at 1
      for (int j=0; j<acty-1; j++) {
        // set the preceding status bit
        statusMask[ccd_id][actx-1][j] |= statusBitPre;
        setBitNeighbors(ccd_id, actx-1, j, npixnbr, statusBitPreNeighbor, statusMask);
      }
    }
    // Extend the acty column by [acty+yextend:SXI_CCD_SIZE] and set the trailing status bit
    if ( acty + yextend <= SXI_CCD_SIZE ) {
      for(int j=acty-1+yextend; j<SXI_CCD_SIZE; j++) {
        // set the trailing status bit
        statusMask[ccd_id][actx-1][j] |= statusBitPre;
        setBitNeighbors(ccd_id, actx-1, j, npixnbr, statusBitPreNeighbor, statusMask);
      }
    }
  }//end loop over bad pixels

}// end maskColumns

// *********************************************************************************************************

void setBitBoundNeighbors(int ccd_id , int ix, int iy, int nboundnbr, int bit_ccd, int bit_seg, std::vector<vec2dLngLng> & statusMask) {

  // CCD boundary
  if (ix == 0 && iy != 0 && iy != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix+j][iy] |= bit_ccd;
    }
  } else if (ix == SXI_CCD_SIZE-1 && iy != 0 && iy != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix-j][iy] |= bit_ccd;
    }
  } else if (iy == 0 && ix != 0 && ix != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix][iy+j] |= bit_ccd;
    }
  } else if (iy == SXI_CCD_SIZE-1  && ix != 0 && ix != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix][iy-j] |= bit_ccd;
    }
  }

  // Segment boundary
  if (ix == SXI_CCD_SIZE/2+1 && iy != 0 && iy != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix+j][iy] |= bit_seg;
    }
  } else if (ix == SXI_CCD_SIZE/2-1 && iy != 0 && iy != SXI_CCD_SIZE-1 ) {
    for (int j=1; j <= nboundnbr; j++) {
      statusMask[ccd_id][ix-j][iy] |= bit_seg;      
    }
  }
}


/** @} */

/* Revision Log
  $Log: sxiflagpix.cxx,v $
  Revision 1.84  2016/11/22 19:19:46  rshill
  Fixed segfault when echoflag=no and echomap=yes (echomap
  should be ignored and effectively 'no' in this case).

  Revision 1.83  2016/08/18 16:32:10  rshill
  Bugfix:  in setBitChar, the second for loop termination expression was
  changed from ii<=LENSTATUS to ii<LENSTATUS.  Was causing l_status array overrun.

  Revision 1.82  2016/08/12 18:30:28  rshill
  Bugfix:  in CR echo detection, each fracmap pixel is now normalized
  before testing normmap pixel against echomin parameter.

  Revision 1.81  2016/08/11 23:10:56  rshill
  Bug fix - was transposing fraction map for output
  in case where it was not allocated.

  Revision 1.80  2016/08/11 22:47:49  rshill
  Optionally (1) use GTI to select events for CR echo detection;
  (2) write CR fraction image in DET as extension of bad pixel image file.

  Revision 1.79  2016/08/10 15:00:42  rshill
  In CR track detection, accept GRADE==0 and NULL events, but skip
  the aimpoint segment.

  Revision 1.78  2016/08/08 19:53:02  rshill
  Changed expression for extracting bits from P_OUTER_MOST column.

  Revision 1.77  2016/08/05 05:22:43  rshill
  Corrected a getParInt() to getParDouble().

  Revision 1.76  2016/08/04 22:32:06  rshill
  Added cosmic ray echo detection.

  Revision 1.75  2016/07/26 18:36:37  rshill
  Corrected setBitChar() to have no effect on bits 23-29
  of the status, consistent with the initialization of the l_status variable.  This avoids
  overwriting bits set by sxiphas and sxipi.

  Revision 1.74  2016/04/06 19:58:20  rshill
  Added STATUS bits section to prologue.

  Revision 1.73  2016/03/24 17:50:30  mdutka
  Address items listed in issue # 610

  Revision 1.72  2016/02/10 19:53:28  mdutka
  Adding check to see if input pixel files are valid

  Revision 1.71  2015/12/29 18:02:20  mwitthoe
  sxiflagpix: throw error if no rows in input event file

  Revision 1.70  2015/11/10 15:54:42  asargent
  Replaced AH_WARN with AH_INFO statements

  Revision 1.69  2015/10/29 13:35:56  mdutka
  updating sxiflagpix after bug caught by eric PHAS was being written incorrectly

  Revision 1.68  2015/09/25 18:48:33  asargent
  Updated PHAS resetting during doWork. Fixed bug during maskColumns where it was incorrectly setting neighboring pixels for columns.

  Revision 1.67  2015/09/15 17:46:04  klrutkow
  added general caldb query for badpix and mask files

  Revision 1.66  2015/08/21 14:24:39  asargent
  Changed long to long long for time local variables for 32 bit machines

  Revision 1.65  2015/08/19 16:21:16  asargent
  Updated bit comments for fits file. Do not print CCDs to output image file if CCD not in DETNAM keyword. Fixed bug in when using yextend from bad pixel file in maskColumns

  Revision 1.64  2015/08/17 23:45:03  asargent
  Bug fixes: changed image writing to long long, fixed yextend boundary, forced LL with hex values. Cleaned up mask file reading. Added +/- 1 day for hot pixels

  Revision 1.63  2015/08/10 13:44:04  mwitthoe
  sxiflagpix: conform to standard header; add parameter stamping to log file; make yes/no lowercase in parameter file; general clean-up

  Revision 1.62  2015/07/30 21:24:24  mdutka
  made change to output logging statements

  Revision 1.61  2015/06/24 20:05:03  mdutka
  adding extra chatter to CALDB search

  Revision 1.60  2015/06/05 19:03:16  mdutka
  bits are flags in documentation now

  Revision 1.59  2015/06/05 18:55:56  mdutka
  decrmenting element in outlist

  Revision 1.58  2015/06/03 17:55:41  mdutka
  adding error for invalid yextend value in badpix

  Revision 1.57  2015/06/03 16:29:08  mdutka
  updatng statusCounter

  Revision 1.56  2015/06/03 16:14:30  mdutka
  fixed hexdecimal issues with CI

  Revision 1.55  2015/06/03 15:46:25  mdutka
  updating help

  Revision 1.54  2015/06/03 15:23:18  mdutka
  removing setting of bit 23

  Revision 1.53  2015/06/03 14:20:36  mdutka
  Reordering status bits after adding bit 24 set in sxiphas

  Revision 1.52  2015/06/02 20:06:10  mdutka
  fixed bug with loading hotpix file

  Revision 1.51  2015/06/02 18:43:45  mdutka
  treating hotpix file the same way as bad pix file

  Revision 1.50  2015/05/29 12:43:02  mdutka
  debugged CALDB query

  Revision 1.49  2015/05/22 15:07:42  mdutka
  Changed EVTTHRES to EVENTTHR

  Revision 1.48  2015/05/20 21:03:05  mdutka
  commiting change from hiroya, chages detailed in trf 04-27

  Revision 1.47  2015/05/12 18:50:43  mdutka
  checking in incremental changes after firt report from eric miller issue #490

  Revision 1.46  2015/04/01 18:40:35  mdutka
  Adding characters LL to the end of hexidecimal constants over 32 bits

  Revision 1.45  2015/03/31 17:17:26  mdutka
  statusMask vectors now hold long longs, corrected initialize of ints, fix to maskareadiscrimination

  Revision 1.43  2015/03/18 15:04:38  mdutka
  sxi tool changes see issue #490

  Revision 1.42  2014/12/30 22:06:24  mdutka
  Updating parameter list see issue #472

  Revision 1.41  2014/12/11 21:16:36  asargent
  Bug fixes: -1 was being written to good elements in output image file. Bug fix is in maskAreaDiscrimination routine. Added constraints to segment and uses the active readnode to determine which coordinates to flag. Sped up routine checkBad3x3 by passing in statusMask by reference. New parameter copyphas to copy phas from telemetry before updating.

  Revision 1.40  2014/11/05 19:40:40  asargent
  Converted STATUS bit FITS writing to write bit values as bit indeces, rather than inversion (i.e. bit 0 is most significant, rather than least). Added new procstatus writing. Added output of bit summary.

  Revision 1.39  2014/10/29 22:16:42  asargent
  Fixed typo and added boolean check to columnRange functions.

  Revision 1.38  2014/10/29 14:17:20  asargent
  Added boundary checks to event loop. Added comments to output files.

  Revision 1.37  2014/10/15 21:19:38  asargent
  Fixed bug where during setBit3x3surround where it was unsetting unneccessary bits. Added global variable SXI_DET_SIZE.

  Revision 1.36  2014/09/30 20:05:27  asargent
  Added option to input NONE for pixel files or mask file

  Revision 1.35  2014/09/28 22:06:45  asargent
  Changed inclusion/exclusion regions to error if a coordinate is -1

  Revision 1.34  2014/09/28 21:26:01  asargent
  New check for area discrimination: if a coordinate is equal to -1, then turn that inclusion/exclusion region off

  Revision 1.33  2014/09/22 17:13:03  asargent
  Changed writing of image to primary extension. See issue 431.

  Revision 1.32  2014/09/15 20:49:18  mwitthoe
  sxiflagpix: add support for extended syntax; issue 179

  Revision 1.31  2014/09/15 14:33:16  mwitthoe
  sxiflagpix: remove instrument argument for procstatus::processRow(); see issue 412

  Revision 1.30  2014/09/12 20:06:22  asargent
  Revamped sxiflagpix tool: statusMask is now stored in ACT coordinates rather than DET.

  Revision 1.29  2014/06/23 13:56:12  asargent
  Updated output bad pixel file to include entire header information from input event file. Changed local variables ccdid, readnode and segment to type char in accordance with changes to FITS file from J-type to B-type.

  Revision 1.28  2014/05/14 20:44:30  asargent
  Updated row writing in output bad pixel file to include writing all of the columns from the input event file.

  Revision 1.27  2014/02/20 19:27:57  asargent
  Converted ahfits image types to current version.

  Revision 1.26  2014/02/18 16:47:09  asargent
  Fixed bug where segReadMap was going out of range during copyAreaDiscrimination

  Revision 1.24  2014/02/12 15:09:43  asargent
  General house cleaning to better match other tools. Cleaned up and reorganized comments and variables in most functions. Fixed possible memory leak issue caused by a return of an unallocated pointer in parseStringInt. Converted par file parameters to a structure. Gave status map same name as output bad pixel list with -map suffix in order to go into desired output directory.

  Revision 1.23  2014/01/31 19:47:56  asargent
  Removed sections that were commented out due to invalid event file.

  Revision 1.22  2014/01/30 21:40:43  asargent
  Changed statusmap to write to separate file rather than writing the status mask to bad pixel list. Fixed an error in copyAreaDiscrimination where segReadMap coordinates were off by one. Changed sciprenbr and scitrailnbr to match TRF ciprenbr and citrailnbr.

  Revision 1.21  2014/01/30 18:12:35  asargent
  Changed parameter file get functions to ones without warnings. Fixed a type where npixnbr was not being passed properly.

  Revision 1.20  2014/01/28 20:08:00  asargent
  Fixed a doxygen comment.

  Revision 1.19  2014/01/27 19:38:45  asargent
  Added in procStatus reading/writing to event file checks

  Revision 1.18  2014/01/27 18:32:37  asargent
  Added doxygen comments

  Revision 1.17  2014/01/17 21:36:54  asargent
  Removed commented out coordinate check.

  Revision 1.16  2014/01/17 21:29:34  asargent
  Added in several comments

  Revision 1.15  2014/01/13 16:03:52  klrutkow
  code review: comments

  Revision 1.14  2014/01/09 21:58:02  asargent
  Added in variable comments and added in new function to create bad pixel list FITS file within tool

  Revision 1.13  2014/01/07 18:01:33  asargent
  Added in image read/write access from ahfits. Fixed passing around of arrays and changed statusMask to type ahfits::Img2dLng.

  Revision 1.12  2014/01/03 20:40:20  asargent
  Updated standard main() to fix flaw where debug mode was checked before startUp is called.

  Revision 1.11  2013/12/19 21:36:29  asargent
  Fixed indexes for areaExc array in copyAreaDiscrim function

  Revision 1.10  2013/12/19 20:43:39  asargent
  Fixed various compilation issues and other changes in accordance with TRF dated 2013-12-18. Added more checks to parseStringInt function. Added in several comments. Rearranged event loop.

  Revision 1.9  2013/12/12 22:14:23  asargent
  Several changes to source code to take care of compilation errors and add in event threshold parameters.

  Revision 1.8  2013/12/02 22:58:13  asargent
  Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

  Revision 1.7  2013/11/26 18:54:41  asargent
  Added bit to char function for TTYPE 1X bit connections, fixed some bit sets, organized router loop a bit more clearly

  Revision 1.6  2013/11/25 15:28:12  asargent
  Fixed bit setting values

  Revision 1.5  2013/11/22 22:40:00  asargent
  dded npixnbr variable to copyMaskInfo function

  Revision 1.4  2013/11/22 14:15:37  asargent
  Added event file loop for writing statuses to outfile, general cleanup and moving of items

  Revision 1.3  2013/11/18 18:06:53  asargent
  Converted bit setting from decimal to hexadecimal, some function changes

  Revision 1.2  2013/11/15 22:28:12  asargent
  Several new functions and modifications to the tool in accordance with the TRF. General cleanup and movement of variables to top of scope.

  Revision 1.1  2013/11/14 21:41:54  asargent
  "Initial version of sxiflagpix.cxx"

*/
