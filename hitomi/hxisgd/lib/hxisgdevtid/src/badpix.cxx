/// \file badpix.cxx
/// \brief functions to act on the CALDB bad pixel file for HXI & SGD
/// \author Mike Witthoeft
/// \date $Date: 2015/08/13 01:09:07 $

#define AHLABEL hxisgdevtid_badpix
#define AHCVSID "$Id: badpix.cxx,v 1.17 2015/08/13 01:09:07 rshill Exp $"

#include "hxisgdevtid/badpix.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"

#include "hdcal.h"

namespace hxisgdevtid {

namespace badpix {
  
// ---------------------------------------------------------------------------

void loadThreshold(const std::string& filename, const std::string& instrume,
                   const std::string& detnam, const std::string& datamode,
                   ThresholdTable & thresh_table, int& alive_asic) {

  // variables: FITS pointer and keywords
  ahfits::FilePtr fptr;     // ahfits file accessor for FITS file
  std::string chk_detnam;   // store DETNAM keyword of FITS file
  std::string chk_instrume; // store INSTRUME keyword of FITS file

  // variables: FITS file columns
  // setting size of temporary arrays to largest of HXI/SGD sizes
  std::string l_datamode;
  long l_readout_id_rmap[NUM_READOUT_SGD];
  double l_epi_thre[NUM_READOUT_SGD];
  for (unsigned long i=0; i < NUM_READOUT_SGD; i++) {
    l_readout_id_rmap[i]=0;
    l_epi_thre[i]=0;
  }
  int l_alive_asic=0;

  // number of readouts depends on the INSTRUME
  int nreadout=0;
  if (instrume == "HXI1" || instrume == "HXI2")
    nreadout=NUM_READOUT_HXI;
  else if (instrume == "SGD1" || instrume == "SGD2")
    nreadout=NUM_READOUT_SGD;
  else
    AH_THROW_RUNTIME("bad pixel CALDB file must have INSTRUME = HXI1, HXI2, SGD1, or SGD2");

  // open file and loop over all binary tables looking for the correct value of DETNAM
  ahfits::open(filename,"",&fptr);  // opens into primary HDU
  if (ahfits::isPrimary(fptr)) ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);  // go to first binary table if extended syntax not used in the filename
  do {
    chk_detnam=ahfits::getKeyValStr(fptr,"DETNAM");
    if (chk_detnam == detnam) break;
  } while (ahfits::nextHDU(fptr,ahfits::e_BINARY_TBL));
  if (chk_detnam != detnam) {
    AH_THROW_RUNTIME("could not find extension with DETNAM="+detnam+
                     " in file: "+filename);
  }

  // check that INSTRUME keyword has expected value
  chk_instrume=ahfits::getKeyValStr(fptr,"INSTRUME");
  if (chk_instrume != instrume) {
    AH_THROW_RUNTIME("expecting FITS file with INSTRUME="+instrume+
                     "; given file ("+filename+") has INSTRUME="+chk_instrume);
  }

  // must clear() before loading again
  thresh_table.clear();

  // make connections between FITS columns and local variables
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"DATAMODE",l_datamode);
  router.connectFixedLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",l_readout_id_rmap);
  router.connectFixedLengthArray(ahfits::e_READONLY,"EPI_THRE",l_epi_thre);
  router.connectScalar(ahfits::e_READONLY,"ALIVE_ASIC",l_alive_asic);

  // search table for DATAMODE
  long iter = 0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    ++iter;
    if (l_datamode == datamode) break;
  }
  if (l_datamode == datamode) {
    AH_INFO(ahlog::LOW) << "Row number " << iter 
      << " of bad pixel table matches DATAMODE " << datamode << std::endl;
  } else {
    AH_THROW_RUNTIME("no data in bad pixel FITS file matching DATAMODE; looking for DATAMODE="+datamode);
  }

  // store values for matching row
  for (int ii=0; ii < nreadout; ii++) {
    thresh_table.insert(std::make_pair(l_readout_id_rmap[ii],l_epi_thre[ii]));
  }
  alive_asic=l_alive_asic;

  // close FITS file
  ahfits::close(fptr);

  AH_INFO(ahlog::HIGH) << "Loaded thresholds from bad pixel file for INSTRUME=" 
    << instrume << " DETNAM=" << detnam << std::endl;
}
  
// ---------------------------------------------------------------------------

/// \callgraph
void loadActiveChan(const std::string & filename, const std::string & instrume,
                    const std::string & detnam, const std::string & datamode, 
                    ActiveChanTable & activechan_flags) {

  // Flag denoting that the requested DATAMODE is found.
  //
  bool datamode_found = false;

  // Local variables to hold data from the CALDB file.
  //
  bool okay=false;                     // true if correct HDU found in FITS file
  std::string l_datamode;              // DATAMODE.
  int l_readout_id_rmap[13312];        // READOUT_ID_RMAP.
  unsigned char l_active_flag[13312];  // ACTIVE_FLAG.
  ahfits::IndexType l_rid_len=0;       // Length of READOUT_ID_RMAP vector.
  ahfits::IndexType l_act_len=0;       // Length of ACTIVE_FLAG vector.

  ahfits::FilePtr fptr=0;         // Pointer to open file.
  ahfits::Router *router;         // Pointer to router for FITS column data.

  // Clear table structure in memory in case we are reloading.
  //
  activechan_flags.clear();

  // Open the CALDB file.
  //
  AH_DEBUG << "Opening CALDB file = " << filename << std::endl;
  ahfits::open(filename,"",&fptr);
 
  // if the filename included extended syntax, we don't need to search for the 
  // correct exension.  We only need to search if we opened file to primary
  if (ahfits::isPrimary(fptr)) {

    // Locate correct extension by matching DETNAM keyword
    //
    okay=false;                                              // true if correct HDU found
    while (ahfits::nextHDU(fptr, ahfits::e_BINARY_TBL)) {    // loop over HDUs
      if (instrume != ahfits::getKeyValStr(fptr,"INSTRUME")) continue;
      if (detnam == ahfits::getKeyValStr(fptr,"DETNAM")) {
        okay=true;
        break;
      }
    }
    if (!okay) AH_THROW_RUNTIME("HDU with INSTRUME="+instrume+", DETNAM="
     +detnam+" not found in calibration FITS file: "+filename);

  }

  // Set up router.  Cannot be done until file is open.
  //
  router = new ahfits::Router(fptr);

  // Make connections to local variables.
  //
  router->connectScalar(ahfits::e_READONLY,"DATAMODE",l_datamode);
  router->connectFixedLengthArray(ahfits::e_READONLY,"READOUT_ID_RMAP",l_readout_id_rmap);
  router->connectFixedLengthArray(ahfits::e_READONLY,"ACTIVE_FLAG",l_active_flag);

  // Get column length, which depends on HXI or SGD.
  //
  l_rid_len = columnRepeat(fptr, "READOUT_ID_RMAP");
  l_act_len = columnRepeat(fptr, "ACTIVE_FLAG");

  // Check consistency of column lengths.
  //
  if (l_rid_len != l_act_len) {
    AH_THROW_RUNTIME("READOUT_ID_RMAP and ACTIVE_FLAG have different lengths in CALDB file");
  }

  //  Loop through DATAMODE values until the right one is found.
  //
  long iter = 0;
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {

    iter++;
    AH_DEBUG << "Loop iteration " << iter << std::endl;

    ahfits::readRow(fptr);

    AH_DEBUG << "l_readout_id_rmap[0] = " << l_readout_id_rmap[0] << std::endl;
    AH_DEBUG << "l_readout_id_rmap[last] = " << l_readout_id_rmap[l_rid_len-1] << std::endl;

    if (l_datamode == datamode) {
      for (int i=0; i<l_rid_len; ++i) {
        activechan_flags.insert(std::make_pair(l_readout_id_rmap[i], 
                                               static_cast<ActiveChanVal>(l_active_flag[i])));
      }
      datamode_found = true;
      break;
    }
  }

  if (datamode_found) {
    AH_INFO(ahlog::LOW) << "Row number " << iter 
      << " of bad pixel table matches DATAMODE " << datamode << std::endl;
  } else {
    AH_ERR << "DATAMODE not found:  " << datamode << std::endl;
    AH_THROW_RUNTIME("Requested DATAMODE not found in bad pixel CALDB file.");
  }

  // Check that the mapping of READOUT_ID_RMAP to ACTIVE_FLAG is what we expect.
  //
  if ((unsigned int)activechan_flags.size() != l_rid_len ||    // Right number of readouts.
      activechan_flags.begin()->first != 1 ||                  // READOUT_ID_RMAP runs from
      activechan_flags.rbegin()->first != l_rid_len) {         //  1 to the number of readouts.
    AH_THROW_RUNTIME("Bad entry in bad pixel CALDB file.");
  }

  // Deallocate the router.
  //
  delete router;

  // Close the CALDB file.
  //
  ahfits::close(fptr); 

  AH_INFO(ahlog::HIGH) << "Loaded pixel flags from bad pixel file for INSTRUME=" 
    << instrume << " DETNAM=" << detnam << std::endl;

}

// ---------------------------------------------------------------------------

}  // namespace badpix

}  // namespace hxisgdevtid

/* Revision Log
 $Log: badpix.cxx,v $
 Revision 1.17  2015/08/13 01:09:07  rshill
 Additional log output.

 Revision 1.16  2015/08/13 01:01:42  rshill
 Added logging to load functions.

 Revision 1.15  2015/07/15 19:21:38  klrutkow
 removed local resolve function in favor of ahmission caldb resolve

 Revision 1.14  2015/07/14 00:13:08  klrutkow
 fixed bug in loadActiveChan when looking for correct extension

 Revision 1.13  2015/07/13 00:15:02  klrutkow
 added resolve() function to get CALDB filename ; in loadActiveChan, added check if we're in primary extension before moving to next extension ; updated error message if loadActiveChan didn't find correct extension

 Revision 1.12  2015/03/18 19:49:26  mwitthoe
 hxisgdevtid library: change DETNAME to DETNAM

 Revision 1.11  2015/03/16 17:54:35  mwitthoe
 hxisgdevtid library: move load function for the active channel (badpix) CALDB file from the hxisgdpha tool since it is now needed by sgdevtid

 Revision 1.10  2014/12/30 16:29:36  mwitthoe
 hxisgdevtid library: require only a portion of the DATAMODE keyword in the load() function for the badpix CALDB file, e.g. NORMAL1 instead of CAMERA_NORMAL1

 Revision 1.9  2014/12/16 15:44:39  mwitthoe
 hxievtidlib: now search badpix CALDB files using DATAMODE instead of TIME; given an event DATAMODE (e.g. CAMERA_NORMAL1 for HXI), search the DATAMODE column for the last part of the event DATAMODE (e.g. NORMAL1); see issues 466/467

 Revision 1.8  2014/12/10 16:01:24  mwitthoe
 hxievtidlib: load() function for the badpix file will now also return the value of the ALIVE_ASIC column in addition to the EPI thresholds; see issues 466 & 467

 Revision 1.7  2014/09/12 20:58:52  mwitthoe
 hxisgdevtid library: allow extended syntax to be used with the bad pixel CALDB file

 Revision 1.6  2014/08/05 12:59:08  mwitthoe
 hxisgdevtid library - badpix: fix bug where a long value was initialized with a double (0.)

 Revision 1.5  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.4  2014/03/12 20:53:59  mwitthoe
 hxisgdevtid library: include extension name in argument list for load() function for the badpix library; this is needed since the SGD badpixel CALDB file will have 3 extensions: CC1, CC2, and CC3; the old version just loaded the 1st extension

 Revision 1.3  2014/01/17 21:05:43  mwitthoe
 hxisgdevtid library: update badpix library to match corrected versions of CALDB files (included at src/test/input/test_caldb_*fits)

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:02  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
