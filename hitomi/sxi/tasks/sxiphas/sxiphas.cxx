/// \file sxiphas.cxx
/// \brief Tool to translate HXI/SGD FFF files to SFF format
/// \author Robert S. Hill
/// \date $Date: 2016/04/06 19:59:06 $
/// \version 1.0

/**

\defgroup tool_sxiphas Combine 3x3 and 5x5 pixel arrays for SXI (sxiphas)
@ingroup mod_sxi_tasks

This task combines the outer 5x5 PH information (16 pixels) contained in the 
PHAS_OUTER5X5 column with the inner 3x3 PH information (9 pixels) contained 
in the PHAS_INNER3x3 column for each event.  The resulting 5x5 (25I) array 
is recorded in the PHASALL column for all events, with null values for 
events without PHAS_OUTER5X5 information

STATUS bits set:

 Bit (zero-based)   Description
     23              3x3 info is present but 5x5 is absent
     24              3x3 is absent

Source files:

  sxiphas.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-30   MSD    Clean-up code

*/

#define AHLABEL tool_sxiphas
#define AHCVSID "$Id: sxiphas.cxx,v 1.39 2016/04/06 19:59:06 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"
#include "ahapp/ahapp.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <cstring>
#include <stdexcept>
#include <sstream>
#include <cstdlib> // used to convert string to long

/** \addtogroup tool_sxiphas
 *   @{
 *   */

const int LENSTATUS = 48;

/// \brief Get parameter values
/// \param[out] infile name of input file (source)
/// \param[out] outfile name of output file (destination)
/// \param[out] colbound string to parse for TNULL, TLMIN, TLMAX
void getPar(std::string & infile, std::string & outfile, std::string & colbound);

/// \brief Open output file and get instrument identifier
/// \param[in] infile name of input file (source)
/// \param[in] outfile name of output file (destination)
/// \param[in] colbound string to parse for TNULL, TLMIN, TLMAX 
/// \param[out] tnull value to use for TNULL
/// \param[out] tlmin value to use for TLMIN
/// \param[out] tlmax value to use for TLMAX
/// \param[out] fp_out pointer to output file (destination)
void initialize(const std::string & infile, const std::string & outfile, 
                const std::string colbound, int & tnull, int & tlmin, 
                int & tlmax, ahfits::FilePtr & fp_out);

/// \brief Call appropriate FFF-to-SFF routine for HXI or SGD
/// \param[in] tnull value to use for TNULL
/// \param[in] tlmin value to use for TLMIN
/// \param[in] tlmax value to use for TLMAX
/// \param[in] fp_out pointer to output file (destination)
void doWork(const int tnull, const int tlmin, const int tlmax, 
            const ahfits::FilePtr fp_out);

/// \brief Close the output file
/// \param[in] fp_out pointer to output file (destination)
void finalize(ahfits::FilePtr & fp_out);

// ----------------------------------------------------------------------------

///\brief sxiphas tool
int main(int argc, char** argv) {

  std::string infile, outfile;
  std::string colbound;
  int tnull = 0;
  int tlmin = 0;
  int tlmax = 0;
  ahfits::FilePtr fp_out = 0;

  int status=ahapp::startUp(argc, argv, TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(infile, outfile, colbound);
      ahapp::writeParametersToLog(); 
      initialize(infile, outfile, colbound, tnull, tlmin, tlmax, fp_out);
      doWork(tnull,tlmin,tlmax,fp_out);
      finalize(fp_out);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile, outfile, colbound);
        initialize(infile, outfile, colbound, tnull, tlmin, tlmax, fp_out);
        doWork(tnull,tlmin,tlmax,fp_out);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fp_out);
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During shutDown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }
  return status;
}

// ----------------------------------------------------------------------------

void getPar(std::string & infile, std::string & outfile, std::string & colbound) {

  infile = ahapp::getParString("infile");
  outfile = ahapp::getParString("outfile");
  colbound = ahapp::getParString("colbound");

}

// ----------------------------------------------------------------------------

void initialize(const std::string & infile, const std::string & outfile, const std::string colbound,
                int & tnull, int & tlmin, int & tlmax, ahfits::FilePtr & fp_out) {

  long long tnull_in = 0;
  long long tnull_out = 0;
  bool hastnull = false;

  std::string extname = "EVENTS";    // Extension name for input/output file
  std::string instrume = "";

  // Create the FITS file
  ahfits::clone(infile, outfile, &fp_out, true);
  if (ahfits::isPrimary(fp_out)) ahfits::move(fp_out, extname);
  ahmission::checkEmptyTable(fp_out,infile);    // throw error if no rows in file

  instrume = getKeyValStr(fp_out, "INSTRUME");
  if (instrume != "SXI") {
    AH_THROW_RUNTIME("INSTRUME in FITS file header should be SXI");
  }

  hastnull = ahfits::columnNull(fp_out, "PHAS_INNER3X3", tnull_in);
  if(!hastnull) {
    AH_THROW_RUNTIME("PHAS_INNER3X3 column lacks TNULL");
  }
  hastnull = ahfits::columnNull(fp_out, "PHAS_OUTER5X5", tnull_out);
  if(!hastnull) {
    AH_THROW_RUNTIME("PHAS_OUTER5X5 column lacks TNULL");
  }

  // Get TNULL, TLMIN and TLMAX values from parameter
  // If not present use default values
  std::istringstream iss(colbound);
  std::string tmp_int = "";
  if(getline(iss, tmp_int, ',')) {
    tnull = std::strtol(tmp_int.c_str(), 0, 10);
    if(tnull < -32768 || tnull > 32767) AH_THROW_RUNTIME("TNULL should be between -32768 and 32767");
  } else {
    tnull = -32768;
  }
  if(getline(iss, tmp_int, ',')) {
    tlmin = std::strtol(tmp_int.c_str(), 0, 10);
    if(tlmin < -32768 || tlmin > 32767) AH_THROW_RUNTIME("TLMIN should be between -32768 and 32767");
  } else {
    tlmin = -32767;
  }
  if(getline(iss, tmp_int, ',')) {
    tlmax = std::strtol(tmp_int.c_str(), 0, 10);
    if(tlmax < -32768 || tlmax > 32767) AH_THROW_RUNTIME("TLMAX should be between -32768 and 32767");
  } else {
    tlmax = 32767;
  }
  AH_DEBUG << "TNULL: " << tnull << " TLMIN: " << tlmin << " TLMAX: " << tlmax << std::endl;

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ----------------------------------------------------------------------------

void doWork(const int tnull, const int tlmin, const int tlmax, const ahfits::FilePtr fp_out) {

  ahfits::Router router(fp_out);

  char l_proc_status[32] = { 0 };
  char l_status[LENSTATUS] = { 0 };

  short phasall[25], phas_outer5x5[16], phas[9], phas_inner3x3[9];
  char null_phasall[25], null_phas[25], null_phas_outer5x5[16], null_phas_inner3x3[16];

  ahfits::IndexType n_p = columnLength(fp_out, "PHAS");
  ahfits::IndexType n_p_all = columnLength(fp_out, "PHASALL");
  ahfits::IndexType n_p_inner3x3 = columnLength(fp_out, "PHAS_INNER3X3");
  ahfits::IndexType n_p_outer5x5 = columnLength(fp_out, "PHAS_OUTER5X5");
  ahfits::IndexType n_proc_status;
  ahfits::IndexType n_status;

  long long nrows = ahfits::getKeyValLLong(fp_out, "NAXIS2");
  long long nevent  = 0;  // total events
  long long nevent1 = 0;  // Total PHAS_OUTER5X5== “5x5 is present” & PHAS==”3x3 is present”
  long long nevent2 = 0;  // Total PHAS_OUTER5X5== “5x5 is present” & PHAS==”3x3 not present”
  long long nevent3 = 0;  // Total PHAS_OUTER5X5== “5x5 not present” & PHAS==”3x3 is present”
  long long nevent4 = 0;  // Total PROC_STATUS bad

  if (n_p != 9) {
    AH_THROW_RUNTIME("Must be 9 elements in PHAS");
  }

  if (n_p_inner3x3 != 9) {
    AH_THROW_RUNTIME("Must be 9 elements in PHAS_INNER3X3");
  }

  if (n_p_outer5x5 != 16) {
    AH_THROW_RUNTIME("Must be 16 elements in PHAS_OUTER5X5");
  }

  if (n_p_all != 25) {
    AH_THROW_RUNTIME("Must be 25 elements in PHASALL");
  }

  setTNull(fp_out,"PHAS",tnull);
  setTNull(fp_out,"PHASALL",tnull);
  setColumnAttribute(fp_out,"PHAS","TLMIN",tlmin,true);
  setColumnAttribute(fp_out,"PHASALL","TLMIN",tlmin,true);
  setColumnAttribute(fp_out,"PHAS","TLMAX",tlmax,true);
  setColumnAttribute(fp_out,"PHASALL","TLMAX",tlmax,true);

  // Get null value for PHAS
  router.connectFixedLengthArray(ahfits::e_WRITEONLY, "PHASALL", phasall, null_phasall);
  router.connectFixedLengthArray(ahfits::e_WRITEONLY, "PHAS", phas, null_phas);
  router.connectFixedLengthArray(ahfits::e_READONLY,  "PHAS_INNER3X3", phas_inner3x3, null_phas_inner3x3);
  router.connectFixedLengthArray(ahfits::e_READONLY,  "PHAS_OUTER5X5", phas_outer5x5, null_phas_outer5x5);
  router.connectBit(ahfits::e_READONLY, "PROC_STATUS", l_proc_status, n_proc_status);
  router.connectBit(ahfits::e_READWRITE, "STATUS", l_status, n_status);

  for (firstRow(fp_out); readOK(fp_out); nextRow(fp_out)) {

    int n_nulls = 0;
    bool present_3x3 = false;
    bool present_outer5x5 = false;

    readRow(fp_out);
    
    //  Assume that TNULL in these columns is assigned in case :
    //      a) telemetry is damaged
    //      b) information is missing
    //      c) bad pixel within the 9 or 16 array.
    //  PHAS_INNER3x3     PHAS_OUTER5x5        PHAS
    //    Good              good                ok   (9+16 pixel value)
    //    Bad               good                bad  (9+16 all pixel null)
    //    Good              bad                 ok   (9+16 only 16 outer set to null)

    // Verify nulls in PHAS_INNER3X3
    for (int i=0; i<9; ++i) {
      if (null_phas_inner3x3[i] == 1) n_nulls++;
    }
    // Set flag '3x3 is not present'
    present_3x3 = ( n_nulls < 9 );

    // Verify nulls in PHAS_OUTER5X5
    int n_nulls_outer5x5 = 0;
    for (int i=0; i<16; ++i) {
      if (null_phas_outer5x5[i] == 1) n_nulls_outer5x5++;
    }
    // If null is found Set flag '5x5 is  not present'
    present_outer5x5 = ( n_nulls_outer5x5 < 16 );

    // Check proc status. If not processing, set all PHAS values to null
    if (!procstatus::processRow(l_proc_status)) {
      for(int ii = 0; ii < 25; ++ii) { 
        phasall[ii] = tnull;
        null_phasall[ii] = 1;
      }
      for(int ii = 0; ii < 9; ++ii) { 
        phas[ii] = tnull;
        null_phas[ii] = 1;
      }
      nevent4++;
      ahfits::writeRow(fp_out);
      AH_INFO(ahlog::HIGH) << "PROC_STATUS false at row: " << ahfits::currentRow(fp_out) << ". Writing nulls to PHAS." << std::endl;
      continue;
    } else if (present_outer5x5 && present_3x3) {
      // copy one-for-one PHAS3x3 to the first 9 elements of PHAS
      for (int j=0; j<=8; j++) {
        phasall[j] = phas_inner3x3[j];
        null_phasall[j] = 0;
        phas[j] = phas_inner3x3[j];
        null_phas[j] = 0;
      }
      // copy one-for-one PHAS_OUTER5X5 to the last 16 elements of PHAS
      for (int j=0; j<=15; j++) {
        phasall[j+9] = phas_outer5x5[j];
        null_phasall[j+9] = 0;
      }
      nevent1++;
    } else if (!present_outer5x5 && present_3x3) {
      // copy one-for-one PHAS3x3 to the first 9 elements of PHAS
      for (int j=0;j<=8;j++) {
        phasall[j] = phas_inner3x3[j];
        null_phasall[j] = 0;
        phas[j] = phas_inner3x3[j];
        null_phas[j] = 0;
      }
      // set the last 16 elements of PHASALL to null
      for (int j=9;j<=24;j++) {
        phasall[j] = tnull;
        null_phasall[j] = 1;
      }
      AH_INFO(ahlog::HIGH) << "3x3 present but 5x5 missing at row: " << ahfits::currentRow(fp_out) 
                           << ". Writing nulls to last 16 elements of PHASALL and Setting STATUS flag 24." << std::endl;
      l_status[23] = 1;
      nevent3++;
    } else if (!present_3x3) {
      // set all elements of PHAS to null
      for (int j=0;j<=24;j++) {
        phasall[j] = tnull;
        null_phasall[j] = 1;
      }
      for (int j=0;j<=8;j++) {
        phas[j] = tnull;
        null_phas[j] = 1;
      }
      // Set status b xxxxxxxx xxxxxxxx xxxxxxxx 1xxxxxxx xxxxxxxx xxxxxxxx
      AH_INFO(ahlog::HIGH) << "3x3 is missing at row: " << ahfits::currentRow(fp_out) << ". Writing nulls to PHAS and PHASALL and setting STATUS flag 25." << std::endl;
      l_status[24] = 1;
      nevent2++;
    } else {
      // Need to revisit.  Error condition undefined in TRF.
      AH_ERR << "Row " << nevent+1 << " of " << nrows << std::endl;
      AH_ERR << "PROC_STATUS says row is good, but 3X3 and 5X5 arrays are null" << std::endl;
      AH_THROW_RUNTIME("Invalid row of SXI event file");
    } // end if procStatus

    nevent++;

    writeRow(fp_out);

  } // end row loop

  AH_INFO(ahlog::HIGH) << "Number of events:        " << nevent << std::endl;
  AH_INFO(ahlog::HIGH) << " good 3x3 and outer 5x5: " << nevent1 << std::endl;
  AH_INFO(ahlog::HIGH) << " good 3x3 only:          " << nevent3 << std::endl;
  AH_INFO(ahlog::HIGH) << " bad 3x3:    " << nevent2 << std::endl;
  AH_INFO(ahlog::HIGH) << " bad PROC_STATUS events:             " << nevent4 << std::endl;

}

// ----------------------------------------------------------------------------

void finalize(ahfits::FilePtr & fp_out) {
  ahfits::close(fp_out);
}

/** @} */

/* Revision Log
 $Log: sxiphas.cxx,v $
 Revision 1.39  2016/04/06 19:59:06  rshill
 Added STATUS bits section to prologue.

 Revision 1.38  2016/03/24 18:36:25  mdutka
 Addressing items in issue #610

 Revision 1.37  2015/12/23 20:23:09  mwitthoe
 sxiphas: throw error if no rows in input file

 Revision 1.36  2015/08/10 14:03:11  mwitthoe
 sxiphas: add standard prologue; stamp parameters to log file; make yes/no lowercase in parameter file; general clean-up

 Revision 1.35  2015/07/30 18:24:26  mdutka
 updating output logging

 Revision 1.34  2015/06/05 19:18:25  mdutka
 setting documention to flag instead of bit

 Revision 1.33  2015/06/03 15:54:05  mdutka
 updating info messages

 Revision 1.32  2015/06/03 15:40:18  mdutka
 changed condition for bit 24

 Revision 1.31  2015/06/03 15:18:42  mdutka
 moving flag down by one

 Revision 1.30  2015/06/03 13:49:12  mdutka
 Removed editing of proc_status row now bits 23 and 24 are set in status

 Revision 1.29  2015/06/01 19:19:33  mdutka
 updated info message for bad proc_status events

 Revision 1.28  2015/05/22 15:22:10  mdutka
 changed status bit 28 to status bit 24

 Revision 1.27  2015/03/18 14:34:56  mdutka
 Update sxiphas status bit column now 48 bits

 Revision 1.26  2014/12/30 20:52:13  asargent
 Updated parameter descriptions in doxygen

 Revision 1.25  2014/12/30 19:08:59  asargent
 Updated parameter descriptions.

 Revision 1.24  2014/12/30 19:06:52  asargent
 Updated parameter descriptions.

 Revision 1.23  2014/12/11 21:12:51  asargent
 Changed PROC_STATUS elements updated.

 Revision 1.22  2014/11/05 19:37:06  asargent
 Changed tnull parameter to colbound to enable options to include tnull, tlmin and tlmax updating for PHAS and PHASALL columns.

 Revision 1.21  2014/10/29 22:09:39  asargent
 Updated default NULL values. Write TNULL keywords to PHAS and PHASALL

 Revision 1.20  2014/09/15 20:51:15  mwitthoe
 sxiphas: add support for extended syntax when cloning input file; issue 179

 Revision 1.19  2014/09/15 14:35:08  mwitthoe
 sxiphas: remove instrument argument for procstatus::processRow(); see issue 412

 Revision 1.18  2014/08/12 01:07:30  asargent
 Fixed ahfits column reading/writing error

 Revision 1.17  2014/08/01 18:15:07  asargent
 Updated sxiphas to include new template for sxi fff

 Revision 1.16  2014/06/23 14:13:48  asargent
 Updated sxiphas to new TRF dated 2014-06-04. Updated name of PHAS, PHAS_OUTER5X5, and PHASALL columns to conform to FFF description in sxi_fffevt_4june2014.txt

 Revision 1.15  2014/02/04 22:39:25  rshill
 Fixed standard main.

 Revision 1.14  2014/01/09 20:55:16  mwitthoe
 sxiphas.cxx: code review: variable declarations

 Revision 1.13  2014/01/09 20:40:18  treichar
 Added code review +++ notes

 Revision 1.12  2013/12/02 22:59:12  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.11  2013/09/26 18:05:52  rshill
 Changed connect calls to connectFixedLengthArray;moved column length check outside main loop.

 Revision 1.10  2013/09/12 15:47:32  mwitthoe
 sxiphas: explicitly move to first binary table to prepare for redefinition of an empty string to represent the primary extension in ahfits (see issue #270)

 Revision 1.9  2013/07/29 14:11:49  rshill
 Clarified some comments.

 Revision 1.8  2013/07/26 21:42:03  rshill
 Got rid of misleading { from a comment.

 Revision 1.7  2013/07/26 21:33:17  rshill
 Corrected doxygen groups.

 Revision 1.6  2013/07/25 23:56:13  rshill
 Expanded doxygen.

 Revision 1.5  2013/07/24 18:56:36  rshill
 Corrected bug in copying phas_outer5x5 to phas5x5 for one case.

 Revision 1.4  2013/07/24 15:53:24  rshill
 Resolved an inconsistency; see comments.

 Revision 1.3  2013/04/23 20:18:50  rshill
 Changed for EM's latest sample input file.

 Revision 1.2  2013/04/16 19:10:29  rshill
 Real code now. Tested with initial sample file from E. Miller.

 Revision 1.1  2013/04/08 19:07:21  rshill
 Initial checkin.

*/
