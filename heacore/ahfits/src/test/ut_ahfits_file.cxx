/// \file ut_ahfits_file.cxx
/// \brief unit test for ahfits_file
/// \author Mike Witthoeft
/// \date $Date: 2015/10/09 21:09:22 $
 
#define AHLABEL ahtime_ut_ahfits_file
#define AHCVSID "$Id: ut_ahfits_file.cxx,v 1.29 2015/10/09 21:09:22 mdutka Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#include <sys/types.h>
#include <sys/stat.h>

// ---------------------------------------------------------------------------

void ut_open_close(void) {

  LABEL_TEST("Test opening/closing FITS files");

  START_TEST_EXCEPTION("open file will blank file name") {
    ahfits::FilePtr ahffp;
    ahfits::open("", "EVENTS",&ahffp);
  } END_TEST

  // Try to open a file that isn't there.
  START_TEST_EXCEPTION("open non-existent-file") {
    ahfits::FilePtr ahffp;
    ahfits::open("non-existent.fits", "EVENTS",&ahffp);
  } END_TEST

  START_TEST_EXCEPTION("open existent file, non-existent extension") {
    ahfits::FilePtr ahffp;
    ahfits::open("./input/event_file_1.fits", "SPUD",&ahffp);
  } END_TEST

  ahfits::FilePtr ahffp;
  START_TEST("open existent file with EVENTS extension") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  START_TEST("get string with file and HDU names") {
    std::string chk=ahfits::getFileAndHDUString(ahffp);
    if ("./input/event_file_1.fits[EVENTS]" != chk) FAIL;
  } END_TEST

  START_TEST("close FITS file") {
    ahfits::close(ahffp);
  } END_TEST

  // It should be safe to close an unopened (0) file.
  START_TEST("close file (null pointer)") {
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("open existent but read-only file") {
    const char filename[] = "./input/event_file_1_read_only.fits";
    if (0 != chmod(filename, S_IRUSR | S_IRGRP | S_IROTH))
      FAILTEXT(std::string("chmod failed to make file ") + filename + " read-only");
    ahfits::open(filename, "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
    ahfits::close(ahffp);
  } END_TEST
} 

// ---------------------------------------------------------------------------

void ut_open_extended(void) {

  LABEL_TEST("Test extended syntax");

  START_TEST("open file to primary (no extended syntax)") {
    std::string filename="./input/test.hk";
    ahfits::FilePtr ahffp;
    ahfits::open(filename,"",&ahffp);
    if (ahffp->m_hduidx != 1) FAIL;
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("open file with HDU included in filename") {
    std::string filename="./input/test.hk[HK_DAT2]";
    ahfits::FilePtr ahffp;
    ahfits::open(filename,"",&ahffp);
    if (ahffp->m_hduidx != 3) FAIL;
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("open file with HDU with conflicting extension names") {
    std::string filename="./input/test.hk[HK_DAT2]";
    std::string extname="HK_DAT3";
    ahfits::FilePtr ahffp;
    ahfits::open(filename,extname,&ahffp);
    if (ahffp->m_hduidx != 4) FAIL;
    ahfits::close(ahffp);
  } END_TEST

  START_TEST_EXCEPTION("try to open file with non-existent extension specified") {
    std::string filename="./input/test.hk[HK_DAT5]";
    ahfits::FilePtr ahffp;
    ahfits::open(filename,"",&ahffp);
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("clone file where HDU is specified with extended syntax") {
    std::string infile="./input/event_file_1.fits[EVENTS]";
    std::string outfile="!./output/clone2.fits";
    ahfits::FilePtr ahffp;
    ahfits::clone(infile,outfile,&ahffp,true);
    if (ahffp->m_hduidx != 2) FAIL;
    ahfits::close(ahffp);
  } END_TEST

  START_TEST_EXCEPTION("try to clone file with non-existent extension specified") {
    std::string infile="./input/event_file_1.fits[EVENTS2]";
    std::string outfile="!./output/clone3.fits";
    ahfits::FilePtr ahffp;
    ahfits::clone(infile,outfile,&ahffp,true);
    ahfits::close(ahffp);
  } END_TEST

  // If cloning to self and clobber is turned on then the file is simply
  // opened.  This test checks that we are located in the correct HDU when
  // using extended syntax.
  START_TEST("clone to self with extended syntax") {
    std::string infile="./output/clone2.fits[EVENTS]";
    std::string outfile="!./output/clone2.fits";
    ahfits::FilePtr ahffp;
    ahfits::clone(infile,outfile,&ahffp,true);
    if (ahffp->m_hduidx != 2) FAIL;
    ahfits::close(ahffp);
  } END_TEST


} 

// -----------------------------------------------------------------------------

void ut_create_clone(void) {

  LABEL_TEST("Test creating/cloning FITS files");

  START_TEST_EXCEPTION("create a file in /, which should not work") {
    ahfits::FilePtr ahffp;
    ahfits::create("/testahfits.fits", "input/event_file_1.fits",&ahffp);
  } END_TEST

  START_TEST_EXCEPTION("create a file in ./, using a non-existent template file, which should not work") {
    ahfits::FilePtr ahffp;
    ahfits::create("./output/testahfits.fits", "non-existent.fits",&ahffp);
  } END_TEST

  START_TEST("create a file in ./, using a valid template file, which should work") {
    ahfits::FilePtr ahffp;
    ahfits::create("!./output/testahfits.fits","./input/event_file_1.fits",
                   &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::create, ahffp is null");
    ahfits::close(ahffp);
  } END_TEST

  START_TEST_EXCEPTION("try to clone to self (with clobber=no") {
    ahfits::FilePtr ahffp;
    ahfits::clone("input/event_file_1.fits","input/event_file_1.fits",&ahffp);
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("try to clone to self (with clobber=yes") {
    ahfits::FilePtr ahffp;
    ahfits::clone("input/event_file_1.fits","!input/event_file_1.fits",&ahffp,
                  true);
    ahfits::close(ahffp);
  } END_TEST

  START_TEST("clone file") {
    std::string outfile="!output/event_file_1_clone1.fits";
    ahfits::FilePtr ahffp;
    ahfits::clone("input/event_file_1.fits",outfile,&ahffp);
    ahfits::close(ahffp);
  } END_TEST

}

// -----------------------------------------------------------------------------

void ut_create_ab_initio(void) {

  LABEL_TEST("Test creating brand new FITS files");

  ahfits::FilePtr ahffp;

  START_TEST("create from scratch a file with one BINTABLE") {
    long x=0, y=0;
    double time=0.0;
    ahfits::create("!output/event_file_ab_initio.fits", "", &ahffp);
    ahfits::addEmptyTbl(ahffp, "EVENTS");
    ahfits::insertColBefore(ahffp, "TIME", "1D", "");
    ahfits::insertColAfter(ahffp, "X", "1J", "TIME");
    ahfits::insertColAfter(ahffp, "Y", "1J", "X");
    ahfits::Router r1(ahffp);
    r1.connectScalar(ahfits::e_WRITEONLY, "TIME", time);
    r1.connectScalar(ahfits::e_WRITEONLY, "X", x);
    r1.connectScalar(ahfits::e_WRITEONLY, "Y", y);
    ahfits::nextRow(ahffp);
    time = 0.2; x=100; y=100;
    ahfits::writeRow(ahffp);
    ahfits::nextRow(ahffp);
    time = 0.3; x=400; y=400;
    ahfits::writeRow(ahffp); 
  } END_TEST

  START_TEST("set TLMIN/TLMAX for new columns") {
    std::string key_tlmin_time;
    std::string key_tlmax_time;
    std::string key_tlmin_x;
    std::string key_tlmax_x;

    ahfits::formColumnAttribute(ahffp,"TIME","TLMIN",key_tlmin_time);
    ahfits::formColumnAttribute(ahffp,"TIME","TLMAX",key_tlmax_time);
    ahfits::formColumnAttribute(ahffp,"X","TLMIN",key_tlmin_x);
    ahfits::formColumnAttribute(ahffp,"X","TLMAX",key_tlmax_x);
    ahfits::writeKeyValDbl(ahffp,key_tlmin_time,1.,"");
    ahfits::writeKeyValDbl(ahffp,key_tlmax_time,10.,"");
    ahfits::writeKeyValLLong(ahffp,key_tlmin_x,20,"");
    ahfits::writeKeyValLLong(ahffp,key_tlmax_x,200,"");

    if (1. != ahfits::getKeyValDbl(ahffp,key_tlmin_time)) FAIL;
    if (10. != ahfits::getKeyValDbl(ahffp,key_tlmax_time)) FAIL;
    if (20 != ahfits::getKeyValLLong(ahffp,key_tlmin_x)) FAIL;
    if (200 != ahfits::getKeyValLLong(ahffp,key_tlmax_x)) FAIL;
  } END_TEST

  ahfits::close(ahffp);

}


// -----------------------------------------------------------------------------

void ut_move_HDU_number(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test moving to HDUs by number");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("move to 2nd HDU (1st extension)") {
      ahfits::move(ahffp,2);
      std::string ext=ahfits::getKeyValStr(ahffp,"EXTNAME");
      if (ext != "EVENTS") FAIL;
    } END_TEST

    START_TEST("move to 3rd HDU (2nd extension)") {
      ahfits::move(ahffp,3);
      std::string ext=ahfits::getKeyValStr(ahffp,"EXTNAME");
      if (ext != "STDGTI") FAIL;
    } END_TEST
    
  }

  // Close
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_loop_HDU(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test moving across HDUs");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    std::string ext0;
    START_TEST("get name of 1st extension") {
      ext0=ahfits::getKeyValStr(ahffp,"EXTNAME");
    } END_TEST

    START_TEST("iterate over all HDUs") {
      do {
        continue;
      } while (ahfits::nextHDU(ahffp));
    } END_TEST

    START_TEST("move back to 1st extension") {
      ahfits::move(ahffp,ext0.c_str());
    } END_TEST
  }

  // Close
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_firstHDU(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Try to move to the first HDU of a certain type");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  START_TEST("go to first HDUs of various types") {
    if (!ahfits::firstHDU(ahffp,ahfits::e_PRIMARY_HDU)) FAIL;
    if (!ahfits::isPrimary(ahffp)) FAIL;
    if (!ahfits::firstHDU(ahffp,ahfits::e_BINARY_TBL)) FAIL;
    if (ahfits::isPrimary(ahffp)) FAIL;
    if (ahfits::firstHDU(ahffp,ahfits::e_ASCII_TBL)) FAIL;
    if (!ahfits::firstHDU(ahffp,ahfits::e_IMAGE_HDU)) FAIL;
    if (!ahfits::firstHDU(ahffp,ahfits::e_ANY_HDU)) FAIL;
    if (!ahfits::firstHDU(ahffp,ahfits::e_PRIMARY_HDU)) FAIL;
    if (!ahfits::isPrimary(ahffp)) FAIL;
  } END_TEST
}

// -----------------------------------------------------------------------------

void ut_nextPrimaryHDU(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Try to move to a second Primary extension and fail");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  START_TEST("go to next primary and image HDUs") {
    if (ahfits::nextHDU(ahffp,ahfits::e_PRIMARY_HDU)) FAIL;
    if (ahfits::nextHDU(ahffp,ahfits::e_IMAGE_HDU)) FAIL;
  } END_TEST
}

// -----------------------------------------------------------------------------

void ut_nextBinaryTbl(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Try to move to the next binary HDU");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  START_TEST("go to next binary table") {
    if (!ahfits::nextHDU(ahffp,ahfits::e_BINARY_TBL)) FAIL;
  } END_TEST
}

// -----------------------------------------------------------------------------

void ut_clone1HDU(void) {

    // these pointers will be used for all tests
  std::string src="./input/test.hk";
  std::string dest="./output/clone1.hk";
  std::string other="./input/types.fits";

  std::string extsrc="HK_DAT1";
  std::string extdest="TEST";
  std::string extother="types";

  LABEL_TEST("Clone 1 HDU and create another");

  ahfits::FilePtr fpout=0;
  ahfits::FilePtr fpother=0;
  ahfits::open(other,extother,&fpother);

  START_TEST("clone single HDU to new file") {
    ahfits::cloneSingleHDU(src,extsrc,'!'+dest,fpout);
  } END_TEST

  if (0 == ahgen::getTestStatus()) {
    START_TEST("create new HDU with header from HDU of same file") {
      ahfits::addHDU(fpout,extsrc,fpout,extdest);
    } END_TEST
  }

  if (0 == ahgen::getTestStatus()) {
    START_TEST("create new HDU with header from HDU of different file") {
      ahfits::addHDU(fpother,extother,fpout,extother);
    } END_TEST
  }

  if (0 == ahgen::getTestStatus()) {
    START_TEST_EXCEPTION("try create new HDU with existing HDU name") {
      ahfits::addHDU(fpout,extsrc,fpout,extsrc);
    } END_TEST
  }

  ahfits::close(fpother);
  ahfits::close(fpout);

}

// -----------------------------------------------------------------------------

void ut_insert_delete_rows(void) {

    // these pointers will be used for all tests
  std::string src1="./input/test.hk";
  std::string src2="./output/ins100.fits";
  std::string dest1=(std::string)"!"+src2;
  std::string dest2="!./output/del100.fits";

  std::string extsrc="HK_DAT1";

  LABEL_TEST("Try to append then delete rows");

  ahfits::FilePtr ahffp;
  ahfits::clone(src1,dest1,&ahffp);
  ahfits::move(ahffp,extsrc);

  START_TEST("add and then remove 100 blank rows to end of file") {
    ahfits::addExtraRows(ahffp,100);
    if (ahffp->m_numrow_with_padding-ahffp->m_numrow != 100) FAIL;
    ahfits::removeExtraRows(ahffp);
    if (ahffp->m_numrow_with_padding-ahffp->m_numrow != 0) FAIL;
  } END_TEST
  ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

void ut_removeAllRows(void) {

    // these pointers will be used for all tests
  std::string src="./input/test.hk";
  std::string dest="!./output/empty.fits";

  std::string extsrc="HK_DAT1";

  LABEL_TEST("delete all rows in table");

  ahfits::FilePtr ahffp;
  ahfits::clone(src,dest,&ahffp);
  ahfits::move(ahffp,extsrc);

  START_TEST("Remove rows") {
    ahfits::removeAllRows(ahffp);
    if (ahffp->m_numrow != 0) FAIL;
    if (0 != ahfits::getKeyValLLong(ahffp,"NAXIS2")) FAIL;
  } END_TEST


  ahfits::close(ahffp);

}

/* Revision Log
 $Log: ut_ahfits_file.cxx,v $
 Revision 1.29  2015/10/09 21:09:22  mdutka
 adding new function removeAllRows to ahfits file

 Revision 1.28  2015/09/09 15:16:46  mwitthoe
 ahfits library unit test: add check for correct extension name when testing the overloaded move() function using HDU number

 Revision 1.27  2015/06/29 14:40:34  mwitthoe
 ahfits: add new function (ahfits::move) to allow moving to a FITS extension by number; the new function is an overload of ahfits::move where the argument is the extension name

 Revision 1.26  2015/04/02 15:03:37  mwitthoe
 ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

 Revision 1.25  2014/08/20 20:40:31  mwitthoe
 ahfits: add extended syntax support for ahfits::open() and ahfits::clone(); see issue 179

 Revision 1.24  2014/07/28 18:14:47  mwitthoe
 ahfits: add test for adding TLMIN/TLMAX values for integer and floating type columns: ut_create_ab_initio() in ut_ahfits_file.cxx

 Revision 1.23  2014/03/31 18:07:50  mwitthoe
 ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

 Revision 1.22  2013/12/10 20:32:46  mwitthoe
 ahfits bug fixes: the create() function needed to add necessary keywords to the Primary HDU (now accomplished with fits_create_img); the old version was okay if immediately creating a new table after ahfits::create(), but created an incomplete FITS file if the newly created file was closed before any other action; the addHDU() function did not work if the active HDU was the primary HDU

 Revision 1.21  2013/10/16 01:40:50  mwitthoe
 ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

 Revision 1.20  2013/10/04 15:39:09  mwitthoe
 ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

 Revision 1.19  2013/09/13 02:14:35  klrutkow
 updated referenes to FilePtr to all use ahffp convention (per issue 270)

 Revision 1.18  2013/09/11 21:04:24  mwitthoe
 ahfits unit tests (colinfo, file, row_nulls): switched over to using new connect* functions, e.g. connectScalar

 Revision 1.17  2013/09/11 20:18:23  mwitthoe
 ahfits library tests: remove reference to empty string, use  in ahfits::open(), use full extension name instead

 Revision 1.16  2013/09/10 20:41:32  klrutkow
 added firstHDU function, and tests

 Revision 1.15  2013/09/10 14:10:35  mwitthoe
 ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

 Revision 1.14  2013/07/16 20:10:06  mwitthoe
 ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

 Revision 1.13  2013/07/09 19:06:56  rshill
 Contains working version of creating a BINTABLE from scratch.

 Revision 1.12  2013/07/09 17:41:40  rshill
 Added test for creation of a FITS binary table from scratch.

 Revision 1.11  2013/03/09 04:27:45  peachey
 Correct some errors (unexpected nulls, acces violations)
 revealed by valgrind.

 Revision 1.10  2012/12/10 19:21:20  mwitthoe
 ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

 Revision 1.9  2012/11/13 18:17:00  mwitthoe
 add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

 Revision 1.8  2012/11/04 23:25:20  mwitthoe
 ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

 Revision 1.7  2012/11/02 19:47:44  mwitthoe
 change units tests for ahfits to use new clone functions which return an opened FITS pointer to the created file

 Revision 1.6  2012/11/02 17:13:05  mwitthoe
 ahfits: cleanp up how clobber is handled in open/create/clone cunctions

 Revision 1.5  2012/11/01 20:49:38  mwitthoe
 ahfits: clobber is not handled internally in create/clone functions; open() ignores bang in filename; open/create now use strings as arguments

 Revision 1.4  2012/10/24 17:03:00  mwitthoe
 changed ahfits::addHDU() to work on file names instead of open FITS pointers

 Revision 1.3  2012/10/24 15:05:42  mwitthoe
 expanded test for ahfits::addHDU()

 Revision 1.2  2012/10/24 15:00:07  mwitthoe
 ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

 Revision 1.1  2012/10/10 20:30:56  mwitthoe
 ahfits: complete overloaded connection functions; add test code


*/
