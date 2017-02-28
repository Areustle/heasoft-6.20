/// \file ut_ahfits_header.cxx
/// \brief unit test for ahfits_header
/// \author Mike Witthoeft
/// \date $Date: 2015/07/29 01:10:12 $
 
#define AHLABEL ahtime_ut_ahfits_header
#define AHCVSID "$Id: ut_ahfits_header.cxx,v 1.16 2015/07/29 01:10:12 mwitthoe Exp $"

#include "ahgen/ahgen.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"


// -----------------------------------------------------------------------------

void ut_read_header(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading header values");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST("check if EXTNAME keyword exists") {
      if (!ahfits::keywordExists(ahffp,"EXTNAME")) FAIL;
    } END_TEST

    START_TEST("check that BLAHBLAH keyword does not exist") {
      if (ahfits::keywordExists(ahffp,"BLAHBLAH")) FAIL;
    } END_TEST

    START_TEST("read string") {
      std::string val=ahfits::getKeyValStr(ahffp,"EXTNAME");
      if (val != "EVENTS") FAIL;
    } END_TEST

    START_TEST("read double") {
      double val=ahfits::getKeyValDbl(ahffp,"DELAY1");
      if (val != 7.9) FAIL;
    } END_TEST

    START_TEST("read long long") {
      long long val=ahfits::getKeyValLLong(ahffp,"NAXIS1");
      if (val != 140) FAIL;
    } END_TEST

    START_TEST("read boolean") {
      bool val=ahfits::getKeyValBool(ahffp,"CLOCKAPP");
      if (val != true) FAIL;
    } END_TEST
  }

  // Close
  ahfits::close(ahffp);
  
  // Test reading primary HDU
  ahffp = 0;

  LABEL_TEST("Test reading primary HDU header values");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "", &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST("check if OBSERVER keyword exists") {
      if (!ahfits::keywordExists(ahffp,"OBSERVER")) FAIL;
    } END_TEST

    START_TEST("check that BLAHBLAHB keyword does not exist") {
      if (ahfits::keywordExists(ahffp,"BLAHBLAHB")) FAIL;
    } END_TEST

    START_TEST("read string") {
      std::string val=ahfits::getKeyValStr(ahffp,"OBSERVER");
      if (val != "MOTOHIDE KOKUBUN") FAIL;
    } END_TEST
  }

  ahfits::close(ahffp);
    
  // test reading from the STDGTI HDU
  ahffp = 0;

  LABEL_TEST("Test reading HDU header values, STDGTI");

  START_TEST("open test file") {
    ahfits::open("./input/event_file_1.fits", "STDGTI", &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST("check that OBSERVER keyword does not exist in in STDGTI") {
      if (ahfits::keywordExists(ahffp,"OBSERVER")) FAIL;
    } END_TEST

    START_TEST("check that BLAHBLAHB keyword does not exist") {
      if (ahfits::keywordExists(ahffp,"BLAHBLAHB")) FAIL;
    } END_TEST

  }

  // Close
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_write_header(void) {

  // these values will be used for all tests
  ahfits::FilePtr ahffp = 0;
  std::string infile="./input/event_file_1.fits";
  std::string outfile="./output/theader.fits";

  LABEL_TEST("Test writing header values");

  START_TEST("clone test file") {
    ahfits::clone(infile,'!'+outfile,&ahffp);
  } END_TEST
  
  // the ahffp should be open to the Primary HDU now

  if (0 != ahffp) {
    START_TEST("check that Primary HDU is not bintable") {
      if (ahfits::isBintable(ahffp)) FAIL;
    } END_TEST

    START_TEST("check that SIMPLE keyword exists in Primary HDU") {
     if (!ahfits::keywordExists(ahffp,"SIMPLE")) FAIL;
    } END_TEST
    
    START_TEST("overwrite keyword, string") {
      ahfits::writeKeyValStr(ahffp,"OBJECT","TESTTEST","TESTCOMMENT");
      if (ahfits::getKeyValStr(ahffp,"OBJECT") != "TESTTEST") FAIL;
    } END_TEST
    
    START_TEST("write new keyword, string") {
      ahfits::writeKeyValStr(ahffp,"NEW_OBJ","TESTTEST","TESTCOMMENT");
      if (ahfits::getKeyValStr(ahffp,"NEW_OBJ") != "TESTTEST") FAIL;
    } END_TEST
    
    START_TEST("write new keyword, empty string") {
      ahfits::writeKeyValStr(ahffp,"EMP_OBJ","","");
      if (!ahfits::keywordExists(ahffp,"EMP_OBJ")) FAIL;
    } END_TEST

    START_TEST("write string") {
      ahfits::writeKeyValStr(ahffp,"OBJECT","TESTTEST","TESTCOMMENT");
      if (ahfits::getKeyValStr(ahffp,"OBJECT") != "TESTTEST") FAIL;
    } END_TEST

    START_TEST("write double") {
      ahfits::writeKeyValDbl(ahffp,"DELAY1",999.999,"TESTCOMMENT");
      if (ahfits::getKeyValDbl(ahffp,"DELAY1") != 999.999) FAIL;
    } END_TEST

    START_TEST("write long long") {
      ahfits::writeKeyValLLong(ahffp,"CODE_ID",999,"TESTCOMMENT");
      if (ahfits::getKeyValLLong(ahffp,"CODE_ID") != 999) FAIL;
    } END_TEST

    START_TEST("write boolean") {
      ahfits::writeKeyValBool(ahffp,"CLOCKAPP",false,"TESTCOMMENT");
      if (ahfits::getKeyValBool(ahffp,"CLOCKAPP") != false) FAIL;
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write history value to header") {
      ahfits::writeKeyHistory(ahffp,"Test writing history value!!!");
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write comment value to header") {
      ahfits::writeKeyComment(ahffp,"Test writing comment value!!!");
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write long history value to header") {
      ahfits::writeKeyHistory(ahffp,"This is a really long history string; it should be wrapped in the FITS files at spaces instead of having words being chopped at an arbitrary location; this feature was requested by Tim Reichard; I only mention his name to make this string longer.");
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write long comment value to header") {
      ahfits::writeKeyComment(ahffp,"This is a really long comment string; it should be wrapped in the FITS files at spaces instead of having words being chopped at an arbitrary location; this feature was requested by Tim Reichard; I only mention his name to make this string longer.");
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write history value with new line characters to header") {
      ahfits::writeKeyHistory(ahffp,"Test3: Line 1\nTest3: Line 2\nTest3: Line3");
    } END_TEST

    // cannot test if written correctly, check by hand
    START_TEST("write comment with new line characters value to header") {
      ahfits::writeKeyComment(ahffp,"Test3: Line 1\nTest3: Line 2\nTest 3: Line3");
    } END_TEST
  }
  
  ahfits::close(ahffp);
  // open the dest file up to the EVENTS HDU
  ahffp = 0;

  START_TEST("open test file") {
    ahfits::open(outfile, "EVENTS", &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST
  
  if (0 != ahffp) {
    START_TEST("check that HDU is bintable") {
      if (!ahfits::isBintable(ahffp)) FAIL;
    } END_TEST

    START_TEST("check that SIMPLE keyword does not exist in EVENTS HDU") {
     if (ahfits::keywordExists(ahffp,"SIMPLE")) FAIL;
    } END_TEST
    
    START_TEST("overwrite keyword, string") {
      ahfits::writeKeyValStr(ahffp,"OBJECT","TESTTEST","TESTCOMMENT");
      if (ahfits::getKeyValStr(ahffp,"OBJECT") != "TESTTEST") FAIL;
    } END_TEST
    
    START_TEST("write new keyword, string") {
      ahfits::writeKeyValStr(ahffp,"NEW_OBJ","TESTTEST","TESTCOMMENT");
      if (ahfits::getKeyValStr(ahffp,"NEW_OBJ") != "TESTTEST") FAIL;
    } END_TEST
    
    START_TEST("write new keyword, empty string") {
      ahfits::writeKeyValStr(ahffp,"EMP_OBJ","","");
      if (!ahfits::keywordExists(ahffp,"EMP_OBJ")) FAIL;
    } END_TEST

    START_TEST("overwrite double") {
      ahfits::writeKeyValDbl(ahffp,"DELAY1",999.999,"TESTCOMMENT");
      if (ahfits::getKeyValDbl(ahffp,"DELAY1") != 999.999) FAIL;
    } END_TEST

    START_TEST("overwrite long long") {
      ahfits::writeKeyValLLong(ahffp,"CODE_ID",999,"TESTCOMMENT");
      if (ahfits::getKeyValLLong(ahffp,"CODE_ID") != 999) FAIL;
    } END_TEST

    START_TEST("overwrite boolean") {
      ahfits::writeKeyValBool(ahffp,"CLOCKAPP",false,"TESTCOMMENT");
      if (ahfits::getKeyValBool(ahffp,"CLOCKAPP") != false) FAIL;
    } END_TEST

    START_TEST("modify keyword description") {
      ahfits::modifyKeyComment(ahffp,"OBJECT","Updated keyword comment");
    } END_TEST

    START_TEST_EXCEPTION("modify keyword description for non-existing keyword") {
      ahfits::modifyKeyComment(ahffp,"MISSING","Updated keyword comment");
    } END_TEST

  }

  START_TEST("write keyword to primary (no other extensions)") {
    ahfits::FilePtr ahffp2 = 0;
    ahfits::create("!output/testkw_primary.fits","",&ahffp2);
    ahfits::writeKeyValLLong(ahffp2,"CODE_ID",999,"TESTCOMMENT");
    ahfits::close(ahffp2);
  } END_TEST

  // Close
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_copy_header(void) {

  // these values will be used for all tests
  ahfits::FilePtr ahffp_src = 0;
  ahfits::FilePtr ahffp_dest = 0;
  std::string insrc="./input/simple.fits";
  std::string indest="./input/types.fits";
  std::string outdest="./output/keycopy.fits";

  LABEL_TEST("Test copying header values");

  START_TEST("open source file") {
    ahfits::open(insrc,"",&ahffp_src);
    ahfits::firstHDU(ahffp_src,ahfits::e_BINARY_TBL);
  } END_TEST

  START_TEST("clone destination file") {
    ahfits::clone(indest,'!'+outdest,&ahffp_dest);
    ahfits::firstHDU(ahffp_dest,ahfits::e_BINARY_TBL);
  } END_TEST
  
  // the ahffp should be open to the Primary HDU now

  if (0 != ahffp_src && 0 != ahffp_dest) {

    START_TEST("copy integer keyword") {
      ahfits::copyKey(ahffp_src,ahffp_dest,"TKEYINT");
    } END_TEST

    START_TEST("copy float keyword") {
      ahfits::copyKey(ahffp_src,ahffp_dest,"TKEYFLT");
    } END_TEST

    START_TEST("copy string keyword") {
      ahfits::copyKey(ahffp_src,ahffp_dest,"TKEYSTR");
    } END_TEST

    START_TEST("copy logical keyword") {
      ahfits::copyKey(ahffp_src,ahffp_dest,"TKEYBOOL");
    } END_TEST

  }
  
  ahfits::close(ahffp_src);
  ahffp_src=0;
  ahfits::close(ahffp_dest);
  ahffp_dest=0;

}

// -----------------------------------------------------------------------------


/* Revision Log
 $Log: ut_ahfits_header.cxx,v $
 Revision 1.16  2015/07/29 01:10:12  mwitthoe
 ahfits: add new function to copy a header keyword from one HDU to another regardless of value type: ahfits::copyKey()

 Revision 1.15  2015/05/18 14:31:08  asargent
 Added modifyKeyComment and setColumnDescription functions

 Revision 1.14  2014/09/22 14:44:01  asargent
 Added new test to write keyword to primary extension with no other extensions in file.

 Revision 1.13  2014/01/29 21:45:51  mwitthoe
 ahfits: allow HISTORY keywords to be automatically split across several lines if too long; see issue 332

 Revision 1.12  2013/12/10 22:08:17  mwitthoe
 ahfits: add functions to copy keywords from one HDU to another, e.g. copyKeyDbl()

 Revision 1.11  2013/10/04 15:39:09  mwitthoe
 ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

 Revision 1.10  2013/09/26 13:19:09  klrutkow
 changed an unnecessary FAILTEXT to FAIL

 Revision 1.9  2013/09/16 20:43:40  klrutkow
 changed clone to end up at primary HDU instead of last HDU

 Revision 1.8  2013/09/16 20:20:19  klrutkow
 added tests for going to the Primary HDU, writing keywords, checking if keyword exists

 Revision 1.7  2013/09/11 20:18:23  mwitthoe
 ahfits library tests: remove reference to empty string, use  in ahfits::open(), use full extension name instead

 Revision 1.6  2013/09/09 17:43:09  mwitthoe
 ahfits library: added function, keywordExists(), to check if a keyword is present in the header; added test case to test function

 Revision 1.5  2012/12/10 19:21:20  mwitthoe
 ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

 Revision 1.4  2012/11/13 18:17:00  mwitthoe
 add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

 Revision 1.3  2012/11/02 19:47:44  mwitthoe
 change units tests for ahfits to use new clone functions which return an opened FITS pointer to the created file

 Revision 1.2  2012/11/01 20:49:38  mwitthoe
 ahfits: clobber is not handled internally in create/clone functions; open() ignores bang in filename; open/create now use strings as arguments

 Revision 1.1  2012/10/10 20:30:56  mwitthoe
 ahfits: complete overloaded connection functions; add test code


*/

