/** \brief Unit test of ahgen library.
    \author James Peachey
    \date 2012-01-29
*/

#define AHLABEL test_testahgen
#define AHCVSID "$Id: testahgen.cxx,v 1.30 2014/12/16 18:45:04 mwitthoe Exp $"

#include "ahgen/ahgen.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"
#include "ahgen/BitBuf.h"

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <string>
#include <sstream>
#include <stdio.h>
#include <unistd.h>

using namespace ahgen;

void ut_strings(void);
void ut_filepath(void);
void ut_clobber(void);
void ut_BitBuf(void);
void ut_buffer(void);
void ut_random(void);
void ut_history(void);
void ut_refdata(void);

// ---------------------------------------------------------------------------

int main(int argc, char ** argv) {

  // chatter level: 0 -- no output to screen
  //                1 -- no test information
  //                2 -- full test information
  //                3 -- HIGH chatter statements from routines
  int chatter=1;

  // set up logger
  ahlog::setup("testahgen","!DEFAULT",chatter,false);

  ut_strings();
  ut_filepath();
  ut_clobber();
  ut_BitBuf();
  ut_buffer();
  ut_random();
  ut_history();
  ut_refdata();

  int status=ahgen::finalReport();
  ahlog::shutdown();

  return status;
}

// ---------------------------------------------------------------------------

void ut_strings() {

  LABEL_TEST("test string functions");

  START_TEST("test isNumber function") {
    if (ahgen::isNumber("")) FAILTEXT("empty string should not be a number");
    if (ahgen::isNumber("abc")) FAILTEXT("'abc' should not be a number");
    if (ahgen::isNumber("1.1.1")) FAILTEXT("'1.1.1' should not be a number");
    if (ahgen::isNumber("2-2")) FAILTEXT("'2-2' should not be a number");
    if (!ahgen::isNumber("0")) FAILTEXT("'0' should be a number");
    if (!ahgen::isNumber("1")) FAILTEXT("'1' should be a number");
    if (!ahgen::isNumber("1.")) FAILTEXT("'1.' should be a number");
    if (!ahgen::isNumber("3.14")) FAILTEXT("'3.14' should be a number");
    if (!ahgen::isNumber("-2")) FAILTEXT("'-2' should be a number");
    if (!ahgen::isNumber("-8.2")) FAILTEXT("'-8.2' should be a number");
    if (!ahgen::isNumber("1234567890")) FAILTEXT("'1234567890' should be a number");
    if (!ahgen::isNumber("4e6")) FAILTEXT("'4e6' should be a number");
    if (!ahgen::isNumber("4.e3")) FAILTEXT("'4.e3' should be a number");
    if (!ahgen::isNumber("9.9e2")) FAILTEXT("'9.9e2' should be a number");
    if (!ahgen::isNumber("-9.9e2")) FAILTEXT("'-9.9e2' should be a number");
  } END_TEST

}

// ---------------------------------------------------------------------------

/// \callgraph
void ut_filepath() {

  LABEL_TEST("test file path functions");

  START_TEST("check that two files are different") {
    std::string file1="./input/event1.fits";
    std::string file2="./input/event2.fits";
    if (ahgen::filePathsEquivalent(file1,file2)) FAIL;
  } END_TEST

  START_TEST("check that two files are the same") {
    std::string file1="./input/event1.fits";
    std::string file2="./input/event1.fits";
    if (!ahgen::filePathsEquivalent(file1,file2)) FAIL;
  } END_TEST

  START_TEST("check that two files are the same (ignore bang)") {
    std::string file1="./input/event1.fits";
    std::string file2="!./input/event1.fits";
    if (!ahgen::filePathsEquivalent(file1,file2)) FAIL;
  } END_TEST

  START_TEST("check given non-existent file1") {
    std::string file1="./input/event11.fits";
    std::string file2="./input/event2.fits";
    if (ahgen::filePathsEquivalent(file1,file2)) FAIL;
  } END_TEST

  START_TEST("check given non-existent output file") {
    std::string file1="./input/event1.fits";
    std::string file2="./input/event11.fits";
    if (ahgen::filePathsEquivalent(file1,file2)) FAIL;
  } END_TEST

  START_TEST("create symbolic link for testing") {
    std::string file1="./input/event1.fits";
    std::string file2="./input/event1_link.fits";
    if (0 != symlink(file1.c_str(),file2.c_str())) 
      FAILTEXT("could not create symbolic link: event1_link.fits; if it already exists, delete the file before running test code");
  } END_TEST

  START_TEST("identify symbolic link") {
    std::string file1="./input/event1_link.fits";
    if (!ahgen::filePathSymbolic(file1,true)) FAIL;
  } END_TEST

  START_TEST("identify file is not symbolic link") {
    std::string file1="./input/event1.fits";
    if (ahgen::filePathSymbolic(file1,true)) FAIL;
  } END_TEST

  START_TEST("identify file is not symbolic link (ignore bang)") {
    std::string file1="!./input/event1.fits";
    if (ahgen::filePathSymbolic(file1,true)) FAIL;
  } END_TEST

  START_TEST_EXCEPTION("check for failure in filePathSymbolic given non-existent file") {
    std::string file1="./input/event11_link.fits";
    ahgen::filePathSymbolic(file1,true);
  } END_TEST

  START_TEST("Test if file exists") {
    std::string file2="./input/event1.fits";
    if (0 == fileExists(file2.c_str())) FAILTEXT("File does not exist, when it should");
  } END_TEST

  START_TEST("Test if file does not exist") {
    std::string file2="./input/fakeevent.fits";
    if (1 == fileExists(file2.c_str())) FAILTEXT("File exists, when it should not");
  } END_TEST

  START_TEST("removing symbolic link") {
    std::string file2="./input/event1_link.fits";
    if (0 != remove(file2.c_str())) FAILTEXT("error trying to remove output/event1_link.fits; make sure it is gone");
  } END_TEST

}

// ---------------------------------------------------------------------------

/// \callgraph
void ut_clobber() {

  LABEL_TEST("test clobber functions");

  START_TEST("set global clobber") {
    ahgen::setClobber(true);
    if (!ahgen::getClobber()) FAIL;
    ahgen::setClobber(false);
    if (ahgen::getClobber()) FAIL;
  } END_TEST

  START_TEST("get clobber state from filename") {
    std::string fileclob="!test1.dat";
    std::string filenoclob="test2.dat";

    ahgen::setClobber(false);
    if (!ahgen::isFileClobbered(fileclob,ahgen::getClobber())) FAIL;
    if (ahgen::isFileClobbered(filenoclob,ahgen::getClobber())) FAIL;

    ahgen::setClobber(true);
    if (!ahgen::isFileClobbered(fileclob,ahgen::getClobber())) FAIL;
    if (!ahgen::isFileClobbered(filenoclob,ahgen::getClobber())) FAIL;
  } END_TEST

  START_TEST("add/remove bang to filename") {
    std::string fileclob="!test1.dat";
    std::string filenoclob="test2.dat";

    if ("test1.dat" != ahgen::stripBang(fileclob)) FAIL;
    if ("test2.dat" != ahgen::stripBang(filenoclob)) FAIL;
    if ("!test1.dat" != ahgen::addBang(fileclob)) FAIL;
    if ("!test2.dat" != ahgen::addBang(filenoclob)) FAIL;
  } END_TEST

}

// ---------------------------------------------------------------------------

/// \callgraph
void ut_buffer() {

  LABEL_TEST("test setting/getting global buffer state");

  START_TEST("set global buffer") {
    ahgen::setBuffer(-1);
    if (ahgen::getBuffer() != -1) FAIL;
    ahgen::setBuffer(-99);                  // should be reset to -1
    if (ahgen::getBuffer() != -1) FAIL;
    ahgen::setBuffer(0);
    if (ahgen::getBuffer() != 0) FAIL;
    ahgen::setBuffer(100);
    if (ahgen::getBuffer() != 100) FAIL;
  } END_TEST

}

// ---------------------------------------------------------------------------

/// \callgraph
void ut_random() {

  LABEL_TEST("test random number generator access routines");

  START_TEST("set seed to zero") {
    ahgen::seedRandom(0);
    double aval1=-1.;
    aval1=ahgen::getRandom();   // should return between 0 and 1
    if (0. > aval1 && 1. <= aval1) FAILTEXT("did not get a random number in range");
    
    // get two more values
    double aval2=ahgen::getRandom();
    double aval3=ahgen::getRandom();
    ahgen::freeRandom();
    
    // reseed to get different values (0 means to use system time)
    ahgen::seedRandom(0);
    double bval1=ahgen::getRandom();
    double bval2=ahgen::getRandom();
    double bval3=ahgen::getRandom();
    ahgen::freeRandom();
    
    if (aval1 == bval1) FAILTEXT("first values agree");
    if (aval2 == bval2) FAILTEXT("second values agree");
    if (aval3 == bval3) FAILTEXT("third values agree");  
    
  } END_TEST

  START_TEST("seed with positive value") {
    ahgen::seedRandom(1234);
    double aval1=ahgen::getRandom();
    double aval2=ahgen::getRandom();
    double aval3=ahgen::getRandom();
    ahgen::freeRandom();

    // reseed to get same first 3 values
    ahgen::seedRandom(1234);
    double bval1=ahgen::getRandom();
    double bval2=ahgen::getRandom();
    double bval3=ahgen::getRandom();
    ahgen::freeRandom();

    if (aval1 != bval1) FAILTEXT("first values do not agree");
    if (aval2 != bval2) FAILTEXT("second values do not agree");
    if (aval3 != bval3) FAILTEXT("third values do not agree");    
  } END_TEST

  START_TEST("generate random integers") {
    // generate 100 integers and check for range violation
    ahgen::seedRandom(1234);
    int rmin=1;
    int rmax=10;
    for (int i=0; i < 100; i++) {
      int r=ahgen::getRandomInt(rmin,rmax);
      if (r < rmin || r > rmax) FAIL;
    }
    ahgen::freeRandom();
  } END_TEST

  START_TEST_EXCEPTION("try to get random integer with invalid range") {
    ahgen::getRandomInt(10,0);
  } END_TEST

}

// ---------------------------------------------------------------------------

/// \callgraph
void ut_history() {

  LABEL_TEST("test setting/getting global history state");

  START_TEST("set global history") {
    ahgen::setHistory();      // default: true
    if (!ahgen::getHistory()) FAIL;
    ahgen::unsetHistory();
    if (ahgen::getHistory()) FAIL;
    ahgen::setHistory(true);
    if (!ahgen::getHistory()) FAIL;
    ahgen::setHistory(false);
    if (ahgen::getHistory()) FAIL;
  } END_TEST

}

// ---------------------------------------------------------------------------

void ut_refdata(void){
  LABEL_TEST("Check to see if an existing file is found in refdata directory");
   std::string pPathHEADAS;
   pPathHEADAS = getenv("HEADAS");
   START_TEST("Check if an existing file in found using refdata option") { 
     if (ahgen::getRefDataPath("refdata" , "leapsec.fits") 
         != (pPathHEADAS + "/refdata/leapsec.fits")){
       std::cout << "getRefDataPath returned: " << ahgen::getRefDataPath("refdata" , "leapsec.fits") << std::endl;
       std::cout << "and '" + pPathHEADAS + "/refdata/leapsec.fits' was expected" << std::endl;  
       FAIL; 
     }
   }END_TEST 
   LABEL_TEST("pass REFDATA in all caps");
   START_TEST("Check if an existing file in found passing REFDATA (all caps") { 
     if (ahgen::getRefDataPath("REFDATA" , "leapsec.fits") 
         != (pPathHEADAS + "/refdata/leapsec.fits")){
       std::cout << "getRefDataPath returned: " << ahgen::getRefDataPath("refdata" , "leapsec.fits") << std::endl;
       std::cout << "and '" + pPathHEADAS + "/refdata/leapsec.fits' was expected" << std::endl;  
       FAIL; 
     }
   }END_TEST 
   LABEL_TEST("Try passing CALDB string");
   START_TEST("Check behavior for CALDB") {
     if (ahgen::getRefDataPath("CALDB", "mike.fits") != "CALDB"){
       std::cout << "getRefDataPath returned: " << ahgen::getRefDataPath("refdata" , "leapsec.fits") << std::endl;
       std::cout << "and 'CALDB' was expected" << std::endl;
       FAIL;
     }
   }END_TEST
   LABEL_TEST("Test any file path");
   START_TEST("Check behavior for any file path") {
     if (ahgen::getRefDataPath("Any file path" , "leapsec.fits") 
         != "Any file path"){
       std::cout << "getRefDataPath returned: " << ahgen::getRefDataPath("Any file path" , "leapsec.fits") << std::endl;
       std::cout << "and 'Any file path' was expected" << std::endl; 
       FAIL; 
     }
   }END_TEST
}

/* Revision Log
 $Log: testahgen.cxx,v $
 Revision 1.30  2014/12/16 18:45:04  mwitthoe
 ahgen: change the filePathsEquivalent() function so that no errors are thrown if the stat() call fails on either function; in those cases, false is returned (see issue 469; change AH_OUT messages to AH_INFO in ut_BitBuf

 Revision 1.29  2014/09/29 23:32:52  mwitthoe
 ahgen: add function to check if the given string represents a number: isNumber()

 Revision 1.28  2014/09/23 04:00:06  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437

 Revision 1.27  2014/08/14 14:09:45  mwitthoe
 ahgen library test: bug-fix in random number test where generator was not being seeded

 Revision 1.26  2014/07/28 16:17:55  mdutka
 Added additional test for get refdatapath

 Revision 1.25  2014/06/30 21:41:37  mdutka
 Updated ut_refdata

 Revision 1.24  2014/06/27 18:42:15  asargent
 Added new function, fileExists.

 Revision 1.23  2014/06/23 14:38:59  mdutka
 Added function getRefDataPath.

 Revision 1.22  2014/04/03 20:51:35  klrutkow
 added calls to freeRandom()

 Revision 1.21  2014/04/03 20:40:06  klrutkow
 added test that passing seed=0 should be random

 Revision 1.20  2014/03/04 13:13:17  mwitthoe
 ahgen: put in some range checking for getRandomInt

 Revision 1.19  2014/03/04 02:42:20  mwitthoe
 ahgen: add function to generate random integer in range: getRandomInt()

 Revision 1.18  2013/11/27 15:23:56  mwitthoe
 ahgen: implement new random number scheme (Mersenne Twister) as discussed in issues 295 and 267

 Revision 1.17  2013/07/10 18:20:04  mwitthoe
 ahgen: add global history state which will be set by ahapp (from the history standard parameter) and accessed by ahfits for writing parameters to the header of modified FITS files

 Revision 1.16  2013/07/09 18:19:26  mwitthoe
 add two functions to ahgen to get random numbers: seedRandom() ad getRandom(); the underlying RNG is from native C++ and is only temporary; see issue 267

 Revision 1.15  2013/07/01 19:48:29  mwitthoe
 add global buffer state to ahgen for storing the buffer standard parameter

 Revision 1.14  2013/01/23 20:04:32  rshill
 Include ut_BitBuf.

 Revision 1.13  2012/11/02 17:11:49  mwitthoe
 ahgen: add functions to add/remove bang to filename to indicate clobber

 Revision 1.12  2012/10/24 15:31:17  mwitthoe
 add parameter to ahgen::filePathSymbolic() to ignore the case where the given file path does not exist; modified test code to create and remove the test symbolic link since it cannot be saved to the repository

 Revision 1.11  2012/10/23 20:24:21  mwitthoe
 add parameter to ahgen::filePathsEquivalent() allowing second file not to exist

 Revision 1.10  2012/10/23 19:03:06  mwitthoe
 remove instrument stuff from ahgen (now in ahmission); add functions for determining if two file paths refer to the same file and if a file path is a symbolic link

 Revision 1.8  2012/10/18 17:57:25  mwitthoe
 removed tests of standard main functions from testahgen; this functionality is now in ahapp

 Revision 1.6  2012/09/14 23:55:25  mwitthoe
 apply version standards to ahgen

 Revision 1.5  2012/08/29 18:16:18  peachey
 Add unit test for BitBuf struct. Mostly comply with current coding
 and testing standards, including use of current facilities in ahtest.
 Add calls to startUp etc. so that errors are reported correctly.

 Revision 1.4  2012/04/24 15:26:07  mwitthoe
 add instrument enumeration and conversion function to ahgen

 Revision 1.3  2012/04/13 17:30:30  peachey
 Use new ahlog macros (that use st_stream.

 Revision 1.2  2012/02/03 15:30:45  peachey
 Add and test getClobber facility, from parameter file.

 Revision 1.1  2012/01/31 22:23:33  peachey
 Add first version of general support library.

*/
