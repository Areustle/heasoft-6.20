/// \file ut_ahfits_colinfo.cxx
/// \brief unit test for ahfits_colinfo
/// \author Mike Witthoeft
/// \date $Date: 2015/05/18 14:31:08 $
 
#define AHLABEL ahtime_ut_ahfits_colinfo
#define AHCVSID "$Id: ut_ahfits_colinfo.cxx,v 1.22 2015/05/18 14:31:08 asargent Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"


// -----------------------------------------------------------------------------

void ut_colinfo(void) {

  // these values will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test retrieval of column properties");

  START_TEST("open test file") {
    ahfits::clone("./input/event_file_1.fits", "!./output/event_check.fits",&ahffp);
    ahfits::move(ahffp,"EVENTS");
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST("check for existing column") {
      if (!ahfits::haveColumn(ahffp,"TIME")) FAIL;
    } END_TEST

    START_TEST("check for non-existing column") {
      if (ahfits::haveColumn(ahffp,"FAKECOLUMN")) FAIL;
    } END_TEST

    START_TEST("get index of column") {
      if (ahfits::name2Num(ahffp,"TIME") != 26) FAIL;
    } END_TEST

    START_TEST("get column type") {
      if (ahfits::columnType(ahffp,"TIME") != TDOUBLE) FAIL;
    } END_TEST

    START_TEST("get column units") {
      if (ahfits::columnUnits(ahffp,"TIME") != "s") FAIL;
    } END_TEST

    START_TEST("get column repeat") {
      if (ahfits::columnRepeat(ahffp,"PHAS") != 9) FAIL;
    } END_TEST
    
    START_TEST("get variable column repeat") {
      if (ahfits::columnRepeat(ahffp,"ASIC_MOCKUP") != 9) FAIL;
    } END_TEST

    START_TEST("get variable column length") {
      if (ahfits::columnLength(ahffp,"PHAS") != 9) FAIL;
    } END_TEST

    START_TEST("get scalar column length") {
      if (ahfits::columnLength(ahffp,"TIME") != 1) FAIL;
    } END_TEST

    START_TEST("get column width") {
      if (ahfits::columnWidth(ahffp,"PHAS") != 2) FAIL; // 2 byte integer
    } END_TEST

    START_TEST("get column range which is not specified using long long") {
      long long tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"TIME",tmin,tmax);
      if (chk) FAIL;
    } END_TEST

    START_TEST("get column range which is not specified using std::string") {
      std::string tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"TIME",tmin,tmax);
      if (chk) FAIL;
    } END_TEST

    START_TEST("get column range which is not specified using double") {
      double tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"TIME",tmin,tmax);
      if (chk) FAIL;
    } END_TEST

    START_TEST("get column range which is specified using double") {
      double tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"SEGMENT",tmin,tmax);
      if (!chk) FAILTEXT("range reported as not specified, but it should be");
      if (tmin != 0 || tmax != 3) FAILTEXT("incorrect range");
    } END_TEST
  
    START_TEST("get column range which is specified using std::string") {
      std::string tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"SEGMENT",tmin,tmax);
      if (!chk) FAILTEXT("range reported as not specified, but it should be");
      if (tmin != "0" || tmax != "3") FAILTEXT("incorrect range");
    } END_TEST

    START_TEST("get column range which is specified using long long") {
      long long tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"SEGMENT",tmin,tmax);
      if (!chk) FAILTEXT("range reported as not specified, but it should be");
      if (tmin != 0 || tmax != 3) FAILTEXT("incorrect range");
    } END_TEST

    START_TEST("update column range: TLMIN/TLMAX") {
      ahfits::setTLmin(ahffp,"SEGMENT",10);
      ahfits::setTLmax(ahffp,"SEGMENT",20);
      long long tmin,tmax;
      bool chk=ahfits::columnRange(ahffp,"SEGMENT",tmin,tmax);
      if (!chk) FAILTEXT("range reported as not specified, but it should be");
      if (tmin != 10 || tmax != 20) FAILTEXT("incorrect range");
    } END_TEST

    START_TEST("perform column search with no results") {
      ahfits::ColumnList collist;
      std::string srch="ABCDEFGH";
      if (0 != ahfits::searchColumn(ahffp,srch,collist)) FAIL;
    } END_TEST

    START_TEST("perform column search with single result") {
      ahfits::ColumnList collist;
      std::string srch="SEGMENT";
      if (1 != ahfits::searchColumn(ahffp,srch,collist)) FAIL;
      if (srch != collist.front()) FAIL;
    } END_TEST

    START_TEST("perform column search with multiple results") {
      ahfits::ColumnList collist;
      std::string srch="*TIME*";
      if (5 != ahfits::searchColumn(ahffp,srch,collist)) FAIL;
    } END_TEST

  }

  // Close
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_addcolumn(void) {

  ahfits::FilePtr ahffp;

  LABEL_TEST("Test adding column to file");

  START_TEST("clone test file and move to EVENTS HDU") {
    ahfits::clone("input/event_file_1.fits","!output/addcol.fits",&ahffp);
    ahfits::move(ahffp,"EVENTS");
  } END_TEST

  int l_actx=0;
  int actx_chk;
  ahfits::Router router(ahffp);
  START_TEST("connect local variable to ACTX column") {
    router.connectScalar(ahfits::e_READONLY, "ACTX",l_actx);
    ahfits::readRow(ahffp);
  } END_TEST
  actx_chk=l_actx;

  START_TEST_EXCEPTION("try to add existing column, RAWX (as first column)") {
    ahfits::insertColBefore(ahffp,"RAWX","D");
  } END_TEST

  START_TEST_EXCEPTION("try to add existing column, RAWX (as last column)") {
    ahfits::insertColAfter(ahffp,"RAWX","D");
  } END_TEST

  START_TEST_EXCEPTION("try to add column with no name (as first column)") {
    ahfits::insertColBefore(ahffp,"","D");
  } END_TEST

  START_TEST_EXCEPTION("try to add column with no name (as last column)") {
    ahfits::insertColAfter(ahffp,"","D");
  } END_TEST

  START_TEST("add column, TEST1, as first column") {
    ahfits::insertColBefore(ahffp,"TEST1","D");
  } END_TEST

  START_TEST("check ACTX value after inserting TEST1") {
   ahfits::readRow(ahffp);
   if (l_actx != actx_chk) FAIL;
  } END_TEST

  START_TEST("add column, TEST2, before ACTX column") {
    ahfits::insertColBefore(ahffp,"TEST2","D","ACTX");
  } END_TEST

  START_TEST("check ACTX value after inserting TEST2") {
   ahfits::readRow(ahffp);
   if (l_actx != actx_chk) FAIL;
  } END_TEST

  START_TEST("add column, TEST3, as last column") {
    ahfits::insertColAfter(ahffp,"TEST3","D");
  } END_TEST

  START_TEST("check ACTX value after inserting TEST3") {
   ahfits::readRow(ahffp);
   if (l_actx != actx_chk) FAIL;
  } END_TEST

  START_TEST("add column, TEST4, after ACTX column") {
    ahfits::insertColAfter(ahffp,"TEST4","D","ACTX");
  } END_TEST

  START_TEST("set column description") {
    ahfits::setColumnDescription(ahffp,"TEST1","Description for column TEST1");
  } END_TEST

  START_TEST_EXCEPTION("set column description for non-existing column") {
    ahfits::setColumnDescription(ahffp,"TEST5","Description for column TEST5");
  } END_TEST


  ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

void ut_addtnulltunittdisp(void) {

  ahfits::FilePtr ahffp;

  LABEL_TEST("Test adding column to file");

  START_TEST("clone test file and move to types HDU") {
    ahfits::clone("input/types.fits","!output/types_tnull.fits",&ahffp);
    ahfits::move(ahffp,"types");
  } END_TEST

  short l_short=5;
  char l_isnull=0;

  START_TEST_EXCEPTION("try to NULL-connect to column with no TNULL set") {
    ahfits::Router router(ahffp);
    router.connectScalar(ahfits::e_READWRITE,"a_short",l_short,&l_isnull);
  } END_TEST

  START_TEST("add TNULL keyword for column") {
    ahfits::setTNull(ahffp,"a_short",-99);
  } END_TEST

  START_TEST("try to set NULL in column after adding TNULL keyword") {
    ahfits::Router router(ahffp);
    router.connectScalar(ahfits::e_READWRITE,"a_short",l_short,&l_isnull);
    l_isnull=1;
    ahfits::firstRow(ahffp);
    ahfits::writeRow(ahffp);
  } END_TEST

  START_TEST("add unit for column") {
    ahfits::setTUnit(ahffp,"a_short","UNIT");
    if ("UNIT" != ahfits::columnUnits(ahffp,"a_short")) FAIL;
  } END_TEST

  START_TEST("add display format for column") {
    ahfits::setTDisp(ahffp,"a_short","I4.4");
    if ("I4.4" != ahfits::columnDisplay(ahffp,"a_short")) FAIL;
  } END_TEST

  ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

void ut_addtzerotscale(void) {

  ahfits::FilePtr ahffp;

  LABEL_TEST("Test setting TZERO and TSCALE for column");

  START_TEST("clone test file and move to types HDU") {
    ahfits::clone("input/types.fits","!output/types_tzero.fits",&ahffp);
    ahfits::move(ahffp,"types");
  } END_TEST

  START_TEST_EXCEPTION("should not be able to set TZERO for non-integer column") {
    ahfits::setTZero(ahffp,"a_double",123);
  } END_TEST

  START_TEST_EXCEPTION("should not be able to set TSCALE for non-integer column") {
    ahfits::setTScale(ahffp,"a_double",2);
  } END_TEST

  START_TEST("set TZERO for integer type column") {
    ahfits::setTZero(ahffp,"a_long",123);
    std::string key;
    ahfits::formColumnAttribute(ahffp,"a_long","TZERO",key);
    if (ahfits::getKeyValLLong(ahffp,key) != 123) FAIL;
  } END_TEST

  START_TEST("set TSCALE for integer type column") {
    ahfits::setTScale(ahffp,"a_long",2);
    std::string key;
    ahfits::formColumnAttribute(ahffp,"a_long","TSCALE",key);
    if (ahfits::getKeyValLLong(ahffp,key) != 2) FAIL;
  } END_TEST

  ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

void ut_addtnullreloadcol(void) {

    ahfits::FilePtr ahffp;

    LABEL_TEST("Test adding tnull to file and connecting");

    START_TEST("clone test file and move to first binary HDU") {
        ahfits::clone("input/sxi_test.fits","!output/sxi_test_null.fits",&ahffp);
        ahfits::firstHDU(ahffp,ahfits::e_BINARY_TBL);
    } END_TEST    

    START_TEST("add TNULL keyword for column and read nulls") {

        // for this file, column, -999 signifies NULL
        ahfits::setTNull(ahffp,"PHAS_OUTER5X5",-999);
                
        // now check that you can read that TNULL keyword.
        std::string key;
        ahfits::formColumnAttribute(ahffp,"PHAS_OUTER5X5","TNULL",key);
        std::string val = ahfits::getKeyValStr(ahffp,key);
        if (val != "-999") FAILTEXT("Did not correctly read TNULL37 value");
        
        // create the arrays for connecting
        short phas_outer5x5[16];
        char null_phas_outer5x5[16];

        ahfits::Router router(ahffp);
        router.connectFixedLengthArray(ahfits::e_READONLY, "PHAS_OUTER5X5", 
                                       phas_outer5x5, null_phas_outer5x5);
        
        // row 101 (and all of rows 101-200 and 401-500) has the null values
        gotoRow(ahffp, 101);
        readRow(ahffp);
        
        // check that null_phas_outer5x5 has 16 elements
        long nullcount = 0;
        for ( int ii = 0 ; ii<16 ; ii++ ) {
            if (null_phas_outer5x5[ii] == 1) nullcount++;
        }
        if (nullcount != 16) FAILTEXT("Nulls not detected, should be 16");
        
    } END_TEST

    ahfits::close(ahffp);

}

// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ut_ahfits_colinfo.cxx,v $
 Revision 1.22  2015/05/18 14:31:08  asargent
 Added modifyKeyComment and setColumnDescription functions

 Revision 1.21  2014/11/03 21:38:30  mwitthoe
 ahfits: change names of setTlmin() & setTlmax() to setTLmin() & setTLmax to make them consistent with other functions

 Revision 1.20  2014/11/03 21:30:56  mwitthoe
 ahfits: add functions to set TLMIN/TLMAX for a column; refactored functions setting column attributes to reduce code duplication; related to issue 459

 Revision 1.19  2014/11/03 20:48:21  mwitthoe
 ahfits: add functions, setTZero() & setTScale(), in ahfits_colinfo for setting additional column properties; related to issue 459

 Revision 1.18  2014/10/07 14:38:59  mdutka
 TLMIN and TLMAX column attributed are stored as strings, TDMIN/TDMAX are ignored see redmine issue #418 for more details

 Revision 1.17  2014/03/26 18:45:40  mwitthoe
 ahfits library test: finish switching from connect() to connect*(), e.g. connectScalar()

 Revision 1.16  2013/10/15 17:49:01  peachey
 (On behalf of Andy Sargent): correct loadColInfo and columnRepeat
 functions so that they accurately report the maximum repeat
 count of variable-width columns.

 Revision 1.15  2013/09/26 13:49:28  klrutkow
 renamed ut_addtnulltunit() to ut_addtnulltunitdisp(); added a new test ut_addtnullreloadcol() for testing that connect() can read the nulls after creating a new TNULL keyword (issue 293)

 Revision 1.14  2013/09/13 02:14:35  klrutkow
 updated referenes to FilePtr to all use ahffp convention (per issue 270)

 Revision 1.13  2013/09/11 21:04:24  mwitthoe
 ahfits unit tests (colinfo, file, row_nulls): switched over to using new connect* functions, e.g. connectScalar

 Revision 1.12  2013/07/17 17:26:56  mwitthoe
 ahfits: add support for TDISP; refactoring related to code review (issue 266)

 Revision 1.11  2013/07/10 02:11:22  mwitthoe
 ahfits: add function to set the TUNIT value for a column: setTUnit()

 Revision 1.10  2013/07/08 18:25:39  mwitthoe
 ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

 Revision 1.9  2013/03/19 17:48:44  rshill
 Version with column read/write flags.

 Revision 1.8  2013/01/11 19:40:53  mwitthoe
 move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

 Revision 1.7  2013/01/09 22:06:00  mwitthoe
 add insert column functions to ahfits: insertColBefore() and insertColAfter(); also add function, reloadColInfo(), needed to reset loaded column indices after adding a column

 Revision 1.6  2012/12/11 18:52:50  mwitthoe
 add new function, searchColumn(), to ahfits which will search for a column name matching the given search string (containing wildcards)

 Revision 1.5  2012/12/10 19:21:20  mwitthoe
 ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

 Revision 1.4  2012/11/13 18:17:00  mwitthoe
 add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

 Revision 1.3  2012/10/12 22:50:01  mwitthoe
 ahfits: use pass-by-reference in scalar connections; add columnLength() function

 Revision 1.2  2012/10/11 21:23:51  mwitthoe
 add min/max column values to ahfits_colinfo

 Revision 1.1  2012/10/10 20:30:56  mwitthoe
 ahfits: complete overloaded connection functions; add test code


*/

