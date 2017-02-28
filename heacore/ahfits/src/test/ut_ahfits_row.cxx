/// \file ut_ahfits_row.cxx
/// \brief unit test for ahfits_row (also tests ahfits_router)
/// \author Mike Witthoeft
/// \date $Date: 2014/12/15 22:28:05 $
 
#define AHLABEL ahtime_ut_ahfits_row
#define AHCVSID "$Id: ut_ahfits_row.cxx,v 1.42 2014/12/15 22:28:05 mdutka Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <iomanip>
#include <cstring>

// -----------------------------------------------------------------------------

void ut_checklast(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Check if at last row in extension");

  START_TEST("open FITS file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST("check at first row") {
      ahfits::firstRow(ahffp);
      if (ahfits::atLastRow(ahffp)) FAIL;
    } END_TEST

    START_TEST("check at last row") {
      ahfits::lastRow(ahffp);
      if (!ahfits::atLastRow(ahffp)) FAIL;
    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

#define MAXEVENT 3u
#define EVENTSIZE 3u
#define ASCII_COL_WIDTH 32u
void ut_read_row(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading from FITS columns");

  START_TEST("open FITS file") {
    ahfits::open("./input/event_file_1.fits", "EVENTS",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("check number of rows") {
      if (ahfits::numRows(ahffp) != 100) FAIL;
    } END_TEST

    START_TEST("read row data into 2 routers") {
      ahfits::Router router1(ahffp);

      int segment1=0;
      router1.connectScalar(ahfits::e_READONLY,"SEGMENT",segment1);

      int rawx=0;
      router1.connectScalar(ahfits::e_READONLY,"RAWX",rawx);

      double exptime=0.;
      router1.connectScalar(ahfits::e_READONLY,"EXPTIME_AETIME",exptime);

      double stime=0.;
      router1.connectScalar(ahfits::e_READONLY,"S_TIME",stime);

      ahfits::Router router2(ahffp);

      int segment2=0;
      router2.connectScalar(ahfits::e_READONLY,"SEGMENT",segment2);

      int rawy=0;
      router2.connectScalar(ahfits::e_READONLY,"RAWY",rawy);

      ahfits::readRow(ahffp);
      if (1 != segment1) FAILTEXT("1st row: router 1: incorrect SEGMENT");
      if (166 != rawx) FAILTEXT("1st row: router 1: incorrect RAWX");
      if (!ahgen::isEqual(2.277025934834E+08,exptime)) FAILTEXT("1st row: router 1: incorrect EXPTIME_AETIME");
      if (1 != segment2) FAILTEXT("1st row: router 2: incorrect SEGMENT");
      if (20 != rawy) FAILTEXT("1st row: router 2: incorrect RAWY");
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      if (1 != segment1) FAILTEXT("2nd row: router 1: incorrect SEGMENT");
      if (193 != rawx) FAILTEXT("2nd row: router 1: incorrect RAWX");
      if (!ahgen::isEqual(2.277025934834E+08,exptime)) FAILTEXT("2nd row: router 1: incorrect EXPTIME_AETIME");
      if (1 != segment2) FAILTEXT("2nd row: router 2: incorrect SEGMENT");
      if (101 != rawy) FAILTEXT("2nd row: router 2: incorrect RAWY");

    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_duplicate(void) {

  typedef char ttype;
  ttype a_byte1, a_byte2;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Try to connect to the same column twice");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST_EXCEPTION("attempt duplicate connection") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte1);
      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte2);

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_router_to_primary(void) {

  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Try (and fail) to create router to primary HDU");

  START_TEST("open FITS file to primary HDU") {
    ahfits::open("./input/types.fits", "",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    START_TEST_EXCEPTION("try to make router") {
      ahfits::Router router1(ahffp);
    } END_TEST
  }

  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_char(void) {

  typedef char ttype;
  ttype a_byte, a_char;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as chars");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as char") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_char",a_char);           // 3

      // check 1st row
      ahfits::readRow(ahffp);
      if (a_byte != 65) FAILTEXT("failed reading TBYTE");
      if (a_char != 65) FAILTEXT("failed reading char");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_bool(void) {

  typedef bool ttype;
  ttype a_bool;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as booleans");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as boolean") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_bool",a_bool);           // 2

      // check 1st row
      ahfits::readRow(ahffp);
      if (a_bool != true) FAILTEXT("failed reading TLOGICAL");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_short(void) {

  typedef short ttype;
  ttype a_byte, a_short, a_float, a_double;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as shorts");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as short") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short);         // 4
      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float);         // 7
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);       // 8

      // check 1st row
      ahfits::readRow(ahffp);
//      std::cout << a_byte << std::endl;
//      std::cout << a_short << std::endl;
//      std::cout << a_float << std::endl;
//      std::cout << a_double << std::endl;
      if (a_byte != 65) FAILTEXT("failed reading TBYTE");
      if (a_short != 1234) FAILTEXT("failed reading TSHORT");
      if (a_float != 1) FAILTEXT("failed reading TFLOAT");
      if (a_double != 1) FAILTEXT("failed reading TDOUBLE");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_long(void) {

  typedef long ttype;
  ttype a_byte, a_short, a_long, a_float, a_double;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as longs");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as long") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short);         // 4
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long);           // 5
      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float);         // 7
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);       // 8

      // check 1st row
      ahfits::readRow(ahffp);
//      std::cout << a_byte << std::endl;
//      std::cout << a_short << std::endl;
//      std::cout << a_long << std::endl;
//      std::cout << a_float << std::endl;
//      std::cout << a_double << std::endl;
      if (a_byte != 65L) FAILTEXT("failed reading TBYTE");
      if (a_short != 1234L) FAILTEXT("failed reading TSHORT");
      if (a_long != 1234567L) FAILTEXT("failed reading TLONG");
      if (a_float != 1L) FAILTEXT("failed reading TFLOAT");
      if (a_double != 1L) FAILTEXT("failed reading TDOUBLE");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_longlong(void) {

  typedef long long ttype;
  ttype a_byte, a_short, a_long, a_longlong, a_float, a_double;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as long longs");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as long long") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short);         // 4
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long);           // 5
      router1.connectScalar(ahfits::e_READONLY,"a_longlong",a_longlong);   // 6
      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float);         // 7
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);       // 8

      // check 1st row
      ahfits::readRow(ahffp);
      if (a_byte != 65LL) FAILTEXT("failed reading TBYTE");
      if (a_short != 1234LL) FAILTEXT("failed reading TSHORT");
      if (a_long != 1234567LL) FAILTEXT("failed reading TLONG");
      if (a_longlong != 123456789012LL) FAILTEXT("failed reading TLONGLONG");
      if (a_float != 1LL) FAILTEXT("failed reading TFLOAT");
      if (a_double != 1LL) FAILTEXT("failed reading TDOUBLE");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_float(void) {

  typedef float ttype;
  ttype a_byte, a_short, a_long, a_longlong, a_float, a_double;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as floats");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as float") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short);         // 4
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long);           // 5
      router1.connectScalar(ahfits::e_READONLY,"a_longlong",a_longlong);   // 6
      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float);         // 7
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);       // 8

      // check 1st row
      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(a_byte,65.)) FAILTEXT("failed reading TBYTE");
      if (!ahgen::isEqual(a_short,1234.)) FAILTEXT("failed reading TSHORT");
      if (!ahgen::isEqual(a_long,1234567.)) FAILTEXT("failed reading TLONG");
      if (!ahgen::isEqual(a_longlong,123456790528.)) FAILTEXT("failed reading TLONGLONG");
      if (!ahgen::isEqual(a_float,1.2345678806304931641)) FAILTEXT("failed reading TFLOAT");
      if (!ahgen::isEqual(a_double,1.2345678806304931641)) FAILTEXT("failed reading TDOUBLE");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_double(void) {

  typedef double ttype;
  ttype a_byte, a_short, a_long, a_longlong, a_float, a_double;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as doubles");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    ahfits::Router router1(ahffp);

    START_TEST("read row data") {
      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte);           // 1
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short);         // 4
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long);           // 5
      router1.connectScalar(ahfits::e_READONLY,"a_longlong",a_longlong);   // 6
      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float);         // 7
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);       // 8

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(a_byte,65.)) FAILTEXT("failed reading TBYTE");
      if (!ahgen::isEqual(a_short,1234.)) FAILTEXT("failed reading TSHORT");
      if (!ahgen::isEqual(a_long,1234567.)) FAILTEXT("failed reading TLONG");
      if (!ahgen::isEqual(a_longlong,123456789012.)) FAILTEXT("failed reading TLONGLONG");
      if (!ahgen::isEqual(a_float,1.2345678806304931641)) FAILTEXT("failed reading TFLOAT");
      if (!ahgen::isEqual(a_double,1.2345678901234566904)) FAILTEXT("failed reading TDOUBLE");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_string(void) {

  typedef std::string ttype;
  ttype a_string;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading row data as strings");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as string") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_string",a_string);       // 9

      // check 1st row
      ahfits::readRow(ahffp);
      if (a_string != "Hello") FAILTEXT("failed reading string");

    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_chars(void) {

  typedef char ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as char");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_bytes",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (dat[0] != 'A') FAIL;
      if (dat[1] != 'B') FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_bools(void) {

  typedef bool ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as bool");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_bools",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (dat[0] != true) FAIL;
      if (dat[6] != false) FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_shorts(void) {

  typedef short ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as short");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_shorts",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (dat[0] != 1000) FAIL;
      if (dat[1] != 2000) FAIL;
    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_longs(void) {

  typedef long ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as long");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_longs",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (dat[0] != 100000) FAIL;
      if (dat[1] != 200000) FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_longlongs(void) {

  typedef long long ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as long long");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_longlongs",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (dat[0] != 10000000000LL) FAIL;
      if (dat[1] != 20000000000LL) FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_floats(void) {

  typedef float ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length column data as float");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_floats",dat);

      // check 1st row
      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(10.,dat[0])) FAIL;
      if (!ahgen::isEqual(20.,dat[1])) FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_doubles(void) {

  typedef double ttype;
  const int maxsize=10;
  // for fixed length array
  ttype dat[maxsize];
  // for variable length array
  ttype dat2[maxsize];
  ahfits::IndexType dat2_count=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading fixed length and variable length column data as double");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types", &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_doubles",dat);
      router1.connectVariableLengthArray(ahfits::e_READONLY,"vv_doubles",dat2,dat2_count);

      // check 1st row
      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(10.,dat[0])) FAIL;
      if (!ahgen::isEqual(20.,dat[1])) FAIL;
      if (dat2_count != 5) FAIL;
      if (!ahgen::isEqual(1.,dat2[0])) FAIL;
      if (!ahgen::isEqual(2.,dat2[1])) FAIL;
    } END_TEST

  }

  // Clean up
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_arr_strings(void) {

  // This tests that trying to connect to an array of strings throws an
  // exception (not supported). No rows are actually read. Note dat variable is
  // not correct for this test case but it does not affect the test behavior.
  typedef char ttype;
  const int maxsize=10;
  ttype dat[maxsize];
  ahfits::IndexType data_count = 0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test connecting to fixed length column data as vector of strings");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST_EXCEPTION("connect to vector of strings") {
      ahfits::Router router1(ahffp);

      // Use connect_generic because there is no connectFixedLengthArray for
      // vectors of strings.
      router1.connect_generic(ahfits::e_READONLY,"v_bytes",dat,&data_count,false,TSTRING,0); 
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_bits(void) {

  typedef char ttype;
  const int maxsize=20; // Make bigger than needed.
  ttype dat[maxsize];
  ahfits::IndexType dat_count=0;

  std::memset(dat, '\0', sizeof(dat));

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading vector bit-fields, both fixed and variable sized, into bytes (chars)");

  START_TEST("open FITS file") {
    ahfits::open("./input/types.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {
    ahfits::Router router1(ahffp);

    // attempt to connect to TBIT column with wrong connect functions
    START_TEST_EXCEPTION("try to connect to TBIT column with connect()") {
      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_bits",dat);
    } END_TEST
    START_TEST_EXCEPTION("try to connect to TBIT column with connectFixedLengthArray()") {
      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_bits",dat);
    } END_TEST
    START_TEST_EXCEPTION("try to connect to TBIT column with connectVariableLengthArray()") {
      router1.connectVariableLengthArray(ahfits::e_READONLY,"v_bits",dat,dat_count);
    } END_TEST

    START_TEST("read fixed-size bit vector") {

      router1.connectBit(ahfits::e_READONLY,"v_bits",dat,dat_count);

      // check all rows, should be all trues (1) everywhere.
      ahfits::IndexType expected_dat_count[] = { 10u, 10u, 10u };
      int rownum = 0;
      ahfits::firstRow(ahffp);
      for (rownum = 0; ahfits::readOK(ahffp); ++rownum) {
        std::memset(dat, '\0', sizeof(dat));
        dat_count = 0u;
        ahfits::readRow(ahffp);
        if (expected_dat_count[rownum] != dat_count) FAILTEXT("failed 1");
        for (ahfits::IndexType ii = 0; ii != dat_count; ++ii) {
          if ('\1' != dat[ii]) FAILTEXT("failed 2");
        }
        ahfits::nextRow(ahffp);
      }
      if (rownum != 3) FAIL;

    } END_TEST

  }

  if (0 != ahffp) {

    START_TEST("read variable-size bit vector") {
      ahfits::Router router1(ahffp);

      router1.connectBit(ahfits::e_READONLY,"vv_bits",dat,dat_count);

      // check all rows, should be all trues (1) everywhere.
      ahfits::IndexType expected_dat_count[] = { 11u, 9u, 10u };
      int rownum = 0;
      ahfits::firstRow(ahffp);
      for (rownum = 0; ahfits::readOK(ahffp); ++rownum) {
        std::memset(dat, '\0', sizeof(dat));
        dat_count = 0u;
        ahfits::readRow(ahffp);
        if (expected_dat_count[rownum] != dat_count) FAIL;
        for (ahfits::IndexType ii = 0; ii != dat_count; ++ii) {
          if ('\1' != dat[ii]) FAIL;
        }
        ahfits::nextRow(ahffp);
      }
      if (rownum != 3) FAIL;

    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);
}


// -----------------------------------------------------------------------------

#define MAXHIT 3u
#define HITSIZE 3u
void ut_write_row(void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test writing FITS columns");

  START_TEST("clone input file and open") {
    ahfits::clone("./input/event_file_1.fits","!./output/event_file_1_out.fits",
                  &ahffp);
    ahfits::move(ahffp,"EVENTS");
  } END_TEST

  if (0 != ahffp) {

    START_TEST ("change value of SEGMENT for first 2 rows") {

      ahfits::Router router1(ahffp);

      int segment=0;
      router1.connectScalar(ahfits::e_WRITEONLY,"SEGMENT",segment);

      ahfits::IndexType asic_count=0;
      double asic_mockup[9];
      std::memset(asic_mockup, '\0', sizeof(asic_mockup));
      asic_mockup[0]=4.5;
      asic_mockup[1]=5.5;
      asic_mockup[2]=5.6;
      asic_mockup[3]=5.7;
      asic_mockup[4]=5.8;
      asic_mockup[5]=5.9;
      asic_mockup[6]=6.5;
      asic_mockup[7]=6.6;
      asic_mockup[8]=6.7;
      asic_count=9;
      router1.connectVariableLengthArray(ahfits::e_WRITEONLY,"ASIC_MOCKUP",asic_mockup,asic_count);

      segment=4;
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
      segment=10;
      asic_mockup[2]=6.5;
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
      segment=20;
      asic_mockup[3]=7.5;
      ahfits::writeRow(ahffp);

    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);


  ahffp = 0;

  LABEL_TEST("Test writing bit vectors (fixed and variable-length) in FITS columns");

  START_TEST("create and open input file") { // Do not clone so that the output file will have no rows.
    ahfits::clone("./input/types.fits","!./output/types_out.fits",&ahffp);
    ahfits::move(ahffp,"types");
  } END_TEST

  if (0 != ahffp) {

    START_TEST ("change value of v_bits and vv_bits for all 3 rows") {

      ahfits::Router router1(ahffp);

      char v_bits[] = { '\1', '\0', '\1', '\0', '\1', '\0', '\1', '\0', '\1', '\0' };
      ahfits::IndexType v_bit_count = sizeof(v_bits)/sizeof(v_bits[0]);

      char vv_bits[] = { '\1', '\0', '\1', '\0', '\1', '\0', '\1', '\0', '\1', '\0', '\1', '\0', '\1' };
      ahfits::IndexType vv_bit_count[] = { 13u, 9u, 11u };
      ahfits::IndexType bit_count = 0u;

      router1.connectBit(ahfits::e_WRITEONLY, "v_bits", v_bits, v_bit_count);
      router1.connectBit(ahfits::e_WRITEONLY, "vv_bits", vv_bits, bit_count);

      for (int ii = 0; ii != 3; ++ii) {
        bit_count = vv_bit_count[ii];
        ahfits::writeRow(ahffp);
        ahfits::nextRow(ahffp);
      }

    } END_TEST
  }
  // Clean up.
  ahfits::close(ahffp);

  ahffp = 0;

}

// -----------------------------------------------------------------------------

void ut_outside_router(void) {
  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test using router outside of HDU loop");

  START_TEST("clone input file and open") {
    ahfits::clone("./input/test.hk","!./output/test.hk.out",&ahffp);
  } END_TEST

  if (0 != ahffp) {

    START_TEST ("in 1st row of each HDU, set TIME=1.0") {

      // go to the first binary HDU (not the Primary, where clone leaves us)
      ahfits::firstHDU(ahffp,ahfits::e_BINARY_TBL);
      ahfits::Router router1(ahffp);

      double time=1.;
      router1.connectScalar(ahfits::e_WRITEONLY,"TIME",time);

      do {
        if (!ahfits::isBintable(ahffp)) continue;
        ahfits::writeRow(ahffp);
      } while (ahfits::nextHDU(ahffp));

    } END_TEST
  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_clear_connections(void) {
  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test clearConnections()");

  START_TEST("open input file") {
    ahfits::open("./input/event_file_1.fits","EVENTS",&ahffp);
  } END_TEST

  if (0 != ahffp) {

    ahfits::Router router1(ahffp);

    START_TEST ("read data from first extension") {

      int l_segment=0;
      router1.connectScalar(ahfits::e_READONLY,"SEGMENT",l_segment);

      ahfits::readRow(ahffp);
      if (1 != l_segment) FAIL;
    } END_TEST

    START_TEST ("read data from second extension") {

      router1.clearConnections();
      ahfits::move(ahffp,"STDGTI");

      double l_start=0;
      router1.connectScalar(ahfits::e_READONLY,"START",l_start);

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(227702588.405962,l_start)) FAIL;
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_check_readonly(void) {
  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("check READONLY state is obeyed");

  START_TEST("open input file") {
    ahfits::open("./input/types.fits","types",&ahffp);
  } END_TEST

  if (0 != ahffp) {

    double a_double=0.;
    double v_doubles[20]={0.};
    double vv_doubles[20]={0.};
    ahfits::IndexType n_vvd=0;

    ahfits::Router router1(ahffp);
    router1.connectScalar(ahfits::e_READONLY,"a_double",a_double);
    router1.connectFixedLengthArray(ahfits::e_READONLY,"v_doubles",v_doubles);
    router1.connectVariableLengthArray(ahfits::e_READONLY,"vv_doubles",vv_doubles,n_vvd);

    START_TEST ("read first row doubles; change values; try to write") {

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(1.23456789012346,a_double)) FAILTEXT("unexpected value for a_double");
      if (!ahgen::isEqual(10.,v_doubles[0])) FAILTEXT("unexpected value for v_doubles[0]");
      if (!ahgen::isEqual(1.,vv_doubles[0])) FAILTEXT("unexpected value for vv_doubles[0]");

      a_double=2.0;
      v_doubles[0]=2.0;
      vv_doubles[0]=2.0;
      ahfits::writeRow(ahffp);

    } END_TEST

    START_TEST("close and re-open input file") {
      ahfits::close(ahffp);
      ahfits::open("./input/types.fits","types",&ahffp);
    } END_TEST

    ahfits::Router router2(ahffp);
    router2.connectScalar(ahfits::e_READONLY,"a_double",a_double);
    router2.connectFixedLengthArray(ahfits::e_READONLY,"v_doubles",v_doubles);
    router2.connectVariableLengthArray(ahfits::e_READONLY,"vv_doubles",vv_doubles,n_vvd);

    START_TEST ("read data first row data again; should not change") {

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(1.23456789012346,a_double)) FAILTEXT("unexpected value for a_double");
      if (!ahgen::isEqual(10.,v_doubles[0])) FAILTEXT("unexpected value for v_doubles[0]");
      if (!ahgen::isEqual(1.,vv_doubles[0])) FAILTEXT("unexpected value for vv_doubles[0]");

    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_check_writeonly(void) {
  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("check WRITEONLY state is obeyed");

  START_TEST("open input file") {
    ahfits::clone("./input/types.fits","!./output/types.wo.out",&ahffp);
    ahfits::move(ahffp, "types");
  } END_TEST

  if (0 != ahffp) {

    double a_double=0.;
    double v_doubles[20]={0.};
    double vv_doubles[20]={0.};
    ahfits::IndexType n_vvd=0;

    ahfits::Router router1(ahffp);
    router1.connectScalar(ahfits::e_WRITEONLY,"a_double",a_double);
    router1.connectFixedLengthArray(ahfits::e_WRITEONLY,"v_doubles",v_doubles);
    router1.connectVariableLengthArray(ahfits::e_WRITEONLY,"vv_doubles",vv_doubles,n_vvd);

    START_TEST ("read first row doubles; change values; try to write") {

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(0.,a_double)) FAILTEXT("unexpected value for a_double");
      if (!ahgen::isEqual(0.,v_doubles[0])) FAILTEXT("unexpected value for v_doubles[0]");
      if (!ahgen::isEqual(0.,vv_doubles[0])) FAILTEXT("unexpected value for vv_doubles[0]");

      a_double=2.0;
      v_doubles[0]=2.0;
      vv_doubles[0]=2.0;
      ahfits::writeRow(ahffp);

    } END_TEST

    START_TEST("close and re-open input file") {
      ahfits::close(ahffp);
      ahfits::open("./output/types.wo.out","types",&ahffp);
    } END_TEST

    ahfits::Router router2(ahffp);
    router2.connectScalar(ahfits::e_READONLY,"a_double",a_double);
    router2.connectFixedLengthArray(ahfits::e_READONLY,"v_doubles",v_doubles);
    router2.connectVariableLengthArray(ahfits::e_READONLY,"vv_doubles",vv_doubles,n_vvd);

    START_TEST ("read data first row data again; should not change") {

      ahfits::readRow(ahffp);
      if (!ahgen::isEqual(2.,a_double)) FAILTEXT("unexpected value for a_double");
      if (!ahgen::isEqual(2.,v_doubles[0])) FAILTEXT("unexpected value for v_doubles[0]");
      if (!ahgen::isEqual(2.,vv_doubles[0])) FAILTEXT("unexpected value for vv_doubles[0]");

    } END_TEST


  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_writemiddle_buffer(void) {
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("with WRITEONLY, write middle row with buffering enabled");

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(-1);

  START_TEST("open input file") {
    ahfits::clone("./input/simple.fits","!./output/simple.buffer.out",&ahffp);
    ahfits::move(ahffp,"TEST");
  } END_TEST

  if (0 != ahffp) {

    int a_int=0;
    float a_float=0.;

    ahfits::Router router1(ahffp);
    router1.connectScalar(ahfits::e_WRITEONLY,"a_int",a_int);
    router1.connectScalar(ahfits::e_WRITEONLY,"a_float",a_float);

    START_TEST ("goto row=5 and change value") {
      a_int=99;
      a_float=99.;
      ahfits::gotoRow(ahffp,5);
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::writeRow(ahffp);
    } END_TEST

  }

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_writemiddle_nobuffer(void) {
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("with WRITEONLY, write middle row with buffering disabled");

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(0);

  START_TEST("open input file") {
    ahfits::clone("./input/simple.fits","!./output/simple.nobuffer.out",&ahffp);
    ahfits::move(ahffp,"TEST");
  } END_TEST

  if (0 != ahffp) {

    int a_int=0;
    float a_float=0.;

    ahfits::Router router1(ahffp);
    router1.connectScalar(ahfits::e_WRITEONLY,"a_int",a_int);
    router1.connectScalar(ahfits::e_WRITEONLY,"a_float",a_float);

    START_TEST ("goto row=5 and change value") {
      a_int=99;
      a_float=99.;
      ahfits::gotoRow(ahffp,5);
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::writeRow(ahffp);
    } END_TEST

  }

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_connect_crosstype_nulls() {

  LABEL_TEST("Test open FITS file and connect crosstype data with null flags set");

  // create output file (clone, but close and reopen file)
  ahfits::FilePtr ahffp;
  std::string infile="./input/types_with_nulls.fits";
  std::string outfile="!./output/types_with_nulls2.fits";
  std::string extname="types";
  START_TEST("copy and open file") {
    ahfits::clone(infile,outfile,&ahffp);
    ahfits::move(ahffp,extname);
  } END_TEST

  std::string colname_long="a_long";
  std::string colname_float="a_float";
 
  long integerVar=0;                     // store column values
  float floatVar=0.;
  char outnullint = 0;
  char outnullfloat = 0;
  ahfits::Router router(ahffp);   

  START_TEST_EXCEPTION("connect local variables to columns for writing with wrong variable types") {
    router.connectScalar(ahfits::e_WRITEONLY,colname_long,floatVar,&outnullfloat);
    router.connectScalar(ahfits::e_WRITEONLY,colname_float,integerVar,&outnullint);
  } END_TEST
  

}


// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ut_ahfits_row.cxx,v $
 Revision 1.42  2014/12/15 22:28:05  mdutka
 updated exception for issue #341 to occur as soon as the colmuns are connected

 Revision 1.41  2014/11/26 15:12:45  mwitthoe
 ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

 Revision 1.40  2014/08/04 15:36:19  peachey
 Initialize ints with an int expression to silence compiler warning.

 Revision 1.39  2014/03/26 18:45:40  mwitthoe
 ahfits library test: finish switching from connect() to connect*(), e.g. connectScalar()

 Revision 1.38  2014/01/07 21:42:17  mwitthoe
 ahfits: added test cases to ut_ahfits_row.cxx to check for invalid connects to TBIT columns; see issue 256

 Revision 1.37  2013/10/22 19:23:31  mwitthoe
 from Andy: ahfits: add function numRows() to ahfits_row; add test for new function in ut_ahfits_row.cxx

 Revision 1.36  2013/10/16 01:40:50  mwitthoe
 ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

 Revision 1.35  2013/10/16 01:14:31  mwitthoe
 ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

 Revision 1.34  2013/10/10 17:05:50  peachey
 Internal refactoring of connect_generic and functions that call it.
 Connections to scalar/vector client data are now indicated by a new
 argument (bool scalar) rather than keying off the sign of the out_type
 variable. Changed all functions that call connect_generic to use the
 new signature.

 Added a test to confirm connecting to vectors of strings leads to
 an exception being thrown. Moved the check for this case from the
 Connection class into the connect_generic function.

 Revision 1.33  2013/09/18 22:01:15  klrutkow
 added a call to move() after clone() in ut_check_writeonly(), to get to correct HDU for test

 Revision 1.32  2013/09/16 20:43:40  klrutkow
 changed clone to end up at primary HDU instead of last HDU

 Revision 1.31  2013/09/11 21:17:19  klrutkow
 updated rest of calls to connect() to use the new functions

 Revision 1.30  2013/09/11 20:01:33  klrutkow
 finished creating new overloaded connect functions.  now we have
 connectScalar(), connectFixedLengthArray(), and connectVariableLengthArray(),
 all of which are overloaded to accept different data types (float, double, etc)
 and to use the same functions for the null flag (dnull_flag is now default set
 to NULL). Also, added \param tags that were missing.  Changed the ut to call
 the new functions rather than the old connect() functions.

 Revision 1.29  2013/09/09 18:52:22  mwitthoe
 ahfits library: preliminary addition of new connect functions (non-null double scalar and array)

 Revision 1.28  2013/07/20 04:11:08  mwitthoe
 add tests to check behavior of WRITEONLY with buffering enabled and disabled

 Revision 1.27  2013/07/16 20:10:06  mwitthoe
 ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

 Revision 1.26  2013/06/20 21:10:14  mwitthoe
 fix bug in ahfits where READ/WRITEONLY state was being ignore for variable-length FITS columns; add test cases for each in ut_ahfits_row

 Revision 1.25  2013/06/17 15:54:34  mwitthoe
 ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

 Revision 1.24  2013/05/23 19:58:47  mwitthoe
 restore buffered version for ahfits_row.cxx and ut_ahfits_row.cxx, but keep it untagged for now

 Revision 1.23  2013/05/23 19:33:59  mwitthoe
 fix ahfits::LastRow() bug; change unit test for LastRow() to fail if using old, buggy version of function

 Revision 1.22  2013/05/23 19:08:49  mwitthoe
 revert ahfits_row.cxx and ut_ahfits_row.cxx to versions 1.25 and 1.15; some buffer stuff was inadvertantly tagged

 Revision 1.15  2013/04/10 19:56:19  mwitthoe
 add new function, atLastRow(), to ahfits_row which will return true if currently at last row in active extension of a FITS file

 Revision 1.14  2013/03/19 17:48:44  rshill
 Version with column read/write flags.

 Revision 1.13  2013/02/27 21:05:27  peachey
 Initial version of test code shows inexplicable behavior. On OS X,
 the first time a file is opened, fits_get_rowsize returns 802 rows.
 As additional clones of that file are opened, fits_get_rowsize returns
 7020, more than 10x greater? Would expect either the same value or a
 smaller value as the number of open files increases. This does not show
 up if one calls cfitsio directly in a similar fashion.

 Revision 1.12  2013/01/18 18:26:19  mwitthoe
 revert ahfits connections to string variables to the stable, but-leaky version; this problem will be sorted out in build 3

 Revision 1.11  2012/12/28 20:51:15  peachey
 Add and test support for bit-field type columns (type X).

 Revision 1.10  2012/12/26 15:56:56  mwitthoe
 ahfits: add insert and delete row functions needed for ahgti

 Revision 1.9  2012/12/10 19:21:20  mwitthoe
 ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

 Revision 1.8  2012/11/13 18:17:00  mwitthoe
 add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

 Revision 1.7  2012/11/02 19:47:44  mwitthoe
 change units tests for ahfits to use new clone functions which return an opened FITS pointer to the created file

 Revision 1.6  2012/11/01 20:49:38  mwitthoe
 ahfits: clobber is not handled internally in create/clone functions; open() ignores bang in filename; open/create now use strings as arguments

 Revision 1.5  2012/10/22 17:31:12  mwitthoe
 add 'L' or 'LL' to long and long long constants for comparisons in ut_ahfits_row.cxx

 Revision 1.4  2012/10/22 17:20:54  mwitthoe
 fix in ahfits to allow reading/writing of variable-length columns

 Revision 1.3  2012/10/18 14:38:19  mwitthoe
 fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

 Revision 1.2  2012/10/12 22:50:01  mwitthoe
 ahfits: use pass-by-reference in scalar connections; add columnLength() function

 Revision 1.1  2012/10/10 20:30:56  mwitthoe
 ahfits: complete overloaded connection functions; add test code


*/
