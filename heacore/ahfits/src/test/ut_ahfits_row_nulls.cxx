/// \file ut_ahfits_row_nulls.cxx
/// \brief unit test for ahfits_row with undefined value capability
/// \author Robert S. Hill
/// \date $Date: 2013/09/11 21:04:24 $
 
#define AHLABEL ahtime_ut_ahfits_row_nulls
#define AHCVSID "$Id:"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <iomanip>
#include <cstring>

// -----------------------------------------------------------------------------

#define MAXEVENT 3u
#define EVENTSIZE 3u
#define ASCII_COL_WIDTH 32u

// -----------------------------------------------------------------------------

void ut_read_byte_null(void) {

  typedef char ttype;
  ttype a_byte;

  char dnull_flag_1=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test detection of TNULL for char");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as char") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte,&dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
       AH_INFO(3) << (int)a_byte << "  " << (int)dnull_flag_1 << std::endl;
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      AH_INFO(3) << (int)a_byte << "  " << (int)dnull_flag_1 << std::endl;
      if (dnull_flag_1 != 1) FAILTEXT("failed detecting TNULL for byte");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_short_null(void) {

  typedef short ttype;
  ttype a_byte, a_short;

  char dnull_flag_1=0, dnull_flag_2=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test detection of TNULL for short");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types", &ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as short") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte,&dnull_flag_1);
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short,&dnull_flag_2);

      // check 2nd row
      ahfits::readRow(ahffp);
      // AH_INFO(3) << (int)a_byte << "  " << (int)dnull_flag_1 << std::endl;
      // AH_INFO(3) << a_short << "  " << (int)dnull_flag_2 << std::endl;
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      // AH_INFO(3) << (int)a_byte << "  " << (int)dnull_flag_1 << std::endl;
      // AH_INFO(3) << a_short << "  " << (int)dnull_flag_2 << std::endl;
      if (dnull_flag_1 != 1) FAILTEXT("failed detecting TNULL for TBYTE as short");
      if (dnull_flag_2 != 1) FAILTEXT("failed TNULL for TSHORT");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_long_null(void) {

  typedef long ttype;
  ttype a_byte, a_short, a_long;

  char dnull_flag_1=0, dnull_flag_2=0, dnull_flag_3=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test detection of TNULL for long");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as long") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte,&dnull_flag_1);
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short,&dnull_flag_2);
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long,&dnull_flag_3);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      if (dnull_flag_1 != 1) FAILTEXT("failed detecting TNULL for TBYTE as long");
      if (dnull_flag_2 != 1) FAILTEXT("failed detecting TNULL for TSHORT as long ");
      if (dnull_flag_3 != 1) FAILTEXT("failed detecting TNULL for TLONG");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_longlong_null(void) {

  typedef long long ttype;
  ttype a_byte, a_short, a_long, a_longlong;

  char dnull_flag_1=0, dnull_flag_2=0, dnull_flag_3=0, dnull_flag_4=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test detection of TNULL for long long");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as long long") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_byte",a_byte,&dnull_flag_1);
      router1.connectScalar(ahfits::e_READONLY,"a_short",a_short,&dnull_flag_2);
      router1.connectScalar(ahfits::e_READONLY,"a_long",a_long,&dnull_flag_3);
      router1.connectScalar(ahfits::e_READONLY,"a_longlong",a_longlong,&dnull_flag_4);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      if (dnull_flag_1 != 1) FAILTEXT("failed detecting TNULL for TBYTE as long long");
      if (dnull_flag_2 != 1) FAILTEXT("failed detecting TNULL for TSHORT as long long");
      if (dnull_flag_3 != 1) FAILTEXT("failed detecting TNULL for TLONG as long long");
      if (dnull_flag_4 != 1) FAILTEXT("failed detecting TNULL for TLONGLONG");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_float_null(void) {

  typedef float ttype;
  ttype a_float, a_double;

  char dnull_flag_1=0, dnull_flag_2=0;

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test detection of NaN for floats");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data as float") {
      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY,"a_float",a_float,&dnull_flag_1);
      router1.connectScalar(ahfits::e_READONLY,"a_double",a_double,&dnull_flag_2);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      if (dnull_flag_1 != 1) FAILTEXT("failed detecting NaN for float");
      if (dnull_flag_2 != 1) FAILTEXT("failed detecting NaN for double");

    } END_TEST

  }


  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_bytes_null(void) {

  typedef char ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10]; //  = {0,0,0,0,0,0,0,0,0,0};
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as char");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_bytes",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dat[i] << "   " << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
      }
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dat[i] << "   " << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting TNULL from byte array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_shorts_null(void) {

  typedef short ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10];
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as short");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_shorts",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting TNULL from short array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_longs_null(void) {

  typedef long ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10];
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as long");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_longs",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting TNULL from long array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_longlongs_null(void) {

  typedef long long ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10];
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as longlong");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_longlongs",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting TNULL from longlong array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_floats_null(void) {

  typedef float ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10];
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as float");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_floats",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting NaN from float array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_vec_doubles_null(void) {

  typedef double ttype;
  const int maxsize=10;
  ttype dat[maxsize];

  char dnull_flag_1[10];
  char dnull_flag_values[10] = {0,0,1,0,0,0,1,0,0,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as double");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectFixedLengthArray(ahfits::e_READONLY,"v_doubles",dat,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<10;++i) {
        AH_INFO(3) << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting NaN from double array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_var_bytes_null(void) {

  typedef char ttype;
  const int maxsize=5;
  ttype dat[maxsize];
  ahfits::IndexType dat_count=0;

  char dnull_flag_1[5];
  char dnull_flag_values[5] = {1,0,0,1,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as char");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectVariableLengthArray(ahfits::e_READONLY,"vv_bytes",dat,dat_count,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      for (int i=0;i<5;++i) {
        AH_INFO(3) << (int) dat[i] << "   " << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
      }
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<5;++i) {
        AH_INFO(3) << (int) dat[i] << "   " << (int) dnull_flag_1[i] << "   " << (int) dnull_flag_values[i] << std::endl;
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting TNULL from byte array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_read_var_doubles_null(void) {

  typedef double ttype;
  const int maxsize=5;
  ttype dat[maxsize];
  ahfits::IndexType dat_count=0;

  char dnull_flag_1[5];
  char dnull_flag_values[5] = {1,0,0,1,0};

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test reading variable-column data as double");

  START_TEST("open FITS file") {
    ahfits::open("./input/types_with_nulls.fits", "types",&ahffp);
    if (0 == ahffp) FAILTEXT("after ahfits::open, ahffp is null");
  } END_TEST

  if (0 != ahffp) {

    START_TEST("read row data") {
      ahfits::Router router1(ahffp);

      router1.connectVariableLengthArray(ahfits::e_READONLY,"vv_doubles",dat,dat_count,dnull_flag_1);

      // check 2nd row
      ahfits::readRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::readRow(ahffp);
      bool OK=true;
      for (int i=0;i<5;++i) {
        if (dnull_flag_1[i] != dnull_flag_values[i]) OK=false;
      }
      if (!OK) FAILTEXT("failed getting NaN from double array");
    } END_TEST

  }

  // Clean up.
  ahfits::close(ahffp);
}

// -----------------------------------------------------------------------------

void ut_write_row_nulls (void) {

  // this pointer will be used for all tests
  ahfits::FilePtr ahffp = 0;

  LABEL_TEST("Test writing FITS columns");

  START_TEST("clone input file and open") {
    ahfits::clone("./input/types_with_nulls.fits","!./output/types_with_nulls_out.fits",
                  &ahffp);
    ahfits::move(ahffp,"types");
  } END_TEST

  char dnf_a_byte[1], dnf_a_short[1], dnf_a_long[1], dnf_a_longlong[1];
  char dnf_a_float[1], dnf_a_double[1];
  char dnf_v_longs[10], dnf_v_bytes[10], dnf_v_shorts[10], dnf_v_longlongs[10];
  char dnf_v_floats[10], dnf_v_doubles[10];
  char dnf_vv_doubles[10], dnf_vv_bytes[10];

  ahfits::IndexType v_bits_count;
  ahfits::IndexType vv_doubles_count, vv_bytes_count, vv_bits_count;

  char a_byte;
  bool a_bool;
  char a_char;
  short a_short;
  long a_long;
  long long a_longlong;
  float a_float;
  double a_double;
  std::string a_string;
  long v_longs[10];
  char v_bytes[10];
  bool v_bools[10];
  short v_shorts[10];
  long long v_longlongs[10];
  float v_floats[10];
  double v_doubles[10];
  char v_bits[10];
  double vv_doubles[10];
  unsigned char vv_bytes[10];
  char vv_bits[10];

  if (0 != ahffp) {

    START_TEST ("copy a row and rearrange the nulls") {

      AH_INFO(3) << "About to make connections" << std::endl;

      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READWRITE,"a_byte", a_byte, dnf_a_byte);
      router1.connectScalar(ahfits::e_READWRITE,"a_bool", a_bool);
      router1.connectScalar(ahfits::e_READWRITE,"a_char", a_char);
      router1.connectScalar(ahfits::e_READWRITE,"a_short", a_short, dnf_a_short);
      router1.connectScalar(ahfits::e_READWRITE,"a_long", a_long, dnf_a_long);
      router1.connectScalar(ahfits::e_READWRITE,"a_longlong", a_longlong, dnf_a_longlong);
      router1.connectScalar(ahfits::e_READWRITE,"a_float", a_float, dnf_a_float);
      router1.connectScalar(ahfits::e_READWRITE,"a_double", a_double, dnf_a_double);
      router1.connectScalar(ahfits::e_READWRITE,"a_string", a_string);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_longs", v_longs, dnf_v_longs);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_bytes", v_bytes, dnf_v_bytes);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_bools", v_bools);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_shorts", v_shorts, dnf_v_shorts);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_longlongs", v_longlongs, dnf_v_longlongs);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_floats", v_floats, dnf_v_floats);
      router1.connectFixedLengthArray(ahfits::e_READWRITE,"v_doubles", v_doubles, dnf_v_doubles);
      router1.connectBit(ahfits::e_READWRITE,"v_bits", v_bits, v_bits_count);
      router1.connectVariableLengthArray(ahfits::e_READWRITE,"vv_doubles", vv_doubles, vv_doubles_count, dnf_vv_doubles);
      router1.connectVariableLengthArray(ahfits::e_READWRITE,"vv_bytes", vv_bytes, vv_bytes_count, dnf_vv_bytes);
      router1.connectBit(ahfits::e_READWRITE,"vv_bits", vv_bits, vv_bits_count);

      ahfits::firstRow(ahffp);  // row=1
      ahfits::nextRow(ahffp);   // row=2
      AH_INFO(3) << "Reading row 2" << std::endl;
      ahfits::readRow(ahffp);   // row=2  
      ahfits::nextRow(ahffp);   // row=3
      ahfits::nextRow(ahffp);   // row=4

      //  Put the good stuff in here:
      //  Change up the nulls a little.

      a_byte = 32; dnf_a_byte[0] = 0;
      a_short = 33; dnf_a_short[0] = 0;
      a_long = 34; dnf_a_long[0] = 0;
      a_longlong = 35; dnf_a_longlong[0] = 0;
      a_float = 36.0; dnf_a_float[0] = 0;
      a_double = 37.0; dnf_a_double[0] = 0;
      dnf_v_longs[4] = 1;
      dnf_v_bytes[4] = 1;
      dnf_v_shorts[4] = 1;
      dnf_v_longlongs[4] = 1;
      dnf_v_floats[4] = 1;
      dnf_v_doubles[4] = 1;
      dnf_vv_doubles[4] = 1;
      dnf_vv_bytes[4] = 1;

      AH_INFO(3) << "Writing row 4" << std::endl;
      ahfits::writeRow(ahffp);  // Should process all the connections

      ahfits::close(ahffp);

    } END_TEST

  }

  START_TEST("re-open output file and check") {
    ahfits::open("./output/types_with_nulls_out.fits", "types", &ahffp);
  } END_TEST

  if (0 != ahffp) {

    START_TEST ("check the nulls") {

      AH_INFO(3) << "About to make connections" << std::endl;

      ahfits::Router router1(ahffp);

      router1.connectScalar(ahfits::e_READONLY, "a_byte", a_byte, dnf_a_byte);
      router1.connectScalar(ahfits::e_READONLY, "a_bool", a_bool);
      router1.connectScalar(ahfits::e_READONLY, "a_char", a_char);
      router1.connectScalar(ahfits::e_READONLY, "a_short", a_short, dnf_a_short);
      router1.connectScalar(ahfits::e_READONLY, "a_long", a_long, dnf_a_long);
      router1.connectScalar(ahfits::e_READONLY, "a_longlong", a_longlong, dnf_a_longlong);
      router1.connectScalar(ahfits::e_READONLY, "a_float", a_float, dnf_a_float);
      router1.connectScalar(ahfits::e_READONLY, "a_double", a_double, dnf_a_double);
      router1.connectScalar(ahfits::e_READONLY, "a_string", a_string);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_longs", v_longs, dnf_v_longs);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_bytes", v_bytes, dnf_v_bytes);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_bools", v_bools);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_shorts", v_shorts, dnf_v_shorts);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_longlongs", v_longlongs, dnf_v_longlongs);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_floats", v_floats, dnf_v_floats);
      router1.connectFixedLengthArray(ahfits::e_READONLY, "v_doubles", v_doubles, dnf_v_doubles);
      router1.connectBit(ahfits::e_READONLY, "v_bits", v_bits, v_bits_count);
      router1.connectVariableLengthArray(ahfits::e_READONLY, "vv_doubles", vv_doubles, vv_doubles_count, dnf_vv_doubles);
      router1.connectVariableLengthArray(ahfits::e_READONLY, "vv_bytes", vv_bytes, vv_bytes_count, dnf_vv_bytes);
      router1.connectBit(ahfits::e_READONLY, "vv_bits", vv_bits, vv_bits_count);

      ahfits::firstRow(ahffp);  // row=1
      ahfits::nextRow(ahffp);   // row=2
      ahfits::nextRow(ahffp);   // row=3
      ahfits::nextRow(ahffp);   // row=4
      AH_INFO(3) << "Reading row 4" << std::endl;
      ahfits::readRow(ahffp);   // row=4  

      ahfits::close(ahffp);

      bool OK = true;
      if (a_byte != 32 || dnf_a_byte[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_byte field = " << (int) a_byte << std::endl;
        AH_INFO(3) << "Data null flag        = " << (int) dnf_a_byte[0] << std::endl;
      }
      if (a_short != 33 || dnf_a_short[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_short field = " << a_short << std::endl;
        AH_INFO(3) << "Data null flag         = " << (int) dnf_a_short[0] << std::endl;
      }
      if (a_long != 34 || dnf_a_long[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_long field = " << a_long << std::endl;
        AH_INFO(3) << "Data null flag        = " << (int) dnf_a_long[0] << std::endl;
      }
      if (a_longlong != 35 || dnf_a_longlong[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_longlong field = " << a_longlong << std::endl;
        AH_INFO(3) << "Data null flag            = " << (int) dnf_a_longlong[0] << std::endl;
      }
      if (a_float != 36.0 || dnf_a_float[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_float field = " << a_float << std::endl;
        AH_INFO(3) << "Data null flag         = " << (int) dnf_a_float[0] << std::endl;
      }
      if (a_double != 37.0 || dnf_a_double[0] != 0) {
        OK = false;
        AH_INFO(3) << "Error in a_double field = " << a_double << std::endl;
        AH_INFO(3) << "Data null flag          = " << (int) dnf_a_double[0] << std::endl;
      }

      short shortvals[] = {1,2,3,4,5,6,7,8,9,10};
      long longvals[] = {1,2,3,4,5,6,7,8,9,10};
      long long llongvals[] = {1,2,3,4,5,6,7,8,9,10}; 
      char bytevals[] = {97,98,99,100,101,102,103,104,105,106};
      float floatvals[] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0};
      double dblvals[] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0};
      char dnf_v[] = {0,0,1,0,1,0,1,0,0,0};
      char bytevals2[] = {65,66,67,68,69};

      for (int ii=0; ii<10; ++ii) {
        if (dnf_v_longs[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_longs[" << ii << "] = " << v_longs[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_longs[ii] << std::endl;
        }
        if (dnf_v_bytes[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_bytes[" << ii << "] = " << (int) v_bytes[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_bytes[ii] << std::endl;
        }
        if (dnf_v_shorts[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_shorts[" << ii << "] = " << v_shorts[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_shorts[ii] << std::endl;
        }
        if (dnf_v_longlongs[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_longlongs[" << ii << "] = " << v_longlongs[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_longlongs[ii] << std::endl;
        }
        if (dnf_v_floats[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_floats[" << ii << "] = " << v_floats[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_floats[ii] << std::endl;
        }
        if (dnf_v_doubles[ii] != dnf_v[ii]) {
          OK = false;
          AH_INFO(3) << "Error in v_doubles[" << ii << "] = " << v_doubles[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_doubles[ii] << std::endl;
        }

        if (v_longs[ii] != longvals[ii] && dnf_v_longs[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_longs[" << ii << "] = " << v_longs[ii] << std::endl;
        }
        if (v_bytes[ii] != bytevals[ii] && dnf_v_bytes[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_bytes[" << ii << "] = " <<  (int) v_bytes[ii] << std::endl;
        }
        if (v_shorts[ii] != shortvals[ii] && dnf_v_shorts[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_shorts[" << ii << "] = " << v_shorts[ii] << std::endl;
        }
        if (v_longlongs[ii] != llongvals[ii] && dnf_v_longlongs[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_longlongs[" << ii << "] = " << v_longlongs[ii] << std::endl;
        }
        if (v_floats[ii] != floatvals[ii] && dnf_v_floats[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_floats[" << ii << "] = " << v_floats[ii] << std::endl;
        }
        if (v_doubles[ii] != dblvals[ii] && dnf_v_doubles[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in v_doubles[" << ii << "] = " << v_doubles[ii] << std::endl;
        }

        if (dnf_v_longs[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_longs[" << ii << "] = " << v_longs[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_longs[ii] << std::endl;
          if (dnf_v_longs[ii] != 1) OK = false;
        }
        if (dnf_v_bytes[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_bytes[" << ii << "] = " <<  (int) v_bytes[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_bytes[ii] << std::endl;
          if (dnf_v_bytes[ii] != 1) OK = false;
        }
        if (dnf_v_shorts[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_shorts[" << ii << "] = " << v_shorts[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_shorts[ii] << std::endl;
          if (dnf_v_shorts[ii] != 1) OK = false;
        }
        if (dnf_v_longlongs[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_longlongs[" << ii << "] = " << v_longlongs[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_longlongs[ii] << std::endl;
          if (dnf_v_longlongs[ii] != 1) OK = false;
        }
        if (dnf_v_floats[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_floats[" << ii << "] = " << v_floats[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_floats[ii] << std::endl;
          if (dnf_v_floats[ii] != 1) OK = false;
        }
        if (dnf_v_doubles[ii] != 0) {
          AH_INFO(3) << "Note: null flag non-zero: v_doubles[" << ii << "] = " << v_doubles[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_v_doubles[ii] << std::endl;
          if (dnf_v_doubles[ii] != 1) OK = false;
        }
      }

      char dnf_vv[] = {1,0,0,1,1};

      for (int ii=0; ii<5; ++ii) {
        if (dnf_vv_doubles[ii] != dnf_vv[ii]) {
          OK = false;
          AH_INFO(3) << "Error in vv_doubles[" << ii << "] = " << vv_doubles[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_vv_doubles[ii] << std::endl;
        }
        if (dnf_vv_bytes[ii] != dnf_vv[ii]) {
          OK = false;
          AH_INFO(3) << "Error in vv_bytes[" << ii << "] = " << (int) vv_bytes[ii] << std::endl;
          AH_INFO(3) << "Data null flag = " << (int) dnf_vv_bytes[ii] << std::endl;
        }
        if (vv_doubles[ii] != dblvals[ii] && dnf_vv_doubles[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in vv_doubles[" << ii << "] = " << vv_doubles[ii] << std::endl;
        }
        if (vv_bytes[ii] != bytevals2[ii] && dnf_vv_bytes[ii] == 0) {
          OK = false;
          AH_INFO(3) << "Error in vv_bytes[" << ii << "] = " << (int) vv_bytes[ii] << std::endl;
        }
      }

      if (!OK) FAILTEXT("error in reading re-opened file");

    } END_TEST
  }


}

// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ut_ahfits_row_nulls.cxx,v $
 Revision 1.9  2013/09/11 21:04:24  mwitthoe
 ahfits unit tests (colinfo, file, row_nulls): switched over to using new connect* functions, e.g. connectScalar

 Revision 1.8  2013/07/16 20:10:06  mwitthoe
 ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

 Revision 1.7  2013/05/14 17:45:44  mwitthoe
 ahfits buffering: disable buffering to TBIT output types; fix bug causing wrong number of rows to be written for fixed-length types; fix bug where NULLs were not being written for fixed-length types

 Revision 1.6  2013/05/13 19:45:51  mwitthoe
 add buffering to ahfits (no TBIT support yet)

 Revision 1.5  2013/03/19 20:46:21  rshill
 Column ead/write flags implemented; inner-loop debugging code deleted.

 Revision 1.4  2013/03/19 17:48:44  rshill
 Version with column read/write flags.

 Revision 1.3  2013/03/05 23:01:49  rshill
 Extensive tests of TNULL and NaN capability.

 Revision 1.2  2013/03/04 20:28:46  rshill
 Progressing through addition of tests.

 Revision 1.1  2013/03/02 00:51:57  rshill
 Added test for undefined numeric values (TNULL or NaN).


*/
