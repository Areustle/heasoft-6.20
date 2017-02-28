/// \file ut_ahfits_buffer.cxx
/// \brief unit test for ahfits buffering
/// \author Mike Witthoeft
/// \date $Date: 2014/12/15 22:28:05 $
 
#define AHLABEL ahtime_ut_ahfits_buffer
#define AHCVSID "$Id: ut_ahfits_buffer.cxx,v 1.5 2014/12/15 22:28:05 mdutka Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <iomanip>
#include <cstring>
#include <sstream>

// -----------------------------------------------------------------------------

void createOutputFileWith1Column(const std::string& filename, const std::string& extname,
                                 const std::string& colname, const std::string& coltype,
                                 ahfits::FilePtr& ahffp) {
  ahfits::create(filename,"",&ahffp);
  ahfits::addEmptyTbl(ahffp,extname);
  ahfits::insertColAfter(ahffp,colname,coltype);
}

// -----------------------------------------------------------------------------

void copyFileTheHardWay(const std::string& src, const std::string& dest) {
  ahfits::FilePtr fp;
  ahfits::clone(src,"!"+dest,&fp,true);
  ahfits::close(fp);
}

// -----------------------------------------------------------------------------

void ut_write_new_file_scalar(int buffer) {

  std::stringstream label;
  label << "Test creating FITS file with single scalar column (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string filename="./output/app_scalar.fits";
  std::string extname="TEMP";
  std::string colname="DUMMY";
  std::string coltype="1I";
  ahfits::FilePtr ahffp;
  START_TEST("create new FITS file with single column") {
    createOutputFileWith1Column("!"+filename,extname,colname,coltype,ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  ahfits::Router router1(ahffp);
  int lvar=0;         // store column values
  START_TEST("connect local variable to column") {
    router1.connectScalar(ahfits::e_WRITEONLY,colname,lvar);
  } END_TEST

  int nrows=100;
  START_TEST("write many rows of data") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) {
      lvar=i;
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check if written correctly
  START_TEST("open FITS file for reading") {
    ahfits::open(filename,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column") {
    router2.connectScalar(ahfits::e_READONLY,colname,lvar);
  } END_TEST

  START_TEST("read and check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      ahfits::readRow(ahffp);
      if (lvar != i) {
        std::stringstream msg;
        msg << "Row " << i << " has wrong value: " << lvar;
        FAILTEXT(msg.str());
      }
      i++;
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_write_new_file_fixed_length(int buffer) {

  std::stringstream label;
  label << "Test creating FITS file with single fixed-length array column (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string filename="./output/app_fixed.fits";
  std::string extname="TEMP";
  std::string colname="DUMMY";
  std::string coltype="10I";
  ahfits::FilePtr ahffp;
  START_TEST("create new FITS file with single column") {
    createOutputFileWith1Column("!"+filename,extname,colname,coltype,ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  ahfits::Router router1(ahffp);
  int lvar[10]={0,0,0,0,0,0,0,0,0,0};         // store column values
  START_TEST("connect local variable to column") {
    router1.connectFixedLengthArray(ahfits::e_WRITEONLY,colname,lvar);
  } END_TEST

  int nrows=100;
  START_TEST("write many rows of data") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) {
      for (int j=0; j < 10; j++) {
        lvar[j]=10*i+j;
      }
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check if written correctly
  START_TEST("open FITS file for reading") {
    ahfits::open(filename,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column") {
    router2.connectFixedLengthArray(ahfits::e_READONLY,colname,lvar);
  } END_TEST

  START_TEST("read and check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      ahfits::readRow(ahffp);
      for (int j=0; j < 10; j++) {
        if (lvar[j] != 10*i+j) {
          std::stringstream msg;
          msg << "Row " << i << " has wrong value: " << lvar;
          FAILTEXT(msg.str());
        }
      }
      i++;
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_write_new_file_variable_length(int buffer) {

  std::stringstream label;
  label << "Test creating FITS file with single variable-length array column (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string filename="./output/app_variable.fits";
  std::string extname="TEMP";
  std::string colname="DUMMY";
  std::string coltype="PI(10)";
  ahfits::FilePtr ahffp;
  START_TEST("create new FITS file with single column") {
    createOutputFileWith1Column("!"+filename,extname,colname,coltype,ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  ahfits::Router router1(ahffp);
  int lvar[10]={0,0,0,0,0,0,0,0,0,0};         // store column values
  ahfits::IndexType lvar_size=0;
  START_TEST("connect local variable to column") {
    router1.connectVariableLengthArray(ahfits::e_WRITEONLY,colname,lvar,lvar_size);
  } END_TEST

  int nrows=100;
  START_TEST("write many rows of data") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) {
      lvar_size=i%10+1;
      for (int j=0; j < lvar_size; j++) {
        lvar[j]=10*i+j;
      }
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check if written correctly
  START_TEST("open FITS file for reading") {
    ahfits::open(filename,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column") {
    router2.connectVariableLengthArray(ahfits::e_READONLY,colname,lvar,lvar_size);
  } END_TEST

  START_TEST("read and check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      ahfits::readRow(ahffp);
      for (int j=0; j < lvar_size; j++) {
        if (lvar[j] != 10*i+j) {
          std::stringstream msg;
          msg << "Row " << i << " has wrong value: " << lvar;
          FAILTEXT(msg.str());
        }
      }
      i++;
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_write_new_file_bit(int buffer) {

  std::stringstream label;
  label << "Test creating FITS file with single bit-type column (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string filename="./output/app_bit.fits";
  std::string extname="TEMP";
  std::string colname="DUMMY";
  std::string coltype="10X";
  ahfits::FilePtr ahffp;
  START_TEST("create new FITS file with single column") {
    createOutputFileWith1Column("!"+filename,extname,colname,coltype,ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  ahfits::Router router1(ahffp);
  char lvar[10]={0,0,0,0,0,0,0,0,0,0};         // store column values
  ahfits::IndexType lvar_size=10;
  START_TEST("connect local variable to column") {
    router1.connectBit(ahfits::e_WRITEONLY,colname,lvar,lvar_size);
  } END_TEST

  int nrows=100;
  START_TEST("write many rows of data") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) {
      for (int j=0; j < 10; j++) {
        lvar[j]=(i+j)%2;
      }
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check if written correctly
  START_TEST("open FITS file for reading") {
    ahfits::open(filename,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column") {
    router2.connectBit(ahfits::e_READONLY,colname,lvar,lvar_size);
  } END_TEST

  START_TEST("read and check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      ahfits::readRow(ahffp);
      for (int j=0; j < 10; j++) {
        if (lvar[j] != (i+j)%2) {
          std::stringstream msg;
          msg << "Row " << i << " has wrong value: " << lvar;
          FAILTEXT(msg.str());
        }
      }
      i++;
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_write_new_file_multiple(int buffer) {

  std::stringstream label;
  label << "Test creating FITS file with single bit-type column (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string filename="./output/app_bit.fits";
  std::string extname="TEMP";
  std::string colname="DUMMY";
  std::string coltype="10X";
  ahfits::FilePtr ahffp;
  START_TEST("create new FITS file with single column") {
    createOutputFileWith1Column("!"+filename,extname,colname,coltype,ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  ahfits::Router router1(ahffp);
  char lvar[10]={0,0,0,0,0,0,0,0,0,0};         // store column values
  ahfits::IndexType lvar_size=10;
  START_TEST("connect local variable to column") {
    router1.connectBit(ahfits::e_WRITEONLY,colname,lvar,lvar_size);
  } END_TEST

  int nrows=100;
  START_TEST("write many rows of data") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) {
      for (int j=0; j < 10; j++) {
        lvar[j]=(i+j)%2;
      }
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check if written correctly
  START_TEST("open FITS file for reading") {
    ahfits::open(filename,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column") {
    router2.connectBit(ahfits::e_READONLY,colname,lvar,lvar_size);
  } END_TEST

  START_TEST("read and check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      ahfits::readRow(ahffp);
      for (int j=0; j < 10; j++) {
        if (lvar[j] != (i+j)%2) {
          std::stringstream msg;
          msg << "Row " << i << " has wrong value: " << lvar;
          FAILTEXT(msg.str());
        }
      }
      i++;
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_clone_and_edit(int buffer) {

  std::stringstream label;
  label << "CLone file and edit rows (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string infile="./input/types.fits";
  std::string outfile="./output/edit_types1.fits";
  std::string extname="types";
  std::string colname_sca="a_short";
  std::string colname_fix="v_floats";
  std::string colname_var="vv_bytes";
  ahfits::IndexType num_var=1;
  ahfits::FilePtr ahffp;
  START_TEST("clone FITS file") {
    ahfits::clone(infile,"!"+outfile,&ahffp,true);
    ahfits::move(ahffp,extname);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  int nrows=ahfits::numRows(ahffp);

  ahfits::Router router(ahffp);
  int lvar_sca=0;                                  // store column values
  float lvar_fix[10];
  char lvar_var[10];
  for (int i=0; i < 10; i++) {
    lvar_fix[i]=0.;
    lvar_var[i]=0;
  }
  START_TEST("connect local variable to column for writing") {
    router.connectScalar(ahfits::e_WRITEONLY,colname_sca,lvar_sca);
    router.connectFixedLengthArray(ahfits::e_WRITEONLY,colname_fix,lvar_fix);
    router.connectVariableLengthArray(ahfits::e_WRITEONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("write many rows of data") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      i++;
      lvar_sca=i*i;
      for (int j=0; j < 10; j++) lvar_fix[j]=i+j;
      num_var=i;
      for (int j=0; j < num_var; j++) lvar_var[j]=(i+1)*(j+1);
      ahfits::writeRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check number of rows
  START_TEST("open FITS file for reading") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column for reading") {
    router2.connectScalar(ahfits::e_READONLY,colname_sca,lvar_sca);
    router2.connectFixedLengthArray(ahfits::e_READONLY,colname_fix,lvar_fix);
    router2.connectVariableLengthArray(ahfits::e_READONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      i++;
      ahfits::readRow(ahffp);
      if (lvar_sca != i*i) FAIL;
      for (int j=0; j < 10; j++) {
        if (lvar_fix[j] != i+j) FAIL;
      }
      if (num_var != i) FAIL;
      for (int j=0; j < num_var; j++) {
        if (lvar_var[j] != (i+1)*(j+1)) FAIL;
      }
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_open_and_edit(int buffer) {

  std::stringstream label;
  label << "Open file and edit rows (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file (clone, but close and reopen file)
  std::string infile="./input/types.fits";
  std::string outfile="./output/edit_types2.fits";
  START_TEST("copy file") {
    copyFileTheHardWay(infile,outfile);
  } END_TEST

  // open file
  std::string extname="types";
  std::string colname_sca="a_short";
  std::string colname_fix="v_floats";
  std::string colname_var="vv_bytes";
  ahfits::IndexType num_var=1;
  ahfits::FilePtr ahffp;
  START_TEST("open FITS file") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  int nrows=ahfits::numRows(ahffp);

  ahfits::Router router(ahffp);
  int lvar_sca=0;                                  // store column values
  float lvar_fix[10];
  char lvar_var[10];
  for (int i=0; i < 10; i++) {
    lvar_fix[i]=0.;
    lvar_var[i]=0;
  }
  START_TEST("connect local variable to column for writing") {
    router.connectScalar(ahfits::e_WRITEONLY,colname_sca,lvar_sca);
    router.connectFixedLengthArray(ahfits::e_WRITEONLY,colname_fix,lvar_fix);
    router.connectVariableLengthArray(ahfits::e_WRITEONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("write many rows of data") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      i++;
      lvar_sca=i*i;
      for (int j=0; j < 10; j++) lvar_fix[j]=i+j;
      num_var=i;
      for (int j=0; j < num_var; j++) lvar_var[j]=(i+1)*(j+1);
      ahfits::writeRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check number of rows
  START_TEST("open FITS file for reading") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column for reading") {
    router2.connectScalar(ahfits::e_READONLY,colname_sca,lvar_sca);
    router2.connectFixedLengthArray(ahfits::e_READONLY,colname_fix,lvar_fix);
    router2.connectVariableLengthArray(ahfits::e_READONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("check row values") {
    int i=0;
    for (ahfits::firstRow(ahffp);ahfits::readOK(ahffp);ahfits::nextRow(ahffp)) {
      i++;
      ahfits::readRow(ahffp);
      if (lvar_sca != i*i) FAIL;
      for (int j=0; j < 10; j++) {
        if (lvar_fix[j] != i+j) FAIL;
      }
      if (num_var != i) FAIL;
      for (int j=0; j < num_var; j++) {
        if (lvar_var[j] != (i+1)*(j+1)) FAIL;
      }
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_clone_and_append(int buffer) {

  std::stringstream label;
  label << "Clone file and append rows (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file
  std::string infile="./input/types.fits";
  std::string outfile="./output/append_types1.fits";
  std::string extname="types";
  std::string colname_sca="a_short";
  std::string colname_fix="v_floats";
  std::string colname_var="vv_bytes";
  ahfits::IndexType num_var=1;
  ahfits::FilePtr ahffp;
  START_TEST("clone FITS file") {
    ahfits::clone(infile,"!"+outfile,&ahffp,true);
    ahfits::move(ahffp,extname);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  int nrows=ahfits::numRows(ahffp);

  ahfits::Router router(ahffp);
  int lvar_sca=0;                                  // store column values
  float lvar_fix[10];
  char lvar_var[10];
  for (int i=0; i < 10; i++) {
    lvar_fix[i]=0.;
    lvar_var[i]=0;
  }
  START_TEST("connect local variable to column for writing") {
    router.connectScalar(ahfits::e_WRITEONLY,colname_sca,lvar_sca);
    router.connectFixedLengthArray(ahfits::e_WRITEONLY,colname_fix,lvar_fix);
    router.connectVariableLengthArray(ahfits::e_WRITEONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("append 3 rows to end of file") {
    ahfits::lastRow(ahffp);
    ahfits::nextRow(ahffp);     // move past last row to append
    for (int i=1; i <= 3; i++) {
      lvar_sca=i*i;
      for (int j=0; j < 10; j++) lvar_fix[j]=i+j;
      num_var=i;
      for (int j=0; j < num_var; j++) lvar_var[j]=(i+1)*(j+1);
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check number of rows
  START_TEST("open FITS file for reading") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows+3 != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column for reading") {
    router2.connectScalar(ahfits::e_READONLY,colname_sca,lvar_sca);
    router2.connectFixedLengthArray(ahfits::e_READONLY,colname_fix,lvar_fix);
    router2.connectVariableLengthArray(ahfits::e_READONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("check new row values") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) ahfits::nextRow(ahffp);
    for (int i=1; i <= 3; i++) {
      ahfits::readRow(ahffp);
      if (lvar_sca != i*i) FAIL;
      for (int j=0; j < 10; j++) {
        if (lvar_fix[j] != i+j) FAIL;
      }
      if (num_var != i) FAIL;
      for (int j=0; j < num_var; j++) {
        if (lvar_var[j] != (i+1)*(j+1)) FAIL;
      }
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------

void ut_open_and_append(int buffer) {

  std::stringstream label;
  label << "Clone file and append rows (buffer = " << buffer << ")";
  LABEL_TEST(label.str());

  // override buffer size; will reset original value at end of routine
  int buffer_orig=ahfits::getBuffer();
  ahfits::setBuffer(buffer);

  // create output file (clone, but close and reopen file)
  std::string infile="./input/types.fits";
  std::string outfile="./output/append_types2.fits";
  START_TEST("copy file") {
    copyFileTheHardWay(infile,outfile);
  } END_TEST

  // open file
  std::string extname="types";
  std::string colname_sca="a_short";
  std::string colname_fix="v_floats";
  std::string colname_var="vv_bytes";
  ahfits::IndexType num_var=1;
  ahfits::FilePtr ahffp;
  START_TEST("open FITS file") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  int nrows=ahfits::numRows(ahffp);

  ahfits::Router router(ahffp);
  int lvar_sca=0;                                  // store column values
  float lvar_fix[10];
  char lvar_var[10];
  for (int i=0; i < 10; i++) {
    lvar_fix[i]=0.;
    lvar_var[i]=0;
  }
  START_TEST("connect local variable to column for writing") {
    router.connectScalar(ahfits::e_WRITEONLY,colname_sca,lvar_sca);
    router.connectFixedLengthArray(ahfits::e_WRITEONLY,colname_fix,lvar_fix);
    router.connectVariableLengthArray(ahfits::e_WRITEONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("append 3 rows to end of file") {
    ahfits::lastRow(ahffp);
    ahfits::nextRow(ahffp);     // move past last row to append
    for (int i=1; i <= 3; i++) {
      lvar_sca=i*i;
      for (int j=0; j < 10; j++) lvar_fix[j]=i+j;
      num_var=i;
      for (int j=0; j < num_var; j++) lvar_var[j]=(i+1)*(j+1);
      ahfits::writeRow(ahffp);
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // now read file in and check number of rows
  START_TEST("open FITS file for reading") {
    ahfits::open(outfile,extname,&ahffp);
    if (0 == ahffp) FAIL;
  } END_TEST
  if (0 == ahffp) return;

  START_TEST("check number of rows") {
    if (nrows+3 != ahfits::numRows(ahffp)) FAIL;
  } END_TEST

  ahfits::Router router2(ahffp);
  START_TEST("connect local variable to column for reading") {
    router2.connectScalar(ahfits::e_READONLY,colname_sca,lvar_sca);
    router2.connectFixedLengthArray(ahfits::e_READONLY,colname_fix,lvar_fix);
    router2.connectVariableLengthArray(ahfits::e_READONLY,colname_var,lvar_var,num_var);
  } END_TEST

  START_TEST("check new row values") {
    ahfits::firstRow(ahffp);
    for (int i=0; i < nrows; i++) ahfits::nextRow(ahffp);
    for (int i=1; i <= 3; i++) {
      ahfits::readRow(ahffp);
      if (lvar_sca != i*i) FAIL;
      for (int j=0; j < 10; j++) {
        if (lvar_fix[j] != i+j) FAIL;
      }
      if (num_var != i) FAIL;
      for (int j=0; j < num_var; j++) {
        if (lvar_var[j] != (i+1)*(j+1)) FAIL;
      }
      ahfits::nextRow(ahffp);
    }
  } END_TEST

  // close file
  ahfits::close(ahffp);

  // restore value of buffer
  ahfits::setBuffer(buffer_orig);

}

// -----------------------------------------------------------------------------







/* Revision Log
 $Log: ut_ahfits_buffer.cxx,v $
 Revision 1.5  2014/12/15 22:28:05  mdutka
 updated exception for issue #341 to occur as soon as the colmuns are connected

 Revision 1.4  2014/12/12 22:47:11  mdutka
 Exception is thrown if buffer is set and crosstypes with nulls are connected (see issue #341)

 Revision 1.3  2014/11/26 15:12:45  mwitthoe
 ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

 Revision 1.2  2014/05/06 22:04:54  mwitthoe
 ahfits: add unit tests to check different buffer parameters when editing or appending rows to a cloned or copied file; see issue 368

 Revision 1.1  2014/03/27 15:06:49  mwitthoe
 ahfits: add unit tests to test buffering when creating a file or editing an entire file


*/
