/// \file testahfits.h
/// \brief Prototypes of test routines for ahfits libraries
/// \author Mike Witthoeft
/// \date $Date: 2015/10/09 21:09:22 $
 
#ifndef AHTIME_TESTAHFITS_H
#define AHTIME_TESTAHFITS_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_TESTAHFITS,"$Id: testahfits.h,v 1.38 2015/10/09 21:09:22 mdutka Exp $")

#include <string>

/// \brief unit test for ahfits_file: opening and closing files
void ut_create_ab_initio(void);

/// \brief unit test for ahfits_file: opening and closing files
void ut_open_close(void);

/// \brief unit test for ahfits_file: using extended syntax
void ut_open_extended(void);

/// \brief unit test for ahfits_file: creating and cloning files
void ut_create_clone(void);

/// \brief unit test for ahfits_file: moving to extension by number
void ut_move_HDU_number(void);

/// \brief unit test for ahfits_file: iterating over extensions
void ut_loop_HDU(void);

/// \brief unit test for cloning a single HDU to a new file and then
///  adding a new HDU based on header from existing HDU
void ut_clone1HDU(void);

/// \brief unit test for trying to go to 1st primary HDU 
void ut_firstHDU(void);

/// \brief unit test for trying to go to 2nd primary HDU (which fails)
void ut_nextPrimaryHDU(void);

/// \brief unit test for moving to the next binary table
void ut_nextBinaryTbl(void);

/// \brief test appending and deleting rows from end of file
void ut_insert_delete_rows(void);

/// \brief test removal of all rows in an extension
void ut_removeAllRows(void);

/// \brief read/write image for ahfits_image
void ut_write_read_image_dbl(void);

/// \brief read/write image for ahfits_image
void ut_write_read_image_lng(void);

/// \brief read/write image for ahfits_image
void ut_write_read_image_flt(void);

/// \brief read/write image for ahfits_image
void ut_write_read_image_shr(void);

/// \brief read/write image for ahfits_image
void ut_write_read_image_byt(void);

/// \brief unit test for ahfits_colinfo
void ut_colinfo(void);

/// \brief unit test for ahfits_addcolumn
void ut_addcolumn(void);

/// \brief write TNULL keyword for a column and then readRow with NULL
void ut_addtnulltunittdisp(void);

/// \brief write TZERO and TSCALE keywords for a column
void ut_addtzerotscale(void);

/// \brief write TNULL keyword, then connect to that column
void ut_addtnullreloadcol(void);

/// \brief unit test for ahfits_connect
void ut_connect(void);

/// \brief unit test for ahfits_header: read values
void ut_read_header(void);

/// \brief unit test for ahfits_header: write values
void ut_write_header(void);

/// \brief unit test for ahfits_header: copy values
void ut_copy_header(void);

/// \brief unit test for checking if at last row in extension
void ut_checklast(void);

/// \brief unit test for ahfits_row: read values
void ut_read_row(void);

/// \brief unit test for ahfits_row: read char values
void ut_duplicate(void);

/// \brief try (and fail) to create router to primary HDU
void ut_router_to_primary(void);

/// \brief unit test for ahfits_row: read char values
void ut_read_char(void);

/// \brief unit test for ahfits_row: read boolean values
void ut_read_bool(void);

/// \brief unit test for ahfits_row: read short values
void ut_read_short(void);

/// \brief unit test for ahfits_row: read long values
void ut_read_long(void);

/// \brief unit test for ahfits_row: read long long values
void ut_read_longlong(void);

/// \brief unit test for ahfits_row: read float values
void ut_read_float(void);

/// \brief unit test for ahfits_row: read double values
void ut_read_double(void);

/// \brief unit test for ahfits_row: read string values
void ut_read_string(void);

/// \brief unit test for ahfits_row: read char values
void ut_read_arr_chars(void);

/// \brief unit test for ahfits_row: read bool values
void ut_read_arr_bools(void);

/// \brief unit test for ahfits_row: read short values
void ut_read_arr_shorts(void);

/// \brief unit test for ahfits_row: read long values
void ut_read_arr_longs(void);

/// \brief unit test for ahfits_row: read long long values
void ut_read_arr_longlongs(void);

/// \brief unit test for ahfits_row: read float values
void ut_read_arr_floats(void);

/// \brief unit test for ahfits_row: read double values
void ut_read_arr_doubles(void);

/// \brief unit test for ahfits_row: read string array values
///        (not supported, i.e., test verifies error is thrown).
void ut_read_arr_strings(void);

/// \brief unit test for ahfits_row: read char values with null flag
void ut_read_byte_null(void);

/// \brief unit test for ahfits_row: read short values with null flag
void ut_read_short_null(void);

/// \brief unit test for ahfits_row: read long values with null flag
void ut_read_long_null(void);

/// \brief unit test for ahfits_row: read long long values with null flag
void ut_read_longlong_null(void);

/// \brief unit test for ahfits_row: read float values with null flag
void ut_read_float_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_bytes_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_shorts_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_longs_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_longlongs_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_floats_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_vec_doubles_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_var_bytes_null(void);

/// \brief unit test for ahfits_row: read vector of short values with null flags
void ut_read_var_doubles_null(void);

/// \brief unit test for ahfits_row: read bit values
void ut_read_bits(void);

/// \brief unit test for ahfits_row: write values
void ut_write_row(void);

/// \brief unit test for ahfits_row: write values, including undefined ones
void ut_write_row_nulls(void);

/// \brief test using router outside of HDU loop; this checks that
///  the column information is properly reloaded for all relevant
///  routers when moving to a new HDU.
void ut_outside_router(void);

/// \brief read data from two extensions using different columns; this tests
///  the clearConnections() function in ahfits_router
void ut_clear_connections(void);

/// \brief check that READONLY columns cannot be written to
void ut_check_readonly(void);

/// \brief check that WRITEONLY columns cannot be read from
void ut_check_writeonly(void);

/// \brief write middle-row with buffering enabled
void ut_writemiddle_buffer(void);

/// \brief write middle-row with buffering disabled
void ut_writemiddle_nobuffer(void);

/// \brief test connecting crosstypes with nullflags set
void ut_connect_crosstype_nulls();

void ut_file_list(void);

// === ut_ahfits_buffer ===

// The following tests check if buffering is done properly when creating
// a new FITS file with a single column of different types

/// \brief write new FITS file with a single, scalar column
void ut_write_new_file_scalar(int buffer);

/// \brief write new FITS file with a single, fixed-length array column
void ut_write_new_file_fixed_length(int buffer);

/// \brief write new FITS file with a single, variable-length array column
void ut_write_new_file_variable_length(int buffer);

/// \brief write new FITS file with a single, bit-type column
void ut_write_new_file_bit(int buffer);

/// \brief write new FITS file with columns of different types
///
/// This unit test is to check a bug where an extra row is created when
/// writing a variable-width column before a scalar or fixed-width column.
void ut_write_new_file_multiple(int buffer);

// The following test checks that editing an entire FITS file is done
// properly regardless of the buffer size.  This test originated from a 
// problem that occurred when the number of data rows in the FITS file
// differed from the temporary size of the table (because the buffering
// inserted extra rows at the end of the file

/// \brief clone file and edit rows for scalar, fixed-length, and variable-
///  length columns.
void ut_clone_and_edit(int buffer);

/// \brief open file and edit rows for scalar, fixed-length, and variable-
///  length columns.
void ut_open_and_edit(int buffer);

/// \brief clone file and append rows for scalar, fixed-length, and variable-
///  length columns.
void ut_clone_and_append(int buffer);

/// \brief open file and append rows for scalar, fixed-length, and variable-
///  length columns.
void ut_open_and_append(int buffer);




#endif /* AHTIME_TESTAHFITS_H */

/* Revision Log
 $Log: testahfits.h,v $
 Revision 1.38  2015/10/09 21:09:22  mdutka
 adding new function removeAllRows to ahfits file

 Revision 1.37  2015/06/29 14:40:33  mwitthoe
 ahfits: add new function (ahfits::move) to allow moving to a FITS extension by number; the new function is an overload of ahfits::move where the argument is the extension name

 Revision 1.36  2014/12/15 22:28:05  mdutka
 updated exception for issue #341 to occur as soon as the colmuns are connected

 Revision 1.35  2014/12/12 22:47:11  mdutka
 Exception is thrown if buffer is set and crosstypes with nulls are connected (see issue #341)

 Revision 1.34  2014/11/03 20:48:21  mwitthoe
 ahfits: add functions, setTZero() & setTScale(), in ahfits_colinfo for setting additional column properties; related to issue 459

 Revision 1.33  2014/08/20 20:40:31  mwitthoe
 ahfits: add extended syntax support for ahfits::open() and ahfits::clone(); see issue 179

 Revision 1.32  2014/05/06 22:04:54  mwitthoe
 ahfits: add unit tests to check different buffer parameters when editing or appending rows to a cloned or copied file; see issue 368

 Revision 1.31  2014/03/27 15:06:49  mwitthoe
 ahfits: add unit tests to test buffering when creating a file or editing an entire file

 Revision 1.30  2014/02/14 17:07:24  asargent
 Updated unit tests for ahfits image reading/writing

 Revision 1.29  2014/01/06 16:29:28  asargent
 Added new function HDUExists to check whether the HDU exists or not

 Revision 1.28  2013/12/10 22:08:17  mwitthoe
 ahfits: add functions to copy keywords from one HDU to another, e.g. copyKeyDbl()

 Revision 1.27  2013/10/16 01:14:31  mwitthoe
 ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

 Revision 1.26  2013/10/10 17:05:50  peachey
 Internal refactoring of connect_generic and functions that call it.
 Connections to scalar/vector client data are now indicated by a new
 argument (bool scalar) rather than keying off the sign of the out_type
 variable. Changed all functions that call connect_generic to use the
 new signature.

 Added a test to confirm connecting to vectors of strings leads to
 an exception being thrown. Moved the check for this case from the
 Connection class into the connect_generic function.

 Revision 1.25  2013/09/26 13:49:28  klrutkow
 renamed ut_addtnulltunit() to ut_addtnulltunitdisp(); added a new test ut_addtnullreloadcol() for testing that connect() can read the nulls after creating a new TNULL keyword (issue 293)

 Revision 1.24  2013/09/11 20:01:33  klrutkow
 finished creating new overloaded connect functions.  now we have
 connectScalar(), connectFixedLengthArray(), and connectVariableLengthArray(),
 all of which are overloaded to accept different data types (float, double, etc)
 and to use the same functions for the null flag (dnull_flag is now default set
 to NULL). Also, added \param tags that were missing.  Changed the ut to call
 the new functions rather than the old connect() functions.

 Revision 1.23  2013/09/10 20:41:32  klrutkow
 added firstHDU function, and tests

 Revision 1.22  2013/09/10 14:10:35  mwitthoe
 ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

 Revision 1.21  2013/07/20 04:11:08  mwitthoe
 add tests to check behavior of WRITEONLY with buffering enabled and disabled

 Revision 1.20  2013/07/16 20:10:06  mwitthoe
 ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

 Revision 1.19  2013/07/10 02:11:22  mwitthoe
 ahfits: add function to set the TUNIT value for a column: setTUnit()

 Revision 1.18  2013/07/09 17:41:20  rshill
 Added test for creation of a FITS binary table from scratch.

 Revision 1.17  2013/07/08 18:25:39  mwitthoe
 ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

 Revision 1.16  2013/06/20 21:10:14  mwitthoe
 fix bug in ahfits where READ/WRITEONLY state was being ignore for variable-length FITS columns; add test cases for each in ut_ahfits_row

 Revision 1.15  2013/06/17 15:54:34  mwitthoe
 ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

 Revision 1.14  2013/05/13 20:17:42  mwitthoe
 remove buffer unit test from testahfits since it doesn't really do anything and the buffer is being tested by the existing reading/writing unit tests

 Revision 1.13  2013/05/13 19:45:51  mwitthoe
 add buffering to ahfits (no TBIT support yet)

 Revision 1.12  2013/04/10 19:56:19  mwitthoe
 add new function, atLastRow(), to ahfits_row which will return true if currently at last row in active extension of a FITS file

 Revision 1.11  2013/03/05 23:01:49  rshill
 Extensive tests of TNULL and NaN capability.

 Revision 1.10  2013/03/04 20:28:46  rshill
 Progressing through addition of tests.

 Revision 1.9  2013/03/02 00:51:57  rshill
 Added test for undefined numeric values (TNULL or NaN).

 Revision 1.8  2013/02/27 21:02:10  peachey
 Add ut_buffer for testing buffering.

 Revision 1.7  2013/01/09 22:06:00  mwitthoe
 add insert column functions to ahfits: insertColBefore() and insertColAfter(); also add function, reloadColInfo(), needed to reset loaded column indices after adding a column

 Revision 1.6  2012/12/28 20:51:15  peachey
 Add and test support for bit-field type columns (type X).

 Revision 1.5  2012/12/26 15:56:56  mwitthoe
 ahfits: add insert and delete row functions needed for ahgti

 Revision 1.4  2012/11/29 02:59:20  peachey
 Add and call unit test of file list (@file) facilities.

 Revision 1.3  2012/10/24 15:00:07  mwitthoe
 ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

 Revision 1.2  2012/10/18 14:38:19  mwitthoe
 fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

 Revision 1.1  2012/10/10 20:30:56  mwitthoe
 ahfits: complete overloaded connection functions; add test code


*/
