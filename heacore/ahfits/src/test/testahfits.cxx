/// \brief Unit test of ahfits library.
/// \author James Peachey
/// \date $Date: 2015/10/09 21:09:22 $

#define AHLABEL test_testahfits
#define AHCVSID "$Id: testahfits.cxx,v 1.57 2015/10/09 21:09:22 mdutka Exp $"

#include "./testahfits.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

// Variable to hold banner (reused throughout the test).
// to be removed...
static std::string s_banner;

int main(int argc, char ** argv) {

  bool debug=false;
  ahlog::setup("testahfits", "!DEFAULT", 1, debug);

  ut_open_close();
  ut_open_extended();
  ut_create_clone();
  ut_create_ab_initio();
  ut_move_HDU_number();
  ut_loop_HDU();
  ut_firstHDU();
  ut_nextPrimaryHDU();
  ut_nextBinaryTbl();
  ut_clone1HDU();
  ut_insert_delete_rows();
  ut_removeAllRows();
  ut_write_read_image_dbl();
  ut_write_read_image_lng();
  ut_write_read_image_flt();
  ut_write_read_image_shr();
  ut_write_read_image_byt();

  ut_colinfo();
  ut_addcolumn();
  ut_addtnulltunittdisp();
  ut_addtzerotscale();
  ut_addtnullreloadcol();
  ut_read_header();
  ut_write_header();
  ut_copy_header();
  ut_checklast();
  ut_read_row();
  ut_duplicate();

  ut_outside_router();
  ut_clear_connections();
  ut_file_list();

  ut_writemiddle_buffer();
  ut_writemiddle_nobuffer();

  ut_connect_crosstype_nulls();           // connecting conflicting null types

  ut_router_to_primary();

  // in ut_ahfits_buffer
  ut_write_new_file_scalar(0);             // buffering disabled
  ut_write_new_file_scalar(-1);            // automatic buffering
  ut_write_new_file_scalar(7);             // manual buffer < number of rows
  ut_write_new_file_fixed_length(0);       // buffering disabled
  ut_write_new_file_fixed_length(-1);      // automatic buffering
  ut_write_new_file_fixed_length(19);      // manual buffer < number of rows
  ut_write_new_file_variable_length(0);    // buffering disabled
  ut_write_new_file_variable_length(-1);   // automatic buffering
  ut_write_new_file_variable_length(23);   // manual buffer < number of rows
  ut_write_new_file_multiple(0);           // buffering disabled
  ut_write_new_file_multiple(10);          // manual buffering
  ut_write_new_file_bit(0);                // buffering disabled
  ut_write_new_file_bit(-1);               // automatic buffering
  ut_clone_and_edit(0);                    // buffering disabled
  ut_clone_and_edit(7);                    // manual buffer < number of rows; numrow%buffer != 0
  ut_clone_and_edit(-1);                   // automatic buffering
  ut_open_and_edit(0);                     // buffering disabled
  ut_open_and_edit(7);                     // manual buffer < number of rows; numrow%buffer != 0
  ut_open_and_edit(-1);                    // automatic buffering
  ut_clone_and_append(0);                  // buffering disabled
  ut_clone_and_append(7);                  // manual buffer < number of rows; numrow%buffer != 0
  ut_clone_and_append(-1);                 // automatic buffering
  ut_open_and_append(0);                   // buffering disabled
  ut_open_and_append(7);                   // manual buffer < number of rows; numrow%buffer != 0
  ut_open_and_append(-1);                  // automatic buffering
   
  // repeat following tests using different buffering schemes
  std::vector<int> buffer_vals;
  buffer_vals.push_back(-1);        // cfitsio choose buffer size
  buffer_vals.push_back(0);         // buffering disabled
  buffer_vals.push_back(1);         // single row buffered (different algorithm than buffering disabled)
  buffer_vals.push_back(100);       // buffer 100 rows
  for (std::vector<int>::iterator it=buffer_vals.begin(); it != buffer_vals.end(); it++) {
    ahfits::setBuffer(*it);

    ut_read_char();
    ut_read_bool();
    ut_read_short();
    ut_read_long();
    ut_read_longlong();
    ut_read_float();
    ut_read_double();
    ut_read_string();

    ut_read_arr_chars();
    ut_read_arr_bools();
    ut_read_arr_shorts();
    ut_read_arr_longs();
    ut_read_arr_longlongs();
    ut_read_arr_floats();
    ut_read_arr_doubles();
    ut_read_arr_strings();
    ut_read_bits();
  
    ut_read_byte_null();
    ut_read_short_null();
    ut_read_long_null();
    ut_read_longlong_null();
    ut_read_float_null();
  
    ut_read_vec_bytes_null();
    ut_read_vec_shorts_null();
    ut_read_vec_longs_null();
    ut_read_vec_longlongs_null();
    ut_read_vec_floats_null();
    ut_read_vec_doubles_null();
  
    ut_read_var_bytes_null();
    ut_read_var_doubles_null();
  
    ut_check_readonly();
    ut_check_writeonly();
  
    ut_write_row();
    ut_write_row_nulls();
    
  }

  int status=ahgen::finalReport();
  ahlog::shutdown();

  return status;
}


/* Revision Log
   $Log: testahfits.cxx,v $
   Revision 1.57  2015/10/09 21:09:22  mdutka
   adding new function removeAllRows to ahfits file

   Revision 1.56  2015/06/29 14:40:34  mwitthoe
   ahfits: add new function (ahfits::move) to allow moving to a FITS extension by number; the new function is an overload of ahfits::move where the argument is the extension name

   Revision 1.55  2014/12/16 16:32:30  mdutka
   updated exception for issue #341 to occur as soon as the colmuns are connected

   Revision 1.54  2014/12/15 22:28:05  mdutka
   updated exception for issue #341 to occur as soon as the colmuns are connected

   Revision 1.53  2014/12/12 22:47:11  mdutka
   Exception is thrown if buffer is set and crosstypes with nulls are connected (see issue #341)

   Revision 1.52  2014/11/26 15:12:45  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.51  2014/11/03 20:48:21  mwitthoe
   ahfits: add functions, setTZero() & setTScale(), in ahfits_colinfo for setting additional column properties; related to issue 459

   Revision 1.50  2014/08/20 20:40:31  mwitthoe
   ahfits: add extended syntax support for ahfits::open() and ahfits::clone(); see issue 179

   Revision 1.49  2014/05/06 22:04:54  mwitthoe
   ahfits: add unit tests to check different buffer parameters when editing or appending rows to a cloned or copied file; see issue 368

   Revision 1.48  2014/03/27 15:06:49  mwitthoe
   ahfits: add unit tests to test buffering when creating a file or editing an entire file

   Revision 1.47  2014/02/14 17:07:24  asargent
   Updated unit tests for ahfits image reading/writing

   Revision 1.46  2014/01/06 16:29:28  asargent
   Added new function HDUExists to check whether the HDU exists or not

   Revision 1.45  2013/12/10 22:08:17  mwitthoe
   ahfits: add functions to copy keywords from one HDU to another, e.g. copyKeyDbl()

   Revision 1.44  2013/10/16 01:14:31  mwitthoe
   ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

   Revision 1.43  2013/10/10 17:05:50  peachey
   Internal refactoring of connect_generic and functions that call it.
   Connections to scalar/vector client data are now indicated by a new
   argument (bool scalar) rather than keying off the sign of the out_type
   variable. Changed all functions that call connect_generic to use the
   new signature.

   Added a test to confirm connecting to vectors of strings leads to
   an exception being thrown. Moved the check for this case from the
   Connection class into the connect_generic function.

   Revision 1.42  2013/09/26 13:49:28  klrutkow
   renamed ut_addtnulltunit() to ut_addtnulltunitdisp(); added a new test ut_addtnullreloadcol() for testing that connect() can read the nulls after creating a new TNULL keyword (issue 293)

   Revision 1.41  2013/09/11 20:01:33  klrutkow
   finished creating new overloaded connect functions.  now we have
   connectScalar(), connectFixedLengthArray(), and connectVariableLengthArray(),
   all of which are overloaded to accept different data types (float, double, etc)
   and to use the same functions for the null flag (dnull_flag is now default set
   to NULL). Also, added \param tags that were missing.  Changed the ut to call
   the new functions rather than the old connect() functions.

   Revision 1.40  2013/09/10 20:41:32  klrutkow
   added firstHDU function, and tests

   Revision 1.39  2013/09/10 14:10:35  mwitthoe
   ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

   Revision 1.38  2013/07/20 04:11:08  mwitthoe
   add tests to check behavior of WRITEONLY with buffering enabled and disabled

   Revision 1.37  2013/07/16 20:10:06  mwitthoe
   ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

   Revision 1.36  2013/07/10 02:11:22  mwitthoe
   ahfits: add function to set the TUNIT value for a column: setTUnit()

   Revision 1.35  2013/07/09 17:41:01  rshill
   Added test for creation of a binary table from scratch.

   Revision 1.33  2013/07/02 18:05:16  mwitthoe
   testahfits: loop read/write test functions over several buffering modes: -1 (auto), 0 (disabled), 1 (single row), 100 (100 rows)

   Revision 1.32  2013/06/20 21:10:14  mwitthoe
   fix bug in ahfits where READ/WRITEONLY state was being ignore for variable-length FITS columns; add test cases for each in ut_ahfits_row

   Revision 1.31  2013/06/17 15:54:34  mwitthoe
   ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

   Revision 1.30  2013/05/14 17:45:44  mwitthoe
   ahfits buffering: disable buffering to TBIT output types; fix bug causing wrong number of rows to be written for fixed-length types; fix bug where NULLs were not being written for fixed-length types

   Revision 1.29  2013/05/14 14:50:30  mwitthoe
   restore all working ahfits unit tests

   Revision 1.28  2013/05/14 14:17:32  mwitthoe
   ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging

   Revision 1.27  2013/05/13 20:17:42  mwitthoe
   remove buffer unit test from testahfits since it doesn't really do anything and the buffer is being tested by the existing reading/writing unit tests

   Revision 1.26  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.25  2013/04/10 19:56:19  mwitthoe
   add new function, atLastRow(), to ahfits_row which will return true if currently at last row in active extension of a FITS file

   Revision 1.24  2013/03/09 04:31:47  peachey
   Do not test ahfits_connect facilities.

   Revision 1.23  2013/03/05 23:01:49  rshill
   Extensive tests of TNULL and NaN capability.

   Revision 1.22  2013/03/04 20:28:46  rshill
   Progressing through addition of tests.

   Revision 1.21  2013/03/02 00:51:57  rshill
   Added test for undefined numeric values (TNULL or NaN).

   Revision 1.20  2013/02/27 21:02:10  peachey
   Add ut_buffer for testing buffering.

   Revision 1.19  2013/01/09 22:06:00  mwitthoe
   add insert column functions to ahfits: insertColBefore() and insertColAfter(); also add function, reloadColInfo(), needed to reset loaded column indices after adding a column

   Revision 1.18  2012/12/28 20:51:14  peachey
   Add and test support for bit-field type columns (type X).

   Revision 1.17  2012/12/26 15:56:56  mwitthoe
   ahfits: add insert and delete row functions needed for ahgti

   Revision 1.16  2012/11/29 02:59:20  peachey
   Add and call unit test of file list (@file) facilities.

   Revision 1.15  2012/10/24 15:00:07  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.14  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.13  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.12  2012/10/01 17:21:57  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:15:00  mwitthoe
   add test code for ahfits2

   Revision 1.11  2012/09/14 23:53:09  mwitthoe
   apply version standards to ahfits

   Revision 1.10  2012/08/24 22:07:15  mwitthoe
   add call to ahlog::shutdown in testahfits and testahmath

   Revision 1.9  2012/08/24 19:48:55  mwitthoe
   clean up argument lists in ahfits library; activate all tests in testahfits

   Revision 1.8  2012/08/23 21:27:32  mwitthoe
   conformed testahfits to new testing standard and added tests for missing ahfits functions

   Revision 1.7  2012/07/31 20:20:21  peachey
   Add test for accessing read-only files via ahFitsOpen.

   Revision 1.6  2012/07/27 18:31:13  mwitthoe
   change debug to false in testahfits.cxx to avoid stacktrace in test

   Revision 1.5  2012/05/17 19:35:11  peachey
   Add test code for reading and writing ascii columns.

   Revision 1.4  2012/04/13 17:30:29  peachey
   Use new ahlog macros (that use st_stream.

   Revision 1.3  2012/02/07 18:31:19  dhon
   checkin

   Revision 1.2  2012/02/02 20:54:55  peachey
   Use and test latest changes to ahfits, including tests for writing
   FITS files with ahFitsWriteRow.

   Revision 1.1  2012/01/31 22:21:29  peachey
   Add first version of fits support code.

*/
