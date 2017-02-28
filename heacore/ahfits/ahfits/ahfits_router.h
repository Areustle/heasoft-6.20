/// \file ahfits_router.h
/// \brief ahfits: Handles connections from local scope to FITS columns
/// \author James Peachey
/// \date $Date: 2014/05/13 14:52:36 $

/// \addtogroup mod_ahfits
/// \section ahfits_router Router Library - ahfits_router
///
/// This library contains a class which stores a set of connections between 
/// local variables and FITS columns for a single HDU in a FITS file.  The 
/// purpose of the class is to make reading row data easy and safe (i.e. no
/// memory leaks).
///
/// To use, simply create an instance of the router using a FITS FilePtr as the
/// sole argument.  The router constructor has an optional, second argument 
/// specifying the buffer overlap (default value: 0).  See the Buffer 
/// documentation for more information.  Then use the connect functions which
/// take the following arguments:
///  - read/write state of connection: ahfits::e_READONLY, ahfits::e_READWRITE, or ahfits::e_WRITEONLY
///  - the name of the column to connect to
///  - the local variable which will store the row value(s) of the given column
///  - (variable-length array only) local variable holding the array size
///  - (optional) local variable flag indicating if the value is NULL or not
/// The connect functions are overloaded based on the data type of the local
/// variable, but separate functions exist for connecting to scalar, 
/// fixed-length array, or variable-lengths array columns.
///
/// There is no need to disconnect the router from the local variables; this 
/// will be automatically done when either the router goes out of scope or the 
/// FITS file is closed.
///
/// Example:
///
/// \code
/// // open FITS file
/// std::string filename="test.fits";
/// ahfits::FilePtr fp;
/// ahfits::open(filename,"",&fp)     // 2nd argument implies open to primary HDU
/// ahfits::firstHDU(ahfits::e_BINARY_TBL)   // go to first binary table
///
/// // declare some local variables
/// int v_segment=0;
/// double v_stime=0.;
/// double v_phas[20];
/// ahfits::IndexType phas_count=0;             // to hold size of PHAS column as read from FITS file
///
/// // define router and make connections
/// ahfits::Router router(fp);
/// router.connectScalar(ahfits::e_READONLY,"SEGMENT",v_segment);
/// router.connectScalar(ahfits::e_READONLY,"S_TIME",v_stime);
/// router.connectVariableLengthArray(ahfits::e_READONLY,"PHAS",v_phas,phas_count);   // variable-length column
///
/// // read 1st row and move to next row
/// ahfits::readRow(fp);                        // now local variables contain 1st row values
/// ahfits::nextRow(fp);
///
/// // close FITS file
/// ahfits::close(fp);
/// \endcode
///

#ifndef AHFITS_AHFITS_ROUTER_H
#define AHFITS_AHFITS_ROUTER_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_ROUTER,"$Id: ahfits_router.h,v 1.36 2014/05/13 14:52:36 mwitthoe Exp $")

#include "ahfits/ahfits_base.h"
#include "ahfits/ahfits_connect.h"

#include "fitsio.h"

#include <map>
#include <string>
#include <vector>

/// \ingroup mod_ahfits
namespace ahfits {

/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief Class which manages connections of local data to
///   ahfits::FilePtr objects.
class Router {
public:

  typedef std::map<std::string, connect::Connection*> ConnectionContType;
  typedef ConnectionContType::iterator ConnectionIteratorType;

  /// \brief Create a router object to handle connections to the
  ///   provided ahfits::FilePtr object.
  /// \param[in] ahffp The FITS file object that will be connected to the data.
  /// \param[in] overlap number of rows to keep before current row when filling buffer
  Router(FilePtr ahffp);

  /// \brief Create a router object to handle connections to the
  ///   provided ahfits::FilePtr object.
  /// \param[in] ahffp The FITS file object that will be connected to the data.
  /// \param[in] overlap number of rows to keep before current row when filling buffer
  Router(FilePtr ahffp, int overlap);

  /// \brief Destructor frees all allocated memory and disconnect
  ///   all connected variables. This way clean-up is automatic when
  /// the router goes out of scope.
  ~Router();

  /// \brief For testing/debugging: return router index
  int index();

  /// \brief return the overlap size
  /// \return overlap size
  int getOverlap(void);

  /// \brief General connection routine used by overloaded connect() functions
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in] rwmode Selects read-only, read-write, or write-only mode.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count pointer to variable with size of data for variable-length 
  ///            columns; set to NULL for scalar data
  /// \param[in] scalar If true, require FITS data to be scalar in the column.
  /// \param[in] out_type requested output type as a cfitsio constant; e.g. TINT
  /// \param[in,out] dnull_flags Pointer to flags for undefined values
  ///
  /// For variable-length columns, data_count is filled with the size of the
  /// column; this can be used to check against the size of the local array
  void connect_generic(const RWModeEnum rwmode, const std::string & colname,  
                       void* data, IndexType* data_count, bool scalar, int out_type, char* dnull_flags);







  //
  //  Scalar
  //

  /// \brief Connect a scalar char to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, char & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar unsigned char to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, unsigned char & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar bool to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, bool & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar short to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, short & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar long to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, long & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar long long to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, long long & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar int to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, int & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar unsigned int to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, unsigned int & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar unsigned short to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, unsigned short & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar unsigned long to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, unsigned long & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar unsigned long long to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, unsigned long long & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar float to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, float & data,
                     char* dnull_flags=0);

  /// \brief Connect a scalar double to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, double & data,
                     char* dnull_flags=0);
                     
  /// \brief Connect a scalar string to the given column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  void connectScalar(const RWModeEnum rwmode, const std::string & colname, std::string & data,
                     char* dnull_flags=0);


//
//  Fixed Length Arrays
//

  /// \brief Connect an array of chars to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, char* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of unsigned chars to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned char* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of booleans to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, bool* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of shorts to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, short* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of longs to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, long* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of long longs to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, long long* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of ints to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, int* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of unsigned ints to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned int* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of unsigned shorts to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned short* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of unsigned longss to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned long* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of unsigned long longs to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned long long* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of floats to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, float* data,
                    char* dnull_flags=0);

  /// \brief Connect an array of doubles to the given fixed-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values).
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, double* data,
                    char* dnull_flags=0);


//
//  Variable Length Arrays
//

  /// \brief Connect an array of chars to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, char* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of unsigned chars to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned char* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of booleans to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, bool* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of shorts to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, short* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of longs to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, long* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of long longs to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, long long* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of ints to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, int* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of unsigned ints to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned int* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of unsigned shorts to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned short* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of unsigned longss to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned long* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of unsigned long longs to the given variable-length column
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, unsigned long long* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of floats to the given variable-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, float* data, 
                     IndexType & data_count, char* dnull_flags=0);

  /// \brief Connect an array of doubles to the given variable-length column.
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data Pointer to data variable.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, double* data, 
                     IndexType & data_count, char* dnull_flags=0);

  
//
//  Bit
//

  /// \brief Connect an array of chars to the given variable-length bit column (FITS type X).
  /// \param[in] rwmode Read/write mode.
  /// \param[in] colname The name of the column to associate with the data.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] data_count Reference to variable to return number of items read.
  /// \param[in,out] dnull_flags (optional) Flag for null values (default: do not check for NULL data values)
  ///
  /// Upon output, data_count will be filled with the size of the column; this
  /// can be used to check against the size of the local array
  void connectBit(const RWModeEnum rwmode, const std::string & colname, char* data, 
                       IndexType & data_count);


  /// \brief Disconnect the given data element. Note the *address* of the 
  ///  element previously passed to connect must be passed here. This is 
  ///  normally called just by the Router destructor.
  /// \param[in] data_ptr Address of the data to be disconnected.
  ///
  /// Note that for this function, the type of the element is irrelevant,
  /// so we need only one disconnect method. This method also
  /// frees up any memory allocated previously for this variable by connect.
  void disconnect(void * data_ptr);

  /// \brief Disconnect this Router from its parent FilePtr. Sever all connections
  ///  made on behalf of this Router.
  void disconnect(void);

  /// \brief Remove all active connections
  void clearConnections(void);

  /// \brief Flush and clear buffers for all connections in router.
  void flushAndClearBuffers(void);

  ConnectionIteratorType connectionBegin();

  ConnectionIteratorType connectionEnd();

  size_t numConnection();

private:

  FilePtr m_ahffp;   ///< pointer to FITS file object
  int m_ridx;        ///< router index
  ConnectionContType m_connection;
  int m_overlap;     ///< number of rows to keep before current row when moving buffer region
  bool m_report_log;    ///< true if buffer size and overlap have been reported to log file 
  bool m_report_fits;   ///< true if buffer size and overlap have been reported to the FITS file 
};

/** @} */


} // namespace ahfits

#endif   /* AHFITS_AHFITS_ROUTER_H */

/* Revision Log
   $Log: ahfits_router.h,v $
   Revision 1.36  2014/05/13 14:52:36  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.35  2014/03/27 15:06:48  mwitthoe
   ahfits: add unit tests to test buffering when creating a file or editing an entire file

   Revision 1.34  2014/03/26 18:47:42  mwitthoe
   ahfits: remove old connect() functions; these have been replaced by, for example, connectScalar()

   Revision 1.33  2013/10/30 18:01:21  mwitthoe
   ahfits: update documentation according to Kristin's recommendations in update 3 of issue #289

   Revision 1.32  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.31  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.30  2013/10/10 17:05:50  peachey
   Internal refactoring of connect_generic and functions that call it.
   Connections to scalar/vector client data are now indicated by a new
   argument (bool scalar) rather than keying off the sign of the out_type
   variable. Changed all functions that call connect_generic to use the
   new signature.

   Added a test to confirm connecting to vectors of strings leads to
   an exception being thrown. Moved the check for this case from the
   Connection class into the connect_generic function.

   Revision 1.29  2013/10/04 21:20:46  mwitthoe
   ahfits library: previous version of writing buffer information to the FITS header would fail if a keyword was written to the output file before making a table connection; this has been fixed by moving the recording of buffer information to FITS files from stamp() to connect_generic()

   Revision 1.28  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.27  2013/10/03 23:27:45  klrutkow
   removed dnull_flags from argument to connectBit and just passed 0 to connectGeneral

   Revision 1.26  2013/09/11 20:17:47  klrutkow
   fixed typos in doxygen

   Revision 1.25  2013/09/11 20:01:33  klrutkow
   finished creating new overloaded connect functions.  now we have
   connectScalar(), connectFixedLengthArray(), and connectVariableLengthArray(),
   all of which are overloaded to accept different data types (float, double, etc)
   and to use the same functions for the null flag (dnull_flag is now default set
   to NULL). Also, added \param tags that were missing.  Changed the ut to call
   the new functions rather than the old connect() functions.

   Revision 1.24  2013/09/09 18:52:22  mwitthoe
   ahfits library: preliminary addition of new connect functions (non-null double scalar and array)

   Revision 1.23  2013/07/20 03:09:39  mwitthoe
   ahfits router: remove unnecessary typedef

   Revision 1.22  2013/07/18 15:54:35  mwitthoe
   ahfits: add ahfits_buffer.h to ahfits.h; the 2nd argument of the constructor had a default value which was messing up the building of tools (I do not know why), changed to an overloaded constructor for now

   Revision 1.21  2013/07/18 14:21:04  mwitthoe
   ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap

   Revision 1.20  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.19  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.18  2013/06/17 15:54:34  mwitthoe
   ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

   Revision 1.17  2013/05/14 14:17:32  mwitthoe
   ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging

   Revision 1.16  2013/03/18 23:45:53  rshill
   Column-specific readable/writeable flag added.

   Revision 1.15  2013/03/09 04:21:55  peachey
   Use local container of Connections rather than the ahfits_connect
   facilities.

   Revision 1.14  2013/03/05 22:59:51  rshill
   Edits of function declarations relevant to nulls.

   Revision 1.13  2013/03/02 00:56:04  rshill
   Added connect overloads for scalars with nulls.

   Revision 1.12  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.11  2013/01/24 15:56:28  mwitthoe
   update afits Doxygen

   Revision 1.10  2012/12/28 20:51:13  peachey
   Add and test support for bit-field type columns (type X).

   Revision 1.9  2012/10/23 21:45:56  mwitthoe
   add connections for unsigned short, long, and long long to the ahfits router

   Revision 1.8  2012/10/23 20:10:44  mwitthoe
   add connection for array of unsigned ints to ahfits router

   Revision 1.7  2012/10/18 20:12:57  mwitthoe
   add router connection for unsigned long long scalar variables

   Revision 1.6  2012/10/18 19:26:58  mwitthoe
   added router connection for unsigned char to ahfits

   Revision 1.5  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.4  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.3  2012/10/12 22:50:00  mwitthoe
   ahfits: use pass-by-reference in scalar connections; add columnLength() function

   Revision 1.2  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.1  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object


*/
