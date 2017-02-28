/// \file cams2detlib.h
/// \brief Declares the task-specific functions for the cams2det task.
/// \author Timothy Reichard
/// \date $Date: 2015/07/31 20:50:25 $
///
/// This header file declares the task-specific enumerations and functions needed for use in the 
/// cams2det task.  The functions are defined in functions.cxx.

#ifndef TASKS_CAMS2DET_CAMS2DETLIB_H
#define TASKS_CAMS2DET_CAMS2DETLIB_H(arg) const char arg##TASKS_CAMS2DET_CAMS2DETLIB_rcsId_svnId[] = "$Version$";

#include "ahgen/ahversion.h"
AHVERSION(TASKS_CAMS2DET_CAMS2DETLIB,"$Id: cams2detlib.h,v 1.8 2015/07/31 20:50:25 rshill Exp $")

#include "ahmission/hxiteldef.h"
#include "ahmission/camsteldef.h"
#include "ahmission/camsio.h"
#include "teldef2.h"

#include "ahlog/ahlog.h"
#include "ahfits/ahfits.h"

#include <limits>
#include <cmath>
#include <string>
#include <vector>

/** \addtogroup tool_cams2det
 * @{
 */

// ======================================================================

/// Null values 
const double DOUBLENULL = std::numeric_limits<double>::quiet_NaN(); ///< Double-precision null value
const int INTNULL = 32767; ///< Integer position and jump null value
const int QNULL = 32767;   ///< Quality flag null value

/// \brief Flags regarding calculation problems for the CAMS displacement.
/// 
/// These bit flags are used in the value of the output file's CALCQUALITY field 
/// to note any problems in the DELTARAWX, DELTARAWY, COSANGLE, and SINANGLE calculation,
/// such as when data aren't available from one of the CAMS units, or COSANGLE or SINANGLE
/// were set in a non-standard way.  It has been decided to flag the three "fixed" angle flags
/// with the same value.
enum CalcQualityFlagsEnum 
  {
    e_QF_OK = 0x0,               ///< No problem with angle calculation.
    e_QF_NO_ANGLE = 0x1,         ///< Angle not calculated because one or both CAMS units had no good data.
    e_QF_FIXED_POS_COSINE = 0x2, ///< cos_angle > 1 initially, so cos_angle = 1 and sin_angle = 0 were set.
    e_QF_FIXED_NEG_COSINE = 0x2, ///< cos_angle < -1 initially, so cos_angle = -1 and sin_angle = 0 were set. 
    e_QF_FIXED_SINE = 0x2,       ///< Division by zero initially for sin_angle, so sin_angle = 1 was set.
    e_QF_NO_DATA = 0x4           ///< No data from either CAMS unit
  };

/// \brief CALC_QUALITY flag descriptions for output header
const std::string CALC_QUALITY_DESC = 
  std::string("CALC_QUALITY Bit Flags: \n")
  + "  0x0 = Successful calculation of offsets and twist angle\n"
  + "  0x1 = Twist angle not calculated\n"
  + "  0x2 = Tiny twist angle fixed to cos(twist) = +-1, sin(twist) = 0\n"
  + "  0x4 = No calculation of offsets and twist angle";

/// \brief Notes which CAMS units did not have good data for a certain timestamp.
///
/// This flag equals the sum of the CAMS unit numbers of CAMS units
/// not having good data. The range is 0 to 3.
enum BadCAMSDataEnum 
  {
    e_CAMS12_OK = 0x0,  ///< Both CAMS units provided good data.
    e_CAMS1_BAD = 0x1,  ///< Only CAMS2 provided good data.
    e_CAMS2_BAD = 0x2,  ///< Only CAMS1 provided good data.
    e_CAMS12_BAD = 0x3  ///< Neither CAMS unit provided good data.
  };

/// \brief BAD_UNITS flag descriptions for output header
const std::string BAD_UNITS_DESC = 
  std::string( "BAD_UNITS Bit Flags: \n")
  + "  0x0 = Non-null data from both CAMS1 and CAMS2\n"
  + "  0x1 = Null data from CAMS1\n"
  + "  0x2 = Null data from CAMS2\n";



// ======================================================================

/// \brief Input and derived parameters used in the cams2det task.
///
/// Params is a structure storing the input parameters and various related/derived
/// parameters needed in the cams2det task.
struct Params 
{
  std::string m_cams1_teldef_file; ///< CAMS1 TelDef filename
  std::string m_cams2_teldef_file; ///< CAMS2 TelDef filename
  std::string m_hxi_teldef_file;   ///< HXI TelDef filename
  std::string m_in_file1_as_input;   ///< CAMS1 position data filename
  std::string m_in_file2_as_input;   ///< CAMS2 position data filename
  std::string m_out_file_as_input;   ///< Output offsets filename
  std::string m_in_file1;          ///< CAMS1 filename without additions in []
  std::string m_in_file2;          ///< CAMS2 filename without additions in []
  std::string m_out_file;          ///< Output filename without additions in []
  std::string m_temp_out_file;     ///< Temporary output filename
  
  std::string m_instrume;          ///< HXI Instrument name
  int m_hxi_unit_number;           ///< HXI Detector number 
  std::string m_in_data_ext;       ///< Input position data extension
  std::string m_out_data_ext;      ///< Output offsets data extension
  bool m_use_cams1_file;           ///< Use the CAMS1 position data file?
  bool m_use_cams2_file;           ///< Use the CAMS2 position data file?
  double m_t_tol;                  ///< Tolerance for matching times
  int m_flip_output_sign;          ///< Flip the sign of output angles and offsets?
  std::string m_starting_sys;      ///< Starting coordinate system (RAW or FOC)

  ahfits::FilePtr m_fp_out;        ///< Output offsets file pointer
  
  ahfits::FilePtr m_fp_cams1;      ///< Input CAMS1 position file pointer
  long m_cams1_n_rows;             ///< Number of rows in CAMS1 position data

  ahfits::FilePtr m_fp_cams2;      ///< Input CAMS1 position file pointer
  long m_cams2_n_rows;             ///< Number of rows in CAMS2 position data
  
  bool m_clobber;                  ///< Should output file be clobbered?
  bool m_debug;                    ///< Show debug output?
};


/// Stores relative positions and angles associated with the locations of the CAMS and HXI units.
struct CAMSHXIGeometry
{
  /// \brief Initialize the geometry structure by calculating its
  /// members' values from the TelDef and parameters structures.
  ///
  /// Initialize the geometry structure by calculating its members'
  /// values from the members of the CAMS1, CAMS2, standard HXI, and extra
  /// HXI TelDef structures and from the parameters structures.
  CAMSHXIGeometry
  (const ahmission::teldef::CAMSTelDef& cams1,    ///< [in] TelDef structure for CAMS1
   const ahmission::teldef::CAMSTelDef& cams2,    ///< [in] TelDef structure for CAMS2
   const ahmission::teldef::HXITelDef& hxi_extra, ///< [in] Extra TelDef structure for HXI
   TELDEF2* hxi_std,           ///< [in] Standard TelDef structure for HXI
   const Params& param         ///< [in] Parameters structure
   );

  /// \brief Print the structure contents to a stream
  ///
  /// Print the member names and values to a stream.  Useful for debugging purposes.
  /// \return Stream containing the members
  friend st_stream::OStream& operator<<( 
    st_stream::OStream& str,             ///< [in] Input stream 
    const CAMSHXIGeometry& g             ///< [in] Geometry structure
  );
  
  double m_cos_cams1;   ///< Cosine of the rotation angle for the CAMS1 unit
  double m_sin_cams1;   ///< Sine of the rotation angle for the CAMS1 unit
  double m_cos_cams2;   ///< Cosine of the rotation angle for the CAMS2 unit
  double m_sin_cams2;   ///< Sine of the rotation angle for the CAMS2 unit
  double m_cos_hxi;     ///< Cosine of the rotation angle for the HXI unit
  double m_sin_hxi;     ///< Sine of the rotation angle for the HXI unit
  double m_cos_foc;     ///< Cosine of the rotation angle for the FOC coordinate system
  double m_sin_foc;     ///< Sine of the rotation angle for the FOC coordinate system
  double m_raw_xcen;    ///< X coord. of center of RAW coord. sys.
  double m_raw_ycen;    ///< Y coord. of center of RAW coord. sys.
  double m_raw_xhxi;    ///< X coord. of center of HXI unit
  double m_raw_yhxi;    ///< Y coord. of center of HXI unit
  double m_r12x;        ///< X distance between the CAMS units 
  double m_r12y;        ///< Y distance between the CAMS units
  double m_x_offset_px;    ///< X offset from HXI center to the center of RAW coord. sys. (px)
  double m_y_offset_px;    ///< Y offset from HXI center to the center of RAW coord. sys. (px)
  double m_x_offset_mm_raw; ///< X offset from HXI center to the center of RAW coord. sys. (mm)
  double m_y_offset_mm_raw; ///< Y offset from HXI center to the center of RAW coord. sys. (mm)
  double m_x_offset_mm; ///< X offset from HXI to center of satellite coord. sys.
  double m_y_offset_mm; ///< Y offset from HXI to center of satellite coord. sys.
  double m_x_cam_hxi;   ///< X distance from center of the two CAMS to the center of the HXI RAW system
  double m_y_cam_hxi;   ///< Y distance from center of the two CAMS to the center of the HXI RAW system
};

/// \brief Stores one row of CAMS offsets data.
///
/// Stores one row of CAMS offsets data and includes methods to
/// manipulate the members in bulk and to connect the columns to file
/// columns.
struct CAMSOffsetsDataRow
{
  /// \brief Connect the structure members to the columns of the offsets FITS file.
  ///
  /// Connect each structure member to the corresponding column of an
  /// offsets FITS file in a certain read/write mode.
  /// \return Pointer to a FITS i/o router
  ahfits::Router* connectColumns(  
    ahfits::FilePtr& fp,           ///< [in,out] File pointer
    const ahfits::RWModeEnum mode  ///< [in] Read/write mode
  );

  /// \brief Set the members to null/default values.
  ///
  /// Sets the member to null values based on type, m_bad_units to e_CAMS12_OK, and calc_quality to 0.
  void setRowToNull();

  /// \brief Fill the position and quality members associated with one CAMS unit.
  ///
  /// Copies the x, y, and quality values from data to the correct members for the CAMS unit number.
  void fillWithCAMSUnitData(
    const ahmission::camsio::UnitDataRow& data, ///< [in] One row of data from the CAMS unit data files
    const int unit               ///< [in] CAMS unit number (1 or 2)
  );

  double m_t;          ///< Time
  double m_dx;         ///< X offset in HXI RAW pixels
  double m_dy;         ///< Y offset in HXI RAW pixels
  double m_cos_twist;  ///< Cosine of twist angle
  double m_sin_twist;  ///< Sine of twist angle
  long m_x1;            ///< X bit from CAMS1 data
  long m_y1;            ///< Y bit from CAMS1 data
  long m_x2;            ///< X bit from CAMS2 data
  long m_y2;            ///< Y bit from CAMS2 data
  long m_jump_x1;       ///< Jump in x1 since previous non-null value
  long m_jump_y1;       ///< Jump in y1 since previous non-null value
  long m_jump_x2;       ///< Jump in x2 since previous non-null value
  long m_jump_y2;       ///< Jump in y2 since previous non-null value
  long m_q1;            ///< Quality flag for CAMS1 input data
  long m_q2;            ///< Quality flag for CAMS2 input data
  double m_x_dist;     ///< X distance between CAMS units
  double m_y_dist;     ///< Y distance between CAMS units
  double m_dx_sat;     ///< X offset in satellite coord. sys.
  double m_dy_sat;     ///< Y offset in satellite coord. sys.
  long m_bad_units; ///< Bit flags for CAMS units with bad/missing data
  long m_calc_quality; ///< Bit flags for calculation issues
};

// ======================================================================

/// \brief Create a blank output CAMS offset file.
///
/// Add columns to output CAMS offset file.
void addCAMSOffsetsFileColumns(
  ahfits::FilePtr& fp          ///< [in] Output file pointer
);

/// \brief Calculate the jump in consecutive good CAMS X or Y position values.
///
/// Calculate the jump in a position according to a few cases.  
/// If the current value is null, return null.
/// If the last good value is null, return null.
/// Otherwise, return the difference between the current and previous good values.
/// \return Jump in position
int calcPositionJump(        
  const int value,           ///< [in] Current position value
  const int last_good_value, ///< [in] Previous good position value
  const int null_value       ///< [in] Value to set for bad position value
);

/// \brief Calculate the output file data from the input file data.
///
/// Calculate the CAMS displacements (position and angle) in units of
/// the HXI RAW coordinate system pixels and the calculated distance
/// between the CAMS units. Also return flags noting which CAMS units
/// did not provide good data and any computational problems
/// encountered in the displacement calculations. 
void calcCAMSDisplacements(
  CAMSOffsetsDataRow& off,     ///< [in,out] Structure storing output row data
  double& last_good_cos_angle, ///< [out] Previous good cosine of twist angle correction
  double& last_good_sin_angle, ///< [out] Previous good sine of twist angle correction
  const CAMSHXIGeometry geom,  ///< [in] Structure of CAMS-HXI geometry
  const ahmission::teldef::CAMSTelDef& cams1,     ///< [in] Structure holding CAMS1 TelDef keyword values.
  const ahmission::teldef::CAMSTelDef& cams2,     ///< [in] Structure holding CAMS2 TelDef keyword values.
  const ahmission::teldef::HXITelDef& hxi_extra,  ///< [in] HXITelDef structure holding non-standard TelDef keyword values for the HXI unit.
  TELDEF2* hxi_std,            ///< [in] Pointer to TELDEF2 structure holding standard TelDef keyword values for the HXI unit.
  const bool start_from_raw,   ///< [in] Flag true if starting coordinate system is RAW, false if FOC.
  const int raw_sys,           ///< [in] HXI RAW coordinate system number.
  const int raw_min_seg,       ///< [in] Minimum segment number in HXI RAW coordinate systems.
  const int foc_sys,           ///< [in] HXI FOC coordinate system number.
  const int foc_min_seg        ///< [in] Minimum segment number in HXI FOC coordinate systems.
);

/// \brief Open and prepare a CAMS position FITS file for reading.
///
/// This function opens the CAMS position file, moves to the data
/// extension, and checks that the file is intended for an Astro-H
/// CAMS unit. Next, the function finds the required columns: TIME, X,
/// Y, and QUALITY.  Finally, the function determines how many rows are
/// in the table.
void openCAMSDataExtension(
  ahfits::FilePtr& fp,          ///< [out] File pointer
  const std::string& filename,  ///< [in] Name of file
  const std::string& ext_name,  ///< [in] Name of data extension
  const std::string& unit_name, ///< [in] Name of CAMS unit (CAMS1 or CAMS2)
  long& n_rows                  ///< [out] Number of rows in data table
);

/// \brief Check if a floating-point value is null.
///
/// Give a boolean answer to the question "Is t's value equivalent to DOUBLENULL?"
/// \return True if t is null, false if t is non-null
bool isDoubleNull( 
  const double t     ///< [in] Value to check
);

/** @} */

#endif

/* Revision Log
   $Log: cams2detlib.h,v $
   Revision 1.8  2015/07/31 20:50:25  rshill
   Include temperature-corrected CAMS data columns.

   Revision 1.7  2015/07/31 00:00:14  rshill
   Code cleanup; added proc_status processing; added more counters; added descriptions and comments in output FITS header.

   Revision 1.6  2015/07/30 00:50:15  rshill
   Incorporated keyword copying.

   Revision 1.5  2015/07/29 21:37:37  rshill
   Changed all detnam to instrume.

   Revision 1.4  2015/04/01 00:04:16  rshill
   Put in accounting for height difference between CAMS and HXI.  Allow starting coord system to be either RAW or FOC.

   Revision 1.3  2014/01/27 15:56:54  treichar
   Changed bit flag descriptions to single string, using the new ahfits::writeKeyComment function that handles newline and spaces for line breaks

   Revision 1.2  2014/01/10 20:41:35  treichar
   Coupled the null-ness of X and Y values and corresponding jumps according to the prescribed algorithm change.

   Revision 1.1  2014/01/07 19:34:11  treichar
   Moved hxiteldef.{h,cxx} and camsteldef.{h,cxx} to the ahmission library.  Moved the functions for reading a CAMS unit data file out of functions.{h,cxx} to the ahmission
   library.  Reorganized the remaining functions in functions.{h,cxx} and params.h into cams2detlib.{h,cxx}.  Updated the standard main function.

   Revision 1.16  2014/01/02 19:17:39  treichar
   Changed the time-matching algorithm to a more robust one that does not rely on matching file times to a nominal time determined by the regularly spaced time offsets in CALDB. The
   new algorithm scans the times in each of the two input files, combines the input data rows as a single output row when the two times match within a tolerance, and keeps the
   input data rows in separate output rows when the two times are too widely spaced.

   Revision 1.15  2013/12/27 20:23:05  treichar
   Grouped various quantities into structures for simpler function calls and better organization.

   Revision 1.14  2013/08/02 20:04:55  treichar
   Updated the position offsets and twist angle calculation according to the corrections devised by Hans Krimm and Casey Lambert.  Changed default value of flipoutputsign to no, and this parameter may be removed in
   the future.

   Revision 1.13  2013/07/25 20:18:32  treichar
   Added missing #include <limits>.

   Revision 1.12  2013/07/25 19:18:39  treichar
   Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
   throughout code.

   Revision 1.11  2013/07/24 15:37:38  treichar
   Converted the reading of CAMS data files and writing of the output from CFITSIO calls to ahfits calls.

   Revision 1.10  2013/04/18 19:56:35  treichar
   Updated camstoffset library calls due to its overhaul/move to the ahmission library.  Changed output column names DELTADETX, DELTADETY to DELTARAWX, DELTARAWY.  Changed default output file extension from
   CAMS_DISPLACEMENT to CAMS_OFFSETS.  Changed form of main() function to new standard.

   Revision 1.9  2013/01/16 20:00:10  treichar
   Updated reading of input X, Y, and QUALITY values, which are now expected to be signed ints rather than unsigned ints.  Updated integer
   null values from 65535 to 32767 to match.  Removed unneeded precheck command-line parameter.

   Revision 1.8  2012/12/28 21:15:47  treichar
   Added DELTASATX, DELTASATY columns to output file.  Added these values also as debug=yes output.  Allow t >= 0 instead of only t > 0 times in processing.  Fixed bug where tool gives up if
   infile1=NONE.

   Revision 1.7  2012/12/03 04:13:10  treichar
   Fixed some typos in doxygen comments.

   Revision 1.6  2012/11/15 20:58:45  treichar
   Updated subsecond timing to use new CAMS time offset CALDB file.

   Revision 1.5  2012/11/13 20:35:44  treichar
   Updated calls to printTelDef2.

   Revision 1.4  2012/09/20 21:19:00  treichar
   Minor tweaks and some clearer comments.

   Revision 1.3  2012/09/17 15:04:47  treichar
   Updated to for compliance with ahversion.

   Revision 1.2  2012/09/12 20:46:24  treichar
   More development.  Added doxygen comments to source code.

   Revision 1.1  2012/09/07 20:51:03  treichar
   Initial version of new tool, cams2det, which converts bit-formatted extended optical bench positions detected by the Astro-H CAMS units into HXI RAW pixel offsets.

*/
