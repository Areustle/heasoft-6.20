/// \file cams2det.cxx
/// \brief Calculate HXI offsets from CAMS data due to wobbling of Extended Optical Bench
/// \author Timothy Reichard
/// \date $Date: 2016/04/08 15:06:59 $
/// \version 1.0

/** 
 
\defgroup tool_cams2det Calculate HXI offsets due to Extended Optical Bench wobbling (cams2det)
@ingroup mod_mission_tasks

This tool converts CAMS displacement data to offsets in an HXI coordinate system (RAW or FOC).

Source files:

  cams2det.cxx
  cams2detlib.cxx
  cams2detlib.h

Library depenencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  attitude/lib/coordfits
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission

Modification history:
 1.0   2015-09-21  RSH     Initial implementation
    
*/

#define AHLABEL tool_cams2det
#define AHCVSID "$Id: cams2det.cxx,v 1.44 2016/04/08 15:06:59 rshill Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "cams2detlib.h"

#include "coordfits2.h"
#include "ahmission/keyword.h"
#include "ahmission/caldb.h"
#include "ahmission/ahmission.h"
#include "ahgen/ahversion.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"

#include "ape/ape_trad.h"
#include "ape/ape_error.h"

#include <cstdio>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

// ======================================================================
// Function declarations

/// \brief Read parameters and store related quantities
/// \param[in,out] param Parameter structure
void getPar(Params& param);

/// \brief Read TelDef files and open input and output data files.
/// \param[in,out] param Parameter structure
/// \param[out] cams1 CAMS1 TelDef structure
/// \param[out] cams2 CAMS2 TelDef structure
/// \param[out] hxi_extra HXI non-standard TelDef keywords
/// \param[out] hxi_std HXI standard TelDef structure
void initialize(Params& param, ahmission::teldef::CAMSTelDef& cams1, 
    ahmission::teldef::CAMSTelDef& cams2, 
    ahmission::teldef::HXITelDef& hxi_extra, TELDEF2*& hxi_std);

/// \brief Process the CAMS position data files and produce the output offsets file.
/// \param[in,out] param Parameter structure
/// \param[in] cams1 CAMS1 TelDef structure
/// \param[in] cams2 CAMS2 TelDef structure
/// \param[in] hxi_extra HXI non-standard TelDef keywords
/// \param[in] hxi_std HXI standard TelDef structure
void doWork(Params& param, const ahmission::teldef::CAMSTelDef& cams1, 
      const ahmission::teldef::CAMSTelDef& cams2, 
      const ahmission::teldef::HXITelDef& hxi_extra, TELDEF2*& hxi_std);

/// \brief Close files and free memory.
/// \param[in] param Parameter structure
/// \param[in,out] hxi_std HXI standard TelDef structure
void finalize(Params& param, TELDEF2*& hxi_std);

// ======================================================================
// Function definitions

int main(int argc, char** argv)
{
  // Declare the parameters and TelDef structures.

  Params param;
  ahmission::teldef::CAMSTelDef cams1;
  ahmission::teldef::CAMSTelDef cams2;
  ahmission::teldef::HXITelDef hxi_extra;
  TELDEF2* hxi_std = NULL;

  // Initialize the runtime status and run the task's pieces. 

  int status = ahapp::startUp(argc, argv, TOOLTAG);

  if(status == 0)
  {
    if(ahlog::get_debug())
    {
      getPar(param);
      ahapp::writeParametersToLog(); 
      initialize(param, cams1, cams2, hxi_extra, hxi_std);
      doWork(param, cams1, cams2, hxi_extra, hxi_std);
      finalize(param, hxi_std);
      ahapp::shutDown();
    }      
    else
    {
      try 
      {
        getPar(param);
        initialize(param, cams1, cams2, hxi_extra, hxi_std);
        doWork(param, cams1, cams2, hxi_extra, hxi_std);
      } 
      catch (const std::exception& x)
      {
        ahapp::writeParametersToLog(); 
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      
      try
      {
        finalize(param, hxi_std);
      }
      catch (const std::exception& x)
      {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      
      try
      {
        ahapp::shutDown();
      }
      catch (const std::exception& x)
      {
        status = 1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }  
  }
  else
    std::cerr << "Unable to start up tool." << std::endl;
  
  return status;
}

// ------------------------------------------------------------

void getPar(Params& param)
{
  std::string tmp="";  // Temporary string buffer
  size_t pos=0;  // Result of substring search

  AH_OUT << "Started cams2det." << std::endl;

  // Initialize file pointers for the two input and one output files.

  param.m_fp_cams1 = NULL;
  param.m_fp_cams2 = NULL;
  param.m_fp_out = NULL;
 
  // Get and check instrume parameter. It should refer to an HXI1 or 2 unit.

  param.m_instrume = ahgen::strtoupper(ahapp::getParString("instrume"));
  if(param.m_instrume == "1" || param.m_instrume == "2")
    param.m_instrume = "HXI" + param.m_instrume;
  
  if(param.m_instrume != "HXI1" && param.m_instrume != "HXI2")
    {
      AH_THROW_RUNTIME("Parameter instrume must specify either HXI1 or HXI2.");
    }

  if(param.m_instrume == "HXI1")
    param.m_hxi_unit_number = 1;
  else if(param.m_instrume == "HXI2")
    param.m_hxi_unit_number = 2;
  else
    param.m_hxi_unit_number = 0; // This case shouldn't be needed.

  // Get the CAMS1, CAMS2, and HXI teldef filenames.

  param.m_cams1_teldef_file = ahapp::getParString("cams1teldef");
  param.m_cams2_teldef_file = ahapp::getParString("cams2teldef");
  param.m_hxi_teldef_file = ahapp::getParString("hxiteldef");

  // Read the CAMS position file names and set variables for whether
  // each exists to be used.

  tmp = ahapp::getParString("infile1");
  pos = tmp.find("[");
  param.m_in_file1 = tmp.substr(0, pos);
  param.m_in_file1_as_input = tmp;
  if (std::string::npos != pos) {
    AH_INFO(ahlog::HIGH) << "Explicit HDU numbers and extended filename syntax ignored (infile1)." << std::endl;
  }

  tmp = ahapp::getParString("infile2");
  pos = tmp.find("[");
  param.m_in_file2 = tmp.substr(0, pos);
  param.m_in_file2_as_input = tmp;
  if (std::string::npos != pos) {
    AH_INFO(ahlog::HIGH) << "Explicit HDU numbers and extended filename syntax ignored (infile2)." << std::endl;
  }

  param.m_use_cams1_file = (ahgen::strtoupper(param.m_in_file1).compare("NONE") != 0);
  param.m_use_cams2_file = (ahgen::strtoupper(param.m_in_file2).compare("NONE") != 0);
  if(!param.m_use_cams1_file && !param.m_use_cams2_file)
    AH_THROW_RUNTIME("At least one of the CAMS1 and CAMS2 data files must be specified.");

  // Get the remaining tool parameters.

  tmp = ahapp::getParString("outfile");
  pos = tmp.find("[");
  param.m_out_file = tmp.substr(0, pos);
  param.m_out_file_as_input = tmp;
  if (std::string::npos != pos) {
    AH_INFO(ahlog::HIGH) << "Explicit HDU numbers and extended filename syntax ignored (outfile)." << std::endl;
  }

  param.m_debug = ahapp::getParBool("debug");
  param.m_clobber = ahapp::getParBool("clobber");
  param.m_in_data_ext = ahapp::getParString("inext");
  param.m_out_data_ext = ahapp::getParString("outext");
  param.m_flip_output_sign = (ahapp::getParBool("flipsign") ? -1 : 1);
  param.m_starting_sys = ahgen::strtoupper(ahapp::getParString("startsys"));

  param.m_t_tol = 0.001; //seconds
}

// ------------------------------------------------------------

void initialize(Params& param, ahmission::teldef::CAMSTelDef& cams1, ahmission::teldef::CAMSTelDef& cams2, 
    ahmission::teldef::HXITelDef& hxi_extra, TELDEF2*& hxi_std)
{

  // Declare a status for reading the standard TelDef file.

  int status = 0;

  // Resolve CALDB references

  std::string actual_cams1teldef = ahmission::caldb::resolve(param.m_cams1_teldef_file,
    "TelDef", "CAMS1", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

  ape_trad_set_string("cams1teldef", actual_cams1teldef.c_str());

  std::string actual_cams2teldef = ahmission::caldb::resolve(param.m_cams2_teldef_file,
    "TelDef", "CAMS2", "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

  ape_trad_set_string("cams2teldef", actual_cams2teldef.c_str());

  std::string actual_hxiteldef = ahmission::caldb::resolve(param.m_hxi_teldef_file,
    "TelDef", param.m_instrume, "-", "TELDEF", "-", "-", ahmission::getTELESCOPString());

  ape_trad_set_string("hxiteldef", actual_hxiteldef.c_str());

  // Load CAMS and HXI TelDef files into structures.
  
  AH_INFO(ahlog::HIGH) << "Reading CAMS1 TelDef file " << param.m_cams1_teldef_file << "." << std::endl;
  cams1 = ahmission::teldef::CAMSTelDef(actual_cams1teldef, 1);
  AH_DEBUG << "\n\nCAMS1 TelDef structure:\n" << cams1 << std::endl;
  
  AH_INFO(ahlog::HIGH) << "Reading CAMS2 TelDef file " << param.m_cams2_teldef_file << "." << std::endl;;
  cams2 = ahmission::teldef::CAMSTelDef(actual_cams2teldef, 2);
  AH_DEBUG << "\n\nCAMS2 TelDef structure:\n" << cams2 << std::endl;;

  AH_INFO(ahlog::HIGH) << "Reading HXI TelDef file " << param.m_hxi_teldef_file << "." << std::endl;;
  hxi_extra = ahmission::teldef::HXITelDef(actual_hxiteldef, param.m_hxi_unit_number);
  AH_DEBUG << "\n\nHXI TelDef-extra structure:\n" << hxi_extra << std::endl;;

  status = readTelDef2(actual_hxiteldef.c_str(), &hxi_std);
  if(status)
    AH_THROW_RUNTIME("Cannot read HXI TelDef file " + param.m_hxi_teldef_file + ".");

  // Print the HXI standard TelDef structure if in debug mode.

  if(param.m_debug)
    printTelDef2(hxi_std, stderr);

  // Check if outfile can/should be clobbered.

  if(!ahgen::isFileClobbered(param.m_out_file,ahfits::getClobber()))
    {
      if(ahgen::fileExists(param.m_out_file))
        {
          AH_THROW_RUNTIME("Clobbering disabled, but the output file " + param.m_out_file + " already exists.");
        }
    }
  
  // Open the input files.

  param.m_fp_cams1 = NULL;
  if(param.m_use_cams1_file) 
    {
      ahfits::open(param.m_in_file1, "", &param.m_fp_cams1);
    }
  
  param.m_fp_cams2 = NULL;
  if(param.m_use_cams2_file) 
    {
      ahfits::open(param.m_in_file2, "", &param.m_fp_cams2);
    }
  
  

  // Create the output file, after making sure it's OK.

  if (param.m_out_file == param.m_in_file1 || 
      param.m_out_file == param.m_in_file2)
    {
      AH_THROW_RUNTIME("Output filename same as one of the input filenames.");
    }

  ahfits::create(param.m_out_file, "", &param.m_fp_out);

  // Copy standard keywords to output file.
  
  if (param.m_use_cams1_file) 
    {
      AH_INFO(ahlog::LOW) << "Copying primary header keywords from CAMS1 input file." << std::endl;
      ahmission::keyword::copyAllKeywords(param.m_fp_cams1, param.m_fp_out, ahmission::keyword::e_PRIMARY);
    } 
  else if (param.m_use_cams2_file) 
    {
      AH_INFO(ahlog::LOW) << "Copying primary header keywords from CAMS1 input file." << std::endl;
      ahmission::keyword::copyAllKeywords(param.m_fp_cams2, param.m_fp_out, ahmission::keyword::e_PRIMARY);
    } 
  else 
    {
      AH_THROW_RUNTIME("Must have at leat one CAMS input file.");
    }

  // Force instrument to the HXI unit.
  
  ahfits::writeKeyValStr(param.m_fp_out, "INSTRUME", param.m_instrume, "Instrument name");

  // Move to the input data extensions, and check they're not empty.

  if(param.m_use_cams1_file) {
    openCAMSDataExtension(param.m_fp_cams1, param.m_in_file1, param.m_in_data_ext, "CAMS1", param.m_cams1_n_rows);
    ahmission::checkEmptyTable(param.m_fp_cams1,param.m_in_file1);
  } else {
    AH_INFO(ahlog::HIGH) << "Not using any data from CAMS1.  No CAMS1 data file was specified." << std::endl;;
  }
  
  if(param.m_use_cams2_file) {
    openCAMSDataExtension(param.m_fp_cams2, param.m_in_file2, param.m_in_data_ext, "CAMS2", param.m_cams2_n_rows);
    ahmission::checkEmptyTable(param.m_fp_cams2,param.m_in_file2);
  } else {
    AH_INFO(ahlog::HIGH) << "Not using any data from CAMS2.  No CAMS2 data file was specified." << std::endl;;
  }
  
  // Create the output data extension.

  ahfits::addEmptyTbl(param.m_fp_out, param.m_out_data_ext);

  //  Add the necessary columns.

  addCAMSOffsetsFileColumns(param.m_fp_out);

  // Copy standard keywords.

  if (param.m_use_cams1_file) 
    {
      AH_INFO(ahlog::LOW) << "Copying extension keywords from CAMS1 input file." << std::endl;
      ahmission::keyword::copyAllKeywords(param.m_fp_cams1, param.m_fp_out, ahmission::keyword::e_HK);
    } 
  else if (param.m_use_cams2_file) 
    {
      AH_INFO(ahlog::LOW) << "Copying extension keywords from CAMS1 input file." << std::endl;
      ahmission::keyword::copyAllKeywords(param.m_fp_cams2, param.m_fp_out, ahmission::keyword::e_HK);
    } 

  // Add cams2det-specific keywords.
  
  ahfits::writeKeyValStr(param.m_fp_out, "CAM1DATA", (param.m_use_cams1_file ? "YES" : "NO"), 
                         std::string("Data from CAMS1 ") + std::string(param.m_use_cams1_file ? "" : "NOT ") 
                         + std::string("included"));
  ahfits::writeKeyValStr(param.m_fp_out, "CAM2DATA", (param.m_use_cams2_file ? "YES" : "NO"), 
                         std::string("Data from CAMS2 ") + std::string(param.m_use_cams2_file ? "" : "NOT ") 
                         + std::string("included"));

  ahfits::writeKeyValStr(param.m_fp_out, "ORIGSYS", param.m_starting_sys, "Originating coordinate system");
  ahfits::writeKeyValStr(param.m_fp_out, "DESTSYS", "ACT", "Destination coordinate system");
  ahfits::writeKeyComment(param.m_fp_out, "ORIGSYS/DESTSYS keywords indicate the coordinate system");
  ahfits::writeKeyComment(param.m_fp_out, "transformation ORIGSYS->DESTSYS for which the DELTARAWX");
  ahfits::writeKeyComment(param.m_fp_out, "and DELTARAWY offsets are calculated.");

  // Force instrument to the HXI unit.
  
  ahfits::writeKeyValStr(param.m_fp_out, "INSTRUME", param.m_instrume, "Instrument name");

  // Write list of parameters to log file

  ahapp::writeParametersToLog(); 
      
}

// ------------------------------------------------------------

void doWork(Params& param, 
      const ahmission::teldef::CAMSTelDef& cams1, const ahmission::teldef::CAMSTelDef& cams2, 
      const ahmission::teldef::HXITelDef& hxi_extra, TELDEF2*& hxi_std)
{
  // Declare TelDef indices.

  int raw_sys = -1;
  int raw_min_seg = -1;
  int foc_sys = -1;
  int foc_min_seg = -1;

  // Calculate derived geometric properties of both CAMS units.

  const CAMSHXIGeometry geom(cams1, cams2, hxi_extra, hxi_std, param);

  // Initialize the data row structures, two for the input files and one for the output.

  ahmission::camsio::UnitDataRow data1;
  ahmission::camsio::UnitDataRow data2;
  CAMSOffsetsDataRow outrow;
  
  // Initialize the ahfits router pointers for those files.

  ahfits::Router* p_rt_cams1 = NULL;
  ahfits::Router* p_rt_cams2 = NULL;
  ahfits::Router* p_rt_out = NULL;

  // Initialize the start times and rows of the input files.  Later
  // the values will be the first non-null time and the corresponding
  // row number.

  double cams1_t_start = DOUBLENULL;
  double cams2_t_start = DOUBLENULL;
  long cams1_row_start = -1;
  long cams2_row_start = -1;

  // Initialize start and end times of the output file.

  double t_start = DOUBLENULL;
  double t_stop = DOUBLENULL;

  // Initialize loop variables to track the loop iteration, input file
  // rows, the need to read the next input row(s), and if the
  // calculation for the current row should defer to the next file
  // reading.

  long iter = 0;
  long iter_max = 1000000000; 
  double last_t = 0.;
  long cams1_row = -1;
  long cams2_row = -1;
  bool cams1_time_past_end = false;
  bool cams2_time_past_end = false;
  bool need_to_read_cams1_row = true;
  bool need_to_read_cams2_row = true;
  bool skip_to_next_row = false;
  bool proc_status_ok_cams1 = true;
  bool proc_status_ok_cams2 = true;

  // Initialize variables for keeping track of the most recently read/used non-null values.

  int last_good_x1 = INTNULL;
  int last_good_y1 = INTNULL;
  int last_good_x2 = INTNULL;
  int last_good_y2 = INTNULL;
  double last_good_cos_twist = 1.;
  double last_good_sin_twist = 0.;
  int any_good_x1_yet = false;
  int any_good_y1_yet = false;
  int any_good_x2_yet = false;
  int any_good_y2_yet = false;

  // Initialize line counters for the input files and the output file.

  long n_written_lines = 0;
  long n_cams1_read_lines = 0;
  long n_cams2_read_lines = 0;
  long n_proc_status_ok_cams1 = 0;
  long n_proc_status_ok_cams2 = 0;
  long n_proc_status_bad_cams1 = 0;
  long n_proc_status_bad_cams2 = 0;
  long n_time_ok_cams1 = 0;
  long n_time_ok_cams2 = 0;
  long n_time_null_cams1 = 0;
  long n_time_null_cams2 = 0;
  long n_null_rows_written = 0;
  long n_nonnull_rows_written = 0;

  // Determine starting coordinate system.
  
  bool start_from_raw = ( param.m_starting_sys == "RAW" );

  // Get the coordinate system number of the RAW system in the 
  // HXI standard teldef structure.

  raw_sys = getCoordSystemNumberFromName(hxi_std, "RAW");
  if(raw_sys < 0)
    AH_THROW_RUNTIME("Cannot find RAW coordinate system in HXI TelDef file " + param.m_hxi_teldef_file + ".");
  raw_min_seg = hxi_std->min_segment[raw_sys];

  // Get the coordinate system number of the FOC system in the 
  // HXI standard teldef structure.

  foc_sys = getCoordSystemNumberFromName(hxi_std, "FOC");
  if(foc_sys < 0)
    AH_THROW_RUNTIME("Cannot find FOC coordinate system in HXI TelDef file " + param.m_hxi_teldef_file + ".");
  foc_min_seg = hxi_std->min_segment[foc_sys];

  // Connect the data variables to the input CAMS data columns.
  
  if(param.m_use_cams1_file)
    p_rt_cams1 = data1.connectColumns(param.m_fp_cams1, ahfits::e_READONLY);

  if(param.m_use_cams2_file)
    p_rt_cams2 = data2.connectColumns(param.m_fp_cams2, ahfits::e_READONLY);

  // Connect the output variables to output table columns.

  p_rt_out = outrow.connectColumns(param.m_fp_out, ahfits::e_WRITEONLY);

  // Initialize for the main loop through the input and output files. 
  
  last_t = t_start;
  cams1_row = cams1_row_start;
  cams2_row = cams2_row_start;
  need_to_read_cams1_row = param.m_use_cams1_file;
  need_to_read_cams2_row = param.m_use_cams2_file;
  cams1_time_past_end = !param.m_use_cams1_file;
  cams2_time_past_end = !param.m_use_cams2_file;
  
  // Loop through input table rows and produce output table rows.

  AH_INFO(ahlog::LOW) << "Matching times between CAMS data and calculating offsets..." << std::endl;;

  firstRow(param.m_fp_out);

  while(iter < iter_max && (!cams1_time_past_end || !cams2_time_past_end))
    {
      skip_to_next_row = false;
      
      // Set output data to null/default values for this row.

      outrow.setRowToNull();
      
      // Check if the last row of each file has already been read. 

      if(!cams1_time_past_end && atLastRow(param.m_fp_cams1))
        cams1_time_past_end = true;

      if(!cams2_time_past_end && atLastRow(param.m_fp_cams2))
        cams2_time_past_end = true;

      // Read the next input data rows if needed.

      if(!cams1_time_past_end && need_to_read_cams1_row)
        {
          if(iter == 0)
            firstRow(param.m_fp_cams1);
          else 
            nextRow(param.m_fp_cams1);
          
          readRow(param.m_fp_cams1);
          n_cams1_read_lines++;
          proc_status_ok_cams1 = procstatus::processRow(data1.m_proc_status);
            
          if(proc_status_ok_cams1) 
            n_proc_status_ok_cams1++;
          else
            {
              n_proc_status_bad_cams1++;
              AH_DEBUG << "Bad PROC_STATUS in CAMS1 row " << n_cams1_read_lines
                << " (iteration " << iter << std::endl;
            }

          if (isDoubleNull(data1.m_t))
            {
              n_time_null_cams1++;
              AH_DEBUG << "Null time in CAMS1 row " << n_cams1_read_lines
                << " (iteration " << iter << std::endl;
            }
          else
            n_time_ok_cams1++;

          if(isDoubleNull(cams1_t_start) && !isDoubleNull(data1.m_t) && proc_status_ok_cams1)
            {
              cams1_t_start = data1.m_t;
              AH_DEBUG << "CAMS1 data begins at t = " << cams1_t_start 
                 << " in row " << currentRow(param.m_fp_cams1) << "." << std::endl;
            }
        }

      if(!cams2_time_past_end && need_to_read_cams2_row)
        {
          if(iter == 0)
            firstRow(param.m_fp_cams2);
          else 
            nextRow(param.m_fp_cams2);
          
          readRow(param.m_fp_cams2);
          n_cams2_read_lines++;
          proc_status_ok_cams1 = procstatus::processRow(data2.m_proc_status);
            
          if(proc_status_ok_cams2) 
            n_proc_status_ok_cams2++;
          else
            {
              n_proc_status_bad_cams2++;
              AH_DEBUG << "Bad PROC_STATUS in CAMS1 row " << n_cams1_read_lines
                << " (iteration " << iter << std::endl;
            }

          if (isDoubleNull(data2.m_t))
            {
              n_time_null_cams2++;
              AH_DEBUG << "Null time in CAMS1 row " << n_cams1_read_lines
                << " (iteration " << iter << std::endl;
            }
          else
            n_time_ok_cams2++;
          
          if(isDoubleNull(cams2_t_start) && !isDoubleNull(data2.m_t) && proc_status_ok_cams2)
            {
              cams2_t_start = data2.m_t;
              AH_DEBUG << "CAMS2 data begins at t = " << cams2_t_start 
                 << " in row " << currentRow(param.m_fp_cams2) << "." << std::endl;
            } 
        }

      // Check if times are null. Skip to the next loop iteration if
      // either time is null or if PROC_STATUS is bad.

      if(!cams1_time_past_end && (isDoubleNull(data1.m_t) || !proc_status_ok_cams1))
        {
          need_to_read_cams2_row = false;
          skip_to_next_row = true;
        }

      if(!cams2_time_past_end && (isDoubleNull(data2.m_t) || !proc_status_ok_cams2))
        {
          need_to_read_cams1_row = false;
          skip_to_next_row = true;
        }

      if(!cams1_time_past_end && !cams2_time_past_end && !need_to_read_cams1_row && !need_to_read_cams2_row)
        {
          // If two input files are used and the times are null for
          // both, read the next row from each file in the next
          // iteration.

          need_to_read_cams1_row = true;
          need_to_read_cams2_row = true;
        }

      if(!cams1_time_past_end)
        AH_DEBUG << "CAMS1 t=" << data1.m_t << " row=" << currentRow(param.m_fp_cams1) << " q=" << data1.m_q << std::endl;
      
      if(!cams2_time_past_end)
        AH_DEBUG << "CAMS2 t=" << data2.m_t << " row=" << currentRow(param.m_fp_cams2) << " q=" << data2.m_q << std::endl;

      if(skip_to_next_row)
        {
          AH_DEBUG << "No computation. Skipping to next row.  iter = " << iter
            << "; CAMS1 row number = " << n_cams1_read_lines << "; CAMS2 row number = " <<
            n_cams2_read_lines << std::endl;
          iter++;
          continue;
        }

      // Determine which input file(s) have both valid position and
      // valid time data.  Set the corresponding bad_units flags and
      // the output time.  Assume first that both CAMS units have
      // valid data and turn on flags as that assumption is found to
      // be incorrect.

      outrow.m_bad_units = e_CAMS12_OK;
      need_to_read_cams1_row = true;
      need_to_read_cams2_row = true;

      if(!cams1_time_past_end && !cams2_time_past_end)
        {
          // Case: Data read from two CAMS files

          if(fabs(data1.m_t - data2.m_t) <= param.m_t_tol)
            {
              // Case: Times closely match

              outrow.fillWithCAMSUnitData(data1, 1);
              outrow.fillWithCAMSUnitData(data2, 2);
              outrow.m_t = 0.5*(data1.m_t + data2.m_t);
              
              if(data1.m_x_raw == INTNULL || data1.m_y_raw == INTNULL)
                {
                  // Case: Invalid position data from CAMS1
                  
                  outrow.m_bad_units |= e_CAMS1_BAD;
                  outrow.m_t = data2.m_t;
                }
              
              if(data2.m_x_raw == INTNULL || data2.m_y_raw == INTNULL)
                {
                  // Case: Invalid position data from CAMS2
                  
                  outrow.m_bad_units |= e_CAMS2_BAD;
                  outrow.m_t = data1.m_t;
                }
            }
          else if(data1.m_t < data2.m_t)
            {
              // Case: Times do not closely match and CAMS1 time is earlier.
              
              outrow.m_bad_units |= e_CAMS2_BAD;
              outrow.m_t = data1.m_t;
              outrow.fillWithCAMSUnitData(data1, 1);
              need_to_read_cams2_row = false;

              if(data1.m_x_raw == INTNULL || data1.m_y_raw == INTNULL)
                {
                  // Case: Invalid position data from CAMS1
                  
                  outrow.m_bad_units |= e_CAMS1_BAD;
                }
            }
          else if(data1.m_t > data2.m_t)
            {
              // Case: Times do not closely match and CAMS2 time is earlier.
              
              outrow.m_bad_units |= e_CAMS1_BAD;
              outrow.m_t = data2.m_t;
              outrow.fillWithCAMSUnitData(data2, 2);
              need_to_read_cams1_row = false;
              
              if(data2.m_x_raw == INTNULL || data2.m_y_raw == INTNULL)
                {
                  // Case: Invalid position data from CAMS2
                  
                  outrow.m_bad_units |= e_CAMS2_BAD;
                }
            }
        } // end case: data read from two CAMS files
      else if(!cams1_time_past_end && cams2_time_past_end)
        {
          // Case: One data row was read, and it's for CAMS1
          
          outrow.m_bad_units |= e_CAMS2_BAD;
          outrow.m_t = data1.m_t;
          outrow.fillWithCAMSUnitData(data1, 1);
          
          if(data1.m_x_raw == INTNULL || data1.m_y_raw == INTNULL)
            {
              // Case: The CAMS1 position is not valid
              
              outrow.m_bad_units |= e_CAMS1_BAD;
            }
        } 
      else if(cams1_time_past_end && !cams2_time_past_end)
        {
          // Case: One data row was read, and it's for CAMS2
          
          outrow.m_bad_units |= e_CAMS1_BAD;
          outrow.m_t = data2.m_t;
          outrow.fillWithCAMSUnitData(data2, 2);
          
          if(data2.m_x_raw == INTNULL || data2.m_y_raw == INTNULL)
            {
              // Case: The CAMS2 position is not valid
              
              outrow.m_bad_units |= e_CAMS2_BAD;
            }
        }
      
      // Set the start time as the first non-null time.

      if(isDoubleNull(t_start))
        t_start = outrow.m_t;

      // Set flags for what cannot be calculated.

      if(outrow.m_bad_units == e_CAMS12_BAD)
        {
          outrow.m_calc_quality |= e_QF_NO_DATA;
          outrow.m_calc_quality |= e_QF_NO_ANGLE;
        }
      if((outrow.m_bad_units == e_CAMS2_BAD) ^ (outrow.m_bad_units == e_CAMS1_BAD))
        {
          outrow.m_calc_quality |= e_QF_NO_ANGLE;
        }
      
      AH_DEBUG << "iter=" << iter 
         << "  using time t=" << outrow.m_t 
         << " row=" << currentRow(param.m_fp_out) 
         << " bad_units=" << outrow.m_bad_units 
         << " q1=" << outrow.m_q1 
         << " q2=" << outrow.m_q2 << std::endl;

      // If there is no valid position data, output the row without
      // calculated values besides time, calc quality, and bad units.

      if(outrow.m_bad_units == e_CAMS12_BAD)
        {
          AH_DEBUG << "Writing null row " << currentRow(param.m_fp_out) << std::endl;
          writeRow(param.m_fp_out);
          n_written_lines++;
          n_null_rows_written++;
          nextRow(param.m_fp_out);
          t_stop = outrow.m_t;
          iter++;
          continue;
        }

      // Calculate jumps between consecutive good x or y values.
      // Jumps are set to null if the corresponding CAMS file is not used or 
      // if the current line does not have good CAMS position data. 
      // Jumps are set to 0 with the first good value of x or y.
      
      if(param.m_use_cams1_file)
        {
          outrow.m_jump_x1 = (any_good_x1_yet ? calcPositionJump(outrow.m_x1, last_good_x1, INTNULL) : 0);
          outrow.m_jump_y1 = (any_good_y1_yet ? calcPositionJump(outrow.m_y1, last_good_y1, INTNULL) : 0);
        }
      else
        {
          outrow.m_jump_x1 = INTNULL;
          outrow.m_jump_y1 = INTNULL;
        }

      if(param.m_use_cams2_file)
        {
          outrow.m_jump_x2 = (any_good_x2_yet ? calcPositionJump(outrow.m_x2, last_good_x2, INTNULL) : 0);
          outrow.m_jump_y2 = (any_good_y2_yet ? calcPositionJump(outrow.m_y2, last_good_y2, INTNULL) : 0);
        }
      else
        {
          outrow.m_jump_x2 = INTNULL;
          outrow.m_jump_y2 = INTNULL;
        }

      // Update any_good_**_yet variables to true if a good value has
      // been found.

      any_good_x1_yet = any_good_x1_yet || outrow.m_x1 != INTNULL;
      any_good_y1_yet = any_good_y1_yet || outrow.m_y1 != INTNULL;
      any_good_x2_yet = any_good_x2_yet || outrow.m_x2 != INTNULL;
      any_good_y2_yet = any_good_y2_yet || outrow.m_y2 != INTNULL;

      // Calculate CAMS displacements.

      calcCAMSDisplacements(outrow, last_good_cos_twist, last_good_sin_twist, 
          geom, cams1, cams2, hxi_extra, hxi_std, start_from_raw,
          raw_sys, raw_min_seg, foc_sys, foc_min_seg);


      // Set quality to 1 even if it's read as 0 but one or both of x and y are NULL.

      if(outrow.m_q1 == 0 && (outrow.m_bad_units & e_CAMS1_BAD))
        outrow.m_q1 = 1;

      if(outrow.m_q2 == 0 && (outrow.m_bad_units & e_CAMS2_BAD))
        outrow.m_q2 = 1;
         
      // Flip signs of offsets if the flipsign parameter is enabled.

      outrow.m_dx *= param.m_flip_output_sign;
      outrow.m_dy *= param.m_flip_output_sign;
      outrow.m_dx_sat *= param.m_flip_output_sign;
      outrow.m_dy_sat *= param.m_flip_output_sign;

      // Flip sign of twist angle if the flipsign parameter is enabled.
      // Sine only; cosine stays the same for minus the angle.
      outrow.m_sin_twist *= param.m_flip_output_sign;

      // Write values to output table if at least one CAMS unit has good data.
      
      if((!cams1_time_past_end || !cams2_time_past_end) && outrow.m_bad_units != e_CAMS12_BAD)
        {
          AH_DEBUG << "Writing non-null row " << currentRow(param.m_fp_out) << std::endl;
          writeRow(param.m_fp_out);
          n_written_lines++;
          n_nonnull_rows_written++;
          nextRow(param.m_fp_out);
          t_stop = outrow.m_t;
        }
      
      // Update variables holding previous row data.
      
      last_t = outrow.m_t;
      last_good_x1 = (outrow.m_x1 == INTNULL ? last_good_x1 : outrow.m_x1);
      last_good_y1 = (outrow.m_y1 == INTNULL ? last_good_y1 : outrow.m_y1);
      last_good_x2 = (outrow.m_x2 == INTNULL ? last_good_x2 : outrow.m_x2);
      last_good_y2 = (outrow.m_y2 == INTNULL ? last_good_y2 : outrow.m_y2);
      
      iter++;
    }
  
  AH_INFO(ahlog::HIGH) << "Finished calculating positions." << std::endl;;
  
  // Display the number of lines read and written.

  AH_INFO(ahlog::HIGH) << "CAMS1 data file:        " << param.m_in_file1 << std::endl;
  AH_INFO(ahlog::HIGH) << "CAMS2 data file:        " << param.m_in_file2 << std::endl;
  AH_INFO(ahlog::HIGH) << "Output file:            " << param.m_out_file << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows read from CAMS1 data file: " << n_cams1_read_lines << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS1 rows with good PROC_STATUS:   " << n_proc_status_ok_cams1 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS1 rows with bad PROC_STATUS:    " << n_proc_status_bad_cams1 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS1 rows with good time:          " << n_time_ok_cams1 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS1 rows with null time:          " << n_time_null_cams1 << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows read from CAMS2 data file: " << n_cams2_read_lines << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS2 rows with good PROC_STATUS:   " << n_proc_status_ok_cams2 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS2 rows with bad PROC_STATUS:    " << n_proc_status_bad_cams2 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS2 rows with good time:          " << n_time_ok_cams2 << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of CAMS2 rows with null time:          " << n_time_null_cams2 << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of rows written to output file:    " << n_written_lines << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of null rows written to output file:    " << n_null_rows_written << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of nonnull rows written to output file: " << n_nonnull_rows_written << std::endl;
  AH_INFO(ahlog::HIGH) << "Number of iterations of main loop:        " << iter-1 << std::endl;

  // Write time range of offsets table and the bit flag descriptions to the header.

  if(isDoubleNull(t_start) || isDoubleNull(t_stop))
    {
      ahfits::writeKeyValStr(param.m_fp_out, "TSTART", "NULL", "Time of first row");
      ahfits::writeKeyValStr(param.m_fp_out, "TSTOP", "NULL", "Time of last row");
    }
  else
    {
      ahfits::writeKeyValDbl(param.m_fp_out, "TSTART", t_start, "Time of first row");
      ahfits::writeKeyValDbl(param.m_fp_out, "TSTOP", t_stop, "Time of last row");
    }

  ahfits::writeKeyComment(param.m_fp_out, BAD_UNITS_DESC);
  ahfits::writeKeyComment(param.m_fp_out, CALC_QUALITY_DESC);

  // Clean up the routers.

  delete p_rt_cams1;
  delete p_rt_cams2;
  delete p_rt_out;
}

// ------------------------------------------------------------

void finalize(Params& param, TELDEF2*& hxi_std)
{
  // Close files that were opened, and deallocate the standard HXI
  // TelDef structure.

  if(param.m_fp_out != NULL)
    ahfits::close(param.m_fp_out);
  if(param.m_fp_cams1 != NULL)
    ahfits::close(param.m_fp_cams1);
  if(param.m_fp_cams2 != NULL)
    ahfits::close(param.m_fp_cams2);
  if(hxi_std != NULL)
    destroyTelDef2(hxi_std);
  
  AH_OUT << "Finished cams2det." << std::endl;
}

// ======================================================================

/* Revision Log
 $Log: cams2det.cxx,v $
 Revision 1.44  2016/04/08 15:06:59  rshill
 Changed AH_INFO to AH_DEBUG in processing loop.

 Revision 1.43  2016/03/22 16:10:41  rshill
 Added 2 calls to writeParametersToLog (issue #610).

 Revision 1.42  2015/12/29 19:16:14  klrutkow
 throw error if no rows in the input file(s)

 Revision 1.41  2015/09/21 15:53:02  rshill
 Changed processing of twist angle with flipsign so that only sine is changed.

 Revision 1.40  2015/09/04 21:09:56  rshill
 Added stamping of parameters to log.

 Revision 1.39  2015/07/31 23:27:37  rshill
 Prolog edit.

 Revision 1.38  2015/07/31 20:50:25  rshill
 Include temperature-corrected CAMS data columns.

 Revision 1.37  2015/07/31 00:00:14  rshill
 Code cleanup; added proc_status processing; added more counters; added descriptions and comments in output FITS header.

 Revision 1.36  2015/07/30 00:50:15  rshill
 Incorporated keyword copying.

 Revision 1.35  2015/07/29 21:37:37  rshill
 Changed all detnam to instrume.

 Revision 1.34  2015/07/21 21:46:46  rshill
 Fixed typo.

 Revision 1.33  2015/07/21 21:45:49  rshill
 Added +++ comments to be addressed.

 Revision 1.32  2015/07/16 22:14:01  rshill
 Added CALDB filename resolution.

 Revision 1.31  2015/04/01 00:04:16  rshill
 Put in accounting for height difference between CAMS and HXI.  Allow starting coord system to be either RAW or FOC.

 Revision 1.30  2015/03/18 17:49:47  mwitthoe
 cams2det: change DETNAME to DETNAM

 Revision 1.29  2015/01/07 15:51:53  mwitthoe
 cams2det: update parameters; see issue 472

 Revision 1.28  2014/12/04 18:37:04  mwitthoe
 cams2det: add include statement for ahgen

 Revision 1.27  2014/01/31 22:22:57  treichar
 Eliminated a minor memory leak

 Revision 1.26  2014/01/29 21:51:15  treichar
 Changed parameter-getting functions to eliminate a few minor memory leaks

 Revision 1.25  2014/01/27 16:28:29  treichar
 Added more comments

 Revision 1.24  2014/01/27 15:56:54  treichar
 Changed bit flag descriptions to single string, using the new ahfits::writeKeyComment function that handles newline and spaces for line breaks

 Revision 1.23  2014/01/16 20:23:28  treichar
 Added more comments.

 Revision 1.22  2014/01/10 20:41:35  treichar
 Coupled the null-ness of X and Y values and corresponding jumps according to the prescribed algorithm change.

 Revision 1.21  2014/01/07 19:34:11  treichar
 Moved hxiteldef.{h,cxx} and camsteldef.{h,cxx} to the ahmission library.  Moved the functions for reading a CAMS unit data file out of functions.{h,cxx} to the ahmission
 library.  Reorganized the remaining functions in functions.{h,cxx} and params.h into cams2detlib.{h,cxx}.  Updated the standard main function.

 Revision 1.20  2014/01/02 19:17:39  treichar
 Changed the time-matching algorithm to a more robust one that does not rely on matching file times to a nominal time determined by the regularly spaced time offsets in CALDB. The
 new algorithm scans the times in each of the two input files, combines the input data rows as a single output row when the two times match within a tolerance, and keeps the
 input data rows in separate output rows when the two times are too widely spaced.

 Revision 1.19  2013/12/27 20:23:05  treichar
 Grouped various quantities into structures for simpler function calls and better organization.

 Revision 1.18  2013/12/18 22:10:28  treichar
 Completed CFITSIO->ahfits conversion.

 Revision 1.17  2013/12/02 22:52:52  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.16  2013/08/02 20:04:54  treichar
 Updated the position offsets and twist angle calculation according to the corrections devised by Hans Krimm and Casey Lambert.  Changed default value of flipoutputsign to no, and this parameter may be removed in
 the future.

 Revision 1.15  2013/07/25 19:18:39  treichar
 Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
 throughout code.

*/
