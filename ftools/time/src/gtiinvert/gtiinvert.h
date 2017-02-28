/*
 * \file gtiinvert.h
 * \brief Output a FITS file of Good Time Intervals (GTI) not covered in the input FITS file of GTI invervals
 * \author David Riethmiller
 * \date $Date: 2015/12/23 16:47:10 $  
 *
 *  If we have time on the x-axis, then a visual representation of this function should look like:
 *
 *  Input GTI:   -------  ---------        ------------  ---- ---------------   ---------
 *  Output GTI:         --         --------            --    -               ---
 *
 */

#include "fitsio.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <stdexcept>
#include <cstring>
#include <string>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>

#define STR_LEN 300

/// \brief Structure to hold all input data from par file
/// \param[in,out] infile Name of input FITS GTI file
/// \param[in,out] outfile Name of output FITS GTI file
/// \param[in,out] outext Name of extention in outfile where data will be written
/// \param[in,out] marginti Boolean: Create GTI between TSTART/TSTOP and first/last input GTI
/// \param[in,out] tstart Value to use for TSTART in seconds (or take from infile)
/// \param[in,out] tstop Value to use for TSTOP in seconds(or take from infile)
/// \param[in,out] dt Time separation between input and output GTI (seconds)
/// \param[in,out] clobber Boolean: overwrite existing output file
/// \param[in,out] chatter How much diagnostic data to print to terminal screen
/// \param[in,out] logfile Name of file to capture run data
/// \param[in,out] debug Boolean: print diagnostic info to screen
/// \param[in,out] history Boolean: track history
/// \param[in,out] mode Query mode for automatic parameters
struct PARAMS{
  std::string infile;
  std::string outfile;
  std::string outext;
  bool margingti;
  std::string tstart;
  std::string tstop;
  double dt;
  bool clobber;
  int chatter;
  std::string logfile;
  bool debug;
  bool history;
  std::string mode;
} par;


// Function declarations
// =====================================================================================

/// \brief Get parameter values from par file
/// \param[in,out] par Parameter structure
/// \param[in,out] argc
/// \param[in,out] argv
int getPars(PARAMS & par, int argc, char ** argv);


/// \brief Set up tool operation
/// \param[in] par Parameter structure
/// \param[out] tstart Start time
/// \param[out] tstop Stop time
/// \param[out] ahffp_in Pointer to input FITS file
/// \param[out] ahffp_out Pointer to output FITS file
int initialize(PARAMS * par, 
               double & tstart, 
               double & tstop,
               fitsfile ** ahffp_in,
               fitsfile ** ahffp_out);


/// \brief Do the actual work of the tool
/// \param[in] par Parameter structure
/// \param[out] tstart Start time
/// \param[out] tstop Stop time
/// \param[out] ahffp_in Pointer to input FITS file
/// \param[out] ahffp_out Pointer to output FITS file  
int doWork(PARAMS * par,
            double tstart,
            double tstop,
            fitsfile ** ahffp_in,
            fitsfile ** ahffp_out);

/// \brief Clean up output and close FITS files
/// \param[in] par Parameter structure
/// \param[out] ahffp_in Pointer to input FITS file
/// \param[out] ahffp_out Pointer to output FITS file
int finalize(PARAMS & par,
             fitsfile ** ahffp_in,
             fitsfile ** ahffp_out);

/// \brief Close FITS files and delete output file upon error
/// \param[out] ahffp_in Pointer to input FITS file
/// \param[out] ahffp_out Pointer to output FITS file
void cleanup_error(fitsfile ** ahffp_in,
                   fitsfile ** ahffp_out);

/// \brief Generate a gnuplot script to visualize the resulting input and output GTI invervals
/// \param[in] par Parameter structure
void visualize(PARAMS * par);

/// \brief Return true if given file exists
/// \param[in] filename The filename to test
bool file_exists(const char *filename);

/// \brief Remove an element from an array, and resize the array to fit
/// \param[in,out] array The array to be modfied
/// \param[in] n_array Number of elements in input array
/// \param[in] index_to_remove Array index of element to be removed
void remove_array_element(double ** array, long n_array, int index_to_remove);

/// \brief Copy keyword if it does not exist
/// \param[in] fpin cfitsio file pointer to copy from
/// \param[in] fpout cfitsio file pointer to copy to
/// \param[in] keyword name of keyword to copy
int copy_keyword(fitsfile* fpin, fitsfile* fpout, char* keyword);

