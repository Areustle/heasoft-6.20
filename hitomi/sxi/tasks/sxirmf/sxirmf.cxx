/// \file sxirmf.cxx
/// \brief Calculate the response for the SXI based on tables containing parameters for the response components
/// \author Andy Sargent
/// \date $Date: 2016/07/06 18:26:18 $
/// \version 1.0

/**

\defgroup tool_sxirmf SXI RMF (sxirmf)

@ingroup mod_sxi_tasks

Calculate the response for the SXI based on tables containing parameters 
for the response components. 

Source files:

  sxirmf.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/mission/lib/rmflib

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-29   MSD    Clean-up code

*/

#define AHLABEL tool_sxirmf
#define AHCVSID "$Id: sxirmf.cxx,v 1.41 2016/07/06 18:26:18 mdutka Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include "rmflib/rmflib.h"
#include "ahmath/ahmath.h"
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahgen/ahgen.h"

// Regional includes
#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas.h"    // expand_item_list

#include "hdcal.h"    //Caldb query utilities

#include <vector>
#include <sstream>

/** \addtogroup tool_sxirmf
 *  @{
 */

#define MAXSUBREG 250
#define EVTOKEV 0.001

/// \brief Define parameter file holding information
struct Params {
  
  Params(): m_eminin(0), m_dein(0), m_nchanin(0), m_ngrid(0), 
            m_nchanin_total(0), m_eminout(0.), m_deout(0.), m_nchanout(0),
            m_rmfthresh(0.) {}

  std::string m_phafile;        ///< Input PHA file with WMAP, or none
  std::string m_rmfparam;       ///< Output response file name
  std::string m_outfile;        ///< SXI line response table

  std::string m_dein_str;       ///< comma deliminated list of grid energy spacings for each grid
  std::string m_nchanin_str;    ///< comma deliminated list of the number of channels within each grid
  double m_eminin_first;        ///< first element energy grid 
  double * m_eminin;            ///< list of start energies for energy grids 
  double * m_dein;              ///< list of energy spacing for grids
  int * m_nchanin;              ///< list of the number of channels in each grid
  int m_ngrid;                  ///< The total number of grids
  int m_nchanin_total;          ///< The total number of matrix elements in all grids

  double m_eminout;             ///< Minimum boundary energy (default 0 eV)
  double m_deout;               ///< Boundary grid energy (default 6 eV)
  int m_nchanout;               ///< Number of channels (default 4096 chan)
  double m_rmfthresh;           ///< Response matrix low threshold

};

/// \brief Define response data from RMF CALDB file
struct ResponseData {

  ResponseData(): m_cistatus(0),m_nsubreg(0),m_corner_detx(0),m_corner_dety(0), 
                  m_energy_grid(0),m_primary_fwhm(0),m_secondary_offset(0),
                  m_secondary_amp(0),m_secondary_fwhm(0),m_escape_offset(0),
                  m_escape_amp(0),m_escape_fwhm(0),m_flu_offset(0),m_flu_amp(0),
                  m_flu_fwhm(0),m_con_amp(0) {}


  // Keyword values
  int m_cistatus;               ///< Charge injection of status
  int m_nsubreg;                ///< Number of subregions present

  int ** m_corner_detx;          ///< Corner DETX of subregion   
  int ** m_corner_dety;          ///< Corner DETY of subregion 

  double ** m_energy_grid;       ///< grid of energies for subregion
  double ** m_primary_fwhm;      ///< response shape, main peak FWHM
  double ** m_secondary_offset;  ///< response shape, secondary peak center
  double ** m_secondary_amp;     ///< response shape, secondary peak amplitude
  double ** m_secondary_fwhm;    ///< response shape, secondary peak FWHM
  double ** m_escape_offset;     ///< response shape, escape peak center
  double ** m_escape_amp;        ///< response shape, escape peak amplitude
  double ** m_escape_fwhm;       ///< response shape, escape peak FWHM
  double ** m_flu_offset;        ///< response shape, Si fluorescent peak center
  double ** m_flu_amp;           ///< response shape, Si fluorescent peak amplitude
  double ** m_flu_fwhm;          ///< response shape, Si fluorescent peak FWHM
  double ** m_con_amp;           ///< response shape, constant component

};

/// \brief Get parameter values
/// \param[out] param           Structure containing data from parameter file
void getPar(Params & param);

/// \brief Copy contents of infile to outfile; initialize CALDB data into structures
/// \param[in] param            Structure containing data from parameter file
/// \param[out] responseData    Structure to hold SXI line response table data
/// \param[out] rmfdat          RMF structure to be written to output file
/// \param[out] fppha           Input PHA file with WMAP
/// \param[out] fptime          Time validity file
/// \param[out] wmapimg         WMAP image from PHA file
/// \param[out] num_subregions  Number of response regions to calculate
/// \param[out] egrid_size      Length of energy grid array
/// \param[out] xlength         Total length of x-axis in WMAP
/// \param[out] ylength         Total length of y-axis in WMAP
/// \param[out] xoffset         Pixel offset for x-coordinate in WMAP
/// \param[out] yoffset         Pixel offset for y-coordinate in WMAP
/// \param[out] wmrebin         Rebinning factor for pixels in WMAP
/// \param[out] wmap_detxmin    Corresponding DETX offset coordinate in WMAP
/// \param[out] wmap_detymin    Corresponding DETY offset coordinate in WMAP
/// \param[out] ematcen         Average of low and high matrix energies
/// \param[out] echancen        Energy boundary average
void initialize(Params & param, ResponseData & responseData, rmflib::RMFData& rmfdat, ahfits::FilePtr & fppha, 
                ahfits::FilePtr & fptime, ahfits::Img2dLng & wmapimg, int & num_subregions, int & egrid_size,
                int & xlength, int & ylength, int & xoffset, int & yoffset, int & wmrebin,
                int & wmap_detxmin, int & wmap_detymin, double ** ematcen, double ** echancen);

/// \brief Main event loop, calculate response for each energy
/// \param[in] responseData     Structure to hold SXI line response table data
/// \param[in] param            Structure containing data from parameter file
/// \param[in,out] rmfdat       RMF structure to be written to output file
/// \param[in] wmapimg          WMAP image from PHA file
/// \param[in] num_subregions   Number of response regions to calculate
/// \param[in] egrid_size       Length of energy grid array
/// \param[in] xlength          Total length of x-axis in WMAP
/// \param[in] ylength          Total length of y-axis in WMAP
/// \param[in] xoffset          Pixel offset for x-coordinate in WMAP
/// \param[in] yoffset          Pixel offset for y-coordinate in WMAP
/// \param[in] wmrebin          Rebinning factor for pixels in WMAP
/// \param[in] wmap_detxmin     Corresponding DETX offset coordinate in WMAP
/// \param[in] wmap_detymin     Corresponding DETY offset coordinate in WMAP
/// \param[in] ematcen          Average of low and high matrix energies
/// \param[in] echancen         Energy boundary average
void doWork(ResponseData responseData, Params param, rmflib::RMFData& rmfdat, 
            ahfits::Img2dLng wmapimg, int num_subregions, int egrid_size,
            int xlength, int ylength, int xoffset, int yoffset, int wmrebin,
            int wmap_detxmin, int wmap_detymin, double * ematcen, double * echancen);

/// \brief Close file pointers
/// \param[out] fppha           File pointer to input PHA file with WMAP
/// \param[out] fptime          File pointer to time validity file
/// \param[out] rmfdat          struct containing data for response file
/// \param[out] param           Structure containing data from parameter file
void finalize(ahfits::FilePtr & fppha, ahfits::FilePtr & fptime, 
              rmflib::RMFData& rmfdat, Params & param);

/// \brief Returns a single matrix response row (i.e. for a single energy)
/// \param[in] egrid_index      Energy_mid index number
/// \param[in] energy_mid       Average of low and high matrix energies
/// \param[in] nchanout            Number of channels (default 4096 chan)
/// \param[in] deout            Boundary grid energy (default 6 eV)
/// \param[in] eminout          Minimum boundary energy (default 0 eV)
/// \param[in] ecen             Energy boundary average
/// \param[in] pri_fwhm         response shape, main peak FWHM
/// \param[in] sec_offset       response shape, secondary peak center
/// \param[in] sec_amp          response shape, secondary peak amplitude
/// \param[in] sec_fwhm         response shape, secondary peak FWHM
/// \param[in] escape_offset    response shape, escape peak center
/// \param[in] escape_amp       response shape, escape peak amplitude
/// \param[in] escape_fwhm      response shape, escape peak FWHM
/// \param[in] flu_offset       response shape, Si fluorescent peak center
/// \param[in] flu_amp          response shape, Si fluorescent peak amplitude
/// \param[in] flu_fwhm         response shape, Si fluorescent peak FWHM
/// \param[in] con_amp          response shape, constant component
/// \param[out] response        Output response for subregion
int makeResponse(int egrid_index, double energy_mid, int nchanout,
                 double deout, double eminout, double * ecen,
                 double pri_fwhm, double sec_offset, double sec_amp, double sec_fwhm, 
                 double escape_offset, double escape_amp, double escape_fwhm, 
                 double flu_offset, double flu_amp, double flu_fwhm, double con_amp, 
                 double * response);

/// \brief Read data from input RMF file
/// \param[in] rmffile          Name of input RMF file with response data
/// \param[out] responseData    Structure to hold SXI line response table data
/// \param[in] time_obs         Average observing time
/// \param[in] dateobs          Value of DATE-OBS keyword
/// \param[in] telescop         Value of TELESCOP keyword
/// \param[in] instrume         Value of INSTRUME keyword
/// \param[in] datamode         Value of DATAMODE keyword
/// \param[in] cistatus         Charge injection of status
/// \param[in] ciperiod         Spacing of CI rows in ACTY
/// \param[in] cioffset         Offset in ACTY of second CI row
/// \param[in] cifirst          ACTY of first CI row
/// \param[in] wmap_detxmax     Maximum x-coordinate of subregion
/// \param[in] wmap_detymax     Maximum y-coordinate of subregion
/// \param[in] wmap_detxmin     Minimum x-coordinate of subregion
/// \param[in] wmap_detymin     Minimum y-coordinate of subregion
/// \param[out] num_subregions  Number of response regions to calculate
/// \param[in] egrid_size       Length of energy grid array
void loadResponseData(std::string & rmffile, ResponseData & responseData,
                      double time_obs, std::string dateobs, std::string telescop, std::string instrume, std::string datamode, 
                      double cistatus, int wmap_detxmax, int wmap_detymax, int wmap_detxmin,
                      int wmap_detymin, int & num_subregions, int & egrid_size);

/// \brief Deallocate memory from response data arrays
/// \param[out] responseData    Structure to hold SXI line response table data
void destroyResponseData(ResponseData & responseData);

/// \brief parse keyword string value from CALDB file to obtain double value 
/// \param[in] keystr           Input string of format KEYWORDSTR(0) to parse
/// \param[out] keydbl          Output value from string
void parseCALDBKeywordDbl(std::string keystr, double & keydbl);

// ****************************************************************************

/// \brief sxirmf tool
int main(int argc, char** argv) {

  Params param;                      // Structure to store parameters for par file
  ResponseData responseData;         // Structure to hold SXI line response table data
  
  ahfits::FilePtr fppha = 0;         // File pointer to SXI PHA file
  ahfits::FilePtr fptime = 0;        // File pointer to time validity file 

  ahfits::Img2dLng wmapimg;          // WMAP image from PHA file
  int num_subregions = 0;            // Number of response regions to calculate
  int egrid_size = 0;                // Length of energy grid array

  int xlength = 0;                   // Total length of x-axis in WMAP
  int ylength = 0;                   // Total length of y-axis in WMAP
  int xoffset = 0;                   // Pixel offset for x-coordinate in WMAP
  int yoffset = 0;                   // Pixel offset for y-coordinate in WMAP
  int wmrebin = 0;                   // Rebinning factor for pixels in WMAP

  int wmap_detxmin = 0;              // Corresponding DETX offset coordinate in WMAP
  int wmap_detymin = 0;              // Corresponding DETY offset coordinate in WMAP

  double * ematcen = NULL;           // Average of low and high matrix energies
 
  double * echancen = NULL;          // Energy boundary average

  rmflib::RMFData rmfdat;            //struct which stores all the data 
                                     //neccesary for the output response matrix

  int status = ahapp::startUp(argc, argv, TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(param);
      ahapp::writeParametersToLog();
      initialize(param,responseData,rmfdat,fppha,fptime,wmapimg,num_subregions,egrid_size,xlength,
                 ylength,xoffset,yoffset,wmrebin,wmap_detxmin,wmap_detymin,&ematcen,&echancen);
      doWork(responseData,param,rmfdat,wmapimg,num_subregions,egrid_size,xlength,ylength,
             xoffset,yoffset,wmrebin,wmap_detxmin,wmap_detymin,ematcen,echancen);
      finalize(fppha,fptime, rmfdat,param);
      ahapp::shutDown();
    } else {
      try {
        getPar(param);
        initialize(param,responseData,rmfdat,fppha,fptime,wmapimg,num_subregions,egrid_size,xlength,
                   ylength,xoffset,yoffset,wmrebin,wmap_detxmin,wmap_detymin,&ematcen,&echancen);
        doWork(responseData,param,rmfdat,wmapimg,num_subregions,egrid_size,xlength,ylength,
               xoffset,yoffset,wmrebin,wmap_detxmin,wmap_detymin,ematcen,echancen);
      } catch (const std::exception & x) {
          ahapp::writeParametersToLog();
          status = 1;
          AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fppha,fptime, rmfdat,param);
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception & x) {
          status = 1;
          AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;

} // end main

// ****************************************************************************

void getPar(Params & param) {

  param.m_phafile = ahapp::getParString("infile");
  param.m_outfile = ahapp::getParString("outfile");
  param.m_rmfparam = ahapp::getParString("rmfparam"); // CALDB

  param.m_eminin_first = ahapp::getParDouble("eminin");
  param.m_dein_str = ahapp::getParString("dein"); 
  param.m_nchanin_str = ahapp::getParString("nchanin");
  param.m_eminout = ahapp::getParDouble("eminout");
  param.m_deout = ahapp::getParDouble("deout"); 
  param.m_nchanout = ahapp::getParInt("nchanout");

  param.m_rmfthresh = ahapp::getParDouble("rmfthresh");

  // convert m_dein_str into an array of doubles
  char* dein_char=(char*)param.m_dein_str.c_str();
  char** items=0;
  int nitems_dein=0;
  int status=0;
  int trim=1;         // trim spaces
  int skip=1;         // exclude empty items
  int guard=0;        // do not protect against commas in parentheses
  items=expand_item_list(dein_char, &nitems_dein, ',',trim,skip,guard,&status);
  if (status != 0)
    AH_THROW_RUNTIME("invalid value for dein parameter; expect list of values separated by commas");
  param.m_dein = new double[nitems_dein];
  for (int ii=0; ii < nitems_dein; ++ii) {
    param.m_dein[ii] = atof(items[ii]);
  }
 
  //convert m_nchanin_str into an array of ints
  char* nchanin_char=(char*)param.m_nchanin_str.c_str();
  items=0;
  int nitems_nchanin=0;
  status=0;
  trim=1;         // trim spaces
  skip=1;         // exclude empty items
  guard=0;        // do not protect against commas in parentheses
  items=expand_item_list(nchanin_char, &nitems_nchanin, ',',trim,skip,guard,&status);
  if (status != 0)
    AH_THROW_RUNTIME("invalid value for nchanin parameter; expect list of values separated by a comma");
  param.m_nchanin = new int[nitems_nchanin];
  for (int ii=0; ii < nitems_nchanin; ++ii) {
    param.m_nchanin[ii] = atoi(items[ii]);
  }
 
  //store the number of grids into the param struct
  if (nitems_dein == nitems_nchanin) {
    param.m_ngrid = nitems_nchanin;
  } else {
    AH_THROW_RUNTIME("The number of energy grids do not match");
  }

  //convert eminin into an array of doubles
  param.m_eminin = new double[param.m_ngrid]; 
  param.m_eminin[0] = param.m_eminin_first;
  for (int ii = 0; ii < param.m_ngrid-1; ++ii) {
    param.m_eminin[ii+1] = param.m_eminin[ii] + param.m_nchanin[ii] * param.m_dein[ii];
  } 
  

  //store the total number of channels
  for (int ii = 0; ii < param.m_ngrid; ++ii){
    param.m_nchanin_total += param.m_nchanin[ii];
  }
  
  AH_INFO(ahlog::HIGH) << "There are " << param.m_ngrid << " energy intervals, with " 
                       << param.m_nchanin_total << " total energy bins." << std::endl;
  for (int ii = 0; ii < param.m_ngrid; ++ii) {
    AH_INFO(ahlog::LOW) << "For energy interval " << ii << std::endl;
    AH_INFO(ahlog::LOW) << "  Minimum input energy = " << param.m_eminin[ii] << std::endl;
    AH_INFO(ahlog::LOW) << "  Number of energy bins = " << param.m_nchanin[ii] << std::endl;
    AH_INFO(ahlog::LOW) << "  Energy bin size = " << param.m_dein[ii] << std::endl;
  }
 
} // end getPar

// ****************************************************************************

void initialize(Params & param, ResponseData & responseData, rmflib::RMFData& rmfdat, ahfits::FilePtr & fppha, 
                ahfits::FilePtr & fptime, ahfits::Img2dLng & wmapimg, int & num_subregions, int & egrid_size,
                int & xlength, int & ylength, int & xoffset, int & yoffset, int & wmrebin,
                int & wmap_detxmin, int & wmap_detymin, double ** ematcen, double ** echancen) {

  std::string inst;               // Name of current instrument
  std::string telescop;           // Name of current telescope
  std::string mtypestr;           // String to compare coordinate type in WMAP in PHA file
  std::string dateobs;            //string contain observation date and time
  std::string date;               //string containing date
  std::string time;               //string containing time  

  std::string string_cbd10001;    // boundary keyword
  std::string string_cbd20001;    // boundary keyword
  std::stringstream maxchan_ss;   // boundary keyword
  std::stringstream cistatus_ss;  // boundary keyword

  double tstart = 0.;             // Observation start time
  double tstop = 0.;              // Observation stop time
  std::string datamode;           // datamode keyword
  int cistatus = 0;               // Charge injection of status

  long long tlmin_chan = 0;       // CHANNEL column TLMIN in SPECTRUM extension
  long long tlmax_chan = 0;       // CHANNEL column TLMAX in SPECTRUM extension
  
  double time_obs = 0.;           // Average observation time
  long nx, ny;                    // coordinate lengths for WMAP
  int wmap_detxmax = 0;           // Corresponding DETX offset coordinate in WMAP
  int wmap_detymax = 0;           // Corresponding DETY offset coordinate in WMAP
  
  double * eminin_kev = 0;        // list of min energies kev
  double * dein_kev = 0;          // list of grid energies kev
  int * nchanin = 0;              // list of the number of energy bins

  double eminout_kev = 0.0;               //  Minimum boundary energy (default 0 eV)
  double deout_kev = 0.0;                 //  Boundary grid energy (default 6 eV)
  int nchanout = 0;                       //  Number of channels (default 4096 chan)
  double low_thresh = param.m_rmfthresh;  //  Response matrix low threshold

  double * ematstart = 0;         // 0...nchanin-1 input energy bin lower edge
  double * ematstop = 0;          // 0...nchanin-1 input energy bin lower edge
  double * echanstart = 0;        // 0...nchanout-1 output energy channel lower edge
  double * echanstop = 0;         // 0...nchanout-1 output energy channel upper edge

  // Open PHA file and load WMAP
  ahfits::open(param.m_phafile, "WMAP", &fppha);

  // Check if FITS file pointer is valid
  if(!ahfits::isImage(fppha)) AH_THROW_RUNTIME("No image in PHA file " + param.m_phafile);
  
  // Check for correct instrument and coordinate type
  inst = ahfits::getKeyValStr(fppha, "INSTRUME");
  telescop = ahfits::getKeyValStr(fppha, "TELESCOP");
  mtypestr = ahfits::getKeyValStr(fppha,"MTYPE1");
  if("SXI" != inst) AH_THROW_RUNTIME("INSTRUME keyword in inputfile should be SXI, not " + inst);
  if("DET" != mtypestr) AH_THROW_RUNTIME("Invalid coordinate type in WMAP image");

  // Get observation times, CI status
  tstart = ahfits::getKeyValDbl(fppha, "TSTART");
  tstop = ahfits::getKeyValDbl(fppha, "TSTOP");
  datamode = ahfits::getKeyValStr(fppha, "DATAMODE");
  cistatus = ahfits::getKeyValLLong(fppha, "CISTATUS");
  xlength = ahfits::getKeyValLLong(fppha, "NAXIS1");
  ylength = ahfits::getKeyValLLong(fppha, "NAXIS2");
  xoffset = ahfits::getKeyValLLong(fppha, "X-OFFSET");
  yoffset = ahfits::getKeyValLLong(fppha, "Y-OFFSET");
  wmrebin = ahfits::getKeyValLLong(fppha, "WMREBIN");
  dateobs = ahfits::getKeyValStr(fppha, "DATE-OBS");
 
  //split dateobs into date and time
  if (dateobs.find("T") != std::string::npos) {
    date = dateobs.substr(0,10);
    time = dateobs.substr(11,8);
  } else {
    AH_THROW_RUNTIME("unexpected value for DATE-OBS keyword: "+dateobs+"; expecting format YYYY-MM-DDThh:mm:ss");
  }

  // Get WMAP image data from PHA file
  ahfits::readImage(fppha, wmapimg, nx, ny);
  if(nx!=xlength || ny!=ylength) AH_THROW_RUNTIME("PHA WMAP image LENGTH not equal to NAXIS");

  // Get TLMIN and TLMAX from this extension
  ahfits::move(fppha, "SPECTRUM");
  ahfits::columnRange(fppha, "CHANNEL", tlmin_chan, tlmax_chan);
  AH_INFO(ahlog::LOW) << "CHANNEL of PHA file has TLMIN=("<<tlmin_chan<<"), TLMAX=("<<tlmax_chan<<")" << std::endl;

  // find corresponding DETX/DETY region from WMAP
  wmap_detxmin = (xoffset-1)*wmrebin+1;
  wmap_detymin = (yoffset-1)*wmrebin+1;
  wmap_detxmax = (xlength+xoffset-1)*wmrebin;
  wmap_detymax = (ylength+yoffset-1)*wmrebin;

  AH_INFO(ahlog::HIGH) << "Found WMAP with size " << xlength << "x" << ylength << std::endl;
  AH_INFO(ahlog::HIGH) << "WMAP lower left boundary is " << wmap_detxmin << "," << wmap_detymin << std::endl;
  AH_INFO(ahlog::HIGH) << "WMAP upper left boundary is " << wmap_detxmax << "," << wmap_detymax << std::endl; 

  // Calculate average observation time
  time_obs = (tstop+tstart)/2.0;
  AH_INFO(ahlog::LOW) << "Average observing time is " << time_obs << std::endl;

  // Load CALDB
  loadResponseData(param.m_rmfparam, responseData, time_obs, dateobs, telescop, inst, datamode, 
                   cistatus, wmap_detxmax, wmap_detymax,
                   wmap_detxmin, wmap_detymin, num_subregions, egrid_size);

  // set up energies for RMF file
  // MATRIX
  eminin_kev = new double[param.m_ngrid];     //  list of min energies kev
  dein_kev = new double[param.m_ngrid];       //  list of grid energies kev
  nchanin = new int[param.m_ngrid];           //  list of the number of energy bins
  for (int ii = 0; ii < param.m_ngrid; ++ii) {
    eminin_kev[ii] = param.m_eminin[ii]*EVTOKEV;
    dein_kev[ii] = param.m_dein[ii]*EVTOKEV;
    nchanin[ii] = param.m_nchanin[ii]; 
  }

  // EBOUNDS
  eminout_kev = param.m_eminout*EVTOKEV;
  deout_kev = param.m_deout*EVTOKEV;
  nchanout = param.m_nchanout; 
  
  // Allocate mememory for energy and channel bins
  ematstart = new double[param.m_nchanin_total];
  ematstop = new double[param.m_nchanin_total];
  *ematcen = new double[param.m_nchanin_total];

  echanstart = new double[nchanout];
  echanstop = new double[nchanout];
  *echancen = new double[nchanout];

  // Set up the matrix grids for channels and energy
  AH_DEBUG << "Setting up matrix grids for channels and energy." << std::endl;
  AH_DEBUG << "nchanout = " << nchanout << "deout = " << deout_kev << std::endl;  

  for (int ii = 0; ii < nchanout; ++ii){
    echanstart[ii] = eminout_kev + ii*deout_kev;
    echanstop[ii] = echanstart[ii] + deout_kev;
    (*echancen)[ii] = 0.5 * (echanstart[ii]+echanstop[ii]);
    if (ii == 0 && eminout_kev == 0) echanstart[ii] = 0.1 * deout_kev;
    AH_DEBUG << "Output channel number: " << ii << std::endl;
    AH_DEBUG << "Start energy: " << echanstart[ii] << std::endl;
    AH_DEBUG << "Stop energy: " << echanstop[ii] << std::endl;
    AH_DEBUG << "Center Energy: " << (*echancen)[ii] << std::endl;
  }

  AH_DEBUG << "MATRIX extension energy grid" << std::endl;
  AH_DEBUG << "Number of grids: " << param.m_ngrid << std::endl;
  
  int ii = 0; //This is the index of input energy grid
  for (int kk = 0; kk < param.m_ngrid; ++kk){
    AH_DEBUG << "Grid spacing number: " << kk << std::endl;
    AH_DEBUG << "number of channels with this spacing: " << nchanin[kk] << std::endl;
    AH_DEBUG << "energy spacing: " << dein_kev[kk] << std::endl;
    for (int jj = 0; jj < nchanin[kk]; ++jj) {
      if (ii > param.m_nchanin_total) {
        delete [] eminin_kev; eminin_kev=0;
        delete [] dein_kev; dein_kev=0;
        delete [] nchanin; nchanin=0;
        delete [] ematstart; ematstart=0;
        delete [] ematstop; ematstop=0;
        delete [] echanstart; echanstart=0;
        delete [] echanstop; echanstop=0;
        AH_THROW_RUNTIME("Energy channel index is out of range");
      }
      ematstart[ii] = eminin_kev[kk] + jj * dein_kev[kk];
      ematstop[ii] = ematstart[ii] + dein_kev[kk];
      (*ematcen)[ii] = 0.5 * (ematstart[ii] + ematstop[ii]);
      AH_DEBUG << "Input channel number: " << ii << std::endl;
      AH_DEBUG << "Start energy: " << ematstart[ii] << std::endl;
      AH_DEBUG << "Stop energy: " << ematstop[ii] << std::endl;
      AH_DEBUG << "Center energy: " << (*ematcen)[ii] << std::endl;
      ii++;
    }  
  } 
  
  //
  // RMF FITS file:
  // MATRIX extension (binary table) - Columns
  // TTYPE1 'ENERG_LO' TFORM1 '1E  ' TUNIT1 = 'keV  '
  // TTYPE2 'ENERG_HI' TFORM2 '1E  ' TUNIT2 = 'keV  '
  // TTYPE3 'N_GRP   ' TFORM3 '1I  ' TUNIT3 = '     '
  // TTYPE4 'F_CHAN  ' TFORM4 'nJ  ' TUNIT4 = '     '
  // TTYPE5 'N_CHAN  ' TFORM5 'nJ  ' TUNIT5 = '     '
  // TTYPE6 'MATRIX  ' TFORM6 'PE((1,nchanout),ngrp) ' TUNIT6 = '     '
  // 
  // EBOUNDS extension (binary table) - Columns
  // TTYPE1 'CHANNEL ' TFORM1 '1I  ' TUNIT1 = '     '
  // TTYPE2 'E_MIN   ' TFORM2 '1E  ' TUNIT2 = 'keV  '
  // TTYPE3 'E_MAX   ' TFORM3 '1E  ' TUNIT3 = 'keV  '

  
  // Set up RMF file
  AH_DEBUG << "Setting up RMF file." << std::endl;
  rmflib::createRMFFile(&rmfdat, param.m_outfile);

  //set header keywords for output RMF file                                                                                           
  rmfdat.m_lo_thresh = low_thresh;
  rmfdat.m_telescop = telescop;
  rmfdat.m_filter = "";
  rmfdat.m_instrume = inst;
  rmfdat.m_detnam = "PIXEL";
  rmfdat.m_chantype = "PI";
  rmfdat.m_time = time;
  rmfdat.m_date = date;
  rmfdat.m_tlmin_channel = 0;
  rmfdat.m_tlmax_channel = nchanout-1;
  rmfdat.m_tlmin_fchan = 0;
  rmfdat.m_tlmax_fchan = nchanout-1;

  rmfdat.m_description_matrix = "SXI RMF based on ground calibration";
  rmfdat.m_description_ebounds = "Energy boundaries of spectral channels";

  // Boundary keywords
  // Energy grid boundary keyword
  maxchan_ss << rmfdat.m_tlmax_channel;
  string_cbd10001 = "CHAN(0-" + maxchan_ss.str() + ")";
  // CI boundary keyword
  cistatus_ss << cistatus;
  string_cbd20001 = "CISTATUS(" + cistatus_ss.str() + ")";

  //set channel and energy grids
  AH_DEBUG << "setting input energy  grid" << std::endl;
  rmflib::setInputEnergies(&rmfdat, ematstart, ematstop, *ematcen, param.m_nchanin_total);
  AH_DEBUG << "setting channel energies" << std::endl;
  rmflib::setChannelEnergies(&rmfdat, echanstart, echanstop, *echancen, nchanout);


  //create Ebounds extensions, and write boundary keywords
  AH_DEBUG << "writing EBOUND HDU" << std::endl;
  rmflib::writeEboundsHDU(&rmfdat);
  ahfits::writeKeyValLLong(rmfdat.m_ahffp, "CISTATUS", cistatus, "Charge inject 1=on/ 0=off"); 

  //create Matrix extensions, and write boundary keywords
  AH_DEBUG << "Setting up matrix HDU" << std::endl;
  rmflib::createMatrixHDU(&rmfdat); 
  ahfits::move(rmfdat.m_ahffp,"MATRIX");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD10001",string_cbd10001,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD20001",string_cbd20001,"Keyword information for Caltools Software");  
  
  //deallocate memory for dynamic arrays
  delete [] eminin_kev; eminin_kev=0;
  delete [] dein_kev; dein_kev=0;
  delete [] nchanin; nchanin=0;
  delete [] ematstart; ematstart=0;
  delete [] ematstop; ematstop=0;
  delete [] echanstart; echanstart=0;
  delete [] echanstop; echanstop=0;

  AH_DEBUG << "done setting up rmf file" << std::endl;

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

} // end initialize

// ****************************************************************************

void doWork(ResponseData responseData, Params param, rmflib::RMFData& rmfdat,
            ahfits::Img2dLng wmapimg, int num_subregions, int egrid_size,
            int xlength, int ylength, int xoffset, int yoffset, int wmrebin,
            int wmap_detxmin, int wmap_detymin, double * ematcen, double * echancen) {

  double sum_of_weights = 0.0;    // Sum of all subregion weights
  double * weight;                // Sum of WMAP values for each subregion
  double * response;              // Sum of WMAP values for each subregion
  double * totmatrix;             // Weighted response for a single energy

  ahmath::IndexType idx0 = 0;     // Indexing value for search and interpolate functions
  bool extrap;                    // Extrapolate boolean for search function

  // Get par file parameters
  
  double eminout_kev = param.m_eminout*EVTOKEV;
  double deout_kev = param.m_deout*EVTOKEV;
  int nchanin_total = param.m_nchanin_total; 
  int nchanout = param.m_nchanout; 

  // Corner coordinates
  int a_x = 0;                    // DETX coordinates 
  int b_x = 0;                    // 
  int c_x = 0;                    // 
  int d_x = 0;                    // 
  int a_y = 0;                    // DETY coordinates
  int b_y = 0;                    // 
  int c_y = 0;                    // 
  int d_y = 0;                    // 

  // WMAP indeces
  int w_x = 0;                    // Resized x-index to get WMAP coordinates
  int w_y = 0;                    // Resized y-index to get WMAP coordinates

  // WMAP cross-products
  int cp_a = 0;                   // Cross-product (W-A) x (B-A)
  int cp_b = 0;                   // Cross-product (W-B) x (C-B)
  int cp_c = 0;                   // Cross-product (W-C) x (D-C)
  int cp_d = 0;                   // Cross-product (W-D) x (A-D)

  int s_a = 0;                    // Sign of cp_a (boolean, 1 == negative, 0 otherwise) 
  int s_b = 0;                    // Sign of cp_b (boolean, 1 == negative, 0 otherwise) 
  int s_c = 0;                    // Sign of cp_c (boolean, 1 == negative, 0 otherwise) 
  int s_d = 0;                    // Sign of cp_d (boolean, 1 == negative, 0 otherwise) 

  // Initialize response parameters
  double pri_fwhm = 0.0;          // response shape, main peak FWHM
  double sec_offset = 0.0;        // response shape, secondary peak center
  double sec_amp = 0.0;           // response shape, secondary peak amplitude
  double sec_fwhm = 0.0;          // response shape, secondary peak FWHM
  double escape_offset = 0.0;     // response shape, escape peak center
  double escape_amp = 0.0;        // response shape, escape peak amplitude
  double escape_fwhm = 0.0;       // response shape, escape peak FWHM
  double flu_offset = 0.0;        // response shape, Si fluorescent peak center
  double flu_amp = 0.0;           // response shape, Si fluorescent peak amplitude
  double flu_fwhm = 0.0;          // response shape, Si fluorescent peak FWHM
  double con_amp = 0.0;           // response shape, constant component

  // Allocate space for matrices
  weight = new double[num_subregions];
  response = new double[nchanout];
  totmatrix = new double[nchanout];

  // Loop through subregions
  AH_INFO(ahlog::HIGH) << "Calculating weights from WMAP." << std::endl;
  for(int jj = 0; jj < num_subregions; ++jj) {
    weight[jj] = 0;
    // Get corner data
    a_x = responseData.m_corner_detx[jj][0];
    a_y = responseData.m_corner_dety[jj][0];
    b_x = responseData.m_corner_detx[jj][1];
    b_y = responseData.m_corner_dety[jj][1];
    c_x = responseData.m_corner_detx[jj][2];
    c_y = responseData.m_corner_dety[jj][2];
    d_x = responseData.m_corner_detx[jj][3];
    d_y = responseData.m_corner_dety[jj][3];
    // Weight in subregion is sum of WMAP image within x/y min/max
    for(int p = 0; p < xlength; ++p) {
      for(int q = 0; q < ylength; ++q) {
        // Resize index to get WMAP coordinates
        w_x = p*wmrebin+wmap_detxmin;
        w_y = q*wmrebin+wmap_detymin;
        // Check if (wmap_detx, wmap_dety) is confined in the rectangle
        // formed by the four corner points
        // Use cross products of the vectors
        // A x B = (ax*by - ay*bx), where A = (ax, ay), B = (bx, by)
        // (W-A) x (B-A)
        cp_a = (w_x-a_x)*(b_y-a_y)-(w_y-a_y)*(b_x-a_x);
        // (W-B) x (C-B)
        cp_b = (w_x-b_x)*(c_y-b_y)-(w_y-b_y)*(c_x-b_x);
        // (W-C) x (D-C)
        cp_c = (w_x-c_x)*(d_y-c_y)-(w_y-c_y)*(d_x-c_x);
        // (W-D) x (A-D)
        cp_d = (w_x-d_x)*(a_y-d_y)-(w_y-d_y)*(a_x-d_x);

        // If W is confined in the square, the signs of the cross-products
        // are identical.
        // If W is on the subregion boundary, one of the products is zero
        s_a = std::signbit(cp_a);
        s_b = std::signbit(cp_b);
        s_c = std::signbit(cp_c);
        s_d = std::signbit(cp_d);

        // Calculate weighting of subregion
        if((s_a == s_b && s_b == s_c && s_c == s_d && s_d == s_a)
           || cp_a == 0 || cp_b == 0 || cp_c == 0 || cp_d == 0) {
          if (wmapimg[p][q] > 0) {
            weight[jj] += wmapimg[p][q];
          }
        }
      }
    }
    // Calculate total RMF weighting
    sum_of_weights += weight[jj];
    AH_INFO(ahlog::HIGH) << "Weight for region " << jj << " is " << weight[jj] << ", sum is " << sum_of_weights << std::endl;
  }

  // Verify sum of weights is valid
  if(0 == sum_of_weights) {
    delete [] ematcen; ematcen=0;
    delete [] echancen; echancen=0;
    delete [] weight; weight=0;
    delete [] response; response=0;
    delete [] totmatrix; totmatrix=0;
    destroyResponseData(responseData);
    AH_THROW_RUNTIME("Sum of all weights in subregions is zero, when it should not be");
  }

  // Main algorithm
  // Begin loop over matrix energy grid, calculating the response for each energy row
  AH_INFO(ahlog::HIGH) << "Looping over matrix grid." << std::endl;
  for(int ii = 0; ii < nchanin_total; ++ii) {

    if (ii % 1000 == 0) {
      AH_OUT << "Completed " << ii << " energy channels out of " << nchanin_total << std::endl;
    }

    AH_DEBUG << "Initializing response and weighted response for bin " << ii << std::endl;  
    // Initialize response and weighted response (for single energy row)
    for(int jj = 0; jj < nchanout; ++jj) {
      response[jj] = 0.0;
      totmatrix[jj] = 0.0;
    }

    // Calculate response for each subregion with non-zero weight 
    for(int kk = 0; kk < num_subregions; ++kk) { // loop through subregions
      // if WMAP has flux, make a response
      AH_DEBUG << "Calculating bin " << ii << " subregion " << kk << "." << std::endl; 
      if(weight[kk] <= 0) { 
        continue;
        AH_DEBUG << "Skipping, weight = 0." << std::endl;
      }
      // Use ahmath to search for idx0 value
      // idx0 is used in the following interpolations as well as search in next iteration
      // +++ Need to use arrays and not a reference to first element...it's possible vector 
      // not contiguous in memory
     
      idx0 = ahmath::search(ematcen[ii],responseData.m_energy_grid[kk],egrid_size,extrap,idx0);
      AH_DEBUG << "Interpolating response components." << std::endl;
      pri_fwhm = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_primary_fwhm[kk],egrid_size,ahmath::TWOPOINT,idx0);
      sec_offset = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_secondary_offset[kk],egrid_size,ahmath::TWOPOINT,idx0);
      sec_amp = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_secondary_amp[kk],egrid_size,ahmath::TWOPOINT,idx0);
      sec_fwhm = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_secondary_fwhm[kk],egrid_size,ahmath::TWOPOINT,idx0);
      escape_offset = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_escape_offset[kk],egrid_size,ahmath::TWOPOINT,idx0);
      escape_amp = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_escape_amp[kk],egrid_size,ahmath::TWOPOINT,idx0);
      escape_fwhm = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_escape_fwhm[kk],egrid_size,ahmath::TWOPOINT,idx0);
      flu_offset = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_flu_offset[kk],egrid_size,ahmath::TWOPOINT,idx0);
      flu_amp = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_flu_amp[kk],egrid_size,ahmath::TWOPOINT,idx0);
      flu_fwhm = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_flu_fwhm[kk],egrid_size,ahmath::TWOPOINT,idx0);
      con_amp = ahmath::interpolate(ematcen[ii],responseData.m_energy_grid[kk],responseData.m_con_amp[kk],egrid_size,ahmath::TWOPOINT,idx0);

      AH_DEBUG << "Creating response for channel " <<  ii << ", subregion " << kk << std::endl; 
      // makeResponse returns a single row
      if(makeResponse(ii,ematcen[ii],nchanout,deout_kev,eminout_kev,echancen,pri_fwhm,sec_offset,sec_amp,sec_fwhm,
                      escape_offset,escape_amp,escape_fwhm,flu_offset,flu_amp,flu_fwhm,con_amp,response)) {
        // If there was an error during reading of FWHM, clean up and throw
        // error
        AH_DEBUG << "Combining with weight " << weight[kk] << " to current matrix." << std::endl;
        delete [] ematcen; ematcen=0;
        delete [] echancen; echancen=0;
        delete [] weight; weight=0;
        delete [] response; response=0;
        delete [] totmatrix; totmatrix=0;
        destroyResponseData(responseData);
        AH_THROW_RUNTIME("Error creating response.");
      }

      // Weight and copy data output from makeResponse
      for(int jj = 0; jj < nchanout; ++jj) {
        response[jj] *= weight[kk]/sum_of_weights;
        totmatrix[jj] += response[jj];
      }

    } // end loop on subregions

    // write each row of the output response matrix
    AH_DEBUG << "Writing matrix row " << ii << " to RMF" << std::endl;
    rmflib::writeMatrixRow(&rmfdat, totmatrix, nchanout, ii);
   
  } // end loop on matrix energy grid

  AH_INFO(ahlog::HIGH) << "Finished calculating response matrix, closing RMF file." << std::endl;
  // Release memory from arrays
  delete [] ematcen; ematcen=0;
  delete [] echancen; echancen=0;
  delete [] weight; weight=0;
  delete [] response; response=0;
  delete [] totmatrix; totmatrix=0;
  destroyResponseData(responseData);
  
} // end doWork

// ****************************************************************************

void finalize(ahfits::FilePtr & fppha, ahfits::FilePtr & fptime, 
              rmflib::RMFData& rmfdat, Params & param) {

  // deallocate memory in parameter struct
  delete [] param.m_dein; param.m_dein=0;
  delete [] param.m_nchanin; param.m_nchanin=0;
  delete [] param.m_eminin; param.m_eminin=0;

  // Close all FITS file pointers
  ahfits::close(fppha);
  ahfits::close(fptime);

  //1. deallocating memory for energy_lo, energy_hi, and matrixrow
  //2. writenumgroup and num element keywords
  //3. close the RMF fits file 
  rmflib::closeRMFFile(&rmfdat);

} // end finalize

// ****************************************************************************

int makeResponse(int egrid_index, double energy_mid, int nchanout,
                 double deout, double eminout, double * ecen,
                 double pri_fwhm, double sec_offset, double sec_amp, double sec_fwhm, 
                 double escape_offset, double escape_amp, double escape_fwhm, 
                 double flu_offset, double flu_amp, double flu_fwhm, double con_amp, 
                 double * response) {

  double * pri_matrix;            // Response matrix for primary energy peak
  double * sec_matrix;            // Response matrix for secondary energy peak
  double * escape_matrix;         // Response matrix for escape energy peak
  double * flu_matrix;            // Response matrix for fluorescent energy peak
  double * con_matrix;            // Response matrix for continuum energy
  double * totmatrix;             // Sum of all response matrices, normalized

  double Eo = energy_mid;         // Center of energy for primary peak
  double Esec = 0.0;              // Center of energy offset for secondary peak
  double Eescape = 0.0;           // Center of energy offset for escape peak
  double Eflu = 0.0;              // Center of energy offset for fluorescent peak

  double all_amp;                 // Total energy amplitude of all peaks
  double pri_frac = 0.0;          // Fraction of response amplitude dominated by primary energy peak
  double sec_frac = 0.0;          // Fraction of response amplitude dominated by secondary energy peak
  double escape_frac = 0.0;       // Fraction of response amplitude dominated by escape energy peak
  double flu_frac = 0.0;          // Fraction of response amplitude dominated by fluorescent energy peak
  double con_frac = 0.0;          // Fraction of response amplitude dominated by continuum energy 

  double renorm_pri = 0.0;        // Normalization factor for primary energy peak
  double renorm_sec = 0.0;        // Normalization factor for secondary energy peak
  double renorm_escape = 0.0;     // Normalization factor for escape energy peak
  double renorm_flu = 0.0;        // Normalization factor for fluorescent energy peak
  double renorm_con = 0.0;        // Normalization factor for continuum energy

  int chanstart = 0;
  int chanstop = 0;

  // Bounds to calculate the Gaussian components
  double de_numsigma = 5.0; // +++ this should not be hard-coded
  double de_pri = (de_numsigma*ahmath::convertFWHM2sigma(pri_fwhm));
  double de_sec = (de_numsigma*ahmath::convertFWHM2sigma(sec_fwhm));
  double de_escape = (de_numsigma*ahmath::convertFWHM2sigma(escape_fwhm));
  double de_flu = (de_numsigma*ahmath::convertFWHM2sigma(flu_fwhm));

  if(pri_fwhm == 0) {
    AH_INFO(ahlog::HIGH) << " *** For energy " << egrid_index << " keV:\n";
    AH_INFO(ahlog::HIGH) << "Primary FWHM should not be zero" << std::endl;
    return 1;
  }
  if(sec_fwhm == 0) {
    AH_INFO(ahlog::HIGH) << " *** For energy " << egrid_index << " keV:\n";
    AH_INFO(ahlog::HIGH) << "Secondary FWHM should not be zero" << std::endl;
    return 1;
  }
  if(escape_fwhm == 0) {
    AH_INFO(ahlog::HIGH) << " *** For energy " << egrid_index << " keV:\n";
    AH_INFO(ahlog::HIGH) << "Escape FWHM should not be zero" << std::endl;
    return 1;
  }
  if(flu_fwhm == 0) {
    AH_INFO(ahlog::HIGH) << " *** For energy " << egrid_index << " keV:\n";
    AH_INFO(ahlog::HIGH) << "Fluorescent FWHM should not be zero" << std::endl;
    return 1;
  }

  pri_matrix = new double[nchanout];
  sec_matrix = new double[nchanout];
  con_matrix = new double[nchanout];
  escape_matrix = new double[nchanout];
  flu_matrix = new double[nchanout];
  totmatrix = new double[nchanout];

  // initialize all response matrix components to zero
  for(int ii = 0; ii < nchanout; ++ii) {
    pri_matrix[ii] = 0.0;
    sec_matrix[ii] = 0.0;
    con_matrix[ii] = 0.0;
    escape_matrix[ii] = 0.0;
    flu_matrix[ii] = 0.0;
    totmatrix[ii] = 0.0;
  }

  // Calculate peaks (keV)

  // Primary Guassian peak
  rmflib::energy2Chan(Eo-de_pri,eminout,deout,nchanout,chanstart);
  rmflib::energy2Chan(Eo+de_pri,eminout,deout,nchanout,chanstop);
  rmflib::gaussianPeak(Eo,chanstart,chanstop,nchanout,pri_fwhm,ecen,pri_matrix,renorm_pri);

  // Secondary Gaussian peak
  Esec = Eo - sec_offset;
  rmflib::energy2Chan(Esec-de_sec,eminout,deout,nchanout,chanstart);
  rmflib::energy2Chan(Esec+de_sec,eminout,deout,nchanout,chanstop);
  rmflib::gaussianPeak(Esec,chanstart,chanstop,nchanout,sec_fwhm,ecen,sec_matrix,renorm_sec);

  // Escape peak component
  Eescape = Eo - escape_offset;
  rmflib::energy2Chan(Eescape-de_escape,eminout,deout,nchanout,chanstart);
  rmflib::energy2Chan(Eescape+de_escape,eminout,deout,nchanout,chanstop);
  rmflib::gaussianPeak(Eescape,chanstart,chanstop,nchanout,escape_fwhm,ecen,escape_matrix,renorm_escape);

  // Fluorescent peak component
  Eflu = Eo - flu_offset;
  rmflib::energy2Chan(Eflu-de_flu,eminout,deout,nchanout,chanstart);
  rmflib::energy2Chan(Eflu+de_flu,eminout,deout,nchanout,chanstop);
  rmflib::gaussianPeak(Eflu,chanstart,chanstop,nchanout,flu_fwhm,ecen,flu_matrix,renorm_flu);

  // Constant continuum component
  rmflib::energy2Chan(eminout,eminout,deout,nchanout,chanstart);
  rmflib::energy2Chan(Eo,eminout,deout,nchanout,chanstop);
  rmflib::continuumPeak(Eo,chanstart,chanstop,nchanout,ecen,con_matrix,renorm_con);

  // Combine the properly normalized components
  for(int ii = 0; ii < nchanout; ++ii) {
    all_amp = 1.0 + sec_amp + escape_amp + flu_amp + con_amp;

    pri_frac = 1.0/all_amp;
    sec_frac = sec_amp/all_amp;
    escape_frac = escape_amp/all_amp;
    flu_frac = flu_amp/all_amp;
    con_frac = con_amp/all_amp;

    totmatrix[ii] = pri_frac*pri_matrix[ii]/renorm_pri;
    totmatrix[ii] += sec_frac*sec_matrix[ii]/renorm_sec;
    totmatrix[ii] += escape_frac*escape_matrix[ii]/renorm_escape;
    totmatrix[ii] += flu_frac*flu_matrix[ii]/renorm_flu;
    totmatrix[ii] += con_frac*con_matrix[ii]/renorm_con;
    // Copy to output matrix 
    response[ii] = totmatrix[ii];
  }

  delete [] pri_matrix; pri_matrix=0;
  delete [] sec_matrix; sec_matrix=0;
  delete [] escape_matrix; escape_matrix=0;
  delete [] flu_matrix; flu_matrix=0;
  delete [] con_matrix; con_matrix=0;
  delete [] totmatrix; totmatrix=0;

  return 0;

}

// ****************************************************************************

void loadResponseData(std::string & rmffile, ResponseData & responseData,
                      double time_obs, std::string dateobs, std::string telescop, std::string instrume, std::string datamode, 
                      double cistatus, int wmap_detxmax, int wmap_detymax, int wmap_detxmin,
                      int wmap_detymin, int & num_subregions, int & egrid_size) {

  AH_DEBUG << "start loadResponseData" << std::endl;

  std::string inst;            // Instrument variable string

  double caldbTime = 0.0;      // Subregional time
  int row = 0;                 // row number in CALDB file

  // Two sets of corner coordinates are needed to swap
  // coordinates and check for minima and maxima. This is
  // needed since coordinate axes on CCD 1 and CCD 2 are
  // inverted from CCD 3 and CCD 4
  // (Explained in detail during event loop)

  int detx_a = 0;              // DETX coordinates 
  int detx_b = 0;              // 
  int detx_c = 0;              // 
  int detx_d = 0;              // 
  int dety_a = 0;              // DETY coordinates
  int dety_b = 0;              // 
  int dety_c = 0;              // 
  int dety_d = 0;              // 

  int detx_low = 0;             // Lower corner, x-coordinate
  int detx_high = 0;            // Upper corner, x-coordinate
  int dety_low = 0;             // Lower corner, y-coordinate
  int dety_high = 0;            // Upper corner, y-coordinate

  double l_time = 0.0;          // Local variable to hold subregion time
  long long rowFirst = 0;       // First row within observation time

  int l_corner_detx[4];         // Local array to corner x-coordinates in DET
  int l_corner_dety[4];         // Local array to corner y-coordinates in DET 

  double * l_energyGrid;        // Local array grid of energies for subregion
  double * l_primaryFWHM;       // Local array response shape, main peak FWHM
  double * l_secondaryOffset;   // Local array response shape, secondary peak center
  double * l_secondaryAmp;      // Local array response shape, secondary peak amplitude
  double * l_secondaryFWHM;     // Local array response shape, secondary peak FWHM
  double * l_escapeOffset;      // Local array response shape, escape peak center
  double * l_escapeAmp;         // Local array response shape, escape peak amplitude
  double * l_escapeFWHM;        // Local array response shape, escape peak FWHM
  double * l_fluorOffset;       // Local array response shape, Si fluorescent peak center
  double * l_fluorAmp;          // Local array response shape, Si fluorescent peak amplitude
  double * l_fluorFWHM;         // Local array response shape, Si fluorescent peak FWHM
  double * l_constantAmp;       // Local array response shape, constant component

  ahfits::FilePtr fprmf = 0;    // File pointer to RMF CALDB file 

  //caldb query keywords
  std::string filetype = "sxi rmf parameters";
  std::string detnam = "-";
  std::string codename = "RMF_PARAMETERS";
   
  std::string dmode;
  std::stringstream cistatus_ss;

  cistatus_ss << cistatus;
 
  for (int ii=0; ii<7; ++ii) {
    dmode += datamode.at(ii);
  }
  std::string expression = "DATAMODE.eq." + dmode + 
                           ".and.CISTATUS.eq."+cistatus_ss.str();

  if (rmffile == "CALDB") {
    AH_INFO(ahlog::HIGH) << "Using RMF param file from CALDB." << std::endl;
    rmffile = ahmission::caldb::resolve(rmffile,filetype,instrume,detnam,codename,dateobs,expression,telescop); 
    ape_trad_set_string("rmfparam",rmffile.c_str());
    ahfits::open(rmffile,"",&fprmf);
  } else {
    AH_INFO(ahlog::HIGH) << "Using RMF param file " << rmffile << std::endl;
    //Open specified RMF file (including optional extended syntax)
    ahfits::open(rmffile,"",&fprmf);
    
    // Move to the correct extension depending on the data mode 
    std::string windowmode = expression.substr(12,7); 
    if (ahgen::strtoupper(windowmode) == "WINDOW1") {
      std::cout << "Datamode is Window1 moving to first extension" << std::endl;
      AH_INFO(ahlog::HIGH) << "Datamode is Window1 moving to first extension" << std::endl;
      ahfits::move(fprmf,2);
    } else if (ahgen::strtoupper(windowmode) == "WINDOW2") {
      std::cout << "Datamode is Window2 moving to second extension" << std::endl;
      AH_INFO(ahlog::HIGH) << "Datamode is Window2 moving to second extension" << std::endl;
      ahfits::move(fprmf,3);
    } else {
      AH_THROW_RUNTIME("Datamode keyword is not recognized, must be either Window1 or Window2");
    }


    // Check for correct instrument
    inst = ahfits::getKeyValStr(fprmf, "INSTRUME");
    if("SXI" != inst) {
      AH_THROW_RUNTIME("INSTRUME keyword in inputfile should be SXI, not " + inst);
    }
    // Checks on DATAMODE and CISTATUS are not done, since these are CALDB boundary keywords.
  }

  // Obtain the number of subregions
  responseData.m_nsubreg = ahfits::getKeyValLLong(fprmf,"NSUBREG");

  // Get grid size of subregions 
  egrid_size = ahfits::columnRepeat(fprmf,"ENERGY");

  // Set up router for CALDB file
  ahfits::Router router(fprmf);

  // Load TIME column to locate first occurence within observation time
  router.connectScalar(ahfits::e_READONLY, "TIME", l_time);

  // Move to and read first row
  // +++ 2014-06-06 AS: Does the process of getting the CALDB time here make sense?
  rowFirst = 1;
  ahfits::firstRow(fprmf);
  ahfits::readRow(fprmf); 
  while(ahfits::readOK(fprmf)) { // Read subsequent rows until within observation time
    caldbTime = l_time;
    ahfits::nextRow(fprmf);
    ahfits::readRow(fprmf);
    if(l_time > time_obs) break;
    // if we encounter a new CALDB time stamp, reset the starting CALDB row
    if(l_time != caldbTime) rowFirst = ahfits::currentRow(fprmf);
    // if the observation is fully after the CALDB, it should just use the last CALDB time stamp
    if(ahfits::atLastRow(fprmf)) break;
  }
  // Verify no errors while reading
  if(!ahfits::readOK(fprmf)) AH_THROW_RUNTIME("Invalid data for TIME column in RMF file at row " + ahfits::currentRow(fprmf));


  // allocate memory for local variables
  l_energyGrid = new double[egrid_size];
  l_primaryFWHM = new double[egrid_size];
  l_secondaryOffset = new double[egrid_size];
  l_secondaryAmp = new double[egrid_size];
  l_secondaryFWHM = new double[egrid_size];
  l_escapeOffset = new double[egrid_size];
  l_escapeAmp = new double[egrid_size];
  l_escapeFWHM = new double[egrid_size];
  l_fluorFWHM = new double[egrid_size];
  l_fluorOffset = new double[egrid_size];
  l_fluorAmp = new double[egrid_size];
  l_constantAmp = new double[egrid_size];

  //Allocate memory for response data containers
  responseData.m_corner_detx = new int *[responseData.m_nsubreg];
  responseData.m_corner_dety = new int *[responseData.m_nsubreg];
  responseData.m_energy_grid = new double *[responseData.m_nsubreg];
  responseData.m_primary_fwhm = new double *[responseData.m_nsubreg];
  responseData.m_secondary_offset = new double *[responseData.m_nsubreg];
  responseData.m_secondary_amp = new double *[responseData.m_nsubreg];
  responseData.m_secondary_fwhm = new double *[responseData.m_nsubreg];
  responseData.m_escape_offset = new double *[responseData.m_nsubreg];
  responseData.m_escape_amp = new double *[responseData.m_nsubreg];
  responseData.m_escape_fwhm = new double *[responseData.m_nsubreg];
  responseData.m_flu_offset = new double *[responseData.m_nsubreg];
  responseData.m_flu_amp = new double *[responseData.m_nsubreg];
  responseData.m_flu_fwhm = new double *[responseData.m_nsubreg];
  responseData.m_con_amp = new double *[responseData.m_nsubreg];

  // Allocate space for response data
  for(int ii = 0; ii < responseData.m_nsubreg; ++ii) {
    responseData.m_corner_detx[ii] = new int[4];
    responseData.m_corner_dety[ii] = new int[4];
    responseData.m_energy_grid[ii] = new double[egrid_size];
    responseData.m_primary_fwhm[ii] = new double[egrid_size];
    responseData.m_secondary_offset[ii] = new double[egrid_size];
    responseData.m_secondary_amp[ii] = new double[egrid_size];
    responseData.m_secondary_fwhm[ii] = new double[egrid_size];
    responseData.m_escape_offset[ii] = new double[egrid_size];
    responseData.m_escape_amp[ii] = new double[egrid_size];
    responseData.m_escape_fwhm[ii] = new double[egrid_size];
    responseData.m_flu_offset[ii] = new double[egrid_size];
    responseData.m_flu_amp[ii] = new double[egrid_size];
    responseData.m_flu_fwhm[ii] = new double[egrid_size];
    responseData.m_con_amp[ii] = new double[egrid_size];
  }

  // Load rest of columns
  router.connectFixedLengthArray(ahfits::e_READONLY, "CORNER_DETX", l_corner_detx);
  router.connectFixedLengthArray(ahfits::e_READONLY, "CORNER_DETY", l_corner_dety);
  router.connectFixedLengthArray(ahfits::e_READONLY, "ENERGY", l_energyGrid);
  router.connectFixedLengthArray(ahfits::e_READONLY, "PRIMARY_FWHM", l_primaryFWHM);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SECONDARY_OFFSET", l_secondaryOffset);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SECONDARY_AMP", l_secondaryAmp);
  router.connectFixedLengthArray(ahfits::e_READONLY, "SECONDARY_FWHM", l_secondaryFWHM);
  router.connectFixedLengthArray(ahfits::e_READONLY, "ESCAPE_OFFSET", l_escapeOffset);
  router.connectFixedLengthArray(ahfits::e_READONLY, "ESCAPE_AMP", l_escapeAmp);
  router.connectFixedLengthArray(ahfits::e_READONLY, "ESCAPE_FWHM", l_escapeFWHM);
  router.connectFixedLengthArray(ahfits::e_READONLY, "FLUOR_OFFSET", l_fluorOffset);
  router.connectFixedLengthArray(ahfits::e_READONLY, "FLUOR_AMP", l_fluorAmp);
  router.connectFixedLengthArray(ahfits::e_READONLY, "FLUOR_FWHM", l_fluorFWHM);
  router.connectFixedLengthArray(ahfits::e_READONLY, "CONST_AMP", l_constantAmp);

  ahfits::gotoRow(fprmf,rowFirst); // Make sure that router is connected to correct first row
  ahfits::readRow(fprmf); // Read rest of columns

  // Continue reading rows while time in observation time
  while(caldbTime == l_time && readOK(fprmf)) {

    detx_a = l_corner_detx[0];
    detx_b = l_corner_detx[1];
    detx_c = l_corner_detx[2];
    detx_d = l_corner_detx[3];

    dety_a = l_corner_dety[0];
    dety_b = l_corner_dety[1];
    dety_c = l_corner_dety[2];
    dety_d = l_corner_dety[3];

    // Get upper-right and lower-left corners of subregion
    detx_low = std::min(std::min(detx_a,detx_b),std::min(detx_c,detx_d));
    dety_low = std::min(std::min(dety_a,dety_b),std::min(dety_c,dety_d));
    detx_high = std::max(std::max(detx_a,detx_b),std::max(detx_c,detx_d));
    dety_high = std::max(std::max(dety_a,dety_b),std::max(dety_c,dety_d));

    if((detx_low <= wmap_detxmax && dety_low <= wmap_detymax) ||
       (detx_high >= wmap_detxmin && dety_high >= wmap_detymin)) {

      // Insert new subregion to response data

      // Write corner coordinate data into responseData struct
      responseData.m_corner_detx[num_subregions][0] = detx_a;
      responseData.m_corner_detx[num_subregions][1] = detx_b;
      responseData.m_corner_detx[num_subregions][2] = detx_c;
      responseData.m_corner_detx[num_subregions][3] = detx_d;
      responseData.m_corner_dety[num_subregions][0] = dety_a;
      responseData.m_corner_dety[num_subregions][1] = dety_b;
      responseData.m_corner_dety[num_subregions][2] = dety_c;
      responseData.m_corner_dety[num_subregions][3] = dety_d;

      // Write subregion data from CALDB to responseData struct
      for(int ii = 0; ii < egrid_size; ++ii) {
        responseData.m_energy_grid[num_subregions][ii] = l_energyGrid[ii];
        responseData.m_primary_fwhm[num_subregions][ii] = l_primaryFWHM[ii];
        responseData.m_secondary_offset[num_subregions][ii] = l_secondaryOffset[ii];
        responseData.m_secondary_amp[num_subregions][ii] = l_secondaryAmp[ii];
        responseData.m_secondary_fwhm[num_subregions][ii] = l_secondaryFWHM[ii];
        responseData.m_escape_offset[num_subregions][ii] = l_escapeOffset[ii];
        responseData.m_escape_amp[num_subregions][ii] = l_escapeAmp[ii];
        responseData.m_escape_fwhm[num_subregions][ii] = l_escapeFWHM[ii];
        responseData.m_flu_offset[num_subregions][ii] = l_fluorOffset[ii];
        responseData.m_flu_amp[num_subregions][ii] = l_fluorAmp[ii];
        responseData.m_flu_fwhm[num_subregions][ii] = l_fluorFWHM[ii];
        responseData.m_con_amp[num_subregions][ii] = l_constantAmp[ii];
      }

      num_subregions++;
      AH_DEBUG << "Found RMF subregion " << num_subregions << " in CALDB row " << row << std::endl;
      AH_DEBUG << "CORNER_DETX A = " << detx_a << std::endl;
      AH_DEBUG << "CORNER_DETX B = " << detx_b << std::endl;
      AH_DEBUG << "CORNER_DETX C = " << detx_c << std::endl;
      AH_DEBUG << "CORNER_DETX D = " << detx_d << std::endl;
      AH_DEBUG << "CORNER_DETY A = " << dety_a << std::endl;
      AH_DEBUG << "CORNER_DETY B = " << dety_b << std::endl;
      AH_DEBUG << "CORNER_DETY C = " << dety_c << std::endl;
      AH_DEBUG << "CORNER_DETY D = " << dety_d << std::endl;
    }

    if(ahfits::atLastRow(fprmf)) break; // Don't read any more rows if at last row
    ahfits::nextRow(fprmf);
    ahfits::readRow(fprmf);
    row++;
  }

  AH_INFO(ahlog::HIGH) << "Found " << num_subregions << " total RMF subregions overlapping the WMAP" << std::endl;

  // Deallocate local arrays (before checking for error)
  delete [] l_energyGrid; l_energyGrid=0;
  delete [] l_primaryFWHM; l_primaryFWHM=0;
  delete [] l_secondaryOffset; l_secondaryOffset=0;
  delete [] l_secondaryAmp; l_secondaryAmp=0;
  delete [] l_secondaryFWHM; l_secondaryFWHM=0;
  delete [] l_escapeOffset; l_escapeOffset=0;
  delete [] l_escapeAmp; l_escapeAmp=0;
  delete [] l_escapeFWHM; l_escapeFWHM=0;
  delete [] l_fluorOffset; l_fluorOffset=0;
  delete [] l_fluorAmp; l_fluorAmp=0;
  delete [] l_fluorFWHM; l_fluorFWHM=0;
  delete [] l_constantAmp; l_constantAmp=0;

  // Verify no errors
  if(!ahfits::readOK(fprmf)) {
    destroyResponseData(responseData);
    AH_THROW_RUNTIME("Invalid data for RMF file at row " + ahfits::currentRow(fprmf));
  }

  // Close file, not to be used again
  ahfits::close(fprmf);
  
  AH_DEBUG << "done loadResponseData" << std::endl;
}

// ****************************************************************************

void destroyResponseData(ResponseData & responseData) {

  for(int ii = 0; ii < responseData.m_nsubreg; ++ii) {
    delete [] responseData.m_energy_grid[ii];
    delete [] responseData.m_primary_fwhm[ii];
    delete [] responseData.m_secondary_offset[ii];
    delete [] responseData.m_secondary_amp[ii];
    delete [] responseData.m_secondary_fwhm[ii];
    delete [] responseData.m_escape_offset[ii];
    delete [] responseData.m_escape_amp[ii];
    delete [] responseData.m_escape_fwhm[ii];
    delete [] responseData.m_flu_offset[ii];
    delete [] responseData.m_flu_amp[ii];
    delete [] responseData.m_flu_fwhm[ii];
    delete [] responseData.m_con_amp[ii];

    responseData.m_energy_grid[ii]=0;
    responseData.m_primary_fwhm[ii]=0;
    responseData.m_secondary_offset[ii]=0;
    responseData.m_secondary_amp[ii]=0;
    responseData.m_secondary_fwhm[ii]=0;
    responseData.m_escape_offset[ii]=0;
    responseData.m_escape_amp[ii]=0;
    responseData.m_escape_fwhm[ii]=0;
    responseData.m_flu_offset[ii]=0;
    responseData.m_flu_amp[ii]=0;
    responseData.m_flu_fwhm[ii]=0;
    responseData.m_con_amp[ii]=0;
  }

}
// ****************************************************************************

// +++ This should be moved to ahgen(?)
void parseCALDBKeywordDbl(std::string keystr, double & keydbl) {

  keydbl = 0;
  std::istringstream iss(keystr);
  iss.ignore(FLEN_KEYWORD,'(');
  iss >> keydbl;

}

// ****************************************************************************

/** @} */


/** Revision Log
 $Log: sxirmf.cxx,v $
 Revision 1.41  2016/07/06 18:26:18  mdutka
 Correcting the initialization of the normalization factor

 Revision 1.40  2016/04/13 20:53:27  mdutka
 changing ah-info to ah_out for progress report

 Revision 1.39  2016/04/13 18:39:17  mdutka
 Adding a rows completed counter and now displaying the weights for each of the subregions

 Revision 1.38  2016/04/07 20:24:37  mdutka
 Moving high volumne logging messages to debug

 Revision 1.37  2016/04/06 18:34:33  mdutka
 Correcting the calculation of the weighting factor, there were negative elements in the WMAP image which should not be included in the weight

 Revision 1.36  2016/03/24 19:27:07  mdutka
 Adding parameters logging to standard main and adding ape_trad_set_string after caldb query for rmfparam file

 Revision 1.35  2016/03/18 15:11:32  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.34  2016/01/29 21:03:04  mdutka
 updating sxirmf to move to correct extension based on datamode

 Revision 1.33  2015/12/29 13:35:58  mdutka
 Adding boundary keyword writing for sxirmf

 Revision 1.32  2015/12/24 02:13:22  rshill
 Made writeEboundsHDU and closeRMFFile pass-by-pointer.

 Revision 1.31  2015/11/20 20:40:26  mdutka
 making rmf param data from CALDB dynamically sized now depends on keyword NSUBREG

 Revision 1.30  2015/11/18 20:00:38  mdutka
 fixed segmentation fault with larger rmfparam file

 Revision 1.29  2015/11/17 22:45:25  mwitthoe
 sxirmf: add keyword comment for CISTATUS

 Revision 1.28  2015/11/04 20:53:42  mdutka
 checking in new version of sxirmf

 Revision 1.27  2015/10/22 20:22:14  mdutka
 updating sxirmf so caldb query no longer considers certain keywords

 Revision 1.26  2015/08/24 11:47:33  klrutkow
 changed validity check for DATE-OBS: it will now accept a decimal in the time

 Revision 1.25  2015/08/23 22:06:05  klrutkow
 fix bug in time_obs assignment ; in row loop, compare time to first time ; after row loop, just use first time for caldb time instead of throwing an error if no times were found

 Revision 1.24  2015/07/31 20:08:33  mwitthoe
 sxirmf: add parameter stamping to log file

 Revision 1.23  2015/07/31 20:02:30  mwitthoe
 sxirmf: 1) add standard prologue (issue 534); 2) fix bug in a log statement; 3) fix memory leaks

 Revision 1.22  2015/07/30 17:25:58  mdutka
 Updating sxirmf to use new caldb query routine

 Revision 1.21  2015/07/29 21:16:10  mdutka
 Adding loggin to sxirmf

 Revision 1.20  2015/07/29 14:44:31  klrutkow
 read TLMIN and TLMAX from SPECTRUM extension CHANNEL column

 Revision 1.19  2015/07/29 13:19:36  mdutka
 updateing sxirmf to be compatable with new rmflib library

 Revision 1.18  2015/06/24 19:55:30  mdutka
 Commiting updating chatter output for caldb query

 Revision 1.17  2015/05/28 21:05:19  mdutka
 Adding CALDB query to sxirmf

 Revision 1.16  2014/12/30 19:57:32  mdutka
 Updated parameter list see issue #472

 Revision 1.15  2014/12/10 14:47:31  mdutka
 Removed AH::INFO finished statement

 Revision 1.14  2014/11/18 21:10:58  mdutka
 made chan(start/stop) emat(start/stop) local to function initialize

 Revision 1.13  2014/11/18 20:18:58  mdutka
 Made changes suggested by Andy in redmine (see issue #435)

 Revision 1.12  2014/10/23 18:48:21  mdutka
 removed debugging messages

 Revision 1.11  2014/10/20 14:31:10  mdutka
 sxirmf has been updated to handle variable energy grids see issue #435

 Revision 1.10  2014/09/12 18:01:07  mdutka
 Added support for new rmf writing rotuines

 Revision 1.9  2014/08/12 03:10:06  asargent
 Fixed allocation of corner coordinates in loadResponseData

 Revision 1.8  2014/08/06 13:54:01  asargent
 Cleaned up an int -> double type conversion.

 Revision 1.7  2014/08/05 14:10:43  asargent
 Updated variable-type definitions for wmap detx and y and conversions for cistatus, period, first and offset

 Revision 1.6  2014/06/27 18:43:34  asargent
 Updated algorithms to include corner coordinates rather than TelDef usage. Updated use of clobbering, since the heasp RMF library has no clobbering option.

 Revision 1.5  2014/06/24 18:23:49  asargent
 Updated functions in makeResponse to conform to updated rmflib inputs.

 Revision 1.4  2014/06/06 19:32:25  asargent
 Significant updates to algorithm to account for ACT->DET misalignment. Changes during reading of coordinates from CALDB file and when loading the WMAP. TelDef file is now required due to these changes.

 Revision 1.3  2014/06/03 18:21:37  asargent
 Added ability to clobber output RMF file. This option is not available in the RMF class. The workaround is to use ahfits to create a new file using clobber options, then immediately closing the file. the RMF class will then append to the outfile.

 Revision 1.2  2014/06/03 17:35:49  asargent
 Updated comments and descriptions. Rearrangement of declarations in makeResponse. Removed emincont parameter. Added in checks and summary for RMF to logfile. Fixed bug with wmap resizing.

 Revision 1.1  2014/05/27 20:25:52  asargent
 First version of sxirmf

*/


