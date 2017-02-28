/// \file sxsrmf.cxx
/// \brief Calculate the response table for the SXS 
/// \author Mike Dutka
/// \date $Date: 2016/04/13 20:47:40 $
/// \version 1.0

/**

\defgroup tool_sxsrmf compute SXS response matrix (sxsrmf)
@ingroup mod_sxs_tasks

The sxsrmftool tool calculates the response of the SXS instrument based on
the tables containing the fitting results of the calibration line.  

Source files:

  sxsrmf.cxx
  sxsrmflib.h
  sxsrmflib.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
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

#define AHLABEL tool_sxsrmf
#define AHCVSID "$Id: sxsrmf.cxx,v 1.39 2016/04/13 20:47:40 mdutka Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sxsrmflib.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "rmflib/rmflib.h"
#include "ahmath/ahmath.h"
#include "ahmission/caldb.h"
#include "ahmission/ahmission.h"
#include "headas.h"    // expand_item_list
#include "ahgen/ahgen.h"
#include "arfgenlib/arfgenlib.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <cmath>


/** \addtogroup tool_sxsrmf
 *  @{
 */

const int NPIXELS = 36;
const double keVtoeV = 0.001;

/// \brief Get parameter values
/// \param[in]  eVtokeV        conversion factor from eV to keV
/// \param[out] emin_eb        minimum EBOUNDS energy in keV
/// \param[out] de_eb          EBOUNDS bin spacing in keV
/// \param[out] pixel          input pixel number [0-35] (d/f 0)" 
/// \param[out] resol          Resolution of Events (d/f H other H M L)"
/// \param[out] time           Time validity YYYY-MM-DD
/// \param[out] infile         Input weighting factor file
/// \param[out] outrsp         Output RSP file (YES, NO)
/// \param[out] outfile        output response file name
/// \param[out] arffile        Input ARF file
/// \param[out] rspfile        Ouptut response file
/// \param[out] whichrmf       Calculate small (s), medium (m) or large (l) 
///                            response (d/f s)
/// \param[out] rmfsigma       name of the sigma CALDB table use for all three rmf 
///                            sizes
/// \param[out] rmftau         name of the tau CALDB table used for medium and 
///                            large rmf sizes 
/// \param[out] emincont       min energy for electron continuum in eV (d/f 10)
/// \param[out] rmfthresh      response matrix low threshold (d/f 1.0e-9)
/// \param[out] eminin         Minimum energy default 10 eV
/// \param[out] dein           Grid energy default 0.5 eV
/// \param[out] nchanin        Number of Channels, the default is 32768
/// \param[out] useingrd       flag to set output grid equal to input grid
/// \param[out] eminout        Minimum output bin energy [eV]
/// \param[out] deout          Output energy bin size [eV]
/// \param[out] nchanout       Number of output energy bins
/// \param[out] ngrid          number of input energy grid spacings
/// \param[out] nchanin_total  total number of input bins
void getPar(const double eVtokeV,  double& emin_eb,  double& de_eb, int& pixel, 
            std::string& resol, std::string& time,std::string & infile,
            bool & outrsp, std::string & arffile, std::string & rspfile,
            std::string& outfile, std::string& whichrmf, std::string& rmfsigma, 
            std::string& rmftau, double& emincont, double& rmfthresh, double& eminin, 
            std::vector<double> & dein, std::vector<long long> & nchanin, 
            bool & useingrd, double & eminout, double & deout, 
            long long & nchanout, int& ngrid, long long& nchanin_total);

/// \brief load data from CALDB files
/// \param[in] dat_sigma      Will store data from sigma CALDB table (used for 
///                           s, m ,l resposne).
/// \param[in] dat_tau        Will store data from the tau CALDB table (used for 
///                           m and l response).                 
/// \param[in] outfile        name of the output response file
/// \param[in] rmfthresh      response matrix low threshold (d/f 1.0e-9) 
/// \param[in] pixel          input pixel number [0-35] (d/f 0)"
/// \param[in] resol          Resolution of Events (d/f H other H M L)"
/// \param[in] whichrmf       Calculate small (s), medium (m) or large (l) 
///                           response
/// \param[in] time           Time validity YYYY-MM-DD
/// \param[in] rmfsigma       name of the sigma CALDB table use for all three
///                           rmf sizes
/// \param[in] rmftau         name of the tau CALDB table used for medium and 
///                           large rmf sizes
/// \param[in] resolution     integer which specifies the resolution to use for 
///                           the gaussian fwhm (0 = low, 1 = medium, 2 = high)
/// \param[in] eminin         Minimum energy default 10 eV
/// \param[in] dein           Grid energy default 0.5 eV
/// \param[in] deout          output energy bin spacing
/// \param[in] nchanin        number of energy bins, the default is 32768
/// \param[in] nchanout       number of output energy bins
/// \param[in] nchan          number of “output energy channels” in response 
///                           matrix
/// \param[in] eVtoKeV        conversion factor from eV to keV
/// \param[in] emin_eb        minimum output energy 
/// \param[in] de_eb          energy grid spacing of output energy channels – 
///                           assumed constant
/// \param[in] ngrid          number of input energy grid spacings
/// \param[in] nchanin_total  total number of input energy bins
/// \param[out] flag_rmf      integer which specifies the size of the rmf to 
///                           create 0 = small, 1= medium, 2= large
/// \param[out] echanstart    0...nchan-1 output energy channel lower edge
/// \param[out] echanstop     0...nchan-1 output energy channel upper edge
/// \param[out] echancen      0...nchan-1 output energy channel center
/// \param[out] ematstart     0...nchan-1 input energy bin lower edge
/// \param[out] ematstop      0...nchan-1 input energy bin lower edge
/// \param[out] ematcen       0...nchan-1 input energy bin center
/// \param[out] rmfdat        struct which stores all data for response file
void initialize(sxsrmf_sigma::DataType& dat_sigma, 
                sxsrmf_tau::DataType& dat_tau, std::string infile, 
                bool & outrsp, std::string & arffile, std::string & rspfile,
                const std::string outfile, long & nfrac, double& rmfthresh, 
                int& pixel, std::string& resol, 
                std::string& whichrmf, std::string & time, 
                std::string& rmfsigma, std::string & rmftau, int& resolution,  
                double& eminin, std::vector<double> & dein, double & deout,
                std::vector<long long>& nchanin, long long & nchanout, 
                int& nchan, const double eVtokeV, double& emin_eb, 
                double& de_eb, int& ngrid, long long& nchanin_total, 
                int& flag_rmf, double ** echanstart, double ** echanstop, 
                double ** echancen, double ** ematstart, double ** ematstop, 
                double ** ematcen, rmflib::RMFData& rmfdat, 
                rmflib::RMFData& rspdat, std::vector<int> & pixInfile, 
                std::vector<int> & resolInfile, 
                std::vector<double> & fracInfile, std::vector<double> & arfeffarea);

/// \brief Compute the resposne and fill in output file
/// \param[in] dat_sigma      will store data from sigma CALDB table (used for
///                           s, m ,l resposne).
/// \param[in] dat_tau        will store data from the tau CALDB table 
///                           (used for m and l response).
/// \param[in] flag_rmf       integer which specifies the size of the rmf 
///                           to create (0 = small, 1= medium, 2= large).
/// \param[in] ngrid          number of input energy grid spacings
/// \param[in] nchanin        Number of Channels, the default is 32768
/// \param[in] nchanin_total  total number of input energy bins
/// \param[in] eminin         minimum input energy
/// \param[in] dein           input energy grid spacing
/// \param[in] nchanout       number of output energy bins
/// \param[in] eminout        Minimum output bin energy [eV]
/// \param[in] deout          output energy bin spacing
/// \param[in] rmfthresh      response matrix low threshold (d/f 1.0e-9)
/// \param[out] rmfdat        struct which stores all data for response file  
/// \param[in] emincont       min energy for electron continuum in eV (d/f 10)
/// \param[in] ematcen        0...nchanin-1 input energy bin center – in keV
/// \param[out] echancen      0...nchan-1 output energy channel center
/// \param[in] de_eb          energy grid spacing of output energy channels – 
///                           assumed constant
/// \param[in] emin_eb        minimum output energy channel
/// \param[in] eVtokeV        conversion factor for eV to keV
/// \param[in] outfile        name of the output file that the rmf is written to
void doWork(sxsrmf_sigma::DataType& dat_sigma, sxsrmf_tau::DataType& dat_tau, 
            int& flag_rmf, bool & outrsp, std::string & rspfile, 
            long nfrac, int ngrid, std::vector<long long> & nchanin, 
            long long& nchanin_total, double& eminin, std::vector<double>& dein, 
            long long & nchanout, double & eminout, double& deout,
            double& rmfthresh, std::vector<double> & arfeffarea, rmflib::RMFData& rmfdat, 
            rmflib::RMFData& rspdat, double& emincont, double * ematcen, 
            double * echancen, double& de_eb, double& emin_eb,
            const double eVtokeV, const std::string& outfile,std::vector<int> & pixInfile,
            std::vector<int> & resolInfile,std::vector<double> & fracInfile);

/// \brief close open FITS files
/// \param[in] rmfdat          struct which stores all data for response file 
void finalize(rmflib::RMFData& rmfdat, rmflib::RMFData& rspdat);

// ****************************************************************************

/// \Brief sxsrmf tool
int main(int argc, char** argv) {

  std::string infile;                // Input weighting factor file
  bool outrsp;                       // Output RSP file (YES, NO)
  std::string arffile;               // Input ARF file
  std::string rspfile;               // Ouptut response file
  std::string outfile;               // name of output file
  int pixel = 0;                     // interger pixel number; must be  0-35
  std::string resol;                 // resolution grade string, possible values
                                     // are (H) High, (M) medium, (L) low
  std::string time;                  // Time string in formate YYYY-MM-DD
  std::string whichrmf;              // rmf file size option; must be 
                                     // recognizable as (s)mall, (m)edium or 
                                     // (l)arge
  std::string rmfsigma;              // gaussian sigma table string
  std::string rmftau;                // medium/large option parameter table 
                                     // string
  double emincont = 0;               // lower limit electron loss continuum
  sxsrmf_sigma::DataType dat_sigma;  // struct which stores data from the sigma
                                     // CALDB file (rmf small)
  sxsrmf_tau::DataType dat_tau;      // struct which stores data from the tau 
                                     // CALDB file (rmf medium, rmf large)
  rmflib::RMFMatrixRowVars matrixrow;// struct with stores varibles which
                                     // are written to the Matrix extension  

  long nfrac;
  double rmfthresh = 0.0;            // response matrix lower threshold
  double eminin;                     // Minimum energy default 10eV
  std::vector<double> dein;          // Grid energy default 0.5 eV
  
  int flag_rmf = 0;                  // specifes size of the response file to 
                                     // create.
  int resolution = 0;                // This integer specifies the resolution 
                                     // and is set by the resol string 
                                     // (H = 0, M = 1, L = 2)
  bool useingrd = true;              // flag to set output grid equal to input grid
  double eminout = 0.;               // Minimum output bin energy [eV]
  double deout = 0.;                 // Output energy bin size [eV]
  long long nchanout = 0;            // Number of output energy bins
  
  std::vector<long long> nchanin;    // total number of input energy bins
  int ngrid = 0;                     // number of grid spacings to use for input grid
  long long nchanin_total = 0;       // total number of channels
  double * ematstart = 0;            // 0...nchanin-1 input energy bin lower edge
  double * ematstop = 0;             // 0...nchanin-1 input energy bin lower edge
  double * ematcen = 0;              // 0...nchanin-1 output energy channel center
  int nchan = 0;                     // number of output energy channels
  double * echanstart = 0;           // 0...nchan-1 output energy channel lower edge
  double * echanstop = 0;            // 0...nchan-1 output energy channel upper edge
  double * echancen = 0;             // 0...nchan-1 output energy channel center

  std::vector<double> arfeffarea;

  std::vector<int> pixInfile;        //pixel number from weighting file
  std::vector<int> resolInfile;      //resolution from weighting file
  std::vector<double> fracInfile;    //weighting fraction from weighting file


  double emin_eb = 0.0;              // minum energy for ebounds table
  double de_eb = 0.0;                // energy grid spacing of output energy 
                                     // channels (ebounds)– assumed constant
  rmflib::RMFData rmfdat;            // struct which stores all the data 
                                     // neccesary for the output response matrix
  rmflib::RMFData rspdat;

  //conversion ev to keV
  const double eVtokeV=0.001;

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(eVtokeV, emin_eb, de_eb, pixel, resol, time, infile, 
             outrsp, arffile, rspfile, outfile, whichrmf, 
             rmfsigma, rmftau, emincont, rmfthresh, eminin, dein, 
             nchanin, useingrd, eminout, deout, nchanout, ngrid, nchanin_total);
      ahapp::writeParametersToLog();
      initialize(dat_sigma, dat_tau, infile, outrsp, arffile, rspfile, outfile,
                 nfrac, rmfthresh, pixel, resol, whichrmf, time, rmfsigma, 
                 rmftau, resolution, eminin, dein, deout, nchanin, nchanout, 
                 nchan, eVtokeV, emin_eb, de_eb, ngrid, nchanin_total, flag_rmf,
                 &echanstart, &echanstop, &echancen, &ematstart, &ematstop, 
                 &ematcen, rmfdat, rspdat, pixInfile, resolInfile, fracInfile, arfeffarea);
      doWork(dat_sigma, dat_tau, flag_rmf, outrsp, rspfile, nfrac, ngrid, nchanin, nchanin_total,
             eminin, dein, nchanout, eminout, deout, rmfthresh, arfeffarea, rmfdat, rspdat,
             emincont, ematcen, echancen, de_eb, emin_eb, eVtokeV, outfile, 
             pixInfile, resolInfile, fracInfile); 
      finalize(rmfdat,rspdat);
      ahapp::shutDown();
    } else {
      try {
        getPar(eVtokeV, emin_eb, de_eb, pixel, resol, time, infile, 
               outrsp, arffile, rspfile, outfile, whichrmf, 
               rmfsigma, rmftau, emincont, rmfthresh, eminin, dein, 
               nchanin, useingrd, eminout, deout, nchanout, ngrid, nchanin_total);
        initialize(dat_sigma, dat_tau, infile, outrsp, arffile, rspfile, outfile,
                 nfrac, rmfthresh, pixel, resol, whichrmf, time, rmfsigma, 
                 rmftau, resolution, eminin, dein, deout, nchanin, nchanout, 
                 nchan, eVtokeV, emin_eb, de_eb, ngrid, nchanin_total, flag_rmf,
                 &echanstart, &echanstop, &echancen, &ematstart, &ematstop, 
                 &ematcen, rmfdat, rspdat, pixInfile, resolInfile, fracInfile, arfeffarea);
        doWork(dat_sigma, dat_tau, flag_rmf, outrsp, rspfile, nfrac, ngrid, nchanin, nchanin_total, eminin, dein, 
               nchanout, eminout, deout, rmfthresh, arfeffarea, rmfdat, rspdat, emincont, ematcen, echancen, 
                de_eb, emin_eb, eVtokeV, outfile, pixInfile, resolInfile, fracInfile); 
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(rmfdat,rspdat);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(const double eVtokeV,  double& emin_eb,  double& de_eb, int& pixel, 
            std::string& resol, std::string& time,std::string & infile,
            bool & outrsp, std::string & arffile, std::string & rspfile,
            std::string& outfile, std::string& whichrmf, std::string& rmfsigma, 
            std::string& rmftau, double& emincont, double& rmfthresh, double& eminin, 
            std::vector<double> & dein, std::vector<long long> & nchanin, 
            bool & useingrd, double & eminout, double & deout, 
            long long & nchanout, int& ngrid, long long& nchanin_total) {
  
  std::string dein_str;
  std::string nchanin_str;

  //Load in parameters from the .par file
  infile = ahapp::getParString("infile");
  outfile = ahapp::getParString("outfile");
  outrsp = ahapp::getParBool("outrsp");
  arffile = ahapp::getParString("arffile");
  rspfile = ahapp::getParString("rspfile");
  pixel = ahapp::getParInt("pixel");
  resol = ahapp::getParString("resol");
  time = ahapp::getParString("time");
  whichrmf = ahapp::getParString("whichrmf");
  rmfsigma = ahapp::getParString("rmfsigma");
  rmftau = ahapp::getParString("rmftau");
  eminin = ahapp::getParDouble("eminin");
  dein_str = ahapp::getParString("dein"); 
  nchanin_str = ahapp::getParString("nchanin");
  useingrd = ahapp::getParBool("useingrd");
  eminout = ahapp::getParDouble("eminout");
  deout = ahapp::getParDouble("deout");
  nchanout = ahapp::getParDouble("nchanout");
  rmfthresh = ahapp::getParDouble("rmfthresh");
  emincont = ahapp::getParDouble("emincont");
 
  //check the values of the user supplied values
  
  //pixel
  if (pixel < 0 || pixel > 35) {
    AH_THROW_RUNTIME("Error: invalid pixel number");
  } 
  
  //resol
  resol=ahgen::strtoupper(resol);
  if (resol != "H" && resol != "M" && resol != "L"){
    AH_THROW_RUNTIME("Error: invalid resolution");
  }


  //outfile 
  //whichrmf
  whichrmf=ahgen::strtoupper(whichrmf);
  if (whichrmf != "S" && whichrmf != "M" && whichrmf != "L" && whichrmf != "X"){
    AH_THROW_RUNTIME("Error: invalid rmf size");
  } 

  // convert dein_str into an array of doubles convert to 
  char* dein_char=(char*)dein_str.c_str();
  char** items=0;
  int nitems_dein=0;
  int status=0;
  int trim=1;         // trim spaces
  int skip=1;         // exclude empty items
  int guard=0;        // do not protect against commas in parentheses
  items=expand_item_list(dein_char, &nitems_dein, ',',trim,skip,guard,&status);
  if (status != 0)
    AH_THROW_RUNTIME("invalid value for dein parameter; expect list of values separated by commas");
  for (int ii=0; ii < nitems_dein; ++ii) {
    dein.push_back(atof(items[ii]));
  }
 
  //convert m_nchanin_str into an array of ints
  char* nchanin_char=(char*)nchanin_str.c_str();
  items=0;
  int nitems_nchanin=0;
  status=0;
  trim=1;         // trim spaces
  skip=1;         // exclude empty items
  guard=0;        // do not protect against commas in parentheses
  items=expand_item_list(nchanin_char, &nitems_nchanin, ',',trim,skip,guard,&status);
  if (status != 0)
    AH_THROW_RUNTIME("invalid value for nchanin parameter; expect list of values separated by a comma");
  for (int ii=0; ii < nitems_nchanin; ++ii) {
    nchanin.push_back(atoi(items[ii]));
  }
 
  //store the number of grids into the param struct
  if (nitems_dein == nitems_nchanin) {
    ngrid = nitems_nchanin;
  } else {
    AH_THROW_RUNTIME("The number of energy grids do not match");
  }

  //store the total number of channels
  for (int ii = 0; ii < ngrid; ++ii){
    nchanin_total += nchanin[ii];
  }

  if (useingrd) {
    if (ngrid != 1) {
      AH_THROW_RUNTIME("Using input energy grid for output, can only have one value for input energy grid spacing.");
    } else {
      eminout = eminin;
      deout = dein[0];
      nchanout = nchanin[0];
    }
  }

  //convert eV to keV for relavant varibles
  emincont = eVtokeV * emincont;
  eminin = eVtokeV * eminin;
  
  eminout = eVtokeV * eminout;
  deout = eVtokeV * deout;
  for (int ii = 0; ii < ngrid; ++ii) {
    dein[ii] = eVtokeV * dein[ii];
  }
  
  //set emin_eb = eminin in keV. This may change later if we 
  //decide we want different grid spacings for Ebounds and Matrix 
  emin_eb = eminout;  
  de_eb = deout;  
  
 
}

// ****************************************************************************

void initialize(sxsrmf_sigma::DataType& dat_sigma, 
                sxsrmf_tau::DataType& dat_tau, std::string infile, 
                bool & outrsp, std::string & arffile, std::string & rspfile,
                const std::string outfile, long & nfrac, double& rmfthresh, 
                int& pixel, std::string& resol, 
                std::string& whichrmf, std::string & time, 
                std::string& rmfsigma, std::string & rmftau, int& resolution,  
                double& eminin, std::vector<double> & dein, double & deout,
                std::vector<long long>& nchanin, long long & nchanout, 
                int& nchan, const double eVtokeV, double& emin_eb, 
                double& de_eb, int& ngrid, long long& nchanin_total, 
                int& flag_rmf, double ** echanstart, double ** echanstop, 
                double ** echancen, double ** ematstart, double ** ematstop, 
                double ** ematcen, rmflib::RMFData& rmfdat, 
                rmflib::RMFData& rspdat, std::vector<int> & pixInfile, 
                std::vector<int> & resolInfile, 
                std::vector<double> & fracInfile, std::vector<double> & arfeffarea){

  // const int MAXNFRAC = 108;                 //Maximum number of weights possible

  ahfits::FilePtr ahffp_infile = 0;         //file pointer to input file 

  double totfrac = 0.;                     //total fraction from weighting factors file
  int l_pixel = 0;
  int l_grad = 0; 
  double l_weight = 0;
  int flagres[3] = {};
  int flagpix[NPIXELS] = {};
  int numpix = 0;
  int numres = 0;
  int pixrow = 0; 
  std::vector<int> pixlist;
  std::vector<int> reslist;
  int first = 0; 
  int last = 0; 
  int graderow = 0;
  std::stringstream first_ss;
  std::stringstream last_ss;

  std::string pixelStr;
  std::string cbd10001;
  std::string cbd20001;
  std::string date;
  std::string time_hms;

  // parameter used for CALDB query
  std::string filetype = "RMF paramenter file";
  std::string instrume = "SXS";
  std::string detnam = "-";
  std::string codename_sigma = "RMFPARAM1";
  std::string codename_tau = "RMFPARAM2"; 

  //get date and time into two separate variables
  date = time.substr(0,10);
  time_hms = time.substr(11,8);

  int arfflag = 0;
  long numarfens = 0; 
  std::vector<double> arf_energ_lo; 
  std::vector<double> arf_energ_hi; 
  std::vector<double> arf_ecenter; 
  
 
  //Allocates memory for dynamic arrays and initializes every element to zero 
  //To allocate memory we passed a pointer to the pointer in main the line below
  //dereference this pointer and allocate the memory to the pointer which was 
  //intialized in main which is then passed to do work. 
  *ematstart = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) (*ematstart)[ii] = 0.0;

  *ematstop = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) (*ematstop)[ii] = 0.0;

  *ematcen = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) (*ematcen)[ii] = 0.0;

  *echanstart = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) (*echanstart)[ii] = 0.0;
  
  *echanstop = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) (*echanstop)[ii] = 0.0;
  
  *echancen = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) (*echancen)[ii] = 0.0;
  

  if (whichrmf == "S") flag_rmf = 0;
  if (whichrmf == "M") flag_rmf = 1;
  if (whichrmf == "L") flag_rmf = 2;
  if (whichrmf == "X") flag_rmf = 3;

  //Here we decide which data to load depending on which rmf file size 
  //the user wants to create.  For small rmf only the sigma caldb table
  //is loaded,for medium large and extra large the tau CALDB table is loaded 
  // small rmf: gaussian componet is written to final response matrix 
  
  //The gaussian data are loaded for all possible rmf sizes
  rmfsigma = ahmission::caldb::resolve(rmfsigma,filetype,instrume,detnam,
                                       codename_sigma,time);
  ape_trad_set_string("rmfsigma",rmfsigma.c_str());  

  //Open validate and process the input file
  if (ahgen::strtoupper(infile) != "NONE") { 
    ahfits::open(infile,"SXSFRAC",&ahffp_infile);
    ahmission::checkEmptyTable(ahffp_infile,infile);
    if (!ahmission::isValidTELESCOP(ahfits::getKeyValStr(ahffp_infile,"TELESCOP"))) {
      AH_THROW_RUNTIME("TELESCOP keyword in input weighting factor file is invalid.");
    }
    if (ahfits::getKeyValStr(ahffp_infile,"INSTRUME") != "SXS") {
      AH_THROW_RUNTIME("INSTRUME keyword in input weighting factor file is invalid.");
    }
    nfrac = ahfits::getKeyValLLong(ahffp_infile,"NAXIS2");
    if (nfrac > 108) {
      AH_THROW_RUNTIME("Weighting factor file is invalid, number of rows in SXSFRAC extension is above 108");
    }
     
    //connect local variables to columns 
    ahfits::Router router(ahffp_infile);
   
    router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel);
    router.connectScalar(ahfits::e_READONLY,"GRADE",l_grad);  
    router.connectScalar(ahfits::e_READONLY,"WEIGHT",l_weight);

    //Loop over the rows in weighting factors file
    totfrac = 0.0;
    for (ahfits::firstRow(ahffp_infile); ahfits::readOK(ahffp_infile); 
         ahfits::nextRow(ahffp_infile)) {
      ahfits::readRow(ahffp_infile);
      if ((l_pixel<0 || l_pixel>35) || (l_grad<0 || l_grad>2) || 
          (l_weight<0 || l_weight>1.0)) {
        AH_THROW_RUNTIME("Invalid entry in rmf weighting table.");
      }
      pixInfile.push_back(l_pixel);
      resolInfile.push_back(l_grad);
      fracInfile.push_back(l_weight);
      totfrac += l_weight;
    }
    if (totfrac > (1.0 + 1.0e-6)) {
      AH_THROW_RUNTIME("sum of weights in weighting file must be <= 1");
    }
  } else { //single pixel and grade from resol and pixel parameters
    //resolution tag to determine which fwhm to use for the primaray gaussian 
    //component
    if (resol == "H") {
      resolution = 0;
    } else if (resol == "M") {
      resolution = 1;
    } else if (resol == "L") {
      resolution = 2;
    } else {
      AH_THROW_RUNTIME("Invalid value for resol");     
    }
    nfrac = 1;
    pixInfile.push_back(pixel);
    resolInfile.push_back(resolution);
    fracInfile.push_back(1.0);
  } // end if infile

  sxsrmf_sigma::load(rmfsigma,dat_sigma,nfrac,pixInfile,resolInfile);

 
  //medium, large and extra large rmf: additional respose data is loaded
  if (flag_rmf > 0){
    rmftau = ahmission::caldb::resolve(rmftau,filetype,instrume,detnam,
                                       codename_sigma,time);
    ape_trad_set_string("rmftau",rmftau.c_str()); 
    sxsrmf_tau::load(rmftau, dat_tau, whichrmf); 
   
    if (dat_sigma.m_egrid_tab[0] != dat_tau.m_egrid_tab[0] && 
        dat_sigma.m_egrid_tab[dat_sigma.m_naxis2] != dat_tau.m_egrid_tab[dat_tau.m_naxis2]){
      AH_INFO(ahlog::HIGH) << "Energy grid in CALDB files do not match.";
      AH_INFO(ahlog::HIGH) << "Calculating only the small matrix" << std::endl;
      flag_rmf = 0;
    }
  }
 
  //set up the matrix grids for channels and energies
  AH_INFO(ahlog::LOW) << "EBOUND extension energy grid " << std::endl;
  AH_INFO(ahlog::LOW) << "number of channels: " << nchanout << std::endl;
  AH_INFO(ahlog::LOW) << "channel spacing = " << deout << std::endl;

  //EBOUNDS extension energy grid
  AH_DEBUG << "nchanout = " << nchanout << std::endl;
  AH_DEBUG << "deout = " << deout << std::endl;
  for (int ii=0; ii<nchanout; ++ii)
  { 
    (*echanstart)[ii] = emin_eb+float(ii)*de_eb;
    (*echanstop)[ii] = (*echanstart)[ii]+de_eb;
    (*echancen)[ii] = 0.5*((*echanstart)[ii] + (*echanstop)[ii]);
    if (ii==0 && emin_eb==0) (*echanstart)[ii]=0.1*de_eb;
    AH_DEBUG << "Output channel number: " << ii << std::endl;
    AH_DEBUG << "Start energy: " << (*echanstart)[ii] << std::endl;
    AH_DEBUG << "Stop energy: " << (*echanstop)[ii] << std::endl;
    AH_DEBUG << "Center Energy: " << (*echancen)[ii] << std::endl;
  }

  AH_DEBUG << "MATRIX extension energy grid" << std::endl;
  AH_DEBUG << "Number of grids: " << ngrid << std::endl;
  AH_DEBUG << "Minimun energy: " << eminin << std::endl;
  //Matrix extension energy grid
  int ii = 0; //This is the index of input energy grid
  double startenergy = 0;
  for (int kk=0; kk<ngrid; ++kk)
  {
    AH_DEBUG << "Grid spacing number: " << kk << std::endl;
    AH_DEBUG << "number of channels with this spacing: " << nchanin[kk] << std::endl;
    AH_DEBUG << "energy spacing: " << dein[kk] << std::endl;
    if (kk == 0) {
      startenergy = eminin;
    } else {
      startenergy = (*ematstop)[ii-1];
    }
    for (int jj = 0; jj < nchanin[kk]; ++jj) { 
      if (jj > nchanin_total) {
        AH_THROW_RUNTIME("Energy channel index is out of range");
      }
      (*ematstart)[ii] = startenergy + float(jj)*dein[kk];
      (*ematstop)[ii] = (*ematstart)[ii] + dein[kk];
      (*ematcen)[ii] = 0.5 * ((*ematstart)[ii] + (*ematstop)[ii]);
      AH_DEBUG << "Input channel number: " << ii << std::endl;
      AH_DEBUG << "Start energy: " << (*ematstart)[ii] << std::endl;
      AH_DEBUG << "Stop energy: " << (*ematstop)[ii] << std::endl;
      AH_DEBUG << "Center Energy: " << (*ematcen)[ii] << std::endl;
      ii++; 
    }
  }      
 
  // 1) create file “outfile”
  // 2) create Primary extension (TYPE: Image)
  // 3) fill Primary extension header keywords see list input /output section :
  // 4) create  MATRIX extension (TYPE: Binary Table)
  // 5) fill MATRIX extension header set up columns 
  //      a) TTYPE1  = 'ENERG_LO'  TFORM1  = '1E' TUNIT1  = 'keV'          
  //      b) TTYPE2  = 'ENERG_HI' TFORM2  = '1E' TUNIT2  = 'keV'           
  //      c) TTYPE3  = 'N_GRP' TFORM3  = '1I'  TUNIT3  = ''         
  //      d) TTYPE4  = 'F_CHAN' TFORM4  = 'nI' TUNIT4  = ''           
  //      e) TTYPE5  = 'N_CHAN' TFORM5  = 'nI' TUNIT5  = ''           
  //      f) TTYPE6  = 'MATRIX' TFORM6  = 'PE(1,n_chan ngrp)'  TUNIT6  = ' ' 
  // 6) create  EBOUNDS extension (TYPE: Binary Table)
  // 7) fill EBOUNDS extension header additional keywords (to be confirmed):
  //   a) TTYPE1  = 'CHANNEL ' TFORM1  = '1I      '  TUNIT1  = '        '
  //   b) TTYPE2  = 'E_MIN'  TFORM2  = '1E      ' TUNIT2  = 'keV     '          
  //   c) TTYPE3  = 'E_MAX' TFORM3  = '1E      ' TUNIT3  = 'keV     '      

  //setup RMF file
  rmflib::createRMFFile(&rmfdat, outfile);

  //set header keywords for output RMF file
  rmfdat.m_lo_thresh = rmfthresh;
  rmfdat.m_telescop = ahmission::getTELESCOPString();
  rmfdat.m_filter = "";
  rmfdat.m_instrume = "SXS";
  rmfdat.m_detnam = "PIXEL";
  rmfdat.m_chantype = "PI";
  rmfdat.m_time = time;
  rmfdat.m_date = date;
  rmfdat.m_tlmin_channel = 0;
  rmfdat.m_tlmax_channel = nchanout-1;
  rmfdat.m_tlmin_fchan = 0;
  rmfdat.m_tlmax_fchan = nchanout-1;

  rmfdat.m_description_matrix = "SXS RMF based on ground calibration";
  rmfdat.m_description_ebounds = "Energy boundaries of spectral channels";

  if (outrsp) {
    arfflag = 1;
    if (ahgen::strtoupper(arffile) != "NONE") {
      // read ARF file - user routine from arfgen
      arfgenlib::getarfdata(arffile,numarfens,arf_energ_lo,arf_energ_hi, 
                 arf_ecenter,arfeffarea);

      // Covert arf grid to eV 

      if (numarfens != nchanin_total) {
        AH_THROW_RUNTIME("arf and rmf input energy grid sizes are not equal");
      } else {
        for (int ii=0; ii<nchanin_total;++ii) {
          if (!ahgen::isEqual(arf_energ_hi[ii],(*ematstop)[ii],0.0000001)) {
            AH_THROW_RUNTIME("arf and rmf input energy grids are not the same");
          }
        }
      }
      arfflag = 0;
    }
    if (arfflag == 1) { // unity over input channels if no arffile or problem
     
      numarfens = nchanin_total;
      for (int ii=0; ii<nchanin_total; ++ii) {
        arf_energ_lo.push_back((*ematstart)[ii]);
        arf_energ_hi.push_back((*ematstop)[ii]);
        arfeffarea.push_back(1.0);
      }
    } // end if arfflag = 1 
  
    
   
    //setup RSP file
    rmflib::createRMFFile(&rspdat, rspfile);

    //set header keywords for output RMF file
    rspdat.m_lo_thresh = rmfthresh;
    rspdat.m_telescop = ahmission::getTELESCOPString();
    rspdat.m_filter = "";
    rspdat.m_instrume = "SXS";
    rspdat.m_detnam = "PIXEL";
    rspdat.m_chantype = "PI";
    rspdat.m_time = time;
    rspdat.m_date = date;
    rspdat.m_tlmin_channel = 0;
    rspdat.m_tlmax_channel = nchanout-1;
    rspdat.m_tlmin_fchan = 0;
    rspdat.m_tlmax_fchan = nchanout-1;

    rspdat.m_description_matrix = "SXS RSP with rmf based on ground calibration and arffile";
    rspdat.m_description_ebounds = "Energy boundaries of spectral channels";
  }
      

  //Boundary keywords
  if (ahgen::strtoupper(infile) != "NONE") {
    for (int ii=0;ii<nfrac;ii++) { 
      pixrow=pixInfile[ii];
      graderow=resolInfile[ii];
      if (flagpix[pixrow] == 0) { 
        flagpix[pixrow] = 1;
        pixlist.push_back(pixrow);
        numpix+=1;
      }
      if (flagres[graderow] == 0) {
        flagres[graderow]=1;
        reslist.push_back(graderow);
        numres+=1;
      }
    }
  } else { //single pixel and grade from resol and pixel parameters
    numpix = 1;
    numres = 1;
    reslist.push_back(resolution);
    pixlist.push_back(pixel);
  } // end if infile
    std::sort(pixlist.begin(),pixlist.end()); 
    cbd10001 = "PIXEL(";
    first = -1;
    for (int ii = 0; ii<numpix; ii++) {
      if (first == -1) first = pixlist[ii]; //new group
      if ((ii == numpix-1) || pixlist[ii] == pixlist[ii+1]+1) { //close group
        last = pixlist[ii];
        if (ii == numpix-1) {
          first_ss << first;
          last_ss << last;
          cbd10001 = cbd10001 + first_ss.str() + ":" + last_ss.str() + ")";
        } else {
          first_ss << first;
          last_ss << last;
          cbd10001 = cbd10001 + first_ss.str() + ":" + last_ss.str() + ",";
        }
        first = -1;
      }
    }
      std::sort(reslist.begin(),reslist.end());
      if (numres == 1) { 
        std::stringstream reslist0;
        reslist0 << reslist[0];
        if (reslist[0] == 0) {
          cbd20001 = "ITYPE(0:0)";
        } else if (reslist[0] == 1) {
           cbd20001 = "ITYPE(1:2)";
        } else {
           cbd20001 = "ITYPE(3:4)";
        }
      } else if (numres==2) {
        if (reslist[0] == 0) {
          if (reslist[1] == 1) {
            cbd20001 = "ITYPE(0:2)";
          } else {
            cbd20001 = "ITYPE(0:0,3:4)";
          }
        }
      } else {
        cbd20001 = "ITYPE(1:4)";
      }
  
      if (numres == 3) { 
        cbd20001 = "ITYPE(0:5)";
      }

  
  //set channel and energy grids
  rmflib::setInputEnergies(&rmfdat, *ematstart, *ematstop, *ematcen, nchanin_total);
  rmflib::setChannelEnergies(&rmfdat, *echanstart, *echanstop, *echancen, nchanout);

  //create Ebounds extensions, and write boundary keywords
  rmflib::writeEboundsHDU(&rmfdat);
  ahfits::move(rmfdat.m_ahffp,"EBOUNDS");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD10001",cbd10001,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD20001",cbd20001,"Keyword information for Caltools Software");  

  //create Matrix extensions, and write boundary keywords
  rmflib::createMatrixHDU(&rmfdat);  
  ahfits::move(rmfdat.m_ahffp,"MATRIX");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD10001",cbd10001,"Keyword information for Caltools Software");
  ahfits::writeKeyValStr(rmfdat.m_ahffp,"CBD20001",cbd20001,"Keyword information for Caltools Software");  

  if (outrsp) {
    //set channel and energy grids
    rmflib::setInputEnergies(&rspdat, *ematstart, *ematstop, *ematcen, nchanin_total);
    rmflib::setChannelEnergies(&rspdat, *echanstart, *echanstop, *echancen, nchanout);

    //create Ebounds extensions, and write boundary keywords
    rmflib::writeEboundsHDU(&rspdat);
    ahfits::move(rspdat.m_ahffp,"EBOUNDS");
    ahfits::writeKeyValStr(rspdat.m_ahffp,"CBD10001",cbd10001,"Keyword information for Caltools Software");
    ahfits::writeKeyValStr(rspdat.m_ahffp,"CBD20001",cbd20001,"Keyword information for Caltools Software");  

    //create Matrix extensions, and write boundary keywords
    rmflib::createMatrixHDU(&rspdat);  
    ahfits::move(rspdat.m_ahffp,"MATRIX");
    ahfits::writeKeyValStr(rspdat.m_ahffp,"CBD10001",cbd10001,"Keyword information for Caltools Software");
    ahfits::writeKeyValStr(rspdat.m_ahffp,"CBD20001",cbd20001,"Keyword information for Caltools Software"); 
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

  delete [] *ematstart;
  delete [] *ematstop; 
  delete [] *echanstart; 
  delete [] *echanstop; 

 
      
}

// ****************************************************************************
 
      
void doWork(sxsrmf_sigma::DataType& dat_sigma, sxsrmf_tau::DataType& dat_tau, 
            int& flag_rmf, bool & outrsp, std::string & rspfile,
            long nfrac, int ngrid, std::vector<long long> & nchanin, 
            long long& nchanin_total, double& eminin, std::vector<double>& dein,      
            long long & nchanout, double & eminout, double& deout,
            double& rmfthresh, std::vector<double> & arfeffarea, rmflib::RMFData& rmfdat, 
            rmflib::RMFData& rspdat, double& emincont, double * ematcen, 
            double * echancen, double& de_eb, double& emin_eb,
            const double eVtokeV, const std::string& outfile,std::vector<int> & pixInfile,
            std::vector<int> & resolInfile,std::vector<double> & fracInfile) {
  
  double dmatrix = 0;
  double fgau_mid = 0.0;       //primary gaussian fraction interpolated at the 
                               //center energy
  double ** fwhm_mid = 0;       //fwhm of the primary gaussian interpolated at 
                               //the center energy 
  double * tau_mid = 0;        //decay width of the exponential tail 
                               //interpolated at the center energy 
  double * fexp_mid = 0;       //expoential fraction interpolated at the center 
                               //energy
  double * fcon_mid = 0;       //continuum fraction interpolated at the center 
                               //energy
  double ** fpk_mid = 0;       //escape peaks fraction interpolated at the the 
                               //center energy
  double renorm_gau = 0;     //gaussian renormalizaiton factor
  double renorm_exp = 0;     //exponential renormalization factor
  double renorm_con = 0;     //continuum renormalizaiton factor
  double *renorm_pk = 0;     //escape peaks renomralizaiton factor
  double * gmatrix = 0;        //final response of the gaussian component 
  double * ematrix = 0;        //final response of the exponential component  
  double * ematrix_unconvolved = 0;    //temporary container for the unconvolved
                                       //exponetial vaules used by the 
                                       //convolution function   
  double * cmatrix = 0;        //final response of the continuum component
  double ** pmatrix = 0;       //final response of the escape peaks component
  double * totmatrix = 0;      //total response matrix one pixel one res
  double * grandtotmatrix = 0; //total response matrix for all pixels and all res
  
  double * totpeak = 0;        //total fraction for the escape peaks
  double Eo = 0.0;             //Center of energy for energy bin
  double Estart_gau = 0.0;     //starting energy of the gaussian for one bin
  double Estop_gau = 0.0;      //stoping energy of the gaussian for one bin
  double Estart_exp = 0.0;     //starting eenrgy of the exponential for one bin
  double Estop_exp = 0.0;      //stoping energy of the expoennetial for one bin
  double Estart_conv_exp = 0.0;//starting energy over which we convolve the 
                               //expoennetial function
  double Estart_con = 0.0;     //starting energy of the continuum for one bin 
  double Estop_con = 0.0;      //stoping energy of the continuum for one bin
  double Eshift = 0.0;         //Energy offset from main peak for escape peaks
  double Estart_pk = 0.0;      //starting energy for the peak
  double Estop_pk = 0.0;       //ending energy for the peak


  int chanstart_gau = 0;       //starting channel of the gaussian component for 
                               //one bin
  int chanstop_gau = 0;        //stoping channel of the gaussian component 
  int chanstart_exp = 0;       //starting channel of the exponential component 
  int chanstop_exp = 0;        //stoping channel of the exponential componenet 
  int chanstart_conv_exp = 0;  //starting channel over which we convolve the 
                               //exponential component
  int chanstart_pk[100];       //first channel to calculate peaks
  int chanstop_pk[100];        //last channel to calculate peaks
  int chanstart_con = 0;       //starting channel of the continuum component
  int chanstop_con = 0;        //stoping channel of the continuum component 
  int num_chan_conv = 0;       //the total number of channels over which we 
                               //convolve the exponential
  double sumgau = 0;           //sum of the gaussian fraction   
  double sumexp = 0;           //sum of the exponential fraction
  double sumcon = 0;           //sum of the continuum fraction
  double sumpks = 0;           //sum of the peak fraction
  
  int npeak = 0;               //number of escape peaks
  long srcidx = 0;             //index to interpolate over
  bool extrap = true;          //boolean value for the ahmath::search function 
                               //extrap true if x not in range of x arr  
  int ipeak = 0;               //used to interpolate closest FWHM for escape 
                               //peaks 

                               //over all input energies  
  


  const double AREATOL = 0.0000001; //Used to check the normalization after 
                                    //assembling totmatrix
 
  npeak =  dat_tau.m_npeak;    //total number of escape peaks 
  
  //pixel number resoluation and weight
  int ipix = 0;
  int ires = 0;
  double weight = 0.;

  
  //Allocate memory for dynamic arrays and initialize all elements to zero 
 

  fwhm_mid = new double *[nfrac];
  for (int ii = 0; ii < nfrac; ++ii) fwhm_mid[ii] = new double[nchanin_total];

  tau_mid = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) tau_mid[ii] = 0.0;

  fexp_mid = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) fexp_mid[ii] = 0.0;

  fcon_mid = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) fcon_mid[ii] = 0.0;

  fpk_mid = new double*[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii){
    fpk_mid[ii] = new double[npeak];
    for (int jj = 0; jj < npeak; ++jj) fpk_mid[ii][jj] = 0.0;
  }

   
  renorm_pk = new double[npeak];
  for (int ii = 0; ii < npeak; ++ii){
    renorm_pk[ii] = 0;
  }

  gmatrix = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) gmatrix[ii] = 0.0;  

  ematrix = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) ematrix[ii] = 0.0;

  ematrix_unconvolved = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) ematrix_unconvolved[ii] = 0.0;
  
  cmatrix = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) cmatrix[ii] = 0.0;   
 
  pmatrix = new double*[npeak];
  for (int ii = 0; ii < npeak; ++ii){
    pmatrix[ii] = new double[nchanout];
    for (int jj = 0; jj < nchanout; ++jj) pmatrix[ii][jj] = 0.0;
  }
  
  totpeak = new double[nchanin_total];
  for (int ii = 0; ii < nchanin_total; ++ii) totpeak[ii] = 0.0;
  
  totmatrix = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) totmatrix[ii] = 0.0;
  
  grandtotmatrix = new double[nchanout];
  for (int ii = 0; ii < nchanout; ++ii) grandtotmatrix[ii] = 0.0;
  
 

  //interpolate to get table values on matrix energy grid:
  AH_INFO(ahlog::HIGH) << "Interpolating line spread function parameters onto input energy grid" << std::endl; 
  
  //Begin loop over matrix energy grid
  for (int ii=0; ii<nchanin_total; ii++){
    for (int ifrac=0; ifrac<nfrac; ifrac++) {
      ipix = pixInfile[ifrac];
      ires = resolInfile[ifrac];
      weight = fracInfile[ifrac];
    
      //search for the index element using the ahmath::search() function
      srcidx=ahmath::search(ematcen[ii],dat_sigma.m_egrid_tab,
                            dat_sigma.m_naxis2,extrap,srcidx);
      if (!extrap) {
        AH_DEBUG << "Interpolation search of line spread function succeeded" << std::endl;
      } else {
        AH_DEBUG << "Interpolation search of line spread function failed, extrapolating value" << std::endl;
      }

      //for gaussian core always need this component
      //Needs to pick out dat_sigma.m_fwhm for this pixel and resolution grade
      fwhm_mid[ifrac][ii] = ahmath::interpolate(ematcen[ii], dat_sigma.m_egrid_tab, 
                                         dat_sigma.m_fwhm[ifrac], dat_sigma.m_naxis2, 
                                         ahmath::TWOPOINT, srcidx);
    } //end loop over ifrac
      if (flag_rmf > 0) {
        //for the exponential shoulder component, we need this for flag_rmf = 1 
        //or 2
        tau_mid[ii] = ahmath::interpolate(ematcen[ii], dat_tau.m_egrid_tab, 
                                          dat_tau.m_tau, dat_sigma.m_naxis2, 
                                          ahmath::TWOPOINT, srcidx);
        fexp_mid[ii] = ahmath::interpolate(ematcen[ii], dat_tau.m_egrid_tab, 
                                           dat_tau.m_frac_exp, dat_sigma.m_naxis2,
                                           ahmath::TWOPOINT, srcidx);
        if (flag_rmf > 1) {
          /* for flagrmf > 1 if flag rmf = 2 The continuum fraction is set to 0 
             if flagrmf = 3 then we keep we keep the continuum component if the 
             continuum is above the threshold the final response matrix will be 
             the maximum possible size (32768 ^2) this causes problems when we 
             try to write the final matrix */
          if (flag_rmf == 2) fcon_mid[ii] = 0;
          if (flag_rmf == 3) { 
            fcon_mid[ii] = ahmath::interpolate(ematcen[ii], 
                           dat_tau.m_egrid_tab, dat_tau.m_frac_cont, 
                           dat_sigma.m_naxis2, ahmath::TWOPOINT, srcidx);
          }
          //escape peaks component this is included if flag rmf > 1
          for (int jp = 0; jp < dat_tau.m_npeak; jp++){
            fpk_mid[ii][jp] = ahmath::interpolate(ematcen[ii], 
                              dat_tau.m_egrid_tab, dat_tau.m_frac_peak[jp], 
                              dat_sigma.m_naxis2, ahmath::TWOPOINT, srcidx);
          } //end loop over escape peaks
        } //end if flag_rmf > 1
      } //end if flag_rmf > 0
      
  }//ends loop over the matrix energy grid
  

  /*Extract and save information on which escape peaks are relavant 
    for which energy bin.  Also calculate the total fraction peaks 
    (needed to normalize the gaussian for the large matrix option) */
  if (flag_rmf > 1) {
    for (int ii = 0; ii < nchanin_total; ii++) {
      totpeak[ii] = 0.0; 
      for (int jj = 0; jj < npeak; ++jj) {
        double eshift=ematcen[ii]-dat_tau.m_de_peak[jj];
        if (eshift < ematcen[0]) continue;
        totpeak[ii] = totpeak[ii] + fpk_mid[ii][jj];
      }
    } // end loop over the peak energies
  } //endif (flag_rmf>1)


  AH_INFO(ahlog::HIGH) << "Calculating response matrix elements" << std::endl;
  //intialize all response matrix components to zero
  //begin loop jj over matrix energy grid, i.e. calculate response line by line
  for (int jj = 0; jj<nchanin_total; jj++) {

    if (jj % 1000 == 0) {
      AH_OUT << "Completed " << jj << " energy channels out of " << nchanin_total << std::endl;
    } 
    
    Eo = ematcen[jj]; 
    // Set up energy boundaries 

    Estart_exp = Eo - dat_tau.m_de_exp;     // Lower bound of exponential
    Estop_exp = Eo;                         // Upper bound of exponential
    Estart_con = emincont;                  // Lower energy bound of continuum peak
    Estop_con = Eo;                         // Upper energy bound of continuum peak

 
    
    //convert the energy boundaries into channel boundaries

    rmflib::energy2Chan(Estart_exp, eminout, deout, nchanout, chanstart_exp);
    rmflib::energy2Chan(Estop_exp, eminout, deout, nchanout, chanstop_exp);

    rmflib::energy2Chan(Estart_con, eminout, deout, nchanout, chanstart_con);
    rmflib::energy2Chan(Estop_con, eminout, deout, nchanout, chanstop_con);

    // Resetting grandtotmatrix
    for (int kk=0; kk<nchanout; ++kk) grandtotmatrix[kk] = 0;
    
  

    // Begining loop over grade/pixel combination with non-zero weights
    for (int ifrac=0; ifrac<nfrac; ifrac++) {
                                
      ipix = pixInfile[ifrac];
      ires = resolInfile[ifrac];
      weight = fracInfile[ifrac]; 

      if (jj == 0) {
        if (ifrac == 0) AH_INFO(ahlog::LOW) << "Computing response for these pixels, resolutions and weights" << std::endl;
        AH_INFO(ahlog::LOW) << "Pixel: " << ipix << std::endl;
        AH_INFO(ahlog::LOW) << "Resolution: " << ires << std::endl;
        AH_INFO(ahlog::LOW) << "Weight: " << weight << std::endl;
      }  

      AH_DEBUG << "Response matrix: set up energy boundaries for LSF for input channel " << jj << std::endl;  
   
      Estart_gau = Eo - dat_sigma.m_de_gaus[ires];  // Lower energy bound of gaussian
      Estop_gau = Eo + dat_sigma.m_de_gaus[ires];   // Upper energy bound of gaussian 
      Estart_conv_exp = std::min(Estart_gau, Estart_exp); //Lower energy bound to
                                                        //start the convolution 
                                                        //of the exponential 
                                                        //function 

      rmflib::energy2Chan(Estart_conv_exp, eminout, deout, nchanout, 
                          chanstart_conv_exp);
      rmflib::energy2Chan(Estart_gau, eminout, deout, nchanout, chanstart_gau);
      rmflib::energy2Chan(Estop_gau, eminout, deout, nchanout, chanstop_gau);

      //get the total number of channels to convolve the exponetial over
      num_chan_conv = chanstop_gau - chanstart_conv_exp + 1;     

      //initialize all response matrix norms to zero
      renorm_gau = 0.;
      renorm_exp = 0.;
      renorm_con = 0.;
      for (int jp=0; jp < dat_tau.m_npeak; jp++){
        renorm_pk[jp] = 0.0;
      }
      // Gaussian core, small medium and large rmf
      AH_DEBUG << "Response matrix: compute Gaussian core contribution for input channel " << jj << std::endl; 
      rmflib::gaussianPeak(Eo, chanstart_gau, chanstop_gau, nchanout, 
                           fwhm_mid[ifrac][jj], echancen, gmatrix, renorm_gau);


      //Exponential Component- medium and large rmf
      if (flag_rmf > 0) {
        AH_DEBUG << "Response matrix: compute exponetial contribution for output channel " << jj << std::endl;
        rmflib::exponentialShoulder(Eo, chanstart_exp, chanstop_exp, nchanout, 
                                    tau_mid[jj],echancen, ematrix, renorm_exp);
        renorm_exp = 0.0;

        //Fill in the array ematrix_unconvolved with the values from ematrix
        for (int iii = chanstart_exp; iii < chanstop_exp; iii++){
          ematrix_unconvolved[iii] = ematrix[iii];
        }

        //convolve exponential function with the gaussian: chanstart_conv_exp= 
        //starting channel, num_chan_conv= number of channels to convolve
        AH_DEBUG << "Response matrix: convolve exponential contribution with Gaussian for input channel " << jj << std::endl;
        ahmath::convolveWithGaussianFixed(&echancen[chanstart_conv_exp], 
                                          &ematrix_unconvolved[chanstart_conv_exp],
                                          num_chan_conv, 
                                          ahmath::convertFWHM2sigma(fwhm_mid[ifrac][jj]),
                                          &ematrix[chanstart_conv_exp]);  
        for (int ii = chanstart_conv_exp; ii < chanstop_gau+1; ++ii){
          renorm_exp = renorm_exp+ematrix[ii];
        } 
      }// endif flag_rmf>0
    
      //continuum
      if (flag_rmf > 2){
        AH_DEBUG << "Response matrix: compute continuum contribution for input channel " << jj << std::endl;
        rmflib::continuumPeak(Eo, chanstart_con, chanstop_con, nchanout, echancen, 
                              cmatrix, renorm_con);
      }//endif flag_rmf > 2 continuum
    
      //escape peaks 
      if (flag_rmf > 1) {
        ipeak = 0;   
        for(int jp = 0; jp < npeak; jp++) {
          // Escape peak energy offset from main peak
          Eshift = Eo-dat_tau.m_de_peak[jp];  
          Estart_pk = Eshift-dat_sigma.m_de_gaus[ires];
          Estop_pk = Eshift+dat_sigma.m_de_gaus[ires];
          AH_DEBUG << "Response matrix: escape peak energies for input channel: " << jj 
                   << ":" << jp << " Estart_pk = " << Estart_pk << " Estop_pk = " << Estop_pk << std::endl; 
          rmflib::energy2Chan(Estart_pk, eminout, deout, nchanout, chanstart_pk[jp]);
          rmflib::energy2Chan(Estop_pk, eminout, deout, nchanout, chanstop_pk[jp]);
 
          //Interpolate closest FWHM energy 
          if (Eshift >= ematcen[0]) {
            while(ipeak < nchanin_total && ematcen[ipeak] < Eshift) ++ipeak;
            if(Eshift-ematcen[ipeak] > ematcen[ipeak+1]-Eshift) ++ipeak;
          }
          rmflib::gaussianPeak(Eshift, chanstart_pk[jp], chanstop_pk[jp], nchanout, 
                               fwhm_mid[ifrac][ipeak], echancen, pmatrix[jp], 
                               renorm_pk[jp]);     

        }// end loop over peaks
    
      } //end if flag rmf > 1 escape peaks

      //set the sums of each components equal to zero before computing
      //the next row of the response matrix.
      sumgau=0.0;
      sumexp=0.0;
      sumcon=0.0;
      sumpks=0.0;

      //compute the total fraction of each component
      //small matrix gaussian only, fraction in gaussion = 1:
      fgau_mid = 1.0;
   
      //medium matrix: gaussian + exp, fraction in gaussian = 1-fexp;
      //this is only done if there is any area in the exponential component
      if (flag_rmf > 0 && renorm_exp > 0.) fgau_mid = fgau_mid - fexp_mid[jj];
     
      //large matrix: gaussian + exp + peaks, 
      //fraction in gaussian = 1-fexp-sum(fpeaks):
      if (flag_rmf > 1) fgau_mid=fgau_mid-totpeak[jj];

      //extra large matrix: gaussian + exp + peaks + con, 
      //fraction in gaussian = 1-fexp-sum(fpeaks)-fcon:
      //this is only done if there is any area in the continuum component
      if (flag_rmf > 2 && renorm_con) fgau_mid=fgau_mid-fcon_mid[jj];    
   
      AH_DEBUG << "Response matrix: fractions in components for input channel" << jj << std::endl;
      AH_DEBUG << "Fraction in Gaussian: " << fgau_mid << std::endl;
      AH_DEBUG << "Fraction in exponential: " << fexp_mid[jj] << std::endl;
      AH_DEBUG << "Fraction in escape peaks: " << totpeak[jj] << std::endl;
      AH_DEBUG << "Fraction in continuum: " << fcon_mid[jj] << std::endl;
 
      AH_DEBUG << "Response matrix: compute total matrix contribution for each output channel; input channel number:" << jj <<  std::endl;
   

      
      double factor=fgau_mid/renorm_gau;
      // start at zero instead of chanstart_gau as to initialize totmatrix to zero
      for (int ii=0; ii <= chanstop_gau; ++ii){       
        totmatrix[ii] = factor*gmatrix[ii];
        sumgau += factor*gmatrix[ii];
      }

      if (flag_rmf > 0 && renorm_exp > 0.){
        factor=fexp_mid[jj]/renorm_exp;
        for (int ii=chanstart_conv_exp; ii <= chanstop_gau; ++ii){
          totmatrix[ii] = totmatrix[ii]+factor*ematrix[ii];
          sumexp += factor*ematrix[ii];
        }
      }
      sumexp += sumgau;
      if (flag_rmf == 3 && renorm_con){
        factor=fcon_mid[jj]/renorm_con;
        for (int ii=chanstart_con; ii <= chanstop_con; ++ii){
          totmatrix[ii] = totmatrix[ii] + factor*cmatrix[ii];
          sumcon += factor*cmatrix[ii]; 
        }
      }
      sumcon += sumexp;
      if (flag_rmf > 1) {
        for (int jp = 0; jp < npeak; ++jp) {
          if (renorm_pk[jp] <= 0.) continue;
          double eshift=ematcen[jj]-dat_tau.m_de_peak[jp];
          if (eshift < ematcen[0]) continue;
          factor=fpk_mid[jj][jp]/renorm_pk[jp];
          for (int ii=chanstart_pk[jp]; ii <= chanstop_pk[jp]; ++ii){
            totmatrix[ii] += factor*pmatrix[jp][ii];
            // assumes escape peaks do not overlap; otherwise will have double-counting
            sumpks += factor*pmatrix[jp][ii]; 
          }
        }
        sumpks += sumcon;
      }

      // only need to add totmatrix up to max index of the main Gaussian; all
      // other components occur at smaller indices
      for (int ii=0; ii <= chanstop_gau; ++ii){
        dmatrix = weight*totmatrix[ii];
        grandtotmatrix[ii] += dmatrix;
      }
      

      /* Check to see if any of the varibles sumgau, sumexp, sumpks or sumcon 
      equals 1 if not the response matrix is not properly normalized and an 
      error is thrown */  
      
      if (!(ahgen::isEqual(sumgau, 1, AREATOL) || 
            ahgen::isEqual(sumexp, 1, AREATOL) || 
-           ahgen::isEqual(sumpks, 1, AREATOL) || 
            ahgen::isEqual(sumcon, 1, AREATOL))) {    

        AH_OUT << "response not normalized," << std::endl
               << "sumgau " << sumgau << std::endl                    
               << "sumexp " << sumexp << std::endl
               << "sumpks " << sumpks << std::endl
               << "sumcon " << sumcon << std::endl;
                           
        AH_THROW_RUNTIME("response matrix is not properly normalized"); 
      }
      
    } //end loop over grade/pixel combinations with non-zero weights
    if (ahgen::strtoupper(outfile) != "NONE") {
      rmflib::writeMatrixRow(&rmfdat, grandtotmatrix, nchanout, jj);
    }
    if (outrsp) {
      rmflib::writeMatrixRow(&rspdat, grandtotmatrix, nchanout, jj, arfeffarea[jj]);
    }
           
  }// end loop over jj (energy)


  AH_INFO(ahlog::LOW) << "Processing completed." << std::endl;

  //Frees the memory which has been allocated to the dynamic arrays
  delete [] echancen;
  delete [] ematcen;
  for (int ii=0; ii<nfrac; ++ii) {
    delete [] fwhm_mid[ii]; 
  }
  delete [] fwhm_mid;
  delete [] tau_mid;
  delete [] fexp_mid;
  delete [] fcon_mid;
  for (int dd = 0; dd < nchanin_total; dd++) {
    delete [] fpk_mid[dd];
    fpk_mid[dd] = 0;
  } 
  delete [] fpk_mid;
  delete [] gmatrix;
  delete [] ematrix;
  delete [] ematrix_unconvolved;
  delete [] cmatrix;
  delete [] totmatrix;
  delete [] grandtotmatrix;
  delete [] totpeak;
  for (int dd = 0; dd < npeak; dd++) {
    delete [] pmatrix[dd];
    pmatrix[dd] = 0;
  }
  delete [] renorm_pk;
  delete [] pmatrix;

  //clean up input data
  sxsrmf_sigma::cleanup(dat_sigma, nfrac);
  sxsrmf_tau::cleanup(dat_tau);

}
// ****************************************************************************

void finalize(rmflib::RMFData& rmfdat, rmflib::RMFData & rspdat) {


  //1. deallocating memory for energy_lo, energy_hi, and matrixrow
  //2. writenumgroup and num element keywords
  //3. close the RMF fits file 
  rmflib::closeRMFFile(&rmfdat);
  rmflib::closeRMFFile(&rspdat);
}

// ****************************************************************************

/** @} */

/* Revision Log
 $Log: sxsrmf.cxx,v $
 Revision 1.39  2016/04/13 20:47:40  mdutka
 Changing ah_info to ah_out in progress report

 Revision 1.38  2016/04/13 17:32:20  mdutka
 Change from percent to number of channels processed

 Revision 1.37  2016/04/13 16:54:35  mdutka
 Adding new logging statements, percentage done and Pixel resolutions and grades

 Revision 1.36  2016/03/22 16:47:39  mdutka
 adding ape_trad_set_string

 Revision 1.35  2016/03/22 15:38:10  mdutka
 Adding writeparameters to two more places, after getpar for debug =yes and in the catch block if the program crashes

 Revision 1.34  2016/02/19 00:17:28  klrutkow
 use ahmission::getTELESCOPString() to set TELESCOP variable

 Revision 1.33  2016/01/11 20:49:13  mdutka
 Changing check on totfrac to have a 10e-6 totlerance

 Revision 1.32  2016/01/11 14:28:11  mdutka
 sxsrmf will now generate an output response file which includes the rmf and arf

 Revision 1.31  2015/12/31 19:34:55  mdutka
 Adding speed improvments to the algorithm

 Revision 1.30  2015/12/29 16:48:27  mwitthoe
 sxsrmf: throw error if input file provided but has zero rows

 Revision 1.29  2015/12/24 02:11:17  rshill
 Made writeEboundsHDU and closeRMFFile pass-by-pointer.

 Revision 1.28  2015/12/16 20:31:58  mdutka
 correcting memory leaks

 Revision 1.27  2015/12/09 15:47:23  mdutka
 fixing bug

 Revision 1.26  2015/12/08 18:43:55  mdutka
 updating sxsrmf to handle multiple pixels and grades

 Revision 1.25  2015/11/17 22:40:33  mwitthoe
 sxsrmf: add keyword/column comments

 Revision 1.24  2015/10/19 18:52:17  mwitthoe
 sxsrmf refactor: remove unneeded npeak_eff variable

 Revision 1.23  2015/10/19 18:37:46  mwitthoe
 sxsrmf: allow tool to work even if the escape peaks are not given in energy-order

 Revision 1.22  2015/08/17 16:07:50  mwitthoe
 sxsrmf (from Mike L): 1) make rmfthresh and emincont parameters hidden; 2) correct TLMAX value for CHANNEL and FCHAN columns; 3) update HTML file

 Revision 1.21  2015/08/10 16:15:48  mwitthoe
 sxsrmf: rearrange order of some commands when setting up output RMF file (as suggested by Mike L)

 Revision 1.20  2015/08/06 14:16:13  mwitthoe
 sxsrmf bug-fixes: 1) when the output grid spacing was less than half the input spacing, there was a silent divide-by-zero error causing a blank row to appear in the output RMF file; fixed bug in printing input energy grid to log file

 Revision 1.19  2015/08/03 15:04:40  mdutka
 correcting typos pointed out by Mike L

 Revision 1.18  2015/07/31 18:51:55  mwitthoe
 sxsrmf: fix typos in log statements

 Revision 1.17  2015/07/31 18:21:51  mwitthoe
 sxsrmf: stamp parameters to log file

 Revision 1.16  2015/07/31 18:17:54  mwitthoe
 sxsrmf: fix bug where only the small RMF was being computed; updated tool prologue to standard form; general clean-up


*/
