/// \file sgdarfgen.cxx
/// \brief Tool generates addtional calbration information for off axis sources
/// \author Michael Dutka
/// \date $Date: 2016/09/19 14:34:31 $

/** 

\defgroup tool_sgdarfgen Compute SGD transmission ratios (sgdarfgen)
@ingroup mod_hxisgd_tasks

This task computes the SGD transmission ratio for a source on the sky, given
a satellite pointing. The transmission ratio depends on source position because
the SGD collimator partially blocks light from off-axis sources. The computation
is done by interpolating between points in FOC coordinates read from a
calibration file.  

Source files:

  sgdevtid.cxx

Library dependencies:

  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  heacore/ahfits
  heacore/ahlog
  heacore/ape

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-10  RSH     Code cleanup.

*/

#define AHLABEL tool_sgdarfgen
#define AHCVSID "$Id: sgdarfgen.cxx,v 1.27 2016/09/19 14:34:31 mdutka Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahapp/ahapp.h"
#include "ahgen/ahgen.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"
#include "ahmission/ahmission.h"

#include "ape/ape_trad.h"
#include "headas.h"              // expand_item_list  
#include "fitsio.h"

#include <sstream>
#include <math.h>                // defines M_PI
#include <cstring>
#include <stdexcept>
#include <algorithm>             // min/max_element
#include <vector>

/** \addtogroup tool_sgdarfgen
 *  @{
 *   */

const int VERSION=2;                  ///< algorithm to use for computing focx_src & focy_src

const double RAD2DEG = (180./M_PI);   ///< conversion factor: radians to degrees
const double DEG2ARCMIN = 60.;        ///< conversion factor: degrees to arcmin

/// \brief Get parameter values
/// \param[out] infile           Input event file 
/// \param[out] rspfile          Input response file(s)
/// \param[out] outfile          Output ARF file root
/// \param[out] ra               Input RA [deg]
/// \param[out] dec              Input Dec [deg]
/// \param[out] sgdid            SGD ID (1 or 2)
/// \param[out] ccid             Compton camera ID (0 for all)
/// \param[out] tranratfile      Input transmission ratio file
void getPar(std::string & infile, std::string& rspfile, std::string & outfile, 
            double & ra, double & dec, int & sgdid, int & ccid, 
            std::string & tranratfile);

/// \brief Setup tool operation by opening the output FITS file
/// \param[out] ahffp_in        Ahfits file pointer to infile
/// \param[out] ahffp_art       Ahfits file pointer to tranratfile
/// \param[in]  infile          Input PI file name 
/// \param[in]  rspfile         Input response file(s)
/// \param[in] rspfilelist      List of all input response files by parsing rspfile
/// \param[in]  tranratfile     File containing Transmission ratio according 
///                              to offset angle position and Energ
/// \param[in]  ra              Incident position, R.A.(deg)
/// \param[in]  dec             Incident position, DEC.(deg)
/// \param[in]  ccid            CC ID (1, 2, 3: output only the corresponding
///                             ARF, 0: output ARFs for all three CCs, 
///                             >4:UNDEFINED)
/// \param[out] channel_last    last channel index value
/// \param[out] instrume        INSTRUME keyword value from Input PI file
/// \param[out] ra_nom_rad      RA_NOM in radians
/// \param[out] dec_nom_rad     DEC_NOM in radians
/// \param[out] pa_nom_rad      PA_NOM in radians
/// \param[out] eminarr         Energy bin lower edge read from tranratfile
/// \param[out] emaxarr         Energy bin upper edge read from tranratfile 
void initialize(ahfits::FilePtr & ahffp_in, ahfits::FilePtr & ahffp_art,  
                const std::string & infile, const std::string& rspfile,
                std::vector<std::string>& rspfilelist, const std::string & tranratfile,
                double & ra, double & dec, int & ccid, int & channel_last, 
                std::string & instrume, double & ra_nom_rad, 
                double & dec_nom_rad, double & pa_nom_rad, double ** eminarr, double ** emaxarr,
                int & sgdid);

/// \brief obtain 4 closest points and CALDB file on obatin transmission ratio
/// \param[in] ahffp_in        ahfits file pointer to infile
/// \param[in] ahffp_art       ahfits file pointer to tranratfile
/// \param[in] rspfilelist      List of all input response files by parsing rspfile
/// \param[out] outfile        Root name of output ARF file name
/// \param[in] ra              incident position, R.A.(deg)
/// \param[in] dec             incident position, DEC.(deg)
/// \param[in] ccid            CC ID (1, 2, 3: output only the corresponding
///                            ARF, 0: output ARFs for all three CCs, 
///                            >4:UNDEFINED)
/// \param[in] instrume        INSTRUME keyword value from Input PI file
/// \param[out] channel_last   last channel index value
/// \param[in] ra_nom_rad      RA_NOM in radians
/// \param[in] dec_nom_rad     DEC_NOM in radians
/// \param[in] pa_nom_rad      PA_NOM in radians
/// \param[in] eminarr         Energy bin lower edge read from tranratfile
/// \param[in] emaxarr         Energy bin upper edge read from tranratfile 
/// \param[out] sgdid          SGD ID (1 or 2)
void doWork(ahfits::FilePtr & ahffp_in, ahfits::FilePtr & ahffp_art, 
            std::vector<std::string>& rspfilelist, 
            std::string outfile, double & ra, double & dec, int & ccid, 
            std::string & instrume, int & channel_last, double & ra_nom_rad, 
            double & dec_nom_rad, double & pa_nom_rad, double * eminarr, 
            double * emaxarr, int & sgdid);

/// \brief Close the output file
/// \param[in] ahffp_in        ahfits file pointer to infile
/// \param[in] ahffp_art       ahfits file pointer to tranratfile
void finalize(ahfits::FilePtr ahffp_in, ahfits::FilePtr ahffp_art);

// ****************************************************************************

int main(int argc, char** argv) {

  std::string infile;                         // Input PI file name 
  std::string rspfile;                        // Input RSP file name(s)
  std::string outfile;                        // Root name of output ARF file name
  double ra = 0.;                             // Incident position, R.A.(deg)
  double dec = 0.;                            // Incident position, DEC.(deg)
  int sgdid =0;                               // SGD ID used for CALDB lookup 
  int ccid = 0;                               // CC ID (1, 2, 3: output only the corresponding 
                                              // ARF, 0: output ARFs all three CCs, >4:UNDEFINED)   
  std::string tranratfile;                    // File containing transmission ratio according
                                              // to offset angle position and energy
  std::string instrume;                       // INSTRUME keyword value from input file header 
  int channel_last = 0;                       // Rows in tranratfile ART_ENERGIES extension 
  double ra_nom_rad = 0.;                     // RA_NOM in radians
  double dec_nom_rad = 0.;                    // DEC_NOM in radians
  double pa_nom_rad = 0.;                     // PA_NOM in radians

  double * eminarr = 0;                       // Lower edge of energy bins read from tranratfile
  double * emaxarr = 0;                       // Upper edge of energy bins read from tranratfile 
 
  ahfits::FilePtr ahffp_in = 0;               // ahfits file pointer to input file
  ahfits::FilePtr ahffp_art = 0;              // ahfits file pointer to tranratfile
  std::vector<std::string> rspfilelist(3);    // list of input rsp files (from rspfile)

  int status = ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(infile, rspfile, outfile, ra, dec, sgdid, ccid, tranratfile);
      ahapp::writeParametersToLog();
      initialize(ahffp_in, ahffp_art, infile, rspfile, rspfilelist, tranratfile,
                 ra, dec, ccid, channel_last, instrume, ra_nom_rad, dec_nom_rad,
                 pa_nom_rad, &eminarr, &emaxarr, sgdid);
      doWork(ahffp_in, ahffp_art, rspfilelist, outfile, ra, 
             dec, ccid, instrume, channel_last, ra_nom_rad, dec_nom_rad,
             pa_nom_rad, eminarr, emaxarr, sgdid);
      finalize(ahffp_in, ahffp_art);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile, rspfile, outfile, ra, dec, sgdid, ccid, tranratfile);
        initialize(ahffp_in, ahffp_art, infile, rspfile, rspfilelist, tranratfile,
                   ra, dec, ccid, channel_last, instrume, ra_nom_rad, dec_nom_rad,
                   pa_nom_rad, &eminarr, &emaxarr, sgdid);
        doWork(ahffp_in, ahffp_art, rspfilelist, outfile, ra, dec, ccid, instrume,
               channel_last, ra_nom_rad, dec_nom_rad, pa_nom_rad, eminarr, emaxarr, sgdid);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(ahffp_in, ahffp_art);
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status = 1;
        AH_ERR << "During shutDown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }
  return status;
}

// ****************************************************************************

void getPar(std::string & infile, std::string& rspfile, std::string & outfile, 
            double & ra, double & dec, int & sgdid, int & ccid, 
            std::string & tranratfile) {

  // Get infile
  infile = ahapp::getParString("infile");

  // Get rspfile
  rspfile = ahapp::getParString("rspfile");

  // Get outfile
  outfile = ahapp::getParString("outfile");

  // Get incident right ascension
  ra = ahapp::getParDouble("ra");

  // Get incident declination
  dec = ahapp::getParDouble("dec");
  
  //Get sgd id 
  sgdid = ahapp::getParInt("sgdid");

  // Get the cc id number 
  ccid = ahapp::getParInt("ccid");
  if (ccid < 0 || ccid > 3) {
    AH_THROW_RUNTIME("Invalid input for CC ID");
  }

  // Get file containing the transmission ratios
  tranratfile = ahapp::getParString("tranratfile");

 

}

// ----------------------------------------------------------------------------

void initialize(ahfits::FilePtr & ahffp_in, ahfits::FilePtr & ahffp_art,  
                const std::string & infile, const std::string& rspfile,
                std::vector<std::string>& rspfilelist, const std::string & tranratfile,
                double & ra, double & dec, int & ccid, int & channel_last, 
                std::string & instrume, double & ra_nom_rad, 
                double & dec_nom_rad, double & pa_nom_rad, double ** eminarr, double ** emaxarr,
                int & sgdid) {
  
  double ra_nom = 0.;                 // RA_NOM header keyvalue from input pi file
  double dec_nom = 0.;                // DEC_NOM header keyvalue from input pi file
  double pa_nom = 0.;                 // PA_NOM header keyvalue from input pi file
  std::string dateobs;                // DATE-OBS header keyvalue from input pi file
  double emin = 0.;                   // Energy bin minimum
  double emax = 0.;                   // Energy bin maximum
  long nrow_rsp=0;                    // number of rows in RSP files
  std::string actual_tranratfile;     // Transmission ratio CALDB file

  // Parse response file name parameter.  The parameter value can be three
  // things:
  //  1. single file name (if ccid = 1, 2, or 3)
  //  2. 3 file names separated by commas (if ccid = 0)
  //  3. @filelist 
  std::vector<std::string> tmprspfilelist;       // hold files unsorted by CC
  if ('@' == rspfile[0]) {
    ahfits::expandFileList(rspfile,tmprspfilelist);
  } else {
    char** items=0;         // output list
    int nitems=0;           // number of items found
    int status=0;           // output status of expand_item_list
    int trim=1;             // trim spaces
    int skip=1;             // exclude empty items
    int guard=0;            // do not protect against commas in parentheses
    items=expand_item_list((char*)rspfile.c_str(),&nitems,',',trim, skip,guard,&status);
    if (status != 0)
      AH_THROW_RUNTIME("invalid value for rspfile parameter; expect single file, or list of values separated by commas, or a file list");
    for (int ii = 0; ii < nitems; ++ii) {
      std::stringstream tmp;
      tmp << items[ii];
      tmprspfilelist.push_back(tmp.str());
    }
  }

  // Check that the list of RSP files is consistent with the ccid parameter
  if (ccid == 0) {                      // expecting RSP files for all 3 CCs
    if (tmprspfilelist.size() != 3) 
      AH_THROW_RUNTIME("ccid parameter is zero, but did not get 3 RSP files");

    long nrow_rsplist[3]={0,0,0};     // number of rows for each RSP file
    for (int ii=0; ii < 3; ii++) {
      ahfits::FilePtr rspfp;
      ahfits::open(tmprspfilelist[ii],"",&rspfp);
      if (ahfits::isPrimary(rspfp)) ahfits::firstHDU(rspfp,ahfits::e_BINARY_TBL);
      std::string rspdetnam=ahfits::getKeyValStr(rspfp,"DETNAM");
      rspdetnam=rspdetnam.substr(0,3);
      nrow_rsplist[ii]=ahfits::getKeyValLLong(rspfp,"NAXIS2");
      ahfits::close(rspfp);

      if (rspdetnam == "CC1") {
        if (rspfilelist[0] != "") AH_THROW_RUNTIME("cannot have multiple RSP files with DETNAM = CC1");
        rspfilelist[0]=tmprspfilelist[ii];
      } else if (rspdetnam == "CC2") {
        if (rspfilelist[1] != "") AH_THROW_RUNTIME("cannot have multiple RSP files with DETNAM = CC2");
        rspfilelist[1]=tmprspfilelist[ii];
      } else if (rspdetnam == "CC3") {
        if (rspfilelist[2] != "") AH_THROW_RUNTIME("cannot have multiple RSP files with DETNAM = CC3");
        rspfilelist[2]=tmprspfilelist[ii];
      } else {
        AH_THROW_RUNTIME("RPS file ("+tmprspfilelist[ii]+") has invalid DETNAM; expecting CC1, CC2, or CC3");
      }
    }
    if (nrow_rsplist[0] != nrow_rsplist[1] || nrow_rsplist[0] != nrow_rsplist[2]) {
      AH_THROW_RUNTIME("Number of rows in input response files is inconsistent.");
    }
    nrow_rsp=nrow_rsplist[0];
  } else {                            // only operating on one CC
    if (tmprspfilelist.size() != 1) 
      AH_THROW_RUNTIME("ccid parameter is greater than zero, but multiple RSP files provided");

    ahfits::FilePtr rspfp;
    ahfits::open(tmprspfilelist[0],"",&rspfp);
    if (ahfits::isPrimary(rspfp)) ahfits::firstHDU(rspfp,ahfits::e_BINARY_TBL);
    std::string rspdetnam=ahfits::getKeyValStr(rspfp,"DETNAM");
    rspdetnam=rspdetnam.substr(0,3);
    nrow_rsp=ahfits::getKeyValLLong(rspfp,"NAXIS2");
    ahfits::close(rspfp);
    if ((ccid == 1 && rspdetnam != "CC1") || (ccid == 2 && rspdetnam != "CC2") ||
        (ccid == 3 && rspdetnam != "CC3")) {
      AH_THROW_RUNTIME("ccid parameter not consistent with DETNAM of response file");
    }
    rspfilelist[ccid-1]=tmprspfilelist[0];
  }

  // Open input file.
  ahfits::open(infile, "", &ahffp_in);
  AH_INFO(ahlog::HIGH) << "Opened input file " << infile << std::endl;
  
  // Move to the first binary table.
  ahfits::firstHDU(ahffp_in, ahfits::e_BINARY_TBL);
  
  // Get keywords.
  instrume = ahfits::getKeyValStr(ahffp_in, "INSTRUME");  
  ra_nom = ahfits::getKeyValDbl(ahffp_in, "RA_NOM");
  dec_nom = ahfits::getKeyValDbl(ahffp_in, "DEC_NOM");
  pa_nom = ahfits::getKeyValDbl(ahffp_in, "PA_NOM");
  dateobs = ahfits::getKeyValStr(ahffp_in, "DATE-OBS");

  // Test instrument value.
  if (ahgen::strtoupper(instrume) != "SGD1" && ahgen::strtoupper(instrume) != "SGD2") {
    AH_THROW_RUNTIME("INSTRUME keyword should be SGD1 or SGD2.");
  }

  // Compare sgdid in parameter with infiel
  if ((sgdid == 1) && (ahgen::strtoupper(instrume) != "SGD1")) {
    AH_THROW_RUNTIME("Parameter sgdid = 1, INSTRUME keyword should be SGD1.");
  }  
  
  if ((sgdid == 2) && (ahgen::strtoupper(instrume) != "SGD2")) {
    AH_THROW_RUNTIME("Parameter sgdid = 2, INSTRUME keyword should be SGD2.");
  }  

  // Convert input angles into radians
  ra_nom_rad = ra_nom/RAD2DEG;
  dec_nom_rad = dec_nom/RAD2DEG;
  pa_nom_rad = pa_nom/RAD2DEG;

  AH_INFO(ahlog::HIGH) << "Nominal coordinates [deg]: RA=" << ra_nom << " Dec=" << dec_nom << std::endl;
  AH_INFO(ahlog::HIGH) << "Source coordinates [deg]:  RA=" << ra << " Dec=" << dec << std::endl;
  
  // Convert RA and Dec to radians.
  ra = ra/RAD2DEG;
  dec = dec/RAD2DEG;

  AH_INFO(ahlog::LOW) << "Input angles [rad]: RA_NOM=" << ra_nom_rad << " DEC_NOM=" << dec_nom_rad << " PA_NOM=" << pa_nom_rad << std::endl;
  AH_INFO(ahlog::LOW) << "Source coordinates [rad]: RA=" << ra << " Dec=" << dec << std::endl;
  
  // Query CALDB for transmission ratio file.
  actual_tranratfile=ahmission::caldb::resolve(tranratfile, "transmission ratio", instrume, "-", "ART_ENERGIES", dateobs);
  ape_trad_set_string("tranratfile",actual_tranratfile.c_str());   // to record actual file path in history
  ahfits::open(actual_tranratfile, "ART_ENERGIES", &ahffp_art);
  AH_INFO(ahlog::HIGH) << "Opened calibration file " << actual_tranratfile << std::endl;
  channel_last = ahfits::getKeyValLLong(ahffp_art, "NAXIS2");  // Number of rows

  // Set up router for transmission ratio file.
  ahfits::Router router(ahffp_art);
  router.connectScalar(ahfits::e_READONLY, "E_MIN", emin);
  router.connectScalar(ahfits::e_READONLY, "E_MAX", emax); 

  // Allocate and initialize EMIN and EMAX.
  *eminarr = new double[channel_last];
  for (int ii = 0; ii < channel_last; ++ii) (*eminarr)[ii] = 0.; 
  
  *emaxarr = new double[channel_last];
  for (int ii = 0; ii < channel_last; ++ii) (*emaxarr)[ii] = 0.;

  //  Read E_MIN and E_MAX columns and store values in eminarr and emaxarr arrays.
  int ii = 0;
  for (ahfits::firstRow(ahffp_art); ahfits::readOK(ahffp_art); 
       ahfits::nextRow(ahffp_art)) { 
    ahfits::readRow(ahffp_art);
    (*eminarr)[ii] = emin;
    (*emaxarr)[ii] = emax;
    ++ii;
  } //end of loop over rows in tranratfile

  // check that RSP files and transrat file have the same number of energies
  if (nrow_rsp != channel_last) {
    AH_THROW_RUNTIME("input RSP files have a different number of energies than the transmission ratio CALDB file");
  }

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ----------------------------------------------------------------------------

void doWork(ahfits::FilePtr & ahffp_in, ahfits::FilePtr & ahffp_art, 
            std::vector<std::string>& rspfilelist, 
            std::string outfile, double & ra, double & dec, int & ccid, 
            std::string & instrume, int & channel_last, double & ra_nom_rad, 
            double & dec_nom_rad, double & pa_nom_rad, double * eminarr, 
            double * emaxarr, int & sgdid) {
  
  const int QUADRANTS = 4;           // Number of quadrants
  const int EBINS = 2048;            // Number of energy bins
                                     
  ahfits::Router * router_arf=0;     // Router for output ARF files
  ahfits::Router * router_rsp=0;     // Router for output RSP files
  ahfits::Router * router_art=0;     // Router for transmission ratio file
  std::stringstream ss_cc_cnt;       // String stream for CC ID
  std::string arthdu;                // EXTNAME of transmission ratio table
  std::string arfFileName;           // Output ARF file name
  std::string rspFileName;           // Output RSP file name

  ahfits::FilePtr ahffp_cc = 0;      // ahfits file pointer to output ARF file           
  ahfits::FilePtr ahffp_rsp = 0;     // ahfits file pointer to output RSP file           

  long i_row = 0;                    // Current row of calibration table

  double skyx_src=0.;                // SKYX of source in degrees
  double skyy_src=0.;                // SKYY of source in degrees

  double focx_src = 0.;              // FOCX of source in arcmin
  double focy_src = 0.;              // FOCY of source in arcmin
  double dist2[QUADRANTS];           // Min distance^2 in each quadrant  
  double focx_tab[QUADRANTS];        // Intermediate array for min distance search
  double focy_tab[QUADRANTS]; 
  double emin = 0.;                  // Energy bin min
  double emax = 0.;                  // Energy bin max
  double r_tab[QUADRANTS][EBINS];    // Input transmission ratios for interpolation
  long row_tab[QUADRANTS];           // Rows contributing points for interpolation


  double specresp[EBINS];            // local variable for SPECRESP column in RSP file
  ahfits::IndexType nspecresp=0;
  for (int ii=0; ii < EBINS; ii++) specresp[ii]=0.;
  
  int quad = 0;                      // Quadrant
  double FOCX = 0.;                  // FOCX value from transmission ratio file
  double FOCY = 0.;                  // FOCY value from transmission ratio file
  std::vector<double> focx_vec;      // Vector of FOCX from transmission ratio file
  std::vector<double> focy_vec;      // Vector of FOCY from transmission ratio file
  double TRANS_RATIO[EBINS];         // TRANS_RATIO from transmission ratio file

  ahfits::IndexType nrat = 0;        // Number of ratios read from the file 

  double dx = 0.;
  double dy = 0.;
  double d2 = 0.;

  double a = 0.;                     // Intermediate variables
  double b = 0.;                   
  
  double TRFinal = 0.;               // Interpolated transmission ratio

  int kk = 0;                        // Index of energy bin
  int firstCC = 0;                   // First CC to process   
  int lastCC = 0;                    // Last CC to process

  // Initialize arrays
  for (int ii=0; ii<QUADRANTS; ++ii){
    dist2[ii]=0.;
    focx_tab[ii]=0.;
    focy_tab[ii]=0.;
    row_tab[ii]=-1;
    for (int jj=0; jj<EBINS; ++jj){
      r_tab[ii][jj] = 0.;
    }
  }
  for (int jj=0; jj < EBINS; ++jj) {
    TRANS_RATIO[jj]=0.;
  }

  if (VERSION == 1) {

    // convert source ra/dec into sky coordinates in tangent plane
    if (ra-ra_nom_rad < M_PI) {
      skyx_src=+acos( cos(dec_nom_rad)*cos(dec_nom_rad)*cos(ra-ra_nom_rad)+
                      sin(dec_nom_rad)*sin(dec_nom_rad))*-1.*RAD2DEG;
    } else {
      skyx_src=-acos( cos(dec_nom_rad)*cos(dec_nom_rad)*cos(ra-ra_nom_rad)+
                      sin(dec_nom_rad)*sin(dec_nom_rad))*-1.*RAD2DEG;
    }
    skyy_src=(dec-dec_nom_rad)*RAD2DEG;
    AH_INFO(ahlog::LOW) << "SKY source [deg]     x=" << skyx_src << " y=" << skyy_src << std::endl;
  
    // convert sky coordinates in tangent plane to foc coordinates (in radians)
    focx_src=DEG2ARCMIN*(skyx_src*cos(pa_nom_rad)-skyy_src*sin(pa_nom_rad));
    focy_src=DEG2ARCMIN*(skyx_src*sin(pa_nom_rad)+skyy_src*cos(pa_nom_rad));
    AH_INFO(ahlog::LOW) << "FOC source [arcmin]     x=" << focx_src << " y=" << focy_src << std::endl;

  } else {         // VERSION = 2 

    // convert source ra/dec into sky coordinates in tangent plane
    int status=0;
    char ctype[5]="-TAN";
    fits_world_to_pix(ra*RAD2DEG,dec*RAD2DEG,ra_nom_rad*RAD2DEG,dec_nom_rad*RAD2DEG,
                      1215.5,1215.5,-.0004911,.0004911,0,ctype,&skyx_src,&skyy_src,&status);
    if (0 != status) {
      std::stringstream msg; 
      msg << "failure in fits_world_to_pix; status = " << status;
      AH_THROW_RUNTIME(msg.str());
    }
    AH_INFO(ahlog::LOW) << "SKY source [deg]     x=" << skyx_src << " y=" << skyy_src << std::endl;
  
    // convert sky coordinates in tangent plane to foc coordinates (in radians)
    focx_src=DEG2ARCMIN*4.911e-4*((skyx_src-1215.5)*cos(pa_nom_rad)-(skyy_src-1215.5)*sin(pa_nom_rad));
    focy_src=DEG2ARCMIN*4.911e-4*((skyx_src-1215.5)*sin(pa_nom_rad)+(skyy_src-1215.5)*cos(pa_nom_rad));
    AH_INFO(ahlog::LOW) << "FOC source [arcmin]     x=" << focx_src << " y=" << focy_src << std::endl;
  }

  // Interpolate the transmission ratios at the computed FOC position
  // as a function of energy.  The closest four points from the 
  // transmission ratio file are used with the proviso that each point must
  // be in a different quadrant.
  
  //std::cout << "Start determining the the transmission ratios for a celestial source" << std::endl;
  
  // Select range of CC IDs.
  if (ccid == 0) {  
    firstCC = 1;
    lastCC = 3;
  } else {
    firstCC=ccid;
    lastCC=ccid; 
  }

  //  Loop over CCs.
  for (int cc = firstCC; cc <= lastCC; ++cc) {

    // Initialize stringstream.
    ss_cc_cnt.str("");  
    ss_cc_cnt.clear();     

    // Convert CC ID to string.
    ss_cc_cnt << cc;
    arthdu = "TRANS_CC" + ss_cc_cnt.str();
    
    // New router for transmission ratio file.
    router_art = new ahfits::Router(ahffp_art);
    
    // Move to extension. 
    ahfits::move(ahffp_art, arthdu);
    AH_INFO(ahlog::LOW) << "Moved to extension " << arthdu << " of calibration file." << std::endl;
 
    // Make connections to local variables.
    router_art->connectScalar(ahfits::e_READONLY, "FOCX", FOCX);
    router_art->connectScalar(ahfits::e_READONLY, "FOCY", FOCY);
    router_art->connectFixedLengthArray(ahfits::e_READONLY, "TRANS_RATIO", TRANS_RATIO);  
  
    nrat = ahfits::columnRepeat(ahffp_art, "TRANS_RATIO");  // Number of values
 
    if (channel_last != nrat) {
      AH_THROW_RUNTIME("Number of energies in ART_ENERGIES extension does not match number of transmission ratios for a single positon");
    } 

    // Initialize working storage.
    for (int ii=0; ii<QUADRANTS; ++ii){
      dist2[ii]=10e15;
      focx_tab[ii]=0.;
      focy_tab[ii]=0.;
      for (int jj=0; jj<EBINS; ++jj){
        r_tab[ii][jj] = 0.;
      }
    }
   
    // Loop through calibration points in the transmission ratio file,
    // looking for the four closest values in separate quadrants.
    i_row = 0L;
    for (ahfits::firstRow(ahffp_art); ahfits::readOK(ahffp_art); ahfits::nextRow(ahffp_art)) {

      // Get next point.
      ahfits::readRow(ahffp_art);
      i_row++;
      focx_vec.push_back(FOCX);
      focy_vec.push_back(FOCY);
      
      // Difference in position between calibration point
      // and the source.
      dx = FOCX - focx_src;
      dy = FOCY - focy_src;

      // Quadrant of calibration point wrt source position and FOC axes.
      if (dx >= 0 && dy >= 0) quad=0;
      if (dx >= 0 && dy < 0) quad=1;
      if (dx < 0 && dy < 0) quad=2;
      if (dx < 0 && dy >= 0) quad=3;  

      // Distance^2 of calibration point from source.
      d2 = (dx*dx) + (dy*dy);
     
      // Record the four closest positions around the source position and the 
      // transmission ratio for all energies at these four positions.
      if (d2 < dist2[quad]) {
        row_tab[quad] = i_row;
        dist2[quad] = d2;
        focx_tab[quad] = FOCX;
        focy_tab[quad] = FOCY;
        for (int ii=0; ii<EBINS; ++ii) {
          r_tab[quad][ii] = TRANS_RATIO[ii];
        }
      }
    } // End loop through calibration points.

    for (int ii=0; ii<QUADRANTS; ++ii) {
      if (row_tab[ii] < 0.) {
        AH_DEBUG << "Quadrant " << ii << ": no point in quadrant" << std::endl;
      } else {
        AH_DEBUG << "Quadrant " << ii << ": point is at FOCX=" << focx_tab[ii]
          << " FOCY=" << focy_tab[ii] << " (row " << row_tab[ii] << ")" << std::endl;
      }
    }

    // Check that focx_src and focy_src within the boundaries
    // of the transmission ratio file.

    if ( focx_src < *std::min_element(focx_vec.begin(), focx_vec.end()) ||
         focx_src > *std::max_element(focx_vec.begin(), focx_vec.end()) ||  
         focy_src < *std::min_element(focy_vec.begin(), focy_vec.end()) ||
         focy_src > *std::max_element(focy_vec.begin(), focy_vec.end()) ) {

      AH_INFO(ahlog::HIGH) << "FOCX_SRC, FOCY_SRC = " << focx_src << ", " << focy_src << std::endl;
      AH_THROW_RUNTIME("Source position is outside of the boundaries defined in the transmission ratio file");        
    }

    // Convert Parameter sgdid to string this is 
    // added to the output file name
    std::stringstream sgdid_ss;
    sgdid_ss <<  sgdid;
    std::string sgdid_str = sgdid_ss.str();

    // Create output arf file ("sgdarf_sgd1_cc1.arf")
    arfFileName = outfile + "_sgd" + sgdid_str + "_cc" + ss_cc_cnt.str() + ".arf"; 
    ahfits::create(arfFileName, "",&ahffp_cc);  
    ahfits::addEmptyTbl(ahffp_cc, "TRANS_RATIO");
    ahfits::move(ahffp_cc, "TRANS_RATIO");
    ahmission::keyword::copyAllKeywords(ahffp_in,ahffp_cc,ahmission::keyword::e_HK);
    ahfits::insertColAfter(ahffp_cc,"ENERG_LO", "E", "");
    ahfits::setTUnit(ahffp_cc,"ENERG_LO","keV"); 
    ahfits::insertColAfter(ahffp_cc,"ENERG_HI", "E", "");
    ahfits::setTUnit(ahffp_cc,"ENERG_HI","keV"); 
    ahfits::insertColAfter(ahffp_cc,"SPECRESP", "E", "");
   
    // Set up connections to ARF columns
    router_arf = new ahfits::Router(ahffp_cc);
    router_arf->connectScalar(ahfits::e_WRITEONLY, "ENERG_LO", emin);
    router_arf->connectScalar(ahfits::e_WRITEONLY, "ENERG_HI", emax);
    router_arf->connectScalar(ahfits::e_WRITEONLY, "SPECRESP", TRFinal);

    // Create output RSP file ("sgdarf_sgd1_cc1.rsp")
    rspFileName = outfile + "_sgd" + sgdid_str + "_cc" + ss_cc_cnt.str() + ".rsp"; 
    ahfits::clone(rspfilelist[cc-1],rspFileName,&ahffp_rsp,true);
    if (ahfits::isPrimary(ahffp_rsp)) ahfits::firstHDU(ahffp_rsp,ahfits::e_BINARY_TBL);   // move to first binary table if extended syntax not used
    ahfits::firstRow(ahffp_rsp);

    // Set up connections to RSP columns
    router_rsp = new ahfits::Router(ahffp_rsp);
    router_rsp->connectVariableLengthArray(ahfits::e_READWRITE, "MATRIX", specresp, nspecresp);
 
    // Four points are stored in focx_tab, focy_tab, r_tab.
    // Loop over energies (i.e. elements in the 2nd dimension for r_tab).
    ahfits::firstRow(ahffp_cc);     
    for (kk = 0; kk < EBINS; ++kk) {
      
      // TRFINAL = transmission ratio [focx_src, focy_src][Energy] is obtained 
      // via interpolation of the four closest points found in the transmission
      // ratio file:
      // 
      // focx[0...3] focy[0...3] = coordinates of the four points
      // Quadrants: 0=upper right, 1=lower right, 2=lower left, 3=upper left
      //
      // Interpolation method:
      // 1. 2-D interpolation (a) at (focx_src,focy[1])
      // 2. 2-D interpolation (b) at (focx_src,focy[3])
      // 3. 2-D interplation between (a) and (b)

      a = r_tab[2][kk] + ((focx_src-focx_tab[2])/(focx_tab[1]-focx_tab[2])) 
          * (r_tab[1][kk]-r_tab[2][kk]);

      b = r_tab[3][kk] + ((focx_src-focx_tab[3])/(focx_tab[0]-focx_tab[3])) 
          * (r_tab[0][kk]-r_tab[3][kk]);
    
      TRFinal = a + ((focy_src-focy_tab[2])/(focy_tab[3]-focy_tab[2]))
          * (b - a);      
 
      // Bounds of current energy bin.
      emin = eminarr[kk];
      emax = emaxarr[kk];

      AH_DEBUG << "Emin=" << emin << " Emax=" << emax
        << " a=" << a << " b=" << b << " result=" << TRFinal << std::endl; 

      // Write energy bin min and max and transmission ratio.
      ahfits::nextRow(ahffp_cc);
      ahfits::writeRow(ahffp_cc);

      // Scale response by transmission ratio
      ahfits::readRow(ahffp_rsp);
      for (int iii=0; iii < nspecresp; iii++) {
        if (nspecresp > EBINS) {
          std::stringstream msg;
          msg << "Response array in row " << ahfits::currentRow(ahffp_rsp) 
              << " larger than expected (" << nspecresp 
              << "); maximum supported size is " << EBINS;
          AH_THROW_RUNTIME(msg.str());
        }
        specresp[iii]=specresp[iii]*TRFinal;
      }
      ahfits::writeRow(ahffp_rsp);
      ahfits::nextRow(ahffp_rsp);

    } // End loop over energies.

    // Close files for current CC.
    ahfits::close(ahffp_cc);
    ahfits::close(ahffp_rsp);
   
    // Deallocate routers.
    delete router_arf; router_arf=0;
    delete router_art; router_art=0;
    delete router_rsp; router_rsp=0;
    
  }  // End loop over CC ID.
  
  // Deallocate working arrays.
  delete [] eminarr;
  delete [] emaxarr; 
}


void finalize(ahfits::FilePtr ahffp_in, ahfits::FilePtr ahffp_art) {
 
  ahfits::close(ahffp_in); 
  ahfits::close(ahffp_art);
    
}
/** @} */


/* Revision Log
 $Log: sgdarfgen.cxx,v $
 Revision 1.27  2016/09/19 14:34:31  mdutka
 Adding check to compare parameter sgdid and INSTRUME keyword in input file

 Revision 1.26  2016/09/14 18:10:46  mdutka
 Output fits files wil now have to correct sgd id in the file name

 Revision 1.25  2016/04/11 14:57:36  mdutka
 Adding parameter stamping to standard main and case insesnsitivity for instrume keyword check

 Revision 1.24  2016/04/07 20:00:58  mdutka
 Moving high volumn logging statements to debug

 Revision 1.23  2016/02/19 21:35:36  mdutka
 removing check on rows in input file

 Revision 1.22  2015/12/29 20:13:19  mwitthoe
 sgdarfgen: fix another log message bug

 Revision 1.21  2015/12/29 20:05:40  mwitthoe
 sgdarfgen: initialize arrays and fix bug in log message

 Revision 1.20  2015/12/29 19:13:27  rshill
 Added checkEmptyTable().

 Revision 1.19  2015/12/15 10:26:37  mwitthoe
 sgdarfgen: 1) add 2nd algorithm to compute focx_src & focy_src using a cfitsio call; 2) change column names for output transmission ratio file

 Revision 1.18  2015/12/09 21:11:12  mwitthoe
 sgdarfgen: update code to use new algorithm for computing focx and focy source angles; scale response by transmission ratio

 Revision 1.17  2015/11/17 19:12:19  mwitthoe
 sgdarfgen: copy keywords from input file to output

 Revision 1.16  2015/09/04 16:04:13  rshill
 Added parameter stamping to log.

 Revision 1.15  2015/08/12 23:15:37  rshill
 Changed column names in CALDB file:  FOCX to FOCPH, FOCY to FOCTH.  Improved comments.

 Revision 1.14  2015/08/11 19:57:38  rshill
 Retract one change: look-down to look-up coords.

 Revision 1.13  2015/08/11 16:25:26  rshill
 Tweaked comments.

 Revision 1.12  2015/08/11 02:20:20  rshill
 Code and TRF cleanup.

 Revision 1.11  2015/07/15 15:21:50  klrutkow
 added caldb query

 Revision 1.10  2015/07/15 14:32:48  mdutka
 Adding validity check for instrume keyword

 Revision 1.9  2015/04/03 20:19:33  mdutka
 addressing issue closing comments from Mike W.

 Revision 1.8  2015/03/10 20:30:48  mwitthoe
 sgdarfgen: fix doxygen

 Revision 1.7  2015/02/02 14:47:52  mdutka
 Made changes to sgdarfgen after meeting on 01/29/2015

 Revision 1.6  2014/12/23 21:15:49  mdutka
 updating input parameters

 Revision 1.5  2014/11/07 22:24:10  mdutka
 removed ahgen from include statements

 Revision 1.4  2014/11/07 22:00:33  mdutka
 fixed minor bug with closing the file

 Revision 1.3  2014/10/28 13:19:48  mdutka
 Corrected error when writing the output file for cc3

 Revision 1.1  2014/09/29 13:43:47  mdutka
 Adding new tool sgdarfgen




*/
