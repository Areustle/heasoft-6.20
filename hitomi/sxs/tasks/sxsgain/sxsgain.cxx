/// \file sxsgain.cxx
/// \brief Calculate drift correction table for SXS events
/// \author Mike Witthoeft
/// \date $Date: 2016/10/13 16:04:09 $

/** 

\defgroup tool_sxsgain Calculate drift correction table for SXS (sxsgain)
@ingroup mod_sxs_tasks

The sxsgain tool calculates the time-dependent drift corrections based on a
single calibration source: cal pixel, MXS, or Fe55 filter.  From these drifts,
a pixel temperature is inferred which is also included in the output file.
The pixel temperature in correction table is used by the sxspha2pi tool to
convert PHA to the final PI value.

Source files:

  sxsgain.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahgen
  heacore/ahlog
  heacore/ape
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahgain
  astroh/mission/lib/ahmission
  astroh/sxs/lib/ahsxs

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-10  KLR     Clean-up code


*/
 
#define AHLABEL tool_sxsgain
#define AHCVSID "$Id: sxsgain.cxx,v 1.40 2016/10/13 16:04:09 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "sxsgainlib.h"
#include "ahgain/callines.h"           // calibration CALDB file access
#include "ahgain/ahgain.h"             // gain fitting routines
#include "ahsxs/ahsxs.h"               // function to check STATUS column
#include "ahsxs/engain.h"
#include "ahsxs/pixgti.h"              // functions for pixel-dependent GTI files
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"           // CALDB query
#include "ahmission/keyword.h"         // copy keywords
#include "ahmath/ahmath.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <map>
#include <vector>                       // std::vector
#include <algorithm>                    // std::sort, std::find
#include <cmath>                        // std::fmin, std::fmax

/** \addtogroup tool_sxsgain
 *  @{
 */

// *****************************************************************************


/// \brief Get parameter values
/// \param[out] par           structure with parameter values
void getPar(Par& par);

/// \brief Open input file, check for columns, load calibration line data, and
///  create output file.
/// \param[in] par            structure with parameter values
/// \param[out] havegti       true if GTI file is given
/// \param[out] gti           structure for reading GTI file
/// \param[out] linedat       structure containing calibration line data
/// \param[out] gaindat       
/// \param[out] infilelist    list of input file names
void initialize(Par& par, bool & havegti, ahsxs::GTIFile& gti, 
                callines::CalLines& linedat,
                ahsxs::engain::AllEnergyGainData & gaindat, 
                ahfits::ListStringType& infilelist);

/// \brief Group event data and perform gain fitting.
/// \param[in] par            structure with parameter values
/// \param[in] havegti        true if GTI file is given
/// \param[in,out] gti           structure for reading GTI file
/// \param[out] fpout         file pointer to output file
/// \param[in] linedat        structure containing calibration line data
/// \param[out] gaindat       
/// \param[in] infilelist     list of input file names
void doWork(const Par& par, bool havegti, ahsxs::GTIFile& gti, ahfits::FilePtr& fpout,
            callines::CalLines& linedat, 
            ahsxs::engain::AllEnergyGainData & gaindat, 
            const ahfits::ListStringType& infilelist);

/// \brief Close open FITS files.
/// \param[in] fpout          file pointer to output file
void finalize(ahfits::FilePtr& fpout);


// ****************************************************************************

/// \brief sxsgain tool
///
/// Long description
int main(int argc, char** argv) {

  Par par;                                     // structure with parameter values
  bool havegti=false;                          // true if GTI file given
  ahsxs::GTIFile gti;                          // structure for reading GTI file

  ahfits::FilePtr fpout=0;                     // FITS file pointer to output file
  callines::CalLines linedat;                  // line data for single calibration source
  ahsxs::engain::AllEnergyGainData gaindat;    // line data for single calibration source
  ahfits::ListStringType infilelist;           // list of input files

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog();
      initialize(par,havegti,gti,linedat,gaindat,infilelist);
      doWork(par,havegti,gti,fpout,linedat,gaindat,infilelist);
      finalize(fpout);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,havegti,gti,linedat,gaindat,infilelist);
        doWork(par,havegti,gti,fpout,linedat,gaindat,infilelist);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout);
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

void getPar(Par& par)
{
  // Read parameters in common with gainfit
  par.infile=ahapp::getParString("infile");
  par.outfile=ahapp::getParString("outfile");
  par.gainfile=ahapp::getParString("gainfile");
  par.tempidx=ahapp::getParInt("tempidx");
  par.gaincoeff=ahgen::strtoupper(ahapp::getParString("gaincoeff"));
  par.linefitfile=ahapp::getParString("linefitfile");
  par.linetocorrect=ahapp::getParString("linetocorrect");
  par.itypecol=ahapp::getParString("itypecol");
  par.ntemp=ahapp::getParInt("ntemp");
  par.splitcolumn="PIXEL";                     // This must be PIXEL for SXS
  par.numevent=ahapp::getParInt("numevent");
  par.minevent=ahapp::getParInt("minevent");
  par.gtifile=ahapp::getParString("gtifile");
  par.gapdt=ahapp::getParDouble("gapdt");
  par.grpoverlap=ahapp::getParDouble("grpoverlap");
  par.startenergy=ahapp::getParDouble("startenergy");
  par.stopenergy=ahapp::getParDouble("stopenergy");
  par.extraspread=ahapp::getParDouble("extraspread");
  par.pxphaoffset=ahapp::getParDouble("pxphaoffset");
  par.broadening=ahapp::getParDouble("broadening");
  par.gridprofile=ahapp::getParBool("gridprofile");
  par.fitwidth=ahapp::getParBool("fitwidth");
  par.spangti=ahapp::getParBool("spangti");
  par.ckrisetime=ahapp::getParBool("ckrisetime");
  par.calcerr=ahapp::getParBool("calcerr");
  par.writeerrfunc=ahapp::getParBool("writeerrfunc");
  par.extrap=ahapp::getParBool("extrap");

  // read numerical parameters
  par.avgwinrad=ahapp::getParDouble("avgwinrad");
  par.minwidth0=ahapp::getParDouble("minwidth0");
  par.maxitcycle=ahapp::getParInt("maxitcycle");
  par.r2tol=ahapp::getParDouble("r2tol");
  par.searchstepshift=ahapp::getParDouble("searchstepshift");
  par.maxdshift=ahapp::getParDouble("maxdshift");
  par.bisectolshift=ahapp::getParDouble("bisectolshift");
  par.searchstepwidth=ahapp::getParDouble("searchstepwidth");
  par.maxdwidth=ahapp::getParDouble("maxdwidth");
  par.bisectolwidth=ahapp::getParDouble("bisectolwidth");
  par.minwidth=ahapp::getParDouble("minwidth");
  par.nerrshift=ahapp::getParInt("nerrshift");
  par.nerrwidth=ahapp::getParInt("nerrwidth");
  par.shifterrfac=ahapp::getParDouble("shifterrfac");
  par.widtherrfac=ahapp::getParDouble("widtherrfac");

  // check for valid background type - only check first character
  std::string background=ahgen::strtoupper(ahapp::getParString("background"));
  if (background[0] == 'N')
    par.background=ahgain::e_NOBACKGROUND;
  else if (background[0] == 'C')
    par.background=ahgain::e_CONSTBACKGROUND;
  else if (background[0] == 'S')
    par.background=ahgain::e_SLOPEBACKGROUND;
  else
    AH_THROW_RUNTIME("invalid value for background parameter; expecting NONE, CONST, or SLOPE");

  // covert gaincoeff value into an ITYPE value for the gain lookup
  if (par.gaincoeff == "H") {
    par.gainitype=0;
  } else if (par.gaincoeff == "M") {
    par.gainitype=1;
  } else if (par.gaincoeff == "L") {
    par.gainitype=3;
  } else {
    std::stringstream msg;
    msg << "invalid value of gaincoeff parameter (" << par.gaincoeff
        << "); must be H, M, or L";
    AH_THROW_RUNTIME(msg.str());
  }

  // Read parameters not in gainfit
  par.calmethod=ahgen::strtoupper(ahapp::getParString("calmethod"));
  par.usemp=ahapp::getParBool("usemp");
  par.ckant=ahapp::getParBool("ckant");
  par.ckctrec=ahapp::getParBool("ckctrec");
  par.ckctel=ahapp::getParBool("ckctel");

  // Check for legal calmethod
  if (par.calmethod != "CAL-PIX" && par.calmethod != "MXS" && par.calmethod != "FE55") {
    std::stringstream msg; 
    msg << "invalid value of calmethod parameter (" << par.calmethod
        << "); must be CAL-PIX, MXS, or FE55";
    AH_THROW_RUNTIME(msg.str());
  }

  // Confirm that temperature index is positive
  if (par.tempidx <= 0) AH_THROW_RUNTIME("tempidx parameter must be greater than zero");

  // should have: minevent <= numevent
  if (par.minevent > par.numevent) {
    AH_INFO(ahlog::HIGH) << "minevent should not be larger than numevent; setting minevent=numevent" << std::endl;
    par.minevent=par.numevent;
  }

  // avgwinrad should be greater than zero
  if (par.avgwinrad <= 0) {
    AH_THROW_RUNTIME("avgwinrad should be greater than 0.");
  }

  // if startenergy or stopenergy are specified (positive value), then center
  // profile in energy range
  par.centerprof=false;
  if (par.startenergy > 0. || par.stopenergy > 0.) par.centerprof=true;

  // check for valid values of likelihood parameters
  if (par.calcerr) {
    if (par.nerrshift <= 0) AH_THROW_RUNTIME("nerrshift parameter must be greater than zero");
    if (par.nerrshift < 20) AH_INFO(ahlog::HIGH) << "small value of nerrshift parameter given (" << par.nerrshift << "); likelihood uncertainty may be inaccurate" << std::endl;
    if (par.nerrwidth <= 0) AH_THROW_RUNTIME("nerrwidth parameter must be greater than zero");
    if (par.nerrwidth < 20) AH_INFO(ahlog::HIGH) << "small value of nerrwidth parameter given (" << par.nerrshift << "); likelihood uncertainty may be inaccurate" << std::endl;
    if (par.shifterrfac <= 0.) AH_THROW_RUNTIME("shifterrfac parameter must be greater than zero");
    if (par.widtherrfac <= 0.) AH_THROW_RUNTIME("widtherrfac parameter must be greater than zero");
  }

}

// ****************************************************************************

void initialize(Par& par, bool & havegti, ahsxs::GTIFile& gti,
                callines::CalLines& linedat,
                ahsxs::engain::AllEnergyGainData & gaindat, 
                ahfits::ListStringType& infilelist) {

  std::string telescop="N/A";
  std::string instrume="N/A";
  std::string detnam="N/A";

  // store TSTART & TSTOP for all files
  std::vector<double> vec_tstart;
  std::vector<double> vec_tstop;
  std::vector<double> vec_tstart_sorted;
  std::vector<int> file_order;

  // paramenters needed for CALDB query
  std::string gainfile;
  std::string linefitfile;
  std::string filetype_gain = "gain coefficients";
  std::string filetype_lnft = "calibration line data";
  std::string codename_gain = "GAINPIX";
  std::string codename_lnft = par.linetocorrect;
  std::string datetime;

  // need to check if all input files have zero rows
  bool allempty=true;

  // iterator for file list
  ahfits::ListStringType::const_iterator fit;  // iterator over input files

  // Read GTI file, if necessary
  // This tool rejects an input GTI file which contains both general and
  // pixel-dependent GTIs, because it cannot merge overlapping GTI which
  // may occur in a fitting group for computing the exposure.
  if (ahgen::strtoupper(par.gtifile) != "NONE" && !par.gridprofile) {
    ahsxs::loadGTIFile(par.gtifile,gti);
    if (gti.m_havegen && gti.m_haveanypix) {
      AH_THROW_RUNTIME("GTI file, "+par.gtifile+", cannot contain both a general GTI and pixel-dependent GTI extensions");
    }
    if (gti.m_havegen || gti.m_haveanypix) havegti=true;
  }
  if (par.calmethod == "MXS" && !havegti) {
    AH_THROW_RUNTIME("GTI file required when calmethod=MXS");
  }

  // get list of input files
  ahfits::expandFileList(par.infile, infilelist);
  if (infilelist.size() == 0)
    AH_THROW_RUNTIME("no file list found in infile: "+par.infile);

  AH_INFO(ahlog::HIGH) << infilelist.size() << " input event files" << std::endl;
  
  // do not need to check keywords or sort input files if only computing profile
  double tstart_min=-999.;     // smallest TSTART from filelist
  if (!par.gridprofile) {

    // 1. ensure that all files have the same values for TELESCOP, INSTRUME, DETNAM
    // 2. check that required columns are present
    // 3. read TSTART and TSTOP from all input files 
    for (fit=infilelist.begin(); fit != infilelist.end(); fit++) {
      std::string infile=*fit;

      ahfits::FilePtr fp=0;
      ahfits::open(infile,"",&fp);
      if (ahfits::isPrimary(fp)) ahfits::move(fp,"EVENTS");   // if extended syntax not used to specify extension, move to EVENTS extensions
      if (0 == ahfits::numRows(fp)) continue;
      allempty=false;

      // read TELESCOP, INSTRUME, DETNAM from first file
      if (telescop == "N/A") telescop=ahfits::getKeyValStr(fp,"TELESCOP");
      if (instrume == "N/A") instrume=ahfits::getKeyValStr(fp,"INSTRUME");
      if (detnam == "N/A") detnam=ahfits::getKeyValStr(fp,"DETNAM");

      // check that TELESCOP, INSTRUME, DETNAM agree with first file
      std::string tmp_telescop=ahfits::getKeyValStr(fp,"TELESCOP");
      std::string tmp_instrume=ahfits::getKeyValStr(fp,"INSTRUME");
      std::string tmp_detnam=ahfits::getKeyValStr(fp,"DETNAM");

      if (tmp_telescop != telescop || tmp_instrume != instrume || tmp_detnam != detnam) {
        AH_THROW_RUNTIME("Mismatch of TELESCOP, INSTRUME, & DETNAM between input files");
      }

      // Check for require columns
      bool missing=false;
      if (!ahfits::haveColumn(fp,"TIME")) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing TIME column" << std::endl;
      }
      if (!ahfits::haveColumn(fp,"PROC_STATUS")) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing PROC_STATUS column" << std::endl;
      }
      if (!ahfits::haveColumn(fp,par.splitcolumn)) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing " << par.splitcolumn << " column" << std::endl;
      }
      if (missing)
        AH_THROW_RUNTIME("Input file, "+infile+", missing columns... stopping program.");
  
      // Store TSTART & TSTOP
      vec_tstart.push_back(ahfits::getKeyValDbl(fp,"TSTART"));
      vec_tstop.push_back(ahfits::getKeyValDbl(fp,"TSTOP"));

      if (tstart_min < 0.) {
        tstart_min=vec_tstart.back();
        datetime=ahfits::getKeyValStr(fp,"DATE-OBS");
      } else if (vec_tstart.back() < tstart_min) {
        tstart_min=vec_tstart.back();
        datetime=ahfits::getKeyValStr(fp,"DATE-OBS");
      }

      ahfits::close(fp);
      
      AH_INFO(ahlog::HIGH) << "TELESCOP = " << telescop << std::endl;
      AH_INFO(ahlog::HIGH) << "INSTRUME = " << instrume << std::endl;
      AH_INFO(ahlog::HIGH) << "DETNAM = " << detnam << std::endl;
      
    }

    // if all input files are empty, then throw error
    if (allempty) AH_THROW_RUNTIME("all input FITS tables contain no rows; tool cannot continue");

    // check if any files overlap in time (illegal) and sort files by TSTART
    vec_tstart_sorted=vec_tstart;
    std::sort(vec_tstart_sorted.begin(),vec_tstart_sorted.end());
    bool overlap=false;
    for (int i=0; i < (int)vec_tstart.size(); i++) {
      // get sorted index for current file
      int pos=std::find(vec_tstart_sorted.begin(),vec_tstart_sorted.end(),vec_tstart[i])-vec_tstart_sorted.begin();
      file_order.push_back(pos);

      // check if current file overlaps with any previous file
      for (int j=0; j < i; j++) {
        if ( ((vec_tstart[i] <= vec_tstart[j]) && (vec_tstop[i] >= vec_tstart[j])) ||
             ((vec_tstart[j] <= vec_tstart[i]) && (vec_tstop[j] >= vec_tstart[i])) ) {
          overlap=true;
          AH_INFO(ahlog::HIGH) << "Illegal: files " << i << " and " << j << " overlap in time." << std::endl;
        }
      }
    }
    AH_INFO(ahlog::LOW) << "File list has been sorted." << std::endl;
    if (overlap) AH_THROW_RUNTIME("Overlapping files found... stopping program.");

    // Rearrange input files based on sort
    ahfits::ListStringType infilelist_sorted=infilelist;    // copy being done, just to get size correct
    for (int i=0; i < (int)vec_tstart.size(); i++) {
      infilelist_sorted[file_order[i]]=infilelist[i];
    }
    infilelist=infilelist_sorted;                           // copy sorted list into main variable
    AH_INFO(ahlog::LOW) << std::endl;
    AH_INFO(ahlog::LOW) << "After sorting, following files will be processed in order: " << std::endl;
    for (int i=0; i < (int)vec_tstart.size(); i++)
      AH_INFO(ahlog::LOW) << "  " << infilelist[i] << std::endl;
    AH_INFO(ahlog::LOW) << std::endl;
  }

  //obtain gain file path from parameter or CALDB
  gainfile = ahmission::caldb::resolve(par.gainfile,filetype_gain,instrume,
                                       detnam,codename_gain,datetime); 
  ape_trad_set_string("gainfile",gainfile.c_str());

  // open CALDB file with gain information per pixel per temperature
  ahsxs::engain::loadEnergyGainData(gainfile,gaindat);
  AH_INFO(ahlog::HIGH) << "Opened "<<gainfile<<", "<< gaindat.m_numTemps <<" temperatures" << std::endl;

  // check that tempidx is in range of the temperatures in gain file
  if (par.tempidx > gaindat.m_numTemps) {
    std::stringstream msg;
    msg << "tempidx parameter value (" << par.tempidx << ") exceeds number of "
        << "temperatures in gain file (" << gaindat.m_numTemps << ")";
    AH_THROW_RUNTIME(msg.str());
  }

  //obtain line file path from parameter or CALDB
  linefitfile = ahmission::caldb::resolve(par.linefitfile,filetype_lnft,"GEN", "-",codename_lnft,datetime);
  ape_trad_set_string("linefitfile",linefitfile.c_str());

  // Read calibration line data from CALDB file
  callines::load(linefitfile,par.linetocorrect,linedat);
  AH_INFO(ahlog::HIGH) << "Opened "<<linefitfile<<", "<< linedat.m_nlines <<" components in feature " << par.linetocorrect << std::endl;

  // ** Hidden feature **
  // If the ntemp parameter is negative, then use the energy vs temperature 
  // table to compute the temperature instead of the PHA vs temperature table.
  par.useevst=true;
  if (par.ntemp < 0) {
    AH_INFO(ahlog::HIGH) << "Using PHA vs. temperature table to compute temperature since the ntemp parameter is negative." << std::endl;
    par.useevst=false;         // useevst is not a real parameter
    par.ntemp=-par.ntemp;
  }

  // Check validity of ntemp
  if (par.ntemp < 2) {
    AH_THROW_RUNTIME("the ntemp parameter must have a value of at least 2");
  }
  par.ntemp = std::min(par.ntemp,gaindat.m_numTemps);    // cannot be larger than the number of rows in the gainfile

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(const Par& par, bool havegti, ahsxs::GTIFile& gti, 
            ahfits::FilePtr& fpout, callines::CalLines& linedat,
            ahsxs::engain::AllEnergyGainData & gaindat, 
            const ahfits::ListStringType& infilelist) {

  // declare variables
  int nmesh=0;                                // size of binning grid
  int maxnmesh=0;                             // max size of binning grid
  int pnmesh=0;                               // size of individual pixel binning grid
  int imesh[ahsxs::NPIXEL] = { 0 };           // Index tracker for pixel meshes

  double* emesh=0;                            // used to compute profile
  double* mesh=0;                             // total profile mesh
  double** pmesh=new double*[ahsxs::NPIXEL];  // Individual pixel profile mesh
  double gwidth=0.;                           // convolution width computed from FWHM parameter

  long long irow=0;                           // event loop row counter

  double efirst=0.;                           // energy of first bin
  double elast=0.;                            // energy of last bin
  double chfirst=-1.;                         // channel of first bin
  double chlast=-1.;                          // channel of last bin
  double pixfirst=-1.;                        // channel of first bin for pixel
  double pixlast=-1.;                         // channel of last bin for pixel

  // fitting profile and results
  std::vector<ahgain::CalProfile> profdat(ahsxs::NPIXEL);    // theoretical profile per pixel; convolved
  ahgain::EventData eventRow;                 // data for single row from infile
  ahgain::FitResults results;                 // data for single output row
  int rise_time=0;                            // value of RISE_TIME column
  double temp_fit = 0.;                       // Temperature derived from fitted PHA
  double temp_ave = 0.;                       // Temperature derived from average PHA
  double pr_energy=0.;                        // ENERGY column in 2nd EXT of output
  double pr_amplitude[ahsxs::NPIXEL]= {0.};   // AMPLITUDEnn column in 2nd EXT of output

  double tstart=-1.;                          // after event loop, will contain TIME from first event used
  double tstop=0.;                            // after event loop, will contain TIME from last event used
  int nevent=0;                               // after event loop, will contain total number of events read
  int nskip_procstatus=0;                     // after event loop, will contain total number of events skipped due to PROC_STATUS
  int nskip_timenull=0;                       // after event loop, will contain total number of events skipped due to TIME=NULL
  int nskip_risetime=0;                       // after event loop, will contain total number of events skipped due to RISE_TIME > 127
  std::vector<int> ninrange(ahsxs::NPIXEL);   // after event loop, will contain total number of events in PHA range per pixel
  std::vector<int> nvalid(ahsxs::NPIXEL);     // after event loop, will contain total number of events passing ITYPE, cross-talk, etc
  std::vector<int> ningti(ahsxs::NPIXEL);     // after event loop, will contain total number of events in GTI per pixel
  std::vector<int> nused(ahsxs::NPIXEL);      // after event loop, will contain total number of events fitted per pixel
  std::vector<int> ngroups(ahsxs::NPIXEL);    // after event loop, will contain number of fitting groups per pixel
  std::vector<int> ngoodfit(ahsxs::NPIXEL);   // after event loop, will contain number of successful fits per pixel

  std::map<int, double> gtigaptime;           // cumulative time that GTI is off, used to compute EXPOSURE
  int nptremove=0;                            // number of points to remove after processing; determined with the grpoverlap parameter

  // tolerance for reverse lookup of gain
  double revlooktol=1.e-8;

  // In the case of not fitting the width, we need a non-zero intrinsic width 
  // of the calibration feature to determine the range of shifts to use in
  // the uncertainty calculation.  The precise value is not important.
  double basewidth=0.5;

  // In order to compute the exposure for each group, need to keep track of
  // GTI gap intervals inside each group per pixel.  Each GTI gap interval is
  // represented as stop,start in a std::pair where stop is from the previous
  // GTI and start is from the current GTI.  The list of GTI gap intervals in
  // stored in a std::vector.  And the map takes pixel number to the GTI list.
  GTIIntervalsMap gtigaps;
  for (int ipix=0; ipix < ahsxs::NPIXEL; ipix++) {    // initialize vector for each pixel
    gtigaps.insert(GTIIntervalsMap::value_type(ipix,GTIIntervals()));
  }

  // Variable to flag output columns as NULL
  char nullcol[1] = { 0 };

  ahfits::ListStringType::const_iterator fit;  // iterator over input files

  ahfits::FilePtr fpin=0;              // ahfits FITS file pointer to input file
  ahfits::Router* routin=0;            // router connecting to input file
  ahfits::Router* routout=0;           // router connecting to output file
  ahgain::EventVecMap eventdat;        // store groups of points for each splitcolumn value

  // declare variables -- not in gainfit
  int itype=0;                         // ITYPE column

  // Store STATUS from input file -- not in gainfit
  char col_status[ahsxs::LEN_SXSSTATUS];
  ahfits::IndexType len_status=ahsxs::LEN_SXSSTATUS;
  ahsxs::clear_status(col_status);

  // Store PROC_STATUS value from input file
  char proc_status[LENPROCSTATUS];
  ahfits::IndexType len_proc_status=LENPROCSTATUS;
  for (int k=0; k < LENPROCSTATUS; k++) proc_status[k]=0;
  for (int k=0; k < ahsxs::NPIXEL; k++) imesh[k]=0;

  // If the startenergy and stopenergy parameters are negative, automatically
  // determine energy range from calibration line energies. 
  efirst=par.startenergy;
  if (efirst < 0) efirst=callines::getMinEnergy(linedat)-par.extraspread;
  elast=par.stopenergy;
  if (elast < 0) elast=callines::getMaxEnergy(linedat)+par.extraspread;
  AH_INFO(ahlog::HIGH) << "Search range for calibration feature is [" << efirst << " : " << elast << "] in eV" << std::endl;

  // First need to get the nominal size of the PHA mesh for each pixel
  // For convenience, use the same mesh for all pixels constructed using
  // the smallest start PHA channel and largest stop PHA channel
  AH_INFO(ahlog::LOW) << "Determine PHA mesh for each pixel" << std::endl;
  for (int ip = 0; ip < ahsxs::NPIXEL; ++ip) {

    // Convert energy range into PHA units for each pixel.  We include an
    // offset from the pxphaoffset parameter which reflects how the PHA values
    // in the gain file are defined.  If the gain PHA represents the center of
    // the PHA bin, then the offset should be 0.5.
    pixfirst = par.pxphaoffset+(int)ahsxs::engain::reverseLookup(efirst,ip,par.gainitype,gaindat.m_engainVec[par.tempidx-1],revlooktol);
    pixlast = par.pxphaoffset+(int)ahsxs::engain::reverseLookup(elast,ip,par.gainitype,gaindat.m_engainVec[par.tempidx-1],revlooktol);
    
    // Calculate size of pixel mesh
    pnmesh = 1+(int)(pixlast-pixfirst); //using binwidth
    if(maxnmesh < pnmesh) maxnmesh=pnmesh; // save the largest pnmesh
    // Calculate number of mesh points needed for binning mesh
    if (pnmesh <= MINPT) {
      std::stringstream msg;
      msg << "number of binning points for pixel " << ip << " smaller than minimum: " << pnmesh << " < " << MINPT;
      AH_THROW_RUNTIME(msg.str());
    }
    if (pnmesh > MAXPT) {
      std::stringstream msg;
      msg << "number of binning points for pixel " << ip << " exceeds maximum: " << nmesh << " > " << MAXPT;
      AH_THROW_RUNTIME(msg.str());
    }
    AH_INFO(ahlog::LOW) << "                          pixel " << ip << " mesh [" << pixfirst << " : " << pixlast << "] in channel units" << std::endl;
    AH_INFO(ahlog::HIGH) << "Profile and binning mesh for pixel " << ip << " covering search range has " << pnmesh << " points " << std::endl;
    profdat[ip].m_nmesh = pnmesh;
    pmesh[ip] = new double[pnmesh];
    for (int ii=0; ii < pnmesh; ii++) { 
      pmesh[ip][ii] = 0;
      pmesh[ip][ii] = pixfirst+ii;
    }

    // Save the lowest/highest pha channels for total mesh
    if(chfirst < 0)
      chfirst = pixfirst;
    else
      chfirst = fmin(chfirst,pixfirst);
    if(chlast < 0)
      chlast = pixlast;
    else
      chlast = fmax(chlast,pixlast);

  }

  // Calculate size of mesh
  nmesh=1+(int)(chlast-chfirst); // using binwidth = 1

  // Convert energy range into channel units
  AH_INFO(ahlog::LOW) << "                             total mesh [" << chfirst << " : " << chlast << "] in channel units" << std::endl;
  AH_INFO(ahlog::HIGH) << "Profile and binning mesh covering search range has " << nmesh << " points" << std::endl;
  
  // Construct bin mesh
  mesh=new double[nmesh];
  emesh=new double[nmesh];
  for (int ii=0; ii < nmesh; ii++) {
    // Initialize mesh arrays
    mesh[ii]=0;
    emesh[ii]=0;

    // construct energy mesh
    mesh[ii]=chfirst+ii;
  }

  // need to construct separate profile for each pixel
  for (int ip = 0; ip < ahsxs::NPIXEL; ++ip) {
    // for computing the profile, need to have a mesh over energy
    double energy = 0.;
    char enull = 0;
    for (int ii = 0; ii < profdat[ip].m_nmesh; ++ii) {
      ahsxs::engain::applyGain(pmesh[ip][ii],ip,par.gainitype,gaindat.m_engainVec[par.tempidx-1],energy,enull);
      emesh[ii] = energy;
    }

    // Construct profile using Lorentzians for each calibration line and
    // convolve with Gaussian
    gwidth=ahmath::convertFWHM2sigma(par.broadening);    // convert to std dev in channel units
    ahgain::constructCalibrationProfile(emesh,profdat[ip].m_nmesh,1,linedat,
                                        par.centerprof,gwidth,profdat[ip]);

    // When using centerprof, we need to convert profile offset from energy
    // to PHA units.  Since offset is a relative value, not an absolute value,
    // we have to do the conversion on the position of the shifted profile:
    // prof_avg + offset.  Here we do this conversion and the new offset will
    // be calculated after the original profile average is converted (below).
    double profavg_old=profdat[ip].m_avg_prof;
    double profavg_new=profdat[ip].m_avg_prof+profdat[ip].m_offset;
    if (par.centerprof) {
      profavg_old = ahsxs::engain::reverseLookup(profavg_old,ip,par.gainitype,
                    gaindat.m_engainVec[par.tempidx-1],revlooktol);

      profavg_new = ahsxs::engain::reverseLookup(profavg_new,ip,par.gainitype,
                    gaindat.m_engainVec[par.tempidx-1],revlooktol);
    }

    // Will be fitting with PHA, so replace energy quantities of profdat with PHA mesh
    for(int ii = 0; ii < profdat[ip].m_nmesh; ++ii) profdat[ip].m_mesh[ii] = pmesh[ip][ii];
    ahgain::calcAverageAndVariance(pmesh[ip],profdat[ip].m_prof,profdat[ip].m_nmesh,
                                   profdat[ip].m_avg_prof,profdat[ip].m_var_prof);

    AH_INFO(ahlog::LOW) << "Average of calibration profile on mesh " << ip 
                        << " is: " << profdat[ip].m_avg_prof << std::endl;
    AH_INFO(ahlog::LOW) << "Variance of calibration profile on mesh " << ip 
                        << " is: " << profdat[ip].m_var_prof << std::endl;

    // Now that we have both the offset profile average and the original profile
    // average in PHA units, we can compute the new offset value.
    if (par.centerprof) {
      profdat[ip].m_offset=profavg_new-profdat[ip].m_avg_prof;
    }
  }

  // Create output gain file 
  // Note: this is not done in initialize, because we do not know the number
  // of bin mesh points until now.
  createGainOutput(par.outfile,"PIXEL",ahmission::getTELESCOPString(),
                   "SXS","PIXEL",par.gridprofile,par.calmethod,
                   par.tempidx,gaindat.m_temps[par.tempidx-1],par.linetocorrect,
                   chfirst,chlast,1,"PHA",maxnmesh,par.calcerr, par.writeerrfunc,
                   par.nerrshift, par.nerrwidth,fpout);
  ahfits::move(fpout,"Grid_profile");

  // Create router for writing output file
  routout=new ahfits::Router(fpout);

  // Write profile to output
  routout->connectScalar(ahfits::e_WRITEONLY,"PHA",pr_energy);
  for(int ip = 0; ip<ahsxs::NPIXEL; ++ip) {
    // Create string for columns AMPLITUDE[00:35]
    std::stringstream amp;
    if(ip<10) amp << "AMPLITUDE" << 0 << ip;
    else amp << "AMPLITUDE" << ip;
    // Can't pass in array elements individually, since they are pointers
    // Need to dereference pointer element in order to loop over elements
    // Cleaner than making 36 individual variables
    routout->connectScalar(ahfits::e_WRITEONLY,amp.str(),*(pr_amplitude+ip));
  }

  ahfits::firstRow(fpout);
  for (int ii=0; ii < nmesh; ++ii) {
    pr_energy=mesh[ii];
    for(int ip = 0; ip < ahsxs::NPIXEL; ++ip) {
      int curr_nmesh = profdat[ip].m_nmesh-1;
      if(pr_energy>=profdat[ip].m_mesh[0]&&pr_energy<=profdat[ip].m_mesh[curr_nmesh]&&imesh[ip]<=curr_nmesh) {
        pr_amplitude[ip]=profdat[ip].m_prof[imesh[ip]];
        imesh[ip]++;
      } else { 
        pr_amplitude[ip] = 0;
      }
    }
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);
  }

  // Done if only generating profile
  if (par.gridprofile) return;

  // Reset router connections; move to Drift_energy extension in output file
  routout->clearConnections();
  ahfits::move(fpout,"Drift_energy");

  // The number of bins is dependent on the pixel. For now, allocate
  // space for the largest size of a pixel mesh (saved earlier).
  // results is updated every time fitEvents is called, except for m_mesh.
  // m_mesh is updated outside of fitEvents in the event loop and saved in
  // the binmesh column
  //
  ahgain::initializeFitResults(mesh,maxnmesh,par.nerrshift,par.nerrwidth,results);

  // Set up connections to output file
  routout->connectScalar(ahfits::e_WRITEONLY,"TIME",results.m_time);
  routout->connectScalar(ahfits::e_WRITEONLY,par.splitcolumn,results.m_splitval);
  routout->connectScalar(ahfits::e_WRITEONLY,"COR_FIT",results.m_cor_fit,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"COR_AVE",results.m_cor_ave);
  routout->connectScalar(ahfits::e_WRITEONLY,"CHISQ",results.m_chisq,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"AVGUNBIN",results.m_avgunbin);
  routout->connectScalar(ahfits::e_WRITEONLY,"AVGBIN",results.m_avgbin);
  routout->connectScalar(ahfits::e_WRITEONLY,"AVGFIT",results.m_avgfit,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"SHIFT",results.m_fit_shift,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"SCALE",results.m_fit_scale,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"BGRND",results.m_fit_bgrnd,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"WIDTH",results.m_fit_width,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"TELAPSE",results.m_telapse);
  routout->connectScalar(ahfits::e_WRITEONLY,"EXPOSURE",results.m_exposure);
  routout->connectScalar(ahfits::e_WRITEONLY,"NEVENT",results.m_nevent);
  routout->connectScalar(ahfits::e_WRITEONLY,"TEMP_FIT",temp_fit,nullcol);
  routout->connectScalar(ahfits::e_WRITEONLY,"TEMP_AVE",temp_ave);
  routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"BINMESH",results.m_mesh);
  routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"SPECTRUM",results.m_bindat);
  routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"FITPROF",results.m_fitdat);
  if (par.calcerr) {
    routout->connectScalar(ahfits::e_WRITEONLY,"SIGSHLIKE",results.m_sigshlike);
    routout->connectScalar(ahfits::e_WRITEONLY,"SIGWDLIKE",results.m_sigwdlike);
    routout->connectScalar(ahfits::e_WRITEONLY,"SIGSHCHI2",results.m_sigshchi2);
    routout->connectScalar(ahfits::e_WRITEONLY,"SIGWDCHI2",results.m_sigwdchi2);
    if (par.writeerrfunc) {
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"SHIFTS",results.m_shifts);
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"SHLIKE",results.m_shlike);
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"SHCHI2",results.m_shchi2);
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"WIDTHS",results.m_widths);
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"WDLIKE",results.m_wdlike);
      routout->connectFixedLengthArray(ahfits::e_WRITEONLY,"WDCHI2",results.m_wdchi2);
    }
  }

  // Normally, we only want to include Hp events.  However, the usemp parameter
  // can be set to also include Mp events.  -- not in gainfit
  int itypemax=0;                 // Hp 
  if (par.usemp) itypemax=1;      // + Mp

  // Determine the number of points to remove from the grpoverlap parameter.
  // Note: this number is always with respect to a full group (size numevent),
  // which is okay since groups are always emptied if there are less than this
  // number in a group (e.g. if a GTI is crossed).
  nptremove=par.numevent;
  if (par.grpoverlap >= 0.) nptremove=(int)(100.-par.grpoverlap)*par.numevent/100;   // note: grpoverlap is a percentage
  if (nptremove <= 0) nptremove=1;   // always remove at least one point
  AH_INFO(ahlog::LOW) << "Removing "<<nptremove<<" points, based on the input parameter grpoverlap="<< par.grpoverlap << std::endl;

  // Loop over all input files
  tstart=-1.;         // after event loop, will contains TIME from first event used
  tstop=0.;           // after event loop, will contains TIME from last event used  
  nevent=0;           // total number of events read
  nskip_procstatus=0; // total number of events skipped due to PROC_STATUS
  nskip_timenull=0;   // total number of events skipped due to TIME=NULL
  for (fit=infilelist.begin(); fit != infilelist.end(); fit++) {
    std::string infile=*fit;

    // Open current file
    ahfits::open(infile,"",&fpin);
    if (ahfits::isPrimary(fpin)) ahfits::move(fpin,"EVENTS");   // if extended syntax not used to specify extension, move to EVENTS extensions

    AH_INFO(ahlog::HIGH) << "Processing file: "<< infile << std::endl;

    // copy keywords from first input file to both output extensions
    if (fit == infilelist.begin()) {
      ahmission::keyword::copyAllKeywords(fpin,fpout,ahmission::keyword::e_EVENT);

      ahfits::FilePtr fpprof=0;
      ahfits::open(par.outfile,"Grid_profile",&fpprof);
      ahmission::keyword::copyAllKeywords(fpin,fpprof,ahmission::keyword::e_EVENT);
      ahfits::close(fpprof);
    }

    // Connect local variables to infile columns
    routin=new ahfits::Router(fpin);
    routin->connectScalar(ahfits::e_READONLY,"TIME",eventRow.m_time,&eventRow.m_timenull);
    routin->connectScalar(ahfits::e_READONLY,"PHA",eventRow.m_pi);
    if (par.ckrisetime) routin->connectScalar(ahfits::e_READONLY,"RISE_TIME",rise_time);
    routin->connectBit(ahfits::e_READONLY,"PROC_STATUS",proc_status,len_proc_status);
    routin->connectScalar(ahfits::e_READONLY,par.splitcolumn,eventRow.m_split);

    // Connect other local variables which are not in gainfit
    routin->connectScalar(ahfits::e_READONLY,par.itypecol,itype);
    routin->connectBit(ahfits::e_READONLY,"STATUS",col_status,len_status);

    // Main EVENT loop
    irow=0;
    for (ahfits::firstRow(fpin); ahfits::readOK(fpin); ahfits::nextRow(fpin) ) {
      bool addevent=true;       // true if adding event to queue
      bool emptyme=false;       // true if need to process and empty current pixel group
      bool emptyall=false;      // true if need to process and empty all queues

      int status=0;

      // Reset null flag
      nullcol[0] = 0;

      ahfits::readRow(fpin);
      nevent++;
      irow++;

      // Skip row if PROC_STATUS is bad
      if (!procstatus::processRow(proc_status)) {
        ++nskip_procstatus;
        AH_DEBUG << "Skipping row "<<irow<<": bad PROC_STATUS" << std::endl;
        continue;
      }

      // Skip row if TIME is NULL
      if (eventRow.m_timenull == 1) {
        ++nskip_timenull;
        AH_DEBUG << "Skipping row "<<irow<<": TIME=NULL" << std::endl;
        continue;
      }

      // Skip row if RISE_TIME > 127
      if (par.ckrisetime && rise_time > 127) {
        ++nskip_risetime;
        AH_DEBUG << "Skipping row "<<irow<<": RISE_TIME > 127" << std::endl;
        continue;
      }

      // Skip if event not in range of profile
      // This is actually PHA, not PI as the variable says
      // PHA is never null
      pnmesh = profdat[eventRow.m_split].m_nmesh;
      pixfirst = profdat[eventRow.m_split].m_mesh[0];
      pixlast = profdat[eventRow.m_split].m_mesh[pnmesh-1];
      if (eventRow.m_pi < pixfirst || eventRow.m_pi > pixlast) continue;
      ++ninrange[eventRow.m_split];

      // skip if not Hp
      if (itype > itypemax) continue;    // itype populated by ahfits::readRow() call

      // based on calibration method, some events are to be skipped
      // Note: status populated by ahfits::readRow() call
      if (par.calmethod == "CAL-PIX" && eventRow.m_split != 12) {
        continue;
      }

      // If necessary, reject antico and cross talk events (electric and recoil)
      if (par.ckant && ahsxs::has_status_antico(col_status)) continue;
      if (par.ckctrec && ahsxs::has_status_recoil_crosstalk(col_status)) continue;
      if (par.ckctel && ahsxs::has_status_electrical_crosstalk_residual(col_status)) continue;
      
      // end sxsdrit-specific checks
      ++nvalid[eventRow.m_split];

      // Make sure that gtigaptime is initialized for current PIXEL
      //if (0 == gtigaptime.count(eventRow.m_split)) gtigaptime[eventRow.m_split]=0.;

      // Check if TIME in GTI interval
      // If active GTI ends (and the spangti parameter is false), then need to 
      // process event queues for all pixels; in which case emptyall=true
      // note: if emptyall=true and addevent=true; then event is added to start
      // of next queue (after current queue is processed)
      addevent=true;
      if (havegti) {
        if (gti.m_havegen) {
          while (eventRow.m_time > gti.m_stopgen) {     // looking for GTI either including or after current event
            if (!par.spangti) emptyall=true;
            double prev_stop=gti.m_stopgen;
            bool readok=ahsxs::readNextGeneralGTI(gti);
            if (!readok) {                                // event TIME past last GTI
              addevent=false;
              break;
            } else {
              if (par.spangti && eventdat[eventRow.m_split].size() > 0) {    // if spanning GTI intervals and group for current pixel is not empty
                gtigaptime[eventRow.m_split]+=gti.m_startgen-prev_stop;
                gtigaps[eventRow.m_split].push_back(GTIInterval(prev_stop,gti.m_startgen));
              }
            }
          }
          if (eventRow.m_time < gti.m_startgen) addevent=false;     // event is between GTI, do not add to group
        } else if (gti.m_havepix[eventRow.m_split]) {
          while (eventRow.m_time > gti.m_stoppix[eventRow.m_split]) {     // looking for GTI either including or after current event
            if (!par.spangti) emptyall=true;
            double prev_stop=gti.m_stoppix[eventRow.m_split];
            bool readok=ahsxs::readNextPixelGTI(gti,eventRow.m_split);
            if (!readok) {                                // event TIME past last GTI
              addevent=false;
              break;
            } else {
              if (par.spangti && eventdat[eventRow.m_split].size() > 0) {    // if spanning GTI intervals and group for current pixel is not empty
                gtigaptime[eventRow.m_split]+=gti.m_startpix[eventRow.m_split]-prev_stop;
              }
            }
          }
          if (eventRow.m_time < gti.m_startpix[eventRow.m_split]) addevent=false;     // event is between GTI, do not add to group
        }
      }
      if (addevent) ningti[eventRow.m_split]++;

      // check if gap time exceeded b/w current event and last event in group
      if (par.gapdt > 0.) {    // if <= 0, allow infinite gap
        if (addevent && !emptyall && eventdat[eventRow.m_split].size() > 0) {
          if (eventRow.m_time - eventdat[eventRow.m_split].end()->m_time > par.gapdt) emptyme=true;
          AH_INFO(ahlog::LOW) << "Encountered large time gap between event at row #"<<irow<<" for pixel #"<<eventRow.m_split << std::endl;
          AH_INFO(ahlog::LOW) << "Processing current group and starting new group." << std::endl;
        }
      }

      // Update tstart and tstop for output keywords
      if (tstart < 0.) tstart=eventRow.m_time;
      tstop=eventRow.m_time;

      // Add event to current queue
      // Note: if empty==true, then event will be added to group after processing
      if (addevent && !emptyall && !emptyme) {
        if (0 == eventdat.count(eventRow.m_split)) eventdat[eventRow.m_split]=ahgain::EventDataVec();
        eventdat[eventRow.m_split].push_back(eventRow);
      }

      // process pixel groups
      if (emptyall) {    // GTI boundary crossed, process all queues -- in this case, EXPOSURE=TELAPSE
        AH_INFO(ahlog::LOW) << std::endl;
        AH_INFO(ahlog::LOW) << "GTI boundary crossed, processing all pixel groups" << std::endl;
        for (ahgain::EventVecMap::iterator mit=eventdat.begin(); mit != eventdat.end(); mit++) {  // iterating over all groups
          // mit->first  = split column value
          // mit->second = vector of events (as EventDataVec) 
          if (par.minevent > (int)(mit->second).size()) {      // need at least minevent points to process group
            AH_INFO(ahlog::LOW) << "Skipping group; too few events: pixel=" << mit->first << ", npoints=" << (mit->second).size() << std::endl;
            continue;
          }
          AH_INFO(ahlog::LOW) << "Processing group: pixel=" << mit->first << ", npoints=" << (mit->second).size() << std::endl;
          nused[mit->first] += (int)(mit->second).size();
          ngroups[mit->first]++;
          nullcol[0]=0;            // reset NULL flag
          if (par.calcerr) ahgain::resetUncertainties(results);
          ahgain::fitEvents((mit->second),(mit->second).size(),profdat[mit->first],
                            gwidth,par.fitwidth,par.background,par.avgwinrad,
                            par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                            par.maxdshift,par.bisectolshift,par.searchstepwidth,
                            par.minwidth,par.maxdwidth,par.bisectolwidth,results,
                            status);
          if (status != 0) {     // fitting failed
            int splitval=mit->first;
            AH_INFO(ahlog::HIGH) << "Failure fitting group with splitval = " << splitval << std::endl;
            (mit->second).clear();   // empty group
            setNullRow(profdat[mit->first], results, nullcol);
          } else {
            // Calculate temperature from fitted and average PHA
            ngoodfit[mit->first]++;
            double profavg=profdat[mit->first].m_avg_prof;
            temp_fit = computeTemperature(results.m_avgfit,mit->first,par.gainitype,
                                          par.useevst,gaindat,profavg,par.extrap);
            AH_INFO(ahlog::LOW) << "Pixel " << mit->first << 
                                   ", group size " << (mit->second).size() << 
                                   ": temp from fitted PHA = " << temp_fit << std::endl;

            if (par.calcerr) {

              // shift uncertainity
              if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
                ahgain::computeLikelihoodShift(results);  
                ahgain::computeChi2ShiftUncertainty(results);
              }

              // width uncertainty (only if fitting the width)
              if (par.fitwidth) {
                if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
                  ahgain::computeLikelihoodWidth(results,profdat[mit->first],gwidth);
                  ahgain::computeChi2WidthUncertainty(results,profdat[mit->first],gwidth);
                }
              }
            }    // end if calcerr
          }

          // temperature from average calculation is always done
          double profavg=profdat[mit->first].m_avg_prof;
          temp_ave = computeTemperature(results.m_avgbin,mit->first,par.gainitype,
                                        par.useevst,gaindat,profavg,par.extrap);
          AH_INFO(ahlog::LOW) << "Pixel " << mit->first << 
                                 ", group size " << (mit->second).size() << 
                                 "  temp from average PHA = " << temp_ave << std::endl;

          // Since emptyall=true, we are not spanning GTI and EXPOSURE=TELAPSE
          results.m_exposure=results.m_telapse;
          ahfits::writeRow(fpout);    // write contents of results variable (see connect() calls above)
          ahfits::nextRow(fpout);
          (mit->second).clear();      // empty event group just processed
        }
      } else if (emptyme || (int)eventdat[eventRow.m_split].size() > par.numevent+par.minevent) {   // group has enough events to guarantee that the next group has at least minevent events
        // Should process numevent points unless emptyme is set in which case process everything.
        int npoints=par.numevent;
        if (emptyme) npoints=(int)eventdat[eventRow.m_split].size();
        
        AH_INFO(ahlog::LOW) << std::endl;
        AH_INFO(ahlog::LOW) << "Processing group: pixel=" << eventRow.m_split << ", npoints=" << npoints << std::endl;
        nused[eventRow.m_split] += npoints;
        ngroups[eventRow.m_split]++;
        if (par.calcerr) ahgain::resetUncertainties(results);
        ahgain::fitEvents(eventdat[eventRow.m_split],npoints,profdat[eventRow.m_split],
                          gwidth,par.fitwidth,par.background,par.avgwinrad,
                          par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                          par.maxdshift,par.bisectolshift,par.searchstepwidth,
                          par.minwidth,par.maxdwidth,par.bisectolwidth,results,status);

        if (status != 0) {     // fitting failed
            int splitval=eventdat[eventRow.m_split][0].m_split;
            AH_INFO(ahlog::HIGH) << "Failure fitting group with splitval = " << splitval << std::endl;
            setNullRow(profdat[eventRow.m_split], results, nullcol);
        } else {
          // Calculate temperature from fitted and average PHA
          ngoodfit[eventRow.m_split]++;
          double profavg=profdat[eventRow.m_split].m_avg_prof;
          temp_fit = computeTemperature(results.m_avgfit,eventRow.m_split,par.gainitype,
                                        par.useevst,gaindat,profavg,par.extrap);
          AH_INFO(ahlog::LOW) << "Pixel " << eventRow.m_split << 
                                 ", group size " << npoints << 
                                 ": temp from fitted PHA = " << temp_fit << std::endl;

          if (par.calcerr) {

            // shift uncertainties
            if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
              ahgain::computeLikelihoodShift(results);
              ahgain::computeChi2ShiftUncertainty(results);
            }

            // width uncertainty (only if fitting the width)
            if (par.fitwidth) {
              if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
                ahgain::computeLikelihoodWidth(results,profdat[eventRow.m_split],gwidth);
                ahgain::computeChi2WidthUncertainty(results,profdat[eventRow.m_split],gwidth);
              }
            }
          }    // end if calcerr
        }

        // temperature from average calculation is always done
        double profavg=profdat[eventRow.m_split].m_avg_prof;
        temp_ave = computeTemperature(results.m_avgbin,eventRow.m_split,par.gainitype,
                                      par.useevst,gaindat,profavg,par.extrap);
        AH_INFO(ahlog::LOW) << "Pixel " << eventRow.m_split << 
                               ", group size " << npoints << 
                               "  temp from average PHA = " << temp_ave << std::endl;

        // Calculate exposure by summing GTI gaps within range of first and
        // last events in group
        double firsttime=eventdat[eventRow.m_split].front().m_time;
        double lasttime=eventdat[eventRow.m_split][npoints-1].m_time;
        double gaptime=0.;
        int igti=0;
        for (igti=0; igti < (int)gtigaps[eventRow.m_split].size(); igti++) {
          if (gtigaps[eventRow.m_split][igti].first < firsttime) continue;                          // skip GTIs that occur before group (this should not happen, but just in case...)
          if (gtigaps[eventRow.m_split][igti].first > lasttime) break;                              // GTI is after group, so finished with loop
          gaptime+=(gtigaps[eventRow.m_split][igti].second-gtigaps[eventRow.m_split][igti].first);  // add gap time
        }
        results.m_exposure=results.m_telapse-gaptime;        // subtract GTI-off time to get EXPOSURE

        ahfits::writeRow(fpout);    // write contents of results variable (see connect() calls above)
        ahfits::nextRow(fpout);

        // Remove points from group
        if (emptyme) {     // clear all points in group
          eventdat[eventRow.m_split].clear();
          gtigaps[eventRow.m_split].clear();
        } else {           // number of points to remove based on grpoverlap parameter
          eventdat[eventRow.m_split].erase(eventdat[eventRow.m_split].begin(),eventdat[eventRow.m_split].begin()+nptremove);
          gtigaps[eventRow.m_split].erase(gtigaps[eventRow.m_split].begin(),gtigaps[eventRow.m_split].begin()+igti);   // igti is the index of the first GTI outside the event time range (calculated above)
        }
        gtigaptime[eventRow.m_split]=0.;   // reset gap time after group is written
      }

      // Initialize queue with current row, if necessary; this can only be
      // satisfied if given a GTI file.
      if (addevent && (emptyall || emptyme)) eventdat[eventRow.m_split].push_back(eventRow);
    }


  // Close current input file
  ahfits::close(fpin);
  fpin=0;

  }    // end loop over input files

  // Done reading all input files, process remaining events in all queues
  AH_INFO(ahlog::LOW) << std::endl;
  AH_INFO(ahlog::LOW) << "Finished reading file; process remaining non-empty queues" << std::endl;
  AH_INFO(ahlog::LOW) << std::endl;
  for (ahgain::EventVecMap::iterator mit=eventdat.begin(); mit != eventdat.end(); mit++) {
    // mit->first  = split column value
    // mit->second = vector of events (as EventDataVec) 
    if ((mit->second).size() < (unsigned int)par.minevent) continue;
    nused[mit->first]+= (int)(mit->second).size();
    ngroups[mit->first]++;
    AH_INFO(ahlog::LOW) << std::endl;
    AH_INFO(ahlog::LOW) << "Processing group: pixel=" << mit->first << ", npoints=" << (mit->second).size() << std::endl;
    int splitval=(mit->second)[0].m_split;
    int status=0;

    // Reset null flag
    nullcol[0] = 0;
    if (par.calcerr) ahgain::resetUncertainties(results);
    ahgain::fitEvents((mit->second),(mit->second).size(),profdat[splitval],
                      gwidth,par.fitwidth,par.background,par.avgwinrad,
                      par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                      par.maxdshift,par.bisectolshift,par.searchstepwidth,
                      par.minwidth,par.maxdwidth,par.bisectolwidth,results,status);

    if (status != 0) {     // fitting failed
      AH_INFO(ahlog::HIGH) << "Failure fitting group with splitval = " << splitval << std::endl;
      setNullRow(profdat[splitval], results, nullcol);
    } else {
      // Calculate temperature from fitted and average PHA
      ngoodfit[mit->first]++;
      double profavg=profdat[splitval].m_avg_prof;
      temp_fit = computeTemperature(results.m_avgfit,splitval,par.gainitype,
                                    par.useevst,gaindat,profavg,par.extrap);
      AH_INFO(ahlog::LOW) << "Pixel " << splitval << 
                             ", group size " << (mit->second).size() << 
                             ": temp from fitted PHA = " << temp_fit << std::endl;

      if (par.calcerr) {

        // shift uncertainties
        if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
          ahgain::computeLikelihoodShift(results);
          ahgain::computeChi2ShiftUncertainty(results);
        }

        // width uncertainty (only if fitting the width)
        if (par.fitwidth) {
          if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
            ahgain::computeLikelihoodWidth(results,profdat[splitval],gwidth);
            ahgain::computeChi2WidthUncertainty(results,profdat[splitval],gwidth);
          }
        }
      }    // end if calcerr
    }

    // temperature from average calculation is always done
    double profavg=profdat[splitval].m_avg_prof;
    temp_ave = computeTemperature(results.m_avgbin,splitval,par.gainitype,
                                  par.useevst,gaindat,profavg,par.extrap);
    AH_INFO(ahlog::LOW) << "Pixel " << splitval << 
                           ", group size " << (mit->second).size() << 
                           "  temp from average PHA = " << temp_ave << std::endl;

    // Calculate exposure by summing GTI gaps within range of first and
    // last events in group
    double firsttime=mit->second.front().m_time;
    double lasttime=mit->second.back().m_time;
    double gaptime=0.;
    int igti=0;
    for (igti=0; igti < (int)gtigaps[eventRow.m_split].size(); igti++) {
      if (gtigaps[eventRow.m_split][igti].first < firsttime) continue;                          // skip GTIs that occur before group (this should not happen, but just in case...)
      if (gtigaps[eventRow.m_split][igti].first > lasttime) break;                              // GTI is after group, so finished with loop
      gaptime+=(gtigaps[eventRow.m_split][igti].second-gtigaps[eventRow.m_split][igti].first);  // add gap time
    }
    results.m_exposure=results.m_telapse-gaptime;        // subtract GTI-off time to get EXPOSURE
    pnmesh = profdat[splitval].m_nmesh;

    ahfits::writeRow(fpout);    // write contents of results variable (see connect() calls above)
    ahfits::nextRow(fpout);
  }

  // Write keywords to output file
  ahfits::writeKeyValDbl(fpout,"TSTART",tstart,"Start time");
  ahfits::writeKeyValDbl(fpout,"TSTOP",tstop,"Stop time");
  
  // Report counts: nevent, nskip_*, ninrange, nvalid, nused
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "EVENT COUNTS: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  TOTAL NUMBER EVENTS READ:            " << nevent << std::endl;
  AH_INFO(ahlog::HIGH) << "  NUMBER WITH BAD PROC_STATUS:         " << nskip_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  NUMBER WITH TIME=NULL:               " << nskip_timenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  NUMBER WITH RISE_TIME > 127:         " << nskip_risetime << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "  COUNTS PER PIXEL" << std::endl;
  for (int iPixel=0; iPixel < ahsxs::NPIXEL; iPixel++) {
    AH_INFO(ahlog::HIGH) << "    PIXEL " << iPixel << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER OF EVENTS IN BIN RANGE : " << ninrange[iPixel] << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER OF VALID* EVENTS :       " << nvalid[iPixel] << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER IN GTI :                 " << ningti[iPixel] << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER OF EVENTS FITTED :       " << nused[iPixel] << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER OF FITTED GROUPS :       " << ngroups[iPixel] << std::endl;
    AH_INFO(ahlog::HIGH) << "        TOTAL NUMBER OF SUCCESSFUL FITS :     " << ngoodfit[iPixel] << std::endl;
  }
  AH_INFO(ahlog::HIGH) << "*Valid events are those that:" << std::endl;
  AH_INFO(ahlog::HIGH) << "  1. have the correct ITYPE" << std::endl;
  AH_INFO(ahlog::HIGH) << "  2. are consistent with calmethod (MXS or CAL-PIX)" << std::endl;
  AH_INFO(ahlog::HIGH) << "  3. are not coincident with an antico event (if ckant)" << std::endl;
  AH_INFO(ahlog::HIGH) << "  4. do not exhibit cross-talk (if ckctrec or ckctel)" << std::endl;

  // free memory
  if (pmesh != 0) {
    for(int ip=0; ip<ahsxs::NPIXEL; ++ip) { 
      if (pmesh[ip] != 0) delete [] pmesh[ip], pmesh[ip]=0;
    }
    delete [] pmesh, pmesh=0;
  }
  if (mesh != 0) delete [] mesh, mesh=0;
  if (emesh != 0) delete [] emesh, emesh=0;
  if (routin != 0) delete routin, routin=0;
  if (routout != 0) delete routout, routout=0;

}

// ****************************************************************************

void finalize(ahfits::FilePtr& fpout) {

  if (fpout != 0) ahfits::close(fpout), fpout=0;

}

// ****************************************************************************
/** @} */

/* Revision Log
 $Log: sxsgain.cxx,v $
 Revision 1.40  2016/10/13 16:04:09  mwitthoe
 sxsgain: use tabulated profile average instead of theoretical average when determining the temperature from the fitted shift

 Revision 1.39  2016/08/22 20:55:22  mwitthoe
 sxsgain: bug-fix - the event DATE-OBS keyword value was not being given to the CALDB query

 Revision 1.38  2016/08/10 16:29:57  mwitthoe
 sxsgain: now support pixel-dependent GTI files in two formats

 Revision 1.37  2016/06/01 21:36:02  mwitthoe
 sxsgain: add new parameter, pxphaoffset, to shift the PHA bin values

 Revision 1.36  2016/04/07 21:30:27  mwitthoe
 sxsgain: add spacer line in log file

 Revision 1.35  2016/04/05 20:27:27  mwitthoe
 sxsgain: change some INFO statements to DEBUG

 Revision 1.34  2016/03/24 20:35:53  mdutka
 Correcting error with ape_trad_set_string call for parameter linefitfile

 Revision 1.33  2016/03/24 13:17:03  mdutka
 Addressing item listed in issue #610

 Revision 1.32  2015/12/29 18:22:10  mwitthoe
 sxsgain: move where no-row check is being made

 Revision 1.31  2015/12/29 16:32:05  mwitthoe
 sxsgain: throw error if all input files contain zero rows

 Revision 1.30  2015/11/19 21:14:04  mwitthoe
 sxsgain: now tool will use energy vs temperature table by default when computing the group temperature, but the PHA vs temperature table can be used instead when tempidx < 0

 Revision 1.29  2015/11/17 21:49:57  mwitthoe
 sxsgain: add column/keyword comments

 Revision 1.28  2015/11/17 18:41:35  mwitthoe
 sxsgain: copy keywords from input file to both extensions of output drift file

 Revision 1.27  2015/11/17 16:30:59  mwitthoe
 sxsgain: 1) remove centerprof parameter (behavior now enabled when startenergy or stopenergy is specified); 2) make PHA bins centered on half-integers; 3) use PHA vs temperature table to calculate temperature (make ntemp parameter negative to use energy vs temperature table instead); 4) write same keywords to 1st and 2nd extensions

 Revision 1.26  2015/10/28 18:27:58  mwitthoe
 sxsgain: update tool after restructuring of ahgain library

 Revision 1.25  2015/10/27 19:11:23  mwitthoe
 sxsgain: move resetUncertainties() function to ahgain library

 Revision 1.24  2015/10/27 18:07:14  mwitthoe
 sxsgain: change names of parameters: nlikeshift, nlikewidth, likeshiftfac, likewidthfac to nerrshift, nerrwidth, shifterrfac, widtherrfac since these parameters are used for both the likelihood and Chi^2 uncertainty calculations

 Revision 1.23  2015/10/27 17:26:11  mwitthoe
 sxsgain: change name of writelike parameter to writeerrfunc; this parameter now causes the tool to write both the likelihood and Chi^2 functions

 Revision 1.22  2015/10/27 14:33:06  mwitthoe
 sxsgain: add Chi^2 uncertainties

 Revision 1.21  2015/10/16 20:33:04  mwitthoe
 sxsgain: 1) set default value of ckrisetime to yes; 2) increase MAXPT to 5000; 3) set default value of tempidx to 2; 4) new parameter extrap to allow for extrapolating the temperature

 Revision 1.20  2015/10/13 20:01:58  mwitthoe
 sxsgain: 1) option to skip events with RISE_TIME > 127; 2) allow calculation of uncertainties on fitted shift and width using the likelihood method

 Revision 1.19  2015/10/01 21:14:23  mwitthoe
 sxsgain: now can accept either a GTI file with a single, general GTI extension or a GTI file with 36, pixel-specific extensions

 Revision 1.18  2015/08/13 15:36:34  mwitthoe
 sxsgain: add parameter stamping to log file

 Revision 1.17  2015/08/12 14:18:38  klrutkow
 tool cleanup (issue 532): shortened prologue (issue 534), added logging

 Revision 1.16  2015/07/17 15:50:17  mdutka
 adding caldb query for linefit

 Revision 1.15  2015/07/15 20:58:46  mdutka
 Adding CALDB query

 Revision 1.14  2015/07/15 20:46:22  mdutka
 Adding CALDB query

 Revision 1.13  2015/06/03 21:50:09  asargent
 Updated parameter description for avgwinrad

 Revision 1.12  2015/06/03 18:36:22  asargent
 Removed extra clearing of groups when there is a fit failure

 Revision 1.11  2015/06/03 16:35:56  asargent
 Use null variable for ahfits routers rather than setting NAN directly. Add missing null row writing.

 Revision 1.10  2015/06/03 15:17:58  asargent
 Updated sxsgain to write NULL rows in Drift_energy extension in the case of a non-fitted profile

 Revision 1.9  2015/05/15 17:52:06  asargent
 Added more comments to initializeFitResults call, minimized results allocation

 Revision 1.8  2015/05/14 16:17:24  asargent
 Added new column BINMESH, decreased size of variable-length columns SPECTRUM and FITPROF to size of pixel mesh.

 Revision 1.7  2015/04/29 16:41:29  asargent
 Removed extra character in code

 Revision 1.6  2015/04/29 16:40:26  asargent
 Updated descriptions

 Revision 1.5  2015/04/27 19:49:31  asargent
 Added pixel descriptor to calibration profile printout

 Revision 1.4  2015/04/27 19:47:24  asargent
 Fixed array allocation for imesh, unallocated routin and added pixel value to mesh descriptor.

 Revision 1.3  2015/04/23 18:33:42  asargent
 Removed extra output during runtime error when checking avgwinrad parameter. Removed use of null variable when connecting to PHA column during event loop.

 Revision 1.2  2015/04/23 16:34:56  asargent
 Added use of functions applyGain and reverseLookup from ahsxs::engain library. Separated mesh profiles to be done per pixel.

 Revision 1.1  2015/04/14 17:26:01  asargent
 Initial version of sxsgain

*/
