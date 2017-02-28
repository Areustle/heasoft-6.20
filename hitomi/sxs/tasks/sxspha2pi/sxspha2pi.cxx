/// \file sxspha2pi.cxx
/// \brief Calculate 
/// \author Kristin Rutkowski
/// \date $Date: 2016/12/07 22:28:32 $
/// \version 1.0

/** 

\defgroup tool_sxspha2pi Calculate event PI (sxspha2pi)
@ingroup mod_sxs_tasks

The sxspha2pi tool calculates the final PI using the drift correction table 
constructed by the sxsgain tool.  This tool will store the floating point
and integral PI values in the EPI and PI columns, respectively.  The tool also 
calculates the uncorrected PI (UPI) from the PHA column using an energy scale 
derived from ground calibration and stored in a CALDB file. 

Source files:

  sxspha2pi.cxx
  sxspha2pilib.h
  sxspha2pilib.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  hitomi/gen/lib/ahapp
  hitomi/gen/lib/ahmath
  hitomi/mission/lib/ahmission

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-13   MCW    Clean-up code

*/
 

#define AHLABEL tool_sxspha2pi
#define AHCVSID "$Id: sxspha2pi.cxx,v 1.36 2016/12/07 22:28:32 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"


#include "sxspha2pilib.h"

#include "ahmath/ahmath.h"    // interpolation
#include "ahmission/ahmission.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"      // random numbers
#include "ahlog/ahlog.h"
#include "ahmission/caldb.h"  // caldb query

#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "headas.h"           // expand_item_list

#include <iostream>
#include <sstream>
#include <cmath>              // std::abs()
#include <algorithm>          // std::find()
#include <cstdio>             // std::remove()


/** \addtogroup tool_sxspha2pi
 *  @{
 */


/// \brief Get parameter values
/// \param[out] par           structure with parameter values
void getPar(ahsxs::Par & par);

/// \brief Copy contents of infile to outfile; resolve default values of columns
/// \param[in] par          struct with all user input parameters
/// \param[out] fp          FITS file pointer to output file
/// \param[out] gaindat     structure containing energy gain coefficients for each pixel and temperature
/// \param[out] tempdat     structure containing drift file information
/// \param[out] scaledat    structure containing EPI scale factors
/// \param[out] tlmin_pi    TLMIN for PI column
/// \param[out] tlmax_pi    TLMAX for PI column
void initialize(ahsxs::Par & par, 
                ahfits::FilePtr & fp, 
                ahsxs::engain::AllEnergyGainData & gaindat, 
                ahsxs::TempDataAllPixels & tempdat,
                ahsxs::EPIScaleFactors & scaledat,
                long & tlmin_pi, long & tlmax_pi);

/// \brief Fill in 2nd extension of output file with UPI, EPI, PI, etc
/// \param[in]  par          struct with all user input parameters
/// \param[in]  fp           FITS file pointer to output file
/// \param[in]  gaindat      structure containing energy gain coefficients for each pixel and temperature
/// \param[in]  tempdat      structure containing drift file information
/// \param[in]  scaledat     structure containing EPI scale factors
/// \param[in]  tlmin_pi     TLMIN for PI column
/// \param[in]  tlmax_pi     TLMAX for PI column
/// \param[out] clean        if yes, delete output files
void doWork(const ahsxs::Par & par, 
            ahfits::FilePtr fp, 
            ahsxs::engain::AllEnergyGainData & gaindat,
            const ahsxs::TempDataAllPixels & tempdat,
            const ahsxs::EPIScaleFactors & scaledat,
            long tlmin_pi, long tlmax_pi, bool& clean);

/// \brief close open FITS files
/// \param[in]  fp FITS file pointer to output file
/// \param[out] clean        if yes, delete output files
void finalize(ahfits::FilePtr fp, bool clean);


// ****************************************************************************


/// \brief sxspha2pi tool
///
/// 
int main(int argc, char** argv) {

  ahsxs::Par par;                            // structure with parameter values
  ahfits::FilePtr fpout=0;                   // FITS file pointer to output file
  ahsxs::engain::AllEnergyGainData gaindat;  // data from energy gain file
  ahsxs::TempDataAllPixels tempdat;          // data from drift/gain file
  ahsxs::EPIScaleFactors scaledat;           // data from EPI scale factor file
  long tlmin_pi = 0;
  long tlmax_pi = 0;
  bool clean=true;                           // delete output files (upon error)
  
  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      
      // Write all parameters to the log file.  When debug=yes, the params will 
      // be written twice: here, and again at the end of initialize.  That is 
      // to insure against a possible failure in initialize().
      ahapp::writeParametersToLog();
      
      initialize(par, fpout, gaindat, tempdat, scaledat, tlmin_pi, tlmax_pi);
      doWork(par, fpout, gaindat, tempdat, scaledat, tlmin_pi, tlmax_pi, clean);
      finalize(fpout, clean);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par, fpout, gaindat, tempdat, scaledat, tlmin_pi, tlmax_pi);
        doWork(par, fpout, gaindat, tempdat, scaledat, tlmin_pi, tlmax_pi, clean);
      } catch (const std::exception &x) {
        // Write all parameters to the log file.  This may be the second time 
        // they are written (the first was at the end of initialize()), just 
        // in case there was a failure in initialize() or doWork().
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout, clean);
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

  
} // end main()


// ****************************************************************************


void getPar(ahsxs::Par & par) {

  // Read parameters
  par.m_infile      = ahapp::getParString("infile");
  par.m_outfile     = ahapp::getParString("outfile");
  par.m_calcupi     = ahapp::getParBool("calcupi");
  par.m_calcpi      = ahapp::getParBool("calcpi");
  par.m_scaleepi    = ahapp::getParBool("scaleepi");
  if (par.m_calcpi) {
    par.m_driftfile   = ahapp::getParString("driftfile");
  }
  par.m_gainfile    = ahapp::getParString("gainfile");
  par.m_tempidx     = ahapp::getParInt("tempidx");
  par.m_pxphaoffset = ahapp::getParDouble("pxphaoffset");
  if (par.m_calcpi) {
    par.m_secphacol   = ahapp::getParString("secphacol");
    par.m_addepicol   = ahapp::getParString("addepicol");
    par.m_method      = ahapp::getParString("method");
  }
  par.m_itypecol    = ahapp::getParString("itypecol");
  par.m_extended    = ahapp::getParBool("extended");
  if (par.m_calcpi) {
    par.m_binwidth    = ahapp::getParDouble("binwidth");
    par.m_offset      = ahapp::getParDouble("offset");
    par.m_tlmax       = ahapp::getParInt("tlmax");
    par.m_gapdt       = ahapp::getParDouble("gapdt");
    par.m_ntemp       = ahapp::getParInt("ntemp");
    par.m_writetemp   = ahapp::getParBool("writetemp");
    par.m_extrap      = ahapp::getParBool("extrap");
  }
  if (par.m_scaleepi) {
    par.m_scalefile   = ahapp::getParString("scalefile");
    par.m_scalegrade  = ahapp::getParString("scalegrade");
  }
  par.m_randomize   = ahapp::getParBool("randomize");
  par.m_seed        = ahapp::getParInt("seed");

  if (!par.m_calcupi && ! par.m_calcpi) 
    AH_THROW_RUNTIME("calcupi and calcpi both false; nothing to do!");

  // convert scalegrade parameter to list of grades
  if (par.m_scaleepi) {
    par.m_gradeset.clear();

    // get comma-delimited values from scalegrade parameter
    int status=0;
    int trim=1;        // trim spaces
    int skip=1;        // exclude empty items
    int guard=0;       // do not protect against commas in parentheses
    char* in=(char*)par.m_scalegrade.c_str();
    char** items=0;
    int nitems=0;
    items=expand_item_list(in,&nitems,',',trim,skip,guard,&status);
    if (0 != status) {
      free(items);
      std::stringstream msg;
      msg << "expand_item_list failed with status " << status;
      AH_DEBUG << msg.str() << std::endl;
      AH_THROW_RUNTIME("Could not parse scalegrade parameter; should be comma-delimited list of integers");
    }

    // put list of grades into structure
    for (int i=0; i < nitems; i++) {
      if (!ahgen::isNumber(items[i])) {
        std::stringstream msg; 
        msg << "Invalid grade: " << items[i] << "; expecting an integer";
        free(items);
        AH_THROW_RUNTIME(msg.str());
      }
      int grade=atoi(items[i]);
      if (grade < 0 || grade > 7) {
        std::stringstream msg;
        msg << "Invalid grade: " << grade << "; expecting number in range 0 to 7";
        free(items);
        AH_THROW_RUNTIME(msg.str());
      }
      par.m_gradeset.insert(grade);
    }
    free(items);
  }

} // end getPar()


// ****************************************************************************


void initialize(ahsxs::Par & par, 
                ahfits::FilePtr & fp, 
                ahsxs::engain::AllEnergyGainData & gaindat, 
                ahsxs::TempDataAllPixels & tempdat,
                ahsxs::EPIScaleFactors & scaledat,
                long & tlmin_pi, long & tlmax_pi) {
  
  //paramenters needed for CALDB query
  std::string gainfile;
  std::string filetype_gain = "gain_coefficients";
  std::string scalefile;
  std::string filetype_scale= "Cal-pix EPI scale factors";
  std::string instrume;
  std::string detnam = "-";
  std::string codename_gain = "GAINPIX";
  std::string codename_scale = "PIX12GAINCOR";
  std::string datetime;

  // copy contents of infile to outfile and return opened output file (fp);
  // if editing input file in-place (allowed with last argument = true), 
  // fp will point to the opened input file.
  ahfits::clone(par.m_infile,par.m_outfile,&fp,true);
  if (ahfits::isPrimary(fp)) ahfits::move(fp,"EVENTS");   // move to proper extension in file if extended syntax not used
  ahmission::checkEmptyTable(fp,par.m_infile);

  // check that is SXS event of SXS-specific HK file
  instrume = ahfits::getKeyValStr(fp,"INSTRUME");
  datetime = ahfits::getKeyValStr(fp,"DATE-OBS");
  if ("SXS" != ahgen::strtoupper(instrume))
    AH_THROW_RUNTIME("input file is not SXS event or HK file");
  if ("PIXEL" != ahgen::strtoupper(ahfits::getKeyValStr(fp,"DETNAM")))
    AH_THROW_RUNTIME("input file is not SXS PIXEL file");

  // Read TLMIN/TLMAX for PI column.
  // The UPI column will be sent to NULL if the PI is out-of-range.
  long pi_null=0;
  std::string outcol;
  if (par.m_calcpi) {
    if (par.m_extended) {
      AH_INFO(ahlog::HIGH) << "Using extended energy range" << std::endl;
      if (par.m_tlmax <= 0) AH_THROW_RUNTIME("tlmax parameter must be positive");
      if (par.m_binwidth <= 0.) AH_THROW_RUNTIME("binwidth parameter must be positive");
      tlmax_pi=par.m_tlmax;
      tlmin_pi=-par.m_tlmax;
      pi_null=-par.m_tlmax-1;
      outcol="PIE";
    } else {
      std::string keyname;
      ahfits::formColumnAttribute(fp,"PI","TLMIN",keyname);
      tlmin_pi=ahfits::getKeyValLLong(fp,keyname);
      ahfits::formColumnAttribute(fp,"PI","TLMAX",keyname);
      tlmax_pi=ahfits::getKeyValLLong(fp,keyname);
      ahfits::formColumnAttribute(fp,"PI","TNULL",keyname);
      pi_null = ahfits::getKeyValLLong(fp,keyname);
      outcol="PI";
    }
    AH_INFO(ahlog::HIGH) << "Output channel limits (" << outcol << "): " << std::endl;
    AH_INFO(ahlog::HIGH) << "  TLMIN: " << tlmin_pi << std::endl;
    AH_INFO(ahlog::HIGH) << "  TLMAX: " << tlmax_pi << std::endl;
    AH_INFO(ahlog::HIGH) << "  TNULL: " << pi_null << std::endl;
  }

  // check that PHA column for secondary calculation is present
  if (!ahfits::haveColumn(fp,par.m_secphacol)) {
    std::stringstream msg;
    msg << "input file missing column given by secphacol parameter: " << par.m_secphacol;
    AH_THROW_RUNTIME(msg.str());
  }

  // may need to insert UPIE, EPIE, PIE columns if in extended mode
  if (par.m_extended) {
    if (par.m_calcupi && !ahfits::haveColumn(fp,"UPIE")) {
      ahfits::insertColAfter(fp,"UPIE","D","");
      ahfits::setColumnDescription(fp,"UPIE","UPI in extended energy range");
      AH_INFO(ahlog::HIGH) << "Inserting UPIE column" << std::endl;
    }
    if (par.m_calcpi && !ahfits::haveColumn(fp,"EPIE")) {
      ahfits::insertColAfter(fp,"EPIE","D","");
      ahfits::setColumnDescription(fp,"EPIE","EPI in extended energy range");
      AH_INFO(ahlog::HIGH) << "Inserting EPIE column" << std::endl;
    }
    if (par.m_calcpi && !ahfits::haveColumn(fp,"PIE")) {
      ahfits::insertColAfter(fp,"PIE","J","");
      ahfits::setTLmin(fp,"PIE",tlmin_pi);
      ahfits::setTLmax(fp,"PIE",tlmax_pi);
      ahfits::setTNull(fp,"PIE",pi_null);
      ahfits::setColumnDescription(fp,"PIE","PI in extended energy range");
      AH_INFO(ahlog::HIGH) << "Inserting PIE column" << std::endl;
    }

    // write keywords giving offset and width
    ahfits::writeKeyValDbl(fp,"PIEOFFST",par.m_offset,"Energy offset (eV) of extended energy mode");
    ahfits::writeKeyValDbl(fp,"PIEWIDTH",par.m_binwidth,"Channel width (eV) of extended energy mode");
  } else {
    // write keywords giving offset and width
    ahfits::writeKeyValDbl(fp,"PIOFFST",0.5,"Energy offset of PI channel (eV)");
    ahfits::writeKeyValDbl(fp,"PIWIDTH",0.5,"PI channel width (eV)");
  }
  
  // write EPI2 column (user may have chosen a different col name in addepicol)
  par.m_epi2colname=par.m_addepicol;
  if (par.m_extended) par.m_epi2colname=par.m_addepicol+'E';   // add an E to the column name in extended mode
  if (par.m_calcpi && !ahfits::haveColumn(fp,par.m_epi2colname)) {
    ahfits::insertColAfter(fp,par.m_epi2colname,"D","");
    ahfits::setColumnDescription(fp,par.m_epi2colname,"As EPI but Ms/Ls with secondary correction");
  }

  // write TEMP column containing lookup temperature
  if (par.m_calcpi && par.m_writetemp) {
    if (!ahfits::haveColumn(fp,"TEMP")) {
      AH_INFO(ahlog::HIGH) << "Inserting TEMP column" << std::endl;
      ahfits::insertColAfter(fp,"TEMP","D","");
      ahfits::setColumnDescription(fp,"TEMP","gain temperature (K)");
      ahfits::setTUnit(fp,"TEMP","K");
    }
  }

  //obtain gain file path from parameter or CALDB
  gainfile = ahmission::caldb::resolve(par.m_gainfile,filetype_gain,instrume,
                                       detnam,codename_gain,datetime); 
  ape_trad_set_string("gainfile",gainfile.c_str());   // to record actual file path in history
    
  // read gain coefficients from CALDB file
  ahsxs::engain::loadEnergyGainData(gainfile, gaindat);
  AH_INFO(ahlog::HIGH) << "Number of temperatures loaded from gain file: " << gaindat.m_numTemps << std::endl;
  
  // ntemp parameter cannot be larger than number of rows in the gain file
  if (par.m_calcpi && gaindat.m_numTemps < par.m_ntemp) {
    par.m_ntemp=gaindat.m_numTemps;
    AH_INFO(ahlog::HIGH) << "Fewer temperatures in gain file (" << gaindat.m_numTemps 
                         << ") than requested (" << par.m_ntemp 
                         << "); only using temperatures found" << std::endl;
  }

  // Load driftfile into tempdat
  if (par.m_calcpi) {
    ahsxs::loadTempGainData(par.m_driftfile, par.m_method, tempdat);
    AH_INFO(ahlog::HIGH) << "Loaded driftfile from sxsgain" << std::endl;
    AH_INFO(ahlog::HIGH) << "  Calibration type (CALTYPE):  " << tempdat.m_caltype << std::endl;
  }

  // Check if need to use a different gain temperature index
  if (par.m_calcupi) {
    int tempidx=par.m_tempidx;
    if (tempidx < 0 && par.m_calcpi) tempidx=tempdat.m_tempidx;
    AH_INFO(ahlog::HIGH) << "  UPI Temperature index (TEMPIDX): " << tempidx << std::endl;
    if (tempidx < 1) {
      std::stringstream msg;
      msg << "invalid gain temperature index (" << tempidx << "); must be greater than zero";
      AH_THROW_RUNTIME(msg.str());
    }
    if (tempidx > gaindat.m_numTemps) {
      std::stringstream msg;
      msg << "invalid gain temperature index (" << tempidx << "); must be smaller than the number of gain temperatures (" << gaindat.m_numTemps << ")";
      AH_THROW_RUNTIME(msg.str());
    }
    tempdat.m_tempidx=tempidx;
  }

  // Can only apply EPI scale factors if calibration type from drift file is cal-pix
  if (par.m_scaleepi && tempdat.m_caltype != "CAL-PIX") {
    AH_INFO(ahlog::HIGH) << "Resetting scaleepi=no since input driftfile not from calibration pixel (CAL-PIX)" << std::endl;
    ape_trad_set_bool("scaleepi",0);         // to record new value in history
    par.m_scaleepi=false;
  }

  // obtain EPI scale factor file from CALDB (if necessary)
  if (par.m_scaleepi) {
    scalefile = ahmission::caldb::resolve(par.m_scalefile,filetype_scale,instrume,
                                         detnam,codename_scale,datetime); 
    ape_trad_set_string("scalefile",scalefile.c_str());   // to record actual file path in history
  }

  // load EPI scale factor data file (if necessary)
  if (par.m_scaleepi) {
    ahsxs::loadEPIScaleFactors(scalefile,scaledat);
    AH_INFO(ahlog::HIGH) << "Loaded scalefile: " << par.m_scalefile << std::endl;
  }

  // seed random number generator used to convert integral PHA to float
  if (par.m_randomize) ahgen::seedRandom(par.m_seed);

  // Write all parameters to the log file
  ahapp::writeParametersToLog();

} // end initialize()


// ****************************************************************************


void doWork(const ahsxs::Par & par, 
            ahfits::FilePtr fp, 
            ahsxs::engain::AllEnergyGainData & gaindat,
            const ahsxs::TempDataAllPixels & tempdat,
            const ahsxs::EPIScaleFactors & scaledat,
            long tlmin_pi, long tlmax_pi, bool& clean) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------

  // input:
  double l_time=0.;                   // column: TIME
  char l_timenull=0;                  // NULL flag for TIME column
  int l_pixel=0;                      // column: PIXEL (valid range: 0-35)
  char l_pixel_null=0;                // NULL flag for PIXEL column
  int l_itype=0;                      // column: ITYPE: 0=Hp, 1=Mp, 2=Ms, 3=Lp, 4=Ls, 5=BL, 6=LO, 7=Rj
  long l_pha=0;                       // column: PHA
  char l_phanull=0;                   // NULL flag for PHA column
  long l_pha2=0;                      // column: secphacol
  char l_pha2null=0;                  // NULL flag for secphacol column
  char l_proc_status[32]={0};         // column: PROC_STATUS (1st bit=1 => BAD)
  ahfits::IndexType num_proc_status;  // number of PROC_STATUS bits found

  // output:
  double l_upi=0.;                    // column: UPI or UPIE
  char l_upinull=0;                   // set to 1 if output UPI column should be NULL
  double l_epi=0.;                    // column: EPI or EPIE
  char l_epinull=0;                   // set to 1 if output EPI column should be NULL
  long l_pi=0;                        // column: PI or PIE
  char l_pinull=0;                    // set to 1 if output PI column should be NULL
  double l_epi2=0.;                   // column: EPI2 (column name is actually derived from par.addepicol)
  char l_epi2null=0;                  // set to 1 if output EPI2 column should be NULL
  double l_temp=0.;                   // temperature from gain lookup
  char l_tempnull=0;                  // set to 1 if output temperature should be NULL

  double pha_random=0.;               // store double version of PHA
  double pha2_random=0.;              // store double version of PHA2
  int dpixel=0;                       // PIXEL number to use in drift look-up
  const int calpix = 12;              // Cal-pix only has pixel=12 drift
  ahsxs::engain::EnergyGainData currdat = gaindat.m_engainVec.at(tempdat.m_tempidx-1);

  // counters
  long long nevents=0;                // number of events read
  long long nprocstatus=0;            // number skipped due to bad PROC_STATUS
  long long nbadpixel=0;              // number skipped b/c PIXEL not found in drift file
  long long ntimenull=0;              // number skipped b/c TIME=NULL
  long long nphanull=0;               // number skipped b/c PHA=NULL
  long long npha2null=0;              // number with PHA2=NULL
  long long ngap=0;                   // number in a TIME gap of the drift file
  long long nokay=0;                  // number with PI computed
  long long nbaselimit=0;             // number of baseline events with PI out-of-range (set to NULL)
  long long npixlow=0;                // number of pixel events with PI < TLMIN (set to NULL)
  long long npixhigh=0;               // number of pixel events with PI > TLMAX (set to TLMAX)
  long long npixnull=0;               // number of pixel events skipped b/c PIXEL=NULL

  // current index in drift file for each pixel
  std::vector<int> driftindex(ahsxs::NPIXEL);

  // -------------------------------------

  // connect local variables
  ahfits::Router router(fp);

  // input columns:
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time,&l_timenull);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel,&l_pixel_null);
  router.connectScalar(ahfits::e_READONLY,par.m_itypecol,l_itype);
  router.connectScalar(ahfits::e_READONLY,"PHA",l_pha,&l_phanull);
  if (par.m_secphacol != "PHA") router.connectScalar(ahfits::e_READONLY,par.m_secphacol,l_pha2,&l_pha2null);
  router.connectBit(ahfits::e_READWRITE,"PROC_STATUS",l_proc_status,num_proc_status);

  // output columns:
  // connect the upi, epi, pi variables to the appropriate column, depending 
  // on if we're in extended mode or not
  if (par.m_extended) {
    if (par.m_calcupi) router.connectScalar(ahfits::e_WRITEONLY,"UPIE",l_upi,&l_upinull);
    if (par.m_calcpi) router.connectScalar(ahfits::e_WRITEONLY,"EPIE",l_epi,&l_epinull);
    if (par.m_calcpi) router.connectScalar(ahfits::e_WRITEONLY,"PIE",l_pi,&l_pinull);
  } else {
    if (par.m_calcupi) router.connectScalar(ahfits::e_WRITEONLY,"UPI",l_upi,&l_upinull);
    if (par.m_calcpi) router.connectScalar(ahfits::e_WRITEONLY,"EPI",l_epi,&l_epinull);
    if (par.m_calcpi) router.connectScalar(ahfits::e_WRITEONLY,"PI",l_pi,&l_pinull);
  }
  if (par.m_calcpi) router.connectScalar(ahfits::e_WRITEONLY,par.m_epi2colname,l_epi2,&l_epi2null);
  if (par.m_calcpi && par.m_writetemp) router.connectScalar(ahfits::e_WRITEONLY,"TEMP",l_temp,&l_tempnull);

  // start event reading 
  clean=false;      // at this point the output files are not deleted if there is an error
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp) ) {
    ahfits::readRow(fp);
    int currRow = ahfits::currentRow(fp);
    nevents++;

    // if secphacol == PHA, then no secondary correction performed
    if (par.m_secphacol == "PHA") l_pha2=l_pha;

    // reset all values and nulls to 0
    l_upi = 0.0;
    l_upinull = 0;
    l_epi = 0.0;
    l_epinull = 0;
    l_pi = 0.0;
    l_pinull = 0;
    l_epi2 = 0.0;
    l_epi2null = 0;
    l_temp=0.;
    l_tempnull = 0;

    // if drift corrections were calculated from Cal-pix, only
    // use corrections from PIXEL=12
    dpixel=l_pixel;
    if (ahgen::strtoupper(tempdat.m_caltype) == "CAL-PIX") {
      dpixel=calpix;
    }

    // check for a bad procstatus
    if (!procstatus::processRow(l_proc_status)) {
      AH_DEBUG << "Row: " << currRow << ": bad PROC_STATUS; set output to NULL" << std::endl;
      ahsxs::setValuesToNull(l_upinull, l_epinull, l_pinull, l_epi2null, l_tempnull);
      ahfits::writeRow(fp);
      nprocstatus++;
      continue;
    } 

    // check for PIXEL=NULL
    // Normally, this condition should never be satisfied because PROC_STATUS
    // should be bad if PIXEL=NULL.  It is here as protection against a
    // segmentation fault.
    if (l_pixel_null == 1) {
      AH_DEBUG << "Row: " << currRow << ": PIXEL=NULL; set output to NULL" << std::endl;
      ahsxs::setValuesToNull(l_upinull, l_epinull, l_pinull, l_epi2null, l_tempnull);
      ahfits::writeRow(fp);
      npixnull++;
      continue;
    }

    // check that this pixel was in the drift file
    if (par.m_calcpi) {
      if (tempdat.m_pixels.find(dpixel) == tempdat.m_pixels.end()) {
        AH_DEBUG << "Row " << currRow 
                 << ": no data for pixel " << dpixel 
                 << " in input drift file; setting output to NULL" << std::endl;
        char upidummy=0;    // do not want to set UPI to NULL 
        ahsxs::setValuesToNull(upidummy, l_epinull, l_pinull, l_epi2null, l_tempnull);
        ahfits::writeRow(fp);
        nbadpixel++;
        continue;
      }
    }

    // check that TIME isn't NULL
    if (l_timenull == 1) {
      AH_DEBUG << "Row " << currRow << ": TIME is NULL; cannot "
               << "retrieve temperature, so setting output to NULL" << std::endl;
      ahsxs::setValuesToNull(l_upinull, l_epinull, l_pinull, l_epi2null, l_tempnull);
      ahfits::writeRow(fp);
      ntimenull++;
      continue;
    }

    // check if PHA is NULL
    if (l_phanull == 1) {
      AH_DEBUG << "Row " << currRow << ": PHA is NULL; setting output to NULL" << std::endl;
      ahsxs::setValuesToNull(l_upinull, l_epinull, l_pinull, l_epi2null, l_tempnull);
      ahfits::writeRow(fp);
      nphanull++;
      continue;
    }

    // add a random number to the PHA before applying the gain.  This is to 
    // avoid unphysical discretization of the floating-point EPI values and, 
    // consequently, an unphysical distribution of the final, integer PI values
    double ran = par.m_pxphaoffset;
    if (par.m_randomize) ran += ahgen::getRandom()-0.5;   // number in range [-0.5:+0.5]
    pha_random = (double)l_pha+ran;
    if (l_pha2null == 0) pha2_random = (double)l_pha2+ran;

    // Use Hp gain coefficients for baseline, lost, and rejected events
    int temp_itype = ((l_itype > 4) ? 0 : l_itype);

    // Calculate UPI using gain curve with same temperatures as used in sxsgain
    if (par.m_calcupi) {
      ahsxs::engain::applyGain(pha_random,l_pixel,temp_itype,currdat,l_upi,l_upinull);
    }

    if (par.m_calcpi) {

      // Search the driftfile to locate a row with the largest TIME less than the
      // current event TIME.  If the event TIME is smaller than first drift time,
      // use first drift temperature.  If the event time is larger than last drift
      // time, use last drift temperature.
      while (l_time > tempdat.m_tempdatpix[dpixel].m_time[driftindex[dpixel]]) {
        driftindex[dpixel]++;
        if ( driftindex[dpixel] >= (tempdat.m_tempdatpix.at(dpixel).m_size-1) ) break;
      } // end-while
      if (driftindex[dpixel] > 0) driftindex[dpixel]--;     // position temperature to just before event in time

      // use linear interpolation to get temperature from two nearest points 
      // in drift file; unless before first or after last drift point
      double time1=tempdat.m_tempdatpix[dpixel].m_time[driftindex[dpixel]];
      double temp1=tempdat.m_tempdatpix[dpixel].m_temp[driftindex[dpixel]];
      double time2=tempdat.m_tempdatpix[dpixel].m_time[driftindex[dpixel]+1];
      double temp2=tempdat.m_tempdatpix[dpixel].m_temp[driftindex[dpixel]+1];
      if (!par.m_extrap && (l_time < time1)) {            // event before 1st drift row
        l_temp=temp1;
      } else if (!par.m_extrap && (l_time > time2)) {     // event after last drift row
        l_temp=temp2;
      } else {
        l_temp=ahmath::interpolate_point_twopoint(l_time,time1,temp1,time2,temp2);
      }

      // if the user specified a time gap, check it now
      if (par.m_gapdt > 0) {
        // if the time between the event and the closest row in the drift file
        // row exceeds the gapdt parameter, then set current row to NULL
        double timeDiff = std::min(std::abs(l_time-time1),std::abs(time2-l_time));
        if (timeDiff > par.m_gapdt) {
          AH_DEBUG << "Row " << currRow 
                   << ": no gain entry before event within gapdt; skipping" << std::endl;
          char upidummy=0;    // do not want to set UPI to NULL 
          ahsxs::setValuesToNull(upidummy, l_epinull, l_pinull, l_epi2null, l_tempnull);
          ahfits::writeRow(fp);
          ngap++;
          continue;
        }
      }

      // compute EPI values from PHA and secphacol
      ahsxs::computeEPI(par,pha_random, l_pixel, l_itype, l_temp, temp_itype, gaindat, scaledat, l_epi);
      if (l_pha2null == 0) {
        ahsxs::computeEPI(par,pha2_random, l_pixel, l_itype, l_temp, temp_itype, gaindat, scaledat, l_epi2);
      }

      // compute PI if PHA2 is not NULL
      if (l_pha2null == 0) {

        nokay++;
        if (par.m_extended) {
          l_pi = std::floor( (l_epi2-par.m_offset)/par.m_binwidth + 1.0);
        } else {
          l_pi = std::floor( (l_epi2-0.5)/0.5 + 1.0);
        }
    
        // check if PI is out of range
        if (l_itype == 5) {   // baseline
          if ( (l_pi < tlmin_pi) || (l_pi > tlmax_pi) ) {
            nbaselimit++;
            l_pinull = 1;
          }
        } else {              // not baseline, e.g. PIXEL (or lost/rejected)
          if (l_pi < 0) {
            l_pinull=1;
            npixlow++;
          } else if (l_pi > tlmax_pi) {
            l_pi=tlmax_pi;
            npixhigh++;
          }
        }

      } else {    // if PHA2 is NULL, then EPI2 and PI are NULL
        npha2null++;
        l_epi2null=1;
        l_pinull=1;
      }

    }     // end if (par.m_calcpi)

    // Write UPI/UPIE, EPI/EPIE, PI/PIE, and EPI2 to output file
    ahfits::writeRow(fp);

  } // end-loop through events (rows)

  // record how secondaries processed in output FITS header
  if (par.m_calcpi) {
    if ("PHA" == par.m_secphacol) {
      ahfits::writeKeyValLLong(fp, "SXSPISEC", 0, "Secondary (added by software )");
    } else {
      ahfits::writeKeyValLLong(fp, "SXSPISEC", 1, "Secondary (added by software )");
    }
  }

  // Report counters
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Counters: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of events read:                         " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c bad PROC_STATUS:            " << nprocstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c PIXEL=NULL:                 " << npixnull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c PIXEL not in drift file:    " << nbadpixel << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c TIME=NULL:                  " << ntimenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c PHA=NULL:                   " << nphanull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c TIME gap of drift file:     " << ngap << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped b/c secphacol=NULL:             " << npha2null << std::endl;
  AH_INFO(ahlog::HIGH) << std::endl;
  if (par.m_calcpi) {
    AH_INFO(ahlog::HIGH) << "  Number with PI computed:                       " << nokay << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of baseline with PI out-of-range*:      " << nbaselimit << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of pixel events with PI < TLMIN*:       " << npixlow << std::endl;
    AH_INFO(ahlog::HIGH) << "  Number of pixel events with PI > TLMAX**:      " << npixhigh << std::endl;
    AH_INFO(ahlog::HIGH) << " *PI set to NULL" << std::endl;
    AH_INFO(ahlog::HIGH) << "**PI set to TLMAX" << std::endl;
    AH_INFO(ahlog::HIGH) << std::endl;
  }

} // end doWork()


// ****************************************************************************


void finalize(ahfits::FilePtr fp, bool clean) {

  std::string filename;
  if (0 != fp) filename=fp->m_filename;
  ahfits::close(fp);
  if (clean && "" != filename && ahgen::fileExists(filename)) std::remove(filename.c_str());

} // end finalize()


// ****************************************************************************


/** @} */


/* Revision Log
 $Log: sxspha2pi.cxx,v $
 Revision 1.36  2016/12/07 22:28:32  mwitthoe
 sxspha2pi: skip events with PIXEL=NULL

 Revision 1.35  2016/11/03 15:51:38  mwitthoe
 sxspha2pi: fix bug affecting only extended energy mode where the tool failed if the EPI2E column already existed in the event file

 Revision 1.34  2016/10/17 14:54:00  mwitthoe
 sxspha2pi: write PIOFFST/PIWIDTH keywords in non-extended mode (PIEOFFST/PIEWIDTH keywords were already being written in extended mode)

 Revision 1.33  2016/10/13 14:58:08  mwitthoe
 sxspha2pi: when in extended energy mode (extended parameter), now add an 'E' to the end of the column name given by the addepicol parameter

 Revision 1.32  2016/06/02 15:13:08  mwitthoe
 sxspha2pi: 1) add parameter, pxphaoffset, which is added to each event PHA along with a random number between -0.5 and +0.5 before the gain is applied; 2) now support a 2nd format for the scalefile which has columns for all three grades instead of just Hp

 Revision 1.31  2016/04/25 19:10:44  mwitthoe
 sxspha2pi: change the CALDB code name for the scalefile from SCALEEPI to PIX12GAINCOR

 Revision 1.30  2016/04/22 14:20:23  mwitthoe
 sxspha2pi: 1) check if TEMP column exists in output file before trying to insert it; 2) write TUNIT for TEMP column; 3) add CVS log to lib source file

 Revision 1.29  2016/04/20 21:31:02  mwitthoe
 sxspha2pi: 1) add option to compute EPI2 using PHA2 (secondary correction); 2) if driftfile is cal-pix, add option to scale EPI values by pixel-dependent factors stored in a CALDB file

 Revision 1.28  2016/04/07 21:09:40  mwitthoe
 sxspha2pi: change INFO statements to DEBUG in row loop

 Revision 1.27  2016/03/22 18:37:33  klrutkow
 changed library path to hitomi

 Revision 1.26  2016/03/22 18:27:49  klrutkow
 per issue 610: added writeParametersToLog to two other spots, added ape_trad_set_string call, made string comparisons case-insensitive

 Revision 1.25  2016/03/07 17:11:05  mwitthoe
 sxspha2pi: when inverting the fitting quadratic to get EPI from PHA, use the negative-sign solution for events with PHA < 0; this allows the distribution of baseline EPI values about zero to be symmetric

 Revision 1.24  2015/12/29 16:37:18  mwitthoe
 sxspha2pi: throw error if no rows in either the input event file or the input drift file

 Revision 1.23  2015/11/20 17:58:03  mwitthoe
 sxspha2pi: write PIEOFFST and PIEWIDTH keywords when in extended energy move; add extrap parameter to allow extrapolation when determining drift temperature

 Revision 1.22  2015/11/19 23:21:38  mwitthoe
 sxspha2pi: fix array out-of-bounds bug in drift interpolation

 Revision 1.21  2015/11/19 22:45:11  mwitthoe
 sxspha2pi: do not extrapolate to get drift, instead use the first or last drift temperature

 Revision 1.20  2015/11/19 21:36:16  mwitthoe
 sxspha2pi: interpolate drift temperature over event time; instead of interpolating energy vs temperature table to get energy, invert the quadratic from the interpolation of this table to get temperature (used in sxsgain)

 Revision 1.19  2015/11/17 22:06:29  mwitthoe
 sxspha2pi: add column/keyword comments

 Revision 1.18  2015/11/13 21:14:34  mwitthoe
 sxspha2pi: 1) add parameter, writetemp, to output TEMP column with drift temperature for each event; 2) fix drift file lookup - old version was retrieving the first row after the event time instead of the row immediately before the event; 3) skip reading the INDEX and GROUPS columns if the allprimary parameter is yes (these columns are only to be used in the PI correction for secondary events

 Revision 1.17  2015/10/16 20:24:20  mwitthoe
 sxspha2pi bug-fix: change tempidx from zero-based to one-based to match sxsgain

 Revision 1.16  2015/09/18 20:13:49  mwitthoe
 sxssecid & sxspha2pi: be more careful in removing files in finalize upon an error; now the file is checked for existence before trying to delete it

 Revision 1.15  2015/09/18 17:35:15  mwitthoe
 sxspha2pi: stop tool if both calcupi and calcpi are false

 Revision 1.14  2015/09/17 18:32:16  mwitthoe
 sxspha2pi: 1) add parameters, calcupi and calcpi, specifying which calculations to perform; 2) add parameter tempidx giving the gain temperature index to use in the UPI calculation (or -1 to use the index in the drift file); 3) add itypecol parameter giving the ITYPE column to use with the event grade information; 4) delete output file if there is an early error

 Revision 1.13  2015/08/19 21:47:26  mwitthoe
 sxspha2pi: fix Doxygen labels

 Revision 1.12  2015/08/17 18:09:46  klrutkow
 removed unused driftfile variable, would have been used for CALDB query

 Revision 1.11  2015/08/13 15:29:10  mwitthoe
 sxspha2pi: move all structs and non-standard functions from sxspha2pi to sxspha2pilib, add standard prologue, implement extended energy mode, general clean-up; see issues 532 & 534

 Revision 1.10  2015/07/20 17:01:49  mwitthoe
 sxspha2pi bug-fix: in Cal-pix mode, all events were using the pixel 12 gain coefficients

 Revision 1.9  2015/07/15 20:58:47  mdutka
 Adding CALDB query

 Revision 1.8  2015/07/15 19:25:00  mdutka
 Adding CALDB query

 Revision 1.7  2015/05/13 19:54:34  klrutkow
 updated variable names for tlmin/max for PI (not PHA)

 Revision 1.6  2015/05/13 16:40:26  klrutkow
 updated doxygen

 Revision 1.5  2015/05/01 14:47:33  klrutkow
 updated to write params to log file (issue 485)

 Revision 1.4  2015/04/30 01:09:35  klrutkow
 updated doxygen

 Revision 1.3  2015/04/29 22:55:16  klrutkow
 write to columns for extended mode; added check for gapdt; check TLMIN for PI intsead of PHA; remove check for PHA TNULL; change local pi variable to a long

 Revision 1.2  2015/04/21 19:51:30  klrutkow
 updated use of itype in call to applyGain, if itype gt 4

 Revision 1.1  2015/04/21 19:24:26  klrutkow
 new tool sxspha2pi


*/

