/// \file ahgainfit.cxx
/// \brief Calculate gain correction table.
/// \author Mike Witthoeft
/// \date $Date: 2016/04/15 22:34:15 $
/// \version 1.0

/** 

\defgroup tool_ahgainfit Calculate gain correction factors (ahgainfit)
@ingroup mod_mission_tasks

The ahgainfit tool calculates the time-dependent gain corrections based on a 
single calibration feature.  The corrections are determined via a fitting
procedure where an energy shift, convolution width, scale, factor, and 
background are applied to the theoretical calibration feature profile to get
the best agreement with the binned event spectrum.  

Source files:

  ahgainfit.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/mission/lib/ahgain

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-11   MCW    Clean-up code

*/
 
#define AHLABEL tool_ahgainfit
#define AHCVSID "$Id: ahgainfit.cxx,v 1.18 2016/04/15 22:34:15 rshill Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahgain/callines.h"           // calibration CALDB file access
#include "ahgain/ahgain.h"             // gain fitting routines
#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahmission/keyword.h"
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

/** \addtogroup tool_ahgainfit
 *  @{
 */

/// \brief minimum number of bin points
const int MINPT=5;

/// \brief maximum number of bin points
const int MAXPT=500;

/// \brief length of PROC_STATUS column in Astro-H FITS files
const int LENPROCSTATUS=32;

/// \brief structure to hold parameter values
struct Par {
  Par(): numevent(0), minevent(0), gapdt(0.), grpoverlap(0.), startenergy(0.), 
         stopenergy(0.), extraspread(0.), evchannel(0.), binwidth(0),
         broadening(0.), gridprofile(false), centerprof(false), fitwidth(false),
         backtype(0), spangti(false), avgwinrad(0.), calcerr(false), 
         writeerrfunc(false),minwidth0(0.), maxitcycle(0), r2tol(0.), 
         searchstepshift(0.), maxdshift(0.), bisectolshift(0.), 
         searchstepwidth(0.), maxdwidth(0.), bisectolwidth(0.), minwidth(0.),
         nerrshift(0), nerrwidth(0), shifterrfac(0.), widtherrfac(0.) {};

  std::string infile;           // name of input file
  std::string outfile;          // name of output file
  std::string linefitfile;      // name of CALDB file with calibration line data
  std::string linetocorrect;    // name of calibration line to use; e.g. Mnka
  std::string energycol;        // name of column to use in fit (e.g. PI or PHA)
  std::string splitcol;         // column name used to separate the data
  int numevent;                 // nominal number of events to group
  int minevent;                 // minimum nunber of events allowed in group
  std::string gtifile;          // name of gti file
  double gapdt;                 // largest allowed separation of two consecutive events in the same group
  double grpoverlap;            // percentage of points shared between two consecutive groups
  double startenergy;           // start of selection region in eV
  double stopenergy;            // end of selection region
  double extraspread;           // enlarge line spread in eV
  double evchannel;             // size of PI channel in eV
  double binwidth;              // width of energy grid in units of channel
  double broadening;            // Gaussian FWHM to convolve profile with
  bool gridprofile;             // true to exit tool after writing profile
  bool centerprof;              // true if centering theoretical profile in energy range (instead of using natural position)
  bool fitwidth;                // true to fit convolution width
  int backtype;                 // type of background to fit - none, constant, or sloped (as enumerated value; see ahgain library)
  bool spangti;                 // ignore GTI in the data accumulation
  double avgwinrad;             // radius of interval used to update binned average (channel units), -1 for auto
  bool calcerr;                 // true if calculating uncertainties on shift and width
  bool writeerrfunc;            // output likelihood functions used in shift/width uncertainty (if calcerr)
  double minwidth0;             // smallest allowed initial value in width fitting (eV)
  int maxitcycle;               // maximum number of fitting iterations
  double r2tol;                 // convergence criterion for R^2
  double searchstepshift;       // step size when fitting shift (channel units)
  double maxdshift;             // largest allowed deviation from initial shift (channel units)
  double bisectolshift;         // tolerance of shift in bisection method (channel units)
  double searchstepwidth;       // step size when fitting width (channel units)
  double maxdwidth;             // largest allowed deviation from initial width (channel units)
  double bisectolwidth;         // tolerance of width in bisection method (channel units)
  double minwidth;              // smallest width to allow in fitting (channel units)
  int nerrshift;                // number of shifts in uncertainty calculations
  int nerrwidth;                // number of widths in uncertainty calculations
  double shifterrfac;           // used to determine domain of shifts
  double widtherrfac;           // used to determine domain of widths
};

// *****************************************************************************


/// \brief Get parameter values
/// \param[out] par           structure with parameter values
void getPar(Par& par);

/// \brief Open input file, check for columns, load calibration line data, and
///  create output file.
/// \param[in] par            structure with parameter values
/// \param[in] havegti        true if GTI file is given
/// \param[out] gp            file pointer to GTI file (if present)
/// \param[out] telescop      TELESCOP keyword value
/// \param[out] instrume      INSTRUME keyword value
/// \param[out] detnam        DETNAM keyword value
/// \param[out] linedat       structure containing calibration line data
/// \param[out] infilelist    list of input file names
void initialize(const Par& par, bool & havegti, ahfits::FilePtr& gp,
                std::string& telescop, std::string& instrume, std::string& detnam,
                callines::CalLines& linedat,ahfits::ListStringType& infilelist);

/// \brief Group event data and perform gain fitting.
/// \param[in] par            structure with parameter values
/// \param[in] havegti        true if GTI file is given
/// \param[out] telescop      TELESCOP keyword value
/// \param[out] instrume      INSTRUME keyword value
/// \param[out] detnam        DETNAM keyword value
/// \param[in] fpout          file pointer to output file
/// \param[in] gp             file pointer to GTI file (if present)
/// \param[in] linedat        structure containing calibration line data
/// \param[in] infilelist     list of input file names
void doWork(Par& par, bool havegti, ahfits::FilePtr& fpout, ahfits::FilePtr gp,
            const std::string& telescop, const std::string& instrume, 
            const std::string& detnam, callines::CalLines& linedat,
            const ahfits::ListStringType& infilelist);

/// \brief Close open FITS files.
/// \param[in] fpout          file pointer to output file
/// \param[in] gp             file pointer to GTI file (if present)
void finalize(ahfits::FilePtr& fpout, ahfits::FilePtr& gp);

/// \brief Write warnings to log file when the fitting fails for a group.
/// \param[in] mesh binning mesh
/// \param[in] bindat counts per mesh bin
/// \param[in] nmesh number of bins
/// \param[in] splitval split column value of group
void reportFailure(double* mesh, double* bindat, int nmesh, int splitval);

/// \brief Set the columns as NULL for a non-fitted profile
/// \param[in] profdat   ProfDat item to set to copy non-fitted profile from
/// \param[out] results  FitResults item to copy non-fitted profile to
/// \param[out] nullcol  Flag to set columns to null
void setNullRow(const ahgain::CalProfile & profdat, ahgain::FitResults & results, 
                char * nullcol);

// ****************************************************************************

/// \brief ahgainfit tool
///
/// Long description
int main(int argc, char** argv) {

  Par par;                            // structure with parameter values
  bool havegti=false;                 // true if GTI file given

  std::string telescop="N/A";         // TELESCOP keyword value
  std::string instrume="N/A";         // INSTRUME keyword value
  std::string detnam="N/A";           // DETNAM keyword value

  ahfits::FilePtr fpout=0;            // FITS file pointer to output file
  ahfits::FilePtr gp=0;               // FITS file pointer to GTI file
  callines::CalLines linedat;         // line data for single calibration source
  ahfits::ListStringType infilelist;  // list of input files

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      ahapp::writeParametersToLog(); 
      initialize(par,havegti,gp,telescop,instrume,detnam,linedat,infilelist);
      doWork(par,havegti,fpout,gp,telescop,instrume,detnam,linedat,infilelist);
      finalize(fpout,gp);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,havegti,gp,telescop,instrume,detnam,linedat,infilelist);
        doWork(par,havegti,fpout,gp,telescop,instrume,detnam,linedat,infilelist);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpout,gp);
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
  par.infile=ahapp::getParString("infile");
  par.outfile=ahapp::getParString("outfile");
  par.linefitfile=ahapp::getParString("linefitfile");
  par.linetocorrect=ahapp::getParString("linetocorrect");
  par.energycol=ahapp::getParString("energycol");
  par.splitcol=ahapp::getParString("splitcol");
  par.numevent=ahapp::getParInt("numevent");
  par.minevent=ahapp::getParInt("minevent");
  par.gtifile=ahapp::getParString("gtifile");
  par.gapdt=ahapp::getParDouble("gapdt");
  par.grpoverlap=ahapp::getParDouble("grpoverlap");
  par.startenergy=ahapp::getParDouble("startenergy");
  par.stopenergy=ahapp::getParDouble("stopenergy");
  par.extraspread=ahapp::getParDouble("extraspread");
  par.evchannel=ahapp::getParDouble("evchannel");
  par.binwidth=ahapp::getParDouble("binwidth");
  par.broadening=ahapp::getParDouble("broadening");
  par.gridprofile=ahapp::getParBool("gridprofile");
  par.fitwidth=ahapp::getParBool("fitwidth");
  par.spangti=ahapp::getParBool("spangti");
  par.calcerr=ahapp::getParBool("calcerr");
  par.writeerrfunc=ahapp::getParBool("writeerrfunc");

  // read numerical parameters and convert units from binwidth to channel
  par.avgwinrad=ahapp::getParDouble("avgwinrad")*par.binwidth;
  par.minwidth0=ahapp::getParDouble("minwidth0")*par.binwidth;
  par.maxitcycle=ahapp::getParInt("maxitcycle");
  par.r2tol=ahapp::getParDouble("r2tol");
  par.searchstepshift=ahapp::getParDouble("searchstepshift")*par.binwidth;
  par.maxdshift=ahapp::getParDouble("maxdshift")*par.binwidth;
  par.bisectolshift=ahapp::getParDouble("bisectolshift")*par.binwidth;
  par.searchstepwidth=ahapp::getParDouble("searchstepwidth")*par.binwidth;
  par.maxdwidth=ahapp::getParDouble("maxdwidth")*par.binwidth;
  par.bisectolwidth=ahapp::getParDouble("bisectolwidth")*par.binwidth;
  par.minwidth=ahapp::getParDouble("minwidth")*par.binwidth;
  par.nerrshift=ahapp::getParInt("nerrshift");
  par.nerrwidth=ahapp::getParInt("nerrwidth");
  par.shifterrfac=ahapp::getParDouble("shifterrfac");
  par.widtherrfac=ahapp::getParDouble("widtherrfac");

  // check for valid background type - only check first character
  std::string background=ahgen::strtoupper(ahapp::getParString("background"));
  if (background[0] == 'N')
    par.backtype=ahgain::e_NOBACKGROUND;
  else if (background[0] == 'C')
    par.backtype=ahgain::e_CONSTBACKGROUND;
  else if (background[0] == 'S')
    par.backtype=ahgain::e_SLOPEBACKGROUND;
  else
    AH_THROW_RUNTIME("invalid value for background parameter; expecting NONE, CONST, or SLOPE");

  // should have: minevent <= numevent
  if (par.minevent > par.numevent) {
    AH_INFO(ahlog::HIGH) << " *** minevent should not be larger than numevent; setting minevent=numevent" << std::endl;
    par.minevent=par.numevent;
  }

  // if startenergy or stopenergy are specified (positive value), then center
  // profile in energy range
  par.centerprof=false;
  if (par.startenergy > 0. || par.stopenergy > 0.) par.centerprof=true;

  // check for valid values of likelihood parameters
  if (par.calcerr) {
    if (par.nerrshift <= 0) AH_THROW_RUNTIME("nlikeshift parameter must be greater than zero");
    if (par.nerrshift < 20) AH_INFO(ahlog::HIGH) << "small value of nlikeshift parameter given (" << par.nerrshift << "); likelihood uncertainty may be inaccurate" << std::endl;
    if (par.nerrwidth <= 0) AH_THROW_RUNTIME("nlikewidth parameter must be greater than zero");
    if (par.nerrwidth < 20) AH_INFO(ahlog::HIGH) << "small value of nlikewidth parameter given (" << par.nerrshift << "); likelihood uncertainty may be inaccurate" << std::endl;
    if (par.shifterrfac <= 0.) AH_THROW_RUNTIME("likeshiftfac parameter must be greater than zero");
    if (par.widtherrfac <= 0.) AH_THROW_RUNTIME("likewidthfac parameter must be greater than zero");
  }

}

// ****************************************************************************

void initialize(const Par& par, bool & havegti, ahfits::FilePtr& gp,
                std::string& telescop, std::string& instrume, std::string& detnam,
                callines::CalLines& linedat,ahfits::ListStringType& infilelist) {

  // for resolved CALDB query
  std::string actual_linefitfile;
  std::string dateobs;
  
  // store TSTART & TSTOP for all files
  std::vector<double> vec_tstart;
  std::vector<double> vec_tstop;
  std::vector<double> vec_tstart_sorted;
  std::vector<int> file_order;

  // need to check if all input files have zero rows
  bool allempty=true;

  // iterator for file list
  ahfits::ListStringType::const_iterator fit;            // iterator over input files

  // Open GTI file, if necessary
  if (ahgen::strtoupper(par.gtifile) != "NONE" && !par.gridprofile) {
    ahfits::open(par.gtifile,"",&gp);
    if (ahfits::isPrimary(gp)) ahfits::move(gp,"GTI");   // if extended syntax not used to specify extension, move to GTI extension
    havegti=true;
    AH_INFO(ahlog::HIGH) << "Opened GTI file: " << par.gtifile << std::endl;
  }

  // Get list of input files.
  ahfits::expandFileList(par.infile, infilelist);
  if (infilelist.size() == 0)
    AH_THROW_RUNTIME("no file list found in infile: "+par.infile);
  AH_INFO(ahlog::HIGH) << "Number of input files: " << infilelist.size() << std::endl;

  // Get the DATE-OBS keyword from the EVENTS extension of the first infile,
  // for the linefitfile CALDB query.
  std::string firstinfile=*infilelist.begin();
  ahfits::FilePtr firstfp=NULL;
  ahfits::open(firstinfile,"",&firstfp);
  if (ahfits::isPrimary(firstfp)) ahfits::move(firstfp,"EVENTS");
  if (!ahfits::readOK(firstfp))
    AH_THROW_RUNTIME("failed to open input file: "+firstinfile+"at HDU: EVENTS");
  dateobs=ahfits::getKeyValStr(firstfp,"DATE-OBS");
  ahfits::close(firstfp);
  
  // Read calibration line data from CALDB file.
  actual_linefitfile=ahmission::caldb::resolve(par.linefitfile, "line data", "GEN", "-", par.linetocorrect, dateobs);
  ape_trad_set_string("linefitfile",actual_linefitfile.c_str());   // to record actual file path in history
  callines::load(actual_linefitfile,par.linetocorrect,linedat);
  AH_INFO(ahlog::HIGH) << "Loaded calibration line data for " << par.linetocorrect << "; number of components: " << linedat.m_nlines << std::endl;

  // Do not need to check keywords or sort input files if only computing profile.
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

      // Read TELESCOP, INSTRUME, DETNAM from first file.
      if (telescop == "N/A") telescop=ahgen::strtoupper(ahfits::getKeyValStr(fp,"TELESCOP"));
      if (instrume == "N/A") instrume=ahgen::strtoupper(ahfits::getKeyValStr(fp,"INSTRUME"));
      if (detnam == "N/A") detnam=ahgen::strtoupper(ahfits::getKeyValStr(fp,"DETNAM"));

      // Check that TELESCOP, INSTRUME, DETNAM agree with first file.
      std::string tmp_telescop=ahgen::strtoupper(ahfits::getKeyValStr(fp,"TELESCOP"));
      std::string tmp_instrume=ahgen::strtoupper(ahfits::getKeyValStr(fp,"INSTRUME"));
      std::string tmp_detnam=ahgen::strtoupper(ahfits::getKeyValStr(fp,"DETNAM"));

      if (tmp_telescop != telescop || tmp_instrume != instrume || tmp_detnam != detnam) {
        AH_THROW_RUNTIME("Mismatch of TELESCOP, INSTRUME, & DETNAM between input files");
      }

      // Check for require columns.
      bool missing=false;
      if (!ahfits::haveColumn(fp,"TIME")) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing TIME column" << std::endl;
      }
      if (!ahfits::haveColumn(fp,"PROC_STATUS")) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing PROC_STATUS column" << std::endl;
      }
      if (!ahfits::haveColumn(fp,par.energycol)) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing " << par.energycol << " column" << std::endl;
      }
      if (par.splitcol != "" && !ahfits::haveColumn(fp,par.splitcol)) {
        missing=true;
        AH_INFO(ahlog::LOW) << "input file, " << infile << ", missing " << par.splitcol << " column" << std::endl;
      }
      if (missing)
        AH_THROW_RUNTIME("Input file, "+infile+", missing columns... stopping program.");
  
      // Store TSTART & TSTOP.
      vec_tstart.push_back(ahfits::getKeyValDbl(fp,"TSTART"));
      vec_tstop.push_back(ahfits::getKeyValDbl(fp,"TSTOP"));

      ahfits::close(fp);
    }

    if (allempty) AH_THROW_RUNTIME("all input FITS tables contain no rows; tool cannot continue");
    AH_INFO(ahlog::HIGH) << "Input TELESCOP, INSTRUME, DETNAME = " << telescop << ", " << instrume << ", " << detnam << std::endl;

    // Check if any files overlap in time (illegal) and sort files by TSTART.
    vec_tstart_sorted=vec_tstart;
    std::sort(vec_tstart_sorted.begin(),vec_tstart_sorted.end());
    bool overlap=false;
    for (int i=0; i < (int)vec_tstart.size(); i++) {

      // Get sorted index for current file.
      int pos=std::find(vec_tstart_sorted.begin(),vec_tstart_sorted.end(),vec_tstart[i])-vec_tstart_sorted.begin();
      file_order.push_back(pos);

      // Check if current file overlaps with any previous file.
      for (int j=0; j < i; j++) {
        if ( ((vec_tstart[i] <= vec_tstart[j]) && (vec_tstop[i] >= vec_tstart[j])) ||
             ((vec_tstart[j] <= vec_tstart[i]) && (vec_tstop[j] >= vec_tstart[i])) ) {
          overlap=true;
          AH_INFO(ahlog::HIGH) << "Illegal: files " << i << " and " << j << " overlap in time." << std::endl;
        }
      }
    }
    if (overlap) AH_THROW_RUNTIME("Overlapping files found... stopping program.");

    // Rearrange input files based on sort.
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

  // Write list of parameters to log file.
  ahapp::writeParametersToLog(); 
}

// ****************************************************************************

void doWork(Par& par, bool havegti, ahfits::FilePtr& fpout, ahfits::FilePtr gp,
            const std::string& telescop, const std::string& instrume, 
            const std::string& detnam, callines::CalLines& linedat,
            const ahfits::ListStringType& infilelist) {

  // declare variables
  int nmesh=0;                         // size of binning grid
  double* mesh=0;                      // profile mesh
  double gwidth=0.;                    // convolution width computed from FWHM parameter
  long long irow=0;                    // event loop row counter
  double efirst=0.;                    // energy of first bin
  double elast=0.;                     // energy of last bin
  double chfirst=0.;                   // channel of first bin
  double chlast=0.;                    // channel of last bin

  ahgain::CalProfile profdat;          // theoretical profile; convolved
  ahgain::EventData eventRow;          // data for single row from infile
  ahgain::FitResults results;          // data for single output row
  double gt_start=0.;                  // current TSTART from GTI file
  double gt_stop=0.;                   // current TSTOP from GTI file
  double pr_energy=0.;                 // ENERGY column in 2nd EXT of output
  double pr_amplitude=0.;              // AMPLITUDE column in 2nd EXT of output

  double tstart=-1.;                   // after event loop, will contains TIME from first event used
  double tstop=0.;                     // after event loop, will contains TIME from last event used
  std::map<int, double> gtigaptime;    // cumulative time that GTI is off, used to compute EXPOSURE
  double gtistop_prev=0.;              // STOP time from previous GTI
  int nptremove=0;                     // number of points to remove after processing; determined with the grpoverlap parameter

  // In the case of not fitting the width, we need a non-zero intrinsic width 
  // of the calibration feature to determine the range of shifts to use in
  // the uncertainty calculation.  The precise value is not important.
  double basewidth=0.5;

  char nullcol[1] = { 0 };             // null flag for columns

  ahfits::ListStringType::const_iterator fit;  // iterator over input files

  ahfits::FilePtr fpin=0;              // ahfits FITS file pointer to input file
  ahfits::Router* routin=0;            // router connecting to input file
  ahfits::Router* routgp=0;            // router connecting to GTI file

  ahgain::EventVecMap eventdat;        // store groups of points for each splitcol value

  // Store PROC_STATUS value from input file.
  char proc_status[LENPROCSTATUS];
  ahfits::IndexType len_proc_status=LENPROCSTATUS;
  for (int k=0; k < LENPROCSTATUS; k++) proc_status[k]=0;

  // If the startenergy and stopenergy parameters are negative, automatically
  // determine energy range from calibration line energies. 
  efirst=par.startenergy;
  if (efirst < 0) efirst=callines::getMinEnergy(linedat)-par.extraspread;
  elast=par.stopenergy;
  if (elast < 0) elast=callines::getMaxEnergy(linedat)+par.extraspread;
  AH_INFO(ahlog::LOW) << "Search range for calibration feature is [" << efirst << " : " << elast << "] in eV" << std::endl;

  // Convert energy range into channel units.
  chfirst=efirst/par.evchannel;
  chlast=elast/par.evchannel;
  AH_INFO(ahlog::LOW) << "                                     or [" << chfirst << " : " << chlast << "] in channel units" << std::endl;

  // Calculate number of mesh points needed for binning mesh.
  nmesh=1+(int)((chlast-chfirst)/par.binwidth);
  if (nmesh <= MINPT) {
    std::stringstream msg;
    msg << "number of binning points smaller than minimum: " << nmesh << " < " << MINPT;
    AH_THROW_RUNTIME(msg.str());
  }
  if (nmesh > MAXPT) {
    std::stringstream msg;
    msg << "number of binning points exceeds maximum: " << nmesh << " > " << MAXPT;
    AH_THROW_RUNTIME(msg.str());
  }
  AH_INFO(ahlog::HIGH) << "Profile and binning mesh covering search range has " << nmesh << " points" << std::endl;

  // Construct bin mesh.
  mesh=new double[nmesh];
  for (int i=0; i < nmesh; i++) {
    mesh[i]=chfirst+par.binwidth*i;
  }

  // Construct profile using Lorentzians for each calibration line and
  // convolve with Gaussian.
  gwidth=ahmath::convertFWHM2sigma(par.broadening/par.evchannel);    // convert to std dev in channel units
  ahgain::constructCalibrationProfile(mesh,nmesh,par.evchannel,linedat,par.centerprof,gwidth,profdat);
  AH_INFO(ahlog::HIGH) << "Average of calibration profile on mesh is: " << profdat.m_avg_prof << std::endl;

  // Create output file as new FITS file and move to profile extension.
  // Note: this is not done in initialize, because we do not know the number
  // of bin mesh points until now.
  ahgain::createGainOutput(par.outfile,par.splitcol,telescop,instrume,
                           detnam,par.gridprofile,par.linetocorrect,chfirst,
                           chlast,par.binwidth,par.energycol,nmesh,par.calcerr,
                           par.writeerrfunc,par.nerrshift,par.nerrwidth,fpout);
  ahfits::move(fpout,"Grid_profile");

  // Create router for writing output file
  ahfits::Router routout(fpout);       // router connecting to output file

  // Write profile to output.
  routout.connectScalar(ahfits::e_WRITEONLY,"ENERGY",pr_energy);
  routout.connectScalar(ahfits::e_WRITEONLY,"AMPLITUDE",pr_amplitude);
  ahfits::firstRow(fpout);
  for (int i=0; i < nmesh; i++) {
    pr_energy=profdat.m_mesh[i];
    pr_amplitude=profdat.m_prof[i];
    ahfits::writeRow(fpout);
    ahfits::nextRow(fpout);
  }

  // Done if only generating profile.
  if (par.gridprofile) return;

  // Define the radius of the window used to calculate updated binned average
  // when fitting the theoretical profile to a group of event data; the updated
  // average is the initial condition of the fitting method; the window is
  // defined by [bavg1-avgwinrad : bavg1+avgwinrad] where bavg1 is the initial
  // binned average of all points in the group.
  if (par.avgwinrad < 0.) {
    int nvoigt=3;                   // cover 3 Voight widths beyond center of furthest line from average
    par.avgwinrad=ahgain::computeAverageWindowRadius(linedat,par.broadening,nvoigt);
    par.avgwinrad=par.avgwinrad/par.evchannel;              // convert from eV to channel
  }
  AH_INFO(ahlog::LOW) << "Value of avgwinrad: " << par.avgwinrad << std::endl;

  // Reset router connections; move to Drift_energy extension in output file.
  routout.clearConnections();
  ahfits::move(fpout,"Drift_energy");

  // Set up connections to output file.
  ahfits::IndexType num_bindat=nmesh;
  ahfits::IndexType num_fitdat=nmesh;
  ahgain::initializeFitResults(mesh,nmesh,par.nerrshift,par.nerrwidth,results);
  routout.connectScalar(ahfits::e_WRITEONLY,"TIME",results.m_time);
  if (par.splitcol != "") routout.connectScalar(ahfits::e_WRITEONLY,par.splitcol,results.m_splitval);
  routout.connectScalar(ahfits::e_WRITEONLY,"COR_FIT",results.m_cor_fit,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"COR_AVE",results.m_cor_ave,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"CHISQ",results.m_chisq,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"AVGUNBIN",results.m_avgunbin,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"AVGBIN",results.m_avgbin,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"AVGFIT",results.m_avgfit,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"SHIFT",results.m_fit_shift,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"SCALE",results.m_fit_scale,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"BGRND",results.m_fit_bgrnd,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"SLOPE",results.m_fit_slope,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"WIDTH",results.m_fit_width,nullcol);
  routout.connectScalar(ahfits::e_WRITEONLY,"TELAPSE",results.m_telapse);
  routout.connectScalar(ahfits::e_WRITEONLY,"EXPOSURE",results.m_exposure);
  routout.connectScalar(ahfits::e_WRITEONLY,"NEVENT",results.m_nevent);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"SPECTRUM",results.m_bindat,num_bindat);
  routout.connectVariableLengthArray(ahfits::e_WRITEONLY,"FITPROF",results.m_fitdat,num_fitdat);
  if (par.calcerr) {
    routout.connectScalar(ahfits::e_WRITEONLY,"SIGSHLIKE",results.m_sigshlike);
    routout.connectScalar(ahfits::e_WRITEONLY,"SIGWDLIKE",results.m_sigwdlike);
    routout.connectScalar(ahfits::e_WRITEONLY,"SIGSHCHI2",results.m_sigshchi2);
    routout.connectScalar(ahfits::e_WRITEONLY,"SIGWDCHI2",results.m_sigwdchi2);
    if (par.writeerrfunc) {
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SHIFTS",results.m_shifts);
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SHLIKE",results.m_shlike);
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"SHCHI2",results.m_shchi2);
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"WIDTHS",results.m_widths);
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"WDLIKE",results.m_wdlike);
      routout.connectFixedLengthArray(ahfits::e_WRITEONLY,"WDCHI2",results.m_wdchi2);
    }
  }

  // Set up GTI connections and read first GTI interval.
  // Note: using a pointer to a Router since GTI file is not always present.
  if (havegti) {
    routgp=new ahfits::Router(gp);
    routgp->connectScalar(ahfits::e_READONLY,"START",gt_start);
    routgp->connectScalar(ahfits::e_READONLY,"STOP",gt_stop);
    ahfits::readRow(gp);
  }

  // Determine the number of points to remove from the grpoverlap parameter.
  // Note: this number is always with respect to a full group (size numevent),
  // which is okay since groups are always emptied if there are less than this
  // number in a group (e.g. if a GTI is crossed).
  nptremove=par.numevent;
  if (par.grpoverlap >= 0.) nptremove=(int)(100.-par.grpoverlap)*par.numevent/100;   // note: grpoverlap is a percentage
  if (nptremove <= 0) nptremove=1;   // always remove at least one point
  AH_INFO(ahlog::HIGH) << "Number of points to retain in group after fitting: " << (par.numevent-nptremove) << std::endl;

  // Counters
  long long nevents=0;             // total number of events read
  long long nskip_procstatus=0;    // number skipped due to bad PROC_STATUS
  long long nskip_pinull=0;        // number skipped due to NULL energy
  long long nskip_timenull=0;      // number skipped due to TIME=NULL
  long long ninrange=0;            // number of events in bin range
  long long nused=0;               // number of events used in fitting

  // Loop over all input files.
  tstart=-1.;         // after event loop, will contains TIME from first event used
  tstop=0.;           // after event loop, will contains TIME from last event used  
  for (fit=infilelist.begin(); fit != infilelist.end(); fit++) {
    std::string infile=*fit;

    // Open current file.
    ahfits::open(infile,"",&fpin);
    if (ahfits::isPrimary(fpin)) ahfits::move(fpin,"EVENTS");   // if extended syntax not used to specify extension, move to EVENTS extensions
    AH_INFO(ahlog::HIGH) << "Processing file: " << infile << " with " << ahfits::numRows(fpin) << " rows" << std::endl;

    // copy keywords from first input file to both output extensions
    if (fit == infilelist.begin()) {
      ahmission::keyword::copyAllKeywords(fpin,fpout,ahmission::keyword::e_EVENT);

      ahfits::FilePtr fpprof=0;
      ahfits::open(par.outfile,"Grid_profile",&fpprof);
      ahmission::keyword::copyAllKeywords(fpin,fpprof,ahmission::keyword::e_EVENT);
      ahfits::close(fpprof);
    }

    // Connect local variables to infile columns.
    routin=new ahfits::Router(fpin);
    routin->connectScalar(ahfits::e_READONLY,"TIME",eventRow.m_time,&eventRow.m_timenull);
    routin->connectScalar(ahfits::e_READONLY,par.energycol,eventRow.m_pi,&eventRow.m_pinull);
    routin->connectBit(ahfits::e_READONLY,"PROC_STATUS",proc_status,len_proc_status);
    if (par.splitcol != "") 
      routin->connectScalar(ahfits::e_READONLY,par.splitcol,eventRow.m_split);
  
    // Main EVENT loop
    irow=0;
    for (ahfits::firstRow(fpin); ahfits::readOK(fpin); ahfits::nextRow(fpin) ) {
      bool addevent=true;       // true if adding event to queue
      bool emptyme=false;       // true if need to process and empty current pixel group
      bool emptyall=false;      // true if need to process and empty all queues
  
      int status=0;
  
      // Reset null flag.
      nullcol[0] = 0;

      ahfits::readRow(fpin);
      irow++;
      nevents++;
  
      // Skip row if PROC_STATUS is bad.
      if (!procstatus::processRow(proc_status)) {
        nskip_procstatus++;
        AH_DEBUG << "Row " << irow << ": bad PROC_STATUS, skipping event" << std::endl;
        continue;
      }
  
      // Skip row if PI is NULL.
      if (eventRow.m_pinull == 1) {
        nskip_pinull++;
        AH_DEBUG << "Row " << irow << ": null energy, skipping event" << std::endl;
        continue;
      }
  
      // Skip row if TIME is NULL.
      if (eventRow.m_timenull == 1) {
        nskip_timenull++;
        AH_DEBUG << "Row " << irow << ": TIME=NULL, skipping event" << std::endl;
        continue;
      }
  
      // Skip if event not in range of profile.
      if (eventRow.m_pi < chfirst || eventRow.m_pi > chlast) continue;
      ninrange++;
  
      // Make sure that gtigaptime is initialized for current PIXEL.
      if (0 == gtigaptime.count(eventRow.m_split)) gtigaptime[eventRow.m_split]=0.;
  
      // Check if TIME in GTI interval.
      // If active GTI ends (and the spangti parameter is false), then need to 
      // process event queues for all pixels; in which case emptyall=true
      // note: if emptyall=true and addevent=true; then event is added to start
      //  of next queue (after current queue is processed).
      addevent=true;
      if (havegti) {
        while (eventRow.m_time > gt_stop) {   // looking for GTI either including our after current event
          if (!par.spangti) emptyall=true;
          ahfits::nextRow(gp);
          if (!ahfits::readOK(gp)) {
            addevent=false;
          } else {
            gtistop_prev=gt_stop;
            ahfits::readRow(gp);
            if (par.spangti) gtigaptime[eventRow.m_split]+=gt_start-gtistop_prev;   // keep track of total GTI off time
          }
        }
        if (eventRow.m_time < gt_start) addevent=false;
      }
  
      // Check if gap time exceeded b/w current event and last event in group.
      if (par.gapdt > 0.) {    // if <= 0, allow infinite gap
        if (addevent && !emptyall && eventdat[eventRow.m_split].size() > 0) {
          if (eventRow.m_time - eventdat[eventRow.m_split].end()->m_time > par.gapdt) emptyme=true;
        }
      }
  
      // Update tstart and tstop for output keywords.
      if (tstart < 0.) tstart=eventRow.m_time;
      tstop=eventRow.m_time;
  
      // Add event to current queue.
      // Note: if empty==true, then event will be added to group after processing.
      if (addevent && !emptyall && !emptyme) {
        if (0 == eventdat.count(eventRow.m_split)) eventdat[eventRow.m_split]=ahgain::EventDataVec();
        eventdat[eventRow.m_split].push_back(eventRow);
        nused++;
      }
  
      // Process queues.
      if (emptyall) {    // GTI boundary crossed, process all queues
        AH_INFO(ahlog::LOW) << std::endl;
        AH_INFO(ahlog::LOW) << "GTI boundary crossed, processing all queues" << std::endl;
        for (ahgain::EventVecMap::iterator mit=eventdat.begin(); mit != eventdat.end(); mit++) {  // iterating over all groups
          // mit->first  = split column value
          // mit->second = vector of events (as EventDataVec) 
          if (par.minevent > (int)(mit->second).size()) {      // need at least minevent points to process group
            AH_INFO(ahlog::LOW) << "Skipping group; too few events: splitval=" << mit->first << ", points=" << (mit->second).size() << std::endl;
            continue;
          }
          AH_INFO(ahlog::LOW) << "Processing group: splitval=" << mit->first << ", npoints=" << (mit->second).size() << std::endl;
          if (par.calcerr) ahgain::resetUncertainties(results);
          ahgain::fitEvents((mit->second),(mit->second).size(),profdat,
                            gwidth,par.fitwidth,par.backtype,par.avgwinrad,
                            par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                            par.maxdshift,par.bisectolshift,par.searchstepwidth,
                            par.minwidth,par.maxdwidth,par.bisectolwidth,results,
                            status);
          if (status != 0) {     // fitting failed
            int splitval=(mit->second)[0].m_split;
            reportFailure(profdat.m_mesh,results.m_bindat,profdat.m_nmesh,splitval);
            (mit->second).clear();   // empty group
            setNullRow(profdat, results, nullcol);
          } else {
            // Calculate shift likelihood function
            if (par.calcerr) {

              // Shift uncertainity
              if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
                ahgain::computeLikelihoodShift(results);
                ahgain::computeChi2ShiftUncertainty(results);
              }

              // width uncertainty (only if fitting the width)
              if (par.fitwidth) {
                if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
                  ahgain::computeLikelihoodWidth(results,profdat,gwidth);
                  ahgain::computeChi2WidthUncertainty(results,profdat,gwidth);
                }
              }
            }    // end if calcerr
          }

          // Since emptyall=true, gtispan is False and EXPOSURE=TELAPSE.
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
        AH_INFO(ahlog::LOW) << "Processing group: splitval=" << eventRow.m_split << ", npoints=" << npoints << std::endl;
        if (par.calcerr) ahgain::resetUncertainties(results);
        ahgain::fitEvents(eventdat[eventRow.m_split],npoints,profdat,
                          gwidth,par.fitwidth,par.backtype,par.avgwinrad,
                          par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                          par.maxdshift,par.bisectolshift,par.searchstepwidth,
                          par.minwidth,par.maxdwidth,par.bisectolwidth,results,status);
  
        if (status != 0) {     // fitting failed
            int splitval=eventdat[eventRow.m_split][0].m_split;
            reportFailure(profdat.m_mesh,results.m_bindat,profdat.m_nmesh,splitval);
            setNullRow(profdat, results, nullcol);
        } else {

          if (par.calcerr) {

            // shift uncertainty
            if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
              ahgain::computeLikelihoodShift(results);
              ahgain::computeChi2ShiftUncertainty(results);
            }

            // width uncertainty (only if fitting the width)
            if (par.fitwidth) {
              if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
                ahgain::computeLikelihoodWidth(results,profdat,gwidth);
                ahgain::computeChi2WidthUncertainty(results,profdat,gwidth);
              }
            }
          }    // end if calcerr
        }
        results.m_exposure=results.m_telapse-gtigaptime[eventRow.m_split];  // subtract GTI-off time to get EXPOSURE
        ahfits::writeRow(fpout);    // write contents of results variable (see connect() calls above)
        ahfits::nextRow(fpout);

        // Remove points from group.
        if (emptyme)    // clear all points in group
          eventdat[eventRow.m_split].clear();
        else            // number of points to remove based on grpoverlap parameter
          eventdat[eventRow.m_split].erase(eventdat[eventRow.m_split].begin(),eventdat[eventRow.m_split].begin()+nptremove);

        // Reset gap time after group is written.
        gtigaptime[eventRow.m_split]=0.;
      }
  
      // Initialize queue with current row, if necessary; this can only be
      // satisfied if given a GTI file.
      if (addevent && (emptyall || emptyme)) {
        eventdat[eventRow.m_split].push_back(eventRow);
        nused++;
      }
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
    AH_INFO(ahlog::LOW) << std::endl;
    AH_INFO(ahlog::LOW) << "Processing group: splitval=" << mit->first << ", npoints=" << (mit->second).size() << std::endl;
    int status=0;
    nullcol[0] = 0;     // reset null flag
    if (par.calcerr) ahgain::resetUncertainties(results);
    ahgain::fitEvents((mit->second),(mit->second).size(),profdat,
                      gwidth,par.fitwidth,par.backtype,par.avgwinrad,
                      par.minwidth0,par.maxitcycle,par.r2tol,par.searchstepshift,
                      par.maxdshift,par.bisectolshift,par.searchstepwidth,
                      par.minwidth,par.maxdwidth,par.bisectolwidth,results,status);
    if (status != 0) {     // fitting failed
      int splitval=(mit->second)[0].m_split;
      reportFailure(profdat.m_mesh,results.m_bindat,profdat.m_nmesh,splitval);
      setNullRow(profdat, results, nullcol);
    } else {
      // Calculate uncertainties
      if (par.calcerr) {

        // shift uncertainty
        if (ahgain::constructShiftArray(results,basewidth,par.shifterrfac)) {
          ahgain::computeLikelihoodShift(results);
          ahgain::computeChi2ShiftUncertainty(results);
        }

        // width uncertainty (only if fitting the width)
        if (par.fitwidth) {
          if (ahgain::constructWidthArray(results,gwidth,par.widtherrfac)) {
            ahgain::computeLikelihoodWidth(results,profdat,gwidth);
            ahgain::computeChi2WidthUncertainty(results,profdat,gwidth);
          }
        }
      }    // end if calcerr
    }
    results.m_exposure=results.m_telapse-gtigaptime[eventRow.m_split];  // subtract GTI-off time to get EXPOSURE
    ahfits::writeRow(fpout);    // write contents of results variable (see connect() calls above)
    ahfits::nextRow(fpout);
  }

  // Write keywords to output file
  ahfits::writeKeyValDbl(fpout,"TSTART",tstart,"");
  ahfits::writeKeyValDbl(fpout,"TSTOP",tstop,"");

  // free memory
  if (mesh != 0) delete [] mesh, mesh=0;
  if (routgp != 0) delete routgp; routgp=0;

  // Report counters
  AH_INFO(ahlog::HIGH) << std::endl;
  AH_INFO(ahlog::HIGH) << "Counters: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Total number of events read:           " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped due to bad PROC_STATUS: " << nskip_procstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped due to NULL energy:     " << nskip_pinull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number skipped due to NULL TIME:       " << nskip_timenull << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number in bin range:                   " << ninrange << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number used in fitting:                " << nused << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr& fpout, ahfits::FilePtr& gp) {

  if (fpout != 0) ahfits::close(fpout); fpout=0;
  if (gp != 0) ahfits::close(gp); gp=0;
}

// ****************************************************************************

void reportFailure(double* mesh, double* bindat, int nmesh, int splitval) {

  AH_INFO(ahlog::HIGH) << " *** Failure fitting group with splitval = " << splitval << std::endl;
  AH_INFO(ahlog::HIGH) << "Binned data printed below." << std::endl;
  AH_INFO(ahlog::HIGH) << " Mesh (channel)    Counts" << std::endl;
  for (int j=0; j < nmesh; j++) {
    AH_INFO(ahlog::HIGH) << mesh[j] << "   " << bindat[j] << std::endl;
  }
}

// ****************************************************************************

void setNullRow(const ahgain::CalProfile & profdat, ahgain::FitResults & results,
                char * nullcol) {

  // Set members of results to NULL in the case of a non-fit: 
  // 1. Fitted parameters are not applied to column FITPROF
  //    Reset results fitted data to profile data
  // 2. Columns TIME, splitcol, TELAPSE, EXPOSURE, NEVENT, 
  //    BINMESH and SPECTRUM are left as-is
  for (int j=0; j < results.m_nmesh; j++) {
    results.m_fitdat[j] = profdat.m_prof[j];
  }

  // 3. Results members set as NULL:
  //    COR_FIT, COR_AVE, CHISQ, AVGUNBIN, AVGBIN, AVGFIT, 
  //    SHIFT, SCALE, BGRND, WIDTH
  nullcol[0] = 1;

}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: ahgainfit.cxx,v $
 Revision 1.18  2016/04/15 22:34:15  rshill
 Add two writeParametersToLog calls to main.

 Revision 1.17  2016/04/15 22:28:03  rshill
 Force telescop, instrume, detnam to upper case (issue #610).

 Revision 1.16  2016/04/07 21:29:44  mwitthoe
 ahgainfit tool: change INFO messages to DEBUG in row loop

 Revision 1.15  2016/03/18 15:05:47  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.14  2015/12/29 18:24:40  rshill
 Added checkEmptyTable().

 Revision 1.13  2015/11/17 20:02:48  mwitthoe
 ahgainfit: copy keywords from input file to both output extensions

 Revision 1.12  2015/11/17 16:48:49  mwitthoe
 ahgainfit: remove centerprof parameter (behavior is automatically enabled when startenergy or stopenergy is specified)

 Revision 1.11  2015/10/28 18:27:24  mwitthoe
 ahgainfit: update tool after restructuring of ahgain library

 Revision 1.10  2015/10/27 19:07:28  mwitthoe
 ahgainfit: allow calculation of shift and width uncertainties

 Revision 1.9  2015/08/11 19:32:03  mwitthoe
 ahgainfit: add log statements; stamp parameters to log file; general clean-up

 Revision 1.8  2015/07/16 14:38:39  klrutkow
 added CALDB queries with ahmission query

 Revision 1.7  2015/06/03 18:35:53  asargent
 Removed extra clearing of groups when there is a fit failure

 Revision 1.6  2015/06/03 17:40:44  asargent
 Added new function to write null columns when profile fitting failed.

 Revision 1.5  2015/04/03 17:49:39  mwitthoe
 ahgainfit: convert gtifile parameter to uppercase before checking for NONE

 Revision 1.4  2015/04/01 20:05:18  mwitthoe
 ahgainfit: fix warning from 32-bit linux compiler

 Revision 1.3  2015/03/18 18:09:43  mwitthoe
 ahgainfit: change DETNAME to DETNAM

 Revision 1.2  2015/03/11 16:00:31  mwitthoe
 ahgainfit: add background parameter to parameter listing in source code

 Revision 1.1  2015/03/10 16:49:04  mwitthoe
 ahgainfit: rename gainfit task




 Logs below refer to old name of tool: gainfit

 Revision 1.21  2015/03/10 14:49:07  mwitthoe
 gainfit: add background parameter which specifies what kind of background to fit with: none, constant, or sloped; see issue 481

 Revision 1.20  2015/01/21 16:13:36  mwitthoe
 gainfit: add parameter to allow centering of the theoretical profile in the energy mesh (centerprof)

 Revision 1.19  2014/12/30 22:21:44  mwitthoe
 gainfit: fix outfile parameter description; issue 472

 Revision 1.18  2014/12/30 19:12:10  mwitthoe
 gainfit: update parameters; see issue 472

 Revision 1.17  2014/11/12 13:23:35  mwitthoe
 gainfit: now open all input files temporarily in initialize in order to check for validity and sort by TSTART; see issue 455

 Revision 1.16  2014/11/06 22:08:52  mwitthoe
 gainfit: updated parameter description in par file; updated HTML file; added support for filelists for input files (infile parameter), see issue 455

 Revision 1.15  2014/10/09 19:51:11  mwitthoe
 gainfit: correct reported unit of binwidth as channel instead of eV

 Revision 1.14  2014/10/03 00:59:09  mwitthoe
 gainfit: remove obsolete output structure

 Revision 1.13  2014/10/02 08:07:59  mwitthoe
 gainfit: add parameter to control largest allowed gap in time between two events allowed in a single group (gapdt); add parameter to set the overlap between two consecutive groups as a percentage of the group size; see issue 445

 Revision 1.12  2014/10/02 01:19:21  mwitthoe
 gainfit: check if TIME is NULL before adding to group; see issue 445

 Revision 1.11  2014/10/02 00:54:00  mwitthoe
 gainfit: write FITPROF output column containing the theoretical profile with the fitted parameters applied; fix the listed unit of binwidth in the parameter file

 Revision 1.10  2014/09/15 21:26:47  mwitthoe
 gainfit: fix bug in extended syntax support for input file; issue 179

 Revision 1.9  2014/09/12 20:52:36  mwitthoe
 gainfit: allow extended syntax for the input EVENTS file and GTI file

 Revision 1.8  2014/08/18 20:59:05  mwitthoe
 gainfit: fix segmentation fault that occurred when the gridprofile parameter was set to yes; see issue 422

 Revision 1.7  2014/08/07 17:39:42  mwitthoe
 gainfit tool: switch to new version of constructCalibrationProfile()

 Revision 1.6  2014/08/07 14:30:20  mwitthoe
 gainfit: fix bug where SPECTRUM column was not being written

 Revision 1.5  2014/08/06 20:35:59  mwitthoe
 gainfit tool: correct keywords in output file; compute EXPOSURE column; change DELTATIME column to TELAPSE; write group averages to output file

 Revision 1.4  2014/08/05 13:52:41  mwitthoe
 gainfit: change binwidth parameter from integer to real

 Revision 1.3  2014/08/05 12:55:31  mwitthoe
 gainfit: change CH_START, CH_STOP, and CH_WIDTH keywords from int to double in the output FITS file

 Revision 1.2  2014/07/21 15:37:50  mwitthoe
 gainfit: correct the calculation of the fitted width; previous version was using the FWHM in eV from the broadening parameter instead of the standard deviation in channel units

 Revision 1.1  2014/07/17 19:50:35  mwitthoe
 add new tool, gainfit, which calculates gain correction factors; see issue 378


*/

