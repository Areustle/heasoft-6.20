/// \file sxsanticopi.cxx
/// \brief calculates the final PHA and PI for the antico data
/// \author Mike Dutka
/// \date $Date: 2016/06/01 21:15:41 $
/// \version 1.0

/** 

\defgroup tool_sxsanticopi Calculate PHA and PI for antico data (sxsanticopi)
@ingroup mod_sxs_tasks

This task calculates the PHA and PI for the antico data file.  The PHA is 
calculated only if FLAG_EVENT_LOST is NOT=0 (indicating a lost event) or if 
PROC_STATUS is good. The PHA is always calculated as ADC_SAMPLE_MAX - 
ADC_SAMPLE_PEDESTAL and the PI is calculated using the antico gain file. 
The antico file contains data from both side of the PSP. The PI is 
calculated using the appropriate PSP_ID gain file.

Source files:

  sxsanticopi.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/mission/lib/ahmission

Modification history:

  Ver   Date        Author  Description
  1.0   2015-07-15   MSD    Add CALDB query

*/
 
#define AHLABEL tool_sxsanticopi
#define AHCVSID "$Id: sxsanticopi.cxx,v 1.18 2016/06/01 21:15:41 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahmission/ahmission.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"      // for random numbers
#include "ahlog/ahlog.h"
#include "ahmission/caldb.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <cmath>


/** \addtogroup tool_sxsanticopi
 *  @{
 */

/// \brief Get parameter values
/// \param[out] infile          Input sff event file name
/// \param[out] outfile         Output file name
/// \param[out] gainantfile     Input CALDB antico gain file
/// \param[out] acphaoffset     Average offset to add to PHA before applying gain
/// \param[out] randomize       Allow for randomization  (d/f yes)
/// \param[out] seed            Random number generator seed (0 - system time)
void getPar(std::string& infile, std::string& outfile, std::string& gainantfile,
            double& acphaoffset, bool& randomize, int& seed);

/// \brief Copy contents of infile to outfile; read input data
/// \param[in] infile               Input sff event file name
/// \param[in] outfile              output file name
/// \param[in] gainantfile          input CALDB antico gain file
/// \param[in] randomize            Allow for randomization  (d/f yes)
/// \param[in] seed                 Random number generator seed (0 - system time)
/// \param[out] fpevt               ahfits file pointer to evt file
/// \param[out] fpgain              ahfits file pointer to gain file
/// \param[out] tlmin_pha           TLMIN keyword value for PHA
/// \param[out] tlmax_pha           TLMAX keyword value for PHA
/// \param[out] tlmin_pi            TLMIN keyword value for PI
/// \param[out] tlmax_pi            TLMAX keyword value for PI
/// \param[out] ncoeff              maximum number of coeffcients in gainantfile 
/// \param[out] coeff_a             array containing gain coefficients for 
///                                 psp_id = 0 or 1
/// \param[out] coeff_b             array containing gain coefficients for 
///                                 psp_id = 2 or 3
void initialize(const std::string& infile, const std::string& outfile, 
                std::string& gainantfile, bool randomize, int seed,
                ahfits::FilePtr& fpevt, ahfits::FilePtr& fpgain, 
                long long& tlmin_pha, long long& tlmax_pha, long long& tlmin_pi, 
                long long& tlmax_pi, int& ncoeff, double ** coeff_a, double ** coeff_b);

/// \brief compute and write PHA and PI columns
/// \param[in] fpevt               ahfits file pointer to evt file
/// \param[in] infile              Input sff event file name
/// \param[in] outfile             output file name
/// \param[in] gainantfile         input CALDB antico gain file
/// \param[out] acphaoffset        Average offset to add to PHA before applying gain
/// \param[in] randomize           allow for randomization  (d/f yes)
/// \param[out] tlmin_pha          TLMIN keyword value for PHA
/// \param[out] tlmax_pha          TLMAX keyword value for PHA
/// \param[out] tlmin_pi           TLMIN keyword value for PI
/// \param[out] tlmax_pi           TLMAX keyword value for PI
/// \param[in] ncoeff              maximum number of coeffcients in gainantfile 
/// \param[in] coeff_a             array containing gain coefficients for 
///                                 psp_id = 0 or 1
/// \param[in] coeff_b             array containing gain coefficients for 
///                                 psp_id = 2 or 3
void doWork(ahfits::FilePtr fpevt, std::string infile, std::string outfile,
            std::string gainantfile, double acphaoffset, bool randomize,
            long long& tlmin_pha, long long& tlmax_pha, long long& tlmin_pi,
            long long& tlmax_pi, int ncoeff, double * coeff_a, double * coeff_b);

/// \brief close open FITS files
/// \param[in] fpevt               ahfits file pointer to evt file
/// \param[in] fpgain              ahfits file pointer to gain file
void finalize(ahfits::FilePtr& fpevt, ahfits::FilePtr& fpgain);


// ****************************************************************************

/// \brief sxsanticopi tool
///
int main(int argc, char** argv) {

  ahfits::FilePtr fpevt= 0;
  ahfits::FilePtr fpgain = 0;
  std::string infile;
  std::string outfile; 
  std::string gainantfile;
  double acphaoffset=0.;
  bool randomize = true;
  int seed = 0;
  long long tlmin_pha=0;
  long long tlmax_pha=0;
  long long tlmin_pi=0;
  long long tlmax_pi=0;
  int ncoeff = 0; 
  double * coeff_a = 0;
  double * coeff_b = 0;

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
     
      getPar(infile,outfile,gainantfile,acphaoffset,randomize,seed);
      ahapp::writeParametersToLog(); 
      initialize(infile,outfile,gainantfile,randomize,seed,fpevt,fpgain, 
                   tlmin_pha,tlmax_pha,tlmin_pi,tlmax_pi,ncoeff,&coeff_a,
                   &coeff_b);
      doWork(fpevt,infile,outfile,gainantfile,acphaoffset,randomize,tlmin_pha,
             tlmax_pha,tlmin_pi,tlmax_pi,ncoeff,coeff_a,coeff_b);
      finalize(fpevt,fpgain);
      ahapp::shutDown();
    } else {
      try {
        getPar(infile,outfile,gainantfile,acphaoffset,randomize,seed);
        initialize(infile,outfile,gainantfile,randomize,seed,fpevt,fpgain, 
                   tlmin_pha,tlmax_pha,tlmin_pi,tlmax_pi,ncoeff,&coeff_a,
                   &coeff_b);
        doWork(fpevt,infile,outfile,gainantfile,acphaoffset,randomize,tlmin_pha,
               tlmax_pha,tlmin_pi,tlmax_pi,ncoeff,coeff_a,coeff_b);
      } catch (const std::exception &x) {
        ahapp::writeParametersToLog(); 
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpevt,fpgain);
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

void getPar(std::string& infile, std::string& outfile, std::string& gainantfile,
            double& acphaoffset, bool& randomize, int& seed) {

  infile=ahapp::getParString("infile");
  outfile=ahapp::getParString("outfile");
  gainantfile=ahapp::getParString("gainantfile");
  randomize=ahapp::getParBool("randomize");
  seed=ahapp::getParInt("seed");
}

// ****************************************************************************

void initialize(const std::string& infile, const std::string& outfile, 
                std::string& gainantfile, bool randomize, int seed,
                ahfits::FilePtr& fpevt, ahfits::FilePtr& fpgain, 
                long long& tlmin_pha, long long& tlmax_pha, long long& tlmin_pi, 
                long long& tlmax_pi, int& ncoeff, double ** coeff_a, double ** coeff_b) {

  // declare variables
  std::string instrume_evt;     // INSTRUME keyword from infile
  std::string detnam_evt;       // DETNAM keyword from infile
    int psp_id_gain = 0;        // local value to store PSP_ID from CALDB table
  double* coeff=0;              // local array to store coefficients from CALDB table

  //parameters to query CALDB
  std::string filetype = "antico gain file";
  std::string instrume = "SXS";
  std::string detnam = "-";
  std::string codename = "GAINANT";
  std::string datetime;

  // copy contents of infile to outfile and return opened output file (fpevt);
  ahfits::clone(infile,outfile,&fpevt,true);
  if (ahfits::isPrimary(fpevt)) ahfits::move(fpevt, "EVENTS");   // move to EVENTS extension if extended syntax not used
  ahmission::checkEmptyTable(fpevt,infile);

  //read header keywords of event file
  instrume_evt = ahfits::getKeyValStr(fpevt, "INSTRUME");
  detnam_evt = ahfits::getKeyValStr(fpevt, "DETNAM");
  datetime = ahfits::getKeyValStr(fpevt, "DATE-OBS");

  //Check if DETNAM is antico if not end the task
  if (detnam_evt != "ANTICO") { 
    AH_THROW_RUNTIME("DETNAM is not ANTICO; aborting task");
  }

  // Get TLMIN/TLMAX values for PHA and PI columns from infile
  // Note: the columnRange function returns false if the TLMIN/TLMAX
  // keywords are missing for the given column, or if TLMAX < TLMIN
  if (!ahfits::columnRange(fpevt, "PHA", tlmin_pha, tlmax_pha)) {
    AH_THROW_RUNTIME("TLMIN/TLMAX undefined or illegal for PHA column");
  }
  if (!ahfits::columnRange(fpevt, "PI", tlmin_pi, tlmax_pi)) {
    AH_THROW_RUNTIME("TLMIN/TLMAX undefined or illegal for PI column");
  }
  AH_INFO(ahlog::HIGH) << "PHA TLMIN, TLMAX = " << tlmin_pha << ", " << tlmax_pha << std::endl;
  AH_INFO(ahlog::HIGH) << " PI TLMIN, TLMAX = " << tlmin_pi << ", " << tlmax_pi << std::endl;

  //query caldb for gainant file
  gainantfile = ahmission::caldb::resolve(gainantfile,filetype,instrume,detnam,
                                          codename,datetime);
  ape_trad_set_string("gainantfile",gainantfile.c_str());

  // open gainf file and get number of coefficients
  ahfits::open(gainantfile,"GAIN",&fpgain);
  ncoeff = ahfits::getKeyValLLong(fpgain, "NCOEFF"); 

  //assign memory to coeff_a and coeff_b based on the length of ncoeff
  //and intialize the elements to zero
  *coeff_a = new double[ncoeff];
  for (int ii = 0; ii < ncoeff; ++ii) (*coeff_a)[ii] = 0.0;
  *coeff_b = new double[ncoeff];
  for (int ii = 0; ii < ncoeff; ++ii) (*coeff_b)[ii] = 0.0;

  // allocate array to store coefficients from single CALDB table row
  coeff = new double[ncoeff];
  for (int ii=0; ii < ncoeff; ii++) coeff[ii]=0.;

  // connect local variables 
  // Note: the first coefficient in the gain CALDB file is COEF0; so the gain 
  // formula is COEF0 + COEF1*pha + COEF2*pha^2 + ...
  ahfits::Router router(fpgain);
  router.connectScalar(ahfits::e_READONLY,"PSP_ID",psp_id_gain);
  for (int ii=0; ii < ncoeff; ii++) {
    // form column name; e.g. COEF2
    std::stringstream colname;
    colname << "COEF" << ii;
    router.connectScalar(ahfits::e_READONLY,colname.str(), coeff[ii]);
  }

  // Read gain CALDB file to get coefficients for PSP_ID = 0/1 & 2/3
  for (ahfits::firstRow(fpgain); ahfits::readOK(fpgain); ahfits::nextRow(fpgain)) {
    ahfits::readRow(fpgain);
    if (psp_id_gain == 0 || psp_id_gain == 1) {
      for (int ii=0; ii < ncoeff; ii++) (*coeff_a)[ii] = coeff[ii];
    } else if (psp_id_gain == 2 || psp_id_gain == 3) {
      for (int ii=0; ii < ncoeff; ii++) (*coeff_b)[ii] = coeff[ii];
    }
  }

  // seed random number generator
  if (randomize) ahgen::seedRandom(seed);

  // deallocate memory for local variables connected to gain CALDB columns
  if (coeff != 0) delete [] coeff, coeff=0;

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(ahfits::FilePtr fpevt, std::string infile, std::string outfile,
            std::string gainantfile, double acphaoffset, bool randomize,
            long long& tlmin_pha, long long& tlmax_pha, long long& tlmin_pi,
            long long& tlmax_pi, int ncoeff, double * coeff_a, double * coeff_b) {
  
  //define local variables to connect to values stored in the event file
  int psp_id_evt = 0;
  int adc_sample_max = 0; 
  int adc_sample_pedestalepi = 0;           // ADC_SAMPLE_PEDESTALEPI
  char flg_event_lost = 0;                  // FLG_EVENT_LOST column 
  ahfits::IndexType num_flg_event_lost = 0; // size of flg_event_lost column
  int pha = 0;                              // pha column
  int pi = 0;                               // pi column
  double phar = 0.0;                        // pha real value 
  double pir = 0.0;                         // pi real value
  char outnull_pha = 0;                     // null flag for pha column
  char outnull_pi = 0;                      // null flag for pi column
  char proc_status[32];                     // PROC_STATUS column
  ahfits::IndexType num_proc_status=32;     // size of PROC_STATUS column
  for (int i=0; i < num_proc_status; i++) {
    proc_status[i] = 0;
  }

  // Counters
  long long nevents=0;                      // number of events read
  long long nbadprocstatus=0;               // number with bad PROC_STATUS
  long long nflageventlost=0;               // number flagged as lost
  long long nokay=0;                        // number with PHA/PI computed
  long long nphalimit=0;                    // number of PHA values outside range
  long long npilimit=0;                     // number of PI values outside range

  //setup router 
  ahfits::Router router_evt(fpevt);

  // connect local varibles to fits columns
  router_evt.connectScalar(ahfits::e_READONLY,"PSP_ID",psp_id_evt);
  router_evt.connectScalar(ahfits::e_READONLY,"ADC_SAMPLE_MAX",adc_sample_max);
  router_evt.connectScalar(ahfits::e_READONLY,"ADC_SAMPLE_PEDESTAL", adc_sample_pedestalepi);
  router_evt.connectBit(ahfits::e_READONLY,"FLG_EVENT_LOST",&flg_event_lost,num_flg_event_lost);
  router_evt.connectBit(ahfits::e_READWRITE,"PROC_STATUS",proc_status,num_proc_status);
  
  //connect local variables to output columns
  router_evt.connectScalar(ahfits::e_WRITEONLY,"PHA",pha,&outnull_pha);
  router_evt.connectScalar(ahfits::e_WRITEONLY,"PI",pi,&outnull_pi);

  long long irow=0;
  for (ahfits::firstRow(fpevt); ahfits::readOK(fpevt); ahfits::nextRow(fpevt)) {
    ahfits::readRow(fpevt);
    irow++;
    nevents++;

    //reset outnull flags
    outnull_pha = 0; 
    outnull_pi = 0;
 
    //If PROC_STATUS is bad set the output rows to null
    if (!procstatus::processRow(proc_status) || flg_event_lost == 1){
      outnull_pha = 1; 
      outnull_pi = 1;
      nbadprocstatus++;
      AH_INFO(ahlog::HIGH) << "Row " << irow << ": bad PROC_STATUS; setting PHA/PI = NULL" << std::endl;
    } else {
      nokay++;

      // get base value of PHA
      pha = adc_sample_max - adc_sample_pedestalepi;

      // generate random double between -0.5 and +0.5 to add to PHA with offset
      double randnum=acphaoffset;
      if (randomize) randnum+=(ahgen::getRandom()-0.5);
    
      // set real pha value using random such that the integer distrbution 
      // of pha values will come out correctly 
      phar = (double)(pha + randnum);

      // apply gain (PSP dependent)
      pir=0.;
      if (psp_id_evt < 2) {   // PSP_ID = 0 or 1
        for (int ii=0; ii < ncoeff; ii++) pir+=coeff_a[ii]*std::pow(phar,ii);
      } else {                // PSP_ID = 2 or 3
        for (int ii=0; ii < ncoeff; ii++) pir+=coeff_b[ii]*std::pow(phar,ii);
      }
   
      // If PHA out-of-range, set to TLMAX
      if (pha < tlmin_pha || pha > tlmax_pha) {
        AH_INFO(ahlog::LOW) << "Row " << irow << ": PHA value of " << pha << " out-of-range; setting to TLMAX = " << tlmax_pha << std::endl;
        pha = tlmax_pha;
        nphalimit++;
      }

      // Compute PI and check range
      pi = (int) (floor( (pir-0.5)/0.5 + 1.0));
      if (pi<tlmin_pi || pi>tlmax_pi) {
        AH_INFO(ahlog::LOW) << "Row " << irow << ": PI value of " << pi << " out-of-range; setting to TLMAX = " << tlmax_pi << std::endl;
        pi = tlmax_pi;
        npilimit++;
      }

    } //end if PROC_STATUS

    // write to file
    ahfits::writeRow(fpevt);

  }//end loop

  //Deallocate memory
  if (coeff_a != 0) delete [] coeff_a, coeff_a=0;
  if (coeff_b != 0) delete [] coeff_b, coeff_b=0;

  // Report counters
  AH_INFO(ahlog::HIGH) << "Counters: " << std::endl;
  AH_INFO(ahlog::HIGH) << "  Total number of events:            " << nevents << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with bad PROC_STATUS:       " << nbadprocstatus << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number flagged as lost:            " << nflageventlost << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number with PHA/PI assigned:       " << nokay << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of PHA values out-of-range: " << nphalimit << std::endl;
  AH_INFO(ahlog::HIGH) << "  Number of PI values out-of-range:  " << npilimit << std::endl;

}

// ****************************************************************************

void finalize(ahfits::FilePtr& fpevt, ahfits::FilePtr& fpgain) {

  ahfits::close(fpevt);
  ahfits::close(fpgain);
}

// ****************************************************************************


/** @} */

/* Revision Log
 $Log: sxsanticopi.cxx,v $
 Revision 1.18  2016/06/01 21:15:41  mwitthoe
 sxsanticopi: add parameter, acphaoffset, which sets the average value of the random offset added to PHA values before applying the gain

 Revision 1.17  2016/03/23 17:15:21  mdutka
 Correcting items listed in issue #610

 Revision 1.16  2015/12/29 16:06:21  mwitthoe
 sxsanticopi: throw error if no rows in input FITS table

 Revision 1.15  2015/08/11 16:57:02  mwitthoe
 sxsanticopi: add standard prologue; fix capitalization of yes/no in parameter file; add log statements and counters; general clean-up; see issues 532 & 543

 Revision 1.14  2015/07/15 17:52:44  mdutka
 Adding CALDB query

 Revision 1.13  2015/06/03 16:03:41  klrutkow
 gain file COEF columns are now 0-based not 1-based

 Revision 1.12  2015/04/06 19:16:52  mdutka
 ADC_SAMPLE_PEDESTALEPI -> ADC_SAMPLE_PEDESTAL, FLG_EVENT_LOST 1X not 1B see issue #497

 Revision 1.11  2015/03/18 20:40:10  asargent
 Changed DETNAME to DETNAM

 Revision 1.10  2015/01/09 22:43:52  mwitthoe
 sxsanticopi: check if TLMIN/TLMAX are defined for the PHA and PI columns; if not then throw a runtime error

 Revision 1.9  2014/12/30 14:35:47  mdutka
 Changed parameter list see issue #472

 Revision 1.8  2014/11/06 20:50:05  mdutka
 removed debugging message

 Revision 1.7  2014/10/26 17:46:35  mdutka
 Read TLMIN/TLMAX using new columnRange functions and replaced flag_baseline with flag_event_lost

 Revision 1.6  2014/09/15 21:04:22  mwitthoe
 sxsanticopi: add support for extended syntax with the input file; issue 179

 Revision 1.5  2014/08/11 20:58:27  mwitthoe
 sxsanticopi: fix doxygen

 Revision 1.4  2014/08/06 17:30:18  mwitthoe
 sxsanticopi: changes after review of tool/TRF: 1) change MAXCOEFF keyword to NCOEFF in gain CALDB file, 2) remove reading of MISSION keyword from input file, 3) generalize how gain formula is computed, 4) do not add PHA or PI columns (they should already exist), 5) read TLMIN/TLMAX values for PHA and PI columns from input file


*/
