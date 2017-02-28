/// \file sxsbranch.cxx
/// \brief compute SXS branching ratios 
/// \author Mike Dutka
/// \date $Date: 2016/04/14 16:40:46 $
/// \version 1.0

/** 

\defgroup tool_sxsbranch Compute SXS branching ratios (sxsbranch)
@ingroup mod_sxs_tasks

Sxsbranch calculates branching ratios and effective exposure times for
either (1) an actual SXS event file, (2) a simulated SXS event file,
or (3) an input count rate and exposure time. Branching ratios in each
grade are calculated as the number of events in that grade divided by
the total number of events, and calculated for each SXS pixel and for
the entire array. Effective exposure times for each grade, and for
each pixel, are calculated as the exposure time multiplied by the
fraction of events in that grade. In case (1) branching ratios are
derived according to the grades in the file, while for cases (2) and
(3) cross-talk events may be inserted, grades are assigned based on
event (including any cross-talk events) times in each pixel, and
branching ratios derived accordingly. The task also calculates the
expected branching ratios based on Poisson statistics.

Source files:

  sxsbranch.cxx
  sxsbranchlib.h
  sxsbranchlib.cxx

Library dependencies:

  heacore/ahfits
  heacore/ahlog
  heacore/ahgen
  heacore/ape
  heacore/heautils
  astroh/gen/lib/ahapp
  astroh/gen/lib/ahmath
  astroh/mission/lib/ahmission
  astroh/sxs/lib/ahsxs

Modification history:

  Ver   Date        Author  Description
  1.0   2015-08-03   MSD    Clean-up code

*/
 
#define AHLABEL tool_sxsbranch
#define AHCVSID "$Id: sxsbranch.cxx,v 1.47 2016/04/14 16:40:46 mdutka Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahmission/ahmission.h"
#include "ahmission/caldb.h"
#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"      // for random numbers
#include "ahlog/ahlog.h"
#include "sxsbranchlib.h"
#include "ahsxs/ahsxs.h"
#include "ahsxs/sxsstatus.h"  //STATUS column bit definitions

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <sstream>
#include <cmath>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>  // sort()

 
/** \addtogroup tool_sxsbranch
 *  @{
 */

/// \brief Get parameter values
/// \param[out] par      structure to store input parameters
void getPar(branchlib::Par& par);

/// \brief Obtains pixel mask, pixel fraction, and pixel proximity information.
///        Also opens input files for reading
/// \param[in] par       structure to store input parameters
/// \param[out] fpInput Fits file pointer to input event file
/// \param[out] datIn    structure which holds column and keyword data from 
///                      input file
/// \param[out] mask_arr array identifying which pixels are masked
/// \param[out] frac_arr array indicating the fraction of counts in each 
///                      based on the point spread function
/// \param[out] cumfrac_arr cumulative fraction
/// \param[out] prox_arr array containing pixel proximity information
/// \param[out] quad     array contianing pixel proximity information  
void initialize(branchlib::Par& par, ahfits::FilePtr& fpInput,
                branchlib::infileData& datIn, int (&mask_arr)[branchlib::NPIXELS], 
                double (&frac_arr)[branchlib::NPIXELS], 
                double (&cumfrac_arr)[branchlib::NPIXELS],
                int (&prox_arr)[branchlib::NPIXELS][4], 
                int (&quad)[branchlib::NPIXELS]);

/// \brief  Determines effective exposures and branching ratios for all modes
/// \param[in] fpInput   Fits file pointer to input event file
/// \param[in] fpSim     Fits file pointer to output sim file
/// \param[in] par       structure to store input parameters
/// \param[in] datIn     structure which holds column and keyword data from 
///                      input file 
/// \param[in] mask_arr array identifying which pixels are masked
/// \param[in] frac_arr array indicating the fraction of counts in each 
///                     based on the point spread function
/// \param[out] cumfrac_arr cumulative probability fraction per pixel
/// \param[in] prox_arr array containing pixel proximity information
/// \param[in] quad     array containing pixel proximity information 

void doWork(ahfits::FilePtr& fpInput, ahfits::FilePtr& fpSim, branchlib::Par& par, 
            branchlib::infileData& datIn, int (&mask_arr)[branchlib::NPIXELS], 
            double (&frac_arr)[branchlib::NPIXELS],
            double (&cumfrac_arr)[branchlib::NPIXELS], 
            int (&prox_arr)[branchlib::NPIXELS][4],
            int (&quad)[branchlib::NPIXELS]);

/// \brief  closes input files 
/// \param[in] fpInput   fits file pointer to input file  
/// \param[in] par       structure to store input parameters
void finalize(ahfits::FilePtr &fpInput, ahfits::FilePtr& fpSim, branchlib::Par& par);



// ****************************************************************************

/// \brief sxssimbranch tool
///
int main(int argc, char** argv) {

  branchlib::Par par;                   //Structure containing input parameter values
  int mask_arr[branchlib::NPIXELS];
  double frac_arr[branchlib::NPIXELS];
  double cumfrac_arr[branchlib::NPIXELS]; 
  branchlib::infileData datIn;
  int prox_arr[branchlib::NPIXELS][4];
  int quad[branchlib::NPIXELS]; 

  // initialize arrays
  for (int ii=0; ii < branchlib::NPIXELS; ii++) {
    mask_arr[ii]=0;
    frac_arr[ii]=0.;
    cumfrac_arr[ii]=0.;
    quad[ii]=0;
    for (int jj=0; jj < 4; jj++) prox_arr[ii][jj]=0;
  }
   
  ahfits::FilePtr fpInput=0;     //Fits file pointer to output event file
  ahfits::FilePtr fpSim = 0;     //fits file pointer to output simulated file

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
     
      getPar(par);
      // Write list of parameters to log file                                                          
      ahapp::writeParametersToLog();
      initialize(par,fpInput,datIn,mask_arr,frac_arr,cumfrac_arr,prox_arr,
                 quad);
      doWork(fpInput,fpSim,par,datIn,mask_arr,frac_arr,cumfrac_arr,prox_arr,quad);
      finalize(fpInput, fpSim, par);
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,fpInput,datIn,mask_arr,frac_arr,cumfrac_arr,prox_arr,quad);
        doWork(fpInput,fpSim,par,datIn,mask_arr,frac_arr,cumfrac_arr,prox_arr,quad);
      } catch (const std::exception &x) {
        // Write list of parameters to log file
        ahapp::writeParametersToLog();
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(fpInput, fpSim, par);
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

void getPar(branchlib::Par& par) {

  bool have_infile=true;

  par.m_infile=ahapp::getParString("infile");
  if (ahgen::strtoupper(par.m_infile) == "NONE") have_infile=false;
  if (have_infile) {
    par.m_filetype=ahgen::strtoupper(ahapp::getParString("filetype"));
  } else {
    par.m_filetype="NONE";
  }
  par.m_outfile=ahapp::getParString("outfile");
  if (!have_infile) par.m_countrate=ahapp::getParDouble("countrate");
  if (!have_infile) par.m_exposure=ahapp::getParDouble("exposure");
  par.m_pixfrac=ahapp::getParString("pixfrac"); 
  par.m_pixmask=ahapp::getParString("pixmask");
  par.m_ctelpixfile=ahapp::getParString("ctelpixfile");
  par.m_dtprimary_str=ahapp::getParString("dtprimary");
  par.m_dtmidhigh_str=ahapp::getParString("dtmidhigh");
  par.m_dtlowmid_str=ahapp::getParString("dtlowmid");
  par.m_calpixrate=ahapp::getParDouble("calpixrate");
  par.m_ctphafrac1=ahapp::getParDouble("ctphafrac1");
  par.m_ctphafrac2=ahapp::getParDouble("ctphafrac2");
  if (ahgen::strtoupper(par.m_filetype) == "SIM") par.m_debin=ahapp::getParDouble("debin");
  if (ahgen::strtoupper(par.m_filetype) == "SIM") par.m_enrgthr=ahapp::getParDouble("enrgthr");
  if (ahgen::strtoupper(par.m_filetype) != "REAL") par.m_flagmerge=ahapp::getParBool("flagmerge");
  if (ahgen::strtoupper(par.m_filetype) != "REAL" && par.m_flagmerge) par.m_dtmerge=ahapp::getParDouble("dtmerge");
  if (ahgen::strtoupper(par.m_filetype) != "REAL" && par.m_flagmerge) par.m_dtmax=ahapp::getParDouble("dtmax");
  if (par.m_filetype == "SIM" && par.m_flagmerge) par.m_maxenergy=ahapp::getParDouble("maxenergy");
  if (!have_infile) par.m_flagburst=ahapp::getParBool("flagburst");
  if (!have_infile && par.m_flagburst) par.m_ratburst=ahapp::getParDouble("ratburst");
  if (!have_infile && par.m_flagburst) par.m_tburst=ahapp::getParDouble("tburst");
  if (!have_infile && par.m_flagburst) par.m_trise=ahapp::getParDouble("trise");
  if (!have_infile && par.m_flagburst) par.m_tdecay=ahapp::getParDouble("tdecay");
  if (ahgen::strtoupper(par.m_filetype) == "REAL") par.m_rtmax=ahapp::getParDouble("rtmax");
  if (ahgen::strtoupper(par.m_filetype) == "REAL") par.m_dmmin=ahapp::getParDouble("dmmin"); 
  par.m_seed=ahapp::getParInt("seed");

  if (ahgen::strtoupper(par.m_filetype) == "REAL") { 
    AH_OUT << "Calculate branching ratios for real file " << par.m_infile << std::endl;
  } else if (ahgen::strtoupper(par.m_filetype) == "SIM") {
    AH_OUT << "Calculate branching ratios for simulated infile " << par.m_infile << std::endl;
  } else {
    AH_OUT << "Calculate branching ratios for source distribution described in " 
                         << par.m_pixfrac << ", count rate = " << par.m_countrate << ", exposure = " 
                         << par.m_exposure << std::endl;
    if (par.m_countrate * par.m_exposure > 3.6e6) {
      AH_OUT << "Branching ratios depend only on the count rate consider reducing exposure time" << std::endl;
    }
  } 

  AH_INFO(ahlog::HIGH) << "Parameters Acquired." << std::endl;
}
// ****************************************************************************

void initialize(branchlib::Par& par, ahfits::FilePtr& fpInput,
                branchlib::infileData& datIn, int (&mask_arr)[branchlib::NPIXELS], 
                double (&frac_arr)[branchlib::NPIXELS], 
                double (&cumfrac_arr)[branchlib::NPIXELS],
                int (&prox_arr)[branchlib::NPIXELS][4], 
                int (&quad)[branchlib::NPIXELS]) {

  AH_INFO(ahlog::HIGH) << "Start Initialize." << std::endl;
  
  int col1val = -1;
  int col2val = -1;
  int col1frac = -1;
  double col2frac = 0.;
  int pixnumber = -1;
  int start = 0;
  double totfrac = 0.;
  double tstart = 0.;
  std::string datetime;
  std::string ctelpixfile;

  // seed random number generator, if user provided seed is greater than zero 
  // use it, otherwise seed using the system time
  ahgen::seedRandom(par.m_seed);

  //intitialize mask_arr and frac_arr
  for (int ii = 0; ii < branchlib::NPIXELS;  ii++) {
    mask_arr[ii] = 1;
    frac_arr[ii] = 1. / (double)(branchlib::NPIXELS-1);
  }
  frac_arr[12] = 0.0;     //pixel 12 is the calibration pixel

  // +++ MCW - moved cumulative fraction (cumfrac_arr) calculation to
  // +++ MCW - end of initialize() so that the calculation is not
  // +++ MCW - repeated in pixfrac == NONE if-clause.
  
  //Open and validate event file; get the number of events and exposure time
  //connect columns to variables in the infileData struct 
  if (ahgen::strtoupper(par.m_infile) != "NONE") { 
    branchlib::open_evtfile(fpInput,datIn,par);
    tstart = ahfits::getKeyValDbl(fpInput, "TSTART");
    datetime = ahfits::getKeyValStr(fpInput, "DATE-OBS");
  }

  //+++MSD might change later possibly add parameter or use some other fixed value
  if (ahgen::strtoupper(par.m_infile) == "NONE") {
    tstart = 0.;
  }

  //obtian dt values
  branchlib::getdt(par, tstart, datetime);

  //convert dtmerge and dtmax from milliseconds to seconds
  par.m_dtmerge = branchlib::MILTOSEC * par.m_dtmerge;
  par.m_dtmax = branchlib::MILTOSEC * par.m_dtmax;

  //Check that dtmerge < dtlowmid if flagmerge == YES
  if (par.m_flagmerge) {
    if (par.m_dtmerge >= par.m_dtlowmid) {
      AH_THROW_RUNTIME("dtmerge must be smaller than dtlowmid");
    } 
    if (par.m_dtmax < par.m_dtmerge) {
      AH_THROW_RUNTIME("dtmerge must be smaller than dtmax");
    }
  }

  //Set up the mask array
  if (ahgen::strtoupper(par.m_pixmask) != "NONE") {
    AH_INFO(ahlog::HIGH) << "Setting up mask array." << std::endl;
    if (ahgen::fileExists(par.m_pixmask)) { 
      std::string linePixmask; 
      std::ifstream pixmask (par.m_pixmask.c_str());
      if (pixmask.is_open())
      {
        //Loop over the lines in the file 
        while (getline(pixmask,linePixmask))
        {
          std::istringstream lpmss(linePixmask);
          int value;
          int col = 1;
          while (lpmss >> value)
          {
            if (col == 1) col1val = value;
            if (col1val != 0 && start != 1) continue;
            if (col == 2) col2val = value;
            col++;
            start = 1;
          }
          if (start != 1) continue;
          if (start == 1 && col2val < 0) continue;
          if (start == 1 && (col2val != 0 && col2val != 1)) {
            AH_THROW_RUNTIME("Error pixmask values can only be 0 or 1, please review pixmask file");
          }
          pixnumber += 1;
          if (col1val != pixnumber){
            AH_THROW_RUNTIME("Error incorrect pixel number in pixmask file");
          } else {
            mask_arr[pixnumber] = 1-col2val;
          }//end if (col1 != ll)
        }//end while (getline(pixmask,linePixmask))
      }//end if (myfile.is_open())
      if (pixnumber != 35) { 
        AH_THROW_RUNTIME("Error not every pixel was assigned a mask value, please review pixmask file");
      }
    } else {
      AH_OUT << "pixmask file not found; using all pixels" << std::endl;
    } //end if pixmask exists
  } else {
    AH_INFO(ahlog::HIGH) << "No mask - using all pixels" << std::endl; 
  } //end if (par.m_pixmask != "NONE")      

  pixnumber = -1;
  start = 0; 

  //Set up the list of fractions of counts in each pixel
  if (ahgen::strtoupper(par.m_pixfrac) != "NONE") {
    if (ahgen::fileExists(par.m_pixfrac)) { 
      std::string linePixfrac; 
      std::ifstream pixfrac (par.m_pixfrac.c_str());
      if (pixfrac.is_open())
      {
        //Loop over the lines in the file
        while (getline(pixfrac,linePixfrac))
        {
          std::istringstream lpfss(linePixfrac);
          double value;
          double col = 1;
          while (lpfss >> value)
          {
            if (col == 1) col1frac = (int)value; 
            if (col1frac != 0 && start != 1) continue;
            if (col == 2) col2frac = value;
            col++;
            start = 1;  
          }
          if (start != 1) continue;
          if (start == 1 && col2frac < 0) continue;
          pixnumber += 1;
          if (col1frac != pixnumber){
            AH_THROW_RUNTIME("Error incorrect pixel number in pixfrac file");
          } else if (pixnumber == 12) {    // pixel 12 (calibration) gets no events
            frac_arr[pixnumber]=0.;
          } else {
            frac_arr[pixnumber] = col2frac;
          }
          totfrac = totfrac+frac_arr[pixnumber];
        } // end loop over rows
      }//end (pixfrac.is_open())
      if (pixnumber != 35) { 
        AH_THROW_RUNTIME("Error not every pixel was assigned a fraction value, please review pixfrac file");
      } 

      //Normalize frac_arr (just to be sure, should already be normalized)
      for (int ll = 0; ll < branchlib::NPIXELS; ll++) frac_arr[ll] /= totfrac;
    } else {
      AH_OUT << "pixfrac file not found; assuming uniform illumination" << std::endl;
    }
  } else {
    AH_INFO(ahlog::HIGH) << "No pixfrac file - assuming uniform array illumnation" << std::endl;  
  }// end if (par.m_pixfrac != "NONE")

  // determine cumulative probability for the distrubution of  ocunts over pixels
  cumfrac_arr[0]=frac_arr[0];
  for (int ii=1; ii < branchlib::NPIXELS; ii++) cumfrac_arr[ii]=cumfrac_arr[ii-1]+frac_arr[ii];

  //get filename of ctelpixfile
  ctelpixfile=ahmission::caldb::resolve(par.m_ctelpixfile,"pixel map","SXS","-","PIXMAP","-");
  ape_trad_set_string("ctelpixfile",ctelpixfile.c_str()); 

  //Set up the proximity matrix
  branchlib::mkproxmat(ctelpixfile, prox_arr, quad);

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 
 
  AH_INFO(ahlog::HIGH) << "Finished Initialize" << std::endl;
}

// ****************************************************************************

void doWork(ahfits::FilePtr& fpInput, ahfits::FilePtr& fpSim, branchlib::Par& par, 
            branchlib::infileData& datIn, int (&mask_arr)[branchlib::NPIXELS], 
            double (&frac_arr)[branchlib::NPIXELS],
            double (&cumfrac_arr)[branchlib::NPIXELS], 
            int (&prox_arr)[branchlib::NPIXELS][4],
            int (&quad)[branchlib::NPIXELS]) {

  double dt1 = 0.;
  double dt2 = 0.;
  double out_arr[7][branchlib::NPIXELS];
  for (int ii=0; ii < 7; ii++) {
    for (int jj=0; jj < branchlib::NPIXELS; jj++) out_arr[ii][jj]=0.;
  }
  
  long long nevents = 0;         //number of events for single pixel case
  std::vector<double> evtime;    //event time
  std::vector<int> evtpix;       //array of event pixel
  int ipix = 0;                  //current event pixel
  std::vector<int> grade;        //array containing the grade of each event
 
  double energy_ev = 0.; 

  std::vector<double> time_vec;  //holds event time for simulated data
  std::vector<int> pixel_vec;    //holds pixel number for simulated data
 
  int nn = -1;                    //counter which tracks events for 
                                  //simulated events
  std::vector<int> flag_ct;       //flag crosstalk events for 
                                  //simulator data 
  branchlib::Nevtgr nvg;          //struct which contains the number of elements
                                  //in each grade in each pixel
  branchlib::outputVars outVars;  //struct which contains variables to be writtn
                                  //to the output file
  branchlib::simevents simevt;    //struct for simulated events
  int this_pixel = 0;             //current pixel number for simulated events
  int itype_sim = 0;              //itype of simulated file
  double exposure = 0.;           //exposure time  
  int cnt_real = 0;               //Events counter for real files

  // First do the semi-analytical calculation 
  dt1 = par.m_dtmidhigh;
  dt2 = par.m_dtlowmid;
  if (dt1 != par.m_dtprimary) {
    AH_INFO(ahlog::HIGH) << "For semi-analytic calculation, dtprimary=dtmidhigh assumed; using dtmidhigh" << std::endl;
  }


  // If infile==NONE, simulate list of TIMEs and PIXELs
  if (ahgen::strtoupper(par.m_infile) == "NONE") {     
    
    //set number of events to exposure time * count rate
    nevents = (int) (floor(par.m_exposure*par.m_countrate));

    AH_OUT << "Number of events to process: " << nevents << std::endl;

    //intitalize TIME and PIXEL vectors: evtime and evtpix
    evtime.reserve(nevents);            // reserve memory of vector all at once
    evtpix.reserve(nevents);
    for (int ii=0; ii<nevents; ++ii) evtime.push_back(0.);
    for (int ii=0; ii<nevents; ++ii) evtpix.push_back(0);

    // simulate TIMEs
    if (!par.m_flagburst) {
      //set time assuming constant or sinusoidaly varying source
      branchlib::timeset(par.m_exposure,0.0,0.0,nevents,evtime);
    } else {
      // set times according to burst parameters
      branchlib::timeset_burst(nevents,par.m_exposure,par.m_ratburst,
                 par.m_tburst,par.m_trise, par.m_tdecay, evtime);
    }
    std::sort(evtime.begin(), evtime.end());    // sort times

    AH_OUT << "Time assignment complete" << std::endl;   

    AH_OUT << "Assign pixel randomly following distribution in pixfrac file" << std::endl;
    // Assign PIXEL for each TIME
    for (int jj=0; jj < nevents; jj++) {
      double x = ahgen::getRandom();     // random number between 0 and 1

      // look-up random number in cumfrac_arr to get pixel (pixel = array index)
      int idx=0;                            // index in cumfrac_arr
      while (x > cumfrac_arr[idx]) idx++;   // since the last cumfrac is 1.; idx should never go out of bounds
      evtpix[jj]=idx;
    }

  }    // end if infile == NONE

  //*****************************REAL EVENT FILE*******************************
  AH_OUT << "Start: Calculate grades and compile statistics" << std::endl;
  if (ahgen::strtoupper(par.m_filetype) == "REAL") { 
    AH_OUT << "Processing real event file with " << datIn.m_nevents << " events" << std::endl;
    //loop over the rows in filename
    for (ahfits::firstRow(fpInput); ahfits::readOK(fpInput); ahfits::nextRow(fpInput))
    {
      ++cnt_real;
      if (cnt_real % 1000 == 0) {
        AH_OUT << "Processed " << cnt_real << " events out of " << datIn.m_nevents << std::endl;
      }
      // read columns in input file (real)
      // note: ahfits connections are made with the open_evt() function
      // which is called from initialize()
      ahfits::readRow(fpInput);

      //set pixel number 
      ipix = datIn.m_pixel;

      //check status column
      nvg.m_ntotal[ipix] += 1; 
      if (datIn.m_itype == 7) { 
        nvg.m_nrj[ipix] += 1; 
      } else if (datIn.m_itype == 6) { 
        nvg.m_nlo[ipix] += 1; 
        nvg.m_ncounts[ipix] += datIn.m_el_lost_cnt;  
      } else if (datIn.m_itype == 5) {
        nvg.m_nbl[ipix] += 1;  
      } else if (datIn.m_quick_double == 1) {
        nvg.m_nquick[ipix] += 1;
        nvg.m_ncounts[ipix] += 2;
      } else if (branchlib::readStatus(datIn.m_status) == "CROSSTALK") {
        nvg.m_nct[ipix] += 1; 
        switch (datIn.m_itype) {
          case 0:
            nvg.m_nhp_ct[ipix] += 1;
            break;
           case 1: 
            nvg.m_nmp_ct[ipix] += 1;
            break;
          case 2:
            nvg.m_nms_ct[ipix] += 1;
            break;
          case 3: 
            nvg.m_nlp_ct[ipix] += 1;
            break;
          case 4:
            nvg.m_nls_ct[ipix] += 1;
            break;
          default:
            AH_THROW_RUNTIME("event grade not recognized"); 
        }

      } else if ((datIn.m_slope_differ == 1) ||
                 (datIn.m_rise_time > par.m_rtmax) ||
                 (datIn.m_deriv_max < par.m_dmmin) || 
                 (branchlib::readStatus(datIn.m_status) == "BAD")) {
        nvg.m_nbad[ipix] += 1;  
      } else {
        nvg.m_ngood[ipix] += 1; 
        nvg.m_ncounts[ipix] += 1; 
        switch (datIn.m_itype) { 
          case 0: 
            nvg.m_nhp[ipix] += 1; 
            break;
          case 1: 
            nvg.m_nmp[ipix] += 1;
            break;
          case 2:
            nvg.m_nms[ipix] += 1; 
            break;
          case 3: 
            nvg.m_nlp[ipix] += 1;
            break;
          case 4: 
            nvg.m_nls[ipix] += 1;
            break;
          default:
            AH_THROW_RUNTIME("event grade not recognized");
        }//end switch(itype)
      }//end else  
    }//end loop over file

  //************************SIMULATED OR NO EVENT FILE**************************
  } else {      // simulated or no file
    if (ahgen::strtoupper(par.m_filetype) == "SIM") { 
      AH_OUT << "Processing simulated event file with " << datIn.m_nevents << " events" << std::endl;
      //merge events which are close in time
      if (par.m_flagmerge) {
        branchlib::mergeSim(fpInput, datIn, par, simevt);
        branchlib::sortSim(simevt);
      } else {     // assign pixel and fill the simevt struct
        AH_INFO(ahlog::HIGH) << "Assigning pixel number to " << datIn.m_nevents << " events" << std::endl;
        int ii = 0;
        for (ahfits::firstRow(fpInput); ahfits::readOK(fpInput); ahfits::nextRow(fpInput)) {
          ahfits::readRow(fpInput);
          ++ii; 
          if (ii % 1000 == 0) {
            AH_INFO(ahlog::HIGH) << "Assigned pixel number to " << ii << " events out of " << datIn.m_nevents << std::endl; 
          }
          this_pixel=branchlib::convertSkyXYtoPIXEL(datIn.m_x, datIn.m_y, 
                     datIn.m_roll_nom, datIn.m_tcrpx2, datIn.m_tcrpx3);
          simevt.m_time.push_back(datIn.m_time);
          simevt.m_x.push_back(datIn.m_x);
          simevt.m_y.push_back(datIn.m_y);
          simevt.m_pi.push_back(datIn.m_pi);
          simevt.m_pixel.push_back(this_pixel);
          simevt.m_pileup.push_back(0);
        }
        simevt.m_evtnum=datIn.m_nevents;
      }
      AH_OUT << "Number of simulated events before merging = " << datIn.m_nevents << std::endl;
      AH_OUT << "Number of simulated events after merging = " << simevt.m_evtnum << std::endl;
     
      //write new simulated event file with merged events
      branchlib::writeSim(fpSim, par, datIn, simevt);

      //set nevents to NAXIS2 keyword in header of simulator event file
      nevents = simevt.m_evtnum;

    } else {     // infile == NONE
      AH_OUT << "Processing event list with " << nevents << " events" << std::endl;
      if (par.m_flagmerge) {
        branchlib::mergeNone(par,nevents,evtime,evtpix,simevt);
        nevents=simevt.m_evtnum;      // number of events can change due to merging
        branchlib::sortSim(simevt);
      } else {
        for (int ii=0; ii < nevents; ii++) {
 
          simevt.m_time.push_back(evtime[ii]);
          simevt.m_pixel.push_back(evtpix[ii]);
          simevt.m_pileup.push_back(0);
          AH_DEBUG << "event " << ii << ", time = " << simevt.m_time[ii] << ", pixel = " 
                              << simevt.m_pixel[ii] << ", pile up flag = " << simevt.m_pileup[ii] << std::endl;
        }
      }
    }     // end else infile == NONE

    if ((par.m_ctphafrac1 != 0) || (par.m_ctphafrac2 != 0) ) {
      AH_OUT << "Add crosstalk events" << std::endl;
    }
    // simulate cross-talk
    int rownum = 0;
    for (int ii = 0; ii < nevents; ++ii) {
      if (ii % 1000 == 0) {
        AH_OUT << "Processed " << ii << " events out of " << nevents << std::endl;
      }
      //read columns in input file (sim)
      rownum += 1;
    
      //set event energy eV
      if (par.m_filetype == "SIM") 
        energy_ev = (double)simevt.m_pi[ii] * par.m_debin;
      else     // infile = NONE
        energy_ev = 0.;

      //set pixel number
      this_pixel = simevt.m_pixel[ii];        

      nn += 1; 
      time_vec.push_back(simevt.m_time[ii]);  
      pixel_vec.push_back(this_pixel);

      flag_ct.push_back(0);
      AH_INFO(ahlog::LOW) << "ORIGINAL EVENT: row = " << rownum << ", event number = " << nn << ", time = " 
                          << time_vec[nn] << ", pixel = " << pixel_vec[nn] << ", energy = " << energy_ev << std::endl;

      //add crosstalk events if thresholds are exceeded
      if ( par.m_ctphafrac1 > 0.) {
        if ( (par.m_filetype == "NONE") || (par.m_ctphafrac1*energy_ev > par.m_enrgthr)) {
          if ((this_pixel < 35) && (quad[this_pixel] == quad[this_pixel+1])) {
            nn += 1;
            time_vec.push_back(simevt.m_time[ii]); 
            pixel_vec.push_back(this_pixel + 1);
            flag_ct.push_back(1); //crosstalk event flag
            AH_INFO(ahlog::LOW) << "ADDING CROSSTALK EVENT TO NEXT PIXEL: row = " << rownum << ", event number = " << nn 
                                << ", time = " << time_vec[nn] << ", pixel = " << pixel_vec[nn] << ", energy = " 
                                << energy_ev << std::endl;        
  
          }  
          if ((this_pixel > 0) && (quad[this_pixel] == quad[this_pixel-1])) {
            nn += 1;
            time_vec.push_back(simevt.m_time[ii]);
            pixel_vec.push_back(this_pixel - 1);
            flag_ct.push_back(1); //crosstalk event flag
            AH_INFO(ahlog::LOW) << "ADDING CROSSTALK EVENT TO PREVIOUS PIXEL: row = " << rownum << ", event number = " << nn
                                << ", time = " << time_vec[nn] << ", pixel = " << pixel_vec[nn] << ", energy = "
                                << energy_ev << std::endl;

          }
        } //end if filetype == NONE || par.m_ctphafrac1*energy_ev > = par.m_energthr
      }  // end if par.m_ctphafrac1 > 0
      if ( par.m_ctphafrac2 > 0.) {
        if ( (par.m_filetype == "NONE") || (par.m_ctphafrac2*energy_ev > par.m_enrgthr)) {
          if ((this_pixel < 34) && (quad[this_pixel] == quad[this_pixel+2])) {
            nn += 1;
            time_vec.push_back(simevt.m_time[ii]);
            pixel_vec.push_back(this_pixel + 2);
            flag_ct.push_back(1); //crosstalk event flag
            AH_INFO(ahlog::LOW) << "ADDING CROSSTALK EVENT TO CURRENT PIXEL+2: row = " << rownum << ", event number = " << nn
                                << ", time = " << time_vec[nn] << ", pixel = " << pixel_vec[nn] << ", energy = "
                                << energy_ev << std::endl;


          }
          if ((this_pixel > 1) && (quad[this_pixel] == quad[this_pixel-2])) {
            nn += 1;
            time_vec.push_back(simevt.m_time[ii]);
            pixel_vec.push_back(this_pixel - 2);
            flag_ct.push_back(1); //crosstalk event flag
            AH_INFO(ahlog::LOW) << "ADDING CROSSTALK EVENT TO CURRENT PIXEL-2: row = " << rownum << ", event number = " << nn
                                << ", time = " << time_vec[nn] << ", pixel = " << pixel_vec[nn] << ", energy = "
                                << energy_ev << std::endl;
          }
        } // endif filetype == NONE || ctphafrac2*energy_ev >= energthr
      }  // end if par.m_ctphafrac1 > 0

    }// end loop over input file
    AH_OUT << "Event totals: " << nn+1 << " (all), " << nevents << " (original), " << nn-nevents+1 << " (crosstalk)" << std::endl;
  
    
    //initilize and assign memory to grade
    for (int ii=0; ii<=nn; ++ii) grade.push_back(0);

    //assign grades to the simulated events
    branchlib::grade(nn, time_vec, pixel_vec, par.m_dtprimary, 
                     par.m_dtmidhigh, par.m_dtlowmid, grade);

    // (SIM only) fill ITYPE column in output event file
    if (par.m_filetype == "SIM") {
      AH_INFO(ahlog::HIGH) << "Adding ITYPE to simulated event file." << std::endl;
      ahfits::Router router(fpSim);
      router.connectScalar(ahfits::e_WRITEONLY, "ITYPE", itype_sim);

      ahfits::firstRow(fpSim);
      for (int evt = 0; evt < nn; ++evt) {
        if (flag_ct[evt] == 0) {
          itype_sim = grade[evt];
          ahfits::writeRow(fpSim);
          ahfits::nextRow(fpSim);
        }
      }
    }

    //loop over events and obtain counts of events in each grade
    for (int ii = 0; ii <= nn; ++ii) {
      ipix = pixel_vec[ii];
      nvg.m_ntotal[ipix] += 1;
      if (flag_ct[ii] == 0) {
        nvg.m_ngood[ipix] += 1; 
        switch (grade[ii]) {
          case 0:
            nvg.m_nhp[ipix] += 1;
            break;
          case 1:
            nvg.m_nmp[ipix] += 1;  
            break;
          case 2:
            nvg.m_nms[ipix] += 1;
            break;
          case 3:
            nvg.m_nlp[ipix] += 1;
            break;
          case 4:
            nvg.m_nls[ipix] += 1;
            break;
          default:
            AH_THROW_RUNTIME("event grade not recognized"); 
        }
      } else {
        nvg.m_nct[ipix] += 1; 
        switch (grade[ii]) {
          case 0:
            nvg.m_nhp_ct[ipix] += 1;
            break;
          case 1: 
            nvg.m_nmp_ct[ipix] += 1;
            break;
          case 2:
            nvg.m_nms_ct[ipix] += 1;
            break;
          case 3: 
            nvg.m_nlp_ct[ipix] += 1;
            break;
          case 4:
            nvg.m_nls_ct[ipix] += 1;
            break;
          default:
            AH_THROW_RUNTIME("event grade not recognized"); 
        }
      }
    } //end loop over events

    // set number of counts = good counts
    for (int ii=0; ii < branchlib::NPIXELS; ii++) {
      nvg.m_ncounts[ipix]=nvg.m_ngood[ipix];
    }

  } //end else filetype == sim or none

  AH_INFO(ahlog::HIGH) << "The number of each type of event in each pixel, Crosstalk events will not be present for real event files" << std::endl; 
  // Output the number of each type of event in each pixel to log
  for (int ii=0; ii < branchlib::NPIXELS; ii++) {
    AH_INFO(ahlog::HIGH) << "Pixel Number: " << ii << std::endl; 
    AH_INFO(ahlog::HIGH) << "Total number of events: " << nvg.m_ntotal[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "Good events: " << nvg.m_ngood[ii] << std::endl;         
    AH_INFO(ahlog::HIGH) << "cross talk events: " << nvg.m_nct[ii] << std::endl;     
    AH_INFO(ahlog::HIGH) << "high primary events: " << nvg.m_nhp[ii] << std::endl;   
    AH_INFO(ahlog::HIGH) << "mid primary events: " << nvg.m_nmp[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "low primary events: " << nvg.m_nlp[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "mid secondary events: " << nvg.m_nms[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "low secondary events: " << nvg.m_nls[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "high primary crosstalk: " << nvg.m_nhp_ct[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "mid primary crosstalk: " << nvg.m_nmp_ct[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "low primary crosstalk: " << nvg.m_nlp_ct[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "mid secondary crosstalk: " << nvg.m_nms_ct[ii] << std::endl;
    AH_INFO(ahlog::HIGH) << "low secondary crosstalk: " << nvg.m_nls_ct[ii] << std::endl; 
    AH_INFO(ahlog::HIGH) << "********************************" << std::endl;
  }

  // Determine count rate [cts/sec]  excluding crosstalk and cal-pixel
  double rarray=0.;
  if (ahgen::strtoupper(par.m_infile) == "NONE") { 
    rarray=par.m_countrate;
    exposure = par.m_exposure;
  } else {
    rarray=datIn.m_evtrate;
    exposure = datIn.m_exposure;
  }
  
  //subtract the calbration pixel count rate from rarray
  //if the file type is real
  if (ahgen::strtoupper(par.m_filetype) == "REAL") {
    rarray = rarray - ((double)nvg.m_ntotal[12]/(double)exposure);
  }

  if (rarray <= 0) {
    AH_THROW_RUNTIME("Total count rate is less than or equal to 0 can not determine estimated branching ratio");
  } 

  long total_good_events = 0;
  for (int ii = 0; ii < branchlib::NPIXELS; ++ii) {
    total_good_events += nvg.m_ngood[ii];
  }
  
  if (total_good_events <= 0) {
    AH_THROW_RUNTIME("Total number of good events is 0, can not determine branching ratios, Aborting...");
  }

  //semi-analytic calculation results stored in out_arr 
  branchlib::branch_est_arr(rarray, par.m_pixfrac, dt1, dt2, par.m_calpixrate, 
                            par.m_ctphafrac1, par.m_ctphafrac2, frac_arr,
                            prox_arr, out_arr);

  //calculate branching ratios and other output quantities
  branchlib::calcBranch(par, exposure, out_arr, mask_arr, nvg, outVars);

  //write results to output fits file
  branchlib::writeOutput(par, frac_arr, prox_arr, outVars);

  AH_INFO(ahlog::HIGH) << "Completed calculations and output." << std::endl;

  delete datIn.m_router; 
  datIn.m_router = 0;   

}

// ****************************************************************************

void finalize(ahfits::FilePtr &fpInput, ahfits::FilePtr& fpSim, branchlib::Par& par) {

  AH_OUT << "Finished SXSBRANCH." << std::endl;

  if (ahgen::strtoupper(par.m_infile) != "NONE") {
    ahfits::close(fpInput);
  }

  if (ahgen::strtoupper(par.m_filetype) == "SIM") {
    ahfits::close(fpSim);
  }
}

// ****************************************************************************

/** @} */

/* Revision Log
 $Log: sxsbranch.cxx,v $
 Revision 1.47  2016/04/14 16:40:46  mdutka
 Adding progress meters to event processing for real and simulated data

 Revision 1.46  2016/03/22 16:37:14  mdutka
 adding ape_trad_set_string after caldb call

 Revision 1.45  2016/03/22 15:42:09  mdutka
 Adding write paramters in main()

 Revision 1.44  2015/10/14 15:03:54  mdutka
 changing handling of exposure =0 and pi = 0

 Revision 1.43  2015/10/14 13:04:01  mdutka
 checking in new version with checks for division by zero

 Revision 1.42  2015/10/02 19:52:47  mwitthoe
 sxsbranch: switch over to general CALDB query for sxsbranch pixmap file

 Revision 1.41  2015/08/19 21:03:26  mwitthoe
 sxsbranch: doxygen label fixes

 Revision 1.40  2015/08/13 18:12:31  mwitthoe
 sxsbranch: add standard tool prologue, move non-standard functions/structs from tool source to lib file; clean-up code; remove compiler warnings; remove unused functions

 Revision 1.39  2015/08/03 19:14:07  mdutka
 switching to general CALDB query routine

 Revision 1.38  2015/07/15 20:58:46  mdutka
 Adding CALDB query

 Revision 1.37  2015/07/09 18:11:42  mdutka
 updating branch, fixed issue with lack of preciasion in pixel calculation, fixed writing extra rows in sim out file as well

 Revision 1.36  2015/07/02 14:26:23  mdutka
 updating sxsbranch, crosstalk event for real files, subtracting calpix counts from rate

 Revision 1.35  2015/06/23 17:41:27  mdutka
 debugging

 Revision 1.34  2015/06/22 21:26:27  mdutka
 debuging...

 Revision 1.33  2015/06/19 20:51:31  mdutka
 checking in multiple bug fixes

 Revision 1.32  2015/06/19 13:40:22  mdutka
 fixing sign error

 Revision 1.31  2015/06/18 20:52:35  mdutka
 fixed bug with chatter statements, changed ROLL_NOM to PA_NOM

 Revision 1.29  2015/06/18 14:26:08  mdutka
 updating log information

 Revision 1.28  2015/06/17 17:05:05  mdutka
 updating sxsbranch to provide more output, check STATUS column for real files, reordered parameters

 Revision 1.27  2015/06/12 20:49:05  mdutka
 Untagged incremental progess on branch

 Revision 1.26  2015/05/28 14:54:13  mwitthoe
 sxsbranch bug-fixes: 1) change condition to read maxenergy parameter; 2) fix unit mismatch in check between dtmax and dtmerge; 3) fix output file size; 4) fix problem where queues in mergeSim were not reset if an event was rejected due to a large energy

 Revision 1.25  2015/05/27 17:28:39  mwitthoe
 sxsbranch: in writeSim(), use infile as a template for the output event file so that all the keywords are copied

 Revision 1.24  2015/05/27 14:19:34  mwitthoe
 sxsbranch: fix bug where pileup was not added when processing the last group for each pixel in mergeNone()

 Revision 1.23  2015/05/27 14:08:26  mwitthoe
 sxsbranch bug-fixes in mergeSim when processing the last group of each pixel: 1) pitot was not being initialized; 2) pileup was not being flagged

 Revision 1.22  2015/05/27 01:31:23  mwitthoe
 sxsbranch bug-fix: after merging events with infile=NONE, need to update the nevents variable

 Revision 1.21  2015/05/26 22:38:50  mwitthoe
 sxsbranch: fix bug where cross-talk was always computed for infile=NONE and filetype=SIM cases even when ctphafrac1/2 is zero

 Revision 1.20  2015/05/26 19:08:36  mwitthoe
 sxsbranch: 1) bug-fixes from code restructuring; 2) add new burst time calculation from heasim; 3) add merging and cross-talk changes for infile=NONE; 4) update mergeSim() function; 5) fix output column names and types

 Revision 1.19  2015/05/22 20:27:58  mwitthoe
 sxsbranch: change behavior when infile=NONE; from trf_sxsbranch_15-05-20

 Revision 1.18  2015/05/20 17:36:24  mdutka
 commiting incremental changes to sxsbranch, merging simulated events is implemeted

 Revision 1.17  2015/04/21 17:03:43  mdutka
 Minor change to conditional statements which are used to identify cross talk events see lines 1046 and 1060

 Revision 1.16  2015/04/20 17:39:22  mdutka
 Made change to output file, now BranchEst extension will not be created if skipcalc=yes

 Revision 1.15  2015/04/14 20:42:37  mdutka
 now using ahgen::strtoupper

 Revision 1.14  2015/04/13 20:56:32  mdutka
 Changed check on the energy threshold for real event files, now the code will simply check in the value of pi is less than zero

 Revision 1.13  2015/04/08 17:49:14  mdutka
 Updated SLOPE_DIFFER and QUICK_DOUBLE columns to 1X format

 Revision 1.12  2015/04/02 20:41:18  mdutka
 adding use of getTELESCOPString function

 Revision 1.11  2015/04/02 18:11:43  mdutka
 Made updates to sxsbranch, now clones input file and writes pixel and itype for sim data see issue #500

 Revision 1.10  2015/03/10 20:28:29  mwitthoe
 sxsbranch: fix doxygen

 Revision 1.9  2015/02/23 15:48:27  mdutka
 Adding doxygen comments

 Revision 1.8  2015/02/19 16:08:23  mdutka
 updating after bug fix with next nearest neighbhor crosstalk in branch_estimate_arr

 Revision 1.7  2015/02/19 15:49:10  mdutka
 updating after bug fix with next nearest neighbhor crosstalk in branch_estimate_arr

 Revision 1.6  2015/02/12 20:47:46  mdutka
 checking in working version of new tool sxsbranch

 Revision 1.5  2015/02/09 15:07:29  mdutka
 checking in incrmental progress on sxsbranch

 Revision 1.4  2015/02/05 22:28:18  mdutka
 checking in progress on sxsbranch

 Revision 1.3  2015/01/27 23:58:57  mdutka
 fixed multiple bugs in sxsbranch adding par file..

 Revision 1.2  2015/01/22 20:08:58  mdutka
 updating sxsbranch in process of debugging...

 Revision 1.1  2015/01/06 18:33:09  mdutka
 changed name of sxssimbranch to sxsbranch

 Revision 1.1  2014/12/22 20:46:52  mdutka
 adding new tool sxssimbranch

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
