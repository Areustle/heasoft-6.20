/// \file sxsbranchlib.cxx
/// \brief functions for sxsbranch
/// \author Mike Dutka
/// \date $Date: 2016/03/29 21:24:14 $

#define AHLABEL tool_sxssimbranch_sxssimbranchlib
#define AHCVSID "$Id: sxsbranchlib.cxx,v 1.19 2016/03/29 21:24:14 mdutka Exp $"


#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "sxsbranchlib.h"
#include "ahsxs/ahsxs.h"
#include "ahgen/ahgen.h"

#include <iomanip>
#include <stdlib.h>
#include <math.h>


namespace branchlib {

// ---------------------------------------------------------------------------

void grade(long long nevents, std::vector<double> &evtime,
           std::vector<int> &evtpix, double dtprimary, double dtmidhigh,
           double dtlowmid, std::vector<int> &grade) {

/* The grade of a single event can be determined by knowing the TIME
   of the previous and next events from the same PIXEL.  Therefore, we will
   always hold three events in memory for each pixel and determine the grade
   of the middle of the three events. */

  //Local Variables
  long long row1[36];    //array to hold row number for first point per pixel
  double time1[36];      //array to hold TIME for first point per pixel
  long long row2[36];    //array to hold row number for second point per pixel
  double time2[36];      //array to hold TIME for second point per pixel

  AH_OUT << "Assign grades to all events based on time intervals between events in each pixel, using dtprimary = " 
         << dtprimary << ", dtmidhigh = " << dtmidhigh << " dtlowmid = " << dtlowmid << std::endl;

  //intialize arrays to store first point per pixel info 
  for (int ip = 0; ip < 36; ip++) {
    row1[ip]=-1;
    time1[ip]=-1;
  } 

  //initialize arrays to store second point per pixel info 
  for (int ip = 0; ip < 36; ip++) {
    row2[ip]=-1;
    time2[ip]=-1;
  } 

  //iterate through the events to assign grade
  for (int ii = 0; ii < nevents; ii++) {
    //If point 1 is undefined, set to current event and continue loop
    if (row1[evtpix[ii]] < 0) {
      row1[evtpix[ii]]=ii;
      time1[evtpix[ii]]=evtime[ii];
      continue;
    }

    //If point 2 is undefined; set to current event and continue
    //once second event within a pixel is recorded we can 
    //determine the grade of the first event, we make up a time 
    //before the first event in order to assure it is always assigned 
    //as a primary event.
    if (row2[evtpix[ii]] < 0) {
      row2[evtpix[ii]] = ii;
      time2[evtpix[ii]]=evtime[ii];
      
      //assign grade to first events
      grade[row1[evtpix[ii]]]=ahsxs::calcGrade(time1[evtpix[ii]]-
                                  2*dtprimary, time1[evtpix[ii]], 
                                  time2[evtpix[ii]],dtprimary,dtlowmid,
                                  dtmidhigh);
      continue;
    }  

    //Now we have three events occuring within a pixel and we can assign grades
    //to events which are not the first or the last event within a pixel
    grade[row2[evtpix[ii]]] = ahsxs::calcGrade(time1[evtpix[ii]], 
                                  time2[evtpix[ii]],evtime[ii],dtprimary,
                                  dtlowmid,dtmidhigh);

    //shift stored events
    row1[evtpix[ii]]=row2[evtpix[ii]];
    time1[evtpix[ii]]=time2[evtpix[ii]];
    row2[evtpix[ii]]=ii;
    time2[evtpix[ii]]=evtime[ii];

  }//end of loop over events 

  //assign grades for the last event of each pixel 
  //such that they are always primary
  for (int ii = 0; ii < 36; ++ii) {
    if (row2[ii] != -1) { 
      grade[row2[ii]]=ahsxs::calcGrade(time1[ii],time2[ii],
                                       time2[ii] + 2*dtmidhigh,
                                       dtprimary,dtlowmid,dtmidhigh);
    }
  } 

}

// ---------------------------------------------------------------------------

  int convertSkyXYtoPIXEL(double xsky_pix, double ysky_pix, double roll_pnt, 
                          double crpxx_sky, double crpxy_sky) {

  const double d2r = 0.01745329252;          //radians per degree
  const double focal_length_m = 5.6;         //focal length of sxs instrument   
  const double pixelsize_detx_mic = 832.0;   //pixel x-dimension in microns
  const double pixelsize_dety_mic =  832.0;  //pixel y-dimension in microns
  const double numberpixels_detx = 6;        //number of detector pixels in x-dir
  const double numberpixels_dety = 6;        //number of detector pixels in y-dir
  // plate scale degree/mm
  const double plate_scale = (360.0 / (2.0*M_PI)) / (1.0e3 * focal_length_m);     
  //degrees/det-pixel
  const double cdltx_det = -1e-03 * plate_scale * pixelsize_detx_mic; 
  // degrees/det-pixel
  const double cdlty_det =  1e-03 * plate_scale * pixelsize_dety_mic; 
  // pixel id map; 
  int pixel_ids[36] = {5,6,8,16,14,12,3,4,7,15,13,11,1,2,0,17,10,9,27,28,
                       35,18,20,19,29,31,33,25,22,21,30,32,34,26,24,23};
  double tcldt2 = -0.000491106681541;
  double tcldt3 = 0.000491106681541;
  double xsky = 0.;
  double ysky = 0.; 
  double xsky_rel = 0.;
  double ysky_rel = 0.;
  double rolldeg = 0.;
  double xroll = 0.;
  double yroll = 0.;
  double resampx = 0.;
  double resampy = 0.;
  double xrollres = 0.;
  double yrollres = 0.; 
  double signx = 0.;
  double signy = 0.;
  double xrollres_ran = 0.;
  double yrollres_ran = 0.;
  int ixdet = 0;
  int iydet = 0;
  int iddet = 0;
  int pixelnum = 0; 

  //convert discrete sky pixel to continous sky by randomizing; note
  //these coordinates are "look-up"
  xsky = xsky_pix - 0.5 + (ahgen::getRandom());
  ysky = ysky_pix - 0.5 + (ahgen::getRandom());

  //convert to sky coordinates relative to referent pixel; now the 
  //coordinates are look down
  xsky_rel = (crpxx_sky - xsky);
  ysky_rel = (ysky - crpxy_sky);

  //roll to detector/focal plane --still "look-down"
  rolldeg = -d2r * roll_pnt; //convert to radians
  xroll = xsky_rel * cos(rolldeg) + ysky_rel * sin(rolldeg);
  yroll = ysky_rel * cos(rolldeg) - xsky_rel * sin(rolldeg);

  //resample onto grid with detector pixel size; still "look-down"
  resampx = cdltx_det / tcldt2;
  resampy = cdlty_det / tcldt3;
  xrollres = xroll / resampx;
  yrollres = yroll / resampy;

  //re-randomize in detector pixel ML: Not sure if this is neccesary
  signx = copysign(1.0,xroll);
  signy = copysign(1.0,yroll);
  xrollres_ran = signx * (((int)(fabs(xrollres))) + (ahgen::getRandom()));
  yrollres_ran = signy * (((int)(fabs(yrollres))) + (ahgen::getRandom()));

  // pixel(x,y) indicies, where (1,1) is look-down lower left pixel
  //                            (6,1) is look-down lower-right pixel
  //                            (1,6) is look-down upser-left pixel
  //                            (6,6) is look-down upper-right pixel
  ixdet = int(xrollres + 0.5 + 0.5 * (numberpixels_detx + 1));
  iydet = int(yrollres + 0.5 + 0.5 * (numberpixels_dety + 1));
  
  if (ixdet == 0) ixdet = 1;
  if (ixdet == 7) ixdet = 6;
  if (iydet == 0) iydet = 1;
  if (iydet == 7) iydet = 6;


  if (ixdet < 1 || ixdet > 6 || iydet < 1 || iydet > 6) {
    AH_THROW_RUNTIME("Attempting to assign non-existent pixel");
  }   

  //unique pixel identifier (0: lower-left pixel, 5: lower-right,
  //30:upper-right, 35:upper-left):
  iddet = numberpixels_detx*(iydet-1)+(ixdet-1);

  //use pixel id map to get pixel detector channel number
  pixelnum = pixel_ids[iddet];
 
  //Note: A BASIC ASSUMPTION HERE IS THAT THE REFERENCE PIXEL REFERS TO 
  //# THE CENTER OF THE SXS ARRAY  -- THIS IS THE CASE FOR (HEA)SIMULATED SXS 
  //EVENT FILES USING THE CURRENT VERSION OF heasim.mdb.
  //  if (idx == 20508887) {
  //  std::cout << "X = " << xsky_pix << std::endl;
  //  std::cout << "Y = " << ysky_pix << std::endl;
  //  std::cout << "roll = " << roll_pnt << std::endl;
  //  std::cout << "tcldt2 = " << tcldt2_local << std::endl;
  //  std::cout << "tcldt3 = " << tcldt3_local << std::endl;
  //  std::cout << "crpxx = " << crpxx_sky << std::endl;
  //  std::cout << "crpxy = " << crpxy_sky << std::endl; 
  //  std::cout << "iydet = " << iydet << std::endl;
  //  std::cout << "ixdet = " << ixdet << std::endl;
  //  std::cout << "iddet = " << iddet << std::endl;
  //  std::cout << "r1 = " << r1 << std::endl;
  //  std::cout << "xsky = " << xsky << std::endl;
  //  std::cout<< std::setprecision(15) << "r2 = " << r2 << std::endl;
  //  std::cout << "ysky = " << ysky << std::endl;
  //  std::cout << "r3 = " << r3 << std::endl;
  //  std::cout << "xrollres_ran = " << xrollres_ran << std::endl;
  //  std::cout << "r4 = " << r4 << std::endl;
  //  std::cout << "yrollres_ran = " << yrollres_ran << std::endl;
  //  std::cout << "pixelnum = " << pixelnum << std::endl; 
 // }

  return pixelnum;
} 

// ---------------------------------------------------------------------------

int find_index(double pos, int nvec, double* vec) {

    int jl=0;  /* lower index */
    int ju=0;  /* upper index */
    int jm=0;  /* middle index */

    jl=0;
    if (pos <= vec[0])
        return 0;

    ju=nvec;
    if (pos >= vec[nvec-1])
        return nvec-1;

    while (ju-jl > 1){
        jm = (ju+jl)/2;
        if (pos <= vec[jm]){
            ju=jm;
        }else
            jl=jm;
    }
    return ju;
}

// ---------------------------------------------------------------------------

int timeset_burst(int nn, double expose,double ratburst,double tburst,
                  double trise,double tdecay,std::vector<double>& evt_time) {

  double small_number = 1.0e-9;
  int ii = 0;

  AH_INFO(ahlog::HIGH) << "Setting event times according to burst parameters" << std::endl;
  // make output vector the correct size and initialize
  evt_time.resize(nn,0.);

  if (ratburst < small_number) {
    /* constant */
    for (ii=0; ii<nn; ii++) 
      evt_time[ii] = expose * ahgen::getRandom();

  } else {
    double tpeak = tburst + trise;  /* time of peak = beginning plus rise time */

    /* normalize timescales to exposure time to simplify computation */
    double tb_norm = tburst / expose;
    double tr_norm = trise / expose;
    double td_norm = tdecay / expose;
    double tp_norm = tpeak / expose;
    double tm_norm = 0.0;

    /* normalizations and factors */
    double norm0 = 1.0;
    double fac1 = 0.5 * ratburst  / tr_norm;
    double norm1 = 1.0 + fac1 * (1.0 - tb_norm) * (1.0 - tb_norm); 
    double fac2a = 0.5 * ratburst  * tr_norm;
    double fac2b = ratburst * td_norm; 
    double norm2 = (1.0 + fac2a) + fac2b * (1.0 - exp(-((1-tp_norm)/td_norm)));

    int ntable=1000;
    double dtn0 = 1.0 / (double)ntable; /* mean (normalized) interval */

    /* initialize: constant intervals */
    int ibeg1 = 0;
    int iend1 = ntable;
    int ibeg2 = ntable;
    int iend2 = ntable;
    int ibeg3 = ntable;
    int iend3 = ntable;
    int ibeg4 = ntable;
    int iend4 = ntable;
  
    double dtn1 = dtn0;
    double dtn2 = 0.0;
    double dtn3 = 0.0;
    double dtn4 = 0.0;
    double dtn10 = 0.0;
    double dtn20 = 0.0;
    double dtn30 = 0.0;

    double tfac = 0.0;

    double n10 = 0.0;
    //double n20 = 0.0;     // for debugging
    double n30 = 0.0;
    double n40 = 0.0;

    int n1 = 0;
    int n2 = 0;
    int n3 = 0;
    int n4 = 0;

    double norm = norm0;
    double * dt_table = 0;
    double * table_time = 0;
    double * table_x = 0;
    long itim = 0;

    if ( (tb_norm < 1.0) && (ratburst != 0.0) ) { /* burst within exposure */
      if (tp_norm >= 1.0){
        norm = norm1; /* decay outside of exposure; resolve risetime */
        tfac = (1.0-tb_norm) * dtn0;
        dtn10 = tb_norm * dtn0 + tfac * dtn0 / tr_norm;
        dtn20 = tb_norm * tr_norm + tfac;
        if (dtn20 > dtn0) dtn20 = dtn0;
        n10 = tb_norm / dtn10;
        //n20 = (1.0-tb_norm) / dtn20;
        n1 = floor(n10);
        n2 = ntable - n1;
        dtn2 = dtn20;
        dtn1 = (1 - (double)n2 * dtn20) / (double)n1;
        ibeg1 = 0;
        iend1 = n1;
        ibeg2 = n1;
        iend2 = ntable;
      }  else {
        norm = norm2;
        tm_norm = tp_norm + td_norm * log(ratburst); /* natural log*/
        if (tm_norm < tp_norm) tm_norm = tp_norm;
        if (tm_norm > 1) { /* slow decay; resolve risetime */
            tfac = dtn0 + (1.0-tp_norm);
                        dtn10 = tb_norm * dtn0 + dtn0 * dtn0 + tfac * dtn0 / td_norm;
                        dtn20 = tb_norm * tr_norm + dtn0 * tr_norm + tfac * tr_norm / td_norm;
                        dtn30 = tb_norm * td_norm + dtn0 * td_norm + tfac;
            if (dtn20 > dtn0) dtn20 = dtn0;
            if (dtn30 > dtn0) dtn30 = dtn0;
                        n10 = tb_norm / dtn10;
                        //n20 = tr_norm / dtn20;
            n30 = (1.0-tp_norm) / dtn30;
            n1 = floor(n10);
            n3 = floor(n30);
                        n2 = ntable - n1 - n3;
                        dtn2 = dtn20;
                        dtn3 = dtn30;
            dtn1 = (1 - (double)n2 * dtn20 - (double)n3 * dtn30) / (double)n1;
                        ibeg1 = 0;
                        iend1 = n1; 
                        ibeg2 = n1;
                        iend2 = n1+n2; 
                        ibeg3 = n1+n2;
                        iend3 = ntable; 
        } else { /* resolve risetime and decay*/
                        double ttemp = tb_norm + 1.0 - tm_norm;
            tfac = (tm_norm-tp_norm) * dtn0; 
                        dtn10 = ttemp * dtn0 + dtn0 * dtn0 + tfac * dtn0 / td_norm;
                        dtn20 = ttemp * tr_norm + dtn0 * tr_norm + tfac * tr_norm / td_norm;
                        dtn30 = ttemp * td_norm + dtn0 * td_norm + tfac;
            if (dtn20 > dtn0) dtn20 = dtn0;
            if (dtn30 > dtn0) dtn30 = dtn0;
                        n10 = tb_norm / dtn10;
                        //n20 = tr_norm / dtn20;
            n30 = (tm_norm-tp_norm) / dtn30;
            n40 = (1.0-tm_norm) / dtn10;
            n1 = floor(n10);
            n4 = floor(n40);
            n3 = floor(n30);
                        n2 = ntable - n1 - n3 - n4;
                        dtn2 = dtn20;
                        dtn3 = dtn30;
            dtn1 = (1 - (double)n2 * dtn20 - (double)n3 * dtn30) / (double)(n1+n4);
            dtn4 = dtn1;
                        ibeg1 = 0;
                        iend1 = n1; 
                        ibeg2 = n1;
                        iend2 = n1+n2;
                        ibeg3 = n1+n2;
                        iend3 = n1+n2+n3; 
                        ibeg4 = n1+n2+n3;
                        iend4 = ntable;
        }  /* endif (tm_norm > 1) */
      } /* endif (tp_norm >= 1.0) */
    } /* endif  ((tb_norm < 1.0) && (ratburst != 0.0)) */

    /* Populate dt_table */
    dt_table = (double*)calloc(ntable, sizeof(double));
    for (ii=0; ii<ntable; ii++){
        if ( (ii >= ibeg1) && (ii < iend1) ){
      dt_table[ii] = dtn1;
        } else if ( (ii >= ibeg2) && (ii < iend2) ){
      dt_table[ii] = dtn2;
        } else if ( (ii >= ibeg3) && (ii < iend3) ){
                  dt_table[ii] = dtn3;
        } else if ( (ii >= ibeg4) && (ii < iend4) ){
                  dt_table[ii] = dtn4;
        } else {
      printf("ERROR: this case of dt_table population should not happen!\n");
      free(dt_table);
      return -1;
        }
    }

    /* lookup table, mapping uniform distribution (table_x) onto lightcurve (table_time) */
  
    /* table size is ntable +1 */
    table_time = (double*)calloc( (ntable+1), sizeof(double));
    table_x = (double*)calloc ( (ntable+1), sizeof(double));
  
    table_time[0] = 0.0;
    for (int itable=1; itable<=ntable; itable++){
        table_time[itable] = table_time[itable-1] + dt_table[itable-1]; 
        double tnorm = table_time[itable]; 
        if ((tnorm <= tb_norm) || (ratburst == 0.0)) { /* before the burst*/
      table_x[itable] = tnorm;
        } else {
      if (tnorm <= tp_norm) { /* before the peak*/
          table_x[itable] = tnorm + fac1*(tnorm - tb_norm)*(tnorm - tb_norm);
      } else {
          double facx = 1.0 - exp(-((tnorm-tp_norm)/td_norm));
          table_x[itable] = (tnorm+fac2a) + fac2b *facx;
      }
        }   
        table_x[itable] /= norm;
    } /* endfor itable=0,ntable */
  
    /* time assignment: */
    itim = 0;
    while (itim < nn) {
        /* random number on uniform unit interval: */
        double x = ahgen::getRandom();
        int table_ind = branchlib::find_index(x, ntable+1, &table_x[0]);   // &table_x[0] -> pointer to first element allows find_index to operate on an array instead of std::vector
        if (table_ind < 1) AH_THROW_RUNTIME("Table index out of bounds");
        double weight1 = (table_x[table_ind] - x)/(table_x[table_ind] - table_x[table_ind-1]);
        double weight2 = (x - table_x[table_ind-1])/(table_x[table_ind] - table_x[table_ind-1]); 
        double time_int =  table_time[table_ind] * weight2 + table_time[table_ind-1] * weight1;
         if (time_int <= 1.0) {
       evt_time[itim] = expose * time_int;
       itim++;
         } else { 
       continue;
         }
    } /* end while loop */
    
    free(dt_table);
    free(table_time);
    free(table_x);

  } /* end constant/burst conditional */

    return 0;
}

// ---------------------------------------------------------------------------

void getdt(branchlib::Par& par, const double tstart, std::string datetime) {

  //Parameter needed for caldb query
  std::string dtfile;                 //filename of dtfile is numerical value isn't provided
  std::string filetype = "delta-t";   //short description of caldb file
  std::string instrume = "SXS";
  std::string detnam = "-";
  std::string codename = "DELTIMES";

  //Determine the dt timescales from the initial values for interval times;  
  //negative implies value comes from "CALDB"

  par.m_dtprimary = -1.;
  par.m_dtmidhigh = -1.;
  par.m_dtlowmid = -1.;

  //Try to convert each time interval parameter to a double; if it fails
  //Then assume the parmeters represent a file name.

  if (ahgen::isNumber(par.m_dtmidhigh_str)) {
    //set value of dtmidhigh (double) and convert from ms to sec 
    par.m_dtmidhigh = 0.001*atof(par.m_dtmidhigh_str.c_str());
    if (par.m_dtmidhigh <= 0.) {
      AH_THROW_RUNTIME("dtmidhigh parmater must be greater than zero");
    }
  } else {
    dtfile = par.m_dtmidhigh_str;
  }

  if (ahgen::isNumber(par.m_dtprimary_str)) {
    //set value of dtmidhigh (double) and convert from ms to sec
    par.m_dtprimary = 0.001*atof(par.m_dtprimary_str.c_str());
    if (par.m_dtprimary <= 0.) {
      AH_THROW_RUNTIME("dtmidhigh parmater must be greater than zero");
    }
  } else { 
    if (dtfile != "" && par.m_dtprimary_str != dtfile) {
      AH_THROW_RUNTIME("time interval CALDB file names must be consistent");
    } else {
      dtfile = par.m_dtprimary_str;
    }
  }

  if (ahgen::isNumber(par.m_dtlowmid_str)) {
    //set value of dtmidhigh (double) and convert from ms to sec
    par.m_dtlowmid = 0.001*atof(par.m_dtlowmid_str.c_str());
    if (par.m_dtlowmid <= 0.) {
      AH_THROW_RUNTIME("dtlowmid parmater must be greater than zero");
    }
  } else { 
    if (dtfile != "" && par.m_dtlowmid_str != dtfile) {
      AH_THROW_RUNTIME("time interval CALDB file names must be consistent");
    } else {
      dtfile = par.m_dtlowmid_str;
    }
  }

  //Get time intervals from "CALDB" file if necessary
  //Note: time in CALDB file are in seconds
  if (dtfile != "") {

    //resolve dtfile name and query CALDB if neccessary
    dtfile = ahmission::caldb::resolve(dtfile, filetype, instrume, detnam, codename, datetime);

    ahsxs::sxsdt::SXSTimeIntervals dts;    // store CALDB information
    ahsxs::sxsdt::loadSXSTimeIntervals(dtfile,tstart,dts);
    if (par.m_dtmidhigh < 0.) par.m_dtmidhigh=dts.m_dtmidhigh;
    if (par.m_dtprimary < 0.) par.m_dtprimary=dts.m_dtprimary;
    if (par.m_dtlowmid < 0.) par.m_dtlowmid=dts.m_dtlowmid;
  }

  // Check that dtlowmid < dtmidhigh
  if (par.m_dtlowmid >= par.m_dtmidhigh){
    AH_THROW_RUNTIME("dtlowmid must be smaller than dtmidhigh");
  }
}

// ---------------------------------------------------------------------------

void open_evtfile(ahfits::FilePtr& fpInput, 
                  branchlib::infileData & datIn, branchlib::Par& par) {
 

  //open input file
  ahfits::open(par.m_infile, "EVENTS", &fpInput);
  ahmission::checkEmptyTable(fpInput,par.m_infile);

  //setup router
  datIn.m_router = new ahfits::Router(fpInput);

  //Check TELESCOP=HITOMI and INSTRUME=SXS 
  if ((ahfits::getKeyValStr(fpInput, "TELESCOP")) != ahmission::getTELESCOPString()) {
    AH_THROW_RUNTIME("TELESCOP keyword is not HITOMI, this tool is for HITOMI data only");
  }

  if ((ahfits::getKeyValStr(fpInput, "INSTRUME")) != "SXS") {
    AH_THROW_RUNTIME("INSTRUME keyword is not SXS, this tool is for SXS data only.");
  }

  //Make connections from columns to variables stored in infileData structure 
  if (ahgen::strtoupper(par.m_filetype) == "REAL") {
    datIn.m_router->connectScalar(ahfits::e_READONLY,"TIME",datIn.m_time);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"PIXEL",datIn.m_pixel);
    datIn.m_router->connectBit(ahfits::e_READONLY,"QUICK_DOUBLE",&datIn.m_quick_double,datIn.m_num_quick_double);
    datIn.m_router->connectBit(ahfits::e_READONLY,"SLOPE_DIFFER",&datIn.m_slope_differ,datIn.m_num_slope_differ);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"PI",datIn.m_pi);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"RISE_TIME",datIn.m_rise_time);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"ITYPE",datIn.m_itype);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"DERIV_MAX",datIn.m_deriv_max);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"EL_REASON",datIn.m_el_reason);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"EL_LOST_CNT",datIn.m_el_lost_cnt);
    datIn.m_router->connectBit(ahfits::e_READONLY,"STATUS",datIn.m_status,datIn.m_num_status);
  } else if (ahgen::strtoupper(par.m_filetype) == "SIM") {
    datIn.m_router->connectScalar(ahfits::e_READONLY,"TIME",datIn.m_time);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"X",datIn.m_x); 
    datIn.m_router->connectScalar(ahfits::e_READONLY,"Y",datIn.m_y);
    datIn.m_router->connectScalar(ahfits::e_READONLY,"PI",datIn.m_pi); 
  } else {
    AH_THROW_RUNTIME("The filetype specified is not recognized");
  } 

  // Set number of events to the number of event file rows
  datIn.m_nevents = ahfits::getKeyValLLong(fpInput, "NAXIS2");

  //Read total exposure time and total number of events
  if (ahfits::keywordExists(fpInput, "EXPOSURE") && (ahfits::getKeyValDbl(fpInput,"EXPOSURE")) > 0) {
    AH_INFO(ahlog::HIGH) << "Using EXPOSURE keyword for exposure time" << std::endl;
    datIn.m_exposure = ahfits::getKeyValDbl(fpInput, "EXPOSURE");
  } else if (ahfits::keywordExists(fpInput, "ONTIME") && (ahfits::getKeyValDbl(fpInput,"ONTIME")) > 0) {
    AH_INFO(ahlog::HIGH) << "Using ONTIME keyword for exposure time" << std::endl;
    datIn.m_exposure = ahfits::getKeyValDbl(fpInput, "ONTIME");
  } else if (ahfits::keywordExists(fpInput, "LIVETIME") && (ahfits::getKeyValDbl(fpInput,"LIVETIME")) > 0) {
    AH_INFO(ahlog::HIGH) << "Using LIVETIME keyword for exposure time" << std::endl;
    datIn.m_exposure = ahfits::getKeyValDbl(fpInput, "LIVETIME");
  } else {
    double tstart=0.;
    double tstop=0.;
    if (!ahfits::keywordExists(fpInput,"TSTART") || !ahfits::keywordExists(fpInput,"TSTOP") 
        || (ahfits::getKeyValDbl(fpInput,"TSTART") - ahfits::getKeyValDbl(fpInput,"TSTOP")) >= 0) {
      AH_INFO(ahlog::HIGH) << "Using first and last values in the TIME column to determine exposure time" << std::endl;
      ahfits::firstRow(fpInput);
      ahfits::readRow(fpInput);
      tstart=datIn.m_time;
      ahfits::lastRow(fpInput);
      ahfits::readRow(fpInput);
      tstop=datIn.m_time;
    } else {
      AH_INFO(ahlog::HIGH) <<  "Using TSTOP - TSTART for exposure time" << std::endl; 
      tstart=ahfits::getKeyValDbl(fpInput,"TSTART");
      tstop=ahfits::getKeyValDbl(fpInput,"TSTOP");
    }
    datIn.m_exposure=tstop-tstart;
  }

  //compute evtrate from file
  if (datIn.m_exposure == 0) {
    AH_THROW_RUNTIME("Exposure time is zero, can not determine estimated branching ratios, Aborting...");
  }
  datIn.m_evtrate = (double)datIn.m_nevents / datIn.m_exposure;

  //for simulated event file, read keywords related to the conversion of 
  //incident sky X Y to pixel number  
  if (ahgen::strtoupper(par.m_filetype) == "SIM") { 
    datIn.m_telescop = ahfits::getKeyValStr(fpInput, "TELESCOP");
    datIn.m_instrume = ahfits::getKeyValStr(fpInput, "INSTRUME");
    datIn.m_roll_nom = ahfits::getKeyValDbl(fpInput, "PA_NOM");
    datIn.m_tcrpx2 = ahfits::getKeyValDbl(fpInput, "TCRPX2");
    datIn.m_tcrpx3 = ahfits::getKeyValDbl(fpInput, "TCRPX3");
    datIn.m_tcdlt2 = ahfits::getKeyValDbl(fpInput, "TCDLT2");
    datIn.m_tcdlt3 = ahfits::getKeyValDbl(fpInput, "TCDLT3");
  }

}

// ---------------------------------------------------------------------------

void mkproxmat(std::string ctelpixfile, int (&prox_arr)[branchlib::NPIXELS][4],
               int (&quad)[branchlib::NPIXELS]) {

  ahfits::FilePtr fppixmap = 0;
  int qpix[branchlib::NPIXELS];

  //local variables to hold column data
  int pixel;
  int spcquad;
  int spcpixel;

  //intialize prox_arr
  for (int ii = 0; ii < branchlib::NPIXELS; ii++) {
    for (int jj = 0; jj < 4; jj++) {
      prox_arr[ii][jj] = 99;
    }
  }  

  ahfits::open(ctelpixfile, "PIXMAP", &fppixmap);

  //Check TELESCOP=HITOMI and INSTRUME=SXS 
  if ((ahfits::getKeyValStr(fppixmap, "TELESCOP")) != ahmission::getTELESCOPString()) {
    AH_THROW_RUNTIME("TELESCOP keyword is not HITOMI, this tool is for HITOMI data only");
  }

  if ((ahfits::getKeyValStr(fppixmap, "INSTRUME")) != "SXS") {
    AH_THROW_RUNTIME("INSTRUME keyword is not SXS, this tool is for SXS data only.");
  } 

  if ((ahfits::getKeyValLLong(fppixmap, "NAXIS2")) != branchlib::NPIXELS) {
    AH_THROW_RUNTIME("The number of rows in the pixmap file does not match the number of sxs pixels.");
  }

  // Declare router and connect columns values to local variables   
  ahfits::Router router(fppixmap);
  router.connectScalar(ahfits::e_READONLY, "PIXEL", pixel);
  router.connectScalar(ahfits::e_READONLY, "SPCQUAD", spcquad);
  router.connectScalar(ahfits::e_READONLY, "SPCPIXEL", spcpixel); 

  ahfits::firstRow(fppixmap);
  // Loop over rows in pixmap.fits 
  for (int kk = 0; kk < branchlib::NPIXELS; kk++) {
    ahfits::readRow(fppixmap);
    ahfits::nextRow(fppixmap);
    if (pixel != kk) {
      AH_THROW_RUNTIME("pixel number does not match the row number in pixel map file"); 
    } else {
      quad[kk] = spcquad;
      qpix[kk] = spcpixel;
    }
  }
  ahfits::close(fppixmap);

  for (int kk = 0; kk < branchlib::NPIXELS; kk++) {
    for (int ll = 0; ll < branchlib::NPIXELS; ll++) {
      if ((quad[ll] == quad[kk]) && (qpix[ll] == qpix[kk]-1)) {
        prox_arr[kk][0] = ll;
      }
      if ((quad[ll] == quad[kk]) && (qpix[ll] == qpix[kk]+1)) {
        prox_arr[kk][1] = ll;
      } 
      if ((quad[ll] == quad[kk]) && (qpix[ll] == qpix[kk]-2)) {
        prox_arr[kk][2] = -ll;
      }
      if ((quad[ll] == quad[kk]) && (qpix[ll] == qpix[kk]+2)) {
        prox_arr[kk][3] = -ll;
      }
    }
  }  
}
  
// ---------------------------------------------------------------------------

void branch_est_pix(double rpixel,double& dt1,double& dt2,
                    double (&out_arr)[7][branchlib::NPIXELS])
{
  // branch_est estimates the branching ratios for a single SXS pixel. These are
  // determined using Poisson statistics, and requires two timescales: dt1 which
  // distinguishes both primary/secondary and high/mid-resolution event grades, 
  //  and dt2 which distinguishes mid/low-resolution event grade. Crosstalk may 
  //  be included using the ctrat parameter – the ratio crosstalk-events / 
  //  real-events, where the “real-event” rate is given by the rpixel input 
  //  parameter.  
  
  // Initialize rate arrays
  double mu_mh = dt1 * rpixel;            //expected counts in interval dtmidhigh
  double mu_lm = dt2 * rpixel;            //expected counts in interval dtlowmid
  double p_mh = exp(-mu_mh);              //probability of inter-event interval > dtmidhigh
  double p_lm = exp(-mu_lm);              //probabilty of inter-event interval > dtlowmid
  double dp = p_lm - p_mh;                //probability of inter-event interval between 
                                          //dtlowmid and dtmidhigh

  double omplm = 1.0 - p_lm;              //probabilty of inter-event interval < dtlowmid 
  double branch_hp =  p_mh * p_mh;        //expected HP branching 
  double branch_mp =  p_mh * dp;          //expected MP branching 
  double branch_lp =  p_mh * omplm;       //expected LP branching  
  double branch_ms =  dp * p_lm;          //expected MS branching 
  double branch_ls =  dp * omplm + omplm; //expected LS branching 

  out_arr[0][0] = rpixel;
  out_arr[1][0] = branch_hp;
  out_arr[2][0] = branch_mp;
  out_arr[3][0] = branch_lp;
  out_arr[4][0] = branch_ms;
  out_arr[5][0] = branch_ls;
  out_arr[6][0] = 0.0;

} 

// ---------------------------------------------------------------------------

void branch_est_arr(double& rarray, std::string pixfrac, double& dt1, double& dt2, 
                    double& rate_calpix, double ctphafrac1, double ctphafrac2, 
                    double * frac_arr, int (&prox_arr)[branchlib::NPIXELS][4], 
                    double (&out_arr)[7][branchlib::NPIXELS]){

  // branch_est_arr estimates the branching ratios for the entire SXS array or 
  // a subarray (specified by mask_arr),  for a given full-array count rate  
  // (rate_array), count-rate-distribution over the pixels (specified by 
  // frac_arr), and cal-pixel count rate (rate_calpix). These are determined 
  // using Poisson statistics, and requires two timescales: dt1 which 
  // distinguishes both primary/secondary and high/mid-resolution event grades,
  // and dt2 which distinguishes mid/low-resolution event grade. The cases of 
  // no crosstalk, nearest-neighbor electrical crosstalk, and nearest+next-
  // nearest-neighbor crosstalk are computed, where proximity is determined by 
  // the SXS wiring configuration (information encapsulated in prox_arr matrix).

  double rate_pixel[branchlib::NPIXELS];
  for (int ii=0; ii < branchlib::NPIXELS; ii++) rate_pixel[ii]=0.;
  double rpix = 0.;      //rate in current pixel
  double ctrpix = 0.;    //rate including crosstalk
  int ctindex = 0;       //cross talk index
  double rate_ct = 0.;   //cross talk rate alone 
  double mu_mh = 0.;     //expected counts in interval dtmidhigh 
  double mu_lm = 0.;     //expected counts in interval dtlowmid
  double p_mh = 0.;      //probability of inter-event interval > dtmidhigh 
  double p_lm = 0.;      //probability of inter-event interval > dtlowmid
  double dp = 0.;        //probability of inter-event interval between dtlowmid
                         //and dtmidhigh
  double omplm = 0.;     //probabilty of inter-event interval < dtlowmid

  //branching ratios
  double branch_hp = 0.; //expected HP branching
  double branch_mp = 0.; //expected MP branching
  double branch_lp = 0.; //expected lp branhcing
  double branch_ms = 0.; //expected ms branching
  double branch_ls = 0.; //expected ls branching

  AH_OUT << "Calculate branching ratios per pixel estimates based on Poisson statistics assuming count rate "
         << rarray << " distributed as specified in " << pixfrac << ".  Grades based on dtprimary = dtmidhigh = " 
         << dt1 << " dtlowmid = " << dt2 << " and rate in calibration pixel 12 of " << rate_calpix << "." << std::endl; 

  //begin loop over pixels
  for (int ii=0; ii<branchlib::NPIXELS; ii++) { 
    if (ii != 12) { 
      rate_pixel[ii] = rarray * frac_arr[ii];
    } else { 
      rate_pixel[ii] = rate_calpix;
    }
  }//end loop over pixels

  //begin loop over pixels
  for (int ii = 0; ii<branchlib::NPIXELS; ii++) {
    rpix = rate_pixel[ii];
    ctrpix = rpix;
    if (ctphafrac1 != 0) {
      for (int mm=0; mm<2; mm++) {
        if ((prox_arr[ii][mm] != 99) && (prox_arr[ii][mm] >= 0)) {
          ctindex = prox_arr[ii][mm];
          ctrpix = ctrpix + rate_pixel[ctindex];
        }
      }//end loop mm
    }//end if ctphafrac1 != 0
    if (ctphafrac2 != 0) {
      for (int mm=2; mm<4; mm++) {
        if ((prox_arr[ii][mm] != 99) && (prox_arr[ii][mm] <= 0)) {
          ctindex = -prox_arr[ii][mm];
          ctrpix = ctrpix + rate_pixel[ctindex];
        }
      }//end loop mm
    } //end if ctphafrac2 != 0

    //compute branching ratios
    mu_mh = dt1 * ctrpix;
    mu_lm = dt2 * ctrpix;
    p_mh = exp(-mu_mh); 
    p_lm = exp(-mu_lm);
    dp = p_lm - p_mh;  
    omplm = 1.0 - p_lm;
    branch_hp = p_mh * p_mh;
    branch_mp = p_mh * dp;
    branch_lp = p_mh * omplm;
    branch_ms = dp * p_lm;
    branch_ls = dp * omplm + omplm;

    //store branching ratios for each pixel in out_arr
    out_arr[0][ii] = rpix;
    out_arr[1][ii] = branch_hp;
    out_arr[2][ii] = branch_mp;
    out_arr[3][ii] = branch_lp;
    out_arr[4][ii] = branch_ms;
    out_arr[5][ii] = branch_ls;
    rate_ct = ctrpix-rpix;
    out_arr[6][ii] = rate_ct;
  }//end loop over pixels
}

// ---------------------------------------------------------------------------

void timeset(double& expose, double period, double pulsed, long long& nn, 
             std::vector<double> &evtime) {

  // timeset assigns times, according to the temporal characteristics of a
  // source, to nn events and returns these in a one-dimensional array (evtime) 
  // of size nn. Currently, only constant and sinusoidally varying sources are 
  // supported (future enhancements could include an option for a user-input 
  // lightcurve, and consideration of the time resolution of detector). Loosely 
  // adapted from routine in quicksim_subs.c, but replaces the algorithm with 
  // one that uses “sinusoidal” deviates (following numerical recipes, and 
  // similar to what is used in sourcedis), and can be generalized to any 
  // differentiatable variation. The new algorithm has been tested (albeit, 
  // in python). 

  //declare constant values
  const double small_number = 1.0e-9;     //threshold for pulsed component
  const double TWOPI = M_PI * 2.;         // 2 * pi
  const ahfits::IndexType NTABLE = 1000;  //number of elements in look up table

  double omega = 0.;                      //angular frequency
  double phase_in =0.;                    //initial phase
  double cosphase = 0.;                   //cosine of initial phase  
  double table_time[NTABLE] = {0.};       //lightcurve
  double table_x[NTABLE] = {0.};          //lookup table which maps a distribution onto the lightcurve
  double table_phase = 0.;                //current phase
  int ncycle = 0;                         //number of cycles included within exposure time
  double icycle = 0.;                     //random number between 1 and ncycle
  ahfits::IndexType table_ind = 0;        //index of random value in lookup table
  double weight1 = 0.;                    //weight for interpolation   
  double weight2 = 0.;                    //weight for interpolation
  double time_int = 0.;                   //interpolated time
  double dt_table = 0.;                   //time unit associated with each element of look up table
  bool extrap = true;                     //boolean value for the ahmath::search function 
                                          //extrap true if x not in range of x arr

  AH_OUT << "Setting event times randomly within the exposure." << std::endl;

  if (period == 0.0 || pulsed < small_number) {
    //source has constant brightness
    for (int ii=0; ii<nn; ii++) {
      evtime[ii] = expose*(ahgen::getRandom());
    }
  } else {

    // source is sinusoidally varying

    //set angular frequency
    omega = TWOPI/period; 

    //set the number of cycles included within the exposure time
    ncycle = floor(expose/period) + 1;

    //The initial phase is set to be random
    phase_in = TWOPI*(ahgen::getRandom());
    cosphase = cos(phase_in);

    //look up table, mapping uniform distribution (table_x) onto 
    //lightcurve (table_time) for a single cycle

    //table size is ntable+1
    dt_table=period/float(NTABLE);
    for (int itable=0; itable<=NTABLE; itable++) {
      table_time[itable] = dt_table*float(itable);
      table_phase=omega*table_time[itable];
      table_x[itable] = (table_phase + pulsed*(cosphase-cos(table_phase+
                        phase_in))) / TWOPI;
    }

    //Time assignment for sinusiodally varying source
    int itim = 0; 
    while (itim < nn) {
      //random cycle

      //generate random number between 1 and ncycle
      icycle = ahgen::getRandomInt(1, ncycle);

      //genrate random number between 0 and 1
      double x = ahgen::getRandom();

      //get the index of the element in table_x which has the 
      //closest value to x   
      table_ind = ahmath::search(x,table_x,NTABLE,extrap,table_ind);

      weight1 = (table_x[table_ind]-x)/(table_x[table_ind]-table_x[table_ind+1]);
      weight2 = (x-table_x[table_ind+1])/(table_x[table_ind]-table_x[table_ind+1]);

      time_int = (table_time[table_ind]*weight2)+(table_time[table_ind+1]*weight1);

      time_int = period*(icycle-1)+time_int;

      if (time_int<=expose) {
        evtime[itim]=time_int;
        itim=itim+1;
      } else {
        continue;
      }
    }//end while itim<nn
  }//end constant/periodic conditional     
} 

// ---------------------------------------------------------------------------

void mergeSim(ahfits::FilePtr& fpInput, branchlib::infileData& datIn,
              branchlib::Par& par, branchlib::simevents& simevt) {

  //queues to store each merged groups information
  std::vector<double> q_time[branchlib::NPIXELS];
  std::vector<long> q_x[branchlib::NPIXELS];
  std::vector<long> q_y[branchlib::NPIXELS];
  std::vector<long> q_pi[branchlib::NPIXELS];

  double q_tottime[branchlib::NPIXELS];
  for (int ii=0; ii < branchlib::NPIXELS; ii++) q_tottime[ii]=0;
  int q_nev[branchlib::NPIXELS]; //number of events currently in the queue for each pixel; initially 0
  for (int ii=0; ii < branchlib::NPIXELS; ii++) q_nev[ii]=0;
  int qnev = 0;                  //location in queue
  int this_pixel = 0;            //current pixel simulated data
  double dt = 0.;                //time to most recently added evt in queue
  long xtot = 0.;
  long ytot = 0.;
  long pitot =0.;
  int cnt = 0;

  AH_OUT << "Merge simulated events arriving in same pixel within dtmerge = " << par.m_dtmerge <<
            " up to a maximum dtmax = " << par.m_dtmax << " discard event with energy above maxenergy = " <<  
            par.m_maxenergy << std::endl;  

  for (ahfits::firstRow(fpInput); ahfits::readOK(fpInput); ahfits::nextRow(fpInput)) {
    cnt++;
    ahfits::readRow(fpInput);

    //obtain pixel number 
    this_pixel = branchlib::convertSkyXYtoPIXEL((double)datIn.m_x, 
                                                (double)datIn.m_y, 
                                                datIn.m_roll_nom,
                                                datIn.m_tcrpx2,
                                                datIn.m_tcrpx3);
    //if (cnt == 1) std::cout << "time, pixel = " << datIn.m_time << " " << this_pixel << std::endl;
    if (q_nev[this_pixel] == 0) { 
      //Add event to new queue
      //if (cnt==1) std::cout << "Adding to new queue" << std::endl;
      q_time[this_pixel].push_back(datIn.m_time);
      q_tottime[this_pixel] = 0;
      q_x[this_pixel].push_back(datIn.m_x);
      q_y[this_pixel].push_back(datIn.m_y);
      q_pi[this_pixel].push_back(datIn.m_pi);
      q_nev[this_pixel]=1;
    } else { 
      //Add event to existing queue or closing out existing queue
      qnev = q_nev[this_pixel]-1; //location within queue
      dt = datIn.m_time - q_time[this_pixel][qnev]; //time to most recently added evt in queue 
      double qtot_try=q_tottime[this_pixel]+dt;
      //if (this_pixel == 3) std::cout << "time, dt = " << datIn.m_time << " " << dt << std::endl
      if ((dt < (par.m_dtmerge)) && (qtot_try < (par.m_dtmax))) {
        //Add event to this existing queue
        q_nev[this_pixel]++;
        qnev = q_nev[this_pixel] - 1; //advance one spot in queue
        q_time[this_pixel].push_back(datIn.m_time);
        q_tottime[this_pixel]=qtot_try;
        q_x[this_pixel].push_back(datIn.m_x);
        q_y[this_pixel].push_back(datIn.m_y);
        q_pi[this_pixel].push_back(datIn.m_pi);  
      } else {
        // close and process this queue; start a new one
        xtot = 0;
        ytot = 0; 
        pitot = 0; 
        for (int iq=0; iq < q_nev[this_pixel]; iq++) {
          xtot += q_x[this_pixel][iq];
          ytot += q_y[this_pixel][iq];
          pitot += q_pi[this_pixel][iq];
        } 

        if (((double)pitot*par.m_debin) < (1000.0 * par.m_maxenergy)) {
          //in event struct to be further processed: we use average X and Y,
          //and the summmed PI, and first time ("arrival time") for the 
          //characteristics of the merged event.  pixel and a redefinend pileup
          //flag are also used. The flag allows us to avoid sorting 
          simevt.m_time.push_back(q_time[this_pixel][0]); 
          simevt.m_x.push_back(xtot/q_nev[this_pixel]);
          simevt.m_y.push_back(ytot/q_nev[this_pixel]);
          simevt.m_pi.push_back(pitot);
          simevt.m_pixel.push_back(this_pixel);
          //if events are being merged set pileup flag to 1
          //else set pileup flag equal to 0
          if (q_nev[this_pixel] > 1) {
            simevt.m_pileup.push_back(1);
          } else {
            simevt.m_pileup.push_back(0);
          }
          //increment event counter

          simevt.m_evtnum++;

        } //end if pitot*debin > 1000. * maxenergy
        //prepare to start a new queue
        q_nev[this_pixel] = 1;
        q_time[this_pixel].clear();
        q_time[this_pixel].push_back(datIn.m_time);
        q_tottime[this_pixel] = 0;
        q_x[this_pixel].clear();
        q_x[this_pixel].push_back(datIn.m_x);
        q_y[this_pixel].clear();
        q_y[this_pixel].push_back(datIn.m_y);
        q_pi[this_pixel].clear();
        q_pi[this_pixel].push_back(datIn.m_pi);
      } // end if (dt < dtmerge)
    } // end if (q_nev[this_pixel] == 0)
  } //end loop over events

  //write last event for each pixel
   for (int pix=0; pix<branchlib::NPIXELS; ++pix) {
    if (q_nev[pix] > 0) {
      xtot=0;
      ytot=0;
      pitot=0;
      for (int iq=0; iq < q_nev[pix]; iq++) {
        xtot += q_x[pix][iq];
        ytot += q_y[pix][iq];
        pitot += q_pi[pix][iq];
      }
      if ((double)pitot*par.m_debin < 1000.*par.m_maxenergy) {
        simevt.m_time.push_back(q_time[pix][0]); 
        simevt.m_x.push_back(xtot/q_nev[pix]);
        simevt.m_y.push_back(ytot/q_nev[pix]);
        simevt.m_pi.push_back(pitot);
        simevt.m_pixel.push_back(pix);
        //if events are being merged set pileup flag to 1
        //else set pileup flag equal to 0
        if (q_nev[pix] > 1) {
          simevt.m_pileup.push_back(1);
        } else {
          simevt.m_pileup.push_back(0);
        }
        //increment event counter
        simevt.m_evtnum++;
      }
    }
  }

  //check on first event
  ahfits::firstRow(fpInput);
  ahfits::readRow(fpInput);
  AH_DEBUG << "************************************" << std::endl;
  AH_DEBUG << "datIn.m_time = " << datIn.m_time << std::endl;
  AH_DEBUG << "simevet.m_time.front() = " << simevt.m_time.front() << std::endl; 
  AH_DEBUG << "************************************" << std::endl;
  AH_DEBUG << "final cnt = " << cnt << std::endl;
  AH_DEBUG << "final simevt.m_evtnum = " << simevt.m_evtnum << std::endl; 
  AH_DEBUG << "simevt.m_time size = " << simevt.m_time.size() << std::endl; 
}

// ---------------------------------------------------------------------------

void mergeNone(branchlib::Par& par, long long nevents, std::vector<double> &evtime, 
               std::vector<int> &evtpix, branchlib::simevents& simevt) {

  //queues to store each merged groups information
  std::vector<double> q_time[branchlib::NPIXELS];    // time of event in queue
  double q_tottime[branchlib::NPIXELS];              // total time contained in queue
  int q_nev[branchlib::NPIXELS];                     // number of events currently in the queue for each pixel; initially 0
  for (int ii=0; ii < branchlib::NPIXELS; ii++) {
    q_tottime[ii]=0.;
    q_nev[ii]=0;
  }

  long long evtnum=0;         // final event tracker
  long long cnt=0;            // initial event tracker

  AH_OUT << "Merge simulated events arriving in same pixel within dtmerge = " << par.m_dtmerge <<
    " up to a maximum dtmax = " << par.m_dtmax << " discard event with energy above maxenergy = " <<
    par.m_maxenergy << std::endl;

  // loop over events
  for (int ii=0; ii < nevents; ii++) {
    cnt++;

    // grab current time and pixel
    double time=evtime[ii];
    int this_pixel=evtpix[ii];

    // fill queue
    if (q_nev[this_pixel] == 0) {      // add event if queue is empty
      q_time[this_pixel].push_back(time);
      q_tottime[this_pixel]=0;
      q_nev[this_pixel]=1;
    } else {                           // add event to existing queue, or process queue
      int qnev=q_nev[this_pixel]-1;
      double dt=time-q_time[this_pixel][qnev];
      double qtot_try=q_tottime[this_pixel]+dt;
      if ( (dt < par.m_dtmerge) && (qtot_try < par.m_dtmax) ) {
        // add event to existing queue
        q_nev[this_pixel]++;
        qnev=q_nev[this_pixel]-1;
        q_time[this_pixel].push_back(time);
        q_tottime[this_pixel]=qtot_try;
      } else {                         // process queue and start new queue
        // process queue
        simevt.m_time.push_back(q_time[this_pixel][0]);    // final event time is from first event in queue
        simevt.m_pixel.push_back(this_pixel);
        if (q_nev[this_pixel] > 1) {
          simevt.m_pileup.push_back(1);
        } else {
          simevt.m_pileup.push_back(0);
        }
        evtnum++;
        simevt.m_evtnum=evtnum;

        // restart queue with current event
        q_nev[this_pixel]=1;
        q_time[this_pixel].clear();
        q_time[this_pixel].push_back(time);
        q_tottime[this_pixel]=0.;
      }
    }   // end else (add event or process queue)
  }   // end for 

  // write last event for each pixel
  for (int pix=0; pix < branchlib::NPIXELS; ++pix) {
    if (q_nev[pix] > 0) {                   // only process non-empty queues
      simevt.m_time.push_back(q_time[pix][0]);
      simevt.m_pixel.push_back(pix);
      if (q_nev[pix] > 1) {
        simevt.m_pileup.push_back(1);
      } else {
        simevt.m_pileup.push_back(0);
      }
      evtnum++;
      simevt.m_evtnum=evtnum;
    }
  }

}

// ---------------------------------------------------------------------------

void sortSim(branchlib::simevents& simevt) {

  // do not want to swap unassigned columns
  bool swap_x=(simevt.m_x.size() > 0);
  bool swap_y=(simevt.m_y.size() > 0);
  bool swap_pi=(simevt.m_pi.size() > 0);
  int ii = 1;

  AH_INFO(ahlog::HIGH) << "Sorting " << simevt.m_evtnum << " in time after merging" << std::endl;

  while (ii < simevt.m_evtnum) {
    //if time is out of order
    if (simevt.m_time[ii] < simevt.m_time[ii-1]) { 

      //swap all elements of struct
      iter_swap(simevt.m_time.begin() + ii, simevt.m_time.begin() + (ii-1));
      if (swap_x) iter_swap(simevt.m_x.begin() + ii, simevt.m_x.begin() + (ii-1));
      if (swap_y) iter_swap(simevt.m_y.begin() + ii, simevt.m_y.begin() + (ii-1));
      if (swap_pi) iter_swap(simevt.m_pi.begin() + ii, simevt.m_pi.begin() + (ii-1));
      iter_swap(simevt.m_pixel.begin() + ii, simevt.m_pixel.begin() + (ii-1));
      iter_swap(simevt.m_pileup.begin() + ii, simevt.m_pileup.begin() + (ii-1));

      //decrement counter
      ii--;
    } else {
      //elements are in order increment counter
      ii++;
    }
  }
}

// ---------------------------------------------------------------------------

void writeSim(ahfits::FilePtr& fpSim, branchlib::Par& par, 
              branchlib::infileData& datIn, branchlib::simevents& simevt) {

  std::string outfile = par.m_infile +".out";  

  //local variables which store column data for output simulated event file
  double time = 0.;
  long x = 0.;
  long y = 0.;
  long pi = 0.;
  int pixel = 0;
  int pileup = 0; 

  AH_OUT << "Write new simulated event fits file" << std::endl;

  // create output file using infile as a template
  ahfits::clone(par.m_infile,outfile,&fpSim,true);

  ahfits::move(fpSim,"EVENTS");

  ahfits::removeAllRows(fpSim);

  // add new columns
  ahfits::insertColAfter(fpSim, "PIXEL", "B", "");
  ahfits::setColumnDescription(fpSim,"PIXEL","sxsbranch");
  ahfits::insertColAfter(fpSim, "ITYPE", "B", "");
  ahfits::setColumnDescription(fpSim,"ITYPE","sxsbranch");
  if (!ahfits::haveColumn(fpSim, "PILEUP")) {
    ahfits::insertColAfter(fpSim, "PILEUP", "B", "");
    ahfits::setColumnDescription(fpSim,"PILEUP","sxsbranch");
  }
  //set up router and connect columns to local variables
  ahfits::Router router(fpSim);

  router.connectScalar(ahfits::e_WRITEONLY, "TIME", time);
  router.connectScalar(ahfits::e_WRITEONLY, "X", x);
  router.connectScalar(ahfits::e_WRITEONLY, "Y", y);
  router.connectScalar(ahfits::e_WRITEONLY, "PI", pi);
  router.connectScalar(ahfits::e_WRITEONLY, "PIXEL", pixel);
  router.connectScalar(ahfits::e_WRITEONLY, "PILEUP", pileup);

  //Write simevt.m_time, simevt.m_x, simevt.m_y, simevt.m_pi, 
  //simevt.m_pixel to TIME ,X , Y, PI and PIXEL columns 
  ahfits::firstRow(fpSim);
  for (int ii = 0; ii < simevt.m_evtnum; ++ii) {
    time = simevt.m_time[ii];
    x = simevt.m_x[ii];
    y = simevt.m_y[ii];
    pi = simevt.m_pi[ii];
    pixel = simevt.m_pixel[ii];
    pileup = simevt.m_pileup[ii];
    ahfits::writeRow(fpSim);
    ahfits::nextRow(fpSim);
  }

}

// ---------------------------------------------------------------------------

void burst_reset(long long &nn, double &expose, double &ratburst, double &tburst, 
                 double &trise, double &tdecay,std::vector<double> &evtime) { 

  const double small_number = 1.0e-9;
  const ahfits::IndexType NTABLE = 1000;
  double table_time[NTABLE+1] = {0.}; //lightcurve 
  double table_x[NTABLE+1] = {0.};    //lookup table which maps a distribution onto the lightcurve

  double tpeak = 0.;                  //time of peak = beginning + rise time  

  //normalized timescale
  double tb_norm = 0.;     //tburst / total exposure time
  double tr_norm = 0.;     //trise / total exposure time 
  double td_norm = 0.;     //tedecay / total exposure time
  double tp_norm = 0.;     //tpeal / total exposure time 
  double tnorm = 0.;       //table_time[n] / total exposure time
  double norm0 = 0.;       //default normalization
  double norm = 0.;        //normalization 
  double norm1 = 0.;       //normalizaiton if decay is outside of the exposure
  double fac1 = 0.;     
  double fac2a = 0.; 
  double norm2 = 0.;       //normalizaion if burst and decay is inside exposure
  double fac2b = 0.;
  double facx = 0.; 

  int table_ind = 0;

  double weight1 = 0.;
  double weight2 = 0.; 
  double time_int = 0.;
  double dt_table = 0.;    //time unit associated with each element of look up table
  double x = 0.;           //random number between 0 and 1
  bool extrap = true;      //boolean value for the ahmath::search function 
                           //extrap true if x not in range of x arr

  //There is no pulsed parameter
  if (ratburst < small_number) {
    for (int ii=0;ii<nn;ii++) {
      evtime[ii] = expose * ahgen::getRandom();
    } 
  } else {

    tpeak = tburst+trise;

    //normalize timescale to exposure time to simplify computation
    tb_norm = tburst/expose;
    tr_norm = trise/expose;
    td_norm = tdecay/expose;
    tp_norm = tpeak/expose;  

    //normalizations and factors
    norm0 = 1.0;
    fac1 = 0.5*ratburst/tr_norm;
    norm1 = 1.0 + fac1 * (1.0 - tb_norm) * (1.0 - tb_norm);
    fac2a = 0.5 * ratburst * tr_norm;
    fac2b = ratburst * td_norm;
    norm2 = (1.0 + fac2a) + fac2b * (1.0 - exp(-((1 - tp_norm) / td_norm)));

    if ((tb_norm >= 1.0) || (ratburst == 0.0)) {
      norm = norm0;  //burst is outside of exposure 
   } else {
      if (tp_norm >= 1.0) {
        norm = norm1; //decay outside of exposure
      } else {
        norm = norm2;
      }
    }

    //lookup table mapping uniform distribution (table_x) onto 
    //lightcurve (table_time)
    //table size is ntable+1  
    dt_table=expose/float(NTABLE);
    for (int itable = 0; itable <= NTABLE; itable++) { 
      table_time[itable] = dt_table*float(itable);
      tnorm = table_time[itable]/expose;
      if ((tnorm <= tb_norm) || (ratburst == 0.0)) {
        table_x[itable] = tnorm;
      } else {
        if (tnorm <= tp_norm) { 
          table_x[itable] = tnorm + fac1 * (tnorm - tb_norm) * (tnorm - tb_norm);
        } else {
          facx = 1.0 - exp(-((tnorm-tp_norm)/td_norm));
          table_x[itable] = (tnorm + fac2a) + fac2b * facx;
        }
      }
      table_x[itable] /= norm;
    } //end loop over look up table

    //time assignment 
    long long itim = 0;
    while (itim < nn) {
      //random number on uniform unit interval
      x = ahgen::getRandom(); 
      table_ind = ahmath::search(x,table_x,NTABLE+1,extrap,table_ind);

      weight1 = (table_x[table_ind]-x)/(table_x[table_ind]-table_x[table_ind+1]);
      weight2 = (x-table_x[table_ind+1])/(table_x[table_ind]-table_x[table_ind+1]);

      time_int = (table_time[table_ind]*weight2) + (table_time[table_ind+1]*weight1);

      if (time_int<=expose) {
        evtime[itim]=time_int;
        itim=itim+1;
      } else {
        continue;
      }
    }//end while itim<nn
  }//end constant/burst conditional

}

// ---------------------------------------------------------------------------

void calcBranch(branchlib::Par& par, double exposure, double (&out_arr)[7][branchlib::NPIXELS], 
                int (&mask_arr)[branchlib::NPIXELS], branchlib::Nevtgr &nvg, 
                branchlib::outputVars &outVars) {

  double mask_frac = 0.;
  int nmask = 0;

  AH_OUT << "Calculate branching ratios per pixel for event list" << std::endl;

  for (int ii = 0; ii< branchlib::NPIXELS; ii++)  {
    mask_frac = (double)mask_arr[ii];
    if (mask_frac == 1) { nmask += 1; }
    outVars.m_rate_good_est[ii] = out_arr[0][ii];
    // check: rate_good_est[ii] = rate_hp_est[ii] + rate_mp_est[ii] 
    //+ rate_lp_est[ii] + rate_ms_est[ii]+ rate_ls_est[ii]
    outVars.m_rate_hp_est[ii] = out_arr[1][ii] * out_arr[0][ii]; 
    outVars.m_rate_hp_arr_est +=  mask_frac * outVars.m_rate_hp_est[ii];      
    outVars.m_rate_mp_est[ii] = out_arr[2][ii] * out_arr[0][ii];    
    outVars.m_rate_mp_arr_est += mask_frac * outVars.m_rate_mp_est[ii];              
    outVars.m_rate_lp_est[ii] = out_arr[3][ii] * out_arr[0][ii];       
    outVars.m_rate_lp_arr_est += mask_frac * outVars.m_rate_lp_est[ii];     
    outVars.m_rate_ms_est[ii] = out_arr[4][ii] * out_arr[0][ii];     
    outVars.m_rate_ms_arr_est += mask_frac * outVars.m_rate_ms_est[ii];                  
    outVars.m_rate_ls_est[ii] = out_arr[5][ii] * out_arr[0][ii];
    outVars.m_rate_ls_arr_est += mask_frac * outVars.m_rate_ls_est[ii];

    //cross talk events
    outVars.m_rate_ct_est[ii] = out_arr[6][ii];
    outVars.m_rate_hp_ct_est[ii] = out_arr[1][ii] * out_arr[6][ii];        
    outVars.m_rate_hp_ct_arr_est += mask_frac * outVars.m_rate_hp_ct_est[ii];     
    outVars.m_rate_mp_ct_est[ii] = out_arr[2][ii] * out_arr[6][ii];    
    outVars.m_rate_mp_ct_arr_est += mask_frac * outVars.m_rate_mp_ct_est[ii];             
    outVars.m_rate_lp_ct_est[ii] = out_arr[3][ii] * out_arr[6][ii];       
    outVars.m_rate_lp_ct_arr_est += mask_frac * outVars.m_rate_lp_ct_est[ii];    
    outVars.m_rate_ms_ct_est[ii] = out_arr[4][ii] * out_arr[6][ii];     
    outVars.m_rate_ms_ct_arr_est += mask_frac * outVars.m_rate_ms_ct_est[ii];                 
    outVars.m_rate_ls_ct_est[ii] = out_arr[5][ii] * out_arr[6][ii];
    outVars.m_rate_ls_ct_arr_est += mask_frac * outVars.m_rate_ls_ct_est[ii];
    outVars.m_rate_total_est[ii] = outVars.m_rate_good_est[ii] + outVars.m_rate_ct_est[ii];

    outVars.m_expose_hp_est[ii] = (double)exposure * (double)mask_frac * ((double)outVars.m_rate_hp_est[ii] / (double)outVars.m_rate_good_est[ii]);
    outVars.m_expose_mp_est[ii] = (double)exposure * (double)mask_frac * ((double)outVars.m_rate_mp_est[ii] / (double)outVars.m_rate_good_est[ii]);
    outVars.m_expose_ms_est[ii] = (double)exposure * (double)mask_frac * ((double)outVars.m_rate_ms_est[ii] / (double)outVars.m_rate_good_est[ii]);
    outVars.m_expose_lp_est[ii] = (double)exposure * (double)mask_frac * ((double)outVars.m_rate_lp_est[ii] / (double)outVars.m_rate_good_est[ii]);
    outVars.m_expose_ls_est[ii] = (double)exposure * (double)mask_frac * ((double)outVars.m_rate_ls_est[ii] / (double)outVars.m_rate_good_est[ii]);
  }

  outVars.m_rate_good_arr_est = outVars.m_rate_hp_arr_est + outVars.m_rate_mp_arr_est + outVars.m_rate_lp_arr_est + 
                        outVars.m_rate_ms_arr_est + outVars.m_rate_ls_arr_est;

  outVars.m_rate_ct_arr_est = outVars.m_rate_hp_ct_arr_est + outVars.m_rate_mp_ct_arr_est + outVars.m_rate_lp_ct_arr_est
                      + outVars.m_rate_ms_ct_arr_est + outVars.m_rate_ls_ct_arr_est;

  outVars.m_rate_total_arr_est = outVars.m_rate_good_arr_est + outVars.m_rate_ct_arr_est; 
  outVars.m_rate_avg_sub_est = outVars.m_rate_total_arr_est  / nmask;
  outVars.m_branch_hp_est = outVars.m_rate_hp_arr_est / outVars.m_rate_good_arr_est;
  outVars.m_branch_mp_est = outVars.m_rate_mp_arr_est / outVars.m_rate_good_arr_est;
  outVars.m_branch_ms_est = outVars.m_rate_ms_arr_est / outVars.m_rate_good_arr_est;
  outVars.m_branch_lp_est = outVars.m_rate_lp_arr_est / outVars.m_rate_good_arr_est;
  outVars.m_branch_ls_est = outVars.m_rate_ls_arr_est / outVars.m_rate_good_arr_est;

  for (int ii = 0; ii< branchlib::NPIXELS; ii++)  {
    mask_frac = (double)mask_arr[ii];
    outVars.m_rate_good[ii] = (double)nvg.m_ngood[ii] / exposure;
    outVars.m_rate_good_arr += mask_frac * outVars.m_rate_good[ii];
    outVars.m_rate_bad[ii] = (double)nvg.m_nbad[ii] / exposure;
    outVars.m_rate_bad_arr += mask_frac * outVars.m_rate_bad[ii];
    outVars.m_rate_bl[ii] = (double)nvg.m_nbl[ii] / exposure;
    outVars.m_rate_bl_arr += mask_frac * outVars.m_rate_bl[ii];
    outVars.m_rate_lo[ii] = (double)nvg.m_nlo[ii] / exposure;
    outVars.m_rate_lo_arr += mask_frac * outVars.m_rate_lo[ii];
    outVars.m_rate_rj[ii] = (double)nvg.m_nrj[ii] / exposure;
    outVars.m_rate_rj_arr += mask_frac * outVars.m_rate_rj[ii];
    outVars.m_rate_ct[ii] = (double)nvg.m_nct[ii] / exposure;
    outVars.m_rate_ct_arr += mask_frac * outVars.m_rate_ct[ii];
    outVars.m_rate_total[ii] = (double)nvg.m_ntotal[ii] / exposure;
    outVars.m_rate_total_arr += mask_frac * outVars.m_rate_total[ii];
    outVars.m_rate_hp[ii] = (double)nvg.m_nhp[ii] / exposure;
    outVars.m_rate_hp_arr += mask_frac * outVars.m_rate_hp[ii]; 
    outVars.m_rate_mp[ii] = (double)nvg.m_nmp[ii] / exposure;
    outVars.m_rate_mp_arr += mask_frac * outVars.m_rate_mp[ii]; 
    outVars.m_rate_ms[ii] = (double)nvg.m_nms[ii] / exposure;
    outVars.m_rate_ms_arr += mask_frac * outVars.m_rate_ms[ii]; 
    outVars.m_rate_lp[ii] = (double)nvg.m_nlp[ii] / exposure;
    outVars.m_rate_lp_arr += mask_frac * outVars.m_rate_lp[ii]; 
    outVars.m_rate_ls[ii] = (double)nvg.m_nls[ii] / exposure;
    outVars.m_rate_ls_arr += mask_frac * outVars.m_rate_ls[ii]; 
    //crosstalk events
    outVars.m_rate_hp_ct[ii] = (double)nvg.m_nhp_ct[ii] / exposure;
    outVars.m_rate_hp_ct_arr += mask_frac * outVars.m_rate_hp_ct[ii]; 
    outVars.m_rate_mp_ct[ii] = (double)nvg.m_nmp_ct[ii] / exposure;
    outVars.m_rate_mp_ct_arr += mask_frac * outVars.m_rate_mp_ct[ii]; 
    outVars.m_rate_ms_ct[ii] = (double)nvg.m_nms_ct[ii] / exposure;
    outVars.m_rate_ms_ct_arr += mask_frac * outVars.m_rate_ms_ct[ii];
    outVars.m_rate_lp_ct[ii] = (double)nvg.m_nlp_ct[ii] / exposure;
    outVars.m_rate_lp_ct_arr += mask_frac * outVars.m_rate_lp_ct[ii]; 
    outVars.m_rate_ls_ct[ii] = (double)nvg.m_nls_ct[ii] / exposure;
    outVars.m_rate_ls_ct_arr += mask_frac * outVars.m_rate_ls_ct[ii]; 

    // rate_hp_ct[ii]  + rate_mp_ct[ii]  + rate_ms_ct[ii]  
    //+ rate_lp_ct[ii]  + rate_ls_ct[ii] = rate_good_ct[ii]   

    //calculate effective exposure for real and sim files  
    if (ahgen::strtoupper(par.m_filetype) == "REAL") {
      outVars.m_expose_hp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nhp[ii] / (double)nvg.m_ncounts[ii]);
      outVars.m_expose_mp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nmp[ii] / (double)nvg.m_ncounts[ii]);
      outVars.m_expose_ms[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nms[ii] / (double)nvg.m_ncounts[ii]);
      outVars.m_expose_lp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nlp[ii] / (double)nvg.m_ncounts[ii]);
      outVars.m_expose_ls[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nls[ii] / (double)nvg.m_ncounts[ii]);
    } else {  //par.m_filetype = "sim" or "none"
      outVars.m_expose_hp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nhp[ii] / (double)nvg.m_ngood[ii]);
      outVars.m_expose_mp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nmp[ii] / (double)nvg.m_ngood[ii]);
      outVars.m_expose_ms[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nms[ii] / (double)nvg.m_ngood[ii]);
      outVars.m_expose_lp[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nlp[ii] / (double)nvg.m_ngood[ii]);
      outVars.m_expose_ls[ii] = (double)exposure * (double)mask_frac * ((double)nvg.m_nls[ii] / (double)nvg.m_ngood[ii]);
    }

    AH_DEBUG << "***********************************************"<< std::endl;
    AH_DEBUG << "pixel = " << ii << std::endl;
    AH_DEBUG << "exposure time = " << exposure << std::endl;
    AH_DEBUG << "mask_frac = " << mask_frac << std::endl;
    AH_DEBUG << "expose hp = " << outVars.m_expose_hp[ii] << std::endl;
    AH_DEBUG << "expose mp = " << outVars.m_expose_mp[ii] << std::endl;
    AH_DEBUG << "expose ms = " << outVars.m_expose_ms[ii] << std::endl;
    AH_DEBUG << "expose lp = " << outVars.m_expose_lp[ii] << std::endl;
    AH_DEBUG << "expose ls = " << outVars.m_expose_ls[ii] << std::endl;
    AH_DEBUG << "***********************************************"<< std::endl;

    //average rate per pixel
    outVars.m_rate_avg_sub = outVars.m_rate_total_arr / nmask;
    outVars.m_branch_hp = outVars.m_rate_hp_arr / outVars.m_rate_good_arr;
    outVars.m_branch_mp = outVars.m_rate_mp_arr / outVars.m_rate_good_arr;
    outVars.m_branch_ms = outVars.m_rate_ms_arr / outVars.m_rate_good_arr;
    outVars.m_branch_lp = outVars.m_rate_lp_arr / outVars.m_rate_good_arr;
    outVars.m_branch_ls = outVars.m_rate_ls_arr / outVars.m_rate_good_arr;
  }

  for (int ii = 0; ii != branchlib::NPIXELS; ++ii) {
    AH_DEBUG << "rejected = " << nvg.m_nrj[ii] << std::endl;
    AH_DEBUG << "lost = " << nvg.m_nlo[ii] << std::endl;
    AH_DEBUG << "crosstalk = " << nvg.m_nct[ii] << std::endl;
    AH_DEBUG << "baseline = " << nvg.m_nbl[ii] << std::endl;
    AH_DEBUG << "quick double = " << nvg.m_nquick[ii] << std::endl;
    AH_DEBUG << "high primary = " << nvg.m_nhp[ii] << std::endl;
    AH_DEBUG << "mid primary = " << nvg.m_nmp[ii] << std::endl;
    AH_DEBUG << "mid secondary = " << nvg.m_nms[ii] << std::endl;
    AH_DEBUG << "low primary = " << nvg.m_nlp[ii] << std::endl;
    AH_DEBUG << "low secondary = " << nvg.m_nls[ii] << std::endl;
    AH_DEBUG << "total events = " << nvg.m_ncounts[ii] << std::endl;
    AH_DEBUG << "total bad events = " << nvg.m_nbad[ii] << std::endl;
  }

}

// ---------------------------------------------------------------------------

void writeOutput(branchlib::Par &par, double * frac_arr, int (&prox_arr)[branchlib::NPIXELS][4], 
                 branchlib::outputVars &outVars) {

  //Declare local variables
  ahfits::FilePtr ahffp;
  int pixel = 0;                  //current pixel number
  double psffrac = 0.;

  //cross-talk pixels 
  int ctpixel1 = 0;
  int ctpixel2 = 0;
  int ctpixel3 = 0;
  int ctpixel4 = 0;

  //null flags for crosstalk pixels
  char nullpix1 = 0;
  char nullpix2 = 0;
  char nullpix3 = 0;
  char nullpix4 = 0;

  //semi-analytic estimate 
  double rate_good_est = 0.;
  double rate_ct_est = 0.;
  double rate_total_est = 0.; 
  double fracgood_est = 0.;
  double fracCT_est = 0.;
  double rate_hp_est = 0.;
  double rate_hp_ct_est = 0.;
  double rate_hp_total_est = 0.;
  double rate_mp_est = 0.;
  double rate_mp_ct_est = 0.;
  double rate_mp_total_est = 0.;
  double rate_lp_est = 0.;
  double rate_lp_ct_est = 0.;
  double rate_lp_total_est = 0.;
  double rate_ms_est = 0.;
  double rate_ms_ct_est = 0.;
  double rate_ms_total_est = 0.;
  double rate_ls_est = 0.;
  double rate_ls_ct_est = 0.;
  double rate_ls_total_est = 0.;
  double expose_hp_est = 0.;
  double expose_mp_est = 0.;
  double expose_lp_est = 0.;
  double expose_ms_est = 0.;
  double expose_ls_est = 0.;

  //full calculation
  double rate_good = 0.;
  double rate_ct = 0.;
  double rate_total = 0.; 
  double fracgood = 0.;
  double fracCT = 0.;
  double rate_hp = 0.;
  double rate_hp_ct = 0.;
  double rate_hp_total = 0.;
  double rate_mp = 0.;
  double rate_mp_ct = 0.;
  double rate_mp_total = 0.;
  double rate_lp = 0.;
  double rate_lp_ct = 0.;
  double rate_lp_total = 0.;
  double rate_ms = 0.;
  double rate_ms_ct = 0.;
  double rate_ms_total = 0.;
  double rate_ls = 0.;
  double rate_ls_ct = 0.;
  double rate_ls_total = 0.;
  double expose_hp = 0.;
  double expose_mp = 0.;
  double expose_lp = 0.;
  double expose_ms = 0.;
  double expose_ls = 0.;

  AH_OUT << "Write results to output file." << std::endl;

  //create output file
  ahfits::create(par.m_outfile, "", &ahffp);
  ahfits::addEmptyTbl(ahffp, "BRANCHEST");
  ahfits::addEmptyTbl(ahffp, "BRANCHCALC");
  ahfits::Router router(ahffp);

//************************Branch Estimate Extension*****************************

  ahfits::move(ahffp, "BRANCHEST");

  //write header keywords
  ahfits::writeKeyValDbl(ahffp,"BRANCHHP",outVars.m_branch_hp_est,"");
  ahfits::writeKeyValDbl(ahffp,"BRANCHMP",outVars.m_branch_mp_est,"");
  ahfits::writeKeyValDbl(ahffp,"BRANCHMS",outVars.m_branch_ms_est,"");
  ahfits::writeKeyValDbl(ahffp,"BRANCHLP",outVars.m_branch_lp_est,"");
  ahfits::writeKeyValDbl(ahffp,"BRANCHLS",outVars.m_branch_ls_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEHP",outVars.m_rate_hp_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEMP",outVars.m_rate_mp_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATELP",outVars.m_rate_lp_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEMS",outVars.m_rate_ms_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATELS",outVars.m_rate_ls_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEHPCT",outVars.m_rate_hp_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEMPCT",outVars.m_rate_mp_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATELPCT",outVars.m_rate_lp_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEMSCT",outVars.m_rate_ms_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATELSCT",outVars.m_rate_ls_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEGOOD",outVars.m_rate_good_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATECT",outVars.m_rate_ct_arr_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATEAVG",outVars.m_rate_avg_sub_est,"");
  ahfits::writeKeyValDbl(ahffp,"RATETOT",outVars.m_rate_total_arr_est,"");

  //Write Columns for BranchEst extension
  ahfits::insertColAfter(ahffp,"PIXEL","B","");
  ahfits::setColumnDescription(ahffp,"PIXEL","sxsbranch");
  ahfits::insertColAfter(ahffp,"CTPIXEL1","B","");
  ahfits::setColumnDescription(ahffp,"CTPIXEL1","sxsbranch");
  ahfits::insertColAfter(ahffp,"CTPIXEL2","B","");
  ahfits::setColumnDescription(ahffp,"CTPIXEL2","sxsbranch");
  ahfits::insertColAfter(ahffp,"CTPIXEL3","B","");
  ahfits::setColumnDescription(ahffp,"CTPIXEL3","sxsbranch");
  ahfits::insertColAfter(ahffp,"CTPIXEL4","B",""); 
  ahfits::setColumnDescription(ahffp,"CTPIXEL4","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOOD","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOOD","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECT","E","");
  ahfits::setColumnDescription(ahffp,"RATECT","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOT","E","");
  ahfits::setColumnDescription(ahffp,"RATETOT","sxsbranch");
  ahfits::insertColAfter(ahffp,"FRACPSF","E","");
  ahfits::setColumnDescription(ahffp,"FRACPSF","sxsbranch");
  ahfits::insertColAfter(ahffp,"FRACGOOD","E","");
  ahfits::setColumnDescription(ahffp,"FRACGOOD","sxsbranch");
  ahfits::insertColAfter(ahffp,"FRACCT","E","");
  ahfits::setColumnDescription(ahffp,"FRACCT","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOODHP","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOODHP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECTHP","E","");
  ahfits::setColumnDescription(ahffp,"RATECTHP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOTHP","E","");
  ahfits::setColumnDescription(ahffp,"RATETOTHP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOODMP","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOODMP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECTMP","E","");
  ahfits::setColumnDescription(ahffp,"RATECTMP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOTMP","E","");
  ahfits::setColumnDescription(ahffp,"RATETOTMP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOODLP","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOODLP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECTLP","E","");
  ahfits::setColumnDescription(ahffp,"RATECTLP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOTLP","E","");
  ahfits::setColumnDescription(ahffp,"RATETOTLP","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOODMS","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOODMS","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECTMS","E","");
  ahfits::setColumnDescription(ahffp,"RATECTMS","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOTMS","E","");
  ahfits::setColumnDescription(ahffp,"RATETOTMS","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATEGOODLS","E","");
  ahfits::setColumnDescription(ahffp,"RATEGOODLS","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATECTLS","E","");
  ahfits::setColumnDescription(ahffp,"RATECTLS","sxsbranch");
  ahfits::insertColAfter(ahffp,"RATETOTLS","E","");
  ahfits::setColumnDescription(ahffp,"RATETOTLS","sxsbranch");
  ahfits::insertColAfter(ahffp,"EXPOSUREHP","E","");
  ahfits::setColumnDescription(ahffp,"EXPOSUREHP","sxsbranch");
  ahfits::insertColAfter(ahffp,"EXPOSUREMP","E","");
  ahfits::setColumnDescription(ahffp,"EXPOSUREMP","sxsbranch");
  ahfits::insertColAfter(ahffp,"EXPOSURELP","E","");
  ahfits::setColumnDescription(ahffp,"EXPOSURELP","sxsbranch");
  ahfits::insertColAfter(ahffp,"EXPOSUREMS","E","");
  ahfits::setColumnDescription(ahffp,"EXPOSUREMS","sxsbranch");
  ahfits::insertColAfter(ahffp,"EXPOSURELS","E","");
  ahfits::setColumnDescription(ahffp,"EXPOSURELS","sxsbranch");

  //set TNULL for cross talking pixel columns
  ahfits::setTNull(ahffp, "CTPIXEL1", 99);
  ahfits::setTNull(ahffp, "CTPIXEL2", 99);
  ahfits::setTNull(ahffp, "CTPIXEL3", 99);
  ahfits::setTNull(ahffp, "CTPIXEL4", 99);

  //connect variable in outputVars struct to columns
  router.connectScalar(ahfits::e_WRITEONLY,"PIXEL",pixel);
  router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL1",ctpixel1,&nullpix1);
  router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL2",ctpixel2,&nullpix2);
  router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL3",ctpixel3,&nullpix3);
  router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL4",ctpixel4,&nullpix4); 
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOOD",rate_good_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECT",rate_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOT",rate_total_est); 
  router.connectScalar(ahfits::e_WRITEONLY,"FRACPSF",psffrac);
  router.connectScalar(ahfits::e_WRITEONLY,"FRACGOOD",fracgood_est);
  router.connectScalar(ahfits::e_WRITEONLY,"FRACCT",fracCT_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODHP",rate_hp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECTHP",rate_hp_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOTHP",rate_hp_total_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODMP",rate_mp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECTMP",rate_mp_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOTMP",rate_mp_total_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODLP",rate_lp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECTLP",rate_lp_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOTLP",rate_lp_total_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODMS",rate_ms_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECTMS",rate_ms_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOTMS",rate_ms_total_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODLS",rate_ls_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATECTLS ",rate_ls_ct_est);
  router.connectScalar(ahfits::e_WRITEONLY,"RATETOTLS",rate_ls_total_est);
  router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREHP",expose_hp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREMP",expose_mp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"EXPOSURELP",expose_lp_est);
  router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREMS",expose_ms_est);
  router.connectScalar(ahfits::e_WRITEONLY,"EXPOSURELS",expose_ls_est);

  //write data to columns
  for (pixel=0; pixel < branchlib::NPIXELS; ++pixel) {

    //if pixel is not present (still at initialized value of 99) 
    //set output to null
    ctpixel1 = prox_arr[pixel][0]; 
    ctpixel2 = prox_arr[pixel][1]; 
    ctpixel3 = abs(prox_arr[pixel][2]); 
    ctpixel4 = abs(prox_arr[pixel][3]); 

    psffrac = frac_arr[pixel];
    rate_good_est = outVars.m_rate_good_est[pixel];
    rate_ct_est = outVars.m_rate_ct_est[pixel];
    rate_total_est = outVars.m_rate_total_est[pixel];
    fracgood_est = rate_good_est / (rate_good_est+rate_ct_est);
    fracCT_est = rate_ct_est / (rate_good_est+rate_ct_est); 
    rate_hp_est = outVars.m_rate_hp_est[pixel];
    rate_hp_ct_est = outVars.m_rate_hp_ct_est[pixel];
    rate_hp_total_est = rate_hp_est + rate_hp_ct_est;
    rate_mp_est = outVars.m_rate_mp_est[pixel];
    rate_mp_ct_est = outVars.m_rate_mp_ct_est[pixel];
    rate_mp_total_est = rate_mp_est + rate_mp_ct_est;
    rate_lp_est = outVars.m_rate_lp_est[pixel];
    rate_lp_ct_est = outVars.m_rate_lp_ct_est[pixel];
    rate_lp_total_est = rate_lp_est + rate_lp_ct_est;
    rate_ms_est = outVars.m_rate_ms_est[pixel];
    rate_ms_ct_est = outVars.m_rate_ms_ct_est[pixel];
    rate_ms_total_est = rate_ms_est + rate_ms_ct_est;
    rate_ls_est = outVars.m_rate_ls_est[pixel];
    rate_ls_ct_est = outVars.m_rate_ls_ct_est[pixel];
    rate_ls_total_est = rate_ls_est + rate_ls_ct_est;
    expose_hp_est = outVars.m_expose_hp_est[pixel];
    expose_mp_est = outVars.m_expose_mp_est[pixel];
    expose_lp_est = outVars.m_expose_lp_est[pixel];
    expose_ms_est = outVars.m_expose_ms_est[pixel];
    expose_ls_est = outVars.m_expose_ls_est[pixel];
    ahfits::lastRow(ahffp);
    ahfits::nextRow(ahffp);
    ahfits::writeRow(ahffp);
  }  

  //clear connections to router
  router.clearConnections();   
  
//*******************************Branch Extension*****************************

    ahfits::move(ahffp, "BRANCHCALC");

    //write header keywords
    ahfits::writeKeyValStr(ahffp,"FILETYPE",par.m_filetype,"");
    ahfits::writeKeyValDbl(ahffp,"BRANCHHP",outVars.m_branch_hp,"");
    ahfits::writeKeyValDbl(ahffp,"BRANCHMP",outVars.m_branch_mp,"");
    ahfits::writeKeyValDbl(ahffp,"BRANCHMS",outVars.m_branch_ms,"");
    ahfits::writeKeyValDbl(ahffp,"BRANCHLP",outVars.m_branch_lp,"");
    ahfits::writeKeyValDbl(ahffp,"BRANCHLS",outVars.m_branch_ls,"");
    ahfits::writeKeyValDbl(ahffp,"RATEHP",outVars.m_rate_hp_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEMP",outVars.m_rate_mp_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATELP",outVars.m_rate_lp_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEMS",outVars.m_rate_ms_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATELS",outVars.m_rate_ls_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEHPCT",outVars.m_rate_hp_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEMPCT",outVars.m_rate_mp_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATELPCT",outVars.m_rate_lp_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEMSCT",outVars.m_rate_ms_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATELSCT",outVars.m_rate_ls_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEGOOD",outVars.m_rate_good_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATECT",outVars.m_rate_ct_arr,"");
    ahfits::writeKeyValDbl(ahffp,"RATEAVG",outVars.m_rate_avg_sub,"");
    ahfits::writeKeyValDbl(ahffp,"RATETOT",outVars.m_rate_total_arr,"");

    //Write Columns for Branch extension
    ahfits::insertColAfter(ahffp,"PIXEL","B","");
    ahfits::setColumnDescription(ahffp,"PIXEL","sxsbranch");
    ahfits::insertColAfter(ahffp,"CTPIXEL1","B","");
    ahfits::setColumnDescription(ahffp,"CTPIXEL1","sxsbranch");
    ahfits::insertColAfter(ahffp,"CTPIXEL2","B","");
    ahfits::setColumnDescription(ahffp,"CTPIXEL2","sxsbranch");
    ahfits::insertColAfter(ahffp,"CTPIXEL3","B","");
    ahfits::setColumnDescription(ahffp,"CTPIXEL3","sxsbranch");
    ahfits::insertColAfter(ahffp,"CTPIXEL4","B",""); 
    ahfits::setColumnDescription(ahffp,"CTPIXEL4","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOOD","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOOD","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECT","E","");
    ahfits::setColumnDescription(ahffp,"RATECT","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOT","E","");
    ahfits::setColumnDescription(ahffp,"RATETOT","sxsbranch");
    ahfits::insertColAfter(ahffp,"FRACPSF","E","");
    ahfits::setColumnDescription(ahffp,"FRACPSF","sxsbranch");
    ahfits::insertColAfter(ahffp,"FRACGOOD","E","");
    ahfits::setColumnDescription(ahffp,"FRACGOOD","sxsbranch");
    ahfits::insertColAfter(ahffp,"FRACCT","E","");
    ahfits::setColumnDescription(ahffp,"FRACCT","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOODHP","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOODHP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECTHP","E","");
    ahfits::setColumnDescription(ahffp,"RATECTHP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOTHP","E","");
    ahfits::setColumnDescription(ahffp,"RATETOTHP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOODMP","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOODMP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECTMP","E","");
    ahfits::setColumnDescription(ahffp,"RATECTMP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOTMP","E","");
    ahfits::setColumnDescription(ahffp,"RATETOTMP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOODLP","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOODLP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECTLP","E","");
    ahfits::setColumnDescription(ahffp,"RATECTLP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOTLP","E","");
    ahfits::setColumnDescription(ahffp,"RATETOTLP","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOODMS","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOODMS","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECTMS","E","");
    ahfits::setColumnDescription(ahffp,"RATECTMS","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOTMS","E","");
    ahfits::setColumnDescription(ahffp,"RATETOTMS","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATEGOODLS","E","");
    ahfits::setColumnDescription(ahffp,"RATEGOODLS","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATECTLS","E","");
    ahfits::setColumnDescription(ahffp,"RATECTLS","sxsbranch");
    ahfits::insertColAfter(ahffp,"RATETOTLS","E","");
    ahfits::setColumnDescription(ahffp,"RATETOTLS","sxsbranch");
    ahfits::insertColAfter(ahffp,"EXPOSUREHP","E","");
    ahfits::setColumnDescription(ahffp,"EXPOSUREHP","sxsbranch");
    ahfits::insertColAfter(ahffp,"EXPOSUREMP","E","");
    ahfits::setColumnDescription(ahffp,"EXPOSUREMP","sxsbranch");
    ahfits::insertColAfter(ahffp,"EXPOSURELP","E","");
    ahfits::setColumnDescription(ahffp,"EXPOSURELP","sxsbranch");
    ahfits::insertColAfter(ahffp,"EXPOSUREMS","E","");
    ahfits::setColumnDescription(ahffp,"EXPOSUREMS","sxsbranch");
    ahfits::insertColAfter(ahffp,"EXPOSURELS","E","");
    ahfits::setColumnDescription(ahffp,"EXPOSURELS","sxsbranch");

    //set TNULL for cross talking pixel columns
    ahfits::setTNull(ahffp, "CTPIXEL1", 99);
    ahfits::setTNull(ahffp, "CTPIXEL2", 99);
    ahfits::setTNull(ahffp, "CTPIXEL3", 99);
    ahfits::setTNull(ahffp, "CTPIXEL4", 99);

    //connect variable in outputVars struct to columns
    router.connectScalar(ahfits::e_WRITEONLY,"PIXEL",pixel);
    router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL1",ctpixel1,&nullpix1);
    router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL2",ctpixel2,&nullpix2);
    router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL3",ctpixel3,&nullpix3);
    router.connectScalar(ahfits::e_WRITEONLY,"CTPIXEL4",ctpixel4,&nullpix4); 
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOOD",rate_good);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECT",rate_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOT",rate_total);
    router.connectScalar(ahfits::e_WRITEONLY,"FRACPSF",psffrac);
    router.connectScalar(ahfits::e_WRITEONLY,"FRACGOOD",fracgood);
    router.connectScalar(ahfits::e_WRITEONLY,"FRACCT",fracCT);
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODHP",rate_hp);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECTHP",rate_hp_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOTHP",rate_hp_total);
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODMP",rate_mp);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECTMP",rate_mp_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOTMP",rate_mp_total);
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODLP",rate_lp);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECTLP",rate_lp_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOTLP",rate_lp_total);
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODMS",rate_ms);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECTMS",rate_ms_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOTMS",rate_ms_total);
    router.connectScalar(ahfits::e_WRITEONLY,"RATEGOODLS",rate_ls);
    router.connectScalar(ahfits::e_WRITEONLY,"RATECTLS ",rate_ls_ct);
    router.connectScalar(ahfits::e_WRITEONLY,"RATETOTLS",rate_ls_total);
    router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREHP",expose_hp);
    router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREMP",expose_mp);
    router.connectScalar(ahfits::e_WRITEONLY,"EXPOSURELP",expose_lp);
    router.connectScalar(ahfits::e_WRITEONLY,"EXPOSUREMS",expose_ms);
    router.connectScalar(ahfits::e_WRITEONLY,"EXPOSURELS",expose_ls);

    //write data to columns
    for (pixel=0; pixel != branchlib::NPIXELS; ++pixel) {
      //reset null flags 
      ctpixel1 = prox_arr[pixel][0]; 
      ctpixel2 = prox_arr[pixel][1]; 
      ctpixel3 = abs(prox_arr[pixel][2]); 
      ctpixel4 = abs(prox_arr[pixel][3]);   

      psffrac = frac_arr[pixel];
      rate_good = outVars.m_rate_good[pixel];
      rate_ct = outVars.m_rate_ct[pixel];
      rate_total = outVars.m_rate_total[pixel];
      fracgood = rate_good / (rate_good+rate_ct);
      fracCT = rate_ct / (rate_good+rate_ct); 
      rate_hp = outVars.m_rate_hp[pixel];
      rate_hp_ct = outVars.m_rate_hp_ct[pixel];
      rate_hp_total = rate_hp + rate_hp_ct;
      rate_mp = outVars.m_rate_mp[pixel];
      rate_mp_ct = outVars.m_rate_mp_ct[pixel];
      rate_mp_total = rate_mp + rate_mp_ct;
      rate_lp = outVars.m_rate_lp[pixel];
      rate_lp_ct = outVars.m_rate_lp_ct[pixel];
      rate_lp_total = rate_lp + rate_lp_ct;
      rate_ms = outVars.m_rate_ms[pixel];
      rate_ms_ct = outVars.m_rate_ms_ct[pixel];
      rate_ms_total = rate_ms + rate_ms_ct;
      rate_ls = outVars.m_rate_ls[pixel];
      rate_ls_ct = outVars.m_rate_ls_ct[pixel];
      rate_ls_total = rate_ls + rate_ls_ct;
      expose_hp = outVars.m_expose_hp[pixel];
      expose_mp = outVars.m_expose_mp[pixel];
      expose_lp = outVars.m_expose_lp[pixel];
      expose_ms = outVars.m_expose_ms[pixel];
      expose_ls = outVars.m_expose_ls[pixel];
      ahfits::lastRow(ahffp);
      ahfits::nextRow(ahffp);
      ahfits::writeRow(ahffp);
    }  //end loop over pixels 

  ahfits::close(ahffp);

}

// ---------------------------------------------------------------------------

std::string readStatus(char* status) {
  
  //Determine whether an event has good, bad or crosstalk STATUS
  //bad, in this case, is defined as bits 2, 3, 4, 5, 8,
  // 9, 10 or 11 being set in the STATUS column.
  //crosstalk is defined as bit 6 being set and bit 7 not being set
  //bit 7 idenfies primary event which induce crosstalk events 

  std::string status_str = "GOOD";

  //check if status is "bad"
  if (status[ahsxs::e_STATUS_ANTICO] == 1 || status[ahsxs::e_STATUS_PROXIMITY] == 1 ||
      status[ahsxs::e_STATUS_NEAR_CALPIX] == 1 || status[ahsxs::e_STATUS_RECOIL] == 1 ||
      status[ahsxs::e_STATUS_MXS_DIRECT] == 1 || status[ahsxs::e_STATUS_MXS_DIRECT_GLOW] == 1 ||
      status[ahsxs::e_STATUS_MXS_INDIRECT] == 1 || status[ahsxs::e_STATUS_MXS_INDIRECT_GLOW] == 1) {
    status_str = "BAD";
    return status_str; 
  } else if (status[ahsxs::e_STATUS_ELECTRICAL] == 1 && status[ahsxs::e_STATUS_ELEC_MAX] == 0) {
    status_str = "CROSSTALK";
    return status_str;
  } else {
    return status_str;
  }

}

// ---------------------------------------------------------------------------

}  // namespace sxssimbranch

/* Revision Log
 $Log: sxsbranchlib.cxx,v $
 Revision 1.19  2016/03/29 21:24:14  mdutka
 find_index will return 0 in lowerbound is out of range

 Revision 1.18  2016/03/29 18:42:05  mdutka
 correcting bug in find_index routine, could lead to an array out of bounds issue

 Revision 1.17  2016/02/19 01:05:36  klrutkow
 changed telescop to HITOMI in error message

 Revision 1.16  2015/12/29 16:14:39  mwitthoe
 sxsbranch: throw error if input file is provided, but has no rows

 Revision 1.15  2015/10/14 15:03:54  mdutka
 changing handling of exposure =0 and pi = 0

 Revision 1.14  2015/10/14 13:54:42  mdutka
 fixing exposure time acquisition from event file

 Revision 1.13  2015/10/14 13:04:01  mdutka
 checking in new version with checks for division by zero

 Revision 1.12  2015/10/09 21:12:56  mdutka
 Chaged writing of simulated data and fixed issue with pixel assignment for simulated data

 Revision 1.11  2015/08/13 18:12:34  mwitthoe
 sxsbranch: add standard tool prologue, move non-standard functions/structs from tool source to lib file; clean-up code; remove compiler warnings; remove unused functions

 Revision 1.10  2015/07/09 18:11:42  mdutka
 updating branch, fixed issue with lack of preciasion in pixel calculation, fixed writing extra rows in sim out file as well

 Revision 1.9  2015/06/18 14:36:18  mdutka
 updating doxygen comments

 Revision 1.8  2015/06/17 17:05:05  mdutka
 updating sxsbranch to provide more output, check STATUS column for real files, reordered parameters

 Revision 1.7  2015/05/26 19:08:36  mwitthoe
 sxsbranch: 1) bug-fixes from code restructuring; 2) add new burst time calculation from heasim; 3) add merging and cross-talk changes for infile=NONE; 4) update mergeSim() function; 5) fix output column names and types

 Revision 1.6  2015/05/20 17:36:24  mdutka
 commiting incremental changes to sxsbranch, merging simulated events is implemeted

 Revision 1.5  2015/02/12 20:47:46  mdutka
 checking in working version of new tool sxsbranch

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

 Revision 1.5  2014/09/12 17:52:26  mdutka
 Added support for the new response writing tools (rmflib)

 Revision 1.4  2014/08/04 17:19:36  mdutka
 Fixed converting double to integer warnings

 Revision 1.3  2014/07/10 18:47:19  mdutka
 removed debugging message

 Revision 1.2  2014/07/10 18:21:11  mdutka
 all dynamically sized arrays are now initialized to zero, added dynamic sizing to containers which store data from the CALDB information

 Revision 1.1  2014/06/30 19:02:36  mdutka
 Added sxsrmf tool

 Revision 1.3  2014/01/22 20:40:21  mwitthoe
 sxsflagpix: update accordng to code review; issue 331

 Revision 1.2  2014/01/15 22:26:33  peachey
 Add Peer Review comments regarding variable declarations,

 Revision 1.1  2013/11/12 17:45:03  mwitthoe
 sxsflagpix: moved getPar_warn() functions to ahapp; renamed sxspixel map to sxsflagpixlib; worked on standards compliance for source (mainly moving variable declarations to top of scope)



 BELOW IS THE REVISION LOG FOR sxspixelmap.cxx BEFORE RENAMING TO sxsflagpixlib.cxx

 Revision 1.3  2013/10/07 21:20:11  mwitthoe
 sxsflagpix: switch over to new ahfits connection functions (issue 270)

 Revision 1.2  2013/04/16 17:19:26  mwitthoe
 sxsflagpix: change source to match latest TRF (major change to cross-talk algorithm, add MXS algorithm); modify form ahcaldb library, sxspixelmap, to follow new standards

 Revision 1.1  2013/01/02 19:09:52  mwitthoe
 add new ahcaldb library for the SXS pixel map


*/
