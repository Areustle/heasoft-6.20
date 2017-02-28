/// \file sxsgainlib.cxx
/// \brief Functions for sxsgain
/// \author Michael Witthoeft
/// \date $Date: 2016/10/13 16:04:09 $

#define AHLABEL tool_sxsgain_sxsgainlib
#define AHCVSID "$Id: sxsgainlib.cxx,v 1.11 2016/10/13 16:04:09 mwitthoe Exp $"

#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahsxs/ahsxs.h"
#include "sxsgainlib.h"

// ****************************************************************************

void createGainOutput(const std::string& filename, const std::string& splitcolumn,
                      const std::string& telescop, const std::string& instrume,
                      const std::string& detnam, bool gridprofile, 
                      const std::string& calmethod, int tempidx, double gaintemp,
                      const std::string& energref, double chfirst, double chlast,
                      double chwidth, const std::string& picolumn, int maxnmesh,
                      bool calcerr, bool writeerrfunc, int nshift, int nwidth, 
                      ahfits::FilePtr& fpout) {

  ahfits::create(filename,"",&fpout);

  // construct data type for arrays in drift table
  std::stringstream ssdtype;
  ssdtype << maxnmesh << "D";
  std::string dtype=ssdtype.str();

  if (!gridprofile) {
    // The Drift_energy extension contains table of drift corrections.
    ahfits::addEmptyTbl(fpout,"Drift_energy");
    if (telescop != "") ahfits::writeKeyValStr(fpout,"TELESCOP",telescop,"Telescope mission name");
    if (instrume != "") ahfits::writeKeyValStr(fpout,"INSTRUME",instrume,"Instrument name");
    if (detnam != "") ahfits::writeKeyValStr(fpout,"DETNAM",detnam,"Detector subsystem");
    ahfits::insertColBefore(fpout,"TIME","1D","");
    ahfits::setTUnit(fpout,"TIME","sec");
    ahfits::setColumnDescription(fpout,"TIME","Seconds from 01 Jan 2014 00:00:00");
    ahfits::insertColAfter(fpout,splitcolumn,"1I","");
    ahfits::setColumnDescription(fpout,splitcolumn,"Pixel Number range 0-35");
    ahfits::insertColAfter(fpout,"COR_FIT","1D","");
    ahfits::setColumnDescription(fpout,"COR_FIT","Ratio of fitted energy to profile");
    ahfits::insertColAfter(fpout,"COR_AVE","1D","");
    ahfits::setColumnDescription(fpout,"COR_AVE","Ratio of bin average energy to profile");
    ahfits::insertColAfter(fpout,"CHISQ","1D","");
    ahfits::setColumnDescription(fpout,"CHISQ","Reduced Chi^2 of fit");
    ahfits::insertColAfter(fpout,"AVGUNBIN","1D","");   // unbinned average
    ahfits::setColumnDescription(fpout,"AVGUNBIN","Unbinned average energy");
    ahfits::insertColAfter(fpout,"AVGBIN","1D","");     // binned average
    ahfits::setColumnDescription(fpout,"AVGBIN","Binned average energy");
    ahfits::insertColAfter(fpout,"AVGFIT","1D","");     // average from fit
    ahfits::setColumnDescription(fpout,"AVGFIT","Energy from fit");
    ahfits::insertColAfter(fpout,"SHIFT","1D","");      // fitted shift
    ahfits::setColumnDescription(fpout,"SHIFT","Fitted energy shift");
    ahfits::insertColAfter(fpout,"SCALE","1D","");      // fitted scale factor
    ahfits::setColumnDescription(fpout,"SCALE","Fitted count scale factor");
    ahfits::insertColAfter(fpout,"BGRND","1D","");      // fitted background
    ahfits::setColumnDescription(fpout,"BGRND","Fitted background counts");
    ahfits::insertColAfter(fpout,"SLOPE","1D","");      // fitted background slope
    ahfits::setColumnDescription(fpout,"SLOPE","Fitted background slope");
    ahfits::insertColAfter(fpout,"WIDTH","1D","");      // fitted width
    ahfits::setColumnDescription(fpout,"WIDTH","Fitted Gaussian convolution width");
    ahfits::insertColAfter(fpout,"TELAPSE","1D","");
    ahfits::setColumnDescription(fpout,"TELAPSE","Seconds between first and last events");
    ahfits::insertColAfter(fpout,"EXPOSURE","1D","");
    ahfits::setColumnDescription(fpout,"EXPOSURE","Exposure seconds between first and last events");
    ahfits::insertColAfter(fpout,"NEVENT","1I","");
    ahfits::setColumnDescription(fpout,"NEVENT","Number of events in fitting group");
    ahfits::insertColAfter(fpout,"BINMESH",dtype,"");   // binning mesh
    ahfits::setColumnDescription(fpout,"BINMESH","Energy bins");
    ahfits::insertColAfter(fpout,"SPECTRUM",dtype,"");  // binned event spectrum
    ahfits::setColumnDescription(fpout,"SPECTRUM","Counts spectrum");
    ahfits::insertColAfter(fpout,"FITPROF",dtype,"");   // theoretical profile with fitted parameters applied
    ahfits::setColumnDescription(fpout,"FITPROF","Fitted profile");
    ahfits::insertColAfter(fpout,"TEMP_FIT","1D","");
    ahfits::setColumnDescription(fpout,"TEMP_FIT","Temperature from fit");
    ahfits::insertColAfter(fpout,"TEMP_AVE","1D","");
    ahfits::setColumnDescription(fpout,"TEMP_AVE","Temperature from bin average");

    ahfits::writeKeyValStr(fpout,"CALTYPE",calmethod,"Calibration method: Cal-pix, MXS, or Fe55");
    ahfits::writeKeyValLLong(fpout,"TEMPIDX",tempidx,"Gain temperature index used for building profile");
    ahfits::writeKeyValDbl(fpout,"GAINTEMP",gaintemp,"Gain temperature [K] used for building profile");
    ahfits::writeKeyValStr(fpout,"ENERGREF",energref,"Calibration line used in fit");
    ahfits::writeKeyValDbl(fpout,"CH_START",chfirst,"First energy channel");
    ahfits::writeKeyValDbl(fpout,"CH_STOP",chlast,"Last energy channel");
    ahfits::writeKeyValDbl(fpout,"CH_WIDTH",chwidth,"Energy channel width");
    ahfits::writeKeyValStr(fpout,"PICOLUMN",picolumn,"Energy column used in fitting");

    if (calcerr) {
      ahfits::insertColAfter(fpout,"SIGSHLIKE","1D","");
      ahfits::setColumnDescription(fpout,"SIGSHLIKE","Standard deviation of shift likelihood");
      ahfits::insertColAfter(fpout,"SIGWDLIKE","1D","");
      ahfits::setColumnDescription(fpout,"SIGWDLIKE","Standard deviation of width likelihood");
      ahfits::insertColAfter(fpout,"SIGSHCHI2","1D","");
      ahfits::setColumnDescription(fpout,"SIGSHCHI2","Standard deviation from shift Chi^2");
      ahfits::insertColAfter(fpout,"SIGWDCHI2","1D","");
      ahfits::setColumnDescription(fpout,"SIGWDCHI2","Standard deviation from width Chi^2");

      if (writeerrfunc) {
        std::stringstream shform;         // format specifier for shift likelihood
        shform << nshift << "D";
        ahfits::insertColAfter(fpout,"SHIFTS",shform.str(),"");
        ahfits::setColumnDescription(fpout,"SHIFTS","Shift values for error functions");
        ahfits::insertColAfter(fpout,"SHLIKE",shform.str(),"");
        ahfits::setColumnDescription(fpout,"SHLIKE","Shift likelihood values");
        ahfits::insertColAfter(fpout,"SHCHI2",shform.str(),"");
        ahfits::setColumnDescription(fpout,"SHCHI2","Shift reduced Chi^2 values");

        std::stringstream wdform;         // format specifier for width likelihood
        wdform << nwidth << "D";
        ahfits::insertColAfter(fpout,"WIDTHS",wdform.str(),"");
        ahfits::setColumnDescription(fpout,"WIDTHS","Width values for error functions");
        ahfits::insertColAfter(fpout,"WDLIKE",wdform.str(),"");
        ahfits::setColumnDescription(fpout,"WDLIKE","Width likelihood values");
        ahfits::insertColAfter(fpout,"WDCHI2",wdform.str(),"");
        ahfits::setColumnDescription(fpout,"WDCHI2","Width reduced Chi^2 values");
      }
    }
  }

  // The Grid_profile extension contains the theoretical calibration profile
  // on the binned energy grid used in the fitting.
  ahfits::addEmptyTbl(fpout,"Grid_profile");
  if (telescop != "") ahfits::writeKeyValStr(fpout,"TELESCOP",telescop,"Telescope mission name");
  if (instrume != "") ahfits::writeKeyValStr(fpout,"INSTRUME",instrume,"Instrument name");
  if (detnam != "") ahfits::writeKeyValStr(fpout,"DETNAM",detnam,"Detector subsystem");
  ahfits::insertColBefore(fpout,"PHA","1D","");
  ahfits::setTUnit(fpout,"PHA","chan");
  ahfits::setColumnDescription(fpout,"PHA","Pulse height amplitude");
  for( int ip = 0; ip < ahsxs::NPIXEL; ++ip) {
    std::stringstream amp;                        // column name
    if(ip<10) amp << "AMPLITUDE" << 0 << ip;
    else amp << "AMPLITUDE" << ip;

    std::stringstream desc;                       // column description
    desc << "Profile amplitude for pixel " << ip;

    ahfits::insertColAfter(fpout,amp.str(),"1D","");
    ahfits::setColumnDescription(fpout,amp.str(),desc.str());
  }
  ahfits::writeKeyValStr(fpout,"CALTYPE",calmethod,"Calibration method: Cal-pix, MXS, or Fe55");
  ahfits::writeKeyValLLong(fpout,"TEMPIDX",tempidx,"Gain temperature index used for building profile");
  ahfits::writeKeyValDbl(fpout,"GAINTEMP",gaintemp,"Gain temperature [K] used for building profile");
  ahfits::writeKeyValStr(fpout,"ENERGREF",energref,"Calibration line used in fit");
  ahfits::writeKeyValDbl(fpout,"CH_START",chfirst,"First energy channel");
  ahfits::writeKeyValDbl(fpout,"CH_STOP",chlast,"Last energy channel");
  ahfits::writeKeyValDbl(fpout,"CH_WIDTH",chwidth,"Energy channel width");
  ahfits::writeKeyValStr(fpout,"PICOLUMN",picolumn,"Energy column used in fitting");

}

// ****************************************************************************

double computeTemperature(double avgpha, int pixel, int itype, bool useevst,
                          ahsxs::engain::AllEnergyGainData & gaindat, 
                          double profavgpha, bool parextrap) {

  // Two methods can be used to compute the temperature.  The first (useevst =
  // true) method constructs an energy vs temperature table where the energies
  // are computed from avgpha using the gain coefficients at each temperature.
  // This table is interpolated at the theoretical calibration energy to obtain
  // temperature.
  //
  // The second method (useevst = false) instead builds a PHA vs temperature
  // table by doing a reverse gain lookup on the theoretical calibration energy
  // to get a PHA at each temperature.  The measured PHA (avgpha) is then
  // interpolated on this table to get the temperature.
  //
  // When the final temperature is near one of the tabulated values in the 
  // gain file, these two methods are in agreement.  However, when between
  // temperatures, the second method gives a smaller error.  The first method
  // is retained here for future testing and can be enabled by setting the
  // ntemp parameter to a negative value.  This option does not appear in the
  // documentation of the tool.

  double temp = 0.;                    // Temperature
  unsigned long idx = 0;               // Temperature index to search for 
  bool extrap=false;

  double * temps;
  double * vals;         // method 1: energy; method 2: pha
  double energy = 0.;
  char enull = 0;

  // convert profile average from PHA to energy
  double profavg=0.;
  ahsxs::engain::applyGain(profavgpha,pixel,itype,gaindat.m_engainVec[1],profavg,enull);

  // number of temperatures
  int ntemp = gaindat.m_numTemps;

  temps = new double[ntemp];
  vals = new double[ntemp];

  if (useevst) {

    // obtain table of energy vs temperature
    for(int it = 0; it < ntemp; ++it) {
      temps[it] = gaindat.m_engainVec[it].m_temperature;
      ahsxs::engain::applyGain(avgpha,pixel,itype,gaindat.m_engainVec[it],energy,enull);
      if (enull == 1) {
        AH_THROW_LOGIC("gain lookup unexpectedly failed when constructing energy v temperature lookup table.");
      }
      vals[it] = energy;
    }
  
    // Use interpolation to calculate temperature of pixel
    // The search_npoint function is required to determine that 
    // there is no extrapolation, it will always return 0 as an index.
    idx = ahmath::search_npoint(profavg,vals,ntemp,ntemp,extrap,idx);
    AH_DEBUG << "Index used to interpolate temperature for energy: " << idx << std::endl;
    if (extrap) {
      if (parextrap) {
        AH_INFO(ahlog::HIGH) << "Using extrapolation to compute temperature." << std::endl;
      } else {
        AH_THROW_RUNTIME("Cannot compute effective temperature; profile energy outside range tabulated using gain coefficients.");
      }
    }
    temp = ahmath::interpolate_point_npoint(profavg,vals,temps,ntemp,idx);

  } else {     // useevst=no
 
    // obtain table of PHA vs temperature
    for(int it = 0; it < ntemp; ++it) {
      temps[ntemp-it-1] = gaindat.m_engainVec[it].m_temperature;
      vals[ntemp-it-1]=ahsxs::engain::reverseLookup(profavg,pixel,itype,gaindat.m_engainVec[it],1.e-8);
    }
 
    // Use interpolation to calculate temperature of pixel
    // The search_npoint function is required to determine that 
    // there is no extrapolation, it will always return 0 as an index.
    idx = ahmath::search_npoint(avgpha,vals,ntemp,ntemp,extrap,idx);
    AH_DEBUG << "Index used to interpolate temperature for energy: " << idx << std::endl;
    if (extrap) {
      if (parextrap) {
        AH_INFO(ahlog::HIGH) << "Using extrapolation to compute temperature." << std::endl;
      } else {
        AH_THROW_RUNTIME("Cannot compute effective temperature; profile energy outside range tabulated using gain coefficients.");
      }
    }
    temp = ahmath::interpolate_point_npoint(avgpha,vals,temps,ntemp,idx);
  }

  AH_DEBUG << "Temperature calculated for pixel " << pixel << ": " << temp << std::endl;

  // Unallocate arrays
  delete [] temps;
  delete [] vals;

  return temp;

} 

// ****************************************************************************

void setNullRow(const ahgain::CalProfile & profdat, ahgain::FitResults & results,
                char * nullcol) {

  // Set members of results to NULL in the case of a non-fit: 
  // 1. Fitted parameters are not applied to column FITPROF
  //    Reset results fitted data to profile data
  // 2. Columns TIME, splitcol, TELAPSE, EXPOSURE, NEVENT, 
  //    BINMESH and SPECTRUM are left as-is
  for (int j=0; j < profdat.m_nmesh; j++) {
    results.m_fitdat[j] = profdat.m_prof[j];
  }

  // 3. Results members set as NULL:
  //    COR_FIT, COR_AVE, CHISQ, AVGUNBIN, AVGBIN, AVGFIT, 
  //    SHIFT, SCALE, BGRND, WIDTH, TEMP_FIT, TEMP_AVE 
  nullcol[0] = 1;

}

// ****************************************************************************

/* Revision Log
 $Log: sxsgainlib.cxx,v $
 Revision 1.11  2016/10/13 16:04:09  mwitthoe
 sxsgain: use tabulated profile average instead of theoretical average when determining the temperature from the fitted shift

 Revision 1.10  2016/08/10 16:29:57  mwitthoe
 sxsgain: now support pixel-dependent GTI files in two formats

 Revision 1.9  2016/05/10 16:59:08  mwitthoe
 sxsgain: for the input GTI file: 1) if DETNAM not defined in an extension, treat it as a general GTI (pixel-independent); 2) if extended syntax is used, only read that single extension which can be pixel-dependent or not

 Revision 1.8  2015/11/17 21:49:57  mwitthoe
 sxsgain: add column/keyword comments

 Revision 1.7  2015/11/17 16:30:59  mwitthoe
 sxsgain: 1) remove centerprof parameter (behavior now enabled when startenergy or stopenergy is specified); 2) make PHA bins centered on half-integers; 3) use PHA vs temperature table to calculate temperature (make ntemp parameter negative to use energy vs temperature table instead); 4) write same keywords to 1st and 2nd extensions

 Revision 1.6  2015/10/27 19:11:24  mwitthoe
 sxsgain: move resetUncertainties() function to ahgain library

 Revision 1.5  2015/10/27 17:26:11  mwitthoe
 sxsgain: change name of writelike parameter to writeerrfunc; this parameter now causes the tool to write both the likelihood and Chi^2 functions

 Revision 1.4  2015/10/27 14:33:06  mwitthoe
 sxsgain: add Chi^2 uncertainties

 Revision 1.3  2015/10/16 20:33:04  mwitthoe
 sxsgain: 1) set default value of ckrisetime to yes; 2) increase MAXPT to 5000; 3) set default value of tempidx to 2; 4) new parameter extrap to allow for extrapolating the temperature

 Revision 1.2  2015/10/13 20:01:58  mwitthoe
 sxsgain: 1) option to skip events with RISE_TIME > 127; 2) allow calculation of uncertainties on fitted shift and width using the likelihood method

 Revision 1.1  2015/10/01 21:14:24  mwitthoe
 sxsgain: now can accept either a GTI file with a single, general GTI extension or a GTI file with 36, pixel-specific extensions


*/
