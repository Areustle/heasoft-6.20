/// \file sxspha2pilib.cxx
/// \brief Functions for sxspha2pi
/// \author Kristin Rutkowski
/// \date $Date: 2016/06/02 15:13:08 $
 
#define AHLABEL tool_sxspha2pi_sxspha2pilib
#define AHCVSID "$Id: sxspha2pilib.cxx,v 1.11 2016/06/02 15:13:08 mwitthoe Exp $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmath/ahmath.h"
#include "ahmission/ahmission.h"
#include "sxspha2pilib.h"

#include <sstream>

namespace ahsxs {

// ****************************************************************************

void loadTempGainData(const std::string & filename, 
                      const std::string & method,
                      ahsxs::TempDataAllPixels & tempdat) {
  
    // connect local variables to drift (gain) input columns
  double l_time=0.;                   // column: TIME
  char l_timenull=0;                  // NULL flag for TIME column
  int l_pixel=0;                      // column: PIXEL (valid range: 0-35)
  double l_temp=0.;                   // column: TEMP_FIT or TEMP_AVG
  char l_tempnull=0;                  // NULL flag for TEMP column
  
  ahfits::FilePtr drift_fp;
  ahfits::open(filename, "Drift_energy", &drift_fp);
  ahmission::checkEmptyTable(drift_fp,filename);

  // make connections to local variables
  ahfits::Router router(drift_fp);
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time,&l_timenull);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel);
  std::string temp_col;
  if (ahgen::strtoupper(method) == "FIT") {
    temp_col = "TEMP_FIT";
  } else if (ahgen::strtoupper(method) == "AVERAGE") {
    temp_col = "TEMP_AVE";
  }
  router.connectScalar(ahfits::e_READONLY,temp_col,l_temp,&l_tempnull);
  
  // store calibration method, and temperature index used to create file
  tempdat.m_caltype = ahgen::strtoupper(ahfits::getKeyValStr(drift_fp,"CALTYPE"));
  tempdat.m_tempidx = ahfits::getKeyValLLong(drift_fp,"TEMPIDX");
  
  // start reading drift file, loading into struct
  for (ahfits::firstRow(drift_fp); ahfits::readOK(drift_fp); ahfits::nextRow(drift_fp) ) {
    ahfits::readRow(drift_fp);
    
    // if the fitting failed in sxsgain, skip this row
    if (l_tempnull == 1) {
      continue;
    }
    
    // store the unique pixels
    tempdat.m_pixels.insert(l_pixel);
    tempdat.m_tempdatpix.at(l_pixel).m_size++;
    
    // for this pixel, store the times and temps
    // note: this is assuming rows are sorted by time, and l_pixel is 0-based
    tempdat.m_tempdatpix.at(l_pixel).m_time.push_back(l_time);
    tempdat.m_tempdatpix.at(l_pixel).m_temp.push_back(l_temp);
    
  } // end reading drift file
  
} // end loadTempGainData()

// ****************************************************************************

void loadEPIScaleFactors(const std::string & filename, 
                         ahsxs::EPIScaleFactors & scaledat) {

  int l_pixel=0;                      // column: PIXEL
  double l_hpscale=0.;                // EPI scale factor for Hp
  double l_mscale=0.;                 // EPI scale factor for Mp, Ms
  double l_lscale=0.;                 // EPI scale factor for Lp, Ls

  ahfits::FilePtr fp;
  ahfits::open(filename, "PIX12GAINCOR", &fp);
  ahmission::checkEmptyTable(fp,filename);

  // make connections to local variables
  ahfits::Router router(fp);
  router.connectScalar(ahfits::e_READONLY,"PIXEL",l_pixel);
  router.connectScalar(ahfits::e_READONLY,"HP",l_hpscale);
  if (ahfits::haveColumn(fp,"M")) {     // if M column exists, so must L (or throw error)
    router.connectScalar(ahfits::e_READONLY,"M",l_mscale);
    router.connectScalar(ahfits::e_READONLY,"L",l_lscale);
    scaledat.m_haveallgrades=true;
  } else {
    scaledat.m_haveallgrades=false;
  }

  // read file and load data into struct
  for (ahfits::firstRow(fp); ahfits::readOK(fp); ahfits::nextRow(fp) ) {
    ahfits::readRow(fp);

    if (l_pixel < 0 || l_pixel > 35) {
      std::stringstream msg;
      msg << "Failed reading EPI scale factor file; invalid pixel number: " << l_pixel;
      AH_THROW_RUNTIME(msg.str());
    }

    scaledat.m_hpscale[l_pixel]=l_hpscale;
    scaledat.m_mscale[l_pixel]=l_mscale;
    scaledat.m_lscale[l_pixel]=l_lscale;
  }

}

// ****************************************************************************

void setValuesToNull(char & l_upinull, 
                     char & l_epinull, 
                     char & l_pinull, 
                     char & l_epi2null,
                     char & l_tempnull) {

    l_upinull  = 1;
    l_epinull  = 1;
    l_pinull   = 1;
    l_epi2null = 1;
    l_tempnull = 1;

}

// ****************************************************************************

void computeEPI(const Par& par, double pha, int pixel, int itype, double temp,
                int temp_itype, ahsxs::engain::AllEnergyGainData& gaindat, 
                const ahsxs::EPIScaleFactors& scaledat,
                double& epi) {

  // Construct energy vs temperature lookup table
  std::vector<double> temps(par.m_ntemp);      // temperatures of gain curves
  std::vector<double> ens(par.m_ntemp);        // event PHA -> energy using gain curves
  char ensnull = 0;
  for (int iTemp = 0 ; iTemp < par.m_ntemp ; ++iTemp) {
    temps[iTemp] = gaindat.m_engainVec.at(iTemp).m_temperature; 
    ahsxs::engain::applyGain(pha, pixel, temp_itype, gaindat.m_engainVec.at(iTemp), ens[iTemp], ensnull);
    if (ensnull==1) AH_THROW_LOGIC("NULL value found when applying gain: provide valid ITYPE");
  }

  // Get coefficients for quadratic: T = a*E^2 + b*E + c where T is temperature
  // and E is energy.  Then invert this equation to compute E from T.  This
  // is done instead of interpolation so that the the energy is exact at the
  // PHA used for calibration.
  double a=0.,b=0.,c=0.;
  bool okay=true;
  ahmath::getQuadraticCoefficients(ens[0],temps[0],ens[1],temps[1],ens[2],temps[2],a,b,c,okay);
  if (!okay) {
    AH_THROW_LOGIC("Singular matrix in quadratic coefficient calculation; this should never happen if unless duplicate temperatures exist in the SXS gain file.");
  }

  // Invert quadratic which has two solutions.  We take the solution
  // with the positive sign for events with a PHA > 0 in order to 
  // guarantee that the resulting EPI is positive.  For baseline events
  // with a negative PHA, we use the negative-sign solution instead.
  // This choice allows the distribution of baseline EPI values about
  // zero to be symmetric.  If the positive-sign solution was used 
  // instead, the baseline distribution would be asymmetric.
  if (pha < 0.) {
    epi=-0.5*b/a-0.5/a*std::sqrt(b*b-4.*a*(c-temp));
  } else {
    epi=-0.5*b/a+0.5/a*std::sqrt(b*b-4.*a*(c-temp));
  }

  // Scale EPI value if scaleepi=yes and event has correct grade
  if (par.m_scaleepi) {
    if (scaledat.m_haveallgrades) {     // CALDB file has scale factors for all grades (ignore scalegrade parameter)
      if (itype == 0) {
        epi*=scaledat.m_hpscale[pixel];
      } else if (itype == 1 || itype == 3) {
        epi*=scaledat.m_mscale[pixel];
      } else if (itype == 2 || itype == 4) {
        epi*=scaledat.m_lscale[pixel];
      }
    } else {                            // CALDB file only has scale factors for Hp (use scalegrade parameter)
      if (par.m_gradeset.count(itype) > 0) {
        epi*=scaledat.m_hpscale[pixel];
      }
    }
  }

}

// ****************************************************************************

}  // end namespace


/* Revision Log
 $Log: sxspha2pilib.cxx,v $
 Revision 1.11  2016/06/02 15:13:08  mwitthoe
 sxspha2pi: 1) add parameter, pxphaoffset, which is added to each event PHA along with a random number between -0.5 and +0.5 before the gain is applied; 2) now support a 2nd format for the scalefile which has columns for all three grades instead of just Hp

 Revision 1.10  2016/04/22 14:20:23  mwitthoe
 sxspha2pi: 1) check if TEMP column exists in output file before trying to insert it; 2) write TUNIT for TEMP column; 3) add CVS log to lib source file


*/
