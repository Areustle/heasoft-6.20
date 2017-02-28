/// \file engain.cxx
/// \brief General SXS energy gain functions
/// \author Kristin Rutkowski
/// \date $Date: 2015/11/17 21:28:49 $

#define AHLABEL ahsxs_engain
#define AHCVSID "$Id: engain.cxx,v 1.6 2015/11/17 21:28:49 mwitthoe Exp $"

#include "ahsxs/engain.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <sstream>            // stringstream
#include <cmath>              // std::pow()

namespace ahsxs {

namespace engain {


void loadEnergyGainData(const std::string & filename, 
                        AllEnergyGainData & allDat) {

  // clear any data currently in structure
  clearEnergyGainData(allDat);

  // open file
  ahfits::FilePtr fptr=0;           // ahfits file pointer
  ahfits::open(filename,"",&fptr);
  ahfits::firstHDU(fptr,ahfits::e_BINARY_TBL);
  if (!ahfits::readOK(fptr))
    AH_THROW_RUNTIME("failed to open SXS energy gain CALDB file");
  
  // get number of temperatures = number of rows
  int nTemp = ahfits::numRows(fptr);
  if (nTemp <= 0) 
    AH_THROW_RUNTIME("CALDB file has invalid number of temperatures; There must be at least one row in the file");
  allDat.m_numTemps = nTemp;
  allDat.m_temps.resize(nTemp);
  allDat.m_engainVec.resize(nTemp);
  
  // get number of coefficients from keyword
  int ncoeff = ahfits::getKeyValLLong(fptr,"NCOEFF");
  if (ncoeff <= 0) 
    AH_THROW_RUNTIME("CALDB file has invalid number of coefficients; NCOEFF must be positive");
  allDat.m_numCoeff = ncoeff;
  for (int iTemp = 0 ; iTemp < nTemp ; ++iTemp) {
    allDat.m_engainVec[iTemp].m_ncoeff = ncoeff;
  }

  // allocate memory in structure based on number of coefficients
  for (int iTemp = 0 ; iTemp < nTemp ; ++iTemp) {
    for (int iPixel = 0 ; iPixel < NPIXEL ; ++iPixel) {
      allDat.m_engainVec[iTemp].m_high[iPixel].resize(ncoeff);
      allDat.m_engainVec[iTemp].m_mid[iPixel].resize(ncoeff);
      allDat.m_engainVec[iTemp].m_low[iPixel].resize(ncoeff);
    }
  }

  // declare variables for reading FITS file based on number of expected
  // coefficients.  We need the arrays here to be in dimensions backward from
  // EnergyGainData, to easily get the entire 36-element (for 36 pixels) array
  // from each coefficient column.  So the dimensions of these vectors will be 
  // (ncoeff, npixel)
  // 'l_' means 'local' for data loaded from file
  double l_temp = 0;                  // column: TEMP
  std::vector< std::vector<double> > l_h(ncoeff);   // columns: H0, H1, H2, etc
  std::vector< std::vector<double> > l_m(ncoeff);   // columns: M0, M1, M2, etc
  std::vector< std::vector<double> > l_l(ncoeff);   // columns: L0, L1, L2, etc
  for (int iCoeff = 0 ; iCoeff < ncoeff ; ++iCoeff) {
    l_h[iCoeff].resize(NPIXEL);
    l_m[iCoeff].resize(NPIXEL);
    l_l[iCoeff].resize(NPIXEL);
  }

  // make connections to local variables
  ahfits::Router router(fptr);
  
  router.connectScalar(ahfits::e_READONLY, "TEMP", l_temp);
  
  for (int iCoeff=0 ; iCoeff < ncoeff ; iCoeff++) {
    // form column names: H0, M0, L0, H1, M1, L1, .... L4 etc
    std::stringstream hcol, mcol, lcol;
    hcol << "H" << iCoeff;    // e.g. H3
    mcol << "M" << iCoeff;
    lcol << "L" << iCoeff;
    
    // make sure each column has the correct dimension
    if (NPIXEL != ahfits::columnRepeat(fptr, hcol.str()))
      AH_THROW_RUNTIME("CALDB file has invalid dimension of data columns");
    if (NPIXEL != ahfits::columnRepeat(fptr, mcol.str()))
      AH_THROW_RUNTIME("CALDB file has invalid dimension of data columns");
    if (NPIXEL != ahfits::columnRepeat(fptr, lcol.str()))
      AH_THROW_RUNTIME("CALDB file has invalid dimension of data columns");

    // connect the file data into the local variables, to load later
    router.connectFixedLengthArray(ahfits::e_READONLY, hcol.str(), &(l_h[iCoeff][0]));
    router.connectFixedLengthArray(ahfits::e_READONLY, mcol.str(), &(l_m[iCoeff][0]));
    router.connectFixedLengthArray(ahfits::e_READONLY, lcol.str(), &(l_l[iCoeff][0]));
  }

  // read table
  int iTemp = 0;
  for (ahfits::firstRow(fptr) ; ahfits::readOK(fptr) ; ahfits::nextRow(fptr), ++iTemp) {

    ahfits::readRow(fptr);
    
    // store the current temperature (for this row)
    allDat.m_temps[iTemp]=l_temp;
    allDat.m_engainVec.at(iTemp).m_temperature = l_temp;
    
    // store each H,M,L column vector (vector by pixel)
    for (int iCoeff = 0 ; iCoeff < ncoeff ; ++iCoeff) {
      for (int iPixel = 0 ; iPixel < NPIXEL ; ++iPixel) {
        allDat.m_engainVec.at(iTemp).m_high[iPixel][iCoeff] = l_h[iCoeff][iPixel];
        allDat.m_engainVec.at(iTemp).m_mid[iPixel][iCoeff]  = l_m[iCoeff][iPixel];
        allDat.m_engainVec.at(iTemp).m_low[iPixel][iCoeff]  = l_l[iCoeff][iPixel];
      }
    }
  } // end-loop through rows

  // close FITS file
  ahfits::close(fptr);

} // end loadEnergyGainData()


// ---------------------------------------------------------------------------


void clearEnergyGainData(AllEnergyGainData & allDat) {
  
  for (int iTemp = 0 ; iTemp < allDat.m_numTemps ; ++iTemp) {
    EnergyGainData & currDat = allDat.m_engainVec.at(iTemp);
    currDat.m_ncoeff = 0;
    currDat.m_temperature = 0.;
  }
  allDat.m_numTemps = 0;
  allDat.m_numCoeff = 0;
  
} // end clearEnergyGainData()


// ---------------------------------------------------------------------------


/// \brief Convert energy into PHA using polynomial coefficients from gain file
/// \param[in] energy energy for which to calculate a PHA
/// \param[in] pixel Which pixel in SXS, used to access correct profile
/// \param[in] itype value from the ITYPE column in the event file.  Describes 
///               the grade and whether the event is primary or secondary.  
///               The values are:
///                   ITYPE   Description
///                   --------------------
///                    0      High Primary  - Hp
///                    1      Mid Primary   - Mp
///                    2      Mid Secondary - Ms
///                    3      Low Primary   - Lp
///                    4      Los Secondary - Ls
///                    5      Baseline      - BL
///                    6      Lost          - LO
///                    7      Rejected      - Rj 
/// \param[in] gaindat energy gain data from CALDB file for single temperature
/// \param[in] tol optional tolerance for determining when PHA values converge
/// \return The PHA corresonding to this energy, for this pixel/temperature
///
/// Used to convert energy range into PHA units; per pixel.  Takes in all the 
/// polynomial coefficients for the energy gain, and the pixel identifier 
/// (0-based).  Calculates back to determine the PHA for a given energy, for 
/// the temperature at this specific gaindat struct.  The gaindat struct is 
/// basically one row, for a single temperature, from the CALDB energy gain 
/// file.  
double reverseLookup(double energy, int pixel, int itype, 
                     const EnergyGainData & coeff,
                     double tol) {
  
  // y = a0 + a1*x + a2*x^2 + ...
  //    y: energy         (input)
  //    x: PHA            (output)
  //   aN: coefficient N  (from input gain file, in gaindat struct)
  // x0 = [1/a1]*[y - a0]
  // xK = [1/a1]*[y - a0 - a2*x{K-1}^2 - a3*x{k-1}^3 - ... - aN*x{k-1}^N]
  
  // number of coefficients for this energy gain file
  int ncoeff = coeff.m_ncoeff;
  
  // flag if we successfully found x
  bool foundX = false;
  
  // grab a ptr to the coefficients array for this pixel, depending on ITYPE
  const std::vector<double> *a;
  if (0 == itype || 5 == itype) {           // Hp or BL
    a = &coeff.m_high[pixel];
  } else if (1 == itype || 2 == itype) {    // Mp or Ms
    a = &coeff.m_mid[pixel];
  } else if (3 == itype || 4 == itype) {    // Lp or Ls
    a = &coeff.m_low[pixel];
  } else {
    AH_THROW_LOGIC("Invalid ITYPE value.  Only values of 0-5 are allowed.");
  }
  
  double mult = (1. / a->at(1));
  double y = energy;
  double a0 = a->at(0);
  double x0 = mult * (y - a0);
  
  double xk = 0.0;              // x_k,   start with x_1 in loop
  double xkm1 = x0;             // x_k-1, start with x_0 in loop
  
  double prevDiff = 1e6;        // set it very large at first
  double currDiff = prevDiff;
  
  // continue k loop until x values converge
  // at the start, x_k = x_1
  do {
    
    // set the x_{k-1} to be the x_k from last iteration
    xkm1 = xk;
    
    // determine the current x_k
    // rememeber: xK = [1/a1]*[y - a0 - a2*x{K-1}^2 - a3*x{k-1}^3 - ...]
    xk = y - a0;
    // loop over coefficients, from i=2 to ncoeff
    for (int icoeff = 2 ; icoeff < ncoeff ; ++icoeff) {
      xk -= (a->at(icoeff) * std::pow(xkm1,icoeff));
    }
    xk *= mult;
    
    // grab the difference from the last loop iteration
    // (or from the initial large value 1e6, if this is first time in loop)
    prevDiff = currDiff;
    currDiff = std::abs(xk - xkm1);
  
    // once we reach our tolerance, or if the differences stabilize, 
    // we can say we've found our final x_k
    if ( (currDiff < tol) || (currDiff == prevDiff) ) {
      foundX = true;
      break;
    }
    
    // otherwise, keep going.
    // loop will break when differences start to get larger, and 
    // the values no longer converge 
  } while (currDiff < prevDiff);
  
  // if we exited while-loop without converging, there was an error
  // given proper gain coefficients, this should never be triggered, so we 
  // needn't worry about recovering
  if (!foundX) {
    AH_THROW_RUNTIME("PHA did not converge.");
  }
  
  // return the converged of x
  return xk;
  
} // end reverseLookup()


// ---------------------------------------------------------------------------


void applyGain(double pha, int pixel, int itype, 
               const EnergyGainData & gaindat, 
               double & energy, char & energynull) {
  
  // initialize output variables
  energy = 0.0;
  energynull = 0;
  
  if (0 == itype || 5 == itype) {           // Hp or BL
    for (int ic=0; ic < gaindat.m_ncoeff; ic++) {
      energy += gaindat.m_high[pixel][ic] * std::pow(pha,ic);
    }
  } else if (1 == itype || 2 == itype) {    // Mp or Ms
    for (int ic=0; ic < gaindat.m_ncoeff; ic++) 
      energy += gaindat.m_mid[pixel][ic] * std::pow(pha,ic);
  } else if (3 == itype || 4 == itype) {    // Lp or Ls
    for (int ic=0; ic < gaindat.m_ncoeff; ic++) 
      energy += gaindat.m_low[pixel][ic] * std::pow(pha,ic);
  } else {
    energynull=1;
  }
  
} // end applyGain()


// ---------------------------------------------------------------------------



} // end namespace engain

} // end namespace ahsxs



/* Revision Log
   $Log
 
*/
