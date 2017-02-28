/// \file xrtreftable_lib.cxx
/// \brief Calculate reflectivity probability for ray tracing
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2016/04/07 21:07:03 $



#define AHLABEL tool_xrtreftable_lib
#define AHCVSID "$Id: xrtreftable_lib.cxx,v 1.33 2016/04/07 21:07:03 rshill Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#define TOOLNAME "xrtreftable"

#include "xrtreftable_lib.h"

// Parameter file access.
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "ape/ape_util.h"   // for writeParametersToLog()

#include "hdcal.h"          // HDgtcalf
#include "headas_utils.h"   // for get_toolname()

#include <strings.h>        // strcasecmp, strncasecmp



/**********************************************
 * ********************************************
 * 		supporting functions
 * ********************************************
**********************************************/


void decomposeMaterial(const std::string & coatingmaterial, AtomicData & atom,
                       int & nelemcoat, std::vector<int> & zcoat, 
                       std::vector<int> & fcoat, double & wcoat) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // localFormula will be shortened each time the leading character is examined.
  std::string localFormula = coatingmaterial;
  std::string currElemSymbol;
  std::string numberString;
  int atomicNumber = 0;
  // number of occurrences of an atom in the molecule
  int elementRepeats = 0;
  
  // for error checking below
  std::string errorMsg;
  
  // -------------------------------------
  
  // start out by setting input counters to 0
  nelemcoat = 0;
  wcoat = 0.e0;
  zcoat.clear();
  fcoat.clear();

  // loop until all characters in input string coatingmaterial have been
  // processed. In each iteration,  the lead character of localFormula 
  // (position 0) is examined.
  while (localFormula.length() != 0) {
    
    // if it's a space, erase it and go to next letter
    if (localFormula[0] == ' ') {
      localFormula.erase(0,1);
      continue;
    }
    
    // Check for an upper case letter which begins an element symbol
    if (isupper(localFormula[0])) {
      
      // Save the upper case letter as the beginning of element symbol
      currElemSymbol = localFormula.substr(0,1);
      localFormula.erase(0,1);

      // Check for a following lower case as part of the symbol
      if (islower(localFormula[0])) {
        // save the 2nd part of the symbol
        currElemSymbol.append(localFormula.substr(0,1));
        localFormula.erase(0,1);
      }
      
      // Does a numeric value follow?
      if (isdigit(localFormula[0])) {
        
        // extract the numeric value
        numberString = localFormula.substr(0,1);
        localFormula.erase(0,1);
        while (localFormula.length() != 0) {
          if (isdigit(localFormula[0])) {
            // extract the numeric value
            numberString.append(localFormula.substr(0,1));
            localFormula.erase(0,1);
          } else {
            // We reached the end of the number
            break;
          }
        }
        
        // convert the numeric value
        elementRepeats = atoi(numberString.c_str());
        
      } else {
        // If the char following the chemical symbol letters wasn't a digit, 
        // then default val of 1 for num of the element’s atoms in molecule.
        elementRepeats = 1;
      }
      
      // We now have both a symbol and a repeat number
      // Search for the symbol in the table of atomic symbols
      for (unsigned int i = 1 ; i < atom.m_elsymbol.size() ; i++) {
        // if we found it, note what atomic number it is
        if (atom.m_elsymbol[i].compare(currElemSymbol) == 0 ) {
          atomicNumber = i;
          break;
        }
      }
      
      // if we didn't find the symbol, error
      if ( atomicNumber == 0 ) {
        errorMsg = "Invalid element symbol ";
        errorMsg.append(currElemSymbol);
        AH_THROW_RUNTIME(errorMsg);
      }
      
      // a valid element has been found. Update the output array holding 
      // atomic number and frequency of occurrence for each element’s atoms
      if (atomicNumber > 0) {
        // the nelemencoat'th entry in zcoat is the current atomic number
        zcoat.push_back(atomicNumber);
        fcoat.push_back(elementRepeats);
        // Update the cumulative total atomic weight of the molecule
        wcoat += elementRepeats * atom.m_elweight[atomicNumber];
        // Update the number of elements found in the chemical formula 
        nelemcoat++;
      }
    
    } else {
      // if the first character of element wasn't an uppercase letter, error
      errorMsg = "Invalid chemical formula ";
      errorMsg.append(coatingmaterial);
      AH_THROW_RUNTIME(errorMsg);
    }
    
  } // while (localFormula.length() != 0)
  
} // decomposeMaterial()



void calcsf1sf2materials(const double ephotonev, const double ephotoncm, 
                         const double ephotoncmsq, const Scattering & scat, 
                         const AtomicData & atom, Param & param,
                         const Edge & edge, const Surface & surf,
                         std::vector<double> & sf1material,
                         std::vector<double> & sf2material, 
                         std::vector<double> & mabsmaterial) {
                           
  AH_DEBUG << "******************* START OF calcsf1sf2materials() *******************" << std::endl;
  AH_DEBUG << "ephotonev = " << ephotonev << std::endl;
  AH_DEBUG << "ephotoncm = " << ephotoncm << std::endl;
  AH_DEBUG << "ephotoncmsq = " << ephotoncmsq << std::endl;
    
    
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
 
  // factors to convert scat factors into optical constants
  // opticalConstant = r_e * N_A / 2pi;
  const double opticalConstant = 2.7008760e10;
  const double opticalConstantFactor = opticalConstant * ephotoncmsq;
  //  mabsConstant = 2 * r_e * N_A
  const double mabsConstant = 3.39402097e11;
  const double mabsFactor = mabsConstant * ephotoncm;
  
  // max epected atomic number in files
  const int maxzofelements = atom.m_nelements;
  
  // scaled f1 and f2.  will be calculated in call to interpolatef1f2
  double sf1 = 0;
  double sf2 = 0;
  
  // these will hold temp scattering factors for each unique element.
  std::vector<double> sf1z;
  std::vector<double> sf2z;
  // resize these to how many elements there are.  we will look up scat factor
  // by Z, which is the index here.  So size is actually maxZ + 1, because
  // index 0 won't be used
  sf1z.resize(maxzofelements+1);
  sf2z.resize(maxzofelements+1);
  
  // for checking that all the needed elements are in the scattering file
  std::vector<long>::const_iterator iter;
  std::string errorMsg;
  
  // -------------------------------------

  // start out by clearing output arrays
  sf1material.clear();
  sf2material.clear();
  mabsmaterial.clear();
  
  // Get the interpolated scattering factors for this energy, for each 
  // unique atomic number
  // for each material
  for (int iMat = 0 ; iMat < surf.m_nummaterials ; iMat++) {
    // for each element in this material
    for (int iElem = 0 ; iElem < surf.m_materialnumelements[iMat] ; iElem++) {
      
      // check that this element was in the scattering file
      iter = find(scat.m_znumbers.begin(), scat.m_znumbers.end(), surf.m_materialz[iMat][iElem]);
      if (iter == scat.m_znumbers.end()) {
        errorMsg = "The scattering file does not have an entry for element Z = ";
        errorMsg.append(longToString(surf.m_materialz[iMat][iElem]));
        AH_THROW_RUNTIME(errorMsg);
      }
      
      // Don’t calculate again if this atomic number has been done already
      if (sf1z[surf.m_materialz[iMat][iElem]] == 0.0) {
        AH_DEBUG << "surf.m_materialz[iMat][iElem] = " << surf.m_materialz[iMat][iElem] << std::endl;
        interpolatef1f2(param, surf.m_materialz[iMat][iElem], ephotonev, scat, edge, sf1, sf2);
        sf1z[surf.m_materialz[iMat][iElem]] = sf1;
        sf2z[surf.m_materialz[iMat][iElem]] = sf2;
      }
      
    }
  }
  
  // Now create the arrays that will eventually become the optical constants 
  // (at the requested energy) for each unique material after they are 
  // multiplied by the density of a coating layer (cgs units). They will be 
  // sf1material() and sf2material(), which will yield the real and 
  // imaginary parts of the optical constants respectively
  for (int iMat = 0 ; iMat < surf.m_nummaterials ; iMat++) {
    
    // initialize current entries to 0
    sf1material.push_back(0.0);
    sf2material.push_back(0.0);
    mabsmaterial.push_back(0.0);
    
    // sum over atoms in a molecule for every unique material
    for (int iMol = 0 ; iMol <  surf.m_materialnumelements[iMat]; iMol++) {
      sf1material[iMat] = sf1material[iMat] + ( surf.m_materialnumatoms[iMat][iMol] * sf1z[surf.m_materialz[iMat][iMol]] ); 
      sf2material[iMat] = sf2material[iMat] + ( surf.m_materialnumatoms[iMat][iMol] * sf2z[surf.m_materialz[iMat][iMol]] );
    }
    
    // normalize by atomic weight of each molecule in each material.
    mabsmaterial[iMat] = ( mabsFactor * sf2material[iMat] ) / surf.m_materialatomicweight[iMat]; 
    sf1material[iMat]  = ( opticalConstantFactor * sf1material[iMat] ) / surf.m_materialatomicweight[iMat];
    sf2material[iMat]  = ( opticalConstantFactor * sf2material[iMat] ) / surf.m_materialatomicweight[iMat];
    
  }
  
} //  calcsf1sf2materials()


/******************************************************************************/


void interpolatef1f2(Param & param, const int Z, const double ephotonev, 
                     const Scattering & scat, const Edge & edge, 
                     double & sf1out, double & sf2out) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // indicates whether a slope of simple or "edge-method" interpolation has
  // already been calculated in order to determine whether to default to simple
  // the end value is for diagnostic purposes to identify which path was used.
  // value    meaning
  //   0        interpolation wasn't needed
  //   1        absorption edge energies were used for a more complex interpolation
  //   2        simple linear interpolation was done
  int doneSlope = 0;
  
  // used for interpolating the energy if the energy isn't exactly in the file
  double deltaEnergy = 0.;
  double slopef1 = 0.;
  double slopef2 = 0.;
  // holds the index of the scattering energy that is just below input energy
  int fbin = 0;
  
  // for checking that the energy is within the allowed range
  std::string errorMsg;
  bool needToAbort = false;
                 
  // -------------------------------------
  
  // make sure requested energy is in range
  errorMsg = "Photon energy (";
  errorMsg.append(doubleToString(ephotonev));
  errorMsg.append(") is ");
  if (ephotonev < scat.m_energy[scat.m_f1effindex[Z][0]]) {
    errorMsg.append("below minimum for which a valid f1 value exists ");
    needToAbort = true;
  } else if (ephotonev < scat.m_energy[scat.m_f2effindex[Z][0]]) {
    errorMsg.append("below minimum for which a valid f2 value exists ");
    needToAbort = true;
  } else if (ephotonev > scat.m_energy[scat.m_f1effindex[Z][1]]) {
    errorMsg.append("above maximum for which a valid f1 value exists ");
    needToAbort = true;
  } else if (ephotonev > scat.m_energy[scat.m_f2effindex[Z][1]]) {
    errorMsg.append("above maximum for which a valid f2 value exists ");
    needToAbort = true;
  }
  errorMsg.append("for Z = ");
  errorMsg.append(intToString(Z));
  if (needToAbort) {
    AH_THROW_RUNTIME(errorMsg);
  }
  
  
  // if ephotonev exactly = the very last valid bin in energy array for this Z,
  // we don't need to interpolate and can leave the function
  if (ephotonev == scat.m_energy[scat.m_f1effindex[Z][1]]) {
    sf1out = scat.m_f1real[scat.m_f1effindex[Z][1]];
    sf2out = scat.m_f2imag[scat.m_f2effindex[Z][1]];
    return;
  }  
  
  // Loop over the f1 scattering factor energy array for this element Z to 
  // find the pair of bins that bracket input energy ephotonev
  for (int i = scat.m_f1effindex[Z][0] ; i < scat.m_f1effindex[Z][1] ; i++) {
    
    // if ephotonev exactly = lower bin, we don't need to interpolate and
    // can leave the function
    if (ephotonev == scat.m_energy[i]) {
      sf1out = scat.m_f1real[i];
      sf2out = scat.m_f2imag[i];
      return;
    }
    
    // otherwise, go through energies to find surrounding bins
    if ( (ephotonev > scat.m_energy[i]) && (ephotonev < scat.m_energy[i+1]) ) {
      fbin = i;
    }
    
  }
  
  // if the edge energy array is to be used, determine if the input energy and 
  // any of the edges lie between the same two energy points in the 
  // interpolation array.
  if (edge.m_nedges > 0) {
    for (int iEdge = 0 ; iEdge < edge.m_nedges ; iEdge++) {
      
      // this edge is not in range if it has a negative index 
      // (was flagged negative, if not in range, inside of initialize_edge())
      if (edge.m_edgearray_sbin[iEdge] < 0) {
        continue;
      }
      
      // if we're at the bin marked as just below the input energy, and it equals
      // the edge energy in this range: 
      if (fbin == edge.m_edgearray_sbin[iEdge]) {
        // is the input energy higher or lower than the edge energy?
        if (ephotonev >= edge.m_energy[iEdge]) {
          // it's higher, so get slope from next two points, if they exist
          if (fbin+2 <= scat.m_f1effindex[Z][1]) {
            deltaEnergy = scat.m_energy[fbin+2] - scat.m_energy[fbin+1];
            slopef1 = (scat.m_f1real[fbin+2] - scat.m_f1real[fbin+1]) / deltaEnergy;
            slopef2 = (scat.m_f2imag[fbin+2] - scat.m_f2imag[fbin+1]) / deltaEnergy;
            sf1out = scat.m_f1real[fbin+1] - ( (scat.m_energy[fbin+1] - ephotonev) * slopef1 );
            sf2out = scat.m_f2imag[fbin+1] - ( (scat.m_energy[fbin+1] - ephotonev) * slopef2 );
            doneSlope = 1;
            return;
          }
          // if the next two points didn't exist, then simple linear 
          // interpolation will be used below
        } else {
          // the input energy is lower than the edge energy, so get slope from 
          // two points below, if they exist
          if (fbin-2 >= scat.m_f1effindex[Z][0]) {
            deltaEnergy = scat.m_energy[fbin-1] - scat.m_energy[fbin-2];
            slopef1 = (scat.m_f1real[fbin-1] - scat.m_f1real[fbin-2]) / deltaEnergy;
            slopef2 = (scat.m_f2imag[fbin-1] - scat.m_f2imag[fbin-2]) / deltaEnergy;
            sf1out = scat.m_f1real[fbin] + ( (ephotonev - scat.m_energy[fbin]) * slopef1 );
            sf2out = scat.m_f2imag[fbin] + ( (ephotonev - scat.m_energy[fbin]) * slopef2 );
            doneSlope = 1;
            return;
          }
          // if the two points below didn't exist, then simple linear 
          // interpolation will be used below
        }
      }
      
    } // end for-loop through edges
  } // end if edge energies are to be used (edge.m_nedges > 0)
  
  // If there is an edge between the two energy bins enclosing the input 
  // energy, the slope for interpolation will already have been calculated and  
  // interpolation already done. If not, then doneSlope will still be 0, 
  // and the interpolation is to be simple linear interpolation.
  if (doneSlope == 0) {
    deltaEnergy = scat.m_energy[fbin+1] - scat.m_energy[fbin];
    slopef1 = (scat.m_f1real[fbin+1] - scat.m_f1real[fbin]) / deltaEnergy;
    slopef2 = (scat.m_f2imag[fbin+1] - scat.m_f2imag[fbin]) / deltaEnergy;
    sf1out = scat.m_f1real[fbin] + ( (ephotonev - scat.m_energy[fbin]) * slopef1 );
    sf2out = scat.m_f2imag[fbin] + ( (ephotonev - scat.m_energy[fbin]) * slopef2 );
    doneSlope = 2;
  }
                
} // interpolatef1f2()


/******************************************************************************/


void multilayerRefl(const double photonlam, const double lambdaSqFac, 
                    const int currGroup, const Surface & surf, 
                    const Angle & angle, const Param & param, 
                    const std::vector<double> & sf1material, 
                    const std::vector<double> & sf2material, 
                    std::vector<double> & sreflect, 
                    std::vector<double> & strans) {
  
  AH_DEBUG << "******************* START OF multilayerRefl() *******************" << std::endl;
  AH_DEBUG << "photonlam = " << photonlam << std::endl;
  AH_DEBUG << "lambdaSqFac = " << lambdaSqFac << std::endl;
  AH_DEBUG << "currGroup = " << currGroup << std::endl;
  
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  const double twopi = 6.283185482025;
  const double fourpi = 12.566370964050;
  
  // optical constants
  std::vector< std::complex<double> > opticalConstant;
  // refractive indices
  std::vector< std::complex<double> > nx;
  // squares of refractive indices
  std::vector< std::complex<double> > nxSq;
  
  // the following will be used for calculation of refl and transm amplitudes
  // in each layer.  S and P polarization.
  // g-factors:
  std::complex<double> gl_sq;
  std::vector< std::complex<double> > gl_s;
  std::vector< std::complex<double> > gl_p;
  // Fresnel coefficients (will be modified by roughness):
  std::vector< std::complex<double> > fresnel_s;
  std::vector< std::complex<double> > fresnel_p;
  // reflected amplitudes:
  std::vector< std::complex<double> > rAmpl_s;
  std::vector< std::complex<double> > rAmpl_p;
  // transmitted amplitudes:
  std::vector< std::complex<double> > tAmpl_s;
  std::vector< std::complex<double> > tAmpl_p;
  
  // these will be output from calcRoughness() routine
  std::complex<double> lroughfac_s;
  std::complex<double> lroughfac_p;
  
  // +++ come up with better names for these
  double rExpArgFac = 0.;
  std::complex<double> rExpArg;
  std::complex<double> rExpFac;
  double tExpArgFac = 0.;
  std::complex<double> tExpArg;
  std::complex<double> tExpFac;
  
  // for calculations inside angle loop
  // +++ add comments
  
  // cos, cos^2, and sin of current angle in angle loop below
  double cosTheta = 0.;
  double cosSqTheta = 0.;
  double sinTheta = 0.;
  double sinSqTheta = 0.;
  
  // cumulative transmission amplitude products, per layer
  std::complex<double> cumlTran_s;
  std::complex<double> cumlTran_p;
  
  // +++ add comments
  std::complex<double> rsAmpl;
  std::complex<double> rpAmpl;
  std::complex<double> tsAmpl;
  std::complex<double> tpAmpl;
  double refl_s_sq = 0.;
  double refl_p_sq = 0.;
  double transmissionfactor = 0.;
  double s_transmission = 0.;
  double p_transmission = 0.;
  double trans_s_productsq = 0.;
  double trans_p_productsq = 0.;
  double modnsqnplusone = 0.;
  
  // groups are 0-indexed, but the group number was passed in
  const int iGroup = currGroup - 1;
  
  // for calls to our cx___ functions
  std::complex<double> cxout;
  
  // -------------------------------------
  
  // loop over layers, including the substrate, to calculate the optical 
  // constants and refractive indices in the coating layers requested for 
  // this group. The substrate is the first thick layer below the multilayer 
  // film; ignore thick layers below this substrate.
  AH_DEBUG << std::endl;
  AH_DEBUG << "PRINT OPTICAL CONSTANT, ETC" << std::endl;
  for (int iLayer = 0 ; iLayer <= surf.m_numlayerplusone[iGroup] ; iLayer++) {
    
//    AH_DEBUG << std::endl;
//    AH_DEBUG << "NEW LAYER" << std::endl;
//    AH_DEBUG << "iLayer = " << iLayer << std::endl;
    
    // if we're at vacuum, explicity set refractive index
    if (iLayer == 0) {
      // opticalConstant isn't used for layer=0
      opticalConstant.push_back(std::complex<double>(0.0,0.0));
      nx.push_back(std::complex<double>(1.0,0.0));
      nxSq.push_back(std::complex<double>(1.0,0.0));
    } else {
      
      // calculate optical constant and refractive index for all other layers
      // up to layer=N+1
      opticalConstant.push_back( std::complex<double>(
            surf.m_layerdensity[iGroup][iLayer] * sf1material[surf.m_layermaterialindex[iGroup][iLayer]], 
            surf.m_layerdensity[iGroup][iLayer] * sf2material[surf.m_layermaterialindex[iGroup][iLayer]] ) );
      nx.push_back( std::complex<double>(1.0 - opticalConstant[iLayer].real(), opticalConstant[iLayer].imag()) );
      
      // store square of refractive index for use later
      //cxasq(nx[iLayer].real(), nx[iLayer].imag(), c_real, c_img);
      cxasq(nx[iLayer], cxout);
      nxSq.push_back( std::complex<double>(cxout) );
      //+++nxSq.push_back(nx[i] * nx[i]);
      /* for debugging
      std::cout <<  "surf.m_layerdensity[iGroup][i] = " << surf.m_layerdensity[iGroup][i] << std::endl;
      std::cout <<  "surf.m_layermaterialindex[iGroup][i] = " << surf.m_layermaterialindex[iGroup][i] << std::endl;
      std::cout <<  "sf1material[surf.m_layermaterialindex[iGroup][i]] = " << sf1material[surf.m_layermaterialindex[iGroup][i]] << std::endl;
      std::cout <<  "sf2material[surf.m_layermaterialindex[iGroup][i]] = " << sf2material[surf.m_layermaterialindex[iGroup][i]] << std::endl;
      std::cout <<  "opticalConstant[i] = " << opticalConstant[i] << std::endl;
      */
    }
    
    //std::cout <<  "nx[i] = " << nx[i] << std::endl;
    //std::cout <<  "nxSq[i] = " << nxSq[i] << std::endl;
  
  } // end for-loop through layers
  
  // All energy-dependent quantities should be calculated by now. Begin the 
  // nested loops for incident angle and layers to calculate the reflectivity 
  // and transmission.
  AH_DEBUG << std::endl;
  AH_DEBUG << "LOOP THROUGH ANGLES, LAYERS" << std::endl;
  
  // +++ got a segfault in here somewhere, before next AH_DEBUG, when reached group 2 for incorrect sxt file (with three groups, single layer, negative layer nums)
  for (int iAngle = 0 ; iAngle < angle.m_numangles ; iAngle++) {
    
    // get cos, cos^2, sin, and sin^2 of current angle (assumes radians)
    cosTheta = cos(angle.m_incidentangle[iAngle]);
    cosSqTheta = cosTheta * cosTheta;
    sinTheta = sin(angle.m_incidentangle[iAngle]);
    sinSqTheta = sinTheta * sinTheta;
    
    /* for debugging
    AH_DEBUG << std::endl;
    AH_DEBUG << std::endl;
    AH_DEBUG << "NEW ANGLE" << std::endl;
    std::cout << std::fixed;
    std::cout << std::setprecision(18) << "theta in rad = " << angle.m_incidentangle[iAngle] << std::endl;
    AH_DEBUG << "theta in degrees = " << angle.m_incidentangle[iAngle] * 180 / (3.14) << std::endl;
    std::cout << "cosTheta = " << cosTheta << std::endl;
    AH_DEBUG << "cosSqTheta = " << cosSqTheta << std::endl;
    AH_DEBUG << "sinTheta = " << sinTheta << std::endl;
    AH_DEBUG << "sinSqTheta = " << sinSqTheta << std::endl;
    AH_DEBUG << "sin^2 + cos^2 = " << cosSqTheta + sinSqTheta << std::endl;
    */
  
    // +++ are all of these really necessary as vectors?  Or can they just be 
    //     like gl_sq?  tAmpl_ and fresnel_ dont' need vectors
    // reset variables and clear vectors that will be used in the layer loop
    cumlTran_s = 1.;
    cumlTran_p = 1.;
    gl_s.clear();
    gl_p.clear();
    rAmpl_s.clear();
    rAmpl_p.clear();
    tAmpl_s.clear();
    tAmpl_p.clear();
    fresnel_s.clear();
    fresnel_p.clear();
    // +++ reset refl_s_sq?  and rename it?
    
//    AH_DEBUG << "surf.m_numlayerplusone[iGroup] = " << surf.m_numlayerplusone[iGroup] << std::endl;
    
    // the vectors used need to be resized first, so we can always assign and 
    // reference using the index, not assign using push_back().  push_back()
    // will insert at the beginning (starting at index 0), but since the
    // following layer loop is in reverse, we want to start at layer 
    // surf.m_numlayerplusone[iGroup]
    gl_s.resize(surf.m_numlayerplusone[iGroup]+1);
    gl_p.resize(surf.m_numlayerplusone[iGroup]+1);
    rAmpl_s.resize(surf.m_numlayerplusone[iGroup]+1);
    rAmpl_p.resize(surf.m_numlayerplusone[iGroup]+1);
    tAmpl_s.resize(surf.m_numlayerplusone[iGroup]+1);
    tAmpl_p.resize(surf.m_numlayerplusone[iGroup]+1);
    fresnel_s.resize(surf.m_numlayerplusone[iGroup]+1);
    fresnel_p.resize(surf.m_numlayerplusone[iGroup]+1);
    
    // loop over layers in reverse order to do bottom (substrate) layer first
    // ignore thick layers below substrate (iLayer > surf.m_numlayerplusone[iGroup])
    for (int iLayer = surf.m_numlayerplusone[iGroup] ; iLayer >= 0 ; iLayer--) {
      
//      AH_DEBUG << std::endl;
//      AH_DEBUG << "NEW LAYER" << std::endl;
//      AH_DEBUG << "iLayer = " << iLayer << " (surf.m_numlayerplusone[iGroup] = " << surf.m_numlayerplusone[iGroup] << ")" << std::endl;
      
      // calc "g" values for this layer
      if (iLayer == 0) {
        
        // for the vacuum layer (N=0), explicitly use the fact that the refractive
        // index is real and equal to 1.0
        gl_sq = std::complex<double>(sinSqTheta, 0.0);
        gl_s[iLayer] = std::complex<double>(sinTheta, 0.0);
        gl_p[iLayer] = std::complex<double>(gl_s[iLayer]);
        
      } else {
      
        gl_sq = std::complex<double>( nxSq[iLayer].real() - cosSqTheta, nxSq[iLayer].imag() );
        
        //cxpower(gl_sq.real(), gl_sq.imag(), 0.5, c_real, c_img);
        cxpower(gl_sq, 0.5, cxout);
        gl_s[iLayer] = std::complex<double>(cxout);
        cxnumratio(gl_s[iLayer], nxSq[iLayer], cxout);
        gl_p[iLayer] = std::complex<double>(cxout);
        
        //+++gl_s[iLayer] = sqrt(gl_sq);
        //gl_p[iLayer] = gl_s[iLayer] / nxSq[iLayer];
      
      }
      
      /*std::cout << "gl_sq = " << gl_sq << std::endl;
      std::cout << "gl_s[iLayer] = " << gl_s[iLayer] << std::endl;
      std::cout << "gl_p[iLayer] = " << gl_p[iLayer] << std::endl;
      */
    
      // if we're doing substrate layer (iLayer=N+1), apply boundary condition
      if (iLayer == surf.m_numlayerplusone[iGroup]) {
        rAmpl_s[iLayer] = 0.;
        rAmpl_p[iLayer] = 0.;
        
      } else {
        // only do the following if we're not at the substrate layer
        // because we'll be referring to layers in the [iLayer+1] index
        
        // for the Fresnel coefficients (before the roughness factor is applied)
        // we want ratios with g factors in numerator and denominator, but 
        // many cancel out.  ratio reduces to (g_l - g_l+1)/(g_l + g_l+1)
        
        cxdiffoversum(gl_s[iLayer], gl_s[iLayer+1], cxout);
        fresnel_s[iLayer] = std::complex<double>(cxout);
        cxdiffoversum(gl_p[iLayer], gl_p[iLayer+1], cxout);
        fresnel_p[iLayer] = std::complex<double>(cxout);
        
        //std::cout << "fresnel_s[iLayer] = " << fresnel_s[iLayer] << std::endl;
        //std::cout << "fresnel_p[iLayer] = " << fresnel_p[iLayer] << std::endl;
        
        //+++fresnel_s[iLayer] = std::complex<double>( (gl_s[iLayer] - gl_s[iLayer+1]) / (gl_s[iLayer] + gl_s[iLayer+1]) );
        //fresnel_p[iLayer] = std::complex<double>( (gl_p[iLayer] - gl_p[iLayer+1]) / (gl_p[iLayer] + gl_p[iLayer+1]) );
        
        // Now calculate the (complex) roughness factor for the reduction in 
        // the reflected amplitude. Use the roughness parameter squared for the 
        // appropriate group and layers, in sigmaroughnessSq, which is the 
        // square of the roughness parameter array from the SURFACE extension 
        // (in Angstrom squared). The string “roughmodel” (=NC, DW, or none) 
        // determines which roughness model is used. Note that the NC option 
        // also uses information in the next adjacent layer (iLayer+1).
        // The outputs of the routine are the complex roughness factors:
        // lroughfac_s for S polarization, and lroughfac_p for P polarization.
        
        // reset the output files in this loop before calling calcRoughness
        lroughfac_s = 0.;
        lroughfac_p = 0.;
        // call calcRoughness with the sigmaroughnesssq for the next layer, 
        // because that's the roughness param we care about for this layer
        calcRoughness(param.m_roughmodel, surf.m_sigmaroughnesssq[iGroup][iLayer+1], 
                      lambdaSqFac, gl_sq, gl_s[iLayer], gl_p[iLayer], gl_s[iLayer+1], gl_p[iLayer+1], 
                      lroughfac_s, lroughfac_p);
        
        // multiply the Fresnel coefficients by the roughness factor
        
        //cxatimesb(fresnel_s[iLayer].real(), fresnel_s[iLayer].imag(), lroughfac_s.real(), lroughfac_s.imag(), c_real, c_img);
        cxatimesb(fresnel_s[iLayer], lroughfac_s, cxout);
        fresnel_s[iLayer] = std::complex<double>(cxout);
        //cxatimesb(fresnel_p[iLayer].real(), fresnel_p[iLayer].imag(), lroughfac_p.real(), lroughfac_p.imag(), c_real, c_img);
        cxatimesb(fresnel_p[iLayer], lroughfac_p, cxout);
        fresnel_p[iLayer] = std::complex<double>(cxout);
        
        //+++fresnel_s[iLayer] = fresnel_s[iLayer] * lroughfac_s;
        //fresnel_p[iLayer] = fresnel_p[iLayer] * lroughfac_p;
        
        // now calc the reflection and transmission amplitudes
        
        // +++ read this comment more
        // If iLayer>0 and iLayer<N+1 we need the exponential factor below for the 
        // reflected amplitude. For transmission the exponential factor is not 
        // used for layer N and higher. In the current if-block iLayer is <N+1
        // so we only need to exclude iLayer=N for transmission. 
        // Note that the exponential factors are different for reflection and 
        // transmission.
        // For layer N, the reflected amplitude is simply equal to the Fresnel 
        // coefficient (from the recursion formula, since the (N+1)th layer has 
        // zero amplitude by definition). 
        // Note that exponential factors are the same for S and P polarization.
        
        // don't do this for the iLayer=0 layer
        if (iLayer > 0) {
          
          // calc exponential factor for refl ampl for layer 1 to N
          rExpArgFac = fourpi * surf.m_layerthickness[iGroup][iLayer] / photonlam;
          // we need exp(i * (4pi/lambda) * d * g) so switch real and imag components
          rExpArg = std::complex<double>(-1. * rExpArgFac * gl_s[iLayer].imag(),
                                         rExpArgFac * gl_s[iLayer].real());
          cxexponent(rExpArg, cxout);
          rExpFac = std::complex<double>(cxout);
          //+++rExpFac = exp(rExpArg);
          
          //std::cout << "rExpFac = " << rExpFac << std::endl;
          
          
        } // end if iLayer > 0 block
        
        // calc exponential factor for trans ampl for layer 1 to N-1
        // (this is needed even if iLayer=0)
        if (iLayer < surf.m_numlayerplusone[iGroup]-1) {
          tExpArgFac = twopi * surf.m_layerthickness[iGroup][iLayer+1] / photonlam;
          // we need exp(i * (2pi/lambda) * d_l+1 * g_l+1) so switch real and imag components
          tExpArg = std::complex<double>(-1. * tExpArgFac * gl_s[iLayer+1].imag(),
                                         tExpArgFac * gl_s[iLayer+1].real());
          
          cxexponent(tExpArg, cxout);
          tExpFac = std::complex<double>(cxout);
          //+++tExpFac = exp(tExpArg);
          
          //std::cout << "tExpFac = " << tExpFac << std::endl;
          
        } // end if iLayer < N block
          
        
        // if layer=N we don't need a lengthly complex number calculation
        // because the refl ampl is equal to Fresnel coefficient, and trans
        // ampl is equal to 1+(refl ampl)
        if (iLayer == surf.m_numlayerplusone[iGroup]-1) {
          
          rAmpl_s[iLayer] = fresnel_s[iLayer];
          rAmpl_p[iLayer] = fresnel_p[iLayer];
          //std::cout << "rAmpl_s[iLayer] = " << rAmpl_s[iLayer] << std::endl;
          //std::cout << "rAmpl_p[iLayer] = " << rAmpl_p[iLayer] << std::endl;
          
          // multiply S and P pol. refl ampl by exp factor calculated earlier
          // only if iLayer != 0, because rExpFac hasn't been set in that case
          if ( (iLayer > 0) && (surf.m_numlayerplusone[iGroup] > 1) ) {
            
            //cxatimesb(rAmpl_s[iLayer].real(), rAmpl_s[iLayer].imag(), rExpFac.real(), rExpFac.imag(), c_real, c_img);
            cxatimesb(rAmpl_s[iLayer], rExpFac, cxout);
            rAmpl_s[iLayer] = std::complex<double>(cxout);
            cxatimesb(rAmpl_p[iLayer], rExpFac, cxout);
            rAmpl_p[iLayer] = std::complex<double>(cxout);
            
            //rAmpl_s[iLayer] = rAmpl_s[iLayer] * rExpFac;
            //+++rAmpl_p[iLayer] = rAmpl_p[iLayer] * rExpFac;
          }
          //std::cout << "rExpFac = " << rExpFac << std::endl;
          //std::cout << "rAmpl_s[iLayer] = " << rAmpl_s[iLayer] << std::endl;
          //std::cout << "rAmpl_p[iLayer] = " << rAmpl_p[iLayer] << std::endl;
          
          
          tAmpl_s[iLayer] = std::complex<double>(1. + fresnel_s[iLayer].real(),
                                                 fresnel_s[iLayer].imag());
          tAmpl_p[iLayer] = std::complex<double>(1. + fresnel_p[iLayer].real(),
                                                 fresnel_p[iLayer].imag());
          
          //std::cout << "tAmpl_s[iLayer] = " << tAmpl_s[iLayer] << std::endl;
          //std::cout << "tAmpl_p[iLayer] = " << tAmpl_p[iLayer] << std::endl;
         
         
        } else if (iLayer < surf.m_numlayerplusone[iGroup]-1) {
          // if iLayer<N we need to do full calculation.
          
          // reflection: recursive relation involving Fresnel coefficients
          // and refl ampl[iLayer+1].  uses (a + b)/(1 + ab)
          // +++ I'd like a better name for these variables
          cxaplusboveronepab(fresnel_s[iLayer], rAmpl_s[iLayer+1], cxout);
          rsAmpl = std::complex<double>(cxout);
          cxaplusboveronepab(fresnel_p[iLayer], rAmpl_p[iLayer+1], cxout);
          rpAmpl = std::complex<double>(cxout);
          
          // +++rsAmpl = (fresnel_s[iLayer] - rAmpl_s[iLayer+1]) / (1. + fresnel_s[iLayer] * rAmpl_s[iLayer+1]);
          //rpAmpl = (fresnel_p[iLayer] - rAmpl_p[iLayer+1]) / (1. + fresnel_p[iLayer] * rAmpl_p[iLayer+1]);
          
          // transmission: recursive relation involving Fresnel coefficients
          // and trans ampl[iLayer+1].  uses (1 + a)/(1 + ab)
          cxoneplusaoveronepab(fresnel_s[iLayer], rAmpl_s[iLayer+1], cxout);
          tsAmpl = std::complex<double>(cxout);
          cxoneplusaoveronepab(fresnel_p[iLayer], rAmpl_p[iLayer+1], cxout);
          tpAmpl = std::complex<double>(cxout);
          
          //+++tsAmpl = (1. + fresnel_s[iLayer]) / (1. + fresnel_s[iLayer] * rAmpl_s[iLayer+1]);
          //tpAmpl = (1. + fresnel_p[iLayer]) / (1. + fresnel_p[iLayer] * rAmpl_p[iLayer+1]);
         
         
          // transmission, S pol
          cxatimesb(tsAmpl, tExpFac, cxout);
          tAmpl_s[iLayer] = std::complex<double>(cxout);
          //+++tAmpl_s[iLayer] = (tsAmpl * tExpFac);
          // transmission, P pol
          cxatimesb(tpAmpl, tExpFac, cxout);
          tAmpl_p[iLayer] = std::complex<double>(cxout);
          //+++tAmpl_p[iLayer] = (tpAmpl * tExpFac);
         
         
          // if iLayer>0 (and <N), multiply by one of the earlier exponentials
          if (iLayer > 0) {
            // reflection, S pol
            cxatimesb(rsAmpl, rExpFac, cxout);
            rAmpl_s[iLayer] = std::complex<double>(cxout);
            //+++rAmpl_s[iLayer] = (rsAmpl * rExpFac);
            // reflection, P pol
            cxatimesb(rpAmpl, rExpFac, cxout);
            rAmpl_p[iLayer] = std::complex<double>(cxout);
            //+++rAmpl_p[iLayer] = (rpAmpl * rExpFac);
            
            
          } else {
            // if iLayer=0, make reflection and transmission amplitudes equal 
            // to amplitudes calculated earlier without exponential factor
            
            // reflection, S pol
            rAmpl_s[iLayer] = rsAmpl;
            // reflection, P pol
            rAmpl_p[iLayer] = rpAmpl;
            
          } // end if iLayer > 0 else block
          
        } // end if iLayer = N else if iLayer < N block
        
        // cumulative (complex) product of the transmission amplitudes for 
        // iLayer=0 to N, to later calculate the net transmission
        if (iLayer < surf.m_numlayerplusone[iGroup]) {
          cxatimesb(cumlTran_s, tAmpl_s[iLayer], cxout);
          cumlTran_s = std::complex<double>(cxout);
          cxatimesb(cumlTran_p, tAmpl_p[iLayer], cxout);
          cumlTran_p = std::complex<double>(cxout);
        }
        
      } // end if iLayer = N+1 else block
      
    } // end layer loop
    
    // now we should have the reflection amplitudes at the top of the first 
    // layer (iLayer=0) for angle bin iAngle, so we can fill in the 
    // reflectivity for this angle bin by calculating the modulus squared of
    // the S and P reflected amplitudes. The final reflectivity is the simple
    // average of the latter for S and P polarizations.
    refl_s_sq = (rAmpl_s[0].real() * rAmpl_s[0].real()) + (rAmpl_s[0].imag() * rAmpl_s[0].imag());
    refl_p_sq = (rAmpl_p[0].real() * rAmpl_p[0].real()) + (rAmpl_p[0].imag() * rAmpl_p[0].imag());
    
    //+++refl_s_sq = std::norm(rAmpl_s[0]);
    //refl_p_sq = std::norm(rAmpl_p[0]);
    sreflect.push_back( (refl_s_sq + refl_p_sq) / 2 );
    //std::cout << "sreflect.push_back( (refl_s_sq + refl_p_sq) / 2 ) = " << (refl_s_sq + refl_p_sq) / 2 << std::endl;
    
    // Next calculate the transmisson at the film/substrate boundary. First 
    // calculate the modulus squared of the product of the transmitted 
    // amplitudes from layer 0 to N:
    
    trans_s_productsq = (cumlTran_s.real() * cumlTran_s.real()) + (cumlTran_s.imag() * cumlTran_s.imag());
    trans_p_productsq = (cumlTran_p.real() * cumlTran_p.real()) + (cumlTran_p.imag() * cumlTran_p.imag());
    
    //std::cout << "trans_s_productsq = " << trans_s_productsq << std::endl;
    // +++ these were off 1.999356839064246660 vs 1.999356839064246882
    //std::cout << "std::norm(cumlTran_s) = " << std::norm(cumlTran_s) << std::endl;
    //std::cout << "trans_p_productsq = " << trans_p_productsq << std::endl;
    //std::cout << "std::norm(cumlTran_p) = " << std::norm(cumlTran_p) << std::endl;
    
    //+++trans_s_productsq = std::norm(cumlTran_s);
    //+++ltrans_p_productsq = std::norm(cumlTran_p);
    
    // The reciprocal of the modulus squared of the complex refractive index of 
    // layer N+1 (the substrate layer) is needed for P-polarization transmission
    modnsqnplusone = (nx[surf.m_numlayerplusone[iGroup]].real() * nx[surf.m_numlayerplusone[iGroup]].real()) + (nx[surf.m_numlayerplusone[iGroup]].imag() * nx[surf.m_numlayerplusone[iGroup]].imag());
    //+++modnsqnplusone = std::norm(nx[surf.m_numlayerplusone[iGroup]]);
    
    //std::cout << "modnsqnplusone = " << modnsqnplusone << std::endl;
    //std::cout << "std::norm(nx[surf.m_numlayerplusone[iGroup]]) = " << std::norm(nx[surf.m_numlayerplusone[iGroup]]) << std::endl;
    
    // if this is a single layer surface (first layer is neg), 
    // simply write T=1-R
    if (surf.m_layernumber[0] < 0) {
      strans.push_back(1.0 - sreflect[iAngle]);
    } else {
    
      // +++ per trf, more work to do here?
      // transmissionfactor is common to both S and P polarization; 
      // however, it will probably have to be recast because it blows up at small 
      // incident angles. For the time being we simply make sure that the 
      // incident angle is never zero in the expression; the case of zero 
      // incident angle is handled explicitly to give zero net transmission.
      if (sinTheta > 0.0) {
        transmissionfactor = gl_s[surf.m_numlayerplusone[iGroup]].real() / sinTheta;
        
        //std::cout << "transmissionfactor = " << transmissionfactor << std::endl;
        
        // S and P-polarization transmission: 
        s_transmission = transmissionfactor * trans_s_productsq ;
        p_transmission = transmissionfactor * trans_p_productsq / modnsqnplusone;
        // Net transmission is the average of S and P polarization transmission 
        strans.push_back( (s_transmission + p_transmission) / 2 );
      } else {
        strans.push_back(0.0);
      }
    
    }
    
  } // end incident angle loop

} // multilayerRefl()


/******************************************************************************/


void calcRoughness(const std::string & roughModel, const double sigmaRoughnessSq, 
                   const double lambdaSqFac, const std::complex<double> & g1sq,
                   const std::complex<double> g1_s, 
                   const std::complex<double> g1_p,
                   const std::complex<double> g2_s,
                   const std::complex<double> g2_p,
                   std::complex<double> & lroughfac_s,
                   std::complex<double> & lroughfac_p) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
 
  double argFactor = -1. * lambdaSqFac * sigmaRoughnessSq;
  std::complex<double> arg_s;
  std::complex<double> arg_p;
  std::complex<double> cxout;       // for calls to cx__ functions
  
  // -------------------------------------
  

  if ( isEqualCaseInsens(roughModel, "NC") ) {
    
    cxatimesb(g1_s, g2_s, cxout);
    //+++arg = g1_s * g2_s;   // +++ ftdiff found no differences between this
                              //     and using cxatimesb
    //arg_s = argFactor * arg;
    arg_s = std::complex<double>(argFactor * cxout.real(), argFactor * cxout.imag());
    
    cxexponent(arg_s, cxout);
    lroughfac_s = std::complex<double>(cxout);
    lroughfac_p = lroughfac_s;
    //+++lroughfac_s = exp(arg_s);
    //lroughfac_p = exp(arg_p);
    
    
    
//    cxatimesb(g1_s, g2_s, cxout);
//    //+++arg = g1_s * g2_s;   // +++ ftdiff found no differences between this
//                              //     and using cxatimesb
//    arg_s = std::complex<double>(argFactor * cxout.real(), argFactor * cxout.imag());
//    //arg_s = argFactor * arg;
//    
//    cxatimesb(g1_p, g2_p, cxout);
//    //+++arg = g1_p * g2_p;
//    arg_p = std::complex<double>(argFactor * cxout.real(), argFactor * cxout.imag());
//    //+++arg_p = argFactor * arg;
//    
//    cxexponent(arg_s, cxout);
//    lroughfac_s = std::complex<double>(cxout);
//    cxexponent(arg_p, cxout);
//    lroughfac_p = std::complex<double>(cxout);
//    //+++lroughfac_s = exp(arg_s);
//    //lroughfac_p = exp(arg_p);
    
    
    
  } else if ( isEqualCaseInsens(roughModel, "DW") ) {
    
    //+++ should I be passing in glsq[iLayer+1], so I should be storing glsq in a vector
    //    see page 64 in trf
    
    arg_s = std::complex<double>(argFactor * g1sq.real(), argFactor * g1sq.imag());
    //+++arg_s = argFactor * g1sq;
    
    cxexponent(arg_s, cxout);
    lroughfac_s = std::complex<double>(cxout);
    //+++lroughfac_s = exp(arg_s);
    lroughfac_p = lroughfac_s;
        
  } else if ( isEqualCaseInsens(roughModel, "NONE") ) {
    
    lroughfac_s = 1.;
    lroughfac_p = 1.;
    
  } else {
    
    AH_INFO(ahlog::HIGH) 
      << "Requested roughness model is not supported – assuming NONE" 
      << std::endl;
    lroughfac_s = 1.;
    lroughfac_p = 1.;
    
  }

} // calcRoughness()



/**********************************************
 * ********************************************
 * 		getPar*() functions
 * ********************************************
**********************************************/


std::string reportGetParErr(const std::string & parname, int status) {
  std::stringstream msg;
  msg << "problem getting parameter: " << parname << " (APE status: " 
      << status << ")";
  return msg.str();
}


std::string getParString(const std::string & parname) {
  char* t_par;
  if (int status = ape_trad_query_string(parname.c_str(),&t_par) != eOK )
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  std::string out = (std::string)t_par;
  free(t_par); t_par=0;
  return out;
}


bool getParBool(const std::string & parname) {
  char t_par;
  if (int status = ape_trad_query_bool(parname.c_str(),&t_par) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  if (t_par != 0) return true;
  return false;
}


double getParDouble(const std::string & parname) {
  double out;
  if (int status = ape_trad_query_double(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}


int getParInt(const std::string & parname) {
  int out;
  if (int status = ape_trad_query_int(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}


std::string convertCharToString(char * input) {
  std::string out = (std::string)input;
  free(input); input=0;
  return out;
}


std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression,
                    const std::string & telescop) {
  
  // initialize the return variable to the input filename. If it's not CALDB,
  // this is what will be returned.  
  std::string filenameFull = filename;
  
  // these will only be updated if the user passed in valid a datetime
  std::string querydate = "-";
  std::string querytime = "-";
  
  if (isEqualCaseInsens(filename, "CALDB")) {
    
    AH_INFO(ahlog::HIGH) << "Searching CALDB for "<<filetype<<" file" << std::endl;
    
    // check if relevant environment variables are set
    if (getenv("CALDB") == NULL) {
      AH_THROW_RUNTIME("$CALDB environment variable not defined");
    }
    if (getenv("CALDBCONFIG") == NULL) {
      AH_THROW_RUNTIME("$CALDBCONFIG environment variable not defined");
    }
    if (getenv("CALDBALIAS") == NULL) {
      AH_THROW_RUNTIME("$CALDBALIAS environment variable not defined");
    }
    
    // Log location of user's CALDB
    AH_INFO(ahlog::LOW) << "$CALDB="<<getenv("CALDB") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBCONFIG="<<getenv("CALDBCONFIG") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBALIAS="<<getenv("CALDBALIAS") << std::endl;
    
    // get the date and time search values
    if (isEqualCaseInsens(datetime, "NOW")) {
      querydate = "NOW";
      querytime = "NOW";
    } else if (datetime == "-") {
      querydate = "-";
      querytime = "-";
    } else if (datetime.length() == 0) {
      // empty string
      querydate = "-";
      querytime = "-";
    } else if (datetime.length() == 10) {
      // just a date was passed in
      querydate = datetime.substr(0,10);
      querytime = "-";
    } else if (datetime.find("T") != std::string::npos) {
      // datetime should be in the format yyyy-mm-ddThh:mm:ss
      querydate = datetime.substr(0,10);
      querytime = datetime.substr(11,8);
    } else {
      AH_THROW_RUNTIME("The datetime parameter ("+datetime+") for the CALDB search must be in a valid format (either 'now', '-', '', 'yyyy-mm-dd', or 'yyyy-mm-ddThh::mm:ss')");
    }
    
    const char* tele      = telescop.c_str();
    const char* instr     = instrume.c_str();
    const char* det       = detnam.c_str();
    const char* filt      = "-";
    const char* codenam   = codename.c_str();
    const char* strtdate  = querydate.c_str();
    const char* strttime  = querytime.c_str();
    const char* stpdate   = "-";
    const char* stptime   = "-";
    const char* expr      = expression.c_str();

    const int fnamesize   = 256;    // max size of filename
    const int onlinesize  = 10;     // size of online 
    int maxret            = 1;
    char filename[fnamesize];
    char* fnptr           = filename;
    long extno            = 0;
    char online[onlinesize];
    char* onptr           = online;
    int nret              = 0;
    int nfound            = 0;
    int status            = 0;
    
    AH_INFO(ahlog::HIGH) << "Querying CALDB with these parameters: " << std::endl; 
    AH_INFO(ahlog::HIGH) << "   TELESCOPE:  "  << tele << std::endl;
    AH_INFO(ahlog::HIGH) << "   INSTRUMENT: "  << instr << std::endl;
    AH_INFO(ahlog::HIGH) << "   DETNAM:     "  << det << std::endl;
    AH_INFO(ahlog::HIGH) << "   FILTER:     "  << filt << std::endl;
    AH_INFO(ahlog::HIGH) << "   CODENAME:   "  << codenam << std::endl;
    AH_INFO(ahlog::HIGH) << "   START DATE: "  << strtdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   START TIME: "  << strttime << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP DATE:  "  << stpdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP TIME:  "  << stptime << std::endl;
    AH_INFO(ahlog::HIGH) << "   EXPRESSION: "  << expr << std::endl;

    HDgtcalf(tele,instr,det,filt,codenam,strtdate,strttime,
             stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
             &onptr,&nret,&nfound,&status);

    if (status != 0) {
      std::stringstream serr;
      serr << "Could not get "<<filetype<<" file from CALDB; "
           << "HDgtcalf status: " << status;
      AH_THROW_RUNTIME(serr.str());
    }

    if (nfound == 0) {
      AH_THROW_RUNTIME("No "+filetype+" file found in CALDB");
    }
    
    // include extended syntax, extension number in string to be returned
    std::stringstream tmp;
    tmp << filename << "[" << extno << "]";
    filenameFull = tmp.str();

    AH_INFO(ahlog::HIGH) << "CALDB " << filetype << " file: " << filenameFull << std::endl;
    
  } // end-if CALDB
   
  return filenameFull;

} // end resolve()


void writeParametersToLog() {
  char ** par_names = 0;
  int ii = 0;
  std::stringstream out;

  // start output line with name of tool
  char toolname[128];          // 128 is the size used in headas_toolname.c
  get_toolname(toolname);
  out << toolname;

  // write parameters to output line
  ape_trad_get_par_names(&par_names);
  while(par_names[ii] != NULL){
    char* value=0;
    ape_trad_get_string(par_names[ii],&value);
    out << " '" << par_names[ii] << "=" << value << "'";
    free(value);
    ii++;
  }

  // Write parameter list to log file
  AH_INFO(ahlog::HIGH) << "START PARAMETER LIST:" << std::endl;
  AH_INFO(ahlog::HIGH) << out.str() << std::endl;
  AH_INFO(ahlog::HIGH) << "END PARAMETER LIST" << std::endl << std::endl;

  // Clean up parameter list array
  ape_util_free_string_array(par_names);

} // end of writeParametersToLog


/**********************************************
 * ********************************************
* 		misc functions
 * ********************************************
**********************************************/



void listStringsToDoubles(const std::string & stringList, std::vector<double> & doubleList) {
  
  double tempDouble;
  std::string errorMsg;
  std::string::size_type begin = stringList.find_first_not_of(" \t\n");
  std::string::size_type end;
  
  // empty out the input array
  doubleList.clear();
  
  // loop through the input string which should be a list
  while (std::string::npos != begin) {
    
    // get the position of to the first space, comma, tab, or newline
    end = stringList.find_first_of(" ,\t\n", begin);

    std::istringstream ss(stringList.substr(begin, end - begin));
    if ( (ss >> tempDouble) && (ss >> std::ws).eof() ) {
      doubleList.push_back(tempDouble);
    } else {
      errorMsg = "There was an error converting input parameter '";
      errorMsg.append(stringList);
      errorMsg.append("' to a list of real numbers. If this is supposed to be a filename, verify that the file exists.");
      AH_THROW_RUNTIME(errorMsg);
    }
    
    // now get the starting position of the next non-space character
    begin = stringList.find_first_not_of(" ,\t\n", end);
    
  }

} // end listStringsToDoubles()



energyUnits_e getEnergyUnit(fitsfile * fits_fp, const std::string & filename,
                          int energyColNum) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  //+++ can't i get filename from fits_fp?
  
  int status = 0;                       // for cfitsio function calls
  
  char value[FLEN_VALUE] = "";          // getting TUNITn keyword
  char keyname[FLEN_KEYWORD] = "";      // getting TUNITn keyword
  std::string energyUnit;
  std::string errorMsg;
  
  // -------------------------------------
  
  sprintf(keyname, "TUNIT%d", energyColNum);
  fits_read_keyword(fits_fp, keyname, value, 0, &status);
  checkForFITSError(status, "checking units of energy in", filename);
  energyUnit = (std::string)value;
  energyUnit.erase(std::remove_if(energyUnit.begin(), energyUnit.end(), isspace), energyUnit.end());
  energyUnit.erase(std::remove(energyUnit.begin(), energyUnit.end(), '\''), energyUnit.end());
  
  if (isEqualCaseInsens(energyUnit,"keV")) {
    return e_keV;
  } else if (isEqualCaseInsens(energyUnit,"eV")) {
    return e_eV;
  } else {
    errorMsg = "Energy column must have TUNIT of eV or keV, in file ";
    errorMsg.append(filename);
    AH_THROW_RUNTIME(errorMsg);
  }
  
} // end getEnergyUnit()


void storeEnergyIneV(energyUnits_e energyUnit, std::vector<double> & energyGrid) {
  if (e_keV == energyUnit) {
    try {
      std::transform(energyGrid.begin(), energyGrid.end(), 
                     energyGrid.begin(),
                     std::bind2nd(std::multiplies<double>(), s_keVToeV));
    } catch(const std::exception & e) {
      AH_THROW_RUNTIME("Error converting energy to keV");
    }
  }
  
}


std::string intToString(const int value) {
	std::ostringstream os;
	os << value;
	return os.str();
}


std::string doubleToString(const double value) {
	std::ostringstream os;
  // make sure the entire double is passed as a string
	os << std::setprecision( 15 ) << value;
	return os.str();
}


std::string longToString(long value) {
	std::ostringstream os;
	os << value;
	return os.str();
}


bool isEqualCaseInsens(const std::string & str1, const std::string & str2) {
   // check that they're the same length
   if ( str1.length() != str2.length() ) {
     return false;
   }
   // now check for equality
   for (unsigned int i = 0 ; i < str1.length() ; i++) {
    if ( toupper(str1[i]) != toupper(str2[i]) )
      return false;
  }
  // if we made it out of the loop, they must be equal
  return true;
}


int getMaxInt(std::vector<int> & vec) {
  std::vector<int>::iterator iter;
  iter = std::max_element(vec.begin(), vec.end());
  return *iter;
}


std::string getFilename(const std::string & str) {
  // look for the last delimeter of a file path
  int found = str.find_last_of("/\\");
  // return only the filename, at the end of the file
  return str.substr(found+1);
}


void checkForFITSError(const int status, const std::string & doing, const std::string & filename) {
  
  // the stream to hold the message
  std::stringstream msg;
    
  // if it was a success, and we're in debug mode, print a message to log
  /*if (status == 0 && debug) {
    msg << TOOLNAME << " debug: FITSIO success while " << doing << " file " << filename;
  }*/
 
  // if the returned status from the fits call isn't zero, terminate the
  // program with a message
  if(status != 0) {
    msg << TOOLNAME << " error: FITSIO error while " << doing << " file " << filename;
    fits_report_error(stdout,status);
    AH_THROW_RUNTIME(msg.str());
  }
  
} // end checkForFITSError()



/**********************************************
 * ********************************************
* 		complex functions
 * ********************************************
**********************************************/


void cxasq(const std::complex<double> & cxin, std::complex<double> & cxout) {

  double a_real_sq = cxin.real() * cxin.real();
  double a_img_sq  = cxin.imag() * cxin.imag();
  
  cxout = std::complex<double>( a_real_sq - a_img_sq, 
                                2.0e0 * cxin.real() * cxin.imag() );
  
}


void cxpower(const std::complex<double> & cxin, double xpower, std::complex<double> & cxout) {
  
  double pi = 3.141592653589793;
  double twopi = 6.283185482025;
  double ahypotenuse = sqrt( (cxin.real() * cxin.real()) + (cxin.imag() * cxin.imag()) );
  double cos_psi = cxin.real() / ahypotenuse;
  double sin_psi = cxin.imag() / ahypotenuse;
  double psi = 0;
  
  if (cxin.imag() == 0. || cxin.imag() == -0.0) {
    
    if (cxin.real() < 0.0) {
      psi=pi;
    } else if (cxin.real() > 0.0) {
      cxout = std::complex<double>( pow(cxin.real(),xpower), 0.0);
      return;
    } else if (cxin.real() == 0.0 || cxin.real() == -0.0) {
      cxout = std::complex<double>(0.0, 0.0);
      return;
    }
    
  } else {
    
    if (sin_psi > 0.0) { 
      psi = acos(cos_psi);
    } else {
      psi = twopi-acos(cos_psi);
    }
    
  }
  
  double xpsi = xpower * psi;
  double cos_xpsi = cos(xpsi);
  double sin_xpsi = sin(xpsi);
  double cmagnitude = pow(ahypotenuse, xpower);
  cxout = std::complex<double>( cmagnitude * cos_xpsi,
                                cmagnitude * sin_xpsi );

}


void cxexponent(const std::complex<double> & cxin, std::complex<double> & cxout) {
  
  double cnorm = exp(cxin.real());
  cxout = std::complex<double>( cnorm * cos(cxin.imag()), 
                                cnorm * sin(cxin.imag()) );

}


void cxatimesb(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout) {
  cxout = std::complex<double>( (cx1.real() * cx2.real()) - (cx1.imag() * cx2.imag()),
                                (cx1.real() * cx2.imag()) + (cx1.imag() * cx2.real()) );
}


void cxnumratio(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout) {
  double cdenominator = (cx2.real() * cx2.real()) + (cx2.imag() * cx2.imag());
  double c_real = ( (cx1.real() * cx2.real()) + (cx1.imag() * cx2.imag()) ) / cdenominator;
  double c_imag = ( (cx1.imag() * cx2.real()) - (cx1.real() * cx2.imag()) ) / cdenominator;
  cxout = std::complex<double>( c_real, c_imag );
}
  

void cxdiffoversum(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout) {
  double real_aplusb = cx1.real() + cx2.real();
  double img_aplusb = cx1.imag() + cx2.imag();
  double cdenominator = (real_aplusb*real_aplusb) + (img_aplusb*img_aplusb);
  double c_real = ((cx1.real()*cx1.real())+(cx1.imag()*cx1.imag()) - (cx2.real()*cx2.real())-(cx2.imag()*cx2.imag())) / cdenominator;
  double c_imag = ((real_aplusb*(cx1.imag()-cx2.imag())) - ((cx1.real()-cx2.real())*img_aplusb)) / cdenominator;
  cxout = std::complex<double>( c_real, c_imag );
}


void cxoneplusaoveronepab(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout) {
  double fac1 = 1.0 + (cx1.real() * cx2.real()) - (cx1.imag() * cx2.imag());
  double fac2 = (cx1.real() * cx2.imag()) + (cx1.imag() * cx2.real());
  double onepareal = 1.0 + cx1.real();
  double cdenominator=(fac1 * fac1) + (fac2 * fac2);
  double c_real = ((onepareal * fac1) + (cx1.imag() * fac2)) / cdenominator;
  double c_imag = ((cx1.imag() * fac1) - (onepareal * fac2)) / cdenominator;
  cxout = std::complex<double>( c_real, c_imag );
}


void cxaplusboveronepab(const std::complex<double> & cx1, const std::complex<double> & cx2, std::complex<double> & cxout) {
  double fac1 = 1.0e0 + (cx1.real() * cx2.real()) - (cx1.imag() * cx2.imag());
  double fac2 = (cx1.real() * cx2.imag()) + (cx1.imag() * cx2.real());
  double apbreal = cx1.real() + cx2.real();
  double apbimag = cx1.imag() + cx2.imag();
  double cdenominator = (fac1 * fac1) + (fac2 * fac2);
  double c_real = ((apbreal * fac1) + (apbimag * fac2)) / cdenominator;
  double c_imag = ((apbimag * fac1) - (apbreal * fac2)) / cdenominator;
  cxout = std::complex<double>( c_real, c_imag );
}



void print2Dmatrix(std::vector< std::vector<double> > & matrix) {
  
  int numRows = matrix.size();
  int numCols = 0;
  
  std::cout << "----------------------" << std::endl;
  std::cout << "Printing 2D matrix" << std::endl;
  std::cout << "----------------------" << std::endl;
  for (int i = 0 ; i < numRows ; i++) {
    numCols = matrix[i].size();
    for (int j = 0 ; j < numCols ; j++) {
      std::cout << matrix[i][j] << " ";
    }
    std::cout << std::endl;
  }
  std::cout << "----------------------" << std::endl;
  
}


void print2Dmatrixlong(std::vector< std::vector<long> > & matrix) {
  
  int numRows = matrix.size();
  int numCols = 0;
  
  std::cout << "----------------------" << std::endl;
  std::cout << "Printing 2D matrix" << std::endl;
  std::cout << "----------------------" << std::endl;
  for (int i = 0 ; i < numRows ; i++) {
    numCols = matrix[i].size();
    for (int j = 0 ; j < numCols ; j++) {
      std::cout << matrix[i][j] << " ";
    }
    std::cout << std::endl;
  }
  std::cout << "----------------------" << std::endl;
  
}



/* Revision Log
  $Log: xrtreftable_lib.cxx,v $
  Revision 1.33  2016/04/07 21:07:03  rshill
  Parameter dump to log changed to command line format.

  Revision 1.32  2016/02/19 01:22:30  klrutkow
  updated author

  Revision 1.31  2015/09/23 13:17:14  klrutkow
  added ape_util.h for writeParametersToLog function

  Revision 1.30  2015/09/16 21:05:45  klrutkow
  added string.h for strncasecmp in writeParametersToLog

  Revision 1.29  2015/09/15 17:18:07  klrutkow
  added writeParametersToLog

  Revision 1.28  2015/08/17 14:07:50  klrutkow
  fixed bug in resolve

  Revision 1.27  2015/08/14 22:03:19  klrutkow
  remove CALDBCONFIG check for ASTRO-H

  Revision 1.26  2015/08/14 20:33:19  klrutkow
  all edits in caldb resolve query: added more checks for datetime format; add check through caldbconfig to make sure astro-h is supported ; added logging to output users caldb location

  Revision 1.25  2015/08/13 03:17:37  klrutkow
  added resolve() function to query CALDB ; fixed bug in cxpower

  Revision 1.24  2015/06/30 00:41:06  klrutkow
  added longToString, for CALDB queries

  Revision 1.23  2015/01/24 19:00:53  klrutkow
  updated tool with new parameters, per issue 472

  Revision 1.22  2014/11/07 15:43:59  klrutkow
  implement heaapp for startup() and shutdown()

  Revision 1.21  2014/10/06 14:10:08  klrutkow
  energy param accepts filename or list of energies; only write substrate (thick) keywords to front extension, not all materials; write all materials to the mass absorption extension; fix the mass absorption calculation in calcsf1sf2materials()

  Revision 1.20  2014/10/03 16:23:21  klrutkow
  param input filenames now include extension

  Revision 1.19  2014/09/04 16:50:07  klrutkow
  updates for v1.01 delivery

  Revision 1.18  2014/08/11 20:46:27  klrutkow
  updated default parameters

  Revision 1.16  2014/03/07 14:31:15  klrutkow
  changed signature of cx__ functions to accept a complex number rather than two doubles

  Revision 1.15  2014/02/28 21:08:40  klrutkow
  updating many misc changes

  Revision 1.14  2014/02/25 18:16:10  klrutkow
  misc updates

  Revision 1.13  2014/02/20 20:51:06  klrutkow
  checking in recent updates for new B05 tag

  Revision 1.12  2014/02/13 22:08:28  peachey
  Correct a bug in which logfile string was freed prematurely.

  Revision 1.11  2014/02/07 21:38:31  klrutkow
  updated, added comments

  Revision 1.10  2014/02/06 15:27:18  klrutkow
  updated keywords, set precision for energy range keyword

  Revision 1.9  2014/02/05 18:44:16  klrutkow
  cleaned up, ready for build

  Revision 1.8  2014/02/04 13:44:22  klrutkow
  moved around transmission amplitudes, per updated trf

  Revision 1.7  2014/02/03 17:55:48  klrutkow
  added include cstring, for strlen

  Revision 1.6  2014/02/03 13:18:04  peachey
  Temporarily add internal implementation of startUp and shutDown functions
  pending complete integration of these into headas.

  Revision 1.5  2014/02/03 02:58:52  klrutkow
  replaced cxaminusboveronepab with cxaplusboveronepab

  Revision 1.4  2014/01/31 15:56:58  klrutkow
  cleaned up code

  Revision 1.3  2014/01/30 16:29:55  klrutkow
  updated tool, single layer seems to work now
 

*/


