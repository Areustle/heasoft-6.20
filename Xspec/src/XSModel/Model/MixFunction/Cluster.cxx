//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSsymbol.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/GlobalContainer/DataSetTypes.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSstreams.h>
#include <cmath>
#include <map>
#include <set>
#include <sstream>

// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// ClusterRegion
#include "ClusterRegion.h"
// PsfImage
#include <PsfImage.h>
// XRTResponse
#include <XRTResponse.h>
// Cluster
#include <Cluster.h>

using namespace XSContainer;
extern "C" double Get_gis_eff(double lo, double hi, double be, int correct);


// Class Cluster::ClusterError 

Cluster::ClusterError::ClusterError (const string& msg)
  : YellowAlert("Cluster Model Error: ")
{
  std::cerr << msg << std::endl;
}


// Class Cluster 
Real Cluster::s_PI = 4.*atan(1.);
const string Cluster::s_psfFileName = string("allpsf.nosm.fits");
const string Cluster::s_xrtFileName = string("xrt_ea.fits");

Cluster::Cluster (const string& name)
  : MixUtility(name),
      m_alpha(.0),
      m_beta(.0),
      m_core(.0),
      m_sbModelType(0),
      m_factArray(),
      m_parametersAreFrozen(true),
      m_regions(),
      m_instInUse(),
      m_psfEnergy(),
      m_modelName(),
      m_engIndices(),
      m_psfImage(0),
      m_weight(),
      m_xrtResponse(0),
      m_dataChanged(false),
      m_allEnergies(),
      m_centers(),
      m_psfArray()
{
  const size_t nPSFE = 10;
  m_psfEnergy.resize(nPSFE);
  for (size_t i=0; i<nPSFE; ++i)
  {
     m_psfEnergy[i] = i + 1.5;
  }
}


Cluster::~Cluster()
{
   clearRegions();
   delete m_psfImage;
   delete m_xrtResponse;
}


void Cluster::initialize (const std::vector<Real>& params, const IntegerArray& spectrumNums, const std::string& modelName)
{
  //This function does a complete initialize.  This is what should
  // be called anytime datasets->Notify() is sent, or whenever
  // the model containing this component is first constructed.
  clearRegions();
  setSpecNums(spectrumNums);
  verifyData();
  // m_psfImage and m_xrtResponse are only created once, and
  // will last the duration of Cluster object.
  if (!m_psfImage)
  {
     m_psfImage = new PsfImage(s_psfFileName);
  }
  if (!m_xrtResponse)
  {
     m_xrtResponse = new XRTResponse(s_xrtFileName);
  }
  m_modelName = modelName;
  const size_t nReg = datasets->numberOfGroups();
  const size_t nInst = ClusterRegion::NINST();
  m_regions.resize(nInst);
  m_instInUse.resize(nInst,false);
  
  for (size_t i=0; i<nInst; ++i)
  {
     m_regions[i].resize(nReg, static_cast<ClusterRegion*>(0));
  }
  try
  {

     // Once we have instrument information (through the ClusterRegion object),
     //  do 2 more data checks in addition to what was already performed in verifyData.
     //  1) Multiple spectra cannot belong to the same instrument AND region.
     //  2) All spectra for an instrument must share the same response (this 
     //     is relevant for the assumption in calcGISEfficiency).
     const Model* anyModFromGroups = models->modelSet().find(m_modelName)->second;
     if (!anyModFromGroups)
        throw RedAlert("Unable to locate model in Cluster::initialize.");
     const size_t sourceNum = anyModFromGroups->sourceNumber();
     std::map<size_t,string> respForInst;
     for (size_t i=0; i<specNums().size(); ++i)
     {
        const SpectralData* spec = datasets->lookup(specNums()[i]);
        const size_t dG = spec->parent()->dataGroup();
        const string fileName = spec->parent()->getFullPathName();
        int iReg=0;
        // Assume keyword existance has already been checked in verifyData().
	if (spec->inxflt("region") ) {
	  iReg = static_cast<int>(spec->xflt("region"));
	} else {
	  iReg = static_cast<int>(spec->xflt(1));
	}
        std::auto_ptr<ClusterRegion> tmp(new ClusterRegion(fileName,iReg,specNums()[i]));
        const size_t iInst = tmp->instrument();
        if (m_regions[iInst][dG-1])
        {
           string msg("2 spectra cannot be assigned to the same instrument and region.");
           throw ClusterError(msg);
        }
        
        const Response* resp = spec->detector(sourceNum-1);
        if (!resp)
           throw RedAlert("Unable to locate response in Cluster::initialize."); 
        const RealResponse* rresp = resp->toRealResponse();
        if (!rresp)
        {
           std::ostringstream oss;
           oss <<"ascac mix model requires a response file for spectrum "<<specNums()[i];
           throw ClusterError(oss.str());
        }  
        if (respForInst.find(iInst) == respForInst.end())
           respForInst[iInst] = rresp->rmfName();
        if (rresp->rmfName() != respForInst[iInst])
        {
           string msg("To use ascac model, all spectra for an instrument must share same response.");
           throw ClusterError(msg);
        }
        m_regions[iInst][dG-1] = tmp.release();
        m_instInUse[iInst] = true;
     }     
     
     calcCentersOfEmission();
     calcEngIndices();
     m_dataChanged = true;
     initializeForFit(params, true);
  }
  catch (YellowAlert&)
  {
     clearRegions();
     m_psfArray.clear();
     throw;
  }
}

void Cluster::perform (const EnergyPointer& energy, const std::vector<Real>& params, GroupFluxContainer& flux, GroupFluxContainer& fluxError)
{
  if (!m_parametersAreFrozen)
  {
     clusterFact();
  }
  bool isError(!fluxError.empty());
  doPerform(flux, false);
  if (isError)
  {
     doPerform(fluxError, true);
  }
}

void Cluster::verifyData ()
{
  if (!specNums().size())
  {
     string msg("No data assigned to ascac model, mixing will not be performed.");
     throw ClusterError(msg);
  }

  // Check that these conditions are met: 
  // 1. All spectra in data group are in the same region, REGARDLESS
  //     OF INSTRUMENT.
  // 2. Different data groups are assigned to each region.
  std::map<size_t,int> groupToRegion;
  for (size_t i=0; i<specNums().size(); ++i)
  {
     const SpectralData* spec = datasets->lookup(specNums()[i]);
     const size_t dG = spec->parent()->dataGroup();
     
     int iRegion=0;
     if ( spec->inxflt("region") ) {
       iRegion = static_cast<int>(spec->xflt("region"));
     } else if (spec->inxflt(1) ) {
       iRegion = static_cast<int>(spec->xflt(1));
     } else {
       string diag = "  Cannot find keyword for region number in " 
	 + spec->parent()->dataName();
       throw IncompatibleData(diag);
     }

     // This checks rule 1.
     if (groupToRegion.find(dG) == groupToRegion.end())
     {
        groupToRegion[dG] = iRegion;
     }
     else if (groupToRegion[dG] != iRegion)
     {
        string msg("To use ASCA mix model, all spectra in data group must belong");
        msg += "\nto same region.  Check spectra XFLT#### key values.";
        throw ClusterError(msg);
     }
  }
  // For rule 2, must be no duplication among iRegion values:
  std::set<int> regionValues;
  std::map<size_t,int>::const_iterator itGroupToRegion = groupToRegion.begin();
  while (itGroupToRegion != groupToRegion.end())
  {
     regionValues.insert(itGroupToRegion->second);
     ++itGroupToRegion;
  }
  if (regionValues.size() != groupToRegion.size())
  {
     string msg("To use ASCA mix model, spectra in different data groups must");
     msg += "\n be assigned to different regions.  Check spectra XFLT### key values.\n";
     throw ClusterError(msg);
  }
  
}

void Cluster::initializeForFit (const std::vector<Real>& params, bool paramsAreFixed)
{
  // This is the initialization that should be performed in response
  // to a fit command, or a fit update call.  It is a subset of the  
  // general initialize function.
  bool paramsChanged = changedParameters(params);
  m_parametersAreFrozen = paramsAreFixed;

  // If we got here immediately after first getting here from the 
  // initialize function (such as from the datasets->models->fit 
  // notify update chain), no problem. In that case, nothing below 
  // will be done. 
  if (paramsChanged || m_dataChanged)
  {
     m_alpha = params[0];
     m_beta = params[1];
     m_core = params[2];
     m_sbModelType = static_cast<int>(params[3]);
     clusterFact();
     m_dataChanged = false;
  }
}

void Cluster::clearRegions ()
{
  size_t nInst = m_regions.size();
  for (size_t i=0; i<nInst; ++i)
  {
     std::vector<ClusterRegion*>& regions_i = m_regions[i];
     size_t nReg = regions_i.size();
     for (size_t j=0; j<nReg; ++j)
     {
        delete regions_i[j];
     }
  }
  m_regions.clear();  
  m_instInUse.clear(); 
}

void Cluster::calcCentersOfEmission ()
{
  m_centers.resize(ClusterRegion::NINST());
  for (size_t i=0; i<ClusterRegion::NINST(); ++i)
  {
     std::pair<Real,Real>& center = m_centers[i];
     center.first = center.second = .0;
     const std::vector<ClusterRegion*>& instRegions = m_regions[i];
     size_t nReg = instRegions.size();
     Real xpos=.0, ypos=.0;
     int nTotalPix=0;
     if (m_instInUse[i])
     {
        for (size_t j=0; j<nReg; ++j)
        {
           if (instRegions[j])
           {
              int nPix = instRegions[j]->nPixInRegion();
              xpos += instRegions[j]->center().first*nPix;
              ypos += instRegions[j]->center().second*nPix;
              nTotalPix += nPix;
           }
        }
        if (nTotalPix)
        {
           center.first = xpos/(4.0*nTotalPix);
           center.second = ypos/(4.0*nTotalPix);
        }        
     }
  }
  if (tpout.maxChatter() >= 25)
  {
     const std::vector<string>& instNames = ClusterRegion::instrumentNames();
     size_t sz = instNames.size();
     tcout << "\nCluster centers:" << std::endl;
     for (size_t i=0; i<sz; ++i)
     {
        tcout << "  " << instNames[i] << ":  " << m_centers[i].first 
               << " , " << m_centers[i].second <<std::endl; 
     }
  }
}

void Cluster::clusterFact ()
{
  size_t nInst = ClusterRegion::NINST();
  size_t nReg = datasets->numberOfGroups();
  const size_t nPSFE = m_psfEnergy.size();
  const size_t nDIM = PsfImage::NDIM();
  const Real threshold = 1./32.;
  int nTotE = m_engIndices[nInst-1][nReg];
  m_factArray.resize(nReg*nTotE, .0);
  m_weight.resize(nPSFE*nReg, .0);
  m_psfArray.resize(nPSFE);
  for (size_t i=0; i<nPSFE; ++i)
  {
     m_psfArray[i].resize(nDIM*nDIM);
  }

  for (size_t inst=0; inst<nInst; ++inst)
  {
     if (m_instInUse[inst])
     {
        std::pair<Real,Real> optic = getOptic(inst);
        const std::pair<Real,Real>& centers_i = m_centers[inst];
        int nMap = ClusterRegion::instMapSizes()[inst];
        const int icen = (nMap + 1)/2;
        // Note center points are 1-based.
        // Loop over source map pixels.
        for (int iy=1; iy<=nMap; ++iy)
        {
           Real y = iy - centers_i.second;
           Real yoff = iy - optic.second;
           int iyOffset = (iy-1)*nMap;
           int jyLow=0, jyHigh=0;
           if (iy <= icen)
           {
              jyLow = 1;
              jyHigh = iy + (icen-1);
           }
           else
           {
              jyLow = iy - (icen-1);
              jyHigh = nMap;
           }
           for (int ix=1; ix<=nMap; ++ix)
           {
              Real x = ix - centers_i.first;
              Real xoff = ix - optic.first;
              int jxLow=0, jxHigh=0;
              if (ix <= icen)
              {
                 jxLow = 1;
                 jxHigh = ix + (icen-1);
              }
              else
              {
                 jxLow = ix - (icen-1);
                 jxHigh = nMap;
              }
              if (isPixInRegions(inst, ix, iy))
              {
                 Real theta=.0, phi=.0;
                 calcCoordinates(xoff, yoff, theta, phi);
                 if (theta <= 17.)
                 {
                    Real sbint = integrateSurfaceBrightness(x, y);
                    for (size_t iPSFE=0; iPSFE<nPSFE; ++iPSFE)
                    {
                      m_psfImage->calcPsf(m_psfEnergy[iPSFE], theta,
                            phi, m_psfArray[iPSFE]);
                    }
                    m_weight = 0.;

                    // Loop over target map pixels.
                    // ix,iy,jx,jy, and therefore xpsf,ypsf are 1-based.
                    for (int jy=jyLow; jy<=jyHigh; ++jy)
                    {
                       int ypsf = icen - iy + jy;
                       int jyOffset = (jy-1)*nMap;
                       for (int jx=jxLow; jx<=jxHigh; ++jx)
                       {
                          int xpsf = icen - ix + jx;
                          int psfElem = (xpsf-1) + (ypsf-1)*nDIM;
                          // Loop over the target regions.
                          for (size_t jreg=0; jreg<nReg; ++jreg)
                          {
                             const ClusterRegion* cr = m_regions[inst][jreg];
                             if (cr)
                             {
                                int jregOffset = jreg*nPSFE;
                                const RealArray& regFrac = cr->instMapFractions();
                                Real regFrac_jxjy = regFrac[jx-1 + jyOffset];
                                if (regFrac_jxjy >= threshold)
                                {

                                   for (size_t iPSFE=0; iPSFE<nPSFE; ++iPSFE)
                                   {
                                      m_weight[iPSFE+jregOffset] += 
                                        m_psfArray[iPSFE][psfElem]*regFrac_jxjy;
                                   }
                                }
                             }

                          } // end target regions loop
                       } // end target map x pixels loop
                    } // end target map y pixels loop
                    // Sum the contribution from this source pixel into 
                    // the appropriate element of the fact array. Interpolate
                    // on the weight array to get the PSF contribution at each
                    // energy. Multiply by the XRT effective area for this 
                    // source pixel.
                    const std::vector<int>& engIndices_i = m_engIndices[inst];
                    int nPsfe = m_psfEnergy.size();
                    for (size_t ireg=0; ireg<nReg; ++ireg)
                    {
                       const ClusterRegion* cr = m_regions[inst][ireg];
                       if (cr)
                       {
                          const RealArray& regFrac = cr->instMapFractions();
                          if (regFrac[ix-1+iyOffset] >= threshold)
                          {
                             int ilowE = engIndices_i[ireg];
                             int ihighE = engIndices_i[ireg+1];
                             int nE = ihighE - ilowE;
                             const RealArray& engs = m_allEnergies.find(ireg+1)->
                                        second->find(cr->spectrumNumber())->second;
                             int startFrom = 0;
                             for (int ie=1; ie<nE; ++ie)
                             {
                                Real centerE = (engs[ie-1] + engs[ie])/2.0;
                                Real frac0 = .0;
                                int iPsfe = findPsfEngBracket(centerE, startFrom);
                                if (iPsfe < 0)
                                {
                                   startFrom = 0;
                                   iPsfe = 0;
                                   frac0 = 1.0;
                                }
                                else if (iPsfe == nPsfe-1)
                                {
                                   startFrom = iPsfe;
                                   frac0 = 0.0;
                                   // decrement is necessary since we'll
                                   // be taking iPsfe+1 below.
                                   --iPsfe;
                                }
                                else
                                {
                                   startFrom = iPsfe;
                                   Real deltaE = m_psfEnergy[iPsfe+1] - 
                                                m_psfEnergy[iPsfe];
                                   frac0 = (m_psfEnergy[iPsfe+1] - centerE)/deltaE;
                                } 
                                Real frac1 = 1.0 - frac0;
                                Real effec = sbint*regFrac[ix-1 + iyOffset]*
                                   m_xrtResponse->ebinEffectiveArea(engs[ie-1],
                                   engs[ie], theta,.0);
                                for (size_t jreg=0; jreg<nReg; ++jreg)
                                {
                                   m_factArray[ilowE+ie-1 + jreg*nTotE] +=
                                        effec*(frac0*m_weight[iPsfe+nPsfe*jreg] +
                                        frac1*m_weight[iPsfe+1+nPsfe*jreg]);
                                }
                             } // end energy loop
                          } // end if regFrac greater than threshold
                       }  // end if cluster region exists
                    } // end ireg loop
                 } // end if theta <= 17.
              } // end if pixel in regions
           } // end source map x pixels loop
        } // end source map y pixels loop
        if (inst >= 2)
        {
           calcGISEfficiency(inst);
        }
     } // end if instrument in use
  } // end instrument loop
  m_psfArray.clear();
}

void Cluster::calcEngIndices ()
{
  size_t nReg = datasets->numberOfGroups();
  size_t nInst = ClusterRegion::NINST();
  m_allEnergies = models->gatherGroupEnergy(m_modelName);

  m_engIndices.resize(nInst);
  for (size_t i=0; i<nInst; ++i)
  {
     m_engIndices[i].resize(nReg+1, 0);
  }

  // First pass:  simply store the size of the energy arrays
  // found for each region for each instrument.

  // EnergyPointer is a map of a map of model energy arrays.
  // The outer map contains datagroups (regions), the inner
  // contains spectra (instruments).  
  for (size_t i=0; i<nInst; ++i)
  {
     if (m_instInUse[i])
     {
        for (size_t j=0; j<nReg; ++j)
        {
           const ClusterRegion* cr = m_regions[i][j];
           if (cr)
           {
              size_t dG = j+1;
              // If cr exists, we know there must be an energy
              // map for this data group number.
              const ArrayContainer& dgEnergies = *(m_allEnergies.find(dG)->second);
              size_t specNum = cr->spectrumNumber();
              size_t nE = dgEnergies.find(specNum)->second.size();
              m_engIndices[i][j] = static_cast<int>(nE);
           }
        }
     }
  }

  // Second pass:  now replace the energy array sizes with their
  // cumulative offset from the beginning of inst 0 dg 1.
  // Note that inner loop now goes over all nReg+1 elements rather
  // than just nReg as in first pass.
  int cumulative = 0;
  for (size_t i=0; i< nInst; ++i)
  {
     IntegerArray& engIndices_i = m_engIndices[i];
     for (size_t j=0; j<=nReg; ++j)
     {
        int currEnergySize = engIndices_i[j];
        engIndices_i[j] = cumulative;
        cumulative += currEnergySize;
     }
  }
}

std::pair<Real,Real> Cluster::getOptic (int instrument) const
{
  // This function assumes that all regions of a particular
  // instrument have the same optic axis values.  Therefore,
  // it looks for only the first region it can find and returns
  // that value.
  size_t nReg = m_regions[instrument].size();
  std::pair<Real,Real> optic(.0, .0);
  for (size_t i=0; i<nReg; ++i)
  {
     if (m_regions[instrument][i])
     {
        optic = m_regions[instrument][i]->optic();
        break;
     }
  }
  return optic;
}

bool Cluster::isPixInRegions (int instrument, int ix, int iy) const
{
  bool isInReg = false;
  int nMapSize = ClusterRegion::instMapSizes()[instrument];
  Real small = 1./32.;
  const std::vector<ClusterRegion*>& regions_i = m_regions[instrument];
  // Note: this function assumes ix, iy are 1-BASED.
  for (size_t j=0; j<regions_i.size(); ++j)
  {
     const ClusterRegion* cr = regions_i[j];
     if (cr)
     {
        if (cr->instMapFractions()[(ix-1) + nMapSize*(iy-1)] > small)
        {
           isInReg = true;
           break;
        }
     }
  }
  return isInReg;
}

void Cluster::calcCoordinates (const Real xoff, const Real yoff, Real& theta, Real& phi)
{
  theta = sqrt(xoff*xoff + yoff*yoff);
  if (fabs(xoff) < SMALL)
  {
     phi = 90.;
     if (yoff < .0)
     {
        phi = 270.;
     }
  }
  else
  {
     phi = (180./s_PI)*atan(fabs(yoff)/fabs(xoff));
     if (xoff < .0)
     {
        if (yoff > .0)
        {
           phi += 90.;
        }
        else
        {
           phi += 180.;
        }
     }
     else if (yoff < .0)
     {
        phi += 270.;
     }
  }
}

Real Cluster::integrateSurfaceBrightness (const Real x, const Real y) const
{
  Real sbint = .0;
  Real powVal = -3.*m_beta + 0.5;
  if (m_sbModelType == 0)
  {
     for (int jx=1; jx<=10; ++jx)
     {
        Real xDist2 = x - ((Real)jx-5.5)*.1;
        xDist2 *= xDist2;
        for (int jy=1; jy<=10; ++jy)
        {
           Real dist2 = xDist2 + (y - ((Real)jy-5.5)*.1)*(y - ((Real)jy-5.5)*.1);
           sbint += .01*pow((1. + dist2/(m_core*m_core)),powVal);
        }
     }
  }
  else if (m_sbModelType == 1)
  {
     for (int jx=1; jx<=10; ++jx)
     {
        Real xDist2 = x - ((Real)jx-5.5)*.1;
        xDist2 *= xDist2;
        for (int jy=1; jy<=10; ++jy)
        {
           Real dist = sqrt(xDist2 + (y - ((Real)jy-5.5)*.1)*(y - ((Real)jy-5.5)*.1))
                        / m_core;
           sbint += .01*(pow(dist,-m_alpha)*exp(-dist) + pow(dist,-m_beta)*
                                (1.-exp(-dist)));
        }
     }
  }
  return sbint;
}

int Cluster::findPsfEngBracket (Real energy, int startFrom) const
{
  // Given an input energy, this function should find the energies 
  // that bracket it in the m_psfEnergy array.  It returns the index
  // of the lower energy of the bracket.  If energy is less than
  // all energies in the psfArray, it returns -1 (unless startFrom
  // parameter is used). If energy is greater than all psfArray 
  // energies, it returns nPsfe-1.

  // If startFrom parameter is used, only begin the search from 
  // index = startFrom.
  int iLowPsfe = (startFrom > 0) ? startFrom-1 : -1;
  int nPsfe = static_cast<int>(m_psfEnergy.size());
  if (energy >= m_psfEnergy[iLowPsfe + 1])
  {
     ++iLowPsfe;
     // iLowPsfe should now = 0, or startFrom, and 
     // energy is >= m_psfEnergy[iLowPsfe].
     while (iLowPsfe < (nPsfe-1) && energy >= m_psfEnergy[iLowPsfe+1])
     {
        ++iLowPsfe;
     }
     // Either m_psfEnergy[iLowPsfe] <= energy < m_psfEnergy[iLowPsfe+1]
     // or m_psfEnergy[nPsfe-1] <= energy.
  }
  return iLowPsfe;
}

void Cluster::doPerform (GroupFluxContainer& fluxes, bool isError)
{
   const size_t nInst = m_regions.size();
   const size_t nReg = m_regions[0].size();
   int nTotE = m_engIndices[nInst-1][nReg];
   // GroupFluxContainer is a map of maps.  The outer map is
   // a collection of regions (data groups), the inner map is
   // a collection of instruments (spectra) for the region.
   GroupFluxContainer tmpFlux(fluxes);
   GroupFluxContainer::iterator t(tmpFlux.begin());
   GroupFluxContainer::iterator tEnd(tmpFlux.end());
   // recall ArrayContainer == std::map<size_t,std::valarray<Real> > so that
   // s->second has a vectorized assignment operator that takes a scalar. 
   while ( t != tEnd )
   {
         ArrayContainer::iterator s (t->second.begin());
         ArrayContainer::iterator sEnd (t->second.end());
         for ( ; s != sEnd; ++s ) s->second = 0;
         ++t;         
   }

   for (size_t inst=0; inst<nInst; ++inst)
   {
      if (m_instInUse[inst])
      {
         // One final safety check.  Every region with a non-null
         //  CluterRegion* must have corresponding spectrum passed in
         //  the GroupFluxContainer.
         for (size_t jreg=0; jreg<nReg; ++jreg)
         {
            const ClusterRegion* cr = m_regions[inst][jreg];
            if (cr)
            {
               int specNum = cr->spectrumNumber();
               if (fluxes.find(jreg+1) == fluxes.end())
               {
                  throw RedAlert("Regions mismatch in Cluster::doPerform()");
               }
               if (fluxes[jreg+1].find(specNum) == fluxes[jreg+1].end())
               {
                  throw RedAlert("Spectrum mismatch in Cluster::doPerform()");
               }
            }
         }
         
	 for (size_t jreg=0; jreg<nReg; ++jreg)
         {
            const ClusterRegion* tcr = m_regions[inst][jreg];
            if (tcr)
            {
               int tSpecNum = tcr->spectrumNumber();
               RealArray& targetFlux = tmpFlux[jreg+1][tSpecNum];
               int jOffset = jreg*nTotE;
               for (size_t ireg=0; ireg<nReg; ++ireg)
               {
                  const ClusterRegion* scr = m_regions[inst][ireg];
                  if (scr)
                  {
                     int sSpecNum = scr->spectrumNumber();
                     RealArray& sourceFlux = fluxes[ireg+1][sSpecNum];
                     int nE = sourceFlux.size();
                     int iOffset = m_engIndices[inst][ireg];
                     for (int ie=0; ie<nE; ++ie)
                     {
                        Real factor = m_factArray[ie + iOffset + jOffset];
                        if (isError)
                        {
                           factor *= factor;
                        }
                        targetFlux[ie] += sourceFlux[ie]*factor;
                     }
                  }
               } // end ireg loop
            }
         } // end jreg loop
      } // end if instInUse
   } // end inst loop
   fluxes = tmpFlux;
}

void Cluster::calcGISEfficiency (int inst)
{

   // ASSUMES all spectra for inst have exactly the same energy bins.
   // When instrument is the GIS then calculate the efficiency and multiply
   //  it into the fact array.
   
   const size_t nInst = m_regions.size();
   const size_t nReg = m_regions[inst].size();
   const ClusterRegion* cr=0;
   // Just find the first region/dg in use.
   size_t dg=0;
   for (size_t ireg=0; ireg<nReg && !cr; ++ireg)
   {
      cr = m_regions[inst][ireg];
      dg = ireg+1;
   }
   if (cr)
   {
      size_t specNum = cr->spectrumNumber();
      const RealArray& energy = m_allEnergies.find(dg)->second->
                                find(specNum)->second;
      int sz = energy.size();
      const std::vector<int>& engIndices_i = m_engIndices[inst];
      int nTotE = m_engIndices[nInst-1][nReg];
      for (int ie=1; ie<sz; ++ie)
      {
         Real effect = Get_gis_eff(energy[ie-1], energy[ie], 10.7, 0);
         // Now apply only to cases where regions exist for both ireg and jreg.
         for (size_t ireg=0; ireg<nReg; ++ireg)
         {
            if (m_regions[inst][ireg])
            {
               int iOffset = engIndices_i[ireg] + ie-1;
               for (size_t jreg=0; jreg<nReg; ++jreg)
               {
                  if (m_regions[inst][jreg])
                     m_factArray[iOffset + nTotE*jreg] *= effect;
               }
            }
         }
      }

   }
}

bool Cluster::changedParameters (const std::vector<Real>& params) const
{
  return (std::fabs(m_alpha-params[0]) > 1.0e-6 ||
          std::fabs(m_beta-params[1]) > 1.0e-6  ||
          std::fabs(m_core-params[2]) > 1.0e-6  ||
          m_sbModelType != static_cast<int>(params[3]));
}


// Additional Declarations
