//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <CCfits/FITSBase.h>

// ClusterRegion
#include "ClusterRegion.h"


// Class ClusterRegion::ClusterRegionError 

ClusterRegion::ClusterRegionError::ClusterRegionError (const string& msg)
  : YellowAlert("Cluster region error: ")
{
  std::cerr << msg << std::endl;
}


// Class ClusterRegion 
std::vector<std::string> ClusterRegion::s_instrumentNames;
const size_t ClusterRegion::s_NINST = 4;
IntegerArray ClusterRegion::s_instMapSizes;

ClusterRegion::ClusterRegion (const string& fileName, int regionNumber, size_t spectrumNumber)
  : WmapRegion(),
    m_optic(std::pair<Real,Real>(.0,.0)),
    m_instrument(0),
    m_center(std::pair<Real,Real>(.0,.0)),
    m_nPixInRegion(0),
    m_regionNumber(regionNumber),
    m_spectrumNumber(spectrumNumber),
    m_instMapFractions()
{
  if (s_instrumentNames.empty())
  {
     s_instrumentNames.push_back("SIS0");
     s_instrumentNames.push_back("SIS1");
     s_instrumentNames.push_back("GIS2");
     s_instrumentNames.push_back("GIS3");
  }
  if (s_instMapSizes.empty())
  {
     s_instMapSizes.push_back(40);
     s_instMapSizes.push_back(40);
     s_instMapSizes.push_back(64);
     s_instMapSizes.push_back(64);
  }
  if (s_instrumentNames.size() != s_NINST || s_instMapSizes.size() != s_NINST)
  {
     throw RedAlert("ClusterRegion: Intstrument names/sizes array size mismatch");
  }
  int nullVal = -1;
  read(fileName, &nullVal);
  calcInstMapFractions();
}


ClusterRegion::~ClusterRegion()
{
}


void ClusterRegion::readSpecificKeys ()
{
  const CCfits::PHDU& hdu = file()->pHDU();
  string errorKey("INSTRUME");
  try
  {
     string instrument;
     hdu.keyWord("INSTRUME").value(instrument);
     convertNameToInt(instrument);
     errorKey = "OPTIC1";
     float tmpFloat = .0;
     hdu.keyWord(errorKey).value(tmpFloat);
     m_optic.first = static_cast<Real>(tmpFloat);
     m_optic.first /= 4.0;
     errorKey = "OPTIC2";
     hdu.keyWord(errorKey).value(tmpFloat);
     m_optic.second = static_cast<Real>(tmpFloat);
     m_optic.second /= 4.0;
  }
  catch (CCfits::FitsException&)
  {
     string msg = "Error reading ";
     msg += errorKey;
     msg += " keyword(s) in ";
     msg += hdu.parent()->name();
     throw ClusterRegionError(msg);
  }
}

void ClusterRegion::convertNameToInt (const string& instrument)
{
  int i = 0;
  int sz = s_instrumentNames.size();  
  while (i < sz)
  {
     if (instrument == s_instrumentNames[i])  break;
     ++i;   
  }
  if (i < sz)
  {
     m_instrument = i;
  }
  else
  {
     string msg = "Unrecognized instrument name in file ";
     msg += file()->pHDU().parent()->name();
     throw ClusterRegionError(msg);
  }
}

void ClusterRegion::calcInstMapFractions ()
{
  int nInst = s_instMapSizes[m_instrument];
  int nxpix = nx();
  int nypix = ny();
  Real xmap = lowerLeft().first;
  Real ymap = lowerLeft().second;
  const std::valarray<bool>& isWmapOn = isPixOn();
  if (version() >= 2.0)
  {
     xmap += (wmrbn()-1.0)/2.0;
     xmap /= wmrbn();
     ymap += (wmrbn()-1.0)/2.0;
     ymap /= wmrbn();
  }
  m_instMapFractions.resize(nInst*nInst, .0);
  for (int iy=0; iy<nInst; ++iy)
  {
     int iyOffset = iy*nInst;
     for (int ix=0; ix<nInst; ++ix)
     {
        int iFrac = iyOffset + ix;
        // ixpos and iypos below are 1-based as in the original
        // fortran code.  This results in m_center also being 
        // 1-based.
        for (int jy=0; jy<4; ++jy)
        {
           // truncation of Real values is intentional
           int iypos = static_cast<int>(jy+1 + iy*4 - ymap + 1);
           if (iypos >= 1 && iypos <= nypix)
           {
              for (int jx=0; jx<4; ++jx)
              {
                 int ixpos = static_cast<int>(jx+1 + ix*4 - xmap + 1);
                 if (ixpos >= 1 && ixpos <= nxpix)
                 {
                    if (isWmapOn[nxpix*(iypos-1)+ixpos-1])
                    {
                       m_instMapFractions[iFrac] += 1./16.;
                       m_center.first += ixpos + xmap - 1.;
                       m_center.second += iypos + ymap - 1.;
                       ++m_nPixInRegion;
                    }
                 }
              }
           }
        }
     }
  }
  if (m_nPixInRegion)
  {
     m_center.first /= m_nPixInRegion;
     m_center.second /= m_nPixInRegion;
  }
}

// Additional Declarations
