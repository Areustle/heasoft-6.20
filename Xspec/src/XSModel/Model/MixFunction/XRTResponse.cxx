//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <CCfits/CCfits>
#include <algorithm>
#include <cmath>
#include <sstream>

// XRTResponse
#include <XRTResponse.h>


// Class XRTResponse::XRTResponseError 

XRTResponse::XRTResponseError::XRTResponseError (const string& msg)
  : YellowAlert("XRT Response Error: ")
{
  std::cerr << msg << std::endl;
}


// Class XRTResponse 
StringArray XRTResponse::s_colNames;
string XRTResponse::s_ENERG_LO = "ENERG_LO";
string XRTResponse::s_THETA = "THETA";
string XRTResponse::s_PHI = "PHI";
string XRTResponse::s_EFFAREA = "EFFAREA";

XRTResponse::XRTResponse (const string& fileName)
  : m_fileName(),
    m_energy(),
    m_theta(),
    m_phi(),
    m_effectiveArea(),
    m_inEng(.0),
    m_eOrder(),
    m_inTheta(.0),
    m_inPhi(.0),
    m_rspTheta(.0, 2),
    m_rspPhi(.0, 2),
    m_wghtTheta(.0, 2),
    m_wghtPhi(.0, 2)
{
  m_fileName = FunctionUtility::modelDataPath() + fileName;
  if (s_colNames.empty())
  {
      s_colNames.push_back(s_ENERG_LO);
      s_colNames.push_back(s_THETA);
      s_colNames.push_back(s_PHI);
      s_colNames.push_back(s_EFFAREA);
  }
  readEffectiveArea();
  order();
}


XRTResponse::~XRTResponse()
{
}


void XRTResponse::readEffectiveArea ()
{
  using namespace CCfits;
  try
  {
     std::auto_ptr<FITS> p(new FITS(m_fileName));
     ExtHDU& response = p->extension(string("RESPONSE"));
     response.readData(true, s_colNames);

     response.column(s_ENERG_LO).read(m_energy, 1); 
     response.column(s_THETA).read(m_theta, 1);
     response.column(s_PHI).read(m_phi, 1);
     response.column(s_EFFAREA).read(m_effectiveArea, 1);
  }
  catch (CCfits::FITS::CantOpen)
  {
     string msg = "Cannot open XRT response file: ";
     msg += m_fileName;
     throw XRTResponseError(msg);
  }
  catch (CCfits::FitsException&)
  {
     string msg = "Error attempting to read data from XRT response file: ";
     msg += m_fileName;
     throw XRTResponseError(msg);
  }
}

Real XRTResponse::ebinEffectiveArea (Real eMin, Real eMax, Real theta, Real phi)
{

  // Function to return the XRT effective area in the eMin to eMax
  // bin. Calls the XRT function calcEffectiveArea but integrates
  // over the messy region between 2 and 4 keV.
  m_inTheta = theta;
  m_inPhi = phi;
  Real step = eMax - eMin;
  Real effArea = .0;
  // If the energy is in the messy range and the size of the bin exceeds 
  // 10 eV then integrate
  if (eMin > 2.0 && eMax < 4.0 && step > .01)
  {
     while (step >= .01)
     {
        step /= 2.0;
     }
     m_inEng = eMin + step/2.0;
     size_t n=0;
     while (m_inEng < eMax)
     {
        effArea += calcEffectiveArea();
        m_inEng += step;
        ++n;
     }
     // No divide by zero danger, n must be at least 1 to reach here.
     effArea /= static_cast<Real>(n);
  }
  else
  {
     // otherwise just take the area at the middle of the bin
     m_inEng = .5*(eMin + eMax);
     effArea = calcEffectiveArea();     
  }
  return effArea;
}

Real XRTResponse::calcEffectiveArea ()
{
  int iTheta = static_cast<int>(m_inTheta);
  iTheta = std::min((int)m_theta.size()-1, iTheta);
  int iPhi = static_cast<int>(3.0 - fabs(fmod(m_inPhi + 360.,90.)/15.0 - 3.0));
  // iPhi above should only = 3 for case of inPhi = 45.+n*90 
  if (iPhi >= 3) iPhi = 2;
  int iw = std::max(0, static_cast<int>(m_inEng*100.0 + 0.001));
  iw = std::min((int)m_eOrder.size()-2, iw);
  int ie = m_eOrder[iw];
  if (ie < 0)
  {
     std::ostringstream msg;
     msg << "Out of bounds: ASCA model energy is below " << m_energy[0] 
         << std::endl;
     throw XRTResponseError(msg.str());
  }

  int sz = m_rspTheta.size();
  for (int j=0; j<sz; ++j)
  {
     for (int i=0; i<sz; ++i)
     {
        m_rspTheta[i] = getSmoothRsp(ie, iTheta+i, iPhi+j);
        m_wghtTheta[i] = getWeightTheta(iTheta+i);
     }
     m_rspPhi[j] = weightAverage(0);
     m_wghtPhi[j] = getWeightPhi(iPhi+j);
  } 
  return weightAverage(1);
}

void XRTResponse::order ()
{
  const size_t nOrder = 1200;
  m_eOrder.resize(nOrder);

  size_t j=0;
  for (size_t i=1; i<=nOrder; ++i)
  {
     Real en = i*.01 + .0001;
     j = std::max(0, (int)j-10);
     size_t sz = m_energy.size();
     while (j < sz && m_energy[j] < en)
     {
        ++j;
     }
     m_eOrder[i-1] = j-1;

  }
}

Real XRTResponse::getSmoothRsp (int ie, int iTheta, int iPhi)
{
  const Real effAreaCrt = 130.0;
  const int nm = 3;

  Real smoothRsp = .0;
  int nE = m_energy.size();
  int iOffset = nE*iTheta + m_theta.size()*nE*iPhi;
  if (m_effectiveArea[ie + iOffset] <= effAreaCrt && (m_inEng > 2.4 ||
                m_inEng < 2.1))
  {
     int ieMin = std::max(0, ie-nm);
     int ieMax = (ieMin == 0) ? 2*nm : std::min(nE-1, ie+nm);
     if (ieMax == nE-1)  ieMin = nE-1 - 2*nm;
     size_t sz = static_cast<size_t>(ieMax - ieMin + 1);
     RealArray xWork(.0, sz);
     RealArray yWork(.0, sz);
     for (int i=ieMin; i<=ieMax; ++i)
     {
        xWork[i-ieMin] = m_energy[i];
        yWork[i-ieMin] = m_effectiveArea[i + iOffset];
     }
     smoothRsp = smooth(xWork, yWork);
  }
  else
  {
     smoothRsp = effAreaInterp(ie, iTheta, iPhi);
  }
  return smoothRsp;
}

Real XRTResponse::smooth (const RealArray& X, const RealArray& Y)
{
  size_t sz = Y.size();
  Real sumy = Y.sum();
  Real sumx = X.sum();
  Real sumxy = (X*Y).sum();
  Real sumx2 = (X*X).sum();
  Real deta = sz*sumxy - sumx*sumy;
  Real detb = sumy*sumx2 - sumx*sumxy;
  Real det = sz*sumx2 - sumx*sumx;
  Real a = deta/det;
  Real b = detb/det;

  return a*m_inEng + b;
}

Real XRTResponse::effAreaInterp (int ie, int iTheta, int iPhi)
{
  Real effArea = .0;
  int iOffset = ie + m_energy.size()*iTheta + 
                m_theta.size()*m_energy.size()*iPhi;
  Real deltax = m_energy[ie+1] - m_energy[ie];
  if (deltax == .0)
  {
     string msg = "Divide by 0 attempt.  2 consecutive energy array values are equal.";
     throw XRTResponseError(msg);
  }
  Real deltay = m_effectiveArea[iOffset+1] - m_effectiveArea[iOffset];
  effArea = (m_inEng - m_energy[ie])*deltay/deltax + m_effectiveArea[iOffset];
  return effArea;
}

Real XRTResponse::getWeightTheta (int iTheta)
{
  Real weight = .0;
  if (m_theta[iTheta] == m_inTheta)
  {
     weight = 1.0e6;
  }
  else
  {
     weight = 1.0/fabs(m_theta[iTheta]-m_inTheta);
  }
  return weight;
}

Real XRTResponse::getWeightPhi (int iPhi)
{
  Real weight = .0;
  Real wph = 45.0 - fabs(fmod(m_inPhi+360., 90.) - 45.0);
  if (m_phi[iPhi] == wph)
  {
     weight = 1.0e6;
  }
  else
  {
     weight = 1.0/fabs(m_phi[iPhi]-wph);
  }
  return weight;
}

Real XRTResponse::weightAverage (bool isPhi)
{
  const RealArray& X = isPhi ? m_rspPhi : m_rspTheta;
  const RealArray& weight = isPhi ? m_wghtPhi : m_wghtTheta;

  return (X*weight).sum() / weight.sum();
}

// Additional Declarations
