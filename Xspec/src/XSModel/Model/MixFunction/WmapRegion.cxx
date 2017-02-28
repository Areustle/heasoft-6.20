//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <CCfits/FITSBase.h>
#include <sstream>

// WmapRegion
#include "WmapRegion.h"


// Class WmapRegion::WmapRegionError 

WmapRegion::WmapRegionError::WmapRegionError (const string& msg)
  : YellowAlert("Wmap region error: ")
{
  std::cerr << msg << std::endl;
}


// Class WmapRegion 

WmapRegion::WmapRegion()
  : m_lowerLeft(std::pair<Real,Real>(.0,.0)),
    m_upperRight(std::pair<Real,Real>(.0,.0)),
    m_nx(0),
    m_ny(0),
    m_wmap(),
    m_wmrbn(1),
    m_file(0),
    m_refBinKeys(),
    m_wmrbnKey(""),
    m_version(.0),
    m_wmapPos(),
    m_isPixOn()
{
}


void WmapRegion::readCommonKeys ()
{
  CCfits::PHDU& hdu = m_file->pHDU();
  const CCfits::PHDU& chdu = const_cast<const CCfits::PHDU&>(hdu);
  const string fileName = chdu.parent()->name();
  if (hdu.axes() == 2)
  {
     m_nx = static_cast<int>(hdu.axis(0));
     m_ny = static_cast<int>(hdu.axis(1));
  }
  else
  {
     string msg("Primary HDU does not have 2 axes in file: ");
     msg += fileName;
     throw WmapRegionError(msg);        
  }
  hdu.readAllKeys();

  string errorKey("HDUVERS");
  try
  {
     m_version = getVersion();
     if (m_version < 0)
     {
        string msg("Error reading ");
        msg += errorKey + string(" keyword in file: ");
        msg += fileName;
        throw WmapRegionError(msg);
     }
     else if (m_version < 2)
     {
        m_wmrbnKey = "WMREBIN";
        m_refBinKeys.first = "CRVAL1";
        m_refBinKeys.second = "CRVAL2";
     }
     else
     {
        m_wmrbnKey = "CDELT1P";
        m_refBinKeys.first = "CRVAL1P";
        m_refBinKeys.second = "CRVAL2P";
     }
     errorKey = m_wmrbnKey;
     getWmrbn();
     errorKey = m_refBinKeys.first + ", " + m_refBinKeys.second;
     getCorners();
  }
  catch (CCfits::FitsException&)
  {
     string msg("Error reading ");
     msg += errorKey + string(" keyword(s) in file: ");
     msg += fileName;
     throw WmapRegionError(msg);
  }
}

void WmapRegion::readWmap (int* nullVal)
{
  CCfits::PHDU& primary = m_file->pHDU();
  try
  {
     int nPix = m_nx * m_ny;
     std::valarray<int> tmpArray;
     m_file->pHDU().read(tmpArray, 1, nPix, nullVal);

     const int EMPTY = -1;
     m_wmapPos.clear();
     m_isPixOn.resize(nPix, false);
     for (int i=0; i<nPix; ++i)
     {
        if (tmpArray[i] != EMPTY)
        {
           m_isPixOn[i] = true;
           m_wmapPos.push_back(i);
        }
     }
     int sz = m_wmapPos.size();
     m_wmap.resize(sz);
     // SunCC v6 has linktime trouble when m_isPixOn is used
     // as a mask_array.  So, do this the more cumbersome way...
     for (int i=0; i<sz; ++i)
        m_wmap[i] = tmpArray[m_wmapPos[i]];
  }
  catch (CCfits::FitsException&)
  {
     string msg("Error attempting to read image in primary HDU of ");
     const CCfits::PHDU& chdu = const_cast<const CCfits::PHDU&>(primary);
     msg += chdu.parent()->name();
     throw WmapRegionError(msg);     
  }
}

Real WmapRegion::getVersion ()
{
  string keyword("HDUVERS");
  string keyval("");
  Real vers = -1.0;
  CCfits::PHDU& primary = m_file->pHDU();
  try
  {
     primary.keyWord(keyword).value(keyval);
  }
  catch (CCfits::FitsException&)
  {
     // Failed once, try slightly different keyword.
     keyword += "1";
     primary.keyWord(keyword).value(keyval);
  }
  std::istringstream ssVal(keyval);
  // If this fails (keyval does not start with a Real), vers
  // will remain negative and calling program will deal with it.
  ssVal >> vers;

  return vers;
}

void WmapRegion::getWmrbn ()
{
  float tmpFloat = .0;
  m_file->pHDU().keyWord(m_wmrbnKey).value(tmpFloat);
  m_wmrbn = int(tmpFloat);
}

void WmapRegion::getCorners ()
{
  float tmpFloat = .0;
  m_file->pHDU().keyWord(m_refBinKeys.first).value(tmpFloat);
  m_lowerLeft.first = static_cast<Real>(tmpFloat);
  m_file->pHDU().keyWord(m_refBinKeys.second).value(tmpFloat);
  m_lowerLeft.second = static_cast<Real>(tmpFloat);
  m_upperRight.first = m_lowerLeft.first + static_cast<Real>(m_nx*m_wmrbn);
  m_upperRight.second = m_lowerLeft.second + static_cast<Real>(m_ny*m_wmrbn);
}

void WmapRegion::read (const string& fileName, int* nullVal)
{
  // Re-open the spectrum file as given by fileName, which should
  // be the full path name.
  try
  {
     std::auto_ptr<CCfits::FITS> pf(new CCfits::FITS(fileName));
     m_file = pf.get();
     // Any CCfits exceptions thrown inside the following
     // read functions will be caught there and rethrown
     // as Xspec exceptions.
     readCommonKeys();
     readSpecificKeys();
     readWmap(nullVal);
     // m_file pointer will be invalid once pf goes out of scope.
     m_file = 0;
  }
  catch (CCfits::FITS::CantOpen)
  {
     string msg = "Cannot open file: ";
     msg += fileName;
     msg += "\nfor retrieving data for mixing model.";
     throw WmapRegionError(msg);
  }
  catch (CCfits::FitsException&)
  {
     throw WmapRegionError("");
  }
}

// Additional Declarations
  WmapRegion::~WmapRegion() {}
