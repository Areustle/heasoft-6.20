//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include "XSContainer.h"
#include "XSstreams.h"

// Bearden
#include <XSModel/Model/EmissionLines/Bearden.h>


// Class Bearden 
string Bearden::s_ELEMENT = "ELEMENT";
string Bearden::s_TRANS = "TRANS";
string Bearden::s_ENERGY = "ENERGY";
string Bearden::s_WAVE = "WAVE";
StringArray Bearden::s_colNames;

Bearden::Bearden()
{
  s_colNames.push_back(s_ELEMENT);
  s_colNames.push_back(s_TRANS);
  s_colNames.push_back(s_ENERGY);
  s_colNames.push_back(s_WAVE);
}

Bearden::Bearden(const Bearden &right)
  : LineList(right)
{
  const string BeardenFile("Bearden.fits");
  string fullName = FunctionUtility::modelDataPath() + BeardenFile;
  fileName(fullName);  
}


Bearden::~Bearden()
{
}


LineList* Bearden::clone () const
{
  return new Bearden(*this);
}

void Bearden::readData ()
{
  using namespace CCfits;
  if (!file())
  {
     throw RedAlert("Attempting to read from unopened Line List file.");
  }
  try
  {
     ExtHDU& data = file()->extension(1);
     long nRows = data.rows();
     if (!nRows)
     {
        string msg("Error finding required data in ext 1 of file: ");
        msg += fileName();
        throw FileFormatError(msg);        
     }
     data.readData(true, s_colNames);
     data.column(s_TRANS).read(m_trans, 1, nRows);
     data.column(s_ELEMENT).read(m_element, 1, nRows);
     string colToGet = isWave() ? s_WAVE : s_ENERGY;
     data.column(colToGet).read(m_energyWave, 1, nRows);
  }
  catch (FitsException&)
  {
     string msg("Error finding required data in ext 1 of file: ");
     msg += fileName();
     throw FileFormatError(msg);
  }
}

void Bearden::report (std::ostream& os) const
{
  using namespace std;
  if (m_iRows.first == m_iRows.second)
  {
     tcout << "\n    No lines found in the specified range." <<std::endl;
  }
  else
  {
     for (size_t i=m_iRows.first; i<m_iRows.second; ++i)
     {
        os << right << setw(12) << "Bearden" << setw(7) << ":" <<
              setw(11) << fixed << showpoint << setprecision(4) <<
              m_energyWave[i] << setw(7) << m_element[i] << " " <<
              left << setw(20) << m_trans[i] << std::endl;
     }
  }
}

void Bearden::findLines ()
{
  size_t sz = m_energyWave.size();
  Real low = energyRange().first;
  Real high = energyRange().second;
  m_iRows.first = 0, m_iRows.second = 0;
  bool isFound = false;
  // NOTE: This assumes wavelegnths are sorted in INCREASING order,
  //       energies are sorted in DECREASING order.
  // Finds m_iRows.first and m_iRows.second such that valid range  is [m_iRows.first, m_iRows.second),
  //   so for energy mode, m_iRows.first actually points to eHigh and m_iRows.second
  //   is one past eLow. 
  if (isWave())
  {
     for (size_t i=0; i<sz; ++i)
     {
        if (m_energyWave[i] >= low)
        {
           isFound = true;
           m_iRows.first = i;
           break;
        }
     }
     if (!isFound)
     {
        // All wavelengths are lower than the specified minimum.
        return;
     }
     m_iRows.second = m_iRows.first;
     for (size_t i=m_iRows.first; i<sz; ++i)
     {
        if (m_energyWave[i] <= high)
        {
           ++m_iRows.second;
        }
        else
        {
           break;
        }
     }
     // If All wavelengths are lower than the specified maximum,
     // then m_iRows.second = sz at this point.
  }
  else
  {
     for (size_t i=0; i<sz; ++i)
     {
        if (m_energyWave[i] <= high )
        {
           isFound = true;
           m_iRows.first = i;
           break;
        }
     }
     if (!isFound)
     {
        // All energies are higher than the specified maximum.
        return;
     }
     m_iRows.second = m_iRows.first;
     for (size_t i=m_iRows.first; i<sz; ++i)
     {
        if (m_energyWave[i] >= low)
        {
           ++m_iRows.second;
        }
        else
        {
           break;
        }
     }
  }
}

// Additional Declarations
