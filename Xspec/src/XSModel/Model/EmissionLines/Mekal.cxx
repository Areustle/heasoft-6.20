//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUtil/Numerics/Numerics.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include "XSContainer.h"
#include "XSstreams.h"

// Mekal
#include <XSModel/Model/EmissionLines/Mekal.h>


// Class Mekal 
string Mekal::s_WAVE = "WAVE";
string Mekal::s_ION = "ION";
string Mekal::s_TRANS = "TRANS";
string Mekal::s_FVALUE = "FVALUE";
StringArray Mekal::s_colNames;

Mekal::Mekal()
{
  s_colNames.push_back(s_WAVE);
  s_colNames.push_back(s_ION);
  s_colNames.push_back(s_TRANS);
  s_colNames.push_back(s_FVALUE);
}

Mekal::Mekal(const Mekal &right)
  : LineList(right)
{
  const string MekalFile("mekal_lines.fits");
  string fullName = FunctionUtility::modelDataPath() + MekalFile;
  fileName(fullName);  
}


Mekal::~Mekal()
{
}


LineList* Mekal::clone () const
{
  return new Mekal(*this);
}

void Mekal::readData ()
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
     data.column(s_WAVE).read(m_wave, 1, nRows);
     data.column(s_ION).read(m_ion, 1, nRows);
     data.column(s_TRANS).read(m_trans, 1, nRows);
     data.column(s_FVALUE).read(m_fvalue, 1, nRows);
  }
  catch (FitsException&)
  {
     string msg("Error finding required data in ext 1 of file: ");
     msg += fileName();
     throw FileFormatError(msg);
  }
}

void Mekal::report (std::ostream& os) const
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
        Real output = isWave() ? m_wave[i] : Numerics::KEVTOA/m_wave[i];
        os << right << setw(19) << "Mewe-Kaastra :" << fixed << showpoint 
              << setprecision(4) << setw(11) << output << "     " << left
              << setw(9) << m_ion[i] << setw(12) << m_trans[i] << right
              << setw(9) << fixed << showpoint << m_fvalue[i] << std::endl;
     }
  }
}

void Mekal::findLines ()
{
  size_t sz = m_wave.size();
  m_iRows.first = 0;
  m_iRows.second = 0;
  Real low = energyRange().first;
  Real high = energyRange().second;
  if (!isWave())
  {
     // Assume user input is in keV.  Convert low and high to
     // wavelengths for direct comparison to the "wave" column data.
     Real tmp = low;
     low = Numerics::KEVTOA/high;
     high = Numerics::KEVTOA/tmp;
  }
  bool isFound = false;
  // NOTE:  This assumes wavelengths are sorted in increasing order.
  // Finds m_iRows.first and m_iRows.second such that valid wavelengths are 
  // bracketted by [m_iRows.first, m_iRows.second).
  for (size_t i=0; i<sz; ++i)
  {
     if (m_wave[i] >= low)
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
     if (m_wave[i] <= high)
     {
        ++m_iRows.second;
     }
     else
     {
        break;
     }
  }
}

// Additional Declarations
