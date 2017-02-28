//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataSet
#include <XSModel/Data/DataSet.h>
// EinsteinBackground
#include <XSModel/Data/BackCorr/EinsteinBackground.h>


// Class EinsteinBackCorr 

EinsteinBackCorr::EinsteinBackCorr()
  : BackCorr()
{
}

EinsteinBackCorr::EinsteinBackCorr(const EinsteinBackCorr &right)
  : BackCorr(right)
{
}


EinsteinBackCorr::~EinsteinBackCorr()
{
}


bool EinsteinBackCorr::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return EinsteinIO::fileFormat(fileName,type);
}

EinsteinBackCorr* EinsteinBackCorr::clone () const
{

  return new EinsteinBackground(*this);
}

void EinsteinBackCorr::setDescription (size_t spectrumNumber)
{
}

void EinsteinBackCorr::scaleArrays (bool correction)
{
}

void EinsteinBackCorr::setArrays (size_t backgrndRow, bool ignoreStats)
{
}

size_t EinsteinBackCorr::read (const string& fileName, bool readFlag)
{
  return EinsteinIO::read(fileName);
}

void EinsteinBackCorr::closeSourceFiles ()
{
}

void EinsteinBackCorr::initialize (DataSet* parentData, size_t row, const string& fileName, size_t backCorRow)
{
}

// Additional Declarations
