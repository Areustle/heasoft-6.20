//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataSet
#include <XSModel/Data/DataSet.h>
// SfBackground
#include <XSModel/Data/BackCorr/SfBackground.h>


// Class SfBackCorr 

SfBackCorr::SfBackCorr()
  : BackCorr()
{
}

SfBackCorr::SfBackCorr(const SfBackCorr &right)
  : BackCorr(right)
{
}


SfBackCorr::~SfBackCorr()
{
}


bool SfBackCorr::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return SfIO::fileFormat(fileName,type);
}

SfBackCorr* SfBackCorr::clone () const
{

  return new SfBackCorr(*this);
}

void SfBackCorr::setDescription (size_t spectrumNumber)
{
}

void SfBackCorr::scaleArrays (bool correction)
{
}

void SfBackCorr::setArrays (size_t backgrndRow, bool ignoreStats)
{
}

size_t SfBackCorr::read (const string& fileName, bool readFlag)
{
  return SfIO::read(fileName);
}

void SfBackCorr::closeSourceFiles ()
{
}

void SfBackCorr::initialize (DataSet* parentData, size_t row, const string& fileName, size_t backCorRow)
{
}

// Additional Declarations
