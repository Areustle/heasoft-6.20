//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataSetBase
#include <XSModel/Data/DataSetBase.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// EinsteinData
#include <XSModel/Data/EinsteinData.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>


// Class EinsteinData 

EinsteinData::EinsteinData()
  : DataSet(), EinsteinIO()
{
}

EinsteinData::EinsteinData(const EinsteinData &right)
  : DataSet(right), EinsteinIO(right)
{
}


EinsteinData::~EinsteinData()
{
}


EinsteinData* EinsteinData::clone () const
{

  return new EinsteinData(*this);
}

bool EinsteinData::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  // oh, the joys of multiple inheritance!
  // fileFormat is virtual both in RegisteredFormat and EinsteinIO.
  // the default implementation for EinsteinIO::fileFormat returns
  // true for Einstein format spectrum files (meaning, spectrum
  // background and correction). For those objects we can just call that
  // EinsteinIO default implementation and we're done.
  return EinsteinIO::fileFormat(fileName);
}

void EinsteinData::defineArrays ()
{
}

void EinsteinData::groupQuality ()
{
}

void EinsteinData::setArrays (size_t row)
{
}

void EinsteinData::scaleArrays (size_t row)
{
}

void EinsteinData::initialize (DataPrototype* proto, DataInputRecord& record)
{
}

void EinsteinData::setDescription (size_t spectrumNumber, size_t row)
{
}

bool EinsteinData::setAncillaryData (size_t row, int ancRow)
{
  return true;
}

void EinsteinData::closeSourceFiles ()
{
}

void EinsteinData::reportResponse (size_t row) const
{
}

void EinsteinData::initializeFake (DataPrototype* proto, FakeDataInputRecord& record)
{
}

void EinsteinData::outputData ()
{
}

std::pair<string,size_t>  EinsteinData::getBackCorrLocation (size_t rowNum, bool isCorr) const
{
  return std::pair<string,size_t>(string(),0);
}

FakeDataInputRecord::Detectors EinsteinData::getResponseName (size_t rowNum) const
{
   return FakeDataInputRecord::Detectors();
}

FakeDataInputRecord::Arfs EinsteinData::getAncillaryLocation (size_t rowNum, const FakeDataInputRecord::Detectors& respInfo) const
{
   return FakeDataInputRecord::Arfs();
}

// Additional Declarations
