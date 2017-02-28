//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataSetBase
#include <XSModel/Data/DataSetBase.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// SfData
#include <XSModel/Data/SfData.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>


// Class SfData 

SfData::SfData()
  : DataSet(),SfIO()
{
}

SfData::SfData(const SfData &right)
  : DataSet(right), SfIO(right)
{
}


SfData::~SfData()
{
}


SfData* SfData::clone () const
{

  return new SfData(*this);
}

bool SfData::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
bool format = false;
try
{
        format =   SfIO::fileFormat(fileName);     
}
catch ( XspecDataIO::CannotOpen ) {}
return format;
}

void SfData::setArrays (size_t row)
{
}

void SfData::scaleArrays (size_t row)
{
}

void SfData::initialize (DataPrototype* proto, DataInputRecord& record)
{
}

void SfData::setDescription (size_t spectrumNumber, size_t row)
{
}

bool SfData::setAncillaryData (size_t row, int ancRow)
{
  return true;
}

void SfData::closeSourceFiles ()
{
}

void SfData::reportResponse (size_t row) const
{
}

void SfData::initializeFake (DataPrototype* proto, FakeDataInputRecord& record)
{
}

void SfData::outputData ()
{
}

std::pair<string,size_t>  SfData::getBackCorrLocation (size_t rowNum, bool isCorr) const
{
  return std::pair<string,size_t>(string(),0);
}

FakeDataInputRecord::Detectors SfData::getResponseName (size_t rowNum) const
{
   return FakeDataInputRecord::Detectors();
}

FakeDataInputRecord::Arfs SfData::getAncillaryLocation (size_t rowNum, const FakeDataInputRecord::Detectors& respInfo) const
{
   return FakeDataInputRecord::Arfs();
}

// Additional Declarations
