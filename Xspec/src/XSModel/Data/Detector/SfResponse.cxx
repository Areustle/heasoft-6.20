//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SpectralData
#include <XSModel/Data/SpectralData.h>
// typeinfo
#include <typeinfo>
// SfResponse
#include <XSModel/Data/Detector/SfResponse.h>


// Class SfResponse 

SfResponse::SfResponse()
{
}

SfResponse::SfResponse(const SfResponse &right)
      : RealResponse(right), SfIO(right)
{
}


SfResponse::~SfResponse()
{
}


size_t SfResponse::read (const string& fileName, bool readFlag)
{
  return 1;  //for now
}

SfResponse* SfResponse::clone () const
{

  return new SfResponse(*this);
}

bool SfResponse::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return SfIO::fileFormat(fileName,type);
}

void SfResponse::setArrays ()
{
}

void SfResponse::setDescription (size_t spectrumNumber, size_t groupNumber)
{
}

void SfResponse::closeSourceFiles ()
{
}

// Additional Declarations
