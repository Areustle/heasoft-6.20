//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SpectralData
#include <XSModel/Data/SpectralData.h>
// typeinfo
#include <typeinfo>
// EinsteinResponse
#include <XSModel/Data/Detector/EinsteinResponse.h>


// Class EinsteinResponse 

EinsteinResponse::EinsteinResponse()
{
}

EinsteinResponse::EinsteinResponse(const EinsteinResponse &right)
      : RealResponse(right),EinsteinIO(right)
{
}


EinsteinResponse::~EinsteinResponse()
{
}


EinsteinResponse* EinsteinResponse::clone () const
{

  return new EinsteinResponse(*this);
}

bool EinsteinResponse::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  return EinsteinIO::fileFormat(fileName,type);
}

void EinsteinResponse::groupQuality ()
{
}

void EinsteinResponse::setArrays ()
{
}

void EinsteinResponse::setDescription (size_t spectrumNumber, size_t groupNumber)
{
}

size_t EinsteinResponse::read (const string& fileName, bool readFlag)
{
  // do nothing (yet)
  return 1;   // for now
}

void EinsteinResponse::closeSourceFiles ()
{
}

// Additional Declarations
