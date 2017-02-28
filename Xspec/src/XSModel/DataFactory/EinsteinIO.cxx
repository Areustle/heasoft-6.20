//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSModel/DataFactory/EinsteinIO.h>
#include <XSUtil/Utils/XSutility.h>
#include <CCfits/CCfits>
#include <XSstreams.h>
#include <cstring>


// Class EinsteinIO 

EinsteinIO::EinsteinIO()
  : m_dataSource(static_cast<CCfits::FITS*>(0))
{
}

EinsteinIO::EinsteinIO(const EinsteinIO &right)
  : m_dataSource(static_cast<CCfits::FITS*>(0))
{
}


EinsteinIO::~EinsteinIO()
{
  delete m_dataSource;
}


void EinsteinIO::write (const string& fileName)
{
}

size_t EinsteinIO::read (const string& fileName, bool readFlag)
{
  return 1;  // for now
}

bool EinsteinIO::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  // default implementation of fileFormat, detects SPECTRUM types.
  using namespace CCfits;
  static const string specExt = "SPECTRUM";
  static const string tele    = "TELESCOP";
  static char einstein[] = "EINSTEIN";
  bool form = false;
  try
  {
          std::auto_ptr<FITS> p(new FITS(fileName,Read,specExt));
          ExtHDU& spectrum = p->extension(specExt);
          string keyValue("");
          try 
          {
                spectrum.readKey(tele,keyValue);
                // it's an Einstein spectrum extension if keyValue == einstein
                form =  (strcasecmp(keyValue.c_str(),einstein) == 0);

          }
          catch (HDU::NoSuchKeyword) 
          {

          } 
  } 
  catch (FitsException& )
  {

  }

  return form;  
}

void EinsteinIO::channelBounds (int& startChannel, int& endChannel, size_t row) const
{
}

void EinsteinIO::closeFile ()
{
  delete m_dataSource;
  m_dataSource = 0;  
}

// Additional Declarations
