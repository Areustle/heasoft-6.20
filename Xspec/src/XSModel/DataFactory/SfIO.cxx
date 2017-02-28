//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// XSutility
#include <XSUtil/Utils/XSutility.h>
// SfIO
#include <XSModel/DataFactory/SfIO.h>
#include <fstream>


// Class SfIO 

SfIO::SfIO()
{
}


size_t SfIO::read (const string& fileName, bool readFlag)
{
  return 1;  // for now
}

void SfIO::write (const string& fileName)
{
}

bool SfIO::fileFormat (const string& fileName, XspecDataIO::DataType type)
{
  static const string SF = "SF01";
  string sfmt;
  std::ifstream sffile(fileName.c_str(),std::ios_base::in);

  if (!sffile) throw XspecDataIO::CannotOpen(fileName);

  getline(sffile,sfmt);  

  bool isSfData = false;
  if (sfmt.substr(4,4) == SF)
  {
        sffile.close(); 
        // ...  code that calls sf package to check that the file contains
        // spectrum data and sets isSfData to true if so.

  }
  return isSfData;
  // note that streams are explicitly closed on destruction.   
}

void SfIO::channelBounds (int& startChannel, int& endChannel, size_t row) const
{
}

void SfIO::closeFile ()
{
}

// Additional Declarations
