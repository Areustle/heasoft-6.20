//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SpectralData
#include <XSModel/Data/SpectralData.h>
// XspecDataIO
#include <XSModel/DataFactory/XspecDataIO.h>

#include <iostream>
#ifdef INXSPEC
#include "XSstreams.h"
#define _ERRSTREAM tcerr
#else
#define _ERRSTREAM std::cerr
#endif


// Class XspecDataIO::CannotOpen 

XspecDataIO::CannotOpen::CannotOpen()
  : YellowAlert()
{
}

XspecDataIO::CannotOpen::CannotOpen (const string& msg)
  : YellowAlert(" cannot open file named: ")
{
  _ERRSTREAM << msg << std::endl;
}


// Class XspecDataIO::RequiredDataNotPresent 

XspecDataIO::RequiredDataNotPresent::RequiredDataNotPresent (const string& diag)
  : YellowAlert(string(" data file missing required entries: "))
{
  _ERRSTREAM << diag << std::endl;
}


// Class XspecDataIO::SingleSpectrumOnly 

XspecDataIO::SingleSpectrumOnly::SingleSpectrumOnly (const string& diag)
  : YellowAlert(string(" multiple sources requested from file containing single source"))
{
  _ERRSTREAM << diag << std::endl;
}


// Class XspecDataIO::UnspecifiedSpectrumNumber 

XspecDataIO::UnspecifiedSpectrumNumber::UnspecifiedSpectrumNumber (const string& diag)
   : YellowAlert(string(" multiple source file needs spectrum number specified - File: ") )
{
  _ERRSTREAM << diag << std::endl;
}


// Class XspecDataIO::CatchAllIO 

XspecDataIO::CatchAllIO::CatchAllIO (const string& msg)
  : YellowAlert(string(" IO error while:\n"))
{
  _ERRSTREAM <<"  "<< msg << std::endl;
}


// Class XspecDataIO 

XspecDataIO::XspecDataIO()
{
}

XspecDataIO::~XspecDataIO() {}
