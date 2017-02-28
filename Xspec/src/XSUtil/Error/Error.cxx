//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Error
#include <XSUtil/Error/Error.h>
#include <iostream>
#include <cstdlib>
#include <XSUtil/Utils/IosHolder.h>
using std::endl;


// Class YellowAlert 

YellowAlert::YellowAlert()
{
}

YellowAlert::YellowAlert (const string& message)
{
  *IosHolder::errHolder() << "\n***XSPEC Error: " << message << std::flush;
}


// Class InputError 

InputError::InputError (const string& msg)
  : YellowAlert(msg)
{
}


YellowAlertNS::YellowAlertNS(const string& msg)
  : YellowAlert(),
   m_message(msg)
{
}


// Class RedAlert 

RedAlert::RedAlert (const string& message, int returnCode)
{
    reportAndExit(message, returnCode);
}

RedAlert::RedAlert (const std::string& msg)
{
  *IosHolder::errHolder() << "\nXSPEC Fatal Error: " << msg << endl;
}


void RedAlert::reportAndExit (const string& message, const int returnCode)
{
  *IosHolder::errHolder() << "\n*** XSPEC Fatal Error " << message << endl;
  exit(returnCode); 
}
