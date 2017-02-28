//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// IosHolder
#include <XSUtil/Utils/IosHolder.h>


// Class IosHolder 
std::istream* IosHolder::s_inHolder = &std::cin;
std::ostream* IosHolder::s_outHolder = &std::cout;
std::ostream* IosHolder::s_errHolder = &std::cerr;
const char* IosHolder::s_xsPrompt = "XSPEC12>";

void IosHolder::setStreams (std::istream* inStream, std::ostream* outStream, std::ostream* errStream)
{
  s_inHolder = inStream;
  s_outHolder = outStream;
  s_errHolder = errStream;
}

// Additional Declarations
