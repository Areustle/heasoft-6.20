//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ComponentInfo
#include <XSUtil/FunctionUtils/ComponentInfo.h>


// Class ComponentInfo 


ComponentInfo::ComponentInfo()
      : name(""),
        sourceFile(""),
        location(0),
        type("null"),
        error(false),
        infoString(""),
        isStandardXspec(false),
        isPythonModel(false),
        isSpecDependent(false)
{
}

ComponentInfo::ComponentInfo (const string& componentName, const string& dataFile, size_t loc, const string& componentType, bool errorFlag, string addString, bool isStandard)
      : name(componentName),
        sourceFile(dataFile),
        location(loc),
        type(componentType),
        error(errorFlag),
        infoString(addString),
        isStandardXspec(isStandard),
        isPythonModel(false),
        isSpecDependent(false)
{
}


void ComponentInfo::reset ()
{
  name = "";
  sourceFile = "";
  location = std::string::npos;
  type = "nul";    
  error = false;
  infoString = "";
  isStandardXspec = false;
  isPythonModel = false;
  isSpecDependent = false;
  
}
