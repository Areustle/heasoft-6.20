//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSstreams.h>
#include <XSUtil/Utils/XSstream.h>

// DataFactory
#include <XSModel/DataFactory/DataFactory.h>
// Error
#include <XSUtil/Error/Error.h>
// typeinfo
#include <typeinfo>
// SfData
#include <XSModel/Data/SfData.h>
// EinsteinData
#include <XSModel/Data/EinsteinData.h>
// OGIP-92aData
#include <XSModel/Data/OGIP-92aData.h>
// SfBackground
#include <XSModel/Data/BackCorr/SfBackground.h>
// EinsteinBackground
#include <XSModel/Data/BackCorr/EinsteinBackground.h>
// OGIP-92aBackground
#include <XSModel/Data/BackCorr/OGIP-92aBackground.h>
// EinsteinResponse
#include <XSModel/Data/Detector/EinsteinResponse.h>
// SfResponse
#include <XSModel/Data/Detector/SfResponse.h>
// OGIP-92aResponse
#include <XSModel/Data/Detector/OGIP-92aResponse.h>
// XspecRegistry
#include <XSModel/DataFactory/XspecRegistry.h>



// Class XspecRegistry::UnrecognizedFormat 

XspecRegistry::UnrecognizedFormat::UnrecognizedFormat (const string& msg)
  : YellowAlert(string("file format not recognized: input file "))  
{
 tcerr << msg << std::endl; 
}


// Class XspecRegistry 
XspecRegistry* XspecRegistry::s_instance = 0;

XspecRegistry::XspecRegistry()
{
  registerNativeTypes();
}


XspecRegistry::~XspecRegistry()
{
   std::map<const char*,DataPrototype*>::iterator itMap = m_dataFormats.begin();
   std::map<const char*,DataPrototype*>::iterator itMapEnd = m_dataFormats.end();
   while (itMap != itMapEnd)
   {
      delete itMap->second;
      ++itMap;
   }
}


void XspecRegistry::addToRegistry (DataFactory* factory)
{

        try
        {
                // can throw bad_cast.
                DataPrototype* proto = static_cast<DataPrototype*>(factory);
                dataFormats(typeid(*(proto->protoDataSet())).name(),proto);
        }
        catch (...)
        {
                // throw a YellowAlert here, to allow this to be caught by
                // calling routine.
                throw YellowAlert();       
        }
}

void XspecRegistry::registerNativeTypes ()
{

  try
  {
        addToRegistry(new DataPrototype(new OGIP_92aData, new OGIP_92aResponse, 
                      new OGIP_92aBackground,new OGIP_92aCorrection));

      //  addToRegistry(new DataPrototype(new EinsteinData, new EinsteinResponse, 
     //                 new EinsteinBackground, new EinsteinCorrection));

     //   addToRegistry(new DataPrototype(new SfData, new SfResponse, 
     //                   new SfBackground,new SfCorrection));
   }
   catch (YellowAlert)
   {
         // if registering the native datatypes fails, terminate.  
         throw RedAlert("Error registering XSPEC data formats. Exiting");  
   }
}

XspecRegistry* XspecRegistry::Instance ()
{
  if (s_instance == 0) {s_instance = new XspecRegistry;} return s_instance;
}

DataPrototype* XspecRegistry::returnPrototype (const string& fileName, XspecDataIO::DataType type)
{
  std::map<const char*, DataPrototype*>::const_iterator dp =     m_dataFormats.begin();
  std::map<const char*, DataPrototype*>::const_iterator endList = m_dataFormats.end();
  DataPrototype* test = 0;
  try
  {
     // Since it's OK to have mismatches before getting
     // the right dataset class type, turn off whatever error
     // output the various fileFormat functions may produce.
     tperr.setRestrictedVerbose(9999);
     while (dp != endList)
     {
             test = dp->second;
             if (test->protoDataSet()->fileFormat(fileName, type)) break;
             ++dp;
     }
     tperr.removeRestrictedVerbose();
  }
  catch (YellowAlert&)
  {
     tperr.removeRestrictedVerbose();
     throw;
  }
  if ( dp == endList ) throw UnrecognizedFormat(fileName);

  return test;
}

DataPrototype* XspecRegistry::returnPrototype (const std::type_info& dataClass)
{
  std::map<const char*, DataPrototype*>::const_iterator dp =     m_dataFormats.begin();
  std::map<const char*, DataPrototype*>::const_iterator endList = m_dataFormats.end();
  DataPrototype* test = 0;
  while (dp != endList)
  {
          test = dp->second;
	  if (typeid(*(test->protoDataSet())) == dataClass)  break;
          ++dp;
  }
  if ( dp == endList ) throw UnrecognizedFormat(dataClass.name());

  return test;
}

// Additional Declarations

// Class RegisteredFormat 

RegisteredFormat::RegisteredFormat()
{
}


bool RegisteredFormat::isRegistered (const XspecRegistry& registry) const
{
  return true;
}

// Additional Declarations
RegisteredFormat::~RegisteredFormat() {}
