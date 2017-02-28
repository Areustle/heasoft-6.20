//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>

#include <iomanip>

int
XSGlobal::xsComsum(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doComsum(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doComsum(const StringArray& rawArgs)
{
   using namespace std;
   const size_t nArgs = rawArgs.size();
   string docCommand;

   if (nArgs > 1)
   {
      docCommand = rawArgs[1];
   }

   map<string,string>::const_iterator docs;

   if (docCommand.size() == 0)
   {

      for ( docs = XSGlobal::summaryMap.begin(); docs != XSGlobal::summaryMap.end(); docs++)
      {
         tcout << docs->first << ": " << docs->second  << "\n" << endl;   
      }
   }
   else
   {
      if ( (docs = XSGlobal::summaryMap.find(docCommand)) != XSGlobal::summaryMap.end() )
      {
         tcout << setw(8) << docs->first << ": " << docs->second   << endl;   
      }
      else 
      {
         tcout << "No such command: " << docCommand  << endl;   
      }
   }    
   return 0;
}

