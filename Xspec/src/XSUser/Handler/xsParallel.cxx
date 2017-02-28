#include <xsTypes.h>
#include <XSstreams.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/ProcessManager.h>

int
XSGlobal::xsParallel(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doParallel(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doParallel(const StringArray& rawArgs)
{
   using namespace std;
   
   map<string,int>::iterator itMaxProcs = ProcessManager::maxProcs().begin();
   map<string,int>::iterator itMaxProcsEnd = ProcessManager::maxProcs().end();
   if (rawArgs.size() > 1)
   {
      try
      {         
         string errMsg("Proper usage is \"parallel <task> <maxNProcs>\"");
         errMsg+="\n     where <task> = ";
         errMsg+= (itMaxProcs++)->first;
         while (itMaxProcs != itMaxProcsEnd)
         {
            errMsg += " | ";
            errMsg += itMaxProcs->first;
            ++itMaxProcs;
         }
         errMsg += "\n     Input was";
	 for (size_t i=0; i<rawArgs.size(); i++) {
	   errMsg += " ";
	   errMsg += rawArgs[i];
	 }
         errMsg += "\n";
         if (rawArgs.size() < 3)
            throw YellowAlert(errMsg);
         size_t testInt = XSutility::isInteger(rawArgs[2]);
         if (testInt == string::npos)
            throw YellowAlert(errMsg);
         int nProcs = static_cast<int>(testInt);
         if (nProcs < 0)
            throw YellowAlert("<maxNProcs> is too large for signed integer value.\n");
         string parallelCase(XSutility::lowerCase(rawArgs[1]));
         itMaxProcs = ProcessManager::maxProcs().lower_bound(parallelCase);
         if (itMaxProcs != itMaxProcsEnd && itMaxProcs->first.find(parallelCase) == 0)
         {
            itMaxProcs->second = nProcs;
         }
         else
            throw YellowAlert(errMsg);
      }
      catch (...)
      {
         return -1;
      }
   }
   else
   {
      tcout << "Maximum number of parallel processes: "<<endl;
      while (itMaxProcs != itMaxProcsEnd)
      {
         tcout << "   " << itMaxProcs->first << ": " << itMaxProcs->second<<endl;
         ++itMaxProcs;
      }
   }
   return 0;
}
