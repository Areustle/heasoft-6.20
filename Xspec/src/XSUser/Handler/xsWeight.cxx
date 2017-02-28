//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <xsTypes.h>

int
XSGlobal::xsWeight(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doWeight(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doWeight(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "weight";
   const size_t nArgs = rawArgs.size();
   string arg;
   bool isChanged = false;
   if (nArgs == 1 || (arg = rawArgs[1]) == "?")
   {
       printDocs(cmd,"?");
   } 
   else
   {
       // StatManager's weightCmdSetting stores what the user has selected as
       // the weighting, either through their init file or this command.
       // DataUtility::statWeight holds the scheme that is actually applied. 
       // These may differ if the chosen statistic doesn't allow the selected 
       // weighting.

       Weight* tmpTest = StatManager::getWeightingMethod(arg);
       StatManager* manager = fit->statManager();
       const string& origWeightInUse = DataUtility::statWeight().name();
       if (!tmpTest)
       {
          tcerr << "*** Unrecognized weighting scheme: " << arg << std::endl;
          printDocs(cmd,"?");
          tcerr << "Current selected weighting scheme: " << manager->weightCmdSetting() << std::endl;
          if (manager->weightCmdSetting() != origWeightInUse)
             tcerr << "Actual APPLIED weighting scheme: "
                   << origWeightInUse << std::endl;
          return -1;   
       }

       const string GEHRELS("gehrels");
       if (GEHRELS.find(XSutility::lowerCase(arg)) == 0)
       {
          tcout << "Warning: this weight scheme is only valid for Poisson data." 
             << std::endl;
       }

       // This call will set StatManager's weightCmdSetting to arg, and
       // DataUtility's statWeight to either arg or in case of conflict,
       // "standard".  If there is a conflict, it will issue a warning message.
       manager->setStatWeight(arg);
       if (origWeightInUse != DataUtility::statWeight().name())
          isChanged = true;  
   }

   if (isChanged)
   {
      StatManager::recalculateWeightVariances();
      fit->Update();
   }
   tcout << std::flush;
   return 0;
}
