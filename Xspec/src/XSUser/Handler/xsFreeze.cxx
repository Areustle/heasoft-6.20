//
//  XSPEC12  November 2003
//
//

#include <xsTypes.h>
#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSModel/Parameter/Parameter.h>  
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUser/UserInterface/xstcl.h>
#include <XSUtil/Parse/XSparse.h>
#include <sstream>

namespace
{
   // Strategy class needed for freezeThawUntie template function.
   class FreezeStrategy
   {
      public:
         static bool perform(Parameter* par, string& msg);
   };
}

int
XSGlobal::xsFreeze(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doFreeze(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doFreeze(const StringArray& rawArgs)
{
   using namespace XSContainer;
   
   const size_t nArgs = rawArgs.size();

   // prevModelName is only ever applied when user enters command with no args,
   // in which case the previous <modName>:<range> specifier (if any) is used.
   // prevRange can also be applied when using single '*' notation in range.
   RangePair prevRange = HandlerUtils::FreezeThawRange::getRange();
   const string& prevModelName = HandlerUtils::FreezeThawRange::getGroupName();

   bool isAnyChanged = false;
   const bool isRespPar = (rawArgs[0] == string("rfreeze"));
   const char* cmd = isRespPar ? "rfreeze" : "freeze";
   try
   {
      string defName;
      StringArray inArgs;
      if (nArgs == 1)
      {
         if (prevRange.second == 0)
         {
            printDocs(cmd,"?");
            return 0;
         }
         else
         {
            defName = prevModelName;
            std::ostringstream oss;
            oss << prevRange.first;
            if (prevRange.second > prevRange.first)
               oss << "-" << prevRange.second;
            inArgs.push_back(oss.str());
         }
      }   
      else
      {
         string arg(rawArgs[1]);
         if (arg[0] == '?') 
         {
            printDocs(cmd,"?");
            return 0;
         }
         else
         {
            for (size_t i=1; i<nArgs; ++i)
               inArgs.push_back(rawArgs[i]);
         }      
      }


      // Break up input args into groups of StringArrays, where
      // each group belongs to a particular model.  Then we
      // can use the getRanges function to do the grunt work.
      std::map<string, StringArray> modelArgGroups;
      XSparse::collateRanges(inArgs, defName, modelArgGroups);

      std::map<string, StringArray>::iterator itModGroups = 
             modelArgGroups.begin();
      std::map<string, StringArray>::iterator itModGroupsEnd =
             modelArgGroups.end();
      while (itModGroups != itModGroupsEnd)
      {
         bool isChanged = false;
         if (isRespPar)
            isChanged = freezeThawUntie<ResponseParam,FreezeStrategy>
                              (itModGroups->first,itModGroups->second, prevRange);
         else
            isChanged = freezeThawUntie<Parameter,FreezeStrategy>
                              (itModGroups->first,itModGroups->second, prevRange);
         if (isChanged)
            isAnyChanged = true;

         HandlerUtils::FreezeThawRange::setRange(prevRange);

         // For xsFreeze, xsThaw, and xsUntie: Deliberately incrementing itModGroups 
         // this way rather than on a separate line.  This gets around a strange -O2 
         // level compiler bug that first appeared only with a build using g++ 4.2.1 
         // on powerpc-apple-darwin9.5.0, and seems to be triggered by the above 
         // template instantiations being enclosed in a loop.  
         HandlerUtils::FreezeThawRange::setGroupName((itModGroups++)->first);
      }

      if (isAnyChanged)
      {
         XSContainer::fit->Update();
      }
      return 0;     
   }
   catch (YellowAlert&)
   {
      if (isAnyChanged)
      {
         XSContainer::fit->Update();
      }
      return -1;       
   }      

}


namespace
{
   bool FreezeStrategy::perform(Parameter* par, string& msg)
   {
      bool isPerformed = false;
      if (par->isFrozen())
      {
         msg = string(" is already frozen.");
      }
      else
      {
         try
         {
            par->freeze();
            isPerformed = true;
         }
         catch(YellowAlert&)
         {
            // We want the external loops to continue.
            msg = string(" is linked and cannot be frozen.");
         }
      }
      return isPerformed;
   }
}
