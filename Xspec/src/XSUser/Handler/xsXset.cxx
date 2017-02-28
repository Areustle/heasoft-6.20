#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <sstream>

int
XSGlobal::xsXset(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doXset(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doXset(const StringArray& rawArgs)
{
   const char* cmd = "xset";
   const size_t nArgs = rawArgs.size();
   int status=0;
   if (nArgs == 1)
   {
      std::map<string,string>::const_iterator 
                      itMS (FunctionUtility::modelStringDataBase().begin());
      std::map<string,string>::const_iterator 
                      itEnd (FunctionUtility::modelStringDataBase().end());
      tcout << "\nCurrent model string variables:" <<std::endl;
      while (itMS != itEnd)
      {
         tcout << "  " << itMS->first << "    " << itMS->second <<std::endl;
         ++itMS;
      }
   }
   else if (rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
   }
   else
   {
      enum SUBCOMS {ABUND, COSMO, DELTA, MDATADIR, METHOD, SEED, 
           STATISTIC, WEIGHT, XSECT};
      std::vector<string>::const_iterator itArg1 = rawArgs.begin();
      ++itArg1;
      std::vector<string>::const_iterator itEnd = rawArgs.end();
      const string arg1 = *itArg1;
      const string lcArg1 = XSutility::lowerCase(arg1);
      const string ucArg1 = XSutility::upperCase(arg1);
      std::map<string,int> options;
      options["abund"] = ABUND;
      options["cosmo"] = COSMO;
      options["delta"] = DELTA;
      options["mdatadir"] = MDATADIR;
      options["method"] = METHOD;
      options["seed"] = SEED;
      options["statistic"] = STATISTIC;
      options["weight"] = WEIGHT;
      options["xsect"] = XSECT;
      const std::map<string,int>::const_iterator itOpt = 
                   options.lower_bound(lcArg1);
      if (itOpt != options.end() && itOpt->first.find(lcArg1)==0)
      {
         switch (itOpt->second)
         {
            case ABUND:
               status = XSGlobal::doAbund(StringArray(itArg1, itEnd));
               break;
            case COSMO:
               status = XSGlobal::doCosmo(StringArray(itArg1, itEnd));
               break;
            case DELTA:
               if (nArgs > 2)
               {
                  string tmpStr(rawArgs[2]);
                  std::istringstream iss(tmpStr);
                  Real delta = .0;
                  if (!(iss >> delta) || !iss.eof())
                  {
                     tcerr << "***Error: Delta must be given a floating-point value."
                        << std::endl;
                     return -1;
                  }
                  else
                  {
                     XSContainer::models->proportionalDelta(delta);
                     if (delta > 0.0)
                     {
                        tcout << "Parameter delta values for fits will now be "
                           << delta << " * parValue" << std::endl;
                     }
                     else
                     {
                        tcout << "xset delta <= 0.0:  Parameters will now use their own fixed delta values."
                          << std::endl;
                     }
                  }
               }
               else
               {
                  tcout << "Set or remove usage of proportional parameter fit delta values:"
                     <<  "\n           xset delta <value>"
                     <<  "\n   where <value> <= 0.0 removes usage of proportional deltas." 
                     <<  "\n   Current value: " << XSContainer::models->proportionalDelta() << std::endl;
               }
               break;
            case MDATADIR:
               if (nArgs > 2)
               {
                  string query("Change model data directory to: ");
                  string newDir(rawArgs[2]);
                  query += newDir;
                  query += "\nAre you sure? (y/n) ";

                  if (XSutility::yesToQuestion(query,0,tcin)==1)
                  {
                     FunctionUtility::modelDataPath(newDir);
                     tcout << "Model data directory has been updated." 
                           << std::endl;
                  }
               }
               break;
            case METHOD:
               status = XSGlobal::doMethod(rawArgs);
               break;
            case SEED:
               if (nArgs > 2)
               {
                  int seed = 0;
                  std::istringstream iss(rawArgs[2]);
                  if (!(iss >> seed) || !iss.eof())
                  {
                     tcout << "***Error: Seed value must be an integer" << std::endl;
                     return -1;
                  }
                  Numerics::DefaultRandomGenerator& randGen = 
                        Numerics::DefaultRandomGenerator::instance();
                  randGen.seed(seed);
                  randGen.initialize();
                  tcout << "\nRandom number generator has been re-initialized with seed = "
                     << seed << std::endl << std::endl; 
               }
               else
               {
                  tcout << "   xset seed requires an integer argument." << std::endl;
               }
               break;
            case STATISTIC:
               status = XSGlobal::doStatistic(StringArray(itArg1, itEnd));
               break;
            case WEIGHT:
               status = XSGlobal::doWeight(StringArray(itArg1, itEnd));
               break;
            case XSECT:
               status = XSGlobal::doXsect(StringArray(itArg1, itEnd));
               break;                    
            default:
               break;
         }
      }
      else
      {
         // model string name/value
         string stringValue("");
         if (nArgs > 2)
         {
            stringValue = string(rawArgs[2]);
         }
         FunctionUtility::setModelString(ucArg1, stringValue);

         // We can't be sure what effect the user's setting is intended
         // to cause.  So to be safe do a full update:
         XSContainer::datasets->Notify();
      }

   }

   return status;
}
