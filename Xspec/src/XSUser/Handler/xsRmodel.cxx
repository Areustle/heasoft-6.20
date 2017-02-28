#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <sstream>

int
XSGlobal::xsRmodel (ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doRmodel(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doRmodel(const StringArray& rawArgs)
{
   using namespace XSContainer;
   static const string NONE("none");
   static const string CLEAR("clear");
   static const string GAIN("gain");

   const char* cmd = "rmodel";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1 || rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
   }
   else
   {
      try
      {
         string firstArg(rawArgs[1]);
         string lFirstArg(XSutility::lowerCase(firstArg));
         if (lFirstArg.substr(0,5) == CLEAR)
         {
            // Initial implementation (4/09), this is synonomous with
            // removing all gain parameters.  No other types of
            // response parameters exist yet.
            if (responses->totalResponseParams())
            {
               responses->clearGainParameters();
               tcout << "\nAll response parameters are now removed.\n"<<std::endl;
            }
            else
            {
               tcout <<"\nThere are no response parameters to remove.\n"<<std::endl;
            }
            return 0;
         }
         else if (nArgs == 2)
         {
            printDocs(cmd,"?");
            return -1;
         }
         else
         {
            string secondArg(rawArgs[2]);
            string lSecondArg(XSutility::lowerCase(secondArg));
            if (lSecondArg == NONE)
            {
               // Remove response parameters and reset gain back to
               //  original values.
               int sourceNum=1;
               int specNum=1; 
               Response* resp = 
                  HandlerUtils::getFromIntPair<Response>(firstArg,sourceNum,specNum);
               if (!resp)
               {
                  std::ostringstream oss;
                  oss << "Spectrum "<<specNum<<" has no response for source "<<sourceNum<<'\n';
                  throw YellowAlert(oss.str());
               }
               resp->removeGain();
               tcout << " Gain removed from response for spectrum "<<specNum
                  <<" source "<<sourceNum<<std::endl;
               datasets->Notify();
            }
            else
            {
               if (GAIN.find(secondArg) == 0)
               {
                  // Treat exactly as "gain fit source:spec [<batch args>]"
                  // Shouldn't even be able to get here with nArgs < 3.
                  if (nArgs < 3)
                  {
                     throw YellowAlert("Internal rmodel arguments parsing error.\n");
                  }
                  StringArray newArgs(nArgs);
                  newArgs[0] = GAIN;
                  newArgs[1] = "fit";
                  newArgs[2] = firstArg;
                  for (size_t i=3; i<nArgs; ++i)
                  {
                     const string& batchArg = rawArgs[i];
                     newArgs[i] = batchArg;
                  }

                  int retval = XSGlobal::doGain(newArgs);
                  return retval;
               }
               else
               {
                  string err("Unrecognized response model: ");
                  err += secondArg;
                  err += "\n  Valid models: gain\n";
                  throw YellowAlert(err);
               }
            }
         } // end if nArgs > 2
      }
      catch (YellowAlert&)
      {
         return -1;
      }

   }

   return 0;
}
