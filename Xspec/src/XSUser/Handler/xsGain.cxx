#include <xsTypes.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Parameter/Parameter.h>  
#include <XSModel/Parameter/ResponseParam.h>     
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h> 
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>  
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Error/Error.h>
#include <sstream>     

// Unnamed namespace, helper functions with local file scope:
namespace
{
   Real getSlope(Real current, string& param);
   Real getOffset(Real current, string& param);
   string getParamSubstring(const ResponseParam* respPar);
}

int
XSGlobal::xsGain(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doGain(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doGain(const StringArray& rawArgs)
{
   using namespace XSContainer;
   using HandlerUtils::RegEx;
   const size_t nArgs = rawArgs.size();
   const string FIT("fit");
   const string NOFIT("nofit");
   const string OFF("off");
   const string ws(" \t");
   static int specNum = 1;
   static int sourceNum = 1;
   // Secondary parameter values.  Ordering matches that
   // in model.dat file: l,b,t,h,delta.
   // Limits may be overriden by Response if file contains gain
   // limit keywords.
   string slopeParamVals("0.01     0.5     1.5     5.0    0.01");
   string offsetParamVals("-1.0     -1.0    1.0     1.0     0.01");

   if (nArgs >= 2)
   {
      const char* cmd = "gain";
      string arg = rawArgs[1];
      if (arg == "?")
      {
           printDocs(cmd,"?");
      }
      else if (FIT == XSutility::lowerCase(arg))  // gain fit [source:]spec
      {
         string cmdLine = rawArgs[1];
         for (size_t i=2; i<nArgs; ++i)
         {
            cmdLine += " ";
            cmdLine += rawArgs[i];
         }
         string cmdStr;
         string paramIdStr;
         std::deque<string> batchParams;
         XSparse::findBatchString(cmdLine, cmdStr, batchParams);
         bool promptUser(batchParams.empty());
         if (nArgs == 2 || (!promptUser && rawArgs[2]=="&"))
         {
            // User entered just "gain fit".  Only allow this for
            // 1 spectrum, 1 source.
            const size_t nSpec = datasets->numberOfSpectra();
            const size_t nSource = datasets->numSourcesForSpectra();
            if (nSpec > 1 || nSource > 1)
            {
               tcout << "***XSPEC Error: Multiple spectra and/or sources are available."
                    << "\n      You must enter \"gain fit [sourceNum:]specNum\"" <<std::endl;
               return -1;
            }
            else if (!nSpec)
            {
               tcout << "No spectra loaded:  no gains to fit" << std::endl;
               return -1;
            }
            else
            {
               specNum = sourceNum = 1;
            }
         }
         else
         {
            paramIdStr = rawArgs[2];
         }

         bool deletionOccurred = false;
         Response* resp=0;
         try
         {
            resp = HandlerUtils::getFromIntPair<Response>(paramIdStr,sourceNum,specNum);
            if (!resp)
            {
               std::ostringstream oss;
               oss << "Spectrum "<<specNum<<" has no response for source "<<sourceNum <<'\n';
               throw YellowAlert(oss.str());            
            }
            if (resp->toUserDummyResponse())
            {
               string err("Gain functionality has not been implemented for dummy responses.\n");
               throw YellowAlert(err);
            }
            
            // Use pre-existing gain settings for defaults.
            Real slope = resp->gainFactor()[1];
            Real offset = resp->gainFactor()[0];
            const ResponseParam* respPar = resp->getLinearGain();
            if (respPar)
               slopeParamVals = getParamSubstring(respPar);
            respPar = resp->getConstGain();
            if (respPar)
               offsetParamVals = getParamSubstring(respPar);

            // This will destroy previously existing response params
            // (if any) and create new ones.   
            std::ostringstream ssSlope;
            ssSlope << "slope \" \" " << slope << " " << slopeParamVals;
            std::ostringstream ssOffset;
            ssOffset << "offset \" \" " << offset << " " << offsetParamVals;
            if (resp->makeGainParams(ssSlope.str(), ssOffset.str()))
                        deletionOccurred = true; 

            string paramVals;
            if (promptUser)
            {
               resp->promptParamValues(paramVals, Response::LINEAR);
            }
            else
            {
               paramVals = XSparse::trimWhiteSpace(batchParams.front());
               batchParams.pop_front();
            }
            if (paramVals != XSparse::SKIP())
            {
               if (paramVals != XSparse::USE_DEFAULT())
               {
                  // This may throw
                  resp->setParamValues(paramVals, Response::LINEAR);
                  const ResponseParam* rp = resp->getLinearGain();
                  slope = rp->value('v');
               }
               if (promptUser)
               {
                  resp->promptParamValues(paramVals, Response::OFFSET);
               }
               else
               {
                  if (!batchParams.empty())
                  {
                     paramVals = XSparse::trimWhiteSpace(batchParams.front());
                     batchParams.pop_front();
                  }
                  else
                  {
                     paramVals = XSparse::USE_DEFAULT();
                  }
               }
               if (paramVals != XSparse::SKIP() && paramVals != XSparse::USE_DEFAULT())
               {
                  resp->setParamValues(paramVals, Response::OFFSET);
                  const ResponseParam* rp = resp->getConstGain();
                  offset = rp->value('v');
               } 
            } 
            // This may throw, but if it does, energies, gain coeffs,
            // and arfs will retain their original values.
            resp->applyGainFromPrompt(slope, offset);
         }
         catch (YellowAlert&)
         {
            // If anything went wrong, destroy any newly created
            // responseParam objects.
            if (resp)
            {
               resp->removeGainParams();
               tcout << " No gain fit parameters created for this response."<<std::endl;
               if (deletionOccurred)
               {
                  // Old responsePars were lost, model needs to recalculate.
                  datasets->Notify();
               }
            }
            return -1;
         }
         datasets->Notify();
      } // end gain fit

      else if (NOFIT == XSutility::lowerCase(arg)) // gain nofit [source:]spec
      {
         string paramIdStr;
         if (nArgs == 2)
         {
            // User entered just "gain nofit".  Only allow this for
            // 1 spectrum, 1 source.
            const size_t nSpec = datasets->numberOfSpectra();
            const size_t nSource = datasets->numSourcesForSpectra();
            if (nSpec > 1 || nSource > 1)
            {
               tcout << "***XSPEC Error: Multiple spectra and/or sources are available."
                    << "\n      You must enter \"gain nofit [sourceNum:]specNum\"" 
                    << " or \"gain nofit all\"" << std::endl;
               return -1;
            }
            else if (!nSpec)
            {
               tcout << "No spectra loaded:  no gain fit to remove" << std::endl;
               return -1;
            }
            else
            {
               specNum = sourceNum = 1;
            }
         }
         else 
         {
            paramIdStr = rawArgs[2];
            if (string("all") == XSutility::lowerCase(paramIdStr))
            {
               XSContainer::responses->clearGainParameters();
               tcout << " All gain parameters will be removed from next fit."
                     << "\nResponses will retain their currently applied gains." << std::endl;
               datasets->Notify();   
               return 0;
            }
         }
         try
         {
            Response* resp = HandlerUtils::getFromIntPair<Response>(paramIdStr,sourceNum,specNum);
            if (!resp)
            {
               std::ostringstream oss;
               oss << "Spectrum "<<specNum<<" has no response for source "<<sourceNum <<'\n';
               throw YellowAlert(oss.str());            
            }
            if (!resp->getLinearGain() && !resp->getConstGain())
            {
               tcout << " Response for spectrum " <<specNum<<" source "<<sourceNum
                   << " has no gain fit parameters to remove." << std::endl;
            }
            else
            {
               resp->removeGainParams();
               tcout << " Gain fit parameters for spectrum "<<specNum
                  <<" source "<<sourceNum<< " response have been removed."<<std::endl;
               datasets->Notify();   
               return 0;
            }
         }
         catch (YellowAlert&)
         {
            // Most likely response was never found.
            // Still call Notify just to be sure.
            datasets->Notify();   
            return -1;
         }
      } // end gain nofit

      else if (OFF == XSutility::lowerCase(arg))
      {
         if (nArgs > 2)
         {
            // They are possibly trying to enter "gain off" for
            // just one response.  
            tcout << " Improper usage of \"gain off.\"\n It takes no additional "
               << "parameters, and will apply to ALL loaded responses." <<std::endl;
            return -1;
         }
         const size_t nSpec = datasets->numberOfSpectra();
         if (!nSpec)
         {
            tcout << " No spectra loaded:  no gains to be removed." << std::endl;
            return -1;
         }
         ResponseMapIter itResp = responses->responseList().begin();
         ResponseMapIter itEnd = responses->responseList().end();
         while (itResp != itEnd)
         {
            itResp->second->removeGain();
            ++itResp;
         }
         datasets->Notify();
         tcout << " All applied gains have been removed." << std::endl;
         return 0;
      } // end gain off

      else
      {
         const size_t nSpec = datasets->numberOfSpectra();
         if (!nSpec)
         {
            tcout << " No spectra loaded:  unable to set gain" << std::endl;
            return -1;
         }

         StringArray args(nArgs-1,"");
         for (size_t i=1; i<nArgs; ++i)  
         {
            args[i-1] = rawArgs[i];
         }

         IntegerArray iParams(0);
         StringArray xsParams(0);
         XSparse::collectParams(args, iParams, xsParams);

         // If anything fails below, reset these to their 
         // previous values.
         int savSpecNum = specNum;
         int savSourceNum = sourceNum; 
         Real slope = 1.0;
         Real offset = 0.0;
         Real savSlope = slope;
         Real savOffset = offset;        
         try
         {
            // First verify response exists.  If user entered a value
            // for sourceNum:specNum, it must be in iParams[0].
            size_t iStart = 0;
            string paramIdStr;
            if (iParams.size() && iParams[0] == 0)
            {
               paramIdStr = xsParams[0];
               iStart = 1;
            }
            // Both spectrum and source num should be valid by this point,
            // though there still may be no actual response object in the
            // detector array.
            Response* resp = 
                     HandlerUtils::getFromIntPair<Response>(paramIdStr,sourceNum,specNum);
            if (!resp)
            {
               tcout << "Spectrum "<<specNum<<" has no response for source "<<sourceNum <<std::endl;
               specNum = savSpecNum;
               sourceNum = savSourceNum;
               return -1;
            }

            // These values must be valid.
            slope = resp->gainFactor()[1];
            offset = resp->gainFactor()[0];
            savSlope = slope;
            savOffset = offset;                        
            for (size_t i=iStart; i<iParams.size(); ++i)
            {
               string param = xsParams[i];
               switch (iParams[i])
               {
                  case 1:  
                     slope = getSlope(slope, param);
                     break;
                  case 2:  
                     offset = getOffset(offset, param);
                     break;
                  default:
                     break;
               }
            }
            // This may throw.
            resp->applyGainFromPrompt(slope, offset);
            datasets->Notify();
         }
         catch (YellowAlert&)
         {
            specNum = savSpecNum;
            sourceNum = savSourceNum;
            tcout << " Resetting current gain defaults: spectrum " << specNum 
                  << " source " << sourceNum << "\n   slope = " << savSlope
                  << "  offset = " << savOffset << std::endl;
            return -1;
         }
         tcout << " Gain applied: spectrum " << specNum << " source " << sourceNum
               << "\n   slope = " << slope << "  offset = " << offset << std::endl;
      } // end gain command line parameter input

   } // end if nArgs >= 2
   return 0;
}

namespace
{

   Real getSlope(Real current, string& param)
   {
      Real tmpSlope = .0;
      std::istringstream iss(param);
      if (!(iss >> tmpSlope) || !iss.eof())
      {
         bool isValid = false;
         string msg("Must enter valid number for slope");
         while (!isValid)
         {
            tmpSlope = current;
            isValid = XSparse::promptReal(msg, tmpSlope);
         }
      }
      return tmpSlope;
   }

   Real getOffset(Real current, string& param)
   {
      Real tmpOffset = .0;
      std::istringstream iss(param);
      if (!(iss >> tmpOffset) || !iss.eof())
      {
         bool isValid = false;
         string msg("Must enter valid number for offset");
         while (!isValid)
         {
            tmpOffset = current;
            isValid = XSparse::promptReal(msg, tmpOffset);
         }
      }
      return tmpOffset;
   }

   string getParamSubstring(const ResponseParam* respPar)
   {
      // Purpose: return all of the parameter settings in 
      // a single string format, that matches the ordering
      // expected of ModParam::init function (ie. the order
      // in model.dat files).  This means delta must be
      // moved to the back of the string.  The val setting
      // is also left out since it is dealt with seperately.
      std::ostringstream oss;
      oss << respPar->value('l') <<' '<< respPar->value('b') <<' '
          << respPar->value('t') <<' '<< respPar->value('h') <<' '
          << respPar->value('d');
      return oss.str();
   }
}
