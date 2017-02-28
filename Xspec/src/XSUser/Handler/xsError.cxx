//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitErrorCalc.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Parse/XSparse.h>   
#include <XSUtil/Signals/SignalHandler.h>   

#include <XSContainer.h>
#include <XSstreams.h>
#include <sstream>
#include <iomanip>

namespace
{
   void reportParameter(const ModParam* par, bool isFitValid);
   void setAllErrParamStrings (Fit* fit, const IntegerArray& iParams, FitErrorCalc::ErrorCalcCodes code);
}

int
XSGlobal::xsError(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doError(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doError(const StringArray& rawArgs)
{
   using XSContainer::fit;
   const char* cmd = "error";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 2 && rawArgs[1] == "?") 
   {
           printDocs(cmd,"?");
           return 0;
   }

   const bool isRespPar = (rawArgs[0] == string("rerror"));
   const string ss = "sS";
   const string mm = "mM";
   const string nn = "nN";
   const string ir = ".Ee";


   StringArray inArgs;
   StringArray params;
   IntegerArray iParams;
   for (size_t i=1; i<nArgs; ++i)
   {
      inArgs.push_back(rawArgs[i]);
   }

   static IntegerArray savErrorParams(1,1);
   static IntegerArray savRespParams(1,1);
   static StringArray savModNames(1,string(""));
   static StringArray savRespSources(1,string(""));
   int tmpNtrial = fit->errorTry();
   Real tmpTolerance = fit->tolerance();
   Real tmpChiMax = fit->chiMax();
   Real tmpDeltaStat = fit->deltaStat();
   // doNewMinRecalc should default to 'true' for EACH call to 'error'.
   // A 'false' setting isn't saved.
   fit->doNewMinRecalc() = true;
   StringArray tmpModNames = isRespPar ? savRespSources : savModNames;
   IntegerArray tmpErrorParams = isRespPar ? savRespParams : savErrorParams;
   string prevNamedMod;

   XSparse::collectParams(inArgs, iParams, params);
   size_t nPars = iParams.size();
   bool parseError = false;
   bool stopatProcessed = false;
   bool deltaProcessed = false;
   bool parProcessed = false;
   for (size_t i=0; i<nPars && !parseError; ++i)
   {
      const string& parString = params[i];
      // qualified range here means a parameter range of form  
      // modelName:{range string}. We need to do this to allow
      // model names beginning with 's' or 'S'.
      bool isQualified = (parString.find_first_of(':') != string::npos);
      bool isStopat = (!isQualified && ss.find(parString[0]) != string::npos);
      bool isMaximum = (!isQualified && mm.find(parString[0]) != string::npos);
      bool isNoNew = (!isQualified && nn.find(parString[0]) != string::npos);
      // Rule: If 'stopat', 'maximum', or 'nonew' are specified, they must
      // appear BEFORE any and all <delta fit stat> or <mod param> specifiers.
      if (isStopat)
      {
         if (parProcessed)
         {
            tcout << "stopat option must be specified before parameters."
              << std::endl;
            parseError = true;
         }
         else
         {
            // Process stopat w/ possible pars 2 and 3
            const int stopatPos = iParams[i];
            size_t itmp = i+1;
            while (itmp < nPars && iParams[itmp] < stopatPos+3 && !parseError)
            {
               std::istringstream testIss(params[itmp]);
               if (iParams[itmp] == stopatPos+1)
               {
                  // numTrials
                  if (!(testIss >> tmpNtrial) || !testIss.eof() || tmpNtrial < 1)
                  {
                     tcout << "Improper value for nTrial parameter." << std::endl;
                     parseError = true;
                  }
               }
               else if (iParams[itmp] == stopatPos+2)
               {
                  // tolerance
                  if (!(testIss >> tmpTolerance) || !testIss.eof() || tmpTolerance <= .0)
                  {
                     tcout << "Improper value for tolerance parameter." << std::endl;
                     parseError = true;
                  }
               }
               // i will get incremented again at end of outer for loop, 
               // so it should be lagging itmp by 1 here.
               ++i;
               ++itmp;
            }
            stopatProcessed = true;
         }
      } // end if isStopat
      else if (isMaximum)
      {
         if (parProcessed)
         {
            tcout << " \"maximum\" option must be specified before parameters."
               << std::endl;
            parseError = true;
         }
         else
         {
            if (i+1 < nPars && iParams[i+1] == iParams[i]+1)
            {
               // chi max
               ++i;
               std::istringstream testIss(params[i]);
               if (!(testIss >> tmpChiMax) || !testIss.eof() || tmpChiMax <=.0)
               {
                  tcout << "Improper maximum chi-square value." << std::endl;
                  parseError = true;
               }
            }
         }
      } // end if isMaximum
      else if (isNoNew)
      {
         if (parProcessed)
         {
            tcout << " \"nonew\" option must be specified before parameters."
               << std::endl;
            parseError = true;
         }
         else
         {
            fit->doNewMinRecalc() = false;
         }
      } // end if isNoNew
      else
      {
         // Only constraint here is that delta stat (identified
         // as a Real) must appear just once, and it must 
         // appear before ANY parameter range indicators.
         size_t testInt = XSutility::isInteger(parString);
         Real testReal=.0;
         if (testInt != string::npos)
         {
            // testInt must be positive if it makes it here.
            // Assume it must be trying to give a parameter range.
            if (!parProcessed)
            {
               tmpErrorParams.clear();
               tmpModNames.clear();
               parProcessed = true;
            }
            if (testInt == 0)
            {
               tcout << "Improper parameter specifier: 0" << std::endl;
               parseError = true;
            }
            else
            {
               tmpErrorParams.push_back(static_cast<int>(testInt));
               // The idea here is to assume no-named model unless
               //   an earlier parameter from THIS call (and not those
               //   stored in savModNames from a previous call)
               //   included a name.  For example:
               //      error 1 2 alpha:3 4
               //   1 and 2 will have no model name but 4 will have 'alpha'.
               tmpModNames.push_back(prevNamedMod);
            }
         }
         else if (XSutility::isReal(parString, testReal) && !parProcessed)
         {
            if (deltaProcessed)
            {
               tcout << "Improper parameter range specifier: " 
                  << testReal << std::endl;
               parseError = true;
            }
            else if (testReal <= .0)
            {
               tcout << "Improper delta fit stat value: " << testReal
                 << std::endl;
               parseError = true;
            }
            else
            {
               tmpDeltaStat = testReal;
               deltaProcessed = true;
            }
         }
         else
         {
            // Could be <modname>:[<par>|<par range>] or just <par range>
            try
            {
               IntegerArray paramRange;
               string testModName;
               // This can throw
               XSparse::stringRangePair(parString, testModName, paramRange);
               if (!testModName.empty())
                  prevNamedMod = testModName;
               if (!parProcessed)
               {
                  tmpErrorParams.clear();
                  tmpModNames.clear();
                  parProcessed = true;
               }
               
               for (size_t j=0; j<paramRange.size(); ++j)
               {
                  if (paramRange[j] <= 0)
                  {
                     tcout << "Improper parameter specifier: " 
                        << paramRange[j] << std::endl;
                     parseError = true;
                  }
                  tmpErrorParams.push_back(paramRange[j]);
                  tmpModNames.push_back(prevNamedMod);
               }
            }
            catch (YellowAlert&)
            {
               parseError = true;
            }

         }
      } // end not a single string option
   } // end parameters for loop
   if (parseError)
   {
      isRespPar ? printDocs("rerror","?") : printDocs("error","?");
      return -1;           
   }
   else
   {
      fit->errorTry() = tmpNtrial;
      fit->tolerance() = tmpTolerance;
      fit->chiMax() = tmpChiMax;
      fit->deltaStat() = tmpDeltaStat;
      if (isRespPar)
      {
         savRespSources = tmpModNames;
         savRespParams = tmpErrorParams;
      }
      else
      {
         savModNames = tmpModNames;
         savErrorParams = tmpErrorParams; 
      }  
   }

   // Parsing syntax is validated by this point, though still not
   // home free.  Check if parameters are valid.
   const size_t nSpecifiedPar = isRespPar ? savRespParams.size() 
                        : savErrorParams.size();
   // These two arrays will contain a subset of savErrorParams.
   // Errors from chains need just the short par index nums (which is
   // what the user sees), while fit errors can perform easier 
   // lookup with the full parameter index.
   IntegerArray fullErrorParams;
   IntegerArray shortErrorParams;

   const bool useChains = fit->chainManager()->isSynchedWithFitParams();

   bool anyMissing = false;
   for (size_t i=0; i<nSpecifiedPar; ++i)
   {
      Parameter* testPar=0;
      int parNum=0;
      string nameString;
      if (isRespPar)
      {
         parNum = savRespParams[i];
         std::ostringstream oss;
         if (savRespSources[i].length())
            oss << savRespSources[i] << ":" << parNum;
         else
            oss << "1:" << parNum;
         nameString = oss.str();
         int dummySource=1;
         int dummyIdx=1;
         testPar = HandlerUtils::getFromIntPair<ResponseParam>
                        (nameString,dummySource,dummyIdx); 
      }
      else
      {
         parNum = savErrorParams[i];
         std::ostringstream oss;
         if (savModNames[i].length())
            oss << savModNames[i] << ":" << parNum;
         else
            oss << parNum;
         nameString = oss.str();
         testPar = 
              XSContainer::models->lookupParameter(parNum,savModNames[i]);
      }   

      if (testPar)
      {
         ModParam* modPar = dynamic_cast<ModParam*>(testPar);
         if (modPar)
         {
            // If ANYTHING should prevent a proper error calculation for
            // a particular bound on this parameter from this point on, 
            // this ensures that its stored error value will be 0.
            modPar->setValue(0.0, 'p');
            modPar->setValue(0.0, 'm');

            std::map<int,ModParam*>::const_iterator vp =
                 fit->variableParameters().begin();
            std::map<int,ModParam*>::const_iterator vpEnd =
                 fit->variableParameters().end(); 
            while (vp != vpEnd && modPar != vp->second)
            {
               ++vp;
            }
            if (vp == vpEnd)
            {
               tcout << "*** Parameter " << nameString
                  << " is not a variable model parameter and no confidence range will be calculated." 
                  << std::endl;
               modPar->lastErrorStatus(FitErrorCalc::errCodeToString(FitErrorCalc::FROZEPAR));
            }
            else
            {
               fullErrorParams.push_back(vp->first);
               shortErrorParams.push_back(parNum);
            }           
         }
         else
         {
            // Not a ModParam - Switch or Scale
            string parType = isRespPar ? string("response") : string("model");
            tcout << "*** Parameter " << nameString
               << " is not a variable "<<parType<<" parameter and no confidence range will be calculated."
               << std::endl;
         }
      }
      else
      {
         tcout << "***Error:  Unable to locate parameter " << nameString;
         if (!isRespPar)
            tcout << "\nIf parameter belongs to a named model,"
              << " make sure model name is specified.";
         tcout << std::endl;
         anyMissing = true;
      }
   } // end pars loop
   if (anyMissing)
         return -1;

   // Now checking for fit validity much later.  This is done after
   // pars are verified so we can easily set their status strings.
   if (!useChains && !fit->isStillValid())
   {
      tcout << "A valid fit is first required in order to run error command."
           <<std::endl;
      setAllErrParamStrings(fit, fullErrorParams, FitErrorCalc::GENPROB);
      return -1;           
   }


   SIGINT_Handler intHandler;
   EventHandler* oldHandler = 0;

   std::ios_base::fmtflags save (tcout.flags());
   try 
   {
      if (useChains)
      {
         oldHandler = SignalHandler::instance()->registerHandler(
                         SIGINT, &intHandler);
         tcout <<"Errors calculated from chains\n"     
           << " Parameter   Confidence Range (" << fit->deltaStat() << ")"
           << std::endl;
         for (size_t i=0; i<shortErrorParams.size(); ++i)
         {
            // This can throw
            std::pair<Real,Real> range = 
                fit->chainManager()->getParErrorRange(fit->deltaStat(),
                        shortErrorParams[i], savModNames[i]);
            // This has already been verified in loop above:
            ModParam* modPar = 
               fit->variableParameters().find(fullErrorParams[i])->second;
            modPar->setValue(range.first,'m');
            modPar->setValue(range.second,'p');
            modPar->lastErrorStatus(FitErrorCalc::errCodeToString(FitErrorCalc::OK));
            reportParameter(modPar, fit->isStillValid());
         }
         SignalHandler::instance()->registerHandler(SIGINT,oldHandler);
      }
      else
      {
         fit->reinitialize(true);

         // check fit is not overdetermined (more variable
         // parameters than data bins.
         const StatManager* stats = fit->statManager();         
         std::pair<int,size_t> dofVals = stats->totalDegreesOfFreedom();

         if (dofVals.first <= 0 ) 
         {
            setAllErrParamStrings(fit, fullErrorParams, FitErrorCalc::GENPROB);
            throw Fit::FitOverDetermined();       
         }
         const StatMethod* singleStat = stats->usingSingleStat();
         if (singleStat && singleStat->name() == "chi")
         {         
            Real redChi = singleStat->statistic()/dofVals.first;
            if (redChi > fit->chiMax())
            {
               tcout << "Cannot do error calc: Reduced Chi^2 (= " << redChi << ") > maximum (" 
                     << fit->chiMax() << ")"<< std::endl; 
               setAllErrParamStrings(fit, fullErrorParams,
                         FitErrorCalc::TOOLARGE);
               return -1;
            }
         }
         fit->errorCalc(true);

         tcout << " Parameter   Confidence Range (" << fit->deltaStat() << ")\n";
         tcout <<std::flush;

         oldHandler = SignalHandler::instance()->registerHandler(
                         SIGINT, &intHandler);
         fit->getErrors(fullErrorParams);
         SignalHandler::instance()->registerHandler(SIGINT,oldHandler);
      }
   }
   catch (YellowAlert&)
   {
      if (useChains)
      {
         // If something threw by this point using chain errors,
         // can assume the problem exists for all parameters.
         setAllErrParamStrings(fit, fullErrorParams, FitErrorCalc::GENPROB);
      }
      if (oldHandler)
      {
         SignalHandler::instance()->registerHandler(SIGINT,oldHandler);
      }
      tcout.flags(save); 
      fit->errorCalc(false);
      return -1;
   }
   tcout << std::flush;
   tcout.flags(save);        
   fit->errorCalc(false);
   return 0;       
}

namespace
{
   void reportParameter(const ModParam* par, bool isFitValid)
   {
      // This is very similar to FitErrorCalc::reportParameter,
      // which is not accessible when getting errors from chains.
      using std::setw;

      std::streamsize savePrec(tcout.precision());
      std::ios_base::fmtflags save(tcout.flags());
      tcout.precision(6);
      tcout << setw(6) << par->index() << setw(13) << par->value('m')
            << setw(13) << par->value('p');
      if (isFitValid)
      {      
         tcout << "    (" << par->value('m') - par->value('a') << ","
	    << par->value('p') - par->value('a') << ")";
      }
      tcout << std::endl;
      tcout.flags(save);
      tcout.precision(savePrec);
   }

   void setAllErrParamStrings (Fit* fit, const IntegerArray& iParams, FitErrorCalc::ErrorCalcCodes code)
   {
     // This function ASSUMES all pars in iParams have been verified to exist
     // in fit's m_variableParameters.  If they don't, there's gonna be trouble.
     for (size_t i=0; i<iParams.size(); ++i)
     {
        ModParam* modPar = fit->variableParameters(iParams[i]);
        if (!modPar)
        {
           throw RedAlert("Missing fit parameter in FitErrorCalc::setAllErrParamStrings.");
        }
        modPar->lastErrorStatus(FitErrorCalc::errCodeToString(code));
     }  
   }

}
