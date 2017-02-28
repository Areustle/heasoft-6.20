#include <xsTypes.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <string>
#include <sstream>

int
XSGlobal::xsBayes(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doBayes(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doBayes(const StringArray& rawArgs)
{
   enum SUBCOMS {OFF=1, ON, CONS};
   typedef std::map<string, ModParam::PriorType> PriorTypeMap;
   const char* cmd = "bayes";
   const size_t nArgs = rawArgs.size();
   static std::map<string, int> subComs;
   static PriorTypeMap priorTypes;
   static std::map<string, int> priorNumHyperParams;
   static string defModName; // mod name only applies to mod pars.
   static size_t defParNum = 1;
   static string defPriorType("CONS");
   static bool defIsRespPar = false;
   static size_t defSourceNum = 1;  // source num only applies to resp pars.
   static RealArray defHyperParam;

   if (!subComs.size())
   {
      subComs["off"] = OFF;
      subComs["on"] = ON;
      subComs["cons"] = CONS;
   }
   if (!priorTypes.size())
   {
      priorTypes["CONS"] = ModParam::CONS;
      priorTypes["EXP"] = ModParam::EXP;
      priorTypes["JEFFREYS"] = ModParam::JEFFREYS;
      priorTypes["GAUSS"] = ModParam::GAUSS;
      priorNumHyperParams["CONS"] = 0;
      priorNumHyperParams["EXP"] = 1;
      priorNumHyperParams["JEFFREYS"] = 0;
      priorNumHyperParams["GAUSS"] = 2;
   }
   if (nArgs == 1)
   {
      XSGlobal::printDocs(cmd,"?");
      return 0;
   }

   try
   {
      bool requireUpdate = false;
      IntegerArray iParams(0);
      StringArray xsParams(0);
      std::vector<string> args(nArgs-1,"");
      for (size_t i=1; i<nArgs; ++i) 
                args[i-1] = rawArgs[i];
      XSparse::collectParams(args, iParams, xsParams);

      string firstArg;
      if (iParams[0] == 0)  firstArg = xsParams[0];
      if (firstArg == "?")
      {
         XSGlobal::printDocs(cmd,"?");
         return 0;
      }

      // First, need to determine if firstArg is a subcommand or a 
      // param specifier.  If firstArg is left blank, assume
      // user is requesting default param specifier.  
      size_t parNum = string::npos;
      string modName;
      size_t sourceNum=1;
      bool isParam = false;
      bool isRespPar = false;
      if (!firstArg.length())
      {
         parNum = defParNum;
         modName = defModName;
         sourceNum = defSourceNum;
         isRespPar = defIsRespPar;
         isParam = true;
      }
      else
      {
         // First check for any sign of a response param based on location
         // of 'r' or 'R' in firstArg.
         string::size_type rLoc = XSparse::checkForRespPar(firstArg);
         if (rLoc != string::npos)
         {
            // Assume they are attempting to find a response par.
            // Need to remove the 'r' specifier.
            string respIDStr(firstArg);
            respIDStr.erase(rLoc,1);
            int tmpSourceNum=0;
            int tmpParNum=0;
            if (XSparse::integerPair(respIDStr,tmpSourceNum,tmpParNum))
            {
               if (tmpParNum == -1)
               {
                  // Only 1 int entered, stored in tmpSourceNum.
                  parNum = static_cast<size_t>(tmpSourceNum);
                  sourceNum = 1;
               }
               else
               {
                  parNum = static_cast<size_t>(tmpParNum);
                  sourceNum = static_cast<size_t>(tmpSourceNum);
               }
               modName.clear();
               isRespPar = true;
               isParam = true;
            }
            else
            {
               string err(firstArg);
               firstArg += " is not a valid subcommand or parameter specifier.\n";
               throw YellowAlert(err);
            }
         } // end if looking for resp par
         else
         {            
            if (XSparse::stringIntPair(firstArg, modName, parNum))
            {
               isParam = true;
               isRespPar = false;
               sourceNum = 1;
            }
               // If it failed stringIntPair test, subCom is either
               // 1. 'ssss' which may be a number, or 2. 'ssss:tttt'
               // where 'tttt' is not a number.
            else if (modName.length())
            {
               // 2nd case above, must be erroneous input.
               std::ostringstream msg;
               msg << firstArg << " is not a valid subcommand or parameter specifier.\n";
               throw YellowAlert(msg.str());
            }
            else if (parNum == string::npos)
            {
               // 'ssss' not a number
               isParam = false;
            }
            else
            {
               // 'ssss' is a number
               isParam = true;
               isRespPar = false;
               sourceNum = 1;
            }
         } // end not looking for resp par    
      }

      const std::map<int,ModParam*>& varPars = XSContainer::fit->variableParameters();
      std::map<int,ModParam*>::const_iterator itVarPars = varPars.begin();
      std::map<int,ModParam*>::const_iterator itVarParsEnd = varPars.end();
      if (isParam)
      {
         // 1st arg is [<modName>:]parNum
         // Verify all args before making any changes.
         string priorType = defPriorType;
         RealArray hyperParam(defHyperParam);
         ModParam* modPar = 0;
         // verify fit model parameter
         itVarPars = varPars.begin();
         while (itVarPars != itVarParsEnd && !modPar)
         {
            if (isRespPar)
            {
               if (itVarPars->first > 
                        (Fit::RESPAR_INDEX() <<XSContainer::ModelContainer::SHIFT()))
               {
                  ResponseParam* testPar = dynamic_cast<ResponseParam*>(itVarPars->second);
                  if (parNum == testPar->index() &&
                        sourceNum == testPar->responseParent()->sourceNumber())
                  {
                     modPar = testPar;
                  }
               }
            }
            else
            {
               ModParam* testPar = itVarPars->second;
               string testName = modName.length() ? modName : Model::DEFAULT();
               if (testName == testPar->modelName() && parNum == testPar->index())
               {
                  modPar = testPar;
               }
            }
            ++itVarPars;
         }
         if (!modPar)
         {
            std::ostringstream msg;
            msg << firstArg << " is not a currently active fit parameter.\n";
            throw YellowAlert(msg.str());
         }

         // verify additional priorType and hyperParam args.
         for (size_t i=0; i<iParams.size(); ++i)
         {
            size_t iPar = iParams[i];
            if (iPar == 1)
            {
               priorType = xsParams[i];
               string upPriorType = XSutility::upperCase(priorType);
               PriorTypeMap::const_iterator itTypes = priorTypes.lower_bound(upPriorType);
               PriorTypeMap::const_iterator itTypesEnd = priorTypes.end();
               if (itTypes == itTypesEnd || 
                   itTypes->first.find(upPriorType) != 0)
               {
                  tcerr << "  " << priorType << " is an unrecognized Prior type.\n"
                     << "  Choose from the following Prior options (currently \""
                     << defPriorType << "\"):" << std::endl;
                  itTypes = priorTypes.begin();
                  while (itTypes != itTypesEnd)
                  {
                     tcerr << "  " << itTypes->first;
                     ++itTypes;
                  }
                  tcerr << std::endl;
                  return -1;
               }
               priorType = itTypes->first;
	       if ( priorNumHyperParams[priorType] != (int)hyperParam.size() )
		 hyperParam.resize(priorNumHyperParams[priorType]);
            }
            else if (iPar >= 2)
            {
               if (iPar-2 >= hyperParam.size())
               {
                  tcout << "Warning: Extra hyper parameter ignored: "
                    << xsParams[i] << std::endl;
               }
               else
               {
                  std::istringstream iss(xsParams[i]);
                  Real testPar=0.0;
                  if (!(iss >> testPar) || !iss.eof())
                  {
                     string msg("Must enter a valid floating-point number for hyper-param value.\n");
                     throw YellowAlert(msg);
                  }
                  hyperParam[iPar-2] = testPar;
               }
            }            
         }

         if (priorTypes[priorType] == ModParam::EXP &&
              hyperParam[0] <= 0.0)
         {
            string msg("EXP Prior type requires a finite positive hyper param value.\n");
            throw YellowAlert(msg);
         }
         if (priorTypes[priorType] == ModParam::GAUSS &&
              hyperParam[1] <= 0.0)
         {
            string msg("GAUSS Prior type requires a finite positive second hyper param value (i.e. sigma).\n");
            throw YellowAlert(msg);
         }

         // If it made it here, all args should be valid.

         requireUpdate = true;
         modPar->priorType(priorTypes[priorType]);
	 modPar->hyperParam(hyperParam);
         defPriorType = priorType;
         defModName = modName;
         defParNum = parNum;
         defHyperParam.resize(hyperParam.size());
	 defHyperParam = hyperParam;
         defIsRespPar = isRespPar;
         defSourceNum = sourceNum;
      }
      else
      {
         // By this point, assume 1st arg is a sub-command, not
         // a model parameter. 
         firstArg = XSutility::lowerCase(firstArg);
         int choice = -1;
         requireUpdate = true;
         std::map<string,int>::const_iterator itSubCom(subComs.lower_bound(firstArg));
         std::map<string,int>::const_iterator itSubComEnd(subComs.end());
         if (itSubCom != itSubComEnd && itSubCom->first.find(firstArg) == 0)
                   choice = itSubCom->second;
         switch (choice)
         {
            using XSContainer::fit;
            case OFF:
               fit->statManager()->setBayes(false);
               break;
            case ON:
               fit->statManager()->setBayes(true);
               break;
            case CONS:
               {
                  itVarPars = varPars.begin();
                  while (itVarPars != itVarParsEnd)
                  {
                     itVarPars->second->priorType(ModParam::CONS);
                     itVarPars->second->hyperParam(RealArray(0.0,0));
                     ++itVarPars;
                  }
                  fit->statManager()->setBayes(true);
               }
               break;
            default:
               requireUpdate = false;
               tcout << "   Subcommand " << firstArg << " does not exist" << std::endl;
               XSGlobal::printDocs(cmd,"?");
               return -1;
         }
      }
      if (requireUpdate)
         XSContainer::fit->Update();
   }
   catch (YellowAlert&)
   {
      return -1;
   }
   return 0;
}
