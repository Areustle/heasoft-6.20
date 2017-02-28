//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ModelTypes.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <sstream>    

int
XSGlobal::xsEqwidth(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doEqwidth(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doEqwidth(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "eqwidth";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1 || rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
      return 0;
   } 
   if (!models->modelSet().size())
   {
      tcout << "\n   Cannot use eqwidth command:  No models loaded." << std::endl;
      return 0;
   }

   IntegerArray iParams(0);
   StringArray xsParams(0);
   std::vector<string> args(nArgs,"");
   for (size_t i=1; i<nArgs; ++i)
   {
      args[i-1] = rawArgs[i];
   }
   XSparse::collectParams(args, iParams, xsParams);

   enum entryType {startNull, rangeSpec, floatVal, intVal, errSpec, stringInt} 
                        previous = startNull, current = startNull;

   size_t nPars = iParams.size();
   size_t iPar = 0;
   size_t parsAfterError = 2;
   size_t errNumber = 0;
   size_t compNum = 0;
   Real errLevel = .0;
   Real realInput = .0;
   bool errorIsParsed = false;
   bool waitingForCompNum = false;
   static Real s_fracDef = .05;
   string modelName("");
   std::vector<EqWidthRecord> components;
   EqWidthRecord currComp;
   currComp.fraction = s_fracDef;

   try
   {
      while (iPar < nPars && parsAfterError)
      {
         // What type of parameter are we dealing with?
         if (XSutility::lowerCase(xsParams[iPar]) == "range")
         {
            current = rangeSpec;
         }
         else if (XSutility::lowerCase(xsParams[iPar]) == "err" ||
                  XSutility::lowerCase(xsParams[iPar]) == "noerr")
         {
            current = errSpec;
         }
         else if (XSutility::isInteger(xsParams[iPar]) != std::string::npos)
         {
            current = intVal;
         }
         else if (XSutility::isReal(xsParams[iPar], realInput))
         {
            if (realInput < 0)
            {
               throw XSparse::InputError("Improper floating point input");
            }
            current = floatVal;
         }
         else if (XSparse::stringIntPair(xsParams[iPar], modelName, compNum))
         {
            current = stringInt;
         }
         else
         {
            throw XSparse::InputError(xsParams[iPar]);
         }

         switch (current)
         {
            case rangeSpec:
               if (errorIsParsed || (previous != startNull && previous != intVal  && 
                        previous != stringInt)) 
               {
                  throw XSparse::InputError("Improper position of range specifier.");
               }
               break;
            case errSpec:
               if (errorIsParsed || (previous != intVal && previous != stringInt))
               {
                  throw XSparse::InputError("Improper position of error specifier");
               }
               if (previous == stringInt)
               {
                  components.push_back(currComp);
               }
               parsAfterError = (XSutility::lowerCase(xsParams[iPar]) == "err") ?
                                2 : 0;
               errorIsParsed = true;              
               break;
            case intVal:
               {
                  int intInput;
                  std::istringstream tmp(xsParams[iPar]);
                  tmp >> intInput;
                  if (intInput < 0)
                  {
                     // No use for negative integers in this function.
                     throw XSparse::InputError("Improper integer input.");
                  }
                  if (previous == rangeSpec)
                  {
                     if (intInput > 1)
                     {
                        throw XSparse::InputError("Improper energy range specifier.");
                     }
                     currComp.fraction = s_fracDef = static_cast<Real>(intInput);
                     current = floatVal;
                  }
                  else if (errorIsParsed)
                  {
                     if (previous == errSpec)
                     {
                        errNumber = intInput;
                     }
                     else
                     {
                        errLevel = static_cast<Real>(intInput);
                        if (errLevel < .0 || errLevel > 100.0)
                        {
                           throw XSparse::InputError("Improper error range specifier.");
                        }
                        current = floatVal;
                     }
                     --parsAfterError;
                  }
                  else
                  {
                     // previous == startNull, stringInt, intVal, or 
                     // floatVal, which means this refers to a default 
                     // model component.
                     currComp.modelName = Model::DEFAULT();
                     currComp.compNumber = static_cast<size_t>(intInput);
                     waitingForCompNum = false;
                     components.push_back(currComp);
                  }
               }
               break;
            case floatVal:
               if ((previous != rangeSpec && !errorIsParsed) || 
                        previous == errSpec)
               {
                  throw XSparse::InputError("Improper floating point input.");
               }
               if (previous == rangeSpec)
               {
                  currComp.fraction = s_fracDef = realInput;
                  waitingForCompNum = true;
               }
               else 
               {
                  errLevel = realInput;
                  if (errLevel < .0 || errLevel > 100.0)
                  {
                     throw XSparse::InputError("Improper error range specifier.");
                  }
                  --parsAfterError;
               }
               break;
            case stringInt:
               if (errorIsParsed || previous == rangeSpec)
               {
                  throw XSparse::InputError("Improper placement of \"name:component\"");
               }
               waitingForCompNum = false;
               if (!modelName.length())
               {
                  modelName = Model::DEFAULT();
               }
               currComp.modelName = modelName;
               currComp.compNumber = compNum;
               components.push_back(currComp);
               break;
            default:
               break;
         }
         previous = current;
         ++iPar;
      }
      if (waitingForCompNum)
      {
         throw XSparse::InputError("Missing model:component specifier.");
      }
      if (errorIsParsed && parsAfterError)
      {
         tcout << "\n***Warning: \"error\" specifier missing 1 or more parameters."
                <<"\n   No errors will be estimated." << std::endl;
         errNumber = 0;
         errLevel = .0;
      }
   }
   catch (YellowAlert&)
   {
      return -1;
   }

   // The input parameters have been verified, proceed with the
   // actual task.
   for (size_t i=0; i<components.size(); ++i)
   {
      const EqWidthRecord& currRec = components[i];
      std::vector<Model*> currMods = models->lookupModelGroup(currRec.modelName);
      string reportName = (currRec.modelName == Model::DEFAULT()) ?
                                string("(unnamed)") : currRec.modelName;
      if (!currMods.size())
      {
         tcout << "\nModel named: " << reportName << "   not found." << std::endl;
         break;
      }
      // If first model is active, all are.
      bool modelIsActive = currMods[0]->isActive();
      if (currRec.compNumber > currMods[0]->numberOfComponents())
      {
         tcout << "No component " << currRec.compNumber << " in model: "
             << reportName << std::endl;
         break;
      }

      try
      {
         models->clearSources();
         models->makeSourceComponents(currMods);
      
         // Loop over data groups
         for (size_t j=0; j<currMods.size(); ++j)
         {
            Model* mod = currMods[j];
            models->calcEqWidths(currRec, mod);
            if (errNumber)
            {
               if (modelIsActive)
               {
                  if (j==0)
                  {
                     fit->reportErrorSimulationMethod();
                  }
                  fit->calcEqErrors(currRec, currMods, j, errNumber, errLevel);
               }
               else
                  tcout << "Cannot calculate eqwidth error for a model without"
                    <<"\n     a suitable response matrix."<<std::endl;
            }
         }
      }
      catch  (YellowAlert&)
      {
         return -1;
      }
   }
   return 0;
}
