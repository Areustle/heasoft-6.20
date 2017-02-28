//
//  XSPEC12  November 2003
//
//

#include <XSContainer.h>
#include <XSstreams.h>        
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Model/EmissionLines/LineList.h>
#include <XSPlot/Plot/PlotDirector.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <sstream>

int
XSGlobal::xsIdentify(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doIdentify(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doIdentify(const StringArray& rawArgs)
{
   const char* cmd = "identify";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 2 && rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
      return 0;
   }

   const size_t nMaxParams = 6;
   static Real energy = 1.0;
   static Real delta = .01;
   static Real redshift = 0.0;
   static string lineListName("apec");
   static Real tPlasma = 1.0;
   static Real emissMin = 1.0e-19;
   LineList *lineList = 0;

   IntegerArray iParams(0);
   StringArray xsParams(0);
   StringArray args(nArgs,"");
   for (size_t i=1; i<nArgs; ++i)
   {
      args[i-1] = rawArgs[i];
   }
   XSparse::collectParams(args, iParams, xsParams);

   try
   {
      for (size_t i=0; i<iParams.size() && i<nMaxParams; ++i)
      {
         size_t iPar = iParams[i];
         if (iPar >= nMaxParams)
         {
            break;
         }
         Real tmp;
         string tmpString;
         std::istringstream iss(xsParams[i]);
         bool isOk = true;
         switch (iPar)
         {
            case 0:  // energy
               if (!(iss >> tmp) || !iss.eof() || tmp < .0)
               {
                  isOk = false;
                  while (!isOk)
                  {
                     tmp = energy;
                     string msg("Improper energy input.  Line energy (keV):");
                     isOk = XSparse::promptReal(msg, tmp);
                     if (isOk && tmp < .0)
                     {
                        isOk = false;
                     }
                  }
               }
               energy = tmp;
               break;
            case 1:  // delta
               if (!(iss >> tmp) || !iss.eof() || tmp < .0)
               {
                  isOk = false;
                  while (!isOk)
                  {
                     tmp = delta;
                     string msg("Improper delta energy input.  Delta energy (keV):");
                     isOk = XSparse::promptReal(msg, tmp);
                     if (isOk && tmp < .0)
                     {
                        isOk = false;
                     }
                  }
               }
               delta = tmp;
               break;
            case 2:  // redshift
               if (!(iss >> tmp) || !iss.eof())
               {
                  isOk = false;
                  while (!isOk)
                  {
                     tmp = redshift;
                     string msg("Improper redshift input.  Redshift:");
                     isOk = XSparse::promptReal(msg, tmp);
                  }
               }
               redshift = tmp;
               break;
            case 3:  // line list
               tmpString = XSutility::lowerCase(xsParams[i]);
               isOk = false;
               while (!isOk)
               {
                  if (LineList::lineLists().find(tmpString) == 
                        LineList::lineLists().end())
                  {
                     string prompt("Improper line list name. Line List (");
                     prompt += lineListName + "): ";
                     XSparse::basicPrompt(prompt, tmpString);
                     if (!tmpString.length() ||
                          tmpString.substr(0,1) == XSparse::USE_DEFAULT()) 
                     {
                        tmpString = lineListName;
                     }
                     else
                     {
                        tmpString = XSutility::lowerCase(tmpString);
                     }
                  }
                  else
                  {
                     isOk = true;
                  }
               }
               lineListName = tmpString;
               break;
            case 4:  // plasma temperature
               if (!(iss >> tmp) || !iss.eof() || tmp < .0)
               {
                  isOk = false;
                  while (!isOk)
                  {
                     tmp = tPlasma;
                     string msg("Improper plasma temp input.  Plasma Temperature (keV):");
                     isOk = XSparse::promptReal(msg, tmp);
                     if (isOk && tmp < .0)
                     {
                        isOk = false;
                     }
                  }
               }
               tPlasma = tmp;
               break;
            case 5:  // min emissivity
               if (!(iss >> tmp) || !iss.eof() || tmp < .0)
               {
                  isOk = false;
                  while (!isOk)
                  {
                     tmp = emissMin;
                     string msg("Improper min emissivity input.  Minimum Emissivity:");
                     isOk = XSparse::promptReal(msg, tmp);
                     if (isOk && tmp < .0)
                     {
                        isOk = false;
                     }
                  }
               }
               emissMin = tmp;
               break;
            default:
               break;
         }
      }
      // Having made it this far, we can assume all inputs are valid.
      bool isWave = (XSContainer::plot->setplot().xOption() == WAVELENGTH);
      Real lowE = .0;
      Real highE = .0;
      if (isWave)
      {
         lowE = (energy - delta)/(1.0 + redshift);
         highE = (energy + delta)/(1.0 + redshift);
      }
      else
      {
         lowE = (energy - delta)*(1.0 + redshift);
         highE = (energy + delta)*(1.0 + redshift);
      }
      lineList = LineList::get(lineListName);
      if (lineList)
      {
         lineList->initialize(lowE, highE, tPlasma, emissMin, isWave);
         // This could throw:
         lineList->showList(tcout);
      }   
   }
   catch (YellowAlert&)
   {
      delete lineList;
      return -1;
   }
   delete lineList;
   return 0;
}
