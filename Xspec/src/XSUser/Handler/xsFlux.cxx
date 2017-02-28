//
//  XSPEC12  November 2003
//
//

#include <XSFit/Fit/Fit.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsFlux(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   string cmd = string(static_cast<char*>(cdata));

   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doFlux(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doFlux(const StringArray& rawArgs)
{
        using XSContainer::fit;

        // err to be estimated or no.
        static bool errorCalc (false);
        static Real  realArgs[3] = {2.,10.,0.};
        static int nRealize (100); // do 100 realizations for err option by default
        static Real confidenceLevel(68.); // 1-sigma confidence level by default

        const size_t nArgs = rawArgs.size();
        const bool isFlux = rawArgs[0] == "flux";
        // we have enough defaults for the command to run without arguments,
        // with the exception of lumin with redshift, which needs the ranges
        // and the redshift s
        if (nArgs == 2 && rawArgs[1] == "?") 
        {
                printDocs(rawArgs[0].c_str(),"?");
                return 0;
        }

        int errPos(-1);
        StringArray args(nArgs-1);
        StringArray params(0);
        IntegerArray iParams(0);
        for (size_t i=1; i<nArgs; ++i)
        {
           args[i-1] = rawArgs[i];
        }
        XSparse::collectParams(args, iParams, params);
        size_t nP = iParams.size();
        for (size_t i=0; i<nP; ++i)
        {
           const string rootMsg("Unable to parse a number from ");
           if (errPos < 0)
           {
              string errCheck = XSutility::lowerCase(params[i]);
              if (errCheck.substr(0,3) == "err")
              {
                 errPos = iParams[i];
                 errorCalc = true;
                 continue;
              }
              else if (errCheck.substr(0,2) == "no")
              {
                 errPos = iParams[i];
                 errorCalc = false;
                 continue;
              }

              // Max of 3 args (which should be floats) prior to err/noerr 
              // are handled, the rest are ignored. 
              if (iParams[i] < 3)
              {
                 std::istringstream iss(params[i]); 
                 Real tmp=.0;
                 bool isOk = true;
                 if (!(iss >> tmp) || !iss.eof())
                 {
                    isOk = false;
                    while (!isOk)
                    {
                       string msg(rootMsg);
                       msg += '\"' + params[i] + '\"';
                       switch (iParams[i])
                       {
                          case 0:  // eLow
                             msg += " for Lower Energy (keV):";
                             tmp = realArgs[0];
                             break;
                          case 1:  // eHigh
                             msg += " for Upper Energy (keV):";
                             tmp = realArgs[1];
                             break;
                          case 2:  // redshift
                             msg += " for redshift:";
                             tmp = realArgs[2];
                             break;
                          default:
                             break;                                        
                       }
                       isOk = XSparse::promptReal(msg, tmp);
                    }
                 }
                 realArgs[iParams[i]] = tmp;
              } // end if iParams[i] < 3
              else
              {
                 tcout << "Warning: Parameter \"" << params[i] 
                    << "\" is ignored" << std::endl;
              }
           } // end if !errPos
           else
           {
              int offset = iParams[i] - (errPos + 1);
              // Max of 2 args (int, float) after err/noerr,
              // ignore the rest.
              if (offset == 0)
              {
                 int tmp = static_cast<int>(XSutility::isInteger(params[i]));
                 while (tmp < 0)
                 {
                    std::ostringstream os;
                    os << "Unable to parse an integer from "
                       << "\"" << params[i] << "\" for no. of realizations: ("
                       << nRealize << ")";
                    XSparse::basicPrompt(os.str(), params[i]);
                    if (!params[i].length() || params[i].substr(0,1) ==
                                XSparse::USE_DEFAULT())
                    {
                       tmp = nRealize;
                    }
                    else 
                    {
                       tmp = static_cast<int>(XSutility::isInteger(params[i]));
                    }
                 }
                 nRealize = tmp;                 
              }
              else if (offset == 1)
              {
                 string msg("Unable to parse a number from ");
                 std::istringstream iss(params[i]); 
                 Real tmp=.0;
                 bool isOk = true;
                 if (!(iss >> tmp) || !iss.eof())
                 {
                    isOk = false;
                    while (!isOk)
                    {
                       msg += '\"' + params[i] + '\"';
                       msg += " for confidence level:";
                       tmp = confidenceLevel;
                       isOk = XSparse::promptReal(msg, tmp);
                    }
                 }
                 confidenceLevel = tmp;
              }
              else
              {
                 tcout << "Warning: Parameter \"" << params[i] 
                    << "\" is ignored" << std::endl;
              }
           } // end if errPos
        } // end input parameters loop



        static Real& eMin = realArgs[0]; // default energy range 2-10 keV
        static Real& eMax = realArgs[1];
        static Real& redshift = realArgs[2]; // ignore if the command was "flux".

        Real zfactor (1.);

        if (!isFlux)
        {
                // of course redshift < 0 is nonsense, but this
                // simple test will remove  possible inadvertent user error.
                if ( redshift <= 0.)
                {
                        tcerr << "redshift must > 0 to compute luminosity ";
                        return -1;       
                }        
                zfactor += redshift;
        }

        // swap limits if necessary
        if ( eMin > eMax)
        {
                Real __tmp (eMin);
                eMin = eMax;
                eMax = __tmp;        
        }

        Real energyLow  ( eMin / zfactor );
        Real energyHigh ( eMax / zfactor );

        if (errorCalc)
        {
           if (!fit->isStillValid())
           {
              tcout << "Warning:  Fit is not currently valid, therefore no " 
                  << rawArgs[0] << " error will be calculated." << std::endl;
           }
           tcout << xsverbose(15)<<"Number of realizations used:  " << nRealize
                <<"\nConfidence level (percent):  " << confidenceLevel
                << std::endl << xsverbose();
        }

        try
        {
           fit->fluxes(!isFlux, errorCalc, nRealize, confidenceLevel, energyLow, energyHigh, redshift);
        }
        catch (YellowAlert&)
        {
           return -1;
        }

        tcout << std::flush;
        return 0;
}
