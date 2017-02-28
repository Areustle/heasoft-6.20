//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>  
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSUtil/Parse/XSparse.h> 
#include <XSUtil/Signals/SignalHandler.h>    
#include <XSUtil/Utils/XSutility.h> 
#include <XSstreams.h>
#include <XSContainer.h>
#include <xsTypes.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSRegEx.h>
#include <XSUser/UserInterface/TclRegEx.h>
#include <sstream>
#include <iomanip>

int
XSGlobal::xsSteppar(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doSteppar(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doSteppar(const StringArray& rawArgs)
{
    const char* cmd = "steppar";
    const size_t nArgs = rawArgs.size();
    const bool SAVE(true);
    using XSContainer::fit;
    using XSContainer::models;
    using HandlerUtils::RegEx;

    Step* stepData = fit->stepGrid();

    // If there are no arguments and previous parameter settings exist, use them.
    static Step::ParameterSpec* prevSettings = 0;
    const bool usePreviousSettings (nArgs == 1 && prevSettings);


    if (nArgs == 1 && !prevSettings)
    {
	tcout << "No parameters yet specified: summary of command:\n";
	printDocs(cmd,"?");
	return -1;        
    }

    // ensure that all the fit parameter structures are up to date
    // and the statistic is current. Also, throw if there's anything wrong
    // with the model setup ( no data, no model, no degrees of freedom ).

    try
    {
	if (!stepData) 
	{
	    fit->stepGrid(new Step(fit));
	    stepData = fit->stepGrid();
	}
	stepData->bestFit(fit->statistic());

	fit->reinitialize(SAVE);

	// renormalize and recompute statistic from current models and data.
	fit->initializeStatistic();        
    }
    catch ( YellowAlert& )
    {
	return -1;       
    }

    Step::SpecContainer newStepSettings; 

    Fit::querySetting qmode(fit->queryMode());
    int con (tpout.consoleChatterLevel());
    int log (tpout.logChatterLevel());

    SIGINT_Handler intHandler;
    EventHandler* oldHandler = 0;
    bool resetVerbose=false;
    try
    {  
	if (nArgs >= 2)
	{
	    string arg(rawArgs[1]);
	    if (arg[0] == '?')
	    {
		printDocs(cmd,"?");
		return 0;              
	    }
	    else
	    {
               bool isBest = false;
               HandlerUtils::commonStepParsing(rawArgs, prevSettings, 
                        newStepSettings, isBest);
               if (!newStepSettings.size())
               {
                  return -1;
               }		
               stepData->best(isBest);
	    }
	}

	if (usePreviousSettings)
        {
           // While syntax and ranges must remain valid, the original
           // ModParam object may have been removed or replaced. Since
           // ParameterSpec object stores a pointer to its ModParam,
           // its crucial that it gets updated before doGrid().

           // Reconstruct the user-entered par ID string from what is
           // stored in ParameterSpec.
           const string& storedFullName = prevSettings->name;
           string modName;
           const string::size_type colonPos = storedFullName.find(':');
           // Want to include the ':' in the modName.
           if (colonPos != string::npos)
              modName = storedFullName.substr(0, colonPos+1);
           const int parIdx = prevSettings->parIndex;
           std::ostringstream oss;
           oss << modName;
           if (prevSettings->fullFitIndex > (Fit::RESPAR_INDEX() <<
                        XSContainer::ModelContainer::SHIFT()))
              oss << 'r';
           oss << parIdx;
           string parIDstring(oss.str());

           ModParam* par=0;
           // Note that full parameter index may also have changed.
           int fullIdx=0;
           string dummyName;
           int dummyParIdx=-1;
           // This throws if parIDstring no longer matches an existing parameter.
           Step::retrieveParameter(fit, parIDstring, par, dummyParIdx, fullIdx, dummyName);
           prevSettings->fullFitIndex = fullIdx;
           prevSettings->address = par;
        }
        else
           stepData->reinitialize(newStepSettings);
	fit->queryMode(Fit::YES);
	fit->errorCalc(true);
        
        tpout.setVerbose(con, log);
        resetVerbose=true;

	// change interrupt handling to allow interrupts during long command
	// run.
        oldHandler = SignalHandler::instance()->registerHandler(SIGINT,
								&intHandler);
	stepData->doGrid();
        SignalHandler::instance()->registerHandler(SIGINT,oldHandler);       
	fit->errorCalc(false);

        tpout.setVerbose();
        resetVerbose=false;
	fit->queryMode(qmode);

        // A new found min is relevant only if the fit was in a valid
        // state to begin with.  Note that the doGrid call would not have
        // changed the isStillValid state.
        if (fit->isStillValid())
        {
           const Real origFit = stepData->bestFit();
           const Real minFound = stepData->minStatFound();
           const Real deltaCrit = fit->fitMethod()->deltaCrit();
           if ((origFit - minFound) >= deltaCrit) 
           {
              // New min found
              bool doReplace = false;
              if (qmode == Fit::ON)
              {
                 string prompt("\nA new best fit was found during steppar which exceeds the fit critical delta.");
                 prompt += "\nUpdate parameters with new best fit values? ";
                 if (XSutility::yesToQuestion(prompt, 0, tcin) > 0)
                    doReplace = true;
              }
              if (doReplace || qmode == Fit::YES)
              {
                 // minStatParams map keys should match EXACTLY with
                 // Fit's variableParameters.
                 if (stepData->minStatParams().size() != fit->variableParameters().size())
                    throw RedAlert("Variable parameters map size mismatch in xsSteppar.cxx");
                 std::map<int,Real>::const_iterator itVal = stepData->minStatParams().begin();
                 std::map<int,Real>::const_iterator itValEnd = stepData->minStatParams().end();
                 while (itVal != itValEnd)
                 {
                    ModParam* varPar = fit->variableParameters(itVal->first);
                    if (!varPar)
                       throw RedAlert("Variable parameter mismatch in xsSteppar.cxx");
                    varPar->setValue(itVal->second,'v');

                    ++itVal;
                 }

                 if (qmode == Fit::YES)
                    tcout << "\nA new best fit was found during steppar."
                      << "\nParameters have been updated to the new best fit values.\n"
                      << std::endl;

                 fit->calculateModel();
                 fit->initializeStatistic();
                 fit->perform();
                 fit->report();
                 stepData->bestFit(fit->statistic());
                 // This is needed to ensure 'plot contour' places
                 //  places the cross-hairs in the corrected best fit position.
                 Step::SpecContainer::const_iterator itParSpec = stepData->getParameter().begin();
                 Step::SpecContainer::const_iterator itParSpecEnd = stepData->getParameter().end();
                 while (itParSpec != itParSpecEnd)
                 {
                    (*itParSpec)->value = fit->variableParameters((*itParSpec)->fullFitIndex)->value('v');
                    ++itParSpec;
                 }
                  
                
              }
           } // end if new min found
        } // end if started with valid fit

	return 0;
    }
    catch ( ... )
    {
        if (oldHandler)
        {
	    SignalHandler::instance()->registerHandler(SIGINT,oldHandler);
        }
	fit->errorCalc(false);
	fit->queryMode(qmode);
        if (resetVerbose)
            tpout << xsverbose();
	return -1;       
    }

}
