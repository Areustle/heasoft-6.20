//
//  XSPEC12  November 2003
//
//

// location for functions that are in namespace XSGlobal but not part
// of the Global class.

#include <XSUser/Global/Global.h>
#include <XSUser/Help/HelpComposite.h>
#include <XSUser/UserInterface/TclRedAlert.h>
#include <XSPlot/Plot/PlotCommand.h>
#include <XSPlot/Plot/PlotCommandCreator.h>
#include <XSPlot/Plot/PlotDirector.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/FitMethod/LevMarq/LevMarq.h>
#include <XSFit/FitMethod/Minuit/Minuit.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSFit/Randomizer/Randomizer.h>
#include <XSFit/Randomizer/RandomizerPolicies.h>
#include <XSFit/StatMethod/ChiSquare/ChiSquare.h>
#include <XSFit/StatMethod/ChiSquare/PearsonChiSquare.h>
#include <XSFit/StatMethod/Cstat/Cstat.h>
#include <XSFit/StatMethod/Cstat/CstatVariants.h>
#include <XSFit/StatMethod/EDF/EDF.h>
#include <XSFit/StatMethod/EDF/EDFVariants.h>
#include <XSFit/StatMethod/Runs/Runs.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/Weight.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Component/TableComponent.h>
#include <XSModel/Model/Component/OGIPTable/OGIPTable.h>
#include <XSModel/Model/EmissionLines/LineList.h>
#include <XSModel/Model/EmissionLines/Apec.h>
#include <XSModel/Model/EmissionLines/Bearden.h>
#include <XSModel/Model/EmissionLines/Mekal.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/Parameter/ParamLinkList.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/Detector/DummyResponse.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/MathExpression.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSFunctions/functionMap.h>
#include <Handler/HandlerUtils.h>
#include <XSGlobal.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <iostream>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>    
#include <cassert>
#include <memory>
#include <cstring>

extern "C" {
#ifdef GUI_SUPPORT
  #include <pow.h>
  #include <itk.h>
#endif
}

namespace xstcl
{
   // container definitions for the command set and their
   // one-line help summaries
   char xsLogOut[] = "LogOut";
   char xsLogErr[] = "ErrOut";
   int  xs_echo_script = 0;
   char xsTclPrompt[] = "xs_tcl_prompt";
   char* xs_tcl_prompt = 0;
   Tcl_Trace XS_Echo_Trace = 0;
}

namespace XSGlobal
{
    GlobalData* globalData = 0;
    std::map <string,Tcl_ObjCmdProc*> commandMap;
    std::map <string,string> summaryMap;
    std::map <string,string> pdfMap;        //good place for this?

    void nonTclAppInit();
}

bool
XSGlobal::printDocs(const char* command, const char* firstArgument)
{  
        bool done = false;
        if (!firstArgument || (strlen(firstArgument) == 1 && *firstArgument == '?') )
        {
                // print out summary information on blank or '?'
                tcout << summaryMap[command] << std::endl;
                done = true;
        }        
        return done;   
}

void 
XSGlobal::registerNativeFitMethods()
{
   FitMethod::registerMethod("leven",new LevMarq("10 0.01"));        
   summaryMap["method"] =  refreshFitMethodNames();
        
   Minuit* minuitObj = new Minuit("500 0.01");
   // This passes ownership of minuitObj to FitMethod::s_fitMethodObjs
   FitMethod::registerMethod("migrad",minuitObj);
   FitMethod::registerMethod("simplex",minuitObj);
   XSGlobal::summaryMap["method"] = XSGlobal::refreshFitMethodNames();
}

void XSGlobal::initializeGUILibraries(Tcl_Interp* xsInterp)
{
// initialize Tk. If the initialization succeeds, defind the Tk package to Tcl. 
// If it fails, generally speaking this is because the user does not have access
// to and X display, either because of permissions, environment (no DISPLAY variable)
// or because they are not running under X. In this case, simply continue silently
// and do not initialize Tk. 

#ifdef GUI_SUPPORT
        int status(TCL_OK);
        if ( (status = Tk_Init(xsInterp)) == TCL_OK) 
        {
                char tk[] = "Tk";
                Tcl_StaticPackage(xsInterp, tk, Tk_Init, Tk_SafeInit); 
        } 

        // initialize Itcl/Itk

        //
        //  This is itkwish, so import all [incr Tcl] commands by
        //  default into the global namespace.  Fix up the autoloader
        //  to do the same. Use TclInitErr exception to make the flow look simple.
        //  In principle we could load Itcl for the non GUI case, but this is not
        //  too necessary, so we load both or neither.

        if (status == TCL_OK) 
        {
		if ( (status = Itk_Init(xsInterp)) == TCL_OK)
		{
               	        char CItk[] = "Itk";
               	        char itkn[] = "::itk::*";
               	        Tcl_StaticPackage(xsInterp, CItk, Itk_Init, (Tcl_PackageInitProc *) NULL);
                        if ((status = 
                                Tcl_Import(xsInterp, Tcl_GetGlobalNamespace(xsInterp),itkn, 1)) == TCL_OK)
                        {
               	                char mkindex[] = 
               	                       "auto_mkindex_parser::slavehook { _%@namespace import -force ::itcl::* ::itk::* }";
               	                if ( (status = Tcl_Eval(xsInterp,mkindex))  == TCL_ERROR)
                                {
                   	                throw TclInitErr("Itk Error: constructing script index itcl/itk");

                                }
                        }
                        else
                        {
                                throw TclInitErr("Itk Error: getting itk namespace");
                        }
                }
                else
                {
 			throw TclInitErr("Itcl Error: cannot initialize Itk");
		}
        }
        else
        {
                throw TclInitErr("Itcl Error: getting itcl namespace");
        }

        if ( (status = Pow_Init(xsInterp)) == TCL_ERROR)
        {
                std::cerr << "Cannot initialize graphical interface.\n";
                std::cerr << "Switching to command line mode." << std::endl;
                globalData->gui(false); 

        }
#endif
    // NB this necessitates "true" as the default
    // setting since the .xspecrc file hasn't been read yet.

}

void 
XSGlobal::registerNativeStatMethods()
{
        using namespace XSContainer;
        Weight* standard = new StandardWeight;
        Weight* churazov = new ChurazovWeight;
        Weight* gehrels = new GehrelsWeight;
        Weight* modWeight = new ModelWeight;
        StatManager::registerWeightingMethod(standard->name(), standard);     
        StatManager::registerWeightingMethod(churazov->name(), churazov);     
        StatManager::registerWeightingMethod(gehrels->name(), gehrels);     
        StatManager::registerWeightingMethod(modWeight->name(), modWeight);

        // Can't assume Fit container already exists (in fact it doesn't),
        // so can't access its StatManager pointer.  That's OK since
        // StatManager is a singleton, we'll just instantiate it here.
        // However we ARE going to assume that Fit will soon be created and
        // take ownership of it, so don't add it to the cleanUp function.
        StatManager* sm = StatManager::Instance();
        sm->registerStatMethod("chi", new ChiSquare);
        sm->registerStatMethod("cstat", new Cstat<StdCstat>);
        sm->registerStatMethod("lstat", new Cstat<LorStat>);
        sm->registerStatMethod("pgstat", new Cstat<PGstat>);
        sm->registerStatMethod("pstat", new Cstat<Pstat>);
        sm->registerStatMethod("whittle", new Cstat<WhittleStat>);
        sm->setDefaultStat("chi");

        sm->registerTestStatMethod("chi", new ChiSquare);
        sm->registerTestStatMethod("pchi", new PearsonChiSquare);
	sm->registerTestStatMethod("ks",new EDF<K_S>);
	sm->registerTestStatMethod("cvm",new EDF<C_vM>);
	sm->registerTestStatMethod("ad",new EDF<A_D>);
	sm->registerTestStatMethod("runs",new Runs);
        sm->setDefaultTestStat("chi");

        string doc("set statistics to optimize or test for all or selected spectra\n");
        doc += "   Syntax:statistic [ ";
        doc += sm->statNames();
        doc += " ]  [<optional spectrum range>]\n";
	doc += "          statistic test [ ";
        doc += sm->testStatNames();
        doc += " ]  [<optional spectrum range>]";
        summaryMap["statistic"] = doc;
        doc =  "set data weighting\n    Syntax:weight [ "   ; 
        doc += StatManager::weightNames();
        doc += " ]";
        summaryMap["weight"] = doc;
}

void 
XSGlobal::registerNativePlotCommands()
{
        string cmdList(PlotCommandCreator::registerPlotCommands());
        string doc ("plot data/models/fits etc\n    Syntax: plot commands:\n");
        size_t fieldWidth(15);
        size_t npcommands(cmdList.size() / fieldWidth);
        size_t  cnpl ( npcommands/5);
        for (size_t j = 0; j < cnpl; ++j)
        {
                doc +=  "\t";
                doc +=  cmdList.substr( j*5*fieldWidth, 5*fieldWidth );
                doc +=  "\n";     
        }
        doc +=  "\t";
        doc += cmdList.substr(cnpl*5*fieldWidth);
        doc +=  "\n";
        doc += "    Multi-panel plots are created by entering multiple commands\n";
        doc += "      e.g. \"plot data chisq\"\n";

        summaryMap["plot"] = doc;

}

void
XSGlobal::registerNativeTables()
{
    TableComponent::registerTableFormat(new OGIPTable(""));
}

void
XSGlobal::registerNativeLineLists()
{
   LineList::registerLineList("apec", new Apec);
   LineList::registerLineList("bearden", new Bearden);
   LineList::registerLineList("mekal", new Mekal);
}

void XSGlobal::nonTclAppInit()
{
    // When Xspec is NOT run as a Tcl interpreter application (ie. when
    // run from a Python shell), this function is needed to create an
    // internal Tcl interpreter for the global "interp" variable, and
    // to load libraries that would otherwise be loaded from the
    // xspec.tcl script.  See also nonTclAppClean().

    // When Xspec IS run as a Tcl interpreter, "interp" should already
    // be set and this function should do nothing.
    if (!interp)
    {
       interp = Tcl_CreateInterp();
       Tcl_Init(interp);

       const string LIBDIR(string(getenv("HEADAS")) + "/lib");
       string tclCmd(string("lappend auto_path ") + LIBDIR);
       Tcl_Eval(interp, tclCmd.c_str());
       Tcl_ResetResult(interp);
    }
}

void 
XSGlobal::startUp(bool displayVersion)
{


        using namespace XSContainer;

        string title, buildDate;
	XSutility::XSVersionString(title, buildDate);
        if (displayVersion)
        {
           tcout << '\n' << "\t\t" << title << std::endl; 
	   tcout << '\t' << buildDate << '\n' << std::endl;
        }            

        DataSet::xspecVersion(title);

        // This does nothing if Xspec is run as a Tcl interpreter -- the
        // standard case.
        nonTclAppInit();

        // now we can process the other initial settings. 

        globalData->processSettings();


	if (!globalData->gui())
	{
	   plot = PlotDirector::Instance(globalData->defaultGraph(),globalData->managerDir());
           string waveUnits(globalData->settings("WAVE_PLOT_UNITS"));
           if (waveUnits.length())
           {
              const string angstroms("angstroms");
              if (angstroms.find(XSutility::lowerCase(waveUnits)) == 0)
                 plot->setplot().isWavePerHz(false);
           }    
	}
	else
	{
	   // This will throw a RedAlert for the time being.
	        plot = PlotDirector::Instance(" ",globalData->managerDir());
	}

        string plotDevice (globalData->settings("PLOTDEVICE"));
	try
	{
        	if ( plotDevice.length())
        	{
                   plot->setPlottingDevice(plotDevice);
        	}  
	}
	catch ( ... )
	{
		// do nothing, the exception will have  printed a
		// message
	}

	string contourImage (globalData->settings("CONTOUR_IMAGE"));
	if ( contourImage.length() ) {
	  if (XSutility::lowerCase(contourImage) == string("true")) {
	    plot->setplot().contBackImage(true);
	  } else {
	    plot->setplot().contBackImage(false);
	  }
	}

        //register plot commands. Must happen after the plot control object 
        // is instantiated

        registerNativePlotCommands();


}

void
XSGlobal::cleanUp()
{
   LineList::clearLineLists();
   TableComponent::clearTableFormats();
   FitMethod::clearMethods();
   StatManager::clearWeightingMethods();
   delete Help::helpTree();
   delete DummyResponse::Instance();
   delete XspecRegistry::Instance();

   // in case the order is important this should be right.
   delete SignalHandler::instance();
   delete XSContainer::plot;
      XSContainer::plot = 0;
   delete XSContainer::fit;
      XSContainer::fit = 0;
   delete XSContainer::models;
      XSContainer::models = 0;
   delete XSContainer::datasets;
      XSContainer::datasets = 0;
   delete ParamLinkList::Instance();
   delete XSContainer::responses;
      XSContainer::responses = 0;

   // Deliberately placing this after the destruction of models.  
   // Not currently necessary (5/08), but model components do carry
   // generator pointers to model functions, and it's best that these
   // remain valid throughout the model destruction process.
   clearFunctionMap();
   MathExpression::clearOperatorsMap();

   system(string("rm -f " + globalData->undoFile()).c_str());

   std::ostringstream cmd;

   string autosaveFile = globalData->autoSaveFile();

   struct stat info;

   stat(autosaveFile.c_str(), &info);

   if(info.st_size)
       cmd << "mv -f " << autosaveFile << ' ' 
	   << autosaveFile.substr(0, autosaveFile.rfind("xautosav_") + 8) << ".xcm";
   else
       cmd << "rm -f " << autosaveFile;

   system((cmd.str()).c_str());

   delete globalData;
}

void
XSGlobal::nonTclAppClean()
{
   // Clean up for items specifically created in nonTclAppInit.
   Tcl_DeleteInterp(interp);
}

void 
XSGlobal::createCommandMap()
{

        const string& managerDir = XSGlobal::globalData->managerDir();
	string comfilename = managerDir + "/CommandSummary.txt";
	ifstream comfile;
	comfile.open(comfilename.c_str());
	if (!comfile) {
	  string message = "Cannot open " + comfilename;
	  throw RedAlert(message);
	}

	string line("");
	size_t nlines;
	string comName;

	while (!comfile.eof()) {

	  getline(comfile,line);
	  istringstream s(line);
	  s >> comName >> nlines;
	  s.clear();

	  string doc("");
	  for(size_t i=0; i<nlines; i++) {
	    getline(comfile,line);
	    doc += line;
	    if (i < nlines-1) doc += "\n";
	  }

	  summaryMap[comName] = doc;

	  getline(comfile,line);

	}


        commandMap["?"] = &xsComsum;
        commandMap["abund"] = &xsAbund;
        commandMap["addcomp"] = &xsAddcomp;
        commandMap["arf"] = &xsArf;
        commandMap["autosave"] = &xsAutosave;
        commandMap["backgrnd"] = &xsBackgrnd;
        commandMap["bayes"] = &xsBayes;
        commandMap["chain"] = &xsChain;
        commandMap["comsum"] = &xsComsum; // broken 11/29/01 - typing in comsum gives
                                          // nonsense.
        commandMap["corfile"] = &xsCorfile;
        commandMap["cornorm"] = &xsCornorm;
        commandMap["cosmo"] = &xsCosmo;
        commandMap["cpd"] = &xsCpd;
        commandMap["chatter"] = &xsChatter;
        commandMap["data"] = &xsData;
        commandMap["delcomp"] = &xsDelcomp;
        commandMap["diagrsp"] = &xsDiagrsp;
        commandMap["dummyrsp"] = &xsDummyrsp;
        commandMap["editmod"] = &xsEditmod;
        commandMap["energies"] = &xsEnergies;
        commandMap["eqwidth"] = &xsEqwidth;
        commandMap["error"] = &xsError;
        commandMap["uncertain"] = &xsError; 
        commandMap["exit"] = &xsExit;
        commandMap["fakeit"] = &xsFakeit;
        commandMap["fit"] = &xsFit;
        commandMap["flux"] = &xsFlux;
        commandMap["lumin"] = &xsFlux;
        commandMap["freeze"] = &xsFreeze;
        commandMap["ftest"] = &xsFtest;
        commandMap["gain"] = &xsGain;
        commandMap["goodness"] = &xsGoodness;
        commandMap["help"] = &xsHelp;
        commandMap["identify"] = &xsIdentify;        
        commandMap["ignore"] = &xsIgnore;
        commandMap["improve"] = &xsImprove;
        commandMap["initpackage"] = &xsInitpackage;
	commandMap["iplot"] = &xsIplot;
        commandMap["log"] = &xsLog;
        commandMap["lmod"] = &xsLmod;
        commandMap["margin"] = &xsMargin;
        commandMap["mdefine"] = &xsMdefine;
        commandMap["method"] = &xsMethod;
        commandMap["model"] = &xsModel;
        commandMap["newpar"] = &xsNewpar;
        commandMap["notice"] = &xsNotice;
        commandMap["parallel"] = &xsParallel;
        commandMap["plot"] = &xsPlot;
        commandMap["query"] = &xsQuery;
        commandMap["quit"] = &xsExit;
        commandMap["rerror"] = &xsError;
        commandMap["renorm"] = &xsRenorm;
        commandMap["response"] = &xsResponse;
        commandMap["rfreeze"] = &xsFreeze;
        commandMap["rmodel"] = &xsRmodel;
        commandMap["rnewpar"] = &xsNewpar;
        commandMap["rthaw"] = &xsThaw;
        commandMap["runtie"] = &xsUntie;
        commandMap["script"] = &xsScript;
        commandMap["setplot"] = &xsSetplot;
        commandMap["save"] = &xsSave;        
        commandMap["show"] = &xsShow;
        commandMap["statistic"] = &xsStatistic;
        commandMap["steppar"] = &xsSteppar;
        commandMap["systematic"] = &xsSystematic;
        commandMap["thaw"] = &xsThaw;
        commandMap["tclout"] = &xsTclout;
        commandMap["time"] = &xsTime;
	commandMap["undo"] = &xsUndo;
        commandMap["untie"] = &xsUntie;
        commandMap["version"] = &xsVersion;
        commandMap["weight"] = &xsWeight;
        commandMap["xsect"] = &xsXsect;
        commandMap["xset"] = &xsXset;

        if ( !XSGlobal::globalData->gui()) 
        {       
                // internal, not documented.
                commandMap["unknown"] = &xsUnknown; 
        }
        else 
        {
                // internal, not documented: used by GUI
                // which redefines unknown by itself.
                // XSunknown is the default handler for
                // unknown commands caught this way.
                commandMap["XSunknown"] = &xsUnknown; 
        }


        commandMap["genetic"] = &xsGenetic;
        commandMap["recornrm"] = &xsRecornrm;       
        commandMap["thleqw"] = &xsThleqw;

}        

void XSGlobal::registerFunctionUtility ()
{

        FunctionUtility::managerPath(globalData->managerDir());
        string dataPath(globalData->managerDir() + "/../modelData/");        
        FunctionUtility::modelDataPath(dataPath);
        string crossSections;
        string abundances;
        FunctionUtility::readInitializers(crossSections,abundances);
        string xSectDoc("Change/Report the photoionization cross sections\n");
        xSectDoc += "    Syntax: xsect " + crossSections;
        summaryMap["xsect"] = xSectDoc;
        string abundDoc("Change/Report the solar abundance table in use\n");
        abundDoc += "    Syntax: abund " + abundances +"\n";
        abundDoc += "            abund <name> <file> \n"; 
        abundDoc += "     (define & load new vector <name> from data in <file>)";
        summaryMap["abund"] = abundDoc;

}

string XSGlobal::refreshFitMethodNames()
{                       
        string fitMethodDoc("set fitting algorithm\n");     
        fitMethodDoc += "    Syntax:method [ ";
        fitMethodDoc += FitMethod::fitNames();
        fitMethodDoc += "]  <initial data for method>" ; 
        return fitMethodDoc;               
}

void XSGlobal::commonPlotHandler(const StringArray& args, bool isInteractive)
{
   using XSContainer::plot;

   // If user only entered "plot" (or "iplot"), args should still be
   // filled with 1 empty string.
   //lastCommand defaults to "data" in PlotDirector constructor
   IntegerArray iParams;  // Contour plots will need this.
   StringArray plotArgs;
   if (args[0].empty())
   {
      plotArgs = plot->lastCommand();
      iParams = plot->lastCommandIndices();
   }
   else
   {
      XSparse::collectParams(args,iParams,plotArgs);
      // In case of something silly such as only entering commas...
      if (!plotArgs.size())
      {
         plotArgs = plot->lastCommand();
         iParams = plot->lastCommandIndices();
      }
   }

   // For compatibility with previous xspec versions:
   // The "plot sum" command must treated differently than every other
   // PlotCommand class.  It displays a 4-pane plot consisting of
   // pairs of data and residual panes.  PlotDirector will implement
   // it by replacing a PlotSum object with PlotData and PlotResiduals
   // objects.  Here we just make sure that "plot sum" is combined
   // with NO OTHER PLOTS.
   bool isPlotSum = false;

   bool isContourPlot = false;
   std::vector<PlotCommand*> plotCommands; 
   try
   {  
      for (size_t iCom=0; iCom<plotArgs.size(); ++iCom)
      {
         // This is complicated by the fact that some plot commands can
         // also take parameters (ie. chain, model).  This loop however
         // must begin with a valid plot command and not a parameter.

         // Also if doing a contour plot, only 1 PlotCommand is allowed.
         const string& plotArg = plotArgs[iCom];
         PlotCommand* plotCom = 
                   PlotCommandCreator::commands(XSutility::lowerCase(plotArg));
         if (!plotCom)
         {
            string errMsg("Plot subcommand does not exist: ");
            errMsg += plotArg + "\n";
            throw YellowAlert(errMsg);
         }
         if (isPlotSum || (plotCom->cmdName() == "sum" && !plotCommands.empty()))
         {
            throw YellowAlert("Plot sum cannot be displayed with other plots.\n");
         }
         if (isContourPlot || (plotCom->isContour() && !plotCommands.empty()))
         {
            throw YellowAlert("Contour plots cannot be displayed with other plots.\n");
         }
         if (plotCom->cmdName() == "sum")
            isPlotSum = true;      
         if (plotCom->isContour())
            isContourPlot = true;

         // Add plotCom to vector PRIOR to any calls to processAdditionalParams.
         // It makes exception handling easier.
         plotCommands.push_back(plotCom);
         if (plotCom->doesTakeParameters())
         {
            // Keep gathering arguments left-to-right until we either find 
            // another valid plot command, or we reach the end.
            // All parameters will simply be passed as strings.  This
            // leaves it up to the plot command to do what it wants with them.
            StringArray additionalParams;
            IntegerArray additionalParamIndices;
            size_t iTest = iCom+1;
            PlotCommand* testCom = 0;
            while (iTest < plotArgs.size() && !testCom)
            {
               const string& testArg(XSutility::lowerCase(plotArgs[iTest]));
               testCom = PlotCommandCreator::commands(testArg);
               if (!testCom)
               {
                  additionalParams.push_back(plotArgs[iTest]);
                  additionalParamIndices.push_back(iParams[iTest]);
                  ++iTest;
               }
            }

            plotCom->processAdditionalParams(additionalParams,additionalParamIndices);

            iCom = iTest-1; // Remember, iCom will be auto-incremented at loop's end. 
         }

      } // end plotArgs loop
   }
   catch (...)
   {
      // processAdditionalParams may have changed the state of 1 or more
      // plot commands.
      std::vector<PlotCommand*>::iterator itCom = plotCommands.begin();
      while (itCom != plotCommands.end())
      {
         (*itCom)->cleanup();
         ++itCom;
      }
      throw;
   }

   // Only if all strings have been verified do we want to save as
   // default command.  Note that this is not as rigid as requiring
   // that the plot itself will proceed with no throwing.
   plot->lastCommand(plotArgs);
   plot->lastCommandIndices(iParams);
   plot->setplot().isInteractive(isInteractive);

   plot->makePlot(plotCommands);    
}

void XSGlobal::saveAll(const string& outFileName)
{
   std::ofstream outFile(outFileName.c_str());
   if (!outFile)
   {
      if (outFileName == globalData->autoSaveFile())
      {
         // Do NOT throw.  See note below in try/catch section.
         tcerr << "***XSPEC Error: Auto-save file cannot be opened for writing\n"; 
         // disable autosave to forbid further messages.  
         globalData->autoSaveFrequency(XSparse::NOTFOUND());
      }
      else
      {
         // Presumably this is a user-input file from xsSave.
         string errMsg("Unable to open file: ");
         errMsg += outFileName + '\n';
         throw YellowAlert(errMsg);
      }
   }

   // Since this could easily be called from the final catch block
   // of a handler file, we don't want any more YellowAlerts
   // to escape.  There may be nothing left to catch them.
   try
   {
      if (outFileName == globalData->autoSaveFile())
      {
         // Only autosave files should contain an absolute path,
         // and only at the start.
         outFile << "cd " << XSutility::getRunPath() << "\n" << std::endl;
      }

      saveData(outFile);
      saveModel(outFile);

      if (outFileName == globalData->autoSaveFile())
      {
         // It's possible for autosave.xcm (or undo) to end up in 
         // a different current working directory.  Therefore make
         // it a little more noticeable to user.
         outFile << "\npwd" << std::endl;
      }
   }
   catch(YellowAlert&)
   {
   }
}

void XSGlobal::saveData(std::ofstream& outStream)
{
   // defaultStat info must be handled here.
   // DataContainer has no access to StatManager.
   const StatMethod* statMethod = XSContainer::fit->statManager()->defaultStat();
   outStream << "statistic "  << statMethod->name() << endl;
   const string& defaultStat = XSContainer::fit->statManager()->defaultStat()->name();
   XSContainer::datasets->saveData(outStream, defaultStat);
}

void XSGlobal::saveModel(std::ofstream& outStream)
{
    using namespace std;
    FitMethod* fitMethod  (XSContainer::fit->fitMethod());
    outStream << "method "  << fitMethod->selectedSubMethod() << ' ' 
	    << fitMethod->settingString()   << endl;
    if(FunctionUtility::abundChanged())
        outStream << "cd " << FunctionUtility::abundPath() << "\nabund file " 
	          << FunctionUtility::abundanceFile() << endl;
    else
        outStream << "abund " << FunctionUtility::ABUND() << endl;
    outStream << "xsect " << FunctionUtility::XSECT() << endl;
    outStream << "cosmo " << FunctionUtility::getH0() << " "
        << FunctionUtility::getq0() << " " << FunctionUtility::getlambda0()
        << endl;
    outStream << "xset delta " 
        << XSContainer::models->proportionalDelta() << endl;
    const map<string,string>& xsetStrings = 
            FunctionUtility::modelStringDataBase();
    map<string,string>::const_iterator itString = xsetStrings.begin();
    map<string,string>::const_iterator itStringEnd = xsetStrings.end();
    while (itString != itStringEnd)
    {
       outStream << "xset " << itString->first << "  " 
            << itString->second << endl;
       ++itString;
    }
    outStream << "systematic " << XSContainer::models->modelSystematicError() << endl;

    XSContainer::models->saveData(outStream);
}

void XSGlobal::registerNativeRandomizingStrategies(Fit* fit)
{
   std::auto_ptr<RandomizerBase> rand(new Randomizer<Fit,RandomizerGaussDist>());
   // If the following call doesn't throw, relinquish ownership of 
   // rand to the fit container.
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   RandomizerBase* tmp = rand.release();
   fit->chainManager()->setChainProposal(tmp->name());
   rand.reset(new Randomizer<ChainManager,RandomizerGaussDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();
   rand.reset(new Randomizer<RandomizerCmdLine,RandomizerGaussDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();
   rand.reset(new Randomizer<RandomizerCovarFile,RandomizerGaussDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();

   rand.reset(new Randomizer<Fit,RandomizerCauchyDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();
   rand.reset(new Randomizer<ChainManager,RandomizerCauchyDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();
   rand.reset(new Randomizer<RandomizerCmdLine,RandomizerCauchyDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();
   rand.reset(new Randomizer<RandomizerCovarFile,RandomizerCauchyDist>());
   fit->registerRandomizingStrategy(rand->name(), rand.get());
   rand.release();


   // We want to distinguish these native classes from any the
   // user may add in at a later time.  So fill in Fit's
   // nativeRandomizerNames container with these names.
   // (This can only be done once.)
   fit->initNativeRandomizerNames();
}
