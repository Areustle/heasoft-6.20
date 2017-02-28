/* 
 * Driver routine for XSPEC C++ code with TCL command interpreter
 *
 * 
 * Modified 2/99 for signal handling, C++ compatibility, Tk usage
 * Keith Arnaud, Ben Dorman
 *
 * Programs to load Xspec commands into TCL interpreter.  Should
 * also allow for dynamic loading of XSPEC into an existing TCL
 * package.
 */

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/UserInterface/TkGui.h>
#include <XSUser/UserInterface/TclSigInt.h>
#include <XSUser/UserInterface/TclStream.h>
#include <XSUser/UserInterface/xstcl.h>
#include <XSUser/Help/Help.h>
#include <XSUser/Help/HelpComposite.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/ProcessManager.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSModel/Model/MixFunction/xsmixFunctionMap.h>
#include <XSFunctions/functionMap.h>
#include <XSFunctions/trns.h>
#include <XSsymbol.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <unistd.h>
#include <fstream>
#include <ctime>
#include <cstring>


namespace xstcl 
{
        int Xspec_Init(Tcl_Interp* xsInterp); 
        TkGUI* GUIdata = 0;
        int xs_return_result;
}   

extern "C" {
#ifdef GUI_SUPPORT
   #include <pow.h>
   #include <itcl.h>
   #include <itk.h>
#endif
#include <tclreadline.h>
}




namespace XSGlobal
{
        XSchannel* textChan = 0;
        XSchannel* errChan = 0;
        XSchannel* inChan = 0;
        XSchannel* conChan = 0;
}


static char* XSPEC_CURRENT;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tcl_Main never returns here, so this procedure never
 *	returns either.
 *
 *----------------------------------------------------------------------
 */

int
main(int argc,char ** argv)		/* Values of command-line arguments. */
{
    using namespace XSGlobal;

    globalData = XSGlobal::GlobalData::Instance();

    string binaryLoc(getenv("HEADAS"));
    binaryLoc += "/bin/xspec";
    XSPEC_CURRENT = new char[15 + binaryLoc.length()];
    strcat(strcpy(XSPEC_CURRENT,"XSPEC_CURRENT="),binaryLoc.c_str());
    putenv(XSPEC_CURRENT);
   // code for getting command line argument, initialization file etc goes here...

    globalData->readSettings();

    globalData->setDisplayMode();

#ifdef GUI_SUPPORT        
    if ( globalData->gui() )
    {
        Tk_Main(argc, argv, Tcl_AppInit);
    }
    else
    {    
        Tcl_Main(argc, argv, Tcl_AppInit);   
    }
#else    
    Tcl_Main(argc, argv, Tcl_AppInit);       
#endif

    delete [] XSPEC_CURRENT; 

    return 0;			/* Needed only to prevent compiler warning. */
// run event loop.

}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(Tcl_Interp *xsInterp)	/* Interpreter for application. */
{
    using namespace XSGlobal;
    using std::cerr;
    using std::endl;

    if (Tcl_Init(xsInterp) ) throw TclInitErr();


    interp = xsInterp;


    string tclrlPath = string(getenv("HEADAS"));
    tclrlPath += "/lib/tclreadline2.1.0";
    string tclrlInit (tclrlPath + string("/tclreadlineInit.tcl"));

    if (Tcl_EvalFile(xsInterp, tclrlInit.c_str()))  throw TclInitErr();    

    // add xspec's internal scripting path to tcl's path

    string addPath ("lappend auto_path  ");
    addPath  += globalData->scriptDir();
    Tcl_Obj* addPathObj = Tcl_NewStringObj(const_cast<char*>(addPath.c_str()),addPath.length());
    Tcl_GlobalEvalObj(xsInterp,addPathObj);




//#ifdef GUI_SUPPORT 
  if ( globalData->gui())
  {
        XSGlobal::initializeGUILibraries(xsInterp);

  }
//#endif    
    // Set up handler for Ctrl-C or SIGINT 
    // This TclSigInt obj should last for duration of program.
    // SignalHandler singleton can then destroy it during its
    // own destruction at the program's end.
    EventHandler* oldEh = SignalHandler::instance()->
                registerHandler(SIGINT, new TclSigInt());
    // There shouldn't have been any previous SIGINT handler at 
    //  this point, but just to be thorough....
    if (oldEh) delete oldEh;

    if (xstcl::Xspec_Init(xsInterp) == TCL_ERROR) 
    {
	return TCL_ERROR;
    }
     
    // Specify a user-specific startup file to invoke if the application
    // is run interactively. tcl_rcFileName is an internal Tcl variable.
    // Upon start-up, it causes Tcl to source the file to which it is set. 

    string tclrcCmd("tcl_rcFileName");
    string xsTclReadline(XSGlobal::globalData->scriptDir() + "/xs_tclreadline.tcl");
    Tcl_SetVar(xsInterp, tclrcCmd.c_str(), xsTclReadline.c_str(), TCL_GLOBAL_ONLY);
    return TCL_OK;
}


/*
 * Xspec_Init -- main XSPEC initialization routine.
 */
int
xstcl::Xspec_Init(Tcl_Interp *xsInterp)                
{

        using namespace XSGlobal;
        using namespace XSContainer;
        using namespace xstcl;
        xs_return_result = 0 ;



        createCommandMap();

        const string tclPref = "tcl";
        const char* renTclCommands[] = { "error","flush","time","exit",NULL};

// Set up for setting in XSPEC prompt in TCL. 

	xstcl::xs_tcl_prompt = new char[strlen(IosHolder::xsPrompt())+1];
        strcpy(xstcl::xs_tcl_prompt,IosHolder::xsPrompt());
        Tcl_SetVar(xsInterp, xsTclPrompt, xstcl::xs_tcl_prompt, 0);        



// set up xspec's command interpreter.
// code for renaming tcl/tk commands which clash with xspec command abbreviations


        Tcl_CmdInfo* infoPtr = new Tcl_CmdInfo;
        int i = 0;

        while ( renTclCommands[i] != NULL )
        {
                string currStrg = renTclCommands[i];
                int status = Tcl_GetCommandInfo(xsInterp, 
                         const_cast<char*>(renTclCommands[i]), infoPtr);
                if ( status ) 
                {
                        string renStrg = "rename " + currStrg + " " + tclPref + currStrg;
                        try
                        {
                                status = 
                                    Tcl_VarEval(xsInterp,const_cast<char*>(renStrg.c_str()),0) ;
                                if (status != TCL_OK) 
                                    throw TclInitErr("Tcl Error: renaming commands");
                        }
                        catch (TclInitErr) {}

                }
                ++i;
        }

         //rename unknown command if we aren't running gui, which deals with unknown
         // itself otherwise.
        if (!globalData->gui())
        {
                string renStrg = "rename unknown tclunknown";
                try
                {
                        int status = Tcl_VarEval(xsInterp,const_cast<char*>(renStrg.c_str()),0) ;
                        if (status != TCL_OK) throw TclInitErr("Tcl Error: renaming commands");
                }
                catch (TclInitErr) {}
        }



        delete infoPtr;


        // Add xspec commands to itkwish

        // create all the Xspec commands as TclObjCommands by iterating the command map.
        // yes, it's this simple!
        // in the command map, the "first" field (cmd->first] is the name of the command,
        // and the second field is a pointer of type Tcl_ObjCmdProc

        std::map <string, Tcl_ObjCmdProc*>::const_iterator   cmdEnd(commandMap.end());
        std::map <string, Tcl_ObjCmdProc*>::const_iterator   cmd(commandMap.begin());

        Tcl_CmdDeleteProc* nullProc = 0;

        while (cmd != cmdEnd)
        {
                char* cmdName = const_cast<char*>(cmd->first.c_str()) ;
                Tcl_CreateObjCommand(xsInterp,cmdName,cmd->second,(ClientData) cmdName,nullProc); 
                ++cmd;  
        }                

        Tcl_SetVar(xsInterp, "xs_tcl_prompt", IosHolder::xsPrompt(), TCL_GLOBAL_ONLY);
        Tcl_SetVar(xsInterp, "tcl_prompt1","puts -nonewline \"$xs_tcl_prompt\"", TCL_GLOBAL_ONLY);
        Tcl_SetVar(xsInterp, "tcl_prompt2","puts -nonewline \"-> \"", TCL_GLOBAL_ONLY);


        try
        {
          int linkStat = Tcl_LinkVar(xsInterp,xsTclPrompt, (char *) &xstcl::xs_tcl_prompt, TCL_LINK_STRING);
          if (linkStat != TCL_OK) throw TclInitErr("Tcl Error: linking prompt variable") ; 
        }
        catch (TclInitErr) {}

// set up IO channels        


        if  ( !globalData->gui() )
        {
                XSGlobal::textChan = new TclIO(Tcl_GetStdChannel(TCL_STDOUT));
                XSGlobal::errChan = new TclIO(Tcl_GetStdChannel(TCL_STDERR));
                XSGlobal::inChan  = new TclIO(Tcl_GetStdChannel(TCL_STDIN));
                XSstream::defineChannel(tcin,inChan,-32768);
	        tcin.tie(&tcout);

       }
        else
        {
	        //
	        // call the TkGUI constructor. This assigns those values to C variables
                // which are data members of the TkGUI class.
	        //
                xstcl::GUIdata = TkGUI::Instance();
	        //
                // create the windows in the startup script. 
	        //
                string guiStartup = globalData->scriptDir() + "/XStkcon.tcl";
                Tcl_EvalFile(xsInterp,const_cast<char*>(guiStartup.c_str()));

	        //
                // create output streams for output and error, and assign to the
                // named text and console window.
	        // It does not seem necessary to address the plot window with a stream.
	        // Also, if input streams are necessary from graphical widgets (i.e.
	        // to grab input from a dialogue box, these can be created and destroyed
	        // on the fly so are not created here.
                //
                XSGlobal::textChan = new TkIO(xstcl::GUIdata->textPath());
                XSGlobal::errChan  = new TkIO(xstcl::GUIdata->conPath());  // temporary setting
                XSGlobal::inChan   = new TkIO; // default is a null pointer. The
                                                 // widget will supply the window name.
                XSGlobal::conChan  = new TclIO(Tcl_GetStdChannel(TCL_STDOUT));

                // define a very large buffer for input. This is because for tcl
                // the tcl buffering mechanism is actually used. Thus a small
                // buffer might actually cause the internal read operations
                // to read less than tcl does, causing an almost incomprehensible
                // error [Tcl will grab the entire input whilst the read operation
                // will expect to call uflow() after the buffer length has been
                // read.

                // - sign indicates that this is an input channel.
                XSstream::defineChannel(tcin,inChan,-32768);
        }

        //Tcl_SetVar(xsInterp, ps1Cmd,ps1, 0);
        //Tcl_SetVar(xsInterp, ps2Cmd,ps2, 0);

        XSstream::defineChannel(tcout,textChan,bufSize);
        XSstream::defineChannel(tcerr,errChan,0);
        IosHolder::setStreams(&tcin, &tcout, &tcerr);

        // initialize verboseness levels.  Right now 5 is normal mode.
	// the 'terse' mode is 0,0 (con,log) and anything above 5 
	// prints out additional info. Setting con verbose to zero
	// and log verbose to something big will print out diagnostics.
	// if con or log chatter level are set really high then everything
	// will be printed to the respective destination.




  // Link C and TCL vars for echoing script commands. Default to zero. 

        char xes[] = "xs_echo_script";
        char xrr[] = "xs_return_result";

        try
        {
               int linkStat =Tcl_LinkVar(xsInterp, xes,(char *) &xs_echo_script, TCL_LINK_BOOLEAN);
               if (linkStat != TCL_OK) throw TclInitErr("Error linking result/echo variables");


        // Link C and TCL vars for whether or not to return Tcl results.  

              linkStat = Tcl_LinkVar(xsInterp, xrr, (char *) &xstcl::xs_return_result, TCL_LINK_BOOLEAN);
              if (linkStat != TCL_OK) throw TclInitErr("Error linking result/echo variables");
        }
        catch (TclInitErr) {}  
        // Execute the XSPEC standard initialization script. 

        createFunctionMap();

// create model & response containers. This will eventually be in the equivalent call to xs_start
// will also need to create data container.


        Parameter::initializeLinks();

        const string& datDir = globalData->managerDir();
        string modelDat = datDir + "/";
        modelDat += globalData->modDescripFile();

        XSModelFunction::updateComponentList(modelDat, true);

        // Now add mix models
        createxsmixFunctionMap();     
        XSModelFunction::updateComponentList(datDir + "/mixmodel.dat", true);
        
        registerFunctionUtility();

        DummyResponse* defaultEnergyArrayResponse (globalData->getDummyResponse());

        xsRegistry = XspecRegistry::Instance();

        XSContainer::datasets  = XSContainer::DataContainer::Instance();

        XSContainer::models    = XSContainer::ModelContainer::Instance();

        XSContainer::responses = XSContainer::ResponseContainer::Instance(defaultEnergyArrayResponse);

        // Provide a default initial seed to RandomGenerator singleton based on clock time.
        Numerics::DefaultRandomGenerator& randGen = 
                Numerics::DefaultRandomGenerator::instance();
        randGen.seed(time(0));
        randGen.initialize();

        // need to register Fit, Statistic and Weightings so we can check  
        // whether settings file makes any sense.
        registerNativeFitMethods();

        // also registers Statistical Weighting methods.
        registerNativeStatMethods();

	//registers all known table formats
	registerNativeTables();        

        // This is deliberately not a reference.  Want to store this 
        // even after globalData's method may be changed by init file.
        const string defaultMethod = globalData->method();      
        fit = Fit::Instance(defaultMethod);

        registerNativeRandomizingStrategies(fit);

        registerNativeLineLists();
        
        ProcessManager::initMaxProcsMap();

        // A ridiculous hack added to get around Mac OS X 
        // unresolved symbol problem when loading dynamic
        // libraries during the program via tcl's load command.
        // libxsmix calls the following function, but it must
        // first be called from the main body:
        Real dummy = MYLER(.5);

        // initialize user settings and interface

        XSGlobal::startUp();

        // This should go after call to startUp() since Help tree
        // initialization looks for LOCAL_MODEL_DIRECTORY, set it
        // GlobalData::processSettings().
        if (globalData->useOnlineHelp())
        {
           // URL will be filled in during Help::initHelpTree.
           Help::helpTree (new HelpComposite("XspecManual.html", string(""), Help::HTML, true));
        }
        else  
        {
           string helpDir;
           string startPage;
           if (globalData->localHelpFormat() == Help::HTML)
           {
              startPage = "XspecManual.html";
              helpDir = globalData->docuDir() + "/html";
           }
           else  // default PDF
           {
              startPage = "XspecManual.pdf";
              helpDir = globalData->docuDir();
           }
           Help::helpTree (new HelpComposite(startPage, helpDir, 
                                globalData->localHelpFormat(), false));
        }
        Help::initHelpTree();

        string initFile = globalData->scriptDir() + "/" + globalData->initScript();
        Tcl_EvalFile(xsInterp,initFile.c_str());

        // This delayed setting of the user's init file METHOD selection is
        // deliberately placed after the xspec.tcl script evaluation so that
        // minuit libraries are already loaded.
        const string& methodName = globalData->method();
        bool methodExists = false;
        if (methodName.length())
        {
           FitMethod* test = FitMethod::get(methodName);
           if (test)
           {
              methodExists = true;
              fit->fitMethod(test);
           }
        }
        if (!methodExists)
        {
          tcerr << " Fitting algorithm named: " << methodName 
                << " specified in " << globalData->initSettings() 
                << " not available - using default algorithm: " 
                << defaultMethod << '\n';  

        }

        // Execute the user's initialization script, but only
        // if they're not running in batch mode.

        // Note: Tcl's argc and argv do NOT contain the program name,
        // so look for "-" as 0th argv. 
        Tcl_Obj* argList = Tcl_GetVar2Ex(xsInterp,"argv", 0, 0);
        Tcl_Obj* argPtr=0;
        Tcl_ListObjIndex(xsInterp, argList, 0, &argPtr);
        if (!argPtr  || string(Tcl_GetString(argPtr)) != "-")
        {
           string userInit (getenv("HOME"));
           userInit += "/.xspec/xspec.rc";

           if ( !access(userInit.c_str(),R_OK) )
           {
                   Tcl_EvalFile(xsInterp,userInit.c_str());
           }
        }

        return TCL_OK;
}
