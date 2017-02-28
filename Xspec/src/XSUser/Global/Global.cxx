//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

// Global
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/Cosmology.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/Detector/DummyResponse.h>
#include <XSUser/Help/Help.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSRegEx.h>
#include <XSUser/UserInterface/TclRegEx.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/DataContainer.h>

#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include "XSContainer.h"
#include "XSstreams.h"
#include "XSsymbol.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <unistd.h> // access.

namespace XSGlobal {

    // Class XSGlobal::GlobalData::Startup 

    GlobalData::Startup::Startup (const string& msg)
    : RedAlert(msg)    
    {
    }


    // Class XSGlobal::GlobalData::NoSetting 

    GlobalData::NoSetting::NoSetting (const string& msg)
    : YellowAlert(msg) 
    {
    }


    // Class XSGlobal::GlobalData 
    GlobalData* GlobalData::s_instance = 0;

    GlobalData::GlobalData()
        : m_logging(false),
          m_gui(false),
          m_initSettings("Xspec.init"),
          m_managerDir("manager"),
          m_docuDir("help"),
          m_modDescripFile("model.dat"),
          m_commandFile("xspec.cmd"),
          m_userDir(""),
          m_initScript("xspec.tcl"),
          m_commandLine(""),
          m_method("leven"),
          m_currentTime(new struct rusage),
          m_initTime(new struct rusage),
          m_defaultGraph(""),
          m_defaultLocalModelDirectory(""),
          m_scriptDir("scripts"),
          m_commandCount(0),
          m_autoSaveFrequency(1),
          m_autoSaveFile(""),
          m_userScriptDir(""),
	  m_undoFile(""),
	  m_memento(0),
          m_useOnlineHelp(false),
          m_localHelpFormat(Help::PDF),
          m_settings()
    {

        // initialize CPU counter.
        getrusage(RUSAGE_SELF,m_initTime);
        getrusage(RUSAGE_SELF,m_currentTime);

        // set up directory paths: exit on failure.
        const string HEADAS(getenv("HEADAS"));
        const string HOME (getenv("HOME"));


        if ( HEADAS.length() == 0 )   throw Startup(" HEADAS environment variable not set");

        m_userDir = HOME + "/.xspec";

	// if the user directory doesn't exist then create it
        bool cantMakeXspecDir = false;
	if(access(m_userDir.c_str(),F_OK|W_OK|R_OK|X_OK))
	{
	   if(mkdir(m_userDir.c_str(),S_IRWXU))
	   {
	      std::cerr << "*** Warning: Unable to create a $HOME/.xspec directory\n";
              cantMakeXspecDir = true;
           }
           else
           { 
	      std::cout << "Creating a $HOME/.xspec directory for you\n";
           }
        }
        // Can't assume that if .xspec directory already exists then so 
        // does .xspec/cache.  v11 may have created just an empty .xspec dir.
        if (!cantMakeXspecDir)
        {
           string cacheDir (m_userDir + "/cache");
           if (access(cacheDir.c_str(),F_OK|W_OK|R_OK|X_OK))
           {
              if(mkdir(cacheDir.c_str(),S_IRWXU))
	      {
	         std::cerr << "*** Warning: Unable to create a $HOME/.xspec/cache directory\n";
	      }
           }
        }

        // The spectral directory tree may either be in $HEADAS/../ (most likely)
        //   or in $HEADAS.
        string testDir = HEADAS + "/../spectral/";
        if (access(testDir.c_str(),R_OK))
        {
           testDir = HEADAS + "/spectral/";
           if (access(testDir.c_str(),R_OK))
           {
              throw Startup(" XSPEC not properly installed - cannot find spectral directory\n");
           }
        }
        
        m_managerDir  = testDir + m_managerDir;      
        m_scriptDir   = testDir + m_scriptDir;      
        m_docuDir     = testDir + m_docuDir;

	// test whether these system directories exist and throw error if they
        // don't
	if ( access(m_managerDir.c_str(),R_OK) || 
             access(m_scriptDir.c_str(),R_OK) || 
             access(m_docuDir.c_str(),R_OK) ) throw Startup(" XSPEC not properly installed - cannot find manager, script, or help directory");


	string processID = XSparse::IntToString(getpid());

	std::ostringstream autoSaveOut;
	autoSaveOut << m_userDir << "/cache/xautosav_" << processID << ".xcm";
	m_autoSaveFile = autoSaveOut.str();

	system(("touch " + m_autoSaveFile).c_str());

	std::ostringstream undoOut;
	undoOut << m_userDir << "/cache/.xspec_undo_" << processID << ".xcm";
	m_undoFile = undoOut.str();
    }


    GlobalData::~GlobalData()
    {
       delete m_currentTime;
       delete m_initTime;
    }


    GlobalData* GlobalData::Instance ()
    {
      if (s_instance == 0) {s_instance = new GlobalData;} return s_instance;
    }

    void GlobalData::readSettings ()
    {

            // this is called only once, very early in the programme, before
            // the Tcl-based streams have been ctored. Any I/O has to go through
            // std streams.

            static const string NULLCHARS(" \t");

            using namespace std;


	    string  UsersettingFileName (m_userDir + "/" + initSettings());   
            string      settingFileName (UsersettingFileName);   

            // look for permissions on user setting directory and 
            // open setting file there if present. If not, look at
            // XSPEC source code location.
            if (access(settingFileName.c_str(),R_OK))
            {
               settingFileName = m_managerDir + "/" + initSettings();
               if (access(settingFileName.c_str(),R_OK))
	       {
                  std::cerr << "*** Fatal: Cannot read XSPEC initial settings file " << initSettings()
                     << "\n*** either in XSPEC source installation (" << m_managerDir  << ")"
                     << "\n*** or in user's home directory (" << m_userDir << "). exiting\n" ;                            
                  abort();
	       } 
               else
	       {
		  // we have a file in the XSPEC source code location but not 
		  // in the user's directory so attempt to copy over. First
                  // check whether there is a directory that we can write into
		  // if not then create one and tell the user

		  string cmdStr ("cp -p " + settingFileName + " " + m_userDir + "/" + initSettings());
		  std::system(cmdStr.c_str());

		  // Check whether the copy worked - if it did then switch
		  // the string to point to it and if not then warn

		  if(access(UsersettingFileName.c_str(),R_OK))
		  {
		     std::cerr << "*** Warning : Unable to copy Xspec.init to $HOME/.xspec\n";
		  }
		  else
		  {
		     settingFileName = UsersettingFileName;
		  }

               }
            }



            ifstream setupFile(settingFileName.c_str());

            // cannot throw exceptions: this is called before the tcl interpreter
            // has been initialized. 

            setupFile.exceptions(ios_base::goodbit);

            if (!checkInitVersion(setupFile))
            {
               std::cerr << "***Warning: You do not have an up-to-date version of Xspec.init\n"
                         <<"      in your ~/.xspec directory.  It is recommended that you move\n"
                         <<"      your old Xspec.init aside and start up XSPEC again.  This will\n"
                         <<"      place a new version Xspec.init in your directory, which you may\n"
                         <<"      then modify using your old settings." << std::endl;
            }            
            while ( setupFile )
            {
                string rawLine("");
                string setupKey("");
                string setupData("");   

                getline(setupFile,rawLine);
                int first = rawLine.find_first_not_of(NULLCHARS);
                if (first > 0 )rawLine = rawLine.substr(first);
                if ( rawLine.length() != 0 && first != -1)
                {
                        // ignore lines with '#' comment mark as first significant character.
                        if ( rawLine[0] == '#') continue;


                        // ignore lines that do not meet syntax specification (no delimiter)
                        int delim = rawLine.find_first_of(":");
                        if (delim <= 0  ) continue;

                        setupKey = rawLine.substr(0,rawLine.substr(0,delim).find_last_not_of(NULLCHARS)+1);


                        setupData = rawLine.substr(delim+1);
                        // strip leading and trailing blanks.
                        size_t firstChar (setupData.find_first_not_of(NULLCHARS));
                        size_t length (setupData.find_last_not_of(NULLCHARS) - firstChar + 1);
                        setupData   = setupData.substr(firstChar,length);
                        settings(setupKey,setupData);

                }

            }



            // GUI setting.
    }

    std::string GlobalData::settings (const std::string& key) const
    {
     std::map<std::string,std::string>::const_iterator f = m_settings.begin();
     std::map<std::string,std::string>::const_iterator fEnd = m_settings.end();
     while (f != fEnd)
     {
        if (key.find(f->first) != std::string::npos)
	{
	   break;
	}
        ++f;
     }     
     if ( f == fEnd) return std::string("");
     return f->second;
    }

    void GlobalData::setDisplayMode ()
    {
      string ifKey =  XSutility::lowerCase(settings("GUI"));
      if (ifKey.length() > 0 && ifKey[0] == 't') gui(true);    
    }

    void GlobalData::processSettings ()
    {

      string ifKey;

      // chatter settings
      ifKey = settings("CHAT");
      if (ifKey.length() > 0)
      {
              std::istringstream k(ifKey);
              int logChat(-1);
              int conChat(-1);
              // ignore stream exceptions.
              k >> conChat >> logChat;
              if (conChat > 0 && logChat > 0)
              {
                      tpout.consoleChatterLevel(conChat);
                      tpout.logChatterLevel(logChat);       
              }
	      else if (conChat < 0)
	      {
			tpout.consoleChatterLevel(10);
			tpout.logChatterLevel(0);
              }
      }

      ifKey =  settings("XSECT");
      if (ifKey.length() && FunctionUtility::checkXsect(ifKey))
      {
              tpout.setVerbose(0, 25);
              FunctionUtility::XSECT(ifKey);
              tpout.setVerbose();   
      }
      else
      {
              tcerr << " Incorrect setting for cross section tables: " << ifKey 
                << " specified in " << m_initSettings 
                << " using default : (Balucinska-Church & McCammon 1998)\n"; 
      }

      ifKey =  settings("ABUND");
      if (ifKey.length() && FunctionUtility::checkAbund(ifKey))
      {
              tpout.setVerbose(0, 25);
              FunctionUtility::ABUND(ifKey);
              tpout.setVerbose();
      }
      else
      {
              tcerr << " Incorrect setting for abundance vector: " << ifKey 
                << " specified in " << m_initSettings 
                << " using default : (Anders & Grevesse 1989)\n"; 
      }

      ifKey =  settings("ATOMDB_VERSION");
      if (ifKey.length())
      {
              tpout.setVerbose(0, 25);
              FunctionUtility::atomdbVersion(ifKey);
              tpout.setVerbose();
      }
      else
      {
              tcerr << " Cannot find ATOMDB_VERSION in Xspec.init, "
                << " using default : (3.0.7)\n";
              tpout.setVerbose(0, 25);
              FunctionUtility::atomdbVersion("3.0.7");
              tpout.setVerbose();
      }

      ifKey =  settings("NEI_VERSION");
      if (ifKey.length())
      {
              tpout.setVerbose(0, 25);
              FunctionUtility::neiVersion(ifKey);
              tpout.setVerbose();
      }
      else
      {
              tcerr << " Cannot find NEI_VERSION in Xspec.init, "
                << " using default : (3.0.4)\n";
              tpout.setVerbose(0, 25);
              FunctionUtility::atomdbVersion("3.0.4");
              tpout.setVerbose();
      }

      // Don't test fit method at this stage.  We'll do it later
      // in xspec.cxx after minuit libraries have been loaded.
      m_method = settings("METHOD");

      ifKey = settings("STATISTIC");
      const StatMethod* testStat=0;
      if (ifKey.length())
      {
         testStat = XSContainer::fit->statManager()->setDefaultStat(ifKey);
      }
      if (!testStat)
      {
          tcerr << " Statistic named: " << ifKey 
                << " specified in " << m_initSettings 
                << " not available - using default statistic: " 
                << XSContainer::fit->statManager()->defaultStat()->name() << '\n';  
      }


      ifKey = settings("WEIGHT");
      bool weightExists = false;
      if (ifKey.length())
      { 
         XSContainer::Weight* tmp = StatManager::getWeightingMethod(ifKey);
         if (tmp)
         {
            // Only print low chatter weight output in this start-up 
            // context if user is doing something non-standard.
            bool usingStdWeight=false;
            if (tmp->name() == XSContainer::fit->statManager()->weightCmdSetting())
            {
               usingStdWeight = true;
               tpout << xsverbose(15);
            }
             weightExists = true;
             // Some stat methods may not accept tmp->name(), and will
             // substitute standard instead.
             XSContainer::fit->statManager()->setStatWeight(tmp->name());
             
             if (usingStdWeight)
                tpout << xsverbose();
         }
      }
      if (!weightExists)
      {
          tcerr << " Statistical weighting scheme: " << ifKey 
                << " specified in " << m_initSettings 
                << " not available - using standard weighting.\n" ;  
      }

      ifKey = settings("USE_NUMERICAL_DIFFERENTIATION");
      if (ifKey.length())
      {
         if (XSutility::lowerCase(ifKey) == string("true"))
            XSContainer::fit->useNumericalDifferentiation(true);
      }

      ifKey = settings("COSMO");
      std::istringstream cs(ifKey);
      Real qq(0), hh(0), ll(0);
      cs.exceptions( std::ios_base::badbit | std::ios_base::failbit );
      try
      {
                cs >> hh >> qq >> ll;
      }
      catch ( std::exception& )
      {
        tcerr << " Cosmology settings in initialization file corrupted - check manager/Xspec.init\n";       
      }

      XSContainer::Cosmology C(hh,qq,ll);

      XSContainer::models->cosmo(C);
      FunctionUtility::setFunctionCosmoParams(hh,qq,ll);

      ifKey = settings("GRAPH");
      if (ifKey.length())
      {
         m_defaultGraph = ifKey;
      }
      else
      {
         throw RedAlert("Bad Xspec.init file.  No default graphics package found.\n");
      }



      const string USERSCRIPT("USER_SCRIPT_DIRECTORY");
      const string LDPATH("LD_LIBRARY_PATH");

      ifKey = settings(USERSCRIPT);
      if (ifKey.length())
      {
            string userScript (XSparse::expandDirectoryPath(ifKey));
            static const string GLOB(".~/$");
            if (userScript.length())
            {
                 if ( GLOB.find(userScript[0]) != XSparse::NOTFOUND())
                 {
                        static const string TCLAPPEND("lappend auto_path ");
                        string tclAppend (TCLAPPEND + userScript);
                        Tcl_Eval(interp,tclAppend.c_str());
                        m_userScriptDir = userScript;
                }
                else
                {
                        tcerr << "*** Warning: setting for user script "
                              << " directory in  Xspec.init must begin with \n"
                              << " *** absolute path or environment "
                              << "variable: ignored.\n";
                }

            }
            else
            {
               // Something went wrong in expandDirectoryPath which will
               // issue its own error message.
               tcerr << "*** Warning: setting for user script directory "
                  << "will be ignored."<<std::endl;
            }                
      }

      const string LOCAL("LOCAL_MODEL_DIRECTORY");
      ifKey = settings(LOCAL);
      // expanded environment variable and shell glob characters.
      string fullDirectory("");

      if (ifKey.length())
      {
                if ( !XSparse::addToLibraryPath(ifKey,fullDirectory,F_OK|R_OK|W_OK|X_OK))
                {

                        tcerr << "*** Warning: Local model directory\n*** " << ifKey
                              << "\n*** specified in Xspec.init  cannot be added to the load path."
                              << "\n*** most likely it has insufficient access permissions. "
                              << "\n*** The directory must exist and be writable\n";
                }
                m_defaultLocalModelDirectory = fullDirectory;
      }

      const string ONLINE("USE_ONLINE_HELP");
      ifKey = settings(ONLINE);
      if (ifKey.length())
      {
         if (XSutility::lowerCase(ifKey) == string("true"))
            m_useOnlineHelp = true;
      }
      const string LOCALFORMAT("LOCAL_HELP_FORMAT");
      ifKey = settings(LOCALFORMAT);
      if (ifKey.length())
      {
         // default to pdf
         if (XSutility::lowerCase(ifKey) == string("html"))
            m_localHelpFormat = Help::HTML;
      }

      const string PARALLEL("PARALLEL");
      ifKey = settings(PARALLEL);
      StringArray allArgs;
      XSparse::returnAllDelimitedArguments(ifKey, " ", allArgs);
      for (size_t i=0; i<allArgs.size(); i++) {
	if (i%2 == 1) {
	  StringArray parallelInput;
	  parallelInput.push_back("parallel");
	  parallelInput.push_back(allArgs[i-1]);
	  parallelInput.push_back(allArgs[i]);
	  XSGlobal::doParallel(parallelInput);
	}
      }

      const string PDF("PDF_COMMAND");
      ifKey = settings(PDF);
      if (ifKey.length())
      {
         Help::pdfCommand(ifKey);
      }
      const string HTML("HTML_COMMAND");
      ifKey = settings(HTML);
      if (ifKey.length())
      {
         Help::htmlCommand(ifKey);
      }
      
      ifKey = settings("FIT_DELTAS");
      Real fitDelta = .01;
      if (ifKey.length())
      {
         bool isOK = false;
         if (ifKey[0] == 'p' || ifKey[0] == 'P')
         {
            std::istringstream iss(ifKey);
            string dummy;
            iss >> dummy;
            Real tmpDelta = .0;
            if ((iss >> tmpDelta) && iss.eof())
            {
               fitDelta = tmpDelta;
               isOK = true;
            }
               
         }
         else if (ifKey[0] == 'f' || ifKey[0] == 'F')
         {
            // Using fixed deltas
            fitDelta = -1.0;
            isOK = true; 
         }
         if (!isOK)
         {
            tcerr <<"***Warning: Invalid setting in Xspec.init for FIT_DELTAS\n"
                  <<"            Will use default setting: proportional deltas = "
                  << fitDelta <<"\n\n";
         }         
      }
      XSContainer::models->proportionalDelta(fitDelta);
    }
    // End processSettings()

    DummyResponse* GlobalData::getDummyResponse ()
    {
          Real    eMin(0.5);
          Real    eMax(20.);
          size_t  nE(40);
          bool    ln(false); 
          string  key(DUMMY_RSP);
          string  line(globalData->settings(key));
          if (line.length() > 0)
          {
                  std::istringstream inputLine(line);

                  string  binning="";

                  try
                  {
                        inputLine >> eMin >> eMax >> nE >> binning;
                        ln = ((XSutility::lowerCase(binning)=="log") ? true : false);
                  }
                  catch (std::ios_base::failure&)
                  {
                        tcout << "*** Warning: Dummy Response settings are not readable, using defaults\n";
                        tcout << "eMin: " << eMin << " eMax: " << eMax << 
                                        " nE: " << nE << " ln: " << ln << std::endl;
                  } 


          }
          else
          {
                        tcout << "*** Warning: Dummy Response settings are not readable, using defaults\n";
                        tcout << "eMin: " << eMin << " eMax: " << eMax << 
                                        " nE: " << nE << " ln: " << ln << std::endl;
          }


          return DummyResponse::Instance(eMin,eMax,nE,ln);
    }

    int GlobalData::autoSave (int tclReturnValue, size_t emptyTrash)
    {
	if(emptyTrash)
	{
	    XSContainer::datasets->emptyTrash();
	    m_memento = 0;
	}

        ++m_commandCount;
        // the first condition is a test for "autosave off", but 
        // shouldn't really be necessary since if the first condition
        // is false the second condition is 'never' going to be true.
        if ( m_autoSaveFrequency != XSparse::NOTFOUND() &&
                     m_commandCount % m_autoSaveFrequency == 0)
        {
		copyFile(m_undoFile, m_autoSaveFile);
                XSGlobal::saveAll(m_autoSaveFile);
        }

        return tclReturnValue;
    }

    void GlobalData::copyFile (const string& dest, const string& source) const
    {
	using namespace std;

	struct stat source_info;

	fstream f_source(source.c_str(), ios::in);

	if(f_source)
	{
	    fstream f_dest(dest.c_str(), ios::out);

	    if(!f_dest)
	    {
		tcerr << "Error opening file: " << dest << endl;
		exit(1);
	    }

	    stat(source.c_str(), &source_info);

	    string buf;

	    bool wroteMementoID = false;

	    XSRegEx<TclRegEx> regex("^\\s*cd ");
	    XSRegEx<TclRegEx>::result_type matches;

	    while(!f_source.eof())
	    {	    
		getline(f_source, buf);

		if(regex.regex_search(buf, matches))
		{
		    //ditch leading white space
		    while(!f_source.eof() &&
			  string(" \t\r\n").find(f_source.get()) != string::npos);

		    if(!f_source.eof()) f_source.unget();

		    if(m_memento && XSutility::peek(f_source, 5) == "data ")
		    {
			//ditch the next line
			f_source.ignore(source_info.st_size, '\n');

			if(!wroteMementoID)
			{
			    f_dest << "data %MEMENTO\n";
			    wroteMementoID = true;
			}
		    }
		    else
			f_dest.write(string(buf + "\n").c_str(), buf.length() + 1);
		}
		else
		    f_dest.write(string(buf + "\n").c_str(), buf.length() + 1);
	    }

	    f_dest.close();
	}

	f_source.close();
    }

    bool GlobalData::checkInitVersion (std::ifstream& initFile)
    {
        bool isUpToDate = false;
        const int REQUIRED_INIT_VERS = 120901;
        if (initFile)
        {
           string firstLine;
           getline(initFile, firstLine);
           string::size_type iStart = 
                firstLine.find_first_not_of(" #\t");
           if (iStart != string::npos)
           {
              string::size_type len = string::npos;
              string::size_type iStop =
                   firstLine.find_first_of(" \t\n", iStart);
              if (iStop != string::npos)
                  len = iStop - iStart; 
              string firstEntry(firstLine.substr(iStart, len));
              std::istringstream ssTest(firstEntry);
              int vers=0;
              if ((ssTest >> vers) && ssTest.eof())
              {
                 if (vers >= REQUIRED_INIT_VERS)
                    isUpToDate = true;
              }
           }
           initFile.seekg(0);
        }
        return isUpToDate;
    }

    // Additional Declarations

} // namespace XSGlobal
