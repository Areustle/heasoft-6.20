//
//  XSPEC12  November 2003
//
//

#include <XSPlot/Plot/PlotDirector.h> 
#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h> 
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSContainer.h> 
#include <XSstreams.h> 
#include <xsTypes.h>
#include <sstream>

int
XSGlobal::xsSetplot(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doSetplot(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doSetplot(const StringArray& rawArgs)
{
   using XSContainer::plot;
   using std::map;
   enum SUBCOMS {HELP=0, ADD, BACKGROUND, CHANNEL, COMMAND, DELETE, DEVICE, ENERGY, GROUP,
                ID, LIST, NOADD, NOBACKGROUND, NOID, REBIN, REDSHIFT, SPLASHPAGE, UNGROUP, 
		 WAVE, XLOG, YLOG, AREA, NOAREA, CONTIMAGE, NOCONTIMAGE};
   const string cmdName("setplot");
   const size_t nArgs = rawArgs.size();
   string arg;

   static map<string, size_t> option;
   if (!option.size())
   {
      option["?"]            = HELP;
      option["add"]          = ADD;
      option["area"]         = AREA;
      option["background"]   = BACKGROUND;
      option["channel"]      = CHANNEL;
      option["command"]      = COMMAND;
      option["contimage"]    = CONTIMAGE;
      option["delete"]       = DELETE;
      option["device"]       = DEVICE;
      option["energy"]       = ENERGY;
      option["group"]        = GROUP;
      option["id"]           = ID;
      option["list"]         = LIST;
      option["noadd"]        = NOADD;
      option["noarea"]       = NOAREA;
      option["nobackground"] = NOBACKGROUND;
      option["nocontimage"]  = NOCONTIMAGE;
      option["noid"]         = NOID;
      option["rebin"]        = REBIN;
      option["redshift"]     = REDSHIFT;
      option["splashpage"]   = SPLASHPAGE;
      option["ungroup"]      = UNGROUP;
      option["wave"]         = WAVE;
      option["xlog"]         = XLOG;
      option["ylog"]         = YLOG;
   }


   if (nArgs == 1 || (arg = rawArgs[1]) == "?")
   {
       XSutility::printValidOptions(tcout, cmdName,option);
   } 
   else
   {
      try
      {
          PlotSettings& settings = plot->setplot();
//	  int subCommand = plot->spCommandVals(XSutility::lowerCase(arg));
          map<string,size_t>::const_iterator itOpt(option.lower_bound(arg));
          map<string,size_t>::const_iterator itOptEnd(option.end());
          size_t subCommand (0);
          if (itOpt != itOptEnd && itOpt->first.find(arg) == 0) subCommand = itOpt->second;

	  switch (subCommand)
	  {
	    case ADD: 
	       settings.setShowAddComponent(true);
	       break;
            case BACKGROUND:
               settings.setShowBackground(0,true);
               break;
	    case CHANNEL: 
	       settings.setXOption("channel");
	       break;
	    case COMMAND:
	       if (nArgs > 2)
	       {
	          string totCmd;
		  for (size_t i = 2; i < nArgs; ++i)
		  {
		     totCmd += rawArgs[i];
		     totCmd += " ";
		  }
	          int commandNum (settings.addUserCommand(totCmd));
                  Tcl_Obj* cmd (Tcl_NewIntObj(commandNum));
                  Tcl_SetObjResult(interp,cmd);
	       }
	       break;
	    case DELETE:
	       if (nArgs > 2) {
		 std::istringstream iss(rawArgs[2]);
		 int endCmd, startCmd;
		 size_t dash;
		 // First check for the string "all"
		 if (iss.str().find("all") == 0) {
		   endCmd = settings.numberUserCommands();
		   startCmd = 1;
		 } else if ((dash = iss.str().find("-")) !=string::npos){
		   // Then check for a range
		   std::stringstream subss;
		   subss << iss.str().substr(0,dash);
		   if (!(subss >> startCmd) || !subss.eof()) {
		     tcout << "***Error:  Command number must be an integer.\n";
		     break;
		   }
		   subss.clear();
		   subss << iss.str().substr(dash+1,string::npos);
		   if (!(subss >> endCmd) || !subss.eof()) {
		     tcout << "***Error:  Command number must be an integer.\n";
		     break;
		   }
		 } else {
		   // else parameter should be an int, and only an int.
		   if (!(iss >> endCmd) || !iss.eof()) {
		     tcout << "***Error:  Command number must be an integer.\n";
		     break;
		   }
		   startCmd = endCmd;
		 }
		 if ( startCmd != endCmd ) {
		   settings.removeUserCommandRange(startCmd,endCmd);
		 } else {
		   settings.removeUserCommand(startCmd);
		 }
	       } else {
		 tcout << "setplot delete requires either all, a command range or a command number\n";
		 tcout << "   enter \"setplot list\" for command list.\n";
	       }
	       break;
	    case DEVICE:
               XSGlobal::doCpd(rawArgs);
       	       break;
	    case ENERGY:
	       settings.setXOption("energy");
	       if (nArgs > 2) // Also select units
	       {
	          string unitStr(rawArgs[2]);
                  plot->selectUnits(unitStr,::ENERGY);
	       }
	       break;
	    case GROUP:
	       if (nArgs == 2)
	       {
		  tcout << "setplot group requires spectrum number arguments\n";
	       }
	       else
	       {
		  string allRanges(""); 
		  try
		  {
		     for (size_t i=2; i<nArgs; i++)
		     {
			string subRange(rawArgs[i]);
			int len = subRange.length();
			if (subRange[0] == ',' || subRange[len-1] == ',')
			{
			   std::string msg = "range " + subRange +
				   " contains invalid characters ";
			   throw XSparse::InvalidRange(msg);
			}
			for (int j=1; j<len-1; ++j)
			{
			   if (subRange[j] == ',')
			   {
			      subRange[j] = ' ';
			   }
			}
			allRanges += (subRange + " ");
		     }
		     settings.setPlotGroupNums(allRanges);
		  }
		  catch (XSparse::InvalidRange)
		  {
		  }
	       }
	       break;
	    case ID:
               {
                  std::vector<Real> values;
                  values.push_back(settings.temperature());
                  values.push_back(settings.emisLimit());
                  values.push_back(settings.redshiftLinesToObs());
                  values.push_back(settings.IDLowEnergy());
                  values.push_back(settings.IDHighEnergy());
                  StringArray tclInArgs;
                  StringArray params;
                  IntegerArray iParams;
                  for (size_t i=2; i<nArgs; ++i)
                  {
                     tclInArgs.push_back(rawArgs[i]);
                  }
                  XSparse::collectParams(tclInArgs, iParams, params);
                  for (size_t i=0; i<iParams.size(); ++i)
                  {
                     std::istringstream iss(params[i]);
                     Real tmpVal = 0.0;
                     switch (iParams[i])
                     {
                        case 0: // temperature
                           if (!(iss >> tmpVal) || !iss.eof() || tmpVal < 0.0)
                           {
                              tcout << "Invalid temperature value " << params[i] 
                                << " will be ignored." << std::endl;
                           }
                           else
                              values[0] = tmpVal;
                           break;
                        case 1: // emissivity limit
                           if (!(iss >> tmpVal) || !iss.eof() || tmpVal < 0.0)
                           {
                              tcout << "Invalid emissivity limit " << params[i] 
                                << " will be ignored." << std::endl;
                           }
                           else
                              values[1] = tmpVal;
                           break;
                        case 2: // redshift
                           if (!(iss >> tmpVal) || !iss.eof() || tmpVal <= -1.0)
                           {
                              tcout << "Invalid redshift value " << params[i] 
                                << " will be ignored." << std::endl;
                           }
                           else
                              values[2] = tmpVal;
                           break;
                        case 3: // low energy
                           if (!(iss >> tmpVal) || !iss.eof() || tmpVal < 0.0)
                           {
                              tcout << "Invalid low energy value " << params[i] 
                                << " will be ignored." << std::endl;
                           }
                           else
                              values[3] = tmpVal;
                           break;
                        case 4: // high energy
                           if (!(iss >> tmpVal) || !iss.eof() || tmpVal < 0.0)
                           {
                              tcout << "Invalid high energy value " << params[i] 
                                << " will be ignored." << std::endl;
                           }
                           else
                              values[4] = tmpVal;
                           break;
                        default:
                           break;
                     }
                  }
                  settings.setIDs(values);
               }
	       break;
	    case LIST:
	       settings.showUserCommands();
	       break;
	    case NOADD:
	       settings.setShowAddComponent(false);
	       break;
            case NOBACKGROUND:
               settings.setShowBackground(0,false);
               break;
	    case NOID:
	       settings.setShowLineIDs(0,false);
	       break;
	    case REBIN:
	       if (nArgs > 2)
	       {
		  bool allOk = true;
                  string errMsg;
                  PlotSettings::RebinInfo& rbLast = settings.lastRebinEntry().second;
                  Real sigmas = rbLast.sigma;
                  int maxBins = rbLast.maxBins;
                  int groupNum = settings.lastRebinEntry().first;
                  PlotSettings::PlotErrMode mode = rbLast.mode;
                  StringArray inArgs;
                  StringArray params;
                  IntegerArray iParams;
                  for (size_t i=2; i<nArgs; ++i)
                  {
                     inArgs.push_back(rawArgs[i]);
                  }
                  XSparse::collectParams(inArgs, iParams, params);
                  for (size_t i=0; i<iParams.size() && allOk; ++i)
                  {
                     std::istringstream iss(params[i]);
                     switch (iParams[i])
                     {
                        case 0:
                           if (!(iss >> sigmas) || !iss.eof() || sigmas < .0)
                           {
                              allOk = false;
                              errMsg = "min significance";
                           }
                           break;
                        case 1:
                           if (!(iss >> maxBins) || !iss.eof() || maxBins < 1)
                           {
                              allOk = false;
                              errMsg = "max bins";
                           }
                           break;
                        case 2:
                           if (!(iss >> groupNum) || !iss.eof() || 
                                (groupNum < 1 && groupNum != -1))
                           {
                              allOk = false;
                              errMsg = "plot group";
                           }
                           break;
                        case 3:
                           {
                              string errType(XSutility::lowerCase(iss.str()));
                              if (errType == "quad")
                              {
                                 mode = PlotSettings::STD;
                              }
                              else if (errType == "sqrt")
                              {
                                 mode = PlotSettings::ROOTN;
                              }
                              else if (errType == "poiss-1")
                              {
                                 mode = PlotSettings::GEHRELS1;
                              }
                              else if (errType == "poiss-2")
                              {
                                 mode = PlotSettings::GEHRELS2;
                              }
                              else if (errType == "poiss-3")
                              {
                                 mode = PlotSettings::GEHRELSM;
                              }
                              else
                              {
                                 allOk = false;
                                 errMsg = "error type";
                              }
                           }
                           break;
                        default:
                           break;
                     }
                  }

		  if (allOk)
		  {
		     settings.lastRebinEntry().first = groupNum;
		     rbLast.sigma = sigmas;
		     rbLast.maxBins = maxBins;
		     rbLast.mode = mode;
		     std::map<int, PlotSettings::RebinInfo>& rbMap = settings.groupsRebinInfo();
		     if (groupNum == -1)
		     {
			rbMap.clear();
			rbMap.insert(std::pair<int, PlotSettings::RebinInfo>(groupNum, rbLast));		
		     }
		     else
		     {
		        std::map<int, PlotSettings::RebinInfo>::iterator rbEntry = 
					rbMap.find(groupNum);
			if (rbEntry == rbMap.end())
			{
			   rbMap.insert(std::pair<int, PlotSettings::RebinInfo>
			   		(groupNum, rbLast));
			}
			else
			{
			   rbEntry->second = rbLast;
			}

		     }		     
		  }
		  else
		  {
                     if (errMsg == "error type")
                     {
                        tcout <<"***Error: Allowed error types are:\n"
                              <<"      quad | sqrt | poiss-1 | poiss-2 | poiss-3"<<std::endl;
                     }
                     else
                     {
		        tcout << "***Error: " << errMsg << " parameter is invalid." <<std::endl;
                     }
		  }
	       }
	       break;
            case REDSHIFT:
               if (nArgs > 2)
               {
                  Real z=0.0;
                  string strZ(rawArgs[2]);
                  std::istringstream iss(strZ);
                  if (!(iss >> z) || !iss.eof())
                  {
                     tcout << "***Error: Proper format is \"setplot redshift <z>\"\n"
                           << "          where -1.0 < z" << std::endl; 
                  }
                  else if (z <= -1.0)
                  {
                     tcout << "***Error: Valid redshift range is -1.0 < z" << std::endl;
                  }
                  else
                  {
                     settings.redshiftToSource(z);
                     tcout << "Spectra plots will be shifted to source frame by redshift value z = "
                          << z << std::endl;
                  } 
               }
               break;
            case SPLASHPAGE:
	       if (nArgs > 2)
	       {
		  string cmd = XSutility::lowerCase(rawArgs[2]);
		  if (cmd == "on")
		  {
		     settings.splashPage(true);
		  }
		  else if (cmd == "off")
		  {
		     settings.splashPage(false);
		  }
		  else 
		  {
		     tcout << "***Error: Allowed splashpage values are (on | off)"
			  	   << std::endl;
		  }
	       }
               break;
            case UNGROUP:
               settings.ungroupAll();
               break;
	    case WAVE:
	       settings.setXOption("wavelength");
	       if (nArgs > 2) // Also select units or set perHz flag.
	       {
	          const string unitStr(rawArgs[2]);
                  const string lcUnitStr(XSutility::lowerCase(unitStr));
                  // First check for perHz flag. 
                  const string PERHZ("perhz");
                  if (PERHZ.find(lcUnitStr) == 0)
                  {
                     bool turnOn = true;
                     if (nArgs > 3)
                     {
                        const string lcFlag(XSutility::lowerCase(rawArgs[3]));
                        if (lcFlag == "off")
                           turnOn = false;
                        else if (lcFlag == "on")
                           turnOn = true;
                        else
                        {
                           tcout <<"***Error: Invalid setplot wave perHz flag.  Proper usage is:\n"
                              << "     setplot wave perHz       ;Y-values plotted per Hz\n"
                              << "     setplot wave perHz on\n"
                              << "     setplot wave perHz off   ;Y-values plotted per length unit\n"
                              << std::endl;
                           throw YellowAlert();
                        }
                     }
                     settings.isWavePerHz(turnOn);
                  }
                  else
                     plot->selectUnits(unitStr,WAVELENGTH);
	       }
	       break;
	    case XLOG:
	       if (nArgs > 2)
	       {
		  string cmd = XSutility::lowerCase(rawArgs[2]);
		  if (cmd == "on")
		  {
		     settings.xLog(true);
		  }
		  else if (cmd == "off")
		  {
		     settings.xLog(false);
		  }
		  else 
		  {
		     tcout << "***Error: Allowed xLog values are (on | off)"
			  	   << std::endl;
		  }
	       }
	       break;
	    case YLOG:
	       if (nArgs > 2)
	       {
		  string cmd = XSutility::lowerCase(rawArgs[2]);
		  if (cmd == "on")
		  {
		     settings.yLog(true);
		  }
		  else if (cmd == "off")
		  {
		     settings.yLog(false);
		  }
		  else 
		  {
		     tcout << "***Error: Allowed yLog values are (on | off)"
			  	   << std::endl;
		  }
	       }
	       break;
            case AREA:
               settings.setDivideByArea(0,true);
               break;
            case NOAREA:
               settings.setDivideByArea(0,false);
               break;
            case CONTIMAGE:
               settings.contBackImage(true);
               break;
            case NOCONTIMAGE:
               settings.contBackImage(false);
               break;
	    default: 
	       tcout << "Subcommand " << arg  << " does not exist/is not implemented" <<std::endl;
               XSutility::printValidOptions(tcout, cmdName,option);
               // and print valid commands...
	       break; 
	 }	      
      }
      catch (...)
      {

      }
      tcout << std::flush;
   }

   return 0;
}
