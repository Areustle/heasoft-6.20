//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <HandlerUtils.h>
#include <fstream>
#include <sstream>

int
XSGlobal::xsSave(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doSave(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doSave(const StringArray& rawArgs)
{
    using XSContainer::models;
    using XSContainer::fit;
    using namespace std;

    static const string DEFAULTFILE("savexspec.xcm");
    static const string XCM(".xcm");
    int option(0);
    std::vector<string> allowedOptions(3);
    allowedOptions[0] = "model";
    allowedOptions[1] = "files";
    allowedOptions[2] = "all";


    const char* cmd = "save";
    const size_t nArgs = rawArgs.size();
    string fileName;


    try
    {
	if (nArgs == 1)
	{
	    fileName = DEFAULTFILE;
	    tcout << "Saving model definition to file savexspec.xcm" << endl;

	    if(HandlerUtils::fileExists(fileName))
	    {
                std::ostringstream promptStr;
		promptStr << fileName << " exists. Replace? (y/n) ";

                if (XSutility::yesToQuestion(promptStr.str(), 0, tcin)<=0)    
		    return -1;
	    }

            std::ofstream outputFile(fileName.c_str());
            if (!outputFile)
            {
               string errMsg("Unable to open file: ");
               errMsg += fileName + '\n';
               throw YellowAlert(errMsg);
            }
	    saveModel(outputFile);
	    return 0;
	}
	else
	{
	    string arg(rawArgs[1]);
	    if (arg == "?")
	    {
		printDocs(cmd,"?");
		return 0;
	    }
	    else if (nArgs >= 2)
	    {
		int n = allowedOptions.size();
		while (option  < n )
		{
		    if ( allowedOptions[option].compare(0,
			   std::min(arg.length(),allowedOptions[option].length()),arg) == 0) break;
		    ++option;
		}

		if ( option == n )
		{
		    printDocs(cmd,"?");
		    return -1;
		}
		else
		{
		    if (nArgs > 2 )
		    {
			fileName = rawArgs[2];
                        // If name has no extension, give it ".xcm".
                        string::size_type dotLoc = fileName.rfind('.');
                        if (dotLoc != string::npos)
                        {
                           // fileName may include path, which means it may 
                           // contain relative specifiers like ./, /../, or 
                           // dir names with an '.'.  So can't assume 
                           // finding a '.' means it has an extension.
                           string::size_type slashLoc = fileName.rfind('/');
                           if (slashLoc != string::npos && (dotLoc < slashLoc))
                           {
                              fileName += XCM;
                           }
                        }
                        else                        
                           fileName += XCM;
		    }
		    else
		    {
			tcout << "Saving to file savexspec.xcm" << endl;
			fileName = DEFAULTFILE;
		    }
		    // the reason for instantiating the stream
		    // separately in each option is that saveAll
		    // must be "atomic" for use by autosave.

		    switch (option)
		    {
			// note that the file is closed
			// automatically at the end of 
			// the scope.
			// model
		    default:
		    case 0: 
                      {
			if(HandlerUtils::fileExists(fileName))
			{
                            std::ostringstream promptStr;
			    promptStr << fileName << " exists. Replace? (y/n) ";

			    if(XSutility::yesToQuestion(promptStr.str(), 0, tcin)<=0)
			       return -1;
			}
                        std::ofstream outputFile(fileName.c_str());
                        if (!outputFile)
                        {
                           string errMsg("Unable to open file: ");
                           errMsg += fileName + '\n';
                           throw YellowAlert(errMsg);
                        }
			saveModel(outputFile);
                      }
		      break;
			// files
		    case 1:
                      { 
			if(HandlerUtils::fileExists(fileName))
			{
			    std::ostringstream promptStr;
			    promptStr << fileName << " exists. Replace? (y/n) ";

			    if(XSutility::yesToQuestion(promptStr.str(), 0, tcin)<=0)
			       return -1;
			}
                        std::ofstream outputFile(fileName.c_str());
                        if (!outputFile)
                        {
                           string errMsg("Unable to open file: ");
                           errMsg += fileName + '\n';
                           throw YellowAlert(errMsg);
                        }
			saveData(outputFile);

			// the gain setting would
			// go here.
                      }
		      break;
			// all   - both 0 and 1.
		    case 2:
                      {
			ifstream in(fileName.c_str());
			if(in)
			{
			    in.close();

                            std::ostringstream promptStr;
			    promptStr << fileName << " exists. Replace? (y/n) ";

			    if(XSutility::yesToQuestion(promptStr.str(), 0, tcin)<=0)
                                return -1;
                            else
				saveAll(fileName);
			}
			else
			    saveAll(fileName);
                      }
	              break;  
		    }
		}
	    }    
	    return 0;
	}
    }
    catch (YellowAlert&)
    {
	return -1;
    }

}
