/* script execution procedure, called by xs_unknown if the input starts with "@" */

#include <xsTypes.h> 
#include <XSUser/Global/XSGlobal.h>      
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <xstcl.h>
#include <XSContainer.h>
#include <iostream>
#include <stack>
#include <cstring>
#include <set> 

namespace
{
   int findNestedUserProcs(const string& fileName, std::set<string>& userProcs);
   bool findProcFromLineArgs(const StringArray& lineArgs, string& procName);
   bool isCallToScript(const StringArray& lineArgs, string& fileName);
}

namespace xstcl 
{
   int xs_noecho_command = 0;
}

extern "C" int
xs_execute_script(Tcl_Interp* xsInterp, char* command)
{
    using namespace std;
    using namespace xstcl;
    string origString(++command);
    string cmdString;
    if (origString.length())
    {
       // This will allow user to use "~" syntax for home directories.
       cmdString = XSparse::expandDirectoryPath(origString);
       if (!cmdString.length())
       {
          cerr <<"Unable to open script file " << origString << endl;
          return TCL_ERROR;
       }
    }
    else
    {
       cerr <<"***Error: No name given for script file following the \'@\'"
          << endl;
       return TCL_ERROR;
    }

    string fname(XSparse::returnDelimitedArgument(cmdString," \n"));
    int suffix = fname.find_first_of(".");

    if (suffix < 0) fname += ".xcm";


    /* open the file  and give Tcl the commands in chunks that it can interpret
     * tcl nesting needs to be treated as well as xspec commands, where 
     * line breaks must be replaced by an alternative delimiter. This is
     * treated in the returnCommand function
     */


    //  Create a command trace which will echo commands to the output 
    //  if from a script, or to a log file. 

    ifstream script;
    script.open(fname.c_str());

    if (!script)
    {
	cerr << "Error opening script file " << fname << endl;
	return TCL_ERROR;
    }
    else
    {

	bool leaveTraceOpen (false);

	if (XS_Echo_Trace)
	{
	    leaveTraceOpen = true; 
	}
	else
	{
	    XS_Echo_Trace = Tcl_CreateObjTrace (xsInterp, XS_MAX_LEV, 0, 
						xs_echo_command, (ClientData)0, 
						(Tcl_CmdObjTraceDeleteProc*)0);
	}
	XSparse::executingScript(true);

	string strFileContents;

        // We need to keep track of user-defined tcl procedures for one reason
        // only:  When searching for the end of a multi-line command, we have to
        // know if a seemingly random string is actually a user-defined proc name
        // that hasn't yet been sourced.  (If it were already sourced, it will
        // show up during the "info command" test.)  This means we also must
        // check for proc definitions in nested scripts.
        std::set<string> userProcs;

	while (script)
	{

	    string buffer;
	    unsigned int location = 0;

	    /* returns NULL on an end of file condition */    
	    location = script.tellg();
	    buffer = XSparse::getNextLine(script);

	    if (buffer.size() != 0)
	    {
	       returnCommand(script, xsInterp, buffer, &location, userProcs);
	       strFileContents += buffer + '\n';
	    }
	}

	Tcl_Eval(xsInterp, const_cast<char*>(strFileContents.c_str()));

	script.close();

	if ( !leaveTraceOpen )
	{
	    XSparse::executingScript(false);
	    Tcl_DeleteTrace(xsInterp,XS_Echo_Trace);
	    XS_Echo_Trace = 0;   
	}

    }
    return TCL_OK;
}


void 
xstcl::returnCommand(std::ifstream& stream, Tcl_Interp* xsInterp, std::string& buf, 
                unsigned int* location, std::set<string>& userProcs)
{
    using namespace std;

    // This function assumes buf has already been checked to have finite
    // length, though might just be whitespace.
    string subBuffer("");        
    static set<string> multiLineCommands;
    if (multiLineCommands.empty())
    {
       multiLineCommands.insert("model");
       multiLineCommands.insert("editmod");
       multiLineCommands.insert("addcomp");
       multiLineCommands.insert("newpar");
       multiLineCommands.insert("fakeit");
       multiLineCommands.insert("gain");
       multiLineCommands.insert("rmodel");
       multiLineCommands.insert("rnewpar");
    }

    //  deal with multi-line commands. If the current command is an interactive command
    //  as enumerated above, read the next lines following until we find a line that
    //  is a real command. If it is, back the file pointer up one line and start again.

    bool foundMulti = false;

    StringArray inArgs;
    XSparse::collateByWhitespace(inArgs, buf);
    if (inArgs.size())
    {
       string procName;
       string scriptName;
       if (findProcFromLineArgs(inArgs, procName))
       {
          userProcs.insert(procName);
       }
       else if (isCallToScript(inArgs, scriptName))
       {
          findNestedUserProcs(scriptName, userProcs);
       }
       else
       {
          string firstArg = XSutility::lowerCase(inArgs[0]); 
          std::map<string,Tcl_ObjCmdProc*>::const_iterator itClosest = 
                  XSGlobal::commandMap.lower_bound(firstArg);
          if (itClosest != XSGlobal::commandMap.end())
          {
             if (itClosest->first.find(firstArg) == 0)
             {
                set<string>::const_iterator itMulti = multiLineCommands.find(itClosest->first);
                if (itMulti != multiLineCommands.end())
                {
                   foundMulti = true;
                   // Still not home free.  If this is a gain command, it's only
                   // multi if it's "gain fit" mode.
                   if (*itMulti == "gain")
                   {
                      if (inArgs.size() < 2 || XSutility::lowerCase(inArgs[1]) != "fit")
                         foundMulti = false;
                   }
                }
             }
          }
       }
       if(foundMulti)
       {
	   bool done = false, isCmd = true;

	   do
	   {
	       //might have to add 1 for ending line feed
	       *location = stream.tellg();

	       if(stream)
	       {
		   subBuffer = XSparse::getNextLine(stream);

		   //stop on a command or a line where the first char is a '}'
		   if( (isCmd = isCommand(interp, subBuffer.c_str(), userProcs)) ) 
                   {
                      done = true;
		      stream.seekg(*location);
                   }
		   else
		       buf += " \n " + subBuffer;
	       }
	       else
		   done = true;
	   }
	   while(!isCmd && !done);
	   buf.resize(buf.find_last_not_of(" \t\n") + 1);

           // change all the linefeeds apart from the last into "&".
           // this will avoid tcl seeing a command terminator until
           // the end of the multiple line command.

           char oldChar('\n');
           string newStr(" & ");
           string::size_type iCur = 0;
           while (iCur != string::npos)
           {
              iCur = buf.find(oldChar, iCur);
              if (iCur != string::npos)
              {
                 buf.replace(iCur, 1, newStr);
              }
           }
       } // end if foundMulti
    } // end if not just whitespace
}

/* Routine to check script lines for command-hood, and 	add noncommands to the
 * current command string, while creating new command lines for commands. 
 * This is achieved by issuing the tcl command "info commands <token>*"
 * where <token> is the first space (or newline)token in the input string.
*/

int 
xstcl::isCommand (Tcl_Interp* xsInterp, const char* s, std::set<string>& userProcs)
{

   int value = 0;
   string tempStr(s);
   // strip leading blanks
   const string whiteSpace(" \n\t");
   string::size_type start = tempStr.find_first_not_of(whiteSpace);
   if (start != string::npos)
   {
      // intercept "/" as not-command, even though it actually is (xs_null),
      // because when in a script, it is part of a preceding command. 
      if (tempStr[start] == '/' ) return 0;

      // Intercept "}" as a new command, since a line starting with this
      // can only mean end of a tcl loop.  It can't mean continuation of an
      // xspec command.
      if (tempStr[start] == '}') return 1;

      // Assume '@' means a nested script.  The case of "source" will be
      // handled below since it shows up under tcl's "info commands".
      if (tempStr[start] == '@') return 1;

      // Need to check for case of 1-char string of 'n', 'N', 'y', 'Y'.
      // These should be treated as user answers to query, and not as
      // commands.  Otherwise, "n*" would be identified as xspec
      // or tcl command.
      if (start == tempStr.length()-1 || whiteSpace.find(tempStr[start+1]) != string::npos)
      {
         const char c(tempStr[start]);
         if (c == 'n' || c == 'N' || c == 'y' || c == 'Y') return 0;
      }

      string::size_type stop = tempStr.find_first_of(whiteSpace, start);
      string::size_type nChars = (stop == string::npos) ? 
                string::npos : stop - start;
      string strtmp(tempStr, start, nChars);

      // If a '$' exists anywhere in the word, it is a variable 
      // substitution and clearly not a command.  This will fail
      // the simple Tcl_Eval test below because it will return an
      // error string.  Therefore intercept it now.
      if (strtmp.find('$') != string::npos) return 0;

      // Perhaps it is a user defined proc not yet inserted into Tcl.
      std::set<string>::const_iterator itProcs = userProcs.lower_bound(strtmp);
      if (itProcs != userProcs.end())
      {
         if (itProcs->find(strtmp) == 0)
            return 1;
      }

      string testString("info commands ");
      // don't echo results to log in this routine 
      xs_noecho_command = 1;

      testString += strtmp;
      testString += "*";
      Tcl_Eval(xsInterp, testString.c_str());
      if (strlen(Tcl_GetStringResult(xsInterp)) > 0) value = 1;

      xs_noecho_command = 0;
   }
   return value;
} // end isCommand


namespace {

int findNestedUserProcs(const string& fileName, std::set<string>& userProcs)
{
   using namespace std;

   int status = TCL_OK;
   ifstream script(fileName.c_str());
   if (!script)
   {
      cerr << "Error opening script file " << fileName << endl;
      status = TCL_ERROR;
   }
   while (script && status == TCL_OK)
   {
      string buffer(XSparse::getNextLine(script));
      if (buffer.size())
      {
         StringArray lineArgs;
         XSparse::collateByWhitespace(lineArgs, buffer);
         string procName;
         string scriptName;
         if (findProcFromLineArgs(lineArgs, procName))
            userProcs.insert(procName);
         else if (isCallToScript(lineArgs, scriptName))
         {
            // Recursive call
            status = findNestedUserProcs(scriptName, userProcs);
         }
      }
   }

   return status;
}

bool findProcFromLineArgs(const StringArray& lineArgs, string& procName)
{
   bool isFound = false;
   if (lineArgs.size() > 1 && lineArgs[0] == "proc")
   {
      procName = lineArgs[1];
      isFound = true;
   }
   return isFound;
}

bool isCallToScript(const StringArray& lineArgs, string& fileName)
{
   bool isScript = false;
   if (lineArgs.size())
   {
      const string& firstArg = lineArgs[0];
      if (firstArg.size() > 1 && firstArg[0] == '@')
      {
         fileName = firstArg.substr(1);
         isScript = true;
      }
      else if (lineArgs.size() > 1 && lineArgs[0] == "source")
      {
         fileName = lineArgs[1];
         isScript = true;
      }
   }   
   return isScript;
}

}
