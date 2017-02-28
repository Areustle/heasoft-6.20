//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
//	/local/data/veris2/dorman/cpp/xspec_cxx/userint

#       include <cstdlib>
#include <iostream> // for debugging purposes.
        using namespace std;
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <xstcl.h>





using namespace xstcl;

Tcl_Channel XS_Script_Chan;

/*
 * xs_echo_command --
 * Routine to write the entered commands to the output if running a script,
 * or to the log file if logging data.
 */
//int
//xstcl::xs_echo_command (ClientData cdata,Tcl_Interp *xsInterp,int level,char* command,
//        Tcl_CmdProc *cmdproc, ClientData cmddata,int argc,char** argv)
int 
xstcl::xs_echo_command (ClientData cdata,Tcl_Interp *xsInterp,int level, const char* command, 
        Tcl_Command cmdproc, int objc, Tcl_Obj *const objv[] )
{

  int true_lev(-1);
  static char nl[] = "\n";
  char* stmp(0);
  Tcl_Channel logChannel;
  int argcm=0;
  char **argvm = 0;
  static int unknown_level = XS_MAX_LEV;
  const string logPrompt("\n!" + string(xstcl::xs_tcl_prompt));

  string firstArg(Tcl_GetString(objv[0]));

  string cmdStr(command);

  static char tcp1[] = "tcl_prompt1";
  char* tcprompt (tcp1);
  stmp = const_cast<char*>(Tcl_GetVar(xsInterp,tcprompt, TCL_LEAVE_ERR_MSG | TCL_GLOBAL_ONLY));

  if  (xs_noecho_command 
                  || string(firstArg) ==  "tclunknown" 
                  || string(firstArg) == "namespace" ) return TCL_OK;


  /*
   *
   * Supress echoing of the command for setting the prompt.
   * currently with the use of tcl-readline the prompt string variable
   * is different depending on whether we are in GUI mode or not.
   *
 */
 // don't echo command to print the prompt.
  if (!stmp || cmdStr == stmp) return TCL_OK;
  // don't  echo command to turn off echoing!
  if ( cmdStr ==  string("set xs_echo_script 0") ) return TCL_OK;
  // don't echo "history add" commands.
  if ( (string(firstArg) == "history")  && (objc > 1 && string(Tcl_GetString(objv[1])) == "add")) 
  {
            return TCL_OK;
  }




 /*
   * Keep track of whether or not we are in the unknown proc.
   * Do this by storing it's level.
   */
  if( level <= unknown_level ) unknown_level = XS_MAX_LEV + 1;

  // if this hasn't     (a) been called  in the "unknown" procedure
  //                    (b) the command is not a script execution request
  //                    (c) unknown_level has been set 
  if( (string(Tcl_GetString(objv[0])) ==  "unknown") 
        && *command != '@'  
        && unknown_level == XS_MAX_LEV + 1 )
  {
          unknown_level = level;
  }

  int saveEcho(xs_noecho_command);
  xs_noecho_command = 1;
  /*
   * Run TCL commands to find out what the execution level is for this
   * command. executingScript flag is set in xs_execute_script
   */

  char infolev[] = "info level";

  int result(Tcl_VarEval(xsInterp, infolev , 0));
  if( result == TCL_OK ) sscanf(Tcl_GetStringResult(xsInterp), "%d", &true_lev);

  xs_noecho_command = saveEcho;


  /*
   * See if we need to echo a command to a log file, or the terminal,
   * or both.
   */
  bool scriptExec(XSparse::executingScript());

//  if (level <= unknown_level && true_lev == 0 &&
//   ( scriptExec && xs_echo_script || Tcl_GetChannel(xsInterp, xsLogOut, 0))  )
  if (level <= unknown_level && true_lev == 0 && 
      ( (xs_echo_script && scriptExec) || Tcl_GetChannel(xsInterp, xsLogOut, 0))  )
  {

        if (scriptExec && xs_echo_script) 
	{
		logChannel = Tcl_GetStdChannel(TCL_STDOUT);
	}
	else
	{
		logChannel = XS_Logfile_Chan;
	}
        /*
        * Print the prompt, preceded by a `!', if we are executing a
        * script
        */

        if (scriptExec)
           Tcl_Write(logChannel, logPrompt.c_str(), -1); 

        // Precede command by `level' spaces, to indicate what the level is.

        string logCmd(command);
        //logCmd += XS_PROMPT;
  	//logCmd += string(level,' ');
        //logCmd += string(command);      
        //If the command string is longer than 80 characters, truncate it.
        //Replace any newline characters with spaces.
        //logCmd = logCmd.substr(0,std::min(LEN-5,logCmd.size()));
        //if (logCmd.size() == LEN - 5)
        //{
        //        logCmd += string(trunc);       
        //}        

        int searchChar = logCmd.find_first_of('&');

	// for scripting files, replace the "&" used as a line 
        // delimiter with newline characters */
	while (searchChar  > 0)
	{
                logCmd[searchChar ] = '\n';

                searchChar  = logCmd.find_first_of('&',++searchChar );
	}

        /*
        * Replace any newline characters by semicolons.
        * BD 9/01 -don't see why you need to do this unless
        * we are writing out a script - will switch off and revisit
        * when implementing script command.
        */
        if (xs_echo_script)
        {
                static char NL('\n');
                searchChar  = logCmd.find_first_of(NL);
	        while (searchChar  > 0)
	        {
                        logCmd[searchChar] = ';';

                        searchChar = logCmd.find_first_of(NL,++searchChar );
	        }
        }
        logCmd += "\n";


        if( Tcl_Write(logChannel, const_cast<char*>(logCmd.c_str()), -1) < 1 )
        {
              static char errMsg[] = "Error writing command to log file.";
              Tcl_AppendResult(xsInterp, errMsg,0);
        }

        Tcl_SetVar(xsInterp,xsTclPrompt,xs_tcl_prompt,0);

   }
   else Tcl_ResetResult(xsInterp);


  /*
   * See if we need to write the command to a script file.
   */

  if( xsScriptFile.length() != 0 && true_lev == 0 && level <= 1 &&
      (unknown_level == 1 || unknown_level == XS_MAX_LEV + 1) )
  {

          //
          // If necessary, try and do command name completion
          //
          if( unknown_level == 1 && TCL_OK ==
              Tcl_VarEval(xsInterp, "info commands ", Tcl_GetString(objv[1]), "*", (char *)0) )
          {

                  if( Tcl_SplitList((Tcl_Interp *)0, 
                        Tcl_GetStringResult(xsInterp), &argcm, const_cast<const char***>(&argvm))
                        == TCL_OK )
                  {
                        //
                        // if one and only one match was found, insert it into the
                        // command that gets printed.
                        //
                        char* fixedCommand (0);
                        if( argcm == 1 )
                        {
                                fixedCommand = Tcl_GetString(objv[1]);
                        }
                        else
                        {
                                fixedCommand = Tcl_GetString(Tcl_ConcatObj(objc-1,&objv[1])); 
                        }

                        if( Tcl_Write(XS_Script_Chan, fixedCommand, -1) < 1 )
                        {
                                Tcl_AppendResult(xsInterp, 
                                        "Error writing command to script file.",(char *)0);
                        }
                        free(argvm);
                  }

            }
            else
            {
                if ( Tcl_Write(XS_Script_Chan, command, -1) < 1 )
                {
                    Tcl_AppendResult(xsInterp, "Error writing command to script file.",(char *)0);
                }
            }

            if ( Tcl_Write(XS_Script_Chan,nl, -1) < 1 )
            {
                Tcl_AppendResult(xsInterp, "Error writing command to script file.",(char *)0);
            }

  }
  else Tcl_ResetResult(xsInterp);

  return TCL_OK;
}
