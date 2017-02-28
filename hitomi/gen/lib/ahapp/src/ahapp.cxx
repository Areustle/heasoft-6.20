/// \brief Members and functions related to the general tool layout
/// \author James Peachey
/// \date $Date: 2016/04/07 19:37:41 $

#define AHLABEL ahapp_ahapp
#define AHCVSID "$Id: ahapp.cxx,v 1.33 2016/04/07 19:37:41 mwitthoe Exp $"

#include "ahapp/ahapp.h"
#include "ahfits/ahfits.h"
#include "ahgen/ahgen.h"
#include "ahlog/cahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "ape/ape_msg.h"
#include "ape/ape_util.h"
#include "pil.h"

#include "headas_utils.h"

#include <cstdlib>
#include <stdexcept>
#include <string>
#include <string.h>
#include <sstream>

namespace ahapp {

/// \brief (internal to ahapp) Form error message when problem occurs in 
///  getPar* functions
/// \param[in] parname parameter name
/// \param[in] status ape status value
std::string reportGetParErr(const std::string & parname, int status);

// Internal functions to connect ape message streams to ahlog.
static void ape_out_redirect(const char* msg);
static void ape_err_redirect(const char* msg);

int startUp(int argc, char ** argv, const std::string& tooltag) {
  std::string banner(__func__); banner += ": ";
  int status = ape_trad_init(argc, argv);
  if (eOK != status) {
    // TODO: report status code along with message.
    std::cerr << banner << "ape_trad_init returned non-0 status" << std::endl;
    return status;
  }

  // Disable all prompting if HEADASNOQUERY environment variable is set.
  if (0 != getenv("HEADASNOQUERY")) PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

  set_history(1);

  // attempt to read chatter from parameter file
  // ... if chatter out of range, adjust to appropriate limit
  // ... if other error, set chatter to max value
  int chatter=0;
  std::string chatter_warn_msg="";
  status = ape_trad_query_int("chatter", &chatter);
  if (eValueBelowMin == status) {
    chatter=ahlog::MINCHAT;
    std::stringstream msg;
    msg << " *** Chatter too small; resetting to minimum value of " << ahlog::MINCHAT;
    chatter_warn_msg=msg.str();
  } else if (eValueAboveMax == status) {
    chatter=ahlog::MAXCHAT;
    std::stringstream msg;
    msg << " *** Chatter too large; resetting to maximum value of " << ahlog::MAXCHAT;
    chatter_warn_msg=msg.str();
  } else if (eOK != status) {
    chatter=ahlog::MAXCHAT;
    std::stringstream msg;
    msg << " *** Invalid chatter value; resetting to maximum value of " << ahlog::MAXCHAT;
    chatter_warn_msg=msg.str();
  }
  status=0;    // reset ape status

  // attempt to read log file name
  // ... if ape error, set log file name to !DEFAULT
  char* logfilepar = 0;
  const char* logfile = 0;
  std::string logfile_warn_msg="";
  status = ape_trad_query_string("logfile",&logfilepar);
  if (eOK == status && strlen(logfilepar) > 0) {
    logfile = logfilepar;
  } else {
    logfile = "!DEFAULT";
    logfile_warn_msg=" *** APE error reading logfile parameter; setting logfile to !DEFAULT";
  }
  status=0;    // reset ape status

  // attempt to read debug parameter
  // ... if ape error, set debug parameter to false
  char c_debug = 0;
  bool debug=false;
  std::string debug_warn_msg="";
  status = ape_trad_query_bool("debug", &c_debug);
  if (eOK != status) {
    debug=false;
    debug_warn_msg=" *** APE error reading debug parameter; setting debug to false";
  } else {
    debug = 0 != c_debug ? true : false;
  }
  status=0;    // reset ape status

  // set up ahlog as soon as possible to have as many messages sent through ahlog
  status = ahlog_setup(argv[0], logfile, chatter, debug);
  if (0 != status) {
    free(logfilepar); logfilepar=0;
    return status;
  }

  // display any necessary warning messages
  if (!chatter_warn_msg.empty()) AH_INFO(ahlog::LOW) << chatter_warn_msg << std::endl;
  if (!logfile_warn_msg.empty()) AH_INFO(ahlog::LOW) << logfile_warn_msg << std::endl;
  if (!debug_warn_msg.empty()) AH_INFO(ahlog::LOW) << debug_warn_msg << std::endl;

  // feed APE error messages through ahlog
  ape_msg_set_out_handler(&ape_out_redirect);
  ape_msg_set_err_handler(&ape_err_redirect);

  // attempt to read clobber parameter
  //  ... if ape error, set clobber to false
  char clobber = 0;
  bool b_clobber=false;
  status = ape_trad_query_bool("clobber", &clobber);
  if (eOK != status) {
    clobber=false;
    AH_INFO(ahlog::LOW) << " *** APE error reading clobber parameter; setting clobber to false" << std::endl;
  } else {
    b_clobber = 0 != clobber ? true : false;
  }
  status=0;
  ahgen::setClobber(b_clobber);

  // attempt to read buffer parameter
  //  ... if buffer < -1; set to buffer=-1
  //  ... if ape error; set buffer=-1
  int buffer = -1;    // default is automatic buffering
  status = ape_trad_query_int("buffer", &buffer);
  if (eOK != status) {
    buffer=-1;
    AH_INFO(ahlog::LOW) << " *** APE error reading buffer parameter; setting buffer to -1 (automatic buffering)" << std::endl;
  } else if (buffer < -1) {
    buffer=-1;
    AH_INFO(ahlog::LOW) << " *** buffer parameter set smaller than -1; resetting buffer to -1 (automatic buffering)" << std::endl;
  }
  status=0;
  ahgen::setBuffer(buffer);

  // attempt to read history parameter which enables/disables parameter stamping
  char c_history = 0;
  bool history=true;
  status = ape_trad_query_bool("history", &c_history);
  if (eOK != status) {
    history=true;
    AH_INFO(ahlog::LOW) << " *** APE error reading history parameter; setting history to true" << std::endl;
  } else {
    history = 0 != c_history ? true : false;
  }
  status=0;
  ahgen::setHistory(history);

  // make sure that stamped parameters match those used; this is necessary since
  // in case ape errors above cause a hard-coded default value to be used
  ape_trad_set_int("chatter",chatter);
  ape_trad_set_string("logfile",logfile);
  ape_trad_set_bool("debug",debug);
  ape_trad_set_bool("clobber",clobber);
  ape_trad_set_int("buffer",buffer);
  ape_trad_set_bool("history",history);

  // Free the logfile parameter, if indeed it was allocated.
  free(logfilepar); logfilepar=0;

  // set tool name used when stamping parameters
  std::string tname=argv[0];
  size_t found;
  found=tname.rfind("/");
  if (found != std::string::npos) tname.replace(0,found+1,"");
  set_toolname(tname.c_str());

  // set tool version
  // the tool version is typically set using a macro set as the CVS macro,
  // Tag, so that the tooltag argument to this function will take the form:
  // $Tag: AstroH_B02$.  Before setting the tool version, we want to strip
  // the CVS formatting, namely "$Tag: " at the beginning, and "$" at the
  // end.
  std::string ttooltag=tooltag;
  std::string tagstr="$Name: ";
  // note: in the following search, the leading $ is ommitted to prevent CVS from expanding the keyword
  if (ttooltag.find("Name$") != std::string::npos) ttooltag="";     // check if no CVS flag set yet
  if (ttooltag[ttooltag.size()-1] == '$')
    ttooltag.erase(ttooltag.end()-1,ttooltag.end());
  if (ttooltag.find(tagstr) != std::string::npos)
    ttooltag.replace(ttooltag.find(tagstr),tagstr.size(),"");
  set_toolversion(ttooltag.c_str());

  // set up ahfits
  ahfits::setClobber(clobber);
  ahfits::setBuffer(buffer);
  ahfits::setHistory(history);

  return status;
}

int shutDown(void) {
  int status = ape_trad_close(1);
  ahlog::shutdown();
  return status;
}

static int s_status = 0;

int getStatus(void) { return s_status; }

int setStatus(int status) { if (0 == s_status) s_status = status; return s_status; }

void resetStatus(void) { s_status = 0; }

std::string reportGetParErr(const std::string & parname, int status) {
  std::stringstream msg;
  msg << "problem getting parameter: " << parname << " (APE status: " 
      << status << ")";
  return msg.str();
}

std::string getParString(const std::string & parname) {
  char* t_par = 0;
  if (int status = ape_trad_query_string(parname.c_str(),&t_par) != eOK ) {
    free(t_par); t_par = 0;
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  }
  std::string out=(std::string)t_par;
  free(t_par); t_par=0;
  return out;
}

std::string getParString_warn(const std::string& parname, bool & okay) {
  std::string out="";
  try {
    out=ahapp::getParString(parname);
  } catch (const std::exception &x) {
    okay=false;
    out="";
    AH_INFO(ahlog::HIGH) << " *** " << x.what() << std::endl;
  }
  return out;
}

bool getParBool(std::string parname) {
  char t_par;
  if (int status = ape_trad_query_bool(parname.c_str(),&t_par) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  if (t_par != 0) return true;
  return false;
}

bool getParBool_warn(const std::string& parname, bool & okay) {
  bool out=false;
  try {
    out=ahapp::getParBool(parname);
  } catch (const std::exception &x) {
    okay=false;
    out=false;
    AH_INFO(ahlog::HIGH) << " *** " << x.what() << std::endl;
  }
  return out;
}

double getParDouble(std::string parname) {
  double out;
  if (int status = ape_trad_query_double(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}

double getParDouble_warn(const std::string& parname, bool & okay) {
  double out=0.0;
  try {
    out=ahapp::getParDouble(parname);
  } catch (const std::exception &x) {
    okay=false;
    out=0.0;
    AH_INFO(ahlog::HIGH) << " *** " << x.what() << std::endl;
  }
  return out;
}

int getParInt(std::string parname) {
  int out;
  if (int status = ape_trad_query_int(parname.c_str(),&out) != eOK)
    AH_THROW_RUNTIME(reportGetParErr(parname,status));
  return out;
}

int getParInt_warn(const std::string& parname, bool & okay) {
  int out=0;
  try {
    out=ahapp::getParInt(parname);
  } catch (const std::exception &x) {
    okay=false;
    out=0;
    AH_INFO(ahlog::HIGH) << " *** " << x.what() << std::endl;
  }
  return out;
}

static void ape_out_redirect(const char* msg) {
  AH_OUT << msg;
}

static void ape_err_redirect(const char* msg) {
  AH_INFO(ahlog::LOW) << " *** " << msg;
}

void writeParametersToLog() {
  char ** par_names = 0;
  int ii = 0;
  std::stringstream out;

  // start output line with name of tool
  char toolname[128];          // 128 is the size used in headas_toolname.c
  get_toolname(toolname);
  out << toolname;

  // write parameters to output line
  ape_trad_get_par_names(&par_names);
  while(par_names[ii] != NULL){
    char* value=0;
    ape_trad_get_string(par_names[ii],&value);
    out << " '" << par_names[ii] << "=" << value << "'";
    free(value);
    ii++;
  }

  // Write parameter list to log file
  AH_INFO(ahlog::HIGH) << "START PARAMETER LIST:" << std::endl;
  AH_INFO(ahlog::HIGH) << out.str() << std::endl;
  AH_INFO(ahlog::HIGH) << "END PARAMETER LIST" << std::endl << std::endl;

  // Clean up parameter list array
  ape_util_free_string_array(par_names);

} // end of writeParametersToLog
 

} // namespace ahapp

/* Revision Log
 $Log: ahapp.cxx,v $
 Revision 1.33  2016/04/07 19:37:41  mwitthoe
 ahapp: simplify writeParametersToLog() and add tool name at beginning of parameter string

 Revision 1.32  2016/04/07 16:42:58  mwitthoe
 ahapp: change writeParametersToLog() to write all parameters on a single line instead of one parameter per line

 Revision 1.31  2016/03/18 16:02:52  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.30  2016/03/18 14:59:37  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.29  2015/09/27 20:26:17  peachey
 Free string returned by ape prior to throwing exception. Fixes
 a memory leak in the case of an ape error.

 Revision 1.28  2015/03/31 18:04:26  driethmi
 Added function writeParametersToLog() function, to allow parameter stamping
 for compiled tools.

 Revision 1.27  2014/12/02 16:42:40  mwitthoe
 ahapp: add ahfits initialization to startUp(); see issue 437

 Revision 1.26  2014/02/13 21:54:03  peachey
 Correct a bug in which logfile string was freed prematurely.

 Revision 1.25  2014/01/10 16:20:45  peachey
 Per Redmine issue #313, make internal (remove from public header) several
 functions that are not for use outside the library.

 Revision 1.24  2014/01/03 16:44:30  peachey
 Change startUp and shutDown to behave in a more C-like way,
 handling lower exceptions and not throwing any, and returning int status.

 Revision 1.23  2013/11/08 21:35:01  mwitthoe
 ahapp: add alternative getPar functions (e.g. getParDouble_warn()) which will send a warning instead of throwing an error if there is a problem reading a parameter; the new functions also have a 2nd argument which returns true/false based on the success of reading the parameter; these fucntions were originally written in the sxsflagpix tool, but I decided to move them into ahapp since they seemed useful; test cases added for the new functions

 Revision 1.22  2013/09/18 18:15:54  mwitthoe
 ahapp: startUp(): check for all unexpected values of the standard parameters, correct as necessary, and print warning messages via ahlog; ensure that the values of the standard parameters that are stamped to FITS files match those used by calling ape_trad_set_*()

 Revision 1.21  2013/09/09 14:06:52  mwitthoe
 ahapp library: redirect ape output and error messages through ahlog whenever possible using new ape_out_redirect() and ape_err_redirect() functions; move call to ahlog set up as early as possible in ahapp::startUp() (basically after chatter and debug are read from the parameter file

 Revision 1.20  2013/08/19 15:30:09  mwitthoe
 ahapp: set value of chatter parameter within APE to be the same as the value set within ahlog; this is to avoid an error when stamping parameters to output FITS file when chatter is out-of-range; if the chatter parameter limits are set improperly (i.e. max value is smaller than the max value of 3 in ahlog), the stamping error will still occur.  Otherwise, the min/max chatter values from the parameter file are ignored in Astro-H tools

 Revision 1.19  2013/07/10 18:22:08  mwitthoe
 ahapp: update test parameter file with new standard parameter set; have startUp() read the history standard parameter and set the global history state in ahgen

 Revision 1.18  2013/07/01 19:54:50  mwitthoe
 allow ahapp library to read new buffer standard parameter

 Revision 1.17  2013/04/10 14:20:48  mwitthoe
 ahapp: fix problem in startUp() where no error message is printed if an error occurs in ape_trad_init(); issue #128

 Revision 1.16  2013/04/04 15:38:41  mwitthoe
 ahapp.cxx: change delete to free for char* variables allocated by APE (using calloc); this prevents a valgrind error

 Revision 1.15  2013/03/22 16:51:30  mwitthoe
 fix bug in ahapp.cxx:startUp() where routine is trying to remove an empty value for the CVS keyword Name; in the old version, the search string was being updated by CVS leading to inproper behavior

 Revision 1.14  2013/03/22 16:26:06  mwitthoe
 ahapp: startUp() will now remove CVS keyword string, Name, from the tooltag argument rather than the invalid Tag

 Revision 1.13  2013/03/20 15:35:54  mwitthoe
 assign tool version in startUp() using a new (optional) argument; version is used in the set_toolversion routine from HEAUTILS and will appear in the parameter dump in FITS files.

 Revision 1.12  2013/01/11 18:38:15  mwitthoe
 fix memory leaks in ahapp

 Revision 1.11  2012/11/30 14:52:25  mwitthoe
 call set_toolname() in ahapp::startUp() using the name of the tool as called from the command line where any path information is stripped out; the result of this change is that the tool name will appear in the FITS history where the parameters are stamped

 Revision 1.10  2012/11/29 18:22:19  mwitthoe
 add getPar functions to ahapp for integers and doubles; standardize errors for these functions and report APE status

 Revision 1.9  2012/11/19 14:55:52  peachey
 Comply with standard Ftool practice, and suppress prompts if
 HEADASNOQUERY environment variable is set.

 Revision 1.8  2012/11/01 14:38:35  mwitthoe
 remove clobber state from ahapp; now uses clobber state added to ahgen

 Revision 1.7  2012/10/31 13:09:39  peachey
 Correct string problem in case logfile parameter is not found.

 Revision 1.6  2012/10/25 16:45:38  mwitthoe
 ahapp: add parameter retrieval functions (using APE) for string and boolean

 Revision 1.5  2012/10/12 22:48:17  mwitthoe
 revert untagged ahapp to match tagged version; turn on parameter history in startUp()

 Revision 1.3  2012/09/14 23:51:18  mwitthoe
 apply version standards to ahapp

 Revision 1.2  2012/08/29 19:47:30  mwitthoe
 remove static debug and chatter variables from ahapp (they are now only in ahlog); fix some warning messages in testahapp

 Revision 1.1  2012/08/22 21:18:46  mwitthoe
 add ahapp library under gen/lib


*/
