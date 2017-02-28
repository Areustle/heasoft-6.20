/// \file caldb.cxx
/// \brief functions to act on the CALDB input parameters for Astro-H.
/// \author Kristin Rutkowski
/// \date $Date: 2016/02/11 19:07:15 $
 
#define AHLABEL ahmission_caldb
#define AHCVSID "$Id: caldb.cxx,v 1.10 2016/02/11 19:07:15 klrutkow Exp $"


#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmission/caldb.h"

#include "hdcal.h"

#include <string>
#include <sstream>
#include <fstream>      // std::ifstream
#include <stdlib.h> 


// ===========================================================================

namespace ahmission {

namespace caldb {
  
std::string resolve(const std::string & filename,
                    const std::string & filetype,
                    const std::string & instrume,
                    const std::string & detnam,
                    const std::string & codename,
                    const std::string & datetime,
                    const std::string & expression,
                    const std::string & telescop) {
  
  // initialize the return variable to the input filename. If it's not CALDB
  // or REFDATA, this is what will be returned.  
  std::string filenameFull = filename;
  
  std::string querydate = "-";
  std::string querytime = "-";
  
  if (ahgen::strtoupper(filename) == "CALDB") {
    
    AH_INFO(ahlog::HIGH) << "Searching CALDB for "<<filetype<<" file" << std::endl;
    
    // check if relevant environment variables are set
    if (getenv("CALDB") == NULL) {
      AH_THROW_RUNTIME("$CALDB environment variable not defined");
    }
    if (getenv("CALDBCONFIG") == NULL) {
      AH_THROW_RUNTIME("$CALDBCONFIG environment variable not defined");
    }
    if (getenv("CALDBALIAS") == NULL) {
      AH_THROW_RUNTIME("$CALDBALIAS environment variable not defined");
    }
    
    // Log location of user's CALDB 
    AH_INFO(ahlog::LOW) << "$CALDB="<<getenv("CALDB") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBCONFIG="<<getenv("CALDBCONFIG") << std::endl;
    AH_INFO(ahlog::LOW) << "$CALDBALIAS="<<getenv("CALDBALIAS") << std::endl;
    
    // get the date and time search values
    if (ahgen::strtoupper(datetime) == "NOW") {
      querydate = "NOW";
      querytime = "NOW";
    } else if (datetime == "-") {
      querydate = "-";
      querytime = "-";
    } else if (datetime.length() == 0) {
      // empty string
      querydate = "-";
      querytime = "-";
    } else if (datetime.length() == 10) {
      // just a date was passed in
      querydate = datetime.substr(0,10);
      querytime = "-";
    } else if (datetime.find("T") != std::string::npos) {
      // datetime should be in the format yyyy-mm-ddThh:mm:ss
      querydate = datetime.substr(0,10);
      querytime = datetime.substr(11,8);
    } else {
      AH_THROW_RUNTIME("The datetime parameter ("+datetime+") for the CALDB search must be in a valid format (either 'now', '-', '', 'yyyy-mm-dd', or 'yyyy-mm-ddThh::mm:ss')");
    }
    
    const char* tele      = telescop.c_str();
    const char* instr     = instrume.c_str();
    const char* det       = detnam.c_str();
    const char* filt      = "-";
    const char* codenam   = codename.c_str();
    const char* strtdate  = querydate.c_str();
    const char* strttime  = querytime.c_str();
    const char* stpdate   = "-";
    const char* stptime   = "-";
    const char* expr      = expression.c_str();

    const int fnamesize   = 256;    // max size of filename
    const int onlinesize  = 10;     // size of online 
    int maxret            = 1;
    char filename[fnamesize];
    char* fnptr           = filename;
    long extno            = 0;
    char online[onlinesize];
    char* onptr           = online;
    int nret              = 0;
    int nfound            = 0;
    int status            = 0;
    
    AH_INFO(ahlog::HIGH) << "Querying CALDB with these parameters: " << std::endl; 
    AH_INFO(ahlog::HIGH) << "   TELESCOPE:  "  << tele << std::endl;
    AH_INFO(ahlog::HIGH) << "   INSTRUMENT: "  << instr << std::endl;
    AH_INFO(ahlog::HIGH) << "   DETNAM:     "  << det << std::endl;
    AH_INFO(ahlog::HIGH) << "   FILTER:     "  << filt << std::endl;
    AH_INFO(ahlog::HIGH) << "   CODENAME:   "  << codenam << std::endl;
    AH_INFO(ahlog::HIGH) << "   START DATE: "  << strtdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   START TIME: "  << strttime << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP DATE:  "  << stpdate << std::endl;
    AH_INFO(ahlog::HIGH) << "   STOP TIME:  "  << stptime << std::endl;
    AH_INFO(ahlog::HIGH) << "   EXPRESSION: "  << expr << std::endl;

    HDgtcalf(tele,instr,det,filt,codenam,strtdate,strttime,
             stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
             &onptr,&nret,&nfound,&status);

    if (status != 0) {
      std::stringstream serr;
      serr << "Could not get "<<filetype<<" file from CALDB; "
           << "HDgtcalf status: " << status;
      AH_THROW_RUNTIME(serr.str());
    }

    if (nfound == 0) {
      AH_THROW_RUNTIME("No "+filetype+" file found in CALDB");
    }
    
    // include extended syntax, extension number in string to be returned
    std::stringstream tmp;
    tmp << filename << "[" << extno << "]";
    filenameFull = tmp.str();

    AH_INFO(ahlog::HIGH) << "CALDB " << filetype << " file: " << filenameFull << std::endl;
    
  } else if (ahgen::strtoupper(filename) == "REFDATA") {
    
    AH_INFO(ahlog::HIGH) << "Searching REFDATA for "<<filetype<<" file" << std::endl;
    
    char* lhea_data = getenv("LHEA_DATA");
    if (lhea_data == NULL) {
      std::string msg="using REFDATA for "+filetype+" file requires ";
      msg+="LHEA_DATA environment variable";
      AH_THROW_RUNTIME(msg);
    }
    
    if (ahgen::strtoupper(codename) == "LEAPSECS") { 
      filenameFull = (std::string)lhea_data+"/leapsec.fits";
      
    } else if (ahgen::strtoupper(codename) == "RIGIDITY") {

      // use the date to decide which rigidity file to grab
      // Use rigidity_20060421.fits if date is between 2005-01-01 and 2016-01-01
      // Use rigidity_20160101.fits if date is on or after 2016-01-01
      
      // NOTE: In order to retrieve the rigidity_20060421.fits file in refdata,
      // Suzaku needs to be included in the sourced build. 
      // This is used in the tool ahmkehk.

      // if no date string was entered, get the latest rigidity file
      if ( (ahgen::strtoupper(datetime) == "NOW") ||
           (datetime.length() == 0) || 
           (datetime == "-") ) {
        filenameFull = (std::string)lhea_data+"/rigidity_20160101.fits";
      } else if (datetime == "-") {
        filenameFull = (std::string)lhea_data+"/rigidity_20060421.fits";
        
      } else if ( (datetime.length() >= 10) && 
           ("-" == datetime.substr(4,1)) && 
           ("-" == datetime.substr(7,1)) && 
           ahgen::isNumber(datetime.substr(0,4)) ) {
        // check the format of the date string
        
        // check for the year
        std::string yearstr = datetime.substr(0,4);
        int yearint;
        std::istringstream (yearstr) >> yearint;
        if (yearint >= 2016) {
          filenameFull = (std::string)lhea_data+"/rigidity_20160101.fits";
        } else if ( (yearint >= 2005) && (yearint < 2016) ) {
          filenameFull = (std::string)lhea_data+"/rigidity_20060421.fits";
        } else {
          AH_THROW_RUNTIME("The datetime parameter ("+datetime+") for the RIGIDITY REFDATA search must be a valid date, on or after 2005-01-01");
        }
        
      } else {
        AH_THROW_RUNTIME("The datetime parameter ("+datetime+") for the REFDATA search must be in a valid format (either 'NOW', '-', '', or 'yyyy-mm-dd')");
      }
      
    } else {
      // as of now, we only support LEAPSEC and RIGIDITY for REFDATA
      AH_THROW_RUNTIME("REFDATA is not a supported option for the "+filetype+" file");
    }
    
    AH_INFO(ahlog::HIGH) << "REFDATA " << filetype << " file: " << filenameFull << std::endl;
    
  } // end-if CALDB or REFDATA
   
  return filenameFull;

} // end resolve()


// ---------------------------------------------------------------------------

}  // namespace caldb

}  // namespace ahmission


/* Revision Log
 $Log: caldb.cxx,v $
 Revision 1.10  2016/02/11 19:07:15  klrutkow
 support rigidity file in refdata

 Revision 1.9  2015/09/20 16:21:26  klrutkow
 log location of user's CALDB

 Revision 1.8  2015/08/14 22:02:05  klrutkow
 remove CALDBCONFIG check for ASTRO-H

 Revision 1.7  2015/08/14 20:26:30  klrutkow
 edited acceptable date format to look for T instead of length 19 ; add check through caldbconfig to make sure astro-h is supported ; added logging to output users caldb location (commented out until after build)

 Revision 1.6  2015/07/30 00:54:14  klrutkow
 udpated datetime param to allow for 'now' and '-' and 'yyyy-mm-dd' ; updated rigidity refdata query - still not supported though

 Revision 1.5  2015/07/16 14:47:29  klrutkow
 udpated error message to include the word CALDB

 Revision 1.4  2015/07/14 18:38:56  klrutkow
 changed info to throw_runtime if user passes REFDATA for rigidity file

 Revision 1.3  2015/07/14 18:05:36  klrutkow
 added AH_INFO statements to REFDATA section

 Revision 1.2  2015/07/14 17:18:52  mdutka
 adding include stdlib.h

 Revision 1.1  2015/07/14 16:16:46  klrutkow
 added new, general, CALDB query in caldb.h and caldb.cxx


 */
