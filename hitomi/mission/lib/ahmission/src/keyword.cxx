/// \file keyword.cxx
/// \brief functions to copy mission-specific keywords to new files
/// \author Kristin Rutkowski
/// \date $Date: 2016/04/07 16:55:12 $
 
#define AHLABEL ahmission_keyword
#define AHCVSID "$Id: keyword.cxx,v 1.8 2016/04/07 16:55:12 mwitthoe Exp $"


#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahfits/ahfits.h"
#include "ahmission/keyword.h"

#include <string>
#include <sstream>
#include <stdlib.h> 


// ===========================================================================

namespace ahmission {

namespace keyword {
  
typedef std::map< Filetype, std::list<std::string> > filekeymap;
typedef std::map< Category, filekeymap > keymap;
static keymap s_KeywordsMap;

// ===========================================================================

void copyKeywordsBase (ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype, Category category) {

  // Check inputs
  if (0 == src || 0 == src->m_cfitsfp) 
    AH_THROW_RUNTIME("null pointer passed for source");
  if (0 == dest || 0 == dest->m_cfitsfp) 
    AH_THROW_RUNTIME("null pointer passed for destination");

  const int maxRPTFile = 9;
  std::string srcFileName = getFileAndHDUString(src);
  std::string destFileName = getFileAndHDUString(dest);

  // count the number of keywords found and not found
  int numFound = 0;
  int numNotFound = 0;

  // for the output message
  std::string keyType = "";
  std::stringstream tmpFound;
  std::stringstream tmpNotFound;
  std::string msgFound = "";
  std::string msgNotFound = "";
  std::string listFound = "   ";
  std::string listNotFound = "   ";

  // get the list of keywords
  std::list<std::string> keywords = getKeywordsMap(filetype, category);

  // loop over the list of keywords for this filetype and category
  typedef std::list<std::string>::iterator list_iterator;
  for (list_iterator iter_list = keywords.begin() ; iter_list != keywords.end(); ++iter_list) {
    std::string currKey = *iter_list;

    if (currKey == "RPTFILEn") {
      // we need to loop over any existing RPTFILE keywords
      for (int ii = 1 ; ii <= maxRPTFile ; ++ii) {
        std::stringstream fileNumStrStr;
        fileNumStrStr << ii;
        std::string fileNumStr = fileNumStrStr.str();
        // turn the keyname into RPTFILE1, RPTFILE2, etc
        currKey.replace(7,1,fileNumStr);
        // if exists in src, copy
        if (ahfits::keywordExists(src, currKey)) {
          ahfits::copyKey(src, dest, currKey);
          listFound += currKey + " ";
          numFound++;
        }
        // we don't need to add any missing one to the list of not-found,
        // since we don't know how many each file should have
      }
    } else {
      // else, it's a normal keyword
      // if exists in src, copy
      if (ahfits::keywordExists(src, currKey)) {
        ahfits::copyKey(src, dest, currKey);
        listFound += currKey + " ";
        numFound++;
      } else {
        listNotFound += currKey + " ";
        numNotFound++;
      }  
    }

  } // end-for through keywords

  // determine which type (category) of keywords were copied, for the logging
  switch( category ) {
    case e_COORDINATE:
        keyType = "coordinate";
        break;
    case e_OBSERVATION:
        keyType = "observation";
        break;
    case e_PROCESSING:
        keyType = "processing";
        break;
    case e_TIMING:
        keyType = "timing";
        break;
  } 

  // print log messages
  if (numFound == 0) {
    AH_DEBUG << "No "<< keyType << " keywords were copied from " 
             << srcFileName << " to " << destFileName 
             << std::endl;
  } else {
    tmpFound << "The following " << numFound << " " << keyType;
    tmpFound << " keywords were copied from ";
    tmpFound << srcFileName << " to " << destFileName << ": ";
    msgFound = tmpFound.str();

    AH_DEBUG << msgFound << std::endl;
    AH_DEBUG << listFound << std::endl;
  }

  if (numNotFound == 0) {
    AH_DEBUG << "All the appropriate "<< keyType 
             << " keywords were copied from " 
             << srcFileName << " to " << destFileName 
             << std::endl;
  } else {
    tmpNotFound << "The following " << numNotFound << " " << keyType;
    tmpNotFound << " keywords were not found in the source file ";
    tmpNotFound << srcFileName << ": ";
    msgNotFound = tmpNotFound.str();

    AH_DEBUG << msgNotFound << std::endl;
    AH_DEBUG << listNotFound << std::endl;
  }

} // end copyKeywordsBase()

// ---------------------------------------------------------------------------

void copyCoordinateKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype) {
  copyKeywordsBase(src, dest, filetype, e_COORDINATE);
} // end copyCoordinateKeywords()

// ---------------------------------------------------------------------------

void copyObservationKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype) {
  copyKeywordsBase(src, dest, filetype, e_OBSERVATION);
} // end copyObservationKeywords()

// ---------------------------------------------------------------------------

void copyProcessingKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype) {
  copyKeywordsBase(src, dest, filetype, e_PROCESSING);
} // end copyProcessingKeywords()

// ---------------------------------------------------------------------------

void copyTimingKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype) {
  copyKeywordsBase(src, dest, filetype, e_TIMING);
} // end copyTimingKeywords()

// ---------------------------------------------------------------------------

void copyAllKeywords(ahfits::FilePtr src, ahfits::FilePtr dest, Filetype filetype) {
  copyCoordinateKeywords(src, dest, filetype);
  copyObservationKeywords(src, dest, filetype);
  copyProcessingKeywords(src, dest, filetype);
  copyTimingKeywords(src, dest, filetype);  
} // end copyAllKeywords()

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

const std::list<std::string>& getKeywordsMap(Filetype filetype, Category category) {
  
  // NOTE: The PSEUDOHZ keyword is specific to HXI/SGD.  Although there is no 
  // instrument-specific behavior in the keyword library, it is okay to 
  // include it in the 'observation' list since keywords are only copied if 
  // they are present in the input file
  
  // if the map hasn't been initialized yet, do that
  if (s_KeywordsMap.empty()) {
    
    // -------------------------------------
    // ------- create the lists ------------
    
    // ------- coordinates ------------
    
    std::list<std::string> coordinatePrimaryList;
    coordinatePrimaryList.push_back("RA_OBJ");
    coordinatePrimaryList.push_back("DEC_OBJ");
    coordinatePrimaryList.push_back("EQUINOX");
    coordinatePrimaryList.push_back("RADECSYS");
    coordinatePrimaryList.push_back("RA_NOM");
    coordinatePrimaryList.push_back("DEC_NOM");
    coordinatePrimaryList.push_back("PA_NOM");
    
    std::list<std::string> coordinateHKList;
    coordinateHKList.push_back("RA_OBJ");
    coordinateHKList.push_back("DEC_OBJ");
    coordinateHKList.push_back("EQUINOX");
    coordinateHKList.push_back("RADECSYS");
    coordinateHKList.push_back("RA_NOM");
    coordinateHKList.push_back("DEC_NOM");
    coordinateHKList.push_back("PA_NOM");
    
    std::list<std::string> coordinateEventList;
    coordinateEventList.push_back("RA_OBJ");
    coordinateEventList.push_back("DEC_OBJ");
    coordinateEventList.push_back("RA_PNT");
    coordinateEventList.push_back("DEC_PNT");
    coordinateEventList.push_back("EQUINOX");
    coordinateEventList.push_back("RADECSYS");
    coordinateEventList.push_back("RA_NOM");
    coordinateEventList.push_back("DEC_NOM");
    coordinateEventList.push_back("PA_NOM");
    coordinateEventList.push_back("ABERRAT");
    coordinateEventList.push_back("FOLOWSUN");
    coordinateEventList.push_back("OPTDETX");
    coordinateEventList.push_back("OPTDETY");
    coordinateEventList.push_back("OPTFOCX");
    coordinateEventList.push_back("OPTFOCY");
    coordinateEventList.push_back("OPTSKYX");
    coordinateEventList.push_back("OPTSKYY");
    
    std::list<std::string> coordinateRateList;
    coordinateRateList.push_back("RA_OBJ");
    coordinateRateList.push_back("DEC_OBJ");
    coordinateRateList.push_back("RA_PNT");
    coordinateRateList.push_back("DEC_PNT");
    coordinateRateList.push_back("EQUINOX");
    coordinateRateList.push_back("RADECSYS");
    coordinateRateList.push_back("RA_NOM");
    coordinateRateList.push_back("DEC_NOM");
    coordinateRateList.push_back("PA_NOM");
    coordinateRateList.push_back("ABERRAT");
    coordinateRateList.push_back("FOLOWSUN");
    coordinateRateList.push_back("OPTDETX");
    coordinateRateList.push_back("OPTDETY");
    coordinateRateList.push_back("OPTFOCX");
    coordinateRateList.push_back("OPTFOCY");
    coordinateRateList.push_back("OPTSKYX");
    coordinateRateList.push_back("OPTSKYY");
    
    std::list<std::string> coordinateGTIList;
    coordinateGTIList.push_back("RA_OBJ");
    coordinateGTIList.push_back("DEC_OBJ");
    coordinateGTIList.push_back("EQUINOX");
    coordinateGTIList.push_back("RADECSYS");
    coordinateGTIList.push_back("RA_NOM");
    coordinateGTIList.push_back("DEC_NOM");
    coordinateGTIList.push_back("PA_NOM");
    
    // ------- observation ------------
    
    std::list<std::string> observationPrimaryList;
    observationPrimaryList.push_back("TELESCOP");
    observationPrimaryList.push_back("INSTRUME");
    observationPrimaryList.push_back("OBS_ID");
    observationPrimaryList.push_back("OBJECT");
    observationPrimaryList.push_back("OBSERVER");
    observationPrimaryList.push_back("OBS_MODE");
    
    std::list<std::string> observationHKList;
    observationHKList.push_back("TELESCOP");
    observationHKList.push_back("INSTRUME");
    observationHKList.push_back("OBS_ID");
    observationHKList.push_back("OBJECT");
    observationHKList.push_back("OBSERVER");
    observationHKList.push_back("OBS_MODE");
    observationHKList.push_back("HDUCLASS");
    observationHKList.push_back("HDUCLAS1");
    observationHKList.push_back("HDUCLAS2");
    observationHKList.push_back("ORBFILE");
    observationHKList.push_back("ATTFILE");
    observationHKList.push_back("TIMFILE");
    observationHKList.push_back("RPTFILEn");
    observationHKList.push_back("LEAPFILE");
    
    std::list<std::string> observationEventList;
    observationEventList.push_back("TELESCOP");
    observationEventList.push_back("INSTRUME");
    observationEventList.push_back("DETNAM");
    observationEventList.push_back("FILTER");
    observationEventList.push_back("DATAMODE");
    observationEventList.push_back("OBS_ID");
    observationEventList.push_back("OBJECT");
    observationEventList.push_back("OBSERVER");
    observationEventList.push_back("OBS_MODE");
    observationEventList.push_back("HDUCLASS");
    observationEventList.push_back("HDUCLAS1");
    observationEventList.push_back("ORBFILE");
    observationEventList.push_back("ATTFILE");
    observationEventList.push_back("TIMFILE");
    observationEventList.push_back("RPTFILEn");
    observationEventList.push_back("PSEUDOHZ"); // see the NOTE above
    observationEventList.push_back("LEAPFILE");
    
    std::list<std::string> observationRateList;
    observationRateList.push_back("TELESCOP");
    observationRateList.push_back("INSTRUME");
    observationRateList.push_back("DETNAM");
    observationRateList.push_back("FILTER");
    observationRateList.push_back("DATAMODE");
    observationRateList.push_back("OBS_ID");
    observationRateList.push_back("OBJECT");
    observationRateList.push_back("OBSERVER");
    observationRateList.push_back("OBS_MODE");
    observationRateList.push_back("HDUCLASS");
    observationRateList.push_back("HDUCLAS1");
    observationRateList.push_back("HDUCLAS2");
    observationRateList.push_back("ORBFILE");
    observationRateList.push_back("ATTFILE");
    observationRateList.push_back("TIMFILE");
    observationRateList.push_back("RPTFILEn");
    observationRateList.push_back("LEAPFILE");
    
    std::list<std::string> observationGTIList;
    observationGTIList.push_back("TELESCOP");
    observationGTIList.push_back("INSTRUME");
    observationGTIList.push_back("DETNAM");
    observationGTIList.push_back("FILTER");
    observationGTIList.push_back("DATAMODE");
    observationGTIList.push_back("OBS_ID");
    observationGTIList.push_back("OBJECT");
    observationGTIList.push_back("OBSERVER");
    observationGTIList.push_back("OBS_MODE");
    observationGTIList.push_back("HDUCLASS");
    observationGTIList.push_back("HDUCLAS1");
    
    // ------- processing ------------
    
    std::list<std::string> processingPrimaryList;
    processingPrimaryList.push_back("TLM2FITS");
    processingPrimaryList.push_back("PROCVER");
    processingPrimaryList.push_back("SEQPNUM");
    processingPrimaryList.push_back("MKFFF");
    processingPrimaryList.push_back("ORIGIN");
    processingPrimaryList.push_back("SOFTVER");
    processingPrimaryList.push_back("CALDBVER");
    
    std::list<std::string> processingHKList;
    processingHKList.push_back("TLM2FITS");
    processingHKList.push_back("PROCVER");
    processingHKList.push_back("SEQPNUM");
    processingHKList.push_back("MKFFF");
    processingHKList.push_back("ORIGIN");
    processingHKList.push_back("SOFTVER");
    processingHKList.push_back("CALDBVER");
    
    
    std::list<std::string> processingEventList;
    processingEventList.push_back("TLM2FITS");
    processingEventList.push_back("PROCVER");
    processingEventList.push_back("SEQPNUM");
    processingEventList.push_back("MKFFF");
    processingEventList.push_back("ORIGIN");
    processingEventList.push_back("SOFTVER");
    processingEventList.push_back("CALDBVER");
    
    std::list<std::string> processingRateList;
    processingRateList.push_back("TLM2FITS");
    processingRateList.push_back("PROCVER");
    processingRateList.push_back("SEQPNUM");
    processingRateList.push_back("MKFFF");
    processingRateList.push_back("ORIGIN");
    processingRateList.push_back("SOFTVER");
    processingRateList.push_back("CALDBVER");
    
    std::list<std::string> processingGTIList;
    processingGTIList.push_back("TLM2FITS");
    processingGTIList.push_back("PROCVER");
    processingGTIList.push_back("SEQPNUM");
    processingGTIList.push_back("MKFFF");
    processingGTIList.push_back("ORIGIN");
    processingGTIList.push_back("SOFTVER");
    processingGTIList.push_back("CALDBVER");
    
    // ------- timing ------------
    
    std::list<std::string> timingPrimaryList;
    timingPrimaryList.push_back("TIMESYS");
    timingPrimaryList.push_back("MJDREFI");
    timingPrimaryList.push_back("MJDREFF");
    timingPrimaryList.push_back("TIMEUNIT");
    timingPrimaryList.push_back("TIMEREF");
    timingPrimaryList.push_back("TASSIGN");
    timingPrimaryList.push_back("GPSOFFET");
    timingPrimaryList.push_back("CLOCKAPP");
    timingPrimaryList.push_back("TSTART");
    timingPrimaryList.push_back("TSTOP");
    timingPrimaryList.push_back("TELAPSE");
    timingPrimaryList.push_back("DATE-OBS");
    timingPrimaryList.push_back("DATE-END");
    timingPrimaryList.push_back("SMUUNIT");
    
    std::list<std::string> timingHKList;
    timingHKList.push_back("TIMESYS");
    timingHKList.push_back("MJDREFI");
    timingHKList.push_back("MJDREFF");
    timingHKList.push_back("TIMEUNIT");
    timingHKList.push_back("TIMEREF");
    timingHKList.push_back("TASSIGN");
    timingHKList.push_back("GPSOFFET");
    timingHKList.push_back("CLOCKAPP");
    timingHKList.push_back("TSTART");
    timingHKList.push_back("TSTOP");
    timingHKList.push_back("TELAPSE");
    timingHKList.push_back("DATE-OBS");
    timingHKList.push_back("DATE-END");
    timingHKList.push_back("SMUUNIT");
    
    std::list<std::string> timingEventList;
    timingEventList.push_back("TIMESYS");
    timingEventList.push_back("MJDREFI");
    timingEventList.push_back("MJDREFF");
    timingEventList.push_back("TIMEUNIT");
    timingEventList.push_back("TIMEREF");
    timingEventList.push_back("TASSIGN");
    timingEventList.push_back("GPSOFFET");
    timingEventList.push_back("CLOCKAPP");
    timingEventList.push_back("TSTART");
    timingEventList.push_back("TSTOP");
    timingEventList.push_back("TELAPSE");
    timingEventList.push_back("DATE-OBS");
    timingEventList.push_back("DATE-END");
    timingEventList.push_back("SMUUNIT");
    timingEventList.push_back("ONTIME");
    timingEventList.push_back("LIVETIME");
    timingEventList.push_back("EXPOSURE");
    timingEventList.push_back("TIMEPIXR");
    timingEventList.push_back("TIMEDEL");
    
    std::list<std::string> timingRateList;
    timingRateList.push_back("TIMESYS");
    timingRateList.push_back("MJDREFI");
    timingRateList.push_back("MJDREFF");
    timingRateList.push_back("TIMEUNIT");
    timingRateList.push_back("TIMEREF");
    timingRateList.push_back("TASSIGN");
    timingRateList.push_back("GPSOFFET");
    timingRateList.push_back("CLOCKAPP");
    timingRateList.push_back("TSTART");
    timingRateList.push_back("TSTOP");
    timingRateList.push_back("TELAPSE");
    timingRateList.push_back("DATE-OBS");
    timingRateList.push_back("DATE-END");
    timingRateList.push_back("SMUUNIT");
    timingRateList.push_back("ONTIME");
    timingRateList.push_back("LIVETIME");
    timingRateList.push_back("EXPOSURE");
    timingRateList.push_back("TIMEPIXR");
    timingRateList.push_back("TIMEDEL");
    
    std::list<std::string> timingGTIList;
    timingGTIList.push_back("TIMESYS");
    timingGTIList.push_back("MJDREFI");
    timingGTIList.push_back("MJDREFF");
    timingGTIList.push_back("TIMEUNIT");
    timingGTIList.push_back("TIMEREF");
    timingGTIList.push_back("TASSIGN");
    timingGTIList.push_back("GPSOFFET");
    timingGTIList.push_back("CLOCKAPP");
    timingGTIList.push_back("TSTART");
    timingGTIList.push_back("TSTOP");
    timingGTIList.push_back("TELAPSE");
    timingGTIList.push_back("DATE-OBS");
    timingGTIList.push_back("DATE-END");
    timingGTIList.push_back("SMUUNIT");
    timingGTIList.push_back("LTISTART");
    timingGTIList.push_back("LTISTOP");
    
    
    // -------------------------------------
    // ------- populate the map ------------
    
    s_KeywordsMap[e_COORDINATE][e_PRIMARY]    = coordinatePrimaryList;
    s_KeywordsMap[e_COORDINATE][e_HK]         = coordinateHKList;
    s_KeywordsMap[e_COORDINATE][e_EVENT]      = coordinateEventList;
    s_KeywordsMap[e_COORDINATE][e_RATE]       = coordinateRateList;
    s_KeywordsMap[e_COORDINATE][e_GTI]        = coordinateGTIList;
    
    s_KeywordsMap[e_OBSERVATION][e_PRIMARY]   = observationPrimaryList;
    s_KeywordsMap[e_OBSERVATION][e_HK]        = observationHKList;
    s_KeywordsMap[e_OBSERVATION][e_EVENT]     = observationEventList;
    s_KeywordsMap[e_OBSERVATION][e_RATE]      = observationRateList;
    s_KeywordsMap[e_OBSERVATION][e_GTI]       = observationGTIList;
    
    s_KeywordsMap[e_PROCESSING][e_PRIMARY]    = processingPrimaryList;
    s_KeywordsMap[e_PROCESSING][e_HK]         = processingHKList;
    s_KeywordsMap[e_PROCESSING][e_EVENT]      = processingEventList;
    s_KeywordsMap[e_PROCESSING][e_RATE]       = processingRateList;
    s_KeywordsMap[e_PROCESSING][e_GTI]        = processingGTIList;
    
    s_KeywordsMap[e_TIMING][e_PRIMARY]        = timingPrimaryList;
    s_KeywordsMap[e_TIMING][e_HK]             = timingHKList;
    s_KeywordsMap[e_TIMING][e_EVENT]          = timingEventList;
    s_KeywordsMap[e_TIMING][e_RATE]           = timingRateList;
    s_KeywordsMap[e_TIMING][e_GTI]            = timingGTIList;
    
    
  } // end-if empty map

  return s_KeywordsMap[category][filetype];
  
}

// ---------------------------------------------------------------------------

}  // namespace keyword

}  // namespace ahmission


/* Revision Log
 $Log: keyword.cxx,v $
 Revision 1.8  2016/04/07 16:55:12  mwitthoe
 ahmission library: change AH_INFO to AH_DEBUG in copy keyword functions

 Revision 1.7  2016/02/20 02:38:24  klrutkow
 added PSEUDOHZ keyword to observation list for event files ; removed commented out DATE,CHECKSUM,DATASUM

 Revision 1.6  2015/07/30 18:34:48  mwitthoe
 ahmission keyword lib: add HDUCLAS2 to list of keywords copied into HK extensions; comment out DATE/CHECKSUM/DATASUM for copying in Primary HDUs

 Revision 1.5  2015/07/29 20:42:09  klrutkow
 don't copy EXTNAME keyword anymore

 Revision 1.4  2015/07/29 20:15:45  klrutkow
 added keyword type to logging ; loop over RPTFILE keywords ; don't copy DATE, CHECKSUM, DATASUM keywords

 Revision 1.3  2015/07/29 18:22:48  klrutkow
 implemented full keyword-copying functionality

 Revision 1.2  2015/07/29 01:59:00  klrutkow
 added declarations to other necessary keyword-copying functions (not fully implemented yet)

 Revision 1.1  2015/07/27 17:06:49  klrutkow
 added new files to copy keywords




 */
