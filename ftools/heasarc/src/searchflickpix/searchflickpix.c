/**
  @file   searchflickpix.c
  @brief  Search for flickering pixels from event data
  @author A. J. Sargent
  @date   $Date: 2016/04/21 13:34:55 $	 
  @version 1.0

  @defgroup tool_searchflickpix FLICKERING PIXELS (searchflickpix)

  Searching so-called flickering pixels, which show anomalously high dark current
  compared to normal pixels due to radiation damage on a semi-conductor, and output
  a FITS file containing location, counts, and duration of the detected pixels.

  Source files:

    searchflickpix.c

  Library dependencies:

    ftools/ftoolslib/gen
    heacore/ahlog
    heacore/ape
    heacore/cfitsio
    heacore/heaapp
    heacore/heautils

  Modification history:

    Ver     Date        Author  Description
    1.0   2015-10-16     AJS    First complete version.

*/

#define AHLABEL tool_searchflickpix
#define AHCVSID "$Id: searchflickpix.c,v 1.21 2016/04/21 13:34:55 mdutka Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahlog/cahlog.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "headas_utils.h"
#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"
#include "fitsio.h"   /* fitsio functions */
#include "cfortran.h" /* ft_gammq() */

#include <math.h>     /* fmin(), fmax() */

/** @brief Prototype and declaration of call to ftools function ft_gammq
 */
                        PROTOCCALLSFFUN3(FLOAT,FT_GAMMQ,ft_gammq,FLOAT,FLOAT,PINT)
#define FT_GAMMQ(a,x,status) CCALLSFFUN3(FT_GAMMQ,ft_gammq,FLOAT,FLOAT,PINT,a,x,status)

/** @addtogroup tool_searchflickpix
 *  @{
 */

/** @brief Structure of parameters from user-input or parameter file
 */
struct Params{
  
  char m_infile[FLEN_FILENAME];       /**< Input event file name */
  char m_outfile[FLEN_FILENAME];      /**< Output flickering pixel file name */

  int m_cellsize;                     /**< Poisson clean cell size (odd integer > 1) */                  
  int m_impfac;                       /**< Factor for gamma function */
  double m_logprob1;                  /**< Log Poisson probability threshold (e.g., -5.6)*/
  double m_logprob2;                  /**< Log Poisson probability threshold for second step */
  int m_bthresh;                      /**< Zero background threshold */

  char m_timecol[FLEN_KEYWORD];       /**< Time column name (NONE is allowed) */
  char m_chipcol[FLEN_KEYWORD];       /**< Chip column name (NONE is allowed)*/
  char m_xcol[FLEN_KEYWORD];          /**< X coordinate column name */
  char m_ycol[FLEN_KEYWORD];          /**< Y coordinate column name */
  char m_chancol[FLEN_KEYWORD];       /**< Pulse height column name (NONE is allowed) */
  char m_gradecol[FLEN_KEYWORD];      /**< Event grade column name (NONE is allowed)*/

  char m_firstchip[FLEN_KEYWORD];     /**< Minimum value for chip number */
  char m_lastchip[FLEN_KEYWORD];      /**< Maximum value for chip number */
  char m_xmin[FLEN_KEYWORD];          /**< Minimum value for X coordinate */
  char m_xmax[FLEN_KEYWORD];          /**< Maximum value for X coordinate */
  char m_ymin[FLEN_KEYWORD];          /**< Minimum value for Y coordinate */
  char m_ymax[FLEN_KEYWORD];          /**< Maximum value for Y coordinate */
  char m_chanmin[FLEN_KEYWORD];       /**< Minimum pulse-height value for clean (inclusive) */
  char m_chanmax[FLEN_KEYWORD];       /**< Maximum pulse-height value for clean (inclusive) */

  char m_grade[FLEN_KEYWORD];         /**< Event grade for clean from parfile (string: '0,2,3','0-12','0,2-4', 'all' allowed) */
  int m_gradeInt[26];                 /**< Event grades from user input (array of ints) */

  char m_iterate;                     /**< Iterate the second step Poisson clean (yes/no) */
  char m_flagedge;                    /**< Zero chip edge pixels (yes/no) */
  int m_n_division;                   /**< Divide total observation time into the given number */
  char m_duration;                    /**< Perform detailed search for flickering duration (yes,no) */
  double m_sigma;                       /**< Significance level for flickering duration */
  
  char m_cleanimg;                    /**< Output cleaned image for debug purposes (yes/no) */

};

/** @brief Structure to hold information of keywords from FITS input file
 */
struct Keywords{

  double m_tstart;                    /**< Observation start time */
  double m_tstop;                     /**< Observation stop time */

  int m_timeColNum;                   /**< Column number for user-defined time column (optional) */
  int m_chipColNum;                   /**< Column number for user-defined chip column (optional) */
  int m_firstchip;                    /**< Minimum chip number (user-defined or FITS-defined) */
  int m_lastchip;                     /**< Maximum chip number (user-defined or FITS-defined) */
  int m_xColNum;                      /**< Column number for x-coordinate column */
  int m_xmin;                         /**< Minimum x-coordinate (user-defined or FITS-defined) */
  int m_xmax;                         /**< Maximum x-coordinate (user-defined or FITS-defined) */
  int m_yColNum;                      /**< Column number for y-coordinate column */
  int m_ymin;                         /**< Minimum y-coordinate (user-defined or FITS-defined) */
  int m_ymax;                         /**< Maximum y-coordinate (user-defined or FITS-defined) */
  int m_chanColNum;                   /**< Column number for user-defined pulse-height column (optional) */
  long long m_chanrepeat;             /**< Repeat value of pulse-height column */
  int m_chanmin;                      /**< Minimum pulse-height value (user-defined or FITS-defined) */
  int m_chanmax;                      /**< Minimum pulse-height value (user-defined or FITS-defined) */
  int m_gradeColNum;                  /**< Column number for user-defined grade column (optional) */

};

/** @brief Structure to hold FITS event file information
 */
struct Event{
  double m_time;                      /**< Time of event (s)*/
  int m_ccdid;                        /**< Chip ID of event */
  int m_coordx;                       /**< x-coordinate of event */
  int m_coordy;                       /**< y-coordinate of event */
  int m_grade;                        /**< grade of event */
  int m_chan;                         /**< Index-specific pulse-height value */
};

struct ColNum{
  int m_time;                   /**<Column number for time PIXELS extension */ 
  int m_timelast;               /**<Column number for timelast PIXELS extension */ 
  int m_chipcol;                /**<Column number for chipcol PIXELS extension */ 
  int m_rawx;                   /**<Column number for rawx PIXELS extension */
  int m_rawy;                   /**<Column number for rawy PIXELS extension */
  int m_counts;                 /**<Column number for counts PIXELS extension */
  int m_fraction;               /**<Column number for fraction PIXELS extension */
  int m_chancol;                /**<Column number for chancol PIXELS extension */
};

/**
 * @brief define structure types
 */
typedef struct Params Params;
typedef struct Keywords Keywords;
typedef struct Event Event;
typedef struct ColNum ColNum;

int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);
void writeParametersToLog();
int shutDown(HeaAppData * appdata);

/**
 * @brief Get parameter values
 * @param[out] param         Structure containing parameter file data
 */
int getPar(Params * param);

/**
 * @brief Set up file pointers and keyword structure
 * @param[out] fpin          Input event file pointer [EVENTS]
 * @param[out] fpout1        Output event file pointer [EVENTS]
 * @param[out] fpout2        Output event file pointer [PIXELS]
 * @param[out] key           Structure containing input file keyword data
 * @param[out] param         Structure containing parameter file data
 * @param[out] ch_element     User specified element number for pulse-height column
 * @param[out] chcolName     User specified pulse-height column name
 * @param[out] imsizeX       Size of CCD (x-coordinates)
 * @param[out] imsizeY       Size of CCD (y-coordinates)
 * @param[out] num_chips      Number of CCDs
 * @param[out] phaselength   time length of each phase 
 * @param[out] n_phase       number of phaselengths
 * @param[out] bthresh       Zero background threshold
 * @param[out] cellsize      Poisson clean cell size (odd integer > 1)
 * @param[out] impfrac       Factor for gamma function    
 * @param[out] pmax1         Poisson probability threshold for first iteration
 * @param[out] pmax2         Poisson probability threshold for second iteration
 * @param[out] colNums       Structure to store PIXELS extension column numbers
 * @return Return false if no errors, else true
 */
int initialize(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2, 
               struct Keywords * key, struct Params * param, int * ch_element, 
               char * chcolName, int * imsizeX, int * imsizeY, int * num_chips, 
               double * phaselength, int * n_phase, int * bthresh, 
               int * cellsize, int * impfac, double * pmax1, 
               double * pmax2, ColNum * colNums);

/**
 * @brief Main routine for event loops and file reading/writing
 * @param[out] fpin          Input event file pointer [EVENTS]
 * @param[out] fpout1        Output event file pointer [EVENTS]
 * @param[out] fpout2        Output event file pointer [PIXELS]
 * @param[in] key            Structure containing input file keyword data
 * @param[in] param          Structure containing parameter file data
 * @param[in] ch_element      User specified element number for pulse-height column
 * @param[in] chcolName      User specified pulse-height column name
 * @param[in] imsizeX        Size of CCD (x-coordinates)
 * @param[in] imsizeY        Size of CCD (y-coordinates)
 * @param[in] num_chips       Number of CCDs
 * @param[in] bthresh        Zero background threshold
 * @param[in] cellsize       Poisson clean cell size (odd integer > 1)
 * @param[in] impfrac        Factor for gamma function
 * @param[in] pmax1          Poisson probability threshold for first iteration
 * @param[in] pmax2          Poisson probability threshold for second iteration
 * @param[in] phaselength   time length of each phase 
 * @param[in] n_phase       number of phaselengths
 * @param[in] colNums       Structure to store PIXELS extension column numbers
 * @return Return false if no errors, else true
 */
int doWork(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2,
           struct Keywords key, struct Params param, int ch_element,
           char * chcolName, int imsizeX, int imsizeY, int num_chips, 
           int bthresh, int cellsize, int impfac, 
           double pmax1, double pmax2, double phaselength, int n_phase, 
           ColNum colNums);

/**
 * @brief Close input/output files
 * @param[out] fpin          Input event file pointer [EVENTS]
 * @param[out] fpout1        Output event file pointer [EVENTS]
 * @param[out] fpout2        Output event file pointer [PIXELS]
 * @return Return false if no errors, else true
 */
int finalize(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2);

/**
 * @brief Search for flickering pixels
 * @param[in] imsizeX        Size of CCD (x-coordinates)
 * @param[in] imsizeY        Size of CCD (y-coordinates)
 * @param[in] ipmin          Minimum window x-location where pixel event detected
 * @param[in] ipmax          Maximum window x-location where pixel event detected
 * @param[in] jpmin          Minimum window y-location where pixel event detected
 * @param[in] jpmax          Maximum window y-location where pixel event detected
 * @param[in] pixels
 * @param[in] bthresh        Zero background threshold 
 * @param[in] iterate        Iterate the second step Poisson clean
 * @param[in] flagedge       Zero chip edge pixels 
 * @param[in] pmax1          Poisson probability threshold for first iteration
 * @param[in] pmax2          Poisson probability threshold for second iteration
 * @param[out] chip_image     Chip specific copy of detector image counts
 * @param[out] chip_flag      Array of chip-specific pixels with flagged flickering pixels
 * @param[out] fix_counts     Number of detected pixels
 * @param[out] all_counts     Total number of counts
 * @param[out] bad_counts     Number of counts in the detected flickering pixels
 * @param[out] error_status   Return error status > 0 if bad calculation
 * @return Return false if no errors, else true
 */
int clean_chip(int imsizeX, int imsizeY, int ipmin, int ipmax, int jpmin,
              int jpmax, int pixels, double pmax1, double pmax2, int cellsize, int impfac,
              int bthresh, int iterate, int flagedge, int *** chip_image, int *** chip_flag,
              int * fix_counts, int * all_counts, int * bad_counts, int * error_status);

/**
 * @brief Calculate time duration of flickering pixel
 * @param[in] n               Total number of counts
 * @param[in] time_evt        Times of flickering events
 * @param[in] interval_evt    Times since previous flickering event
 * @param[in] ph_evt          Counts of flickering events
 * @param[in] phasefirst      Current phase start time
 * @param[in] phaselast       Current phase stop time
 * @param[in] sigma           Significance level for flickering duration
 * @param[out] timefirst      Start time of flickering pixel
 * @param[out] timelast       Stop time of flickering pixel
 * @param[out] evnum_good     Total number of non-flickering events
 * @param[out] phsum_good     Total count of non-flickering events
 * @return Return false if no errors, else true
 */
int duration_search (int n, double * time_evt, double * interval_evt, int * ph_evt,
                     double phasefirst, double phaselast, double sigma,
                     double * timefirst, double * timelast, int * evnum_good,
                     int * phsum_good);
/**
 * @brief Create and open output flickering pixel file with two HDUs
 * @param[in] fpin           Input event file pointer [EVENTS]
 * @param[out] fpout1        Output event file pointer [EVENTS]
 * @param[in] chcolName      User specified pulse-height column name
 * @param[in] param          Structure containing parameter file data
 * @param[out] colNums       Structure to store PIXELS extension column numbers
 * @return Return false if no errors, else true
 */
int create_flickering_file(fitsfile ** fpout1, char * chcolName, struct Params * param, ColNum * colNums);

/**
 * @brief Determine minimum/maximum value of column (e.g. minimum/maximum x-coordinates)
 * @param[in] fpin           Input event file pointer [EVENTS]
 * @param[in] parstrMin      Keyword with minimum data
 * @param[in] parstrMax      Keyword with maximum data
 * @param[in] colnum         Column number with min/max data
 * @param[out] min           Output minimum value
 * @param[out] max           Output maximum value
 * @return Return false if no errors, else true
 */
int get_min_max(fitsfile ** fpin, char * parmin, char * parmax, char * parstrMin,
              char * parstrMax, int colnum, int * min, int * max);

/**
 * @brief Construct the Name of each WCS keyword as it appears in the EVENTS extension
          then store the value and write it (using the correct number for the keyword
          name) to the output PIXEL extension header
 * @param[in] fpin           input filepointer
 * @param[in] fpout          output filepointer
 * @param[in] keyroot        root name of keyword    
 * @param[in] colnum_xcol    column number of xcol in EVENTS
 * @param[in] colnum_ycol    column number of ycol in EVENTS     
 * @param[in] colNums        column number struct
 * @param[in] error_status   error_status for called subroutines
 * @return Return false if no errors, else true
 */
int writeWCSKeys(fitsfile * fpin, fitsfile ** fpout, char * keyroot, int colnum_xcol,
                 int colnum_ycol, ColNum * colNums, int * error_status); 

/****************************************************************************/

/** @brief searchflickpix tool */
int main(int argc, char** argv) {

  Params param;                            /* Structure to store parameters for par file */
  Keywords key;                            /* Structure to store keyword values from FITS file */
  ColNum colNums;                    /* Structure to store PIXELS extension column numbers */
  
  fitsfile * fpin = NULL;                  /* File pointer to event input file */
  fitsfile * fpout1 = NULL;                /* File pointer to event output file [EVENTS] HDU */
  fitsfile * fpout2 = NULL;                /* File pointer to event output file [PIXELS] HDU */

  int imsizeX = 0;                         /* x-coordinate image size in pixels */
  int imsizeY = 0;                         /* y-coordinate image size in pixels */
  int num_chips = 0;                        /* Number of CCDs in detector */

  int ch_element = 0;                       /* Pulse-height vector element (0: default)*/
  char chcolName[FLEN_KEYWORD] = "";       /* Pulse-height column name (string) */

  int status = 0;                          /* Status (0: normal) */
  int finalStatus = 0;                     /* Final error status */

  double phaselength = 0;                  /* time length of each phase */
  int n_phase = 0;                         /* number of phase lengths */
  int bthresh = 0;                         /* Zero background threshold */
  int cellsize = 0;                        /* Poisson clean cell size */
  int impfac = 0;                          /* Impact factor for gamma function */
  double pmax1 = 0.0, pmax2 = 0.0;         /* Poisson probability threshold */

  HeaAppData appdata = { 0 };

  status = startUp(argc, argv, TOOLTAG, &appdata);
  if (0 != status) {
    ahlog_err(__PRETTY_FUNCTION__, "startUp returned status %d, not 0 as expected.\n", status);
    finalStatus = 1; /* Latch error status). */
  }

  if (0 == status) {
    status = getPar(&param);
    if(0 != status) {
      writeParametersToLog();
      ahlog_err(__PRETTY_FUNCTION__, "getPar returned status %d, not 0 as expected.\n", status);
      finalStatus = 0;
    }

    if (0 == status) {
      status = initialize(&fpin,&fpout1,&fpout2,&key,&param,&ch_element,chcolName,&imsizeX,&imsizeY,
                          &num_chips,&phaselength,&n_phase,&bthresh,&cellsize,&impfac,&pmax1,&pmax2,&colNums);
      if (0 != status) {
        ahlog_err(__PRETTY_FUNCTION__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) {
      status = doWork(&fpin,&fpout1,&fpout2,key,param,ch_element,chcolName,imsizeX,
                      imsizeY,num_chips,bthresh,cellsize,impfac,pmax1,pmax2,
                      phaselength,n_phase,colNums);
      if(0 != status) {
        ahlog_err(__PRETTY_FUNCTION__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(&fpin,&fpout1,&fpout2);
    if(0 != status) {
      ahlog_err(__PRETTY_FUNCTION__, "finalize returned status %d, not 0 as expected.\n", status);
      finalStatus = 1;
    }
      
  } else {
    ahlog_err(__PRETTY_FUNCTION__, "Unable to start up tool.\n");
  }

  status = shutDown(&appdata);
  if (0 != status) {
    appdata.printerr(__PRETTY_FUNCTION__, "shutDown returned status %d, not 0 as expected.\n", status);
    finalStatus = 1;
  }

  return finalStatus;

}

/****************************************************************************/

int getPar(Params * param) {

  int status = 0;                          /* Temporary variable to hold status */
  int statusOut = 0;                       /* Return getPar if statusOut is ever set to true */
  int tempInt;                             /* Temporary holder variable for ints */
  double tempDouble;                       /* Temporary holder variable for doubles */
  char tempBool;                           /* Temporary holder variable for booleans */
  char * tempStr;                          /* Temporary holder variable for strings */

  if ((status = ape_trad_query_string("infile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get infile\n");
    statusOut = 1;
  } else {
    strcpy(param->m_infile, tempStr);
  }
  if ((status = ape_trad_query_string("outfile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get outfile\n");
    statusOut = 1;
  } else {
    strcpy(param->m_outfile, tempStr);
  }
if ((status = ape_trad_query_string("timecol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter timecol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_timecol, tempStr);
  }
  if ((status = ape_trad_query_string("chipcol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter chipcol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_chipcol, tempStr);
  }
  if ((status = ape_trad_query_string("xcol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter xcol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_xcol, tempStr);
  }
  if ((status = ape_trad_query_string("ycol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter ycol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_ycol, tempStr);
  }
  if ((status = ape_trad_query_string("chancol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter chancol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_chancol, tempStr);
  }
  if ((status = ape_trad_query_string("gradecol",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter gradecol\n");
    statusOut = 1;
  } else {
    strcpy(param->m_gradecol, tempStr);
  }
 if ((status = ape_trad_query_string("grade",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter grade\n");
    statusOut = 1;
  } else {
    strcpy(param->m_grade, tempStr);
  }
  if ((status = ape_trad_query_int("n_division",&tempInt)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter n_division\n");
    statusOut = 1;
  } else {
    param->m_n_division = tempInt;
  }
  if ((status = ape_trad_query_bool("cleanimg",&tempBool)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter cleanimg\n");
    statusOut = 1;
  } else {
    param->m_cleanimg = tempBool;
  }
  if ((status = ape_trad_query_int("cellsize",&tempInt)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter cellsize\n");
    statusOut = 1;
  } else {
    param->m_cellsize = tempInt;
  }
  if ((status = ape_trad_query_int("impfac",&tempInt)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter impfac\n");
    statusOut = 1;
  } else {
    param->m_impfac = tempInt;
  }
  if ((status = ape_trad_query_double("logprob1",&tempDouble)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter logprob1\n");
    statusOut = 1;
  } else {
    param->m_logprob1 = tempDouble;
  }
  if ((status = ape_trad_query_double("logprob2",&tempDouble)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter logprob2\n");
    statusOut = 1;
  } else {
    param->m_logprob2 = tempDouble;
  }
  if ((status = ape_trad_query_bool("iterate",&tempBool)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter iterate\n");
    statusOut = 1;
  } else {
    param->m_iterate = tempBool;
  }
  if ((status = ape_trad_query_bool("flagedge",&tempBool)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter flagedge\n");
    statusOut = 1;
  } else {
    param->m_flagedge = tempBool;
  }
  if ((status = ape_trad_query_int("bthresh",&tempInt)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter bthresh\n");
    statusOut = 1;
  } else {
    param->m_bthresh = tempInt;
  }
  if ((status = ape_trad_query_bool("duration",&tempBool)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter duration\n");
    statusOut = 1;
  } else {
    param->m_duration = tempBool;
  }
  if ((status = ape_trad_query_double("sigma",&tempDouble)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter sigma\n");
    statusOut = 1;
  } else {
    param->m_sigma = tempDouble;
  }  
  if ((status = ape_trad_query_string("firstchip",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter firstchip\n");
    statusOut = 1;
  } else {
    strcpy(param->m_firstchip, tempStr);
  }
  if ((status = ape_trad_query_string("lastchip",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter lastchip\n");
    statusOut = 1;
  } else {
    strcpy(param->m_lastchip, tempStr);
  }
  if ((status = ape_trad_query_string("xmin",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter xmin\n");
    statusOut = 1;
  } else {
    strcpy(param->m_xmin, tempStr);
  }
  if ((status = ape_trad_query_string("xmax",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter xmax\n");
    statusOut = 1;
  } else {
    strcpy(param->m_xmax, tempStr);
  }
  if ((status = ape_trad_query_string("ymin",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter ymin\n");
    statusOut = 1;
  } else {
    strcpy(param->m_ymin, tempStr);
  }
  if ((status = ape_trad_query_string("ymax",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter ymax\n");
    statusOut = 1;
  } else {
    strcpy(param->m_ymax, tempStr);
  }
  if ((status = ape_trad_query_string("chanmin",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter chanmin\n");
    statusOut = 1;
  } else {
    strcpy(param->m_chanmin, tempStr);
  }
  if ((status = ape_trad_query_string("chanmax",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get parameter chanmax\n");
    statusOut = 1;
  } else {
    strcpy(param->m_chanmax, tempStr);
  }

  return statusOut ? statusOut : 0;
}

/****************************************************************************/

int initialize(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2, 
               struct Keywords * key, struct Params * param, int * ch_element, 
               char * chcolName, int * imsizeX, int * imsizeY, int * num_chips, 
               double * phaselength, int * n_phase, int * bthresh, 
               int * cellsize, int * impfac, double * pmax1, 
               double * pmax2, ColNum * colNums) {

  char extname[FLEN_KEYWORD] = "EVENTS";  /* Extension name for input/output file */
  char tempChar[FLEN_KEYWORD];            /* Temporary string to hold input grade list */

  int justZero = 0;                       /* Boolean variable to specify if 0 is only grade specified */
  int ii = 0, jj = 0;                     /* Iterative indexes */
  int error_status = 0;                    /* Error status (0: Normal) */

  char * pch;                             /* Temporary string to search pulse-height column name */
  size_t stringlen;                             /* Temporary string to search pulse-height column name */
  int ranges = 0;                         /* Array to hold number of ranges when searching through grade string */
  long minRange[26] = { 0 };              /* Array to hold low-end of ranges for grades */
  long maxRange[26] = { 0 };              /* Array to hold high-end of ranges for grades */
  
  double cmin = 0.0;                      /* Minimum cellsize */
  double bmax = 0.0;                      /* Maximum background size */
  
  double logprob1 = 0.0, logprob2 = 0.0;  /* Log Poisson probability threshold 
                                             for first and second iteration */ 
  /* Open the input event file */
  fits_open_file(fpin, param->m_infile, READONLY, &error_status);

  /* Check if FITS file pointer is valid */
  if(FILE_NOT_OPENED == error_status) { 
    ahlog_err(__PRETTY_FUNCTION__,"failed to open SXI EVENT file\n"); 
    return error_status; 
  }
  ahlog_info(1,__PRETTY_FUNCTION__,"Opening input file %s\n", param->m_infile);
 
  fits_movnam_hdu(*fpin, ANY_HDU, extname,0, &error_status);
  if(0 != error_status) { 
    ahlog_err(__PRETTY_FUNCTION__,"Unable to move to extension %s.\n",extname); 
    return error_status; 
  }
  ahlog_info(1,__PRETTY_FUNCTION__,"Opening EVENTS extension\n");

  /* 1. Find time column 
   *    TIME column is required when duration == yes, otherwise unnecessary */
  if(param->m_duration) {
    if(strcasecmp(param->m_timecol,"NONE") == 0) {
      ahlog_err(__PRETTY_FUNCTION__,"Time column is required when duration=yes.\n");
      return 1;
    }
    /* Get the column number for param->timecol */
    fits_get_colnum(*fpin,CASEINSEN,param->m_timecol,&key->m_timeColNum,&error_status);
    if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"TIME column not found\n"); return error_status; }
  } else if (strcasecmp(param->m_timecol,"NONE") != 0) {
    fits_get_colnum(*fpin,CASEINSEN,param->m_timecol,&key->m_timeColNum,&error_status);
    if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"TIME column not found\n"); return error_status; }
  }
 
  /* 2. Find chip column 
   * CHIP column is not necessary, but num_chips must be defined */
  if(strcasecmp(param->m_chipcol,"NONE") == 0) {
    key->m_firstchip = 0;
    *num_chips = 1;
  } else { 
    /* Find the CHIPCOL column number into key.tlmin_chip */
    fits_get_colnum(*fpin,CASEINSEN,param->m_chipcol,&key->m_chipColNum,&error_status);
    if(error_status != 0 ) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n",param->m_chipcol); return error_status; }
    ahlog_info(3,__PRETTY_FUNCTION__,"CHIPCOL = %s found\n", param->m_chipcol);
    error_status = get_min_max(fpin,"firstchip","lastchip",param->m_firstchip,
                            param->m_lastchip,key->m_chipColNum,
                            &key->m_firstchip,&key->m_lastchip);
    
    /* Calculate the chip number */
    *num_chips = key->m_lastchip - key->m_firstchip + 1;
  }
  ahlog_info(2,__PRETTY_FUNCTION__,"Number of chips: %i\n",*num_chips);

  /* 3. Find COORDX column (must exist) */
  if(strcasecmp(param->m_xcol,"NONE") == 0) { ahlog_err(__PRETTY_FUNCTION__,"X-coordinate column must be set\n"); return 1; }
  fits_get_colnum(*fpin,CASEINSEN,param->m_xcol,&key->m_xColNum,&error_status);
  if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n", param->m_xcol); return error_status; }
  ahlog_info(3,__PRETTY_FUNCTION__,"XCOL = %s found\n", param->m_xcol);
  /* Set minimum/maximum values for x-coordinates */
  error_status = get_min_max(fpin,"xmin","xmax",
                          param->m_xmin,param->m_xmax,
                          key->m_xColNum,&key->m_xmin,&key->m_xmax);
  if(error_status != 0) return error_status;
  
  /* calculate the image size and save the first pixel number */
  *imsizeX = key->m_xmax - key->m_xmin + 1;
  ahlog_info(2,__PRETTY_FUNCTION__,"Size of x-axis: %i\n",*imsizeX);

  /* 4. Find COORDY column (must exist) */
  if(strcasecmp(param->m_ycol,"NONE") == 0) { ahlog_err(__PRETTY_FUNCTION__,"Y-coordinate column must be set\n"); return 1; }
  fits_get_colnum(*fpin,CASEINSEN,param->m_ycol,&key->m_yColNum,&error_status);
  if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n",param->m_ycol); return error_status; }
  ahlog_info(3,__PRETTY_FUNCTION__,"YCOL = %s found\n", param->m_ycol);
  /* Set minimum/maximum values for y-coordinates */
  error_status = get_min_max(fpin,"ymin","ymax",
                          param->m_ymin,param->m_ymax,
                          key->m_yColNum,&key->m_ymin,&key->m_ymax);
  if(error_status != 0) return error_status;

  /* calculate the image size and save the first pixel number */
  *imsizeY = key->m_ymax - key->m_ymin + 1;
  ahlog_info(2,__PRETTY_FUNCTION__,"Size of y-axis: %i\n",*imsizeY);

  /* 5. Find pulse height column (this column is not necessary) */
  *ch_element = 0;
  key->m_chanrepeat = 0;
  if(strcasecmp(param->m_chancol,"NONE") != 0) {

    /* Search for opening user-specified pulse-height element */
    pch = strpbrk(param->m_chancol,"[");
    if(NULL != pch) { 
      /* If square-bracket is present, get user-specified pulse-height element */
      strncpy(chcolName,param->m_chancol,strcspn(param->m_chancol,"["));
      *ch_element = atol(pch+1);
    } else {
      /* If no square-bracket, save pulse-height column name */
      strcpy(chcolName,param->m_chancol);
    }

    /* Get pulse-height column number */
    if(*ch_element < 0) {
      ahlog_err(__PRETTY_FUNCTION__,"Invalid index for %s column\n",chcolName); return 1;
    } else if(*ch_element > 0){
      fits_get_colnum(*fpin,CASEINSEN,chcolName,&key->m_chanColNum,&error_status);
      if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n",chcolName); return error_status; }
    } else {
      *ch_element = 0;
      fits_get_colnum(*fpin,CASEINSEN,chcolName,&key->m_chanColNum,&error_status);
      if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n",chcolName); return error_status; }
    }
    ahlog_info(3,__PRETTY_FUNCTION__,"CHANCOL = %s found\n", param->m_chancol);
    
    /* Set minimum/maximum values for pulse-height */
    error_status = get_min_max(fpin,"chanmin","chanmax",
                            param->m_chanmin,param->m_chanmax,
                            key->m_chanColNum,&key->m_chanmin,&key->m_chanmax);
    if(error_status != 0) return error_status;

    /* Get repeat value of pulse-height, verify element within range */
    fits_get_coltypell(*fpin,key->m_chanColNum,0,&key->m_chanrepeat,0, &error_status);
    if(key->m_chanrepeat < *ch_element+1) { ahlog_err(__PRETTY_FUNCTION__,"Invalid index for %s column\n",chcolName); return 1; }
    
    ahlog_info(2,__PRETTY_FUNCTION__,"Number of channels: %i\n",key->m_chanmax-key->m_chanmin+1);
  }

  /* initialize grade flags to 0 */
  for(ii = 0; ii < 26; ++ii) param->m_gradeInt[ii] = 0;

  if(strcasecmp(param->m_gradecol,"NONE") != 0) {
    fits_get_colnum(*fpin,CASEINSEN,param->m_gradecol,&key->m_gradeColNum,&error_status);
    if(0 != error_status) { ahlog_err(__PRETTY_FUNCTION__,"%s column not found\n",param->m_gradecol); return error_status; }
    ahlog_info(3,__PRETTY_FUNCTION__,"GRADECOL = %s found\n", param->m_gradecol);

    /* Calculate the grades to be used */
    strcpy(tempChar,param->m_grade);

    /* Case: Grade 0 */
    /* If grade string starts with '-', assume that user means start from 0.*/
    if(param->m_grade[0] == '-') param->m_gradeInt[0] = 1;
    /* fits_parse_range() doesn't accept 0 as a value, need to bypass it. */
    if(tempChar[0] == '0') {
      if(strcasecmp(tempChar, "0") == 0) {
        justZero = 1;
      } else {
        /* Locate the final character and adjust the string to skip the leading 0 or ,*/
        pch=strrchr(param->m_grade,'\0');
        stringlen=pch-param->m_grade+1;
        memmove(tempChar,tempChar+1,stringlen);
        if(tempChar[0] == ',') memmove(tempChar,tempChar+1,stringlen-2);
      }
      param->m_gradeInt[0] = 1;
    }

    /* Case: Grade > 0 */
    /* If grade string starts with '-', assume that user means start from 0.*/
    if(strcasecmp(param->m_grade,"ALL") == 0) {
      ahlog_info(ahlog_get_chatter(),__PRETTY_FUNCTION__,"Including all grades\n");
      for(ii = 0; ii < 26; ++ii) param->m_gradeInt[ii] = 1;
    } else if(!justZero) {
      /* set grade flags from par file */
      /* Parse range list using fitsio function fits_parse_range */
      /* fits_parse_range is used to take row range(s) and parse the string */
      fits_parse_range(tempChar,26,26,&ranges,minRange,maxRange,&error_status);
      if(error_status == RANGE_PARSE_ERROR) { ahlog_err(__PRETTY_FUNCTION__,"Invalid range in grades\n"); return error_status; }
      /* Set grade flags to true */
      for(ii = 1; ii < 26; ++ii) {
        for(jj = 0; jj < ranges; ++jj) {
          if(ii >= minRange[jj] && ii <= maxRange[jj]) param->m_gradeInt[ii] = 1;
        }
      } /* end grade range loop */
    } /* end par file grades */
    for(ii = 0; ii < 26; ++ii) { ahlog_info(3,__PRETTY_FUNCTION__,"  Grade %i = %i\n",ii,param->m_gradeInt[ii]); }
  } /* end check on grades */

  /* Get observation times */
  fits_read_key(*fpin,TDOUBLE,"TSTART",&key->m_tstart,NULL,&error_status);
  if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"TSTART keyword not found.\n"); return error_status; }
  fits_read_key(*fpin,TDOUBLE,"TSTOP",&key->m_tstop,NULL,&error_status);
  if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"TSTOP keyword not found.\n"); return error_status; }
  ahlog_info(3,__PRETTY_FUNCTION__,"Reading TSTART and TSTOP keywords\n");
  ahlog_info(3,__PRETTY_FUNCTION__,"  TSTART = %f found\n", key->m_tstart);
  ahlog_info(3,__PRETTY_FUNCTION__,"  TSTOP  = %f found\n", key->m_tstop);
  if (key->m_tstart > key->m_tstop) {
    ahlog_err(__PRETTY_FUNCTION__,"  TSTART is greater than TSTOP\n");
    error_status = 1;
  }

  if(strcasecmp(param->m_timecol,"NONE") != 0) {
    *n_phase = param->m_n_division;
    if (*n_phase < 1) {
      ahlog_err(__PRETTY_FUNCTION__,"n_divsion must be an integer >= 1\n");
      error_status = 1;  
    }
  } else { /* if TIMECOL == NONE, ignore n_division and give  1 to n_phase */
    *n_phase = 1;
  }
  *phaselength = (key->m_tstop - key->m_tstart) / (float)*n_phase;
  ahlog_info(3,__PRETTY_FUNCTION__,"Number of phases = %d\n",*n_phase);
  ahlog_info(3,__PRETTY_FUNCTION__,"Length of each phase = %f\n",*phaselength);

  /* Check for sensible arguments and determine Poisson probability
  * threshold */
  ahlog_info(3,__PRETTY_FUNCTION__,"Checking for sensible arguments\n");

  error_status = 0; 
  *cellsize = param->m_cellsize;   /* Poisson clean cell size */
  *impfac = param->m_impfac;       /* Impact factor for gamma function */
  *bthresh = param->m_bthresh;     /* Zero background threshold */
  logprob1 = param->m_logprob1;    /* Log Poisson probability threshold for first iteration */ 
  logprob2 = param->m_logprob2;    /* Log Poisson probability threshold for second iteration */
  if (*cellsize != 0) {
    /* cellsize at 0 is a special option */
    /* cellsize must be a postive, odd integer */
    if (*cellsize <= 1 || *cellsize%2 == 0) error_status = 1;
    *pmax1 = pow(10,logprob1); /* pmax is Poisson probability threshold */
    *pmax2 = pow(10,logprob2); /* pmax is Poisson probability threshold */
    cmin = 1.0/(*cellsize*(*cellsize-1));
    if (*bthresh < 1) {
      *bthresh = 1;
      bmax = cmin;
      while (bmax > *pmax2) {
        *bthresh = *bthresh - 1;
        bmax = cmin*bmax;
      }
      /* bthresh must be greater than or equal to one */
      *bthresh = fmax(*bthresh - 1, 1);
    }
  } else {
    if(*bthresh <= 0) error_status = 1;
  }
  if (error_status == 1) { ahlog_err(__PRETTY_FUNCTION__,"Bad clean parameter input value(s)\n"); }
  ahlog_info(3,__PRETTY_FUNCTION__,"...Passed.\n");

  /* Open flickering pixel list output file, move to EVENTS and PIXELS HDUs */
  error_status = create_flickering_file(fpout1,chcolName,param,colNums);
  fits_reopen_file(*fpout1,fpout2,&error_status);
  fits_movnam_hdu(*fpout1, ANY_HDU,(char*)"EVENTS",0,&error_status);
  fits_movnam_hdu(*fpout2, ANY_HDU,(char*)"PIXELS",0,&error_status);

  writeParametersToLog();

  return error_status;

}

/****************************************************************************/

int doWork(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2,
           struct Keywords key, struct Params param, int ch_element,
           char * chcolName, int imsizeX, int imsizeY, int num_chips, 
           int bthresh, int cellsize, int impfac, 
           double pmax1, double pmax2, double phaselength, int n_phase,
           ColNum colNums) {
  
  struct Event evt;                        /* Structure to hold event data */
  
  int pixels = 0;                          /* Number of pixels to clean */
  int fix_counts = 0;                       /* Number of detected pixels */
  int all_counts = 0;                       /* Total number of counts */
  int bad_counts = 0;                       /* Number of flickering pixels */
  int ipmin = 0,ipmax = 0;                 /* Min/max window x-location where pixel event detected */ 
  int jpmin = 0,jpmax = 0;                 /* Min/max window y-location where pixel event detected*/
  int chanmin = key.m_chanmin;            /* Minimum pulse-height value */
  int chanmax = key.m_chanmax;            /* Maximum pulse-height value */
  char timeCol = 1;                        /* Boolean check to read time column */
  char chipCol = 1;                        /* Boolean check to read chip column*/
  char chanCol = 1;                        /* Boolean check to read pulse-height column*/
  char gradeCol = 1;                       /* Boolean check to read grade column*/
  int i_phase = 0;                         /* number of current phaselength */        
  double phasefirst = 0.0;                 /* Time of first phase*/
  double phaselast = 0.0;                  /* Time of last phase*/
  
  int fix = 0, all = 0, bad = 0;           /* Total number of detected pixels, counts and flickering pixels */ 
  double rate = 0;                         /* Total rate and percent */
  int ii = 0, jj = 0;                      /* Iterative indexes */
  int ci = 0, cx = 0, cy = 0;              /* Chip, x, and y coordinate indexes */
  int rownumpix = 1;                       /* Current row of output file EVENTS extension */
  int rowcount = 1;                        /* Current row of output file PIXELS extension */
  long numRows = 0;                        /* Number of rows in input file EVENTS extension */
  int error_status = 0;                     /* 0: Normal */
  char buf1[256];                          /* Buffer to hold output count string */
  char buf2[256];                          /* Buffer to hold output count string */

  /* Step 4 statistical variables */
  int n = 0, ei = 0;                       /* Number of events on pixel and event index (Step 4) */
  int anynul=0;
  char timenull;
  double * time_ev = NULL;                 /* Array to hold flickering pixel times */
  double * interval_ev = NULL;             /* Array to hold flickering pixel interval times */
  int * ph_ev = NULL;                      /* Array to hold flickering pixel event PH*/
  double prevTime = 0.0;                   /* flickering pixel previous event times */
  double timefirst = 0.0;                  /* flickering pixel start times */
  double timelast = 0.0;                   /* flickering pixel stop times */
  int evnum_good = 0;
  int phsum_good = 0;

  /* Output arrays which contain values stored in PIXELS extension columns */
  int chan_status = 0;             /* channel out of bounds status */
  int grade_status = 0;            /* Invalid grade status */
  double fraction = 0.0;           /* Duration of flickering pixel events */
  int phave = 0;                   /* average pulse height */ 

  /* Counters */
  int * fix_each = (int*)malloc(num_chips*sizeof(int)); /* Number of detected pixels per chip */
  int * all_each = (int*)malloc(num_chips*sizeof(int)); /* Number of total counts per chip */
  int * bad_each = (int*)malloc(num_chips*sizeof(int)); /* Number of detected flickering pixels per chip */
  double * rate_each = (double*)malloc(num_chips*sizeof(double)); /* Rate of detected counts */

  /* Image/Coordinate vectors */
  int *** image = (int***)malloc(num_chips*sizeof(int**));           /* Array to hold counts in each pixel */
  int *** bad_image = (int***)malloc(num_chips*sizeof(int**));        /* Array to flag bad pixels */
  int *** flag_image = (int***)malloc(num_chips*sizeof(int**));       /* Array to hold pixel flags */
  int *** phsum = (int***)malloc(num_chips*sizeof(int**));           /* Array to hold pulse-height sum */
  double *** timefirst_pix = (double***)malloc(num_chips*sizeof(double**)); /* Array to hold flickering pixel start times */
  double *** timelast_pix = (double***)malloc(num_chips*sizeof(double**)); /* Array to hold flickering pixel stop times */
  int ** chip_image = (int**)malloc(imsizeX*sizeof(int*));           /* Array to hold chip image copy */
  int ** chip_flag = (int**)malloc(imsizeX*sizeof(int*));           /* Array to hold chip flag copy */

  /* Allocate vectors of pointers  */
  for(ci = 0; ci < num_chips; ++ci) {
    image[ci] = (int**)malloc(imsizeX*sizeof(int*));
    bad_image[ci] = (int**)malloc(imsizeX*sizeof(int*));
    flag_image[ci] = (int**)malloc(imsizeX*sizeof(int*));
    phsum[ci] = (int**)malloc(imsizeX*sizeof(int*));
    if(timeCol) {
      timefirst_pix[ci] = (double**)malloc(imsizeX*sizeof(double*));
      timelast_pix[ci] = (double**)malloc(imsizeX*sizeof(double*));
    }
    for(cx = 0; cx < imsizeX; ++cx) {
      image[ci][cx] = (int*)malloc(imsizeY*sizeof(int));
      bad_image[ci][cx] = (int*)malloc(imsizeY*sizeof(int));
      flag_image[ci][cx] = (int*)malloc(imsizeY*sizeof(int));
      phsum[ci][cx] = (int*)malloc(imsizeY*sizeof(int));
      if(timeCol) {
        timefirst_pix[ci][cx] = (double*)malloc(imsizeY*sizeof(double));
        timelast_pix[ci][cx] = (double*)malloc(imsizeY*sizeof(double));
      }
      if(ci == 0) {
        chip_image[cx] = (int*)malloc(imsizeY*sizeof(int));
        chip_flag[cx] = (int*)malloc(imsizeY*sizeof(int));
      }
    }
  }

  /* set up column read/write flags */
  if(strcasecmp(param.m_timecol,"NONE") == 0) timeCol = 0;
  if(strcasecmp(param.m_chipcol,"NONE") == 0) chipCol = 0;
  if(strcasecmp(param.m_chancol,"NONE") == 0) chanCol = 0;
  if(strcasecmp(param.m_gradecol,"NONE") == 0) gradeCol = 0;

  for (i_phase = 0; i_phase < n_phase; i_phase++) { 
  
    /* Step 1: Divide the exposure time by n_phase and calculate start and end time */
    phasefirst = key.m_tstart + (phaselength * (double)i_phase); 
    phaselast = phasefirst + phaselength;
    ahlog_info(2,__PRETTY_FUNCTION__,"Phase %i\n",i_phase);
    ahlog_info(3,__PRETTY_FUNCTION__,"  Start = %f\n",phasefirst);
    ahlog_info(3,__PRETTY_FUNCTION__,"  Stop  = %f\n",phaselast);

    /* Step 2: make chip image */
    /* 2.1: put 0 in image/bad_image, and -1 in other arrays */
    for(ci = 0; ci < num_chips; ++ci) {
      for(cx = 0; cx < imsizeX; ++cx) {
        for(cy = 0; cy < imsizeY; ++cy) {
          image[ci][cx][cy] = 0;
          bad_image[ci][cx][cy] = 0;
          flag_image[ci][cx][cy] = 0;
          phsum[ci][cx][cy] = 0;
          if(timeCol) {
            timefirst_pix[ci][cx][cy] = phasefirst;
            timelast_pix[ci][cx][cy] = phaselast;
          }
        }
      }
    }

    /* 2.2: read event file and put count number in arrays */
    ahlog_info(2,__PRETTY_FUNCTION__,"  Start to read rows in the input file\n");

    /* Find number of rows from event file */
    fits_get_num_rows(*fpin,&numRows,&error_status);
    if (error_status != 0) {
      ahlog_err(__PRETTY_FUNCTION__,"  Unable to get number of rows from input file.\n");
    }

    /* Main event loop */
    for(ii = 1; ii < numRows+1; ++ii){
      if(ii%1000==0) {
        ahlog_info(3,__PRETTY_FUNCTION__,"    Reading event %i\n", ii);
      }
      if(timeCol) {
        fits_read_colnull(*fpin,TDOUBLE, key.m_timeColNum,ii,1,1,&evt.m_time,&timenull,&anynul,&error_status);
        if(timenull) {
          ahlog_info(3,__PRETTY_FUNCTION__," *** TIME column NULL in row %i. skipping row.\n",ii);
          continue;
        }
        /* Out of range, go to next event */
        if ((evt.m_time < phasefirst) || (evt.m_time > phaselast)) {
          continue;
        }
      }
      if(chipCol) fits_read_col(*fpin,TINT, key.m_chipColNum,ii,1,1,0,&evt.m_ccdid,0,&error_status);
      fits_read_col(*fpin,TINT,key.m_xColNum,ii,1,1,0,&evt.m_coordx,0,&error_status);
      fits_read_col(*fpin,TINT,key.m_yColNum,ii,1,1,0,&evt.m_coordy,0,&error_status);
      if(chanCol) { 
        /* Read CHANCOL. If par.chancol has index, like PHAS[2], ch_element indicates the element
         * to be read. Otherwise ch_element = 0 and does nothing. */
        fits_read_col(*fpin,TINT,key.m_chanColNum,ii,ch_element+1,1,0,&evt.m_chan,0,&error_status);
        /* Out of range, go to next event */
        if ((evt.m_chan < chanmin) || (evt.m_chan > chanmax)) { continue; }
      }
      if(gradeCol) {
        fits_read_col(*fpin,TINT,key.m_gradeColNum,ii,1,1,0,&evt.m_grade,0,&error_status);
        if(param.m_gradeInt[evt.m_grade] != 1) { continue; }
      }
      if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"    Error reading row %i from event file\n",ii); break; }

      if(!timeCol) evt.m_time = (phasefirst+phaselast)/2;
      if(!chipCol) evt.m_ccdid = 0;
      if(!chanCol) evt.m_chan = 0;
      if(!gradeCol) evt.m_grade = 0;

      /* Calculate coordinate values from min/max */
      ci = evt.m_ccdid - key.m_firstchip;
      cx = evt.m_coordx - key.m_xmin;
      cy = evt.m_coordy - key.m_ymin;
      /* Coordinates out of user-defined range, skipping row. 
       * This shouldn't happen with default file mins/maxs: */
      if(cx < 0 || cx >= imsizeX || cy < 0 || cy >= imsizeY || ci < 0 || ci >= num_chips) {
        continue;
      }

      /* calculate counts in each pixel*/
      image[ci][cx][cy] += 1;
      /* sum up the pulse height valued in each pixel for
       * the later calculation of average pulse height */
      phsum[ci][cx][cy] += evt.m_chan;

    } /* end event row loop */

    /* 2.3 Copy image array to bad_image array */
    for(ci = 0; ci < num_chips; ++ci) {
      for(cx = 0; cx < imsizeX; ++cx) {
        for(cy = 0; cy < imsizeY; ++cy) {
          bad_image[ci][cx][cy] = image[ci][cx][cy];
        }
      }
    }

    /* Pixels where flickering is detected will
     * eventually contain 0 value (cleaned) for the image array. */

    /* Step 3: Detect flickering pixels in each chip */
    ahlog_info(2,__PRETTY_FUNCTION__,"  Start to search flickering pixels\n");
    for(ci = 0; ci < num_chips; ++ci) {
      ahlog_info(2,__PRETTY_FUNCTION__,"    Searching for flickering pixels in chip %i\n", ci+key.m_firstchip);
      fix_each[ci] = 0;
      all_each[ci] = 0;
      bad_each[ci] = 0;
      rate_each[ci] = 0.0;
      if(error_status != 0) break; /* verify no errors */

      /* 3.1: Calculate min/max x/y locations of pixels where
       * event(s) is detected*/
      ipmin = imsizeX;
      ipmax = 1;
      jpmin = imsizeY;
      jpmax = 1;
      for(cx = 0; cx < imsizeX; ++cx) {
        for(cy = 0; cy < imsizeY; ++cy) {
          chip_image[cx][cy] = image[ci][cx][cy];
          chip_flag[cx][cy] = flag_image[ci][cx][cy];
          all_counts += chip_image[cx][cy];
          if(chip_image[cx][cy] > 0) {
            ipmin = fmin(ipmin, cx);
            ipmax = fmax(ipmax, cx);
            jpmin = fmin(jpmin, cy);
            jpmax = fmax(jpmax, cy);
          }
        }
      }

      if(0==all_counts) {
        ahlog_info(1,__PRETTY_FUNCTION__," *** No counts found on chip %i. Skipping chip.\n",ci+key.m_firstchip);
        continue;
      }
      
      /* 3.2: Search flickering pixels */
      /* normal case (cellsize > 0) */
      if(cellsize != 0) {
        ahlog_info(3,__PRETTY_FUNCTION__,"  cellsize = %i. Standard searching method is applied\n", cellsize);
        pixels = (ipmax-ipmin+1)*(jpmax-jpmin+1);
        if (pixels <= 1) { 
          ahlog_info(1,__PRETTY_FUNCTION__," *** Not enough pixels to clean, skipping chip %i\n",ci+key.m_firstchip); 
          continue; 
        }
        /* Main function */
        clean_chip(imsizeX,imsizeY,ipmin,ipmax,jpmin,jpmax,
                  pixels,pmax1,pmax2,cellsize,impfac,param.m_bthresh,param.m_iterate,
                  param.m_flagedge,&chip_image,&chip_flag,&fix_counts,&all_counts,
                  &bad_counts,&error_status);
        if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"  Error found cleaning chip %i.\n",ii); break; }
        if(0==all_counts) {
          ahlog_info(1,__PRETTY_FUNCTION__," *** During clean_chip, no counts found on chip %i. Skipping chip.\n",ci+key.m_firstchip);
          continue;
        }
      } else {
        /* When cellsize = 0, the simple-counts/pixel-cutoff will be applied */
        ahlog_info(3,__PRETTY_FUNCTION__,"  cellsize = 0. Simple cutoff is applied\n", cellsize);
        fix_counts = 0;
        all_counts = 0;
        bad_counts = 0;
        for(cx = 0; cx < imsizeX; ++cx) {
          for(cy = 0; cy < imsizeY; ++cy) {
            all_counts += chip_image[cx][cy];
            if(chip_image[cx][cy] > bthresh) {
              fix_counts += 1;
              bad_counts += chip_image[cx][cy];
              chip_image[cx][cy] = 0;
              chip_flag[cx][cy] = -1;
            }
          } /* end for cy */
        } /* end for cx */
      } /* end if-else */

      /* 3.3: Copy chip_image array to image array */
      for(cx = 0; cx < imsizeX; ++cx) {
        for(cy = 0; cy < imsizeY; ++cy) {
          image[ci][cx][cy] = chip_image[cx][cy];
          if(bad_image[ci][cx][cy] != 0) flag_image[ci][cx][cy] = chip_flag[cx][cy];
        }
      }

      /* 3.4: Calculate the number/ratio of detected flickering pixels */
      fix_each[ci] = fix_counts;
      all_each[ci] = all_counts;
      bad_each[ci] = bad_counts;

    } /* end loop on chip */
    /* End step 3 */

    /* Step 4: Determine first and last flickering events for each pixel */
    if(param.m_duration) {
      /* 4.1: Allocate and initialize step 4 */
      for(ci = 0; ci < num_chips; ++ci) {
        for(cx = 0; cx < imsizeX; ++cx) {
          for(cy = 0; cy < imsizeY; ++cy) {
            if(-1 == flag_image[ci][cx][cy]) {
              /* Number of events on this pixel */
              n = bad_image[ci][cx][cy];
              /* If n<-3, keep timefirst=phasefirst and timelast=phaselast
               * and go to the next pixel */
              if ( n <= 3 ) continue;
            
              /* Allocate the space for time, interval and ph */
              time_ev = (double*)malloc(n*sizeof(double));
              interval_ev = (double*)malloc(n*sizeof(double));
              ph_ev = (int*)malloc(n*sizeof(int));

              /* 4.1: Initialization */
              for(ei = 1; ei <= n; ++ei) {
                time_ev[ei-1] = 0.0;
                interval_ev[ei-1] = 0.0;
                ph_ev[ei-1] = 0;
              } /* end loop events */
              ei = 1; /* count up evt num in each pix */
              prevTime = phasefirst;
              timefirst = phasefirst;
              timelast = phaselast;

              /* 4.2: Loop through the event file to determine intervals between events */
              /* Read par.infile.Events extension */
              for(ii = 1; ii < numRows+1; ++ii) {
                if(chipCol) fits_read_col(*fpin,TINT, key.m_chipColNum,ii,1,1,0,&evt.m_ccdid,0,&error_status);
                else evt.m_ccdid = 0;
                fits_read_col(*fpin,TINT,key.m_xColNum,ii,1,1,0,&evt.m_coordx,0,&error_status);
                fits_read_col(*fpin,TINT,key.m_yColNum,ii,1,1,0,&evt.m_coordy,0,&error_status);
                if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"Error reading row %i from event file\n",ii); break; }

                /* Check boundaries */
                evt.m_ccdid -= key.m_firstchip;
                evt.m_coordx -= key.m_xmin;
                evt.m_coordy -= key.m_ymin;
                if(cx < 0 || cx >= imsizeX || cy < 0 || cy >= imsizeY || ci < 0 || ci >= num_chips) {
                  continue;
                }

                /* Find the corresponding pixel */
                if( ci != evt.m_ccdid  ) continue; 
                if( cx != evt.m_coordx ) continue; 
                if( cy != evt.m_coordy ) continue;

                /* Read timecol, chancol and  gradecol */
                fits_read_colnull(*fpin,TDOUBLE, key.m_timeColNum,ii,1,1,&evt.m_time,&timenull,&anynul,&error_status);
                if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"Error reading row %i from event file\n",ii); break; }
                if(timenull) { continue; }
                if(chanCol) { 
                  fits_read_col(*fpin,TINT,key.m_chanColNum,ii,ch_element+1,1,0,&evt.m_chan,0,&error_status); 
                  if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"Error reading row %i from event file\n",ii); break; }
                  if ((evt.m_chan < chanmin) || (evt.m_chan > chanmax)) { continue; }
                }
                if(gradeCol) {
                  fits_read_col(*fpin,TINT,key.m_gradeColNum,ii,1,1,0,&evt.m_grade,0,&error_status);
                  if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"Error reading row %i from event file\n",ii); break; }
                  if(param.m_gradeInt[evt.m_grade] != 1) { continue; }
                }

                time_ev[ei-1] = evt.m_time;
                interval_ev[ei-1] = evt.m_time-prevTime;
                if(chanCol) ph_ev[ei-1] = evt.m_chan;

                prevTime = evt.m_time;
                ei++;

              } /* End loop events */

              /* Now we know the intervals between events in this pixel, as well
               * as the ph (pulse height) of each event */

              /* 4.3: Determine the first and last flickering events for each detected pixel */
              duration_search(n,time_ev,interval_ev,ph_ev,
                              phasefirst,phaselast,param.m_sigma,
                              &timefirst,&timelast,&evnum_good,&phsum_good);

              timefirst_pix[ci][cx][cy] = timefirst;
              timelast_pix[ci][cx][cy] = timelast;
              bad_image[ci][cx][cy] -= evnum_good;
              bad_each[ci] -= evnum_good;
              phsum[ci][cx][cy] -= phsum_good;

              free(time_ev);
              free(interval_ev);
              free(ph_ev);
            } /* end if flag_image[ci][cx][cy] == -1 */
          } /* end loop y */
        } /* end loop x */
      } /* end loop chip */
    } /* end step 4 */

    /* Update counter and calculate rates per phase division */
    fix = 0;
    all = 0;
    bad = 0;
    rate = 0;
    for(ci = 0; ci < num_chips; ++ci) {
      fix += fix_each[ci];
      all += all_each[ci];
      bad += bad_each[ci];

      rate_each[ci] = 0;
      if( all_each[ci] != 0 ) {
        rate_each[ci] = 100.0*(double)bad_each[ci]/(double)all_each[ci];
      }
    }
    if ( all != 0 ) { rate = 100.0*(double)bad/(double)all; }

    /* Step 5: Output */
    ahlog_info(1,__PRETTY_FUNCTION__,"  Writing output results in %s\n", param.m_outfile);
    if(!param.m_cleanimg) {
      ahlog_info(3,__PRETTY_FUNCTION__,"  cleanimg=no: Only flickering events are written\n");
    } else {
      ahlog_info(3,__PRETTY_FUNCTION__,"  cleanimg=yes: Non-flickering events are written\n");
    }

    /* Start loop over events */
    for(ii = 1; ii < numRows+1; ++ii) {
      /* verify no errors. cfitsio functions automatically return if nonzero status is input*/
      if(error_status != 0) break; 
      chan_status = 0;
      grade_status = 0;
      if(timeCol) { 
        fits_read_colnull(*fpin,TDOUBLE, key.m_timeColNum,ii,1,1,&evt.m_time,&timenull,&anynul,&error_status);
        if(timenull) { continue; }
        if (evt.m_time < phasefirst) { continue; }
        /* if evt.m_time > phaselast, we are past the end of the phase and can skip to the next phase */
        if (evt.m_time > phaselast) { continue; } /* file does not need to be time-ordered */
      }
      if(chipCol != 0) fits_read_col(*fpin,TINT, key.m_chipColNum,ii,1,1,0,&evt.m_ccdid,0,&error_status);
      else evt.m_ccdid = 0;
      fits_read_col(*fpin,TINT,key.m_xColNum,ii,1,1,0,&evt.m_coordx,0,&error_status);
      fits_read_col(*fpin,TINT,key.m_yColNum,ii,1,1,0,&evt.m_coordy,0,&error_status);
      if(chanCol) {
        /* Read CHANCOL. If par.chancol has index, like PHAS[2], ch_element indicates the element
         * to be read. Otherwise ch_element = 0 and does nothing. */
        fits_read_col(*fpin,TINT,key.m_chanColNum,ii,ch_element+1,1,0,&evt.m_chan,0,&error_status);
        /* Out of range, go to next event */
        if ((evt.m_chan < chanmin) || (evt.m_chan > chanmax)) { chan_status = 1; }
      }
      if(gradeCol) {
        fits_read_col(*fpin,TINT,key.m_gradeColNum,ii,1,1,0,&evt.m_grade,0,&error_status);
        if(param.m_gradeInt[evt.m_grade] != 1) { grade_status = 1; }
      }
      if(error_status != 0) { ahlog_err(__PRETTY_FUNCTION__,"    Error reading row %i from event file\n",ii); break; }

      /* Set up coordinates */
      ci = evt.m_ccdid-key.m_firstchip;
      cx = evt.m_coordx-key.m_xmin;
      cy = evt.m_coordy-key.m_ymin;
      if(cx < 0 || cx >= imsizeX || cy < 0 || cy >= imsizeY || ci < 0 || ci >= num_chips) {
        continue;
      }
    
      /* Write to the EVENTS extension */
      if(!param.m_cleanimg) {
        if(flag_image[ci][cx][cy] == -1 || flag_image[ci][cx][cy] == 1 ) {
          /* Copy only flickering pixels to outfile */
          if(chan_status == 0 && grade_status == 0) {
	    if(evt.m_time >= timefirst_pix[ci][cx][cy] && 
               evt.m_time <= timelast_pix[ci][cx][cy]) {
              if(rowcount%1000==0) { ahlog_info(3,__PRETTY_FUNCTION__,"    Writing event %i to first extension\n", rowcount); }
              ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"    Writing flickering pixel (chip,x,y) = (%i,%i,%i) to first extension\n",evt.m_ccdid,evt.m_coordx,evt.m_coordy);
              fits_copy_rows(*fpin,*fpout1,ii,1,&error_status);
              rowcount++;
	    } /* end time interval check */
          } /* end channel/grade status check */
        } /* end flickering event */
      } else {
        /* only non-flickering pixels are copied */
        if(flag_image[ci][cx][cy] == 0) {
          if(rowcount%1000==0) { ahlog_info(3,__PRETTY_FUNCTION__,"    Writing event %i to first extension\n", rowcount); }
          ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"    Writing non-flickering pixel (chip,x,y) = (%i,%i,%i) to first extension\n",evt.m_ccdid,evt.m_coordx,evt.m_coordy);
          fits_copy_rows(*fpin,*fpout1,ii,1,&error_status);
          rowcount++;
        } else if(chan_status == 1 || grade_status == 1) {
          if(rowcount%1000==0) { ahlog_info(3,__PRETTY_FUNCTION__,"    Writing event %i to first extension\n", rowcount); }
          ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"    Writing non-flickering pixel (chip,x,y) = (%i,%i,%i) to first extension\n",evt.m_ccdid,evt.m_coordx,evt.m_coordy);
          fits_copy_rows(*fpin,*fpout1,ii,1,&error_status);
          rowcount++;
        } else if(evt.m_time < timefirst_pix[ci][cx][cy] || 
                  evt.m_time > timelast_pix[ci][cx][cy]) {
          if(rowcount%1000==0) { ahlog_info(3,__PRETTY_FUNCTION__,"    Writing event %i to first extension\n", rowcount); }
          ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"    Writing non-flickering pixel (chip,x,y) = (%i,%i,%i) to first extension\n",evt.m_ccdid,evt.m_coordx,evt.m_coordy);
          fits_copy_rows(*fpin,*fpout1,ii,1,&error_status);
          rowcount++;
        }
      }
   
      /* Second Extension: only the first event in each pixel is copied */
      if(flag_image[ci][cx][cy] == -1) {
        if(rownumpix%1000==0) { ahlog_info(3,__PRETTY_FUNCTION__,"    Writing event %i to second extension\n", rownumpix); }
        ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"    Writing flickering pixel (chip,x,y) = (%i,%i,%i) to second extension\n",evt.m_ccdid,evt.m_coordx,evt.m_coordy);
        /*Write START STOP and FRACTION*/
        if (0 != fits_write_col(*fpout2,TDOUBLE,colNums.m_time,rownumpix,1,1,&timefirst_pix[ci][cx][cy],&error_status)) {
          ahlog_err(__PRETTY_FUNCTION__,"unable to write to column START error status %i \n",error_status); 
        } 
        if (0 != fits_write_col(*fpout2,TDOUBLE,colNums.m_timelast,rownumpix,1,1,&timelast_pix[ci][cx][cy],&error_status)) {
         ahlog_err(__PRETTY_FUNCTION__,"unable to write to column STOP error status %i \n",error_status); 
        } 
        if (chipCol) {
          if (0 != fits_write_col(*fpout2,TINT,colNums.m_chipcol,rownumpix,1,1,&evt.m_ccdid,&error_status)) {
           ahlog_err(__PRETTY_FUNCTION__,"unable to write to column %s error status %i \n",param.m_chipcol,error_status); 
          } 
        }
        if (0 != fits_write_col(*fpout2,TINT,colNums.m_rawx,rownumpix,1,1,&evt.m_coordx,&error_status)) { 
          ahlog_err(__PRETTY_FUNCTION__,"unable to write to column %s error status %i \n",param.m_xcol,error_status); 
        } 
        if (0 != fits_write_col(*fpout2,TINT,colNums.m_rawy,rownumpix,1,1,&evt.m_coordy,&error_status)) {
          ahlog_err(__PRETTY_FUNCTION__,"unable to write to column %s error status %i \n",param.m_ycol,error_status); 
        } 
        if (0 != fits_write_col(*fpout2,TINT,colNums.m_counts,rownumpix,1,1,&bad_image[ci][cx][cy],&error_status)) {
          ahlog_err(__PRETTY_FUNCTION__,"unable to write to column COUNTS error status %i \n",error_status); 
        } 
        if(param.m_duration) {
          fraction = (timelast_pix[ci][cx][cy] - timefirst_pix[ci][cx][cy]) / (key.m_tstop-key.m_tstart);  
          if (0 != fits_write_col(*fpout2,TDOUBLE,colNums.m_fraction,rownumpix,1,1,&fraction,&error_status)) {
            ahlog_err(__PRETTY_FUNCTION__,"unable to write to column FRACTION error status %i \n",error_status); 
          } 
        }
        /* Calculate the PHAS value vs number of counts to get average */
        if (chanCol) {
          phave = phsum[ci][cx][cy]/bad_image[ci][cx][cy];
          if (0 != fits_write_col(*fpout2,TINT,colNums.m_chancol,rownumpix,1,1,&phave,&error_status)) {
            ahlog_err(__PRETTY_FUNCTION__,"unable to write to column %s error status %i \n",param.m_chancol,error_status); 
          } 
        }
        /* This pixel will no longer be written into second extension */
        flag_image[ci][cx][cy] = 1;
        ++rownumpix;
      } /* end second extension writing */
    } /* End event file loop */
    
    /* Print Results */
    /* Printout needs to be dynamic and print to log file. 
     * Loops save temporary char arrays to full string
     * and print out with ahlog. */
    ahlog_out(__PRETTY_FUNCTION__,"Phase %i\n",i_phase);
    ahlog_out(__PRETTY_FUNCTION__,"Start time = %f\n",phasefirst);
    ahlog_out(__PRETTY_FUNCTION__,"Stop time  = %f\n",phaselast);
    snprintf(buf1,sizeof buf1,"Phase %i",i_phase);
    fits_write_history(*fpout2,buf1,&error_status);
    snprintf(buf1,sizeof buf1,"Start time %f",phasefirst);
    fits_write_history(*fpout2,buf1,&error_status);
    snprintf(buf1,sizeof buf1,"Stop time  %f",phaselast);
    fits_write_history(*fpout2,buf1,&error_status);
    snprintf(buf1,sizeof buf1,"%30s"," ");
    for(ci = 0; ci < num_chips; ++ci) {
      if (strcasecmp(param.m_chipcol,"NONE") != 0) {
        snprintf(buf2,sizeof buf2,"%s%8s%2i ",buf1,param.m_chipcol,ci+key.m_firstchip);
      } else {
        snprintf(buf2,sizeof buf2,"%s%8s%2i ",buf1,"CCD",ci+key.m_firstchip);
      }
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    if(num_chips>1) {
      snprintf(buf2,sizeof buf2,"%s%10s ",buf1,"Total");
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    ahlog_out(__PRETTY_FUNCTION__,"%s\n",buf2);
    fits_write_history(*fpout2,buf2,&error_status);

    snprintf(buf1,sizeof buf1,"%30s","Number of flickering pixels:");
    for(ci = 0; ci < num_chips; ++ci) {
      snprintf(buf2,256,"%s%10i ",buf1,fix_each[ci]);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    if(num_chips>1) {
      snprintf(buf2,256,"%s%10i ",buf1,fix);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    ahlog_out(__PRETTY_FUNCTION__,"%s\n",buf2);
    fits_write_history(*fpout2,buf2,&error_status);
 
    snprintf(buf1,sizeof buf1,"%30s","Number of counts:");
    for(ci = 0; ci < num_chips; ++ci) {
      snprintf(buf2,sizeof buf2,"%s%10i ",buf1,all_each[ci]);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    if(num_chips>1) {
      snprintf(buf2,256,"%s%10i ",buf1,all);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    ahlog_out(__PRETTY_FUNCTION__,"%s\n",buf2);
    fits_write_history(*fpout2,buf2,&error_status);
    snprintf(buf1,sizeof buf1,"%30s","Number of detected counts:");
    for(ci = 0; ci < num_chips; ++ci) {
      snprintf(buf2,sizeof buf2,"%s%10i ",buf1,bad_each[ci]);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    if(num_chips>1) {
      snprintf(buf2,256,"%s%10i ",buf1,bad);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    ahlog_out(__PRETTY_FUNCTION__,"%s\n",buf2);
    fits_write_history(*fpout2,buf2,&error_status);

    snprintf(buf1,sizeof buf1,"%30s","Rate of detected counts:");
    for(ci = 0; ci < num_chips; ++ci) {
      snprintf(buf2,sizeof buf2,"%s%10.5f ",buf1,rate_each[ci]);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    if(num_chips>1) {
      snprintf(buf2,256,"%s%10.5f ",buf1,rate);
      snprintf(buf1,sizeof buf1,"%s",buf2);
    }
    ahlog_out(__PRETTY_FUNCTION__,"%s\n",buf2);
    fits_write_history(*fpout2,buf2,&error_status);


  }/*-------------------------------------end loop over i_phase----------------------------*/  

  /* deallocate vectors */
  for(ii = 0; ii < num_chips; ++ii) {
    for(jj = 0; jj < imsizeX; ++jj) {
      free(image[ii][jj]);
      free(bad_image[ii][jj]);
      free(phsum[ii][jj]);
      free(flag_image[ii][jj]);
      if(timeCol) {
        free(timefirst_pix[ii][jj]);
        free(timelast_pix[ii][jj]);
      }
      if(ii == 0) {
        free(chip_image[jj]);
      }
    }
    free(image[ii]);
    free(bad_image[ii]);
    free(phsum[ii]);
    free(flag_image[ii]);
    if(timeCol) {
      free(timefirst_pix[ii]);
      free(timelast_pix[ii]);
    }
  }
  free(image);
  free(bad_image);
  free(phsum);
  free(chip_image);
  free(flag_image);

  free(fix_each);
  free(all_each);
  free(bad_each);
  free(rate_each);

  if(timeCol) {
    free(timefirst_pix);
    free(timelast_pix);
  }

  return error_status ? error_status : 0;

} /* end doWork */

/****************************************************************************/

int finalize(fitsfile ** fpin, fitsfile ** fpout1, fitsfile ** fpout2) {

  int error_status = 0;                     /* 0: Normal */
  
  /* Close all FITS file pointers */
  fits_close_file(*fpin,&error_status);
  if(FILE_NOT_CLOSED != error_status) fpin = 0;
  fits_close_file(*fpout1,&error_status);
  if(FILE_NOT_CLOSED != error_status) fpout1 = 0;
  if(fpout1 != 0) fits_close_file(*fpout2,&error_status);
  if(FILE_NOT_CLOSED != error_status) fpout2 = 0;
  if(0 == error_status) ahlog_out(__PRETTY_FUNCTION__,"Finished.\n");
  
  return error_status ? error_status : 0;
}

/****************************************************************************/

int clean_chip(int imsizeX, int imsizeY, int ipmin, int ipmax, int jpmin,
              int jpmax, int pixels, double pmax1, double pmax2, int cellsize, int impfac, 
              int bthresh, int iterate, int flagedge, int *** chip_image, int *** chip_flag,
              int * fix_counts, int * all_counts, int * bad_counts, int * error_status) {

  int zz = 0;                              /* Temporary variable to hold pixel counts */
  int prefix = 0;                          /* Number of cleaned pixels */
  int prebad = 0;                          /* Number of flickering pixels */
  int npix = 0;                            /* Number of background pixels */
  int ii = 0, jj = 0;                      /* Iterative indexes */
  int iCell = 0, jCell = 0;                /* Cellsize index */
  int iFlag = 0, jFlag = 0;                /* Flag index */
  double bgdTemp = 0;                      /* Temporary variable to hold background counts */
  double ftgammq = 0.0;                    /* Gamma function output */
  double img = 0;                          /* Number of counts in pixel */
                                           
  double bac = 0;                          /* Background input for gamma function */
  double pixelProb = 0;                    /* Probability of flickering pixel */
  double ** bgd = (double**)malloc(imsizeX*sizeof(double*));        /* Array to hold background counts */

  *fix_counts = 0;
  *all_counts = 0;
  *bad_counts = 0;

  /* allocate/initialize bgd array, get total counts in image */
  for(ii = 0; ii < imsizeX; ++ii) {
    bgd[ii] = (double*)malloc(imsizeY*sizeof(double));
    for(jj = 0; jj < imsizeY; ++jj) {
      bgd[ii][jj] = 0;
      zz = (*chip_image)[ii][jj];
      *all_counts = *all_counts + zz;
    }
  }
  
  if (0 == *all_counts) {
    for(jj = 0; jj < imsizeX; ++jj) { free(bgd[jj]); } free(bgd);
    return 0;
  }

  /* First Step: detect flickering pixels */
  /* Counts in each pixel are compared to the mean counts in the chip,
   * and the pixels with improbable counts are detected. */
  if(pmax1 <= 1) { /* if param.m_logprob1 is positive, this step is skipped */
    ahlog_info(3,__PRETTY_FUNCTION__,"  First step: Counts in each pixel are compared to the mean counts in the entire chip\n");
    for(ii = 0; ii < imsizeX; ++ii) {
      for(jj = 0; jj < imsizeY; ++jj) {
        img = (double)(*chip_image)[ii][jj];
        bac = impfac*(double)(*all_counts)/(double)(pixels-1);
        ftgammq = FT_GAMMQ(img+1.0, bac, *error_status);
        pixelProb = 1.0 - ftgammq;
        if (pixelProb < pmax1) {
          prefix = prefix + 1;
          prebad = prebad + (*chip_image)[ii][jj];
          (*chip_image)[ii][jj] = 0;
          (*chip_flag)[ii][jj] = -1;
        }
      }
    }
  }
  if (*error_status != 0) {
    ahlog_err(__PRETTY_FUNCTION__,"During clean_chip, error removing flickering pixel.\n");
    for(jj = 0; jj < imsizeX; ++jj) { free(bgd[jj]); } free(bgd);
    return *error_status;
  }

  /* Update number of counts */
  *fix_counts = *fix_counts + prefix;
  *bad_counts = *bad_counts + prebad;

  /* Second Step: Detect statistical anomalies by comparing to surrounding pixels
   * the counts in each pixel are compared to the surrounding (cellsize x cellsize) pixels
   * and the test pixels for which the Poisson probability exceeds the threshold
   * given by the "logprob" parameter are detected as well */
  if(pmax2 <= 1) {
    ahlog_info(3,__PRETTY_FUNCTION__,"  Second step: Counts in each pixel are compared to the counts in surrounding pixels\n");
    prefix = -1;
    while (prefix != 0 && *error_status == 0) {
      prefix = 0;
      prebad = 0;
      /* Create bgd image */
      for(ii = ipmin; ii <= ipmax; ++ii) {
        for(jj = jpmin; jj <= jpmax; ++jj) {
          img = 0;
          npix = 0;
          bgdTemp = 0;
          for(iCell = 1; iCell <= cellsize; ++iCell) {
            for(jCell = 1; jCell <= cellsize; ++jCell) {
              iFlag = fmin(fmax(ii+iCell-cellsize/2 - 1,ipmin),ipmax);
              jFlag = fmin(fmax(jj+jCell-cellsize/2 - 1,jpmin),jpmax);
              if((*chip_flag)[iFlag][jFlag] == 0) {
                img = img + (double)(*chip_image)[iFlag][jFlag];
                npix = npix + 1;
              } /* end if */
            } /* end jCell loop */
          } /* end iCell loop */
          bgdTemp = img - (double)(*chip_image)[ii][jj];
          bgdTemp = bgdTemp/((double)npix-1.0);
          bgd[ii][jj] = bgdTemp;
        } /* end ii */
      } /* end jj */

      /* search statistical anomalies */
      for(ii = ipmin; ii <= ipmax; ++ii) {
        for(jj = jpmin; jj <= jpmax; ++jj) {
          img = (double)(*chip_image)[ii][jj];
          bac = (double)bgd[ii][jj];
          if(img > bac) {
            if(!(bac == 0.0 && img < bthresh)) {
              ftgammq = FT_GAMMQ(img+1.0, bac, *error_status);
              pixelProb = 1.0 - ftgammq;
              if (pixelProb < pmax2) {
                prefix = prefix + 1;
                prebad = prebad + (*chip_image)[ii][jj];
                (*chip_image)[ii][jj] = 0;
                (*chip_flag)[ii][jj] = -1;
              }
            }
          } /* end if (img > bac) */
        } /* end jj */
      } /* end ii */
      if(prefix != 0) {
        *fix_counts = *fix_counts + prefix;
        *bad_counts = *bad_counts + prebad;
      }
      if(!iterate) prefix = 0;
    } /* end while loop */
  } /* end if (pmax2 <= 1) */

  /* Third Step (optional): flag zero edge pixel (edge pixels are considered as flickering pixel) */
  if (flagedge) {
    ahlog_info(3,__PRETTY_FUNCTION__,"  Third step: Detecting edge pixels\n");
    prefix = 0;
    prebad = 0;
    for(ii = ipmin; ii <= ipmax; ++ii) {
      for(jj = jpmin; jj <= jpmax; ++jj) {
        if(ii < ipmin+cellsize/2 || ii > ipmax-cellsize/2 ||
           jj < jpmin+cellsize/2 || jj > jpmax-cellsize/2) {
          prefix = prefix + 1;
          prebad = prebad + (*chip_image)[ii][jj];
          (*chip_image)[ii][jj] = 0;
          (*chip_flag)[ii][jj] = -1;
        } /* end if */
      } /* end jj */
    } /* end ii */
    *fix_counts = *fix_counts + prefix;
    *bad_counts = *bad_counts + prebad;
  } /* end flagedge */

  for(jj = 0; jj < imsizeX; ++jj) { free(bgd[jj]); } free(bgd);
  return *error_status;
}

int duration_search (int n, double * time_evt, double * interval_evt, int * ph_evt,
                     double phasefirst, double phaselast, double sigma,
                     double * timefirst, double * timelast, int * evnum_good,
                     int * phsum_good) {

  double average = 0;
  double min_sum_sqerr = 0;
  double sum_sqerr = 0;
  double sum_int_in = 0, sum_int_out = 0;
  double sqerr_in = 0, sqerr_out = 0;
  double sum_sqerr_in=0, sum_sqerr_out=0;                                   
  int t1 = 0, t2 = 0;
  int p = 0, q = 0;
  double int_in = 0, int_out = 0;
  double ave_int_in = 0, ave_int_out = 0;
  double sq_diff = 0, sq_diff_err = 0;
  int ei = 0;

  /* Initialization */
  *timefirst = phasefirst;
  *timelast = phaselast;
  *evnum_good = 0;
  *phsum_good = 0;

  /* Average of (n-1) intervals. Note that interval(1) is that between the first
   * event and phasefirst, so not accounted for here. */
  average = (time_evt[n-1]-time_evt[1])/(n-1);
  
  /* tentative values */
  min_sum_sqerr = (phaselast - phasefirst) * (phaselast - phasefirst);
  t1 = 0;
  t2 = n-1;
  int_in = average;
  int_out = average;
  sqerr_in = 0;
  sqerr_out = 0;

  for(p = 1; p < n-1; ++p) {
    for(q = p+1; q < n; ++q) {
      sum_int_in = 0;
      sum_int_out = 0;
      sum_sqerr_in=0;
      sum_sqerr_out=0;
      sum_sqerr = 0;
      /* Calculate total inner and outer interval time (In) of flickering events */
      for(ei = 2; ei <= n; ++ei) {
        if(ei > p && ei <= q) {
          sum_int_in += interval_evt[ei-1];
        } else {
          sum_int_out += interval_evt[ei-1];
        } /* end event within p and q */
      } /* end loop events */

      ave_int_in = sum_int_in/(q-p);        /* A: Average of all the inner event time intervals */
      ave_int_out = sum_int_out/(n-q+p-1);  /* B: Average of all the outer event time intervals */
      for(ei = 2; ei <= n; ++ei) {
        /* Calculate total squared error: s = sum((In-A)^2 + (In-B)) */
        if(ei > p && ei <= q) {
          sum_sqerr_in += (interval_evt[ei-1]-ave_int_in)*(interval_evt[ei-1]-ave_int_in);
        } else {
          sum_sqerr_out += (interval_evt[ei-1]-ave_int_out)*(interval_evt[ei-1]-ave_int_out);
        } /* end event within p and q */
      } /* end loop events */

      sum_sqerr = sum_sqerr_in + sum_sqerr_out;

      if(sum_sqerr < min_sum_sqerr) {
        min_sum_sqerr = sum_sqerr;
        t1 = p;
        t2 = q;
        int_in = ave_int_in;
        int_out = ave_int_out;
        sqerr_in = sum_sqerr_in/(q-p);
        sqerr_out = sum_sqerr_out/(n-q+p-1);
      } /* end set values */
    } /* end loop q */
  } /* end loop p */
  
  /* We've determined t1 and t2 which give the minimum sum_sqerr,
   * int_in, and int_out are the average of interval in two 
   * periods for t1 and t2 */
  /* squared difference of two averages: (A-B)^2*/
  sq_diff = (int_in-int_out)*(int_in-int_out);
  /* squared error of difference: A+B */
  sq_diff_err = int_in+int_out;

  if(sq_diff >= sigma * sigma * sq_diff_err) {
    if(t1 == 1) {
      if(int_in < int_out) { /* flickering occurred in the former part */
        *timelast = time_evt[t2-1];
        /* count up the number of ph and good events */
        for(ei = t2+1; ei <= n; ++ei) { /* these are good events */
          *evnum_good += 1;
          *phsum_good += ph_evt[ei-1];
        }
      } else { /* flickering event occurred in the latter part */
        *timefirst = time_evt[t2-1];
        for(ei = 1; ei < t2; ++ei) { /* these are good events, remove from lists */
          *evnum_good += 1;
          *phsum_good += ph_evt[ei-1];
        }
      }
    } else { /* t1 > 1 */
      if(int_in < int_out) { /* flickering occurred in the middle */
        *timefirst = time_evt[t1-1];
        *timelast = time_evt[t2-1];
        for(ei = 1; ei < t1; ++ei) { /* these are good events, remove from lists */
          *evnum_good += 1;
          *phsum_good += ph_evt[ei-1];
        }
        for(ei = t2+1; ei <= n; ++ei) { /* these are also good events, remove from lists */
          *evnum_good += 1;
          *phsum_good += ph_evt[ei-1];
        }
      }
    }
  } /* end if sq_diff > sigma^2 * sq_diff_err */

  return 0;

}

/****************************************************************************/

int create_flickering_file(fitsfile ** fpout1, char * chcolName, struct Params * param, ColNum * colNums) {

  int error_status = 0;                     /* 0: normal */

  char * outfile;                          /* Temporary string to hold outfile name */
  char * infile;                           /* Temporary string to hold outfile name */
  fitsfile * fpin = NULL;                  /* File pointer to event input file */
  long long naxis2 = 0LL;
  int colnum = 0;                          /* Column number for PIXELS extension */
  int colnum_xcol = 0;                     /* Column number for xcol in EVENTS extension */
  int colnum_ycol = 0;                     /* Column number for ycol in EVENTS extension */

  /* WCS keywords for xcol and ycol in PIXELS extension */
  char * keyroot;                         /* root of keyword name */
 

  /* Keyword strings for TELESCOP, INSTRUME, DETNAM, DATAMODE */
  char card[FLEN_CARD];
  int ikey = 0;
  int keysexist = 0;
  int morekeys = 0;
  int cardtype = 0;

  /* intialize PIXELS extension column numbers */
  colNums->m_time = 0;                  
  colNums->m_timelast = 0;               
  colNums->m_chipcol = 0;                 
  colNums->m_rawx = 0;                  
  colNums->m_rawy = 0;                   
  colNums->m_counts = 0;                  
  colNums->m_fraction = 0;               
  colNums->m_chancol = 0;   

  /* Allocate memory for outfile name */
  outfile = (char*)malloc(FLEN_FILENAME*sizeof(char));
  infile = (char*)malloc(FLEN_FILENAME*sizeof(char));

  /* Check for clobbering */
  strcpy(outfile,param->m_outfile);
  strcpy(infile,param->m_infile);
  headas_clobberfile(outfile);

  /* Create new flickering pixel list FITS file */
  fits_create_file(fpout1,outfile,&error_status);
  if(FILE_NOT_CREATED == error_status) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to create flickering file. File exists?\n");
    free(outfile);
    free(infile);
    return error_status;
  }
  /* Free outfile string memory */
  free(outfile);

  /* Set up infile name, with filter to not copy any row data to EVENTS extension. */
  /* Open infile with filtered data */
  if (0 != fits_open_file(&fpin,infile,READONLY,&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to open file %s error status %i \n",infile,error_status);
  }
  if (0 != fits_movnam_hdu(fpin,ANY_HDU,"EVENTS",0,&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to move to extension EVENTS error status %i \n",error_status);
  } 

  /* Copy columns and keywords from event file to outfile HDU1 */
  /* Stamp parameters to output file */
  fits_copy_header(fpin,*fpout1,&error_status);
  fits_update_key(*fpout1,TLONGLONG,"NAXIS2",&naxis2,"",&error_status);
  HDpar_stamp(*fpout1,0,&error_status);
   
  if (0 != fits_create_tbl(*fpout1,BINARY_TBL,0LL,0,NULL,NULL,NULL,(char*)"PIXELS",&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to move to extension EVENTS error status %i \n",error_status);
  }

  /* Copy the infile keywords. Skip copying of keywords if any are missing */
  /* Calculate total number of keywords */
  fits_get_hdrspace (fpin, &keysexist, &morekeys, &error_status);
  for ( ikey = 1; ikey < keysexist; ++ikey ) {
    /* Read each record in the input file*/
    fits_read_record (fpin, ikey, card, &error_status);
    /* Determine the type of keyword. We do not want keywords such as EXTNAME, NAXIS2, etc. */
    cardtype = fits_get_keyclass (card);
    if (cardtype == TYP_REFSYS_KEY ||
        cardtype == TYP_COMM_KEY ||
        cardtype == TYP_CONT_KEY ||
        cardtype == TYP_USER_KEY ) {
      /* Write the keyword to the output file */
      if(0 != fits_write_record (*fpout1, card, &error_status)) {
          ahlog_err(__PRETTY_FUNCTION__,"unable to copy header from infile to extension PIXELS error status %i \n",error_status);
        }
    }
  }
  HDpar_stamp(*fpout1,0,&error_status);

  if (0 != fits_insert_col(*fpout1,++colnum,(char*)"START",(char*)"1D",&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
  } else {
    colNums->m_time = colnum;
  }
  if (0 != fits_insert_col(*fpout1,++colnum,(char*)"STOP",(char*)"1D",&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
  } else {
    colNums->m_timelast = colnum;
  }
  if (strcasecmp(param->m_chipcol,"NONE") != 0) {
    if (0 != fits_insert_col(*fpout1,++colnum,(char*)param->m_chipcol,(char*)"1B",&error_status)) {
      ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
    } else {
      colNums->m_chipcol = colnum;
    }
  }
  if (0 != fits_insert_col(*fpout1,++colnum,(char*)param->m_xcol,(char*)"1I",&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
  } else {
    colNums->m_rawx = colnum;
  } 
  if (0 != fits_insert_col(*fpout1,++colnum,(char*)param->m_ycol,(char*)"1I",&error_status)) { 
    ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
  } else {
    colNums->m_rawy = colnum;
  }
  if (0 != fits_insert_col(*fpout1,++colnum,(char*)"COUNTS",(char*)"1I",&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
  } else {
    colNums->m_counts = colnum;
  } 
  if(param->m_duration) {
    if (0 != fits_insert_col(*fpout1,++colnum,(char*)"FRACTION",(char*)"1D",&error_status)) {
      ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status); 
    } else {
      colNums->m_fraction = colnum;
    } 
  }
  if (strcasecmp(param->m_chancol,"NONE") != 0) {
    if (0 != fits_insert_col(*fpout1,++colnum,(char*)chcolName,(char*)"1I",&error_status)) {
      ahlog_err(__PRETTY_FUNCTION__,"unable to insert column error status %i \n",error_status);
    } else {
      colNums->m_chancol = colnum;
    }

  }
  
  /* Insert correct WCS keywords for xcol and ycol in PIXELS extension */
  
  /* Get column number in EVENTS extension of xcol and ycol */
  fits_get_colnum(fpin,CASEINSEN,param->m_xcol,&colnum_xcol,&error_status);
  fits_get_colnum(fpin,CASEINSEN,param->m_ycol,&colnum_ycol,&error_status);

  /* Construct the Name of each WCS keyword as it appears in the EVENTS extension
     then store the value and write it (using the correct number for the keyword
     name) to the output PIXEL extension header */
  
  keyroot = "TCRPX";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);
  
  keyroot = "TCRVL";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);
  
  keyroot = "TCDLT";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);

  keyroot = "TCTYP";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);

  keyroot = "TCUNI";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);
 
  keyroot = "TNULL";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);

  keyroot = "TLMIN";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);

  keyroot = "TLMAX";
  writeWCSKeys(fpin, fpout1, keyroot,colnum_xcol,
               colnum_ycol, colNums, &error_status);

  /* Close filtered event file pointer */
  if (0 != fits_close_file(fpin,&error_status)) {
    ahlog_err(__PRETTY_FUNCTION__,"unable to close file error status %i \n",error_status);
  }


  /* Free outfile string memory */
  free(infile);

  return error_status;

}

/****************************************************************************/

int get_min_max(fitsfile ** fpin, char * parmin, char * parmax, char * parstrMin,
              char * parstrMax, int colnum, int * min, int * max) {

  long minInt = 0;                       /* Temporary minimum value variable */
  long maxInt = 0;                       /* Temporary maximum value variable */
  char colname[FLEN_KEYWORD];            /* String to hold column name */
  char ttype[FLEN_KEYWORD];              /* String to hold TFORM## keyword */
  char tlmin[FLEN_KEYWORD];              /* String to hold TLMIN## keyword */
  char tlmax[FLEN_KEYWORD];              /* String to hold TLMAX## keyword */
  int error_status = 0;                   /* 0: Normal */

  /* Get absolute minimum/maximum values from FITS input file */
  sprintf(tlmin,"TLMIN%i",colnum);
  sprintf(tlmax,"TLMAX%i",colnum);
  sprintf(ttype,"TTYPE%i",colnum);

  fits_read_key(*fpin,TSTRING,ttype,&colname,NULL,&error_status);

  /* Determine if user-defined min or FITS min */
  if(strcasecmp(parstrMin,"TLMIN") == 0) {
    /* Read keyword TLMIN## */
    fits_read_key(*fpin,TLONG,tlmin,&minInt,NULL,&error_status);
    if(error_status != 0 ) { 
      ahlog_err(__PRETTY_FUNCTION__,"  %s keyword not found for column %s\n",tlmin,colname); 
      return error_status; 
    }
    ahlog_info(3,__PRETTY_FUNCTION__,"  Using %s keyword (value = %d) for %s\n",tlmin,minInt,parmin);
    *min = minInt;
  } else {
    /* Convert parameter to integer */
    *min = atol(parstrMin);
    /* Read keyword TLMIN## */
    fits_read_key(*fpin,TLONG,tlmin,&minInt,NULL,&error_status);
    if(error_status != 0 ) { 
      ahlog_out(__PRETTY_FUNCTION__,"*** %s keyword not found for column %s. Standard FITS format?\n",tlmin,colname); 
    } else if ( *min == minInt ) {
      ahlog_out(__PRETTY_FUNCTION__,"*** Make sure %s value doesn't match %s\n",parmin, tlmin); 
    } else if ( *min < minInt ) {
      *min = minInt;
    }
    /* reset error status */
    error_status=0;
  }

  /* Determine if user-defined min or FITS min */
  if(strcasecmp(parstrMax,"TLMAX") == 0) {
    /* Read keyword TLMIN## */
    fits_read_key(*fpin,TLONG,tlmax,&maxInt,NULL,&error_status);
    if(error_status != 0 ) { 
      ahlog_err(__PRETTY_FUNCTION__,"  %s keyword not found for column %s\n",tlmax,colname); 
      return error_status; 
    }
    ahlog_info(3,__PRETTY_FUNCTION__,"  Using %s keyword (value = %d) for %s\n",tlmax,maxInt,parmax);
    *max = maxInt;
  } else {
    /* Convert parameter to integer */
    *max = atol(parstrMax);
    /* Read keyword TLMAX## */
    fits_read_key(*fpin,TLONG,tlmax,&maxInt,NULL,&error_status);
    if(error_status != 0 ) { 
      ahlog_out(__PRETTY_FUNCTION__,"*** %s keyword not found for column %s. Standard FITS format?\n",tlmax,colname); 
    } else if ( *max == maxInt ) {
      ahlog_out(__PRETTY_FUNCTION__,"*** Make sure %s value doesn't match %s\n",parmax,tlmax); 
    } else if (*max > maxInt) {
      *max = maxInt;
    }
    /* reset error status */
    error_status=0;
  }

  if ( *min >= *max ) {
    ahlog_err(__PRETTY_FUNCTION__,"  min >= max for %s (min=%i,max=%i)\n",colname,*min,*max); 
    return 1; 
  }

  ahlog_debug(__PRETTY_FUNCTION__,__FILE__,__LINE__,"  %s: minimum: %i maximum: %i\n",colname,*min,*max);

  return error_status;

}

/****************************************************************************/

int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata) {

  int status = 0;

  /* Check arguments. Use stdio functions for reporting problems at this stage, since no other message streams are set up yet. */
  if (0 >= argc) { fprintf(stderr, "startUp: logic (programming) error; argc == %d; must be positive.\n", argc); status = 1; }
  if (0 == argv) { fprintf(stderr, "startUp: logic (programming) error; argv is null.\n"); status = 1; }
  else if (0 == *argv) { fprintf(stderr, "startUp: logic (programming) error; *argv is null.\n"); status = 1; }
  /* if (0 == tooltag) no problem; tooltag is optional. */
  if (0 == appdata) { fprintf(stderr, "startUp: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  if (0 == status) {
    /** Initialize the application data structure. */
    *appdata = heaapp_construct_appdata(argc, argv); /* TODO: add tooltag when heaapp has it. */
  }

  /** From here on, use I/O functions in appdata to report errors. These will function correctly even if
      all else fails below. */

  if (0 == status) {
    /** Connect ape. Note this does not actually initialize ape, but it connects code that will initialize ape.
        The ape initialization code will read standard parameters, including chatter, logfile, history and
        clobber and store them in the application data structure. */
    status = heaapp_ape_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect ape.\n");
  }

  if (0 == status) {
    /** Connect ahlog. Note this does not actually initialize ahlog, but it connects code that will initialize ahlog.
        The ahlog initialization code will pull the name of the tool, chatter and logfile parameters, etc., from the
        application data structure when it *does* run. */
    status = heaapp_ahlog_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect ahlog.\n");
  }

  if (0 == status) {
    /** Connect heautils. Note this does not actually initialize heautils, but it connects code that will initialize heautils.
        The heautils initialization code will pull history, clobber, etc., from the
        application data structure when it *does* run. */
    status = heaapp_heautils_connect(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to connect heautils.\n");
  }

  if (0 == status) {
    /** Finally, run all the initialization codes that were connected above. */
    status = heaapp_initialize(appdata);
    if (0 != status) appdata->printerr(__PRETTY_FUNCTION__, "unable to initialize application.\n");
  }

  return status;

}

/****************************************************************************/

void writeParametersToLog() {
  char** par_names = 0;
  char* value = 0;
  char* out = 0;
  char* new_out = 0;
  int ii = 0;

  /* start output line with name of tool */
  char toolname[128];          /* 128 is the size used in headas_toolname.c */
  get_toolname(toolname);
  ape_util_copy_string(toolname, &out);

  /* write parameters to output line */
  ape_trad_get_par_names(&par_names);
  while(par_names[ii] != NULL){
    ape_trad_get_string(par_names[ii],&value);
    ape_util_cat_string(out, " '", &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, par_names[ii], &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, "=", &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, value, &new_out);
    free(out); out = new_out; new_out = 0;
    ape_util_cat_string(out, "'", &new_out);
    free(out); out = new_out; new_out = 0;
    free(value);
    ii++;
  }

  /* Write parameter list to log file */
  ahlog_info(HIGH, __func__, "START PARAMETER LIST:\n");
  ahlog_info(HIGH, __func__, "%s\n", out);
  ahlog_info(HIGH, __func__, "END PARAMETER LIST\n");

  /* Clean up output string */
  free(out);

  /* Clean up parameter list array */
  ape_util_free_string_array(par_names);

} /* end of writeParametersToLog */

/****************************************************************************/

int writeWCSKeys(fitsfile * fpin, fitsfile ** fpout, char * keyroot, int colnum_xcol,
                 int colnum_ycol, ColNum * colNums, int * error_status) {
  char keyname[8];                         /* keyword name of relavent WCS keys in EVENTS extension */
  char WCSkeyval[FLEN_KEYWORD];            /* Value of relvant WCS key */
  char keynamePIXEL[8];                    /* keyword name of WCS keys in output PIXELS extension */ 

  
  if ((strcasecmp(keyroot,"TCTYP") == 0) || (strcasecmp(keyroot,"TCUNI") == 0)) { 
    sprintf(keyname,"%s%d",keyroot,colnum_xcol); 
    fits_read_key(fpin,TSTRING,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawx);
    fits_write_key(*fpout,TSTRING,keynamePIXEL,WCSkeyval,NULL,error_status); 
    sprintf(keyname,"%s%d",keyroot,colnum_ycol); 
    fits_read_key(fpin,TSTRING,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawy);
    fits_write_key(*fpout,TSTRING,keynamePIXEL,WCSkeyval,NULL,error_status);
  } else if ((strcasecmp(keyroot,"TNULL") == 0) || (strcasecmp(keyroot,"TLMIN") == 0) 
             || (strcasecmp(keyroot,"TLMAX") == 0)) { 
    sprintf(keyname,"%s%d",keyroot,colnum_xcol); 
    fits_read_key(fpin,TINT,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawx);
    fits_write_key(*fpout,TINT,keynamePIXEL,WCSkeyval,NULL,error_status); 
    sprintf(keyname,"%s%d",keyroot,colnum_ycol); 
    fits_read_key(fpin,TINT,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawy);
    fits_write_key(*fpout,TINT,keynamePIXEL,WCSkeyval,NULL,error_status);
  } else {
    sprintf(keyname,"%s%d",keyroot,colnum_xcol); 
    fits_read_key(fpin,TDOUBLE,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawx);
    fits_write_key(*fpout,TDOUBLE,keynamePIXEL,WCSkeyval,NULL,error_status); 
    sprintf(keyname,"%s%d",keyroot,colnum_ycol); 
    fits_read_key(fpin,TDOUBLE,keyname,WCSkeyval,NULL,error_status); 
    sprintf(keynamePIXEL,"%s%d",keyroot,colNums->m_rawy);
    fits_write_key(*fpout,TDOUBLE,keynamePIXEL,WCSkeyval,NULL,error_status);
  }
  return *error_status;

}
/****************************************************************************/

int shutDown(HeaAppData * appdata) {

  int status = 0;
  /* Check arguments. Use stdio functions (only) for reporting null appdata, since no other option. */
  if (0 == appdata) { fprintf(stderr, "shutDown: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  /* Do finalize operations. */
  if (0 == status) {
    /** This will shut down the libraries in reverse order that were started up in the startUp function. */
    status = heaapp_finalize(appdata);
    /* Report error using appdata IO, which is valid even if there was an error. */
    if (0 != status) { appdata->printerr("shutDown", "heaapp_finalize returned an error.\n"); }
  }
  return status;

}

/****************************************************************************/

/** @} */


/** Revision Log
 $Log: searchflickpix.c,v $
 Revision 1.21  2016/04/21 13:34:55  mdutka
 TNULL TLMIN/MAX are int not double

 Revision 1.20  2016/04/15 19:20:23  rshill
 Change a couple of strcmp to strcasecmp.

 Revision 1.19  2016/04/08 15:18:18  rshill
 Added writeParametersToLog().

 Revision 1.18  2016/04/06 21:46:15  mdutka
 Now writing coordinate related column keywords correctly

 Revision 1.17  2015/11/03 14:09:41  asargent
 Removed extraneous printout when counting output rows. Removed requirement that input file be time-ordered.

 Revision 1.16  2015/10/16 19:41:47  asargent
 Update chipcol counter printout in case of chipcol==none

 Revision 1.15  2015/10/16 19:31:39  asargent
 Updated chipcol print name and offset. Fixed total counter.

 Revision 1.14  2015/10/16 17:32:48  asargent
 New duration algorithm. Updated output file. Cleanup of code. Add chatter.

 Revision 1.13  2015/08/19 22:05:32  asargent
 Added keyword copying to second extension of output file

 Revision 1.12  2015/08/13 21:51:48  rshill
 Changed output columns TIME to START, TIMELAST to STOP.

 Revision 1.11  2015/06/09 19:12:10  mdutka
 Adding bug fixes found by hiroya and switching parameter order

 Revision 1.10  2015/05/13 21:28:04  mdutka
 accounting for change to prototype for ahlog_debug

 Revision 1.9  2015/03/24 18:38:37  mdutka
 Changes to second extension in output file for searchflickpix see issue #490

 Revision 1.8  2015/01/07 15:31:29  asargent
 Updated parameter descriptions in par file, updated unit tests.

 Revision 1.7  2014/11/05 19:43:03  asargent
 Added in NULL value checking for TIME column.

 Revision 1.6  2014/10/14 14:25:41  asargent
 Slight revision to step four algorithm for speed improvement. Better use of heautils routines during creation of output file.

 Revision 1.5  2014/10/01 19:08:58  asargent
 Added startUp and shutdown functions to match Astro-H main standards. Added parstamping tou output file. Added TIMECOL deleting from PIXELS extension in output file.

 Revision 1.4  2014/09/30 15:14:19  asargent
 New algorithm to detect non-flickering pixels. Previously, any pixel associated with a flickering pixel was counted as flickering. The new algorithm accounts for possible outliers and removes them from flickering pixel list.

 Revision 1.3  2014/09/15 18:24:22  asargent
 Removed explicit parameter file reading of clobber to use heaapp/ahlog parameter instead.

 Revision 1.2  2014/09/10 17:45:19  asargent
 Implemented heaapp and ahlog into searchflickpix.

 Revision 1.1  2014/07/08 17:00:28  asargent
 New tool searchflickpix

*/
