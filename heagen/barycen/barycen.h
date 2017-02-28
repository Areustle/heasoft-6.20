/**
   \file barycen.h
   \brief Header file for barycen.c
   \author David Riethmiller, Hans Krimm
   \date $Date: 2016/06/02 21:54:52 $  */

#include "genorbfile.h"   // attitude/lib/coordfits/genorbfile.h
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>

#define PI 3.14159265358979323
#define BARYCEN_TIME_UNIT_SEC 1
#define BARYCEN_TIME_UNIT_DAY 86400

/* Chatter priority settings */
#define ALWAYS 0
#define HIGH 1
#define MED 2
#define LOW 3

#define SECDAY  86400                   /* Seconds per day */
#define EPHEM "JPLEPH"
#define MAX_LEAPS 5000

#define NEW_FLEN_VALUE 300

/* ########## ENUMS ############## */

static enum TimeSys {TT, TDB, TCG, TCB, TAI, ET, UTC} fromTS, toTS ;
static enum Observatory {Unknown, Geocenter, XTE, AXAF, SWIFT, NuSTAR, HITOMI} mission=Unknown ;

typedef enum{
    EXTTYPE_EVENT,  /* Event extension */
    EXTTYPE_GTI,    /* GTI extension */
    EXTTYPE_OTHER,  /* Other extension with TIME column */
    EXTTYPE_SKIP    /* Extension without TIME column */
} EXTENSION_TYPE;


/* ############### STRUCTURES ################## */

/**
 * \brief Structure to hold barycen tool parameters
 * \param[in,out] infile               Name of input file with time to correct
 * \param[in,out] outfile              Name of output file corrected
 * \param[in,out] orbfile              Name of satellite orbit file
 * \param[in,out] ra                   Right ascension of position used in the corrction (deg)
 * \param[in,out] dec                  Declination of position used in the correction (deg)
 * \param[in,out] orbext               Orbit extension
 * \param[in,out] orbform              Orbit velocity format (VECTOR|COMPONENTS|KEPLERIAN)
 * \param[in,out] orbcol               Orbital columns
 * \param[in,out] refframe             Ephemeris reference frame
 * \param[in,out] orbinterp            Interpolation method
 * \param[in,out] timecol              Name of time column in file
 * \param[in,out] startcol             Name of start column in GTI extension
 * \param[in,out] stopcol              Name of stop column in GTI extension
 * \param[in,out] useweights           Use weight column to combine multiple rows in orbit file
 * \param[in,out] clobber              Overwrite existing output file
 * \param[in,out] chatter              Chatter level for output
 * \param[in,out] logfile              Name of log file, DEFAULT, or NONE; '!' to clobber
 * \param[in,out] debug                Debug messages on
 * \param[in,out] history              Record tool parameters in HISTORY
 * \param[in,out] mode                 Mode of automatic parameters
 * \param[in,out] tstarti              Integer part of start time 
 * \param[in,out] tstartf              Fractional part of start time 
 * \param[in,out] tstopi               Integer part of stop time 
 * \param[in,out] tstopf               Fractional part of stop time 
 * \param[in,out] num_orb_cols         Number of orbit columns 
 * \param[in,out] orb_col_name         List of orbit column names 
 * \param[in,out] orb_format           Orbit format 
 * \param[in,out] interp_method        Interpolation method 
 * \param[in,out] get_ra_from_infile   Flag to read RA from input fits file 
 * \param[in,out] get_dec_from_infile  Flag to read DEC from input fits file 
 * \param[in,out] torbstart            Time of orbit start 
 * \param[in,out] torbstop             Time of orbit stop 
 * \param[in,out] history_string       String containing parameter history */
typedef struct {
    /* Read directly from par file */
    char infile[NEW_FLEN_VALUE];        /* Name of input file with time to correct */
    char outfile[NEW_FLEN_VALUE];       /* Name of output file corrected */
    char orbfile[NEW_FLEN_VALUE];       /* Name of satellite orbit file */
    double ra;                      /* Right ascension of position used in the corrction (deg) */
    double dec;                     /* Declination of position used in the correction (deg) */
    char orbext[NEW_FLEN_VALUE];        /* Orbit extension */
    char orbform[NEW_FLEN_VALUE];       /* Orbit velocity format (VECTOR|COMPONENTS|KEPLERIAN) */
    char orbcol[NEW_FLEN_VALUE];        /* Orbital columns */
    char refframe[NEW_FLEN_VALUE];      /* Ephemeris reference frame */
    char orbinterp[NEW_FLEN_VALUE];     /* Interpolation method */
    char timecol[NEW_FLEN_VALUE];       /* Name of time column in file */
    char startcol[NEW_FLEN_VALUE];      /* Name of start column in GTI extension */
    char stopcol[NEW_FLEN_VALUE];       /* Name of stop column in GTI extension */
    int useweights;                     /* Use weight column to combine multiple rows in orbit file */
    int clobber;                    /* Overwrite existing output file */
    int chatter;                    /* Chatter level for output */
    char logfile[NEW_FLEN_VALUE];       /* Name of log file, DEFAULT, or NONE; '!' to clobber */
    int debug;                      /* Debug messages on */
    int history;                    /* Record tool parameters in HISTORY */
    char mode[NEW_FLEN_VALUE];          /* Mode of automatic parameters */

    /* Derived, not in par file */
    int tstarti;                    /* Integer part of start time */
    double tstartf;                 /* Fractional part of start time */
    int tstopi;                     /* Integer part of stop time */
    double tstopf;                  /* Fractional part of stop time */
    int num_orb_cols;               /* Number of orbit columns */
    char ** orb_col_name;           /* List of orbit column names */
    ORBFORMAT orb_format;           /* Orbit format */
    ORBINTERP interp_method;        /* Interpolation method */
    int get_ra_from_infile;         /* Flag to read RA from input fits file */
    int get_dec_from_infile;        /* Flag to read DEC from input fits file */
    double torbstart;               /* Time of orbit start */
    double torbstop;                /* Time of orbit stop */
    char * history_string;          /* String containing parameter history */
} PARAM;


/**
 * \brief Structure to hold column number info
 * \param[in,out]  nrow  Number of rows
 * \param[in,out]  t_col TIME column number
 * \param[in,out]  s_col START column number
 * \param[in,out]  e_col STOP  column number  */
typedef struct {
    long nrow;      /* Number of rows */
    int t_col;      /* TIME column number */
    int s_col;      /* START column number */
    int e_col;      /* STOP  column number */
} COLUMN_INFO;


/**
 * \brief Structure to hold MDJ time info
 * \param[in,out] ts      Enumerated list of time systems
 * \param[in,out] MDJint  Integer part of MJD time
 * \param[in,out] MDJfr   Fractional part of MJD time  */
typedef struct {
    enum TimeSys ts;  /* Enumerated list of time systems */
    long MJDint;      /* Integer part of MJD time */
    double MJDfr;     /* Fractional part of MJD time */
} MJDTime ;


/**
 * \brief Structure to hold what used to be global variables in bary.c
 * \param[in,out] ephfile    Name of ephemeris file
 * \param[in,out] c          speed of light (m/s)
 * \param[in,out] msol       GM(solar), using light second as unit of length
 * \param[in,out] radsol     solar radius (light secs)
 * \param[in,out] denum      DE number of ephemeris sed
 * \param[in,out] met2day    Scale ephemeris time to days
 * \param[in,out] day2met    Scale days to ephemeris time
 * \param[in,out] met2sec    Scale ephemeris time to seconds
 * \param[in,out] mjdRefi    Integer part of MJD time
 * \param[in,out] mjdReff    Fractional part of MJD time
 * \param[in,out] mjdRef     Combined MJD time
 * \param[in,out] numleaps   Number of leapseconds
 * \param[in,out] leapsMJD   Leapseconds in MJD
 * \param[in,out] leapsecs   leapseconds
 * \param[in,out] MJDoffset  Offset to modified julian date
 * \param[in,out] leapcoeff  Leapsecond coefficient    */
typedef struct {
    char ephfile[1024];  /* Name of ephemeris file */
    double c;            /* speed of light (m/s) */
    double msol;         /* GM(solar), using light second as unit of length */
    double radsol;       /* solar radius (light secs) */
    int denum;           /* DE number of ephemeris sed */
    double met2day;      /* Scale ephemeris time to days */
    double day2met;      /* Scale days to ephemeris time */
    double met2sec;      /* Scale ephemeris time to seconds */
    long mjdRefi;        /* Integer part of MJD time */
    double mjdReff;      /* Fractional part of MJD time */
    MJDTime mjdRef;      /* Combined MJD time */
    long numleaps;       /* Number of leapseconds */
    long * leapsMJD;     /* Leapseconds in MJD */
    double * leapsecs;   /* leapseconds */
    double * MJDoffset;  /* Offset to modified julian date */
    double * leapcoeff;  /* Leapsecond coefficient */
} BARYCORR_DATA;


/**
 * \brief Structure to hold what used to be global variables in dpleph.c
 * \param[in,out] irecsz      length of ephemeris records (no. of doubles)  
 * \param[in,out] ss1         lower bound for epoch range
 * \param[in,out] ss2         upper bound for epoch range
 * \param[in,out] ss3         time delay
 * \param[in,out] ss3inv      inverse of ss3
 * \param[in,out] currec      current record number
 * \param[in,out] emratinv    emratinv = 1.0 / (1.0 + emrat) 
 * \param[in,out] buffer      buffer containing array of ephemeris time data
 * \param[in,out] iptr        array of pointers for object's coefficients in record
 * \param[in,out] ncf         number of Chebyshev coefficients for object
 * \param[in,out] na          number of time sub-intervals for object
 * \param[in,out] buflen      buffer length
 * \param[in,out] nrecs       Number of records
 * \param[in,out] clight      speed of light
 * \param[in,out] au          astronomical unit
 * \param[in,out] aufac       astronomical unit factor
 * \param[in,out] velfac      velocity factor           */
typedef struct {
    long irecsz;      /* length of ephemeris records (no. of doubles)  */
    double ss1;       /* lower bound for epoch range */
    double ss2;       /* upper bound for epoch range */
    double ss3;       /* time delay */
    double ss3inv;    /* inverse of ss3 */
    long currec;      /* current record number */
    double emratinv ; /* emratinv = 1.0 / (1.0 + emrat) */
    double * buffer;  /* buffer containing array of ephemeris time data */
    long iptr[13];    /* array of pointers for object's coefficients in record */
    long ncf[13];     /* number of Chebyshev coefficients for object */
    long na[13];      /* number of time sub-intervals for object */
    long buflen;      /* buffer length */
    long nrecs;       /* Number of records */
    double clight;    /* speed of light */
    double au;        /* astronomical unit */
    double aufac;     /* astronomical unit factor */
    double velfac;    /* velocity factor */
} DPLEPH_DATA;


/**
 * \brief Structure to hold quantities to print out per input row
 * \param[in,out] vearth      Earth speed
 * \param[in,out] vearthx     Earth velocity, X component
 * \param[in,out] vearthy     Earth velocity, Y component
 * \param[in,out] vearthz     Earth velocity, Z component
 * \param[in,out] eradvel     Apparent radial velocity of source due to Earth velocity
 * \param[in,out] total       Total barycentric correction
 * \param[in,out] t2b         Time system shift
 * \param[in,out] scssbc      Term for S/C to SSBC light travel time
 * \param[in,out] scearth     Term for Earth velocity
 * \param[in,out] grav        Term for Sun's gravity  */                               
typedef struct {
    double vearth;   /* Earth speed */
    double vearthx;  /* Earth velocity, X component */
    double vearthy;  /* Earth velocity, Y component */
    double vearthz;  /* Earth velocity, Z component */
    double eradvel;  /* Apparent radial velocity of source due to Earth velocity */
    double total;    /* Total barycentric correction */
    double t2b;      /* Time system shift */
    double scssbc;   /* Term for S/C to SSBC light travel time */
    double scearth;  /* Term for Earth velocity */
    double grav;     /* Term for Sun's gravity  */                               
} PRINT_DATA;


/* ############### PROTOTYPE DEFINITIONS IN barycen.c ##################### */

/**
 * \brief Initialize all possible structure variables to zero
 *  \param[in] param       Data structure holding barycen tool parameters
 *  \param[in] bcorr       Data structure holding former global variables from barycorr/bary.c
 *  \param[in] dpl         Data structure holding former global variables from barycorr/dpleph.c   */
void initialize_structures(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);

/**
 * \brief Read the parameter file and store to param structure
 * \param[in] argc   Number of arguments given on command line
 * \param[in] argv   String of arguments gieven on command line
 * \param[out] param Data structure holding barycen tool parameters   */
int getPars(int argc, char ** argv, PARAM * param);


/**
 * \brief Set up for barycen tool to run
 * \param[in,out] param  Data structure holding barycen tool parameters    */
int initialize(PARAM * param);


/**
 * \brief Main body of work in the barycen tool
 *  \param[in] param       Data structure holding barycen tool parameters
 *  \param[in] bcorr       Data structure holding former global variables from barycorr/bary.c
 *  \param[in] dpl         Data structure holding former global variables from barycorr/dpleph.c   */
int doWork(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);


/**
 * \brief Clean up any remaining data in memory
 *  \param[in] param       Data structure holding barycen tool parameters
 *  \param[in] bcorr       Data structure holding former global variables from barycorr/bary.c
 *  \param[in] dpl         Data structure holding former global variables from barycorr/dpleph.c   */
int finalize(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);



/**
 * \brief Execute the actual barycenter correction
 * \param[in] GenOrbFile  Data structure holding orbit info
 * \param[in] time        Time to be corrected
 * \param[in] mjdrefi     Integer part of MJD
 * \param[in] mjdreff     Fractional part of MJD
 * \param[in] src_dir     Source direction
 * \param[in] vunit       Velocity unit
 * \param[in] bcorr       Data structure holding former global variables from barycorr/bary.c
 * \param[in] dpl         Data structure holding former global variables from barycorr/dpleph.c
 * \param[in] param       Data structure holding run parameters  */
double correct_bary (GENORBFILE * GenOrbFile,  /* Data structure holding orbit info */
                     double time,              /* Time to be corrected */
                     int mjdrefi,              /* Integer part of MJD */
                     double mjdreff,           /* Fractional part of MJD */
                     double * src_dir,         /* Source direction */
                     char * vunit,             /* Velocity unit */
                     PARAM * param,            /* Data structure holding run parameters */
                     BARYCORR_DATA * bcorr,    /* Data structure holding former global variables from barycorr/bary.c */
                     DPLEPH_DATA * dpl);       /* Data structure holding former global variables from barycorr/dpleph.c */


/**
 * \brief Apply a barycenter correction to a time-related keyword
 *  \param[in] keyword     Time-related keyword to be corrected
 *  \param[in] fits_ptr    Pointer to fits file containing keyword
 *  \param[in] GenOrbFile  Data structure holding orbit info
 *  \param[in] mjdrefi     Integer part of MJD 
 *  \param[in] mjdreff     Fractional part of MJD
 *  \param[in] src_dir     Source direction
 *  \param[in] vunit       Velocity unit
 *  \param[in] param       Data structure holding barycen tool parameters
 *  \param[in] bcorr       Data structure holding former global variables from barycorr/bary.c
 *  \param[in] dpl         Data structure holding former global variables from barycorr/dpleph.c   */
void correct_bary_keyword(char * keyword,           /* Time-related keyword to be corrected */
                          fitsfile * fits_ptr,      /* Pointer to fits file containing keyword */
                          GENORBFILE * GenOrbFile,  /* Data structure holding orbit info */
                          int mjdrefi,              /* Integer part of MJD */
                          double mjdreff,           /* Fractional part of MJD */
                          double * src_dir,         /* Source direction */
                          char * vunit,             /* Velocity unit */
                          PARAM * param,            /* Data structure holding barycen tool parameters */
                          BARYCORR_DATA * bcorr,    /* Data structure holding former global variables from barycorr/bary.c */
                          DPLEPH_DATA * dpl);       /* Data structure holding former global variables from barycorr/dpleph.c */


/**
 * \brief Determine if the given FITS extension should be scheduled for barycenter correction
 * \param[in] fp          Pointer to (already opened) fits file
 * \param[in] iHDU        Current HDU number
 * \param[in] given_HDU   User-specified HDU to correct
 * \param[in] param       Structure containing parameter info
 * \param[in] cp          Structure containing start/stop column info
 * \param[in] exttype     Enumerated type of HDU extention        */
int check_extension(fitsfile * fp,              /* Pointer to (already opened) fits file */
                    int iHDU,                   /* Current HDU number */
                    int given_HDU,              /* User-specified HDU to correct */
                    PARAM * param,              /* Structure containing parameter info */
                    COLUMN_INFO * cp,           /* Structure containing start/stop column info */
                    EXTENSION_TYPE * exttype);  /* Enumerated type of HDU extention */


/**
 * \brief Convert the string name of a mission to its corresponding enumerated type
 * \param[in]  input_string  String name of the mission   */
int ObsString2Enum(char * input_string);

/**
 * \brief Convert aste mission time to mjd
 * \param[in] mission_time   Mission time
 * \param[in] mjdrefi        Integer component of mjd
 * \param[in] mjdreff        Fractional component of mjd   */
double aste2mjdtt(double mission_time, int mjdrefi, double mjdreff);

/**
 * \brief  print information both to terminal screen and to log file, depending on chatter level
 * \param[in]  param          Data structure holding barycen tool parameters
 * \param[in]  chat_priority  Level at which statement should be printed to screen
 * \param[in]  format         String to be printed
 * \param[in]  ...            Variables to be inserted into string   */
int info(PARAM * param, int chat_priority, const char * format, ...);

/**
 * \brief get the number of lines contained within an ASCII file
 * \param[in] filename  Name of the file to be checked  */
int get_nlines(char * filename);

/**
 * \brief Report the most recent version number of barycen.c
 * \param[in,out] version  the version number    */
void get_version(char * version);

/**
 * \brief remove a substring from a string
 * \param[in,out] s     input string
 * \param[in] toremove  substring to remove    */
void remove_substring(char *s, const char *toremove);

/**
 * \brief isolate the root and suffix of a filename
 * \param[in] filename   the name of the file
 * \param[out]    root   the root of the filename
 * \param[out]  suffix   the suffix of the filename  */
void get_root_suffix(char * filename, char * root, char * suffix);


/* PROTOTYPE DEFINITIONS IN from_barycorr.c */
void c200to405 (double *dir, int debug);
int baryinit (enum Observatory obs, char *fromts, char *tots, double mjdi,double mjdf, char *timeunit, 
              int ephnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);
double barycorr (MJDTime *mjdXT, double *sourcedir, double *scposn, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl, PRINT_DATA * pdata);
FILE *openAFile (const char *name);
fitsfile *openFFile (const char *name);
double ctatv (long jdno, double fjdno);
double toTT (MJDTime *mjd, BARYCORR_DATA * bcorr);
double *dpleph (double *jd, int ntarg, int ncent, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);
int state (double *jd, int ntarg, int ncent, double *posn, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);
int getstate (int ntarg, double t1, double *posn, DPLEPH_DATA * dpl);
int interp (double *buf, double t1, double *pv, DPLEPH_DATA * dpl, int index);
int readephem (long recnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);
int initephem (int ephnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl);
double findcval (char **cnam, double *cval, long n, char *name);



/* ############ END OF FROM BARYCORR ####################################### */


/* Revision Log
   $Log: barycen.h,v $
   Revision 1.20  2016/06/02 21:54:52  rshill
   Added radial velocity diagnostic printout; expanded tab characters in code.

   Revision 1.19  2016/06/02 21:31:39  rshill
   Corrected argument lists and cleaned up output.

   Revision 1.18  2016/06/02 01:36:50  rshill
   Implemented printout of Earth velocity and barycentric correction components for debug=yes.

   Revision 1.17  2016/02/18 23:57:39  klrutkow
   changed ah mission to HITOMI

   Revision 1.16  2015/12/29 18:36:50  driethmi
   Removed unnecessary parameter "orbdt" from code and par files.

   Revision 1.15  2015/11/17 20:35:35  driethmi
   If using KEPLERIAN orbit format, and useweights is set to "yes", then keep
   using KEPLERIAN.  If useweights is set to "no", then use KEPLERIAN_NODUP.

   Revision 1.14  2015/09/15 19:12:05  driethmi
   Extended definition of PI to include more digits.

   Revision 1.13  2015/09/11 20:28:06  driethmi
   Added function to handle filename root and suffix strings in a more
   intelligent way.

   Revision 1.12  2015/09/10 18:13:34  driethmi
   Added NEW_FLEN_VALUE as the default string length.

   Revision 1.11  2015/09/03 19:19:01  driethmi
   Removed leapsecond and ephemeris files from parameters; these are hard-coded
   into the barycorr routines.

   Revision 1.10  2015/08/31 18:20:54  driethmi
   Added capability to write history keyword paramters at end of fits header.

   Revision 1.9  2015/08/31 15:00:09  driethmi
   Added doxygen comment blocks before each function/structure declaration.

   Revision 1.8  2015/08/27 19:15:03  driethmi
   Eliminated global variables related to dpleph.c, in favor of structure
   which is passed to all functions which require it.  Code now passes
   Valgrind memory test cleanly.

   Revision 1.7  2015/08/27 17:21:28  driethmi
   Eliminated global variables copied from bary.c to from_barycorr.c with a structure
   which is passed to functions which need it.

   Revision 1.6  2015/08/27 14:54:21  driethmi
   Combined several conversion functions in from_barycorr.c, reworked "ephfile"
   so it is no longer a global variable.

   Revision 1.5  2015/08/26 20:46:00  driethmi
   Removed function TDBtoTCB().

   Revision 1.4  2015/08/26 19:19:19  driethmi
   Modified some function declarations.

   Revision 1.3  2015/08/25 17:28:30  driethmi
   Changes to correct errstatus functionality, and rename output file in the case
   where input=output file name.

   Revision 1.2  2015/08/24 15:46:25  driethmi
   Added revision log at bottom of files.

*/
