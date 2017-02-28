
/**

  @file    sxsperseus.c
  @brief   Simple FITS file manipulation program to remove differential 
           gain error and fiducial gain error from GV closed Perseus data
  @author  F. S. Porter
  @date    3016/03/26
  @version 1.0

  @defgroup tool_sxsperseus

  
  Source file:
     sxsperseus.c

   Modification History:
   V1.0 2016/03/26 FSP: Initial version
   V1.1 2016/05/31 FSP: Clean up and add reading cal pix gain from FITS 
                        output from sxsgain
   V2.0 2016/06/01 FSP: Finish cleanup and distribute.
   V3.0 2016/06/14 MSD: Integrate tool with the rest of the astroH suite        

*/
#define AHLABEL tool_sxsperseus
#define AHCVSID "$Id: sxsperseus.c,v 1.16 2016/11/03 15:56:53 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <float.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "fitsio.h"
#include "ahlog/cahlog.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "headas_utils.h"
#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"



/* Cal pixel gain at FW measurement */
#define CAL_PIX_GAIN_FIDUCIAL 1.000197  



/** @brief Structure of parameters from user-input or parameter file
 */
struct Params{

  char m_infile[FLEN_FILENAME];         /**< Input event file name */
  char m_outfile[FLEN_FILENAME];        /**< Output event file name */
  char m_gainCoefFile[FLEN_FILENAME];   /**< Differential gain file */ 
  char m_calOffsetFile[FLEN_FILENAME];  /**< Calibration offset file */ 
  char m_driftfile[FLEN_FILENAME];      /**< Input calibration pixel gain file */
  char m_outrange[FLEN_FILENAME];       /**< How events are handled outside time range */ 
  char m_method[FLEN_FILENAME];         /**< Specifies column to use from gain history file */ 
  int m_extended;                       /**< Use extended energy range (yes/[no]) */ 
  double m_binwidth;                    /**< PI bin width for extended energy range [eV] */
  double m_offset;                      /**< Offset for first PI for extended energy range [eV] */  
  long m_tlmax;                         /**< Maximum PI channel for extended energy range */

};

/** @brief Structure to contain column data for input, output and calibration files
 */
struct columnData{

  double *m_myEPI;                   /**< EPI column in event file */ 
  double *m_myEPI2;                  /**< EPI2 column in event file */ 
  int *m_myPI;                       /**< PI column event file */
  int *m_myITYPE;                    /**< ITYPE column in event file */
  double *m_myTime;                  /**< TIME column in event file*/
  double *m_CalPix_Time;             /**< TIME column in gain history file */ 
  double *m_CalPix_Gain;             /**< COR_FIT or COR_AVG (method paramter) gain history file */
  double *m_tmpCalPix_Time;          /**< temporary container for TIME column gain history file */
  double *m_tmpCalPix_Gain;          /**< temporary container for COR_FIT or COR_AVG (method paramter) gain history file */
  int *m_myPixel;                    /**< PIXEL column event file */

};

/** @brief Structure which contains fits file pointers
 */
struct fitsPointers{

  fitsfile *m_infptr;            /**< Input event file */
  fitsfile *m_outfptr;           /**< output event file */
  fitsfile *m_CalOffsetFile;     /**< Offset file */
  fitsfile *m_DGGainFile;        /**< Differential Gain file */
  fitsfile *m_CalGainFFile;      /**< Gain History file */

};


/**
 * @brief define structure types
 */
typedef struct Params Params;
typedef struct columnData columnData;
typedef struct fitsPointers fitsPointers;

/**
 * @brief Get parameter values
 * @param[out] par         Structure containing parameter file data
 * @return Return false if no errors, else true
 */
int getPar(Params * par);


/**
 * @brief Opens input fits files for reading or writing
 * @param[in] par       to store PIXELS extension column numbers
 * @param[out] fitsPointers struct with pointers to all fits files
 * @return Return false if no errors, else true
 */

int initialize(Params par, fitsPointers * fitsPointers);


/**
 * @brief Main routine for event loops and file reading/writing
 * @param[in] par       to store PIXELS extension column numbers
 * @param[out] coldat   Column data for input and output files
 * @param[in] fitsPointers struct with pointers to all fits files
 * @return Return false if no errors, else true
 */
int doWork(Params par, columnData * coldat, fitsPointers * fitsPointers);

int finalize(columnData * coldat, fitsPointers * fitsPointers);


/**
 * @brief Calculates EPI using interpolated offset
 * @param[in] Temp2       interpolated offset
 * @param[in] EPI         orignal EPI values 
 * @param[in] FWCalOffsets filter wheel offsets
 * @return Return false if no errors, else true
 */
double calcEPI(double Temp2, double EPI, double FWCalOffsets);



int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);

void writeParametersToLog();

int shutDown(HeaAppData * appdata);


/**
 * @brief deallocates column data
 * @param[out] coldat   Column data for input and output files
 * @return Return false if no errors, else true
 */

int deleteColdat(columnData * coldat);


/**
 * @brief Colese fits file 
 * @param[in] fitsPointers struct with pointers to all fits files
 * @return Return false if no errors, else true
 */
int closeFiles(fitsPointers * fitsPointers, int status);

int main(int argc, char** argv)
{


  HeaAppData appdata = { 0 };
  int status = 0;             /* Status (0: normal) */
  int finalStatus = 0;        /* Final error status */
  Params par;                 /* Structure to store parameters for par file */ 
  columnData coldat;          /* Structure to store input and output file column data */   
  fitsPointers fitsPointers;  /* Structure to store fits file pointers */


  /* Intialize values in Par struct */
  par.m_extended = 0;
  par.m_binwidth = 0.;
  par.m_offset = 0.;
  par.m_tlmax = 0;

  status = startUp(argc, argv, TOOLTAG, &appdata);
  if (0 != status) {
    ahlog_err(__PRETTY_FUNCTION__, "startUp returned status %d, not 0 as expected.\n", status);
    finalStatus = 1; /* Latch error status). */
  }

  if (0 == status) {
    status = getPar(&par);
    if(0 != status) {
      writeParametersToLog();
      ahlog_err(__PRETTY_FUNCTION__, "getPar returned status %d, not 0 as expected.\n", status);
      finalStatus = 0;
    }

    if (0 == status) {
      status = initialize(par, &fitsPointers);
      if (0 != status) {
        ahlog_err(__PRETTY_FUNCTION__, "initialize returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    }
    
    if (0 == status) {
      status = doWork(par,&coldat,&fitsPointers);
      if(0 != status) {
        ahlog_err(__PRETTY_FUNCTION__, "doWork returned status %d, not 0 as expected.\n", status);
        finalStatus = 1;
      }
    } 

    status = finalize(&coldat, &fitsPointers);
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

} /* end of main() */

int getPar(Params * par) {

    /************************* Read Parameter file **************************/
  
  int status = 0;

  char * tempStr = 0;  /* Temporary holder string to process par file */
  char * refdata = getenv("LHEA_DATA");
  char tmprefgain[FLEN_FILENAME];
  char tmprefoffset[FLEN_FILENAME];
  char tempbool = 0;
  double tempdouble = 0.;  
  long templong = 0;

  strcpy(tmprefgain,refdata);
  strcpy(tmprefoffset,refdata);

  if ((status = ape_trad_query_string("infile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get infile\n");
    return status;
  } else {
    strcpy(par->m_infile, tempStr);
  }

  if ((status = ape_trad_query_string("outfile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get outfile\n");
    return status;
  } else {
    strcpy(par->m_outfile, tempStr);
  }

  if ((status = ape_trad_query_string("driftfile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get driftfile \n");
    return status;
  } else {
    strcpy(par->m_driftfile, tempStr);  
  }
  
  if ((status = ape_trad_query_string("dgfile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get gaincoeffsfile\n");
    return status;
  } else {
    strcpy(par->m_gainCoefFile, tempStr);
    /* if parameter = REFDATA look in refdata area for file */
    if (strcasecmp(par->m_gainCoefFile,"REFDATA") == 0) {
      strcat(tmprefgain,"/ahsxs_dggain.fits");
      strcpy(par->m_gainCoefFile, tmprefgain);
    }   
  }

  if ((status = ape_trad_query_string("offsetfile",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get caloffsetfile \n");
    return status;
  } else {
    strcpy(par->m_calOffsetFile, tempStr);
    /* if parameter = REFDATA look in refdata area for file */
   if (strcasecmp(par->m_calOffsetFile,"REFDATA") == 0) {
      strcat(tmprefoffset,"/ahsxs_offsets.fits");
      strcpy(par->m_calOffsetFile, tmprefoffset);
    }   
  }

  if ((status = ape_trad_query_string("outrange",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get outrange parameter \n");
    return status;
  } else {
    strcpy(par->m_outrange, tempStr);  
  }

  if ((status = ape_trad_query_string("method",&tempStr)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get method parameter \n");
    return status;
  } else {
    strcpy(par->m_method, tempStr);  
  }
  
  if ((status = ape_trad_query_bool("extended",&tempbool)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get extended parameter \n");
    return status;
  } else {
    par->m_extended = tempbool;  
  }
  
  if ((status = ape_trad_query_double("binwidth",&tempdouble)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get binwidth parameter \n");
    return status;
  } else {
    par->m_binwidth = tempdouble;  
  }
  
  if ((status = ape_trad_query_double("offset",&tempdouble)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get offset parameter \n");
    return status;
  } else {
    par->m_offset = tempdouble;  
  }

  if ((status = ape_trad_query_long("tlmax",&templong)) != eOK) {
    ahlog_err(__PRETTY_FUNCTION__,"Failed to get tlmax parameter \n");
    return status;
  } else {
    par->m_tlmax = templong;  
  }

 
  return status;

}

int initialize(Params par, fitsPointers * fitsPointers) {
  
  int status = 0;

  /*********************************/
  /* Initialize fits file pointers */
  /*********************************/

  fitsPointers->m_infptr = NULL;
  fitsPointers->m_outfptr = NULL;
  fitsPointers->m_CalOffsetFile = NULL;
  fitsPointers->m_DGGainFile = NULL;
  fitsPointers->m_CalGainFFile = NULL;
  
  /*********************************/
  /*       Open Input files        */ 
  /*********************************/
 
  /* Open the input FITS file */
  if (fits_open_file(&fitsPointers->m_infptr, par.m_infile, READONLY, &status) )
    {
      ahlog_err( __func__,"Can't open FITS input file [%s]\n",par.m_infile);
      closeFiles(fitsPointers, status);
      return(status);
    }

  /* Check if output FITS file needs to be clobbered */
  headas_clobberfile(par.m_outfile);

  /* Open the output FITS file */
  if (fits_create_file(&fitsPointers->m_outfptr, par.m_outfile, &status) )
    {
      ahlog_err( __func__,"Can't create FITS output file [%s]\n",par.m_outfile);
      closeFiles(fitsPointers, status);
      return(status);
    }

  /* Open the differential gain coeffs file */
  if (fits_open_file(&fitsPointers->m_DGGainFile, par.m_gainCoefFile, READONLY, &status))
    {
      ahlog_err( __func__,"Can't open the differential gain coeffs file [%s]\n",par.m_gainCoefFile);
      closeFiles(fitsPointers, status);
      return(status);
    }

  /* Open the cal pixel offsets file */
  if (fits_open_file(&fitsPointers->m_CalOffsetFile, par.m_calOffsetFile, READONLY, &status))
    {
      ahlog_err( __func__,"Can't open the cal pixel offsets file [%s]\n",par.m_calOffsetFile);
      closeFiles(fitsPointers, status);
      return(status);
    }

  if (strcasecmp(par.m_driftfile,"NONE") != 0) 
    {
      if (fits_open_file(&fitsPointers->m_CalGainFFile, par.m_driftfile, READONLY, &status))
        {
          ahlog_err( __func__,"Cannot open pixel gain FITS file = [%s]\n",par.m_driftfile);
          closeFiles(fitsPointers, status);
          return(status);
        }
    }
  
  return status;
}

int doWork(Params par, columnData * coldat, fitsPointers * fitsPointers) {

  int status = 0;                      /* CFITSIO status value MUST be initialized to zero!  */
  int columnCheck = 0;                 /* CFITSIO status value used to check if columns exist */
  int hdutype = 0;                     /* Type of fits extension */
  long inrows = 0;                     /* Number of rows in fits file */
  int incols = 0;                      /* Number of columns in fits file */
  int TimeCol = 0;                     /* Column number of the TIME column gain history file */
  int EPICol = 0;                      /* Column number EPI column input event file */
  int EPI2Col = 0;                     /* Column number EP2 column input event file */
  int PICol = 0;                       /* PI column input event file */
  int tPICol=0;                        /* Used to check if PIE column exists in extended energy mode */
  int PixelCol = 0;                    /* Pixel column input event file */
  int ITYPECol = 0;                    /* ITYPE column input event file */
  int FitCol = 0;                      /* COR_FIT or COR_AVG column number gain history file */
  int GainCol = 0;                     /* Gain Column number differential gain file */
  int OffsetCol = 0;                   /* Offset column number offset file */
  int EPIPERCol = 0;                   /* EPIPERCol column number */
  int EPI2PERCol = 0;                  /* EPI2PER column number */

  int anynull = 0;                     /* anynull CFITSIO parameter */
  long frow = 0;                       /* first row CFITSIO paramter */
  long felem = 0;                      /* first element CFITSIO paramter */
  long nelem = 0;                      /* number of elements CFITSIO paramter */
  double doublenull = 0.;              /* null value for double columns */
  long i = 0;                          /* index counter used reading gain history file */
  long j = 0;                          /* index counter used reading gain fistory file */
  long CurrCalPixPoint1 = 0;           /* index of first gain point used in interpolation */ 
  long CurrCalPixPoint2 = 0;           /* index of second gain point used in interpolation */
  long loop1 = 0;                      /* counters for events */
  long loopend = 0;                    /* end point for events processing */
  int done = 0;                        /* done flag for events events processing */
  double Offset1 = 0.;                 /* first offset used in interpolation */
  double Offset2 = 0.;                 /* second offset use in interpolation */
  double Temp1 = 0.;                   /* Intermediate step in interpolation */
  double Temp2 = 0.;                   /* Intermediate  step in interpolation */
  long NumCalPoints = 0;               /* Number of points read from gain history file */  
  double FWCalOffsets[50];             /* filterwheel calibration offsets */
  double DG_Gain[50];                  /* differential gains */
  char charnull;                       /* null value for characters CFITSIO parameter */
 
  double TempDouble = 0.;              /* Used for inversion of gain history file values */             
  char caltype[FLEN_KEYWORD];          /* CALTYPE keyword gain histroy file */
  char tnullPIKeyName[FLEN_KEYWORD];   /* tnull value for PI column */
  char *tnullPIKeyNamePointer;         /* pointer to tnull keyword name */
  char tlmaxPIKeyName[FLEN_KEYWORD];   /* tlmax keyword name pi column */
  char *tlmaxPIKeyNamePointer;         /* pointer to tlmax keyowrd name */
  char tlminPIKeyName[FLEN_KEYWORD];   /* tlmin keyword name pi column */
  char *tlminPIKeyNamePointer;         /* pointer to TLMIN key name */
  long tnullPI = 0;                    /* tnull value PI column */
  long tlmaxPI = 0;                    /* tlmax value PI column */
  long tlminPI = 0;                    /* tlmin value PI column */
  double offsetPI = 0.;                /* from parameter PI offset */
  double widthPI = 0.;                 /* from parameter PI width */

  char * EPIcolName;       /* Name of epi column */
  char * EPI2colName;      /* Name of epi2 columm */
  char * EPIPERcolName;    /* Name of epiper column */
  char * EPI2PERcolName;   /* Name of epi2per columm */  
  char * PIcolName;        /* Name of pi column */
  char * PERextn;          /* PER extension for EPIPER EPI2PER column names */
  
  char tlmaxPIEKeyName[8];             /* tlmax PIE column keyname */
  char tlminPIEKeyName[8];             /* tlmin PIE column keyname */
  char tnullPIEKeyName[8];             /* tnull PIE column keyname */
 
  /* Initialize column data arrays */
  coldat->m_myEPI = 0;
  coldat->m_myEPI2 = 0;
  coldat->m_myPI = 0;
  coldat->m_myITYPE = 0;
  coldat->m_myTime = 0;
  coldat->m_CalPix_Time = 0;
  coldat->m_CalPix_Gain = 0;
  coldat->m_tmpCalPix_Time = 0;
  coldat->m_tmpCalPix_Gain = 0;
  coldat->m_myPixel = 0;


  /********************************************************/
  /* All files open. Proceed to read in the data we need. */
  /********************************************************/


  /* Read the calpixel gain */

  NumCalPoints = 0;  


  /* Move to the right HDU: */
  if (fits_movabs_hdu(fitsPointers->m_CalGainFFile, 2, &hdutype, &status)) { 
    ahlog_err( __func__,"Failed to move to extension 2 in gain history file");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  /* Get file input file geometry */
  if (fits_get_num_cols(fitsPointers->m_CalGainFFile,  &incols,  &status)) {

    ahlog_err( __func__,"Failed to get number of column in gain history file status = %d \n",status);
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
  if (fits_get_num_rows(fitsPointers->m_CalGainFFile,  &inrows,  &status)) {
    ahlog_err( __func__,"Failed to get number of column in gain history file status = %d \n",status);
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  /* Check the gain history file is from a cal-pix run of sxsgain */
  if (fits_read_key(fitsPointers->m_CalGainFFile,TSTRING,(char*)"CALTYPE",caltype,NULL,&status)) {
    ahlog_err( __func__,"Failed to read keyword CALTYPE status = %d \n",status);
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  } 
  if (strcasecmp(caltype,"CAL-PIX") != 0) { 
    ahlog_err( __func__,"CALTYPE keyword is not the correct value, expected CAL-PIX, make sure sxsgain was run \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  /* Translate the column names to indices */
  if (fits_get_colnum(fitsPointers->m_CalGainFFile, CASEINSEN, "TIME", &TimeCol, &status)) {
    ahlog_err( __func__,"Failed to get column number for the TIME column gain history file");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  } 

  if (strcasecmp(par.m_method,"FIT") == 0 ) {
    if (fits_get_colnum(fitsPointers->m_CalGainFFile, CASEINSEN, "COR_FIT", &FitCol, &status)) {
       ahlog_err( __func__,"Failed to get column number for the COR_FIT column gain history file \n");
       deleteColdat(coldat);
       closeFiles(fitsPointers, status);
       return status;
    }
  } else if (strcasecmp(par.m_method,"AVERAGE") == 0) { 
    if (fits_get_colnum(fitsPointers->m_CalGainFFile, CASEINSEN, "COR_AVE", &FitCol, &status)) {
       ahlog_err( __func__,"Failed to get column number for the COR_AVE column gain history file \n");
       deleteColdat(coldat);
       closeFiles(fitsPointers, status);
       return status;
    }
  }     


  /* make arrays to hold the Time, EPI, and Pixel columns */
  if ((coldat->m_CalPix_Gain = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate Cal Pix Gain memory\n");
    exit (1);
  }

  if ((coldat->m_CalPix_Time = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate TIME  memory\n");
    exit (1);
  }

  if ((coldat->m_tmpCalPix_Gain = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate Cal Pix Gain memory\n");
    exit (1);
  }

  if ((coldat->m_tmpCalPix_Time = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate TIME  memory\n");
    exit (1);
  }
      
  /* Read the Time, EPI, and Pixel columns into memory */
  frow      = 1;
  felem     = 1;
  nelem     = inrows;
  doublenull = -999.0;
      

  /* read the columns */
  if (fits_read_col(fitsPointers->m_CalGainFFile, TDOUBLE, TimeCol, frow, felem, nelem, &doublenull, coldat->m_tmpCalPix_Time,
      &anynull, &status)) {
    ahlog_err( __func__,"Failed to read column TIME in gain history file");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }    


  if (fits_read_col(fitsPointers->m_CalGainFFile, TDOUBLE, FitCol, frow, felem, nelem, &doublenull, coldat->m_tmpCalPix_Gain,
                    &anynull, &status)) {
    ahlog_err( __func__,"Failed to read fit column (COR_FIT or COR_AVE depending on method parameter) in gain history file status = %d", status);
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
      


  /* Now, for historical reasons we want the cal pixel gain and not the 
     correction factor so we need to invert.  Check and make sure there 
     are no zero values, which will generate an error (better than seg 
     faulting!). */

  j = 0;
  for (i=0;i<inrows;i++)
    {
      TempDouble = coldat->m_tmpCalPix_Gain[i];
      if (coldat->m_tmpCalPix_Gain[i] != doublenull) {
        coldat->m_CalPix_Gain[j] = 1/TempDouble;
        coldat->m_CalPix_Time[j] = coldat->m_tmpCalPix_Time[i]; 
        j++;
      } 
    } /* end of for */

  NumCalPoints = j;
      

  ahlog_info(HIGH, __func__,"Number of CalPix Gain points = %ld\n",NumCalPoints);
  
  /* Read the differential gain slopes */
  

  /* Move to the right HDU: */
  if (fits_movabs_hdu(fitsPointers->m_DGGainFile, 2, &hdutype, &status)) {

    ahlog_err(__func__,"failed to move to the first extension in dggain file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  /* Get file input file geometry */
  if (fits_get_num_cols(fitsPointers->m_DGGainFile,  &incols,  &status)) {

    ahlog_err(__func__,"Failed to obtain number of columns dggain file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  if (fits_get_num_rows(fitsPointers->m_DGGainFile,  &inrows,  &status)) {
    ahlog_err(__func__,"Failed to obtain number of rows dggain file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  /* Translate the column names to indices */
  if (fits_get_colnum(fitsPointers->m_DGGainFile, CASEINSEN, "GAIN", &GainCol, &status)) { 
    ahlog_err(__func__, "failed to get column number for GAIN column in dggain file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status); 
    return status;
  }

  frow      = 1;
  felem     = 1;
  nelem     = inrows;
  doublenull = 0.;
      

  /*  read the column */ 
  if (fits_read_col(fitsPointers->m_DGGainFile, TDOUBLE, GainCol, frow, felem, nelem, &doublenull, DG_Gain,
                    &anynull, &status)){ 
    ahlog_err(__func__, "failed to read column GAIN in dggain file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  /* Read the FW 55-Fe offsets */

 
  /* Move to the right HDU: */
  if (fits_movabs_hdu(fitsPointers->m_CalOffsetFile, 2, &hdutype, &status)) {

    ahlog_err(__func__,"failed to move to the first extension in offset file \n");
    deleteColdat(coldat); 
    closeFiles(fitsPointers, status);
    return status;
  }


  /* Get file input file geometry */
  if (fits_get_num_cols(fitsPointers->m_CalOffsetFile,  &incols,  &status)) {

    ahlog_err(__func__,"Failed to obtain number of columns offset file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  if (fits_get_num_rows(fitsPointers->m_CalOffsetFile,  &inrows,  &status)) {
    ahlog_err(__func__,"Failed to obtain number of columns offset file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  /* Translate the column names to indices */
  if (fits_get_colnum(fitsPointers->m_CalOffsetFile, CASEINSEN, "OFFSET", &OffsetCol, &status)) { 

    ahlog_err(__func__, "failed to get column number for OFFSET column in offset file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  frow      = 1;
  felem     = 1;
  nelem     = inrows;
  doublenull = 0.;
      
  /* read the column */ 
  if (fits_read_col(fitsPointers->m_CalOffsetFile, TDOUBLE, OffsetCol, frow, felem, nelem, &doublenull, FWCalOffsets,

                    &anynull, &status)) { 
    ahlog_err(__func__, "failed to read OFFSET column in offset file \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
  


  /***************************************************************/
  /* copy all of the HDUs from the input file to the output file */
  /***************************************************************/

  i=1;
  /* Copy every HDU until we get an error */
  while( !fits_movabs_hdu(fitsPointers->m_infptr, i++, NULL, &status) )
    fits_copy_hdu(fitsPointers->m_infptr, fitsPointers->m_outfptr, 0, &status);
  
  /* Reset status after normal error */
  if (status == END_OF_FILE)
    status = 0;
 
  /* move to the events extension */
  if (fits_movabs_hdu(fitsPointers->m_outfptr, 2, NULL, &status)) {
    ahlog_err(__func__, "failed to move to events extension infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  /* Read data from the input file: */  
  
  /* Get file input file geometry */
  fits_get_num_cols(fitsPointers->m_outfptr,  &incols,  &status);
  fits_get_num_rows(fitsPointers->m_outfptr,  &inrows,  &status);

   /* Translate the column names to indices */
  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, "TIME", &TimeCol, &status)) {
    ahlog_err(__func__, "failed to get column number TIME infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  } 

  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, "PIXEL", &PixelCol, &status)) {
    ahlog_err(__func__, "failed to get column number PIXEL infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }   

  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, "ITYPE", &ITYPECol, &status)) {
    ahlog_err(__func__, "failed to get column number ITYPE infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }  


  /* Set the name of EPI and EPI2 depending on exteded mode paramter */
  if (par.m_extended) { 
    EPIcolName = "EPIE";
    EPI2colName = "EPI2E";
    PIcolName = "PIE";
  } else {
    EPIcolName = "EPI"; 
    EPI2colName = "EPI2";
    PIcolName = "PI";
  } 


  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPIcolName, &EPICol, &status))  {
    ahlog_err(__func__, "failed to get column number EPI infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
   
  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPI2colName, &EPI2Col, &status)) {
    ahlog_err(__func__, "failed to get column number EPI2 infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
  

  /* Create EPIPER and EPI2PER Columns to store corrected EPI and EPI2 
     if these columns do not exist already, if in extended mode create 
     EPIEPER and EPI2EPER */

  /* Set the names of EPIPER and EPI2PER columns depending on the name of
     the EPI and EPI2 columns */
  PERextn = "PER";
  
  EPIPERcolName = malloc(strlen(EPIcolName) + strlen(PERextn) + 1);
  strcpy(EPIPERcolName,EPIcolName);
  strcat(EPIPERcolName,PERextn);
 
  EPI2PERcolName = malloc(strlen(EPI2colName) + strlen(EPI2colName) + 1);
  strcpy(EPI2PERcolName,EPI2colName);
  strcat(EPI2PERcolName,PERextn);

  /*printf("EPIPERcolName = %s \n",EPIPERcolName);
    printf("EPI2PERcolName = %s \n",EPI2PERcolName); */
 

  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPIPERcolName, &EPIPERCol, &columnCheck) == COL_NOT_FOUND) {
    columnCheck = 0;
    EPIPERCol = EPI2Col+1;
    if (fits_insert_col(fitsPointers->m_outfptr,EPIPERCol,(char*)EPIPERcolName,(char*)"1E",&status)) {
      ahlog_err(__func__, "failed to insert column EPIPER after EPI2 column in outfile \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    } 
  } else {
    if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPIPERcolName, &EPIPERCol, &status)) {
      ahlog_err(__func__, "failed to get column number EPIPER infile \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    }   
  }

  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPI2PERcolName, &EPI2PERCol, &columnCheck) == COL_NOT_FOUND) {
    columnCheck = 0; 
    EPI2PERCol = EPIPERCol+1;
    if (fits_insert_col(fitsPointers->m_outfptr,EPI2PERCol,(char*)EPI2PERcolName,(char*)"1E",&status)) {
      ahlog_err(__func__, "failed to insert column EPI2PER after EPIPER column in outfile \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    }
  } else {
    if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, EPI2PERcolName, &EPI2PERCol, &status)) {
      ahlog_err(__func__, "failed to get column number EPI2PER infile \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    }  
  }


  /* Translate column names to indicies for column after EPIPER and EPI2PER */


  if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, "PI", &PICol, &status)) {
    ahlog_err(__func__, "failed to get column number EPI2 infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }  

  

  /* Create additional PIE column if extended parameter is set */
  if (par.m_extended) {
    if (par.m_tlmax <= 0) {
      ahlog_err(__func__,"tlmax parameter must be positive");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    }
    if (par.m_binwidth <= 0.) {
      ahlog_err(__func__,"binwidth parameter must be positive");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    }
    tlmaxPI= par.m_tlmax;
    tlminPI=-par.m_tlmax;
    tnullPI=-par.m_tlmax-1;
    if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, PIcolName, &tPICol, &columnCheck) == COL_NOT_FOUND) {
      columnCheck = 0;
      PICol = PICol+1;
      if (fits_insert_col(fitsPointers->m_outfptr,PICol,(char*)PIcolName,(char*)"1J",&status)) {
        ahlog_err(__func__, "failed to insert column PIE after PI column in outfile \n");
        deleteColdat(coldat);
        closeFiles(fitsPointers, status);
        return status;
      } 

    } else {
      if (fits_get_colnum(fitsPointers->m_outfptr, CASEINSEN, PIcolName, &PICol, &status)) {
        ahlog_err(__func__, "failed to get column number of PI from infile \n");
        deleteColdat(coldat);
        closeFiles(fitsPointers, status);
        return status;
      }   
    }

    /* Set tlmix tlmax and tnull for pi column */
    sprintf(tnullPIEKeyName,"TNULL%d",PICol);

    fits_update_key(fitsPointers->m_outfptr,TLONG,tnullPIEKeyName,&tnullPI,NULL,&status); 
    sprintf(tlmaxPIEKeyName,"TLMAX%d",PICol);
    fits_update_key(fitsPointers->m_outfptr,TLONG,tlmaxPIEKeyName,&tlmaxPI,NULL,&status);
    sprintf(tlminPIEKeyName,"TLMIN%d",PICol);
    fits_update_key(fitsPointers->m_outfptr,TLONG,tlminPIEKeyName,&tlminPI,NULL,&status); 

    offsetPI = par.m_offset;
    widthPI = par.m_binwidth;
    
    /* force keyword values to be updated */
    fits_flush_file(fitsPointers->m_outfptr, &status);
    
  } else {

    /* Get TNULL value for PI column */
    sprintf(tnullPIKeyName,"TNULL%d",PICol);
    tnullPIKeyNamePointer = tnullPIKeyName;
    if (fits_read_key_lng(fitsPointers->m_outfptr,tnullPIKeyNamePointer,&tnullPI,NULL,&status)) {
      tnullPIKeyNamePointer = 0;
      ahlog_err( __func__,"Failed to read TNULL values for PI column \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    } 
    tnullPIKeyNamePointer = 0;

    /* Get TLMAX from PI column */
    sprintf(tlmaxPIKeyName,"TLMAX%d",PICol);
    tlmaxPIKeyNamePointer = tlmaxPIKeyName;
    if (fits_read_key_lng(fitsPointers->m_outfptr,tlmaxPIKeyNamePointer,&tlmaxPI,NULL,&status)) {
      tlmaxPIKeyNamePointer = 0;
      ahlog_err( __func__,"Failed to read tlmax values for PI column \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    } 
    tlmaxPIKeyNamePointer = 0;

    /* Get TLMIN from PI column */
    sprintf(tlminPIKeyName,"TLMIN%d",PICol);
    tlminPIKeyNamePointer = tlminPIKeyName;
    if (fits_read_key_lng(fitsPointers->m_outfptr,tlminPIKeyNamePointer,&tlminPI,NULL,&status)) {
      tlminPIKeyNamePointer = 0;
      ahlog_err( __func__,"Failed to read tlmin values for PI column \n");
      deleteColdat(coldat);
      closeFiles(fitsPointers, status);
      return status;
    } 
    tlminPIKeyNamePointer = 0;
    
    offsetPI = 0.5;
    widthPI = 0.5;
  }

  /* make arrays to hold the Time, EPI, and Pixel columns */
  if ((coldat->m_myEPI = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate EPI memory\n");
    exit (1);
  }
  
  if ((coldat->m_myEPI2 = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate EPI2 memory\n");
    exit (1);
  }


  if ((coldat->m_myPI = (int *)malloc(inrows*sizeof(int))) == NULL) {
    ahlog_err( __func__,"Can't allocate PI memory\n");

    exit (1);
  }

  if ((coldat->m_myTime = (double *)malloc(inrows*sizeof(double))) == NULL) {
    ahlog_err( __func__,"Can't allocate TIME  memory\n");
    exit (1);
  }

  if ((coldat->m_myPixel = (int *)malloc(inrows*sizeof(int))) == NULL) {
    ahlog_err( __func__,"Can't allocate Pixel memory\n");
    exit (1);
  }


  if ((coldat->m_myITYPE = (int *)malloc(inrows*sizeof(int))) == NULL) {
    ahlog_err( __func__,"Can't allocate ITYPE memory\n");
    exit (1);
  }
      
  /* Read the Time, EPI, and Pixel columns into memory */
  frow      = 1;
  felem     = 1;
  nelem     = inrows;
  doublenull = -DBL_MAX;
      

  /*  read the columns */
  if (fits_read_col(fitsPointers->m_outfptr, TDOUBLE, TimeCol, frow, felem, nelem, 
                    &doublenull, coldat->m_myTime,&anynull, &status)) {

    ahlog_err(__func__, "failed to read column TIME infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  if (fits_read_col(fitsPointers->m_outfptr, TDOUBLE, EPICol, frow, felem, nelem, &doublenull, coldat->m_myEPI,
                    &anynull, &status)) {
    ahlog_err(__func__, "failed to read column EPI infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }
  
  
  if (fits_read_col(fitsPointers->m_outfptr, TDOUBLE, EPI2Col, frow, felem, nelem, &doublenull, coldat->m_myEPI2,
                    &anynull, &status)) {
    ahlog_err(__func__, "failed to read column EPI2 infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  if (fits_read_col(fitsPointers->m_outfptr, TINT, PICol, frow, felem, nelem, &tnullPI, coldat->m_myPI,
                    &anynull, &status)) {
    ahlog_err(__func__, "failed to read column PI infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }


  if (fits_read_col(fitsPointers->m_outfptr, TINT, PixelCol, frow, felem, nelem, &charnull, coldat->m_myPixel,
                    &anynull, &status)) {
    ahlog_err(__func__, "failed to read column PIXEL infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  if (fits_read_col(fitsPointers->m_outfptr, TINT, ITYPECol, frow, felem, nelem, &charnull, coldat->m_myITYPE,
                    &anynull, &status)) {
    ahlog_err(__func__, "failed to read column ITYPE infile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }

  /***********************************/
  /* Start Event by event correction */
  /***********************************/
 
  /* Set startpoint of loop over events which do overlap 
     with gain history file */      
  loopend = inrows;  
  loop1 = 0;
  done = 0;
  /* Walk event list */
  while (!done) {
    /* Check if TIME or EPI is null */

    if (coldat->m_myTime[loop1] != doublenull && coldat->m_myEPI[loop1] != doublenull) {
      if (coldat->m_myPixel[loop1] != 12) { /* don't process cal  pixel */
       
        /* Check if Time in event file is outside of the time range in gain history file */
        if (coldat->m_myTime[loop1] < coldat->m_CalPix_Time[0] || coldat->m_myTime[loop1] > coldat->m_CalPix_Time[NumCalPoints-1]) {
          if (strcasecmp(par.m_outrange,"NULL") == 0) {  
            coldat->m_myEPI[loop1] = -DBL_MAX;
            coldat->m_myEPI2[loop1] = -DBL_MAX;
            coldat->m_myPI[loop1] = tnullPI;
            loop1 += 1;
            if (loop1 >= loopend) done = 1;
            continue;
          } else if (strcasecmp(par.m_outrange,"CONST") == 0) {
            /* Set current cal pix point to either the first of the last point */
            if (coldat->m_myTime[loop1] < coldat->m_CalPix_Time[0]) {
              CurrCalPixPoint1 = 0;
              CurrCalPixPoint2 = 0;
            } else {
              CurrCalPixPoint1 = NumCalPoints-1;
              CurrCalPixPoint2 = NumCalPoints-1;      
            }         
          } else { /* outrange = EXTRAP */
            /* Set current cal pix point to either the first or the 
               second-to-last point The EPI is then extroplated using 
               either the first and second points or 
               the second-to-last and last points */
            if (coldat->m_myTime[loop1] < coldat->m_CalPix_Time[0]) {
              CurrCalPixPoint1 = 0;
              CurrCalPixPoint2 = 1;
            } else {
              CurrCalPixPoint1 = NumCalPoints-2;
              CurrCalPixPoint2 = NumCalPoints-1;      
            }  
          }
        } else {         

          /* make sure the time is beyond the current cal point or advance it */
          while (coldat->m_myTime[loop1] >  coldat->m_CalPix_Time[CurrCalPixPoint1+1]) {

      CurrCalPixPoint1++;
          }
          CurrCalPixPoint2 = CurrCalPixPoint1+1;
        }

        if (CurrCalPixPoint1 == CurrCalPixPoint2) { 

          Temp2 = DG_Gain[coldat->m_myPixel[loop1]]*(coldat->m_CalPix_Gain[CurrCalPixPoint1] - CAL_PIX_GAIN_FIDUCIAL);

        } else {

          /* Now the current data point is between two calpix gain points */
          /* do a linear interpolation */


          Offset1 = DG_Gain[coldat->m_myPixel[loop1]]*(coldat->m_CalPix_Gain[CurrCalPixPoint1] - CAL_PIX_GAIN_FIDUCIAL);
          Offset2 = DG_Gain[coldat->m_myPixel[loop1]]*(coldat->m_CalPix_Gain[CurrCalPixPoint2] - CAL_PIX_GAIN_FIDUCIAL);
    

          /* fraction of distance between adjacent points First check for 
             divide by zero if there is no time difference between cal pix 
             gain points: */
          if ((coldat->m_CalPix_Time[CurrCalPixPoint2] - coldat->m_CalPix_Time[CurrCalPixPoint1]) == 0.0) {

            ahlog_info(HIGH, __func__,"Two adjacent cal pix gain points have the same time at index %ld\n",CurrCalPixPoint1);
            exit(2);
          }
          Temp1 = (coldat->m_myTime[loop1]-coldat->m_CalPix_Time[CurrCalPixPoint1])/(coldat->m_CalPix_Time[CurrCalPixPoint2] - coldat->m_CalPix_Time[CurrCalPixPoint1]);
        
          /* calculate the new scale point */
          Temp2 = Temp1*(Offset2-Offset1)+ Offset1;
        }


        coldat->m_myEPI[loop1] = calcEPI(Temp2,coldat->m_myEPI[loop1],FWCalOffsets[coldat->m_myPixel[loop1]]);
        coldat->m_myEPI2[loop1] = calcEPI(Temp2,coldat->m_myEPI2[loop1],FWCalOffsets[coldat->m_myPixel[loop1]]);
       
        /* Calculate PI */
        coldat->m_myPI[loop1] = floor( (coldat->m_myEPI2[loop1]-offsetPI)/widthPI + 1.0);
    
        /* check if PI is out of range */
        if (coldat->m_myITYPE[loop1] == 5) {   /* baseline */
          if ( (coldat->m_myPI[loop1] < tlminPI) || (coldat->m_myPI[loop1] > tlmaxPI) ) {
            coldat->m_myPI[loop1] = tnullPI;
          }
        } else { /* not baseline, e.g. PIXEL (or lost/rejected) */
          if (coldat->m_myPI[loop1] < 0) {
            coldat->m_myPI[loop1] = tnullPI;
          } else if (coldat->m_myPI[loop1] > tlmaxPI) {
            coldat->m_myPI[loop1]=tlmaxPI;
          }
        }

      } /* end of if pixel not 12 */
    } /* end if TIME != NULL and EPI != NULL */
    
    loop1 += 1;
    if (loop1 >= loopend) done = 1;
  } /* end of while walking event list */

  /********************************************************/
  /* Write the corrected EPI EPI2 and PI (or PIE) columns */
  /********************************************************/

  frow      = 1;
  felem     = 1;
  nelem     = inrows;
  doublenull = -DBL_MAX;

  if (fits_movabs_hdu(fitsPointers->m_outfptr, 2, NULL, &status)) {

    ahlog_err(__func__, "failed to move to EVENTS extension outfile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }    
  
  if (fits_write_colnull(fitsPointers->m_outfptr, TDOUBLE, EPIPERCol, frow, felem, nelem, coldat->m_myEPI,
                       &doublenull,&status)) {
    ahlog_err(__func__, "failed to write EPIPER column outfile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }   

  if (fits_write_colnull(fitsPointers->m_outfptr, TDOUBLE, EPI2PERCol, frow, felem, nelem, coldat->m_myEPI2,
                       &doublenull,&status)) {
    ahlog_err(__func__, "failed to write EPI2PER column outfile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }   

  if (fits_write_colnull(fitsPointers->m_outfptr, TINT, PICol, frow, felem, nelem, coldat->m_myPI,
                       &tnullPI,&status)) {
    ahlog_err(__func__, "failed to write PI column outfile \n");
    deleteColdat(coldat);
    closeFiles(fitsPointers, status);
    return status;
  }   

  return status;

}
 

int finalize(columnData * coldat, fitsPointers * fitsPointers) {
  
  int status = 0;

    /* Close all open fits files */
  closeFiles(fitsPointers, status);

  /* Free allocated memory */
  deleteColdat(coldat);



  return status;

}


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

double calcEPI(double Temp2,double EPI, double FWCalOffsets) {

  double OrigEPI = 0.;
  double myOffset = 0.;
  double myOffset2 = 0.;


  OrigEPI = EPI;
     

  /* Subtract the differential gain error for this point. Hard coded 
     value is the fiducial that the gain errors were calculated from
     (He-like Fe w, at redshift 0.0179)
     6582.93 is the Fe 25 helium like line at z=0.0179 */

  myOffset = Temp2*EPI/6582.93;
        
  EPI = OrigEPI-myOffset;


  /* Subtract the 55-Fe fudical error (at 5.9keV) scaled to the current point. 
     Hard coded value is the center of mass of the Mn Ka line. */


  myOffset2 = FWCalOffsets*EPI/5894.57;

  
  EPI = EPI - myOffset2;
  

  return EPI;

}

int deleteColdat(columnData * coldat) {

  /* Free column data arrays */
  if (coldat->m_myEPI != 0) free(coldat->m_myEPI);
  if (coldat->m_myEPI2 != 0) free(coldat->m_myEPI2);
  if (coldat->m_myPI != 0) free(coldat->m_myPI);
  if (coldat->m_myITYPE != 0) free(coldat->m_myITYPE);
  if (coldat->m_myTime != 0) free(coldat->m_myTime);
  if (coldat->m_CalPix_Time != 0) free(coldat->m_CalPix_Time);
  if (coldat->m_CalPix_Gain != 0) free(coldat->m_CalPix_Gain);
  if (coldat->m_tmpCalPix_Time != 0) free(coldat->m_tmpCalPix_Time);
  if (coldat->m_tmpCalPix_Gain != 0) free(coldat->m_tmpCalPix_Gain);
  if (coldat->m_myPixel != 0) free(coldat->m_myPixel);


  /* Set column data arrays to null */
  coldat->m_myEPI = 0;
  coldat->m_myEPI2 = 0;
  coldat->m_myPI = 0;
  coldat->m_myITYPE = 0;
  coldat->m_myTime = 0;
  coldat->m_CalPix_Time = 0;
  coldat->m_CalPix_Gain = 0;
  coldat->m_tmpCalPix_Time = 0;
  coldat->m_tmpCalPix_Gain = 0;
  coldat->m_myPixel = 0;

  return 0 ;
}

int closeFiles(fitsPointers * fitsPointers, int status) {

 
  /* Close open files */
  if (fitsPointers->m_CalGainFFile != NULL) {
    if (fits_close_file(fitsPointers->m_CalGainFFile, &status)) {
      ahlog_err( __func__,"Failed to close gain history fits file. \n");
      return status;
    }
  } 

  if (fitsPointers->m_DGGainFile != NULL) {
    if (fits_close_file(fitsPointers->m_DGGainFile, &status)) { 
      ahlog_err(__func__, "Failed to close differential gain file. \n");
      return status;
    }
  }  

  if (fitsPointers->m_CalOffsetFile != NULL) {
    if (fits_close_file(fitsPointers->m_CalOffsetFile, &status)) { 
      ahlog_err(__func__, "Failed to close offset file. \n");
      return status;
    }
  }

  if (fitsPointers->m_infptr != NULL) {
    if (fits_close_file(fitsPointers->m_infptr, &status)) {
      ahlog_err(__func__, "Failed to close input file. \n");
      return status;
    }
  }

  if (fitsPointers->m_outfptr != NULL) {
    if (fits_close_file(fitsPointers->m_outfptr, &status)) {
      ahlog_err(__func__, "Failed to close outfile. \n");
      return status;
    }
  }
  
  /* Set pointer to null */
  fitsPointers->m_infptr = NULL;
  fitsPointers->m_outfptr = NULL;
  fitsPointers->m_CalOffsetFile = NULL;
  fitsPointers->m_DGGainFile = NULL;
  fitsPointers->m_CalGainFFile = NULL;

  return status;

}


/** @} */
/* Revision Log
 $Log: sxsperseus.c,v $
 Revision 1.16  2016/11/03 15:56:53  mwitthoe
 sxsperseus: fix bug where the PIE column was being inserted in the event file without checking if it already existed

 Revision 1.15  2016/10/13 19:05:52  mdutka
 Now reading EPI(2)E columns when in extended mode, whne in exteded mode add EPI(2)EPER instead of EPIPER

 Revision 1.14  2016/06/29 16:14:52  mdutka
 Updating sxsperseus to add column EPIPER and EPI2PER instead of editing columns EPI and EPI2


*/
