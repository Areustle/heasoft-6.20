/*
 *	xrttimetag.c:
 *
 *	INVOCATION:
 *
 *		xrttimetag [parameter=value ...]
 *
 *	DESCRIPTION:
 *           Routine to compute photon arrival time photons 
 *
 *	DOCUMENTATION:
 *        
 *           XRT-PSU-037 "XRT Science Algorithms"
 * 
 *	CHANGE HISTORY:
 *           0.1.0 - BS 13/11/2002 - First version
 *           0.2.0 -    13/05/2003 - Added check on read out time in 'CalcTimeRow' 
 *                                   and 'CalcTimePix' functions
 *           0.2.1 -    04/07/2003 - Added routine to read telescope definition file 
 *                                   for compute DETX in Windowed Timing Mode
 *           0.2.2 -    04/08/2003 - Added history blocks into output file
 *           0.2.3 -    04/09/2003 - Added routine to write or update GTI extension
 *           0.2.4 -    08/10/2003 - Implemented GTI computation without first 'npixels' 
 *                                   (parameter) events for PD and PU Modes.
 *           0.2.5 -    09/10/2003 - Implemented GTI computation without frame with 
 *                                   pixels greater than ULD percentage higher than 
 *                                   percent (parameter)
 *                                 - Added computation of ONTIME and TIMEDEL keywords
 *           0.2.6 -    15/10/2003 - Bug fixed
 *           0.2.7 -    03/11/2003 - Replaced call to 'headas_parstamp()' with 'HDpar_stamp()'
 *           0.2.8 -    18/11/2003 - Used routine in 'xrttimefunc' to compute times
 *           0.2.9 -    21/11/2003 - Used routine in headas_gti to handle gti extension
 *           0.2.10-    25/11/2003 - Added XRTDETXY keyword in PHOTODIODE modes
 *           0.3.0 -    09/03/2004 - Added routines to compute source DETX/Y starting from
 *                                   RA and Dec
 *                                 - Implemented routines to fill DETX/Y columns and to add 
 *                                   and fill only for WT X/Y columns using "attitude" routines
 *                                 - Added new parameters 'attfile', 'ra', 'dec', 'ranom' and
 *                                   'decnom' and deleted 'detx' and 'dety'
 *           0.3.1 -    24/03/2004 - Minor changes 
 *           0.3.2 -    26/03/2004 - Deleted input files list handling
 *           0.3.3 -    28/04/2004 - Added 'usehkkey' to use 'XRA_PNT', 'XDEC_PNT', 'XRA_OBJ' and 
 *                                   'XDEC_OBJ' keywords values of the Housekeeping corrected file
 *                                   to compute arrival time. 
 *           0.3.4 -    17/05/2004 - Messages displayed revision
 *           0.3.5 -    28/05/2004 - Used "HDget_frac_time" routine to read
 *                                   MJDREF keyword
 *           0.3.6 -    24/06/2004 - Bug fixed on 'fits_update_key' for 
 *                                   LOGICAL keyword
 *           0.3.7 -    09/07/2004 - Endian bug on Mac OS X and Solaris fixed
 *                                   (See "Build 8 status report" sent via 
 *                                   mail by Mike Tripicco)
 *           0.3.8 -    29/07/2004 - Added routine to write or update GTI extension
 *                                 - renamed 'hkfile' parameter in 'hdfile'
 *           0.3.9 -    03/08/2004 - Bug fixed
 *                                 - Added or update XRA/DEC_PNT XRA/DEC_OBJ keywords
 *                                 - Check OBS_MODE keyword and in case of SLEW and PD using 
 *                                   the DETX/Y of the detector center
 *           0.3.10-    24/09/2004 - Added TNULL for X,Y,DETX and DETY columns  
 *           0.3.11- RP 01/10/2004 - Added check on column wm1stcol and n_col
 *                                   Print number of pixels out of CCD
 *           0.3.12-    13/10/2004 - Added check on TIME = -1 before create the GTI
 *           0.4.0 - FT 14/10/2004 - Changed GTI generation method
 *           0.4.1 - FT 19/10/2004 - fixed bug on Alpha: floating exception
 *           0.4.2 - BS 02/11/2004 - Bug fixed 
 *           0.4.3 -    22/11/2004 - Modified GetGTIValues routine to handle obs_mode=SLEW
 *           0.4.4 -    23/11/2004 - Modified GetGTIValues routine to handle WT first frame
 *           0.4.5 -    01/12/2004 - Added test for infinity or not-a-number on calculated coordinates
 *           0.4.6 -    17/12/2004 - Allow data processing for all data modes Slew and Settling Data
 *           0.4.7 -    20/01/2005 - Added routine to set TIME column to NULL  when the source position
 *                                   is out of CCD 
 *                                 - Check obsmode in GetGtiValue routine
 *                                 - Modified routine to reconstruct photon arrival times   
 *           0.4.8 -    03/02/2005 - Added 'attinterpol' and 'aberration' input pars
 *                                 - Changed algorithm to calculate time row for WT and time pixels for PDs
 *           0.4.9 -               - Bug Fixed
 *           0.4.10-    04/02/2005 - Added check on TimePix return value
 *           0.4.11-    07/02/2005 - Bug fixed 
 *           0.4.12-    08/02/2005 - Added handling of photodiode variable dimension frame 
 *                                 - Modified routine to calculate GTI extension
 *                                 - Erased GetGtiValues routine
 *           0.4.13-    10/02/2005 - Re-used GetGtiValues routine
 *           0.4.14-    11/02/2005 - bug fixed
 *           0.4.15-    02/03/2005 - Modified routines to calculate rows and pixels time
 *                                   to improve task efficency
 *           0.4.16-    09/03/2005 - Modified to use fixed detx/y values to tag times
 *           0.4.17-    22/03/2005 - Bug fixed
 *           0.4.18-    23/03/2005 - Bug fixed and handled 'XRA/XDEC_PNT/OBJ' and/or 'XRTDETX/Y' keywords 
 *                                   Deleted SlewTimeTag routine
 *           0.5.0 -    24/03/2005 - Minor change
 *           0.5.1 - FT 30/03/2005 - Bug Fixed on Alpha (XRTDETX/Y keyword)
 *           0.6.0 - BS 19/04/2005 - Added new input parameter 'trfile' and 
 *                                   check on WT frame dimension to create WT GTI.
 *                 -    04/05/2005 - Exit with error if infile is empty
 *           0.6.1 - FT 20/05/2005 - Added 'sens' when check the start frame time
 *                                   versus the trailer start/stop time
 *           0.6.2 - BS 13/05/2005 - Bug fixed in GetGtiValues routine
 *           0.6.3 -    07/06/2005 - Modified GetGtiValues to merge GTI from consecutive 
 *                                   frames
 *                                 - Added check on 'infile' Bintable rows 
 *           0.6.4 -               - checked if start exposure time is greater than stop exposure time
 *           0.6.5 -    27/06/2005 - Set time keywords to zero when GTI is empty
 *           0.6.6 -    30/06/2005 - Subtracted a read row time every photon for WT mode
 *                                   (XRT Calibration Meeting - 28/29 June 2005 Leicester)
 *           0.6.7 -    13/07/2005 - Replaced ReadBintable with ReadBintableWithNULL
 *                                   to perform check on undefined value reading hdfile
 *           0.6.8 -    14/07/2005 - Exit with warning if infile is empty
 *           0.6.9 -    09/08/2005 - Create outfile if task exits with warning
 *           0.7.0 -    03/02/2006 - Added HISTORY lines in the output file
 *           0.7.1 -    21/02/2006 - Added new input par 'colfile' to correct telemetred rawx
 *                                   and add and fill RAWXTL colunm, if needed
 *                                 - If 'trfile' input parameter set to 'NONE' use a default 
 *                                   offset value
 *           0.7.2 -    23/02/2006 - Added XRAWXCOR keyword in the output file
 *                                 - Moved 'ReadColFile' routine in xrt/lib/xrtcaldb/ReadCalFile.c
 *           0.7.3 - AB 25/05/2006 - Updating of WM1STCOLNUM when shift is applied, implemented
 *           0.7.3 - AB            - Comment to WM1STCOL updated as CARD_COMM_NEWWM1STCOL
 *           0.7.4 - NS 31/10/2006 - Query to CALDB for teldef file with 'DATE-OBS' and 'TIME-OBS'
 *                                   of input event file
 *                                 - Added ATTFLAG keyword in 'EVENTS' hdu of output file
 *           0.7.5 -    08/02/2007 - If WMCOLNUM or WM1STCOL in trfile have values out of range
 *                                   is used the default value
 *           0.7.6 -    12/03/2007 - Changed CalcTimeRows, CalcTimePixs and GetGtiValues function to
 *                                   accept Settled=1 In10Arcm=0 InSafeM=1 for events with obsmode=SLEW
 *           0.7.7 -    30/10/2007 - Bug fixed in 'TIMEDEL' keyword updating
 *           0.7.8 -    29/09/2008 - Changed CalcTimeRows, CalcTimePixs and GetGtiValues function to
 *                                   to evaluate obsmode=SLEW when InSafeM=1 
 *           0.7.9 -    03/11/2011 - Modified 'TIMEDEL' keyword updating
 *                                   Update 'TIMEPIXR' keyword in output file
 *           0.8.0 -    17/12/2013 - Added ATTFLAG keyword in primary hdu of output file
 *                                 - Added check of the 'ATTFLAG' keyword of the input attitude file
 *                                 - Removed GetAttitudeATTFLAG routine (moved in highfits library)
 *           0.8.1 -    17/02/2014 - Modified check of the 'ATTFLAG' keyword of the input attitude file
 *
 *                                    
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrttimetag  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrttimetag.h" 

/********************************/
/*         definitions          */
/********************************/

#define XRTTIMETAG_C
#define XRTTIMETAG_VERSION     "0.8.1"
#define PRG_NAME               "xrttimetag"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrttimetag_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrttimetag.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void xrttimetag_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */


int xrttimetag_getpar()
{
  
  /*struct timeval     tv, tz;*/
  /*
   *  Get File name Parameters
   */
  
  /* Input Event List Files Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  /* Output File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;
    }

  /* Input hk File Name */
  if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
      goto Error;
    }

  if (ReadDatamode(global.par.infile,global.datamode))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read %s keyword\n", global.taskname, KWNM_DATAMODE); 
      headas_chat(NORMAL,"%s: Error: in %s file.\n", global.taskname,global.par.infile);
      goto Error;
    }
  
  global.pd=0;
  if(strcasecmp(global.datamode, KWVL_DATAMODE_TM))
    {
      global.pd=1;
      if(PILGetInt(PAR_NPIXELS, &global.par.npixels)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NPIXELS);
	  goto Error;	
	}
      
      if(PILGetReal(PAR_PERCENT, &global.par.percent)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PERCENT);
	  goto Error;	
	}  
    }
  else
    {
      /* Input tr File Name */
      if(PILGetFname(PAR_TRFILE, global.par.trfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TRFILE);
	  goto Error;
	}
      /* Input wt column File Name */
      if(PILGetFname(PAR_COLFILE, global.par.colfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_COLFILE);
	  goto Error;
	}
    }


  if(PILGetBool(PAR_USEHKKEY, &global.par.usehkkey)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USEHKKEY);
      goto Error;
    }

  if(PILGetBool(PAR_USESRCDET, &global.par.usesrcdet)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USESRCDET);
      goto Error;
    }


  if(global.par.usesrcdet && !global.par.usehkkey)
    {
      /* Input nominal RA */
      if(PILGetInt(PAR_SRCDETX, &global.par.srcdetx))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETX); 
	  goto Error;	 
	} 
      /* Input nominal RA */
      if(PILGetInt(PAR_SRCDETY, &global.par.srcdety))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETY); 
	  goto Error;	 
	} 
    }

  if(!global.par.usesrcdet || (global.par.usesrcdet && !global.pd))
    {
      /* Input teldef file name or CALDB */
      if(PILGetFname(PAR_TELDEF, global.par.teldef))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TELDEF);
	  goto Error;
	}
	  
      /* Input attitude File Name */
      if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
	  goto Error;
	}
  
      /* Input nominal RA */
      if(PILGetReal(PAR_RANOM, &global.par.ranom))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_RANOM); 
	  goto Error;	 
	} 
      
      /* Input nominal Dec */
      if(PILGetReal(PAR_DECNOM, &global.par.decnom)) 
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DECNOM); 
	  goto Error;	 
	}
      
      if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ABERRATION);
	  goto Error;
	}
      
      if(PILGetBool(PAR_ATTINTERPOL, &global.par.attinterpol))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ATTINTERPOL);
	  goto Error;
	}
    }

  if(!global.par.usehkkey && !global.par.usesrcdet)
    {
      /* Input source RA */
      
      if(PILGetReal(PAR_SRCRA, &global.par.srcra)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCRA);
	  goto Error;	
	}
      /* Input source Dec */
      if(PILGetReal(PAR_SRCDEC, &global.par.srcdec)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDEC);
	  goto Error;	
	} 
	
    }

  get_history(&global.hist);
  xrttimetag_info();
  

  return OK;

 Error:
  return NOT_OK;
  
} /* xrttimetag_getpar */

/*
 *	xrttimetag_work
 *
 *
 *	DESCRIPTION:
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           char * strtok(char *, char *);
 *           FitsFileUnit_t OpenReadFitsFile(char *name);
 *           int headas_chat(int , char *, ...);
 *           int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *           int CloseFitsFile(FitsFileUnit_t file);
 *           BOOL KeyWordMatch( FitsFileUnit_t unit, const char *KeyWord, ValueTag_t vtag, 
 *                              const void *NominalValue, void *ActValue);
 *           FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *           SPTYPE GetSVal(const FitsHeader_t *header, const char *KeyWord);
 *           char * strcpy(char *, char *);
 *           int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *           pid_t getpid(void);
 *           int sprintf(char *str, const char *format, ...);
 *           char *DeriveFileName(const char *OldName, char *NewName, const char *ext);
 *           BOOL FileExists(const char *FileName);
 *           FitsFileUnit_t OpenWriteFitsFile(char *name);
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype,
 *                               int *status ); 
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status); 
 *           int TimeTag(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 *           size_t strlen(const char *s);
 *	
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 15/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */

int xrttimetag_work()
{
  int            status = OK, evExt, inExt, outExt, gtiExt,i=0;
  int            logical, n=0;
  int            old_wm1stcol;
  char		 charstr[100], *newcard;
  char           attflag[FLEN_KEYWORD];
  long           extno, nrows;
  BOOL           nogti=0;
  double         tstart=0.0, tstop=0.0, elapse=0.0, ontime=0.0, livetime=0.0, exposure=0.0, deadc=0.0, vdouble=0.0;
  GtiCol_t       *gtivalues;
  FitsHeader_t   head;
  FitsCard_t     *card;
  FitsFileUnit_t inunit=NULL, outunit=NULL, colunit=NULL;


  TMEMSET0( &head, FitsHeader_t );
  global.slew=0;
  global.warning=0;
  global.trnrows=0;
  global.colrows=0;
  global.hdrows=0;

  if(xrttimetag_checkinput())
    goto Error;

  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Move in events extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
      
    }

  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,inunit,global.par.infile,global.taskname);
  headas_chat(NORMAL,"%s: Info: Processing '%s' file.\n",global.taskname,global.par.infile );        
  
  /* Initialize GTI array */
  if(HDgti_init(&global.newgti))
    {
      headas_chat(CHATTY, "%s: Error: Unable to  initialize GTIs table\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: for %s file.\n", global.taskname, global.par.outfile);
      goto Error;
    }

  /* Check readout mode of input events file */
  head=RetrieveFitsHeader(inunit);

  if(!strcasecmp(global.datamode, KWVL_DATAMODE_TM))
    global.xrtmode = XRTMODE_WT;
  else if(!strcasecmp(global.datamode, KWVL_DATAMODE_PD))
    global.xrtmode= XRTMODE_LR;
  else if(!strcasecmp(global.datamode, KWVL_DATAMODE_PDPU))
    global.xrtmode= XRTMODE_PU;
  else
    {
      headas_chat(NORMAL,"%s: Error: This task does not process the datamode %s\n", global.taskname, global.datamode);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid datamodes  are:  %s, %s, and %s.\n", global.taskname, KWVL_DATAMODE_PDPU, KWVL_DATAMODE_PD,KWVL_DATAMODE_TM);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
    }
	

  global.newgti.mjdref=HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);


  if(status)
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve date-obs and time-obs from input events file  */

  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.dateobs=card->u.SVal;
      if(!(strchr(global.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	      
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
      
    }
  
  /* Retrieve obs-mode from input events file */    
  
  if((ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.obsmode, card->u.SVal);
      if(!strcasecmp (global.obsmode, KWVL_OBS_MODE_S))
	global.slew=1;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  if(global.xrtmode == XRTMODE_WT)
    {

      if(!(ExistsKeyWord(&head, KWNM_XRAWXCOR  , NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRAWXCOR)))
	{
	  headas_chat(NORMAL, "%s: Info: %s keyword not found or set to FALSE\n", global.taskname,KWNM_XRAWXCOR);
	  headas_chat(NORMAL, "%s: Info: the RAWX will be corrected.\n", global.taskname);
	  global.rawxcorr=1;
	  
	}
      else
	{

	  headas_chat(NORMAL, "%s: Info: %s keyword set to TRUE\n", global.taskname,KWNM_XRAWXCOR);
	  headas_chat(NORMAL, "%s: Info: the RAWX is already corrected.\n", global.taskname);
	  global.rawxcorr=0;
	}

      if(global.rawxcorr)
	{
	  /* Query CALDB to get col filename? */
	  if (!(strcasecmp (global.par.colfile, DF_CALDB)))
	    {
	      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_COL_DSET, global.par.colfile, HD_EXPR, &extno))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
		  goto Error;
		}
	      else
		headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.colfile);
	    }
	  
	  
	  /* Open readonly input tr file */
	  if ((colunit=OpenReadFitsFile(global.par.colfile)) <= (FitsFileUnit_t )0)
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.colfile);
	      goto Error;
	    }
	  else
	    headas_chat(NORMAL,"%s: Info: Processing %s input file.\n", global.taskname, global.par.colfile);
	  
	  /* Move in WTCOLOFFSET extension */
	  if (fits_movnam_hdu(colunit, ANY_HDU, KWVL_EXTNAME_WTCOLOFFSET, 0, &status))
	    { 
	      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_WTCOLOFFSET);
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.colfile); 
	      
	      if( CloseFitsFile(colunit))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
		  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.colfile);
		}
	      goto Error;
	      
	    }
	  
	  if(fits_get_num_rows(colunit, &nrows, &status))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to get total number of rows\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: in the '%s' file.\n ", global.taskname, global.par.colfile);
	      
	      goto Error;
	    }
	  
	  global.colrows=(int)nrows;
	  if(global.colrows)
	    {
	      global.wtcol=(WTOffsetT_t *)calloc(global.colrows, sizeof(WTOffsetT_t));
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.colfile);
	      goto Error;
	    }
	  
	  if(ReadColFile(global.par.colfile, colunit, global.colrows, global.wtcol))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to read \n", global.taskname,KWNM_OBS_MODE);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	}

      global.telemcorr=0;
      if((ExistsKeyWord(&head, KWNM_WM1STCOL  , &card))  && ((card->u.JVal >= WM1STCOL_MIN) && (card->u.JVal <=  WM1STCOL_MAX)))  
	global.wm1stcol=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Warning: In the %s file,\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Warning: the %s keyword value is out of range\n", global.taskname, KWNM_WM1STCOL);
	  headas_chat(NORMAL, "%s: Warning: using the default value: %d.\n", global.taskname, DEFAULT_WM1STCOL);
	  global.telemcorr=1;
	  global.wm1stcol=DEFAULT_WM1STCOL;
	}
      if((ExistsKeyWord(&head, KWNM_WMCOLNUM  , &card)) && ((card->u.JVal >= WT_NCOL_MIN) && (card->u.JVal <= WT_NCOL_MAX)))
	global.wmcolnum=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Warning: In the %s file,\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Warning: the %s keyword value is out of range\n", global.taskname, KWNM_WMCOLNUM);
	  headas_chat(NORMAL, "%s: Warning: using the default value: %d.\n", global.taskname, DEFAULT_NCOL);
	  global.telemcorr=1;
	  global.wmcolnum=DEFAULT_NCOL;
	}
      if ((global.wm1stcol+global.wmcolnum) > RAWX_WT_MAX ) {
	headas_chat(NORMAL, "%s: Error: In the %s file,\n", global.taskname, global.par.infile);
	headas_chat(NORMAL, "%s: Error: sum of %s and %s keywords exceeds max value.\n", global.taskname,KWNM_WM1STCOL, KWNM_WMCOLNUM);
	goto Error;

      }

    }
  else
    {
      if(ExistsKeyWord(&head, KWNM_BIASONBD, &card) && card->u.LVal )
	{
	  global.biason=1;
	}
      else
	{
	  global.biason=0;
	}
    }
  

  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
      goto Error;
    }
  
  nogti=0;
  /* Check if GTI ext exists */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_GTI, 0, &status))
    {
      if (status == BAD_HDU_NUM)
	{
	  
	  nogti=1;
	  headas_chat(NORMAL,"%s: Warning: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	  headas_chat(NORMAL,"%s: Warning: '%s' file.\n",global.taskname, global.par.infile);
	  headas_chat(NORMAL,"%s: Warning: The task will create and fill it.\n",global.taskname);
	  
	  status=0;
	}
      else
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
	  goto Error;
	}
    }
  else
    {
      /* Get GTI ext number */
      if (!fits_get_hdu_num(inunit, &gtiExt))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
	  goto Error;
	}
    }
  

  /* Query CALDB to get teldef filename? */
  if (!(strcasecmp (global.par.teldef, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto Error;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.teldef);
    }

  /* Get ATTFLAG from attitude file */

  if (GetAttitudeATTFLAG(global.par.attfile, attflag)==NOT_OK)
    {
      headas_chat(NORMAL,"%s: Error: Unable to get %s in\n", global.taskname, KWNM_ATTFLAG);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.attfile);
      goto Error;
    }
  
  if ( !strcmp(attflag, "111") || !strcmp(attflag, "101") )
    {
      headas_chat(NORMAL,"%s: Error: input attitude file sw*uat.fits not suitable for XRT data processing,\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: use the sw*sat.fits or sw*pat.fits one.\n", global.taskname);
      goto Error;
    }


  /* Create new file */
  if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY,"%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    { 
      headas_chat(CHATTY,"%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extensions before events from input to output file */
  outExt=1;
 
  while(outExt<evExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  goto Error;
	}

      if(fits_update_key(outunit, TSTRING, KWNM_ATTFLAG, attflag, CARD_COMM_ATTFLAG, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_ATTFLAG);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary output file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}

      outExt++;
    }
    
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL,"%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  /* Read HK to get useful information */
  if(global.xrtmode == XRTMODE_WT)
    {  
      if(strcasecmp(global.par.trfile, DF_NONE))
	{
	  if(ReadTrFile())
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to read %s file.\n", global.taskname, global.par.trfile);
	      goto Error;
	      
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Info: Using %s file.\n", global.taskname, global.par.trfile);
	   
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: '%s' parameter set to '%s'.\n", global.taskname, PAR_TRFILE, global.par.trfile);
	  headas_chat(NORMAL, "%s: Warning: processing only corrupted telemetry.\n", global.taskname);
	  
	}
  
      if(CalcTimeRows())
	{
	  headas_chat(CHATTY,"%s: Error: Unable to calculate read row time\n", global.taskname);
	  headas_chat(CHATTY,"%s: Error: using %s file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}
      
    }
  else
    {
      if(CalcTimePixs())
	{
	  headas_chat(CHATTY,"%s: Error: Unable to calculate read row time.\n", global.taskname);
	  headas_chat(CHATTY,"%s: Error: using %s file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}
    }




  /* fill gti array */
  if ( GetGtiValues( &global.newgti, global.xrtmode, global.obsmode) ) 
    {
      headas_chat(NORMAL,"%s: Error: Unable to calculate GTI intervals\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: using %s file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Time tag events */
  if ((Timetag(inunit, outunit)))
    {
      headas_chat(NORMAL, "%s: Error: Unable to tag times and to make coordinates trasformation\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file\n", global.taskname, global.par.outfile);
      goto Error;
    }

  if(global.warning)
    goto ok_end;
   
  
  outExt++;
  
  /* copy any extensions after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}

      if(fits_update_key(outunit, TSTRING, KWNM_ATTFLAG, attflag, CARD_COMM_ATTFLAG, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_ATTFLAG);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary output file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}

      outExt++; 
    }
 

  if(global.newgti.ngti)
    {
      
      gtivalues =(GtiCol_t *)calloc(global.newgti.ngti, sizeof(GtiCol_t));
      headas_chat(CHATTY,"%s: Info: GTI ranges are:\n", global.taskname);
      for(i = 0; i < global.newgti.ngti; i++)
	{
	  gtivalues[i].start= global.newgti.start[i]; /* Read start column */
	  gtivalues[i].stop = global.newgti.stop[i];/* Read stop column */
	  headas_chat(CHATTY,"%s: Info: START = %15.6f  STOP = %15.6f\n",global.taskname,gtivalues[i].start,gtivalues[i].stop);
	  
	}
    }
  else
    {  
      headas_chat(NORMAL,"%s: Warning: In %s output file\n", global.taskname, global.par.outfile);
      headas_chat(NORMAL,"%s: Warning: the GTI range is empty.\n", global.taskname);
    }

  if(nogti)
    {

      if(WriteGTIExt( inunit, outunit, global.newgti.ngti,gtivalues ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to add %s extension\n", global.taskname, KWVL_EXTNAME_GTI);
	  headas_chat(NORMAL, "%s: Error: in %s temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	  
	}
    }
  else
    {
      if(UpdateGTIExt(outunit, global.newgti.ngti,gtivalues ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s extension\n", global.taskname, KWVL_EXTNAME_GTI);
	  headas_chat(NORMAL, "%s: Error: in %s temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}
    }


  /* move to events extension */
  if(fits_movabs_hdu( outunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  
  /* Update time keywords in events extension */


  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTTIMES, &logical, CARD_COMM_XRTTIMES, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTTIMES);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }      
    
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTDETXY, &logical, CARD_COMM_XRTDETXY, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTDETXY);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }

  if(global.xrtmode == XRTMODE_WT)
    {
      vdouble=0.5;
      if(fits_update_key(outunit, TDOUBLE, "TIMEPIXR", &vdouble, "Bin time beginning=0.0 middle=0.5 end=1.", &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, "TIMEPIXR");
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}
    }
	
  if(global.rawxcorr)
    {
      logical=TRUE;
      if(fits_update_key(outunit, TLOGICAL, KWNM_XRAWXCOR, &logical, CARD_COMM_XRAWXCOR, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRAWXCOR);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}	
      old_wm1stcol=global.wm1stcol;
      global.wm1stcol=global.wm1stcol+global.wtcoloffset;
      newcard=CARD_COMM_NEWWM1STCOL;
      sprintf(charstr,"%s %d",newcard, old_wm1stcol);
      if(fits_update_key(outunit, TINT, KWNM_WM1STCOL, &global.wm1stcol ,charstr , &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_WM1STCOL);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}	
    }


  if(global.xrtmode == XRTMODE_WT){
    /* global.timedel=(10*15 + (global.wm1stcol + 5)*1.5 + global.wmcolnum*6.55)/1000000.; */
    global.timedel = global.header[0].delta;
  }
  else
    global.timedel=PD_TIMEDEL;


  if(fits_update_key(outunit, TDOUBLE, KWNM_TIMEDEL, &global.timedel, CARD_COMM_TIMEDEL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TIMEDEL);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  


  if(fits_read_key(outunit, TDOUBLE, KWNM_DEADC, &deadc, NULL, &status))
    {
      status=0;
      deadc=1.;
    }

  if ( global.newgti.ngti ) 
    {
      tstart=gtivalues[0].start;
      tstop=gtivalues[global.newgti.ngti-1].stop;
      elapse=gtivalues[global.newgti.ngti-1].stop-gtivalues[0].start;

      for (n=0; n < global.newgti.ngti; n++)
	ontime+= (gtivalues[n].stop - gtivalues[n].start);

      exposure=livetime=deadc*ontime; 
      
    }
  if(fits_update_key(outunit, TDOUBLE, KWNM_TSTART, &tstart, CARD_COMM_TSTART, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TSTART);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  if(fits_update_key(outunit, TDOUBLE, KWNM_TSTOP, &tstop, CARD_COMM_TSTOP, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TSTOP);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  if(fits_update_key(outunit, TDOUBLE, KWNM_TELAPSE, &elapse, CARD_COMM_TELAPSE, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_TELAPSE);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  if(fits_update_key(outunit, TDOUBLE, KWNM_ONTIME, &ontime, CARD_COMM_ONTIME, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_ONTIME);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  if(fits_update_key(outunit, TDOUBLE, KWNM_LIVETIME, &livetime, CARD_COMM_LIVETIME, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_LIVETIME);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
	
  if(fits_update_key(outunit, TDOUBLE, KWNM_EXPOSURE, &exposure, CARD_COMM_EXPOSURE, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_EXPOSURE);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  if(fits_update_key(outunit, TSTRING, KWNM_ATTFLAG, attflag, CARD_COMM_ATTFLAG, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_ATTFLAG);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }

  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }

  /* Add checksum and datasum in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
    
  /* close files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Close output fits file */
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto Error;
    } 
  

  /*Rename temporary file */
  if(rename (global.tmpfile, global.par.outfile) == -1)
    {
      headas_chat(CHATTY, "%s: Error: Unable to rename temporary file.\n", global.taskname);
      goto Error;
    }
  headas_chat(NORMAL,"%s: Info: File '%s' successfully written.\n",global.taskname,
	      global.par.outfile );
  
  
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  /*
    gettimeofday(&tv, &tz);
    printf(">>>>>>>>>> 8 %ld END >>>>>>>>\n", tv.tv_sec);
  */

 ok_end:

  if(global.warning && strcasecmp(global.par.infile, global.par.outfile) )
    {
      if(CopyFile(global.par.infile, global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outfile.\n", global.taskname);
	  goto Error;
	}
    }
  

  if(global.count)
    free(global.header);
  return OK;
  
 Error:
  /*  if ( inunit != NULL ) 
      CloseFitsFile(inunit); 
  */

  if(global.count)
    free(global.header);
  if (FileExists(global.tmpfile))
    remove(global.tmpfile);
  
  return NOT_OK;
} /* xrttimetag_work */

/*
 *	Timetag
 *
 *
 *	DESCRIPTION:
 *           Routine to compute photons time tag.
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int strcmp(const char *s1, const char *s2);
 *             int headas_chat(int ,char *, ...);
 *             void  GetGMTDateTime(char *);
 *             int sprintf(char *str, const char *format, ...);
 *
 *             void AddHistory(FitsHeader_t *header, const char *Comment);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 15/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int Timetag(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  int                amp=0, rawy, rawx,detx=0, dety=0, ccdframe_new=0, ccdframe_old=0, rawx_offset=0, first_row=1, iii=0;
  int                sizex, sizey, offset=0, jj=0, kk=0, rawx_totoffset=0, status=0, wmcolnum=0, wm1stcol=0;
  JTYPE              null_det;
  LTYPE              logical;
  char		     KeyWord[FLEN_KEYWORD];
  BOOL               offset_found=0;
  double             time_tag=0.,time = 0., time_old=0., sens=0.0000001;
  double             time_row=0., time_pix=0.;
  double             roll, pixsize, src_x, src_y, db_val;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,countNullPixel=0,numRows=0;  
  Coord_t            coordset;
  Ev2Col_t           indxcol;
  FitsCard_t         *card;
  Bintable_t	     intable;
  FitsHeader_t	     head;
  float              percent;

  /*  struct timeval     tv, tz; */

  TELDEF* teldef;
  ATTFILE* attfile;
  
  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  head=RetrieveFitsHeader(evunit);

  if(!global.par.usesrcdet || (global.par.usesrcdet && !global.pd))
    {
      /* Open Attitude file */
      attfile = openAttFile(global.par.attfile);
      
      if(global.par.attinterpol)
	setAttFileInterpolation(attfile, 1);
      else
	setAttFileInterpolation(attfile, 0);
      
      
      /* Get nominal RA */
      if(global.par.ranom < RA_MIN)
	{
	  if((ExistsKeyWord(&head, KWNM_RA_NOM  , &card)))
	    global.par.ranom=card->u.DVal;
	  else if ((ExistsKeyWord(&head, KWNM_RA_PNT  , &card)))
	    global.par.ranom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n",global.taskname, KWNM_RA_NOM, KWNM_RA_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: Please use input par '%s'.\n", global.taskname, PAR_RANOM);
	      goto timetag_end;
	    }
	} 
      
      if (global.par.ranom > RA_MAX || global.par.ranom < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal RA value is out of range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: Valid range is: [%d - %d]\n", global.taskname, RA_MIN, RA_MAX);
	  goto timetag_end;
	}
            
      /* Get nominal Dec */
      if(global.par.decnom < DEC_MIN)
	{
	  
	  if((ExistsKeyWord(&head, KWNM_DEC_NOM  , &card)))
	    global.par.decnom=card->u.DVal;
	  else if((ExistsKeyWord(&head, KWNM_DEC_PNT  , &card)))
	    global.par.decnom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n",global.taskname, KWNM_DEC_NOM, KWNM_DEC_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: Please use input par '%s'.\n", global.taskname, PAR_DECNOM);
	      goto timetag_end;
	    }
	}
      
      if (global.par.decnom > DEC_MAX || global.par.decnom < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal DEC value is out of range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: Valid range is: [%d - %d]\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto timetag_end;
	}   

      
      /* Open TELDEF file */
      teldef = readTelDef(global.par.teldef);
      
      setSkyCoordCenterInTeldef(teldef, global.par.ranom, global.par.decnom);
    
    }

  if(!global.par.usesrcdet)
    {
      /* Get source RA */
      if(global.par.srcra < RA_MIN)
	{
	  if((ExistsKeyWord(&head, KWNM_RA_OBJ  , &card)))
	    global.par.srcra=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n",global.taskname, KWNM_RA_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: Please use input par '%s'.\n", global.taskname, PAR_SRCRA);
	      
	      goto timetag_end;
	    }
	} 
      
      if (global.par.srcra > RA_MAX || global.par.srcra < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source RA value is out of range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: Valid range is: [%d - %d]\n", global.taskname, RA_MIN, RA_MAX);
	  goto timetag_end;
	}
      
      
      /* Get nominal Dec */
      if(global.par.srcdec < DEC_MIN)
	{
	  
	  if((ExistsKeyWord(&head, KWNM_DEC_OBJ  , &card)))
	    global.par.srcdec=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keywords not found\n",global.taskname, KWNM_DEC_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: Please use input par '%s'.\n", global.taskname, PAR_SRCDEC);
	      goto timetag_end;
	    }
	}
      
      
      if (global.par.srcdec > DEC_MAX || global.par.srcdec < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source DEC value is out of range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: Valid range is: [%d - %d]\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto timetag_end;
	}
   
     
      /* Compute source X/Y */
      sizex=SIZE_X;
      sizey=SIZE_Y;
      roll=270.;
      pixsize=PIX_SIZE_DEG;
      
      
      if(RADec2XY(global.par.srcra, global.par.srcdec, sizex, sizey, global.par.ranom, global.par.decnom, roll, pixsize, &src_x, &src_y))
	{
	  
	  headas_chat(NORMAL, "%s: Error: Unable to calculate X and Y position from the source and pointing coordinates.\n",global.taskname);
	  goto timetag_end;
	} 
	  
    }
  else
    {
      src_x=0.;
      src_y=0.;
    }

  /* Check if Photon arrival TIME is already computed */
  if((ExistsKeyWord(&head, KWNM_XRTTIMES  , NULLNULLCARD)) && (GetLVal(&head, KWNM_XRTTIMES)))
    {
      headas_chat(NORMAL, "%s: Info: Photon arrival times already calculated,\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: the %s column will be overwritten.\n", global.taskname, CLNM_TIME);
    }
  
  GetBintableStructure(&head, &intable, BINTAB_ROWS, 0, NULL);

  if(!intable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  /* Get cols index from name */
  nCols=intable.nColumns;
  /*  TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }

  /* CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }

  /* TIME_RO */
  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }
  
  /* For Windowed Timing Mode */
  if(global.xrtmode == XRTMODE_WT)
    {
      /* RAWX */
      if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto timetag_end;
	}


      if(global.rawxcorr)
	{
	  
	  /* RAWX */
	  if ((indxcol.RAWXTL=ColNameMatch(CLNM_RAWXTL, &intable)) == -1)
	    {
	      AddColumn(&head, &intable,CLNM_RAWXTL, CARD_COMM_RAWXTL, "1I", TUNIT|TMIN|TMAX, UNIT_CHANN, CARD_COMM_PHYSUNIT, RAWX_MIN, RAWX_MAX);
	      indxcol.RAWXTL=ColNameMatch(CLNM_RAWXTL, &intable);
	      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_RAWXTL);
	      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	    }
	}
      /* RAWX */
      if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto timetag_end;
	}
      
      /* X */
      if((indxcol.X=ColNameMatch(CLNM_X, &intable)) == -1)
	{
	  AddColumn(&head, &intable,CLNM_X,CARD_COMM_X , "I",TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT, X_MIN, X_MAX);
	  headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_X);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	  indxcol.X=ColNameMatch(CLNM_X, &intable);
	}
      else
	{
	  sprintf(KeyWord, "TLMIN%d", (indxcol.X+1));
	  null_det=X_MIN;
	  AddCard(&head, KeyWord, J, &null_det, "minimum legal value in the column");

	  sprintf(KeyWord, "TLMAX%d", (indxcol.X+1));
	  null_det=X_MAX;
	  AddCard(&head, KeyWord, J, &null_det, "maximum legal value in the column");
	}

      /* Y */
      if((indxcol.Y=ColNameMatch(CLNM_Y, &intable)) == -1)
	{
	  AddColumn(&head, &intable,CLNM_Y,CARD_COMM_Y , "I",TUNIT|TMIN|TMAX, UNIT_PIXEL, CARD_COMM_PHYSUNIT, Y_MIN, Y_MAX);
	  headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_Y);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	  ManageDMKeywords(&head, "SKY","X,Y","names of the SKY columns"); 
	  indxcol.Y=ColNameMatch(CLNM_Y, &intable);
	}
      else
	{
	  sprintf(KeyWord, "TLMIN%d", (indxcol.Y+1));
	  null_det=Y_MIN;
	  AddCard(&head, KeyWord, J, &null_det, "minimum legal value in the column");

	  sprintf(KeyWord, "TLMAX%d", (indxcol.Y+1));
	  null_det=Y_MAX;
	  AddCard(&head, KeyWord, J, &null_det, "maximum legal value in the column");
	}

      null_det=KWVL_SKYNULL;
      sprintf(KeyWord, "TNULL%d", (indxcol.X+1));
      AddCard(&head, KeyWord, J, &null_det, CARD_COMM_TNULL);
      
      sprintf(KeyWord, "TNULL%d", (indxcol.Y+1));
      AddCard(&head, KeyWord, J, &null_det, CARD_COMM_TNULL);
      sprintf(KeyWord, "TCTYP%d", (indxcol.X+1));
      AddCard(&head, KeyWord, S,KWVL_CTYPE1_SKY , "Coordinate projection TAN");
      sprintf(KeyWord, "TCTYP%d", (indxcol.Y+1));
      AddCard(&head, KeyWord,S,KWVL_CTYPE2_SKY, "Coordinate projection TAN");
      
      sprintf(KeyWord, "TCDLT%d", (indxcol.X+1));
      db_val=-PIX_SIZE_DEG;
      AddCard(&head, KeyWord, D, &db_val, "X image scale (deg/pixel)");
      sprintf(KeyWord, "TCDLT%d", (indxcol.Y+1));
      db_val=PIX_SIZE_DEG;
      AddCard(&head, KeyWord, D, &db_val, "Y image scale (deg/pixel)");
      sprintf(KeyWord, "TCRPX%d", (indxcol.X+1));
      db_val=CENTER_X;
      AddCard(&head, KeyWord, D, &db_val, "X image reference pixel");
      sprintf(KeyWord, "TCRPX%d", (indxcol.Y+1));
      db_val=CENTER_Y ;
      AddCard(&head, KeyWord, D, &db_val, "Y image reference pixel");      

      sprintf(KeyWord, "TCRVL%d", (indxcol.X+1));
      db_val=global.par.ranom;
      AddCard(&head, KeyWord, D, &db_val, "X image reference pixel coords (deg)");
      sprintf(KeyWord, "TCRVL%d", (indxcol.Y+1));
      db_val=global.par.decnom;
      AddCard(&head, KeyWord, D, &db_val, "Y image reference pixel coords (deg)");      

    }
  else
    {
      /* For photodiode lowrate and piledup readout mode */
      /* OFFSET */
      if ((indxcol.OFFSET=ColNameMatch(CLNM_OFFSET, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto timetag_end;
	}
    }

    
  /* DETX */
  if ((indxcol.DETX=ColNameMatch(CLNM_DETX, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }
  

  /* DETY */
  if ((indxcol.DETY=ColNameMatch(CLNM_DETY, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }
 
  null_det=KWVL_DETNULL;
  sprintf(KeyWord, "TNULL%d", (indxcol.DETX+1));
  AddCard(&head, KeyWord, J, &null_det, CARD_COMM_TNULL);

  sprintf(KeyWord, "TNULL%d", (indxcol.DETY+1));
  AddCard(&head, KeyWord, J, &null_det, CARD_COMM_TNULL);

  sprintf(KeyWord, "TCTYP%d", (indxcol.DETX+1));
  AddCard(&head, KeyWord, S,CLNM_DETX, "World Axis Type");
  sprintf(KeyWord, "TCTYP%d", (indxcol.DETY+1));
  AddCard(&head, KeyWord,S,CLNM_DETY, "World Axis Type");

  sprintf(KeyWord, "TCDLT%d", (indxcol.DETX+1));
  db_val=PIX_SIZE_MM;
  AddCard(&head, KeyWord, D, &db_val, "DETX image scale (deg/pixel)");
  sprintf(KeyWord, "TCDLT%d", (indxcol.DETY+1));
  db_val=PIX_SIZE_MM;
  AddCard(&head, KeyWord, D, &db_val, "DETY image scale (deg/pixel)");

  sprintf(KeyWord, "TCRPX%d", (indxcol.DETX+1));
  db_val=CENTER_DETX;
  AddCard(&head, KeyWord, D, &db_val, "DETX image reference pixel");
  sprintf(KeyWord, "TCRPX%d", (indxcol.DETY+1));
  db_val=CENTER_DETY ;
  AddCard(&head, KeyWord, D, &db_val, "DETY image reference pixel");

  sprintf(KeyWord, "TCRVL%d", (indxcol.DETX+1));
  db_val=0.;
  AddCard(&head, KeyWord, D, &db_val, "DETX image reference pixel coords (mm)");
  sprintf(KeyWord, "TCRVL%d", (indxcol.DETY+1));
  db_val=0. ;
  AddCard(&head, KeyWord, D, &db_val, "DETY image reference pixel coords (mm)");

  if(global.par.usesrcdet)
    {

      JTYPE jdetx, jdety;

      if(ExistsKeyWord(&head, KWNM_XRA_OBJ,NULL))
	DeleteCard(&head, KWNM_XRA_OBJ);
      if(ExistsKeyWord(&head, KWNM_XDEC_OBJ,NULL))
	DeleteCard(&head, KWNM_XDEC_OBJ);
      
      jdetx = global.par.srcdetx;
      jdety = global.par.srcdety;
      
      AddCard(&head, KWNM_XRTDETX, J, &jdetx, CARD_COMM_XRTDETX);
      AddCard(&head, KWNM_XRTDETY, J, &jdety, CARD_COMM_XRTDETY);
    }
  else
    {
      if(ExistsKeyWord(&head, KWNM_XRTDETX,NULL))
	DeleteCard(&head, KWNM_XRTDETX);
      if(ExistsKeyWord(&head, KWNM_XRTDETY,NULL))
	DeleteCard(&head, KWNM_XRTDETY);
    
      /* write ra and dec values */
      AddCard(&head, KWNM_XRA_OBJ, D, &global.par.srcra, CARD_COMM_XRA_OBJ);
      AddCard(&head, KWNM_XDEC_OBJ, D, &global.par.srcdec, CARD_COMM_XDEC_OBJ);
      AddCard(&head, KWNM_XRA_PNT, D, &global.par.ranom, CARD_COMM_XRA_PNT);
      AddCard(&head, KWNM_XDEC_PNT, D, &global.par.decnom, CARD_COMM_XDEC_PNT);

    }

  if(global.par.usesrcdet && global.pd)
    {

      if(ExistsKeyWord(&head, KWNM_XRA_PNT,NULL))
	DeleteCard(&head, KWNM_XRA_PNT);
      if(ExistsKeyWord(&head, KWNM_XDEC_PNT,NULL))
	DeleteCard(&head, KWNM_XDEC_PNT);
      
    }
  else if(!global.pd)
    {
      AddCard(&head, KWNM_XRA_PNT, D, &global.par.ranom, CARD_COMM_XRA_PNT);
      AddCard(&head, KWNM_XDEC_PNT, D, &global.par.decnom, CARD_COMM_XDEC_PNT);
    }
  /* Amp */
  if ((indxcol.Amp=ColNameMatch(CLNM_Amp, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timetag_end;
    }

  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: derived exposure time. ", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "Used following file(s):");
      AddHistory(&head, global.strhist);
      if(!global.par.usesrcdet || (global.par.usesrcdet && !global.pd))
	{
	  
	  sprintf(global.strhist, "%s teldef CALDB file", global.par.teldef);
	  AddHistory(&head, global.strhist);
	}

      if(!global.pd)
	{
	  sprintf(global.strhist, "%s trailer file", global.par.trfile);
	  AddHistory(&head, global.strhist);
	  if(global.rawxcorr)
	    {
	      sprintf(global.strhist, "%s WT offset column  file", global.par.colfile);
	      AddHistory(&head, global.strhist);
	    }
	}
      sprintf(global.strhist, "%s housekeeping file", global.par.hdfile);
      AddHistory(&head, global.strhist);
    }

 
  if(global.xrtmode == XRTMODE_WT)
    {
      if(global.par.aberration)
	{
	  logical=TRUE;
	  AddCard(&head, KWNM_ABERRAT, L, &logical, CARD_COMM_ABERRAT);
	  AddCard(&head, KWNM_FOLLOWSU, L,&logical, CARD_COMM_FOLLOWSU);
	}
      else
	{
	  logical=FALSE;
	  AddCard(&head, KWNM_ABERRAT, L, &logical, CARD_COMM_ABERRAT);
	  AddCard(&head, KWNM_FOLLOWSU, L,&logical, CARD_COMM_FOLLOWSU);
	}
    }
  
  EndBintableHeader(&head, &intable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &intable);
  
  FromRow = 1;
  ReadRows=intable.nBlockRows;
  OutRows = 0;

  countNullPixel = 0;
  if(!global.trnrows && global.xrtmode == XRTMODE_WT)
    {
      headas_chat(NORMAL, "%s: Info: '%s' input parameter set to '%s'\n",global.taskname, PAR_TRFILE, global.par.trfile);
      headas_chat(NORMAL, "%s: Info: use -1 default value to correct WT RAWX\n",global.taskname);
    }

  /* Read input bintable and compute photons time-tag */
  while(ReadBintable(evunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      numRows+=ReadRows;
      for(n=0; n<ReadRows; ++n)
	{
	  if (first_row)
	    {
	      time_old= time = DVEC(intable, n, indxcol.TIME_RO);
	      ccdframe_old = ccdframe_new = VVEC(intable, n, indxcol.CCDFrame);
	    }
	  else
	    {
	      time_old=time;
	      time = DVEC(intable, n, indxcol.TIME_RO);
	      ccdframe_old = ccdframe_new;
	      ccdframe_new = VVEC(intable, n, indxcol.CCDFrame);
	    }
	 
	  if (intable.TagVec[indxcol.Amp] == B)
	    amp=BVEC(intable, n,indxcol.Amp);
	  else
	    amp=IVEC(intable, n,indxcol.Amp);
	  if (amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	      goto timetag_end;
	    }
	  /* Windowed Timing Mode */
	  if(global.xrtmode == XRTMODE_WT)
	    { 
	      if (first_row || (ccdframe_new != ccdframe_old) || !(time_old >= time-sens && time_old <= time+sens))
		{
		  first_row=0;
		  global.newquat=1;

		  


		  offset_found=0;
		  for (; iii < global.count ; iii++)
		    {
		      if(ccdframe_new == global.header[iii].ccdframe &&  
			 (global.header[iii].hktime >= time-sens && global.header[iii].hktime <= time+sens))
			{
			  time_row=global.header[iii].delta;
			  headas_chat(CHATTY, "%s: Info: Used %5.7f read row time for %d ccdframe.\n",global.taskname, time_row, ccdframe_new);
			  if(!global.trnrows || !global.rawxcorr)
			    {
			      
			      offset_found = 1;
			      rawx_offset= -1;
			      
			    }
			  else
			    {
			      
			      for (jj = 0; jj < global.trnrows; jj++)
				{
				  if(global.header[iii].hktime >= global.trailer[jj].fst-sens && global.header[iii].hktime <= global.trailer[jj].fet+sens)
				    {
				      wmcolnum=global.trailer[jj].cols;
				      wm1stcol=global.trailer[jj].firstcol;

				      if((wmcolnum < WT_NCOL_MIN) || (wmcolnum > WT_NCOL_MAX))
					{
					  headas_chat(NORMAL, "%s: Warning: For %d ccdframe the %s value in trfile is out of range\n", global.taskname, ccdframe_new, CLNM_WMCOLNUM);
					  headas_chat(NORMAL, "%s: Warning: using the default value: %d.\n", global.taskname, DEFAULT_NCOL);
					  wmcolnum=DEFAULT_NCOL;
					}
				      
				      if((wm1stcol < WM1STCOL_MIN) || (wm1stcol > WM1STCOL_MAX))
					{
					  headas_chat(NORMAL, "%s: Warning: For %d ccdframe the %s value in trfile is out of range\n", global.taskname, ccdframe_new, CLNM_WM1STCOL);
					  headas_chat(NORMAL, "%s: Warning: using the default value: %d.\n", global.taskname, DEFAULT_WM1STCOL);
					  wm1stcol=DEFAULT_WM1STCOL;
					}

				      for (kk=0; kk < global.colrows ; kk++)
					{
					  if(global.header[iii].amp == global.wtcol[kk].amp && 
					     global.header[iii].wave == global.wtcol[kk].wave &&
					     wm1stcol == global.wtcol[kk].wm1stcol &&
					     wmcolnum == global.wtcol[kk].wmcolnum )
					    {

					      global.wtcoloffset= global.wtcol[kk].offset;
					      rawx_offset       = global.wtcol[kk].offset;
					      offset_found = 1;
					    }
					  
					}
				      
				    }
				}
			    }

			  if (offset_found)
			    goto row_found;
			  else
			    {
			      headas_chat(NORMAL, "%s: Error: Unable to get rawx offset.\n", global.taskname);
			      goto timetag_end;
			    }
			  
			}
		    }
		  headas_chat(NORMAL, "%s: Error: Unable to get read row time.\n", global.taskname);
		  goto timetag_end;
		}
	      
	    row_found:
	      if(global.rawxcorr)
		{
		  IVEC(intable,n,indxcol.RAWXTL) = IVEC(intable,n,indxcol.RAWX);
		  IVEC(intable,n,indxcol.RAWX) = IVEC(intable,n,indxcol.RAWX) + rawx_offset;
		  rawx_totoffset += rawx_offset;
		}

	      rawx=IVEC(intable,n,indxcol.RAWX);
	      rawy=IVEC(intable,n,indxcol.RAWY);
	      

	      if(WTComputeCoord(rawx, rawy, src_x, src_y, amp, time, teldef, attfile, &coordset))
		{
		  headas_chat(MUTE, "%s: Error: Unable to calculate DET and SKY coordinates.\n", global.taskname);
		  goto timetag_end;
		}
	      /*
	      gettimeofday(&tv, &tz);
	      */
	      if(!finite(coordset.detx) || !finite(coordset.dety) ||
		 coordset.detx > DETX_MAX || coordset.detx < DETX_MIN ||
		 coordset.dety > DETY_MAX || coordset.dety < DETY_MIN )
		{
		  IVEC(intable,n,indxcol.DETX) = KWVL_DETNULL ;
		  IVEC(intable,n,indxcol.DETY) = KWVL_DETNULL;
		}
	      else
		{
		  IVEC(intable,n,indxcol.DETX) = (int)(coordset.detx + 0.5) ;
		  IVEC(intable,n,indxcol.DETY) = (int)(coordset.dety + 0.5) ;
		}

	      if(!finite(coordset.x) || !finite(coordset.y)||
		 coordset.x > X_MAX || coordset.x < X_MIN || 
		 coordset.y > Y_MAX || coordset.y < Y_MIN)
		{
		  
		  IVEC(intable,n,indxcol.X) = KWVL_SKYNULL;
		  IVEC(intable,n,indxcol.Y) = KWVL_SKYNULL;
		}
	      else
		{
		  IVEC(intable,n,indxcol.X) = (int)(coordset.x + 0.5);
		  IVEC(intable,n,indxcol.Y) = (int)(coordset.y + 0.5);
		}

	      	      
	      if(coordset.dety > DETY_MAX || coordset.dety < DETY_MIN)
		{
		  headas_chat(CHATTY, "%s: Warning: source position is out of range.\n", global.taskname);
		  DVEC(intable, n, indxcol.TIME) = -1.;
		  countNullPixel++;
		}
	      else
		{
		  WTTimeTag(time, rawy, (int)(coordset.dety +0.5), time_row, &time_tag);
		  DVEC(intable, n, indxcol.TIME) = time_tag - time_row;
		  
		}
	    }
	
	  else
	    { 
	      if (first_row || ccdframe_new != ccdframe_old|| !(time_old >= time-sens && time_old <= time+sens))
		{

		  global.newquat=1;
		  first_row=0;
		  for (; iii < global.count ; iii++)
		    {
		      if(ccdframe_new == global.header[iii].ccdframe &&  
			 (global.header[iii].hktime >= time-sens && global.header[iii].hktime <= time+sens))
			{
			  time_pix=global.header[iii].delta;
			  headas_chat(CHATTY, "%s: Info: Used %5.7f read pixel time for %d ccdframe.\n",global.taskname, time_pix, ccdframe_new);
			  goto pix_found;
			}
		    }
		  
		  headas_chat(NORMAL, "%s: Error: Unable to get read pixel time.\n", global.taskname);
		  goto timetag_end;
		}
	      
	    pix_found:  
		
	      /*printf(" TIMEPIX == %5.10f\n", time_pix);*/
	      if(global.newquat)
		{
		  if(!global.par.usesrcdet)
		    {
		      
		      if(PDComputeCoord(src_x, src_y, amp, time, teldef, attfile, &coordset))
			{
			  headas_chat(MUTE, "%s: Error: Unable to calculate DET coordinates.\n", global.taskname);
			  goto timetag_end;
			}


		      if(!finite(coordset.detx) || !finite(coordset.dety)|| coordset.detx > DETX_MAX || coordset.detx < DETX_MIN ||
			 coordset.dety > DETY_MAX || coordset.dety < DETY_MIN )
			{
		      
			  detx = KWVL_DETNULL;
			  dety = KWVL_DETNULL;
			  countNullPixel++;
			  
			}
		      else
			{
			  detx=(int)(coordset.detx + 0.5);
			  dety=(int)(coordset.dety + 0.5);
			}
		    }
		  else
		    {
		      detx=global.par.srcdetx;
		      dety=global.par.srcdety;
		      
		    }
		}
	      
	      offset=JVEC(intable, n, indxcol.OFFSET);


	      IVEC(intable, n, indxcol.DETX) = detx;
	      IVEC(intable, n, indxcol.DETY) = dety;
	      /*printf("DETX == %d DETY == %d \t",detx, dety);*/ 
	      if(detx == KWVL_DETNULL || dety == KWVL_DETNULL )
		{
		  headas_chat(CHATTY, "%s: Warning: source position is out of range.\n", global.taskname);
		  DVEC(intable, n, indxcol.TIME) = -1.; 
		}
	      else
		{
		
		  PDTimeTag(amp, time, offset, detx, dety, time_pix, &time_tag);
		  DVEC(intable, n, indxcol.TIME)=time_tag;
		
		}
	   
	    }

	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &intable, OutRows, FALSE);
	      OutRows=0;
	    }
	  
	}
      
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
 
  if(!numRows)
    {
      headas_chat(NORMAL, "%s: Error: The %s file is empty.\n", global.taskname, global.par.infile);
      goto timetag_end;
    }

  percent = (float)(countNullPixel/numRows)*100;
  if (percent>0 && percent < 50) {
    headas_chat(NORMAL,"%s: Info: Found %d of %d pixels out of CCD \n",global.taskname,countNullPixel,numRows);
    headas_chat(NORMAL,"%s: Info: %.2f %% pixels will be rejected \n",global.taskname,percent);
  } else if (percent != 0)  {
    headas_chat(NORMAL,"%s: Warning: Found %d of %d  pixels out of CCD\n",global.taskname,countNullPixel,numRows);
    headas_chat(NORMAL,"%s: Warning: %.2f %% pixels will be rejected \n",global.taskname,percent);
  }


  WriteFastBintable(ounit, &intable, OutRows, TRUE);
  ReleaseBintable(&head, &intable);
  
  if(rawx_totoffset == 0 && global.rawxcorr)
    {
      if(fits_delete_col(ounit,indxcol.RAWXTL, &status))
	{
	  
	  headas_chat(CHATTY, "%s: Error: Unable to delete %s column\n", global.taskname, CLNM_RAWXTL);
	  headas_chat(CHATTY, "%s: Error: of the %s file.\n", global.taskname, global.par.outfile);
	  goto timetag_end;
	}
    }
  
  if(countNullPixel)
    {
      /*
	gettimeofday(&tv, &tz);
	printf(">>>>>>>>>> 8 %ld setdouble start >>>>>>>>\n", tv.tv_sec);
      */

      if(SetDoubleColumnToNull(ounit, numRows, (indxcol.TIME+1)))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to set to NULL %s column\n", global.taskname, CLNM_TIME);
	  headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.outfile);

	  goto timetag_end;
	}
      /*  
	  gettimeofday(&tv, &tz);
	  printf(">>>>>>>>>> 8 %ld setdouble end >>>>>>>>\n", tv.tv_sec);
      */
    }
  return OK;
  
 timetag_end:
  if (head.first)
    ReleaseBintable(&head, &intable);
  
  return NOT_OK;
  
} /* Timetag */

/*
 *	xrttimetag
 *
 *
 *	DESCRIPTION:
 *        
 *                Routine to assign grades to faint mode data
 * 		  Based on the CLASSIFY program
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolname(char *);
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *
 *             void xrttimetag_getpar(void);
 *             void xrttimetag_info(void); 
 * 	       void xrttimetag_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrttimetag()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTTIMETAG_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /* Get parameter values */ 
  if ( xrttimetag_getpar() == OK ) {
    
    if ( xrttimetag_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto timetag_end;
    }
    else if(global.warning)
      {
	headas_chat(MUTE,"---------------------------------------------------------------------\n");
	headas_chat(MUTE, "%s: Exit with Warning.\n", global.taskname);
	headas_chat(MUTE,"---------------------------------------------------------------------\n");
	if (FileExists(global.tmpfile))
	  remove (global.tmpfile);
      }
    

  }
  else
    goto timetag_end;
  return OK;

 timetag_end:
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;

} /* xrttimetag */
/*
 *	xrttimetag_info: 
 *
 *
 *	DESCRIPTION:
 *         Display information about input parameter.
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrttimetag_info(void)
{


  /* Aggiornare e far stampare RA e Dec solamente se usehkkey=no */
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                  :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                 :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input Housekeeping Header Packets :'%s'\n",global.par.hdfile);
  if(!global.pd)
    {
      headas_chat(NORMAL,"Name of the input Housekeeping Trailer Packets:'%s'\n",global.par.trfile);
      headas_chat(NORMAL,"Name of the WT offset file                    :'%s'\n",global.par.colfile);
    }
  if(!global.par.usesrcdet || (global.par.usesrcdet && !global.pd))
    {
      headas_chat(NORMAL,"Name of the input Attitude file               :'%s'\n",global.par.attfile);
      headas_chat(NORMAL,"Name of the input teldef file                 :'%s'\n",global.par.teldef);
    }

  if(global.par.usehkkey)
    headas_chat(NORMAL,"Use keywords value in HK Header Packets file? :yes\n");
  else
    headas_chat(NORMAL,"Use keywords value in HK Header Packets file? :no\n");
    
  if(global.par.usesrcdet)
    {  
      headas_chat(NORMAL,"Use detector coordinates?                     :yes\n");
      if(!global.par.usehkkey)
	{
	  headas_chat(NORMAL,"Source DETX                                   :'%d'\n",global.par.srcdetx);
	  headas_chat(NORMAL,"Source DETY                                   :'%d'\n",global.par.srcdety);
	
	  if(!global.pd)
	    {
	      headas_chat(NORMAL,"Nominal RA                                    :'%f'\n",global.par.ranom);
	      headas_chat(NORMAL,"Nominal Dec                                   :'%f'\n",global.par.decnom);
	    }
	}
    }
  else
    {
      headas_chat(NORMAL,"Use detector coordinates?                     :no\n");
      if(!global.par.usehkkey)
	{
	  headas_chat(NORMAL,"Source RA                                     :'%f'\n",global.par.srcra);
	  headas_chat(NORMAL,"Source Dec                                    :'%f'\n",global.par.srcdec);
	
	  headas_chat(NORMAL,"Nominal RA                                    :'%f'\n",global.par.ranom);
	  headas_chat(NORMAL,"Nominal Dec                                   :'%f'\n",global.par.decnom);
	}
    }

  if(global.pd)
    {
      headas_chat(NORMAL,"Number of pixels to delete from first PD frame:'%d'\n",global.par.npixels);
      headas_chat(NORMAL,"Events percentage to consider a frame piled up:'%f'\n",global.par.percent);
    }
  if (global.par.aberration)
    headas_chat(NORMAL,"Should aberration be included in aspecting?   : yes\n");
  else
    headas_chat(NORMAL,"Should aberration be included in aspecting?   : no\n");
  if (global.par.attinterpol)
    headas_chat(NORMAL,"Interpolate Attitude values?                  : yes\n");
  else
    headas_chat(NORMAL,"Interpolate Attitude values?                  : no\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file?        : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file?        : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output files?                       : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output files?                       : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrttimetag_info */

int WTComputeCoord(int rawx, int rawy, double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord)
{

  double     db_detx, db_dety_tmp, db_x, db_y, src_dety=0, db_detx_tmp;
  double     mjd;
  QUAT*      q;
  XFORM2D*   sky2det;
  
  
  sky2det = allocateXform2d();
  q = allocateQuat();

  convertRawToDetectorUsingTeldef(teldef, &db_detx, &db_dety_tmp,
					      amp, rawx , rawy, 
					      0.0, 0.0);

  /* Calculate DETX */
  coord->detx = db_detx;

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Info: The times in %s file\n", global.taskname, global.par.infile);
      headas_chat(NORMAL,"%s: Info: are not included in attitude range time.\n", global.taskname);
      coord->x = 0.;
      coord->y = 0.;
      coord->dety = KWVL_DETNULL; 
      headas_chat(NORMAL,"%s: Info: Set X = %d, Y = %d and DETY = %d\n",global.taskname, coord->x,coord->y, coord->dety);
      
      return OK;
	  
    } 

  findQuatInAttFile(attfile,q,time);
  if(global.newquat)
    {
      
      /***************************************************
       * the current time is covered by the attitude file *
       ***************************************************/
      if(global.par.aberration)
	{
	  mjd=global.newgti.mjdref + time/86400.;
	  
	  coord->v=earth_velocity(coord->vhat,earth_longitude(mjd));
	}
      else
	{
	  coord->v=0.;
	  coord->vhat[0]=0.;
	  coord->vhat[1]=0.;
	  coord->vhat[2]=0.;
	}
    
      
      convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				      db_detx, src_dety, 
				      q, coord->v, coord->vhat);
      
      invertXform2d(sky2det, teldef->det2sky);


      if(!global.par.usesrcdet)	 
	{
	  applyXform2dToContinuousCoords(sky2det, &db_detx_tmp, &src_dety, src_x, src_y);
	  coord->dety=src_dety;
	}
      else
	{
	  coord->dety=global.par.srcdety;
	  src_dety=global.par.srcdety;
	}

      convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				      db_detx, src_dety, 
				      q, coord->v, coord->vhat);
      
      
      global.newquat=0;
  
    }
  else
    {
      repeatDetectorToSkyTeldefConversion(teldef,  &db_x, &db_y,
	                        	  coord->detx, coord->dety );
      
    }

  coord->x=db_x;
  coord->y=db_y;

  return OK;
  
} /* WTComputeCoord */

int PDComputeCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord)
{

  
  double     db_dety_tmp=1., db_x, db_y, src_dety=0., db_detx_tmp=1., src_detx=0.;
  double     mjd;
  QUAT*      q;
  XFORM2D*   sky2det;
  
  
  sky2det = allocateXform2d();
  q = allocateQuat();

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Info: The times in %s file\n", global.taskname, global.par.infile);
      headas_chat(NORMAL,"%s: Info: are not included in attitude range time.\n", global.taskname);
      coord->detx = KWVL_DETNULL; 
      coord->dety = KWVL_DETNULL; 
      headas_chat(NORMAL,"%s: Info: Set DETX = %d and DETY = %d\n",global.taskname, coord->detx,coord->dety);		 
      return OK;
	    	      
    } 

     
  /***************************************************
   * the current time is covered by the attitude file *
   ***************************************************/
  
  findQuatInAttFile(attfile,q,time);
 

  if(global.par.aberration)
    {
      mjd=global.newgti.mjdref + time/86400.;

      coord->v=earth_velocity(coord->vhat,earth_longitude(mjd));
    }
  else
    {
      coord->v=0.;
      coord->vhat[0]=0.;
      coord->vhat[1]=0.;
      coord->vhat[2]=0.;
    }

  convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				  db_detx_tmp, db_dety_tmp, 
				  q, coord->v, coord->vhat);
  
  invertXform2d(sky2det, teldef->det2sky);
  applyXform2dToContinuousCoords(sky2det, &src_detx, &src_dety, src_x, src_y);
  
  coord->detx=src_detx;
  coord->dety=src_dety;
  
  global.newquat=0;

 
  return OK;

} /* PDComputeCoord */


int xrttimetag_checkinput(void)
{
  int            overwrite=0;
  char           stem[10], ext[MAXEXT_LEN];
  pid_t          tmp;

  
  /* Check if output file exists */
  if (strcasecmp(global.par.outfile, DF_NONE))
    {
      if(FileExists(global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	      
	      if(remove (global.par.outfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
		  goto check_end;
		}  
	    }
	}
    }
  else
    {/* Overwrite input file */
      
      GetFilenameExtension(global.par.infile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	{
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	  goto check_end;
	}
      else
	overwrite=1;
	 
    }
    
  /* Derive temporary event filename */
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  if(overwrite)
    {
      DeriveFileName(global.par.infile, global.tmpfile, stem);
      strcpy(global.par.outfile, global.par.infile);
    }
  else
    DeriveFileName(global.par.outfile, global.tmpfile, stem);


  if(FileExists(global.tmpfile))
    {
      if(remove (global.tmpfile) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpfile);
	  goto check_end;
	}  
    }
  return OK;
 check_end:
  return NOT_OK;
}




/*
 *	GetGtiValues
 *
 *
 *	DESCRIPTION:
 *           Routine to calculate GTI.
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int strcmp(const char *s1, const char *s2);
 *             int headas_chat(int ,char *, ...);
 *             void  GetGMTDateTime(char *);
 *             int sprintf(char *str, const char *format, ...);
 *
 *             void AddHistory(FitsHeader_t *header, const char *Comment);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 15/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int GetGtiValues(struct gti_struct *gti, unsigned XRTmode, char *obs_mode)
{
  int                amp=0, status=OK, tot_offset=0, iii;
  int                sizex, sizey, detx=0, dety=0, trcols=0;
  int                ccdframe_hk=0, first_row = 1, xrtmode_hk=0, ccdframe_old;
  char               hk_obs_mode[15];
  BOOL               biason_hk=0, tr_found=1, first_gti=1;
  double             fet=0.0, fst=0.0, time_row=0.0, time_pix=0.0;
  double             roll, pixsize, src_x, src_y, pdbiaslvl=0.0;
  double             evtlost_hk=0.0, pileup_hk=0., sens = 0.0000001, sens_2 = 0.1;
  double             nom_expo=0.0, start_gti=0.0, stop_gti=0.0;
  double             start, stop;
  Coord_t            coordset;
  HKCol_t            hkcol;
  unsigned           FromRow, ReadRows, n, nCols,ActCols[17];
  unsigned           last_mode = 0, settled=0, in10arcm=0, InSafeM=0;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TELDEF* teldef;
  ATTFILE* attfile;


  if(!global.par.usesrcdet)
    {  
      /* Open Attitude file */
      attfile = openAttFile(global.par.attfile);


      if(global.par.attinterpol)
	setAttFileInterpolation(attfile, 1);
      else
	setAttFileInterpolation(attfile, 0);

      if (global.par.ranom > RA_MAX || global.par.ranom < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal RA value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
	  goto GetGtiValues_end;
	}

  
      if (global.par.decnom > DEC_MAX || global.par.decnom < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal DEC value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto GetGtiValues_end;
	}
  
      if (global.par.srcra > RA_MAX || global.par.srcra < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source RA value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
	  goto GetGtiValues_end;
	}
  
         
      if (global.par.srcdec > DEC_MAX || global.par.srcdec < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source DEC value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto GetGtiValues_end;
	}
  
      /* Open TELDEF file */
      teldef = readTelDef(global.par.teldef);

      setSkyCoordCenterInTeldef(teldef, global.par.ranom, global.par.decnom);
  
  
      /* Compute source X/Y */
      sizex=SIZE_X;
      sizey=SIZE_Y;
      roll=270.;
  
      pixsize=PIX_SIZE_DEG;
      
      if(RADec2XY(global.par.srcra, global.par.srcdec, sizex, sizey, global.par.ranom, global.par.decnom, roll, pixsize, &src_x, &src_y))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to calculate X and Y position from the source and pointing coordinates.\n",global.taskname);
	  goto GetGtiValues_end;
	} 

       
    }
    
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }
  
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile); 

      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      goto GetGtiValues_end;
	 
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
 
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
      
    }


  /* Get needed columns number */
  /*  hk CCDFrame */
  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

  /* Frame End Time */
  if ((hkcol.FETS=ColNameMatch(CLNM_FETS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }
  
  if ((hkcol.FETSub=ColNameMatch(CLNM_FETSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

 if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

 if ((hkcol.PixGtULD=ColNameMatch(CLNM_PixGtULD, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixGtULD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }
  
 if ((hkcol.EVTLOST=ColNameMatch(CLNM_EVTLOST, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EVTLOST);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

 if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

 if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto GetGtiValues_end;
    }

 if ((hkcol.Amp=ColNameMatch(CLNM_Amp, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

 if ((hkcol.nPixels=ColNameMatch(CLNM_nPixels, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_nPixels);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

 if ((hkcol.PDBIASLVL=ColNameMatch(CLNM_PDBIASLVL, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PDBIASLVL);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }
 if ((hkcol.CCDEXPOS=ColNameMatch(CLNM_CCDEXPOS, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDEXPOS);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

 if ((hkcol.CCDEXPSB=ColNameMatch(CLNM_CCDEXPSB, &hktable)) == -1)
   {
     headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDEXPSB);
     headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
     goto GetGtiValues_end;
   }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  first_row=1;
  nCols=16;
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.FSTS;
  ActCols[2]=hkcol.FSTSub;
  ActCols[3]=hkcol.FETS;
  ActCols[4]=hkcol.FETSub;
  ActCols[5]=hkcol.XRTMode;
  ActCols[6]=hkcol.PixGtULD;
  ActCols[7]=hkcol.EVTLOST;
  ActCols[8]=hkcol.Settled;
  ActCols[9]=hkcol.In10Arcm;
  ActCols[10]=hkcol.Amp;
  ActCols[11]=hkcol.nPixels;
  ActCols[12]=hkcol.PDBIASLVL;
  ActCols[13]=hkcol.CCDEXPOS;
  ActCols[14]=hkcol.CCDEXPSB;
  ActCols[15]=hkcol.InSafeM;

  /* Read input bintable and compute ranges time-tag */
  while (ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	
	  if(first_row)
	    {
	      last_mode = -1;
	      first_row=0;
	      ccdframe_old=-1;
	    }
	  else 
	    {
	      last_mode = xrtmode_hk;
	      ccdframe_old=ccdframe_hk;
	    }
	    	  
	  xrtmode_hk=BVEC(hktable, n, hkcol.XRTMode);
	  ccdframe_hk=VVEC(hktable, n, hkcol.CCDFrame);

	  /* check XRT Mode */
	  if ( xrtmode_hk != XRTmode ) 
	    {
	      headas_chat(CHATTY,"%s: Info: Frame %d with XRTmode %d (!= %d) rejected from GTIs generation\n", 
			  global.taskname,ccdframe_hk,xrtmode_hk, XRTmode);
	      continue;
	    }
	  
	  
	  /* check the amplifier number */
	  if (hktable.TagVec[hkcol.Amp] == B)
	    amp=BVEC(hktable, n,hkcol.Amp);
	  else
	    amp=IVEC(hktable, n,hkcol.Amp);
	  if (amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Warning: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(NORMAL,"%s: Warning: The Frame %d will be rejected from GTIs generation\n",global.taskname,ccdframe_hk);
	      continue;
	    }

	  settled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  in10arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);
	  InSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);

	  if((!settled && !in10arcm) || InSafeM)
	    {
	      strcpy(hk_obs_mode, KWVL_OBS_MODE_S);
	    }
	  else if(!settled && in10arcm)
	    {
	      strcpy(hk_obs_mode, KWVL_OBS_MODE_ST);
	    }
	  else if(settled && in10arcm)
	    {
	      strcpy(hk_obs_mode, KWVL_OBS_MODE_PG);
	    }
	  else
	    {
	      strcpy(hk_obs_mode, KWVL_OBS_MODE_PG);
	      headas_chat(CHATTY, "%s: Warning: Flag 'settled' set but not 'in10arcm' for frame %d.\n",global.taskname,ccdframe_hk);
	    }

	  if(strcasecmp(hk_obs_mode, obs_mode)) 
	    {
	      headas_chat(CHATTY,"%s: Info: Frame %d in %s rejected from GTIs generation\n", global.taskname, ccdframe_hk, hk_obs_mode);
	      continue;
	    }

	 
	  evtlost_hk=DVEC(hktable, n, hkcol.EVTLOST);
	  
	  fst=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	  fet=(VVEC(hktable, n, hkcol.FETS) + ((20.*UVEC(hktable, n, hkcol.FETSub))/1000000.));
	  nom_expo=(JVEC(hktable, n, hkcol.CCDEXPOS) + ((20.*UVEC(hktable, n, hkcol.CCDEXPSB))/1000000.));
	  
	  /* check if data are corrupted */
	  if ( fabs(evtlost_hk) > sens )
	    {
	      headas_chat(CHATTY,"%s: Info: Frame %d with %f %% corrupted events rejected from GTIs generation\n", 
			  global.taskname,ccdframe_hk,evtlost_hk);
	      continue;
	    }

	  /* Photodiode */
	  if ( (xrtmode_hk == XRTMODE_PU || xrtmode_hk == XRTMODE_LR ))
	    {
	 
	      if (xrtmode_hk == XRTMODE_PU)
		tot_offset=JVEC(hktable, n, hkcol.nPixels);
	      else
		{
		  CalculateNPixels(fst, fet, &tot_offset);
		}

	      pileup_hk=((JVEC(hktable, n, hkcol.PixGtULD)*100.)/tot_offset);
	      /* check piled-up frames */
	      if ( pileup_hk > global.par.percent) 
		{
		  headas_chat(CHATTY,"%s: Info: PD Frame %d with %f %% events over ULD rejected from GTIs generation\n", global.taskname,pileup_hk);
		  continue;
		}

	      
	      pdbiaslvl=DVEC(hktable, n, hkcol.PDBIASLVL);
	      if(pdbiaslvl > 0. + sens)
		biason_hk=1;
	      else 
		biason_hk=0;

	      if (biason_hk != global.biason)
		{
		  if(biason_hk)
		    headas_chat(CHATTY,"%s: Info: Frame %d with XRTmode %d is bias subtracted\n", global.taskname,ccdframe_hk,xrtmode_hk);
		  else
		    headas_chat(CHATTY,"%s: Info: Frame %d with XRTmode %d is not bias subtracted\n", global.taskname,ccdframe_hk,xrtmode_hk);
	      
		  headas_chat(CHATTY,"%s: Info: rejected from GTIs generation\n",global.taskname);
		  continue;
		}




	    }
		  
	  /* for settled  and in10arcmin compute  source detx and dety coordinates */
	  /* 20/12/2004 FT */
/*  	  if ( (strcmp(obs_mode,KWVL_OBS_MODE_S) && ((xrtmode_hk == XRTMODE_PU || xrtmode_hk == XRTMODE_LR ))) || */
/*  	       (!strcmp(obs_mode,KWVL_OBS_MODE_PG) && xrtmode_hk == XRTMODE_WT ) )  */

	  if (!global.par.usesrcdet) 
	    {
	      if(ComputeDetCoord(src_x, src_y, amp, fst, teldef, attfile, &coordset))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to calculate source DET coordinates.\n", global.taskname);
		  goto GetGtiValues_end;
		}

	      
	      if(coordset.detx > DETX_MAX || coordset.detx < DETX_MIN ||
		 coordset.dety > DETY_MAX || coordset.dety < DETY_MIN )
		{
		  headas_chat(NORMAL, "%s: Warning: The source is out of the CCD for the frame number %4d\n", global.taskname, ccdframe_hk); 
		  headas_chat(NORMAL, "%s: Warning: of the %s file.\n", global.taskname,global.par.hdfile); 
		  detx = KWVL_DETNULL;
		  dety = KWVL_DETNULL;
		}
	      else
		{
		  detx=(int)(coordset.detx + 0.5);
		  dety=(int)(coordset.dety + 0.5);
		}
	      

	    }
	  else
	    {
	      detx=global.par.srcdetx;
	      dety=global.par.srcdety;
	      headas_chat(CHATTY, "%s: Info: Using the detector coordinates of the center of the CCD\n", global.taskname);
	    }
	  
	  if(xrtmode_hk == XRTMODE_WT)
	    {
	      tr_found=1;
	      trcols=0;

	      	      
	      for(iii=0; iii<global.trnrows && tr_found; iii++)
		{
		  if(fst > global.trailer[iii].fst-sens && fst < global.trailer[iii].fet+sens)
		    {
		      tr_found=0;
		      trcols=global.trailer[iii].cols;
		    }
		}

	      if(tr_found != global.telemcorr)
		continue;
	      
	      if(!tr_found && global.wmcolnum != trcols )
		continue;


	      if(detx == KWVL_DETNULL || dety == KWVL_DETNULL )
		{
		  headas_chat(NORMAL, "%s: Warning: In the %s file\n", global.taskname,global.par.hdfile); 
		  headas_chat(NORMAL, "%s: Warning: WT frame %d with detector coordinates (%d,%d) out of range\n", global.taskname,
			      ccdframe_hk,detx,dety);
		  headas_chat(NORMAL, "%s: Warning: not included in GTIs generation\n", global.taskname);
		  continue;
		}
	      else
		{

		  /* compute start GTI time */
		  TimeRow(fst, fet, &time_row);
		  if ( last_mode != XRTMODE_WT)
		    {
		      WTTimeTagFirstFrame(fst, time_row,&start);
		      
		    }
		  else
		    WTTimeTag(fst, WT_RAWY_MIN,dety,time_row,&start);
		  /* compute stop GTI time */
		  WTTimeTag(fst, WT_RAWY_MAX,dety,time_row,&stop);
		 

		  if(!(((fet - fst + time_row) >= (nom_expo - sens_2)) && ((fet - fst + time_row) <= (nom_expo + sens_2))))
		    { 
		      headas_chat(NORMAL,"%s: Warning: In the %s file\n", global.taskname,global.par.hdfile); 
		      headas_chat(NORMAL,"%s: Warning: effective exposure time is %5.5f but\n", global.taskname, (fet - fst + time_row));
		      headas_chat(NORMAL,"%s: Warning: nominal exposure time is %5.5f for %d frame.\n", global.taskname, nom_expo, ccdframe_hk);
		    }

		  if(start >= (stop - sens))
		    { 
		      headas_chat(NORMAL,"%s: Warning: In the %s file\n", global.taskname,global.par.hdfile); 
		      headas_chat(NORMAL,"%s: Warning: exposure start time is greater than exposure stop time\n", global.taskname);
		      headas_chat(NORMAL,"%s: Warning: for %d frame.\n", global.taskname, ccdframe_hk);
		    }


		  if(first_gti)
		    {
		      start_gti=start;
		      stop_gti=stop;
		      first_gti=0;
		    }
		  else
		    {
		      if(ccdframe_hk == (ccdframe_old + 1) && ((start - stop_gti) <=  (MIN_EXPOTIME + sens)))
			{
			  stop_gti=stop;
			}
		      else
			{			
			  if (AddGti( gti, (start_gti-0.5*time_row - time_row), (stop_gti+0.5*time_row-time_row) )) 
			    goto GetGtiValues_end;
			  headas_chat(CHATTY, "%s: Info: Added new GTI record: START %15.6f STOP %15.6f\n", global.taskname, 
				      start_gti,stop_gti);
			  start_gti=start;
			  stop_gti=stop;
			  
			}
		    }
		}
	    }
	  else if (xrtmode_hk == XRTMODE_PU || xrtmode_hk == XRTMODE_LR)
	    {
	      
	      if(detx == KWVL_DETNULL || dety == KWVL_DETNULL )
		{
		  headas_chat(NORMAL, "%s: Warning: PD Frame %d with detector coordinates (%d,%d) out of range\n", global.taskname,
			      ccdframe_hk,detx,dety);
		  headas_chat(NORMAL, "%s: Warning: not included in GTIs generation\n", global.taskname);
		  continue;
		}
	      else
		{
		  
		  
		  headas_chat (CHATTY, "%s: Info: Photodiode source DETX calculated is: %d.\n", global.taskname, detx);
		  headas_chat (CHATTY, "%s: Info: Photodiode source DETY calculated is: %d.\n", global.taskname, dety);
		  /* compute start GTI time */
		  if(TimePix(fst, fet, tot_offset,&time_pix))
		    {
		      headas_chat(CHATTY, "%s: Error: Unable to calculate read pixel time.\n", global.taskname);
		      goto GetGtiValues_end;
		    }


		  if(!(((fet - fst + PD_NPIXS*time_pix) >= (nom_expo - sens_2)) && ((fet - fst + PD_NPIXS*time_pix ) <= (nom_expo + sens_2) )))
		    { 
		      headas_chat(NORMAL,"%s: Warning: In the %s file\n", global.taskname,global.par.hdfile); 
		      headas_chat(NORMAL,"%s: Warning: Effective exposure time is %5.5f but\n", global.taskname, (fet - fst + PD_NPIXS*time_pix));
		      headas_chat(NORMAL,"%s: Warning: nominal exposure time is %5.5f for %d frame.\n", global.taskname, nom_expo, ccdframe_hk);
		      
		     
		    }
		  

		  if ( !(last_mode == XRTMODE_PU || last_mode == XRTMODE_LR ) ) 
		    {
		      PDTimeTag(amp,fst, global.par.npixels,detx, dety, time_pix, &start);
		      headas_chat(CHATTY, "%s: Info: PD Frame %8d GTI START %15.6f offset %d\n", global.taskname, 
				  ccdframe_hk,start,global.par.npixels);
		    }
		  else
		    {
		      
		      PDTimeTag(amp,fst, FIRST_OFFSET,detx, dety,time_pix,&start);
		      headas_chat(CHATTY, "%s: Info: PD Frame %8d GTI START %15.6f offset %d\n", global.taskname, 
				  ccdframe_hk,start,FIRST_OFFSET);
		    }
		  
		  /* compute stop GTI time */
		  PDTimeTag(amp,fst, (tot_offset -1 ),detx, dety,time_pix,&stop);
		  if(start >= (stop - sens))
		    { 
		      headas_chat(NORMAL,"%s: Warning: In the %s file\n", global.taskname,global.par.hdfile); 
		      headas_chat(NORMAL,"%s: Warning: exposure start time is greater than exposure stop time\n", global.taskname);
		      headas_chat(NORMAL,"%s: Warning: for %d frame.\n", global.taskname, ccdframe_hk);
		    }
		  
		  if(first_gti)
		    {
		      start_gti=start;
		      stop_gti=stop;
		      first_gti=0;
		    }
		  else
		    {
		      if(ccdframe_hk == (ccdframe_old + 1) && ((start - stop_gti) <=  (MIN_EXPOTIME + sens)))
			{
			  stop_gti=stop;
			}
		      else
			{			
			  if (AddGti( gti, start_gti,stop_gti)) 
			    goto GetGtiValues_end;
			  headas_chat(CHATTY, "%s: Info: Added new GTI record: START %15.6f STOP %15.6f\n", global.taskname, 
				      start_gti,stop_gti);
			  start_gti=start;
			  stop_gti=stop;
			}
		    }

	  
		  
		}
	    }




	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  
      if(XRTmode == XRTMODE_WT)
	{
	  if (AddGti( gti, (start_gti-0.5*time_row - time_row), (stop_gti+0.5*time_row - time_row) )) 
	    goto GetGtiValues_end;
	}
      else
	{
	  if (AddGti( gti, start_gti, stop_gti )) 
	    goto GetGtiValues_end;
	  
	}
      headas_chat(CHATTY, "%s: Info: Added new GTI record: START %15.6f STOP %15.6f\n", global.taskname, 
		  start_gti,stop_gti);
      
      /*    ReleaseBintable(&hkhead, &hktable); */
      return OK;
  
    GetGtiValues_end:
  /*    if (hkhead.first) */
  /*      ReleaseBintable(&hkhead, &hktable); */
  
  return NOT_OK;


} /* TimetagHDFile */


int ComputeDetCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord)
{

  
  double     db_dety_tmp=1., db_x, db_y, src_dety=0., db_detx_tmp=1., src_detx=0.;
  double     mjd=0.;
  QUAT*      q;
  XFORM2D*   sky2det;
  
  
  sky2det = allocateXform2d();
  q = allocateQuat();

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Warning: CCD Frame Time: %f is not included\n", global.taskname, time);
      headas_chat(NORMAL,"%s: Warning: in the time range of the\n", global.taskname); 
      headas_chat(NORMAL,"%s: Warning: %s Attitude file.\n", global.taskname, global.par.attfile);
      coord->detx = KWVL_DETNULL; 
      coord->dety = KWVL_DETNULL; 
      headas_chat(CHATTY,"%s: Warning: Setting detx = %d and dety = %d.\n", global.taskname, coord->detx,coord->dety );	 
      return OK;
	    	      
    } 

     
  /***************************************************
   * the current time is covered by the attitude file *
   ***************************************************/
  
  findQuatInAttFile(attfile,q,time);
  
  if(global.par.aberration)
    {
      mjd=global.newgti.mjdref + time/86400.;
      
      coord->v=earth_velocity(coord->vhat,earth_longitude(mjd));
    }
  else
    {
      coord->v=0.;
      coord->vhat[0]=0.;
      coord->vhat[1]=0.;
      coord->vhat[2]=0.;
    }

  convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				  db_detx_tmp, db_dety_tmp, 
				  q, coord->v, coord->vhat);
  
  invertXform2d(sky2det, teldef->det2sky);
  applyXform2dToContinuousCoords(sky2det, &src_detx, &src_dety, src_x, src_y);
  
  coord->detx=src_detx;
  coord->dety=src_dety;
  

  
 
  return OK;
} /* ComputeDetCoord */


int AddGti( struct gti_struct *gti, double start, double stop )
{

  int  status = OK;
  char comm[32]; 

 
  if(HDgti_grow(gti, (gti->ngti+1), &status))
    {
      fits_get_errstatus(status, comm);
      headas_chat(CHATTY,"%s: Error: %s\n", global.taskname, comm); 
      headas_chat(NORMAL,"%s: Error: Unable to calculate GTIs range.\n", global.taskname);
      return NOT_OK;
    }
  gti->ngti++;
  gti->start[gti->ngti-1]=start;
  gti->stop[gti->ngti-1]=stop;

  return OK;
}


int CalcTimeRows(void)
{

  int                status=OK, hkmode=0, good_row=1;
  double             fst, fet,delta;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[11];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.count=0;
  
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }
 
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      goto CalcTimeRows_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  if(GetCoordKeyVal(&hkhead))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read necessary keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n ", global.taskname, global.par.hdfile);
  
      goto CalcTimeRows_end;
    }
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);  
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
      
    }


  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }


  /* Frame End Time */
  if ((hkcol.FETS=ColNameMatch(CLNM_FETS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.FETSub=ColNameMatch(CLNM_FETSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.WAVE=ColNameMatch(CLNM_WaveID, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WaveID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  if ((hkcol.Amp=ColNameMatch(CLNM_Amp, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimeRows_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=11; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.FSTS;
  ActCols[2]=hkcol.FSTSub;
  ActCols[3]=hkcol.FETS;
  ActCols[4]=hkcol.FETSub;
  ActCols[5]=hkcol.XRTMode;
  ActCols[6]=hkcol.Settled;
  ActCols[7]=hkcol.In10Arcm;
  ActCols[8]=hkcol.WAVE;
  ActCols[9]=hkcol.Amp;
  ActCols[10]=hkcol.InSafeM;
  
  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  good_row=1;

	  hkInSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);  
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);

	  
	  if(!strcmp(global.obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }

	  
	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
	  
	  /*printf("%u -- %u -- %d\n",hksettled,hkin10Arcm,hkmode);*/

	  if(good_row && (hkmode == global.xrtmode))
	    {
	      if(global.count)
		{
		  global.count+=1;
		  global.header = (DeltaT_t *)realloc(global.header, ((global.count)*sizeof(DeltaT_t)));
		}
	      else
		{
		  global.count+=1;
		  global.header = (DeltaT_t *)malloc(sizeof(DeltaT_t));  
		
		}

	      fst=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	      fet=(VVEC(hktable, n, hkcol.FETS) + ((20.*UVEC(hktable, n, hkcol.FETSub))/1000000.));
	      TimeRow(fst, fet, &delta);
	      
	      global.header[global.count - 1].ccdframe=VVEC(hktable, n, hkcol.CCDFrame); 
	      global.header[global.count - 1].hktime=fst;
	      global.header[global.count - 1].delta=delta;
	      global.header[global.count - 1].amp=BVEC(hktable, n, hkcol.Amp); 
	      global.header[global.count - 1].wave=BVEC(hktable, n, hkcol.WAVE); 

	      
	    }
	  
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.count)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  CalcTimeRows_end;
    }


  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 CalcTimeRows_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* CalcTimeRows */




int CalcTimePixs(void)
{

  int                status=OK,  hkmode=0, tot_offset, good_row=1;
  double             fst, fet,delta;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[11];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.count=0;

  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }
 
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      goto CalcTimePixs_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  if(GetCoordKeyVal(&hkhead))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read necessary keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n ", global.taskname, global.par.hdfile);
  
      goto CalcTimePixs_end;
    }
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
      
    }

  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  /* Frame End Time */
  if ((hkcol.FETS=ColNameMatch(CLNM_FETS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.FETSub=ColNameMatch(CLNM_FETSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.PixGtULD=ColNameMatch(CLNM_PixGtULD, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixGtULD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  if ((hkcol.nPixels=ColNameMatch(CLNM_nPixels, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_nPixels);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto CalcTimePixs_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=11; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.FSTS;
  ActCols[2]=hkcol.FSTSub;
  ActCols[3]=hkcol.FETS;
  ActCols[4]=hkcol.FETSub;
  ActCols[5]=hkcol.XRTMode;
  ActCols[6]=hkcol.Settled;
  ActCols[7]=hkcol.In10Arcm;
  ActCols[8]=hkcol.PixGtULD;
  ActCols[9]=hkcol.nPixels;
  ActCols[10]=hkcol.InSafeM;

  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  good_row=1;

	  hkInSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);

	  if(!strcmp(global.obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }

	  
	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
	  
	  if(good_row && (hkmode == global.xrtmode))
	    {
	      if(global.count)
		{
		  global.count+=1;
		  global.header = (DeltaT_t *)realloc(global.header, ((global.count)*sizeof(DeltaT_t)));
		}
	      else
		{

		  global.count+=1;
		  global.header = (DeltaT_t *)malloc(sizeof(DeltaT_t));  
		  

		}/* End memory allocation */

	      fst=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	      fet=(VVEC(hktable, n, hkcol.FETS) + ((20.*UVEC(hktable, n, hkcol.FETSub))/1000000.));
	      if(hkmode==XRTMODE_PU)
		tot_offset=JVEC(hktable, n, hkcol.nPixels);
	      else
		{
		  CalculateNPixels(fst, fet, &tot_offset);
		}

	      if(TimePix(fst, fet, tot_offset, &delta))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to calculate read pixel time.\n", global.taskname);
		  goto CalcTimePixs_end;
		}
		
	      global.header[global.count - 1].ccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.header[global.count - 1].hktime=fst;
	      global.header[global.count - 1].delta=delta;

	      
	    }
	  
	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.count)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  CalcTimePixs_end;
    }


  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 CalcTimePixs_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* CalcTimePixs */

/* GetCoordKeyVal */
int GetCoordKeyVal(FitsHeader_t	*hkhead)
{

  FitsCard_t         *card;

  if(global.slew)
    {
      
      headas_chat(NORMAL, "%s: Info: %s keyword is set to %s\n", global.taskname, KWNM_OBS_MODE,global.obsmode);
      headas_chat(NORMAL, "%s: Info: Using %d, %d default coordinates to calculate\n", global.taskname,(int)CENTER_DETX, (int)CENTER_DETY);
      headas_chat(NORMAL, "%s: Info: pixels exposure time\n", global.taskname);

      global.par.srcdetx=(int)CENTER_DETX;
      global.par.srcdety=(int)CENTER_DETY;
      global.par.usesrcdet=1;

    }
  else if(global.par.usehkkey)
    {
      /* Check if Housekeeping file is already corrected */
      if(!((ExistsKeyWord(hkhead, KWNM_XRTTIMES  , &card)) && (card->u.LVal)))
	{
	  headas_chat(NORMAL, "%s: Error: %s and %s columns\n",global.taskname, CLNM_TIME, CLNM_ENDTIME);
	  headas_chat(NORMAL, "%s: Error: in %s file\n", global.taskname, global.par.hdfile);
	  headas_chat(NORMAL, "%s: Error: are not corrected.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: But '%s' parameter is set to 'yes'.\n", global.taskname, PAR_USEHKKEY);
      
	  goto getcoord_end;
	}
      
      if(global.par.usesrcdet)
	{
	  if((ExistsKeyWord(hkhead, KWNM_XRTDETX  , &card)))
	    global.par.srcdetx=card->u.JVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XRTDETX);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }
	
	  if((ExistsKeyWord(hkhead, KWNM_XRTDETY  , &card)))
	    global.par.srcdety=card->u.JVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XRTDETY);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }
	}
      else
	{
	  
	  if((ExistsKeyWord(hkhead, KWNM_XRA_OBJ  , &card)))
	    global.par.srcra=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XRA_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }

	  if((ExistsKeyWord(hkhead, KWNM_XDEC_OBJ  , &card)))
	    global.par.srcdec=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XDEC_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }
	
	  if((ExistsKeyWord(hkhead, KWNM_XRA_PNT  , &card)))
	    global.par.ranom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XRA_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }
  
	  if((ExistsKeyWord(hkhead, KWNM_XDEC_PNT  , &card)))
	    global.par.decnom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XDEC_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto getcoord_end;
	    }

	}

      /*      if((global.par.usesrcdet && !global.pd))
	      {
	      
	      global.par.ranom=-1;
	      global.par.decnom=-1;
	      }
      */
    }

  /*  if(global.par.usesrcdet)
    headas_chat(MUTE, " USING DETX = %d -- DETY = %d\n", global.par.srcdetx, global.par.srcdety);
  else
    headas_chat(MUTE, " USING ranom = %f -- decnom = %f -- srcra = %f -- srcdec = %f \n", global.par.ranom, global.par.decnom, global.par.srcra, global.par.srcdec); 
  
  */
  return OK;

    getcoord_end:
  return NOT_OK;


}/* GetCoordKeyVal */
int ReadTrFile()
{
  int                status=OK;
  TRCol_t            trcol;
  unsigned           FromRow, ReadRows, n, nCols,ActCols[6];
  Bintable_t	     trtable;
  FitsHeader_t	     trhead;
  FitsFileUnit_t     trunit=NULL;

  
  /* Open readonly input tr file */
  if ((trunit=OpenReadFitsFile(global.par.trfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }
  else
    headas_chat(NORMAL,"%s: Info: Processing %s input file.\n", global.taskname, global.par.trfile);

  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(trunit, ANY_HDU, KWVL_EXTNAME_OBSH, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OBSH);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.trfile); 
      
      if( CloseFitsFile(trunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.trfile);
	}
      goto ReadTrFile_end;
      
    }
  
  trhead=RetrieveFitsHeader(trunit);
  GetBintableStructure(&trhead, &trtable, BINTAB_ROWS, 0, NULL);
  if(!trtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
      
    }

  global.trnrows=trtable.MaxRows;

  if( global.trnrows)
    {

      global.trailer=(TrailerT_t *)calloc(global.trnrows, sizeof(TrailerT_t));

    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }
      

  /* Get needed columns number */

  if ((trcol.FrstFST=ColNameMatch(CLNM_FrstFST, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FrstFST);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }

  /* Frame Start Time */
  if ((trcol.FrstFSTS=ColNameMatch(CLNM_FrstFSTS, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FrstFSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }


  if ((trcol.LastFST=ColNameMatch(CLNM_LastFST, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LastFST);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }


  if ((trcol.LastFSTS=ColNameMatch(CLNM_LastFSTS, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LastFSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }

  if ((trcol.WM1STCOL=ColNameMatch(CLNM_WM1STCOL, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WM1STCOL);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }

  if ((trcol.WMCOLNUM=ColNameMatch(CLNM_WMCOLNUM, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WMCOLNUM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto ReadTrFile_end;
    }

  EndBintableHeader(&trhead, &trtable);
 
  FromRow = 1;
  ReadRows=trtable.nBlockRows;

  nCols=6;
  ActCols[0]=trcol.FrstFST;
  ActCols[1]=trcol.FrstFSTS;
  ActCols[2]=trcol.LastFST;
  ActCols[3]=trcol.LastFSTS;
  ActCols[4]=trcol.WM1STCOL;
  ActCols[5]=trcol.WMCOLNUM;

  /* Read input bintable and compute ranges time-tag */
  while (ReadBintable(trunit, &trtable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
    
      for(n=0; n<ReadRows ; ++n)
	{

	  global.trailer[n].fst=(VVEC(trtable, n, trcol.FrstFST) + ((20.*UVEC(trtable, n, trcol.FrstFSTS))/1000000.));
	  global.trailer[n].fet=(VVEC(trtable, n, trcol.LastFST) + ((20.*UVEC(trtable, n, trcol.LastFSTS))/1000000.));
	  global.trailer[n].cols=UVEC(trtable, n, trcol.WMCOLNUM);	  
	  global.trailer[n].firstcol=UVEC(trtable, n, trcol.WM1STCOL);	  

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  ReleaseBintable(&trhead, &trtable);



  /*  for (n=0; n<global.trnrows; n++)
    {
      printf("START == %5.6f --- STOP == %5.6f COLS=%d\n", global.trailer[n].fst,global.trailer[n].fet,global.trailer[n].cols);
    }
  */
  return OK;


 ReadTrFile_end:
  return NOT_OK;

}
