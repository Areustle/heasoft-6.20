/*
 * 
 *	xrtevtrec.c:
 *
 *	INVOCATION:
 *
 *		xrtevtrec [parameter=value ...]
 *
 *	DESCRIPTION:
 *   
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 21/03/2003 - First version
 *        0.1.1 - FT 29/04/2003 - bugs fixed
 *        0.1.2 - BS 06/05/2003 - Set default to compute only 3x1
 *                                event recognition, while 3x3 is optional.
 *        0.1.3 - BS 14/05/2003 - Added default NULL value for PHA, PHAS, GRADE, PHA3x3, 
 *                                PHAS3x3 and GRADE3x3 columns.
 *        0.1.4 - BS 26/05/2003 - Changed NULL value for PHA, PHAS, GRADE, PHA3x3, 
 *                                PHAS3x3 and GRADE3x3 columns.
 *        0.2.0 - BS 10/06/2003 - Added routines to handle PiledUp and LowRate 
 *                                Photodiode read out modes
 *                              - Modified WTEventRec routine to compute only 3x1 event recognition 
 *                              - Added check on event threshold for windowed timing mode  
 *        0.2.1 - FT 26/06/2003 - Changed condition on local maximum. The PHA of the pixel
 *                                must be >= of adiacent pixels (not only >). Burrows request (23/06/2003)
 *        0.2.2 - BS 01/07/2003 - Added flag and routine to delete columns
 *        0.2.3 -    04/07/2003 - Added flag and routine to delete rows where PHA value is NULL 
 *                              - Added routine to insert statistic keywords
 *        0.2.4 -    25/07/2003 - Get CALDB grades file to assign grades to events
 *        0.2.5 -    16/09/2003 - Implemented routine to use CALDB grades file for Photodiode Modes
 *        0.3.0 -    22/09/2003 - Implemented new algorithm for TIMING Modes using 7x1 patterns
 *                                (XRT Meeting Frascati 17-18/09/2003)
 *        0.3.1 -    23/10/2003 - Added DS keywords 
 *        0.3.2 -    03/11/2003 - Replaced call to 'headas_parstam()' with 'HDpar_stamp()'
 *        0.3.3 -    05/11/2003 - Changed expression to select CALDB grade file.
 *        0.3.4 -    19/11/2003 - Added default values for GRADE TLMIN/TLMAX when needed 
 *        0.3.5 -    12/12/2003 - Implemented Amplifier number handling 
 *        0.3.6 -    25/03/2004 - Added 'flagneigh' parameters to flag events near bad column
 *                              - Deleted input files list handling
 *        0.3.7 -    06/05/2004 - Modified some columns comment
 *        0.3.8 -    12/05/2004 - Messages displayed revision
 *        0.3.9 -    21/05/2004 - Bug fixed in "AddStatKeywords"
 *        0.3.10-    24/06/2004 - Bug fixed on 'fits_update_key' call 
 *                                for LOGICAL keyword
 *        0.3.11-    29/07/2004 - Added check on InitBintable status
 *        0.3.12-    07/10/2004 - Added  check on BIASONBORD keyword to the
 *                                "Check if PHA is already computed" check
 *        0.3.13-    02/11/2004 - Bug fixed
 *        0.4.1 -    10/11/2004 - Added check on Amplifier number
 *        0.4.2 -    16/03/2005 - Modified to read EVTLLD column instead of
 *                                LVLLTHR keyword if 'split' and/or 'event'
 *                                parameters are negative   
 *                              - Added new input parameter 'hdfile'
 *        0.4.3 -    19/05/2005 - Bug fixed
 *        0.4.4 -    13/07/2005 - Replaced ReadBintable with ReadBintableWithNULL
 *                                to perform check on undefined value reading hdfile
 *        0.4.5 -    14/07/2005 - Exit with warning if infile is empty
 *        0.4.6 -    09/08/2005 - Create outfile if task exits with warning
 *        0.4.7 -    01/02/2006 - Bug fixed in PDEventRec routine  
 *        0.4.8 -    03/03/2006 - Go back to 0.4.6 version and fix the 
 *                                bug in FillPD7x1LastRows 
 *        0.4.9 - NS 12/03/2007 - Changed WTReadHK and PDReadHK function to accept
 *                                Settled=1 In10Arcm=0 InSafeM=1 for events with obsmode=SLEW
 *        0.5.0 -    06/07/2007 - New input parameter 'thrfile'
 *        0.5.1 -    30/09/2008 - Changed WTReadHK and PDReadHK functions to evaluate
 *                                obsmode=SLEW when InSafeM=1 
 *
 * 
 *      NOTE:
 *       
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtevtrec  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtevtrec.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTEVTREC_C
#define XRTEVTREC_VERSION      "0.5.1"
#define PRG_NAME               "xrtevtrec"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtevtrec_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtevtrec.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void xrtevtrec_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 21/03/2003 - First version
 *        0.1.1: - BS 27/05/2003 - Added photodiode mode parameters
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */

int xrtevtrec_getpar()
{

  /*
   *  Get File name Parameters
   */
  
  /* Input Timing Event List Files Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname, PAR_INFILE);
      goto Error;
    }
  
  /* Output Event List File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  /* Input CALDB grade file */
  if(PILGetFname(PAR_GRADEFILE, global.par.gradefile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRADEFILE);
      goto Error;	
    }

  /* Input Thresholds file */
  if(PILGetFname(PAR_THRFILE, global.par.thrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_THRFILE);
      goto Error;	
    }

  /* Added columns PHAS and PixsAbove? */ 
  if(PILGetBool(PAR_ADDCOL, &global.par.addcol))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get '%s' parameter.\n",global.taskname, PAR_ADDCOL);
      goto Error;
     }

  /* Delete NULL events? */ 
  if(PILGetBool(PAR_DELNULL, &global.par.delnull))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get '%s' parameter.\n",global.taskname, PAR_DELNULL);
      goto Error;
     }

  global.ret=0;
  if (IsThisEvDatamode(global.par.infile,KWVL_DATAMODE_TM, &global.ret))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read %s keyword\n", global.taskname, KWNM_DATAMODE); 
      headas_chat(NORMAL,"%s: in %s file.\n", global.taskname,global.par.infile);
      goto Error;
    }

  global.par.flagneigh=0;

  if (global.ret)
    {

      /* Flag events? */ 
      if(PILGetBool(PAR_FLAGNEIGH, &global.par.flagneigh))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to get '%s' parameter.\n",global.taskname, PAR_FLAGNEIGH);
	  goto Error;
	}
    }

  global.usethrfile=1;

  if ( !strcasecmp(global.par.thrfile,DF_NONE) )
    {
      global.usethrfile=0;
      
      /* Split Threshold Level */
      if(PILGetInt(PAR_SPLIT, &global.par.split)) 
	{
	  headas_chat(NORMAL,"%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SPLIT);
	  goto Error;	
	}
      /* Event threshold Level */
      if(PILGetInt(PAR_EVENT, &global.par.event)) 
	{
	  headas_chat(NORMAL,"%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVENT);
	  goto Error;	
	}
    }
  
  if((!global.usethrfile && (global.par.split < 0 || global.par.event < 0)) || !global.ret)
    {
      /* Input header packet Files Name */
      if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	  goto Error;
	}
    }
  
 get_history(&global.hist); 
 xrtevtrec_info();
 
 return OK;

 Error:
  return NOT_OK;
  
} /* xrtevtrec_getpar */

/*
 *	xrtevtrec_work
 *
 *
 *	DESCRIPTION:
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
 *           int strcasecmp(const char *s1, const char *s2);
 *           char * SplitFilePath(const char *FilePath, char *DirName, char *BaseName);
 *           char * StripExtension(char * filename);
 *           char *strcat(char *dest, const char *src);
 *           BOOL FileExists(const char *FileName);
 *           FitsFileUnit_t OpenWriteFitsFile(char *name);
 *           int remove(const char *pathname);
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype,
 *                               int *status ); 
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status); 
 *           int fits_update_key(fitsfile *fptr, int datatype, char *keyname, DTYPE *value,
 *                               char *comment, int *status);  
 *           int AddEventBintable(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 21/03/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtevtrec_work()
{
  int            i, status = OK, inExt, outExt, evExt;   
  long           extno=-1;
  char           expr[256];
  ThresholdInfo_t thrvalues;
  FitsHeader_t	 head;
  FitsCard_t     *card;
  FitsFileUnit_t inunit=NULL, outunit=NULL;              /* Input and Output fits file pointer */ 

  TMEMSET0( &head, FitsHeader_t );

  global.warning=0;
  if (xrtevtrec_checkinput())
    goto Error;

  /* Initialize counters */
  for(i = 0; i < TOT_TIM_GRADE; i++)
    {
      global.grade[i]=0;
      global.per_grade[i]=0;
    }

  global.tot_sat=0;
  global.per_tot_sat=0;

  global.wt=0;
  global.lr=0;
  global.pu=0;
    
  global.scount=0;

  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
    
  /* Check readout mode */
    
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

  head=RetrieveFitsHeader(inunit);
  
  /* Check if Event reconstruction  is already done */
  if(ExistsKeyWord(&head, KWNM_XRTEVREC, &card))
    {
      if(card->u.LVal)
	{
	  headas_chat(CHATTY,"%s: Error: %s keyword set\n",global.taskname,KWNM_XRTEVREC);
	  headas_chat(CHATTY,"%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL,"%s: Error: Event recostruction is already done.\n", global.taskname);
	  goto Error;
	} 
    }

  /* Retrieve DATEOBS from input event file */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.evt.dateobs=card->u.SVal;
      if(!(strchr(global.evt.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.evt.timeobs=card->u.SVal;
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

  /* Retrieve XRTVSUB of input event file */
  if((ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.evt.xrtvsub=card->u.JVal;
    }
  else
    {
      global.evt.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }

  /* Retrieve datamode of input event file */
  if(ExistsKeyWord(&head, KWNM_DATAMODE, &card))
    strcpy(global.evt.datamode,card->u.SVal);
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  /* Check readout mode of input events file */
  /* LOW RATE o PILEDUP */
  if(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_PDPU, NULL))
    global.pu=1;
  else if(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_PD, NULL))
    global.lr=1;
  /* WINDOWED TIMING */
  else if(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_TM, NULL))
    global.wt=1;
  else 
    {
      headas_chat(NORMAL,"%s: Error: This task does not process the datamode\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid datamodes  are:  %s, %s, and %s.\n", global.taskname, KWVL_DATAMODE_PDPU, KWVL_DATAMODE_PD,KWVL_DATAMODE_TM);
      
      if( CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	} 
      goto Error;
    }
  

  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n",global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname,global.par.infile); 
      goto Error;
    } 
    

  
  
  /********************************
   *     Get CALDB grades file    *
   ********************************/
  
  extno=-1;

  if ( !strcasecmp(global.par.gradefile,DF_CALDB) )
    {
      if(global.wt)
	sprintf(expr, "datamode.eq.%s", KWVL_DATAMODE_TM);
      else if(global.lr)
	sprintf(expr, "datamode.eq.%s", KWVL_DATAMODE_PD);
      else if(global.pu)
	sprintf(expr, "datamode.eq.%s", KWVL_DATAMODE_PDPU);
      
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_GRADE_DSET, global.par.gradefile, expr, &extno ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto Error;
	}
      extno++;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.gradefile);
  
  if (GetGrades(extno)) 
    {
      headas_chat(CHATTY,"%s: Error: Unable to process\n", global.taskname);
      headas_chat(CHATTY,"%s: Error: %s file.\n", global.taskname, global.par.gradefile);
      goto Error;
    }


  /********************************
   *     Get CALDB thresholds file    *
   ********************************/

  if(global.usethrfile)
    {
      extno=-1;
      
      if (!(strcasecmp (global.par.thrfile, DF_CALDB)))
	{
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_EXTNAME_XRTVSUB, global.par.thrfile,HD_EXPR, &extno))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto Error;
	    }
	  else
	    headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.thrfile);
	  extno++;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.thrfile);

      if(ReadThrFile(global.par.thrfile, extno, global.evt.xrtvsub, &thrvalues))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read event and split threshold level from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the input thrfile.\n", global.taskname);
	  goto Error;
	}
      
      if(global.wt)
	{
	  global.par.event=thrvalues.wtevent;
	  global.par.split=thrvalues.wtsplit;
	}
      else
	{
	  global.par.event=thrvalues.pdevent;
	  global.par.split=thrvalues.pdsplit;
	}

      headas_chat(NORMAL, "%s: Info: Event Threshold Level from input thresholds file = %d\n", global.taskname, global.par.event);
      headas_chat(NORMAL, "%s: Info: Split Threshold Level from input thresholds file = %d\n", global.taskname, global.par.split);
    }

   
  /* Create output file */
  if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extension before event extension  from input to output file */
  outExt=1;
  
  while(outExt<evExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
	      
      outExt++;
    }
  
    
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);
  
  /* Event recognition */
  
  if(global.wt)
    {
      if ((WTEventRec(inunit, outunit)))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to reconstruct events.\n", global.taskname);
	  goto Error;
	}
      if(global.warning)
	goto ok_end;
    }
  else if (global.lr || global.pu)
    {
      if ((PDEventRec(inunit, outunit)))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to reconstruct events.\n", global.taskname);
	  goto Error;
	}
      if(global.warning)
	goto ok_end;
    }

  if(!global.par.addcol)
    {
      if(DelCols(outunit))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to delete %s or %s columns.\n", global.taskname, CLNM_PHAS, CLNM_ABOVE);
	  goto Error;
	}
    }
  outExt++;
  
  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
      
      outExt++;
    }
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
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
      headas_chat(NORMAL, "%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ",global.taskname, global.par.infile);
      goto Error;
    }  
  if (!CloseFitsFile(outunit)) 
    headas_chat(CHATTY,"%s: Info: File '%s' successfully written.\n",global.taskname,
		global.tmpfile);
  else
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto Error;
      
    }  
  /* rename temporary file into output event file */
  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto Error;
    }
  
  headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.outfile);
  
  PrintStatistics();    
    

  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

 ok_end:

  if(global.warning && strcasecmp(global.par.infile, global.par.outfile) )
    {
      if(CopyFile(global.par.infile, global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outfile.\n", global.taskname);
	  goto Error;
	}
    }
  if(global.graderows)
    free(global.gradevalues);
  return OK;
  
 Error:

  return NOT_OK;
} /* xrtevtrec_work */
/*
 *	WTEventRec
 *
 *
 *	DESCRIPTION:
 *
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
 *             void DeleteCard(FitsHeader_t *header, const char *KeyWord);
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
 *        0.1.0: - BS 21/03/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int WTEventRec(FitsFileUnit_t evunit,FitsFileUnit_t ounit)
{
  int                start=0, status=0;
  int                blockn=0, next_amp, logical;
  int                iii=0;
  int                next_ccdframe=0, next_rawy=0, ccdframe=0, rawy=0, ccdframe_old;
  double             evtime,sens=0.0000001;
  BOOL               stop_flag=0, found=0;
  char		     KeyWord[FLEN_KEYWORD];
  unsigned           OutRows=0, FromRow, ReadRows, n=0,nCols=0, WriteRows=0;  
  Ev2Col_t           indxcol;
  Bintable_t	     intable;
  FitsCard_t         *card;
  FitsHeader_t	     head;

  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
   
  head=RetrieveFitsHeader(evunit);
  
  blockn=BINTAB_ROWS;
 
  if (blockn < 650)
    blockn=650;


  if( !global.usethrfile && (global.par.split < 0 || global.par.event < 0))
    {
      headas_chat(NORMAL, "%s: Info: '%s' and/or  %s input parameters are negative\n", global.taskname, PAR_SPLIT, PAR_EVENT);
      headas_chat(NORMAL, "%s: Info: using %s file\n", global.taskname,global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: to read event and/or split threshold values.\n",global.taskname); 
      

      if((!ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
	{ 
	  headas_chat(NORMAL,"%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE );
	  headas_chat(NORMAL,"%s: Error: in the %s file.\n", global.taskname, global.par.infile);
	  goto event_end;
	}
      else
	strcpy(KeyWord,card->u.SVal);

      if(WTReadHK(KeyWord))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read event and/or split threshold values from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the %s file.\n", global.taskname, global.par.hdfile);
	  goto event_end;
	}

    }
  
  /* Initialize bintable with number of rows = blockn */
  if(InitBintable(&head, &intable, &nCols, &indxcol, blockn))
    {
      headas_chat(CHATTY, "%s: Error: Unable to read binary table\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto event_end;
    }
  if(global.warning)
    goto ok_end;


  /* Add or update DS keywords */
  sprintf(KeyWord, "%d:%d",(int) global.grademin,(int) global.grademax); 
  
  ManageDSKeywords(&head, "GRADE", "I", KeyWord); 
  EndBintableHeader(&head, &intable); 
 
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &intable);

  FromRow = 1;

  global.totrows=global.maxrows;

  ReadRows=intable.nBlockRows;
  OutRows=0;

  ccdframe_old=-1;
  
  /* Read input bintable */
  while(ReadBintable(evunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 && ReadRows!=0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  /*  printf("nrows=%d.\n", global.totrows); */
	  stop_flag=0;
	  start=n;
	  ccdframe = VVEC(intable, n, indxcol.CCDFrame);

	  if(ccdframe != ccdframe_old && (!global.usethrfile && (global.par.split < 0 ||global.par.event < 0 )))
	    {
	      evtime=DVEC(intable, n, indxcol.TIME_RO);
	      for(;iii < global.scount; iii++)
		{
		  found=1;
		  
		  if(ccdframe==global.split_wt[iii].ccdframe && 
		     (global.split_wt[iii].hktime >= evtime-sens && global.split_wt[iii].hktime <= evtime+sens))
		    {
		      found=0;
		      
		      if(global.par.split < 0)
			{
			  global.onboardsplit=global.split_wt[iii].split;
			  headas_chat(CHATTY, "%s: Info: Used %d split threshold value for %d ccdframe.\n",global.taskname, global.onboardsplit, ccdframe);
			}
		      if(global.par.event < 0)
			{
			  global.onboardevent=global.split_wt[iii].split;
			  headas_chat(CHATTY, "%s: Info: Used %d event threshold value for %d ccdframe.\n",global.taskname, global.onboardevent, ccdframe);
			}
		      	  ccdframe_old=ccdframe;
			  goto split_found;
		    }
		}
	    }
	  else
	    {
	      found=0;
	      global.onboardevent=global.par.event;
	      global.onboardsplit=global.par.split;
	      ccdframe_old=ccdframe;
	      
	    }

	  if(found)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get split threshold value.\n", global.taskname);
	      goto event_end;
	    }
	  
	split_found:

	  while (n+1 < ReadRows && !stop_flag) /*  && (OutRows+1) < blockn) */
	    {
	      ccdframe = VVEC(intable, n, indxcol.CCDFrame);
	      next_ccdframe=VVEC(intable, (n+1), indxcol.CCDFrame);
	      
	      rawy=IVEC(intable, n, indxcol.RAWY);
	      next_rawy=IVEC(intable, (n+1), indxcol.RAWY);
	      
	      global.amp=BVEC(intable, n, indxcol.Amp);
	      if(global.amp != AMP1 && global.amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
		  goto event_end;
		}
	      
	      next_amp=BVEC(intable, (n+1), indxcol.Amp);
	      if(next_amp != AMP1 && next_amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, next_amp);
		  goto event_end;
		}
	      
	      
	      if (ccdframe == next_ccdframe && rawy == next_rawy && global.amp == next_amp)
		n++;
	      else
		stop_flag=1;
	    }

	  if (stop_flag)
	    {
	      if(WTAssignGrade(&start, n, intable, indxcol, &OutRows))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to assign grade to the events.\n", global.taskname);
		  goto event_end;
		}
	    }
	  else
	    {
	      if ((global.totrows - (n - start + 1)) == 0)
		{
		
		  
		  if(WTAssignGrade(&start, n, intable, indxcol, &OutRows))
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to assign grade to the events.\n", global.taskname);
		      goto event_end;
		    }
		 /*   OutRows = n+1; */
/*  		  global.totrows -= (n - start + 1); */
		}
	    }
	  
	}/* End for(n > ReadRows) */
      
      WriteRows=OutRows;
      if(global.par.delnull)
	{
	  
	  if(RemoveNullEvents( &intable, &WriteRows, indxcol.PHA))
	    {  
	      headas_chat(NORMAL, "%s: Error: Unable to remove pixels with PHA set to NULL.\n", global.taskname);
	      goto event_end;
	    }
	}
      
      WriteFastBintable(ounit, &intable, WriteRows, FALSE);
      
      FromRow += OutRows;
      OutRows=0;
      if(global.totrows < blockn)
	ReadRows=global.totrows;
      else
	ReadRows = blockn;
    }
  
  if (OutRows > 0 )
    {
      WriteRows=OutRows;
      if(global.par.delnull)
	{
	  
	  if(RemoveNullEvents( &intable, &WriteRows, indxcol.PHA))
	    {  
	      headas_chat(NORMAL, "%s: Error: Unable to remove pixels with PHA set to NULL.\n", global.taskname);
	      goto event_end;
	    }
	}

      WriteFastBintable(ounit, &intable, WriteRows, TRUE);
    }
  else
    WriteFastBintable(ounit, &intable, 0, TRUE);


  /*  ReleaseBintable(&head, &intable);  */
  logical=TRUE;
  if(fits_update_key(ounit, TLOGICAL, KWNM_XRTEVREC, &logical, CARD_COMM_XRTEVREC, &status))
    goto event_end;

  /* Update column comment */
  sprintf(KeyWord, "TTYPE%d", (indxcol.PHA+1));
  if (fits_modify_comment(ounit, KeyWord, CARD_COMM_PHA, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s column comment.\n", global.taskname, CLNM_PHA);
      goto event_end;
    }

  /* Add statitistic keywords into events hdu */
  if(AddStatKeywords(&ounit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add statistic keywords\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.outfile);
      goto event_end;
    }


 ok_end:
  
  if(global.scount)
    free(global.split_wt);
  return OK;
 
 event_end:
  if (head.first)
    ReleaseBintable(&head, &intable);
 
  return NOT_OK;
  
} /* WTEventRec */

/*
 *	WTAssignGrade
 *
 *
 *	DESCRIPTION:
 *        
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *            
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - 23/09/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int WTAssignGrade( int *start, int stop, Bintable_t intable, Ev2Col_t indxcol, unsigned *OutRows)
{
  int                j, q, grade=0, above=0;
  int                handle[PHAS7_MOL], pha=0, rawx_count=0;

  BOOL               found=0;
  BOOL               first_time=0;
  Ev2Info_t          tmp_buff[PHAS7_MOL];
  if (stop == *start)
    { 
      /* Event is single */
      handle[0]=JVEC(intable, *start, indxcol.PHA);
      if (handle[0] >= global.onboardevent)
	{
	  for (j=1; j < PHAS7_MOL; j++)
	    handle[j]= KWVL_PHANULL;
	  
	  if(faint7x1(handle, &pha, &grade, &above))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate PHA value.\n", global.taskname);
	      goto evrec_end;
	    }

	  /* Fill columns */
	  /* Put PHA values in PHAS column? */ 
	  if(global.par.addcol)
	    {
	      for (j=0; j < PHAS7_MOL; j++)
		IVECVEC(intable,*start,indxcol.PHAS,j)=handle[j];
	      IVEC(intable, *start, indxcol.ABOVE) = above;
	    }
	  
	  JVEC(intable, *start, indxcol.EVTPHA) = JVEC(intable, *start, indxcol.PHA);
	  JVEC(intable, *start, indxcol.PHA) = pha;
	  IVEC(intable, *start, indxcol.GRADE) = grade;
	 
	  global.grade[grade]++;
	}
      else
	SetNonRecEvent(intable, *start, indxcol);
      
      *OutRows+=1;
      global.totrows--;
      
    }
  
  else
    {
      rawx_count=0;
      /* Loop on all table rows with same rawy */ 
      while ( *start <= stop )
	{
	  FillWT7x1Buff(*start, stop, rawx_count, &first_time, intable, indxcol, tmp_buff);
	  found=0;
	  
	  if(tmp_buff[3].pha >= global.onboardevent )
	    {
	      if(!IsLocalMax(tmp_buff))
		{
		  found=1;
		  if(TMEvtRec7x1(tmp_buff, indxcol, intable, OutRows))
		    goto evrec_end;
		  
		}
	      else if(tmp_buff[0].nrow != -1)
		{
		  SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
		  global.totrows--;
		  *OutRows+=1;
		}
	    }
	  else if(tmp_buff[0].nrow != -1)
	    {
	      SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
	      global.totrows--;
	      *OutRows+=1;
	    } 
	
	  if(found)
	    {
	      rawx_count=tmp_buff[3].offset;
	      *start=(tmp_buff[3].nrow +1);
	      for(q=4; q < PHAS7_MOL; q++)
		{
		  if(tmp_buff[q].pha >= global.onboardsplit)
		    {
		      *start+=1;
		      rawx_count++;
		    }
		  else
		    {
		      rawx_count=(tmp_buff[q].offset - 3);
		      q=PHAS7_MOL;

		    }
		}
	    }
	  else
	    {
	      if(tmp_buff[0].nrow != -1)
		*start+=1;

	      rawx_count=tmp_buff[0].offset;
	      		  
		  
	    }
	
	}/* End loop on pha values */
      *start-=1;
    }
  return OK;
      
 evrec_end:
  return NOT_OK;
  
  
}/* WTAssignGrade */


/*
 *	xrtevtrec
 *
 *
 *	DESCRIPTION:
 *        
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
 *             void xrtevtrec_getpar(void);
 *             void xrtevtrec_info(void); 
 * 	       void xrtevtrec_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 20/03/2003 - First version
 *        0.1.2: -    27/05/2003 - Bug fixed on 'xrtevtrec_getpar' failure
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtevtrec()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTEVTREC_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /* Get parameter values */ 
  if ( xrtevtrec_getpar() == OK ) {
    
    if ( xrtevtrec_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto evtrec_end;
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
    goto evtrec_end;
    

  return OK;
 evtrec_end:
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;
} /* xrtevtrec */
/*
 *	xrtevtrec_info: 
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
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtevtrec_info()
{

  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file         :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file        :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input GRADES file        :'%s'\n",global.par.gradefile);
  headas_chat(NORMAL,"Name of the input Thresholds file    :'%s'\n",global.par.thrfile);
  if(!global.usethrfile)
    {
      headas_chat(NORMAL,"Event Threshold Level                :'%d'\n",global.par.event);
      headas_chat(NORMAL,"Split Threshold Level                :'%d'\n",global.par.split);
    }

  if (global.par.addcol)
    headas_chat(NORMAL,"Added 'PHAS' and 'PixsAbove colums?  : yes\n");
  else
    headas_chat(NORMAL,"Added 'PHAS' and 'PixsAbove colums?  : no\n");

  if (global.par.delnull)
    headas_chat(NORMAL,"Delete rows if PHA is NULL?          : yes\n");
  else
    headas_chat(NORMAL,"Delete rows if PHA is NULL?          : no\n");

  if (global.ret)
    {
      if (global.par.flagneigh)
	headas_chat(NORMAL,"Flag events?                         : yes\n");
      else
	headas_chat(NORMAL,"Flag events?                         : no\n");
    }
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?              : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?              : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?               : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?               : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtevtrec_info*/

/* 
 *
 * InitBintable
 *
 */

int InitBintable(FitsHeader_t *head, Bintable_t *intable, unsigned *nCols, Ev2Col_t *indxcol, int blockn)
{
  char		     KeyWord[FLEN_KEYWORD];
  JTYPE              null_pha, null_grade;

  null_pha=KWVL_PHANULL;
  null_grade=KWVL_GRADENULL;
  
 /*   GetBintableStructure(head, intable, BINTAB_ROWS*blockn, 0, NULL); */
  GetBintableStructure(head, intable, blockn, 0, NULL);

  if(!intable->MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  if ( *nCols == 0 ) {
    *nCols=intable->nColumns;
    global.maxrows=intable->MaxRows;
  } 
  
  /* Get cols index from name */
  

  /* Columns needed only for Windowed Timing Mode */  
  if (global.wt)
    {
      
      if((indxcol->RAWX=ColNameMatch(CLNM_RAWX, intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto event_end; 
	}
      
      if((indxcol->RAWY=ColNameMatch(CLNM_RAWY, intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto event_end; 
	}
      if((indxcol->STATUS=ColNameMatch(CLNM_STATUS, intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STATUS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto event_end; 
	}
    }

  /* Columns needed for all modes */
  
  if((indxcol->CCDFrame=ColNameMatch(CLNM_CCDFrame, intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end; 
    }

  if((indxcol->Amp=ColNameMatch(CLNM_Amp, intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end; 
    }
  
  if((indxcol->TIME_RO=ColNameMatch(CLNM_TIME_RO, intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if ((indxcol->EVTPHA=ColNameMatch(CLNM_EVTPHA, intable)) == -1)
    { /* if EVTPHA column does not exist add it */
      AddColumn(head, intable,CLNM_EVTPHA, CARD_COMM_EVTPHA, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PHA_MIN, PHA_MAX, KWVL_PHANULL);
      indxcol->EVTPHA=ColNameMatch(CLNM_EVTPHA, intable);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_EVTPHA);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
    }
  else
    {
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol->EVTPHA+1));
      /*if (!ExistsKeyWord(head, KeyWord, NULLNULLCARD))*/
      AddCard(head, KeyWord, J, &null_pha, CARD_COMM_TNULL);
    }  
  
  if((indxcol->PHA=ColNameMatch(CLNM_PHA, intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end; 
    }
  
  /* Add TNULL keyword */
  sprintf(KeyWord, "TNULL%d", (indxcol->PHA+1));
  /*if (!ExistsKeyWord(head, KeyWord, NULLNULLCARD))*/
    AddCard(head, KeyWord, J, &null_pha, CARD_COMM_TNULL);

  /* Check if PHAS and PixsAbove columns shall be added */

  if(global.par.addcol)
    {
      if ((indxcol->PHAS=ColNameMatch(CLNM_PHAS, intable)) == -1)
	{ /* if PHAS column does not exist add it */
	  AddColumn(head, intable, CLNM_PHAS, CARD_COMM_PHAS, "7I", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PHA_MIN, PHA_MAX, KWVL_PHANULL);
	  indxcol->PHAS=ColNameMatch(CLNM_PHAS, intable);
	    
	}
      if((indxcol->ABOVE=ColNameMatch(CLNM_ABOVE, intable)) == -1)
	{
	  AddColumn(head, intable,CLNM_ABOVE,CARD_COMM_ABOVE , "1I",TNONE);    
	  indxcol->ABOVE=ColNameMatch(CLNM_ABOVE, intable);
	}
    }
  
  if((indxcol->GRADE=ColNameMatch(CLNM_GRADE, intable)) == -1)
    {
      AddColumn(head, intable,CLNM_GRADE, CARD_COMM_GRADE, "1I",TNULL,KWVL_GRADENULL);    
      indxcol->GRADE=ColNameMatch(CLNM_GRADE, intable);    
    }
  else
    {
      sprintf(KeyWord, "TNULL%d", (indxcol->GRADE+1));
      if (!ExistsKeyWord(head, KeyWord, NULLNULLCARD))
	AddCard(head, KeyWord, J, &null_grade, CARD_COMM_TNULL);
    }

  if(global.grademin == -1)
    global.grademin=TIM_GRADE_MIN;
  
  sprintf(KeyWord, "TLMIN%d", (indxcol->GRADE+1));  

  
  AddCard(head, KeyWord, J, &global.grademin, CARD_COMM_TLMIN);
  
  if(global.grademax == -1)
    global.grademax = TIM_GRADE_MAX;
  
  sprintf(KeyWord, "TLMAX%d", (indxcol->GRADE+1));
 
  AddCard(head, KeyWord, J, &global.grademax, CARD_COMM_TLMAX);
    
  if (global.lr || global.pu)
    {
       if((indxcol->OFFSET=ColNameMatch(CLNM_OFFSET, intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto event_end; 
	}
    }
  

  return OK;
  
 event_end:
  if (head->first)
    ReleaseBintable(head, intable);
  
  return NOT_OK;
  
} /* InitBintable */

/*
 *
 * SetTMNullEvent
 *
 */

void SetTMNullEvent(Bintable_t intable, int n, Ev2Col_t indxcol)
{
  int q;
  if(global.par.addcol)
    {
      for (q=0; q < PHAS7_MOL ; q++)
	IVECVEC(intable, n,indxcol.PHAS,q) = KWVL_PHANULL;
      IVEC(intable, n, indxcol.ABOVE) = 0;
    }
  JVEC(intable, n, indxcol.EVTPHA) = JVEC(intable, n, indxcol.PHA);
  JVEC(intable, n, indxcol.PHA) = KWVL_PHANULL;
  IVEC(intable, n, indxcol.GRADE) = KWVL_GRADENULL;
 
  global.grade[NULLREC]++;
}
/*
 *
 * PrintStatistics
 *
 */

void PrintStatistics(void)
{

  int i;
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL, "%s: Info: \t\t Swift XRT GRADES\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL, "%s: Info: Total pixels                     %15d\n", global.taskname, global.maxrows);
  headas_chat(NORMAL, "%s: Info: Total saturated events           %15d%10.4f %%\n", global.taskname, global.tot_sat, global.per_tot_sat);

  for (i=0; i<(TOT_TIM_GRADE - 2); i++)
    headas_chat(NORMAL, "%s: Info: Total events with grade %2d       %15d%10.4f %%\n", global.taskname, i, global.grade[i], global.per_grade[i]);
  headas_chat(NORMAL, "%s: Info: Total NULL pixels                %15d%10.4f %%\n", global.taskname, global.grade[i], global.per_grade[i]);
  i++;
  headas_chat(NORMAL, "%s: Info: Total non reconstructed pixels   %15d%10.4f %%\n", global.taskname, global.grade[i], global.per_grade[i]);
  
  
}

/*
 * DelCols
 */
int DelCols(FitsFileUnit_t evunit)
{
  int colnum=0, status=OK;

  /* Check if PHAS column exists and get the number */
  if(!fits_get_colnum(evunit, CASEINSEN, CLNM_PHAS, &colnum, &status))
    {
      headas_chat(CHATTY, "%s: Warning: the input parameter %s is set to 'no',\n", global.taskname, PAR_ADDCOL);
      headas_chat(CHATTY, "%s: Warning: but the %s column exists\n", global.taskname, CLNM_PHAS);
      headas_chat(CHATTY, "%s: Warning: in %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY, "%s: Warning: The %s column will be deleted.\n", global.taskname, CLNM_PHAS);
      if(fits_delete_col(evunit, colnum, &status))
      {
	headas_chat(CHATTY, "%s: Error: Unable to delete %s column.\n", global.taskname, CLNM_PHAS);
	return NOT_OK;
      }
    }

  /* Check if PixsAbove column exists and get the number */
  if(!fits_get_colnum(evunit, CASEINSEN, CLNM_ABOVE, &colnum, &status))
    {
      headas_chat(CHATTY, "%s: Warning: The input parameter %s is set to 'no',\n", global.taskname, PAR_ADDCOL);
      headas_chat(CHATTY, "%s: Warning: but the %s column exists\n", global.taskname, CLNM_ABOVE);
      headas_chat(CHATTY, "%s: Warning: in %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY, "%s: Warning: The %s column will be deleted.\n", global.taskname, CLNM_ABOVE);
      if(fits_delete_col(evunit, colnum, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to delete %s column.\n", global.taskname, CLNM_ABOVE);
	  return NOT_OK;
	}
    }

  return OK;
}

/*
 *
 * RemoveNullEvents 
 *
 */

int RemoveNullEvents(Bintable_t *table, unsigned *nRows, int colnum)
{
  unsigned n,ActRow;
  ActRow=0;
  for (n=0; n<*nRows; ++n) 
    if(JVEC(*table, n, colnum)!= KWVL_PHANULL) 
      if(CopyRows(table, ActRow++, n))
	{
	  return NOT_OK;
	}

  *nRows = ActRow;

  return OK;
} /*RemoveNullEvents*/

/*
 *
 * AddStatKeywords
 * 
 */
int AddStatKeywords(FitsFileUnit_t *ounit)
{
  int                status=OK, i;
  
  GetGMTDateTime(global.date);
  /* EVENT THRESHOLD */
  if(fits_update_key(*ounit, TINT, KWNM_EVENT_TH, &global.onboardevent , CARD_COMM_EVENT_TH, &status))
    goto add_end;

  /* SPLIT THRESHOLD */
  if(fits_update_key(*ounit, TINT, KWNM_SPLIT_TH,&global.onboardsplit ,CARD_COMM_SPLIT_TH , &status))
    goto add_end;

  if(fits_update_key(*ounit, TINT, KWNM_TOTEV, &global.maxrows, CARD_COMM_TOTEV, &status))
    goto add_end;

  for(i=0; i<(TOT_TIM_GRADE-2); i++)
    global.per_grade[i] = 100.*(double)global.grade[i]/(double)global.maxrows;

  
  global.per_grade[i] = 100.*(double)global.grade[i]/(double)global.maxrows;
 
  if(fits_update_key(*ounit, TDOUBLE, KWNM_XRTNULEV, &global.per_grade[i],CARD_COMM_XRTNULEV, &status))
    goto add_end;
  i++;
  
  global.per_grade[i] = 100.*(double)global.grade[i]/(double)global.maxrows;
 
  if(fits_update_key(*ounit, TDOUBLE, KWNM_XRTNOREC, &global.per_grade[i],CARD_COMM_XRTNOREC, &status))
    goto add_end;

  global.per_tot_sat = 100.*(double)global.tot_sat/(double)global.maxrows;
  
  if(fits_update_key(*ounit, TDOUBLE, KWNM_XRTSATEV,&global.per_tot_sat ,CARD_COMM_XRTSATEV, &status))
    goto add_end;

  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: performed event recostrunction for windowed timing mode. ", global.taskname, global.swxrtdas_v,global.date );
      if(fits_write_history(*ounit, global.strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto add_end;
	}
      sprintf(global.strhist, "Used %s CALDB grade fits file.", global.par.gradefile);
      if(fits_write_history(*ounit, global.strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto add_end;
	}
      if(global.par.addcol)
	{
	  sprintf(global.strhist, "Added  %s and %s columns.", CLNM_PHAS, CLNM_ABOVE);
	  if(fits_write_history(*ounit, global.strhist, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	      goto add_end;
	    }
	}
      if(global.par.delnull)
	{
	  sprintf(global.strhist, "Deleted  rows with PHA set to NULL.");
	  if(fits_write_history(*ounit, global.strhist, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	      goto add_end;
	    }
	  
	}

      
    }
 
  return OK;
 add_end:
  return NOT_OK;

}

/*
 *      GetGrades
 *
 */
int GetGrades(long extno)
{
  int            n, status=OK, jj, count=0;
  char		 KeyWord[FLEN_KEYWORD];
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  GradeCol_t     gradecol; 
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;
  FitsCard_t     *card;                 /* Extension pointer */
  FitsFileUnit_t gradeunit=NULL;        /* Bias file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  global.grademin=-1;
  global.grademax=-1;

  /* Open read only bias file */
  if ((gradeunit=OpenReadFitsFile(global.par.gradefile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname, global.par.gradefile);
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, gradeunit, global.par.gradefile, global.taskname);
  
  if(extno != -1)
    {
      /* move to GRADES extension */
      if (fits_movabs_hdu(gradeunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_TMGRADES);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gradefile); 
	  goto get_end;
	} 
    }
  else
    {
      if (fits_movnam_hdu(gradeunit,ANY_HDU,KWVL_EXTNAME_TMGRADES,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_TMGRADES);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gradefile); 
	  goto get_end;
	} 
    }
  
  
  head = RetrieveFitsHeader(gradeunit);
  
  /* Read grade bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.gradefile);
      goto get_end;
    }

  /* Get columns index from name */

  if ((gradecol.Amp = GetColNameIndx(&table, CLNM_Amp)) == -1 )
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }


  if ((gradecol.GRADEID = GetColNameIndx(&table, CLNM_GRADEID)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }

  sprintf(KeyWord, "TLMIN%d", (gradecol.GRADEID+1));
  if (ExistsKeyWord(&head, KeyWord, &card))
    global.grademin=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Warning: TLMIN keywords for %s column not found\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.gradefile);
    }

  sprintf(KeyWord, "TLMAX%d", (gradecol.GRADEID+1));
  if (ExistsKeyWord(&head, KeyWord, &card))
    global.grademax=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Warning: TLMAX keywords for %s column not found\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.gradefile);
    }
  
  if ((gradecol.GRADE = GetColNameIndx(&table, CLNM_GRADE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  /* Get total grade rows number */
  global.graderows=table.MaxRows;
  /* Allocate memory to storage all coefficients */
  global.gradevalues=(TMGradeRow_t *)calloc(table.MaxRows, sizeof(TMGradeRow_t));

  /* Read blocks of bintable rows from input grade file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  while (ReadBintable(gradeunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	  global.gradevalues[count].amp=BVEC(table,n,gradecol.Amp);
	  global.gradevalues[count].gradeid=IVEC(table,n,gradecol.GRADEID);
	  /* Get grades */
	  for (jj=0; jj< PHAS7_MOL; jj++)
	    global.gradevalues[count].grade[jj] = IVECVEC(table,n,gradecol.GRADE,jj);
	  count++;
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close grade file */
  if (CloseFitsFile(gradeunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
  
  return NOT_OK;
  
}/* GetGrades */

/*
 *	PDEventRec
 *
 *
 *	DESCRIPTION:
 *
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
 *             void DeleteCard(FitsHeader_t *header, const char *KeyWord);
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
 *        0.1.0: - BS 11/09/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PDEventRec(FitsFileUnit_t evunit,FitsFileUnit_t ounit)
{
  int                found=0, status=0, q=0, logical, iii=0;
  int                ccdframe_count=0, offset_count=0, ccdframe_old, ccdframe=0;
  char		     KeyWord[FLEN_KEYWORD];
  BOOL               first_time=0;
  double             evtime, sens=0.0000001;
  unsigned           FromRow, ReadRows, OutRows=0, n=0,nCols=0, WriteRows=0;  
  Ev2Col_t           indxcol;
  Ev2Info_t          tmp_buff[PHAS7_MOL];
  Bintable_t	     intable;
  FitsCard_t         *card, *cardbias;
  FitsHeader_t	     head;

  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  head=RetrieveFitsHeader(evunit);
	
  /* Check if PHA is already computed */
  if( (!ExistsKeyWord(&head, KWNM_XRTPHACO, &card) || !card->u.LVal) &&
     (!ExistsKeyWord(&head, KWNM_BIASONBD, &cardbias) || !cardbias->u.LVal) )
    {
      headas_chat(CHATTY, "%s: Error: %s or %s keyword not found or unset\n",global.taskname, KWNM_BIASONBD, KWNM_XRTPHACO);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      headas_chat(NORMAL, "%s: Error: No bias subtraction has been applied on %s column,\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: the event reconstruction cannot be applied.\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: Please run 'xrtpdcorr' task.\n", global.taskname);
      goto event_end; 
    }
   
  if((!ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
    { 
      headas_chat(NORMAL,"%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE );
      headas_chat(NORMAL,"%s: Error: in the %s file.\n", global.taskname, global.par.infile);
      goto event_end;
    }
  else
    strcpy(KeyWord,card->u.SVal);
  
  if(PDReadHK(KeyWord))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read event and/or split threshold values from\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: the %s file.\n", global.taskname, global.par.hdfile);
      goto event_end;
    }
  
  /* Initialize bintable with number of rows = BINTAB_ROWS */
  InitBintable(&head, &intable, &nCols, &indxcol, BINTAB_ROWS);
  if(global.warning)
    goto ok_end;

  /* Add or update DS keywords */
  sprintf(KeyWord, "%d:%d",(int) global.grademin,(int) global.grademax); 
  
  ManageDSKeywords(&head, "GRADE", "I", KeyWord); 

  EndBintableHeader(&head, &intable); 
 
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &intable);
 
  FromRow = 1;

  global.totrows=global.maxrows;
  
  ReadRows=intable.nBlockRows;
  OutRows=0;
  global.amp=0;
  ccdframe_old=-1;
  /* Read input bintable */
  while(ReadBintable(evunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 && ReadRows!=0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  
	  ccdframe = VVEC(intable, n, indxcol.CCDFrame);
	  if( ccdframe != ccdframe_old )
	    {
	      evtime=DVEC(intable, n, indxcol.TIME_RO);
	      for(;iii < global.scount; iii++)
		{
		  found=1;
		  if(ccdframe==global.split_pd[iii].ccdframe && 
		     (global.split_pd[iii].hktime >= evtime-sens && global.split_pd[iii].hktime <= evtime+sens))
		    {
		      found=0;
		      if(global.par.split < 0)
			{
			  global.onboardsplit=global.split_pd[iii].split;
			  headas_chat(CHATTY, "%s: Info: Used %d split threshold value for %d ccdframe.\n",global.taskname, global.onboardsplit, ccdframe);
			}
		      if(global.par.event < 0)
			{
			  global.onboardevent=global.split_pd[iii].split;
			  headas_chat(CHATTY, "%s: Info: Used %d event threshold value for %d ccdframe.\n",global.taskname, global.onboardevent, ccdframe);
			}
		
		      ccdframe_old=ccdframe;
		      
		      goto split_found;
		    }
		}
	    }
	  else
	    {
	      found=0;
	      global.onboardevent=global.par.event;
	      global.onboardsplit=global.par.split;
	      ccdframe_old=ccdframe;
	      
	    }

	  if(found)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get split threshold value.\n", global.taskname);
	      goto event_end;
	    }
	  else
	    global.lastoffset=global.split_pd[iii].lastoffset;
	  
	split_found:


	  /*  printf(" nrows = %d\n", global.totrows);  */ 
	  if ((n+6) < ReadRows && (OutRows + 6) < BINTAB_ROWS) 
	    {

	      if(FillPD7x1Buff(n, offset_count, ccdframe_count, &first_time, intable, indxcol, tmp_buff))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to fill PD buffer.\n", global.taskname);
		  goto event_end;
		}
	      found=0;
	      if(tmp_buff[3].pha >= global.onboardevent)
		{
		  if(!IsLocalMax(tmp_buff))
		    {
		      found=1;
		      if(TMEvtRec7x1(tmp_buff, indxcol, intable, &OutRows))
			goto event_end;
		      
		    }
		  else if(tmp_buff[0].nrow != -1)
		    {
		      SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
		      global.totrows--;
		      OutRows++;
		    }
		 

		}
	      else if(tmp_buff[0].nrow != -1)
		{
		  SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
		  global.totrows--;
		  OutRows++;
		}

	      if(found)
		{
		  offset_count=tmp_buff[3].offset;
		  ccdframe_count=tmp_buff[3].ccdframe;
		  n=tmp_buff[3].nrow;
		  for(q=4; q < PHAS7_MOL; q++)
		    {
		      if(tmp_buff[q].pha >= global.onboardsplit)
			{
			  n++;
			  offset_count=tmp_buff[q].offset;
			  ccdframe_count=tmp_buff[q].ccdframe;
			}
		      else
			{
			  offset_count=(tmp_buff[q].offset - 3);
			  q=PHAS7_MOL;
  
			}
		    }
		}
	      else
		{

		  offset_count=tmp_buff[0].offset;
		  ccdframe_count=tmp_buff[0].ccdframe;
		  if(tmp_buff[0].nrow == -1)
		    n--;
		}
	    }
	  else 
	    {
	      if (global.totrows < 7 )
		{
		  for (; global.totrows > 0 && n!=BINTAB_ROWS; n++)
		    {
		      
		      if(FillPD7x1LastRows(n, offset_count, ccdframe_count, &first_time, intable, indxcol, tmp_buff))
			{
			  headas_chat(NORMAL, "%s: Error: Unable to fill PD buffer.\n", global.taskname);
			  goto event_end;
			}
		      found=0;
		      if(tmp_buff[3].pha >= global.onboardevent)
			{
			  if(!IsLocalMax(tmp_buff))
			    {
			      found=1;
			      if(TMEvtRec7x1(tmp_buff, indxcol, intable, &OutRows))
				goto event_end;
			     			      
			    }
			  else if(tmp_buff[0].nrow != -1)
			    {
			      SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
			      global.totrows--;
			      OutRows++;

			    }
			}
		      else if(tmp_buff[0].nrow != -1)
			{
			  SetNonRecEvent(intable, tmp_buff[0].nrow, indxcol);
			  global.totrows--;
			  OutRows++;

			}

		      if(found)
			{
			  offset_count=tmp_buff[3].offset;
			  ccdframe_count=tmp_buff[3].ccdframe;
			  n=tmp_buff[3].nrow;
			  for(q=4; q < PHAS7_MOL; q++)
			    {
			      if(tmp_buff[q].pha >= global.onboardsplit)
				{
				  n++;
				  offset_count=tmp_buff[q].offset;
				  ccdframe_count=tmp_buff[q].ccdframe;
				}
			      else
				{
				  offset_count=(tmp_buff[q].offset - 3);
				  q=PHAS7_MOL;
				}
			    }
			}

		      else
			{
			  offset_count=tmp_buff[0].offset;
			  ccdframe_count=tmp_buff[0].ccdframe;
			  if(tmp_buff[0].nrow == -1)
			    n--;			  
			}
		    }
		}
	      else 
		ReadRows=n; 
	    }
	 
	  
	}/* for(n) */

      WriteRows=OutRows;
      if(global.par.delnull)
	{
	  
	  if(RemoveNullEvents( &intable, &WriteRows, indxcol.PHA))
	    {  
	      headas_chat(NORMAL, "%s: Error: Unable to remove pixels with PHA set to NULL.\n", global.taskname);
	      goto event_end;
	    }
	}
      
      WriteFastBintable(ounit, &intable, WriteRows, FALSE);
      FromRow += OutRows;
      OutRows=0;
      ReadRows = BINTAB_ROWS;
      
    }/* while ReadBintable */ 

  if (OutRows > 0 )
    {
      WriteRows=OutRows;
      
      if(global.par.delnull)
	{
	  if(RemoveNullEvents( &intable, &WriteRows, indxcol.PHA))
	    {  
	      headas_chat(NORMAL, "%s: Error: Unable to remove pixels with PHA set to NULL.\n", global.taskname);
	      goto event_end;
	    }
	}

      WriteFastBintable(ounit, &intable, WriteRows, TRUE);
    }
  else
    WriteFastBintable(ounit, &intable, 0, TRUE);
 
  if(AddStatKeywords(&ounit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add statitic keywords\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.outfile);
      goto event_end;
    }
  logical=TRUE;
  if(fits_update_key(ounit, TLOGICAL, KWNM_XRTEVREC, &logical, CARD_COMM_XRTEVREC, &status))
    goto event_end;

  /* Update column comment */
  sprintf(KeyWord, "TTYPE%d", (indxcol.PHA+1));
  if (fits_modify_comment(ounit, KeyWord, CARD_COMM_PHA, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s column comment.\n", global.taskname, CLNM_PHA);
      goto event_end;
    }

 ok_end:
 
  return OK;
 
 event_end:
  if (head.first)
    ReleaseBintable(&head, &intable);
 
  return NOT_OK;
  
} /* PDPUEventRec */


int IsLocalMax(Ev2Info_t tmp_buff[PHAS7_MOL])
{
  int j=0;

  if(tmp_buff[3].pha < global.onboardevent)
    return NOT_OK;

  for (j=2; j >=0 ; j--)
    {
      if (tmp_buff[j].pha < global.onboardsplit)
	j=0;
      else if (tmp_buff[3].pha < tmp_buff[j].pha)
	return NOT_OK;
    }

  for(j=4; j<PHAS7_MOL; j++)
    {
      if (tmp_buff[j].pha < global.onboardsplit)
	j=PHAS7_MOL;
      else if (tmp_buff[3].pha < tmp_buff[j].pha)
	return NOT_OK;
    }
  return OK;
}
/* 
 *
 * FillPD7x1Buff
 *
 */
int FillPD7x1Buff(int count, int offset_count, int ccdframe_count, BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL])
{
  int q=0, offset=0, ccdframe=0, i, diff=0, under=1, amp=0, amp_1=0;
  
  
  /* Initialize buffer */
  for (q=0; q < PHAS7_MOL; q++)
    {
      tmp_buff[q].nrow=-1;
      tmp_buff[q].pha=-1;
      tmp_buff[q].offset=-1;
      tmp_buff[q].ccdframe=-1;
    }
  q=0;
  /* Get offset and ccdframe from current row */
  offset=JVEC(intable, count, indxcol.OFFSET);
  ccdframe=VVEC(intable, count, indxcol.CCDFrame);
  amp=BVEC(intable, count, indxcol.Amp);
  if(amp != AMP1 && amp != AMP2)
    {
      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp);
      goto fill_end;
    }
  

  /* Fill pha array */
  
  if(!(*first_time) || (((ccdframe - ccdframe_count) != 1) &&  ((ccdframe - ccdframe_count) != 0)) 
     || (offset - offset_count) <= 0 || amp != global.amp)
    {
      for(; q<3; q++)
	{
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  tmp_buff[q].offset=offset - 3 + q;
	  tmp_buff[q].ccdframe=ccdframe;
	}
      
      tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
      global.amp=amp;
      tmp_buff[q].nrow=count;
      tmp_buff[q].offset=offset;
      tmp_buff[q].ccdframe=ccdframe;
      q++;
      *first_time=1;
     
    }
  else if (ccdframe == ccdframe_count)
    {
      if((offset-offset_count) > 3)
	{
	  for(q=0; q<3; q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=offset - 3 + q;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	}

      /*******/
      else if(JVEC(intable, count, indxcol.PHA) < global.onboardsplit)
 	{ 
	  for (q=0; q < 2 ;q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=offset - 2 + q;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	  under=0;
	}
      else
	{
	  int flag=1;
	  for (q=0, i=1; q < 3 && flag; q++, i++)
	    {
	      if (offset == offset_count+i)
		{
		  tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
		  tmp_buff[q].nrow=count;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		  flag=0;
		}
	      else
		{
		  tmp_buff[q].pha=KWVL_PHANULL;
		  tmp_buff[q].nrow=-1;
		  tmp_buff[q].offset=offset_count+i;
		  tmp_buff[q].ccdframe=ccdframe;
	      
		}
	    }
	}
    }
  else if (ccdframe == (ccdframe_count+1))
    {
      if(offset > (FIRST_OFFSET+2) || offset_count < (global.lastoffset -2))
	{
	  for(q=0; q<3; q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      diff=offset-3+q;
	      if(diff >= FIRST_OFFSET)
		{ 
		  tmp_buff[q].offset=diff;
		  tmp_buff[q].ccdframe=ccdframe;
		}
	      else
		{
		  tmp_buff[q].offset=global.lastoffset+1+diff;
		  tmp_buff[q].ccdframe=ccdframe_count;
		}
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	}
      else
	{
	  int flag=1, j;
	  for (i=0; i<4 && flag ; i++)
	    {
	      if(offset_count == (global.lastoffset - i))
		{
		  for(j=0; j < (4 - i) && flag; j++)
		    {
		      if(offset == FIRST_OFFSET+j)
			{
			  for(q=0; q < j; q++)
			    {
			      tmp_buff[q].pha=KWVL_PHANULL;
			      tmp_buff[q].nrow=-1;
			      diff=offset-j+q;
			      if(diff >= FIRST_OFFSET)
				{
				  tmp_buff[q].offset=diff;
				  tmp_buff[q].ccdframe=ccdframe;
				}
			      else
				{
				  tmp_buff[q].offset=global.lastoffset+1+diff;
				  tmp_buff[q].ccdframe=ccdframe_count;
				}      
			    }
			  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
			  tmp_buff[q].nrow=count;
			  tmp_buff[q].offset=FIRST_OFFSET+j;
			  tmp_buff[q].ccdframe=ccdframe;
			  q++;  
			  flag = 0;
			}
		    }
		}
	
	    }
	}
    }
  count++;
 
  

 for(; q<7 ;q++)
    {
      amp_1=BVEC(intable, count, indxcol.Amp);
      if(amp_1 != AMP1 && amp_1 != AMP2)
	{
	  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp_1);
	  goto fill_end;
	}

      if (ccdframe == VVEC(intable, count, indxcol.CCDFrame) && global.amp == amp_1)
	{
	  if(offset != (JVEC(intable, count, indxcol.OFFSET) - 1))
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      offset++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  else
	    {
	      tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
	      global.amp=amp_1;
	      tmp_buff[q].nrow=count;
	      offset++;
	      count++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	}
      else if (ccdframe == (VVEC(intable, count, indxcol.CCDFrame) - 1) && global.amp == amp_1 )
	{
	  if(offset != global.lastoffset)
  	    { 
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      offset++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    } 
	  else 
	    { 
	      if(JVEC(intable, count, indxcol.OFFSET) == FIRST_OFFSET)
		{
		  tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
		  global.amp=amp_1;
		  tmp_buff[q].nrow=count;
		  ccdframe++;
		  offset=FIRST_OFFSET;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		  count++;
		} 
	      else 
		{ 
		  tmp_buff[q].pha=KWVL_PHANULL;
		  tmp_buff[q].nrow=-1;
		  offset=FIRST_OFFSET;
		  ccdframe++;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		} 
	      
	    } 
	}
      else
	{
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  offset++;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	}
    }
  if(!under)
    {
      int shift=0;
      for(shift=0; shift < 2 && IsLocalMax(tmp_buff); shift++)
	{
	  
	  for (q=1; q < PHAS7_MOL; q++)
	    {
	      tmp_buff[q-1].pha=tmp_buff[q].pha;
	      tmp_buff[q-1].nrow=tmp_buff[q].nrow;
	      tmp_buff[q-1].offset=tmp_buff[q].offset;
	      tmp_buff[q-1].ccdframe=tmp_buff[q].ccdframe;
	    }
	  amp_1=BVEC(intable, count, indxcol.Amp);
	  if(amp_1 != AMP1 && amp_1 != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp_1);
	      goto fill_end;
	    }

	  if (ccdframe == VVEC(intable, count, indxcol.CCDFrame) && global.amp == amp_1)
	    {
	      if(offset != (JVEC(intable, count, indxcol.OFFSET) - 1))
		{
		  tmp_buff[6].pha=KWVL_PHANULL;
		  tmp_buff[6].nrow=-1;
		  offset++;
		  tmp_buff[6].offset=offset;
		  tmp_buff[6].ccdframe=ccdframe;
		}
	      else
		{
		  tmp_buff[6].pha=JVEC(intable, count, indxcol.PHA);
		  global.amp=amp_1;
		  tmp_buff[6].nrow=count;
		  offset++;
		  count++;
		  tmp_buff[6].offset=offset;
		  tmp_buff[6].ccdframe=ccdframe;
		}
	    }
	  else if (ccdframe == (VVEC(intable, count, indxcol.CCDFrame) - 1) && global.amp == amp_1)
	    {
	      if(offset != global.lastoffset)
		{ 
		  tmp_buff[6].pha=KWVL_PHANULL;
		  tmp_buff[6].nrow=-1;
		  offset++;
		  tmp_buff[6].offset=offset;
		  tmp_buff[6].ccdframe=ccdframe;
		} 
	      else 
		{ 
		  if(JVEC(intable, count, indxcol.OFFSET) == FIRST_OFFSET)
		    {
		      tmp_buff[6].pha=JVEC(intable, count, indxcol.PHA);
		      global.amp=amp_1;
		      tmp_buff[6].nrow=count;
		      ccdframe++;
		      offset=FIRST_OFFSET;
		      tmp_buff[6].offset=offset;
		      tmp_buff[6].ccdframe=ccdframe;
		      count++;
		    } 
		  else 
		    { 
		      tmp_buff[6].pha=KWVL_PHANULL;
		      tmp_buff[6].nrow=-1;
		      offset=FIRST_OFFSET;
		      ccdframe++;
		      tmp_buff[6].offset=offset;
		      tmp_buff[6].ccdframe=ccdframe;
		    } 
		  
		} 
	    }
	  else
	    { 
	      tmp_buff[6].pha=KWVL_PHANULL;
	      tmp_buff[6].nrow=-1;
	      offset++;
	      tmp_buff[6].offset=offset;
	      tmp_buff[6].ccdframe=ccdframe;
	    } 
	}
      
    }
  return OK;
  
 fill_end:
  return NOT_OK;


}/* FillPD7x1Buff */
  
int FillPD7x1LastRows(int count, int offset_count, int ccdframe_count, BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL])
{
  int q=0, offset=0, ccdframe=0, read_rows=0, i=0, diff=0, under=1, amp=0, amp_1=0;
  
  
  /* Initialize buffer */
  for (q=0; q < PHAS7_MOL; q++)
    {
      tmp_buff[q].nrow=-1;
      tmp_buff[q].pha=-1;
      tmp_buff[q].offset=-1;
      tmp_buff[q].ccdframe=-1;
    }
  q=0;
  /* Get offset and ccdframe from current row */
  offset=JVEC(intable, count, indxcol.OFFSET);
  ccdframe=VVEC(intable, count, indxcol.CCDFrame);
  amp=BVEC(intable, count, indxcol.Amp);
  if(amp != AMP1 && amp != AMP2)
    {
      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp);
      goto last_end;
    }


	      
  /* Fill pha array */
  if(!(*first_time)  || (((ccdframe - ccdframe_count) != 1) && ((ccdframe - ccdframe_count) != 0))
     || (offset - offset_count) <= 0 || global.amp != amp)
    {
      for(; q<3; q++)
	{
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  tmp_buff[q].offset=offset - 3 + q;
	  tmp_buff[q].ccdframe=ccdframe;
	}
      global.amp=amp;      
      tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
      tmp_buff[q].nrow=count;
      tmp_buff[q].offset=offset;
      tmp_buff[q].ccdframe=ccdframe;
      q++;
      *first_time=1;
    }
  else if (ccdframe == ccdframe_count)
    {
      if((offset-offset_count) > 3)
	{
	  for(q=0; q<3; q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=offset - 3 + q;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  global.amp = BVEC(intable, count, indxcol.Amp);
	  if(global.amp != AMP1 && global.amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
	      goto last_end;
	    }
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	}
      /*******/
      else if(JVEC(intable, count, indxcol.PHA) < global.onboardsplit)
 	{ 
	  for (q=0; q < 2 ;q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=offset - 2 + q;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  global.amp = BVEC(intable, count, indxcol.Amp);
	  if(global.amp != AMP1 && global.amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
	      goto last_end;
	    }
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	  under=0;
	}
      else
	{
	  int flag=1;
	  for (q=0, i=1; q < 3 && flag ; q++, i++)
	    {
	      if (offset == offset_count+i)
		{
		  tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
		  global.amp = BVEC(intable, count, indxcol.Amp);
		  if(global.amp != AMP1 && global.amp != AMP2)
		    {
		      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
		      goto last_end;
		    }
		  tmp_buff[q].nrow=count;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		  flag=0;
		}
	      else
		{
		  tmp_buff[q].pha=KWVL_PHANULL;
		  tmp_buff[q].nrow=-1;
		  tmp_buff[q].offset=offset_count+i;
		  tmp_buff[q].ccdframe=ccdframe;
	      
		}
	    }
	 
	}
    }
  else if (ccdframe == (ccdframe_count+1))
    {
      if(offset > (FIRST_OFFSET+2) || offset_count < (global.lastoffset -2))
	{
	  for(q=0; q<3; q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      diff=offset-3+q;
	      if(diff >= FIRST_OFFSET)
		{
		  tmp_buff[q].offset=diff;
		  tmp_buff[q].ccdframe=ccdframe;
		}
	      else
		{
		  tmp_buff[q].offset=global.lastoffset+1+diff;
		  tmp_buff[q].ccdframe=ccdframe_count;
		}
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  global.amp = BVEC(intable, count, indxcol.Amp);
	  if(global.amp != AMP1 && global.amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
	      goto last_end;
	    }
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=offset;
	  tmp_buff[q].ccdframe=ccdframe;
	  q++;
	}
      else
	{
	  int flag=1, j;
	  for (i=0; i<4 && flag ; i++)
	    {
	      if(offset_count == (global.lastoffset - i))
		{
		  for(j=0; j < (4 - i) && flag; j++)
		    {
		      if(offset == FIRST_OFFSET+j)
			{
			  for(q=0; q < j; q++)
			    {
			      tmp_buff[q].pha=KWVL_PHANULL;
			      tmp_buff[q].nrow=-1;
			      diff=offset-j+q;
			      if(diff >= FIRST_OFFSET)
				{
				  tmp_buff[q].offset=diff;
				  tmp_buff[q].ccdframe=ccdframe;
				}
			      else
				{
				  tmp_buff[q].offset=global.lastoffset+1+diff;
				  tmp_buff[q].ccdframe=ccdframe_count;
				}      
			      
			    }
		     
			  
			  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
			  global.amp = BVEC(intable, count, indxcol.Amp);
			  if(global.amp != AMP1 && global.amp != AMP2)
			    {
			      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, global.amp);
			      goto last_end;
			    }
			  tmp_buff[q].nrow=count;
			  tmp_buff[q].offset=FIRST_OFFSET+j;
			  tmp_buff[q].ccdframe=ccdframe;
			  q++;  
			  flag = 0;
			}
		    }
		}
	      
	    }
	}
    }

  read_rows=1;
  count++;

  for(; q<7 && (global.totrows - read_rows)> 0 ;q++)
    {

      amp_1=BVEC(intable, count, indxcol.Amp);
      if(amp != AMP1 && amp != AMP2)
	{
	  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp);
	  goto last_end;
	}

      if (ccdframe == VVEC(intable, count, indxcol.CCDFrame) && global.amp == amp_1)
	{
	  if(offset != (JVEC(intable, count, indxcol.OFFSET) - 1))
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      offset++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  else
	    {
	      tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
	      global.amp = amp_1;
	      tmp_buff[q].nrow=count;
	      offset++;
	      read_rows++;
	      count++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	}
      else if (ccdframe == (VVEC(intable, count, indxcol.CCDFrame) - 1) && global.amp == amp_1)
	{
	  if(offset != global.lastoffset)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      offset++;
	      tmp_buff[q].offset=offset;
	      tmp_buff[q].ccdframe=ccdframe;
	    }
	  else
	    {
	      if(JVEC(intable, count, indxcol.OFFSET) == FIRST_OFFSET)
		{
		  tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
		  tmp_buff[q].nrow=count;
		  global.amp = amp_1;
		  read_rows++;
		  ccdframe++;
		  offset=FIRST_OFFSET;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		  count++;
		}
	      else
		{
		  tmp_buff[q].pha=KWVL_PHANULL;
		  tmp_buff[q].nrow=-1;
		  offset=FIRST_OFFSET;
		  ccdframe++;
		  tmp_buff[q].offset=offset;
		  tmp_buff[q].ccdframe=ccdframe;
		}
	      
	    }
	}
    }

  for(; q < 7; q++)
    {
      offset++;
      tmp_buff[q].pha=KWVL_PHANULL;
      tmp_buff[q].nrow=-1;
      tmp_buff[q].offset=offset;
      tmp_buff[q].ccdframe=ccdframe;
    }

  if(!under)
    {
      int shift=0;
      for(shift=0; shift < 2 && IsLocalMax(tmp_buff); shift++)
	{
	  for (q=1; q < PHAS7_MOL; q++)
	    {
	      tmp_buff[q-1].pha=tmp_buff[q].pha;
	      tmp_buff[q-1].nrow=tmp_buff[q].nrow;
	      tmp_buff[q-1].offset=tmp_buff[q].offset;
	      tmp_buff[q-1].ccdframe=tmp_buff[q].ccdframe;
	    }
	  if((global.totrows - read_rows) > 0)
	    {



	      amp_1=BVEC(intable, count, indxcol.Amp);
	      if(amp_1 != AMP1 && amp_1 != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname, amp_1);
		  goto last_end;
		}
	      
	      
	      
	      if (ccdframe == VVEC(intable, count, indxcol.CCDFrame) && global.amp == amp_1)
		{
		  if(offset != (JVEC(intable, count, indxcol.OFFSET) - 1))
		    {
		      tmp_buff[6].pha=KWVL_PHANULL;
		      tmp_buff[6].nrow=-1;
		      offset++;
		      tmp_buff[6].offset=offset;
		      tmp_buff[6].ccdframe=ccdframe;
		    }
		  else
		    {
		      tmp_buff[6].pha=JVEC(intable, count, indxcol.PHA);
		      global.amp=amp_1;
		      tmp_buff[6].nrow=count;
		      offset++;
		      count++;
		      read_rows++;
		      tmp_buff[6].offset=offset;
		      tmp_buff[6].ccdframe=ccdframe;
		    }
		}
	      else if (ccdframe == (VVEC(intable, count, indxcol.CCDFrame) - 1) && global.amp == amp_1)
		{
		  if(offset != global.lastoffset)
		    { 
		      tmp_buff[6].pha=KWVL_PHANULL;
		      tmp_buff[6].nrow=-1;
		      offset++;
		      tmp_buff[6].offset=offset;
		      tmp_buff[6].ccdframe=ccdframe;
		    } 
		  else 
		    { 
		      if(JVEC(intable, count, indxcol.OFFSET) == FIRST_OFFSET)
			{
			  tmp_buff[6].pha=JVEC(intable, count, indxcol.PHA);
			  global.amp=amp_1;
			  tmp_buff[6].nrow=count;
			  ccdframe++;
			  offset=FIRST_OFFSET;
			  tmp_buff[6].offset=offset;
			  tmp_buff[6].ccdframe=ccdframe;
			  count++;
			  read_rows++;
			} 
		      else 
			{ 
			  tmp_buff[6].pha=KWVL_PHANULL;
			  tmp_buff[6].nrow=-1;
			  offset=FIRST_OFFSET;
			  ccdframe++;
			  tmp_buff[6].offset=offset;
			  tmp_buff[6].ccdframe=ccdframe;
			} 
		  
		    } 
		
		}
	    }
	  else
	    {
	      tmp_buff[6].pha=KWVL_PHANULL;
	      tmp_buff[6].nrow=-1;
	      tmp_buff[6].offset=offset;
	      tmp_buff[6].ccdframe=ccdframe;
	    }
	}
      

    }

  return OK;

  last_end:
  return NOT_OK;


}/* FillPD7x1LastRows */
  
/*
 *
 *       SetNonRecEvent
 */

void SetNonRecEvent(Bintable_t intable, int n, Ev2Col_t indxcol)
{
  int q;
  if(global.par.addcol)
    {
      for (q=0; q < PHAS7_MOL ; q++)
	IVECVEC(intable, n,indxcol.PHAS,q) = KWVL_PHANULL;
      IVEC(intable, n, indxcol.ABOVE) = 0;
    }
  if (JVEC(intable, n, indxcol.PHA) < global.onboardsplit || JVEC(intable, n, indxcol.PHA) < global.onboardevent)
    {
      IVEC(intable, n, indxcol.GRADE) = KWVL_GRADENULL;
      global.grade[NULLREC]++;
    }
  else
    {
      IVEC(intable, n, indxcol.GRADE) = KWVL_GRADENONREC;
      global.grade[NONREC]++;
    }
  JVEC(intable, n, indxcol.EVTPHA) = JVEC(intable, n, indxcol.PHA);
  JVEC(intable, n, indxcol.PHA) = KWVL_PHANULL;

 
  
}

/*
 *
 *    FillWT7x1Buff
 *
 */

void FillWT7x1Buff(int count, int stop, int rawx_count,  BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL])
{
  int q=0, rawx=0, i=0, under=1;
  
  
  /* Initialize buffer */
  for (q=0; q < PHAS7_MOL; q++)
    {
      tmp_buff[q].nrow=-1;
      tmp_buff[q].pha=-1;
      tmp_buff[q].offset=-1;
    }
  q=0;
  /* Get rawx and from current row */
  rawx=IVEC(intable, count, indxcol.RAWX);
  /* Fill pha array */
  
  if(!(*first_time))
    {
      for(q=0; q<3; q++)
	{
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  tmp_buff[q].offset=rawx - 3 + q;
	}
      
      tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
      tmp_buff[q].nrow=count;
      tmp_buff[q].offset=rawx;
      q++;
      *first_time=1;
    }
  else if(((rawx-rawx_count) > 3) || (rawx - rawx_count) <= 0 )
	{
	 
	  for(q=0; q<3; q++)
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=rawx - 3 + q;
	    }
	  tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
	  tmp_buff[q].nrow=count;
	  tmp_buff[q].offset=rawx;
	  q++;
	}
  /*******/

  else if(JVEC(intable, count, indxcol.PHA) < global.onboardsplit)
    { 
      for (q=0; q < 2 ;q++)
	{
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  tmp_buff[q].offset=rawx - 2 + q;
	}
      tmp_buff[q].pha = JVEC(intable, count, indxcol.PHA);
      tmp_buff[q].nrow=count;
      tmp_buff[q].offset=rawx;
      q++;
      under=0;
    }


  else
    {
      int flag=1;
      for(q=0, i=1; q < 3 && flag; q++,i++)
	{ 
	  if (rawx == rawx_count+i)
	    {
	      tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
	      tmp_buff[q].nrow=count;
	      tmp_buff[q].offset=rawx;
	      flag=0;
	    }
	  else
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      tmp_buff[q].offset=rawx_count+i;
	    }
	 	
	}
    }
 
  count++;
  for(; q<PHAS7_MOL ;q++)
    {
      if(count <= stop)
	{
	  
	  if(rawx != (IVEC(intable, count, indxcol.RAWX) - 1))
	    {
	      tmp_buff[q].pha=KWVL_PHANULL;
	      tmp_buff[q].nrow=-1;
	      rawx++;
	      tmp_buff[q].offset=rawx;
	    }
	  else
	    {
	      tmp_buff[q].pha=JVEC(intable, count, indxcol.PHA);
	      tmp_buff[q].nrow=count;
	      rawx++;
	      count++;
	      tmp_buff[q].offset=rawx;
	    }
	}
      else
	{
	  
	  tmp_buff[q].pha=KWVL_PHANULL;
	  tmp_buff[q].nrow=-1;
	  rawx++;
	  tmp_buff[q].offset=rawx;
	}
      
    }


  if(!under)
    {
      int shift=0;

      for(shift=0; shift < 2 && IsLocalMax(tmp_buff) ; shift++)
	{
	  
	  for (q=1; q < PHAS7_MOL; q++)
	    {
	      tmp_buff[q-1].pha=tmp_buff[q].pha;
	      tmp_buff[q-1].nrow=tmp_buff[q].nrow;
	      tmp_buff[q-1].offset=tmp_buff[q].offset;
	    }

	  if(count <= stop)
	    {
	      if(rawx != (IVEC(intable, count, indxcol.RAWX) - 1))
		{
		  tmp_buff[6].pha=KWVL_PHANULL;
		  tmp_buff[6].nrow=-1;
		  rawx++;
		  tmp_buff[6].offset=rawx;
		}
	      else
		{
		  tmp_buff[6].pha=JVEC(intable, count, indxcol.PHA);
		  tmp_buff[6].nrow=count;
		  rawx++;
		  count++;
		  tmp_buff[6].offset=rawx;
		}
	    }
	  else
	    {
	      tmp_buff[6].pha=KWVL_PHANULL;
	      tmp_buff[6].nrow=-1;
	      rawx++;
	      tmp_buff[6].offset=rawx;
	    }

	}
	
    }



}/* FillWT7x1Buff */
int TMEvtRec7x1(Ev2Info_t tmp_buff[PHAS7_MOL], Ev2Col_t indxcol, Bintable_t intable, unsigned *OutRows)
{
  int                handle[PHAS7_MOL], ii=0, q=0, pha=KWVL_PHANULL, grade=KWVL_GRADENULL, above=0;
  
  /* Fill handle array */
  handle[0]=tmp_buff[3].pha;
  ii=1;
  for (q=0; q < PHAS7_MOL; q++)
    {
      if(q != 3)
	{
	  handle[ii]=tmp_buff[q].pha;
	  ii++;
	}
    }
  
  
  if(faint7x1(handle, &pha, &grade, &above))
    {
      headas_chat(NORMAL, "%s: Error: Unable to calculate PHA value.\n", global.taskname);
      goto evtrec_end;
      
    }


  for(ii=3; ii > 0 ; ii--)
    {
      if(handle[ii] < global.onboardsplit)
	{
	  ii--;
	  for(;ii > 0; ii--)
	    {
	      handle[ii]=KWVL_PHANULL;
	    }
	}
    }
  for(ii=4; ii < PHAS7_MOL; ii++)
    {
      if(handle[ii] < global.onboardsplit)
	{
	  ii++;
	  for(; ii < PHAS7_MOL; ii++)
	    handle[ii]=KWVL_PHANULL;
	  
	}
    }
  


  for(ii=2; ii >= 0 ; ii--)
    {
      if(tmp_buff[ii].nrow != -1)
	{
	  SetTMNullEvent(intable, tmp_buff[ii].nrow, indxcol);
	  global.totrows--;
	  *OutRows+=1;
	}
      
      if(tmp_buff[ii].pha < global.onboardsplit)
	{
	  ii--;
	  for(;ii >= 0; ii--)
	    {
	      
	      if(tmp_buff[ii].nrow != -1)
		{
		  SetNonRecEvent(intable, tmp_buff[ii].nrow, indxcol);
		  global.totrows--;
		  *OutRows+=1;
		}
	    }

	}
    }

  /* Fill central event row */
  if(global.par.addcol)
    {
      for (ii=0; ii < PHAS7_MOL ; ii++)
	IVECVEC(intable, tmp_buff[3].nrow,indxcol.PHAS,ii) = handle[ii];
      IVEC(intable, tmp_buff[3].nrow, indxcol.ABOVE) = above;
    }
	  
  if(global.par.flagneigh && global.wt)
    {
      for (ii=0; ii < PHAS7_MOL ; ii++)
	{
	  if (ii != 3 && tmp_buff[ii].nrow != -1)
	    if (XVEC2BYTE(intable, tmp_buff[ii].nrow,indxcol.STATUS) != 0)
	      XVEC2BYTE(intable, tmp_buff[3].nrow,indxcol.STATUS) = EV_NEIGH_BP;
	}
	
	
    }

  JVEC(intable,tmp_buff[3].nrow , indxcol.EVTPHA) = JVEC(intable,tmp_buff[3].nrow , indxcol.PHA);
  JVEC(intable,tmp_buff[3].nrow , indxcol.PHA) = pha;
  IVEC(intable,tmp_buff[3].nrow, indxcol.GRADE) = grade;
	  
  global.grade[grade]++;
  global.totrows--;
  *OutRows+=1;
	
  for(q=4; q<PHAS7_MOL;q++)
    {
      if(tmp_buff[q].pha < global.onboardsplit)
	q=PHAS7_MOL;
	
      else if(tmp_buff[q].nrow != -1)
	{
	  SetTMNullEvent(intable, tmp_buff[q].nrow, indxcol);
	  global.totrows--;
	  *OutRows+=1;
	}
    }
      

  return OK;
 evtrec_end:
  return NOT_OK;

    }
int faint7x1(int handle[PHAS7_MOL], int *pha, int *grade, int *above)
{
  int ii=0, jj=0, pattern[PHAS7_MOL], found;
  
  *grade=KWVL_GRADENULL;

  /* Assign grade */
  
  /* Central event */
  *pha=0;
  *above=0;
  pattern[0]=1;


  /* neighbor */
  for (jj=1; jj< PHAS7_MOL; jj++)
    {
      if(handle[jj] >= global.onboardsplit)
	pattern[jj]=1;
      else
	pattern[jj]=2;
    }
  
  found=0;
  
  for(ii=0; ii<global.graderows && *grade==KWVL_GRADENULL; ii++)
    {
      if(global.gradevalues[ii].amp == global.amp)
	{
	  for(jj=0; jj<PHAS7_MOL && found==0; jj++) 
	    if((pattern[jj]!=global.gradevalues[ii].grade[jj]) && (global.gradevalues[ii].grade[jj]))
	      found=1;
      
	  if(!found)
	    *grade=global.gradevalues[ii].gradeid;
	  else
	    found=0;
	}
    }

  for(jj=0; jj<PHAS7_MOL ; jj++)
    {
      if(global.gradevalues[ii - 1].grade[jj] == 1)
	{	
	  *pha+=handle[jj];
	  *above+=1;
	}
    }

  if (*pha >= SAT_VAL)
    {
      *pha=SAT_VAL;
      global.tot_sat++;
    }


  return OK;
  
}/* faint7x1*/

/*
 *  xrtevtrec_checkinput
 *
 *	DESCRIPTION:
 *          check integrity of input data and define
 *          input/output default file name
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *          get_toolnamev(char *);
 *          headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 24/03/2004 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 *
 */
int xrtevtrec_checkinput(void)
{
  int      overwrite=0;
  char     stem[10] , ext[MAXEXT_LEN] ;
  pid_t    tmp;


  /* Check if outfile == NONE */    
  if (!(strcasecmp (global.par.outfile, DF_NONE)))
    {/* To overwrite input file, it must be unzipped */
      GetFilenameExtension(global.par.infile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	{
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	  goto check_end;
	  
	}
      else
	/* Overwrite input file */
	overwrite=1;
      
    }
  else
    {
      /* Check if output file exists and if is possible remove it */
      if(FileExists(global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists.\n", global.taskname, global.par.outfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: Cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: Parameter 'clobber' set,\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	      if(remove (global.par.outfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: '%s' file.\n", global.taskname, global.par.outfile);
		  goto check_end;
		}  
	    }
	}
    }
  /* Derive temporary event filename */
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  if (overwrite)
    {
      DeriveFileName(global.par.infile, global.tmpfile, stem);
      strcpy(global.par.outfile, global.par.infile);
    }
  else
    DeriveFileName(global.par.outfile, global.tmpfile, stem);
  
  /* Check if file exists to remove it*/
  if(FileExists(global.tmpfile))
    if(remove (global.tmpfile) == -1)
      {
	headas_chat(CHATTY, "%s: Error: Unable to remove\n", global.taskname);
 	headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto check_end;
      }

  return OK;

 check_end:
  return NOT_OK;

}
/* 
   WTReadHK
*/

int WTReadHK(char *obsmode)
{

  int                status=OK,  hkmode=0, good_row=1;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[8];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.scount=0;

  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
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
      goto WTReadHK_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }


  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  if ((hkcol.EvtLLD=ColNameMatch(CLNM_EVTLLD, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EVTLLD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto WTReadHK_end;
    }



  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following columns */
  nCols=8; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.XRTMode;
  ActCols[2]=hkcol.Settled;
  ActCols[3]=hkcol.In10Arcm;
  ActCols[4]=hkcol.EvtLLD;
  ActCols[5]=hkcol.FSTS;
  ActCols[6]=hkcol.FSTSub;
  ActCols[7]=hkcol.InSafeM;
  
  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  good_row=1;

	  hkInSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);	  
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);
	  
	  if(!strcmp(obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }


	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
  
	  /*printf("%u -- %u -- %d\n",hksettled,hkin10Arcm,hkmode);*/

	  if(good_row && (hkmode == XRTMODE_WT))
	    {
	      if(global.scount)
		{
		  global.scount+=1;
		  global.split_wt = (WTSplitValue_t *)realloc(global.split_wt, ((global.scount)*sizeof(WTSplitValue_t)));
		  
		}
	      else
		{
		  global.scount+=1;
		  global.split_wt = (WTSplitValue_t *)malloc(sizeof(WTSplitValue_t));  
		  
		}

	      global.split_wt[global.scount - 1].ccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.split_wt[global.scount - 1].hktime=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	      global.split_wt[global.scount - 1].split=UVEC(hktable, n, hkcol.EvtLLD);

	      
	    }
	  
	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.scount)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  WTReadHK_end;
    }


  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 WTReadHK_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* WTReadHK */

/* 
   PDReadHK
*/
int PDReadHK(char *obsmode)
{

  int                status=OK,  hkmode=0, evmode=0, maxoffset, good_row=1;
  double             fet, fst;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[11];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.scount=0;
  if(global.lr)
    evmode=XRTMODE_LR;
  else
    evmode=XRTMODE_PU;

  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
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
      goto PDReadHK_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  if ((hkcol.EvtLLD=ColNameMatch(CLNM_EVTLLD, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EVTLLD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FETS=ColNameMatch(CLNM_FETS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.FETSub=ColNameMatch(CLNM_FETSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }
  /* Frame Start Time */
  if ((hkcol.nPixels=ColNameMatch(CLNM_nPixels, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_nPixels);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto PDReadHK_end;
    }



  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following columns */
  nCols=11; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.XRTMode;
  ActCols[2]=hkcol.Settled;
  ActCols[3]=hkcol.In10Arcm;
  ActCols[4]=hkcol.EvtLLD;
  ActCols[5]=hkcol.FSTS;
  ActCols[6]=hkcol.FSTSub;
  ActCols[7]=hkcol.FETS;
  ActCols[8]=hkcol.FETSub;
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
	  
	  if(!strcmp(obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }

	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
	  
	  /*printf("%u -- %u -- %d\n",hksettled,hkin10Arcm,hkmode);*/

	  if(good_row && (hkmode == evmode))
	    {
	      if(global.scount)
		{
		  global.scount+=1;
		  global.split_pd = (PDSplitValue_t *)realloc(global.split_pd, ((global.scount)*sizeof(PDSplitValue_t)));
		  
		}
	      else
		{
		  global.scount+=1;
		  global.split_pd = (PDSplitValue_t *)malloc(sizeof(PDSplitValue_t));  
		  
		}

	      fst=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	      if(global.lr)
		{
		 
		  fet=(VVEC(hktable, n, hkcol.FETS) + ((20.*UVEC(hktable, n, hkcol.FETSub))/1000000.));
		  CalculateNPixels(fst, fet, &maxoffset);
		}
	      else
		maxoffset=JVEC(hktable, n, hkcol.nPixels);

	      global.split_pd[global.scount - 1].lastoffset=maxoffset-1;

	      global.split_pd[global.scount - 1].ccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.split_pd[global.scount - 1].hktime=fst;
	      global.split_pd[global.scount - 1].split=UVEC(hktable, n, hkcol.EvtLLD);

	      
	    }
	  
	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.scount)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  PDReadHK_end;
    }

  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 PDReadHK_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* PDReadHK */


