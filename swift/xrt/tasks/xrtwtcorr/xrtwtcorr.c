/*
 * 
 *	xrtwtcorr.c:
 *
 *	INVOCATION:
 *
 *		xrtwtcorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *                Routine to correct Windowed Timing PHA value 
 *                subtracting the difference between bias calculated on-board
 *                and bias calculated on-ground 
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 07/06/2005 - First version
 *        0.1.1 -    24/06/2005 - Bug fixed calculating median value
 *        0.1.2 -    13/07/2005 - Replaced ReadBintable with ReadBintableWithNULL
 *                                to perform check on undefined value reading hdfile
 *        0.1.3 -    09/08/2005 - Create outfile if task exits with warning
 *                              - Exit with error if trfile or hdfile are empty
 *        0.1.4 -    03/02/2006 - If biasdiff=0, nothing will be done and
 *                                exit with warning
 *        0.1.5 -    23/02/2006 - Added 'colfile' input parameter
 *                              - Check 'XRAWXCOR' keyword and shift RAWX BiasRow if 
 *                                keyword is set to TRUE
 *        0.1.6 -    03/03/2006 - Check BRow[1/2]CO and BRow[/2]Len instead of WM1stCol
 *                                and WMColNum to calculate RAWX offset 
 *        0.1.7 -    27/07/2006 - Added 'nframe' input parameter
 *                              - Compute median "ME20" using a set of n consecutive frames
 *                              - Added new metod for bias correction. With this metod the
 *                                on-board bias is calculated using the last 20 pixels median of
 *                                the related bias row (not taken the real value subtracted on-board)
 *                              - Added 'biasmode' input parameter, allowed values are 'SP', that is
 *                                for the old method, and 'M20P' for the new one
 *        0.1.8 -    30/08/2006 - Modified some stdout messages with chatter=5
 *                              - Modified check of 'biasmode' input parameter to be not case sensitive
 *        0.1.9 -    07/11/2006 - Bug fixed in 'xrtwtcorr' function when 'xrtwtcorr_getpar' gives an error
 *        0.2.0 -    06/02/2007 - Added 'biasth' and 'npixels' input parameters
 *        0.2.1 -    11/07/2007 - New input parameter 'thrfile'
 *        0.2.2 -    15/07/2016 - Minor changes for median calculation of the last frames
 *        0.2.3 -    06/10/2016 - Bug fix for median calculation of the last frames
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtwtcorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtwtcorr.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTWTCORR_C
#define XRTWTCORR_VERSION      "0.2.3"
#define PRG_NAME               "xrtwtcorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtwtcorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtwtcorr.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void WriteInfoParams(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 08/06/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtwtcorr_getpar()
{
  /*
   *  Get File name Parameters
   */
  
  /* Input Windowed Timing Event List File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }
  
  /* Output Event List File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }
  
  if(PILGetInt(PAR_BIASDIFF, &global.par.biasdiff)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASDIFF);
      goto Error;	
    }
  
  if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
      goto Error;	
    }
  
  if(PILGetFname(PAR_TRFILE, global.par.trfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TRFILE);
      goto Error;	
    }
  
  if(PILGetInt(PAR_NFRAME, &global.par.nframe)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NFRAME);
      goto Error;	
    }

  if(PILGetString(PAR_BIASMODE, global.par.biasmode)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASMODE);
      goto Error;	
    }

  /* Input Thresholds file */
  if(PILGetFname(PAR_THRFILE, global.par.thrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_THRFILE);
      goto Error;	
    }

  global.usethrfile=1;
  
  if ( !strcasecmp(global.par.thrfile,DF_NONE) )
    {
      global.usethrfile=0;

      if(PILGetInt(PAR_BIASTH, &global.par.biasth)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASTH);
	  goto Error;	
	}
    }

  if(PILGetInt(PAR_NPIXELS, &global.par.npixels)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NPIXELS);
      goto Error;	
    }

  /* Input Windowed Timing Event List File Name */
  if(PILGetFname(PAR_COLFILE, global.par.colfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_COLFILE);
      goto Error;
    }

   if(PILGetInt(PAR_NEVENTS, &global.par.nevents)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NEVENTS);
      goto Error;	
    }
  
  
  get_history(&global.hist);
  xrtwtcorr_info();
  
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* xrtwtcorr_getpar */
/*
 *	xrtwtcorr_work
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
 *           int PDPhaCorrect (FitsFileUnit_t unit, FitsFileUnit_t unit, int *);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum, int *)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 *               GetFilenameExtension(char *, char *);
 *               DeriveFileName(char *, char *, char *);
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 08/06/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtwtcorr_work()
{
  int            status = OK, inExt, outExt, evExt;   
  int            logical;
  long           nrows=0, extno=0;
  ThresholdInfo_t thrvalues;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL, colunit=NULL;              /* Input and Output fits file pointer */ 

  /* Initialize global vars */
  
  global.trnrows=0;
  global.totbias=0;
  global.colrows=0;
  global.rawxcorr=0;
  
  
  if(xrtwtcorr_checkinput())
    goto Error;

  if(!global.par.biasdiff)
    {
      global.warning=1;
      headas_chat(NORMAL,"%s: Warning: %s input parameter set to %d\n", global.taskname, PAR_BIASDIFF, global.par.biasdiff);
      headas_chat(NORMAL,"%s: Warning: nothing will be done.\n", global.taskname);
      goto ok_end;
    }
  
  /* Read bias rows in the trailer file */  
  if(ReadTrFile())
    {
      headas_chat(NORMAL,"%s: Error: Unable to process\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.trfile);
      goto Error;
    }
  
  
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
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    
  
  if(ExistsKeyWord(&head, KWNM_XRTPHACO, &card))
    {
      if(card->u.LVal)
	{
	  headas_chat(NORMAL, "%s: Error: %s is already corrected on ground,\n", global.taskname, CLNM_PHA);
	  headas_chat(NORMAL, "%s: Error: nothing to be done.\n", global.taskname);
	  goto Error;
	}
    }
  
  if(!ExistsKeyWord(&head, KWNM_DATAMODE, &card))
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  if(strcmp(card->u.SVal, KWVL_DATAMODE_TM))
    {
      headas_chat(NORMAL,"%s: Error: This task does not process the %s readout mode\n", global.taskname, card->u.SVal);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid readout mode is: %s\n", global.taskname, KWVL_DATAMODE_TM);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: ReadInputFileKeywords: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: ReadInputFileKeywords: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
      
    }
  /* Retrieve TSTART from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    global.tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Retrieve TSTOP from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    global.tstop=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve DATEOBS from input event file */
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

  /* Retrieve XRTVSUB of input event file */
  if((ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.xrtvsub=card->u.JVal;
    }
  else
    {
      global.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }


  if(!(ExistsKeyWord(&head, KWNM_XRAWXCOR  , NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRAWXCOR)))
    {
      headas_chat(NORMAL, "%s: Info: %s keyword not found or set to FALSE\n", global.taskname,KWNM_XRAWXCOR);
      headas_chat(NORMAL, "%s: Info: the RAWX column is not corrected.\n", global.taskname);
      global.rawxcorr=0;
      
    }
  else
    {
      
      headas_chat(NORMAL, "%s: Info: %s keyword set to TRUE\n", global.taskname,KWNM_XRAWXCOR);
      headas_chat(NORMAL, "%s: Info: the RAWX is corrected.\n", global.taskname);
      global.rawxcorr=1;
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


  if(global.usethrfile)
    {
      /********************************
       *     Get CALDB thresholds file    *
       ********************************/
      
      extno=-1;
      
      if (!(strcasecmp (global.par.thrfile, DF_CALDB)))
	{
	  if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_EXTNAME_XRTVSUB, global.par.thrfile,HD_EXPR, &extno))
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

      if(ReadThrFile(global.par.thrfile, extno, global.xrtvsub, &thrvalues))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read Bias Threshold level from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the input thrfile.\n", global.taskname);
	  goto Error;
	}
      
      global.par.biasth=thrvalues.wtbiasth;

      headas_chat(NORMAL, "%s: Info: Bias Threshold from input thresholds file = %d\n", global.taskname, global.par.biasth);
    }


  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
      goto Error;
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
  /* Correct PHA  */
  
  if(ComputeBiasMedian())
    {
      headas_chat(NORMAL, "%s: Error: Unable to calculate difference between\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: on-board bias subtracted and median value.\n", global.taskname);
      goto Error;
    }  
  
  if(global.warning)
    goto ok_end;
  
  if ((WTPhaReCorrect(inunit, outunit)))
    {
      headas_chat(NORMAL, "%s: Error: Unable to correct PHA values.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;
  
  /* Set XRTPHACO keyword to true */
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPHACO, &logical, CARD_COMM_XRTPHACO, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTPHACO);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
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
  
  /* Update checksum and datasum keywords in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
  
  /* close input and output files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
      goto Error;
    }  
  if (CloseFitsFile(outunit)) 
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.outfile);
      goto Error;
    }   
  /* rename temporary file into output event file */
  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file.\n", global.taskname);
      goto Error;
    }
  
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);
  
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

  if(global.totbias)
    free(global.bias_median);
  if(global.trnrows)
    free(global.trailer);
  return OK; 
  
 Error:
  if(global.totbias)
    free(global.bias_median);
  if(global.trnrows)
    free(global.trailer);
  return NOT_OK;
} /* xrtwtcorr_work */
/*
 *	xrtwtcorr
 *
 *
 *	DESCRIPTION:
 *              Tool to correct pha events value for Windowed Timing readout mode  
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
 *             void xrtwtcorr_getpar(void);
 *             void WriteInfoParams(void); 
 * 	       void xrtwtcorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/06/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtwtcorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTWTCORR_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  global.warning=0;
  /* Get parameter values */ 
  if ( xrtwtcorr_getpar() == OK) 
    {
      
      if ( xrtwtcorr_work()) 
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  goto pdcorr_end;
	}
      else if(global.warning)
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with Warning.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  if (FileExists(global.tmpfile))
	    remove (global.tmpfile);
	}
      
      return OK;
  
    }
  
 pdcorr_end:
  
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;
  
} /* xrtwtcorr */
/*
 *	xrtwtcorr_info:
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
 *        0.1.0: - BS 09/06/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtwtcorr_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input Housekeeping Header Packet file     :'%s'\n",global.par.hdfile);
  headas_chat(NORMAL,"Name of the input Housekeeping Trailer Packet file    :'%s'\n",global.par.trfile);
  headas_chat(NORMAL,"Bias difference                                       : %d\n",global.par.biasdiff);
  headas_chat(NORMAL,"Minimum number of events to calculate the bias value  : %d\n",global.par.nevents);
  headas_chat(NORMAL,"Number of frames to calculate the bias median         : %d\n",global.par.nframe);
  headas_chat(NORMAL,"Metod to calculate module bias difference             : %s\n",global.par.biasmode);
  headas_chat(NORMAL,"Name of the input Thresholds file                     :'%s'\n",global.par.thrfile);
  if(!global.usethrfile)
    {
      headas_chat(NORMAL,"Bias Threshold of pix to calculate the bias median    : %d\n",global.par.biasth);
    }
  headas_chat(NORMAL,"Minimum number of pix to calculate the bias median    : %d\n",global.par.npixels);
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtwtcorr_info */
/* 
 * 
 * xrtwtcorr_checkinput 
 *
 */
int xrtwtcorr_checkinput(void)
{
  int            overwrite=0;
  char           stem[10] , ext[MAXEXT_LEN] ;
  pid_t          tmp;
  
  /* Check if outfile == NONE */    
  if (!(strcasecmp (global.par.outfile, DF_NONE)))
    {
      /* If outfile == NONE check if input file is compressed */
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
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	      if(remove (global.par.outfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
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
	headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpfile);
	goto check_end;
      }

  if((stricmp(global.par.biasmode,SP_BIASMODE))&&(stricmp(global.par.biasmode,M20P_BIASMODE)))
    {
      headas_chat(NORMAL, "%s: Error: '%s' not a valid parameter for biasmode.\n", global.taskname,global.par.biasmode);
      goto check_end;
    }
  
  return OK;
 check_end:
  return NOT_OK;
}
/*
 *
 *  ComputeBiasMedian
 *
 *  DESCRIPTION:
 *    Routine to Calculate the median using pixels value in the hd file
 *
 *
 */


int ComputeBiasMedian(void)
{  
  int                status=OK, jj=0, tmpmode=0, newsoft=0;
  int                iii=0, *median=NULL, level=0, mod=0, ind=0;
  double             diff_db=0.0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[7];
  HKCol_t            hkcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  unsigned start_nframe=0, end_nframe=0;
  int next_nframe=1;
  double trailer_fet=0.0;
  int median_length=0;
  int num_frame=0;
  int c_frame_npix=0;
  double c_frame_time=0.0;
  int median_index=0;
  unsigned n_init=0;
  int pix_over_biasth=0;
  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  if ((unit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      
      goto ComputeBiasMedian_end;
      
    }
  
  head=RetrieveFitsHeader(unit);
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {
      if(global.tstart < (card->u.DVal - TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file\n", global.taskname,global.par.infile);
	  headas_chat(NORMAL, "%s: Error: are not included in the range time\n", global.taskname); 
	  headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.hdfile);
	  goto ComputeBiasMedian_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    {
      
      if(global.tstop > (card->u.DVal + TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file are not included in the range time\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.hdfile);
	  goto ComputeBiasMedian_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  /* Get needed columns number from name */
  
  /*  XRTMode */
  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  /* BiasPx */
  if ((hkcol.BiasPx=ColNameMatch(CLNM_BiasPx, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Warning: '%s' column does not exist\n", global.taskname, CLNM_BiasPx);
      headas_chat(NORMAL, "%s: Warning: in '%s' file.\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Warning: This task processes only new header packet format file.\n",global.taskname);
      global.warning=1;
      return OK;
    }
  
  if ((hkcol.HPIXCT=ColNameMatch(CLNM_HPIXCT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Warning: '%s' column does not exist\n", global.taskname, CLNM_HPIXCT);
      headas_chat(NORMAL, "%s: Warning: in '%s' file.\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Warning: This task processes only new header packet format file.\n",global.taskname);
      global.warning=1;
      return OK;
      
    }
  
  /* TIME */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }

  if ((hkcol.WAVE=ColNameMatch(CLNM_WaveID, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WaveID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end;
    }
  
  EndBintableHeader(&head, &table);
  
  FromRow = 1;
  ReadRows=table.nBlockRows;

  nCols=7;
  ActCols[0]=hkcol.XRTMode;
  ActCols[1]=hkcol.CCDFrame;
  ActCols[2]=hkcol.BiasPx;
  ActCols[3]=hkcol.HPIXCT;
  ActCols[4]=hkcol.FSTS;
  ActCols[5]=hkcol.FSTSub;
  ActCols[6]=hkcol.WAVE;

  
  /* Read only the following columns */
  
  while(ReadBintableWithNULL(unit, &table, nCols, ActCols,FromRow,&ReadRows) == 0)
    {
    ReadCurrentTable:
      for(n=n_init; n<ReadRows ; ++n)
	{  
	  tmpmode=BVEC(table, n, hkcol.XRTMode);
	  if(tmpmode==XRTMODE_WT)
	    {
	      if(next_nframe)
		{
		  start_nframe=FromRow+n;
		  end_nframe=FromRow+n;

		  if(!global.totbias)
		    {
		      global.totbias++;
		      global.bias_median = (BiasMedianFormat_t *)malloc(sizeof(BiasMedianFormat_t));
		    }
		  else
		    {
		      global.totbias++;
		      global.bias_median = (BiasMedianFormat_t *)realloc(global.bias_median, (global.totbias*sizeof(BiasMedianFormat_t)));
		    }
		  
		  global.bias_median[global.totbias-1].time = (VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
		  global.bias_median[global.totbias-1].ccdframe =VVEC(table, n, hkcol.CCDFrame) ;
		  global.bias_median[global.totbias-1].wave =BVEC(table, n, hkcol.WAVE) ;
		  global.bias_median[global.totbias-1].npix=IVEC(table,n,hkcol.HPIXCT);
		  
		  if(global.bias_median[global.totbias-1].npix > table.Multiplicity[hkcol.BiasPx])
		    {
		      headas_chat(NORMAL, "%s: Warning: In the %s file for %d ccdframe\n", global.taskname, global.par.hdfile, global.bias_median[global.totbias-1].ccdframe);
		      headas_chat(NORMAL, "%s: Warning: multiplicy of the %s column is %d\n", global.taskname, CLNM_BiasPx,table.Multiplicity[hkcol.BiasPx]);
		      headas_chat(NORMAL, "%s: Warning: but %s column contains %d.\n", global.taskname, CLNM_HPIXCT,global.bias_median[global.totbias-1].npix);
		      global.bias_median[global.totbias-1].npix=0;
		      global.bias_median[global.totbias-1].median=0.0;
		      
		    }
		  else if(!global.bias_median[global.totbias-1].npix)
		    {
		  
		      headas_chat(CHATTY, "%s: Warning: Unable to calculate median for %d ccdframe.\n", global.taskname, global.bias_median[global.totbias-1].ccdframe);
		      headas_chat(CHATTY, "%s: Warning: the %s column contains %d counts.\n", global.taskname, CLNM_HPIXCT,global.bias_median[global.totbias-1].npix);
		      global.bias_median[global.totbias-1].median=0.0;
		    }
		  else if(global.bias_median[global.totbias-1].npix < global.par.nevents)
		    {   
		      headas_chat(CHATTY, "%s: Warning: Unable to calculate median for %d ccdframe.\n", global.taskname, global.bias_median[global.totbias-1].ccdframe);
		      headas_chat(CHATTY, "%s: Warning: the %s column contains %d counts\n", global.taskname, CLNM_HPIXCT,global.bias_median[global.totbias-1].npix);
		      headas_chat(CHATTY, "%s: Warning: but '%s' input parameter is set to %d\n", global.taskname, PAR_NEVENTS,global.par.nevents);
		      global.bias_median[global.totbias-1].median=0.0;
		      global.bias_median[global.totbias-1].npix=0;
		    }
		  else
		    {
		      median_length=global.bias_median[global.totbias-1].npix;
		      median=(int *)malloc(median_length*sizeof(int));
		      num_frame=1;
		      pix_over_biasth=0;
		      
		      headas_chat(CHATTY,"%s: Info: Last 20 px bias of consecutive 'nframe' Frames :\n", global.taskname);
		      headas_chat(CHATTY,"%s: Info: Frame: %10u :",
				  global.taskname,
				  global.bias_median[global.totbias-1].ccdframe 
				  );
		      
		      for (jj=0; jj<global.bias_median[global.totbias-1].npix ; jj++)
			{
			  level=IVECVEC(table,n,hkcol.BiasPx,jj);
			  
			  headas_chat(CHATTY," %4d",level);
			  
			  if (level>global.par.biasth)
			    {
			      headas_chat(CHATTY," (>biasth)");
			      pix_over_biasth++;
			      continue;
			    }
			  
			  if((jj-pix_over_biasth)==0)
			    median[jj-pix_over_biasth]=level;
			  else
			    if(level >= median[jj-1-pix_over_biasth])
			      median[jj-pix_over_biasth]=level;
			    else
			      {
				for(iii=jj-pix_over_biasth; iii >= 0 ; iii--)
				  {
				    if(!iii)
				      median[iii]=level;
				    
				    else
				      if (level < median[iii-1])
					{
					  median[iii]=median[iii-1];
					}
				      else
					{
					  median[iii]=level;
					  iii=0;
					}
				  }
			      }
			}
		      headas_chat(CHATTY,"\n");

		      median_length-=pix_over_biasth;

		      if(num_frame==global.par.nframe)
					goto Compute_Median;

     	   	      for(jj=0; jj < global.trnrows; jj++)
			{
			  if(global.bias_median[global.totbias-1].time <= (global.trailer[jj].fet))
			    {
			      trailer_fet=global.trailer[jj].fet;
			      next_nframe=0;     
			      break;
			    } 
			}
		      if(next_nframe)
			{
			  headas_chat(CHATTY, "%s: Warning: bias row containing the %d frame\n", global.taskname,global.bias_median[global.totbias-1].ccdframe );
			  headas_chat(CHATTY, "%s: Warning: not found in file %s.\n",  global.taskname,global.par.trfile);
			  goto Compute_Median;
			}
		    }
		}
	      else
		{
		  c_frame_time= (VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
		      
		  if(c_frame_time<=trailer_fet)
		    {
		      if(c_frame_time<global.bias_median[global.totbias-1].time+MAX_NFRAME_TIME)
			{
			  c_frame_npix=IVEC(table,n,hkcol.HPIXCT);
			  
			  if(!((c_frame_npix > table.Multiplicity[hkcol.BiasPx])||( c_frame_npix < global.par.nevents)|| (!c_frame_npix)))
			    {
			      global.totbias++;
			      global.bias_median = (BiasMedianFormat_t *)realloc(global.bias_median, (global.totbias*sizeof(BiasMedianFormat_t)));
			      global.bias_median[global.totbias-1].time = (VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
			      global.bias_median[global.totbias-1].ccdframe =VVEC(table, n, hkcol.CCDFrame) ;
			      global.bias_median[global.totbias-1].wave =BVEC(table, n, hkcol.WAVE) ;
			      global.bias_median[global.totbias-1].npix=IVEC(table,n,hkcol.HPIXCT);
			      
			      median_index=median_length;
			      median_length+=c_frame_npix;
			      median=(int *)realloc(median, median_length*sizeof(int));
			      pix_over_biasth=0;

			      headas_chat(CHATTY,"%s: Info: Frame: %10u :", 
					  global.taskname,
					  global.bias_median[global.totbias-1].ccdframe 
					  );

			      for (jj=0; jj< c_frame_npix; jj++)
				{
				  level=IVECVEC(table,n,hkcol.BiasPx,jj);
				  
				  headas_chat(CHATTY," %4d",level);
				  
				  if (level>global.par.biasth)
				    {
				      headas_chat(CHATTY," (>biasth)");
				      pix_over_biasth++;
				      continue;
				    }

				  if(!median_index)
				    median[median_index+jj-pix_over_biasth]=level;
				  else
				    if(level >= median[median_index+jj-1-pix_over_biasth])
				      median[median_index+jj-pix_over_biasth]=level;
				    else
				      {
					for(iii=median_index+jj-pix_over_biasth; iii >= 0 ; iii--)
					  {
					    if(!iii)
					      median[iii]=level;
					    else
					      if (level < median[iii-1])
						{
						  median[iii]=median[iii-1];
						}
					      else
						{
						  median[iii]=level;
						  iii=0;
						}
					  }
				      }
				}
			      headas_chat(CHATTY,"\n");

			      median_length-=pix_over_biasth;

			      num_frame++;
			      end_nframe=FromRow+n;
			      
			      if((num_frame==global.par.nframe)||(num_frame==ReadRows-1))
			      {
					goto Compute_Median;
				  }
			    }
			  else
			    {
			      goto Compute_Median;
			    }
			}
		      else /* too much distant time */
			{
			  headas_chat(NORMAL, "%s: Warning: median for WT frames between rows %d-%d in file %s\n", global.taskname, start_nframe, end_nframe, global.par.hdfile);
			  headas_chat(NORMAL, "%s: Warning: calculated with only %d frames (exceeded nframe time).\n", global.taskname,num_frame);
			  goto Compute_Median;
			}
		    }
		  else /* not the same bias row */
		    {
		      headas_chat(NORMAL, "%s: Warning: median for WT frames between rows %d-%d in file %s\n", global.taskname, start_nframe, end_nframe, global.par.hdfile);
		      headas_chat(NORMAL, "%s: Warning: calculated with only %d frames (new bias row).\n", global.taskname,num_frame);
		      goto Compute_Median;
		    }
		  
		  /* Read_Next_Frame */
		  continue;
		  
		Compute_Median:

		  headas_chat(CHATTY,"%s: Info: median array for frames between hd rows %i-%i :\n", global.taskname,
			      start_nframe, end_nframe);
		  for(jj=0;jj<median_length;jj++)
		    {
		      headas_chat(CHATTY," %i ", median[jj]);
		    }
		  headas_chat(CHATTY," \n");

		  if (!median_length)
		    {
		      headas_chat(NORMAL,"%s: Error: unable to compute median for frames between hd rows %i-%i (verify 'biasth' value)\n",
				  global.taskname, start_nframe, end_nframe);
		      goto ComputeBiasMedian_end;
		    }

		  if(median_length<global.par.npixels)
		    {
		      headas_chat(NORMAL,"%s: Warning: median for frames between hd rows %i-%i computed with only %i pix pha values\n",
				  global.taskname, start_nframe, end_nframe, median_length);
		    }

		  mod=median_length%2;
		  if(mod)
		    global.bias_median[global.totbias-1].median=(double)(median[(int)(median_length/2)]);
		  else
		    {
		      ind=(int)(median_length/2.0);
		      if(!ind)
			diff_db=(double)median[ind];
		      else
			diff_db=((median[ind] + median[ind - 1])/2.);
		      
		      global.bias_median[global.totbias-1].median=diff_db;
		    }

		  headas_chat(CHATTY,"%s: Info: Median on frames :\n", global.taskname);
		  for(jj=num_frame-1;jj>=0;jj--)
		    {
		      global.bias_median[global.totbias-1-jj].median=global.bias_median[global.totbias-1].median;
		      headas_chat(CHATTY,"%s: Info: Frame: %10u time: %18.8f Median: %9.4f\n", global.taskname,
				  global.bias_median[global.totbias-1-jj].ccdframe, 
				  global.bias_median[global.totbias-1-jj].time, 
				  global.bias_median[global.totbias-1-jj].median);
		    }
		  headas_chat(CHATTY,"%s: Info: Time and Median Frame: %18.8f %9.4f\n", global.taskname, global.bias_median[global.totbias-num_frame].time, global.bias_median[global.totbias-num_frame].median);
		  
		  free(median);
		  newsoft=1;
		  next_nframe=1;

		  if((end_nframe>FromRow)&&(end_nframe<FromRow+ReadRows-global.par.nframe))
		    {
		      n_init=end_nframe+1-FromRow;
		      goto ReadCurrentTable;
		    }
		  else
		    {
		      FromRow=end_nframe+1;
		      goto Next_Block;
		    }

		} /* end else next_frame */
	    }

	  if(((FromRow+n)==table.MaxRows-1)&&(!next_nframe))  /* end of table */
	    {
	      headas_chat(NORMAL, "%s: Warning: median for WT frames between rows %d-%d in file %s\n", global.taskname, start_nframe, end_nframe, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Warning: calculated with only %d frames (end of hd file).\n", global.taskname,num_frame);
	      goto Compute_Median;
	    }
	}
      FromRow += ReadRows;
      
    Next_Block:
      n_init=0;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  if(!newsoft)
    {
      headas_chat(NORMAL, "%s: Warning: Unable to calculate median for all frames,\n", global.taskname);
      headas_chat(NORMAL, "%s: Warning: using %s file\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Warning: nothing to be done.\n", global.taskname);
      global.warning=1;
      return OK;
    }
      
  ReleaseBintable(&head, &table);  
  if( CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto ComputeBiasMedian_end; 
    }
  
  
  return OK;
  
 ComputeBiasMedian_end:
  
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

}/* ComputeBiasMedian */
/*
 *
 *      WTPhaReCorrect
 *
 *	DESCRIPTION:
 *             Routine to correct events PHA
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int ExistsKeyWord(FitsHeader_t, char *, char *);
 *             BOOL GetLVal(FitsHeader_t, char *);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
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
 *        0.1.0: - 09/06/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int WTPhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  int                pha=0, i_bias=0, i_median=0, difftmp=0, amp=0, i_brow=0;
  int                median_int=0, rawx=0, rawx_offset=0, kk=0;
  BOOL               first=1;
  BOOL               newframe=0;
  double             sens=0.0000001, evtime=0.0;
  unsigned           ccdframe_old=0, ccdframe=0;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  Ev2Col_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  
  nCols=outtable.nColumns;
  
  /* Get cols index from name */
  
  /* PHA */
  if((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    { 
      headas_chat(CHATTY, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHA);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
  
  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    { 
      headas_chat(CHATTY, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_Amp);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
  
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
  
  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;      
    }
  
  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
  
  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: re-calcolated PHA.", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);

      if(global.rawxcorr)
	{
	  
	  sprintf(global.strhist, "Used following file(s):");
	  AddHistory(&head, global.strhist);
	  sprintf(global.strhist, "%s WT offset column  file", global.par.colfile);
	  AddHistory(&head, global.strhist);
	}
      
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;
  i_median=0;
  i_bias=0;
  i_brow=0;

  headas_chat(CHATTY, "%s: Info: Computing PHA correction:\n",  global.taskname);

  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  evtime=DVEC(outtable,n,indxcol.TIME_RO);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);
	  amp=BVEC(outtable, n, indxcol.Amp);
	  if(amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	      goto reco_end;
	    }
	  
	  if(first || ccdframe_old != ccdframe)
	    {
	      newframe=1;
	      first=0;
	      ccdframe_old=ccdframe;

	      /* Search median */
	      for (i_median=0; i_median < global.totbias; i_median++)
		{
		  
		  if(ccdframe == global.bias_median[i_median].ccdframe &&  
		     (global.bias_median[i_median].time >= (evtime-sens) && global.bias_median[i_median].time <= (evtime+sens)))
		    {
		      median_int=(int)floor(global.bias_median[i_median].median + 0.5);
		      goto found;
		    }
		}
	      
	      headas_chat(NORMAL, "%s: Error: Row containg the %d frame information\n", global.taskname, ccdframe);
	      headas_chat(NORMAL, "%s: Error: is not been found in %s file.\n", global.taskname, global.par.hdfile);
	      goto reco_end;
	      
	    found:
	      /* Search bias row */
	      for(i_bias=0; i_bias < global.trnrows; i_bias++)
		{
		  if((evtime >=  (global.trailer[i_bias].fst - sens)) && (evtime <= (global.trailer[i_bias].fet + sens)))
		    goto found_2;
		}
	      headas_chat(NORMAL, "%s: Error: %s file does not contain\n",  global.taskname,global.par.trfile);
	      headas_chat(NORMAL, "%s: Error: the bias row of the %d frame.\n", global.taskname, ccdframe);
	      goto reco_end;
	      
	    found_2:
	      if(global.rawxcorr)
		{
		  for (kk=0; kk < global.colrows ; kk++)
		    {
		      if(global.trailer[i_bias].amp == global.wtcol[kk].amp && 
			 global.bias_median[i_median].wave == global.wtcol[kk].wave &&
			 global.trailer[i_bias].first_rawx == global.wtcol[kk].wm1stcol &&
			 global.trailer[i_bias].nbias == global.wtcol[kk].wmcolnum )
			{
			  rawx_offset=global.wtcol[kk].offset;
			  goto found_3;
			}
		    }
	  
		  headas_chat(NORMAL, "%s: Error: %s file does not contain\n",  global.taskname,global.par.colfile);
		  headas_chat(NORMAL, "%s: Error: the RAWX offset for the %d frame.\n", global.taskname, ccdframe);
		  goto reco_end;
		 

		}
	      else
		rawx_offset=0;
	    }
	found_3:

	  if(!stricmp(global.par.biasmode,SP_BIASMODE))
	    {
	      rawx=IVEC(outtable, n, indxcol.RAWX);
	      /* Calculate what bias row column to use */
	      if(amp != global.trailer[i_bias].amp)
		rawx = RAWX_MAX - rawx;
	      
	      i_brow=rawx - (global.trailer[i_bias].first_rawx + rawx_offset);
	      
	      if((i_brow >= 0 ) && (i_brow < global.trailer[i_bias].nbias))
		{
		  difftmp=abs((median_int - global.trailer[i_bias].bias[i_brow]));
		  if(difftmp > global.par.biasdiff)
		    {
		      difftmp=median_int - global.trailer[i_bias].bias[i_brow];
	
		      headas_chat(NORMAL, "%s: Info: ccdframe=%u rawx = %3d trailer bias = %4d frame median = %4d diff = %4d (row %i of input file) --> corrected\n",
				  global.taskname, ccdframe, rawx, global.trailer[i_bias].bias[i_brow], median_int, difftmp, n+FromRow);
		    }
		  else
		    {
		      difftmp=median_int - global.trailer[i_bias].bias[i_brow];
		      headas_chat(NORMAL, "%s: Info: ccdframe=%u rawx = %3d trailer bias = %4d frame median = %4d diff = %4d (row %i of input file) --> not corrected\n",
				  global.taskname, ccdframe, rawx, global.trailer[i_bias].bias[i_brow], median_int, difftmp, n+FromRow);
		      difftmp=0;
		    }
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Error: The %d CCD column\n",  global.taskname,IVEC(outtable, n, indxcol.RAWX));
		  headas_chat(NORMAL, "%s: Error: is not covered by bias row.\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: The bias row limits are: %d -- %d \n", global.taskname,
			      (global.trailer[i_bias].first_rawx + rawx_offset),
			      (global.trailer[i_bias].nbias+(global.trailer[i_bias].first_rawx + rawx_offset)));
		  goto reco_end;
		}
	    }
	  else
	    if(!stricmp(global.par.biasmode,M20P_BIASMODE))
	      {
		
		difftmp=abs((median_int - global.trailer[i_bias].lastbiasmedian));
		if(difftmp > global.par.biasdiff)
		  {
		    difftmp=median_int - global.trailer[i_bias].lastbiasmedian;
		    if(newframe)
		      {
			headas_chat(NORMAL, "%s: Info: ccdframe=%u  trailer median = %4d frame median = %4d diff = %4d  --> corrected\n",
				    global.taskname, ccdframe, global.trailer[i_bias].lastbiasmedian, median_int, difftmp); 
			newframe=0;
		      }
		  }
		else
		  {
		    if(newframe)
		      {
			difftmp=median_int - global.trailer[i_bias].lastbiasmedian;
			headas_chat(NORMAL, "%s: Info: ccdframe=%u  trailer median = %4d frame median = %4d diff = %4d  --> not corrected\n",
				    global.taskname, ccdframe, global.trailer[i_bias].lastbiasmedian, median_int, difftmp); 
			newframe=0;
		      }
		    difftmp=0;
		  }
	      }
	    else
	      {
		headas_chat(NORMAL, "%s: Error: '%s' not a valid parameter for biasmode.\n", global.taskname,global.par.biasmode);
		goto reco_end;
	      }
	  
	  
	  /* Get raw pha value */
	  pha=JVEC(outtable, n, indxcol.PHA);
	  
	  /* Put corrected pha value into PHA column */
	  if(pha != SAT_VAL)
	    {
	      pha=(pha - difftmp);
	      JVEC(outtable, n, indxcol.PHA)=pha;
	    }
	  
	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    }
	  
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);
  
  return OK;
  
 reco_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* LRPhaReCorrect */
/* 
 *
 *  ReadTrFile
 *
 */
int ReadTrFile(void)
{
  int                status=OK, iii=0;
  TrailerCol_t       trcol;
  unsigned           FromRow, ReadRows, n, nCols,ActCols[12];
  Bintable_t	     trtable;
  FitsHeader_t	     trhead;
  FitsFileUnit_t     trunit=NULL;
  int *              lastbias;
  int                lastbiaslength;
  
  /* Open readonly input tr file */
  if ((trunit=OpenReadFitsFile(global.par.trfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.trfile);
      goto reco_end;
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
      goto reco_end;
      
    }
  
  trhead=RetrieveFitsHeader(trunit);
  GetBintableStructure(&trhead, &trtable, BINTAB_ROWS, 0, NULL);
  
  if(!trtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  global.trnrows=trtable.MaxRows;
  global.trailer=(TrailerT_t *)calloc(global.trnrows, sizeof(TrailerT_t));
  
  /* Get needed columns number */
  
  if ((trcol.FrstFST=ColNameMatch(CLNM_FrstFST, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FrstFST);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  /* Frame Start Time */
  if ((trcol.FrstFSTS=ColNameMatch(CLNM_FrstFSTS, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FrstFSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.LastFST=ColNameMatch(CLNM_LastFST, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LastFST);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  
  if ((trcol.LastFSTS=ColNameMatch(CLNM_LastFSTS, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LastFSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.BROW1=ColNameMatch(CLNM_BROW1, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  if ((trcol.BROW1CO=ColNameMatch(CLNM_BROW1CO, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW1CO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.BROW1LEN=ColNameMatch(CLNM_BROW1LEN, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW1LEN);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }

  if ((trcol.BROW1AMP=ColNameMatch(CLNM_BROW1AMP, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW1AMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.BROW2=ColNameMatch(CLNM_BROW2, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  if ((trcol.BROW2CO=ColNameMatch(CLNM_BROW2CO, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW2CO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.BROW2LEN=ColNameMatch(CLNM_BROW2LEN, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW2LEN);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.BROW2AMP=ColNameMatch(CLNM_BROW2AMP, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BROW2AMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  /*
    if ((trcol.WM1STCOL=ColNameMatch(CLNM_WM1STCOL, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WM1STCOL);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  
  if ((trcol.WMCOLNUM=ColNameMatch(CLNM_WMCOLNUM, &trtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WMCOLNUM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.trfile);
      goto reco_end;
    }
  */

  
  EndBintableHeader(&trhead, &trtable);
  
  FromRow = 1;
  ReadRows=trtable.nBlockRows;
  
  nCols=12;
  ActCols[0]=trcol.FrstFST;
  ActCols[1]=trcol.FrstFSTS;
  ActCols[2]=trcol.LastFST;
  ActCols[3]=trcol.LastFSTS;
  ActCols[4]=trcol.BROW1; 
  ActCols[5]=trcol.BROW1CO;
  ActCols[6]=trcol.BROW1LEN;
  ActCols[7]=trcol.BROW1AMP;
  ActCols[8]=trcol.BROW2; 
  ActCols[9]=trcol.BROW2CO;
  ActCols[10]=trcol.BROW2LEN;
  ActCols[11]=trcol.BROW2AMP;
  /*  ActCols[12]=trcol.WM1STCOL;
      ActCols[13]=trcol.WMCOLNUM;
  */
  /* Read input bintable and compute ranges time-tag */
  while (ReadBintable(trunit, &trtable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
    
      for(n=0; n<ReadRows ; ++n)
	{

	  global.trailer[n].fst=(VVEC(trtable, n, trcol.FrstFST) + ((20.*UVEC(trtable, n, trcol.FrstFSTS))/1000000.));
	  global.trailer[n].fet=(VVEC(trtable, n, trcol.LastFST) + ((20.*UVEC(trtable, n, trcol.LastFSTS))/1000000.));

	  /*	  global.trailer[n].cols=UVEC(trtable, n, trcol.WMCOLNUM);	  
		  global.trailer[n].firstcol=UVEC(trtable, n, trcol.WM1STCOL);	  
	  */
	  if(BVEC(trtable, n, trcol.BROW1AMP))
	    {
	      global.trailer[n].amp=BVEC(trtable, n, trcol.BROW1AMP);
	      global.trailer[n].nbias=UVEC(trtable, n, trcol.BROW1LEN);
	      global.trailer[n].first_rawx=UVEC(trtable, n, trcol.BROW1CO);
	      for (iii=0; iii < MAX_WINDOW_LEN; iii++)
		{
		  if(iii < global.trailer[n].first_rawx)
		    global.trailer[n].bias[iii]=UVECVEC(trtable,n,trcol.BROW1,iii);
		  else
		    global.trailer[n].bias[iii]=0;
		}
	    }
	  else if(BVEC(trtable, n, trcol.BROW2AMP))
	    {
	      global.trailer[n].amp=BVEC(trtable, n, trcol.BROW2AMP);
	      global.trailer[n].nbias=UVEC(trtable, n, trcol.BROW2LEN);
	      global.trailer[n].first_rawx=UVEC(trtable, n, trcol.BROW2CO);
	      for (iii=0; iii < MAX_WINDOW_LEN; iii++)
		{
		  if(iii < global.trailer[n].first_rawx)
		    global.trailer[n].bias[iii]=UVECVEC(trtable,n,trcol.BROW2,iii);
		  else
		    global.trailer[n].bias[iii]=0;
		}
	    }
	
	  headas_chat(CHATTY,"%s: Info: Bias row: %18.8f %18.8f:\n", global.taskname,
		      global.trailer[n].fst, global.trailer[n].fet);
	  
	  for( iii=0; iii<global.trailer[n].nbias;iii++)
	    headas_chat(CHATTY,"%4d ", global.trailer[n].bias[iii]);
	  
	  headas_chat(CHATTY,"\n");
  
	  if(global.trailer[n].nbias>20)
	    lastbiaslength=20;
	  else
	    lastbiaslength=global.trailer[n].nbias;
	  
	  lastbias=(int *)malloc(lastbiaslength*sizeof(int));
	  for (iii=0; iii<lastbiaslength; iii++ )
	    {
	      lastbias[iii] = global.trailer[n].bias[global.trailer[n].nbias-lastbiaslength+iii];
	    }
	  global.trailer[n].lastbiasmedian = CalcMedian(lastbias, lastbiaslength);
	  
	  headas_chat(CHATTY," ==>>  Median last 20 pix : %i\n", global.trailer[n].lastbiasmedian);
	  headas_chat(CHATTY,"%s: Info: Time and Median Bias Row: %18.8f %i\n", global.taskname, global.trailer[n].fet, global.trailer[n].lastbiasmedian);
	  free(lastbias);
	  
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  ReleaseBintable(&trhead, &trtable);
  

  return OK;
  
  
 reco_end:
  return NOT_OK;

}

/*
 *
 *  CalcMedian
 *
 *  DESCRIPTION:
 *    Routine to Calculate the median value for array of int
 *
 *
 */
int CalcMedian(int * bias, int length) 
{
  int level, mod=0, jj, iii;
  int * median;
  double medianval,diff_db;

  median=(int *)malloc(length*sizeof(int));

  for (jj=0; jj< length; jj++)
    {
      level=bias[jj];
      
      if(jj==0)
	median[jj]=level;
      else 
	if(level >= median[jj-1])
	  median[jj]=level;
	else
	  {
	    for(iii=jj; iii >= 0 ; iii--)
	      {
		if(!iii)
		  median[iii]=level;
		else 
		  if (level < median[iii-1])
		    {
		      median[iii]=median[iii-1];
		    }
		  else
		    {
		      median[iii]=level;
		      iii=0;
		    }
	      }
	  }
    }

  mod = length%2;
  if (mod)
    {
      medianval = (double) (median[(int)(length/2.0)] );
    }
  else
    {
      int ind;
      ind=(int)(length/2.0);
      if(!ind)
	diff_db=(double)median[ind];
      else
	diff_db=((median[ind] + median[ind - 1])/2.);
      medianval=diff_db;
    }
  
  free(median);
  return (int)floor(medianval + 0.5);

} /* ComputeMedian */
