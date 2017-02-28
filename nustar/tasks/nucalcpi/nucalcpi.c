/*
 * 
 *	nucalcpi.c
 *
 *	INVOCATION:
 *
 *		nucalcpi [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine for the conversion of the charge of each event from electronic units ("Pulse Height Amplitude", PHA)
 *              into energy ("Pulse Invariant", PI) units
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 03/02/11 - First version
 *        0.1.1 - NS 28/02/11 - Added 'temperature' input parameter
 *                            - Modified handling of evt with TIME not included in HK input file
 *        0.1.2 - NS 03/08/11 - PI values for events with grade>12 (excluded grade 26,26,28 and 29) are set to NULL
 *                            - Events with grade=[26|27|28|29] are treated as grade=0 events in PI computation 
 *        0.1.3 - NS 10/10/11 - Bug fixed when 'outfile' equal to 'infile' parameter
 *        0.1.4 - NS 21/10/11 - Add 'TLMIN*' and 'TLMAX*' keywords for PI column
 *        0.1.5 - NS 15/11/11 - Modified 'GetTemperatures' function to improve performances
 *        0.1.6 - NS 17/11/11 - Fixed bug in 64-bit architectures
 *        0.1.7 - NS 15/02/12 - Handle charge loss correction for PI computation
 *                            - Added 'clcfile' and 'clcfilterfile' input parameters
 *        0.1.8 - NS 11/05/12 - Added consistence check of input files
 *        0.1.9 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.2.0 - NS 15/11/12 - Bug fixed in 'GetTemperatures' routine
 *        0.2.1 - BG 07/04/14 - Adjusted how grade-gain is applied for grade 0 and grades 9-20
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nucalcpi  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nucalcpi.h"


/********************************/
/*         definitions          */
/********************************/

#define NUCALCPI_C
#define NUCALCPI_VERSION      "0.2.1"
#define PRG_NAME               "nucalcpi"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nucalcpi_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nucalcpi.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nucalcpi_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 03/02/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpi_getpar()
{
  
  /* Input Event File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }
  
  /* Output Event File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  /* Input HK File Name */
  if(PILGetFname(PAR_HKFILE, global.par.hkfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HKFILE);
      goto Error;	
    }
  
  if ( !strcasecmp(global.par.hkfile,DF_NONE) )
    {
      /* Input temperature */
      if(PILGetReal(PAR_TEMPERATURE, &global.par.temperature))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TEMPERATURE);
	  goto Error;	
	}
    }

  /* Input GAIN File Name */
  if(PILGetFname(PAR_GAINFILE, global.par.gainfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GAINFILE);
      goto Error;	
    }
  
  if(PILGetFname(PAR_CLCFILE, global.par.clcfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLCFILE);
      goto Error;	
    }
  
  if(PILGetFname(PAR_CLCFILTERFILE, global.par.clcfilterfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLCFILTERFILE);
      goto Error;
    }


  get_history(&global.hist);
  nucalcpi_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nucalcpi_getpar */


/*
 *	nucalcpi_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nucalcpi_checkinput();
 *             int headas_chat(int , char *, ...);
 *             int strcasecmp(const char *s1, const char *s2);
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *             FitsHeader_t  RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *             FitsFileUnit_t OpenWriteFitsFile(char *name);
 *             int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, int *status );
 *             int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status);
 *             int HDpar_stamp(fitsfile *fptr, int hdunum);
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int RenameFile (char *, char *);
 *             int CopyFile(char *source, char *destination);
 *             int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, 
 *                                char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);
 *	
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 03/02/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpi_work()
{
  int            status = OK, inExt, outExt, evExt;
  FitsHeader_t   head;
  FitsCard_t     *card;
  FitsFileUnit_t inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 

  if(nucalcpi_checkinput())
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

  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  /* Retrieve INSTRUME from input event file */  
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(global.evt.instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
 
  /* Retrieve date-obs and time-obs from input events file  */
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

  /* Retrieve date-end and time-end from input events file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      global.evt.dateend=card->u.SVal;
      if(!(strchr(global.evt.dateend, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
	    global.evt.timeend=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEEND);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      global.evt.tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Retrieve observation end time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      global.evt.tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
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


  /* Compute PI */
  if (ComputePHAtoPI(inunit, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute PI values.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;

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
  if (RenameFile(global.tmpfile,global.par.outfile) == -1)
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

  return OK; 
  
 Error:

  return NOT_OK;
} /* nucalcpi_work */


/*
 *	nucalcpi
 *
 *	DESCRIPTION:
 *                 main function
 *               
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *             void GetNuSTARDASVersion(Version_t verstr);
 *             void nucalcpi_getpar(void);
 * 	       void nucalcpi_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 03/02/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpi()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUCALCPI_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nucalcpi_getpar() == OK) 
    {
      
      if ( nucalcpi_work()) 
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
      
    }
  
  return OK;
  
 pdcorr_end:
  
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;
  
} /* nucalcpi */


/*
 *	nucalcpi_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 03/02/11 - First version
 *
 */
void nucalcpi_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the input HK Header file                      :'%s'\n",global.par.hkfile);
  headas_chat(NORMAL,"Name of the input GAIN file                           :'%s'\n",global.par.gainfile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nucalcpi_info */


/*
 *	nucalcpi_checkinput
 *
 *	DESCRIPTION:
 *         Check input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *               int strcasecmp(const char *s1, const char *s2);
 *               GetFilenameExtension(char *, char *);
 *               int strcmp(const char *s1, const char *s2);
 *               FileExists(char *);
 *               remove(char *);
 *               pid_t getpid(void);
 *               int sprintf(char *str, const char *format, ...);
 *               char *DeriveFileName(const char *OldName, char *NewName, const char *ext);
 *               char *strcpy(char *, char *);
 *               
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 03/02/11 - First version
 *
 */
int nucalcpi_checkinput(void)
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
	      if( strcmp(global.par.outfile,global.par.infile) ){
		if(remove (global.par.outfile) == -1)
		  {
		    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
		    goto check_end;
		  }  
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
 
  return OK;

 check_end:
  return NOT_OK;
}


/*
 *
 *      ComputePHAtoPI
 *
 *	DESCRIPTION:
 *           Routine to compute PI values and put them into EVENTS bintable.
 *                            
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *               FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *               void GetBintableStructure (FitsHeader_t *header, Bintable_t *table,
 *     			                    const int MaxBlockRows, const unsigned nColumns,
 *  					    const unsigned ActCols[]);
 *               int ColNameMatch(const char *ColName, const Bintable_t *table);
 *               void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			        const char *TypeComment, const char *tform, const unsigned present, ...);
 *               void GetGMTDateTime(char *);
 *               int sprintf(char *str, const char *format, ...);
 *               void AddHistory(FitsHeader_t *header, const char *Comment);
 *               void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *               void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *               int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				   const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *               int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *               unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *               int fits_get_colnum(FitsFileUnit_t unit, int casesen, char *templt, int *colnum,
 *                                   int *status);
 *               int fits_delete_col(FitsFileUnit_t unit, int colnum, int *status);
 *               
 *
 *      CHANGE HISTORY:
 *        0.1.0: - NS 03/02/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputePHAtoPI(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                rawx, rawy, det_id, grade, px2_grade, px3_grade;
  int                totrows;
  int                jj=0, x, y, ipix2, ipix3;
  int                neigh_x[] = {NEIGH_X};
  int                neigh_y[] = {NEIGH_Y};
  JTYPE              jval;
  char               KeyWord[FLEN_KEYWORD];
  JTYPE              null_pi=KWVL_PINULL;
  double             time, oldtime=-1;
  float              hktemp[4];
  double             slope, offset, surrpi;
  double             eng1=0., eng2=0., eng3=0., clcval, gr_slopeval, gr_offsetval;
  double             ata1, ata2, ata3, cor1, cor2, cor3, rr1, rr2, rr3, pi_clc, dpi;
  EvtCol_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;

  HKRow_t            *hkinfo=NULL;
  int                hknrows=0;
  ClcFilterInfo_t   *clcfilterinfo=NULL;
  int                clcfiltercount;
  GainData_t         gaindata[4][DET_PIXS][DET_ROWS];
  ClcData_t          clcdata[4][DET_PIXS][DET_ROWS]; 


  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  

  if ( strcasecmp(global.par.hkfile,DF_NONE) ){
    /* Read Detector Temperatures from HK input file */
    if( ReadTemperatures(&hkinfo, &hknrows) )
      {
	headas_chat(NORMAL, "%s: Error: Unable to read detector temperatures\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hkfile);
	goto pha2pi_end;
      }
  }
  else{
    /* set Detector Temperatures to 'temperature' input parameter */
    hkinfo = (HKRow_t *)malloc(sizeof(HKRow_t));
    hknrows = 1;
    hkinfo->time = 0;
    hkinfo->czt0temp = (float)global.par.temperature;
    hkinfo->czt1temp = (float)global.par.temperature;
    hkinfo->czt2temp = (float)global.par.temperature;
    hkinfo->czt3temp = (float)global.par.temperature;
  }

  /* Read Gain Coefficients from input gainfile */
  if( ReadGainFile(gaindata) )
    {
      goto pha2pi_end;
    }

  /* Read Charge loss correction data from input clcfile */
  if( ReadClcFile(clcdata) )
    {
      goto pha2pi_end;
    }

  /* Retrieve clc filter data from input clcfilterfile */
  if(ReadClcFilterFile(&clcfilterinfo, &clcfiltercount))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read input charge loss correction filter file.\n", global.taskname);
      goto pha2pi_end;
    }


  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  
  nCols=outtable.nColumns;
  totrows=outtable.MaxRows;


  /* TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* RAWX */
  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* RAWY */
  if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* DET_ID */
  if ((indxcol.DET_ID=ColNameMatch(CLNM_DET_ID, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_DET_ID);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* GRADE */
  if ((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* PHAS */
  if ((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* SWTRIG */
  if ((indxcol.SWTRIG=ColNameMatch(CLNM_SWTRIG, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_SWTRIG);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto pha2pi_end;
    }

  /* Add new columns if needed */

  if((indxcol.PIS_GAIN=ColNameMatch(CLNM_PIS_GAIN, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PIS_GAIN);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);  
      AddColumn(&head, &outtable, CLNM_PIS_GAIN, CARD_COMM_PIS_GAIN, "9E", TUNIT, UNIT_CHANN, CARD_COMM_PHYSUNIT);
      indxcol.PIS_GAIN=ColNameMatch(CLNM_PIS_GAIN, &outtable);
    }

  if((indxcol.SURRPI=ColNameMatch(CLNM_SURRPI, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_SURRPI);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_SURRPI, CARD_COMM_SURRPI, "1J", TNONE);
      indxcol.SURRPI=ColNameMatch(CLNM_SURRPI, &outtable);
    }

  if((indxcol.PI_CLC=ColNameMatch(CLNM_PI_CLC, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_CLC);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_PI_CLC, CARD_COMM_PI_CLC, "1E", TNONE);
      indxcol.PI_CLC=ColNameMatch(CLNM_PI_CLC, &outtable);
    }

  if((indxcol.PI=ColNameMatch(CLNM_PI, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);  
      AddColumn(&head, &outtable, CLNM_PI, CARD_COMM_PI, "1J", TUNIT, UNIT_CHANN, CARD_COMM_PHYSUNIT);
      indxcol.PI=ColNameMatch(CLNM_PI, &outtable);
    }
  sprintf(KeyWord, "TNULL%d", (indxcol.PI+1));
  AddCard(&head, KeyWord, J, &null_pi, CARD_COMM_TNULL);
  sprintf(KeyWord, "TLMIN%d", (indxcol.PI+1));
  jval = PI_MIN;
  AddCard(&head, KeyWord, J, &jval, "Minimum value for PI column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.PI+1));
  jval = PI_MAX;
  AddCard(&head, KeyWord, J, &jval, "Maximum value for PI column");


  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: computed PI.", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
      
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);



  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;

  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  time = DVEC(outtable,n,indxcol.TIME);
	  rawx = BVEC(outtable,n,indxcol.RAWX);
	  rawy = BVEC(outtable,n,indxcol.RAWY);
	  det_id = BVEC(outtable,n,indxcol.DET_ID);
	  grade = IVEC(outtable,n,indxcol.GRADE);

	  /* Check input data */
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto pha2pi_end;
	  }
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Error: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    goto pha2pi_end;
	  }

	  /* Get detectors temperatures for a new 'time' value */
	  if(!(time>=oldtime-TIME_SENS && time<=oldtime+TIME_SENS)){

	    if(GetTemperatures(time, hkinfo, hknrows, hktemp))
	      goto pha2pi_end;

	    oldtime=time;
	  }


	  /* Compute PIS_GAIN value */

	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      x = rawx + neigh_x[jj];
	      y = rawy + neigh_y[jj];

	      if(x<RAWX_MIN||x>RAWX_MAX||y<RAWY_MIN||y>RAWY_MAX){
		EVECVEC(outtable,n,indxcol.PIS_GAIN,jj) = 0.0 ;
	      }
	      else{

		/* Get slope and offset values for the current pixel */
		if( GetGainCoeff(&gaindata[det_id][x][y], time, hktemp[det_id], &slope, &offset) ){   
		  headas_chat(NORMAL,"%s: Error: Unable to get gain coefficients for TIME=%f RAWX=%d RAWY=%d DET_ID=%d TEMP=%f .\n", 
			      global.taskname, time, x, y, det_id, hktemp[det_id]);
		  goto pha2pi_end;
		}

		headas_chat(CHATTY,"%s: Info: TIME=%f RAWX=%d RAWY=%d DET_ID=%d TEMP=%f => SLOPE=%f OFFSET=%f.\n", 
			    global.taskname, time, x, y, det_id, hktemp[det_id], slope, offset);

		if(EVECVEC(outtable,n,indxcol.PHAS,jj)>-PHAS_SENS && EVECVEC(outtable,n,indxcol.PHAS,jj)<PHAS_SENS )
		  EVECVEC(outtable,n,indxcol.PIS_GAIN,jj) = 0.0;
		else
		  EVECVEC(outtable,n,indxcol.PIS_GAIN,jj) = EVECVEC(outtable,n,indxcol.PHAS,jj) * slope + offset ;

	      }

	    }

	  /* Compute SURRPI values */

	  surrpi = 0.0;
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      if(!BVECVEC(outtable,n,indxcol.SWTRIG,jj))
		surrpi += EVECVEC(outtable,n,indxcol.PIS_GAIN,jj);
	    }

	  if( GetClcData(&clcdata[det_id][rawx][rawy], time, 0, &clcval, &gr_offsetval, &gr_slopeval) ){   
	    headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			global.taskname, time, rawx, rawy, grade, det_id);
	    goto pha2pi_end;
	  }

	  surrpi += gr_offsetval;
	  
	  JVEC(outtable,n,indxcol.SURRPI) = surrpi>0 ? (JTYPE)(surrpi+0.5) : (JTYPE)(surrpi-0.5); 


	  /* Compute PI_CLC and PI values */

	  /* ----------------------------- CASE GRADE=0 ------------------------------------------------- */
	  if(grade==0)
	    {
	      /* Get CLC value for the current pixel */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, grade, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, grade, det_id);
		goto pha2pi_end;
	      }

            eng1 = EVECVEC(outtable,n,indxcol.PIS_GAIN,4);
            dpi = gr_offsetval + eng1*gr_slopeval ;
            
            
            JVEC(outtable,n,indxcol.PI) = (JTYPE)( dpi +0.5 );
	    }

	  /* ----------------------------- CASE GRADE=[1,2,3,4] ----------------------------------------- */
	  else if(grade==1||grade==2||grade==3||grade==4)
	    {
	      eng1 = EVECVEC(outtable,n,indxcol.PIS_GAIN,4);

	      switch (grade) {  
	      case 1:
		eng2 = EVECVEC(outtable,n,indxcol.PIS_GAIN,1);
		break;
	      case 2:
		eng2 = EVECVEC(outtable,n,indxcol.PIS_GAIN,5);
		break;		
	      case 3:
		eng2 = EVECVEC(outtable,n,indxcol.PIS_GAIN,7);
		break;
	      case 4:
		eng2 = EVECVEC(outtable,n,indxcol.PIS_GAIN,3);
		break;
	      default:
		goto pha2pi_end;
	      }

	      /* Get CLC value for the current pixel */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, grade, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, grade, det_id);
		goto pha2pi_end;
	      }

	      if(ApplyEnCorrection(clcfilterinfo, clcfiltercount, (float)eng2))
		{
		  ata1 = atan(eng2/eng1);
		  cor1 = sin(ata1*2.0)*clcval +1;
		  rr1 = sqrt(eng1*eng1 + eng2*eng2);
		  eng1 = rr1*cor1*cos(ata1);
		  eng2 = rr1*cor1*sin(ata1);

		  headas_chat(CHATTY,"%s: Info: TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d ata1=%f cor1=%f rr1=%f eng1=%f eng2=%f clcval=%f gr_offsetval=%f gr_slopeval=%f\n",
			      global.taskname,time, rawx, rawy, grade, det_id, ata1, cor1, rr1, eng1, eng2, clcval, gr_offsetval, gr_slopeval);
		}
	      else
		{
		  headas_chat(CHATTY,"%s: Info: TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d eng1=%f eng2=%f clcval=%f gr_offsetval=%f gr_slopeval=%f  (Fluorescence event)\n",
			      global.taskname,time, rawx, rawy, grade, det_id, eng1, eng2, clcval, gr_offsetval, gr_slopeval);
		}
	      
	      pi_clc = eng1 + eng2;
	      EVEC(outtable,n,indxcol.PI_CLC) = (ETYPE) pi_clc;

	      dpi = gr_offsetval + gr_slopeval*pi_clc;
	      if(isnan(dpi))
		JVEC(outtable,n,indxcol.PI) = KWVL_PINULL;		
	      else
		JVEC(outtable,n,indxcol.PI) = (JTYPE)(dpi+0.5);
	    }

	  /* ----------------------------- CASE GRADE=[5,6,7,8] ----------------------------------------- */
	  else if(grade==5||grade==6||grade==7||grade==8)
	    {
	      
	      switch (grade) {  
	      case 5:
		ipix2 = 1;
		ipix3 = 5;
		break;
	      case 6:
		ipix2 = 5;
		ipix3 = 7;
		break;
	      case 7:
		ipix2 = 7;
		ipix3 = 3;
		break;
	      case 8:
		ipix2 = 3;
		ipix3 = 1;
		break;
	      default:
		goto pha2pi_end;
	      }

	      eng1 = EVECVEC(outtable,n,indxcol.PIS_GAIN,4);
	      eng2 = EVECVEC(outtable,n,indxcol.PIS_GAIN,ipix2);
	      eng3 = EVECVEC(outtable,n,indxcol.PIS_GAIN,ipix3);


	      switch (ipix2) {  
	      case 1:
		px2_grade = 1;
		break;
	      case 3:
		px2_grade = 4;		
		break;
	      case 5:
		px2_grade = 2;		
		break;
	      case 7:
		px2_grade = 3;	
		break;
	      default:
		goto pha2pi_end;
	      }

	      switch (ipix3) {  
	      case 1:
		px3_grade = 1;
		break;
	      case 3:
		px3_grade = 4;		
		break;
	      case 5:
		px3_grade = 2;		
		break;
	      case 7:
		px3_grade = 3;	
		break;
	      default:
		goto pha2pi_end;
	      }


	      /* Get CLC value for pixel 2 */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, px2_grade, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, px2_grade, det_id);
		goto pha2pi_end;
	      }

	      ata2 = atan(eng2/(eng1+eng3));
	      cor2 = sin(ata2*2.0)*clcval +1;
	      rr2 = sqrt((eng1+eng3)*(eng1+eng3) + eng2*eng2);
	      eng2 = rr2*cor2*sin(ata2);

	      headas_chat(CHATTY,"%s: Info: TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d ata2=%f cor2=%f rr2=%f eng2=%f clcval=%f gr_offsetval=%f gr_slopeval=%f\n",
			  global.taskname,time, rawx, rawy, grade, det_id, ata2, cor2, rr2, eng2, clcval, gr_offsetval, gr_slopeval);

	      /* Get CLC value for pixel 3 */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, px3_grade, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, px3_grade, det_id);
		goto pha2pi_end;
	      }

	      ata3 = atan(eng3/(eng1+eng2));
	      cor3 = sin(ata3*2.0)*clcval +1;
	      rr3 = sqrt((eng1+eng2)*(eng1+eng2) + eng3*eng3);
	      eng3 = rr3*cor3*sin(ata3);

	      headas_chat(CHATTY,"%s: Info: TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d ata3=%f cor3=%f rr3=%f eng3=%f clcval=%f gr_offsetval=%f gr_slopeval=%f\n",
			  global.taskname,time, rawx, rawy, grade, det_id, ata3, cor3, rr3, eng3, clcval, gr_offsetval, gr_slopeval);

	      pi_clc = eng1 + eng2 + eng3;
	      EVEC(outtable,n,indxcol.PI_CLC) = (ETYPE) pi_clc;

	      /* Get CLC value for the current pixel */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, grade, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, grade, det_id);
		goto pha2pi_end;
	      }  
	      
	      dpi = gr_offsetval + gr_slopeval*pi_clc;
	      if(isnan(dpi))
		JVEC(outtable,n,indxcol.PI) = KWVL_PINULL;		
	      else
		JVEC(outtable,n,indxcol.PI) = (JTYPE)(dpi+0.5);

	    }

/*   	  /\* ----------------------------- CASE GRADE=[9,10,11,12] -------------------------------------- *\/ */
/* 	  else if(grade==9||grade==10||grade==11||grade==12) */
/* 	    { */
/* 	      dpi = 0.0; */
/* 	      for (jj=0; jj< PHAS_MOL; jj++) */
/* 		{ */
/* 		  if(BVECVEC(outtable,n,indxcol.SWTRIG,jj)) */
/* 		    dpi += EVECVEC(outtable,n,indxcol.PIS_GAIN,jj); */
/* 		} */

/* 	      JVEC(outtable,n,indxcol.PI) = (JTYPE)(dpi+0.5); */
/* 	    } */

  	  /* ----------------------------- CASE GRADE>=9 && GRADE =<20 ------------------------------------------------ */
	  else if(grade>=9&&grade<=20)
	    {
	      /* Get CLC value for the current pixel in the GRADE==9 slot, which is used for all high-grade events.
		 Here, we just sum over all of the PI_GAINS > SWTRIG */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, 9, &clcval, &gr_offsetval, &gr_slopeval) ){
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, grade, det_id);
		goto pha2pi_end;
	      }

	      dpi = 0.0;
	      for (jj=0; jj< PHAS_MOL; jj++)
		{
		  if(BVECVEC(outtable,n,indxcol.SWTRIG,jj))
		    dpi += EVECVEC(outtable,n,indxcol.PIS_GAIN,jj);
		}

	      JVEC(outtable,n,indxcol.PI) = (JTYPE)(dpi*gr_slopeval+gr_offsetval +0.5);
	    }
	  /* ------------------------------ GRADE>=21 ---------------------------------------------------------------- */
	  else 
	    {
	      /* Get CLC value for the current pixel */
	      if( GetClcData(&clcdata[det_id][rawx][rawy], time, 0, &clcval, &gr_offsetval, &gr_slopeval) ){   
		headas_chat(NORMAL,"%s: Error: Unable to get charge loss correction coefficient for TIME=%f RAWX=%d RAWY=%d GRADE=%d DET_ID=%d .\n", 
			    global.taskname, time, rawx, rawy, grade, det_id);
		goto pha2pi_end;
	      }

	      dpi = 0.0;
	      for (jj=0; jj< PHAS_MOL; jj++)
		{
		  if(BVECVEC(outtable,n,indxcol.SWTRIG,jj))
		    dpi += EVECVEC(outtable,n,indxcol.PIS_GAIN,jj);
		}

	      JVEC(outtable,n,indxcol.PI) = (JTYPE)(dpi+gr_offsetval +0.5);
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
  
 pha2pi_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* ComputePHAtoPI */


/*
 *
 *      ReadTemperatures
 *
 *	DESCRIPTION:
 *           Routine to get detectors temperatures from HK input file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadTemperatures(HKRow_t **hkinfo, int *hknrows){

  unsigned           FromRow, ReadRows, n, nCols, ActCols[5];
  int                hkcount=0, status=OK;
  double             hktstart=0, hktstop=0;
  char               hkinstrume[FLEN_VALUE];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsCard_t         *card;
  FitsFileUnit_t     hkunit=NULL;



  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hkfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hkfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }
 
  /* Move in HK4FPM extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_HK4FPM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_HK4FPM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hkfile); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hkfile);
	}
      goto ReadTemperatures_end;
    }
  
  
  hkhead=RetrieveFitsHeader(hkunit);

  /* Retrieve TSTART */
  if((ExistsKeyWord(&hkhead, KWNM_TSTART, &card)))
    {
      hktstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.hkfile);
    }

  /* Retrieve TSTOP */
  if((ExistsKeyWord(&hkhead, KWNM_TSTOP, &card)))
    {
      hktstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.hkfile);
    }

  /* Retrieve INSTRUME */  
  if(ExistsKeyWord(&hkhead, KWNM_INSTRUME, &card))
    {
      strcpy(hkinstrume, card->u.SVal);
    }
  else
    {
      strcpy(hkinstrume, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.hkfile);
    }

  /* Check if hkfile is appropriate for input event file */

  if( global.evt.tstart<hktstart || global.evt.tstop>hktstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, global.par.hkfile);

  if( strcasecmp(global.evt.instrume,hkinstrume) )
    headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file and event file are not consistent\n", global.taskname, global.par.hkfile);


  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }


  /* Get needed columns number from name */

  /* Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }

  /* CZT0TEMP */
  if ((hkcol.CZT0TEMP=ColNameMatch(CLNM_CZT0TEMP, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CZT0TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }
  /* CZT1TEMP */
  if ((hkcol.CZT1TEMP=ColNameMatch(CLNM_CZT1TEMP, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CZT1TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }

  /* CZT2TEMP */
  if ((hkcol.CZT2TEMP=ColNameMatch(CLNM_CZT2TEMP, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CZT2TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }
  /* CZT3TEMP */
  if ((hkcol.CZT3TEMP=ColNameMatch(CLNM_CZT3TEMP, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CZT3TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hkfile);
      goto ReadTemperatures_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following columns */
  nCols=5; 
  ActCols[0]=hkcol.TIME;
  ActCols[1]=hkcol.CZT0TEMP;
  ActCols[2]=hkcol.CZT1TEMP;
  ActCols[3]=hkcol.CZT2TEMP;
  ActCols[4]=hkcol.CZT3TEMP;

  while(ReadBintable(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  
	  if(!hkcount)
	    {
	      hkcount++;
	      *hkinfo = (HKRow_t *)malloc(sizeof(HKRow_t));
	    }
	  else
	    {
	      hkcount++;
	      *hkinfo = (HKRow_t *)realloc(*hkinfo, (hkcount*sizeof(HKRow_t)));
	    }
	  
	  (*hkinfo)[hkcount-1].time = DVEC(hktable,n,hkcol.TIME);
	  (*hkinfo)[hkcount-1].czt0temp = EVEC(hktable,n,hkcol.CZT0TEMP);
	  (*hkinfo)[hkcount-1].czt1temp = EVEC(hktable,n,hkcol.CZT1TEMP);
	  (*hkinfo)[hkcount-1].czt2temp = EVEC(hktable,n,hkcol.CZT2TEMP);
	  (*hkinfo)[hkcount-1].czt3temp = EVEC(hktable,n,hkcol.CZT3TEMP);

	  headas_chat(CHATTY, "%s: Info: HK time=%f czt0temp=%f czt1temp=%f czt2temp=%f czt3temp=%f \n",global.taskname,(*hkinfo)[hkcount-1].time,(*hkinfo)[hkcount-1].czt0temp,(*hkinfo)[hkcount-1].czt1temp,(*hkinfo)[hkcount-1].czt2temp,(*hkinfo)[hkcount-1].czt3temp);

	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  
  if(!hkcount){
    headas_chat(NORMAL, "%s: Error: No data available in '%s' file\n", global.taskname, global.par.hkfile);
    headas_chat(NORMAL, "%s: Error: for the input event file\n", global.taskname);
    goto ReadTemperatures_end;
  }
  
  *hknrows = hkcount;


  return OK;
  
 ReadTemperatures_end:
  if (hkhead.first)
    ReleaseBintable(&hkhead, &hktable);
  
  return NOT_OK;

 
}  /* ReadTemperatures */


/*
 *
 *      ReadGainFile
 *
 *	DESCRIPTION:
 *           Routine to read input gainfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadGainFile(GainData_t gaindata[4][DET_PIXS][DET_ROWS]){
  

  char        gainfile0[PIL_LINESIZE], gainfile1[PIL_LINESIZE], gainfile2[PIL_LINESIZE], gainfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB gain filename */  
  if ( !strcasecmp(global.par.gainfile,DF_CALDB) )
    {
      /* Retrieve DET0 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_GAIN_DSET, gainfile0, HD_EXPR, &extfile0, global.evt.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for gainfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadGainFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_GAIN_DSET, gainfile1, HD_EXPR, &extfile1, global.evt.instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for gainfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadGainFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_GAIN_DSET, gainfile2, HD_EXPR, &extfile2, global.evt.instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for gainfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadGainFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_GAIN_DSET, gainfile3, HD_EXPR, &extfile3, global.evt.instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for gainfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadGainFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(gainfile0, global.par.gainfile);
    strcpy(gainfile1, global.par.gainfile);
    strcpy(gainfile2, global.par.gainfile);
    strcpy(gainfile3, global.par.gainfile);
  }

  /* Retrieve DET0 gain info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for gain data of detector %s.\n", global.taskname, gainfile0, KWVL_DETNAM_DET0);
  if( ReadGainInfo(gaindata[0], gainfile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read gain coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, gainfile0);
      goto ReadGainFile_end;
    }

  /* Retrieve DET1 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for gain data of detector %s.\n", global.taskname, gainfile1, KWVL_DETNAM_DET1);
  if( ReadGainInfo(gaindata[1], gainfile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read gain coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, gainfile1);
      goto ReadGainFile_end;
    }

  /* Retrieve DET2 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for gain data of detector %s.\n", global.taskname, gainfile2, KWVL_DETNAM_DET2);
  if( ReadGainInfo(gaindata[2], gainfile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read gain coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, gainfile2);
      goto ReadGainFile_end;
    }

  /* Retrieve DET3 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for gain data of detector %s.\n", global.taskname, gainfile3, KWVL_DETNAM_DET3);
  if( ReadGainInfo(gaindata[3], gainfile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read gain coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, gainfile3);
      goto ReadGainFile_end;
    }



  return OK;
  
 ReadGainFile_end:
  
  return NOT_OK;

}


/*
 *
 *      ReadGainInfo
 *
 *	DESCRIPTION:
 *           Routine to get gain coefficient from input gainfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadGainInfo(GainData_t gaindata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam){

  int                n, i=0, j=0, status=OK, found=NOT_OK, newdata=0;
  int                inExt, totExt, rawx, rawy;
  double             time;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  GainCol_t          gaincol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      gaindata[i][j].ninfo = 0;
    }
  }


  /* Open read only gain file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadGainInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadGainInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input gainfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadGainInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to GAIN extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadGainInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(gunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_PH2PI) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_PH2PI,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadGainInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read gain bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadGainInfo_end;
    }


  /* Get columns index from name */

  if ((gaincol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  if ((gaincol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  if ((gaincol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  if ((gaincol.TEMP = GetColNameIndx(&table, CLNM_TEMP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  if ((gaincol.SLOPE = GetColNameIndx(&table, CLNM_SLOPE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SLOPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  if ((gaincol.OFFSET = GetColNameIndx(&table, CLNM_OFFSET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGainInfo_end;
    }

  /* Check columns multiplicity */
  if(table.Multiplicity[gaincol.TEMP]!=GAIN_TEMP_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_TEMP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadGainInfo_end;
    }
  if(table.Multiplicity[gaincol.SLOPE]!=GAIN_TEMP_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_SLOPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadGainInfo_end;
    }
  if(table.Multiplicity[gaincol.OFFSET]!=GAIN_TEMP_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadGainInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  newdata=0;
	  time = DVEC(table,n,gaincol.TIME);
	  rawx = BVEC(table,n,gaincol.RAWX);
	  rawy = BVEC(table,n,gaincol.RAWY);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadGainInfo_end;
	  }
	  
	  if(gaindata[rawx][rawy].ninfo==0){
	    gaindata[rawx][rawy].ninfo = 1;
	    gaindata[rawx][rawy].info = ( GainInfo_t*)malloc(sizeof(GainInfo_t));
	    newdata=1;
	  }
	  else if(time > gaindata[rawx][rawy].info[(gaindata[rawx][rawy].ninfo-1)].time){
	    gaindata[rawx][rawy].ninfo++;
	    gaindata[rawx][rawy].info = (GainInfo_t*) realloc( gaindata[rawx][rawy].info, (gaindata[rawx][rawy].ninfo * sizeof(GainInfo_t)) );
	    newdata=1;
	  }

	  if(newdata>0){
	    
	    gaindata[rawx][rawy].info[ (gaindata[rawx][rawy].ninfo-1) ].time = time;
	    
	    for (j=0; j< GAIN_TEMP_DIM; j++)
	      gaindata[rawx][rawy].info[ (gaindata[rawx][rawy].ninfo-1) ].temp[j] = EVECVEC(table,n,gaincol.TEMP,j);
	    
	    for (j=0; j< GAIN_TEMP_DIM; j++)
	      gaindata[rawx][rawy].info[ (gaindata[rawx][rawy].ninfo-1) ].slope[j] = EVECVEC(table,n,gaincol.SLOPE,j);
	    
	    for (j=0; j< GAIN_TEMP_DIM; j++)
	      gaindata[rawx][rawy].info[ (gaindata[rawx][rawy].ninfo-1) ].offset[j] = EVECVEC(table,n,gaincol.OFFSET,j);
	    
	  }


	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadGainInfo_end;
    }



  return OK;
  
 ReadGainInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadGainInfo */

/*
 *
 *      ReadClcFile
 *
 *	DESCRIPTION:
 *           Routine to read input clcfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadClcFile(ClcData_t clcdata[4][DET_PIXS][DET_ROWS]){
  

  char        file0[PIL_LINESIZE], file1[PIL_LINESIZE], file2[PIL_LINESIZE], file3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB CLC filename */  
  if ( !strcasecmp(global.par.clcfile,DF_CALDB) )
    {
      /* Retrieve DET0 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CLC_DSET, file0, HD_EXPR, &extfile0, global.evt.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for clcfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadClcFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CLC_DSET, file1, HD_EXPR, &extfile1, global.evt.instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for clcfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadClcFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CLC_DSET, file2, HD_EXPR, &extfile2, global.evt.instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for clcfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadClcFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 gain filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CLC_DSET, file3, HD_EXPR, &extfile3, global.evt.instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for clcfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadClcFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(file0, global.par.clcfile);
    strcpy(file1, global.par.clcfile);
    strcpy(file2, global.par.clcfile);
    strcpy(file3, global.par.clcfile);
  }

  /* Retrieve DET0 gain info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for charge loss correction data of detector %s.\n", global.taskname, file0, KWVL_DETNAM_DET0);
  if( ReadClcInfo(clcdata[0], file0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read charge loss correction coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input clc file: %s.\n", global.taskname, file0);
      goto ReadClcFile_end;
    }

  /* Retrieve DET1 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for charge loss correction data of detector %s.\n", global.taskname, file1, KWVL_DETNAM_DET1);
  if( ReadClcInfo(clcdata[1], file1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read charge loss correction coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, file1);
      goto ReadClcFile_end;
    }

  /* Retrieve DET2 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for charge loss correction data of detector %s.\n", global.taskname, file2, KWVL_DETNAM_DET2);
  if( ReadClcInfo(clcdata[2], file2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read charge loss correction coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, file2);
      goto ReadClcFile_end;
    }

  /* Retrieve DET3 gain info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for charge loss correction data of detector %s.\n", global.taskname, file3, KWVL_DETNAM_DET3);
  if( ReadClcInfo(clcdata[3], file3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read charge loss correction coefficients\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input gain file: %s.\n", global.taskname, file3);
      goto ReadClcFile_end;
    }



  return OK;
  
 ReadClcFile_end:
  
  return NOT_OK;

}


/*
 *
 *      ReadClcInfo
 *
 *	DESCRIPTION:
 *           Routine to get charge loss correction coefficient from input clcfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadClcInfo(ClcData_t clcdata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam){

  int                n, i=0, j=0, status=OK, found=NOT_OK, newdata=0;
  int                inExt, totExt, rawx, rawy;
  double             time;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  ClcCol_t           col;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      clcdata[i][j].ninfo = 0;
    }
  }


  /* Open read only gain file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadClcInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadClcInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input gainfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadClcInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to CLC extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadClcInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(gunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_CLC) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_CLC,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadClcInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read gain bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadClcInfo_end;
    }


  /* Get columns index from name */

  if ((col.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  if ((col.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  if ((col.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  if ((col.CLC = GetColNameIndx(&table, CLNM_CLC)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CLC);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  if ((col.GR_SLOPE = GetColNameIndx(&table, CLNM_GR_SLOPE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GR_SLOPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  if ((col.GR_OFFSET = GetColNameIndx(&table, CLNM_GR_OFFSET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GR_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadClcInfo_end;
    }

  /* Check columns multiplicity */

  if(table.Multiplicity[col.CLC]!=CLC_GRADES_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_CLC);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadClcInfo_end;
    }
  if(table.Multiplicity[col.GR_SLOPE]!=CLC_GRADES_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_GR_SLOPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadClcInfo_end;
    }
  if(table.Multiplicity[col.GR_OFFSET]!=CLC_GRADES_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_GR_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadClcInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  newdata=0;
	  time = DVEC(table,n,col.TIME);
	  rawx = BVEC(table,n,col.RAWX);
	  rawy = BVEC(table,n,col.RAWY);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadClcInfo_end;
	  }
	  
	  if(clcdata[rawx][rawy].ninfo==0){
	    clcdata[rawx][rawy].ninfo = 1;
	    clcdata[rawx][rawy].info = ( ClcInfo_t*)malloc(sizeof(ClcInfo_t));
	    newdata=1;
	  }
	  else if(time > clcdata[rawx][rawy].info[(clcdata[rawx][rawy].ninfo-1)].time){
	    clcdata[rawx][rawy].ninfo++;
	    clcdata[rawx][rawy].info = (ClcInfo_t*) realloc( clcdata[rawx][rawy].info, (clcdata[rawx][rawy].ninfo * sizeof(ClcInfo_t)) );
	    newdata=1;
	  }

	  if(newdata>0){
	    
	    clcdata[rawx][rawy].info[ (clcdata[rawx][rawy].ninfo-1) ].time = time;
	    
	    for (j=0; j< CLC_GRADES_DIM; j++)
	      clcdata[rawx][rawy].info[ (clcdata[rawx][rawy].ninfo-1) ].clc[j] = EVECVEC(table,n,col.CLC,j);
	    
	    for (j=0; j< CLC_GRADES_DIM; j++)
	      clcdata[rawx][rawy].info[ (clcdata[rawx][rawy].ninfo-1) ].gr_slope[j] = EVECVEC(table,n,col.GR_SLOPE,j);
	    
	    for (j=0; j< CLC_GRADES_DIM; j++)
	      clcdata[rawx][rawy].info[ (clcdata[rawx][rawy].ninfo-1) ].gr_offset[j] = EVECVEC(table,n,col.GR_OFFSET,j);

	  }


	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadClcInfo_end;
    }



  return OK;
  
 ReadClcInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadClcInfo */


int ReadClcFilterFile(ClcFilterInfo_t ** info, int *infocount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  long               extfile=-1;
  ClcFilterCol_t     col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  

  /* Derive CALDB CLCFILTER filename */  
  if ( !strcasecmp(global.par.clcfilterfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CLCFILTER_DSET, global.par.clcfilterfile, HD_EXPR, &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for clcfilterfile parameter.\n", global.taskname);
      	  return NOT_OK;
      	}
      extfile++;
    }
  
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.clcfilterfile);
  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(global.par.clcfilterfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.clcfilterfile);
      return NOT_OK;
    }
  
  /* Move in CLCFILTER extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_CLCFILTER, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_CLCFILTER);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.clcfilterfile); 
      goto ReadClcFilterFile_end;
    }
  
  
  head=RetrieveFitsHeader(unit);
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.clcfilterfile);
      goto ReadClcFilterFile_end;
    }
  
  /* Get columns index from name */

  if ((col.ELOW = GetColNameIndx(&table, CLNM_ELOW)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ELOW);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.clcfilterfile);
      goto ReadClcFilterFile_end;
    }
  
  if ((col.EHIGH = GetColNameIndx(&table, CLNM_EHIGH)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EHIGH);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.clcfilterfile);
      goto ReadClcFilterFile_end;
    }
  
  
  EndBintableHeader(&head, &table);


  /* Allocate memory to storage all data */
  *infocount = table.MaxRows;
  *info = (ClcFilterInfo_t *)calloc(*infocount, sizeof(ClcFilterInfo_t));
  if(*info==NULL){
    headas_chat(CHATTY,"%s: Error: ReadClcFilterFile: memory allocation failure.\n", global.taskname);
    goto ReadClcFilterFile_end;
  }
  
  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;
  
  while((count<*infocount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  (*info)[count].elow = EVEC(table,n,col.ELOW);
	  (*info)[count].ehigh = EVEC(table,n,col.EHIGH);
	  count++;
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  *infocount = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.clcfilterfile);
      return NOT_OK;
    }

  return OK;

  
 ReadClcFilterFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadClcFilterFile  */


/*
 *
 *      GetTemperatures
 *
 *	DESCRIPTION:
 *           Routine to get detectors temperatures for a given time
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int GetTemperatures(double time, const HKRow_t *hkinfo, int hknrows, float temp[4]){
  
  int  i=0;
  int  low=0, high=0, mid;

  if( time<( hkinfo[0].time + TIME_SENS) ){
    headas_chat(NORMAL, "%s: Warning: TIME=%f not included in HK input file (using the TEMP values of the first row!)\n", global.taskname, time);

    temp[0] = hkinfo[0].czt0temp;
    temp[1] = hkinfo[0].czt1temp;
    temp[2] = hkinfo[0].czt2temp;
    temp[3] = hkinfo[0].czt3temp;
    
    return OK;
  }

  /* Find appropriate row in hk file ( index 'i' indicates the first row over 'time' value) */
  low = 0;
  high = hknrows-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if ((hkinfo[mid].time + TIME_SENS) <= time) {
      /* This index, and everything below it, must not be the first element
       * greater than what we're looking for because this element is no greater
       * than the element.
       */
      low = mid + 1;
    }
    else {
      /* This element is at least as large as the element, so anything after it can't
       * be the first element that's at least as large.
       */
      high = mid;
    }
  }
  i = low;

  if( (i>0) && (hkinfo[i].time + TIME_SENS > time) ){
    i--;
  }

  temp[0] = hkinfo[i].czt0temp;
  temp[1] = hkinfo[i].czt1temp;
  temp[2] = hkinfo[i].czt2temp;
  temp[3] = hkinfo[i].czt3temp;
  

  return OK;
  
/*  GetTemperatures_end: */
/*   return NOT_OK; */

} /* GetTemperatures */


/*
 *
 *      GetGainCoeff
 *
 *	DESCRIPTION:
 *           Routine to get gain coefficents for a given event
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int GetGainCoeff(const GainData_t *gaindata, double time, float hktemp, double *slope, double *offset){

  int    i=0;
  double slope1=0, offset1=0, slope2=0, offset2=0;
  double dslope=0, doffset=0;

  /* Checks if no data available  */
  if(gaindata->ninfo==0)
    goto GetGainCoeff_end;
  
  if(time<(gaindata->info[0].time + TIME_SENS)){
    headas_chat(NORMAL, "%s: Error: TIME=%f not included in GAIN input file\n", global.taskname, time);
    goto GetGainCoeff_end;    
  }
  
  if(gaindata->ninfo==1){
    
    GetGainCoeffbyTemp(&gaindata->info[0], hktemp, &slope1, &offset1);
    *slope = slope1;
    *offset = offset1;
    return OK;

  }
  else if(time >= (gaindata->info[ gaindata->ninfo -1 ].time - TIME_SENS)){
    
    GetGainCoeffbyTemp(&gaindata->info[ gaindata->ninfo -1 ], hktemp, &slope1, &offset1);
    *slope = slope1;
    *offset = offset1;
    return OK;

  }
  else{
    /* Find appropriate row ( index 'i' indicates the first row over 'time' value) */
    for(i=1; i < gaindata->ninfo; i++){
      if(time < (gaindata->info[i].time + TIME_SENS))
	break;
    }

    GetGainCoeffbyTemp(&gaindata->info[i-1], hktemp, &slope1, &offset1);
    GetGainCoeffbyTemp(&gaindata->info[i], hktemp, &slope2, &offset2);
    
    InterpolateValues(gaindata->info[i-1].time, gaindata->info[i].time, time, slope1, slope2, &dslope);
    InterpolateValues(gaindata->info[i-1].time, gaindata->info[i].time, time, offset1,offset2, &doffset); 

    *slope = dslope;
    *offset = doffset;
    return OK;
  }
  

 GetGainCoeff_end:
  
  return NOT_OK;

} /* GetGainCoeff */



void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value){

  double delta, frac1, frac2;

  delta = up - down;
  frac1 = (up - this) / delta;
  frac2 = (this - down) /delta;

  *value = valuedown * frac1 + valueup * frac2;

}/* InterpolateValues */



void GetGainCoeffbyTemp(const GainInfo_t *info, float hktemp, double *slope, double *offset){

  int i=0;  
  double dslope=0, doffset=0;


  if(hktemp<=info->temp[0]){
    dslope = info->slope[0];
    doffset = info->offset[0];
    goto GetGainCoeffbyTemp_found;
  }
  else if(hktemp>=info->temp[GAIN_TEMP_DIM-1]){
    dslope = info->slope[GAIN_TEMP_DIM-1];
    doffset = info->offset[GAIN_TEMP_DIM-1];
    goto GetGainCoeffbyTemp_found;
  }
  else{
    
    /* Find appropriate row ( index 'i' indicates the first row over 'hktemp' value) */
    for(i=1; i < GAIN_TEMP_DIM; i++){
      if(hktemp <= info->temp[i])
	break;
    }

    InterpolateValues((double)info->temp[i-1], (double)info->temp[i], hktemp, (double)info->slope[i-1], (double)info->slope[i], &dslope);
    InterpolateValues((double)info->temp[i-1], (double)info->temp[i], hktemp, (double)info->offset[i-1], (double)info->offset[i], &doffset);
    goto GetGainCoeffbyTemp_found;
  }


 GetGainCoeffbyTemp_found:

  *slope = dslope;
  *offset = doffset;
  return;
}/* GetGainCoeffbyTemp */


int GetClcData(const ClcData_t *clcdata, double time, int grade, double *clcval, double *gr_offset, double *gr_slope){
 
  int  i=0;
  int  low=0, high=0, mid;

  if(grade>=CLC_GRADES_DIM){
     headas_chat(CHATTY, "%s: Error: GetClc: grade=%d not allowed.\n", global.taskname, grade);
    return NOT_OK;
  }

  if(time<clcdata->info[0].time + TIME_SENS){
     headas_chat(NORMAL, "%s: Error: GetClc: time=%f not included in clcfile data.\n", global.taskname, time);
     return NOT_OK;
  }

  /* Find appropriate row ( index 'i' indicates the first row over 'time' value) */
  low = 0;
  high = clcdata->ninfo-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if ((clcdata->info[i].time + TIME_SENS) <= time) {
      /* This index, and everything below it, must not be the first element
       * greater than what we're looking for because this element is no greater
       * than the element.
       */
      low = mid + 1;
    }
    else {
      /* This element is at least as large as the element, so anything after it can't
       * be the first element that's at least as large.
       */
      high = mid;
    }
  }
  i = low;

  if( (i>0) && (i!=clcdata->ninfo-1) ){
    i--;
  }

  *clcval = (double)clcdata->info[i].clc[grade];
  *gr_offset = (double)clcdata->info[i].gr_offset[grade];
  *gr_slope = (double)clcdata->info[i].gr_slope[grade];

  return OK;

} /* GetClcData */


int ApplyEnCorrection(ClcFilterInfo_t *info, int infocount, float en){

  int i;

  for(i=0; i<infocount; i++){
    if(en>=info[i].elow*KEV2PICH && en<=info[i].ehigh*KEV2PICH)
      return 0;
  }

  return 1;

} /* ApplyEnCorrection */
