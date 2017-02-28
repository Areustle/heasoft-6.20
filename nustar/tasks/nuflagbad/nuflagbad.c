/*
 * 
 *	nuflagbad.c
 *
 *	INVOCATION:
 *
 *		nuflagbad [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine that identifies and flags events falling on detector bad pixels
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 26/04/11 - First version
 *        0.1.1 - NS 27/06/11 - Modified to allow reprocessing of input event file
 *        0.1.2 - NS 04/08/11 - Add new column 'BADPOS' to output event file
 *        0.1.3 - NS 10/10/11 - Bug fixed on 'outbpfile' default name handling
 *                            - Bug fixed when 'outfile' equal to 'infile' parameter
 *        0.1.4 - NS 09/05/12 - Handle new keywords in BADPIX extension
 *                            - Modified 'ReadDisPixFile' function call
 *        0.1.5 - NS 18/09/12 - Handle long file naming
 *        0.1.6 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.7 - RF 05/04/16 - Modified call 'CreateBPFile' and 'CreateBPExt' function to bug on output file
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuflagbad  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuflagbad.h"


/********************************/
/*         definitions          */
/********************************/

#define NUFLAGBAD_C
#define NUFLAGBAD_VERSION      "0.1.7"
#define PRG_NAME               "nuflagbad"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuflagbad_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuflagbad.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuflagbad_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 26/04/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagbad_getpar()
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

  /* On-Board disabled pixel File Name */
  if(PILGetFname(PAR_DISPIXFILE, global.par.dispixfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DISPIXFILE);
      goto Error;
    }
  if ( !strcasecmp(global.par.dispixfile,DF_NONE) )
    global.dispixfile = FALSE;
  else
    global.dispixfile = TRUE;
  
  /* On-Ground bad pixel File Name */
  if(PILGetFname(PAR_BPFILE, global.par.bpfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BPFILE);
      goto Error;
    }
  if ( !strcasecmp(global.par.bpfile,DF_NONE) )
    global.bpfile = FALSE;
  else
    global.bpfile = TRUE;

  /* User bad pixel File Name */
  if(PILGetFname(PAR_USERBPFILE, global.par.userbpfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USERBPFILE);
      goto Error;
    }
  if ( !strcasecmp(global.par.userbpfile,DF_NONE) )
    global.userbpfile = FALSE;
  else
    global.userbpfile = TRUE;

  /* Output Bad Pixel File Name */
  if(PILGetFname(PAR_OUTBPFILE, global.par.outbpfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTBPFILE);
      goto Error;
    }
  if ( !strcasecmp(global.par.outbpfile,DF_NONE) )
    global.outbpfile = FALSE;
  else
    global.outbpfile = TRUE;


  get_history(&global.hist);
  nuflagbad_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuflagbad_getpar */


/*
 *	nuflagbad_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuflagbad_checkinput();
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
 *        0.1.0 - NS 26/04/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagbad_work()
{
  int               status = OK, inExt, outExt, evExt, inExtNum;
  int               logical, k, i, j;
  int               bpExt0=-1, bpExt1=-1, bpExt2=-1, bpExt3=-1;
  static BadPixExtData_t   bp[4][DET_PIXS][DET_ROWS];
  BadPixKeys_t      bpkeys;
  FitsHeader_t      head;
  FitsCard_t        *card;
  FitsFileUnit_t    inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  unsigned short int nobadflag=0;

  nobadflag |= CALDB_BP;
  nobadflag |= ONBOARD_BP;
  nobadflag |= USER_BP;
  
  for(k=0; k<4; k++){
    for(i=0; i<DET_PIXS; i++){
      for(j=0; j<DET_ROWS; j++){
	bp[k][i][j].ninfo = 0;
      }
    }
  }


  if(nuflagbad_checkinput())
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
      strcpy(bpkeys.instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve OBS_ID from input event file */  
  if(ExistsKeyWord(&head, KWNM_OBS_ID, &card))
    {
      strcpy(bpkeys.obs_id, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve ORIGIN from input event file */  
  if(ExistsKeyWord(&head, KWNM_ORIGIN, &card))
    {
      strcpy(bpkeys.origin, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ORIGIN);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* MJDREFI */
  if((ExistsKeyWord(&head, KWNM_MJDREFI, &card)))
    {
      bpkeys.mjdrefi = card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* MJDREFF */
  if((ExistsKeyWord(&head, KWNM_MJDREFF, &card)))
    {
      bpkeys.mjdreff = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      bpkeys.tstart = card->u.DVal;
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
      bpkeys.tstop = card->u.DVal;
      global.evt.tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
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
  
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
      goto Error;
    }


  /* Read Event BADPIX extensions */
  if(ReadEvtBPdata(bp,&bpExt0,&bpExt1,&bpExt2,&bpExt3))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get bad pixels data from input event file\n", global.taskname);
      goto Error;
    }

  /* Read bad pixels data */
  if(ReadBPdata(bp))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get bad pixels data from input files\n", global.taskname);
      goto Error;
    }
  

  /* Sort bad pixel data */
  SortBadPixExtData(bp[0]);
  SortBadPixExtData(bp[1]);
  SortBadPixExtData(bp[2]);
  SortBadPixExtData(bp[3]);


  /* Create output file */
  if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExtNum, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extension before event extension  from input to output file */
  inExt=1;
  outExt=1;
    
  while(inExt<evExt && status == OK)
    {
      if(inExt!=bpExt0 && inExt!=bpExt1 && inExt!=bpExt2 && inExt!=bpExt3)
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	  if(fits_copy_hdu( inunit, outunit, 0, &status ))
	    {
	      headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,inExt);
	      headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	      headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	      goto Error;
	    }
	  inExt++;
	  outExt++;
	}
      else
	{
	  inExt++;	  
	}
    }
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);

  /* Flags events falling on detector bad pixels */
  if (BadPixFlag(inunit, outunit, bp))
    {
      headas_chat(NORMAL, "%s: Error: Unable to flag events falling on detector bad pixels.\n", global.taskname);
      goto Error;
    }
  
  /* Set NUFLAG keyword to true */
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_NUFLAG, &logical, CARD_COMM_NUFLAG, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_NUFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }

  /* Write event bad flags comments */
  if(WriteStatus(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write COMMENT lines\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }


  if(global.warning)
    goto ok_end;


  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[0], 1, KWVL_DETNAM_DET0,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET0);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[1], 2, KWVL_DETNAM_DET1,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET1);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[2], 3, KWVL_DETNAM_DET2,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET2);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[3], 4, KWVL_DETNAM_DET3,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET3);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }


  /* copy any extension after the extension to be operated on */
  inExt++;
  while ( status == OK && inExt <= inExtNum) 
    {
      if(inExt!=bpExt0 && inExt!=bpExt1 && inExt!=bpExt2 && inExt!=bpExt3)
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	  if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	    {
	      headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,inExt);
	      headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	      headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	      goto Error;
	    }
	  
	  inExt++;
	  outExt++;
	}
      else
	{
	  inExt++;	  
	}
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
  
   
  /* Create Output Bad Pixel file */
  if(global.outbpfile)
    {
      headas_chat(NORMAL, "%s: Info: Creating output bad pixel file.\n", global.taskname);
      if (CreateBPFile(bp, global.par.outbpfile, &bpkeys, global.par.outfile, nobadflag))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create '%s' file.\n", global.taskname, global.par.outbpfile);
	  goto Error;
	}
    }


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
} /* nuflagbad_work */


/*
 *	nuflagbad
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
 *             void nuflagbad_getpar(void);
 * 	       void nuflagbad_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 26/04/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagbad()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUFLAGBAD_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuflagbad_getpar() == OK) 
    {
      
      if ( nuflagbad_work()) 
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
  
} /* nuflagbad */


/*
 *	nuflagbad_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 26/04/11 - First version
 *
 */
void nuflagbad_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input on-board disabled pixel file        :'%s'\n",global.par.dispixfile);
  headas_chat(NORMAL,"Name of the input on-ground bad pixel file            :'%s'\n",global.par.bpfile);
  headas_chat(NORMAL,"Name of the input user bad pixel file                 :'%s'\n",global.par.userbpfile);
  headas_chat(NORMAL,"Name of the output Bad Pixel file                     :'%s'\n",global.par.outbpfile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nuflagbad_info */


/*
 *	nuflagbad_checkinput
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
 *        0.1.0: - NS 26/04/11 - First version
 *
 */
int nuflagbad_checkinput(void)
{
  int            overwrite=0;
  char           stem[10] , ext[MAXEXT_LEN] ;
  pid_t          tmp;
  char           BaseName[MAXFNAME_LEN];
  
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


  if(global.outbpfile){

    /* If global.par.outbpfile == DEFAULT build filename */ 
    if (!(strcasecmp (global.par.outbpfile, DF_DEFAULT)))
      {
	SplitFilePath(global.par.outfile, NULL, BaseName);
	strcpy(global.par.outbpfile, BaseName);

	StripExtension(global.par.outbpfile);
	strcat(global.par.outbpfile, EXT_FITS_BP);
	headas_chat(NORMAL, "%s: Info: Name for the bad pixels file is:\n",global.taskname);
	headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outbpfile);
      }
    
    if ( FileExists(global.par.outbpfile) ) {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outbpfile);
      if ( !headas_clobpar ) {
	headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outbpfile);
	headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	goto check_end;
      }
      else
	{ 
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outbpfile);
	  if(remove (global.par.outbpfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outbpfile);
	      goto check_end;
	    }
	}
    }
    
  } /*if(global.outbpfile)*/

 
  return OK;

 check_end:
  return NOT_OK;
}


/*
 *
 *      BadPixFlag
 *
 *	DESCRIPTION:
 *           Routine to create event bintable with bad pixels flagged.
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
 *        0.1.0: - NS 26/04/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int BadPixFlag(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixExtData_t bp[4][DET_PIXS][DET_ROWS])
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                rawx, rawy, rawx_n, rawy_n, det_id, i, j;
  int                newstatus, newbadpos;
  double             time;
  char               comm[PIL_LINESIZE];
  BTYPE              neigh_pix_badpos[] = {NEIGH_PIX_BADPOS};
  int                neigh_x[] = {NEIGH_PIX_X};
  int                neigh_y[] = {NEIGH_PIX_Y};
  EvtCol_t           indxcol;
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

  /* RAWX */
  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* RAWY */
  if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* DET_ID */
  if ((indxcol.DET_ID=ColNameMatch(CLNM_DET_ID, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_DET_ID);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* Add STATUS column */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
    }

  /* Add BADPOS column */
  newbadpos=0;
  if((indxcol.BADPOS=ColNameMatch(CLNM_BADPOS, &outtable)) == -1)
    {
      newbadpos=1;
      AddColumn(&head, &outtable,CLNM_BADPOS,CARD_COMM_BADPOS , "8X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_BADPOS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.BADPOS=ColNameMatch(CLNM_BADPOS, &outtable);
    }


  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);

      if(newstatus)
	{
	  sprintf(comm, "Added %s column", CLNM_STATUS);
	  AddHistory(&head, comm);
	}
      if(newbadpos)
	{
	  sprintf(comm, "Added %s column", CLNM_BADPOS);
	  AddHistory(&head, comm);
	}

      if(global.userbpfile||global.bpfile||global.dispixfile)
	{
	  sprintf(comm, "Used following file(s) to get bad pixels: ");
	  AddHistory(&head, comm);

	  if(global.bpfile)
	    {
	      sprintf(comm, "%s CALDB bad pixels file", global.par.bpfile);
	      AddHistory(&head, comm);
	    }
	  if(global.userbpfile)
	    {
	      sprintf(comm, "%s user bad pixels file", global.par.userbpfile);
	      AddHistory(&head, comm);
	    }
	  if(global.dispixfile)
	    {
	      sprintf(comm, "%s on-board bad pixels map", global.par.dispixfile);
	      AddHistory(&head, comm);
	    }
	}     
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

	  /* Check input data */
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto reco_end;
	  }
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Error: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    goto reco_end;
	  }

	  /* Reset STATUS column */
	  if(newstatus){
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	  }
	  else{
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_CAL_BP;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_ONBOARD_BP;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_USER_BP;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_NEIGH_BP;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_EDGE_BP;
	  }

	  /* Reset BADPOS column */
	  XVEC1BYTE(outtable,n,indxcol.BADPOS)=0;


	  /* Bad pixel */
	  if(bp[det_id][rawx][rawy].ninfo > 0)
	    {
	      for(i=0; i<bp[det_id][rawx][rawy].ninfo; i++)
		{
		  
		  if( (bp[det_id][rawx][rawy].info[i].badflag & EV_CAL_BP) &&
		      ( time>=(bp[det_id][rawx][rawy].info[i].time-TIME_SENS) && time<=(bp[det_id][rawx][rawy].info[i].time_stop+TIME_SENS) ) )
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_CAL_BP;
 
		  if( (bp[det_id][rawx][rawy].info[i].badflag & EV_ONBOARD_BP) &&
		      ( time>=(bp[det_id][rawx][rawy].info[i].time-TIME_SENS) && time<=(bp[det_id][rawx][rawy].info[i].time_stop+TIME_SENS) ) )
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_ONBOARD_BP;

		  if( (bp[det_id][rawx][rawy].info[i].badflag & EV_USER_BP) &&
		      ( time>=(bp[det_id][rawx][rawy].info[i].time-TIME_SENS) && time<=(bp[det_id][rawx][rawy].info[i].time_stop+TIME_SENS) ) )
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_USER_BP;

		}
	    }


	  /* Check if event falls in a pixel on a detector edge */
	  if( rawx==RAWX_MIN || rawx==RAWX_MAX || rawy==RAWY_MIN || rawy==RAWY_MAX )
	    XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_EDGE_BP;


	  /* Check neighbours bad pixels */
	  for (j=0; j< NEIGH_PIX_MOL; j++)
	    {
	      rawx_n = rawx + neigh_x[j];
	      rawy_n = rawy + neigh_y[j];

	      if(rawx_n>=RAWX_MIN && rawx_n<=RAWX_MAX && rawy_n>=RAWY_MIN && rawy_n<=RAWY_MAX){
		for(i=0; i<bp[det_id][rawx_n][rawy_n].ninfo; i++)
		  {
		    if( (bp[det_id][rawx_n][rawy_n].info[i].badflag & EV_CAL_BP)
			|| (bp[det_id][rawx_n][rawy_n].info[i].badflag & EV_ONBOARD_BP)
			|| (bp[det_id][rawx_n][rawy_n].info[i].badflag & EV_USER_BP) )
		      {
			if( time>=(bp[det_id][rawx_n][rawy_n].info[i].time-TIME_SENS) && time<=(bp[det_id][rawx_n][rawy_n].info[i].time_stop+TIME_SENS) ){

			  XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_NEIGH_BP;			  
			  XVEC1BYTE(outtable,n,indxcol.BADPOS) |= neigh_pix_badpos[j];
			}
		      }
		  }
	      }
	      else{
		/* pixel outside the detector */
		XVEC1BYTE(outtable,n,indxcol.BADPOS) |= neigh_pix_badpos[j];
	      }
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
  
} /* BadPixFlag */


/*
 *
 *      ReadEvtBPdata
 *
 *	DESCRIPTION:
 *           Routine to read the bad pixel extensions of the input event file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], int *bpExt0, int *bpExt1, int *bpExt2, int *bpExt3){

  static BadPixExtData_t inbp[DET_PIXS][DET_ROWS];
  BOOL                   extfound;
  short int              nobadflag=0;


  /* Exclude BADPIX rows with the following BADFLAG bit set to 1 */
  nobadflag |= CALDB_BP;
  nobadflag |= ONBOARD_BP;
  nobadflag |= USER_BP;


  /* Read BADPIX ext for DET0 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET0, nobadflag, &extfound, bpExt0))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[0]))
    return NOT_OK;

  /* Read BADPIX ext for DET1 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET1, nobadflag, &extfound, bpExt1))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[1]))
    return NOT_OK; 

  /* Read BADPIX ext for DET2 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET2, nobadflag, &extfound, bpExt2))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[2]))
    return NOT_OK; 

  /* Read BADPIX ext for DET3 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET3, nobadflag, &extfound, bpExt3))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[3]))
    return NOT_OK; 


  return OK;

} /* ReadEvtBPdata */


/*
 *
 *      ReadBPdata
 *
 *	DESCRIPTION:
 *           Routine to read bad pixels map from 'bpfile', 'userbpfile' and 'dispixfile' input files
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS]){
  
  char        file0[PIL_LINESIZE], file1[PIL_LINESIZE], file2[PIL_LINESIZE], file3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* ON_GROUND BAD PIXELS */
  if(global.bpfile)
    {
      /* Derive CALDB gain filename */  
      if ( !strcasecmp(global.par.bpfile,DF_CALDB) )
	{
	  /* Retrieve DET0 on-ground bad pixel filename */
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET, file0, HD_EXPR, &extfile0, global.evt.instrume, KWVL_DETNAM_DET0))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for 'bpfile' parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
	      goto ReadBPdata_end;
	    }
	  extfile0++;
	  
	  /* Retrieve DET1 on-ground bad pixel filename */
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET, file1, HD_EXPR, &extfile1, global.evt.instrume, KWVL_DETNAM_DET1))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for 'bpfile' parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
	      goto ReadBPdata_end;
	    }
	  extfile1++;
	  
	  /* Retrieve DET2 on-ground bad pixel filename */
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET, file2, HD_EXPR, &extfile2, global.evt.instrume, KWVL_DETNAM_DET2))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for 'bpfile' parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
	      goto ReadBPdata_end;
	    }
	  extfile2++;
	  
	  /* Retrieve DET3 on-ground bad pixel filename */
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET, file3, HD_EXPR, &extfile3, global.evt.instrume, KWVL_DETNAM_DET3))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for 'bpfile' parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
	      goto ReadBPdata_end;
	    }
	  extfile3++;
	  
	}
      else{
	strcpy(file0, global.par.bpfile);
	strcpy(file1, global.par.bpfile);
	strcpy(file2, global.par.bpfile);
	strcpy(file3, global.par.bpfile);
      }
      
      /* Retrieve DET0 on-ground bad pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-ground bad pixel data of detector %s.\n", global.taskname, file0, KWVL_DETNAM_DET0);
      if( ReadBPInfo(bp[0], file0, extfile0, KWVL_DETNAM_DET0, CALDB_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-ground bad pixel file: %s.\n", global.taskname, file0);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET1 on-ground bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-ground bad pixel data of detector %s.\n", global.taskname, file1, KWVL_DETNAM_DET1);
      if( ReadBPInfo(bp[1], file1 ,extfile1, KWVL_DETNAM_DET1, CALDB_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-ground bad pixel file: %s.\n", global.taskname, file1);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET2 on-ground bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-ground bad pixel data of detector %s.\n", global.taskname, file2, KWVL_DETNAM_DET2);
      if( ReadBPInfo(bp[2], file2, extfile2, KWVL_DETNAM_DET2, CALDB_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-ground bad pixel file: %s.\n", global.taskname, file2);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET3 on-ground bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-ground bad pixel data of detector %s.\n", global.taskname, file3, KWVL_DETNAM_DET3);
      if( ReadBPInfo(bp[3], file3, extfile3, KWVL_DETNAM_DET3, CALDB_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-ground bad pixel file: %s.\n", global.taskname, file3);
	  goto ReadBPdata_end;
	}

    } /* if(global.bpfile) */


  /* ON-BOARD DISABLED PIXELS */
  if(global.dispixfile)
    {
      /* Retrieve DET0 on-board disabled pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-board disabled pixel data of detector %s.\n", global.taskname, global.par.dispixfile, KWVL_DETNAM_DET0);
      if( ReadDisInfo(bp[0], global.par.dispixfile, -1, KWVL_DETNAM_DET0) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read disabled pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-board disabled pixel file: %s.\n", global.taskname, global.par.dispixfile);
	  goto ReadBPdata_end;
	}

      /* Retrieve DET1 on-board disabled pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-board disabled pixel data of detector %s.\n", global.taskname, global.par.dispixfile, KWVL_DETNAM_DET1);
      if( ReadDisInfo(bp[1], global.par.dispixfile, -1, KWVL_DETNAM_DET1) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read disabled pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-board disabled pixel file: %s.\n", global.taskname, global.par.dispixfile);
	  goto ReadBPdata_end;
	}

      /* Retrieve DET2 on-board disabled pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-board disabled pixel data of detector %s.\n", global.taskname, global.par.dispixfile, KWVL_DETNAM_DET2);
      if( ReadDisInfo(bp[2], global.par.dispixfile, -1, KWVL_DETNAM_DET2) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read disabled pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-board disabled pixel file: %s.\n", global.taskname, global.par.dispixfile);
	  goto ReadBPdata_end;
	}

      /* Retrieve DET3 on-board disabled pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for on-board disabled pixel data of detector %s.\n", global.taskname, global.par.dispixfile, KWVL_DETNAM_DET3);
      if( ReadDisInfo(bp[3], global.par.dispixfile, -1, KWVL_DETNAM_DET3) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read disabled pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input on-board disabled pixel file: %s.\n", global.taskname, global.par.dispixfile);
	  goto ReadBPdata_end;
	}

    } /* if(global.dispixfile) */


  /* USER BAD PIXELS */
  if(global.userbpfile)
    {
      
      /* Retrieve DET0 user bad pixel info */    
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for user bad pixel data of detector %s.\n", global.taskname, global.par.userbpfile, KWVL_DETNAM_DET0);
      if( ReadBPInfo(bp[0], global.par.userbpfile, -1, KWVL_DETNAM_DET0, USER_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input user bad pixel file: %s.\n", global.taskname, global.par.userbpfile);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET1 user bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for user bad pixel data of detector %s.\n", global.taskname, global.par.userbpfile, KWVL_DETNAM_DET1);
      if( ReadBPInfo(bp[1], global.par.userbpfile, -1, KWVL_DETNAM_DET1, USER_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input user bad pixel file: %s.\n", global.taskname, global.par.userbpfile);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET2 user bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for user bad pixel data of detector %s.\n", global.taskname, global.par.userbpfile, KWVL_DETNAM_DET2);
      if( ReadBPInfo(bp[2], global.par.userbpfile, -1, KWVL_DETNAM_DET2, USER_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input user bad pixel file: %s.\n", global.taskname, global.par.userbpfile);
	  goto ReadBPdata_end;
	}
      
      /* Retrieve DET3 user bad pixel info */ 
      headas_chat(NORMAL, "%s: Info: Processing '%s' file for user bad pixel data of detector %s.\n", global.taskname, global.par.userbpfile, KWVL_DETNAM_DET3);
      if( ReadBPInfo(bp[3], global.par.userbpfile, -1, KWVL_DETNAM_DET3, USER_BP) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read bad pixel data\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in input user bad pixel file: %s.\n", global.taskname, global.par.userbpfile);
	  goto ReadBPdata_end;
	}

    } /* if(global.userbpfile) */


  return OK;
  
 ReadBPdata_end:
  
  return NOT_OK;


}  /* ReadBPdata */


/*
 *
 *      ReadBPInfo
 *
 *	DESCRIPTION:
 *           
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadBPInfo(BadPixExtData_t bp[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam, short int inbadflag){

  static BadPixData_t    inbp[DET_PIXS][DET_ROWS];

  if(ReadBadPixFile(inbp, filename, extno, detnam, global.evt.instrume, global.evt.tstop, inbadflag))
    return NOT_OK;

  if(UpdateBPExtWithBadPixData(inbp, bp, global.evt.tstop))
    return NOT_OK;

  return OK;

} /* ReadBPInfo */


/*
 *
 *      ReadDisInfo
 *
 *	DESCRIPTION:
 *           
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadDisInfo(BadPixExtData_t bp[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam){

  static DisPixData_t    inbp[DET_PIXS][DET_ROWS];

  if(ReadDisPixFile(inbp, filename, extno, detnam, global.evt.instrume, global.evt.tstart, global.evt.tstop))
    return NOT_OK;

  if(UpdateBPExtWithDisPixData(inbp, bp))
    return NOT_OK;

  return OK;

} /* ReadDisInfo */

