/*
 * 
 *	nucalcpha.c
 *
 *	INVOCATION:
 *
 *		nucalcpha [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine to reconstruct energy and assign grades to NuSTAR data
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 25/10/10 - First version
 *        0.1.1 - NS 12/11/10 - Added 'cleancols' input parameter
 *                            - Modified handling of "DENRISE = 0" Events
 *        0.1.2 - NS 15/12/10 - Handle CALDB query for 'offsetfile' and 'gradefile'
 *                              input parameters
 *        0.1.3 - NS 10/01/11 - Added 'SWTRIG' column in output evt file
 *                            - Renamed column from 'COMPHAS' to 'PHAS' in output evt file
 *                            - Set PHAS[i]=TRPHAS[i] for events below software threshold 
 *        0.1.4 - NS 12/01/11 - Added 'CLNM_SURR' column in output evt file
 *        0.1.5 - NS 07/07/11 - Added 'phaparfile' input parameter
 *                            - Handle 'PHAPAR' CALDB file 
 *                            - Query to CALDB for 'grade' and 'offset' files depending on time
 *        0.1.6 - NS 31/08/11 - Handle 'BADPOS' column of input evt file
 *        0.1.7 - NS 20/09/11 - Handle 'HOTPOS' column of input evt file
 *        0.1.8 - NS 10/10/11 - Bug fixed when 'outfile' equal to 'infile' parameter 
 *                            - Handle input event file without 'BADPOS' or 'HOTPOS' columns
 *                            - Add 'TUNIT*' keyword for PHA column
 *                            - Bug fixed for MacOSX platform
 *        0.1.9 - NS 13/02/12 - Removed 'PHA' and 'SURR' columns in output evt file
 *        0.2.0 - NS 26/07/12 - Add 'TLMIN*' and 'TLMAX*' keywords for GRADE column
 *        0.2.1 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nucalcpha  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nucalcpha.h"


/********************************/
/*         definitions          */
/********************************/

#define NUCALCPHA_C
#define NUCALCPHA_VERSION      "0.2.1"
#define PRG_NAME               "nucalcpha"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nucalcpha_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nucalcpha.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nucalcpha_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 25/10/10 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpha_getpar()
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
  
  /* Input Offset File Name */
  if(PILGetFname(PAR_OFFSETFILE, global.par.offsetfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OFFSETFILE);
      goto Error;	
    }

  /* Input GRADE File Name */
  if(PILGetFname(PAR_GRADEFILE, global.par.gradefile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRADEFILE);
      goto Error;	
    }

  /* Input PHAPAR File Name */
  if(PILGetFname(PAR_PHAPARFILE, global.par.phaparfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHAPARFILE);
      goto Error;	
    }
  if ( !strcasecmp(global.par.phaparfile,DF_NONE) )
    global.phaparfile = FALSE;
  else
    global.phaparfile = TRUE;
  
    /* evthr */
  if(PILGetInt(PAR_EVTTHR, &global.par.evtthr)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVTTHR);
      goto Error;	
    }

  /* timerise */
  if(PILGetReal(PAR_TIMERISE, &global.par.timerise))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TIMERISE); 
      goto Error;	 
    }
 
  /* cleancols */
  if(PILGetBool(PAR_CLEANCOLS, &global.par.cleancols))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANCOLS); 
      goto Error;	 
    }


  get_history(&global.hist);
  nucalcpha_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nucalcpha_getpar */


/*
 *	nucalcpha_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nucalcpha_checkinput();
 *             int headas_chat(int , char *, ...);
 *             int strcasecmp(const char *s1, const char *s2);
 *             int GetGrades(long extno);
 *             int GetOffset(OffsetInfo_t* , OffsetInfo_t* , OffsetInfo_t* , OffsetInfo_t*);
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *             FitsHeader_t  RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *             FitsFileUnit_t OpenWriteFitsFile(char *name);
 *             int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, int *status );
 *             int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status);
 *             int ComputePHAandGRADE(FitsFileUnit_t evunit, FitsFileUnit_t ounit, OffsetInfo_t*, OffsetInfo_t*, OffsetInfo_t*, OffsetInfo_t*);
 *             int HDpar_stamp(fitsfile *fptr, int hdunum);
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int RenameFile (char *, char *);
 *             int CopyFile(char *source, char *destination);
 *             int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, 
 *                                char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);
 *	
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 25/10/10 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpha_work()
{
  int            status = OK, inExt, outExt, evExt;
  long           extno;
  FitsHeader_t   head;
  FitsCard_t     *card;
  FitsFileUnit_t inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  static OffsetInfo_t offset_det0[DET_PIXS][DET_ROWS];
  static OffsetInfo_t offset_det1[DET_PIXS][DET_ROWS];
  static OffsetInfo_t offset_det2[DET_PIXS][DET_ROWS];
  static OffsetInfo_t offset_det3[DET_PIXS][DET_ROWS];
  static PhaParData_t phapardata[4][DET_PIXS][DET_ROWS];

  if(nucalcpha_checkinput())
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
 
  
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
      goto Error;
    }



  /********************************
   *          grades file         *
   ********************************/

  extno=-1;

  if ( !strcasecmp(global.par.gradefile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_GRADE_DSET, global.par.gradefile, HD_EXPR, &extno, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for gradefile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extno++;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.gradefile);
  if (GetGrades(extno)) 
    {
      headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: %s file.\n", global.taskname, global.par.gradefile);
      goto Error;
    }


  /********************************
   *          offset file         *
   ********************************/

  extno=-1;

  if ( !strcasecmp(global.par.offsetfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_CAP_OFFSET_DSET, global.par.offsetfile, HD_EXPR, &extno, global.evt.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for offsetfile parameter.\n", global.taskname);
	  goto Error;
      	}
      extno++;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.offsetfile);
  
  
  if (GetOffset(offset_det0,offset_det1,offset_det2,offset_det3)) 
    {
      headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: %s file.\n", global.taskname, global.par.offsetfile);
      goto Error;
    }
  

  /********************************
   *          PHAPAR file         *
   ********************************/

  if(global.phaparfile){
    if( ReadPhaParFile(phapardata) )
      {
	headas_chat(NORMAL, " %s: Error: Unable to read PHAPAR data\n", global.taskname);
	headas_chat(NORMAL, " %s: Error: in input phaparfile: %s.\n", global.taskname, global.par.phaparfile);
	goto Error;
      }
  }

  /********************************
   ********************************/



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

  /* Compute PHA and GRADE */
  if (ComputePHAandGRADE(inunit, outunit, offset_det0,offset_det1,offset_det2,offset_det3,phapardata))
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute PHA and GRADE values.\n", global.taskname);
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
} /* nucalcpha_work */


/*
 *	nucalcpha
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
 *             void nucalcpha_getpar(void);
 * 	       void nucalcpha_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 25/10/10 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpha()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUCALCPHA_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nucalcpha_getpar() == OK) 
    {
      
      if ( nucalcpha_work()) 
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
  
} /* nucalcpha */


/*
 *	nucalcpha_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 25/10/10 - First version
 *
 */
void nucalcpha_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input Offset file                         :'%s'\n",global.par.offsetfile);
  headas_chat(NORMAL,"Name of the input Grade file                          :'%s'\n",global.par.gradefile);
  if(global.phaparfile){
    headas_chat(NORMAL,"Name of the input PHAPAR file                         :'%s'\n",global.par.phaparfile);  
  }
  else{
    headas_chat(NORMAL,"Event software threshold                              : %d \n",global.par.evtthr);
    headas_chat(NORMAL,"Time of rise correction coefficient                   : %f \n",global.par.timerise);
  }

  if (global.par.cleancols)
    headas_chat(NORMAL,"Delete temporary columns                              : yes\n");
  else
    headas_chat(NORMAL,"Delete temporary columns                              : no\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nucalcpha_info */


/*
 *	nucalcpha_checkinput
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
 *        0.1.0: - NS 25/10/10 - First version
 *
 */
int nucalcpha_checkinput(void)
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
 *      ComputePHAandGRADE
 *
 *	DESCRIPTION:
 *           Routine to compute GRADE, RAWPHAS, OFFPHAS, TRPHAS, COMPHAS and PHA values
 *           and put them into EVENTS bintable.
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
 *        0.1.0: - NS 25/10/10 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputePHAandGRADE(FitsFileUnit_t evunit, FitsFileUnit_t ounit, OffsetInfo_t offset_det0[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det1[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det2[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det3[DET_PIXS][DET_ROWS], PhaParData_t phapardata[4][DET_PIXS][DET_ROWS])
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                jj=0, ii=0, rawx, rawy, s_cap, det_id, grade, x, y;
/*   int                pha; */
  BTYPE              badpos, hotpos;
  JTYPE              jval;
  char               KeyWord[FLEN_KEYWORD];
  BOOL               read_badpos, read_hotpos;
  BTYPE              badpos9bit[PHAS_MOL];
  BTYPE              neigh_pix_badpos[] = {NEIGH_PIX_BADPOS};
  int                numrise, denrise, n_downthr, handle[PHAS_MOL], totrows, colnum, status=OK;
  int                evtthr[PHAS_MOL];
  double             timerise[PHAS_MOL];
  int                neigh_x[] = {NEIGH_X};
  int                neigh_y[] = {NEIGH_Y};
  double             per_grade[33], time;
  double             tr_corr=0.0;
  double             pha_tr, sum_downthr, avg_downthr;
/*   double             pha_d, spha_d; */
  BOOL               found=0;
  OffsetInfo_t       *offset_det;
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
  totrows=outtable.MaxRows;

  /* NUMRISE */
  if ((indxcol.NUMRISE=ColNameMatch(CLNM_NUMRISE, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_NUMRISE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* DENRISE */
  if ((indxcol.DENRISE=ColNameMatch(CLNM_DENRISE, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_DENRISE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* POSTPHAS */
  if ((indxcol.POSTPHAS=ColNameMatch(CLNM_POSTPHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_POSTPHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
  
  if (outtable.Multiplicity[indxcol.POSTPHAS] != PHAS_MOL)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, CLNM_POSTPHAS, outtable.Multiplicity[indxcol.POSTPHAS]); 
      headas_chat(NORMAL,"%s: Error: but should be: %d \n", global.taskname, PHAS_MOL); 
      goto reco_end;
    }
  /* PREPHAS */
  if ((indxcol.PREPHAS=ColNameMatch(CLNM_PREPHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PREPHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
  
  if (outtable.Multiplicity[indxcol.PREPHAS] != PHAS_MOL)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, CLNM_PREPHAS, outtable.Multiplicity[indxcol.PREPHAS]); 
      headas_chat(NORMAL,"%s: Error: but should be: %d \n", global.taskname, PHAS_MOL); 
      goto reco_end;
    }

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

  /* S_CAP */
  if ((indxcol.S_CAP=ColNameMatch(CLNM_S_CAP, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_S_CAP);
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

  /* TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* BADPOS */
  if ((indxcol.BADPOS=ColNameMatch(CLNM_BADPOS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Warning: %s column does not exist\n",  global.taskname, CLNM_BADPOS);
      headas_chat(NORMAL, "%s: Warning: in '%s' file,\n", global.taskname, global.par.infile); 
      read_badpos = FALSE;
    }
  else
    {
      read_badpos = TRUE;
    }

  /* HOTPOS */
  if ((indxcol.HOTPOS=ColNameMatch(CLNM_HOTPOS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Warning: %s column does not exist\n",  global.taskname, CLNM_HOTPOS);
      headas_chat(NORMAL, "%s: Warning: in '%s' file.\n", global.taskname, global.par.infile); 
      read_hotpos = FALSE;
    }
  else
    {
      read_hotpos = TRUE;
    }

  /* Add new columns if needed */

  if((indxcol.RAWPHAS=ColNameMatch(CLNM_RAWPHAS, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_RAWPHAS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable, CLNM_RAWPHAS, CARD_COMM_RAWPHAS, "9I", TNONE);
      indxcol.RAWPHAS=ColNameMatch(CLNM_RAWPHAS, &outtable);
    }

  if((indxcol.OFFPHAS=ColNameMatch(CLNM_OFFPHAS, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_OFFPHAS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable, CLNM_OFFPHAS, CARD_COMM_OFFPHAS, "9E", TNONE);
      indxcol.OFFPHAS=ColNameMatch(CLNM_OFFPHAS, &outtable);
    }

  if((indxcol.TRPHAS=ColNameMatch(CLNM_TRPHAS, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_TRPHAS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable, CLNM_TRPHAS, CARD_COMM_TRPHAS, "9E", TNONE);
      indxcol.TRPHAS=ColNameMatch(CLNM_TRPHAS, &outtable);
    }
  
  if((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PHAS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable, CLNM_PHAS, CARD_COMM_PHAS, "9E", TNONE);
      indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable);
    }

/*   if((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1) */
/*     { */
/*       headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PHA); */
/*       headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    */
/*       AddColumn(&head, &outtable, CLNM_PHA, CARD_COMM_PHA, "1J", TUNIT, UNIT_CHANN, CARD_COMM_PHYSUNIT); */
/*       indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable); */
/*     } */

/*   if((indxcol.SURR=ColNameMatch(CLNM_SURR, &outtable)) == -1) */
/*     { */
/*       headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_SURR); */
/*       headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    */
/*       AddColumn(&head, &outtable, CLNM_SURR, CARD_COMM_SURR, "1J", TNONE); */
/*       indxcol.SURR=ColNameMatch(CLNM_SURR, &outtable); */
/*     } */

  if((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_GRADE);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable,CLNM_GRADE, CARD_COMM_GRADE, "1I",TNONE);    
      indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable);    
    }
  sprintf(KeyWord, "TLMIN%d", (indxcol.GRADE+1));
  jval = GRADE_MIN;
  AddCard(&head, KeyWord, J, &jval, "Minimum value for GRADE column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.GRADE+1));
  jval = GRADE_MAX;
  AddCard(&head, KeyWord, J, &jval, "Maximum value for GRADE column");


  if((indxcol.SWTRIG=ColNameMatch(CLNM_SWTRIG, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_SWTRIG);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable,CLNM_SWTRIG, CARD_COMM_SWTRIG, "9B",TNONE);    
      indxcol.SWTRIG=ColNameMatch(CLNM_SWTRIG, &outtable);    
    }

  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: computed PHA.", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
      
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);



  for(jj=0; jj<33;jj++)
    global.grade_tot[jj]=0;

  
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
	  s_cap = BVEC(outtable, n,indxcol.S_CAP);
	  det_id = BVEC(outtable, n,indxcol.DET_ID);
	  if(read_badpos)
	    badpos = XVEC1BYTE(outtable, n,indxcol.BADPOS);
	  else
	    badpos = 0;
	  if(read_hotpos)
	    hotpos = XVEC1BYTE(outtable, n,indxcol.HOTPOS);
	  else
	    hotpos = 0;
	  
	  numrise = JVEC(outtable,n,indxcol.NUMRISE);
	  denrise = JVEC(outtable,n,indxcol.DENRISE);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto reco_end;
	  }

	  if(s_cap<S_CAP_MIN||s_cap>S_CAP_MAX){
	    headas_chat(NORMAL,"%s: Error: S_CAP=%d out of range value.\n", global.taskname, s_cap);
	    goto reco_end;
	  }


	  switch (det_id) {  
	  case KWVL_DET_ID_0:
	    offset_det = &offset_det0[0][0];
	    break;
	  case KWVL_DET_ID_1:
	    offset_det = &offset_det1[0][0];
	    break;
	  case KWVL_DET_ID_2:
	    offset_det = &offset_det2[0][0];
	    break;
	  case KWVL_DET_ID_3:
	    offset_det = &offset_det3[0][0];
	    break;
	  default:
	    headas_chat(NORMAL, "%s: Error: Unexpected  DET_ID value (%d) in input file.\n",global.taskname, det_id);
	    goto reco_end;
	  }

	  /* offset_det[ (rawx*DET_ROWS) + rawy ]  <=>  offset_det<x>[rawx][rawy] */


	  /* Compute 'evtthr' and 'timerise' values */
	  if(global.phaparfile){
	    for (jj=0; jj< PHAS_MOL; jj++)
	      {
		x = rawx + neigh_x[jj];
		y = rawy + neigh_y[jj];
		
		if(x<RAWX_MIN||x>RAWX_MAX||y<RAWY_MIN||y>RAWY_MAX){
		  evtthr[jj] = MAX_EVTTHR;
		  timerise[jj] = 0;		
		}
		else{
		  evtthr[jj] = phapardata[det_id][x][y].evtthr;
		  timerise[jj] = phapardata[det_id][x][y].timerise;
		}
	      }
	  }
	  else{
	    for (jj=0; jj< PHAS_MOL; jj++)
	      {
		evtthr[jj] = global.par.evtthr;
		timerise[jj] = global.par.timerise;
	      }
	  }


	  /* Compute RAWPHAS values */
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      IVECVEC(outtable,n,indxcol.RAWPHAS,jj) = IVECVEC(outtable,n,indxcol.POSTPHAS,jj) - IVECVEC(outtable,n,indxcol.PREPHAS,jj);
	    }


	  /* Compute OFFPHAS values */

	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      x = rawx + neigh_x[jj];
	      y = rawy + neigh_y[jj];
	      
	      if(x<RAWX_MIN||x>RAWX_MAX||y<RAWY_MIN||y>RAWY_MAX){
		EVECVEC(outtable,n,indxcol.OFFPHAS,jj) = IVECVEC(outtable,n,indxcol.RAWPHAS,jj);
	      }
	      else{
		if(offset_det[x*DET_ROWS + y].set!=1){
		  headas_chat(NORMAL,"%s: Error: OFFSET values for DET_ID=%d RAWX=%d RAWY=%d not found.\n", global.taskname,det_id,x,y);
		  goto reco_end;
		}

		EVECVEC(outtable,n,indxcol.OFFPHAS,jj) = (float)IVECVEC(outtable,n,indxcol.RAWPHAS,jj) - offset_det[x*DET_ROWS + y].offset[s_cap];
	      }

	    }


	  /* Compute TRPHAS values */ 

	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      tr_corr = (denrise!=0) ? (1.0 + ((double)numrise/(double)denrise) * timerise[jj]) : 1;
	      headas_chat(CHATTY, "%s: Info: TIME=%f DET_ID=%d RAWXX=%d RAWY=%d (TRPHAS[%d]) time of rise correction=%f\n",global.taskname,time,det_id,rawx,rawy,jj,tr_corr);
	      EVECVEC(outtable,n,indxcol.TRPHAS,jj) = (float)( EVECVEC(outtable,n,indxcol.OFFPHAS,jj) * tr_corr );
	    }


	  /* Store badpos and hotpos values in a 9 elements array */
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      if(jj<4){
		if( (badpos & neigh_pix_badpos[jj]) || (hotpos & neigh_pix_badpos[jj]) )
		  badpos9bit[jj]=1;
		else
		  badpos9bit[jj]=0;
	      }
	      else if(jj==4){
		badpos9bit[jj]=0;
	      }
	      else{
		if( (badpos & neigh_pix_badpos[jj-1]) || (hotpos & neigh_pix_badpos[jj-1]) )
		  badpos9bit[jj]=1;
		else
		  badpos9bit[jj]=0;	
	      }
	    }


	  /* Check event software threshold */ 
	  
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      pha_tr = EVECVEC(outtable,n,indxcol.TRPHAS,jj);
	      
	      if( (pha_tr < evtthr[jj]) || (badpos9bit[jj]==1) ){
		handle[jj]=2;
	      }
	      else{
		handle[jj]=1;
	      }
	    }


	  /* Compute PHAS values */ 

	  n_downthr = 0;
	  sum_downthr = 0.0;

	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      pha_tr = EVECVEC(outtable,n,indxcol.TRPHAS,jj);
	     
	      if(badpos9bit[jj]==0){
		if( pha_tr < evtthr[jj] ){
		  n_downthr++;
		  sum_downthr += pha_tr;
		}
	      }
	    }
	  
	  avg_downthr = (n_downthr>0) ? sum_downthr/n_downthr : 0;
	  headas_chat(CHATTY, "%s: Info: TIME=%f DET_ID=%d RAWXX=%d RAWY=%d average not triggered pixels=%f\n",global.taskname,time,det_id,rawx,rawy,avg_downthr);
	  
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      pha_tr = EVECVEC(outtable,n,indxcol.TRPHAS,jj);

	      if(badpos9bit[jj]>0)
		EVECVEC(outtable,n,indxcol.PHAS,jj) = 0;
	      else if(pha_tr<evtthr[jj])
		EVECVEC(outtable,n,indxcol.PHAS,jj) = (float)pha_tr;
	      else
		EVECVEC(outtable,n,indxcol.PHAS,jj) = (float)(pha_tr-avg_downthr);
 	    }


	  /* Compute SWTRIG values */ 

	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      if(handle[jj]==1){
		BVECVEC(outtable,n,indxcol.SWTRIG,jj) = 1;
	      }
	      else{
		BVECVEC(outtable,n,indxcol.SWTRIG,jj) = 0;
	      }
	    }

	  
/* 	  /\* Compute PHA and SURR values *\/ */

/* 	  pha_d=0.0; */
/* 	  spha_d=0.0; */
/* 	  for (jj=0; jj< PHAS_MOL; jj++) */
/* 	    { */
/* 	      if(BVECVEC(outtable,n,indxcol.SWTRIG,jj)>0)  */
/* 		pha_d+= EVECVEC(outtable,n,indxcol.PHAS,jj); */
/* 	      else */
/* 		spha_d+= EVECVEC(outtable,n,indxcol.PHAS,jj); */
/*  	    } */


/* 	  pha = (int)floor(spha_d + 0.5); */
/* 	  JVEC(outtable,n,indxcol.SURR) = pha; */

/* 	  pha = (int)floor(pha_d + 0.5); */
/* 	  JVEC(outtable,n,indxcol.PHA) = pha; */


	  /* Compute GRADE values */
	    
	  found=0;
	  grade=KWVL_GRADENULL;
	  
	  for(ii=0; ii<global.graderows && grade==KWVL_GRADENULL; ii++)
	    {
	      for(jj=0; jj<PHAS_MOL && found==0; jj++) 
		if((handle[jj]!=global.gradevalues[ii].grade[jj]) && (global.gradevalues[ii].grade[jj]))
		  found=1;
	      
	      if(!found)
		grade=global.gradevalues[ii].gradeid;
	      else
		found=0;	      
	    }
	  
	  IVEC(outtable, n, indxcol.GRADE) = grade;

	  
	  if(grade == KWVL_GRADENULL)
	    global.grade_tot[32]++;
	  else
	    global.grade_tot[grade]++;
	  

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


  /* Delete temporary columns  */
  if(global.par.cleancols){

    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_RAWPHAS, &colnum, &status))
      if(fits_delete_col(ounit, colnum, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_RAWPHAS);
	  goto reco_end;
	}
    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_OFFPHAS, &colnum, &status))
      if(fits_delete_col(ounit, colnum, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_OFFPHAS);
	  goto reco_end;
	}
    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_TRPHAS, &colnum, &status))
      if(fits_delete_col(ounit, colnum, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_TRPHAS);
	  goto reco_end;
	}

  }


  /* Write grade events statistics */
  
  headas_chat(NORMAL, "---------------------------------------------------\n");
  headas_chat(NORMAL, "%s: Info: \t\t NuSTAR GRADES\n", global.taskname);
  headas_chat(NORMAL, "%s: Info: Total events                   %15d\n", global.taskname, totrows);

  for(jj=0; jj<33;jj++){
    per_grade[jj] = 100.*(double)global.grade_tot[jj]/(double)(totrows);
  }
  
  for(jj=0; jj<33;jj++){
    headas_chat(NORMAL, "%s: Info: Total events with grade %2d     %15d%10.4f%%\n", global.taskname, jj, global.grade_tot[jj], per_grade[jj]);
  }



  return OK;
  
 reco_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* ComputePHAandGRADE */


/*
 *      GetGrades
 *
 *	DESCRIPTION:
 *           Routine to get GRADE data from grade input file
 *                            
 *      FUNCTION CALL:
 *             int headas_chat(int, char *, ...);
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, int *status );
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header, Bintable_t *table,
 *     			                  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int GetColNameIndx(const Bintable_t *table, const char *ColName);
 *             void *calloc(size_t nmemb, size_t size);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *               
 *
 *      CHANGE HISTORY:
 *        0.1.0: - NS 25/10/10 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 *
 */
int GetGrades(long extno)
{
  int            n, status=OK, jj;
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  GradeCol_t     gradecol; 
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */
  FitsFileUnit_t gradeunit=NULL;        /* Bias file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only grade file */
  if ((gradeunit=OpenReadFitsFile(global.par.gradefile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname,global.par.gradefile);
  
  /* Check whether it is a NuSTAR File */
/*   ISNUSTARFILE_WARNING(NORMAL, gradeunit, global.par.gradefile, global.taskname); */
  
  if(extno != -1)
    {
      /* move to GRADES extension */
      if (fits_movabs_hdu(gradeunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_GRADES);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gradefile); 
	  goto get_end;
	} 
    }
  else
    {
      if (fits_movnam_hdu(gradeunit,ANY_HDU,KWVL_EXTNAME_GRADES,0,&status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_GRADES);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gradefile); 
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

  if ((gradecol.GRADEID = GetColNameIndx(&table, CLNM_GRADEID)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }

  if ((gradecol.GRADE = GetColNameIndx(&table, CLNM_GRADE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  global.graderows=table.MaxRows;
  /* Allocate memory to storage all coefficients */
  global.gradevalues=(GradeRow_t *)calloc(table.MaxRows, sizeof(GradeRow_t));
  
  /* Read blocks of bintable rows from input bias file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  while (ReadBintable(gradeunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	  global.gradevalues[n].gradeid=IVEC(table,n,gradecol.GRADEID);
	   /* Get grades */
	  for (jj=0; jj< PHAS_MOL; jj++)
	    global.gradevalues[n].grade[jj] = IVECVEC(table,n,gradecol.GRADE,jj);
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(gradeunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
  
  return NOT_OK;
  
} /* GetGrades */


/*
 *      ReadOffData
 *
 *	DESCRIPTION:
 *           Routine to get OFFSET data from a selected extension of the offset input file
 *                            
 *      FUNCTION CALL:
 *             int headas_chat(int, char *, ...);
 *             FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header, Bintable_t *table,
 *     			                  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int GetColNameIndx(const Bintable_t *table, const char *ColName);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *               
 *
 *      CHANGE HISTORY:
 *        0.1.0: - NS 25/10/10 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 *
 */
static int ReadOffData(FitsFileUnit_t inunit, OffsetInfo_t offset_det[DET_PIXS][DET_ROWS])
{
  int            n, jj, rawx, rawy;
  unsigned       irawx, irawy, ioffset;
  unsigned       FromRow, ReadRows, nCols; 
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  head = RetrieveFitsHeader(inunit);
  
  /* Read offset bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: empty OFFSET extension in %s file.\n", global.taskname, global.par.offsetfile);
      goto readoff_end;
    }

  /* Get columns index from name */

  if ((irawx = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.offsetfile);
      goto readoff_end;
    }

  if ((irawy = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.offsetfile);
      goto readoff_end;
    }

  if ((ioffset = GetColNameIndx(&table, CLNM_OFFSET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.offsetfile);
      goto readoff_end;
    }

  if (table.Multiplicity[ioffset] != CAP_DIM)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, CLNM_OFFSET, table.Multiplicity[ioffset]); 
      headas_chat(NORMAL,"%s: Error: but should be: %d \n", global.taskname, CAP_DIM); 
      goto readoff_end;
    }



  /* Read blocks of bintable rows from input offset file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(inunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,irawx);
	  rawy = BVEC(table,n,irawy);
	  
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto readoff_end;
	  }

	  for (jj=0; jj< CAP_DIM; jj++)
	    {
	      offset_det[rawx][rawy].offset[jj]=EVECVEC(table,n,ioffset,jj);
	    }

	  offset_det[rawx][rawy].set=1;

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  return OK;
  
 readoff_end:
  
  return NOT_OK;
}


/*
 *      GetOffset
 *
 *	DESCRIPTION:
 *           Routine to get OFFSET data from offset input file
 *                            
 *      FUNCTION CALL:
 *             int headas_chat(int, char *, ...);
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, int *status );
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header, Bintable_t *table,
 *     			                  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int GetColNameIndx(const Bintable_t *table, const char *ColName);
 *             void *calloc(size_t nmemb, size_t size);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *               
 *
 *      CHANGE HISTORY:
 *        0.1.0: - NS 25/10/10 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 *
 */
int GetOffset(OffsetInfo_t offset_det0[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det1[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det2[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det3[DET_PIXS][DET_ROWS])
{
  int            status=OK, inExt, outExt;
  char		 extname[FLEN_KEYWORD];
  char		 detnam[FLEN_VALUE];
  BOOL           offdet0=0, offdet1=0, offdet2=0, offdet3=0;
  FitsCard_t     *card;
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */
  FitsFileUnit_t inunit=NULL;          /* Offset file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  
  /* Open read only offset file */
  if ((inunit=OpenReadFitsFile(global.par.offsetfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.offsetfile);
      goto getoff_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname,global.par.offsetfile);
  

  /* Get number of hdus in input offset file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.offsetfile);
      goto getoff_end;
    }


  outExt=1;
  status=OK;
  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.offsetfile);
	  goto getoff_end;
	}
      
      /* Retrieve header pointer */    
      head=RetrieveFitsHeader(inunit);    


      if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	strcpy(extname, card->u.SVal);
      else
	strcpy(extname, "NONAME");

      if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	strcpy(detnam, card->u.SVal);
      else
	strcpy(detnam, "-");


      if( !strcmp(extname,KWVL_EXTNAME_OFFSET) && !strcmp(detnam,KWVL_DETNAM_DET0) ){
	if(ReadOffData(inunit, offset_det0)){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, global.par.offsetfile);
	  goto getoff_end;
	}
	offdet0=1;
      }
      
      if( !strcmp(extname,KWVL_EXTNAME_OFFSET) && !strcmp(detnam,KWVL_DETNAM_DET1) ){
	if(ReadOffData(inunit, offset_det1)){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, global.par.offsetfile);
	  goto getoff_end;
	}
	offdet1=1;
      }

      if( !strcmp(extname,KWVL_EXTNAME_OFFSET) &&  !strcmp(detnam,KWVL_DETNAM_DET2) ){
	if(ReadOffData(inunit, offset_det2)){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, global.par.offsetfile);
	  goto getoff_end;
	}
	offdet2=1;
      }

      if( !strcmp(extname,KWVL_EXTNAME_OFFSET) &&  !strcmp(detnam,KWVL_DETNAM_DET3) ){
	if(ReadOffData(inunit, offset_det3)){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, global.par.offsetfile);
	  goto getoff_end;
	}
	offdet3=1;
      }

      
      outExt++;
    }
  
    
  /* Close offset file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.offsetfile);
      goto getoff_end;
    }
  

  if(!offdet0){
    headas_chat(NORMAL, "%s: Error: Unable to get offset info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_0, global.par.offsetfile);
    goto getoff_end;
  }
  if(!offdet1){
    headas_chat(NORMAL, "%s: Error: Unable to get offset info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_1, global.par.offsetfile);
    goto getoff_end;
  }
  if(!offdet2){
    headas_chat(NORMAL, "%s: Error: Unable to get offset info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_2, global.par.offsetfile);
    goto getoff_end;
  }
  if(!offdet3){
    headas_chat(NORMAL, "%s: Error: Unable to get offset info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_3, global.par.offsetfile);
    goto getoff_end;
  }


  return OK;
  
 getoff_end:
  
  return NOT_OK;
  
} /* GetOffset */



/*
 *
 *      ReadPhaParFile
 *
 *	DESCRIPTION:
 *           Routine to read input phaparfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPhaParFile(PhaParData_t phapardata[4][DET_PIXS][DET_ROWS]){
  

  char        phaparfile0[PIL_LINESIZE], phaparfile1[PIL_LINESIZE], phaparfile2[PIL_LINESIZE], phaparfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB phapar filename */  
  if ( !strcasecmp(global.par.phaparfile,DF_CALDB) )
    {
      /* Retrieve DET0 phapar filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PHAPAR_DSET, phaparfile0, HD_EXPR, &extfile0, global.evt.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for phaparfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadPhaParFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 phapar filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PHAPAR_DSET, phaparfile1, HD_EXPR, &extfile1, global.evt.instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for phaparfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadPhaParFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 phapar filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PHAPAR_DSET, phaparfile2, HD_EXPR, &extfile2, global.evt.instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for phaparfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadPhaParFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 phapar filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PHAPAR_DSET, phaparfile3, HD_EXPR, &extfile3, global.evt.instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for phaparfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadPhaParFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(phaparfile0, global.par.phaparfile);
    strcpy(phaparfile1, global.par.phaparfile);
    strcpy(phaparfile2, global.par.phaparfile);
    strcpy(phaparfile3, global.par.phaparfile);
  }

  /* Retrieve DET0 phapar info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for PHAPAR data of detector %s.\n", global.taskname, phaparfile0, KWVL_DETNAM_DET0);
  if( ReadPhaParInfo(phapardata[0], phaparfile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read PHAPAR data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input phaparfile: %s.\n", global.taskname, phaparfile0);
      goto ReadPhaParFile_end;
    }

  /* Retrieve DET1 phapar info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for PHAPAR data of detector %s.\n", global.taskname, phaparfile1, KWVL_DETNAM_DET1);
  if( ReadPhaParInfo(phapardata[1], phaparfile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read PHAPAR data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input phaparfile: %s.\n", global.taskname, phaparfile1);
      goto ReadPhaParFile_end;
    }

  /* Retrieve DET2 phapar info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for PHAPAR data of detector %s.\n", global.taskname, phaparfile2, KWVL_DETNAM_DET2);
  if( ReadPhaParInfo(phapardata[2], phaparfile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read PHAPAR data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input phaparfile: %s.\n", global.taskname, phaparfile2);
      goto ReadPhaParFile_end;
    }

  /* Retrieve DET3 phapar info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for PHAPAR data of detector %s.\n", global.taskname, phaparfile3, KWVL_DETNAM_DET3);
  if( ReadPhaParInfo(phapardata[3], phaparfile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read PHAPAR data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input phaparfile: %s.\n", global.taskname, phaparfile3);
      goto ReadPhaParFile_end;
    }



  return OK;
  
 ReadPhaParFile_end:
  
  return NOT_OK;

} /* ReadPhaParFile */


/*
 *
 *      ReadPhaParInfo
 *
 *	DESCRIPTION:
 *           Routine to get phapar coefficient from input phaparfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPhaParInfo(PhaParData_t phapardata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam){

  int                n, i=0, j=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  PhaParCol_t        phaparcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      phapardata[i][j].evtthr  = MAX_EVTTHR;
      phapardata[i][j].timerise = 0.0;
    }
  }


  /* Open read only phapar file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPhaParInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadPhaParInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input phaparfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadPhaParInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to PHAPAR extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadPhaParInfo_end;
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

	  if( !strcmp(r_extname,KWVL_EXTNAME_PHAPAR) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_PHAPAR,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadPhaParInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read phapar bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadPhaParInfo_end;
    }


  /* Get columns index from name */

  /* RAWX */
  if ((phaparcol.RAWX=ColNameMatch(CLNM_RAWX, &table)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename); 
      goto ReadPhaParInfo_end;
    }

  /* RAWY */
  if ((phaparcol.RAWY=ColNameMatch(CLNM_RAWY, &table)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename); 
      goto ReadPhaParInfo_end;
    }

  /* EVTHTR */
  if ((phaparcol.EVTTHR = GetColNameIndx(&table, CLNM_EVTTHR)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EVTTHR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPhaParInfo_end;
    }

  /* TIMERISE */
  if ((phaparcol.TIMERISE = GetColNameIndx(&table, CLNM_TIMERISE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIMERISE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPhaParInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,phaparcol.RAWX);
	  rawy = BVEC(table,n,phaparcol.RAWY);

	  /* Check input data */
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadPhaParInfo_end;
	  }

	  phapardata[rawx][rawy].evtthr  = IVEC(table,n,phaparcol.EVTTHR);
	  phapardata[rawx][rawy].timerise = EVEC(table,n,phaparcol.TIMERISE);
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
      goto ReadPhaParInfo_end;
    }


  return OK;
  
 ReadPhaParInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadPhaParInfo */
