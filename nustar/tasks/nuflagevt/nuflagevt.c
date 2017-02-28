/*
 * 
 *	nuflagevt.c
 *
 *	INVOCATION:
 *
 *		nuflagevt [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 12/07/12 - First version
 *        0.1.1 - NS 26/07/12 - Flags Event with PI out of range
 *        0.1.2 - NS 17/09/12 - Modified depth cut flagging conditions
 *        0.1.3 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuflagevt  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuflagevt.h"


/********************************/
/*         definitions          */
/********************************/

#define NUFLAGEVT_C
#define NUFLAGEVT_VERSION      "0.1.3"
#define PRG_NAME                 "nuflagevt"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuflagevt_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuflagevt.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuflagevt_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 12/07/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagevt_getpar()
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
  
  /* Input Depth Cut File Name */
  if(PILGetFname(PAR_DEPTHCUTFILE, global.par.depthcutfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DEPTHCUTFILE);
      goto Error;	
    }
  
  /* Input Event Cut File Name */
  if(PILGetFname(PAR_EVTCUTFILE, global.par.evtcutfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVTCUTFILE);
      goto Error;	
    }

  get_history(&global.hist);
  nuflagevt_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuflagevt_getpar */


/*
 *	nuflagevt_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuflagevt_checkinput();
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
 *        0.1.0 - NS 12/07/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagevt_work()
{
  int                 status = OK, inExt, outExt, evExt;
  static DepthInfo_t  depthinfo[8][DET_PIXS][DET_ROWS];
  EvtCutInfo_t        evtcutinfo[4];
  FitsHeader_t        head;
  FitsCard_t          *card;
  FitsFileUnit_t      inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 


  if(nuflagevt_checkinput())
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


  /* Read depth cut data from depthcutfile input */
  if( ReadDepthCutFile(depthinfo, &global.evt) )
    {
      headas_chat(NORMAL,"%s: Error: unable to get depth cut data from input depthcutfile.\n",global.taskname);      
      goto Error;
    }

  /* Read event cut data from evtcutfile input */
  if( ReadEvtCutFile(evtcutinfo, &global.evt) )
    {
      headas_chat(NORMAL,"%s: Error: unable to get event cut data from input evtcutfile.\n",global.taskname);      
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


  /* Compute Depth Cut */
  if (ComputeDepthCut(inunit, outunit, depthinfo, evtcutinfo))
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute depth cut.\n", global.taskname);
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
} /* nuflagevt_work */


/*
 *	nuflagevt
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
 *             void nuflagevt_getpar(void);
 * 	       void nuflagevt_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 12/07/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuflagevt()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUFLAGEVT_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuflagevt_getpar() == OK) 
    {
      
      if ( nuflagevt_work()) 
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
  
} /* nuflagevt */


/*
 *	nuflagevt_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 12/07/12 - First version
 *
 */
void nuflagevt_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
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
} /* nuflagevt_info */


/*
 *	nuflagevt_checkinput
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
 *        0.1.0: - NS 12/07/12 - First version
 *
 */
int nuflagevt_checkinput(void)
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
 *      ComputeDepthCut
 *
 *	DESCRIPTION:
 *           
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
 *        0.1.0: - NS 12/07/12 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeDepthCut(FitsFileUnit_t evunit, FitsFileUnit_t ounit, DepthInfo_t depth[8][DET_PIXS][DET_ROWS], EvtCutInfo_t evtcut[4])
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                rawx, rawy, det_id, grade, prephas5;
  int                totrows, newstatus;
  JTYPE              pi, surrpi;
  double             time, prior, reset;
  char               comm[256];
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

  /* GRADE */
  if ((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
 
  /* PI */
  if ((indxcol.PI=ColNameMatch(CLNM_PI, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PI);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* SURRPI */
  if ((indxcol.SURRPI=ColNameMatch(CLNM_SURRPI, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_SURRPI);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* PRIOR */
  if ((indxcol.PRIOR=ColNameMatch(CLNM_PRIOR, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PRIOR);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* RESET */
  if ((indxcol.RESET=ColNameMatch(CLNM_RESET, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RESET);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* PREPHAS */
  if ((indxcol.PREPHAS=ColNameMatch(CLNM_PREPHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PREPHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* STATUS (added if needed) */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
    }

  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: computed PHA.", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
     
      if(newstatus)
	{
	  sprintf(comm, "Added %s column", CLNM_STATUS);
	  AddHistory(&head, comm);
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
          grade = IVEC(outtable,n,indxcol.GRADE);
	  pi = JVEC(outtable,n,indxcol.PI);
	  surrpi = JVEC(outtable,n,indxcol.SURRPI);
	  prior = DVEC(outtable,n,indxcol.PRIOR);
	  reset = DVEC(outtable,n,indxcol.RESET);
	  prephas5 = IVECVEC(outtable,n,indxcol.PREPHAS,4);

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
	  if(newstatus>0){
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	  }
	  else{
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_DEPTHCUT;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_BASELINECUT;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_PRIORRESET;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_PRIORCUT;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_RESETCUT;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_OUTRANGE_PI;
	  }

	  /* Check PI value */
	  if( pi>=0 && pi<E_DEPTH_DIM )
	    {

	      /* Check if event fails baseline cut */
	      if( (prephas5<evtcut[det_id].baseline1 && pi<evtcut[det_id].pi_baseline) || (prephas5>evtcut[det_id].baseline2) )
		XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_BASELINECUT;
	      

	      /* Only FPMA */
	      if(!strcasecmp(global.evt.instrume,KWVL_INSTRUME_FPMA)){
		/* Check if event fails prior/reset cut */
		if( ( prior>evtcut[det_id].prior2 && prior<evtcut[det_id].prior3 && pi<evtcut[det_id].pi_prior ) ||
		    (reset>evtcut[det_id].reset2 && reset<evtcut[det_id].reset3 && pi>evtcut[det_id].pi1_reset && pi<evtcut[det_id].pi2_reset) )
		  XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_PRIORRESET;	      
	      }

	      /* Check if event fails prior cut */
	      if(prior<evtcut[det_id].prior1)
		XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_PRIORCUT;
	      
	      /* Check if event fails reset cut */   
	      if(reset<evtcut[det_id].reset1)
		XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_RESETCUT;
	      
	    
	      
	      
	      /* Check if event fails depth cut */
	      if (grade == 0)
		{
		  if ( ( (depth[det_id][rawx][rawy].e2[pi] > 0) && (depth[det_id][rawx][rawy].e3[pi] > 0) && (surrpi <= depth[det_id][rawx][rawy].e1[pi])) ||
		       ( (depth[det_id][rawx][rawy].e2[pi] < 0) && (depth[det_id][rawx][rawy].e3[pi] < 0) && ((surrpi <= depth[det_id][rawx][rawy].e1[pi] && surrpi > depth[det_id][rawx][rawy].e2[pi]) || (surrpi <= depth[det_id][rawx][rawy].e3[pi])) ) 
		       )
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_DEPTHCUT;
		    } 
		}
              else {
		if ((grade == 1) || (grade == 2) || (grade == 3) || (grade == 4)) {
		  
		  if( ( (depth[det_id+4][rawx][rawy].e2[pi] > 0) && (depth[det_id+4][rawx][rawy].e3[pi] > 0) && (surrpi <= depth[det_id+4][rawx][rawy].e1[pi])) ||
		      ( (depth[det_id+4][rawx][rawy].e2[pi] < 0) && (depth[det_id+4][rawx][rawy].e3[pi] < 0) && ((surrpi <= depth[det_id+4][rawx][rawy].e1[pi] && surrpi > depth[det_id+4][rawx][rawy].e2[pi]) || (surrpi <= depth[det_id+4][rawx][rawy].e3[pi])) )
		      )
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_DEPTHCUT;
		    } 
		}	
              } 
	    }
	  else
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_OUTRANGE_PI;
	      
	      headas_chat(CHATTY,"%s: Warning: TIME=%f RAWX=%d RAWY=%d -> PI=%d out of range.\n", global.taskname, time, rawx, rawy, pi); 
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
  
} /* ComputeDepthCut */


/*
 *
 *      ReadDepthCutFile
 *
 *	DESCRIPTION:
 *           Routine to read input depthcutfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadDepthCutFile(DepthInfo_t depth[8][DET_PIXS][DET_ROWS], const EVTInfo_t *evt){  
  

  char        depthcutfile0[PIL_LINESIZE], depthcutfile1[PIL_LINESIZE], depthcutfile2[PIL_LINESIZE], depthcutfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;
  char        depthcutfile4[PIL_LINESIZE], depthcutfile5[PIL_LINESIZE], depthcutfile6[PIL_LINESIZE], depthcutfile7[PIL_LINESIZE];
  long        extfile4=-1, extfile5=-1, extfile6=-1, extfile7=-1;

  /* Derive CALDB depthcut filename */  
  if ( !strcasecmp(global.par.depthcutfile,DF_CALDB) )
    {
      /* Retrieve DET0 SINGLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile0, "GRADE.eq.0", &extfile0, evt->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=0).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadDepthCutFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 SINGLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile1, "GRADE.eq.0", &extfile1, evt->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=0).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadDepthCutFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 SINGLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile2, "GRADE.eq.0", &extfile2, evt->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=0).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadDepthCutFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 SINGLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile3, "GRADE.eq.0", &extfile3, evt->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=0).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadDepthCutFile_end;
      	}
      extfile3++;

      /* Retrieve DET0 DOUBLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile4, "GRADE.eq.1", &extfile4, evt->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=1).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadDepthCutFile_end;
      	}
      extfile4++;

      /* Retrieve DET1 DOUBLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile5, "GRADE.eq.1", &extfile5, evt->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=1).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadDepthCutFile_end;
      	}
      extfile5++;

      /* Retrieve DET2 DOUBLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile6, "GRADE.eq.1", &extfile6, evt->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=1).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadDepthCutFile_end;
      	}
      extfile6++;

      /* Retrieve DET3 DOUBLE depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_DEPTHCUT_DSET, depthcutfile7, "GRADE.eq.1", &extfile7, evt->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for depthcutfile parameter (detector=%s grade=1).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadDepthCutFile_end;
      	}
      extfile7++;

    }
  else{
    strcpy(depthcutfile0, global.par.depthcutfile);
    strcpy(depthcutfile1, global.par.depthcutfile);
    strcpy(depthcutfile2, global.par.depthcutfile);
    strcpy(depthcutfile3, global.par.depthcutfile);

    strcpy(depthcutfile4, global.par.depthcutfile);
    strcpy(depthcutfile5, global.par.depthcutfile);
    strcpy(depthcutfile6, global.par.depthcutfile);
    strcpy(depthcutfile7, global.par.depthcutfile);
  }

  /* Retrieve DET0 SINGLE depth info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile0, KWVL_GRADE_DEPTH_0S); 
  if( ReadDepthInfo(depth[0], depthcutfile0, extfile0, KWVL_GRADE_DEPTH_0S) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile0);
      goto ReadDepthCutFile_end;
    }

  /* Retrieve DET1 SINGLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile1, KWVL_GRADE_DEPTH_1S); 
  if( ReadDepthInfo(depth[1], depthcutfile1 ,extfile1, KWVL_GRADE_DEPTH_1S) ) 
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile1);
      goto ReadDepthCutFile_end;
    }

  /* Retrieve DET2 SINGLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile2, KWVL_GRADE_DEPTH_2S); 
 if( ReadDepthInfo(depth[2], depthcutfile2, extfile2, KWVL_GRADE_DEPTH_2S) ) 
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile2);
      goto ReadDepthCutFile_end;
    }
 

  /* Retrieve DET3 SINGLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile3, KWVL_GRADE_DEPTH_3S);  
  if( ReadDepthInfo(depth[3], depthcutfile3, extfile3, KWVL_GRADE_DEPTH_3S) ) 
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile3);
      goto ReadDepthCutFile_end;
    }

    /* Retrieve DET0 DOUBLE depth info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile4, KWVL_GRADE_DEPTH_0D);
  if( ReadDepthInfo(depth[4], depthcutfile4, extfile4, KWVL_GRADE_DEPTH_0D) ) 
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile4);
      goto ReadDepthCutFile_end;
    }

  /* Retrieve DET1 DOUBLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile5, KWVL_GRADE_DEPTH_1D);
  if( ReadDepthInfo(depth[5], depthcutfile5 ,extfile5, KWVL_GRADE_DEPTH_1D) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile5);
      goto ReadDepthCutFile_end;
    }

  /* Retrieve DET2 DOUBLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile6, KWVL_GRADE_DEPTH_2D);
  if( ReadDepthInfo(depth[6], depthcutfile6, extfile6, KWVL_GRADE_DEPTH_2D) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile6);
      goto ReadDepthCutFile_end;
    }

  /* Retrieve DET3 DOUBLE depth info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for depth cut data of detector %s.\n", global.taskname, depthcutfile7, KWVL_GRADE_DEPTH_3D);
  if( ReadDepthInfo(depth[7], depthcutfile7, extfile7, KWVL_GRADE_DEPTH_3D) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read depth cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input depth file: %s.\n", global.taskname, depthcutfile7);
      goto ReadDepthCutFile_end;
    }

  return OK;
  
 ReadDepthCutFile_end:
  
  return NOT_OK;

} /* ReadDepthCutFile */


/*
 *
 *      ReadDepthInfo
 *
 *	DESCRIPTION:
 *           Routine to get depth cut data from input depthcutfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadDepthInfo(DepthInfo_t depth[DET_PIXS][DET_ROWS], char *filename, long extno, char *grade_de){

  int                n, status=OK, found=NOT_OK; 
  int                inExt, totExt, rawx, rawy;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_grade_de[FLEN_VALUE];
  DepthCol_t         depthcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Open read only depth file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadDepthInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input depthcutfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadDepthInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to DEPTH extension with GRADE_DE=<grade_de> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadDepthInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(gunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_GRADE_DEPTH, &card))  
	    strcpy(r_grade_de, card->u.SVal);
	  else
	    strcpy(r_grade_de, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_DEPTHCUT) && !strcmp(r_grade_de,grade_de) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with GRADE_DE='%s'\n", global.taskname,KWVL_EXTNAME_DEPTHCUT,grade_de);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadDepthInfo_end;
	}
    }

  head = RetrieveFitsHeader(gunit);
  
  /* Read depth bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }


  /* Get columns index from name */

  if ((depthcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }

  if ((depthcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }

  if ((depthcol.E1 = GetColNameIndx(&table, CLNM_E1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_E1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }

  if ((depthcol.E2 = GetColNameIndx(&table, CLNM_E2)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_E2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }

  if ((depthcol.E3 = GetColNameIndx(&table, CLNM_E3)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_E3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadDepthInfo_end;
    }

  /* Check columns multiplicity */

  if(table.Multiplicity[depthcol.E1]!=E_DEPTH_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_E1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadDepthInfo_end;
    }

  if(table.Multiplicity[depthcol.E2]!=E_DEPTH_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_E2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadDepthInfo_end;
    }

  if(table.Multiplicity[depthcol.E3]!=E_DEPTH_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_E3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadDepthInfo_end;
    }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;

  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,depthcol.RAWX);
	  rawy = BVEC(table,n,depthcol.RAWY);
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadDepthInfo_end;
	  }
	  JVECVEC_ARRAY_READ(depth[rawx][rawy].e1, E_DEPTH_DIM, table, n, depthcol.E1);
	  JVECVEC_ARRAY_READ(depth[rawx][rawy].e2, E_DEPTH_DIM, table, n, depthcol.E2);
          JVECVEC_ARRAY_READ(depth[rawx][rawy].e3, E_DEPTH_DIM, table, n, depthcol.E3);
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
      goto ReadDepthInfo_end;
    }

  return OK;
  
 ReadDepthInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadDepthInfo */




/*
 *
 *      ReadEvtCutFile
 *
 *	DESCRIPTION:
 *           Routine to read input evtcutfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtCutFile(EvtCutInfo_t evtcut[4], const EVTInfo_t *evt){
  

  char        evtcutfile0[PIL_LINESIZE], evtcutfile1[PIL_LINESIZE], evtcutfile2[PIL_LINESIZE], evtcutfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB gain filename */  
  if ( !strcasecmp(global.par.evtcutfile,DF_CALDB) )
    {
      /* Retrieve DET0 depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_EVTCUT_DSET, evtcutfile0, HD_EXPR, &extfile0, evt->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for evtcutfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadEvtCutFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_EVTCUT_DSET, evtcutfile1, HD_EXPR, &extfile1, evt->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for evtcutfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadEvtCutFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_EVTCUT_DSET, evtcutfile2, HD_EXPR, &extfile2, evt->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for evtcutfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadEvtCutFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 depth filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_EVTCUT_DSET, evtcutfile3, HD_EXPR, &extfile3, evt->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for evtcutfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadEvtCutFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(evtcutfile0, global.par.evtcutfile);
    strcpy(evtcutfile1, global.par.evtcutfile);
    strcpy(evtcutfile2, global.par.evtcutfile);
    strcpy(evtcutfile3, global.par.evtcutfile);
  }

  /* Retrieve DET0 event cut info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for event cut data of detector %s.\n", global.taskname, evtcutfile0, KWVL_DETNAM_DET0);
  if( ReadEvtCutInfo(&evtcut[0], evtcutfile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read event cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input event cut file: %s.\n", global.taskname, evtcutfile0);
      goto ReadEvtCutFile_end;
    }

  /* Retrieve DET1 event cut info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for event cut data of detector %s.\n", global.taskname, evtcutfile1, KWVL_DETNAM_DET1);
  if( ReadEvtCutInfo(&evtcut[1], evtcutfile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read event cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input event cut file: %s.\n", global.taskname, evtcutfile1);
      goto ReadEvtCutFile_end;
    }

  /* Retrieve DET2 event cut info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for event cut data of detector %s.\n", global.taskname, evtcutfile2, KWVL_DETNAM_DET2);
  if( ReadEvtCutInfo(&evtcut[2], evtcutfile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read event cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input event cut file: %s.\n", global.taskname, evtcutfile2);
      goto ReadEvtCutFile_end;
    }

  /* Retrieve DET3 event info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for event cut data of detector %s.\n", global.taskname, evtcutfile3, KWVL_DETNAM_DET3);
  if( ReadEvtCutInfo(&evtcut[3], evtcutfile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read event cut data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input event file: %s.\n", global.taskname, evtcutfile3);
      goto ReadEvtCutFile_end;
    }


  return OK;
  
 ReadEvtCutFile_end:
  
  return NOT_OK;

} /* ReadEvtCutFile */


/*
 *
 *      ReadEvtCutInfo
 *
 *	DESCRIPTION:
 *           Routine to get event cut data from input evtcutfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtCutInfo(EvtCutInfo_t *evtcut, char *filename, long extno, char *detnam){

  int                status=OK, found=NOT_OK;
  int                inExt, totExt;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  EvtCutCol_t        evtcutcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     inunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only event cut file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(inunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadEvtCutInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input evtcutfile */
      if (fits_get_num_hdus(inunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadEvtCutInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to EVTCUT extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadEvtCutInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(inunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_EVTCUT) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_EVTCUT,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadEvtCutInfo_end;
	}
    }


  head = RetrieveFitsHeader(inunit);
  
  /* Read depth bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }


  /* Get columns index from name */

  if ((evtcutcol.BASELINE1 = GetColNameIndx(&table, CLNM_BASELINE1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BASELINE1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PI_BASELINE = GetColNameIndx(&table, CLNM_PI_BASELINE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PI_BASELINE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.BASELINE2 = GetColNameIndx(&table, CLNM_BASELINE2)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BASELINE2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PRIOR1 = GetColNameIndx(&table, CLNM_PRIOR1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PRIOR1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PRIOR2 = GetColNameIndx(&table, CLNM_PRIOR2)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PRIOR2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PRIOR3 = GetColNameIndx(&table, CLNM_PRIOR3)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PRIOR3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PI_PRIOR = GetColNameIndx(&table, CLNM_PI_PRIOR)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PI_PRIOR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.RESET1 = GetColNameIndx(&table, CLNM_RESET1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.RESET2 = GetColNameIndx(&table, CLNM_RESET2)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESET2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.RESET3 = GetColNameIndx(&table, CLNM_RESET3)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESET3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PI1_RESET = GetColNameIndx(&table, CLNM_PI1_RESET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PI1_RESET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  if ((evtcutcol.PI2_RESET = GetColNameIndx(&table, CLNM_PI2_RESET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PI2_RESET);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  if (ReadBintable(inunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      evtcut->baseline1 = IVEC(table,0,evtcutcol.BASELINE1);
      evtcut->pi_baseline = JVEC(table,0,evtcutcol.PI_BASELINE);
      evtcut->baseline2 = IVEC(table,0,evtcutcol.BASELINE2);
      evtcut->prior1 = DVEC(table,0,evtcutcol.PRIOR1);
      evtcut->prior2 = DVEC(table,0,evtcutcol.PRIOR2);
      evtcut->prior3 = DVEC(table,0,evtcutcol.PRIOR3);
      evtcut->pi_prior = JVEC(table,0,evtcutcol.PI_PRIOR);
      evtcut->reset1 = DVEC(table,0,evtcutcol.RESET1);
      evtcut->reset2 = DVEC(table,0,evtcutcol.RESET2);
      evtcut->reset3 = DVEC(table,0,evtcutcol.RESET3);
      evtcut->pi1_reset = JVEC(table,0,evtcutcol.PI1_RESET);
      evtcut->pi2_reset = JVEC(table,0,evtcutcol.PI2_RESET);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: unable to read the first row of '%s' file. \n", global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadEvtCutInfo_end;
    }



  return OK;
  
 ReadEvtCutInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadEvtCutInfo */
