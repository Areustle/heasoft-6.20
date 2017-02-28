/*
 * 
 *	nulivetime.c
 *
 *	INVOCATION:
 *
 *		nulivetime [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 16/02/12 - First version
 *        0.1.1 - NS 11/05/12 - Added consistence check of input files
 *        0.1.2 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.2 - NS 08/04/14 - Handle compressed input event file
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nulivetime  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nulivetime.h"


/********************************/
/*         definitions          */
/********************************/

#define NULIVETIME_C
#define NULIVETIME_VERSION      "0.1.3"
#define PRG_NAME               "nulivetime"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nulivetime_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nulivetime.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nulivetime_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 16/02/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulivetime_getpar()
{
  
  /* Input Event File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }
 
  /* Input HK File Name */
  if(PILGetFname(PAR_HKFILE, global.par.hkfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HKFILE);
      goto Error;	
    }
 
  /* Output Event File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }
  

  get_history(&global.hist);
  nulivetime_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nulivetime_getpar */


/*
 *	nulivetime_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nulivetime_checkinput();
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
 *        0.1.0 - NS 16/02/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulivetime_work()
{
  int                status=OK;
  char               zipfile[MAXFNAME_LEN], ext[MAXEXT_LEN], cmd[BUF_SIZE];
  struct gti_struct  gti;


  if(nulivetime_checkinput())
    goto Error;


  /* Get Observation Info from input file */
  if( GetObsInfo(global.par.infile, KWVL_EXTNAME_EVT, &global.obsinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.infile);
    goto Error;
  }


  /* Read GTI info from infile GTI extension */
  if(HDgti_read(global.par.infile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, global.par.infile);
    goto Error;
  }

  /* Compute DEADC value */
  if(ComputeDEADC(global.par.hkfile, &gti, &global.obsinfoupdate.deadc)){
    headas_chat(NORMAL, "%s: Error: Unable to compute Deadtime correction value.\n", global.taskname, global.par.infile);
    goto Error;
  }

  /* Compute LIVETIME and EXPOSURE values */
  global.obsinfoupdate.livetime = global.obsinfoupdate.deadc * global.obsinfo.ontime;
  global.obsinfoupdate.exposure = global.obsinfoupdate.deadc * global.obsinfo.ontime;


  /* Create temporary file */

  GetFilenameExtension(global.par.infile, ext);
  if ( !(strcmp(ext, "gz")) )
    {
      sprintf(zipfile, "%s.gz", global.tmpfile);
      
      if(CopyFile(global.par.infile, zipfile))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, zipfile);
	  goto Error;
	}
      
      /* Uncompress input file */
      sprintf(cmd,"gunzip -fq %s", zipfile);
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to uncompress temporary file '%s'\n", global.taskname, zipfile);
	goto Error;
      }
    }
  else
    { 
      if(CopyFile(global.par.infile, global.tmpfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create temporary file %s.\n", global.taskname, global.tmpfile);
	  goto Error;
	}
    }


  /* Update Time keywords in output temporary file */
  if(UpdateEvtTimeKeys(global.tmpfile, &global.obsinfoupdate))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update time keywords in '%s' file.\n", global.taskname, global.tmpfile);
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

  return OK; 
  
 Error:

  return NOT_OK;

} /* nulivetime_work */


/*
 *	nulivetime
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
 *             void nulivetime_getpar(void);
 * 	       void nulivetime_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 16/02/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulivetime()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NULIVETIME_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nulivetime_getpar() == OK) 
    {
      
      if ( nulivetime_work()) 
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
  
} /* nulivetime */


/*
 *	nulivetime_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 16/02/12 - First version
 *
 */
void nulivetime_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the input HK file                             :'%s'\n",global.par.hkfile);
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
} /* nulivetime_info */


/*
 *	nulivetime_checkinput
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
 *        0.1.0: - NS 16/02/12 - First version
 *
 */
int nulivetime_checkinput(void)
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


int ComputeDEADC(char *hkfile, struct gti_struct *gti, double *deadc){

  unsigned           FromRow, ReadRows, n, nCols, ActCols[2];
  int                status=OK, firstrow=1;
  double             tstart=0, tstop=0, dtime=0, dtime_gti=0, livetime=0, livetime_gti=0, sum_gti=0;
  double             hktstart, hktstop;
  char               hkinstrume[FLEN_VALUE];
  double             hkinit=0., hkstop=0.;
  HKCol_t            hkcol;
  FitsCard_t         *card;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;


  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, hkfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(hkfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }
 
  /* Move in HK1FPM extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_HK1FPM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_HK1FPM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, hkfile); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, hkfile);
	}
      goto ComputeGTILivetime_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);


  if((ExistsKeyWord(&hkhead, KWNM_TSTART, &card)))
    {
      hktstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }

  if((ExistsKeyWord(&hkhead, KWNM_TSTOP, &card)))
    {
      hktstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }

  if(ExistsKeyWord(&hkhead, KWNM_INSTRUME, &card))
    {
      strcpy(hkinstrume, card->u.SVal);
    }
  else
    {
      strcpy(hkinstrume, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, hkfile);
    }


  /* Check if hkfile is appropriate for input event file */

  if( global.obsinfo.tstart<hktstart || global.obsinfo.tstop>hktstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, hkfile);

  if( strcasecmp(global.obsinfo.instrume,hkinstrume) )
    headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file and event file are not consistent\n", global.taskname, hkfile);



  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }


  /* Get needed columns number from name */

  /* Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }

  /* LIVETIME */
  if ((hkcol.LIVETIME=ColNameMatch(CLNM_LIVETIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LIVETIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, hkfile);
      goto ComputeGTILivetime_end;
    }


  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following columns */
  nCols=2; 
  ActCols[0]=hkcol.TIME;
  ActCols[1]=hkcol.LIVETIME;

  while(ReadBintable(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  
	  if(firstrow>0){
	    /* Update 'tstart' and 'livetime' values */
	    tstart = DVEC(hktable, n, hkcol.TIME);
	    livetime = DVEC(hktable, n, hkcol.LIVETIME);
	    firstrow = 0;
	    hkinit = tstart;
	  }
	  else{
	    /* Update 'tstop' value */
	    tstop = DVEC(hktable, n, hkcol.TIME);
	    dtime = tstop-tstart;
	    
	    /* Compute overlap exposure of the time interval with GTI */
	    dtime_gti = HDgti_exp(tstart, tstop, gti, &status);
	    if(status!=OK){
	      headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time with GTI\n", global.taskname);
	      goto ComputeGTILivetime_end;
	    }
	    
	    /* Update 'livetime_gti' value */
	    livetime_gti += livetime*(dtime_gti/dtime);
	    
	    
	    /* Update 'tstart' and 'livetime' values */
	    tstart = DVEC(hktable, n, hkcol.TIME);
	    livetime = DVEC(hktable, n, hkcol.LIVETIME);
	  }

	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  

  /* Last row */	   
  tstop = tstart + dtime;
  hkstop = tstop;

  /* Compute overlap exposure of the time interval with GTI */
  dtime_gti = HDgti_exp(tstart, tstop, gti, &status);
  if(status!=OK){
    headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time with GTI\n", global.taskname);
    goto ComputeGTILivetime_end;
  }
  
  /* Update 'livetime_gti' value */
  livetime_gti += livetime*(dtime_gti/dtime);


  /* Compute GTI time sum */
  sum_gti = HDgti_exp(hkinit, hkstop, gti, &status);
  if(status!=OK){
    headas_chat(NORMAL, "%s: Error: unable to compute the sum of GTI times.\n", global.taskname);
    goto ComputeGTILivetime_end;
  }

  headas_chat(CHATTY, "%s: Info: livetime_sum=%f gtitime_sum=%f.\n", global.taskname,livetime_gti,sum_gti);


  *deadc = livetime_gti/sum_gti ;

  return OK;

  
 ComputeGTILivetime_end:
  if (hkhead.first)
    ReleaseBintable(&hkhead, &hktable);
  
  return NOT_OK;

 } /* ComputeGTILivetime */


int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo){

  int            status=OK;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL;


  /* Open readonly file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
    
  /* Move in <extname> extension  */
  if (fits_movnam_hdu(inunit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetObsInfo_end;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit); 


  /* Retrieve INSTRUME from input event file */
  if((ExistsKeyWord(&head, KWNM_INSTRUME, &card)))
    {
      strcpy(obsinfo->instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      obsinfo->tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve observation end time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      obsinfo->tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if((ExistsKeyWord(&head, KWNM_ONTIME, &card)))
    {
      obsinfo->ontime=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_ONTIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }
 
  return OK;
  
 GetObsInfo_end:
  
  CloseFitsFile(inunit);
  return NOT_OK;

} /* GetObsInfo */



int UpdateEvtTimeKeys(char *filename, ObsInfoUpdate_t *obsupdate){

  int             status=OK;
  fitsfile        *fptr=NULL;  

  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }
  
  if(fits_movabs_hdu(fptr, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }

  if(AddEvtKeywords(fptr, obsupdate))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }
  
  if(fits_movnam_hdu(fptr, ANY_HDU, KWVL_EXTNAME_EVT, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }

  if(AddEvtKeywords(fptr, obsupdate))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }
  
  if(fits_movnam_hdu(fptr, ANY_HDU, KWVL_EXTNAME_GTI, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_GTI);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }

  if(AddEvtKeywords(fptr, obsupdate))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }
  

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateEvtTimeKeys_end;
    }


  return OK;

 UpdateEvtTimeKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK; 
  
} /* UpdateEvtTimeKeys */

int AddEvtKeywords(FitsFileUnit_t inunit, ObsInfoUpdate_t *obsupdate){

  int    status=OK, logical=0;

  if(fits_update_key(inunit, TDOUBLE, KWNM_DEADC, &global.obsinfoupdate.deadc, CARD_COMM_DEADC, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADC);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_LIVETIME, &global.obsinfoupdate.livetime, CARD_COMM_LIVETIME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_LIVETIME);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_EXPOSURE, &global.obsinfoupdate.exposure, CARD_COMM_EXPOSURE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EXPOSURE);
      goto addevt_end;
    }

  logical=FALSE;
  if(fits_update_key(inunit, TLOGICAL, KWNM_DEADAPP, &logical, CARD_COMM_DEADAPP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADAPP);
      goto addevt_end;
    }


  return OK;
  
 addevt_end:
  return NOT_OK;
  
} /* AddEvtKeywords */
