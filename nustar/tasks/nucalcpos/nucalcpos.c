/*
 * 
 *	nucalcpos.c
 *
 *	INVOCATION:
 *
 *		nucalcpos [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine for the conversion of telemetry coordinates (RAWX,RAWY)(sub-detector coordinates)
 *              in detector coordinates (DET1X,DET1Y) referred to the Focal Plane Bench Frame (FB)
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 11/03/11 - First version
 *        0.1.1 - NS 23/05/11 - Added 'metrologyfile', 'metgridfile', 'outpsdfile' and 'outpsdfilecor' input parameters
 *        0.1.2 - NS 11/07/11 - Handle new format of pixel location file
 *                            - Added 'metflag' and 'initseed' input parameters
 *        0.1.3 - NS 02/08/11 - Added 'inpsdfilecorr' input parameter
 *                            - Added new keywords in 'outpsdfile' and 'outpsdfilecor' output files
 *                            - Handle NULL values in DET1X and DET1Y columns
 *                            - Handle new allowed range of DET1X, DET1Y, RAW2X and RAW2Y values
 *        0.1.4 - NS 30/09/11 - Added 'alignfile' input parameter
 *                            - Added calculation of DET2X and DET2Y columns in output event file
 *                            - Added 'mastaspectfile' input parameter
 *        0.1.5 - NS 10/10/11 - Add 'TLMIN*', 'TLMAX*', 'TCDLT*', 'TCTYP*' and 'TCUNI*' keywords for DET1* and DET2* columns
 *        0.1.6 - NS 24/10/11 - Modified 'GetFBtoOBinfo' function to improve performances
 *        0.1.7 - NS 17/11/11 - Fixed bug in 64-bit architectures
 *        0.1.8 - NS 18/11/11 - Handle CALDB query for metgridfile input parameter
 *                            - Modified 'FindNearestPoint' function to improve performances
 *        0.1.9 - NS 30/11/11 - Removed metrology processing
 *        0.2.0 - NS 10/02/12 - Modified to read block of rows from input mastaspectfile
 *        0.2.1 - NS 28/03/12 - Handle events with TIME before mast aspect data
 *        0.2.2 - NS 11/05/12 - Added consistence check of input files
 *        0.2.3 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nucalcpos  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nucalcpos.h"


/********************************/
/*         definitions          */
/********************************/

#define NUCALCPOS_C
#define NUCALCPOS_VERSION      "0.2.3"
#define PRG_NAME               "nucalcpos"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nucalcpos_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nucalcpos.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nucalcpos_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 11/03/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpos_getpar()
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
    
  /* Input Pixel Location File Name */
  if(PILGetFname(PAR_PIXPOSFILE, global.par.pixposfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXPOSFILE);
      goto Error;	
    }
  
  /* saveraw2coord */
  if(PILGetBool(PAR_SAVERAW2COORD, &global.par.saveraw2coord))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SAVERAW2COORD); 
      goto Error;	 
    }

  /* inistseed */
  if(PILGetBool(PAR_INITSEED, &global.par.initseed))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INITSEED); 
      goto Error;	 
    }

  /* Input Alignment File Name */
  if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
      goto Error;
    }

  /* Output Mast Aspect Solution File Name */
  if(PILGetFname(PAR_MASTASPECTFILE, global.par.mastaspectfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MASTASPECTFILE);
      goto Error;
    }

  
  get_history(&global.hist);
  nucalcpos_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nucalcpos_getpar */


/*
 *	nucalcpos_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nucalcpos_checkinput();
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
 *        0.1.0 - NS 11/03/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpos_work()
{
  int                  status = OK, inExt, outExt, evExt;
  AlignInfo_t          aligninfo;
  long                 extfile=-1;
  FitsHeader_t         head;
  FitsCard_t           *card;
  FitsFileUnit_t       inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  

  if(nucalcpos_checkinput())
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

  /* Derive CALDB alignment filename */  
  extfile=-1;
  if ( !strcasecmp(global.par.alignfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_ALIGN_DSET, global.par.alignfile, "type.eq.systems", &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for alignfile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }
  
  /* Retrieve alignment info from input alignfile*/
  if( ReadAlignInfo(global.par.alignfile, &aligninfo) )
    {
      headas_chat(NORMAL, " %s: Error: unable to read alignment file.\n", global.taskname);
      goto Error;
    }

  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);
  
  /* Compute DET coordinates */
  if (ComputeDETCoords(inunit, &aligninfo, global.par.mastaspectfile, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute DET values.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto warning_end;
  
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
  
  return OK; 


 warning_end:
  
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
} /* nucalcpos_work */


/*
 *	nucalcpos
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
 *             void nucalcpos_getpar(void);
 * 	       void nucalcpos_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 11/03/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nucalcpos()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUCALCPOS_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nucalcpos_getpar() == OK) 
    {
      
      if ( nucalcpos_work()) 
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
  
} /* nucalcpos */


/*
 *	nucalcpos_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 11/03/11 - First version
 *
 */
void nucalcpos_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input pixel location file                 :'%s'\n",global.par.pixposfile);

  if (global.par.saveraw2coord)
    headas_chat(CHATTY,"Save RAW2X,RAW2Y columns                              : yes\n");
  else
    headas_chat(CHATTY,"Save RAW2X,RAW2Y columns                              : no\n");

  headas_chat(NORMAL,"Name of the input Alignment File                           :'%s'\n",global.par.alignfile);
  headas_chat(NORMAL,"Name of the input Mast Aspect Solution File               :'%s'\n",global.par.mastaspectfile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                 : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                 : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                        : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                        : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nucalcpos_info */


/*
 *	nucalcpos_checkinput
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
 *        0.1.0: - NS 11/03/11 - First version
 *
 */
int nucalcpos_checkinput(void)
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
 *      ComputeDETCoords
 *
 *	DESCRIPTION:
 *           Routine to compute DET coordinates and put them into EVENTS bintable.
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
 *        0.1.0: - NS 11/03/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeDETCoords(FitsFileUnit_t evunit, AlignInfo_t *aligninfo, char *mastaspectfile, FitsFileUnit_t ounit)
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                rawx, rawy, det_id, grade, det1x, det1y, det2x, det2y;
  double             time;
  int                status=OK, totrows, colnum;
  char               KeyWord[FLEN_KEYWORD];
  JTYPE              null_det1=KWVL_DET1NULL;
  JTYPE              null_det2=KWVL_DET2NULL;
  JTYPE              jval;
  double             dval;
  long int           seed;
  MastInfo_t         *mastinfo=NULL;
  unsigned           mastindex=1, mastcount=MAST_BLOCK_ROWS;
  BOOL               mastend;
  PixPosData_t       pixpos[4][DET_PIXS][DET_ROWS];
  EvtCol_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
   
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );



  /* Read Pixel Location Coefficients from input pixposfile */
  if( ReadPixPosFile(pixpos) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixel location coefficients\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, global.par.pixposfile);
      goto reco_end;
    }

  /* Retrieve mast aspect solution info from input mastaspectfile */
  if(ReadMastAspectSolutionInfo(mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
      goto reco_end;
    }
  mastindex += mastcount;
  mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */

  
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


  /* Add new columns if needed */

  /* Add DET1X,DET1Y */
  if((indxcol.DET1X=ColNameMatch(CLNM_DET1X, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET1X);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_DET1X, CARD_COMM_DET1X, "1I", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
      indxcol.DET1X=ColNameMatch(CLNM_DET1X, &outtable);
    }
  sprintf(KeyWord, "TLMIN%d", (indxcol.DET1X+1));
  jval = DET1X_MIN;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Minimum value for DET1X column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.DET1X+1));
  jval = DET1X_MAX;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Maximum value for DET1X column");
  sprintf(KeyWord, "TNULL%d", (indxcol.DET1X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &null_det1, CARD_COMM_TNULL);
  sprintf(KeyWord, "TCDLT%d", (indxcol.DET1X+1));
  dval = DET_IMG_SCALE;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, D, &dval, "DET1X image scale (mm/pixel)");
  sprintf(KeyWord, "TCTYP%d", (indxcol.DET1X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "DET1X", "DET1X coordinate type");
  sprintf(KeyWord, "TCUNI%d", (indxcol.DET1X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "mm", "DET1X units");

  if((indxcol.DET1Y=ColNameMatch(CLNM_DET1Y, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET1Y);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_DET1Y, CARD_COMM_DET1Y, "1I", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
      indxcol.DET1Y=ColNameMatch(CLNM_DET1Y, &outtable);
    }
  sprintf(KeyWord, "TLMIN%d", (indxcol.DET1Y+1));
  jval = DET1Y_MIN;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Minimum value for DET1Y column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.DET1Y+1));
  jval = DET1Y_MAX;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Maximum value for DET1Y column");
  sprintf(KeyWord, "TNULL%d", (indxcol.DET1Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &null_det1, CARD_COMM_TNULL);
  sprintf(KeyWord, "TCDLT%d", (indxcol.DET1Y+1));
  dval = DET_IMG_SCALE;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, D, &dval, "DET1Y image scale (mm/pixel)");
  sprintf(KeyWord, "TCTYP%d", (indxcol.DET1Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "DET1Y", "DET1Y coordinate type");
  sprintf(KeyWord, "TCUNI%d", (indxcol.DET1Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "mm", "DET1Y units");


  /* Add RAW2X,RAW2Y */
  if (global.par.saveraw2coord)
    {
      if((indxcol.RAW2X=ColNameMatch(CLNM_RAW2X, &outtable)) == -1)
	{
	  headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_RAW2X);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
	  AddColumn(&head, &outtable, CLNM_RAW2X, CARD_COMM_RAW2X, "1I", TNONE);
	  indxcol.RAW2X=ColNameMatch(CLNM_RAW2X, &outtable);
	}
      if((indxcol.RAW2Y=ColNameMatch(CLNM_RAW2Y, &outtable)) == -1)
	{
	  headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_RAW2Y);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
	  AddColumn(&head, &outtable, CLNM_RAW2Y, CARD_COMM_RAW2Y, "1I", TNONE);
	  indxcol.RAW2Y=ColNameMatch(CLNM_RAW2Y, &outtable);
	}
    }
  else
    {
      indxcol.RAW2X=-1;
      indxcol.RAW2Y=-1;
    }

  /* Add DET2X,DET2Y */
  if((indxcol.DET2X=ColNameMatch(CLNM_DET2X, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET2X);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_DET2X, CARD_COMM_DET2X, "1I", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
      indxcol.DET2X=ColNameMatch(CLNM_DET2X, &outtable);
    }
  sprintf(KeyWord, "TLMIN%d", (indxcol.DET2X+1));
  jval = DET2X_MIN;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Minimum value for DET2X column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.DET2X+1));
  jval = DET2X_MAX;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Maximum value for DET2X column");
  sprintf(KeyWord, "TNULL%d", (indxcol.DET2X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &null_det2, CARD_COMM_TNULL);
  sprintf(KeyWord, "TCDLT%d", (indxcol.DET2X+1));
  dval = DET_IMG_SCALE;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, D, &dval, "DET2X image scale (mm/pixel)");
  sprintf(KeyWord, "TCTYP%d", (indxcol.DET2X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "DET2X", "DET2X coordinate type");
  sprintf(KeyWord, "TCUNI%d", (indxcol.DET2X+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "mm", "DET2X units");


  if((indxcol.DET2Y=ColNameMatch(CLNM_DET2Y, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET2Y);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);
      AddColumn(&head, &outtable, CLNM_DET2Y, CARD_COMM_DET2Y, "1I", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
      indxcol.DET2Y=ColNameMatch(CLNM_DET2Y, &outtable);
    }
  sprintf(KeyWord, "TLMIN%d", (indxcol.DET2Y+1));
  jval = DET2Y_MIN;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Minimum value for DET2Y column");
  sprintf(KeyWord, "TLMAX%d", (indxcol.DET2Y+1));
  jval = DET2Y_MAX;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &jval, "Maximum value for DET2Y column");
  sprintf(KeyWord, "TNULL%d", (indxcol.DET2Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, J, &null_det2, CARD_COMM_TNULL);
  sprintf(KeyWord, "TCDLT%d", (indxcol.DET2Y+1));
  dval = DET_IMG_SCALE;
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, D, &dval, "DET2Y image scale (mm/pixel)");
  sprintf(KeyWord, "TCTYP%d", (indxcol.DET2Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "DET2Y", "DET2Y coordinate type");
  sprintf(KeyWord, "TCUNI%d", (indxcol.DET2Y+1));
  if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
    AddCard(&head, KeyWord, S, "mm", "DET2Y units");


  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s.", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
      
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);


  /* initialize random number generator */
  if(global.par.initseed){
    seed = -345337518;
  }
  else{
    get_ran2seed(&seed);
  }

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
	    goto reco_end;
	  }
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Error: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    goto reco_end;
	  }

	  /* Assign GRADE 0 position to evt with grade>12 */
/* 	  if(grade<0||grade>PIXPOS_GRADES_DIM-1){ */
/* 	    grade = 0; */
/* 	  } */

	  if(ComputeDET1XY(rawx, rawy, det_id, grade, pixpos[det_id], &det1x, &det1y, &seed)){
	    headas_chat(NORMAL, "%s: Error: Unable to compute DET1 coordinates for RAWX=%d RAWY=%d DET_ID=%d\n",global.taskname,rawx,rawy,det_id);
	    goto reco_end;
	  }


	  if( (mastend != OK) && (mastcount>0) && (time > mastinfo[mastcount-1].time) ){
	    
	    free(mastinfo);
	    if(ReadMastAspectSolutionInfo(mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend)){
	      headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
	      goto reco_end;
	    }
	    mastindex += mastcount;
	    mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */
	  }

	  if( mastcount>0 && time<(mastinfo[0].time - TIME_SENS) ){
	    headas_chat(NORMAL, "%s: Warning: No mast aspect data for TIME=%f\n", global.taskname, time);
	    det2x = KWVL_DET2NULL;
	    det2y = KWVL_DET2NULL;
	  }
	  else{
	    if(ComputeDET2XY(det1x, det1y, time, aligninfo, mastinfo, mastcount, &seed, global.evt.instrume, &det2x, &det2y)){
	      headas_chat(NORMAL, "%s: Error: Unable to compute DET2 coordinates for RAWX=%d RAWY=%d DET_ID=%d TIME=%f\n",global.taskname,rawx,rawy,det_id,time);
	      goto reco_end;
	    }
	  }

	  IVEC(outtable,n,indxcol.DET1X) = det1x;
	  IVEC(outtable,n,indxcol.DET1Y) = det1y;
	  IVEC(outtable,n,indxcol.DET2X) = det2x;
	  IVEC(outtable,n,indxcol.DET2Y) = det2y;

	  /* Compute RAW2X and RAW2Y values */	  
	  if (global.par.saveraw2coord){

	    switch (det_id) {
	    case KWVL_DET_ID_0:
	      IVEC(outtable,n,indxcol.RAW2X) = rawy + RAW2_BORDER_OFF+DET_PIXS+RAW2_CENTER_OFF+1;
	      IVEC(outtable,n,indxcol.RAW2Y) = rawx + RAW2_BORDER_OFF+DET_PIXS+RAW2_CENTER_OFF+1;
	      break;
	    case KWVL_DET_ID_1:
	      IVEC(outtable,n,indxcol.RAW2X) = DET_PIXS+RAW2_BORDER_OFF - rawx;
	      IVEC(outtable,n,indxcol.RAW2Y) = rawy + RAW2_BORDER_OFF+DET_PIXS+RAW2_CENTER_OFF+1;
	      break;
	    case KWVL_DET_ID_2:
	      IVEC(outtable,n,indxcol.RAW2X) = DET_PIXS+RAW2_BORDER_OFF - rawy;
	      IVEC(outtable,n,indxcol.RAW2Y) = DET_PIXS+RAW2_BORDER_OFF - rawx;
	      break;
	    case KWVL_DET_ID_3:
	      IVEC(outtable,n,indxcol.RAW2X) = rawx + RAW2_BORDER_OFF+DET_PIXS+RAW2_CENTER_OFF+1;
	      IVEC(outtable,n,indxcol.RAW2Y) = DET_PIXS+RAW2_BORDER_OFF - rawy;
	      break;
	    default:
	      headas_chat(NORMAL, "%s: Error: Unexpected  DET_ID value (%d) in input file.\n",global.taskname, det_id);
	      goto reco_end;
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


  /* Delete RAW2X and RAW2Y columns if 'saveraw2coord' set to "no" */
  if (!global.par.saveraw2coord){

    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_RAW2X, &colnum, &status))
      if(fits_delete_col(ounit, colnum, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_RAW2X);
	  goto reco_end;
	}

    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_RAW2Y, &colnum, &status))
      if(fits_delete_col(ounit, colnum, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_RAW2Y);
	  goto reco_end;
	}
  }


  return OK;
  
 reco_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* ComputeDETCoords */


/*
 *
 *      ReadPixPosFile
 *
 *	DESCRIPTION:
 *           Routine to read input pixposfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPixPosFile(PixPosData_t pixpos[4][DET_PIXS][DET_ROWS]){
  

  char        pixposfile0[PIL_LINESIZE], pixposfile1[PIL_LINESIZE], pixposfile2[PIL_LINESIZE], pixposfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB gain filename */  
  if ( !strcasecmp(global.par.pixposfile,DF_CALDB) )
    {
      /* Retrieve DET0 pixpos filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PIXPOS_DSET, pixposfile0, HD_EXPR, &extfile0, global.evt.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadPixPosFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 pixpos filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PIXPOS_DSET, pixposfile1, HD_EXPR, &extfile1, global.evt.instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadPixPosFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 pixpos filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PIXPOS_DSET, pixposfile2, HD_EXPR, &extfile2, global.evt.instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadPixPosFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 pixpos filename */
      if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, global.evt.dateend, global.evt.timeend, KWVL_PIXPOS_DSET, pixposfile3, HD_EXPR, &extfile3, global.evt.instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadPixPosFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(pixposfile0, global.par.pixposfile);
    strcpy(pixposfile1, global.par.pixposfile);
    strcpy(pixposfile2, global.par.pixposfile);
    strcpy(pixposfile3, global.par.pixposfile);
  }

  /* Retrieve DET0 pixpos info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile0, KWVL_DETNAM_DET0);
  if( ReadPixPosInfo(pixpos[0], pixposfile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixpos coefficients\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile0);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET1 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile1, KWVL_DETNAM_DET1);
  if( ReadPixPosInfo(pixpos[1], pixposfile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixpos coefficients\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile1);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET2 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile2, KWVL_DETNAM_DET2);
  if( ReadPixPosInfo(pixpos[2], pixposfile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixpos coefficients\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile2);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET3 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile3, KWVL_DETNAM_DET3);
  if( ReadPixPosInfo(pixpos[3], pixposfile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixpos coefficients\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile3);
      goto ReadPixPosFile_end;
    }



  return OK;
  
 ReadPixPosFile_end:
  
  return NOT_OK;

} /* ReadPixPosFile */


/*
 *
 *      ReadPixPosInfo
 *
 *	DESCRIPTION:
 *           Routine to get pixpos coefficient from input pixposfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPixPosInfo(PixPosData_t pixpos[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam){

  int                n, i=0, j=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  PixPosCol_t        pixposcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      pixpos[i][j].ninfo = 0;
    }
  }


  /* Open read only pixpos file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadPixPosInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input pixposfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadPixPosInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to PIXPOS extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadPixPosInfo_end;
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

	  if( !strcmp(r_extname,KWVL_EXTNAME_PIXPOS) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_PIXPOS,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadPixPosInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read pixpos bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  /* Get columns index from name */


  if ((pixposcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.GRADE = GetColNameIndx(&table, CLNM_GRADE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.REF_DET1X = GetColNameIndx(&table, CLNM_REF_DET1X)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_REF_DET1X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.REF_DET1Y = GetColNameIndx(&table, CLNM_REF_DET1Y)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_REF_DET1Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.PDF = GetColNameIndx(&table, CLNM_PDF)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PDF);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  /* Check columns multiplicity */
  if(table.Multiplicity[pixposcol.PDF]!=PIXPOS_PDF_DIM)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_PDF);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);      
      goto ReadPixPosInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,pixposcol.RAWX);
	  rawy = BVEC(table,n,pixposcol.RAWY);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadPixPosInfo_end;
	  }
	  
	  if(pixpos[rawx][rawy].ninfo==0){
	    pixpos[rawx][rawy].ninfo = 1;
	    pixpos[rawx][rawy].info = ( PixPosInfo_t*)malloc(sizeof(PixPosInfo_t));
	    
	    if(pixpos[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadPixPosInfo: memory allocation failure.\n", global.taskname);
	      goto ReadPixPosInfo_end;
	    }
	    
	  }
	  else{
	    pixpos[rawx][rawy].ninfo++;
	    pixpos[rawx][rawy].info = (PixPosInfo_t*) realloc( pixpos[rawx][rawy].info, (pixpos[rawx][rawy].ninfo * sizeof(PixPosInfo_t)) );

	    if(pixpos[rawx][rawy].info==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadPixPosInfo: memory allocation failure.\n", global.taskname);
	      goto ReadPixPosInfo_end;
	    }	    
	  }

	  pixpos[rawx][rawy].info[ (pixpos[rawx][rawy].ninfo-1) ].grade = IVEC(table,n,pixposcol.GRADE);
	  pixpos[rawx][rawy].info[ (pixpos[rawx][rawy].ninfo-1) ].ref_det1x = IVEC(table,n,pixposcol.REF_DET1X);	  
	  pixpos[rawx][rawy].info[ (pixpos[rawx][rawy].ninfo-1) ].ref_det1y = IVEC(table,n,pixposcol.REF_DET1Y);
	  for (j=0; j< PIXPOS_PDF_DIM; j++)
	    pixpos[rawx][rawy].info[ (pixpos[rawx][rawy].ninfo-1) ].pdf[j] = EVECVEC(table,n,pixposcol.PDF,j);

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
      goto ReadPixPosInfo_end;
    }



  return OK;
  
 ReadPixPosInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadPixPosInfo */


/*
 *
 *      ComputeDET1XY
 *
 *	DESCRIPTION:
 *           
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeDET1XY(int rawx, int rawy, int det_id, int grade, PixPosData_t pixpos[DET_PIXS][DET_ROWS], int *det1x, int *det1y, long int *seed){

  BOOL  found = FALSE;
  int   i=0, k=0, indx=0, rowindx=0, dx=0, dy=0;
  int   this_det1x, this_det1y;
  float rnd=0.0;
  float cumprob[PIXPOS_PDF_DIM];

  /* generate random number between 0.0 and 1.0 */
  rnd = hd_ran2(seed);

  headas_chat(CHATTY,"%s: Info: Random Number=%f\n",global.taskname,rnd);

  for(i=0; i<pixpos[rawx][rawy].ninfo; i++){

    if(pixpos[rawx][rawy].info[i].grade == grade){
      
      cumprob[0] = pixpos[rawx][rawy].info[i].pdf[0];

      for(k=1;k<PIXPOS_PDF_DIM; k++){
	cumprob[k] = pixpos[rawx][rawy].info[i].pdf[k] + cumprob[k-1];
      }

      rowindx = i;
      found = TRUE;
    }
  }

  if( !found ||  pixpos[rawx][rawy].info[rowindx].ref_det1x<0 ||  pixpos[rawx][rawy].info[rowindx].ref_det1y<0 ){
    *det1x = KWVL_DET1NULL;
    *det1y = KWVL_DET1NULL;
    return OK;
  }

  if( (cumprob[PIXPOS_PDF_DIM-1]< (1-CUMPROB_SENS)) ||  (cumprob[PIXPOS_PDF_DIM-1]> (1+CUMPROB_SENS)) ){
    headas_chat(CHATTY,"%s: Warning: RAWX=%d RAWY=%d DET_ID=%d GRADE=%d - probability distribution map not normalized (value=%f).\n",
		global.taskname, rawx, rawy, det_id, grade, cumprob[PIXPOS_PDF_DIM-1]);
  }

  for(k=0;k<PIXPOS_PDF_DIM; k++){
    if( rnd <= cumprob[k] ){
      break;
    }
  }

  indx = MIN( k, PIXPOS_PDF_DIM-1);
  headas_chat(CHATTY,"%s: Info: vector PDF index=%d\n",global.taskname, indx+1);

  dx = indx % PIXPOS_PDF_WIDTH;
  dy = indx / PIXPOS_PDF_WIDTH;

  this_det1x = pixpos[rawx][rawy].info[rowindx].ref_det1x + dx;
  this_det1y = pixpos[rawx][rawy].info[rowindx].ref_det1y + dy;

  if(this_det1x<DET1X_MIN||this_det1x>DET1X_MAX||this_det1y<DET1Y_MIN||this_det1y>DET1Y_MAX){
    headas_chat(NORMAL,"%s: Warning: RAWX=%d RAWY=%d DET_ID=%d GRADE=%d - DET1X=%d DET1Y=%d out of range.\n",
		global.taskname, rawx, rawy, det_id, grade, this_det1x, this_det1y);
    *det1x = KWVL_DET1NULL;
    *det1y = KWVL_DET1NULL;
  }
  else{
    *det1x = this_det1x;
    *det1y = this_det1y;
  }

  return OK;

} /* ComputeDET1XY */


int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo){

  unsigned           FromRow, ReadRows, nCols;
  int                status=OK;
  AlignCol_t         col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);
  /* Open readonly input align file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }
 
  /* Move in SYSTEM_ALIGNMENT extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_SYSTEM_ALIGNMENT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SYSTEM_ALIGNMENT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadAlignInfo_end;
    }
  
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  /* Get needed columns number from name */

  if ((col.Q_FB_OB=ColNameMatch(CLNM_Q_FB_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_OB=ColNameMatch(CLNM_V_FB_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FPMA_DET1=ColNameMatch(CLNM_Q_FPMA_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FPMA_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FPMB_DET1=ColNameMatch(CLNM_Q_FPMB_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FPMB_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FPMA_DET1=ColNameMatch(CLNM_V_FPMA_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FPMA_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FPMB_DET1=ColNameMatch(CLNM_V_FPMB_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FPMB_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FB_FPMA=ColNameMatch(CLNM_Q_FB_FPMA, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_FPMA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FB_FPMB=ColNameMatch(CLNM_Q_FB_FPMB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_FPMB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_FPMA=ColNameMatch(CLNM_V_FB_FPMA, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_FPMA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_FPMB=ColNameMatch(CLNM_V_FB_FPMB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_FPMB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_DET2A_OB=ColNameMatch(CLNM_Q_DET2A_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_DET2A_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_DET2B_OB=ColNameMatch(CLNM_Q_DET2B_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_DET2B_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_DET2A_OB=ColNameMatch(CLNM_V_DET2A_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_DET2A_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_DET2B_OB=ColNameMatch(CLNM_V_DET2B_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_DET2B_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
    DVECVEC_ARRAY_READ(aligninfo->Qfbob, 4, table, 0, col.Q_FB_OB);
    DVECVEC_ARRAY_READ(aligninfo->Tfbob, 3, table, 0, col.V_FB_OB);
    DVECVEC_ARRAY_READ(aligninfo->Qfbfpm0, 4, table, 0, col.Q_FB_FPMA);
    DVECVEC_ARRAY_READ(aligninfo->Qfbfpm1, 4, table, 0, col.Q_FB_FPMB);
    DVECVEC_ARRAY_READ(aligninfo->Tfbfpm0, 3, table, 0, col.V_FB_FPMA);
    DVECVEC_ARRAY_READ(aligninfo->Tfbfpm1, 3, table, 0, col.V_FB_FPMB);
    DVECVEC_ARRAY_READ(aligninfo->Qdet2Aob, 4, table, 0, col.Q_DET2A_OB);
    DVECVEC_ARRAY_READ(aligninfo->Qdet2Bob, 4, table, 0, col.Q_DET2B_OB);
    DVECVEC_ARRAY_READ(aligninfo->Tdet2Aob, 3, table, 0, col.V_DET2A_OB);
    DVECVEC_ARRAY_READ(aligninfo->Tdet2Bob, 3, table, 0, col.V_DET2B_OB);
    DVECVEC_ARRAY_READ(aligninfo->Qfpm0det1, 4, table, 0, col.Q_FPMA_DET1);
    DVECVEC_ARRAY_READ(aligninfo->Tfpm0det1, 3, table, 0, col.V_FPMA_DET1);
    DVECVEC_ARRAY_READ(aligninfo->Qfpm1det1, 4, table, 0, col.Q_FPMB_DET1);
    DVECVEC_ARRAY_READ(aligninfo->Tfpm1det1, 3, table, 0, col.V_FPMB_DET1);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  

  /* Move in METROLOGY_ALIGNMENT extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_METROLOGY_ALIGNMENT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_METROLOGY_ALIGNMENT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadAlignInfo_end;
    }
  
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  /* Get needed columns number from name */

  if ((col.Q_FB_MD0=ColNameMatch(CLNM_Q_FB_MD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_MD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FB_MD1=ColNameMatch(CLNM_Q_FB_MD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_MD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_MD0=ColNameMatch(CLNM_V_FB_MD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_MD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_MD1=ColNameMatch(CLNM_V_FB_MD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_MD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_OB_ML0=ColNameMatch(CLNM_Q_OB_ML0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_OB_ML0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_OB_ML1=ColNameMatch(CLNM_Q_OB_ML1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_OB_ML1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_OB_ML0=ColNameMatch(CLNM_V_OB_ML0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_OB_ML0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_OB_ML1=ColNameMatch(CLNM_V_OB_ML1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_OB_ML1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.L0_ORIG=ColNameMatch(CLNM_L0_ORIG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L0_ORIG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.L1_ORIG=ColNameMatch(CLNM_L1_ORIG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L1_ORIG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.L0_POINT=ColNameMatch(CLNM_L0_POINT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L0_POINT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.L1_POINT=ColNameMatch(CLNM_L1_POINT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L1_POINT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);

  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
    DVECVEC_ARRAY_READ(aligninfo->Qfbmd0, 4, table, 0, col.Q_FB_MD0);
    DVECVEC_ARRAY_READ(aligninfo->Qfbmd1, 4, table, 0, col.Q_FB_MD1);
    DVECVEC_ARRAY_READ(aligninfo->Tfbmd0, 3, table, 0, col.V_FB_MD0);
    DVECVEC_ARRAY_READ(aligninfo->Tfbmd1, 3, table, 0, col.V_FB_MD1);
    DVECVEC_ARRAY_READ(aligninfo->Qobml0, 4, table, 0, col.Q_OB_ML0);
    DVECVEC_ARRAY_READ(aligninfo->Qobml1, 4, table, 0, col.Q_OB_ML1);
    DVECVEC_ARRAY_READ(aligninfo->Tobml0, 3, table, 0, col.V_OB_ML0);
    DVECVEC_ARRAY_READ(aligninfo->Tobml1, 3, table, 0, col.V_OB_ML1);
    DVECVEC_ARRAY_READ(aligninfo->L0ml, 3, table, 0, col.L0_ORIG);
    DVECVEC_ARRAY_READ(aligninfo->L1ml, 3, table, 0, col.L1_ORIG);
    DVECVEC_ARRAY_READ(aligninfo->D0ob, 3, table, 0, col.L0_POINT);
    DVECVEC_ARRAY_READ(aligninfo->D1ob, 3, table, 0, col.L1_POINT);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadAlignInfo_end;
    }



  return OK;

 ReadAlignInfo_end:
  return NOT_OK;

} /* ReadAlignInfo */


/*
 *
 *      ReadMastAspectSolutionInfo
 *
 *	DESCRIPTION:
 *           Routine to read Mast Aspect Solution File 
 *            
 *            I : filename -> name of the input mast aspect file
 *            O : mastinfo -> mast data struct 
 *            I : initRow  -> first row to read (>=1)
 *            I/O: numRows -> on entry, <*numRows> specifies the number of rows to read;
 *			      on exit, it contains the actual number of lines read which may be less than the requested number
 *            O : endfile  -> 'OK' if end of file reached, 'NOT_OK' otherwise
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadMastAspectSolutionInfo(char *filename, MastInfo_t ** mastinfo, const unsigned initRow, unsigned *numRows, BOOL *endfile){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, mastcount, status=OK;
  double             masttstart=0, masttstop=0;
  MastExtCol_t       indxcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in MAST_ASPECT extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_MAST_ASPECT, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_MAST_ASPECT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto ReadMastAspectSolutionInfo_end;
    }
  

  head=RetrieveFitsHeader(unit);

  /* Retrieve TSTART */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      masttstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTOP */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      masttstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Check if mastaspectfile is appropriate for input event file */

  if( global.evt.tstart<masttstart || global.evt.tstop>masttstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename);



  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);

  /* Get needed columns number from name */
  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ReadMastAspectSolutionInfo_end;
    }

  if ((indxcol.T_FBOB = GetColNameIndx(&table, CLNM_T_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_T_FBOB);
      goto ReadMastAspectSolutionInfo_end;
    }

  if ((indxcol.Q_FBOB = GetColNameIndx(&table, CLNM_Q_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Q_FBOB);
      goto ReadMastAspectSolutionInfo_end;
    }

 EndBintableHeader(&head, &table);


 if(!table.MaxRows){
   headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, filename);
   *endfile = OK;
   *numRows = 0;
   goto ReadMastAspectSolutionInfo_ok;
 }

 if (initRow > (unsigned)table.MaxRows) {
   *endfile = OK;
   *numRows = 0;
   goto ReadMastAspectSolutionInfo_ok;
 }

 if (initRow + *numRows  >= (unsigned)table.MaxRows){
   *endfile = OK;
   *numRows = (unsigned)table.MaxRows - initRow + 1;
 }
 else{
   *endfile = NOT_OK;
 }


 /* Allocate memory to storage all requested data */
 mastcount = (int)*numRows;
 *mastinfo = (MastInfo_t *)calloc(mastcount, sizeof(MastInfo_t));
 if(*mastinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ReadMastAspectSolutionInfo: memory allocation failure.\n", global.taskname);
   goto ReadMastAspectSolutionInfo_end;
 }

 /* Read Bintable */
 FromRow = initRow;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<mastcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows && count<mastcount; ++n)
       {
	 (*mastinfo)[count].time = DVEC(table,n,indxcol.TIME);
	 DVECVEC_ARRAY_READ( (*mastinfo)[count].Tfbob, 3, table, n, indxcol.T_FBOB);
	 DVECVEC_ARRAY_READ( (*mastinfo)[count].Qfbob, 4, table, n, indxcol.Q_FBOB);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
 if(mastcount != count){
   headas_chat(CHATTY,"%s: Error: ReadMastAspectSolutionInfo: error reading file.\n", global.taskname);
   goto ReadMastAspectSolutionInfo_end;
 }


 ReadMastAspectSolutionInfo_ok:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }

  return OK;

  
 ReadMastAspectSolutionInfo_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadMastAspectSolutionInfo  */


/*
 *      RotoTranslate
 *
 *	DESCRIPTION:
 *            
 *            AtQuat Quat,   -> IN
 *            AtVect Tr,     -> IN
 *            AtVect vec,    -> IN
 *            AtVect outvec  -> OUT
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int RotoTranslate(AtQuat Quat, AtVect Tr, AtVect vec, AtVect outvec){

  AtRotMat rm;  /* rotation matrix */
  AtRotMat rm2;	/* inversed rotation matrix */
  AtVect   rv;  /* rotaded vector */
  
  
  /* FIND ROTATION MATRIX FOR A EULER ANGLE SET. */
  if(atQuatToRM(Quat, rm)){
    headas_chat(CHATTY,"Error: RotoTranslate: step1 error.\n");
    return NOT_OK;
  }

  /* CALC INVERSE ROTATION MATRIX */
  if(atInvRotMat(rm, rm2)){
    headas_chat(CHATTY,"Error: RotoTranslate: step2 error.\n");
    return NOT_OK;
  }

  /* ROTATE VECTOR WITH ROTATION MATRIX. */
  if(atRotVect(rm2, vec, rv)){
    headas_chat(CHATTY,"Error: RotoTranslate: step3 error.\n");
    return NOT_OK;
  }

  /* ADD VECTORS */
  if(atAddVect(rv, Tr, outvec)){
    headas_chat(CHATTY,"Error: RotoTranslate: step4 error.\n");
    return NOT_OK;
  }

  return OK;

} /* RotoTranslate */


int ComputeDET2XY(int det1x, int det1y, double time, AlignInfo_t *align, MastInfo_t *mast, int mastcount, long int *seed, char *instr, int *det2x, int *det2y){

  float     rnd=0.0;
  double    det1x_rnd, det1y_rnd, det1x_mm, det1y_mm;
  int       det2x_loc, det2y_loc;
  AtVect    Edet1, Efpm, Efb;
  AtQuat    Qfpmdet1, Qfbfpm, Qdet2ob;
  AtVect    Tfpmdet1, Tfbfpm, Tdet2ob;
  AtVect    Tfbob;
  AtRotMat  Rfbob;
  AtVect    EfbT, Eob, Edet2mm, Edet2px;


  /* Randomize DET1X and DET1Y values */
  rnd = hd_ran2(seed); /* random number between 0.0 and 1.0 */
  det1x_rnd = det1x-1+rnd;
  rnd = hd_ran2(seed); /* random number between 0.0 and 1.0 */
  det1y_rnd = det1y-1+rnd;
  
  det1x_mm = det1x_rnd*SUBPIX_SIZE_MM;
  det1y_mm = det1y_rnd*SUBPIX_SIZE_MM;

  Edet1[0] = det1x_mm;
  Edet1[1] = det1y_mm;
  Edet1[2] = 0;
  headas_chat(CHATTY, "%s: Info: Edet1 =%f %f %f\n",global.taskname,Edet1[0],Edet1[1],Edet1[2]);

  /*  FPMA<->0   FPMB<->1 */
  if(!strcmp(instr,KWVL_INSTRUME_FPMA)){
    memcpy( Qfpmdet1, align->Qfpm0det1, 4*sizeof(double) );
    memcpy( Tfpmdet1, align->Tfpm0det1, 3*sizeof(double) );
    memcpy( Qfbfpm, align->Qfbfpm0, 4*sizeof(double) );
    memcpy( Tfbfpm, align->Tfbfpm0, 3*sizeof(double) );
    memcpy( Qdet2ob, align->Qdet2Aob, 4*sizeof(double) );
    memcpy( Tdet2ob, align->Tdet2Aob, 3*sizeof(double) );   
 }
  else{
    memcpy( Qfpmdet1, align->Qfpm1det1, 4*sizeof(double) );
    memcpy( Tfpmdet1, align->Tfpm1det1, 3*sizeof(double) );
    memcpy( Qfbfpm, align->Qfbfpm1, 4*sizeof(double) );
    memcpy( Tfbfpm, align->Tfbfpm1, 3*sizeof(double) );
    memcpy( Qdet2ob, align->Qdet2Bob, 4*sizeof(double) );
    memcpy( Tdet2ob, align->Tdet2Bob, 3*sizeof(double) );  
  }


  /* Get interpolated translation vector (Tfbob) and rotation matrix (Rfbob) for the selected event */
  if(GetFBtoOBinfo(time, mast, mastcount, Tfbob, Rfbob)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step0 error.\n");
    goto ComputeDET2XY_end;
  }


  /* Step 6a: Event Positions transformation from DET1 into Focal Plane Module Frame (FPM) */
  if(RotoTranslate(Qfpmdet1, Tfpmdet1, Edet1, Efpm)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step1 error.\n");
    goto ComputeDET2XY_end;
  }
  headas_chat(CHATTY, "%s: Info: Efpm =%f %f %f\n",global.taskname,Efpm[0],Efpm[1],Efpm[2]);

  /* Step 6b: Event Positions transformation from FPM into Focal Plane Bench Frame (FB) */
  if(RotoTranslate(Qfbfpm, Tfbfpm, Efpm, Efb)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step2 error.\n");
    goto ComputeDET2XY_end;
  }
  headas_chat(CHATTY, "%s: Info: Efb =%f %f %f\n",global.taskname,Efb[0],Efb[1],Efb[2]);


  /* Step 7: Event Positions transformation into Optics Bench Frame (OB) */
  if(atSubVect(Efb, Tfbob , EfbT)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step3 error.\n");
    goto ComputeDET2XY_end;
  }
  if(atRotVect(Rfbob, EfbT, Eob)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step4 error.\n");
    goto ComputeDET2XY_end;
  }

  
  /* Step 8: Event Positions transformation from OB into DET2 frame */
  if(RotoTranslate(Qdet2ob, Tdet2ob, Eob, Edet2mm)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step5 error.\n");
    goto ComputeDET2XY_end;
  }
  headas_chat(CHATTY, "%s: Info: Edet2mm =%f %f %f\n",global.taskname,Edet2mm[0],Edet2mm[1],Edet2mm[2]);  

  if(atDivVect(SUBPIX_SIZE_MM, Edet2mm, Edet2px)){
    headas_chat(CHATTY,"Error: ComputeDET2XY: step6 error.\n");
    goto ComputeDET2XY_end;
  }


  det2x_loc = (int)(Edet2px[0]+1);
  det2y_loc = (int)(Edet2px[1]+1);

  if(det2x_loc<DET2X_MIN||det2x_loc>DET2X_MAX||det2y_loc<DET2Y_MIN||det2y_loc>DET2Y_MAX){
    headas_chat(CHATTY, "%s: Warning: (TIME=%f DET1X=%d DET1Y=%d) - DET2X=%d DET2Y=%d out of range.\n", global.taskname, time, det1x, det1y, det2x_loc, det2y_loc);
    det2x_loc = KWVL_DET2NULL;
    det2y_loc = KWVL_DET2NULL;
  }

  *det2x = det2x_loc;
  *det2y = det2y_loc;


  return OK;

 ComputeDET2XY_end:
  return NOT_OK;

} /* ComputeDET2XY */


/*
 *      GetFBtoOBinfo
 *
 *	DESCRIPTION:
 *            
 *            double time,      -> IN
 *            MastInfo_t *mast, -> IN
 *            int mastcount,    -> IN
 *            AtVect Tfbob,     -> OUT
 *            AtRotMat Rfbob    -> OUT
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int GetFBtoOBinfo(double time, MastInfo_t *mast, int mastcount, AtVect Tfbob, AtRotMat Rfbob){

  int      n,i;
  AtQuat   q;
  int      low=0, high=0, mid;


  /* Checks if no data available  */
  if(mastcount==0)
    goto GetFBtoOBinfo_end;
  
  if(time<(mast[0].time - TIME_SENS)){
    headas_chat(NORMAL, "%s: Error: No mast aspect data for TIME=%f\n", global.taskname, time);
    goto GetFBtoOBinfo_end;  
  }

  if(mastcount==1){
    atCopyVect(mast[0].Tfbob, Tfbob);
    if(atQuatToRM(mast[0].Qfbob, Rfbob)){
      headas_chat(CHATTY,"Error: GetFBtoOBinfo: step1 error\n");
      goto GetFBtoOBinfo_end;  
    }
    return OK;
  }
  else if(time >= (mast[mastcount-1].time + TIME_SENS)){
    atCopyVect(mast[mastcount-1].Tfbob, Tfbob);
    if(atQuatToRM(mast[mastcount-1].Qfbob, Rfbob)){
      headas_chat(CHATTY,"Error: GetFBtoOBinfo: step2 error\n");
      goto GetFBtoOBinfo_end;  
    }
    return OK;
  }
  else{
    /* Find appropriate row ( index 'i' indicates the first row over 'time' value) */
    low = 0;
    high = mastcount-1;

    while (low != high) {
      mid = low + (high-low)/2;
      if (mast[mid].time <= time) {
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

    /* Interpolate translation vectors */
    for(n=0; n<3; n++){
      InterpolateValues(mast[i-1].time,mast[i].time,time,mast[i-1].Tfbob[n],mast[i].Tfbob[n],&(Tfbob[n]));
    }
    headas_chat(CHATTY, "%s: Info: time=%f Tfbob= %f %f %f\n", global.taskname,time,Tfbob[0],Tfbob[1],Tfbob[2]);

    /* Interpolate q-parameters and compute rotation matrix */
    if(atInterpolateQuat(mast[i-1].time, mast[i-1].Qfbob, mast[i].time, mast[i].Qfbob, time, q)){
      headas_chat(CHATTY,"Error: GetFBtoOBinfo: step3 error\n");
      goto GetFBtoOBinfo_end;  
    }
    headas_chat(CHATTY, "%s: Info: time=%f Qfbob= %f %f %f %f\n", global.taskname,time,q[0],q[1],q[2],q[3]);

    if(atQuatToRM(q, Rfbob)){
      headas_chat(CHATTY,"Error: GetFBtoOBinfo: step4 error\n");
      goto GetFBtoOBinfo_end;  
    }

    return OK;
  }

 GetFBtoOBinfo_end:
  return NOT_OK;

} /* GetFBtoOBinfo */


void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value){

  double delta, frac1, frac2;

  delta = up - down;
  frac1 = (up - this) / delta;
  frac2 = (this - down) /delta;

  *value = valuedown * frac1 + valueup * frac2;

}/* InterpolateValues */


