/*
 * 
 *	numkrmf.c
 *
 *	INVOCATION:
 *
 *		numkrmf [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 02/03/12 - First version
 *        0.1.1 - NS 29/08/12 - Handle long file naming
 *        0.1.2 - NS 24/10/12 - RMF Grouping File CALDB query dependent on 'DEPTHCUT' keyword value
 *        0.1.3 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.4 - NS 14/12/12 - Added 'cmprmf' input parameter
 *        0.1.5 - NS 12/02/13 - Skip 'addrmf' run in the case of a single rmf file
 *        0.1.6 - NS 14/06/13 - Added 'usrgtifile' input parameter
 *        0.1.7 - NS 06/04/14 - Handle compressed RMF input file
 *        0.1.8 - NS 13/10/14 - Bug fixed while using remote CALDB
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB numkrmf  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "numkrmf.h"


/********************************/
/*         definitions          */
/********************************/

#define NUMKRMF_C
#define NUMKRMF_VERSION      "0.1.8"
#define PRG_NAME             "numkrmf"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	numkrmf_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 numkrmf.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void numkrmf_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 02/03/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkrmf_getpar()
{
  
  /* Input Event File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  /* Input Usr GTI File Name */
  if(PILGetFname(PAR_USRGTIFILE, global.par.usrgtifile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USRGTIFILE);
      goto Error;
    }

  /* Input Source Region File Name */
  if(PILGetFname(PAR_SRCREGIONFILE, global.par.srcregionfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCREGIONFILE);
      goto Error;	
    }
  
  /* Output RMF File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  if(PILGetFname(PAR_GRPRMFFILE, global.par.grprmffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRPRMFFILE);
      goto Error;
    }

  if(PILGetFname(PAR_RMFDIR, global.par.rmfdir)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_RMFDIR);
      goto Error;
    }

  if(PILGetBool(PAR_CMPRMF, &global.par.cmprmf))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CMPRMF);
      goto Error;	
    }


  get_history(&global.hist);
  numkrmf_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* numkrmf_getpar */


/*
 *	numkrmf_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int numkrmf_checkinput();
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
 *        0.1.0 - NS 02/03/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkrmf_work()
{
  int                status=OK, totevt=0;
  char               BaseName[MAXFNAME_LEN], CaldbName[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  char               singlermffile[PIL_LINESIZE], zipfile[MAXFNAME_LEN], ext[MAXEXT_LEN];
  long               extno;
  char               cmd[BUF_SIZE];
  GrpRmfInfo_t       grprmf[4];


  if(numkrmf_checkinput())
    goto Error;

  /* Create the task temporary directory */
  if(mkdir(global.tmpout.dirname,0777)){
    headas_chat(NORMAL, "%s: Error: Unable to create the temporary directory '%s'\n", global.taskname, global.tmpout.dirname);
    goto Error;
  }


  /* Get Observation Info from input file */
  if( GetObsInfo(global.par.infile, KWVL_EXTNAME_EVT, &global.obsinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.infile);
    goto Error;
  }


  /* Derive CALDB rmfdir name */
  if ( !strcasecmp(global.par.rmfdir,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_GRPRMF_DSET, CaldbName, HD_EXPR, &extno, global.obsinfo.instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for rmfdir input parameter.\n", global.taskname);
      	  goto Error;
      	}

      SplitFilePath(CaldbName, DirName, BaseName);
      strcpy(global.par.rmfdir, DirName);
      headas_chat(CHATTY,"Name of the input RMF files directory:'%s'\n",global.par.rmfdir);
    }

  RemoveTrailingSlash(global.par.rmfdir);


  /* Create local link to xselect input files */
  
  SplitFilePath(global.par.infile, DirName, BaseName);
  sprintf(global.tmpout.xsel_infile, "%s/%s", global.tmpout.dirname, BaseName);

  if(CreateAbsSymbolicLink(global.par.infile, global.tmpout.xsel_infile ))
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.xsel_infile, global.par.infile);
      goto Error;
    }


  SplitFilePath(global.par.srcregionfile, DirName, BaseName);
  sprintf(global.tmpout.xsel_srcregionfile, "%s/%s", global.tmpout.dirname, BaseName);

  if(CreateAbsSymbolicLink(global.par.srcregionfile, global.tmpout.xsel_srcregionfile ))
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.xsel_srcregionfile, global.par.srcregionfile);
      goto Error;
    }

    
  if (!(strcasecmp (global.par.usrgtifile, DF_NONE)))  /* usrgtifile == NONE */
    {
      sprintf(global.tmpout.xsel_usrgtifile, "NONE");
    }
  else
    {
      SplitFilePath(global.par.usrgtifile, DirName, BaseName);
      sprintf(global.tmpout.xsel_usrgtifile, "%s/%s", global.tmpout.dirname, BaseName);
      
      if(CreateAbsSymbolicLink(global.par.usrgtifile, global.tmpout.xsel_usrgtifile ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.xsel_usrgtifile, global.par.usrgtifile);
	  goto Error;
	}
    }


  /* Create Xselect temporary xco input file */
  if(CreateXselXco(global.tmpout.xsel_infile, global.tmpout.xsel_usrgtifile, global.tmpout.xsel_srcregionfile, global.tmpout.filtered_evt, global.tmpout.xsel_in))
    {
      headas_chat(NORMAL, "%s: Error: unable to create xselect temporary input file %s.\n", global.taskname, global.tmpout.xsel_in);
      goto Error; 
    }

  
  /* Execute xselect ftool */
  sprintf(cmd, "xselect @%s", global.tmpout.xsel_in);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create temporary filtered event file '%s'\n", global.taskname, global.tmpout.filtered_evt);
    goto Error;
  }


  /* Remove local link of xselect input files  */

  if( unlink(global.tmpout.xsel_infile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.xsel_infile);
  }

  if( unlink(global.tmpout.xsel_srcregionfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.xsel_srcregionfile);
  }

  if( (strcasecmp (global.par.usrgtifile, DF_NONE)) && unlink(global.tmpout.xsel_usrgtifile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.xsel_usrgtifile);
  }

  
  /* Read RMF Grouping data from grprmffile input */
  if( ReadGrpRmfFile(grprmf, &global.obsinfo) )
    {
      headas_chat(NORMAL,"%s: Error: unable to get RMP Grouping data from input grprmffile.\n",global.taskname);      
      goto Error;
    }


  /* Set 'evtcnt' values in 'grprmf' struct  */
  if( SetGrpRmfEvtCnt(global.tmpout.filtered_evt, grprmf, &totevt) )
    {     
      goto Error;
    }


  /* Check if only one rmf file - no addrmf needed */
  if( CheckSingleRmf(grprmf, singlermffile) )
    {
      /* Derive temporary addrmf output file name */
      SplitFilePath(global.par.outfile, DirName, BaseName);
      sprintf(global.tmpout.addrmf_outfile, "%s/%s", global.tmpout.dirname, BaseName); 
      sprintf(zipfile, "%s.gz", global.tmpout.addrmf_outfile);


      GetFilenameExtension(singlermffile, ext);
      if ( !(strcmp(ext, "gz")) )
	{
	  if(CopyFile(singlermffile, zipfile))
	    {
	      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, zipfile);
	      goto Error;
	    }
	  
	  /* Uncompress file */
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
	  /* Create temporary copy of the single rmf file */
	  if(CopyFile(singlermffile, global.tmpout.addrmf_outfile))
	    {
	      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.addrmf_outfile);
	      goto Error;
	    }
	}

      
      /* Update Keywords needed by cmprmf */
      if( UpdateRmfKeys(global.tmpout.addrmf_outfile) )
	{
	  headas_chat(NORMAL, "%s: Error: unable to update temporary file '%s'\n", global.taskname, global.tmpout.addrmf_outfile);
	  goto Error;
	}
      
    }
  else
    {
      /* Write temporary addrmf input ascii file name */
      if(WriteRmfListFile(grprmf, totevt, global.tmpout.rmflistfile)){
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' temporary file.\n", global.taskname, global.tmpout.rmflistfile);
	goto Error;
      }


      /* Derive temporary addrmf output file name */
      SplitFilePath(global.par.outfile, DirName, BaseName);
      sprintf(global.tmpout.addrmf_outfile, "%s/%s", global.tmpout.dirname, BaseName);

      /* Execute addrmf */
      sprintf(cmd, "addrmf list=@%s rmffile=%s", global.tmpout.rmflistfile, global.tmpout.addrmf_outfile);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create '%s' output RMF file.\n", global.taskname, global.tmpout.addrmf_outfile);
	goto Error;
      }

    }

      
  /* Execute cmprmf ftool */
  if( global.par.cmprmf ){
    sprintf(cmd, "cmprmf infile=%s outfile=%s threshold=1e-9 clobber=yes", global.tmpout.addrmf_outfile, global.tmpout.addrmf_outfile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: Unable to compress '%s' file.\n", global.taskname, global.tmpout.addrmf_outfile);
      goto Error;
    }
  }

  /* Execute fchecksum */
  sprintf(cmd, "fchecksum infile=%s update=yes", global.tmpout.addrmf_outfile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.tmpout.addrmf_outfile);
    goto Error;
  }


  /* Rename addrmf output file */
  if ( RenameFile(global.tmpout.addrmf_outfile, global.par.outfile) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: Unable to copy temporary file '%s' to '%s'.\n", global.taskname, global.tmpout.addrmf_outfile, global.par.outfile);
      goto Error;
    }
  

  /* Cleanup Temporary Files */

  if ( FileExists(global.tmpout.filtered_evt) ) {
    if(remove (global.tmpout.filtered_evt) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.filtered_evt);
    }
  }

  if ( FileExists(global.tmpout.xsel_in) ) {
    if(remove (global.tmpout.xsel_in) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.xsel_in);
    }
  }

  if ( FileExists(global.tmpout.xsel_log) ) {
    if(remove (global.tmpout.xsel_log) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.xsel_log);
    }
  }

  if ( FileExists("xsel_timefile.asc") ) {
    if(remove ("xsel_timefile.asc") == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove 'xsel_timefile.asc' temporary file.\n", global.taskname);
    }
  }

  if ( FileExists(global.tmpout.rmflistfile) ) {
    if(remove (global.tmpout.rmflistfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.rmflistfile);
    }
  }


  /* Delete the task temporary directory */
  if(rmdir(global.tmpout.dirname)){
    headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmpout.dirname);
  }


  
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

  return OK; 
  
 Error:

  return NOT_OK;

} /* numkrmf_work */


/*
 *	numkrmf
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
 *             void numkrmf_getpar(void);
 * 	       void numkrmf_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 02/03/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkrmf()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUMKRMF_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( numkrmf_getpar() == OK) 
    {
      
      if ( numkrmf_work()) 
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
	}
      
    }
  
  return OK;
  
 pdcorr_end:

  return NOT_OK;
  
} /* numkrmf */


/*
 *	numkrmf_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 02/03/12 - First version
 *
 */
void numkrmf_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the input source region file                  :'%s'\n",global.par.srcregionfile);
  headas_chat(NORMAL,"Name of the output RMF file                           :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input RMF Grouping file                   :'%s'\n",global.par.grprmffile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* numkrmf_info */


/*
 *	numkrmf_checkinput
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
 *        0.1.0: - NS 02/03/12 - First version
 *
 */
int numkrmf_checkinput(void)
{
  pid_t          pid;

  /* Get pid */
  pid=getpid();


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
  
  /* Derive temporary xselect input file name */
  sprintf(global.tmpout.xsel_in, "%dtmp_srcevt.xco", (int)pid);

  if(FileExists(global.tmpout.xsel_in))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.xsel_in);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.xsel_in);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.xsel_in);
	  if(remove (global.tmpout.xsel_in) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.xsel_in);
	      goto check_end;
	    }
	}
    }
  
  /* Derive temporary filtered event file name */
  sprintf(global.tmpout.filtered_evt, "%dtmp_evt.fits", (int)pid);

  if(FileExists(global.tmpout.filtered_evt))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.filtered_evt);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.filtered_evt);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.filtered_evt);
	  if(remove (global.tmpout.filtered_evt) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.filtered_evt);
	      goto check_end;
	    }
	}
    }

  /* Derive temporary addrmf input file name */
  sprintf(global.tmpout.rmflistfile, "%dtmp_rmflist.txt", (int)pid);

  if(FileExists(global.tmpout.rmflistfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.rmflistfile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.rmflistfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.rmflistfile);
	  if(remove (global.tmpout.rmflistfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.rmflistfile);
	      goto check_end;
	    }
	}
    }

  /* Set temporary xselect log file name */
  sprintf(global.tmpout.xsel_log, "xselect.log");


  /* Set temporary task directory name */
  sprintf(global.tmpout.dirname, "%d_tmp_numkrmf", (int)pid);

 
  return OK;

 check_end:
  return NOT_OK;
}


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
  

  /* Retrieve INSTRUME from input file */  
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(obsinfo->instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve DEPTHCUT from input file */  
  if(ExistsKeyWord(&head, KWNM_DEPTHCUT, &card))
    {
      strcpy(obsinfo->depthcut, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEPTHCUT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  /* Retrieve date-obs and time-obs from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      obsinfo->dateobs=card->u.SVal;
      if(!(strchr(obsinfo->dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    obsinfo->timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetObsInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  /* Retrieve date-end and time-end from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      obsinfo->dateend=card->u.SVal;
      if(!(strchr(obsinfo->dateend, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
	    obsinfo->timeend=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEEND);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetObsInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEEND);
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


int CreateXselXco(char *evtfile, char *usrgtifile, char *regfile, char *evtfiltered, char *xcofile){
  
  char     BaseName[MAXFNAME_LEN], DirName1[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  FILE	   *file;
  pid_t    pid;
  
  /* Get pid */
  pid=getpid();
  
  
  /* Get dirname and basename of input evt file */
  SplitFilePath(evtfile, DirName1, BaseName);

  if(DirName1[0]=='/'){
    strcpy(DirName, DirName1);
  }
  else{
    sprintf(DirName, "./%s", DirName1);
  }

  
  if (!(file = fopen(xcofile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,xcofile);
    goto CreateXselXco_end;
  }
  
  fprintf(file, "xsel%d\n", pid);
  fprintf(file, "read eve %s\n", BaseName);
  fprintf(file, "%s\n", DirName);
  fprintf(file, "\n");
  fprintf(file, "set xyname X Y\n");

  if (strcasecmp (usrgtifile, DF_NONE))  /* usrgtifile != NONE */
    fprintf(file, "filter time file \"%s\"\n", usrgtifile);

  fprintf(file, "filter region %s\n", regfile);
  fprintf(file, "extract events copyall=yes\n");
  fprintf(file, "save events ./%s\n", evtfiltered);
  fprintf(file, "no\n");
  fprintf(file, "quit\n");
  fprintf(file, "no\n");

  fclose(file);
  
  
  return OK;
  
 CreateXselXco_end:
  return NOT_OK;
  
} /* CreateXselXco */


/*
 *
 *      ReadGrpRmfFile
 *
 *	DESCRIPTION:
 *           Routine to read input grprmffile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadGrpRmfFile(GrpRmfInfo_t grprmf[4], const ObsInfo_t *evt){
  

  char        grprmffile0[PIL_LINESIZE], grprmffile1[PIL_LINESIZE], grprmffile2[PIL_LINESIZE], grprmffile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;
  char        expr[FLEN_VALUE+12];


  /* Derive CALDB gain filename */  
  if ( !strcasecmp(global.par.grprmffile,DF_CALDB) )
    {
      sprintf(expr,"DEPTHCUT.eq.%s", evt->depthcut);

      /* Retrieve DET0 grprmf filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_GRPRMF_DSET, grprmffile0, expr, &extfile0, evt->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grprmffile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadGrpRmfFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 grprmf filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_GRPRMF_DSET, grprmffile1, expr, &extfile1, evt->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grprmffile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadGrpRmfFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 grprmf filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_GRPRMF_DSET, grprmffile2, expr, &extfile2, evt->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grprmffile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadGrpRmfFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 grprmf filename */
      if (CalGetFileName(HD_MAXRET, evt->dateobs, evt->timeobs, evt->dateend, evt->timeend, KWVL_GRPRMF_DSET, grprmffile3, expr, &extfile3, evt->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grprmffile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadGrpRmfFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(grprmffile0, global.par.grprmffile);
    strcpy(grprmffile1, global.par.grprmffile);
    strcpy(grprmffile2, global.par.grprmffile);
    strcpy(grprmffile3, global.par.grprmffile);
  }

  /* Retrieve DET0 grprmf info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for RMF Grouping data of detector %s.\n", global.taskname, grprmffile0, KWVL_DETNAM_DET0);
  if( ReadGrpRmfInfo(&grprmf[0], grprmffile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read RMF Grouping data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input grprmf file: %s.\n", global.taskname, grprmffile0);
      goto ReadGrpRmfFile_end;
    }

  /* Retrieve DET1 grprmf info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for RMF Grouping data of detector %s.\n", global.taskname, grprmffile1, KWVL_DETNAM_DET1);
  if( ReadGrpRmfInfo(&grprmf[1], grprmffile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read RMF Grouping data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input grprmf file: %s.\n", global.taskname, grprmffile1);
      goto ReadGrpRmfFile_end;
    }

  /* Retrieve DET2 grprmf info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for RMF Grouping data of detector %s.\n", global.taskname, grprmffile2, KWVL_DETNAM_DET2);
  if( ReadGrpRmfInfo(&grprmf[2], grprmffile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read RMF Grouping data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input grprmf file: %s.\n", global.taskname, grprmffile2);
      goto ReadGrpRmfFile_end;
    }

  /* Retrieve DET3 grprmf info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for RMF Grouping data of detector %s.\n", global.taskname, grprmffile3, KWVL_DETNAM_DET3);
  if( ReadGrpRmfInfo(&grprmf[3], grprmffile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read RMF Grouping data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input grprmf file: %s.\n", global.taskname, grprmffile3);
      goto ReadGrpRmfFile_end;
    }


  return OK;
  
 ReadGrpRmfFile_end:
  
  return NOT_OK;

} /* ReadGrpRmfFile */


/*
 *
 *      ReadGrpRmfInfo
 *
 *	DESCRIPTION:
 *           Routine to get RMF Grouping File data
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadGrpRmfInfo(GrpRmfInfo_t *info, char *filename, long extno, char *detnam){

  int                n, status=OK, found=NOT_OK, count=0;
  int                inExt, totExt;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  GrpRmfCol_t        indxcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only input file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadGrpRmfInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input grprmffile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadGrpRmfInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to GRPRMF extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadGrpRmfInfo_end;
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

	  if( !strcmp(r_extname,KWVL_EXTNAME_GRPRMF) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_GRPRMF,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadGrpRmfInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read grprmf bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }


  /* Get columns index from name */

  if ((indxcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }

  if ((indxcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }

  if ((indxcol.XEXTENT = GetColNameIndx(&table, CLNM_XEXTENT)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XEXTENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }

  if ((indxcol.YEXTENT = GetColNameIndx(&table, CLNM_YEXTENT)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YEXTENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }

  if ((indxcol.RMFFILE = GetColNameIndx(&table, CLNM_RMFFILE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RMFFILE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }
  info->rmffile_dim = table.Multiplicity[indxcol.RMFFILE];


  EndBintableHeader(&head, &table);
  
  
  /* Allocate memory to storage all data */
  info->nrows = table.MaxRows;
  info->row = (GrpRmfRow_t *)calloc(info->nrows, sizeof(GrpRmfRow_t));
  if(info==NULL){
    headas_chat(CHATTY,"%s: Error: ReadGrpRmfInfo: memory allocation failure.\n", global.taskname);
    goto ReadGrpRmfInfo_end;
  }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while((count<info->nrows) && (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
    {
      for (n=0; n<ReadRows; ++n)
	{
	  info->row[count].rawx = BVEC(table,n,indxcol.RAWX);
	  info->row[count].rawy = BVEC(table,n,indxcol.RAWY);
	  info->row[count].xextent = BVEC(table,n,indxcol.XEXTENT);
	  info->row[count].yextent = BVEC(table,n,indxcol.YEXTENT);
	  info->row[count].rmffile = (char *)malloc(info->rmffile_dim*sizeof(char));
	  strcpy(info->row[count].rmffile, SVEC(table,n,indxcol.RMFFILE));

	  info->row[count].evtcnt = 0;

	  count++;
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  info->nrows = count;


  /* Close file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadGrpRmfInfo_end;
    }



  return OK;
  
 ReadGrpRmfInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadGrpRmfInfo */


int SetGrpRmfEvtCnt(char *filename, GrpRmfInfo_t grprmf[4], int *totevt){

  int                n, status=OK;
  int                rawx, rawy, det_id;
  EvtCol_t           indxcol;
  unsigned           FromRow, ReadRows, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }
  
  /* Move in events extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }

  head = RetrieveFitsHeader(unit);
  
  /* Read Bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, filename);
      return OK;
    }


  /* Get columns index from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }

  if ((indxcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }

  if ((indxcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }

  if ((indxcol.DET_ID = GetColNameIndx(&table, CLNM_DET_ID)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DET_ID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }


  EndBintableHeader(&head, &table);
  

  /* Total number of events */
  *totevt = table.MaxRows;


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,indxcol.RAWX);
	  rawy = BVEC(table,n,indxcol.RAWY);
	  det_id = BVEC(table,n,indxcol.DET_ID);
	  
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Warning: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    continue;
	  }

	  UpdateEvtCnt(rawx, rawy, &grprmf[det_id]);

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto SetGrpRmfEvtCnt_end;
    }


  return OK;
  
 SetGrpRmfEvtCnt_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 } /* SetGrpRmfEvtCnt */


void UpdateEvtCnt(int rawx, int rawy, GrpRmfInfo_t *grprmf){

  int i;

  for(i=0; i<grprmf->nrows; i++){

    if( rawx>=grprmf->row[i].rawx && rawx<(grprmf->row[i].rawx+grprmf->row[i].xextent) &&
	rawy>=grprmf->row[i].rawy && rawy<(grprmf->row[i].rawy+grprmf->row[i].yextent) ){
      grprmf->row[i].evtcnt = grprmf->row[i].evtcnt + 1 ;
    }

  }

} /* UpdateEvtCnt */


int WriteRmfListFile(GrpRmfInfo_t grprmf[4], int totevt, char *outfile){

  int      i, detid;
  BOOL     empty=TRUE;
  double   weight;
  FILE	   *file;
  
  if (!(file = fopen(outfile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,outfile);
    goto WriteRmfListFile_end;
  }
  
  for(detid=0; detid<4; detid++){
    for(i=0; i<grprmf[detid].nrows; i++){
      
      if( grprmf[detid].row[i].evtcnt >0 ){
	empty=FALSE;
	weight = (double)grprmf[detid].row[i].evtcnt/(double)totevt;
	fprintf(file, "%s/%s %f\n", global.par.rmfdir, grprmf[detid].row[i].rmffile, weight);
      }

    }
  }

  fclose(file);
  
  if(empty)
    goto WriteRmfListFile_end;


  return OK;
  
 WriteRmfListFile_end:
    return NOT_OK;

} /* WriteRmfListFile */


BOOL CheckSingleRmf(GrpRmfInfo_t grprmf[4], char *rmffile){

  int      i, detid, count=0;

  
  for(detid=0; detid<4; detid++){
    for(i=0; i<grprmf[detid].nrows; i++){
      
      if( grprmf[detid].row[i].evtcnt >0 ){
	count++;
	
	if(count==1){
	  snprintf(rmffile, PIL_LINESIZE-1, "%s/%s", global.par.rmfdir, grprmf[detid].row[i].rmffile);
	}

	if(count>1){
	  return FALSE;
	}
      }

    }
  }

  if(count==1)
    return TRUE;
  else
    return FALSE;

} /* CheckSingleRmf */


int UpdateRmfKeys(char *filename){

  int             status=OK;
  double          dval=0.;
  FitsHeader_t	  head;
  FitsCard_t      *card;
  fitsfile        *fptr=NULL;  


  /* Open input file */
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateRmfKeys_end;
    }
  
  /* Move in MATRIX extension */
  if (fits_movnam_hdu(fptr, ANY_HDU, "MATRIX", 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname, "MATRIX");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateRmfKeys_end;
    }


  /* Retrieve Header Fits Pointer */
  head=RetrieveFitsHeader(fptr);

  
  /* LO_THRES */
  if(!(ExistsKeyWord(&head, "LO_THRES", &card)))
    {
      if(fits_update_key(fptr, TDOUBLE, "LO_THRES", &dval, "updated by numkrmf", &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, "LO_THRES");
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto UpdateRmfKeys_end;
	}
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateRmfKeys_end;
    }
  

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateRmfKeys_end;
    }


  return OK;

 UpdateRmfKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK; 
  
} /* UpdateRmfKeys */

