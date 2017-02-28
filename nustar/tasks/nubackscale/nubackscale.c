/*
 * 
 *	nubackscale.c
 *
 *	INVOCATION:
 *
 *		nubackscale [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 06/06/13 - First version ( from nubkgcorr v0.1.1 old task )
 *        0.1.1 - NS 10/07/13 - Handle new input parameters of nuexpomap task
 *        0.1.2 - NS 28/03/14 - Added 'inexpomapfile' input parameter
 *                            - Handle compressed source and background PHA input files
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nubackscale  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nubackscale.h"


/********************************/
/*         definitions          */
/********************************/

#define NUBACKSCALE_C
#define NUBACKSCALE_VERSION      "0.1.2"
#define PRG_NAME               "nubackscale"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nubackscale_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nubackscale.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nubackscale_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 06/06/13 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nubackscale_getpar()
{
  
  if(PILGetBool(PAR_SRCCORRECT, &global.par.srccorrect))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCCORRECT);
      goto Error;	
    }

  if(global.par.srccorrect)
    {
      if(PILGetFname(PAR_SRCPHAFILE, global.par.srcphafile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCPHAFILE);
	  goto Error;
	}

      if(PILGetFname(PAR_SRCOUTFILE, global.par.srcoutfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCOUTFILE);
	  goto Error;	
	}
    }


  if(PILGetBool(PAR_BKGCORRECT, &global.par.bkgcorrect))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BKGCORRECT);
      goto Error;	
    }

  if(global.par.bkgcorrect)
    {
      if(PILGetFname(PAR_BKGPHAFILE, global.par.bkgphafile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BKGPHAFILE);
	  goto Error;
	}
  
      if(PILGetFname(PAR_BKGOUTFILE, global.par.bkgoutfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BKGOUTFILE);
	  goto Error;	
	}
    }

  if(PILGetFname(PAR_EVTFILE, global.par.evtfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVTFILE);
      goto Error;
    }


  if(PILGetFname(PAR_INEXPOMAPFILE, global.par.inexpomapfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INEXPOMAPFILE);
      goto Error;	
    }
  if (!(strcasecmp (global.par.inexpomapfile, DF_NONE)))
    global.getexpomapfile = FALSE;
  else
    global.getexpomapfile = TRUE;


  if(!global.getexpomapfile){
    
    if(PILGetFname(PAR_PIXPOSFILE, global.par.pixposfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXPOSFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_MASTASPECTFILE, global.par.mastaspectfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MASTASPECTFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_TELDEF, global.par.teldef)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TELDEF);
	goto Error;	
      }

    if(PILGetFname(PAR_INSTRPROBMAPFILE, global.par.instrprobmapfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INSTRPROBMAPFILE);
	goto Error;	
      }

    if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ABERRATION);
	goto Error;	
      }

    if(PILGetFname(PAR_DET1REFFILE, global.par.det1reffile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1REFFILE);
	goto Error;	
      }

    if(PILGetInt(PAR_PIXBIN, &global.par.pixbin))
      { 
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXBIN); 
	goto Error;	 
      }

    if(PILGetReal(PAR_PERCENT, &global.par.percent))
      { 
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PERCENT); 
	goto Error;	 
      }

    if(PILGetBool(PAR_INITSEED, &global.par.initseed))
      { 
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INITSEED); 
	goto Error;	 
      }

  }

/*   if(PILGetFname(PAR_USRGTIFILE, global.par.usrgtifile))  */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USRGTIFILE); */
/*       goto Error;	 */
/*     } */
  strcpy(global.par.usrgtifile, "NONE");


  get_history(&global.hist);
  nubackscale_info();
  
  return OK;
  
 Error:

  return NOT_OK;
  
} /* nubackscale_getpar */


/*
 *	nubackscale_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nubackscale_checkinput();
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
 *             int rename (char *, char *);
 *             int CopyFile(char *source, char *destination);
 *             int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, 
 *                                char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);
 *	
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 06/06/13 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nubackscale_work()
{
  char               cmd[BUF_SIZE];
  int                status=OK;
  char               BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  char               zipfile[MAXFNAME_LEN], ext[MAXEXT_LEN];

  pid_t              pid;

  /* Get pid */
  pid = getpid();


  if(nubackscale_checkinput())
    goto Error;


  /* Create the task temporary directory */
  if(mkdir(global.tmp.dirname,0777)){
    headas_chat(NORMAL, "%s: Error: Unable to create the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
    goto Error;
  }


  /* Create filtered event file if usrgtifile!=NONE */
  if (strcasecmp (global.par.usrgtifile, DF_NONE))
    {

      /* Create local link to xselect input files */

      SplitFilePath(global.par.evtfile, DirName, BaseName);
      sprintf(global.tmp.xsel_infile, "%s/%s", global.tmp.dirname, BaseName);
      
      if(CreateAbsSymbolicLink(global.par.evtfile, global.tmp.xsel_infile ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmp.xsel_infile, global.par.evtfile);
	  goto Error;
	}
      
      SplitFilePath(global.par.usrgtifile, DirName, BaseName);
      sprintf(global.tmp.xsel_gtifile, "%s/%s", global.tmp.dirname, BaseName);
      
      if(CreateAbsSymbolicLink(global.par.usrgtifile, global.tmp.xsel_gtifile ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmp.xsel_gtifile, global.par.usrgtifile);
	  goto Error;
	}

      
      /* Create Xselect temporary xco input file */
      if(CreateXselXco(global.tmp.xsel_infile, global.tmp.xsel_gtifile, global.tmp.filtered_evt, global.tmp.xsel_in))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create xselect temporary input file %s.\n", global.taskname, global.tmp.xsel_in);
	  goto Error;
	}
      
      
      /* Execute xselect ftool */
      sprintf(cmd, "xselect @%s", global.tmp.xsel_in);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary filtered event file '%s'\n", global.taskname, global.tmp.filtered_evt);
	goto Error;
      }

      
      /* Remove local link of xselect input files  */
      
      if( unlink(global.tmp.xsel_infile) ){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmp.xsel_infile);
      }
       
      if( unlink(global.tmp.xsel_gtifile) ){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmp.xsel_gtifile);
      }
            
      if( remove(global.tmp.xsel_in)==-1 ){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, global.tmp.xsel_in);
      }

      
      /* The input file of nuexpomap will be the filtered event file */
      strcpy(global.tmp.expo_infile, global.tmp.filtered_evt);

    }
  else
    {
      /* The input file of nuexpomap will be the input event file */
      strcpy(global.tmp.expo_infile, global.par.evtfile);
    }

  
  /* Correction of background PHA FITS File */
  if( global.par.bkgcorrect )
    {

      /* Create temporary output file */

      GetFilenameExtension(global.par.bkgphafile, ext);
      if ( !(strcmp(ext, "gz")) )
	{
	  sprintf(zipfile, "%s.gz", global.tmp.loc_bkgoutfile);
	  
	  if(CopyFile(global.par.bkgphafile, zipfile))
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
	  if(CopyFile(global.par.bkgphafile, global.tmp.loc_bkgoutfile))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to create temporary file %s.\n", global.taskname, global.tmp.loc_bkgoutfile);
	      goto Error;
	    }
	}
     
      
      if( CorrectBackSpectrum(global.tmp.loc_bkgoutfile, global.tmp.expo_infile) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to correct BACKSCAL keyword of background PHA FITS File\n", global.taskname);
	  goto Error;
	}


      /* Rename temporary file into output file */
      if (RenameFile(global.tmp.loc_bkgoutfile,global.par.bkgoutfile) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to rename temporary file '%s' to '%s'.\n", global.taskname, global.tmp.loc_bkgoutfile, global.par.bkgoutfile);
	  goto Error;
	}

      headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.bkgoutfile);
    }


  /* Correction of source PHA FITS File */
  if( global.par.srccorrect )
    {

      /* Create temporary output file */

      GetFilenameExtension(global.par.srcphafile, ext);
      if ( !(strcmp(ext, "gz")) )
	{
	  sprintf(zipfile, "%s.gz", global.tmp.loc_srcoutfile);
	  
	  if(CopyFile(global.par.srcphafile, zipfile))
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
	  if(CopyFile(global.par.srcphafile, global.tmp.loc_srcoutfile))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to create temporary file %s.\n", global.taskname, global.tmp.loc_srcoutfile);
	      goto Error;
	    }
	}

      
      if( CorrectBackSpectrum(global.tmp.loc_srcoutfile, global.tmp.expo_infile) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to correct BACKSCAL keyword of source PHA FITS File\n", global.taskname);
	  goto Error;
	}


      /* Rename temporary file into output file */
      if (RenameFile(global.tmp.loc_srcoutfile,global.par.srcoutfile) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to rename temporary file '%s' to '%s'.\n", global.taskname, global.tmp.loc_srcoutfile, global.par.srcoutfile);
	  goto Error;
	}

      headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.srcoutfile);
    }


  /* Cleanup Temporary Files */

  if ( FileExists(global.tmp.filtered_evt) ) {
    if(remove (global.tmp.filtered_evt) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmp.filtered_evt);
    }
  }

  
  /* Delete the task temporary directory */
  if(rmdir(global.tmp.dirname)){
    perror("rmdir");
    headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
  }


  return OK; 
  
 Error:

  return NOT_OK;

} /* nubackscale_work */


/*
 *	nubackscale
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
 *             void nubackscale_getpar(void);
 * 	       void nubackscale_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 06/06/13 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nubackscale()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUBACKSCALE_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nubackscale_getpar() == OK) 
    {
      
      if ( nubackscale_work()) 
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
      else
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	}

    }
  
  return OK;
  
 pdcorr_end:

  return NOT_OK;
  
} /* nubackscale */


/*
 *	nubackscale_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 06/06/13 - First version
 *
 */
void nubackscale_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  if(global.par.srccorrect){
    headas_chat(NORMAL,"Name of the input source PHA FITS File                :'%s'\n",global.par.srcphafile);
    headas_chat(NORMAL,"Name of the output source PHA FITS File               :'%s'\n",global.par.srcoutfile);
  }
  if(global.par.bkgcorrect){
    headas_chat(NORMAL,"Name of the input background PHA FITS File            :'%s'\n",global.par.bkgphafile);
    headas_chat(NORMAL,"Name of the output background PHA FITS File           :'%s'\n",global.par.bkgoutfile);
  }
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.evtfile);

  if(!global.getexpomapfile){
    headas_chat(NORMAL,"Name of the input pixel location file                 :'%s'\n",global.par.pixposfile);
    headas_chat(NORMAL,"Name of the input alignment file                      :'%s'\n",global.par.alignfile);
    headas_chat(NORMAL,"Name of the input Mast Aspect Solution file           :'%s'\n",global.par.mastaspectfile);
    headas_chat(NORMAL,"Name of the teldef calibration file                   :'%s'\n",global.par.teldef);
    headas_chat(NORMAL,"Name of the input Instrument Probability Map file     :'%s'\n",global.par.instrprobmapfile);
    headas_chat(NORMAL,"Name of the input DET1 Reference Point file           :'%s'\n",global.par.det1reffile);
    headas_chat(NORMAL,"Bin size of aspect histogram in pixels                :'%d'\n",global.par.pixbin);
  }

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* nubackscale_info */


/*
 *	nubackscale_checkinput
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
 *        0.1.0: - NS 06/06/13 - First version
 *
 */
int nubackscale_checkinput(void)
{
  char           BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN], ext[MAXEXT_LEN];
  pid_t          pid;

  /* Get pid */
  pid = getpid();


  /* Set temporary task directory name */
  sprintf(global.tmp.dirname, "%d_tmp_nubkgphacorr", (int)pid);


  if(global.par.bkgcorrect)
    {
      /* Check if bkgoutfile == NONE */    
      if (!(strcasecmp (global.par.bkgoutfile, DF_NONE)))
	{
	  /* If bkgoutfile == NONE check if input file is compressed */
	  GetFilenameExtension(global.par.bkgphafile, ext);
	  if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	    {
	      headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.bkgphafile);
	      headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      /* Overwrite input file */
	      strcpy(global.par.bkgoutfile, global.par.bkgphafile);
	    }
	}
      else
	{
	  if(FileExists(global.par.bkgoutfile))
	    {
	      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.bkgoutfile);
	      if(!headas_clobpar)
		{
		  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.bkgoutfile);
		  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
		  goto check_end;
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
		  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.bkgoutfile);
		  if( strcmp(global.par.bkgoutfile,global.par.bkgphafile) ){
		    if(remove (global.par.bkgoutfile) == -1)
		      {
			headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
			headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.bkgoutfile);
			goto check_end;
		      }
		  }
		}
	    }
	}
   
      
      /* Derive the temporary name of the output file */
      SplitFilePath(global.par.bkgoutfile, DirName, BaseName);
      sprintf(global.tmp.loc_bkgoutfile, "%s/%dout_%s", global.tmp.dirname, (int)pid, BaseName);    

      /* Check if temporary output file exists to remove it */
      if(FileExists(global.tmp.loc_bkgoutfile))
	if(remove (global.tmp.loc_bkgoutfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmp.loc_bkgoutfile);
	    goto check_end;
	  }
    }


  if(global.par.srccorrect)
    {
      /* Check if srcoutfile == NONE */    
      if (!(strcasecmp (global.par.srcoutfile, DF_NONE)))
	{
	  /* If srcoutfile == NONE check if input file is compressed */
	  GetFilenameExtension(global.par.srcphafile, ext);
	  if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	    {
	      headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.srcphafile);
	      headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      /* Overwrite input file */
	      strcpy(global.par.srcoutfile, global.par.srcphafile);
	    }
	}
      else
	{
	  if(FileExists(global.par.srcoutfile))
	    {
	      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.srcoutfile);
	      if(!headas_clobpar)
		{
		  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.srcoutfile);
		  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
		  goto check_end;
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
		  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.srcoutfile);
		  if( strcmp(global.par.srcoutfile,global.par.srcphafile) ){
		    if(remove (global.par.srcoutfile) == -1)
		      {
			headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
			headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.srcoutfile);
			goto check_end;
		      }
		  }
		}
	    }
	}
   
      
      /* Derive the temporary name of the output file */
      SplitFilePath(global.par.srcoutfile, DirName, BaseName);
      sprintf(global.tmp.loc_srcoutfile, "%s/%dout_%s", global.tmp.dirname, (int)pid, BaseName);    

      /* Check if temporary output file exists to remove it */
      if(FileExists(global.tmp.loc_srcoutfile))
	if(remove (global.tmp.loc_srcoutfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmp.loc_srcoutfile);
	    goto check_end;
	  }
    }

  
  /* Derive temporary xselect input file name */
  sprintf(global.tmp.xsel_in, "%dtmp_srcevt.xco", (int)pid);

  if(FileExists(global.tmp.xsel_in))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmp.xsel_in);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmp.xsel_in);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmp.xsel_in);
	  if(remove (global.tmp.xsel_in) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmp.xsel_in);
	      goto check_end;
	    }
	}
    }
  
  /* Derive temporary filtered event file name */
  sprintf(global.tmp.filtered_evt, "%dtmp_evt.fits", (int)pid);

  if(FileExists(global.tmp.filtered_evt))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmp.filtered_evt);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmp.filtered_evt);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmp.filtered_evt);
	  if(remove (global.tmp.filtered_evt) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmp.filtered_evt);
	      goto check_end;
	    }
	}
    }


  return OK;

 check_end:
  return NOT_OK;

} /* nubackscale_checkinput */


int ReadPhaImg(char *filename, float **img, PhaImgInfo_t *imginfo){

  int                   status=OK, anynull=0, nfound;
  long                  dimen; 
  long                  naxes[2];
  int                   x0, y0;
  BOOL                  nubacksc;
  double                crpix1p, crpix2p, crval1p, crval2p, exposure, backscal;
  FitsHeader_t          head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 

  if (fits_read_key(inunit,TINT,KWNM_X_OFFSET, &x0,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_X_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 

  if (fits_read_key(inunit,TINT,KWNM_Y_OFFSET, &y0,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_Y_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  if (fits_read_key(inunit,TDOUBLE,KWNM_CRPIX1P, &crpix1p,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_CRPIX1P);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 

  if (fits_read_key(inunit,TDOUBLE,KWNM_CRVAL1P, &crval1p,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_CRVAL1P);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 

  if (fits_read_key(inunit,TDOUBLE,KWNM_CRPIX2P, &crpix2p,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_CRPIX2P);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 

  if (fits_read_key(inunit,TDOUBLE,KWNM_CRVAL2P, &crval2p,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_CRVAL2P);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  if (fits_read_key(inunit,TDOUBLE,KWNM_EXPOSURE, &exposure,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_EXPOSURE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }


  dimen = naxes[0]*naxes[1];
  *img = (float*)malloc(sizeof(float)*dimen);
  if(*img==NULL){
    headas_chat(CHATTY,"%s: Error: ReadPhaImg: memory allocation failure.\n", global.taskname);
    goto ReadPhaImg_end;
  }


  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, *img, &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  
  /* Move in SPECTRUM extension */
  if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_SPECTRUM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SPECTRUM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadPhaImg_end;
    }

  if (fits_read_key(inunit,TDOUBLE,KWNM_BACKSCAL, &backscal,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_BACKSCAL);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  if (fits_read_key(inunit,TLOGICAL,KWNM_NUBACKSC, &nubacksc,NULL, &status))
    {
      nubacksc = FALSE;
      status=0;
    }


  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  imginfo->xwidth = (int)naxes[0];
  imginfo->ywidth = (int)naxes[1];
  imginfo->x_offset = x0;
  imginfo->y_offset = y0;
  imginfo->crpix1p = crpix1p;
  imginfo->crval1p = crval1p;
  imginfo->crpix2p = crpix2p;
  imginfo->crval2p = crval2p;
  imginfo->exposure = exposure;
  imginfo->backscal = backscal;
  imginfo->nubacksc = nubacksc;


  return OK;

 ReadPhaImg_end:

  return NOT_OK;


} /* ReadPhaImg */


int ReadExpoFile(char *filename, float **expomap, ExposureInfo_t *info){

  int                   status=OK, anynull=0, nfound;
  long                  naxes[2], dimen;
  FitsCard_t            *card;
  FitsHeader_t           head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);


  /* Check VIGNAPP value */
  if( (ExistsKeyWord(&head, KWNM_VIGNAPP, NULLNULLCARD)) && (GetLVal(&head, KWNM_VIGNAPP)) )
    {
      headas_chat(NORMAL, "%s: Error: Input Exposure Map is corrected for vignetting.\n", global.taskname);
      goto ReadExpoFile_end;
    }

  /* Get ONTIME value */
  if(ExistsKeyWord(&head, KWNM_ONTIME, &card))
    {
      info->ontime = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ONTIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get LIVETIME value */
  if(ExistsKeyWord(&head, KWNM_LIVETIME, &card))
    {
      info->livetime = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_LIVETIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get DEADC value */
  if(ExistsKeyWord(&head, KWNM_DEADC, &card))
    {
      info->deadc = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEADC);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRPIX1P value */
  if(ExistsKeyWord(&head, KWNM_CRPIX1P, &card))
    {
      info->crpix1p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX1P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRPIX2P value */
  if(ExistsKeyWord(&head, KWNM_CRPIX2P, &card))
    {
      info->crpix2p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX2P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRVAL1P value */
  if(ExistsKeyWord(&head, KWNM_CRVAL1P, &card))
    {
      info->crval1p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL1P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRVAL2P value */
  if(ExistsKeyWord(&head, KWNM_CRVAL2P, &card))
    {
      info->crval2p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL2P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }


  if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }
  
  info->xwidth = (int)naxes[0];
  info->ywidth = (int)naxes[1];
  
  dimen = info->xwidth * info->ywidth ;
  *expomap = (float*)malloc(sizeof(float)*dimen);
  if(*expomap==NULL){
    headas_chat(CHATTY,"%s: Error: ReadExpoFile: memory allocation failure.\n", global.taskname);
    goto ReadExpoFile_end;
  }

  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, *expomap, &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }


  return OK;

 ReadExpoFile_end:

  CloseFitsFile(inunit);

  return NOT_OK;

} /* ReadExpoFile */


int ComputeExpoMean(float *pha, PhaImgInfo_t *phainfo, float *expo, ExposureInfo_t *expoinfo, double *expomean){

  double          xpha_skyoff, ypha_skyoff, xexpo_skyoff, yexpo_skyoff, expocorr_sum=0;
  int             xpha_expooff, ypha_expooff;
  int             xx, yy, xx_pha, yy_pha, expocorr_num=0;
/*   float           *expocorr; */


/*   expocorr = (float*)malloc(sizeof(float) * expoinfo->xwidth * expoinfo->ywidth); */
/*   if(expocorr==NULL){ */
/*     headas_chat(CHATTY,"%s: Error: ComputeExpoMean: memory allocation failure.\n", global.taskname); */
/*     goto ComputeExpoMean_end; */
/*   } */


  /* SKY coordinates offset of the input pha image */
  xpha_skyoff = phainfo->crval1p + phainfo->crpix1p-1 ;
  ypha_skyoff = phainfo->crval2p + phainfo->crpix2p-1 ;

  /* SKY coordinates offset of the input exposure map */
  xexpo_skyoff = expoinfo->crval1p + expoinfo->crpix1p-1 ;
  yexpo_skyoff = expoinfo->crval2p + expoinfo->crpix2p-1 ;

  /* coordinates offset of the input pha image in the exposure map coordinates system */
  xpha_expooff = (int) (xpha_skyoff - xexpo_skyoff +0.5) ;
  ypha_expooff = (int) (ypha_skyoff - yexpo_skyoff +0.5) ;


  for(yy=0; yy<expoinfo->ywidth; yy++){
    for(xx=0; xx<expoinfo->xwidth; xx++){
      
      yy_pha = yy - ypha_expooff;
      xx_pha = xx - xpha_expooff;
      
      if(yy_pha>=0 && yy_pha<phainfo->ywidth && xx_pha>=0 && xx_pha<phainfo->xwidth)
	{
	  if(pha[yy_pha*phainfo->xwidth + xx_pha] >= 0)
	    {
	      expocorr_sum += (double)expo[yy*expoinfo->xwidth + xx];
	      expocorr_num++;

	      headas_chat(CHATTY,"(xexpo=%d yexpo=%d) %f ", xx+1, yy+1, expo[yy*expoinfo->xwidth + xx]);
	    }
	}
      
    }
  }
  headas_chat(CHATTY,"\n");
  

  *expomean = expocorr_num>0 ? expocorr_sum/expocorr_num : 0;


  return OK;

/*  ComputeExpoMean_end: */

/*   return NOT_OK; */

} /* ComputeExpoMean */


int UpdatePhaKeys(char *filename, PhaKeysUpdate_t *phakeys){

  int             status=OK, inExt, outExt;
  BOOL            logical;
  fitsfile        *fptr=NULL;  


  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdatePhaKeys_end;
    }
   
  /* Get number of hdus in input file */
  if (fits_get_num_hdus(fptr, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdatePhaKeys_end;
    }
  
  /* Update all extension */
  outExt=1;

  while ( status == OK && outExt <= inExt)
    {
      if(fits_movabs_hdu(fptr, outExt, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto UpdatePhaKeys_end;
	}

/*       if(fits_update_key(fptr, TDOUBLE, KWNM_LIVETIME, &phakeys->livetime, CARD_COMM_LIVETIME, &status)) */
/* 	{ */
/* 	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_LIVETIME); */
/* 	  goto UpdatePhaKeys_end; */
/* 	} */
      
/*       if(fits_update_key(fptr, TDOUBLE, KWNM_EXPOSURE, &phakeys->exposure, CARD_COMM_EXPOSURE, &status)) */
/* 	{ */
/* 	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EXPOSURE); */
/* 	  goto UpdatePhaKeys_end; */
/* 	} */

      if(fits_update_key(fptr, TDOUBLE, KWNM_BACKSCAL, &phakeys->backscal, CARD_COMM_BACKSCAL, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_BACKSCAL);
	  goto UpdatePhaKeys_end;
	}

      logical=TRUE;
      if(fits_update_key(fptr, TLOGICAL, KWNM_NUBACKSC, &logical, CARD_COMM_NUBACKSC, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_NUBACKSC);
	  goto UpdatePhaKeys_end;
	}

      outExt++;
    }
 
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdatePhaKeys_end;
    }

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdatePhaKeys_end;
    }


  return OK;

 UpdatePhaKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK; 
  
} /* UpdatePhaKeys */


int CreateXselXco(char *evtfile, char *gtifile, char *evtfiltered, char *xcofile){
  
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
  fprintf(file, "set wmapname X Y\n");
  fprintf(file, "set xyname X Y\n");
  fprintf(file, "filter time file %s\n", gtifile);
  fprintf(file, "extract events copyall=yes\n");
  fprintf(file, "save events ./%s\n", evtfiltered);
  fprintf(file, "yes\n");
  fprintf(file, "quit\n");
  fprintf(file, "no\n");

  fclose(file);
  
  
  return OK;
  
 CreateXselXco_end:
  return NOT_OK;
  
} /* CreateXselXco */


int CorrectBackSpectrum(char *phafile, char *evtfile){

  int                status=OK;
  char               cmd[BUF_SIZE], aberration[5], initseed[5], clobber[5], history[5];
  char               expomapfile[PIL_LINESIZE];
  int                skysize=0;
  double             skyx=0., skyy=0., expomean=0.;
  float              *phaimg=NULL;
  PhaImgInfo_t       phaimginfo;
  float              *expo=NULL;
  ExposureInfo_t     expoinfo;
  PhaKeysUpdate_t    phakeys;
  pid_t              pid;

  /* Get pid */
  pid = getpid();


  /* Read background PHA image extension */
  if(ReadPhaImg(phafile, &phaimg, &phaimginfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", phafile);
    goto CorrectBackSpectrum_end;
  }


  if(phaimginfo.nubacksc){
  
    headas_chat(NORMAL, "%s: Warning: Backscale correction already applied to file, nothing to be done.\n", global.taskname);

    /* Free memory */
    free(phaimg);
    
    return OK;
  }


  if(!global.getexpomapfile){

    if(global.par.aberration)
      sprintf(aberration, "yes");
    else
      sprintf(aberration, "no");
    
    if(global.par.initseed)
      sprintf(initseed, "yes");
    else
      sprintf(initseed, "no");  
    
    if(headas_clobpar)
      sprintf(clobber, "yes");
    else
      sprintf(clobber, "no");
    
    if (global.hist)
      sprintf(history, "yes");
    else
      sprintf(history, "no");
    

    /* Derive the temporary name of the expomapfile */
    sprintf(global.tmp.loc_expomapfile, "%s/%dout_expomap.img", global.tmp.dirname, (int)pid);

    strcpy(expomapfile, global.tmp.loc_expomapfile);


    skysize = (int)( 0.5 + MAX(phaimginfo.xwidth, phaimginfo.ywidth) );
    skyx = skysize/2. - 0.5 + phaimginfo.crval1p + phaimginfo.crpix1p-1 ;
    skyy = skysize/2. - 0.5 + phaimginfo.crval2p + phaimginfo.crpix2p-1 ;
    
    /* Execute nuexpomap */
    sprintf(cmd, "nuexpomap infile=%s det1instrfile=NONE det2instrfile=NONE pixbin=%d skyx=%f skyy=%f skysize=%d expomapfile=%s aberration=%s attfile=%s skyinstrfile=NONE history=%s det1reffile=%s instrprobmapfile=%s initseed=%s mastaspectfile=%s vignflag=no alignfile=%s aspecthistofile=NONE pixposfile=%s teldef=%s offsetfile=NONE chatter=%d clobber=%s percent=%f indet2instrfile=NONE", evtfile, global.par.pixbin, skyx, skyy, skysize, global.tmp.loc_expomapfile, aberration, global.par.attfile, history, global.par.det1reffile, global.par.instrprobmapfile, initseed, global.par.mastaspectfile, global.par.alignfile, global.par.pixposfile, global.par.teldef, headas_chatpar, clobber, global.par.percent);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL,"%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmp.loc_expomapfile);
      goto CorrectBackSpectrum_end;
    }

  }
  else{
    strcpy(expomapfile, global.par.inexpomapfile);
  }


  /* Read exposure map */
  if(ReadExpoFile(expomapfile, &expo, &expoinfo)){
    goto CorrectBackSpectrum_end;
  }
  
  /* Compute exposure mean */
  if(ComputeExpoMean(phaimg, &phaimginfo, expo, &expoinfo, &expomean)){
    headas_chat(NORMAL,"%s: Error: Unable to compute exposure mean.\n", global.taskname);
    goto CorrectBackSpectrum_end;
  }

/*   phakeys.livetime = expomean; */
/*   phakeys.exposure = expomean; */
  phakeys.backscal = phaimginfo.backscal * (expomean / phaimginfo.exposure) ;
  

  /* Update keywords in phafile file */
  if(UpdatePhaKeys(phafile, &phakeys))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update keywords in '%s' file.\n", global.taskname, phafile);
      goto CorrectBackSpectrum_end;
    }


  /* Cleanup Temporary Files */

  if ( FileExists(global.tmp.loc_expomapfile) ) {
    if( remove(global.tmp.loc_expomapfile) == -1 ){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, global.tmp.loc_expomapfile);
    }
  }


  /* Free memory */

  free(phaimg);
  free(expo);


  return OK;

 CorrectBackSpectrum_end:
  return NOT_OK;

} /* CorrectBackSpectrum */
