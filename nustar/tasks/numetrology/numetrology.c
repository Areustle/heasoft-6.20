/*
 * 
 *	numetrology.c
 *
 *	INVOCATION:
 *
 *		numetrology [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 24/11/11 - First version
 *        0.1.1 - NS 06/12/11 - Handle new columns of the PSD file
 *        0.1.2 - NS 20/12/11 - Fixed malloc error when processing large input psd files
 *        0.1.3 - NS 02/01/12 - Modified 'BilinearInterpolation' routine
 *                            - Modified scaling parameters in x_psd* and y_psd* computation
 *        0.1.4 - NS 28/03/12 - Modified to handle large input/output files 
 *                            - Handle 'MET_CMP' extension of input metrology file
 *        0.1.5 - NS 17/09/12 - Do not write rows with X_PSD* and/or Y_PSD* out of calibrated range 
 *                              in output corrected psd file
 *        0.1.6 - NS 15/04/13 - Handle rows with X_PSD* and/or Y_PSD* out of calibrated range in output corrected psd file
 *        0.1.7 - NS 12/09/13 - Added 'psdcal' input parameter
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB numetrology  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "numetrology.h"


/********************************/
/*         definitions          */
/********************************/

#define NUMETROLOGY_C
#define NUMETROLOGY_VERSION      "0.1.7"
#define PRG_NAME               "numetrology"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	numetrology_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 numetrology.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void numetrology_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 24/11/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numetrology_getpar()
{
  
  /* metflag */
  if(PILGetBool(PAR_METFLAG, &global.par.metflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_METFLAG); 
      goto Error;	 
    }
  
  if(global.par.metflag){

    /* Input Metrology File Name */
    if(PILGetFname(PAR_METROLOGYFILE, global.par.metrologyfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_METROLOGYFILE);
	goto Error;
      }
    
    /* Input Metrology Grid File Name */
    if(PILGetFname(PAR_METGRIDFILE, global.par.metgridfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_METGRIDFILE);
	goto Error;
      }
    
    /* Output Position Sensing Detector File Name */
    if(PILGetFname(PAR_OUTPSDFILE, global.par.outpsdfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTPSDFILE);
	goto Error;
      }
    
    /* Output corrected Position Sensing Detector File Name */
    if(PILGetFname(PAR_OUTPSDFILECOR, global.par.outpsdfilecor)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTPSDFILECOR);
	goto Error;
      }

    if(PILGetBool(PAR_PSDCAL, &global.par.psdcal))
      { 
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSDCAL); 
	goto Error;
      }

  }
  else{
    /* Input corrected Position Sensing Detector File Name */
    if(PILGetFname(PAR_INPSDFILECOR, global.par.inpsdfilecor)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INPSDFILECOR);
	goto Error;
      }
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

  /* Init test */
  /* Input Position Sensing Detector File Name */
/*   if(PILGetFname(PAR_INPSDFILE, global.par.inpsdfile))  */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INPSDFILE); */
/*       goto Error; */
/*     } */
/*   if ( !strcasecmp(global.par.inpsdfile,DF_NONE) ) */
/*     global.inpsdfile = FALSE; */
/*   else */
/*     global.inpsdfile = TRUE; */
  /* End test */
 

  get_history(&global.hist);
  numetrology_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* numetrology_getpar */


/*
 *	numetrology_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int numetrology_checkinput();
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
 *        0.1.0 - NS 24/11/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numetrology_work()
{
  MetrologyKeys_t      metkeys;
  static MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS];
  AlignInfo_t          aligninfo;
  

  if(numetrology_checkinput())
    goto Error;


  if(global.par.metflag){

    if(GetObsInfo(global.par.metrologyfile, KWVL_EXTNAME_MET_RAW))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get info from %s file.\n", global.taskname, global.par.metrologyfile);
	goto Error;
      }
    
    if(GetMetrologyKeys(global.par.metrologyfile, KWVL_EXTNAME_MET_RAW, &metkeys))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get metrology info.\n", global.taskname);
	goto Error;
      }
    
    /* Write output Position Sensing Detector File */
    if(WritePSDFile(global.par.metrologyfile, &metkeys, NULL, global.par.outpsdfile, global.par.psdcal))
      {
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file\n", global.taskname, global.par.outpsdfile);
	goto Error;
      }
    
    if(ReadMetGridFile(metgrid))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get metrology grid data.\n", global.taskname);
	goto Error;
      }

    /* Write output corrected Position Sensing Detector File */
    if(WritePSDFile(global.par.metrologyfile, &metkeys, metgrid, global.par.outpsdfilecor, global.par.psdcal))
      {
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file\n", global.taskname, global.par.outpsdfilecor);
	goto Error;
      }
    
  }
  else{

    if(GetObsInfo(global.par.inpsdfilecor, KWVL_EXTNAME_PSDPOS))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get info from %s file.\n", global.taskname, global.par.inpsdfilecor);
	goto Error;
      }

    if(GetMetrologyKeys(global.par.inpsdfilecor, KWVL_EXTNAME_PSDPOS, &metkeys))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get metrology info.\n", global.taskname);
	goto Error;
      }
  }


  /* Get Alignment data from input alignfile */
  if(ReadAlignFile(&aligninfo))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get alignment data.\n", global.taskname);
      goto Error;
    }
  

  /* Write output Mast Aspect Solution File */
  if(WriteMastAspectFile(global.metinfofile, &metkeys, &aligninfo, global.par.mastaspectfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write '%s' file\n", global.taskname, global.par.mastaspectfile);
      goto Error;
    }

  

  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

  return OK; 

  
 Error:

  return NOT_OK;

} /* numetrology_work */


/*
 *	numetrology
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
 *             void numetrology_getpar(void);
 * 	       void numetrology_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 24/11/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numetrology()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUMETROLOGY_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( numetrology_getpar() == OK) 
    {
      
      if ( numetrology_work()) 
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
  
} /* numetrology */


/*
 *	numetrology_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 24/11/11 - First version
 *
 */
void numetrology_info(void)
{ 
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");

  if(global.par.metflag){
    headas_chat(NORMAL,"Name of the input Metrology File                      :'%s'\n",global.par.metrologyfile);
    headas_chat(NORMAL,"Name of the input Metrology Grid File                 :'%s'\n",global.par.metgridfile);
    headas_chat(NORMAL,"Name of the output Position Sensing Detector File     :'%s'\n",global.par.outpsdfile);
    headas_chat(NORMAL,"Name of the output corrected Position Sensing Detector File :'%s'\n",global.par.outpsdfilecor);
    if(global.par.psdcal)
      headas_chat(NORMAL,"Apply PSD linearization to the corrected Position Sensing Detector File : yes\n");
    else
      headas_chat(NORMAL,"Apply PSD linearization to the corrected Position Sensing Detector File : no\n");
  }
  else{
    headas_chat(NORMAL,"Name of the input corrected Position Sensing Detector File  :'%s'\n",global.par.inpsdfilecor);
  }

  headas_chat(NORMAL,"Name of the input Alignment File                           :'%s'\n",global.par.alignfile);
  headas_chat(NORMAL,"Name of the output Mast Aspect Solution File               :'%s'\n",global.par.mastaspectfile);


  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                 : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                 : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                        : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                        : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");


} /* numetrology_info */


/*
 *	numetrology_checkinput
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
 *        0.1.0: - NS 24/11/11 - First version
 *
 */
int numetrology_checkinput(void)
{
  char     BaseName[MAXFNAME_LEN];
  char     ext[MAXEXT_LEN];


  /* If global.par.outpsdfile == DEFAULT build filename */ 
  if (!(strcasecmp (global.par.outpsdfile, DF_DEFAULT)))
    {
      
      SplitFilePath(global.par.metrologyfile, NULL, BaseName);
      strcpy(global.par.outpsdfile, BaseName);
      
      /* remove zip extension if needed */
      GetFilenameExtension(global.par.outpsdfile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "tgz")))
	StripExtension(global.par.outpsdfile);
      
      StripExtension(global.par.outpsdfile);
      strcat(global.par.outpsdfile, EXT_FITS_PSD);
      headas_chat(NORMAL, "%s: Info: Name for Position Sensing Detector file is:\n",global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outpsdfile);
    }
  
  if ( FileExists(global.par.outpsdfile) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outpsdfile);
    if ( !headas_clobpar ) {
      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outpsdfile);
      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
      goto check_end;
    }
    else
      { 
	headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outpsdfile);
	if(remove (global.par.outpsdfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outpsdfile);
	    goto check_end;
	  }
      }
  }
  

  /* If global.par.outpsdfilecor == DEFAULT build filename */ 
  if (!(strcasecmp (global.par.outpsdfilecor, DF_DEFAULT)))
    {
      SplitFilePath(global.par.metrologyfile, NULL, BaseName);
      strcpy(global.par.outpsdfilecor, BaseName);
      
      /* remove zip extension if needed */
      GetFilenameExtension(global.par.outpsdfilecor, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "tgz")))
	StripExtension(global.par.outpsdfilecor);
      
      StripExtension(global.par.outpsdfilecor);
      strcat(global.par.outpsdfilecor, EXT_FITS_PSDCORR);
      headas_chat(NORMAL, "%s: Info: Name for corrected Position Sensing Detector file is:\n",global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outpsdfilecor);
    }
  
  if ( FileExists(global.par.outpsdfilecor) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outpsdfilecor);
    if ( !headas_clobpar ) {
      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outpsdfilecor);
      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
      goto check_end;
    }
    else
      { 
	headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outpsdfilecor);
	if(remove (global.par.outpsdfilecor) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outpsdfilecor);
	    goto check_end;
	  }
      }
  }
  
  
  /* If global.par.mastaspectfile == DEFAULT build filename */ 
  if (!(strcasecmp (global.par.mastaspectfile, DF_DEFAULT)))
    {
      if(global.par.metflag){
	strcpy(global.par.mastaspectfile, global.par.metrologyfile);
      }
      else{
	strcpy(global.par.mastaspectfile, global.par.inpsdfilecor);
      }
      
      SplitFilePath(global.par.mastaspectfile, NULL, BaseName);
      strcpy(global.par.mastaspectfile, BaseName);
  
      /* remove zip extension if needed */
      GetFilenameExtension(global.par.mastaspectfile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "tgz")))
	StripExtension(global.par.mastaspectfile);
    
      StripExtension(global.par.mastaspectfile);
      strcat(global.par.mastaspectfile, EXT_FITS_MAST);
      headas_chat(NORMAL, "%s: Info: Name for Mast Aspect Solution file is:\n",global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.mastaspectfile);
    }
  
  if ( FileExists(global.par.mastaspectfile) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.mastaspectfile);
    if ( !headas_clobpar ) {
      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.mastaspectfile);
      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
      goto check_end;
    }
    else
      { 
	headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.mastaspectfile);
	if(remove (global.par.mastaspectfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.mastaspectfile);
	    goto check_end;
	  }
      }
  }
  
  
  if(global.par.metflag){
    strcpy(global.metinfofile, global.par.outpsdfilecor);
  }
  else{
    strcpy(global.metinfofile, global.par.inpsdfilecor);
  }


  return OK;

 check_end:

  return NOT_OK;

}


/*
 *
 *      ReadMetrologyFile
 *
 *	DESCRIPTION:
 *           Routine to read Metrology input file
 *            
 *            I : filename -> name of the input metrology file
 *            I : extname  -> name of the extension
 *            O : info     -> metrology data struct 
 *            I : initRow  -> first row to read (>=1)
 *            I/O: numRows -> on entry, <*numRows> specifies the number of rows to read;
 *			      on exit, it contains the actual number of lines read which may be less than the requested number
 *            O : endfile  -> 'OK' if end of file reached, 'NOT_OK' otherwise
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadMetrologyFile(char *filename, char *extname, MetrologyRow_t **info, const unsigned initRow, unsigned *numRows, BOOL *endfile){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, metcount=0, status=OK;
  MetrologyCol_t     col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Open readonly input metrology file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }
 
  /* Move in MET_RAW extension in input metrology file */
  if (fits_movnam_hdu(unit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadMetrologyFile_end;
    }
  
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);

  /* Get needed columns number from name */

  /* TIME */
  if ((col.TIME=ColNameMatch(CLNM_TIME, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* XA_PSD0 */
  if ((col.XA_PSD0=ColNameMatch(CLNM_XA_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XA_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* XB_PSD0 */
  if ((col.XB_PSD0=ColNameMatch(CLNM_XB_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XB_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* YA_PSD0 */
  if ((col.YA_PSD0=ColNameMatch(CLNM_YA_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YA_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* YB_PSD0 */
  if ((col.YB_PSD0=ColNameMatch(CLNM_YB_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YB_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* XA_PSD1 */
  if ((col.XA_PSD1=ColNameMatch(CLNM_XA_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XA_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* XB_PSD1 */
  if ((col.XB_PSD1=ColNameMatch(CLNM_XB_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XB_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* YA_PSD1 */
  if ((col.YA_PSD1=ColNameMatch(CLNM_YA_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YA_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* YB_PSD1 */
  if ((col.YB_PSD1=ColNameMatch(CLNM_YB_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YB_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  /* LAS_ON */
  if ((col.LAS_ON=ColNameMatch(CLNM_LAS_ON, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LAS_ON);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetrologyFile_end;
    }


  if(!strcmp(extname,KWVL_EXTNAME_MET_CMP))
    {
      /* CLAMP */
      if ((col.CLAMP=ColNameMatch(CLNM_CLAMP, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CLAMP);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	  goto ReadMetrologyFile_end;
	}
    }
  else
    {
      col.CLAMP=-1;
    }


  /* Init test */
/*   if(global.inpsdfile){ */
/*   /\* X_TMP0 *\/ */
/*   if ((col.X_TMP0=ColNameMatch("X_TMP0", &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "X_TMP0"); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadMetrologyFile_end; */
/*     } */

/*   /\* Y_TMP0 *\/ */
/*   if ((col.Y_TMP0=ColNameMatch("Y_TMP0", &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "Y_TMP0"); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadMetrologyFile_end; */
/*     } */
/*   /\* X_TMP1 *\/ */
/*   if ((col.X_TMP1=ColNameMatch("X_TMP1", &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "X_TMP1"); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadMetrologyFile_end; */
/*     } */

/*   /\* Y_TMP1 *\/ */
/*   if ((col.Y_TMP1=ColNameMatch("Y_TMP1", &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "Y_TMP1"); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadMetrologyFile_end; */
/*     } */
/*   } */
  /* End test */


  EndBintableHeader(&head, &table);


  if(!table.MaxRows){
    headas_chat(NORMAL, "%s: Warning: %s file (ext '%s') is empty.\n", global.taskname, filename, extname);
    *endfile = OK;
    *numRows = 0;
    goto ReadMetrologyFile_ok;
  }

  if (initRow > (unsigned)table.MaxRows) {
    *endfile = OK;
    *numRows = 0;
    goto ReadMetrologyFile_ok;
  }
  
  if (initRow + *numRows  >= (unsigned)table.MaxRows){
    *endfile = OK;
    *numRows = (unsigned)table.MaxRows - initRow + 1;
  }
  else{
    *endfile = NOT_OK;
  }
  
  
  /* Allocate memory to storage all requested data */
  metcount = (int)*numRows;
  *info = (MetrologyRow_t *)calloc(metcount, sizeof(MetrologyRow_t));
  if(*info==NULL){
    headas_chat(CHATTY,"%s: Error: ReadMetrologyFile: memory allocation failure.\n", global.taskname);
    goto ReadMetrologyFile_end;
  }


  /* Read Bintable */
  FromRow = initRow;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  while( (count<metcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows)==0 ) )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  if( FromRow==1 && n==0 && BVEC(table,n,col.LAS_ON)!=0 ){
	    headas_chat(NORMAL,"%s: Warning: 'LAS_ON' NOT equal to zero int the first row of file '%s'.\n", global.taskname, filename);
	  }

	  (*info)[count].time = DVEC(table,n,col.TIME);
	  (*info)[count].xa_psd0 = IVEC(table,n,col.XA_PSD0);
	  (*info)[count].xb_psd0 = IVEC(table,n,col.XB_PSD0);
	  (*info)[count].ya_psd0 = IVEC(table,n,col.YA_PSD0);
	  (*info)[count].yb_psd0 = IVEC(table,n,col.YB_PSD0);
	  (*info)[count].xa_psd1 = IVEC(table,n,col.XA_PSD1);
	  (*info)[count].xb_psd1 = IVEC(table,n,col.XB_PSD1);
	  (*info)[count].ya_psd1 = IVEC(table,n,col.YA_PSD1);
	  (*info)[count].yb_psd1 = IVEC(table,n,col.YB_PSD1);
	  (*info)[count].las_on  = BVEC(table,n,col.LAS_ON);

	  if(!strcmp(extname,KWVL_EXTNAME_MET_CMP))
	    (*info)[count].clamp  = BVEC(table,n,col.CLAMP);
	  else
	    (*info)[count].clamp  = 0;


	  /* Init test */
/* 	  if(global.inpsdfile){ */
/* 	    (*info)[count].x_tmp0 = EVEC(table,n,col.X_TMP0); */
/* 	    (*info)[count].y_tmp0 = EVEC(table,n,col.Y_TMP0); */
/* 	    (*info)[count].x_tmp1 = EVEC(table,n,col.X_TMP1); */
/* 	    (*info)[count].y_tmp1 = EVEC(table,n,col.Y_TMP1); */
/* 	  } */
	  /* End test */

	  count++;
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
   
  if(metcount != count){
    headas_chat(CHATTY,"%s: Error: ReadMetrologyFile: error reading file.\n", global.taskname);
    goto ReadMetrologyFile_end;
  }


  
 ReadMetrologyFile_ok:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadMetrologyFile_end;
    }

  return OK;

  
 ReadMetrologyFile_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;
 
}  /* ReadMetrologyFile */


/*
 *
 *      ReadPSDFile
 *
 *	DESCRIPTION:
 *           Routine to read Position Sensing Detector File 
 *            
 *            I : filename -> name of the input PSD file
 *            O : info     -> PSD data struct 
 *            I : initRow  -> first row to read (>=1)
 *            I/O: numRows -> on entry, <*numRows> specifies the number of rows to read;
 *			      on exit, it contains the actual number of lines read which may be less than the requested number
 *            O : endfile  -> 'OK' if end of file reached, 'NOT_OK' otherwise
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPSDFile(char *filename, MetrologyInfo_t **info, const unsigned initRow, unsigned *numRows, BOOL *endfile){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, psdcount=0, status=OK;
  PSDCol_t           col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open readonly input metrology file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPSDFile_end;
    }
 
  /* Move in PSDPOS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_PSDPOS, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_PSDPOS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadPSDFile_end;
    }
  
  head=RetrieveFitsHeader(unit);

  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadPSDFile_end;
    }


  /* Get needed columns number from name */

  /* TIME */
  if ((col.TIME=ColNameMatch(CLNM_TIME, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

  /* X_PSD0 */
  if ((col.X_PSD0=ColNameMatch(CLNM_X_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

  /* Y_PSD0 */
  if ((col.Y_PSD0=ColNameMatch(CLNM_Y_PSD0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_PSD0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

  /* X_PSD1 */
  if ((col.X_PSD1=ColNameMatch(CLNM_X_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

  /* Y_PSD1 */
  if ((col.Y_PSD1=ColNameMatch(CLNM_Y_PSD1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_PSD1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

/*   /\* X0_INT *\/ */
/*   if ((col.X0_INT=ColNameMatch(CLNM_X0_INT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X0_INT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadPSDFile_end; */
/*     } */

/*   /\* Y0_INT *\/ */
/*   if ((col.Y0_INT=ColNameMatch(CLNM_Y0_INT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y0_INT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadPSDFile_end; */
/*     } */

/*   /\* X1_INT *\/ */
/*   if ((col.X1_INT=ColNameMatch(CLNM_X1_INT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X1_INT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadPSDFile_end; */
/*     } */

/*   /\* Y1_INT *\/ */
/*   if ((col.Y1_INT=ColNameMatch(CLNM_Y1_INT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y1_INT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadPSDFile_end; */
/*     } */


  /* METGRID_FLAG */
  if ((col.METGRID_FLAG=ColNameMatch(CLNM_METGRID_FLAG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_METGRID_FLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPSDFile_end;
    }

  EndBintableHeader(&head, &table);


  if (initRow > (unsigned)table.MaxRows) {
    *endfile = OK;
    *numRows = 0;
    goto ReadPSDFile_ok;
  }
  
  if (initRow + *numRows  >= (unsigned)table.MaxRows){
    *endfile = OK;
    *numRows = (unsigned)table.MaxRows - initRow + 1;
  }
  else{
    *endfile = NOT_OK;
  }


  /* Allocate memory to storage all requested data */
  psdcount = (int)*numRows;
  *info = (MetrologyInfo_t *)calloc(psdcount, sizeof(MetrologyInfo_t));
  if(*info==NULL){
    headas_chat(CHATTY,"%s: Error: ReadPSDFile: memory allocation failure.\n", global.taskname);
    goto ReadPSDFile_end;
  }

  /* Read Bintable */
  FromRow = initRow;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  while( (count<psdcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows)==0 ) )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  (*info)[count].time = DVEC(table, n, col.TIME);
	  (*info)[count].x_psd0 = EVEC(table, n, col.X_PSD0);
	  (*info)[count].y_psd0 = EVEC(table, n, col.Y_PSD0);
	  (*info)[count].x_psd1 = EVEC(table, n, col.X_PSD1);
	  (*info)[count].y_psd1 = EVEC(table, n, col.Y_PSD1);
/* 	  (*info)[count].x0_int = EVEC(table, n, col.X0_INT); */
/* 	  (*info)[count].y0_int = EVEC(table, n, col.Y0_INT); */
/* 	  (*info)[count].x1_int = EVEC(table, n, col.X1_INT); */
/* 	  (*info)[count].y1_int = EVEC(table, n, col.Y1_INT); */
	  (*info)[count].metgrid_flag = BVEC(table, n, col.METGRID_FLAG);

	  count++;
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
   
  if(psdcount != count){
    headas_chat(CHATTY,"%s: Error: ReadPSDFile: error reading file.\n", global.taskname);
    goto ReadPSDFile_end;
  }


 ReadPSDFile_ok:

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadPSDFile_end;
    }

  return OK;

  
 ReadPSDFile_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadPSDFile */


int WritePSDFile(char *metfile, MetrologyKeys_t *metkeys, MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS], char *outfile, BOOL psdcal){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(metfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, metfile);
      goto WritePSDFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WritePSDFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, metfile);
      goto WritePSDFile_end;
    }
  
  /* copy primary header */
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WritePSDFile_end;
    }

  /* close input file */
  if ( fits_close_file(inunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, metfile);
      goto WritePSDFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WritePSDFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);
 
  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&head, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&head, KWNM_DATE, S, date, CARD_COMM_DATE);  
  
  /* Write header */
  if(WriteUpdatedHeader(outunit, &head))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WritePSDFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WritePSDFile_end;
      }
    }

  /* Create PSDPOS Ext  */
  if (WritePSDExt(metfile, outunit, metkeys, metgrid, psdcal))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_PSDPOS);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WritePSDFile_end;
    }
  hducount++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WritePSDFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WritePSDFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WritePSDFile_end;
    }


  return OK;
  
 WritePSDFile_end:

  return NOT_OK;


} /* WritePSDFile */


int WritePSDExt(char *metfile, FitsFileUnit_t ounit, MetrologyKeys_t *metkeys, MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS], BOOL psdcal){

  int             n, skiprow=0;
  unsigned        OutRows=0, metgrid_flag=0;
  PSDCol_t        indxcol;

  MetrologyRow_t  *metptr=NULL;
  MetrologyRow_t  *biasptr=NULL;

  MetrologyRow_t  *metinfo=NULL;
  MetrologyRow_t  bias;
  unsigned        metindex=1, metcount=MET_BLOCK_ROWS, count;
  BOOL            metend=NOT_OK;
  int             bias_set=0;
  unsigned        rawmet=0;

  MetrologyRow_t  *metcmpinfo=NULL;
  MetrologyRow_t  bias_cmp;
  unsigned        metcmpindex=1, metcmpcount=MET_BLOCK_ROWS, count2;
  BOOL            metcmpend=NOT_OK;
  int             bias_cmp_set=0;

  int             xa_0=0, xb_0=0, ya_0=0, yb_0=0;
  int             xa_1=0, xb_1=0, ya_1=0, yb_1=0;
  const float     fnan = log(-1);               /* float NaN value */
  float           x_psd0, y_psd0, x_psd1, y_psd1, x0_int, y0_int, x1_int, y1_int;
  float           x_psd_corr, y_psd_corr;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );


  /* Retrieve metrology RAW info from input metfile */
  if(ReadMetrologyFile(metfile, KWVL_EXTNAME_MET_RAW, &metinfo, metindex, &metcount, &metend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read metrology file.\n", global.taskname);
      goto WritePSDExt_end;
    }
  metindex += metcount;


  /* Retrieve metrology CMP info from input metfile */
  if(ReadMetrologyFile(metfile, KWVL_EXTNAME_MET_CMP, &metcmpinfo, metcmpindex, &metcmpcount, &metcmpend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read metrology file.\n", global.taskname);
      goto WritePSDExt_end;
    }
  metcmpindex += metcmpcount;


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Time of laser position measurement", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_PSD0, CARD_COMM_X_PSD0, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_PSD0, CARD_COMM_Y_PSD0, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_PSD1, CARD_COMM_X_PSD1, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_PSD1, CARD_COMM_Y_PSD1, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X0_INT, CARD_COMM_X0_INT, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y0_INT, CARD_COMM_Y0_INT, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X1_INT, CARD_COMM_X1_INT, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y1_INT, CARD_COMM_Y1_INT, "1E", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);  

  /* Case: PSD correction (metgrid!=NULL) */
  if(metgrid!=NULL){
    AddColumn(&newhead, &table, CLNM_METGRID_FLAG, CARD_COMM_METGRID_FLAG, "1B", TNONE); 
  }

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_PSDPOS, CARD_COMM_EXTNAME);
  AddCard(&newhead, KWNM_HDUCLASS, S, metkeys->hduclass, metkeys->hduclass_comm);
  AddCard(&newhead, KWNM_HDUCLAS1, S, metkeys->hduclas1, metkeys->hduclas1_comm);
  AddCard(&newhead, KWNM_TELESCOP, S, KWVL_TELESCOP, CARD_COMM_TELESCOP);
  AddCard(&newhead, KWNM_TIMEPIXR, E, &metkeys->timepixr, metkeys->timepixr_comm);
  AddCard(&newhead, KWNM_OBS_ID, S, metkeys->obs_id, metkeys->obs_id_comm);  
  AddCard(&newhead, KWNM_TARG_ID, J, &metkeys->targ_id, metkeys->targ_id_comm);
  AddCard(&newhead, KWNM_TIMESYS, S, metkeys->timesys, metkeys->timesys_comm); 
  AddCard(&newhead, KWNM_MJDREFI, J, &metkeys->mjdrefi, metkeys->mjdrefi_comm); 
  AddCard(&newhead, KWNM_MJDREFF, D, &metkeys->mjdreff, metkeys->mjdreff_comm);
  AddCard(&newhead, KWNM_CLOCKAPP, L, &metkeys->clockapp, metkeys->clockapp_comm);
  AddComment(&newhead,"MJDREFI + MJDREFF is the epoch January 1.0, 2010, in the TT time system.");
  AddCard(&newhead, KWNM_TIMEUNIT, S, metkeys->timeunit, metkeys->timeunit_comm);
  AddCard(&newhead, KWNM_TSTART, D, &metkeys->tstart, metkeys->tstart_comm);
  AddCard(&newhead, KWNM_TSTOP, D, &metkeys->tstop, metkeys->tstop_comm);
  AddCard(&newhead, KWNM_DATEOBS, S, metkeys->dateobs, metkeys->dateobs_comm);
  AddCard(&newhead, KWNM_DATEEND, S, metkeys->dateend, metkeys->dateend_comm);


  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&newhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE); 


  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);

  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);


  GetBintableStructure(&newhead, &table, BINTAB_ROWS, 0, NULL);


  /* Get columns index from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto WritePSDExt_end;
    }

  if ((indxcol.X_PSD0 = GetColNameIndx(&table, CLNM_X_PSD0)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_PSD0);
      goto WritePSDExt_end;
    }

  if ((indxcol.Y_PSD0 = GetColNameIndx(&table, CLNM_Y_PSD0)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_PSD0);
      goto WritePSDExt_end;
    }

  if ((indxcol.X_PSD1 = GetColNameIndx(&table, CLNM_X_PSD1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_PSD1);
      goto WritePSDExt_end;
    }

  if ((indxcol.Y_PSD1 = GetColNameIndx(&table, CLNM_Y_PSD1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_PSD1);
      goto WritePSDExt_end;
    }

  if ((indxcol.X0_INT=ColNameMatch(CLNM_X0_INT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X0_INT);
      goto WritePSDExt_end;
    }

  if ((indxcol.Y0_INT=ColNameMatch(CLNM_Y0_INT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y0_INT);
      goto WritePSDExt_end;
    }

  if ((indxcol.X1_INT=ColNameMatch(CLNM_X1_INT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X1_INT);
      goto WritePSDExt_end;
    }

  if ((indxcol.Y1_INT=ColNameMatch(CLNM_Y1_INT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y1_INT);
      goto WritePSDExt_end;
    }

  /* Case: PSD correction (metgrid!=NULL) */
  if(metgrid!=NULL){
    if ((indxcol.METGRID_FLAG=ColNameMatch(CLNM_METGRID_FLAG, &table)) == -1)
      {
	headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_METGRID_FLAG);
	goto WritePSDExt_end;
      }
  }
  else
    indxcol.METGRID_FLAG = -1;


  OutRows = 0;
  count = 0;
  count2= 0;
  n = 0;
  biasptr = &bias;

  while( count<metcount || count2<metcmpcount )
    {   

      if(count2>=metcmpcount){

	/* Read RAW metrology row */
	metptr = &metinfo[count];
	rawmet = 1;
	count++;

	goto select_row_end;
      }
      
      if(count>=metcount){

	/* Read CMP metrology row */
	metptr = &metcmpinfo[count2];
	rawmet = 0;
	count2++;

	goto select_row_end;
      }
      
      if(metinfo[count].time<metcmpinfo[count2].time-TIME_SENS){

	/* Read RAW metrology row */
	metptr = &metinfo[count];
	rawmet = 1;
	count++;

	goto select_row_end;
      }

      if(metinfo[count].time>=metcmpinfo[count2].time-TIME_SENS && metinfo[count].time<=metcmpinfo[count2].time+TIME_SENS ){

	/* Read RAW metrology row */
	metptr = &metinfo[count];
	rawmet = 1;
	count++;

	/* Exclude the CMP row with the same time (bias need also to be stored) */
	if( metcmpinfo[count2].las_on == 0 ){
	  bias_cmp.xa_psd0 = metcmpinfo[count2].xa_psd0;
	  bias_cmp.xb_psd0 = metcmpinfo[count2].xb_psd0;
	  bias_cmp.ya_psd0 = metcmpinfo[count2].ya_psd0;  
	  bias_cmp.yb_psd0 = metcmpinfo[count2].yb_psd0;
	  bias_cmp.xa_psd1 = metcmpinfo[count2].xa_psd1;	  
	  bias_cmp.xb_psd1 = metcmpinfo[count2].xb_psd1;
	  bias_cmp.ya_psd1 = metcmpinfo[count2].ya_psd1;
	  bias_cmp.yb_psd1 = metcmpinfo[count2].yb_psd1;
	  bias_cmp_set=1;
	}
	count2++;

	goto select_row_end;
      }

      if(metinfo[count].time>metcmpinfo[count2].time+TIME_SENS ){
	/* Read CMP metrology row */
	metptr = &metcmpinfo[count2];
	rawmet = 0;
	count2++;

	goto select_row_end;
      }


    select_row_end:


      /* Exclude rows with CLAMP!=0 */
      if( metptr->clamp != 0 )
	continue;


      if( metptr->las_on == 0 ){

	if(rawmet>0){

	  bias.xa_psd0 = metptr->xa_psd0;
	  bias.xb_psd0 = metptr->xb_psd0;
	  bias.ya_psd0 = metptr->ya_psd0;  
	  bias.yb_psd0 = metptr->yb_psd0;
	  bias.xa_psd1 = metptr->xa_psd1;	  
	  bias.xb_psd1 = metptr->xb_psd1;
	  bias.ya_psd1 = metptr->ya_psd1;
	  bias.yb_psd1 = metptr->yb_psd1;
	  bias_set=1;
 
	  biasptr = &bias;
	}
	else{

	  bias_cmp.xa_psd0 = metptr->xa_psd0;
	  bias_cmp.xb_psd0 = metptr->xb_psd0;
	  bias_cmp.ya_psd0 = metptr->ya_psd0;  
	  bias_cmp.yb_psd0 = metptr->yb_psd0;
	  bias_cmp.xa_psd1 = metptr->xa_psd1;	  
	  bias_cmp.xb_psd1 = metptr->xb_psd1;
	  bias_cmp.ya_psd1 = metptr->ya_psd1;
	  bias_cmp.yb_psd1 = metptr->yb_psd1;
	  bias_cmp_set=1;
 
	  biasptr = &bias_cmp;
	}

      }
      else{

	skiprow=0;

	xa_0 = metptr->xa_psd0 - biasptr->xa_psd0;
	xb_0 = metptr->xb_psd0 - biasptr->xb_psd0;
	ya_0 = metptr->ya_psd0 - biasptr->ya_psd0;
	yb_0 = metptr->yb_psd0 - biasptr->yb_psd0;
	xa_1 = metptr->xa_psd1 - biasptr->xa_psd1;
	xb_1 = metptr->xb_psd1 - biasptr->xb_psd1;
	ya_1 = metptr->ya_psd1 - biasptr->ya_psd1; 
	yb_1 = metptr->yb_psd1 - biasptr->yb_psd1;
	
	x_psd0 = X_PSD0_SCALE *(xb_0-xa_0)/(xa_0+xb_0) ;
	y_psd0 = Y_PSD0_SCALE *(yb_0-ya_0)/(ya_0+yb_0) ;
	x_psd1 = X_PSD1_SCALE *(xb_1-xa_1)/(xa_1+xb_1) ;
	y_psd1 = Y_PSD1_SCALE *(yb_1-ya_1)/(ya_1+yb_1) ;
	
	x0_int = xa_0 + xb_0;
	y0_int = ya_0 + yb_0;
	x1_int = xa_1 + xb_1;
	y1_int = ya_1 + yb_1;

	
	/* Apply PSD correction if requested (metgrid!=NULL) */
	if(metgrid!=NULL){

	  if(psdcal){

	    if(isnan(x_psd0)||isnan(x_psd1)||isnan(y_psd0)||isnan(y_psd1) ){
	      headas_chat(NORMAL,"%s: Warning: unable to calculate position for TIME=%f (PSDCORR)\n", global.taskname, metptr->time);
	      skiprow=1;
	    }
	    else{

	      if( CorrectPosition(metgrid[0], x_psd0, y_psd0, &x_psd_corr, &y_psd_corr) ){
		headas_chat(NORMAL,"%s: Error: unable to correct position for X_PSD0=%f Y_PSD0=%f\n", global.taskname, x_psd0, y_psd0);
		goto WritePSDExt_end;
	      }
	      x_psd0 = x_psd_corr;
	      y_psd0 = y_psd_corr;
	    
	  
	      if( CorrectPosition(metgrid[1], x_psd1, y_psd1, &x_psd_corr, &y_psd_corr) ){
		headas_chat(NORMAL,"%s: Error: unable to correct position for X_PSD1=%f Y_PSD1=%f\n", global.taskname, x_psd1, y_psd1);
		goto WritePSDExt_end;
	      }
	      x_psd1 = x_psd_corr;
	      y_psd1 = y_psd_corr;
	    
	      if(isnan(x_psd0)||isnan(x_psd1)||isnan(y_psd0)||isnan(y_psd1) ){
		headas_chat(NORMAL,"%s: Warning: unable to correct position for TIME=%f\n", global.taskname, metptr->time);
		metgrid_flag = 1;
	      }
	      else{
		metgrid_flag = 0;
	      }
	    }

	  }
	  else{

	    if(isnan(x_psd0)||isnan(x_psd1)||isnan(y_psd0)||isnan(y_psd1) ){
	      headas_chat(NORMAL,"%s: Warning: unable to calculate position for TIME=%f (PSDCORR)\n", global.taskname, metptr->time);
	      skiprow=1;
	    }

	    metgrid_flag = 2;
	  }

	}

	if( (rawmet>0&&bias_set>0) || (rawmet==0&&bias_cmp_set>0) ){

	  if( (metgrid==NULL) && (isnan(x_psd0)||isnan(x_psd1)||isnan(y_psd0)||isnan(y_psd1) ) ){
	    headas_chat(NORMAL,"%s: Warning: unable to calculate position for TIME=%f (PSD)\n", global.taskname, metptr->time);
	    skiprow=1;
	  }
	  else{
	    DVEC(table, n, indxcol.TIME)=metptr->time;
	    EVEC(table, n, indxcol.X_PSD0)=x_psd0;
	    EVEC(table, n, indxcol.Y_PSD0)=y_psd0;
	    EVEC(table, n, indxcol.X_PSD1)=x_psd1;
	    EVEC(table, n, indxcol.Y_PSD1)=y_psd1;
	    EVEC(table, n, indxcol.X0_INT)=x0_int;
	    EVEC(table, n, indxcol.Y0_INT)=y0_int;
	    EVEC(table, n, indxcol.X1_INT)=x1_int;
	    EVEC(table, n, indxcol.Y1_INT)=y1_int;

	    /* Case: PSD correction (metgrid!=NULL) */
	    if(metgrid!=NULL)
	      BVEC(table, n, indxcol.METGRID_FLAG) = metgrid_flag;

	  }
	}
	else{
	  DVEC(table, n, indxcol.TIME)=metptr->time;
	  EVEC(table, n, indxcol.X_PSD0) = fnan;
	  EVEC(table, n, indxcol.Y_PSD0) = fnan;
	  EVEC(table, n, indxcol.X_PSD1) = fnan;
	  EVEC(table, n, indxcol.Y_PSD1) = fnan;
	  EVEC(table, n, indxcol.X0_INT) = fnan;
	  EVEC(table, n, indxcol.Y0_INT) = fnan;
	  EVEC(table, n, indxcol.X1_INT) = fnan;
	  EVEC(table, n, indxcol.Y1_INT) = fnan;

	  /* Case: PSD correction (metgrid!=NULL) */
	  if(metgrid!=NULL)
	    BVEC(table, n, indxcol.METGRID_FLAG) = metgrid_flag;

	}

	if(!skiprow)
	  {
	    OutRows++;
	    n++;
	  }

	if(OutRows>=BINTAB_ROWS)
	  {
	    WriteFastBintable(ounit, &table, OutRows, FALSE);
	    OutRows=0;
	    n=0;
	  }

      }


      if((count==metcount)&&(metend!=OK)){
	free(metinfo);
	if(ReadMetrologyFile(metfile, KWVL_EXTNAME_MET_RAW, &metinfo, metindex, &metcount, &metend))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to read metrology file.\n", global.taskname);
	    goto WritePSDExt_end;
	  }
	metindex += metcount;
	count = 0;
      }
      
      if((count2==metcmpcount)&&(metcmpend!=OK)){
	free(metcmpinfo);
	if(ReadMetrologyFile(metfile, KWVL_EXTNAME_MET_CMP, &metcmpinfo, metcmpindex, &metcmpcount, &metcmpend))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to read metrology file.\n", global.taskname);
	    goto WritePSDExt_end;
	  }
	metcmpindex += metcmpcount;
	count2 = 0;
      }
      
    }
  
  WriteFastBintable(ounit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);


  return OK;
  
 WritePSDExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WritePSDExt */


/**********************************************************************************************************/

int WriteMastAspectFile(char *psdfile, MetrologyKeys_t *metkeys, AlignInfo_t *aligninfo, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(psdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, psdfile);
      goto WriteMastAspectFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, psdfile);
      goto WriteMastAspectFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);
 
  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&head, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&head, KWNM_DATE, S, date, CARD_COMM_DATE);  
  
  /* Write header */
  if(WriteUpdatedHeader(outunit, &head))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteMastAspectFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteMastAspectFile_end;
      }
    }

  /* Create Mast Aspect Ext  */
  if (WriteMastAspectExt(psdfile, outunit, metkeys, aligninfo))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_PSDPOS);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }
  hducount++;

  /* Add history to Mast Aspect Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteMastAspectFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteMastAspectFile_end;
    }


  return OK;
  
 WriteMastAspectFile_end:

  return NOT_OK;


} /* WriteMastAspectFile */


int WriteMastAspectExt(char *psdfile, FitsFileUnit_t ounit, MetrologyKeys_t *metkeys, AlignInfo_t *aligninfo){

  int             n;
  MastExtCol_t    indxcol;
  MetrologyInfo_t *psdinfo=NULL;
  unsigned        psdindex=1, psdcount=PSD_BLOCK_ROWS, count=0, OutRows=0; 
  BOOL            psdend=NOT_OK;
  MastInfo_t      mastinfo;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  

  /* Retrieve PSD info from input psdfile */
  if(ReadPSDFile(psdfile, &psdinfo, psdindex, &psdcount, &psdend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read PSD file.\n", global.taskname);
      goto WriteMastAspectExt_end;
    }
  psdindex += psdcount;


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Time of the mast aspect solution", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_T_FBOB, CARD_COMM_T_FBOB, "3D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Q_FBOB, CARD_COMM_Q_FBOB, "4D", TNONE);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_MAST_ASPECT, CARD_COMM_EXTNAME);
  AddCard(&newhead, KWNM_HDUCLASS, S, metkeys->hduclass, metkeys->hduclass_comm);
  AddCard(&newhead, KWNM_HDUCLAS1, S, metkeys->hduclas1, metkeys->hduclas1_comm);
  AddCard(&newhead, KWNM_TELESCOP, S, KWVL_TELESCOP, CARD_COMM_TELESCOP);
  AddCard(&newhead, KWNM_TIMEPIXR, E, &metkeys->timepixr, metkeys->timepixr_comm);
  AddCard(&newhead, KWNM_OBS_ID, S, metkeys->obs_id, metkeys->obs_id_comm);  
  AddCard(&newhead, KWNM_TARG_ID, J, &metkeys->targ_id, metkeys->targ_id_comm);
  AddCard(&newhead, KWNM_TIMESYS, S, metkeys->timesys, metkeys->timesys_comm); 
  AddCard(&newhead, KWNM_MJDREFI, J, &metkeys->mjdrefi, metkeys->mjdrefi_comm); 
  AddCard(&newhead, KWNM_MJDREFF, D, &metkeys->mjdreff, metkeys->mjdreff_comm);
  AddCard(&newhead, KWNM_CLOCKAPP, L, &metkeys->clockapp, metkeys->clockapp_comm);
  AddComment(&newhead,"MJDREFI + MJDREFF is the epoch January 1.0, 2010, in the TT time system.");
  AddCard(&newhead, KWNM_TIMEUNIT, S, metkeys->timeunit, metkeys->timeunit_comm);
  AddCard(&newhead, KWNM_TSTART, D, &metkeys->tstart, metkeys->tstart_comm);
  AddCard(&newhead, KWNM_TSTOP, D, &metkeys->tstop, metkeys->tstop_comm);
  AddCard(&newhead, KWNM_DATEOBS, S, metkeys->dateobs, metkeys->dateobs_comm);
  AddCard(&newhead, KWNM_DATEEND, S, metkeys->dateend, metkeys->dateend_comm);


  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&newhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE); 

  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);

  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);


  GetBintableStructure(&newhead, &table, BINTAB_ROWS, 0, NULL);


  /* Get columns index from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto WriteMastAspectExt_end;
    }

  if ((indxcol.T_FBOB = GetColNameIndx(&table, CLNM_T_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_T_FBOB);
      goto WriteMastAspectExt_end;
    }

  if ((indxcol.Q_FBOB = GetColNameIndx(&table, CLNM_Q_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Q_FBOB);
      goto WriteMastAspectExt_end;
    }


  OutRows = 0;
  count = 0;
  n = 0;

  while(count<psdcount){

    if( psdinfo[count].metgrid_flag==0 || psdinfo[count].metgrid_flag==2 )
      {
	if( ComputeMastAspectSolution(psdinfo[count], aligninfo, &mastinfo) )
	  goto WriteMastAspectExt_end;
	
	DVEC(table, n, indxcol.TIME)=mastinfo.time;
	DVECVEC_ARRAY_WRITE(mastinfo.Tfbob, 3, table, n, indxcol.T_FBOB);
	DVECVEC_ARRAY_WRITE(mastinfo.Qfbob, 4, table, n, indxcol.Q_FBOB);
	n++;
	
	if(++OutRows>=BINTAB_ROWS)
	  {
	    WriteFastBintable(ounit, &table, OutRows, FALSE);
	    OutRows=0;
	    n=0;
	  }
      }

    if(count==(psdcount-1)&&(psdend!=OK)){
      free(psdinfo);
      if(ReadPSDFile(psdfile, &psdinfo, psdindex, &psdcount, &psdend))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read PSD file.\n", global.taskname);
	  goto WriteMastAspectExt_end;
	}
      psdindex += psdcount;
      count = 0;
    }
    else{
      count++;
    }

  }

  
  WriteFastBintable(ounit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);


  return OK;
  
 WriteMastAspectExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteMastAspectExt */

/*********************************************************************************************************/


int ReadMetGridFile(MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS]){

  char        file0[PIL_LINESIZE], file1[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1;


  /* Derive CALDB filename */  
  if ( !strcasecmp(global.par.metgridfile,DF_CALDB) )
    {
      /* Retrieve PSD0 metgrid filename */
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_METROLOGY_DSET, file0, "PSD.eq.0", &extfile0, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for metgridfile parameter.\n", global.taskname);
      	  goto ReadMetGridFile_end;
      	}
      extfile0++;

      /* Retrieve PSD1 metgrid filename */
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_METROLOGY_DSET, file1, "PSD.eq.1", &extfile1, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for metgridfile parameter.\n", global.taskname);
      	  goto ReadMetGridFile_end;
      	}
      extfile1++;
    }
  else{
    strcpy(file0, global.par.metgridfile);
    strcpy(file1, global.par.metgridfile);
  }

  /* Retrieve PSD0 metgrid info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for metgrid data of PSD0.\n", global.taskname, file0);
  if( ReadMetGridInfo(metgrid[0], file0, extfile0, KWVL_PSDID_PSD0) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read metrology grid data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input metgrid file: %s.\n", global.taskname, file0);
      goto ReadMetGridFile_end;
    }

  /* Retrieve PSD1 metgrid info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for metgrid data of PSD1.\n", global.taskname, file1);
  if( ReadMetGridInfo(metgrid[1], file1, extfile1, KWVL_PSDID_PSD1) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read metrology grid data\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input metgrid file: %s.\n", global.taskname, file1);
      goto ReadMetGridFile_end;
    }


  return OK;
  
 ReadMetGridFile_end:
  
  return NOT_OK;

} /* ReadMetGridFile */



int ReadMetGridInfo(MetGridInfo_t metgrid[PSD_ROWS][PSD_PIXS], char *filename, long extno, char *psdid){

  int                n, i=0, j=0, count=0, status=OK, found=NOT_OK;
  int                inExt, totExt;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_psdid[FLEN_VALUE];
  MetGridCol_t       indxcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     inunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only input file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(inunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadMetGridInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input pixposfile */
      if (fits_get_num_hdus(inunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadMetGridInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
      /* Move to METROLOGY extension with PSDID=<psdid> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadMetGridInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(inunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_PSDID, &card))
	    strcpy(r_psdid, card->u.SVal);
	  else
	    strcpy(r_psdid, "-");
	  
	  if( !strcmp(r_extname,KWVL_EXTNAME_METROLOGY) && !strcmp(r_psdid,psdid) ){
	    found=OK;
	  }
	  
	  inExt++;
	}
  
      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with PSDID='%s'\n", global.taskname,KWVL_EXTNAME_METROLOGY,psdid);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename);
	  goto ReadMetGridInfo_end;
	}
    }


  head = RetrieveFitsHeader(inunit);
  
  /* Read metrology bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  

  if(table.MaxRows != (PSD_PIXS*PSD_ROWS))
    {
      headas_chat(NORMAL, "%s: Error: bad number of rows in %s file (%d rows expected!).\n", global.taskname, filename, (PSD_PIXS*PSD_ROWS));
      goto ReadMetGridInfo_end;
    }


  /* Get columns index from name */

  if ((indxcol.X_STAGE = GetColNameIndx(&table, CLNM_X_STAGE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_STAGE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if ((indxcol.Y_STAGE = GetColNameIndx(&table, CLNM_Y_STAGE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_STAGE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if ((indxcol.X_PSD = GetColNameIndx(&table, CLNM_X_PSD)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_PSD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if ((indxcol.Y_PSD = GetColNameIndx(&table, CLNM_Y_PSD)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_PSD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if ((indxcol.DELTAX = GetColNameIndx(&table, CLNM_DELTAX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DELTAX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }

  if ((indxcol.DELTAY = GetColNameIndx(&table, CLNM_DELTAY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DELTAY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadMetGridInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(inunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  i = count / PSD_PIXS;
	  j = count % PSD_PIXS;
	  count++;

	  metgrid[i][j].x_stage = EVEC(table, n, indxcol.X_STAGE);
	  metgrid[i][j].y_stage = EVEC(table, n, indxcol.Y_STAGE);
	  metgrid[i][j].x_psd = EVEC(table, n, indxcol.X_PSD);	  
	  metgrid[i][j].y_psd = EVEC(table, n, indxcol.Y_PSD);
	  metgrid[i][j].deltax = EVEC(table, n, indxcol.DELTAX);	  
	  metgrid[i][j].deltay = EVEC(table, n, indxcol.DELTAY);

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadMetGridInfo_end;
    }


  return OK;
  
 ReadMetGridInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadMetGridInfo */


/*
  FindNearestPoint

     | ----------|
     |     |     |
     |  0  |  1  |
     |     |     |
     | ----------|
     |     |     |
     |  2  |  3  |
     |     |     |
     | ----------|

*/
void FindNearestPoint(MetGridInfo_t metgrid[PSD_ROWS][PSD_PIXS], float xin, float yin, int *ix, int *iy){

  int     i=0, j=0, found=0;
  double  dist=0.0, mindist=0.0;
  int     xmin, xmax, ymin, ymax, np;
  int     xlen, ylen, dx, dy;
  int     m=2;
  PointInfo_t  points[4];

  xmin = 0;
  xmax = PSD_PIXS-1;
  ymin = 0;
  ymax = PSD_ROWS-1;

  /* Split the 'metgrid' matrix in 4 submatrix (recursive) */
  for(j=0; j<2; j++){

    xlen = xmax-xmin;
    ylen = ymax-ymin;

    points[0].ix = xmin + xlen*0.25;
    points[0].iy = ymin + ylen*0.25;
    points[0].dist = pow((xin-metgrid[ (points[0].iy) ][ (points[0].ix) ].x_psd),2) + pow((yin-metgrid[ (points[0].iy) ][ (points[0].ix) ].y_psd),2);
    
    points[1].ix = xmin + xlen*0.75;
    points[1].iy = ymin + ylen*0.25;
    points[1].dist = pow((xin-metgrid[ (points[1].iy) ][ (points[1].ix) ].x_psd),2) + pow((yin-metgrid[ (points[1].iy) ][ (points[1].ix) ].y_psd),2);
    
    points[2].ix = xmin + xlen*0.25;
    points[2].iy = ymin + ylen*0.75;
    points[2].dist = pow((xin-metgrid[ (points[2].iy) ][ (points[2].ix) ].x_psd),2) + pow((yin-metgrid[ (points[2].iy) ][ (points[2].ix) ].y_psd),2);
    
    points[3].ix = xmin + xlen*0.75;
    points[3].iy = ymin + ylen*0.75;
    points[3].dist = pow((xin-metgrid[ (points[3].iy) ][ (points[3].ix) ].x_psd),2) + pow((yin-metgrid[ (points[3].iy) ][ (points[3].ix) ].y_psd),2);
    
    np = 0;
    for(i=0; i<4; i++){
      if(points[i].dist<points[np].dist)
	np=i;
    }
    
    dx = xlen*0.25;
    dy = ylen*0.25;
    xmin = MAX(0,(points[np].ix - dx - m));
    xmax = MIN((points[np].ix + dx + m),(PSD_PIXS-1));
    ymin = MAX(0,(points[np].iy - dy - m));
    ymax = MIN((points[np].iy + dy + m),(PSD_ROWS-1));

  }

  /* Compute distance from every pixel in selected submatrix */
  for(i=ymin; i<=ymax; i++){
    for(j=xmin; j<=xmax; j++){
      dist = pow((xin-metgrid[i][j].x_psd),2) + pow((yin-metgrid[i][j].y_psd),2);
      if( dist<mindist || !found){
	mindist = dist;
	*iy = i;
	*ix = j;
	found=1;
      }
    }
  }

} /* FindNearestPoint */

/*
 *
 *      BilinearInterpolation
 *
 *	DESCRIPTION:
 *           Routine to perform bilinear interpolation. 
 *           The function takes as input:
 *             - the coordinates (xin,yin) of the input point where to compute the interpolation
 *             - the coordinates (x1,y1), (x2,y2),(x3,y3) and (x4,y4) of the points surrounding (xin,yin)
 *               with the following schema:
 *
 *                  (x4,y4)      .       (x3,y3)
 *                               .
 *                        ...(xin,yin)...
 *                               .
 *                  (x1,y1)      .       (x2,y2)
 *        
 *
 *             - the v1, v2, v3 and v4 values to be interpolated in the (xin,yin) point
 *
 *
 *           The function returns the interpolated value
 *
 */
float BilinearInterpolation(float xin, float yin, float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4, float v1, float v2, float v3, float v4){

  float R1, R2, dy_avg, y34_avg, y12_avg, result=0;

  R1 = ( ((x2-xin)/(x2-x1))*v1 ) + ( ((xin-x1)/(x2-x1))*v2 );
  /* R2 = ( ((x2-xin)/(x2-x1))*v4 ) + ( ((xin-x1)/(x2-x1))*v3 ); */
  R2 = ( ((x3-xin)/(x3-x4))*v4 ) + ( ((xin-x4)/(x3-x4))*v3 );

  dy_avg  = 0.5 * ( (y3-y2)+(y4-y1) );
  y34_avg = 0.5 * (y3+y4);
  y12_avg = 0.5 * (y1+y2);

  result = ((y34_avg-yin)/dy_avg)*R1 + ((yin-y12_avg)/dy_avg)*R2;

  return result;

} /* BilinearInterpolation */


int CorrectPosition(MetGridInfo_t metgrid[PSD_ROWS][PSD_PIXS], float x, float y, float *xcorr, float *ycorr){

  int           ix=0, iy=0;
  const float   fnan = log(-1);
  float         dx, dy;
  int           ixmin, ixmax, iymin, iymax;
/*   float         R1, R2; */
  float         deltax, deltay;
  float         x1,y1,x2,y2,x3,y3,x4,y4,v1,v2,v3,v4;


  if(isnan(x)||isnan(y)){
    *xcorr = fnan;
    *ycorr = fnan;
    return OK;
  }

  FindNearestPoint(metgrid, x, y, &ix, &iy);

  /* headas_chat(CHATTY,"X=%f Y=%f X_NEAR=%f Y_NEAR=%f (X_INDX=%d Y_INDX=%d)\n",x,y,metgrid[iy][ix].x_psd,metgrid[iy][ix].y_psd,ix,iy); */

  dx = x - metgrid[iy][ix].x_psd;
  dy = y - metgrid[iy][ix].y_psd;

  if(dx>=0){
    ixmin = ix;
    ixmax = MIN(ix+1,PSD_PIXS-1);
  }
  else{
    ixmin = MAX(ix-1,0);
    ixmax = ix;
  }

  if(dy<0){
    iymin = iy;
    iymax = MIN(iy+1,PSD_ROWS-1);
  }
  else{
    iymin = MAX(iy-1,0);
    iymax = iy;
  }

  /* Case: edge of the grid in X */
  if(ixmin==ixmax){

    *xcorr = fnan;
    *ycorr = fnan;
    return OK;

/*     R1 = metgrid[iymin][ixmin].deltax; */
/*     R2 = metgrid[iymax][ixmin].deltax; */
/*     deltax = 0.5*(R1+R2); */

/*     R1 = metgrid[iymin][ixmin].deltay; */
/*     R2 = metgrid[iymax][ixmin].deltay; */
/*     deltay = 0.5*(R1+R2); */

    /* headas_chat(CHATTY,"(x1=%d y1=%d) (x2=%d y2=%d)\n",ixmin,iymin,ixmin,iymax); */
    /* headas_chat(CHATTY,"x1=%f y1=%f x2=%f y2=%f\n",metgrid[iymin][ixmin].x_psd,metgrid[iymin][ixmin].y_psd,metgrid[iymax][ixmin].x_psd,metgrid[iymax][ixmin].y_psd); */
  }
  /* Case: edge of the grid in Y */
  else if(iymin==iymax){

    *xcorr = fnan;
    *ycorr = fnan;
    return OK;

/*     R1 = metgrid[iymin][ixmin].deltax; */
/*     R2 = metgrid[iymin][ixmax].deltax; */
/*     deltax = 0.5*(R1+R2); */

/*     R1 = metgrid[iymin][ixmin].deltay; */
/*     R2 = metgrid[iymin][ixmax].deltay; */
/*     deltay = 0.5*(R1+R2); */  
    
    /* headas_chat(CHATTY,"(x1=%d y1=%d) (x2=%d y2=%d)\n",ixmin,iymin,ixmax,iymin); */
    /* headas_chat(CHATTY,"x1=%f y1=%f x2=%f y2=%f\n",metgrid[iymin][ixmin].x_psd,metgrid[iymin][ixmin].y_psd,metgrid[iymin][ixmax].x_psd,metgrid[iymin][ixmax].y_psd); */
  
  }
  else{
    x1 = metgrid[iymin][ixmin].x_psd;
    y1 = metgrid[iymin][ixmin].y_psd;

    x2 = metgrid[iymin][ixmax].x_psd;
    y2 = metgrid[iymin][ixmax].y_psd;

    x3 = metgrid[iymax][ixmax].x_psd; 
    y3 = metgrid[iymax][ixmax].y_psd;

    x4 = metgrid[iymax][ixmin].x_psd;
    y4 = metgrid[iymax][ixmin].y_psd;

    /* headas_chat(CHATTY,"x1=%f y1=%f x2=%f y2=%f x3=%f y3=%f x4=%f y4=%f\n",x1,y1,x2,y2,x3,y3,x4,y4); */

    v1 = metgrid[iymin][ixmin].deltax;
    v2 = metgrid[iymin][ixmax].deltax;
    v3 = metgrid[iymax][ixmax].deltax;
    v4 = metgrid[iymax][ixmin].deltax;

    deltax = BilinearInterpolation(x,y,x1,y1,x2,y2,x3,y3,x4,y4,v1,v2,v3,v4);
    
    v1 = metgrid[iymin][ixmin].deltay;
    v2 = metgrid[iymin][ixmax].deltay;
    v3 = metgrid[iymax][ixmax].deltay;
    v4 = metgrid[iymax][ixmin].deltay;

    deltay = BilinearInterpolation(x,y,x1,y1,x2,y2,x3,y3,x4,y4,v1,v2,v3,v4);
  }

  *xcorr = x+deltax;
  *ycorr = y+deltay;

  return OK;

} /* CorrectPosition */


int ReadAlignFile(AlignInfo_t *aligninfo){

  long         extfile=-1;

  /* Derive CALDB alignment filename */  
  if ( !strcasecmp(global.par.alignfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_ALIGN_DSET, global.par.alignfile, "type.eq.systems", &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for alignfile parameter.\n", global.taskname);
      	  goto ReadAlignFile_end;
      	}
      extfile++;
    }

  /* Retrieve alignment info */
  if( ReadAlignInfo(global.par.alignfile, aligninfo) )
    {
      headas_chat(NORMAL, " %s: Error: unable to read alignment file.\n", global.taskname);
      goto ReadAlignFile_end;
    }


  return OK;
  
 ReadAlignFile_end:
  return NOT_OK;

} /* ReadAlignFile */


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
 *      ComputeMastAspectSolution
 *
 *	DESCRIPTION:
 *            MetrologyInfo_t met,      ->  IN: Metrology data
 *            AlignInfo_t *aligninfo,   ->  IN: Alignment data
 *            MastInfo_t *mast          -> OUT: Mast aspect solution data
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeMastAspectSolution(MetrologyInfo_t met, AlignInfo_t *aligninfo, MastInfo_t *mast){

  /* Step 1: input */
  AtQuat Qfbmd0, Qfbmd1;
  AtVect Tfbmd0, Tfbmd1;
  AtVect P0md, P1md;
  /* Step 1: output */
  AtVect P0fb, P1fb;

  /* Step 2: input */
  AtQuat Qobml0, Qobml1;
  AtVect Tobml0, Tobml1;
  AtVect L0ml, L1ml;
  /* Step 2: output */
  AtVect L0ob, L1ob;

  /* Step 3: input */
  /* AtQuat Qfbob_caldb; */
  AtVect Tfbob_caldb;
  AtVect D0ob, D1ob;
  /* Step 3: temporary */
  double s0,s1;
  AtVect D0ob_mul, D1ob_mul;
  /* Step 3: output */
  AtVect P0ob, P1ob;

  /* Step 4: output */
  AtRotMat Mob, Mfb;
  AtRotMat Rfbob;
  AtQuat Qfbob;

  /* Step 5: temporary */
  AtVect Pfb_c, Pob_c;
  AtRotMat Rfbob_inv;
  AtVect Pfbob_c;
  /* Step 5: output */
  AtVect Tfbob;


  memcpy( Qfbmd0, aligninfo->Qfbmd0, 4*sizeof(double) );
  memcpy( Qfbmd1, aligninfo->Qfbmd1, 4*sizeof(double) ); 
  memcpy( Tfbmd0, aligninfo->Tfbmd0, 3*sizeof(double) );
  memcpy( Tfbmd1, aligninfo->Tfbmd1, 3*sizeof(double) );
  P0md[0] = met.x_psd0;
  P0md[1] = met.y_psd0;
  P0md[2] = 0;
  P1md[0] = met.x_psd1;
  P1md[1] = met.y_psd1;
  P1md[2] = 0;

  memcpy( Qobml0, aligninfo->Qobml0, 4*sizeof(double) );
  memcpy( Qobml1, aligninfo->Qobml1, 4*sizeof(double) ); 
  memcpy( Tobml0, aligninfo->Tobml0, 3*sizeof(double) );
  memcpy( Tobml1, aligninfo->Tobml1, 3*sizeof(double) );
  memcpy( L0ml, aligninfo->L0ml, 3*sizeof(double) );
  memcpy( L1ml, aligninfo->L1ml, 3*sizeof(double) );

  /* memcpy( Qfbob_caldb, aligninfo->Qfbob, 4*sizeof(double) ); */
  memcpy( Tfbob_caldb, aligninfo->Tfbob, 3*sizeof(double) );
  memcpy( D0ob, aligninfo->D0ob, 3*sizeof(double) );
  memcpy( D1ob, aligninfo->D1ob, 3*sizeof(double) );


  /* Step 1: Metrology data (MD) transformation into Focal Plane Bench Frame (FB) */

  headas_chat(CHATTY, "%s: Info: P0md =%f %f %f\n",global.taskname,P0md[0],P0md[1],P0md[2]);
  if(RotoTranslate(Qfbmd0, Tfbmd0, P0md, P0fb))
    goto ComputeMastAspectSolution_end;
  headas_chat(CHATTY, "%s: Info: P0fb =%f %f %f\n",global.taskname,P0fb[0],P0fb[1],P0fb[2]);


  headas_chat(CHATTY, "%s: Info: P1md =%f %f %f\n",global.taskname,P1md[0],P1md[1],P1md[2]);
  if(RotoTranslate(Qfbmd1, Tfbmd1, P1md, P1fb))
    goto ComputeMastAspectSolution_end;
  headas_chat(CHATTY, "%s: Info: P1fb =%f %f %f\n",global.taskname,P1fb[0],P1fb[1],P1fb[2]);


  /* Step 2: Laser Positions transformation into Optics Bench Frame (OB) */
  
  headas_chat(CHATTY, "%s: Info: L0ml =%f %f %f\n",global.taskname,L0ml[0],L0ml[1],L0ml[2]);
  /*   headas_chat(CHATTY, "%s: Info: Qobml0 =%f %f %f %f\n",global.taskname,Qobml0[0],Qobml0[1],Qobml0[2],Qobml0[3]); */
  /*   headas_chat(CHATTY, "%s: Info: Tobml0 =%f %f %f\n",global.taskname,Tobml0[0],Tobml0[1],Tobml0[2]); */

  if(RotoTranslate(Qobml0, Tobml0, L0ml, L0ob))
    goto ComputeMastAspectSolution_end;
  headas_chat(CHATTY, "%s: Info: L0ob =%f %f %f\n",global.taskname,L0ob[0],L0ob[1],L0ob[2]);

  headas_chat(CHATTY, "%s: Info: L1ml =%f %f %f\n",global.taskname,L1ml[0],L1ml[1],L1ml[2]);
  /*   headas_chat(CHATTY, "%s: Info: Qobml1 =%f %f %f %f\n",global.taskname,Qobml1[0],Qobml1[1],Qobml1[2],Qobml1[3]); */
  /*   headas_chat(CHATTY, "%s: Info: Tobml1 =%f %f %f\n",global.taskname,Tobml1[0],Tobml1[1],Tobml1[2]); */

  if(RotoTranslate(Qobml1, Tobml1, L1ml, L1ob))
    goto ComputeMastAspectSolution_end;
  headas_chat(CHATTY, "%s: Info: L1ob =%f %f %f\n",global.taskname,L1ob[0],L1ob[1],L1ob[2]);


  /* Step 3: Computation of laser spot intersection in Optics Bench Frame (OB) */

  /* Construct a reference plane */
  s0=(-Tfbob_caldb[2]-L0ob[2]+P0fb[2])/D0ob[2];
  s1=(-Tfbob_caldb[2]-L1ob[2]+P1fb[2])/D1ob[2];
  headas_chat(CHATTY, "%s: Info: s0 = %f s1 = %f\n",global.taskname,s0,s1);

  if(atMulVect(s0, D0ob,D0ob_mul)){
    goto ComputeMastAspectSolution_end;
  }
  if(atAddVect(L0ob, D0ob_mul, P0ob)){
    goto ComputeMastAspectSolution_end;
  }
  headas_chat(CHATTY, "%s: Info: P0ob =%f %f %f\n",global.taskname,P0ob[0],P0ob[1],P0ob[2]);

  if(atMulVect(s1, D1ob,D1ob_mul)){
    goto ComputeMastAspectSolution_end;
  }
  if(atAddVect(L1ob, D1ob_mul, P1ob)){
    goto ComputeMastAspectSolution_end;
  }
  headas_chat(CHATTY, "%s: Info: P1ob =%f %f %f\n",global.taskname,P1ob[0],P1ob[1],P1ob[2]);


  /* Step 4: Computation of the rotation between Focal Plane Bench (FB) and Optics Bench (OB) frames (Rfbob) */

  /* 4a) computation of the new frame K;
     4b) computation of orthonormal basis */
  if(ComputeOrthonormalBasis(P1ob, P0ob, Mob)){
    goto ComputeMastAspectSolution_end;
  }
  if(ComputeOrthonormalBasis(P1fb, P0fb, Mfb)){
    goto ComputeMastAspectSolution_end;
  }

  /* 4c) computation of the rotation matrix Rfbob */
  if(ComputeRotationMatrix(Mfb, Mob, Rfbob)){
    goto ComputeMastAspectSolution_end;
  }

  /* 4d) Computation of the quaternion Qfbob */
  if(atRMToQuat(Rfbob,Qfbob)){
    goto ComputeMastAspectSolution_end;
  }
  headas_chat(CHATTY, "%s: Info: Qfbob =%.10f %.10f %.10f %.10f\n",global.taskname,Qfbob[0],Qfbob[1],Qfbob[2],Qfbob[3]);

  
  /* Step 5: Computation of the translation between Focal Plane Bench (FB) and Optics Bench (OB) frames (Tfbob) */

  /* 5a) computation of the laser spot centroids */
  if(atMulAddVect(0.5, P1fb, 0.5, P0fb, Pfb_c)){
    goto ComputeMastAspectSolution_end;
  }
  if(atMulAddVect(0.5, P1ob, 0.5, P0ob, Pob_c)){
    goto ComputeMastAspectSolution_end;
  }

  /* 5b) rotation of centroid */
  if(atInvRotMat(Rfbob, Rfbob_inv)){
    goto ComputeMastAspectSolution_end;
  }
  if(atRotVect(Rfbob_inv,Pob_c, Pfbob_c)){
    goto ComputeMastAspectSolution_end;
  }

  /* 5c) computation of the translation Tfbob */
  if(atSubVect(Pfb_c, Pfbob_c, Tfbob)){
    goto ComputeMastAspectSolution_end;
  }
  headas_chat(CHATTY, "%s: Info: Tfbob =%.10f %.10f %.10f\n",global.taskname,Tfbob[0],Tfbob[1],Tfbob[2]);


  /* Set output values */
  mast->time = met.time;
  memcpy( mast->Qfbob, Qfbob, 4*sizeof(double) );
  memcpy( mast->Tfbob, Tfbob, 3*sizeof(double) );

  return OK;

 ComputeMastAspectSolution_end:
  return NOT_OK;

} /* ComputeMastAspectSolution */


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


/*
 *      ComputeOrthonormalBasis
 *
 *	DESCRIPTION:
 *            
 *            AtVect v1,    -> IN
 *            AtVect v2,    -> IN
 *            AtRotMat M    -> OUT
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeOrthonormalBasis(AtVect v1, AtVect v2, AtRotMat M){

  AtVect Z = {0,0,1};
  AtVect Y_tot;
  AtVect X;
  AtVect Y;

  /* computation of the new frame K */

  if(atSubVect(v1, v2, Y_tot)){
    headas_chat(CHATTY,"Error: ComputeOrthonormalBasis: step1 error.\n");
    return NOT_OK;
  }

  if(atNormVect(Y_tot, Y)){
    headas_chat(CHATTY,"Error: ComputeOrthonormalBasis: step2 error.\n");
    return NOT_OK;
  }

  if(atVectProd(Y, Z, X)){
    headas_chat(CHATTY,"Error: ComputeOrthonormalBasis: step3 error.\n");
    return NOT_OK;
  }

  /* computation of orthonormal basis */

  M[0][0] = X[0];
  M[1][0] = X[1];
  M[2][0] = X[2];

  M[0][1] = Y[0];
  M[1][1] = Y[1];
  M[2][1] = Y[2];

  M[0][2] = Z[0];
  M[1][2] = Z[1];
  M[2][2] = Z[2];

  return OK;

} /* ComputeOrthonormalBasis */


/*
 *      ComputeRotationMatrix
 *
 *	DESCRIPTION:
 *            
 *            AtRotMat M1,    -> IN
 *            AtRotMat M2,    -> IN
 *            AtRotMat RM     -> OUT
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeRotationMatrix(AtRotMat M1, AtRotMat M2, AtRotMat RM){

  AtRotMat M_inv;

  if(atInvRotMat(M1,M_inv)){
    headas_chat(CHATTY,"Error: ComputeRotationMatrix: step1 error.\n");
    return NOT_OK;
  }

  if(atRMProd(M_inv, M2, RM)){
    headas_chat(CHATTY,"Error: ComputeRotationMatrix: step2 error.\n");
    return NOT_OK;
  }

  return OK;

} /* ComputeRotationMatrix */


int GetMetrologyKeys(char *filename, char *extname, MetrologyKeys_t *metkeys){

  int                status=OK;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;


  /* Open readonly input filename */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetMetrologyKeys_end;
    }
 
  /* Move in <extname> extension in input filename */
  if (fits_movnam_hdu(unit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto GetMetrologyKeys_end;
    }
  
  head=RetrieveFitsHeader(unit);

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      metkeys->tstart=card->u.DVal;
      strcpy(metkeys->tstart_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->tstart=0.0;
      strcpy(metkeys->tstart_comm,"");
    }
  
  /* Retrieve observation end time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      metkeys->tstop=card->u.DVal;
      strcpy(metkeys->tstop_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->tstop=0.0;
      strcpy(metkeys->tstop_comm,"");
    }
  
  /* Retrieve date-obs from input events file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      strcpy(metkeys->dateobs,card->u.SVal);
      strcpy(metkeys->dateobs_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->dateobs,"2000-01-01T00:00:00");
      strcpy(metkeys->dateobs_comm,"");   
    }

  /* Retrieve date-end from input events file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      strcpy(metkeys->dateend,card->u.SVal);
      strcpy(metkeys->dateend_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->dateend,"2000-01-01T00:00:00");
      strcpy(metkeys->dateend_comm,"");  
    }

  if (ExistsKeyWord(&head, KWNM_HDUCLASS, &card))
    {
      strcpy(metkeys->hduclass,card->u.SVal);
      strcpy(metkeys->hduclass_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->hduclass,"");
      strcpy(metkeys->hduclass_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_HDUCLAS1, &card))
    {
      strcpy(metkeys->hduclas1,card->u.SVal);
      strcpy(metkeys->hduclas1_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_HDUCLAS1);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->hduclas1,"");
      strcpy(metkeys->hduclas1_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMEPIXR, &card))
    {
      metkeys->timepixr=card->u.EVal;
      strcpy(metkeys->timepixr_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMEPIXR);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->timepixr=0.0;
      strcpy(metkeys->timepixr_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_OBS_ID, &card))
    {
      strcpy(metkeys->obs_id,card->u.SVal);
      strcpy(metkeys->obs_id_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->obs_id,"");
      strcpy(metkeys->obs_id_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TARG_ID, &card))
    {
      metkeys->targ_id=card->u.JVal;
      strcpy(metkeys->targ_id_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->targ_id=0;
      strcpy(metkeys->targ_id_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMESYS, &card))
    {
      strcpy(metkeys->timesys,card->u.SVal);
      strcpy(metkeys->timesys_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMESYS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->timesys,"");
      strcpy(metkeys->timesys_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFI, &card))
    {
      metkeys->mjdrefi=card->u.JVal;
      strcpy(metkeys->mjdrefi_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->mjdrefi=0;
      strcpy(metkeys->mjdrefi_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFF, &card))
    {
      metkeys->mjdreff=card->u.DVal;
      strcpy(metkeys->mjdreff_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->mjdreff=0.0;
      strcpy(metkeys->mjdreff_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_CLOCKAPP, &card))
    {
      metkeys->clockapp=card->u.LVal;
      strcpy(metkeys->clockapp_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_CLOCKAPP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      metkeys->clockapp=0;
      strcpy(metkeys->clockapp_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMEUNIT, &card))
    {
      strcpy(metkeys->timeunit,card->u.SVal);
      strcpy(metkeys->timeunit_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMEUNIT);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(metkeys->timeunit,"");
      strcpy(metkeys->timeunit_comm,"");
    }

  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetMetrologyKeys_end;
    }


  return OK;

 GetMetrologyKeys_end:
  return NOT_OK;

} /* GetMetrologyKeys */


int GetObsInfo(char *filename, char *extname){

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
/*   if(ExistsKeyWord(&head, KWNM_INSTRUME, &card)) */
/*     { */
/*       strcpy(global.obsinfo.instrume, card->u.SVal); */
/*     } */
/*   else */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME); */
/*       headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename); */
/*       goto GetObsInfo_end; */
/*     } */
    
  /* Retrieve date-obs and time-obs from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.obsinfo.dateobs=card->u.SVal;
      if(!(strchr(global.obsinfo.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.obsinfo.timeobs=card->u.SVal;
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
      global.obsinfo.dateend=card->u.SVal;
      if(!(strchr(global.obsinfo.dateend, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
	    global.obsinfo.timeend=card->u.SVal;
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

