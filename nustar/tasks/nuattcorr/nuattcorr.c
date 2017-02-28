/*
 * 
 *	nuattcorr.c
 *
 *	INVOCATION:
 *
 *		nuattcorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine to correct Attitude data
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 24/10/12 - First version
 *        0.1.1 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.2 - NS 04/06/13 - Handle attitude reprocessing
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuattcorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuattcorr.h"


/********************************/
/*         definitions          */
/********************************/

#define NUATTCORR_C
#define NUATTCORR_VERSION      "0.1.2"
#define PRG_NAME               "nuattcorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuattcorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuattcorr.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuattcorr_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 24/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuattcorr_getpar()
{
  
  /* Input Attitude File Name */
  if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
      goto Error;
    }
  
  /* Output Corrected Attitude File Name */
  if(PILGetFname(PAR_OUTATTFILE, global.par.outattfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTATTFILE);
      goto Error;	
    }
    
  /* Input CHUs Quaternion Offset File Name */
  if(PILGetString(PAR_CHUOFFSETFILE, global.par.chuoffsetfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CHUOFFSETFILE);
      goto Error;
    }

  get_history(&global.hist);
  nuattcorr_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuattcorr_getpar */


/*
 *	nuattcorr_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuattcorr_checkinput();
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
 *             int CopyFile(char *source, char *destination);
 *             int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, 
 *                                char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);
 *	
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 24/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuattcorr_work()
{
  int                status=OK;
  long               extfile=-1;
  char               cmd[BUF_SIZE];


  /* Check input parameters */
  if(nuattcorr_checkinput())
    goto Error;


  /* Get Attitude Info from input attfile */
  if( GetAttitudeInfo(global.par.attfile, KWVL_EXTNAME_ATTITUDE, &global.attinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' attfile.\n", global.taskname, global.par.attfile);
    goto Error;
  }


  /* Create the task temporary directory */
  if(mkdir(global.tmp.dirname,0777)){
    headas_chat(NORMAL, "%s: Error: Unable to create the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
    goto Error;
  }

  /* Create local symbolic link to input attitude file  */
  headas_chat(NORMAL, "%s: Info: Creating '%s' symbolic link to input Attitude file.\n", global.taskname, global.tmp.loc_attfile);
  if( CreateAbsSymbolicLink(global.par.attfile, global.tmp.loc_attfile) )
    {
      headas_chat(NORMAL, "%s: Error: unable to create symbolic link '%s' to '%s'\n", global.taskname, global.tmp.loc_attfile, global.par.attfile);
      goto Error;
    }


  /* Derive CALDB CHUOFFSET filename */  
  extfile=-1;
  if ( !strcasecmp(global.par.chuoffsetfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.attinfo.dateobs, global.attinfo.timeobs, global.attinfo.dateend, global.attinfo.timeend, KWVL_CHUOFFSET_DSET, global.par.chuoffsetfile, HD_EXPR, &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for chuoffsetfile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }


  /* Get CHUs Quaternion Offset Info from input chuoffsetfile */
  if ( GetChuOffsetInfo(global.par.chuoffsetfile, &global.chuoffsetinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' chuoffsetfile.\n", global.taskname, global.par.chuoffsetfile);
    goto Error;
  }


  /* Create temporary file needed by ftcopy ftool */
  if ( CreateColFile(global.tmp.loc_colfile, &global.chuoffsetinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to create ftcopy temporary file '%s'.\n", global.taskname, global.tmp.loc_colfile);
    goto Error;
  }


  /* Execute ftcopy to correct attitude */
  sprintf(cmd, "ftcopy infile='%s[col @%s]' outfile=%s", global.tmp.loc_attfile, global.tmp.loc_colfile, global.tmp.loc_outattfile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, global.tmp.loc_outattfile);
    goto Error;
  }


  /* Update 'NUATTCOR' Keyword */
  global.attinfo.nuattcor = TRUE ;

  if( UpdateKeywords(global.tmp.loc_outattfile, &global.attinfo)  )
    {
      headas_chat(NORMAL, "%s: Error: Unable to update temporary file '%s'.\n", global.taskname, global.tmp.loc_outattfile);
      goto Error;
    }


  /* Rename temporary file into output file */
  if (RenameFile(global.tmp.loc_outattfile,global.par.outattfile) == -1)
    {
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file '%s' to '%s'.\n", global.taskname, global.tmp.loc_outattfile, global.par.outattfile);
      goto Error;
    }

  /* Remove local temporary file  */
  if( unlink(global.tmp.loc_colfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, global.tmp.loc_colfile);
  }

  /* Remove local symbolic link to input attfile  */
  if( unlink(global.tmp.loc_attfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, global.tmp.loc_attfile);
  }


  /* Delete the task temporary directory */
  if(rmdir(global.tmp.dirname)){
    perror("rmdir");
    headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
  }

  
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outattfile);


  return OK; 
  
 Error:

  return NOT_OK;

} /* nuattcorr_work */


/*
 *	nuattcorr
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
 *             void nuattcorr_getpar(void);
 * 	       void nuattcorr_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 24/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuattcorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUATTCORR_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuattcorr_getpar() == OK) 
    {
      
      if ( nuattcorr_work()) 
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
  
} /* nuattcorr */


/*
 *	nuattcorr_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 24/10/12 - First version
 *
 */
void nuattcorr_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Attitude file                       :'%s'\n",global.par.attfile);
  headas_chat(NORMAL,"Name of the output Corrected Attitude file            :'%s'\n",global.par.outattfile);
  headas_chat(NORMAL,"Name of the input CHUs Quaternion Offset FITS file    :'%s'\n",global.par.chuoffsetfile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* nuattcorr_info */


/*
 *	nuattcorr_checkinput
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
 *        0.1.0: - NS 24/10/12 - First version
 *
 */
int nuattcorr_checkinput(void)
{
  char           BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN], ext[MAXEXT_LEN];
  int            overwrite=0;
  pid_t          pid;


  /* Get pid */
  pid = getpid();
  

  /* Check if outattfile == NONE */    
  if (!(strcasecmp (global.par.outattfile, DF_NONE)))
    {
      /* If outattfile == NONE check if input file is compressed */
      GetFilenameExtension(global.par.attfile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	{
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.attfile);
	  headas_chat(NORMAL, "%s: Error: input attitude file is compressed, cannot update it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  /* Overwrite input file */
	  overwrite=1;
	  strcpy(global.par.outattfile, global.par.attfile);
	}
    }
  else
    {
      if(FileExists(global.par.outattfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outattfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outattfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outattfile);
	      if( strcmp(global.par.outattfile,global.par.attfile) ){
		if(remove (global.par.outattfile) == -1)
		  {
		    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outattfile);
		    goto check_end;
		  }
	      }
	    }
	}
    }


  /* Set temporary task directory name */
  sprintf(global.tmp.dirname, "%d_tmp_nuattcorr", (int)pid);


  /* Derive temporary local file name */
  sprintf(global.tmp.loc_colfile, "%s/%din.col", global.tmp.dirname, (int)pid);


  /* Derive the local link name of the input attitude file */
  SplitFilePath(global.par.attfile, DirName, BaseName);
  sprintf(global.tmp.loc_attfile, "%s/%din_%s", global.tmp.dirname, (int)pid, BaseName);


  /* Derive the temporary name of the corrected attitude file */
  SplitFilePath(global.par.outattfile, DirName, BaseName);
  sprintf(global.tmp.loc_outattfile, "%s/%dout_%s", global.tmp.dirname, (int)pid, BaseName);    


  /* Check if temporary putput file exists to remove it */
  if(FileExists(global.tmp.loc_outattfile))
    if(remove (global.tmp.loc_outattfile) == -1)
      {
	headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmp.loc_outattfile);
	goto check_end;
      }

  
  return OK;

 check_end:
  return NOT_OK;
}


int GetAttitudeInfo(char *filename, char *extname, AttitudeInfo_t *attinfo){

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
      goto GetAttitudeInfo_end;
    }
    
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  /* Retrieve NUATTCORR from input file */  
  if(!(ExistsKeyWord(&head, KWNM_NUATTCOR, NULLNULLCARD)) || !(GetLVal(&head, KWNM_NUATTCOR)))
    {
      attinfo->nuattcor = FALSE ;
    }
  else
    {
      attinfo->nuattcor = TRUE ;
    }

    
  /* Retrieve date-obs and time-obs from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      attinfo->dateobs=card->u.SVal;
      if(!(strchr(attinfo->dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    attinfo->timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetAttitudeInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetAttitudeInfo_end;
    }
    
  /* Retrieve date-end and time-end from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      attinfo->dateend=card->u.SVal;
      if(!(strchr(attinfo->dateend, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
	    attinfo->timeend=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEEND);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetAttitudeInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetAttitudeInfo_end;
    }


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }
 
  return OK;
  
 GetAttitudeInfo_end:
  
  CloseFitsFile(inunit);
  return NOT_OK;

} /* GetAttitudeInfo */


int GetChuOffsetInfo(char *filename, ChuOffsetInfo_t *info){

  unsigned           FromRow, ReadRows, nCols;
  int                status=OK;
  ChuOffsetCol_t     col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);

  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetChuOffsetInfo_end;
    }
 
  /* Move in CHUOFFSET extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_CHUOFFSET, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_CHUOFFSET);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto GetChuOffsetInfo_end;
    }
  
  /* Retrieve header pointer */ 
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto GetChuOffsetInfo_end;
    }

  /* Get needed columns number from name */

  if ((col.Q_CHU4SC=ColNameMatch(CLNM_Q_CHU4SC, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_CHU4SC);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto GetChuOffsetInfo_end;
    }

  if (table.Multiplicity[col.Q_CHU4SC]!=4)
    {
      headas_chat(NORMAL, "%s: Error: bad multiplicity of '%s' column\n", global.taskname, CLNM_Q_CHU4SC);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto GetChuOffsetInfo_end;
    }


  /* Finish bintable header */
  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  /* NOTE: read only one row! */
  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

    DVECVEC_ARRAY_READ(info->q_chu4sc, 4, table, 0, col.Q_CHU4SC);
  }


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  

  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetChuOffsetInfo_end;
    }


  return OK;

 GetChuOffsetInfo_end:
  return NOT_OK;

} /* GetChuOffsetInfo */


int CreateColFile(char *filename, ChuOffsetInfo_t *chuoffsetinfo){

  FILE	   *file;

  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n", global.taskname, filename);
    goto CreateColFile_end;
  }

  fprintf(file, "// ATTCHU4.COL - correct SOURCE=2 attitude data\n");
  fprintf(file, "//  Summary: fixes attitude data from spacecraft bus (SOURCE=2) to be in\n");
  fprintf(file, "//   the same coordinate system as science data (SOURCE=1; 'CHU4').\n");
  fprintf(file, "//   By default, the bus quaternions are rotated by 45 deg from CHU4.\n");
  fprintf(file, "// Details\n");
  fprintf(file, "//   1. It applies a quaternion offset to data from the bus.\n");
  fprintf(file, "//   2. It also enforces the normalization criterium for quaternions.\n");
  fprintf(file, "//   3. It also enforces the Q[3] >= 0 convention for quaternions\n");
  fprintf(file, "// Caveats:\n");
  fprintf(file, "//    * must not be applied more than once to a given file\n");
  fprintf(file, "//    * the resulting attitude for SOURCE=2 will be correct at\n");
  fprintf(file, "//      ~5 arcmin level; Data from SOURCE=1 is unchanged EXCEPT\n");
  fprintf(file, "//    * normalization and sign of quaternion may be improved\n");
  fprintf(file, "// How to call:\n");
  fprintf(file, "//   Use CFITSIO calculator syntax:\n");
  fprintf(file, "//      ftcopy 'oldfile.att[col @attchu4.col]' 'newfile.att'\n");
  fprintf(file, "//   \n");
  fprintf(file, "// Original author: C. B. Markwardt GSFC 2012-08-14\n");
  fprintf(file, "\n");
  fprintf(file, "\n");
  fprintf(file, "// Keep all original columns\n");
  fprintf(file, "*;\n");
  fprintf(file, "// Make new scratch column which has same dimensions/type as original\n");

  if(global.attinfo.nuattcor)
    fprintf(file, "QPARAM=QPARAM_ORIG;\n");
  else
    fprintf(file, "QPARAM_ORIG=QPARAM;\n");

  fprintf(file, "NEWQPARAM=QPARAM;\n");
  fprintf(file, "// Spacecraft to CHU4 offset quaternion to be applied\n");
  fprintf(file, "#QCHU4SC0(S/C to CHU4 offset Q0 applied) = %.16g;\n", chuoffsetinfo->q_chu4sc[0]);
  fprintf(file, "#QCHU4SC1(S/C to CHU4 offset Q1 applied) = %.16g;\n", chuoffsetinfo->q_chu4sc[1]);
  fprintf(file, "#QCHU4SC2(S/C to CHU4 offset Q2 applied) = %.16g;\n", chuoffsetinfo->q_chu4sc[2]);
  fprintf(file, "#QCHU4SC3(S/C to CHU4 offset Q3 applied) = %.16g;\n", chuoffsetinfo->q_chu4sc[3]);
  fprintf(file, "\n");
  fprintf(file, "// Quaternion multiplication: NEWQPARAM = QPARAM * QCHU4SC\n");
  fprintf(file, "NEWQPARAM = (SOURCE != 2)?(QPARAM):({+QPARAM[1]*QCHU4SC3+QPARAM[2]*QCHU4SC2-QPARAM[3]*QCHU4SC1+QPARAM[4]*QCHU4SC0,-QPARAM[1]*QCHU4SC2+QPARAM[2]*QCHU4SC3+QPARAM[3]*QCHU4SC0+QPARAM[4]*QCHU4SC1,+QPARAM[1]*QCHU4SC1-QPARAM[2]*QCHU4SC0+QPARAM[3]*QCHU4SC3+QPARAM[4]*QCHU4SC2,-QPARAM[1]*QCHU4SC0-QPARAM[2]*QCHU4SC1-QPARAM[3]*QCHU4SC2+QPARAM[4]*QCHU4SC3});\n");
  fprintf(file, "\n");
  fprintf(file, "// Normalize so that the last quaternion component is always positive\n");
  fprintf(file, "NEWQPARAM = (NEWQPARAM[4] >= 0)?(NEWQPARAM):(-NEWQPARAM);\n");
  fprintf(file, "// Normalize quaternion to unity just to be sure\n");
  fprintf(file, "NEWQPARAM = NEWQPARAM/SQRT(SUM(NEWQPARAM^2));\n");
  fprintf(file, "\n");
  fprintf(file, "// Replace original with new value (preserves original column ordering, data types and documentation keywords)\n");
  fprintf(file, "QPARAM = NEWQPARAM;\n");
  fprintf(file, "\n");
  fprintf(file, "// Remove scratch column\n");
  fprintf(file, "-NEWQPARAM;\n");
  fprintf(file, "\n");


  fclose(file);  
  
  return OK;
  
 CreateColFile_end:
  return NOT_OK;

} /* CreateColFile */


int UpdateKeywords(char *filename, AttitudeInfo_t *info){

  int             status=OK;
  fitsfile        *fptr=NULL;  


  /* Open input file */
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateKeywords_end;
    }
  
  /* Move in ATTITUDE extension */
  if (fits_movnam_hdu(fptr, ANY_HDU,KWVL_EXTNAME_ATTITUDE, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname, KWVL_EXTNAME_ATTITUDE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateKeywords_end;
    }
  
  /* NUATTCOR */
  if(fits_update_key(fptr, TLOGICAL, KWNM_NUATTCOR, &info->nuattcor, CARD_COMM_NUATTCOR, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_NUATTCOR);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateKeywords_end;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateKeywords_end;
    }
  

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateKeywords_end;
    }


  return OK;

 UpdateKeywords_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK; 
  
} /* UpdateKeywords */
