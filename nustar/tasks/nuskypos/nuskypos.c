/*
 * 
 *	nuskypos.c
 *
 *	INVOCATION:
 *
 *		nuskypos [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 12/01/12 - First version
 *        0.1.1 - NS 26/03/12 - Added new keywords in output optaxisfile and det1reffile
 *        0.1.2 - NS 08/02/13 - Added new columns for Aperture Stop in output optaxisfile
 *        0.1.3 - RF 22/04/16 - Update the value of the keywords RA DEC NAME name of the output file 'det1' e 'oa'
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuskypos  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuskypos.h"


/********************************/
/*         definitions          */
/********************************/

#define NUSKYPOS_C
#define NUSKYPOS_VERSION      "0.1.3"
#define PRG_NAME               "nuskypos"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuskypos_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuskypos.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuskypos_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 12/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuskypos_getpar()
{

  if(PILGetReal(PAR_PNTRA, &global.par.pntra))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PNTRA);
      goto Error;
    }
 
  if(PILGetReal(PAR_PNTDEC, &global.par.pntdec))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PNTDEC);
      goto Error;
    }

  if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
      goto Error;	
    }
  
  if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
      goto Error;	
    }

  if(PILGetFname(PAR_TELDEF, global.par.teldef)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TELDEF);
      goto Error;	
    }

  if(PILGetString(PAR_INSTRUMENT, global.par.instrument)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INSTRUMENT);
      goto Error;	
    }

  if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ABERRATION);
      goto Error;	
    }

  if(PILGetFname(PAR_OPTAXISFILE, global.par.optaxisfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OPTAXISFILE);
      goto Error;	
    }
  if ( !strcasecmp(global.par.optaxisfile,DF_NONE) )
    global.optaxisfile = FALSE;
  else
    global.optaxisfile = TRUE;


  if(PILGetFname(PAR_DET1REFFILE, global.par.det1reffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1REFFILE);
      goto Error;	
    }
  if ( !strcasecmp(global.par.det1reffile,DF_NONE) )
    global.det1reffile = FALSE;
  else
    global.det1reffile = TRUE;


  if(global.det1reffile){

    if(PILGetInt(PAR_DET1XREF, &global.par.det1xref))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1XREF);
	goto Error;	
      }
    
    if(PILGetInt(PAR_DET1YREF, &global.par.det1yref))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1YREF);
	goto Error;	
      }

  } /* End -> if(global.det1reffile) */


  if( global.det1reffile ){
    if(PILGetFname(PAR_MASTASPECTFILE, global.par.mastaspectfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MASTASPECTFILE);
	goto Error;	
      }
  }

  /* inistseed */
  if(PILGetBool(PAR_INITSEED, &global.par.initseed))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INITSEED); 
      goto Error;	 
    }


  get_history(&global.hist);
  nuskypos_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuskypos_getpar */


/*
 *	nuskypos_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuskypos_checkinput();
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
 *        0.1.0 - NS 12/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuskypos_work()
{
  AlignInfo_t    aligninfo;
  AttitudeInfo_t *attinfo=NULL;
  AttitudeKeys_t attkeys;
  int            attcount;
  double         mjdref;


  if(nuskypos_checkinput())
    goto Error;


  /* Retrieve alignment info from input alignfile*/
  if( ReadAlignInfo(global.par.alignfile, &aligninfo) )
    {
      headas_chat(NORMAL, " %s: Error: unable to read alignment file.\n", global.taskname);
      goto Error;
    }

  /* Retrieve attitude time values from input attfile */
  if(GetAttitudeTimes(global.par.attfile, &attinfo, &attcount, &mjdref, &attkeys))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read input attitude file.\n", global.taskname);
      goto Error;
    }

  if(global.optaxisfile){
    /* Write output Optical Axis File */
    if(WriteOptAxisFile(attinfo, attcount, mjdref, &attkeys, &aligninfo, global.par.attfile, global.par.optaxisfile))
      {
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.par.optaxisfile);
	goto Error;
      }
    
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.optaxisfile);
  }

  if(global.det1reffile){
    /* Write output DET1 Reference Point File */
    if(WriteDet1RefFile(attinfo, attcount, mjdref, &attkeys, &aligninfo, global.par.attfile, global.par.det1reffile))
      {
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.par.det1reffile);
	goto Error;
      }
    
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.det1reffile);
  }


  return OK; 
  
 Error:

  return NOT_OK;
} /* nuskypos_work */


/*
 *	nuskypos
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
 *             void nuskypos_getpar(void);
 * 	       void nuskypos_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 12/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuskypos()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUSKYPOS_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuskypos_getpar() == OK) 
    {
      
      if ( nuskypos_work()) 
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
  
} /* nuskypos */


/*
 *	nuskypos_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 12/01/12 - First version
 *
 */
void nuskypos_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* nuskypos_info */


/*
 *	nuskypos_checkinput
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
 *        0.1.0: - NS 12/01/12 - First version
 *
 */
int nuskypos_checkinput(void)
{

  if(strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA) && strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMB))
    {
      headas_chat(NORMAL, "%s: Error: input value '%s' not allowed for input parameter 'instrument'.\n", global.taskname, global.par.instrument);
      goto check_end;
    }


  if( global.optaxisfile && FileExists(global.par.optaxisfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.optaxisfile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.optaxisfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.optaxisfile);
	  if(remove (global.par.optaxisfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.optaxisfile);
	      goto check_end;
	    }
	}
    }

 
  if( global.det1reffile && FileExists(global.par.det1reffile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.det1reffile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.det1reffile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.det1reffile);
	  if(remove (global.par.det1reffile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.det1reffile);
	      goto check_end;
	    }
	}
    }

 
  return OK;

 check_end:
  return NOT_OK;
}


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


  /* Move in OPTICAL_AXIS extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OPTICAL_AXIS, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OPTICAL_AXIS);
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

  if ((col.X_DET2A=ColNameMatch(CLNM_X_DET2A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET2A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET2A=ColNameMatch(CLNM_Y_DET2A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET2A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.X_DET2B=ColNameMatch(CLNM_X_DET2B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET2B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET2B=ColNameMatch(CLNM_Y_DET2B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET2B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
    aligninfo->x_det2a = DVEC(table,0,col.X_DET2A);
    aligninfo->y_det2a = DVEC(table,0,col.Y_DET2A);
    aligninfo->x_det2b = DVEC(table,0,col.X_DET2B);
    aligninfo->y_det2b = DVEC(table,0,col.Y_DET2B);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Move in APERTURE_STOP extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_APERTURE_STOP, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_APERTURE_STOP);
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

  if ((col.X_DET1A=ColNameMatch(CLNM_X_DET1A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET1A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET1A=ColNameMatch(CLNM_Y_DET1A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET1A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.X_DET1B=ColNameMatch(CLNM_X_DET1B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET1B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET1B=ColNameMatch(CLNM_Y_DET1B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET1B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
    aligninfo->as_x_det1a = DVEC(table,0,col.X_DET1A);
    aligninfo->as_y_det1a = DVEC(table,0,col.Y_DET1A);
    aligninfo->as_x_det1b = DVEC(table,0,col.X_DET1B);
    aligninfo->as_y_det1b = DVEC(table,0,col.Y_DET1B);
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




int GetAttitudeTimes(char *filename, AttitudeInfo_t ** info, int *attcount, double *mjdref, AttitudeKeys_t *attkeys){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  unsigned           timecol;
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
      return NOT_OK;
    }
 
  /* Move in ATTITUDE extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_ATTITUDE, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ATTITUDE);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto GetAttitudeTimes_end;
    }
  
  head=RetrieveFitsHeader(unit);

  /* Get MJDREF value */
  status = 0;
  *mjdref = HDget_frac_time(unit, KWNM_MJDREF, 0,0, &status);
  if(status){
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto GetAttitudeTimes_end;
  }

  /* Read Attitude keywords */
  if(GetAttitudeKeys(head, filename, attkeys))
    goto GetAttitudeTimes_end;
  

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto GetAttitudeTimes_end;
    }


  /* Get needed columns number from name */
  if ((timecol = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto GetAttitudeTimes_end;
    }

 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *attcount = table.MaxRows;
 *info = (AttitudeInfo_t *)calloc(*attcount, sizeof(AttitudeInfo_t));
 if(*info==NULL){
   headas_chat(CHATTY,"%s: Error: GetAttitudeTimes: memory allocation failure.\n", global.taskname);
   goto GetAttitudeTimes_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*attcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 (*info)[count].time = DVEC(table,n,timecol);
	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
  *attcount = count;


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

  
 GetAttitudeTimes_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* GetAttitudeTimes  */


int GetAttitudeKeys(FitsHeader_t head, char *filename, AttitudeKeys_t *keys){

  FitsCard_t   *card;

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      keys->tstart=card->u.DVal;
      strcpy(keys->tstart_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->tstart=0.0;
      strcpy(keys->tstart_comm,"");
    }
  
  /* Retrieve observation end time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      keys->tstop=card->u.DVal;
      strcpy(keys->tstop_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->tstop=0.0;
      strcpy(keys->tstop_comm,"");
    }
  
  /* Retrieve date-obs from input events file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      strcpy(keys->dateobs,card->u.SVal);
      strcpy(keys->dateobs_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->dateobs,"2000-01-01T00:00:00");
      strcpy(keys->dateobs_comm,"");   
    }

  /* Retrieve date-end from input events file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      strcpy(keys->dateend,card->u.SVal);
      strcpy(keys->dateend_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->dateend,"2000-01-01T00:00:00");
      strcpy(keys->dateend_comm,"");  
    }

  if (ExistsKeyWord(&head, KWNM_HDUCLASS, &card))
    {
      strcpy(keys->hduclass,card->u.SVal);
      strcpy(keys->hduclass_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->hduclass,"");
      strcpy(keys->hduclass_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_HDUCLAS1, &card))
    {
      strcpy(keys->hduclas1,card->u.SVal);
      strcpy(keys->hduclas1_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_HDUCLAS1);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->hduclas1,"");
      strcpy(keys->hduclas1_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMEPIXR, &card))
    {
      keys->timepixr=card->u.EVal;
      strcpy(keys->timepixr_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMEPIXR);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->timepixr=0.0;
      strcpy(keys->timepixr_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_OBS_ID, &card))
    {
      strcpy(keys->obs_id,card->u.SVal);
      strcpy(keys->obs_id_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->obs_id,"");
      strcpy(keys->obs_id_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TARG_ID, &card))
    {
      keys->targ_id=card->u.JVal;
      strcpy(keys->targ_id_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->targ_id=0;
      strcpy(keys->targ_id_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_OBJECT, &card))
    {
      strcpy(keys->object, card->u.SVal);
      strcpy(keys->object_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_OBJECT);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->object,"");
      strcpy(keys->object_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_RA_OBJ, &card))
    {
      keys->raobj = card->u.DVal;
      strcpy(keys->raobj_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_RA_OBJ);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->raobj = 0.0;
      strcpy(keys->raobj_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_DEC_OBJ, &card))
    {
      keys->decobj = card->u.DVal;
      strcpy(keys->decobj_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DEC_OBJ);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->decobj = 0.0;
      strcpy(keys->decobj_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_RA_NOM, &card))
    {
      keys->ranom = card->u.DVal;
      strcpy(keys->ranom_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->ranom = 0.0;
      strcpy(keys->ranom_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_DEC_NOM, &card))
    {
      keys->decnom = card->u.DVal;
      strcpy(keys->decnom_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->decnom = 0.0;
      strcpy(keys->decnom_comm,"");
    }
    
  if(ExistsKeyWord(&head, KWNM_RA_PNT, &card))
    {
      keys->rapnt = card->u.DVal;
      strcpy(keys->rapnt_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_RA_PNT);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->rapnt = 0.0;
      strcpy(keys->rapnt_comm,"");
    }

  if(ExistsKeyWord(&head, KWNM_DEC_PNT, &card))
    {
      keys->decpnt = card->u.DVal;
      strcpy(keys->decpnt_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_DEC_PNT);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->decpnt = 0.0;
      strcpy(keys->decpnt_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMESYS, &card))
    {
      strcpy(keys->timesys,card->u.SVal);
      strcpy(keys->timesys_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMESYS);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->timesys,"");
      strcpy(keys->timesys_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFI, &card))
    {
      keys->mjdrefi=card->u.JVal;
      strcpy(keys->mjdrefi_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->mjdrefi=0;
      strcpy(keys->mjdrefi_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFF, &card))
    {
      keys->mjdreff=card->u.DVal;
      strcpy(keys->mjdreff_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->mjdreff=0.0;
      strcpy(keys->mjdreff_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_CLOCKAPP, &card))
    {
      keys->clockapp=card->u.LVal;
      strcpy(keys->clockapp_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_CLOCKAPP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      keys->clockapp=0;
      strcpy(keys->clockapp_comm,"");
    }

  if (ExistsKeyWord(&head, KWNM_TIMEUNIT, &card))
    {
      strcpy(keys->timeunit,card->u.SVal);
      strcpy(keys->timeunit_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TIMEUNIT);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      strcpy(keys->timeunit,"");
      strcpy(keys->timeunit_comm,"");
    }

  return OK;

/*  GetAttitudeKeys_end: */
/*   return NOT_OK; */

} /* GetAttitudeKeys */


int ConvertRawToSkyCoords(double rawx, double rawy, int seg, double mjdref, double time, TELDEF* teldef, ATTFILE* attfile, Coord_t *coord){

  double     db_detx, db_dety, db_x, db_y;
  double     mjd;
  QUAT*      q;
 
  q = allocateQuat();


  /* Calculate Detector DETX and DETY */
  convertRawToDetectorUsingTeldef(teldef, &db_detx, &db_dety,
					      seg, rawx , rawy, 
					      0.0, 0.0);

  coord->detx = db_detx;
  coord->dety = db_dety;


  findQuatInAttFile(attfile,q,time);


  if(global.par.aberration) {

      mjd = mjdref + time/86400.;      
      coord->v=earth_velocity(coord->vhat,earth_longitude(mjd));
    }
  else{
    coord->v=0.;
    coord->vhat[0]=0.;
    coord->vhat[1]=0.;
    coord->vhat[2]=0.;
  }
   
 
  /* Calculate SKY X and Y */      
  convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				  db_detx, db_dety, 
				  q, coord->v, coord->vhat);
  
  coord->x=db_x;
  coord->y=db_y;


  return OK;
  
} /* ConvertRawToSkyCoords */



int WriteOptAxisFile(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteOptAxisFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteOptAxisFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
   
  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_RA_NOM, &global.par.pntra, CARD_COMM_RA_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
   
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_NOM, &global.par.pntdec, CARD_COMM_DEC_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
   
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);

  /* Add INSTRUME */
  if(!strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&head, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME); 
 
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
      goto WriteOptAxisFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteOptAxisFile_end;
      }
    }

  /* Create 'OPTICAL_AXIS' Ext  */
  if (WriteOptAxisExt(attinfo, attcount, mjdref, attkeys, aligninfo, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_PSDPOS);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
  hducount++;
  
  /* Move to OPTICAL_AXIS ext */
  if(fits_movabs_hdu(outunit, 2, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in OPTICAL_AXIS HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_RA_NOM, &global.par.pntra, CARD_COMM_RA_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
    
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_NOM, &global.par.pntdec, CARD_COMM_DEC_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
   

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteOptAxisFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteOptAxisFile_end;
    }


  return OK;
  
 WriteOptAxisFile_end:

  return NOT_OK;


} /* WriteOptAxisFile */


int WriteOptAxisExt(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, FitsFileUnit_t ounit){

  int             n, optcount=0;
  const float     fnan = log(-1);  /* float NaN value */
  unsigned        OutRows=0;
  double          x_det2, y_det2;
  int             as_det1x, as_det1y;
  int             as_det2x, as_det2y;
  int             seg=1;
  long int        seed;
  Coord_t         coordset;
  TELDEF          *teldef;
  ATTFILE         *attfile;
  MastInfo_t      *mastinfo=NULL;
  unsigned        mastindex=1, mastcount=MAST_BLOCK_ROWS;
  BOOL            mastend;
  OptAxisCol_t    indxcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Open Attitude file */
  attfile = openAttFile(global.par.attfile);
  setAttFileInterpolation(attfile, 0);


  /* Open TELDEF file */
  teldef = readTelDef(global.par.teldef);
  setSkyCoordCenterInTeldef(teldef, global.par.pntra, global.par.pntdec);


  /* Retrieve mast aspect solution info from input mastaspectfile */
  if(ReadMastAspectSolutionInfo(global.par.mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
      goto WriteOptAxisExt_end;
    }
  mastindex += mastcount;
  mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */


  /* initialize random number generator */
  if(global.par.initseed){
    seed = -345337518;
  }
  else{
    get_ran2seed(&seed);
  }


  /* Check instrument (FPMA or FPMB) */
  if(!strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA)){
    x_det2 = aligninfo->x_det2a;
    y_det2 = aligninfo->y_det2a;

    as_det1x = aligninfo->as_x_det1a;
    as_det1y = aligninfo->as_y_det1a;
  }
  else{
    x_det2 = aligninfo->x_det2b;
    y_det2 = aligninfo->y_det2b;  

    as_det1x = aligninfo->as_x_det1b;
    as_det1y = aligninfo->as_y_det1b;  
  }


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Event Time (seconds since Jan 2010 00:00:00 UTC)", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_OA, CARD_COMM_X_OA, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_OA, CARD_COMM_Y_OA, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DET2X_APSTOP, CARD_COMM_DET2X_APSTOP, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DET2Y_APSTOP, CARD_COMM_DET2Y_APSTOP, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_APSTOP, CARD_COMM_X_APSTOP, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_APSTOP, CARD_COMM_Y_APSTOP, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);


  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OPTICAL_AXIS, CARD_COMM_EXTNAME);

  AddCard(&newhead, KWNM_HDUCLASS, S, attkeys->hduclass, attkeys->hduclass_comm);
  AddCard(&newhead, KWNM_HDUCLAS1, S, attkeys->hduclas1, attkeys->hduclas1_comm);
  AddCard(&newhead, KWNM_TELESCOP, S, KWVL_TELESCOP, CARD_COMM_TELESCOP);
  AddCard(&newhead, KWNM_TIMEPIXR, E, &attkeys->timepixr, attkeys->timepixr_comm);
  AddCard(&newhead, KWNM_OBS_ID, S, attkeys->obs_id, attkeys->obs_id_comm);  
  AddCard(&newhead, KWNM_TARG_ID, J, &attkeys->targ_id, attkeys->targ_id_comm);
  AddCard(&newhead, KWNM_OBJECT, S, attkeys->object, attkeys->object_comm);  
  AddCard(&newhead, KWNM_RA_OBJ, D, &attkeys->raobj, attkeys->raobj_comm);
  AddCard(&newhead, KWNM_DEC_OBJ, D, &attkeys->decobj, attkeys->decobj_comm);
  AddCard(&newhead, KWNM_RA_NOM, D, &attkeys->ranom, attkeys->ranom_comm);
  AddCard(&newhead, KWNM_DEC_NOM, D, &attkeys->decnom, attkeys->decnom_comm);
  AddCard(&newhead, KWNM_RA_PNT, D, &attkeys->rapnt, attkeys->rapnt_comm);
  AddCard(&newhead, KWNM_DEC_PNT, D, &attkeys->decpnt, attkeys->decpnt_comm);
  AddCard(&newhead, KWNM_TIMESYS, S, attkeys->timesys, attkeys->timesys_comm); 
  AddCard(&newhead, KWNM_MJDREFI, J, &attkeys->mjdrefi, attkeys->mjdrefi_comm); 
  AddCard(&newhead, KWNM_MJDREFF, D, &attkeys->mjdreff, attkeys->mjdreff_comm);
  AddCard(&newhead, KWNM_CLOCKAPP, L, &attkeys->clockapp, attkeys->clockapp_comm);
  AddComment(&newhead,"MJDREFI + MJDREFF is the epoch January 1.0, 2010, in the TT time system.");
  AddCard(&newhead, KWNM_TIMEUNIT, S, attkeys->timeunit, attkeys->timeunit_comm);
  AddCard(&newhead, KWNM_TSTART, D, &attkeys->tstart, attkeys->tstart_comm);
  AddCard(&newhead, KWNM_TSTOP, D, &attkeys->tstop, attkeys->tstop_comm);
  AddCard(&newhead, KWNM_DATEOBS, S, attkeys->dateobs, attkeys->dateobs_comm);
  AddCard(&newhead, KWNM_DATEEND, S, attkeys->dateend, attkeys->dateend_comm);


  /* Add INSTRUME */
  if(!strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&newhead, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME); 

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
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.X_OA = GetColNameIndx(&table, CLNM_X_OA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_OA);
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.Y_OA = GetColNameIndx(&table, CLNM_Y_OA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_OA);
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.DET2X_APSTOP = GetColNameIndx(&table, CLNM_DET2X_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET2X_APSTOP);
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.DET2Y_APSTOP = GetColNameIndx(&table, CLNM_DET2Y_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET2Y_APSTOP);
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.X_APSTOP = GetColNameIndx(&table, CLNM_X_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_APSTOP);
      goto WriteOptAxisExt_end;
    }

  if ((indxcol.Y_APSTOP = GetColNameIndx(&table, CLNM_Y_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_APSTOP);
      goto WriteOptAxisExt_end;
    }


  OutRows = 0;
  optcount = 0;
  
  while(optcount<attcount){
    
    for(n=0; (n<BINTAB_ROWS)&&(optcount<attcount); n++,optcount++){
      
      if(ConvertRawToSkyCoords(x_det2, y_det2, seg, mjdref, attinfo[optcount].time, teldef, attfile, &coordset)){
	headas_chat(NORMAL, "%s: Error: TIME %f - Unable to calculate SKY coordinates.\n", global.taskname, attinfo[optcount].time);
	goto WriteOptAxisExt_end;
      }
      
      DVEC(table, n, indxcol.TIME) = attinfo[optcount].time;
      DVEC(table, n, indxcol.X_OA) = coordset.x;
      DVEC(table, n, indxcol.Y_OA) = coordset.y;
      
      headas_chat(CHATTY, "%s: Info: TIME=%f RAWX=%f RAWY=%f X=%f Y=%f\n", global.taskname, attinfo[optcount].time, x_det2, y_det2, coordset.x, coordset.y);
      

      if( mastcount>0 && attinfo[optcount].time<(mastinfo[0].time - TIME_SENS) ){
	headas_chat(NORMAL, "%s: Warning: No mast aspect data for TIME=%f , not computing Aperture Stop columns.\n", global.taskname, attinfo[optcount].time);

	DVEC(table, n, indxcol.DET2X_APSTOP) = fnan;
	DVEC(table, n, indxcol.DET2Y_APSTOP) = fnan;
	DVEC(table, n, indxcol.X_APSTOP) = fnan;
	DVEC(table, n, indxcol.Y_APSTOP) = fnan;

	if(++OutRows>=BINTAB_ROWS)
	  {
	    WriteFastBintable(ounit, &table, OutRows, FALSE);
	    OutRows=0;
	  }

	continue;
      }

      if( (mastend != OK) && (mastcount>0) && (attinfo[optcount].time > mastinfo[mastcount-1].time) ){

	free(mastinfo);
	if(ReadMastAspectSolutionInfo(global.par.mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend)){
	  headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
	  goto WriteOptAxisExt_end;
	}
	mastindex += mastcount;
	mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */
      }

      if(ComputeDET2XY(as_det1x, as_det1y, attinfo[optcount].time, aligninfo, mastinfo, (int)mastcount, &seed, global.par.instrument, &as_det2x, &as_det2y)){
	headas_chat(NORMAL, "%s: Error: TIME=%f DET1X=%d DET1Y=%d - Unable to calculate Aperture Stop DET2 coordinates.\n", global.taskname, attinfo[optcount].time, as_det1x, as_det1y);
	goto WriteOptAxisExt_end;
      }
      headas_chat(CHATTY, "%s: Info: Aperture Stop - TIME=%f DET1X=%d DET1Y=%d => DET2X=%d DET2Y=%d\n", global.taskname, attinfo[optcount].time, as_det1x, as_det1y, as_det2x, as_det2y);

      if(ConvertRawToSkyCoords((double)as_det2x, (double)as_det2y, seg, mjdref, attinfo[optcount].time, teldef, attfile, &coordset)){
	headas_chat(NORMAL, "%s: Error: TIME=%f DET2X=%d DET2Y=%d - Unable to calculate Aperture Stop SKY coordinates.\n", global.taskname, attinfo[optcount].time, as_det2x, as_det2y);
	goto WriteOptAxisExt_end;
      }
      headas_chat(CHATTY, "%s: Info: Aperture Stop - TIME=%f DET2X=%d DET2Y=%d => X=%f Y=%f\n", global.taskname, attinfo[optcount].time, as_det2x, as_det2y, coordset.x, coordset.y);
    
      DVEC(table, n, indxcol.DET2X_APSTOP) = as_det2x;
      DVEC(table, n, indxcol.DET2Y_APSTOP) = as_det2y;
      DVEC(table, n, indxcol.X_APSTOP) = coordset.x;
      DVEC(table, n, indxcol.Y_APSTOP) = coordset.y;
      
      
      if(++OutRows>=BINTAB_ROWS)
	{
	  WriteFastBintable(ounit, &table, OutRows, FALSE);
	  OutRows=0;
	}      
    }
    
  }


  WriteFastBintable(ounit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);


  return OK;
  
 WriteOptAxisExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteOptAxisExt */

int WriteDet1RefFile(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open attitude input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteDet1RefFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteDet1RefFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_RA_NOM, &global.par.pntra, CARD_COMM_RA_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
   
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_NOM, &global.par.pntdec, CARD_COMM_DEC_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);

  /* Add INSTRUME */
  if(!strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&head, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME); 
 
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
      goto WriteDet1RefFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteDet1RefFile_end;
      }
    }

  /* Create 'DET1_REFPOINT' Ext  */
  if (WriteDet1RefExt(attinfo, attcount, mjdref, attkeys, aligninfo, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_DET1_REFPOINT);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
  hducount++;

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 2, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in DET1_REFPOINT HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_RA_NOM, &global.par.pntra, CARD_COMM_RA_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
   
    
  if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_NOM, &global.par.pntdec, CARD_COMM_DEC_NOM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteDet1RefFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteDet1RefFile_end;
    }


  return OK;
  
 WriteDet1RefFile_end:

  return NOT_OK;


} /* WriteDet1RefFile */


int WriteDet1RefExt(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, FitsFileUnit_t ounit){

  int             n, rowcount=0;
  unsigned        OutRows=0;
  int             det1x, det1y;
  int             det2x, det2y;
  int             seg=1;
  long int        seed;
  Coord_t         coordset;
  TELDEF          *teldef;
  ATTFILE         *attfile;
  MastInfo_t      *mastinfo=NULL;
  unsigned        mastindex=1, mastcount=MAST_BLOCK_ROWS;
  BOOL            mastend;
  Det1RefCol_t    indxcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Open Attitude file */
  attfile = openAttFile(global.par.attfile);
  setAttFileInterpolation(attfile, 0);


  /* Open TELDEF file */
  teldef = readTelDef(global.par.teldef);
  setSkyCoordCenterInTeldef(teldef, global.par.pntra, global.par.pntdec);


  /* Retrieve mast aspect solution info from input mastaspectfile */
  if(ReadMastAspectSolutionInfo(global.par.mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
      goto WriteDet1RefExt_end;
    }
  mastindex += mastcount;
  mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Event Time (seconds since Jan 2010 00:00:00 UTC)", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_DET1, CARD_COMM_X_DET1, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_DET1, CARD_COMM_Y_DET1, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_DET1_REFPOINT, CARD_COMM_EXTNAME);

  AddCard(&newhead, KWNM_HDUCLASS, S, attkeys->hduclass, attkeys->hduclass_comm);
  AddCard(&newhead, KWNM_HDUCLAS1, S, attkeys->hduclas1, attkeys->hduclas1_comm);
  AddCard(&newhead, KWNM_TELESCOP, S, KWVL_TELESCOP, CARD_COMM_TELESCOP);
  AddCard(&newhead, KWNM_TIMEPIXR, E, &attkeys->timepixr, attkeys->timepixr_comm);
  AddCard(&newhead, KWNM_OBS_ID, S, attkeys->obs_id, attkeys->obs_id_comm);  
  AddCard(&newhead, KWNM_TARG_ID, J, &attkeys->targ_id, attkeys->targ_id_comm);
  AddCard(&newhead, KWNM_OBJECT, S, attkeys->object, attkeys->object_comm);  
  AddCard(&newhead, KWNM_RA_OBJ, D, &attkeys->raobj, attkeys->raobj_comm);
  AddCard(&newhead, KWNM_DEC_OBJ, D, &attkeys->decobj, attkeys->decobj_comm);
  AddCard(&newhead, KWNM_RA_NOM, D, &attkeys->ranom, attkeys->ranom_comm);
  AddCard(&newhead, KWNM_DEC_NOM, D, &attkeys->decnom, attkeys->decnom_comm);
  AddCard(&newhead, KWNM_RA_PNT, D, &attkeys->rapnt, attkeys->rapnt_comm);
  AddCard(&newhead, KWNM_DEC_PNT, D, &attkeys->decpnt, attkeys->decpnt_comm);
  AddCard(&newhead, KWNM_TIMESYS, S, attkeys->timesys, attkeys->timesys_comm); 
  AddCard(&newhead, KWNM_MJDREFI, J, &attkeys->mjdrefi, attkeys->mjdrefi_comm); 
  AddCard(&newhead, KWNM_MJDREFF, D, &attkeys->mjdreff, attkeys->mjdreff_comm);
  AddCard(&newhead, KWNM_CLOCKAPP, L, &attkeys->clockapp, attkeys->clockapp_comm);
  AddComment(&newhead,"MJDREFI + MJDREFF is the epoch January 1.0, 2010, in the TT time system.");
  AddCard(&newhead, KWNM_TIMEUNIT, S, attkeys->timeunit, attkeys->timeunit_comm);
  AddCard(&newhead, KWNM_TSTART, D, &attkeys->tstart, attkeys->tstart_comm);
  AddCard(&newhead, KWNM_TSTOP, D, &attkeys->tstop, attkeys->tstop_comm);
  AddCard(&newhead, KWNM_DATEOBS, S, attkeys->dateobs, attkeys->dateobs_comm);
  AddCard(&newhead, KWNM_DATEEND, S, attkeys->dateend, attkeys->dateend_comm);


  /* Add INSTRUME */
  if(!strcasecmp(global.par.instrument,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&newhead, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME); 

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
      goto WriteDet1RefExt_end;
    }

  if ((indxcol.X_DET1 = GetColNameIndx(&table, CLNM_X_DET1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_DET1);
      goto WriteDet1RefExt_end;
    }

  if ((indxcol.Y_DET1 = GetColNameIndx(&table, CLNM_Y_DET1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_DET1);
      goto WriteDet1RefExt_end;
    }


  /* initialize random number generator */
  if(global.par.initseed){
    seed = -345337518;
  }
  else{
    get_ran2seed(&seed);
  }

  det1x = global.par.det1xref;
  det1y = global.par.det1yref;

  OutRows = 0;
  rowcount = 0;

  while(rowcount<attcount){
    
    for(n=0; (n<BINTAB_ROWS)&&(rowcount<attcount); n++,rowcount++){
      
      if( mastcount>0 && attinfo[rowcount].time<(mastinfo[0].time - TIME_SENS) ){
	headas_chat(NORMAL, "%s: Warning: No mast aspect data for TIME=%f excluding this row from DET2 calculation\n", global.taskname, attinfo[rowcount].time);
	n--;
	continue;
      }

      if( (mastend != OK) && (mastcount>0) && (attinfo[rowcount].time > mastinfo[mastcount-1].time) ){

	free(mastinfo);
	if(ReadMastAspectSolutionInfo(global.par.mastaspectfile, &mastinfo, mastindex, &mastcount, &mastend)){
	  headas_chat(NORMAL, "%s: Error: Unable to read mast aspect solution file.\n", global.taskname);
	  goto WriteDet1RefExt_end;
	}
	mastindex += mastcount;
	mastindex--;  /* the last row will be included in the next 'ReadMastAspectSolutionInfo' call */
      }

      if(ComputeDET2XY(det1x, det1y, attinfo[rowcount].time, aligninfo, mastinfo, (int)mastcount, &seed, global.par.instrument, &det2x, &det2y)){
	headas_chat(NORMAL, "%s: Error: TIME=%f DET1X=%d DET1Y=%d - Unable to calculate DET2 coordinates.\n", global.taskname, attinfo[rowcount].time, det1x, det1y);
	goto WriteDet1RefExt_end;
      }
      headas_chat(CHATTY, "%s: Info: TIME=%f DET1X=%d DET1Y=%d => DET2X=%d DET2Y=%d\n", global.taskname, attinfo[rowcount].time, det1x, det1y, det2x, det2y);

      if(ConvertRawToSkyCoords((double)det2x, (double)det2y, seg, mjdref, attinfo[rowcount].time, teldef, attfile, &coordset)){
	headas_chat(NORMAL, "%s: Error: TIME=%f DET2X=%d DET2Y=%d - Unable to calculate SKY coordinates.\n", global.taskname, attinfo[rowcount].time, det2x, det2y);
	goto WriteDet1RefExt_end;
      }
      headas_chat(CHATTY, "%s: Info: TIME=%f DET2X=%d DET2Y=%d => X=%f Y=%f\n", global.taskname, attinfo[rowcount].time, det2x, det2y, coordset.x, coordset.y);
    
      DVEC(table, n, indxcol.TIME) = attinfo[rowcount].time;
      DVEC(table, n, indxcol.X_DET1) = coordset.x;
      DVEC(table, n, indxcol.Y_DET1) = coordset.y;
      
      if(++OutRows>=BINTAB_ROWS)
	{
	  WriteFastBintable(ounit, &table, OutRows, FALSE);
	  OutRows=0;
	}      
    }
    
  }


  WriteFastBintable(ounit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);


  return OK;
  
 WriteDet1RefExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteDet1RefExt */


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
  MastExtCol_t       indxcol;
  Bintable_t	     table;
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
  if(!strcasecmp(instr,KWVL_INSTRUME_FPMA)){
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


