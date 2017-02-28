/*
 * 
 *	xrtexpocorr.c:
 *
 *	INVOCATION:
 *
 *		xrtexpocorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *           
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 06/10/2005 - First version
 *        0.1.1 -    11/10/2005 - Added routine to update checksum
 *        0.1.2 -    17/10/2005 - Added 'XRTVIGCO' keyword
 *        0.1.3 -    26/10/2005 - replaced  XRTVIGCO with VIGNAPP
 *        0.1.4 -               - Added XRTVIGEN keyword
 *        0.1.5 -    06/02/2006 - Added XRTVIGEN keyword in the corrected image
 *        0.1.6 - NS 12/02/2007 - Applies the vignetting correction to the raw instrument map
 *                                instead to the sky instrument map
 *
 *      NOTE:
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtexpocorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtexpocorr.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTEXPOCORR_C
#define XRTEXPOCORR_VERSION       "0.1.6"
#define PRG_NAME               "xrtexpocorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtexpocorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtexpocorr.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetBool(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void xrtexpocorr_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 30/08/2005 - First version
 *                                 
 *
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtexpocorr_getpar(void)
{
  /*
   *  Get File name Parameters
   */

 
  /* Input Photon-Counting Event List Files Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  /* Input Photon-Counting Event List Files Name */
  if(PILGetFname(PAR_VIGFILE, global.par.vigfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGFILE);
      goto Error;
    }

  /* Output Event List File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  if(PILGetReal(PAR_ENERGY, &global.par.energy)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ENERGY);
      goto Error;	
    }

  
  get_history(&global.hist);
  xrtexpocorr_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtexpocorr_getpar */

/*
 *	xrtexpocorr_work
 *
 *
 *	DESCRIPTION:
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           char * strtok(char *, char *);
 *           FitsFileUnit_t OpenReadFitsFile(char *name);
 *           int headas_chat(int , char *, ...);
 *           int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *           int CloseFitsFile(FitsFileUnit_t file);
 *           BOOL KeyWordMatch( FitsFileUnit_t unit, const char *KeyWord, ValueTag_t vtag, 
 *                              const void *NominalValue, void *ActValue);
 *           FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *           SPTYPE GetSVal(const FitsHeader_t *header, const char *KeyWord);
 *           char * strcpy(char *, char *);
 *           int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *           int strcasecmp(const char *s1, const char *s2);
 *           char * SplitFilePath(const char *FilePath, char *DirName, char *BaseName);
 *           char * StripExtension(char * filename);
 *           char *strcat(char *dest, const char *src);
 *           BOOL FileExists(const char *FileName);
 *           FitsFileUnit_t OpenWriteFitsFile(char *name);
 *           int remove(const char *pathname);
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype,
 *                               int *status ); 
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status); 
 *           int fits_update_key(fitsfile *fptr, int datatype, char *keyname, DTYPE *value,
 *                               char *comment, int *status);  
 *           int AddEventBintable(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 29/08/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtexpocorr_work(void)
{

  int            inExt=0, status=OK, outExt, type, logical; 
  FitsFileUnit_t inunit=NULL, outunit=NULL;

  if(xrtexpocorr_checkinput())
    {
      goto Error;
    }

  if(VignettingCorr())
    {
      headas_chat(NORMAL,"%s: Error: Unable to calculate Vignetting Correction Coefficients\n", global.taskname);
      goto Error;

    }


  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  ISXRTFILE_WARNING(NORMAL,inunit,global.par.infile,global.taskname);

  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
      
    }

  /* Create output file */
  if ((outunit = OpenWriteFitsFile(global.par.outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' output file. \n", global.taskname, global.par.outfile);
      goto Error;
    }

  outExt=1;
  /* Process all image extensions */
  while(outExt <= inExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, &type, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      
      if(outExt==1)
	{
	  /* Copy primary empty hdu */
	  if(fits_copy_hdu(inunit, outunit, 0, &status))
	    { 
	      headas_chat(CHATTY,"%s: Error: Unable to copy Primary HDU\n",global.taskname);
	      headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	      headas_chat(CHATTY,"%s: Error: to %s output file.\n",global.taskname, global.par.outfile);
	      goto Error; 
	    }
	  logical=TRUE;
	  if(fits_update_key(outunit, TLOGICAL, KWNM_VIGNAPP, &logical, CARD_COMM_VIGNAPP, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
	      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	      goto Error;
	    }

  
	  if(fits_update_key(outunit, TDOUBLE, KWNM_XRTVIGEN, &global.par.energy, CARD_COMM_XRTVIGEN, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTVIGEN);
	      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	      goto Error;
	    }

	} 
      else
	{
	  /* Get Image dimension from header */
	  if (CorrImage(inunit, outunit, outExt))
	    {
	      headas_chat(MUTE, "%s: Error: Unable to process %d HDU\n", global.taskname, (outExt - 1));
	      headas_chat(MUTE, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	      
	    }
	}
      outExt++;
    }

/* close files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname,global.par.infile);
      goto Error;
    }  


/* Calculate checksum and add it in file */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto Error;
    }



  if (!CloseFitsFile(outunit)) 
    {
      headas_chat(MUTE,"%s: Info: File '%s' successfully written.\n",global.taskname,
		  global.par.outfile);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname,global.par.outfile);
      goto Error;
    }

  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");


  return OK;

 Error:


  return NOT_OK;
} /* xrtexpocorr_work */


/*
 *	xrtexpocorr
 *
 *
 *	DESCRIPTION:
 *        
 *                Routine to assign grades to faint mode data
 * 		  Based on the CLASSIFY program
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolname(char *);
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *
 *             void xrtexpocorr_getpar(void);
 *             void xrtexpocorr_info(void); 
 * 	       void xrtexpocorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtexpocorr(void)
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTEXPOCORR_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  
  /* Get parameter values */ 
  if ( xrtexpocorr_getpar() == OK ) {
    
    if ( xrtexpocorr_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto expocalc_end;
    }
    /*else if(global.warning)
      {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Warning.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      
      }
    */
    
  }
  else
    goto expocalc_end;
  
  return OK;
  
 expocalc_end:

  
  return NOT_OK;


} /* xrtexpocorr */
/*
 *	xrtexpocorr_info: 
 *
 *
 *	DESCRIPTION:
 *         Display information about input parameter.
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtexpocorr_info (void)
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file             :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file            :'%s'\n",global.par.outfile);
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                   : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                   : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtexpocorr_info */

int ReadInputImage(void)
{
  int                   status=OK, anynull=0;
  long                  dimen;
  /*long                  group=0, firstelem=0, nelements=0;*/
  /*double                nulval;*/
  FitsCard_t            *card;
  FitsHeader_t          head;  
  FitsFileUnit_t        inunit=NULL; 


  /* Open read only input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
    
  
  dimen=CCD_PIXS*CCD_ROWS;
  fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, global.data[0], &anynull, 
                  &status);

  head=RetrieveFitsHeader(inunit);

 /************************************
   *  Read keywords                   *
   ************************************/

  /* if((ExistsKeyWord(&head, KWNM_OBS_ID  , &card)))
     {
     strcpy(global.key.obsid, card->u.SVal);
     }
     else
     {
     headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
     headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
     goto end_func;
     }
  

  if((ExistsKeyWord(&head, KWNM_MJDREFI  , &card)))
    {
      global.key.mjdrefi=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_MJDREFF  , &card)))
    {
      global.key.mjdreff=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_TARG_ID  , &card)))
    {
      global.key.target=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TARG_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_SEG_NUM  , &card)))
    {
      global.key.segnum=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_SEG_NUM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&head, KWNM_RA_OBJ  , &card)))
    {
      global.key.raobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_DEC_OBJ  , &card)))
    {
      global.key.decobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_RA_PNT  , &card)))
    {
      global.key.rapnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&head, KWNM_DEC_PNT  , &card)))
    {
      global.key.decpnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  */
  if((ExistsKeyWord(&head, KWNM_XRTFRTIM  , &card)))
    {
      global.key.timedel=card->u.DVal;
      
    }
  else
    {
      headas_chat(NORMAL, "%s: Info: %s keyword not found\n", global.taskname,KWNM_XRTFRTIM);
      headas_chat(NORMAL, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
      headas_chat(NORMAL, "%s: Info: using %f default value.\n", global.taskname, PC_TIMEDEL);
      global.key.timedel=PC_TIMEDEL;
    }

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);

  /* Close input event file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

    
  return OK; 
   
 end_func:
  return NOT_OK;
}/* ReadEventFile */

int xrtexpocorr_checkinput(void)
{

  if ( FileExists(global.par.outfile) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outfile);
    if ( !headas_clobpar ) {
      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
      goto error_check;
    }
    else
      {
	headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	if(remove (global.par.outfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	    goto error_check;
	  }  
      }
  }
  
  /*
    if (!(strcasecmp (global.par.outbpfile, DF_DEFAULT)))
    {
    strcpy(global.par.outbpfile, global.par.outfile);
    StripExtension(global.par.outbpfile);
    strcat(global.par.outbpfile, BPFITSEXT);
    headas_chat(NORMAL, "%s: Info: Name for the bad pixels file is:\n",global.taskname);
    headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outbpfile);
    }
  
    if ( FileExists(global.par.outbpfile) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outbpfile);
    if ( !headas_clobpar ) {
    headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outbpfile);
    headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
    goto error_check;
    }
    else
    { 
    headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
    headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outbpfile);
    if(remove (global.par.outbpfile) == -1)
    {
    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outbpfile);
    goto error_check;
    } 
    }
    }
  */
  
  return OK;

 error_check:
  return NOT_OK;


}/* xrtexpocorr_checkinput */
int AddKey2ExpoMap(FitsFileUnit_t inunit)
{
  int    status=OK;
  int    tmp_int=0;
  double tmp_dbl=0.0;

  if(fits_update_key(inunit, TSTRING, KWNM_TELESCOP,  KWVL_TELESCOP,CARD_COMM_TELESCOP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_INSTRUME,  KWVL_INSTRUME,CARD_COMM_INSTRUME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE,KWNM_MJDREFI , &global.key.mjdrefi,CARD_COMM_MJDREFI, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE,KWNM_MJDREFF , &global.key.mjdreff,CARD_COMM_MJDREFF, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  /*
    if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID,  global.key.obsid,CARD_COMM_OBS_ID, &status))
    {
    headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
    headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
    goto add_end;
    }
  */
  if(fits_update_key(inunit, TINT,KWNM_TARG_ID , &global.key.target,CARD_COMM_TARG_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TARG_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  if(fits_update_key(inunit, TINT,KWNM_SEG_NUM , &global.key.segnum,CARD_COMM_SEG_NUM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_SEG_NUM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_OBJ,  &global.key.raobj,CARD_COMM_RA_OBJ, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_OBJ);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_OBJ,  &global.key.decobj,CARD_COMM_DEC_OBJ, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_OBJ);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_PNT,  &global.key.rapnt,CARD_COMM_RA_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_PNT,  &global.key.decpnt,CARD_COMM_DEC_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  if(fits_update_key(inunit, TDOUBLE, KWNM_XRTFRTIM,  &global.key.timedel,CARD_COMM_XRTFRTIM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XRTFRTIM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  /*
    if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &tmp_dbl,CARD_COMM_TSTART, &status))
    {
    headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
    headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
    goto add_end;
    }
    tmp_dbl=global.hkinfo[jj].hkendtime;
    if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &tmp_dbl,CARD_COMM_TSTOP, &status))
    {
    headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
    headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
    goto add_end;
    }
  */

  if(fits_update_key(inunit, TSTRING, KWNM_CTYPE1,  KWVL_CTYPE1_RAW,CARD_COMM_CTYPE1, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CTYPE1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  tmp_dbl=300.0;
  if(fits_update_key(inunit, TDOUBLE, KWNM_CRPIX1, &tmp_dbl ,CARD_COMM_CRPIX1, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRPIX1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TSTRING, KWNM_CUNIT1, UNIT_PIXEL,CARD_COMM_CUNIT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CUNIT1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
    }
  tmp_int=299;
  if(fits_update_key(inunit, TINT, KWNM_CRVAL1, &tmp_int,CARD_COMM_CRVAL1, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRVAL1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  tmp_int=1;
  if(fits_update_key(inunit, TINT, KWNM_CDELT1, &tmp_int,CARD_COMM_CDELT1, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CDELT1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
	}
  
  
  if(fits_update_key(inunit, TSTRING, KWNM_CTYPE2,  KWVL_CTYPE2_RAW,CARD_COMM_CTYPE2, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CTYPE2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
    }
  tmp_dbl=300.0;
  if(fits_update_key(inunit, TDOUBLE, KWNM_CRPIX2, &tmp_dbl,CARD_COMM_CRPIX2, &status))
    {
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRPIX2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
    }
  
  if(fits_update_key(inunit, TSTRING, KWNM_CUNIT2, UNIT_PIXEL,CARD_COMM_CUNIT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CUNIT2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
    }
  tmp_int=299;
  if(fits_update_key(inunit, TINT, KWNM_CRVAL2, &tmp_int,CARD_COMM_CRVAL2, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRVAL2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  tmp_int=1;
  if(fits_update_key(inunit, TINT, KWNM_CDELT2, &tmp_int,CARD_COMM_CDELT2, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CDELT2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  return OK;
  
 add_end:
      return NOT_OK;

}


/******************************************************************************************/
/*
 *	vig_corr
 *
 *
 *	DESCRIPTION:
 *           
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *    
 *         double pow(double x, double y);  
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *       
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 */

int VignettingCorr(void)
{    

  int                   status = OK , hdutype=0;
  int                   xx, yy;
  long                  extno=-1;
  double                par0=0.0,par1=0.0,par2=0.0,vcoeff=0.0;
  double                dx, dy, off;
  unsigned              FromRow, ReadRows, n, nCols;
  VigCol_t              vigcol; 
  Bintable_t	        table;
  FitsHeader_t	        head;
  FitsFileUnit_t        inunit=NULL; 

  /* read the coefficient  from vigne CALDB file */
  extno=-1;

  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.vigfile, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_VIGNET_DSET, global.par.vigfile, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto vig_end;
	}
      extno++;
    }
 
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.vigfile);


  if ((inunit=OpenReadFitsFile(global.par.vigfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.vigfile);
      goto vig_end;
    }

  if (extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(inunit, extno, &hdutype, &status) ) /* move to 2nd HDU */
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.vigfile);
	  goto vig_end;
	}
    }
  else
    {
      if (fits_movnam_hdu(inunit,ANY_HDU,KWVL_EXTNAME_VIGCOEF,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_VIGCOEF);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.vigfile);
	  goto vig_end;
	} 
    }


  head=RetrieveFitsHeader(inunit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.vigfile);
      goto vig_end;
    }
  
  if ((vigcol.PAR0=ColNameMatch(CLNM_PAR0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto vig_end;
    }

  
  if ((vigcol.PAR1=ColNameMatch(CLNM_PAR1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto vig_end;
    }

  if ((vigcol.PAR2=ColNameMatch(CLNM_PAR2, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto vig_end;
    }

  EndBintableHeader(&head, &table);

  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  while(ReadBintable(inunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  par0=(double)EVEC(table, n, vigcol.PAR0);
	  par1=(double)EVEC(table, n, vigcol.PAR1);
	  par2=(double)EVEC(table, n, vigcol.PAR2);
	  
	}
	      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
   ReleaseBintable(&head, &table);

   /* Close input event file */
   if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.vigfile);
      goto vig_end;
    }

   vcoeff = par0*pow(par1,global.par.energy)+par2;

   for (xx=0; xx<CCD_PIXS; xx++)
     for(yy=0; yy<CCD_ROWS; yy++)
       {
	 dx=xx - CCD_CENTERX;
	 dy=yy - CCD_CENTERY;

	 off=(sqrt(dx*dx + dy*dy))*(PIXEL_SIZE)/60.0;
	 global.vcorr[yy][xx]=(float)(1.-vcoeff*(off)*(off));
	 
	 if (global.vcorr[yy][xx] < 0.)  
	   global.vcorr[yy][xx]=0.;
       }
    
  
  return OK;

 vig_end:
  
  return NOT_OK;


} /* vig_corr */




int CorrImage(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int ext)
{
  int            status=OK, logical=0;
  int            anynull=0, xx, yy;
  long           dimen, group=0, firstelem=0, nelements=0;
  FitsHeader_t   head;
 

  TMEMSET0( &head, FitsHeader_t );
 
  /*
   *	read the map
   */
  
  dimen=CCD_PIXS*CCD_ROWS;
  if(fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, global.data[0], &anynull, 
		       &status))
    {
      headas_chat(MUTE, "%s: Error: Unable to read %d image\n", global.taskname, (ext-1));
      headas_chat(MUTE, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto corr_end;
    }


  for (xx=0; xx<CCD_PIXS; xx++)
    for(yy=0; yy<CCD_ROWS; yy++)
      global.data[yy][xx]=global.data[yy][xx]*global.vcorr[yy][xx];

    /* Write array */

  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY,"%s: Error: Unable to copy %d header\n", global.taskname, (ext -1));
      headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: to %s output file.\n",global.taskname, global.par.outfile);
      goto corr_end;
    }

  group=0;
  firstelem=1;
  nelements=CCD_PIXS*CCD_ROWS;
  
  if(fits_write_img_flt(outunit, 0l, 1l, nelements, global.data[0], &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %d image extension\n", global.taskname, (ext - 1));
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }


  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_VIGNAPP, &logical, CARD_COMM_VIGNAPP, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }

  
  if(fits_update_key(outunit, TDOUBLE, KWNM_XRTVIGEN, &global.par.energy, CARD_COMM_XRTVIGEN, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTVIGEN);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }

  
  return OK;
  
 corr_end:
  return NOT_OK;
}/* CorrImage */
