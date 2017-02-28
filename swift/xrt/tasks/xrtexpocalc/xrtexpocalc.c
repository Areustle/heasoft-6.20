/*
 * 
 *	xrtexpocalc.c:
 *
 *	INVOCATION:
 *
 *		xrtexpocalc [parameter=value ...]
 *
 *	DESCRIPTION:
 *           
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 27/09/2005 - First version
 *        0.1.1 -    06/10/2005 - Bugs fixed
 *        0.1.2 -    11/10/2005 - Copy keywords from input file
 *                                to output file
 *        0.1.3 -    26/10/2005 - Deleted TIMEDEL keyword
 *        0.1.4 -               - Used XRTFRTIM instead of TIMEDEL
 *      NOTE:
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtexpocalc  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtexpocalc.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTEXPOCALC_C
#define XRTEXPOCALC_VERSION       "0.1.4"
#define PRG_NAME               "xrtexpocalc"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtexpocalc_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtexpocalc.par    
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
 *           void xrtexpocalc_info(void);
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
int xrtexpocalc_getpar(void)
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
  

  /* Output Event List File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  
  get_history(&global.hist);
  xrtexpocalc_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtexpocalc_getpar */

/*
 *	xrtexpocalc_work
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
int xrtexpocalc_work(void)
{

  FitsFileUnit_t        inunit=NULL; 

  if(xrtexpocalc_checkinput())
    {
      goto Error;
    }


  /* Open read only input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  if(ReadInputImage(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }



  if(ExpoCalc(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.outfile);
      goto Error;

    }


  /* Close input event file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }




  /* Check readout mode of input events file */

  headas_chat(NORMAL,"%s: Info:File  %s successfully written.\n", global.taskname, global.par.outfile);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");


  return OK;

 Error:


  return NOT_OK;
} /* xrtexpocalc_work */


/*
 *	xrtexpocalc
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
 *             void xrtexpocalc_getpar(void);
 *             void xrtexpocalc_info(void); 
 * 	       void xrtexpocalc_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtexpocalc(void)
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTEXPOCALC_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  
  /* Get parameter values */ 
  if ( xrtexpocalc_getpar() == OK ) {
    
    if ( xrtexpocalc_work() ) {
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


} /* xrtexpocalc */
/*
 *	xrtexpocalc_info: 
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
void xrtexpocalc_info (void)
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
} /* xrtexpocalc_info */

int ReadInputImage(FitsFileUnit_t inunit)
{
  int                   status=OK, anynull=0;
  long                  dimen;
  FitsCard_t            *card;
  FitsHeader_t          head;  

  /*
    for (xx=0; xx< SIZE_X; xx++)
    for(yy=0; yy<SIZE_Y; yy++)
    global.data[yy][xx]=0;
  */
  /* Get number of hdus in input events file */
  /*
    if (fits_get_num_hdus(inunit, &tot_hdu, &status))
    {
    headas_chat(CHATTY, "%s: Error: getting hdus num in '%s' file.\n",global.taskname, global.par.infile);
    goto end_func;
    }
  */
  dimen=SIZE_X*SIZE_Y;

  if(fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, global.data[0], &anynull, 
		       &status))
    {
      headas_chat(MUTE, "%s: Error: Unable to read image\n", global.taskname);
      headas_chat(MUTE, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }




  /*  for (ii=2; ii <=  tot_hdu; ii++)
    {
  

    if(fits_movabs_hdu( inunit, 2, NULL, &status ))
    {
    headas_chat(CHATTY, "%s: Error: unable to move in hdu %d in '%s' file. \n", global.taskname, tot_hdu, global.par.infile);
    goto end_func;
    }
    
    
    for (xx=0; xx< SIZE_X; xx++)
    for(yy=0; yy<SIZE_Y; yy++)
    global.data[yy][xx]+=tmp_data[yy][xx];
    
    printf("GLOBAL == %f TMP == %f \n", global.data[300][300], tmp_data[300][300]);
    printf("HDU == %d\n", ii);
    
    }
    
    if(fits_movabs_hdu( inunit, 2, NULL, &status ))
    {
    headas_chat(CHATTY, "%s: Error: unable to move in hdu %d in '%s' file. \n", global.taskname, tot_hdu, global.par.infile);
    goto end_func;
    }

  */
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

  /*
    for (xx=0; xx< SIZE_X; xx++)
    for(yy=0; yy<SIZE_Y; yy++)
    {
    printf("%f ", global.data[yy][xx]);
    ii++;
    }
    printf(">>>>>>>>>>> %d \n", ii);
  */
    
  return OK; 
   
 end_func:
  return NOT_OK;
}/* ReadEventFile */


int ExpoCalc(FitsFileUnit_t inunit)
{
  int                 status = OK;
  int                 xx=0, yy=0;
  long                group, firstelem, nelements;
  FitsFileUnit_t      outunit=NULL;              /* Input and Output fits file pointer */ 

  if(fits_create_file(&outunit, global.par.outfile, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to copy keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: from '%s' input file to\n", global.taskname, global.par.infile);
      headas_chat(NORMAL,"%s: Error: '%s' output file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }


  /*  bitpix=8;
      naxis=0;
      naxes[0]=0;
      naxes[1]=0;
      
      if(fits_create_img(outunit, bitpix, naxis, naxes, &status))
      {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto create_end;
      }*/

  /*
    if(AddSwiftXRTKeyword(outunit, bitpix, amp,jj))
    {
    headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
    headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
    goto create_end;
    }
  */



 for (xx=0; xx< SIZE_X; xx++)
    for(yy=0; yy<SIZE_Y; yy++)
      global.data[yy][xx]=global.data[yy][xx]*global.key.timedel;

 /*
   for (xx=0; xx< SIZE_X; xx++)
   for(yy=0; yy<SIZE_Y; yy++)
   {
   printf("%f ", global.data[yy][xx]);
   
   }
 */

 /*
   bitpix=-32;
   naxis=2;
   naxes[0]=SIZE_X;
   naxes[1]=SIZE_Y;
   
   if(fits_create_img(outunit, bitpix, naxis, naxes, &status))
   {
   headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
   headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
   goto create_end;
   }
 */
  group=0;
  firstelem=1;
  nelements=SIZE_X*SIZE_Y;
  
  if(fits_write_img_flt(outunit, 0l, 1l, nelements, global.data[0], &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write image extension\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto create_end;
    }

  /*    
	if(AddSwiftXRTKeyword(outunit, bitpix, amp, jj))
	{
	headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
	headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	goto create_end;
	}
  */

/* Calculate checksum and add it in file */

  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto create_end;
    }
  

  if(fits_close_file(outunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto create_end;
    }
  
      
  return OK;
 create_end:
  return NOT_OK;
  
}
int xrtexpocalc_checkinput(void)
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


}/* xrtexpocalc_checkinput */
