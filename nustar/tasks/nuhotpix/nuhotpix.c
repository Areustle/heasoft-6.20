/*
 * 
 *	nuhotpix.c
 *
 *	INVOCATION:
 *
 *		nuhotpix [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine that searches, identifies and flags hot pixels
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 01/09/11 - First version
 *        0.1.1 - NS 03/10/11 - Removed 'phamin' and 'phamax' input parameters
 *                            - Removed 'gradeiterate' input parameter
 *        0.1.2 - NS 10/10/11 - Bug fixed on 'outhpfile' default name handling
 *                            - Bug fixed when 'outfile' equal to 'infile' parameter 
 *        0.1.3 - NS 09/05/12 - Handle new keywords in BADPIX extension
 *        0.1.4 - NS 20/07/12 - Bug fixed when creating 'outhpfile'
 *        0.1.5 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.6 - RF 05/04/16 - Modified call 'CreateBPFile' and 'CreateBPExt' function
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuhotpix  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuhotpix.h"


/********************************/
/*         definitions          */
/********************************/

#define NUHOTPIX_C
#define NUHOTPIX_VERSION      "0.1.6"
#define PRG_NAME              "nuhotpix"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuhotpix_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuhotpix.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuhotpix_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 01/09/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuhotpix_getpar()
{
  int    tmp=0;
  double sens=0.0000001; 
  
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

  /* Output Hot Pixel File Name */
  if(PILGetFname(PAR_OUTHPFILE, global.par.outhpfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTHPFILE);
      goto Error;
    }
  if ( !strcasecmp(global.par.outhpfile,DF_NONE) )
    global.outhpfile = FALSE;
  else
    global.outhpfile = TRUE;
  
  /* Binsize */
  if(PILGetReal(PAR_BINSIZE, &global.par.binsize)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BINSIZE);
      goto Error;
    }

  /*  */ 
/*   if(PILGetBool(PAR_GRADEITERATE, &global.par.gradeiterate)) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_GRADEITERATE); */
/*       goto Error; */
/*     } */

  /* cellsize dimension */
   if(PILGetInt(PAR_CELLSIZE, &global.par.cellsize))
     {headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_CELLSIZE);
       goto Error;
     }

   if (global.par.cellsize <= 1)
     {
       headas_chat(NORMAL, "%s: Error: '%s' parameter value is not valid.\n", global.taskname, PAR_CELLSIZE);
       headas_chat(NORMAL, "%s: Error: '%s' parameter value must be > 1.\n", global.taskname, PAR_CELLSIZE);
       goto Error;
     }
   else
     {
       
       tmp=global.par.cellsize%2;
       if (tmp == 0)
	 {
	   headas_chat(NORMAL, "%s: Error: '%s' parameter value is not valid.\n", global.taskname, PAR_CELLSIZE);
	   headas_chat(NORMAL, "%s: Error: '%s' parameter must be odd.\n", global.taskname, PAR_CELLSIZE);
	   goto Error;
	 }
     }

   /*  */ 
   if(PILGetReal(PAR_IMPFAC, &global.par.impfac))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_IMPFAC);
       goto Error;
     }

   /*  */ 
   if(PILGetReal(PAR_LOGPOS, &global.par.logpos))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_LOGPOS);
       goto Error;
     }

   if(global.par.logpos > (0.0 - sens))
     {
       headas_chat(NORMAL, "%s: Error: '%s' parameter value is not valid.\n", global.taskname, PAR_LOGPOS);
       headas_chat(NORMAL, "%s: Error: '%s' parameter value must be < 0.\n", global.taskname, PAR_LOGPOS);
       goto Error;
     }

   /*  */ 
   if(PILGetInt(PAR_BTHRESH, &global.par.bthresh))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_BTHRESH);
       goto Error;
     }

   /* Clean flickering pixels */ 
   if(PILGetBool(PAR_CLEANFLICK, &global.par.cleanflick))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_CLEANFLICK);
       goto Error;
     }

   /* Iterate the Poisson clean */ 
   if(PILGetBool(PAR_ITERATE, &global.par.iterate))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ITERATE);
       goto Error;
     }

   
  get_history(&global.hist);
  nuhotpix_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuhotpix_getpar */


/*
 *	nuhotpix_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuhotpix_checkinput();
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
 *        0.1.0 - NS 01/09/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuhotpix_work()
{
  int            status = OK, inExt, outExt, evExt, inExtNum;
  int            bpExt0=-1, bpExt1=-1, bpExt2=-1, bpExt3=-1;
  int            i=0;
  static BadPixExtData_t   bp[4][DET_PIXS][DET_ROWS];
  static BadPixExtData_t   hp[4][DET_PIXS][DET_ROWS];
  BadPixKeys_t   bpkeys;
  FitsHeader_t   head;
  FitsCard_t     *card;
  FitsFileUnit_t inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  unsigned short int nobadflag=0;

  nobadflag |= OBS_HOTFLICK_BP;

  if(nuhotpix_checkinput())
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
      strcpy(bpkeys.instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve OBS_ID from input event file */  
  if(ExistsKeyWord(&head, KWNM_OBS_ID, &card))
    {
      strcpy(bpkeys.obs_id, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve ORIGIN from input event file */  
  if(ExistsKeyWord(&head, KWNM_ORIGIN, &card))
    {
      strcpy(bpkeys.origin, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ORIGIN);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* MJDREFI */
  if((ExistsKeyWord(&head, KWNM_MJDREFI, &card)))
    {
      bpkeys.mjdrefi = card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* MJDREFF */
  if((ExistsKeyWord(&head, KWNM_MJDREFF, &card)))
    {
      bpkeys.mjdreff = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
 
  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      bpkeys.tstart = card->u.DVal;
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
      bpkeys.tstop = card->u.DVal;
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

  /* Read Event BADPIX extensions */
  if(ReadEvtBPdata(bp,&bpExt0,&bpExt1,&bpExt2,&bpExt3))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get bad pixels data from input event file\n", global.taskname);
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
  if (fits_get_num_hdus(inunit, &inExtNum, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extension before event extension  from input to output file */
  inExt=1;
  outExt=1;
    
  while(outExt<evExt && status == OK)
    {
      if(inExt!=bpExt0 && inExt!=bpExt1 && inExt!=bpExt2 && inExt!=bpExt3)
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
	  inExt++;
	  outExt++;
	}
      else
	{
	  inExt++;	  
	}
    }
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }

 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);


  /* Search Hot/Flickering pixels */
  if (SearchHotPix(inunit, hp))
    {
      headas_chat(NORMAL, "%s: Error: Unable to search hot/flickering pixels.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;

  /* Flags Hot/Flickering pixels */
  if (HotPixFlag(inunit, outunit, hp))
    {
      headas_chat(NORMAL, "%s: Error: Unable to flag hot/flickering pixels.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;

  
  /* Update bad pixels array with hot/flickering pixels data */
  for(i=0; i<4; i++){
    if(UpdateBPExtWithBadPixExtData(hp[i],bp[i])){
      headas_chat(NORMAL, "%s: Error: Unable to update bad pixel ext with hot/flickering pixels data.\n", global.taskname);
      goto Error;
    }
  }

  /* Sort bad pixel data */
  SortBadPixExtData(bp[0]);
  SortBadPixExtData(bp[1]);
  SortBadPixExtData(bp[2]);
  SortBadPixExtData(bp[3]);


  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[0], 1, KWVL_DETNAM_DET0,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET0);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[1], 2, KWVL_DETNAM_DET1,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET1);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[2], 3, KWVL_DETNAM_DET2,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET2);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  /* Create Bad Pixel Ext  */
  if (CreateBPExt(outunit, bp[3], 4, KWVL_DETNAM_DET3,&bpkeys,ALL_BP))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension (DET=%s)\n", global.taskname, KWVL_EXTNAME_BADPIX,KWVL_DETNAM_DET3);
      headas_chat(NORMAL, "%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
      goto Error;
    }
  outExt++;
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, outExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }


  /* copy any extension after the extension to be operated on */
  inExt++;
  while ( status == OK && inExt <= inExtNum) 
    {
      if(inExt!=bpExt0 && inExt!=bpExt1 && inExt!=bpExt2 && inExt!=bpExt3)
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	  if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	    {
	      headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,inExt);
	      headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	      headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	      goto Error;
	    }
	  inExt++;
	  outExt++;
	}
      else
	{
	  inExt++;	  
	}      
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


  /* Create Output Hot Pixel file */
  if(global.outhpfile)
    {
      headas_chat(NORMAL, "%s: Info: Creating output hot pixel file.\n", global.taskname);
      if (CreateBPFile(hp, global.par.outhpfile, &bpkeys, global.par.outfile, nobadflag))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create '%s' file.\n", global.taskname, global.par.outhpfile);
	  goto Error;
	}
    }

  
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
} /* nuhotpix_work */

void showBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], unsigned short int nobadflag)
{
  int i,j,k,det;
  
  printf("Maschera locale  : %d\n",nobadflag);
  printf("Maschera globale : %d\n",ALL_BP);
  for(det=0;det<4;det++)
  {
	for(i=0;i<DET_PIXS;i++)
	{
	    for(j=0;j<DET_ROWS;j++)
	    {
		if(bp[det][i][j].ninfo>0)
		{
		  for(k=0; k < bp[det][i][j].ninfo; k++)
		  {
		      if(!(bp[det][i][j].info[k].badflag & nobadflag))
		      {
			  printf("DET%d: Il bit '%d' NON è di mia competenza.\n",det,bp[det][i][j].info[k].badflag);
		      }
		      else
		      {
			  printf("DET%d: Il bit '%d' è di mia competenza.\n",det,bp[det][i][j].info[k].badflag);
		      }
		  }
		}
	    }
	}
  }
} /* showBPdata */

/*
 *	nuhotpix
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
 *             void nuhotpix_getpar(void);
 * 	       void nuhotpix_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 01/09/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuhotpix()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUHOTPIX_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuhotpix_getpar() == OK) 
    {
      
      if ( nuhotpix_work()) 
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
  
} /* nuhotpix */


/*
 *	nuhotpix_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 */
void nuhotpix_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the output Hot Pixel file                     :'%s'\n",global.par.outhpfile);
  headas_chat(NORMAL,"Bin size (seconds) for count image generation         :'%f'\n",global.par.binsize);
/*   if (global.par.gradeiterate) */
/*     headas_chat(NORMAL,"Execute new iteration using only events with grade<=12? : yes\n"); */
/*   else */
/*     headas_chat(NORMAL,"Execute new iteration using only events with grade<=12? : no\n"); */
  headas_chat(CHATTY,"Search cell size                                        :'%d'\n", global.par.cellsize);
  headas_chat(CHATTY,"Factor to estimate input gamma function x               :'%f'\n", global.par.impfac);
  headas_chat(CHATTY,"Log Poisson probability threshold                       :'%f'\n", global.par.logpos);
  headas_chat(CHATTY,"Zero background threshold                               :'%d'\n", global.par.bthresh);
  if (global.par.cleanflick == 0)
    headas_chat(NORMAL,"Search and flag flickering pixels?                      : no\n");
  else
    headas_chat(NORMAL,"Search and flag flickering pixels?                      : yes\n");
  if (global.par.iterate == 0)
    headas_chat(NORMAL,"Iterate the search                                      : no\n");
  else
    headas_chat(NORMAL,"Iterate the search                                      : yes\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                   : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                   : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                          : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                          : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nuhotpix_info */


/*
 *	nuhotpix_checkinput
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
 *        0.1.0: - NS 01/09/11 - First version
 *
 */
int nuhotpix_checkinput(void)
{
  int            overwrite=0;
  char           stem[10] , ext[MAXEXT_LEN] ;
  pid_t          tmp;
  char           BaseName[MAXFNAME_LEN];
  
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


  if(global.outhpfile){

    /* If global.par.outhpfile == DEFAULT build filename */ 
    if (!(strcasecmp (global.par.outhpfile, DF_DEFAULT)))
      {
	SplitFilePath(global.par.outfile, NULL, BaseName);
	strcpy(global.par.outhpfile, BaseName);

	StripExtension(global.par.outhpfile);
	strcat(global.par.outhpfile, EXT_FITS_HP);
	headas_chat(NORMAL, "%s: Info: Name for the bad pixels file is:\n",global.taskname);
	headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outhpfile);
      }
    
    if ( FileExists(global.par.outhpfile) ) {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outhpfile);
      if ( !headas_clobpar ) {
	headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outhpfile);
	headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	goto check_end;
      }
      else
	{ 
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outhpfile);
	  if(remove (global.par.outhpfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outhpfile);
	      goto check_end;
	    }
	}
    }
    
  } /*if(global.outhpfile)*/


 
  return OK;

 check_end:
  return NOT_OK;
}

/*
 *
 *      SearchHotPix
 *
 *	DESCRIPTION:
 *           input  -> event fits file 'evunit'
 *           output -> hot pixel maps 'hp'
 *    
 *      CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int SearchHotPix(FitsFileUnit_t evunit, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS]){
  unsigned           FromRow, ReadRows, n,nCols;
  int                rawx, rawy, det_id;
/*   int                grade; */
  int                i, j, k;
  int                count[4][DET_PIXS][DET_ROWS], gradecount[4][DET_PIXS][DET_ROWS];
  double             time, lasttime;
  double             starttime, stoptime;
  BOOL               readevt=TRUE;
  EvtCol_t           indxcol;
  Bintable_t	     intable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  /*Reset hp array and count maps */
  for(k=0; k<4; k++){
    for(i=0; i<DET_PIXS; i++){
      for(j=0; j<DET_ROWS; j++){
	hp[k][i][j].ninfo = 0;
	count[k][i][j] = 0;
	gradecount[k][i][j] = 0;
      }
    }
  }


  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &intable, BINTAB_ROWS, 0, NULL);
  
  if(!intable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  
  nCols=intable.nColumns;


  /* RAWX */
  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &intable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto SearchHotPix_end;
    }

  /* RAWY */
  if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &intable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto SearchHotPix_end;
    }

  /* TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &intable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto SearchHotPix_end;
    }

  /* DET_ID */
  if ((indxcol.DET_ID=ColNameMatch(CLNM_DET_ID, &intable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_DET_ID);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto SearchHotPix_end;
    }

  /* GRADE */
/*   if ((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &intable)) == -1) */
/*     {  */
/*       headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_GRADE); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);  */
/*       goto SearchHotPix_end; */
/*     } */

  EndBintableHeader(&head, &intable);


  /* Set time interval for the first count map */
  starttime = global.evt.tstart;
  stoptime =  global.evt.tstart + global.par.binsize;
  if( global.evt.tstop-stoptime < 0.5*global.par.binsize ){
    stoptime = global.evt.tstop;
  }

  lasttime=-1;
  FromRow = 1;
  ReadRows=intable.nBlockRows;

  /* Read input bintable */
  while( readevt && (ReadBintable(evunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 ) )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  time = DVEC(intable,n,indxcol.TIME);
	  rawx = BVEC(intable,n,indxcol.RAWX);
	  rawy = BVEC(intable,n,indxcol.RAWY);
	  det_id = BVEC(intable,n,indxcol.DET_ID);

	  /* Check input data */
	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto SearchHotPix_end;
	  }
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Error: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    goto SearchHotPix_end;
	  }

	  /* Check event file sorted by TIME column */
	  if(time<lasttime+TIME_SENS){
	    headas_chat(CHATTY,"%s: Warning: row=%d not sorted by TIME column.\n",global.taskname,(FromRow+n) );
	  }
	  lasttime = time;

	  if(time>=global.evt.tstop+TIME_SENS){
	    headas_chat(NORMAL,"%s: Warning: input file contains events with TIME values after TSTOP that will be ignored\n", global.taskname);
	    headas_chat(NORMAL,"%s: Warning: while creating  count image of the detectors.\n", global.taskname);
	    readevt=FALSE;
	    break;
	  }

	  
	  if(time>=stoptime+TIME_SENS){
	    
	    /* Flags Hot/Flickering pixels in 'count' array */
	    headas_chat(NORMAL,"%s: Info: Creating count maps for events with TIME included in [%f - %f]\n",global.taskname,starttime,stoptime);
	    if(NewSearch(count,gradecount))
	      goto SearchHotPix_end;

	    /* Update bad pixels array (hp) with hot/flickering pixels info from count map array (count) */
	    if(UpdateBPdataWithCountMapsdata(count,starttime,stoptime,hp)){
	      headas_chat(CHATTY, "%s: Error: Unable to update bad pixels data with hot pixels info.\n", global.taskname);
	      goto SearchHotPix_end;
	    }

	    /* Update time interval for the next count maps */
	    starttime = stoptime;
	    stoptime = starttime + global.par.binsize;
	    if( global.evt.tstop-stoptime < 0.5*global.par.binsize ){
	      stoptime = global.evt.tstop;
	    }

	    /* Reset count maps */
	    for(k=0; k<4; k++){
	      for(i=0; i<DET_PIXS; i++){
		for(j=0; j<DET_ROWS; j++){
		  count[k][i][j] = 0;
		  gradecount[k][i][j] = 0;
		}
	      }
	    }

	  }
	  
	  /* Update count maps */
	  count[det_id][rawx][rawy]++;
	  
/* 	  if(global.par.gradeiterate) */
/* 	    { */
/* 	      grade = IVEC(intable,n,indxcol.GRADE); */
	      
/* 	      if(grade<=12) */
/* 		gradecount[det_id][rawx][rawy]++; */
/* 	    } */
	  
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 


  /* Flags Hot/Flickering pixels in 'count' array */
  headas_chat(NORMAL,"%s: Info: Creating count maps for events with TIME included in [%f - %f]\n",global.taskname,starttime,stoptime);
  if(NewSearch(count,gradecount))
    goto SearchHotPix_end;

  /* Update bad pixels array (hp) with hot/flickering pixels info from count map array (count) */
  if(UpdateBPdataWithCountMapsdata(count,starttime,stoptime,hp)){
    headas_chat(CHATTY, "%s: Error: Unable to update bad pixels data with hot pixels info.\n", global.taskname);
    goto SearchHotPix_end;
  }

  /* Sort bad pixel data */
  for(k=0; k<4; k++)
    SortBadPixExtData(hp[k]);


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &intable);

  return OK;
  
 SearchHotPix_end:
  if (head.first)
    ReleaseBintable(&head, &intable);
  
  return NOT_OK;

} /* SearchHotPix */


/*
 *	ComputeMapLimits         
 *   
 *	DESCRIPTION:
 *           input  -> hot pixel maps 'count'
 *           output -> map limits array 'map'
 *
 *
 */
void ComputeMapLimits(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4]){

  int      xmax=0, xmin=0, ymax=0, ymin=0;
  int      ii, jj, detid;
  int      xpro[DET_PIXS], ypro[DET_ROWS];


  for(detid=0; detid<4; detid++){

    /* Reset temporary variables */
    xmax=0; xmin=0; ymax=0; ymin=0;  
    TMEMSET0( xpro, xpro);
    TMEMSET0( ypro, ypro);

    /* Compute xpro[] and ypro[] arrays */
    for (ii=0 ; ii < DET_PIXS ; ii++){
      for (jj = 0; jj < DET_ROWS; jj++){
	xpro[ii] = xpro[ii] + count[detid][ii][jj];
	ypro[jj] = ypro[jj] + count[detid][ii][jj];
      }
    }
    
    /* Compute xmax, xmin, ymax, ymin values  */
    for(ii=0; ii<DET_PIXS; ii++){
      if(xpro[DET_PIXS - 1 - ii] >= 1)
	xmin = DET_PIXS - 1 - ii;
      if(xpro[ii] >= 1)
	xmax=ii;
    }
    for(jj=0; jj<DET_ROWS; jj++){
      if(ypro[DET_ROWS - 1 - jj] >= 1)
	ymin=DET_ROWS - 1 - jj;
      if(ypro[jj] >= 1)
	ymax=jj;
    }

    map[detid].xmax = xmax;
    map[detid].xmin = xmin;
    map[detid].ymax = ymax;
    map[detid].ymin = ymin;
  }

} /* ComputeMapLimits */


/*
 *
 *      IdentifyHotPix
 *
 *	DESCRIPTION:
 *           This routine identify the hot pixels and set their value to 'HOTPIX' in the hot pixels maps array 'count' 
 *    
 *      CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int IdentifyHotPix(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4]){

  int      ii, jj, iii, jjj, detid;
  int      tot, sub_count, pixels, half, pix_num, pix_star;
  float    img, bac, pixel_prob, tmp_gammq, percent;
  double   pmax=0.,expo=10.0, sens=0.0000001;


  pmax = pow(expo, global.par.logpos);
  half = (int)global.par.cellsize/2;


  for(detid=0; detid<4; detid++){

    /* Compute tot counts */
    tot=0;
    for(ii=0; ii < DET_PIXS; ii++)
      for(jj=0; jj < DET_ROWS; jj++)
	tot+=count[detid][ii][jj];    

    if(tot <= 0){
      headas_chat(CHATTY, "%s: Warning: det_id = %d counts image is empty.\n", global.taskname, detid);
      continue;
    }

    pixels=(map[detid].xmax-map[detid].xmin+1)*(map[detid].ymax-map[detid].ymin+1);
    bac = ((float)(global.par.impfac)*(float)(tot)/(float)(pixels - 1));
    

    /*
     * Remove hot pixels
     */

    for(ii=0; ii < DET_PIXS; ii++){
      for (jj=0; jj < DET_ROWS; jj++){
	img = (float)(count[detid][ii][jj]);
	
	/* headas_chat(NORMAL,"DEBUG img %f bac %f\n",img,bac); */

	if(nu_gammq((img+1.0), bac, &tmp_gammq))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to calculate incomplete gamma function.\n", global.taskname);
	    goto IdentifyHotPix_end;
	  }
	
	pixel_prob = 1.0 - tmp_gammq;

	/* headas_chat(NORMAL,"DEBUG ii %d jj %d pixel_prob %f ,pmax %f\n",ii,jj,pixel_prob,pmax); */
	
	if(pixel_prob < pmax){
	  
	  sub_count=0;
	  pix_star=0;
	  pix_num=0;
	  percent=100.;

	  for (iii = ii - half ; iii <= (ii + half) ; iii++)
	    for (jjj = jj - half ; jjj <= (jj + half) ; jjj++)
	      if(iii >= 0 && iii < DET_PIXS && jjj >= 0 && jjj < DET_ROWS){

		/* Exclude itself */
		if((iii != ii)||(jjj != jj)) {
		  sub_count+=count[detid][iii][jjj];
		  pix_num++;
		  
		  if (count[detid][iii][jjj]>0)
		    pix_star++;
		}
		
	      }
	  
	  
	  percent = (float)pow(((float)pix_star/(float)pix_num),2)*(((float)sub_count/(float)pix_num)*100./(float)count[detid][ii][jj]);

	  if(sub_count == 0 || percent < (14 + sens)){
	    headas_chat(NORMAL,"%s: Info: Found Hot Pixel in rawx=%2d rawy=%2d det_id=%d",global.taskname,ii,jj,detid);
	    headas_chat(CHATTY," img=%d bac=%.3f percent=%.2f pix_star=%d sub_count=%d pixel_prob=%G",(int)img, bac, percent, pix_star, sub_count, pixel_prob);
	    headas_chat(NORMAL," \n");
	    count[detid][ii][jj]=HOTPIX;
	  }
	  else{
	    headas_chat(CHATTY,"%s: Info: Candidate Hot Pixel in rawx=%2d rawy=%2d det_id=%d",global.taskname,ii,jj,detid);
	    headas_chat(CHATTY," img=%d bac=%.3f percent=%.2f pix_star=%d sub_count=%d pixel_prob=%G",(int)img, bac, percent, pix_star, sub_count, pixel_prob);
	    headas_chat(CHATTY," not flagged\n");
	  }
	}

      }
    }

  } /* End -> for(detid=0; detid<4; detid++) */



  return OK;
  
 IdentifyHotPix_end:
  
  return NOT_OK;

} /* IdentifyHotPix  */



/*
 *
 *      IdentifyFlickering
 *
 *	DESCRIPTION:
 *           This routine identify the flickering pixels and set their value to 'FLICKPIX' in the hot pixels maps array 'count'
 *    
 *      CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int IdentifyFlickering(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4], char *evttype){

  int      ii, jj, iii, jjj, detid, iter;
  int      sub_count,  half, pix_num, pix_star, prefix, fix;
  float    img, bac, pixel_prob, tmp_gammq, percent;
  double   pmax=0.,expo=10.0, sens=0.0000001;


  pmax = pow(expo, global.par.logpos);
  half = (int)global.par.cellsize/2;


  for(detid=0; detid<4; detid++){ 

    iter = 1;
    fix = 0;
    prefix = -1;

    while(prefix != 0){
      
      prefix=0;
      
      for(ii=0; ii < DET_PIXS; ii++){
	for (jj=0; jj < DET_ROWS; jj++){
	  
	  img = (float)(count[detid][ii][jj]) > 0. ? (float)(count[detid][ii][jj]) : 0.;
	  sub_count=0;
	  pix_star=0;
	  pix_num=0;
	  
	  for (iii = ii - half ; iii <= (ii + half) ; iii++){
	    for (jjj = jj - half ; jjj <= (jj + half) ; jjj++){
	      if(iii >= 0 && iii < DET_PIXS && jjj >= 0 && jjj < DET_ROWS){
		
		/* Exclude itself, hot and flickering pixels already identified */
		if( ((iii != ii)||(jjj != jj)) && (count[detid][iii][jjj]!= HOTPIX && count[detid][iii][jjj]!= FLICKPIX) ) {
		  sub_count+=count[detid][iii][jjj];
		  pix_num++;
		  
		  if (count[detid][iii][jjj]>0)
		    pix_star++;
		}
	      }
	    }
	  }
	  
	  bac = ((float)sub_count/(float)pix_num);
	  
	  if( img>(bac-sens) && (bac>0 || img>=global.par.bthresh) ){
	    
	    if(nu_gammq((img+1.0), bac, &tmp_gammq)){
	      headas_chat(NORMAL, "%s: Error: Unable to calculate incomplete gamma function.\n", global.taskname);
	      goto IdentifyFlickering_end;
	    }
	    
	    pixel_prob = 1.0 - tmp_gammq;

	    if(pixel_prob < pmax){

	      percent = (float)pow(((float)pix_star/(float)pix_num),2)*(bac*100./img);

	      if( percent < (14 + sens) && count[detid][ii][jj]!=FLICKPIX ){

		count[detid][ii][jj]=FLICKPIX;
		prefix++;
		
		headas_chat(NORMAL,"%s: Info: Iter %d Found Hot Pixel in rawx=%2d rawy=%2d det_id=%d",global.taskname,iter,ii,jj,detid);
		headas_chat(CHATTY," img=%d bac=%.3f percent=%.2f pix_star=%d sub_count=%d pixel_prob=%G",(int)img, bac, percent, pix_star, sub_count, pixel_prob);
		headas_chat(NORMAL," %s\n",evttype);

	      }
	      else{
		if(count[detid][ii][jj]!=FLICKPIX){
		  headas_chat(CHATTY,"%s: Info: Iter %d Candidate Hot Pixel in rawx=%2d rawy=%2d det_id=%d",global.taskname,iter,ii,jj,detid);
		  headas_chat(CHATTY," img=%d bac=%.3f percent=%.2f pix_star=%d sub_count=%d pixel_prob=%G",(int)img, bac, percent, pix_star, sub_count, pixel_prob);
		  headas_chat(CHATTY," %s not flagged\n",evttype);
		}
	      }

	    } /* End -> if(pixel_prob < pmax) */

	  } /* End -> if( img>(bac - sens) && ... ) */

	}
      }

      if (prefix != 0){
	fix = fix + prefix;
	headas_chat(CHATTY, "%s: Info: Hot pixels iter: %d  %s\n", global.taskname, iter, evttype);
	headas_chat(CHATTY, "%s: Info: Hot pixels found: %d  %s\n", global.taskname, prefix, evttype);
      }

      if (!global.par.iterate)
	break;

      iter++;

    } /* End -> while(prefix != 0) */

  } /* End -> for(detid=0; detid<4; detid++) */



  return OK;
  
 IdentifyFlickering_end:
  
  return NOT_OK;

} /* IdentifyFlickering */


/*
 *
 *      NewSearch
 *
 *	DESCRIPTION:
 *           This routine get in input the 'count' and 'gradecount' count maps array, identify the hot and flickering
 *           pixels and for these set to 'HOTPIX'/'FLICKPIX' the values in the 'count' array.
 *    
 *      CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int NewSearch(int count[4][DET_PIXS][DET_ROWS], int gradecount[4][DET_PIXS][DET_ROWS]){

/*   int            i, j, k; */
  MapBorders_t   map[4];
  char           evttype[20];

  /* Compute maps limits */
  ComputeMapLimits(count,map);
  
  /* Identifies hot pixels using the current count maps */
  if(IdentifyHotPix(count,map)){
    headas_chat(CHATTY, "%s: Error: Unable to identify hot pixels for the current count maps.\n", global.taskname);
    goto NewSearch_end;
  }
  
  /* Identifies flickering pixels using the current count maps */
  if(global.par.cleanflick){
    strcpy(evttype, "");
    if(IdentifyFlickering(count,map,evttype)){
      headas_chat(CHATTY, "%s: Error: Unable to identify hot pixels for the current count maps.\n", global.taskname);
      goto NewSearch_end;
    }
  }
  
/*   if(global.par.gradeiterate){ */
    
/*     /\* Update 'gradecount' array with hot/flickering pixels from 'count' array *\/ */
/*     for(k=0; k<4; k++){ */
/*       for(i=0; i<DET_PIXS; i++){ */
/* 	for(j=0; j<DET_ROWS; j++){ */
/* 	  if( count[k][i][j]==HOTPIX || count[k][i][j]==FLICKPIX ) */
/* 	    gradecount[k][i][j] = count[k][i][j]; */
/* 	} */
/*       } */
/*     } */
    
/*     /\* Identifies flickering pixels using 'gradecount' maps *\/ */
/*     strcpy(evttype, "(grade<=12)"); */
/*     if(IdentifyFlickering(gradecount,map,evttype)){ */
/*       headas_chat(CHATTY, "%s: Error: Unable to identify hot pixels for the current count maps.\n", global.taskname); */
/*       goto NewSearch_end; */
/*     } */
    
/*     /\* Update 'count' array with hot/flickering pixels from 'gradecount' array *\/ */
/*     for(k=0; k<4; k++){ */
/*       for(i=0; i<DET_PIXS; i++){ */
/* 	for(j=0; j<DET_ROWS; j++){ */
/* 	  if( gradecount[k][i][j]==HOTPIX || gradecount[k][i][j]==FLICKPIX ) */
/* 	    count[k][i][j] = gradecount[k][i][j]; */
/* 	} */
/*       } */
/*     } */
    
/*   } */
  
  return OK;

 NewSearch_end:

  return NOT_OK;

} /* NewSearch */


/*
 *
 *      UpdateBPdataWithCountMapsdata
 *
 *	DESCRIPTION:
 *           
 *    
 *      CHANGE HISTORY:
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int UpdateBPdataWithCountMapsdata(int count[4][DET_PIXS][DET_ROWS], double startime, double stoptime, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS]){
  
  int   detid, i, j; 
  
  
  for(detid=0; detid<4; detid++){
    for(i=0; i<DET_PIXS; i++){
      for(j=0; j<DET_ROWS; j++){

	if((count[detid][i][j]==HOTPIX)||(count[detid][i][j]==FLICKPIX)){


	  if(hp[detid][i][j].ninfo==0){

	    hp[detid][i][j].ninfo = 1;
	    hp[detid][i][j].info = (BadPixExtInfo_t*)malloc(sizeof(BadPixExtInfo_t));
	    if(hp[detid][i][j].info==NULL){
	      headas_chat(CHATTY,"%s: Error: UpdateBPdataWithCountMapsdata: memory allocation failure.\n", global.taskname);
	      goto UpdateBPdata_end;
	    }
	    
	    hp[detid][i][j].info[0].time = startime;
	    hp[detid][i][j].info[0].time_stop = stoptime; 
	    hp[detid][i][j].info[0].badflag = EV_HOTFLICK_BP;
	  }
	  /* Time away from the prevoius => add new element to the array */
	  else if(startime > hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].time_stop + BP_TIME_SENS){
	    
	    hp[detid][i][j].ninfo++;
	    hp[detid][i][j].info = (BadPixExtInfo_t*) realloc( hp[detid][i][j].info, (hp[detid][i][j].ninfo * sizeof(BadPixExtInfo_t)) );
	    if(hp[detid][i][j].info==NULL){
	      headas_chat(CHATTY,"%s: Error: UpdateBPdataWithCountMapsdata: memory allocation failure.\n", global.taskname);
	      goto UpdateBPdata_end;
	    }
	    
	    hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].time = startime;
	    hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].time_stop = stoptime; 
	    hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].badflag = EV_HOTFLICK_BP;
	  }
	  /* Time next to the prevoius => update only the 'time_stop' value */
	  else{
	    hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].time_stop = stoptime; 
	    hp[detid][i][j].info[(hp[detid][i][j].ninfo-1)].badflag = EV_HOTFLICK_BP;	    
	  }

	}

      }
    }
  } /* End -> for(detid=0; detid<4; detid++) */


  return OK;
  
 UpdateBPdata_end:
  
  return NOT_OK;

} /* UpdateBPdataWithCountMapsdata */


/*
 *
 *      HotPixFlag
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
 *        0.1.0: - NS 01/09/11 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int HotPixFlag(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS])
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                rawx, rawy, rawx_n, rawy_n, det_id, i, j;
  int                newstatus, newhotpos;
  double             time;
  BTYPE              neigh_pix_hotpos[] = {NEIGH_PIX_HOTPOS};
  int                neigh_x[] = {NEIGH_PIX_X};
  int                neigh_y[] = {NEIGH_PIX_Y};
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

  /* Add STATUS column */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
    }

  /* Add HOTPOS column */
  newhotpos=0;
  if((indxcol.HOTPOS=ColNameMatch(CLNM_HOTPOS, &outtable)) == -1)
    {
      newhotpos=1;
      AddColumn(&head, &outtable,CLNM_HOTPOS,CARD_COMM_HOTPOS , "8X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_HOTPOS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.HOTPOS=ColNameMatch(CLNM_HOTPOS, &outtable);
    }

  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
      
      if(newstatus)
	{
	  sprintf(comm, "Added %s column", CLNM_STATUS);
	  AddHistory(&head, comm);
	}
      if(newhotpos)
	{
	  sprintf(comm, "Added %s column", CLNM_HOTPOS);
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
	  if(newstatus){
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	  }
	  else{	  
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_HOTFLICK_BP;
	    XVEC2BYTE(outtable,n,indxcol.STATUS) = XVEC2BYTE(outtable,n,indxcol.STATUS) &~ EV_NEIGH_HF;
	  }

	  /* Reset HOTPOS column */
	  XVEC1BYTE(outtable,n,indxcol.HOTPOS)=0;
	  

	  /* Hot/Flickering pixels */
	  if(hp[det_id][rawx][rawy].ninfo > 0)
	    {
	      for(i=0; i<hp[det_id][rawx][rawy].ninfo; i++)
		{
		  
		  if( (hp[det_id][rawx][rawy].info[i].badflag & OBS_HOTFLICK_BP) &&
		      ( time>=(hp[det_id][rawx][rawy].info[i].time-TIME_SENS) && time<=(hp[det_id][rawx][rawy].info[i].time_stop+TIME_SENS) ) )
		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_HOTFLICK_BP;
		}
	    }

	  /* Check neighbours hot/flickering pixels */
	  for (j=0; j< NEIGH_PIX_MOL; j++)
	    {
	      rawx_n = rawx + neigh_x[j];
	      rawy_n = rawy + neigh_y[j];

	      if(rawx_n>=RAWX_MIN && rawx_n<=RAWX_MAX && rawy_n>=RAWY_MIN && rawy_n<=RAWY_MAX){
		for(i=0; i<hp[det_id][rawx_n][rawy_n].ninfo; i++)
		  {
		    if(hp[det_id][rawx_n][rawy_n].info[i].badflag & EV_HOTFLICK_BP)
		      {
			if( time>=(hp[det_id][rawx_n][rawy_n].info[i].time-TIME_SENS) && time<=(hp[det_id][rawx_n][rawy_n].info[i].time_stop+TIME_SENS) ){

			  XVEC2BYTE(outtable,n,indxcol.STATUS) |= EV_NEIGH_HF;			  
			  XVEC1BYTE(outtable,n,indxcol.HOTPOS) |= neigh_pix_hotpos[j];
			}
		      }
		  }
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



  return OK;
  
 reco_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* HotPixFlag */


/*
 *
 *      ReadEvtBPdata
 *
 *	DESCRIPTION:
 *           Routine to read the bad pixel extensions of the input event file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], int *bpExt0, int *bpExt1, int *bpExt2, int *bpExt3){

  static BadPixExtData_t inbp[DET_PIXS][DET_ROWS];
  BOOL                   extfound;
  short int              nobadflag=0;


  /* Exclude BADPIX rows with the following BADFLAG bit set to 1 */
  nobadflag |= OBS_HOTFLICK_BP;


  /* Read BADPIX ext for DET0 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET0, nobadflag, &extfound, bpExt0))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[0]))
    return NOT_OK;

  /* Read BADPIX ext for DET1 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET1, nobadflag, &extfound, bpExt1))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[1]))
    return NOT_OK; 

  /* Read BADPIX ext for DET2 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET2, nobadflag, &extfound, bpExt2))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[2]))
    return NOT_OK; 

  /* Read BADPIX ext for DET3 */
  if(ReadEvtBadPixExt(inbp,global.par.infile, KWVL_DETNAM_DET3, nobadflag, &extfound, bpExt3))
    return NOT_OK;

  if(UpdateBPExtWithBadPixExtData(inbp, bp[3]))
    return NOT_OK; 


  return OK;

} /* ReadEvtBPdata */

