/*
 * 
 *	xrthotpix.c: --- Swift/XRT ---
 *
 *	INVOCATION:
 *
 *		xrthotpix [parameter=value ...]
 *
 *	DESCRIPTION:
 *         
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *        0.1.0 - BS 11/03/2004 - First working version
 *        0.1.1 -    25/03/2004 - Deleted input files list handling
 *        0.1.2 -    13/05/2004 - Messages displayed revision  
 *        0.1.3 -    24/06/2004 - Bug fixed on 'fits_update_key' call 
 *                                for LOGICAL keyword
 *        0.1.4 -    20/07/2004 - Added routine to append or update extension with bad pixels 
 *                                information in output events file
 *        0.1.5 -    21/07/2004 - Minor changes in displayed messages
 *        0.1.6 -    07/01/2005 - Bug fixed in xrthotpix_info displaying 
 *                                'bthresh' parameter
 *        0.1.7 -    13/01/2005 - Added 'phamax' and 'phamin' parameters
 *                                Added check on XRTPHA keyword 
 *        0.1.8 -    23/03/2005 - Added input parameter to use only 
 *                                events flag as good
 *        0.1.9 -    23/03/2005 - Modified selection to screen only events with PHAS[1] < thr
 *        0.2.0 -    12/05/2005 - Added routine to set to zero only bad pixels searched
 *        0.2.1 -    01/07/2005 - Displayed list of flickering pixel if chatter
 *                                >= 3
 *        0.2.2 -    09/08/2005 - Exit with warning if infile is empty
 *        0.2.3 -    20/10/2005 - Flag event with hot and/or flickering pixels in the 
 *                                3x3
 *        0.2.4 -    21/10/2005 - Added flagneigh input parameter to decide
 *                                if to flag events with a neighbour hot/flick
 *        0.2.5                 - Replaced 'flagneigh' with 'hotneigh'
 *        0.2.6 -    26/10/2005 - Modified xrthotpix_info routine
 *                              - Set to yes overstatus input parameter default
 *        0.2.7 - NS 21/02/2007 - Bug fixed in CleanFlickering function
 *                              - Changed CleanHotPixels function
 *        0.2.8 -    05/03/2007 - Added new stdout messages with chatter=5
 *        0.2.9 -    07/03/2007 - Added new input parameter 'usecleanmap'
 *                              - Not overwrite EV_BAD status flag if set
 *        0.3.0 -    09/03/2007 - Modified CleanHotPixels and CleanFlickering functions
 *        0.3.1 -    02/04/2007 - Removed 'usecleanmap' input parameter
 *                              - Added new input parameter 'gradeiterate'
 *        0.3.2 -    17/05/2007 - Modified percent threshold (from 20 to 10)
 *                                in Hot and Flickering pixels search
 *                              - Compute pix_num2 and bac2 values in CleanFlickering functions
 *        0.3.3 -    08/03/2012 - Bug fixed on 'KeyWordMatch' call
 *
 *                               
 *                    
 *
 *     AUTHOR:
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrthotpix          /* headas_main() requires that TOOLSUB 
				       be defined first */
#define XRTHOTPIX_C
#define XRTHOTPIX_VERSION   "0.3.3"
#define PRG_NAME            "xrthotpix"

/******************************
 *        header files        *
 ******************************/
#include "headas_main.c"
#include "xrthotpix.h"

/******************************
 *       definitions          *
 ******************************/

Global_t global;

/*
 *	xrthotpix_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrthotpix.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 *           int PILGetBool(char *name, int *result);  
 *           int headas_chat(int, char * , ...);
 *           void xrthotpix_info(void);
 *	
 *      CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *        0.1.1: -    13/05/2003 - Deleted 'biasmap' parameter and 
 *                                 added 'bptable' parameter
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int xrthotpix_getpar(void)
{

  int    tmp=0;
  double sens=0.0000001;

  /************************
   *   Get parameters.    *
   ************************/
  
  /* Get input events fits file name */ 
  if(PILGetFname(PAR_INFILE, global.par.infile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_INFILE);
      goto Error;
    }

  /* Get output bad pixels file name */
  if(PILGetFname(PAR_OUTBPFILE, global.par.outbpfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_OUTBPFILE);
      goto Error;
    }

  /* Get output events fits file name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_OUTFILE);
      goto Error;
    }

   /* Overwrite status column ? */ 
   if(PILGetBool(PAR_OVERSTATUS, &global.par.overstatus))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_OVERSTATUS);
       goto Error;
     }

   /* Overwrite status column ? */ 
   if(PILGetBool(PAR_USEGOODEVT, &global.par.usegoodevt))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_USEGOODEVT);
       goto Error;
     }

   if(PILGetBool(PAR_GRADEITERATE, &global.par.gradeiterate))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_GRADEITERATE);
       goto Error;
     }

   /* Clean flickering pixels */ 
   if(PILGetBool(PAR_CLEANFLICK, &global.par.cleanflick))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_CLEANFLICK);
       goto Error;
     }

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

   if(PILGetInt(PAR_BTHRESH, &global.par.bthresh))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_BTHRESH);
       goto Error;
     }

   if(PILGetInt(PAR_PHAMIN, &global.par.phamin))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_PHAMIN);
       goto Error;
     }

   if(PILGetInt(PAR_PHAMAX, &global.par.phamax))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_PHAMAX);
       goto Error;
     }

   /* Iterate the Poisson clean */ 
   if(PILGetBool(PAR_ITERATE, &global.par.iterate))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ITERATE);
       goto Error;
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
   
   if(PILGetBool(PAR_FLAGNEIGH, &global.par.flagneigh))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_FLAGNEIGH);
       goto Error;
     }

   get_history(&global.hist);

   xrthotpix_info();

   return OK;

 Error:
   return NOT_OK;
}/*xrthotpix_getpar*/

/*
 *	xrthotpix_info 
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
 *          get_toolnamev(char *);
 *          headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
void xrthotpix_info(void)
{
  headas_chat(MUTE, "\n---------------------------------------------------------------\n");
  headas_chat(MUTE,"\t\tRunning '%s'\n", global.taskname);
  headas_chat(MUTE, "---------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                :'%s'\n", global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file               :'%s'\n", global.par.outfile);
  headas_chat(NORMAL,"Name of the output Badpixel file            :'%s'\n", global.par.outbpfile);
  if (global.par.overstatus == 0)
    headas_chat(NORMAL,"Overwrite STATUS column?                    : no\n");
  else
    headas_chat(NORMAL,"Overwrite STATUS column?                    : yes\n");
  if (global.par.usegoodevt)
    headas_chat(NORMAL,"Using only events flag as good?             : yes\n");
  else
    headas_chat(NORMAL,"Using only events flag as good?             : no\n");
  if (global.par.gradeiterate)
    headas_chat(NORMAL,"Execute new iteration using only events with grade<=12? : yes\n");
  else
    headas_chat(NORMAL,"Execute new iteration using only events with grade<=12? : no\n");
  headas_chat(CHATTY,"Search cell size                            :'%d'\n", global.par.cellsize);
  headas_chat(CHATTY,"Factor to estimate input gamma function x   :'%f'\n", global.par.impfac);
  headas_chat(CHATTY,"Log Poisson probability threshold           :'%f'\n", global.par.logpos);
  headas_chat(CHATTY,"Zero background threshold                   :'%d'\n", global.par.bthresh);
  if (global.par.cleanflick == 0)
    headas_chat(NORMAL,"Search and flag flickering pixels?          : no\n");
  else
    headas_chat(NORMAL,"Search and flag flickering pixels?          : yes\n");
  if (global.par.iterate == 0)
    headas_chat(NORMAL,"Iterate the search                          : no\n");
  else
    headas_chat(NORMAL,"Iterate the search                          : yes\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                      : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                      : no\n");

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file?      : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file?      : no\n");
  headas_chat(NORMAL, "---------------------------------------------------------------\n\n");
  return;

}/* xrthotpix_info */
/*
 *	xrthotpix_checkinput
 *
 *
 *	DESCRIPTION:
 *          check integrity of input data and define
 *          input/output default file name
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *          get_toolnamev(char *);
 *          headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int xrthotpix_checkinput(void)
{

  int            overwrite=0;
  char           stem[10], ext[MAXEXT_LEN] ;  
  pid_t          tmp;      

  /* If global.par.outfile != NONE if it exists */
  if (strcasecmp (global.par.outfile, DF_NONE))
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
		headas_chat(NORMAL, "%s: Error: Unable to remove", global.taskname);
		headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
		goto error_check;
	      }  
	  }
      }
    }
  else
    {
      GetFilenameExtension(global.par.infile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	{
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	  goto error_check;
	}
      else
	/* Overwrite input file */
	overwrite=1;
    }
  
  /* Derive temporary event filename */
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  if(overwrite){
    strcpy(global.par.outfile, global.par.infile);
    DeriveFileName(global.par.infile, global.tmpfile, stem);
  }  
  else
    DeriveFileName(global.par.outfile, global.tmpfile, stem);


  /* If global.par.outbpfile == DEFAULT build filename */ 
  if (!(strcasecmp (global.par.outbpfile, "DEFAULT")))
    {
      strcpy(global.par.outbpfile, global.par.outfile);
      StripExtension(global.par.outbpfile);
      strcat(global.par.outbpfile, HBPFITSEXT);
      headas_chat(CHATTY, "%s: Info: Name for the bad pixels file is:\n",global.taskname);
      headas_chat(CHATTY, "%s: Info: '%s'\n", global.taskname, global.par.outbpfile);
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
	headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten\n", global.taskname, global.par.outbpfile);
	if(remove (global.par.outbpfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outbpfile);
	    goto error_check;
	  } 
      }
  }
  
  return OK;

 error_check:
  return NOT_OK;


}/* xrthotpix_checkinput */ 
/*
 *	xrthotpix_work
 *
 *
 *	DESCRIPTION:
 *        
 *     
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *             set_toolversion(char *);
 *             set_toolname(char *);   
 *             get_toolname(char *);
 *             get_toolnamev(char *);
 *             headas_chat(int ,char *, ...);
 *
 *        
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             BOOL KeyWordMatch(FitsFileUnit_t unit, const char *KeyWord, ValueTag_t vtag,
 *				 const void *NominalValue, void *ActValue);
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *             int xrthotpix_getpar(char *);
 *             int GetBPFromEvents(FitsFileUnit_t iunit, BadPixel_t *badpix[][]);
 *             int CreateEVFile(FitsFileUnit_t evunit, BadPixel_t *bp_table[][]);
 *             int CreateBPFile(FitsFileUnit_t evunit, BadPixel_t *bp_table[][]);
 *
 *             int fits_movnam_hdu(fitsfile *fptr, int hdutype, char *extname, 
 *                                 int extver, int * status);
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

int xrthotpix_work()
{
 
  int                   ii, jj;
  int                   status=OK, hotcounts=0, hotpixs=0, flickcounts=0, flickpixs=0; 
  int                   xpro[CCD_PIXS], ypro[CCD_ROWS], z, xmax=1, xmin=1, ymax=1, ymin=1;
  static int            count[CCD_PIXS][CCD_ROWS], flick[CCD_PIXS][CCD_ROWS], gradecount[CCD_PIXS][CCD_ROWS]; 
  FitsCard_t            *card;
  FitsHeader_t          evhead;            /* Extension header pointer */
  FitsFileUnit_t        evunit=NULL;       /* input and output file pointers */

  TMEMSET0( count, count);
  TMEMSET0( gradecount, gradecount);
  TMEMSET0( xpro, xpro);
  TMEMSET0( ypro, ypro);
  
  global.warning=0;
  if (xrthotpix_checkinput())
    goto end_func;
  
  /* Open read only input event file */
  if ((evunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  /* Move to events extension */ 
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
      if (CloseFitsFile(evunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	  goto end_func;
	}
      goto end_func;
    } 
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, evunit, global.par.infile,global.taskname);
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);
  /* Get extension header pointer  */
  evhead=RetrieveFitsHeader(evunit);

  /************************************
   *  Check if read out mode is valid *
   ************************************/
  /* readout mode is PHOTON ? */
  if(!(KeyWordMatch(evunit, KWNM_DATAMODE, S, KWVL_DATAMODE_PH, NULL)))
    {
      
      headas_chat(NORMAL,"%s: Error: This task does not process the readout mode\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid readout mode  is:  %s.\n", global.taskname, KWVL_DATAMODE_PH);
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto end_func;
    }

  /* Check if PHA is already computed */
  if(ExistsKeyWord(&evhead, KWNM_XRTPHA, &card))
    {
      if(!card->u.LVal)
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword is set to FALSE.\n", global.taskname, KWNM_XRTPHA);
	  headas_chat(NORMAL, "%s: Error: %s column is empty.\n", global.taskname, CLNM_PHA);
	  goto end_func;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found.\n", global.taskname, KWNM_XRTPHA);
      headas_chat(NORMAL, "%s: Error: %s column is empty.\n", global.taskname, CLNM_PHA);
      goto end_func;

    }
  
  if(FillCountImage(evunit, count, gradecount))
    { 
      headas_chat(CHATTY,"%s: Error: Unable to build counts image.\n", global.taskname);
      goto end_func; 
    } 
  if(global.warning)
    goto ok_end;
  
  xmax=1; xmin=1; ymax=1; ymin=1;

  for (ii=0 ; ii < CCD_PIXS ; ii++)
    {
      for (jj = 0; jj < CCD_ROWS; jj++)
	{
	  z=count[ii][jj];
	  xpro[ii]=xpro[ii]+z;
	  ypro[jj]=ypro[jj]+z;
	}
     
    }
  
  for(ii=0; ii<CCD_PIXS; ii++)
    {
      if(xpro[CCD_PIXS - ii - 1] >= 1)
	xmin=CCD_PIXS - 1 - ii;
      if(xpro[ii] >= 1)
	xmax=ii;
    }
  
  for(jj=0; jj<CCD_ROWS; jj++)
    {
      if(ypro[CCD_ROWS - jj - 1] >= 1)
	ymin=CCD_ROWS - 1 - jj;
      if(ypro[jj] >= 1)
	ymax=jj;
    }
  
  
  headas_chat(CHATTY, "%s: Info: Image X Range is: xmin=%3d xmax=%3d.\n",global.taskname,xmin,xmax);
  headas_chat(CHATTY, "%s: Info: Image Y Range is: ymin=%3d ymax=%3d.\n",global.taskname,ymin,ymax);
  
  if(CleanHotPixels(count, xmin, xmax, ymin, ymax, &hotcounts, &hotpixs))
    {
      headas_chat(NORMAL, "%s: Error: Unable to search hot pixels.\n", global.taskname);
      goto end_func;
    }
  
  /*
   *
   * Search flickering pixels 
   *
   */
  
  if(global.par.cleanflick)
    {
      for (ii=0 ; ii < CCD_PIXS ; ii++)
	for (jj = 0; jj < CCD_ROWS; jj++)
	  flick[ii][jj]=count[ii][jj];
      
      
      if(CleanFlickering(flick, xmin, xmax, ymin, ymax, &flickcounts, &flickpixs))
	{
	  headas_chat(NORMAL, "%s: Warning: Unable to search flickering pixels.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: Flickering pixels will be ignored.\n", global.taskname);
	} 
      else
	{
	  for (ii=0 ; ii < CCD_PIXS ; ii++)
	    for (jj = 0; jj < CCD_ROWS; jj++)
	      {
		if(flick[ii][jj] == FLICKPIX)
		  {
		    count[ii][jj]=FLICKPIX;
		    /*headas_chat(NORMAL, "%s: Info: Found a Flickering Pixels in rawx = %3d rawy = %3d\n", global.taskname, ii,  jj);*/
		  }
	      }
	}



      if(global.par.gradeiterate)
	{
	  /*  Execute search of flickering pix using grade selection */

	  for (ii=0 ; ii < CCD_PIXS ; ii++)
	    for (jj = 0; jj < CCD_ROWS; jj++)
	      {
		if((flick[ii][jj]!=FLICKPIX)&&(flick[ii][jj]!=HOTPIX))
		  flick[ii][jj]=gradecount[ii][jj];
	      }
	  
	  headas_chat(NORMAL, "%s: Info: Executing new search of flickering pixels using grade selection.\n", global.taskname);
	  
	  if(CleanFlickering(flick, xmin, xmax, ymin, ymax, &flickcounts, &flickpixs))
	    {
	      headas_chat(NORMAL, "%s: Warning: Unable to search flickering pixels with grade selection.\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: Flickering pixels with grade selection will be ignored.\n", global.taskname);
	    } 
	  else
	    {
	      for (ii=0 ; ii < CCD_PIXS ; ii++)
		for (jj = 0; jj < CCD_ROWS; jj++)
		  {
		    if(flick[ii][jj] == FLICKPIX)
		      {
			count[ii][jj]=FLICKPIX;
			/*headas_chat(NORMAL, "%s: Info: Found a Flickering Pixels in rawx = %3d rawy = %3d\n", global.taskname, ii,  jj);*/
		      }
		  }
	    }
	}
    }

        
  /********************************
   * Create Temporary events file *
   ********************************/
    
  if(CreateEVFile(evunit, count))
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_func;
    }

 
    
  /* Close input event file */
  if (CloseFitsFile(evunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
      goto end_func;
    }
  
  
  /* rename temporary file into output event file */
  
  if (rename (global.tmpfile,global.par.outfile) == -1) 
    { 
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto end_func; 
    } 
    
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);
  
  
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  

 ok_end:
  if(global.warning && strcasecmp(global.par.infile, global.par.outfile) )
    {
      if(CopyFile(global.par.infile, global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outfile.\n", global.taskname);
	  goto end_func;
	}
    }
  return OK;
  
 end_func:
  
  return NOT_OK;
}/* xrthotpix_work */

int xrthotpix(void)
{
  
  /********************** 
   * set HEADAS globals *
   **********************/
  set_toolname(PRG_NAME);
  set_toolversion(XRTHOTPIX_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /*************************
   * Read input parameters *
   *************************/
  if ( xrthotpix_getpar() == OK ) {

    if ( xrthotpix_work() ) { 
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto hotpix_end;
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
  else
    goto hotpix_end;
  
  return OK;
  
 hotpix_end:
  
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  
  return NOT_OK;

} /* xrthotpix */


/*
 *
 *
 */

int FillCountImage(FitsFileUnit_t evunit, int count[CCD_PIXS][CCD_ROWS], int gradecount[CCD_PIXS][CCD_ROWS])
{

  int                status=OK, pha;  
  EvCol_t            indxcol;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,  rawx, rawy, grade;  
  Bintable_t	     intable;
  FitsHeader_t	     head;

  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
    
  /* Move in events extension */
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    {  
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      goto fill_end;
    } 

  /* Build Event Extension and add badflag column */
  head=RetrieveFitsHeader(evunit);
  GetBintableStructure(&head, &intable, BINTAB_ROWS, 0, NULL);
  nCols=intable.nColumns;
  if(!intable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning %s file is empty\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &intable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto fill_end;
    }
  
  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto fill_end;
    }

  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto fill_end;
    }

  if((indxcol.PHA=ColNameMatch(CLNM_PHA, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto fill_end;
    }
  
  if(global.par.usegoodevt)
    {
      if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STATUS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto fill_end;
	}
    }

  if(global.par.gradeiterate)
    {
      if((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto fill_end;
	}
    }
  

  EndBintableHeader(&head, &intable); 

  FromRow = 1;
  ReadRows=intable.nBlockRows;
  OutRows=0;
  global.amp=0;

  /* Check bad pixels and flag them */
  while(ReadBintable(evunit, &intable, nCols, NULL, FromRow,&ReadRows)==0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  if (global.amp == 0)
	    global.amp= BVEC(intable, n,indxcol.Amp);
	  else if (global.amp != BVEC(intable, n,indxcol.Amp))
	    {
	      headas_chat(NORMAL, "%s: Error:  The readout amplifier is changed within the frame.\n", global.taskname);
	      goto fill_end;
	    }
	    
      
	  rawx = IVEC(intable,n,indxcol.RAWX);
	  rawy = IVEC(intable,n,indxcol.RAWY);
	  /* Check if pixel is out of range */
	  if (rawx < 0 || rawx >= CCD_PIXS || rawy < 0 || rawy >= CCD_ROWS)
	    headas_chat(CHATTY, "%s: Warning: pixel rawx=%d rawy=%d is out of range. Ignored.\n", global.taskname, rawx, rawy);
	  else
	    {
	      pha = JVEC(intable, n, indxcol.PHA);
	      if((pha >= global.par.phamin && pha <= global.par.phamax))
		{  
		  
		  if(global.par.gradeiterate)
		    {
		      grade = IVEC(intable,n,indxcol.GRADE);
		      
		      if (grade>12){
			goto fill_count;
		      }
		      
		      if(global.par.usegoodevt)
			{
			  if(!(XVEC2BYTE(intable,n,indxcol.STATUS) & EV_BELOW_TH))
			    gradecount[rawx][rawy]++;    
			}
		      else
			gradecount[rawx][rawy]++; 
		    }


		fill_count:
		    
		  if(global.par.usegoodevt)
		    {
		      if(!(XVEC2BYTE(intable,n,indxcol.STATUS) & EV_BELOW_TH))
			count[rawx][rawy]++;    
		    }
		  else
		    count[rawx][rawy]++;
		}
	    }
	  
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  
  ReleaseBintable(&head, &intable);
  
  
  return OK;
 fill_end:
  
  if (head.first)
    ReleaseBintable(&head, &intable);
  
  return NOT_OK;
}/* FillCountImage */

/*
 * CleanHotPixels
 */

int CleanHotPixels(int count[CCD_PIXS][CCD_ROWS], int xmin, int xmax, int ymin, int ymax, int *totcounts, int *totpixs)
{

  int      ii, jj, tot, pixels=0, iii=0, jjj=0, sub_count;
  int      prebad, prefix, half, pix_num, pix_star;
  float    img, bac, pixel_prob, tmp_gammq, percent; 
  double   pmax=0.,expo=10., sens=0.0000001; 
  
  pmax=pow(expo, global.par.logpos);

  tot=0;
  prebad=0;
  prefix=0;
  pixels=(xmax-xmin+1)*(ymax-ymin+1);

  half=(int)global.par.cellsize/2;
  pix_num=global.par.cellsize*global.par.cellsize-1;

  for(ii=0; ii < CCD_PIXS; ii++)
    for(jj=0; jj < CCD_ROWS; jj++)
	tot+=count[ii][jj];

  if(tot <= 0)
    {
      headas_chat(CHATTY, "%s: Error: counts image is empty.\n", global.taskname);
      goto clean_end;
    }

  /*
   * Remove hot pixels
   */

  for(ii=0; ii<CCD_PIXS; ii++)
    {
      for (jj=0; jj < CCD_ROWS; jj++)
	{
	  img=(float)(count[ii][jj]);
	  bac=((float)(global.par.impfac)*(float)(tot)/(float)(pixels - 1));
	  
	  if(xrt_gammq((img+1.0), bac, &tmp_gammq))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate incomplete gamma function.\n", global.taskname);
	      goto clean_end;
	    } 
	  
	  pixel_prob=1.0 - tmp_gammq;
	  if(pixel_prob < pmax)
	    {
	      sub_count=0;
	      percent=100.;
	      pix_star=0;

	      if((ii - half) >= 0 && (ii + half) < CCD_PIXS && (jj - half) >= 0 && (jj + half) < CCD_ROWS)
		{
		  for (iii = ii - half ; iii <= (ii + half) ; iii++)
		    for (jjj = jj - half ; jjj <= (jj + half) ; jjj++)
		      if((iii != ii)||(jjj != jj))
			{
			  sub_count+=count[iii][jjj];
			
			  if (count[iii][jjj]>0)
			    pix_star++;
			}
		  
		  percent = (float)pow(((float)pix_star/(float)pix_num),2)*(((float)sub_count/(float)pix_num)*100./(float)count[ii][jj]);
		  
		}

	      headas_chat(CHATTY, "%s: Info: Hot Pixel in : x = %3d y = %3d count %d percent %G pix_star %d sub_count %d\n", global.taskname, ii,  jj, count[ii][jj], percent, pix_star, sub_count);

	      if(sub_count == 0 || percent < (10 + sens))
		{
		  headas_chat(NORMAL, "%s: Info: Found a Hot Pixel in : x = %3d y = %3d \n", global.taskname, ii,  jj);
		  count[ii][jj]=HOTPIX;
		  prefix=prefix+1;
		  prebad=prebad + (int)(img);
		}
	      else
		{
		  headas_chat(CHATTY, "%s: Info: candidate Hot Pixel in : x = %3d y = %3d not flagged\n", global.taskname, ii,  jj);
		}
	    }
	}
    }

  *totcounts=prebad;
  *totpixs=prefix;

/*    headas_chat(NORMAL, "prefix = %d prebad=%d \n", prefix, prebad); */

  return OK;

 clean_end:
  return NOT_OK;
}
/*
 *
 *
 */

int FlagBPPixels(FitsFileUnit_t evunit, FitsFileUnit_t ounit, int count[CCD_PIXS][CCD_ROWS])
{
  
  /* int                contatore_hot=0, contatore_flick=0;*/
  int                neigh_x[] = {NEIGH_X}, jj=0;
  int                neigh_y[] = {NEIGH_Y};
  char               comm[256]="";
  EvCol_t            indxcol;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,  rawx, rawy;  
  Bintable_t	     outtable;
  FitsHeader_t	     head;

  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
    
  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto flag_end;
    }
  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto flag_end;
    }
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STATUS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto flag_end;
    }

  GetGMTDateTime(global.date);
  /* Add history */
  if(global.hist)
    {
      sprintf(global.strhist, "File modified  by %s (%s) at %s", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      AddHistory(&head, comm);
      
    }
 /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  
  /* Check bad pixels and flag them */
  while(ReadBintable(evunit, &outtable, nCols, NULL, FromRow,&ReadRows)==0)
    {
      for(n=0; n<ReadRows; ++n)
	{
      
	  rawx = IVEC(outtable,n,indxcol.RAWX);
	  rawy = IVEC(outtable,n,indxcol.RAWY);
	
	  if(global.par.overstatus)
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS)&~EV_HOT_BP);
	      /*XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS)&~EV_BAD);*/
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS)&~EV_NEIGH_HF);
	      if(global.par.cleanflick)
		XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS)&~EV_FLICK_BP);
	      
	    }
	  /* Check if pixel is out of range */
	  if (rawx < 0 || rawx >= CCD_PIXS || rawy < 0 || rawy >= CCD_ROWS)
	    {
	      headas_chat(CHATTY, "%s: Warning:  pixel rawx=%d rawy=%d is out of range.Ignored.\n", global.taskname,rawx, rawy);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BAD;
	    }
	  else if (count[rawx][rawy] == HOTPIX)
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_HOT_BP;
	      /*contatore_hot+=1;*/
	    }
	  else if (count[rawx][rawy] == FLICKPIX)
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_FLICK_BP;
	      /*contatore_flick+=1;*/
	    }


	  /* Flag neighbour */
	  if(global.par.flagneigh)
	    {

	      for (jj=1; jj< PHAS_MOL; jj++)
		{
		  
		  if ( (rawx+neigh_x[jj]>0 && rawx+neigh_x[jj]<CCD_PIXS) 
		       &&
		       (rawy+neigh_y[jj]>0 && rawy+neigh_y[jj]<CCD_ROWS)
		       &&
		       ((count[rawx+neigh_x[jj]][rawy+neigh_y[jj]] == HOTPIX) || (count[rawx+neigh_x[jj]][rawy+neigh_y[jj]] == FLICKPIX)))
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_HF;
		      jj=PHAS_MOL;
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
    }
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);

  return OK;

 flag_end:
  
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
}/* FlagBPPixels */

/*
 *	CreateEVFile:
 *
 *
 *	DESCRIPTION:
 *          Routine to build event photon counting file with bad pixels flagged
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *             int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, int *hdutype, int *status);
 *
 *             int CreateFile(FitsFileUnit_t evunit, char * filename, int );
 *             int CopyHdu(FitsFileUnit_t evunit, FitsFileUnit_t evfile, int hdunum);
 *             int EventBintable(FitsFileUnit_t evunit, FitsFileUnit_t outunit, BadPixel_t *bp_table[][]);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int CreateEVFile(FitsFileUnit_t evunit, int count[CCD_PIXS][CCD_ROWS]) 
{
  int                   status=OK, hdutot, hducount, hduev, hdubp=0, nobphdu=1, x=0, y=0;
  int                   logical;
  FitsFileUnit_t        outunit=NULL;
  static BadPixel_t     *bp_table[CCD_PIXS][CCD_ROWS];


  TMEMSET0( bp_table, bp_table);
  
  /* Get total number of HDUs in input events file */
  if (fits_get_num_hdus(evunit, &hdutot, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_cevf;
    }
  
  hducount=1;
  
  /* Build  primary header */
  if((outunit = CreateFile(evunit, global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_cevf;
    }
  
  hducount++;
  
  /* Move in events extension */
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      goto end_cevf;
    } 

  if (!fits_get_hdu_num(evunit, &hduev))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      goto end_cevf;
    } 
  

  nobphdu=0;
  /* Check if badpixels extension exists */
  if(fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_BAD , 0, &status))
    {
      if(status == BAD_HDU_NUM)
	{
	  headas_chat(CHATTY,"%s: Warning: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(CHATTY,"%s: Warning: '%s' file.\n",global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Info: The task will create and fill it.\n",global.taskname);
	  nobphdu=1;
	  status=0;
	}
      else
	{
	  headas_chat(CHATTY,"%s: Error: Unable to read '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(CHATTY,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
	  goto end_cevf;
	}
    }

  else
    {
      /* Get bad pixels ext number */
      if (!fits_get_hdu_num(evunit, &hdubp))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: : '%s' file.\n",global.taskname, global.par.infile);
	  goto end_cevf;
	}
    }
  if (hduev!=hducount)
    {
      for (; hducount<hduev; hducount++)
	if(CopyHdu(evunit, outunit, hducount)) {
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,hducount);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto end_cevf;
	}
    }
  
  if(fits_movabs_hdu(evunit, hducount, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,hducount);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto end_cevf;
    }
  
  if(FlagBPPixels(evunit, outunit, count))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to flag hot and flickering pixels\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in %s temporary file.\n", global.taskname, global.tmpfile);
      
      goto end_cevf; 
    } 

  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTFLAG, &logical, CARD_COMM_XRTFLAG, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTFLAG);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_cevf;
    }

  if(global.par.flagneigh)
    {
      logical=TRUE;
    }
  else
    {
      logical=FALSE;
    }

  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTHNFLG, &logical, CARD_COMM_XRTHNFLG, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTHNFLG);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_cevf;
    }

  /* Fill bp_amp array */

  for (x=0 ; x < CCD_PIXS ; x++)
    {
      for (y = 0; y < CCD_ROWS; y++)
	{
	  if(count[x][y] == HOTPIX || count[x][y] == FLICKPIX)
	    {
	      /* Add pixel and info about it in bad pixels map */
	      if ( bp_table[x][y] == NULL ) {
		GETMEM(bp_table[x][y], BadPixel_t , 1 ,"CreateEVFile", 1);
		TMEMSET0(bp_table[x][y],BadPixel_t);	
		bp_table[x][y]->bad_type = BAD_PIX; 	
		bp_table[x][y]->xyextent = 1;
		bp_table[x][y]->bad_flag = (count[x][y] == HOTPIX) ?  OBS_HOT_BP:OBS_FLICK_BP;
		
	      }
	      else
		{
		  bp_table[x][y]->bad_flag |= (count[x][y] == HOTPIX) ?  OBS_HOT_BP:OBS_FLICK_BP;
		  		  
		}
	    }
	}
    }

  /* Check if badpixels file shall be created */
  if ( strcasecmp(global.par.outbpfile,DF_NONE)) 
    {  
      /* Create complete bad pixels file */
      if(CreateHotBPFile(evunit, global.amp, bp_table, global.par.outbpfile))
	{
	  headas_chat(NORMAL,"%s: Warning: Unable to create\n",  global.taskname);
	  headas_chat(NORMAL,"%s: Warning: '%s' file. \n", global.taskname, global.par.outbpfile);
	}
      else
	headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outbpfile);
    }


  hducount++;

  for (; hducount<=hdutot; hducount++)
    {
      if(hducount != hdubp)
	{
	  if(CopyHdu(evunit, outunit, hducount)) {
	    headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,hducount);
	    headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	    headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	    goto end_cevf;
	  }
	}
      else
	{
	  if(global.par.overstatus)
	    {
	      if(ReadBPExt(evunit, hdubp,bp_table))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' input file.\n",global.taskname, global.par.infile);
		  goto end_cevf;
		}

	      if(CreateBPExt(outunit,bp_table,KWVL_EXTNAME_BAD, global.amp))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		  goto end_cevf;
		  
		}
	      headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
	    }
	  else
	    {
	     
	    
	      if(UpdateBPExt(outunit, evunit,bp_table,KWVL_EXTNAME_BAD, global.amp, hducount))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to update '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		  goto end_cevf;
		  
		}
	      headas_chat(MUTE,"%s: Info: %s extension successfully updated\n",global.taskname, KWVL_EXTNAME_BAD);
	    }
	    
	}
    }

  if(nobphdu || hdubp < hduev)
    {
      if(CreateBPExt(outunit,bp_table,KWVL_EXTNAME_BAD, global.amp))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
	  goto end_cevf;
	  
	}
      headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
      hdubp=hdutot+1;

    }
  /* write history is parameter history set */
  if(HDpar_stamp(outunit, hduev, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto end_cevf;
    }
  /* Write history and keywords into BADPIXEL extension */
  
  if(WriteBPKeyword(evunit, outunit, global.tmpfile))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write necessary keywords\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: in %s extension\n", global.taskname, KWVL_EXTNAME_BAD);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.tmpfile);
      goto end_cevf;
    }

  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto end_cevf;
    }

  /* Calculate checksum and add it in file */
  if (ChecksumCalc(outunit))
    { 
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto end_cevf;
    }
  
  /* Close events tmp file */
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto end_cevf;
    }
  return OK;
  
 end_cevf:
  
  return NOT_OK;
}/* CreateEVFile */

/*
 * CleanFlickering
 */

int CleanFlickering(int count[CCD_PIXS][CCD_ROWS], int xmin, int xmax, int ymin, int ymax, int *totcounts, int *totpixs)
{

  int         i=0,j=0, ii=0, jj=0, iii=0, jjj=0, iter;
  int         prebad, prefix, fix, bad_counts, pix_num, pix_num2, pix_star, half;
  float       img, bac, bac2, pixel_prob, tmp_gammq, source, percent; 
  double      pmax=0.,expo=10., sens=0.0000001; 
  static int  bgd[CCD_PIXS][CCD_ROWS];
  
  TMEMSET0(bgd,bgd);
  
  fix=0;
  bad_counts=0;
  iter=1;
  prefix=-1;
  pmax=pow(expo, global.par.logpos);
  half=(int)global.par.cellsize/2;
  
  while(prefix != 0)
    {
      prebad=0;
      prefix=0;
      for(j=ymin ; j <= ymax; j++)
	{
	  for(i=xmin; i <= xmax; i++)
	    {
	      img = 0.;
	      source = (float)count[i][j] > 0. ? (float)count[i][j] : 0.;

	      for (jj=1; jj <= global.par.cellsize; jj++)
		{
		  for(ii=1; ii <= global.par.cellsize; ii++)
		    {    

		      if((i+ii-global.par.cellsize/2 -1 >= xmin)&&(i+ii-global.par.cellsize/2 -1<= xmax)
			&&(j+jj-global.par.cellsize/2 -1 >= ymin)&&(j+jj-global.par.cellsize/2 -1<= ymax))
			{
			  iii=i+ii-global.par.cellsize/2 -1;
			  jjj=j+jj-global.par.cellsize/2 -1;

			  if((count[iii][jjj] != HOTPIX)&&(count[iii][jjj] != FLICKPIX))
			    {
			      img = img + (float)count[iii][jjj];
			    }
			}	
		    }
		}

	      bgd[i][j]=(int)(img - source);
	      
	    }
	}

   
      for(i = xmin; i <= xmax ; i++)
	{
	  for (j=ymin; j <= ymax; j++)
	    {
	      pix_num=0;
	      pix_num2=0;

	      for (jj=1; jj <= global.par.cellsize; jj++)
		{
		  for(ii=1; ii <= global.par.cellsize; ii++)
		    {
		      if((i+ii-global.par.cellsize/2 -1 >= xmin)&&(i+ii-global.par.cellsize/2 -1<= xmax)
			&&(j+jj-global.par.cellsize/2 -1 >= ymin)&&(j+jj-global.par.cellsize/2 -1<= ymax))
			{
			  iii=i+ii-global.par.cellsize/2 -1;
			  jjj=j+jj-global.par.cellsize/2 -1;

			  pix_num++;

			  if((count[iii][jjj] != HOTPIX)&&(count[iii][jjj] != FLICKPIX))
			    {
			      pix_num2++;
			    }
			}		            			
		    }
		}	      
	      pix_num--;
	      pix_num2--;

	      img=(float)count[i][j] > 0 ? (float)count[i][j]:0.;
	      bac = ((float)bgd[i][j]/(float)pix_num);
	      bac2 = ((float)bgd[i][j]/(float)pix_num2);

	      if(img > (bac - sens))
		{
		  if(!(((bac >= 0. - sens) && (bac <= 0. + sens)) && img < (global.par.bthresh + sens)))
		    {  

		      if(xrt_gammq((img+1.0), bac, &tmp_gammq))
			{
			  headas_chat(CHATTY, "%s: Error: Unable to calculate gamma function.\n", global.taskname);
			  goto clean_end;
			} 
		      
		      pixel_prob=1.0 - tmp_gammq;
		      
		      if(pixel_prob < pmax)
			{

			  percent=0.;
			  pix_star=0;
			  		  
			  for (jj=1; jj <= global.par.cellsize; jj++)
			    {
			      for(ii=1; ii <= global.par.cellsize; ii++)
				{    
				  
				  if((i+ii-global.par.cellsize/2 -1 >= xmin)&&(i+ii-global.par.cellsize/2 -1<= xmax)
				     &&(j+jj-global.par.cellsize/2 -1 >= ymin)&&(j+jj-global.par.cellsize/2 -1<= ymax))
				    {
				      iii=i+ii-global.par.cellsize/2 -1;
				      jjj=j+jj-global.par.cellsize/2 -1;
				      
				      if((count[iii][jjj]>0)&&((iii != i)||(jjj != j)))
					{
					  pix_star++;
					}
				    }	
				}
			    }
			  percent = (((float)pix_star/(float)pix_num2)*((float)pix_star/(float)pix_num2))*(bac2*100./img);

			  
			  if(count[i][j]!=FLICKPIX){
			    headas_chat(CHATTY,"%s: Info: Flickering Pixels in rawx = %3d rawy = %3d percent %G pix_star %d pix_num %d bac %f\n", global.taskname, i, j, percent, pix_star, pix_num, bac);
			  }

			  if( percent < (10 + sens))
			    {
			      if(count[i][j]!=FLICKPIX)
				{
				  count[i][j]=FLICKPIX;
				  prefix = prefix + 1;
				  prebad = prebad + (int)img;
				  
				  headas_chat(NORMAL,"%s: Info: Iter %d Found Flickering Pixels in rawx = %3d rawy = %3d",global.taskname,iter,i,j);
			      
				  headas_chat(CHATTY," img = %d -- bac = %.3f -- pixel_prob = %G",(int)img, bac, pixel_prob);
			      
				  headas_chat(NORMAL,"\n");
				}
			    }
			  else
			    {
			      if(count[i][j]!=FLICKPIX){
				headas_chat(CHATTY,"%s: Info: candidate Flickering Pixels in rawx = %3d rawy = %3d not flagged\n", global.taskname, i, j);
			      }
			    }
			  
			}
		    }
		}
	    }
	}
  /*      headas_chat(NORMAL, "prefix = %d prebad=%d \n", prefix, prebad); */
      if (prefix != 0)
	{
	  fix = fix + prefix;
	  bad_counts = bad_counts + prebad;
	  headas_chat(CHATTY, "%s: Info: Flickering pixels iter: %d\n", global.taskname, iter);
	  headas_chat(CHATTY, "%s: Info: Flickering pixels pixel numb: %d\n", global.taskname, fix);
	  headas_chat(CHATTY, "%s: Info: Flickering pixels counts: %d\n", global.taskname, bad_counts);
	}
      if (global.par.iterate == 0)
	prefix = 0;
      iter = iter + 1;
    }


  *totcounts=fix;
  *totpixs=bad_counts;

/*    headas_chat(NORMAL, "prefix = %d prebad=%d \n", fix, bad_counts); */

  return OK;

 clean_end:
  return NOT_OK;
}
int ReadBPExt(FitsFileUnit_t evunit,int hducount, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS])
{
  int            x, y, n, status=OK, amp=0;
  short int      flag=0, flag_tmp=0;
  unsigned       FromRow, ReadRows, OutRows, nCols, count, ext; 
  BadCol_t       badcolumns; 
  BadPixel_t     pix;
  Bintable_t     table;          /* Bad table pointer */  
  FitsHeader_t   head;           /* Bad bintable pointer */


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* move to right badpixels extension */
  if (fits_movabs_hdu(evunit,hducount, NULL, &status))
    { 
      headas_chat(CHATTY,"%s: Error: Unable to find %d HDU\n",global.taskname,hducount);
      headas_chat(CHATTY,"%s: Error: in '%s' file\n",global.taskname,global.par.infile);      
      goto readbp_end;
    } 
  
  /* Read badpixels bintable */
  head = RetrieveFitsHeader(evunit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  /* Get numbers columns from name */
  if ((badcolumns.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }
  
  if ((badcolumns.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }

  if ((badcolumns.Amp = GetColNameIndx(&table, CLNM_Amp)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }
  
  if (( badcolumns.TYPE = GetColNameIndx(&table, CLNM_TYPE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TYPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }
  
  if ((badcolumns.XYEXTENT = GetColNameIndx(&table, CLNM_XYEXTENT)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XYEXTENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }
  
  if(( badcolumns.BADFLAG = GetColNameIndx(&table, CLNM_BADFLAG)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto readbp_end;
    }
  
  /* Check if bad pixels table is empty */
  if(!table.MaxRows)
    {
      headas_chat(CHATTY, "%s: Error: bad pixels table of the\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: %s file\n", global.taskname, global.par.infile);
      headas_chat(CHATTY, "%s: Error: is empty.\n",global.taskname);
      goto readbp_end;
    }
  
  if(global.par.cleanflick)
    {
      flag|=EV_FLICK_BP;
    }

  flag|=EV_HOT_BP;
  flag|=BAD_BP;


  /* Read blocks of bintable rows from input badpixels file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  
  while (ReadBintable(evunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{

	  flag_tmp=XVEC2BYTE(table,n,badcolumns.BADFLAG);
	  flag_tmp&=~flag;
	  if(flag_tmp)
	    {

	      /* get columns value */
	      x=IVEC(table,n,badcolumns.RAWX);
	      y=IVEC(table,n,badcolumns.RAWY);
	      pix.bad_type = IVEC(table,n,badcolumns.TYPE);
	      if(pix.bad_type == BAD_COL)
		pix.xyextent = IVEC(table,n,badcolumns.XYEXTENT);
	      else
		pix.xyextent=1;

	      pix.bad_flag = flag_tmp;

	      /* Get Amplifier number */
	      if (table.TagVec[badcolumns.Amp] == B)
		amp=BVEC(table, n,badcolumns.Amp);
	      else
		amp=IVEC(table, n,badcolumns.Amp);
	      if (amp != AMP1 && amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error:  Amplifier Number: %d not allowed.\n", global.taskname,amp);
		  headas_chat(CHATTY,"%s: Error:  Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
		  goto readbp_end;
		}
	  
	      if (AddBadPix(x,y,pix,bp_table))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to  write pixels in bad pixels map.\n",global.taskname);
		  goto readbp_end;
		}
	      
	      /* If xyextent > 1, check if pixel is into column ... */	
	      if (pix.xyextent > 1 ) 
		{
		  ext=pix.xyextent;
		  for (count=1; count<ext; count++)
		    {
		      y++;
		      pix.xyextent = 0;
		      if (AddBadPix(x,y,pix,bp_table))
			{
			  headas_chat(NORMAL, "%s: Error: Unable to  write pixels in bad pixels map.\n",global.taskname);
			  goto readbp_end;
			}
		    }
		}
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  
  return OK;
  
 readbp_end:
  

  
  return NOT_OK;
}/* ReadBPExt */
