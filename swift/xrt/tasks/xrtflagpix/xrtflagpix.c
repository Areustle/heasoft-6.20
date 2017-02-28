/*
 * 
 *	xrtflagpix.c: --- Swift/XRT ---
 *
 *	INVOCATION:
 *
 *		xrtflagpix [parameter=value ...]
 *
 *	DESCRIPTION:
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *        0.1.0 - BS 08/02/2002 - First working version
 *        0.1.2 - 18/04/2002    - SPR_XRT_002 Compilation and run-time 
 *                                problems on DEC Alpha machines
 *        0.1.3 - 05/07/2002    - Changed strcmp with strcasecmp to ignore
 *                                case of characters when needed
 *                              - Modified return values when output files exist
 *                                and 'clobber' = no   
 *                              - Added management return values from functions
 *                                'remove' and 'rename'.
 *                              - Modified temporary file name
 *        0.1.4 - 10/09/2002    - Added functions to locate CALDB bad pixel file 
 *                                by observation date and time 
 *        0.1.5 - 30/10/2002    - Changed functions name following HEADAS style
 *                              - Used DET coordinates to flag events
 *        0.2.0 - 25/02/2003    - Added and modified functions to handle 
 *                                Windowed Photon Counting Mode and Windowed 
 *                                Timing Mode  
 *                              - Modified to read input files name 
 *                                from ASCII file
 *                              - Modified to overwrite input file
 *                              - Modified to compute DETX and DETY coordinates using
 *                                teldef 
 *                              - Added check on input file to do not overwrite it when it is compressed
 *                              - changed XVEC call into XVEC2BYTE
 *                              - Deleted 'GetBPFromEvents' routine   
 *                              - Added check on XRTDETXY keyword to decide if to compute 
 *                                detx/dety coordinates
 *                              - Added checks on amplifier number and datatype STATUS column
 *                              - Added flags on calibration source events   
 *        0.3.0 - 30/04/2003    - Bug fixed (Modified sources center)
 *        0.3.1 - 13/05/2003    - Disabled function GetBiasMap
 *                              - Added 'bptable' parameter
 *        0.4.1 - 25/06/2003    - Moved 'GetBadPix', 'CreateBPFile', 'WriteBPKeywords', 
 *                                'AddBadPix', 'CreateFile' and 'BadpixBintable'  routines to lib/xrtbpfunc/
 *                              - Deleted routine to compute DETX/Y coordinates
 *                              - Modified to flag events using RAWX/Y coordinates
 *        0.4.2 - 09/07/2003    - Added function to get 'bptable' from CALDB 
 *        0.4.3 - 15/07/2003    - Modified to use new prototype of 'CalGetFileName'
 *        0.4.4 - 21/07/2003    - Added name of used bad pixels files in the history of 'EVENTS' hdu
 *        0.4.5 - 09/10/2003    - Using WM1STCOL and WMCOLNUM keywords to compute WT pixels range
 *        0.4.6 - 30/10/2003    - Modified method to flag calibration sources
 *        0.4.7 - 03/11/2003    - Replaced call to 'headas_parstamp()' with
 *                                'HDpar_stamp()'
 *        0.4.8 - 10/11/2003    - Add or update 'XRTFLAG' into output events file 
 *        0.4.9 - 12/11/2003    - Using calibration source detx/y (Analysis performed by David Burrows 
 *                                using data from 30 May 2003 DitL test)
 *        0.4.10- 25/11/2003    - Bug fixed getting default out bad pixels filename
 *        0.4.11- 22/03/2004    - Moved 'CopyHdu' in 'highfits.c'
 *        0.4.12- 23/03/2004    - modified to read only one input fits file at once
 *                                and to handle new bad pixels fits file format. 
 *        0.4.13- 06/04/2004    - Modified input parameters description and 
 *                                 some input parameters name
 *        0.4.14- 12/05/2004    - Messages displayed revision
 *        0.4.15- 24/06/2004    - Bug fixed on 'fits_update_keyword' call for 
 *                                LOGICAL keywords
 *        0.5.0 - 20/07/2004    - Added routine to append or update BADPIX
 *                                in output events file
 *        0.5.1 - 23/07/2004    - Added bad pixels search around 3x3 using baseline value
 *                                from 'hdfile' file. (See Burrows mail "Bad Pixel implementation" 08-07-2003)
 *        0.5.2 - 29/07/2004    - Added routine to read Calibration sources information
 *                                from CALDB file
 *        0.5.3 - 03/08/2004    - Added check on datamode before asking 'hdfile'
 *                                parameter
 *        0.5.4 - 04/08/2004    - Read 'XRTMODE' column in 'hdfile'
 *        0.5.5 - 30/09/2004    - Changed CALDB Badpixel Table  and On Ground Badpixels
 *                                query to take into account the change of the BPTABLE CCNM0001 value 
 *                                and a new CBD20001 keyword added to distingush 
 *                                ONBOARD badpixels from ONGROUND ones 
 *        0.5.6 - 01/10/2004    - Change default value to wm1stcol and n_col
 *        0.5.7 - 13/10/2004    - set as default global.amp to 1
 *        0.5.8 - 08/11/2004    - Bug fixed on Amp column in BADPIX ext if 
 *                                readout mode is windowed
 *                              - Bug fixed when update BADPIX extension
 *                              - Displayed info about bad pixels list and events flagged
 *                                list when chatter>=4 and events flagged percentage when 
 *                                chatter>=2
 *        0.5.9 - 23/02/2005    - Replaced GetSVal with ExistsKeyword routine
 *        0.5.10-               - Modified xrtflagpix_info prototype to display
 *                                hd file only for PC
 *        0.5.11- 18/03/2005    - Added input parameter 'event' to flag central pixels with 
 *                                PHA below this value 
 *        0.5.12- 23/03/2005    - Bug fixed
 *        0.6.0 - 12/05/2005    - Added routine to set to zero only bad pixels searched
 *                              - Added chance to avoid to flag calibration sources
 *        0.6.1 - 21/06/2005    - Replaced GetBaselineValues with ReadHK
 *                              - Added 'maxtemp' input parameter
 *                              - Added chance to flag burned spot (maxtemp!=0.0) if 
 *                                ccdtemp higher than 'maxtemp'
 *        0.6.2 - 23/06/2005    - Bug fixed when overstatus=yes for WT mode
 *        0.6.3 - 13/07/2005    - Replaced ReadBintable with ReadBintableWithNULL
 *                                to perform check on undefined value reading hdfile
 *        0.6.4 - 15/07/2005    - Read OBS_MODE keyword before ReadHK routine
 *        0.6.5 - 03/08/2005    - Update bad pixels list
 *                              - Exit with error if hdfile is empty
 *                              - Create outfile if task exits with warning  
 *        0.6.6 - 05/10/2005    - Added "XRTMAXTP" keyword to put 'maxtemp' input
 *                                parameter value 
 *        0.6.7 - 20/10/2005    - Deleted BP_POINT bad pixels flag
 *        0.6.8 - 27/10/2005    - Added XRTMAXTP keyword only in the PC event file
 *        0.6.9 - 14/02/2006    - Modified to handle  TDRSS Photon Counting 
 *                                format
 *        0.6.10- 17/05/2006 AB - badpixel's TIME column handling implemented
 *        0.7.0 - 19/05/2006 FT - bug fixed
 *        0.7.1 - 26/05/2006 AB - Flagging of WT events at interval's borders debugged and redefined. 
 *        0.7.2 - 12/03/2007 NS - Changed ReadHK function to accept Settled=1 In10Arcm=0 InSafeM=1
 *                                for events with obsmode=SLEW
 *        0.7.3 - 10/07/2007    - Modified EventBintable function
 *                              - Added GetCalFovInfo function
 *                              - New input parameter 'thrfile'
 *        0.7.4 - 29/09/2008    - Added raw coordinates out of range check in 'AddNeighBadPix' function
 *                              - Changed ReadHK function to evaluate obsmode=SLEW when InSafeM=1 
 *        0.7.5 - 01/04/2010    - Events on WT window's borders are flagged
 *                              - Events on second column of WT window are flagged
 *
 *  
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtflagpix          /* headas_main() requires that TOOLSUB 
				       be defined first */
#define XRTFLAGPIX_C
#define XRTFLAGPIX_VERSION  "0.7.5"
#define PRG_NAME            "xrtflagpix"

/******************************
 *        header files        *
 ******************************/
#include "headas_main.c"
#include "xrtflagpix.h"

/******************************
 *       definitions          *
 ******************************/

Global_t global;

/*
 *	xrtflagpix_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtflagpix.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 *           int PILGetBool(char *name, int *result);  
 *           int headas_chat(int, char * , ...);
 *           void xrtflagpix_info(void);
 *	
 *      CHANGE HISTORY
 *        0.1.0: - BS 08/02/2002 - First version
 *        0.1.1: -    13/05/2003 - Deleted 'biasmap' parameter and 
 *                                 added 'bptable' parameter
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

int xrtflagpix_getpar(void)
{
  int ret=0;
  /************************
   *   Get parameters.    *
   ************************/
  
  /* Get input events fits file name */ 
  if(PILGetFname(PAR_INFILE, global.par.infile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  /* Get CALDB bad pixels file name */
  if(PILGetFname(PAR_BPFILE, global.par.bpfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BPFILE);
      goto Error;
    }

  /* Get CALDB calibration sources file name */
  if(PILGetFname(PAR_SRCFILE, global.par.srcfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCFILE);
      goto Error;
    }

  if (ReadDatamode(global.par.infile, global.datamode))
    {
      headas_chat(NORMAL,"%s: Error: Unable to read %s keyword\n", global.taskname, KWNM_DATAMODE); 
      headas_chat(NORMAL,"%s: in %s file.\n", global.taskname,global.par.infile);
      goto Error;
    }

  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      /* Get hd file name */
      if(PILGetFname(PAR_HDFILE, global.par.hdfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	  goto Error;
	}
      
      /* Input Thresholds file */
      if(PILGetFname(PAR_THRFILE, global.par.thrfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_THRFILE);
	  goto Error;	
	}

      global.usethrfile=1;

      if ( !strcasecmp(global.par.thrfile,DF_NONE) )
	{
	  global.usethrfile=0;
	  
	  /* Event threshold */ 
	  if(PILGetInt(PAR_EVENT, &global.par.event))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVENT);
	      goto Error;
	    }
	}

      if(PILGetReal(PAR_MAXTEMP, &global.par.maxtemp))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MAXTEMP);
	  goto Error;
	}


    }

  /* Get user bad pixels file name */ 
  if(PILGetFname(PAR_USERBPFILE, global.par.userbpfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_USERBPFILE);
      goto Error;
    }

  /* Get on-board bad pixels file name */
  if(PILGetFname(PAR_BPTABLE, global.par.bptable))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BPTABLE);
      goto Error;
    }

  /* Get output bad pixels file name */
  if(PILGetFname(PAR_OUTBPFILE, global.par.outbpfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTBPFILE);
      goto Error;
    }

  /* Get output file name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;
    }

   /* Overwrite status column */ 
   if(PILGetBool(PAR_OVERSTATUS, &global.par.overstatus))
     {
       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OVERSTATUS);
       goto Error;
     }



   get_history(&global.hist);

   xrtflagpix_info(ret);

   return OK;

 Error:
   return NOT_OK;
}/*xrtflagpix_getpar*/

/*
 *	xrtflagpix_info 
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
void xrtflagpix_info(int ret)
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file            :'%s'\n", global.par.infile);
  if(ret)
    headas_chat(NORMAL,"Name of the input HK Header      file   :'%s'\n",global.par.hdfile);
  headas_chat(NORMAL,"Name of the input BADPIXEL file         :'%s'\n", global.par.bpfile);
  headas_chat(NORMAL,"Name of the input user bad pixels file  :'%s'\n", global.par.userbpfile);
  headas_chat(NORMAL,"Name of the input BPTABLE file          :'%s'\n", global.par.bptable);
  headas_chat(NORMAL,"Name of the input region  file          :'%s'\n", global.par.srcfile);
  headas_chat(NORMAL,"Name of the output Event file           :'%s'\n", global.par.outfile);
  headas_chat(NORMAL,"Name of the output bad pixels file      :'%s'\n", global.par.outbpfile);
  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      headas_chat(NORMAL,"Name of the input thresholds file       :'%s'\n",global.par.thrfile);
      if(!global.usethrfile)
	headas_chat(NORMAL,"Event threshold                         :'%d'\n", global.par.event);
    }
  if (global.par.overstatus == 0)
    headas_chat(NORMAL,"Overwrite STATUS column?                : no\n");
  else
    headas_chat(NORMAL,"Overwrite STATUS column?                : yes\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output files?                 : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output files?                 : no\n");

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file?  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file?  : no\n");
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  return;

}/* xrtflagpix_info */

/*
 *	xrtflagpix_checkinput
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
int xrtflagpix_checkinput(void)
{

  int            overwrite=0;
  char           stem[10],  ext[MAXEXT_LEN] ;  
  pid_t          tmp;      

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
		headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
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
  
  return OK;

 error_check:
  return NOT_OK;


}/* xrtflagpix_checkinput */
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
int CreateEVFile(FitsFileUnit_t evunit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS]) 
{
  int            status=OK, hdutot, hducount, hduev, hdubp=0;  
  int            val=0, logical, i=0, j=0;
  BOOL           nobphdu=1;
  FitsFileUnit_t outunit=NULL;
  

  global.amp = 1;

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
  
  /* Build Event Extension and add badflag column */

  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      /* Retrieve obs-mode from input events file */    
  
      
       if (AddNeighBadPix(evunit, bp_table_amp1, bp_table_amp2 ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to check neighbour bad pixels\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto end_cevf;
	}
       if(global.warning)
	 goto ok_end;
      
      if (EventBintable(evunit, outunit, bp_table_amp1, bp_table_amp2 ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to write %s HDU\n", global.taskname, KWVL_EXTNAME_EVT);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto end_cevf;
	}

      if(global.warning)
	goto ok_end;


      /* FT no more needed, the pure burned spots are erased (if maxtemp == 0 )  
	  elsewhere */
/*        if(global.par.maxtemp > 0.0 - sens && global.par.maxtemp < 0.0 + sens) */
/*  	{ */
/*  	  CutBPFromTable(bp_table_amp1, (EV_BURNED_BP|EV_CAL_BP)); */
/*  	  CutBPFromTable(bp_table_amp2, (EV_BURNED_BP|EV_CAL_BP)); */
/*  	} */

    }
  else
    {  
      if (WindowedBintable(evunit, outunit, bp_table_amp1, bp_table_amp2 ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to write %s HDU\n", global.taskname, KWVL_EXTNAME_EVT);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto end_cevf;
	}

      if(global.warning)
	goto ok_end;

    }


  headas_chat(CHATTY, "%s: Info: List of Bad pixels used to flag events:\n", global.taskname);
  headas_chat(CHATTY, "%s: Info: Amplifier 1:\n", global.taskname);
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if(bp_table_amp1[i][j] != NULL)
	headas_chat(CHATTY, "%s: Info: RAWX = %d   RAWY = %d \n", global.taskname, i, j);
    }
  }
  
  headas_chat(CHATTY, "%s: Info: Amplifier 2:\n", global.taskname);
  for (i=0; i<CCD_PIXS; i++){
    for (j=0; j<CCD_ROWS; j++){
      if(bp_table_amp2[i][j] != NULL)
	headas_chat(CHATTY, "%s: Info: RAWX = %d   RAWY = %d \n", global.taskname, i, j);
    }
  }
  
  if(fits_read_key_log(evunit, KWNM_XRTFLAG, &val, NULL, &status))
    {
      if(status == KEY_NO_EXIST)
	{
	  status=0;
	  val=0;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_XRTFLAG);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto end_cevf;
	}
    }
  
  if(!val)
    if(WriteStatus(outunit))
      {
	headas_chat(CHATTY, "%s: Error: Unable to write COMMENT lines\n", global.taskname);
	headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto end_cevf;
      }

  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTFLAG, &logical, CARD_COMM_XRTFLAG, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTFLAG);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_cevf;
    }
  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      logical=TRUE;
      if(fits_update_key(outunit, TDOUBLE, KWNM_XRTMAXTP, &global.par.maxtemp, CARD_COMM_XRTMAXTP, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTMAXTP);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto end_cevf;
	}
    }

  /***********************************/
  /* Create complete bad pixels file */
  /***********************************/
    
  /* Check if badpixels file shall be created */
  if ( strcasecmp(global.par.outbpfile,DF_NONE)) 
    {  /* Create complete bad pixels file */
      if(CreateBPFile(evunit, bp_table_amp1, bp_table_amp2, global.par.outbpfile))
	headas_chat(NORMAL,"%s: Warning: Unable to create '%s' file.\n", global.taskname, global.par.outbpfile);
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
	    
	    if(global.amp == AMP1)
	      {      

		if(ReadBPExt(evunit, hdubp,bp_table_amp1))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' input file.\n",global.taskname, global.par.infile);
		    goto end_cevf;
		  }

		if(CreateBPExt(outunit,bp_table_amp1,KWVL_EXTNAME_BAD, global.amp))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		    goto end_cevf;
		    
		  }
		headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
	      }
	    else
	      {

		if(ReadBPExt(evunit, hdubp, bp_table_amp2))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' input file.\n",global.taskname, global.par.infile);
		    goto end_cevf;
		  }
		if(CreateBPExt(outunit,bp_table_amp2,KWVL_EXTNAME_BAD, global.amp))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		    goto end_cevf;
		    
		  }
		headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
	      }   
	    
	    
	  }
	else
	  {
	    if(global.amp == AMP1)
	      {

		if(UpdateBPExt(outunit,evunit, bp_table_amp1,KWVL_EXTNAME_BAD, global.amp, hducount))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to update '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		    goto end_cevf;
		    
		  }
		headas_chat(MUTE,"%s: Info: %s extension successfully updated\n",global.taskname, KWVL_EXTNAME_BAD);
	      }
	    else
	      {
		if(UpdateBPExt(outunit, evunit, bp_table_amp2,KWVL_EXTNAME_BAD, global.amp, hducount))
		  {
		    headas_chat(NORMAL,"%s: Error: Unable to update '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		    headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
		    goto end_cevf;
		
		  }
		headas_chat(MUTE,"%s: Info: %s extension successfully updated\n",global.taskname, KWVL_EXTNAME_BAD);
	      }
	  }
      }
    }
  
  if(nobphdu || hdubp < hduev)
    { 
      if(global.amp == AMP1)
	{      
	  if(CreateBPExt(outunit,bp_table_amp1,KWVL_EXTNAME_BAD, global.amp))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
	      headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
	      goto end_cevf;
	      
	    }
	  headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
	}
      else
	{
	  if(CreateBPExt(outunit,bp_table_amp2,KWVL_EXTNAME_BAD, global.amp))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to append '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
	      headas_chat(NORMAL,"%s: Error: in '%s' temporary file.\n",global.taskname, global.tmpfile);
	      goto end_cevf;
	      
	    }
	  headas_chat(MUTE,"%s: Info: %s extension successfully written\n",global.taskname, KWVL_EXTNAME_BAD);
	}
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

  headas_chat(NORMAL,"%s: Info: Added standard keywords in %s extension\n",global.taskname,KWVL_EXTNAME_BAD);

  if(HDpar_stamp(outunit, hdubp, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto end_cevf;
    }

  /* Calculate checksum and add it in file */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto end_cevf;
    }
  
  
  
  /* Close events tmp file */
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto end_cevf;
    }

 ok_end:
  

  return OK;
  
 end_cevf:
  
  return NOT_OK;
}/* CreateEVFile */
/*
 *	EventBintable:
 *
 *
 *	DESCRIPTION:
 *           Routine to create event bintable with bad pixels flagged and to append it
 *           in event temporary file
 *             
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *             FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure(FitsHeader_t *header, Bintable_t *table, const int MaxBlockRows,
 *					 const unsigned nColumns, const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype, 
 *                            const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 * 			        const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned ReleaseBintable(FitsHeader_t *head,Bintable_t *table);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 28/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int EventBintable(FitsFileUnit_t evunit,FitsFileUnit_t ounit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS])
{
  int                jj, bcal, newstatus, bad=0, ccdframe=0, ccdframe_old=0, ii=0;
  int                neigh_x[] = {NEIGH_X};
  int                neigh_y[] = {NEIGH_Y};
  int                first_x, last_x, first_y, last_y, amp=1;
  BOOL               flag_amp1=0, flag_amp2=0, first=1, first_time=1, nophas=0;
  char               comm[256]="";
  ITYPE              half_x=0, half_y=0;
  double             time=0.0, time_old=0.0,sens=0.0000001,evtemp=0.0;
  EvCol_t            indxcol;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,  rawx, rawy, detx, dety;  
  short int          ev_bad_flag, tmp_flag;
  Bintable_t	     outtable;
  FitsHeader_t	     head;


  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);


  
  if(GetCalSourcesInfo())
    {
      headas_chat(NORMAL, "%s: Error: Unable to read Calibration sources information\n", global.taskname);
      goto event_end;
    }

  if(GetCalFovInfo())
    {
      headas_chat(NORMAL, "%s: Error: Unable to read Field of View information\n", global.taskname);
      goto event_end;
    }
  
  if(!(half_x=GetJVal(&head, KWNM_WHALFWD)))
    half_x=300;
  
  if(!(half_y=GetJVal(&head, KWNM_WHALFHT)))
    half_y=300;
  
  /* Compute rawx and rawy range */
  first_x=CCD_CENTERX - half_x;
  last_x=CCD_CENTERX + half_x - 1;
  
  first_y=CCD_CENTERY - half_y;
  last_y=CCD_CENTERY + half_y - 1;
  
  /* Flag sources                 */
  if(global.nosrcflag)
    {
      flag_amp1 = 0;
      flag_amp2 = 0;
    }
  else
    {
      /* Check if sources shall be flagged ( Amplifier independent? )*/
/*        if(((first_x - global.calsrc[0].detx + 1)*(first_x - global.calsrc[0].detx + 1) + (first_y - global.calsrc[0].dety + 1)*(first_y - global.calsrc[0].dety + 1) <= global.calsrc[0].r*global.calsrc[0].r) || ((first_x - global.calsrc[1].detx + 1)*(first_x - global.calsrc[1].detx + 1) + (last_y - global.calsrc[1].dety + 1)*(last_y - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r) || ((last_x - global.calsrc[2].detx + 1)*(last_x - global.calsrc[2].detx + 1) + (last_y - global.calsrc[2].dety + 1)*(last_y - global.calsrc[2].dety + 1) <= global.calsrc[2].r*global.calsrc[2].r) || ((last_x - global.calsrc[3].detx + 1)*(last_x - global.calsrc[3].detx + 1) + (first_y - global.calsrc[3].dety + 1)*(first_y - global.calsrc[3].dety + 1) <= global.calsrc[3].r*global.calsrc[3].r)) */
	flag_amp1 = 1;
      
/*        if(((last_x + global.calsrc[0].detx - CCD_PIXS)*(last_x + global.calsrc[0].detx - CCD_PIXS) + (first_y - global.calsrc[0].dety + 1)*(first_y - global.calsrc[0].dety + 1) <= global.calsrc[0].r*global.calsrc[0].r) || ((last_x + global.calsrc[1].detx - CCD_PIXS)*(last_x + global.calsrc[1].detx - CCD_PIXS) + (last_y - global.calsrc[1].dety + 1)*(last_y - global.calsrc[1].dety + 1) <= global.calsrc[1].r*global.calsrc[1].r) || ((first_x + global.calsrc[2].detx - CCD_PIXS)*(first_x + global.calsrc[2].detx - CCD_PIXS) + (last_y - global.calsrc[2].dety + 1)*(last_y - global.calsrc[2].dety + 1) <= global.calsrc[2].r*global.calsrc[2].r) || ((first_x + global.calsrc[3].detx - CCD_PIXS)*(first_x + global.calsrc[3].detx - CCD_PIXS) + (first_y - global.calsrc[3].dety + 1)*(first_y - global.calsrc[3].dety + 1) <= global.calsrc[3].r*global.calsrc[3].r)) */
	flag_amp2 = 1;
    }
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  
  if(!outtable.MaxRows)
    {
      
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  if((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Warning: '%s' column does not exist\n", global.taskname, CLNM_PHAS);
      headas_chat(CHATTY, "%s: Warning: in '%s' file. \n", global.taskname, global.par.infile);
      headas_chat(CHATTY, "%s: Warning: unable to search obs bad pixels. \n", global.taskname);
      nophas=1;
    }
  
  /* Add column STATUS */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
      sprintf(comm, "Added %s column", CLNM_STATUS);
    }
  
  if (global.par.overstatus)
    {
      headas_chat(NORMAL, "%s: Info: '%s'  parameter is set\n", global.taskname, PAR_OVERSTATUS);
      headas_chat(NORMAL, "%s: Info: the %s columnn will be overwritten.\n", global.taskname, CLNM_STATUS);
      sprintf(comm, "Overwritten  %s column.", CLNM_STATUS);
    }      
  else
    {
      headas_chat(NORMAL, "%s: Info: '%s' parameter is unset\n", global.taskname, PAR_OVERSTATUS);
      headas_chat(NORMAL, "%s: Info: the %s columnn will be update.\n", global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: Set '%s' parameter to overwrite it.\n", global.taskname, PAR_OVERSTATUS);
      sprintf(comm, "Updated %s column", CLNM_STATUS);
    }
  
  EndBintableHeader(&head, &outtable); 
  
  GetGMTDateTime(global.date);
  /* Add history */
  if(global.hist)
    {
      sprintf(global.strhist, "File modified  by %s (%s) at %s", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      AddHistory(&head, comm);
      if(strcasecmp(global.par.bpfile,DF_NONE) || strcasecmp(global.par.userbpfile,DF_NONE) || strcasecmp(global.par.bptable,DF_NONE))
	{
	  sprintf(comm, "Used following file(s) to get bad pixels: ");
	  AddHistory(&head, comm);
	  if(strcasecmp(global.par.bpfile,DF_NONE) )
	    {
	      sprintf(comm, "%s CALDB bad pixels file", global.par.bpfile);
	      AddHistory(&head, comm);
	    }
	  
	  if(strcasecmp(global.par.userbpfile,DF_NONE) )
	    {
	      sprintf(comm, "%s user bad pixels file",global.par.userbpfile);
	      AddHistory(&head, comm);
	    }
	  
	  if(strcasecmp(global.par.bptable,DF_NONE) )
	    {
	      sprintf(comm, "%s on-board bad pixels table",global.par.bptable);
	      AddHistory(&head, comm);
	    }
	}
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

	  ccdframe_old=ccdframe;
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);
	  time_old=time;
	  time=DVEC(outtable, n, indxcol.TIME);
	  /*;;;;;;*/
	  if(first_time || (ccdframe_old != ccdframe) || !((time >=  time_old - sens) && (time <= time_old + sens)))
	    {	
	      first_time=0;
	      for (; ii < global.count; ii++)
		{
		  if(ccdframe == global.bline[ii].hkccdframe &&  
		     (global.bline[ii].hktime >= (time-sens) && global.bline[ii].hktime <= (time+sens)))
		    {
		      evtemp=global.bline[ii].temp;
		      headas_chat(CHATTY, "%s: Info: Frame %4d - CCD temperature is: %f.\n", global.taskname, ccdframe, evtemp);
		      if((evtemp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
			{
			  headas_chat(NORMAL, "%s: Warning: the CCD temperature (%f) is higher than (%f)\n",global.taskname,evtemp,  global.par.maxtemp);
			  headas_chat(NORMAL, "%s: Warning: the burned spot will be flagged\n", global.taskname);
			}
		      goto row_found;
		    }
		}
	      
	      headas_chat(NORMAL, "%s: Error: Unable to get temperature value.\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file does not contain\n", global.taskname, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Error: %d ccdframe information\n", global.taskname, ccdframe);
	      
	      goto event_end;
	    }
	  
	  
	row_found:
	  first=1;
	  if (strcmp(outtable.tforms[indxcol.STATUS], "16X"))
	    {
	      headas_chat(NORMAL, "%s: Error: %s column datatype is %s, but should be 16X.\n",global.taskname, CLNM_STATUS,outtable.tforms[indxcol.STATUS] ); 
	      goto  event_end;
	    }
	  
	  /* Get Amplifier number */
	  if (outtable.TagVec[indxcol.Amp] == B)
	    amp=BVEC(outtable, n,indxcol.Amp);
	  else
	    amp=IVEC(outtable, n,indxcol.Amp);
	  if (amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	      goto event_end;
	    }
	  
	  rawx = IVEC(outtable,n,indxcol.RAWX);
	  rawy = IVEC(outtable,n,indxcol.RAWY);
	  
	  if(amp == AMP1)
	    {
	      detx = rawx+1;
	      dety = rawy+1;
	    }
	  else
	    {
	      detx = CCD_PIXS-rawx;
	      dety = rawy+1;
	    }
	  
	  if(newstatus)
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	    }
	  else if(global.par.overstatus)
	    {

	      if(strcasecmp(global.par.bpfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CAL_BP);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BURNED_BP);

		}

	      if(strcasecmp(global.par.srcfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_1);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_2);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_3);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_4);

		}

	      if(strcasecmp(global.par.userbpfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_USER_BP);
		}

	      if(strcasecmp(global.par.bptable,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_ONBOARD_BP);
		}
	      
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~BP_COL);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BELOW_TH);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_NEIGH_BP);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BAD);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_TELEM_BP);
	      
	    }

	  if(!nophas)
	    {
	      if(IVECVEC(outtable,n,indxcol.PHAS,0) < global.par.event)
		XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BELOW_TH;
	    }
	  if ((detx>=first_x && detx<=last_x) && (dety>=first_y && dety<=last_y))
	    {
	      /* Check if event fall on the border */
	      if(detx==first_x || detx==last_x || dety==first_y || dety==last_y)
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BAD;
		  first=0;
		  
		}
	      /* Check if event is near the border */
	      else if (detx==(first_x+1) || detx==(last_x-1) || dety==(first_y+1) || dety==(last_y-1))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BAD;
		  first=0;
		}
	      
	      /*  Check if in REGION_CAL  */

	      if((amp == AMP1 && flag_amp1)||(amp == AMP2 && flag_amp2))
		{
		  if((detx - global.calsrc[0].detx)*(detx - global.calsrc[0].detx) +(dety - global.calsrc[0].dety)*(dety - global.calsrc[0].dety) <=global.calsrc[0].r *global.calsrc[0].r)
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_CALIB_SRC_1;
		      first=0;
		    }
		  
		  else if((detx - global.calsrc[1].detx)* (detx - global.calsrc[1].detx)+ (dety - global.calsrc[1].dety)*(dety - global.calsrc[1].dety) <= global.calsrc[1].r*global.calsrc[1].r)
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_CALIB_SRC_2;
		      first=0;
		    }
		  else if((detx - global.calsrc[2].detx)*(detx - global.calsrc[2].detx) +(dety - global.calsrc[2].dety)*(dety - global.calsrc[2].dety) <=global.calsrc[2].r *global.calsrc[2].r)
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_CALIB_SRC_3;
		      first=0;
		    }
		  else if((detx - global.calsrc[3].detx)*(detx - global.calsrc[3].detx) +(dety - global.calsrc[3].dety) *(dety - global.calsrc[3].dety)<=global.calsrc[3].r *global.calsrc[3].r)
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_CALIB_SRC_4;
		      first=0;
		    }
		}

	      /* Check if out of REGION_FOV  */
	        
	      if(!global.nofovflag)
		{
		  if((detx - global.calfov.detx)*(detx - global.calfov.detx) +(dety - global.calfov.dety)*(dety - global.calfov.dety) >global.calfov.r *global.calfov.r)
		    {
		      XVEC2BYTE(outtable,n,indxcol.STATUS)|= EV_BAD;
		      first=0;
		    }
		}
   
	      
	      if(amp == AMP1)
		{		
		  
		  /* Check if event is a bad pixel or column */
		  if (bp_table_amp1[rawx][rawy]!=NULL) 
		    {
                       /* If temp is above threshold and maxtemp is not zero */
		      if((evtemp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
                        /* leave flag  alone  */
			ev_bad_flag=bp_table_amp1[rawx][rawy]->bad_flag;
		      else
		     /* otherwise  */
                          {
                            /* set to bad_flag */
                            ev_bad_flag=bp_table_amp1[rawx][rawy]->bad_flag;
                            /* if "Burned spot flag is on"  */
                            if (bp_table_amp1[rawx][rawy]->bad_flag&EV_BURNED_BP)
                              {
                            /* zero the  "Burned spot" flag   */
                                ev_bad_flag=(bp_table_amp1[rawx][rawy]->bad_flag&~EV_BURNED_BP);
                            /* zero  the event value only for pure burned spots */
                                if(bp_table_amp1[rawx][rawy]->counter == 0 )
                                  ev_bad_flag=GOOD;
                              }
                          }


		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= ev_bad_flag;
		      if(ev_bad_flag)
			first=0;
		      if (bp_table_amp1[rawx][rawy]->bad_type == BAD_COL && ev_bad_flag)
			XVEC2BYTE(outtable,n,indxcol.STATUS)|=BP_COL;		
		      		
		    }
		  bcal=0;
		  tmp_flag=EV_BURNED_BP|EV_CAL_BP;
		  /* Check neighbours */
		  for (jj=1; jj< PHAS_MOL; jj++)
			{

			  /* check burned spot */
			  if((evtemp < global.par.maxtemp+sens) || ((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
			    {
			      
			      if ( (rawx+neigh_x[jj]>first_x && rawx+neigh_x[jj]<last_x) 
				   &&
				   (rawy+neigh_y[jj]>first_y && rawy+neigh_y[jj]<last_y)
				   &&
				   (bp_table_amp1[rawx+neigh_x[jj]][rawy+neigh_y[jj]]!=NULL && bp_table_amp1[rawx+neigh_x[jj]][rawy+neigh_y[jj]]->bad_flag!=tmp_flag))
				bcal=1;
			    }
			  else
			    {
			      
			      if ( (rawx+neigh_x[jj]>first_x && rawx+neigh_x[jj]<last_x) 
				   &&
				   (rawy+neigh_y[jj]>first_y && rawy+neigh_y[jj]<last_y)
				   &&
				   (bp_table_amp1[rawx+neigh_x[jj]][rawy+neigh_y[jj]]!=NULL)
				   )
				
				bcal=1;
			    }
			}
		  
		      if(bcal)
			{
			  XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
			  first=0;
			}
		    
		}/* End if(amp == AMP1) */
	      else
		{		
		  
		  /* Check if event is a bad pixel or column */
		  if (bp_table_amp2[rawx][rawy]!=NULL) 
		    {
                       /* If temp is above threshold and maxtemp is not zero */
		      if((evtemp > global.par.maxtemp-sens) && !((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
                        /* leave flag  alone  */
			ev_bad_flag=bp_table_amp2[rawx][rawy]->bad_flag;
		      else
		     /* otherwise  */
                          {
                            ev_bad_flag=bp_table_amp2[rawx][rawy]->bad_flag;
                            /* if "Burned spot flag is on"  */
                            if (bp_table_amp2[rawx][rawy]->bad_flag&EV_BURNED_BP)
                              {
                            /* zero the  "Burned spot" flag   */
                                ev_bad_flag=(bp_table_amp2[rawx][rawy]->bad_flag&~EV_BURNED_BP);
                            /* zero the event value  only for pure burned spots */
                                if(bp_table_amp2[rawx][rawy]->counter == 0 )
                                  ev_bad_flag=GOOD;
                              }
                          }


		      XVEC2BYTE(outtable,n,indxcol.STATUS) |= ev_bad_flag;
		      if(ev_bad_flag)
			first=0;
		      if (bp_table_amp2[rawx][rawy]->bad_type == BAD_COL && ev_bad_flag)
			XVEC2BYTE(outtable,n,indxcol.STATUS)|=BP_COL;		
		      		
		    }
		  bcal=0;
		  tmp_flag=EV_BURNED_BP|EV_CAL_BP;
		  /* Check neighbours */
		  for (jj=1; jj< PHAS_MOL; jj++)
		    {
		      
		      /* check burned spot */
		      if((evtemp < global.par.maxtemp+sens) || ((global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens)))
			{
			  
			  if ( (rawx+neigh_x[jj]>first_x && rawx+neigh_x[jj]<last_x) 
			       &&
			       (rawy+neigh_y[jj]>first_y && rawy+neigh_y[jj]<last_y)
			       &&
			       (bp_table_amp2[rawx+neigh_x[jj]][rawy+neigh_y[jj]]!=NULL && bp_table_amp2[rawx+neigh_x[jj]][rawy+neigh_y[jj]]->bad_flag!=tmp_flag))
			    bcal=1;
			}
		      else
			{
			  
			  if ( (rawx+neigh_x[jj]>first_x && rawx+neigh_x[jj]<last_x) 
			       &&
			       (rawy+neigh_y[jj]>first_y && rawy+neigh_y[jj]<last_y)
			       &&
			       (bp_table_amp2[rawx+neigh_x[jj]][rawy+neigh_y[jj]]!=NULL)
			       )
			    
			    bcal=1;
			}
		    }
		      
		      if(bcal)
			{
			  XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
			  first=0;
			}
		    }/* End else(amp == AMP2) */
		
	    }
	    		
       	  else
	    {
	      headas_chat(CHATTY, "%s: Warning: event with rawx=%d rawy=%d is out of range.\n", global.taskname, rawx, rawy);
  	      XVEC2BYTE(outtable,n,indxcol.STATUS)|= EV_BAD; /* (int) */
	      first=0;
	    }
	  
          if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    }

	  if(!first)
	    {
	      bad++;
	      headas_chat(CHATTY, "%s: Flagged event with RAWX = %3d and RAWY = %3d \n", global.taskname,rawx, rawy);
	    }
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }

  global.amp=amp;
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  global.percent=100. * (double)bad/(double)outtable.MaxRows;
  ReleaseBintable(&head, &outtable);
  
  
  return OK;
 event_end:
  
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
} /* EventBintable */

/*
 *	WindowedBintable:
 *
 *
 *	DESCRIPTION:
 *           Routine to create event bintable for Windowed Timing Mode FITS file with 
 *           bad columns flagged. The routine also appends it in event temporary file
 *             
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *             FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure(FitsHeader_t *header, Bintable_t *table, const int MaxBlockRows,
 *					 const unsigned nColumns, const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype, 
 *                            const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 * 			        const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned ReleaseBintable(FitsHeader_t *head,Bintable_t *table);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 31/10/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int WindowedBintable(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS])
{

  int                jj=0, newstatus, search, bad=0;
  int                first_x, last_x, first_y, last_y;
  char               comm[256]="";
  BOOL               first=1;
  EvCol_t            indxcol;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,  rawx, rawy, amp=1;  
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  

  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  first_x = GetJVal(&head, KWNM_WM1STCOL);
  if (!first_x || (first_x < WM1STCOL_MIN) || (first_x >  WM1STCOL_MAX)) 
    {
      headas_chat(NORMAL, "%s: Warning: Window's 1st column %d is out of range.\n", global.taskname, first_x);
      first_x = DEFAULT_WM1STCOL;
      headas_chat(NORMAL, "%s: Warning: Using default value %d instead .\n", global.taskname, first_x);
    }
  last_x=GetJVal(&head, KWNM_WMCOLNUM)+first_x -1 ;
  if((last_x==first_x) ||  (last_x < RAWX_WT_MIN) || (last_x > RAWX_WT_MAX))
    {
      headas_chat(NORMAL, "%s: Warning: Window's end column %d is out of range.\n", global.taskname, last_x);
      last_x= DEFAULT_WM1STCOL+DEFAULT_NCOL -1;
      headas_chat(NORMAL, "%s: Warning: Using default value %d instead .\n", global.taskname, last_x);
    }

  first_y=RAWY_MIN;
  last_y=RAWY_MAX;
      
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

 
  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto windowed_end;
    }
  
  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto windowed_end;
    }
  
  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto windowed_end;
    }
  
  /* Add column STATUS */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);  
      sprintf(comm, "Added %s column", CLNM_STATUS);
    }
 
  if (global.par.overstatus)
    {
      headas_chat(NORMAL, "%s: Info: '%s' parameter is set\n", global.taskname, PAR_OVERSTATUS);
      headas_chat(NORMAL, "%s: Info: the %s columnn will be overwritten.\n", global.taskname, CLNM_STATUS);
      sprintf(comm, "Overwritten  %s column.", CLNM_STATUS);
    }      
  else
    {
      headas_chat(NORMAL, "%s: Info: '%s' parameter is unset\n", global.taskname, PAR_OVERSTATUS);
      headas_chat(NORMAL, "%s: Info: the %s columnn will be update.\n", global.taskname, CLNM_STATUS);
      headas_chat(CHATTY, "%s: Info: Set '%s' parameter to overwrite it.\n", global.taskname, PAR_OVERSTATUS);
      sprintf(comm, "Updated %s column", CLNM_STATUS);
    }
    
  
  EndBintableHeader(&head, &outtable); 

  GetGMTDateTime(global.date);
  /* Add history */
  if(global.hist)
    {
      sprintf(global.strhist, "File modified  by %s (%s) at %s", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      AddHistory(&head, comm);
      if(strcasecmp(global.par.bpfile,DF_NONE) || strcasecmp(global.par.userbpfile,DF_NONE) || strcasecmp(global.par.bptable,DF_NONE))
	{
	  sprintf(comm, "Used following file(s) to get bad pixels: ");
	  AddHistory(&head, comm);
	  if(strcasecmp(global.par.bpfile,DF_NONE) )
	    {
	      sprintf(comm, "%s CALDB bad pixels file", global.par.bpfile);
	      AddHistory(&head, comm);
	    }
	  
	  if(strcasecmp(global.par.userbpfile,DF_NONE) )
	    {
	      sprintf(comm, "%s user bad pixels file",global.par.userbpfile);
	      AddHistory(&head, comm);
	    }
	  
	  if(strcasecmp(global.par.bptable,DF_NONE) )
	    {
	      sprintf(comm, "%s on-board bad pixels file",global.par.bptable);
	      AddHistory(&head, comm);
	    }
	}
    }
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  

  /* Check bad pixels and flag them */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows)==0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  first=1;
	  if (strcmp(outtable.tforms[indxcol.STATUS], "16X"))
	    {
	      headas_chat(NORMAL, "%s: Error: %s column datatype is %s, but should be 16X.\n",global.taskname, CLNM_STATUS,outtable.tforms[indxcol.STATUS] ); 
	      goto  windowed_end;
	    }

	  /* Compute RAWX and RAWY coordinates if it is needed */
	  if (outtable.TagVec[indxcol.Amp] == B)
	    amp=BVEC(outtable, n,indxcol.Amp);
	  else
	    amp=IVEC(outtable, n,indxcol.Amp);
	  
	  if (amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	      goto  windowed_end;
	    }
	  
	  rawx = IVEC(outtable,n,indxcol.RAWX);
	  rawy = IVEC(outtable,n,indxcol.RAWY);
	  
  	  if(newstatus)
	    {
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	    }
	  else if(global.par.overstatus)
	    {
	      
	      if(strcasecmp(global.par.bpfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CAL_BP);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BURNED_BP);
		  
		}
	      
	      if(strcasecmp(global.par.srcfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_1);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_2);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_3);
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_CALIB_SRC_4);
		  
		}
	      
	      if(strcasecmp(global.par.userbpfile,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_USER_BP);
		}
	      
	      if(strcasecmp(global.par.bptable,DF_NONE))
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_ONBOARD_BP);
		}
	      
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~BP_COL);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BELOW_TH);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_NEIGH_BP);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_BAD);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)=(XVEC2BYTE(outtable,n,indxcol.STATUS) & ~EV_TELEM_BP);
	      
	    }
	  
	  if ((rawx>=first_x && rawx<=last_x) && (rawy>=first_y && rawy<=last_y))
	    {

	      /* Check if event falls on the border */
	      if(rawx==first_x || rawx==first_x+1 || rawx==last_x)
		{
		  XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BAD;
		  first=0;
		}

	      /* Check if event is on a bad column  */
	      search=0;
	     
	      if (amp == AMP1)
		{
		  jj=0;
		  while(search==0 && jj <= last_y)
		    {
		      if (bp_table_amp1[rawx][jj]!=NULL) {
			if (bp_table_amp1[rawx][jj]->bad_type == BAD_COL)
			  {
			    XVEC2BYTE(outtable,n,indxcol.STATUS)|=BP_COL;
			    XVEC2BYTE(outtable,n,indxcol.STATUS) |= bp_table_amp1[rawx][jj]->bad_flag;
			    first=0;
			    search=1;
			  }
		      }
		      jj++;
		      
		    }

		  if((rawx - 1) >= first_x )
		    {
		      jj=0;
		      while(search==0 && jj <= last_y)
			{
			  if (bp_table_amp1[rawx-1][jj]!=NULL) {
			    if (bp_table_amp1[rawx-1][jj]->bad_type == BAD_COL)
			      {
				XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
				first=0;
				search=1;
			      }
			  }
			  jj++;
			  
			}
		    }
		  if((rawx + 1) <= last_x )
		    {
		      jj=0;
		      while(search==0 && jj <= last_y)
			{
			  if (bp_table_amp1[rawx+1][jj]!=NULL) {
			    if (bp_table_amp1[rawx+1][jj]->bad_type == BAD_COL)
			      {
				XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
				first=0;
				search=1;
			      }
			  }
			  jj++;
			  
			}
		    }
		  

		}/* End if(amp == AMP1)*/
	      else
		{
		  while(search==0 && jj < last_y)
		    {
		      if (bp_table_amp2[rawx][jj]!=NULL) {
			if (bp_table_amp2[rawx][jj]->bad_type == BAD_COL)
			  {
			    XVEC2BYTE(outtable,n,indxcol.STATUS)|=BP_COL;
			    XVEC2BYTE(outtable,n,indxcol.STATUS) |= bp_table_amp2[rawx][jj]->bad_flag;
			    first=0;
			    search=1;
			  }
		      }
		      jj++;
		      
		    }
		  
		  if((rawx - 1) >= first_x )
		    {
		      jj=0;
		      while(search==0 && jj <= last_y)
			{
			  if (bp_table_amp2[rawx-1][jj]!=NULL) {
			    if (bp_table_amp2[rawx-1][jj]->bad_type == BAD_COL)
			      {
				XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
				search=1;
				first=0;
			      }
			  }
			  jj++;
			  
			}
		    }
		  if((rawx + 1) <= last_x )
		    {
		      jj=0;
		      while(search==0 && jj <= last_y)
			{
			  if (bp_table_amp2[rawx+1][jj]!=NULL) {
			    if (bp_table_amp2[rawx+1][jj]->bad_type == BAD_COL)
			      {
				XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_NEIGH_BP;
				search=1;
				first=0;
			      }
			  }
			  jj++;
			  
			}
		    }


		}/* End else */
	    }
	  
	  else
	    {
	      headas_chat(CHATTY, "%s: Warning: event with rawx=%d rawy=%d is out of range.\n", global.taskname, rawx, rawy);
	      XVEC2BYTE(outtable,n,indxcol.STATUS)|=EV_BAD;		
	      first=0;
	    }
	  
          if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    }
	  if(!first)
	    {
	      bad++;
	      headas_chat(CHATTY, "%s: Flagged event with RAWX = %3d and RAWY = %3d \n", global.taskname,rawx, rawy);
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }

  global.amp=amp;
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  global.percent=100. * (double)bad/(double)outtable.MaxRows;
  ReleaseBintable(&head, &outtable);
  
  
  return OK;
  
 windowed_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
} /* WindowedBintable */

/*
 *	xrtflagpix_work
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
 *             int xrtflagpix_getpar(char *);
 *             int GetBadPix(char *, BadPixel_t *badpix[][], BOOL type);
 *             int GetBiasMap(char *, BadPixel_t *badpix[][]);
 *             int GetBPFromEvents(FitsFileUnit_t iunit, BadPixel_t *badpix[][]);
 *             int CreateEVFile(FitsFileUnit_t evunit, BadPixel_t *bp_table[][]);
 *             int CreateBPFile(FitsFileUnit_t evunit, BadPixel_t *bp_table[][]);
 *
 *             int fits_movnam_hdu(fitsfile *fptr, int hdutype, char *extname, 
 *                                 int extver, int * status);
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *        0.1.1: -    04/02/2003 - Deleted GetBPFromEvents routine
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

int xrtflagpix_work()
{

  int                   status=OK;   
  char                  expr[256];
  long                  extno;
  ThresholdInfo_t       thrvalues;
  FitsCard_t            *card;
  FitsHeader_t          evhead;            /* Extension header pointer */
  FitsFileUnit_t        evunit=NULL;       /* input and output file pointers */
  static BadPixel_t     *bp_table_amp1[CCD_PIXS][CCD_ROWS], *bp_table_amp2[CCD_PIXS][CCD_ROWS];  
                        /* Bad Pixels Table will 
			   be of size:CCD_PIXS*CCD_ROWS */
  double  sens=0.00001;

  TMEMSET0( bp_table_amp1, bp_table_amp1 );
  TMEMSET0( bp_table_amp2, bp_table_amp2 );

  global.warning=0;

  if (xrtflagpix_checkinput())
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
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	  
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

   /* readout mode is PHOTON or TIMING ? */
  if(strcmp(global.datamode, KWVL_DATAMODE_PH) && strcmp(global.datamode, KWVL_DATAMODE_TM)) 
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamodes  are:  %s and %s.\n", global.taskname, KWVL_DATAMODE_PH, KWVL_DATAMODE_TM);
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	}
      
      goto end_func;
    }
    

  if((ExistsKeyWord(&evhead, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      if(ReadHK())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read baseline value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.hdfile);
	  goto end_func;
	}
    }



  /* get observation start date and time */
  if((ExistsKeyWord(&evhead, KWNM_DATEOBS  , &card)))
    {
      global.dateobs=card->u.SVal;
      if(!(strchr(global.dateobs, 'T')))
	{
	  if((ExistsKeyWord(&evhead, KWNM_TIMEOBS  , &card)))
	    {
	      global.timeobs=card->u.SVal;
      
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto end_func;
	    }
	}

    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  /* get observation start time */
  if((ExistsKeyWord(&evhead, KWNM_TSTART, &card)))
    {
      global.tstart=card->u.DVal;        
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  /* get observation XRTVSUB */
  if((ExistsKeyWord(&evhead, KWNM_XRTVSUB, &card)))
    {
      global.xrtvsub=card->u.JVal;
    }
  else
    {
      global.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }


  /********************************
   *     Get CALDB thresholds file    *
   ********************************/

  if(!strcmp(global.datamode, KWVL_DATAMODE_PH) && global.usethrfile)
    {
      extno=-1;
      
      if (!(strcasecmp (global.par.thrfile, DF_CALDB)))
	{
	  if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_EXTNAME_XRTVSUB, global.par.thrfile,HD_EXPR, &extno))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto end_func;
	    }
	  else
	    headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.thrfile);
	  extno++;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.thrfile);

      if(ReadThrFile(global.par.thrfile, extno, global.xrtvsub, &thrvalues))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read Event Threshold Level from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the input thrfile.\n", global.taskname);
	  goto end_func;
	}
      
      global.par.event=thrvalues.pcevent;

      headas_chat(NORMAL, "%s: Info: Event Threshold Level from input thresholds file = %d\n", global.taskname, global.par.event);
    }


  /********************************
   *     Get CALDB bad pixels     *
   ********************************/
  
  extno=-1;
 
  if (strcasecmp(global.par.bpfile,DF_NONE) )  /* if a bad pixel file is given for input (NOT NONE) */
    {
      if ( !strcasecmp(global.par.bpfile,DF_CALDB) ) /* If bad pixel file is the CALDB file */
	{
	  sprintf(expr, "datamode.eq.%s.and.type.eq.%s", global.datamode, KWVL_BADPIX_TYPE);
	  if (CalGetFileName(HD_MAXRET,  global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_BADPIX_DSET,global.par.bpfile, expr, &extno ))
	    { 
	      headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto end_func;
	    }
	  extno++;
	}
      else
	{
	  if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
	    {
	      if(CalGetExtNumber(global.par.bpfile, KWVL_EXTNAME_PCBAD, &extno))
		{ 
		  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PCBAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bpfile);
		  goto end_func;
		}
	    }
	  else

	    {
	      if(CalGetExtNumber(global.par.bpfile, KWVL_EXTNAME_WTBAD, &extno))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_WTBAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bpfile);
		  goto end_func;
		}
	    }
	}
     
      if(extno > 0)
	{
	  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.bpfile);
	  if (GetBadPix(global.par.bpfile, bp_table_amp1, bp_table_amp2, CBP, extno, global.tstart)) 
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
	      headas_chat(CHATTY, "%s: Error: %s file.\n", global.taskname, global.par.bpfile);
	      goto end_func;
	    }
	}
      else
	{
	  headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: %s file.\n", global.taskname, global.par.bpfile);
	  goto end_func;
	}
    }
	
  /********************************
   * Get user defined  bad pixels *
   ********************************/
  if (strcasecmp(global.par.userbpfile,DF_NONE))  /* if user bpfile exists (NOT NONE) */ 
    {
      if(!strcmp(global.datamode, KWVL_DATAMODE_PH))  /* if PH */ 
	{
	  if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_PCBAD, &extno))
	    {
	      headas_chat(CHATTY,"%s: Warning: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PCBAD);
	      headas_chat(CHATTY,"%s: Warning: in '%s' file.\n",global.taskname, global.par.userbpfile);
	      headas_chat(CHATTY,"%s: Warning: Check for %s extension.\n",global.taskname,  KWVL_EXTNAME_BAD);
	      if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_BAD, &extno))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.userbpfile);
		  goto end_func;
		}
	    }
	}
      else
	{
	  if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_WTBAD, &extno))
	    {
	      headas_chat(CHATTY,"%s: Warning: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_WTBAD);
	      headas_chat(CHATTY,"%s: Warning: in '%s' file.\n",global.taskname, global.par.userbpfile);
	      headas_chat(CHATTY,"%s: Warning: Check for %s extension.\n",global.taskname,  KWVL_EXTNAME_BAD);
	      if(CalGetExtNumber(global.par.userbpfile, KWVL_EXTNAME_BAD, &extno))
		{
		  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
		  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.userbpfile);
		  goto end_func;
		}
	    }
	}
      
      headas_chat(CHATTY, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.userbpfile);
      if (GetBadPix(global.par.userbpfile, bp_table_amp1, bp_table_amp2, UBP, extno, global.tstart)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file. \n", global.taskname, global.par.userbpfile);
	  goto end_func;
	}
    }

  if (strcasecmp (global.par.bptable, DF_NONE)) {
    if ( !strcasecmp(global.par.bptable,DF_CALDB) )
      {
	sprintf(expr, "datamode.eq.%s.and.type.eq.%s", global.datamode, KWVL_BPTABLE_TYPE);
	if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_BPTABLE_DSET,global.par.bptable, expr, &extno ))
	  {
	    headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	    goto end_func;
	  }
	extno++;
      }

    else
      {
	if(!strcmp(global.datamode, KWVL_DATAMODE_PH))
	  {
	    if(CalGetExtNumber(global.par.bptable, KWVL_EXTNAME_PCBAD, &extno))
	      {
		headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PCBAD);
		headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bptable);
		goto end_func;
	      }
	  }
	else
	  {
	    if(CalGetExtNumber(global.par.bptable, KWVL_EXTNAME_WTBAD, &extno))
	      {
		headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_WTBAD);
		headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.bptable);
		goto end_func;
	      }
	  }
	
      }

    if(extno > 0)
      {
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.bptable);
	if (GetBadPix(global.par.bptable, bp_table_amp1, bp_table_amp2, OBP, extno, global.tstart)) 
	  {
	    headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
	    headas_chat(CHATTY, "%s: Error: %s file. \n", global.taskname, global.par.bptable);
	    goto end_func;
	  }
      }
    else
      {
	headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
	headas_chat(CHATTY, "%s: Error: %s file. \n", global.taskname, global.par.bptable);
	goto end_func;
      }
    
  }

  /*******************************************
   *     Reject Burned spots if maxtemp == 0 *
   ******************************************/

  if ( (global.par.maxtemp >= 0.0 - sens)&&(global.par.maxtemp <= 0.0 + sens) ) {
  
    int  i, j ;

    for (i=0; i<CCD_PIXS; i++)
      {
        for (j=0; j<CCD_ROWS; j++)
          {
            if ( bp_table_amp1[i][j] != NULL )
              {
                if (bp_table_amp1[i][j]->bad_flag&EV_BURNED_BP)
                  {
                    bp_table_amp1[i][j]->bad_flag = bp_table_amp1[i][j]->bad_flag&~EV_BURNED_BP;
                    if ( bp_table_amp1[i][j]->counter == 0 ) {
                      bp_table_amp1[i][j] = NULL;
                    }
                  }
              }
            if ( bp_table_amp2[i][j] != NULL )
              {
                if (bp_table_amp2[i][j]->bad_flag&EV_BURNED_BP)
                  {
                    bp_table_amp2[i][j]->bad_flag = bp_table_amp2[i][j]->bad_flag&~EV_BURNED_BP;
                    if ( bp_table_amp2[i][j]->counter == 0 ) {
                      bp_table_amp2[i][j] = NULL;
                    }
                  }
              }
          }
      }
  }

	    
  /********************************
   * Create Temporary events file *
   ********************************/
  
  if(CreateEVFile(evunit, bp_table_amp1, bp_table_amp2))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, global.par.outfile);
      goto end_func;
    }
  
  if(global.warning)
    goto ok_end;
    
    
  /* Close input event file */
  if (CloseFitsFile(evunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
    
    
  /* rename temporary file into output event file */

  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      headas_chat(CHATTY, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto end_func;
    }
    
  headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.outfile);
  if(!finite(global.percent))
    global.percent=0.;
  headas_chat(NORMAL, "%s: Info: The percentage of the flagged events is: %10.4f\n", global.taskname, global.percent); 


  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  
 ok_end:

  if(global.warning && strcasecmp(global.par.infile, global.par.outfile))
    {
      if(CopyFile(global.par.infile, global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outfile file.\n", global.taskname);
	  goto end_func;
	}
    }

  if(global.count)
    free(global.bline);

  return OK;
  
 end_func:
  
  if(global.count)
    free(global.bline);
  
  return NOT_OK;
}/* xrtflagpix_work */

int xrtflagpix(void)
{
  
  /********************** 
   * set HEADAS globals *
   **********************/
  set_toolname(PRG_NAME);
  set_toolversion(XRTFLAGPIX_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /*************************
   * Read input parameters *
   *************************/
  if ( xrtflagpix_getpar() == OK ) {

    if ( xrtflagpix_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto flagpix_end;
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
    goto flagpix_end;
  
  return OK;
  
 flagpix_end:
  
  if (FileExists(global.tmpfile))
    remove (global.tmpfile); 
  
  return NOT_OK;

} /* xrtflagpix */

/*
 *
 * WriteStatus 
 */

int WriteStatus(FitsFileUnit_t ounit)
{

  int status=0;

  fits_write_comment(ounit, "Events STATUS flags",&status);
  
  fits_write_comment(ounit, "b0000000000000000 Good event", &status);
  fits_write_comment(ounit, "b0000000000000001 Event falls in bad pixel from CALDB ", &status);
  fits_write_comment(ounit, "b0000000000000010 Event falls in bad pixel from on board Bad Pixels Table", &status);  
  fits_write_comment(ounit, "b0000000000000100 Event falls in burned spot region ", &status);  
  fits_write_comment(ounit, "b0000000000001000 Event falls in hot bad pixel ", &status);  
  fits_write_comment(ounit, "b0000000000010000 Event falls in user bad pixel", &status);  
  fits_write_comment(ounit, "b0000000000100000 Event falls in flickering bad pixel ", &status);  
  fits_write_comment(ounit, "b0000000001000000 Event falls in telemetred bad pixel", &status);  
  fits_write_comment(ounit, "b0000000010000000 Event has a neighbor bad from hot and flickering pixels list", &status);  
  fits_write_comment(ounit, "b0000000100000000 Column", &status);   
  fits_write_comment(ounit, "b0000001000000000 Event has PHA[1] < Event threshold", &status);  
  fits_write_comment(ounit, "b0000010000000000 Event has a neighbor bad from bad pixels list ", &status);  
  fits_write_comment(ounit, "b0000100000000000 Bad event", &status);  
  fits_write_comment(ounit, "b0001000000000000 Event from calibration source 1 ", &status);  
  fits_write_comment(ounit, "b0010000000000000 Event from calibration source 2 ", &status);  
  fits_write_comment(ounit, "b0100000000000000 Event from calibration source 3", &status);  
  fits_write_comment(ounit, "b1000000000000000 Event from calibration source 4", &status);  

  if(!status)
    return OK;
  else
    return NOT_OK;

}


/*
 *
 *   ReadHK
 *
 *   DESCRIPTION:
 *     Routine to read baseline values from Housekeeping Header Packets FITS file
 *
 *
 *   DOCUMENTATION:
 *
 *
 *
 *   FUNCTION CALL:
 *     FitsFileUnit_t OpenReadFitsFile(char *name);
 *     int headas_chat(int , char *, ...);
 *     int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *     int CloseFitsFile(FitsFileUnit_t file);
 *     FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *     void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     				  const int MaxBlockRows, const unsigned nColumns,
 *  				  const unsigned ActCols[]);
 *     int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *     void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *     int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *			 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *     unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 */
int ReadHK(void)
{


  int                status=OK, hkmode=0, good_row=1;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[8];
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;
  
  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.count=0;
  
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);

  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
 
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      goto ReadHK_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  
  if(!hktable.MaxRows)
    {
      
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }


  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  /* Frame Start Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.BaseLine=ColNameMatch(CLNM_BASELINE, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BASELINE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.CCDTEMP=ColNameMatch(CLNM_CCDTemp, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=8; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.TIME;
  ActCols[2]=hkcol.BaseLine;
  ActCols[3]=hkcol.XRTMode;
  ActCols[4]=hkcol.Settled;
  ActCols[5]=hkcol.In10Arcm;
  ActCols[6]=hkcol.CCDTEMP;
  ActCols[7]=hkcol.InSafeM;
  
  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  good_row=1;

	  hkInSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);  
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);
	  
	  if(!strcmp(global.obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(global.obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }

	  hkmode=BVEC(hktable, n, hkcol.XRTMode);

	  if(good_row && (hkmode == XRTMODE_PC))
	    {
	      if(global.count)
		{
		  global.count+=1;
		  global.bline = (BLine_t *)realloc(global.bline, ((global.count)*sizeof(BLine_t)));
		}
	      else
		{
		  
		  global.count+=1;
		  global.bline = (BLine_t *)malloc(sizeof(BLine_t));  
		  
		}/* End memory allocation */

	      global.bline[global.count - 1].hkccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.bline[global.count - 1].hktime=DVEC(hktable, n, hkcol.TIME);
	      global.bline[global.count - 1].baseline=JVEC(hktable, n, hkcol.BaseLine);
	      global.bline[global.count - 1].temp=EVEC(hktable, n, hkcol.CCDTEMP);
	      
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.count)
    {
      headas_chat(NORMAL, "%s: Error: The %s file is not appropriate for\n", global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  ReadHK_end;
    }

   ReleaseBintable(&hkhead, &hktable);
   return OK;

 ReadHK_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* ReadHK */
/*
 *
 *	GetCalSourcesInfo
 *
 *
 *	DESCRIPTION:
 *                 Routine to get calibration sources information
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           int                strcasecmp(const char *, const char *);
 *           int                CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd,
 *                                             char *TimeEnd,const char *DataSet, char *CalFileName, char *expr);
 *           int                headas_chat(int , char *, ...);
 *           FitsFileUnit_t     OpenReadFitsFile(char *name);
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit); 
 *           int                fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, 
 *                                              int extver, int *status);
 *           void               GetBintableStructure(FitsHeader_t *header, Bintable_t *table, 
 *                                                   const int MaxBlockRows, const unsigned nColumns, 
 *                                                   const unsigned ActCols[]);
 *           int		GetColNameIndx(const Bintable_t *table, const char *ColName);
 *           void *             calloc(size_t nmemb, size_t size);
 *           int                ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                                           const unsigned nColumns, const unsigned ActCols[], 
 *                                           const unsigned FromRow, unsigned *nRows);
 *           unsigned           ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *           int                CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 29/07/2004 First version
 */
int GetCalSourcesInfo(void)
{
  long           extno;

  if (!(strcasecmp (global.par.srcfile, DF_NONE)))
    {
      global.nosrcflag=1;
      return OK;
    }
  else
    global.nosrcflag=0;


 /* Derive CALDB calibration sources filename */ 
  if (!(strcasecmp (global.par.srcfile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_REGION_DSET, global.par.srcfile, "type.eq.CALSOURCE", &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      extno++;
    }
  
  else
    {
      if(CalGetExtNumber(global.par.srcfile, KWVL_EXTNAME_REGIONCAL, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s extension\n", global.taskname, KWVL_EXTNAME_REGIONCAL);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.srcfile);
	  goto get_end;
	}
	 
    } 

  headas_chat(NORMAL, "%s: Info: Processing '%s' file to get Calibration Sources Info.\n", global.taskname, global.par.srcfile);

  if(ReadCalSourcesFile(global.par.srcfile, global.calsrc, (int)extno))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get Calibratione Sources Position.\n", global.taskname);
      goto get_end;
    }
  
  return OK;
  
 get_end:
   
  return NOT_OK;

}/* GetCalSourceInfo */

/*
 *
 *	GetCalFovInfo
 *
 *
 *	DESCRIPTION:
 *                 Routine to get Field of View information
 *    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *           int                strcasecmp(const char *, const char *);
 *           int                CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd,
 *                                             char *TimeEnd,const char *DataSet, char *CalFileName, char *expr);
 *           int                headas_chat(int , char *, ...);
 *           FitsFileUnit_t     OpenReadFitsFile(char *name);
 *           FitsHeader_t	RetrieveFitsHeader(FitsFileUnit_t unit); 
 *           int                fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, 
 *                                              int extver, int *status);
 *           void               GetBintableStructure(FitsHeader_t *header, Bintable_t *table, 
 *                                                   const int MaxBlockRows, const unsigned nColumns, 
 *                                                   const unsigned ActCols[]);
 *           int		GetColNameIndx(const Bintable_t *table, const char *ColName);
 *           void *             calloc(size_t nmemb, size_t size);
 *           int                ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                                           const unsigned nColumns, const unsigned ActCols[], 
 *                                           const unsigned FromRow, unsigned *nRows);
 *           unsigned           ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *           int                CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - NS 20/06/2007 First version
 */
int GetCalFovInfo(void)
{
  long           extno;

  if (!(strcasecmp (global.par.srcfile, DF_NONE)))
    {
      global.nofovflag=1;
      return OK;
    }
  else
    global.nofovflag=0;


 /* Derive CALDB calibration sources filename */ 
  if (!(strcasecmp (global.par.srcfile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_REGION_DSET, global.par.srcfile, "type.eq.FOV", &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto getfov_end;
	}
      extno++;
    }
  
  else
    {
      if(CalGetExtNumber(global.par.srcfile, KWVL_EXTNAME_REGIONFOV, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s extension\n", global.taskname, KWVL_EXTNAME_REGIONFOV);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.srcfile);
	  goto getfov_end;
	}
	 
    } 

  headas_chat(NORMAL, "%s: Info: Processing '%s' file to get Calibration FOV Info.\n", global.taskname, global.par.srcfile);

  if(ReadCalSourcesFile(global.par.srcfile, &global.calfov, (int)extno))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get Field of View information.\n", global.taskname);
      goto getfov_end;
    }
  
  return OK;
  
 getfov_end:
   
  return NOT_OK;

}/* GetCalFovInfo */


int AddNeighBadPix(FitsFileUnit_t evunit, BadPixel_t * bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2 [CCD_PIXS][CCD_ROWS])
{
  int                iii=0, baseline=0, ccdframe_old=0, ccdframe=0,jj=0;
  int                neigh_x[] = {NEIGH_X}, neigh_y[] = {NEIGH_Y}, amp=1;
  BOOL               first_time=1;
  double             evtime, sens=0.0000001;
  EvCol_t            indxcol;
  BadPixel_t         pix;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0,  rawx, rawy; 
  Bintable_t	     table;
  FitsHeader_t	     head;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  if(!table.MaxRows)
    {
      
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }


  if((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &table)) == -1)
    {
      headas_chat(CHATTY, "%s: Warning: '%s' column does not exist\n", global.taskname, CLNM_PHAS);
      headas_chat(CHATTY, "%s: Warning: in '%s' file. \n", global.taskname, global.par.infile);
      headas_chat(CHATTY, "%s: Warning: Unable to search observation bad pixels.\n", global.taskname);
      return OK;
    }

  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto add_end;
    }
  if((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto add_end;
    }

  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto add_end;
    }

  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto add_end;
    }
  
  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto add_end;
    }



 /*  EndBintableHeader(&head, &table);  */


  FromRow = 1;
  ReadRows=BINTAB_ROWS;
  OutRows=0;

  /* Check bad pixels and flag them */
  while(ReadBintable(evunit, &table, nCols, NULL, FromRow,&ReadRows)==0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  if (table.TagVec[indxcol.Amp] == B)
	    amp=BVEC(table, n,indxcol.Amp);
	  else
	    amp=IVEC(table, n,indxcol.Amp);

	  ccdframe=JVEC(table,n,indxcol.CCDFrame);
	  evtime=DVEC(table,n,indxcol.TIME);

	  if(first_time || ccdframe_old != ccdframe )
	    {	
	      first_time=0;
	      for (; iii < global.count; iii++)
		{
		  if(ccdframe == global.bline[iii].hkccdframe &&  
		     (global.bline[iii].hktime >= (evtime-sens) && global.bline[iii].hktime <= (evtime+sens)))
		    {
		      baseline=global.bline[iii].baseline;
		      headas_chat(CHATTY, "%s: Info: Frame %4d - Using %4d baseline value to search bad pixels around 3x3.\n", global.taskname, ccdframe, baseline);
		      goto row_found;
		    }
		}

	      headas_chat(NORMAL, "%s: Error: Unable to get baseline value.\n", global.taskname);
	      goto add_end;
	    }
	    
	  
	  /* Check bad pixels neighbour using baseline value */
	row_found:
	  rawx=IVEC(table,n,indxcol.RAWX);
	  rawy=IVEC(table,n,indxcol.RAWY);
	  for (jj=1; jj< PHAS_MOL; jj++)
	    {
	      if(IVECVEC(table,n,indxcol.PHAS,jj) == -baseline)
		{
		  
		  pix.bad_type = BAD_PIX;
		  pix.xyextent = 1;
		  pix.bad_flag = OBS_TELEM_BP;


		  if( (rawx+neigh_x[jj]>=RAWX_MIN && rawx+neigh_x[jj]<=RAWX_MAX)&&(rawy+neigh_y[jj]>=RAWY_MIN && rawy+neigh_y[jj]<=RAWY_MAX) )
		    {

		      if(amp == AMP1)
			{
			  /* Add bad pixel and info about it in bad pixels map for AMP1*/
			  if (AddBadPix((rawx + neigh_x[jj]), (rawy + neigh_y[jj]),pix,bp_table_amp1))
			    {
			      headas_chat(NORMAL, "%s: Error: AddNeighBadPix: Unable to  write pixels in bad pixels map.\n",global.taskname);
			      goto add_end;
			    }
			}
		      else
			{
			  if (AddBadPix((rawx + neigh_x[jj]), (rawy + neigh_y[jj]),pix,bp_table_amp2))
			    {
			      headas_chat(NORMAL, "%s: Error: AddNeighBadPix: Unable to  write pixels in bad pixels map.\n",global.taskname);
			      goto add_end;
			    }
			}
		    }
		}
	    }
	}
      FromRow += ReadRows;
      
    }

  ReleaseBintable(&head, &table);

  return OK;

 add_end:


  return NOT_OK;

}/* AddNeighBadPix */

int ReadBPExt(FitsFileUnit_t evunit,int hducount, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS])
{
  int            x, y, n, status=OK, amp=0;
  short int      flag=0, flag_tmp=0;
  unsigned       FromRow, ReadRows, OutRows, nCols, count, ext; 
  double         sens=0.0000001;
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
  
  if(!strcasecmp(global.par.bpfile, DF_NONE) || (global.par.maxtemp > 0.0 - sens && global.par.maxtemp < 0.0 + sens))
    {
      flag|=CALDB_BP;
      flag|=OBS_BURNED_BP;
    }
  if(!strcasecmp(global.par.userbpfile, DF_NONE))
    {
      flag|=USER_BP;
    }
  if(!strcasecmp(global.par.bptable, DF_NONE))
    {
      flag|=ONBOARD_BP;
    }

  flag|=OBS_TELEM_BP;
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
