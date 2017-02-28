/*
 * 
 *	xrtpccorr.c:
 *
 *	INVOCATION:
 *
 *		xrtpccorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *                Routine to correct Photon-Counting PHAS
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 30/03/2007 - First version
 *        0.1.1 -    23/04/2007 - Bug fixed in memory allocation
 *        0.1.2 -    08/05/2007 - Added BIASDIFF extension in output event file
 *                              - Modified chatter for some stdout messages
 *                              - Added tolerance in TSTART and TSTOP check
 *                                of evt and mkf file
 *                              - Write history in the header of 'BIASDIFF' ext
 *        0.1.3 -    10/05/2007 - Added 'AddSwiftXRTKeyword' function
 *                              - Changed name of some columns in 'BIASDIFF' ext
 *                              - Added new columns in 'BIASDIFF' ext
 *        0.1.4 -    14/06/2007 - Fixed bug on big-endian architectures
 *                                (error reading keywords WHALFWT WHALFHT)
 *        0.1.5 -    11/07/2007 - Bug fixed in ComputeOrbits function
 *        0.1.6 -    12/07/2007 - Changed from 1I to 1J datatype of BIASDIFF, BIASDIFF2, BIASDIFF4,
 *                                BIASDIFF7, BIASDIFF9 and NEVENTS columns written in BIASDIFF ext
 *        0.1.7 -    23/08/2007 - Exit with warning if mkffile not include infile range time
 *        0.1.8 -    10/01/2008 - PHAS correction not applied for telemetred bad pixels
 *        0.1.9 -    19/09/2008 - PHAS correction not applied for evt from calibration source
 *        0.2.0 -    12/11/2008 - Get ranom and decnom values from TCRVL<n> keywords
 *        0.2.1 -    28/05/2013 - Bug fixed in ComputeBiasMedian function
 *        0.2.2 -    16/01/2014 - Added check of the 'ATTFLAG' keyword of the input attitude file
 *        0.2.3 -    17/02/2014 - Modified check of the 'ATTFLAG' keyword of the input attitude file
 *        0.2.4 -    08/10/2015 - Handle RAWX/RAWY out of range events
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtpccorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtpccorr.h"

/********************************/
/*         definitions          */
/********************************/

#define XRTPCCORR_C
#define XRTPCCORR_VERSION      "0.2.4"
#define PRG_NAME               "xrtpccorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	xrtpccorr_info:
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
 *        0.1.0: - NS 30/03/2007 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtpccorr_info(void)
{
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input mkf file                            :'%s'\n",global.par.mkffile);
  headas_chat(NORMAL,"Bias method                                           :'%s'\n",global.par.biasmethod);
  headas_chat(NORMAL,"Grades selection                                      :'%d-%d'\n",global.par.grademin,global.par.grademax);

  if(!global.use_detxy)
    {
      headas_chat(NORMAL,"Name of the the input attitude file                  :'%s'\n",global.par.attfile);
      headas_chat(NORMAL,"Name of the input TELDEF file                        :'%s'\n",global.par.teldef);
      headas_chat(NORMAL,"Source RA                                            :'%f'\n",global.par.srcra);
      headas_chat(NORMAL,"Source Dec                                           :'%f'\n",global.par.srcdec);
      headas_chat(NORMAL,"Nominal RA                                           :'%f'\n",global.par.ranom);
      headas_chat(NORMAL,"Nominal Dec                                          :'%f'\n",global.par.decnom);
      if (global.par.aberration)
	headas_chat(NORMAL,"Should aberration be included in aspecting?          : yes\n");
      else
	headas_chat(NORMAL,"Should aberration be included in aspecting?          : no\n");
      if (global.par.attinterpol)
	headas_chat(NORMAL,"Interpolate Attitude values?                         : yes\n");
      else
	headas_chat(NORMAL,"Interpolate Attitude values?                         : no\n");
    }
  else
    {
      headas_chat(NORMAL,"Source DETX                                          :'%d'\n",global.par.srcdetx);
      headas_chat(NORMAL,"Source DETY                                          :'%d'\n",global.par.srcdety);
    }

  headas_chat(NORMAL,"Size in pixel for the subimage                        :'%d'\n",global.par.subimgsize);
  headas_chat(NORMAL,"Nevents                                               :'%d'\n",global.par.nevents);

  if(global.par.subimage)
    headas_chat(NORMAL,"Use subimage method?(yes/no)                          :'yes'\n");
  else
    headas_chat(NORMAL,"Use subimage method?(yes/no)                          :'no'\n");
  if(global.par.lessevents)
    headas_chat(NORMAL,"Use events found within each image even if <nevents?(yes/no):'yes'\n");
  else
    headas_chat(NORMAL,"Use events found within each image even if <nevents?(yes/no):'no'\n");
  if(global.par.badpix)
    headas_chat(NORMAL,"Exclude bad pixels?(yes/no)                           :'yes'\n");
  else
    headas_chat(NORMAL,"Exclude bad pixels?(yes/no)                           :'no'\n");

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* xrtpccorr_info */


/*
 *	xrtpccorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtpccorr.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - NS 30/03/2007 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpccorr_getpar()
{
  /*
   *  Get File name Parameters
   */
  
  /* Input Windowed Timing Event List File Name */
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

  /* Mkf File Name */
  if(PILGetFname(PAR_MKFFILE, global.par.mkffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MKFFILE);
      goto Error;
    }

  /* Bias method */
  if(PILGetString(PAR_BIASMETHOD, global.par.biasmethod)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASMETHOD);
      goto Error;	
    }

  /* Grades selection */
  if(PILGetInt(PAR_GRADEMIN, &global.par.grademin)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRADEMIN);
      goto Error;	
    }

  if(PILGetInt(PAR_GRADEMAX, &global.par.grademax)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRADEMAX);
      goto Error;	
    }

  /* Src detx  */
  if(PILGetInt(PAR_SRCDETX, &global.par.srcdetx)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETX);
      goto Error;	
    }

  /* Src dety  */
  if(PILGetInt(PAR_SRCDETY, &global.par.srcdety)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETY);
      goto Error;	
    }

  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
    {
      global.use_detxy=0;
      
      /* Input teldef file name or CALDB */
      if(PILGetFname(PAR_TELDEF, global.par.teldef))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TELDEF);
	  goto Error;
	}
      
      /* Input attitude File Name */
      if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
	  goto Error;
	}
      

      /* Input nominal RA */
      if(PILGetReal(PAR_RANOM, &global.par.ranom))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_RANOM); 
	  goto Error;	 
	} 

      /* Input nominal Dec */
      if(PILGetReal(PAR_DECNOM, &global.par.decnom)) 
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DECNOM); 
	  goto Error;	 
	}


      /* Input source RA */
      if(PILGetReal(PAR_SRCRA, &global.par.srcra)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCRA);
	  goto Error;	
	}
      /* Input source Dec */
      if(PILGetReal(PAR_SRCDEC, &global.par.srcdec)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDEC);
	  goto Error;	
	}

      if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ABERRATION);
	  goto Error;
	}

      if(PILGetBool(PAR_ATTINTERPOL, &global.par.attinterpol))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ATTINTERPOL);
	  goto Error;
	}

    }
  else
    {
      global.use_detxy=1;
    }

  /* Sub image size  */
  if(PILGetInt(PAR_SUBIMGSIZE, &global.par.subimgsize)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SUBIMGSIZE);
      goto Error;	
    }

  /*   */
  if(PILGetInt(PAR_NEVENTS, &global.par.nevents)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NEVENTS);
      goto Error;	
    }

  /*   */
  if(PILGetBool(PAR_SUBIMAGE, &global.par.subimage)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SUBIMAGE);
      goto Error;	
    }

  /*   */
  if(PILGetBool(PAR_LESSEVENTS, &global.par.lessevents)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_LESSEVENTS);
      goto Error;	
    }

  /*   */
  if(PILGetBool(PAR_BADPIX, &global.par.badpix)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BADPIX);
      goto Error;	
    }

  
  get_history(&global.hist);

  xrtpccorr_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* xrtpccorr_getpar */


/*
 *	xrtpccorr_work
 *
 *
 *	DESCRIPTION:
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - NS 30/03/2007 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtpccorr_work()
{
  int            status = OK, inExt, outExt, evExt , hdubp=0; 
  int            logical;
  unsigned       indxcol;
  char	         KeyWord[FLEN_KEYWORD], attflag[FLEN_KEYWORD];
  Bintable_t	 table;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL;              /* Input and Output fits file pointer */ 
  double         src_x, src_y;

  static BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS];
  static BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS];

  TELDEF * teldef=NULL;
;
  /* Initialize global vars */

  global.tot_orbits=0;

  TMEMSET0( bp_table_amp1, bp_table_amp1 );
  TMEMSET0( bp_table_amp2, bp_table_amp2 );


  if(xrtpccorr_checkinput())
    goto Error;
  

  /* Check ATTFLAG keyword value of the attitude file */

  if(!global.use_detxy)
    {
      if (GetAttitudeATTFLAG(global.par.attfile, attflag)==NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to get %s in\n", global.taskname, KWNM_ATTFLAG);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.attfile);
	  goto Error;
	}
  
      if (! strcmp(attflag, "111") || !strcmp(attflag, "101") )
	{
	  headas_chat(NORMAL,"%s: Error: input attitude file sw*uat.fits not suitable for XRT data processing,\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: use the sw*sat.fits or sw*pat.fits one.\n", global.taskname);
	  goto Error;
	}
    }


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
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,inunit,global.par.infile,global.taskname);
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  if(ExistsKeyWord(&head, KWNM_DATAMODE, &card))
    {
      strcpy(global.evt.datamode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  if(strcmp(card->u.SVal, KWVL_DATAMODE_PH))
    {
      headas_chat(NORMAL,"%s: Error: This task does not process the %s readout mode\n", global.taskname, card->u.SVal);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid readout mode is: %s\n", global.taskname, KWVL_DATAMODE_PH);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
      
    }

  /* Retrieve OBSMODE from input event file */
  if((ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.evt.obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve OBSID from input event file */
  if((ExistsKeyWord(&head, KWNM_OBS_ID  , &card)))
    {
      strcpy(global.evt.obsid, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve ORIGIN from input event file */
  if((ExistsKeyWord(&head, KWNM_ORIGIN, &card)))
    {
      strcpy(global.evt.origin, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ORIGIN);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve TARG_ID from input event file */
  if((ExistsKeyWord(&head, KWNM_TARG_ID  , &card)))
    {
      global.evt.target=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TARG_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  /* Retrieve XPHASCO from input event file */
  if(!(ExistsKeyWord(&head, KWNM_XPHASCO, NULLNULLCARD)) || !(GetLVal(&head, KWNM_XPHASCO)))
    {
      global.evt.xphasco=FALSE;    
    }
  else
    {
      global.evt.xphasco=TRUE;
    }

  
  /* Retrieve TSTART from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    global.evt.tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Retrieve TSTOP from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    global.evt.tstop=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve TIMESYS from input event file */
  if((ExistsKeyWord(&head, KWNM_TIMESYS , &card)))
    {
      strcpy(global.evt.timesys, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMESYS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve MJDREF from input event file */
  global.evt.mjdref=HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);

  if(status)
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve MJDREFI from input event file */
  if(ExistsKeyWord(&head, KWNM_MJDREFI, &card))
    {
      global.evt.mjdrefi=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Retrieve MJDREFF from input event file */
  if((ExistsKeyWord(&head, KWNM_MJDREFF, &card)))
    {
      global.evt.mjdreff=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve TIMEREF from input event file */
  if((ExistsKeyWord(&head, KWNM_TIMEREF , &card)))
    {
      strcpy(global.evt.timeref, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEREF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve TASSIGN from input event file */
  if((ExistsKeyWord(&head, KWNM_TASSIGN , &card)))
    {
      strcpy(global.evt.tassign, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TASSIGN);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve TIMEUNIT from input event file */
  if((ExistsKeyWord(&head, KWNM_TIMEUNIT, &card)))
    {
      strcpy(global.evt.timeunit, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEUNIT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  /* Retrieve WHALFWD from input event file */
  if(ExistsKeyWord(&head, KWNM_WHALFWD, &card))
    global.evt.whalfwd=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_WHALFWD);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve WHALFHT from input event file */
  if(ExistsKeyWord(&head, KWNM_WHALFHT, &card))
    global.evt.whalfht=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_WHALFHT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve DATEOBS from input event file */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.evt.dateobs=card->u.SVal;
      if(!(strchr(global.evt.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.evt.timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto Error;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
      
    }

  /* Retrieve DATEEND from input event file */
  if((ExistsKeyWord(&head, KWNM_DATEEND  , &card)))
    {
      global.evt.dateend=card->u.SVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);


  /* Get nominal RA */
  if(global.par.ranom < RA_MIN)
    {
      headas_chat(NORMAL,"%s: Info: input parameter %s < %d, using value of input file\n", global.taskname,PAR_RANOM,RA_MIN);


      if ((indxcol=ColNameMatch(CLNM_X, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto Error;
	}

      sprintf(KeyWord, "TCRVL%d", (indxcol+1));
      if((ExistsKeyWord(&head, KeyWord, &card)))
	{
	  global.par.ranom = card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
    }
  
  if (global.par.ranom > RA_MAX || global.par.ranom < RA_MIN)
    {
      headas_chat(NORMAL, "%s: Error: Nominal RA value is out of valid range.\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
      goto Error;
    }
  
  
  /* Get nominal Dec */
  if(global.par.decnom < DEC_MIN)
    {
      headas_chat(NORMAL,"%s: Info: input parameter %s < %d, using value of input file\n", global.taskname,PAR_DECNOM,DEC_MIN);


      if ((indxcol=ColNameMatch(CLNM_Y, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto Error;
	}

      sprintf(KeyWord, "TCRVL%d", (indxcol+1));
      if((ExistsKeyWord(&head, KeyWord, &card)))
	{
	  global.par.decnom = card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
    }
  
  
  if (global.par.decnom > DEC_MAX || global.par.decnom < DEC_MIN)
    {
      headas_chat(NORMAL, "%s: Error: Nominal DEC value is out of valid range.\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
      goto Error;
    }

  /* Get source RA */
  if(global.par.srcra < RA_MIN)
    {
      headas_chat(NORMAL,"%s: Info: input parameter %s < %d, using value of input file\n", global.taskname,PAR_SRCRA,RA_MIN);

      if((ExistsKeyWord(&head, KWNM_RA_OBJ  , &card)))
	global.par.srcra=card->u.DVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keywords not found\n", global.taskname,KWNM_RA_OBJ);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_SRCRA);
	  goto Error;
	}
    } 
  
  if (global.par.srcra > RA_MAX || global.par.srcra < RA_MIN)
    {
      headas_chat(NORMAL, "%s: Error: Source RA value is out of valid range.\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
      goto Error;
    }
  
  
  /* Get nominal Dec */
  if(global.par.srcdec < DEC_MIN)
    {
      headas_chat(NORMAL,"%s: Info: input parameter %s < %d, using value of input file\n", global.taskname,PAR_SRCDEC,DEC_MIN);
      
      if((ExistsKeyWord(&head, KWNM_DEC_OBJ  , &card)))
	global.par.srcdec=card->u.DVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n", global.taskname,KWNM_DEC_OBJ);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_SRCDEC);
	  goto Error;
	}
    }
  
  
  if (global.par.srcdec > DEC_MAX || global.par.srcdec < DEC_MIN)
    {
      headas_chat(NORMAL, "%s: Error: Source DEC value is out of valid range.\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
      goto Error;
    }
  
  
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
      goto Error;
    }


  /* Check if BadPixels ext exists */
  if(fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_BAD , 0, &status))
    {
      if(status == BAD_HDU_NUM)
	{
	  headas_chat(NORMAL,"%s: Warning: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: Warning: '%s' file.\n",global.taskname, global.par.infile);
	  status=0;
	}
      else
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
	  goto Error;
	}
    }
  else
    {
      /* Get bad pixels ext number */
      if (!fits_get_hdu_num(inunit, &hdubp))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: : '%s' file.\n",global.taskname, global.par.infile);
	  goto Error;
	}

      if(ReadBPExt(inunit, hdubp, bp_table_amp1, bp_table_amp2))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension\n",global.taskname, KWVL_EXTNAME_BAD);
	  headas_chat(NORMAL,"%s: Error: in '%s' input file.\n",global.taskname, global.par.infile);
	  goto Error;
	}
    }


  /* Create output file */
  if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
  
  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extension before event extension  from input to output file */
  outExt=1;
    
  while(outExt<evExt && status == OK)
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
      outExt++;
    }
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  if(!global.use_detxy)
    {     
      if(ComputeSrc_XY(inunit, teldef, &src_x, &src_y))
	{
	  goto Error;
	}
    }  

  if(ComputeOrbits( src_x, src_y))
    {
      goto Error;
    }

  if(global.warning)
    {
      headas_chat(NORMAL, "%s: Warning: on-ground bias correction NOT performed\n", global.taskname);
      goto ok_end;
    }

  if(Compute_SubImages())
    {
      goto Error;
    }

  if(ComputeBiasMedian(inunit))
    {
      goto Error;
    }

  
  /* Correct PHA  */   
  if ((PCPhaReCorrect(inunit, outunit, bp_table_amp1, bp_table_amp2)))
    {
      headas_chat(NORMAL, "%s: Error: Unable to correct PHAS values.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;
  

  /* Set XPHASCO keyword to true */
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XPHASCO, &logical, CARD_COMM_XPHASCO, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XPHASCO);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }


	 /* Update XBADFRAM keyword */
  
  if(fits_update_key(outunit, TLOGICAL, KWNM_XBADFRAM, &global.xbadfram, CARD_COMM_XBADFRAM, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XBADFRAM);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
      goto Error;
    }
    
    
  outExt++;
  
  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
      
      outExt++;
    }



  if(UpdatePCCorrExt(outunit))
    {
      goto Error;
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
  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file.\n", global.taskname);
      goto Error;
    }
  
		
    
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);
  
  if(global.xbadfram == TRUE)
		{
			
				headas_chat(NORMAL, "%s: Warning: Found events with RAWX/RAWY coordinates out of nominal range.\n", global.taskname);
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
} /* xrtpccorr_work */


/*
 *	xrtpccorr
 *
 *
 *	DESCRIPTION:
 *              Tool to correct phas events values for Photon-Counting readout mode  
 *      
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
 *             void xrtpccorr_getpar(void);
 * 	       void xrtpccorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - NS 30/03/2007 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpccorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTPCCORR_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( xrtpccorr_getpar() == OK) 
    {
      
      if ( xrtpccorr_work()) 
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
  
} /* xrtpccorr */


/* 
 * 
 *  xrtpccorr_checkinput 
 *
 *  DESCRIPTION:
 *    Routine to check input parameters
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtpccorr_checkinput(void)
{
  int            overwrite=0;
  char           stem[10] , ext[MAXEXT_LEN] ;
  pid_t          tmp;
  
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
	      if(remove (global.par.outfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
		  goto check_end;
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


  if((stricmp(global.par.biasmethod, BIAS_MEDIAN))&&(stricmp(global.par.biasmethod, BIAS_MEAN)))
    {
      headas_chat(NORMAL, "%s: Error: '%s' not a valid parameter for biasmethod.\n", global.taskname,global.par.biasmethod);
      goto check_end;
    }


  if(global.par.grademin>global.par.grademax)
    {
      headas_chat(NORMAL, "%s: Error: Input parameter %s must be <= %s\n",global.taskname,PAR_GRADEMIN,PAR_GRADEMAX);
      goto check_end;
    }

 
  return OK;

 check_end:
  return NOT_OK;
} /*  xrtpccorr_checkinput */


/*
 *
 *      PCPhaReCorrect
 *
 *	DESCRIPTION:
 *             Routine to correct events PHAS
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - 30/03/2007 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PCPhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS])
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0, ccdframe;
  unsigned           i_orbit, i_subimg, i_subset, jj;
  int                neigh_x[] = {NEIGH_X};
  int                neigh_y[] = {NEIGH_Y};
  double             time, sens=0.00001;
  int                detx, dety, rawx, rawy, amp=1;
  char               comm[256]="";
  Ev2Col_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
 
  headas_chat(CHATTY, "%s: Info: Computing PHAS correction\n",  global.taskname);
 
  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  
  nCols=outtable.nColumns;
  
  
  /* Get cols index from name */
  
  /*  CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
 
  /* PHAS */
  if ((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  if (outtable.Multiplicity[indxcol.PHAS] != PHAS_MOL)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, CLNM_PHAS, outtable.Multiplicity[indxcol.PHAS]); 
      headas_chat(NORMAL,"%s: Error: but should be: %d \n", global.taskname, PHAS_MOL); 
      goto reco_end;
    }

  /*  STATUS */
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_STATUS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }


  /* PHASO */
  if(global.evt.xphasco)
    {
      headas_chat(NORMAL, "%s: Info: %s keyword set to TRUE\n", global.taskname, KWNM_XPHASCO);
      headas_chat(NORMAL, "%s: Info: column %s will be overwritten\n", global.taskname, CLNM_PHAS);

      if ((indxcol.PHASO=ColNameMatch(CLNM_PHASO, &outtable)) == -1)
	{ 
	  headas_chat(NORMAL, "%s: Error: %s keyword set to TRUE\n", global.taskname, KWNM_XPHASCO);
	  headas_chat(NORMAL, "%s: Error: but %s column does not exist\n",  global.taskname, CLNM_PHASO);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
	  goto reco_end;
	}
    }
  else
    { 
      headas_chat(NORMAL, "%s: Info: %s keyword not found or set to FALSE\n", global.taskname,KWNM_XPHASCO);
      
      if ((indxcol.PHASO=ColNameMatch(CLNM_PHASO, &outtable)) == -1)
	{
	  headas_chat(NORMAL,"%s: Info: new column %s will be added to output file %s\n", global.taskname, CLNM_PHASO,global.par.outfile);
	  AddColumn(&head, &outtable,CLNM_PHASO, CARD_COMM_PHASO, "9I",TUNIT|TMIN|TMAX, UNIT_CHANN, CARD_COMM_PHYSUNIT, PHA_MIN, PHA_MAX);
	  sprintf(comm, "Added %s column", CLNM_PHASO);
	  indxcol.PHASO=ColNameMatch(CLNM_PHASO, &outtable);
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: existing column %s will be overwritten\n", global.taskname, CLNM_PHASO);
	}   
    }


  /* DETX */
  if ((indxcol.DETX=ColNameMatch(CLNM_DETX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
  

  /* DETY */
  if ((indxcol.DETY=ColNameMatch(CLNM_DETY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }

  /*  TIME */
  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }

  /*  RAWX */
  if((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
  
  /*  RAWY */
  if((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }
  
  /*  Amp */
  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }


  
  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: re-calcolated PHAS.", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      AddHistory(&head, comm);
      
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;
  global.xbadfram = FALSE;


  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{

	  if(!global.evt.xphasco)
	    {
	      /*  Copy PHAS column values in PHASO column */
	      for (jj=0; jj< PHAS_MOL; jj++)
		{
		  IVECVEC(outtable,n,indxcol.PHASO,jj)=IVECVEC(outtable,n,indxcol.PHAS,jj);
		}
	    }

	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);
	  time = DVEC(outtable, n, indxcol.TIME);
	  rawx = IVEC(outtable,n,indxcol.RAWX);
	  rawy = IVEC(outtable,n,indxcol.RAWY);

	  if(!(XVEC2BYTE(outtable,n,indxcol.STATUS)&EV_CALIB_SRC_1)
	     && !(XVEC2BYTE(outtable,n,indxcol.STATUS)&EV_CALIB_SRC_2)
	     && !(XVEC2BYTE(outtable,n,indxcol.STATUS)&EV_CALIB_SRC_3)
	     && !(XVEC2BYTE(outtable,n,indxcol.STATUS)&EV_CALIB_SRC_4)
	     )
	    {

	      for(i_orbit=0; i_orbit<global.tot_orbits; i_orbit++)
		{
		  
		  if((!i_orbit)&&time<global.orbits[i_orbit].time-sens)
		    goto found_orbit;
		  
		  if (i_orbit==global.tot_orbits-1)
		    {
		      if(time >= (global.orbits[i_orbit].time-sens))
			goto found_orbit;
		    }
		  else
		    {
		      if(time >= (global.orbits[i_orbit].time-sens) && time < (global.orbits[i_orbit+1].time-sens))
			goto found_orbit;
		    }
		}
	      
	      headas_chat(NORMAL, "%s: Error: CCDFrame %d out of orbits range time\n", global.taskname, ccdframe);
	      goto reco_end;
	      
	      
	    found_orbit:
	      
	      detx=IVEC(outtable,n,indxcol.DETX);
	      dety=IVEC(outtable,n,indxcol.DETY);
	      
	      
	      if(!global.par.subimage)
		{
		  if(global.orbits[i_orbit].tot_subimg)
		    {
		      i_subimg=0;
		      goto found_subimg;
		    }
		}
		if (rawx < RAWX_MIN || rawy < RAWY_MIN || rawx > RAWX_MAX || rawy > RAWY_MAX)
		{
				headas_chat(CHATTY, "%s: Warning: CCDFrame %d out of detector RAWX/RAWY range\n", global.taskname, ccdframe);
				global.xbadfram = TRUE;
	      goto next_row;
		}
	      
	      for(i_subimg=0; i_subimg<global.orbits[i_orbit].tot_subimg; i_subimg++)
		{
		  if( detx >= global.orbits[i_orbit].subimg[i_subimg].x_min && detx <= global.orbits[i_orbit].subimg[i_subimg].x_max &&
		      dety >= global.orbits[i_orbit].subimg[i_subimg].y_min && dety <= global.orbits[i_orbit].subimg[i_subimg].y_max )
		    goto found_subimg;
		}
	      
	      headas_chat(NORMAL, "%s: Error: CCDFrame %d out of subimages range\n", global.taskname, ccdframe);
	      goto reco_end;
	      
	      
	    found_subimg:
	      
	      for(i_subset=0; i_subset<global.orbits[i_orbit].subimg[i_subimg].tot_subset; i_subset++)
		{
		  if((!i_subset) && (time<(global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].start_time-sens)))
		    goto found_subset;
		  
		  if(i_subset==global.orbits[i_orbit].subimg[i_subimg].tot_subset-1)
		    {
		      if(time>=(global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].start_time-sens))
			goto found_subset;
		    }
		  else
		    {
		      if((time>=(global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].start_time-sens))&&
			 (time<(global.orbits[i_orbit].subimg[i_subimg].subset[i_subset+1].start_time-sens)))
			goto found_subset;
		    }
		}
	      
	      headas_chat(NORMAL, "%s: Error: CCDFrame %d out of subset range\n", global.taskname, ccdframe);
	      goto reco_end;
	      
	      
	    found_subset:
	      
	      /*  	  if(i_orbit==0&&i_subimg==0) */
	      /*  	      headas_chat(NORMAL, "%s: Info: CCDFrame %d Time %18.8f SubSet Time %18.8f\n", global.taskname, ccdframe, time, */
	      /*  			  global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].start_time); */
	      
	      
	      /* Get Amplifier number */
	      if (outtable.TagVec[indxcol.Amp] == B)
		amp=BVEC(outtable, n,indxcol.Amp);
	      else
		amp=IVEC(outtable, n,indxcol.Amp);
	      if (amp != AMP1 && amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
		  headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
		  goto reco_end;
		}
	            
	      
	      for (jj=0; jj< PHAS_MOL; jj++)
		{
		  
		  if(amp == AMP1){
		    
		    if((bp_table_amp1[rawx+neigh_x[jj]][rawy+neigh_y[jj]]==NULL) || !(bp_table_amp1[rawx+neigh_x[jj]][rawy+neigh_y[jj]]->bad_flag & OBS_TELEM_BP)){
		      
		      IVECVEC(outtable,n,indxcol.PHAS,jj)=IVECVEC(outtable,n,indxcol.PHASO,jj)-global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].bias;
		    }
		    else{	
		      headas_chat(CHATTY,"%s: Info: FRAME %d RAWX %d RAWY %d  PHAS[%d] (rawx %d rawy %d) not corrected\n",
				  global.taskname, ccdframe, rawx, rawy, jj, rawx+neigh_x[jj], rawy+neigh_y[jj]);
		    }
		  }
		  else{
		    
		    if((bp_table_amp2[rawx+neigh_x[jj]][rawy+neigh_y[jj]]==NULL) || !(bp_table_amp2[rawx+neigh_x[jj]][rawy+neigh_y[jj]]->bad_flag & OBS_TELEM_BP)){
		      
		      IVECVEC(outtable,n,indxcol.PHAS,jj)=IVECVEC(outtable,n,indxcol.PHASO,jj)-global.orbits[i_orbit].subimg[i_subimg].subset[i_subset].bias;
		    }
		    else{
		      headas_chat(CHATTY,"%s: Info: FRAME %d RAWX %d RAWY %d  PHAS[%d] (rawx %d rawy %d) not corrected\n",
				  global.taskname, ccdframe, rawx, rawy, jj, rawx+neigh_x[jj], rawy+neigh_y[jj]);
		    }
		  }
		}
	       
	    }
	  else
	    {
	      headas_chat(CHATTY,"%s: Info: FRAME %d RAWX %d RAWY %d not corrected\n",
			  global.taskname, ccdframe, rawx, rawy);
	    }

	  next_row:
	  	
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
  
} /* PCPhaReCorrect */


/*
 *
 *  ComputeSrc_XY
 *
 *  DESCRIPTION:
 *    Routine to compute source sky coordinates
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeSrc_XY(FitsFileUnit_t evunit, TELDEF* teldef, double* src_x, double* src_y){

  FitsHeader_t	     head;
  int                sizex, sizey;
  double             roll, pixsize, tmp_src_x, tmp_src_y;


  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  /* Compute source X/Y */
  sizex=SIZE_X;
  sizey=SIZE_Y;
  roll=270.;
  
  pixsize=PIX_SIZE_DEG;
  
  if(RADec2XY(global.par.srcra, global.par.srcdec, sizex, sizey, global.par.ranom, global.par.decnom, roll, pixsize, &tmp_src_x, &tmp_src_y))
    {
      headas_chat(NORMAL, "%s: Error: Unable to calculate source SKY coordinates.\n",global.taskname);
      goto ComputeSrc_XY_end;
    } 
  
  *src_x=tmp_src_x;
  *src_y=tmp_src_y;
  
  return OK;
  
 ComputeSrc_XY_end:
  
  return NOT_OK;
  
} /*  ComputeSrc_XY */


/*
 *
 *  ComputeDetCoord
 *
 *  DESCRIPTION:
 *    Routine to calculate source DET coordinates
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeDetCoord(double src_x, double src_y, double time, double mjdref, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord)
{

  
  double     db_dety_tmp=1., db_x, db_y, src_dety=0., db_detx_tmp=1., src_detx=0.;
  double     mjd=0.;
  QUAT*      q;
  XFORM2D*   sky2det;
  
  sky2det = allocateXform2d();
  q = allocateQuat();

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Error: Time: %f is not included\n", global.taskname, time);
      headas_chat(NORMAL,"%s: Error: in the time range of the\n", global.taskname); 
      headas_chat(NORMAL,"%s: Error: %s Attitude file.\n", global.taskname, global.par.attfile);
      coord->detx = KWVL_DETNULL; 
      coord->dety = KWVL_DETNULL; 
      /*headas_chat(CHATTY,"%s: Warning: Setting detx = %d and dety = %d.\n", global.taskname, coord->detx,coord->dety );*/	 
      return NOT_OK;
	    	      
    } 
  
     
  /***************************************************
   * the current time is covered by the attitude file *
   ***************************************************/
  
  findQuatInAttFile(attfile,q,time);

  /*printf("TIME ==%10.5f QUAT ==%5.10f %5.10f  %5.10f  %5.10f\n", time, q->p[0],q->p[1],q->p[2],q->p[3]);*/

  if(global.par.aberration)
    {
      mjd=mjdref + time/86400.;

      coord->v=earth_velocity(coord->vhat,earth_longitude(mjd));
    }
  else
    {
      coord->v=0.;
      coord->vhat[0]=0.;
      coord->vhat[1]=0.;
      coord->vhat[2]=0.;
    }

  convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				  db_detx_tmp, db_dety_tmp, 
				  q, coord->v, coord->vhat);
  
  invertXform2d(sky2det, teldef->det2sky);
  applyXform2dToContinuousCoords(sky2det, &src_detx, &src_dety, src_x, src_y);
  
  coord->detx=src_detx;
  coord->dety=src_dety;
   
  return OK;

} /*  ComputeDetCoord */


/*
 *
 *  ComputeOrbits
 *
 *  DESCRIPTION:
 *    Routine to calculate orbits range time and source DET coordinates
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeOrbits(double src_x, double src_y){

  int                status=OK, tmp_settled, tmp_safehold, readmkf;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[3];
  double             tmp_time, tmp_endtime;
  long               extno;
  double             mkftstart=0, mkftstop=0;
  double             sens=0.00001;
  BOOL               first=1, changeorbit=1;
  Coord_t            coordset;
  MKFCol_t           mkfcol;
/*    FitsCard_t         *card; */
  Bintable_t	     mkftable;
  FitsHeader_t	     mkfhead;
  FitsFileUnit_t     mkfunit=NULL;

  ATTFILE            *attfile=NULL;
  TELDEF             *teldef=NULL;

  TMEMSET0( &mkftable, Bintable_t );
  TMEMSET0( &mkfhead, FitsHeader_t );

  headas_chat(CHATTY, "%s: Info: Computing Orbits\n",  global.taskname);

  if(!global.use_detxy)
    {
      /* Open Attitude file */
      attfile = openAttFile(global.par.attfile);

      if(global.par.attinterpol)
	setAttFileInterpolation(attfile, 1);
      else
	setAttFileInterpolation(attfile, 0);


      /* Query CALDB to get teldef filename? */
      if (!(strcasecmp (global.par.teldef, DF_CALDB)))
	{ 
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extno))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for teldef file.\n", global.taskname);
	      goto ComputeOrbits_end;
	    }
	  else
	    headas_chat(NORMAL, "%s: Info: Processing '%s'  CALDB file.\n", global.taskname, global.par.teldef);
	}
      
      /* Open TELDEF file */
      teldef = readTelDef(global.par.teldef);
      
      setSkyCoordCenterInTeldef(teldef, global.par.ranom, global.par.decnom);
      
    }
  
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.mkffile);

  /* Open readonly input mkf file */
  if ((mkfunit=OpenReadFitsFile(global.par.mkffile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.mkffile);
      goto ComputeOrbits_end;
    }

 
  /* Move in filter extension in input mkf file */
  if (fits_movnam_hdu(mkfunit, ANY_HDU, "FILTER", 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,"FILTER");
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.mkffile); 
      if( CloseFitsFile(mkfunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.mkffile);
	}
      goto ComputeOrbits_end;
    }

  /* Retrieve header pointer */    
  mkfhead=RetrieveFitsHeader(mkfunit);  
  


  /* Retrieve TSTART from mkf file */
/*    if(ExistsKeyWord(&mkfhead, KWNM_TSTART, &card)) */
/*      mkftstart=card->u.DVal; */
/*    else */
/*      { */
/*        headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART); */
/*        headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.mkffile); */
/*        goto ComputeOrbits_end; */
/*      } */

/*    if(mkftstart>global.evt.tstart-sens) */
/*      { */
/*        headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname); */
/*        headas_chat(NORMAL, "%s: Error: %s file\n", global.taskname,global.par.infile); */
/*        headas_chat(NORMAL, "%s: Error: are not included in the range time\n", global.taskname);  */
/*        headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.mkffile); */
/*        goto ComputeOrbits_end; */
/*      } */
  
  /* Retrieve TSTOP from mkf file */
/*    if(ExistsKeyWord(&mkfhead, KWNM_TSTOP, &card)) */
/*      mkftstop=card->u.DVal; */
/*    else */
/*      { */
/*        headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP); */
/*        headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.mkffile); */
/*        goto ComputeOrbits_end; */
/*      } */

/*    if(mkftstop<global.evt.tstop+sens) */
/*      { */
/*        headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname); */
/*        headas_chat(NORMAL, "%s: Error: %s file\n", global.taskname,global.par.infile); */
/*        headas_chat(NORMAL, "%s: Error: are not included in the range time\n", global.taskname);  */
/*        headas_chat(NORMAL, "%s: Error: of the %s file.\n", global.taskname, global.par.mkffile); */
/*        goto ComputeOrbits_end; */
/*      } */


  GetBintableStructure(&mkfhead, &mkftable, BINTAB_ROWS, 0, NULL);
  
  if(!mkftable.MaxRows)
    {
      
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.mkffile);
      goto ComputeOrbits_end;
    }

  /* Start Time */
  if ((mkfcol.TIME=ColNameMatch(CLNM_TIME, &mkftable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.mkffile);
      goto ComputeOrbits_end;
    }

  if ((mkfcol.SAFEHOLD=ColNameMatch("SAFEHOLD", &mkftable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "SAFEHOLD");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.mkffile);
      goto ComputeOrbits_end;
    }

  if ((mkfcol.SETTLED=ColNameMatch("SETTLED", &mkftable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "SETTLED");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.mkffile);
      goto ComputeOrbits_end;
    }


  EndBintableHeader(&mkfhead, &mkftable);
 
  FromRow = 1;
  ReadRows=mkftable.nBlockRows;


  /* Read only the following columns */
  nCols=3; 
  ActCols[0]=mkfcol.TIME;
  ActCols[1]=mkfcol.SAFEHOLD;
  ActCols[2]=mkfcol.SETTLED;

  first=1;
  tmp_endtime=0.;
  readmkf=1;

  while(readmkf && ReadBintableWithNULL(mkfunit, &mkftable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{

	  tmp_time=DVEC(mkftable, n, mkfcol.TIME);
	  /*  headas_chat(NORMAL, "%s: Info: mkf time %G\n", global.taskname, tmp_time); */

	  if(FromRow+n==1)
	    mkftstart=tmp_time;

	  if(tmp_time>global.evt.tstop+sens && !first)
	    {
	      mkftstop=tmp_time;
	      readmkf=0;
	      break;
	    }

	  tmp_settled=BVEC(mkftable, n, mkfcol.SETTLED);
	  /*  headas_chat(NORMAL, "%s: Info: mkf settled %d\n", global.taskname, tmp_settled); */

	  tmp_safehold=BVEC(mkftable, n, mkfcol.SAFEHOLD);
	  /*  headas_chat(NORMAL, "%s: Info: mkf safehold %d\n", global.taskname, tmp_safehold); */
	

	  if(tmp_settled==1 && tmp_safehold==0)
	    {
	      if(first || ((tmp_time - tmp_endtime) > DIFF_TIME))
		changeorbit=1;
	      
	      
	      if(changeorbit)
		{	  

		  if(global.use_detxy)
		    {
		      if(!global.tot_orbits)
			{
			  global.tot_orbits++;
			  global.orbits = (MKFInfo_t *)malloc(sizeof(MKFInfo_t));
			}
		      else
			{
			  /*  Overwrite orbit previous event tstart */
			  if(!(global.orbits[global.tot_orbits-1].time<(global.evt.tstart-sens) && tmp_time<(global.evt.tstart-sens)))
			    {
			      global.tot_orbits++;
			      global.orbits = (MKFInfo_t *)realloc(global.orbits, (global.tot_orbits*sizeof(MKFInfo_t)));
			    }
			}
		      
		      global.orbits[global.tot_orbits-1].time=tmp_time;
		      global.orbits[global.tot_orbits-1].tot_subimg=0;
		      global.orbits[global.tot_orbits-1].total_img=0;
		      global.orbits[global.tot_orbits-1].central_img=0;
		      global.orbits[global.tot_orbits-1].srcdetx = global.par.srcdetx;
		      global.orbits[global.tot_orbits-1].srcdety = global.par.srcdety;
		      
		      changeorbit=0;
		      first=0;
		    }
		  else
		    {
/*  		      headas_chat(NORMAL, "%s: Info: ComputeDetCoord src_x %15f src_y %15f tmp_time %15f mjref %15f\n", */
/*  				  global.taskname, src_x, src_y, tmp_time, global.evt.mjdref); */

		      if(ComputeDetCoord(src_x, src_y, tmp_time, global.evt.mjdref, teldef, attfile, &coordset))
			{
			  headas_chat(NORMAL, "%s: Error: Unable to calculate source DET coordinates.\n", global.taskname);
			  goto ComputeOrbits_end;
			}
		      
		      if(!finite(coordset.detx) || !finite(coordset.dety) || coordset.detx > DETX_MAX || coordset.detx < DETX_MIN ||
			 coordset.dety > DETY_MAX || coordset.dety < DETY_MIN )
			{  
			  /*  printf("TIME==%18.8f  DETX==%5.5f DETY==%5.5f \n", tmp_time, coordset.detx,coordset.dety); */
			  headas_chat(CHATTY, "%s: Warning: The source is out of the CCD for time %18.8f\n", global.taskname, tmp_time);
			}
		      else
			{
			  if(!global.tot_orbits)
			    {
			      global.tot_orbits++;
			      global.orbits = (MKFInfo_t *)malloc(sizeof(MKFInfo_t));
			    }
			  else
			    {
			      /*  Overwrite orbit previous event tstart */
			      if(!(global.orbits[global.tot_orbits-1].time<global.evt.tstart && tmp_time<global.evt.tstart))
				{
				  global.tot_orbits++;
				  global.orbits = (MKFInfo_t *)realloc(global.orbits, (global.tot_orbits*sizeof(MKFInfo_t)));
				}
			    }
			   
			  /*  printf("TIME==%18.8f  DETX==%5.5f DETY==%5.5f \n", tmp_time, coordset.detx,coordset.dety); */
			  
			  global.orbits[global.tot_orbits-1].time=tmp_time;
			  global.orbits[global.tot_orbits-1].tot_subimg=0;
			  global.orbits[global.tot_orbits-1].total_img=0;
			  global.orbits[global.tot_orbits-1].central_img=0;
			  global.orbits[global.tot_orbits-1].srcdetx = (int)(coordset.detx + 0.5);
			  global.orbits[global.tot_orbits-1].srcdety = (int)(coordset.dety + 0.5);

			  changeorbit=0;
			  first=0;
			}
		    }
		}
	    }
	  else
	    {
	      changeorbit=1;
	    }
	  
	  tmp_endtime=tmp_time;
	  mkftstop=tmp_time;

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  
  if(mkftstart>global.evt.tstart+sens+DTIME || mkftstop<global.evt.tstop-sens-DTIME)
    {
      headas_chat(NORMAL, "%s: Warning: The times in the\n", global.taskname);
      headas_chat(NORMAL, "%s: Warning: %s file\n", global.taskname,global.par.infile);
      headas_chat(NORMAL, "%s: Warning: are not included in the range time\n", global.taskname); 
      headas_chat(NORMAL, "%s: Warning: of the %s file.\n", global.taskname, global.par.mkffile);
      global.warning=1;
      return OK;
    }


  if(!global.tot_orbits)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.mkffile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  ComputeOrbits_end;
    }

  for (n=0;n<global.tot_orbits;n++)
    {
      headas_chat(NORMAL, "%s: Info: Orbit %d start time %18.8f srcdetx %d srcdey %d\n", global.taskname,n+1, global.orbits[n].time,
		  global.orbits[n].srcdetx, global.orbits[n].srcdety);
    }


  ReleaseBintable(&mkfhead, &mkftable);
  return OK;

 ComputeOrbits_end:

  if (mkfhead.first)
    ReleaseBintable(&mkfhead, &mkftable);

  return NOT_OK;

} /*  ComputeOrbits */


/*
 *
 *  Compute_SubImages
 *
 *  DESCRIPTION:
 *    Routine to calculate SubImages
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int Compute_SubImages(){

  unsigned           n;
  int                subimg_centerx, subimg_centery;

  global.evt.img_xmin=CCD_CENTERX - global.evt.whalfwd;
  global.evt.img_xmax=CCD_CENTERX + global.evt.whalfwd -1;
  global.evt.img_ymin=CCD_CENTERY - global.evt.whalfht;
  global.evt.img_ymax=CCD_CENTERY + global.evt.whalfht -1;

  headas_chat(CHATTY, "%s: Info: Computing SubImages\n",  global.taskname);

  for (n=0;n<global.tot_orbits;n++)
    {
      headas_chat(CHATTY, "%s: Info: Orbit start time %18.8f srcdetx %d srcdety %d\n",
		  global.taskname, global.orbits[n].time,  global.orbits[n].srcdetx,  global.orbits[n].srcdety);
      
      
      subimg_centerx = global.orbits[n].srcdetx;
      subimg_centery = global.orbits[n].srcdety;
      
      ComputeImgRange(subimg_centerx, subimg_centery, V_CENTER, n);

    }

  return OK;
} /*  Compute_SubImages */


/*
 *
 *  ComputeImgRange
 *
 *  DESCRIPTION:
 *    
 *
 *
 */
void ComputeImgRange(int center_x, int center_y, unsigned versus, unsigned n){

  int                img_xmin, img_xmax, img_ymin, img_ymax;
  int                subimg_xmin, subimg_xmax, subimg_ymin, subimg_ymax;
  int                up=1, down=1, left=1, right=1;
  int                next_center_x, next_center_y;

  img_xmin=CCD_CENTERX - global.evt.whalfwd;
  img_xmax=CCD_CENTERX + global.evt.whalfwd -1;
  img_ymin=CCD_CENTERY - global.evt.whalfht;
  img_ymax=CCD_CENTERY + global.evt.whalfht -1;


  /*  find subimg_xmin */	  
  subimg_xmin = center_x - global.par.subimgsize/2;
  
  if( subimg_xmin < img_xmin)
    {
      center_x = center_x + (img_xmin - subimg_xmin);
      subimg_xmin = center_x - global.par.subimgsize/2;
    }
  
  if( subimg_xmin - img_xmin < global.par.subimgsize)
    {
      subimg_xmin = img_xmin;
      left=0;
    }
  
  /*  find subimg_xmax */  
  subimg_xmax = center_x + global.par.subimgsize/2 - 1;
  
  if( subimg_xmax > img_xmax)
    {
      center_x = center_x - (subimg_xmax - img_xmax);
      subimg_xmax = center_x + global.par.subimgsize/2 -1;
    }
  
  if(img_xmax - subimg_xmax < global.par.subimgsize)
    {
      subimg_xmax = img_xmax;
      right=0;
    }
  
  /*  find subimg_ymin */	  
  subimg_ymin = center_y - global.par.subimgsize/2;
  
  if( subimg_ymin < img_ymin)
    {
      center_y = center_y + (img_ymin - subimg_ymin);
      subimg_ymin = center_y - global.par.subimgsize/2;
    }
  
  if( subimg_ymin - img_ymin < global.par.subimgsize)
    {
      subimg_ymin = img_ymin;
      down=0;
    }
  
  /*  find subimg_ymax */
  subimg_ymax = center_y + global.par.subimgsize/2 - 1;
  
  if( subimg_ymax > img_ymax)
    {
      center_y = center_y - (subimg_ymax - img_ymax);
      subimg_ymax = center_y + global.par.subimgsize/2 -1;
    }
  
  if(img_ymax - subimg_ymax < global.par.subimgsize)
    {
      subimg_ymax = img_ymax;
      up=0;
    }
  
  
  if(!global.orbits[n].tot_subimg)
    {
      global.orbits[n].tot_subimg++;
      global.orbits[n].subimg = (SubImagesInfo_t *)malloc(sizeof(SubImagesInfo_t));
    }
  else
    {
      global.orbits[n].tot_subimg++;
      global.orbits[n].subimg = (SubImagesInfo_t *)realloc(global.orbits[n].subimg ,global.orbits[n].tot_subimg*sizeof(SubImagesInfo_t));
    }
  
  global.orbits[n].subimg[global.orbits[n].tot_subimg-1].x_min = subimg_xmin;
  global.orbits[n].subimg[global.orbits[n].tot_subimg-1].x_max = subimg_xmax;
  global.orbits[n].subimg[global.orbits[n].tot_subimg-1].y_min = subimg_ymin;
  global.orbits[n].subimg[global.orbits[n].tot_subimg-1].y_max = subimg_ymax;
  
  headas_chat(CHATTY, "%s: Info: Orbit start time %18.8f SubImg %d => x_min %d  x_max %d  y_min %d  y_max %d\n",
	      global.taskname, global.orbits[n].time,global.orbits[n].tot_subimg,
	      global.orbits[n].subimg[global.orbits[n].tot_subimg-1].x_min,
	      global.orbits[n].subimg[global.orbits[n].tot_subimg-1].x_max,
	      global.orbits[n].subimg[global.orbits[n].tot_subimg-1].y_min,
	      global.orbits[n].subimg[global.orbits[n].tot_subimg-1].y_max);
  

  if(!global.par.subimage)
    return;
  
  
  if(versus==V_CENTER){
    
    if(up)
      {
	next_center_x = center_x;
	next_center_y = subimg_ymax + global.par.subimgsize/2 +1;
	ComputeImgRange(next_center_x, next_center_y, V_UP,n);
      } 
    
    if(left)
      {
	next_center_x = subimg_xmin - global.par.subimgsize/2;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_LEFT,n);
      }
    
    if(right)
      {
	next_center_x = subimg_xmax + global.par.subimgsize/2 +1;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_RIGHT, n);
      }

    if(down)
      {
	next_center_x = center_x;
	next_center_y = subimg_ymin - global.par.subimgsize/2;
	ComputeImgRange(next_center_x, next_center_y, V_DOWN,n);
      }
  }
  
  if(versus==V_UP){
    
    if(up)
      {
	next_center_x = center_x;
	next_center_y = subimg_ymax + global.par.subimgsize/2 +1;
	ComputeImgRange(next_center_x, next_center_y, V_UP,n);
      } 
    
    if(left)
      {
	next_center_x = subimg_xmin - global.par.subimgsize/2;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_LEFT,n);
      }
    
    if(right)
      {
	next_center_x = subimg_xmax + global.par.subimgsize/2 +1;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_RIGHT, n);
      }
  }
  
  if(versus==V_DOWN){
    
    if(left)
      {
	next_center_x = subimg_xmin - global.par.subimgsize/2;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_LEFT,n);
      }
    
    if(right)
      {
	next_center_x = subimg_xmax + global.par.subimgsize/2 +1;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_RIGHT, n);
      }
    
    if(down)
      {
	next_center_x = center_x;
	next_center_y = subimg_ymin - global.par.subimgsize/2;
	ComputeImgRange(next_center_x, next_center_y, V_DOWN,n);
      }
  }
  
  if(versus==V_LEFT){
    
    if(left)
      {
	next_center_x = subimg_xmin - global.par.subimgsize/2;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_LEFT,n);
      }
  }
  
  if(versus==V_RIGHT){
    
    if(right)
      {
	next_center_x = subimg_xmax + global.par.subimgsize/2 +1;
	next_center_y = center_y;
	ComputeImgRange(next_center_x, next_center_y, V_RIGHT, n);
      }
  }
} /*  ComputeImgRange */


/*
 *
 *  ComputeBiasMedian
 *
 *  DESCRIPTION:
 *    Routine to calculate phas mean or median
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeBiasMedian(FitsFileUnit_t evunit){

  unsigned           FromRow, ReadRows, n, nCols, ActCols[7], grade, ccdframe;
  int                i=0, ii=0, jj=0, nevents=0,nextsubset, kk, total_img;
  int                detx, dety;
  int                in_orbit, in_subimg;
  int                pix2_bias, pix4_bias, pix7_bias, pix9_bias;
  int                subset_end, subset_init, subsetnum, first_subset, bias_median=0, merge_lastsubset, lessevents;
  int                *bias, bias_index;
  double             time, sens=0.00001;
  unsigned           *subimgccdframe;
  int                subimgccdframe_dim=0;
  int                **biasmatrix;
  int                **subsetmatrix;
  int                *biasmatrix_dim;
  int                *subsetmatrix_dim;
  Ev2Col_t           indxcol;
  Bintable_t	     evtable;
  FitsHeader_t	     evhead;

  TMEMSET0( &evtable, Bintable_t );
  TMEMSET0( &evhead, FitsHeader_t );

  headas_chat(CHATTY, "%s: Info: Computing PHAS mean/median\n",  global.taskname);
  
  evhead=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&evhead, &evtable, BINTAB_ROWS, 0, NULL);
  
  if(!evtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  
  /* Get cols index from name */
 
  /*  CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputeBiasMedian_end;
    }
  

  /* PHAS or PHASO */
  if(global.evt.xphasco)
    {
      if ((indxcol.PHAS=ColNameMatch(CLNM_PHASO, &evtable)) == -1)
	{ 
	  headas_chat(NORMAL, "%s: Error: %s keyword set to TRUE\n", global.taskname, KWNM_XPHASCO);
	  headas_chat(NORMAL, "%s: Error: but %s column does not exist\n",  global.taskname, CLNM_PHASO);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
	  goto ComputeBiasMedian_end;
	}
    }
  else
    {
      if ((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &evtable)) == -1)
	{ 
	  headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHAS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
	  goto ComputeBiasMedian_end;
	}
    }


  if (evtable.Multiplicity[indxcol.PHAS] != PHAS_MOL)
    {
      headas_chat(NORMAL,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, CLNM_PHAS, evtable.Multiplicity[indxcol.PHAS]); 
      headas_chat(NORMAL,"%s: Error: but should be: %d \n", global.taskname, PHAS_MOL); 
      goto ComputeBiasMedian_end;
    }

  /*  STATUS */
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_STATUS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto ComputeBiasMedian_end;
    }
  
  /*  GRADE */
  if((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto ComputeBiasMedian_end;
    }

  /* DETX */
  if ((indxcol.DETX=ColNameMatch(CLNM_DETX, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputeBiasMedian_end;
    }
  

  /* DETY */
  if ((indxcol.DETY=ColNameMatch(CLNM_DETY, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputeBiasMedian_end;
    }

  /*  TIME */
  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &evtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputeBiasMedian_end;
    }

  EndBintableHeader(&evhead, &evtable);
 

  /* Read only the following columns */
  nCols=7;
  ActCols[0]=indxcol.PHAS;
  ActCols[1]=indxcol.STATUS;
  ActCols[2]=indxcol.GRADE;
  ActCols[3]=indxcol.CCDFrame;
  ActCols[4]=indxcol.DETX;
  ActCols[5]=indxcol.DETY;
  ActCols[6]=indxcol.TIME;



  for(i=0; i<global.tot_orbits; i++){

    total_img=0;

     headas_chat(CHATTY, "%s: Info: Computing bias for Orbit %d\n", global.taskname, i+1);


  COMPUTE_BIAS:
    
    FromRow = 1;
    ReadRows=evtable.nBlockRows;

    biasmatrix = malloc(global.orbits[i].tot_subimg * sizeof(int *));
    biasmatrix_dim = malloc(global.orbits[i].tot_subimg * sizeof(int));

    subsetmatrix = malloc(global.orbits[i].tot_subimg * sizeof(int *));
    subsetmatrix_dim = malloc(global.orbits[i].tot_subimg * sizeof(int));

    subimgccdframe_dim = global.orbits[i].tot_subimg;
    subimgccdframe = (unsigned*)calloc(subimgccdframe_dim, sizeof(unsigned)); 


    for(ii=0; ii<global.orbits[i].tot_subimg; ii++)
      {
	global.orbits[i].subimg[ii].tot_subset = 0;
	biasmatrix_dim[ii] = 0;
	subsetmatrix_dim[ii] = 0;
      }


    while(ReadBintableWithNULL(evunit, &evtable, nCols, ActCols,FromRow,&ReadRows) == 0 )
      {
	for(n=0; n<ReadRows ; ++n)
	  {

	    in_orbit=0;

	    time = DVEC(evtable, n, indxcol.TIME);

	    if (i==global.tot_orbits-1)
	      {
		if(time >= global.orbits[i].time-sens)
		  in_orbit=1;
	      }
	    else
	      {
		if(time >= global.orbits[i].time-sens && time < global.orbits[i+1].time-sens)
		  in_orbit=1;
	      }

	    if(in_orbit)
	      {
  
		grade = IVEC(evtable, n, indxcol.GRADE);
		
		if((!global.par.badpix || (XVEC2BYTE(evtable,n,indxcol.STATUS) == GOOD))&&
		   (grade>=global.par.grademin && grade<=global.par.grademax))
		  {
		    
		    ccdframe=VVEC(evtable, n, indxcol.CCDFrame);
		    detx=IVEC(evtable,n,indxcol.DETX);
		    dety=IVEC(evtable,n,indxcol.DETY);
		    
/*    		    headas_chat(CHATTY, "%s: Info: CCDFrame %d Time %18.8f  Orbit start time %18.8f  OK for phas correction.\n", */
/*  				global.taskname, ccdframe, time, global.orbits[i].time); */


		    in_subimg=0;
		    for(ii=0; ii<global.orbits[i].tot_subimg; ii++)
		      {
			if( detx >= global.orbits[i].subimg[ii].x_min && detx <= global.orbits[i].subimg[ii].x_max &&
			    dety >= global.orbits[i].subimg[ii].y_min && dety <= global.orbits[i].subimg[ii].y_max )
			  {
			    
/*  			    headas_chat(CHATTY, "%s: Info: CCDFrame %d detx %d dety %d in subimg %d (xmin %d xmax %d ymin %d ymax %d)\n", */
/*  					global.taskname, ccdframe, detx, dety, ii+1, */
/*  					global.orbits[i].subimg[ii].x_min, global.orbits[i].subimg[ii].x_max, */
/*  					global.orbits[i].subimg[ii].y_min, global.orbits[i].subimg[ii].y_max);	 */	    
			    
			    in_subimg=1;
			    break;
			  }
		      }

		    if(in_subimg)
		      {

			if(!subsetmatrix_dim[ii])
			  nevents=biasmatrix_dim[ii]/4;
			else
			  nevents=(biasmatrix_dim[ii] - subsetmatrix[ii][subsetmatrix_dim[ii]-1])/4;

			nextsubset=0;

			if(nevents>=global.par.nevents  && ccdframe!=subimgccdframe[ii])
			  {
			    subimgccdframe[ii]=ccdframe;

			    if(!subsetmatrix_dim[ii])
			      {
				subsetmatrix_dim[ii]=subsetmatrix_dim[ii]+1;
				subsetmatrix[ii] = malloc(subsetmatrix_dim[ii]*sizeof(int));
			      }
			    else
			      {
				subsetmatrix_dim[ii]=subsetmatrix_dim[ii]+1;
				subsetmatrix[ii] = realloc(subsetmatrix[ii], (subsetmatrix_dim[ii]*sizeof(int)));
			      }

			    subsetmatrix[ii][subsetmatrix_dim[ii]-1] = biasmatrix_dim[ii]-1;

			    nextsubset=1;
			  }


			if( !biasmatrix_dim[ii] || nextsubset)
			  {
			    
			    if(!global.orbits[i].subimg[ii].tot_subset)
			      {
				global.orbits[i].subimg[ii].tot_subset++;
				global.orbits[i].subimg[ii].subset = (SubSetInfo_t *)malloc(sizeof(SubSetInfo_t));
			      }
			    else
			      {
				global.orbits[i].subimg[ii].tot_subset++;
				global.orbits[i].subimg[ii].subset = (SubSetInfo_t *)realloc(global.orbits[i].subimg[ii].subset, global.orbits[i].subimg[ii].tot_subset*sizeof(SubSetInfo_t));
			      }

			    global.orbits[i].subimg[ii].subset[global.orbits[i].subimg[ii].tot_subset-1].start_time=time;

			  }


			if(!biasmatrix_dim[ii])
			  {
			    biasmatrix_dim[ii]=biasmatrix_dim[ii]+4;
			    biasmatrix[ii] = malloc(biasmatrix_dim[ii]*sizeof(int));
			  }
			else
			  {
			    biasmatrix_dim[ii]=biasmatrix_dim[ii]+4;
			    biasmatrix[ii] = realloc(biasmatrix[ii], (biasmatrix_dim[ii]*sizeof(int)));
			  }
			
			biasmatrix[ii][biasmatrix_dim[ii]-4]=IVECVEC(evtable,n,indxcol.PHAS,1);
			biasmatrix[ii][biasmatrix_dim[ii]-3]=IVECVEC(evtable,n,indxcol.PHAS,3);
			biasmatrix[ii][biasmatrix_dim[ii]-2]=IVECVEC(evtable,n,indxcol.PHAS,6);
			biasmatrix[ii][biasmatrix_dim[ii]-1]=IVECVEC(evtable,n,indxcol.PHAS,8);			

		      }

		  }
	      }
	    
	  }
	FromRow += ReadRows;
	ReadRows = BINTAB_ROWS;
      }/* while */ 

    if(!total_img)
      {
	headas_chat(CHATTY,"%s: Info: Orbit %d\n", global.taskname, i+1);
	for(jj=0; jj<global.orbits[i].tot_subimg; jj++)
	  {
	    headas_chat(CHATTY, "%s: Info: Subimage %d (values length %d)\n ", global.taskname, jj+1, biasmatrix_dim[jj]);
	    for(ii=0; ii<biasmatrix_dim[jj]; ii++)
	      {
		headas_chat(CHATTY, "%d ", biasmatrix[jj][ii]);
	      }
	    headas_chat(CHATTY, "\n");
	  }
      }
    else
      {
	if(global.orbits[i].tot_subimg)
	  {
	    headas_chat(CHATTY,"%s: Info: Orbit %d\n", global.taskname, i+1);
	    headas_chat(CHATTY, "%s: Info: Total Image (values length %d)\n ", global.taskname, biasmatrix_dim[0]);
	    for(ii=0; ii<biasmatrix_dim[0]; ii++)
	      {
		headas_chat(CHATTY, "%d ", biasmatrix[0][ii]);
	      }
	    headas_chat(CHATTY, "\n");
	  }
      }
	     

    /*  Check if all SubImg have # events > nevents */
    lessevents=0;

    /*  Skip check if lessevents=yes */
    if(global.par.lessevents)
      {
	goto CHECK_EVENTS_END;
      }

    for(jj=0; jj<global.orbits[i].tot_subimg; jj++)
      {
	
	/*  Only one subset */
	if((!subsetmatrix_dim[jj])&&((biasmatrix_dim[jj]/4)<global.par.nevents))
	  {
	    if(!jj)
	      {
		if(total_img)
		  {
		    headas_chat(NORMAL, "%s: Warning: Orbit %d : total image events (%d) < nevents parameter\n",
				global.taskname, i+1,(int)(biasmatrix_dim[jj]/4));

		    lessevents=1;
		    break;
		  }
		else
		  {
		    headas_chat(NORMAL, "%s: Warning: Orbit %d : central image events (%d) < nevents parameter\n",
				global.taskname, i+1,(int)(biasmatrix_dim[jj]/4));
		    headas_chat(NORMAL, "%s: Warning: using TOTAL image for ALL subimages.\n", global.taskname);
		    
		    global.orbits[i].subimg[0].x_min=global.evt.img_xmin;
		    global.orbits[i].subimg[0].x_max=global.evt.img_xmax;
		    global.orbits[i].subimg[0].y_min=global.evt.img_ymin;
		    global.orbits[i].subimg[0].y_max=global.evt.img_ymax;
		    global.orbits[i].total_img=1;
		    					
		    free(subsetmatrix);
		    free(subsetmatrix_dim);
		    free(biasmatrix);
		    free(biasmatrix_dim);
		    
		    total_img=1;
		    goto COMPUTE_BIAS;
		  }
	      }
	    else if(!total_img)
	      {
		headas_chat(NORMAL, "%s: Warning: Orbit %d SubImage %d events(%d)<nevents, using central subimage bias values\n",
			    global.taskname, i+1, jj+1,(int)(biasmatrix_dim[jj]/4));
		headas_chat(NORMAL, "%s: Warning: for ALL SubImages of this Orbit.\n", global.taskname);

		global.orbits[i].central_img=1;

		lessevents=1;
		break;
	      }
	  }

	if(total_img)
	  {
	    lessevents=1;
	    break;
	  }
      }
    
  CHECK_EVENTS_END:


    for(jj=0; jj<global.orbits[i].tot_subimg; jj++)
      {
	
	subsetnum=0;
	subset_init=0;
	subset_end=0;
	first_subset=1;
	merge_lastsubset=0;

	if(!jj&&total_img)
	  headas_chat(CHATTY, "%s: Info: Computing Bias ===>> Total Image\n", global.taskname);
	else
	  headas_chat(CHATTY, "%s: Info: Computing Bias ===>> Subimage %d\n", global.taskname, jj+1); 


	/*  If #events<nevents use Central SubImage bias for all SubImages */
	if(jj && lessevents)
	  {
	    if(!total_img)
	      headas_chat(CHATTY, "%s: Info: Using bias values of SubImage 1\n", global.taskname);
	    else
	      headas_chat(CHATTY, "%s: Info: Using bias values of Total Image\n", global.taskname);


	    if(!global.orbits[i].subimg[jj].tot_subset)
	      {
		global.orbits[i].subimg[jj].tot_subset=global.orbits[i].subimg[0].tot_subset;
		global.orbits[i].subimg[jj].subset = (SubSetInfo_t *)malloc(global.orbits[i].subimg[0].tot_subset*sizeof(SubSetInfo_t));
	      }
	    else
	      {
		global.orbits[i].subimg[jj].tot_subset=global.orbits[i].subimg[0].tot_subset;
		global.orbits[i].subimg[jj].subset = (SubSetInfo_t *)realloc(global.orbits[i].subimg[jj].subset, global.orbits[i].subimg[0].tot_subset*sizeof(SubSetInfo_t));
	      }

	    
	    for(ii=0; ii<global.orbits[i].subimg[0].tot_subset; ii++)
	      {
		global.orbits[i].subimg[jj].subset[ii].start_time=global.orbits[i].subimg[0].subset[ii].start_time;
		global.orbits[i].subimg[jj].subset[ii].bias=global.orbits[i].subimg[0].subset[ii].bias;
		global.orbits[i].subimg[jj].subset[ii].pix2bias=global.orbits[i].subimg[0].subset[ii].pix2bias;
		global.orbits[i].subimg[jj].subset[ii].pix4bias=global.orbits[i].subimg[0].subset[ii].pix4bias;
		global.orbits[i].subimg[jj].subset[ii].pix7bias=global.orbits[i].subimg[0].subset[ii].pix7bias;
		global.orbits[i].subimg[jj].subset[ii].pix9bias=global.orbits[i].subimg[0].subset[ii].pix9bias;
		global.orbits[i].subimg[jj].subset[ii].nevents=global.orbits[i].subimg[0].subset[ii].nevents;
	      }
	    
	    continue;
	  }


	/*  Check if at least 1 events found */
	if(!biasmatrix_dim[jj])
	  {
	    if(!jj&&total_img)
	      headas_chat(NORMAL, "%s: Warning: No events found in Total Image\n", global.taskname);
	    else
	      headas_chat(NORMAL, "%s: Warning: No events found in SubImg %d\n", global.taskname, jj+1);

	    global.orbits[i].subimg[jj].tot_subset=1;
	    global.orbits[i].subimg[jj].subset = (SubSetInfo_t *)malloc(sizeof(SubSetInfo_t));
	    
	    global.orbits[i].subimg[jj].subset[0].start_time=global.orbits[i].time;
	    global.orbits[i].subimg[jj].subset[0].bias=0;
	    global.orbits[i].subimg[jj].subset[0].pix2bias=0;
	    global.orbits[i].subimg[jj].subset[0].pix4bias=0;
	    global.orbits[i].subimg[jj].subset[0].pix7bias=0;
	    global.orbits[i].subimg[jj].subset[0].pix9bias=0;
	    global.orbits[i].subimg[jj].subset[0].nevents=0;
	    continue;
	  }

	
	/*  Compute bias for each SubSet in SubImage */
	while(first_subset || subsetnum<subsetmatrix_dim[jj]+1)
	  {
	    
	    /*  Check if last-1 subset */
	    if(subsetmatrix_dim[jj]&&subsetnum==subsetmatrix_dim[jj]-1)
	      {
		if(((biasmatrix_dim[jj]-subsetmatrix[jj][subsetnum])/4)<global.par.nevents)
		  {
		    subset_end=biasmatrix_dim[jj]-1 > 0 ? biasmatrix_dim[jj]-1 : 0;
		    merge_lastsubset=1;
		  }
		else
		  subset_end=subsetmatrix[jj][subsetnum];
	      }
	    else
	      {
		if(subsetnum==subsetmatrix_dim[jj])
		  subset_end=biasmatrix_dim[jj]-1 > 0 ? biasmatrix_dim[jj]-1 : 0;
		else
		  subset_end=subsetmatrix[jj][subsetnum];
	      }

	    /*  headas_chat(CHATTY, "Subset init %d\n", subset_init); */
	    /*  headas_chat(CHATTY, "Subset end %d\n", subset_end); */
	    
	    if(!stricmp(global.par.biasmethod, BIAS_MEAN))
	      {
		pix2_bias=0;
		pix4_bias=0;
		pix7_bias=0;
		pix9_bias=0;

		headas_chat(CHATTY, "%s: Info: Subset %d:\n", global.taskname, subsetnum+1);

		/*  pix2 mean */
		headas_chat(CHATTY, "%s: Info: pix2 bias array: ", global.taskname);

		for(ii=subset_init; ii<=subset_end; ii=ii+4)
		  {     
		    pix2_bias+=biasmatrix[jj][ii];
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix2_bias= (int)floor(pix2_bias/((subset_end-subset_init+1)/4)+0.5);
		global.orbits[i].subimg[jj].subset[subsetnum].pix2bias=pix2_bias;
		headas_chat(CHATTY, "%s: Info: pix2 mean: %d\n", global.taskname, pix2_bias);

		/*  pix4 mean */
		headas_chat(CHATTY, "%s: Info: pix4 bias array: ", global.taskname);

		for(ii=subset_init+1; ii<=subset_end; ii=ii+4)
		  {     
		    pix4_bias+=biasmatrix[jj][ii];
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix4_bias= (int)floor(pix4_bias/((subset_end-subset_init+1)/4)+0.5);
		global.orbits[i].subimg[jj].subset[subsetnum].pix4bias=pix4_bias;
		headas_chat(CHATTY, "%s: Info: pix4 mean: %d\n", global.taskname, pix4_bias);

		/*  pix7 mean */
		headas_chat(CHATTY, "%s: Info: pix7 bias array: ", global.taskname);

		for(ii=subset_init+2; ii<=subset_end; ii=ii+4)
		  {     
		    pix7_bias+=biasmatrix[jj][ii];
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix7_bias= (int)floor(pix7_bias/((subset_end-subset_init+1)/4)+0.5);
		global.orbits[i].subimg[jj].subset[subsetnum].pix7bias=pix7_bias;
		headas_chat(CHATTY, "%s: Info: pix7 mean: %d\n", global.taskname, pix7_bias);

		/*  pix9 mean */
		headas_chat(CHATTY, "%s: Info: pix9 bias array: ", global.taskname);

		for(ii=subset_init+3; ii<=subset_end; ii=ii+4)
		  {     
		    pix9_bias+=biasmatrix[jj][ii];
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix9_bias= (int)floor(pix9_bias/((subset_end-subset_init+1)/4)+0.5);
		global.orbits[i].subimg[jj].subset[subsetnum].pix9bias=pix9_bias;
		headas_chat(CHATTY, "%s: Info: pix9 mean: %d\n", global.taskname, pix9_bias);

		/*  mean of pix2 pix4 pix7 pix9 */
		bias_median=(int)floor((pix2_bias+pix4_bias+pix7_bias+pix9_bias)/4 + 0.5);
		headas_chat(CHATTY, "%s: Info: Mean %d \n", global.taskname, bias_median);
		
		global.orbits[i].subimg[jj].subset[subsetnum].bias=bias_median;
		global.orbits[i].subimg[jj].subset[subsetnum].nevents=(int)((subset_end-subset_init+1)/4);
		
	      }
	    else if(!stricmp(global.par.biasmethod,BIAS_MEDIAN))
	      {
		headas_chat(CHATTY, "%s: Info: Subset %d:\n", global.taskname, subsetnum+1);

		/*  pix2 median */
		bias=malloc(floor((subset_end-subset_init+1)/4)*sizeof(int));
		
		headas_chat(CHATTY, "%s: Info: pix2 bias array: ", global.taskname);
		
		bias_index=0;
		for(ii=subset_init; ii<=subset_end; ii=ii+4)
		  {
		    bias[bias_index]=biasmatrix[jj][ii];
		    bias_index++;
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix2_bias=CalcMedian(bias, floor((subset_end-subset_init+1)/4));
		global.orbits[i].subimg[jj].subset[subsetnum].pix2bias=pix2_bias;
		headas_chat(CHATTY, "%s: Info: pix2 median: %d\n", global.taskname, pix2_bias);

		free(bias);


		/*  pix4 median */
		bias=malloc(floor((subset_end-subset_init+1)/4)*sizeof(int));
		
		headas_chat(CHATTY, "%s: Info: pix4 bias array: ", global.taskname);
		
		bias_index=0;
		for(ii=subset_init+1; ii<=subset_end; ii=ii+4)
		  {
		    bias[bias_index]=biasmatrix[jj][ii];
		    bias_index++;
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix4_bias=CalcMedian(bias, floor((subset_end-subset_init+1)/4));
		global.orbits[i].subimg[jj].subset[subsetnum].pix4bias=pix4_bias;
		headas_chat(CHATTY, "%s: Info: pix4 median: %d\n", global.taskname, pix4_bias);

		free(bias);

		/*  pix7 median */
		bias=malloc(floor((subset_end-subset_init+1)/4)*sizeof(int));
		
		headas_chat(CHATTY, "%s: Info: pix7 bias array: ", global.taskname);
		
		bias_index=0;
		for(ii=subset_init+2; ii<=subset_end; ii=ii+4)
		  {
		    bias[bias_index]=biasmatrix[jj][ii];
		    bias_index++;
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix7_bias=CalcMedian(bias, floor((subset_end-subset_init+1)/4));
		global.orbits[i].subimg[jj].subset[subsetnum].pix7bias=pix7_bias;
		headas_chat(CHATTY, "%s: Info: pix7 median: %d\n", global.taskname, pix7_bias);

		free(bias);

		/*  pix9 median */
		bias=malloc(floor((subset_end-subset_init+1)/4)*sizeof(int));
		
		headas_chat(CHATTY, "%s: Info: pix9 bias array: ", global.taskname);
		
		bias_index=0;
		for(ii=subset_init+3; ii<=subset_end; ii=ii+4)
		  {
		    bias[bias_index]=biasmatrix[jj][ii];
		    bias_index++;
		    headas_chat(CHATTY, "%d ",biasmatrix[jj][ii]);
		  }
		headas_chat(CHATTY, "\n");
		
		pix9_bias=CalcMedian(bias, floor((subset_end-subset_init+1)/4));
		global.orbits[i].subimg[jj].subset[subsetnum].pix9bias=pix9_bias;
		headas_chat(CHATTY, "%s: Info: pix9 median: %d\n", global.taskname, pix9_bias);

		free(bias);

		/*  mean of pix2 pix4 pix7 pix9 */
		bias_median=(int)floor((pix2_bias+pix4_bias+pix7_bias+pix9_bias)/4 + 0.5);
		headas_chat(CHATTY, "%s: Info: Mean %d \n", global.taskname, bias_median);
		
		global.orbits[i].subimg[jj].subset[subsetnum].bias=bias_median;
		global.orbits[i].subimg[jj].subset[subsetnum].nevents=(int)((subset_end-subset_init+1)/4);
		
	      }


	    if(merge_lastsubset)
	      {
		global.orbits[i].subimg[jj].tot_subset--;
		
		subsetnum++;
	      }
	    
	    subsetnum++;
	    subset_init=subset_end+1;
	    first_subset=0;
	  }
	
      }

    free(subsetmatrix);
    free(subsetmatrix_dim);

    free(biasmatrix);
    free(biasmatrix_dim);

    free(subimgccdframe);
  }


  for(i=0; i<global.tot_orbits; i++){
    
    headas_chat(CHATTY, "%s: Info: Orbit Number %d start time %18.8f\n", global.taskname, i+1, global.orbits[i].time);

    for(kk=0; kk<global.orbits[i].tot_subimg; kk++)
      {
	headas_chat(CHATTY, "%s: Info: SubImage Number %d (xmin %d xmax %d ymin %d ymax %d)\n", global.taskname, kk+1,
		    global.orbits[i].subimg[kk].x_min,global.orbits[i].subimg[kk].x_max,
		    global.orbits[i].subimg[kk].y_min,global.orbits[i].subimg[kk].y_max);

	for(ii=0; ii<global.orbits[i].subimg[kk].tot_subset; ii++)
	  {
	    headas_chat(CHATTY, "%s: Info: SubImage %d Subset Number %d start time %18.8f bias difference %d nevents %d\n",
			global.taskname, kk+1, ii+1,
			global.orbits[i].subimg[kk].subset[ii].start_time,
			global.orbits[i].subimg[kk].subset[ii].bias,
			global.orbits[i].subimg[kk].subset[ii].nevents);
	  }
      }
  }

 
  ReleaseBintable(&evhead, &evtable);
  return OK;

 ComputeBiasMedian_end:

  if (evhead.first)
    ReleaseBintable(&evhead, &evtable);

  return NOT_OK;
} /*  ComputeBiasMedian */


/*
 *
 *  UpdatePCCorrExt
 *
 *  DESCRIPTION:
 *    Routine to Create/Update BIASDIFF Ext in Evt File
 *
 *
 */

int UpdatePCCorrExt(FitsFileUnit_t outunit)
{
  
  int   status=OK, tot_hdu, hdupccorr=0, tfields=13;
  long  nrows, newrows;
  int   i, ii, kk, orbitnum, subimgnum;

  char  *ttype[] = { "TIME", "ORBIT", "SUBIMG", "DETXMIN", "DETXMAX", "DETYMIN", "DETYMAX", "BIASDIFF",
		     "BIASDIFF2", "BIASDIFF4", "BIASDIFF7", "BIASDIFF9", "NEVENTS"};
  char  *tform[] = { "1D","1I","1I","1I","1I","1I","1I","1J","1J","1J","1J","1J","1J"};    /* datatype,        */
  char  *tunit[] = { "s","","","pixel","pixel","pixel","pixel","chan","chan","chan","chan","chan",""};  /* physical units   */
  int   timecol, orbitcol, subimgcol, xmincol, xmaxcol, ymincol, ymaxcol, biasdiffcol, pix2col, pix4col, pix7col, pix9col, neventscol;

  if(fits_get_num_hdus(outunit, &tot_hdu, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get the total number of hdus\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;
    }

  
  /* Check if BIASDIFF extension exists */
  if(fits_movnam_hdu(outunit, ANY_HDU,"BIASDIFF" , 0, &status))
    {
      if(status == BAD_HDU_NUM)
	{
	  headas_chat(CHATTY,"%s: Info: Unable to find '%s' extension in\n", global.taskname,"BIASDIFF");
	  headas_chat(CHATTY,"%s: Info: '%s' file.\n",global.taskname, global.par.outfile);
	  headas_chat(CHATTY,"%s: Info: The task will create and fill it.\n",global.taskname);

	  status=0;

	  if(fits_movabs_hdu( outunit, tot_hdu, NULL, &status ))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,tot_hdu);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	      goto UpdatePCCorrExt_end;
	    }
	  
	  
	  if ( fits_create_tbl( outunit, BINARY_TBL, 0, tfields, ttype, tform, tunit, "BIASDIFF", &status) )  
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to create %s extension\n", global.taskname,"BIASDIFF" );
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.outfile);
	      goto UpdatePCCorrExt_end;
	    }

	  
	  if(fits_movnam_hdu(outunit, ANY_HDU,"BIASDIFF" , 0, &status))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension in\n", global.taskname,"BIASDIFF");
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.outfile);
	      goto UpdatePCCorrExt_end;
	    }

	  if (!fits_get_hdu_num(outunit, &hdupccorr))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,"BIASDIFF");
	      headas_chat(NORMAL,"%s: : '%s' file.\n",global.taskname, global.par.outfile);
	      goto UpdatePCCorrExt_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read '%s' extension in\n", global.taskname,"BIASDIFF");
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.outfile);
	  goto UpdatePCCorrExt_end;
	}
    }
  else
    {
      /* Get BIASDIFF ext number */
      if (!fits_get_hdu_num(outunit, &hdupccorr))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,"BIASDIFF");
	  headas_chat(NORMAL,"%s: : '%s' file.\n",global.taskname, global.par.outfile);
	  goto UpdatePCCorrExt_end;
	}
      
      headas_chat(CHATTY,"%s: Info: '%s' extension exist in\n", global.taskname,"BIASDIFF");
      headas_chat(CHATTY,"%s: Info: '%s' file.\n",global.taskname, global.par.outfile);
      headas_chat(CHATTY,"%s: Info: The task will overwrite it.\n",global.taskname);
    }

  

  if(fits_movabs_hdu( outunit, hdupccorr, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,hdupccorr);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;
    }


  /*  Clear 'BIASDIFF' Ext */
  if(fits_get_num_rows(outunit, &nrows, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get total number of rows\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in the '%s' file.\n ", global.taskname, global.par.outfile);
      
      goto UpdatePCCorrExt_end;
    }

  if(fits_delete_rows(outunit, 1, nrows, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to clear rows of BIASDIFF ext\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in the '%s' file.\n ", global.taskname, global.par.outfile);
      
      goto UpdatePCCorrExt_end;
    }

  
  /*  Get TIME column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "TIME", &timecol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "TIME");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get TIME column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "ORBIT", &orbitcol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "ORBIT");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get SUBIMG column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "SUBIMG", &subimgcol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "SUBIMG");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get DETXMIN column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "DETXMIN", &xmincol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "DETXMIN");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  /*  Get DETXMAX column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "DETXMAX", &xmaxcol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "DETXMAX");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get DETYMIN column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "DETYMIN", &ymincol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "DETYMIN");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get DETYMAX column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "DETYMAX", &ymaxcol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "DETYMAX");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  
  /*  Get BIASDIFF column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "BIASDIFF", &biasdiffcol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "BIASDIFF");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  /*  Get BIASDIFF2 column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "BIASDIFF2", &pix2col, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "BIASDIFF2");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  /*  Get BIASDIFF4 column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "BIASDIFF4", &pix4col, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "BIASDIFF4");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }
  /*  Get BIASDIFF7 column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "BIASDIFF7", &pix7col, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "BIASDIFF7");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  /*  Get BIASDIFF9 column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "BIASDIFF9", &pix9col, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "BIASDIFF9");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  /*  Get NEVENTS column number  */
  if ( fits_get_colnum(outunit, CASEINSEN, "NEVENTS", &neventscol, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, "NEVENTS");
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto UpdatePCCorrExt_end;    
    }

  
  for(i=0; i<global.tot_orbits; i++){
    
    orbitnum=i+1;

    for(kk=0; kk<global.orbits[i].tot_subimg; kk++)
      {
	subimgnum=kk+1;

	newrows=global.orbits[i].subimg[kk].tot_subset;
	
	if(fits_get_num_rows(outunit, &nrows, &status))
	  {
	    headas_chat(NORMAL,"%s: Error: Unable to get total number of rows\n", global.taskname);
	    headas_chat(NORMAL,"%s: Error: in the '%s' file.\n ", global.taskname, global.par.outfile);
	    
	    goto UpdatePCCorrExt_end;
	  }


	if(fits_insert_rows(outunit, nrows, newrows, &status))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to add rows in %s file\n", global.taskname, global.par.outfile);
	    goto UpdatePCCorrExt_end;
	  }


	for(ii=0; ii<global.orbits[i].subimg[kk].tot_subset; ii++)
	  {
   

	    if(fits_write_col(outunit, TDOUBLE, timecol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].start_time, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "TIME");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, orbitcol, nrows+ii+1, 1, 1, &orbitnum, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "ORBIT");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, subimgcol, nrows+ii+1, 1, 1, &subimgnum, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "SUBIMG");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, xmincol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].x_min, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "DETXMIN");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, xmaxcol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].x_max, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "DETXMAX");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, ymincol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].y_min, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "DETYMIN");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, ymaxcol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].y_max, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "DETYMAX");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }
	    
	    if(fits_write_col(outunit, TINT, biasdiffcol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].bias, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "BIASDIFF");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, pix2col, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].pix2bias, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "BIASDIFF2");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, pix4col, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].pix4bias, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "BIASDIFF4");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, pix7col, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].pix7bias, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "BIASDIFF7");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, pix9col, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].pix9bias, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "BIASDIFF9");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	    if(fits_write_col(outunit, TINT, neventscol, nrows+ii+1, 1, 1, &global.orbits[i].subimg[kk].subset[ii].nevents, &status))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to fill values in %s column\n", global.taskname, "NEVENTS");
		headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
		goto UpdatePCCorrExt_end;
	      }

	  }


	if(global.orbits[i].total_img || global.orbits[i].central_img)
	  {
	    /*  Write only Central or Total Image values */
	    kk=global.orbits[i].tot_subimg;
	  }
      }
  }


  /* Add Swift XRT Keyword  */
  if(AddSwiftXRTKeyword(outunit, hdupccorr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add Swift XRT keywords\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in %d HDU of %s file.\n", hdupccorr, global.par.outfile);
      goto UpdatePCCorrExt_end;
    }


  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, hdupccorr, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in %d HDU of %s file.\n", hdupccorr, global.par.outfile);
      goto UpdatePCCorrExt_end;
    }

  
  return OK;
  
    UpdatePCCorrExt_end:
  
  return NOT_OK;

} /*  UpdatePCCorrExt  */



int AddSwiftXRTKeyword(FitsFileUnit_t inunit, int hdunum)
{
  int    status=OK, hdu_type=0;
  char   taskname[FLEN_FILENAME];
  char   date[25];

  /* Task name */
  get_toolnamev(taskname);

  /* Get creation date */
  GetGMTDateTime(date);


  if(fits_movabs_hdu(inunit, hdunum, &hdu_type, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to move in the %d hdu\n", global.taskname, hdunum);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
      
  if(fits_update_key(inunit, TSTRING, KWNM_DATE, date, CARD_COMM_DATE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_CREATOR, taskname, CARD_COMM_CREATOR, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CREATOR);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

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

  if(fits_update_key(inunit, TSTRING, KWNM_DATAMODE,  global.evt.datamode,CARD_COMM_DATAMODE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID,  global.evt.obsid,CARD_COMM_OBS_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TINT,KWNM_TARG_ID , &global.evt.target,CARD_COMM_TARG_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TARG_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_ORIGIN,  global.evt.origin, CARD_COMM_ORIGIN, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_ORIGIN);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TIMESYS, global.evt.timesys, CARD_COMM_TIMESYS, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMESYS);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
      
  if(fits_update_key(inunit, TDOUBLE, KWNM_MJDREFI, &global.evt.mjdrefi,CARD_COMM_MJDREFI, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE,KWNM_MJDREFF , &global.evt.mjdreff,CARD_COMM_MJDREFF, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TIMEREF,  global.evt.timeref, CARD_COMM_TIMEREF, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMEREF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TASSIGN,  global.evt.tassign, CARD_COMM_TASSIGN, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TASSIGN);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TIMEUNIT,  global.evt.timeunit, CARD_COMM_TIMEUNIT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMEUNIT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.evt.tstart,CARD_COMM_TSTART, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.evt.tstop,CARD_COMM_TSTOP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TSTRING, KWNM_DATEOBS,  global.evt.dateobs,CARD_COMM_DATEOBS, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
  if(fits_update_key(inunit, TSTRING, KWNM_DATEEND,  global.evt.dateend,CARD_COMM_DATEEND, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }  
  
   
  
  return OK;
  
 add_end:
  return NOT_OK;
  
}/*AddSwiftXRTKeyword*/



/*
 *
 *  CalcMedian
 *
 *  DESCRIPTION:
 *    Routine to Calculate the median value for array of int
 *
 *
 */
int CalcMedian(int * bias, int length) 
{
  int level, mod=0, jj, iii;
  int * median;
  double medianval,diff_db;

  if(length<=0)
    return 0;

  median=(int *)malloc(length*sizeof(int));

  for (jj=0; jj< length; jj++)
    {
      level=bias[jj];
      
      if(jj==0)
	median[jj]=level;
      else 
	if(level >= median[jj-1])
	  median[jj]=level;
	else
	  {
	    for(iii=jj; iii >= 0 ; iii--)
	      {
		if(!iii)
		  median[iii]=level;
		else 
		  if (level < median[iii-1])
		    {
		      median[iii]=median[iii-1];
		    }
		  else
		    {
		      median[iii]=level;
		      iii=0;
		    }
	      }
	  }
    }


  headas_chat(CHATTY, "%s: Info: Median array ", global.taskname);
  for (jj=0; jj< length; jj++)
    {
      headas_chat(CHATTY, "%d ", (int)median[jj]);
    }
  headas_chat(CHATTY, "\n");


  mod = length%2;
  if (mod)
    {
      medianval = (double) (median[(int)(length/2.0)] );
    }
  else
    {
      int ind;
      ind=(int)(length/2.0);
      if(!ind)
	diff_db=(double)median[ind];
      else
	diff_db=((median[ind] + median[ind - 1])/2.);
      medianval=diff_db;
    }
  
  free(median);
  return (int)floor(medianval + 0.5);

} /* CalcMedian */



int ReadBPExt(FitsFileUnit_t evunit,int hducount, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS])
{
  int            x, y, n, status=OK, amp=0;
  short int      flag_tmp=0;
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
  

  /* Read blocks of bintable rows from input badpixels file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  
  while (ReadBintable(evunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{

	  flag_tmp=XVEC2BYTE(table,n,badcolumns.BADFLAG);

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
	  
	      if(amp == AMP1)
		{

		  if (AddBadPix(x,y,pix,bp_table_amp1))
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
			  if (AddBadPix(x,y,pix,bp_table_amp1))
			    {
			      headas_chat(NORMAL, "%s: Error: Unable to  write pixels in bad pixels map.\n",global.taskname);
			      goto readbp_end;
			    }
			}
		    }
		}
	      else{
		
		if (AddBadPix(x,y,pix,bp_table_amp2))
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
			  if (AddBadPix(x,y,pix,bp_table_amp2))
			    {
			      headas_chat(NORMAL, "%s: Error: Unable to  write pixels in bad pixels map.\n",global.taskname);
			      goto readbp_end;
			    }
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
