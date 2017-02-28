/*
 * 
 *	xrtphascorr.c:
 *
 *	INVOCATION:
 *
 *		xrtphascorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *                Routine to correct PHAS column configuration
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 30/04/2009 - First version
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtphascorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtphascorr.h"


/********************************/
/*         definitions          */
/********************************/

#define XRTPHASCORR_C
#define XRTPHASCORR_VERSION      "0.1.0"
#define PRG_NAME               "xrtphascorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtphascorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtphascorr.par    
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
 *        0.1.0: - NS 30/04/2009 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */

int xrtphascorr_getpar()
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

  /* Input PhasConf File Name */
  if(PILGetFname(PAR_PHASCONFFILE, global.par.phasconffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHASCONFFILE);
      goto Error;	
    }
   
  get_history(&global.hist);
  xrtphascorr_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* xrtphascorr_getpar */
/*
 *	xrtphascorr_work
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
 *        0.1.0: - NS 30/04/2009 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtphascorr_work()
{
  int            status = OK, inExt, outExt, evExt, biasdiffExt, badpixExt, jj;   
  int            logical, phasconfig[9], stdphasconfig=0, phasocolnum;
  long           extno;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL;              /* Input and Output fits file pointer */ 


  if(xrtphascorr_checkinput())
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
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,inunit,global.par.infile,global.taskname);
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    
  

  /* Retrieve DATAMODE from input event file */
  if(!ExistsKeyWord(&head, KWNM_DATAMODE, &card))
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

  /* Retrieve TSTART from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    global.tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve XSTDPHAS from input event file */
  if(ExistsKeyWord(&head, KWNM_XSTDPHAS, &card)){
    if(card->u.LVal){
      headas_chat(NORMAL, "%s: Warning: PHAS configuration is already corrected on ground,\n", global.taskname);
      headas_chat(NORMAL, "%s: Warning: nothing to be done.\n", global.taskname);
      global.warning=1;
      goto ok_end;
    }
  }

  /* Retrieve XPHASCO from input event file */
  if(!(ExistsKeyWord(&head, KWNM_XPHASCO, NULLNULLCARD)) || !(GetLVal(&head, KWNM_XPHASCO)))
    {
      global.xphasco=FALSE;    
    }
  else
    {
      global.xphasco=TRUE;
    }


  /* Query CALDB to get phasconf filename? */
  if (!(strcasecmp (global.par.phasconffile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_PHASCONF_DSET, global.par.phasconffile, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for phasconf file.\n", global.taskname);
	  goto Error;
	}
      else
	headas_chat(CHATTY, "%s: Info: Processing '%s'  CALDB file.\n", global.taskname, global.par.phasconffile);
    }

  /* Get PHAS configuration */
  if( GetPhasConf(global.par.phasconffile, global.tstart, phasconfig, &stdphasconfig) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to get PHAS configuration from %s file.\n", global.taskname,global.par.phasconffile);
      goto Error;
    }

  if( VerifyPhasConf(phasconfig) )
    {
      headas_chat(NORMAL, "%s: Error: bad PHAS configuration\n", global.taskname);
      headas_chat(CHATTY, "%s: Info: PHAS configuration:\n", global.taskname);
      for(jj=0; jj<9; jj++){
	headas_chat(CHATTY, " %d", phasconfig[jj]);
      }
      headas_chat(CHATTY, "\n");
      goto Error;
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: PHAS configuration:", global.taskname);
      for(jj=0; jj<9; jj++){
	headas_chat(CHATTY, " %d", phasconfig[jj]);
      }
      headas_chat(CHATTY, "\n");
    }
  
  global.correctphas=TRUE;
  if(stdphasconfig){
    headas_chat(NORMAL,"%s: Info: PHAS column in standard configuration\n",global.taskname);
    headas_chat(NORMAL,"%s: Info: correction not applied\n",global.taskname, global.par.infile);  
    global.correctphas=FALSE;
  }

 
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
      goto Error;
    }


  /* Get 'BIASDIFF' ext number */
  if (!fits_movnam_hdu(inunit, ANY_HDU,"BIASDIFF", 0, &status))
    {
      fits_get_hdu_num(inunit, &biasdiffExt);
    }
  else
    {
      biasdiffExt=-1 ;
    }
  status = OK;
  

  /* Get BADPIX ext number */
  if (!fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_BAD, 0, &status))
    {
      fits_get_hdu_num(inunit, &badpixExt);
    }
  else
    {
      badpixExt=-1 ;
    }
  status = OK;


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

      /* Skip BIASDIFF and BADPIX ext copy if PHAS configuration correction needed */
      if((outExt!=biasdiffExt && outExt!=badpixExt) || !global.correctphas)
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
  

  /* Correct PHA if necessary */
  if(global.correctphas)
    {
      if (PHASCorrect(inunit, outunit, phasconfig))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to correct PHAS configuration.\n", global.taskname);
	  goto Error;
	}
      if(global.warning)
	goto ok_end;


      /* Set XPHASCO keyword to false */
      logical=FALSE;
      if(fits_update_key(outunit, TLOGICAL, KWNM_XPHASCO, &logical, CARD_COMM_XPHASCO, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XPHASCO);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}

      /* Delete PHASO column */
      if(!fits_get_colnum(outunit, CASEINSEN, CLNM_PHASO, &phasocolnum, &status))
	{
	  if(fits_delete_col(outunit, phasocolnum, &status))
	    {
	      headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_PHASO);
	      goto Error;
	    }
	}
	status = OK;

    }
  else
    {
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
    }


  /* Set XSTDPHAS keyword to true */
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XSTDPHAS, &logical, CARD_COMM_XSTDPHAS, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XSTDPHAS);
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

      /* Skip BIASDIFF and BADPIX ext copy if PHAS configuration correction needed */
      if((outExt!=biasdiffExt && outExt!=badpixExt) || !global.correctphas)
	if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	  {
	    headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	    headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.infile);
	    headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	    goto Error;
	  }
      
      outExt++;
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
} /* xrtphascorr_work */


/*
 *	xrtphascorr
 *
 *
 *	DESCRIPTION:
 *              Tool to correct PHAS column values configuration
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
 *             void xrtphascorr_getpar(void);
 * 	       void xrtphascorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - NS 30/04/2009 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtphascorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTPHASCORR_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( xrtphascorr_getpar() == OK) 
    {
      
      if ( xrtphascorr_work()) 
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
  
} /* xrtphascorr */
/*
 *	xrtphascorr_info:
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
 *        0.1.0: - NS 30/04/2009 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtphascorr_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the input PhasConf file                       :'%s'\n",global.par.phasconffile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtphascorr_info */
/* 
 * 
 * xrtphascorr_checkinput 
 *
 */
int xrtphascorr_checkinput(void)
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

	      if(!(strcasecmp(global.par.outfile,global.par.infile)))
		{
		  headas_chat(NORMAL, "%s: Error: outfile name must be different from input file\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: to overwrite input file please set outfile to 'NONE'\n", global.taskname);
		  goto check_end;
		}
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
 
  return OK;

 check_end:
  return NOT_OK;
}


/*
 *
 *      PHASCorrect
 *
 *	DESCRIPTION:
 *             Routine to correct events PHAS configuration
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - 30/04/2009 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PHASCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit, int phasconf[9])
{
  int                jj, phastmp[9];
  BOOL               phasocol;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0, tlmphas_fromcol;
  char               comm[256]="";
  Ev2Col_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  head=RetrieveFitsHeader(evunit);
  
  headas_chat(NORMAL, "%s: Info: correcting PHAS column configuration\n", global.taskname);

  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  
  nCols=outtable.nColumns;
  
  /* Get cols index from name */
  
  /* PHAS */
  if((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  /* PHASO */
  if((indxcol.PHASO=ColNameMatch(CLNM_PHASO, &outtable)) == -1){

    if(global.xphasco){
      headas_chat(NORMAL, "%s: Error: %s keyword set to TRUE\n", global.taskname, KWNM_XPHASCO);
      headas_chat(NORMAL, "%s: Error: but %s column does not exist\n",  global.taskname, CLNM_PHASO);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }
 
    tlmphas_fromcol=indxcol.PHAS;
    phasocol=FALSE;
  }
  else{
    tlmphas_fromcol=indxcol.PHASO;
    phasocol=TRUE;
  }

  /* TLMPHAS */
  if ((indxcol.TLMPHAS=ColNameMatch(CLNM_TLMPHAS, &outtable)) == -1)
    {
      headas_chat(NORMAL,"%s: Info: new column %s will be added to output file %s\n", global.taskname, CLNM_TLMPHAS,global.par.outfile);
      AddColumn(&head, &outtable,CLNM_TLMPHAS, CARD_COMM_TLMPHAS, "9I",TUNIT|TMIN|TMAX, UNIT_CHANN, CARD_COMM_PHYSUNIT, PHA_MIN, PHA_MAX);
      sprintf(comm, "Added %s column", CLNM_TLMPHAS);
      indxcol.TLMPHAS=ColNameMatch(CLNM_TLMPHAS, &outtable);
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: existing column %s will be overwritten\n", global.taskname, CLNM_TLMPHAS);
    }

  
  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: corrected PHAS configuration.", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist); 
      AddHistory(&head, comm);
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;

  /* Check on PHAS datatype */
  if (outtable.Multiplicity[indxcol.PHAS] != 9)
    {
      headas_chat(CHATTY,"%s: Error: The multiplicity of the %s column is: %d\n", global.taskname, outtable.Multiplicity[indxcol.PHAS]); 
      headas_chat(CHATTY,"%s: Error: but should be: 9 \n", global.taskname); 
      goto reco_end;
    }

  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{

	  /* Copy telemetred PHAS values in TLMPHAS column and create a temporary array with PHAS values*/
	  for (jj=0; jj< 9; jj++)
	    {
	      IVECVEC(outtable,n,indxcol.TLMPHAS,jj) = IVECVEC(outtable,n,tlmphas_fromcol,jj);
	      phastmp[jj] = IVECVEC(outtable,n,tlmphas_fromcol,jj);
	    }

	  /* Apply PHAS and PHASO correction */
	  for (jj=0; jj< 9; jj++)
	    {
	      IVECVEC(outtable,n,indxcol.PHAS,jj) = phastmp[ phasconf[jj]-1 ];
	      if(phasocol)
		IVECVEC(outtable,n,indxcol.PHASO,jj) = phastmp[ phasconf[jj]-1 ];
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
  
} /* PHASCorrect */


int VerifyPhasConf(int phasconf[9])
{
  int i=0;
  int tmparray[9];

  for(i=0; i<9; i++){
    tmparray[i]=-1;
  }

  for(i=0; i<9; i++){
    if(phasconf[i]>0 && phasconf[i]<=9){
      tmparray[ phasconf[i]-1 ] = 1;
    }
  }

  for(i=0; i<9; i++){
    if(tmparray[i]==-1){
      return NOT_OK;
    }
  }

  return OK;
}


