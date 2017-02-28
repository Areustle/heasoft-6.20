/*
 * 
 *	xrtlccorr.c:
 *
 *	INVOCATION:
 *
 *		xrtlccorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *           
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 31/01/2006 - First version
 *        0.1.1 -    06/02/2006 - Added psfflag input parameter
 *                                for apply or not PSF correction
 *                              - Added check on input files
 *        0.1.2 - BS 01/03/2006 - Implemented ERROR column correction
 *        0.1.3 -    03/03/2006 - Modified light curve tstart and added check
 *                                on TIMEDEL keyword value
 *        0.1.4 - BS 07/03/2006 - Modified to handle new XRTNFRAM keyword 
 *                                datatype
 *        0.1.5 -    08/03/2006 - Bug fixed reading XRTNFRAM keyword
 *        0.1.6 -    09/03/2006 - Defined default name for output files
 *        0.1.7 -    10/03/2006 - Added check on instrument map end light 
 *                                curve DATAMODE
 *        0.1.8 -    10/03/2006 - If srcx and srcy input parameter set to -1
 *                                get the center of the first region instead
 *                                of calculate it  
 *        0.1.9 -    16/03/2006 - Added XRTPSFEN and XRTPSFCO keywords
 *                              - Deleted region map from the fracfile
 *        0.2.0 - NS 12/04/2007 - Region shape BOX and -BOX with ROTANG>0 supported
 *                              - Windowed Timing mode supported 
 *                              - Added 'regionfile' input parameter
 *        0.2.1 -    27/04/2007 - Modified rate correction factor computation
 *        0.2.2 -    23/05/2007 - Modified image rotation algorithm for WT mode
 *                              - Bug fixed in wmap construction for BOX shape
 *        0.2.3 -    25/05/2007 - Modified wmap coordinates range computation
 *                                for BOX shape
 *        0.2.4 -    14/06/2007 - Fixed bug on OSX v7.9.0 and Intel PC (segmentation violation)
 *        0.2.5 -    13/03/2007 - Changed 'instrfile' input parameter from sky instrument map
 *                                to raw instrument map of variable dimension
 *        0.2.6 -    19/03/2007 - Create raw instrument map centered in input region
 *        0.2.7 -    02/04/2007 - Apply vignetting correction when creating raw instrument map
 *                              - Modified to handle 'DEFAULT' as value in 'outfile', 'corrfile'
 *                                and 'outinstrfile' input parameters for standard name
 *        0.2.8 -    06/10/2008 - Removed nframe input parameter and added wtnframe and pcnframe parameters
 *        0.2.9 -    12/12/2008 - Added XRA_PNT and XDEC_PNT keywords in instrument map header
 *                              - Get ranom and decnom values from XRA_PNT and XDEC_PNT instead of RA_PNT and DEC_PNT
 *        0.3.0 -    15/10/2009 - Modified to optimize the execution time
 *        0.3.1 -    26/04/2010 - Added masking of pixels on window border while creating instrument map
 *                              - Create instrument map also for WT events in SETTLING mode
 *        0.3.2 -    15/03/2011 - Modified selection of good frames while reading hkfile
 *        0.3.3 -    29/03/2011 - Handle new psf CALDB file format
 *        0.3.4 -    04/05/2011 - Increased tolerance in hk time check
 *        0.3.5 -    20/07/2011 - Added datamode selection in PSF CALDB file query
 *        0.3.6 -    16/01/2014 - Added check of the 'ATTFLAG' keyword of the input attitude file
 *        0.3.7 -    17/02/2014 - Modified check of the 'ATTFLAG' keyword of the input attitude file
 *                              - Handle different type of the ATTFLAG keyword
 *        0.3.8 -    18/06/2014 - Modified default naming convention of output instrument map file
 *
 *
 *
 *      NOTE:
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtlccorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtlccorr.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTLCCORR_C
#define XRTLCCORR_VERSION       "0.3.8"
#define PRG_NAME               "xrtlccorr"

/********************************/
/*           globals            */
/********************************/

#define STR_LEN 125

Global_t global;

/*
 *	xrtlccorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtlccorr.par    
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
 *           void xrtlccorr_info(void);
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
int xrtlccorr_getpar(void)
{
  /*
   *  Get File name Parameters
   */
  

  /* Input Region File Name */
  if(PILGetFname(PAR_REGFILE, global.par.regfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_REGFILE);
      goto Error;	
    }
    
  /* Input LC File Name */
  if(PILGetFname(PAR_LCFILE, global.par.lcfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_LCFILE);
      goto Error;
    }
  
  /* Input OUT File Name */
  if(strcasecmp(global.par.lcfile, "NONE"))
    {
      if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
	  goto Error;	
	}
    }

  /* Output File Name */
  if(PILGetFname(PAR_CORRFILE, global.par.corrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CORRFILE);
      goto Error;	
    }

  if(PILGetBool(PAR_PSFFLAG, &global.par.psfflag)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFLAG);
      goto Error;	
    }
  if(global.par.psfflag)
    {
      if(PILGetFname(PAR_PSFFILE, global.par.psffile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFILE);
	  goto Error;	
	}
    }

  if(PILGetReal(PAR_ENERGY, &global.par.energy)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ENERGY);
      goto Error;	
    }

  if(PILGetReal(PAR_SRCX, &global.par.srcx)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCX);
      goto Error;	
    }


  if(PILGetReal(PAR_SRCY, &global.par.srcy)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCY);
      goto Error;	
    }

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

  if(PILGetBool(PAR_CREATEINSTRMAP, &global.par.createinstrmap))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_CREATEINSTRMAP);
      goto Error;
    }
  
  if(global.par.createinstrmap)
    {
      /* Output INSTR File Name */
      if(PILGetFname(PAR_OUTINSTRFILE, global.par.outinstrfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTINSTRFILE);
	  goto Error;	
	}

      /* NB To be updated in 'xrtlccorr_checkinput()' function if outinstrfile=DEFAULT */
      strcpy(global.instrmap, global.par.outinstrfile);
      

      /* Input PC File Name */
      if(PILGetFname(PAR_INFILE, global.par.infile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
	  goto Error;
	}
      
      /* Input HD File Name */
      if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	  goto Error;	
	}
      
      /* Input FOV File Name */
      if(PILGetFname(PAR_FOVFILE, global.par.fovfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_FOVFILE);
	  goto Error;	
	}

      /* Input Vignetting File Name */
      if(PILGetFname(PAR_VIGFILE, global.par.vigfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGFILE);
	  goto Error;
	}
      
      if(PILGetInt(PAR_WTNFRAME, &global.par.wtnframe)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_WTNFRAME);
	  goto Error;	
	}
      
      if(PILGetInt(PAR_PCNFRAME, &global.par.pcnframe)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PCNFRAME);
	  goto Error;	
	}
    }
  else
    {
      /* Input INSTR File Name */
      if(PILGetFname(PAR_INSTRFILE, global.par.instrfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INSTRFILE);
	  goto Error;	
	}

      strcpy(global.instrmap, global.par.instrfile);
    }


  get_history(&global.hist);
  xrtlccorr_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtlccorr_getpar */

/*
 *	xrtlccorr_work
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
int xrtlccorr_work(void)
{
  FitsFileUnit_t    lcunit=NULL;
  int                   amp=0;
  static BadPixel_t     *bp_table[CCD_PIXS][CCD_ROWS];
  TMEMSET0( bp_table, bp_table);


  if(xrtlccorr_checkinput())
    {
      goto Error;
    }

  /* Check ATTFLAG keyword value of the attitude file */

  if (GetAttitudeATTFLAG(global.par.attfile, global.attflag)==NOT_OK)
    {
      headas_chat(NORMAL,"%s: Error: Unable to get %s in\n", global.taskname, KWNM_ATTFLAG);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.attfile);
      goto Error;
    }
  
  if (! strcmp(global.attflag, "111") || !strcmp(global.attflag, "101") )
    {
      headas_chat(NORMAL,"%s: Error: input attitude file sw*uat.fits not suitable for XRT data processing,\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: use the sw*sat.fits or sw*pat.fits one.\n", global.taskname);
      goto Error;
    }


  if(!strcasecmp(global.par.regfile, "NONE"))
    {
      headas_chat(NORMAL,"%s: Info: regionfile set to NONE, the region will be read from lcfile\n", global.taskname);

      /* Open read only input event file */
      if ((lcunit=OpenReadFitsFile(global.par.lcfile)) <= (FitsFileUnit_t )0)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.lcfile);
	  goto Error;
	}
      
      
      /* Check whether it is a Swift XRT File */
      ISXRTFILE_WARNING(NORMAL, lcunit, global.par.lcfile,global.taskname);
      
      headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.lcfile);
      
      if(ReadLcFile(lcunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.lcfile);
	  goto Error;
	}
    }
  else
    {
      if(ReadRegionFile())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.regfile);
	  goto Error;
	}
    }

  /* Create Raw Instrument Map */
  if (global.par.createinstrmap)
    {

      if(ReadEventFile(bp_table, &amp))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(global.warning)
	goto ok_end;

      if(ReadHK())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}

      if(GetCalFOVInfo())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.fovfile);
	  goto Error;
	}

      if(GetVignettingInfo())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read value\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: from %s file.\n", global.taskname, global.par.vigfile);
	  goto Error;
	}

      if(CreateInstrMap(bp_table, amp))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.outinstrfile);
	  goto Error;
	  
	}
      headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.outinstrfile);

    } /* END Create Raw Instrument Map */


  if(CreateFracTable())
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.corrfile);
      goto Error;
    }

  if(!strcasecmp(global.par.regfile, "NONE"))
    {
      if(CorrectLCCurve(lcunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to correct\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.lcfile);
	  goto Error;
	}
    }

  if(global.warning)
    goto ok_end;


  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

 ok_end:

  return OK;

 Error:

  return NOT_OK;
} /* xrtlccorr_work */

/*
 *	xrtlccorr
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
 *             void xrtlccorr_getpar(void);
 *             void xrtlccorr_info(void); 
 * 	       void xrtlccorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtlccorr(void)
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTLCCORR_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  global.warning=0;
  /* Get parameter values */ 
  if ( xrtlccorr_getpar() == OK ) {
    
    if ( xrtlccorr_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto instrmap_end;
    }
    else if(global.warning)
      {
	headas_chat(MUTE,"---------------------------------------------------------------------\n");
	headas_chat(MUTE, "%s: Exit with Warning.\n", global.taskname);
	headas_chat(MUTE,"---------------------------------------------------------------------\n");
	
      }
    
    
  }
  else
    goto instrmap_end;
  
  return OK;
  
 instrmap_end:

  
  return NOT_OK;


} /* xrtlccorr */
/*
 *	xrtlccorr_info: 
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
void xrtlccorr_info (void)
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");

  headas_chat(NORMAL,"Name of the input Light Curve file       :'%s'\n",global.par.lcfile); 
  headas_chat(NORMAL,"Name of the input region file            :'%s'\n",global.par.regfile);
  if(strcasecmp(global.par.lcfile, "NONE"))
    headas_chat(NORMAL,"Name of the output Light Curve file      :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the output file                  :'%s'\n",global.par.corrfile);

  headas_chat(NORMAL,"Name of the input TELDEF file            :'%s'\n",global.par.teldef);
  headas_chat(NORMAL,"Name of the the input attitude file      :'%s'\n",global.par.attfile);
  if (global.par.aberration)
    headas_chat(NORMAL,"Should aberration be included in aspecting? : yes\n");
  else
    headas_chat(NORMAL,"Should aberration be included in aspecting? : no\n");
  if (global.par.attinterpol)
    headas_chat(NORMAL,"Interpolate Attitude values?             : yes\n");
  else
    headas_chat(NORMAL,"Interpolate Attitude values?             : no\n");

  if(global.par.psfflag)
    {
      headas_chat(NORMAL,"Correct for PSF:                         : yes\n");
      headas_chat(NORMAL,"Name of the PSF CALDB file               :'%s'\n",global.par.psffile);
    }
  else
    headas_chat(NORMAL,"Correct for PSF:                         : no\n");
  headas_chat(NORMAL,"Source X coordinate                      : %f \n",global.par.srcx);
  headas_chat(NORMAL,"Source Y coordinate                      : %f \n",global.par.srcy);
  headas_chat(NORMAL,"Energy value for PSF and Vignetting correction : %f \n",global.par.energy);

  if (global.par.createinstrmap)
    {
      headas_chat(NORMAL,"Name of the output Instrument Map file   :'%s'\n",global.par.outinstrfile);
      headas_chat(NORMAL,"Name of the input Event file             :'%s'\n",global.par.infile); 
      headas_chat(NORMAL,"Name of the input FOV region file        :'%s'\n", global.par.fovfile);
      headas_chat(NORMAL,"Name of the input HK Header file         :'%s'\n",global.par.hdfile);
    }
  else
    {
      headas_chat(NORMAL,"Name of the input Instrument Map file    :'%s'\n",global.par.instrfile);
    }

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                   : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                   : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtlccorr_info */

int xrtlccorr_checkinput(void)
{
  char BaseName[MAXFNAME_LEN];


  /*  one of regionfile or lcfile must be set to NONE */
  if(!strcasecmp(global.par.regfile, "NONE"))
    {
      if(!strcasecmp(global.par.lcfile, "NONE"))
	{
	  headas_chat(NORMAL, "%s: Error: one of 'regionfile' or 'lcfile' parameter\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: must be set.\n", global.taskname);
	  goto error_check;
	}
    }
  else
    {
      if( strcasecmp(global.par.lcfile, "NONE"))
	{
	  headas_chat(NORMAL, "%s: Error: one of 'regionfile' or 'lcfile' parameter\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: must be set to NONE.\n", global.taskname);
	  goto error_check;
	}
    }


  if(strcasecmp(global.par.lcfile, "NONE"))
    {

      /* If global.par.outfile == DEFAULT build filename */ 
      if (!(strcasecmp (global.par.outfile, DF_DEFAULT)))
	{

	  SplitFilePath(global.par.lcfile, NULL, BaseName);
	  strcpy(global.par.outfile, BaseName);

	  StripExtension(global.par.outfile);
	  strcat(global.par.outfile, "_corr.lc");
      
	  headas_chat(NORMAL, "%s: Info: Name for the corrected light curve file is:\n",global.taskname);
	  headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outfile);
	}
  

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

  /* If global.par.corrfile == DEFAULT build filename */ 
  if (!(strcasecmp (global.par.corrfile, DF_DEFAULT)))
    {
      if(strcasecmp(global.par.lcfile, "NONE"))
	{
	  SplitFilePath(global.par.lcfile, NULL, BaseName);
	  strcpy(global.par.corrfile, BaseName);

	  StripExtension(global.par.corrfile);
	  strcat(global.par.corrfile, "_corrfact.fits");
	}
      else
	{
	  SplitFilePath(global.par.regfile, NULL, BaseName);
	  strcpy(global.par.corrfile, BaseName);

	  StripExtension(global.par.corrfile);
	  strcat(global.par.corrfile, "_corrfact.fits");
	}

      headas_chat(NORMAL, "%s: Info: Name for the output file is:\n",global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.corrfile);
    }

  if ( FileExists(global.par.corrfile) ) {
    headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.corrfile);
    if ( !headas_clobpar ) {
      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.corrfile);
      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
      goto error_check;
    }
    else
      {
	headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.corrfile);
	if(remove (global.par.corrfile) == -1)
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.corrfile);
	    goto error_check;
	  }  
      }
  }


  if (global.par.createinstrmap) {

    /* If global.par.outinstrfile == DEFAULT build filename */ 
    if (!(strcasecmp (global.par.outinstrfile, DF_DEFAULT)))
      {

	if(strcasecmp(global.par.lcfile, "NONE"))
	  {
	    SplitFilePath(global.par.lcfile, NULL, BaseName);
	    strcpy(global.par.outinstrfile, BaseName);
	    
	    if( ! StripExtensionWithStem(global.par.outinstrfile,"posr.") )
	      StripExtension(global.par.outinstrfile);
	    else
	      strcat(global.par.outinstrfile, "po");

	    strcat(global.par.outinstrfile, "_srawinstr.img");
	  }
	else
	  {
	    SplitFilePath(global.par.regfile, NULL, BaseName);
	    strcpy(global.par.outinstrfile, BaseName);
	    
	    StripExtension(global.par.outinstrfile);
	    strcat(global.par.outinstrfile, "_srawinstr.img");
	  }

	/* Update name of intrument map to use */
	strcpy(global.instrmap, global.par.outinstrfile);

	headas_chat(NORMAL, "%s: Info: Name for the output instrument file is:\n",global.taskname);
	headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outinstrfile);
      }


    if(FileExists(global.par.outinstrfile)){
      
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outinstrfile);
      if ( !headas_clobpar ) {
	headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outinstrfile);
	headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	goto error_check;
      }
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outinstrfile);
	  if(remove (global.par.outinstrfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
	      goto error_check;
	    }
	}
    }

  }

  
  return OK;

 error_check:
  return NOT_OK;


}/* xrtlccorr_checkinput */

/* ReadLCFile */
int ReadLcFile(FitsFileUnit_t lcunit)
{
  int                   status=OK; 
  char                  shape[17], shape0[17];
  double                rotang;
  LCCol_t               lccol;
  unsigned              FromRow, ReadRows, n, nCols;
  Bintable_t	        table;
  FitsCard_t            *card;
  FitsHeader_t          lchead;            /* Extension header pointer */

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &lchead, FitsHeader_t );

  /* Move to REGION or REG00101 extension */

  if (fits_movnam_hdu(lcunit, ANY_HDU,"REGION", 0, &status))
    {
      if (fits_movnam_hdu(lcunit, ANY_HDU,"REG00101", 0, &status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find REGION extension in\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.lcfile); 
	  /* Close input event file */
	  if (CloseFitsFile(lcunit))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.lcfile);  
	    }
	  goto end_func;
	}
    }

  /* Get extension header pointer  */
  lchead=RetrieveFitsHeader(lcunit);  

  if(!(ExistsKeyWord(&lchead, KWNM_DATAMODE , &card)) || (strcmp(card->u.SVal, KWVL_DATAMODE_PH)&&strcmp(card->u.SVal, KWVL_DATAMODE_TM)))
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, card->u.SVal);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s , %s.\n", global.taskname, KWVL_DATAMODE_PH, KWVL_DATAMODE_TM);
      if (CloseFitsFile(lcunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.lcfile);
	}
      goto end_func;
    }

  GetBintableStructure(&lchead, &table, BINTAB_ROWS, 0, NULL);

  nCols=table.nColumns;  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.lcfile);
      goto end_func;
    }

  /* Get needed columns number from name */
  
  if ((lccol.X=ColNameMatch(CLNM_X, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }
  
  if ((lccol.Y=ColNameMatch(CLNM_Y, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }
  
  if ((lccol.SHAPE=ColNameMatch(CLNM_SHAPE, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SHAPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }
  
  if ((lccol.R=ColNameMatch(CLNM_R, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_R);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }
  
  if ((lccol.ROTANG=ColNameMatch(CLNM_ROTANG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ROTANG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }
  
  if ((lccol.COMPONENT=ColNameMatch(CLNM_COMPONENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COMPONENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto end_func;
    }

  EndBintableHeader(&lchead, &table);
 
  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  while(ReadBintable(lcunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  strcpy(shape, SVEC(table, n, lccol.SHAPE)); 
	  
	  headas_chat(CHATTY,"%s: Info: shape '%s'\n", global.taskname, shape); 

	  if(n==0)
	    {
	      strcpy(shape0, shape);

	      global.region.X[0]= DVEC(table, n, lccol.X) ;
	      global.region.Y[0]= DVEC(table, n, lccol.Y) ;


	      if(!strcmp(shape, "CIRCLE")) 
		{
		  strcpy(global.region.shape, "CIRCLE");
		  
		  if(table.Multiplicity[lccol.R]!=1)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[lccol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto end_func;
		    }
	  
		  global.region.R[0]=DVEC(table, n, lccol.R);
		  	  
		}
	      else if(!strcmp(shape, "ANNULUS"))
		{
		  strcpy(global.region.shape, "ANNULUS");

		  if(table.Multiplicity[lccol.R]!= 2)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname, table.Multiplicity[lccol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto end_func;
		    }
		  global.region.R[0]  = DVECVEC(table,n,lccol.R,0);
		  global.region.R1[0] = DVECVEC(table,n,lccol.R,1);
		  	  
		}
	      else if(!strcmp(shape, "BOX"))
		{
		  strcpy(global.region.shape, "BOX");

		  rotang=DVEC(table,n,lccol.ROTANG);

		  global.region.rotang[0]=rotang;

		  if(table.Multiplicity[lccol.R]!= 2)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[lccol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto end_func;
		    }
		  
		  global.region.R1[0]  = DVECVEC(table,n,lccol.R,0);
		  global.region.R[0] = DVECVEC(table,n,lccol.R,1);
	  
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Error: shape not yet implemented\n", global.taskname);
		  goto end_func;
		}
	    }
	  else if(n==1 && !strcmp(shape, "!CIRCLE") && !strcmp(shape0, "CIRCLE"))
	    {
	      strcpy(global.region.shape, "CIRCLE-CIRCLE");

	      global.region.X[1]= DVEC(table, n, lccol.X) ;
	      global.region.Y[1]= DVEC(table, n, lccol.Y) ;

	      if(table.Multiplicity[lccol.R]!=1)
		{
		  headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[lccol.R] );
		  headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		  goto end_func;
		}
	      
	      global.region.R[1]=DVEC(table, n, lccol.R);
		  	    
	    }
	  else if(n==1 && !strcmp(shape, "!BOX") && !strcmp(shape0, "BOX"))
	    {
	      strcpy(global.region.shape, "BOX-BOX");

	      global.region.X[1]= DVEC(table, n, lccol.X) ;
	      global.region.Y[1]= DVEC(table, n, lccol.Y) ;

	      rotang=DVEC(table,n,lccol.ROTANG);
	      
	      global.region.rotang[1]=rotang;
	      
	      if(table.Multiplicity[lccol.R]!= 2)
		{
		  headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[lccol.R] );
		  headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		  goto end_func;
		}
	      
	      global.region.R1[1]  = DVECVEC(table,n,lccol.R,0);
	      global.region.R[1] = DVECVEC(table,n,lccol.R,1);
	      
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: This region shape is not yet implemented\n", global.taskname);
	      goto end_func;
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  
  ReleaseBintable(&lchead, &table);
  
  return OK; 
  
 end_func:
  return NOT_OK;



}/* ReadLcFile */


int ReadRegionFile()
{
  int         sresult; 
  double      x, y, R, R1, rotang;
  unsigned    line_num=0;
  FILE        *ptrregfile;   /* Pointer to the region file */
  char        buffer[STR_LEN];     /* buffer for fgets() */
  char        *tmp1, *shape=NULL, *shape0=NULL;
  char        sshape[30];


  if (FileExists(global.par.regfile)) 
    {
      if ( (ptrregfile = fopen(global.par.regfile,"r")) ) 
	{
	  while (!feof(ptrregfile) && !ferror(ptrregfile)) 
	    {
	      if(fgets( buffer, STR_LEN, ptrregfile))
		{
		  line_num++;

		  /*  headas_chat(NORMAL,"%s: Info: line %d '%s'\n", global.taskname, line_num, buffer); */

		  tmp1=strchr(buffer, '(');
		  if(!tmp1)
		    {
		      headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
		      goto end_readreg;
		    }

		  shape=(char *)malloc(sizeof(char)*(strlen(buffer)-strlen(tmp1)));
		  strncpy(shape, buffer, strlen(buffer)-strlen(tmp1));
		  shape[strlen(buffer)-strlen(tmp1)]='\0';

		  headas_chat(CHATTY,"%s: Info: shape '%s'\n", global.taskname, shape); 
		  
		  if(line_num==1)
		    {
		      shape0=(char *)malloc(sizeof(char)*strlen(shape));
		      strcpy(shape0, shape);

		      if(!stricmp(shape, "CIRCLE")) 
			{
			  strcpy(global.region.shape, "CIRCLE");
			  
			  sresult = sscanf(buffer, "%[^'('](%lf,%lf,%lf)",sshape, &x, &y, &R);
			  
			  /*  headas_chat(NORMAL,"line  %s %f %f %f\n", sshape, x, y, R); */

			  if(sresult!=4)
			    {
			      headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
			      goto end_readreg;
			    }

			  global.region.X[0]=x;
			  global.region.Y[0]=y;
			  global.region.R[0]=R;
  
			}
		      else if(!stricmp(shape, "ANNULUS"))
			{
			  strcpy(global.region.shape, "ANNULUS");

			  sresult = sscanf(buffer, "%[^'('](%lf,%lf,%lf,%lf)",sshape, &x, &y, &R, &R1);
			  
			  /*  headas_chat(NORMAL,"line  %s %f %f %f %f\n", sshape, x, y, R, R1); */

			  if(sresult!=5)
			    {
			      headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
			      goto end_readreg;
			    }

			  global.region.X[0]=x;
			  global.region.Y[0]=y;
			  global.region.R[0]=R;
			  global.region.R1[0]=R1;

			}
		      else if(!stricmp(shape, "BOX"))
			{
			  strcpy(global.region.shape, "BOX");

			  sresult = sscanf(buffer, "%[^'('](%lf,%lf,%lf,%lf,%lf)",sshape, &x, &y, &R1, &R, &rotang);

			  /*  headas_chat(NORMAL,"line  %s %f %f %f %f %f\n", sshape, x, y, R, R1, rotang); */

			  if(sresult!=6)
			    {
			      headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
			      goto end_readreg;
			    }
			  
			  global.region.X[0]=x;
			  global.region.Y[0]=y;
			  global.region.R[0]=R;
			  global.region.R1[0]=R1;
			  global.region.rotang[0]=rotang;
		  
			}
		      else
			{
			  headas_chat(NORMAL, "%s: Error: shape not yet implemented\n", global.taskname);
			  goto end_readreg;
			}		      
		    }
		  else if(line_num==2 && !stricmp(shape, "-CIRCLE") && !stricmp(shape0, "CIRCLE"))
		    {
		      strcpy(global.region.shape, "CIRCLE-CIRCLE");     
		      
		      sresult = sscanf(buffer, "%[^'('](%lf,%lf,%lf)",sshape, &x, &y, &R);
		      
		      /*  headas_chat(NORMAL,"line  %s %f %f %f\n", sshape, x, y, R); */
		      
		      if(sresult!=4)
			{
			  headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
			  goto end_readreg;
			}
		      
		      global.region.X[1]=x;
		      global.region.Y[1]=y;
		      global.region.R[1]=R;

		    }
		  else if(line_num==2 && !stricmp(shape, "-BOX") && !stricmp(shape0, "BOX"))
		    {
		      strcpy(global.region.shape, "BOX-BOX");

		      sresult = sscanf(buffer, "%[^'('](%lf,%lf,%lf,%lf,%lf)",sshape, &x, &y, &R1, &R, &rotang);

		      /*  headas_chat(NORMAL,"line  %s %f %f %f %f %f\n", sshape, x, y, R, R1, rotang); */

		      if(sresult!=6)
			{
			  headas_chat(NORMAL,"%s: Error: Unknown region file format\n", global.taskname);
			  goto end_readreg;
			}

		      global.region.X[1]=x;
		      global.region.Y[1]=y;
		      global.region.R[1]=R;
		      global.region.R1[1]=R1;
		      global.region.rotang[1]=rotang;
		    }
		  else
		    {
		      headas_chat(NORMAL, "%s: Error: This region shape is not yet implemented\n", global.taskname);
		      goto end_readreg;
		    }

		  free(shape);

		} /*  end if(fgets( buffer, STR_LEN, ptrregfile)) */
	    }

	  if(shape0)
	    free(shape0);
	}
      else
	{
	  headas_chat(NORMAL,"%s: Error: unable to open regionfile '%s'\n", global.taskname, global.par.regfile);
	  goto  end_readreg;
	}
    }
  else
    {
      headas_chat(NORMAL,"%s: Error: regionfile '%s' not found\n", global.taskname, global.par.regfile);
      goto  end_readreg;
    }

  return OK; 
   
 end_readreg:
  return NOT_OK;

}

/*CreateFracTable*/
int CreateFracTable(void)
{
  
  int                tot_hdu=0, status = OK, hdu_type=0, tmp_hdu=0;
  int                anynull=0, xx=0, yy=0, maxd_int=0;
  int                tfields=2, i, limx1=0, limx2=0, limy1=0, limy2=0, int_r;
  long               dimen;
  long               firstrow, firstelem;

  char               *ttype[] = { CLNM_TIME, CLNM_FRACEXPO}; /*define the name,  */
  char               *tform[] = { "1D","1D"};                /* datatype,        */
  char               *tunit[] = { "",""};                    /* physical units   */
  float              tot_pix=0.0, xrtnfram=0.0, sens=0.00001;

  float              *img;
  static float       data[CCD_ROWS][CCD_PIXS];

  static int         wmap[CCD_ROWS][CCD_PIXS];
  float              cexp[CCD_PIXS];

  float              X_min, X_max, Y_min, Y_max;
  int                first_wmapx, first_wmapy, last_wmapx, last_wmapy;
  int                offsetx=0,offsety=0;
  double             img_tstart, img_tstop, img_mjdref, mean_time;
  double             img_crpix1, img_crpix2;
  int                img_naxis1, img_naxis2, img_crval1, img_crval2;
 
  double             expo_pix=0.0, tmp_tstart, tmp_tstop, d[4], maxd=-1.0;
  double             skycx=0.0, skycy=0.0;
  double             cx=0, cy=0, reg_cx,reg_cy, reg2_cx,reg2_cy;
  double             *circ=NULL, *wei=NULL, dbl_r, off, pcorr;
  long               extno;
  FitsCard_t         *card;
  FitsHeader_t       head;  
  FitsFileUnit_t     inunit=NULL, outunit=NULL;

  TELDEF*    teldef;
  ATTFILE*   attfile;
 

  global.frac_index=0;


  headas_chat(NORMAL, "%s: Info: Creating rate correction file.\n", global.taskname);

  /* Open read only input event file */
  if ((inunit=OpenReadFitsFile(global.instrmap)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.instrmap);
      goto create_end;
    }
    
  head=RetrieveFitsHeader(inunit);
  
  if((ExistsKeyWord(&head, KWNM_DATAMODE  , &card)))
    {
      strcpy(global.instr_key.datamode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if(strcmp(global.instr_key.datamode, KWVL_DATAMODE_PH)&&strcmp(global.instr_key.datamode, KWVL_DATAMODE_TM))
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.instr_key.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s , %s.\n", global.taskname, KWVL_DATAMODE_PH, KWVL_DATAMODE_TM);
      if (CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.instrmap);
	}
      goto create_end;
    }


  if((ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.instr_key.obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if((ExistsKeyWord(&head, KWNM_OBS_ID  , &card)))
    {
      strcpy(global.instr_key.obsid, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if(!ExistsKeyWord(&head, KWNM_VIGNAPP, &card))
    {
      headas_chat(CHATTY, "%s: Info: %s keyword not found.\n", global.taskname, KWNM_VIGNAPP);
      global.instr_key.vignapp=FALSE;
      
    }
  else if(card->u.LVal && global.par.psfflag)
    {
      headas_chat(NORMAL,"%s: Info: The instrument map is corrected for vignetting\n", global.taskname);
      global.instr_key.vignapp=TRUE;
      
      if(ExistsKeyWord(&head, KWNM_XRTVIGEN, &card))
	{
	  global.par.energy=card->u.DVal;
	  headas_chat(NORMAL, "%s: Info: set %s input parameter to %f.\n", global.taskname, PAR_ENERGY, global.par.energy);
	    
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found but %s set to TRUE\n", global.taskname, KWNM_XRTVIGEN, KWNM_VIGNAPP);
	}
     
    }
  else 
    global.instr_key.vignapp=FALSE;


  if((ExistsKeyWord(&head, KWNM_MJDREFI  , &card)))
    {
      global.instr_key.mjdrefi=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }
  
  if((ExistsKeyWord(&head, KWNM_MJDREFF  , &card)))
    {
      global.instr_key.mjdreff=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if((ExistsKeyWord(&head, KWNM_TARG_ID  , &card)))
    {
      global.instr_key.target=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TARG_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if((ExistsKeyWord(&head, KWNM_SEG_NUM  , &card)))
    {
      global.instr_key.segnum=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_SEG_NUM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }
  
  if((ExistsKeyWord(&head, KWNM_TSTART  , &card)))
    {
      global.instr_key.tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }
  if((ExistsKeyWord(&head, KWNM_TSTOP  , &card)))
    {
      global.instr_key.tstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }


  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.instr_key.dateobs=card->u.SVal;
      if(!(strchr(global.instr_key.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.instr_key.timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;      
    }


  if((ExistsKeyWord(&head, KWNM_DATEEND  , &card)))
    {
      strcpy(global.instr_key.dateend, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  if((ExistsKeyWord(&head, KWNM_PA_PNT, &card)))
    {
      global.instr_key.pa_pnt= card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_PA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }

  /* Get nominal RA */

  if((ExistsKeyWord(&head, KWNM_XRA_PNT  , &card)))
    global.instr_key.ranom=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XRA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
       goto create_end;
    }
    
  /* Get nominal Dec */
  
  if((ExistsKeyWord(&head, KWNM_XDEC_PNT  , &card)))
    global.instr_key.decnom=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_XDEC_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
      goto create_end;
    }


  /* Open Attitude file */
  attfile = openAttFile(global.par.attfile);
  
  if(global.par.attinterpol)
    setAttFileInterpolation(attfile, 1);
  else
    setAttFileInterpolation(attfile, 0);


  /* Query CALDB to get teldef filename? */
  if (!(strcasecmp (global.par.teldef, DF_CALDB)))
    {
       extno=-1;

      if (CalGetFileName(HD_MAXRET, global.instr_key.dateobs, global.instr_key.timeobs, DF_NOW, DF_NOW, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for teldef file.\n", global.taskname);
	  goto create_end;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s'  CALDB file.\n", global.taskname, global.par.teldef);
    }
  
  /* Open TELDEF file */
  teldef = readTelDef(global.par.teldef);
  
  setSkyCoordCenterInTeldef(teldef, global.instr_key.ranom, global.instr_key.decnom);



  if(fits_get_num_hdus(inunit, &tot_hdu, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get the total number of hdus\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.instrmap);
      goto create_end;
    }


  for (tmp_hdu=0; tmp_hdu < tot_hdu; tmp_hdu++)
    {
      if(fits_movabs_hdu(inunit, (tmp_hdu+1), &hdu_type, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to move in the %d hdu\n", global.taskname, (tmp_hdu+1));
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.instrmap);
	  goto create_end;
	}
      
      head=RetrieveFitsHeader(inunit);  
      
      if(hdu_type == IMAGE_HDU && (ExistsKeyWord(&head, KWNM_NAXIS1, NULLNULLCARD) || ExistsKeyWord(&head, KWNM_NAXIS2, NULLNULLCARD)))
	{
	  
	  if((ExistsKeyWord(&head, KWNM_TSTART  , &card)))
	    {
	      img_tstart = card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }
	  
	  if((ExistsKeyWord(&head, KWNM_TSTOP  , &card)))
	    {
	      img_tstop = card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }
	  
	  img_mjdref = HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);
	  
	  if(status)
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_CRVAL1, &card)))
	    {
	      img_crval1 = card->u.JVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_CRVAL1);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_CRVAL2, &card)))
	    {
	      img_crval2 = card->u.JVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_CRVAL2);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_CRPIX1, &card)))
	    {
	      img_crpix1 = card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_CRPIX1);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_CRPIX2, &card)))
	    {
	      img_crpix2 = card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_CRPIX2);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_NAXIS1, &card)))
	    {
	      img_naxis1 = card->u.JVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_NAXIS1);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if(img_naxis1>CCD_PIXS)
	    {
	      headas_chat(NORMAL, "%s: Error: keyword %s= %i > %i not allowed\n", global.taskname,KWNM_NAXIS1,img_naxis1,(int)CCD_PIXS);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_NAXIS2, &card)))
	    {
	      img_naxis2 = card->u.JVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_NAXIS2);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }

	  if(img_naxis2>CCD_ROWS)
	    {
	      headas_chat(NORMAL, "%s: Error: keyword %s = %i > %i not allowed\n", global.taskname,KWNM_NAXIS2,img_naxis2,(int)CCD_ROWS);
	      headas_chat(NORMAL, "%s: Error: in %s file (ext %i).\n", global.taskname, global.instrmap, tmp_hdu);
	      goto create_end;
	    }


	  dimen =  img_naxis1 * img_naxis2;

	  img = malloc(dimen * sizeof(float));

	  if(fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, img, &anynull, &status))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to read image in %d hdu\n", global.taskname,(tmp_hdu+1));
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }

/*  	  printf("Image %i\n",(tmp_hdu+1)); */
/*  	  if((tmp_hdu+1)==2) */
/*  	    { */
/*  	      printf("Prova Image\n"); */
/*  	      for(yy=0; yy < img_naxis2; yy++) */
/*  		{ */
/*  		  for(xx=0; xx < img_naxis1; xx++) */
/*  		    {      */
/*  		      printf("%f ",img[(yy*img_naxis1)+xx]); */
/*  		    } */
/*  		  printf("\n\n"); */
/*  		} */
/*  	      printf(" Prova Image\n"); */
/*  	    } */

	  /*  Compute offset in standard 600x600 map  */

	  offsetx= (img_crval1+1) - img_crpix1 ;
	  offsety= (img_crval2+1) - img_crpix2 ;
	  /* headas_chat(CHATTY,"%s: Info: Image Offset in 600x600 map (%i,%i)\n", global.taskname, offsetx, offsety); */

	  if(offsetx<0 || offsety<0)
	    {
	      headas_chat(NORMAL,"%s: Error: bad values of CRPIXx and CRVALx keywords\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }

	  /*  Copy rawmap in standard 600x600 map  */

	  for(yy=0; yy < CCD_ROWS; yy++)
	    {
	      if( yy<offsety || yy>=(img_naxis2+offsety) )
		{
		  data[yy][xx]=0;
		}
	      else
		{
		  for(xx=0; xx < CCD_PIXS; xx++)
		    {
		      if( xx<offsetx || xx>=(img_naxis1+offsetx) )
			{
			  data[yy][xx]=0;
			}
		      else
			{
			  data[yy][xx]=img[((yy-offsety)*img_naxis1)+(xx-offsetx)];
			}
		    }
		}
	    }

/*  	  if((tmp_hdu+1)==2) */
/*  	    { */
/*  	      printf("INIT Prova Image 600x600\n"); */
/*  	      for(yy=offsety-1; yy <img_naxis2+offsety+1; yy++) */
/*  		{ */
/*  		  for(xx=offsetx-2; xx <img_naxis1+offsetx+2 ; xx++) */
/*  		    { */
/*  		      printf("%f ",data[yy][xx]); */
/*  		    } */
/*  		  printf("\n\n"); */
/*  		} */
/*  	      printf("END Prova Image 600x600\n"); */
/*  	    } */

	 /* Transform cx and cy to DET coordinates */
	  
	  skycx=global.par.srcx;
	  skycy=global.par.srcy;

	  if(skycx <= 0.) 
	    skycx=global.region.X[0];
	  
	  if(skycy <= 0.) 
	    skycy=global.region.Y[0];
	  

	  mean_time = (img_tstart+img_tstop)/2;
	  

	  if (ComputeRawCoord(skycx, skycy, mean_time, img_mjdref, teldef, attfile, &cx, &cy))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to transform cx=%f and cy=%f to raw coordinates\n", global.taskname,skycx,skycy);
	      goto create_end;
	    }
	  headas_chat(CHATTY, "%s: Info: Source position in RAW coordinates: [%.4f , %.4f] \n",global.taskname,cx,cy);

	  
	  if(!finite(cx) || !finite(cy) || cx > RAWX_MAX || cx < RAWX_MIN || cy > RAWY_MAX || cy < RAWY_MIN )
	    {  
	      headas_chat(CHATTY, "%s: Error: source X=%f Y=%f is out of the CCD for time %18.8f\n", 
			  global.taskname, skycx, skycy, mean_time);
	      goto create_end;
	    }
	  

	  /* Compute wmap[] */


	  for(yy=0; yy<CCD_ROWS; yy++)
	    {
	      for(xx=0; xx<CCD_PIXS; xx++)
		{
		  wmap[yy][xx]=-1;
		}
	    }

	  first_wmapx=CCD_PIXS+1;
	  first_wmapy=CCD_ROWS+1;
	  last_wmapx=-1;
	  last_wmapy=-1;  


	  if(!strcmp(global.region.shape, "CIRCLE"))
	    {
      
	      if (ComputeRawCoord(global.region.X[0], global.region.Y[0], mean_time, img_mjdref, teldef, attfile, &reg_cx, &reg_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region CIRCLE center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg_cx,reg_cy);
	      
	      if(!finite(reg_cx) || !finite(reg_cy) || reg_cx > RAWX_MAX || reg_cx < RAWX_MIN || reg_cy > RAWY_MAX || reg_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[0], global.region.Y[0], mean_time);
		  goto create_end;
		}
	      

	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if((xx-reg_cx)*(xx-reg_cx)+(yy-reg_cy)*(yy-reg_cy) <= global.region.R[0]*global.region.R[0])
			{
			  wmap[yy][xx]=1;
			}
		    }
		}

	    }
	  else if (!strcmp(global.region.shape, "ANNULUS"))
	    {

	      if (ComputeRawCoord(global.region.X[0], global.region.Y[0], mean_time, img_mjdref, teldef, attfile, &reg_cx, &reg_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region ANNULUS center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg_cx,reg_cy);
	      
	      if(!finite(reg_cx) || !finite(reg_cy) || reg_cx > RAWX_MAX || reg_cx < RAWX_MIN || reg_cy > RAWY_MAX || reg_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[0], global.region.Y[0], mean_time);
		  goto create_end;
		}
	      

	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if((xx-reg_cx)*(xx-reg_cx)+(yy-reg_cy)*(yy-reg_cy) <= global.region.R1[0]*global.region.R1[0])
			{
			  wmap[yy][xx]=1;
			}

		      if((xx-reg_cx)*(xx-reg_cx)+(yy-reg_cy)*(yy-reg_cy) <= global.region.R[0]*global.region.R[0])
			{
			  wmap[yy][xx]=-1;
			}
		    }
		}
	    }
  	  else if (!strcmp(global.region.shape, "BOX"))
	    {

	      if( global.region.rotang[0] < (global.instr_key.pa_pnt -10 ) || 
		  global.region.rotang[0] > (global.instr_key.pa_pnt +10 ) )
		{
		  headas_chat(NORMAL, "%s: Error: Bad 'ROTANG' value in input region\n", global.taskname );
		  headas_chat(NORMAL, "%s: Error: (region 'ROTANG' value must be equal to ROLL).\n", global.taskname );
		  goto create_end;
		}
	      

	      if (ComputeRawCoord(global.region.X[0], global.region.Y[0], mean_time, img_mjdref, teldef, attfile, &reg_cx, &reg_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region BOX center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg_cx,reg_cy);
	      
	      if(!finite(reg_cx) || !finite(reg_cy) || reg_cx > RAWX_MAX || reg_cx < RAWX_MIN || reg_cy > RAWY_MAX || reg_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[0], global.region.Y[0], mean_time);
		  goto create_end;
		}
	      
	      
	      X_min = reg_cx - global.region.R[0]/2;
	      X_max = reg_cx + global.region.R[0]/2;

	      Y_min = reg_cy - global.region.R1[0]/2;
	      Y_max = reg_cy + global.region.R1[0]/2;
	      

	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    { 
		      if((xx >= X_min) && (xx <= X_max) && (yy >= Y_min) && (yy <= Y_max))
			{
			  wmap[yy][xx]=1;
			}
		      else
			{
			  wmap[yy][xx]=-1;
			}
		    }
		}
	      
/*  	      headas_chat(CHATTY, "%s: Info: wmap\n", global.taskname); */
/*  	      for(yy=0; yy<CCD_ROWS; yy++) */
/*  		{ */
/*  		  for(xx=0; xx<CCD_PIXS; xx++) */
/*  		    { */
/*  		      if(wmap[yy][xx]>0) */
/*  			{ */
/*  			  headas_chat(CHATTY, "x=%d y=%d ", xx, yy); */
/*  			} */
/*  		    } */
/*  		} */
/*  	      headas_chat(CHATTY, "\n"); */
	      
	    }
	  else if (!strcmp(global.region.shape, "CIRCLE-CIRCLE"))
	    {
	      
	      if (ComputeRawCoord(global.region.X[0], global.region.Y[0], mean_time, img_mjdref, teldef, attfile, &reg_cx, &reg_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY,"%s: Info: Region CIRCLE center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg_cx,reg_cy);

	      
	      if(!finite(reg_cx) || !finite(reg_cy) || reg_cx > RAWX_MAX || reg_cx < RAWX_MIN || reg_cy > RAWY_MAX || reg_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[0], global.region.Y[0], mean_time);
		  goto create_end;
		}

	      if (ComputeRawCoord(global.region.X[1], global.region.Y[1], mean_time, img_mjdref, teldef, attfile, &reg2_cx, &reg2_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region -CIRCLE center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg2_cx,reg2_cy);

	      
	      if(!finite(reg2_cx) || !finite(reg2_cy) || reg2_cx > RAWX_MAX || reg2_cx < RAWX_MIN || reg2_cy > RAWY_MAX || reg2_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[1], global.region.Y[1], mean_time);
		  goto create_end;
		}
	      
	      
	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if((xx-reg_cx)*(xx-reg_cx)+(yy-reg_cy)*(yy-reg_cy) <= global.region.R[0]*global.region.R[0])
			{
			  wmap[yy][xx]=1;
			}
		  
		      if((xx-reg2_cx)*(xx-reg2_cx)+(yy-reg2_cy)*(yy-reg2_cy) <= global.region.R[1]*global.region.R[1])
			{
			  wmap[yy][xx]=-1;
			}
		    }
		}
	    }
	  else if (!strcmp(global.region.shape, "BOX-BOX"))
	    {

	      if( global.region.rotang[0] < (global.instr_key.pa_pnt -10 ) || 
		  global.region.rotang[0] > (global.instr_key.pa_pnt +10 ) ||
		  global.region.rotang[1] < (global.instr_key.pa_pnt -10 ) || 
		  global.region.rotang[1] > (global.instr_key.pa_pnt +10 ) )
		{
		  headas_chat(NORMAL, "%s: Error: Bad 'ROTANG' value in input region\n", global.taskname );
		  headas_chat(NORMAL, "%s: Error: (region 'ROTANG' value must be equal to ROLL).\n", global.taskname );
		  goto create_end;
		}

	      
	      if (ComputeRawCoord(global.region.X[0], global.region.Y[0], mean_time, img_mjdref, teldef, attfile, &reg_cx, &reg_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region BOX center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg_cx,reg_cy);

	      
	      if(!finite(reg_cx) || !finite(reg_cy) || reg_cx > RAWX_MAX || reg_cx < RAWX_MIN || reg_cy > RAWY_MAX || reg_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[0], global.region.Y[0], mean_time);
		  goto create_end;
		}
	      
	      
	      X_min = reg_cx - global.region.R[0]/2;
	      X_max = reg_cx + global.region.R[0]/2;      
	      
	      Y_min = reg_cy - global.region.R1[0]/2;
	      Y_max = reg_cy + global.region.R1[0]/2;
	            

	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    { 
		      if((xx >= X_min) && (xx <= X_max) && (yy >= Y_min) && (yy <= Y_max))
			{
			  wmap[yy][xx]=1;
			}
		      else
			{
			  wmap[yy][xx]=-1;
			}
		    }
		}
      
	      
	      /* Update wmap with -BOX values  */
	      
	      if (ComputeRawCoord(global.region.X[1], global.region.Y[1], mean_time, img_mjdref, teldef, attfile, &reg2_cx, &reg2_cy))
		{
		  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
		  goto create_end;
		}
	      headas_chat(CHATTY, "%s: Info: Region -BOX center in RAW coordinates: [%.4f , %.4f] \n",global.taskname,reg2_cx,reg2_cy);

	      
	      if(!finite(reg2_cx) || !finite(reg2_cy) || reg2_cx > RAWX_MAX || reg2_cx < RAWX_MIN || reg2_cy > RAWY_MAX || reg2_cy < RAWY_MIN )
		{  
		  headas_chat(CHATTY, "%s: Error: region X=%f Y=%f is out of the CCD for time %18.8f\n", 
			      global.taskname, global.region.X[1], global.region.Y[1], mean_time);
		  goto create_end;
		}
	  
	      
	      X_min = reg2_cx - global.region.R[1]/2;
	      X_max = reg2_cx + global.region.R[1]/2;
  
	      Y_min = reg2_cy - global.region.R1[1]/2;
	      Y_max = reg2_cy + global.region.R1[1]/2;


	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    { 
		      if((xx >= X_min) && (xx <= X_max) && (yy >= Y_min) && (yy <= Y_max))
			wmap[yy][xx]=-1;
		    }
		}
	      
/*  	      headas_chat(CHATTY, "%s: Info: wmap\n", global.taskname); */
/*  	      for(yy=0; yy<CCD_ROWS; yy++) */
/*  		{ */
/*  		  for(xx=0; xx<CCD_PIXS; xx++) */
/*  		    { */
/*  		      if(wmap[yy][xx]>0) */
/*  			{ */
/*  			  headas_chat(CHATTY, "x=%d y=%d ", xx, yy); */
/*  			} */
/*  		    } */
/*  		} */
/*  	      headas_chat(CHATTY, "\n"); */
	      
	    }
	  
	  
	  /* Compute first_wmapx,last_wmapx,first_wmapy,last_wmapy */

	  /* WT mode */
	  if(!strcmp(global.instr_key.datamode, KWVL_DATAMODE_TM))
	    {
	      first_wmapy=0;
	      last_wmapy=CCD_ROWS-1;

	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if(wmap[yy][xx]>0)
			{
			  if(xx < first_wmapx)
			    first_wmapx=xx;
			  if(xx > last_wmapx)
			    last_wmapx=xx;
			}
		    }
		}
	    }
	  /* PC mode */
	  else 
	    {
	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if(wmap[yy][xx]>0)
			{
			  if(xx < first_wmapx)
			    first_wmapx=xx;
			  if(yy < first_wmapy)
			    first_wmapy=yy;
			  if(xx > last_wmapx)
			    last_wmapx=xx;
			  if(yy > last_wmapy)
			    last_wmapy=yy;
			}
		    }
		}
	    }
	  /*  printf("first_wmapx %i last_wmapx %i first_wmapy %i last_wmapy %i\n",first_wmapx,last_wmapx,first_wmapy,last_wmapy); */

	  /* Apply wmap[] to raw map data[][] */

	  /* WT mode */
	  if(!strcmp(global.instr_key.datamode, KWVL_DATAMODE_TM))
	    {

	      for(xx=0; xx<CCD_PIXS; xx++)  cexp[xx]=-1;


	      for(xx=0; xx<CCD_PIXS; xx++)
		{
		  if(wmap[(int)floor(cy+0.5)][xx]>0)
		    cexp[xx]=data[(int)floor(cy+0.5)][xx];
		}
	      
	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      data[yy][xx]=cexp[xx];
		    }
		}

	      headas_chat(CHATTY,"%s: Info: Transformation of Image in %d hdu:\n", global.taskname,(tmp_hdu+1));
	      for(xx=0; xx<CCD_PIXS; xx++)
		headas_chat(CHATTY,"(%d) %f | ", xx, cexp[xx]);
	      headas_chat(CHATTY,"\n");
	    }
	  /* PC mode */
	  else
	    {
	      for(yy=0; yy<CCD_ROWS; yy++)
		{
		  for(xx=0; xx<CCD_PIXS; xx++)
		    {
		      if(wmap[yy][xx] < 0)
			data[yy][xx]=-1;
		    }
		}
	    }
	  
	  
	  if(global.par.psfflag)
	    {

	      d[0]=sqrt((cx-first_wmapx)*(cx-first_wmapx) + (cy-first_wmapy)*(cy-first_wmapy));
	      d[1]=sqrt((cx-first_wmapx)*(cx-first_wmapx) + (cy-last_wmapy)*(cy-last_wmapy));
	      d[2]=sqrt((cx-last_wmapx)*(cx-last_wmapx) + (cy-first_wmapy)*(cy-first_wmapy));
	      d[3]=sqrt((cx-last_wmapx)*(cx-last_wmapx) + (cy-last_wmapy)*(cy-last_wmapy));
	      
	      
	      maxd=-1.;
	      
	      for (i=0; i<4; i++)
		{
		  if (d[i] > maxd) maxd=d[i];
		}
	      maxd_int=floor(maxd);

	      wei=(double *)calloc((maxd_int+1), sizeof(double));
	      circ=(double *)calloc((maxd_int+1),sizeof(double));
	      
	      for (i=0; i<=maxd_int; i++) 
		wei[i]=0.;
	      for (i=0; i<=maxd_int; i++) 
		circ[i]=0.;
	      
	      limx1=floor(cx-maxd);
	      limx2=floor(cx+maxd);
	      limy1=floor(cy-maxd);
	      limy2=floor(cy+maxd);
	      /*  printf("limx1 == %i limx2 == %i limy1 == %i limy2 == %i\n", limx1, limx2, limy1, limy2); */
	      
	      off=sqrt((cx-CDETX)*(cx-CDETX) + (cy-CDETY)*(cy-CDETY))*(PIXEL_SIZE)/60.;
	      
	      if(GetPSFFileCoeff(off))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to read\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.psffile);
		}  
	    }

	  xrtnfram=0.0;
	  if((ExistsKeyWord(&head, KWNM_XRTNFRAM  , &card)))
	    {
	      xrtnfram=card->u.DVal;
	      if((xrtnfram >= 0.0 - sens) &&  (xrtnfram <= 0.0 + sens) )
		{
		  headas_chat(NORMAL, "%s: Error: %s keyword set to %f\n", global.taskname,KWNM_XRTNFRAM, xrtnfram);
		  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
		  goto create_end;
		}
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_XRTNFRAM);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_TSTART  , &card)))
	    {
	      tmp_tstart=card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }

	  if((ExistsKeyWord(&head, KWNM_TSTOP  , &card)))
	    {
	      tmp_tstop=card->u.DVal;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.instrmap);
	      goto create_end;
	    }
	  

	  if(global.frac_index)
	    {
	      global.frac_index++;
	      global.frac_expo = (double *)realloc(global.frac_expo, (global.frac_index*sizeof(double)));
	      global.time_expo = (double *)realloc(global.time_expo, (global.frac_index*sizeof(double)));
	    }
	  else
	    {
	      
	      global.frac_index++;
	      global.frac_expo = (double *)malloc(sizeof(double));
	      global.time_expo = (double *)malloc(sizeof(double));
	  
	  
	    }/* End memory allocation */

	  tot_pix=0.0;
	  expo_pix=0.;
	  if(global.par.psfflag)
	    {
	      for (xx=limx1; xx<limx2; xx++) 
		{
		  for (yy=limy1; yy<limy2; yy++)
		    { 
		      dbl_r=sqrt((cx-xx)*(cx-xx)+(cy-yy)*(cy-yy))+0.5;
		      int_r=floor(dbl_r);

		      if (int_r <= maxd_int) 
			circ[int_r]=circ[int_r]+1; 

		      if (xx > first_wmapx && xx < last_wmapx && yy > first_wmapy && 
			  yy < last_wmapy && data[yy][xx] > 0 )
			{	  
			  wei[int_r]+=data[yy][xx];
			 /* printf("wei[%d] = %f data[%d][%d]=%d\n", int_r, wei[int_r],yy, xx, data[yy][xx]); */
			}
		    }
		}
	      
	      if (circ[0] == 0.) 
		circ[0]=1.; 
	      for (i=0; i<=maxd_int; i++)
		{
		  /*      headas_chat(NORMAL, " circ %i %f \n",i,circ[i]);*/
		  if(circ[i] == 0)
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to read region information\n", global.taskname);
		      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.lcfile);
		      goto create_end;
		    }
		  /*  printf("wei[%d] = %f \n", i, wei[i] ); */
		  wei[i]=wei[i]/(circ[i]*xrtnfram);
		}

/*  	      for (i=0; i<=maxd_int; i++) */
/*  		printf("wei[%d] = %f -- circ[%d]=%f\n", i, wei[i], i,circ[i] ); */
	      
	      
	      if(PsfCorr(maxd_int, wei, &pcorr))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to calculate PSF correction.\n", global.taskname);
		  
		}
	    }
	  else
	    {
	      for (yy=first_wmapy ; yy < last_wmapy ; yy++)
		{
		  for (xx=first_wmapx ; xx < last_wmapx ; xx++)
		    {
		      if( data[yy][xx] >0 )
			{
			  expo_pix+=data[yy][xx];
			  tot_pix+=1.0;
			}
		    }
		}
	      
	      tot_pix*=xrtnfram;
	      pcorr=(double)expo_pix/tot_pix;
	      /*printf(" TOT_PIX=%d EXPO_PIX=%f FRACEXPO=%f \n", tot_pix,expo_pix,pcorr);*/
	    }

	  if(pcorr)
	    {
	      global.frac_expo[global.frac_index-1]=1/pcorr;
	    }
	  else
	    global.frac_expo[global.frac_index-1]=0.0;
	  
	  global.time_expo[global.frac_index-1]=tmp_tstart;
	  
	}/* end if hdu is image type */
    }


  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.instrmap);
      goto create_end;
    }


  /* Close Attitude file */
  closeAttFile(attfile);


  /* Create output primary array */
  if(fits_create_file(&outunit, global.par.corrfile, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.corrfile);
      goto create_end;
    }

  if ( fits_create_tbl( outunit, BINARY_TBL, global.frac_index, tfields, ttype, tform, tunit, "LCCORRFACT", &status) )  
    {
      headas_chat(NORMAL,"%s: Error: Unable to create %s extension\n", global.taskname,"LCCORRFACT" );
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.corrfile);
      goto create_end;
    }

  /* Write into output file computed values */ 
  firstrow  = 1;  /* first row in table to write   */
  firstelem = 1;  /* first element in row  (ignored in ASCII tables) */
  
  if(fits_write_col(outunit, TDOUBLE, 1, firstrow, firstelem, global.frac_index,global.time_expo , &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to fill %s column\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
      goto create_end;
    }

  if(fits_write_col(outunit, TDOUBLE, 2, firstrow, firstelem, global.frac_index, global.frac_expo, &status))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to fill %s column.\n", global.taskname, CLNM_FRACEXPO);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
      goto create_end;
    }

  if(AddInstrSwiftXRTKeyword(outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to add keywords in the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.corrfile);
      goto create_end;
    }
  

  /* write history is parameter history set */
  if(HDpar_stamp(outunit, 1, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto create_end;
    }
  
  /* Calculate checksum and add it in file */
  
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.corrfile);
      goto create_end;
    }
  
  
  if(fits_close_file(outunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.corrfile);
      goto create_end;
    }
  
  headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.corrfile);    

  return OK;
 create_end:
  return NOT_OK;
  
}
/*CreateExpoTable*/


int PsfCorr(int rx, double *wei, double *pcorr)
{     
  int       j;
  double    pi=3.1415927,fk,fg,totflux;
  double    jx,tcorr,icorr;


  if((global.psf_c3-1.) == 0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
      goto psf_end;
    }

  totflux=2.*pi*global.psf_c0*global.psf_c1*global.psf_c1+pi*global.psf_c2*global.psf_c2*(1.-global.psf_c0)/(global.psf_c3-1.);
  tcorr=0.;
  icorr=0.;

  for (j=0; j <= rx; j++)
    { 
      if(global.psf_c2 == 0 || global.psf_c1 == 0)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
	  goto psf_end;
	}
      
      jx=1.*j+0.5;
      fk=pi*global.psf_c2*global.psf_c2*(1.-global.psf_c0)/(1.-global.psf_c3)*(pow((1.+(jx/global.psf_c2)*(jx/global.psf_c2)),(1.-global.psf_c3))-1.);
      fg=2.*pi*global.psf_c0*global.psf_c1*global.psf_c1*(1.-exp(-1.*jx*jx/2./(global.psf_c1*global.psf_c1)));
      
      tcorr=tcorr+((fg+fk)-icorr)*wei[j];
      icorr=fg+fk;

    }

  if(totflux == 0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
      goto psf_end;
    }


  *pcorr=tcorr/totflux;

  
  return OK;
  
 psf_end:   
  return NOT_OK;
  
} /* psf_corr */


/* GetPSFFileCoeff */
int GetPSFFileCoeff(double off)
{

  int            n, status=OK;
  long           extno;
  char           expr[256];
  float          par0[NCOEFF],par1[NCOEFF],par2[NCOEFF],par3[NCOEFF];
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  PSFCol_t       psfcol; 
  Bintable_t     table;             /* Bintable pointer */  
  FitsHeader_t   head;              /* Extension pointer */
  FitsFileUnit_t unit=NULL;        /* Gain file pointer */

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Set expression for CALDB query */
  if(!strcmp(KWVL_DATAMODE_PH,global.instr_key.datamode) || !strcmp(global.instr_key.datamode,KWVL_DATAMODE_TM))
    sprintf(expr, "datamode.eq.%s", global.instr_key.datamode);
  else
    sprintf(expr, "%s", HD_EXPR);


  extno=-1;
  
  /* Derive CALDB psf filename */ 
  if (!(strcasecmp (global.par.psffile, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_PSF_DSET, global.par.psffile, expr, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto psf_end;
	}
      extno++;
    }
  
  /* Open read only gain file */
  if ((unit=OpenReadFitsFile(global.par.psffile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to open '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }
 
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,unit,global.par.psffile,global.taskname);
  

  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(unit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile); 
	  goto psf_end;
	}
    }
  else
    {
      if (!strcmp(global.instr_key.datamode,KWVL_DATAMODE_TM ))
	{
	  if (fits_movnam_hdu(unit,ANY_HDU,KWVL_EXTNAME_WTPSFCOEF,0,&status))
	    { 
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_WTPSFCOEF);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
	      goto psf_end;
	    }
	}
      else
	{
	  if (fits_movnam_hdu(unit,ANY_HDU,KWVL_EXTNAME_PCPSFCOEF,0,&status))
	    { 
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PCPSFCOEF);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
	      goto psf_end;
	    } 
	}
    }

  head = RetrieveFitsHeader(unit);

  /* Read ph2pi bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.psffile);
      goto psf_end;
    }
  else if(table.MaxRows != NCOEFF)
    {
      headas_chat(NORMAL, "%s: Error: %s file\n", global.taskname, global.par.psffile);
      headas_chat(NORMAL, "contains %d rows instead of %d.\n", global.taskname, table.MaxRows, NCOEFF);
      goto psf_end;
    }  

  nCols=table.nColumns;
      
  /* Get columns index from name */
  if ((psfcol.COEF0 = GetColNameIndx(&table, CLNM_COEF0)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_COEF0);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }
  /* Get columns index from name */
  if ((psfcol.COEF1 = GetColNameIndx(&table, CLNM_COEF1)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_COEF1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }

  /* Get columns index from name */
  if ((psfcol.COEF2 = GetColNameIndx(&table, CLNM_COEF2)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_COEF2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }

  /* Get columns index from name */
  if ((psfcol.COEF3 = GetColNameIndx(&table, CLNM_COEF3)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_COEF3);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }

  /* Read blocks of bintable rows from input gain file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  
  while (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  par0[n]=EVEC(table, n, psfcol.COEF0);
	  par1[n]=EVEC(table, n, psfcol.COEF1);
	  par2[n]=EVEC(table, n, psfcol.COEF2);
	  par3[n]=EVEC(table, n, psfcol.COEF3);
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  

  /* Close psf file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.psffile);
      goto psf_end;
    }


  global.psf_c0=(double)(par0[0]+off*par1[0]+global.par.energy*par2[0]+off*global.par.energy*par3[0]);
  global.psf_c1=(double)(par0[1]+off*par1[1]+global.par.energy*par2[1]+off*global.par.energy*par3[1]);
  global.psf_c2=(double)(par0[2]+off*par1[2]+global.par.energy*par2[2]+off*global.par.energy*par3[2]);
  global.psf_c3=(double)(par0[3]+off*par1[3]+global.par.energy*par2[3]+off*global.par.energy*par3[3]);

  /*printf("c0=%f c1=%f c2=%f c3=%f\n",  global.psf_c0,global.psf_c1, global.psf_c2,global.psf_c3);*/
  
  return OK;
  
 psf_end:
  
  if (unit)
    CloseFitsFile(unit);
  
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;
}/* GetPSFFileCoeff */


/*CorrectLCCurve*/
int CorrectLCCurve(FitsFileUnit_t lcunit)
{

  int             status=OK, hdutot, hducount, hdulc, iii=0;  
  double          tmp_tstart=0.0, tmp_time=0.0, sens=0.001;
  char            tmp_obsmode[15], tmp_obsid[15];
  RTCol_t         indxcol;
  unsigned        FromRow, ReadRows, OutRows, n, nCols;
  Bintable_t	  outtable;
  FitsCard_t      *card;
  FitsHeader_t    head;
  FitsFileUnit_t  outunit=NULL;
 
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  headas_chat(NORMAL, "%s: Info: Correcting light curve.\n", global.taskname);

  /* Get total number of HDUs in input events file */
  if (fits_get_num_hdus(lcunit, &hdutot, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: the '%s' file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }
  
  hducount=1;
  
  /* Build  primary header */
  if((outunit = CreateFile(lcunit, global.par.outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }
  
  hducount++;
  
  /* Move in events extension */
  if (fits_movnam_hdu(lcunit, ANY_HDU,KWVL_EXTNAME_RATE, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_RATE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.lcfile);
      goto corr_end;
    } 

  if (!fits_get_hdu_num(lcunit, &hdulc))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_RATE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.lcfile);
      goto corr_end;
    } 
  
  if (hdulc!=hducount)
    {
      for (; hducount<hdulc; hducount++)
	if(CopyHdu(lcunit, outunit, hducount)) {
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,hducount);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.lcfile);
	  headas_chat(CHATTY,"%s: Error: to %s output file.\n",global.taskname, global.par.outfile);
	  goto corr_end;
	}
    }
  
  if(fits_movabs_hdu(lcunit, hducount, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,hducount);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }

  head=RetrieveFitsHeader(lcunit);  

  if((ExistsKeyWord(&head, "TIMEZERO"  , &card)))
    {
      tmp_tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }

  if((ExistsKeyWord(&head, KWNM_TIMEDEL  , &card)))
    {
      if(card->u.DVal > (10. + sens))
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword value is larger than 10. sec \n", global.taskname,KWNM_TIMEDEL);
	  goto corr_end;
	}
      tmp_tstart+=(card->u.DVal/2); 
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }
  
  if((ExistsKeyWord(&head, KWNM_OBS_ID  , &card)))
    {
      strcpy(tmp_obsid, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }
  
  if((ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
    {
      strcpy(tmp_obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }

  if(strcmp(tmp_obsid, global.instr_key.obsid) || strcmp(tmp_obsmode, global.instr_key.obsmode))
    {
      headas_chat(NORMAL, "%s: Error: %s instrument map is not appropriate for\n", global.taskname,global.instrmap);
      headas_chat(NORMAL, "%s: Error: %s Light curve.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }


  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.lcfile);
      goto corr_end;
    }

  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto corr_end;
    }
  
  if((indxcol.RATE=ColNameMatch(CLNM_RATE, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RATE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto corr_end;
    }

  if((indxcol.ERROR=ColNameMatch(CLNM_ERROR, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ERROR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.lcfile);
      goto corr_end;
    }
  
  EndBintableHeader(&head, &outtable); 
  
  GetGMTDateTime(global.date);
  /* Add history */
  if(global.hist)
    {
      sprintf(global.strhist, "File modified  by %s (%s) at %s", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
    }

  /* write out new header to new file */
  FinishBintableHeader(outunit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  iii=0;
  /* Check bad pixels and flag them */
  while(ReadBintable(lcunit, &outtable, nCols, NULL, FromRow,&ReadRows)==0)
    {
      for(n=0; n<ReadRows; ++n)
	{
	  tmp_time=tmp_tstart+DVEC(outtable, n,indxcol.TIME);
	 
	
	  while(iii < global.frac_index && global.time_expo[iii] < tmp_time)
	    iii++;
	  /*printf("ROW == %d ", n);*/

	  if(iii==0)/* || ((iii == (global.frac_index - 1) && tmp_time < global.time_expo[iii])))*/
	    {
	      EVEC(outtable, n,indxcol.RATE)=EVEC(outtable, n,indxcol.RATE)*global.frac_expo[iii];
	      EVEC(outtable, n,indxcol.ERROR)=EVEC(outtable, n,indxcol.ERROR)*global.frac_expo[iii];
	      /*  printf("iii=%d TIME_LC==%f -- TIME_CORR == %f\n",iii, tmp_time, global.time_expo[iii]); */     
	    }
	  else
	    {
	      EVEC(outtable, n,indxcol.RATE)=EVEC(outtable, n,indxcol.RATE)*global.frac_expo[iii-1];
	      EVEC(outtable, n,indxcol.ERROR)=EVEC(outtable, n,indxcol.ERROR)*global.frac_expo[iii-1];
	      /*  printf("iii=%d TIME_LC==%f -- TIME_CORR == %f\n",(iii-1), tmp_time, global.time_expo[iii-1]); */
	    }

	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(outunit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    }

	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  
  WriteFastBintable(outunit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);


  if(fits_update_key(outunit, TLOGICAL, KWNM_VIGNAPP, &global.instr_key.vignapp, CARD_COMM_VIGNAPP, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }

  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPSFCO, &global.par.psfflag, CARD_COMM_XRTPSFCO, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname,KWNM_XRTPSFCO);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }

  if(global.par.psfflag)
    {
      if(fits_update_key(outunit, TDOUBLE, KWNM_XRTPSFEN , &global.par.energy, CARD_COMM_XRTPSFEN, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname,KWNM_XRTPSFEN);
	  headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto corr_end;
	}
    }


  hducount++;

  for (; hducount<=hdutot; hducount++)
    {
      if(CopyHdu(lcunit, outunit, hducount)) 
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,hducount);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.lcfile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.par.outfile);
	  goto corr_end;
	}
    }

  if(HDpar_stamp(outunit, hdulc, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto corr_end;
    }
  
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto corr_end;
    }
  
  /* Close events tmp file */
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.par.outfile);
      goto corr_end;
    }

  headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.outfile); 

  return OK;
  
 corr_end:
  
  return NOT_OK;
}/* CorrLCCurve */


int AddInstrSwiftXRTKeyword(FitsFileUnit_t inunit)
{
  int  status=OK, tot_hdu, tmp_hdu, hdu_type=0;


  if(fits_get_num_hdus(inunit, &tot_hdu, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to get the total number of hdus\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
      goto add_end;
    }
  for(tmp_hdu=0; tmp_hdu < tot_hdu; tmp_hdu++)
    {
      if(fits_movabs_hdu(inunit, (tmp_hdu+1), &hdu_type, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to move in the %d hdu\n", global.taskname, (tmp_hdu+1));
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      

      if(fits_update_key(inunit, TSTRING, KWNM_TELESCOP,  KWVL_TELESCOP,CARD_COMM_TELESCOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_INSTRUME,  KWVL_INSTRUME,CARD_COMM_INSTRUME, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TINT,KWNM_MJDREFI , &global.instr_key.mjdrefi,CARD_COMM_MJDREFI, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFI);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_MJDREFF , &global.instr_key.mjdreff,CARD_COMM_MJDREFF, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFF);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID,  global.instr_key.obsid,CARD_COMM_OBS_ID, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TINT,KWNM_TARG_ID , &global.instr_key.target,CARD_COMM_TARG_ID, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TARG_ID);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      if(fits_update_key(inunit, TINT,KWNM_SEG_NUM , &global.instr_key.segnum,CARD_COMM_SEG_NUM, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_SEG_NUM);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.instr_key.tstart,CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.instr_key.tstop,CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_DATEOBS,  global.instr_key.dateobs,CARD_COMM_DATEOBS, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEOBS);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_DATEEND,  global.instr_key.dateend,CARD_COMM_DATEEND, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEEND);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}  
      
      if(fits_update_key(inunit, TLOGICAL, KWNM_VIGNAPP,  &global.instr_key.vignapp,CARD_COMM_VIGNAPP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_VIGNAPP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}  

      if(fits_update_key(inunit, TDOUBLE, KWNM_PA_PNT,  &global.instr_key.pa_pnt,CARD_COMM_PA_PNT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_PA_PNT);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}

      if(fits_update_key(inunit, TLOGICAL, KWNM_XRTPSFCO, &global.par.psfflag, CARD_COMM_XRTPSFCO, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname,KWNM_XRTPSFCO);
	  headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	  goto add_end;
	}
     
      if(global.par.psfflag)
	{
	  if(fits_update_key(inunit, TDOUBLE, KWNM_XRTPSFEN , &global.par.energy, CARD_COMM_XRTPSFEN, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname,KWNM_XRTPSFEN);
	      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.corrfile);
	      goto add_end;
	    }
	}  
    }

  return OK;

 add_end:
  return NOT_OK;

}/*AddInstrSwiftXRTKeyword*/


/*
 *
 *  ComputeRawCoord
 *
 *  DESCRIPTION:
 *    Routine to convert sky coordinates to raw coordinates
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeRawCoord(double sky_x, double sky_y, double time, double mjdref, TELDEF* teldef, ATTFILE* attfile, double* raw_x, double* raw_y)
{  
  double     db_dety_tmp=1., db_x, db_y, det_y=0., db_detx_tmp=1., det_x=0.;
  double     mjd=0.;
  double     rawx=0.0, rawy=0.0;
  int        ret=0;
  Coord_t    coord;

  QUAT*      q;
  XFORM2D*   sky2det;
 
  sky2det = allocateXform2d();
  q = allocateQuat();

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Error: Time: %f is not included\n", global.taskname, time);
      headas_chat(NORMAL,"%s: Error: in the time range of the\n", global.taskname); 
      headas_chat(NORMAL,"%s: Error: %s Attitude file.\n", global.taskname, global.par.attfile);
      *raw_x = -1; 
      *raw_y = -1; 
 
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
      coord.v=earth_velocity(coord.vhat,earth_longitude(mjd));
    }
  else
    {
      coord.v=0.;
      coord.vhat[0]=0.;
      coord.vhat[1]=0.;
      coord.vhat[2]=0.;
    }

  convertDetectorToSkyUsingTeldef(teldef, &db_x, &db_y,
				  db_detx_tmp, db_dety_tmp, 
				  q, coord.v, coord.vhat);
  
  invertXform2d(sky2det, teldef->det2sky);
  applyXform2dToContinuousCoords(sky2det, &det_x, &det_y, sky_x, sky_y);
  
/*    headas_chat(CHATTY,"%s: ComputeRawCoord: skyx=%f skyy=%f\n", global.taskname, sky_x, sky_y); */
/*    headas_chat(CHATTY,"%s: ComputeRawCoord: detx=%f dety=%f\n", global.taskname, det_x, det_y); */


  ret = convertDetectorToRawUsingTelDef(teldef, &rawx, &rawy,
					det_x, det_y);
  if(ret<0)
    {
      headas_chat(NORMAL, "%s: Error: point (DETX=%.1f DETY=%.1f) does not lie on any segment.\n",global.taskname,det_x,det_y);
      return NOT_OK;
    }
/*    headas_chat(CHATTY,"%s: ComputeRawCoord: rawx=%i rawy=%i\n", global.taskname,(int)floor(rawx+0.5), (int)floor(rawy+0.5) ); */

  *raw_x=rawx;
  *raw_y=rawy;
   
  return OK;

} /*  ComputeRawCoord */


int ReadEventFile(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int *amp)
{
  int                   status=OK, rawx=0, rawy=0, count, ext, iii=0; 
  int                   neigh_x[] = {NEIGH_X}, jj;
  int                   neigh_y[] = {NEIGH_Y};
  char		        KeyWord[FLEN_KEYWORD], attflag[FLEN_KEYWORD];
  EvtCol_t              evtcol;
  BPCol_t               bpcol;
  GTICol_t              gticol;
  unsigned              FromRow, ReadRows, n, nCols;
  BadPixel_t            pix, pix_tmp;
  Bintable_t	        table;
  FitsCard_t            *card;
  FitsHeader_t          evhead;            /* Extension header pointer */
  FitsFileUnit_t        evunit=NULL;       /* input and output file pointers */
  

  /* Open read only input event file */
  if ((evunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  /* Move to EVENT extension */ 
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
      goto end_evtfunc;
    }
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, evunit, global.par.infile,global.taskname);
  
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);
  
  /* Get extension header pointer  */
  evhead=RetrieveFitsHeader(evunit);  
  

  /************************************
   *  Read keywords                   *
   ************************************/
  
  if((ExistsKeyWord(&evhead, KWNM_ATTFLAG, &card)))
    {
      switch(card->ValueTag){
      case J:	/* signed integer */
	sprintf(attflag, "%li", card->u.JVal);
	break;
      case I:	/* unsigned integer */
	sprintf(attflag, "%d", card->u.IVal);
	break;
      default: 	/* string */
	strcpy(attflag, card->u.SVal);
	break;
      }

    }
  else
    {
      strcpy(attflag, DEFAULT_ATTFLAG);
      headas_chat(NORMAL,"%s: Warning: 'ATTFLAG' keyword not found in %s\n", global.taskname, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value %s\n", global.taskname, DEFAULT_ATTFLAG);     
    }

  if(strcmp(attflag, global.attflag))
    {
      headas_chat(NORMAL,"%s: Error: Input attitude file not consistent with the one used in the calibration of the input event file.\n", global.taskname);
      goto end_evtfunc;
    }


  if((ExistsKeyWord(&evhead, KWNM_XRTMAXTP  , &card)))
    {
      global.evt_key.maxtemp=card->u.DVal;
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: %s keyword not found\n", global.taskname,KWNM_XRTMAXTP);
      headas_chat(CHATTY, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
      global.evt_key.maxtemp=0.0;
      headas_chat(CHATTY, "%s: Info: using %f default value for temperature threshold for burned spot.\n", global.taskname, global.evt_key.maxtemp);
      
    }

  if((ExistsKeyWord(&evhead, KWNM_OBS_ID  , &card)))
    {
      strcpy(global.evt_key.obsid, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_MJDREFI  , &card)))
    {
      global.evt_key.mjdrefi=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_MJDREFF  , &card)))
    {
      global.evt_key.mjdreff=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TARG_ID  , &card)))
    {
      global.evt_key.target=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TARG_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_SEG_NUM  , &card)))
    {
      global.evt_key.segnum=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_SEG_NUM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.evt_key.obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DATAMODE  , &card)))
    {
      strcpy(global.evt_key.datamode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }


  if(!strcmp(global.evt_key.datamode, KWVL_DATAMODE_PH))
    global.evt_key.xrtmode=XRTMODE_PC;
  else if(!strcmp(global.evt_key.datamode, KWVL_DATAMODE_TM))
    global.evt_key.xrtmode=XRTMODE_WT;
  else
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.evt_key.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s and %s\n", global.taskname, KWVL_DATAMODE_PH,KWVL_DATAMODE_TM);
 
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	}
      
      goto end_evtfunc;
    }
     
/* RQS: se PC => leggi KWNM_WHALFWD... */

    /* Windowed Timing : */
  if(global.evt_key.xrtmode==XRTMODE_WT)
  {
    /* Get mask structure */
    if((ExistsKeyWord(&evhead, KWNM_WM1STCOL  , &card))  && ((card->u.JVal >= RAWX_WT_MIN) && (card->u.JVal <=  WM1STCOL_MAX)))
      global.evt_key.wm1stcol=card->u.JVal;
    else
      {
	headas_chat(NORMAL, "%s: Error: the %s keyword value is out of range\n", global.taskname, KWNM_WM1STCOL);
	goto end_evtfunc;
      }
    if((ExistsKeyWord(&evhead, KWNM_WMCOLNUM  , &card)) && ((card->u.JVal >= WT_NCOL_MIN) && (card->u.JVal <= WT_NCOL_MAX)))
        global.evt_key.wmcolnum=card->u.JVal;
    else
      {
	headas_chat(NORMAL, "%s: Error: the %s keyword value is out of range\n", global.taskname, KWNM_WMCOLNUM);
	goto end_evtfunc;
      }
  }     
    /* Photon counting: */
  else if(global.evt_key.xrtmode==XRTMODE_PC)
    {
    /* Get mask structure */
      if((ExistsKeyWord(&evhead, KWNM_WHALFWD  , &card)) && ((CCD_CENTERX-card->u.JVal >= RAWX_MIN) && (CCD_CENTERX+card->u.JVal-1 <= RAWX_MAX)) )
	global.evt_key.whalfwd=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: the %s keyword not found or  out of range\n", global.taskname, KWNM_WHALFWD);
	  goto end_evtfunc;
	}
      if((ExistsKeyWord(&evhead, KWNM_WHALFHT  , &card)) && ((CCD_CENTERY-card->u.JVal >= RAWY_MIN) && (CCD_CENTERY+card->u.JVal-1 <= RAWY_MAX)) )
	global.evt_key.whalfht=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: the %s keyword not found or  out of range\n", global.taskname, KWNM_WHALFHT);
	  goto end_evtfunc;
	}

    }

  if((ExistsKeyWord(&evhead, KWNM_RA_OBJ  , &card)))
    {
      global.evt_key.raobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DEC_OBJ  , &card)))
    {
      global.evt_key.decobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  if((ExistsKeyWord(&evhead, KWNM_RA_PNT  , &card)))
    {
      global.evt_key.rapnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DEC_PNT  , &card)))
    {
      global.evt_key.decpnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TSTART  , &card)))
    {
      global.evt_key.tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TSTOP  , &card)))
    {
      global.evt_key.tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  

  if(global.evt_key.xrtmode==XRTMODE_PC)
    {
      if(ExistsKeyWord(&evhead, KWNM_TIMEDEL, &card))
	{	  
	  global.evt_key.timedel=card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: %s keyword not found\n", global.taskname, KWNM_TIMEDEL);
	  headas_chat(NORMAL, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Info: using %f default value.\n", global.taskname, PC_TIMEDEL);
	  global.evt_key.timedel=PC_TIMEDEL;
	}
    }
  else
    {
      if(ExistsKeyWord(&evhead, "CCDEXPOS", &card))
	{  
	  global.evt_key.timedel=card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: CCDEXPOS keyword not found\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Info: using 1.0675 default value.\n", global.taskname);
	  global.evt_key.timedel=1.0675;
	}
    }

  if(ExistsKeyWord(&evhead, KWNM_XRTHNFLG, &card))
    {
      global.evt_key.xrthnflg=card->u.LVal;
    }
  else
    {
      global.evt_key.xrthnflg=0;
    }

  if (ExistsKeyWord(&evhead, KWNM_DATEOBS, &card))
    {
      global.evt_key.dateobs=card->u.SVal;
      if(!(strchr(global.evt_key.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&evhead, KWNM_TIMEOBS, &card))
	    global.evt_key.timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto end_evtfunc;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }


  if((ExistsKeyWord(&evhead, KWNM_DATEEND  , &card)))
    {
      strcpy(global.evt_key.dateend, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  if((ExistsKeyWord(&evhead, KWNM_PA_PNT, &card)))
    {
      global.evt_key.pa_pnt= card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_PA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }


  GetBintableStructure(&evhead, &table, BINTAB_ROWS, 0, NULL);

  if ((evtcol.X=ColNameMatch(CLNM_X, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  if ((evtcol.Y=ColNameMatch(CLNM_Y, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  sprintf(KeyWord, "TCRVL%d", (evtcol.X+1));
  if((ExistsKeyWord(&evhead, KeyWord, &card)))
    {
      global.evt_key.tcrvl_x = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  sprintf(KeyWord, "TCRVL%d", (evtcol.Y+1));
  if((ExistsKeyWord(&evhead, KeyWord, &card)))
    {
      global.evt_key.tcrvl_y = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }


  
  /* Move to BP extension */ 
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_BAD, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_BAD);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile); 
      
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);  
	}
      goto end_evtfunc;
    } 
  
  /* Get extension header pointer  */
  evhead=RetrieveFitsHeader(evunit);  

  global.bpcount=0;
  
  GetBintableStructure(&evhead, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  /* Get needed columns number from name */
  
  if ((bpcol.RAWX=ColNameMatch(CLNM_RAWX, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if ((bpcol.RAWY=ColNameMatch(CLNM_RAWY, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if ((bpcol.Amp=ColNameMatch(CLNM_Amp, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if ((bpcol.TYPE=ColNameMatch(CLNM_TYPE, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TYPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if ((bpcol.BADFLAG=ColNameMatch(CLNM_BADFLAG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  if ((bpcol.YEXTENT=ColNameMatch(CLNM_XYEXTENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XYEXTENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }

  EndBintableHeader(&evhead, &table);
 
  FromRow = 1;
  ReadRows=table.nBlockRows;
  *amp=0;
  
  while(ReadBintable(evunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  if(!(*amp))
	    *amp=IVEC(table, n, bpcol.Amp);

	  rawx=IVEC(table, n, bpcol.RAWX);
	  rawy=IVEC(table, n, bpcol.RAWY);

	  if(rawx>=0&&rawx<=CCD_PIXS&&rawy>=0&&rawy<=CCD_ROWS)
	    {
	      pix.bad_type=IVEC(table, n, bpcol.TYPE);
	      pix.xyextent=IVEC(table, n, bpcol.YEXTENT);
	      pix.bad_flag=XVEC2BYTE(table,n,bpcol.BADFLAG);

	      if( AddBadPix(rawx, rawy, pix, bp_table))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to fill bad pixels map.\n", global.taskname);
		  goto end_evtfunc;
		}

	      if(((~(OBS_HOT_BP|OBS_FLICK_BP))&pix.bad_flag) || global.evt_key.xrthnflg)
		{
		  pix_tmp.bad_type=1;
		  pix_tmp.xyextent=0;
		  pix_tmp.bad_flag=EV_NEIGH_BP;
		  for (jj=1; jj< PHAS_MOL; jj++)
		    {
		      if((rawx+neigh_x[jj]>=0 && rawx+neigh_x[jj]<CCD_PIXS)
			 &&(rawy+neigh_y[jj]>=0 && rawy+neigh_y[jj]<CCD_ROWS))
			{
			  AddBadPix((rawx+neigh_x[jj]), (rawy+neigh_y[jj]), pix_tmp, bp_table);
			}
		    }
		}

	      if (pix.xyextent > 1 ) 
		{
		  ext=pix.xyextent;
		  for (count=1; count<ext; count++)
		    {
		      rawy++;
		      pix.xyextent = 0;
		      if (AddBadPix(rawx,rawy,pix,bp_table))
			{
			  headas_chat(NORMAL, "%s: Error: Unable to  write pixels in bad pixels map.\n",global.taskname);
			  goto end_evtfunc;
			}
		 
		      if(((~(OBS_HOT_BP|OBS_FLICK_BP))&pix.bad_flag) || global.evt_key.xrthnflg)
			{     
			  pix_tmp.bad_type=1;
			  pix_tmp.xyextent=0;
			  pix_tmp.bad_flag=EV_NEIGH_BP;
			  for (jj=1; jj< PHAS_MOL; jj++)
			    {
			      if((rawx+neigh_x[jj]>=0 && rawx+neigh_x[jj]<CCD_PIXS)
				 &&(rawy+neigh_y[jj]>=0 && rawy+neigh_y[jj]<CCD_ROWS))
				{
				  AddBadPix((rawx+neigh_x[jj]), (rawy+neigh_y[jj]), pix_tmp, bp_table);
				}
			    }
			}     
		    }
		}
	      
	      global.bpcount++;
	    }
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
   ReleaseBintable(&evhead, &table);

  
  /* Move to GTI extension */ 
  if (fits_movnam_hdu(evunit, ANY_HDU,KWVL_EXTNAME_GTI, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile); 

       /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);  
	}
      goto end_evtfunc;
    } 
  
  /* Get extension header pointer  */
  evhead=RetrieveFitsHeader(evunit);  
  
  
  GetBintableStructure(&evhead, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  global.gticount=table.MaxRows;
  /* Get needed columns number from name */
  
  if ((gticol.START=ColNameMatch(CLNM_START, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_START);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  
  if ((gticol.STOP=ColNameMatch(CLNM_STOP, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
  

  EndBintableHeader(&evhead, &table);


  global.gti=(GTIInfo_t *)calloc(global.gticount, sizeof(GTIInfo_t));
  
  FromRow = 1;
  ReadRows=table.nBlockRows;

  iii=0;
  while(ReadBintable(evunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  if(iii < global.gticount)
	    {
	      global.gti[iii].start=DVEC(table, n, gticol.START);
	      global.gti[iii].stop=DVEC(table, n, gticol.STOP);
	      iii++;
	    }
	  else
	    {
	      headas_chat(NORMAL, " %s: Error: Unable to read GTI interval\n", global.taskname);
	      goto end_evtfunc;
	    }     
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
   ReleaseBintable(&evhead, &table);

    
  /* Close input event file */
  if (CloseFitsFile(evunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto end_evtfunc;
    }
    
  return OK; 
   
 end_evtfunc:
  return NOT_OK;
}/* ReadEventFile */


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
  int                status=OK, hkmode=0, iii=0, tmp_nframe;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[9];
  double             sens=0.00001, tmp_time, tmp_endtime, tmp_end, tsens=1;
  BOOL               good_frame, first=1, changeorbit=1;
  unsigned short     hksettled=0;
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsCard_t         *card;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;
  
  
  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.hkcount=0;
  
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

  if(ExistsKeyWord(&hkhead, KWNM_TSTART, &card))
    {           
      if(global.evt_key.tstart < (card->u.DVal - tsens))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHK_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if(ExistsKeyWord(&hkhead, KWNM_TSTOP, &card))
    { 
      if(global.evt_key.tstop > (card->u.DVal + tsens))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHK_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }
  
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

  if ((hkcol.ENDTIME=ColNameMatch(CLNM_ENDTIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENDTIME);
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

  if ((hkcol.CCDTEMP=ColNameMatch(CLNM_CCDTemp, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.RA=ColNameMatch(CLNM_RA, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  if ((hkcol.Dec=ColNameMatch(CLNM_Dec, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Dec);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHK_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=9; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.TIME;
  ActCols[2]=hkcol.ENDTIME;
  ActCols[3]=hkcol.XRTMode;
  ActCols[4]=hkcol.Settled;
  ActCols[5]=hkcol.In10Arcm;
  ActCols[6]=hkcol.CCDTEMP;
  ActCols[7]=hkcol.RA;
  ActCols[8]=hkcol.Dec;

  global.nsetframe=0;
  tmp_nframe=0;
  
  tmp_endtime=0.;
  changeorbit=1;

  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  /*hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);*/
	  
	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
	  tmp_time=DVEC(hktable, n, hkcol.TIME);
	  tmp_end=DVEC(hktable, n, hkcol.ENDTIME);
	  good_frame=0;
	  
	  if(hkmode != XRTMODE_BIAS)
	    {
	      if(!(first || ((tmp_time - tmp_endtime) < DIFF_TIME)))
		changeorbit=1;
	  
 	      
	      if(hkmode==global.evt_key.xrtmode)
		{
		  for (iii =0; iii < global.gticount ; iii++)
		    {
		      if ( (tmp_time+tmp_end)/2 >= global.gti[iii].start - sens && (tmp_time+tmp_end)/2 <= global.gti[iii].stop + sens)
			{
			  good_frame=1;
			  iii=global.gticount;
			}
		      else
			good_frame=0;
		    }
		}
	      else
		good_frame=0;
	      
	      if(good_frame)
		{
		  if(global.hkcount)
		    {
		      global.hkcount+=1;
		      global.hkinfo = (HKInfo_t *)realloc(global.hkinfo, ((global.hkcount)*sizeof(HKInfo_t)));
		    }
		  else
		    {      
		      global.hkcount+=1;
		      global.hkinfo = (HKInfo_t *)malloc(sizeof(HKInfo_t));  
		      
		    }/* End memory allocation */
		  
		  global.hkinfo[global.hkcount - 1].hkccdframe=VVEC(hktable, n, hkcol.CCDFrame);
		  global.hkinfo[global.hkcount - 1].hktime=tmp_time;
		  global.hkinfo[global.hkcount - 1].hkendtime=DVEC(hktable, n, hkcol.ENDTIME);
		  global.hkinfo[global.hkcount - 1].temp=EVEC(hktable, n, hkcol.CCDTEMP);
		  global.hkinfo[global.hkcount - 1].RA=EVEC(hktable, n, hkcol.RA);
		  global.hkinfo[global.hkcount - 1].Dec=EVEC(hktable, n, hkcol.Dec);
		        
		  if(changeorbit)
		    {
		      if(tmp_nframe)
			{
			  if(global.nsetframe)
			    {
			      global.nsetframe+=1;
			      global.framexorb = (int *)realloc(global.framexorb, ((global.nsetframe)*sizeof(int)));
			      global.framexorb[global.nsetframe-1]=tmp_nframe;      
			    }
			  else
			    {
			      global.nsetframe+=1;
			      global.framexorb = (int *)malloc(sizeof(int));
			      global.framexorb[global.nsetframe-1]=tmp_nframe;
			    }
			}
		      changeorbit=0;
		      tmp_nframe=0;
		    }
		
		  tmp_nframe++;      
		  
		}
	      first=0;
	      tmp_endtime=DVEC(hktable, n, hkcol.ENDTIME);

	    }
	  else
	    changeorbit=1;
	
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  
  if(tmp_nframe )
    {
      if(global.nsetframe)
	{
	  global.nsetframe+=1;
	  global.framexorb = (int *)realloc(global.framexorb, ((global.nsetframe)*sizeof(int)));
	  global.framexorb[global.nsetframe-1]=tmp_nframe-1;  
	}
      else
	{
	  global.nsetframe+=1;
	  global.framexorb = (int *)malloc(sizeof(int));
	  global.framexorb[global.nsetframe-1]=tmp_nframe-1;
	}

      global.framexorb[global.nsetframe-1]=tmp_nframe;
      tmp_nframe=0;
      
    }
  /*
    for (iii=0; iii < global.nsetframe; iii++)
    printf(" NORBIT == %d \n", global.framexorb[iii]);
  */
  if(!global.hkcount)
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


int GetCalFOVInfo(void)
{
  long           extno=0;


 /* Derive CALDB calibration sources filename */ 
  if (!(strcasecmp (global.par.fovfile, DF_CALDB)))
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_REGION_DSET, global.par.fovfile, "type.eq.FOV", &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      extno++;
    }
  else
    {
      if(CalGetExtNumber(global.par.fovfile, KWVL_EXTNAME_REGIONFOV, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s extension\n", global.taskname, KWVL_EXTNAME_REGIONFOV);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.fovfile);
	  goto get_end;
	} 
    }

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.fovfile);

  if(ReadCalSourcesFile(global.par.fovfile, &global.calfov, (int)extno))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get Field of View information.\n", global.taskname);
      goto get_end;
    }
  
  return OK;
  
 get_end:
   
  return NOT_OK;

}/* GetCalFOVInfo */


int GetVignettingInfo()
{

  int                   status = OK , hdutype=0;
  long                  extno=-1;
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
	  goto viginfo_end;
	}
      extno++;
    }
 
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.vigfile);


  if ((inunit=OpenReadFitsFile(global.par.vigfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.vigfile);
      goto viginfo_end;
    }

  if (extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(inunit, extno, &hdutype, &status) ) /* move to 2nd HDU */
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.vigfile);
	  goto viginfo_end;
	}
    }
  else
    {
      if (fits_movnam_hdu(inunit,ANY_HDU,KWVL_EXTNAME_VIGCOEF,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_VIGCOEF);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.vigfile);
	  goto viginfo_end;
	} 
    }


  head=RetrieveFitsHeader(inunit);
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;

  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.vigfile);
      goto viginfo_end;
    }
  
  if ((vigcol.PAR0=ColNameMatch(CLNM_PAR0, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto viginfo_end;
    }

  
  if ((vigcol.PAR1=ColNameMatch(CLNM_PAR1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto viginfo_end;
    }

  if ((vigcol.PAR2=ColNameMatch(CLNM_PAR2, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vigfile);
      goto viginfo_end;
    }

  EndBintableHeader(&head, &table);

  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  while(ReadBintable(inunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  global.viginfo.par0=(double)EVEC(table, n, vigcol.PAR0);
	  global.viginfo.par1=(double)EVEC(table, n, vigcol.PAR1);
	  global.viginfo.par2=(double)EVEC(table, n, vigcol.PAR2);
	  
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
      goto viginfo_end;
    }    
  
  return OK;

 viginfo_end:
  
  return NOT_OK;

}/* GetVignettingInfo */


int CreateInstrMap(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int amp)
{
  int                 status = OK, hdu_count=1, frame_indx, tmp_frame, tmp_totorbit, tmp_nset, count_hdu=0;
  int                 bitpix=0, naxis=0, nframe=0;
  int                 jj=0, xx=0, yy=0, first_frame=1;
  long                naxes[2], group, firstelem, nelements, extno;
  float               flt_frame=0.0, flt_count_frame=0.0,sens=0.0000001, ccd[CCD_ROWS][CCD_PIXS] ;
  double              hktime_diff=0.0;
  short int           ev_bad_flag;
  short int           ev_bad_flag_and;
  FitsFileUnit_t      inunit=NULL;              /* Input and Output fits file pointer */ 
  static short        window_mask[CCD_ROWS][CCD_PIXS] ;
  int 		      first_good_x,last_good_x,first_good_y,last_good_y, goody ;
  int                 rawx_min,rawy_min;
  double              skycx=0.0, skycy=0.0, mean_time, mjdref, rawcx, rawcy;
  int                 max_raw, max_pix;
  int                 RAWX_min=0, RAWX_max=0, RAWY_min=0, RAWY_max=0;
  float               *tmpccd;

  double              dx, dy, off, vcoeff=0.0;
  float               vcorr;


  TELDEF*    teldef;
  ATTFILE*   attfile;


  headas_chat(NORMAL, "%s: Info: Creating Raw Instrument Map.\n", global.taskname);

  if(global.evt_key.maxtemp > 0.0 - sens && global.evt_key.maxtemp < 0.0 + sens)
    {
      headas_chat(CHATTY,"%s: Info: %s keyword set to %f\n", global.taskname, KWNM_XRTMAXTP, global.evt_key.maxtemp);
      headas_chat(CHATTY,"%s: Info: the burned spot will be ignored.\n", global.taskname);
      CutBPFromTable(bp_table, (EV_BURNED_BP|EV_CAL_BP));
    }

  /* Create output primary array */
  if(fits_create_file(&inunit, global.par.outinstrfile, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto createmap_end;
    }

  bitpix=8;
  naxis=0;
  naxes[0]=0;
  naxes[1]=0;

  if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto createmap_end;
    }
  /* Added swift XRT keyword */
  hdu_count=1;
  if(AddEvtSwiftXRTKeyword(inunit, bitpix, amp,hdu_count))
    {
      headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto createmap_end;
    }
  
  /* write history is parameter history set */
  if(HDpar_stamp(inunit, 1, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto createmap_end;
    }
  hdu_count++;
  if(amp==AMP1)
    {
      global.calfov.detx-=1;
      global.calfov.dety-=1;
    }
  else if(amp==2)
    {
      global.calfov.detx=600 - global.calfov.detx;
      global.calfov.dety-=1;
    }
  else
    {
      headas_chat(NORMAL,"%s: Error: %d amplifier not allowed\n", global.taskname);      
    }

  tmp_nset=0;
  tmp_totorbit=0;


/* Compute  Window mask for current modes, PC or WT */ 


    /* Windowed Timing : */
  if(global.evt_key.xrtmode==XRTMODE_WT)
  {
    if ((global.evt_key.wm1stcol+global.evt_key.wmcolnum) > RAWX_WT_MAX ) {
        headas_chat(NORMAL, "%s: Error: In the %s file,\n", global.taskname, global.par.infile);
        headas_chat(NORMAL, "%s: Error: sum of %s and %s keywords exceeds max value.\n", global.taskname,KWNM_WM1STCOL, KWNM_WMCOLNUM);
	goto createmap_end;
    }
    /* Compute WT mask */ 
    rawx_min=RAWX_MIN;
    rawy_min=RAWY_MIN; 
    yy=0; 
    while(yy<CCD_ROWS){
      xx=0; 
      while (xx<CCD_PIXS){
	window_mask[yy][xx] = ( xx > global.evt_key.wm1stcol+1  &&  xx < global.evt_key.wm1stcol+global.evt_key.wmcolnum-1 ); 
	xx++;
      }
      /* set mask to zero OUTSIDE the interval */
      yy++ ; 
    }
  }     
    /* Photon counting: */
  else if(global.evt_key.xrtmode==XRTMODE_PC)
    {
      /* Compute PC mask */ 
      first_good_x=CCD_CENTERX - global.evt_key.whalfwd     + PC_BAD_FRAME_BORDERS; 
      last_good_x =CCD_CENTERX + global.evt_key.whalfwd -1  - PC_BAD_FRAME_BORDERS;
      first_good_y=CCD_CENTERY - global.evt_key.whalfht     + PC_BAD_FRAME_BORDERS;
      last_good_y =CCD_CENTERY + global.evt_key.whalfht -1  - PC_BAD_FRAME_BORDERS;

      for(yy=RAWY_MIN; yy<CCD_ROWS; yy++)
        {
	goody= ( yy>=first_good_y  &&  yy<=last_good_y ); /* set to zero OUTSIDE the interval */
	for (xx=RAWX_MIN; xx<CCD_PIXS; xx++)
          {
	    /* (zero anyway, if outside goody; 1 if inside interval)  */
	    window_mask[yy][xx] =  (! goody ) ? 0 : ( xx>=first_good_x  &&  xx<=last_good_x ) ;   
          }
        }
    }
    /* Else : */
  else
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.evt_key.xrtmode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s and %s\n", global.taskname, KWVL_DATAMODE_PH,KWVL_DATAMODE_TM);
      goto createmap_end;
    }

/* END _______________ Computed  Window mask for current mode, PC or WT */ 


  /* Open Attitude file */
  attfile = openAttFile(global.par.attfile);
  
  if(global.par.attinterpol)
    setAttFileInterpolation(attfile, 1);
  else
    setAttFileInterpolation(attfile, 0);
  
  
  /* Query CALDB to get teldef filename? */
  if (!(strcasecmp (global.par.teldef, DF_CALDB)))
    {
      extno=-1;
      
      if (CalGetFileName(HD_MAXRET, global.evt_key.dateobs, global.evt_key.timeobs, DF_NOW, DF_NOW, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for teldef file.\n", global.taskname);
	  goto createmap_end;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s'  CALDB file.\n", global.taskname, global.par.teldef);
    }
  
  /* Open TELDEF file */
  teldef = readTelDef(global.par.teldef);
  
  setSkyCoordCenterInTeldef(teldef, global.evt_key.tcrvl_x, global.evt_key.tcrvl_y);


  if(global.evt_key.xrtmode==XRTMODE_WT)
    {
      /* For WT Events in SETTLING mode the Number of frames used to create
	 instrument map ('wtnframe' input parameter) is forced to 1  */
      if(!strcmp(global.evt_key.datamode, KWVL_DATAMODE_TM) && !strcmp(global.evt_key.obsmode, KWVL_OBS_MODE_ST)){
	nframe=1;
	headas_chat(NORMAL, "%s: Info: input Event file in 'SETTLING' mode, the 'wtnframe' input parameter value will be ignored,\n", global.taskname);
	headas_chat(NORMAL, "%s: Info: using single frames to create intrument maps.\n", global.taskname);
      }else{
	nframe=global.par.wtnframe;
      }
    } 
  else
    {
      nframe=global.par.pcnframe;
    }

  /* for each extension [CHECK] */ 
  for(jj=0; jj < global.hkcount; )
    {
      if ((tmp_nset == 0 || !tmp_totorbit) && (tmp_nset < global.nsetframe))
	{
	  tmp_totorbit=global.framexorb[tmp_nset];
	  tmp_nset++;
	}
      	
      tmp_frame = (nframe==0) ? tmp_totorbit : nframe ;
      /*
      printf("TMP_ORBIT == %d\n", tmp_frame);
      */
      /* Initialize instrument map */ 
   
      for (xx=0; xx< CCD_PIXS; xx++)
	for(yy=0; yy<CCD_ROWS; yy++)
	  ccd[yy][xx]=0.0;
	        
      flt_count_frame=0.0;

            
/* for each frame in extension, subject to: [please list...!!] */ 
      for (frame_indx = 0; frame_indx < tmp_frame && tmp_totorbit && jj < global.hkcount; frame_indx++, tmp_totorbit--)
	{  
	  if(first_frame)
	    {
	      global.tmp_tstart = global.hkinfo[jj].hktime;
	      first_frame=0;
	    }
	  
	  hktime_diff = global.hkinfo[jj].hkendtime - global.hkinfo[jj].hktime;
	  
	  if((global.evt_key.xrtmode==XRTMODE_WT) && (hktime_diff < (global.evt_key.timedel - WTFRAME_DIFF )))
	    {
	      flt_frame=(float)hktime_diff/global.evt_key.timedel;
	    }
	  else
	    flt_frame=1.0;

	  flt_count_frame+=flt_frame;

	  global.tmp_tstop=global.hkinfo[jj].hkendtime;

          /* calculate the bad pixel flag */
          if((global.hkinfo[jj].temp > global.evt_key.maxtemp-sens) &&
             !((global.evt_key.maxtemp >= 0.0 - sens)&&(global.evt_key.maxtemp <= 0.0 + sens)))
            {
              ev_bad_flag_and = ~0;
            }
          else
            {
              ev_bad_flag_and = ~EV_BURNED_BP;
            }
	  for (xx=0; xx<CCD_PIXS; xx++)
	    {
	      for (yy=0; yy<CCD_ROWS; yy++)
		{
                  if(bp_table[xx][yy] != NULL)
                    {  
                      ev_bad_flag=bp_table[xx][yy]->bad_flag&ev_bad_flag_and;
                      if(ev_bad_flag == EV_CAL_BP)
                        ccd[yy][xx]+=flt_frame;  /* Add 1(unit) to ccd pixel's count if bad flag is only "burned" [CHECK] */
                    }
                  else
                    ccd[yy][xx]+=flt_frame;    /* Add 1(unit) to ccd pixel's count if it is not a bad pixel  */
		}/* end YY */
	      
	    }/* end XX */

	  jj++; /* NEXT frame [CHECK] */
	}

	  /*  Zero out-of-window pixels and out-of-FOV pixels */

	for (yy=0; yy<CCD_ROWS; yy++)
	{
	  for (xx=0; xx<CCD_PIXS; xx++)
	    {
	      if((xx-global.calfov.detx)*(xx-global.calfov.detx)+(yy-global.calfov.dety)*(yy-global.calfov.dety) <= global.calfov.r*global.calfov.r)
                {
                  ccd[yy][xx]=ccd[yy][xx]*window_mask[yy][xx];
                }
              else
                {
                  ccd[yy][xx]=0.0;
              }
	    }
	}
      /*
      for (xx=0; xx<CCD_PIXS; xx++)
	for (yy=0; yy<CCD_ROWS; yy++)
	  printf("%d ", ccd[yy][xx]);
      */


      /* Correct for Vignetting */

	for (yy=0; yy<CCD_ROWS; yy++)
	{
	  for (xx=0; xx<CCD_PIXS; xx++)
	    {

	      vcoeff = global.viginfo.par0*pow(global.viginfo.par1,global.par.energy)+global.viginfo.par2;

	      dx=xx - CCD_CENTERX;
	      dy=yy - CCD_CENTERY;
	      
	      off=(sqrt(dx*dx + dy*dy))*(PIXEL_SIZE)/60.0;
	      
	      vcorr=(float)(1.-vcoeff*(off)*(off));
	      
	      if (vcorr < 0.)  
		vcorr=0.;


	      ccd[yy][xx]=ccd[yy][xx]*vcorr ;
	    }
	}

	
      /* Compute ccd map dimension */
      
      skycx=global.region.X[0]; 
      skycy=global.region.Y[0];
      
      mean_time = (global.tmp_tstart+global.tmp_tstop)/2;
      mjdref = global.evt_key.mjdrefi +  global.evt_key.mjdreff;
      
      if (ComputeRawCoord(skycx, skycy, mean_time, mjdref, teldef, attfile, &rawcx, &rawcy))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to transform region X and Y to raw coordinates\n", global.taskname);
	  goto createmap_end;
	}

      headas_chat(CHATTY, "%s: Info: Image %i Center RAWX=%.4f RAWY=%.4f\n", global.taskname, count_hdu+1, rawcx, rawcy);
      
      if(!finite(rawcx) || !finite(rawcy) || rawcx > RAWX_MAX || rawcx < RAWX_MIN || rawcy > RAWY_MAX || rawcy < RAWY_MIN )
	{  
	  headas_chat(CHATTY, "%s: Error: source X=%f Y=%f is out of the CCD for time %18.8f\n", 
		      global.taskname, skycx, skycy, mean_time);
	  goto createmap_end;
	}
      
      
      if(!strcmp(global.region.shape, "BOX") || !strcmp(global.region.shape, "BOX-BOX"))
	{  
	  RAWX_min = floor(rawcx - global.region.R[0]/2 +0.5) - RAWMAP_DELTA;
	  RAWX_max = floor(rawcx + global.region.R[0]/2 +0.5) + RAWMAP_DELTA;
	  
	  RAWY_min = floor(rawcy - global.region.R[0]/2 +0.5) - RAWMAP_DELTA;
	  RAWY_max = floor(rawcy + global.region.R[0]/2 +0.5) + RAWMAP_DELTA;

	}
      else if(!strcmp(global.region.shape, "CIRCLE") || !strcmp(global.region.shape, "CIRCLE-CIRCLE"))
	{
	  RAWX_min = floor(rawcx - global.region.R[0] +0.5) - RAWMAP_DELTA;
	  RAWX_max = floor(rawcx + global.region.R[0] +0.5) + RAWMAP_DELTA;
	  
	  RAWY_min = floor(rawcy - global.region.R[0] +0.5) - RAWMAP_DELTA;
	  RAWY_max = floor(rawcy + global.region.R[0] +0.5) + RAWMAP_DELTA;
	  
	}
      else if(!strcmp(global.region.shape, "ANNULUS"))
	{
	  RAWX_min = floor(rawcx - global.region.R1[0] +0.5) - RAWMAP_DELTA;
	  RAWX_max = floor(rawcx + global.region.R1[0] +0.5) + RAWMAP_DELTA;
	  
	  RAWY_min = floor(rawcy - global.region.R1[0] +0.5) - RAWMAP_DELTA;
	  RAWY_max = floor(rawcy + global.region.R1[0] +0.5) + RAWMAP_DELTA;
	}


      if(RAWX_min<RAWX_MIN) RAWX_min=RAWX_MIN;
      if(RAWX_max>RAWX_MAX) RAWX_max=RAWX_MAX;  
      if(RAWY_min<RAWY_MIN) RAWY_min=RAWY_MIN;
      if(RAWY_max>RAWY_MAX) RAWY_max=RAWY_MAX;

      headas_chat(CHATTY, "%s: Info: RAWX_min %i RAWX_max %i RAWY_min %i RAWY_max %i\n", global.taskname,RAWX_min,RAWX_max,RAWY_min,RAWY_max);
      

      max_raw = RAWY_max - RAWY_min +1;
      max_pix = RAWX_max - RAWX_min +1;
      

      /* Allocate memory for new array and set values*/
      tmpccd = malloc(max_raw * max_pix * sizeof(float));

      for(yy=0; yy < max_raw; yy++)
	{  
	  for(xx=0; xx < max_pix; xx++)
	    {
	      tmpccd[(yy*max_pix)+xx]=ccd[yy+RAWY_min][xx+RAWX_min];
	    }
	}


      global.evt_key.ontime=flt_count_frame*global.evt_key.timedel;
      global.evt_key.xrtnfram=flt_count_frame;
      
      global.tmp_crval1 = floor(rawcx+0.5);
      global.tmp_crval2 = floor(rawcy+0.5);     
      global.tmp_crpix1 = global.tmp_crval1 - RAWX_min +1;
      global.tmp_crpix2 = global.tmp_crval2 - RAWY_min +1;  


      /*  Craete New Image */
      naxis=2;
      naxes[0]=max_pix;
      naxes[1]=max_raw;
      bitpix=-32;

      if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto createmap_end;
	}

      group=0;
      firstelem=1;
      nelements=max_raw*max_pix;

      /* Write ccd map into output file (global.par.outinstrfile) */

      if(fits_write_img_flt(inunit, group, firstelem, nelements, tmpccd, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write image extension\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto createmap_end;
	}

      count_hdu++;
      headas_chat(NORMAL, "%s: Info: Appended %d image in %s file\n", global.taskname, count_hdu, global.par.outinstrfile);
      
      if(AddEvtSwiftXRTKeyword(inunit, bitpix, amp, hdu_count))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto createmap_end;
	}
      
      first_frame=1;
      /* write history is parameter history set */
      if(HDpar_stamp(inunit, hdu_count, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto createmap_end;
	}      
      hdu_count++;
    }
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outinstrfile);
      goto createmap_end;
    }
  

  if(fits_close_file(inunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto createmap_end;
    }


  /* Close Attitude file */
  closeAttFile(attfile);


  return OK;
 createmap_end:
  return NOT_OK;
  
} /*CreateInstrMap*/

int AddEvtSwiftXRTKeyword(FitsFileUnit_t inunit, int bitpix, int amp, int jj)
{
  int    status=OK;
  int    tmp_int=0, logical=0;
  double tmp_dbl=0.0;

  if(fits_update_key(inunit, TSTRING, KWNM_TELESCOP,  KWVL_TELESCOP,CARD_COMM_TELESCOP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_INSTRUME,  KWVL_INSTRUME,CARD_COMM_INSTRUME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TINT,KWNM_MJDREFI , &global.evt_key.mjdrefi,CARD_COMM_MJDREFI, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE,KWNM_MJDREFF , &global.evt_key.mjdreff,CARD_COMM_MJDREFF, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_DATAMODE,  global.evt_key.datamode,CARD_COMM_DATAMODE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }


  if(fits_update_key(inunit, TSTRING, KWNM_OBS_MODE,  global.evt_key.obsmode,"Observation Mode", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_MODE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID  ,  global.evt_key.obsid,CARD_COMM_OBS_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TINT,KWNM_TARG_ID , &global.evt_key.target,CARD_COMM_TARG_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TARG_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  if(fits_update_key(inunit, TINT,KWNM_SEG_NUM , &global.evt_key.segnum,CARD_COMM_SEG_NUM, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_SEG_NUM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_OBJ,  &global.evt_key.raobj,CARD_COMM_RA_OBJ, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_OBJ);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_OBJ,  &global.evt_key.decobj,CARD_COMM_DEC_OBJ, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_OBJ);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_PNT,  &global.evt_key.rapnt,CARD_COMM_RA_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_PNT,  &global.evt_key.decpnt,CARD_COMM_DEC_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_XRA_PNT,  &global.evt_key.tcrvl_x,CARD_COMM_XRA_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XRA_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_XDEC_PNT,  &global.evt_key.tcrvl_y,CARD_COMM_XDEC_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XDEC_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }


  if(jj==1)
    {
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.evt_key.tstart,CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.evt_key.tstop,CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TSTRING, KWNM_DATEOBS,  global.evt_key.dateobs,CARD_COMM_DATEOBS, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEOBS);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_DATEEND,  global.evt_key.dateend,CARD_COMM_DATEEND, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEEND);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_PA_PNT,  &global.evt_key.pa_pnt,CARD_COMM_PA_PNT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_PA_PNT);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
    }

  if(jj > 1)
    {
      if(fits_update_key(inunit, TDOUBLE, KWNM_XRTMAXTP,  &global.evt_key.maxtemp,CARD_COMM_XRTMAXTP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XRTMAXTP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_XRTFRTIM,  &global.evt_key.timedel,CARD_COMM_XRTFRTIM, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMEDEL);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}   
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.tmp_tstart,CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.tmp_tstop,CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_ONTIME , &global.evt_key.ontime,CARD_COMM_ONTIME, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_ONTIME);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}


      if(fits_update_key(inunit, TFLOAT,KWNM_XRTNFRAM , &global.evt_key.xrtnfram,CARD_COMM_XRTNFRAM, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname,KWNM_XRTNFRAM );
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}


      if(fits_update_key(inunit, TSTRING, KWNM_CTYPE1,  KWVL_CTYPE1_RAW,CARD_COMM_CTYPE1, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CTYPE1);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_CRPIX1, &global.tmp_crpix1 ,CARD_COMM_CRPIX1, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRPIX1);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_CUNIT1, UNIT_PIXEL,CARD_COMM_CUNIT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CUNIT1);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TINT, KWNM_RAWXCEN, &global.tmp_crval1,CARD_COMM_RAWXCEN, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RAWXCEN);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TINT, KWNM_CRVAL1, &global.tmp_crval1,CARD_COMM_CRVAL1, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRVAL1);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      tmp_int=1;
      if(fits_update_key(inunit, TINT, KWNM_CDELT1, &tmp_int,CARD_COMM_CDELT1, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CDELT1);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      
      if(fits_update_key(inunit, TSTRING, KWNM_CTYPE2,  KWVL_CTYPE2_RAW,CARD_COMM_CTYPE2, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CTYPE2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_CRPIX2, &global.tmp_crpix2,CARD_COMM_CRPIX2, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRPIX2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_CUNIT2, UNIT_PIXEL,CARD_COMM_CUNIT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CUNIT2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TINT, KWNM_RAWYCEN, &global.tmp_crval2,CARD_COMM_RAWYCEN, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RAWYCEN);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

      if(fits_update_key(inunit, TINT, KWNM_CRVAL2, &global.tmp_crval2,CARD_COMM_CRVAL2, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CRVAL2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      tmp_int=1;
      if(fits_update_key(inunit, TINT, KWNM_CDELT2, &tmp_int,CARD_COMM_CDELT2, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_CDELT2);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      tmp_int=amp;
      if(fits_update_key(inunit, TINT, KWNM_AMP, &tmp_int,CARD_COMM_Amp, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_AMP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}
      
      tmp_dbl=2000.;
      if(fits_update_key(inunit, TDOUBLE, KWNM_EQUINOX,  &tmp_dbl,CARD_COMM_EQUINOX, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EQUINOX);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
	  goto addevt_end;
	}

    }/* end if(jj > 1) */


  logical=TRUE;
  if(fits_update_key(inunit, TLOGICAL, KWNM_VIGNAPP, &logical, CARD_COMM_VIGNAPP, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_XRTVIGEN, &global.par.energy, CARD_COMM_XRTVIGEN, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTVIGEN);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.outinstrfile);
      goto addevt_end;
    }


  return OK;
  
 addevt_end:
  return NOT_OK;

}


int StripExtensionWithStem(char *filename, char *stemin)
{
	char *tmp;

	if ((tmp = strstr(filename, stemin))){
	  *tmp = '\0';

	  return TRUE;
	}

	return FALSE;

} /* StripExtensionWithStem */
