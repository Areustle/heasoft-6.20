/*
 * 
 *	xrtinstrmap.c:
 *
 *	INVOCATION:
 *
 *		xrtinstrmap [parameter=value ...]
 *
 *	DESCRIPTION:
 *           
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 29/08/2005 - First version
 *        0.1.1 -    05/10/2005 - Implemented Field of view 
 *                                input file processing
 *                              - Using XRTMAXTP keyword value to get
 *                                temperature threshold for burning spot
 *                                instead of 'maxtemp' input parameter
 *        0.1.2 -    11/10/2005 - Read GTI extension to decide what 
 *                                PC frames use for instrument map
 *        0.1.3 -    12/10/2005 - Build intrument frame using group of frames
 *        0.1.4 -    17/10/2005 - Set BITPIX to 32 for images HDU instead of 8
 *        0.1.5 -    18/10/2005 - added TSTART and TSTOP keywords in output 
 *                                file 
 *        0.1.6 -    21/10/2005 - handle as bad pixels the bad pixels 3x3 and
 *                                check XRTHNFLG key for hot and flickering pixels
 *                                3x3
 *        0.1.7 -    25/10/2005 - Bug fixed
 *        0.1.8 -    26/10/2005 - Added EQUINOX keyword in output file
 *        0.1.9 -    27/10/2005 - Changed method to calculate ONTIME keyword value
 *        0.2.0 -    10/11/2005 - Changed algorithm to recognized end of orbit
 *                                using TIME and ENDTIME xhd.hk columns 
 *                              - Changed MJDREFI keyword datatype
 *        0.2.1 -    06/02/2006 - Added WT handling
 *                              - Added OBS_MODE, DATAMODE, OBS_ID and XRTNFRAM  keywords in 
 *                                output file
 *        0.2.2 -    27/02/2006 - Read 'CCDEXPOS' keyword instead of 'TIMEDEL' if
 *                                datamode is 'WT'
 *        0.2.3 -    07/03/2006 - Changed output images datatype to handle different 
 *                                duration of the WT frame 
 *        0.2.4 - AB 26/05/2006 - Masking of outside-window pixels implemented. 
 *        0.2.5 - FT 05/06/2006 - Alpha segmentation fault fixed
 *        0.2.6 - NS 19/01/2007 - Changed sens=0.00001 in ReadHK function
 *        0.2.7 -    17/04/2007 - Added TSTART TSTOP DATE-OBS DATE-END PA_PNT keywords
 *                                in output file
 *        0.2.8 -    22/05/2007 - Added 'checkattitude' input parameter
 *        0.2.9 -    11/12/2008 - Added XRA_PNT and XDEC_PNT keywords in instrument map header
 *        0.3.0 -    15/10/2009 - Modified to optimize the execution time
 *        0.3.1 -    21/04/2010 - Masking of pixels on window border implemented.
 *                              - Create instrument map also for WT events in SETTLING mode
 *        0.3.2 -    15/03/2011 - Modified selection of good frames while reading hkfile
 *        0.3.3 -    04/05/2011 - Increased tolerance in hk time check
 *        0.3.4 -    17/01/2014 - Added ATTFLAG keyword in output file
 *        0.3.5 -    17/02/2014 - Handle different type of the ATTFLAG keyword 
 *
 *
 *      NOTE:
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtinstrmap  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtinstrmap.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTINSTRMAP_C
#define XRTINSTRMAP_VERSION       "0.3.5"
#define PRG_NAME               "xrtinstrmap"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtinstrmap_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtinstrmap.par    
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
 *           void xrtinstrmap_info(void);
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
int xrtinstrmap_getpar(void)
{
  /*
   *  Get File name Parameters
   */
  
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

  /* Output File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  if(PILGetInt(PAR_NFRAME, &global.par.nframe)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NFRAME);
      goto Error;	
    }

  if(PILGetBool(PAR_CHECKATTITUDE, &global.par.checkattitude)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CHECKATTITUDE);
      goto Error;	
    }

  get_history(&global.hist);
  xrtinstrmap_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtinstrmap_getpar */

/*
 *	xrtinstrmap_work
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
int xrtinstrmap_work(void)
{
  int                   amp=0;
  static BadPixel_t     *bp_table[CCD_PIXS][CCD_ROWS];
  
  TMEMSET0( bp_table, bp_table);

  if(xrtinstrmap_checkinput())
    {
      goto Error;
    }
  
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

/* Enable this  as son as is separated out of "CreateInstrMap" 

  if(CreateWindowMask(global.key.datamode, window_mask))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create Window Mask for", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s mode .\n", global.taskname, global.key.datamode);
      goto Error;
    }
*/

  if(CreateInstrMap(bp_table, amp))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.outfile);
      goto Error;

    }
  headas_chat(NORMAL, "%s: Info: File '%s' successfully written.\n", global.taskname, global.par.outfile);


  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

 ok_end:
  if(global.hkcount)
    {
      free(global.hkinfo);
    }
  
  if(global.gticount)
    {
      free(global.gti);
    }

  return OK;

 Error:


  if(global.hkcount)
    {
      free(global.hkinfo);
    }

  if(global.gticount)
    {
      free(global.gti);
    }
  return NOT_OK;
} /* xrtinstrmap_work */

/*
 *	xrtinstrmap
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
 *             void xrtinstrmap_getpar(void);
 *             void xrtinstrmap_info(void); 
 * 	       void xrtinstrmap_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtinstrmap(void)
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTINSTRMAP_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  global.warning=0;
  /* Get parameter values */ 
  if ( xrtinstrmap_getpar() == OK ) {
    
    if ( xrtinstrmap_work() ) {
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


} /* xrtinstrmap */
/*
 *	xrtinstrmap_info: 
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
void xrtinstrmap_info (void)
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file             :'%s'\n",global.par.infile); 
  headas_chat(NORMAL,"Name of the input region file            :'%s'\n", global.par.fovfile);
  headas_chat(NORMAL,"Name of the input HK Header file         :'%s'\n",global.par.hdfile);
  headas_chat(NORMAL,"Name of the output instrument map file   :'%s'\n",global.par.outfile);
  if(global.par.checkattitude)
    headas_chat(NORMAL,"Check pointing stability?(yes/no)        :'yes'\n");
  else
    headas_chat(NORMAL,"Check pointing stability?(yes/no)        :'no'\n");
    if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                   : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                   : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtinstrmap_info */

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
  unsigned short     hkin10Arcm=0, hksettled=0;
  int                *framexorb_tmp;
  int                nsetframe_tmp, hkcount, jjj, frames_num, orbit_end; 
  double             * RA_array, RA_median, first_over_RA;
  double             * Dec_array, Dec_median, first_over_Dec, frame_dist, frame_over_dist;
  unsigned           over_distance;
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
           
      if(global.key.tstart < (card->u.DVal - tsens))
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
      
      if(global.key.tstop > (card->u.DVal + tsens))
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

	  if( hkmode != XRTMODE_BIAS )
	    {
	      if(!(first || ((tmp_time - tmp_endtime) < DIFF_TIME)))
		changeorbit=1;
	  
 	      
	      if(hkmode==global.xrtmode)
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
  
 
  /*  Check pointing stability */
  if(global.par.checkattitude && global.nsetframe)
    {
      framexorb_tmp = (int *)malloc(global.nsetframe*sizeof(int));
      nsetframe_tmp = global.nsetframe;


      for(iii=0 ; iii<global.nsetframe ; iii++)
	{
	  framexorb_tmp[iii]=global.framexorb[iii];
	}


      global.nsetframe = 0;
      free(global.framexorb);


      hkcount=0;

      /*  For each orbit found  */
      for(iii=0 ; iii<nsetframe_tmp ; iii++)
	{
  	  orbit_end=hkcount+framexorb_tmp[iii];
	  
	  RA_array=(double *)malloc(framexorb_tmp[iii]*sizeof(double));
	  Dec_array=(double *)malloc(framexorb_tmp[iii]*sizeof(double));
	  
	  for(jjj=hkcount ; jjj<orbit_end && jjj<global.hkcount ; jjj++)
	    {
	      RA_array[jjj-hkcount]=global.hkinfo[jjj].RA;
	      Dec_array[jjj-hkcount]=global.hkinfo[jjj].Dec;
	    }
	  
	  RA_median=CalcMedian(RA_array, framexorb_tmp[iii]);
	  Dec_median=CalcMedian(Dec_array, framexorb_tmp[iii]);
	  
	  headas_chat(CHATTY, "%s: Info: Orbit %d RA median = %f Dec median = %f\n", global.taskname, 
		      iii+1, RA_median, Dec_median);
	  
	  
	  free(RA_array);
	  free(Dec_array);
	  
	  frames_num=0;
	  over_distance=0;

	  /*  Check frame distance from RA and Dec median of frames within the orbit */
	  for(jjj=hkcount ; jjj<orbit_end && jjj<global.hkcount ; jjj++)
	    {  
	      frames_num++;


	      if(over_distance) 
		{
		  frame_over_dist=pow(pow((global.hkinfo[jjj].RA-first_over_RA)*cos((3.1415927/180.)*(global.hkinfo[jjj].Dec+first_over_Dec)/2), 2) + pow(global.hkinfo[jjj].Dec-first_over_Dec, 2), 0.5);
		  
		  frame_over_dist=frame_over_dist*3600;
		  
		  if(frame_over_dist>WTFRAME_DIST)
		    {
		      headas_chat(CHATTY, "%s: Info: CCDFrame %d distance from first CCDFrame %f arcsec > threshold\n", 
				  global.taskname, global.hkinfo[jjj].hkccdframe, frame_over_dist);
		      
		      if(global.nsetframe)
			{
			  global.nsetframe+=1;
			  global.framexorb = (int *)realloc(global.framexorb, ((global.nsetframe)*sizeof(int)));		      
			}
		      else
			{
			  global.nsetframe+=1;
			  global.framexorb = (int *)malloc(sizeof(int));
			}
		      
		      global.framexorb[global.nsetframe-1]=frames_num-1;
		      frames_num=1;
		      over_distance=0;
		      
		      
		      frame_dist=pow(pow((global.hkinfo[jjj].RA-RA_median)*cos((3.1415927/180.)*(global.hkinfo[jjj].Dec+Dec_median)/2), 2)
				     +pow(global.hkinfo[jjj].Dec-Dec_median, 2), 0.5);

		      frame_dist=frame_dist*3600;
		      
		      
		      if(frame_dist>WTFRAME_DIST)
			{
			  headas_chat(CHATTY, "%s: Info: CCDFrame %d distance from median %f arcsec > distance threshold %f arcsec\n", 
				      global.taskname, global.hkinfo[jjj].hkccdframe, frame_dist, WTFRAME_DIST);

			  first_over_RA=global.hkinfo[jjj].RA;
			  first_over_Dec=global.hkinfo[jjj].Dec;
			  
			  headas_chat(CHATTY, "%s: Info: CCDFrame %d -> first CCDFrame over distance threshold\n", 
				      global.taskname, global.hkinfo[jjj].hkccdframe);
			  
			  over_distance=1;
			  
			}
		    }
		  else
		    {
		      headas_chat(CHATTY, "%s: Info: CCDFrame %d distance from first CCDFrame %f arcsec\n",
				  global.taskname, global.hkinfo[jjj].hkccdframe, frame_over_dist);
		    }
		}
	      else
		{
		  frame_dist=pow(pow((global.hkinfo[jjj].RA-RA_median)*cos((3.1415927/180.)*(global.hkinfo[jjj].Dec+Dec_median)/2), 2)
				 +pow(global.hkinfo[jjj].Dec-Dec_median, 2), 0.5);
		  
		  frame_dist=frame_dist*3600;
		  
		  
		  if(frame_dist>WTFRAME_DIST)
		    {
		      headas_chat(CHATTY, "%s: Info: CCDFrame %d distance from median %f arcsec > distance threshold %f arcsec\n", 
				  global.taskname, global.hkinfo[jjj].hkccdframe, frame_dist, WTFRAME_DIST);
		      
		      first_over_RA=global.hkinfo[jjj].RA;
		      first_over_Dec=global.hkinfo[jjj].Dec;
		      
		      headas_chat(CHATTY, "%s: Info: CCDFrame %d -> first CCDFrame over distance threshold\n", 
				  global.taskname, global.hkinfo[jjj].hkccdframe);
		      
		      over_distance=1;
		      
		    }
		}
	    }

	  if(frames_num)
	    {
	      if(global.nsetframe)
		{
		  global.nsetframe+=1;
		  global.framexorb = (int *)realloc(global.framexorb, ((global.nsetframe)*sizeof(int)));		      
		}
	      else
		{
		  global.nsetframe+=1;
		  global.framexorb = (int *)malloc(sizeof(int));
		}
	      
	      
	      global.framexorb[global.nsetframe-1]=frames_num;
	      frames_num=0;
	    }

	  hkcount=orbit_end;

	}

      free(framexorb_tmp);

    } /*  if(global.par.checkattitude && ..) */



   return OK;

 ReadHK_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* ReadHK */


int ReadEventFile(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int *amp)
{
  int                   status=OK, rawx=0, rawy=0, count, ext, iii=0; 
  int                   neigh_x[] = {NEIGH_X}, jj;
  int                   neigh_y[] = {NEIGH_Y};
  char		        KeyWord[FLEN_KEYWORD];
  BPCol_t               bpcol;
  EvtCol_t              evtcol;
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
      goto end_func;
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
      goto end_func;
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
	sprintf(global.key.attflag, "%li", card->u.JVal);
	break;
      case I:	/* unsigned integer */
	sprintf(global.key.attflag, "%d", card->u.IVal);
	break;
      default: 	/* string */
	strcpy(global.key.attflag, card->u.SVal);
	break;
      }
    }
  else
    {
      strcpy(global.key.attflag, DEFAULT_ATTFLAG);
      headas_chat(NORMAL,"%s: Warning: 'ATTFLAG' keyword not found in %s\n", global.taskname, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value %s\n", global.taskname, DEFAULT_ATTFLAG);     
    }

  if((ExistsKeyWord(&evhead, KWNM_XRTMAXTP  , &card)))
    {
      global.key.maxtemp=card->u.DVal;
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: %s keyword not found\n", global.taskname,KWNM_XRTMAXTP);
      headas_chat(CHATTY, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
      global.key.maxtemp=0.0;
      headas_chat(CHATTY, "%s: Info: using %f default value for temperature threshold for burned spot.\n", global.taskname, global.key.maxtemp);
      
    }

  if((ExistsKeyWord(&evhead, KWNM_OBS_ID  , &card)))
    {
      strcpy(global.key.obsid, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_MJDREFI  , &card)))
    {
      global.key.mjdrefi=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  

  if((ExistsKeyWord(&evhead, KWNM_MJDREFF  , &card)))
    {
      global.key.mjdreff=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TARG_ID  , &card)))
    {
      global.key.target=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TARG_ID);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_SEG_NUM  , &card)))
    {
      global.key.segnum=card->u.JVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_SEG_NUM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_OBS_MODE  , &card)))
    {
      strcpy(global.key.obsmode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DATAMODE  , &card)))
    {
      strcpy(global.key.datamode, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  /* For WT Events in SETTLING mode the Number of frames used to create
     instrument map ('nframe' input parameter) is forced to 1  */
  if(!strcmp(global.key.datamode, KWVL_DATAMODE_TM) && !strcmp(global.key.obsmode, KWVL_OBS_MODE_ST))
    {
      global.par.nframe = 1;
      headas_chat(NORMAL, "%s: Info: input Event file in 'SETTLING' mode, the 'nframe' input parameter value will be ignored,\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: using single frames to create intrument maps.\n", global.taskname);
    }

  if(!strcmp(global.key.datamode, KWVL_DATAMODE_PH))
    global.xrtmode=XRTMODE_PC;
  else if(!strcmp(global.key.datamode, KWVL_DATAMODE_TM))
    global.xrtmode=XRTMODE_WT;
  else
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.key.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s and %s\n", global.taskname, KWVL_DATAMODE_PH,KWVL_DATAMODE_TM);
      /* Close input event file */
      if (CloseFitsFile(evunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	}
      
      goto end_func;
    }
     
/* RQS: se PC => leggi KWNM_WHALFWD... */

    /* Windowed Timing : */
  if(global.xrtmode==XRTMODE_WT)
  {
    /* Get mask structure */
    if((ExistsKeyWord(&evhead, KWNM_WM1STCOL  , &card))  && ((card->u.JVal >= RAWX_WT_MIN) && (card->u.JVal <=  WM1STCOL_MAX)))
      global.wm1stcol=card->u.JVal;
    else
      {
	headas_chat(NORMAL, "%s: Error: the %s keyword value is out of range\n", global.taskname, KWNM_WM1STCOL);
	goto end_func;
      }
    if((ExistsKeyWord(&evhead, KWNM_WMCOLNUM  , &card)) && ((card->u.JVal >= WT_NCOL_MIN) && (card->u.JVal <= WT_NCOL_MAX)))
        global.wmcolnum=card->u.JVal;
    else
      {
	headas_chat(NORMAL, "%s: Error: the %s keyword value is out of range\n", global.taskname, KWNM_WMCOLNUM);
	goto end_func;
      }
  }     
    /* Photon counting: */
  else if(global.xrtmode==XRTMODE_PC)
    {
    /* Get mask structure */
      if((ExistsKeyWord(&evhead, KWNM_WHALFWD  , &card)) && ((CCD_CENTERX-card->u.JVal >= RAWX_MIN) && (CCD_CENTERX+card->u.JVal-1 <= RAWX_MAX)) )
	global.whalfwd=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: the %s keyword not found or  out of range\n", global.taskname, KWNM_WHALFWD);
	  goto end_func;
	}
      if((ExistsKeyWord(&evhead, KWNM_WHALFHT  , &card)) && ((CCD_CENTERY-card->u.JVal >= RAWY_MIN) && (CCD_CENTERY+card->u.JVal-1 <= RAWY_MAX)) )
	global.whalfht=card->u.JVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: the %s keyword not found or  out of range\n", global.taskname, KWNM_WHALFHT);
	  goto end_func;
	}

    }

  if((ExistsKeyWord(&evhead, KWNM_RA_OBJ  , &card)))
    {
      global.key.raobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DEC_OBJ  , &card)))
    {
      global.key.decobj=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&evhead, KWNM_RA_PNT  , &card)))
    {
      global.key.rapnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_RA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_DEC_PNT  , &card)))
    {
      global.key.decpnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEC_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TSTART  , &card)))
    {
      global.key.tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if((ExistsKeyWord(&evhead, KWNM_TSTOP  , &card)))
    {
      global.key.tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  

  if(global.xrtmode==XRTMODE_PC)
    {
      if(ExistsKeyWord(&evhead, KWNM_TIMEDEL, &card))
	{
	  
	  global.key.timedel=card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: %s keyword not found\n", global.taskname, KWNM_TIMEDEL);
	  headas_chat(NORMAL, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Info: using %f default value.\n", global.taskname, PC_TIMEDEL);
	  global.key.timedel=PC_TIMEDEL;
	}

    }
  else
    {

      if(ExistsKeyWord(&evhead, "CCDEXPOS", &card))
	{
	  
	  global.key.timedel=card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: CCDEXPOS keyword not found\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Info: using 1.0675 default value.\n", global.taskname);
	  global.key.timedel=1.0675;
	}
    }

  if(ExistsKeyWord(&evhead, KWNM_XRTHNFLG, &card))
    {
      
      global.key.xrthnflg=card->u.LVal;
    }
  else
    {
      global.key.xrthnflg=0;
    }

  if((ExistsKeyWord(&evhead, KWNM_DATEOBS  , &card)))
    {
      strcpy(global.key.dateobs, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&evhead, KWNM_DATEEND  , &card)))
    {
      strcpy(global.key.dateend, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  if((ExistsKeyWord(&evhead, KWNM_PA_PNT, &card)))
    {
      global.key.pa_pnt= card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_PA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }


  GetBintableStructure(&evhead, &table, BINTAB_ROWS, 0, NULL);


  if ((evtcol.X=ColNameMatch(CLNM_X, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }

  if ((evtcol.Y=ColNameMatch(CLNM_Y, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }

  sprintf(KeyWord, "TCRVL%d", (evtcol.X+1));
  if((ExistsKeyWord(&evhead, KeyWord, &card)))
    {
      global.key.tcrvl_x = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
    }

  sprintf(KeyWord, "TCRVL%d", (evtcol.Y+1));
  if((ExistsKeyWord(&evhead, KeyWord, &card)))
    {
      global.key.tcrvl_y = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KeyWord);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto end_func;
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
      goto end_func;
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
      goto end_func;
    }
  
  
  if ((bpcol.RAWY=ColNameMatch(CLNM_RAWY, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if ((bpcol.Amp=ColNameMatch(CLNM_Amp, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if ((bpcol.TYPE=ColNameMatch(CLNM_TYPE, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TYPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  if ((bpcol.BADFLAG=ColNameMatch(CLNM_BADFLAG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BADFLAG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }
  if ((bpcol.YEXTENT=ColNameMatch(CLNM_XYEXTENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XYEXTENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
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
		  goto end_func;
		}


	      if(((~(OBS_HOT_BP|OBS_FLICK_BP))&pix.bad_flag) || global.key.xrthnflg)
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
			  goto end_func;
			}
		 
		      if(((~(OBS_HOT_BP|OBS_FLICK_BP))&pix.bad_flag) || global.key.xrthnflg)
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
      goto end_func;
    } 
  
  /* Get extension header pointer  */
  evhead=RetrieveFitsHeader(evunit);  

  
  
  GetBintableStructure(&evhead, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      goto end_func;
    }
  global.gticount=table.MaxRows;
  /* Get needed columns number from name */
  
  if ((gticol.START=ColNameMatch(CLNM_START, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_START);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
    }
  
  
  if ((gticol.STOP=ColNameMatch(CLNM_STOP, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto end_func;
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
	      goto end_func;
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
      goto end_func;
    }
    
  return OK; 
   
 end_func:
  return NOT_OK;
}/* ReadEventFile */


/*  IMPLEMENT THE FOLLOWING AS A STAND_ALONE ROUTINE 

int CreateWindowMask(char *datamode, short *window_mask[CCD_ROWS][CCD_PIXS])
{

 
  Creates a mask for PC and WT mode
 

  if(!strcmp(datamode, KWVL_DATAMODE_PH))
    DO SOMETHING
  else if(!strcmp(datamode, KWVL_DATAMODE_TM))
        DO SOMETHING
  else
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.key.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s and %s\n", global.taskname, KWVL_DATAMODE_PH,KWVL_DATAMODE_TM);
       --Close input event file 
      if (CloseFitsFile(evunit))
        {
          headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
          headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
        }
      
      return NOT_OK;
    }

  return OK;
}
-- CreateWindowMask 
*/
int CreateInstrMap(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int amp)
{
  int                 status = OK, hdu_count=1, frame_indx, tmp_frame, tmp_totorbit, tmp_nset, count_hdu=0;
  int                 bitpix=0, naxis=0;
  int                 jj=0, xx=0, yy=0, first_frame=1;
  long                naxes[2], group, firstelem, nelements;
  float               flt_frame=0.0, flt_count_frame=0.0,sens=0.0000001, ccd[CCD_ROWS][CCD_PIXS] ;
  double              hktime_diff=0.0;
  short int           ev_bad_flag;
  short int           ev_bad_flag_and;
  FitsFileUnit_t      inunit=NULL;              /* Input and Output fits file pointer */ 
  static short        window_mask[CCD_ROWS][CCD_PIXS] ;
  int 		      first_good_x,last_good_x,first_good_y,last_good_y, goodx,goody ;
  int                 rawx_min,rawy_min;

  if(global.key.maxtemp > 0.0 - sens && global.key.maxtemp < 0.0 + sens)
    {
      headas_chat(CHATTY,"%s: Info: %s keyword set to %f\n", global.taskname, KWNM_XRTMAXTP, global.key.maxtemp);
      headas_chat(CHATTY,"%s: Info: the burned spot will be ignored.\n", global.taskname);
      CutBPFromTable(bp_table, (EV_BURNED_BP|EV_CAL_BP));
    }

  /* Create output primary array */
  if(fits_create_file(&inunit, global.par.outfile, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }

  bitpix=8;
  naxis=0;
  naxes[0]=0;
  naxes[1]=0;

  if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }
  /* Added swift XRT keyword */
  hdu_count=1;
  if(AddSwiftXRTKeyword(inunit, bitpix, amp,hdu_count))
    {
      headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }
  
  /* write history is parameter history set */
  if(HDpar_stamp(inunit, 1, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto create_end;
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
  if(global.xrtmode==XRTMODE_WT)
  {
    if ((global.wm1stcol+global.wmcolnum) > RAWX_WT_MAX ) {
        headas_chat(NORMAL, "%s: Error: In the %s file,\n", global.taskname, global.par.infile);
        headas_chat(NORMAL, "%s: Error: sum of %s and %s keywords exceeds max value.\n", global.taskname,KWNM_WM1STCOL, KWNM_WMCOLNUM);
	goto create_end;
    }
    /* Compute WT mask */ 
    rawx_min=RAWX_MIN;
    rawy_min=RAWY_MIN; 
    yy=0; 
    while(yy<CCD_ROWS){
      xx=0; 
      while (xx<CCD_PIXS){
	window_mask[yy][xx] = ( xx > global.wm1stcol+1  &&  xx < global.wm1stcol+global.wmcolnum-1 ); 
      xx++;
      }
	/* set mask to zero OUTSIDE the interval */
    yy++ ; 
    }
  }     
    /* Photon counting: */
  else if(global.xrtmode==XRTMODE_PC)
    {
      /* Compute PC mask */ 
      first_good_x=CCD_CENTERX - global.whalfwd     + PC_BAD_FRAME_BORDERS; 
      last_good_x =CCD_CENTERX + global.whalfwd -1  - PC_BAD_FRAME_BORDERS;
      first_good_y=CCD_CENTERY - global.whalfht     + PC_BAD_FRAME_BORDERS;
      last_good_y =CCD_CENTERY + global.whalfht -1  - PC_BAD_FRAME_BORDERS;

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
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.xrtmode);
      headas_chat(CHATTY,"%s: Error: Valid datamode are:  %s and %s\n", global.taskname, KWVL_DATAMODE_PH,KWVL_DATAMODE_TM);
      goto create_end;

    }

/* END _______________ Computed  Window mask for current mode, PC or WT */ 


/* for each extension [CHECK] */ 
  for(jj=0; jj < global.hkcount; )
    {
      if ((tmp_nset == 0 || !tmp_totorbit) && (tmp_nset < global.nsetframe))
	{
	  tmp_totorbit=global.framexorb[tmp_nset];
	  tmp_nset++;
	}
      
      tmp_frame = (global.par.nframe==0) ? tmp_totorbit : global.par.nframe ;
      /*
      printf("TMP_ORBIT == %d\n", tmp_frame);
      */
      /* Initialize instrument map */ 
   
      for (xx=0; xx< CCD_PIXS; xx++)
	for(yy=0; yy<CCD_ROWS; yy++)
	  ccd[yy][xx]=0.0;
	        

      flt_count_frame=0.0;
      naxis=2;
      naxes[0]=600;
      naxes[1]=600;
      bitpix=-32;

      if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	  goto create_end;
	}

/* for each frame in extension, subject to: [please list...!!] */ 
      for (frame_indx = 0; frame_indx < tmp_frame && tmp_totorbit && jj < global.hkcount; frame_indx++, tmp_totorbit--)
	{
	  
	  if(first_frame)
	    {
	      global.tmp_tstart = global.hkinfo[jj].hktime;
	      first_frame=0;
	    }

	  
	  hktime_diff = global.hkinfo[jj].hkendtime - global.hkinfo[jj].hktime;
	  
	  if((global.xrtmode==XRTMODE_WT) && (hktime_diff < (global.key.timedel - WTFRAME_DIFF )))
	    {
	      flt_frame=(float)hktime_diff/global.key.timedel;
	    }
	  else
	    flt_frame=1.0;

	  flt_count_frame+=flt_frame;

	  global.tmp_tstop=global.hkinfo[jj].hkendtime;

          /* calculate the bad pixel flag */
          if((global.hkinfo[jj].temp > global.key.maxtemp-sens) &&
             !((global.key.maxtemp >= 0.0 - sens)&&(global.key.maxtemp <= 0.0 + sens)))
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

      group=0;
      firstelem=1;
      nelements=360000;
      global.key.ontime=flt_count_frame*global.key.timedel;
      global.key.xrtnfram=flt_count_frame;
      

      /* Write ccd map into output file (global.par.outfile) */

      if(fits_write_img_flt(inunit, group, firstelem, nelements, ccd[0], &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write image extension\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto create_end;
	}

      count_hdu++;
      headas_chat(NORMAL, "%s: Info: Appended %d image in %s file\n", global.taskname, count_hdu, global.par.outfile);
      
      if(AddSwiftXRTKeyword(inunit, bitpix, amp, hdu_count))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	  goto create_end;
	}
      
      first_frame=1;
      /* write history is parameter history set */
      if(HDpar_stamp(inunit, hdu_count, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto create_end;
	}      
      hdu_count++;
    }
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n", global.taskname, global.par.outfile);
      goto create_end;
    }
  

  if(fits_close_file(inunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
      goto create_end;
    }


  return OK;
 create_end:
  return NOT_OK;
  
} /*CreateInstrMap*/

int xrtinstrmap_checkinput(void)
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
  
  
  return OK;

 error_check:
  return NOT_OK;


}/* xrtinstrmap_checkinput */
int AddSwiftXRTKeyword(FitsFileUnit_t inunit, int bitpix, int amp, int jj)
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
  
  if(fits_update_key(inunit, TINT,KWNM_MJDREFI , &global.key.mjdrefi,CARD_COMM_MJDREFI, &status))
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

  if(fits_update_key(inunit, TSTRING, KWNM_DATAMODE,  global.key.datamode,CARD_COMM_DATAMODE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }


  if(fits_update_key(inunit, TSTRING, KWNM_OBS_MODE,  global.key.obsmode,"Observation Mode", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_MODE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID  ,  global.key.obsid,CARD_COMM_OBS_ID, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }
  
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

  if(fits_update_key(inunit, TDOUBLE, KWNM_XRA_PNT,  &global.key.tcrvl_x,CARD_COMM_XRA_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XRA_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_XDEC_PNT,  &global.key.tcrvl_y,CARD_COMM_XDEC_PNT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XDEC_PNT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_ATTFLAG, global.key.attflag, CARD_COMM_ATTFLAG, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_ATTFLAG);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto add_end;
    }

  if(jj==1)
    {
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.key.tstart,CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.key.tstop,CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}

      if(fits_update_key(inunit, TSTRING, KWNM_DATEOBS,  global.key.dateobs,CARD_COMM_DATEOBS, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEOBS);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TSTRING, KWNM_DATEEND,  global.key.dateend,CARD_COMM_DATEEND, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEEND);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_PA_PNT,  &global.key.pa_pnt,CARD_COMM_PA_PNT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_PA_PNT);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
    }

  if(jj > 1)
    {
      if(fits_update_key(inunit, TDOUBLE, KWNM_XRTMAXTP,  &global.key.maxtemp,CARD_COMM_XRTMAXTP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XRTMAXTP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}

      if(fits_update_key(inunit, TDOUBLE, KWNM_XRTFRTIM,  &global.key.timedel,CARD_COMM_XRTFRTIM, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMEDEL);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}   
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTART , &global.tmp_tstart,CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_TSTOP , &global.tmp_tstop,CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
      
      if(fits_update_key(inunit, TDOUBLE,KWNM_ONTIME , &global.key.ontime,CARD_COMM_ONTIME, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_ONTIME);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}


      if(fits_update_key(inunit, TFLOAT,KWNM_XRTNFRAM , &global.key.xrtnfram,CARD_COMM_XRTNFRAM, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname,KWNM_XRTNFRAM );
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}


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
      tmp_int=amp;
      if(fits_update_key(inunit, TINT, KWNM_AMP, &tmp_int,CARD_COMM_Amp, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_AMP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}
      
      tmp_dbl=2000.;
      if(fits_update_key(inunit, TDOUBLE, KWNM_EQUINOX,  &tmp_dbl,CARD_COMM_EQUINOX, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EQUINOX);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
	  goto add_end;
	}


    }

      return OK;

 add_end:
      return NOT_OK;

}

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


/*
 *  CalcMedian
 *
 *  DESCRIPTION:
 *    Routine to Calculate the median value for array of double
 *
 */
double CalcMedian(double * array, int length) 
{
  int mod=0, jj, iii;
  double * median;
  double medianval, diff_db, level;

  if(length<=0)
    return 0;

  median=(double *)malloc(length*sizeof(double));

  for (jj=0; jj< length; jj++)
    {
      level=array[jj];
      
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
  return medianval;

} /* CalcMedian */
