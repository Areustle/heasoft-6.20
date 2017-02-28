/*
 * 
 *	xrthkproc.c:
 *
 *	INVOCATION:
 *
 *		xrthkproc [parameter=value ...]
 *
 *	DESCRIPTION:
 *           Routine to compute photon arrival time photons 
 *
 *	DOCUMENTATION:
 *        
 *           XRT-PSU-037 "XRT Science Algorithms"
 * 
 *	CHANGE HISTORY:
 *           0.1.0 - BS 17/11/2003 - First version
 *           0.1.1 -    25/03/2004 - Deleted files list handling
 *           0.1.2 -    07/04/2004 - Added 'HKTIME' column
 *           0.1.3 -    26/04/2004 - Added routines to compute source DETX/Y starting from
 *                                   RA and Dec
 *           0.1.4 -    06/05/2004 - Modified input parameters description and some input 
 *                                   parameters name
 *           0.1.5 -    13/05/2004 - Messages displayed revision
 *           0.1.6 -    28/05/2004 - Used "HDget_frac_time" routine to read 
 *                                   MJDREF keyword
 *           0.1.7 -    24/06/2004 - Bug fixed on 'fits_update_key' call for
 *                                   LOGICAL keyword
 *           0.1.8 -    08/07/2004 - Added chance to input DEFAULT in outfile parameter
 *           0.1.9 -    23/07/2004 - Renamed 'hkfile' parameter in 'hdfile'
 *                                 - Updated TSTART and TSTOP keywords in 'FRAME' HDU
 *                                   when tag times
 *                                 - Added one empty row with TIME(n) == ENDTIME(n-1)
 *                                   to consider the last frame stop time when GTI are built
 *           0.1.10-    03/08/2004 - Bug fixed in TimeTag routine when add the empty row
 *           0.1.11-    10/09/2004 - Added check on XRTTIMES keyword before adding the empty row      
 *           0.1.12-    24/09/2004 - Added check on XRT OBSMODE and if it is in SLEW or In10arcmin
 *                                   using ccd center detx/dety to tag time
 *                                 - Added warning message if source is out of ccd
 *           0.1.13- FT 01/10/2004 - Added additional check on xrtmode to line 898 to set the TIME of
 *                                   the last record equal to the TSTOP of the previous one
 *                                   only if the last record is the dummy record added by the task
 *           0.1.14- RP 14/10/2004 - Change parameter type in computeDetCoord function
 *           0.1.15- FT 14/10/2004 - During 10arcmin compute start/end frame using source coordinates
 *           0.2.1 - BS 24/11/2004 - Modified to handle WT first frame
 *           0.2.2 -    30/11/2004 - Added test for infinity or not-a-number on  calculated coordinates 
 *                                 - Using only valid times (and not -1) when update TSTART and TSTOP keywords in 'FRAME' HDU
 *                                 - Displayed a warning message if all TIME and ENDTIME columns
 *                                   for TIMING frames are set to NULL value
 *           0.2.3 -    20/12/2004 - Process all data modes in the same way
 *           0.2.4 -    18/01/2005 - Bug fixed on noslew variable
 *           0.2.5 -    24/01/2005 - Modified method to calculate time-tag
 *                                 - Added routine to set to NULL not tagged times
 *           0.2.6 -    03/02/2005 - Added 'attinterpol' and aberration' input parameter
 *                                 - Modified algorithm to calculate time row for WT and time pix for PDs
 *           0.2.7 -    04/02/2005 - Added check on TimePix return value
 *           0.2.8 -    07/02/2005 - Bug fixed
 *           0.2.9 -    09/02/2005 - Added routine to handle PD variable frames dimension
 *           0.3.0 -    04/03/2005 - Added chance to tag time using srcdetx/y
 *           0.3.1 -    24/03/2005 - Minor changes
 *           0.3.2 - FT 30/03/2005 - Bug Fixed on Alpha (XRTDETX/Y keyword)
 *           0.3.3 - BS 05/04/2005 - Filled the last row if added
 *           0.3.4 -    30/06/2005 - Subtracted a read row time every photon for WT mode
 *                                  (XRT Calibration Meeting - 28/29 june 2005 Leicester)
 *           0.3.5 -    01/07/2005 - Displayed src detx/y only if the task calculates them
 *           0.3.6 -    04/07/2005 - Set to NULL dummy last row.
 *           0.3.7 -    04/07/2005 - Bug Fixed on Alpha (unsigned long column)
 *           0.3.8 -    13/07/2005 - Replaced ReadBintable with ReadBintableWithNULL
 *                                   to perform check on udefined value reading hdfile
 *                                 - Changed to avoid segmentation fault on Alpha
 *                                   adding the dummy row
 *           0.3.9 -               - Added a comment
 *           0.3.10-    09/08/2005 - Exit with error if hdfile is empty
 *           0.3.11-    12/10/2005 - Bug fixed
 *           0.4.0 -    28/11/2005 - Bug fixed when fills the last row for the 
 *                                   first time
 *           0.4.1 - NS 21/09/2006 - Do not use the ENDTIME record value to update 
 *                                   the TSTOP keyword if the frame has XRTMode set 
 *                                   to 1 (telemetry xrt bias bogus mode)
 *           0.4.2 -    24/10/2006 - Query to CALDB for teldef file with 'DATE-OBS' and 'TIME-OBS'
 *                                   of input hd file
 *                                 - TSTART and TSTOP update in all extensions of hd file
 *           0.4.3 -    12/03/2007 - Settled=1 In10Arcm=0 InSafeM=1 is considered SLEW mode
 *           0.4.4 -    30/09/2008 - InSafeM=1 is considered SLEW mode
 *           0.4.5 -    16/01/2014 - Added check of the 'ATTFLAG' keyword of the input attitude file
 *           0.4.6 -    17/02/2014 - Modified check of the 'ATTFLAG' keyword of the input attitude file
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrthkproc  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrthkproc.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTHKPROC_C
#define XRTHKPROC_VERSION     "0.4.6"
#define PRG_NAME              "xrthkproc"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrthkproc_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrthkproc.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void xrthkproc_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrthkproc_getpar()
{
  /*
   *  Get File name Parameters
   */
  
  /* Input Event List Files Name */
  if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
      goto Error;
    }

  /* Output File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;
    }
  
  if(PILGetInt(PAR_SRCDETX, &global.par.srcdetx)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETX);
      goto Error;	
    }
      
  if(PILGetInt(PAR_SRCDETY, &global.par.srcdety)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETY);
      goto Error;	
    }  

  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
    {
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
      
      if(PILGetBool(PAR_ATTINTERPOL, &global.par.attinterpol))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ATTINTERPOL);
	  goto Error;
	}
      
      if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_ABERRATION);
	  goto Error;
	}
    }
  
  get_history(&global.hist);

  xrthkproc_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrthkproc_getpar */

/*
 *	xrthkproc_work
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
 *           pid_t getpid(void);
 *           int sprintf(char *str, const char *format, ...);
 *           char *DeriveFileName(const char *OldName, char *NewName, const char *ext);
 *           BOOL FileExists(const char *FileName);
 *           FitsFileUnit_t OpenWriteFitsFile(char *name);
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype,
 *                               int *status ); 
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status); 
 *           int TimeTag(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 *           size_t strlen(const char *s);
 *	
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 15/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrthkproc_work()
{
  int            status = OK, evExt, inExt, outExt;
  int            logical;
  char           attflag[FLEN_KEYWORD];
  double         sens=0.0000001;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL;

  TMEMSET0( &head, FitsHeader_t );
 
  global.tstart=-1.;
  global.tstop=-1.;

  
  if(xrthkproc_checkinput())
    goto Error;
  

  /* Check ATTFLAG keyword value of the attitude file */

  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
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


  /* Open input hk file */
  if ((inunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Move in FRAME extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_FRM, 0, &status))
    {  
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile); 
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	} 
      goto Error;
	 
    }
    
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,inunit,global.par.hdfile,global.taskname);
  headas_chat(NORMAL,"%s: Info: Processing '%s' file.\n",global.taskname,global.par.hdfile );        
    
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.hdfile);
      goto Error;
    }
    

  global.mjdref=HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);

  if(status)
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }

  
  /* Create new file */
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
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Copy all extensions before events from input to output file */
  outExt=1;
  
  while(outExt<evExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.hdfile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
      
      outExt++;
    }
  
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Time tag events */
  if ((Timetag(inunit, outunit)))
    {
      headas_chat(NORMAL, "%s: Error: Unable to calculate photon arrival times.\n", global.taskname);
      goto Error;
    }
  
  outExt++;
  /* copy any extensions after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.hdfile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpfile);
	  goto Error;
	}
      
      outExt++;
    }
  
  
  /* move to events extension */
  if(fits_movabs_hdu( outunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Update time keywords in events extension */
  
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTTIMES, &logical, CARD_COMM_XRTTIMES, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword.\n", global.taskname, KWNM_XRTTIMES);
      goto Error;
    }
  
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }
  
  /* Update TSTART and TSTOP in all extensions  */

  for(outExt=1; outExt<= inExt; outExt++){

    if(fits_movabs_hdu( outunit, outExt, NULL, &status ))
      {
	headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.hdfile);
	goto Error;
      }
    if(global.tstart >= (0. - sens))  
      if(fits_update_key(outunit, TDOUBLE, KWNM_TSTART, &global.tstart, CARD_COMM_TSTART, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s keyword in HDU %d.\n", global.taskname, KWNM_TSTART, outExt);
	  goto Error;
	}
    if(global.tstop >= 0. - sens)  
      if(fits_update_key(outunit, TDOUBLE, KWNM_TSTOP, &global.tstop, CARD_COMM_TSTOP, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s keyword in HDU %d.\n", global.taskname, KWNM_TSTOP, outExt);
	  goto Error;
	}
  }
  
  /* Add checksum and datasum in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM.\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
  
  /* close files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto Error;
    }
  
  /* Close output fits file */
  if (!CloseFitsFile(outunit))
    headas_chat(CHATTY,"%s: Info: File '%s' successfully written.\n",global.taskname,
		global.par.outfile );
  else
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto Error;
    }
  
  /*Rename temporary file */
  if(rename (global.tmpfile, global.par.outfile) == -1)
    {
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto Error;
    }
  
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);
  
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Info: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  
  return OK;
   
 Error:
  
  if (FileExists(global.tmpfile))
    remove(global.tmpfile);
  
  return NOT_OK;
} /* xrthkproc_work */

/*
 *	Timetag
 *
 *
 *	DESCRIPTION:
 *           Routine to compute photons time tag.
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int strcmp(const char *s1, const char *s2);
 *             int headas_chat(int ,char *, ...);
 *             void  GetGMTDateTime(char *);
 *             int sprintf(char *str, const char *format, ...);
 *
 *             void AddHistory(FitsHeader_t *header, const char *Comment);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 15/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int Timetag(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  int                amp=0, xrtmode=0, totrow=0;
  int                sizex, sizey, detx=0, dety=0, first=1, count_null=0;
  int                ccdframe, last_xrtmode, tot_offset=0;
  int                jj,status=OK,elem_i;
  long               extno, elem_j;
  float              elem_flt, null_flt;
  unsigned           null_uns, elem_uns;
  unsigned long      elem_v, null_v;
  BOOL               xrttimes=0, first_frame=1, use_detxy=0;
  BOOL               first_tag=1, first_timing=0;
  JTYPE              j_null;
  BTYPE              byt_null;
  char		     KeyWord[FLEN_KEYWORD];
  double             time_row = 0., time_tag=0., fet=0., fst=0., time_pix=0.;
  double             roll, pixsize, src_x, src_y, null_dbl;
  double             sens=0.0000001, elem_dbl;
  Coord_t            coordset;
  HKCol_t            indxcol;
  unsigned           FromRow, ReadRows, n=0,nCols, OutRows=0;  
  FitsCard_t         *card;
  Bintable_t	     intable;
  FitsHeader_t	     head;
  unsigned short     noslew=0, settled, in10arcm, insafeM;

  TELDEF* teldef;
  ATTFILE* attfile;

  TMEMSET0( &intable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  head=RetrieveFitsHeader(evunit);
  
  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
    {
      use_detxy=0;
      /* Open Attitude file */
      attfile = openAttFile(global.par.attfile);
      if(global.par.attinterpol)
	setAttFileInterpolation(attfile, 1);
      else
	setAttFileInterpolation(attfile, 0);

       /* Get nominal RA */
      if(global.par.ranom < RA_MIN)
	{
	  if((ExistsKeyWord(&head, KWNM_RA_PNT  , &card)))
	    global.par.ranom=card->u.DVal;
	  else if ((ExistsKeyWord(&head, KWNM_RA_NOM  , &card)))
	    global.par.ranom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n", global.taskname,KWNM_RA_NOM, KWNM_RA_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_RANOM);
	      goto timetag_end;
	    }
	} 

      if (global.par.ranom > RA_MAX || global.par.ranom < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal RA value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
	  goto timetag_end;
	}


      /* Get nominal Dec */
      if(global.par.decnom < DEC_MIN)
	{
	  if((ExistsKeyWord(&head, KWNM_DEC_PNT  , &card)))
	    global.par.decnom=card->u.DVal;
	  else if((ExistsKeyWord(&head, KWNM_DEC_NOM  , &card)))
	    global.par.decnom=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n", global.taskname,KWNM_DEC_NOM, KWNM_DEC_PNT);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_DECNOM);
	      goto timetag_end;
	    }
	}


      if (global.par.decnom > DEC_MAX || global.par.decnom < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Nominal DEC value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto timetag_end;
	}
      
      /* Get source RA */
      if(global.par.srcra < RA_MIN)
	{
	  if((ExistsKeyWord(&head, KWNM_RA_OBJ  , &card)))
	    global.par.srcra=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keywords not found\n", global.taskname,KWNM_RA_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_SRCRA);
	      goto timetag_end;
	    }
	} 
      
      if (global.par.srcra > RA_MAX || global.par.srcra < RA_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source RA value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: The Valid RA range is:  %d - %d\n", global.taskname, RA_MIN, RA_MAX);
	  goto timetag_end;
	}


      /* Get nominal Dec */
      if(global.par.srcdec < DEC_MIN)
	{
      
	  if((ExistsKeyWord(&head, KWNM_DEC_OBJ  , &card)))
	    global.par.srcdec=card->u.DVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s and %s keywords not found\n", global.taskname,KWNM_DEC_OBJ);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      headas_chat(NORMAL, "%s: Error: Please use %s input parameter.\n", global.taskname, PAR_SRCDEC);
	      goto timetag_end;
	    }
	}
      
      
      if (global.par.srcdec > DEC_MAX || global.par.srcdec < DEC_MIN)
	{
	  headas_chat(NORMAL, "%s: Error: Source DEC value is out of valid range.\n", global.taskname);
	  headas_chat(NORMAL, "%s: Info: The Valid DEC range is:  %d - %d\n", global.taskname, DEC_MIN, DEC_MAX);
	  goto timetag_end;
	}

     
      /* Query CALDB to get teldef filename? */
      if (!(strcasecmp (global.par.teldef, DF_CALDB)))
	{

	  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
	    {
	      global.dateobs=card->u.SVal;
	      if(!(strchr(global.dateobs, 'T')))
		{
		  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
		    global.timeobs=card->u.SVal;
		  else
		    {
		      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
		      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
		      goto timetag_end;
		    }
		}
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	      goto timetag_end;
	      
	    }

	  if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extno))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto timetag_end;
	    }
	  else
	    headas_chat(NORMAL, "%s: Info: Processing '%s'  CALDB file.\n", global.taskname, global.par.teldef);
	}
      
      /* Open TELDEF file */
      teldef = readTelDef(global.par.teldef);

      setSkyCoordCenterInTeldef(teldef, global.par.ranom, global.par.decnom);


      /* Compute source X/Y */
      sizex=SIZE_X;
      sizey=SIZE_Y;
      roll=270.;

      pixsize=PIX_SIZE_DEG;
      
      if(RADec2XY(global.par.srcra, global.par.srcdec, sizex, sizey, global.par.ranom, global.par.decnom, roll, pixsize, &src_x, &src_y))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to calculate source SKY coordinates.\n",global.taskname);
	  goto timetag_end;
	} 

    }
  else
    use_detxy=1;

  /* Check if times are already computed */
  if((ExistsKeyWord(&head, KWNM_XRTTIMES  , NULLNULLCARD)) && (GetLVal(&head, KWNM_XRTTIMES)))
    {
      headas_chat(NORMAL, "%s: Info: Photon arrival times were already calculated.\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: The %s and %s columns will be overwritten.\n", global.taskname, CLNM_TIME, CLNM_ENDTIME);
      xrttimes=1;
    }
      
  GetBintableStructure(&head, &intable, BINTAB_ROWS, 0, NULL);
  if(!intable.MaxRows)
    {
      headas_chat(NORMAL,"%s: Error: %s file is empty\n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }


  nCols=intable.nColumns;
  
  /* Get cols index from name */
  if((indxcol.FrameHID=ColNameMatch(CLNM_FrameHID, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FrameHID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FrameHID_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FrameHID+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    } 
 
  if((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_CCDFRAME_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.CCDFrame+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    } 
  if((indxcol.TARGET=ColNameMatch("TargetID", &intable)) == -1)
    {
      if((indxcol.TARGET=ColNameMatch(CLNM_TARGET, &intable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TARGET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto timetag_end;
	}
    }
  if(indxcol.TARGET != -1)
    {
      j_null=KWVL_TARGET_NULL ;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.TARGET+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.ObsNum=ColNameMatch(CLNM_ObsNum   , &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ObsNum);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null= KWVL_ObsNum_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.ObsNum+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.RA=ColNameMatch(CLNM_RA, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.Dec=ColNameMatch(CLNM_Dec, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Dec);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.Roll=ColNameMatch(CLNM_Roll, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Roll);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.Settled=ColNameMatch(CLNM_Settled, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }

  if((indxcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.InSAA=ColNameMatch(CLNM_InSAA, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSAA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.InSafeM=ColNameMatch(CLNM_InSafeM, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.XRTAuto=ColNameMatch(CLNM_XRTAuto, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTAuto);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.XRTManul=ColNameMatch(CLNM_XRTManul, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTManul);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.XRTRed=ColNameMatch(CLNM_XRTRed, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTRed);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  if((indxcol.XRTMode=ColNameMatch(CLNM_XRTMode, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_XRTMode_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.XRTMode+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.WaveID=ColNameMatch(CLNM_WaveID, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WaveID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_WaveID_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.WaveID+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.ErpCntRt=ColNameMatch(CLNM_ErpCntRt, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ErpCntRt);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.XPosTAM1=ColNameMatch(CLNM_XPosTAM1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XPosTAM1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.YPosTAM1=ColNameMatch(CLNM_YPosTAM1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YPosTAM1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.XPosTAM2=ColNameMatch(CLNM_XPosTAM2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XPosTAM2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.YPosTAM2=ColNameMatch(CLNM_YPosTAM2, &intable)) == -1)			 
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_YPosTAM2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.DNCCDTemp=ColNameMatch(CLNM_DNCCDTemp, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DNCCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_DNCCDTemp_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.DNCCDTemp+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.Vod1=ColNameMatch(CLNM_Vod1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vod1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vod2=ColNameMatch(CLNM_Vod2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vod2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vrd1=ColNameMatch(CLNM_Vrd1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vrd1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.Vrd2=ColNameMatch(CLNM_Vrd2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vrd2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vog1=ColNameMatch(CLNM_Vog1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vog1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vog2=ColNameMatch(CLNM_Vog2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vog2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.S1Rp1=ColNameMatch(CLNM_S1Rp1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S1Rp1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.S1Rp2=ColNameMatch(CLNM_S1Rp2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S1Rp2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.S1Rp3=ColNameMatch(CLNM_S1Rp3, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S1Rp3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.R1pR=ColNameMatch(CLNM_R1pR, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_R1pR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.R2pR=ColNameMatch(CLNM_R2pR, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_R2pR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.S2Rp1=ColNameMatch(CLNM_S2Rp1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S2Rp1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.S2Rp2=ColNameMatch(CLNM_S2Rp2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S2Rp2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.S2Rp3=ColNameMatch(CLNM_S2Rp3, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S2Rp3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vgr=ColNameMatch(CLNM_Vgr, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vgr);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vsub=ColNameMatch(CLNM_Vsub, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vsub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vbackjun=ColNameMatch(CLNM_Vbackjun, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vbackjun);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Vid=ColNameMatch(CLNM_Vid, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Vid);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.Ip1=ColNameMatch(CLNM_Ip1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Ip1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Ip2=ColNameMatch(CLNM_Ip2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Ip2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Ip3=ColNameMatch(CLNM_Ip3, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Ip3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Sp1=ColNameMatch(CLNM_Sp1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Sp1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.Sp2=ColNameMatch(CLNM_Sp2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Sp2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.Sp3=ColNameMatch(CLNM_Sp3, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Sp3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.CpIG=ColNameMatch(CLNM_CpIG, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CpIG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.BaseLin1=ColNameMatch(CLNM_BaseLin1, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BaseLin1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }

  if((indxcol.BaseLin2=ColNameMatch(CLNM_BaseLin2, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BaseLin2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }

  if((indxcol.FSTS=ColNameMatch(CLNM_FSTS, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FSTS_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FSTS+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FSTSub=ColNameMatch(CLNM_FSTSub, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FSTSub_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FSTSub+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FETS=ColNameMatch(CLNM_FETS, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FETS_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FETS+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FETSub=ColNameMatch(CLNM_FETSub, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FETSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FETSub_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FETSub+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.CCDExpo=ColNameMatch(CLNM_CCDEXPOS, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDEXPOS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_CCDEXPOS_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.CCDExpo+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.CCDExpSb=ColNameMatch(CLNM_CCDEXPSB, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDEXPSB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_CCDEXPSB_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.CCDExpSb+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.EvtLLD=ColNameMatch(CLNM_EVTLLD, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EVTLLD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_EVTLLD_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.EvtLLD+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.PixGtLLD=ColNameMatch(CLNM_PixGtLLD, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixGtLLD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_PixGtLLD_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PixGtLLD+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.EvtULD=ColNameMatch(CLNM_EvtULD, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_EvtULD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_EvtULD_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.EvtULD+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.PixGtULD=ColNameMatch(CLNM_PixGtULD, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixGtULD);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_PixGtULD_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PixGtULD+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.SplitThr=ColNameMatch(CLNM_SPLITTHR, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SPLITTHR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_SPLITTHR_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.SplitThr+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.OuterThr=ColNameMatch(CLNM_OuterThr, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OuterThr);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_OuterThr_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.OuterThr+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.SnglePix=ColNameMatch(CLNM_SnglePix, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SnglePix);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_SnglePix_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.SnglePix+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.SngleSpl=ColNameMatch(CLNM_SngleSpl, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SngleSpl);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_SngleSpl_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.SngleSpl+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.ThreePix=ColNameMatch(CLNM_ThreePix, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ThreePix);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_ThreePix_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.ThreePix+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FourPix=ColNameMatch(CLNM_FourPix, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FourPix);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_FourPix_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FourPix+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.WinHalfW=ColNameMatch(CLNM_WinHalfW, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WinHalfW);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_WinHalfW_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.WinHalfW+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.WinHalfH=ColNameMatch(CLNM_WinHalfH, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_WinHalfH);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_WinHalfH_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.WinHalfH+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.Amp=ColNameMatch(CLNM_Amp, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_Amp_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.Amp+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.telemRow=ColNameMatch(CLNM_telemRow, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_telemRow);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_telemRow_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.telemRow+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.telemCol=ColNameMatch(CLNM_telemCol, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_telemCol);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_telemCol_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.telemCol+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.nPixels=ColNameMatch(CLNM_nPixels, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_nPixels);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_nPixels_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.nPixels+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.BaseLine=ColNameMatch(CLNM_BASELINE, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BASELINE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_BASELINE_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.BaseLine+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.PixOverF=ColNameMatch(CLNM_PixOverF, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixOverF);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }

  else
    {
      j_null=KWVL_PixOverF_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PixOverF+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.PixUnder=ColNameMatch(CLNM_PixUnder, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PixUnder);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_PixUnder_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PixUnder+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }

  if((indxcol.ENDTIME=ColNameMatch(CLNM_ENDTIME, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENDTIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }


  if((indxcol.BiasExpo=ColNameMatch(CLNM_BiasExpo, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BiasExpo);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_BiasExpo_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.BiasExpo+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.CCDTemp=ColNameMatch(CLNM_CCDTemp, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  

  if((indxcol.LRHPixCt=ColNameMatch(CLNM_LRHPIXCT, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_LRHPIXCT);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in new format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_LRHPIXCT_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.LRHPixCt+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.LRBPixCt=ColNameMatch(CLNM_LRBPixCt, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LRBPixCt);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  else
    {
      j_null=KWVL_LRBPixCt_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.LRBPixCt+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }
  

  if((indxcol.LRSumBPx=ColNameMatch(CLNM_LRSumBPx, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LRSumBPx);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  
  if((indxcol.LRSoSBPx=ColNameMatch(CLNM_LRSoSBPx, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LRSoSBPx);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.LRBiasPx=ColNameMatch(CLNM_LRBiasPx, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_LRBiasPx);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in new format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_LRBiasPx_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.LRBiasPx+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.LRBiasLv=ColNameMatch(CLNM_LRBiasLv, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LRBiasLv);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
 
  if((indxcol.LRNoise=ColNameMatch(CLNM_LRNoise, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LRNoise);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto timetag_end;
    }
  
  if((indxcol.EVTLOST=ColNameMatch(CLNM_EVTLOST, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_EVTLOST);
      headas_chat(NORMAL, "%s: Info: in '%s' file. \n", global.taskname, global.par.hdfile);
      
    }
 
  if((indxcol.PDBIASTHR=ColNameMatch(CLNM_PDBIASTHR, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_PDBIASTHR);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_PDBIASTHR_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PDBIASTHR+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.PDBIASLVL=ColNameMatch(CLNM_PDBIASLVL, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_PDBIASLVL);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  
  if((indxcol.LDPNUM=ColNameMatch(CLNM_LDPNUM, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_LDPNUM);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_LDPNUM_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.LDPNUM+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.LDPPAGE=ColNameMatch(CLNM_LDPPAGE, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_LDPPAGE);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_LDPPAGE_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.LDPPAGE+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }


  if((indxcol.HPixCt=ColNameMatch(CLNM_HPIXCT, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_HPIXCT);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_HPIXCT_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.HPixCt+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }



  if((indxcol.BiasPx=ColNameMatch(CLNM_BiasPx, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_BiasPx);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
 else
   {
     j_null=KWVL_BiasPx_NULL;
     /* Add TNULL keyword */
     sprintf(KeyWord, "TNULL%d", (indxcol.BiasPx+1));
     AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
   }

  if((indxcol.IMBLvl=ColNameMatch(CLNM_IMBLVL, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname,CLNM_IMBLVL);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_IMBLVL_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.IMBLvl+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FCWSCol=ColNameMatch(CLNM_FCWSCol, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_FCWSCol);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_FCWSCol_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FCWSCol+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FCWSRow=ColNameMatch(CLNM_FCWSRow, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_FCWSRow);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_FCWSRow_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FCWSRow+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FCWNCols=ColNameMatch(CLNM_FCWNCols, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_FCWNCols);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_FCWNCols_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FCWNCols+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FCWNRows=ColNameMatch(CLNM_FCWNRows, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_FCWNRows);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_FCWNRows_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FCWNRows+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.FCWThres=ColNameMatch(CLNM_FCWThres, &intable)) == -1)
    {
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_FCWThres);
      headas_chat(NORMAL, "%s: Info: the %s file\n",  global.taskname, global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: is in old format.\n",  global.taskname);
    }
  else
    {
      j_null=KWVL_FCWThres_NULL;
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.FCWThres+1));
      AddCard(&head, KeyWord, J, &j_null, CARD_COMM_TNULL);
    }

  if((indxcol.HKTIME=ColNameMatch(CLNM_HKTIME, &intable)) == -1)
    {
      AddColumn(&head, &intable,CLNM_HKTIME,CARD_COMM_HKTIME, "D",TNONE);
      headas_chat(NORMAL, "%s: Info: '%s' column does not exist\n", global.taskname, CLNM_HKTIME);
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_HKTIME);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile); 
      indxcol.HKTIME=ColNameMatch(CLNM_HKTIME, &intable);
    }
  
  /*****D*/
  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
    {
      if(ExistsKeyWord(&head, KWNM_XRTDETX, NULL))
	DeleteCard(&head, KWNM_XRTDETX);
      if(ExistsKeyWord(&head, KWNM_XRTDETY, NULL))
	DeleteCard(&head, KWNM_XRTDETY);

      AddCard(&head, KWNM_XRA_OBJ, D, &global.par.srcra, CARD_COMM_XRA_OBJ);
      AddCard(&head, KWNM_XDEC_OBJ, D, &global.par.srcdec, CARD_COMM_XDEC_OBJ);
      AddCard(&head, KWNM_XRA_PNT, D, &global.par.ranom, CARD_COMM_XRA_PNT);
      AddCard(&head, KWNM_XDEC_PNT, D, &global.par.decnom, CARD_COMM_XDEC_PNT);


    }
  else
    {

      JTYPE jdetx, jdety;

      if(ExistsKeyWord(&head, KWNM_XRA_OBJ,NULL))
	DeleteCard(&head, KWNM_XRA_OBJ);
      if(ExistsKeyWord(&head, KWNM_XDEC_OBJ,NULL))
	DeleteCard(&head, KWNM_XDEC_OBJ);
      if(ExistsKeyWord(&head, KWNM_XRA_PNT,NULL))
	DeleteCard(&head, KWNM_XRA_PNT);
      if(ExistsKeyWord(&head, KWNM_XDEC_PNT,NULL))
	DeleteCard(&head, KWNM_XDEC_PNT);


      jdetx = global.par.srcdetx;
      jdety = global.par.srcdety;

      AddCard(&head, KWNM_XRTDETX, J, &jdetx, CARD_COMM_XRTDETX);
      AddCard(&head, KWNM_XRTDETY, J, &jdety, CARD_COMM_XRTDETY);
     
    }
 /* Add history */
 GetGMTDateTime(global.date);
 if(global.hist)
   {
     sprintf(global.strhist, "File modified by '%s' (%s) at %s: derived exposure time for timing modes. ", global.taskname, global.swxrtdas_v,global.date );
     AddHistory(&head, global.strhist);
     
     if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
       {
	 sprintf(global.strhist, "Used following file:");
	 AddHistory(&head, global.strhist);
	 sprintf(global.strhist, "%s teldef CALDB file", global.par.teldef);
	 AddHistory(&head, global.strhist);
	 sprintf(global.strhist, "%s attitude file", global.par.attfile);
	 AddHistory(&head, global.strhist);
       }
   }
 
 if(!xrttimes)
   {
     sprintf(global.strhist, "Last row is added to consider the last frame stop time when GTI are built");
     AddHistory(&head, global.strhist);
   }

 EndBintableHeader(&head, &intable); 
 
 /* write out new header to new file */
 FinishBintableHeader(ounit, &head, &intable);
 
 FromRow = 1;
 ReadRows=intable.nBlockRows;
 OutRows = 0;


 /* Read input bintable and compute ranges time-tag */
 while(ReadBintableWithNULL(evunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 )
   {
     for(n=0; n<ReadRows; ++n)
       {
	 
	 if(first_frame)
	   {
	     last_xrtmode=-1;
	     first_frame=0;
	   }
	 else
	   {
	     last_xrtmode=xrtmode;
	   }

	 xrtmode=BVEC(intable, n, indxcol.XRTMode);
	 ccdframe=VVEC(intable, n, indxcol.CCDFrame);

	 /*printf("CCDFRAME==%d\n", ccdframe);*/ 

	 fst=(VVEC(intable, n, indxcol.FSTS) + ((20.*UVEC(intable, n, indxcol.FSTSub))/1000000.));
	 fet=(VVEC(intable, n, indxcol.FETS) + ((20.*UVEC(intable, n, indxcol.FETSub))/1000000.));

	 noslew=0;
	 settled=XVEC1BYTE(intable, n, indxcol.Settled);
	 in10arcm=XVEC1BYTE(intable, n, indxcol.In10Arcm);
	 insafeM=XVEC1BYTE(intable, n, indxcol.InSafeM);

/*  	 noslew = settled; */
/*  	 if ( xrtmode == XRTMODE_PU || xrtmode == XRTMODE_LR ) */
	   /* if ( settled || in10arcm ) noslew=1; */
	 if ((!settled && !in10arcm)||(insafeM)){
	   noslew=0;
	 }
	 else{
	   noslew=1;
	 }

	 totrow++; 
	 if(!xrttimes || (xrttimes && totrow != intable.MaxRows))
	   DVEC(intable, n, indxcol.HKTIME) = fet - 0.5;

	 /* Compute source detx and dety coordinates if needed */

	  if(xrtmode == XRTMODE_WT || xrtmode == XRTMODE_PU || xrtmode == XRTMODE_LR)
	    {
	      if(!first_timing)
		first_timing=1;

	      if (intable.TagVec[indxcol.Amp] == B)
		amp=BVEC(intable, n,indxcol.Amp);
	      else
		amp=IVEC(intable, n,indxcol.Amp);
	      if (amp != AMP1 && amp != AMP2)
		{
		  headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
		  headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
		  goto timetag_end;
		}
	      
	      
	      
	      if(noslew && !use_detxy)
		{
		  if(ComputeDetCoord(src_x, src_y, amp, fst, teldef, attfile, &coordset))
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to calculate source DET coordinates.\n", global.taskname);
		      goto timetag_end;
		    }
		  

		  
		  if(!finite(coordset.detx) || !finite(coordset.dety) || coordset.detx > DETX_MAX || coordset.detx < DETX_MIN ||
		     coordset.dety > DETY_MAX || coordset.dety < DETY_MIN )
		    {


		      /*printf("DETX==%5.5f DETY==%5.5f \n\n", coordset.detx,coordset.dety);*/
		      headas_chat(NORMAL, "%s: Warning: The source is out of the CCD for the frame number %4d\n", global.taskname, ccdframe); 
		      detx = KWVL_DETNULL;
		      dety = KWVL_DETNULL;
		    }
		  else
		    {
		      /*printf("DETX==%5.5f DETY==%5.5f \n\n", coordset.detx,coordset.dety);*/
		      detx=(int)(coordset.detx + 0.5);
		      dety=(int)(coordset.dety + 0.5);
		    }
		  
		}
	      else if(!noslew)
		{
		  detx=(int)CENTER_DETX;
		  dety=(int)CENTER_DETY;
		  headas_chat(CHATTY, " %s: Warning: Using the detector coordinates of the center of the CCD\n", global.taskname);
 		  headas_chat(CHATTY, " %s: Warning: for frames taken during SLEW or In10Arcmin\n", global.taskname);   
		}
	      else
		{
		  detx=global.par.srcdetx;
		  dety=global.par.srcdety;
		} 
	    }

	  /*printf(" <<<< detx = %d dety = %d slew = %u\n", detx, dety, slew);*/

	  if(xrtmode == XRTMODE_WT)
	    {
	           	      
	      if(detx == KWVL_DETNULL || dety == KWVL_DETNULL )
		{
		  headas_chat(NORMAL, "%s: Warning: Unable to calculate times for this frame.\n", global.taskname);
		  DVEC(intable, n, indxcol.TIME) = -1.; 
		  DVEC(intable, n, indxcol.ENDTIME) = -1.;
		  count_null++;
		}
	      else
		{
		  if(first_tag)
		    first_tag=0;
		  if(noslew && !use_detxy)
		    {
		      headas_chat (CHATTY, "%s: Info: Windowed Timing source DETY calculated for %d ccdframe is: %d\n", global.taskname, ccdframe,dety);
		    } 
		  TimeRow(fst, fet, &time_row);
		  if(last_xrtmode != xrtmode)
		    WTTimeTagFirstFrame(fst, time_row, &time_tag);
		  else
		    WTTimeTag(fst, WT_RAWY_MIN,dety,time_row,&time_tag);
	
		  DVEC(intable, n, indxcol.TIME) = (time_tag - 0.5*time_row - time_row) ;

		  WTTimeTag(fst, WT_RAWY_MAX,dety,time_row,&time_tag);
		  DVEC(intable, n, indxcol.ENDTIME) = (time_tag +0.5*time_row - time_row) ;
		  
		}
	    }
	  else if (xrtmode == XRTMODE_PU || xrtmode == XRTMODE_LR)
	    {
	      
	      if( xrtmode == XRTMODE_PU)
		{
		  tot_offset=JVEC(intable, n,indxcol.nPixels);
		  
		}
	      else 
		{
		  CalculateNPixels(fst, fet, &tot_offset);
		  
		}
	      
	      if(detx == KWVL_DETNULL || dety == KWVL_DETNULL )
		{
		  headas_chat(NORMAL, "%s: Warning: Unable to calculate times for this frame.\n", global.taskname);
		  DVEC(intable, n, indxcol.TIME) = -1.; 
		  DVEC(intable, n, indxcol.ENDTIME) = -1.;
		  count_null++;		   
		}
	      else
		{
		  if(first_tag)
		    first_tag=0;
		  if(noslew && !use_detxy)
		    {
		      headas_chat (CHATTY, "%s: Info: Photodiode source DETX calculated for %d ccdframe is: %d.\n", global.taskname, ccdframe,detx);
		      headas_chat (CHATTY, "%s: Info: Photodiode source DETY calculated for %d ccdframe is: %d.\n", global.taskname, ccdframe,dety);
		    }
		  /* Calculate TIME */
		  if(TimePix(fst, fet, tot_offset, &time_pix))
		    {
		      headas_chat (CHATTY, "%s: Error: Unable to calculate pixel read time.\n", global.taskname);
		      goto timetag_end;
		    }

		  PDTimeTag(amp,fst, FIRST_OFFSET,detx, dety,time_pix, &time_tag);
		  DVEC(intable, n, indxcol.TIME) = time_tag;
		  /* Calculate ENDTIME */
		  PDTimeTag(amp,fst, (tot_offset -1),detx, dety,time_pix, &time_tag);
		  DVEC(intable, n, indxcol.ENDTIME) = time_tag;
		  
		}
	    }
	  
	  if(first && (DVEC(intable, n, indxcol.TIME) >= 0.-sens ))
	    {
	      global.tstart=DVEC(intable, n, indxcol.TIME);
	      first=0;
	    }
	  
	  if(xrttimes && totrow == intable.MaxRows)
	    {
	      DVEC(intable, n, indxcol.TIME) = global.tstop;
	      /*
		XVEC1BYTE(intable, n,indxcol.Settled)=0;
		XVEC1BYTE(intable, n,indxcol.In10Arcm)=0;
		XVEC1BYTE(intable, n,indxcol.InSAA)=0;
		XVEC1BYTE(intable, n,indxcol.InSafeM)=0;
		XVEC1BYTE(intable, n,indxcol.XRTAuto)=0;
	      */
	      JVEC(intable, n,indxcol.TARGET)=KWVL_TARGET_NULL;
	      IVEC(intable, n,indxcol.ObsNum)=KWVL_ObsNum_NULL;
	      IVEC(intable, n,indxcol.telemRow)=KWVL_telemRow_NULL;
	      IVEC(intable, n,indxcol.telemCol)=KWVL_telemCol_NULL;
	      JVEC(intable, n,indxcol.BaseLine)=KWVL_BASELINE_NULL;
	      JVEC(intable, n,indxcol.nPixels)=  KWVL_nPixels_NULL;
	      BVEC(intable, n, indxcol.XRTMode)=KWVL_XRTMode_NULL;
	      BVEC(intable, n, indxcol.WaveID)=KWVL_WaveID_NULL;
	      JVEC(intable, n, indxcol.CCDExpo)=KWVL_CCDEXPOS_NULL;
	      BVEC(intable, n, indxcol.Amp)=KWVL_Amp_NULL;
	      if(indxcol.LRHPixCt != -1)
		IVEC(intable, n, indxcol.LRHPixCt)=KWVL_LRHPIXCT_NULL;
	      if(indxcol.LRBiasPx!= -1)
		for (jj=0; jj< intable.Multiplicity[indxcol.LRBiasPx]; jj++)
		  IVECVEC(intable, n, indxcol.LRBiasPx,jj)=KWVL_LRBiasPx_NULL;
	      if(indxcol.PDBIASTHR!=-1)     
		IVEC(intable, n, indxcol.PDBIASTHR)=KWVL_PDBIASTHR_NULL;
	      if(indxcol.LDPNUM!=-1)     
		JVEC(intable, n, indxcol.LDPNUM)=KWVL_LDPNUM_NULL;
	      if(indxcol.LDPPAGE!=-1)     
		JVEC(intable, n, indxcol.LDPPAGE)=KWVL_LDPPAGE_NULL;
	      if(indxcol.HPixCt!=-1)     
		IVEC(intable, n, indxcol.HPixCt)=KWVL_HPIXCT_NULL;
	      
	      if(indxcol.BiasPx!=-1)     
		for (jj=0; jj< intable.Multiplicity[indxcol.BiasPx]; jj++)
		  IVECVEC(intable, n, indxcol.BiasPx,jj)=KWVL_BiasPx_NULL;
	      if(indxcol.IMBLvl!=-1)     
		IVEC(intable, n, indxcol.IMBLvl)=KWVL_IMBLVL_NULL;
	      if(indxcol.FCWSCol!=-1)     
		IVEC(intable, n, indxcol.FCWSCol)=KWVL_FCWSCol_NULL;
	      if(indxcol.FCWSRow!=-1)     
		IVEC(intable, n, indxcol.FCWSRow)=KWVL_FCWSRow_NULL;
	      if(indxcol.FCWNCols!=-1)     
		IVEC(intable, n, indxcol.FCWNCols)=KWVL_FCWNCols_NULL;
	      if(indxcol.FCWNRows!=-1)     
		IVEC(intable, n, indxcol.FCWNRows)=KWVL_FCWNRows_NULL;
	      if(indxcol.FCWThres!=-1)     
		IVEC(intable, n, indxcol.FCWThres)=KWVL_FCWThres_NULL;
	      
	            
	    }
	  else if ((DVEC(intable, n, indxcol.ENDTIME) >= 0.-sens)&&(BVEC(intable, n, indxcol.XRTMode)!=1))
	    global.tstop=DVEC(intable, n, indxcol.ENDTIME);
	  

	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &intable, OutRows, FALSE);
	      OutRows=0;
	    }

       }
     
     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


 WriteFastBintable(ounit, &intable, OutRows, TRUE);


 if(!xrttimes)
   {
     if(fits_insert_rows(ounit, totrow, 1 , &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to add last row in %s file\n", global.taskname, global.par.hdfile);
	 goto timetag_end;
       }
     totrow++;
   

     elem_j=KWVL_TARGET_NULL;
     
     if( fits_write_col_lng(ounit,(indxcol.TARGET+1), (totrow),1,1, &elem_j, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_TARGET);
	 goto timetag_end;
       }

     elem_i=KWVL_ObsNum_NULL;
     
     if( fits_write_col_int(ounit,(indxcol.ObsNum+1), (totrow),1,1, &elem_i, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_ObsNum);
	 goto timetag_end;
       }



     elem_i=KWVL_telemRow_NULL;

     if(fits_write_col_int(ounit, (indxcol.telemRow + 1), (totrow), 1, 1, &elem_i, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_telemRow);
	 goto timetag_end;
       }
     elem_i=KWVL_telemCol_NULL;
     if(fits_write_col_int(ounit, (indxcol.telemCol + 1), (totrow), 1, 1, &elem_i, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_telemCol);
	 goto timetag_end;
       }

     elem_j=KWVL_BASELINE_NULL;
     if(fits_write_col_lng(ounit, (indxcol.BaseLine + 1), (totrow), 1, 1, &elem_j, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_BASELINE);
	 goto timetag_end;
       }

     elem_j=KWVL_nPixels_NULL;
     if(fits_write_col_lng(ounit, (indxcol.nPixels + 1), (totrow), 1, 1, &elem_j, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_nPixels);
	 goto timetag_end;
       }

     byt_null=KWVL_XRTMode_NULL;
     if(fits_write_col_byt(ounit, (indxcol.XRTMode + 1), (totrow), 1, 1, &byt_null, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_XRTMode);
	 goto timetag_end;
       }
     
     byt_null=KWVL_WaveID_NULL;
     if(fits_write_col_byt(ounit, (indxcol.WaveID + 1), (totrow), 1, 1, &byt_null, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_WaveID);
	 goto timetag_end;
       }
     
     elem_j=KWVL_CCDEXPOS_NULL;
     if(fits_write_col_lng(ounit, (indxcol.CCDExpo + 1), (totrow), 1, 1, &elem_j, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_CCDEXPOS);
	 goto timetag_end;
       }
     
     byt_null=KWVL_Amp_NULL;
     if(fits_write_col_byt(ounit, (indxcol.Amp + 1), (totrow), 1, 1, &byt_null, &status))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Amp);
	 goto timetag_end;
       }
     
     if(indxcol.LRHPixCt != -1) 
       {
	 elem_i=KWVL_LRHPIXCT_NULL;
	 if(fits_write_col_int(ounit, (indxcol.LRHPixCt + 1), (totrow), 1, 1, &elem_i, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRHPIXCT);
	     goto timetag_end;
	   }
       }

     if(indxcol.LRBiasPx!= -1) 
       {
       	 elem_i= KWVL_LRBiasPx_NULL;
	 for (jj=1; jj<= intable.Multiplicity[indxcol.LRBiasPx]; jj++)
	   {
	     
	     if(fits_write_col_int(ounit, (indxcol.LRBiasPx + 1), (totrow), jj, 1, &elem_i, &status))
	       {
		 headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRBiasPx);
		 goto timetag_end;
	       }
	   }
       }

     if(indxcol.PDBIASTHR!= -1) 
       {
       	 elem_i=KWVL_PDBIASTHR_NULL;
	 if(fits_write_col_int(ounit, (indxcol.PDBIASTHR + 1), (totrow), 1, 1, &elem_i, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PDBIASTHR);
	     goto timetag_end;
	   }
       }


     if(indxcol.LDPNUM!=-1)
       {
	 elem_j=KWVL_LDPNUM_NULL;
	 if(fits_write_col_lng(ounit, (indxcol.LDPNUM + 1), (totrow), 1, 1, &elem_j, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LDPNUM);
	     goto timetag_end;
	   }
       }
     if(indxcol.LDPPAGE!=-1) 
       
       {
	 elem_j=KWVL_LDPPAGE_NULL;
	 if(fits_write_col_lng(ounit, (indxcol.LDPPAGE + 1), (totrow), 1, 1, &elem_j, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LDPPAGE);
	     goto timetag_end;
	   }
       }

     if(indxcol.HPixCt!=-1)
       {
	 elem_i=KWVL_HPIXCT_NULL;
	 if(fits_write_col_int(ounit, (indxcol.HPixCt + 1), (totrow), 1, 1, &elem_i, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_HPIXCT);
	     goto timetag_end;
	   }
       }

     if(indxcol.BiasPx!=-1)
       {
	 elem_i=KWVL_BiasPx_NULL;

	 for (jj=1; jj<= intable.Multiplicity[indxcol.BiasPx]; jj++)
	   if(fits_write_col_int(ounit, (indxcol.BiasPx + 1), (totrow), jj, 1, &elem_i, &status))
	     {
	       headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_BiasPx);
	       goto timetag_end;
	     }
       }

     if(indxcol.IMBLvl!=-1)
       {
	 elem_i=KWVL_IMBLVL_NULL;
	 if(fits_write_col_int(ounit, (indxcol.IMBLvl + 1), (totrow), 1, 1, &elem_i, &status))
	   {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_IMBLVL);
	     goto timetag_end;
	   }
       }

       if(indxcol.FCWSCol!=-1)
	 {
	   elem_i=KWVL_FCWSCol_NULL;
	   if(fits_write_col_int(ounit, (indxcol.FCWSCol + 1), (totrow), 1, 1, &elem_i, &status))
	     {
	       headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FCWSCol);
	       goto timetag_end;
	     }
	 }
       if(indxcol.FCWSRow!=-1)
	 {
	   elem_i=KWVL_FCWSRow_NULL;
	   if(fits_write_col_int(ounit, (indxcol.FCWSRow + 1), (totrow), 1, 1, &elem_i, &status))
	     {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FCWSRow);
	     goto timetag_end;
	     }
	 }

      if(indxcol.FCWNCols!=-1) 
	{
	  elem_i=KWVL_FCWSRow_NULL;
	  if(fits_write_col_int(ounit, (indxcol.FCWNCols + 1), (totrow), 1, 1, &elem_i, &status))
	    {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FCWNCols);
	     goto timetag_end;
	    }
	}

      if(indxcol.FCWNRows!=-1) 
	{
	  elem_i=KWVL_FCWNRows_NULL;
	  if(fits_write_col_int(ounit, (indxcol.FCWNRows + 1), (totrow), 1, 1, &elem_i, &status))
	    {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FCWNRows);
	     goto timetag_end;
	    }
	}

       if(indxcol.FCWThres!=-1) 
	 {
	   elem_i=KWVL_FCWThres_NULL;
	   if(fits_write_col_int(ounit, (indxcol.FCWThres + 1), (totrow), 1, 1, &elem_i, &status))
	    {
	     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FCWThres);
	     goto timetag_end;
	    }
	 }




   }

 elem_v=0;
 null_v=0;
 
 if( fits_write_colnull_ulng(ounit,(indxcol.FrameHID+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FrameHID);
     goto timetag_end;
   }
     
 elem_v=0;
 null_v=0;

 
 if( fits_write_colnull_ulng(ounit,(indxcol.CCDFrame+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_CCDFrame);
     goto timetag_end;
   }
 
 
 elem_flt=-1.;
 null_flt=-1.;
 
 if( fits_write_colnull_flt(ounit,(indxcol.RA+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_RA);
     goto timetag_end;
   }
 if( fits_write_colnull_flt(ounit,(indxcol.Dec+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Dec);
     goto timetag_end;
   }
 if( fits_write_colnull_flt(ounit,(indxcol.Roll+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Roll);
     goto timetag_end;
   }
 
 if( fits_write_colnull_flt(ounit,(indxcol.ErpCntRt+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_ErpCntRt);
     goto timetag_end;
   }
 
 if( fits_write_colnull_flt(ounit,(indxcol.XPosTAM1+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_XPosTAM1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_flt(ounit,(indxcol.YPosTAM1+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_YPosTAM1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_flt(ounit,(indxcol.XPosTAM2+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_XPosTAM2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_flt(ounit,(indxcol.YPosTAM2+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_YPosTAM2);
     goto timetag_end;
   }
 
 
 elem_dbl=-1;
 null_dbl=-1;
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vod1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vod1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vod2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vod2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vrd1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vrd1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vrd2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vrd2);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vog1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vog1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vog2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vog2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S1Rp1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S1Rp1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S1Rp2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S1Rp2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S1Rp3+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S1Rp3);
     goto timetag_end;
   }	
 
 if( fits_write_colnull_dbl(ounit,(indxcol.R1pR+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_R1pR);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.R2pR+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_R2pR);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S2Rp1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S2Rp1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S2Rp2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S2Rp2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.S2Rp3+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_S2Rp3);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vgr+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vgr);
     goto timetag_end;
   }	
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vsub+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vsub);
     goto timetag_end;
   }	
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Vbackjun+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vbackjun);
     goto timetag_end;
   }	
 if( fits_write_colnull_dbl(ounit,(indxcol.Vid+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Vid);
     goto timetag_end;
   }	
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Ip1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Ip1);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Ip2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Ip2);
     goto timetag_end;
   }	
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Ip3+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Ip3);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Sp1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Sp1);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Sp2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Sp2);
     goto timetag_end;
   }	
 
 
 if( fits_write_colnull_dbl(ounit,(indxcol.Sp3+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Sp3);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.CpIG+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_CpIG);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.BaseLin1+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_BaseLin1);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.BaseLin2+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_BaseLin2);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.ENDTIME+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_ENDTIME);
     goto timetag_end;
   }
 
 
 elem_flt=-1.;
 null_flt=-1.;
 
 if( fits_write_colnull_flt(ounit,(indxcol.CCDTemp+1), (totrow),1,1, &elem_flt, null_flt, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_CCDTemp);
     goto timetag_end;
   }
 
 elem_dbl=-1.;
 null_dbl=-1.;
 
 if( fits_write_colnull_dbl(ounit,(indxcol.LRSumBPx+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRSumBPx);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.LRSoSBPx+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRSoSBPx);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.LRBiasLv+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRBiasLv);
     goto timetag_end;
   }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.LRNoise+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRNoise);
     goto timetag_end;
   }
 
 if(indxcol.EVTLOST!=-1)
   if( fits_write_colnull_dbl(ounit,(indxcol.EVTLOST+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
     {
       headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_EVTLOST);
       goto timetag_end;
     }
 
 if(indxcol.PDBIASLVL!=-1)
   if( fits_write_colnull_dbl(ounit,(indxcol.PDBIASLVL+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
     {
       headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PDBIASLVL);
       goto timetag_end;
     }
 
 if( fits_write_colnull_dbl(ounit,(indxcol.HKTIME+1), (totrow),1,1, &elem_dbl, null_dbl, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_HKTIME);
     goto timetag_end;
   }
 
 elem_v=0;
 null_v=0;
 
 if( fits_write_colnull_ulng(ounit,(indxcol.FSTS+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FSTS);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_ulng(ounit,(indxcol.FETS+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FETS);
     goto timetag_end;
   }
 
 if( fits_write_colnull_ulng(ounit,(indxcol.PixGtLLD+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PixGtLLD);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_ulng(ounit,(indxcol.PixGtULD+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PixGtULD);
     goto timetag_end;
   }
 
 if( fits_write_colnull_ulng(ounit,(indxcol.LRBPixCt+1), (totrow),1,1, &elem_v, null_v, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_LRBPixCt);
     goto timetag_end;
   }

 elem_uns=0;
 null_uns=0;
 
 if( fits_write_colnull_uint(ounit,(indxcol.DNCCDTemp+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_DNCCDTemp);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.FSTSub+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FSTSub);
     goto timetag_end;
   }
 
 if( fits_write_colnull_uint(ounit,(indxcol.SplitThr+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_SPLITTHR);
     goto timetag_end;
   }
  
 if( fits_write_colnull_uint(ounit,(indxcol.PixUnder+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PixUnder);
     goto timetag_end;
   }
 
 if( fits_write_colnull_uint(ounit,(indxcol.PixOverF+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_PixOverF);
     goto timetag_end;
   }
 
     
 
 if( fits_write_colnull_uint(ounit,(indxcol.FETSub+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FETSub);
     goto timetag_end;
   }
   
 
 if( fits_write_colnull_uint(ounit,(indxcol.CCDExpSb+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_CCDEXPSB);
     goto timetag_end;
   }

 
 if( fits_write_colnull_uint(ounit,(indxcol.EvtLLD+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_EVTLLD);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.EvtULD+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_EvtULD);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.OuterThr+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_OuterThr);
     goto timetag_end;
   }  
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.SnglePix+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_SnglePix);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.SngleSpl+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_SngleSpl);
     goto timetag_end;
   }
   
 
 if( fits_write_colnull_uint(ounit,(indxcol.ThreePix+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_ThreePix);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.FourPix+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_FourPix);
     goto timetag_end;
   }
     
   
 
 if( fits_write_colnull_uint(ounit,(indxcol.WinHalfW+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_WinHalfW);
     goto timetag_end;
   }
   
 
 if( fits_write_colnull_uint(ounit,(indxcol.WinHalfH+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_WinHalfH);
     goto timetag_end;
   }
 
 
 if( fits_write_colnull_uint(ounit,(indxcol.BiasExpo+1), (totrow),1,1, &elem_uns, null_uns, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_BiasExpo);
     goto timetag_end;
   }


 if( fits_write_col_dbl(ounit,(indxcol.TIME+1), (totrow),1,1, &global.tstop, &status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_HKTIME);
     goto timetag_end;
   }

 byt_null=0;
 if( fits_write_col_byt(ounit,(indxcol.Settled+1), (totrow),1,1, &byt_null,&status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_Settled);
     goto timetag_end;
   }

 if( fits_write_col_byt(ounit,(indxcol.In10Arcm+1), (totrow),1,1, &byt_null,&status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_In10Arcm);
     goto timetag_end;
   }

 if( fits_write_col_byt(ounit,(indxcol.InSAA+1), (totrow),1,1, &byt_null,&status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_InSAA);
     goto timetag_end;
   }

 if( fits_write_col_byt(ounit,(indxcol.InSafeM+1), (totrow),1,1, &byt_null,&status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_InSafeM);
     goto timetag_end;
   }

 if( fits_write_col_byt(ounit,(indxcol.XRTAuto+1), (totrow),1,1, &byt_null,&status))
   {
     headas_chat(NORMAL, "%s: Error: Unable to set to NULL last row of the %s column.\n", global.taskname,CLNM_XRTAuto);
     goto timetag_end;
   }

 if(first_timing && first_tag)
   {
     headas_chat ( NORMAL, "%s: Warning: Unable to calculate times of all TIMING frames.\n", global.taskname);
   }  


 if(count_null)
   {
     if(SetDoubleColumnToNull(ounit, totrow, (indxcol.TIME+1)))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL %s column\n", global.taskname, CLNM_TIME);
	 goto timetag_end;
       }
     
     if(SetDoubleColumnToNull(ounit, totrow, (indxcol.ENDTIME+1)))
       {
	 headas_chat(NORMAL, "%s: Error: Unable to set to NULL %s column\n", global.taskname, CLNM_ENDTIME);
	 goto timetag_end;
       }
   }
 
 ReleaseBintable(&head, &intable);
 return OK;
     
 timetag_end:
 if (head.first)
   ReleaseBintable(&head, &intable);
 
 return NOT_OK;
     
} /* Timetag */

/*
 *	xrthkproc
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
 *             void xrthkproc_getpar(void);
 *             void xrthkproc_info(void); 
 * 	       void xrthkproc_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrthkproc()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTHKPROC_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /* Get parameter values */ 
  if ( xrthkproc_getpar() == OK ) {
    
    if ( xrthkproc_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto timetag_end;
    }
  }
  else
    goto timetag_end;
  return OK;

 timetag_end:
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;

} /* xrthkproc */
/*
 *	xrthkproc_info:
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
 *        0.1.0: - BS 14/11/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrthkproc_info()
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Housekeeping Header Packets file   :'%s'\n",global.par.hdfile);
  headas_chat(NORMAL,"Name of the output Housekeeping Header Packet file   :'%s'\n",global.par.outfile);
  if(!((global.par.srcdetx >= DETX_MIN)&&(global.par.srcdetx <= DETX_MAX)) || !((global.par.srcdety >= DETY_MIN)&&(global.par.srcdety <= DETY_MAX)))
    {
      headas_chat(NORMAL,"Name of the the input attitude file                  :'%s'\n",global.par.attfile);
      headas_chat(NORMAL,"Name of the input TELDEF file                        :'%s'\n",global.par.teldef);
      headas_chat(NORMAL,"Source RA                                            :'%f'\n",global.par.srcra);
      headas_chat(NORMAL,"Source Dec                                           :'%f'\n",global.par.srcdec);
      headas_chat(NORMAL,"Nominal RA                                           :'%f'\n",global.par.ranom);
      headas_chat(NORMAL,"Nominal Dec                                          :'%f'\n",global.par.decnom);
      if (global.par.attinterpol)
	headas_chat(NORMAL,"Interpolate Attitude values?                         : yes\n");
      else
	headas_chat(NORMAL,"Interpolate Attitude values?                         : no\n");
      if (global.par.aberration)
	headas_chat(NORMAL,"Should aberration be included in aspecting?          : yes\n");
      else
	headas_chat(NORMAL,"Should aberration be included in aspecting?          : no\n");
    }
  else
    {
      headas_chat(NORMAL,"Source DETX                                          :'%d'\n",global.par.srcdetx);
      headas_chat(NORMAL,"Source DETY                                          :'%d'\n",global.par.srcdety);
    }
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file?               : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file?               : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                               : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                               : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrthkproc_info */

int xrthkproc_checkinput(void)
{
  char           stem[10], ext[MAXEXT_LEN];
  pid_t          tmp;

  if (!strcasecmp(global.par.outfile, DF_DEFAULT))
    {
      strcpy(global.par.outfile, global.par.hdfile);
      StripExtension(global.par.outfile);
      GetFilenameExtension(global.par.hdfile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	StripExtension(global.par.outfile);
      
      strcat(global.par.outfile, HKFITSEXT);
      headas_chat(NORMAL, "%s: Info: Name for the output file is:\n",global.taskname);
      headas_chat(NORMAL, "%s: Info: '%s'\n", global.taskname, global.par.outfile);
    }

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
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	  
	  if(remove (global.par.outfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	      goto check_end;
	    }  
	}
    }
  
    
  /* Derive temporary event filename */
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  
  DeriveFileName(global.par.outfile, global.tmpfile, stem);


  if(FileExists(global.tmpfile))
    {
      if(remove (global.tmpfile) == -1)
	{
	  headas_chat(CHATTY, "%s: Error: Unable to remove", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto check_end;
	}  
    }
  return OK;

 check_end:
  return NOT_OK;

}
int ComputeDetCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord)
{

  
  double     db_dety_tmp=1., db_x, db_y, src_dety=0., db_detx_tmp=1., src_detx=0.;
  double     mjd=0.;
  QUAT*      q;
  XFORM2D*   sky2det;
  
  sky2det = allocateXform2d();
  q = allocateQuat();

  if(!isInExtrapolatedAttFile(attfile,time) ) 
    {
      headas_chat(NORMAL,"%s: Warning: CCD Frame Time: %f is not included\n", global.taskname, time);
      headas_chat(NORMAL,"%s: Warning: in the time range of the\n", global.taskname); 
      headas_chat(NORMAL,"%s: Warning: %s Attitude file.\n", global.taskname, global.par.attfile);
      coord->detx = KWVL_DETNULL; 
      coord->dety = KWVL_DETNULL; 
      /*headas_chat(CHATTY,"%s: Warning: Setting detx = %d and dety = %d.\n", global.taskname, coord->detx,coord->dety );*/	 
      return OK;
	    	      
    } 

     
  /***************************************************
   * the current time is covered by the attitude file *
   ***************************************************/
  
  findQuatInAttFile(attfile,q,time);

  /*printf("TIME ==%10.5f QUAT ==%5.10f %5.10f  %5.10f  %5.10f\n", time, q->p[0],q->p[1],q->p[2],q->p[3]);*/

  if(global.par.aberration)
    {
      mjd=global.mjdref + time/86400.;

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
}

