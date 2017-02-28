/*
 * 
 *	xrtcalcpi.c:
 *
 *	INVOCATION:
 *
 *		xrtcalcpi [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine to convert PHA to PI.  
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 28/05/02 - First version
 *        0.1.1 -    12/09/02 - Added functions to locate CALDB gain file
 *                              by observation date and time 
 *        0.1.2 -    31/10/02 - Added TimingBintable routine to handle Windowed Timing Mode
 *                            - Modified to read input files name 
 *                              from ASCII file  
 *        0.1.3 -    06/02/03 - Modified check on input and output file
 *                            - Added check to verify if PHA values are computed 
 *                            - Disabled compute PI for Timing Modes 
 *        0.2.0 - FT 08/05/03 - Added PHA to PI Conversion for Timing Modes
 *        0.2.1 - BS 12/05/03 - Added flag in STATUS column if PI == PIMAX
 *                            - Added check to verify if exposure times are computed for 
 *                              Timing Modes
 *        0.2.2 -    16/06/03 - Bug fixed on xrtcalcpi_getpar failure
 *        0.2.3 -    09/07/03 - Modified to get latest version of CALDB gain file
 *        0.2.4 -    15/07/03 - Modified to use new prototype of 'CalGetFileName' 
 *                            - Used 'hd_ran2' i instead of 'xrt_ran2'
 *        0.2.5 -    29/07/03 - Deleted 'rawx' and 'rawy' parameters
 *                            - Added HISTORY blocks into output FITS file
 *                            - Added routine to compute RAW coordinates from DET coordinates
 *                            - Added handling PHA NULL events
 *        0.2.6 -    03/11/03 - Replaced call to headas_parstam() with HDpar_stamp()
 *        0.2.7 -    10/11/03 - Add or update 'XRTPI' keyword into output events file
 *        0.2.8 -    22/03/04 - Changed 'EV_PIMAX' in 'EV_SAT'
 *        0.2.9 -    25/03/04 - Deleted input files list handling
 *       0.2.10 -    06/05/04 - Modified some comments line
 *       0.2.11 -    12/05/04 - Messages displayed revision
 *       0.2.12 -    24/06/04 - Bug fixed on 'fits_update_keyword' call for LOGICAL type
 *       0.2.13 -    24/09/04 - Added check on rawx/y range (only for timing) and set STATUS 
 *                              column to EV_BAD                            
 *       0.2.14-     07/10/04 - Added  check on BIASONBORD keyword to the
 *                              "Check if PHA is already computed" check
 *       0.2.15-     21/10/04 - Exit with error if PHA is not calculated
 *       0.2.16-     03/03/05 - Added check on time NULL values
 *       0.3.0 -     22/03/05 - Handled new CALDB gain file format with coefficients temperature dependent
 *       0.3.1 -     24/03/05 - Replaced MUTE with CHATTY in few messages
 *       0.3.2 -     24/03/05 - Deleted check on event TSTART 
 *       0.3.3 -     13/07/05 - Replaced ReadBintable with ReadBintableWithNULL
 *                              to perform check on undefined value reading hdfile
 *       0.3.4 -     14/07/05 - Exit with warning if infile is empty
 *       0.3.5 -     03/08/05 - Exit with error if gainfile or hdfile are empty
 *                            - Create output file when task exits with warning
 *       0.3.6 - NS  12/03/07 - Changed ReadTemperatures function to accept Settled=1 In10Arcm=0 InSafeM=1
 *                              for events with obsmode=SLEW
 *       0.3.7 -     09/07/07 - Query to CALDB for gainfile depending on 'XRTVSUB' of input evt
 *       0.3.8 -     01/04/08 - Handle new gain CALDB file format
 *                            - New additional PI correction
 *       0.3.9 -     26/09/08 - Not interpolate offset value of two successive rows in CALDB gain file
 *                            - Changed ReadTemperatures function to evaluate obsmode=SLEW when InSafeM=1
 *       0.4.0 -     06/03/09 - Added energy dependecy in PI correction for charge traps
 *                            - Handle new gain CALDB file format
 *       0.4.1 -     21/03/11 - Modified PI correction algorithm
 *                            - Handle new gain CALDB file format
 *                            - New input parameters userctipar, beta1, beta2, ecti, offsetniter and savepioffset
 *       0.4.2 -     09/02/12 - Modified computation of corrected GC1/GC2 coefficients
 *       0.4.3 -     20/04/13 - Bug fixed handling 'seed' input parameter
 *                               
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtcalcpi  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtcalcpi.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTCALCPI_C
#define XRTCALCPI_VERSION      "0.4.3"
#define PRG_NAME               "xrtcalcpi"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtcalcpi_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtcalcpi.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 *           int PILGetReal(char *name, double *result);  
 *           int PILGetBool(char *name, int *result);
 *           int PILGetInt(char *name, int *result);  
 *           void get_history (int *);
 *           void ReadInFileNamesFile(char *);
 *           int headas_chat(int , char *, ...);
 *
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 28/05/2002 - First version
 *        0.1.1: - FT 08/05/2003 - Added rawx and rawy parameters
 *        0.1.2: - BS 12/05/2003 - Deleted phamax and pimax parameters
 *        0.1.3: - BS 29/07/2003 - Deleted rawx and rawy parameters  
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */

int xrtcalcpi_getpar(void)
{
  /*
   *  Get File name Parameters
   */
  
  /* Input File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_INFILE);
      goto Error;
    }
 
  /* Output Event List File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  /* Gain  File Name */
  if(PILGetFname(PAR_GAINFILE, global.par.gainfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_GAINFILE);
      goto Error;	
    }

  /* Gain  File Name */
  if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_HDFILE);
      goto Error;	
    }
 
  /*
   *  Get Real Parameters
   */

  /* Nominal gain (eV/ch) */
  if(PILGetReal(PAR_GAINNOM, &global.par.gainnom)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_GAINNOM);
      goto Error;	
    }

  /* Offset in channel/energy relationship */
  if(PILGetReal(PAR_OFFSET, &global.par.offset)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_OFFSET);
      goto Error;	
    }

  /* PI correction type */
  if(PILGetString(PAR_CORRTYPE, global.par.corrtype)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CORRTYPE);
      goto Error;	
    }


  /* Parameters for charge traps correction input by the user?(yes/no) */
  if(PILGetBool(PAR_USERCTCORRPAR, &global.par.userctcorrpar)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_USERCTCORRPAR);
      goto Error;
    }


  if(global.par.userctcorrpar){

    /* powerlaw index for Energy <= break energy for charge traps */
    if(PILGetReal(PAR_ALPHA1, &global.par.alpha1)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_ALPHA1);
	goto Error;	
      }
    
    /* powerlaw index for Energy > break energy for charge traps */
    if(PILGetReal(PAR_ALPHA2, &global.par.alpha2)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_ALPHA2);
	goto Error;	
      }
    
    /* break energy for charge traps */
    if(PILGetReal(PAR_EBREAK, &global.par.ebreak)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_EBREAK);
	goto Error;	
      }
  }

  /* Parameters for GC1/GC2 energy dependence input by the user?(yes/no) */
  if(PILGetBool(PAR_USERCTIPAR, &global.par.userctipar)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_USERCTIPAR);
      goto Error;
    }


  if(global.par.userctipar){

    /* powerlaw index for Energy <= break energy */
    if(PILGetReal(PAR_BETA1, &global.par.beta1)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_BETA1);
	goto Error;	
      }
    
    /* powerlaw index for Energy > break energy */
    if(PILGetReal(PAR_BETA2, &global.par.beta2)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_BETA2);
	goto Error;	
      }
    
    /* break energy */
    if(PILGetReal(PAR_ECTI, &global.par.ecti)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_ECTI);
	goto Error;	
      }
  }


  /*
   *  Get Bool Parameters
   */  
  
  if(PILGetBool(PAR_SAVEPINOM, &global.par.savepinom)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_SAVEPINOM);
      goto Error;
    }

  if(PILGetBool(PAR_SAVEPIOFFSET, &global.par.savepioffset)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_SAVEPIOFFSET);
      goto Error;
    }

  /* Randomize PHA ?*/
  if(PILGetBool(PAR_RANDOMFLAG, &global.par.randomflag)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname, PAR_RANDOMFLAG);
      goto Error;
    }

  /*
   *  Get Int Parameters
   */  
  
  /* Random number generator seed */
  if (global.par.randomflag)
    if (PILGetInt(PAR_SEED, &global.par.seed))
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_SEED);
	goto Error;
      }

  /* Number of iterations for charge traps correction */
  if (PILGetInt(PAR_OFFSETNITER, &global.par.offsetniter))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n", global.taskname,PAR_OFFSETNITER);
      goto Error;
    }


  get_history(&global.hist);

  xrtcalcpi_info();
 
  
  
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* xrtcalcpi_getpar */

/*
 *	xrtcalcpi_work
 *
 *
 *	DESCRIPTION:
 *            This routine reads event and gain files and calculates 
 *            PI values.
 *            Photon Counting, Windowed Timing, Piled Up Photodiode and 
 *            Low Rate Photodiode Modes are allowed		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *       
 *           FitsFileUnit_t OpenReadFitsFile(char *);
 *           int FileExists(char *);
 *           FitsFileUnit_t OpenWriteFitsFile(char *);
 *           FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t);
 *           SPTYPE GetSVal(FitsHeader_t *, char *);
 *           double GetDVal(FitsHeader_t *, char *);
 *           int ChecksumCalc(FitsFileUnit_t unit);
 *           int CloseFitsFile(FitsFileUnit_t file);
 *           char *GetFilenameExtension(char *, char *);
 *           char *DeriveFileName(const char *OldName, char *NewName, const char *ext)
 *           BOOL FileExists(const char *FileName);
 *           BOOL ExistsKeyWord(FitsHeader_t *header, char *KeyWord, FitsCard_t **card); 
 *
 *           int headas_chat(int , char *, ...);
 *           int HDpar_stamp(FitsFileUnit_t, int, int *);
 *
 *
 *           int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *           int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, 
 *                             int morekeys, int *status); 
 *           int fits_movnam_hdu(fitsfile *fptr,int hdutype, 
 *                               char *extname, int extver, int *status);
 *           int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *           int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, 
 *                               int *status );
 *           pid_t getpid(void);
 *           char * strtok(char *, char *);
 *           char * strchr(const char *s, int c);
 *           int strcmp(char *, char *);
 *           int strcasecmp(char *, char *);
 *           int sprintf(char *str, const char *format, ...);
 *           void free(void *ptr)
 *           char *strcpy(char *dest, const char *src);
 *           int rename (char *, char *);
 *           int remove (char *);
 *
 *	
 *           ISXRTFILE_WARNING(int, FitsFileUnit_t, char *,char *);
 *           int GetGainFileCoeff(void);
 *           int PhotonCountingPha2Pi(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int WindowedTimingPha2Pi(FitsFileUnit_t in, FitsFileUnit_t out);
 *           int PhotodiodePha2Pi(FitsFileUnit_t in, FitsFileUnit_t out);
 *
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 28/05/2002 - First version
 *        0.1.2: - FT 08/05/2003 - Added fuctions to handle Timing Modes
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtcalcpi_work(void)
{
  int             status = OK, inExt, outExt, evExt;
  int             logical;
  
  
  FitsHeader_t    head;
  FitsFileUnit_t  inunit=NULL, outunit=NULL;  /* Input and Output fits file pointer */ 
  FitsCard_t      *card;

  /* Get input file name */
  TMEMSET0( &head, FitsHeader_t );
    
  global.warning=0;
  if(xrtcalcpi_checkinput())
    goto Error;


  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Check readout mode */
  
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
  
  headas_chat(NORMAL,"%s: Info: Processing '%s' file.\n",global.taskname,global.par.infile );


  /* Retrieve datamode of input event file */
  head=RetrieveFitsHeader(inunit);
  if(ExistsKeyWord(&head, KWNM_DATAMODE, &card))
    strcpy(global.datamode,card->u.SVal);
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  if(ExistsKeyWord(&head, KWNM_OBS_MODE, &card))
    strcpy(global.obsmode,card->u.SVal);
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_OBS_MODE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }

  /* Retrieve start observation date and time */
  if(ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    global.dateobs=card->u.SVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  if(!(strchr(global.dateobs, 'T')))
    { 
      if(ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	global.timeobs=card->u.SVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
    }

  /* Retrieve XRTVSUB of input event file */
  if((ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.xrtvsub=card->u.JVal;
    }
  else
    {
      global.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }
  
  /* Check datamode keyword */
  if (strcmp(global.datamode, KWVL_DATAMODE_PH) && strcmp(global.datamode, KWVL_DATAMODE_TM) && strcmp(global.datamode, KWVL_DATAMODE_PD)  && strcmp(global.datamode, KWVL_DATAMODE_PDPU) )
    {
      headas_chat(NORMAL,"%s: Error: This task does not process %s datamode.\n", global.taskname, global.datamode);
      headas_chat(CHATTY,"%s: Error: Valid datamodes  are:  %s, %s, %s and %s.\n", global.taskname, KWVL_DATAMODE_PH, KWVL_DATAMODE_TM,KWVL_DATAMODE_PD,KWVL_DATAMODE_PDPU);
      
      if( CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
      
      /* goto skip_file; */
    }
  
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n",global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n",global.taskname,global.par.infile);      
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
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  /* Copy all extensions before events from input to output file */
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
  if(fits_movabs_hdu(inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU.\n",global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto Error;
    }
  
  
  /* Read gain file and get coefficients value */  
  if(GetGainFileCoeff())
    {
      headas_chat(NORMAL,"%s: Error: Unable to process\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.gainfile);
      goto Error;
      
    }
      
  /* Add event extension in output file */
  if (!strcmp(global.datamode, KWVL_DATAMODE_PH))
    {
      /* Retrieve TSTART and TSTOP of input event file */
      global.xrtmode=XRTMODE_PC;
      if(ExistsKeyWord(&head, KWNM_TSTART, &card))
	global.tstart=card->u.DVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n",  global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      
      if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
	global.tstop=card->u.DVal;
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      
      /* Compute PI values for Photon Counting Read Out Mode */ 
      if ((PhotonCountingPha2Pi(inunit, outunit)))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to calculate PI values.\n", global.taskname);
	  goto Error;
	}

      if(global.warning)
	goto ok_end;
    }
  else if (!strcmp(global.datamode, KWVL_DATAMODE_TM))
    {/* Compute PI values for Windowed Timing Read Out Mode */ 
      global.xrtmode=XRTMODE_WT;
      /* Check if Photon arrival  times are already computed */
      if(!(ExistsKeyWord(&head, KWNM_XRTTIMES, NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRTTIMES)))
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found or unset\n",global.taskname,KWNM_XRTTIMES);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: Photon arrival times are not been calculated\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY, "%s: Error: To calculated them, please run 'xrttimetag' task on\n", global.taskname); 
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      else
	{
	  if ((WindowedTimingPha2Pi(inunit, outunit)))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate PI values.\n", global.taskname);
	      goto Error;
	    }
	  if(global.warning)
	    goto ok_end;
	}
    }
  else
    {/* Compute PI values for Photodiode Read Out Mode */ 
      /* Check if exposure time is already computed */
      if(!(ExistsKeyWord(&head, KWNM_XRTTIMES, NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRTTIMES)))
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found or unset\n",global.taskname,KWNM_XRTTIMES);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: photon arrival times are not been calculated\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY, "%s: Error: To calculated them, please run 'xrttimetag' task on\n", global.taskname); 
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	  
	  goto Error;
	}
      else
	{
	  if(strcmp(global.datamode, KWVL_DATAMODE_PDPU))
	    global.xrtmode=XRTMODE_LR;
	  else
	    global.xrtmode=XRTMODE_PU;

	  if ((PhotodiodePha2Pi(inunit, outunit)))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate PI values.\n", global.taskname);
	      goto Error;
	    }
	  if(global.warning)
	    goto ok_end;
	}
    }
  
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPI, &logical, CARD_COMM_XRTPI, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword.\n", global.taskname, KWNM_XRTPI);
      goto Error;
    }
  outExt++;
  
  /* copy any extensions after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to  move in %d HDU\n", global.taskname, outExt);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to copy %d HDU\n", global.taskname,outExt);
	  headas_chat(CHATTY, "%s: Error: from %s input file\n", global.taskname, global.par.infile);
	  headas_chat(CHATTY, "%s: Error: to %s temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}
      
      outExt++;
    }

  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  
  /* Add checksum and datasum in all extensions in output file*/
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM.\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
  /* Close input fits file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n",   global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file. \n ", global.taskname, global.par.infile);
      goto Error;
    }  
  
  /* Close output fits file */
  if (!CloseFitsFile(outunit)) 
    headas_chat(CHATTY,"%s: Info: File '%s' successfully written.\n",global.taskname,
		global.tmpfile );
  else
    {
      
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto Error;
      
    }  
 
  /* rename temporary file into output event file */
  if(rename (global.tmpfile, global.par.outfile) == -1)
    {
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file. \n", global.taskname);
      goto Error;
    }
  
  headas_chat(NORMAL,"%s: Info: File '%s' successfully written.\n",global.taskname,
	      global.par.outfile );
  
  
  /* Get next filename */    

  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Info: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  
  
  /*  for (iii=0; iii<global.count ; iii++)
    printf("HKTIME == %5.5f -- FRAME == %d -- TEMP == %f\n", global.temp[iii].hktime, global.temp[iii].hkccdframe, global.temp[iii].temp);
  */

 ok_end:
  if(global.warning && strcasecmp(global.par.infile, global.par.outfile) )
    {
      if(CopyFile(global.par.infile, global.par.outfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outfile.\n", global.taskname);
	  goto Error;
	}
    }
  if(global.count)
    free(global.temp);
  return OK;


 Error:

  if (FileExists(global.tmpfile))
    if(remove(global.tmpfile) == -1)
      {
	headas_chat(CHATTY, "%s: Error: Unable to remove", global.taskname);
	headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
      }  
   
  return NOT_OK;
} /* xrtcalcpi_work */


static int ComputePI(int rawx, int rawy, int pha, double etime, double temp, double gain, double coeff[GAINCOEFF_NUM_NEW], double coeff_corr[6], double *pi_nom, double *pi, double *pi_offset){

  double      beta1=0.0, beta2=0.0, e_cti=0.0;
  double      rpha, rphanom, gain_calc, offset_calc, offset2;
  static long int  seed=0L;
  double      gc0,gc1,gc2,gc3,gc4,gc5;
  double      gc1_corr,gc2_corr;


  if (!seed){
    seed=(long) global.par.seed;
    hd_ran2(&seed);
  }

  /* Set GC1/GC2 powerlaw parameters */
  if(global.par.userctipar){
    beta1 = global.par.beta1;
    beta2 = global.par.beta2;
    e_cti = global.par.ecti;
  }
  else{
    beta1 = coeff_corr[0];
    beta2 = coeff_corr[1];
    e_cti = coeff_corr[2];
  }

  /* Set GC0..GC5 parameters */
  gc0 = coeff[0];
  gc1 = coeff[1];
  gc2 = coeff[2];
  gc3 = coeff[3];
  gc4 = coeff[4];
  gc5 = coeff[5];


  if (global.par.randomflag)
    /* Randomize PHA */
    rpha=(double)pha + hd_ran2(&seed);
  else 
    rpha=(double)pha;


  /* offset set to zero */
  *pi_offset=0.0;


  /* NOMINAL CORRECTION */

  /* Calculate photon energy in eV */
  gain_calc = gc0 + rawx*gc1 + rawy*gc2;
  offset_calc = gc3 + rawx*gc4 + rawy * gc5;
  rphanom=rpha*gain_calc + offset_calc;
  
  /* Convert rphanom in KeV and calculate PI */
  rphanom = (rphanom/1000.0 - global.par.offset)/gain;
  
  
  /* Calculate corrected GC1 and GC2 */
  if(rphanom<=e_cti){
    gc1_corr =  gc1 * pow(rphanom/e_cti, -beta1);
    gc2_corr =  gc2 * pow(rphanom/e_cti, -beta1);
  }
  else{
    gc1_corr =  gc1 * pow(rphanom/e_cti, -beta2);
    gc2_corr =  gc2 * pow(rphanom/e_cti, -beta2);
  }
  
  /* Calculate photon energy with corrected GC1 and GC2 */
  gain_calc = gc0 + rawx*gc1_corr + rawy*gc2_corr;
  offset_calc = gc3 + rawx*gc4 + rawy * gc5;
  rphanom = rpha*gain_calc + offset_calc;
  
  rphanom = (rphanom/1000.0 - global.par.offset)/gain;
  
  *pi_nom = rphanom;

  
  /* REQUESTED CORRECTION */
  
  
  if(!stricmp(global.par.corrtype,TOTAL_MODE)){

    /* Set GC1/GC2 powerlaw parameters */
    if(!global.par.userctipar){
      beta1 = coeff_corr[3];
      beta2 = coeff_corr[4];
      e_cti = coeff_corr[5];
    }

    /* Set GC0..GC5 parameters */
    gc0 = coeff[6];
    gc1 = coeff[7];
    gc2 = coeff[8];
    gc3 = coeff[9];
    gc4 = coeff[10];
    gc5 = coeff[11];


    /* Calculate photon energy in eV */
    gain_calc = gc0 + rawx*gc1 + rawy*gc2;
    offset_calc = gc3 + rawx*gc4 + rawy * gc5;
    rphanom=rpha*gain_calc + offset_calc;
    
    /* Convert rphanom in KeV and calculate PI */
    rphanom = (rphanom/1000.0 - global.par.offset)/gain;
    
    
    /* Calculate corrected GC1 and GC2 */
    if(rphanom<=e_cti){
      gc1_corr =  gc1 * pow(rphanom/e_cti, -beta1);
      gc2_corr =  gc2 * pow(rphanom/e_cti, -beta1);
    }
    else{
      gc1_corr =  gc1 * pow(rphanom/e_cti, -beta2);
      gc2_corr =  gc2 * pow(rphanom/e_cti, -beta2);
    }
    
    /* Calculate photon energy with corrected GC1 and GC2 */
    gain_calc = gc0 + rawx*gc1_corr + rawy*gc2_corr;
    offset_calc = gc3 + rawx*gc4 + rawy * gc5;
    rphanom=rpha*gain_calc + offset_calc;
    
    rphanom = (rphanom/1000.0 - global.par.offset)/gain;
    
    
    if(global.offset2format)
      {
	CalculateGainOffset2(etime, temp, rphanom, rawx, rawy, &offset2);    
	if(offset2>0)
	  headas_chat(CHATTY, " %s: Info: TIME=%f RAWX=%d RAWY=%d TEMP=%f OFFSET2=%f\n", 
		      global.taskname, etime, rawx, rawy, temp, offset2);
      }
    else
      {
	headas_chat(NORMAL, "%s: Error: %s correction mode not supported for old gain file format.\n", global.taskname,TOTAL_MODE);
	goto pi_end;
      }
    
    *pi = rphanom + offset2;
    *pi_offset = offset2;
  }
  else if(!stricmp(global.par.corrtype,NOMINAL_MODE)){
    
    *pi = *pi_nom;
  }
  else if(!stricmp(global.par.corrtype,TRAPS_MODE)){
    
    if(global.offset2format)
      {
	CalculateGainOffset2(etime, temp, *pi_nom, rawx, rawy, &offset2);
	if(offset2>0)
	  headas_chat(CHATTY, " %s: Info: TIME=%f RAWX=%d RAWY=%d TEMP=%f OFFSET2=%f\n", 
		      global.taskname, etime, rawx, rawy, temp, offset2);
      }
    else
      {
	headas_chat(NORMAL, "%s: Error: %s correction mode not supported for old gain file format.\n", global.taskname,TRAPS_MODE);
	goto pi_end;
      }
    
    
    /* Convert rpha in KeV and calculate PI */
    rphanom = (rpha/1000.0 - global.par.offset)/gain;
    *pi = rphanom + offset2;
    *pi_offset = offset2;
  }
  
  
  return OK;

 pi_end:
  return NOT_OK;
}



/*
 *
 *      PhotonCountingPha2Pi
 *
 *	DESCRIPTION:
 * 		This routine adds input event bintable modified in output file.   
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *          void  GetGMTDateTime(char *);
 *
 *          headas_chat(int ,char *, ...);
 *
 *          FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *          BOOL ExistsKeyWord(FitsHeader_t *header, char *KeyWord, FitsCard_t **card);
 *          void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     		            	       const int MaxBlockRows, const unsigned nColumns,
 *  				       const unsigned ActCols[]);
 *          int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *          void AddColumn(FitsHeader_t *header, Bintable_t *table, char *ttype, char *TypeComment,
 *                         char *tform, unsigned present, ...);
 *          void AddHistory(FitsHeader_t *header, const char *Comment);
 *          void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *          void FinishBintableHeader(const FitsFileUnit_t unit, 
 *                                    FitsHeader_t *header, Bintable_t *table); *
 *          int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                            const unsigned nColumns, const unsigned ActCols[], 
 *                            const unsigned FromRow, unsigned *nRows);
 *          int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, 
 *                                unsigned nRows, BOOL last);
 *          unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *
 *          void get_ran2seed(long int)
 *          int sprintf(char *str, const char *format, ...); 
 *
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 04/06/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PhotonCountingPha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit) 
{
  int                pha, rawx, rawy, newstatus=0, iii=0;
  int                status=OK, ccdframe, ccdframe_old=0, pinomcol;
  JTYPE              null_pi=KWVL_PINULL, max_pi=PI_MAX;
  BOOL               new_coeff=1, first_time=1;
  char               comm[256]="", KeyWord[FLEN_KEYWORD];
  double             gain, coeff[GAINCOEFF_NUM_NEW];
  double             coeff_corr[6], pi=0.0, pi_nom=0.0, pi_offset=0.0;
  double             etime,sens=0.0000001, time_old=0.0, temp=0.0;
  unsigned           FromRow, ReadRows, n, nCols, OutRows=0;  
/*   long int           seed; */
  Ev2Col_t           indxcol; 
  Bintable_t	     outtable;
  FitsHeader_t	     head;
    

  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  if( ReadTemperatures())
    {
      headas_chat(NORMAL, " %s: Error: Unable to read %s column\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, " %s: Error: in %s file.\n", global.taskname, global.par.hdfile);
    }

  /*  if(global.caldbrows > 1)
      {*/  
      /* Check if event time range is covered by gain time range */
  /*if (global.gaincoeff[0].gtime > global.tstart) {
    headas_chat(NORMAL, "%s: Error: Observation start time is not included in the validity time range\n", global.taskname);
    headas_chat(NORMAL, "%s: Error: of the %s GAIN file.\n", global.taskname, global.par.gainfile);
    goto event_end;
    }
      
    }*/

  /* Transform gain in KeV */
  if (global.par.gainnom <= 0)
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n", global.taskname, global.nomgain);
      headas_chat(CHATTY, "%s: Info: from %s GAIN file.\n", global.taskname, global.par.gainfile);
      gain=global.nomgain/1000.0;
      
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n", global.taskname, global.par.gainnom);
      gain=global.par.gainnom/1000.0;
      
    }


    /* Read input event bintable */
  head=RetrieveFitsHeader(evunit);

  /* Check if PHA is already computed */
  if(!(ExistsKeyWord(&head, KWNM_XRTPHA  , NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRTPHA)))
  {
    headas_chat(CHATTY, "%s: Error: %s keyword not found or unset\n",global.taskname,KWNM_XRTPHA);
    headas_chat(CHATTY, "%s: Error: in '%s' file\n",global.taskname, global.par.infile);
    headas_chat(NORMAL, "%s: Error: The  %s file\n", global.taskname, global.par.infile);
    headas_chat(NORMAL, "%s: Error: has the %s column empty, the %s values cannot be calculated.\n", global.taskname, CLNM_PHA,CLNM_PI);
    headas_chat(CHATTY, "%s: Error: To fill it, please run 'xrtpcgrade' task\n", global.taskname);
    headas_chat(CHATTY, "%s: Error: on '%s' event file .\n", global.taskname, global.par.infile);
    goto event_end;
  }

  /* Get table structure */
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  nCols=outtable.nColumns;

  /* Get input cols index from name */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname,CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if ((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  /* CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }


  /* Modify bintable */
  
  /* Add new columns */
  /* Check if PI column exists else add it */
  if((indxcol.PI=ColNameMatch(CLNM_PI, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI, CARD_COMM_PI, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI=ColNameMatch(CLNM_PI, &outtable);
      sprintf(comm, "Added PI column");
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
    }
  else
    {
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PI+1));
      if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
	AddCard(&head, KeyWord, J, &null_pi, CARD_COMM_TNULL);
      
      /* Add TLMIN keyword */
      sprintf(KeyWord, "TLMAX%d", (indxcol.PI+1));
      AddCard(&head, KeyWord, J, &max_pi, CARD_COMM_TLMAX);
    }

 
  /* Check if PI_NOM column exists else add it */
  if((indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI_NOM, CARD_COMM_PI_NOM, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable);
/*       sprintf(comm, "Added PI_NOM column"); */
/*       headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_NOM); */
/*       headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   */ 
    }


  /* Check if PI_OFFSET column exists else add it */
  if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE))){
    if((indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable)) == -1)
      {
	headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_OFFSET);
	headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	AddColumn(&head, &outtable, CLNM_PI_OFFSET, CARD_COMM_PI_OFFSET, "1J", TNONE);
	indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable);
      }
  }else{
    indxcol.PI_OFFSET=-1;
  }
  
  /* Check if STATUS column exists else add it */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled \n", global.taskname, CLNM_STATUS); 
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
      sprintf(comm, "Added %s column", CLNM_STATUS);
    }
  else
    headas_chat(CHATTY, "%s: Info: %s column will be updated.\n", global.taskname, CLNM_STATUS);   

  /* Add history */
  /* Get date */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: ", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "%s.", comm);
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "Used %s gain CALDB file", global.par.gainfile);
      AddHistory(&head, global.strhist);
      
    }
  EndBintableHeader(&head, &outtable); 
  
  /*	write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  
/*   /\* initialize random number generator *\/ */
/*   if (global.par.randomflag) */
/*     { */
/*   seed=(long int)global.par.seed; */
/*   get_ran2seed(&seed); */
/*     } */
  /* Read input bintable modify and write it in output file */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) ==0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  
	  /* Get value needed to calculate PI */
	  etime=DVEC(outtable, n, indxcol.TIME);
	  rawx=IVEC(outtable, n, indxcol.RAWX);
	  rawy=IVEC(outtable, n, indxcol.RAWY);
	  pha=JVEC(outtable, n, indxcol.PHA);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);

	  if (first_time || (ccdframe != ccdframe_old) || !(time_old >= etime-sens && time_old <= etime+sens))
	    {
	      first_time=0;
	      for (; iii < global.count ; iii++)
		{
		  if(ccdframe == global.temp[iii].hkccdframe && 
		     (global.temp[iii].hktime >= etime-sens && global.temp[iii].hktime <= etime+sens))
		    {
		      temp=global.temp[iii].temp;
		      headas_chat(CHATTY, "%s: Info: Used %5.7f temperature for %d ccdframe.\n",global.taskname, temp, ccdframe);
		      ccdframe_old=ccdframe;
		      time_old=etime;
		      new_coeff=1;
		      goto temp_found;
		    }
		}
	      headas_chat(NORMAL, "%s: Error: Unable to get temperature value.\n", global.taskname);
	      goto event_end;
	    }
	
	temp_found:
	  
	  if(newstatus)
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	  /* If PHA=NULL, PI=NULL */

	  if (pha==KWVL_PHANULL || !finite(etime))
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;
	      JVEC(outtable, OutRows, indxcol.PI_NOM)=KWVL_PINULL;
	      /* If PHA=0, PI=0 */
	    }
	  else if (pha==0)
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=0;
	      JVEC(outtable, OutRows, indxcol.PI_NOM)=0;
	    }
	  else if (pha >= PHA_MAX)
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
	      JVEC(outtable, OutRows, indxcol.PI_NOM)=PI_MAX;
	      XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
	    }
	  else
	    { /* Calculate PI */

	      if(new_coeff)
		{
		  new_coeff=0;
		  if(CalculateGainCoeff(etime, temp, coeff, coeff_corr))
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to calculate\n", global.taskname);
		      headas_chat(NORMAL, "%s: Error: GAIN coefficients.\n", global.taskname);
		      goto event_end;		    
			
		    }
		  headas_chat(CHATTY, " %s: Info: Gain coefficients for ccdframe=%d :\n", global.taskname,ccdframe);
		  headas_chat(CHATTY, " %s: Info: GC0=%f GC1=%f GC2=%f GC3=%f GC4=%f GC5=%f GC3_TRAP=%f\n", global.taskname,coeff[0],coeff[1],coeff[2],
			      coeff[3],coeff[4],coeff[5]);
		  headas_chat(CHATTY, " %s: Info: GC0_TRAP=%f GC1=%f GC2_TRAP=%f GC3_TRAP=%f GC4_TRAP=%f GC5_TRAP=%f\n", global.taskname,coeff[6],coeff[7],coeff[8],
			      coeff[9],coeff[10],coeff[11]);

		}


	      if(ComputePI(rawx, rawy, pha, etime, temp, gain, coeff, coeff_corr, &pi_nom, &pi, &pi_offset)){
		goto event_end;
	      }
	      
	      /* Update PI_NOM column */
	      if (pi_nom < 0)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=KWVL_PINULL;	      
	      else if (pi_nom < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=(int)pi_nom;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI_NOM)=PI_MAX;
		}

	      /* Update PI column */
	      if (pi < 0)
		JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;	      
	      else if (pi < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI)=(int)pi;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
		  XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
		}

	      /* Update PI_OFFSET column */
	      if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE)))
		JVEC(outtable, OutRows, indxcol.PI_OFFSET)=(int)(pi_offset+0.5);


	    }/* else calculate pi */
	  
	  
	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    } /* if(++OutRows>=BINTAB_ROWS) */
	} /* for end */
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);
  

  if(!global.par.savepinom){
    /* Delete PI_NOM column */
    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_PI_NOM, &pinomcol, &status))
      if(fits_delete_col(ounit, pinomcol, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_PI_NOM);
	  goto event_end;
	}
  }
  
  if(fits_update_key(ounit, TSTRING, KWNM_XPITYPE, global.par.corrtype, CARD_COMM_XPITYPE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XPITYPE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto event_end;
    }


  return OK;
 event_end:
  
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* PhotonCountingPha2Pi */

/*
 *
 *      WindowedTimingPha2Pi
 *
 *	DESCRIPTION:
 * 		This routine adds input event bintable modified in output file.   
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *          void  GetGMTDateTime(char *);
 *
 *          headas_chat(int ,char *, ...);
 *
 *          FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *          void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     		            	       const int MaxBlockRows, const unsigned nColumns,
 *  				       const unsigned ActCols[]);
 *          int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *          void AddHistory(FitsHeader_t *header, const char *Comment);
 *          void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *          void FinishBintableHeader(const FitsFileUnit_t unit, 
 *                                    FitsHeader_t *header, Bintable_t *table); *
 *          int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                            const unsigned nColumns, const unsigned ActCols[], 
 *                            const unsigned FromRow, unsigned *nRows);
 *          int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, 
 *                                unsigned nRows, BOOL last);
 *          unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *
 *          void get_ran2seed(long int)
 *          int sprintf(char *str, const char *format, ...); 
 *
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 31/10/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int WindowedTimingPha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit)
{
  int                pha, newstatus=0, ccdframe=0, ccdframe_old=0;
  int                status=OK, rawx, rawy, dety, iii=0, new_coeff=1,pinomcol;
  BOOL               first_time=1;
  char               comm[256]="", KeyWord[FLEN_KEYWORD];
  double             gain, etime, sens=0.0000001;
  double             coeff[GAINCOEFF_NUM_NEW], rotime=0.0, time_old=0.0, temp=0.0;
  double             coeff_corr[6], pi=0.0, pi_nom=0.0, pi_offset=0.0;
  JTYPE              null_pi=KWVL_PINULL, max_pi=PI_MAX;
  unsigned           FromRow, ReadRows, n, nCols, OutRows=0;  
/*   long int           seed; */
  Ev2Col_t           indxcol; 
  Bintable_t	     outtable;
  FitsHeader_t	     head;


  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  if( ReadTemperatures())
    {
      headas_chat(NORMAL, " %s: Error: Unable to read %s column\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, " %s: Error: in %s file.\n", global.taskname, global.par.hdfile);
    }

  /* Transform gain in KeV */
  if (global.par.gainnom <= 0)
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n",  global.taskname,global.nomgain);
      headas_chat(CHATTY, "%s: Info: from %s GAIN file.\n", global.taskname,  global.par.gainfile);
      gain=global.nomgain/1000.0;
  
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n", global.taskname, global.par.gainnom);
      gain=global.par.gainnom/1000.0;

    }


  /* Read input event bintable */
  head=RetrieveFitsHeader(evunit);

  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  nCols=outtable.nColumns;
  
  /* Get input cols index from name */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if ((indxcol.DETY=ColNameMatch(CLNM_DETY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }
  
  
  /* Get input cols index from name */
  if ((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }
  
  /* Modify bintable */
  
  /* Add new columns */
  /* Check if PI column exists else add it */
  if((indxcol.PI=ColNameMatch(CLNM_PI, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI, CARD_COMM_PI, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI=ColNameMatch(CLNM_PI, &outtable);
      sprintf(comm, "Added PI column");
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n", global.taskname, CLNM_PI);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
    }
  else
    {
      
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PI+1));
      if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
	AddCard(&head, KeyWord, J, &null_pi, CARD_COMM_TNULL);
      /* Add TLMIN keyword */
      sprintf(KeyWord, "TLMAX%d", (indxcol.PI+1));
      AddCard(&head, KeyWord, J, &max_pi, CARD_COMM_TLMAX);

    } 

  /* Check if PI_NOM column exists else add it */
  if((indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI_NOM, CARD_COMM_PI_NOM, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable);
/*       sprintf(comm, "Added PI_NOM column"); */
/*       headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_NOM); */
/*       headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    */
    }


  /* Check if PI_OFFSET column exists else add it */
  if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE))){
    if((indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable)) == -1)
      {
	headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_OFFSET);
	headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	AddColumn(&head, &outtable, CLNM_PI_OFFSET, CARD_COMM_PI_OFFSET, "1J", TNONE);
	indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable);
      }
  }else{
    indxcol.PI_OFFSET=-1;
  }

  /* Check if STATUS column exists else add it */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n", global.taskname, CLNM_STATUS);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
      sprintf(comm, "Added %s column", CLNM_STATUS);
    }
  else
    headas_chat(CHATTY, "%s: Info: %s column will be updated.\n", global.taskname,  CLNM_STATUS);   

  /* CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timing_end;
    }


  /* Add history */
  /* Get date */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: ", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "%s.", comm);
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "Used %s gain CALDB file", global.par.gainfile );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "The conversion from DETY to RAWY is: RAWY = DETY - 1");
      AddHistory(&head, global.strhist);

    }
  EndBintableHeader(&head, &outtable); 
  
  /*	write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  
/*   /\* initialize random number generator *\/ */
/*   if (global.par.randomflag) */
/*     { */
/*       seed=(long int)global.par.seed; */
/*       get_ran2seed(&seed); */
/*     } */

  /* Read input bintable modify and write it in output file */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) ==0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  /* Get value needed to calculate PI */
	  etime=DVEC(outtable, n, indxcol.TIME);
	  rotime=DVEC(outtable, n, indxcol.TIME_RO);
	  rawx=IVEC(outtable, n, indxcol.RAWX);
	  pha=JVEC(outtable, n, indxcol.PHA);
	  dety=IVEC(outtable, n, indxcol.DETY);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);

	  if (first_time || (ccdframe != ccdframe_old) || !(time_old >= rotime-sens && time_old <= rotime+sens))
	    {
	      first_time=0;
	      for (; iii < global.count ; iii++)
		{
		  if(ccdframe == global.temp[iii].hkccdframe && 
		     (global.temp[iii].hktime >= rotime-sens && global.temp[iii].hktime <= rotime+sens))
		    {
		      temp=global.temp[iii].temp;
		      headas_chat(CHATTY, "%s: Info: Used %5.7f temperature for %d ccdframe.\n",global.taskname, temp, ccdframe);
		      ccdframe_old=ccdframe;
		      time_old=rotime;
		      new_coeff=1;
		      
	      	      goto temp_found;
		    }
		}
	      headas_chat(NORMAL, "%s: Error: Unable to get temperature value.\n", global.taskname);
	      goto timing_end;
	    }
	

	temp_found:
	   
	  
	  /* Source RAWY Position */
	  rawy = dety - 1;

	  if(newstatus)
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	  /* If PHA=NULL, PI=NULL */
	  
	  if(!(rawy >= RAWY_MIN && rawy <= RAWY_MAX) || !finite(etime))
	    {
	      XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
	      JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;
	    }
	  else if (pha==KWVL_PHANULL)
	    JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;
	  /* If PHA=NULL, PI=NULL */
	  else if (pha==0)
	    JVEC(outtable, OutRows, indxcol.PI)=0;
	  else if (pha >= PHA_MAX)
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
	      XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
	    }
	  else
	    {
	      if(new_coeff)
		{
		  new_coeff=0;
		  if(CalculateGainCoeff(etime, temp, coeff, coeff_corr))
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to calculate\n", global.taskname);
		      headas_chat(NORMAL, "%s: Error: GAIN coefficients.\n", global.taskname);
		      goto timing_end;		    
			
		    }
		  headas_chat(CHATTY, " %s: Info: Gain coefficients for ccdframe=%d :\n", global.taskname,ccdframe);
		  headas_chat(CHATTY, " %s: Info: GC0=%f GC1=%f GC2=%f GC3=%f GC4=%f GC5=%f\n", global.taskname,coeff[0],coeff[1],coeff[2],
			      coeff[3],coeff[4],coeff[5]);
		  headas_chat(CHATTY, " %s: Info: GC0_TRAP=%f GC1_TRAP=%f GC2_TRAP=%f GC3_TRAP=%f GC4_TRAP=%f GC5_TRAP=%f\n", global.taskname,coeff[6],coeff[7],coeff[8],
			      coeff[9],coeff[10],coeff[11]);

		}


	      if(ComputePI(rawx, rawy, pha, etime, temp, gain, coeff, coeff_corr, &pi_nom, &pi, &pi_offset)){
		goto timing_end;
	      }
	      
	      /* Update PI_NOM column */
	      if (pi_nom < 0)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=KWVL_PINULL;	      
	      else if (pi_nom < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=(int)pi_nom;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI_NOM)=PI_MAX;
		}

	      /* Update PI column */
	      if (pi < 0)
		JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;	      
	      else if (pi < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI)=(int)pi;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
		  XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
		}

	      /* Update PI_OFFSET column */
	      if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE)))
		JVEC(outtable, OutRows, indxcol.PI_OFFSET)=(int)(pi_offset+0.5);	  


	    }/* else calculate pi */
	      
	  
	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    } /* if(++OutRows>=BINTAB_ROWS) */
	} /* for end */
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);


  if(!global.par.savepinom){
    /* Delete PI_NOM column */
    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_PI_NOM, &pinomcol, &status))
      if(fits_delete_col(ounit, pinomcol, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_PI_NOM);
	  goto timing_end;
	}
  }

  if(fits_update_key(ounit, TSTRING, KWNM_XPITYPE, global.par.corrtype, CARD_COMM_XPITYPE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XPITYPE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto timing_end;
    }
  
  return OK;
 timing_end:
  
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* WindowedTimingPha2Pi */

/*
 *
 *      PhotodiodePha2Pi
 *
 *	DESCRIPTION:
 * 		This routine adds input event bintable modified in output file.   
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *
 *          int GetGainFileCoeff(void);
 *          void  GetGMTDateTime(char *);
 *
 *          headas_chat(int ,char *, ...);
 *
 *          FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *          void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     		            	       const int MaxBlockRows, const unsigned nColumns,
 *  				       const unsigned ActCols[]);
 *          int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *          void AddHistory(FitsHeader_t *header, const char *Comment);
 *          void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *          void FinishBintableHeader(const FitsFileUnit_t unit, 
 *                                    FitsHeader_t *header, Bintable_t *table); *
 *          int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                            const unsigned nColumns, const unsigned ActCols[], 
 *                            const unsigned FromRow, unsigned *nRows);
 *          int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, 
 *                                unsigned nRows, BOOL last);
 *          unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *
 *          void get_ran2seed(long int)
 *
 *
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - FT 08/05/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PhotodiodePha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit)
{
  int                pha, newstatus=0, ccdframe=0, ccdframe_old=0, iii=0;
  int                status=OK, rawx, rawy, detx, dety, amp=0, new_coeff=1,pinomcol;
  BOOL               first_time=1;
  char               comm[256]="", KeyWord[FLEN_KEYWORD];
  double             gain, rotime=0.0, time_old=0.0,sens=0.0000001, temp=0.0;
  double             coeff[GAINCOEFF_NUM_NEW], etime;
  double             coeff_corr[6], pi=0.0, pi_nom=0.0, pi_offset=0.0;
  JTYPE              null_pi=KWVL_PINULL, max_pi=PI_MAX;
  unsigned           FromRow, ReadRows, n, nCols, OutRows=0;  
/*   long int           seed; */
  Ev2Col_t           indxcol; 
  Bintable_t	     outtable;
  FitsHeader_t	     head;


  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  

  if( ReadTemperatures())
    {
      headas_chat(NORMAL, " %s: Error: Unable to read %s column\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, " %s: Error: in %s file.\n", global.taskname, global.par.hdfile);
    }

  /* Transform gain in KeV */
  if (global.par.gainnom <= 0)
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n",  global.taskname,global.nomgain);
      headas_chat(CHATTY, "%s: Info: from %s GAIN file.\n", global.taskname,  global.par.gainfile);
      gain=global.nomgain/1000.0;
 
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: Using gain value: %f\n", global.taskname, global.par.gainnom);
      gain=global.par.gainnom/1000.0;
      
    }


  /* Read input event bintable */
  head=RetrieveFitsHeader(evunit);

  /* Check if PHA is already computed */
  if((!(ExistsKeyWord(&head, KWNM_XRTPHACO  , NULLNULLCARD)) || !(GetLVal(&head, KWNM_XRTPHACO))) &&
     (!(ExistsKeyWord(&head, KWNM_BIASONBD  , NULLNULLCARD)) || !(GetLVal(&head, KWNM_BIASONBD))) )
    {
      headas_chat(CHATTY, "%s: Error: %s or %s keyword not found or unset\n",global.taskname, KWNM_BIASONBD, KWNM_XRTPHACO);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      headas_chat(NORMAL, "%s: Error: No bias subtraction has been applied on %s column,\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: the %s values can not be calculated.\n", global.taskname, CLNM_PI);
      headas_chat(CHATTY, "%s: Error: Please run 'xrtpdcorr' task.\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: on '%s' event file .\n", global.taskname, global.par.infile);
      goto timing_end;
    }
  

  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  nCols=outtable.nColumns;
  
  /* Get input cols index from name */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }

  /* Get input cols index from name */
  if ((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if ((indxcol.DETX=ColNameMatch(CLNM_DETX, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETX);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if ((indxcol.DETY=ColNameMatch(CLNM_DETY, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DETY);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      
      goto timing_end;
    }

  if ((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
      goto timing_end;
    }
  

  /* Modify bintable */
  
  /* Add new columns */
  /* Check if PI column exist else add it */
  if((indxcol.PI=ColNameMatch(CLNM_PI, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI, CARD_COMM_PI, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI=ColNameMatch(CLNM_PI, &outtable);
      sprintf(comm, "Added PI column");
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n", global.taskname, CLNM_PI);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);        

    }
  else
    {
      
      /* Add TNULL keyword */
      sprintf(KeyWord, "TNULL%d", (indxcol.PI+1));
      if (!ExistsKeyWord(&head, KeyWord, NULLNULLCARD))
	AddCard(&head, KeyWord, J, &null_pi, CARD_COMM_TNULL);
      /* Add TLMIN keyword */
      sprintf(KeyWord, "TLMAX%d", (indxcol.PI+1));
      AddCard(&head, KeyWord, J, &max_pi, CARD_COMM_TLMAX);

    } 

  /* Check if PI_NOM column exists else add it */
  if((indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable)) == -1)
    {
      AddColumn(&head, &outtable, CLNM_PI_NOM, CARD_COMM_PI_NOM, "1J", TUNIT|TMIN|TMAX|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, PI_MIN, PI_MAX, KWVL_PINULL);
      indxcol.PI_NOM=ColNameMatch(CLNM_PI_NOM, &outtable);
/*       sprintf(comm, "Added PI_NOM column"); */
/*       headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_NOM); */
/*       headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    */
    }


  /* Check if PI_OFFSET column exists else add it */
  if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE))){
    if((indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable)) == -1)
      {
	headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PI_OFFSET);
	headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	AddColumn(&head, &outtable, CLNM_PI_OFFSET, CARD_COMM_PI_OFFSET, "1J", TNONE);
	indxcol.PI_OFFSET=ColNameMatch(CLNM_PI_OFFSET, &outtable);
      }
  }else{
    indxcol.PI_OFFSET=-1;
  }

  /* Check if STATUS column exists else add it */
  newstatus=0;
  if((indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable)) == -1)
    {
      newstatus=1;
      AddColumn(&head, &outtable,CLNM_STATUS,CARD_COMM_STATUS , "16X",TNONE);
      headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n", global.taskname, CLNM_STATUS);
      headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);    
      
      indxcol.STATUS=ColNameMatch(CLNM_STATUS, &outtable);
      sprintf(comm, "Added %s column", CLNM_STATUS);
    }
  else
    headas_chat(CHATTY, "%s: Info: %s column will be updated.\n", global.taskname,CLNM_STATUS); 


  /* CCDFrame */
  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timing_end;
    }

  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto timing_end;
    }




  /* Add history */
  /* Get date */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: ", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "%s.", comm);
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "Used %s gain CALDB file", global.par.gainfile );
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "The conversion from DET to RAW is:");
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "AMP1:");
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "RAWX = DETX - 1;   RAWY = DETY - 1 ");
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "AMP2:");
      AddHistory(&head, global.strhist);
      sprintf(global.strhist, "RAWX = 600 - DETX;   RAWY = DETY - 1 ");
      AddHistory(&head, global.strhist);
    }
  EndBintableHeader(&head, &outtable); 
  
  /*	write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows=0;
  
/*   /\* initialize random number generator *\/ */
/*   if (global.par.randomflag) */
/*     { */
/*       seed=(long int)global.par.seed; */
/*       get_ran2seed(&seed); */
/*     } */

  /* Read input bintable modify and write it in output file */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) ==0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  
	  /* Get value needed to calculate PI */
	  etime=DVEC(outtable, n, indxcol.TIME);
	  rotime=DVEC(outtable, n, indxcol.TIME_RO);
	  pha=JVEC(outtable, n, indxcol.PHA);
	  detx=IVEC(outtable, n, indxcol.DETX);
	  dety=IVEC(outtable, n, indxcol.DETY);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);

	  if (first_time || (ccdframe != ccdframe_old) || (!(time_old >= rotime-sens && time_old <= rotime+sens)))
	    {
	      for (; iii < global.count ; iii++)
		{
		  first_time=0;
		  if(ccdframe == global.temp[iii].hkccdframe && 
		     (global.temp[iii].hktime >= rotime-sens && global.temp[iii].hktime <= rotime+sens))
		    {
		      temp=global.temp[iii].temp;
		      headas_chat(CHATTY, "%s: Info: Used %5.7f temperature for %d ccdframe.\n",global.taskname, temp, ccdframe);
		      ccdframe_old=ccdframe;
		      time_old=rotime;
		      new_coeff=1;
		      goto temp_found;
		    }
		}
	      headas_chat(NORMAL, "%s: Error: Unable to get temperature value.\n", global.taskname);
	      goto timing_end;
	    }
	

	temp_found:
	
	  if (outtable.TagVec[indxcol.Amp] == B)
	    amp=BVEC(outtable, n,indxcol.Amp);
	  else
	    amp=IVEC(outtable, n,indxcol.Amp);

	  if (amp != AMP1 && amp != AMP2)
	    {
	      headas_chat(NORMAL,"%s: Error: Amplifier Number: %d not allowed.\n", global.taskname,amp);
	      headas_chat(CHATTY,"%s: Error: Values allowed for amplifier are: %d and %d.\n", global.taskname, AMP1, AMP2);
	      goto timing_end;
	    }
	  
	  if(amp == AMP1)
	    rawx=detx - 1;
	  else
	    rawx=600-detx;
	  rawy=dety-1;
	  
	  if(newstatus)
	    XVEC2BYTE(outtable,n,indxcol.STATUS)=0;
	
	  /* If PHA=NULL, PI=NULL */
	  if(!((rawx >= RAWX_MIN && rawx <= RAWX_MAX) && ( rawy >= RAWY_MIN && rawy <= RAWY_MAX))|| !finite(etime))
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;
	      XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
	    }

	  else if (pha==KWVL_PHANULL)
	    JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;
	  else if (pha==0)
	    JVEC(outtable, OutRows, indxcol.PI)=0;

	  else if (pha >= PHA_MAX)
	    {
	      JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
	      XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
	    }
	  else
	    { /* Calculate PI */
  
	      if(new_coeff)
		{
		  new_coeff=0;
		  if(CalculateGainCoeff(etime, temp, coeff, coeff_corr))
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to calculate\n", global.taskname);
		      headas_chat(NORMAL, "%s: Error: GAIN coefficients.\n", global.taskname);
		      goto timing_end;		    
			
		    }
		  headas_chat(CHATTY, " %s: Info: Gain coefficients for ccdframe=%d :\n", global.taskname,ccdframe);
		  headas_chat(CHATTY, " %s: Info: GC0=%f GC1=%f GC2=%f GC3=%f GC4=%f GC5=%f\n", global.taskname,coeff[0],coeff[1],coeff[2],
			      coeff[3],coeff[4],coeff[5]);
		  headas_chat(CHATTY, " %s: Info: GC0_TRAP=%f GC1_TRAP=%f GC2_TRAP=%f GC3_TRAP=%f GC4_TRAP=%f GC5_TRAP=%f\n", global.taskname,coeff[6],coeff[7],coeff[8],
			      coeff[9],coeff[10],coeff[11]);

		}


	      if(ComputePI(rawx, rawy, pha, etime, temp, gain, coeff, coeff_corr, &pi_nom, &pi, &pi_offset)){
		goto timing_end;
	      }
	      
	      /* Update PI_NOM column */
	      if (pi_nom < 0)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=KWVL_PINULL;	      
	      else if (pi_nom < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI_NOM)=(int)pi_nom;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI_NOM)=PI_MAX;
		}

	      /* Update PI column */
	      if (pi < 0)
		JVEC(outtable, OutRows, indxcol.PI)=KWVL_PINULL;	      
	      else if (pi < PI_MAX)
		JVEC(outtable, OutRows, indxcol.PI)=(int)pi;
	      else 
		{
		  JVEC(outtable, OutRows, indxcol.PI)=PI_MAX;
		  XVEC2BYTE(outtable,OutRows,indxcol.STATUS)|=EV_BAD;
		}

	      /* Update PI_OFFSET column */
	      if( global.par.savepioffset && (!stricmp(global.par.corrtype,TOTAL_MODE) || !stricmp(global.par.corrtype,TRAPS_MODE)))
		JVEC(outtable, OutRows, indxcol.PI_OFFSET)=(int)(pi_offset+0.5);


	    }/* else calculate pi */
	  
      
	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    } /* if(++OutRows>=BINTAB_ROWS) */
	} /* for end */
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);


  if(!global.par.savepinom){
    /* Delete PI_NOM column */
    if(!fits_get_colnum(ounit, CASEINSEN, CLNM_PI_NOM, &pinomcol, &status))
      if(fits_delete_col(ounit, pinomcol, &status))
	{
	  headas_chat(NORMAL, "&s: Error removing %s column.\n", global.taskname, CLNM_PI_NOM);
	  goto timing_end;
	}
  }

  if(fits_update_key(ounit, TSTRING, KWNM_XPITYPE, global.par.corrtype, CARD_COMM_XPITYPE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_XPITYPE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, global.par.outfile);
      goto timing_end;
    }
  
  return OK;
 timing_end:
  
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* PhotodiodePha2Pi */



/*
 *	GetGainFileCoeff:
 *
 *
 *	DESCRIPTION:
 *            Get coefficients from gain file ( CALDB or user defined ).
 *        
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             headas_chat(int, char *, ...);
 *             
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             double GetDVal(FitsHeader_t, char *);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, 
 *                                        const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  GetColNameIndx(const Bintable_t *table, const char *ColName);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, 
 *                               const unsigned nColumns, const unsigned ActCols[], 
 *                               const unsigned FromRow, unsigned *nRows); 
 *             unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *
 *
 *             ISXRTFILE_WARNING(int, FitsFileUnit_t, char *,char *);
 *
 *             int fits_movnam_hdu(fitsfile *fptr, int hdutype, char *extname, 
 *                                 int extver, int * status);
 *
 *
 *	CHANGE HISTORY:
 *             08/05/2003 - FT Read the Gain CALDB file related to the
 *                          Instrument Data Mode
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

int GetGainFileCoeff(void)
{

  int            n, status=OK;
  long           extno;
  char           expr[256];
  unsigned       FromRow, ReadRows, OutRows, nCols, i,jj; 
  GainCol_t      gaincol; 
  Bintable_t     table;             /* Bintable pointer */  
  FitsHeader_t   head;              /* Extension pointer */
  FitsFileUnit_t gunit=NULL;        /* Gain file pointer */
  FitsCard_t     *card;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  extno=-1;
  
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.gainfile, DF_CALDB)))
    {
      /* Set CALDB data type related to the Instrument Data Mode*/
      sprintf(expr, "datamode.eq.%s.and.xrtvsub.eq.%d", global.datamode, global.xrtvsub);
      if (CalGetFileName(HD_MAXRET,  DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_GAIN_DSET, global.par.gainfile, expr, &extno))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.gainfile);
      extno++;
    }

  /* Open read only gain file */
  if ((gunit=OpenReadFitsFile(global.par.gainfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to open '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
 
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,gunit,global.par.gainfile,global.taskname);

  if(extno != -1)
    {
      /* move to  PH2PI extension */
      if (fits_movabs_hdu(gunit,(int)(extno),NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to move in '%d' HDU\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
	  goto get_end;
	} 
    }
  else
    {
      /* move to  PH2PI extension */
      if (fits_movnam_hdu(gunit,ANY_HDU,KWVL_EXTNAME_PH2PI,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PH2PI);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
	  goto get_end;
	} 
    }

  head = RetrieveFitsHeader(gunit);

  /* Get nominal gain */
  if(ExistsKeyWord(&head, KWNM_NOM_GAIN, &card))
    global.nomgain=card->u.DVal;
  else
    {
      headas_chat(NORMAL,"%s: Error: %s keyword not found\n", global.taskname, KWNM_NOM_GAIN);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }

 
  /* Read ph2pi bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.gainfile);
      goto get_end;
    }


  nCols=table.nColumns;
      
  /* Get columns index from name */
  if ((gaincol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }

  /* Get columns index from name */
  if ((gaincol.CCDTEMP = GetColNameIndx(&table, CLNM_CCDTEMP)) == -1 )
    {
      global.oldgainformat=1;
      headas_chat(NORMAL,"%s: Warning: '%s' column not found\n", global.taskname, CLNM_CCDTEMP);
      headas_chat(NORMAL,"%s: Warning: '%s' file format is obsolete.\n",global.taskname, global.par.gainfile);
      
    }
  else
    global.oldgainformat=0;
  
  if ((gaincol.GC0 = GetColNameIndx(&table, CLNM_GC0)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC0);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC1 = GetColNameIndx(&table, CLNM_GC1)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC1);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC2 = GetColNameIndx(&table, CLNM_GC2)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC2);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC3 = GetColNameIndx(&table, CLNM_GC3)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC3);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC4 = GetColNameIndx(&table, CLNM_GC4)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC4);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC5 = GetColNameIndx(&table, CLNM_GC5)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC5);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC0_TRAP = GetColNameIndx(&table, CLNM_GC0_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC0_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC1_TRAP = GetColNameIndx(&table, CLNM_GC1_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC1_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC2_TRAP = GetColNameIndx(&table, CLNM_GC2_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC2_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC3_TRAP = GetColNameIndx(&table, CLNM_GC3_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC3_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC4_TRAP = GetColNameIndx(&table, CLNM_GC4_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC4_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  if ((gaincol.GC5_TRAP = GetColNameIndx(&table, CLNM_GC5_TRAP)) == -1 )
    {
      headas_chat(NORMAL,"%s: Error: '%s' column not found\n", global.taskname, CLNM_GC5_TRAP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  

  global.offset2format=1;

  if ((gaincol.RAWX = GetColNameIndx(&table, "RAWX")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "RAWX");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }
  else
    {
      global.caldbdim = table.Multiplicity[gaincol.RAWX];
    }

  if ((gaincol.RAWY = GetColNameIndx(&table, "RAWY")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "RAWY");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.YEXTENT = GetColNameIndx(&table, "YEXTENT")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "YEXTENT");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.OFFSET = GetColNameIndx(&table, "OFFSET")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "OFFSET");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.ALPHA1 = GetColNameIndx(&table, "ALPHA1")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "ALPHA1");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.ALPHA2 = GetColNameIndx(&table, "ALPHA2")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "ALPHA2");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.EBREAK = GetColNameIndx(&table, "EBREAK")) == -1 )
    {
      global.offset2format=0;
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "EBREAK");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.BETA1 = GetColNameIndx(&table, "BETA1")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "BETA1");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.BETA2 = GetColNameIndx(&table, "BETA2")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "BETA2");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.E_CTI = GetColNameIndx(&table, "E_CTI")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "E_CTI");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.BETA1_TRAP = GetColNameIndx(&table, "BETA1_TRAP")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "BETA1_TRAP");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.BETA2_TRAP = GetColNameIndx(&table, "BETA2_TRAP")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "BETA2_TRAP");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if ((gaincol.E_CTI_TRAP = GetColNameIndx(&table, "E_CTI_TRAP")) == -1 )
    {
      headas_chat(CHATTY,"%s: Warning: '%s' column not found\n", global.taskname, "E_CTI_TRAP");
      headas_chat(CHATTY,"%s: Warning: old gain file format.\n",global.taskname);
    }

  if(global.offset2format &&
     (table.Multiplicity[gaincol.RAWX]!=table.Multiplicity[gaincol.RAWY] ||
      table.Multiplicity[gaincol.RAWX]!=table.Multiplicity[gaincol.YEXTENT] ||
      table.Multiplicity[gaincol.RAWX]!=(table.Multiplicity[gaincol.OFFSET]/3)))
    {
      headas_chat(NORMAL,"%s: Error: 'RAWX' 'RAWY' 'YEXTENT' and 'OFFSET' columns with bad multiplicity\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end; 
    }

  
  global.caldbrows = table.MaxRows;
  
  /* Allocate memory to storage all coefficients */
  global.gaincoeff=(GainRow_t *)calloc(global.caldbrows, sizeof(GainRow_t));

  /* Read blocks of bintable rows from input gain file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{

	  if(global.oldgainformat)
	    {
	      /* get columns value */
	      global.gaincoeff[i].gtime=DVEC(table,n,gaincol.TIME);
	      global.gaincoeff[i].gc0[0]=EVEC(table,n,gaincol.GC0);
	      global.gaincoeff[i].gc1[0]=EVEC(table,n,gaincol.GC1);
	      global.gaincoeff[i].gc2[0]=EVEC(table,n,gaincol.GC2);
	      global.gaincoeff[i].gc3[0]=EVEC(table,n,gaincol.GC3);
	      global.gaincoeff[i].gc4[0]=EVEC(table,n,gaincol.GC4);
	      global.gaincoeff[i].gc5[0]=EVEC(table,n,gaincol.GC5);
	     
	    }
	  else
	    {
	     
	      global.gaincoeff[i].gtime=DVEC(table,n,gaincol.TIME);

	      for (jj=0; jj< table.Multiplicity[gaincol.CCDTEMP]; jj++)
		global.gaincoeff[i].ccdtemp[jj]=EVECVEC(table,n,gaincol.CCDTEMP,jj);
 
	      for (jj=0; jj< table.Multiplicity[gaincol.GC0]; jj++)
		global.gaincoeff[i].gc0[jj]=EVECVEC(table,n,gaincol.GC0,jj);
	      
	      for (jj=0; jj< table.Multiplicity[gaincol.GC1]; jj++)
		global.gaincoeff[i].gc1[jj]=EVECVEC(table,n,gaincol.GC1,jj);
	      
	      for (jj=0; jj< table.Multiplicity[gaincol.GC2]; jj++)
		global.gaincoeff[i].gc2[jj]=EVECVEC(table,n,gaincol.GC2,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC3]; jj++)
		global.gaincoeff[i].gc3[jj]=EVECVEC(table,n,gaincol.GC3,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC4]; jj++)
		global.gaincoeff[i].gc4[jj]=EVECVEC(table,n,gaincol.GC4,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC5]; jj++)
		global.gaincoeff[i].gc5[jj]=EVECVEC(table,n,gaincol.GC5,jj);
 
	      for (jj=0; jj< table.Multiplicity[gaincol.GC0_TRAP]; jj++)
		global.gaincoeff[i].gc0_trap[jj]=EVECVEC(table,n,gaincol.GC0_TRAP,jj);
	      
	      for (jj=0; jj< table.Multiplicity[gaincol.GC1_TRAP]; jj++)
		global.gaincoeff[i].gc1_trap[jj]=EVECVEC(table,n,gaincol.GC1_TRAP,jj);
	      
	      for (jj=0; jj< table.Multiplicity[gaincol.GC2_TRAP]; jj++)
		global.gaincoeff[i].gc2_trap[jj]=EVECVEC(table,n,gaincol.GC2_TRAP,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC3_TRAP]; jj++)
		global.gaincoeff[i].gc3_trap[jj]=EVECVEC(table,n,gaincol.GC3_TRAP,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC4_TRAP]; jj++)
		global.gaincoeff[i].gc4_trap[jj]=EVECVEC(table,n,gaincol.GC4_TRAP,jj);

	      for (jj=0; jj< table.Multiplicity[gaincol.GC5_TRAP]; jj++)
		global.gaincoeff[i].gc5_trap[jj]=EVECVEC(table,n,gaincol.GC5_TRAP,jj);


	      if(global.offset2format)
		{
		  global.gaincoeff[i].rawx = malloc(global.caldbdim * sizeof(int));
		  for (jj=0; jj< table.Multiplicity[gaincol.RAWX]; jj++)
		    global.gaincoeff[i].rawx[jj]=IVECVEC(table,n,gaincol.RAWX,jj);      
		  
		  global.gaincoeff[i].rawy = malloc(global.caldbdim * sizeof(int));
		  for (jj=0; jj< table.Multiplicity[gaincol.RAWY]; jj++)
		    global.gaincoeff[i].rawy[jj]=IVECVEC(table,n,gaincol.RAWY,jj);
		  
		  global.gaincoeff[i].yextent = malloc(global.caldbdim * sizeof(int));
		  for (jj=0; jj< table.Multiplicity[gaincol.YEXTENT]; jj++)
		    global.gaincoeff[i].yextent[jj]=IVECVEC(table,n,gaincol.YEXTENT,jj);
		  
		  global.gaincoeff[i].offset2 = malloc(3*global.caldbdim * sizeof(int));
		  for (jj=0; jj< table.Multiplicity[gaincol.OFFSET]; jj++)
		    global.gaincoeff[i].offset2[jj]=EVECVEC(table,n,gaincol.OFFSET,jj);

		  global.gaincoeff[i].alpha1=EVEC(table,n,gaincol.ALPHA1);
		  global.gaincoeff[i].alpha2=EVEC(table,n,gaincol.ALPHA2);
		  global.gaincoeff[i].ebreak=EVEC(table,n,gaincol.EBREAK);
		}

	      global.gaincoeff[i].beta1=EVEC(table,n,gaincol.BETA1);
	      global.gaincoeff[i].beta2=EVEC(table,n,gaincol.BETA2);
	      global.gaincoeff[i].e_cti=EVEC(table,n,gaincol.E_CTI); 

	      global.gaincoeff[i].beta1_trap=EVEC(table,n,gaincol.BETA1_TRAP);
	      global.gaincoeff[i].beta2_trap=EVEC(table,n,gaincol.BETA2_TRAP);
	      global.gaincoeff[i].e_cti_trap=EVEC(table,n,gaincol.E_CTI_TRAP);  
	    }
 
	  i++;
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /*
    for (i=0; i < global.caldbrows; i++)
    {
    if(global.oldgainformat)
    {
    printf(" TIME == %5.5f -- G0==%f G1==%f\n G2==%f G3==%f G4==%f G5==%f\n", global.gaincoeff[i].gtime, global.gaincoeff[i].gc0[0],global.gaincoeff[i].gc1[0],global.gaincoeff[i].gc2[0],global.gaincoeff[i].gc3[0],global.gaincoeff[i].gc4[0],global.gaincoeff[i].gc5[0]);
    }
    else
    {
    printf("TIME == %5.5f \n",  global.gaincoeff[i].gtime);
    for (jj=0; jj< 3; jj++) 
    printf("0  CCDTEMP == %f -- G0==%f G1==%f\n G2==%f G3==%f G4==%f G5==%f\n", global.gaincoeff[i].ccdtemp[jj], global.gaincoeff[i].gc0[jj],global.gaincoeff[i].gc1[jj],global.gaincoeff[i].gc2[jj],global.gaincoeff[i].gc3[jj],global.gaincoeff[i].gc4[jj],global.gaincoeff[i].gc5[jj]);
    
    }
    }
  */

  /* Close gain pixels file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gainfile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
  
  if (gunit)
    CloseFitsFile(gunit);
  
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;
}/* GetGainFileCoeff */

/*
 *	xrtcalcpi_info:
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
 *               headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 17/06/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

void xrtcalcpi_info()
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE,"\t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file             :'%s'\n", global.par.infile);
  headas_chat(NORMAL,"Name of the input GAIN file              :'%s'\n",global.par.gainfile);
  headas_chat(NORMAL,"Name of the input HK Header file         :'%s'\n",global.par.hdfile);
  headas_chat(NORMAL,"Name of the output Event file            :'%s'\n",global.par.outfile);
  if(global.par.randomflag){
    headas_chat(CHATTY,"Randomize PHA values?                    : yes\n");
    headas_chat(CHATTY,"Input seed for number generator          : %d\n",global.par.seed);
  }
  else
    headas_chat(CHATTY,"Randomize PHA values?                    : no\n");
  headas_chat(NORMAL,"Nominal gain                             : %f\n",global.par.gainnom);
  headas_chat(NORMAL,"Offset in channel/energy relationship    : %f\n",global.par.offset);
  headas_chat(NORMAL,"Method to correct positional gain variation : %s\n",global.par.corrtype);
  if(global.par.userctcorrpar){
    headas_chat(NORMAL,"Break energy (by the user)                             : %f\n",global.par.ebreak);
    headas_chat(NORMAL,"Powerlaw index for Energy <= break energy (by the user): %f\n",global.par.alpha1);
    headas_chat(NORMAL,"Powerlaw index for Energy > break energy (by the user) : %f\n",global.par.alpha2);
  }
  headas_chat(NORMAL,"Number of iterations for charge traps correction : %d\n",global.par.offsetniter);

  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                   : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                   : no\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file?   : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file?   : no\n");
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtcalcpi_info */

/*
 *	MAIN
 *
 *
 *	DESCRIPTION:
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
 *             void get_history(BOOL);
 *             void GetSWXRTDASVersion(global.swxrtdas_v);
 *
 *             void xrtcalcpi_getpar(void);
 *             void xrtcalcpi_info(void); 
 * 	       void xrtcalcpi_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 23/05/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int xrtcalcpi(void)
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTCALCPI_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  if ( xrtcalcpi_getpar() == OK ) {
    
       
    if ( xrtcalcpi_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto calcpi_end;
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
    goto calcpi_end;

  return OK;

 calcpi_end:
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;

} /* xrtcalcpi */
int xrtcalcpi_checkinput(void)
{
  int             overwrite=0;
  char            stem[10] , ext[MAXEXT_LEN];
  pid_t           tmp;

  /* Check output file name */
  
  if(strcasecmp(global.par.outfile, DF_NONE))
    {
      if (FileExists(global.par.outfile))
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
		}/* remove */
	    }/* else headas_clobpar */
	}/* FileExists */
    }/* outfile != NONE */
  else
    { /* outfile == NONE input file will be overwritten */   
      GetFilenameExtension(global.par.infile, ext);
      /* Check if input file is compressed */
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
  
  /* Derive temporary event filename */
  tmp=getpid(); 
  sprintf(stem, "%dtmp", (int)tmp);
  if (overwrite)
    DeriveFileName(global.par.infile, global.tmpfile, stem);
  else
    DeriveFileName(global.par.outfile, global.tmpfile, stem);
  if(FileExists(global.tmpfile))
    {/* remove temporary file if it exists */
      if(remove (global.tmpfile) == -1)
	{
	  headas_chat(CHATTY, "%s: Error: Unable to remove\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto check_end;
	}
    }
  
  if(overwrite)
    strcpy(global.par.outfile, global.par.infile);
  

  if((stricmp(global.par.corrtype,TOTAL_MODE))
     &&(stricmp(global.par.corrtype,NOMINAL_MODE))
     &&(stricmp(global.par.corrtype,TRAPS_MODE))
     )
    {
      headas_chat(NORMAL, "%s: Error: '%s' not a valid parameter for corrtype.\n", global.taskname,global.par.corrtype);
      goto check_end;
    }

  return OK;

 check_end:
  return NOT_OK;

}

/* 
 * ReadTemperatures 
 */

int ReadTemperatures(void)
{

  int                status=OK,  hkmode=0, good_row=1;
  double             hktime;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[9];
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
      goto ReadTemperatures_end;
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
      goto ReadTemperatures_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);

  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  
  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }
  /* Frame Start Time */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }
  
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }
    
  /* Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }
  

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  if ((hkcol.CCDTemp=ColNameMatch(CLNM_CCDTemp, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDTemp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadTemperatures_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=9; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.FSTS;
  ActCols[2]=hkcol.FSTSub;
  ActCols[3]=hkcol.TIME;
  ActCols[4]=hkcol.CCDTemp;
  ActCols[5]=hkcol.XRTMode;
  ActCols[6]=hkcol.Settled;
  ActCols[7]=hkcol.In10Arcm;
  ActCols[8]=hkcol.InSafeM;

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
 
	  if( good_row && (hkmode == global.xrtmode))
	    {
	      if(global.count)
		{
		  global.count+=1;
		  global.temp = (Temp_t *)realloc(global.temp, ((global.count)*sizeof(Temp_t)));
		}
	      else
		{

		  global.count+=1;
		  global.temp = (Temp_t *)malloc(sizeof(Temp_t));  
		  

		}/* End memory allocation */

	      if(global.xrtmode == XRTMODE_PC)
		hktime = DVEC(hktable, n, hkcol.TIME);
	      else
		hktime=(VVEC(hktable, n, hkcol.FSTS) + ((20.*UVEC(hktable, n, hkcol.FSTSub))/1000000.));
	     		
	      global.temp[global.count - 1].hkccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.temp[global.count - 1].hktime=hktime;
	      global.temp[global.count - 1].temp=EVEC(hktable, n, hkcol.CCDTemp);;
	      
	    }
	  
	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.count)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  ReadTemperatures_end;
    }


  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 ReadTemperatures_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* ReadTemperatures */

/*
 *
 * CalculateGainCoeff
 *
 */
int CalculateGainCoeff(double time, double temp, double coeff[GAINCOEFF_NUM_NEW], double coeff_corr[6])
{
  
  int    ii=0;
  double deltat, frac1, frac2,sens=0.0000001;
  double coeff_1[GAINCOEFF_NUM_NEW], coeff_2[GAINCOEFF_NUM_NEW];


  if(global.oldgainformat)
    {
      
      /* Calculate coefficients */
      /* If in gain file there is only one row get coefficient values */
      if (global.caldbrows == 1 || (time < (LAUNCH_SECS + sens)) || ( time <= (global.gaincoeff[0].gtime + sens)))
	{
	  coeff[0]=global.gaincoeff[0].gc0[0];
	  coeff[1]=global.gaincoeff[0].gc1[0];
	  coeff[2]=global.gaincoeff[0].gc2[0];
	  coeff[3]=global.gaincoeff[0].gc3[0];
	  coeff[4]=global.gaincoeff[0].gc4[0];
	  coeff[5]=global.gaincoeff[0].gc5[0];
	}
      else if (( time >= (global.gaincoeff[0].gtime - sens)) && (time <= (global.gaincoeff[global.caldbrows-1].gtime + sens))) 
	{
	  /* Search right row */
	  for (ii=0; time > (global.gaincoeff[ii].gtime + sens) || ii > global.caldbrows; ii++)
	    ;
	  if ((time >=  (global.gaincoeff[ii].gtime - sens)) && (time <=  (global.gaincoeff[ii].gtime + sens)))
	    {
	      
	      coeff[0]=global.gaincoeff[ii].gc0[0];
	      coeff[1]=global.gaincoeff[ii].gc1[0];
	      coeff[2]=global.gaincoeff[ii].gc2[0];
	      coeff[3]=global.gaincoeff[ii].gc3[0];
	      coeff[4]=global.gaincoeff[ii].gc4[0];
	      coeff[5]=global.gaincoeff[ii].gc5[0];
	    }
	  
	  else 
	    {
	      
	      /* Interpolate between two successive rows */
	      deltat=global.gaincoeff[ii].gtime - global.gaincoeff[ii-1].gtime;
	      frac1= (global.gaincoeff[ii].gtime - time)/deltat;
	      frac2= (time - global.gaincoeff[ii-1].gtime)/deltat;
	    

	      coeff[0]=global.gaincoeff[ii - 1].gc0[0] * frac1 + global.gaincoeff[ii].gc0[0] * frac2;
	      coeff[1]=global.gaincoeff[ii - 1].gc1[0] * frac1 + global.gaincoeff[ii].gc1[0] * frac2;
	      coeff[2]=global.gaincoeff[ii - 1].gc2[0] * frac1 + global.gaincoeff[ii].gc2[0] * frac2;
	      coeff[3]=global.gaincoeff[ii - 1].gc3[0] * frac1 + global.gaincoeff[ii].gc3[0] * frac2;
	      coeff[4]=global.gaincoeff[ii - 1].gc4[0] * frac1 + global.gaincoeff[ii].gc4[0] * frac2;
	      coeff[5]=global.gaincoeff[ii - 1].gc5[0] * frac1 + global.gaincoeff[ii].gc5[0] * frac2;
	      
	    }/* else interpolate */
	  
	}
      else
	{/* if time > last rows time, get last row coefficients */
	  
	  coeff[0]=global.gaincoeff[global.caldbrows - 1].gc0[0];
	  coeff[1]=global.gaincoeff[global.caldbrows - 1].gc1[0];
	  coeff[2]=global.gaincoeff[global.caldbrows - 1].gc2[0];
	  coeff[3]=global.gaincoeff[global.caldbrows - 1].gc3[0];
	  coeff[4]=global.gaincoeff[global.caldbrows - 1].gc4[0];
	  coeff[5]=global.gaincoeff[global.caldbrows - 1].gc5[0];
	  
	}
    }
  else /* CALDB GAIN FILE NEW FORMAT */
    {
      
      /* Calculate coefficients */
      /* If in gain file there is only one row get coefficient values */
      if ((global.caldbrows == 1) || (time < (LAUNCH_SECS + sens)) || (time < (global.gaincoeff[0].gtime + sens)))
	{
	  
	  coeff_corr[0]=global.gaincoeff[0].beta1;
	  coeff_corr[1]=global.gaincoeff[0].beta2;
	  coeff_corr[2]=global.gaincoeff[0].e_cti;
	  coeff_corr[3]=global.gaincoeff[0].beta1_trap;
	  coeff_corr[4]=global.gaincoeff[0].beta2_trap;
	  coeff_corr[5]=global.gaincoeff[0].e_cti_trap;

	  if(CalculateTempCoeff(temp, 0, coeff))
	    {
	      headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients\n", global.taskname);
	      goto gain_end;
	    }
	  else
	    goto coeff_end;
	}
      else if(( time >= (global.gaincoeff[0].gtime - sens)) && (time <= (global.gaincoeff[global.caldbrows-1].gtime + sens))) 
	{
	  /* Search right row */
	  for (ii=0; time > (global.gaincoeff[ii].gtime + sens) || ii > global.caldbrows; ii++)
	    ;
	  if ((time >=  (global.gaincoeff[ii].gtime - sens)) && (time <=  (global.gaincoeff[ii].gtime + sens)))
	    {

	      coeff_corr[0]=global.gaincoeff[ii].beta1;
	      coeff_corr[1]=global.gaincoeff[ii].beta2;
	      coeff_corr[2]=global.gaincoeff[ii].e_cti;
	      coeff_corr[3]=global.gaincoeff[ii].beta1_trap;
	      coeff_corr[4]=global.gaincoeff[ii].beta2_trap;
	      coeff_corr[5]=global.gaincoeff[ii].e_cti_trap;

	      if(CalculateTempCoeff(temp, ii, coeff))
		{
		  headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients\n", global.taskname);
		  goto gain_end;
		}
	      else
		goto coeff_end;
	      
	    }
	  
	  else 
	    {
	      coeff_corr[0]=global.gaincoeff[ii].beta1;
	      coeff_corr[1]=global.gaincoeff[ii].beta2;
	      coeff_corr[2]=global.gaincoeff[ii].e_cti;
	      coeff_corr[3]=global.gaincoeff[ii].beta1_trap;
	      coeff_corr[4]=global.gaincoeff[ii].beta2_trap;
	      coeff_corr[5]=global.gaincoeff[ii].e_cti_trap;

	      if(CalculateTempCoeff(temp, ii, coeff_1))
		{
		  headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients\n", global.taskname);
		  goto gain_end;
		}
	      
	      if(CalculateTempCoeff(temp, (ii-1), coeff_2))
		{
		  headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients\n", global.taskname);
		  goto gain_end;
		}
	      
	      /* Interpolate between two successive rows */
	      deltat=global.gaincoeff[ii].gtime - global.gaincoeff[ii-1].gtime;
	      frac1= (global.gaincoeff[ii].gtime - time)/deltat;
	      frac2= (time - global.gaincoeff[ii-1].gtime)/deltat;
	      
	      
	      coeff[0]=coeff_2[0] * frac1 + coeff_1[0] * frac2;
	      coeff[1]=coeff_2[1] * frac1 + coeff_1[1] * frac2;
	      coeff[2]=coeff_2[2] * frac1 + coeff_1[2] * frac2;
	      coeff[3]=coeff_2[3] * frac1 + coeff_1[3] * frac2;
	      coeff[4]=coeff_2[4] * frac1 + coeff_1[4] * frac2;
	      coeff[5]=coeff_2[5] * frac1 + coeff_1[5] * frac2;
	      coeff[6]=coeff_2[6] * frac1 + coeff_1[6] * frac2;
	      coeff[7]=coeff_2[7] * frac1 + coeff_1[7] * frac2;
	      coeff[8]=coeff_2[8] * frac1 + coeff_1[8] * frac2;
	      coeff[9]=coeff_2[9] * frac1 + coeff_1[9] * frac2;
	      coeff[10]=coeff_2[10] * frac1 + coeff_1[10] * frac2;
	      coeff[11]=coeff_2[11] * frac1 + coeff_1[11] * frac2;

	      
	      goto coeff_end;	    	    		  
	      
	    }/* else interpolate */
	  
	}
      else
	{

	  coeff_corr[0]=global.gaincoeff[global.caldbrows - 1].beta1;
	  coeff_corr[1]=global.gaincoeff[global.caldbrows - 1].beta2;
	  coeff_corr[2]=global.gaincoeff[global.caldbrows - 1].e_cti;
	  coeff_corr[3]=global.gaincoeff[global.caldbrows - 1].beta1_trap;
	  coeff_corr[4]=global.gaincoeff[global.caldbrows - 1].beta2_trap;
	  coeff_corr[5]=global.gaincoeff[global.caldbrows - 1].e_cti_trap;

	  if(CalculateTempCoeff(temp, (global.caldbrows - 1), coeff))
	    {
	      headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients\n", global.taskname);
	      goto gain_end;
	    }
	  else
	    goto coeff_end;
	}
      
    }
  
 coeff_end:
  headas_chat(CHATTY, " %s: Info: Gain coefficients:\n", global.taskname);
  headas_chat(CHATTY, " %s: Info: GC0=%f GC1=%f GC2=%f GC3=%f GC4=%f GC5=%f\n", global.taskname,coeff[0],coeff[1],coeff[2],
	      coeff[3],coeff[4],coeff[5]);
  headas_chat(CHATTY, " %s: Info: GC0_TRAP=%f GC1_TRAP=%f GC2_TRAP=%f GC3_TRAP=%f GC4_TRAP=%f GC5_TRAP=%f\n", global.taskname,coeff[6],coeff[7],coeff[8],
	      coeff[9],coeff[10],coeff[11]);
  headas_chat(CHATTY, " %s: Info: BETA1=%f BETA2=%f E_CTI=%f\n", global.taskname,coeff_corr[0],coeff_corr[1],coeff_corr[2]);
  headas_chat(CHATTY, " %s: Info: BETA1_TRAP=%f BETA2_TRAP=%f E_CTI_TRAP=%f\n", global.taskname,coeff_corr[3],coeff_corr[4],coeff_corr[5]);
 
  return OK;
  
 gain_end:
  
  return NOT_OK;
  
}/* CalculateGainCoeff */ 

int CalculateTempCoeff(double temp, int rowsnum, double coeff[GAINCOEFF_NUM_NEW])
{
  double deltat, frac1, frac2,sens=0.0000001;
  
  /* TEMP > TEMP(0) */
  if(temp >=  (global.gaincoeff[rowsnum].ccdtemp[0] - sens))
    {
      coeff[0]=global.gaincoeff[rowsnum].gc0[0];
      coeff[1]=global.gaincoeff[rowsnum].gc1[0];
      coeff[2]=global.gaincoeff[rowsnum].gc2[0];
      coeff[3]=global.gaincoeff[rowsnum].gc3[0];
      coeff[4]=global.gaincoeff[rowsnum].gc4[0];
      coeff[5]=global.gaincoeff[rowsnum].gc5[0];
      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[0];
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[0];
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[0];
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[0];
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[0];
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[0];
      goto found_end;
 
    }
  else if(temp <=  (global.gaincoeff[rowsnum].ccdtemp[NUM_GAIN_COLS - 1] + sens))
    {
      coeff[0]=global.gaincoeff[rowsnum].gc0[NUM_GAIN_COLS - 1];
      coeff[1]=global.gaincoeff[rowsnum].gc1[NUM_GAIN_COLS - 1];
      coeff[2]=global.gaincoeff[rowsnum].gc2[NUM_GAIN_COLS - 1];
      coeff[3]=global.gaincoeff[rowsnum].gc3[NUM_GAIN_COLS - 1];
      coeff[4]=global.gaincoeff[rowsnum].gc4[NUM_GAIN_COLS - 1];
      coeff[5]=global.gaincoeff[rowsnum].gc5[NUM_GAIN_COLS - 1];
      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[NUM_GAIN_COLS - 1];
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[NUM_GAIN_COLS - 1];
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[NUM_GAIN_COLS - 1];
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[NUM_GAIN_COLS - 1];
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[NUM_GAIN_COLS - 1];
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[NUM_GAIN_COLS - 1];

      goto found_end;
    }

  else if( (temp >= (global.gaincoeff[rowsnum].ccdtemp[0] - sens)) && (temp <=  (global.gaincoeff[rowsnum].ccdtemp[0] + sens)))
    {
      coeff[0]=global.gaincoeff[rowsnum].gc0[0];
      coeff[1]=global.gaincoeff[rowsnum].gc1[0];
      coeff[2]=global.gaincoeff[rowsnum].gc2[0];
      coeff[3]=global.gaincoeff[rowsnum].gc3[0];
      coeff[4]=global.gaincoeff[rowsnum].gc4[0];
      coeff[5]=global.gaincoeff[rowsnum].gc5[0];
      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[0];
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[0];
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[0];
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[0];
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[0];
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[0];

      goto found_end;
    }

  else if( (temp >= (global.gaincoeff[rowsnum].ccdtemp[1] - sens)) && (temp <=  (global.gaincoeff[rowsnum].ccdtemp[1] + sens)))
    {
      coeff[0]=global.gaincoeff[rowsnum].gc0[1];
      coeff[1]=global.gaincoeff[rowsnum].gc1[1];
      coeff[2]=global.gaincoeff[rowsnum].gc2[1];
      coeff[3]=global.gaincoeff[rowsnum].gc3[1];
      coeff[4]=global.gaincoeff[rowsnum].gc4[1];
      coeff[5]=global.gaincoeff[rowsnum].gc5[1];
      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[1];
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[1];
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[1];
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[1];
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[1];
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[1];
      goto found_end;
    }
  else if( (temp >= (global.gaincoeff[rowsnum].ccdtemp[2] - sens)) && ( temp <=  (global.gaincoeff[rowsnum].ccdtemp[2] + sens)))
    {
      coeff[0]=global.gaincoeff[rowsnum].gc0[2];
      coeff[1]=global.gaincoeff[rowsnum].gc1[2];
      coeff[2]=global.gaincoeff[rowsnum].gc2[2];
      coeff[3]=global.gaincoeff[rowsnum].gc3[2];
      coeff[4]=global.gaincoeff[rowsnum].gc4[2];
      coeff[5]=global.gaincoeff[rowsnum].gc5[2];
      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[2];
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[2];
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[2];
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[2];
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[2];
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[2];
      goto found_end;
    }

  else if((temp <= (global.gaincoeff[rowsnum].ccdtemp[0] + sens )) && (temp >= (global.gaincoeff[rowsnum].ccdtemp[1] - sens )))
    {
      

      deltat=global.gaincoeff[rowsnum].ccdtemp[0] - global.gaincoeff[rowsnum].ccdtemp[1];
      frac1= (global.gaincoeff[rowsnum].ccdtemp[0] - temp)/deltat;
      frac2= (temp - global.gaincoeff[rowsnum].ccdtemp[1])/deltat;
	    
      
      
      coeff[0]=global.gaincoeff[rowsnum].gc0[1]*frac1+global.gaincoeff[rowsnum].gc0[0]*frac2;
      coeff[1]=global.gaincoeff[rowsnum].gc1[1]*frac1+global.gaincoeff[rowsnum].gc1[0]*frac2;
      coeff[2]=global.gaincoeff[rowsnum].gc2[1]*frac1+global.gaincoeff[rowsnum].gc2[0]*frac2;
      coeff[3]=global.gaincoeff[rowsnum].gc3[1]*frac1+global.gaincoeff[rowsnum].gc3[0]*frac2;
      coeff[4]=global.gaincoeff[rowsnum].gc4[1]*frac1+global.gaincoeff[rowsnum].gc4[0]*frac2;
      coeff[5]=global.gaincoeff[rowsnum].gc5[1]*frac1+global.gaincoeff[rowsnum].gc5[0]*frac2;

      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[1]*frac1+global.gaincoeff[rowsnum].gc0_trap[0]*frac2;
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[1]*frac1+global.gaincoeff[rowsnum].gc1_trap[0]*frac2;
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[1]*frac1+global.gaincoeff[rowsnum].gc2_trap[0]*frac2;
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[1]*frac1+global.gaincoeff[rowsnum].gc3_trap[0]*frac2;
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[1]*frac1+global.gaincoeff[rowsnum].gc4_trap[0]*frac2;
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[1]*frac1+global.gaincoeff[rowsnum].gc5_trap[0]*frac2;

      goto found_end;

    }
  else if((temp <= (global.gaincoeff[rowsnum].ccdtemp[1] + sens )) && (temp >= (global.gaincoeff[rowsnum].ccdtemp[2] - sens )))
    {


      deltat=global.gaincoeff[rowsnum].ccdtemp[1] - global.gaincoeff[rowsnum].ccdtemp[2];
      frac1= (global.gaincoeff[rowsnum].ccdtemp[1] - temp)/deltat;
      frac2= (temp - global.gaincoeff[rowsnum].ccdtemp[2])/deltat;
	    
      
      
      coeff[0]=global.gaincoeff[rowsnum].gc0[2]*frac1+global.gaincoeff[rowsnum].gc0[1]*frac2;
      coeff[1]=global.gaincoeff[rowsnum].gc1[2]*frac1+global.gaincoeff[rowsnum].gc1[1]*frac2;
      coeff[2]=global.gaincoeff[rowsnum].gc2[2]*frac1+global.gaincoeff[rowsnum].gc2[1]*frac2;
      coeff[3]=global.gaincoeff[rowsnum].gc3[2]*frac1+global.gaincoeff[rowsnum].gc3[1]*frac2;
      coeff[4]=global.gaincoeff[rowsnum].gc4[2]*frac1+global.gaincoeff[rowsnum].gc4[1]*frac2;
      coeff[5]=global.gaincoeff[rowsnum].gc5[2]*frac1+global.gaincoeff[rowsnum].gc5[1]*frac2;

      coeff[6]=global.gaincoeff[rowsnum].gc0_trap[2]*frac1+global.gaincoeff[rowsnum].gc0_trap[1]*frac2;
      coeff[7]=global.gaincoeff[rowsnum].gc1_trap[2]*frac1+global.gaincoeff[rowsnum].gc1_trap[1]*frac2;
      coeff[8]=global.gaincoeff[rowsnum].gc2_trap[2]*frac1+global.gaincoeff[rowsnum].gc2_trap[1]*frac2;
      coeff[9]=global.gaincoeff[rowsnum].gc3_trap[2]*frac1+global.gaincoeff[rowsnum].gc3_trap[1]*frac2;
      coeff[10]=global.gaincoeff[rowsnum].gc4_trap[2]*frac1+global.gaincoeff[rowsnum].gc4_trap[1]*frac2;
      coeff[11]=global.gaincoeff[rowsnum].gc5_trap[2]*frac1+global.gaincoeff[rowsnum].gc5_trap[1]*frac2;

      /*INTERPOLA TRA 1-2*/

      goto found_end;

    }
    
  else
    {
      headas_chat(NORMAL, " %s: Error: Unable to calculate gain coefficients.\n", global.taskname);
      return NOT_OK;
    }

 found_end:
  headas_chat(CHATTY, " %s: Info: Gain coefficients:\n", global.taskname);
  headas_chat(CHATTY, " %s: Info: GC0=%f GC1=%f GC2=%f GC3=%f GC4=%f GC5=%f\n", global.taskname,coeff[0],coeff[1],coeff[2],
	      coeff[3],coeff[4],coeff[5]);
  headas_chat(CHATTY, " %s: Info: GC0_TRAP=%f GC1_TRAP=%f GC2_TRAP=%f GC3_TRAP=%f GC4_TRAP=%f GC5_TRAP=%f\n", global.taskname,coeff[6],coeff[7],coeff[8],
	      coeff[9],coeff[10],coeff[11]);
  return OK;


}/* CalculateTempCoeff */


/*
 *
 * CalculateGainOffset2
 *
 */
int CalculateGainOffset2(double time, double temp, double en, int rawx, int rawy, double *offset2)
{
  int    ii=0, jj=0, kk=0;
  double tmp_offset=0.0, sens=0.0000001, en2;

  if(global.oldgainformat)
    {
      /* gain file in old format */
      *offset2=0;
      return OK;
    }
  else /* CALDB GAIN FILE NEW FORMAT */
    {
      if (time >= (global.gaincoeff[0].gtime - sens))
	{
	  /* Find row in gain file */
	  for(ii=0; time > (global.gaincoeff[ii].gtime - sens) && ii < global.caldbrows; ii++);


	  /* Check if anomalouse column */
	  for(jj=0; jj<global.caldbdim; jj++)
	    {
	      if( (global.gaincoeff[ii-1].rawx[jj]==rawx) &&
		  (rawy>=global.gaincoeff[ii-1].rawy[jj]) &&
		  (rawy<=(global.gaincoeff[ii-1].rawy[jj]+global.gaincoeff[ii-1].yextent[jj])) )
		{  
		  en2 = en;

		  for(kk=0; kk<=global.par.offsetniter; kk++){
		    CalculateTempOffset2(temp, en2, ii-1, jj, &tmp_offset);
		    en2 = en + tmp_offset;
		  }
		  
		  *offset2=tmp_offset;
		  return OK;
		}
	    }
	  
	  /* Not anomalouse column */
	  *offset2=0;
	  return OK;   
	}
      else
	{
	  /* Time before times in gain file */
	  *offset2=0;
	  return OK;
	}
    }

  *offset2=0;
  return OK;

} /* CalculateGainOffset2 */

/*
 *
 *  CalculateTempOffset2
 *
 */
int CalculateTempOffset2(double temp, double en, int rowsnum, int colsnum, double *tempoffset2)
{
  double deltat, frac1, frac2, sens=0.0000001;
  double offset2_t0=0.0, offset2_t1=0.0, offset2_t2=0.0;
  double temp0=0.0, temp1=0.0, temp2=0.0;
  double tmp_offset=0.0;
  float alpha1, alpha2, ebreak;


  if(global.par.userctcorrpar){
    alpha1 = global.par.alpha1;
    alpha2 = global.par.alpha2;
    ebreak = global.par.ebreak;
  }
  else{
    alpha1 = global.gaincoeff[rowsnum].alpha1;
    alpha2 = global.gaincoeff[rowsnum].alpha2;
    ebreak = global.gaincoeff[rowsnum].ebreak;
  }
  
  if( en <= ebreak ){
    offset2_t0 = global.gaincoeff[rowsnum].offset2[colsnum*3] * pow(en/ebreak ,alpha1);
    offset2_t1 = global.gaincoeff[rowsnum].offset2[colsnum*3 +1] * pow(en/ebreak ,alpha1);
    offset2_t2 = global.gaincoeff[rowsnum].offset2[colsnum*3 +2] * pow(en/ebreak ,alpha1);
  }
  else{
    offset2_t0 = global.gaincoeff[rowsnum].offset2[colsnum*3] * pow(en/ebreak ,alpha2);
    offset2_t1 = global.gaincoeff[rowsnum].offset2[colsnum*3 +1] * pow(en/ebreak ,alpha2);
    offset2_t2 = global.gaincoeff[rowsnum].offset2[colsnum*3 +2] * pow(en/ebreak ,alpha2);
  }
  
  temp0 = global.gaincoeff[rowsnum].ccdtemp[0];
  temp1 = global.gaincoeff[rowsnum].ccdtemp[1];
  temp2 = global.gaincoeff[rowsnum].ccdtemp[2];
  
  
  if(temp > temp0-sens)
    tmp_offset=offset2_t0;
  
  else if(temp <= (temp2 + sens))
    tmp_offset=offset2_t2;
  
  else if(temp>=(temp0-sens) && temp<=(temp0+sens))
    tmp_offset=offset2_t0;
  
  else if(temp>=(temp1-sens) && temp<=(temp1+sens))
    tmp_offset=offset2_t1;
  
  else if(temp>=(temp2-sens) && temp<=(temp2+sens))
    tmp_offset=offset2_t2;
  
  else if(temp<(temp0+sens) && temp>(temp1-sens))
    {
      deltat = temp0-temp1;
      frac1 = (temp0-temp)/deltat;
      frac2 = (temp-temp1)/deltat;
      
      tmp_offset = offset2_t0*frac2 + offset2_t1*frac1;
    }
  
  else if(temp<(temp1+sens) && temp>(temp2-sens))
    {
      deltat = temp1-temp2;
      frac1 = (temp1-temp)/deltat;
      frac2 = (temp-temp2)/deltat;
      
      tmp_offset = offset2_t1*frac2 + offset2_t2*frac1;
    }
  
   headas_chat(CHATTY," %s: Info: OFFSET2 %f\n",global.taskname,tmp_offset);

  *tempoffset2 = tmp_offset;

  return OK;

} /* CalculateTempOffset2 */
