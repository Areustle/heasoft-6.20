/*
 * 
 *	xrtpcgrade.c:
 *
 *	INVOCATION:
 *
 *		xrtpcgrade [parameter=value ...]
 *
 *	DESCRIPTION:
 *                Routine to assign grades to XRT Photon counting
 *                mode data. Based on ASCA 'faint'
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 01/03/02 - First version
 *        0.1.1 -    02/07/02 - Added GetSWXRTDASVersion call
 *                            - Changed strcmp with strcasecmp to ignore
 *                              case of characters when needed
 *                            - Modified return values when output files exist
 *                              and 'clobber' = no
 *                            - Added 'DeriveFileName' to handle compressed
 *                              file names
 *                            - Added management return values from function
 *                              'remove'.
 *                            - Changed PHA column datatype from "1I" to "1J"
 *        0.1.2 -    12/09/02 - Bug fixed for Alpha platform
 *        0.1.3 - FT 16/10/02 - Added 'B' handle into 'AddEventBintable'
 *        0.1.4 - BS 31/10/02 - Minor changes
 *                            - Modified to read input files name 
 *                              from ASCII file
 *                            - Deleted "aboveflag" and "maxgrade"
 *                              parameters
 *                            - Deleted screening for grade
 *                            - Optimized "AddEventBintable" routine
 *                            - Modified to overwrite input file
 *       0.1.5 - BS 25/02/03  - Added chosen to use user or on-board split threshold
 *                            - Added checks on XRTPHA keyword
 *       0.1.6 -    07/07/03  - Bug fixed on 'xrtpcgrade_getpar' failure           
 *       0.1.7 -    10/07/03  - Added routine to compute grade using CALDB grades file
 *                              with XMM patterns definition
 *       0.1.8 -    15/07/03  - Modified to use new prototype of 'CalGetFileName'
 *       0.1.9 -    04/08/03  - Bug fixed
 *       
 *      0.1.10 -    10/09/03  - Added compute events percentage
 *      0.1.11 -    23/10/03  - Update TLMIN and TLMAX for GRADE column
 *                            - Added DS keyword
 *      0.1.12 -    03/11/03  - Replaced call to 'headas_parstamp()' with 'HDpar_stamp()'
 *      0.1.13 -    19/11/03  - Used default value for GRADE TLMIN/TLMAX when needed 
 *      0.1.14 -    11/12/03  - Added handling of amplifiers 
 *      0.2.0  -    22/03/04  - Changed taskname from 'xrtph2br' to 'xrtpcgrade'
 *                            - Added routine 'xrtpcgrade_checkinput'
 *      0.2.1  -              - Bug fixed
 *      0.2.2  -    26/03/04  - Deleted input files list handling
 *      0.2.3  -    14/05/04  - Messages displayed revision
 *      0.2.4  -    24/06/04  - Bug fixed on 'fits_update_key' for LOGICAL
 *                              keywords
 *      0.2.5  -    16/03/05  - Modified to read 'Splitthr' column instead of
 *                              SPLITTHR' keyword if 'split' input parameter is
 *                              negative
 *             -              - Added input parameter 'hdfile' 
 *      0.2.6  -    13/07/05  - Replaced ReadBintable with ReadBintableWithNULL
 *                              to perform check on undefined value reading hdfile
 *      0.2.7  -    14/07/05  - Exit with warning if infile is empty
 *      0.2.8  -    09/08/05  - Exit with error ig gradefile or hdfile are empty
 *                            - Create output file when exits with warning
 *      0.2.9  - NS 12/03/07  - Changed ReadSplitValues function to accept Settled=1 In10Arcm=0
 *                              InSafeM=1 for events with obsmode=SLEW
 *      0.3.0 -    10/07/2007 - New input parameter 'thrfile'
 *      0.3.1 -    29/09/2008 - Changed ReadSplitValues function to evaluate obsmode=SLEW when InSafeM=1
 *
 *      NOTE:
 *        Based on the CLASSIFY routine
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtpcgrade  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtpcgrade.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTPCGRADE_C
#define XRTPCGRADE_VERSION       "0.3.1"
#define PRG_NAME               "xrtpcgrade"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtpcgrade_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtpcgrade.par    
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
 *           void xrtpcgrade_info(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 08/02/2002 - First version
 *        0.1.2: - BS 09/07/2003 - Added 'gradefile' and 'ascagrade' 
 *                                 parameter
 *                                 
 *
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpcgrade_getpar()
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

  /* Compute ASCA grades ? */
  if(PILGetBool(PAR_ASCAGRADE, &global.par.ascagrade))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ASCAGRADE);
      goto Error;
    }
 
  /* Grade File Name */
  if(PILGetFname(PAR_GRADEFILE, global.par.gradefile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRADEFILE);
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

      /* Split Threshold Level */
      if(PILGetInt(PAR_SPLIT, &global.par.split)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SPLIT);
	  goto Error;	
	}
    }
  
  if(!global.usethrfile && global.par.split < 0)
    {
      /* Input header packet Files Name */
      if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	  goto Error;
	}
    }

  get_history(&global.hist);
  xrtpcgrade_info();

  return OK;

 Error:
  return NOT_OK;
  
} /* xrtpcgrade_getpar */

/*
 *	xrtpcgrade_work
 *
 *
 *	DESCRIPTION:
 *            This routine reads in the input FITS file and passes each record
 *            to CLASSIFY, the routine which does the actual grade assignments
 *            to the photon counting mode data.
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
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtpcgrade_work()
{
  int            status = OK, inExt, outExt, evExt;
  int            logical;
  long           extno;
  char           obsmode[20];
  ThresholdInfo_t thrvalues;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL;              /* Input and Output fits file pointer */ 


  global.warning=0;  
  extno=-1;
  if(xrtpcgrade_checkinput())
    goto Error;
    
  global.scount=0;
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

  /* Check readout mode of input events file */
  if(!(KeyWordMatch(inunit, KWNM_DATAMODE, S, KWVL_DATAMODE_PH, NULL)))
    { 
      headas_chat(NORMAL,"%s: Error: This task does not process the readout mode\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: Valid readout mode is: %s\n", global.taskname, KWVL_DATAMODE_PH);

      if( CloseFitsFile(inunit))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto Error;
    }

  /* Check readout mode of input events file */
  head=RetrieveFitsHeader(inunit);    
  
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

  /* Retrieve XRTVSUB of input event file */
  if((ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.evt.xrtvsub=card->u.JVal;
    }
  else
    {
      global.evt.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.infile);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }


  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);
      goto Error;
    } 

  /********************************
   *     Get CALDB grades file    *
   ********************************/

  extno=-1;

  if ( !strcasecmp(global.par.gradefile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_GRADE_DSET, global.par.gradefile, "datamode.eq.photon", &extno ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto Error;
	}
      extno++;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.gradefile);
  if (GetGrades(extno)) 
    {
      headas_chat(CHATTY, "%s: Error: Unable to process\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: %s file.\n", global.taskname, global.par.gradefile);
      goto Error;
    }

  /********************************
   *     Get CALDB thresholds file    *
   ********************************/

  if(global.usethrfile)
    {
      extno=-1;
      
      if (!(strcasecmp (global.par.thrfile, DF_CALDB)))
	{
	  if (CalGetFileName(HD_MAXRET, global.evt.dateobs, global.evt.timeobs, DF_NOW, DF_NOW, KWVL_EXTNAME_XRTVSUB, global.par.thrfile,HD_EXPR, &extno))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to query CALDB.\n", global.taskname);
	      goto Error;
	    }
	  else
	    headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.thrfile);
	  extno++;
	}
      else
	headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.thrfile);

      if(ReadThrFile(global.par.thrfile, extno, global.evt.xrtvsub, &thrvalues))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read split threshold level from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the input thrfile.\n", global.taskname);
	  goto Error;
	}

      global.par.split=thrvalues.pcsplit;
      
      headas_chat(NORMAL, "%s: Info: Split Threshold Level from input thresholds file = %d\n", global.taskname, global.par.split);
    }
  
    
  /* Create output file */
  if ((outunit = OpenWriteFitsFile(global.tmpfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' temporary file. \n", global.taskname, global.tmpfile);
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
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname, outExt);
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

  if( !global.usethrfile && global.par.split < 0)
    {
      headas_chat(NORMAL, "%s: Info: '%s' input parameter is: %d\n", global.taskname, PAR_SPLIT, global.par.split);
      headas_chat(NORMAL, "%s: Info: using %s file\n", global.taskname,global.par.hdfile);
      headas_chat(NORMAL, "%s: Info: to read split threshold values.\n",global.taskname); 
      

      if((!ExistsKeyWord(&head, KWNM_OBS_MODE  , &card)))
	{ 
	  headas_chat(NORMAL,"%s: Error: %s keyword not found\n", global.taskname,KWNM_OBS_MODE );
	  headas_chat(NORMAL,"%s: Error: in the %s file.\n", global.taskname, global.par.infile);
	  goto Error;
	}
      else
	strcpy(obsmode,card->u.SVal);

      if(ReadSplitValues(obsmode))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read split threshold values from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the %s file.\n", global.taskname, global.par.hdfile);
	  goto Error;
	}

    }

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);

  /* Add event extension */
  if ((AddEventBintable(inunit, outunit)))
    {
      
      headas_chat(NORMAL, "%s: Error: Unable to assign grades to the events.\n", global.taskname);
      goto Error;
    }
  if(global.warning)
    goto ok_end;
    
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPHA, &logical, CARD_COMM_XRTPHA, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTPHA);
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
  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }
    
  /* Add checksum and datasum in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
      goto Error;
    }
  /* close files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
      goto Error;
    }  
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n ", global.taskname, global.tmpfile);
      goto Error;
    }   
  
  /* rename temporary file into output event file */
  if (rename (global.tmpfile,global.par.outfile) == -1)
    {
      headas_chat(CHATTY, "%s: Error: Unable to rename temporary file. \n", global.taskname);
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
  
  if(global.maxrows)
    free(global.gradevalues);
  if(global.scount)
    free(global.split);
  return OK;

 Error:
  return NOT_OK;
} /* xrtpcgrade_work */
/*
 *	AddEventBintable
 *
 *
 *	DESCRIPTION:
 *           Routine to compute GRADE, PHA, PixsAbove values and put them
 *           into EVENTS bintable. 
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
 *             void DeleteCard(FitsHeader_t *header, const char *KeyWord);
 *             void  GetGMTDateTime(char *);
 *             int sprintf(char *str, const char *format, ...);
 *
 *             void AddHistory(FitsHeader_t *header, const char *Comment);
 *             void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *             void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *             int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				 const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *             external faint_v50_( int *cf_handle, int cf_split, int *cf_sumph,
 *                                int  *cf_type, int *cf_above );
 *             int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *             unsigned	ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int AddEventBintable(FitsFileUnit_t evunit,FitsFileUnit_t ounit)
{
  int                jj, ii, qfancy = 0, pha, grade, above, handle[PHAS_MOL], totrows, amp=0, tmp=0;
  int                iii=0, split_used, ccdframe, ccdframe_old;
  char		     KeyWord[FLEN_KEYWORD];
  long               asca_split;
  float              echo= 0;
  double             per_grade[33], per_grade_asca[8], per_sat_event, per_sat_asca, evtime,sens=0.0000001;
  BOOL               found=0;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  Ev2Col_t           indxcol;
  FitsCard_t         *card;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  /* Check if PHA is already computed */
  if(ExistsKeyWord(&head, KWNM_XRTPHA, &card))
    if(card->u.LVal)
      {
	headas_chat(NORMAL, "%s: Warning: %s is already calculated,\n", global.taskname, CLNM_PHA);
	headas_chat(NORMAL, "%s: Warning: the %s column will be overwritten.\n", global.taskname, CLNM_PHA);
      }
      
 
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  totrows=outtable.MaxRows;  
  /* Get cols index from name */
  if ((indxcol.Amp=ColNameMatch(CLNM_Amp, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  if ((indxcol.PHAS=ColNameMatch(CLNM_PHAS, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }
  
  /* Add new columns if needed */
  if((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PHA);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable, CLNM_PHA, CARD_COMM_PHA, "1J", TUNIT, UNIT_CHANN, CARD_COMM_PHYSUNIT);
      indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable);
    }

  if((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_GRADE);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable,CLNM_GRADE, CARD_COMM_GRADE, "1I",TNONE);    
      indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable);    
    }

  if((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }


  if(global.grademin!=-1)
    global.grademin=PHO_GRADE_MIN;
  sprintf(KeyWord, "TLMIN%d", (indxcol.GRADE+1)); 

  
  AddCard(&head, KeyWord, J, &global.grademin, CARD_COMM_TLMIN);
  
  if(global.grademax!=-1)
    global.grademax=PHO_GRADE_MAX;
  sprintf(KeyWord, "TLMAX%d", (indxcol.GRADE+1));
 
  AddCard(&head, KeyWord, J, &global.grademax, CARD_COMM_TLMAX);
   
  
  if((indxcol.ABOVE=ColNameMatch(CLNM_ABOVE, &outtable)) == -1)
    {
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_ABOVE);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
      AddColumn(&head, &outtable,CLNM_ABOVE,CARD_COMM_ABOVE , "1I",TNONE);    
      indxcol.ABOVE=ColNameMatch(CLNM_ABOVE, &outtable);
    }

  if(global.par.ascagrade)
    {
      if((indxcol.ASCAGRADE=ColNameMatch("ASCAGRADE", &outtable)) == -1)
	{
	  headas_chat(CHATTY, "%s: Info: ASCAGRADE column will be added and filled\n",  global.taskname);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	  AddColumn(&head, &outtable,"ASCAGRADE", CARD_COMM_GRADE, "1I",TNONE);    
	  indxcol.ASCAGRADE=ColNameMatch("ASCAGRADE", &outtable);    
	}
      if((indxcol.ASCAPHA=ColNameMatch("ASCAPHA", &outtable)) == -1)
	{
	  headas_chat(CHATTY, "%s: Info: ASCAPHA column will be added and filled\n",  global.taskname);
	  headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);   
	  AddColumn(&head, &outtable, "ASCAPHA", CARD_COMM_PHA, "1J", TUNIT, UNIT_CHANN, CARD_COMM_PHYSUNIT);
	  indxcol.ASCAPHA=ColNameMatch("ASCAPHA", &outtable);
	}
    }
  
  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: calcolated PHA from PHAS value. ", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);
    }
  
  /* Add or update DSVAL1 keywords */
  sprintf(KeyWord, "%d:%d",(int) global.grademin,(int) global.grademax); 
 
  ManageDSKeywords(&head, "GRADE", "I", KeyWord); 

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
      goto event_end;
    }

  for(jj=0; jj<33;jj++)
    global.grade_tot[jj]=0;

  for(jj=0; jj<8;jj++)
    global.grade_asca[jj]=0;

  global.sat_event=0;
  global.sat_asca=0;
  ccdframe=0;
  ccdframe_old=-1;
  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	  amp=BVEC(outtable, n, indxcol.Amp);

	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);

	  if(ccdframe != ccdframe_old)
	    {
	      if(!global.usethrfile && global.par.split < 0 )
		{
		  evtime=DVEC(outtable, n, indxcol.TIME);

		  for(;iii < global.scount; iii++)
		    {
		      split_used=-1;
		      if(ccdframe==global.split[iii].ccdframe && 
			 (global.split[iii].hktime >= evtime-sens && global.split[iii].hktime <= evtime+sens))
			{
			  split_used=global.split[iii].split;
			  headas_chat(CHATTY, "%s: Info: Used %d split threshold value for %d ccdframe.\n",global.taskname, split_used, ccdframe);
			  goto split_found;
			}
		    }
		}
	      else
		split_used=global.par.split;
	    }

	  if(split_used < 0)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get split threshold value.\n", global.taskname);
	      goto event_end;
	    }

	split_found:
	  ccdframe_old=ccdframe;
	  if(global.par.ascagrade)
	    {/* Cast for faintv50 */
	      asca_split=(long)split_used;
	      /* Compute PHA and GRADES with ASCA routine */
	      /* Get phas values */
	      for (jj=0; jj< PHAS_MOL; jj++)
		handle[jj] = IVECVEC(outtable,n,indxcol.PHAS,jj);
	      
	      if(amp == 2)
		{
		  tmp=handle[1];
		  handle[1]=handle[3];
		  handle[3]=tmp;

		  tmp=handle[4];
		  handle[4]=handle[5];
		  handle[5]=tmp;
		  
		  tmp=handle[6];
		  handle[6]=handle[8];
		  handle[8]=tmp;
		}
	      /* Calculate grade and pha */
	      faint_v50_(handle, &asca_split, &echo, &qfancy, &pha, &grade,&above);
	      /* Put values in table */
	      
	      IVEC(outtable, n, indxcol.ASCAGRADE) = grade;
	      JVEC(outtable, n, indxcol.ASCAPHA) = pha;
	      if(pha >= PHA_MAX)
		global.sat_asca++;
	      global.grade_asca[grade]++;
	    }
	  
	  pha=0;
	  above=0;
	  /* Assign XMM patterns */
	  for (jj=0; jj< PHAS_MOL; jj++)
	    {
	      if(IVECVEC(outtable,n,indxcol.PHAS,jj) >= split_used)
		{
		  above++;
		  pha+=IVECVEC(outtable,n,indxcol.PHAS,jj);
		  handle[jj]=1;
		}
	      else
		handle[jj]=2;
	    }
	  
	  found=0;
	  grade=KWVL_PCGRADENULL;
	  for(ii=0; ii<global.maxrows && grade==KWVL_PCGRADENULL; ii++)
	    {
	      if(amp == global.gradevalues[ii].amp)
		{
		  for(jj=0; jj<PHAS_MOL && found==0; jj++) 
		    if((handle[jj]!=global.gradevalues[ii].grade[jj]) && (global.gradevalues[ii].grade[jj]))
		      found=1;
		  
		  if(!found)
		    grade=global.gradevalues[ii].gradeid;
		  else
		    found=0;
		}
	    }
	  
	  IVEC(outtable, n, indxcol.GRADE) = grade;
	  JVEC(outtable, n, indxcol.PHA) = pha;
	  if(pha>=PHA_MAX)
	    global.sat_event++;
	  IVEC(outtable, n, indxcol.ABOVE) = above;
	  
	  if(grade == KWVL_PCGRADENULL)
	    global.grade_tot[32]++;
	  else
	    global.grade_tot[grade]++;
	  
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
  
  /* Write grade events statistics */

  headas_chat(NORMAL, "---------------------------------------------------\n");
  headas_chat(NORMAL, "%s: Info: \t\t Swift XRT GRADES\n", global.taskname);
  headas_chat(NORMAL, "%s: Info: Total pixels                   %15d\n", global.taskname, totrows);

  per_sat_event = 100.*(double)global.sat_event/(double)(totrows);
  headas_chat(NORMAL, "%s: Info: Total saturated events         %15d%10.4f%%\n", global.taskname, global.sat_event, per_sat_event);
  
  for(jj=0; jj<33;jj++)
    {
     per_grade[jj] = 100.*(double)global.grade_tot[jj]/(double)(totrows);
    }


  for(jj=0; jj<33;jj++)
    {
      headas_chat(NORMAL, "%s: Info: Total events with grade %2d     %15d%10.4f%%\n", global.taskname, jj, global.grade_tot[jj], per_grade[jj]);
    }

  
  if(global.par.ascagrade)
    {
     
      
      for(jj=0; jj<8;jj++)
	{
	  per_grade_asca[jj] = 100.*(double)global.grade_asca[jj]/(double)(totrows);
	}
      headas_chat(NORMAL, "\n%s: Info: \t\t ASCA GRADES\n", global.taskname);


      headas_chat(NORMAL, "%s: Info: Total pixels                   %15d\n",global.taskname, totrows);
      
      per_sat_asca = 100.*(double)global.sat_asca/(double)(totrows);
      headas_chat(NORMAL, "%s: Info: Total saturated events         %15d%10.4f%%\n", global.taskname, global.sat_asca, per_sat_asca);
      
      for(jj=0; jj<8;jj++)
	{
	  headas_chat(NORMAL, "%s: Info: Total events with grade %2d     %15d%10.4f%%\n", global.taskname, jj, global.grade_asca[jj],per_grade_asca[jj]);
	}
    }

  return OK;
  
 event_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* AddEventBintable */

/*
 *	xrtpcgrade
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
 *             void xrtpcgrade_getpar(void);
 *             void xrtpcgrade_info(void); 
 * 	       void xrtpcgrade_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 08/02/2002 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpcgrade()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTPCGRADE_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);
  
  /* Get parameter values */ 
  if ( xrtpcgrade_getpar() == OK ) {
    
    if ( xrtpcgrade_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto pcgrade_end;
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
    goto pcgrade_end;
  
  return OK;
  
 pcgrade_end:
  if (FileExists(global.tmpfile))
    remove (global.tmpfile);
  return NOT_OK;


} /* xrtpcgrade */
/*
 *	xrtpcgrade_info: 
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
void xrtpcgrade_info ()
{
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE," \t\tRunning '%s'\n",global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file             :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file            :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input GRADES file            :'%s'\n",global.par.gradefile);
  headas_chat(NORMAL,"Name of the input thresholds file        :'%s'\n",global.par.thrfile);
  if(!global.usethrfile)
    {
      headas_chat(NORMAL,"Split Threshold Level                    : %d\n",global.par.split);
      if(global.par.split < 0)
	headas_chat(NORMAL,"Name of the input HK file                :'%s'\n",global.par.hdfile);
    }
  if (global.par.ascagrade)
    headas_chat(CHATTY,"Calculate grades using ASCA patterns?    : yes\n");
  else
    headas_chat(CHATTY,"Calculate grades using ASCA patterns?    : no\n");  

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                   : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                   : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtpcgrade_info */



/*
 *      GetGrades
 *
 */
int GetGrades(long extno)
{
  int            n, status=OK, jj;
  char		 KeyWord[FLEN_KEYWORD];
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  GradeCol_t     gradecol; 
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */
  FitsCard_t     *card;
  FitsFileUnit_t gradeunit=NULL;        /* Bias file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  global.grademin=-1;
  global.grademax=-1;

  /* Open read only bias file */
  if ((gradeunit=OpenReadFitsFile(global.par.gradefile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname,global.par.gradefile);
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, gradeunit, global.par.gradefile, global.taskname);
  
  if(extno != -1)
    {
      /* move to GRADES extension */
      if (fits_movabs_hdu(gradeunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_PCGRADES);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gradefile); 
	  goto get_end;
	} 
    }
  else
    {
      if (fits_movnam_hdu(gradeunit,ANY_HDU,KWVL_EXTNAME_PCGRADES,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_PCGRADES);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.gradefile); 
	  goto get_end;
	} 
    }
  
  head = RetrieveFitsHeader(gradeunit);
  
  /* Read grade bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  /* Get columns index from name */
  if ((gradecol.Amp = GetColNameIndx(&table, CLNM_Amp)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Amp);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }

  if ((gradecol.GRADEID = GetColNameIndx(&table, CLNM_GRADEID)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  sprintf(KeyWord, "TLMIN%d", (gradecol.GRADEID+1));
  if (ExistsKeyWord(&head, KeyWord, &card))
    global.grademin=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Warning: TLMIN keywords for %s column not found\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.gradefile);
    }

  sprintf(KeyWord, "TLMAX%d", (gradecol.GRADEID+1));
  if (ExistsKeyWord(&head, KeyWord, &card))
    global.grademax=card->u.JVal;
  else
    {
      headas_chat(NORMAL, "%s: Warning: TLMAX keywords for %s column not found\n", global.taskname, CLNM_GRADEID);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, global.par.gradefile);
    }

  if ((gradecol.GRADE = GetColNameIndx(&table, CLNM_GRADE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  global.maxrows=table.MaxRows;
  /* Allocate memory to storage all coefficients */
  global.gradevalues=(GradeRow_t *)calloc(table.MaxRows, sizeof(GradeRow_t));
  
  /* Read blocks of bintable rows from input bias file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  while (ReadBintable(gradeunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	  global.gradevalues[n].amp=BVEC(table,n,gradecol.Amp);
	  global.gradevalues[n].gradeid=IVEC(table,n,gradecol.GRADEID);
	   /* Get grades */
	  for (jj=0; jj< PHAS_MOL; jj++)
	    global.gradevalues[n].grade[jj] = IVECVEC(table,n,gradecol.GRADE,jj);
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close bias file */
  if (CloseFitsFile(gradeunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.gradefile);
      goto get_end;
    }
  
  return OK;
  
 get_end:
  
  return NOT_OK;
  
}/* GetGrades */

int xrtpcgrade_checkinput(void)
{
  int            overwrite=0;
  char           ext[MAXEXT_LEN], stem[10];

  pid_t          tmp;
 
  /* Check if outfile == NONE */    
  if (!(strcasecmp (global.par.outfile, DF_NONE)))
    {
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
	  headas_chat(NORMAL, "%s: Info: '%s' file exists.\n", global.taskname, global.par.outfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: Cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: Parameter 'clobber' set,\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
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
	headas_chat(NORMAL, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpfile);
	goto check_end;
      }
  
  return OK;
 
 check_end:
  return NOT_OK;
}
/* 
ReadSplitValues
*/

int ReadSplitValues(char *obsmode)
{

  int                status=OK, hkmode=0, good_row=1;
  unsigned short     hkin10Arcm=0, hksettled=0, hkInSafeM=0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[7];
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

  global.scount=0;

  
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, global.par.hdfile);
  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
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
      goto Readsplitvalues_end;
    }
  
  hkhead=RetrieveFitsHeader(hkunit);
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }
  /* Get needed columns number from name */
  
  /*  CCDFrame */

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  /* Frame Start Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  if ((hkcol.SplitThr=ColNameMatch(CLNM_SPLITTHR, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SPLITTHR);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  if ((hkcol.Settled=ColNameMatch(CLNM_Settled, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Settled);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  if ((hkcol.In10Arcm=ColNameMatch(CLNM_In10Arcm, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_In10Arcm);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  if ((hkcol.InSafeM=ColNameMatch(CLNM_InSafeM, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_InSafeM);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto Readsplitvalues_end;
    }

  EndBintableHeader(&hkhead, &hktable);
 
  FromRow = 1;
  ReadRows=hktable.nBlockRows;

  /* Read only the following five columns */
  nCols=7; 
  ActCols[0]=hkcol.CCDFrame;
  ActCols[1]=hkcol.TIME;
  ActCols[2]=hkcol.SplitThr;
  ActCols[3]=hkcol.XRTMode;
  ActCols[4]=hkcol.Settled;
  ActCols[5]=hkcol.In10Arcm;
  ActCols[6]=hkcol.InSafeM;
  
  while(ReadBintableWithNULL(hkunit, &hktable, nCols, ActCols,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  good_row=1;

	  hkInSafeM=XVEC1BYTE(hktable, n, hkcol.InSafeM);  
	  hksettled=XVEC1BYTE(hktable, n, hkcol.Settled);
	  hkin10Arcm=XVEC1BYTE(hktable, n, hkcol.In10Arcm);
	  
	  if(!strcmp(obsmode, KWVL_OBS_MODE_S)) /*SLEW*/
	    {
	      if( (hkInSafeM==128) || (hksettled==0 && hkin10Arcm==0) ) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_ST)) /*SETTLING*/
	    {
	      if(hksettled==0 && hkin10Arcm==128) {good_row=1;} else {good_row=0;}
	    }
	  else if(!strcmp(obsmode, KWVL_OBS_MODE_PG)) /*POINTING*/
	    {
	      if( (hksettled==128 && hkin10Arcm==128) || (hksettled==128 && hkin10Arcm==0 && hkInSafeM==0) ) {good_row=1;} else {good_row=0;}
	    }


	  hkmode=BVEC(hktable, n, hkcol.XRTMode);
	  
	  /*printf("%u -- %u -- %d\n",hksettled,hkin10Arcm,hkmode);*/

	  if(good_row && (hkmode == XRTMODE_PC))
	    {
	      if(global.scount)
		{
		  global.scount+=1;
		  global.split = (SplitValue_t *)realloc(global.split, ((global.scount)*sizeof(SplitValue_t)));
		  
		}
	      else
		{
		  global.scount+=1;
		  global.split = (SplitValue_t *)malloc(sizeof(SplitValue_t));  
		  
		}

	      global.split[global.scount - 1].ccdframe=VVEC(hktable, n, hkcol.CCDFrame);
	      global.split[global.scount - 1].hktime=DVEC(hktable, n, hkcol.TIME);
	      global.split[global.scount - 1].split=UVEC(hktable, n, hkcol.SplitThr);

	      
	    }
	  
	}


      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  if(!global.scount)
    {
      headas_chat(NORMAL,"%s: Error: The %s file\n",global.taskname, global.par.hdfile);
      headas_chat(NORMAL,"%s: Error: is not appropriate for the\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: %s events file.\n", global.taskname, global.par.infile);
      goto  Readsplitvalues_end;
    }


  ReleaseBintable(&hkhead, &hktable);
  

  return OK;
  
 Readsplitvalues_end:
   if (hkhead.first)
     ReleaseBintable(&hkhead, &hktable);
  
   return NOT_OK;

}/* ReadSplitValues */
