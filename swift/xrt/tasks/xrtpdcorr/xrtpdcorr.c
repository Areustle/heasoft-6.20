/*
 * 
 *	xrtpdcorr.c:
 *
 *	INVOCATION:
 *
 *		xrtpdcorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *                Routine to correct Photodiode PHA value 
 *                subtracting the instrument bias
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *        0.1.0 - BS 09/04/2003 - First version
 *        0.1.1 -    06/05/2003 - Added function to get BIAS value from file. 
 *        0.1.2 -    14/05/2003 - Changed 'PDPhaCorrect' function prototype
 *        0.1.3 -    27/05/2003 - Added 'EVTPHA' column to put bias subtracted pha
 *        0.1.4 -    30/06/2003 - Added routine to interpolate bias values
 *        0.1.5 -    15/07/2003 - Modified to use new prototype of 'CalGetFileName    
 *        0.1.6 -    24/07/2003 - Added CALDB fits file name into CHDU HISTORY
 *        0.1.7 -    10/10/2003 - Added routine to compute bias value
 *                                using HK file (LR)
 *        0.1.8 -    27/10/2003 - Added routine to compute bias value using gaussian
 *                                fit method. (LR and PU)
 *        0.1.9 -    03/11/2003 - Replaced call to 'headas_parstamp()' with
 *                                'HDpar_stamp()'
 *       0.1.10 -    05/11/2003 - Changed expression to select CALDB bias file
 *       0.1.11 -    07/11/2003 - Compute initial guesses for fit parameters
 *       0.1.12 -    10/11/2003 - Updated 'WriteInfoParam' and managed bias == 0
 *       0.1.13 -    14/11/2003 - Bugs fixed
 *       0.1.14 -    27/11/2003 - Disabled chech on DEFAULT for 'hkfile' parameters
 *       0.1.15 -    24/03/2004 - Added 'IsThisEvDatamode' routine to decide
 *                                what input parameters ask
 *       0.1.16 -    26/03/2004 - Deleted input files list handling
 *       0.1.17 -    06/05/2004 - Modified input parameters description and 
 *                                some input parameters name
 *       0.1.18 -    17/05/2004 - Messages displayed revision
 *       0.1.19 -    24/06/2004 - Bug fixed on 'fits_update_key' for 
 *                                LOGICAL keywords
 *       0.1.20 -    08/07/2004 - Changed BIAS datatype from J to D
 *       0.2.0  -    29/07/2004 - Renamed 'hkfile' in 'hdfile' parameter 
 *       0.2.1  -    03/08/2004 - Renamed 'event' in 'biasth' and apply it 
 *                                for LOWRATE
 *                              - Handled new telemetry format taking into 
 *                                account in-flight software changes
 *       0.2.2  -    04/08/2004 - bug fixed
 *       0.2.3  -    05/08/2004 - Modified to subtract bias if XRTPHACO == TRUE 
 *                                and BIASONBD does not exist
 *       0.2.4  -    06/08/2004 - Bug fixed
 *       0.2.5  -    30/08/2004 - Bug fixed
 *       0.2.6  -    09/09/2004 - Modified FillBiasBuff routine to fill bias buffer 
 *                                using only 'nframe' ccdframe values
 *                              - Added WriteBiasExtension routine to append a bias ext.
 *                                to output file with calculated bias value.       
 *       0.2.7  -    24/09/2004 - Bug fixed                        
 *       0.2.8  -    06/10/2004 - Do not write the Bias Extension if  'global.nobias' is TRUE
 *       0.2.9  -    13/10/2004 - Do not put the "XRTBIAS" keyword with bias value
 *       0.3.0  -    19/10/2004 - Fixed Alpha "Floating Exception"
 *       0.3.1  - BS 02/11/2004 - Bug fixed
 *       0.3.2  -    10/12/2004 - Modified routines to build bias histogram using readout time 
 *       0.3.3  -    16/12/2004 - Bug fixed
 *       0.3.4  - BS 20/04/2005 - Handled new header packet file format   
 *       0.4.0  -    26/05/2005 - Using algorithm to correct LR frame with bias already subtracted
 *                              - Changed 'biasth' input parameter range   
 *                              - Added 'biasdiff' input parameter
 *       0.4.1  -    08/06/2005 - Bug fixed in ComputeBiasDiff routine
 *       0.4.2  -    28/06/2005 - Implemented routine to recorrect bias also 
 *                                for PiledUp mode
 *       0.4.3  -    04/07/2005 - Added message with median calculated for PU mode
 *                                if chatter > 4
 *       0.4.4  -    13/07/2005 - Replaced ReadBintable with ReadBintableWithNULL
 *                                to perform check on undefined value reading hdfile
 *       0.4.5  -    09/08/2005 - Exit with error if hdfile is empty
 *                              - Create outfile if task exits with warning
 *       0.4.6  -    01/02/2006 - Bug fixed checking HPIXCT/LRHPixCt column value 
 *       0.4.7  -    10/03/2006 - minor change
 *       0.4.8  - NS 10/07/2007 - New input parameter 'thrfile'
 *                                             
 *                               
 *
 *
 *      NOTE:
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */
#define TOOLSUB xrtpdcorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "xrtpdcorr.h" 


/********************************/
/*         definitions          */
/********************************/

#define XRTPDCORR_C
#define XRTPDCORR_VERSION      "0.4.8"
#define PRG_NAME               "xrtpdcorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;

/*
 *	xrtpdcorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtpdcorr.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);  
 *           int headas_chat(int , char *, ...);
 *           void WriteInfoParams(void);
 *           void get_history(int *);
 *	     void ReadInFileNamesFile(char *);
 *           char * strcpy(char *, char *);
 *
 *      CHANGE HISTORY:
 *       
 *        0.1.0: - BS 09/04/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpdcorr_getpar()
{
  /*
   *  Get File name Parameters
   */
  
  /* Input PhotoDiode Event List Files Name */
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

  if (ReadInputFileKeywords())
    {
      goto Error;
    }

  if(PILGetFname(PAR_BIASFILE, global.par.biasfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASFILE);
      goto Error;	
    }

  if(global.biasonbd)
    {
      headas_chat(NORMAL, "%s: Info: The PHA is already bias corrected on-board\n", global.taskname);
      headas_chat(NORMAL, "%s: Info: but the correction will be done again.\n",global.taskname);
      if(PILGetInt(PAR_BIASDIFF, &global.par.biasdiff)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASDIFF);
	  goto Error;	
	}
    }
  else
    {
      if(PILGetString(PAR_METHOD, global.par.method)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_METHOD);
	  goto Error;	
	}
      if(strcasecmp( global.par.method, DF_CONST) && strcasecmp( global.par.method, DF_MN) && strcasecmp( global.par.method, DF_SG) && strcasecmp( global.par.method, DF_DG))
	{
	  headas_chat(NORMAL, "%s: Error: '%s' value for %s parameter not allowed.\n",global.taskname,global.par.method,PAR_METHOD);
	  headas_chat(CHATTY, "%s: Error: Allowed values are: %s for MEAN, %s to use a constant value,\n", global.taskname, DF_MN, DF_CONST);
	  headas_chat(CHATTY, "%s: Error: %s for a SINGLE GAUSSIAN and %s for a DOUBLE GAUSSIAN.\n", global.taskname, DF_SG, DF_DG);
	  goto Error;	
	}


      if(!strcasecmp(global.par.method, DF_CONST) )
	{
	  
	  /* Bias Value */
	  if(PILGetReal(PAR_BIAS, &global.par.bias)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIAS);
	      goto Error;	
	    }
	}
      if(!strcasecmp( global.par.method, DF_SG) || !strcasecmp( global.par.method, DF_DG))
	{ 
	  if(PILGetReal(PAR_FITTOL, &global.par.fittol)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_FITTOL);
	      goto Error;	
	    }
	}

      if(!strcasecmp( global.par.method, DF_SG) || !strcasecmp( global.par.method, DF_DG) || !strcasecmp( global.par.method, DF_MN))
	{
	  if(PILGetInt(PAR_NSIGMA, &global.par.nsigma)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NSIGMA);
	      goto Error;	
	    }
	  
	  if(PILGetInt(PAR_NCLIP, &global.par.nclip)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NCLIP);
	      goto Error;	
	    }
	}

    }

  /* if LR  */
  if(global.lr || global.biasonbd)
    {
      if(PILGetFname(PAR_HDFILE, global.par.hdfile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HDFILE);
	  goto Error;	
	}

      if(!(strcasecmp (global.par.hdfile, DF_NONE)) && (!strcasecmp( global.par.method, DF_CONST) || !strcasecmp( global.par.method, DF_MN) || !strcasecmp( global.par.method, DF_SG) || !strcasecmp( global.par.method, DF_DG)))
	{
	  headas_chat(NORMAL,"%s: Error: %s parameter set to '%s'.\n", global.taskname,PAR_HDFILE, global.par.hdfile);
	  headas_chat(NORMAL,"%s: Error: but %s parameter is set to '%s'.\n", global.taskname, PAR_METHOD, global.par.method);
	  goto Error;
	}
    }

  if(global.pu)
    {
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
	  
	  if(PILGetInt(PAR_BIASTH, &global.par.biasth)) 
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BIASTH);
	      goto Error;	
	    }
	}

    }
   
  if(PILGetInt(PAR_NFRAME, &global.par.nframe)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NFRAME);
      goto Error;	
    }

  if(PILGetInt(PAR_NEVENTS, &global.par.nevents)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_NEVENTS);
      goto Error;	
    }

  
  get_history(&global.hist);
  xrtpdcorr_info();


  return OK;
  
 Error:
  return NOT_OK;
  
} /* xrtpdcorr_getpar */

/*
 *
 * SingleGauss
 *
 */
void SingleGauss(int IFLAG, int M, int N, double X[], double FVEC[],
						double FJAC[], int LDFJAC)
{
  unsigned	i;
  double        arg, ex, fac;
  
  if (IFLAG == 1)
    /*
     *	calculate function values and store them in FVEC
     */
    for (i=0; i<M; ++i) {
      arg		= (global.xvec[i] - X[1])/X[2];
      
      FVEC[i]	= global.yvec[i] - (X[0] * exp(-arg * arg));
    }
  else if (IFLAG == 2)
    /*
     *	calculate full Jacobian matrix to be stored in FJAC
     *	function values are available in FVEC
     */
    for (i=0; i<M; ++i) {
      arg		= (global.xvec[i] - X[1])/X[2];
      ex		= (global.yvec[i] - FVEC[i])/X[0];
      fac		= 2. * X[0] * ex * arg;
      
      FJAC[i]			 = -ex;
      FJAC[LDFJAC+i]	 = -fac/X[2];
      FJAC[2*LDFJAC+i] = -fac * arg/X[2];
    }
  else if (IFLAG == 0)
    /*
     *	just print iterations
     */
    {
      headas_chat(CHATTY, "%s: Info: SingleGauss coefficients are:\n", global.taskname);
      headas_chat(CHATTY, "%s: Info: A = %6.3f bias_0 = %7.3f sigma = %6.3f chi^2 = %f\n",global.taskname, X[0], X[1], X[2], ReducedChiSquare(FVEC, M, N));
    }
  else
  {
    headas_chat(CHATTY, "%s: Error: Unexpected value of IFLAG: %d\n", global.taskname, IFLAG);
  }
} /* SingleGauss */

/*
 *	Fortran-callable wrapper for above function
 */
FCALLSCSUB7(SingleGauss, SINGLEGAUSS, singlegauss,\
	    INT, INT, INT, DOUBLEV, DOUBLEV, DOUBLEV, INT)

float SGauss(float *x)
{
	double	arg;

	arg		= (*x - global.FitParm[1])/global.FitParm[2];

	return global.FitParm[0] * exp(-arg * arg);
} /* SGauss */



void DoubleGauss(int IFLAG, int M, int N, double X[], double FVEC[],
						double FJAC[], int LDFJAC)
{
	unsigned	i;
	double		arg1, arg2, ex1, ex2, fac1, fac2;

	if (IFLAG == 1)
		/*
		 *	calculate function values and store them in FVEC
		 */
		for (i=0; i<M; ++i) {
			arg1	= (global.xvec[i] - X[1])/X[2];
			arg2	= (global.xvec[i] - DGAUSS_CONST * X[1])/X[2];
			
			FVEC[i]	= global.yvec[i] - (X[0] * exp(-arg1 * arg1) +
										X[3] * exp(-arg2 * arg2));
		}
	else if (IFLAG == 2)
		/*
		 *	calculate full Jacobian matrix to be stored in FJAC
		 *	function values are available in FVEC
		 */
		for (i=0; i<M; ++i) {
			arg1	= (global.xvec[i] - X[1])/X[2];
			arg2	= (global.xvec[i] - DGAUSS_CONST * X[1])/X[2];
			ex1		= exp(-arg1 * arg1);
			ex2		= exp(-arg2 * arg2);
			fac1	= 2. * X[0]/X[2] * arg1 * ex1;
			fac2	= 2. * X[3]/X[2] * arg2 * ex2;

			FJAC[i]			 = -ex1;
			FJAC[LDFJAC+i]	 = -(fac1 + DGAUSS_CONST * fac2);
			FJAC[2*LDFJAC+i] = -(fac1*arg1 + fac2*arg2);
			FJAC[3*LDFJAC+i] = -ex2;
		}
	else if (IFLAG == 0)
		/*
		 *	just print iterations
		 */
		{
		  headas_chat(CHATTY, "%s: Info: DoubleGauss coefficients are:\n", global.taskname);
		  headas_chat(CHATTY, "%s: Info: A = %6.3f  B = %6.3f  bias_0 = %7.3f\n ", global.taskname,X[0], X[3], X[1]); 
		  headas_chat(CHATTY, "%s: Info: sigma = %6.3f chi^2 = %8f\n", global.taskname, X[2],ReducedChiSquare(FVEC, M, N));
		}
	else
	  {
	    headas_chat(CHATTY, "%s: Error: Unexpected value of IFLAG: %d\n",global.taskname, IFLAG);
	  }
} /* DoubleGauss */

/*
 *	Fortran-callable wrapper for above function
 */
FCALLSCSUB7(DoubleGauss, DOUBLEGAUSS, doublegauss,\
			INT, INT, INT, DOUBLEV, DOUBLEV, DOUBLEV, INT)

float DGauss(float *x)
{
	double	arg1, arg2;

	arg1	= (*x - global.FitParm[1])/global.FitParm[2];
	arg2	= (*x - DGAUSS_CONST * global.FitParm[1])/global.FitParm[2];

	return global.FitParm[0] * exp(-arg1 * arg1) +
		   global.FitParm[3] * exp(-arg2 * arg2);
} /* DGauss */
/*
 *	xrtpdcorr_work
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
 *           int PDPhaCorrect (FitsFileUnit_t unit, FitsFileUnit_t unit, int *);
 *           int HDpar_stamp(fitsfile *fptr, int hdunum, int *)
 *           int ChecksumCalc(FitsFileUnit_t unit);
 *               GetFilenameExtension(char *, char *);
 *               DeriveFileName(char *, char *, char *);
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - BS 09/04/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int xrtpdcorr_work()
{
  int            status = OK, inExt, outExt, evExt;   
  int            logical;
  long           extno=-1;
  ThresholdInfo_t thrvalues;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL, outunit=NULL;              /* Input and Output fits file pointer */ 


  global.first=0;
  /* Get input file name */

  /* Initialize global vars */
  global.bias_lo=0;
  global.bias_hi=0;
  global.tot_counts=0;
  global.tot_chan=0;
  global.dim_buff=0;
  global.caldb=0;
  global.biasrows=0;
  global.warning=0;
  global.biaslvlrow=0;
  if(xrtpdcorr_checkinput())
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
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    

    
  /********************************
   *     Get CALDB thresholds file    *
   ********************************/

  if( global.pu && global.usethrfile )
    {
      extno=-1;
      
      if (!(strcasecmp (global.par.thrfile, DF_CALDB)))
	{
	  if (CalGetFileName(HD_MAXRET, global.dateobs, global.timeobs, DF_NOW, DF_NOW, KWVL_EXTNAME_XRTVSUB, global.par.thrfile,HD_EXPR, &extno))
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

      if(ReadThrFile(global.par.thrfile, extno, global.xrtvsub, &thrvalues))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read Bias Threshold level from\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: the input thrfile.\n", global.taskname);
	  goto Error;
	}
      
      global.par.biasth=thrvalues.pdbiasth;

      headas_chat(NORMAL, "%s: Info: Bias Threshold level from input thresholds file = %d\n", global.taskname, global.par.biasth);
    }

  
  /* Get Event ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.infile);      
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

  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.infile);
  /* Correct PHA  */


  if(global.biasonbd)
    {
      if(global.lr)
	{
	  if(ComputeBiasDiff())
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate difference between\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: on-board bias subtracted and median value.\n", global.taskname);
	      goto Error;
	    }  
	  
	  if(global.warning)
	    goto ok_end;
	}
      else
	{
	  if(ComputePUMedian())
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate difference between\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: on-board bias subtracted and median value.\n", global.taskname);
	      goto Error;
	    }  
	  
	  if(global.warning)
	    goto ok_end;
	}	  
      if ((PhaReCorrect(inunit, outunit)))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to correct PHA values.\n", global.taskname);
	  goto Error;
	}
     
      if(global.warning)
	goto ok_end;
      
    }
  else if((PDPhaCorrect(inunit, outunit)))
    {
      headas_chat(NORMAL, "%s: Error: Unable to correct PHA values.\n", global.taskname);
      goto Error;
    }

  if(global.warning)
    goto ok_end;

  if(global.biasrows)
    {

      /* Set XRTPHACO keyword to true */
      logical=TRUE;
      if(fits_update_key(outunit, TLOGICAL, KWNM_XRTPHACO, &logical, CARD_COMM_XRTPHACO, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_XRTPHACO);
	  headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	  goto Error;
	}
      
      if(!global.biasonbd)
	{
	  /* Add or update METHOD keyword */
	  if(fits_update_key(outunit, TSTRING, KWNM_METHOD, global.method, CARD_COMM_METHOD, &status))
	    {
	      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_METHOD);
	      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpfile);
	      goto Error;
	    }
	}
      

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
  
  if ( global.biasrows ) 
    {
      if(WriteBiasExtension(inunit, outunit, global.biasrows, global.outbias))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to write %s extension\n", global.taskname, KWVL_EXTNAME_BIAS);
	  headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outfile);
	  goto Error;
	}

      /* Add history if parameter history set */
      if(HDpar_stamp(outunit, evExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto Error;
	}
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

  if (global.caldb)
    free(global.biasvalues);

  if(global.biasrows)
    free(global.outbias);

  return OK; 
  
 Error:
  if (global.caldb)
    free(global.biasvalues);
  if(global.biasrows)
    free(global.outbias);
  return NOT_OK;
} /* xrtpdcorr_work */
/*
 *
 *      PDPhaCorrect
 *
 *	DESCRIPTION:
 *             Routine to compute bias value and correct events PHA
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int ExistsKeyWord(FitsHeader_t, char *, char *);
 *             BOOL GetLVal(FitsHeader_t, char *);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
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
 *        0.1.0: - BS 09/04/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PDPhaCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  int                pha=0;
  char               comm[256]="";
  BOOL               first=1;
  double             pha_dbl=0., sens=0.0000001, evtime=0.0, stoptime=0.0, bias=0.;
  unsigned           ccdframe_start=0, ccdframe_old=0, ccdframe=0;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  Ev2Col_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  head=RetrieveFitsHeader(evunit);

  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }

  /* Get cols index from name */
  if(global.oldtlm)
    {
      /* RAWPHA */
      if ((indxcol.RAWPHA=ColNameMatch(CLNM_RAWPHA, &outtable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWPHA);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto event_end;
	}
    }
  /* PHA */
  /* Add PHA column if needed */
  if((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    { 
      headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_PHA);
      headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile); 
      AddColumn(&head, &outtable, CLNM_PHA, CARD_COMM_PHA, "1J", TUNIT|TNULL, UNIT_CHANN, CARD_COMM_PHYSUNIT, KWVL_PHANULL);
      indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable);
    }

  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;
    }

  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto event_end;      
    }



  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      if(global.oldtlm)
	sprintf(global.strhist, "File modified by '%s' (%s) at %s: calcolated PHA from RAWPHA values.", global.taskname, global.swxrtdas_v,global.date );
      else
	sprintf(global.strhist, "File modified by '%s' (%s) at %s: subtracted bias from PHA values.", global.taskname, global.swxrtdas_v,global.date );
      AddHistory(&head, global.strhist);

      if(global.lr && !strcasecmp(global.method, DF_CONST))
	{
	  if(global.caldb)
	    sprintf(comm, "Used %s CALDB file to get bias value.", global.par.biasfile);
	  else 
	    sprintf(comm, "Used %s HK file to compute bias value.", global.par.hdfile);
	  AddHistory(&head, comm);
	}
      else if(global.pu && !strcasecmp(global.method, DF_CONST) && global.caldb)
	{
	  sprintf(comm, "Used %s CALDB file to get bias value.", global.par.biasfile);

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
	  
	  evtime=DVEC(outtable,n,indxcol.TIME_RO);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);
	  
	  if(first)
	    {
	      first=0;
	      ccdframe_start=ccdframe;
	      /* Compute bias value */
	      
	      if( BiasValue( &bias, ccdframe_start, evtime, &stoptime))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to calculate bias value.\n", global.taskname);
		  goto event_end;
		}
	      if(global.warning)
		return OK;
	      
	      /*printf("evtime == %5.5f -- stoptime == %5.5f ccdframe_start==%u\n", evtime, stoptime, ccdframe_start);*/
	      headas_chat(NORMAL, "%s: Info: Subtracted bias value: %3.2f\n",global.taskname, bias);
	    }
	  else if(global.par.nframe > 0 && ccdframe != ccdframe_old && strcasecmp(global.par.method, DF_CONST) && (evtime > (stoptime + sens)))
	    {
	      
	      ccdframe_start=ccdframe;


	      /* Compute bias value */
	      if( BiasValue( &bias, ccdframe_start, evtime, &stoptime))
		{
		  headas_chat(NORMAL, "%s: Error: Unable to calculate bias value.\n", global.taskname);
		  goto event_end;
		}
	      if(global.warning)
		return OK;
	      /*printf("evtime == %5.5f -- stoptime == %5.5f ccdframe_start==%u\n", evtime, stoptime, ccdframe_start);*/
	      headas_chat(NORMAL, "%s: Info: Subtracted bias value: %3.2f\n",global.taskname, bias);
		  		  
	    }

	  if(ccdframe_old != ccdframe)
	    {
	      /*printf("evtime == %5.5f -- stoptime == %5.5f ccdframe==%u\n", evtime, stoptime, ccdframe);*/
	      
	      if(!global.biasrows)
		{
		  global.biasrows++;
		  global.outbias = (BiasFormat_t *)malloc(sizeof(BiasFormat_t));
		}
	      else
		{
		  global.biasrows++;
		  global.outbias = (BiasFormat_t *)realloc(global.outbias, (global.biasrows*sizeof(BiasFormat_t)));
		  
		}

	      global.outbias[global.biasrows-1].ccdframe = ccdframe;
	      global.outbias[global.biasrows-1].bias = bias;
	      	  
	      if ((bias >= (0. - sens)) && (bias  <= (0.+sens)))
		{
		  headas_chat(NORMAL, "%s: Info: Bias value is %f\n", global.taskname, bias);
		  headas_chat(NORMAL, "%s: Info: Nothing will be done for %d frame.\n", global.taskname,ccdframe ); 
		}
	    }

	  ccdframe_old = ccdframe;
	  if(global.oldtlm)
	    JVEC(outtable, n, indxcol.PHA)=JVEC(outtable, n, indxcol.RAWPHA);
	  
	  /* Get raw pha value */
	  pha=JVEC(outtable, n, indxcol.PHA);
	  
	  /* Put corrected pha value into PHA column */
	  if(pha != SAT_VAL)
	    {
	      pha_dbl=(pha - bias);
	      JVEC(outtable, n, indxcol.PHA)=(int)floor(pha_dbl + 0.5);
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

 event_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* PDPhaCorrect */
/*
 *
 *	GetBiasValues
 *
 *
 *	DESCRIPTION:
 *                 Routine to compute bias values using  CALDB 
 *                 bias file
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
 *        0.1.0: - BS 10/03/2003 - First version
 */

int GetBiasValues(void)
{
  int            n, status=OK;
  long           extno;
  char           expr[256];
  unsigned       FromRow, ReadRows, OutRows, nCols, i; 
  BiasCol_t      biascol; 
  Bintable_t     table;             /* Bintable pointer */  
  FitsHeader_t   head;              /* Extension pointer */
  FitsFileUnit_t biasunit=NULL;        /* Bias file pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  

  /* Derive CALDB bias filename */ 
  if (!(strcasecmp(global.par.biasfile, DF_CALDB)))
    {
      if(global.pu)
	sprintf(expr, "datamode.eq.%s", KWVL_DATAMODE_PDPU);
      else
	sprintf(expr, "datamode.eq.%s", KWVL_DATAMODE_PD);
      
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_BIAS_DSET, global.par.biasfile, expr, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto get_end;
	}
      else
	headas_chat(NORMAL,"%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.biasfile);
    }

  /* Open read only bias file */
  if ((biasunit=OpenReadFitsFile(global.par.biasfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.biasfile);
      goto get_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname, global.par.biasfile);
  
  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL, biasunit, global.par.biasfile,global.taskname);
  
  /* move to  BIAS extension */
  if (fits_movnam_hdu(biasunit,ANY_HDU,KWVL_EXTNAME_BIAS,0,&status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_BIAS);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.biasfile);
      goto get_end;
    } 
  
  head = RetrieveFitsHeader(biasunit);
  
  /* Read bias bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.biasfile);
      goto get_end;
    }


  /* Get columns index from name */
  if ((biascol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.biasfile);
      goto get_end;
    }
  
  if ((biascol.BIAS = GetColNameIndx(&table, CLNM_BIAS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BIAS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.biasfile);
      goto get_end;
    }
  
  global.caldbrows =table.MaxRows;
  
  /* Allocate memory to storage all coefficients */
  global.biasvalues=(BiasRow_t *)calloc(global.caldbrows, sizeof(BiasRow_t));
  global.caldb=1;
  /* Read blocks of bintable rows from input bias file */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  OutRows=0;
  i=0;
  while (ReadBintable(biasunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  /* get columns value */
	  global.biasvalues[i].time=DVEC(table,n,biascol.TIME);
	  global.biasvalues[i].bias=DVEC(table,n,biascol.BIAS);
	  i++;
	}
      
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close bias file */
  if (CloseFitsFile(biasunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.biasfile);
      goto get_end;
    }
  
 
  return OK;
  
 get_end:
  
  return NOT_OK;
  
}/* GetBiasValues */

/*
 *	xrtpdcorr
 *
 *
 *	DESCRIPTION:
 *              Routine to correct raw pha events value for PHOTODIODE readout modes  
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
 *             void xrtpdcorr_getpar(void);
 *             void WriteInfoParams(void); 
 * 	       void xrtpdcorr_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 09/04/2003 - First version
 *        0.1.1:      27/05/2003 - Bug fixed on 'xrtpdcorr_getpar' failure
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtpdcorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(XRTPDCORR_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /* Get parameter values */ 
  if ( xrtpdcorr_getpar() == OK) {
    
    if ( xrtpdcorr_work() ) {
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

} /* xrtpdcorr */
/*
 *	xrtpdcorr_info:
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
 *        0.1.0: - BS 09/04/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
void xrtpdcorr_info(void)
{

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);
  headas_chat(NORMAL,"Name of the output Event file                         :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the BIAS file                                 :'%s'\n",global.par.biasfile);
  if(global.biasonbd && global.lr)
    headas_chat(CHATTY,"Bias difference                                       : %d\n",global.par.biasdiff);
  if(!global.biasonbd)
    {
      headas_chat(NORMAL,"Method used to calculate bias value                   : %s\n",global.par.method);
      if(!strcasecmp(global.par.method, DF_CONST) )
	headas_chat(NORMAL,"Bias value                                            : %f\n",global.par.bias);
      else if(!strcasecmp( global.par.method, DF_SG) || !strcasecmp( global.par.method, DF_DG))
	{
	  headas_chat(CHATTY,"Relative tolerance of fit error                     : %f\n",global.par.fittol);
	}

      if(!strcasecmp( global.par.method, DF_SG) || !strcasecmp( global.par.method, DF_DG) || !strcasecmp( global.par.method, DF_MN))
	{
	  headas_chat(NORMAL,"Maximum number of iterations                          : %d\n",global.par.nclip);
	  headas_chat(NORMAL,"Number of sigma to clip                               : %d\n",global.par.nsigma);
	}
    }

  if(global.lr||global.biasonbd)
    {
      headas_chat(NORMAL,"Name of the input Housekeeping Header Packet file      :'%s'\n",global.par.hdfile);
    }
  if(global.pu)
    {
      headas_chat(NORMAL,"Name of the input Thresholds file                      :'%s'\n",global.par.thrfile);
      if(!global.usethrfile)
	{
	  headas_chat(NORMAL,"Bias Threshold                                        : %d\n",global.par.biasth);
	}
    }
  headas_chat(NORMAL,"Minimum number of events to calculate the bias value  : %d\n",global.par.nevents);
  headas_chat(NORMAL,"Number of consecutive frames to calculate the bias    : %d\n",global.par.nframe);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");

  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* xrtpdcorr_info */

/*
 *
 *
 *  PDBiasVal
 *
 *
 *  DESCRIPTION:
 *        Routine to compute bias value using HK file
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 09/10/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
 

int PDBiasVal( double * bias, unsigned ccdframe_start, double evtime, double *stoptime)
{
  int                nFitParm=MAX_FIT_PARM;
  BOOL               UseSingleGaussian=0;
  double             GaussFitParm[MAX_FIT_PARM], RedChiSq=0, bias_db=0.,sig_mean=0., sens=0.0000001;
  

  /* Open HK file and get pixel values */
  if(FillBiasBuff(ccdframe_start, evtime, stoptime))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to create bias histogram.\n", global.taskname);
      goto  NoFit_end;
	
    }
  if(global.warning)
    return OK;
  
  if(ComputeMean(&bias_db, &sig_mean))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to calculate bias using %s method.\n", global.taskname, global.par.method);
      goto  NoFit_end;
    }

  headas_chat(NORMAL, "%s: Info: Mean value of the bias is :%3.2f\n",global.taskname, bias_db);

  if(global.tot_counts >= global.par.nevents)
    {  
      if(strcasecmp(global.par.method, DF_SG))
	UseSingleGaussian=0;
      else
	UseSingleGaussian=1;
      nFitParm = UseSingleGaussian ? 3 : 4;
      


      if(sig_mean >= (0. - sens) && sig_mean <= (0. + sens))
	{
	  
	  goto  NoFit_end;
	}

      if(FitBias(nFitParm, GaussFitParm, &RedChiSq, UseSingleGaussian, bias_db, sig_mean))
	{
	  headas_chat(NORMAL, "%s: Warning: Unable to fit bias histogram with a gaussian distribution.\n", global.taskname);
	  goto NoFit_end;
	}
   
      bias_db=GaussFitParm[1];
      *bias = bias_db;
      strcpy(global.method, global.par.method);
  
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: There are only %d events to build bias histogram\n", global.taskname, global.tot_counts);
      headas_chat(NORMAL, "%s: Warning: but the minimum number requested is: %d\n", global.taskname, global.par.nevents);
      headas_chat(NORMAL, "%s: Warning: Unable to fit bias histogram with a gaussian distribution.\n", global.taskname);
      goto NoFit_end;
      
    }

  return OK;

 NoFit_end:  
  headas_chat(NORMAL, "%s: Info: Using CALDB file to calculate bias value.\n", global.taskname);

  if(CALDBBiasVal(bias))
    {	    
      headas_chat(NORMAL, "%s: Error: Unable to calculate bias value using\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.biasfile);
      return NOT_OK;
    }

  
 
  return OK;

   
}/* PDBiasVal */

/*
 *   FitBias
 *
 *
 *  DESCRIPTION:
 *        Routine to fit a histogram with a gaussian distribution 
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *              
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 09/10/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 * 
 *
 */
int FitBias( unsigned nFitParm, double GaussFitParm[MAX_FIT_PARM],double *RedChiSq, BOOL UseSingleGaussian, double mean, double sig_mean)
{
  int		Info, iw[MAX_FIT_PARM], wa_len=0, bias_tmp=0, old_hi=0, old_lo=0, counts=0, min=0, max=0;
  unsigned	n, i;
  
  
  double		*FunVals, *wa;

  if (global.par.nclip < 0)
    {
      headas_chat(NORMAL, "%s: Warning: The value of the input parameter '%s' is not allowed\n", global.taskname, PAR_NCLIP);
      headas_chat(NORMAL, "%s: Warning: Using the default value: %s=0.\n", global.taskname, PAR_NCLIP);
      global.par.nclip=0;
    }
  if(global.par.nclip > 0)
    if(global.par.nsigma <= 0)
      {
	headas_chat(NORMAL, "%s: Warning: The value of the input parameter '%s' is not allowed\n", global.taskname, PAR_NSIGMA);
	headas_chat(NORMAL, "%s: Warning: Using the default value: %s=2.\n", global.taskname, PAR_NSIGMA);
	global.par.nsigma=2;
      }
  
  old_hi=global.bias_hi;
  old_lo=global.bias_lo;

  for(i=0; i <= global.par.nclip; i++)
    {
      /* Total number of channels to build histogram */
      global.tot_chan = global.bias_hi - global.bias_lo + 1;
  
      FunVals=(double *)calloc(global.tot_chan, sizeof(double));
      wa_len=(MAX_FIT_PARM *(global.tot_chan+5)+global.tot_chan);
      wa=(double *)calloc(wa_len, sizeof(double));
	

      global.xvec=(double *)calloc(global.tot_chan, sizeof(double));
      global.yvec=(double *)calloc(global.tot_chan, sizeof(double));

      /*
       *	populate global vectors, <xvec>, <yvec>
       */
  
      for(n=0; n < global.tot_chan; n++)
	global.xvec[n]=(double)(n+global.bias_lo);
      
      for(n=0; n < global.dim_buff ; n++)
	{
	  if((global.biashisto[n].level - global.bias_lo) >=0 && (global.biashisto[n].level - global.bias_lo) < global.tot_chan)
	    global.yvec[(global.biashisto[n].level - global.bias_lo)]=global.biashisto[n].counts;
	}
      
      for(n=0, counts=0; n < global.tot_chan; n++)
	{
	  counts+=(int)global.yvec[n];
	}

      /*
       *	make initial guesses for fit parameters:
       */
      
      /* 
       * Compute height of Gaussian
       *
       */

      if(i==0)
	{
	  GaussFitParm[0]=0;
	  min=(int)((mean - sig_mean) - global.bias_lo);
	  max=(int)((mean + sig_mean) - global.bias_lo);
	  if (min >= 0 && max < global.tot_chan)
	    {
	      for (; min <= max ; min++)
		GaussFitParm[0]+=(int)global.yvec[min];

	      GaussFitParm[0]=GaussFitParm[0]/(2*sig_mean);
	    }
	  else
	    GaussFitParm[0] = GUESS_FPARM1;
	}

      if (!UseSingleGaussian)
	/*
	 *	height of second Gaussian is approx.
	 *	15% of first one
	 */
	GaussFitParm[3] = .15 * GaussFitParm[0];
      
      
      /*
       *	position of Gaussian, for this we take the median
       */
  
      counts = counts/2.;
      n=0;
      while (counts > 0 && n < global.tot_chan)
	{
	  counts-=global.yvec[n];
	  n++;
	}

      GaussFitParm[1]=(global.tot_chan > 0)?global.xvec[n-1]:GUESS_FPARM2;/*global.xvec[((int)(global.tot_chan/2))];*/
      
      /*
       *	width of Gaussian
       */
      GaussFitParm[2] = sig_mean;
  
      /*
       *	now, call the SLATEC routine DNLS1E
       */
  
      SLATEC_DNLS1E(UseSingleGaussian ? C_FUNCTION(SINGLEGAUSS, singlegauss)
		    : C_FUNCTION(DOUBLEGAUSS, doublegauss),/* the function   */
		    2,			/* IOPT = 2: we provide the full Jacobian  */
		    global.tot_chan,	/* the number of functions		   */
		    nFitParm,		/* the number of variables		   */
		    GaussFitParm,	/* vector with values of fit parameters	   */
		    FunVals,		/* will contain function values on exit	   */
		    global.par.fittol,	/* tolerance of the least-square fit	   */
		    1,		        /* iteration print flag			   */
		    Info,		/* info flag set on exit		   */
		    iw, wa,		/* work arrays				   */
		    wa_len	        /* length of wa				   */);


      *RedChiSq = 0;
      switch (Info) {
      case 1:
      case 2:
      case 3:
      case 4: /* found solution is ok */
	*RedChiSq = ReducedChiSquare(FunVals, global.tot_chan, nFitParm);
	headas_chat(CHATTY, "%s: Info: ChiSq= %1.7f\n",global.taskname, *RedChiSq);
	if (Info == 4)
	  headas_chat(CHATTY, "%s: Warning: Solution found but vector with square values is orthogonal\n to the columns of the Jacobian to machine precision\n - proceed with care.\n", global.taskname);
	break;
      case 5: /* too many calls to function */
	headas_chat(CHATTY, "%s: Error: Too many function evaluations (%d) - unsuccessful fit.\n", global.taskname, 100*(global.tot_chan+1));
	return NOT_OK;
      case 6: /* tolerance is too small */
	headas_chat(CHATTY, "%s: Error: Given tolerance too small.\n No further reduction in the sum of squares is possible.\n",global.taskname);
	goto fit_end;
      case 7: /* tolerance is too small */
	headas_chat(CHATTY, "%s: Error: Given tolerance too small.\n No improvement in the approximate solution X is possible.\n", global.taskname);
	goto fit_end;
      default: /* this should not happen */
	headas_chat(NORMAL, "%s: Error: Unexpected return value %d from function 'DNLS1E'\n",global.taskname, Info);
	goto fit_end;
      }
      /* free allocated memory */
      free(wa);
      free(FunVals);
      free(global.xvec);
      free(global.yvec);
      
      
      
      if(i+1 <= global.par.nclip)
	{
	  
	  if(GaussFitParm[2] <= 0)
	    {
	      GaussFitParm[2] = - GaussFitParm[2];

	    }
	 	  
	  bias_tmp=(int)(GaussFitParm[1] - global.par.nsigma*GaussFitParm[2]);
	  if(bias_tmp > global.bias_lo)
	    global.bias_lo =bias_tmp;
	  bias_tmp=(int)((GaussFitParm[1] + global.par.nsigma*GaussFitParm[2] - 1));
	  if(bias_tmp < global.bias_hi)
	    global.bias_hi=bias_tmp; 
	  
	  if(i!=0 && global.bias_lo == old_lo && global.bias_hi == old_hi)
	    {
	      headas_chat(NORMAL, "%s: Info: No more points are rejected! End of clipping.\n", global.taskname);
	      i=global.par.nclip+1;
	    }
	  else if((global.bias_hi - global.bias_lo)<=0)
	    {
	      headas_chat(CHATTY, "%s: Warning: bias_lo = %d but bias_hi = %d\n", global.taskname, global.bias_lo, global.bias_hi);
	      goto fit_2_end;
	    }
	  else
	    {
	      old_hi=global.bias_hi;
	      old_lo=global.bias_lo;
	      
	      headas_chat(CHATTY, "%s: Info: New fit range : bias_lo = %d\t bias_hi = %d\n", global.taskname, global.bias_lo, global.bias_hi);
	    }
	   /*   }	 */
	}
    }
  headas_chat(NORMAL, "%s: Info: bias:%3.2f\t sigma:%3.2f\n", global.taskname, GaussFitParm[1], GaussFitParm[2]);

  return OK;

 fit_end:
  free(wa);
 fit_2_end: 
 free(FunVals);
  free(global.xvec);
  free(global.yvec);


  return NOT_OK;
} /* FitBias */

double ReducedChiSquare(double Vec[], int nPoints, int nVars)
{
	unsigned	n;
	double		ChiSq;

	ChiSq = 0.;
	for (n=0; n<nPoints; ++n)
		ChiSq += Vec[n] * Vec[n];

	if (nPoints - nVars)
		return ChiSq/(double)(nPoints - nVars);
	else {
		headas_chat(CHATTY, "%s: Error: N-m is zero, Chi^2 value not reduced.\n", global.taskname);
		return ChiSq;
	}
} /* ReducedChiSquare */

/* 
 *
 *
 *  BiasValue
 *
 *  DESCRIPTION:
 *       Routine to compute bias value
 */

int BiasValue(double * bias, unsigned ccdframe_start, double evtime, double *stoptime)
{
  double             bias_db=0., sig_mean=0.;

  /*printf(">> evtime=%5.5f -- ccdframe=%u\n", evtime,ccdframe_start);*/

  if(!strcasecmp(global.par.method, DF_CONST))
    {
      /* Get bias value from CALDB file, if it is necessary */
      if (global.par.bias < 0 )
	{
	  if(CALDBBiasVal(bias))
	    {	
	      headas_chat(NORMAL, "%s: Error: Unable to calculate bias value using\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.biasfile);    
	      goto bias_end;
	    }
	}
      else
	*bias=global.par.bias;
      
    }
  else if((!strcasecmp(global.par.method, DF_DG)) ||(!strcasecmp(global.par.method, DF_SG)))
    {
      
 	  
      if(PDBiasVal(bias, ccdframe_start, evtime, stoptime))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to calculate bias value.\n", global.taskname);
	  goto bias_end;
	}
      if(global.warning)
	return OK;
            
    }
  else if((!strcasecmp(global.par.method, DF_MN)))
    {

      if(FillBiasBuff(ccdframe_start, evtime, stoptime) || global.warning)
	{
	  if(global.warning)
	    return OK;
	  headas_chat(NORMAL, "%s: Warning: Unable to build bias histogram.\n", global.taskname);
	  if(CALDBBiasVal(bias))
	    {	
	      headas_chat(NORMAL, "%s: Error: Unable to calculate bias value using\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.biasfile);    
	      goto bias_end;
	    }
	}
      else if(ComputeMean(&bias_db, &sig_mean))
	{
	  headas_chat(NORMAL, "%s: Warning: Unable to calculate bias using %s method.\n", global.taskname, global.par.method);
	  if(CALDBBiasVal(bias))
	    {	
	      headas_chat(NORMAL, "%s: Error: Unable to calculate bias value using\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.biasfile);    
	      goto bias_end;
	    }
	}
      else
	{
	  strcpy(global.method, DF_MN);
	  *bias = bias_db; /*floor(bias_db + 0.5);*/
	}
    }
  
  if(global.dim_buff != 0)
    {
      free(global.biashisto);
      global.dim_buff=0;
      global.tot_counts=0;
      global.bias_hi=0;
      global.bias_lo=0;
    }

  return OK;

 bias_end: 

  if(global.dim_buff != 0)
      free(global.biashisto);



  return NOT_OK;
}/* BiasVal */
/*
 *  CALDBBiasVal
 *
 *  DESCRIPTION:
 *      Routine to compute bias value using Calibration file
 */
int CALDBBiasVal(double* bias)
{

  int                i;
  double            sens=0.0000001;
  BOOL               found=0;

  if(!global.first)
    {
      if(GetBiasValues())
	{
	  headas_chat(NORMAL,"%s: Error: Unable to process\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: %s file.\n", global.taskname, global.par.biasfile);
	  goto caldb_end;
	}
      else
	global.first=1;
    }

  if(global.caldbrows == 1)
    *bias = global.biasvalues[0].bias;
  else if((global.tstart <= global.biasvalues[0].time + sens))
    *bias = global.biasvalues[0].bias;
  else
    { /* Compute bias value */
      
      for (i=1; i < global.caldbrows; i++)
	{
	  if (global.tstart < global.biasvalues[i].time  + sens)
	    {

	      /*  if ((global.tstart > (global.biasvalues[i-1].time - TIMEDELT)) && (global.tstart < (global.biasvalues[i-1].time + TIMEDELT))) */
/*  		bias_db=global.biasvalues[i-1].bias; */
/*  	      else */
/*  		{ */
/*  		  deltat=global.biasvalues[i].time - global.biasvalues[i-1].time; */
/*  		  frac1= (global.biasvalues[i].time - global.tstart)/deltat; */
/*  		  frac2= (global.tstart - global.biasvalues[i-1].time)/deltat; */
/*  		  bias_db=global.biasvalues[i-1].bias * frac1 + global.biasvalues[i].bias * frac2 ; */
/*  		  *bias = bias_db; */
		  
/*  		} */
	      *bias = global.biasvalues[i-1].bias;
	      found=1;
	      i=global.caldbrows;
	    }
	}/* End for(i=1; ....) */
      if (!found)
	*bias=global.biasvalues[global.caldbrows-1].bias;
    }

  strcpy(global.method, DF_CONST);
  return OK;

 caldb_end:
  return NOT_OK;
}/* CALDBBiasVal */

/*
 * 
 *   ComputeMean
 *   
 *   DESCRIPTION:
 *     Routine to compute bias mean value
 *
 */

int ComputeMean(double * mean_db, double *sigma)
{

  int  ii=0, i, tmp_counts=0, high=0, low=0, old_tmp_counts=0;
  
  double totbias=0., totsigma=0.;


  high=global.bias_hi;
  low=global.bias_lo;
  old_tmp_counts=global.tot_counts;
  
  for(i=0; i<=global.par.nclip; i++)
    {
      /* Compute mean */
      totbias=0.;
      tmp_counts=0.;
      for(ii=0; ii < global.dim_buff; ii++)
	{
	  if(global.biashisto[ii].level >= low && global.biashisto[ii].level <= high)
	    {
	      totbias+=(double)(global.biashisto[ii].level*global.biashisto[ii].counts);
	      tmp_counts+=global.biashisto[ii].counts;
	      
	    }
	}  
      
      if(tmp_counts > 0)
	{
	  *mean_db=(totbias/tmp_counts);
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: The total of the event counts is: %d\n", global.taskname, tmp_counts);
	  return NOT_OK;
	}

         /* Compute sigma */
      totsigma=0;
      for(ii=0; ii < global.dim_buff; ii++)
	{
	  if(global.biashisto[ii].level >= low && global.biashisto[ii].level <= high)
	    totsigma+=(double)(global.biashisto[ii].counts*(global.biashisto[ii].level - *mean_db)*(global.biashisto[ii].level - *mean_db));
	    
	}  
      
      *sigma = sqrt(totsigma/tmp_counts);

      headas_chat(CHATTY, "%s: Info: \tsigma:%3.3f - mean:%3.3f\n", global.taskname, *sigma, *mean_db); 

      if((i+1) <= global.par.nclip)
	{
	  if(i == 0 || old_tmp_counts != tmp_counts)
	    {
	      old_tmp_counts=tmp_counts;
	      high=(int)floor(*mean_db+(global.par.nsigma*(*sigma)) + 0.5);
	      low=(int)floor(*mean_db-(global.par.nsigma*(*sigma)) + 0.5);

	      headas_chat(CHATTY, "%s: Info: new bias range is : high:%d - low:%d\n", global.taskname, high, low); 
	    }
	  if(i!=0 && old_tmp_counts == tmp_counts)
	    {
	      headas_chat(CHATTY, "%s: Warning: No more points are rejected! End of clipping.\n", global.taskname);
	      i=global.par.nclip+1;
	    }
	  
	}
    }

  return OK;
	  

}/* ComputeMean */


/*
 *
 *  FillBiasBuff
 *
 *  DESCRIPTION:
 *    Routine to build bias histogram with PHA value
 *
 *
 */
int FillBiasBuff(unsigned ccdframe_start, double evtime, double *stoptime)
{  
  int                status=OK, mul=0, jj=0, mode=0;
  int                ii=0;
  BOOL               found=0, first=1, search=1;
  BOOL               allframe=0, incluseframe=0, offres=0, backframe=0, search_end=0;
  double             sens=0.0000001, curr_time=0.;
  unsigned           ccdframe_tmp=0, ccdframe_old=0, ccdframe_stop=0;
  unsigned           FromRow, ReadRows, n, nCols;
  HKCol_t            hkcol;
  Ev2Col_t           evcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;



  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /*printf(">>> evtime=%5.5f -- ccdframe=%u\n", evtime,ccdframe_start);*/

  
  /* calculate ccdframe_stop */

  if(global.par.nframe > 0)
    {
      if((CCDFRAME_MAX - ccdframe_start + 1 ) >= global.par.nframe)
	{
	  ccdframe_stop = ccdframe_start + global.par.nframe - 1;
	}
      else
	{
	  ccdframe_stop = global.par.nframe - (CCDFRAME_MAX - ccdframe_start + 1) - 1;
	}
    }
  else
    ccdframe_stop=0; 
  
  if(global.lr)
    {
      
      /* Open readonly input hk file */
      if ((unit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}
      
      /* Move in frame extension in input hk file */
      if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile);
	  if( CloseFitsFile(unit))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	    }
	
	  goto FillBiasBuff_end;
	  
	}

      head=RetrieveFitsHeader(unit);
      if(ExistsKeyWord(&head, KWNM_TSTART, &card))
	{
	  /*printf(">>>>> tstart=%5.5f -- hkstart=%5.5f\n", global.tstart,(card->u.DVal-TIMEDELT));*/
	  if(global.tstart < (card->u.DVal - TIMEDELT))
	    {
	      headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
   	      goto FillBiasBuff_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}

      if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
	{
	  /* printf(">>>>> tstop=%5.5f -- hkstop=%5.5f\n", global.tstop,(card->u.DVal+TIMEDELT));*/
	  if(global.tstop > (card->u.DVal + TIMEDELT))
	    {
	      headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	      headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	      goto FillBiasBuff_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}

      GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
      nCols=table.nColumns;

      if(!table.MaxRows)
	{
	  headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}


      /* Get needed columns number from name */
      
      /*  XRTMode */
      if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}

      if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}

      /* LRBiasPx */
      if ((hkcol.BiasPx=ColNameMatch(CLNM_LRBiasPx, &table)) == -1)
	{
	  if ((hkcol.BiasPx=ColNameMatch(CLNM_BiasPx, &table)) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: '%s' and '%s' columns do not exist\n", global.taskname, CLNM_LRBiasPx, CLNM_BiasPx);
	      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	      goto FillBiasBuff_end;
	    }
	  
	}
      else
	{
	  headas_chat(CHATTY, "%s: Info: '%s' column exists\n", global.taskname, CLNM_LRBiasPx);
	  headas_chat(CHATTY, "%s: Info: '%s' file is in old format. \n", global.taskname, global.par.hdfile);
	}


      /* LRHPixCt */
      if ((hkcol.HPIXCT=ColNameMatch(CLNM_LRHPIXCT, &table)) == -1)
	{
	  if ((hkcol.HPIXCT=ColNameMatch(CLNM_HPIXCT, &table)) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: '%s' and '%s' columns do not exist\n", global.taskname, CLNM_LRHPIXCT, CLNM_HPIXCT);
	      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	      goto FillBiasBuff_end;
	    }
	  
	}
      else
	{
	  headas_chat(CHATTY, "%s: Info: '%s' column exists\n", global.taskname,CLNM_LRHPIXCT );
	  headas_chat(CHATTY, "%s: Info: '%s' file is in old format. \n", global.taskname, global.par.hdfile);
	}

      /* TIME */
      if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}
  
      /* ENDTIME */
      if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto FillBiasBuff_end;
	}

      EndBintableHeader(&head, &table);
 
      FromRow = 1;
      ReadRows=table.nBlockRows;
      
      /* Read only the following columns */

      search=1;
      search_end=0;
      while((ReadBintableWithNULL(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0) && !search_end )
	{
	  for(n=0; n<ReadRows ; ++n)
	    {


	      mul=IVEC(table,n,hkcol.HPIXCT);
	      if(!mul)
		{
		  headas_chat(NORMAL, "%s: Warning: the %s column contains %d\n", global.taskname, CLNM_HPIXCT, mul);
		  headas_chat(NORMAL, "%s: Warning: so the %s column is empty.\n", global.taskname, CLNM_BiasPx);
		  goto  FillBiasBuff_end;
		  
		}

	      if(mul > table.Multiplicity[hkcol.BiasPx])
		{
		  headas_chat(NORMAL, "%s: Error: problem with multiplicy of the %s column.\n", global.taskname, CLNM_BiasPx);
		  goto  FillBiasBuff_end;
		}
      
	      if(first)
		ccdframe_old=0;
	      else
		ccdframe_old=ccdframe_tmp;
	      
	      curr_time=(VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
	      ccdframe_tmp=VVEC(table, n, hkcol.CCDFrame);
	      
	      if (search && global.par.nframe > 0 )
		{
		  
		  if((ccdframe_tmp == ccdframe_start) && (curr_time >= evtime-sens && curr_time <= evtime+sens))
		    {
		      /*printf(">>> time=%5.5f -- ccdframe_tmp=%u\n", time,ccdframe_tmp);*/
		      search=0;
		    }
		  else
		    continue;
		}

 
	      if(global.par.nframe <= 0)
		allframe=1;
	      else 
		allframe=0;

	      if ((ccdframe_start <= ccdframe_stop) && 
		  ((ccdframe_tmp <= ccdframe_stop) && (ccdframe_tmp >= ccdframe_start)))
		incluseframe=1;
	      else
		incluseframe=0;

	      if ((ccdframe_start > ccdframe_stop) && 
		  ((ccdframe_tmp <= CCDFRAME_MAX ) || ((ccdframe_tmp >= CCDFRAME_MIN) && (ccdframe_tmp <= ccdframe_stop ))))
		offres=1;
	      else
		offres=0;
	      
	      if(ccdframe_tmp > ccdframe_old || first)
		{
		  first=0;
		  backframe=1;
		}
	      else
		backframe=0;
		 

	      if(allframe || ((incluseframe ||  offres ) && backframe))
		{
		  
		  mode=BVEC(table, n, hkcol.XRTMode);
		  if(mode == XRTMODE_LR)
		    {
		      
		      *stoptime=curr_time;
		      
		      /* Compute histogram */
		      
		      for (jj=0; jj< mul; jj++)
			{
			  if(IVECVEC(table,n,hkcol.BiasPx,jj) >= 0)
			    {
			      global.tot_counts++;
			      
			      if(global.dim_buff == 0)
				{
				  global.dim_buff++;
				  global.biashisto=(BiasPix_t *)calloc(global.dim_buff, sizeof(BiasPix_t));
				  global.biashisto[global.dim_buff-1].level=IVECVEC(table,n,hkcol.BiasPx,jj);
				  global.biashisto[global.dim_buff-1].counts=1;
				  global.bias_hi=global.bias_lo=global.biashisto[global.dim_buff-1].level;
				  
				}
			      else
				{
				  found=0;
				  for (ii=0; ii < global.dim_buff && !found; ii++)
				    {
				      if(IVECVEC(table,n,hkcol.BiasPx,jj) == global.biashisto[ii].level)
					{
					  global.biashisto[ii].counts++;
					  found=1;
					}
				    }
				  if(!found)
				    {
				      global.dim_buff++;
				      global.biashisto=(BiasPix_t *)realloc(global.biashisto, global.dim_buff*sizeof(BiasPix_t));
				      global.biashisto[global.dim_buff-1].level=IVECVEC(table,n,hkcol.BiasPx,jj);
				      global.biashisto[global.dim_buff-1].counts=1;
				      if (global.biashisto[global.dim_buff-1].level > global.bias_hi)
					global.bias_hi=global.biashisto[global.dim_buff-1].level;
				      if (global.biashisto[global.dim_buff-1].level < global.bias_lo)
					global.bias_lo=global.biashisto[global.dim_buff-1].level;
				    }
				  
				}
			    }
			}
		    }
		}
		
	      else 
		{
		  search_end=1;
		  /*headas_chat(MUTE, "ho preso i valori da %u a %u\n", ccdframe_start, ccdframe_stop);*/
		  break;
		}

	    }  
	  
	  FromRow += ReadRows;
	  ReadRows = BINTAB_ROWS;
	}/* while */ 
      
      ReleaseBintable(&head, &table);  
      
      
      
    }/* End if(global.lr) */
  else
    {
      /* Open readonly input ev file */
      if ((unit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
	  goto FillBiasBuff_end;
	}
      
      /* Move in frame extension in input ev file */
      if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_EVT, 0, &status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
	  
	  if( CloseFitsFile(unit))
	    {
	      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	    }
      
	  goto FillBiasBuff_end;
      
	}
  
      head=RetrieveFitsHeader(unit);
      GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
      nCols=table.nColumns;      


      if(!table.MaxRows)
	{
	  headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
	  global.warning=1;
	  return OK;
	}


      /* Get needed columns number from name */
 


     
      /*  RAWPHA */
      if(global.oldtlm)
	{
	  if ((evcol.PHA=ColNameMatch(CLNM_RAWPHA, &table)) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWPHA);
	      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	      goto FillBiasBuff_end;
	    }
	}
      else
	{
	  if ((evcol.PHA=ColNameMatch(CLNM_PHA, &table)) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHA);
	      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	      goto FillBiasBuff_end;
	    }
	}


      if ((evcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto FillBiasBuff_end;
	}   
      
      if ((evcol.OFFSET=ColNameMatch(CLNM_OFFSET, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto FillBiasBuff_end;
	}   

      if ((evcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
	  goto FillBiasBuff_end;
	}   

      EndBintableHeader(&head, &table);
 
      FromRow = 1;
      ReadRows=table.nBlockRows;
      
      search_end=0;
      search=1;
      while((ReadBintable(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0) && !search_end )
	{
	  for(n=0; n<ReadRows ; ++n)
	    {
	      if(first)
		ccdframe_old=0;
	      else
		ccdframe_old=ccdframe_tmp;

	      ccdframe_tmp=VVEC(table, n, evcol.CCDFrame);
	      curr_time=DVEC(table,n,evcol.TIME_RO);


	      if (search && global.par.nframe > 0 )
		{
		  if((ccdframe_tmp == ccdframe_start) && (curr_time >= evtime-sens && curr_time <= evtime+sens))
		    search=0;
		  else
		    continue;
		}


	      if(global.par.nframe <= 0)
		allframe=1;
	      else 
		allframe=0;

	      if ((ccdframe_start <= ccdframe_stop) && 
		  ((ccdframe_tmp <= ccdframe_stop) && (ccdframe_tmp >= ccdframe_start)))
		incluseframe=1;
	      else
		incluseframe=0;

	      if ((ccdframe_start > ccdframe_stop) && 
		  ((ccdframe_tmp <= CCDFRAME_MAX ) || ((ccdframe_tmp >= CCDFRAME_MIN) && (ccdframe_tmp <= ccdframe_stop ))))
		offres=1;
	      else
		offres=0;

	      if(ccdframe_tmp >= ccdframe_old || first)
		{
		  first=0;
		  backframe=1;
		}
	      else
		backframe=0;


	      /*printf("allframe == %u incluseframe == %u offres == %u backframe == %u\n", allframe, incluseframe, offres, backframe);*/	

	      if(allframe || ((incluseframe ||  offres ) && backframe))
		{
		  *stoptime=curr_time;
		  
		  
		  if(JVEC(table, n, evcol.OFFSET) < PU_BAD_PIXS)
		    {
		      continue;
		    }


		  if(JVEC(table, n, evcol.PHA) < global.par.biasth && JVEC(table, n, evcol.PHA) >= 0)
		    {
		      global.tot_counts++;
		      
		      if(global.dim_buff == 0)
			{
			  global.dim_buff++;
			  global.biashisto=(BiasPix_t *)calloc(global.dim_buff, sizeof(BiasPix_t));
			  global.biashisto[global.dim_buff-1].level=JVEC(table,n,evcol.PHA);
			  global.biashisto[global.dim_buff-1].counts=1;
			  global.bias_hi=global.bias_lo=global.biashisto[global.dim_buff-1].level;
			  
			}
		      else
			{
			  found=0;
			  for (ii=0; ii < global.dim_buff && !found; ii++)
			    {
			      if(JVEC(table,n,evcol.PHA) == global.biashisto[ii].level)
				{
				  global.biashisto[ii].counts++;
				  found=1;
				}
			    }
			  if(!found)
			    {
			      global.dim_buff++;
			      global.biashisto=(BiasPix_t *)realloc(global.biashisto, global.dim_buff*sizeof(BiasPix_t));
			      global.biashisto[global.dim_buff-1].level=JVEC(table,n,evcol.PHA);
			      global.biashisto[global.dim_buff-1].counts=1;
			      if (global.biashisto[global.dim_buff-1].level > global.bias_hi)
				global.bias_hi=global.biashisto[global.dim_buff-1].level;
			      if (global.biashisto[global.dim_buff-1].level < global.bias_lo)
				global.bias_lo=global.biashisto[global.dim_buff-1].level;
			    }
			}
		    }
		}
	      else
		{
		  search_end=1;
		  /*headas_chat(MUTE, "ho preso i valori da %u a %u\n", ccdframe_start, ccdframe_stop);*/
		  break;
		}
	    }
	  
	    FromRow += ReadRows;
	    ReadRows = BINTAB_ROWS;
	}/* while */ 
      
      ReleaseBintable(&head, &table);  
      
    }

  
  headas_chat(CHATTY, "%s: Info: bias max value = %d - bias min value=%d\n", global.taskname,global.bias_hi, global.bias_lo);
  return OK;

 FillBiasBuff_end:
  if (head.first)
     ReleaseBintable(&head, &table);
  
  return NOT_OK;

}/* FillBiasBuff */
int xrtpdcorr_checkinput(void)
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

  return OK;
 check_end:
  return NOT_OK;
}

/* WriteBiasExtension */
int WriteBiasExtension(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int nrows, BiasFormat_t *bias)
{
  int           n=0;
  char          date[25];
  Bintable_t	table; 
  FitsHeader_t	newhead;
  char          taskname[FLEN_FILENAME];

  get_toolnamev(taskname);     

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(0, &table);
      
  /* Add extension name */
  AddCard(&newhead, KWNM_EXTNAME, S,KWVL_EXTNAME_BIAS, CARD_COMM_EXTNAME);
  
  /* Rows Number */
  table.nBlockRows = nrows;

  /* Add columns */
  AddColumn(&newhead, &table,CLNM_CCDFrame,CARD_COMM_CCDFrame, "1J", TZERO,2147483648.0,"");
  AddColumn(&newhead, &table,CLNM_BIAS,"", "1D",TNONE);
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE);
 
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);


  for (n = 0; n < nrows; n++)
    {
      ((VTYPE *)(table).cols[0])[n] = (unsigned )bias[n].ccdframe;
      DVEC(table,n,1) = bias[n].bias;  /* BIAS */
    }

  FinishBintableHeader(outunit, &newhead, &table);    
  WriteFastBintable(outunit, &table, nrows, TRUE);  
  
  /* Write bintable in file */
  
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);
  
  if(CopyBiasKeywords(inunit,outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write standard keywords\n", taskname);
      headas_chat(NORMAL,"%s: Error: in %s extension.\n", taskname, KWVL_EXTNAME_BIAS);
    }

  return OK;
  

}/* WriteBiasExt */
/* 
 *
 * CopyBiasKeywords
 *    Routine to copy standard keywords from input to bias output extension
 *
 */
int CopyBiasKeywords(FitsFileUnit_t inunit,FitsFileUnit_t outunit)
{
  int                status=OK, val_int=0;
  double             val=0.;
  char		     keystr[90];
  char               taskname[FLEN_FILENAME];
 
  get_toolnamev(taskname);
  if(fits_movabs_hdu(inunit, 2, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU.\n", taskname);
      goto copy_end;
    }
  
  if(fits_update_key(outunit, TSTRING,KWNM_TELESCOP ,KWVL_TELESCOP ,  CARD_COMM_TELESCOP , &status))
    goto copy_end;
  
  if(fits_update_key(outunit, TSTRING,KWNM_INSTRUME ,KWVL_INSTRUME ,  CARD_COMM_INSTRUME , &status))
    goto copy_end;

  if(fits_read_key(inunit, TSTRING, KWNM_TIMESYS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMESYS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMESYS, keystr, CARD_COMM_TIMESYS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMESYS);
      goto copy_end;
    }
  
  if(!fits_read_key(inunit, TDOUBLE, KWNM_MJDREFF, &val, NULL, &status))
    {
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREFF, &val, CARD_COMM_MJDREFF, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFF);
	  goto copy_end;
	}
      if(fits_read_key(inunit, TDOUBLE, KWNM_MJDREFI, &val, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFI);
	  goto copy_end;
	}
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREFI, &val, CARD_COMM_MJDREFI, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREFI);
	  goto copy_end;
	}
    }
  else
    {
      status=0;
      if(fits_read_key(inunit, TDOUBLE, KWNM_MJDREF, &val, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREF);
	  goto copy_end;
	}
      if(fits_update_key(outunit, TDOUBLE, KWNM_MJDREF, &val, CARD_COMM_MJDREF, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_MJDREF);
	  goto copy_end;
	}
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TIMEREF, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMEREF);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMEREF, keystr, CARD_COMM_TIMEREF, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMEREF);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TASSIGN, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TASSIGN);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TASSIGN, keystr, CARD_COMM_TASSIGN, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TASSIGN);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TIMEUNIT, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIMEUNIT);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TIMEUNIT, keystr, CARD_COMM_TIMEUNIT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIMEUNIT);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_DATEOBS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DATEOBS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_DATEOBS, keystr, CARD_COMM_DATEOBS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DATEOBS);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_DATEEND, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DATEEND);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_DATEEND, keystr, CARD_COMM_DATEEND, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DATEEND);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TLOGICAL, KWNM_CLOCKAPP, &val_int, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_CLOCKAPP);
      status=0;
    }
  else if(fits_update_key(outunit, TLOGICAL, KWNM_CLOCKAPP, &val_int, CARD_COMM_CLOCKAPP, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_CLOCKAPP);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_OBS_ID, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_OBS_ID);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_OBS_ID, keystr, CARD_COMM_OBS_ID, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_OBS_ID);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TARG_ID, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TARG_ID);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TARG_ID, keystr, CARD_COMM_TARG_ID, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TARG_ID);
      goto copy_end;
    }
 
  if(fits_read_key(inunit, TSTRING, KWNM_SEG_NUM, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_SEG_NUM);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_SEG_NUM, keystr, CARD_COMM_SEG_NUM, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_SEG_NUM);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_EQUINOX, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_EQUINOX);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_EQUINOX, &val, CARD_COMM_EQUINOX, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_EQUINOX);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_RADECSYS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_RADECSYS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_RADECSYS, keystr, CARD_COMM_RADECSYS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_RADECSYS);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_ORIGIN, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_ORIGIN);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_ORIGIN, keystr, CARD_COMM_ORIGIN, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_ORIGIN);
      goto copy_end;
    }
  if(fits_read_key(inunit, TSTRING, KWNM_CREATOR, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_CREATOR);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_CREATOR, keystr, CARD_COMM_CREATOR, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_CREATOR);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TSTRING, KWNM_TLM2FITS, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TLM2FITS);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_TLM2FITS, keystr, CARD_COMM_TLM2FITS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TLM2FITS);
      goto copy_end;
    }

  if(fits_read_key(inunit, TSTRING, KWNM_OBJECT, keystr, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_OBJECT);
      status=0;
    }
  else if(fits_update_key(outunit, TSTRING, KWNM_OBJECT, keystr, CARD_COMM_OBJECT, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_OBJECT);
      goto copy_end;
    }
  
  if(fits_read_key(inunit, TDOUBLE, KWNM_RA_OBJ, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_RA_OBJ);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_RA_OBJ, &val, CARD_COMM_RA_OBJ, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_RA_OBJ);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_DEC_OBJ, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_DEC_OBJ);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_DEC_OBJ, &val, CARD_COMM_DEC_OBJ, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_DEC_OBJ);
      goto copy_end;
    }
  if(fits_read_key(inunit, TDOUBLE, KWNM_TIERRELA, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIERRELA);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_TIERRELA, &val, CARD_COMM_TIERRELA, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIERRELA);
      goto copy_end;
    }

  if(fits_read_key(inunit, TDOUBLE, KWNM_TIERABSO, &val, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Warning: Unable to read %s keyword.\n", taskname, KWNM_TIERABSO);
      status=0;
    }
  else if(fits_update_key(outunit, TDOUBLE, KWNM_TIERABSO, &val, CARD_COMM_TIERABSO, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to write %s keyword.\n", taskname, KWNM_TIERABSO);
      goto copy_end;
    }


 return OK;

 copy_end:
 return NOT_OK;
}/* CopyBiasKeywords */

/*
 *
 *  ReadInputFileKeywords
 *
 */


BOOL ReadInputFileKeywords(void)
{ 
  
  int            status=OK;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t unit=NULL;

  global.lr=FALSE;
  global.pu=FALSE;
  global.oldtlm=FALSE;  
  global.biasonbd=FALSE;  


  /* Open readonly input event file */
  if ((unit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {  
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: Unable to open\n", global.taskname); 
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: '%s' FITS file.\n", global.taskname,global.par.infile); 
      goto readkeys_end;
    } 

  /* Move in events extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status)) 
    {  
      headas_chat(CHATTY,"%s: Error: ReadInputFileKeywords: Unable to find  '%s' extension\n", global.taskname, KWVL_EXTNAME_EVT); 
      headas_chat(CHATTY,"%s: Error: ReadInputFileKeywords: in '%s' file.\n",global.taskname, global.par.infile);
      goto readkeys_end; 
    }  
	
  head=RetrieveFitsHeader(unit);    


  /* Retrieve TSTART from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    global.tstart=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: in %s file.\n", global.taskname, global.par.infile);
      goto readkeys_end;
    }

  /* Retrieve TSTOP from input event file */
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    global.tstop=card->u.DVal;
  else
    {
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: in %s file.\n", global.taskname, global.par.infile);
      goto readkeys_end;
    }

  /* Retrieve DATEOBS from input event file */
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
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
	      goto readkeys_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
      goto readkeys_end;
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

  if(!ExistsKeyWord(&head, KWNM_DATAMODE, &card))
    {
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: %s keyword not found\n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: in %s file.\n", global.taskname, global.par.infile);
      goto readkeys_end;
    }
      
  /* Check readout mode of input events file 
     Only PILEDUP and LOWRATE modes are allowed */
  if(!strcmp(card->u.SVal, KWVL_DATAMODE_PDPU))
    global.pu=1;
  else if (!strcmp(card->u.SVal, KWVL_DATAMODE_PD))
    global.lr=1;
  else
    {
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: This task does not process the readout mode\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: of the %s file.\n", global.taskname, global.par.infile);
      headas_chat(CHATTY,"%s: Error: ReadInputFileKeywords: Valid readout mode are: %s and %s\n", global.taskname, KWVL_DATAMODE_PDPU,KWVL_DATAMODE_PD );
      if( CloseFitsFile(unit))
	{
	  headas_chat(CHATTY, "%s: Error: ReadInputFileKeywords: Unable to close\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: ReadInputFileKeywords: '%s' file.\n ", global.taskname, global.par.infile);
	}
      goto readkeys_end;
      
    }

  if(!ExistsKeyWord(&head, KWNM_BIASONBD, &card))
    {
      headas_chat(CHATTY, "%s: Info: ReadInputFileKeywords: %s keyword not found.\n", global.taskname, KWNM_BIASONBD);
      global.oldtlm=TRUE;
      
    }
  else if(card->u.LVal)
    {
      headas_chat(NORMAL,"%s: Info: ReadInputFileKeywords: The bias correction is already applied on-board.\n", global.taskname);
      global.biasonbd=TRUE;
      if(global.pu)
	{
	  return OK;
	}
      if(global.lr)
	  headas_chat(NORMAL,"%s: Info: ReadInputFileKeywords: so the bias will be re-corrected.\n", global.taskname);

    }      

  /* Check if PHA is already computed */
  if(ExistsKeyWord(&head, KWNM_XRTPHACO, &card))
    {
      if(card->u.LVal)
	{
	  if(global.oldtlm)
	    {
	      headas_chat(CHATTY, "%s: Warning: ReadInputFileKeywords: %s is already corrected,\n", global.taskname, CLNM_PHA);
	      headas_chat(CHATTY, "%s: Warning: ReadInputFileKeywords: the %s column will be overwritten.\n", global.taskname, CLNM_PHA);
	    }
	  else
	    {
	      
	      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: %s is already corrected on ground,\n", global.taskname, CLNM_PHA);
	      headas_chat(NORMAL, "%s: Error: ReadInputFileKeywords: nothing to be done.\n", global.taskname);
	      goto readkeys_end;
	    }
	}
    }


  if( CloseFitsFile(unit)) 
    {  
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: Unable to close\n", global.taskname); 
      headas_chat(NORMAL,"%s: Error: ReadInputFileKeywords: '%s' FITS file.\n", global.taskname,global.par.infile);
      goto readkeys_end;  
    }   


  return OK;

 readkeys_end:

  CloseFitsFile(unit); 
  
  return NOT_OK; 
}/* ReadInputFileKeywords */
/*
 *
 *  ComputeBiasDiff
 *
 *  DESCRIPTION:
 *    Routine to build bias histogram with PHA value
 *
 *
 */
int ComputeBiasDiff(void)
{  
  int                status=OK, mul=0, jj=0, xrtmode=0, tmpmode=0;
  int                iii=0, *median, level=0, mod=0, ind=0;
  double             diff_db=0.0;
  unsigned           FromRow, ReadRows, n, nCols;
  HKCol_t            hkcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  global.totdiff=0;  

  if(global.lr)
    xrtmode=XRTMODE_LR;

  if ((unit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }
      
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      
      goto ComputeBiasDiff_end;
	  
    }

  head=RetrieveFitsHeader(unit);
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {

      /*printf(">>>>> tstart=%5.5f -- hkstart=%5.5f\n", global.tstart,(card->u.DVal+TIMEDELT));*/
      if(global.tstart < (card->u.DVal - TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n",  global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the \n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ComputeBiasDiff_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }
  
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    {

      /*printf(">>>>> tstop=%5.5f -- hkstop=%5.5f\n", global.tstop,card->u.DVal);*/
      if(global.tstop > (card->u.DVal + TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n",global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ComputeBiasDiff_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }



  /* Get needed columns number from name */
      
  /*  XRTMode */
  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }

  if ((hkcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }

  if ((hkcol.PDBIASLVL=ColNameMatch(CLNM_PDBIASLVL, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PDBIASLVL);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }
  
  /* LRBiasPx */
  if ((hkcol.BiasPx=ColNameMatch(CLNM_LRBiasPx, &table)) == -1)
    {
      if ((hkcol.BiasPx=ColNameMatch(CLNM_BiasPx, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' and '%s' columns do not exist\n", global.taskname, CLNM_LRBiasPx, CLNM_BiasPx);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto ComputeBiasDiff_end;
	}
	  
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: '%s' column exists\n", global.taskname, CLNM_LRBiasPx);
      headas_chat(CHATTY, "%s: Info: '%s' file is in old format. \n", global.taskname, global.par.hdfile);
    }


  /* LRHPixCt */
  if ((hkcol.HPIXCT=ColNameMatch(CLNM_LRHPIXCT, &table)) == -1)
    {
      if ((hkcol.HPIXCT=ColNameMatch(CLNM_HPIXCT, &table)) == -1)
	{
	  headas_chat(NORMAL, "%s: Error: '%s' and '%s' columns do not exist\n", global.taskname, CLNM_LRHPIXCT, CLNM_HPIXCT);
	  headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
	  goto ComputeBiasDiff_end;
	}
      
    }
  else
    {
      headas_chat(CHATTY, "%s: Info: '%s' column exists\n", global.taskname,CLNM_LRHPIXCT );
      headas_chat(CHATTY, "%s: Info: '%s' file is in old format. \n", global.taskname, global.par.hdfile);
    }
  
  /* TIME */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }
  
  
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end;
    }
  
  EndBintableHeader(&head, &table);
  
  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  /* Read only the following columns */

  while(ReadBintableWithNULL(unit, &table, nCols, NULL,FromRow,&ReadRows) == 0)
    {
      for(n=0; n<ReadRows ; ++n)
	{

	  tmpmode=BVEC(table, n, hkcol.XRTMode);
	  if(xrtmode==tmpmode)
	    {
	      if(!global.totdiff)
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)malloc(sizeof(BiasDiffFormat_t));
		}
	      else
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)realloc(global.diff, (global.totdiff*sizeof(BiasDiffFormat_t)));
		  
		}

	      mul=IVEC(table,n,hkcol.HPIXCT);
	      if(!mul)
		{
		  headas_chat(NORMAL, "%s: Warning: the %s column contains %d\n", global.taskname, CLNM_HPIXCT, mul);
		  headas_chat(NORMAL, "%s: Warning: so the %s column is empty.\n", global.taskname, CLNM_BiasPx);
		  goto  ComputeBiasDiff_end;
		  
		}
	      if(mul > table.Multiplicity[hkcol.BiasPx])
		{
		  headas_chat(NORMAL, "%s: Error: problem with multiplicy of the %s column.\n", global.taskname, CLNM_BiasPx);
		  goto  ComputeBiasDiff_end;
		}
	      
	      global.diff[global.totdiff-1].time = (VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
	      global.diff[global.totdiff-1].ccdframe =VVEC(table, n, hkcol.CCDFrame) ;
	      
	      if(mul)
		median=(int *)calloc(mul, sizeof(int));
	      
	      for (jj=0; jj< mul; jj++)
		{
		  level=IVECVEC(table,n,hkcol.BiasPx,jj);
		  
		  if(jj==0)
		    median[jj]=level;
		  else if(level >= median[jj-1])
		    median[jj]=level;
		  else
		    {
		      for(iii=jj; iii >= 0 ; iii--)
			{
			  if(!iii)
			    median[iii]=level;
			  
			  else if (level < median[iii-1])
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
	      /*jjjjjjj++;
	      printf("%d\n",jjjjjjj);
	      for (jj=0; jj< mul; jj++)
	      {
	      printf("%d --- %d\n", jj,median[jj]);
	      
	      }
	      */
	      mod=mul%2;
	      if(mod)
		{
		  headas_chat(CHATTY, "%s: Info: Frame %4d - median calculated = %f and bias subtracted on board = %f\n",global.taskname,global.diff[global.totdiff-1].ccdframe,  median[(int)(mul/2)], DVEC(table, n, hkcol.PDBIASLVL));
		  diff_db=(double)(median[(int)(mul/2)] - DVEC(table, n, hkcol.PDBIASLVL));
		}
	      else
		{
		  ind=(int)(mul/2.0);
		  if(!ind)/* If ind == 0 */
		    {
		      headas_chat(CHATTY, "%s: Info: Frame %4d - median calculated = %f and bias subtracted on board = %f\n",global.taskname,global.diff[global.totdiff-1].ccdframe,  median[ind], DVEC(table, n, hkcol.PDBIASLVL));
		      diff_db=(double)(median[ind] - DVEC(table, n, hkcol.PDBIASLVL));
		    }
		  else
		    {
		      headas_chat(CHATTY, "%s: Info: Frame %4d - median calculated = %f and bias subtracted on board 4 %f\n",global.taskname,global.diff[global.totdiff-1].ccdframe,((median[ind] + median[ind - 1])/2.), DVEC(table, n, hkcol.PDBIASLVL));
		      diff_db=(((median[ind] + median[ind - 1])/2.) - DVEC(table, n, hkcol.PDBIASLVL));
		      
		    }
		}
	      global.diff[global.totdiff-1].diff=diff_db;
	      if(mul)
		free(median);
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  

  ReleaseBintable(&head, &table);  
  if( CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto ComputeBiasDiff_end; 
    }
  



      
  return OK;

 ComputeBiasDiff_end:
  if (head.first)
     ReleaseBintable(&head, &table);
  
  return NOT_OK;

}/* ComputeBiasDiff */
/*
 *
 *      PhaReCorrect
 *
 *	DESCRIPTION:
 *             Routine to compute bias value and correct events PHA
 *     
 *     
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             FitsHeader_t   RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int ExistsKeyWord(FitsHeader_t, char *, char *);
 *             BOOL GetLVal(FitsHeader_t, char *);
 *             void GetBintableStructure (FitsHeader_t *header,	Bintable_t *table,
 *     					  const int MaxBlockRows, const unsigned nColumns,
 *  					  const unsigned ActCols[]);
 *             int  ColNameMatch(const char *ColName, const Bintable_t *table);
 *             int headas_chat(int ,char *, ...);
 *             void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			      const char *TypeComment, const char *tform, const unsigned present, ...);
 *             void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
 *                          const void *value, const char *comment);
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
 *        0.1.0: - BS 09/04/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int PhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
  int                pha=0, iii=0, difftmp=0, diff_int=0;
  BOOL               first=1;
  double             sens=0.0000001, evtime=0.0, diff_db=0.0;
  unsigned           ccdframe_old=0, ccdframe=0;
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  Ev2Col_t           indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  head=RetrieveFitsHeader(evunit);

  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  nCols=outtable.nColumns;
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }


  /* Get cols index from name */

  /* PHA */
  /* Add PHA column if needed */
  if((indxcol.PHA=ColNameMatch(CLNM_PHA, &outtable)) == -1)
    { 
      headas_chat(CHATTY, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_PHA);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile); 
      goto reco_end;
    }

  if ((indxcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &outtable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;
    }

  if((indxcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto reco_end;      
    }


  /*printf(">>>>>>prima\n");*/ 
  /* Add history */
  GetGMTDateTime(global.date);
  /*printf(">>>>>>dopo\n"); */
  if(global.hist)
    {
      /*printf(">>>>>>dentro\n");*/ 
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: re-calcolated PHA.", global.taskname, global.swxrtdas_v,global.date );
      /*printf(">>>>>>dentro\n"); */
      AddHistory(&head, global.strhist);
    }
     
  /*printf(">>>>>>\n");*/  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);
  
  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;
  iii=0;

  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{
	   
	  evtime=DVEC(outtable,n,indxcol.TIME_RO);
	  ccdframe=VVEC(outtable, n, indxcol.CCDFrame);
	  
	  if(first || ccdframe_old != ccdframe)
	    {
	      first=0;
	      ccdframe_old=ccdframe;


	      
	      if(!global.biasrows)
		{
		  global.biasrows++;
		  global.outbias = (BiasFormat_t *)malloc(sizeof(BiasFormat_t));
		}
	      else
		{
		  global.biasrows++;
		  global.outbias = (BiasFormat_t *)realloc(global.outbias, (global.biasrows*sizeof(BiasFormat_t)));
		  
		}

	      global.outbias[global.biasrows-1].ccdframe = ccdframe;

	     
	      for (; iii < global.totdiff; iii++)
		{

		  /*printf("ccdframe=%d - hkccdframe=%d - time=%5.5f - hktime=%5.5f\n", ccdframe,global.diff[iii].ccdframe,evtime,global.diff[iii].time);*/
		  if(ccdframe == global.diff[iii].ccdframe &&  
		     (global.diff[iii].time >= (evtime-sens) && global.diff[iii].time <= (evtime+sens)))
		    {
		
		      diff_db=global.diff[iii].diff;
		      diff_int=floor(diff_db + 0.5);

		      difftmp=abs(diff_int);
		      
		      if(difftmp > global.par.biasdiff)
			{
			  difftmp=diff_int;
			  global.outbias[global.biasrows-1].bias = diff_db;
			}
		      else
			{
			  difftmp=0;
			  global.outbias[global.biasrows-1].bias = 0.0;
			}
		      headas_chat(CHATTY, "%s: Info: Frame %4d - Using %4.6f value to correct %s value.\n", global.taskname, ccdframe,global.outbias[global.biasrows-1].bias ,CLNM_PHA);
		      goto diff_found;
		    }
		}

	      
	      headas_chat(NORMAL, "%s: Error: Unable to get value to correct %s column for %d ccdframe.\n", global.taskname, CLNM_PHA, ccdframe);
	      goto reco_end;
	    }
	 
	diff_found: 
	     	  
	  /* Get raw pha value */
	  pha=JVEC(outtable, n, indxcol.PHA);
	  
	  /* Put corrected pha value into PHA column */
	  if(pha != SAT_VAL)
	    {
	      
	      pha=(pha - difftmp);
	      JVEC(outtable, n, indxcol.PHA)=pha;
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
  
  if(global.totdiff)
    free(global.diff);


  return OK;

 reco_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* PhaReCorrect */
/*
 *
 *  ComputePUMedian
 *
 *  DESCRIPTION:
 *    Routine to calculate median 
 *
 *
 */
int ComputePUMedian(void)
{  
  int                status=OK, ccdframe=0, ccdframe_old=0;
  int                iii=0, mod=0, ind=0, blvl_ind=0;
  int                buff_len=0, *buff, pha=0, totrows=0;
  BOOL               first=1, read_hk=1;
  double             evtime=0.0, evtime_old=0.0, sens=0.0000001, diff_db=0.0; 
  double             biaslvl=0.0;
  unsigned           FromRow, ReadRows, n, nCols, ActCols[4];
  Ev2Col_t           evcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  global.totdiff=0;  

  if(ReadHKForPU())
    {
      headas_chat(NORMAL,"%s: Error: Unable to read needed values\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: from '%s' file.\n", global.taskname, global.par.hdfile);
      goto ComputePUMedian_end;
    }


  if ((unit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
      goto ComputePUMedian_end;
    }
      
  /* Move in events extension */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
	}
      
      goto ComputePUMedian_end;
	  
    }

  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
   
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
      global.warning=1;
      return OK;
    }
  /* Get needed columns number from name */

  if ((evcol.PHA=ColNameMatch(CLNM_PHA, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PHA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputePUMedian_end;
    }

  if((evcol.TIME_RO=ColNameMatch(CLNM_TIME_RO, &table)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME_RO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputePUMedian_end;      
    }
      
  if ((evcol.CCDFrame=ColNameMatch(CLNM_CCDFrame, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CCDFrame);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.infile);
      goto ComputePUMedian_end;
    }
  
  EndBintableHeader(&head, &table);
  
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=3;
  ActCols[0]=evcol.PHA;
  ActCols[1]=evcol.TIME_RO;
  ActCols[2]=evcol.CCDFrame;
  buff_len=0;
  first=1;
  totrows=0;

  /* Read only the following columns */
  while(ReadBintable(unit, &table, nCols, ActCols,FromRow,&ReadRows) == 0)
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  totrows++;

	  if(first)
	    {
	      ccdframe_old=ccdframe=VVEC(table, n, evcol.CCDFrame);
	      evtime_old=evtime=DVEC(table,n,evcol.TIME_RO);
	      first=0;
	    }
	  else
	    {
	      evtime_old=evtime;
	      ccdframe_old=ccdframe;
	      ccdframe=VVEC(table, n, evcol.CCDFrame);
	      evtime=DVEC(table,n,evcol.TIME_RO);
	    }

	  
	  /*printf("HKROWS==%d\n", global.biaslvlrow);*/

	  if(read_hk)
	    {
	      for(; blvl_ind < global.biaslvlrow && read_hk ; blvl_ind++)
		{
		  /*printf("IND==%d\n", blvl_ind);
		    printf("HKTIME==%f TIME==%f\n",global.hkbias[blvl_ind].hktime,evtime );*/
		  if(global.hkbias[blvl_ind].hktime >= (evtime-sens) && global.hkbias[blvl_ind].hktime <= (evtime+sens))
		    
		    {
		      read_hk=0;
		      biaslvl=global.hkbias[blvl_ind].pdbiaslvl;
		    }
		}
	      if(read_hk)
		{	      
		  headas_chat(NORMAL, "%s: Error: Row containg the %d frame information\n", global.taskname, ccdframe);
		  headas_chat(NORMAL, "%s: Error: is not been found in %s file.\n", global.taskname, global.par.hdfile);
		  goto ComputePUMedian_end;
		}
	    }


	  if( (evtime >= evtime_old - sens && evtime <= evtime_old + sens))
	    {
	      
	      pha=(JVEC(table, n, evcol.PHA) + biaslvl);
	      if(pha <= global.par.biasth)
		{

		  /*printf("pha...= %d biasth=%d\n",pha, global.par.biasth);*/
		    /*put the value into the buffer */
		  if(buff_len)
		    {
		      buff_len++;
		      buff=(int *)realloc(buff, (buff_len*sizeof(int)));
		      
		    }
		  else
		    {
		      buff_len++;
		      buff=(int *)malloc(sizeof(int));	      
		    }
		  
		  if(buff_len==1)
		    buff[0]=pha;
		  else if(pha >= buff[buff_len - 2])
		    buff[buff_len - 1]=pha;
		  else
		    {
		      for(iii=(buff_len - 1); iii >= 0 ; iii--)
			{
			  if(!iii)
			    buff[iii]=pha;
			  else if ( pha < buff[iii-1])
			    {
			      buff[iii]=buff[iii-1];
			    }
			  else
			    {
			      buff[iii]=pha;
			      iii=0;
			    }
			}
		    }/* build histo */ 
		}/* Check on pha */
	    }/* Check on evtime */
	  else 
	    {
	      
	      read_hk=1;
	      if(!global.totdiff)
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)malloc(sizeof(BiasDiffFormat_t));
		}
	      else
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)realloc(global.diff, (global.totdiff*sizeof(BiasDiffFormat_t)));
		  
		}

	      global.diff[global.totdiff-1].time = evtime_old;
	      global.diff[global.totdiff-1].ccdframe=ccdframe_old;
	      

	      /*for(iii=0; iii < buff_len; iii++)
		printf("PHA == %d\n", buff[iii]);
	      */
	      if(buff_len)
		{
		  
		  mod=buff_len%2;
		  if(mod)
		    {
		      diff_db=(double)(buff[(int)(buff_len/2)]);
		    }
		  else
		    {
		      ind=(int)(buff_len/2.0);
		      if(!ind)/* If ind == 0 */
			diff_db=(double)buff[ind];
		      else
			{
			  diff_db=(buff[ind] + buff[ind - 1])/2.;
			  
			}
		    }
		  global.diff[global.totdiff-1].diff=diff_db - biaslvl;
		  if(buff_len)
		    {
		      free(buff);
		      buff_len=0;
		    }
		  headas_chat(CHATTY, "%s: Info: Frame %4d - median calculated = %f and bias subtracted on board = %f\n",global.taskname,global.diff[global.totdiff-1].ccdframe,  diff_db, biaslvl);
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Warning: Unable to calculate median for %d ccdframe\n",global.taskname, ccdframe_old);
		  headas_chat(NORMAL, "%s: Warning: There are not events below the bias threshold\n",global.taskname);
		  global.diff[global.totdiff-1].diff=0.0;
		}

	      if(pha <= global.par.biasth)
		{
		  if(buff_len)
		    {
		      buff_len=0;
		      free(buff);
		    }
		  buff_len++;
		  buff=(int *)malloc(sizeof(int));
		  evtime_old=evtime;
		  buff[0]=pha;
		}
	    }
	  
	  if(totrows==table.MaxRows)
	    {
	      if(!global.totdiff)
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)malloc(sizeof(BiasDiffFormat_t));
		}
	      else
		{
		  global.totdiff++;
		  global.diff = (BiasDiffFormat_t *)realloc(global.diff, (global.totdiff*sizeof(BiasDiffFormat_t)));
		  
		}

	      global.diff[global.totdiff-1].time = evtime_old;
	      global.diff[global.totdiff-1].ccdframe=ccdframe_old;
	      

	      /*for(iii=0; iii < buff_len; iii++)
		printf("PHA == %d\n", buff[iii]);
	      */      

	      if(buff_len)
		{
		  mod=buff_len%2;
		  
		  if(mod)
		    {
		      diff_db=(double)(buff[(int)(buff_len/2)] );
		    }
		  else
		    {
		      ind=(int)(buff_len/2.0);
		      /*printf(">>>ind== %d\n",ind);*/ 
		      if(!ind)/* If ind == 0 */
			diff_db=(double)buff[ind];
		      else
			{
			  diff_db=(buff[ind] + buff[ind - 1])/2.;
			  
			}
		    }
		  /*printf("MEDIAN==%f\n", diff_db);*/
		  global.diff[global.totdiff-1].diff=diff_db - biaslvl;
		  headas_chat(CHATTY, "%s: Info: Frame %4d - median calculated = %f and bias subtracted on board = %f\n",global.taskname,global.diff[global.totdiff-1].ccdframe,  diff_db, biaslvl);
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Warning: Unable to calculate median for %d ccdframe\n",global.taskname, ccdframe_old);
		  headas_chat(NORMAL, "%s: Warning: There are not events below the bias threshold\n",global.taskname);
		  global.diff[global.totdiff-1].diff=0.0;
		}
	      if(buff_len)
		{
		  free(buff);
		  buff_len=0;
		}
	    }

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  

  ReleaseBintable(&head, &table);  

  /*for(iii=0; iii < global.totdiff ; iii++)
    printf("CCDFRAME==%d MEDIAN==%f TIME==%f\n", global.diff[iii].ccdframe, global.diff[iii].diff, global.diff[iii].time);
  */

  if( CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto ComputePUMedian_end; 
    }
  

  if(buff_len)
    free(buff);
  if(global.biaslvlrow)
    free(global.hkbias);

      
  return OK;

 ComputePUMedian_end:
  if (head.first)
     ReleaseBintable(&head, &table);

  if(buff_len)
    free(buff);
  if(global.biaslvlrow)
    free(global.hkbias);
  return NOT_OK;

}/* ComputePUMedian */

/*
 *
 *  ReadHKForPU
 *
 *  DESCRIPTION:
 *    Routine to build bias histogram with PHA value
 *
 *
 */
int ReadHKForPU(void)
{  
  int                status=OK, xrtmode=0, tmpmode=0;
  unsigned           ActCols[4], iiiiiiii=0;
  unsigned           FromRow, ReadRows, n, nCols;
  HKCol_t            hkcol;
  Bintable_t	     table;
  FitsCard_t         *card;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  
  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  xrtmode=XRTMODE_PU;
  
  if ((unit=OpenReadFitsFile(global.par.hdfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  /* Move in frame extension in input hk file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_FRM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_FRM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.hdfile);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
	}
      
      goto ReadHKForPU_end;
	  
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.hdfile);
  head=RetrieveFitsHeader(unit);
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {
      
      /*printf(">>>>> tstart=%5.5f -- hkstart=%5.5f\n", global.tstart,(card->u.DVal+TIMEDELT));*/
      if(global.tstart < (card->u.DVal - TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHKForPU_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    {
      
      /*printf(">>>>> tstop=%5.5f -- hkstop=%5.5f\n", global.tstop,card->u.DVal);*/
      if(global.tstop > (card->u.DVal + TIMEDELT))
	{
	  headas_chat(NORMAL, "%s: Error: The times in the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.infile);
	  headas_chat(NORMAL, "%s: Error: file are not included in the range time of the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.hdfile);
	  goto ReadHKForPU_end;
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  /* Get needed columns number from name */
  
  /*  XRTMode */
  if ((hkcol.XRTMode=ColNameMatch(CLNM_XRTMode, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_XRTMode);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  
  if ((hkcol.PDBIASLVL=ColNameMatch(CLNM_PDBIASLVL, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PDBIASLVL);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  
  /* TIME */
  if ((hkcol.FSTS=ColNameMatch(CLNM_FSTS, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  
  if ((hkcol.FSTSub=ColNameMatch(CLNM_FSTSub, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_FSTSub);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end;
    }
  
  EndBintableHeader(&head, &table);
  
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=4;
  ActCols[0]=hkcol.XRTMode;
  ActCols[1]=hkcol.PDBIASLVL;
  ActCols[2]=hkcol.FSTS;
  ActCols[3]=hkcol.FSTSub;

  /* Read only the following columns */
  
  while(ReadBintableWithNULL(unit, &table, nCols, ActCols,FromRow,&ReadRows) == 0)
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  iiiiiiii++;
	  tmpmode=BVEC(table, n, hkcol.XRTMode);
	  /*printf("XRTMODE==%d -- HKMODE==%d\n", xrtmode, tmpmode);*/
	  if(xrtmode==tmpmode)
	    {
	      
	      if(!global.biaslvlrow)
		{
		  global.biaslvlrow++;
		  global.hkbias = (BiasSub_t *)malloc(sizeof(BiasSub_t));
		}
	      else
		{
		  global.biaslvlrow++;
		  global.hkbias = (BiasSub_t *)realloc(global.hkbias, (global.biaslvlrow*sizeof(BiasSub_t)));
		  
		}
	      
	      global.hkbias[global.biaslvlrow-1].hktime = (VVEC(table, n, hkcol.FSTS) + ((20.*UVEC(table, n, hkcol.FSTSub))/1000000.));
	      global.hkbias[global.biaslvlrow-1].pdbiaslvl=DVEC(table, n, hkcol.PDBIASLVL);
	      
	    }
	}
	
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  
  ReleaseBintable(&head, &table);  
  if( CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.hdfile);
      goto ReadHKForPU_end; 
    }
  
  /*printf("TOTROWS== %d HDROWS==%d\n", iiiiiiii,global.biaslvlrow);*/
  /*for (ii=0; ii<global.biaslvlrow; ii++)
    printf("HKTIME== %f PDBIASLVL==%f\n", global.hkbias[ii].hktime, global.hkbias[ii].pdbiaslvl);
  */
  return OK;
  
 ReadHKForPU_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;
  
}/* ReadHKForPU */
