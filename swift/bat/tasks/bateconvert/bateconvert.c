#include <string.h>
#include <math.h>
#include <stdio.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h"

#define BUFFERSIZE 524288

static char taskname[] = "bateconvert";
static char taskver[]  =   "6.3";

/*
HISTORY
-------
  Version 1.0 written by Hans Krimm, USRA/NASA-GSFC  13-Sep-2002
  HAK 17-Sep-2002 Modified code to search for the appropriate EXTNAME
      keyword to find the correct HDU.
  
  Version 2.0 completed by Hans Krimm USRA/NASA-GSFC  14-Nov-2002
      This version is completely updated from the original version.

  Version 2.1 completed by Hans Krimm USRA/NASA-GSFC  20-May-2003
      Added reading the residfile and pulserfile and applying the correction
      to the linear fitting due to the residuals between linear and quadratic
      fits.
  HAK 14-June-2003 Fixed bug in application of residual fit.  gain2 was
      typed as gain1.

  HAK 18-July-2003 Modified code so that if the "PI" column is not found, 
      it is created and then filled.  The TSCAL and TUNIT values for this
      column are hard coded at 0.1 and keV, respectively.
      
  HAK 18-July-2003 Changed definition of DPH-style arrays in 
      bateconvert_readgain_off so that they are malloc-ed pointer arrays.
      Required to avoid segmentation faults under OSF.

  HAK 22-July-2003 In order for the tool to run under MacOSX, it was 
      necessary to move the defintion of the gain_offset structure to
      global and change the definition of several more arrays in the 
      bateconvert_readgain_off subroutine so that they are malloc-ed
      pointer arrays.

  CM 01-June-2004 Sweep the code for HEASARC keyword name/comment
      compliance.  For HEASARC column-name compliance, this task now
      writes out *two* columns:
         PI - integer value, energy in tenths of keV (no scale factors)
         ENERGY - integer*TSCAL = float energy in keV (after scale factors)
      I also removed a string comparison from the inner loop for speed.

  CM 14-June-2004 Perform internal computations with floating point
      precision instead of integer, and only round at the end.  Add
      the scaled_energy parameter which, when YES, outputs the pure
      floating point energy instead of an integer-scaled version.

  CM 16-June-2004 Be sure to initialize the pi[] and energy[] arrays.
      Since they are local variables, they must be initialized to zero
      explicitly.

  HAK 7-Aug-2004 Added code to make sure that the ENERGY column is
     filled correctly if it already exists in the input file (needed in
     those cases where bateconvert is run multiple times)

  JRC 12-Dec-2004 Added correction of flight_gain and flight_offset to
     best current pulser voltage to energy conversion.

  JRC 19-Feb-2005 Added cubic correction.

  JRC 09-Feb-2005 
  * Implemented "constant DAC" instead of "constant ADU"
    residual calculation.
  * Corrected error in scaling in calculation of poff
  * Made flight_gaincorr and flight_offsetcorr floating point. This 
    should only make a small difference. 
  * Added headas_chat statements with filenames and other handy 
    debugging info. 
  * Changed comment on GAINMETH keyword for linear correction. It had 
    incorrectly used the word "ground", whereas no change to the flight 
    gain/offset occurs.

  JRC 20-Jul-2005 Changed limits on energy to -10 keV to +3276.8 keV

  CBM 28-Jul-2005 Changed version number to 4.1 to reflect 20-Jul-2005
                  modification.

  CBM 03-Apr-2007 
   * Added calls to fits_set_hdustruct() for safety against CFITSIO 
     taking a dump on the file structure.

  CBM 20-Apr-2007 
  * Version 4.3 BUG FIX: It turns out one cannot use
    fits_set_hdustruc() indiscriminately, especially because it resets
    the fits_set_tscale() settings made by the task.
    fits_set_hdustruc() is done more strategically now.

  CBM 24-Jun-2008

  * Version 5.0: fairly massive overhaul here, and in the library
    routine bat_read_calfiles(), for clean-up purposes and also to
    handle null values in the gain/offset file.  Now null gain/offset
    values are propagated to the output event file, where they appear
    as null energy/PI values.

 CBM 23 Jun 2009
  * Version 6.0:
        * Beginning of major overhaul, addition of calls to 
        * bat_read_cal_coeff() and bat_pha_to_keV()
        * but old code is currently still present
  * Version 6.1: remove old energy calculation code, leaving only the
    library routines, and also using the "geographic" method of access

*/

#define TOOLSUB bateconvert
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure for passing all PIL parameters to the main routine. */
typedef struct {
   char infile[PIL_PATH_MAX];
   char outfile[PIL_PATH_MAX];
   char calfile[PIL_PATH_MAX];
   char residfile[PIL_PATH_MAX];
   char pulserfile[PIL_PATH_MAX];
   char fltpulserfile[PIL_PATH_MAX];
   int residext, pulserext, fltpulserext;
   char calmode[10];
   int zeroit;
   int scaled_energy;
} BATECONVERT_PARAMS;   

/* Global array definitions */
static bat_ecal_data caldata;

/* Scale factor for energy -- this is set by the flight software */
double energy_scale_factor = 0.1;  /* keV per data number */

/* Function prototypes */
int bateconvert(void);
int bateconvert_getpar (BATECONVERT_PARAMS *params);
int bateconvert_printsummary (BATECONVERT_PARAMS *params,
			      long int *nevt,char *taskname,char *version);
int bateconvert_ecols(fitsfile *wrtptr, 
		      char *picolname, int *picol, 
		      int pi_tlmin, int pi_tlmax, int pi_tnull, 
		      char *ecolname,  int *ecol,
		      int scaled_energy,
		      int *status);


/*-------------------------------------------------------------*/
int bateconvert(void)
{
  int status,temp_status;
  int detcol,phacol,picol, ecol;
  long int i,bufsiz,ndone;
  long int nevt=0; /* Number of events in input file*/
  int j,gainapp=0;
  double tstart = 0;
  /* Input and output columns */
  int *detid = 0, *adu = 0, *pi = 0;
  double *energy = 0;
  int quadmode = 0;
  int cubicmode = 0;
  
  fitsfile *infptr = 0, *outfptr = 0, *pcfptr = 0, *wrtptr = 0;
  
  char creator[FLEN_CARD];
  char keyname[FLEN_CARD];
  
  int tnull_pi = -32768;
  int tnull_energy = -32768;
  int tlmin = 0, tlmax = 5000;  /* 0 - 500 keV */
  
  int calmode=0;
  
  /* at the time the residuals were calculated */
  /* Used to warn about missing gain/offset values */
  int bad_go;
  
  BATECONVERT_PARAMS params;
  
  headas_chat(5,"...begin task...\n");
  
  /* Register taskname and version. */
  set_toolname(taskname);
  set_toolversion(taskver);
  
  /* CREATOR keyword */
  sprintf(creator, "%s %s", taskname, taskver);
  
  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", taskname, taskver);
  headas_chat(2, "------------------------------------------\n");
  
  
  /*  get input parameters */
  status = bateconvert_getpar(&params);
  if (status) {
    fprintf(stderr, "ERROR: unable to get input parameters\n");
    return (status);
  }
  
  detid = (int *) malloc(sizeof(int)*BUFFERSIZE*3);
  energy = (double *) malloc(sizeof(double)*BUFFERSIZE);
  if (detid == 0 || energy == 0) {
    fprintf(stderr, "ERROR: could not allocate buffer memory (size %d)\n",
	    BUFFERSIZE);
    status = MEMORY_ALLOCATION;
    goto cleanup;
  }
  adu = detid + BUFFERSIZE;
  pi  = adu   + BUFFERSIZE;
  
  /* Initialize the values to zero -- NOTE: esp. because of zeroit */
  for (j=0;j<BUFFERSIZE;j++) {
    pi[j] = 0;
    energy[j] = 0.0;
  }
  
  
  headas_chat(5, "...creating output file %s...\n", params.infile);
  
  /* If an output file name is specified, then open a new output file
     and copy the input file to the output; Otherwise just open the
     input file */
  
  if (fits_open_file(&infptr,params.infile,
		     (*params.outfile) ? (READONLY) : (READWRITE),
		     &status)) {
    fprintf(stderr, "ERROR: could not open %s\n",params.infile);
    goto cleanup;
  }
  
  if (*params.outfile) { 
    /* Delete existing file if clobber=YES */
    headas_clobberfile(params.outfile); 
    fits_create_file(&outfptr,params.outfile,&status);
    fits_copy_file(infptr,outfptr,1,1,1,&status);
    if (status) {
      fprintf(stderr, "ERROR: could not copy infile to outfile\n");
      goto cleanup;
    }
    fits_close_file(infptr,&status);
    
    infptr = 0;
    wrtptr = outfptr;

    outfptr = 0; /* Set to zero so we don't close multiple times */
  } else {
    wrtptr = infptr;
    infptr = 0;
  }
  
  /* Move to the correct HDU in the input file. The file must contain
     an "EVENTS" extension.*/
  if (fits_movnam_hdu(wrtptr,BINARY_TBL,"EVENTS",0,&status)) {
    fprintf(stderr, "ERROR: could not find EVENTS extension\n");
    goto cleanup;
  }
  
  /* Get the number of events in the input file */
  fits_get_num_rows(wrtptr,&nevt,&status);
  headas_chat(5,"...number of events is %d...\n",nevt);
  if (status || (nevt == 0)) {
    fprintf(stderr, "ERROR: no rows found in input file\n");
    goto cleanup;
  }

  /* Get the column numbers for the input file.  The column numbers
     that are read are detector ID and PHA.  The PI column is written.*/
  fits_get_colnum(wrtptr,CASEINSEN,"DET_ID",&detcol,&status);
  fits_get_colnum(wrtptr,CASEINSEN,"PHA",&phacol,&status);
  if (status) {
    fprintf(stderr, "ERROR: DET_ID and PHA columns are required in input file\n");
    goto cleanup;
  }

  if (bateconvert_ecols(wrtptr, "PI", &picol, tlmin, tlmax, tnull_pi, 
			"ENERGY", &ecol, params.scaled_energy, &status)) {
    fprintf(stderr, "ERROR: unable to find/create ENERGY/PI columns\n");
    goto cleanup;
  }

  if (fits_read_key(wrtptr, TDOUBLE, "TSTART", &tstart, 0, &status)) {
    fprintf(stderr, "ERROR: unable to find the TSTART keyword\n");
    goto cleanup;
  }
  
  /* Simple case when just zero-ing the output energy and PI columns */
  if (params.zeroit) {
    
    for (ndone=0; ndone<nevt; ndone+=BUFFERSIZE) {
      bufsiz = (nevt-ndone);
      if (bufsiz > BUFFERSIZE) bufsiz=BUFFERSIZE;

      /* Energy and PI columns have already been zero'd above */
      fits_write_col(wrtptr, TINT, picol, ndone+1, 1,
		     bufsiz, pi, &status);
      fits_write_col(wrtptr, TDOUBLE, ecol, ndone+1, 1,
		     bufsiz, energy, &status);
    }

    /* Gain correction has *not* been applied */
    gainapp = 0;
    fits_update_key(wrtptr, TLOGICAL, "GAINAPP", &gainapp,
		    "Gain correction has been applied", &status);

    goto cleanup;
  }

  
  if (params.scaled_energy) {
    /* Note that the TNULLn value is in original *unscaled*
       values */
    fits_make_keyn("TNULL", ecol, keyname, &status);
    fits_update_key(wrtptr, TINT, keyname, &tnull_energy,
		    "data null value", &status);
    
    /* Only write the scale factor out if we are going to
       scale the energy values */
    fits_make_keyn("TSCAL", ecol, keyname, &status);
    fits_update_key(wrtptr, TDOUBLE, keyname, &energy_scale_factor,
		    "data scaling", &status);
    fits_set_hdustruc(wrtptr, &status);
    
    /* XXXX Tricky!!! We reset the *internal* scaling parameters
       so that internal integer energies are written as float
       values upon output, using the 0.1 keV scale factor */
    fits_set_tscale(wrtptr, ecol, 1.0, 0.0, &status);
  } 
  
  
  quadmode = 0;
  cubicmode = 0;
  /* Now read the gain/offset tables and fill the gain/offset structure */
  if (!strcmp(params.calmode,"INDEF"))     calmode=INDEF_METH;
  if (!strcmp(params.calmode,"LINEAR"))    calmode=LIN_METH;
  if (!strcmp(params.calmode,"QUADRATIC")) calmode=QUAD_METH;
  if (!strcmp(params.calmode,"CUBIC"))     calmode=CUBIC_METH;
  if (!strcmp(params.calmode,"FIXEDDAC"))  calmode=FIXEDDAC_METH;
  if (!strcmp(params.calmode,"DIRECTCUBIC")) calmode=DIRECT_METH;
  if (strcmp(params.calmode,"QUADRATIC") == 0) { 
    quadmode = 1;
  }
  if (strcmp(params.calmode,"CUBIC") == 0) {
     cubicmode = 1;
  }
  if (strcmp(params.calmode,"FIXEDDAC") == 0) {
    cubicmode = 1;
    caldata.gain_meth = FIXEDDAC_METH;
  }
  if (strcmp(params.calmode,"DIRECTCUBIC") == 0) {
    cubicmode = 1;
    caldata.gain_meth = DIRECT_METH;
  }
  
  headas_chat(5,"...before readgain_off...\n");
  /* Read the data geographically ... */
  status=bat_read_cal_coeff  (tstart, 
			      params.calfile,params.residfile,params.residext,
			      params.pulserfile,params.pulserext, 
			      params.fltpulserfile,params.fltpulserext,
			      &calmode,&caldata);
  bat_caldata_adjust(&caldata, 2);
  
  if(caldata.gain_meth == QUAD_METH) quadmode = 1;
  if(caldata.gain_meth == CUBIC_METH || 
     caldata.gain_meth == FIXEDDAC_METH || 
     caldata.gain_meth == DIRECT_METH) {
    cubicmode = 1;
  }
  headas_chat(5,"...after readgain_off (status=%d)...\n",status);
  if (status) goto cleanup;


  /* Check for bad values in flight telemetered gain/offset map */
  {
    int nbad = 0, ntot = 0;
    int nulval = caldata.flight_nulval;
    
    for (i=0; i<NUM_ELEMENTS; i++) {
      nbad += ( (caldata.ftotgain[i] == nulval) +
		(caldata.ftotoffset[i] == nulval) );
      ntot += 2;
    }
    
    if (nbad > 0) {
      fprintf(stderr, 
	      "WARNING: the gain/offset file contains %d null values out of %d\n",
	      nbad, ntot);
    }
  }
  
  headas_chat(5,"...buffersize is %d...\n",BUFFERSIZE);
  
  /* Here is the main part of the program in which BUFFERSIZE rows of
     data are read into memory, converted from ADU to energy and written
     to the output file. The parameter BUFFERSIZE is set in the include
     file bat_gswdev.h.  I have chosen a value of 32767 which is apparently
     the maximum number of elements an array can have.  It is also a
     good balance between size of memory required and number of
     calls.  There are about 750000 rows in one second of event data so
     there are around 22 CFTISIO calls*/
  
  /* Loop through as many times as necessary to read the entire file.
     Left-over rows are picked up at the end */
  
  
  ndone = 0;
  while (ndone < nevt) {
      
    /* Find out how many rows to read.  If not near the end, then read
       a set number (BUFFERSIZE).  If near the end, then read the rest.*/
    bufsiz=(nevt-ndone);
    if (bufsiz > BUFFERSIZE) bufsiz=BUFFERSIZE;
    
    /* First read the detector ID and PHA column for each row */
    
    fits_read_col(wrtptr,TINT,detcol,ndone+1,1,bufsiz,0,detid,0,&status);
    fits_read_col(wrtptr,TINT,phacol,ndone+1,1,bufsiz,0,adu,0,&status);
    if (status) {
      fprintf(stderr, "ERROR: could not read input data rows %ld-%ld\n",
	      ndone+1, ndone+bufsiz);
      goto cleanup;
    }
    
    
    /* Convert from ADUs to energy using the appropriate gain/offset 
       This follows from the prescription given by David Palmer for
       the flight code */
    
    for (j=0; j<bufsiz; j++) {
      
      int detidj = detid[j];
      float aduj = adu[j];
      int gain_meth = caldata.gain_meth;
      
      float ftotgain, ftotoffset, fpulseTokeV, fpulse0keV;
      float gpulseTokeV, gpulse0keV;
      float gpulresid0, gpulresid1, gpulresid2, gpulresid3;
      float gpul_nom_offset, gpul_nom_gain;
      float DAClow;
      int nulval;
      
      float e1;
      int xstatus = 0;
      
      /* Use look-up table to convert from DET_ID to
	 geographic position, which is an index number,
	 expressed as (DETX + DETY*286) */
      detidj = caldata.pos_lookup[detidj];
      
      ftotgain    = caldata.ftotgain[detidj];
      ftotoffset  = caldata.ftotoffset[detidj];
      fpulseTokeV = caldata.fpulseTokeV[detidj];
      fpulse0keV  = caldata.fpulse0keV[detidj];
      gpulseTokeV = caldata.gpulseTokeV[detidj];
      gpulse0keV  = caldata.gpulse0keV[detidj];
      gpulresid0  = caldata.gpulresid0[detidj];
      gpulresid1  = caldata.gpulresid1[detidj];
      gpulresid2  = caldata.gpulresid2[detidj];
      gpulresid3  = caldata.gpulresid3[detidj];
      gpul_nom_gain   = caldata.gpul_nom_gain[detidj];
      gpul_nom_offset = caldata.gpul_nom_offset[detidj];
      DAClow = caldata.DAClow;
      nulval = caldata.flight_nulval;
      
      /* Check for missing gain/offset values */
      bad_go = ( (ftotgain == nulval) || (ftotoffset == nulval) );
      
      bat_pha_to_energy(&aduj, &e1, 1, 
			gain_meth, 
			ftotgain, ftotoffset, 
			fpulseTokeV, fpulse0keV,
			gpulseTokeV, gpulse0keV,
			DAClow, 
			gpulresid0, gpulresid1, 
			gpulresid2, gpulresid3, 
			gpul_nom_offset, gpul_nom_gain, 
			0,   /* XXX Use bateconvert energy scale for resids (gpulseTokeV)*/
			&xstatus);
      
      /* Save resulting energy and scaled PI value */
      if (xstatus == 0) {
	energy[j] = e1;
	pi[j] = floor(e1/energy_scale_factor);
      } else {
	
	/* ... or a failure occurred, store NULL value */
	energy[j] = tnull_energy; 
	pi[j] = tnull_pi;
      }
      
      /* Check for out-of-bounds values */
      if((pi[j] < tlmin) || (pi[j] >= tlmax) || bad_go) {
	
	/* Defective energy, set to null */
	energy[j] = tnull_energy;
	pi[j]     = tnull_pi;
      }
      
      
      if (j<10 && ndone == 0) {
	headas_chat(5,"   (PI: %d ENERGY %f ADU %f)\n",
		    pi[j],energy[j],aduj);
      }
      
    } /* end for j */
    
    gainapp=1; 
    
    /* Write PI column */
    headas_chat(5,"...writing PI column...\n");
    fits_write_col(wrtptr,TINT,picol,ndone+1,1,bufsiz,pi,&status);
    
    /* Write ENERGY column */
    if (params.scaled_energy) {
      
      /* See XXXX Tricky comment above: we are actually writing
	 PI data value into the ENERGY column.  Since 
	 PI = ENERGY / (0.1 keV)    and also   TSCAL = 0.1
	 then we can do this trick by writing the PI value
	 as a raw unscaled value. */
      
      headas_chat(5,"...writing scaled ENERGY column...\n");
      fits_write_colnull(wrtptr,TINT,ecol,ndone+1,1,bufsiz,
			 pi,&tnull_pi,&status);
    } else {
      /* double tnull_energy_f = tnull_energy; */
      fprintf(stderr, "WARNING: invalid NULL energy value being written\n");
      
      headas_chat(5,"...writing floating ENERGY column...\n");
      fits_write_colnull(wrtptr,TDOUBLE,ecol,ndone+1,1,bufsiz,
			 energy,0,&status);
    }
    
    if (status) {
      fprintf(stderr, "ERROR: could not write PI/ENERGY columns\n");
      goto cleanup;
    }
    
    ndone += bufsiz;  
  } /* end while */
  
  
  /* Write out keyword indicating what processing has been done on the 
     event files:
     
     For event files that have been modified by bateconvert:
     GAINAPP = T          / Gain correction has been applied
     GAINMETH = 'LINEAR'  / Linear ground gain/offset correction was applied
     (or) GAINMETH = 'QUADRATIC' / Quadratic ground gain/offset correction was applied  */
  
  fits_update_key(wrtptr, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);
  fits_update_key(wrtptr, TLOGICAL, "GAINAPP", &gainapp,
		  "Gain correction has been applied", &status);

  /* Write the GAINMETH keyword */
  {
    char *keyval = 0;
    char *comment = 0;

    switch (caldata.gain_meth) {
    case DIRECT_METH:
      keyval = "DIRECTCUBIC";
      comment = "Cubic ground gain/offset correction using DAC-based residuals was applied";
      break;
    case FIXEDDAC_METH:
      keyval = "FIXEDDAC";
      comment = "Cubic ground gain/offset correction using DAC-based residuals was applied";
      break;
    case CUBIC_METH:
      keyval = "CUBIC";
      comment = "Cubic ground gain/offset correction was applied";
      break;
    case QUAD_METH:
      keyval = "QUADRATIC";
      comment = "Quadratic ground gain/offset correction was applied";
      break;
    case LIN_METH:
      keyval = "LINEAR";
      comment = "Linear gain/offset from flight file was applied (no ground correction)";
      break;
    default:
      keyval = "UNKNOWN";
      comment = "UNKNOWN energy conversion!";
      break;
    }

    fits_update_key(wrtptr,TSTRING,"GAINMETH", keyval, comment, &status);
  }
    
  /* Update keywords providing calibration tracability */
  { 
    char *p;
    p = rindex(params.calfile,'/');
    if (p == 0) p = params.calfile; else p++;
    fits_update_key(wrtptr, TSTRING, "BCALFILE", p,
		    "BAT total linear gain/offset file name", &status);

    p = rindex(params.residfile,'/');
    if (p == 0) p = params.residfile; else p++;
    fits_update_key(wrtptr, TSTRING, "BRESFILE", p,
		    "BAT Residual from linear gain file name", &status);

    p = rindex(params.pulserfile,'/');
    if (p == 0) p = params.pulserfile; else p++;
    fits_update_key(wrtptr, TSTRING, "BPULFILE", p,
		    "BAT Ground Pulser DAC to keV file name", &status);

    p = rindex(params.fltpulserfile,'/');
    if (p == 0) p = params.fltpulserfile; else p++;
    fits_update_key(wrtptr, TSTRING, "BFLTFILE", p,
		    "BAT Flight Pulser DAC to keV file name", &status);
  }

  
  /* Write optional history keywords */
  /* Replaced headas_parstamp with HDpar_stamp  14-Nov-2003  */
  status = HDpar_stamp(wrtptr,0,&status); 
  if (status) goto cleanup;
  
  /* Print out a summary of operations (unless chatter=0) */
  status=bateconvert_printsummary(&params,&nevt,taskname,taskver);
  if (status) goto cleanup;
  
  if (fits_close_file(wrtptr,&status)) goto cleanup;
  
  return(status);
  
 cleanup:
  
  /* Close any files that accidentally remain open */
  temp_status = 0;
  if (wrtptr) fits_close_file(wrtptr, &temp_status);
  temp_status = 0;
  if (outfptr) fits_close_file(outfptr,  &temp_status);
  temp_status = 0;
  if (infptr) fits_close_file(infptr,  &temp_status);
  temp_status = 0;
  if (pcfptr) fits_close_file(pcfptr,  &temp_status);
  
  /* Free any memory that was allocated */
  if (detid) { free(detid); detid = 0; }
  if (energy) { free(energy); energy = 0; }
  
  if (status) fits_report_error(stderr, status);
  return(status);
}


/*-------------------------------------------------------------*/

/* Routine to make all calls to PIL to fill the structure containing
   all user supplied paramters. */

int bateconvert_getpar (
    BATECONVERT_PARAMS *params)

{
 
   int len,status=0;
   char *cptr;
   fitsfile *infptr=0;
 
   struct caldbparms_struct caldb;

   headas_chat(5,"...entering bateconvert_getpar...\n");
   
   if ((status = PILGetFname("infile", params->infile)))
     fprintf(stderr, "Error reading the 'infile' parameter.\n");
   
   else if ((status = PILGetFname("outfile", params->outfile)))
     fprintf(stderr, "Error reading the 'outfile' parameter.\n");
   
   else if ((status = PILGetString("calmode", params->calmode)))
     fprintf(stderr, "Error reading the 'calmode' parameter.\n");
   
   else if ((status = PILGetBool("zeroit", &params->zeroit)))
     fprintf(stderr, "Error reading the 'zeroit' parameter.\n");
   
   else if ((status = PILGetBool("scaled_energy", &params->scaled_energy)))
     fprintf(stderr, "Error reading the 'scaled_energy' parameter.\n");
   
   if (status) return status;
   
   params->calfile[0] = '\0';
   params->residfile[0] = '\0';
   params->pulserfile[0] = '\0';
   params->fltpulserfile[0] = '\0';
   
   /* Default extension numbers */     
   params->pulserext = -1;
   params->fltpulserext = -1;
   params->residext = -1;
   
   /* Only prompt the user for these parameters if they are actually used */
   
   /* If zeroit==0 then there is no need for a calibration file. In
      this case a filename of "NONE" is allowed. */
   if (!(params->zeroit)) {
     
     if ((status = PILGetFname("calfile", params->calfile))) {
       fprintf(stderr, "Error reading the 'calfile' parameter.\n");
       return(status); 
     }
     
     headas_chat(5,"...parsing calmode...\n");
     if (params->calmode[0] == 'i' || params->calmode[0] == 'I') {
       strcpy(params->calmode,"INDEF"); 
     }
     if (params->calmode[0] == 'l' || params->calmode[0] == 'L') {
       strcpy(params->calmode,"LINEAR"); 
     }
     if (params->calmode[0] == 'q' || params->calmode[0] == 'Q') {
       strcpy(params->calmode,"QUADRATIC"); 
     }
     if (params->calmode[0] == 'c' || params->calmode[0] == 'C') {
       strcpy(params->calmode,"CUBIC"); 
     }
     if (params->calmode[0] == 'f' || params->calmode[0] == 'F') {
       strcpy(params->calmode,"FIXEDDAC"); 
     }
     if (params->calmode[0] == 'd' || params->calmode[0] == 'D') {
       strcpy(params->calmode,"DIRECTCUBIC"); 
     }
     if ((strcmp(params->calmode, "LINEAR")) 
	 && (strcmp(params->calmode, "INDEF"))
	 && (strcmp(params->calmode, "QUADRATIC"))
	 && (strcmp(params->calmode, "CUBIC")) 
	 && (strcmp(params->calmode, "DIRECTCUBIC")) 
	 && (strcmp(params->calmode, "FIXEDDAC"))) {
       status = -1;
       fprintf(stderr, "Error invalid 'calmode' parameter. Use INDEF, LINEAR,  QUADRATIC, CUBIC, DIRECTCUBIC, or FIXEDDAC\n");
       return(status); 
     }
     
     /* If calmode==LINEAR then there is no need for a 
	residual file or pulser calibration files. In
	this case a filename of "NONE" is allowed. */
     headas_chat(5,"...calmode='%s'...\n", params->calmode);
     if (!strcmp(params->calmode,"QUADRATIC")
        || !strcmp(params->calmode,"INDEF") 
        || !strcmp(params->calmode,"CUBIC") 
	|| !strcmp(params->calmode,"FIXEDDAC")
	|| !strcmp(params->calmode,"DIRECTCUBIC")) {
       if ((status = PILGetFname("residfile", params->residfile))) {
	 fprintf(stderr, "Error reading the 'residfile' parameter.\n");
	 return(status); 
       }
       else if ((status = PILGetFname("pulserfile", params->pulserfile))) {
	 fprintf(stderr, "Error reading the 'pulserfile' parameter.\n");
	 return(status); 
       }
       else if ((status = PILGetFname("fltpulserfile", params->fltpulserfile))) {
	 fprintf(stderr, "Error reading the 'fltpulserfile' parameter.\n");
	 return(status); 
       }
       
       if ( (strcasecmp(params->residfile,"CALDB") == 0) ||
	    (strcasecmp(params->pulserfile,"CALDB") == 0) ||
	    (strcasecmp(params->fltpulserfile,"CALDB") == 0) ) {
	 
	 fits_open_data(&infptr, params->infile, READONLY, &status);
	 batkw_to_caldb_parms(infptr, &caldb, 1, &status);
	 fits_close_file(infptr, &status);
	 if (status) {
	   fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
		   params->infile);
	   return status;
	 }
       }
       
       if (strcasecmp(params->residfile,"CALDB") == 0) {
	 char *expr = "-";
	 char *codenam = "DET_GAIN";
	 char *pfile = params->residfile;
	 char online[80], *ponline = online;
	 long int extno[1];
	 int maxret = 1;
	 int nret = 0, nfound = 0;
	 
	 bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			  &pfile, extno, &ponline, &nret, &nfound, &status);
	 if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
	 params->residext = extno[0];
       }
       
       /* Ground-derived pulser DAC to keV conversion */
       if (strcasecmp(params->pulserfile,"CALDB") == 0) {
	 char *expr = "SOURCE.EQ.\"GROUND\"";
	 char *codenam = "PULSER_GAIN";
	 char *pfile = params->pulserfile;
	 char online[80], *ponline = online;
	 long int extno[1];
	 int maxret = 1;
	 int nret = 0, nfound = 0;
	 
	 bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			  &pfile, extno, &ponline, &nret, &nfound, &status);
	 if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
	 params->pulserext = extno[0];
       }

       /* Flight pulser DAC to keV conversion */
       if (strcasecmp(params->fltpulserfile,"CALDB") == 0) {
	 char *expr = "SOURCE.EQ.\"FLIGHT\"";
	 char *codenam = "PULSER_GAIN";
	 char *pfile = params->fltpulserfile;
	 char online[80], *ponline = online;
	 long int extno[1];
	 int maxret = 1;
	 int nret = 0, nfound = 0;
	 
	 bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			  &pfile, extno, &ponline, &nret, &nfound, &status);
	 if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
	 params->fltpulserext = extno[0];
       }
       
     }

      /* remove leading blanks in output file string */
     cptr = params->outfile;
     while (*cptr == ' ') cptr++;  
     if (cptr != params->outfile) {
       len = strlen(cptr);
       memmove(params->outfile, cptr, len + 1);
     }
     
     /* test for special strings */
     if (!strcmp(params->outfile, "none") || !strcmp(params->outfile, "NONE") ) {
       *params->outfile = '\0';
     }
     
   }
   
   headas_chat(2,"     Calmode: %s\n",params->calmode);
   headas_chat(2,"  Input File: %s\n",params->infile);
   headas_chat(2," Output File: %s\n",params->outfile);
   headas_chat(2,"    Cal File: %s\n",params->calfile);
   headas_chat(2,"  Resid File: %s\n",
	       params->residfile[0] ? params->residfile : "NONE");
   headas_chat(2," Pulser File: %s\n",
	       params->pulserfile[0] ? params->pulserfile : "NONE");
   headas_chat(2," Flight Pulser File: %s\n",
	       params->fltpulserfile[0] ? params->fltpulserfile : "NONE");
   headas_chat(2,"  ENERGY Col: %s\n", 
	       params->scaled_energy ? "SCALED" : "FLOAT");
   
   headas_chat(2, "------------------------------------------\n");

   return(status);
   
}

/*-------------------------------------------------------------*/

int bateconvert_printsummary (
   BATECONVERT_PARAMS *params,
   long int *nevt,
   char *taskname,
   char *version)

{ 
  if (params->zeroit) {
    headas_chat(2,"  PI column was zeroed (%d events)\n", *nevt);
  } else {
    headas_chat(2,"  Energy conversion complete (%d events)\n", *nevt);
  }
  headas_chat(1,"  Data written to %s (%s)\n",
	      (params->outfile[0]) ? (params->outfile) : (params->infile),
	      (params->outfile[0]) ? "OUTFILE" : "INFILE");
  headas_chat(2, "------------------------------------------\n");
  
  return(0);
}

/*-------------------------------------------------------------*/

int bateconvert_ecols(fitsfile *wrtptr, 
		      char *picolname, int *picol, 
		      int pi_tlmin, int pi_tlmax, int pi_tnull, 
		      char *ecolname,  int *ecol,
		      int scaled_energy,
		      int *status)
{
  int makepicol = 0, makeecol = 0;
  char keyname[FLEN_CARD];

  if (*status) return (*status);

  /* Bracket these calls in a write/clear errmark functions so
     that errors are not reported to a higher level. */
  fits_write_errmark();
  if (fits_get_colnum(wrtptr, CASEINSEN, picolname, picol, status)) makepicol = 1;
  *status = 0;
  if (fits_get_colnum(wrtptr, CASEINSEN, ecolname, ecol, status)) makeecol = 1;
  *status = 0;
  fits_clear_errmark();
  
  /* ========================== Write the ENERGY and/or PI columns */
  if (makepicol || makeecol) {
    char *ttype[2], *tform[2]; 
    int noldcols = 0;
    int nnewcol = 0;

    headas_chat(5, "NOTE: creating output ENERGY/PI columns.\n");
    if (fits_get_num_cols(wrtptr, &noldcols, status)) 
      return (*status);
    
    /* Columns are added with one call, for efficiency.  Adding
           columns one at a time causes lots of disk access. */
    if (makepicol) {
      ttype[nnewcol] = picolname;
      tform[nnewcol] = "1I";
      nnewcol ++;
    }
    if (makeecol) {
      ttype[nnewcol] = ecolname;
      if (scaled_energy) {
	tform[nnewcol] = "1I";
      } else {
	tform[nnewcol] = "1E";
	  }
      nnewcol ++;
    }
    
    /* Insert these columns at the end of the table row */
    fits_insert_cols(wrtptr,noldcols+1,nnewcol,ttype,tform,status);
    fits_set_hdustruc(wrtptr, status);
    if (*status) {
      fprintf(stderr, "ERROR: could not create PI / ENERGY column\n");
      return (*status);
    }
    
    /* Make sure the columns exist */
    if (fits_get_colnum(wrtptr, CASEINSEN, picolname, picol, status) ||
	fits_get_colnum(wrtptr, CASEINSEN, ecolname, ecol, status)) {
      fprintf(stderr, "ERROR: could not find ENERGY/PI columns just created!\n");
      return (*status);
    }
    
    if (makepicol) {
      
      /* Write associated keywords for PI column */
      fits_make_keyn("TTYPE", *picol, keyname, status);
      fits_modify_comment(wrtptr, keyname, 
			  "Pulse Invariant", status);
      
      fits_make_keyn("TUNIT", *picol, keyname, status);
      fits_update_key(wrtptr, TSTRING, keyname, "chan", 
		      "physical unit of field", status);
      
      fits_make_keyn("TLMIN", *picol, keyname, status);
      fits_update_key(wrtptr, TINT, keyname, &pi_tlmin,
		      "Minimum legal value", status);
      
      fits_make_keyn("TLMAX", *picol, keyname, status);
      fits_update_key(wrtptr, TINT, keyname, &pi_tlmax,
		      "Maximum legal value", status);
      
      fits_make_keyn("TNULL", *picol, keyname, status);
      fits_update_key(wrtptr, TINT, keyname, &pi_tnull,
		      "data null value", status);
      
    }
    
    if (makeecol) {
      /* Write associated keywords for ENERGY column */
      
      headas_chat(5,"...creating ENERGY column...\n");
      fits_make_keyn("TTYPE", *ecol, keyname, status);
      fits_modify_comment(wrtptr, keyname, 
			  "Nominal Energy of event", status);
      
      fits_make_keyn("TUNIT", *ecol, keyname, status);
      fits_update_key(wrtptr, TSTRING, keyname, "keV", 
		      "physical unit of field", status);
    } 
    
  }

  return (*status);
}


/*-------------------------------------------------------------*/
