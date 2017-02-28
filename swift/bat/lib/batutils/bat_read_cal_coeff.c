/* 
 *
 * bat_read_cal_coeff.c - read BAT energy correction coefficients
 *
 * This module reads BAT energy correction coefficients.  It is meant
 * to be a replacement for bat_read_calfiles.c and
 * bat_read_cal_array.c, which were specific to there respective
 * tasks.  This new routine is generic and usable by either task, thus
 * keeping energy coefficent logic in one single place.
 *
 * The routine bat_read_calfiles() reads the following kinds of files,
 *
 *   char *gofile -- A flight linear gain/offset table containing the
 *     total linear gain and offset of the instrument as determined by
 *     the flight computer.
 *   char *resfile, int resext -- The cubic residual file containing
 *     residuals between the flight linear equation and the true
 *     non-linear response.  (filename and extension)
 *   char *pulfile, int pulext -- The best ground determined pulser
 *     linearity, a conversion between the flight pulser DAC units and
 *     energy.  (filename and extension)
 *   char *fltpulfile, int fltpulext -- The on-board flight-used
 *     pulser linearity coefficients.  (filename and extension) In some
 *     cases this term must be used to reverse the flight correction in
 *     order to re-derive pulse height values.
 *
 *   double time - requested time; if the input total linear
 *     gain/offset file contains multiple time rows, then the row
 *     closest in TIME to the requested time will be returned.
 *   int calmode - requested calibration method (linear, cubic, etc)
 * 
 *   bat_ecal_data *caldata - upon return, the values have been read
 *     into this structure, which must be allocated by the caller.
 *     Unlike the previous versions, this version of the routine does
 *     essentially no transformations upon the coefficients.
 * 
 *
 * Before using the caldata structure, you must call,
 *   bat_caldata_adjust(caldata, client)
 * where client takes one of these values:
 *   client==1: old version of bateconvert with energy conversion in-task
 *              (OBSOLETE)
 *   client==2: new version of bateconvert which uses econv.c
 *              new version of baterebin which uses econv.c
 * 
 */

#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h"

/* Routine to read the gain and offset values from the calibration file.
   The format of the gains and offsets is a BAT DPI map. */

static int bat_read_gofile(double time, fitsfile *pcfptr, bat_ecal_data *caldata);
static int bat_read_resfile(fitsfile *resptr, int resext, int *calmode,
			    bat_ecal_data *caldataK);
static int bat_read_pulfile(fitsfile *pulptr, int pulext, 
			    bat_ecal_data *caldata);
static int bat_read_fltpulfile(fitsfile *fltpulptr, int fltpulext, 
			       bat_ecal_data *caldata);
static int bat_mov_map_ext(fitsfile *file, char *extname1, int extnum);

#define NUM_PIXELS DAP_PIXELS

/* These are made global simply to avoid dumping large arrays on the
   stack, which can crash some environments (i.e. Mac OS X) */

static unsigned short int detid[NUM_ELEMENTS];
static short block[NUM_ELEMENTS], dm[NUM_ELEMENTS], det[NUM_ELEMENTS],
  row[NUM_ELEMENTS], col[NUM_ELEMENTS];


/* ==================== MAIN ROUTINE ===================== */
int bat_read_cal_coeff(double time,  
		       char *gofile, 
		       char *resfile, int resext,
		       char *pulfile, int pulext,
		       char *fltpulfile, int fltpulext,
		       int *calmode,
		       bat_ecal_data *caldata)
{
  int status = 0, safestatus = 0;
  long int i, j;
  int nulval = -999999;
  
  fitsfile *resptr = 0, *pulptr = 0, *fltpulptr = 0, *pcfptr = 0;
  
  headas_chat(5,
	      "...bat_read_cal_coeff(gofile='%s',\n"
	      "                      resfile='%s[%d]',\n"
	      "                      pulfile='%s[%d]',\n"
	      "                      fltpulfile='%s[%d]',\n"
	      "                      calmode=%d)...\n",
	      gofile, 
	      resfile, resext,
	      pulfile, pulext,
	      fltpulfile, fltpulext,
	      *calmode);

  /* Set up an array that gives the location within the BAT map of each
     detector */
  for (i=0;i<NUM_ELEMENTS;i++) detid[i]=i;
  batidconvert(NUM_ELEMENTS,detid,block,dm,det,row,col);
  for (i=0;i<NUM_ELEMENTS;i++) caldata->pos_lookup[i]=col[i]+row[i]*DAP_COLS;

  /* Store this value so that downstream software can know that a gain
     or offset value was missing */
  caldata->flight_nulval = nulval;

  /* ==================== GAIN/OFFSET ===================== */
  /* Now open the flight cal file and move to the correct HDU*/
  headas_chat(3, "...opening gofile...\n");
  if (fits_open_data(&pcfptr,gofile,READONLY,&status)) {
    fprintf(stderr, "ERROR: could not open gofile %s\n", gofile);
    return status;
  }
  status = bat_read_gofile(time, pcfptr, caldata);
  safestatus = 0;
  fits_close_file(pcfptr, &safestatus);
  if (status) return (status);

  /* ==================== LINEAR MODE ===================== */
  /* Linear mode is super simple, just the gain/offset data
     from flight, no tweaking from ground.  Just return. */
  if(*calmode == LIN_METH) {

    caldata->gain_meth = LIN_METH;
    return status;
  }


  /* ==================== RESIDUAL FILE ===================== */
  /* Otherwise open the residual file and read relevant keywords */

  /* Now open the ground residuals file and move to the correct HDU*/
  headas_chat(3, "...opening residfile...\n");
  if (fits_open_data(&resptr,resfile,READONLY,&status)) {
    fprintf(stderr, "ERROR: could not open residual file %s\n", resfile);
    return status;
  }
  status = bat_read_resfile(resptr, resext, calmode, caldata);
  safestatus = 0;
  fits_close_file(resptr, &safestatus);

  /* ==================== GROUND PULSER FILE ===================== */
  /* Now open the ground pulser cal file and move to the correct HDU*/
  headas_chat(3, "...opening ground pulser cal file...\n",pulfile);
  if (fits_open_data(&pulptr,pulfile,READONLY,&status)) {
    fprintf(stderr, "ERROR: could not open ground pulser file %s\n", pulfile);
    return status;
  }
  status = bat_read_pulfile(pulptr, pulext, caldata);
  safestatus = 0;
  fits_close_file(pulptr, &safestatus);
  
  
  /* ==================== FLIGHT PULSER FILE ===================== */
  /* Now open the flight pulser cal file and move to the correct HDU*/
  headas_chat(3, "...opening flight pulser file...\n", fltpulfile);
  if (fits_open_data(&fltpulptr,fltpulfile,READONLY,&status)) {
    fprintf(stderr, "ERROR: could not open flight pulser file %s\n", fltpulfile);
    return status;
  }
  status = bat_read_fltpulfile(fltpulptr, fltpulext, caldata);
  safestatus = 0;
  fits_close_file(fltpulptr, &safestatus);

  /* ==================== FINAL PROCESSING ===================== */
  for (j=0; j<NUM_ELEMENTS; j++) {
    i = caldata->pos_lookup[j];
    if (caldata->gpulseTokeV[i] == 0.0) {
      caldata->gpulresid3[i]  = 0.0;
      caldata->gpulresid0[i]  = 0.0;
      caldata->gpulresid2[i]  = 0.0;
      caldata->gpulresid1[i]  = 0.0;
    } else {
      /* Convert from Volts / ADU^n   --->  (0.1 keV) / ADU^n */
      /* The 0.1 keV stuff is because bateconvert works in tenth keV units */
      caldata->gpulresid3[i]  *= 1.0;
      caldata->gpulresid0[i]  *= 1.0;
      caldata->gpulresid2[i]  *= 1.0;
      caldata->gpulresid1[i]  *= 1.0;
    }
  }

  return status;
}

/* ==================== UTILITY ROUTINES ===================== */

/* ==================== GAIN/OFFSET ===================== */
int bat_read_gofile(double time, fitsfile *pcfptr, bat_ecal_data *caldata)
{
  char keyname[FLEN_CARD];
  double tscal = 0.0;
  int status = 0;
  int gaincol = 0, offsetcol = 0, timecol = 0;
  int anynul = 0;
  int i, row = 1;
  long int nrows = 0;
  double *gotimes = 0, timediff = 0;
  
  /* ------- */
  /* Because of HEASARC changes, we must check for the possible
     extension names BAT_MAP and/or Gain_Offset_Map */
  status = bat_mov_map_ext(pcfptr, "Gain_Offset_Map", -1);
  if (status) status = bat_mov_map_ext(pcfptr, "BAT_MAP", -1);
  if (status) {
    fprintf(stderr, "ERROR: could not find Gain_Offset_Map in gofile\n");
    return status;
  }
  
  /* ------- */
  /* Get the column numbers of the gain and offset maps */
  fits_get_num_rows(pcfptr, &nrows, &status);
  fits_get_colnum(pcfptr,CASEINSEN,"TIME",&timecol,&status);
  if (status || nrows == 0) {
    fprintf(stderr, "ERROR: input gain/offset file has no rows\n");
    return status;
  }
  fits_get_colnum(pcfptr,CASEINSEN,"GAIN",&gaincol,&status);
  fits_get_colnum(pcfptr,CASEINSEN,"OFFSET",&offsetcol,&status);
  if (status) {
    fprintf(stderr, "ERROR: GAIN/OFFSET columns not found in gain/offset file (status=%d)\n",
	    status);
    return status;
  }
  fits_read_key(pcfptr, TINT, "OFFPULSE", &(caldata->DAClow), 0, &status);
  fits_read_key(pcfptr, TINT, "GAININD", &(caldata->gainindex), 0, &status);
  fits_read_key(pcfptr, TINT, "OFFINDEX", &(caldata->offsetindex), 0, &status);
  if (status) {
    fprintf(stderr, 
	    "ERROR: Could not read OFFPULSE/GAININD/OFFINDEX keywords in gain/offset file (status=%d)\n", status);
    return status;
  }         

  gotimes = (double *) malloc(nrows*sizeof(double));
  if (gotimes == 0) return MEMORY_ALLOCATION;
  if (fits_read_col(pcfptr, TDOUBLE, timecol, 1, 1, nrows, 0, 
		    gotimes, 0, &status)) {
    fprintf(stderr, "ERROR: Could not read TIME column of gain/offset file (status=%d)\n", 
	    status);
    return status;
  }

  /* ------- */
  /* Search for the row that is closest in time to the requested time */
  timediff = 1e300;
  for (i = 0; i < nrows; i++) {
    if (fabs(time-gotimes[i]) < timediff) {
      timediff = fabs(time-gotimes[i]);
      row = i+1;
    }
  }
  headas_chat(4, "...using row %d from gain/offset file...\n", row);
  if (timediff > 5000 && timediff < 20000) {
    headas_chat(1, "NOTE: Time separation between data (MET=%.1f) and gain/offset sample (MET=%.1f) is %f seconds\n",
		time, gotimes[row-1], timediff);
  } else if (timediff > 20000) {
    fprintf(stderr, "WARNING: Time separation between data (MET=%.1f) and gain/offset sample (MET=%.1f) is %f seconds\n",
	    time, gotimes[row-1], timediff);
  }
  
  /* ------- */
  headas_chat(4,"...reading flight gain/offset file...\n");

  /* NOTE: the linear part is always executed.  The linear conversion
     uses the gain and offset directly from the flight map where the
     numbers are stored as integers.  */
  /* In each case, the entire BAT_MAP array is read into the
     temp_array in geographic order.  Then the location[] array is
     used as a look-up to put them in detector order. */

  /* GAIN */
  /* This little trick writes a slightly more precise value of 
     TSCALE, when it equals the default of 1.0/81920.0. */
  /* TSCALn */
  fits_make_keyn("TSCAL",gaincol,keyname,&status);
  fits_write_errmark();
  fits_read_key(pcfptr, TDOUBLE, keyname, &tscal, 0, &status);
  fits_clear_errmark();
  /* If TSCALn == 1.0/8192.0 within 1E-10 ... */
  if (status == 0 && abs(tscal-1.0/81920.0) < 1E-10) {
    /* ... write a revised value */
    tscal = 1.0 / 81920.0;
    fits_set_tscale(pcfptr,gaincol,tscal,0.0,&status);
  }
  status = 0;
  fits_read_col(pcfptr,TFLOAT,gaincol,row,1,NUM_PIXELS,
		&(caldata->flight_nulval),caldata->ftotgain,&anynul,&status);

  /* OFFSET */
  fits_read_col(pcfptr,TFLOAT,offsetcol,row,1,NUM_PIXELS,
		&(caldata->flight_nulval),caldata->ftotoffset,&anynul,&status);
  if (status) {
    fprintf(stderr, "ERROR: could not read gain/offset values from gofile\n");
  }

  if (gotimes) free(gotimes);
  gotimes = 0;
  return status;
}

/* ==================== RESIDUAL FILE ===================== */
int bat_read_resfile(fitsfile *resptr, int resext, int *calmode,
		     bat_ecal_data *caldata)
{
  int i, j;
  int status = 0;
  char gainmethkey[FLEN_CARD] = "QUADRATIC";
  int qoffsetcol, qgaincol, qgain2col, qgain3col, qlingaincol, qlinoffcol;
  /* NOTE: also used in > 1.0 tests in bat_read_resfile, so
     don't make it negative. */
  float nulval = 1e40;
  int anynul = 0;

  /* ------- */
  /* Move to the correct HDU.  There must be a BAT_MAP extension.*/
  status = bat_mov_map_ext(resptr, "BAT_MAP", resext);
  if (status) {
    fprintf(stderr, "ERROR: the BAT_MAP extension does not exist in residfile\n");
    return status;
  }

  if (fits_read_key(resptr, TSTRING, "GAINMETH", gainmethkey, 0, &status)) {
    fprintf(stderr, "ERROR: Could not find GAINMETH keyword in residfile, status %d\n", status);
    return status;
  }         
  
  if ( (strcasecmp(gainmethkey,"QUADRATIC") == 0) ) {
    
    headas_chat(1, "  QUADRATIC correction used\n");
    caldata->gain_meth = QUAD_METH;
    
  } else  if ( (strcasecmp(gainmethkey,"CUBIC") == 0)) {
    
    headas_chat(1, "  CUBIC correction used\n");
    caldata->gain_meth = CUBIC_METH;
    
  } else  if ( (strcasecmp(gainmethkey,"FULLCUBIC") == 0)) {
    
    if(caldata->gain_meth == DIRECT_METH) {
      headas_chat(1, "  DIRECTCUBIC correction used\n");
    } else if(caldata->gain_meth == FIXEDDAC_METH) {
      headas_chat(1, "  FIXEDDAC correction used\n");
    } else if(*calmode == INDEF_METH) {    /* INDEF */
      caldata->gain_meth = FIXEDDAC_METH;
      headas_chat(1, "  FIXEDDAC correction used\n");
    } else {
      fprintf(stderr, 
	      "ERROR: The value of the GAINMETH keyword in the residuals\n"
	      "   file is FULLCUBIC. For this value, the parameter must\n"
	      "   be FIXEDDAC or DIRECTCUBIC.\n");
      return -1;
    }
      
  } else {
    
    fprintf(stderr, 
	    "ERROR: The value of the GAINMETH keyword in the residuals\n"
	    "   file is not valid for this version of the software. Check\n"
	    "   for software updates.\n");
    return -1;
  }

  if(*calmode == INDEF_METH) {     /* INDEF */
    *calmode = caldata->gain_meth;
  }
  
  if(*calmode != caldata->gain_meth) {
    
    fprintf(stderr, 
	    "ERROR: The value of the GAINMETH keyword in the residuals\n"
	    "   file is %s.\n"
	    "   Your selected value of the calmode parameter does\n"
	    "   not match.\n", gainmethkey);
    return -1;
  }

  /* ------- */
  fits_get_colnum(resptr,CASEINSEN,"OFFSET",&qoffsetcol,&status);
  fits_get_colnum(resptr,CASEINSEN,"GAIN",&qgaincol,&status);
  fits_get_colnum(resptr,CASEINSEN,"GAIN2",&qgain2col,&status);
  if(caldata->gain_meth==CUBIC_METH || 
     caldata->gain_meth==FIXEDDAC_METH || 
     caldata->gain_meth==DIRECT_METH) {
    fits_get_colnum(resptr,CASEINSEN,"GAIN3",&qgain3col,&status);
  }
  if (status) {
    fprintf(stderr, 
	 "ERROR: could not read required OFFSET/GAIN/GAIN2/GAIN3 keywords\n");
    return status;
  }
    
  /* ------- */
  if(caldata->gain_meth==FIXEDDAC_METH || 
     caldata->gain_meth==DIRECT_METH) {
    if (fits_get_colnum(resptr,CASEINSEN,"LIN_GAIN",&qlingaincol,&status) ||
	fits_get_colnum(resptr,CASEINSEN,"LIN_OFFSET",&qlinoffcol,&status)) {
      fprintf(stderr,
	      "WARNING: You appear to be using an old 'residuals' file,\n"
	      "   without the full cubic fit and coefficients for the \n"
	      "   linear fit from which the residuals were calculated.\n"
	      "   You should use the updated file in CALDB.\n");
      status = 0;
    }
  }
  
  /* Read in the quadratic residuals.  The units here are:
     [V],[V/ADU],[V/ADU^2] so they have to be multiplied by the 
     pulser gain in units [keV/V] to get these into the correct
     units.  Also multiply by a factor of 10, since energy is in
     units of 0.1 keV */
  /* HAK 17-June-2003.  The units of the pulser2keV files and the
     quadratic residual files have changed.  The units are now pulser
     DAC units, not voltage.  The conversion is DAC = (4095) * Volts.
     This does not require a change to this code since the change 
     is in both files and cancels out.  However, to conform with the
     flight table the pulser2keV gain is inverted from KeV/Volt (original)
     to DAC/keV (new).  This means the that the residuals have to be
     divided by the pulser gain instead of multiplied. */
  /* The factor of 10.0 a few lines down is needed because all keV
     values in these files are actually in units of 0.1 keV (due to
     the need for them to be integers in the flight code). */

  headas_chat(3,"...reading non-linear correction file...\n");
  if(caldata->gain_meth == CUBIC_METH || 
     caldata->gain_meth == FIXEDDAC_METH || 
     caldata->gain_meth == DIRECT_METH) {
    if(caldata->gain_meth == FIXEDDAC_METH) {
      fits_read_col(resptr,TFLOAT,qlingaincol,1,1,NUM_PIXELS,0,
		    caldata->gpul_nom_gain,0,&status);
      fits_read_col(resptr,TFLOAT,qlinoffcol,1,1,NUM_PIXELS,0,
		    caldata->gpul_nom_offset,0,&status);
      if (status) return status;
    }
      
    fits_read_col(resptr,TFLOAT,qgain3col,1,1,NUM_PIXELS,&nulval,
		  caldata->gpulresid3,&anynul,&status);
    if (status) return status;
  }

  if (fits_read_col(resptr,TFLOAT,qgain2col,1,1,NUM_PIXELS,&nulval,
		    caldata->gpulresid2,&anynul,&status)) return status;
  if (fits_read_col(resptr,TFLOAT,qgaincol,1,1,NUM_PIXELS,&nulval,
		    caldata->gpulresid1,&anynul,&status)) return status;
  if (fits_read_col(resptr,TFLOAT,qoffsetcol,1,1,NUM_PIXELS,0,
		    caldata->gpulresid0,0,&status)) return status;


  /* Zero out any "bad" entries, which effectively removes any cubic
     correction for those detectors. */
  for (j=0;j<NUM_ELEMENTS;j++) {
    i = caldata->pos_lookup[j];
    if ( (caldata->gpulresid3[i]   > 1.0) ||
	 (caldata->gpulresid2[i]   > 1.0) ||
	 (caldata->gpulresid1[i]  == nulval) ||
	 (caldata->gpulresid0[i] == nulval)) {
      caldata->gpulresid3[i] = 0.0; 
      caldata->gpulresid2[i] = 0.0; 
      caldata->gpulresid1[i] = 0.0;       
      caldata->gpulresid0[i] = 0.0; 
    }
    
  }
  
  return status;
}

/* ==================== GROUND PULSER FILE ===================== */
int bat_read_pulfile(fitsfile *pulptr, int pulext, 
		     bat_ecal_data *caldata)
{

  int status = 0;
  int poffsetcol, pgaincol;
  float nulval = 0.0;
  int anynul = 0;

  /* Move to the correct HDU.  There must be a BAT_MAP extension.*/
  status = bat_mov_map_ext(pulptr, "BAT_MAP", pulext);
  if (status) {
    fprintf(stderr, "ERROR: the BAT_MAP extension does not exist in pulfile\n");
    return status;
  }
  fits_get_colnum(pulptr,CASEINSEN,"OFFSET",&poffsetcol,&status);
  fits_get_colnum(pulptr,CASEINSEN,"GAIN",&pgaincol,&status);
  if (status) {
    fprintf(stderr, "ERROR: required columns GAIN/OFFSET not found in pulfile\n");
    return status;
  }

  headas_chat(3,"...reading ground pulserfile...\n");
  fits_read_col(pulptr,TFLOAT,pgaincol,1,1,NUM_PIXELS,&nulval,
		caldata->gpulseTokeV,&anynul,&status);
  fits_read_col(pulptr,TFLOAT,poffsetcol,1,1,NUM_PIXELS,&nulval,
		caldata->gpulse0keV,&anynul,&status);
  if (status) {
    fprintf(stderr, "ERROR: could not read GAIN/OFFSET values from pulfile\n");
  }
  return status;
}  

/* ==================== FLIGHT PULSER FILE ===================== */
int bat_read_fltpulfile(fitsfile *fltpulptr, int fltpulext, 
			bat_ecal_data *caldata)
{
  int i;
  int status = 0;
  int flpoffsetcol, flpgaincol;
  float nulval = 0.0;
  int anynul = 0;

  /* Move to the correct HDU.  There must be a BAT_MAP extension.*/
  status = bat_mov_map_ext(fltpulptr, "BAT_MAP", fltpulext);
  if (status) {
    fprintf(stderr, "ERROR: the BAT_MAP extension does not exist in fltpulfile\n");
    return status;
  }
  
  fits_get_colnum(fltpulptr,CASEINSEN,"OFFSET",&flpoffsetcol,&status);
  fits_get_colnum(fltpulptr,CASEINSEN,"GAIN",&flpgaincol,&status);
  if (status) {
    fprintf(stderr,
       "WARNING: You appear to be using an old pulserfile, without\n"
       "   separate onboard and best values for the gain and offset.\n"
       "   Essentially this will result in an incomplete energy\n"
       "   correction if the onboard table for the time of this \n"
       "   event data was not the same as the best values.\n");
    for (i=0;i<DAP_CELLS;i++) {
      caldata->fpulseTokeV[i] = caldata->gpulseTokeV[i];
      caldata->fpulse0keV[i]  = caldata->gpulse0keV[i];
    }
    return 0;
  }
    
  headas_chat(4,"...reading flight fltpulserfile...\n");
  /* First read in the pulser calibration values.  The FITS TNULL value
     is 65535.0 */
  fits_read_col(fltpulptr,TFLOAT,flpgaincol,1,1,NUM_PIXELS,&nulval,
		caldata->fpulseTokeV,&anynul,&status);
  fits_read_col(fltpulptr,TFLOAT,flpoffsetcol,1,1,NUM_PIXELS,&nulval,
		caldata->fpulse0keV,&anynul,&status);
  if (status) {
    fprintf(stderr, "ERROR: could not read GAIN/OFFSET values from fltpulfile\n");
  }
  return status;
}


/* 
 * bat_caldata_adjust - make client-specific adjustments to calibration data
 * 
 * The goal is to eliminate this function, but it's here for now.
 *
 * bat_ecal_data *caldata - structure returned by bat_read_cal_coeff()
 * int client - client using this data
 *      1 - bateconvert, with in-task energy calculation
 *      2 - bateconvert, with library energy calculation
 */

int bat_caldata_adjust(bat_ecal_data *caldata, int client)
{
  int status = 0;
  int i;

  switch(client) {
  case 1:  /*   1 - bateconvert, with in-task energy calculation  */
    for (i=0; i<DAP_CELLS; i++) {
      float gpulseTokeV;

      if (caldata->gpulresid0[i] != 0 && caldata->gpulresid1[i] != 0) {
	caldata->gpulresid0[i] -= caldata->gpul_nom_offset[i];
	caldata->gpulresid1[i] -= caldata->gpul_nom_gain[i];
      }

      gpulseTokeV = caldata->gpulseTokeV[i];
      caldata->gpulresid0[i] *= (10.0/gpulseTokeV);
      caldata->gpulresid1[i] *= (10.0/gpulseTokeV);
      caldata->gpulresid2[i] *= (10.0/gpulseTokeV);
      caldata->gpulresid3[i] *= (10.0/gpulseTokeV);
      
      /* Old calculation assumes that values are scaled integers, so
	   simulate that effect here. */
      caldata->ftotgain[i]   = rint(caldata->ftotgain[i]*81920.0);
      caldata->ftotoffset[i] = rint(caldata->ftotoffset[i]*8.0);

    }
    break;
  case 2:  /*   2 - bateconvert, with library energy calculation */
    for (i=0; i<DAP_CELLS; i++) {
      if (caldata->fpulseTokeV[i]) {
	caldata->fpulseTokeV[i] = (1.0/(double)caldata->fpulseTokeV[i]);
      }
      if (caldata->gpulseTokeV[i]) {
	caldata->gpulseTokeV[i] = (1.0/(double)caldata->gpulseTokeV[i]);
      }
    }
    break;
  }

  return status;
}


/*
 * bat_mov_ext - move to requested extension name/number
 * 
 * fitsfile *file - pointer to open FITS file
 * char *extname - requested extension name
 * int extnum - requested extension number (-1 = ignore)
 *              primary extension = 0
 * 
 * If extnum >= 0, then it moves to that explicit extension number 
 * and returns immediately.
 *
 * If extnum == -1, then it does the following checks,
 *   * if current extension name matches extnum, then return
 *   * otherwise seek to the first extension which matches that name
 */
static int bat_mov_map_ext(fitsfile *file, char *extname1, int extnum)
{
  int status = 0;
  char cur_extname[FLEN_CARD] = "";

  if (extnum >= 0) {
    fits_movabs_hdu(file, extnum+1, 0, &status);
    if (status) return status;
  }

  /* OK, first check the extension name to see if it matches already */
  fits_read_key(file, TSTRING, "EXTNAME", cur_extname, 0, &status);
  if (status) return status;
  /* A match! */
  if (strcmp(extname1, cur_extname) == 0) return 0;

  /* If we requested an explicit extension number and the name did
     *not* match, then we must return an error here. */
  if (extnum >= 0) {
    return BAD_HDU_NUM;
  }    

  /* Otherwise, move to the named extension */
  return fits_movnam_hdu(file, ANY_HDU, extname1, 0/*HDUVER*/, &status);
}
