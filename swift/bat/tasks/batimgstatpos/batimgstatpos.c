#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"
#include "imageutils.h"

#include "coordfits.h"

static char taskname[] = "batimgstatpos";
static char taskver[]  = "1.4";

/* 
 * Task to compute celestial coordinates of BAT image status packet positions
 *
 *   C. Markwardt
 *   Jul 2007
 *
 * $Id: batimgstatpos.c,v 1.9 2010/12/16 20:15:17 craigm Exp $
 *
 *
 */

#define TOOLSUB batimgstatpos
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


struct parm_struct 
{
  char *taskname;               /* Name of this task */
  char *taskver;                /* Version number string */
  char infile[PIL_PATH_MAX];   /* Output image file name */
  char outfile[PIL_PATH_MAX];   /* Output image file name */
  char attitude[PIL_PATH_MAX];  /* Attitude file name */
  char teldef[PIL_PATH_MAX];    /* Telescope definition file name */
  char distfile[PIL_PATH_MAX];  /* (optional) distortion map file */
  double maxtimegap;            /* Maximum time gap */
};


/* ------------------------------------------------------------------------- */
/* Read parameter file */
int batimgstatpos_getpar(struct parm_struct *parms) 
{
  int status = 0;

  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->attitude[0] = 0;
  parms->teldef[0] = 0;
  parms->distfile[0] = 0;

  if ((status = PILGetString("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("attitude", parms->attitude)))
    fprintf(stderr, "Error reading the 'attitude' parameter.\n");

  else if ((status = PILGetString("teldef", parms->teldef)))
    fprintf(stderr, "Error reading the 'teldef' parameter.\n");

  else if ((status = PILGetString("distfile", parms->distfile)))
    fprintf(stderr, "Error reading the 'distfile' parameter.\n");

  else if ((status = PILGetReal("maxtimegap", &parms->maxtimegap)))
    fprintf(stderr, "Error reading the 'maxtimegap' parameter.\n");

  /* Special case: No distortion file specified */
  if (strcasecmp(parms->distfile, "none") == 0) {
    parms->distfile[0] = 0;
  }

  return status;
}

/* ------------------------------------------------------------------------- */
/* Print a standard banner for the user */
void banner(struct parm_struct *parms)
{

  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "      Input File: %s\n", parms->infile);
  headas_chat(2, "   Attitude File: %s\n", parms->attitude);
  headas_chat(2, "     TelDef File: %s\n", parms->teldef);
  headas_chat(2, "    Output Image: %s\n", parms->outfile);
  if (parms->distfile[0]) 
    headas_chat(2, "  Distortion Map: %s\n", parms->distfile);
  else
    headas_chat(2, "  Distortion Map: NONE\n");
  headas_chat(2, "------------------------------------------\n");
}

/* ------------------------------------------------------------------------- */
/* Final summary */
void summary(struct parm_struct *parms, int ncomplete, int nnull)
{
  headas_chat(2, "  Done.\n");
  headas_chat(2, "  Converted %d coordinates (plus %d nulls)\n", 
	      ncomplete, nnull);
  headas_chat(2, "------------------------------------------\n");
}

/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */

/* 
 * locate_caldb_files - find CALDB files (TELDEF & distortion map)
 *
 * struct parm_struct *parms - pointer to parameter structure
 *                     parms->infile - name of input detector image file
 *                     parms->aperture - name of aperture, or "CALDB"
 *                     parms->teldef - name of TELDEF file, or "CALDB"
 * fitsfile **infile - pointer to FITS file pointer.  Upon return,
 *                     the input file may have been opened.  The
 *                     caller is responsible for closing.
 *
 * RETURNS: FITS error status code
 *
 */
int locate_caldb_files(struct parm_struct *parms, fitsfile *infile)
{
  struct caldbparms_struct caldb;
  int status = 0;

  if (parms == 0 || infile == 0) return NULL_INPUT_PTR;

  batkw_to_caldb_parms(infile, &caldb, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
	    parms->infile);
    return status;
  }
	
  /* Locate the TELDEF file */
  if (parms->attitude[0] && (strcasecmp(parms->teldef, "CALDB") == 0)) {
    char *expr = "-";
    char *codenam = "TELDEF";
    char *pfile = parms->teldef;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;
    
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the BAT teldef file in CALDB\n");
      return status;
    }
  }	       

  /* Read the distortion map if requested - check for CALDB */
  if (strcasecmp(parms->distfile, "CALDB") == 0) {
    char *expr = "-";
    char *codenam = "DET_POSCOR";
    char *pfile = parms->distfile;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;
    
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the BAT distortion map in CALDB\n");
      return status;
    }
  }	       

  return status;
}


/* ------------------------------------------------------------------------- */

/* 
 * Main work routine 
 *
 */
int batimgstatpos_work(struct parm_struct *parms)
{
  fitsfile *infile = 0, *outfile = 0;
  TELDEF *teldef = 0;
  ATTFILE *attfile = 0;
  int timecol, expocol, thetacol, phicol, nsrccol;
  int racol = 0, deccol = 0, raccol = 0, decccol = 0, rollccol = 0;
  int imxcol = 0, imycol = 0, snrcol = 0, oexpocol = 0, 
    catnumcol = 0, ocatnumcol = 0,
    noisecol = 0, strengthcol = 0;
  int tstart1col, tstart2col, tstop1col, tstop2col; /* Columns w/ image start/stop time*/
  int dsp_timecol = 0; /* Column number of DSP_TIME (if used) */
  int null_dsp_time = 0; /* Set DSP_TIME to null? */
  int imgtcorr = 0;  /* Input already has TIME column corrected? */
  int ncols;
  long int nrows, nelt;
  long int nvect, width;
  int irow, jsrc;
  int typecode;
  double RTOD = (180.0 / M_PI);  /* Radian to degree conversion */
  struct distortmap_struct *distmap = 0; /* Distortion map structure (or 0 for none) */

  int ncomplete = 0, nnull = 0;
  int status = 0;
  int safestatus = 0;

  /* Print banner */
  banner(parms);

  /* -------------- */
  headas_chat(5, "...opening input file %s...\n", parms->infile);

  fits_open_file(&infile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
    goto CLEANUP;
  }
  
  status = locate_caldb_files(parms, infile);
  if (status) {
    fprintf(stderr, "ERROR: could not find CALDB files\n");
    return status;
  }


  /* Read distortion map */
  if (parms->distfile[0] && distmap == 0) {
    if (read_distortmap(parms->distfile, &(distmap), &status)) {
      fprintf(stderr, "ERROR: could not read distortion map %s\n",
	      parms->distfile);
      goto CLEANUP;
    }
  }

  attfile = openAttFile(parms->attitude);
  if (attfile == 0) {
    fprintf(stderr, "ERROR: could not open attitude file %s\n",
	    parms->attitude);
    goto CLEANUP;
  }
  teldef = readTelDef(parms->teldef);
  if (teldef == 0) {
    fprintf(stderr, "ERROR: could not read teldef file %s\n",
	    parms->teldef);
    goto CLEANUP;
  }


  /* Copy input to output file */
  headas_chat(5, "...creating output file...\n");
  headas_clobberfile(parms->outfile);
  fits_create_file(&outfile, parms->outfile, &status);
  fits_copy_file(infile, outfile, 1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not create %s\n", parms->outfile);
    goto CLEANUP;
  }
  

  /* Close the input; we don't need it any more */
  fits_close_file(infile, &status);
  infile = 0;   status = 0;

  fits_movnam_hdu(outfile, BINARY_TBL, "TRIG_IMGSTAT", 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: the TRIG_IMGSTAT extension does not exist\n");
    goto CLEANUP;
  }

  /* Get existing column numbers */
  headas_chat(5, "...reading input file properties...\n");
  fits_get_num_cols(outfile, &ncols, &status);
  fits_get_num_rows(outfile, &nrows, &status);
  fits_get_colnum(outfile, CASEINSEN, "TIME", &timecol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mStartForeSec", &tstart1col, &status);
  fits_get_colnum(outfile, CASEINSEN, "mStartForeSubsec", &tstart2col, &status);
  fits_get_colnum(outfile, CASEINSEN, "mEndForeSec", &tstop1col, &status);
  fits_get_colnum(outfile, CASEINSEN, "mEndForeSubsec", &tstop2col, &status);
  fits_get_colnum(outfile, CASEINSEN, "mForeDur", &expocol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mThetaOrI", &thetacol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mPhiOrJ",   &phicol,   &status);
  fits_get_colnum(outfile, CASEINSEN, "mSourcesFound", &nsrccol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mCatNum", &catnumcol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mStrength", &strengthcol, &status);
  fits_get_colnum(outfile, CASEINSEN, "mLocalSigma", &noisecol, &status);
  fits_get_coltype(outfile, thetacol, &typecode, &nvect, &width, &status);

  if (status) {
    fprintf(stderr, "ERROR: could not read column information of %s\n",
	    parms->outfile);
    goto CLEANUP;
  }

  fits_write_errmark();
  fits_read_key(outfile, TLOGICAL, "IMGTCORR", &imgtcorr, 0, &status);
  if (status) {
    status = 0;
    imgtcorr = 0;
  }
  fits_get_colnum(outfile, CASEINSEN, "DSP_TIME", &dsp_timecol, &status);
  status = 0;
  fits_clear_errmark();

  /* Create RA/Dec columns with same vector dimensions as Theta/Phi */
  {
    char tformi[80], tforme[80];
    int ncols1, i, j;
    /* NOTE: do not change the order of this array, or add/subtract elements, without also
       changing the switch() statement below */
    char *ttype[] = {"RA_OBJ", "DEC_OBJ", "RA_CENT", "DEC_CENT", "PA_CENT", "DSP_TIME",
		     "IMX",    "IMY",     "SNR"    , "EXPOSURE",  "CATNUM" };
    char *tform[] = {    "nE",      "nE",      "1D",       "1D",      "1D",       "1D",
			 "nE",      "nE",      "nE",       "1D",      "nI"};
    char *units[] = {   "deg",     "deg",     "deg",      "deg",     "deg",        "s",
 			    0,         0,         0,       "s" ,         0};
    char *tdisp[] = { "F10.4",   "F10.4",   "F10.4",    "F10.4",   "F10.4",    "F17.6",
		      "F10.5",   "F10.5",   "F10.3",    "F11.3",         0};
    char *comments[] = {
      "R.A. of BAT imaging position",
      "Dec. of BAT imaging position",
      "Approx. R.A. of boresite at image center-time",
      "Approx. Dec. of boresite at image center-time",
      "Approx. Roll of boresite at image center-time",
      "DSP telemetry time",
      "IMX Tangent plane coordinate",
      "IMY Tangent plane coordinate",
      "Signal to noise ratio",
      "Exposure time of image",
      "Catalog identification number"
    };


    ncols1 = sizeof(ttype)/sizeof(ttype[0]);

    /* Do not create DSP_TIME column if it has already been created */
    null_dsp_time = 1;
    if (imgtcorr != 0 && dsp_timecol > 0) {
      ttype[6] = 0;
      /* Do not write null values to DSP_TIME column, since it has already been
	 filled with good values. */
      null_dsp_time = 0;
    }
    if (imgtcorr != 0 && dsp_timecol <= 0) {
      fprintf(stderr, 
	"WARNING: IMGTCORR keyword was set but DSP_TIME column was not present\n"
	"         DSP_TIME column will be filled with NULL values\n");
    }

    /* Compact the column list, to remove blank columns */
    for (i=0, j=0; i < ncols1; i++) {
      if (ttype[0] == 0) continue;
      ttype[j] = ttype[i];
      tform[j] = tform[i];
      units[j] = units[i];
      tdisp[j] = tdisp[i];
      comments[j] = comments[i];
      j ++;
    }
    ncols1 = j;

    /* Set the TFORMn value for vector columns: nD -> 20E and so on */
    /* Note that what appears as 'D'ouble precision above actually becomes 
       'E' single precision */
    sprintf(tforme, "%ldE", nvect); /* Single precision */
    sprintf(tformi, "%ldI", nvect); 
    for (i=0; i<ncols1; i++) {
      if (strcmp(tform[i],"nD") == 0) { tform[i] = tforme; }
      if (strcmp(tform[i],"nE") == 0) { tform[i] = tforme; }
      if (strcmp(tform[i],"nI") == 0) { tform[i] = tformi; }
    }

    /* Insert all of the columns at position NCOLS+1 */
    headas_chat(5, "...inserting %d columns...\n", ncols1);
    fits_insert_cols(outfile, ncols+1, ncols1, ttype, tform, &status);
    headas_chat(5, "   (done)\n");

    /* Collect the column numbers and write TUNIT/TTYPE keywords */
    for (i=0; i<ncols1; i++) {
      char keyname[80];
      int thiscol;

      ncols ++;
      thiscol = ncols;

      switch (i) {
      case 0: racol = thiscol; break;
      case 1: deccol = thiscol; break;
      case 2: raccol = thiscol; break;
      case 3: decccol = thiscol; break;
      case 4: rollccol = thiscol; break;
      case 5: dsp_timecol = thiscol; break;
      case 6: imxcol = thiscol; break;
      case 7: imycol = thiscol; break;
      case 8: snrcol = thiscol; break;
      case 9: oexpocol = thiscol; break;
      case 10: ocatnumcol = thiscol; break;
      }

      headas_chat(5, "...creating %s column (%d); status=%d...\n",
		  ttype[i], thiscol, status);

      if (units[i]) {
	sprintf(keyname, "TUNIT%d", thiscol);
	fits_update_key(outfile, TSTRING, keyname, units[i], 
			"physical unit of field", &status);
      }
      if (tdisp[i]) {
	sprintf(keyname, "TDISP%d", thiscol);
	fits_update_key(outfile, TSTRING, keyname, tdisp[i], 
			"display format of field", &status);
      }
      sprintf(keyname, "TTYPE%d", thiscol);
      fits_update_key(outfile, TSTRING, keyname, ttype[i],
		      comments[i], &status);
      headas_chat(5, "...creating TUNIT='%s' and TTYPE='%s' keywords; status=%d...\n",
		  units[i], ttype[i], status);
    }

    if (ocatnumcol) {
      char keyname[80];
      int neg_one = -1;
      sprintf(keyname, "TNULL%d", ocatnumcol);
      fits_update_key(outfile, TINT, keyname, &neg_one,
		      "data null value", &status);
    }
  }

  /* Make sure the internal representation of the file structure is updated */
  fits_set_hdustruc(outfile, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not modify structure of %s\n", 
	    parms->outfile);
    goto CLEANUP;
  }

  nelt = nrows*nvect;

  /* Loop through each row! */
  for (irow=1; irow <= nrows; irow++) {
    double time, expo, distance = 1;
    int nsrc;
    QUAT att, corrected;
    ROTMATRIX rot;
    EULER e;
    double detunit[3], skyunit[3], lonlat[2];
    double ra, dec, roll;
    double tstart1, tstart2, tstop1, tstop2;
    double tcent;
    int bad_att_flag = 0;  /* Bad attitude flag: 1=bad */

    /* Default all values to NULL */
    fits_write_col_null(outfile, racol,  irow, 1, nvect, &status);
    fits_write_col_null(outfile, deccol, irow, 1, nvect, &status);
    fits_write_col_null(outfile, imxcol, irow, 1, nvect, &status);
    fits_write_col_null(outfile, imycol, irow, 1, nvect, &status);
    fits_write_col_null(outfile, snrcol, irow, 1, nvect, &status);
    fits_write_col_null(outfile, ocatnumcol, irow, 1, nvect, &status);
    if (null_dsp_time) {
      fits_write_col_null(outfile, dsp_timecol, irow, 1, 1, &status);
    }

    /* Read TIME, EXPOSURE and number of sources detected */
    fits_read_col(outfile, TDOUBLE, timecol, irow, 1, 1, 0, &time, 0, &status);
    fits_read_col(outfile, TDOUBLE, tstart1col, irow, 1, 1, 0, &tstart1, 0, &status);
    fits_read_col(outfile, TDOUBLE, tstart2col, irow, 1, 1, 0, &tstart2, 0, &status);
    fits_read_col(outfile, TDOUBLE, tstop1col, irow, 1, 1, 0, &tstop1, 0, &status);
    fits_read_col(outfile, TDOUBLE, tstop2col, irow, 1, 1, 0, &tstop2, 0, &status);
    fits_read_col(outfile, TDOUBLE, expocol, irow, 1, 1, 0, &expo, 0, &status);
    fits_read_col(outfile, TINT,    nsrccol, irow, 1, 1, 0, &nsrc, 0, &status);
    if (status) break;

    if (imgtcorr) {
      /* Do not re-correct the times. */
      time = time;
    } else {
      /* DSP telemetry time is taken from the TIME column */
      double dsp_time = time;
      /* Compute image start time from foreground interval; */
      time = tstart1 + tstart2;

      /* Save these values */
      fits_write_col(outfile, TDOUBLE, timecol, irow, 1, 1, &time, &status);
      fits_write_col(outfile, TDOUBLE, dsp_timecol, irow, 1, 1, &dsp_time, &status);
    }
    fits_write_col(outfile, TDOUBLE, oexpocol, irow, 1, 1, &expo, &status);

    headas_chat(5, "...row %d: time=%f expo=%f nsrc=%d...\n",
		irow, time, expo, nsrc);
    nnull += (nvect - nsrc);

    /* Interpolate to the center-time of the image.  This is not as
       good as using a robust estimator, but it will solve most of the
       settling problems. */
    tcent = time + expo/2;
    findQuatInAttFile(attfile, &att, tcent);
    if (fabs(tcent-attfile->search_time0) < parms->maxtimegap) {
      bad_att_flag = 0;
      headas_chat(5, "   (found attitude file row %d near time %f)\n",
		  attfile->search_row, attfile->search_time0);
    } else {
      bad_att_flag = 1;
      headas_chat(2, 
		  "NOTE: missing attitude data for row %d\n"
		  "  Image center at MET %f, attitude data at MET %f (row %d)\n",
		  irow, tcent, attfile->search_time0, attfile->search_row);
    }      

    /* Compute roll angle */
    corrected = att;
    invertQuatInPlace(&corrected);
    convertQuatToRotMatrix(&rot, &corrected);
    convertRotMatrixToEuler(&e, &rot);
    roll = e.theta * 180/M_PI;
    while (roll < 0) roll += 360.0;
    fits_write_col(outfile, TDOUBLE, rollccol, irow, 1, 1, &(roll), &status);

    /* Compute SC X pointing direction */
    detunit[0] = 1.0; detunit[1] = 0; detunit[2] = 0.0;
    applyRotMatrixToVector(&rot, skyunit, detunit);

    ra  = 0;
    dec = asin(skyunit[2]) * RTOD;
    if (fabs(skyunit[2]) > 0) {
      ra = atan2(skyunit[1], skyunit[0]) * RTOD;
    }
    while (ra < 0) ra += 360.0;
    while (ra > 360) ra -= 360;
    headas_chat(5, "...Pointing (ra,dec,roll) = (%f,%f,%f)...\n",
		ra, dec, roll);
    if (! bad_att_flag ) {
      fits_write_col(outfile, TDOUBLE, raccol, irow, 1, 1, &(ra), &status);
      fits_write_col(outfile, TDOUBLE, decccol, irow, 1, 1, &(dec), &status);
    } else {
      /* If the attitude is bad then explicitly write NULL values to
	 each of the main central pointing values */
      double nulval = 1e307;
      fits_write_colnull(outfile, TDOUBLE, raccol, irow, 1, 1, &(nulval), &(nulval), &status);
      fits_write_colnull(outfile, TDOUBLE, decccol, irow, 1, 1, &(nulval), &(nulval), &status);
      fits_write_colnull(outfile, TDOUBLE, rollccol, irow, 1, 1, &(nulval), &(nulval), &status);
    }

    /* Compute transformation matrix detector -> sky */
    productOfQuats(&corrected, &att, teldef->alignment->q_inverse);
    invertQuatInPlace(&corrected);
    convertQuatToRotMatrix(&rot, &corrected);

    /* No need to compute source-specific coordinates */
    if (nsrc == 0) continue;

    /* Loop through each source in each row */
    for (jsrc=1; jsrc <= nsrc; jsrc++) {
      double strength = 0, noise = 0, snr = 0;
      double theta = 0, phi = 0;
      double imx = 0, imy = 0;
      double zero = 0;
      int izero = -999, catnum = -999;
      
      fits_read_col(outfile, TINT,    catnumcol,irow, jsrc, 1, &izero, &catnum, 0, &status);
      fits_read_col(outfile, TDOUBLE, thetacol, irow, jsrc, 1, 0, &theta, 0, &status);
      fits_read_col(outfile, TDOUBLE, phicol,   irow, jsrc, 1, 0, &phi, 0, &status);
      fits_read_col(outfile, TDOUBLE, strengthcol,irow,jsrc,1, 0, &strength, 0, &status);
      fits_read_col(outfile, TDOUBLE, noisecol, irow, jsrc, 1, &zero, &noise, 0, &status);
      if (status) break;

      /* Longitude and latitude in degrees */
      lonlat[0] = RTOD * phi;
      lonlat[1] = RTOD * theta;

      /* Convert to detector unit vector [apparent] */
      detunit[0] = 0; detunit[1] = 0; detunit[2] = 0;
      bat_coordver(BCFSWLONLAT, detunit, lonlat, &distance);
      imx = detunit[0]/detunit[2];
      imy = detunit[1]/detunit[2];

      /* Map apparent detector coordinates to true detector
	 coordinates using distortion map. */
      if (distmap) {
	double imx_new = -999, imy_new = -999;
	distortmap_coco1(distmap, 
			 imx, imy,
			 &imx_new, &imy_new,
			 APP_TO_TRUE);
	/* Convert from tangent plane coordinates to cartesian */
	if ((imx_new != -999) && (imy_new != -999)) {
	  detunit[0] = (imx_new)*detunit[2];
	  detunit[1] = (imy_new)*detunit[2];
	}
      }
      /* 
      headas_chat(5, "   (detector unit=[%f %f %f] length=%f)\n",
		  detunit[0], detunit[1], detunit[2], 
		  sqrt(detunit[0]*detunit[0] +
		       detunit[1]*detunit[1] +
		       detunit[2]*detunit[2]));
      */

      /* Compute sky unit vector */
      applyRotMatrixToVector(&rot, skyunit, detunit);

      ra  = 0;
      dec = asin(skyunit[2]) * RTOD;
      if (fabs(skyunit[2]) > 0) {
	ra = atan2(skyunit[1], skyunit[0]) * RTOD;
      }
      
      /* Range reduction on the RA */
      while (ra < 0)   ra += 360;
      while (ra > 360) ra -= 360;

      headas_chat(5, 
		  "   (writing row %d element %d: "
		  "(imx,imy) = (%f,%f) (ra,dec) = (%f,%f))\n",
		  irow, jsrc, imx, imy, ra, dec);
      if (! bad_att_flag ) {
	fits_write_col(outfile, TDOUBLE, racol,  irow, jsrc, 1, &ra, &status);
	fits_write_col(outfile, TDOUBLE, deccol, irow, jsrc, 1, &dec, &status);
      }
      fits_write_col(outfile,   TDOUBLE, imxcol, irow, jsrc, 1, &imx, &status);
      fits_write_col(outfile,   TDOUBLE, imycol, irow, jsrc, 1, &imy, &status);

      if (noise > 0) {
	snr = strength / noise;
	fits_write_col(outfile, TDOUBLE, snrcol, irow, jsrc, 1, &snr, &status);
      }
      if (catnum != izero) {
	fits_write_col(outfile, TINT,ocatnumcol, irow, jsrc, 1, &catnum, &status);
      }

      ncomplete ++;
    }
      
    if (status) break;
    
  }    
  if (status) {
    fprintf(stderr, "ERROR: processing error while performing conversion\n");
    goto CLEANUP;
  }

  imgtcorr = 1;
  fits_update_key(outfile, TLOGICAL, "IMGTCORR", &imgtcorr, 
		  "TIME column corrected by batimgstatpos?", &status);
  status = HDpar_stamp(outfile, 0, &status);
    

  /* -------------- */
  /* Task cleanup */
  CLEANUP:
  safestatus = 0;
  if (outfile) fits_close_file(outfile, &safestatus);
  safestatus = 0;
  if (infile) fits_close_file(infile, &safestatus);
  safestatus = 0;
  if (attfile)   closeAttFile(attfile);
  safestatus = 0;
  if (teldef) destroyTelDef(teldef);


  summary(parms, ncomplete, nnull);
  return status;
}

int batimgstatpos(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batimgstatpos_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }
  
  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batimgstatpos_work(&parms);

}
