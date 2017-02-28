/*  temp2gain.c                                                 */
/*     created by Y.Ikebe                                       */
/*           21/06/94 H.Kubo   temp2gain_table introduced       */
/*     v3.5  11/07/94 H.Kubo   read_from_fits  introduced       */
/*     v3.6   6/09/94 H.Kubo   coefficient of analytic function */
/*                                 moved to 1st extension       */
/*     v4.1  11/12/96 Y.Ishisaki                                */
/*                             LDHIT counting rate dependence   */
/*                             gain map compensation function   */
/*     v4.2  28/02/97 Y.Ishisaki                                */
/*                   replace FRF read routine with ascatool 3.1 */
/*                   multiply norm factor in outfitssub()       */
/*     v4.3  1998-07-22 ZH     
 *                   switched to native cfitsio
 *     v4.4  1999-10-12 Ning Gan     
 *                   Final Ftoolization
 *                   Created  a htemp2gain.c
 *                   Removed the subdirectory asca and changed 
 *                   the corresponding include statements. 
 *     v4.5  2000-01-26 Ning Gan 
 *                   Bug fix from Ken Ebisawa.    
 *     v4.6  2000-03-17 ZH
 *                   fix stderr/stdout and misc
 *****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "yasca.h"
#include "gis.h"
#include "ytlm.h"
#include "ylog.h"

static char pname[] = "temp2gain";
static char version[] = "V4.6";

#define  FMAX  256
#define  DEFAULT_PFILE "ftools.par"

typedef struct {
  double ts, te;
  float k, g, kerr, gerr;
} GAINHIST;

typedef struct {
  double ts, te, *coef, *err;
} GMAPCORR;

/*** 11/07/94 H.Kubo ***/
#define TABLE_TYPE 1
#define FUNC_TYPE  2
#define FUNC_EFFTIME   7776000
#define TABLE_EFFTIME 15552000

static double off_coeff[2], off_offset[2], off_norm[2], off_tau[2];
static double gain_coeff[2], temp_offset[2], gti_sta, gti_end;
static double ld_grad[2], ld_offs[2], ld_thrs[2];
static double step_time;
static int ldh_flag;

/*** 21/06/94 H.Kubo ***/
static int gh_type;
static long gh_nrows;
static GAINHIST *gainhist[2];

/*** 21/06/96 Y.Ishisaki ***/
static long gm_dim;
static long gm_nrows;
static GMAPCORR *gmapcorr[2];

/*** 21/06/94 H.Kubo temp2peak -> temp2peak_func ***/
static double
temp2peak_func(double ascatime, double ondo, int g23)
{
  double peak, offset;

  ascatime = ascatime * 1e-7;
  offset = off_coeff[g23] * ascatime + off_offset[g23]
    + off_norm[g23] * exp( -off_tau[g23] * ascatime);
  peak = offset * ( gain_coeff[g23] * (ondo - temp_offset[g23]) + 1.0 );
  return peak;
}

static void
set_ph_err_time(int n, double ondo, GAINHIST *ghist, double *t, 
		double *ph, double *err)
{
  int i;
  for (i = 0; i < n; i++) {
    double z;
    t[i] = ( ghist[i].ts + ghist[i].te ) / 2;
    ph[i] = ghist[i].k * ondo + ghist[i].g;
    z = ghist[i].kerr * ondo;
    err[i] = sqrt(z*z + ghist[i].gerr*ghist[i].gerr);
  }
}

static void
line_fit_data(int n, double *x, double *y, double *sig, double *param)
{
  int i;
  double a[3][3], b[3];
  double w, det;
	
  a[1][1] = a[1][2] = a[2][1] = a[2][2] = b[1] = b[2] = 0.0;
  for (i = 0; i < n; i++) {
    if ( 0.0 == sig[i] ) {
      param[0] = y[i];
      param[1] = 0.0;
      return;
    }
    w = 1.0 / ( sig[i] * sig[i] );
    a[1][1] = a[1][1] + w;
    a[1][2] = a[1][2] + x[i] * w;
    a[2][1] = a[1][2];
    a[2][2] = a[2][2] + x[i]*x[i] * w;
    b[1] =   b[1] + y[i] * w;
    b[2] =   b[2] + x[i]*y[i] * w;
  }
  det = a[1][1]*a[2][2] - a[1][2]*a[2][1];
  param[0] = (  a[2][2]*b[1] - a[1][2]*b[2] ) / det;
  param[1] = ( -a[2][1]*b[1] + a[1][1]*b[2] ) / det;
  /*
    error[0] =    sqrt(a[2][2]/det);
    error[1] =    sqrt(a[1][1]/det);
  */
}

static double
temp2peak_table(int i, int nrow, GAINHIST *ghist, double ascatime, double ondo)
{
  int n;
  double t[3], ph[3], err[3], param[2];
  if ( 0 == i ) {
    n = 2;
    i++;
  } else {
    n = 3;
    if ( nrow <= i + 1 ) i--;
  }
  set_ph_err_time(n, ondo, &ghist[i-1], t, ph, err);
  line_fit_data(n, t, ph, err, param);
  return param[1] * ascatime + param[0];
}

static double
temp2peak(int g23, double ascat, double ondo)
{
  static int now[2] = { -1, -1 };
  GAINHIST *ghist;
  int i, method;
  static int func_first = 1;
  double peak;
  
  method = gh_type;
  if ( TABLE_TYPE != method && FUNC_TYPE != method ) {
    if ( gti_sta < ascat && ascat < gti_end ) {
      method = FUNC_TYPE;
    } else {
      method = TABLE_TYPE;
    }
  }
  if ( FUNC_TYPE == method ) {
    peak = temp2peak_func(ascat, ondo, g23);
    if (( ascat < gti_sta || gti_end < ascat) && (1 == func_first)) {
      func_first = 0;
      if ( (ascat - gti_end) > FUNC_EFFTIME || (gti_sta - ascat) > FUNC_EFFTIME ) {
	fprintf(stderr,"Error: the time region of this FRF exceeds the region with parameters effective,\nyou are recommended to use Linear Interpolation method\n");
	exit(0);
      } else {
	fprintf(stderr,"Warning: parameters are valid between %.2lf and %.2lf(ascatime)\n",gti_sta,gti_end);
      }
    }
    return peak;
  }
  i = now[g23];
  if ( i < 0 ) i = 0;
  ghist = gainhist[g23];
  if ( ascat < ghist[i].ts ) {
    while ( 0 < i ) {
      if ( ghist[i].ts <= ascat ) break;
      i--;
    }
    if ( ascat < ghist[i].ts && now[g23] != i ) {
      fprintf(stderr, "Warning: data (%lf) exceeds the calibrated time region\n", ascat);
    }
  } elif ( ghist[i].te <= ascat ) {
    for (i = i + 1; i < gh_nrows; i++) {
      if ( ascat < ghist[i].ts ) break;
    }
    i--;
    if ( ghist[i].te <= ascat && now[g23] != i ) {
      if ((ascat - ghist[i].te) > TABLE_EFFTIME) {
	fprintf(stderr,"Error: the time region of this FRF exceeds the region with parameters effective,\nyou are recommended to update the calibration file\n");
	exit(0);
      } else {
	fprintf(stderr,"Warning: ascatime of the latest parameters for GIS%d in the calibration file is %.2lf\n",g23+2,ghist[i].te);
      }
    }
  }
  now[g23]=i;
  peak = temp2peak_table(i, gh_nrows, ghist, ascat, ondo);
  return peak;
}


static void
get_gmap_coef(int g23, double ascat, double *calc_coef)
{
  static int now[2] = { -1, -1 };
  GMAPCORR *gcorr;
  int i, j, k, n;
  double t[3], coef[3], err[3], param[2];
	
  i = now[g23];
  if ( i < 0 ) i = 0;
  gcorr = gmapcorr[g23];
  if ( ascat < gcorr[i].ts ) {
    while ( 0 < i ) {
      if ( gcorr[i].ts <= ascat ) break;
      i--;
    }
    if ( ascat < gcorr[i].ts && now[g23] != i ) {
      fprintf(stdout, "Warning: data (%lf) exceeds the calibrated time region\n", ascat);
    }
  } elif ( gcorr[i].te <= ascat ) {
    for (i = i + 1; i < gm_nrows; i++) {
      if ( ascat < gcorr[i].ts ) break;
    }
    i--;
    if ( gcorr[i].te <= ascat && now[g23] != i ) {
      fprintf(stdout,"Warning: ascatime of the latest parameters for GIS%d in the calibration file is %.2lf\n", g23+2, gcorr[i].te);
    }
  }
  now[g23] = i;
  
  for (i = 0; i <= gm_dim; i++) {
    if ( 0 == now[g23] ) {
      n = 2;
      k = 0;
    } else {
      n = 3;
      k = now[g23] - 1;
      if ( gm_nrows <= now[g23] + 1 ) k--;
    }
    for (j = k; j < k+n; j++) {
      t[j-k] = ( gcorr[j].ts + gcorr[j].te ) / 2;
      coef[j-k] = gcorr[j].coef[i];
      err[j-k] = gcorr[j].err[i];
    }
    line_fit_data(n, t, coef, err, param);
    calc_coef[i] = param[1] * ascat + param[0];
  }
}

static void
addnull(char *string)
{
  int i,end;
  end = 0;
  i=0;
  for(;;) {
    if( *(string+i) == ' ' ) {
      *(string+i) = '\0';
      break;
    }
    i++;
  }
}


void printerror(int status)
{
  /*****************************************************/
  /* Print out cfitsio error messages and exit program */
  /*****************************************************/
 
  char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG];
  
  if (status)
    fprintf(stderr, "\n*** Error occurred during program execution ***\n");
  
  fits_get_errstatus(status, status_str);   /* get the error description */
  fprintf(stderr, "\nstatus = %d: %s\n", status, status_str);
  
  /* get first message; null if stack is empty */
  if ( fits_read_errmsg(errmsg) ) 
    {
      fprintf(stderr, "\nError message stack:\n");
      fprintf(stderr, " %s\n", errmsg);
      
      while ( fits_read_errmsg(errmsg) )  /* get remaining messages */
	fprintf(stderr, " %s\n", errmsg);
    }
  
  exit( status );       /* terminate the program, returning error status */
}


static void
get_date(char *fitsFile, char *datekey, char *date)
{
  char comment[80];
  int block, hdutype;
  int rwmode = 0;
  int status = 0;

  fitsfile *fin;

  if (fits_open_file (&fin,  fitsFile, READONLY, &status) )
    printerror(status);

  if (fits_movrel_hdu(fin, 1, &hdutype, &status))
    printerror(status);

  if (fits_read_key_str (fin, datekey, date, comment, &status)) {
    strcpy(date, "NONE");
    fits_clear_errmsg();
    status =0;
  }

  if ( fits_close_file(fin, &status) )
    printerror(status);
}


/* Write keywords to FITS header */
static void
write_header(
  fitsfile *fout, char *filename, double tstart_asca, double tstop_asca,
  char *date_obs, char *time_obs, char *date_end, char *time_end,
  int ghversion, int *status)
{
  static int decimals = 10;
	
  if (fits_write_key_str (fout,"TELESCOP","ASCA","Telescope (mission) name", status)) printerror(*status);

  if (fits_write_key_str (fout, "INSTRUME", "GIS", "Instrument name", status)) printerror(*status);

  if (fits_write_key_str (fout, "FILENAME", filename, "Suggested file name", status)) printerror(*status);

  if (fits_write_date (fout, status)) printerror(*status);

  if (fits_write_key_lng (fout, "VERSION", ghversion, "File version number", status)) printerror(*status);

  if (fits_write_key_str (fout, "CCLS0001", "BCF", "Dataset is Primary Calibration File", status)) printerror(*status);

  if (fits_write_key_str (fout, "CCNM0001", "GAIN_HISTORY", "Type of calibration data", status)) printerror(*status);

  if (fits_write_key_str (fout, "CDTP0001", "DATA",  "Calibration file contains data", status)) printerror(*status);

  if (fits_write_key_str (fout, "CVSD0001", date_obs, "UTC date when calibration should first be used", status)) printerror(*status);

  if (fits_write_key_str (fout, "CVST0001", time_obs, "UTC time when calibration should first be used", status)) printerror(*status);

  if (fits_write_key_str (fout, "CDES0001", "GIS TEMPORAL FILE CONTAINS Fe55/RT CAL AND TEMP/HV (L/H)", "", status)) printerror(*status);

  if (fits_write_key_str (fout, "DATE-OBS", date_obs, "Starting date of observations (yyyy-mm-dd)", status)) printerror(*status);

  if (fits_write_key_str (fout, "TIME-OBS", time_obs, "Staring time of observations (hh:mm:ss)", status)) printerror(*status);

  if (fits_write_key_str (fout, "DATE-END", date_end, "Ending date of observations (yyyy-mm-dd)", status)) printerror(*status);

 if (fits_write_key_str (fout, "TIME-END", time_end, "Ending time of observations (hh:mm:ss)", status)) printerror(*status);

 if (fits_write_key_dbl (fout, "TSTART", tstart_asca, decimals, "Begining time for valid data in this file", status))  printerror(*status);

 if (fits_write_key_dbl (fout, "TSTOP", tstop_asca, decimals, "Ending time for valid data in this file", status))  printerror(*status);
}


/* open-up a fits file for write-out */
static void
open_fits(fitsfile **fout, char *filename, double tstart_asca, 
	  double tstop_asca,
	  char *date_obs, char *time_obs, char *date_end, char *time_end,
	  int ghversion, int *status)
{
  static int simple = 1;
  static int bitpix = 16;
  static int naxis = 0;
  static long naxes[2] = { 1, 1 };
  static int pcount = 0;
  static int gcount = 1;
  static int extend = 1;
  static int group  = 1;
  
  if (fits_create_file (fout,filename,status))
    printerror(*status);

  if (fits_write_grphdr(*fout,simple,bitpix,naxis,naxes,pcount,gcount,
			extend,status)) printerror(*status);

  write_header(*fout,filename,tstart_asca,tstop_asca,
	       date_obs, time_obs, date_end, time_end, ghversion, status);
}

static void
write_gmap_header(fitsfile *fout, double tstart_asca, double tstop_asca, 
		  int *status)
{
  static char *comments[] = {
    "",
    "Header keywords GMAPCiSj provides coefficients for GISj",
    "in gain map compensation function as:",
    "",
    "  new-PI = old-PI / ( GMAPC0 + GMAPC1*r + ... + GMAPCn*r**n ),",
    "",
    "where",
    "",
    "  r**2 = (DETX - DET_XCEN)**2 + (DETY - DET_YCEN)**2 .",
    "",
    "Dimension of the polynominal (= n) is specified by 'GMAPCDIM'.",
    ""
  };

  int g23, i;
  double *calc_coef;

  if (fits_write_key_lng (fout, "GMAPCDIM", gm_dim, "dimension of polynomials for gain map com-func.", status)) printerror(*status);

  if (fits_write_key_fixdbl(fout, "DET_XCEN", 128.5, 1,	"DETECTOR ADDRESS SPACE X CENTER (PIXELS)", status))  printerror(*status);

  if (fits_write_key_fixdbl(fout, "DET_YCEN", 128.5, 1,	"DETECTOR ADDRESS SPACE X CENTER (PIXELS)", status)) printerror(*status);
	
  calc_coef = malloc(sizeof(*calc_coef)*(gm_dim+1));
  if ( NULL == calc_coef ) {
    fprintf(stderr, "Out of memory allocating buffer (dim=%d)\n", gm_dim);
    exit(1);
  }

  for (g23 = 0; g23 < 2; g23++) {
    get_gmap_coef(g23, (tstart_asca + tstop_asca) / 2.0, calc_coef);
    for (i = 0; i <= gm_dim; i++) {
      char keywd[16], comment[80];
      sprintf(keywd, "GMAPC%dS%d", i, g23+2);
      sprintf(comment,
	      "%d-th coeff. of GIS%d gain map compensation-func.",
	      i, g23+2);
      if ( 0.0 == calc_coef[i] ) {
	if (fits_write_key_fixdbl(fout, keywd, calc_coef[i], 1, comment, status)) printerror(*status);
      } else {
	int decimals = 10;

	if (fits_write_key_dbl (fout, keywd, calc_coef[i], decimals, comment, status)) printerror(*status);
      }
    }
  }
  free(calc_coef);
	
  for (i = 0; i < sizeof(comments)/sizeof(*comments); i++) {
    if (fits_write_comment(fout, comments[i], status))
      printerror(*status);
  }
}


/* Create a binary tbl xtention and write header keywords/gamp coefs */
static void
open_ext(fitsfile *fout, char *filename, double tstart_asca, 
	 double tstop_asca,
	 char *date_obs, char *time_obs, char *date_end, char *time_end,
	 int ghversion, int ndata, int *status)
{

#define NCOL 19

  static char extname[] = "GIS_GAIN_HIST";
	
  static char *ttype[NCOL] = {
    "CAL_START", "CAL_STOP", "ACC_TIME",
    "HV_LOW_S2", "HV_HIGH_S2", "TEMP_S2", "FE55_PEAK_S2",
    "RT_PEAK_S2", "COUNTS_S2", "LDH_CPS_S2", "LDH_TIM_S2",
    "HV_LOW_S3", "HV_HIGH_S3", "TEMP_S3", "FE55_PEAK_S3",
    "RT_PEAK_S3", "COUNTS_S3", "LDH_CPS_S3", "LDH_TIM_S3"
  };
	
  static char *tform[NCOL] = {
    "1D", "1D", "1D",
    "1I", "1I", "1E", "1E",
    "1I", "1I", "1E", "1E",
    "1I", "1I", "1E", "1E",
    "1I", "1I", "1E", "1E"
  };
	
  static char *tunit[NCOL] = {
    "ASCA_TIME", "ASCA_TIME", "SEC",
    "ADC", "ADC", "CENTIGRADE", "CHANNEL",
    "CHANNEL", "COUNTS", "C/S", "SEC",
    "ADC", "ADC", "CENTIGRADE", "CHANNEL",
    "CHANNEL", "COUNTS", "C/S", "SEC"
  };
  
  int varidt = 0;
  int tfields = NCOL;
  int nrows = ndata;
  
  if (fits_create_hdu(fout, status)) printerror(*status);

  if (fits_write_btblhdr(fout, nrows, tfields, ttype, tform, tunit, extname, varidt, status)) printerror(*status);
	
  write_header(fout, filename, tstart_asca, tstop_asca,
	       date_obs, time_obs, date_end, time_end, ghversion, status);
	
  write_gmap_header(fout, tstart_asca, tstop_asca, status);
	
#undef NCOL
}

static int
outfitssub(char frffile[FMAX],
	   char histfile[FMAX],
	   char input_name[3][FMAX],
	   char *output_name,
	   double *norm,
	   int ghversion,
	   char *date_obs, char *time_obs,
	   char *date_end, char *time_end,
	   unsigned long nls[3],
	   int  status)
{
  char frfdate[80], histdate[80], history[FMAX+80];
	
  struct {
    double s_asca, e_asca, ac_time;
    struct {
      float temp, peak;
    } s[2];
  } *th;
	
  struct {
    int i;
    unsigned long nlows;
    struct {
      double s_asca, e_asca;
      float cps, tim;
    } *a;
  } ldh[2];
	
  double tstart_asca, tstop_asca, s_date, s_time, s_asca, e_asca;
  int    g23, i, frow, ndata;
  FILE   *fp;

  fitsfile *fout;
  unsigned long nlows = nls[0];
  
  ldh[0].nlows = nls[1];
  ldh[1].nlows = nls[2];
	
  th = malloc(nlows*sizeof(*th));
  
  if( (fp = fopen(input_name[0],"r"))==NULL ) {
    fprintf(stderr,"Error: text file open error\n");
    exit(0);
  }

  for (i = 0; i < nlows; i++) {
    int nc;
    nc = fscanf(fp, "%lf %lf %lf %lf %lf %f %f %f %f",
		&s_date, &s_time,
		&th[i].s_asca, &th[i].e_asca, &th[i].ac_time,
		&th[i].s[0].temp, &th[i].s[0].peak,
		&th[i].s[1].temp, &th[i].s[1].peak);

    if ( 9 != nc ) {
      fprintf(stderr, "Error: table file read error\n");
      exit(0);
    }

    th[i].s[0].peak *= norm[0];
    th[i].s[1].peak *= norm[1];
  }
	
  fclose(fp);
	
  if ( ldh_flag ) {
    for (g23 = 0; g23 < 2; g23++) {
      ldh[g23].a = malloc(sizeof(*ldh[0].a) * ldh[g23].nlows);

      if( (fp = fopen(input_name[1+g23],"r"))==NULL ) {
	fprintf(stderr,"Error: GIS%d text file open error\n", 2+g23);
	exit(0);
      }

      for (i = 0; i < ldh[g23].nlows; i++) {
	int nc;
	nc = fscanf(fp,"%lf %lf %lf %lf %f %f",
		    &s_date, &s_time,
		    &ldh[g23].a[i].s_asca, &ldh[g23].a[i].e_asca,
		    &ldh[g23].a[i].cps,    &ldh[g23].a[i].tim);
	if ( 6 != nc ) {
	  fprintf(stderr,
		  "Error: GIS%d LDHIT table file read error\n", 2+g23);
	  exit(0);
	}
      }      
      fclose(fp);
    }
  }

  tstart_asca = th[0].s_asca;
  tstop_asca = th[nls[0]-1].e_asca;
	
  open_fits(&fout, output_name, tstart_asca, tstop_asca,
	    date_obs, time_obs, date_end, time_end, ghversion, &status);

  ndata = nlows;
  if ( ldh_flag ) {
    ndata = 0;
    i = ldh[0].i = ldh[1].i = 0;
    
    while ( i<nlows && ldh[0].i<ldh[0].nlows && ldh[1].i<ldh[1].nlows ) {
      e_asca = th[i].e_asca;
      for (g23 = 0; g23 < 2; g23++) {
	if ( ldh[g23].a[ldh[g23].i].e_asca < e_asca ) {
	  e_asca = ldh[g23].a[ldh[g23].i].e_asca;
	}
      }
      if ( e_asca == th[i].e_asca ) i++;
      for (g23 = 0; g23 < 2; g23++) {
	if ( e_asca == ldh[g23].a[ldh[g23].i].e_asca ) ldh[g23].i++;
      }
      ndata++;
    }
  }
  
  open_ext(fout , output_name, tstart_asca, tstop_asca,
	   date_obs, time_obs, date_end, time_end, ghversion, ndata, &status);

  get_date(frffile, "DATEF", frfdate);

  sprintf(history, "%s %s: INPUT FRF: '%s'",
	  pname, version, frffile);
  if (fits_write_history (fout, history, &status)) 
    printerror(status);

  sprintf(history, "%s %s: INPUT FRF DATE STAMP: '%s'",
	  pname, version, frfdate);
  if (fits_write_history (fout, history, &status))
    printerror(status);

  get_date(histfile, "DATE", histdate);

  sprintf(history, "%s %s: INPUT HIST_FILE: '%s'",
	  pname, version, histfile);
  if (fits_write_history (fout, history, &status))
    printerror(status);

  sprintf(history, "%s %s: INPUT HIST_FILE DATE STAMP: '%s'",
	  pname, version, histdate);
  if (fits_write_history (fout, history, &status))
    printerror(status);

  frow = 1;
  s_asca = th[0].s_asca;
  i = ldh[0].i = ldh[1].i = 0;

  for (;;) {
    int felem = 1;
    int nelem = 1;
    int col = 1;
    float temp[2], peak[2], ldh_cps[2], ldh_tim[2];
    
    if ( i >= nlows ) break;
    if ( ldh_flag && ldh[0].i >= ldh[0].nlows ) break;
    if ( ldh_flag && ldh[1].i >= ldh[1].nlows ) break;
    
    e_asca = th[i].e_asca;
    if ( ldh_flag ) {
      for (g23 = 0; g23 < 2; g23++) {
	if ( ldh[g23].a[ldh[g23].i].e_asca < e_asca ) {
	  e_asca = ldh[g23].a[ldh[g23].i].e_asca;
	}
      }
    }

    if (fits_write_col_dbl(fout,col++,frow,felem,nelem,&s_asca,&status))
      printerror(status);
    if (fits_write_col_dbl(fout,col++,frow,felem,nelem,&e_asca,&status))
      printerror(status);
    if (fits_write_col_dbl(fout,col++,frow,felem,nelem,&th[i].ac_time,&status))
      printerror(status);
    for (g23 = 0; g23 < 2; g23++) {
      long hvl = 3;
      long hvh = 4;
      float temp = th[i].s[g23].temp;
      float peak = th[i].s[g23].peak;
      float rise_time = 180.0;
      float cal_count = 0.0;
      float ldh_cps = 0.0;
      float ldh_tim = 0.0;
      
      if ( ldh_flag ) {
	ldh_cps = ldh[g23].a[ldh[g23].i].cps;
	ldh_tim = ldh[g23].a[ldh[g23].i].tim;
	if ( ld_thrs[g23] < ldh_cps ) {
	  peak *= ld_offs[g23] + ld_grad[g23] * ldh_cps;
	}
      }

      if(fits_write_col_lng(fout,col++,frow,felem,nelem,&hvl,&status))
	printerror(status);
      if(fits_write_col_lng(fout,col++,frow,felem,nelem,&hvh,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&temp,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&peak,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&rise_time,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&cal_count,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&ldh_cps,&status))
	printerror(status);
      if(fits_write_col_flt(fout,col++,frow,felem,nelem,&ldh_tim,&status))
	printerror(status);
    }
		
    if ( e_asca == th[i].e_asca ) i++;
    if ( ldh_flag ) {
      for (g23 = 0; g23 < 2; g23++) {
	if ( e_asca == ldh[g23].a[ldh[g23].i].e_asca ) ldh[g23].i++;
      }
    }
		
    s_asca = e_asca;
    frow++;
  }
	
  /*
    printf("i=%d, nlows=%d\n", i, nlows);
    printf("ldh[0].i=%d, ldh[0].nlows=%d\n", ldh[0].i, ldh[0].nlows);
    printf("ldh[1].i=%d, ldh[1].nlows=%d\n", ldh[1].i, ldh[1].nlows);
  */

  if (fits_close_file(fout, &status))
    printerror(status);

  return 1;
}


static void
write_tbl_file(FILE *fp,
	       double s_asca, double e_asca, double ac_time,
	       double temp[2], double peak[2])
{
  char s_date[10], s_time[8];
  AtTime at;
  /*  int year;  v4.3 */

  asca2attime(s_asca, &at);

  /*  year = at.yr % 100;  v4.3 */

  sprintf(s_date, "%04d%02d%02d", at.yr, at.mo, at.dy);
  sprintf(s_time, "%02d%02d%02d", at.hr, at.mn, at.sc);

  fprintf(fp,"%s %s %.2lf %.2lf %.0lf %.2lf %.2lf %.2lf %.2lf\n",
	  s_date, s_time, s_asca, e_asca, ac_time,
	  temp[0], peak[0], temp[1], peak[1]);
}

static void
write_ldh_file(FILE *fp, double s_asca, double e_asca, double cps, double tim)
{
  char s_date[10], s_time[8];
  AtTime at;
  /*   int year; */
  asca2attime(s_asca, &at);
  /*  year = at.yr % 100;  v4.3 */
  sprintf(s_date, "%04d%02d%02d", at.yr, at.mo, at.dy);
  sprintf(s_time, "%02d%02d%02d", at.hr, at.mn, at.sc);
  fprintf(fp, "%s %s %.2lf %.2lf %.2lf %.2lf\n",
	  s_date, s_time, s_asca, e_asca, cps, tim);
}


/* this will create temporary .tbl file   v4.3 
 * The file will be used by outfitssub() */
static void
data_dump_telem(char *fn, char tbl[3][FMAX],
		double  merge_sec,
		char *date_obs, char *time_obs,
		char *date_end, char *time_end,
		unsigned long nls[3])
{
  static SF sf;          /* the data of 1 SF is read in this array */
  static GISINFO gi;
  gPHbits *bit;
  gPHevent *gev;
  char megtim[30];
  int i, j, sfn, g23, year;
	
  int frame, temp_initial, data_skip;
  
  long super_frame;
  unsigned long nlows;
	
  double  temp[2], s_asca, e_asca, peak[2];
  double  ascatime0, ascatime;
  double  sf_time0, sf_time, ac_time, ac_temp[2];

  struct {
    FILE *fp;
    int nlows;
    double count, dt, cps, sum, tim, ts;
  } ldh[2];
  
  FILE    *fp;
	
  AtTime at;
	
  if ( sfOpen(fn, NULL) ) {
    fprintf(stderr, "Error: '%s' open failed\n", fn);
    exit(0);
  }

  i  = 0;

  /*------ initialize parameters ------*/
  nlows = 0L;
  s_asca = e_asca = 0.0;
  temp[0] = temp[1] = -9999;
  sf_time = 0.0;
  ac_time = 0.0;
  ac_temp[0] = ac_temp[1] = 0.0;
  ascatime0 = 0.0;
  sf_time0 = 0.0;
  ldh[0].nlows = ldh[1].nlows = 0L;
  ldh[0].sum = ldh[1].sum = 0.0;
  ldh[0].tim = ldh[1].tim = 0.0;
	
  /*=========================================================*/
  super_frame = -1;
  temp_initial = -1;
  if( (fp = fopen(tbl[0],"w")) == NULL )
    {
      fprintf(stderr,"Error: file open error\n");
      exit(0);
    }
  
  if ( ldh_flag ) {
    for (g23 = 0; g23 < 2; g23++) {
      if( (ldh[g23].fp = fopen(tbl[1+g23],"w")) == NULL ) {
	fprintf(stderr,"Error: file open error\n");
	exit(0);
      }
    }
  }
  
  /*------------------- loop until break --------------------*/
  for (;;) {
    int status;
    status = sfGetASCA(&sf, &ascatime);

    if ( QL_END == status ) break;
    if ( status ) {
      fprintf(stderr, "Error: FRF read error\n");
      exit(0);
    }

    asca2attime(ascatime, &at);

    /*    year = at.yr % 100;   v4.3 */
    
    if(super_frame==-1)
      {
	s_asca = ascatime - merge_sec;
	ldh[0].ts = s_asca;
	ldh[1].ts = s_asca;
	
	asca2attime(s_asca, &at);
	/*	year = at.yr % 100;   v4.3 */
			
	sprintf(megtim, "%04d%02d%02d %02d%02d%02d",
		at.yr, at.mo, at.dy, at.hr, at.mn, at.sc);
	
	fprintf(stdout,"Data Start Time is %.02lf (%s)\n" ,s_asca,megtim);
	fprintf(stdout,"Time Margin %.01lf sec included\n",merge_sec);
	
	/* GTI check */
	if (FUNC_TYPE == gh_type && s_asca < gti_sta) {
	  fprintf(stdout,"Warning: data exceeds the calibrated time region\n");
	}

	sprintf(date_obs, "%04d-%02d-%02d", at.yr, at.mo, at.dy);
	sprintf(time_obs, "%02d:%02d:%02d", at.hr, at.mn, at.sc);
      } /* end of first sf */

    super_frame++;
		
    /*-----sync code check-----*/
    if( sfCheck(&sf) ) {
      fprintf(stdout,"Sync error detected in %ld th SF\n",super_frame);
      continue;
    }
    
    switch ( sf[15][32] ) {
    case BITH: sf_time = 2.0; break;
    case BITM: sf_time = 16.0; break;
    case BITL: sf_time = 64.0; break;
    default: sf_time = 0.0;
    }

    /* if bit rate is abnormal, skip */
    if(sf_time==0) continue;

    GISget(&sf, &gi);

    data_skip = ( 1.0 < fabs(ascatime-ascatime0) || sf_time0 != sf_time );
    ascatime0 = ascatime + sf_time;	/* set next expected ascatime */
    sf_time0 = sf_time;

    if ( ldh_flag ) {
      static WD work[2];
      WD new, sub, old;

      for (g23 = 0; g23 < 2; g23++) {
	int j = 0;
	long t = 0;
	old = work[g23];
	ldh[g23].count = 0.0;
	if ( data_skip ) old = gi.gis[g23].moni.LDhit.count[j++];
	while ( j < gi.gis[g23].moni.LDhit.n ) {
	  t += gi.gis[g23].moni.LDhit.dt;
	  new = gi.gis[g23].moni.LDhit.count[j++];
	  sub = new - old;
	  old = new;
	  sub &= ( 1 << gi.gis[g23].moni.LDhit.bits ) - 1;
	  ldh[g23].count += sub * gi.gis[g23].moni.LDhit.scale;
	}
	work[g23] = old;
	ldh[g23].dt = (double)t / g1SEC;;
      }

      for (g23 = 0; g23 < 2; g23++) {
	if ( 1.0 < ldh[g23].dt && 4.0 < ldh[g23].tim ) {
	  double cps = ldh[g23].sum / ldh[g23].tim;
	  double cts = cps * ldh[g23].dt;
	  if ( fabs(cts - ldh[g23].count) > 5.0*sqrt(cts) ) {
	    /* counting rate changed */
	    e_asca = ascatime;
	    write_ldh_file(ldh[g23].fp,
			   ldh[g23].ts, e_asca, cps, ldh[g23].tim);
	    ldh[g23].nlows++;
	    /*printf("%s: cps[%d] = %lf (%lf s)\n",
	      megtim, g23, cps, ldh[g23].tim);*/
	    ldh[g23].sum = 0;
	    ldh[g23].tim = 0;
	    ldh[g23].ts = ascatime;
	  }
	}
	ldh[g23].sum += ldh[g23].count;
	ldh[g23].tim += ldh[g23].dt;
	if ( 0 < ldh[g23].tim ) {
	  ldh[g23].cps = ldh[g23].sum / ldh[g23].tim;
	}
      }
    } /* end-if LDH-flag ( apply gain correction to LDHit ) */

    if( (gi.gis[0].hk.T.cvt != -9999) && (gi.gis[1].hk.T.cvt != -9999) ) {
      /*	  if( method< == 2 )     */
      if( step_time > 0 ) {
	ac_time += sf_time;
	ac_temp[0] += sf_time * gi.gis[0].hk.T.cvt;
	ac_temp[1] += sf_time * gi.gis[1].hk.T.cvt;
	if ( ascatime + sf_time - s_asca >= step_time ) {
	  e_asca = ascatime + sf_time;
	  for (g23 = 0; g23 < 2; g23++) {
	    double ascat = ( s_asca + e_asca ) / 2;
	    temp[g23] = ac_temp[g23] / ac_time;
	    peak[g23] = temp2peak(g23, ascat, temp[g23]);
	  }

	  write_tbl_file(fp, s_asca, e_asca, ac_time, temp, peak);
	  nlows++;
	  s_asca = ascatime;
	  /*-------- initialize again ----------*/
	  ac_time = ac_temp[0] = ac_temp[1] = 0.0;
	}

      } else {   /*  else if( method == 1 )    */

	if ( temp_initial == -1 ) {
	  temp[0] = gi.gis[0].hk.T.cvt;
	  temp[1] = gi.gis[1].hk.T.cvt;
	  temp_initial = 0;
	} else if ( (temp[0] != gi.gis[0].hk.T.cvt) ||
		    (temp[1] != gi.gis[1].hk.T.cvt) ) {
	  e_asca = ascatime;
	  for (g23 = 0; g23 < 2; g23++) {
	    double ascat = ( s_asca + e_asca ) / 2;
	    peak[g23] = temp2peak(g23, ascat, temp[g23]);
	  }

	  write_tbl_file(fp, s_asca, e_asca, ac_time, temp, peak);
	  nlows++;
	  s_asca = ascatime;
	  temp[0] = gi.gis[0].hk.T.cvt;
	  temp[1] = gi.gis[1].hk.T.cvt;
	}
      }
    }
  } /* end of for(;;) loop */
  
  sfClose();
  /*=============================================*/
  e_asca = ascatime + sf_time + merge_sec;
	
  /*  if( method == 2 )  */
  if( step_time > 0 ) {
    if( ac_time != 0 ) {
      temp[0] = ac_temp[0]/ac_time;
      temp[1] = ac_temp[1]/ac_time;
    }
  }
  
  for (g23 = 0; g23 < 2; g23++) {
    double ascat = ( s_asca + e_asca ) / 2;
    peak[g23] = temp2peak(g23, ascat, temp[g23]);
  }

  write_tbl_file(fp, s_asca, e_asca, ac_time, temp, peak);
  nlows++;
  nls[0] = nlows;
  fclose(fp);

  if ( ldh_flag) {
    for (g23 = 0; g23 < 2; g23++) {
      write_ldh_file(ldh[g23].fp,
		     ldh[g23].ts, e_asca, ldh[g23].cps, ldh[g23].tim);
      ldh[g23].nlows++;
      nls[1+g23] = ldh[g23].nlows;
      fclose(ldh[g23].fp);
    }
  }
  
  asca2attime(e_asca, &at);
  /*  year = at.yr%100; v4.3 */
  sprintf(megtim, "%04d%02d%02d %02d%02d%02d",
	  at.yr, at.mo, at.dy, at.hr, at.mn, at.sc);
  fprintf(stdout,"Data End Time is %.02lf (%s)\n", e_asca, megtim);
	
  sprintf(date_end, "%04d-%02d-%02d", at.yr, at.mo, at.dy);
  sprintf(time_end, "%02d:%02d:%02d", at.hr, at.mn, at.sc);
}


/*** 11/07/94 H.Kubo ***/
static int
read_from_fits(char *histfile)
{
  static int exact = 1;
  static struct {
    char *temp, *gain;
    char *coeff, *offset, *norm, *tau;
    char *ld_grad, *ld_offs, *ld_thrs;
  } kynm[2] = {
    {
      "TMP_OFF2", "GAN_COE2",
      "OFF_COE2", "OFF_OFF2", "OFF_NOR2", "OFF_TAU2",
      "LD_GRAD2", "LD_OFF2", "LD_THRS2"
    }, {
      "TMP_OFF3", "GAN_COE3",
      "OFF_COE3", "OFF_OFF3", "OFF_NOR3", "OFF_TAU3",
      "LD_GRAD3", "LD_OFF3", "LD_THRS3"
    }
  };
	
  static struct {
    char *ts, *te, *k, *kerr, *g, *gerr;
  } ttype[2] = {
    {
      "START", "STOP",
      "GRADIENT_S2", "GRADIENT_ERR_S2",
      "INTERCEPT_S2", "INTERCEPT_ERR_S2"
    }, {
      "START", "STOP",
      "GRADIENT_S3", "GRADIENT_ERR_S3",
      "INTERCEPT_S3", "INTERCEPT_ERR_S3"
    }
  };
	
  struct {
    int ts, te, k, kerr, g, gerr;
  } icno[2];

  static struct {
    char *ts, *te, *coef, *err;
  } ttyp2[2] = {
    {
      "START", "STOP", "GMAPCNS2", "GMAPCNS2ERR"
    }, {
      "START", "STOP", "GMAPCNS3", "GMAPCNS3ERR"
    }
  };
	
  struct {
    int ts, te, coef, err;
  } icn2[2];

  int inunit, status;
  int rwmode, block, xtensn, hdutype;
  int tfield, vardat, anyf;
  int frow, felem, nelem, nullval=0;
  char comment[132];
  int i, g23;
  double *coef_buf;

  fitsfile *fin;
  
  status = 0;
  rwmode = 0;
  xtensn = 1;
  felem  = 1;
  nelem = 1;
	
	
  if ( fits_open_file (&fin, histfile, READONLY, &status) ) {
    fprintf(stderr,"Error opening histfile: %s (status=%d)\n",histfile,status);
    exit (1);
  }
    
  /* move to the 1st xtention */  
  if (fits_movrel_hdu(fin, xtensn, &hdutype, &status)) {
    fprintf(stderr,"Error moving to xtention %d (status=%d)\n",xtensn,status);
    exit (1);
  }

  for (g23 = 0; g23 < 2; g23++) {    

    if(fits_read_key_dbl(fin,kynm[g23].temp,&temp_offset[g23],comment,&status))
      printerror(status);
    if(fits_read_key_dbl(fin,kynm[g23].gain,&gain_coeff[g23],comment,&status))
      printerror(status);
    if(fits_read_key_dbl(fin,kynm[g23].coeff,&off_coeff[g23],comment,&status))
      printerror(status);
    if(fits_read_key_dbl(fin,kynm[g23].offset,&off_offset[g23],comment,&status))
      printerror(status);
    if(fits_read_key_dbl(fin,kynm[g23].norm,&off_norm[g23],comment,&status))
      printerror(status);
    if(fits_read_key_dbl(fin,kynm[g23].tau,&off_tau[g23],comment,&status))
      printerror(status);

    if ( ldh_flag ) {
      if(fits_read_key_dbl(fin,kynm[g23].ld_grad,&ld_grad[g23],comment,&status))
	printerror(status);
      if(fits_read_key_dbl(fin,kynm[g23].ld_offs,&ld_offs[g23],comment,&status))
	printerror(status);
      if(fits_read_key_dbl(fin,kynm[g23].ld_thrs,&ld_thrs[g23],comment,&status))
	printerror(status);
      printf("GIS%d LD: grad=%e, offs=%f, thrs=%f\n",
	     g23+2, ld_grad[g23], ld_offs[g23], ld_thrs[g23]);
    }
  }
	
  if (fits_read_key_dbl(fin,"TSTART",&gti_sta,comment,&status)) {
    printerror(status);
  }

  if (fits_read_key_dbl(fin,"TSTOP",&gti_end,comment,&status)) {
    printerror(status);
  }

  if (fits_read_key_lng(fin, "NAXIS2", &gh_nrows, comment, &status)) {
    printerror(status);
  }

  gainhist[0] = malloc(sizeof(*gainhist[0]) * gh_nrows);
  gainhist[1] = malloc(sizeof(*gainhist[1]) * gh_nrows);
  if ( NULL == gainhist[0] || NULL == gainhist[1] ) {
    fprintf(stderr, "Out of memory allocating buffer (nrow=%d)\n", gh_nrows);
    exit(1);
  }
	
  for (g23 = 0; g23 < 2; g23++) {
    if (fits_get_colnum(fin,exact,ttype[g23].ts,&icno[g23].ts,&status))
      printerror(status);
    if (fits_get_colnum(fin,exact,ttype[g23].te,&icno[g23].te,&status))
      printerror(status);
    if (fits_get_colnum(fin,exact,ttype[g23].k,&icno[g23].k,&status))
      printerror(status);
    if (fits_get_colnum(fin,exact,ttype[g23].kerr,&icno[g23].kerr,&status))
      printerror(status);
    if (fits_get_colnum(fin,exact,ttype[g23].g,&icno[g23].g,&status))
      printerror(status);
    if (fits_get_colnum(fin,exact,ttype[g23].gerr,&icno[g23].gerr,&status))
      printerror(status);
  }
	
  for (g23 = 0; g23 < 2; g23++) {
    for (i = 0; i < gh_nrows; i++) {
      frow = i + 1;
      if (fits_read_col_dbl(fin,icno[g23].ts, frow, felem, nelem, nullval,
			    &gainhist[g23][i].ts, &anyf, &status))
	printerror(status);
      if (fits_read_col_dbl(fin,icno[g23].te, frow, felem, nelem, nullval,
			    &gainhist[g23][i].te, &anyf, &status))
	printerror(status);
      if (fits_read_col_flt(fin,icno[g23].k, frow, felem, nelem, nullval,
			    &gainhist[g23][i].k, &anyf, &status))
	printerror(status);
      if (fits_read_col_flt(fin, icno[g23].kerr, frow, felem, nelem, nullval,
			    &gainhist[g23][i].kerr, &anyf, &status))
	printerror(status);
      if (fits_read_col_flt(fin, icno[g23].g, frow, felem, nelem, nullval,
			    &gainhist[g23][i].g, &anyf, &status))
	printerror(status);
      if (fits_read_col_flt(fin, icno[g23].gerr, frow, felem, nelem, nullval,
			    &gainhist[g23][i].gerr, &anyf, &status))
	printerror(status);
    }
  }
  

  /* move to the 2nd xtention -- gain map coeffieients */  
  if (fits_movrel_hdu(fin, xtensn, &hdutype, &status)) {
    fprintf(stderr,"Error moving to xtention %d (status=%d)\n",xtensn,status);
    exit (1);
  }
  
  if (fits_read_key_lng(fin, "GMAPCDIM", &gm_dim, comment, &status)){
    fprintf(stderr,"Error reading GMAPCDIM (status=%d)\n",status);
    exit (1);
  }
  
  if (fits_read_key_lng(fin, "NAXIS2", &gm_nrows, comment, &status)){
    fprintf(stderr,"Error reading GMAPCDIM (status=%d)\n",status);
    exit (1);
  }
  
  gmapcorr[0] = malloc(sizeof(*gmapcorr[0]) * gm_nrows);
  gmapcorr[1] = malloc(sizeof(*gmapcorr[1]) * gm_nrows);
  coef_buf = malloc( 4 * sizeof(*coef_buf) * gm_nrows * (gm_dim+1) );
  if ( NULL == gmapcorr[0] || NULL == gmapcorr[1] || NULL == coef_buf ) {
    fprintf(stderr, "Out of memory allocating buffer (nrow=%d)\n", gm_nrows);
    exit(1);
  }
	
  for (g23 = 0; g23 < 2; g23++) {
    if (fits_get_colnum(fin, exact, ttyp2[g23].ts, &icn2[g23].ts, &status))
      printerror(status);
    if (fits_get_colnum(fin, exact, ttyp2[g23].te, &icn2[g23].te, &status))
      printerror(status);
    if (fits_get_colnum(fin, exact, ttyp2[g23].coef, &icn2[g23].coef, &status))
      printerror(status);
    if (fits_get_colnum(fin, exact, ttyp2[g23].err, &icn2[g23].err, &status))
      printerror(status);
  }

  for (g23 = 0; g23 < 2; g23++) {
    for (i = 0; i < gm_nrows; i++) {
      frow = i + 1;
      gmapcorr[g23][i].coef = &coef_buf[2*(g23*gm_nrows + i)*(gm_dim+1)];
      gmapcorr[g23][i].err = gmapcorr[g23][i].coef + gm_dim + 1;
      if (fits_read_col_dbl(fin, icn2[g23].ts, frow, felem, nelem, nullval,
			    &gmapcorr[g23][i].ts, &anyf, &status))
	printerror(status);
      if (fits_read_col_dbl(fin, icn2[g23].te, frow, felem, nelem, nullval,
			    &gmapcorr[g23][i].te, &anyf, &status))
	printerror(status);
      if (fits_read_col_dbl(fin, icn2[g23].coef, frow, felem, (gm_dim+1), nullval,
			    gmapcorr[g23][i].coef, &anyf, &status))
	printerror(status);
      if (fits_read_col_dbl(fin, icn2[g23].err, frow, felem, (gm_dim+1), nullval,
			    gmapcorr[g23][i].err, &anyf, &status))
	printerror(status);
    }
  }

  if (fits_close_file(fin,&status)) {
    fprintf(stderr,"Error closing fits file (status=%d)\n");
    exit (1);
  }

  return 0;
}


int temp2gain(void)
{
  char frffile[FMAX], outfile[FMAX], tblfile[3][FMAX];
  int  status, ghversion, name_length, clobber;
  unsigned long  nlows[3];
  double norm_fac[2];
  double merge_sec;
  char date_obs[16], time_obs[16], date_end[16], time_end[16];

  /*** 21/06/94 H.Kubo ***/
  char  histfile[FMAX];

  /* v4.3 */
  char leaptable[FMAX];
	
  status = 0;

  fprintf(stdout,"%s %s\n", pname, version);
	
  uclgst_("frffile",frffile,&status,7,FMAX);
  if(status != 0) printerror(status);
  addnull(frffile);

  /**** 21/06/94 H.Kubo ***/
  uclgst_("histfile",histfile,&status,8,FMAX);
  if(status != 0) printerror(status);
  addnull(histfile);

  uclgst_("outfile",outfile,&status,7,FMAX);
  if(status != 0) printerror(status);
  addnull(outfile);
	
  uclgst_("tblfile",tblfile[0],&status,7,FMAX);
  if(status != 0) printerror(status);
  addnull(tblfile[0]);

  uclgst_("g2hfile",tblfile[1],&status,7,FMAX);
  if(status != 0) printerror(status);
  addnull(tblfile[1]);
  
  uclgst_("g3hfile",tblfile[2],&status,7,FMAX);
  if(status != 0) printerror(status);
  addnull(tblfile[2]);

  /* read leapsec.fits: v4.3*/
  uclgst_("leapsec",leaptable,&status,7,FMAX);
  if (status != 0) printerror(status);
  addnull(leaptable);

  /*** 21/06/94 H.Kubo ***/
  uclgsi_("type",&gh_type,&status,4);
  if(status != 0) printerror(status);
  if ( 0 != gh_type && 1 != gh_type && 2 != gh_type ) {
    fprintf(stderr,"Method number is not correct\n");
    exit(0);
  }
  
  uclgsd_("steptime",&step_time,&status,8);
  if(status != 0) printerror(status);
  
  uclgsd_("mergesec",&merge_sec,&status,8);
  if(status != 0) printerror(status);
	
  uclgsd_("nor_fac2",&norm_fac[0],&status,8);
  if(status != 0) printerror(status);

  uclgsd_("nor_fac3",&norm_fac[1],&status,8);
  if(status != 0) printerror(status);
  
  uclgsb_("ldh_flag",&ldh_flag,&status,8);
	
  uclgsi_("version",&ghversion,&status,7);
  if(status != 0) printerror(status);
	
  uclgsb_("clobber",&clobber,&status,7);
  if(status != 0) printerror(status);
	
  if ( clobber ) unlink(outfile);
	
  /* initialize leaptable; results are saved in static variables */
  readLeapTable(leaptable);

  /*** 11/07/94 H.Kubo ***/
  read_from_fits(histfile);

  data_dump_telem(frffile, tblfile, merge_sec,
		  date_obs, time_obs, date_end, time_end, nlows);

  outfitssub(frffile, histfile, tblfile, outfile, norm_fac, ghversion,
	     date_obs, time_obs, date_end, time_end, nlows, status);
	
  fprintf(stdout,"Gain History is written in %s\n",outfile);
	
  return 0;
}
