/*************************************************
      xisRespUtil.c

   Version 0.0   2006.02.09    H. Nakajima

   Version 0.1   2006.02.15    H. Nakajima
	weight RMF parameters using wmap in sprctrum file

   Version 0.2   2006.09.15    Y. ISHISAKI
	use anl_msg_error/info() functions
	use mission time in seek_nearest_time()

   Version 1.0   2006.09.16    Y. ISHISAKI
   	write RMFPARAM, PI_TABLE keywords in xisrspPutMatrixHeader()
	write 'ENERG(0.2-16.0)keV' to CBD10001 in xisrspPut***Header()
	other many changes ...

   Version 1.1   2006.10.16    Y. ISHISAKI
   	xisrsp_seek_nearest() -> xisrsp_seek_valid_row()
	add int rebin to several functions

   Version 1.2   2006.10.26    Y. ISHISAKI
   	HDUCLAS3=DETECTOR for MATRIX, remove HDUCLAS3 for EBOUNDS

   Version 1.3   2007.04.30    Y. ISHISAKI
   	add xisrsp_read_format_version()

   Version 1.4   2012.03.27, 2012.04.21    R. KAIDA, Y. ISHISAKI
        To solve the "Si edge problem", followings are implemented
          - equalize the energy resolutions of main and sub-components.
          - introduce a discontinuous change of ratio among components
            at the Si edge.
	For backward compatibility,
	  - bi_si_edge_mode, fi_si_edge_mode are added to respParam
	  - Above implementation is activated when bi_si_edge_mode=1

****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "anl.h"
#include "fitsio.h"
#include "aste_caldb.h"
#include "xisFitsHeaderUtil.h"
/*
#include "xisEventFitsUtil.h"
#include "xisEditEventFitsUtil.h"
*/
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "xisRespUtil";
static char version[] = "1.4";

#define BISENSOR_ID	1

/****************************************************************************/
static int
getevent_th(int sensor)
{
  return sensor == 1 ? 20 : 50;
}

char *
xisrsp_pname(void)
{
  return pname;
}

char *
xisrsp_version(void)
{
  return version;
}

char *
xisrspGetRespExtensionName(int extnum)
{
  if( extnum == PRIMARY_HDU_ID) {
    return "PRIMARY";
  } else if(extnum == MATRIX_EXTENSION_ID) {
    return MATRIX_EXTENSION_NAME;
  } else if(extnum == EBOUND_EXTENSION_ID) {
    return EBOUND_EXTENSION_NAME;
  }
  return NULL;
}

int
xisrspPutMatrixHeader(fitsfile *fp, char *outfile, energyBin *ebin, int rebin)
{
  char *k, CBD10001[FLEN_VALUE];
  int istat = 0;

  sprintf(CBD10001, "ENERG(%.1f-%.1f)keV",
	ebin->energ_lo[0], ebin->energ_hi[ebin->size-1]);

  istat = xisrspWriteOGIPHeader(fp, outfile, rebin);
  if ( istat ) {
    return istat;
  }

  if (
fits_update_key_str(fp, k="HDUCLAS2", "RSP_MATRIX",
		    "dataset is a spectral response matrix", &istat) ||
fits_update_key_str(fp, k="HDUVERS2", "1.2.0",
		    "Obsolete - for backwards compatibility", &istat) ||
fits_update_key_str(fp, k="HDUCLAS3", "DETECTOR",
		    "Some info transfered to ARF (not FULL)", &istat) ||
fits_update_key_str(fp, k="CCLS0001", "CPF",
		    "Calibration Product File", &istat) ||
fits_update_key_str(fp, k="CCNM0001", "SPECRESP MATRIX",
		    "Type of calibration data", &istat) ||
fits_update_key_str(fp, k="CDTP0001", "DATA",
		    "Data type", &istat) ||
fits_update_key_str(fp, k="CVSD0001", "2000-01-01",
		    "UTC date when file should be first used", &istat) ||
fits_update_key_str(fp, k="CVST0001", "00:00:00",
		    "UTC time when file should be first used", &istat) ||
fits_update_key_str(fp, k="CDES0001", "Energy Redistribution Matrix",
		    "Brief description", &istat) ||
fits_update_key_str(fp, k="CBD10001", CBD10001,
		    "Parameter boundaries", &istat) ||
/*
fits_update_key_str(fp, k="CBD10001", "DETCHANS(4096)",
		    "Parameter boundaries", &istat) ||
fits_update_key_str(fp, k="CBD20001", "CHAN(0-4095)",
		    "Parameter boundaries", &istat) ||
fits_update_key_str(fp, k="CBD30001", "CHANTYPE(\"PI\")",
		    "Parameter boundaries", &istat) ||
*/
       0 ) {
    anl_msg_error("\
%s-%s: fits_update_key('%s') failed (%d)\n", pname, version, k, istat);
    return istat;
  }

  return istat;
}

int
xisrspPutEboundHeader(fitsfile *fp, char *outfile, energyBin *ebin, int rebin)
{
  char *k, CBD10001[FLEN_VALUE];
  int istat = 0;

  sprintf(CBD10001, "ENERG(%.1f-%.1f)keV",
	ebin->energ_lo[0], ebin->energ_hi[ebin->size-1]);

  istat = xisrspWriteOGIPHeader(fp, outfile, rebin);
  if ( istat ) {
    return istat;
  }

  if (
fits_update_key_str(fp, k="HDUCLAS2", "EBOUNDS",
		    "nominal energies of PHA chan boundaries", &istat) ||
fits_update_key_str(fp, k="HDUVERS2", "1.2.0",
		    "Obsolete - for backwards compatibility", &istat) ||
fits_update_key_str(fp, k="CCLS0001", "CPF",
		    "Calibration Product File", &istat) ||
fits_update_key_str(fp, k="CCNM0001", "EBOUNDS",
		    "Type of calibration data", &istat) ||
fits_update_key_str(fp, k="CDTP0001", "DATA",
		    "Data type", &istat) ||
fits_update_key_str(fp, k="CVSD0001", "2000-01-01",
		    "UTC date when file should be first used", &istat) ||
fits_update_key_str(fp, k="CVST0001", "00:00:00",
		    "UTC time when file should be first used", &istat) ||
fits_update_key_str(fp, k="CDES0001", "Energy boundaries of spectral bins",
		    "Brief description", &istat) ||
fits_update_key_str(fp, k="CBD10001", CBD10001,
		    "Parameter boundaries", &istat) ||
/*
fits_update_key_str(fp, k="CBD10001", "DETCHANS(4096)",
		    "Parameter boundaries", &istat) ||
fits_update_key_str(fp, k="CBD20001", "CHAN(0-4095)",
		    "Parameter boundaries", &istat) ||
fits_update_key_str(fp, k="CBD30001","CHANTYPE(\"PI\")",
		    "Parameter boundaries", &istat) ||
*/
       0 ) {
    anl_msg_error("\
%s-%s: fits_update_key('%s') failed (%d)\n", pname, version, k, istat);
    return istat;
  }

  return istat;
}

int
xisrspWriteOGIPHeader(fitsfile *fp, char *outfile, int rebin)
{
  char creator[FLEN_VALUE];
  char *k;
  int detchans;
  int istat = 0;
                                  /*******************************************/
                                  /* write OGIP headers to current extension */
                                  /*******************************************/
  if (
fits_write_date(fp, &istat) ) {
    anl_msg_error("\
%s-%s: fits_write_date() failed (%d)\n", pname, version, istat);
    return istat;
  }

  sprintf(creator, "%s version %s", anl_task_name(), anl_task_version());
  detchans = ( PHA_CHAN + rebin - 1 ) / rebin;

  if (
fits_update_key_str(fp, k="CREATOR", creator,
		    "software that created this file", &istat) ||
fits_update_key_str(fp, k="FILENAME", aefits_basename(outfile),
		    "file name", &istat) ||
fits_update_key_str(fp, k="ORIGIN", "ISAS",
		    "Origin of FITS file", &istat) ||
fits_update_key_lng(fp, k="VERSION", -1L,
		    "version not specified", &istat) ||
fits_update_key_str(fp, k="HDUDOC", "OGIP memos CAL/GEN/92-002 & 92-002a",
		    "Documents describing the format", &istat) ||
fits_update_key_str(fp, k="HDUCLASS", "OGIP",
		    "format conforms to OGIP standard", &istat) ||
fits_update_key_str(fp, k="HDUVERS", "1.3.0",
		    "Version of family of formats", &istat) ||
fits_update_key_str(fp, k="HDUCLAS1", "RESPONSE",
		    "dataset relates to spectral response", &istat) ||
fits_update_key_str(fp, k="HDUVERS1", "1.0.0",
		    "Version of family of formats", &istat) ||
fits_update_key_lng(fp, k="DETCHANS", detchans,
		    "total number of detector channels", &istat) ||
fits_update_key_str(fp, k="CHANTYPE", "PI",
		    "Detector Channel Type in use (PHA or PI)", &istat) ||
fits_update_key_str(fp, k="RMFVERSN", "1992a",
		    "OGIP classification of FITS format", &istat) ||
      0 ) {
    anl_msg_error("\
%s-%s: fits_update_key('%s') failed (%d)\n", pname, version, k, istat);
    return istat;
  }

  return istat;
}

                                                              /**************/
                                                              /* Energy bin */
                                                              /**************/
energyBin*
xisrsp_ebin_init()
{
  energyBin* ec;
  if ((ec = malloc(sizeof(energyBin))) != NULL){
    ec->energ_lo = ec->energ_hi = NULL;
    ec->size = 0;
  }
  return ec;
}

int
xisrsp_ebin_init_file(energyBin *ec, char *filename)
{
  int col_elo, col_ehi, anul, hdunum, hdutype;
  fitsfile *fp;
  long nrow;
  char *k;

  int istat = 0, istat2 = 0;

  if (
fits_open_file(&fp, filename, READONLY, &istat) ) {
    anl_msg_error("\
%s-%s: fits_open_file('%s') failed (%d)\n", pname, version, filename, istat);
    return istat;
  }

  fits_get_hdu_num(fp, &hdunum);
  if ( 1 == hdunum ) {
    fits_movabs_hdu(fp, 2, &hdutype, &istat);
    if ( istat ) {
      anl_msg_error("\
%s-%s: fits_movabs_hdu() failed (%d)\n", pname, version, istat);
      return istat;
    }
  }

  if (
fits_get_num_rows(fp, &nrow, &istat) ) {
    anl_msg_error("\
%s-%s: fits_get_num_rows() failed (%d)\n", pname, version, istat);
    return istat;
  }
  ec->size = nrow;

  ec->energ_lo = malloc(nrow * sizeof(*ec->energ_lo));
  ec->energ_hi = malloc(nrow * sizeof(*ec->energ_hi));
  if ( NULL == ec->energ_lo || NULL == ec->energ_hi ) {
    anl_msg_error("\
%s-%s: xisrsp_ebin_init_file(): can't allocate memory.\n", pname, version);
    fits_close_file(fp, &istat2);
    xisrsp_ebin_free(ec);
    return -1;
  }

  if (
fits_get_colnum(fp, CASEINSEN, k="ENERG_LO", &col_elo, &istat) ||
fits_get_colnum(fp, CASEINSEN, k="ENERG_HI", &col_ehi, &istat) ||
       0 ) {
    anl_msg_error("\
%s-%s: fits_get_colnum('%s') failed (%d)\n", pname, version, k, istat);
    fits_close_file(fp, &istat2);
    xisrsp_ebin_free(ec);
    return istat;
  }

  if (
fits_read_col_dbl(fp, col_elo, 1, 1, nrow, 0.0, ec->energ_lo, &anul, &istat) ||
fits_read_col_dbl(fp, col_ehi, 1, 1, nrow, 0.0, ec->energ_hi, &anul, &istat) ||
       0 ) {
    anl_msg_error("\
%s-%s: fits_read_col() failed (%d)\n", pname, version, istat);
    fits_close_file(fp, &istat2);
    xisrsp_ebin_free(ec);
    return istat;
  }

  if (
fits_close_file(fp, &istat) ) {
    anl_msg_error("\
%s-%s: fits_close_file() failed (%d)\n", pname, version, istat);
    xisrsp_ebin_free(ec);
    return istat;
  }

  return istat;
}

int
xisrsp_ebin_init_const(energyBin* ec, double lo, double hi, double wid)
{
  int i;

  if ( lo <= 0.0 ) {
    lo = DEFAULT_LOWEST;
  }
  if ( hi <= 0.0 ) {
    hi = DEFAULT_HIGHEST;
  }

  ec->size = (int)( ( hi - lo ) * 1000.0 / wid + 0.5 );
  ec->energ_lo = malloc(ec->size * sizeof(double));
  ec->energ_hi = malloc(ec->size * sizeof(double));
  if ( ec->energ_lo == NULL || ec->energ_hi == NULL){
    anl_msg_error("\
%s-%s: xisrsp_ebin_init_const(): can't allocate memory.\n", pname, version);
    xisrsp_ebin_free(ec);
    return -1;
  }

  ec->energ_lo[0] = lo;
  for (i = 1; i < ec->size; i++){
    ec->energ_hi[i-1] = ec->energ_lo[i] = lo + (i * wid) / 1000.0;
  }
  ec->energ_hi[ec->size - 1] = hi;

  return 0;
}

void
xisrsp_ebin_free(energyBin* ec)
/* reset ec: release the array memories and reset the size */
{
  if ( NULL != ec->energ_lo ) {
    free(ec->energ_lo);
    ec->energ_lo = NULL;
  }
  if ( NULL != ec->energ_hi ) {
    free(ec->energ_hi);
    ec->energ_hi = NULL;
  }
  ec->size = 0;
}

void
xisrsp_ebin_term(energyBin* ec)
/* energyChannel destructor */
{
  xisrsp_ebin_free(ec);
  free(ec);
}

double
xisrsp_ebin_lo(energyBin* ec, int iebin)
{
  double retval;
  retval = ( iebin < 0 )
       ? *ec->energ_lo
       : iebin >= ec->size
       ? ec->energ_lo[ec->size - 1]
       : ec->energ_lo[iebin];
  return retval;
}


double
xisrsp_ebin_hi(energyBin* ec, int iebin)
{
  double retval;
  retval = ( iebin < 0 )
       ? *ec->energ_hi
       : iebin >= ec->size
       ? ec->energ_hi[ec->size - 1]
       : ec->energ_hi[iebin];
  return retval;
}
                                                              /**************/
                                                              /* Efficiency */
                                                              /**************/
effTable*
xisrsp_efficiency_init()
{
  effTable* t;

  if ((t = malloc(sizeof(effTable)))){
    t->energy = t->value = t->edge = NULL;
    t->size = t->edgenum = 0;
  }
  return t;
}

void
xisrsp_efficiency_free(effTable* t)
{
  if (t->energy != NULL){
    free(t->energy);
    t->energy = NULL;
  }
  if (t->value != NULL){
    free(t->value);
    t->value = NULL;
  }
  if (t->edge != NULL){
    free(t->edge);
    t->edge = NULL;
  }
  t->size = t->edgenum = 0;
}

void
xisrsp_efficiency_term(effTable* wt)
{
  xisrsp_efficiency_free(wt);
  free(wt);
}

static double
interpolate(double f_x1, double f_x2, double x1, double x2, double x3)
{
  double ln_f_x1, ln_f_x2, ln_x1, ln_x2, ln_x3;

  ln_f_x1 = log(f_x1);
  ln_f_x2 = log(f_x2);
  ln_x1   = log(x1);
  ln_x2   = log(x2);
  ln_x3   = log(x3);

  return exp((ln_f_x1 * (ln_x2 - ln_x3) + ln_f_x2 * (ln_x3 - ln_x1))
              / (ln_x2 - ln_x1));
}

double
xisrsp_efficiency_value(effTable* efftable, double energy)
/*
 * A negative value will be returned if
 * 1. the energy is on one of the edges, or
 * 2. an edge exists at lower or higher energy of the table range and
 *    the energy is further outside of the edge range.
 * BUG: If two or more edges exist between adjacent data points,
 *      the returned value for the energy between the points can be bogus.
 */
{
  double eff;
  int i;

  /* Return if the energy is on one of the edges. */
  for (i = 0; i < efftable->edgenum; i++){
    if (energy == efftable->edge[i]){
      anl_msg_info("\
The energy %f keV is on one of the edges..\n", energy);
      return -1;
    }
  }

  /* Return if the energy is out of the table and there is an edge
   * between the energy and the table. */
  i = efftable->size - 1;
  if ((efftable->edge[0] < efftable->energy[0] && energy < efftable->edge[0])
      || (efftable->energy[i] < efftable->edge[efftable->edgenum-1] &&
	  efftable->edge[efftable->edgenum-1] < energy)){
    return -1;
  }

  eff = -1;  /* 'not calculated yet' value */
  for (i = 1; i < efftable->size - 1; i++){
  /* Basic strategy:
   * This loop will find the two points from the table that sandwitche
   * the target energy from the energy table, and interpolate the values.
   * If the ener0gy is out of the table range, the lowest or highest two
   * points will be used: see the comments in/after the loop below.
   * The tables must be sorted in ascending order in advance.
   */
    if (energy <= efftable->energy[i]){
    /* MATCH!   : now  t->energy[i-1] < energy <= t->energy[i]  in most case.
     * Exception: In case that the energy is lower than the table's lower
     *            boundary, i==1 and energy < t->energy[i-1] < t->energy[i]
     *            automaticaly. */
      int j;
      for (j = 0; j < efftable->edgenum; j++){
        if (efftable->energy[i-1] < efftable->edge[j]){
          if (efftable->edge[j] < efftable->energy[i]){
            int i1, i2;
            if (energy < efftable->edge[j]){
              i1 = i-2, i2 = i-1;
            } else {
              i1 = i-1, i2 = i;
            }
            eff = interpolate(efftable->value[i1],efftable->value[i2],
                              efftable->energy[i1],efftable->energy[i2],
			      energy);
          }
          break;
        }
      }
      if (eff < 0){  /* not set yet -> no related edge found */
        eff = interpolate(efftable->value[i-1],efftable->value[i],
                          efftable->energy[i-1],efftable->energy[i], energy);
      }
      break;
    }
 }

  if (eff < 0){
  /* NOTE: If not set yet, the energy is between or higher than the
   *       highest two points of the table. */
    i = efftable->size - 2;
    if (efftable->energy[i] < efftable->edge[efftable->edgenum - 1]){
      /*have to concern an edge?*/
      eff = efftable->edge[efftable->edgenum - 1] < energy
          ? efftable->value[i+1]
          : interpolate(efftable->value[i-1], efftable->value[i],
                        efftable->energy[i-1], efftable->energy[i], energy);
    } else {
      eff = interpolate(efftable->value[i], efftable->value[i+1],
                        efftable->energy[i], efftable->energy[i+1], energy);
    }
  }
  return eff;
}

                                             /*******************************/
                                             /* Read FORMAT_VERSION         */
                                             /*******************************/
int
xisrsp_read_format_version(fitsfile *fp, char *extname, int *format_version)
{
  char *k, format_string[FLEN_VALUE];
  int istat = 0;

  fits_read_key_str(fp, k="CBD10001", format_string, NULL, &istat);

  /* If there's no CBD10001 keyword, assume that FORMAT_VERSION is 1 */
  /* There is not CBD10001 keyword in the CALDB released before Jul 29 2006 */
  if ( KEY_NO_EXIST == istat ) {
    *format_version = 1;
    istat = 0;
  } else if ( 0 == istat ) {
    sscanf(format_string, "FORMAT_VERSION(%d)", format_version);
  } else {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  anl_msg_debug("\
%s: FORMAT_VERSION of %s = %d\n", pname, extname, *format_version);

  return 0;
}

                                             /*******************************/
                                             /* Read proper column in CALDB */
                                             /*******************************/
long
xisrsp_seek_valid_row(fitsfile *fp, double obstime)
{
  long irow, nrows, validrow;
  int icol, anul;
  double caltime, timegap, refgap;
  int istat = 0;

  validrow = 1;
  refgap = 0.0;		/* dummy initialization for gcc warning */

  fits_get_num_rows(fp, &nrows, &istat);
  if ( istat ) {
    anl_msg_error("\
%s-%s: fits_get_num_rows() failed (%d)\n", pname, version, istat);
    return -1;
  }

  if ( nrows == 1 ) {
    /* This is in the case of initial operation or constant CALDB issue */
    return validrow;
  }

  fits_get_colnum(fp, CASEINSEN, "TIME", &icol, &istat);
  if ( istat ) {
    anl_msg_error("\
%s-%s: fits_get_colnum('TIME') failed (%d)\n", pname, version, istat);
    return -1;
  }

  for (irow = 1; irow < nrows; irow++) {
    fits_read_col_dbl(fp, icol, irow, 1, 1, 0.0, &caltime, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s-%s: fits_read_col_dbl('TIME') failed (%d)\n", pname, version, istat);
      return -1;
    }
    if ( 1 == irow ) {
      validrow = irow;
      refgap = fabs(obstime - caltime);
    } else {
      timegap = fabs(obstime - caltime);
      if ( timegap < refgap ) {
	validrow = irow;
	refgap = timegap;
      } else if ( timegap == refgap ) {
	if ( caltime <= obstime ) {
	  validrow = irow;
	  refgap = timegap;
	}
      }
    }
  }

  return validrow;
}


                                                      /**********************/
                                                      /* response parameter */
                                                      /**********************/
respParam*
xisrsp_respparam_init(int bi_si_edge_mode, int fi_si_edge_mode)
{
  respParam* t;

  if ((t = malloc(sizeof(respParam)))){
    t->value = NULL;
    t->size = 0;
    t->bi_si_edge_mode = bi_si_edge_mode;
    t->fi_si_edge_mode = fi_si_edge_mode;
  }
  return t;
}

void
xisrsp_respparam_free(respParam* t)
{
  if (t->value != NULL){
    free(t->value);
    t->value = NULL;
  }
  t->size = 0;
}

void
xisrsp_respparam_term(respParam* wt)
{
  xisrsp_respparam_free(wt);
  free(wt);
}

                                                         /*******************/
                                                         /* response matrix */
                                                         /*******************/
static double
fn_gauss(double input, double sigma)
/*
 * return the gaussian of which integral is 1.
 */
{
  double retval;

  retval = (1.0/SQRT_2PI/sigma)* exp(-(input*input)/(2.0*sigma*sigma));
  return retval;
}

static double
fn_exp(double input, double width)
{
  double retval;

  retval =  (input < 0.0)
           ? exp(input/width)/width
           : 0.0;
  return retval;
}

static double
fn_triangle(int chan, double peak, double c3)
/*
 * return the triangle of which integral is 1.
 */
{
  double retval;

  retval = (((double)chan > c3) && ((double)chan < peak))
           ? ((double)chan-c3)*2.0/(peak-c3)/(peak-c3)
           : 0.0;
  return retval;
}

static double
fn_constant(int chan, double peak, int l_limit)
/*
 * return the constant component of which integral is 1.
 */
{
  double retval;

  retval = ((chan >= l_limit) && ((double)chan <= peak))
           ? 1.0/(peak-(double)l_limit)
	   : 0.0;
  return retval;
}

static double
c1(double energy)
/*
 * center of main gaussian
 */
{
  return energy / BIN_ENE;
}

static double
s1(double energy, double s11, double s12, double s13, double s14)
/*
 * width of main gaussian
 */
{
  return s11 * sqrt(s12 + energy*s13 + energy*energy*s14);
}

static double
t2d(double energy, int sensor, double t21, double t22, double t23)
/*
 * t2d = t2/t1	ratio of sub-gaussian to main-gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    return t21;
  } else {
    return t21*exp(-(energy-t22)/t23*(energy-t22)/t23/2.0);
  }
}

static double
c2(int sensor, double spth)
/*
 * center of sub gaussian
 */
{
  return 0.5 * spth;
}

static double
s2(double energy, int sensor, double s11, double s12, double s13, double s14, double s21, double s22, respParam* param)
/*
 * width of sub gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    if ( 0 == param->bi_si_edge_mode ) {
      return 1.78 * s1(energy, s11, s12, s13, s14);
    } else {
      return s1(energy, s11, s12, s13, s14);
    }
  } else {
    return energy <= Si_Kedge  ? 1.78 * s1(energy, s11, s12, s13, s14)
      : s21+s1(energy, s11, s12, s13, s14)*s22 ;
  }
}

static double
t3d(double energy, int sensor, double t31, double t32, double t33)
/*
 * t3d = t3/t1	ratio of triangle component to main-gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    return 0.00;
  } else {
    return energy <= Si_Kedge  ? 0.00
      : 0.3 * (t31 + (energy - t32)*(energy - t32)*t33 );
  }
}

static double
f3(double energy, int sensor, double f31, double f32, double f33)
/*
 * width of triangle component
 */
{
  if (sensor == BISENSOR_ID) {
    return energy / BIN_ENE - 80.0;
  } else {
    return energy <= Si_Kedge  ? 0.00
      : f31 + (energy - f32) * (energy - f32) *  f33;
  }
}

static double
t4d(double energy, int sensor, double t41, double t42, double t43, double t44, double t45, double t46, double t47, double t48)
/*
 * t4d = t4/t1	ratio of constant component to main-gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    return t41*pow(energy, t42) + t43;
  } else {
    return energy <= Si_Kedge ? t41 * pow(energy, t42) + t43
      : t44 * (t45 + t46 * sin(-2.0*PI*(energy-t47)/t48 ) );
  }
}

static double
t5d(double energy, int sensor, double t51, double t52, double t53)
/*
 * t5d = t5/t1	ratio of Si Ka component to main-gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    return energy <= Si_Kedge ? 0.0
      : t51*pow(energy, t52) ;
  } else {
    return energy <= Si_Kedge ? 0.0
      : t51 + pow(energy,t52)*t53;
  }
}

static double
c5(double energy)
/*
 * center of Si escape line
 */
{
  return energy <= Si_Ka ? 0.0
    : Si_Ka/BIN_ENE;
}

static double
s5(double energy, double s11, double s12, double s13, double s14)
/*
 * width of Si escape line
 */
{
  return energy <= Si_Ka ? 1.0e+5
    : s1(energy - Si_Ka, s11, s12, s13, s14);
}

static double
t6d(double energy, int sensor, double t61, double t62, double t63, double t64)
/*
 * t6d = t6/t1	ratio of escape component to main-gaussian
 */
{
  if (sensor == BISENSOR_ID) {
    return energy <= Si_Kedge ? 0.0
      : t61*pow(energy, t62) ;
  } else {
    return energy <= Si_Kedge ? 0.0
      : t61*pow(energy, t62) / (1.0-exp(t63*pow(energy, t64)));
  }
}

static double
c6(double energy)
/*
 * center of Si Ka line
 */
{
  return Si_Ka/BIN_ENE;
}

static double
s6(double energy, double s11, double s12, double s13, double s14)
/*
 * width of Si Ka line
 */
{
  return s1(Si_Ka, s11, s12, s13, s14);
}


xisResponse*
xisrsp_init(int sensor)
{
  xisResponse* r;
  static double edge[3];

  edge[0] = O_Kedge;
  edge[1] = Si_Ka;
  edge[2] = Si_Kedge;

  if ((r = malloc(sizeof(*r))) != NULL){
    r->size = PHA_CHAN;
    r->peakIndex = 0;
    r->eventTh = getevent_th(sensor);
    r->edge = edge;
    r->edgenum = sizeof(edge) / sizeof(edge[0]);
    if (!(r->response = malloc(r->size * sizeof(*r->response)))){
      free(r);
      r = NULL;
    }
  }
  return r;
}


void
xisrsp_term(xisResponse* r)
{
  if (r->response != NULL){
    free(r->response);
  }
  free (r);
}


double
xisrsp_peakValue(xisResponse* r)
/* return the peak value of the response calculated lastly */
{
  return r->response[r->peakIndex];
}


int
xisrsp_eventTh(xisResponse* r)
{
  return r->eventTh;
}


/*int
xisrsp_splitTh(xisResponse* r)
{
  return r->splitTh;
}*/


int
xisrsp_edgeNum(xisResponse* r)
{
  return r->edgenum;
}


double*
xisrsp_edgeArray(xisResponse* r)
{
  return r->edge;
}


respSlice
xisrsp_resp(xisResponse* r, double E, double factor, int sensor, respParam* param, double spth)
/* return:
 * returns the array size, or 0 if malloc failed.
 * NOTE: you must NOT free the allocated memory by yourself.
 */
{
  respSlice s;
  int i, chan_low;
  double primary_ratio, primary_ratio_org, primary_center, primary_sigma;
  double sub_ratio, sub_ratio_org, sub_center, sub_sigma;
  double triangle_ratio, triangle_peak, triangle_width;
  double constant_ratio, constant_max;
  int constant_min;
  double escape_ratio, escape_center, escape_sigma;
  double silicon_ratio, silicon_center,silicon_sigma;
  double ratio;
  double resp_th;
  double s11, s12, s13, s14;
  double s21, s22;
  double t21, t22, t23;
  double t31, t32, t33;
  double t41, t42, t43, t44, t45, t46, t47, t48;
  double t51, t52, t53;
  double t61, t62, t63, t64;
  double f31, f32, f33;
  double jslope, jfactor;

  /* parameters given by experiments:
	t1	1st Gaussian branching ratio
	c1	1st Gaussian center channel
	s1	1st Gaussian sigma
	t2	Sub Gaussian branching ratio
	c1-c2	Sub Gaussian center channel
	s2	Sub Gaussian sigma
	t3	Triangle component branching ratio
	c1	Triangle component peak channel (== 1st Gaussian center)
	f3	Triangle component width
	t4	Constant component branching ratio
	evth	Constant component minimum channel (== Event Th.)
	c1	Constant component maximum channel (== 1st Gaussian center)
	t5	Si-escape branching ratio
	c1-c5	Si-escape center channel
	s5	Si-escape sigma
	t6	Si-K branching ratio
	c6	Si-K center channel
	s6	Si-K sigma
  */
  /* correspondence between CALDB and RMF parameter */

  s11 = param->value[0];
  s12 = param->value[1];
  s13 = param->value[2];
  s14 = param->value[3];

  t21 = param->value[4];
  t22 = param->value[5];
  t23 = param->value[6];
  s21 = param->value[7];
  s22 = param->value[8];

  t31 = param->value[9];
  t32 = param->value[10];
  t33 = param->value[11];
  f31 = param->value[12];
  f32 = param->value[13];
  f33 = param->value[14];

  t41 = param->value[15];
  t42 = param->value[16];
  t43 = param->value[17];
  t44 = param->value[18];
  t45 = param->value[19];
  t46 = param->value[20];
  t47 = param->value[21];
  t48 = param->value[22];

  t51 = param->value[23];
  t52 = param->value[24];
  t53 = param->value[25];

  t61 = param->value[26];
  t62 = param->value[27];
  t63 = param->value[28];
  t64 = param->value[29];

  if (sensor == BISENSOR_ID) {
    primary_ratio_org = 1.0/ (1.0+t2d(E, sensor, t21, t22, t23)
       +t3d(E, sensor, t31, t32, t33)
       +t4d(E, sensor, t41, t42, t43, t44, t45, t46, t47, t48)
       +t5d(E, sensor, t51, t52, t53)
       +t6d(E, sensor, t61, t62, t63, t64)
       +0.15*t2d(E, sensor, t21, t22, t23));
  } else {
    primary_ratio_org = 1.0/ (1.0+t2d(E, sensor, t21, t22, t23)
       +t3d(E, sensor, t31, t32, t33)
       +t4d(E, sensor, t41, t42, t43, t44, t45, t46, t47, t48)
       +t5d(E, sensor, t51, t52, t53)
       +t6d(E, sensor, t61, t62, t63, t64));
  }

  primary_center = c1(E);
  primary_sigma  = s1(E, s11, s12, s13, s14);
  sub_ratio_org  = t2d(E, sensor, t21, t22, t23) * primary_ratio_org;
  sub_center     = primary_center - c2(sensor, spth);
  sub_sigma      = s2(E, sensor, s11, s12, s13, s14, s21, s22, param);
  triangle_ratio = t3d(E, sensor, t31, t32, t33) * primary_ratio_org;
  triangle_peak  = primary_center;
  triangle_width = f3(E, sensor, f31, f32, f33);
  constant_min   = getevent_th(sensor);
  constant_max   = primary_center;
  constant_ratio = t4d(E, sensor, t41, t42, t43, t44, t45, t46, t47, t48) * primary_ratio_org;
  escape_ratio   = t5d(E, sensor, t51, t52, t53) * primary_ratio_org;
  escape_center  = primary_center - c5(E);
  escape_sigma   = s5(E, s11, s12, s13, s14);
  silicon_ratio  = t6d(E, sensor, t61, t62, t63, t64) * primary_ratio_org;
  silicon_center = c6(E);
  silicon_sigma  = s6(E, s11, s12, s13, s14);

  if ( BISENSOR_ID == sensor && 0 != param->bi_si_edge_mode ) {
    jslope = (0.0 - 1.0) / (4.0 - Si_Kedge);
    if (E <= Si_Kedge) {
      jfactor = 1.0;
    } else if (Si_Kedge < E && E <= 4.0) {
      jfactor = 1.0 + 8.0 * (jslope * (E - Si_Kedge) + 1.0);
    } else {
      jfactor = 1.0;
    }
    primary_ratio = primary_ratio_org - (jfactor - 1.0) * sub_ratio_org;
    sub_ratio = sub_ratio_org * jfactor;
  } else {
    primary_ratio = primary_ratio_org;
    sub_ratio = sub_ratio_org;
  }

  resp_th
       = primary_ratio
          * fn_gauss((double)getevent_th(sensor) - primary_center, primary_sigma)
       + sub_ratio
          * fn_gauss((double)getevent_th(sensor) - sub_center, sub_sigma)
       + triangle_ratio
          * fn_triangle(getevent_th(sensor),triangle_peak,primary_center - triangle_width)
       + constant_ratio
          * fn_constant(getevent_th(sensor), constant_max, constant_min)
       + escape_ratio
          * fn_gauss((double)getevent_th(sensor) - escape_center, escape_sigma)
       + silicon_ratio
          * fn_gauss((double)getevent_th(sensor) - silicon_center, silicon_sigma);

  chan_low = getevent_th(sensor);
  r->peakIndex = chan_low;
  for (i = chan_low ; i < PHA_CHAN; i++){
    if (sensor == BISENSOR_ID) {
    ratio
      = primary_ratio
          * fn_gauss((double)i - primary_center, primary_sigma)
       + sub_ratio
          * fn_gauss((double)i - sub_center, sub_sigma)
       + triangle_ratio
          * fn_triangle(i, triangle_peak, triangle_width)
       + constant_ratio
          * fn_constant(i, constant_max, constant_min)
       + escape_ratio
          * fn_gauss((double)i - escape_center, escape_sigma)
       + silicon_ratio
          * fn_gauss((double)i - silicon_center, silicon_sigma)
       + sub_ratio_org*0.15
          * fn_exp((double)i - primary_center, 140.0);
    } else {
    ratio
      = primary_ratio
          * fn_gauss((double)i - primary_center, primary_sigma)
       + sub_ratio
          * fn_gauss((double)i - sub_center, sub_sigma)
       + triangle_ratio
          * fn_triangle(i, triangle_peak, primary_center - triangle_width)
       + constant_ratio
          * fn_constant(i, constant_max, constant_min)
       + escape_ratio
          * fn_gauss((double)i - escape_center, escape_sigma)
       + silicon_ratio
          * fn_gauss((double)i - silicon_center, silicon_sigma);
    }

    if ( primary_center < i && ratio < 0.01*resp_th ) {
      break;
    }
    r->response[i] = ratio * factor;
    if ( r->response[r->peakIndex] < r->response[i] ) {
      r->peakIndex = i;
    }
/*    if (i==chan_low){
      fprintf(stdout,"Energy=%.8le, ratio=%.8le, factor=%.8le\n",E, ratio,factor);
    }*/
  }
  s.array = r->response + chan_low;
  s.startIndex = chan_low;
  s.size = i - chan_low;

  return s;
}


void
xisrsp_Param_free(respParam* p)
{
  if (p->value != NULL){
    free(p->value);
    p->value = NULL;
  }
  p->size = 0.0;
}

void
xisrsp_spthParam_free(spthParam* spthp)
{
  if (spthp->offset != NULL){
    free(spthp->offset);
    spthp->offset = NULL;
  }
  if (spthp->slope != NULL){
    free(spthp->slope);
    spthp->slope = NULL;
  }
  if (spthp->minimum != NULL){
    free(spthp->minimum);
    spthp->minimum = NULL;
  }
  spthp->size = 0.0;
}

void
xisrsp_gainParam_free(gainParam* gainp)
{
  if (gainp->offl != NULL){
    free(gainp->offl);
    gainp->offl = NULL;
  }
  if (gainp->linrl != NULL){
    free(gainp->linrl);
    gainp->linrl = NULL;
  }
  if (gainp->quadl != NULL){
    free(gainp->quadl);
    gainp->quadl = NULL;
  }
  if (gainp->offh != NULL){
    free(gainp->offh);
    gainp->offh = NULL;
  }
  if (gainp->linrh != NULL){
    free(gainp->linrh);
    gainp->linrh = NULL;
  }
  if (gainp->quadh != NULL){
    free(gainp->quadh);
    gainp->quadh = NULL;
  }
  gainp->size = 0.0;
}

respParam*
xisrsp_respParam_init()
{
  respParam* p;

  if ((p = malloc(sizeof(respParam)))){
    p->value = NULL;
    p->size = 0.0;
  }
  return p;
}

spthParam*
xisrsp_spthParam_init()
{
  spthParam* spthp;

  if ((spthp = malloc(sizeof(spthParam)))){
    spthp->offset = NULL;
    spthp->slope = NULL;
    spthp->minimum = NULL;
    spthp->size = XIStotalSegNo;
  }
  return spthp;
}

gainParam*
xisrsp_gainParam_init()
{
  gainParam* gainp;

  if ((gainp = malloc(sizeof(gainParam)))){
    gainp->offl = NULL;
    gainp->linrl = NULL;
    gainp->quadl = NULL;
    gainp->offh = NULL;
    gainp->linrh = NULL;
    gainp->quadh = NULL;
    gainp->size = XIStotalSegNo;
  }
  return gainp;
}

void
xisrsp_calc_weighted_qe(effTable* qe, effTable* weightqe, double* weight)
{
  int iseg, ipos;
  long iene;

  /* memory allocation for weightqe*/
  weightqe->size    = qe->size;
  weightqe->edgenum = qe->edgenum;
  weightqe->edge    = malloc(qe->edgenum * sizeof(double));
  weightqe->energy  = malloc(qe->size * sizeof(double));
  weightqe->value   = malloc(qe->size * sizeof(double));

  /* copy energy and edge info from input qe */
  memcpy(weightqe->energy, qe->energy, qe->size * sizeof(double));
  memcpy(weightqe->edge, qe->edge, qe->edgenum * sizeof(double));

  /* calculate weightqe->value using weighting parameter */
  for (iene=0;iene<qe->size;iene++) {
    weightqe->value[iene] = 0.0;
    for (iseg=0;iseg<XIStotalSegNo;iseg++) {
      for (ipos=0;ipos<XIStotalPosNo;ipos++) {
	weightqe->value[iene] += weight[iseg*XIStotalPosNo+ipos]*qe->value[iene+(iseg*XIStotalPosNo+ipos)*qe->size] ;
      }
    }
  }
}

void
xisrsp_calc_weighted_param(respParam* param, respParam* weightparam, double* weight)
{
  int iseg, ipos;
  long ipara;

  /* memory allocation for weightparam*/
  weightparam->size  = param->size;
  weightparam->value = malloc(param->size * sizeof(double));

  /* calculate weightparam->value using weighting parameter */
  for (ipara=0;ipara<param->size;ipara++) {
    weightparam->value[ipara] = 0.0;
    for (iseg=0;iseg<XIStotalSegNo;iseg++) {
      for (ipos=0;ipos<XIStotalPosNo;ipos++) {
	weightparam->value[ipara] += weight[iseg*XIStotalPosNo+ipos]*param->value[ipara+(iseg*XIStotalPosNo+ipos)*param->size] ;
      }
    }
  }
}

void
xisrsp_calc_weighted_spthp(spthParam* spthp, spthParam* weightspthp, double* weight)
{
  int iseg, ipos;
  long ipara;

  /* memory allocation for weightspthp*/
  weightspthp->size    = 1.0;
  weightspthp->offset  = malloc(sizeof(double));
  weightspthp->slope   = malloc(sizeof(double));
  weightspthp->minimum = malloc(sizeof(double));

  /* calculate weightspthp->value using weighting parameter */
  for (ipara=0;ipara<spthp->size;ipara++) {
    weightspthp->offset[0] = 0.0;
    weightspthp->slope[0] = 0.0;
    weightspthp->minimum[0] = 0.0;
    for (iseg=0;iseg<XIStotalSegNo;iseg++) {
      for (ipos=0;ipos<XIStotalPosNo;ipos++) {
	weightspthp->offset[0] += weight[iseg*XIStotalPosNo+ipos]*spthp->offset[iseg] ;
	weightspthp->slope[0] += weight[iseg*XIStotalPosNo+ipos]*spthp->slope[iseg] ;
	weightspthp->minimum[0] += weight[iseg*XIStotalPosNo+ipos]*spthp->minimum[iseg] ;
      }
    }
  }
}

void
xisrsp_calc_weighted_gainp(gainParam* gainp, gainParam* weightgainp, double* weight)
{
  int iseg, ipos;
  long ipara;

  /* memory allocation for weightgainp*/
  weightgainp->size  = 1.0;
  weightgainp->offl  = malloc(sizeof(double));
  weightgainp->linrl = malloc(sizeof(double));
  weightgainp->quadl = malloc(sizeof(double));
  weightgainp->offh  = malloc(sizeof(double));
  weightgainp->linrh = malloc(sizeof(double));
  weightgainp->quadh = malloc(sizeof(double));

  /* calculate weightgainp->value using weighting parameter */
  for (ipara=0;ipara<gainp->size;ipara++) {
    weightgainp->offl[0] = 0.0;
    weightgainp->linrl[0] = 0.0;
    weightgainp->quadl[0] = 0.0;
    weightgainp->offh[0] = 0.0;
    weightgainp->linrh[0] = 0.0;
    weightgainp->quadh[0] = 0.0;
    for (iseg=0;iseg<XIStotalSegNo;iseg++) {
      for (ipos=0;ipos<XIStotalPosNo;ipos++) {
	weightgainp->offl[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->offl[iseg];
	weightgainp->linrl[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->linrl[iseg];
	weightgainp->quadl[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->quadl[iseg];
	weightgainp->offh[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->offh[iseg];
	weightgainp->linrh[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->linrh[iseg];
	weightgainp->quadh[0] += weight[iseg*XIStotalPosNo+ipos]*gainp->quadh[iseg];
      }
    }
  }
}

/*static COLUMN_INF matrix_column_inf[MATRIX_EXTENSION_COLNUM];*/
static char *matrix_extname = MATRIX_EXTENSION_NAME;
static char *matrix_colname[MATRIX_EXTENSION_COLNUM] = {
  "ENERG_LO", "ENERG_HI", "N_GRP", "F_CHAN","N_CHAN","MATRIX"
};
static char *matrix_colform[MATRIX_EXTENSION_COLNUM] = {
  "1E", "1E", "1I", "1I", "1I", "PE"
};
static char *matrix_colunit[MATRIX_EXTENSION_COLNUM] = {
  "keV", "keV", " "," "," "," "
};
static int matrix_coltype[MATRIX_EXTENSION_COLNUM] = {
  TDOUBLE, TDOUBLE, TINT, TINT, TINT, TDOUBLE
};
static int matrix_colwid[MATRIX_EXTENSION_COLNUM] = {
  sizeof(double), sizeof(double), sizeof(int),
  sizeof(int), sizeof(int), sizeof(double *)
};

/*static COLUMN_INF ebound_column_inf[EBOUND_EXTENSION_COLNUM];*/
static char *ebound_extname = EBOUND_EXTENSION_NAME;
static char *ebound_colname[EBOUND_EXTENSION_COLNUM] = {
  "CHANNEL", "E_MIN", "E_MAX"
};
static char *ebound_colform[EBOUND_EXTENSION_COLNUM] = {
  "1I", "1E", "1E"
};
static char *ebound_colunit[EBOUND_EXTENSION_COLNUM] = {
  " ", "keV", "keV"
};
static int ebound_coltype[EBOUND_EXTENSION_COLNUM] = {
  TINT, TDOUBLE, TDOUBLE
};
static int ebound_colwid[EBOUND_EXTENSION_COLNUM] = {
  sizeof(int), sizeof(double), sizeof(double)
};

static int
define_bnk(char *extname, int colnum,
	   char *colname[], int *coltype, int *colwid, int *colrep/*,
	   COLUMN_INF *inf*/)
{
  char bnkname[128];
  int icol, width, repeat;

#if 0	/* obsolete */
  int max_value_size = 0;
#endif

  for (icol = 1; icol <= colnum; icol++) {
    /* check data type */
    /* TLONG is 4byte integer in CFITSIO */
    /* short is used as integer in bank */
    width = colwid[icol-1];
    if ( 2 == width ) {
      width = 4;
    }
    repeat = ( NULL == colrep ) ? 1 : colrep[icol-1];
    sprintf(bnkname, "XIS:%s:%s", extname, colname[icol-1]);
#if 0	/* obsolete */
    /* pack column information */
    inf[icol-1].datatype = coltype[icol-1];
    inf[icol-1].name = strdup(bnkname);
    inf[icol-1].size = repeat*width;
    inf[icol-1].nelem = repeat;
    inf[icol-1].colnum = icol;
    if ( max_value_size < repeat*width ) {
      max_value_size = repeat*width;
    }
#endif
    BnkDef(bnkname, repeat*width);
  }

#if 0	/* obsolete */
  sprintf(bnkname, "XIS:%s:LIST:PTR", extname);
  BnkDef(bnkname, sizeof(inf));
  BnkPut(bnkname, sizeof(inf), &inf);

  sprintf(bnkname, "XIS:%s:LIST:NUM", extname);
  BnkDef(bnkname, sizeof(colnum));
  BnkPut(bnkname, sizeof(colnum), &colnum);

  sprintf(bnkname, "XIS:%s:LIST:MAXSIZE", extname);
  BnkDef(bnkname, sizeof(max_value_size));
  BnkPut(bnkname, sizeof(max_value_size), &max_value_size);
#endif

  return 0;
}

void
xirsp_bnkdef_matrix(void)
{
  define_bnk(matrix_extname, MATRIX_EXTENSION_COLNUM,
	     matrix_colname, matrix_coltype, matrix_colwid, NULL/*,
	     matrix_column_inf*/);
}

void
xirsp_bnkdef_ebound(void)
{
  define_bnk(ebound_extname, EBOUND_EXTENSION_COLNUM,
	     ebound_colname, ebound_coltype, ebound_colwid, NULL/*,
	     ebound_column_inf*/);
}

void
xisrsp_bnkput_matrix(double energ_lo, double energ_hi, respSlice *sp)
{
  static int n_grp = 1;	/* 1 array for 1 input energy */

#if 0	/* obsolete */
  matrix_column_inf[5].nelem = sp->size;	/* MATRIX column */
#endif
  BnkfPutM("XIS:MATRIX:ENERG_LO", sizeof(energ_lo), &energ_lo);
  BnkfPutM("XIS:MATRIX:ENERG_HI", sizeof(energ_hi), &energ_hi);
  BnkfPutM("XIS:MATRIX:N_GRP", sizeof(n_grp), &n_grp);
  BnkfPutM("XIS:MATRIX:F_CHAN", sizeof(sp->startIndex), &sp->startIndex);
  BnkfPutM("XIS:MATRIX:N_CHAN", sizeof(sp->size), &sp->size);
  BnkfPutM("XIS:MATRIX:MATRIX", sizeof(sp->array), &sp->array);
}

int
xisrsp_fitswrite_matrix(fitsfile *ofp, long irow, int rebin)
{
  double energ_lo, energ_hi, *matrix;
  int n_grp, f_chan, n_chan;
  int new_f_chan, new_n_chan, ichan, new_ichan, pre_ichan;
  int icol, istat, used;

  BnkfGetM("XIS:MATRIX:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
  BnkfGetM("XIS:MATRIX:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
  BnkfGetM("XIS:MATRIX:N_GRP", sizeof(n_grp), &used, &n_grp);
  BnkfGetM("XIS:MATRIX:F_CHAN", sizeof(f_chan), &used, &f_chan);
  BnkfGetM("XIS:MATRIX:N_CHAN", sizeof(n_chan), &used, &n_chan);
  BnkfGetM("XIS:MATRIX:MATRIX", sizeof(matrix), &used, &matrix);

  if ( 1 < rebin ) {
    new_f_chan = (f_chan - RESP_BASE) / rebin + RESP_BASE;
    new_n_chan = (f_chan + n_chan - 1 - RESP_BASE) / rebin + RESP_BASE;
    new_n_chan = new_n_chan - new_f_chan + 1;
    pre_ichan = -1;
    for (ichan = 0; ichan < n_chan; ichan++) {
      new_ichan = (f_chan + ichan - RESP_BASE) / rebin + RESP_BASE;
      new_ichan = new_ichan - new_f_chan;
      if ( pre_ichan != new_ichan ) {
	matrix[new_ichan] = matrix[ichan];
	pre_ichan = new_ichan;
      } else {
	matrix[new_ichan] += matrix[ichan];
      }
    }
    f_chan = new_f_chan;
    n_chan = new_n_chan;
  }

  icol = 1;
  istat = 0;
  if (
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &energ_lo, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &energ_hi, &istat) ||
fits_write_col_int(ofp, icol++, irow, 1, 1, &n_grp, &istat) ||
fits_write_col_int(ofp, icol++, irow, 1, 1, &f_chan, &istat) ||
fits_write_col_int(ofp, icol++, irow, 1, 1, &n_chan, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, n_chan, matrix, &istat) ||
       0 ) {
    anl_msg_error("\
%s-%s: fits_write_col('%s') failed (%d)\n",
	pname, version, matrix_colname[icol-1], istat);
    return istat;
  }

#if 0	/* obsolete */
  int used, maxsize;

  BnkfGetM("XIS:MATRIX:LIST:MAXSIZE", sizeof(int), &used, &maxsize);

  /* BnkGet column info and write them to event fits */
  if ( bank2fits(ofp,
		 matrix_column_inf, MATRIX_EXTENSION_COLNUM, irow,
		 maxsize) != ANL_TRUE) {
    anl_msg_error("\
........cannot bank2fits here\n");
    return -1;
  }
#endif

  return 0;
}

void
xisrsp_bnkput_ebound(int channel, double Emin, double Emax)
{
  BnkfPutM("XIS:EBOUNDS:CHANNEL", sizeof(channel), &channel);
  BnkfPutM("XIS:EBOUNDS:E_MIN", sizeof(Emin), &Emin);
  BnkfPutM("XIS:EBOUNDS:E_MAX", sizeof(Emax), &Emax);
}

int
xisrsp_fitswrite_ebound(fitsfile *ofp, long irow, int rebin)
{
  double Emin, Emax;
  int icol, istat, used, channel;

  BnkfGetM("XIS:EBOUNDS:CHANNEL", sizeof(channel), &used, &channel);
  BnkfGetM("XIS:EBOUNDS:E_MIN", sizeof(Emin), &used, &Emin);
  BnkfGetM("XIS:EBOUNDS:E_MAX", sizeof(Emax), &used, &Emax);

  icol = 1;
  istat = 0;

  if ( 1 < rebin ) {
    if ( 0 == (irow - 1) % rebin ) {
      irow = (irow - 1) / rebin + 1;
      channel = (channel - RESP_BASE) / rebin + RESP_BASE;
      if (
fits_write_col_int(ofp, icol++, irow, 1, 1, &channel, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &Emin, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &Emax, &istat) ||
	   0 ) {
	goto error;
      }
    } else {
      irow = (irow - 1) / rebin + 1;
      icol = icol + 2;	/* skip channel, Emin */
      if (
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &Emax, &istat) ||
	   0 ) {
	goto error;
      }
    }
  } else if (
fits_write_col_int(ofp, icol++, irow, 1, 1, &channel, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &Emin, &istat) ||
fits_write_col_dbl(ofp, icol++, irow, 1, 1, &Emax, &istat) ||
       0 ) {
 error:
    anl_msg_error("\
%s-%s: fits_write_col('%s') failed (%d)\n",
	pname, version, ebound_colname[icol-1], istat);
    return istat;
  }

#if 0	/* obsolete */
  int used, maxsize;
  BnkfGetM("XIS:EBOUND:LIST:MAXSIZE", sizeof(int), &used, &maxsize);

  if ( bank2fits(ofp,
		 ebound_column_inf, EBOUND_EXTENSION_COLNUM, (long)i+1,
		 maxsize) != ANL_TRUE) {
    anl_msg_error("\
........cannot bank2fits here\n");
    return -1;
  }
#endif

  return 0;
}

int
xisrsp_create_matrix_ext(fitsfile *ofp, long nrow, int rebin)
{
  char *k;
  int tlmax;
  int istat = 0;

  fits_create_tbl(ofp, BINARY_TBL, nrow,
		  MATRIX_EXTENSION_COLNUM,
		  matrix_colname, matrix_colform,
		  matrix_colunit, matrix_extname, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_tbl('%s') failed (%d)\n", pname, matrix_extname, istat);
    return istat;
  }

  tlmax = RESP_BASE + ( PHA_CHAN + rebin - 1 ) / rebin - 1;

  if (
fits_write_key_lng(ofp, k="TLMIN4", RESP_BASE,
		   "Minimum legal value of F_CHAN", &istat) ||
fits_write_key_lng(ofp, k="TLMAX4", tlmax,
		   "Maximum legal value of F_CHAN", &istat) ||
       0 ) {
    anl_msg_error("\
%s-%s: fits_write_key('%s') failed (%d)\n", pname, version, k, istat);
    return istat;
  }

  return istat;
}

int
xisrsp_create_ebound_ext(fitsfile *ofp, long nrow, int rebin)
{
  char *k;
  int tlmax;
  int istat = 0;

  fits_create_tbl(ofp, BINARY_TBL, nrow,
		  EBOUND_EXTENSION_COLNUM,
		  ebound_colname, ebound_colform,
		  ebound_colunit, ebound_extname, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_tbl('%s') failed (%d)\n", pname, ebound_extname, istat);
    return istat;
  }

  tlmax = RESP_BASE + ( PHA_CHAN + rebin - 1 ) / rebin - 1;

  if (
fits_write_key_lng(ofp, k="TLMIN1", RESP_BASE,
		   "The first channel in the response", &istat) ||
fits_write_key_lng(ofp, k="TLMAX1", tlmax,
		   "The highest channel in the response", &istat) ||
       0 ) {
    anl_msg_error("\
%s-%s: fits_write_key('%s') failed (%d)\n", pname, version, k, istat);
    return istat;
  }

  return istat;
}

char *
xisrsp_get_caldb_file(char *instrume, char *codename, char *o_filename)
{
  CALDB_INFO caldb;

  if ( 0 != CLstricmp("CALDB", o_filename) ) {
    return o_filename;
  }

  aste_caldb_init(&caldb);
  caldb.instrume = instrume;
  caldb.codename = codename;
  aste_caldb_get(&caldb);

  if ( 0 != caldb.status || 0 == caldb.nfound ) {
    anl_msg_error("\
%s-%s: no CALDB entry for '%s' (status=%d)\n",
		  pname, version, codename, caldb.status);
      return NULL;
    }

  if ( 1 != caldb.nfound ) {
    anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
		    pname, caldb.nfound, codename);
  }

  return caldb.filename;	/* return allocated string */
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
