/*************************************

  XISrmfgen (XISmkResp & XISwriteResp)

----------------------------------------------------------------------------
  XISmkResp

	2006/02/09 created by H.Nakajima

    ver 0.0 2006/02/09 H.Nakajima

    ver 0.1 2006/02/15 H.Nakajima
	weight RMF parameters using wmap in sprctrum file

    ver 0.2 2006/02/25 H.Nakajima
	weight RMF parameters using wmap in sprctrum file
	fix the bug in the XISmkResp_init

    ver 0.3 2006/05/24 H.Nakajima
	fix typos
	put parameters in order in COM

    ver 0.4 2006/09/15 Y.ISHISAKI
	stop using random number, non-integer spth in _ana()
	bug fix in realloc(edge) in _init()
	use anl_msg_error/info() functions
	call showParam() in _init()
	check edgenum in edge++, in _ana()
	free(etmp) instead of edge, in _exit()
	BnkDef/Put("XIS:MATRIX:N_ROWS") in _init()
	rename infile -> phafile
	change type of parameter, flag_constch2e, integer -> boolean
	add leapfile, telescop, instrume, date_obs, clk_mode parameters
	support phafile=none
	add read_header(), read_weight()
	rename BNK, "XIS:QEFILE" -> "XIS:QEFILE:PTR"
	add BNK, "XIS:INSTRUME:PTR"

    ver 1.0 2006/09/16 Y.ISHISAKI
	fix edge treatment in _init()
	add BNK,"XIS:TELESCOP:PTR","XIS:RMFPARAMFILE:PTR","XIS:MAKEPIFILE:PTR"
	add BNK,"XIS:MATRIX:ENERGY_BIN"
	rename parameter, caldbqefile -> quantefffile
	rename parameter, caldbrmffile -> rmfparamfile
	rename parameter, caldbspthfile -> makepifile
	rename parameter, ch2efile -> ebinfile
	rename parameter, flag_const_ebin -> ebin_mode, chan2ene -> ebin_width
	add parameter, edge_treatment, ebin_lowermost, ebin_uppermost
	support caldb for quanteff, rmfparam, makepi, leapsec files
	write parameters into HISOTRY, in _endrun()
	move _term() calls from _endrun() to _exit()

    ver 1.1 2006/10/16 Y.ISHISAKI
	add check_instrume()
	use xisrsp_seek_valid_row() instead of xisrsp_seek_nearest()
	use fits_get_colnum() instead of fixed column number

----------------------------------------------------------------------------
  XISwriteResp

    2006/02/09 created by H.Nakajima

    ver 0.0 2006/02/09 H.Nakajima

    ver 0.1 2006/02/15 H.Nakajima
	weight RMF parameters using wmap in sprctrum file

    ver 0.2 2006/09/15 Y.ISHISAKI
	use anl_msg_error/info() functions
	BnkGet("XIS:MATRIX:N_ROWS") in _init()
	set nrows in creating RMF, in _init()
	use BNK, "XIS:QEFILE:PTR", instead of "XIS:QEFILE"
	use BNK, "XIS:INSTRUME:PTR", instead of "XIS:SENSORID"

    ver 1.0 2006/09/15 Y.ISHISAKI
	use BNK, "XIS:TELESCOP:PTR", "XIS:INSTRUME:PTR"
	use BNK, "XIS:RMFPARAMFILE:PTR", "XIS:MAKEPIFILE:PTR"
	use BNK, "XIS:MATRIX:ENERGY_BIN"
	add writeTelescopInstrume(), writeCaldbFiles(), write_timestamp()

    ver 1.1 2006/10/16 Y.ISHISAKI
	add rebin parameter

    ver 1.2 2006/10/26 Y.ISHISAKI
	use aefits_aetime2datestr() for DATE_OBS in writeTelescopInstrume()

----------------------------------------------------------------------------

    ver 1.2 2006/11/26 Y.ISHISAKI
	merge XISmkResp & XISwriteResp
	merge parameter I/F, showParam()
	remove BNK, XIS:MATRIX:N_ROWS, XIS:MATRIX:ENERGY_BIN

    ver 1.3 2007/05/14 Y.ISHISAKI
	read EDITMODE, WINOPT, CI keywords
	add 'enable_scirmf' parameter

    ver 1.4 2009/02/28 Y.ISHISAKI
	modify 2x2 and psum messages

    ver 1.5 2011/07/02 M.Nobukawa, Y.ISHISAKI
	add 'RMF_PARAMETERS_PSUM'
	modify psum/timing message

    ver 1.6 2012/04/21 Y.ISHISAKI
	add 'bi_si_edge_mode', 'fi_si_edge_mode' parameters
	add 'winopt=%d' in history of RMF in XISmkResp_endrun()

*************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "anl.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_caldb.h"
#include "xisEventFitsUtil.h"
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "xisrmfgen";
char XISmkResp_version[] = "version 1.6";
char XISwriteResp_version[] = "version 1.6";

static struct {
  int sensor;
  double obs_time;
  char telescop[PIL_LINESIZE];
  char instrume[PIL_LINESIZE];
  char clk_mode[PIL_LINESIZE];
  char editmode[PIL_LINESIZE];
  char date_obs[PIL_LINESIZE];
  int winopt;	/* WINOPT header keyword, [0:off 1:1/4 2:1/8 3:1/16] */
  int ci;	/* CI header keyword, [0:none 1:diagn 2:SCI-54 3:SCI-108] */

  char phafile[PIL_LINESIZE]; /* pha or image file (wmap must be in DET) */
  int flag_use_phafile;

  int edge_treatment;			/* 0:ignore,1:move boundaries */
  int num_edge;

  int ebin_mode;			/* 0:const,1:file */
  double ebin_lowermost;		/* keV */
  double ebin_uppermost;		/* keV */
  double ebin_width;			/* eV */
  char ebinfile[PIL_LINESIZE];		/* file to refer energy bin */

  int enable_scirmf;
  int bi_si_edge_mode;
  int fi_si_edge_mode;

  char *leapfile, o_leapfile[PIL_LINESIZE];
  char *quantefffile;
  char o_quantefffile[PIL_LINESIZE];	/* caldb (QE parameters) */
  char *rmfparamfile;
  char o_rmfparamfile[PIL_LINESIZE];	/* caldb (RMF parameters) */
  char *makepifile;
  char o_makepifile[PIL_LINESIZE];	/* caldb (SpTh parameters) */

  char outfile[PIL_LINESIZE];
  int clobber;
  int rebin;
} com;

static fitsfile *ofp;
static long irow;
static energyBin *energy_bin;
static xisResponse *xisresp;
static effTable *ccdqe;
static effTable *wccdqe;
static effTable *obfqe;
static effTable *wobfqe;
static respSlice slice;
static spthParam *spthp;
static gainParam *gainp;
static spthParam *wspthp;
static gainParam *wgainp;
static respParam *param;
static respParam *wparam;

#define NUM_WEIGHT	(XIStotalSegNo*XIStotalPosNo)
static double weight[NUM_WEIGHT];

/***************************************************************************
  XISmkResp
***************************************************************************/

/* ignore digits less than 1.0e-6 keV */
static double
ebin_trunc(double dval)
{
  int ival;

  ival = (int)(dval * 1.0e6 + 0.5);
  dval = ival / 1.0e6;

  return dval;
}

static int
ebin_comp(double d1, double d2)
{
  int i1, i2;

  i1 = (int)( d1 * 1.0e6 + 0.5 );
  i2 = (int)( d2 * 1.0e6 + 0.5 );

  if ( i1 == i2 ) {
    return 0;
  } else if ( i1 < i2 ) {
    return -1;
  }

  return 1;
}

static int
ebin_comp_for_qsort(const void *p1, const void *p2)
{
  return ebin_comp(*(double *)p1, *(double *)p2);
}

static void
det2act(int* actco, int* detco, int isen)
{/* See "Astro-E2 XIS Science FITS siyousyo p.18 " */
    switch (isen){
    case 0:
      actco[0] = 1024 - detco[0];
      actco[1] = detco[1] - 1;
      break;
    case 1:
      actco[0] = detco[1] - 1;
      actco[1] = detco[0] - 1;
      break;
    case 2:
      actco[0] = 1024 - detco[1];
      actco[1] = 1024 - detco[0];
      break;
    case 3:
      actco[0] = 1024 - detco[0];
      actco[1] = detco[1] - 1;
      break;
    }
}

static int
check_instrume(fitsfile *fp)
{
  char cal_instrume[FLEN_VALUE];
  int istat = 0;

  fits_read_key_str(fp, "INSTRUME", cal_instrume, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_key_str('INSTRUME') failed (%d)\n", pname, istat);
    return istat;
  }
  if ( 0 != strcmp(com.instrume, cal_instrume) ) {
    anl_msg_error("\
%s: INSTRUME (%s) is inconsistent with CALDB (%s)\n",
	pname, com.instrume, cal_instrume);
    return -1;
  }

  return 0;
}

/**************************************************************************
  read com.telescop, com.instrume, com.obs_time, com.clk_mode
  from input file
**************************************************************************/
static int
read_header(fitsfile *fp)
{
  char *k;
  double tstart, tstop;

  int istat = 0;

  /* check sensor ID observation date */
  if (
fits_read_key_str(fp, k="TELESCOP", com.telescop, NULL, &istat) ||
fits_read_key_str(fp, k="INSTRUME", com.instrume, NULL, &istat) ||
fits_read_key_dbl(fp, k="TSTART", &tstart, NULL, &istat) ||
fits_read_key_dbl(fp, k="TSTOP", &tstop, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  com.obs_time = (tstart + tstop) / 2.0;
  aefits_aetime2datestr(com.obs_time, com.date_obs);

  /* check clock mode */
  if (
fits_read_key_str(fp, "CLK_MODE", com.clk_mode, NULL, &istat) ) {
    anl_msg_warning("\
%s: Keyword CLK_MODE was not found in the header (%d).\n\
    Assuming normal mode\n", pname, istat);
    strcpy(com.clk_mode, "normal");
  }

  /* check edit mode */
  if (
fits_read_key_str(fp, "EDITMODE", com.editmode, NULL, &istat) ) {
    anl_msg_warning("\
%s: Keyword EDITMODE was not found in the header (%d).\n\
    Assuming 5x5 mode\n", pname, istat);
    strcpy(com.editmode, "5x5");
  }

  /* check window option */
  if (
fits_read_key(fp, TINT, "WINOPT", &com.winopt, NULL, &istat) ) {
    anl_msg_warning("\
%s: Keyword WINOPT was not found in the header (%d).\n\
    Assuming window off\n", pname, istat);
    com.winopt = 0;
  }

  /* check CI mode */
  if (
fits_read_key(fp, TINT, "CI", &com.ci, NULL, &istat) ) {
    anl_msg_warning("\
%s: Keyword CI was not found in the header (%d).\n\
    Assuming CI = 0 (no charge injection)\n", pname, istat);
    com.ci = 0;
  }

  PILPutString("instrume", com.instrume);
  PILPutString("date_obs", com.date_obs);
  PILPutString("clk_mode", com.clk_mode);
  PILPutString("editmode", com.editmode);
  PILPutInt   ("winopt", com.winopt);
  PILPutInt   ("ci", com.ci);

  return 0;
}

/**************************************************************************
  read weight[] from input file
**************************************************************************/
static int
read_weight(fitsfile *fp, double *weight)
{
  char *k;
  char hduname[FLEN_VALUE];
  char wmxaxis[FLEN_VALUE], wmyaxis[FLEN_VALUE];
  int i, anynul, rebin, xoff, yoff, imapx, imapy, detco[2], actco[2];
  double wval, nulval = 0, *wmap = NULL;
  long naxis, naxis1, naxis2, evnum;

  int istat = 0;

  if (
fits_read_key_str(fp, k="HDUNAME", hduname, NULL, &istat) ) {
 read_key_error:
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }
  if ( 0 == strcmp("IMAGE", hduname) ) {
    if (
fits_read_key(fp, TINT, "IMGBIN", &rebin, NULL, &istat) ) {
      goto read_key_error;
    }
    xoff = 1;
    yoff = 1;
  } else if ( 0 == strcmp("WMAP", hduname) ) {
    if (
fits_read_key_lng(fp, k="NAXIS", &naxis, NULL, &istat) ||
	 0 ) {
      goto read_key_error;
    }
    if ( 0 == naxis ) {
      anl_msg_warning("\n\
%s: WARNING: HDUNAME='WMAP' is found, but no image is contained.\n\
%s: WARNING: Use constant weight on the whole CCD.\n", pname, pname);
      goto constant_weight;
    }
    if (
fits_read_key(fp, TINT, k="WMREBIN", &rebin, NULL, &istat) ||
fits_read_key(fp, TINT, k="X-OFFSET", &xoff, NULL, &istat) ||
fits_read_key(fp, TINT, k="Y-OFFSET", &yoff, NULL, &istat) ||
	 0 ) {
      goto read_key_error;
    }
  } else {
    anl_msg_error("\
%s: Input file must not be image or sprctrum file.\n", pname);
    return -1;
  }

  if (
fits_read_key_str(fp, k="CTYPE1P", wmxaxis, NULL, &istat) ||
fits_read_key_str(fp, k="CTYPE2P", wmyaxis, NULL, &istat) ||
fits_read_key_lng(fp, k="NAXIS1", &naxis1, NULL, &istat) ||
fits_read_key_lng(fp, k="NAXIS2", &naxis2, NULL, &istat) ||
       0 ) {
    goto read_key_error;
  }
  if ( 0 != strcmp("DETX", wmxaxis) || 0 != strcmp("DETY", wmyaxis) ) {
    anl_msg_warning("\n\
%s: WARNING: Weighted map or image is not in DET coordinate.\n\
%s: WARNING: Use constant weight on the whole CCD.\n", pname, pname);
 constant_weight:
    for (i = 0; i < NUM_WEIGHT; i++) {
      weight[i] = 1.0 / NUM_WEIGHT;	/* constant weight on whole CCD */
    }
    goto skip;
  }

  wmap = malloc( naxis1 * naxis2 * sizeof(*wmap) );
  if ( NULL == wmap ) {
    anl_msg_error("\
%s: malloc( wmap[%ldx%ld] ) failed\n", pname, naxis1, naxis2);
    return -1;
  }

  fits_read_img_dbl(fp, 0, 1, naxis1*naxis2, nulval, wmap, &anynul, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_img() failed (%d)\n", pname, istat);
    return istat;
  }

  /*****************************************************************
  How to weight each response parameters.

 [0][2]  _____ [0][3] Y
        |    |        /|\
        |    |         |
        |    |         |
        |    |         |
        |____|         |________\ X
 [0][0] \    / [0][1]           /
         |  |
         |  |
      /|_|__|_|\
      \|  or  |/

            [*][*]
             |  |_
             |__  the position in a segment
                segment
  ******************************************************************/

  for (i = 0; i < NUM_WEIGHT; i++) {
    weight[i] = 0.0;
  }
  /*fprintf(stdout,"detxoff=%d, detyoff=%d\n",xoff, yoff);*/

  evnum = 0;
  for (imapx = 0; imapx < naxis1; imapx++) {
    detco[0] = rebin * ( xoff + imapx ) - rebin/2;
    for (imapy = 0; imapy < naxis2; imapy++){
      detco[1] = rebin * ( yoff + imapy ) - rebin/2;
      det2act(actco, detco, com.sensor);
      /*fprintf(stdout,"detx = %d, dety=%d, actx=%d, acty=%d, eventnum=%ld\n ",detco[0], detco[1], actco[0], actco[1], (long)*(wmap+imapy*dim1+imapx));*/

      wval = wmap[imapy*naxis1 + imapx];
      if ( 0.0 < wval ) {
	evnum += (long)wval;
	weight[(int)(actco[0]/XISactiveSegmentHsize)*XIStotalSegNo + 0] +=
	  wval * ( (255.0 - actco[0]%256)/255.0 * (1023.0 - actco[1])/1023.0 );
	weight[(int)(actco[0]/XISactiveSegmentHsize)*XIStotalSegNo + 1] +=
	  wval * ( (actco[0]%256)/255.0 * (1023 - actco[1])/1023.0 );
	weight[(int)(actco[0]/XISactiveSegmentHsize)*XIStotalSegNo + 2] +=
	  wval * ( (255.0 - actco[0]%256)/255.0 * actco[1]/1023.0 );
	weight[(int)(actco[0]/XISactiveSegmentHsize)*XIStotalSegNo + 3] +=
	  wval * ( (actco[0]%256)/255.0 * actco[1]/1023.0);
      }
    }
  }

  free(wmap);

  if ( evnum <= 0 ) {
    anl_msg_error("\
%s: WMAP is zero or negative\n", pname);
    return -1;
  }

  if ( evnum < 10 ) {
    anl_msg_warning("\
*****************WARNING*********************\n\
Event number is too few.\n\
Please check your input spectrum file.\n\
*********************************************\n");
  }
  for (i = 0; i < NUM_WEIGHT; i++) {
    weight[i] /= evnum;
  }

 skip:

  anl_msg_info("\
\n\
Weight of paramters:\n\
      +-----------------+-----------------+-----------------+-----------------+\n\
      |%.2e %.2e|%.2e %.2e|%.2e %.2e|%.2e %.2e|\n\
      |                 |                 |                 |                 |\n\
      |    SEGMENT A    |    SEGMENT B    |    SEGMENT C    |    SEGMENT D    |\n\
      |                 |                 |                 |                 |\n\
 ACTY |%.2e %.2e|%.2e %.2e|%.2e %.2e|%.2e %.2e|\n\
   ^  +-----------------+-----------------+-----------------+-----------------+\n\
   |   \\                |                / \\                |                /\n\
   +---> ACTX\n\
\n",	*(weight+2),  *(weight+3),  *(weight+6), *(weight+7),
	*(weight+10), *(weight+11), *(weight+14), *(weight+15),
	*(weight+0),  *(weight+1),  *(weight+4), *(weight+5),
	*(weight+8),  *(weight+9),  *(weight+12), *(weight+13));

  anl_msg_info("weightsum = %.8e\n", *(weight+2)+
	 *(weight+3)+ *(weight+6)+ *(weight+7)+ *(weight+10)+ *(weight+11)+
	 *(weight+14)+ *(weight+15)+ *(weight+0)+
	 *(weight+1)+ *(weight+4)+ *(weight+5)+ *(weight+8)+ *(weight+9)+
	 *(weight+12)+ *(weight+13) );

  return 0;
}

static char *
mkfname(char *name, char *o_name)
{
  static char buf[2*PIL_LINESIZE];
  if ( name == o_name ) {
    return name;
  }
  sprintf(buf, "%s (%s)", name, o_name);
  return buf;
}

static char *
mkfname_history(char *buf, char *param, char *name, char *o_name)
{
  if ( name == o_name ) {
    sprintf(buf, "  %s='%s'", param, name);
  } else {
    sprintf(buf, "  %s='%s' (%s)", param, name, o_name);
  }
  return buf;
}

static void
showParam(void)
{
  static char *winopt_hlp = "[0:off 1:1/4 2:1/8 3:1/16]";
  static char *ci_hlp = "[0:none 1:diagn 2:SCI-54 3:SCI-108]";

  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "PHAFILE", com.phafile);
  printf("%20s   %s\n", "OUTFILE", com.outfile);
  printf("%20s   %s\n", "TELESCOP", com.telescop);
  printf("%20s   %s\n", "INSTRUME", com.instrume);
  printf("%20s   %s\n", "DATE_OBS", com.date_obs);
  printf("%20s   %s\n", "CLK_MODE", com.clk_mode);
  printf("%20s   %s\n", "EDITMODE", com.editmode);
  printf("%20s   %d    %s\n", "WINOPT", com.winopt, winopt_hlp);
  printf("%20s   %d    %s\n", "CI", com.ci, ci_hlp);
  printf("%20s   %d\n", "REBIN", com.rebin);
  printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
  printf("%20s   %s\n", "ENABLE_SCIRMF", com.enable_scirmf ? "YES" : "NO");
  printf("%20s   %d:%s\n", "EDGE_TREATMENT", com.edge_treatment,
	(0 == com.edge_treatment) ? "IGNORE" :
	(1 == com.edge_treatment) ? "SHIFT_EBIN" : "ERROR");
  printf("%20s   %d\n", "BI_SI_EDGE_MODE", com.bi_si_edge_mode);
  printf("%20s   %d\n", "FI_SI_EDGE_MODE", com.fi_si_edge_mode);
  printf("%20s   %d:%s\n", "EBIN_MODE", com.ebin_mode,
	(0 == com.ebin_mode) ? "CONST" :
	(1 == com.ebin_mode) ? "FILE" : "ERROR");
  if( 0 == com.ebin_mode ) {
    printf("%20s   %6.3f keV\n", "EBIN_LOWERMOST", com.ebin_lowermost);
    printf("%20s   %6.3f keV\n", "EBIN_UPPERMOST", com.ebin_uppermost);
    printf("%20s   %.2f eV\n", "EBIN_WIDTH", com.ebin_width);
  } else if ( 1 == com.ebin_mode ) {
    printf("%20s   %s\n", "EBINFILE", com.ebinfile);
  }
  printf("%20s   %s\n", "LEAPFILE", mkfname(com.leapfile, com.o_leapfile));
  printf("%20s   %s\n", "QUANTEFFFILE",
	 mkfname(com.quantefffile, com.o_quantefffile));
  printf("%20s   %s\n", "RMFPARAMFILE",
	 mkfname(com.rmfparamfile, com.o_rmfparamfile));
  printf("%20s   %s\n", "MAKEPIFILE",
	 mkfname(com.makepifile, com.o_makepifile));
  printf("\n");
}

void
XISmkResp_startup(int *status)
{
  com.ebin_lowermost  = 0.2;	/* keV */
  com.ebin_uppermost = 16.0;	/* keV */
  com.ebin_width = 2.0;		/* eV */
  com.quantefffile = com.o_quantefffile;
  com.rmfparamfile = com.o_rmfparamfile;
  com.makepifile = com.o_makepifile;
  com.leapfile = com.o_leapfile;

  *status = ANL_OK;
}

void
XISmkResp_com(int *status)
{
  static char *keytbl[] = {
    "PHAFILE",
    "OUTFILE",
    "REBIN",
    "CLOBBER",
    "EDGE_TREATMENT",
    "EBIN_MODE",
    "EBIN_LOWERMOST",
    "EBIN_UPPERMOST",
    "EBIN_WIDTH",
    "EBINFILE",
    "QUANTEFFFILE",
    "RMFPARAMFILE",
    "MAKEPIFILE",
    "LEAPFILE",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "Input filename",
    "Output RMF FITS file name",
    "Rebinning factor",
    "Overwrite output file",
    "How to treat atomic edges (0:ignore,1:shift ebin)",
    "How to determine energy bin (0:const,1:file)",
    "Lowermost energy bin in keV ",
    "Uppermost energy bin in keV ",
    "Constant energy bin width in eV",
    "CALDB file for QE parameters",
    "CALDB file for RMF parameters",
    "CALDB file for Split Threshold parameters",
    "CALDB file for CH2E parameters",
    "leap-seconds file name",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetFname(k="phafile", com.phafile) ||
PILGetFname(k="outfile", com.outfile) ||
PILGetInt  (k="rebin", &com.rebin) ||
PILGetBool (k="clobber", &com.clobber) ||
PILGetBool (k="enable_scirmf", &com.enable_scirmf) ||
PILGetInt  (k="edge_treatment", &com.edge_treatment) ||
PILGetInt  (k="ebin_mode", &com.ebin_mode) ||
PILGetInt  (k="bi_si_edge_mode", &com.bi_si_edge_mode) ||
PILGetInt  (k="fi_si_edge_mode", &com.fi_si_edge_mode) ||
PILGetFname(k="quantefffile", com.o_quantefffile) ||
PILGetFname(k="rmfparamfile", com.o_rmfparamfile) ||
PILGetFname(k="makepifile", com.o_makepifile) ||
PILGetFname(k="leapfile", com.o_leapfile) ||
	 0 ) {
      goto pil_error;
    }
    if ( 0 == com.ebin_mode ) {
      if (
PILGetReal(k="ebin_lowermost", &com.ebin_lowermost) ||
PILGetReal(k="ebin_uppermost", &com.ebin_uppermost) ||
PILGetReal(k="ebin_width", &com.ebin_width) ||
	   0 ) {
	goto pil_error;
      }
    } else if ( 1 == com.ebin_mode ) {
      if (
PILGetFname(k="ebinfile", com.ebinfile) ) {
	goto pil_error;
      }
    }
    com.flag_use_phafile = ANL_YES;
    if ( '\0' == com.phafile[0] || 0 == CLstricmp("NONE", com.phafile) ) {
      com.flag_use_phafile = ANL_NO;
      if (
PILGetString(k="telescop", com.telescop) ||
PILGetString(k="instrume", com.instrume) ||
PILGetString(k="date_obs", com.date_obs) ||
PILGetString(k="clk_mode", com.clk_mode) ||
PILGetString(k="editmode", com.editmode) ||
PILGetInt   (k="winopt", &com.winopt) ||
PILGetInt   (k="ci", &com.ci) ||
	   0 ) {
	goto pil_error;
      }
    }
    *status = ANL_OK;
    return;
 pil_error:
    anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
    *status = ANL_QUIT;
    return;
  }

  for(;;) {
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if ( 0 == strcmp("PHAFILE", key) ) {
      CLtxtrd(key, com.phafile, sizeof(com.phafile));
    } else if ( 0 == strcmp("OUTFILE", key) ) {
      CLtxtrd(key, com.outfile, sizeof(com.outfile));
    } else if ( 0 == strcmp("REBIN", key) ) {
      CLintrd(key, &com.rebin);
    } else if ( 0 == strcmp("CLOBBER", key) ) {
      CLlogrd(key, &com.clobber);
    } else if ( 0 == strcmp("EDGE_TREATMENT", key) ) {
      CLintrd(key, &com.edge_treatment);
    } else if ( 0 == strcmp("EBIN_MODE", key) ) {
      CLintrd(key, &com.ebin_mode);
    } else if ( 0 == strcmp("EBIN_LOWERMOST", key) ) {
      CLfdprd(key, &com.ebin_lowermost);
    } else if ( 0 == strcmp("EBIN_UPPERMOST", key) ) {
      CLfdprd(key, &com.ebin_uppermost);
    } else if ( 0 == strcmp("EBIN_WIDTH", key) ) {
      CLfdprd(key, &com.ebin_width);
    } else if ( 0 == strcmp("EBINFILE", key) ) {
      CLtxtrd(key, com.ebinfile, sizeof(com.ebinfile));
    } else if ( 0 == strcmp("QUANTEFFFILE", key) ) {
      CLtxtrd(key, com.o_quantefffile, sizeof(com.o_quantefffile));
    } else if ( 0 == strcmp("RMFPARAMFILE", key) ) {
      CLtxtrd(key, com.o_rmfparamfile, sizeof(com.o_rmfparamfile));
    } else if ( 0 == strcmp("MAKEPIFILE", key) ) {
      CLtxtrd(key, com.o_makepifile, sizeof(com.o_makepifile));
    } else if ( 0 == strcmp("LEAPFILE", key) ) {
      CLtxtrd(key, com.o_leapfile, sizeof(com.o_leapfile));
    } else if (strcmp (key, "SHOW") == 0) {
      showParam();
    } else if (strcmp (key, "EXIT") == 0) {
      return;
    }
  }

  *status = ANL_OK;
}

static void
show_reading_row_info(char *extname, long validrow)
{
  int len;
  char buf[80];

  sprintf(buf, "%ld", validrow);
  len = strlen(buf);
  switch ( buf[len-1] ) {
  case '1':
    strcat(buf, "st");
    break;
  case '2':
    strcat(buf, "nd");
    break;
  case '3':
    strcat(buf, "rd");
    break;
  default:
    strcat(buf, "th");
    break;
  }
  anl_msg_info("\
reading %s in %s row\n", extname, buf);
}

static void
show_ok(void)
{
  anl_msg_info("\
   ....................................... o.k.\n");
}

void
XISmkResp_init(int *status)
{
  fitsfile *fp;
  char *extname;
  int i, j, verbose, icol, iseg, ipos, hdutype, extver, anynul, fitsStatus = 0;
  long validrow = 0;

  char *sptr;
  char tform2[FLEN_VALUE];
  int edgenum;
  double *edge, *etmp;

  FITS_GET_MEMORY;

  com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
  if ( NULL == com.leapfile ) {
    goto quit;
  }

/* initialize aste_time */
  verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
  if ( -1 == verbose ) anl_msg_info("\n");
  if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
    anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
    goto quit;
  }

          /******************************************************************/
          /* get clock mode, sensor ID, and obervation date from input file */
          /******************************************************************/
  if ( com.flag_use_phafile ) {
    FITS_CHECK_ERROR_ANL(
fits_open_file(&fp, com.phafile, READONLY, &fitsStatus));
    FITS_CHECK_ERROR_ANL(
fits_movabs_hdu(fp, 1, &hdutype, &fitsStatus));	/* goto PRIMARY */

    if ( read_header(fp) ) {
      goto quit;
    }
  } else {
    if ( aefits_datestr2aetime(com.date_obs, &com.obs_time) ) {
      goto quit;
    }
  }

  com.sensor = aste_instrume_id(com.instrume) - ASTE_XIS0_ID;
  if ( com.sensor < 0 || com.sensor > XIStotalXISno ) {
    anl_msg_error("\
%s: illegal INSTRUME = %s\n", pname, com.instrume);
    goto quit;
  }

/* get caldb files */
  if ( NULL == (com.quantefffile = xisrsp_get_caldb_file(com.instrume,
	"EFFICIENCY_CCD", com.o_quantefffile)) ) {
    goto quit;
  }

  if ( NULL == (com.rmfparamfile = xisrsp_get_caldb_file(com.instrume,
	"RMF_PARAMETERS", com.o_rmfparamfile)) ) {
    goto quit;
  }

  if ( NULL == (com.makepifile = xisrsp_get_caldb_file(com.instrume,
	"SPTH_PARAM", com.o_makepifile)) ) {
    goto quit;
  }

  showParam();

  anl_msg_info("\
%s: Creating %s %s RMF for %s %s WINOPT=%d CI=%d mode ...\n",
	pname, com.telescop, com.instrume,
	com.clk_mode, com.editmode, com.winopt,
	com.enable_scirmf ? com.ci : 0);

  /* check clock mode */
  if ( 0 == strcmp("normal", com.clk_mode) ) {
    ;
  } else if ( 0 == strcmp("burst", com.clk_mode) ) {
    anl_msg_warning("\n\
%s: *** WARNING ***\n\
    'xisrmfgen' generates the same RMF files for the burst mode as\n\
    those for the normal mode. So far there is no evidence that\n\
    the response of the XIS in the burst mode is different from\n\
    that in the normal mode. However, quantitative studies about the\n\
    response of the burst mode have not been completed yet.\n\
\n", pname);
  } else if ( 0 == strcmp("psum", com.clk_mode) ) {
    ;	/* supported since XISrmfgen-1.5 */
  } else {
    anl_msg_error("\
%s: ERROR: the clock mode '%s' is not supported.\n", pname, com.clk_mode);
    goto quit;
  }

  /* check edit mode */
  if ( 0 == strcmp("5x5", com.editmode) ||
       0 == strcmp("3x3", com.editmode) ) {
    ;
  } else if ( 0 == strcmp("2x2", com.editmode) ) {
    anl_msg_warning("\n\
%s: *** WARNING ***\n\
    'xisrmfgen' generates the same RMF files for the 2x2 mode data as\n\
    those for the 5x5/3x3 mode data since we found no remarkable\n\
    difference in response between the 2x2 mode data and the 5x5/3x3\n\
    mode data.\n\
\n", pname);
  } else if ( 0 == strcmp("timing", com.editmode) &&
	      0 == strcmp("psum", com.clk_mode) ) {
    ;	/* supported since XISrmfgen-1.5 */
  } else {
    anl_msg_error("\
%s: ERROR: the edit mode '%s' is not supported.\n", pname, com.editmode);
    goto quit;
  }

  /* check window option */
  switch ( com.winopt ) {
  case 0:	/* no window */
    break;
  case 1:	/* 1/4 window */
  case 2:	/* 1/8 window */
  case 3:	/* 1/16 window */
    anl_msg_warning("\n\
%s: *** WARNING ***\n\
    For the window options, 'xisrmfgen' also generates the same\n\
    RMF files as those for observations without the window options.\n\
    However, please note that there is a possibility that the gain and\n\
    resolution of the window options are slightly different from those\n\
    without the options.\n\
\n", pname);
    break;
  default:
    anl_msg_error("\
%s: ERROR: WINOPT=%d is not supported.\n", pname, com.winopt);
    goto quit;
  }

  /* check CI option */
  switch ( com.enable_scirmf ? com.ci : 0 ) {
  case 0:	/* no CI */
  case 2:	/* SCI-54 */
  case 3:	/* SCI-108 */
    break;
  default:	/* diagnostic CI is not supported */
    anl_msg_error("\
%s: ERROR: CI=%d is not supported.\n", pname, com.ci);
    goto quit;
  }

  anl_msg_info("\
date_obs = %.1f [%s]\n", com.obs_time, com.date_obs);

                                      /***************************************/
                                      /* Read weighted map in the input file */
                                      /***************************************/
  if ( com.flag_use_phafile ) {
    if ( read_weight(fp, weight) ) {
      goto quit;
    }
  } else {
    for (i = 0; i < NUM_WEIGHT; i++) {
      weight[i] = 1.0 / NUM_WEIGHT;	/* constant weight on whole CCD */
    }
  }

  if ( com.flag_use_phafile ) {
    FITS_CHECK_ERROR_ANL(
fits_close_file(fp, &fitsStatus));
  }

  hdutype = BINARY_TBL;
  extver = 0;	/* ignore EXTVER */
#define GET_EXTNAME(NAME, NAME_SCI) \
	( 0 == com.enable_scirmf || 0 == com.ci ) ? NAME : NAME_SCI

                     /********************************************************/
                     /* Input Quantum Efficiency parameters from a fits file */
                     /********************************************************/
  extname = GET_EXTNAME( "EFFICIENCY_CCD" , "EFFICIENCY_CCD_SCI" );
  FITS_CHECK_ERROR_ANL(
fits_open_file(&fp, com.quantefffile, READONLY, &fitsStatus));
  ccdqe = xisrsp_efficiency_init();
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_key_str(fp, "TFORM2", tform2, NULL, &fitsStatus));
  ccdqe->size = atol(&tform2[3]);
  ccdqe->energy = malloc(ccdqe->size * sizeof(double));
  ccdqe->value  = malloc(NUM_WEIGHT*ccdqe->size * sizeof(double));
  if ( NULL == ccdqe->energy || NULL == ccdqe->value ) {
    anl_msg_error("\
%s: ERROR: malloc( ccdqe[%ld] ) failed\n", pname, ccdqe->size);
    goto quit;
  }

  if ( check_instrume(fp) ) {
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  /* Read CCD-QE values */
  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "ENERGY", &icol, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol++, validrow, 1, ccdqe->size, 0.0,
			ccdqe->energy, &anynul, &fitsStatus));
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ipos = 0; ipos < XIStotalPosNo; ipos++) {
      FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp,
		iseg*XIStotalPosNo+ipos+icol, validrow, 1, ccdqe->size,
		0.0, ccdqe->value+(iseg*XIStotalPosNo+ipos)*ccdqe->size,
		&anynul, &fitsStatus));
    }
  }
  show_ok();
  /*fprintf(stdout,"CCDQE value = %lf, %lf, ........\n",ccdqe->value[0],ccdqe->value[1] );*/

  extname = "EFFICIENCY_OBF";	/* same for SCI-off and SCI-on */
  obfqe = xisrsp_efficiency_init();
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_key_str(fp, "TFORM2", tform2, NULL, &fitsStatus));
  obfqe->size = atol(&tform2[3]);
  obfqe->energy = malloc(obfqe->size * sizeof(double));
  obfqe->value  = malloc(NUM_WEIGHT*obfqe->size * sizeof(double));
  if ( NULL == obfqe->energy || NULL == obfqe->value ) {
    anl_msg_error("\
%s: ERROR: malloc( obfqe[%ld] ) failed\n", pname, obfqe->size);
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  /* Read OBF-QE values */
  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "ENERGY", &icol, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol++, validrow, 1, obfqe->size, 0.0,
			obfqe->energy, &anynul, &fitsStatus));
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ipos = 0; ipos < XIStotalPosNo; ipos++) {
      FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp,
		iseg*XIStotalPosNo+ipos+icol, validrow, 1, obfqe->size,
	        0.0, obfqe->value+(iseg*XIStotalPosNo+ipos)*obfqe->size,
		&anynul, &fitsStatus));
    }
  }
  show_ok();
  /*fprintf(stdout,"OBFQE value = %lf, %lf, ........\n",obfqe->value[0],obfqe->value[1] );*/

                    /*********************************************************/
                    /* Input Quantum Efficiency Edge energy from a fits file */
                    /*********************************************************/
  extname = GET_EXTNAME( "EDGE_CCD" , "EDGE_CCD_SCI" );
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_key_str(fp, "TFORM2", tform2, NULL, &fitsStatus));
  ccdqe->edgenum = atol(&tform2[3]);
  ccdqe->edge  = malloc(ccdqe->edgenum * sizeof(double));
  if ( NULL == ccdqe->edge ) {
    anl_msg_error("\
%s: ERROR: malloc( ccdqe->edge[%ld] ) failed\n", pname, ccdqe->edgenum);
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  /* Read CCD QE edges */
  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "ENERGY", &icol, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol, validrow, 1, ccdqe->edgenum, 0.0,
		  ccdqe->edge, &anynul, &fitsStatus));
  show_ok();

  extname = "EDGE_OBF";		/* same for SCI-off and SCI-on */
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_key_str(fp, "TFORM2", tform2, NULL, &fitsStatus));
  obfqe->edgenum = atol(&tform2[3]);
  obfqe->edge  = malloc(obfqe->edgenum * sizeof(double));
  if ( NULL == obfqe->edge ) {
    anl_msg_error("\
%s: ERROR: malloc( obfqe->edge[%ld] ) failed\n", pname, obfqe->edgenum);
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  /* Read OBF QE edges */
  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "ENERGY", &icol, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol, validrow, 1, obfqe->edgenum, 0.0,
		  obfqe->edge, &anynul, &fitsStatus));
  show_ok();

  FITS_CHECK_ERROR_ANL(
fits_close_file(fp, &fitsStatus));


                                    /*****************************************/
                                    /* Input RMF parameters from a fits file */
                                    /*****************************************/
  extname = GET_EXTNAME( "RMF_PARAMETERS" , "RMF_PARAMETERS_SCI" );
  param = xisrsp_respparam_init(com.bi_si_edge_mode, com.fi_si_edge_mode);
  FITS_CHECK_ERROR_ANL(
fits_open_file(&fp,com.rmfparamfile, READONLY, &fitsStatus));
  if ( 0 == strcmp("psum", com.clk_mode) ) {
    extname = "RMF_PARAMETERS_PSUM";
    if (
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus) ) {
      anl_msg_error("\
%s: ERROR: the rmfparam file '%s' is not supported in the Psum mode.\n\
    New CALDB file is required.\n", pname, com.rmfparamfile);
    }
    FITS_CHECK_ERROR_ANL(fitsStatus);
  } else {
    FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  }
  FITS_CHECK_ERROR_ANL(
fits_read_key_str(fp, "TFORM2", tform2, NULL, &fitsStatus));
  param->size = atol(&tform2[3]);
  param->value = malloc(NUM_WEIGHT*param->size * sizeof(double));
  if ( NULL == param->value ) {
    anl_msg_error("\
%s: ERROR: malloc( param->value[%ld] ) failed\n", pname, param->size);
    goto quit;
  }

  if ( check_instrume(fp) ) {
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "Param_s0_p0", &icol, &fitsStatus));

  /* Read RMF parameters */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ipos = 0; ipos < XIStotalPosNo; ipos++) {
      FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp,
		iseg*XIStotalPosNo+ipos+icol, validrow, 1, param->size, 0.0,
		param->value+(iseg*XIStotalPosNo+ipos)*param->size,
		&anynul, &fitsStatus));
    }
  }
  show_ok();

  FITS_CHECK_ERROR_ANL(
fits_close_file(fp, &fitsStatus));

                          /***************************************************/
                          /* Input SpTh and Gain parameters from a fits file */
                          /***************************************************/
  extname = "SPTH_PARAM";		/* same for SCI-off and SCI-on */
  spthp = xisrsp_spthParam_init();
  gainp = xisrsp_gainParam_init();
  FITS_CHECK_ERROR_ANL(
fits_open_file(&fp,com.makepifile, READONLY, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  spthp->offset  = malloc(spthp->size * sizeof(double));
  spthp->slope   = malloc(spthp->size * sizeof(double));
  spthp->minimum = malloc(spthp->size * sizeof(double));
  if ( NULL== spthp->offset || NULL== spthp->slope || NULL== spthp->minimum ) {
    anl_msg_error("\
%s: ERROR: malloc( spthp[%ld] ) failed\n", pname, spthp->size);
    goto quit;
  }

  if ( check_instrume(fp) ) {
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "SEGMENT", &icol, &fitsStatus));

  /* Read SPTH parameters */
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+1, validrow, 1, spthp->size, 0.0,
		  spthp->offset, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+2, validrow, 1, spthp->size, 0.0,
		  spthp->slope, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+3, validrow, 1, spthp->size, 0.0,
		  spthp->minimum, &anynul, &fitsStatus));
  show_ok();

  extname = GET_EXTNAME( "GAIN_NORMAL" , "GAIN_NORMAL_SCI" );
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(fp, hdutype, extname, extver, &fitsStatus));
  gainp->offl  = malloc(gainp->size * sizeof(double));
  gainp->linrl = malloc(gainp->size * sizeof(double));
  gainp->quadl = malloc(gainp->size * sizeof(double));
  gainp->offh  = malloc(gainp->size * sizeof(double));
  gainp->linrh = malloc(gainp->size * sizeof(double));
  gainp->quadh = malloc(gainp->size * sizeof(double));
  if ( NULL == gainp->offl  || NULL == gainp->linrl || NULL == gainp->quadl ||
       NULL == gainp->offh  || NULL == gainp->linrh || NULL == gainp->quadh ) {
    anl_msg_error("\
%s: ERROR: malloc( gainp[%ld] ) failed\n", pname, gainp->size);
    goto quit;
  }

  /* Seek proper caltime value */
  validrow = xisrsp_seek_valid_row(fp, com.obs_time);
  show_reading_row_info(extname, validrow);

  FITS_CHECK_ERROR_ANL(
fits_get_colnum(fp, CASEINSEN, "SEGMENT", &icol, &fitsStatus));

  /* Read Gain parameters */
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+1, validrow, 1, gainp->size, 0.0,
		  gainp->quadl, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+2, validrow, 1, gainp->size, 0.0,
		  gainp->linrl, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+3, validrow, 1, gainp->size, 0.0,
		  gainp->offl, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+4, validrow, 1, gainp->size, 0.0,
		  gainp->quadh, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+5, validrow, 1, gainp->size, 0.0,
		  gainp->linrh, &anynul, &fitsStatus));
  FITS_CHECK_ERROR_ANL(
fits_read_col_dbl(fp, icol+6, validrow, 1, gainp->size, 0.0,
		  gainp->offh, &anynul, &fitsStatus));
  show_ok();

  FITS_CHECK_ERROR_ANL(
fits_close_file(fp, &fitsStatus));

                                                     /************************/
                                                     /* Set Energy Bin Width */
                                                     /************************/
  energy_bin = xisrsp_ebin_init();
  if ( 0 == com.ebin_mode ) {
    anl_msg_info("\
adopting constant energy bin\n");
    if ( xisrsp_ebin_init_const(energy_bin,
		com.ebin_lowermost, com.ebin_uppermost, com.ebin_width) ) {
      goto quit;
    }
  } else if ( 1 == com.ebin_mode ) {
    anl_msg_info("\
reading energy bin from file\n");
    if ( xisrsp_ebin_init_file(energy_bin, com.ebinfile) ) {
      goto quit;
    }
  } else {
    anl_msg_error("\
%s: ERROR: unknown ebin_mode=%d\n", pname, com.ebin_mode);
    goto quit;
  }
  show_ok();

                                                           /******************/
                                                           /* retrieve edges */
                                                           /******************/
  anl_msg_info("\
sorting Edges\n");
  xisresp = xisrsp_init(com.sensor);
  edge = NULL;
  if ( 0 == com.edge_treatment ) {
    edgenum = 0;	/* ignore edge */
  } else if ( 1 == com.edge_treatment ) {
    edgenum = ccdqe->edgenum + obfqe->edgenum + xisrsp_edgeNum(xisresp);
  } else {
    anl_msg_error("\
%s: ERROR: unknown edge_treatment=%d\n", pname, com.edge_treatment);
    goto quit;
  }

  if ( 0 != edgenum ) {
    edge = malloc((edgenum + 1) * sizeof(*edge));
    if ( NULL == edge ) {
      anl_msg_error("\
%s: ERROR: malloc( edge[%d] ) failed\n", pname, edgenum+1);
      goto quit;
    }

    i = 0;
    if ( 0 != (j = ccdqe->edgenum) ) {
      memcpy(edge+i, ccdqe->edge, j*sizeof(*edge));
      i += j;
    }
    if ( 0 != (j = obfqe->edgenum) ) {
      memcpy(edge+i, obfqe->edge, j*sizeof(*edge));
      i += j;
    }
    if ( 0 != (j = xisrsp_edgeNum(xisresp)) ) {
      memcpy(edge+i, xisresp->edge, j*sizeof(*edge));
      i += j;
    }
    edgenum = i;
    for (i = 0; i < edgenum; i++) {
      edge[i] = ebin_trunc( edge[i] );	/* ignore digits less than 1e-6 keV */
    }
    qsort(edge, i, sizeof(*edge), ebin_comp_for_qsort);
    i = 1;
    while ( i < edgenum ) {
      if ( 0 == ebin_comp(edge[i-1], edge[i]) ) {
	for (j = i + 1; j < edgenum; j++) {
          edge[j-1] = edge[j];
	}
	edgenum--;
      } else {
	i++;
      }
    }
    edge = realloc(edge, edgenum * sizeof(*edge));
  }

  com.num_edge = edgenum;
  etmp = edge; /* save it */
  for (i = 0; 0 < edgenum && i < energy_bin->size; i++) {
    double e, e_lo, e_hi;
    e_lo = energy_bin->energ_lo[i];
    e_hi = energy_bin->energ_hi[i];
    while ( 0 < edgenum && ebin_comp(*edge, e_lo) < 0 ) {
      edge++;
      edgenum--;
    }
    e = *edge;
    if ( 0 < edgenum && ebin_comp(e_lo, e) < 0 && ebin_comp(e, e_hi) < 0 ) {
      anl_msg_debug("\
found edge at %.6f keV\n", e);
      if ( ebin_comp(e - e_lo, e_hi - e) <= 0 ) {
	energy_bin->energ_lo[i] = e;
	if ( 0 < i ) {
	  energy_bin->energ_hi[i-1] = e;
	}
      } else {
	energy_bin->energ_hi[i] = e;
	if ( i + 1 < energy_bin->size ) {
	  energy_bin->energ_lo[i+1] = e;
	}
      }
      edge++;
      edgenum--;
    }
  }
  if ( NULL != etmp ) free(etmp);

  /* ignore digits less than 1.0e-6 keV */
  for (i = 0; i < energy_bin->size; i++) {
    energy_bin->energ_lo[i] = ebin_trunc(energy_bin->energ_lo[i]);
    energy_bin->energ_hi[i] = ebin_trunc(energy_bin->energ_hi[i]);
  }

  if ( ANL_MSG_DEBUG3 <= anl_msg_chatter() ) {
    anl_msg_debug3("\
%s: DEBUG3: print energy bin ...\n", pname);
    for (i = 0; i < energy_bin->size; i++) {
      anl_msg_debug3("\
%4d %.15f %.15f\n", i+1, energy_bin->energ_lo[i], energy_bin->energ_hi[i]);
    }
    anl_msg_debug3("\
%s: DEBUG3: ... done\n", pname);
  }
  show_ok();

                                   /******************************************/
                                   /* prepare efficiency and response tables */
                                   /******************************************/
  anl_msg_info("\
weighting Q.E. and RMF parameters\n");
  wccdqe = xisrsp_efficiency_init();
  xisrsp_calc_weighted_qe(ccdqe, wccdqe, weight);
  wobfqe = xisrsp_efficiency_init();
  xisrsp_calc_weighted_qe(obfqe, wobfqe, weight);
  wparam = xisrsp_respparam_init(com.bi_si_edge_mode, com.fi_si_edge_mode);
  xisrsp_calc_weighted_param(param, wparam, weight);
  wspthp = xisrsp_spthParam_init();
  wgainp = xisrsp_gainParam_init();
  xisrsp_calc_weighted_spthp(spthp, wspthp, weight);
  xisrsp_calc_weighted_gainp(gainp, wgainp, weight);
  show_ok();

  anl_msg_info("\n");

  sptr = com.telescop;
  BnkDef("XIS:TELESCOP:PTR", sizeof(sptr));
  BnkPut("XIS:TELESCOP:PTR", sizeof(sptr), &sptr);
  sptr = com.instrume;
  BnkDef("XIS:INSTRUME:PTR", sizeof(sptr));
  BnkPut("XIS:INSTRUME:PTR", sizeof(sptr), &sptr);
  sptr = com.date_obs;
  BnkDef("XIS:DATE_OBS:PTR", sizeof(sptr));
  BnkPut("XIS:DATE_OBS:PTR", sizeof(sptr), &sptr);
  BnkDef("XIS:OBS_TIME", sizeof(com.obs_time));
  BnkPut("XIS:OBS_TIME", sizeof(com.obs_time), &com.obs_time);
  sptr = com.clk_mode;
  BnkDef("XIS:CLK_MODE:PTR", sizeof(sptr));
  BnkPut("XIS:CLK_MODE:PTR", sizeof(sptr), &sptr);
  sptr = com.editmode;
  BnkDef("XIS:EDITMODE:PTR", sizeof(sptr));
  BnkPut("XIS:EDITMODE:PTR", sizeof(sptr), &sptr);
  BnkDef("XIS:WINOPT", sizeof(int));
  BnkPut("XIS:WINOPT", sizeof(int), &com.winopt);
  BnkDef("XIS:CI", sizeof(int));
  BnkPut("XIS:CI", sizeof(int), &com.ci);

/*
  BnkDef("XIS:MATRIX:N_ROWS", sizeof(energy_bin->size));
  BnkPut("XIS:MATRIX:N_ROWS", sizeof(energy_bin->size), &energy_bin->size);

  BnkDef("XIS:MATRIX:ENERGY_BIN", sizeof(energy_bin));
  BnkPut("XIS:MATRIX:ENERGY_BIN", sizeof(energy_bin), &energy_bin);
*/

                           /*************************************************/
                           /* Bnk def & put column info in matrix extension */
                           /*************************************************/
  xirsp_bnkdef_matrix();

                                      /**************************************/
                                      /* Bnk def & put for ebound extension */
                                      /**************************************/
  xirsp_bnkdef_ebound();

  sptr = com.quantefffile;
  BnkDef("XIS:QEFILE:PTR", sizeof(sptr));
  BnkPut("XIS:QEFILE:PTR", sizeof(sptr), &sptr);

  sptr = com.rmfparamfile;
  BnkDef("XIS:RMFPARAMFILE:PTR", sizeof(sptr));
  BnkPut("XIS:RMFPARAMFILE:PTR", sizeof(sptr), &sptr);

  sptr = com.makepifile;
  BnkDef("XIS:MAKEPIFILE:PTR", sizeof(sptr));
  BnkPut("XIS:MAKEPIFILE:PTR", sizeof(sptr), &sptr);

  EvsDef("XISmkResp:BEGIN");
  EvsDef("XISmkResp:ENTRY");
  EvsDef("XISmkResp:OK");

  irow = 1L;

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISmkResp_his(int *status)
{
  *status = ANL_OK;
}

void
XISmkResp_bgnrun(int *status)
{
  EvsfSetM("XISmkResp:BEGIN");
  *status = ANL_OK;
}

void
XISmkResp_ana(int nevent, int eventid, int *status)
{
  double E, e_lo, e_hi, pha, spth, factor;

  EvsfSetM("XISmkResp:ENTRY");

  if ( energy_bin->size < irow ) {
    *status = ANL_QUIT;
    return;
  }

  e_lo = xisrsp_ebin_lo(energy_bin, irow-1);
  e_hi = xisrsp_ebin_hi(energy_bin, irow-1);
  E = (e_lo + e_hi) / 2.0;

                                                        /*********************/
                                                        /* make the response */
                                                        /*********************/
  /* determine spth from E */
  if ( Si_Kedge < E ) {
    if ( gainp->quadh[0] != 0.0 ) {
      pha = (-1.0*gainp->linrh[0] +
	     pow( pow(gainp->linrh[0],2.0) -
		 4.0 * gainp->quadh[0] * (gainp->offh[0] - E*1000) , 0.5 )
	     ) / 2.0 / gainp->quadh[0];
    } else {
      pha = ( E*1000 - gainp->offh[0] ) / gainp->linrh[0];
    }
  } else {
    if ( gainp->quadl[0] != 0.0 ) {
      pha = (-1.0*gainp->linrl[0] +
	     pow( pow(gainp->linrl[0],2.0) -
		 4.0 * gainp->quadl[0] * (gainp->offl[0] - E*1000) , 0.5 )
	     ) / 2.0 / gainp->quadl[0];
    } else {
      pha = ( E*1000 - gainp->offl[0] ) / gainp->linrl[0];
    }
  }

  spth = wspthp->minimum[0]  +
	 wspthp->slope[0] * log10(pha) + wspthp->offset[0];
  if ( spth < wspthp->minimum[0] ) {
    spth = wspthp->minimum[0];
  }

/*  fprintf(stdout,"spth = %lf, Energy= %lf, pha=%lf \n",spth, E, pha);
  fprintf(stdout,"Energy=%.8le, ccdqe=%.8le, obfqe=%.8le\n",E,
      efficiency_value(wccdqe, E),efficiency_value(wobfqe, E));*/

  factor = xisrsp_efficiency_value(wccdqe, E)
	 * xisrsp_efficiency_value(wobfqe, E);
  slice = xisrsp_resp(xisresp, E, factor, com.sensor, wparam, spth);
/*  fprintf(stdout,"E= %lf, s.array[0]= %.10le, s.size = %d \n",E,
      slice.array[0], slice.size);*/

  xisrsp_bnkput_matrix(e_lo, e_hi, &slice);

  EvsfSetM("XISmkResp:OK");

  *status = ANL_OK;
}

void
XISmkResp_endrun(int *status)
{
  char buf[2*PIL_LINESIZE];
  int istat = 0;

                          /*************************************************/
                          /* Write a HISTORY keyword into output fits file */
                          /*************************************************/

  sprintf(buf, "  phafile='%s'", com.phafile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  outfile='%s'", com.outfile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  clobber=%s  enable_scirmf=%s  rebin=%d",
	com.clobber ? "yes":"no", com.enable_scirmf ? "yes" : "no", com.rebin);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  telescop='%s'  instrume='%s'", com.telescop, com.instrume);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  date_obs='%s' (t=%.1f)", com.date_obs, com.obs_time);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  clk_mode='%s'  editmode='%s'  winopt=%d  ci=%d",
	  com.clk_mode, com.editmode, com.winopt, com.ci);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  edge_treatment=%d:%s (num_edge=%d)", com.edge_treatment,
	  (0 == com.edge_treatment) ? "IGNORE" :
	  (1 == com.edge_treatment) ? "SHIFT_EBIN" : "ERROR", com.num_edge);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  bi_si_edge_mode=%d  fi_si_edge_mode=%d",
	  com.bi_si_edge_mode, com.fi_si_edge_mode);
  fits_write_history(ofp, buf, &istat);
  if ( 0 == com.ebin_mode ) {
    sprintf(buf, "\
  ebin_mode=0:CONST  ebin_lowermost=%.1f keV  ebin_uppermost=%.1f keV",
	    com.ebin_lowermost, com.ebin_uppermost);
    fits_write_history(ofp, buf, &istat);
    sprintf(buf, "  ebin_width=%.2f eV", com.ebin_width);
    fits_write_history(ofp, buf, &istat);
  } else if ( 1 == com.ebin_mode ) {
    sprintf(buf, "ebin_mode=1:FILE");
    fits_write_history(ofp, buf, &istat);
    sprintf(buf, "  ebinfile='%s'",com.ebinfile);
    fits_write_history(ofp, buf, &istat);
  }
  mkfname_history(buf, "leapfile", com.leapfile, com.o_leapfile);
  fits_write_history(ofp, buf, &istat);
  mkfname_history(buf, "quantefffile", com.quantefffile, com.o_quantefffile);
  fits_write_history(ofp, buf, &istat);
  mkfname_history(buf, "rmfparamfile", com.rmfparamfile, com.o_rmfparamfile);
  fits_write_history(ofp, buf, &istat);
  mkfname_history(buf, "makepifile", com.makepifile, com.o_makepifile);
  fits_write_history(ofp, buf, &istat);

  fits_write_history(ofp, "\
Weight of paramters:", &istat);
  sprintf(buf, "\
-----------------+-----------------+-----------------+-----------------");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
%.2e %.2e|%.2e %.2e|%.2e %.2e|%.2e %.2e",
	*(weight+2), *(weight+3), *(weight+6), *(weight+7),
	*(weight+10), *(weight+11), *(weight+14), *(weight+15));
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
                 |                 |                 |");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
    SEGMENT A    |    SEGMENT B    |    SEGMENT C    |    SEGMENT D");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
                 |                 |                 |");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
%.2e %.2e|%.2e %.2e|%.2e %.2e|%.2e %.2e",
	*(weight+0), *(weight+1), *(weight+4), *(weight+5),
	*(weight+8), *(weight+9), *(weight+12), *(weight+13));
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "\
-----------------+-----------------+-----------------+-----------------");
  fits_write_history(ofp, buf, &istat);

  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XISmkResp_exit(int *status)
{
  xisrsp_ebin_term(energy_bin);	/* this must be after XISwriteResp_endrun() */
  xisrsp_efficiency_term(ccdqe);
  xisrsp_efficiency_term(obfqe);
  xisrsp_term(xisresp);

  *status = ANL_OK;
}

/***************************************************************************
  XISwriteResp
***************************************************************************/

static int
writeTelescopInstrume(fitsfile *ofp)
{
  char *k, *telescop, *instrume, date_obs[FLEN_VALUE];
  char *clk_mode, *editmode;
  int winopt, ci;
  double obs_time;
  int used;
  int istat = 0;

  BnkfGetM("XIS:TELESCOP:PTR", sizeof(telescop), &used, &telescop);
  BnkfGetM("XIS:INSTRUME:PTR", sizeof(instrume), &used, &instrume);
/*  BnkfGetM("XIS:DATE_OBS:PTR", sizeof(date_obs), &used, &date_obs);*/
  BnkfGetM("XIS:OBS_TIME", sizeof(obs_time), &used, &obs_time);
  aefits_aetime2datestr(obs_time, date_obs);
  BnkfGetM("XIS:CLK_MODE:PTR", sizeof(clk_mode), &used, &clk_mode);
  BnkfGetM("XIS:EDITMODE:PTR", sizeof(editmode), &used, &editmode);
  BnkfGetM("XIS:WINOPT", sizeof(winopt), &used, &winopt);
  BnkfGetM("XIS:CI", sizeof(ci), &used, &ci);

  if (
fits_write_key_str(ofp, k="TELESCOP", telescop,
		   "Telescope (mission) name", &istat) ||
fits_write_key_str(ofp, k="INSTRUME", instrume,
		   "Instrument name", &istat) ||
fits_write_key_str(ofp, k="DATE-OBS", date_obs,
		   "date of observation (yyyy-mm-ddThh:mm:ss UTC)", &istat) ||
fits_write_key_fixdbl(ofp, k="OBS_TIME", obs_time, 6,
		   "mission time of the observation date (s)", &istat) ||
fits_write_key_str(ofp, k="CLK_MODE", clk_mode,
		   "how the CCD clocks are driven by the AE", &istat) ||
fits_write_key_str(ofp, k="EDITMODE", editmode,
		   "how detected events are edited by the DE", &istat) ||
fits_write_key(ofp, TINT, k="WINOPT", &winopt,
		   "window option (0:Off, 1:1/4, 2:1/8, 3:1/16)", &istat) ||
fits_write_key(ofp, TINT, k="CI", &ci,
		   "0:no CI,1:diagn.CI,2:SCI-54rows,3:SCI-108rows", &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  return istat;
}

static int
writeCaldbFiles(fitsfile *ofp)
{
  char *k, *qefile, *rmfparamfile, *makepifile;
  int used;
  int istat = 0;

  BnkGet("XIS:QEFILE:PTR", sizeof(qefile), &used, &qefile);
  BnkGet("XIS:RMFPARAMFILE:PTR", sizeof(rmfparamfile), &used, &rmfparamfile);
  BnkGet("XIS:MAKEPIFILE:PTR", sizeof(makepifile), &used, &makepifile);

  if (
fits_update_key_str(ofp, k="QE_TABLE", aefits_basename(qefile),
		      "quanteff file name", &istat) ||
fits_update_key_str(ofp, k="RMFPARAM", aefits_basename(rmfparamfile),
		      "rmfparam file name", &istat) ||
fits_update_key_str(ofp, k="PI_TABLE", aefits_basename(makepifile),
		      "makepi file name", &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  return istat;
}

static int
write_timestamp(fitsfile *fp)
{
  char history[FLEN_COMMENT];
  char *task_name, *task_version;
  int timeref;
  char date_str[FLEN_VALUE];
  int istat = 0;

  /* get the task name */
  task_name = anl_task_name();
  task_version = anl_task_version();

  /* get the current date */
  fits_get_system_time(date_str, &timeref, &istat);

  /* construct history string */
  *history = '\0';
  strncat(history, "*** ", sizeof(history)-strlen(history)-1);
  strncat(history, task_name, sizeof(history)-strlen(history)-1);
  strncat(history, " version ", sizeof(history)-strlen(history)-1);
  strncat(history, task_version, sizeof(history)-strlen(history)-1);
  strncat(history, " finished at ", sizeof(history)-strlen(history)-1);
  strncat(history, date_str, sizeof(history)-strlen(history)-1);
  strncat(history, " ***", sizeof(history)-strlen(history)-1);

  /*write out the history */
  fits_write_history(fp, history, &istat);

  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    return istat;
  }

  return 0;
}

void
XISwriteResp_startup(int *status)
{
  com.rebin = 1;
  strcpy(com.outfile, "out.fits");
  *status = ANL_OK;
}

void
XISwriteResp_com(int *status)
{
  *status = ANL_OK;
}

void
XISwriteResp_init(int *status)
{
  long nrows;

  int istat = 0;

  nrows = energy_bin->size;

  /* create blank output RMF */
  if ( com.clobber ) {
    unlink(com.outfile);
  }
  fits_create_file(&ofp, com.outfile, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, com.outfile, istat);
    goto quit;
  }

/* create primary extension */
  fits_create_img(ofp, SHORT_IMG, 0, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_img() failed (%d)\n", pname, istat);
    goto quit;
  }
  if (
writeTelescopInstrume(ofp) ) {
    goto quit;
  }
  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    goto quit;
  }

/* create 1st extension */
  istat = xisrsp_create_matrix_ext(ofp, nrows, com.rebin);
  if ( istat ) {
    goto quit;
  }

  if (
writeTelescopInstrume(ofp) ||
writeCaldbFiles(ofp) ||
xisrspPutMatrixHeader(ofp, com.outfile, energy_bin, com.rebin) ||
aefits_write_module_history(ofp, pname) ||
aefits_write_tool_version(ofp, xisrsp_pname(), xisrsp_version()) ||
       0 ) {
    goto quit;
  }

  /* bnkdef/put output file pointer */
  BnkDef("XIS:OUTFITS:PTR", sizeof(ofp));
  BnkPut("XIS:OUTFITS:PTR", sizeof(ofp), &ofp);

  EvsDef("XISwriteResp:BEGIN");
  EvsDef("XISwriteResp:ENTRY");
  EvsDef("XISwriteResp:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISwriteResp_his(int *status)
{
  *status = ANL_OK;
}

void
XISwriteResp_bgnrun(int *status)
{
  EvsfSetM("XISwriteResp:BEGIN");
  *status = ANL_OK;
}

void
XISwriteResp_ana(int nevent, int eventid, int *status)
{

  EvsfSetM("XISwriteResp:ENTRY");
                                                       /**********************/
                                                       /* write the response */
                                                       /**********************/
  /* BnkGet column info and write them to event fits */
  if ( xisrsp_fitswrite_matrix(ofp, irow, com.rebin) ) {
    *status = ANL_QUIT;
    return;
  }

  irow++;

  EvsfSetM("XISwriteResp:OK");
  *status = ANL_OK;
}

void
XISwriteResp_endrun(int *status)
{
  int i, channel;
  double Emin, Emax;

  int istat = 0;

                          /*************************************************/
                          /* Write a HISTORY keyword into output fits file */
                          /*************************************************/

  istat = write_timestamp(ofp);
  if ( istat ) {
    goto quit;
  }

  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    goto quit;
  }

/* create 2nd extension */
  istat = xisrsp_create_ebound_ext(ofp, 0L, com.rebin);
  if ( istat ) {
    goto quit;
  }

  if (
writeTelescopInstrume(ofp) ||
xisrspPutEboundHeader(ofp, com.outfile, energy_bin, com.rebin) ) {
    goto quit;
  }

  for (i = 0; i < PHA_CHAN; i++){
    Emin = BIN_ENE * i;
    Emax = BIN_ENE * (i+1);
    if ( 0 == i ) {
      Emin = BIN_ENE * 0.1;	/* old SHERPA fails when start from 0.0 */
    }

    channel = i + RESP_BASE;	/* RESP_BASE (PI start channel) is 0 for XIS */
    xisrsp_bnkput_ebound(channel, Emin, Emax);
                                                       /**********************/
                                                       /*  write the EBOUND  */
                                                       /**********************/
    if ( xisrsp_fitswrite_ebound(ofp, i+1, com.rebin) ) {
      goto quit;
    }
  }

  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    goto quit;
  }

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISwriteResp_exit(int *status)
{
  int istat = 0;

  /* close outfile */
  fits_close_file(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
