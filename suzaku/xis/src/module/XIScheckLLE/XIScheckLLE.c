/**************************************

  XIScheckLLE

    ver 1.0 2009/03/05	Y. ISHISAKI
	made from XIScheckEventNo-1.2 & mkllgti_v1.4.pl

    ver 1.1 2009/03/08	Y. ISHISAKI
	write LIGHT_LEAK values in the 2nd extension of output FITS
	write BAD_LLE_STATISTICS values in the 3rd extension of output FITS
	change keywords N_LLEERR, T_LLEERR -> N_REJECT, T_REJECT
	update NAXIS2 keywords in GTI extension in write_gti()
	free allocated memory in _exit()
	check solitary only for not unchanged in _exit()

    ver 1.2 2009/03/23	Y. ISHISAKI
	bug fix (int -> short) in compare_short_integer()

**************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "pil.h"
#include "aste_gti.h"
#include "aste_caldb.h"
#include "xisTelemFormat.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "XIScheckLLE";
char XIScheckLLE_version[] = "version 1.2";

#define MaxBndry	64
#define MaxBndryH	4
#define MaxBndryV	16

static struct {
  char *llebndryfile, o_llebndryfile[PIL_LINESIZE];
  char areaid[PIL_LINESIZE];
  double gapsec;
  double mediandiff;
  int constinterval;
  int default_llebndry_h;
  int default_llebndry_v;
  int clobber;

  fitsfile *ofp;
  char *infile;
  char *outfile;
  int col_start, col_stop;
  long num_frames;

  long num_llebndry_history;
  struct llebndry {
    double aetime;
    unsigned char direction;
    unsigned char lle_boundary;
  } *llebndry_history;

  unsigned char area_to_check[XIStotalSegNo][MaxBndry];

} com;

static GTI_DATA gti;
static double *frame_st_aetime;
static double *frame_end_aetime;
static char *flags_unchanged;
static char *flags_solitary;
static int initflag;
static int ithframe;
static short last_lle[XIStotalSegNo][MaxBndry];
static int last_unchanged[XIStotalSegNo][MaxBndry];
static int statistics_solitary[XIStotalSegNo][MaxBndry];
static int statistics_unchanged[XIStotalSegNo][MaxBndry];
static struct light_leak {
  unsigned char nh, nv, nb;
  double aetime;
  short val[XIStotalSegNo][MaxBndry];
} *lldata;

static int nbmax = 0;
static int n_tested = 0;
static int n_passed = 0;
static int n_reject = 0;
static double t_tested = 0.0;
static double t_passed = 0.0;
static double t_reject = 0.0;

static struct {
  char *N_FRAMES;
  char *N_TESTED;
  char *N_PASSED;
  char *N_T_JUMP;
  char *N_REJECT;
  char *T_TESTED;
  char *T_PASSED;
  char *T_T_JUMP;
  char *T_REJECT;
} cm = {
  "number of frames in the input event file",
  "number of non-zero frames tested",
  "number of frames passed the test",
  "number of frames detected time jump",
  "number of frames rejected as LLE error",
  "exposure of non-zero frames tested",
  "exposure of frames passed the test",
  "loss of exposure due to time jump",
  "exposure of frames rejected as LLE error"
};

/*static FILE *fp;*/

static void
showParam(void)
{
  printf ("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   '%s'%s\n", "LLEBNDRYFILE", com.llebndryfile,
	com.llebndryfile == com.o_llebndryfile ? "" : " (CALDB)");
  printf("%20s   %d\n", "DEFAULT_LLEBNDRY_H", com.default_llebndry_h);
  printf("%20s   %d\n", "DEFAULT_LLEBNDRY_V", com.default_llebndry_v);
  printf("%20s   '%s'\n", "AREAID", com.areaid);
  printf("%20s   %d\n", "CONSTINTERVAL", com.constinterval);
  printf("%20s   %.1f\n", "MEDIANDIFF", com.mediandiff);
  printf("%20s   %.6f (s)\n", "GAPSEC", com.gapsec);
  printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
  printf("\n");
}

static int
read_llebndry(char *fn)
{
  static char *extname = "LLE_BOUNDARY";

  int col_time, col_direction, col_lle_boundary;
  int icol, anynul;
  long irow, nrows;
  char *k;

  fitsfile *fp = NULL;
  int istat = 0;

  if ( 0 == CLstricmp("NONE", com.llebndryfile) ) {
    com.num_llebndry_history = 0;
    anl_msg_warning("\
%s: WARNING: llebndryfile='none', using default values of H:%d, V:%d\n",
	pname, com.default_llebndry_h, com.default_llebndry_v);
    return 0;
  }

  anl_msg_info("\
%s: reading '%s' ...\n", pname, com.llebndryfile);

  if ( fits_open_file(&fp, com.llebndryfile, READONLY, &istat) ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, com.llebndryfile, istat);
    goto quit;
  }
  if ( fits_movnam_hdu(fp, BINARY_TBL, extname, 0, &istat) ) {
    anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
    goto quit;
  }
  if (
fits_read_key_lng(fp, k="NAXIS2", &nrows, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  com.llebndry_history = malloc(sizeof(*com.llebndry_history) * nrows);
  if ( NULL == com.llebndry_history ) {
    anl_msg_error("\
%s: can't allocate memory for llebndry_history\n", pname);
    goto quit;
  }

  if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="DIRECTION", &col_direction, &istat) ||
fits_get_colnum(fp, CASESEN, k="LLE_BOUNDARY", &col_lle_boundary, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  for (irow = 1; irow <= nrows; irow++) {
    struct llebndry *lp = &com.llebndry_history[irow-1];;
    if (
fits_read_col_dbl(fp, icol=col_time, irow, 1, 1, 0.0,
	&lp->aetime, &anynul, &istat) ||
fits_read_col_byt(fp, icol=col_direction, irow, 1, 1, 0,
	&lp->direction, &anynul, &istat) ||
fits_read_col_byt(fp, icol=col_lle_boundary, irow, 1, 1, 0,
	&lp->lle_boundary, &anynul, &istat) ||
        0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, icol, irow, istat);
      goto quit;
    }

    anl_msg_debug("\
%.1f\t%1c\t%d\n", lp->aetime, lp->direction, lp->lle_boundary);

  }

  fits_close_file(fp, &istat);
  fp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  anl_msg_debug("   nrows=%ld\n", nrows);
  anl_msg_info("\n");

  com.num_llebndry_history = nrows;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
parse_areaid(char *areaid, unsigned char area[XIStotalSegNo][MaxBndry])
{
#define IS_WORD_ELEM(c)	\
  ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

  int i, iseg, ibndry;
  char *p, word[8];

  int flag_area_set = 1;

  anl_msg_info("\
%s: parsing areaid ...\n", pname);

/* initialize area[XIStotalSegNo][MaxBndry] */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ibndry = 0; ibndry < MaxBndry; ibndry++) {
      area[iseg][ibndry] = 0;
    }
  }

  p = areaid;
  while ( *p ) {
    if ( IS_WORD_ELEM(*p) ) {

/* split out word */
      i = 0;
      while ( IS_WORD_ELEM(*p) ) {
	if ( i < sizeof(word) - 1 ) {
	  word[i] = *p;
	  i++;
	}
	p++;
      }
      word[i] = '\0';
      anl_msg_debug("word='%s'\n", word);

/* parse word */
      if ( 0 == CLstricmp("ALL", word) ) {	/* areaid='ALL' */
	for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
	  for (ibndry = 0; ibndry < MaxBndry; ibndry++) {
	    area[iseg][ibndry] = 1;
	  }
	}
	flag_area_set = 1;
	continue;
      }

      switch ( word[0] ) {
      case 'A': case 'a':
	iseg = 0;
	break;
      case 'B': case 'b':
	iseg = 1;
	break;
      case 'C': case 'c':
	iseg = 2;
	break;
      case 'D': case 'd':
	iseg = 3;
	break;
      default:
	goto ignore;
      }

      if ( 0 <= iseg && iseg < XIStotalSegNo ) {
	if ( '\0' == word[1] ) {	/* areaid = 'A', 'B', 'C', 'D' */
	  for (ibndry = 0; ibndry < MaxBndry; ibndry++) {
	    area[iseg][ibndry] = 1;
	  }
	  flag_area_set = 1;
	  continue;
	}
	ibndry = atoi(&word[1]) - 1;
	if ( 0 <= ibndry && ibndry < MaxBndry ) { /* areaid = '[A-D][01-64]' */
	  area[iseg][ibndry] = 1;
	  flag_area_set = 1;
	  continue;
	}
      }

    ignore:
      anl_msg_warning("\
%s: WARNING: invalid word '%s' in areaid ignored\n", pname, word);

    }

    p++;
  }

  if ( 0 == flag_area_set ) {
    anl_msg_error("\
%s: void area for areaid='%s'\n", pname, areaid);
    return -1;
  }

  anl_msg_info("\
-------------------------------------------------------------------\n\
   0000000001111111111222222222233333333334444444444555555555566666\n\
   1234567890123456789012345678901234567890123456789012345678901234\n\
-------------------------------------------------------------------\n");

  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    anl_msg_info("%c: ", 'A'+iseg);
    for (ibndry = 0; ibndry < MaxBndry; ibndry++) {
      anl_msg_info(area[iseg][ibndry] ? "*" : ".");
    }
    anl_msg_info("\n");
  }
  anl_msg_info("\
-------------------------------------------------------------------\n\n");

  return 0;
}

static int
compare_short_integer(const void *p1, const void *p2)
{
  short i1, i2;

  i1 = *(short *)p1;
  i2 = *(short *)p2;

  return i1 - i2;
}

static int
check_solitary(int j, int nb, short lle[XIStotalSegNo][MaxBndry])
{
  int iseg, ib;
  short median[XIStotalSegNo], lle_sorted[MaxBndry];

  int num_solitary = 0;

/* calculate median for each segment */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ib = 0; ib < nb; ib++) {
      lle_sorted[ib] = lle[iseg][ib];
    }
    qsort(lle_sorted, nb, sizeof(*lle_sorted), compare_short_integer);
    median[iseg] = lle_sorted[(nb+1)/2];
  }

/* check solitary for specified area */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ib = 0; ib < nb; ib++) {
      if ( 0 == com.area_to_check[iseg][ib] ) continue;
      if ( abs(median[iseg] - lle[iseg][ib]) > com.mediandiff ) {
	if ( 0 == num_solitary ) {
	  if ( anl_msg_chatter() < ANL_MSG_DEBUG ) {
	    anl_msg_info("\
solitary LLE, t=%.0f - %.0f at %c%02d",
		frame_st_aetime[j], frame_end_aetime[j], 'A'+iseg, ib+1);
	  } else {
	    anl_msg_debug("\
solitary LLE (m=%d), t=%.0f - %.0f at %c%02d(%d)", median[iseg],
		frame_st_aetime[j], frame_end_aetime[j], 'A'+iseg, ib+1,
		lle[iseg][ib]);
	  }
	} else {
	  if ( anl_msg_chatter() < ANL_MSG_DEBUG ) {
	    anl_msg_info(",%c%02d", 'A'+iseg, ib+1);
	  } else {
	    anl_msg_debug(",%c%02d(%d)", 'A'+iseg, ib+1, lle[iseg][ib]);
	  }
	}
	num_solitary++;
	statistics_solitary[iseg][ib]++;
      }
    }
  }

  if ( num_solitary ) {
    anl_msg_info("\n");
  }

  flags_solitary[j] = num_solitary ? 1 : 0;

  return num_solitary;
}

static void
message_unchanged(int j, int iseg, int ib, int num_unchanged)
{
  anl_msg_info("\
unchanged LLE (%d), t=%.0f - %.0f (%d frames) at %c%02d\n",
	last_lle[iseg][ib], frame_st_aetime[last_unchanged[iseg][ib]],
	frame_end_aetime[j-1], num_unchanged, 'A'+iseg, ib+1);
}

static int
flush_unchanged(int j)
{
  int i, iseg, ib, num_unchanged;

  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ib = 0; ib < MaxBndry; ib++) {
      if ( 0 == com.area_to_check[iseg][ib] ) continue;
      if ( last_unchanged[iseg][ib] < 0 ) continue;
      num_unchanged = com.num_frames - last_unchanged[iseg][ib];
      if ( com.constinterval < num_unchanged ) {
	for (i = last_unchanged[iseg][ib]; i < j; i++) {
	  flags_unchanged[i] = 1;
	  statistics_unchanged[iseg][ib]++;
	}
	message_unchanged(j, iseg, ib, num_unchanged);
      }
      last_unchanged[iseg][ib] = -1;
    }
  }
  return 0;
}

static int
check_unchanged(int j, int nb, short lle[XIStotalSegNo][MaxBndry])
{
  int i, iseg, ib, num_unchanged;

/* check unchanged for specified area */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ib = 0; ib < nb; ib++) {
      if ( 0 == com.area_to_check[iseg][ib] ) continue;
      if ( last_unchanged[iseg][ib] < 0 ) {
	last_unchanged[iseg][ib] = j;
	last_lle[iseg][ib] = lle[iseg][ib];
	continue;
      }
      if ( lle[iseg][ib] != last_lle[iseg][ib] ) {
	num_unchanged = j - last_unchanged[iseg][ib];
	if ( com.constinterval < num_unchanged ) {
	  for (i = last_unchanged[iseg][ib]; i < j; i++) {
	    flags_unchanged[i] = 1;
	    statistics_unchanged[iseg][ib]++;
	  }
	  message_unchanged(j, iseg, ib, num_unchanged);
	}
	last_unchanged[iseg][ib] = j;
	last_lle[iseg][ib] = lle[iseg][ib];
      }
    }
  }

  return 0;
}

static int
get_bndry(int j, double aetime, int *nh, int *nv)
{
  static int last_hpos = -1, last_vpos = -1;

  int i, hpos, vpos;
  unsigned char hval, vval;

  hval = com.default_llebndry_h;
  vval = com.default_llebndry_v;

  for (i = 0; i < com.num_llebndry_history; i++) {
    struct llebndry *lp = &com.llebndry_history[i];
    if ( lp->aetime <= aetime ) {
      switch (lp->direction) {
      case 'H':
	hpos = i;
	hval = lp->lle_boundary;
	break;
      case 'V':
	vpos = i;
	vval = lp->lle_boundary;
	break;
      default:
	;
      }
    }
  }

  if ( last_hpos != hpos ) {
    last_hpos = hpos;
    anl_msg_info("\
%s: LLE_BOUNDARY=%-2d for H-direction at t=%.1f\n", pname, hval, aetime);
  }

  if ( last_vpos != vpos ) {
    last_vpos = vpos;
    anl_msg_info("\
%s: LLE_BOUNDARY=%-2d for V-direction at t=%.1f\n", pname, vval, aetime);
  }

  if ( last_hpos != hpos || last_vpos != vpos ) {
    flush_unchanged(j);
  }

  *nh = hval;
  *nv = vval;

  return 0;
}

static int
checkTimeJump(int j)
{
  return ( 0 < j && frame_end_aetime[j-1] + com.gapsec < frame_st_aetime[j] );
}

static int
checkLLE(int j)
{
  return flags_solitary[j] || flags_unchanged[j];
}

static void
addToStatistics(int j)
{
  double frame_exposure = 0.0;

  if ( 0.0 != frame_end_aetime[j] && 0.0 != frame_st_aetime[j] ) {
    frame_exposure = frame_end_aetime[j] - frame_st_aetime[j];
  }

  n_tested++;
  t_tested += frame_exposure;
  if ( checkLLE(j) ) {
    n_reject++;
    t_reject += frame_exposure;
  } else {
    n_passed++;
    t_passed += frame_exposure;
  }

}

static int
write_gti(fitsfile *ofp)
{
  char buf[PIL_LINESIZE];
  char *k, tdisp1[FLEN_KEYWORD], tdisp2[FLEN_KEYWORD];

  int istat = 0;

  istat = aefits_write_module_history(ofp, pname);
  if ( istat ) goto quit;
  sprintf(buf, "  infile='%s'", com.infile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  outfile='%s'", com.outfile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  llebndryfile='%s'%s", com.llebndryfile,
	com.llebndryfile == com.o_llebndryfile ? "" : " (CALDB)");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  default_llebndry_h=%d  default_llebndry_v=%d",
	com.default_llebndry_h, com.default_llebndry_v);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  areaid='%s'", com.areaid);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  constinterval=%d  mediandiff=%.1f",
	com.constinterval, com.mediandiff);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  gapsec=%.6f  clobber=%s",
	com.gapsec, com.clobber ? "yes" : "no");
  fits_write_history(ofp, buf, &istat);

  sprintf(tdisp1, "TDISP%d", com.col_start);
  sprintf(tdisp2, "TDISP%d", com.col_stop);
  if (
fits_update_key_str(ofp, k=tdisp1, "F16.6", "display format of START",&istat)||
fits_update_key_str(ofp, k=tdisp2, "F16.6", "display format of STOP", &istat)||
fits_update_key_fixdbl(ofp, k="TSTART", gti.tstart, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TSTOP", gti.tstop, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TELAPSE", gti.telapse, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="ONTIME", gti.ontime, 6, NULL, &istat) ||
fits_update_key_lng(ofp, k="NAXIS2", gti.ngti, NULL, &istat) ||
fits_write_key_lng(ofp, k="N_FRAMES", com.num_frames, cm.N_FRAMES, &istat) ||
fits_write_key_lng(ofp, k="N_TESTED", n_tested, cm.N_TESTED, &istat) ||
fits_write_key_lng(ofp, k="N_PASSED", n_passed, cm.N_PASSED, &istat) ||
fits_write_key_lng(ofp, k="N_REJECT", n_reject, cm.N_REJECT, &istat) ||
fits_write_key_fixdbl(ofp, k="T_TESTED", t_tested, 6, cm.T_TESTED, &istat) ||
fits_write_key_fixdbl(ofp, k="T_PASSED", t_passed, 6, cm.T_PASSED, &istat) ||
fits_write_key_fixdbl(ofp, k="T_REJECT", t_reject, 6, cm.T_REJECT, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  fits_write_col_dbl(ofp, com.col_start, 1, 1, gti.ngti, gti.start, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_col('START') failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_write_col_dbl(ofp, com.col_stop, 1, 1, gti.ngti, gti.stop, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_col('STOP') failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed for GTI extension (%d)\n", pname, istat);
    goto quit;
  }

 quit:
  return istat;
}

static int
write_light_leak(fitsfile *ofp, int nll, struct light_leak *ll)
{
  static char extnam[] = "LIGHT_LEAK";
  static char ttype_body[XIStotalSegNo][MaxBndry][4];

  int i, ib, icol, iseg;
  char *ttype[4+XIStotalSegNo*MaxBndry];
  char *tform[4+XIStotalSegNo*MaxBndry];
  char *tunit[4+XIStotalSegNo*MaxBndry];

  int istat = 0;
  int nb = nbmax;
  int ncol = 4 + nb * XIStotalSegNo;

  ttype[0] = "TIME";	tform[0] = "1D";	tunit[0] = "s";
  ttype[1] = "NH";	tform[1] = "1B";	tunit[1] = "";
  ttype[2] = "NV";	tform[2] = "1B";	tunit[2] = "";
  ttype[3] = "NB";	tform[3] = "1B";	tunit[3] = "";
  for (i = 0; i < nb; i++) {
    sprintf(ttype_body[0][i], "A%02d", i+1);
    sprintf(ttype_body[1][i], "B%02d", i+1);
    sprintf(ttype_body[2][i], "C%02d", i+1);
    sprintf(ttype_body[3][i], "D%02d", i+1);
    ttype[4     +i] = ttype_body[0][i];
    ttype[4+  nb+i] = ttype_body[1][i];
    ttype[4+2*nb+i] = ttype_body[2][i];
    ttype[4+3*nb+i] = ttype_body[3][i];
    tform[4     +i] = "1I";	tunit[4     +i] = "";
    tform[4+  nb+i] = "1I";	tunit[4+  nb+i] = "";
    tform[4+2*nb+i] = "1I";	tunit[4+2*nb+i] = "";
    tform[4+3*nb+i] = "1I";	tunit[4+3*nb+i] = "";
  }

/* construct binary table header */
fits_create_tbl(ofp, BINARY_TBL, 0, ncol, ttype, tform, tunit, extnam, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_tbl() failed (%d)\n", pname, istat);
    goto quit;
  }

fits_write_key_str(ofp, "TDISP1", "F16.6", "display format of TIME", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_key('TDISP1') failed (%d)\n", pname, istat);
    goto quit;
  }

  for (i = 0; i < nll; i++) {
    icol = 0;
    if (
fits_write_col_dbl(ofp, ++icol, i+1, 1, 1, &ll[i].aetime, &istat) ||
fits_write_col_byt(ofp, ++icol, i+1, 1, 1, &ll[i].nh, &istat) ||
fits_write_col_byt(ofp, ++icol, i+1, 1, 1, &ll[i].nv, &istat) ||
fits_write_col_byt(ofp, ++icol, i+1, 1, 1, &ll[i].nb, &istat) ||
         0 ) {
      anl_msg_error("\
%s: fits_write_col('%s') failed (%d)\n", pname, ttype[icol-1], istat);
      goto quit;
    }
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      for (ib = 0; ib < nb; ib++) {
	if (
fits_write_col_sht(ofp, ++icol, i+1, 1, 1, &ll[i].val[iseg][ib], &istat) ||
             0 ) {
	  anl_msg_error("\
%s: fits_write_col('%s') failed (%d)\n", pname, ttype[icol-1], istat);
	  goto quit;
	}
      }
    }
  }

 quit:
  return istat;
}

static int
write_statistics(fitsfile *ofp)
{
  static char extnam[] = "BAD_LLE_STATISTICS";
  static char *ttype[3] = { "AREAID", "UNCHANGED", "SOLITARY" };
  static char *tform[3] = { "3A", "1J", "1J" };
  static char *tunit[3] = { "", "frame", "frame" };

  int iseg, ib, icol, unchanged, solitary;
  char areaid[4];

  int ncol = 3;
  int irow = 1;
  int istat = 0;
  char *areaid_ptr = areaid;

/* construct binary table header */
fits_create_tbl(ofp, BINARY_TBL, 0, ncol, ttype, tform, tunit, extnam, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_tbl() failed (%d)\n", pname, istat);
    goto quit;
  }

  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    for (ib = 0; ib < nbmax; ib++) {
      if ( 0 == com.area_to_check[iseg][ib] ) continue;
      sprintf(areaid, "%c%02d", 'A'+iseg, ib+1);
      unchanged = statistics_unchanged[iseg][ib];
      solitary = statistics_solitary[iseg][ib];
      if (
fits_write_col_str(ofp, icol=1, irow, 1, 1, &areaid_ptr, &istat) ||
fits_write_col_int(ofp, icol=2, irow, 1, 1, &unchanged, &istat) ||
fits_write_col_int(ofp, icol=3, irow, 1, 1, &solitary, &istat) ||
         0 ) {
	anl_msg_error("\
%s: fits_write_col('%s') failed (%d)\n", pname, ttype[icol-1], istat);
	goto quit;
      }
      irow++;
    }
  }

 quit:
  return istat;
}

static void
show_statistics(void)
{
  int iseg, ib;

  printf("\
%s: BAD LLE STATISTICS\n\
===============================================================================\n\
 unchanged solitary  unchanged solitary  unchanged solitary  unchanged solitary\n\
-------------------------------------------------------------------------------\n", pname);

  for (ib = 0; ib < nbmax; ib++) {
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      if ( 0 < iseg ) {
	printf("  ");
      }
      printf("%c%02d:", 'A'+iseg, ib+1);
      if ( 0 == com.area_to_check[iseg][ib] ) {
	printf("%7s%7s", "......", "......");
      } else {
	printf("%7d%7d",
		statistics_unchanged[iseg][ib], statistics_solitary[iseg][ib]);
      }
    }
    printf("\n");
  }

  printf("\
-------------------------------------------------------------------------------\n\n");

}

void
XIScheckLLE_startup(int *status)
{
  com.llebndryfile = strcpy(com.o_llebndryfile, "CALDB");
  strcpy(com.areaid, "ALL");
  com.gapsec = 0.1;
  com.constinterval = 10;
  com.mediandiff = 100.0;
  com.clobber = ANL_YES;

  *status = ANL_OK;
}

void
XIScheckLLE_com(int *status)
{
  static char *keytbl[] = {
    "SHOW",
    "LLEBNDRYFILE",
    "DEFAULT_LLEBNDRY_H",
    "DEFAULT_LLEBNDRY_V",
    "AREAID",
    "CONSTINTERVAL",
    "MEDIANDIFF",
    "GAPSEC",
    "CLOBBER",
    "EXIT"
  };
  static char *help[] = {
    "Show current setting",
    "CALDB file for LLE boundary",
    "LLE boundary H used when LLEBNDRY=NONE",
    "LLE boundary V used when LLEBNDRY=NONE",
    "LLE area-id, ALL or list of [A-D][01-64]",
    "Allowed maximum frames without update of LLE",
    "Allowed maximum difference from median of LLE in each segment",
    "Allowed gap between frames in second",
    "Overwrite output file if exists?",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

  if ( *status ) {	/* ftools */
    char *k;
    if (
PILGetFname(k="llebndryfile", com.o_llebndryfile) ||
PILGetInt (k="default_llebndry_h", &com.default_llebndry_h) ||
PILGetInt (k="default_llebndry_v", &com.default_llebndry_v) ||
PILGetString(k="areaid", com.areaid) ||
PILGetInt (k="constinterval", &com.constinterval) ||
PILGetReal(k="mediandiff", &com.mediandiff) ||
PILGetReal(k="gapsec", &com.gapsec) ||
PILGetBool(k="clobber", &com.clobber) ||
	 0 ) {
      anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
    return;
  }

  for(;;) {
    char *key;
    int ans[2];
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if ( 0 == strcmp("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp("LLEBNDRYFILE", key) ) {
      CLtxtrd(key, com.o_llebndryfile, sizeof(com.o_llebndryfile));
    } else if ( 0 == strcmp("DEFAULT_LLEBNDRY_H", key) ) {
      CLintrd(key, &com.default_llebndry_h);
    } else if ( 0 == strcmp("DEFAULT_LLEBNDRY_V", key) ) {
      CLintrd(key, &com.default_llebndry_v);
    } else if ( 0 == strcmp("AREAID", key) ) {
      CLtxtrd(key, com.areaid, sizeof(com.areaid));
    } else if ( 0 == strcmp("GAPSEC", key) ) {
      CLfdprd(key, &com.gapsec);
    } else if ( 0 == strcmp("CONSTINTERVAL", key) ) {
      CLintrd(key, &com.constinterval);
    } else if ( 0 == strcmp("MEDIANDIFF", key) ) {
      CLfdprd(key, &com.mediandiff);
    } else if ( 0 == strcmp("CLOBBER", key) ) {
      CLintrd(key, &com.clobber);
    } else if ( 0 == strcmp("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XIScheckLLE_init(int *status)
{
  static char *codename = "LLE_BOUNDARY";

  long i;
  int used;
  char *instrume, o_instrume[FLEN_VALUE];
  fitsfile *ofp;

/* caldb support */
  BnkGet("XIS:INSTRUME", sizeof(o_instrume), &used, o_instrume);
  instrume = o_instrume + 1;	/* remove beginning "'" */
  com.llebndryfile = aste_caldb_find(instrume, codename, com.o_llebndryfile);
  if ( NULL == com.llebndryfile ) {
    goto quit;
  }

  showParam();

  BnkGet("XIS:FRAMES", sizeof(com.num_frames), &used, &com.num_frames);
  if ( 0 == com.num_frames ) {	/* NO FRAMES EXTENSION */
    anl_msg_error("\
%s: no FRAMES extension found.\n", pname);
    goto quit;
  }

  if ( ANL_OK != BnkIsDef("XIS:FRAMES:LIGHT_LEAK") ) {
    anl_msg_error("\
%s: LIGHT_LEAK column not found in FRAMES extension.\n", pname);
    goto quit;
  }

/* read llebndry file */
  if ( read_llebndry(com.llebndryfile) ) {
    goto quit;
  }

/* parse areaid */
  if ( parse_areaid(com.areaid, com.area_to_check) ) {
    goto quit;
  }

/* memory allocation */
  frame_st_aetime = malloc(2 * com.num_frames * sizeof(*frame_st_aetime));
  frame_end_aetime = &frame_st_aetime[com.num_frames];
  flags_unchanged = malloc(2 * com.num_frames * sizeof(*flags_unchanged));
  flags_solitary = &flags_unchanged[com.num_frames];
  gti.start = malloc(2 * com.num_frames * sizeof(*gti.start));
  gti.stop = &gti.start[com.num_frames];
  lldata = malloc(com.num_frames * sizeof(*lldata));

  if ( NULL == frame_st_aetime ||
       NULL == flags_solitary ||
       NULL == gti.start || NULL == lldata ) {
    anl_msg_error("\
%s: memory allocation error", pname);
    goto quit;
  }

/* initialize memory */
  for (i = 0; i < com.num_frames; i++) {
    frame_st_aetime[i] = 0.0;
    frame_end_aetime[i] = 0.0;
    flags_unchanged[i] = 0;
    flags_solitary[i] = 0;
  }

  memset(lldata, 0, com.num_frames * sizeof(*lldata));

  for (i = 0; i < XIStotalSegNo*MaxBndry; i++) {
    last_unchanged[0][i] = -1;
    statistics_solitary[0][i] = 0;
    statistics_unchanged[0][i] = 0;
  }

  BnkGet("XIS:FILENAME:PTR", sizeof(com.infile), &used, &com.infile);
  BnkGet("XIS:OUTFITS:FILENAME:PTR", sizeof(com.outfile), &used, &com.outfile);

  if ( 0 == CLstricmp(com.outfile, "NONE") ) {
    ofp = NULL;
  } else {
    fitsfile *ifp;
    char *k;
    int hdutype;
    int extver = 0;
    int istat = 0;

    BnkGet("XIS:FITS:PTR", sizeof(ifp), &used, &ifp);
    fits_reopen_file(ifp, &ifp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
      goto quit;
    }
    fits_movabs_hdu(ifp, 1, &hdutype, &istat);	/* move to primary */
    if ( istat ) {
      anl_msg_error("\
%s: fits_movabs_hdu(hdunum=1) failed (%d)\n", pname, istat);
      goto quit;
    }

    if ( com.clobber ) {
      unlink(com.outfile);
    }
    fits_create_file(&ofp, com.outfile, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, com.outfile, istat);
      goto quit;
    }

    fits_copy_header(ifp, ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_copy_header() failed for primary extension (%d)\n", pname, istat);
      goto quit;
    }
    fits_write_date(ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_date() failed for primary extension (%d)\n", pname, istat);
      goto quit;
    }
    fits_write_chksum(ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_chksum() failed for primary extension (%d)\n", pname, istat);
      goto quit;
    }

    fits_movnam_hdu(ifp, BINARY_TBL, "GTI", extver, &istat);
    if ( istat ) {
      istat = 0;
      fits_movnam_hdu(ifp, BINARY_TBL, "STDGTI", extver, &istat);
    }
    if ( istat ) {
      anl_msg_error("\
%s: fits_movnam_hdu('GTI') failed (%d)\n", pname, istat);
      goto quit;
    }

    fits_copy_header(ifp, ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_copy_header() failed for GTI extension (%d)\n", pname, istat);
      goto quit;
    }

    fits_close_file(ifp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_close_file() failed for input event file (%d)\n", pname, istat);
      goto quit;
    }

    if ( fits_get_colnum(ofp, CASESEN, k="START", &com.col_start, &istat) ||
	 fits_get_colnum(ofp, CASESEN, k="STOP", &com.col_stop, &istat) ) {
      anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }

  }

  initflag = ANL_TRUE;
  com.ofp = ofp;

/*  fp = fopen("lle_lc.qdp", "w");*/

  EvsDef("XIScheckLLE:BEGIN");
  EvsDef("XIScheckLLE:ENTRY");
  EvsDef("XIScheckLLE:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XIScheckLLE_his(int *status)
{
  *status = ANL_OK;
}

void
XIScheckLLE_bgnrun(int *status)
{
  *status = ANL_OK;
}

void
XIScheckLLE_ana(int nevent, int eventid, int *status)
{
  int i, iseg, ih, iv, ib, nh, nv, nb, used;
  double aetime, t0, t1;
  int light_leak[XIStotalSegNo*MaxBndry];

  EvsfSetM("XIScheckLLE:ENTRY");

  BnkfGetM("XIS:FRAMES:FRAME_ST_AETIME", sizeof(double), &used, &t0);

  if ( initflag ) {
    ithframe = 0;
    initflag = ANL_FALSE;
  } else if ( frame_st_aetime[ithframe] + com.gapsec < t0 ) {
    ithframe++;
  } else {
    goto skip;
  }

  if ( com.num_frames <= ithframe ) {
    anl_msg_error("\
%s: too many frames (%d), something is wrong\n", pname, ithframe);
    anl_put_exit_status(1);
    *status = ANL_QUIT;
    return;
  }
  /*=========== initial frame or 1st event in a frame ===== */
  BnkfGetM("XIS:FRAMES:FRAME_END_AETIME", sizeof(double), &used, &t1);
  BnkfGetM("XIS:FRAMES:TIME", sizeof(double), &used, &aetime);

  frame_st_aetime[ithframe] = t0;
  frame_end_aetime[ithframe] = t1;
  get_bndry(ithframe, aetime, &nh, &nv);
  nb = nh * nv;
  if ( nbmax < nb ) {
    nbmax = nb;
  }

  lldata[ithframe].nh = nh;
  lldata[ithframe].nv = nv;
  lldata[ithframe].nb = nb;
  lldata[ithframe].aetime = aetime;

  BnkfGetM("XIS:FRAMES:LIGHT_LEAK", sizeof(light_leak), &used, light_leak);

  for (i = 0; i < XIStotalSegNo*MaxBndry; i++) {
/* light_leak[256] = {
	A-H0V0, A-H1V0, A-H2V0, A-H3V0,
	...
	A-H0Vf, A-H1Vf, A-H2Vf, A-H3Vf,
	B-H0V0, B-H1V0, B-H2V0, B-H3V0,
	...
	B-H0Vf, B-H1Vf, B-H2Vf, B-H3Vf,
	C-H0V0, C-H1V0, C-H2V0, C-H3V0,
	...
	C-H0Vf, C-H1Vf, C-H2Vf, C-H3Vf,
	D-H0V0, D-H1V0, D-H2V0, D-H3V0,
	...
	D-H0Vf, D-H1Vf, D-H2Vf, D-H3Vf,
    }
*/
    ih = i % MaxBndryH;
    iv = (i / MaxBndryH) % MaxBndryV;
    if ( ih < nh && iv < nv ) {
      iseg = i / MaxBndry;
      ib = iv * nh + ih;
      lldata[ithframe].val[iseg][ib] = light_leak[i];
    }
  }

/*  fprintf(fp, "%.6f %d\n", aetime, lle[1][3]);*/

  check_unchanged(ithframe, nb, lldata[ithframe].val);
/*  check_solitary(ithframe, nb, lldata[ithframe].val);*/

 skip:

  EvsfSetM("XIScheckLLE:OK");

  *status = ANL_OK;
}

void
XIScheckLLE_endrun(int *status)
{
  *status = ANL_OK;
}

void
XIScheckLLE_exit(int *status)
{
  int j;

  int igti = 0;
  int flag_goodtime = ANL_FALSE;
  fitsfile *ofp = com.ofp;
  int istat = 0;

  flush_unchanged(ithframe+1);	/* flush flag_unchanged[] */

  for (j = 0; j <= ithframe; j++) {
    if ( 0 == flags_unchanged[j] ) {
      check_solitary(j, lldata[j].nb, lldata[j].val);
    }
    addToStatistics(j);
  }

  anl_msg_info("\n");
  show_statistics();

  for (j = 0; j < n_tested && checkLLE(j); j++) {
    ;
  }

  if ( j < n_tested ) {
    gti.start[igti] = frame_st_aetime[j];
    flag_goodtime = ANL_TRUE;
    j++;
  }

  while ( j < n_tested ) {

    if ( flag_goodtime ) {	/* during good time */

      if ( checkLLE(j) ) {	/* staurated frame, not usable */
	flag_goodtime = ANL_FALSE;
	gti.stop[igti] = frame_end_aetime[j-1];
	igti++;

      } else {				/* not saturated, usable frame */

	if ( checkTimeJump(j) ) {	/* frame time jump detected */
	  gti.stop[igti] = frame_end_aetime[j-1];
	  igti++;
	  gti.start[igti] = frame_st_aetime[j];
	}

      }

    } else {			/* during bad time */

      if ( 0 == checkLLE(j) ) {	/* not saturated, usable frame */
	flag_goodtime = ANL_TRUE;
	gti.start[igti] = frame_st_aetime[j];
      }
    }

    j++;
  }

  if ( flag_goodtime ) {
    gti.stop[igti] = frame_end_aetime[n_tested-1];
    igti++;
  }

  gti.ngti = igti;
  gti.tstart = gti.start[0];
  gti.tstop = gti.stop[igti-1];
  gti.telapse = gti.tstop - gti.tstart;
  gti.ontime = t_passed;

  anl_msg_debug("\
%s: ngti=%d\n", pname, gti.ngti);

  if ( NULL != ofp ) {
    if ( write_gti(ofp) ) {
      goto quit;
    }

    if ( write_light_leak(ofp, n_tested, lldata) ) {
      goto quit;
    }

    if ( write_statistics(ofp) ) {
      goto quit;
    }

    fits_close_file(ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.outfile, istat);
      goto quit;
    }

    anl_msg_info("\
%s: GTI file '%s' created\n", pname, com.outfile);
  }

  anl_msg_always("\
N_FRAMES = %16ld / %s\n\
N_TESTED = %16d / %s\n\
N_PASSED = %16d / %s\n\
N_REJECT = %16d / %s\n\
T_TESTED = %16.6f / %s\n\
T_PASSED = %16.6f / %s\n\
T_REJECT = %16.6f / %s\n",
	com.num_frames, cm.N_FRAMES,
	n_tested, cm.N_TESTED,
	n_passed, cm.N_PASSED,
	n_reject, cm.N_REJECT,
	t_tested, cm.T_TESTED,
	t_passed, cm.T_PASSED,
	t_reject, cm.T_REJECT);

/*  fclose(fp);*/

  free(lldata);
  free(gti.start);
  free(flags_unchanged);
  free(frame_st_aetime);
  free(com.llebndry_history);

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
