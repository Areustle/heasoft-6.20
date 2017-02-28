/**************************************

  XIScheckEventNo

    ver 0.0 2005/08/28	K. Hayashida

    ver 0.1 2005/12/16	K. Hayashida
	Check event no to search for telemetry saturation
    ver 0.2 2005/12/18	K. Hayashida
	Implement segment selection
    ver 0.3 2007/01/31	Y. ISHISAKI
	add gapsec parameter, remove gtifile parameter
	BnkGet XIS:FRAMES to determine required memory in _init()
	BnkGet XIS:OUTFITS:FILENAME:PTR to determine output GTI file name
	add functions, checkTimeJump(), checkSaturation()
	add functions, showTimeJump(), showFrameInfo(), addToStatistics()
	consider last frame in _exit()
    ver 1.0 2007/01/31	Y. ISHISAKI
	write FITS GTI file
	add clobber parameter
    ver 1.1 2007/04/30	Y. ISHISAKI
	print comment
    ver 1.2 2007/05/07	Y. ISHISAKI
	bug fix in handling frame time jump in _exit()
	static declaration of gti
    ver 1.3 2009/03/08	Y. ISHISAKI
	update NAXIS2 keywords in GTI extension in write_gti()
	free allocated memory in _exit()
    ver 2.0 2011/12/19  S. Yamada
        made appicable to window/timing modes
    ver 2.1 2012/04/22  Y. ISHISAKI
        add "wingti" parameter
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
#include "xisTelemFormat.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "XIScheckEventNo";
char XIScheckEventNo_version[] = "version 2.1";

static int wingti;
static int clobber;
static double gapsec = 0.1;	// read from xisgtigen.par
static int check_seg[XIStotalSegNo];

static fitsfile *ofp;
static char *infile;
static char *outfile;
static int col_start, col_stop;
static GTI_DATA gti;

static double *frame_st_aetime;
static double *frame_end_aetime;
static double *frame_exposure_inlist;
static int *frame_event_totno;
static int *event_totno_inlist;

static int *rawy_max_inlist;
static int rawy_max_tmp_seg[XIStotalSegNo];
static int eventnum_seg[XIStotalSegNo];
static double t_saturated_seg[XIStotalSegNo];

static int initflag;
static int ithframe;
static long frames;
static int n_tested = 0;
static int n_passed = 0;
static int n_time_jump = 0;
static int n_saturated = 0;
static double t_tested = 0.0;
static double t_passed = 0.0;
static double t_time_jump = 0.0;
static double t_saturated = 0.0;
static double t_saturated_frame = 0.0;

static int editmode;
static int windowoption;
static int windownumber;
static int windowsize;

static struct {
  char *N_FRAMES;
  char *N_TESTED;
  char *N_PASSED;
  char *N_T_JUMP;
  char *N_SATURA;
  char *T_TESTED;
  char *T_PASSED;
  char *T_T_JUMP;
  char *T_SATURA;
  char *T_SATURA_wingti;
  char *T_SATU_F_wingti;
} cm = {
  "number of frames in the input event file",
  "number of non-zero frames tested",
  "number of frames passed the test",
  "number of frames detected time jump",
  "number of frames telemetry saturated",
  "exposure of non-zero frames tested",
  "exposure of frames passed the test",
  "loss of exposure due to time jump",
  "exposure of telemetry saturated frames",
  "exposure of telemetry saturated windows",
  "exposure of telemetry saturated frames"
};

static void
showParam(void)
{
  printf ("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %.6f (s)\n", "GAPSEC", gapsec);
  printf("%20s   %s\n", "SEGMENT_A", check_seg[0] ? "YES" : "NO");
  printf("%20s   %s\n", "SEGMENT_B", check_seg[1] ? "YES" : "NO");
  printf("%20s   %s\n", "SEGMENT_C", check_seg[2] ? "YES" : "NO");
  printf("%20s   %s\n", "SEGMENT_D", check_seg[3] ? "YES" : "NO");
  printf("%20s   %s\n", "WINGTI", wingti ? "YES" : "NO");
  printf("%20s   %s\n", "CLOBBER", clobber ? "YES" : "NO");
  printf("\n");
}

static int
checkTimeJump(int j)
{
  return ( 0 < j && frame_end_aetime[j-1] + gapsec < frame_st_aetime[j] );
}

static int
checkSaturation(int j)
{
  return (event_totno_inlist[j] != frame_event_totno[j]);
}

static void
showTimeJump(int j)
{
  anl_msg_info("\
frame time jump, t=%.3f - %.3f by %.3f s\n",
	frame_end_aetime[j-1], frame_st_aetime[j],
	frame_st_aetime[j] - frame_end_aetime[j-1]);
}

static void
showFrameInfo(int j)
{
  if ( checkSaturation(j) ) {
    anl_msg_info("\
saturated frame, t=%.3f - %.3f  %d (%d/%d) seg=%d%d%d%d\n",
	frame_st_aetime[j], frame_end_aetime[j],
	frame_event_totno[j] - event_totno_inlist[j],
	event_totno_inlist[j], frame_event_totno[j],
	check_seg[0], check_seg[1], check_seg[2], check_seg[3]);
  } else {
    anl_msg_debug("\
%.3f - %.3f  %d (%d/%d) seg=%d%d%d%d\n",
	frame_st_aetime[j], frame_end_aetime[j],
	frame_event_totno[j] - event_totno_inlist[j],
	event_totno_inlist[j], frame_event_totno[j],
	check_seg[0], check_seg[1], check_seg[2], check_seg[3]);
  }
}

static void
showFrameInfo_wingti(int j)
{
  if ( checkSaturation(j) ) {
    anl_msg_info("\
Saturated frame, t=%.3f - %.3f  %d (%d/%d) seg=%d%d%d%d\
 (%.3fs/8.0s) rawy_max=%d(%d/%d/%d/%d)\n",
	frame_st_aetime[j], frame_end_aetime[j],
	frame_event_totno[j] - event_totno_inlist[j],
	event_totno_inlist[j], frame_event_totno[j],
	check_seg[0], check_seg[1], check_seg[2], check_seg[3],
	frame_exposure_inlist[j], rawy_max_inlist[j],
	rawy_max_tmp_seg[0], rawy_max_tmp_seg[1],
	rawy_max_tmp_seg[2], rawy_max_tmp_seg[3]);
  } else {
    anl_msg_debug("\
%.3f - %.3f  %d (%d/%d) seg=%d%d%d%d\
 (%.3fs/8.0s) rawy_max=%d(%d/%d/%d/%d)\n",
	frame_st_aetime[j], frame_end_aetime[j],
	frame_event_totno[j] - event_totno_inlist[j],
	event_totno_inlist[j], frame_event_totno[j],
	check_seg[0], check_seg[1], check_seg[2], check_seg[3],
	frame_exposure_inlist[j], rawy_max_inlist[j],
	rawy_max_tmp_seg[0], rawy_max_tmp_seg[1],
	rawy_max_tmp_seg[2], rawy_max_tmp_seg[3]);
  }
}

static void
addToStatistics(int j)
{
  int iseg;
  double frame_exposure = 0.0;

  if ( 0.0 != frame_end_aetime[j] && 0.0 != frame_st_aetime[j] ) {
    frame_exposure = frame_end_aetime[j] - frame_st_aetime[j];
  }

  n_tested++;
  t_tested += frame_exposure;
  if ( checkSaturation(j) ) {
    n_saturated++;
    t_saturated += frame_exposure;
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      /* status info */
      t_saturated_seg[iseg] += frame_exposure;
    }
  } else {
    n_passed++;
    t_passed += frame_exposure;
  }

  if ( checkTimeJump(j) ) {
    n_time_jump++;
    if ( 0.0 != frame_st_aetime[j] && 0.0 != frame_end_aetime[j-1] ) {
      t_time_jump += frame_st_aetime[j] - frame_end_aetime[j-1];
    }
    showTimeJump(j);
  }

  showFrameInfo(j);
}

static double
calc_window_exposure(double frame_exposure, int rawy_max)
{
  int num_usable_exposures;
  double window_exposure;

  num_usable_exposures = rawy_max / windowsize;
  window_exposure = frame_exposure * num_usable_exposures / windownumber;

  return window_exposure;
}

static void
addToStatistics_wingti(int j)
{
  int iseg;
  double window_exposure;
  double frame_exposure = 0.0;

  if ( 0.0 != frame_end_aetime[j] && 0.0 != frame_st_aetime[j] ) {
    frame_exposure = frame_end_aetime[j] - frame_st_aetime[j];
    if ( 1 == windownumber ) {	/* Full window */
      frame_exposure_inlist[j] = 0.0;
    } else {			/* window mode */
      frame_exposure_inlist[j] =
	calc_window_exposure(frame_exposure, rawy_max_inlist[j]);
    }
  }

  n_tested++;
  t_tested += frame_exposure;
  if ( checkSaturation(j) ) {
    n_saturated++;
    t_saturated += ( frame_exposure - frame_exposure_inlist[j] );
    t_saturated_frame += frame_exposure;
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      /* status info */
      window_exposure =
	calc_window_exposure(frame_exposure, rawy_max_tmp_seg[iseg]);
      t_saturated_seg[iseg] += ( frame_exposure - window_exposure );
    }
  } else {
    n_passed++;
    t_passed += frame_exposure;
  }

  if ( checkTimeJump(j) ) {
    n_time_jump++;
    if ( 0.0 != frame_st_aetime[j] && 0.0 != frame_end_aetime[j-1] ) {
      t_time_jump += frame_st_aetime[j] - frame_end_aetime[j-1];
    }
    showTimeJump(j);
  }

  showFrameInfo_wingti(j);
}

static int
write_gti(fitsfile *ofp)
{
  char buf[PIL_LINESIZE];
  char *k, tdisp1[FLEN_KEYWORD], tdisp2[FLEN_KEYWORD];
  int istat = 0;

  istat = aefits_write_module_history(ofp, pname);
  if ( istat ) goto quit;
  sprintf(buf, "  infile='%s'", infile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  outfile='%s'", outfile);
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  gapsec=%.6f  wingti=%s  clobber=%s",
	  gapsec, wingti ? "yes" : "no", clobber ? "yes" : "no");
  fits_write_history(ofp, buf, &istat);
  sprintf(buf, "  segment_a=%s  segment_b=%s  segment_c=%s  segment_d=%s",
	  check_seg[0] ? "yes" : "no",
	  check_seg[1] ? "yes" : "no",
	  check_seg[2] ? "yes" : "no",
	  check_seg[3] ? "yes" : "no");
  fits_write_history(ofp, buf, &istat);

  sprintf(tdisp1, "TDISP%d", col_start);
  sprintf(tdisp2, "TDISP%d", col_stop);
  if (
fits_update_key_str(ofp, k=tdisp1, "F16.6", "display format of START",&istat)||
fits_update_key_str(ofp, k=tdisp2, "F16.6", "display format of STOP", &istat)||
fits_update_key_fixdbl(ofp, k="TSTART", gti.tstart, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TSTOP", gti.tstop, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TELAPSE", gti.telapse, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="ONTIME", gti.ontime, 6, NULL, &istat) ||
fits_update_key_lng(ofp, k="NAXIS2", gti.ngti, NULL, &istat) ||
fits_write_key_lng(ofp, k="N_FRAMES", frames, cm.N_FRAMES, &istat) ||
fits_write_key_lng(ofp, k="N_TESTED", n_tested, cm.N_TESTED, &istat) ||
fits_write_key_lng(ofp, k="N_PASSED", n_passed, cm.N_PASSED, &istat) ||
fits_write_key_lng(ofp, k="N_T_JUMP", n_time_jump, cm.N_T_JUMP, &istat) ||
fits_write_key_lng(ofp, k="N_SATURA", n_saturated, cm.N_SATURA, &istat) ||
fits_write_key_fixdbl(ofp, k="T_TESTED", t_tested, 6, cm.T_TESTED, &istat) ||
fits_write_key_fixdbl(ofp, k="T_PASSED", t_passed, 6, cm.T_PASSED, &istat) ||
fits_write_key_fixdbl(ofp, k="T_T_JUMP", t_time_jump, 6, cm.T_T_JUMP, &istat)||
fits_write_key_fixdbl(ofp, k="T_SATURA", t_saturated, 6, cm.T_SATURA, &istat)||
      0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if ( wingti ) {
    if (
fits_modify_comment(ofp, k="T_SATURA", cm.T_SATURA_wingti, &istat) ||
fits_write_key_fixdbl(ofp, k="T_SATU_F", t_saturated_frame, 6,
		      cm.T_SATU_F_wingti, &istat) ) {
      anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
  }

  fits_write_col_dbl(ofp, col_start, 1, 1, gti.ngti, gti.start, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_col('START') failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_write_col_dbl(ofp, col_stop, 1, 1, gti.ngti, gti.stop, &istat);
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

void
XIScheckEventNo_startup(int *status)
{
  int iseg;

  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    check_seg[iseg] = 1;
  }

  *status = ANL_OK;
}

void
XIScheckEventNo_com(int *status)
{
  static char *keytbl[] = {
    "SHOW",
    "GAPSEC",
    "SEGMENT_A",
    "SEGMENT_B",
    "SEGMENT_C",
    "SEGMENT_D",
    "EXIT"
  };
  static char *help[] = {
    "Show current setting",
    "Allowed gap between frames in second",
    "Check Segment A",
    "Check Segment B",
    "Check Segment C",
    "Check Segment D",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

  if ( *status ) {	/* ftools */
    char *k;
    if (
PILGetReal(k="gapsec", &gapsec) ||
PILGetBool(k="segment_a", &check_seg[0]) ||
PILGetBool(k="segment_b", &check_seg[1]) ||
PILGetBool(k="segment_c", &check_seg[2]) ||
PILGetBool(k="segment_d", &check_seg[3]) ||
PILGetBool(k="wingti", &wingti) ||
PILGetBool(k="clobber", &clobber) ||
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
    if ( 0 == strcmp("GAPSEC", key) ) {
      CLfdprd(key, &gapsec);
    } else if ( 0 == strcmp("SEGMENT_A", key) ) {
      CLlogrd(key, &check_seg[0]);
    } else if ( 0 == strcmp("SEGMENT_B", key) ) {
      CLlogrd(key, &check_seg[1]);
    } else if ( 0 == strcmp("SEGMENT_C", key) ) {
      CLlogrd(key, &check_seg[2]);
    } else if ( 0 == strcmp("SEGMENT_D", key) ) {
      CLlogrd(key, &check_seg[3]);
    } else if ( 0 == strcmp ("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XIScheckEventNo_init(int *status)
{
  long i;
  int used, iseg, psum_l;
  double delta_t;

  EvsDef("XIScheckEventNo:BEGIN");
  EvsDef("XIScheckEventNo:ENTRY");
  EvsDef("XIScheckEventNo:OK");

  showParam();

  BnkGet("XIS:FRAMES", sizeof(frames), &used, &frames);
  if ( 0 == frames ) {	/* NO FRAMES EXTENSION */
    anl_msg_error("\
%s: no FRAMES extension found.\n", pname);
    *status = ANL_QUIT;
    return;
  }

/* Memory Allocation */
  frame_st_aetime = malloc(3 * frames * sizeof(double));
  frame_end_aetime = &frame_st_aetime[frames];
  frame_exposure_inlist = &frame_end_aetime[frames];
  frame_event_totno = malloc(3 * frames * sizeof(int));
  event_totno_inlist = &frame_event_totno[frames];
  rawy_max_inlist = &event_totno_inlist[frames];
  gti.start = malloc(2 * frames * sizeof(*gti.start));
  gti.stop = &gti.start[frames];

  if ( NULL == frame_st_aetime ||
       NULL == frame_event_totno ||
       NULL == gti.start ) {
    anl_msg_error("\
%s: memory allocation error", pname);
    *status = ANL_QUIT;
    return;
  }

  for (i = 0; i < frames; i++) {
    frame_st_aetime[i] = 0.0;
    frame_end_aetime[i] = 0.0;
    frame_exposure_inlist[i] = 0.0;
    frame_event_totno[i] = 0;
    event_totno_inlist[i] = 0;
    rawy_max_inlist[i] = 0;
  }

/* status info */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    t_saturated_seg[iseg] = 0.0;
    rawy_max_tmp_seg[iseg] = 0;
    eventnum_seg[iseg] = 0;
  }

  if ( wingti ) {	/* wingti=yes */

/* edit mode, window option */
    BnkGet("XIS:EDITMODE", sizeof(editmode), &used, &editmode);
    BnkGet("XIS:WINOPT", sizeof(windowoption), &used, &windowoption);

    anl_msg_info("\
EDITMODE      = %d\n\
WINOPT        = %d\n", editmode, windowoption);

/* window option, windownumber*/
    switch (windowoption) {
    case 0: /* full window */
      windownumber = 1;
      windowsize = 1024;
      anl_msg_info("\
Window Mode   = Full window \n");
      break;
    case 1: /* 1/4 window */
      windownumber = 4;
      windowsize = 256;
      anl_msg_info("\
Window Mode   = 1/4 window \n");
      break;
    case 2: /* 1/8 window */
      windownumber = 8;
      windowsize = 128;
      anl_msg_info("\
Window Mode   = 1/8 window \n");
      break;
    case 3: /* 1/16 window */
      windownumber = 16;
      windowsize = 64;
      anl_msg_info("\
Window Mode   = 1/16 window \n");
      break;
    default: /* no such mode */
      anl_msg_error("\
%s: ERROR: invalid window option %d!!!\n", pname, windowoption);
      goto quit;
    }

    if ( XISeditTiming == editmode ) {
      BnkGet("XIS:PSUM_L", sizeof(psum_l), &used, &psum_l);
      windownumber = 1024;
      windowsize = 1;
      anl_msg_info("\
PSUM_L        = %d\n\
Edit Mode     = Timing\n", psum_l);
    }

    anl_msg_info("\
Window number = %d\n\
Window size   = %d\n", windownumber, windowsize);

    if ( 0 < windownumber ) {
      delta_t = 8.0 / windownumber;
      anl_msg_info("\
Delta T       = %.9f (s) \n\n", delta_t);
    } else{
      anl_msg_error("\
%s: ERROR: window number is negative %d!!!\n\n", pname, windownumber);
      goto quit;
    }

  }	/* wingti=yes */

  BnkGet("XIS:FILENAME:PTR", sizeof(infile), &used, &infile);
  BnkGet("XIS:OUTFITS:FILENAME:PTR", sizeof(outfile), &used, &outfile);

  if ( 0 == CLstricmp(outfile, "NONE") ) {
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

    if ( clobber ) {
      unlink(outfile);
    }
    fits_create_file(&ofp, outfile, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, outfile, istat);
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

    if ( fits_get_colnum(ofp, CASESEN, k="START", &col_start, &istat) ||
	 fits_get_colnum(ofp, CASESEN, k="STOP", &col_stop, &istat) ) {
      anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }

  }

  initflag = ANL_TRUE;

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XIScheckEventNo_his(int *status)
{
  *status = ANL_OK;
}

void
XIScheckEventNo_bgnrun(int *status)
{
  *status = ANL_OK;
}

void
XIScheckEventNo_ana(int nevent, int eventid, int *status)
{
  int iseg, segment, used;
  int ev_tot_no[XIStotalSegNo];
  double t0, t1;
  int rawy_max_tmp, rawy;

  EvsfSetM("XIScheckEventNo:ENTRY");

  BnkfGetM("XIS:FRAMES:FRAME_ST_AETIME", sizeof(double), &used, &t0);
  /*printf("frame st aetime %f\n", t0); */

  if ( initflag ) {
    ithframe = 0;
    initflag = ANL_FALSE;
  } else if ( frame_st_aetime[ithframe] + gapsec < t0 ) {
    if ( wingti ) {
      addToStatistics_wingti(ithframe);
    } else {
      addToStatistics(ithframe);
    }
    ithframe++;
  } else {
    goto skip;
  }

  if ( frames <= ithframe ) {
    anl_msg_error("\
%s: too many frames (%d), something is wrong\n", pname, ithframe);
    anl_put_exit_status(1);
    *status = ANL_QUIT;
    return;
  }

  /*=========== initial frame or 1st event in a frame ===== */
  BnkfGetM("XIS:FRAMES:FRAME_END_AETIME", sizeof(double), &used, &t1);
  /* printf("frame end aetime %lf\n", t1); */
  BnkfGetM("XIS:FRAMES:EVENT_TOT_NO", sizeof(ev_tot_no), &used, &ev_tot_no);
  /* printf("event tot no %d %d %d %d\n",
     ev_tot_no[0], ev_tot_no[1], ev_tot_no[2], ev_tot_no[3]); */

  frame_st_aetime[ithframe] = t0;
  frame_end_aetime[ithframe] = t1;
  frame_event_totno[ithframe] = 0;

  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    if ( check_seg[iseg] ) {
      frame_event_totno[ithframe] += ev_tot_no[iseg];
    }
  }
  event_totno_inlist[ithframe] = 0;

/* set rawy for the 1st event */
  if ( wingti ) {
    rawy_max_inlist[ithframe] = 0;
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      rawy_max_tmp_seg[iseg] = 0;
    }
  }

 skip:

  BnkfGetM("XIS:SEGMENT", sizeof(int), &used, &segment);
  if ( check_seg[segment] ) {
    event_totno_inlist[ithframe]++;
  }

/* status info */
  eventnum_seg[segment]++;

  if ( wingti ) {
    BnkfGetM("XIS:RAWY", sizeof(int), &used, &rawy);
    if ( rawy_max_tmp_seg[segment] < rawy ) {
      rawy_max_tmp_seg[segment] = rawy;
    }

/* select miminum (for the safety reason) rawy among segements */
    rawy_max_tmp = -1;
    for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
      if ( check_seg[iseg] ) {
	if ( -1 == rawy_max_tmp || rawy_max_tmp_seg[iseg] < rawy_max_tmp ) {
	  rawy_max_tmp = rawy_max_tmp_seg[iseg];
	}
      }
    }
    rawy_max_inlist[ithframe] = rawy_max_tmp;
  }

  EvsfSetM("XIScheckEventNo:OK");

  *status = ANL_OK;
}

void
XIScheckEventNo_endrun(int *status)
{
  *status = ANL_OK;
}

void
XIScheckEventNo_exit(int *status)
{
  int j;
  int iseg;
  int igti = 0;
  int flag_goodtime = ANL_FALSE;
  int istat = 0;
  double eventnum_all = 0.0;

  if ( wingti ) {
    addToStatistics_wingti(ithframe);	/* add last frame */
  } else {
    addToStatistics(ithframe);		/* add last frame */
  }

/* status info */
  for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
    eventnum_all += eventnum_seg[iseg];
  }

  for (j = 0; j < n_tested && checkSaturation(j); j++) {
    ;
  }

  if ( j < n_tested ) {
    gti.start[igti] = frame_st_aetime[j];
    flag_goodtime = ANL_TRUE;
    j++;
  }

  if ( 0 == wingti || 1 == windownumber ) {	/* wingti=no || Full window */

    while ( j < n_tested ) {

      if ( flag_goodtime ) {		/* during good time */

	if ( checkSaturation(j) ) {	/* staurated frame, not usable */
	  flag_goodtime = ANL_FALSE;
	  gti.stop[igti] = frame_end_aetime[j-1];
	  igti++;

	} else {			/* not saturated, usable frame */

	  if ( checkTimeJump(j) ) {	/* frame time jump detected */
	    gti.stop[igti] = frame_end_aetime[j-1];
	    igti++;
	    gti.start[igti] = frame_st_aetime[j];
	  }

	}

      } else {			/* during bad time */

	if ( 0 == checkSaturation(j) ) {
	  /* not saturated, usable frame */
	  flag_goodtime = ANL_TRUE;
	  gti.start[igti] = frame_st_aetime[j];
	}
      }

      j++;
    }

  } else {	/* wingti=yes && window or timing mode */

    while ( j < n_tested ) {

      if ( flag_goodtime ) {		/* during good time */

	if ( checkSaturation(j) ) {	/* staurated frame, not usable */
	  flag_goodtime = ANL_FALSE;
	  /* gti.stop[igti] = frame_end_aetime[j-1]; */
	  gti.stop[igti] = frame_st_aetime[j] + frame_exposure_inlist[j];
	  igti++;

	} else {			/* not saturated, usable frame */

	  if ( checkTimeJump(j) ) {	/* frame time jump detected */
	    gti.stop[igti] = frame_end_aetime[j-1];
	    igti++;
	    gti.start[igti] = frame_st_aetime[j];
	  }

	}

      } else {			/* during bad time */

	if ( 0.0 < frame_exposure_inlist[j] ) {
	  if ( 0 == checkSaturation(j) ) {
	    /* not saturated at all, completely usable frame */
	    flag_goodtime = ANL_TRUE;
	    gti.start[igti] = frame_st_aetime[j];
	  } else {
	    /* not fully saturated, partically usable frame */
	    gti.start[igti] = frame_st_aetime[j];
	    gti.stop[igti] = frame_st_aetime[j] + frame_exposure_inlist[j];
	    igti++;
	  }
	}
      }

      j++;
    }

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

  if ( NULL != ofp ) {
    if ( write_gti(ofp) ) {
      goto quit;
    }

    fits_close_file(ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, outfile, istat);
      goto quit;
    }

    anl_msg_info("\
%s: GTI file '%s' created\n", pname, outfile);
  }

  anl_msg_always("\
%s: GTI file %d column\n", pname, gti.ngti);

  anl_msg_always("\
N_FRAMES = %16ld / %s\n\
N_TESTED = %16d / %s\n\
N_PASSED = %16d / %s\n\
N_T_JUMP = %16d / %s\n\
N_SATURA = %16d / %s\n\
T_TESTED = %16.6f / %s\n\
T_PASSED = %16.6f / %s\n\
T_T_JUMP = %16.6f / %s\n\
T_SATURA = %16.6f / %s\n",
	frames, cm.N_FRAMES,
	n_tested, cm.N_TESTED,
	n_passed, cm.N_PASSED,
	n_time_jump, cm.N_T_JUMP,
	n_saturated, cm.N_SATURA,
	t_tested, cm.T_TESTED,
	t_passed, cm.T_PASSED,
	t_time_jump, cm.T_T_JUMP,
	t_saturated, wingti ? cm.T_SATURA_wingti : cm.T_SATURA);

  if ( wingti ) {
    anl_msg_always("\
T_SATU_F = %16.6f / %s\n",
	t_saturated_frame, cm.T_SATU_F_wingti);
  }

/* status info */
  if ( 0.0 < eventnum_all ) {
    anl_msg_always("\n\
SEGMENT_A %10d events (%6.2f %%) LossTime = %.3f [s]\n\
SEGMENT_B %10d events (%6.2f %%) LossTime = %.3f [s]\n\
SEGMENT_C %10d events (%6.2f %%) LossTime = %.3f [s]\n\
SEGMENT_D %10d events (%6.2f %%) LossTime = %.3f [s]\n\
TOTAL     %10.0f events (%6.2f %%) LossTime = %.3f [s]\n",
eventnum_seg[0], 100.0 * eventnum_seg[0] / eventnum_all, t_saturated_seg[0],
eventnum_seg[1], 100.0 * eventnum_seg[1] / eventnum_all, t_saturated_seg[1],
eventnum_seg[2], 100.0 * eventnum_seg[2] / eventnum_all, t_saturated_seg[2],
eventnum_seg[3], 100.0 * eventnum_seg[3] / eventnum_all, t_saturated_seg[3],
eventnum_all,    100.0, t_saturated
	);
  } else {
    anl_msg_warning("\n\
%s: WARNING: eventnum is zero !!!\n", pname);
  }

  free(gti.start);
  free(frame_event_totno);
  free(frame_st_aetime);

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
