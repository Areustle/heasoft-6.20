/*
 XRStimeSet.c

	2005/02/16 Y.ISHISAKI	version 1.0

	2005/04/21 Y.ISHISAKI	version 1.1
		bug fix in BnkPut('XRS:TIME')

	2005/04/25 Y.ISHISAKI	version 1.2
		BnkGet XRS:ETI -> XRS:TI, XRS:PACKET_TIME -> XRS:RECV_TIME

	2005/04/30 Y.ISHISAKI	version 1.3
		change column name REASON -> TIME_QUALITY
		BnkPut XRS:TIME_QUALITY

	2005/05/03 Y.ISHISAKI	version 1.4
		rename all RECV_TIME -> S_TIME

	2005/05/04 Y.ISHISAKI	version 1.5
		re-calculate GTI, update header keywords
		add gti_gap_tolerance, leapsec parameter
		write history for leapfile, margin_sec, gti_gap_tolerance

	2005/05/15 Y.ISHISAKI	version 1.6
		change aste_ti2time_init() arguments
		parameters 'time_pkt_file', 'dp_timc_file' merged into 'timfile'

	2005/07/04,05 Y.ISHISAKI	version 1.7
		change auto completion file name '_tick.hk' -> '.hk'
		change parameter name tick_hk_file -> hkfile
		add printf("\n") at the end of _init()

	2005/07/26	Y.ISHISAKI	version 1.8
		check pixel by pixel time inversion after time asignment

	2005/07/27	Y.ISHISAKI	version 1.9
		print & write history time quality statistics
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_ti2time.h"
#include "xrs_tick2ti.h"
#include "pil.h"
#include "headas.h"
#include "xrsFitsHeaderUtil.h"

static char pname[] = "XRStimeSet";
char XRStimeSet_version[] = "version 1.9";

#define DEFAULT_MARGIN_SEC	60

static struct {
	char tim_file[1024];
	char tick_hk_file[1024];
	char leapfile[1024];
	double margin_sec;
	double gti_gap_tolerance;

	char f_tim[1024];		/* completed file name for tim_file */
	char f_tick_hk[1024];	/* completed file name for tick_hk_file */

	XRS_TICK2TI *xtp;
	TI2TIME *ttp;

	double prev_time[32];
	int stat_tinv[32];		/* time inversion statistics */
	int stat_tqua[32][10];	/* time quality statistics */
} com;

XRS_STD_KEYS stdkeys;

static int
compare_time(double *t1, double *t2)
{
	if ( *t1 < *t2 ) {
		return -1;
	} else if ( *t2 < *t1 ) {
		return +1;
	}
	return 0.0;
}

static int
complete_fn(char *fff_name, char *hk_name, int hk_size, char *hk_ext)
{
	static char fff_ext[] = ".fff";
	static char fff_ext_len = sizeof(fff_ext) - 1;
	int len, hk_ext_len;

	if ( 0 == strcasecmp("auto", hk_name) ) {
		len = strlen(fff_name) - fff_ext_len;
		hk_name[hk_size - 1] = '\0';
		if ( 0 == strcmp(".fff", fff_name + len) ) {
			if ( len < hk_size ) {
				strncpy(hk_name, fff_name, len);
				hk_ext_len = strlen(hk_ext);
				if ( len + hk_ext_len < hk_size ) {
					strcpy(hk_name+len, hk_ext);
				} else {
					fprintf(stderr, "\
%s: too long fff name '%s'\n", pname, fff_name);
					return -1;
				}
			}
		} else {
			fprintf(stderr, "\
%s: inappropriate fff name for auto completion\n", pname);
			return -1;
		}
	}

	return 0;
}

static void
show_parameter(void)
{
	static char form1[] = "%20s   '%s'\n";
	static char form2[] = "%20s   '%s' (%s)\n";
	char *key, *format;

	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");

	key = "TIMFILE";
	format = form1;
	if ( 0 != strcmp(com.tim_file, com.f_tim) ) format = form2;
	printf(format, key, com.tim_file, com.f_tim);

	key = "HKFILE";
	format = form1;
	if ( 0 != strcmp(com.tick_hk_file, com.f_tick_hk) ) format = form2;
	printf(format, key, com.tick_hk_file, com.f_tick_hk);

	printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	printf("%20s   %.3f\n", "MARGIN_SEC", com.margin_sec);
	printf("%20s   %.3f\n", "GTI_GAP_TOLERANCE", com.gti_gap_tolerance);
}

void
XRStimeSet_startup(int *status)
{
	strcpy(com.tim_file, "none");
	strcpy(com.tick_hk_file, "auto");
	com.margin_sec = 60.0;
	com.gti_gap_tolerance = 24.0;

	*status = ANL_OK;
}

void
XRStimeSet_com(int *status)
{
#define NVAL	7
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"TIMFILE",
		"HKFILE",
		"LEAPFILE",
		"MARGIN_SEC",
		"GTI_GAP_TOLERANCE",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		".tim FITS file name",
		"xrs_tick.hk FITS file name",
		"leap seconds table file name",
		"time margin [s] to judge TICK entry is valid",
		"time gap tolerance [s] for new GTI rows",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	if ( *status ) {	/* ftools */

		if (
PILGetFname("timfile", com.tim_file) ||
PILGetFname("hkfile", com.tick_hk_file) ||
PILGetFname("leapfile", com.leapfile) ||
PILGetReal ("margin_sec", &com.margin_sec) ||
PILGetReal ("gti_gap_tolerance", &com.gti_gap_tolerance) ||
			 0 ) {
			*status = ANL_QUIT;
			return;
		}

		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("TIMFILE", p) ) {
			CLtxtrd(p, com.tim_file, sizeof(com.tim_file));
		} else if ( 0 == strcmp("HKFILE", p) ) {
			CLtxtrd(p, com.tick_hk_file, sizeof(com.tick_hk_file));
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.leapfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("MARGIN_SEC", p) ) {
			CLfdprd(p, &com.margin_sec);
		} else if ( 0 == strcmp("GTI_GAP_TOLERANCE", p) ) {
			CLfdprd(p, &com.gti_gap_tolerance);
		}else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
XRStimeSet_init(int *status)
{
	int i, used, istat;
	char *fff_name;

	BnkGet("XRS:EVENT:IFILE_NAME:PTR", sizeof(fff_name), &used, &fff_name);

	strcpy(com.f_tim, com.tim_file);
	strcpy(com.f_tick_hk, com.tick_hk_file);
	istat = complete_fn(fff_name, com.f_tick_hk, sizeof(com.f_tick_hk), ".hk");

	show_parameter();

	printf("\n"); fflush(NULL);

	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	if ( *com.leapfile && NULL == mission_time_init(NULL) ) {
		mission_time_init(com.leapfile);
	}

	istat = xrs_tick2ti_init(&com.xtp, com.f_tick_hk);
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	istat = aste_ti2time_init(&com.ttp, com.f_tim);
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	printf("\n"); fflush(NULL);

	for (i = 0; i < 32; i++) {
		com.stat_tinv[i] = -1;
	}
	memset(com.stat_tqua, 0, sizeof(com.stat_tqua));

	*status = ANL_OK;
}

void
XRStimeSet_his(int *status)
{
	*status = ANL_OK;
}

void
XRStimeSet_bgnrun(int *status)
{
	static char form1[] = "  %s='%s'";
	static char form2[] = "  %s='%s' (%s)";
	char *key, *format;

	fitsfile *fp;
	int used, istat;
	char buf[1024];

	used = 0;
	BnkGet("XRS:EVENT:OFP", sizeof(fp), &used, &fp);
	if ( used != sizeof(fp) ) {
/* output file not opend */
		*status = ANL_OK;
		return;
	}

/* initialize cfitsio return status */
	istat = 0;

/* write parameters into the event file header */
	sprintf(buf, "%s %s", pname, XRStimeSet_version);
	fits_write_history(fp, buf, &istat);

	key = "timfile";
	format = form1;
	if ( 0 != strcmp(com.tim_file, com.f_tim) ) format = form2;
	sprintf(buf, format, key, com.tim_file, com.f_tim);
	fits_write_history(fp, buf, &istat);

	key = "hkfile";
	format = form1;
	if ( 0 != strcmp(com.tick_hk_file, com.f_tick_hk) ) format = form2;
	sprintf(buf, format, key, com.tick_hk_file, com.f_tick_hk);
	fits_write_history(fp, buf, &istat);

	key = "leapfile";
	sprintf(buf, form1, key, com.leapfile);
	fits_write_history(fp, buf, &istat);

	key = "margin_sec";
	sprintf(buf, "  %s=%.6f", key, com.margin_sec);
	fits_write_history(fp, buf, &istat);

	key = "gti_gap_tolerance";
	sprintf(buf, "  %s=%.6f", key, com.gti_gap_tolerance);
	fits_write_history(fp, buf, &istat);

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
	}

	*status = ANL_OK;
}

void
XRStimeSet_ana(int *nevent, int *eventid, int *status)
{
	int istat, used, pixel, tick, tick_bits, sample, vernier, time_quality;
	double s_time, ti_dbl, aetime;
	unsigned int pkt_ti, ti;

	BnkfGetM("XRS:S_TIME", sizeof(s_time), &used, &s_time);
	BnkfGetM("XRS:PIXEL", sizeof(pixel), &used, &pixel);
	BnkfGetM("XRS:TI", sizeof(pkt_ti), &used, &pkt_ti);
	BnkfGetM("XRS:TICK_COUNTER", sizeof(tick), &used, &tick);
	BnkfGetM("XRS:SAMPLE_COUNT", sizeof(sample), &used, &sample);
	BnkfGetM("XRS:TIME_VERNIER", sizeof(vernier), &used, &vernier);

	tick_bits = 4;
	istat = xrs_tick2ti(com.xtp, pkt_ti, s_time, pixel, tick, tick_bits,
				com.margin_sec, &ti, &time_quality);
	if ( istat ) {
/* this should not happen, but keep this code for check purpose */
		fprintf(stderr, "\
%s: xrs_tick2ti() failed at:\n\
    s_time=%.0f, pkt_ti=%08xh, tick=%02xh\n",
			pname, s_time, pkt_ti, tick);
		ti = pkt_ti;
		time_quality = 9;
	}

	ti_dbl = ti + 4096 * ( (sample*16 + vernier) / 12288. / 16. );

	istat = aste_ti2time_dp_dbl(com.ttp, ti_dbl, s_time, &aetime);
	if ( istat ) {
		fprintf(stderr, "\
%s: aste_ti2time() failed at:\n\
    s_time=%.0f, pkt_ti=%08xh, tick=%02xh\n",
			pname, s_time, pkt_ti, tick);
		aetime = s_time;
		time_quality = 9;
	}

	BnkfPutM("XRS:TIME", sizeof(aetime), &aetime);
	BnkfPutM("XRS:TIME_QUALITY", sizeof(time_quality), &time_quality);

	if ( pixel < 0 || 32 <= pixel ) {
		fprintf(stderr, "\
%s: invalid pixel value (%d)\n", pname, pixel);
		*status = ANL_QUIT;
		return;
	}
	if ( -1 == com.stat_tinv[pixel] ) {
		com.stat_tinv[pixel] = 0;
	} else {
		if ( aetime < com.prev_time[pixel] ) {
			com.stat_tinv[pixel]++;
			fprintf(stderr, "\
%s: WARNING: PIXEL=%-2d TIME inversion: %.3f -> %.3f\n",
	pname, pixel, com.prev_time[pixel], aetime);
		}
	}
	com.prev_time[pixel] = aetime;

	if ( 0 <= time_quality && time_quality < 10 ) {
		com.stat_tqua[pixel][time_quality]++;
	} else {
/* this should not happen, but keep this code for check purpose */
		fprintf(stderr, "\
%s: WARNING: invalid TIME_QUALITY=%d\n", pname, time_quality);
	}

	*status = ANL_OK;
	return;
}

void
XRStimeSet_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRStimeSet_exit(int *status)
{
	fitsfile *ofp;
	long orow, nevent, ngti;
	int i, used, hdutype, tcol, anul;
	double *tbuf, t0, t1, ontime;
	char history[120];

	int istat = 0;

/* get output fits file pointer */
	BnkGet("XRS:EVENT:OFP", sizeof(ofp), &used, &ofp);
	BnkGet("XRS:EVENT:OROW", sizeof(orow), &used, &orow);

	nevent = orow;
	if ( 0 == nevent ) {
	/* no data, do nothing */
		*status = ANL_OK;
		return;
	}

/* move to 1st extension */
	fits_movabs_hdu(ofp, 2, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
		goto error;
	}

/* print time inversion statistics */
	strcpy(history, "\
                     TIME IVERSION STATISTICS");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);

	strcpy(history, "\
  ===============================================================");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	strcpy(history, "\
   PIXEL     NUM   PIXEL     NUM   PIXEL     NUM   PIXEL     NUM");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	strcpy(history, "\
  ---------------------------------------------------------------");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	for (i = 0; i < 8; i++) {
		sprintf(history, "\
   %3d:%9d   %3d:%9d   %3d:%9d   %3d:%9d",
				i*4+0, com.stat_tinv[i*4+0],
				i*4+1, com.stat_tinv[i*4+1],
				i*4+2, com.stat_tinv[i*4+2],
				i*4+3, com.stat_tinv[i*4+3]);
		printf("%s\n", history); fits_write_history(ofp, history, &istat);
	}
	strcpy(history, "\
  ---------------------------------------------------------------");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* print time quality statistics */
	strcpy(history, "\
                     TIME QUALITY STATISTICS");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);

	strcpy(history, "\
  =====================================================================");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	strcpy(history, "\
   PIXEL      0      1      2      3      4      5    6    7    8    9");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	strcpy(history, "\
  ---------------------------------------------------------------------");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	for (i = 0; i < 32; i++) {
		sprintf(history, "\
   %3d%9d%7d%7d%7d%7d%7d%5d%5d%5d%5d",
			i, com.stat_tqua[i][0], com.stat_tqua[i][1], com.stat_tqua[i][2],
			com.stat_tqua[i][3], com.stat_tqua[i][4], com.stat_tqua[i][5],
			com.stat_tqua[i][6], com.stat_tqua[i][7], com.stat_tqua[i][8],
			com.stat_tqua[i][9]);
		printf("%s\n", history); fits_write_history(ofp, history, &istat);
	}
	strcpy(history, "\
  ---------------------------------------------------------------------");
	printf("%s\n", history); fits_write_history(ofp, history, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* get column number of TIME */
	fits_get_colnum(ofp, CASESEN, "TIME", &tcol, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_get_colnum('TIME') failed (%d)\n", pname, istat);
		goto error;
	}

/* allocate memory */
	tbuf = malloc(nevent * sizeof(*tbuf));
	if ( NULL == tbuf ) {
		fprintf(stderr, "\
%s: malloc(%ld) failed for TIME column\n", pname, nevent);
		goto error;
	}

/* read TIME column into memory */
	fits_read_col_dbl(ofp, tcol, 1, 1, nevent, 0.0, tbuf, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_col_dbl('TIME') failed (%d)\n", pname, istat);
		goto error;
	}

/* try sorting for myself */
	fflush(NULL); printf("\
%s: sorting %ld events ...", pname, nevent);
	fflush(NULL);
	qsort(tbuf, nevent, sizeof(*tbuf), compare_time);
	printf(" done\n");
	fflush(NULL);

/* move to 2nd extension */
	fits_movabs_hdu(ofp, 3, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
		goto error;
	}

	fits_read_key_lng(ofp, "NAXIS2", &ngti, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	}

	fits_delete_rows(ofp, 1, ngti, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_delete_rows() failed (%d)\n", pname, istat);
		goto error;
	}

	ngti = 0;
	t0 = t1 = ontime = 0.0;
	for (i = 0; i < nevent; i++) {
		if ( 0.0 == tbuf[i] ) {		/* ignore TIME==0.0 */
			continue;
		}
		if ( 0.0 == t0 ) {
			t0 = t1 = tbuf[i];
			continue;
		}
		if ( t1 + com.gti_gap_tolerance < tbuf[i] ) {
			ngti++;
			fits_write_col_dbl(ofp, 1, ngti, 1, 1, &t0, &istat);
			fits_write_col_dbl(ofp, 2, ngti, 1, 1, &t1, &istat);
			ontime += t1 - t0;
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_write_col('START/STOP') failed (%d)\n", pname, istat);
				goto error;
			}
			t0 = t1 = tbuf[i];
			continue;
		}
		t1 = tbuf[i];
	}

	ngti++;
	fits_write_col_dbl(ofp, 1, ngti, 1, 1, &t0, &istat);
	fits_write_col_dbl(ofp, 2, ngti, 1, 1, &t1, &istat);
	ontime += t1 - t0;
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_col('START/STOP') failed (%d)\n", pname, istat);
		goto error;
	}

	stdkeys.tstart = tbuf[0];
	stdkeys.tstop  = tbuf[nevent-1];
	stdkeys.ontime = ontime;
	istat = xrsUpdateStdTimeKeys(ofp, &stdkeys);
	if ( istat ) {
		goto error;
	}

/* move to 1st extension */
	fits_movabs_hdu(ofp, 2, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
		goto error;
	}
	istat = xrsUpdateStdTimeKeys(ofp, &stdkeys);
	if ( istat ) {
		goto error;
	}

	free(tbuf);

	*status = ANL_OK;
	return;

 error:
	*status = ANL_QUIT;
	return;
}
