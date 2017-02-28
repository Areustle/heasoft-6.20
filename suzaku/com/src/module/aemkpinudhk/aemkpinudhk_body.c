/*
  aemkpinudhk_body.c

	2007/09/24 Y.ISHISAKI	version 1.1

	2007/11/10 Y.ISHISAKI	version 1.2
		use copy_keys() instead of aeWriteStdKeys()
		comment out aeUpdateStdTimeKeys()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_orbit.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aemkpinudhk";
char aemkpinudhk_version[] = "version 1.2";

typedef struct {
	double aetime;
	float period;
	unsigned int cts_all;
} PINUD_COUNT_1ROW;

typedef struct {
	long nrow;
	long ipos;
	PINUD_COUNT_1ROW *p;
} PINUD_COUNT_DATA;

typedef struct {
	double aetime, t0, t1;
	double pinud_cps;
	double pinud_err;
	double pinud_exp;
	int pinud_cts;
	int pinud_bad;
} PINUD_CPS_1ROW;

typedef struct {
	double aetime;
	int pinud[16];
} PINUD_HXDHK_1ROW;

static struct {
/* aemkpinudhk */
	char *outfile, o_outfile[PIL_LINESIZE];
	char hkfile[PIL_LINESIZE];
	char reference_file[PIL_LINESIZE];
	int reffile_none;
	char orbit_file[PIL_LINESIZE];
	char *leapfile, o_leapfile[PIL_LINESIZE];
	char *rigidity, o_rigidity[PIL_LINESIZE];
	char time_col_name[PIL_LINESIZE];
	double t0, t1;
	double pinud_cps_lo;
	double pinud_cps_hi;
	double pinud_period_lo;
	double pinud_period_hi;
	int clobber;

	PINUD_COUNT_DATA pinud;
	int nbad;

	ORBIT *orbit;
	fitsfile *reffile;
	AtRigData2 *rdp;
	int time_col_num;
	long irow, nrow;

	fitsfile *ofp;
	long orow;

} com;

static AE_STD_KEYS stdkeys;

static int
copy_keys(fitsfile *ofp, fitsfile *ifp)
{
	static char skip_to_key[] = "TELESCOP";
	static char *ignore_keys[] = {
		"BITPIX", "NAXIS*", "PCOUNT", "GCOUNT", "TFIELDS", "THEAP",
		"TBCOL*", "TTYPE*", "TFORM*", "TUNIT*", "TSCAL*", "TZERO*",
		"TDISP*", "TDIM*", "TNULL*",
		"TLMIN*", "TLMAX*", "TDMIN*", "TDMAX*",
		"TCDLT*", "TCRPX*", "TCRVL*", "TCTYP*", "TCUNI*", "TCROT*",
		"CRPIX*", "CRVAL*", "CDELT*", "CROTA*", "PC*", "CD*", "PV*",
		"CRDER*", "CSYER*", "CTYPE*", "CUNIT*", "PS*",
		"BZERO", "DATAMAX", "DATAMIN", "LONPOLE", "LATPOLE",
		"BUNIT", "WCSNAME", "NPIXSOU", NULL
	};
	static char *accept_keys[] = {
		"COMMENT", "HISTORY", NULL
	};

	int i, len, keysexist, keynum;
	char keyname[FLEN_KEYWORD], card[FLEN_CARD], card2[FLEN_CARD];

	int istat = 0;

	fits_read_card(ifp, skip_to_key, card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, skip_to_key, istat);
		goto quit;
	}
	fits_get_hdrpos(ifp, &keysexist, &keynum, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_hdrpos() failed (%d)\n", pname, istat);
		goto quit;
	}

	for (keynum = keynum - 1; keynum <= keysexist; keynum++) {
		fits_read_record(ifp, keynum, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_record() failed at keynum=%d (%d)\n", pname, keynum, istat);
			goto quit;
		}
		strncpy(keyname, card, 8);
		keyname[8] = '\0';
		for (i = 7; 0 <= i; i--) {
			if ( ' ' == keyname[i] ) {
				keyname[i] = '\0';
			}
		}
		for (i = 0; NULL != ignore_keys[i]; i++) {
			if ( 0 == strcmp(keyname, ignore_keys[i]) ) {
				break;
			}
			len = strlen(ignore_keys[i]);
			if ( 0 < len && '*' == ignore_keys[i][len-1] ) {
				if ( 0 == strncmp(keyname, ignore_keys[i], len-1) ) {
					break;
				}
			}
		}
		if ( NULL != ignore_keys[i] ) {
			continue;
		}
		for (i = 0; NULL != accept_keys[i]; i++) {
			if ( 0 == strcmp(keyname, accept_keys[i]) ) {
				break;
			}
		}
		if ( NULL == accept_keys[i] ) {
			fits_read_card(ofp, keyname, card2, &istat);
			if ( 0 == istat ) {
				continue;
			} else if ( KEY_NO_EXIST == istat ) {
				istat = 0;
			} else {
				anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, keyname, istat);
				goto quit;
			}
		}
		fits_write_record(ofp, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_card('%s') failed (%d)\n", pname, keyname, istat);
			goto quit;
		}
	}

	return 0;

 quit:
	return istat;
}

static int
write_keys(fitsfile *fp)
{
	char *k;
	fitsfile *hxd_fp;
	int istat, istat2;

	AE_STD_KEYS *s = &stdkeys;

	istat = istat2 = 0;

/* copy keywords from HXD HK file */
	fits_open_file(&hxd_fp, com.hkfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, com.hkfile, istat);
		return istat;
	}
	istat = copy_keys(fp, hxd_fp);
	fits_close_file(hxd_fp, &istat2);
	if ( istat ) {
		return istat;
	}
	if ( istat2 ) {
		anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat2);
		return istat2;
	}

	if (
fits_update_key_str(fp, k="CREATOR", s->creator, s->comment.creator, &istat) ||
fits_update_key_str(fp, k="ORB_FILE",s->orb_file,s->comment.orb_file,&istat) ||
fits_update_key_str(fp, k="LEAPFILE",s->leapfile,s->comment.leapfile,&istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

#if 0
/* write standard header keywords */
	istat = aeWriteStdKeys(fp, &stdkeys);
	if ( istat ) {
		anl_msg_error("\
%s: aeWriteStdKeys() failed (%d)\n", pname, istat);
		return istat;
	}
#endif

	return istat;
}

static char *
get_filename_nogz(char *filename)
{
	char *filename_nogz;
	char *base = aefits_basename(filename);
	int len = strlen(base);

	if ( 3 < len && 0 == strcmp(".gz", &base[len-3]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-3] = '\0';
			return filename_nogz;
		}
	} else if ( 2 < len && 0 == strcmp(".Z", &base[len-2]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-2] = '\0';
			return filename_nogz;
		}
	} else if ( 2 < len && 0 == strcmp(".z", &base[len-2]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-2] = '\0';
			return filename_nogz;
		}
	}

	return base;
}

static int
read_hxdhk(fitsfile *fp, long nrow, int co_time, PINUD_COUNT_DATA *pinud_count)
{
	static char *hkname[16] = {
	"HXD_HKA_PINUD00", "HXD_HKA_PINUD01", "HXD_HKA_PINUD02", "HXD_HKA_PINUD03",
	"HXD_HKA_PINUD10", "HXD_HKA_PINUD11", "HXD_HKA_PINUD12", "HXD_HKA_PINUD13",
	"HXD_HKA_PINUD20", "HXD_HKA_PINUD21", "HXD_HKA_PINUD22", "HXD_HKA_PINUD23",
	"HXD_HKA_PINUD30", "HXD_HKA_PINUD31", "HXD_HKA_PINUD32", "HXD_HKA_PINUD33"
	};

	char *k;
	int i, anul, colnum[16], cts_all, scaler[16], last_scaler[16];
	double aetime, last_aetime;
	long irow;

	int istat = 0;

	for (i = 0; i < 16; i++) {
		fits_get_colnum(fp, CASESEN, k=hkname[i], &colnum[i], &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	for (irow = 1; irow <= nrow; irow++) {
fits_read_col_dbl(fp, co_time, irow, 1, 1, 0.0, &aetime, &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
			goto quit;
		}

		for (i = 0; i < 16; i++) {
fits_read_col_int(fp, colnum[i], irow, 1, 1, 0, &scaler[i], &anul, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_read_col('%s') failed at irow=%ld (%d)\n",
					pname, hkname[i], irow, istat);
				goto quit;
			}
		}

		if ( 1 < irow ) {
			cts_all = 0;
			for (i = 0; i < 16; i++) {
				if ( last_scaler[i] <= scaler[i] ) {
					cts_all += scaler[i] - last_scaler[i];
				} else {
					cts_all += 16777216 + scaler[i] - last_scaler[i];
				}
			}
			if ( 65535 < cts_all ) {
				cts_all = 65535;
			}
			pinud_count->p[irow-2].aetime = (aetime + last_aetime) / 2;
			pinud_count->p[irow-2].period = aetime - last_aetime;
			pinud_count->p[irow-2].cts_all = cts_all;
		}

		last_aetime = aetime;
		memcpy(last_scaler, scaler, sizeof(scaler));
	}

	return 0;

 quit:
	return istat;
}

static int
compare_1row(void const *p1, void const *p2)
{
	double t1 = ((PINUD_COUNT_1ROW *)p1)->aetime;
	double t2 = ((PINUD_COUNT_1ROW *)p2)->aetime;

	if ( t1 < t2 ) {
		return -1;
	} else if ( t1 > t2 ) {
		return +1;
	}

	return 0;
}

static int
read_pinud_data(char *hkfile, int flag_sel, PINUD_COUNT_DATA *pinud_count)
{
	fitsfile *fp;
	int hdunum, hdutype, anul;
	int co, co_time, co_period, co_cts_all;
	long irow, nrow, nvalid, nrej;
	char *k;
	double aetime, period, cps;
	int cts_all;

	int istat = 0;

	pinud_count->nrow = 0;
	pinud_count->ipos = 0;
	pinud_count->p = NULL;
	nvalid = nrej = 0;

/* open pinud file */
	fits_open_file(&fp, hkfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, hkfile, istat);
		goto quit;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
	}

	fits_read_key_lng(fp, k="NAXIS2", &nrow, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	anl_msg_info("\
reading %s, nrows=%ld\n", hkfile, nrow);

	pinud_count->p = malloc( nrow * sizeof(*pinud_count->p) );
	if ( NULL == pinud_count->p ) {
		anl_msg_error("\
%s: malloc() failed for pinud_count->p, nrow=%ld\n", pname, nrow);
		istat = -1;
		goto quit;
	}

	fits_get_colnum(fp, CASESEN, k="TIME", &co_time, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	if (
fits_get_colnum(fp, CASESEN, k="PERIOD", &co_period, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTS_ALL", &co_cts_all, &istat) ||
		 0 ) {
		istat = read_hxdhk(fp, nrow, co_time, pinud_count);
		if ( istat ) {
			goto quit;
		}
		nrow--;
	} else {
		anl_msg_info("\
%s: INFO: PERIOD and CTS_ALL columns are found, using it\n", pname);
		for (irow = 1; irow <= nrow; irow++) {
			if (
fits_read_col_dbl(fp, co=co_time, irow, 1, 1, 0.0, &aetime, &anul, &istat) ||
fits_read_col_dbl(fp, co=co_period, irow, 1, 1, 0.0, &period, &anul, &istat) ||
fits_read_col_int(fp, co=co_cts_all, irow, 1, 1, 0, &cts_all, &anul, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_read_col(col=%d) failed at irow=%ld (%d)\n", pname, co, irow, istat);
				goto quit;
			}
			pinud_count->p[irow-1].aetime = aetime;
			pinud_count->p[irow-1].period = period;
			pinud_count->p[irow-1].cts_all = cts_all;
		}
	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, hkfile, istat);
		goto quit;
	}

	if ( 0 == flag_sel ) {
		pinud_count->nrow = nrow;
		return 0;
	}

	for (irow = 0; irow < nrow; irow++) {
		aetime = pinud_count->p[irow].aetime;
		period = pinud_count->p[irow].period;
		cts_all = pinud_count->p[irow].cts_all;

		if ( period < com.pinud_period_lo || com.pinud_period_hi < period ) {
			anl_msg_debug3("\
%s: DEBUG: strange period=%.3f rejected at irow=%ld\n", pname, period, irow);
			nrej++;
			continue;
		}
		if ( 65535 == cts_all ) {
			anl_msg_debug3("\
%s: DEBUG: too large cts_all=%d rejected at irow=%ld\n", pname, cts_all, irow);
			nrej++;
			continue;
		}
		cps = cts_all / period;
		if ( cps < com.pinud_cps_lo || com.pinud_cps_hi < cps ) {
			anl_msg_debug3("\
%s: DEBUG: strange cps=%.3f rejected at irow=%ld\n", pname, cps, irow);
			nrej++;
			continue;
		}
		pinud_count->p[nvalid].aetime = aetime;
		pinud_count->p[nvalid].period = period;
		pinud_count->p[nvalid].cts_all = cts_all;
		nvalid++;
	}

	qsort(pinud_count->p, nvalid, sizeof(*pinud_count->p), compare_1row);

	anl_msg_info("\
   nvalid=%ld  nrej=%ld  time=%.1f - %.1f [s]\n",
		nvalid, nrej,
		pinud_count->p[0].aetime, pinud_count->p[nvalid-1].aetime);

	pinud_count->nrow = nvalid;

	return 0;

 quit:
	pinud_count->nrow = 0;
	if ( NULL != pinud_count->p ) {
		free(pinud_count->p);
		pinud_count->p = NULL;
	}
	return istat;
}

static int
get_pinud_cps(PINUD_COUNT_DATA *pinud_count, PINUD_CPS_1ROW *output)
{
	int pinud_cts, pinud_bad;
	double pinud_exp, pinud_cps, pinud_err;

	double t0 = output->t0;
	double t1 = output->t1;
	long ipos = pinud_count->ipos;
	long nrow = pinud_count->nrow;
	PINUD_COUNT_1ROW *p = pinud_count->p;

	while ( 0 < ipos && t0 < p[ipos].aetime ) {
		ipos--;
	}

	pinud_cts = 0;
	pinud_bad = 0;
	pinud_exp = 0.0;

	while ( ipos < nrow && p[ipos].aetime <= t1 ) {
		if ( t0 <= p[ipos].aetime ) {
			pinud_cts += p[ipos].cts_all;
			pinud_exp += p[ipos].period;
		}
		ipos++;
	}

	if ( 0.0 < pinud_exp ) {
		pinud_cps = pinud_cts / pinud_exp;
		pinud_err = sqrt(pinud_cts) / pinud_exp;
	} else {
		pinud_cps = pinud_err = 0.0;
		pinud_bad = 1;
	}

	pinud_count->ipos = ipos;
	output->pinud_cps = pinud_cps;
	output->pinud_err = pinud_err;
	output->pinud_cts = pinud_cts;
	output->pinud_exp = pinud_exp;
	output->pinud_bad = pinud_bad;

	return pinud_bad;
}

static struct {
	char *ttype, *tform, *tunit, *tdisp, *comment;
	int index;
} pinud_hk_tbl[] = {
	{"TIME",		"1D", "s",		"F16.6","mission time (s)" },
	{"PINUD",		"1E", "c/s",	"F11.6","PIN UD count rate" },
	{"PINUD_ERR",	"1E", "c/s",	"F11.6","Poisson error" },
	{"PINUD_EXP",	"1E", "s",		"F8.3",	"Exposure time" },
	{"PINUD_CTS",	"1U", "count",	"I6",	"Raw PIN UD count" },
	{"PINUD_BAD",	"1B", "",		"I3",	"Bad status flag"},
	{"COR",			"1E", "GV",		"F8.3",	"cut off rigidity with old table"},
	{"COR2",		"1E", "GV",		"F8.3",	"cut off rigidity with new table"},
	{NULL,NULL,NULL,NULL,NULL}
};

static int
create_pinud_hk(void)
{
	static char extname[] = "PINUD_HK";
	static char *rigidity_cm = "name of the cut-off rigidity file for COR2";

#define NF	(sizeof(pinud_hk_tbl) / sizeof(*pinud_hk_tbl) - 1)
	static int nf = NF;
	static char *ttype[NF], *tform[NF], *tunit[NF];
#undef NF

	int i;
	fitsfile *fp;
	char *k, *rigidity_file, keyname[16], card[FLEN_CARD];

	int tbltype = BINARY_TBL;
	int istat = 0;

/* print information message */
	anl_msg_info("\n\
%s: creating %s file '%s'\n", pname, extname, com.outfile);

/* create file */
	fits_create_file(&fp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_file('%s') failed\n", pname, com.outfile);
		return istat;
	}

/* create primary extension */
	fits_create_img(fp, BYTE_IMG, 0, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img failed (%d)\n", pname, istat);
		return istat;
	}

/* create table & define BNK */
	for (i = 0; i < nf; i++) {
		ttype[i] = pinud_hk_tbl[i].ttype;
		tform[i] = pinud_hk_tbl[i].tform;
		tunit[i] = pinud_hk_tbl[i].tunit;
	}
	fits_create_tbl(fp, tbltype, 0, nf, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl() failed for '%s' (%d)\n", pname, com.outfile, istat);
		return istat;
	}

/* modify comments */
	for (i = 0; i < nf; i++) {
		sprintf(keyname, "TTYPE%d", i+1);
		istat = aefits_modify_comment(fp, keyname, pinud_hk_tbl[i].comment);
		if ( istat ) {
			return istat;
		}
	}

/* set TDISPn keywords */
	for (i = 0; i < nf; i++) {
		char *tdisp;
		char key[16], comment[80];

		tdisp = pinud_hk_tbl[i].tdisp;
		if ( NULL == tdisp || '\0' == *tdisp ) {
			continue;
		}
		sprintf(key, "TDISP%d", i+1);
		sprintf(comment, "display format of %s", ttype[i]);
		fits_write_key_str(fp, key, tdisp, comment, &istat);
		if ( istat ) {
			break;
		}
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key('TDISPn') failed (%d)\n", pname, istat);
		return istat;
	}

/* copy keywords from HXD HK file */
	istat = write_keys(fp);
	if ( istat ) return istat;

/* insert RIGIDITY keyword after LEAPFILE */
	if ( fits_read_card(fp, k="LEAPFILE", card, &istat) ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	rigidity_file = get_filename_nogz(com.rdp->filename);
	fits_insert_key_str(fp, k="RIGIDITY", rigidity_file, rigidity_cm, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

/* keep keyword space for 'DATE' & 'CHECKSUM' */
	if ( fits_write_date(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
		return istat;
	}
	if ( fits_write_chksum(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_chksum failed (%d)\n", pname, istat);
		return istat;
	}

/* initialize row number */
	com.ofp = fp;
	com.orow = 1;

	return 0;
}

static struct {
	char *ttype, *tform, *tunit, *tdisp, *comment;
	int index;
} cts_pinud_tbl[] = {
	{"TIME",		"1D", "s",		"F16.6",	"mission time (s)" },
	{"PERIOD",		"1E", "c/s",	"F11.6",	"HK period (s)" },
	{"CTS_ALL",		"1U", "count",	"I6",		"Raw PIN UD count" },
	{NULL,NULL,NULL,NULL,NULL}
};

static int
create_cts_pinud(void)
{
	static char extname[] = "CTS_PINUD";

#define NF	(sizeof(cts_pinud_tbl) / sizeof(*cts_pinud_tbl) - 1)
	static int nf = NF;
	static char *ttype[NF], *tform[NF], *tunit[NF];
#undef NF

	int i;
	fitsfile *fp;
	char keyname[16];

	int tbltype = BINARY_TBL;
	int istat = 0;

/* print information message */
	anl_msg_info("\n\
%s: creating %s file '%s'\n", pname, extname, com.outfile);

/* create file */
	fits_create_file(&fp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_file('%s') failed\n", pname, com.outfile);
		return istat;
	}

/* create primary extension */
	fits_create_img(fp, BYTE_IMG, 0, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img failed (%d)\n", pname, istat);
		return istat;
	}

/* create table & define BNK */
	for (i = 0; i < nf; i++) {
		ttype[i] = cts_pinud_tbl[i].ttype;
		tform[i] = cts_pinud_tbl[i].tform;
		tunit[i] = cts_pinud_tbl[i].tunit;
	}
	fits_create_tbl(fp, tbltype, 0, nf, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl() failed for '%s' (%d)\n", pname, com.outfile, istat);
		return istat;
	}

/* modify comments */
	for (i = 0; i < nf; i++) {
		sprintf(keyname, "TTYPE%d", i+1);
		istat = aefits_modify_comment(fp, keyname, cts_pinud_tbl[i].comment);
		if ( istat ) {
			return istat;
		}
	}

/* set TDISPn keywords */
	for (i = 0; i < nf; i++) {
		char *tdisp;
		char key[16], comment[80];

		tdisp = cts_pinud_tbl[i].tdisp;
		if ( NULL == tdisp || '\0' == *tdisp ) {
			continue;
		}
		sprintf(key, "TDISP%d", i+1);
		sprintf(comment, "display format of %s", ttype[i]);
		fits_write_key_str(fp, key, tdisp, comment, &istat);
		if ( istat ) {
			break;
		}
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key('TDISPn') failed (%d)\n", pname, istat);
		return istat;
	}

/* copy keywords from HXD HK file */
	istat = write_keys(fp);
	if ( istat ) return istat;

/* keep keyword space for 'DATE' & 'CHECKSUM' */
	if ( fits_write_date(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
		return istat;
	}
	if ( fits_write_chksum(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_chksum failed (%d)\n", pname, istat);
		return istat;
	}

/* initialize row number */
	com.ofp = fp;
	com.orow = 1;

	return 0;
}

static int
write_cts_pinud(fitsfile *fp, PINUD_COUNT_DATA *pinud_count)
{
	long irow, nrow;
	double aetime, period;
	int ic, cts_all;

	int istat = 0;

	nrow = pinud_count->nrow;
	for (irow = 1; irow <= nrow; irow++) {
		aetime = pinud_count->p[irow-1].aetime;
		period = pinud_count->p[irow-1].period;
		cts_all = pinud_count->p[irow-1].cts_all;
		if (
fits_write_col_dbl(com.ofp, ic=1, irow, 1, 1, &aetime, &istat) ||
fits_write_col_dbl(com.ofp, ic=2, irow, 1, 1, &period, &istat) ||
fits_write_col_int(com.ofp, ic=3, irow, 1, 1, &cts_all, &istat) ||
		 0 ) {
			anl_msg_error("\
%s: fits_write_col('%s') failed at irow=%ld (%d)\n",
				pname, cts_pinud_tbl[ic-1].ttype, irow, istat);
			return istat;
		}
	}

	return istat;
}

static int
read_reference_file(char *reference_filename)
{
	int hdunum, hdutype, anynul, col;
	fitsfile *fp;
	long naxis2;
	char *key;
	double t0, t1;

	int istat = 0;

/* open time reference file */
	fits_open_file(&fp, reference_filename, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed\n", pname, reference_filename);
		goto quit;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
	}

	if ( '\0' == com.time_col_name ) {
		strcpy(com.time_col_name, "TIME");	/* default column name TIME */
	}
	key = com.time_col_name;
	fits_get_colnum(fp, CASESEN, key, &col, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed in '%s'\n", pname, key, reference_filename);
		goto quit;
	}

	fits_read_key_lng(fp, "NAXIS2", &naxis2, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: NAXIS2 not found in '%s'\n", pname, reference_filename);
		goto quit;
	}

	com.irow = 1;
	com.nrow = naxis2;

	fits_read_col_dbl(fp, col, com.irow, 1, 1, 0.0, &t0, &anynul, &istat);
	fits_read_col_dbl(fp, col, com.nrow, 1, 1, 0.0, &t1, &anynul, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col('%s') faild in '%s'\n", pname, key, reference_filename);
		goto quit;
	}

	anl_msg_info("\
%s: INFO: generate TIME from %.3f to %.3f,\n\
   referring '%s'(%s), %ld rows\n",
		pname, t0, t1, reference_filename, key, com.nrow);

	com.reffile = fp;
	com.time_col_num = col;
	com.t0 = t0;
	com.t1 = t1;

	return 0;

 quit:
	return istat;
}

void
aemkpinudhk_startup(int *status)
{
	com.outfile = com.o_outfile;
	com.leapfile = com.o_leapfile;
	com.rigidity = com.o_rigidity;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "OUTFILE", com.outfile);
	printf("%20s   '%s'\n", "HKFILE", com.hkfile);
	printf("%20s   '%s'\n", "REFERENCE", com.reference_file);

if ( 0 == com.reffile_none ) {
	printf("%20s   '%s'\n", "ORBIT", com.orbit_file);
	if ( com.rigidity == com.o_rigidity ) {
		printf("%20s   '%s'\n", "RIGIDITY", com.rigidity);
	} else {
		printf("%20s   '%s' (%s)\n", "RIGIDITY", com.rigidity, com.o_rigidity);
	}
}

	if ( com.leapfile == com.o_leapfile ) {
		printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	} else {
		printf("%20s   '%s' (%s)\n", "LEAPFILE", com.leapfile, com.o_leapfile);
	}

if ( 0 == com.reffile_none ) {
	printf("%20s   '%s'\n", "TIME_COL_NAME", com.time_col_name);
	printf("%20s   %.1f\n", "PINUD_CPS_LO", com.pinud_cps_lo);
	printf("%20s   %.1f\n", "PINUD_CPS_HI", com.pinud_cps_hi);
	printf("%20s   %.1f\n", "PINUD_PERIOD_LO", com.pinud_period_lo);
	printf("%20s   %.1f\n", "PINUD_PERIOD_HI", com.pinud_period_hi);
}

	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
	printf("\n");
}

void
aemkpinudhk_com(int *status)
{
#define NVAL	14
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"OUTFILE",
		"HKFILE",
		"ORBIT",
		"REFERENCE",
		"LEAPFILE",
		"RIGIDITY",
		"TIME_COL_NAME",
		"PINUD_CPS_LO",
		"PINUD_CPS_HI",
		"PINUD_PERIOD_LO",
		"PINUD_PERIOD_HI",
		"CLOBBER",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"output event file name",
		"input HXD HK file name",
		"input orbit file name",
		"time reference file name",
		"leap seconds table file name",
		"rigidity data file name",
		"time column name in the reference file",
		"valid lower range for PIN UD (c/s)",
		"valid upper range for PIN UD (c/s)",
		"valid lower range for HXD HK period (s)",
		"valid upper range for HXD HK period (s)",
		"overwrite output file if exists",
		"exit from this menu"
	};

	char *p, *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		*status = ANL_QUIT;
		if (
PILGetFname(k="outfile", com.outfile) ||
PILGetFname(k="hkfile", com.hkfile) ||
PILGetFname(k="reference", com.reference_file) ||
			 0 ) {
			goto error;
		}
		com.reffile_none = 0;
		if ( 0 == CLstricmp("none", com.reference_file) ) {
			com.reffile_none = 1;
		} else if (
PILGetFname(k="orbit", com.orbit_file) ||
PILGetFname(k="rigidity", com.o_rigidity) ||
PILGetReal (k="pinud_cps_lo", &com.pinud_cps_lo) ||
PILGetReal (k="pinud_cps_hi", &com.pinud_cps_hi) ||
PILGetReal (k="pinud_period_lo", &com.pinud_period_lo) ||
PILGetReal (k="pinud_period_hi", &com.pinud_period_hi) ||
PILGetString(k="time_col_name", com.time_col_name) ||
			 0 ) {
			goto error;
		}
		if (
PILGetFname(k="leapfile", com.o_leapfile) ||
PILGetBool (k="clobber", &com.clobber) ||
			 0 ) {
			goto error;
		}

		*status = ANL_OK;;
		return;

	error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;;
		return;
	}

	for (;;) {
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("OUTFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("HKFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.hkfile));
		} else if ( 0 == strcmp("ORBIT", p) ) {
			CLtxtrd(p, com.orbit_file, sizeof(com.orbit_file));
		} else if ( 0 == strcmp("REFERENCE", p) ) {
			CLtxtrd(p, com.reference_file, sizeof(com.reference_file));
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.leapfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("RIGIDITY", p) ) {
			CLtxtrd(p, com.o_rigidity, sizeof(com.o_rigidity));
		} else if ( 0 == strcmp("TIME_COL_NAME", p) ) {
			CLtxtrd(p, com.time_col_name, sizeof(com.time_col_name));
		} else if ( 0 == strcmp("PINUD_CPS_LO", p) ) {
			CLfdprd(p, &com.pinud_cps_lo);
		} else if ( 0 == strcmp("PINUD_CPS_HI", p) ) {
			CLfdprd(p, &com.pinud_cps_hi);
		} else if ( 0 == strcmp("PINUD_PERIOD_LO", p) ) {
			CLfdprd(p, &com.pinud_period_lo);
		} else if ( 0 == strcmp("PINUD_PERIOD_HI", p) ) {
			CLfdprd(p, &com.pinud_period_hi);
		} else if ( 0 == strcmp("CLOBBER", p) ) {
			CLlogrd(p, &com.clobber);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
aemkpinudhk_init(int *status)
{
	static char creator[80];	/* must be statically declared */

	char *task_name, *task_version;
	int verbose;

	int istat = 0;

	task_name = anl_task_name();
	task_version = anl_task_version();

/* caldb support */
	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}

	if ( 0 == com.reffile_none ) {
		com.rigidity = aste_caldb_find_rigidity(com.o_rigidity);
		if ( NULL == com.rigidity ) {
			goto quit;
		}
	}

/* show current parameters */
	show_parameter();

/* initialize aste_time */
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");

/* setup output file */
	aeSetDefaultKeywordValues(&stdkeys);
	stdkeys.hduclas1 = "TEMPORALDATA";
	stdkeys.hduclas2 = "HKP";
	sprintf(creator, "%s %s", task_name, task_version);
	stdkeys.creator = creator;
	if ( com.clobber ) {
		unlink(com.outfile);
	}
	stdkeys.leapfile = get_filename_nogz(com.leapfile);

/* check if reference="none" */
	com.reffile = NULL;
	if ( com.reffile_none ) {
		istat = read_pinud_data(com.hkfile, 0, &com.pinud);
		if ( istat ) goto quit;
		istat = create_cts_pinud();
		if ( istat ) goto quit;
		com.irow = 1;
		com.nrow = 0;
		goto skip;
	}

/* initialize rigidity for COR2 */
	istat = atRigSet2(&com.rdp, com.rigidity);
	if ( istat ) {
		anl_msg_error("\
%s: atRigSet2('%s') failed (%d)\n", pname, com.rigidity, istat);
		goto quit;
	}

/* initialize orbit file */
	istat = aste_orbit_init(&com.orbit, com.orbit_file);
	if ( istat ) {
		anl_msg_error("\
%s: error in orbit file '%s'\n", pname, com.orbit_file);
		goto quit;
	}
	stdkeys.orb_file = get_filename_nogz(com.orbit->orbit_file);
	anl_msg_info("\n");

/* read reference file */
	istat = read_pinud_data(com.hkfile, 1, &com.pinud);
	if ( istat ) goto quit;
	com.nbad = 0;
	istat = read_reference_file(com.reference_file);
	if ( istat ) goto quit;
	istat = create_pinud_hk();
	if ( istat ) goto quit;

 skip:

	istat = aefits_write_module_history(com.ofp, pname);
	if ( istat ) goto quit;

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
aemkpinudhk_his(int *status)
{
	*status = ANL_OK;
}

void
aemkpinudhk_bgnrun(int *status)
{
	char history[PIL_LINESIZE+80];

	int istat = 0;
	fitsfile *fp = com.ofp;

	sprintf(history, "  outfile='%s'", com.outfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "  hkfile='%s'", com.hkfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "  reference='%s'", com.reference_file);
	fits_write_history(fp, history, &istat);

	if ( 0 == com.reffile_none ) {
		sprintf(history, "  orbit='%s'", com.orbit_file);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  rigidity='%s'", com.rigidity);
		fits_write_history(fp, history, &istat);
	}

	if ( com.leapfile == com.o_leapfile ) {
		sprintf(history, "  leapfile='%s'", com.leapfile);
	} else {
		sprintf(history, "  leapfile='%s' (%s)", com.leapfile, com.o_leapfile);
	}
	fits_write_history(fp, history, &istat);

	if ( 0 == com.reffile_none ) {
		sprintf(history, "  time_col_name='%s'", com.time_col_name);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  pinud_cps_lo=%.1f  pinud_cps_hi=%.1f",
			com.pinud_cps_lo, com.pinud_cps_hi);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  pinud_period_lo=%.1f  pinud_cps_hi=%.1f",
			com.pinud_period_lo, com.pinud_period_hi);
		fits_write_history(fp, history, &istat);
	}

	sprintf(history, "  clobber=%s", com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
	}

	*status = ANL_OK;
}

void
aemkpinudhk_ana(int *nevent, int *eventid, int *status)
{
	double aetime, mjd;
	int anynul, ic;
	double cor, cor2;
	PINUD_CPS_1ROW o;
	AtVect vSat, vSatG;
	AtPolarVect pvSatG;

	int istat = 0;

/* check if finished */
	if ( com.nrow < com.irow ) {
		*status = ANL_QUIT;
		return;
	}

/* read TIME column from reffile */
	fits_read_col_dbl(com.reffile, com.time_col_num, com.irow, 1, 1, 0.0,
					  &aetime, &anynul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col('%s') failed at irow=%ld in '%s'\n",
			pname, com.time_col_name, com.irow, com.reference_file);
		*status = ANL_ERROR;
		return;
	}

	com.irow++;

/* PINUD */
	o.aetime = aetime;
	o.t0 = aetime - 16;
	o.t1 = aetime + 16;
	com.nbad += get_pinud_cps(&com.pinud, &o);

/* orbit */
	istat = aste_orbit(com.orbit, aetime, vSat, NULL);

/* COR */
	mjd = aste2mjd(aetime);
	atGeodetic(mjd, vSat, vSatG);
	atVectToPol(vSatG, &pvSatG);
	atRigidityD(&pvSatG, &cor);

/* COR2 */
	atRigidity2(com.rdp, &pvSatG, &cor2);

	ic = 0;
	if (
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &o.aetime, &istat) ||
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &o.pinud_cps, &istat) ||
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &o.pinud_err, &istat) ||
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &o.pinud_exp, &istat) ||
fits_write_col_int(com.ofp, ++ic, com.orow, 1, 1, &o.pinud_cts, &istat) ||
fits_write_col_int(com.ofp, ++ic, com.orow, 1, 1, &o.pinud_bad, &istat) ||
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &cor, &istat) ||
fits_write_col_dbl(com.ofp, ++ic, com.orow, 1, 1, &cor2, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_col('%s') failed at irow=%ld (%d)\n",
			pname, pinud_hk_tbl[ic-1].ttype, com.orow, istat);
		*status = ANL_ERROR;
		return;
	}
	com.orow++;

	*status = ANL_OK;
}

void
aemkpinudhk_endrun(int *status)
{
	*status = ANL_OK;
}

void
aemkpinudhk_exit(int *status)
{
	int istat = 0;
	fitsfile *fp = com.ofp;

	if ( fp ) {

		if ( com.reffile_none ) {
			istat = write_cts_pinud(fp, &com.pinud);
			if ( istat ) goto quit;
		}

/*		stdkeys.tstart = com.t0;
		stdkeys.tstop = com.t1;

		istat = aeUpdateStdTimeKeys(fp, &stdkeys);
		if ( istat ) goto quit;
*/

		if ( fits_write_date(fp, &istat) ) {
			anl_msg_error("\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
			goto quit;
		}

		if ( fits_write_chksum(fp, &istat) ) {
			anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
			goto quit;
		}

		fits_close_file(fp, &istat);
		anl_msg_info("\
%s: '%s' created, nrows=%ld, nbad=%d\n",
			pname, com.outfile, com.orow-1, com.nbad);
	}

	if ( NULL != com.reffile ) {
		fits_close_file(com.reffile, &istat);
		com.reffile = NULL;
	}

	if ( NULL != com.orbit ) {
		aste_orbit_free(com.orbit);
	}

	if ( NULL != com.rdp ) {
		atRigFree2(com.rdp);
	}

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}
