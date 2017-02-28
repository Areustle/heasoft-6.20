/*
	xisucode.c

# Version 1 2005-10-29 by Ken Ebisawa (ebisawa@isas.jaxa.jp)
# We decided to split xissubmode.pl into two scripts.
# (a) Divide TFF into FFF according to the microcode value in the HK file
#    ("S?_CPH_CODE_ID" HK parameter). Microcode list file is NOT used,
#     hence the script does not know the meaning of the microcode.
#    Only the 'CODE_ID' keyword is added for the microcode value
# (b) Reading the latest microcode list file (ae_xis_ucodelst_YYYYMMDD.fits)
#     and the 'CODE_ID' value in the header, corresponding sub-mode
#     is looked for, and the submodes keywords (WINOPT, WIN_ST etc) are added to the header.
# The new xissubmode.pl will be the script (a).  The script (b)
# will be xisucode.pl.
# xissubmode.pl is used together with mkxis1stfits, and run only at ISAS.
# (As a backup, sent to GSFC too).  xisucode.pl will be a part of the
# Critical FTOOLS, and run in the pipe-line processing at GSFC and ISAS.
# Users may run xisucode.pl by themselves when new microcode list is released.
#
# Version 1.1 2005-11-01 by Y.ISHISAKI (ishisaki@phys.metro-u.ac.jp)
# (1) change program name "xisucode.pl" -> "xisucode"
# (1) move help messages into usage()
# (2) add run() for print "$command"; system "$command";
# (3) change arguments order "xiucode ucode-file file1 [file2 ...]"
#
# Version 1.2 2005-12-18 by Y.ISHISAKI (ishisaki@phys.metro-u.ac.jp)
# bug fix in handling EXPOSURE extension
#
# Version 1.3 2006-07-26 by M.OZAKI (ozaki@astro.isas.jaxa.jp)
# (1) taking care of TIMEDEL keyword.
# (2) code cleanup: expand each tab to 4 whitespaces.
# (3) run() now accepts an array instead of a scalar.
#
# Version 1.4 2006-07-26 by Y.ISHISAKI (ishisaki@phys.metro-u.ac.jp)
# (1) bug fix of sprintf() sentence for SNAPTIn comments.
# (2) set TIMEDEL=7.8125e-3 if SNAPTIME=0.0 (PSUM mode, etc)
#
# Version 1.5 2006-07-27 by H.MATSUMOTO (matumoto@cr.scphys.kyoto-u.ac.jp)
# (1) bug fix in the pettern matching to detect DELAY[2-16]
#
# Version 1.6 2007-01-11 by Y.ISHISAKI (ishisaki@phys.metro-u.ac.jp)
# (1) support ucodefile = "CALDB"
# (2) copy comments of WINOPT, WIN_ST, WIN_SIZ, PSUM_L, CI, BINNING, SRAM_VER
#
# Version 1.7 2007-01-31 by Y.ISHISAKI (ishisaki@phys.metro-u.ac.jp)
# (1) run fchecksum after changing header keyword
#
# Version 2.0 2007-04-12 Y.ISHISAKI
# (1) rewrite all code in C.
# (2) write SCI keywords,
#     SCISTATY, SCISPEED, SCIN, SCIYn, AP4N, AP4Yn, AP256N, AP256Yn
# (3) read CODE_ID from primary header.
#
# Version 2.1 2007-05-03 Y.ISHISAKI
# (1) increase AP4Y, AP256Y size 16 -> 32
# (2) bug fix in finding CODE_ID for SCI extension
# (3) bug fix in writing WIN_ST
#
# Version 2.2 2007-05-14 Y.ISHISAKI
# (1) just move to secondary extension for UCODE_LIST_SCI
# (2) return error status on exit
# (3) ignore TGT_SENSOR if CODE_ID != 0
#
# Version 2.3 2007-05-30 Y.ISHISAKI
# (1) always conduct fits_update_key('CODE_ID') in process_hdu()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "cli.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

#define TASKNAME	"xisucode"
#define VERSION		"ver.2.3"
#define	DATE		"2007-05-30"
#define	CREDIT		TASKNAME " " VERSION " [" DATE "]\nby Ken Ebisawa, M.OZAKI (ISAS), Y.ISHISAKI (TMU), H.MATSUMOTO (Kyoto Univ.)"
static char pname[] = TASKNAME;
static char version[] = VERSION;
static char credit[] = CREDIT;

static long nrow1 = 0;

static char extname1[] = "UCODE_LIST";

static char *n1[] = {
	"CODE_ID",
	"TGT_SENSOR",
	"CLKMOD",
	"WINOPT",
	"WIN_ST",
	"WIN_SIZ",
	"PSUM_L",
	"CI",
	"BINNING",
	"SRAM_VER",
	"SNAPTIME",
	"DELAY"
};

static struct ucode_column {
	int CODE_ID;
	int TGT_SENSOR;
	int CLKMOD;
	int WINOPT;
	int WIN_ST;
	int WIN_SIZ;
	int PSUM_L;
	int CI;
	int BINNING;
	int SRAM_VER;
	int SNAPTIME;
	int DELAY;
} c1;

static struct ucode_value {
	short CODE_ID;
	short TGT_SENSOR;
	short CLKMOD;
	short WINOPT;
	short WIN_ST;
	short WIN_SIZ;
	short PSUM_L;
	short CI;
	short BINNING;
	char SRAM_VER[FLEN_VALUE];
	double SNAPTIME[16];
	double DELAY[16];
} *v1;

static struct ucode_comment {
	char CODE_ID[FLEN_COMMENT];
	char TGT_SENSOR[FLEN_COMMENT];
	char CLKMOD[FLEN_COMMENT];
	char WINOPT[FLEN_COMMENT];
	char WIN_ST[FLEN_COMMENT];
	char WIN_SIZ[FLEN_COMMENT];
	char PSUM_L[FLEN_COMMENT];
	char CI[FLEN_COMMENT];
	char BINNING[FLEN_COMMENT];
	char SRAM_VER[FLEN_COMMENT];
	char SNAPTIME[FLEN_COMMENT];
	char DELAY[FLEN_COMMENT];
} cm1;


static long nrow2 = 0;

static char extname2[] = "UCODE_LIST_SCI";

static char *n2[] = {
	"CODE_ID",
	"SCI_PERIOD_RAWY",
	"SCI_START_RAWY",
	"SCI_ISPEED",
	"SCI_NROW",
	"SCI_RAWY",
	"SCI_AP4_NROW",
	"SCI_AP4_RAWY",
	"SCI_AP256_NROW",
	"SCI_AP256_RAWY"
};

static struct sci_column {
	int CODE_ID;
	int SCIPERIY;
	int SCISTATY;
	int SCISPEED;
	int SCIN;
	int SCIY;
	int AP4N;
	int AP4Y;
	int AP256N;
	int AP256Y;
} c2;

static struct sci_value {
	short CODE_ID;
	short SCIPERIY;
	short SCISTATY;
	short SCISPEED;
	short SCIN;
	short SCIY[32];
	short AP4N;
	short AP4Y[32];
	short AP256N;
	short AP256Y[32];
} *v2;

static struct sci_comment {
	char CODE_ID[FLEN_COMMENT];
	char SCIPERIY[FLEN_COMMENT];
	char SCISTATY[FLEN_COMMENT];
	char SCISPEED[FLEN_COMMENT];
	char SCIN[FLEN_COMMENT];
	char SCIY[FLEN_COMMENT];
	char AP4N[FLEN_COMMENT];
	char AP4Y[FLEN_COMMENT];
	char AP256N[FLEN_COMMENT];
	char AP256Y[FLEN_COMMENT];
} cm2;

char *
column_name(int colnum, void *clist, char **namelist)
{
	int *ip = clist;

	while ( colnum != *ip ) {
		namelist++;
	}

	return *namelist;
}

int
read_ucodefile(char *ucodefile)
{
	char *k, *p, *ss[1], key[FLEN_KEYWORD], ttype[FLEN_VALUE];
	int i, j, a, hdutype;
	fitsfile *fp;

	int istat = 0;

/* open ucodefile */
	fits_open_file(&fp, ucodefile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, ucodefile, istat);
		return istat;
	}

/* read "UCODE_LIST" extension */
	fits_movnam_hdu(fp, BINARY_TBL, extname1, 0, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname1, istat);
		return istat;
	}

	fits_read_key_lng(fp, k="NAXIS2", &nrow1, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	i = 0;
	if (
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.CODE_ID,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.TGT_SENSOR, &istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.CLKMOD,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.WINOPT,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.WIN_ST,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.WIN_SIZ,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.PSUM_L,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.CI,			&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.BINNING,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.SRAM_VER,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.SNAPTIME,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n1[i++], &c1.DELAY,		&istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	v1 = malloc(sizeof(*v1) * nrow1);
	if ( NULL == v1 ) {
		anl_msg_error("\
%s: v1 = malloc(%ld) failed\n", pname, nrow1);
		return -1;
	}

	for (i = 0; i < nrow1; i++) {
		ss[0] = v1[i].SRAM_VER;
		if (
fits_read_col_sht(fp,j=c1.CODE_ID,   i+1,1, 1,0,&v1[i].CODE_ID,   &a,&istat)||
fits_read_col_sht(fp,j=c1.TGT_SENSOR,i+1,1, 1,0,&v1[i].TGT_SENSOR,&a,&istat)||
fits_read_col_sht(fp,j=c1.CLKMOD,    i+1,1, 1,0,&v1[i].CLKMOD,    &a,&istat)||
fits_read_col_sht(fp,j=c1.WINOPT,    i+1,1, 1,0,&v1[i].WINOPT,    &a,&istat)||
fits_read_col_sht(fp,j=c1.WIN_ST,    i+1,1, 1,0,&v1[i].WIN_ST,    &a,&istat)||
fits_read_col_sht(fp,j=c1.WIN_SIZ,   i+1,1, 1,0,&v1[i].WIN_SIZ,   &a,&istat)||
fits_read_col_sht(fp,j=c1.PSUM_L,    i+1,1, 1,0,&v1[i].PSUM_L,    &a,&istat)||
fits_read_col_sht(fp,j=c1.CI,        i+1,1, 1,0,&v1[i].CI,        &a,&istat)||
fits_read_col_sht(fp,j=c1.BINNING,   i+1,1, 1,0,&v1[i].BINNING,   &a,&istat)||
fits_read_col_str(fp,j=c1.SRAM_VER,  i+1,1, 1,"",ss,              &a,&istat)||
fits_read_col_dbl(fp,j=c1.SNAPTIME,  i+1,1,16,0, v1[i].SNAPTIME,  &a,&istat)||
fits_read_col_dbl(fp,j=c1.DELAY,     i+1,1,16,0, v1[i].DELAY,     &a,&istat)||
		 0 ) {
			anl_msg_error("\
%s: fits_read_col('%s') failed (%d)\n", pname, column_name(j, &c1, n1), istat);
			return istat;
		}
	}

	anl_msg_info("\n\
  [%s]\n", extname1);
	for (i = 0; i < sizeof(c1)/sizeof(int); i++) {
		sprintf(k=key, "TTYPE%d", ((int *)&c1)[i]);
		p = (char *)&cm1+i*FLEN_COMMENT;
		fits_read_key_str(fp, key, ttype, p, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
			return istat;
		}
		anl_msg_info("\
%16s / %s\n", n1[i], (char *)&cm1+i*FLEN_COMMENT);
	}

/* read "UCODE_LIST_SCI" extension */
	fits_movrel_hdu(fp, 1, &hdutype, &istat);
	if ( istat ) {
		istat = 0;
		goto skip;
	}

	fits_read_key_lng(fp, k="NAXIS2", &nrow2, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	i = 0;
	if (
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.CODE_ID,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.SCIPERIY,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++],	&c2.SCISTATY,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++],	&c2.SCISPEED,	&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.SCIN, 		&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.SCIY,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.AP4N,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.AP4Y,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.AP256N,		&istat) ||
fits_get_colnum(fp, CASESEN, k=n2[i++], &c2.AP256Y,		&istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	v2 = malloc(sizeof(*v2) * nrow2);
	if ( NULL == v2 ) {
		anl_msg_error("\
%s: v2 = malloc(%ld) failed\n", pname, nrow2);
		return -1;
	}

	for (i = 0; i < nrow2; i++) {
		ss[0] = v1[i].SRAM_VER;
		if (
fits_read_col_sht(fp,j=c2.CODE_ID,	i+1, 1, 1,0, &v2[i].CODE_ID,  &a, &istat)||
fits_read_col_sht(fp,j=c2.SCIPERIY,	i+1, 1, 1,0, &v2[i].SCIPERIY, &a, &istat)||
fits_read_col_sht(fp,j=c2.SCISTATY,	i+1, 1, 1,0, &v2[i].SCISTATY, &a, &istat)||
fits_read_col_sht(fp,j=c2.SCISPEED,	i+1, 1, 1,0, &v2[i].SCISPEED, &a, &istat)||
fits_read_col_sht(fp,j=c2.SCIN,		i+1, 1, 1,0, &v2[i].SCIN,	  &a, &istat)||
fits_read_col_sht(fp,j=c2.SCIY,		i+1, 1,32,0,  v2[i].SCIY,     &a, &istat)||
fits_read_col_sht(fp,j=c2.AP4N,		i+1, 1, 1,0, &v2[i].AP4N,     &a, &istat)||
fits_read_col_sht(fp,j=c2.AP4Y,		i+1, 1,32,0,  v2[i].AP4Y,     &a, &istat)||
fits_read_col_sht(fp,j=c2.AP256N,	i+1, 1, 1,0, &v2[i].AP256N,   &a, &istat)||
fits_read_col_sht(fp,j=c2.AP256Y,	i+1, 1,32,0,  v2[i].AP256Y,   &a, &istat)||
		 0 ) {
			anl_msg_error("\
%s: fits_read_col('%s') failed (%d)\n", pname, column_name(j, &c2, n2), istat);
			return istat;
		}
	}

	anl_msg_info("\n\
  [%s]\n", extname2);
	for (i = 0; i < sizeof(c2)/sizeof(int); i++) {
		sprintf(k=key, "TTYPE%d", ((int *)&c2)[i]);
		p = (char *)&cm2+i*FLEN_COMMENT;
		fits_read_key_str(fp, key, ttype, p, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
			return istat;
		}
		anl_msg_info("\
%16s / %s\n", n2[i], (char *)&cm2+i*FLEN_COMMENT);
	}

 skip:

/* close ucodefile */
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, ucodefile, istat);
		return istat;
	}

	return istat;
}

int
process_hdu(fitsfile *fp, int i1, int i2, char *ucodefile)
{
	int iwin, num_win, iy;
	char *k, key[FLEN_KEYWORD], card[FLEN_CARD], cm[FLEN_COMMENT];
	double timedel;
	struct ucode_value *p1;
	struct sci_value *p2;

	int istat = 0;

	p1 = &v1[i1];

	if ( 0 == p1->WINOPT ) {
		num_win = 1;
	} else {
		num_win = (2 << p1->WINOPT);
	}

	fits_delete_key(fp, "WINOPT", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "WIN_ST", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "WIN_SIZ", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "PSUM_L", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "CI", &istat); istat = 0;		/* ignore error */
	fits_delete_key(fp, "BINNING", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "SRAM_VER", &istat); istat = 0;	/* ignore error */
	for (iwin = 0; iwin < 16; iwin++) {
		sprintf(key, "DELAY%d", iwin+1);
		fits_delete_key(fp, key, &istat); istat = 0;	/* ignore error */
		sprintf(key, "SNAPTI%d", iwin+1);
		fits_delete_key(fp, key, &istat); istat = 0;	/* ignore error */
	}
	fits_delete_key(fp, "SCIPERIY", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "SCISTATY", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "SCISPEED", &istat); istat = 0;	/* ignore error */
	fits_delete_key(fp, "SCIN", &istat); istat = 0;		/* ignore error */
	for (iwin = 0; iwin < 32; iwin++) {
		sprintf(key, "SCIY%d", iwin+1);
		fits_delete_key(fp, key, &istat); istat = 0;	/* ignore error */
	}
	fits_delete_key(fp, "AP4N", &istat); istat = 0;		/* ignore error */
	fits_delete_key(fp, "AP256N", &istat); istat = 0;	/* ignore error */
	for (iwin = 0; iwin < 32; iwin++) {
		sprintf(key, "AP4Y%d", iwin+1);
		fits_delete_key(fp, key, &istat); istat = 0;	/* ignore error */
		sprintf(key, "AP256Y%d", iwin+1);
		fits_delete_key(fp, key, &istat); istat = 0;	/* ignore error */
	}

fits_update_key_lng(fp, k="CODE_ID", p1->CODE_ID, cm1.CODE_ID, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to update keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_lng(fp, k="WINOPT", p1->WINOPT, cm1.WINOPT, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_lng(fp, k="WIN_ST", p1->WIN_ST, cm1.WIN_ST, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_lng(fp, k="WIN_SIZ", p1->WIN_SIZ, cm1.WIN_SIZ, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

	timedel = 999;

	for (iwin = 0; iwin < num_win; iwin++) {

		sprintf(key, "SNAPTI%d", iwin+1);
		sprintf(cm, "Exposure of Window %d in Normal and Burst clock", iwin+1);
		fits_insert_key_fixdbl(fp, k=key, p1->SNAPTIME[iwin], 6, cm, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

		sprintf(key, "DELAY%d", iwin+1);
		sprintf(cm, "Time lag of Window %d start", iwin+1);
		fits_insert_key_fixdbl(fp, k=key, p1->DELAY[iwin], 6, cm, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

		if ( p1->SNAPTIME[iwin] < timedel ) {
			timedel = p1->SNAPTIME[iwin];
		}
		if ( 0.0 == timedel ) {
			timedel = 7.8125e-3;
		}

	}

fits_insert_key_lng(fp, k="PSUM_L", p1->PSUM_L, cm1.PSUM_L, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_lng(fp, k="CI", p1->CI, cm1.CI, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_lng(fp, k="BINNING", p1->BINNING, cm1.BINNING, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

fits_insert_key_str(fp, k="SRAM_VER", p1->SRAM_VER, cm1.SRAM_VER, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
		return istat;
	}

	if ( -1 != i2 ) {
		p2 = &v2[i2];

fits_insert_key_lng(fp, k="SCIPERIY", p2->SCIPERIY, cm2.SCIPERIY, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

fits_insert_key_lng(fp, k="SCISTATY", p2->SCISTATY, cm2.SCISTATY, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

fits_insert_key_lng(fp, k="SCISPEED", p2->SCISPEED, cm2.SCISPEED, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

fits_insert_key_lng(fp, k="SCIN", p2->SCIN, cm2.SCIN, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

		for (iy = 0; iy < p2->SCIN; iy++) {
			sprintf(key, "SCIY%d", iy+1);
fits_insert_key_lng(fp, k=key, p2->SCIY[iy], cm2.SCIY, &istat);
			if ( istat ) {
				anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
				return istat;
			}
		}

fits_insert_key_lng(fp, k="AP4N", p2->AP4N, cm2.AP4N, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

		for (iy = 0; iy < p2->AP4N; iy++) {
			sprintf(key, "AP4Y%d", iy+1);
fits_insert_key_lng(fp, k=key, p2->AP4Y[iy], cm2.AP4Y, &istat);
			if ( istat ) {
				anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
				return istat;
			}
		}

fits_insert_key_lng(fp, k="AP256N", p2->AP256N, cm2.AP256N, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
			return istat;
		}

		for (iy = 0; iy < p2->AP256N; iy++) {
			sprintf(key, "AP256Y%d", iy+1);
fits_insert_key_lng(fp, k=key, p2->AP256Y[iy], cm2.AP256Y, &istat);
			if ( istat ) {
				anl_msg_error("\
##### failed to insert keyword '%s' (%d).\n", k, istat);
				return istat;
			}
		}

	}

	if ( timedel < 999 ) {
		fits_update_key_fixdbl(fp, k="TIMEDEL", timedel, 7, NULL, &istat);
		if ( istat ) {
			anl_msg_error("\
##### failed to update keyword '%s' (%d).\n", k, istat);
			return istat;
		}
	}

	sprintf(card, "%s %s applied with %s", pname, version, ucodefile);
	fits_write_history(fp, card, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to write HISTORY (%d).\n", istat);
		return istat;
	}

	fits_update_chksum(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
##### failed to update keyword '%s' (%d).\n", "CHECKSUM", istat);
		return istat;
	}

	return 0;
}

int
process_fff(int num_fff, char **ffflist, char *ucodefile)
{
	short tgt_sensor, code_id, clkmod;
	int i, i1, i2, hdutype, sensor, found;
	char *k, *fff;
	char telescop[FLEN_VALUE], instrume[FLEN_VALUE], clk_mode[FLEN_VALUE];

	int istat = 0;
	int num_process = 0;
	int num_skip = 0;
	fitsfile *fp = NULL;

	anl_msg_info("\n");

	for (i = 0; i < num_fff; i++) {

		if ( NULL != fp ) {
			fits_close_file(fp, &istat);
			fp = NULL;
			if ( istat ) {
				anl_msg_error("\
##### failed to close file (%d).\n", istat);
				goto error;
			}
		}

		fff = ffflist[i];
		anl_msg_info("\
### Processing %s\n", fff);

		fits_open_file(&fp, fff, READWRITE, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### failed to open file (%d), skipping.\n", istat);
			num_skip++;
			continue;
		}

#if 0
/* exposure is either EXPOSURES (new files) or EXPOSURE (till Aug 2005). */
		fits_movnam_hdu(fp, BINARY_TBL, "EXPOSURES", 0, &istat);
		if ( istat ) {
			istat = 0;
			fits_movnam_hdu(fp, BINARY_TBL, "EXPOSURE", 0, &istat);
			if ( istat ) {
				istat = 0;
				anl_msg_warning("\
##### no EXPOSURES or EXPOSURE extension found, skipping.\n");
				num_skip++;
				continue;
			}
		}
#endif

#if 0
/* Get the microcode from the 1st extension. */
		fits_movabs_hdu(fp, 1, &hdutype, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### no 1st extension, skipping.\n");
			num_skip++;
			continue;
		}
#endif

/* Get the microcode from the primary extension. */
		fits_read_key_str(fp, k="TELESCOP", telescop, NULL, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### no '%s' keyword, skipping.\n", k);
			num_skip++;
			continue;
		}
		if ( ASTE_TELESCOP_ID != aste_telescop_id(telescop) ) {
			anl_msg_warning("\
##### TELESCOP='%s' is not supported, skipping.\n", telescop);
			num_skip++;
			continue;
		}

		fits_read_key_str(fp, k="INSTRUME", instrume, NULL, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### no '%s' keyword, skipping.\n", k);
			num_skip++;
			continue;
		}
		sensor = aste_instrume_id(instrume);
		if ( ASTE_XIS0_ID == sensor ) {
			tgt_sensor = 1;
		} else if ( ASTE_XIS1_ID == sensor ) {
			tgt_sensor = 2;
		} else if ( ASTE_XIS2_ID == sensor ) {
			tgt_sensor = 4;
		} else if ( ASTE_XIS3_ID == sensor ) {
			tgt_sensor = 8;
		} else {
			anl_msg_warning("\
##### INSTRUME='%s' is not supported, skipping.\n", instrume);
			num_skip++;
			continue;
		}

		fits_read_key(fp, TSHORT, k="CODE_ID", &code_id, NULL, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### no '%s' keyword, skipping.\n", k);
			num_skip++;
			continue;
		}

		fits_read_key_str(fp, k="CLK_MODE", clk_mode, NULL, &istat);
		if ( istat ) {
			istat = 0;
			anl_msg_warning("\
##### no '%s' keyword, skipping.\n", k);
			num_skip++;
			continue;
		}
		if ( 0 == CLstricmp(clk_mode, "normal") ) {
			clkmod = 0;
		} else if ( 0 == CLstricmp(clk_mode, "psum") ) {
			clkmod = 1;
		} else if ( 0 == CLstricmp(clk_mode, "burst") ) {
			clkmod = 2;
		} else {
			anl_msg_warning("\
##### CLK_MODE='%s' is not supported, skipping.\n", clk_mode);
			num_skip++;
			continue;
		}

		found = 0;
		for (i1 = 0; i1 < nrow1; i1++) {
			if ( code_id == v1[i1].CODE_ID && clkmod == v1[i1].CLKMOD ) {
				if ( 0 != code_id ) {	/* ignore TGT_SENSOR */
					found = 1;
					break;
				}
				if ( 0 != (tgt_sensor & v1[i1].TGT_SENSOR) ) {
					found = 1;
					break;
				}
			}
		}

		if ( 0 == found ) {
			anl_msg_error("\
##### Microcode %d is used in %s,\n\
##### but this is not found in the input microcode list.\n",
				code_id, aefits_basename(fff));
			goto error;
		}

		if ( v1[i1].CI < 2 ) {
			i2 = -1;		/* SCI not used */
		} else {
			if ( 0 == nrow2 ) {
				anl_msg_error("\
##### SCI is used in %s,\n\
##### but the SCI extension is not found in the input microcode list.\n",
				aefits_basename(fff));
				goto error;
			}
			found = 0;
			for (i2 = 0; i2 < nrow2; i2++) {
				if ( code_id == v2[i2].CODE_ID ) {
					found = 1;
					break;
				}
			}
			if ( 0 == found ) {
				anl_msg_error("\
##### Microcode %d is used in %s,\n\
##### but this is not found in the SCI extension of input microcode list.\n",
					code_id, aefits_basename(fff));
				goto error;
			}
		}

/* process all HDU */
		for (;;) {
			istat = process_hdu(fp, i1, i2, ucodefile);
			if ( istat ) {
				goto error;
			}
			fits_movrel_hdu(fp, 1, &hdutype, &istat);
			if ( istat ) {
				istat = 0;
				break;
			}
		}

		num_process++;
	}

	if ( NULL != fp ) {
		fits_close_file(fp, &istat);
		fp = NULL;
		if ( istat ) {
			anl_msg_error("\
##### failed to close file (%d).\n", istat);
			goto error;
		}
	}

	anl_msg_info("\
### Processed %d files, skipped %d files.\n", num_process, num_skip);

	return 0;

 error:
	if ( NULL != fp ) {
		istat = 0;
		fits_close_file(fp, &istat);
		fp = NULL;
		if ( istat ) {
			anl_msg_error("\
##### failed to close file.\n");
			goto error;
		}
	}

	anl_msg_error("\
### We are going to terminate the process.\n\
### Use the latest microcode file, and try again!\n");
	return -1;
}

void
usage(void)
{
	printf("\
\n\
%s\n\
\n\
Read \"CODE_ID\" keyword value of input FFF for the microcode,\n\
and obtain the corresponding minor-modes from the input microcode list\n\
which describe the XIS minor observation modes.\n\
Minor-mode parameters will be written in each header unit of the FFF.\n\
\n\
usage: xisucode CALDB FFF-file1 [FFF-file2 ...]\n\
\n\
The first argument is the microcode file from which minor-mode values\n\
WINOPT, WIN_ST, WIN_SIZ, PSUM_L, CI, BINNING, SRAM_VER, SNAPTIn, and\n\
DELAYn are obtained.  These values are written in each FFF header.\n\
The TIMEDEL value is also taken care of.\n\
\n\
If CI = 2 (SCI-54rows) or 3 (SCI-108rows), keywords of SCIPERIY,\n\
SCISTATY, SCISPEED, SCIN, SCIYn, AP4N, AP4Yn, AP256N, and AP256Yn\n\
are also written.\n\
\n\
", CREDIT);
}

int
xisucode_body(int argc, char **argv)
{
	int istat, num_fff;
	char *ucodefile, **ffflist;
	CALDB_INFO caldb;

	if ( argc < 2 ) {
		usage();
		return 0;
	}

	printf("%s\n", credit);

	ucodefile = argv[1];
	if ( 0 == CLstricmp("CALDB", ucodefile) ) {
		aste_caldb_init(&caldb);
		caldb.instrume = "XIS";
		caldb.codename = "UCODE_LIST";
		aste_caldb_get(&caldb);
		if ( 0 != caldb.status || 0 == caldb.nfound ) {
			anl_msg_error("\
%s-%s: no CALDB entry for '%s' (status=%d)\n",
				pname, version, caldb.codename, caldb.status);
			return 1;
		}
		ucodefile = caldb.filename;
		anl_msg_info("\n\
ucodefile='%s' (CALDB)\n", ucodefile);
	} else {
		anl_msg_info("\n\
ucodefile='%s'\n", ucodefile);
	}

	istat = read_ucodefile(ucodefile);
	if ( istat ) {
		return istat;
	}

	num_fff = argc - 2;
	ffflist = &argv[2];
	istat = process_fff(num_fff, ffflist, aefits_basename(ucodefile));

	return istat;
}
