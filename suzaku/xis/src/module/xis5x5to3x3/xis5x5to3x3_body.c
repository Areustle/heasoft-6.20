/*
  xis5x5to3x3_body.c

	2007/09/30 Y.ISHISAKI	version 1.0
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "xisTelemFormat.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "xis5x5to3x3";
char xis5x5to3x3_version[] = "version 1.0";

static struct {
/* xis5x5to3x3 */
	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];
	char hkfile[PIL_LINESIZE];
	int spthot[XIStotalXISno][XIStotalSegNo];
	int clobber;
} com;

static int
read_hkfile(int sensor, char *hkfile, int spthot[XIStotalSegNo])
{
	static int hdutype = BINARY_TBL;
	static char *extname = "XIS_EXPCND";
	static int extver = 0;

	fitsfile *fp;
	int iseg, icol, anul;
	char *k, instrume[FLEN_VALUE], key[FLEN_VALUE];

	int istat = 0;

	if ( 0 == CLstricmp("none", hkfile) ) {
		return 0;
	}

	anl_msg_info("\
%s: reading hkfile '%s'\n", pname, hkfile);

/* open hk file */
	fits_open_file(&fp, hkfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, hkfile, istat);
		goto quit;
	}

/* move to 'XIS_EXPCND' extension input file */
	fits_movnam_hdu(fp, hdutype, extname, extver, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
		goto quit;
	}

/* check 'INSTRUME' keyword */
	fits_read_key_str(fp, k="INSTRUME", instrume, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	if ( sensor != aste_instrume_id(instrume) - ASTE_XIS0_SENSOR ) {
		anl_msg_error("\
%s: INSTRUME=`%s' mismatch\n", pname, instrume);
		istat = -1;
		goto quit;
	}

/* read Sn_SPTHOT_[A-D] column */
	for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
		sprintf(key, "S%d_SPTHOT_%c", sensor, 'A' + iseg);
		fits_get_colnum(fp, CASESEN, key, &icol, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
		fits_read_col_int(fp, icol, 1, 1, 1, 0, &spthot[iseg], &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
	}

/* close input file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, hkfile, istat);
		goto quit;
	}

 quit:
	return istat;
}

static int
write_history(fitsfile *ofp, int sensor)
{
	char history[PIL_LINESIZE+80];
	int istat = 0;

	istat = aefits_write_module_history(ofp, pname);
	if ( istat ) {
		return istat;
	}

	sprintf(history, "  infile='%s'", com.infile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "  outfile='%s'", com.outfile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "  hkfile='%s'", com.hkfile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  s%d_spthot_a=%d\
  s%d_spthot_b=%d\
  s%d_spthot_c=%d\
  s%d_spthot_d=%d",
		sensor, com.spthot[sensor][0],
		sensor, com.spthot[sensor][1],
		sensor, com.spthot[sensor][2],
		sensor, com.spthot[sensor][3]);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "  clobber='%s'", com.clobber ? "yes" : "no");
	fits_write_history(ofp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
	}

	return istat;
}

static int
fix_keypos(fitsfile *ofp, int colnum, char *tunit, long tlmin, long tlmax)
{
	char key[FLEN_VALUE], comment[FLEN_COMMENT];
	char card[FLEN_CARD], ttype_card[FLEN_CARD], tform_card[FLEN_CARD];

	int istat = 0;

	sprintf(key, "TTYPE%d", colnum);
	fits_read_card(ofp, key, ttype_card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}
	fits_delete_key(ofp, key, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_delete_key('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}

	sprintf(key, "TFORM%d", colnum);
	fits_read_card(ofp, key, tform_card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}
	fits_delete_key(ofp, key, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_delete_key('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}

	sprintf(key, "TUNIT%d", colnum-1);
	fits_read_card(ofp, key, card, &istat);
	if ( istat ) {
		istat = 0;
		sprintf(key, "TFORM%d", colnum-1);
		fits_read_card(ofp, key, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
	}
	fits_insert_card(ofp, ttype_card, &istat);
	fits_insert_card(ofp, tform_card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_card() failed (%d)\n", pname, istat);
		goto quit;
	}
	if ( NULL != tunit ) {
		sprintf(key, "TUNIT%d", colnum);
		sprintf(comment, "physical unit of field");
		fits_insert_key_str(ofp, key, tunit, comment, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_insert_key('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
	}

	if ( tlmin != tlmax ) {
		sprintf(key, "TLMAX%d", colnum-1);
		fits_read_card(ofp, key, card, &istat);
		istat = 0;	/* ignore error */
		sprintf(key, "TLMIN%d", colnum);
		fits_insert_key_lng(ofp, key, tlmin, "Minimum range", &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_insert_key('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
		sprintf(key, "TLMAX%d", colnum);
		fits_insert_key_lng(ofp, key, tlmax, "Maximum range", &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_insert_key('%s') failed (%d)\n", pname, key, istat);
			goto quit;
		}
	}

 quit:
	return istat;
}

static int
copy_phas(fitsfile *ifp, fitsfile *ofp,
	int col_seg, int col_phas, int col_grade, long nrow, int spthot[4])
{
	long irow;
	int anul, ipix;
	short iseg, phas[25], p_omost, sum_omost;

	int istat = 0;

	for (irow = 1; irow <= nrow; irow++) {
		fits_read_col_sht(ifp, col_seg, irow, 1, 1, 0, &iseg, &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('SEGMENT') failed (%d)\n", pname, istat);
			goto quit;
		}
		fits_read_col_sht(ifp, col_phas, irow, 1, 25, 0, phas, &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('PHAS') failed (%d)\n", pname, istat);
			goto quit;
		}
		fits_write_col_sht(ofp, col_phas, irow, 1, 9, phas, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('PHAS') failed (%d)\n", pname, istat);
			goto quit;
		}
		p_omost = 0;
		sum_omost = 0;
		for (ipix = 0; ipix < 16; ipix++) {
			if ( spthot[iseg] < phas[9+ipix] ) {
				p_omost |= (1 << (15-ipix));
			} else {
				sum_omost += phas[9+ipix];
			}
		}
		fits_write_col_sht(ofp, col_grade+1, irow, 1, 1, &p_omost, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('P_OUTER_MOST') failed (%d)\n", pname, istat);
			goto quit;
		}
		fits_write_col_sht(ofp, col_grade+2, irow, 1, 1, &sum_omost, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('SUM_OUTER_MOST') failed (%d)\n", pname, istat);
			goto quit;
		}
	}

 quit:
	return istat;
}

static int
copy_events_ext(fitsfile *ifp, fitsfile *ofp)
{
	static char *extname = "EVENTS";
	char *k, key[FLEN_KEYWORD], instrume[FLEN_VALUE], tunit[FLEN_VALUE];
	int sensor, icol, ocol, ncols, col_seg, col_phas, col_grade;
	long tlmin, tlmax, nrow;

	int istat = 0;

	fits_get_num_rows(ifp, &nrow, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_read_key_str(ifp, k="INSTRUME", instrume, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	sensor = aste_instrume_id(instrume) - ASTE_XIS0_SENSOR;
	if ( sensor < 0 || XIStotalXISno <= sensor ) {
		anl_msg_error("\
%s: invalud INSTRUME='%s'\n", pname, instrume);
		istat = -1;
		goto quit;
	}

	anl_msg_info("\
   INSTRUME='%s'  nrow=%ld\n", instrume, nrow);

	istat = read_hkfile(sensor, com.hkfile, com.spthot[sensor]);
	if ( istat ) goto quit;

	anl_msg_info("\
   using S%d_SPTHOT_A/B/C/D=%d/%d/%d/%d\n",
		sensor,
		com.spthot[sensor][0], com.spthot[sensor][1],
		com.spthot[sensor][2], com.spthot[sensor][3]);

	fits_get_num_cols(ifp, &ncols, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_num_cols() failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_copy_header(ifp, ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_copy_header('%s') failed (%d)\n", pname, extname, istat);
		goto quit;
	}
	fits_flush_file(ofp, &istat);

/* insert P_OUTER_MOST, SUM_OUTER_MOST */
	fits_get_colnum(ifp, CASESEN, k="GRADE", &col_grade, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	fits_insert_col(ofp, col_grade+1, k="P_OUTER_MOST", "1I", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_col('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	istat = fix_keypos(ofp, col_grade+1, NULL, 0, 0);
	if ( istat ) goto quit;

	fits_insert_col(ofp, col_grade+2, k="SUM_OUTER_MOST", "1I", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_col('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	istat = fix_keypos(ofp, col_grade+2, NULL, 0, 0);
	if ( istat ) goto quit;

/* modify PHAS size */
	fits_get_colnum(ifp, CASESEN, k="SEGMENT", &col_seg, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	fits_get_colnum(ifp, CASESEN, k="PHAS", &col_phas, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	sprintf(key, "TUNIT%d", col_phas);
	fits_read_key_str(ifp, key, tunit, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}
	sprintf(key, "TLMIN%d", col_phas);
	fits_read_key_lng(ifp, key, &tlmin, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}
	sprintf(key, "TLMAX%d", col_phas);
	fits_read_key_lng(ifp, key, &tlmax, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, key, istat);
		goto quit;
	}

	fits_delete_col(ofp, col_phas, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_delete_col('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	fits_insert_col(ofp, col_phas, k, "9I", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_col('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	istat = fix_keypos(ofp, col_phas, tunit, tlmin, tlmax);
	if ( istat ) goto quit;

	istat = write_history(ofp, sensor);
	if ( istat ) goto quit;

	for (icol = 1; icol <= ncols; icol++) {
		if ( col_phas == icol ) {
			istat = copy_phas(ifp, ofp,
				col_seg, col_phas, col_grade, nrow, com.spthot[sensor]);
			if ( istat ) goto quit;
			continue;
		} else if ( icol <= col_grade ) {
			ocol = icol;
		} else {
			ocol = icol + 2;
		}
		fits_copy_col(ifp, ofp, icol, ocol, FALSE, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_copy_col(icol=%d, ocol=%d) failed (%d)\n", pname, icol, ocol, istat);
			goto quit;
		}
	}

 quit:
	return istat;
}

void
xis5x5to3x3_startup(int *status)
{
	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "INFILE", com.infile);
	printf("%20s   '%s'\n", "OUTFILE", com.outfile);
	printf("%20s   '%s'\n", "HKFILE", com.hkfile);
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
	printf("\n");
}

void
xis5x5to3x3_com(int *status)
{
#define NVAL	6
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"INFILE",
		"OUTFILE",
		"HKFILE",
		"CLOBBER",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"input 5x5 event file name",
		"output 3x3 event file name",
		"XIS HK file name to read SPTHOT",
		"overwrite output file if exists",
		"exit from this menu"
	};

	int idet, iseg;
	char *p, *k;
	char key[16];
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		if (
PILGetFname(k="infile", com.infile) ||
PILGetFname(k="outfile", com.outfile) ||
PILGetFname(k="hkfile", com.hkfile) ||
			 0 ) {
			goto pil_error;
		}

		if ( 0 == CLstricmp("none", com.hkfile) ) {
			for (idet = 0; idet < XIStotalXISno; idet++) {
				for (iseg = 0; iseg < XIStotalSegNo; iseg++) {
					sprintf(key, "s%d_spthot_%c", idet, 'a' + iseg);
					if (
PILGetInt(k=key, &com.spthot[idet][iseg]) ||
						 0 ) {
						goto pil_error;
					}
				}
			}
		}

		if (
PILGetBool (k="clobber", &com.clobber) ||
			 0 ) {
			goto pil_error;
		}

		*status = ANL_OK;;
		return;

	pil_error:
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
		} else if ( 0 == strcmp("INFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.infile));
		} else if ( 0 == strcmp("OUTFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("HKFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.hkfile));
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
xis5x5to3x3_init(int *status)
{
	char *k, extname[FLEN_VALUE], editmode[FLEN_VALUE];
	fitsfile *ifp, *ofp;
	int ihdu, hdunum, hdutype;

	int morekeys = 0;
	int istat = 0;

/* show current parameters */
	show_parameter();

/* open input file */
	fits_open_file(&ifp, com.infile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, com.infile, istat);
		goto quit;
	}

/* create output file */
	if ( com.clobber ) {
		unlink(com.outfile);
	}
	fits_create_file(&ofp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, com.outfile, istat);
		goto quit;
	}

/* copy hdu */
	fits_get_num_hdus(ifp, &hdunum, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_num_hdus() failed (%d)\n", pname, istat);
		goto quit;
	}

	anl_msg_info("\
%s: found %d HDU in %s\n", pname, hdunum, com.infile);
	for (ihdu = 1; ihdu <= hdunum; ihdu++) {
		fits_movabs_hdu(ifp, ihdu, &hdutype, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_movabs_hdu(hdu=%d) failed (%d)\n", pname, ihdu, istat);
			goto quit;
		}
		fits_read_key_str(ifp, k="EDITMODE", editmode, NULL, &istat);
		if ( istat ) {
			*editmode = '\0';
			istat = 0;	/* ignore error */
		} else if ( 0 != strcmp("5x5", editmode) ) {
			anl_msg_error("\
%s: %s = '%s' is not '5x5'\n", pname, k, editmode);
			istat = -1;
			goto quit;
		}
		if ( 1 == ihdu ) {
			strcpy(extname, "PRIMARY");
		} else {
			fits_read_key_str(ifp, k="EXTNAME", extname, NULL, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
				goto quit;
			}
		}
		anl_msg_info("\
%s: processing '%s' ...\n", pname, extname);
		if ( 0 == strcmp("EVENTS", extname) ) {
			istat = copy_events_ext(ifp, ofp);
			if ( istat ) goto quit;
		} else {
			fits_copy_hdu(ifp, ofp, morekeys, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_copy_hdu('%s') failed (%d)\n", pname, extname, istat);
				goto quit;
			}
		}
		if ( *editmode ) {
			fits_modify_key_str(ofp, k="EDITMODE", "3x3", NULL, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
				goto quit;
			}
		}
		fits_write_chksum(ofp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_update_chksum() failed (%d)\n", pname, istat);
			goto quit;
		}
	}

/* close input file */
	fits_close_file(ifp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.infile, istat);
		goto quit;
	}

/* close output file */
	fits_close_file(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.outfile, istat);
		goto quit;
	}

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
xis5x5to3x3_his(int *status)
{
	*status = ANL_OK;
}

void
xis5x5to3x3_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
xis5x5to3x3_ana(int *nevent, int *eventid, int *status)
{
	*status = ANL_QUIT;		/* do nothing in _ana() */
}

void
xis5x5to3x3_endrun(int *status)
{
	*status = ANL_OK;
}

void
xis5x5to3x3_exit(int *status)
{
	*status = ANL_OK;
}
