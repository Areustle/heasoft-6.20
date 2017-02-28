/* $Id: aste_rpt.c,v 1.10 2007/05/07 16:45:32 ishisaki Exp $ */
/*******************************************************************

	aste_rpt.c		RPT acess routines

	int aste_rpt_open()			open RPT file
	int aste_rpt_read()			read RPT file
	int aste_rpt_close()		close RPT file

	2005-01-11	Y.ISHISAKI	version 1.0

	2005-02-23	Y.ISHISAKI	version 1.31
		change "char *ccsds_packet" -> "unsigned char *ccsds_packet"

	2005-05-27	Y.ISHISAKI	version 1.50
		modified for latest RPT, which have S_TIME, R_TIME, TI columns

************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "aste_rpt.h"

static char *pname = "aste_rpt";

/************************************************************************
int aste_rpt_open()			: open RPT file

Input:
	char *filename			: RPT file name to open
	int cpn_cnt				: number of and/xor masks
	int vcid_and_mask[]		: check VCID & APID as
	int vcid_xor_mask[]		: 	(vcid & vcid_and_mask) == vcid_xor_mask
	int apid_and_mask[]		:				&&
	int apid_xor_mask[]		:	(apid & apid_and_mask) == apid_xor_mask

Output:
	RPTFILE **rpt_ptr		: pointer to RPTFILE pointer

Return_Values:
	0						: success
	-1						: invalid file name
	others					: CFITSIO error
************************************************************************/
int
aste_rpt_open(
	RPTFILE **rpt_ptr,
	char *filename,
	int cpn_cnt,
	int vcid_and_mask[/*cpn_cnt*/],
	int vcid_xor_mask[/*cpn_cnt*/],
	int apid_and_mask[/*cpn_cnt*/],
	int apid_xor_mask[/*cpn_cnt*/]
)
{
	RPTFILE *rpt;
	fitsfile *fp;
	int i, len, istat, istat2;
	int casesen, hdunum, hdutype;

	*rpt_ptr = NULL;

	if ( NULL == filename ) {
		fprintf(stderr, "\
%s: open: filename == NULL\n", pname);
		return -1;
	}
	len = strlen(filename);
	if ( 0 == len ) {
		fprintf(stderr, "\
%s: open: filename == ''\n", pname);
		return -1;
	}
	rpt = malloc( sizeof(*rpt) + 4*cpn_cnt*sizeof(int) + (len+1) );
	if ( NULL == rpt ) {
		fprintf(stderr, "\
%s: open: malloc() failed\n", pname);
		return -1;
	}

	if ( 0 == cpn_cnt ) {
		rpt->vcid_and_mask = NULL;
		rpt->vcid_xor_mask = NULL;
		rpt->apid_and_mask = NULL;
		rpt->apid_xor_mask = NULL;
		rpt->filename = (char *)( rpt + 1 );
	} else {
		rpt->vcid_and_mask = (int *)( rpt + 1 );
		rpt->vcid_xor_mask = rpt->vcid_and_mask + cpn_cnt;
		rpt->apid_and_mask = rpt->vcid_xor_mask + cpn_cnt;
		rpt->apid_xor_mask = rpt->apid_and_mask + cpn_cnt;
		rpt->filename = (char *)( rpt->apid_xor_mask + cpn_cnt );
		for (i = 0; i < cpn_cnt; i++) {
			rpt->vcid_and_mask[i] = vcid_and_mask[i];
			rpt->vcid_xor_mask[i] = vcid_xor_mask[i];
			rpt->apid_and_mask[i] = apid_and_mask[i];
			rpt->apid_xor_mask[i] = apid_xor_mask[i];
		}
	}

	strcpy(rpt->filename, filename);
	rpt->irow = 0;
	rpt->cpn_cnt = cpn_cnt;

	istat = istat2 = 0;
	fits_open_file(&fp, filename, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_open_file('%s') failed (%d)\n", pname, filename, istat);
		free(rpt);
		return istat;
	}

	rpt->fp = fp;

	hdunum = 2;
	fits_movabs_hdu(rpt->fp, hdunum, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_movabs_hdu() failed (%d)\n", pname, istat);
		goto quit;
	} else if ( BINARY_TBL != hdutype ) {
		fprintf(stderr, "\
%s: open: hdutype is not BINARY_TBL (hdutype=%d)\n", pname, hdutype);
		goto quit;
	}

	fits_read_key_lng(fp, "NAXIS2", &rpt->nrow, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_read_key_lng() failed (%d)\n", pname, istat);
		goto quit;
	} else if ( rpt->nrow <= 0 ) {
		fprintf(stderr, "\
%s: open: invalid NAXIS2 = %ld\n", pname, rpt->nrow);
		goto quit;
	}

	casesen = TRUE;
	fits_get_colnum(fp, casesen, "S_TIME", &rpt->col_s_time, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('S_TIME') failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_colnum(fp, casesen, "R_TIME", &rpt->col_r_time, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('R_TIME') failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_colnum(fp, casesen, "TI", &rpt->col_ti, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('TI') failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_colnum(fp, casesen, "VCID", &rpt->col_vcid, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('VCID') failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_colnum(fp, casesen, "APID", &rpt->col_apid, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('APID') failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_colnum(fp, casesen, "CCSDS_PACKET", &rpt->col_ccsds_packet, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: open: fits_get_colnum('CCSDS_PACKET') failed (%d)\n", pname, istat);
		goto quit;
	}

	*rpt_ptr = rpt;
	return 0;

 quit:
	fits_close_file(fp, &istat2);
	free(rpt);
	return istat;
}

/************************************************************************
int aste_rpt_read()			: read RPT file

Input:
	RPTFILE *rpt			: RPTFILE pointer to read
	int max_packet_size		: size of ccsds_packet[]

Output:
	double *aetime			: TIME
	double *recv_time		: RECV_TIME (or TIME)
	unsigned int eti[2]		: ETI
	int *vcid_return		: VCID
	int *apid_return		: APID
	unsigned char ccsds[]	: CCSDS_PACKET
	int *packet_size_return	: actual size of CCSDS_PACKET

Return_Values:
	0						: success
	-1						: invalid RPTFILE pointer
	-2						: end of file
	others					: CFITSIO error
************************************************************************/
int
aste_rpt_read(
	RPTFILE *rpt,
	int max_packet_size,
	double *s_time,
	double *r_time,
	unsigned int *ti,
	int *vcid_return,
	int *apid_return,
	unsigned char ccsds_packet[/*max_packet_size*/],
	int *packet_size_return
)
{
	fitsfile *fp;
	long irow, pksize, offset;
	int istat, anul, apid, vcid, i, cpn_cnt;

	if ( NULL == rpt ) {
		fprintf(stderr, "\
%s: close: rpt == NULL\n", pname);
		return -1;
	}

	fp = rpt->fp;
	irow = rpt->irow;
	cpn_cnt = rpt->cpn_cnt;
	istat = 0;

 again:

	if ( rpt->nrow <= irow ) {
		return -2;
	}

	irow++;
	rpt->irow = irow;
fits_read_col_int(fp, rpt->col_vcid, irow,1,1, 0, &vcid, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_col_int('VCID') failed at irow=%ld\n", pname, irow);
		return istat;
	}
fits_read_col_int(fp, rpt->col_apid, irow,1,1, 0, &apid, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_col_int('APID') failed at irow=%ld\n", pname, irow);
		return istat;
	}

	if ( cpn_cnt ) {
		for (i = 0; i < cpn_cnt; i++) {
			if ( rpt->vcid_xor_mask[i] == (vcid & rpt->vcid_and_mask[i]) &&
				 rpt->apid_xor_mask[i] == (apid & rpt->apid_and_mask[i]) ) {
				goto ok;
			}
		}
		goto again;
	}

 ok:

fits_read_col_dbl(fp, rpt->col_s_time, irow,1,1, 0.0, s_time, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_col_dbl('S_TIME') failed at irow=%ld\n", pname, irow);
		return istat;
	}

fits_read_col_dbl(fp, rpt->col_r_time, irow,1,1, 0, r_time, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_col_dbl('R_TIME') failed at irow=%ld\n", pname, irow);
		return istat;
	}

fits_read_col_uint(fp, rpt->col_ti, irow,1,1, 0, ti, &anul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_col_dbl('TI') failed at irow=%ld\n", pname, irow);
		return istat;
	}

	if ( NULL != vcid_return ) {
		*vcid_return = vcid;
	}

	if ( NULL != apid_return ) {
		*apid_return = apid;
	}

fits_read_descript(fp, rpt->col_ccsds_packet, irow, &pksize, &offset, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: read: fits_read_descript('CCSDS_PACKET') failed at irow=%ld\n", pname, irow);
		return istat;
	}
	if ( NULL != packet_size_return ) {
		*packet_size_return = pksize;
	}

	if ( max_packet_size < pksize ) {
		pksize = max_packet_size;
	}
	if ( NULL != ccsds_packet && 0 < pksize ) {
fits_read_col_byt(fp, rpt->col_ccsds_packet, irow,1,pksize, 0, ccsds_packet, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: read: fits_read_col_byt('CCSDS_PACKET') failed at irow=%ld\n", pname, irow);
			return istat;
		}
	}

	return 0;
}

/************************************************************************
int aste_rpt_close()		: close RPT file

Input:
	RPTFILE *rpt			: RPTFILE pointer to close

Return_Values:
	0						: success
	-1						: invalid RPTFILE pointer
	others					: CFITSIO error
************************************************************************/
int
aste_rpt_close(
	RPTFILE *rpt
)
{
	int istat;

	if ( NULL == rpt ) {
		fprintf(stderr, "\
%s: close: rpt == NULL\n", pname);
		return -1;
	}
	istat = 0;
	fits_close_file(rpt->fp, &istat);
	free(rpt);
	return istat;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
