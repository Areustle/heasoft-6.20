/* $Id: aste_rpt.h,v 1.10 2007/05/07 16:45:31 ishisaki Exp $ */
/************************************************************************

	aste_rpt.h		RPT acess routine header

	int aste_rpt_open()			open RPT file
	int aste_rpt_read()			read RPT file
	int aste_rpt_close()		close RPT file

	2005-01-11	Y.ISHISAKI	version 1.0

	2005-02-23	Y.ISHISAKI	version 1.31
		change "char *ccsds_packet" -> "unsigned char *ccsds_packet"
		initialize casesen=TRUE, in aste_rpt_open()

	2005-05-27	Y.ISHISAKI	version 1.50
		modified for latest RPT, which have S_TIME, R_TIME, TI columns

************************************************************************/

#ifndef _ASTE_RPT_H_
#define _ASTE_RPT_H_

typedef struct {
	fitsfile *fp;
	long irow;
	long nrow;
	char *filename;
	int cpn_cnt;
	int *vcid_and_mask;
	int *vcid_xor_mask;
	int *apid_and_mask;
	int *apid_xor_mask;
	int col_s_time;
	int col_r_time;
	int col_ti;
	int col_vcid;
	int col_apid;
	int col_ccsds_packet;
} RPTFILE;

#ifdef __cplusplus
extern "C"
{
#endif

int aste_rpt_open(
	RPTFILE **rpt_ptr,
	char *filename,
	int cpn_cnt,
	int vcid_and_mask[/*cpn_cnt*/],
	int vcid_xor_mask[/*cpn_cnt*/],
	int apid_and_mask[/*cpn_cnt*/],
	int apid_xor_mask[/*cpn_cnt*/]
);

int aste_rpt_read(
	RPTFILE *rpt,
	int max_packet_size,
	double *aetime,
	double *recv_time,
	unsigned int eti[2],
	int *vcid,
	int *apid,
	unsigned char ccsds_packet[/*max_packet_size*/],
	int *packet_size_return
);

int aste_rpt_close(
	RPTFILE *rpt
);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_RPT_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
