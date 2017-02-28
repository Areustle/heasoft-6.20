/*
  XRSeventFitsRDWT.c

	1999/08/06 Y.ISHISAKI	version 1.0

	1999/08/06 Y.ISHISAKI	version 1.1
		BNK definition of XRSeventFitsWT moved to XRSeventFitsRD_init()

	1999/12/23 Y.ISHISAKI	version 1.2
		size of ifilename & ofilename 256 -> 1024
		XRS:EVENT:FILE_NAME  -> XRS:EVENT:IFILE_NAME:PTR
		XRS:EVENT:OFILE_NAME -> XRS:EVENT:OFILE_NAME:PTR

	2003/09/30 Y.ISHISAKI	version 1.2-headas
		modifications for HEADAS

	2004/01/08 Y.ISHISAKI	version 1.3
		add FLAGS & FLAGS_STR
		add dataSave in struct table_data
		use fits_read_tblbytes & fits_write_tblbytes to copy each row
		update NAXIS2 when some rows are discarded
		[rewrite CHECKSUM & DATASUM after writing output file] -> comment out
		support overwrite mode for outfile=overwrite

	2004/07/26 Y.ISHISAKI	version 1.4
	    write task_name, version, processed date/time to HISTORY
		increase buffer size into (PIL_LINESIZE+20) in _bgnrun

	2004/12/07 R.FUJIMOTO	version 1.5
		changed keyword names PULSE_QUALITY -> MEAN_SQ_DIFF,
		FLAGS_STR -> TYPE, FLAG_CROSSTALK -> PIX_COINCIDENCE,
		PRE_INTERVAL -> PREV_INTERVAL
		added PACKET_TIME

	2005/02/16 Y.ISHISAKI	version 1.6
		remove EvsDef("XRS:FLAG_CROSSTALK")
		BnkDef("XRS:EVENT:OROW")
		BnkPut/Get "XRS:EVENT:IROW", "XRS:EVENT:OROW" each time in _ana()
		add newline before "event file '..' created" message

	2005/04/25 Y.ISHISAKI	version 1.7
		add UPI, EPI columns, rename ETI -> TI, PACKET_TIME -> RECV_TIME

	2005/04/30 Y.ISHISAKI	version 1.8
		add TIME_QUALITY column

	2005/05/03 Y.ISHISAKI	version 1.9
		rename all RECV_TIME -> S_TIME

	2005/07/05 Y.ISHISAKI	version 2.0
		read all infile/outfile/clobber parameters in XRSeventFitsRD_com()
		write both infile/outfile to FITS in XRSeventFitsRD_bgnrun()
		support for outfile="none"

	2005/07/27 Y.ISHISAKI	version 2.1
		consider '\0' termination in reading TYPE column
		initialize fitsWT_used in _startup & check it in show_parameter()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "pil.h"
#include "headas.h"

static char pname[] = "XRSeventFitsRDWT";
char XRSeventFitsRD_version[] = "version 2.1";
char XRSeventFitsWT_version[] = "version 2.1";

static struct {
/* XRSeventFitsRD */
	char ifilename[PIL_LINESIZE];
	fitsfile *ifp;
	long irow, nrow;
/* XRSeventFitsWT */
	int fitsWT_used;
	char ofilename[PIL_LINESIZE];
	fitsfile *ofp;
	long orow, tblwid;
	char *tblval;
	int clobber;
} com;

void
XRSeventFitsRD_startup(int *status)
{
	com.ifp = com.ofp = NULL;
	com.fitsWT_used = 0;
	strcpy(com.ofilename, "");

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "INFILE", com.ifilename);
	if ( com.fitsWT_used ) {
		printf("%20s   '%s'\n", "OUTFILE", com.ofilename);
	}
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
}

void
XRSeventFitsRD_com(int *status)
{
#define NVAL	3
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"INFILE",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"input event file name",
		"exit from this menu"
	};
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */

		if (
PILGetFname("infile",  com.ifilename)
			 ) {
			*status = ANL_QUIT;
			return;
		}

		if ( '\0' != com.ofilename[0] ) {	/* WT defined */
			if (
PILGetFname("outfile", com.ofilename) ||
PILGetBool ("clobber", &com.clobber) ||
				 0 ) {
				*status = ANL_QUIT;
				return;
			}
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
		} else if ( 0 == strcmp("INFILE", p) ) {
			CLtxtrd(p, com.ifilename, sizeof(com.ifilename));
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

static struct column_num {
	int aetime;
	int pixel, pha, upi, epi, pi, riseTime, rti;
	int detx, dety, focx, focy, x, y;
	int roll;
	int flag_type;
	int flags;
	int meanSqDiff, timeQuality;
	int secondary, midRes, lowRes, baseline, antiCo, clipped;
	int ccsds_header;
	int ti;
	int tickCounter, sampleCount, timeVernier;
	int parity, parityError, pixco;
	int preInterval;
	int prevPha;
	int s_time;
} columnNo;

static struct event_data {
	double aetime;
	int pixel, pha;
	double upi, epi;
	int pi, riseTime, rti;
	int detx, dety, focx, focy, x, y;
	double roll;
	char flag_type[2];
	int flags;
	int meanSqDiff, timeQuality;
	int secondary, midRes, lowRes, baseline, antiCo, clipped;
	unsigned char ccsds_header[6];
	unsigned int ti;
	int tickCounter, sampleCount, timeVernier;
	int parity, parityError, pixco;
	double preInterval;
	int prevPha;
	double s_time;
} eventData, eventDataSave;

#define TBLDATA(type,name,member)	{\
	type,name,\
	&columnNo.member,\
	&eventData.member,\
	&eventDataSave.member,\
	sizeof(eventData.member),\
	0 }

static struct table_data {
	int type;
	char *name;
	int *num;
	void *data;
	void *dataSave;
	int dataSize;
	int index;
} tbl[] = {
	TBLDATA(TDOUBLE,"TIME",aetime),
	TBLDATA(TINT,"PIXEL",pixel),
	TBLDATA(TINT,"PHA",pha),
	TBLDATA(TDOUBLE,"UPI",upi),
	TBLDATA(TDOUBLE,"EPI",epi),
	TBLDATA(TINT,"PI",pi),
	TBLDATA(TINT,"RISE_TIME",riseTime),
	TBLDATA(TINT,"RTI",rti),
	TBLDATA(TINT,"DETX",detx),
	TBLDATA(TINT,"DETY",dety),
	TBLDATA(TINT,"FOCX",focx),
	TBLDATA(TINT,"FOCY",focy),
	TBLDATA(TINT,"X",x),
	TBLDATA(TINT,"Y",y),
	TBLDATA(TDOUBLE,"ROLL",roll),
	TBLDATA(TINT,"MEAN_SQ_DIFF",meanSqDiff),
	TBLDATA(TINT,"TIME_QUALITY",timeQuality),
	TBLDATA(TSTRING,"TYPE",flag_type),
	TBLDATA(TINT,"FLAGS",flags),
	TBLDATA(TINT,"FLAG_SECONDARY",secondary),
	TBLDATA(TINT,"FLAG_MIDRES",midRes),
	TBLDATA(TINT,"FLAG_LOWRES",lowRes),
	TBLDATA(TINT,"FLAG_BASELINE",baseline),
	TBLDATA(TINT,"FLAG_ANTICO",antiCo),
	TBLDATA(TINT,"FLAG_CLIPPED",clipped),
	TBLDATA(TBYTE,"CCSDS_HEADER",ccsds_header),
	TBLDATA(TUINT,"TI",ti),
	TBLDATA(TINT,"TICK_COUNTER",tickCounter),
	TBLDATA(TINT,"SAMPLE_COUNT",sampleCount),
	TBLDATA(TINT,"TIME_VERNIER",timeVernier),
	TBLDATA(TINT,"PARITY",parity),
	TBLDATA(TINT,"FLAG_PARITY_ERROR",parityError),
	TBLDATA(TINT,"PIX_COINCIDENCE",pixco),
	TBLDATA(TDOUBLE,"PREV_INTERVAL",preInterval),
	TBLDATA(TINT,"PREV_PHA",prevPha),
	TBLDATA(TDOUBLE,"S_TIME",s_time),
	TBLDATA(TINT,NULL,pixel),
};

static int
open_event_file(char *fn)
{
	fitsfile *fp;
	int istat = 0;
	int hdunum = 2;
	int hdutype;
	int casesen = TRUE;
	int icol;
	char comment[80];

	fits_open_file(&fp, fn, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_open_file failed (%d)\n", pname, istat);
		return -1;
	}
	fits_movabs_hdu(fp, hdunum, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu failed (%d)\n", pname, istat);
		return -1;
	} else if ( BINARY_TBL != hdutype ) {
		fprintf(stderr, "\
%s: hdutype is not BINARY_TBL (%d)\n", pname, istat);
		return -1;
	}
	fits_read_key_lng(fp, "NAXIS2", &com.nrow, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		return -1;
	}
	for (icol = 0; NULL != tbl[icol].name; icol++) {
		fits_get_colnum(fp, casesen, tbl[icol].name, tbl[icol].num, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n\
%s: this column ignored.\n", pname, tbl[icol].name, istat, pname);
			*tbl[icol].num = -1;
			istat = 0;
		}
	}

	com.ifp = fp;
	com.irow = 0;
	return 0;
}

void
XRSeventFitsRD_init(int *status)
{
	int i;
	char *p;

/* BNK definition */
	for (i = 0; NULL != tbl[i].name; i++) {
		char bnkname[64];
		sprintf(bnkname, "XRS:%s", tbl[i].name);
		BnkDef(bnkname, tbl[i].dataSize);
	/* initialize index */
		BnkfPut(bnkname, &tbl[i].index, tbl[i].dataSize, tbl[i].data);
	}

	BnkDef("XRS:EVENT:IFILE_NAME:PTR", sizeof(char*));
	BnkDef("XRS:EVENT:IFP", sizeof(com.ifp));
	BnkDef("XRS:EVENT:IROW", sizeof(com.irow));

/* BNK definition for XRSeventFitsWT */
	BnkDef("XRS:EVENT:OFILE_NAME:PTR", sizeof(char*));
	BnkDef("XRS:EVENT:OFP", sizeof(com.ifp));
	BnkDef("XRS:EVENT:OROW", sizeof(com.orow));

/* EVS definition */
	EvsDef("XRS:FLAG_HIRES");
	EvsDef("XRS:FLAG_SECONDARY");
	EvsDef("XRS:FLAG_MIDRES");
	EvsDef("XRS:FLAG_LOWRES");
	EvsDef("XRS:FLAG_BASELINE");
	EvsDef("XRS:FLAG_ANTICO");
	EvsDef("XRS:FLAG_CLIPPED");
	EvsDef("XRS:FLAG_PARITY_ERROR");

	show_parameter();

	if ( open_event_file(com.ifilename) ) {
		*status = ANL_QUIT;
		return;
	}

	p = com.ifilename;
	BnkfPutM("XRS:EVENT:IFILE_NAME:PTR", sizeof(p), &p);
	BnkfPutM("XRS:EVENT:IFP", sizeof(com.ifp), &com.ifp);
	BnkfPutM("XRS:EVENT:IROW", sizeof(com.irow), &com.irow);

	*status = ANL_OK;
}

void
XRSeventFitsRD_his(int *status)
{
	*status = ANL_OK;
}

void
XRSeventFitsRD_bgnrun(int *status)
{
	fitsfile *fp;
	int used = 0;

	BnkGet("XRS:EVENT:OFP", sizeof(fp), &used, &fp);
	if ( used == sizeof(fp) && NULL != fp ) {
		int istat = 0;
		char buf[PIL_LINESIZE+20];
		char *task_name, *task_version;
		int timeref, len;
		char datestr[20], waku[80];

		task_name = anl_task_name();
		task_version = anl_task_version();
		fits_get_system_time(datestr, &timeref, &istat);
		sprintf(buf, " %s version %s at %s", task_name, task_version, datestr);
		len = strlen(buf) + 1;
		if ( 71 < len ) len = 71;
		memset(waku, '-', len);
		waku[len] = '\0';
		fits_write_history(fp, waku, &istat);
		fits_write_history(fp, buf, &istat);
		fits_write_history(fp, waku, &istat);

		sprintf(buf, "%s %s", pname, XRSeventFitsRD_version);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  infile='%.*s'", PIL_LINESIZE, com.ifilename);
		fits_write_history(fp, buf, &istat);
		if ( '\0' != com.ofilename[0] ) {	/* WT defined */
			sprintf(buf, "  outfile='%.*s'", PIL_LINESIZE, com.ofilename);
			fits_write_history(fp, buf, &istat);
		}
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}
	}

	*status = ANL_OK;
}

void
XRSeventFitsRD_ana(int *nevent, int *eventid, int *status)
{
	int i, used, istat, anynul;

	BnkfGetM("XRS:EVENT:IROW", sizeof(com.irow), &used, &com.irow);
	com.irow++;
	BnkfPutM("XRS:EVENT:IROW", sizeof(com.irow), &com.irow);
	if ( com.nrow < com.irow ) {
		*status = ANL_QUIT;
		return;
	}

/* read event fits and BNKPUT */
	istat = 0;
	for (i = 0; NULL != tbl[i].name; i++) {
		struct table_data *p = &tbl[i];
		void *data = p->data;
		int num = *p->num;

		if ( num < 0 ) {	/* non-existing column */
			continue;
		}

		if ( eventData.ccsds_header == data ) {
fits_read_col_byt(com.ifp, num, com.irow, 1, 6, 0, data, &anynul, &istat);
		} else if ( &eventData.ti == data ) {
fits_read_col_uint(com.ifp, num, com.irow, 1, 1, 0, data, &anynul, &istat);
		} else if ( &eventData.flag_type == data ) {
			char type[3];	/* need one more char space for '\0' */
			char *array[1];
			array[0] = type;
fits_read_col_str(com.ifp, num, com.irow, 1, 1, "", array, &anynul, &istat);
			eventData.flag_type[0] = type[0];
			eventData.flag_type[1] = type[1];
		} else if ( TINT == p->type ) {
fits_read_col_int(com.ifp, num, com.irow, 1, 1, 0, data, &anynul, &istat);
		} else if ( TDOUBLE == p->type ) {
fits_read_col_dbl(com.ifp, num, com.irow, 1, 1, 0.0, data, &anynul, &istat);
		}
		BnkfPut(NULL, &p->index, p->dataSize, data);
		memcpy(p->dataSave, data, p->dataSize);
	}

/* check cfitsio error */
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_col failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* set EVS */
	if ( 0 == eventData.secondary &&
		 0 == eventData.midRes &&
		 0 == eventData.lowRes &&
		 0 == eventData.baseline &&
		 0 == eventData.antiCo &&
		 0 == eventData.clipped &&
		 0 == eventData.parityError ) {
		EvsfSetM("XRS:FLAG_HIRES");
	}
	if ( eventData.secondary ) {
		EvsfSetM("XRS:FLAG_SECONDARY");
	}
	if ( eventData.midRes ) {
		EvsfSetM("XRS:FLAG_MIDRES");
	}
	if ( eventData.lowRes ) {
		EvsfSetM("XRS:FLAG_LOWRES");
	}
	if ( eventData.baseline ) {
		EvsfSetM("XRS:FLAG_BASELINE");
	}
	if ( eventData.antiCo ) {
		EvsfSetM("XRS:FLAG_ANTICO");
	}
	if ( eventData.clipped ) {
		EvsfSetM("XRS:FLAG_CLIPPED");
	}
	if ( eventData.parityError ) {
		EvsfSetM("XRS:FLAG_PARITY_ERROR");
	}

	*status = ANL_OK;
}

void
XRSeventFitsRD_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSeventFitsRD_exit(int *status)
{
	int used;

	BnkfGetM("XRS:EVENT:IROW", sizeof(com.irow), &used, &com.irow);
	BnkfGetM("XRS:EVENT:OROW", sizeof(com.orow), &used, &com.orow);

	if ( com.ofp ) {
		int istat = 0;

		if ( com.ifp == com.ofp ) {	/* overwrite mode */
			goto skip_copy_gti_close_file;
		}

/* update NAXIS2 for discarded rows */
		if ( com.orow < com.irow ) {
			fits_modify_key_lng(com.ofp, "NAXIS2", com.orow, NULL, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_modify_key_lng('NAXIS2') from %ld -> %ld (%d)\n",
						pname, com.irow, com.orow, istat);
			}

/* This does not work in cfitsio-2.430
			fits_delete_rows(com.ofp, com.orow+1, com.irow - com.orow, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_delete_rows failed from %ld -> %ld rows (%d)\n",
						pname, com.irow, com.orow, istat);
			}
*/

		}

/* update CHECKSUM & DATASUM for discarded rows */
/*		fits_write_chksum(com.ofp, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_chksum failed (%d)\n", pname, istat);
		}
*/

/* copy GTI extension */
		if ( com.ifp ) {
			int hdutype;
			fits_movabs_hdu(com.ifp, 3, &hdutype, &istat);
			fits_copy_hdu(com.ifp, com.ofp, 0, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_copy_hdu('GTI') failed (%d)\n", pname, istat);
			}
		}
	}

/* close input file */
	if ( com.ifp ) {
		int istat = 0;
		fits_close_file(com.ifp, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_close_file failed for infile (ignored)\n", pname);
		}
		com.ifp = NULL;
	}

 skip_copy_gti_close_file:

	*status = ANL_OK;
}



void
XRSeventFitsWT_startup(int *status)
{
	com.fitsWT_used = 1;
	strcpy(com.ofilename, "none");

	*status = ANL_OK;
}

void
XRSeventFitsWT_com(int *status)
{
#define NVAL	4
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"OUTFILE",
		"CLOBBER",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"output event file name",
		"overwrite output file if exists",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	if ( *status ) {	/* ftools */
		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("OUTFILE", p) ) {
			CLtxtrd(p, com.ofilename, sizeof(com.ofilename));
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
XRSeventFitsWT_init(int *status)
{
	char *p;
	int hdutype;
	int istat = 0;
	int morekeys = 0;

/* setup for outfile="none" */
	if ( 0 == CLstricmp("none", com.ofilename) ) {
		com.ofp = NULL;
		goto skip;
	}

/* setup for overwrite mode */
	if ( 0 == strcmp(com.ifilename, com.ofilename) ) {
		fflush(NULL); printf("\
%s: infile & outfile are same, using OVERWRITE mode\n", pname);
		strcpy(com.ofilename, "overwrite");
		fflush(NULL);
	}

	if ( 0 == CLstricmp(com.ofilename, "overwrite") ) {
		if ( fits_close_file(com.ifp, &istat) ) {
			fprintf(stderr, "\
%s: fits_close_file() failed for infile (%d)", pname, istat);
			*status = ANL_QUIT;
			return;
		}
		if ( fits_open_file(&com.ifp, com.ifilename, READWRITE, &istat) ) {
			fprintf(stderr, "\
%s: fits_open_file() failed in READWRITE mode (%d)", pname, istat);
			*status = ANL_QUIT;
			return;
		}
		if ( fits_movabs_hdu(com.ifp, 2, &hdutype, &istat) ) {
			fprintf(stderr, "\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}
		com.ofp = com.ifp;
		goto skip;
	}

/* setup for normal mode */
	if ( com.clobber ) {
		unlink(com.ofilename);
	}
	fits_create_file(&com.ofp, com.ofilename, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_file() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return ;
	}
	fits_movabs_hdu(com.ifp, 1, &hdutype, &istat);
	fits_copy_hdu(com.ifp, com.ofp, morekeys, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_copy_hdu() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return ;
	}
	fits_movabs_hdu(com.ifp, 2, &hdutype, &istat);
	fits_copy_header(com.ifp, com.ofp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_copy_header('EVENTS') failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return ;
	}

	fits_read_key_lng(com.ifp, "NAXIS1", &com.tblwid, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key_lng('NAXIS1') failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return ;
	}
	com.tblval = malloc(com.tblwid);
	if ( NULL == com.tblval ) {
		fprintf(stderr, "\
%s: malloc() failed for com.tblval (size=%ld)\n", pname, com.tblwid);
		*status = ANL_QUIT;
		return ;
	}

 skip:

	com.orow = 0;
	p = com.ofilename;
	BnkfPutM("XRS:EVENT:OFILE_NAME:PTR", sizeof(p), &p);
	BnkfPutM("XRS:EVENT:OFP", sizeof(com.ofp), &com.ofp);
	BnkfPutM("XRS:EVENT:OROW", sizeof(com.orow), &com.orow);

	*status = ANL_OK;
}

void
XRSeventFitsWT_his(int *status)
{
	*status = ANL_OK;
}

void
XRSeventFitsWT_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
XRSeventFitsWT_ana(int *nevent, int *eventid, int *status)
{
	int i, istat, used;

/* initialize CFITSIO return status */
	istat = 0;

/* increment output table row number */
	BnkfGetM("XRS:EVENT:OROW", sizeof(com.orow), &used, &com.orow);
	com.orow++;
	BnkfPutM("XRS:EVENT:OROW", sizeof(com.orow), &com.orow);

/* check for outfile="none" */
	if ( NULL == com.ofp ) {
		*status = ANL_OK;
		return;
	}

/* check for overwrite mode */
	if ( com.ifp == com.ofp ) {
		if ( com.orow != com.irow ) {
			fprintf(stderr, "\
%s: discarding row is not acceptable for overwrite mode\n", pname);
			*status = ANL_QUIT;
			return;
		}
		goto skip_copy_row;
	}

/* copy all table data of the current row */
	fits_read_tblbytes(com.ifp, com.irow, 1, com.tblwid, com.tblval, &istat);
	fits_write_tblbytes(com.ofp, com.orow, 1, com.tblwid, com.tblval, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read/write_tblbytes() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}
 skip_copy_row:

/* BNKGET and write event fits if changed */
	for (i = 0; NULL != tbl[i].name; i++) {
		struct table_data *p = &tbl[i];
		void *data = p->data;
		int num = *p->num;

		if ( num < 0 ) {	/* non-existing column */
			continue;
		}

		BnkfGet(NULL, &p->index, p->dataSize, &used, data);
		if ( 0 == memcmp(data, p->dataSave, p->dataSize) ) {
			continue;		/* data not changed */
		}

		if ( eventData.ccsds_header == data ) {
fits_write_col_byt(com.ofp, num, com.orow, 1, 6, data, &istat);
		} else if ( &eventData.ti == data ) {
fits_write_col_uint(com.ofp, num, com.orow, 1, 1, data, &istat);
		} else if ( &eventData.flag_type == data ) {
			char *array[1];
			array[0] = data;
fits_write_col_str(com.ofp, num, com.orow, 1, 1, array, &istat);
		} else if ( TINT == tbl[i].type ) {
fits_write_col_int(com.ofp, num, com.orow, 1, 1, data, &istat);
		} else if ( TDOUBLE == tbl[i].type ) {
fits_write_col_dbl(com.ofp, num, com.orow, 1, 1, data, &istat);
		}
	}

/* check cfitsio error */
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_col() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
XRSeventFitsWT_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSeventFitsWT_exit(int *status)
{
/* close output file */
	if ( NULL != com.ofp ) {
		int istat = 0;
		fits_close_file(com.ofp, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_close_file() failed for outfile\n", pname);
			*status = ANL_QUIT;
			return;
		}
		if ( com.ifp == com.ofp ) {	/* overwrite mode */
			com.ifp = NULL;
			printf("\
\n\
%s: event file '%s' overwrited\n", pname, com.ifilename);
		} else {
			printf("\
\n\
%s: event file '%s' created\n", pname, com.ofilename);
		}
		com.ofp = NULL;
	}

	*status = ANL_OK;
}
