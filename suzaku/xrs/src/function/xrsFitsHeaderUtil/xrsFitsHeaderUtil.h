/*
    xrsFitsHeaderUtil.h

	2005/04/29 Y.ISHISAKI	version 1.7
		change function names
		add XRS_STD_KEYS_COMMENT *comment in structure
		modified to use ASTE:RPT:IFILE_NAME:PTR in xrsGetFFFname()
		add xrsGetFFForigin()

	2005/06/03 Y.ISHISAKI	version 1.9
		telescope -> telescop, instrument -> instrume
		add ra_obj, dec_obj, ra_nom, dec_nom, pa_nom, mean_ea1/2/3

	2005/06/19 Y.ISHISAKI	version 2.1
		rename tlmfile -> tlm_file
		add tim_file, att_file, orb_file, teldef, gatevalv

	2005/06/26 Y.ISHISAKI	version 2.3
		add XRS_STD_KEYS_NAME keyname
		add xrsCopyStdKeys() declaration
*/

#ifndef _XRS_FITS_HEADER_UTIL_H_
#define _XRS_FITS_HEADER_UTIL_H_

typedef struct {
	char * telescop;
	char * instrume;
	char * obs_mode;
	char * datamode;
	char * filter;
	char * gatevalv;
	char * object;
	char * observer;
	char * date_obs;
	char * time_obs;
	char * date_end;
	char * time_end;
	double tstart;
	double tstop;
	double telapse;
	double ontime;
	char * radecsys;
	int    equinox;
	double ra_obj;
	double dec_obj;
	double ra_pnt;
	double dec_pnt;
	double ra_nom;
	double dec_nom;
	double pa_nom;
	double mean_ea1;
	double mean_ea2;
	double mean_ea3;
	int    mjdrefi;
	double mjdreff;
	char * timeref;
	char * timesys;
	char * timeunit;
	char * tassign;
	int    clockapp;
	double timedel;
	double timepixr;
	double tierrela;
	double tierabso;
	char * creator;
	char * origin;
	char * tlm_file;
	char * tim_file;
	char * att_file;
	char * orb_file;
	char * teldef;
	char * hduclass;
	char * hduclas1;
	char * hduclas2;
/* DM keywords */
	char * mtype1;
	char * mform1;
	char * mtype2;
	char * mform2;
	char * mtype3;
	char * mform3;

	struct XRS_STD_KEYS_NAME {
		char * telescop;
		char * instrume;
		char * obs_mode;
		char * datamode;
		char * filter;
		char * gatevalv;
		char * object;
		char * observer;
		char * date_obs;
		char * time_obs;
		char * date_end;
		char * time_end;
		char * tstart;
		char * tstop;
		char * telapse;
		char * ontime;
		char * radecsys;
		char * equinox;
		char * ra_obj;
		char * dec_obj;
		char * ra_pnt;
		char * dec_pnt;
		char * ra_nom;
		char * dec_nom;
		char * pa_nom;
		char * mean_ea1;
		char * mean_ea2;
		char * mean_ea3;
		char * mjdrefi;
		char * mjdreff;
		char * timeref;
		char * timesys;
		char * timeunit;
		char * tassign;
		char * clockapp;
		char * timedel;
		char * timepixr;
		char * tierrela;
		char * tierabso;
		char * creator;
		char * origin;
		char * tlm_file;
		char * tim_file;
		char * att_file;
		char * orb_file;
		char * teldef;
		char * hduclass;
		char * hduclas1;
		char * hduclas2;
	/* DM keywords */
		char * mtype1;
		char * mform1;
		char * mtype2;
		char * mform2;
		char * mtype3;
		char * mform3;
	} keyname;

	struct XRS_STD_KEYS_COMMENT {
		char * telescop;
		char * instrume;
		char * obs_mode;
		char * datamode;
		char * filter;
		char * gatevalv;
		char * object;
		char * observer;
		char * date_obs;
		char * time_obs;
		char * date_end;
		char * time_end;
		char * tstart;
		char * tstop;
		char * telapse;
		char * ontime;
		char * radecsys;
		char * equinox;
		char * ra_obj;
		char * dec_obj;
		char * ra_pnt;
		char * dec_pnt;
		char * ra_nom;
		char * dec_nom;
		char * pa_nom;
		char * mean_ea1;
		char * mean_ea2;
		char * mean_ea3;
		char * mjdrefi;
		char * mjdreff;
		char * timeref;
		char * timesys;
		char * timeunit;
		char * tassign;
		char * clockapp;
		char * timedel;
		char * timepixr;
		char * tierrela;
		char * tierabso;
		char * creator;
		char * origin;
		char * tlm_file;
		char * tim_file;
		char * att_file;
		char * orb_file;
		char * teldef;
		char * hduclass;
		char * hduclas1;
		char * hduclas2;
	/* DM keywords */
		char * mtype1;
		char * mform1;
		char * mtype2;
		char * mform2;
		char * mtype3;
		char * mform3;
	} comment;
} XRS_STD_KEYS;

#ifdef __cplusplus
extern "C"
{
#endif

void xrsSetDefaultKeywordValues(XRS_STD_KEYS *stdkeys);
int xrsWriteStdKeys(fitsfile *fp, XRS_STD_KEYS *v);
int xrsWriteDMcoorKeys(fitsfile *fp, XRS_STD_KEYS *v);
int xrsUpdateStdTimeKeys(fitsfile *fp, XRS_STD_KEYS *v);
int xrsFinalizeStdKeys(fitsfile *fp, XRS_STD_KEYS *v, long nrow, char *pname);
int xrsCopyStdKeys(fitsfile *ifp, fitsfile *ofp, XRS_STD_KEYS *stdkeys);
void xrsGetFFFname(XRS_STD_KEYS *v, char *name, int name_size, char *ext);
void xrsGetFFForigin(XRS_STD_KEYS *v);

#ifdef __cplusplus
}
#endif

#endif

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
