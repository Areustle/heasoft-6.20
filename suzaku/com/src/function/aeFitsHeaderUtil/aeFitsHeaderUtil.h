/*
    aeFitsHeaderUtil.h

    2005/04/06 Y.ISHISAKI	version 1.8
		split from AEcomHKfitsWrite-1.7

    2005/05/15 Y.ISHISAKI	version 2.0
		add aefits_write_name_version(), aefits_modify_comment(),
		aefits_modify_table_comments()

    2005/06/03 Y.ISHISAKI	version 2.1
		telescope -> telescop, instrument -> instrume
		add ra_obj, dec_obj, ra_nom, dec_nom, pa_nom, mean_ea1/2/3
		add declarations of aeGetFFFname(), aeGetFFForigin()

	2005/06/07 Y.ISHISAKI	version 2.3
		rename tlmfile -> tlm_file, add att_file, orb_file, teldef

	2005/06/22 Y.ISHISAKI	version 2.4
		add tim_file
		sort variable list order in the FITS file order

	2005/07/04 Y.ISHISAKI	version 2.5
		add aefits_delta_phi(),aefits_mjd_tt2mission(),aefits_mission2mjd_tt()
		add AE_STD_KEYS_NAME keyname
		add aeCopyStdKeys() declaration

	2005/10/08 Y.ISHISAKI	version 2.9
		add SEQ_NUM, OBS_REM

	2005/10/18 Y.ISHISAKI	version 3.0
		rename SEQ_NUM -> OBS_ID, add LEAPFILE, re-order keywords

	2006/08/01 Y.ISHISAKI	version 3.2
		move aefits_mjd_tt2mission(), aefits_mission2mjd_tt() to astetool-1.80
		aefits_mjd_tt2mission(), aefits_mission2mjd_tt() remains but obsolete
		add aefits_datestr2attimeD(), aefits_attimeD2datestr()
		add aefits_degToRAstr(), aefits_degToDECstr()

	2006/08/07 Y.ISHISAKI	version 3.3
		add filter

	2006/09/17 Y.ISHISAKI	version 3.4
		add aefits_write_tool_version()
*/

#ifndef _AE_FITS_HEADER_UTIL_H_
#define _AE_FITS_HEADER_UTIL_H_

typedef struct {
	char * telescop;
	char * instrume;
	char * obs_mode;
	char * datamode;
	char * filter;
	char * obs_id;
	char * observer;
	char * object;
	char * obs_rem;
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
	char * radecsys;
	int    equinox;
	char * date_obs;
	char * time_obs;
	char * date_end;
	char * time_end;
	double tstart;
	double tstop;
	double telapse;
	double ontime;
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
	char * hduclass;
	char * hduclas1;
	char * hduclas2;
	char * tlm_file;
	char * tim_file;
	char * att_file;
	char * orb_file;
	char * leapfile;
	char * teldef;
	char * creator;
	char * origin;

	struct AE_STD_KEYS_NAME {
		char * telescop;
		char * instrume;
		char * obs_mode;
		char * datamode;
		char * obs_id;
		char * filter;
		char * observer;
		char * object;
		char * obs_rem;
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
		char * radecsys;
		char * equinox;
		char * date_obs;
		char * time_obs;
		char * date_end;
		char * time_end;
		char * tstart;
		char * tstop;
		char * telapse;
		char * ontime;
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
		char * hduclass;
		char * hduclas1;
		char * hduclas2;
		char * tlm_file;
		char * tim_file;
		char * att_file;
		char * orb_file;
		char * leapfile;
		char * teldef;
		char * creator;
		char * origin;
	} keyname;

	struct AE_STD_KEYS_COMMENT {
		char * telescop;
		char * instrume;
		char * obs_mode;
		char * datamode;
		char * obs_id;
		char * filter;
		char * observer;
		char * object;
		char * obs_rem;
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
		char * radecsys;
		char * equinox;
		char * date_obs;
		char * time_obs;
		char * date_end;
		char * time_end;
		char * tstart;
		char * tstop;
		char * telapse;
		char * ontime;
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
		char * hduclass;
		char * hduclas1;
		char * hduclas2;
		char * tlm_file;
		char * tim_file;
		char * att_file;
		char * orb_file;
		char * leapfile;
		char * teldef;
		char * creator;
		char * origin;
	} comment;

} AE_STD_KEYS;

#ifdef __cplusplus
extern "C"
{
#endif

void aeSetDefaultKeywordValues(AE_STD_KEYS *stdkeys);
int aeUpdateStdTimeKeys(fitsfile *fp, AE_STD_KEYS *v);
int aeWriteStdKeys(fitsfile *fp, AE_STD_KEYS *v);
int aeFinalizeStdKeys(fitsfile *fp, AE_STD_KEYS *v, long nrow, char *pname);
int aeCopyStdKeys(fitsfile *ifp, fitsfile *ofp, AE_STD_KEYS *stdkeys);
void aeGetFFFname(AE_STD_KEYS *v, char *name, int name_size, char *ext);
void aeGetFFForigin(AE_STD_KEYS *v);

/************************************************************************
int aefits_write_module_history()	: write ANL modules in FITS header

Input:
	fitsfile *fp		: fits file pointer to be written
	char *pname			: message header string

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int aefits_write_module_history(fitsfile *fp, char *pname);

/************************************************************************
int aefits_write_tool_version()	: write tool version in FITS header

HISTORY + pname      version N.n

Input:
	fitsfile *fp		: fits file pointer to be written
	char *pname			: tool name
	char *version		: version string

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int aefits_write_tool_version(fitsfile *fp, char *pname, char *version);

/************************************************************************
int aefits_write_name_vers()	: write name & version into fits header,

HISTORY ---------------------------------------------------------
HISTORY AEtimePacketFitsWrite version 2.4 at 2005-05-15T10:51:21
HISTORY ---------------------------------------------------------

Input:
	fitsfile *fp		: fits file pointer to be written
	char *name			: name of task/module
	char *version		: version of task/module

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int aefits_write_name_vers(fitsfile *fp, char *name, char *version);

/************************************************************************
int aefits_modify_comment()	: modify comment of the specified keyword

	This function is similar to fits_modify_comment(),
	but long comment is automatically wrapped to the next line.

Input:
	fitsfile *fp		: fits file pointer to be modified
	char *key			: header keyword to modify comment
	char *comment		: new comment words

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int aefits_modify_comment(fitsfile *fp, char *key, char *comment);

/************************************************************************
int aefits_modify_table_comments()	: modify comment of the table TTYPEnnn

Input:
	fitsfile *fp		: fits file pointer to be modified
	int tfield			: number of table columns
	char *comments[]	: new comment words

Return_Values:
	0					: success
	-1					: invalid number of tfield
	others				: CFITSIO error
************************************************************************/
int aefits_modify_table_comments(fitsfile *fp, int tfield, char *comments[]);

/************************************************************************
int aefits_del_write_key()	: write key after delete previous one

Input:
	fitsfile *fp		: fits file pointer to be modified
	int datatype		: specifies the data type of the value
	char *keyname		: name of a keyword (8 char max, null-terminated)
	void *value			: keyword value
	char *comment		: keyword comment field (72 char max, null-terminated)

Output:
	int *status			: returned error status code (0 = OK)
************************************************************************/
int aefits_del_write_key(fitsfile *fp, int datatype, char *keyname, \
void *value, char *comment, int *status);

/************************************************************************
int aefits_del_write_key_fixdbl()	: write key after delete previous one

Input:
	fitsfile *fp		: fits file pointer to be modified
	char *keyname		: name of a keyword (8 char max, null-terminated)
	double numval		: numerical data value
	int decimals		: number of decimal places to be displayed
	char *comment		: keyword comment field (72 char max, null-terminated)

Output:
	int *status			: returned error status code (0 = OK)
************************************************************************/
int aefits_del_write_key_fixdbl(fitsfile *fp, char *keyname, \
double numval, int decimals, char *comment, int *status);

/************************************************************************
int aefits_basename()	: get file name without directory

Input:
	char *path			: new comment words

Return_Values:
	char *basename		: the  component following the final '/'
************************************************************************/
char *aefits_basename(char *path);

/************************************************************************
double aefits_delta_phi()	: calculate angular differnce

Input:
	double phi1, phi0	: angles in degree

Return_Values:
	double delta_phi	: (phi1 - phi0), in the range of (-180.0, 180.0]
************************************************************************/
double aefits_delta_phi(double phi1, double phi0);

/************************************************************************
double aefits_mjd_tt2mission()	: convert MJD-TT into mission time (OBSOLETE)

Input:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mission_time	: mission time (s)
************************************************************************/
double aefits_mjd_tt2mission(double mjd, int mjdrefi, double mjdreff);

/************************************************************************
double aefits_mission2mjd_tt()	: convert mission time int MJD-TT (OBSOLETE)

Input:
	double mission_time	: mission time (s)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
************************************************************************/
double aefits_mission2mjd_tt(double mission_time, int mjdrefi, double mjdreff);

#ifdef MJD_J2000	/* check atFunctions */
/************************************************************************
int aefits_datestr2attimeD()	:

		convert date string '2000-01-01T00:00:00.000' into AtTimeD

Input:
	char *datestrIN		: input date string '2000-01-01T00:00:00.000'

Output:
	AtTimeD *attimeOUT	: output AtTimeD structure

Return_Values:
	0					: success
	-1					: invalid format
************************************************************************/
int aefits_datestr2attimeD(char *datestrIN, AtTimeD *attimeOUT);

/************************************************************************
char *aefits_attimeD2datestr() :

		convert AtTimeD into date string '2000-01-01T00:00:00.000'

Input:
	AtTimeD *attimeIN	: input AtTimeD structure

Output:
	char *datestrOUT	: output date string '2000-01-01T00:00:00.000'

Return_Values:
	char *datestrOUT	: same as argument
************************************************************************/
char *aefits_attimeD2datestr(AtTimeD *attimeIN, char *datestrOUT);
#endif

/************************************************************************
double aefits_datestr2aetime()	:

		convert date string '2000-01-01T00:00:00.000' into Astro-E time

Input:
	char *datestrIN		: input date string '2000-01-01T00:00:00.000'

Output:
	double *aetime		: output Astro-E time

Return_Values:
	0					: success
	-1					: invalid format
************************************************************************/
int aefits_datestr2aetime(char *datestrIN, double *aetime);

/************************************************************************
char *aefits_aetime2datestr() :

		convert Astro-E time into date string '2000-01-01T00:00:00.000'

Input:
	double aetime		: input Astro-E time

Output:
	char *datestrOUT	: output date string '2000-01-01T00:00:00.000'

Return_Values:
	char *datestrOUT	: same as argument
************************************************************************/
char *aefits_aetime2datestr(double aetime, char *datestrOUT);

/************************************************************************
char *aefits_degToRAstr() :	convert deg into R.A. string 'NNhNNmNN.Ns'

Input:
	double deg			: Right Ascension in degree

Output:
	char *outstr		: output R.A. string 'NNhNNmNN.Ns'

Return_Values:
	char *outstr		: same as argument
************************************************************************/
char *aefits_degToRAstr(double deg, char *outstr);

/************************************************************************
char *aefits_degToDECstr() : convert deg into DEC. string '+NNdNNmNNs'

Input:
	double deg			: Declination in degree

Output:
	char *outstr		: output DEC. string '+NNdNNmNNs'

Return_Values:
	char *outstr		: same as argument
************************************************************************/
char *aefits_degToDECstr(double deg, char *outstr);

#ifdef __cplusplus
}
#endif

#endif

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; c-basic-offset:4  ***
;;; End: ***
*/
