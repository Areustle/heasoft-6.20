/*
  aste_caldb: CALDB support routines for SUZAKU

	2006-07-02	Y.ISHISAKI	version 1.0

	2006-09-15	Y.ISHISAKI	version 1.1
		add aste_caldb_find_leapfile()

	2007-05-14	Y.ISHISAKI	version 1.2
		add aste_caldb_find_rigidity(), aste_caldb_find_candidate()
*/

#ifndef _ASTE_CALDB_H_
#define _ASTE_CALDB_H_

typedef struct {

/* input parameters */
	char *telescop;	/* TELESCOP keyword */
	char *instrume;	/* INSTRUME keyword */
	char *detnam;	/* DETNAM keyword */
	char *filter;	/* FILTER keyword */
	char *codename;	/* CCNM0001 keyword */
	char *expr;		/* logical expression for CBDnnnnn */
	char *date0;	/* start of valid date 'YYYY-MM-DD' in UTC */
	char *time0;	/* start of valid time 'hh:mm:ss.uuuuuu' in UTC */
	char *date1;	/*  end  of valid date 'YYYY-MM-DD' in UTC */
	char *time1;	/*  end  of valid date 'hh:mm:ss.uuuuuu' in UTC */

/* output parameters */
	int nfound;
	int status;
	char *filename;	/* primary candidate file name, allocated by malloc() */
	char **fnames;	/* list of file names, if multiple candidates exists */
	long *extno;	/* list of FITS extension number */

/* internal variables */

} CALDB_INFO;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int aste_caldb_init()	: initialize CALDB_INFO variable

Input:
	CALDB_INFO *caldb	: empty CALDB_INFO structure

Return_Values:
	0					: success
************************************************************************/
int aste_caldb_init(CALDB_INFO *caldb);


/************************************************************************
int aste_caldb_free()	: free allocated memory in CALDB_INFO
						: ** CAUTION ** this also free caldb->filename

Input:
	CALDB_INFO *caldb	: CALDB_INFO structure

Return_Values:
	0					: success
************************************************************************/
int aste_caldb_free(CALDB_INFO *caldb);


/************************************************************************
int aste_caldb_get()	: get CALDB filename
						: ** CAUTION ** there can be multiple candidates

Input/Output:
	CALDB_INFO *caldb	: CALDB_INFO structure

Return_Values: (same as caldb->status)
	0					: success
	-1					: malloc() error
	others  			: CFITSIO / HEADAS errors (defined in headas_error.h)
************************************************************************/
int aste_caldb_get(CALDB_INFO *caldb);


/************************************************************************
char *aste_caldb_find_leapfile()	: find leap-seconds file name

Input:
	char *o_leapfile	: filename or AUTO or CALDB or CALDB;${LHEA_DATA}/...

Return_Values:
	NULL  				: no CALDB entry, or malloc() error
	o_leapfile			: o_leapfile != "AUTO && o_leapfile != "CALDB"
	others  			: allocated string, point to leapsecXXX.fits
************************************************************************/
char *aste_caldb_find_leapfile(char *o_leapfile);

/************************************************************************
char *aste_caldb_find_rigidity()	: find rigidity file name

Input:
	char *o_rigidity	: filename or CALDB or CALDB;${LHEA_DATA}/...

Return_Values:
	NULL  				: no CALDB entry, or malloc() error
	o_rigidity			: o_rigidity != "CALDB"
	others  			: allocated string, point to rigidityXXX.fits
************************************************************************/
char *aste_caldb_find_rigidity(char *o_rigidity);

/************************************************************************
char *aste_caldb_find_candidate()	: find a candidate from file name list

Input:
	char *o_candidate	: filename or CALDB or CALDB;${LHEA_DATA}/...
	char *(*f)(char *)	: function to get CALDB file name

Return_Values:
	NULL  				: no CALDB entry, or malloc() error
	o_candidate			: o_candidate != "CALDB"
	others  			: allocated string, point to XXX.fits
************************************************************************/
char *aste_caldb_find_candidate(char *o_candidate, char *(*f)(char *));

/************************************************************************
char *aste_caldb_find()	: find CALDB file if o_filename='CALDB'

Input:
	char *instrume		: instrument name
	char *codename		: code name for CALDB
	char *o_filename	: filename or CALDB

Return_Values:
	NULL  				: no CALDB entry, or malloc() error
	o_filename			: o_filename != "CALDB"
	others  			: allocated string, point to XXX.fits
************************************************************************/
char *aste_caldb_find(char *instrume, char *codename, char *o_filename);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_CALDB_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
