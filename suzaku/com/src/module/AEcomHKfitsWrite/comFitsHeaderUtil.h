#ifndef _COM_FITS_HEADER_UTIL_H_
#define _COM_FITS_HEADER_UTIL_H_

#define GTI_HDU_NFIELDS 2 /* gti HDU column number */

typedef struct {
	char * telescope;
	char * instrument;
	char * object;
	char * observer;
	char * obs_mode;
	char * datamode;
	char * date_obs;
	char * time_obs;
	char * date_end;
	char * time_end;
	double tstart;
	double tstop;
	double ontime;
	double telapse;
	int nevents;
	char * radecsys;
	int equinox;
	double ra_pnt;
	double dec_pnt;
	double mjdrefi;
	double mjdreff;
	char * timeref;
	char * timesys;
	char * timeunit;
	char * tassign;
	int  clockapp;
	double  timedel;
	double timepixr;
	double tierrela;
	double tierabso;
	char * creator;
	char * origin;
	char * tlmfile;
	char * hduclass;
	char * hduclas1;
	char * hduclas2;
} COM_STD_KEYS;

#endif


#ifndef _COM_FITS_HEADER_UTIL_FUNC_
#define _COM_FITS_HEADER_UTIL_FUNC_
void AEsetDefaultKeywordValues(COM_STD_KEYS *stdkeys);
int AEupdateStdTimeKeys(fitsfile*, COM_STD_KEYS *stdkeys, int, int *);
int AEwriteCOMStdKeys (fitsfile*, COM_STD_KEYS *stdkeys, int, int *);
#endif
