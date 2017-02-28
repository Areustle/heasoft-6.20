/*
	telemetry data handling basic routines.
					originally coded by K.Mitsuda.
	Modified to use Fujitsu functions as slave routines
					by T.Dotani
	Modified to see Frame Indicator
					by Y.Ishisaki

	sfOpen		open telemetry data.
	sfGet		gets 1 sf of data.
	sfClose		close telemetry data.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <atFunctions.h>
#include "ascatool.h"
#include "ytlm.h"
#include "ylog.h"
#include "yfrf.h"

int _QL_MODE_ = gFALSE;					/* dummy */
TIME_TYPE _TIME_TYPE_ = TIME_IS_ADTIME;	/* default ADTIME */

/*
static void
stderr_MSG(char *format, ...)
{
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputc('\n', stderr);
	fflush(stderr);
}
static void
stdout_MSG(char *format, ...)
{
	va_list args;
	va_start(args, format);
	vfprintf(stdout, format, args);
	va_end(args);
	fputc('\n', stdout);
	fflush(stdout);
}

*/

int
sfAllcBuff(unsigned char **headerPtr, unsigned char **sfPtr)
{
	static unsigned  headerSize=128;
	static unsigned  sfSize=(64*128);
	if ((*headerPtr = (unsigned char*)malloc(headerSize))==NULL) return(1);
	if ((*sfPtr = (unsigned char*)malloc(sfSize))==NULL) return(11);
	return 0;
}

static int (*openfunc)(char *fn);
static int (*readfunc)(FR*, ADTIME*) = ascatool_logread;
static int (*closefunc)(void) = ascatool_logclose;
static long (*sfnumfunc)(void) = ascatool_logsfnum;

static int
sf_Open(char *fn)
{
	int imode, ignore_error;
	ignore_error = 0;
	if ( '!' == *fn ) {
		fn++;
		ignore_error++;
	}
	openfunc = ascatool_logopen;
	readfunc = ascatool_logread;
	closefunc = ascatool_logclose;
	imode = ascatool_logopen(fn);

	if ( imode ) {
		openfunc = ascatool_tlmopen;
		readfunc = ascatool_tlmread;
		closefunc = ascatool_tlmclose;
		sfnumfunc = ascatool_tlmsfnum;
		imode = ascatool_tlmopen(fn);

		if ( imode ) {
			openfunc = ascatool_frfopen;
			readfunc = ascatool_frfread;
			closefunc = ascatool_frfclose;
			sfnumfunc = ascatool_frfsfnum;
			imode = ascatool_frfopen(fn);
		}
	}

	if ( imode ) {
		if ( ignore_error ) {
			stderr_MSG("'%s' open error, code=%d (ignored)", fn, imode);
			return 0;
		} else {
			stderr_MSG("'%s' open error, code=%d", fn, imode);
			return 90+imode;
		}
	}

	stdout_MSG("'%s' is successfully opened", fn);
	return 0;
}

static char *fnames = NULL;
static char *fnamep;

int
sfOpen(char *fn, char *header)
{
	if ( NULL != fnames ) free(fnames);
	if ( '@' == *fn ) {
		fnames = ascatool_read_region_file(fn+1);
		if ( NULL == fnames ) {
			stderr_MSG("'%s' read error", fn);
			return 94;
		}
	} else {
		fnames = malloc(strlen(fn)+2);
		if ( NULL == fnames ) {
			stderr_MSG("out of memory");
			return 94;
		}
		strcpy(fnames, fn);
		fnames[strlen(fn)+1] = '\0';
	}
	for (fnamep = fnames; fnamep[0] || fnamep[1]; fnamep++) {
		if ( ':' == *fnamep ) {
			while ( *++fnamep ) ;
		}
		if ( ',' == *fnamep ) *fnamep = '\0';
	}
	fnamep[1] = '\0';		/* end fnames at "\0\0" */
	fnamep = fnames;
	return sf_Open(fnamep);
}

int
sfClose(void)
{
	int icode;
	icode = (*closefunc)();
	if ( icode ) {
		stderr_MSG("Telemetory file close error, code=%d", icode);
		return 999;
	}
	return 0;
}

int
frameGet(FR *frp, int *sfn, ADTIME *adt_ptr)
{
	int icode, sfnum;
	sfnum = (*sfnumfunc)();
	icode = (*readfunc)(frp, adt_ptr);
	switch ( icode ) {
	case 4:
		stderr_MSG("'%s' access error, sf=%d", fnamep, sfnum);
		return 99;
	case 1:
		stdout_MSG("'%s' EOF detected, sf=%d", fnamep, sfnum);
		while ( *fnamep++ ) ;
		if ( '\0' == *fnamep ) return QL_END;
		sfClose();
		if ( sf_Open(fnamep) ) return 99;
		return frameGet(frp, sfn, adt_ptr);
	case 2: case 3:
		return QL_NODATA;
	}
	*sfn = sfnum;
	return 0;
}

union sf_times {
	ADTIME adt;
	AtTime att;
	ASCATIME asca;
	MJD mjd;
};

int
sfGet(SF *sfp, int *sfn_ptr, ADTIME *adt_ptr)
{
	static int frn = 0;
	static int sfn;				/* for QL mode, static buffer is required */
	static union sf_times sft;
	static SF sf;
	static WD fi;
	int icode;
	do {
		if ( 0 == frn ) {
			do {
				icode = frameGet(&sf[0], &sfn, &sft.adt);
				if ( icode ) return icode;
				fi = fr_FI(sf[0]);
			} while ( fi % SFFR );
			fi++;
			frn++;
		}
		while ( frn  < SFFR ) {
			int sfn2;
			union sf_times sft2;
			icode = frameGet(&sf[frn], &sfn2, &sft2.adt);
			if ( icode ) return icode;
			if ( fi == fr_FI(sf[frn]) ) {
				frn++;
				fi++;
			} else {
				fi = fr_FI(sf[frn]);
				if ( fi % SFFR ) {
					frn = 0;
				} else {
					sft = sft2;
					memcpy(&sf[0], &sf[frn], sizeof(sf[0]));
					frn = 1;
					fi++;
				}
				break;
			}
		}
	} while ( frn < SFFR );
	frn = 0;
	memcpy(sfp, &sf, sizeof(sf));
	*sfn_ptr = sfn;
	switch ( _TIME_TYPE_ ) {
	case TIME_IS_MJD: *(MJD*)adt_ptr = sft.mjd; break;
	case TIME_IS_ASCATIME: *(ASCATIME*)adt_ptr = sft.asca; break;
	case TIME_IS_ATTIME: *(AtTime*)adt_ptr = sft.att; break;
	default: *adt_ptr = sft.adt;
	}
	return 0;
}

int
frameGetMJD(FR *sfp, MJD *mjd)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_MJD;	/* MJD */
	return frameGet(sfp, &sfnum, (ADTIME*)mjd);
}

int
sfGetMJD(SF *sfp, MJD *mjd)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_MJD;	/* MJD */
	return sfGet(sfp, &sfnum, (ADTIME*)mjd);
}

int
frameGetASCA(FR *sfp, ASCATIME *ascatime)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_ASCATIME;
	return frameGet(sfp, &sfnum, (ADTIME*)ascatime);
}

int
sfGetASCA(SF *sfp, ASCATIME *ascatime)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_ASCATIME;
	return sfGet(sfp, &sfnum, (ADTIME*)ascatime);
}

int
frameGetAtTime(FR *sfp, AtTime *ji)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_ATTIME;
	return frameGet(sfp, &sfnum, (ADTIME*)ji);
}

int
sfGetAtTime(SF *sfp, AtTime *ji)
{
	int sfnum;
	_TIME_TYPE_ = TIME_IS_ATTIME;
	return sfGet(sfp, &sfnum, (ADTIME*)ji);
}

int
sfCheck(SF *sfp)
{
	WD fi;
	int i;
	fi = (*sfp)[0][3] / SFFR * SFFR;
	for (i = 0; i < SFFR; i++) {
		FR *frp = &(*sfp)[i];
		unless ( 0xfa == (*frp)[0] ) break;
		unless ( 0xf3 == (*frp)[1] ) break;
		unless ( 0x20 == (*frp)[2] ) break;
		unless ( fi + i == (*frp)[3] ) break;
	}
	if ( i < SFFR ) {
		return -1;
	}
	return 0;
}
