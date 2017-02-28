/************************************************************************/
/*  faio by R.Fujimoto							*/
/*	履歴								*/
/* Read ASCA attitude file and interpolate the Q parameters             */
/*	93/04/24 Ver 1.0				       		*/
/*	93/04/27 Ver 1.1				       		*/
/*	93/07/12 Ver 1.2				       		*/
/*	93/08/05 Ver 1.3						*/
/*		ファイルの最後の0を読み始めた場合に、faReadADStimeが	*/
/*		先に進まず、その場で足踏みするように変更した。	       	*/
/*	93/09/25 Ver 1.4	by Y.Ishisaki				*/
/*		時刻を戻した時にヘッダーのサイズが正しく認識される	*/
/*		ように修正。その他いろいろ。				*/
/*     1997/05/21 Ver. 2.0  by Jeff Guerber, Hughes STX Corp/NASA-Goddard   */
/*              Use cfitsio package for reading the FITS files.  I've tried */
/*              to keep the functionality as similar as possible to the     */
/*              original.  Note: use of cfitsio requires an ANSI C compiler.*/
/*     1997/07/02 V.2.1  by Jeff Guerber.  Report cfitsio errors.           */
/*     1998/03/06 V.2.2  Jeff Guerber. Bug in faReadADStime resulted in an  */
/*          infinite loop when ascatime's between last 2 rows; also report  */
/*          errors. Mod faSearchADStime to not ignore faReadADStime errors. */
/*     1998-08-26 V.2.3  Jeff Guerber.  Added faQparamE.
/*                                                                          */
/* NOTE: FOR USE IN FTOOLS, COMPILE WITH "-DFTOOL" (SEE faRptErr()), AND BE */
/* SURE THE MAIN PROGRAM CALLS c_ptaskn (C) OR ptaskn (FORTRAN).            */
/****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "faio.h"

#ifndef SEEK_SET
#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2
#endif

static fitsfile *fp;
static double starttime;
static double endtime;
static long cur_row, max_row;  /* position in and size of data table */

/****************************************/
/*  faOpen				*/
/*	姿勢FRFを開く			*/
/*  Open a file and position it to the ATTITUDE HDU */
/****************************************/
int
faOpen(char filename[])
{
	int status, hdutype;
	char keyval[80];

	status = 0;
	if ((fits_open_file(&fp, filename, READONLY, &status)) != 0) {
	       faRptErr("faOpen: attitude file open error: %d, %s\n", status);
	       return (OPEN_ERR);
	}
	/* Position to 2nd HDU */
	if (fits_movabs_hdu(fp, 2, &hdutype, &status) != 0) {
		faRptErr("faOpen: attitude file positioning error: %d, %s\n",
			 status);
		status=0;  fits_close_file(fp, &status);
		return (SEEK_ERR);
	}
	/* Check that EXTNAME = 'ATTITUDE' */
	fits_read_key_str(fp,"EXTNAME", keyval, NULL, &status);
	if ( status != 0 )  {
	        faRptErr("faOpen: attitude file, EXTNAME keyword: %d, %s\n",
			 status);
		status = 0;  fits_close_file(fp, &status);
		return (SEEK_ERR);
	}
	if (strncmp(keyval,"ATTITUDE",8) != 0) {
              faRptErr("faOpen: attitude file, EXTNAME is not ATTITUDE\n", 0);
	      status = 0;  fits_close_file(fp, &status);
	      return (SEEK_ERR);
	}

	/* Read the number of rows in the data table. Init cur_row. */
	fits_read_key(fp, TLONG, "NAXIS2", &max_row, NULL, &status);
	cur_row = 1;

	/* Read the MTIME0, MTIME1 keywords */
	fits_read_key(fp, TDOUBLE, "MTIME0", &starttime, NULL, &status);
	fits_read_key(fp, TDOUBLE, "MTIME1", &endtime, NULL, &status);
	if (status != 0) {
	       faRptErr ( "faOpen: attitude file keyword error: %d, %s\n",
			  status);
	       status = 0;  fits_close_file(fp, &status);
	       return (NO_KEYWORD);
	}

	return (NORMAL_END);
}

/********************************/
/*  faClose			*/
/*	姿勢FRFを閉じる		*/
/********************************/
int
faClose(void)
{
        int status;
	status = 0;
	if (fits_close_file(fp, &status) != 0) {
             faRptErr ("faClose: attitude file close error: %d, %s\n", status);
	     return (CLOSE_ERR);
	} else {
	     return 0;
	}
}

/********************************/
/*  faHeader			*/
/*	FITSヘッダーを読み出す	*/
/*  Return the value of a given keyword, as a string. */
/********************************/
int
faHeader(char keyword[], char value[])
{
	int len, status;

	status = 0;
	len = strlen(keyword);
	if ( 0 == len ) {
		return(ILLEGAL_INPUT);
	}

	fits_read_keyword(fp, keyword, value, NULL, &status);
	if (status != 0) {
             return(NO_KEYWORD);
	}
        return(NORMAL_END);
}

/************************************************************************/
/*  faReadADStime							*/
/*	現在位置の時刻を読み出し、ファイルポインタを次の位置に進める	*/
/*  Read one time from the table.                                       */
/************************************************************************/
double
faReadADStime(void)
{
	int i;
	double adstime;
	int anynul, status;

	status = 0;

	if (cur_row > max_row) {
                faRptErr("faReadADStime: want to read past eof\n", 0);
		return(-9999.0);
	}

	fits_read_col_dbl(fp, 1, cur_row, 1L, 1L, 0.0, &adstime,
			  &anynul, &status);
        if (status != 0) {
	        faRptErr("faReadADStime: read error: %d, %s\n", status );
	        return (-999.0);
        }
	cur_row++;

	/* ファイルの終わりの方を埋めている0を読み始めたら、最後で足踏みする */
	if (adstime == 0.0) {
		faMove(-2);
		adstime = faReadADStime();
	}
	return (adstime);
}

/************************************************************************/
/*  faSearchADStime							*/
/*	与えられたあすか時刻のデータにファイルポインタを移動させる	*/
/*  I think it's supposed to search for the last ADStime < arg.         */
/************************************************************************/
int
faSearchADStime(double ascatime)
{
        double adstime;

	if ((ascatime < starttime) || (ascatime > endtime)) {
		return (ILLEGAL_INPUT);
	}

	adstime = faReadADStime();
	if (adstime < 0.0) return (SEEK_ERR);
	if (ascatime < adstime) {
		faMove(-3);
		adstime = faReadADStime();
		if (adstime < 0.0) return (SEEK_ERR);
		if (ascatime < adstime) {
			/* Here's what the original had. IS THIS INTENDED?  */
			/* if ( fseek(fp, 80*(36+headersize), SEEK_SET) ) { */
			/*	return (SEEK_ERR);                          */
			/* }                                                */
		    cur_row = 1;
		    return (SEEK_ERR);
		}
	}

	while (1) {
	        adstime = faReadADStime();
                if (adstime < 0.0) return (SEEK_ERR);
		if (ascatime < adstime) break;
	}
	faMove(-2);
	return (NORMAL_END);
}

/****************************************************************/
/*  faMove							*/
/*	現在位置からレコード単位でファイルポインタを移動させる	*/
/*  Basically just adjusts the current position in the table.   */
/****************************************************************/
int
faMove(int numsf)
{
        cur_row = cur_row + numsf;

	/* I think this could produce infinite loops, eg in faReadADSTime, */
	/* but the original did something similar. */
	if (cur_row < 1) {
	      cur_row = 1;
	}

	if (cur_row > max_row) {
	      return(SEEK_ERR);
	}
	return (NORMAL_END);
}

/************************************************************************/
/*  faReadADSdata							*/
/*	現在位置の姿勢データを読み出し、ADS_DATA構造体に入れて返す	*/
/*  Read a full row into an ADS_DATA structure; increment row counter.  */
/************************************************************************/
int
faReadADSdata(ADS_DATA *adsdata)
{
	int i;
	int status, anynul;

	status = 0;
	fits_read_col_dbl(fp, 1, cur_row, 1L, 1L, 0.0, &adsdata->adstime,
			  &anynul, &status);    /* TIME column */
        fits_read_col_dbl(fp, 2, cur_row, 1L, 4L, 0.0, adsdata->qparam,
			  &anynul, &status);    /* QPARAM column */
	fits_read_col_dbl(fp, 3, cur_row, 1L, 3L, 0.0, adsdata->qparamerr,
			  &anynul, &status);    /* SIGMA column */
	fits_read_col_byt(fp, 4, cur_row, 1L, 3L, '0', adsdata->sensor,
			  &anynul, &status);    /* SENSOR column */
	cur_row++;

	if (status != 0) {
	  faRptErr( "faReadADSdata: read error: %d, %s\n", status);
	  return (READ_ERR);
	}
	return (NORMAL_END);
}

/********************************************************/
/*  faIntrpolQparam					*/
/*    任意の時刻のQパラメタを内挿によって求める		*/
/********************************************************/
int
faIntrpolQparam(double t0, double qparam0[4],
				double t1, double qparam1[4],
				double t, double qparam[4])
{
	int i, j;
	double qparam0inv[4], deltaq[4], deltaq_t[4];
	double si;
	double e[4];
	double Phi2;
	double deltaPhi2;
	for (j = 0; j < 3; j++) {
		qparam0inv[j] = -qparam0[j];
	}
	qparam0inv[3] = qparam0[3];
	atQuatProd(qparam0inv, qparam1, deltaq);
	if (deltaq[3] > 1.0 - EPS) {
		for (j = 0; j < 4; j++) {
			qparam[j] = qparam0[j];
		}
		return (NOT_INTERPOLATED);
	} else {
		Phi2 = acos(deltaq[3]);
		si = sin(Phi2);
		for (j = 0; j < 3; j++) {
			e[j] = deltaq[j]/si;
		}
	}
	deltaPhi2 = Phi2*(t - t0)/(t1 - t0);
	si = sin(deltaPhi2);
	for (j = 0; j < 3; j++) {
		deltaq_t[j] = e[j]*si;
	}
	deltaq_t[3] = cos(deltaPhi2);
	atQuatProd(qparam0, deltaq_t, qparam);
	return (NORMAL_END);
}

static ADS_DATA adsdata0 = {
	0, { 0, 0, 0, 0 }, { 0, 0, 0 }, "   "
};
static ADS_DATA adsdata1 = {
	0, { 0, 0, 0, 0 }, { 0, 0, 0 }, "   "
};

/************************************************/
/*  faQparam					*/
/*	任意の時刻のQパラメタを返す		*/
/************************************************/
int
faQparam(double ascatime, double qparam[4])
{
	int code;
	if ( ascatime < adsdata0.adstime || adsdata1.adstime < ascatime ) {
		code = faSearchADStime(ascatime);
		if ( code ) return code;
		code = faReadADSdata(&adsdata0);
		if ( code ) return code;
		code = faReadADSdata(&adsdata1);
		if ( code ) return code;
	}
	code = faIntrpolQparam(adsdata0.adstime, adsdata0.qparam,
			       adsdata1.adstime, adsdata1.qparam,
			       ascatime, qparam);
	return code;
}

/**********************************************************/
/*  faQparamE  					          */
/*	Like faQparam, but extrapolate to +/- margin sec  */
/*      before or after the atttitude file's range        */
/**********************************************************/
int
faQparamE(double ascatime, double qparam[4], double margin)
{
	int code;

	if ( (starttime-margin <= ascatime) && (ascatime <= starttime) ) {

                cur_row = 1;
                code = faReadADSdata(&adsdata0);   if (code != 0) return code;
		code = faReadADSdata(&adsdata1);   if (code != 0) return code;

        } else if ( (endtime <= ascatime) && (ascatime <= endtime+margin) ) {

                cur_row = max_row - 1;
                code = faReadADSdata(&adsdata0);   if (code != 0) return code;
		code = faReadADSdata(&adsdata1);   if (code != 0) return code;

        } else if ( ascatime < adsdata0.adstime || adsdata1.adstime < ascatime ) {

		code = faSearchADStime(ascatime);  if (code != 0) return code;
		code = faReadADSdata(&adsdata0);   if (code != 0) return code;
		code = faReadADSdata(&adsdata1);   if (code != 0) return code;
	}
        /* else, ascatime is already between the two current rows */

	code = faIntrpolQparam(adsdata0.adstime, adsdata0.qparam,
			       adsdata1.adstime, adsdata1.qparam,
			       ascatime, qparam);
	return code;
}

/****************************************************************/
/*  faAttitude							*/
/*	任意の時刻のQパラメタとその前後の姿勢決定結果を返す	*/
/****************************************************************/
int
faAttitude(double ascatime, ADS_DATA *ads0p, ADS_DATA *ads1p, double qparam[4])
{
	int code;
	code = faQparam(ascatime, qparam);
	*ads0p = adsdata0;
	*ads1p = adsdata1;
	return code;
}

/*************************************************************************/
/*  faRptErr                                                             */
/*  Report Cfitsio error messages.  Argument `msg' *may* contain %d      */
/*  followed by %s, so that the cfitsio status (argument `status') and   */
/*  message can be written into it (printf ignores excess arguments).    */
/*  Output must be done differently for use in FTOOLS (which use         */
/*  special calls for IRAF compatiblity) and standalone (where we might  */
/*  not have the ftools libraries); compile with -DFTOOL for FTOOLS case.*/
/*  Also for FTOOLS, the main program *must* set the taskname by calling */
/*  c_ptaskn (C, in library/c_utilities/) or ptaskn (Fortran, in         */
/*  library/utilities/gen/misc.for) or c_fcerr won't work right.         */
/*  Added by Jeff Guerber, HSTX/NASA-GSFC, July 1997                     */
/*************************************************************************/
#ifdef FTOOL
#include "cftools.h"
#endif

int
faRptErr( char *msg, int status )
{
      char outmsg[120], cfmsg[30];

      fits_get_errstatus(status, cfmsg);

#ifdef FTOOL
      sprintf(outmsg, msg, status, cfmsg);
      c_fcerr(outmsg);
#else
      fprintf(stderr, msg, status, cfmsg);
#endif

      return 0;
}
