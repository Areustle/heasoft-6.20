/************************************************************************

  aste_gethk.c		functions to get HK values

	(basic)
		aste_gethk_init()
		aste_gethk_id()
		aste_gethk()

	(obsolete)
  		aste_gethk_register()
  		aste_gethk_int()
  		aste_gethk_double()

	version 0.4	1999.07.30	Emi Miyata
	FITS file が読めるようになった。

	version 0.5	1999.08.03	Emi Miyata
	release to astroe team

	version 0.6	1999.08.20	Emi Miyata
	fix for bug in reading bit/byte column data

	version 0.7	1999.08.28	Emi Miyata
	speed up to read time column

	version 0.8	1999.12.30	Emi Miyata
	add stderr at fprintf

	version 0.9	1999.12.30	Emi Miyata
	fix for bug in cast

	version 1.0	2000.02.03	Emi Miyata
	support pre-read in data column

	version 1.1	2000.02.04	Emi Miyata
	fix for treatment of row<= 0 & row> max
	add _PRINT_OUT_

	2004-03-14 Y.ISHISAKI	version 1.3
		increase MAX_HKFILE_NUM 20 -> 256
		fix bug in readling filelist in aste_gethk_init();

	2004-04-05 Y.ISHISAKI	version 2.0
		add aste_gethk_id() & aste_gethk()
		many substantial modifications
		now, aste_gethk_register(), aste_gethk_int/double() are obsolete

	2004-04-07 Y.ISHISAKI	version 2.1
		fix typecode handling bug when unsigned or 64-bit long
		check (typecode < 0) for variable length column
		return CFITSIO error if anl_gethk() fails

	2004-04-14 Y.ISHISAKI	version 2.2
		change warning message when aste_time < TSTART or TSTOP < aste_time

	2005-06-08 Y.ISHISAKI	version 2.3
		support for cfitsio3 in get_column_info()

	2005-09-19 Y.ISHISAKI	version 2.4
		ffgcpr() -> ffgcprll() for cfitsio-3.003, do not work with old cfitsio

	2007-04-08 Y.ISHISAKI	version 2.5
		use anl_msg() instead of printf() & fprintf(stderr, ..)
		return ASTE_GETHK_GTI_ERROR & stime[] when aste_time is out of range
		bug fix of get_value() that anl_gethk() returns error when t=TSTOP

*************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "aste_gethk.h"
#include "fitsio.h"
#include "fitsio2.h"

static char pname[] = "aste_gethk";
static char version[] = "2.5";

#define	MAX_LINE_LEN			1024
#define HK_CONTENTS_ALLOC_UNIT	200

/* CFITSIO binary table column information, obtained from cfitsio-2.430 */
struct column_info {
	double scale;	/* O - FITS scaling factor (TSCALn keyword value)   */
	double zero;	/* O - FITS scaling zero pt (TZEROn keyword value)  */
	char tform[20];	/* O - ASCII column format: value of TFORMn keyword */
	long twidth;	/* O - width of ASCII column (characters)           */
	int tcode;		/* O - column datatype code: I*4=41, R*4=42, etc    */
	int maxelem;	/* O - max number of elements that fit in buffer    */
	OFF_T startpos;	/* O - offset in file to starting row & column      */
	OFF_T elemnum;	/* O - starting element number ( 0 = 1st element)   */
	long incre;		/* O - byte offset between elements within a row    */
	OFF_T repeat;	/* O - number of elements in a row (vector column)  */
	OFF_T rowlen;	/* O - length of a row, in bytes                    */
	int hdutype;	/* O - HDU type: 0, 1, 2 = primary, table, bintable */
	LONGLONG tnull;	/* O - null value for integer columns               */
	char snull[20];	/* O - null value for ASCII table columns           */
};

/* HK item */
typedef struct {
	ASTE_HK *aste_hk;			/* list of file names */
	int current_file;			/* current position of aste_hk->filenames */
	char *data_name;			/* HK name */
	char *time_name;			/* TIME column name */

/* fits */
	fitsfile *fitsd;			/* fits file pointer */
	int found;					/* ANL_TRUE if hk file was found */
	int hdunum;					/* sequence number of the HDU */
	long nrows;					/* total row number */
	int data_col;				/* column position of HK item */
	int time_col;				/* column position of "TIME" column */
	double tstart, tstop;		/* File start and end time */
	int typecode;				/* HK data code */
	long repeat, width;			/* HK data type */
	struct column_info info;	/* fits column information */
	long irow;					/* current row which is reading */

/* pre-read for time & data column */
	double *time_buf;			/* time buffer */
	void *data_buf;				/* data buffer */
	long num_alloc;				/* number of allocated rows */
} HK_CONTENTS;

static int hk_num = 0;
static int hk_num_alloc = 0;
static HK_CONTENTS *hks = NULL;

/************************************************************************
	get_column_info()	wrapper to ffgcpr(), Get Column PaRameters

	RETURN VALUE
		0:		success
  		others:	CFITSIO error
*************************************************************************/
static int
get_column_info(fitsfile *fp, int colnum, long nelem, struct column_info *p)
{
	int istat = 0;
	long firstrow = 1;
	OFF_T firstelem = 1;
	int writemode = 0;		/* reading data */

	LONGLONG startpos, elemnum, repeat, rowlen;

	ffgcprll(fp, colnum, firstrow, firstelem, nelem, writemode,
		   &p->scale, &p->zero, p->tform, &p->twidth,
		   &p->tcode, &p->maxelem, &startpos, &elemnum,
		   &p->incre, &repeat, &rowlen, &p->hdutype,
		   &p->tnull, p->snull, &istat);

	p->startpos = startpos;
	p->elemnum = elemnum;
	p->repeat = repeat;
	p->rowlen = rowlen;

	return istat;
}

/************************************************************************
	fits_read_col_raw()		read raw column data to allocated memory

	RETURN VALUE
		0:		success
  		others:	CFITSIO error
*************************************************************************/
static int
fits_read_col_raw(fitsfile *fp, HK_CONTENTS *p)
{
	int istat;
	OFF_T readptr;
	long nrows, repeat, width, irow, nelem, ntodo, incre;
	void *buffptr;
    char message[256];

	nrows = p->nrows;
	repeat = p->repeat;
	width = p->width;
	nelem = nrows * repeat;

	istat = get_column_info(fp, p->data_col, nelem, &p->info);
	if ( istat ) {
		return istat;
	}

	ntodo = repeat;			/* number of elements in a row (vector column) */
	incre = width;			/* byte offset between elements within a row */

	for (irow = 1; irow <= nrows; irow++) {
/*
   limit the number of pixels to read at one time
   to the number of pixels that remain in the current vector
*/
        readptr = p->info.startpos + (irow - 1) * p->info.rowlen;
		buffptr = (unsigned char *)p->data_buf + (irow - 1) * repeat * width;

		switch ( p->typecode ) {
		case TLONG:
			ffgi4b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		case TLONGLONG:
			ffgi8b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		case TBYTE:
			ffgi1b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		case TSHORT:
			ffgi2b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		case TFLOAT:
			ffgr4b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		case TDOUBLE:
			ffgr8b(fp, readptr, ntodo, incre, buffptr, &istat);
			break;
		default:	/*  error trap for invalid column format */
			sprintf(message, "\
Cannot read numbers from column %d which has format %s",
				p->data_col, p->info.tform);
			fits_write_errmsg(message);
			if ( ASCII_TBL == p->info.hdutype ) {
				istat = BAD_ATABLE_FORMAT;
				return istat;
			} else {
				istat = BAD_BTABLE_FORMAT;
				return istat;
			}
        } /* End of switch block */

        /*-------------------------*/
        /*  Check for fatal error  */
        /*-------------------------*/
        if ( 0 < istat ) {	/* test for error during previous read operation */
			sprintf(message, "\
Error reading rown %ld from column %d (ffgcl).", irow,  p->data_col);
			fits_write_errmsg(message);
			return istat;
		}

    }  /*  End of main while Loop  */

	return istat;
}

/************************************************************************
	expand_mem()	expand HK_CONTENTS hks[]

	RETURN VALUE
		ANL_OK (0):		success
  		ANL_NG (-1):	memory allocation error
*************************************************************************/
static int
expand_mem(void)
{
	void *new_hks;

	new_hks = calloc(sizeof(HK_CONTENTS), hk_num_alloc+HK_CONTENTS_ALLOC_UNIT);
	if ( NULL == new_hks ) {
		anl_msg_error("\
%s-%s: cannot expand memory for HK\n", pname, version);
		return ANL_NG;
	}

	if ( NULL != hks ) {
		memcpy(new_hks, hks, sizeof(HK_CONTENTS) * hk_num_alloc);
		free(hks);
	}
	hks = new_hks;
	hk_num_alloc += HK_CONTENTS_ALLOC_UNIT;

	return ANL_OK;
}

/************************************************************************
	read_fits()		read fits specified column

	RETURN VALUE
		0:			success
  		others:		CFITSIO error or OVERFLOW_ERR
*************************************************************************/
static int
read_fits(
	HK_CONTENTS *p,		/* input: pointer to hks[id] */
	long irow,			/* input: row number to read */
	int typecode,		/* input: TINT or TDOUBLE */
	int nelem,			/* input: number of elements to read */
	double *aste_time,	/* output: Astro-E time */
	void *value			/* output: HK value, ignored when NULL */
	)
{
	double scale, zero;
	int istat, anynul;
	void *v;

	if ( irow < 1 ) {
		irow = 1;
	} else if ( p->nrows < irow ) {
		irow = p->nrows;
	}

	istat = 0;

	if ( 0 != p->time_buf ) {
		*aste_time = p->time_buf[irow-1];
	} else {
		fits_read_col_dbl(p->fitsd, p->time_col, irow, 1, 1, 0.0,
				aste_time, &anynul, &istat);
		if ( istat ) {
			return istat;
		}
	}

	if ( NULL == value ) {
		return 0;
	}

	if ( NULL == p->data_buf ) {
		fits_read_col(p->fitsd, typecode, p->data_col, irow, 1, nelem, NULL,
				value, &anynul, &istat);
		return istat;
	}

	zero = p->info.zero;
	scale = p->info.scale;
	v = (char *)p->data_buf + (irow - 1) * p->repeat * p->width;
	if ( 0.0 == zero && 1.0 == scale ) {
		switch (typecode) {
		case TDOUBLE:
		case TFLOAT:
		case TLONGLONG:
		case TSHORT:
		case TBYTE:
			if ( typecode == p->typecode ) {
				memcpy(value, v, p->width * nelem);
				return 0;
			}
			break;
		case TINT:
			if ( 4 == sizeof(int) && TLONG == p->typecode ) {
				memcpy(value, v, p->width * nelem);
				return 0;
			}
			break;
		case TLONG:
			if ( 4 == sizeof(long) && TLONG == p->typecode ) {
				memcpy(value, v, p->width * nelem);
				return 0;
			}
			if ( 8 == sizeof(long) && TLONGLONG == p->typecode ) {
				memcpy(value, v, p->width * nelem);
				return 0;
			}
			break;
		default:
			;
		}
	}

	switch (typecode) {
	case TDOUBLE:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8r8(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4r8(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8r8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4r8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2r8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1r8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TFLOAT:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8r4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4r4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8r4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4r4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2r4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1r4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TLONGLONG:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8i8(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4i8(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8i8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4i8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2i8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1i8(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TLONG:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8i4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4i4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8i4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4i4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2i4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1i4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TULONG:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8u4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4u4(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8u4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4u4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2u4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1u4(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TINT:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8int(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4int(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8int(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4int(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2int(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1int(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TUINT:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8uint(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4uint(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8uint(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4uint(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2uint(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1uint(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TSHORT:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8i2(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4i2(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8i2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4i2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2i2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1i2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TUSHORT:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8u2(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4u2(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8u2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4u2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2u2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1u2(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	case TBYTE:
		switch (p->typecode) {
		case TDOUBLE:
return fffr8i1(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TFLOAT:
return fffr4i1(v, nelem, scale, zero, 0, 0.0, NULL, &anynul, value, &istat);
			;
		case TLONGLONG:
return fffi8i1(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TLONG:
return fffi4i1(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TSHORT:
return fffi2i1(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		case TBYTE:
return fffi1i1(v, nelem, scale, zero, 0, 0, 0, NULL, &anynul, value, &istat);
			;
		default:
goto unknown_type;
			;
		}
		;
	default:
goto unknown_type;
		;
	}

 unknown_type:
	return BAD_DATATYPE;
}

/************************************************************************
	get_value()		get HK value at specifiled Astro-E time

	RETURN VALUE
		0:		found
		+1:		not found and request next file
		-1:		not found and request previous file
		others:	CFITSIO error or OVERFLOW_ERR
*************************************************************************/
static int
get_value(
	HK_CONTENTS *p,		/* input: pointer to hks[id] */
	double aste_time,	/* input: astroe time */
	int typecode,		/* input: TINT or TDOUBLE */
	long nelem,			/* input: number of elements to read */
	void *value,		/* output: HK value */
	double *stime,		/* output: time iroiro */
	int *sflag			/* output: flag whether stime is set */
	)
{
	double t0, t1;
	int istat = 0;

	istat = read_fits(p, p->irow, typecode, nelem, &t0, NULL);
	if ( istat ) {
		return istat;
	}
	t1 = t0;

	if ( t0 <= aste_time ) {
		while ( p->irow < p->nrows ) {
			istat = read_fits(p, p->irow, typecode, nelem, &t0, value);
			if ( istat ) {
				return istat;
			}
			istat = read_fits(p, p->irow+1, typecode, nelem, &t1, NULL);
			if ( istat ) {
				return istat;
			}
			if ( aste_time < t1 ) {
				goto found;
			}
			p->irow++;
		}
		if ( aste_time == t1 ) {	/* equal to last row */
			t0 = t1;
			istat = read_fits(p, p->irow, typecode, nelem, &t0, value);
			if ( istat ) {
				return istat;
			}
			goto found;
		}
		anl_msg_info("\
%s-%s: t=%.3f > TSTOP=%.3f for '%s'\n",
			pname, version, aste_time, p->tstop, p->data_name);
		if ( NULL != stime ) {
			if ( 0 == sflag[1] ) {
				stime[1] = t1;
				sflag[1]++;
			} else if ( stime[1] < t0 ) {
				stime[1] = t1;
			}
		}
		return +1;

	} else {
		p->irow--;
		while ( 1 <= p->irow ) {
			istat = read_fits(p, p->irow, typecode, nelem, &t0, value);
			if ( istat ) {
				return istat;
			}
			if ( t0 <= aste_time ) {
				goto found;
			}
			t1 = t0;
			p->irow--;
		}
		anl_msg_info("\
%s-%s: t=%.3f < TSTART=%.3f for '%s'\n",
			pname, version, aste_time, p->tstart, p->data_name);
		if ( NULL != stime ) {
			if ( 0 == sflag[2] ) {
				stime[2] = t1;
				sflag[2]++;
			} else if ( t1 < stime[2] ) {
				stime[2] = t1;
			}
		}
		return -1;
	}

 found:
	if ( NULL != stime ) {
		stime[0] = t0;
	}
	istat = read_fits(p, p->irow-1, typecode, nelem, &t0, NULL);
	if ( NULL != stime ) {
		stime[1] = t0;
		stime[2] = t1;
	}
	return istat;
}

/************************************************************************
	check_file()	check if current file include specified HK item

	RETURN VALUE
		ANL_OK (0):		found HK item
		ANL_NG (-1):	not found
		ASTE_GETHK_OUT_OF_GTI (-2):	out of GTI range
		others:			CFITSIO error
*************************************************************************/
static int
check_file(HK_CONTENTS *p, double aste_time, double *stime, int *sflag)
{
	fitsfile *fp;
	HK_CONTENTS *q;
	int ihk, istat, flag_gti_error, hdutype, anynul;
	char *filename;

/* check if HK item was already found */
	if ( ANL_TRUE == p->found ) {
		return ANL_OK;
	}

/* close old fits file if opened */
	if ( p->fitsd ) {
		for (ihk = 0; ihk < hk_num; ihk++) {
			q = &hks[ihk];
			if ( p == q ) continue;
			if ( p->fitsd == q->fitsd ) break;
		}
		if ( ihk == hk_num ) {
			fits_close_file(p->fitsd, &istat);
			if ( istat ) {
				anl_msg_error("\
%s-%s: fits_close_file() failed (%d)\n", pname, version, istat);
				return istat;
			}
		}
		p->fitsd = NULL;
	}

/* release allocated memory if allocated */
	if ( NULL != p->data_buf ) {
		free(p->data_buf);
		p->data_buf = NULL;
	}
	if ( NULL != p->time_buf ) {
		for (ihk = 0; ihk < hk_num; ihk++) {
			q = &hks[ihk];
			if ( p == q ) continue;
			if ( p->time_buf == q->time_buf ) break;
		}
		if ( ihk == hk_num ) {
			free(p->time_buf);
			p->time_buf = NULL;
		}
	}

/* open fits file */
	istat = 0;
	filename = p->aste_hk->filenames[p->current_file];
	fits_open_file(&fp, filename, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s-%s: HK file '%s' open failed (%d)\n", pname, version, filename, istat);
		return istat;
	}

	flag_gti_error = 0;
	for (p->hdunum = 2; 0 == istat; p->hdunum++) {
/* move to specified fits extension */
		fits_movabs_hdu(fp, p->hdunum, &hdutype, &istat);
		if ( istat ) {
			break;		/* cannot move to extension */
		}
/* read number of rows */
		fits_get_num_rows(fp, &p->nrows, &istat);
		if ( istat || 0 == p->nrows ) {
			istat = 0;
			continue;
		}
/* read column number for data */
		fits_get_colnum(fp, CASESEN, p->data_name, &p->data_col, &istat);
		if ( istat ) {
			istat = 0;
			continue;
		}
		anl_msg_info("\
%s-%s: found '%s' at hdu=%d, col=%d in '%s'\n",
			   pname, version, p->data_name, p->hdunum, p->data_col, filename);
/* read column type for data */
		fits_get_coltype(fp, p->data_col,
					&p->typecode, &p->repeat, &p->width, &istat);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_get_coltype('%s') failed in '%s' (%d)\n",
					pname, version, p->data_name, filename, istat);
			istat = 0;
			continue;
		}
/* read column number for time */
		fits_get_colnum(fp, CASESEN, p->time_name, &p->time_col, &istat);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_get_colnum('%s') failed in '%s' (%d)\n",
					pname, version, p->time_name, filename, istat);
			istat = 0;
			continue;
		}
/* read TSTART & TSTOP */
		fits_read_col_dbl(fp, p->time_col, 1, 1, 1, 0.0,
					&p->tstart, &anynul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_read_key_dbl('TSTART') failed in '%s' (%d)\n",
					pname, version, filename, istat);
			istat = 0;
			continue;
		}
		fits_read_col_dbl(fp, p->time_col, p->nrows, 1, 1, 0.0,
					&p->tstop, &anynul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_read_key_dbl('TSTOP') failed in '%s' (%d)\n",
					pname, version, filename, istat);
			istat = 0;
			continue;
		}
/* check time range */
		if ( aste_time < p->tstart ) {
			anl_msg_info("\
%s-%s: t=%.3f < TSTART=%.3f for '%s' in '%s'\n",
				pname, version, aste_time, p->tstart, p->data_name, filename);
			flag_gti_error = ASTE_GETHK_GTI_ERROR;
			if ( NULL != stime ) {
				if ( 0 == sflag[2] ) {
					stime[2] = p->tstart;
					sflag[2]++;
				} else if ( p->tstart < stime[2] ) {
					stime[2] = p->tstart;
				}
			}
			break;
		}
		if ( p->tstop < aste_time ) {
			anl_msg_info("\
%s-%s: t=%.3f > TSTOP=%.3f for '%s' in '%s'\n",
				pname, version, aste_time, p->tstop, p->data_name, filename);
			flag_gti_error = ASTE_GETHK_GTI_ERROR;
			if ( NULL != stime ) {
				if ( 0 == sflag[1] ) {
					stime[1] = p->tstop;
					sflag[1]++;
				} else if ( stime[1] < p->tstop ) {
					stime[1] = p->tstop;
				}
			}
			break;
		}
/* allocate pre-read memory for time if required */
		for (ihk = 0; ihk < hk_num; ihk++) {
			q = &hks[ihk];
			if ( p == q ) continue;
			if ( NULL != q->time_buf &&
				 p->aste_hk == q->aste_hk &&
				 p->current_file == q->current_file &&
				 p->hdunum == q->hdunum &&
				 p->nrows == q->nrows &&
				 p->time_col == q->time_col ) {
				p->time_buf = q->time_buf;
				break;
			}
		}
		if ( ihk == hk_num ) {
			p->time_buf = calloc(sizeof(double), p->nrows);
			if ( NULL == p->time_buf ) {
				goto direct_reading;
			}
			fits_read_col_dbl(fp, p->time_col, 1, 1, p->nrows, 0.0,
					p->time_buf, &anynul, &istat);
			if ( istat ) {
				anl_msg_error("\
%s-%s: fits_read_col('%s') failed in '%s' (%d)\n",
						pname, version, p->time_name, filename, istat);
				istat = 0;
				free(p->time_buf);
				p->time_buf = NULL;
				continue;
			}
		}

/* check if variable length column */
/* negative type code means variable length array */
		if ( p->typecode < 0 ) {
			goto direct_reading;
		}

/* allocate pre-read memory for data */
		p->data_buf = calloc(p->width, p->repeat * p->nrows);
		if ( NULL == p->data_buf ) {
			goto direct_reading;
		}

/* read data contents */
		istat = fits_read_col_raw(fp, p);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_read_col_raw('%s') failed in '%s' (%d)\n",
					pname, version, p->data_name, filename, istat);
			istat = 0;
			free(p->data_buf);
			p->data_buf = NULL;
			if ( ihk == hk_num ) {	/* time_buf allocated */
				free(p->time_buf);
				p->time_buf = NULL;
			}
			continue;
		}

/* close fits file */
		fits_close_file(fp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s-%s: fits_close_file() failed in '%s' (%d)\n",
					pname, version, filename, istat);
			return istat;
		}
		p->found = ANL_TRUE;
		p->irow = 1;
		return ANL_OK;
	}

/* not found */
	istat = 0;
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s-%s: fits_close_file() failed in '%s' (%d)\n",
				pname, version, filename, istat);
		return istat;
	}

	if ( flag_gti_error ) {
		return ASTE_GETHK_GTI_ERROR;
	}

	return ANL_NG;

 direct_reading:

/* direct reading file */
	p->fitsd = fp;
	p->found = ANL_TRUE;
	p->irow = 1;

	for (ihk = 0; ihk < hk_num; ihk++) {
		q = &hks[ihk];
		if ( p == q ) continue;
		if ( NULL != q->fitsd &&
			 p->aste_hk == q->aste_hk &&
			 p->current_file == q->current_file &&
			 p->hdunum == q->hdunum &&
			 p->nrows == q->nrows ) {
			p->fitsd = q->fitsd;
			fits_close_file(fp, &istat);
			if ( istat ) {
				anl_msg_error("\
%s-%s: fits_close_file() failed in '%s' (%d)\n",
					pname, version, filename, istat);
				return istat;
			}
			break;
		}
	}

	return ANL_OK;

}

/************************************************************************
	aste_gethk_init()	set HK file list

	RETURN VALUE
		NULL:	Error
		others:	pointer to ASTE_HK structure
*************************************************************************/
ASTE_HK *
aste_gethk_init(
	char *filelist		/* input: an HK file to read or
						          a list of HK files when start with "@" */
	)
{
	ASTE_HK *aste_hk;

	aste_hk = calloc(sizeof(ASTE_HK), 1);
	if ( NULL == aste_hk ) {
		anl_msg_error("\
%s-%s: aste_hk allocation failed\n", pname, version);
		return NULL;
	}

	if (filelist[0] == '@') {
/* file list */
		int i;
		FILE *fp;
		char line[MAX_LINE_LEN];

		anl_msg_info("\n\
%s-%s: reading filelist '%s'\n", pname, version, filelist+1);
		fp = fopen(filelist+1, "r");
		if (fp == NULL) {
			anl_msg_error("\
%s-%s: cannot open filelist '%s'\n", pname, version, filelist+1);
			return NULL;
		}
		aste_hk->num_file = 0;
		for (;;) {
			if ( NULL == fgets(line, sizeof(line), fp) ) {
				break;
			}
			/* strip trailing SPACES & linefeed */
			for (i = strlen(line); 0 < i && line[i-1] <= ' '; i--) {
				line[i-1] = '\0';
			}
			/* strip leadling SPACES & TABS */
			for (i = 0; '\0' != line[i] && line[i] <= ' '; i++) {
				;
			}
			if ( '\0' == line[i] ) {
				continue;
			}
			if ( ASTE_GETHK_MAX_HKFILE_NUM <= aste_hk->num_file ) {
				anl_msg_error("\
%s-%s: too many HK files. should be less than %d\n",
					 pname, version, ASTE_GETHK_MAX_HKFILE_NUM);
				goto quit;
			}
			anl_msg_info("\
  %d: %s\n", aste_hk->num_file+1,  line+i);
			aste_hk->filenames[aste_hk->num_file] = strdup(line + i);
			if ( NULL == aste_hk->filenames[aste_hk->num_file] ) {
				anl_msg_error("\
%s-%s: aste_hk->filenames[%d] allocation failed\n",
					 pname, version, aste_hk->num_file);
			quit:
				fclose(fp);
				for (i = 0; i < aste_hk->num_file; i++) {
					free(aste_hk->filenames[i]);
				}
				free(aste_hk);
				return NULL;
			}
			aste_hk->num_file++;
		}
		fclose (fp);
	} else {
/* direct file name */
		aste_hk->num_file = 1;
		aste_hk->filenames[0] = strdup(filelist);
		if ( NULL == aste_hk->filenames[0] ) {
			anl_msg_error("\
%s-%s: aste_hk->filenames[0] allocation failed\n", pname, version);
			free(aste_hk);
			return NULL;
		}
	}

	if ( 0 == aste_hk->num_file ) {
		free(aste_hk);
		return NULL;
	}

	return aste_hk;
}

/************************************************************************
	aste_gethk_id()		get id for specified HK name

	RETURN VALUE
		0 or positive values:		successs
		negative values (-1):		memory allocation error
*************************************************************************/
int
aste_gethk_id(
	ASTE_HK *aste_hk,	/* input: HK struct */
	char *hk_name		/* input: HK name */
	)
{
	int id;
	HK_CONTENTS *p;

/* check aste_hk */
	if ( NULL == aste_hk || 0 == aste_hk->num_file ) {
		return -1;
	}

/* check hk number */
	if ( hk_num_alloc <= hk_num && ANL_OK != expand_mem() ) {
		return -1;
	}
	p = &hks[hk_num];

/* initialize */
	p->data_name = strdup(hk_name);
	if ( NULL == p->data_name ) {
		return -1;
	}
	p->time_name = "TIME";		/* default time column name "TIME" */
	p->aste_hk = aste_hk;
	p->current_file = 0;
	p->found = ANL_FALSE;
	p->fitsd = NULL;
	p->time_buf = NULL;
	p->data_buf = NULL;

	id = hk_num;
	hk_num++;

	return id;
}

/************************************************************************
	aste_gethk()	return HK value

	RETURN VALUE
		ANL_OK (0):		success
		ANL_NG (-1):	not found / illegal HK id
		others:			CFITSIO error
*************************************************************************/
int
aste_gethk(
	int id,				/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	int typecode,		/* input: TINT, TDOUBLE, etc */
	int nelem,			/* input: number of elements to read */
	void *value,		/* output: HK value */
	double *stime		/* output: time iroiro
		   					stime[0]: time when value was set,
		   					stime[1]: previous time when HK is put,
		   					stime[2]: next time when HK is put
						*/
	)
{
	int ifile, istat, flag_gti_error, sflag[3];
	ASTE_HK *aste_hk;
	HK_CONTENTS *p = &hks[id];

/* check id number */
	if ( id < 0 || hk_num <= id ) {
		anl_msg_error("\
%s-%s: illegal HK item (id=%d) called\n", pname, version, id);
		return ANL_NG;
	}

	aste_hk = p->aste_hk;

	flag_gti_error = 0;
	sflag[0] = sflag[1] = sflag[2] = 0;

	for (ifile = 0; ifile < aste_hk->num_file; ifile++) {

/* check current file for current HK item */
		istat = check_file(p, aste_time, stime, sflag);
		if ( ANL_OK != istat ) {
			if ( ASTE_GETHK_GTI_ERROR == istat ) {
				flag_gti_error = ASTE_GETHK_GTI_ERROR;
			}
			p->current_file++;
			p->current_file %= aste_hk->num_file;
			p->found = ANL_FALSE;
			continue;
		}

/* read file contents */
		istat = get_value(p, aste_time, typecode, nelem, value, stime, sflag);
		if ( 0 == istat ) {							/* found */
			return ANL_OK;
		} else if ( 1 == istat || -1 == istat ) {	/* not found */
			flag_gti_error = ASTE_GETHK_GTI_ERROR;
			p->current_file++;
			p->current_file %= aste_hk->num_file;
		} else if ( OVERFLOW_ERR == istat ) {		/* numerical overflow */
			fits_write_errmsg("\
Numerical overflow during type conversion while reading FITS data.");
			istat = NUM_OVERFLOW;
			return istat;
		} else {									/* cfitsio error */
			return istat;
		}
		p->found = ANL_FALSE;

	}

	if ( flag_gti_error ) {
		return ASTE_GETHK_GTI_ERROR;
	}

	anl_msg_error("\
%s-%s: keyword '%s' at t=%.3f was not found in HK lists\n",
		pname, version, p->data_name, aste_time);

	return ANL_NG;	/* not found */
}

/************************************************************************
	aste_gethk_register()	register HK item to read (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	memory allocation error
*************************************************************************/
int
aste_gethk_register(
	ASTE_HK *aste_hk,	/* input: HK struct */
	char *hk_name,		/* input: HK name */
	int *id_ptr			/* output: ID of HK item */
	)
{
	int id;

	id = aste_gethk_id(aste_hk, hk_name);
	if ( id < 0 ) {
		return ANL_FALSE;
	}
	*id_ptr = id;

	return ANL_TRUE;
}

/************************************************************************
	aste_gethk_int()	return HK value with integer (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	not found / illegal HK id
*************************************************************************/
int
aste_gethk_int(
	ASTE_HK *aste_hk,	/* input: HK struct (not used) */
	char *hk_name,		/* input: HK name (not used) */
	int *id_ptr,		/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	int *value,			/* output: HK value */
	double *stime		/* output: time iroiro */
	)
{
	int istat;

	istat = aste_gethk(*id_ptr, aste_time, TINT, 1, value, stime);

	if ( ANL_OK != istat ) {
		return ANL_FALSE;
	}

	return ANL_TRUE;
}

/************************************************************************
	aste_gethk_double()	return HK value with double (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	not found / illegal HK id
*************************************************************************/
int
aste_gethk_double(
	ASTE_HK *aste_hk,	/* input: HK struct (not used) */
	char *hk_name,		/* input: HK name (not used) */
	int *id_ptr,		/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	double *value,		/* output: HK value */
	double *stime		/* output: time iroiro */
	)
{
	int istat;

	istat = aste_gethk(*id_ptr, aste_time, TDOUBLE, 1, value, stime);

	if ( ANL_OK != istat ) {
		return ANL_FALSE;
	}

	return ANL_TRUE;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
