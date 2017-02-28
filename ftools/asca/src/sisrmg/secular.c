/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/secular.c,v 3.8 1997/04/02 00:24:34 guerber Exp $   */
/*                   */

/*
 filename:	secular.c
 purpose:	read SIS secular file(s)
 author:	Geoffrey B. Crew
 date:		July 1994
 updated:	February 1996
 modified:      Jeff Guerber GSFC/HSTX Apr. 1996. Convert to cfitsio.
 */
#ifdef SISECULAR
  typedef float Real;
# define ERRCHK(C,M,V,A)  do if(C){		\
		(void)fprintf(stderr,(M),(V));	\
		A;				\
	} while (0)
# include "fitsio.h"

#else
# include "defs.h"
#endif /* SISECULAR */


#define CHTR_CTI	1
#define CHTR_ECHO	2
#define CHTR_RDD	3
#define CHTR_FANO	4

static int	stts, *status = &stts;

static double	inv_gain;
static Real	c1[2], c2[2], c3[2], c4[2], c5[2], c6[2];
static Real	ek[2];
static Real	rf[2], rx[2], rs[2], rp[2], rq[2];
static Real	fn[2];

static void	find_ext(), interpolate(), cal_chatter();
static double	ph2pi(), ph2ph();

static fitsfile  *fc;

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/secular.c,v 3.8 1997/04/02 00:24:34 guerber Exp $";

/*
 *  Possibly this can become a separate routine.
 *  But at the moment its use is still in flux.
 */
#include "rddsub.c"

/*
 *  Lookup some RDD parameters.  Use a calibration file `later'.
 */
void
get_rddis(efile, epoch, calep, sisn, mode, frac, xone, dsig2, qpeak, qmean
#ifdef SISECULAR
	, chatr
#endif /* SISECULAR */
)
	char	*efile;
	double	epoch, calep;
	int	sisn, mode;
	Real	*frac, *xone, *dsig2, *qpeak, *qmean;
{
    int		coln;

    /* mode == 1 2 or 4 maps into column number below */
    /* this also picks up the default 3CCD mode of no */
    /* RDD distribution included */
    switch (mode) {
	case 1:	coln =  1 + 5*sisn;	break;
	case 2:	coln = 11 + 5*sisn;	break;
	case 4:	coln = 21 + 5*sisn;	break;
	default:	/* bail out */
		*frac = *qpeak = *qmean = *dsig2 = 0.0;
		*xone = 5.0;
		return;
    }

    /* FORMAT IS YET TO BE DETERMINED */
    if (efile && *efile && *efile != ' ') {
	static double	time[100];
	static Real	data[100];
	int		rows = 0, any = 0;

	find_ext(efile, "RDD_T1", &rows);
	if (*status) { *dsig2 = -1; return; }	/* yell and scream */

	ffgcvd( fc, 1, 1l, 1l, (long)rows, 0.0, time, &any, status );

	ffgcve( fc, ++coln, 1l, 1l, (long)rows, 0.0f, data, &any, status );
	interpolate(calep, rf+0,  time, data, 1, rows);
	interpolate(epoch, frac,  time, data, 1, rows);

	ffgcve( fc, ++coln, 1l, 1l, (long)rows, 0.0f, data, &any, status );
	interpolate(calep, rs+0,  time, data, 1, rows);
	interpolate(epoch, rs+1,  time, data, 1, rows);

	ffgcve( fc, ++coln, 1l, 1l, (long)rows, 0.0f, data, &any, status );
	interpolate(calep, rx+0,  time, data, 1, rows);
	interpolate(epoch, xone,  time, data, 1, rows);

	ffgcve( fc, ++coln, 1l, 1l, (long)rows, 0.0f, data, &any, status );
	interpolate(calep, rp+0,  time, data, 1, rows);
	interpolate(epoch, qpeak, time, data, 1, rows);

	ffgcve( fc, ++coln, 1l, 1l, (long)rows, 0.0f, data, &any, status );
	interpolate(calep, rq+0,  time, data, 1, rows);
	interpolate(epoch, qmean, time, data, 1, rows);

	ffclos( fc, status );

    } else {		/* generate some numbers */
    /*
     *  For now, just spit out some numbers, loosely based on calibration
     *  report 22/2/95 by Dotani&Yamashita, as interpreted by Arasmus.
     */
	static double	fscale[5] = { 0, 0.50, 0.75, 0, 1.0 };
	static double	xscale[5] = { 0, 0.25, 0.50, 0, 1.0 };
	static double	s_o[2][5] = { 0, 5.0, 5.30, 0, 5.6,
			      0, 5.0, 5.15, 0, 5.3 };
	static double	s_s[2][5] = { 0, .6*3.65/7., .58*3.65/7., 0, .55*3.65/7.,
			      0, .4*3.65/7., .50*3.65/7., 0, .60*3.65/7. };
	static double	f_rate[2] = { -360./365., -335./365.};
	static double	x1at1y[2] = { 365.*22./700., 365.*27./700. };

	static double	secyear = 31536000;	/* secs/(year~365.00d) */
	static double	slaunch = 4327200;	/* Feb 2 1993, 2:00 UT */
	double		years = (epoch - slaunch) / secyear;
	double		yrcal = (calep - slaunch) / secyear;

	*xone = xscale[mode] * x1at1y[sisn] * years;
	rx[0] = xscale[mode] * x1at1y[sisn] * yrcal;
	*frac = fscale[mode] * (1.0 - exp(f_rate[sisn] * years));
	rf[0] = fscale[mode] * (1.0 - exp(f_rate[sisn] * yrcal));

	rs[1] = s_o[sisn][mode] + s_s[sisn][mode] * years;
	rs[0] = s_o[sisn][mode];

	/* figure out possible values of qzero */
	rdd_qzero((float)rs[1], *xone, *frac, qpeak, qmean);
	rdd_qzero((float)rs[0], rx[0], rf[0], rp+0,  rq+0 );
    }

    rf[1] = *frac;  rx[1] = *xone;
    rp[1] = *qpeak; rq[1] = *qmean;

    *dsig2 = rs[1]*rs[1] - rs[0]*rs[0];
    if (*dsig2 < 0.0) *dsig2 = 0.0;
#ifdef SISECULAR
    if (chatr) cal_chatter(CHTR_RDD, epoch, calep);
#else
    if (rmg_works.chatr & C_CAL) cal_chatter(CHTR_RDD, epoch, calep);
#endif /* SISECULAR */
}

/*
 *  Lookup the echo parameter appropriate to the epoch.
 *  On failure, do nothing and assume sensible defaults are in place.
 */
void
get_echos(efile, epoch, sisn, echop
#ifdef SISECULAR
	, chatr
#endif /* SISECULAR */
)
	char	*efile;
	double	epoch;
	int	sisn;
	Real	*echop;
{
	static double	time[100];
	static Real	echo[100];
	int		rows = 0, any = 0;

	find_ext(efile, "ECHO_T1", &rows);
	if (*status) { *status = 0; return; }

	ffgcvd( fc, 1, 1l, 1l, (long)rows, 0.0, time, &any, status );
	ffgcve( fc, 2+sisn, 1l, 1l, (long)rows, 0.0f, echo, &any, status );

	interpolate(epoch, echop,  time, echo, 1, rows);
	ek[1] = *echop;

	ffclos( fc, status );

#ifdef SISECULAR
	if (chatr) cal_chatter(CHTR_ECHO, epoch, 0.0);
#else
	if (rmg_works.chatr & C_CAL) cal_chatter(CHTR_ECHO, epoch, 0.0);
#endif /* SISECULAR */
}

/*
 *  Lookup the PH to PI transformation appropriate to the epoch.
 *  On failure, do nothing and assume sensible defaults are in place.
 */
void
get_ph2pi(pfile, epoch, calep, sisn, chip, xform
#ifdef SISECULAR
	, sispi, chatr
#endif /* SISECULAR */
)
	char	*pfile;
	double	epoch, calep;
	int	sisn, chip;
	double	(**xform)();
{
	static double	time[600];
	static Real	data[600];
	static char	com[49];
	int		rows = 0, any = 0;
	long            anyl;

	find_ext(pfile, "PH2PI_T1", &rows);
	if (*status) { *status = 0; return; }

	ffgkyd( fc, "NOM_GAIN", &inv_gain, com, status );
	inv_gain = 1.0/inv_gain;
	ffgkyj( fc, "XFORMTYP", &anyl, com, status ); any = anyl;    /*why?*/

	ffgcvd( fc, 1, 1l, 1l, (long)rows, 0.0, time, &any, status );

	ffgcve( fc, 2+4*sisn+chip, 1l, 1l, 6l*(long)rows, 0.0f, data,
		&any, status );

	interpolate(calep, c1 + 0, time, data + 0, 6, rows);
	interpolate(calep, c2 + 0, time, data + 1, 6, rows);
	interpolate(calep, c3 + 0, time, data + 2, 6, rows);
	interpolate(calep, c4 + 0, time, data + 3, 6, rows);
	interpolate(calep, c5 + 0, time, data + 4, 6, rows);
	interpolate(calep, c6 + 0, time, data + 5, 6, rows);

	interpolate(epoch, c1 + 1, time, data + 0, 6, rows);
	interpolate(epoch, c2 + 1, time, data + 1, 6, rows);
	interpolate(epoch, c3 + 1, time, data + 2, 6, rows);
	interpolate(epoch, c4 + 1, time, data + 3, 6, rows);
	interpolate(epoch, c5 + 1, time, data + 4, 6, rows);
	interpolate(epoch, c6 + 1, time, data + 5, 6, rows);
#ifdef SISECULAR
	if (chatr) cal_chatter(CHTR_CTI, epoch, calep);
#else
	if (rmg_works.chatr & C_CAL) cal_chatter(CHTR_CTI, epoch, calep);
#endif /* SISECULAR */

	ffclos( fc, status );

	if (*status) return;
#ifdef SISECULAR
	*xform = (sispi) ? ph2pi : ph2ph;
#else
	*xform = (rmg_works.sispi) ? ph2pi : ph2ph;
#endif /* SISECULAR */
}

/*
 *  Lookup the echo parameter appropriate to the epoch.
 *  On failure, do nothing and assume sensible defaults are in place.
 */
void
get_nucti(efile, epoch, sisn, fanof
#ifdef SISECULAR
	, chatr
#endif /* SISECULAR */
)
	char	*efile;
	double	epoch;
	int	sisn;
	Real	*fanof;
{
	static double	time[100];
	static Real	fano[100];
	int		rows = 0, any = 0;

	find_ext(efile, "NUCTI_T1", &rows);
	if (*status) { *status = 0; *fanof = 0.0; return; }

	ffgcvd( fc, 1, 1l, 1l, (long)rows, 0.0, time, &any, status );
	ffgcve( fc, 2+sisn, 1l, 1l, (long)rows, 0.0f, fano, &any, status );

	interpolate(epoch, fanof,  time, fano, 1, rows);
	fn[1] = *fanof;

	ffclos( fc, status );

#ifdef SISECULAR
	if (chatr) cal_chatter(CHTR_FANO, epoch, 0.0);
#else
	if (rmg_works.chatr & C_CAL) cal_chatter(CHTR_FANO, epoch, 0.0);
#endif /* SISECULAR */
}


/*
 *  Commentary on the PI transform data
 */
static void
cal_chatter(who, epoch, calep)
	int	who;
	double	epoch, calep;
{
    switch (who) {
    case CHTR_CTI:
	ERRCHK(1, "PI coeffs of calibration epoch = %.3le:\n", calep, 0);
	ERRCHK(1, "\tC1 = %.3le\t", c1[0], 0);
	ERRCHK(1,   "C2 = %.3le\t", c2[0], 0);
	ERRCHK(1,   "C3 = %.3le\n", c3[0], 0);
	ERRCHK(1, "\tC4 = %.3le\t", c4[0], 0);
	ERRCHK(1,   "C5 = %.3le\t", c5[0], 0);
	ERRCHK(1,   "C6 = %.3le\n", c6[0], 0);
	ERRCHK(1, "PI coeffs of requested epoch = %.3le:\n", epoch, 0);
	ERRCHK(1, "\tC1 = %.3le\t", c1[1], 0);
	ERRCHK(1,   "C2 = %.3le\t", c2[1], 0);
	ERRCHK(1,   "C3 = %.3le\n", c3[1], 0);
	ERRCHK(1, "\tC4 = %.3le\t", c4[1], 0);
	ERRCHK(1,   "C5 = %.3le\t", c5[1], 0);
	ERRCHK(1,   "C6 = %.3le\n", c6[1], 0);
	break;

    case CHTR_ECHO:
	ERRCHK(1, "Echo value at requested epoch = %.3le:\n", epoch, 0);
	ERRCHK(1, "\tEcho = %.3le\n", ek[1], 0);
	break;

    case CHTR_RDD:
	ERRCHK(1, "RDD parameters at calibration epoch = %.3le:\n", calep, 0);
	ERRCHK(1, "\tFrac  = %.3le\t",  rf[0], 0);
	ERRCHK(1,   "Xone  = %.3le\t",  rx[0], 0);
	ERRCHK(1,   "Sigma = %.3le\n",  rs[0], 0);
	ERRCHK(1, "\tQpeak = %.3le\t",  rp[0], 0);
	ERRCHK(1,   "Qmean = %.3le\t",  rq[0], 0);
	ERRCHK(1,   "F * X = %.3le\n",  -rf[0]*rx[0], 0);
	ERRCHK(1, "RDD parameters at requested epoch = %.3le:\n", epoch, 0);
	ERRCHK(1, "\tFrac  = %.3le\t",  rf[1], 0);
	ERRCHK(1,   "Xone  = %.3le\t",  rx[1], 0);
	ERRCHK(1,   "Sigma = %.3le\n",  rs[1], 0);
	ERRCHK(1, "\tQpeak = %.3le\t",  rp[1], 0);
	ERRCHK(1,   "Qmean = %.3le\t",  rq[1], 0);
	ERRCHK(1,   "F * X = %.3le\n",  -rf[1]*rx[1], 0);
	break;

    case CHTR_FANO:
	ERRCHK(1, "Fano value at requested epoch = %.3le:\n", epoch, 0);
	ERRCHK(1, "\tFano = %.3le eV\n", fn[1], 0);
	break;

    default:
    	break;
    }
}

/*
 *  ph2pi transform routine; generalized to double ph/pi.
 */
static double
ph2pi(ph, x, y)
	double	ph, x, y;
{
	double	pi;

	pi = (  ( ph ) *
		( c1[0] + x * c2[0] + y * c3[0] ) +
		( c4[0] + x * c5[0] + y * c6[0] )
	) * inv_gain;

	return(pi);
}

/*
 *  ph2ph transform routine:  assuming valid PI
 *  transformation, then PI(PH(t1)) == PI(PH(t2)).
 */
static double
ph2ph(ph1, x, y)
	double	ph1, x, y;
{
	double	ph2;

	ph2 = ( (( ph1 ) *
		 ( c1[0] + x * c2[0] + y * c3[0] ) +
		 ( c4[0] + x * c5[0] + y * c6[0] )
		) - ( c4[1] + x * c5[1] + y * c6[1] )
	      )  /  ( c1[1] + x * c2[1] + y * c3[1] );

	return(ph2);
}

/*
 *  Find an extension by name.  (Probably obsolete.)
 *  Result returned by *status;
 */
static void
find_ext(file, name, rows)
	char	*file, *name;
	int	*rows;
{
	static char	val[FITS_CLEN_KEYVAL], com[FITS_CLEN_COMMENT];
	int		ext, blk = 0, hdu = 0;
	long            rowsl;

	ffopen( &fc, file, 0, status );

	for (ext = 1; ; ext++) {
	  ffmahd( fc, ext, &hdu, status );
	  if (*status) break;
	  ffgkys( fc, "EXTNAME", val, com, status );
	  if (!*status && !strncmp(name, val, strlen(name))) break;
	  else *status = 0;
	}
	ffgkyj( fc, "NAXIS2  ", &rowsl, com, status ); *rows = rowsl;
}

/*
 *  Interpolate between (or extrapolate beyond) rows of the data.
 */
static void
interpolate(epoch, datap, time, data, dimd, nrows)
	double	epoch, *time;
	Real	*datap, *data;
	int	nrows, dimd;
{
	register int	i, j;

	for (i = 1; i < nrows; i++)
		if (epoch < time[i]) break;

	if (i == nrows) i--;	/* extrapolate after time[nrows] */
	j = i - 1;

	*datap = data[j*dimd] + (data[i*dimd] - data[j*dimd]) *
		((epoch - time[j]) / (time[i] - time[j]));
}
