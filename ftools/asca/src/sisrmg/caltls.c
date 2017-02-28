
/*
 filename:	caltls.c
 purpose:	support for the caltools world
 author:	Geoffrey B. Crew
 date:		January 1994
 modified:      Jeff Guerber, GSFC/HSTX, Apr. 1996. FCPKYS->ffpkys
                Jeff Guerber, GSFC/HSTX, Nov. 1996. HDUVERS2 updated; CBD10001
		      fixed for case where first channel is 0.
 */
#include "defs.h"

#define GENERIC	"Keyword information for Caltools Software.       "
#define N_WHITE	"                                                 "

#define PUT(K,V,C)	do {	/* REVISIT */			  \
				ffpkys(f_ounit, K, V, C, status); \
			} while(0)
static char	key[FITS_CLEN_KEYNAME],
		val[FITS_CLEN_KEYVAL],
		com[FITS_CLEN_COMMENT];

static char	descrip[120], n0[70],
		gradelist[15], n1[15],
		arf_info[12], n2[12];
static Point	*rqt;
static int	arf_included = 0, sta = 0, *status = &sta;

static void	str_init(), cal_cache(), do_matrix(), do_bounds(), do_etc();

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/caltls.c,v 3.10 1997/01/15 02:33:32 guerber Exp $";

/*
 *  Entry point to the caltools support.  Basically this
 *  gets called from the thinking routines with random bits
 *  of information, and by the MATRIX and EBOUNDS portions
 *  for the insertion of various keywords.
 */
void
cal_stuff(hdu, who, data, pt)
	int	hdu, who, data;
	Point	*pt;
{
	switch (hdu) {
		case 0:	cal_cache(who, data, pt);	break;
		case 1:	do_matrix();	do_etc();	break;
		case 2:	do_bounds();	do_etc();	break;
		default:				break;
	}
}

static void
cal_cache(who, data, pt)
	int	who, data;
	Point	*pt;
{
	char	*g;
	int	gm, i;

	switch (who) {
		case 'A':	arf_included = data;	break;
		case 'P':	rqt = pt;		break;
		default :				break;
	}
	if (!rqt) return;

	/*
	 *  Included ARFs are now deprecated.
	 */
	if (arf_included) strncpy(arf_info, " includedARF", 12);
	else		  strncpy(arf_info, "            ", 12);

	/*
	 *  Make up a comma-delimited grade-string.  Real crude.
	 *
	 *  No, now they want a bunch of characters.
	 */
	gm = (rmg_works.gmask & MSK_FTBTX) ? MSK_FTBT0 : MSK_FAST0;
	i = 0;
	g = gradelist;	*g++ = '"';
	do {
		if (gm & rmg_works.gmask) {
			*g++ = '0'+i; /* *g++ = ','; */
		}
		gm <<= 1;
		i++;
	} while (gm != MSK_FTBT7 && gm != MSK_FAST1);
	*g++ = '"';
	*g++ = ' ';	*g = '\0';

	/*
	 *  Make up a descriptive string.  Hope it's short enough.
	 */
	(void)sprintf(descrip,
		"SISRMGv%.2f:%dx%d S%dC%d G%s V%d P%d E%.1f %s%s",
		rqt->vers, rmg_works.e_bin, rmg_works.pcbin,
		(int)rqt->detr, (int)rqt->chip,
		gradelist,
		(int)rqt->evth, (int)rqt->spth,
		(100.0*rqt->echo),
		arf_info, N_WHITE);

	ERRCHK(*n0 && *n0 != ' ', "Descrip   overwrite %s\n", n0, 0);
	ERRCHK(*n1 && *n1 != ' ', "gradelist overwrite %s\n", n1, 0);
	ERRCHK(*n2 && *n2 != ' ', "arf_info  overwrite %s\n", n2, 0);
}

static void
do_matrix()
{
	static char	value[18], n0[20];

	PUT("HDUCLASS", "OGIP              ", GENERIC);
	PUT("HDUCLAS1", "RESPONSE          ", GENERIC);
	PUT("HDUVERS1", "1.0.0             ", GENERIC);

	PUT("HDUCLAS2", "RSP_MATRIX        ", GENERIC);
	PUT("HDUVERS2", "1.2.0             ", GENERIC);
	PUT("HDUCLAS3", (arf_included)
		      ? "SPECRESP MATRIX   "
		      : "DETECTOR          ", GENERIC);
	PUT("CCNM0001", (arf_included)
		      ? "MATRIX            "
		      : "MATRIX            ", GENERIC);

	PUT("CCLS0001", "CPF               ", GENERIC);
	PUT("CDTP0001", "DATA              ", GENERIC);
	PUT("CVSD0001", "20/02/93          ", GENERIC);
	PUT("CVST0001", "11/11/11          ", GENERIC);
	descrip[68] = '\0';   /* work around cfitsio bug */
	PUT("CDES0001", descrip,              N_WHITE);

	ERRCHK(*n0 && *n0 != ' ', "Do_matrix overwrite %s\n", n0, return);
}

static void
do_bounds()
{
	static char	value[18], n0[20];

	PUT("HDUCLASS", "OGIP              ", GENERIC);
	PUT("HDUCLAS1", "RESPONSE          ", GENERIC);
	PUT("HDUVERS1", "1.0.0             ", GENERIC);

	PUT("HDUCLAS2", "EBOUNDS           ", GENERIC);
	PUT("HDUVERS2", "1.2.0             ", GENERIC);
	PUT("CCNM0001", "EBOUNDS           ", GENERIC);

	PUT("CCLS0001", "CPF               ", GENERIC);
	PUT("CDTP0001", "DATA              ", GENERIC);
	PUT("CVSD0001", "20/02/93          ", GENERIC);
	PUT("CVST0001", "11/11/11          ", GENERIC);
	PUT("CDES0001", descrip,              N_WHITE);

	ERRCHK(*n0 && *n0 != ' ', "Do_bounds overwrite %s\n", n0, return);
}

static void
do_etc()
{
	static char	value[18], n0[20];

	(void)sprintf(value, "CHAN(%1d-%4d)      ", rmg_works.first,
		      (rmg_works.first + rmg_works.pcbin - 1) );
	PUT("CBD10001", value,		   GENERIC);

	(void)sprintf(value,
			"ENER(%3.1f-%4.1f)keV",
			rmg_works.e_min, rmg_works.e_max);
	PUT("CBD20001", value,		   GENERIC);

	(void)sprintf(value,
			"GRADE(%s)         ", gradelist);
	PUT("CBD30001", value,		   GENERIC);

	(void)sprintf(value,
			"RAWX(% 3d-% 3d)     ",
			(int)rqt->xcen - (int)rqt->xwid/2,
			(int)rqt->xcen + (int)rqt->xwid/2);
	PUT("CBD40001", value,		   GENERIC);

	(void)sprintf(value,
			"RAWY(% 3d-% 3d)     ",
			(int)rqt->ycen - (int)rqt->ywid/2,
			(int)rqt->ycen + (int)rqt->ywid/2);
	PUT("CBD50001", value,		   GENERIC);

	ERRCHK(*n0 && *n0 != ' ', "Do_bounds overwrite %s\n", n0, return);
}
