/*
 SimASTE_XRSevtFitsWrite.c
   SimASTE module : Photon Reader

  1998-02-24 version 1.00	Y.ISHISAKI
		coded first

  1998-09-10 version 1.10	Y.ISHISAKI
		work with ftools parameter interface
		change FITS file keywords

  1998-Nov   version 1.11       Ning Gan
		Added WCS, Date/Time, mission keywords.
		Added X,Y, ROLL, columns
		Added GTI extensions.
		Added TLMIN, TLMAX and WCS keywords
		Added routine WriteWCSkeys
		put the task name into the FTOOL common block.

  1998-11-23 version 1.12       Ken Ebisawa
		* FOCXY and XRSXY definitions back to the original ones
		  by Ishisaki and Ueda
		* X and Y columns 'I' format, and ROLL column 'E' format

  1998-12-21 version 1.13       Ken Ebisawa and Ning Gan
		* Check the fitsio status after ffpclk call for detx, dety,
		  x and y, so that if it is 412 (overflow the range), reset to zero.
		  In that case, system dependent maximum or minimum value in the range
		  is written in the file (for example, -32768 or 32767 on some PCs).
		* x and y are casted to int (it used to be long, though they are
		  defined as int).

  1999-01-22 version 1.20	Y.ISHISAKI
	Renamed to SimASTE_XRSeventFITSwrite.c (formerly asteEventFITSwrite.c)
	Add FLAG_SECONDARY, FLAG_MIDRES, FLAG_MIDRES column
	Read TSTART, TSTOP, EXPOSURE, XRSFILTER keywords from BNK

  2003-11-07 version 1.40	Y.ISHISAKI
	Add OBS_MODE=DEFAULT & DATAMODE=DEFAULT in writeEvtkeys()
	Get TLMIN, TLMAX keywords from BNK
	Get OPTIC, TCRPX, TCRVL, TCDLT keywords using aste_coord

  2004-02-10 version 1.41	Y.ISHISAKI
	fix header values of TCDLT keywords of X/Y
	FLAG_SECONDARY/MIDRES/MIDRES tform changed from 1X -> 1B

  2004-05-14 version 1.50	Y.ISHISAKI
	fix TCDLT for Sky X (should be negative value) & TCTYP (should be RA---TAN)

  2006-04-09 version 1.6	Y.ISHISAKI
	rename SimASTE_XRSeventFITSwrite -> SimASTE_XRSevtFitsWrite
	increase filename[256] -> [1024]

  2006-08-05 version 2.2	Y.ISHISAKI
	support GTI
	set DATAMODE='STANDARD', OBS_MODE='POINTING'
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	rename c_timestamp() -> write_timestamp(), change format
	add write_random_number_info()
	rename com.filename -> com.outfile
	write GEOMAREA, TELDEF, LEAPFILE keywords
	remove TIME-OBS, TIME-END keywords, DATE-OBS, DATE-END include time
	call getlogin(), getenv() to get OBSERVER keyword in _write_std_keys()
	BnkPut SimASTE:N_DETECT, SimASTE:N_WEISUM in _ana()
	write N_PHOTON, N_DETECT, N_WEISUM keywords in _write_photon_detect_info()
	ignore events when istat=NUM_OVERFLOW (412) in _ana()

  2006-10-18 version 2.3	Y.ISHISAKI
	fix unit "sec" -> "s" for "PHOTON_TIME" column
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "fitsio.h"
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_time.h"
#include "aste_rand.h"
#include "aste_gti.h"
#include "aeFitsHeaderUtil.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_XRSevtFitsWrite";
char SimASTE_XRSevtFitsWrite_version[] = "version 2.3";

/* followings are examples of input paraeters for asteEventFITSwrite */
static struct {
	fitsfile *fp;
	int clobber;
	char outfile[PIL_LINESIZE];
	char *telescop, *instrume, *filter;
	long nrow;
	double n_detect, n_weisum;
	int (*prev_write_history)(fitsfile *);
} com = {
	NULL,				/* fp */
	1,					/* clobber */
	"xrs.evt",			/* outfile */
	NULL, NULL,	NULL,	/* telescop, instrume, filter */
	0L,					/* nrow */
	0.0, 0.0,			/* n_detect, n_weisum */
	NULL				/* prev_write_history */
};

#define NCOL 21
static char *ttype[NCOL] = {
	"TIME", "PHA", "PI", "PIXEL", "DETX", "DETY", "X","Y","ROLL",
	"FLAG_SECONDARY", "FLAG_MIDRES", "FLAG_LOWRES",
	"XRSX", "XRSY", "FOCX", "FOCY", "WEIGHT",
	"PHOTON_TIME", "PHOTON_ENERGY", "RA", "DEC"
};

static char *tform[NCOL] = {
	"1D", "1I", "1I", "1I", "1I", "1I", "1I", "1I","1E",
	"1B", "1B", "1B",
	"1E", "1E", "1E", "1E", "1E",
	"1D", "1D", "1D", "1D"
};

static char *tunit[NCOL] = {
	"s", "chan", "chan", "", "pixel", "pixel", "pixel", "pixel", "deg",
	"", "", "",
	"pixel", "pixel", "pixel", "pixel", " ",
	"s", "keV", "deg", "deg"
};

static struct {
	int ti, ph, pi, pixel, detx, dety, x, y, roll;
	int fs, fm, fl;
	int xrsx, xrsy, focx, focy, weight;
	int photon_time, energy, ra, dec;
} col = {
	1, 2, 3, 4, 5, 6, 7, 8, 9,
	10, 11, 12,
	13, 14, 15, 16, 17,
	18, 19, 20, 21
};

AE_STD_KEYS stdkeys;

static int
write_history(fitsfile *fp)
{
	int istat;
	char history[PIL_LINESIZE + FLEN_VALUE];

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	sprintf(history, "\
  outfile='%s'", com.outfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  clobber=%s", com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static void
MSG(char *format, ...)
{
	FILE *fp = stdout;
	va_list args;
	va_start(args, format);
	if ( '!' == *format ) {
		vfprintf(fp, format+1, args);
	} else {
		vfprintf(fp, format, args);
		fputc('\n', fp);
	}
	va_end(args);
	if ( isatty(fileno(fp)) ) fflush(fp);
}

static void
show_parameter(char *title)
{
	MSG("");
	MSG(title, pname);
	MSG("");

	MSG("%4s%-20s'%s'", "", "OUTFILE", com.outfile);
	MSG("%4s%-20s%s", "", "CLOBBER", com.clobber ? "YES" : "NO");
}

static int
write_wcs_keys(fitsfile *fp)
{
	int i, used, ival;
	double dval, xch, ych, xmm, ymm;
	char maxkey[FLEN_KEYWORD];
	char minkey[FLEN_KEYWORD];
	char keynam[FLEN_KEYWORD];
	SKYREF skyref;
	TELDEF *teldef;
	TELDEF_ASTROE *p;
	AtEulerAng ea;
	int istat = 0;

	BnkGet("SimASTE:TELDEF", sizeof(teldef), &used, &teldef);
	p = teldef->mission.aste;

/* TLMIN, TLMAX */
	i = col.ph;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:PHA:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for PHA", &istat);
	BnkGet("SimASTE:PHA:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for PHA", &istat);

	i = col.pi;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:PI:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for PI", &istat);
	BnkGet("SimASTE:PI:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for PI", &istat);

	i = col.pixel;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:PIXEL:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for PIXEL", &istat);
	BnkGet("SimASTE:PIXEL:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for PIXEL", &istat);

	i = col.detx;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:DETX:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for DETX", &istat);
	BnkGet("SimASTE:DETX:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for DETX", &istat);

	i = col.dety;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:DETY:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for DETY", &istat);
	BnkGet("SimASTE:DETY:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for DETY", &istat);

	i = col.x;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:SKYX:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for X", &istat);
	BnkGet("SimASTE:SKYX:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for X", &istat);

	i = col.y;
	fits_make_keyn("TLMIN", i, minkey, &istat);
	fits_make_keyn("TLMAX", i, maxkey, &istat);
	BnkGet("SimASTE:SKYY:TLMIN", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, minkey, &ival, "\
minimum legal value for Y", &istat);
	BnkGet("SimASTE:SKYY:TLMAX", sizeof(ival), &used, &ival);
	fits_write_key(fp, TINT, maxkey, &ival, "\
maximum legal value for Y", &istat);

/* WCS keywords */
	BnkGet("SimASTE:EULER", sizeof(ea), &used, &ea);
	BnkGet("SimASTE:SKYREF", sizeof(skyref), &used, &skyref);

	i = col.x;
	aste_det2sky(teldef, &ea, &skyref, p->optaxisx, p->optaxisy, &xch, &ych);
	dval = xch;
	fits_make_keyn("OPTIC", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Optical axis X coordinate (pixel)", &istat);
	dval = p->sky.xcen;
	fits_make_keyn("TCRPX", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Sky X axis reference pixel", &istat);
	dval = skyref.alpha;
	fits_make_keyn("TCRVL", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Sky X coordinate at reference pixel", &istat);
	fits_make_keyn("TCTYP", i, keynam, &istat);
	fits_write_key_str(fp, keynam, "RA---TAN", "\
Coordinate projection", &istat);
	dval = - (p->sky.xscl / p->focallen) * RAD2DEG;	/* should be negative */
	fits_make_keyn("TCDLT", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 7, "\
Sky X pixel scale (deg/pixel)", &istat);

	i = col.y;
	aste_det2sky(teldef, &ea, &skyref, p->optaxisx, p->optaxisy, &xch, &ych);
	dval = ych;
	fits_make_keyn("OPTIC", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Optical axis Y coordinate (pixel)", &istat);
	dval = p->sky.ycen;
	fits_make_keyn("TCRPX",i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Sky Y axis reference pixel", &istat);
	dval = skyref.delta;
	fits_make_keyn("TCRVL", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Sky Y coordinate at reference pixel", &istat);
	fits_make_keyn("TCTYP", i, keynam, &istat);
	fits_write_key_str(fp, keynam, "DEC--TAN", "\
Coordinate projection", &istat);
	dval = (p->sky.yscl / p->focallen) * RAD2DEG;
	fits_make_keyn("TCDLT", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 7, "\
Sky Y pixel scale (deg/pixel)", &istat);

	i = col.detx;
	dval = p->optaxisx;
	fits_make_keyn("OPTIC", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Optical axis DETX coordinate (pixel)", &istat);
	dval = p->det.xcen;
	fits_make_keyn("TCRPX", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETX reference pixel", &istat);
	aste_det_ch2mm(teldef, p->det.xcen, p->det.ycen, &xmm, &ymm);
	dval = xmm;
	fits_make_keyn("TCRVL", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETX reference pixel value (mm)", &istat);
	dval = p->det.xscl;
	fits_make_keyn("TCDLT", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETX pixel scale (mm/pixel)", &istat);

	i = col.dety;
	dval = p->optaxisy;
	fits_make_keyn("OPTIC", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
Optical axis DETY coordinate (pixel)", &istat);
	dval = p->det.ycen;
	fits_make_keyn("TCRPX", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETY reference pixel", &istat);
	aste_det_ch2mm(teldef, p->det.xcen, p->det.ycen, &xmm, &ymm);
	dval = ymm;
	fits_make_keyn("TCRVL", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETY reference pixel value (mm)", &istat);
	dval = p->det.yscl;
	fits_make_keyn("TCDLT", i, keynam, &istat);
	fits_write_key_fixdbl(fp, keynam, dval, 6, "\
DETY pixel scale (mm/pixel)", &istat);

	if ( istat ) {
		anl_msg_error("\
%s: Error in write_wcs_keys()\n", pname);
		fits_report_error(stderr,istat);
		return istat;
	}

	return istat;
}

void
SimASTE_XRSevtFitsWrite_startup(int *status)
{
	;
}

void
SimASTE_XRSevtFitsWrite_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"OUTFILE",
		"CLOBBER",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Set output file name",
		"When set, overwrite output file if exists",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	char *k;

	if ( *status ) {	/* ftools */
		if (
PILGetFname(k="outfile", com.outfile) ||
PILGetBool (k="clobber", &com.clobber) ||
			 0 ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("OUTFILE", key) ) {
			CLtxtrd(key, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("CLOBBER", key) ) {
			CLlogrd(key, &com.clobber);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_XRSevtFitsWrite_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	static int bitpix = 8;
	static int naxis = 0;
	static long naxes[1] = { 0 };
	static int type = BINARY_TBL;
	static char extname[] = "EVENTS";

	char *k, *leapfile;
	TELDEF *teldef;
	double geomarea;
	int used, morekeys;
	fitsfile *fp;
	int istat = 0;

	EvsDef("SimASTE_XRSevtFitsWrite:BEGIN");
	EvsDef("SimASTE_XRSevtFitsWrite:ENTRY");
	EvsDef("SimASTE_XRSevtFitsWrite:OK");

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	BnkGet("SimASTE:TELESCOP:PTR", sizeof(com.telescop), &used, &com.telescop);
	BnkGet("SimASTE:INSTRUME:PTR", sizeof(com.instrume), &used, &com.instrume);
	BnkGet("SimASTE:FILTER:PTR", sizeof(com.filter), &used, &com.filter);

	aeSetDefaultKeywordValues(&stdkeys);
	stdkeys.telescop = com.telescop;
	stdkeys.instrume = com.instrume;
	stdkeys.filter = com.filter;
	stdkeys.obs_mode = "POINTING";
	stdkeys.datamode = "STANDARD";
	stdkeys.object = "XRS-Simulation";
	stdkeys.timedel  = 5.086263021E-6;	/* (2^-12)/3/16 */
    /* XRS time resolution set to 5.08626302E-6 sec Ken Ebisawa 1998-11-23 */

	if ( com.clobber ) {
		unlink(com.outfile);
	}

	fits_create_file(&fp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: '%s' create failed\n", pname, com.outfile);
		goto quit;
	}

	if (
fits_create_img(fp, bitpix, naxis, naxes, &istat) ||
fits_create_tbl(fp, type, 0, NCOL, ttype, tform, tunit, extname, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: '%s' binary table create failed\n", pname, com.outfile);
		goto quit;
	}

	/* write standard keywords */
	if ( SimASTE_write_std_keys(fp, &stdkeys) ||
	/* write the WCS, Tlmin and Tlmax keywords */
		 write_wcs_keys(fp) ||
		 0 ) {
		goto quit;
	}

	BnkGet("SimASTE:GEOMAREA", sizeof(geomarea), &used, &geomarea);
	fits_write_key_fixdbl(fp, k="GEOMAREA", geomarea, 4,
		"geometrical area of XRT (cm2)", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key_fixdbl('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	BnkGet("SimASTE:TELDEF", sizeof(teldef), &used, &teldef);
	fits_write_key_str(fp, k="TELDEF", teldef->filename,
		"name of the telescope definition file", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key_str('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	BnkGet("SimASTE:LEAPFILE:PTR", sizeof(leapfile), &used, &leapfile);
	fits_write_key_str(fp, k="LEAPFILE", aefits_basename(leapfile),
		"name of the leap second file", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key_str('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	if ( aefits_write_module_history(fp, pname) ||
		 write_history(fp) ||
		 0 ) {
		goto quit;
	}

	morekeys = 80;
	fits_set_hdrsize(fp, morekeys, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_set_hdrsize(morekeys=%d) failed (%d)\n", pname, morekeys, istat);
		goto quit;
	}

	com.fp = fp;
	com.nrow = 0L;
	com.n_detect = com.n_weisum = 0.0;

	show_parameter("%s:  *** show parameter ***");
	anl_msg_always("\n");

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XRSevtFitsWrite_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRSevtFitsWrite_bgnrun(int *status)
{
	EvsSet("SimASTE_XRSevtFitsWrite:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_XRSevtFitsWrite_ana(int nevent, int eventid, int *status)
{
	static long prev_event_row[32];

	char *k;
	int i, ic, used;
	long irow, irow_prev;
	double photon_time, energy, ra, dec;
	double astetime, xrsx, xrsy, focx, focy, weight, roll;
	int pha, pi, pixel, detx, dety, skyx, skyy;
	unsigned char flag_secondary, flag_midres, flag_lowres;
	char event_flags[8];

	int istat = 0;
	fitsfile *fp = com.fp;
	int *ip = NULL;

	EvsfSetM("SimASTE_XRSevtFitsWrite:ENTRY");

	/* put data to BNK */
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &used, &photon_time);
	BnkfGetM("SimASTE:PHOTON_ENERGY", sizeof(energy), &used, &energy);
	BnkfGetM("SimASTE:RA", sizeof(ra), &used, &ra);
	BnkfGetM("SimASTE:DEC", sizeof(dec), &used, &dec);

	BnkfGetM("SimASTE:XRSX", sizeof(xrsx), &used, &xrsx);
	BnkfGetM("SimASTE:XRSY", sizeof(xrsy), &used, &xrsy);
	BnkfGetM("SimASTE:FOCX", sizeof(focx), &used, &focx);
	BnkfGetM("SimASTE:FOCY", sizeof(focy), &used, &focy);

	BnkfGetM("SimASTE:TIME", sizeof(astetime), &used, &astetime);
	BnkfGetM("SimASTE:PIXEL", sizeof(pixel), &used, &pixel);
	BnkfGetM("SimASTE:DETX", sizeof(detx), &used, &detx);
	BnkfGetM("SimASTE:DETY", sizeof(dety), &used, &dety);
	BnkfGetM("SimASTE:SKYX", sizeof(skyx), &used, &skyx);
	BnkfGetM("SimASTE:SKYY", sizeof(skyy), &used, &skyy);
	BnkfGetM("SimASTE:ROLL", sizeof(roll), &used, &roll);
	BnkfGetM("SimASTE:PHA", sizeof(pha), &used, &pha);
	BnkfGetM("SimASTE:PI", sizeof(pi), &used, &pi);

	BnkfGetM("SimASTE:WEIGHT", sizeof(weight), &used, &weight);

	irow = com.nrow + 1;

	if (
fits_write_col_dbl(fp, ic=col.ti, irow, 1, 1, &astetime, &istat) ||
fits_write_col_int(fp, ic=col.ph, irow, 1, 1, &pha, &istat) ||
fits_write_col_int(fp, ic=col.pi, irow, 1, 1, &pi, &istat) ||
fits_write_col_int(fp, ic=col.pixel, irow, 1, 1, &pixel, &istat) ) {
		goto write_col_error;
	}

	if (
fits_write_col_int(fp, ic=col.detx,	irow, 1, 1, ip=&detx, &istat) ||
fits_write_col_int(fp, ic=col.dety,	irow, 1, 1, ip=&dety, &istat) ||
fits_write_col_int(fp, ic=col.x,	irow, 1, 1, ip=&skyx, &istat) ||
fits_write_col_int(fp, ic=col.y,	irow, 1, 1, ip=&skyy, &istat) ||
		 0 ) {
		if ( NUM_OVERFLOW == istat ) {
			anl_msg_warning("\
%s: WARNING: %s=%d exceeds the limit at irow=%ld.\n\
%s: WARNING: This event is ignored.\n", pname, ttype[ic-1], *ip, irow, pname);
			*status = ANL_SKIP;
			return;
		}
		goto write_col_error;
	}

	if ( 0 <= pixel && pixel < 32 ) {
		irow_prev = prev_event_row[pixel];

		if ( 0 < irow_prev ) {
			BnkfGetM("XRSsim:PREV_EVENT_FLAGS",
					 sizeof(event_flags), &used, event_flags);
			flag_secondary = flag_midres = flag_lowres = 0;
			for (i = 0; i < used; i++) {
				switch (event_flags[i]) {
				case 'S': flag_secondary = 255; break;
				case 'M': flag_midres = 255; break;
				case 'L': flag_lowres = 255; break;
				default:
					;
				}
			}

			if (
fits_write_col_byt(fp, ic=col.fs, irow_prev, 1, 1, &flag_secondary, &istat) ||
fits_write_col_byt(fp, ic=col.fm, irow_prev, 1, 1, &flag_midres, &istat) ||
fits_write_col_byt(fp, ic=col.fl, irow_prev, 1, 1, &flag_lowres, &istat) ||
				 0 ) {
				irow = irow_prev;
				goto write_col_error;
			}
		}

		prev_event_row[pixel] = irow;
	}

	BnkfGetM("XRSsim:EVENT_FLAGS", sizeof(event_flags), &used, event_flags);
	flag_secondary = flag_midres = flag_lowres = 0;
	for (i = 0; i < used; i++) {
		switch (event_flags[i]) {
		case 'S': flag_secondary = 255; break;
		case 'M': flag_midres = 255; break;
		case 'L': flag_lowres = 255; break;
		default:
			;
		}
	}

	if (
fits_write_col_dbl(fp, ic=col.roll,	irow, 1, 1, &roll, &istat) ||
fits_write_col_byt(fp, ic=col.fs, irow, 1, 1, &flag_secondary, &istat) ||
fits_write_col_byt(fp, ic=col.fm, irow, 1, 1, &flag_midres, &istat) ||
fits_write_col_byt(fp, ic=col.fl, irow, 1, 1, &flag_lowres, &istat) ||
fits_write_col_dbl(fp, ic=col.xrsx, irow, 1, 1, &xrsx, &istat) ||
fits_write_col_dbl(fp, ic=col.xrsy, irow, 1, 1, &xrsy, &istat) ||
fits_write_col_dbl(fp, ic=col.focx, irow, 1, 1, &focx, &istat) ||
fits_write_col_dbl(fp, ic=col.focy, irow, 1, 1, &focy, &istat) ||
fits_write_col_dbl(fp, ic=col.weight, irow, 1, 1, &weight, &istat) ||
fits_write_col_dbl(fp, ic=col.photon_time, irow, 1, 1, &photon_time, &istat)||
fits_write_col_dbl(fp, ic=col.energy, irow, 1, 1, &energy, &istat) ||
fits_write_col_dbl(fp, ic=col.ra, irow, 1, 1, &ra, &istat) ||
fits_write_col_dbl(fp, ic=col.dec, irow, 1, 1, &dec, &istat) ||
		 0 ) {
		goto write_col_error;
	}

	com.nrow++;
	com.n_detect += 1.0;
	com.n_weisum += weight;
	BnkfPutM("SimASTE:N_DETECT", sizeof(com.n_detect), &com.n_detect);
	BnkfPutM("SimASTE:N_WEISUM", sizeof(com.n_weisum), &com.n_weisum);

	*status = ANL_OK;
	return;

 write_col_error:
	k = ttype[ic-1];
	anl_msg_error("\
%s: fits_write_col('%s') failed at irow=%ld (%d)\n", pname, k, irow, istat);
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XRSevtFitsWrite_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRSevtFitsWrite_exit(int *status)
{
	double tstart, tstop, expo;
	int used;
	GTI_DATA *gp;

	int istat = 0;
	fitsfile *fp = com.fp;

/* get the tstart and tstop */
	BnkGet("SimASTE:TSTART", sizeof(tstart), &used, &tstart);
	BnkGet("SimASTE:TSTOP", sizeof(tstop), &used, &tstop);
	BnkGet("SimASTE:EXPOSURE", sizeof(expo), &used, &expo);
	BnkGet("SimASTE:GTI_DATA:PTR", sizeof(gp), &used, &gp);
	if ( 0 == used || NULL == gp || 0.0 == gp->ontime ) {
		gp->ngti = 1;
		gp->tstart = tstart;
		gp->tstop = tstop;
		gp->ontime = expo;
		gp->telapse = tstop - tstart;
		gp->start = &gp->tstart;
		gp->stop = &gp->tstop;
	}

	if (
	/* update the date/obs keywords */
		 SimASTE_update_time_keys(fp, tstart, tstop, expo) ||
		 SimASTE_write_timestamp(fp) ||
		 SimASTE_write_photon_detect_info(fp) ||
		 SimASTE_write_random_number_info(fp) ||
	/* write the gti extension */
		 SimASTE_write_gti(fp, gp, &stdkeys) ||
		 0 ) {
		*status = ANL_QUIT;
		return;
	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
		fits_report_error(stderr, istat);
		*status = ANL_QUIT;
		return;
	}

	anl_msg_info("\
%s: %ld events were written to '%s'\n", pname, com.nrow, com.outfile);

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
