/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/science.c,v 3.9 2002/02/07 17:10:37 irby Exp $   */
/*                   */

/*
 filename:	science.c
 purpose:	read and interpret the science data file
 author:	Geoffrey B. Crew
 date:		May 1993
 */
/******************************************************************************
Taskname:    sqaplot

Filename:    science.c

Description: read and interpret the science data file

Author/Date: Geoffrey B. Crew, 5/93

Modification History:
    James Peachey, HSTX, 8/96 - changed to use native cfitsio library:
    - Replaced macro syntax FFTXXXX with cfitsio ffxxxx
    - Added extern declaration for fun_cfits, a global file handle 
      defined in sqaplot.c
    - For I/O of the sqwork structure, use new structure element fitsfile
      *en_cfits instead of element int en (LUN)
    - Modified str_fix to deal correctly with strings which are already
      null terminated
    - Used fitsio.h macros in some cases when setting array sizes

Notes:

Functions implemented in this file:
    void get_science(int *status);
    static void read_primary(int *status);
    static void read_pri_hk(int *status);

******************************************************************************/

#include "defs.h"
#include "compat.h"
#if !defined (vax) && !defined (__APPLE__)
# include <malloc.h>
#endif

#define BATCH	512
#define MHIST	65535
#define FLIXF	10
#define FLIXL	5
#define HITME	2

#define FQB(H)	( ( (H) > MAX_FRAMES ) ? MAX_FRAMES : (H) )
#define PHB(P)	( ( ( (isphc) ? PH(P) : (P) ) * ADU_BINS) / 4096 )
#define TMB(T)	( ( (T) >= t_stop ) ? TIME_BINS :		\
		  ( (T) < t_start ) ?    -1     :		\
			(int)( ( (T) - t_start ) * t_scale ) )

static int	isphc, total_events, mode_fbf, mode_hml, fits_rows = 0;
static double	t_scale, t_start, t_stop;
static Real	frfvers;

static int	funit = F_IUNIT, *fun = &funit,
		zero = 0,	*zer = &zero,
		wone = 1,	*wun = &wone,
		sevn = 7,	*sev = &sevn,
		nine = 9,	*nin = &nine,
		rown = 0,	*row = &rown;
static char	com[MAX_FNAME], hkk[HKKWORDS][82];

static void	read_primary(), read_header(), read_events(), dump_header(),
		flix_setup(), flix_cleanup(), flix_cdist(), flix_dump(),
		flix_thre(), str_fix(), time_setup(), dump_hist(), tlm_sat(),
		read_pri_hk(), events_hdr(), write_evt(), dump_evt(),
		fix_hdr(), extra_hdr(), event_ph2pi(),
		do_thinking();
static double	frac();
static int	count_ccds(), flix_glob();
static int	threshold(), threshold_p(), threshold_s(),
		threshold_o(), threshold_v(), threshold_u();
static Event	*get_events();
static Flick	*flix_alloc();
static void	(*evt_ph2pi)();

extern fitsfile *fun_cfits;

/*
 *  Read and interpret the science file.
 *  This version makes lots of assumptions, and is likely to
 *  break big-time if zillions of events are thrown into it.
 */
void
get_science(status)
	int	*status;
{
	read_primary(status);
	ERRCK2(*status,"Problem %d with FITS primary.\n",*status,return);

	read_header(status);
	ERRCK2(*status,"Problem %d with EVENTS header.\n",*status,return);

	do_thinking(status);
	ERRCK2(*status,"Problem %d sorting it all out.\n",*status, return);

	time_setup();				/* prep for time binning */
	read_events(0, status);
	ERRCK2(*status,"Problem %d with EVENTS bintable.\n",*status,return);

	flix_setup(status);
	ERRCK2(*status,"Problem %d prepping flicker pixels.\n",*status,return);

	if (sqwork.ts || sqwork.mx)
		tlm_sat();			/* prep for light curve */
	if (sqwork.fe) events_hdr(status);	/* setup new extension	*/
	ERRCK2(*status,"Problem %d on new EVENTS bintable.\n",*status,return);
#ifdef SISDEBUG
	if (!sqwork.op)
#endif SISDEBUG
	read_events(1, status);
	ERRCK2(*status,"Problem %d with EVENTS bintable.\n",*status,return);
	if (sqwork.fe) fix_hdr(status);
	ERRCK2(*status,"Problem %d closing EVENTS bintable.\n",*status,return);

	flix_cleanup(status);
	ERRCK2(*status,"Problem %d prepping for plots.\n",*status,return);

	if (sqwork.fd >= 0) dump_hist();
}

#define G_K(K,V,F)	do {					\
/*				FFTGKEY(fun,K,V,com,status);*/	\
				ffgkey(fun_cfits,K,V,com,status);	\
				if (F && *status) *status = 0;	\
			} while(0)
/*
 *  Read the Primary FITS header of the Science file, doing idoit checks.
 */
static void
read_primary(status)
	int	*status;
{
	static int	hdu;

/*	FFTMAHD(fun, wun, &hdu, status);*/
	ffmahd(fun_cfits, 1, &hdu, status);

	/*  Conversational stuff */
	G_K("INSTRUME",sqwork.hd.instrume,1);
	G_K("ORIGIN  ",sqwork.hd.origin  ,1);
	G_K("OBJECT  ",sqwork.hd.object  ,1);
	G_K("SEQPI   ",sqwork.hd.seqpi   ,1);
	G_K("OBSERVER",sqwork.hd.seqpi   ,1);
	G_K("RA--NOM ",sqwork.hd.ra_nom  ,1);
	G_K("RA_NOM  ",sqwork.hd.ra_nom  ,1);
	G_K("DEC--NOM",sqwork.hd.dec_nom ,1);
	G_K("DEC_NOM ",sqwork.hd.dec_nom ,1);
	G_K("EQUINOX ",sqwork.hd.equinox ,1);
	G_K("TELESCOP",sqwork.hd.telescop,1);
	G_K("DATE-OBS",sqwork.hd.date_obs,1);
	G_K("TIME-OBS",sqwork.hd.time_obs,1);
	G_K("DATE-END",sqwork.hd.date_end,1);
	G_K("TIME-END",sqwork.hd.time_end,1);
	G_K("BIT_RATE",sqwork.hd.bit_rate,1);
	G_K("DATAMODE",sqwork.hd.datamode,1);
	G_K("DATE    ",sqwork.hd.date    ,1);
	G_K("AUTHOR  ",sqwork.hd.author  ,1);
	G_K("CREATOR ",sqwork.hd.author  ,1);
	G_K("ORBIT0  ",sqwork.hd.orbit0  ,1);
	G_K("ORBITBEG",sqwork.hd.orbit0  ,1);
	G_K("ORBIT1  ",sqwork.hd.orbit1  ,1);
	G_K("ORBITEND",sqwork.hd.orbit1  ,1);
	G_K("TLM_FILE",sqwork.hd.tlm_file,1);
	G_K("TIMEDEL ",sqwork.hd.timedel ,1);

	/*  Really useful stuff:  mandatory */
	G_K("TSTART  ",sqwork.hd.tstart  ,0);
	G_K("TSTOP   ",sqwork.hd.tstop   ,0);
	G_K("EXPOSURE",sqwork.hd.exposure,1);
	G_K("ONTIME  ",sqwork.hd.exposure,1);
	G_K("NEVENTS ",sqwork.hd.nevents ,0);	str_fix(sqwork.hd.nevents);
	sqwork.hd.nvents = atoi(sqwork.hd.nevents);

	read_pri_hk(status);

	if (sqwork.fe) extra_hdr(0, status);
}

/*
 *  See what we can learn from the primary housekeeping data.
 */
static void
read_pri_hk(status)
	int	*status;
{
	int	numb, kadd, j, k;

	/* oldest formats */
	G_K("CCDLST0 ",sqwork.hd.ccdlst0 ,1);
	G_K("CCDLST1 ",sqwork.hd.ccdlst1 ,1);
	/* old formats */
	G_K("CCDPOWS0",sqwork.hd.ccdpows0,1);
	G_K("CCDPOWS1",sqwork.hd.ccdpows1,1);
	G_K("CCDMODE0",sqwork.hd.ccdmode0,1);
	G_K("CCDMODE1",sqwork.hd.ccdmode1,1);
	G_K("CCDLIST0",sqwork.hd.ccdlist0,1);
	G_K("CCDLIST1",sqwork.hd.ccdlist1,1);
	/* new formats */
	G_K("S0CCDPOW",sqwork.hd.ccdpows0,1);
	G_K("S0CCDMOD",sqwork.hd.ccdmode0,1);
	G_K("S0CCDLST",sqwork.hd.ccdlist0,1);
	G_K("S1CCDPOW",sqwork.hd.ccdpows1,1);
	G_K("S1CCDMOD",sqwork.hd.ccdmode1,1);
	G_K("S1CCDLST",sqwork.hd.ccdlist1,1);
	/* NeXt formats */

	/*
	 *  Grab the last primary header keywords, which
	 *  presumably contain the modal housekeeping data.
	 */
/*	FFTGHSP(fun, &numb, &kadd, status);*/
	ffghsp(fun_cfits, &numb, &kadd, status);
	if (numb > HKKWORDS) numb = HKKWORDS;
	for (k = 0, j = PRIWORDS + 1; k < numb; k++, j++) {
/*		FFTGREC(fun, &j, hkk[k], status); */
		ffgrec(fun_cfits, j, hkk[k], status); 
		if (hkk[k][0] == ' ' || !strncmp(hkk[k], "END", 3)) break;
		hkk[k][80] = '\n';
		hkk[k][81] = '\0';
	}
	if (k < HKKWORDS)
		hkk[k][0]  = '\0';
	else
		(void)strcpy(hkk[HKKWORDS-1],
			"Additional primary header keywords were present.\n");
}

/*
 *  Read the header of the EVENTS extension, grabbing useful info
 *  and comparing common information with the primary header data.
 */
static void
read_header(status)
	int	*status;
{
	static int	hdu;

/*	FFTMAHD(fun, &sqwork.ex, &hdu, status);*/
	ffmahd(fun_cfits, sqwork.ex, &hdu, status);
	G_K("INSTRUME",sqwork.hd.instrume,1);	str_fix(sqwork.hd.instrume);
	G_K("ORIGIN  ",sqwork.hd.origin  ,1);	str_fix(sqwork.hd.origin  );
	G_K("OBJECT  ",sqwork.hd.object  ,1);	str_fix(sqwork.hd.object  );
	G_K("SEQPI   ",sqwork.hd.seqpi   ,1);
	G_K("OBSERVER",sqwork.hd.seqpi   ,1);	str_fix(sqwork.hd.seqpi   );
	G_K("RA--NOM ",sqwork.hd.ra_nom  ,1);
	G_K("RA_NOM  ",sqwork.hd.ra_nom  ,1);
	G_K("RA_PNT  ",sqwork.hd.ra_nom  ,1);	str_fix(sqwork.hd.ra_nom  );
	G_K("DEC--NOM",sqwork.hd.dec_nom ,1);
	G_K("DEC_NOM ",sqwork.hd.dec_nom ,1);
	G_K("DEC_PNT ",sqwork.hd.dec_nom ,1);	str_fix(sqwork.hd.dec_nom );
	G_K("EQUINOX ",sqwork.hd.equinox ,1);	str_fix(sqwork.hd.equinox );
	G_K("TELESCOP",sqwork.hd.telescop,1);	str_fix(sqwork.hd.telescop);
	G_K("DATE-OBS",sqwork.hd.date_obs,1);	str_fix(sqwork.hd.date_obs);
	G_K("TIME-OBS",sqwork.hd.time_obs,1);	str_fix(sqwork.hd.time_obs);
	G_K("DATE-END",sqwork.hd.date_end,1);	str_fix(sqwork.hd.date_end);
	G_K("TIME-END",sqwork.hd.time_end,1);	str_fix(sqwork.hd.time_end);
	G_K("BIT_RATE",sqwork.hd.bit_rate,1);	str_fix(sqwork.hd.bit_rate);
	G_K("DATAMODE",sqwork.hd.datamode,1);	str_fix(sqwork.hd.datamode);
	G_K("DATE    ",sqwork.hd.date    ,1);	str_fix(sqwork.hd.date    );
	G_K("AUTHOR  ",sqwork.hd.author  ,1);
	G_K("CREATOR ",sqwork.hd.author  ,1);	str_fix(sqwork.hd.author  );
	G_K("ORBIT0  ",sqwork.hd.orbit0  ,1);
	G_K("ORBITBEG",sqwork.hd.orbit0  ,1);	str_fix(sqwork.hd.orbit0  );
	G_K("ORBIT1  ",sqwork.hd.orbit1  ,1);
	G_K("ORBITEND",sqwork.hd.orbit1  ,1);	str_fix(sqwork.hd.orbit1  );
	G_K("TLM_FILE",sqwork.hd.tlm_file,1);	str_fix(sqwork.hd.tlm_file);
	G_K("TIMEDEL ",sqwork.hd.timedel ,1);	str_fix(sqwork.hd.timedel );

	G_K("TSTART  ",sqwork.hd.tstart  ,0);	str_fix(sqwork.hd.tstart  );
	G_K("TSTOP   ",sqwork.hd.tstop   ,0);	str_fix(sqwork.hd.tstop   );
	G_K("EXPOSURE",sqwork.hd.exposure,1);
	G_K("ONTIME  ",sqwork.hd.exposure,1);	str_fix(sqwork.hd.exposure);
	G_K("NAXIS2  ",sqwork.hd.naxis2  ,0);	str_fix(sqwork.hd.naxis2  );
	G_K("PHA_BINS",sqwork.hd.pha_bins,1);	str_fix(sqwork.hd.pha_bins);
	sqwork.hd.tstrt   = atof(sqwork.hd.tstart  );
	sqwork.hd.tstp    = atof(sqwork.hd.tstop   );
	sqwork.hd.expsure = atof(sqwork.hd.exposure);
	sqwork.hd.tdelay  = atof(sqwork.hd.timedel );
	sqwork.hd.nxis2   = atoi(sqwork.hd.naxis2  );
	sqwork.hd.phabins = atoi(sqwork.hd.pha_bins);
}

/*
 *  Ok, we read just about everything.  Time to make some sense of it.
 */
static void
do_thinking(status)
	int	*status;
{
	char		*s;

	/*
	 *  Below is really messy while file formats are changing
	 *  and we might be reading just about anything.  Later,
	 *  this can probably be simplified.
	 */
	(void)sscanf(sqwork.hd.author, " FRFREAD %f", &frfvers);
#ifdef SISDEBUG
	ERRCK2(frfvers > sqwork.frfvers,
		"Science file (%f) is more recent than Sqaplot.\n",
		frfvers, 0);
	ERRCK2(1, "File created with frfread version %f.\n", frfvers, 0);
#endif

	t_start = sqwork.hd.tstrt;
	t_stop  = sqwork.hd.tstp;

	if (!strncmp(sqwork.hd.bit_rate+1, "HIGH", 4)) mode_hml = 0; else
	if (!strncmp(sqwork.hd.bit_rate+1, "MEDI", 4)) mode_hml = 1; else
	if (!strncmp(sqwork.hd.bit_rate+1, "LOW",  3)) mode_hml = 2; else
	ERRCK2(1, "Unrecognized bitrate %s, assuming HIGH\n",
		sqwork.hd.bit_rate, mode_hml = 0);
	if (!strncmp(sqwork.hd.datamode+1,"BRIGHT2",7))mode_fbf = 3; else
	if (!strncmp(sqwork.hd.datamode+1, "FAIN", 4)) mode_fbf = 0; else
	if (!strncmp(sqwork.hd.datamode+1, "BRIG", 4)) mode_fbf = 1; else
	if (!strncmp(sqwork.hd.datamode+1, "FAST", 4)) mode_fbf = 2; else
	ERRCK2(1, "Unrecognized datamode %s, assuming BRIGHT\n",
		sqwork.hd.datamode, mode_fbf = 1);

	if (sqwork.hd.phabins == 0) switch (mode_fbf) {
		case 0: case 3:	sqwork.hd.phabins = 4096;	break;
		case 1: case 2:	sqwork.hd.phabins = 2048;	break;
		default:					break;
	} else {
		ERRCK2(sqwork.hd.phabins != 2048 && sqwork.hd.phabins != 4096,
			"Phabins undefined, assuming %d\n", 4096,
			sqwork.hd.phabins = 4096);
	}

	if (mode_fbf == 0 || mode_fbf == 3)		isphc = 0; else	
	if (mode_fbf == 1 || mode_fbf == 2)		isphc = 1; else	
	ERRCK2(1, "Unsure about PH compression, assuming 4096 PH bins\n",
		0, isphc = 0);

	if (s = strpbrk(sqwork.hd.instrume, "01"))
		sqwork.hd.sisn = *s - '0';
	else
		*status = 10001;
	sqwork.hd.nccd = count_ccds(0);

	if (mode_fbf == 0) {
		get_echos(		/* Cf. secular */
			sqwork.esp,
			(sqwork.hd.tstrt + sqwork.hd.tstp)/2.0,
			sqwork.hd.sisn,
			sqwork.ek+sqwork.hd.sisn);
		grade_setup();
#ifdef SISDEBUG
		gr_setup(&sqwork.st, sqwork.ek+sqwork.hd.sisn, &sqwork.sy);
#endif SISDEBUG
	}

	/* setup ph2pi transformation function */
	evt_ph2pi = event_ph2pi;	/* old default */
	get_ph2pi(			/* Cf. secular */
		sqwork.ctp,
		(sqwork.hd.tstrt + sqwork.hd.tstp)/2.0,
		sqwork.hd.sisn,
		&evt_ph2pi);
	evt_ph2pi((Event *)0);

	dump_header(status);
}

/*
 *  Create events extension of the output FITS file.
 */
static void
events_hdr(status)
	int	*status;
{
/*	static int	nrows, nflds = 10, i;*/
	static long	nrows, pcount = 0;
	static int	nflds = 10, i;

/*	static char	ttype[][8] = {	"X       ", "Y       ",*/
	static char	*ttype[] = {	"X       ", "Y       ",
					"RAWX    ", "RAWY    ",
					"TIME    ", "PHA     ",
					"DETX    ", "DETY    ",
					"GRADE   ", "CCDID   ",
					"PHAS    ", "PI      "  },
/*			tform[][8] = {	"1I      ", "1I      ",*/
			*tform[] = {	"1I      ", "1I      ",
					"1I      ", "1I      ",
					"1D      ", "1I      ",
					"1I      ", "1I      ",
				/*	"1B      ", "1B      ",	*/
					"1I      ", "1I      ",
					"9I      ", "1I      "  },
/*			tunit[][8] = {	"pixel   ", "pixel   ",*/
			*tunit[] = {	"pixel   ", "pixel   ",
					"pixel   ", "pixel   ",
					"s       ", "pha     ",
					"pixel   ", "pixel   ",
					"NONE    ", "NONE    ",
					"pha     ", "channel "  },
			vals[8], com[47], rec[80];

/*	FFTCRHD(&sqwork.fe, status);*/
	ffcrhd(sqwork.fe_cfits, status);
	(void)strncpy(vals, "EVENTS  ", 8);
	if (mode_fbf == 0) nflds = (sqwork.dopi) ? 12 : 11;
	else if (sqwork.dopi) {
		nflds = 11;
		(void)strncpy(ttype[10], ttype[11], 8);
		(void)strncpy(tform[10], tform[11], 8);
		(void)strncpy(tunit[10], tunit[11], 8);
	}
/*	nrows = sqwork.hd.nvents;		/* could be zero? */
	nrows = (long) sqwork.hd.nvents;	/* could be zero? */
/*	FFTPHBN(&sqwork.fe, &nrows, &nflds,
		ttype, tform, tunit, vals, zer, status);*/
	ffphbn(sqwork.fe_cfits, nrows, nflds,
		ttype, tform, tunit, vals, pcount, status);
	/* additional keywords */
	for (i = 1; !*status; i++) {
/*		FFTGREC(fun, &i, rec, status);*/
		ffgrec(fun_cfits, i, rec, status);
		if (!strncmp(rec, "EXTNAME ", 8)) break;
	}
	for (++i ; !*status; ++i) {
/*		FFTGREC(fun, &i, rec, status);*/
		ffgrec(fun_cfits, i, rec, status);
		if (rec[0] == ' ' || !strncmp(rec, "END     ", 8)) break;
/*		FFTPREC(&sqwork.fe, rec, status);*/
		ffprec(sqwork.fe_cfits, rec, status);
	}
	if (sqwork.fe) extra_hdr(1, status);
/*	FFTBDEF(&sqwork.fe, &nflds, tform, zer, &nrows, status);*/
/* JP 8/7/96: no replacement call needed for FFTBDEF, since there is no
cfitsio equivalent routine. Cfitsio automatically uses the keywords which
were written above in ffphbn to define the table structure. */
	ERRCK2(*status, "Problem %d defining EVENTS extension\n", *status,0);
}

/*
 *  Additional header keywords when cloning event file.
 */
static void
extra_hdr(ext, status)
	int	ext, *status;
{
/*	static char	key[8], cmt[47], rec[80], buf[240], white[] = */
	static char	key[FLEN_KEYWORD],
			cmt[FLEN_COMMENT], rec[FLEN_CARD], buf[240], white[] = 
		"                                                 ";

	if (ext == 0) {		/* copy primary */
/*		FFTCOPY(fun, &sqwork.fe, sev, status);*/
		ffcopy(fun_cfits, sqwork.fe_cfits, 7, status);
	}

/*	(void)strncpy(key, "SPLIT_TH", 8);*/
	(void)strncpy(key, "SPLIT_TH", FLEN_KEYWORD);
/*	(void)strncpy(cmt,
		"split threshold level                          ", 47);*/
	(void)strncpy(cmt,
		"split threshold level                          ",
		FLEN_COMMENT);
/*	FFTPKYJ(&sqwork.fe, key, &sqwork.st, cmt, status);*/
	ffpkyj(sqwork.fe_cfits, key, (long) sqwork.st, cmt, status);

	(void)sprintf(buf,
		"SQAPLOT of %s", sqwork.in);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Flicker thresholds: %d %d %d %d          %s",
		sqwork.cs[0].knk, sqwork.cs[1].knk,
		sqwork.cs[2].knk, sqwork.cs[3].knk, white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Nominal Echo correction: %f              %s",
		sqwork.ek[sqwork.hd.sisn], white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Pixels corrected per event: %d           %s",
		sqwork.sy, white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Maximum grade output: %d                 %s",
		sqwork.gm, white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Saturated-telemetry frames %s            %s",
		(sqwork.ts) ? "ignored" : "retained", white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
	(void)sprintf(buf,
		"Maximum counts per sec imposed at %d     %s",
		sqwork.mx, white);
	(void)strncpy(rec, buf, 80);
/*	FFTPHIS(&sqwork.fe, rec, status);*/
	ffphis(sqwork.fe_cfits, rec, status);
}

/*
 *  Go back and clean up the FITS output files' headers.
 */
static void
fix_hdr(status)
	int	*status;
{
	static int	hdu, i;
	static char	keep[47];

	for (i = 1, keep[0] = '&'; i < 47; i++) keep[i] = ' ';
	/*
	 *  Patch up the current events extension.
	 */
/*	FFTMKYJ(&sqwork.fe, "NAXIS2  ", &fits_rows, keep, status);*/
	ffmkyj(sqwork.fe_cfits, "NAXIS2  ", (long) fits_rows, keep, status);

	/*
	 *  Patch up the primary header.
	 */
/*	FFTMAHD(&sqwork.fe, wun, &hdu, status);*/
	ffmahd(sqwork.fe_cfits, 1, &hdu, status);
/*	FFTMKYJ(&sqwork.fe, "NEVENTS ", &fits_rows, keep, status);*/
	ffmkyj(sqwork.fe_cfits, "NEVENTS ", (long) fits_rows, keep, status);
}

/*
 *  Dump out an event to the FITS output file.
 */
static void
write_evt(ev, status)
	Event	*ev;
	int	*status;
{
	static int	*rw = &fits_rows,
			col[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

	fits_rows++;
/*	FFTPCLI(&sqwork.fe, col+1,  rw, wun, wun, &ev->a, status);
	FFTPCLI(&sqwork.fe, col+2,  rw, wun, wun, &ev->d, status);
	FFTPCLI(&sqwork.fe, col+3,  rw, wun, wun, &ev->x, status);
	FFTPCLI(&sqwork.fe, col+4,  rw, wun, wun, &ev->y, status);
	FFTPCLD(&sqwork.fe, col+5,  rw, wun, wun, &ev->t, status);
	FFTPCLI(&sqwork.fe, col+6,  rw, wun, wun,
			       (sqwork.dopi) ? &ev->q : &ev->p, status);
	FFTPCLI(&sqwork.fe, col+7,  rw, wun, wun, &ev->u, status);
	FFTPCLI(&sqwork.fe, col+8,  rw, wun, wun, &ev->v, status);
	FFTPCLB(&sqwork.fe, col+9,  rw, wun, wun, &ev->g, status);
	FFTPCLB(&sqwork.fe, col+10, rw, wun, wun, &ev->c, status);*/

	ffpcli(sqwork.fe_cfits, 1, (long) fits_rows, 1, 1, &ev->a, status);
	ffpcli(sqwork.fe_cfits, 2, (long) fits_rows, 1, 1, &ev->d, status);
	ffpcli(sqwork.fe_cfits, 3, (long) fits_rows, 1, 1, &ev->x, status);
	ffpcli(sqwork.fe_cfits, 4, (long) fits_rows, 1, 1, &ev->y, status);
	ffpcld(sqwork.fe_cfits, 5, (long) fits_rows, 1, 1, &ev->t, status);
	ffpcli(sqwork.fe_cfits, 6, (long) fits_rows, 1, 1,
			       (sqwork.dopi) ? &ev->q : &ev->p, status);
	ffpcli(sqwork.fe_cfits, 7, (long) fits_rows, 1, 1, &ev->u, status);
	ffpcli(sqwork.fe_cfits, 8, (long) fits_rows, 1, 1, &ev->v, status);
	ffpclb(sqwork.fe_cfits, 9, (long) fits_rows, 1, 1, &ev->g, status);
	ffpclb(sqwork.fe_cfits, 10,(long) fits_rows, 1, 1, &ev->c, status);

	if (mode_fbf == 0) {
/*		FFTPCLI(&sqwork.fe, col+11, rw, wun, nin, ev->s, status);*/
		ffpcli(sqwork.fe_cfits, 11, (long) fits_rows, 1, 9,
			ev->s, status);
/*		if (sqwork.dopi) FFTPCLI(&sqwork.fe, col+12,
				    rw, wun, wun, &ev->p, status);*/
		if (sqwork.dopi) ffpcli(sqwork.fe_cfits, 12,
				    (long) fits_rows, 1, 1, &ev->p, status);
	} else if (sqwork.dopi)
/*		FFTPCLI(&sqwork.fe, col+11, rw, wun, wun, &ev->p, status);*/
		ffpcli(sqwork.fe_cfits, 11, (long) fits_rows, 1, 1,
			&ev->p, status);
}

/*
 *  Dump out diagnostic error on bad events.
 */
static void
dump_evt(ev)
	Event	*ev;
{
	(void)sprintf(com,
		"!!! Invalid event: chip%d (%d,%d) grade%d\n !!!",
		ev->c, ev->x, ev->y, ev->g);
	sqecho(com);
}

/*
 *  Dump out what we've learned from the FITS keywords.
 */
static void
dump_header(status)
	int	*status;
{
	static char	bf[MAX_FNAME];
	int		k;

	(void)sprintf(bf, "Origin:%s\t",
		sqwork.hd.origin);			sqecho(bf);
	(void)sprintf(bf, "Object:%s\t",
		sqwork.hd.object);			sqecho(bf);
	(void)sprintf(bf, "Observer:%s\n",
		sqwork.hd.seqpi);			sqecho(bf);
	(void)sprintf(bf, "Instrument:%s\t",
		sqwork.hd.instrume);			sqecho(bf);
	(void)sprintf(bf, "Mode:%d-CCD %s\t",
		sqwork.hd.nccd, sqwork.hd.datamode);	sqecho(bf);
	(void)sprintf(bf, "Bit_Rate:%s\n",
		sqwork.hd.bit_rate);			sqecho(bf);
	if (frfvers <= 2.995 || frfvers >= 3.005) {
		(void)sprintf(bf, "Nominal RA, DEC:  %lg, %lg (EQUINOX=%lg)\n",
			atof(sqwork.hd.ra_nom), atof(sqwork.hd.dec_nom),
			atof(sqwork.hd.equinox));		sqecho(bf);
	}
	(void)sprintf(bf, "Start Time (ASCA Time, UT) %s %s %s\n",
		sqwork.hd.tstart, sqwork.hd.date_obs, sqwork.hd.time_obs);
							sqecho(bf);
	(void)sprintf(bf, "End   Time (ASCA Time, UT) %s %s %s\n",
		sqwork.hd.tstop,  sqwork.hd.date_end, sqwork.hd.time_end);
							sqecho(bf);
	(void)sprintf(bf, "Orbits:  %s -%s\tTelemetry file:%s\n",
		sqwork.hd.orbit0, sqwork.hd.orbit1, sqwork.hd.tlm_file);
							sqecho(bf);
	(void)sprintf(bf, "Author of the File:  %s\tCreation Date %s\n",
		sqwork.hd.author, sqwork.hd.date);	sqecho(bf);

	if (sqwork.hd.nvents == sqwork.hd.nxis2) {
		(void)sprintf(bf, "Number of Events:  %d\n\n",
		sqwork.hd.nxis2);			sqecho(bf);
	} else {
		(void)sprintf(bf, "Number of Events (Primary):  %d\n",
		sqwork.hd.nvents);			sqecho(bf);
		(void)sprintf(bf, "Number of Events (EVENTS):   %d\n",
		sqwork.hd.nxis2);			sqecho(bf);
/*		*status = sqwork.hd.nvents - sqwork.hd.nxis2;		*/
	}

	sqecho("\n** SQAPLOT additional configuration information **\n\n");
	(void)sprintf(bf, "\tIgnore telemetry-saturated frames = %s\n",
		(sqwork.ts) ? "yes" : "no");	sqecho(bf);
	(void)sprintf(bf, "\tImposed maximum counts per sec limit of = %d\n",
		sqwork.mx);			sqecho(bf);

	/* Dumpout Echo information */
	(void)sprintf(bf, "\tEcho Correction = %.4e\n",
		sqwork.ek[sqwork.hd.sisn]);	sqecho(bf);

	/* Dumpout PI mapping information */
	if (sqwork.dopi) {
		evt_ph2pi((Event *)0);
		(void)sprintf(bf, "\tNominal PI gain %.4lf eV/ADU\n",
			1000.0*sqwork.gn);	sqecho(bf);
	}

	sqecho("\n** Housekeeping Keywords from Primary Header **\n\n");
	for (k = 0; k < HKKWORDS && hkk[k][0]; k++) sqecho(hkk[k]);
	sqecho("\n");

	*status = 0;
}

/*
 *  Read the EVENTS binary table and acquire statistics.
 *  We make two passes through the data:  on the first pass,
 *  the image histograms are aquired, which allows for the
 *  identification of the bad pixels.  On the second pass,
 *  the spectral and light curve info is acquired, skipping
 *  the bad pixels.  At this point we're ready to plot.
 */
static void
read_events(which, status)
	int	which, *status;
{
	static Event	events[BATCH];
	register Event	*ev = events;
	register Chips	*cp;

	rown = 0;	/* flag for get_events */
	/*
	 *  Work through events; grab from FITS in batches
	 */
	while ( (ev > events) ||
		(ev = get_events(events, BATCH, status)) > events ) {
		ev--;
		cp = sqwork.cs + ev->c;
		if (which == 0) {
			/* first pass:  accumulate image */
			cp->xy[ev->x][ev->y]++;
			cp->gt[TMB(ev->t)]++;	
			total_events++;
#ifdef SISEXTRA
			(void)ex_events(ev, 0);
#endif SISEXTRA
			continue;
		} else {
			register int	p;
			register int	h;
			register Flick	*f;
#ifdef SISEXTRA
			(void)ex_events(ev, 1);
#endif SISEXTRA
			/* second pass:  flicker statistics */
			if ( (h = (int)(cp->xy[ev->x][ev->y] - cp->knk))
				>= 0 && (f = cp->flx) ) {
					f += h;
					if (!f->born) {
						f->born = ev->t;
						f->x = ev->x;
						f->y = ev->y;
					}
					f->died = ev->t;
					f->mpha += (double)ev->p;
					f->spha += (double)(ev->p)
						 * (double)(ev->p);
					f->numb++;

					cp->flixes++;
					continue;
			}
#ifdef SISEXTRA
			(void)ex_events(ev, 2);
#endif SISEXTRA
			/* second pass:  image, spectrum & light curve */
			if (!cp->gt[h = TMB(ev->t)]) {
				cp->satevs++;
			} else if (ev->g > sqwork.gm) {
				cp->grades++;
			} else if ((p = PHB(ev->p)) >= ADU_BINS) {
				cp->phabov++;
			} else {
				cp->xy[ev->x][ev->y]++;
				cp->ph[p]++;
				cp->lc[h]++;	
#ifdef SISEXTRA
			(void)ex_events(ev, 3);
#endif SISEXTRA
				cp->events++;
				if(sqwork.fe) write_evt(ev, status);
			}

		}
	}
	ERRCK2(*status, "Problem getting events %d\n", *status, return);
	ERRCK2(!which && (total_events - sqwork.hd.nxis2) != 0,
		"Events found - NAXIS2 = %d\n",
		(total_events - sqwork.hd.nxis2), 0);
}

/*
 *  Assess pixel frequencies for each chip; determine an
 *  appropriate threshold and allocate space to hold the
 *  information for each flickering pixel.  
 */
static void
flix_setup(status)
	int	*status;
{
	Chips		*cp;
	register int	x, y;
	int		n, t;

	for (cp = sqwork.cs; cp < sqwork.cs + 4; cp++) {
		/* construct frequency distribution */
		for (x = 0; x < MAX_COLS; x++)
			for (y = 0; y < MAX_ROWS; y++)
				cp->fq[FQB(cp->xy[x][y])]++;
		flix_cdist(cp->fq);

		cp->knk = t = threshold(cp);
		cp->flx = flix_alloc(n = cp->fq[cp->knk]);

		/* replace pixel value with flx index */
		for (x = 0; x < MAX_COLS; x++)
			for (y = 0; y < MAX_ROWS; y++)
				if (cp->xy[x][y] >= t)
					cp->xy[x][y] = t + --n;
				else
					cp->xy[x][y] = 0;
	}

	sqwork.hd.nccd = count_ccds(1);
	*status = 0;
}

/*
 *  Figure out how many CCD's are really present.
 *  On the first pass, we look at what the HK thinks,
 *  and on the second, we look at events we got....
 */
static int
count_ccds(t)
	int	t;
{
	register int	i, n;
	char		b, *l;
	static int	h[4] = { 0, 0, 0, 0 };

	if (t) {	/* there are events */
		for (i = 0, n = 0; i < 4; i++)
			if (sqwork.cs[i].fq[1]) n++;
	
		i = sqwork.hd.nccd - n;
		ERRCK2(i > 0, "#CCD's - #chips with events = %d\n",
			i, return(sqwork.hd.nccd));
		ERRCK2(i < 0, "#chips with events - #CCD's = %d\n",
			-i, return(n));
		n = sqwork.hd.nccd;
	} else {	/* there is HK info -- which kind */
		/*
		 *  b = CCDMODE, an integer.
		 */
		n = (sqwork.hd.sisn) ? atoi(sqwork.hd.ccdmode1)
				     : atoi(sqwork.hd.ccdmode0);
		if (n) return(n);

		/*
		 *  l = CCDLIST, a sequence '0 1 2 3 '
		 */
		l = (sqwork.hd.sisn) ? sqwork.hd.ccdlist1
				     : sqwork.hd.ccdlist0;
		for (i = 1; *l && i < 9; i+=2)	h[atoi(l+i)]++;
		for (i = 0, n = 0; i < 4; i++)	if (h[i]) n++;
		if (n) return(n);

		/*
		 *  b = bb bb bb bb -- a list of CCD ID's from CCDLST?
		 */
		b = (sqwork.hd.sisn) ? atoi(sqwork.hd.ccdlst1)
				     : atoi(sqwork.hd.ccdlst0);
		for (i = 0; i < 4; i++, b >>= 2) h[b & CCD_BITS]++;
		for (i = 0, n = 0; i < 4; i++)	 if (h[i]) n++;
		ERRCK2(!n, "No CCD's in CCDLST!\n", 0, n = 4);
	}
	return(n);
}

/*
 *  Choose a suitable time binning scale.  The number of bins
 *  must be less than TIME_BINS, and the duration of each bin
 *  must be a multiple of the instrument cycle time.
 */
static void
time_setup()
{
	register int	s = 0, t_bins;

	/* estimate minimal time bin */
	switch (sqwork.hd.nccd) {
		case 1 :	t_scale =  4.0;	break;
		case 2 :	t_scale =  8.0;	break;
		case 3 :
		case 4 :
		default:	t_scale = 16.0;	break;
	}
#ifdef SISDEBUG
	ERRCK2(sqwork.hd.tdelay > 0.0 && t_scale != sqwork.hd.tdelay,
		"Suspicious TIMEDEL = %lf\n", sqwork.hd.tdelay, 0);
#endif SISDEBUG

	/* for a full day in 1CCD mode this'll take ~ 40 iterations */
	do {
		sqwork.hd.tdelta = ++s * t_scale;
		t_bins = (t_stop - t_start) / sqwork.hd.tdelta + 1.0;
	} while (t_bins > TIME_BINS);

	t_scale = 1.0 / sqwork.hd.tdelta;
	sqwork.hd.tbins = t_bins;
}

/*
 *  Use simple telemetry saturation considerations to setup Chips gt.
 *  Alternatively, notice an imposed maximum count per frame threshold.
 */
static void
tlm_sat()
{
	static int	max_cps[3][4] = { { 128, 512, 1024, 128 },
					  {  16,  64,  128,  16 },
					  {   4,  16,   32,   4 } };
	static char	buf[80];
	int		m = mode_hml, n = mode_fbf, max, b;
	Chips		*cp;

	max = (sqwork.mx) ? sqwork.mx : max_cps[m][n];
	(void)sprintf(buf, "** Telemetry Max (max_cps[%d][%d]) = %d **\n",
		m, n, max_cps[m][n]);			sqecho(buf);

	/* maximum number counts per bin ASSUMING 2 SIS in use */
	max = (max * sqwork.hd.tdelta) / (sqwork.hd.nccd * 2.0);

	for (n = 0, cp = sqwork.cs; n < 4; n++, cp++) {
		for (m = 0, b = 0; m < TIME_BINS; m++)
			if (cp->gt[m] >= max) {
				cp->gt[m] = 0;
				b++;
			}	
		if (b) {
			(void)sprintf(buf, "\tChip %d:  %d saturated bins\n",
				n, b);			sqecho(buf);
		}
	}	

	(void)sprintf(buf, "** %.1lf-sec bins, %d CCDs: max/bin = %d\n\n",
		sqwork.hd.tdelta, sqwork.hd.nccd, max);	sqecho(buf);
}

/*
 *  Dump flickering pixel information and prep for plots.
 */
static void
flix_cleanup(status)
	int	*status;
{
	Chips		*cp;
	register int	x, y;
	int		n, c;
	Hist		t, m = 0;

	for (cp = sqwork.cs, c = 0; cp < sqwork.cs + 4; cp++, c++) {
		/* mention alternate thresholds, etc. */
		flix_thre(cp->fq, c);

		n = cp->fq[cp->knk];
		/* dump out flx pixel info  */
		flix_dump(cp->flx, cp->knk, cp->events, n, c,
			cp->satevs + cp->grades + cp->phabov);		

		t = (Hist)cp->knk;
		if (t > m) m = t;

		/* zero out flx pixel value */
		for (x = 0; x < MAX_COLS; x++)
			for (y = 0; y < MAX_ROWS; y++)
				if (cp->xy[x][y] >= t)
					cp->xy[x][y] = 0;
	}

	sqwork.max = (Real)--m;
	*status = 0;
}

/*
 *  Acquire events from the EVENTS extension in batches of
 *  at most n.  A pointer to the last event + 1 is returned.
 */
#define FTGCN(N,M)	do {						\
				(void)strcpy(column, N);		\
				column[5] = ' ';			\
/*				FFTGCNO(fun, zer, column, M, status);*/	\
				ffgcno(fun_cfits, CASEINSEN,		\
				column, M, status);	\
			} while (0)

static Event *
get_events(ev, n, status)
	Event	*ev;
	int	n, *status;
{
	static int	hdu,
			anyn,	*any = &anyn,
			rawx,	*rwx = &rawx,
			rawy,	*rwy = &rawy,
			ccdi,	*ccd = &ccdi,
			grde,	*grd = &grde,
			tyme,	*tim = &tyme,
			detx,	*dtx = &detx,
			dety,	*dty = &dety,
			skyx,	*skx = &skyx,
			skyy,	*sky = &skyy,
			phav,	*pha = &phav;
	static double	dnul = 0.0,	*dd0 = &dnul;
	static short	snul = 0,	*ss0 = &snul;
	static char	cnul = 4,	*cc0 = &cnul,
			gnul = 8,	*gg0 = &gnul,
			column[71] =
  "                                                                      ";

	if (rown == 0) {
/*		FFTMAHD(fun, &sqwork.ex,  &hdu, status);*/
		ffmahd(fun_cfits, sqwork.ex,  &hdu, status);
		FTGCN("X    ",  skx);
		FTGCN("Y    ",  sky);
		FTGCN("DETX ",  dtx);
		FTGCN("DETY ",  dty);
		FTGCN("RAWX ",  rwx);
		FTGCN("RAWY ",  rwy);
		FTGCN("TIME ",  tim);
		if (mode_fbf == 0) {
			FTGCN("PHAS ",  pha);
		} else if (mode_fbf == 1 || mode_fbf == 3) {
			FTGCN("PHA  ",  pha);
			FTGCN("GRADE",  grd);
		}
		FTGCN("CCDID",  ccd);
	}

	if (mode_fbf == 1 || mode_fbf == 3) while
		(!*status && n-- && rown < sqwork.hd.nxis2) {
		rown++;
/*		FFTGCVI(fun, skx, row, wun, wun, ss0, &ev->a, any, status);
		FFTGCVI(fun, sky, row, wun, wun, ss0, &ev->d, any, status);
		FFTGCVI(fun, dtx, row, wun, wun, ss0, &ev->u, any, status);
		FFTGCVI(fun, dty, row, wun, wun, ss0, &ev->v, any, status);

		FFTGCVI(fun, rwx, row, wun, wun, ss0, &ev->x, any, status);
		FFTGCVI(fun, rwy, row, wun, wun, ss0, &ev->y, any, status);
		FFTGCVD(fun, tim, row, wun, wun, dd0, &ev->t, any, status);
		FFTGCVB(fun, ccd, row, wun, wun, cc0, &ev->c, any, status);
		FFTGCVI(fun, pha, row, wun, wun, ss0, &ev->p, any, status);
		FFTGCVB(fun, grd, row, wun, wun, gg0, &ev->g, any, status);*/

		ffgcvi(fun_cfits, skyx, (long) rown, 1L, 1L, snul, &ev->a,
			any, status);
		ffgcvi(fun_cfits, skyy, (long) rown, 1L, 1L, snul, &ev->d,
			any, status);
		ffgcvi(fun_cfits, detx, (long) rown, 1L, 1L, snul, &ev->u,
			any, status);
		ffgcvi(fun_cfits, dety, (long) rown, 1L, 1L, snul, &ev->v,
			any, status);

		ffgcvi(fun_cfits, rawx, (long) rown, 1L, 1L, snul, &ev->x,
			any, status);
		ffgcvi(fun_cfits, rawy, (long) rown, 1L, 1L, snul, &ev->y,
			any, status);
		ffgcvd(fun_cfits, tyme, (long) rown, 1L, 1L, dnul, &ev->t,
			any, status);
		ffgcvb(fun_cfits, ccdi, (long) rown, 1L, 1L, cnul, &ev->c,
			any, status);
		ffgcvi(fun_cfits, phav, (long) rown, 1L, 1L, snul, &ev->p,
			any, status);
		ffgcvb(fun_cfits, grde, (long) rown, 1L, 1L, gnul, &ev->g,
			any, status);

		if (ev->x < MAX_COLS && ev->y < MAX_ROWS &&
			ev->c >= 0 && ev->c < 4 && ev->g < 8) {
			ev++;		/* valid event, so keep it */
			if (sqwork.dopi) evt_ph2pi(ev-1);
		} else {
			dump_evt(ev);
		}
	} else if (mode_fbf == 0) while
		(!*status && n-- && rown < sqwork.hd.nxis2) {
		rown++;
/*		FFTGCVI(fun, skx, row, wun, wun, ss0, &ev->a, any, status);
		FFTGCVI(fun, sky, row, wun, wun, ss0, &ev->d, any, status);
		FFTGCVI(fun, dtx, row, wun, wun, ss0, &ev->u, any, status);
		FFTGCVI(fun, dty, row, wun, wun, ss0, &ev->v, any, status);

		FFTGCVI(fun, rwx, row, wun, wun, ss0, &ev->x, any, status);
		FFTGCVI(fun, rwy, row, wun, wun, ss0, &ev->y, any, status);
		FFTGCVD(fun, tim, row, wun, wun, dd0, &ev->t, any, status);
		FFTGCVB(fun, ccd, row, wun, wun, cc0, &ev->c, any, status);
		FFTGCVI(fun, pha, row, wun, nin, ss0,  ev->s, any, status);*/

		ffgcvi(fun_cfits, skyx, (long) rown, 1L, 1L, snul, &ev->a,
			any, status);
		ffgcvi(fun_cfits, skyy, (long) rown, 1L, 1L, snul, &ev->d,
			any, status);
		ffgcvi(fun_cfits, detx, (long) rown, 1L, 1L, snul, &ev->u,
			any, status);
		ffgcvi(fun_cfits, dety, (long) rown, 1L, 1L, snul, &ev->v,
			any, status);

		ffgcvi(fun_cfits, rawx, (long) rown, 1L, 1L, snul, &ev->x,
			any, status);
		ffgcvi(fun_cfits, rawy, (long) rown, 1L, 1L, snul, &ev->y,
			any, status);
		ffgcvd(fun_cfits, tyme, (long) rown, 1L, 1L, dnul, &ev->t,
			any, status);
		ffgcvb(fun_cfits, ccdi, (long) rown, 1L, 1L, cnul, &ev->c,
			any, status);
		ffgcvi(fun_cfits, phav, (long) rown, 1L, 9L, snul,  ev->s,
			any, status);
		if (ev->x < MAX_COLS && ev->y < MAX_ROWS &&
			ev->c >= 0 && ev->c < 4) {
#ifdef SISDEBUG
			ex_grading(ev++);
#else  SISDEBUG
			do_grading(ev++);
#endif SISDEBUG
			if (sqwork.dopi) evt_ph2pi(ev-1);
		} else {
			dump_evt(ev);
		}
	}

	return(ev);			/* ev pts past valid events */
}

/*
 *  Convert the flickering pixel distribution
 *  density into a cumulative distribution.
 */
static void
flix_cdist(fq)
	int	*fq;
{
	register int	*fp = fq + MAX_FRAMES, *ff = fp - 1;

	while (fp > fq)
		*ff-- += *fp--;
}

/*
 *  Allocate space for n Flick pixels.
 */
static Flick *
flix_alloc(n)
	int	n;
{
	union { char *c; Flick *f; }	p;

	/* lint_laundry */
	p.c = malloc((unsigned)(n * sizeof(Flick)));
	(void)bzero(p.c, (int)(n * sizeof(Flick)));
	return(p.f);
}

/*
 *  Output routine for flickering pixels.
 */
static void
flix_dump(f, k, e, n, c, x)
	Flick	*f;
	int	k, e, n, c, x;
{
	static char	buf[80], fmt[] =
		"FX%d % 7d (%4hd%4hd )\t% 7.1lf\t% 7.1lf\t% 7.1lf\t% .7lg\n";
	register Flick	*g;
	double		mean, duty;
	int		h = 0, mx;

	mx = flix_glob(c, &duty);

	(void)sprintf(buf,
		"** Chip %d : %d pixels for >%d frames <%d frames\n",
		c, n, k, mx);
	sqecho(buf);
	sqecho("** # frames (   x   y )\tdelta-t\tave-pha\tsig-pha\trate\n");

	for (g = f + --n; g >= f; g--) {
		h += g->numb;
		mean = g->mpha / (double)g->numb;
		(void)sprintf(buf, fmt, c, g->numb, g->x, g->y,
			duty * (g->died - g->born) / (double)g->numb,
			mean,
			sqrt((g->spha/(double)g->numb) - mean*mean),
			(double)g->numb / (double)mx);
		sqecho(buf);
	}

	(void)sprintf(buf, "** Chip %d:  discarded %d events\n", c, h+x);
	sqecho(buf);
	(void)sprintf(buf, "** Chip %d:  retained  %d events\n", c, e);
	sqecho(buf);
}

/*
 *  Hacks to correct for relative exposure, lack of frame count:
 *  Return the number of frames on, as approximately max h.p. count.
 */
static int
flix_glob(c, dp)
	int	c;
	double	*dp;
{
	register int	m = MAX_FRAMES, *fq = sqwork.cs[c].fq;

	while (fq[--m] == 0);	/* find last one */

	switch (sqwork.hd.nccd) {
		case 1 :	*dp =  4.0*m;	break;
		case 2 :	*dp =  8.0*m;	break;
		default:
		case 4 :	*dp = 16.0*m;	break;
	}
#ifdef SISDEBUG
	do {
		double	temp = sqwork.hd.expsure / (*dp);
		ERRCK2(temp > 1.02 || temp < 0.98,
			"ONTIME/READOUT = %lf\n", temp, 0);
	} while (0);
#endif SISDEBUG

	*dp /= (sqwork.hd.tstp - sqwork.hd.tstrt);
	return(m);
}

/*
 *  Cleanup on FITS value strings.
 */
static void
str_fix(s)
	char	*s;
{
	register int	n, m;

/*	for (n = 0, m = 0; s[n] && n < KEY_VALUE; n++) {*/
	for (n = 0, m = 0; n < KEY_VALUE; n++) {
		if(s[n] == '\0') return; /* 8/96 */
		if (s[n] != '\'') continue;
		s[n] = ' ';
		if (m == 0) {
			m = 1;
		} else {
			n++;
			break;
		}
	}
	s[--n] = '\0';
}

/*
 *  Alternate pixel flicker thresholds
 */
static void
flix_thre(f, c)
	int	*f, c;
{
	static char	buf[80];

	(void)sprintf(buf, "** Chip %d event breakdown:\n", c);
		sqecho(buf);
	(void)sprintf(buf, "\t flickering events % 8d\n", sqwork.cs[c].flixes);
		sqecho(buf);
	(void)sprintf(buf, "\t saturation events % 8d\n", sqwork.cs[c].satevs);
		sqecho(buf);
	(void)sprintf(buf, "\tlarge-grade events % 8d\n", sqwork.cs[c].grades);
		sqecho(buf);
	(void)sprintf(buf, "\thigh-energy events % 8d\n", sqwork.cs[c].phabov);
		sqecho(buf);
	(void)sprintf(buf, "\t  low-grade events % 8d\n", sqwork.cs[c].events);
		sqecho(buf);

	(void)sprintf(buf, "** Chip %d flickering thresholds:\n", c);
		sqecho(buf);
	(void)sprintf(buf, "\t    vector % 6d\n", threshold_v(f));
		sqecho(buf);
	(void)sprintf(buf, "\t   poisson % 6d\n", threshold_o(f));
		sqecho(buf);
	(void)sprintf(buf, "\t power-law % 6d\n", threshold_p(f));
		sqecho(buf);
	(void)sprintf(buf, "\t  two-step % 6d\n", threshold_s(f, 1));
		sqecho(buf);
	(void)sprintf(buf, "\tthree-step % 6d\n", threshold_s(f, 2));
		sqecho(buf);
	(void)sprintf(buf, "\tupperlimit % 6d\n", threshold_u(f));
		sqecho(buf);
}

/*
 *  Provide a threshold by averaging some choices.
 */
static int
threshold(cp)
	Chips	*cp;
{
	int	*f = cp->fq, tp, ts1, ts2, ts3, ts4, w[5], tpu;

	if (cp->ask > 0)		/* is the Human in control? */
		return(cp->ask);
	/*
	 *  crude:  take an average of the methods....
	 */
	tp  = threshold_p(f);		w[0] =  0;
	ts1 = threshold_s(f, 1);	w[1] = 25;	/* highest ? */
	ts2 = threshold_s(f, 2);	w[2] =  0;
	ts3 = threshold_o(f);		w[3] = 25;	/* lowest ?  */
	ts4 = threshold_v(f);		w[4] = 50;	/* average ? */
	tpu = threshold_u(f);		/* provides an upper limit   */

	if (ts4 > ts1) ts4 = ts1;
/*
	if (tp == MHIST) {
		w[0] =  0; w[1] = 25; w[2] =  0; w[3] = 25; w[4] = 50;
	}
 */
	tp  = w[0]*tp + w[1]*ts1 + w[2]*ts2 + w[3]*ts3 + w[4]*ts4;
	tp /= 100;
	return((tp > tpu) ? tpu : tp);
}

/*
 *  Produce an upper limit on the thresholds.  Here we
 *  place an upper limit on thresholds at nframes/FLIXF,
 *  as long as that limit exceeds FLIXL.
 */
static int
threshold_u(fq)
	int	*fq;
{
	register int	n = MAX_FRAMES + 1;

	while (!fq[--n]);
	n /= FLIXF;
	return((n < FLIXL) ? MHIST : n);
}

/*
 *  Generate a threshold based on Poisson statistics.
 *  (Cf. SISCLEAN method due to Eric Gotthelf.)
 *
 *  This works with the distribution
 *
 *	H(n) == # pixels with n counts
 *	     =  F(n+1) - F(n)
 *	     =  Ho  exp(-m) m^n / n!
 *	F(n) == # pixels with >= n counts
 *	     =  H(n) + F(n+1)
 *  
 *  where F is the cumulative distribution.  This routine
 *  returns a threshold at the 1 count level.
 */
static int
threshold_o(fq)
	int	*fq;
{
	int	hn, hp, n;
	double	mean;
	
	*fq  = NROWS * NCOLS;
	hn   = fq[0] - fq[1];		/* H(0) */
	hp   = fq[1] - fq[2];		/* H(1) */
	mean = (double)hp / (double)hn;
	n    = 1;

	while (hp > 1 && n++ < MAX_FRAMES) {
		hn = hp;
		hp = (double)hn * mean / (double)n;	/* H(n) */
	}

	return(n);
}

/*
 *  Presented with flickering pixel frequencies,
 *  return a suitable threshold.  Returning MHIST
 *  should turn off any further flix pix processing.
 *
 *  This algorithm assumes that the high end of
 *  the distribution is dominated by a distribution
 *  of the form:  F ~ C n^a + H (a < 0) for which
 *
 *	a = (n / F) d(F)/dn
 */
static int
threshold_p(fq)
	int	*fq;
{
	register int	n = MAX_FRAMES + 1;
	int		nht, nlst, flst, hits,
			n2, f2, n3, f3, n4, f4;
	double		asum = 0.0, aave = -1.0, acnt = 0.0, anew;

	/* get the truly hot pixel count estimate */
	while (!fq[--n]);
	nht = (9 * n) / 10 + 1;
        while (n-- > nht);
	nht /= 4;				/* start looking at */
	f4 = f3 = f2 = flst = fq[n];
	n4 = n3 = n2 = nlst = n;
	hits = 0;

	/* accumulate & compare power law index */
	while (--n > 0) {
		if (fq[n] == flst) continue;
		anew = (double) (nlst + n) * (double)(fq[n] - flst) /
                	((double) (fq[n] + flst) * (double) (n - nlst));

		if (n < nht && anew < aave) {
			if (++hits > HITME)     break;
			if (hits == 1)		nht = n + 1;
		} else
			hits = 0;

		flst = f4;		nlst = n4;
		f4 = f3;		n4 = n3;
		f3 = f2;		n3 = n2;
		f2 = fq[n];		n2 = n;

		asum += anew;		acnt += 1.0;
		aave  = asum / acnt;
	}

	if (n <= 1)	n = MHIST;
	else		n = nht;
	return(n);
}

/*
 *  Another approach -- look for the first flat spot where
 *  the distribution is no longer monotonic decreasing.
 */
static int
threshold_s(fq, n)
	int	*fq, n;
{
	register int	*fp = fq + n;

	for (n = 1; *++fq > *++fp && n < MAX_FRAMES; n++);

	if (n == MAX_FRAMES) n = MHIST;
        return(n);
}

/*
 *  Another approach -- look for the first spot where
 *  the vectorial product of two vectors changes sign.
 */
static int
threshold_v(fq)
	int *fq;
{
  double ax, bx, ay, by;
  int    n;
  double vp[4];

  vp[0] = 0;
  vp[1] = 0;
  vp[2] = 0;
  vp[3] = 0;

  for (n = 2; fq[n+1] > 0 && n < MAX_FRAMES-1; n++) {

    ax=(log10((double)fq[n-1])-log10((double)fq[n]));
    bx=(log10((double)fq[n])-log10((double)fq[n+1]));
    ay=(log10((double)n)-log10((double)(n-1)));
    by=(log10((double)n+1)-log10((double)n));

    vp[3] = (ax*by-ay*bx);

    if (vp[1] < 0 && vp[2] > 0 && vp[3] > 0) break;

    vp[0] = vp[1];
    vp[1] = vp[2];
    vp[2] = vp[3];

  }
  return(n);
}

/*
 *  Dump out the histograms to the output file.
 */
static void
dump_hist()
{
	register int	i;
	int		*hp[4];

	sqecho("\n** Flicker Histograms **\n");
	for (i = 0; i < 4; i++)
		hp[i] = sqwork.cs[i].fq + 1;
	for (i = 1; i < MAX_FRAMES; i++) {
		(void)sprintf(com, "%d\t%d\t%d\t%d\t%d\n",
			i, *hp[0]++, *hp[1]++, *hp[2]++, *hp[3]++);
		sqecho(com);
		if (*hp[0] == 0 &&
		    *hp[1] == 0 &&
		    *hp[2] == 0 &&
		    *hp[3] == 0) break;
	}

#ifdef SISDEBUG
	if (!sqwork.op) {
#endif SISDEBUG
	sqecho("\n** Spectral Histograms **\n");
	for (i = 0; i < 4; i++)
		hp[i] = sqwork.cs[i].ph;
	for (i = 0; i < ADU_BINS; i++) {
		(void)sprintf(com, "%d\t%d\t%d\t%d\t%d\n",
			i*8, *hp[0]++, *hp[1]++, *hp[2]++, *hp[3]++);
		sqecho(com);
	}

	sqecho("\n** LightCurve Histograms **\n");
	for (i = 0; i < 4; i++)
		hp[i] = sqwork.cs[i].lc;
	for (i = 0; i < sqwork.hd.tbins; i++) {
		(void)sprintf(com, "%lg\t%d\t%d\t%d\t%d\n",
			i*sqwork.hd.tdelta,
			*hp[0]++, *hp[1]++, *hp[2]++, *hp[3]++);
		sqecho(com);
	}
#ifdef SISDEBUG
	}
#endif SISDEBUG

#ifdef SISEXTRA
	(void)dump_extras();
#endif SISEXTRA
	sqecho("\n** End of Histograms **\n");
}

/*
 *  Convert an event from PH to PI.  Called with ev==0 does a
 *  setup to the PH linear gain of SIS s, Chip c; otherwise
 *  the event ph values are transformed.  The transformation
 *  is not deterministic, due to the random rebinning.
 */
static void
event_ph2pi(ev)
	Event	*ev;
{				/* Chip 0  Chip 1  Chip 2  Chip 3 */
/* v0.2	static double	mult[] = { 3.5654, 3.5740, 3.2888, 3.4686,	/*S0*/
/* v0.2				   3.1875, 3.2696, 3.5178, 3.2495 };	/*S1*/
/* SISRMG v0.4, v0.5: */
	static double	mult[] = { 3.5706, 3.5827, 3.3086, 3.4728,	/*S0*/
				   3.1958, 3.2734, 3.5217, 3.2502 };	/*S1*/
/* SISRMG v0.4, v0.5: */

	static int	sci;
	int		s, c;
	register int	i;

	if (ev) {	/* do the tranform */
		i = 4 * sqwork.hd.sisn + ev->c;
		ev->q = ev->p;
		ev->p = (int)(ev->p * mult[i] + frac());
	} else {	/* do the setup */
		if (sqwork.dopi != 1) {
			sqwork.pi = 0;
			sqwork.gn = 1.0;
			/* get the designated chip by truncation */
			s = (int)(sqwork.pi);
			c = (int)(10.001 * (sqwork.pi - s));
			sci = 4 * s + c;
			if (sci < 0 || sci > 7) return;

			sqwork.dopi = 1;
			sqwork.gn = mult[sci] / 1000.0;
			for (i = 0; i < 8; i++) {
				if (i != sci) mult[i] /= mult[sci];
			}
			mult[sci] = 1.0;
		} else {
			sqecho("\n** PI transform factors follow **\n\n");
			sqecho("\t\tChip 0\tChip 1\tChip 2\tChip 3\n");
			(void)sprintf(com,
				"\tS0\t%.5lf\t%.5lf\t%.5lf\t%.5lf\n",
				mult[0], mult[1], mult[2], mult[3]);
			sqecho(com);
			(void)sprintf(com,
				"\tS1\t%.5lf\t%.5lf\t%.5lf\t%.5lf\n",
				mult[4], mult[5], mult[6], mult[7]);
			sqecho(com);
			sqecho("\n** End of PI transform factors **\n");
		}
	}
}

/*
 *  Random fraction for PH -> PI rebinning.  Not very
 *  random, but that shouldn't matter....Cf Num Rec Chp.7.
 */
static double
frac()
{
	static int	m = 6075, a = 106, c = 1283, s = 0;

	s = (s * a + c) % m;
	return((double)s/(double)m);
}
