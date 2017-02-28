/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/defs.h,v 3.7 1996/08/13 18:22:43 peachey Exp $   */
/*                   */

/*
 filename:	defs.h
 purpose:	header for sqaplot
 author:	Geoffrey B. Crew
 date:		May 1993
 */
/******************************************************************************
Taskname:    sqaplot

Filename:    sqaplot.h

Description: header for sqaplot

Author/Date: Geoffrey B. Crew, 5/93

Modification History:
    James Peachey, HSTX, 8/96 - changed to use native cfitsio library
    - Modified Swork structure to include new element fitsfile *en_fits,
      which replaces element int en (LUN) in cfitsio calls
    - Now includes fitsio.h to use structures and macros in cfitsio library

Notes:

******************************************************************************/

#include <fitsio.h> /* header for cfitsio library */

#define VERSION		0.90
#define FRFVERS		3.018		/* it matters less...	*/
#define SQADATE		"1994/06/23"	/* happy anniversary!	*/

#define MAX_FNAME       256		/* general string use	*/
#define MAX_COLS	480		/* with room for crap	*/
#define MAX_ROWS	448		/* 468 cols x 441 rows	*/
#define NCOLS		420		/* number of image x	*/
#define NROWS		422		/* number of image y	*/
#define MAX_FRAMES	8192		/* flicker statistics	*/
#define ADU_BINS	512		/* for quicky spectra	*/
#define TIME_BINS	512		/* for quicky lght crv	*/
#define KEY_VALUE	24		/* len for keyword value*/
#define NCONTOURS	8		/* in gray-scale plot	*/
#define HKKWORDS	100		/* number of HK words	*/
#define PRIWORDS	29		/* number pri keywords	*/

typedef float Real;			/* a FITS data type	*/
typedef unsigned short Hist;		/* for counting stats	*/

/*
 *  Structure for relevant CCD event info
 */
struct event {
	double	t;			/* time			*/
	short	x, y, p, s[9], q;	/* h,v,pha/pi,phas,pha	*/
	short	a, d, u, v;		/* ra,dec,detx,dety	*/
	char	c, g;			/* chip,grade		*/
};
typedef struct event Event;
/*
 *  Structure to capture hot pixel info
 */
struct flick {
	double	born,			/* first time it is on	*/
		died,			/* last time it is on	*/
		mpha,			/* mean, sigma sums	*/
		spha;			/* of flickering event	*/
	int	numb;			/* number of times on	*/
	short	x, y;			/* it's coordinates	*/
};
typedef struct flick Flick;
/*
 *  Structure for chip statistics
 */
struct chips {
	Hist	xy[MAX_COLS][MAX_ROWS];	/* image/good pixel map	*/
	Flick	*flx;			/* flicker statistics	*/
	int	knk, ask,		/* flicker threshold	*/
		fq[MAX_FRAMES],		/* flicker frequencies	*/
		hot,			/* really hot count	*/
		ph[ADU_BINS],		/* binned spectra	*/
		early,			/* oob before interval	*/
		lc[TIME_BINS],		/* simple light curves	*/
		later,			/* oob after interval	*/
		gt[TIME_BINS],		/* good time intervals	*/
		flixes,			/* flicker events	*/
		satevs,			/* saturation events	*/
		grades,			/* large-graded events	*/
		phabov,			/* large-pha faint evts	*/
		events;			/* real events		*/
};
typedef struct chips Chips;

/*
 *  Structure to hold all FITS (and other) derived information.
 */
struct head {
	double	tstrt, tstp, tdelta, expsure, tdelay;
	int	nvents, nxis2, phabins, sisn, nccd, tbins;
	char	instrume[KEY_VALUE],
		origin	[KEY_VALUE],
		object	[KEY_VALUE],
		seqpi	[KEY_VALUE],
		ra_nom	[KEY_VALUE],
		dec_nom	[KEY_VALUE],
		equinox	[KEY_VALUE],
		telescop[KEY_VALUE],
		tstart  [KEY_VALUE],
		tstop   [KEY_VALUE],
		exposure[KEY_VALUE],
		nevents [KEY_VALUE],
		date_obs[KEY_VALUE],
		time_obs[KEY_VALUE],
		date_end[KEY_VALUE],
		time_end[KEY_VALUE],
		bit_rate[KEY_VALUE],
		datamode[KEY_VALUE],
		date	[KEY_VALUE],
		author	[KEY_VALUE],
		orbit0	[KEY_VALUE],
		orbit1	[KEY_VALUE],
		tlm_file[KEY_VALUE],
		timedel [KEY_VALUE],
		naxis2  [KEY_VALUE],
		pha_bins[KEY_VALUE],
		ccdlst0 [KEY_VALUE],
		ccdlst1 [KEY_VALUE],
		ccdpows0[KEY_VALUE],
		ccdpows1[KEY_VALUE],
		ccdmode0[KEY_VALUE],
		ccdmode1[KEY_VALUE],
		ccdlist0[KEY_VALUE],
		ccdlist1[KEY_VALUE],
		nothing_here;
};
typedef struct head Head;

/*
 *  Handle for globals
 */
struct swork {
	Real	version,		/* why not...		*/
		rg,			/* for image coloring	*/
		pi,			/* chip of PI ph values	*/
		gn,			/* nominal PI gain val	*/
		ek[2],			/* for echo corrections	*/
		frfvers;		/* of Frfread tested	*/
	char	date[11];
	int	fd,			/* output file descrip	*/
		fe,			/* events fortran unit	*/
		nc,			/* number contours	*/
		sm,			/* number of smoothings	*/
		st,			/* split threshold	*/
		sy,			/* echo correct style	*/
		gm,			/* maximum grade 	*/
		ts,			/* telemetry saturation	*/
		mx,			/* max counting rate	*/
		rd,			/* raw image display	*/
		ex;			/* FITS sci. extension	*/
	char	in[MAX_FNAME],		/* input file name	*/
		x, *inp,
		ou[MAX_FNAME],		/* output file name	*/
		y, *oup,
		dv[MAX_FNAME],		/* pgplot device name	*/
		z, *dvp,
		ev[MAX_FNAME],		/* event file name	*/
		w, *evp,
		es[MAX_FNAME],		/* echo secular fname	*/
		u, *esp,
		ct[MAX_FNAME],		/* cti secular fname	*/
		v, *ctp,
		zzz;
	Chips	*cs;			/* quick-analized data	*/
	Real	max;			/* max counts / pixel	*/
	Head	hd;			/* various FITS info	*/
	int	op;			/* one pass flag	*/
	int	dopi;			/* do the pi bit flag	*/
	fitsfile *fe_cfits;		/* for cfitsio, replaces element fe */
};
typedef struct swork Swork;

#ifdef unix
# ifndef SISDEBUG
#  define SQAPLT	sqaplt_		/* compatibilty */
#  define get_science	sqasci_
#  define do_picture	sqapic_
#  define do_grading	sqagrd_
#  define grade_setup	sqagsp_
#  define get_echos	sqaech_
#  define get_ph2pi	sqap2p_
#  define do_pi		sqadpi_
#  define ex_events	sqaexe_
#  define ex_grading	sqaexg_
# else
   extern void		parse_extras();
# endif SISDEBUG
# define gr_setup	sqastp_
# define ka_grading	sqadgr_
#endif unix

#ifdef vms
# define SQAPLT		sqaplt		/* compatibilty */
# define get_science	sqasci
# define do_picture	sqapic
# define do_grading	sqagrd
# define grade_setup	sqagsp
# define get_echos	sqaech
# define get_ph2pi	sqap2p
# define gr_setup	sqastp
# define ka_grading	sqadgr
# define do_pi		sqadpi
# define ex_events	sqaexe
# define ex_grading	sqaexg
#endif vms

#define F_IUNIT		37		/* for fitsio	*/
#define F_OUNIT		38		/* for fitsio	*/

#define PC(H)		( ((H)<   0) ? 	0		   :	\
			  ((H)<1024) ? (H)		   :	\
			  ((H)<2048) ? 1024 + ((H)-1024)/2 :	\
			  ((H)<4096) ? 1536 + ((H)-2048)/4 :	\
				       2047			)	
#define PH(C)		( ((C)<   0) ? 	0		   :	\
			  ((C)<1024) ? (C)		   :	\
			  ((C)<1536) ? 1024 + ((C)-1024)*2 :	\
			  ((C)<2048) ? 2048 + ((C)-1536)*4 :	\
				       4095			)	

extern void		sqecho(), get_science(), do_picture(),
			grade_setup(), do_grading(),
			ex_events(), ex_grading(),
			get_echos(), get_ph2pi();

extern Swork		sqwork;
