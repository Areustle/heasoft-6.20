
/*
 filename:	defs.h
 purpose:	response matrix generator global definitions
 author:	Geoffrey B. Crew
 date:		November 1993
 modified:	Jeff Guerber, GSFC/HSTX, Apr. 1996.  To native-C fitsio.
                Jeff Guerber, Dec. 1997. Added C_CDB.
 */

#include <stdio.h>  /* fitsio.h needs this */

#include "fitsio.h"  /* cfitsio (native-C version) */

/*
 *  Include all the stuff that might be needed for compatibility.
 *  The rest of the definitions/declarations should be generic.
 */
#include "compat.h"

/*
 *  The following definitions are for response Point configuration.
 */
#define MAX_FEATS	64	/* max # resp features	*/
#define	MAX_PARAM	6	/* their max # of args	*/
#define	NUM_RANGE	6	/* # of energy ranges	*/
#define	NUM_TYPES	12	/* of grades & gains	*/
#define FAR_AWAY	1.2E+34	/* max metric distance	*/
#define MAX_MESS	1600	/* max message size	*/
#define VERSION		1.1	/* of SISRMG, not mods	*/
#define DATE		"04/97"	/* of configuration	*/
#define SISsCHIPc	41	/* first default point	*/

/*
 *  The following definitions control the level of chattiness.
 *  And a few other things as well....
 */
#define C_ZIP		0x0000	/* no chatr output	*/
#define C_INP		0x0001	/* input chatr output	*/
#define C_PNT		0x0002	/* points chatr output	*/
#define C_LOP		0x0004	/* rmloop chatr output	*/
#define C_EBD		0x0008	/* rmebxt chatr output	*/
#define C_PRI		0x0010	/* primary chatr output	*/
#define C_KEY		0x0020	/* echo input keywords	*/
#define C_XRT		0x0040	/* echo xrt response	*/
#define C_RQT		0x0080	/* RQT vals are request	*/
#define C_ARF		0x0100	/* multiply RMF by ARF	*/
#define C_CAL		0x0200	/* calibration comments	*/
#define C_CDB           0x0400  /* extra CALDB chatter  */
#define C_HLP		0x1000	/* generate help text 	*/
#define C_NUL		0x8000	/* flag it as not set	*/
#define C_BAD(C)	((C)&(~(C_INP|C_PNT|C_LOP|C_EBD|C_PRI|\
				C_KEY|C_XRT|C_RQT|C_ARF|C_CAL|C_HLP)))
#define C_NONE		C_KEY
#define C_SOME		C_HLP|C_RQT|C_KEY|C_EBD|C_LOP
#define C_LOTS	        C_HLP|C_CAL|C_RQT|C_KEY|C_PRI|C_EBD|C_LOP|C_PNT|C_INP|C_CDB

/*
 *  The following definitions specify the meaning of the grade mask.
 */
#define MSK_FAINT	0x0001	/* faint e-ph mapping	*/
#define MSK_FTBT0	0x0002	/* faint/bright  S	*/
#define MSK_FTBT1	0x0004	/* faint/bright  S+	*/
#define MSK_FTBT2	0x0008	/* faint/bright  Pv	*/
#define MSK_FTBT3	0x0010	/* faint/bright  Pl	*/
#define MSK_FTBT4	0x0020	/* faint/bright  Pr	*/
#define MSK_FTBT5	0x0040	/* faint/bright  P+	*/
#define MSK_FTBT6	0x0080	/* faint/bright  L+Q	*/
#define MSK_FTBT7	0x0100	/* faint/bright  O	*/
#define MSK_BRITE	0x0200	/* fast/bright e-ph map	*/
#define MSK_FAST0	0x0400	/* fast grade    S	*/
#define MSK_FAST1	0x0800	/* fast grade    O	*/
#define MSK_RBIN1	0x1000	/* rebin factor bit 1	*/
#define MSK_RBIN2	0x2000	/* rebin factor bit 2	*/
#define MSK_RBIN3	0x4000	/* rebin factor bit 3	*/
#define MSK_RBIN4	0x8000	/* rebin factor bit 4	*/

#define MSK_FTBTX	0x01FE	/* union of FTBT grades */
#define MSK_FASTX	0x0C00	/* union of FAST grades */
#define MSK_GRADE	(MSK_FTBTX|MSK_FASTX)
#define MSK_REBIN	(MSK_RBIN1|MSK_RBIN2|MSK_RBIN3|MSK_RBIN4)
#define MSK_EBNDS	(MSK_FAINT|MSK_BRITE)

#define MSK_RB(G)	( ((G) & MSK_REBIN) >> NUM_TYPES )
#define MXOR(A,B)	( ( (A) && !(B) ) || ( (B) && !(A) ) )
#define MSK_OK(G)	( MXOR((G)&MSK_FAINT,(G)&MSK_BRITE) )

#define DM_BRIGHT2	12	/* datamode */
#define DM_BRIGHT	2	/* datamode */
#define DM_FAST		3	/* datamode */
#define DM_UNKNOWN	0	/* datamode */

#define DFE_OLD		0	/* zerodef */
#define DFE_NEW		1	/* zerodef */
#define DFE_BRIGHT	2	/* zerodef */
#define DFE_UNKNOWN	3	/* zerodef */

/*
 *  The following definitions are useful for the response calculations.
 */
#define EMIN		 0.2			/* mn energy	*/
#define EMAX		12.0			/* mx energy	*/
#define KEVE		0.00365			/* keV / e-	*/
#define EKEV		273.972602739726027397  /* e- / keV	*/
#define EKESC		1.7400			/* Si K esc E	*/
#define EEDGE		1.8400			/* Si K edge E	*/
#define M_IS2PI	(M_2_SQRTPI*M_SQRT2*0.25)	/* (2 pi)^-1/2	*/

/*
 *  The following definitions are for the FITS manifestation of RMF.
 */
#define MAX_NGRPS	128		/* max # chan. subsets	*/
#define MAX_FNAME	256		/* max length file name	*/
#define RM_MAX		0.10		/* hope 0 < RM < RM_MAX	*/
#define B_RANGE		32760.0		/* |TAPE| <= +- B_RANGE	*/
#define LO_THRES	(RM_MAX*1.0e-06)/* min RM value to keep	*/
#define RM_THRES	(LO_THRES*0.005)/* min Gcomp to include	*/

/*
 *  The following typedefs help sort out the various levels in the
 *  configuration of the instrument response.  Refer to sp_0000.k.
 */
typedef float Real;		/* final FITS data type	*/

#define Rincr void		/* routines that will	*/
typedef void (*Rincp)();	/* += response matrix	*/
typedef double Pfunc;		/* routines to generate	*/
typedef double (*Pfunp)();	/* required parameters	*/

struct gcomp {			/* grade-comp of resp	*/
	Rincp	f;		/* function to call w/	*/
	Pfunp	p[MAX_PARAM];	/* argument parameters	*/
};
typedef struct gcomp Gcomp;	/* grade-comp of resp	*/
typedef Gcomp *Gcptr;		/* grade-comp of resp	*/

struct range {			/* of parametrized resp	*/
	Real	elow,		/* lower response bound	*/
		eupp;		/* upper response bound	*/
	Gcptr	*gc[NUM_TYPES];	/* response feature tbl	*/
	int	null;		/* paranoia termination	*/
};
typedef struct range Range;	/* of parametrized resp	*/

struct point {			/* configuration space	*/
	double	vers,		/* version of analysis	*/
		epch,		/* of instrument life	*/
		evth,		/* event threshold	*/
		spth,		/* split threshold	*/
		xcen,		/* x/h/column center	*/
		xwid,		/* x/h/column width	*/
		ycen,		/* y/v/row    center	*/
		ywid,		/* y/v/row    width	*/
		temp,		/* focal plane temp (K)	*/
		evpf,		/* events per frame	*/
		imhi,		/* image clock hi	*/
		imlo,		/* image clock lo	*/
		echo,		/* echo factor applied	*/
		leak,		/* average light leak	*/
		dark;		/* ave dark frame error	*/
	int	detr,		/* sis#  0(5),1(3),2,4	*/
		chip,		/* chip   0,1,2,3	*/
		mode;		/* 0=mf,1=m1,2=m2,4=m4	*/
	struct point *me;	/* for config checking	*/
	char	*id, *ego;	/* short/long names	*/
	Range	*rp[NUM_RANGE],	/* ptr to range table	*/
		*null;		/* set null termination	*/
	int	(*load)();	/* for private config	*/
};
typedef struct point Point;	/* configuration space	*/

struct plist {
	struct plist	*n;	/* to point to the next	*/
	Point		*p;	/* to point to a point	*/
};
typedef struct plist Plist;

/*
 *  Collected lookup table definitions
 */
struct table {
	int	k,		/* previous evaluation	*/
		l;		/* last lookup index	*/
	double	*x,		/* array of abscissa	*/
		*y,		/* array of ordinates	*/
		(*f)();		/* evaluation function	*/
};
typedef struct table Table;
#define	LK_LINLIN	1	/* linear search lintrp	*/
#define LK_BINLIN	2	/* binary search lintrp	*/
#define LK_LETAIL1	3	/* get out the shoehorn	*/
#define LK_LETAIL2	4	/* get out the shoehorn	*/
#define LK_HETAIL	5	/* get out the shoehorn	*/

/*
 *  The following structure holds all externally required
 *  information.  Note that the response matrix is effectively
 *  of size e_bin x phbin, where e_min,e_max are the extremal
 *  energies of the extremal energy bins, and the pha values
 *  are enumerated from phmin through phmax, inclusive.
 */
struct rwork {
	Real	*evals,		/* input energy values	*/
		*earea,		/* effective area array	*/
		eamax,		/* effective area max	*/
		*pharr,		/* working matrix space	*/
		*ebnds,		/* working ebound array	*/
		*pcarr,		/* cmpresd matrix space	*/
		*ecnds,		/* cmpresd ebound array	*/
		e_min,		/* minimum model energy	*/
		e_max;		/* maximum model energy	*/
	int	e_bin,		/* number o energy bins	*/
		chatr,		/* level of chattiness	*/
		gmask,		/* mask of resp. grades	*/
		phmin,		/* min requested ph out	*/
		phmax,		/* max requested ph out	*/
		phbin,		/* total number ph bins	*/
		rebin,		/* ph rebinning factor	*/
		first,		/* output channel conv	*/
		gains,		/* ebnds gain smoothing	*/
		pcmin,		/* compressed phmin	*/
		pcmax,		/* compressed phmax	*/
		pcbin,		/* compressed phbin	*/
		sisde,		/* inc. detector eff.	*/
		sispi,		/* use PI channels	*/
		rddcv,		/* RDD correction vers	*/
		dfezd;		/* FAINTDFE zerodef	*/
	char	in[MAX_FNAME],	/* RMG input file name	*/
		*inp,		/* pts to private copy	*/
		ar[MAX_FNAME],	/* ARF input file name	*/
		*arp,		/* pts to private copy	*/
		rm[MAX_FNAME],	/* RMF output file name	*/
		*rmp,		/* pts to private copy	*/
		dd[MAX_FNAME],	/* root data directory	*/
		*ddp,		/* pts to private copy	*/
		f0[MAX_FNAME],	/* file 0: arf energies	*/
		*f0p,		/* pts to private copy	*/
		f1[MAX_FNAME],	/* file 1: gaussian	*/
		*f1p,		/* pts to private copy	*/
		f2[MAX_FNAME],	/* file 2: diplodicus	*/
		*f2p,		/* pts to private copy	*/
		f3[MAX_FNAME],	/* file 3: grade branch	*/
		*f3p,		/* pts to private copy	*/
		gd[MAX_FNAME],	/* ECD gaus/dipl data	*/
		*gdp,		/* pts to private copy	*/
		ct[MAX_FNAME],	/* ph to pi trans. file	*/
		*ctp,		/* pts to private copy	*/
		eh[MAX_FNAME],	/* echo secular hi file	*/
		*ehp,		/* pts to private copy	*/
		rd[MAX_FNAME],	/* rdd secular his file	*/
		*rdp,		/* pts to private copy	*/
		zzzzzzzz;	/* I forget what it's 4 */
};
typedef struct rwork Rwork;

#ifdef SIS_SHORT
#	define MAX_PBINS	1024	/* max number pha bins	*/
#	define MAX_EBINS	1024	/* max number e   bins	*/
#else
#	define MAX_PBINS	4096	/* max number pha bins	*/
#	define MAX_EBINS	4096	/* max number e   bins	*/
#endif /* SIS_SHORT */
#define MAX_EBNDS	MAX_PBINS+1	/* for holding EBOUNDS	*/
#define MAX_EVALS	MAX_EBINS+1	/* for holding Evalues	*/

#define PAR(G,N,E)	(G->p[N](E,(Point*)0))	/* get param */
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

extern void	get_point(),		/* in database	*/
		rmf_array(),		/* output FITS	*/
		rmf_ebnds(),		/* output FITS	*/
		cal_stuff(),		/* caltools io	*/
		get_ph2pi(),		/* CTI secular	*/
		get_echos(),		/* echo secular	*/
		get_rddis(),		/* RDD secular	*/
		get_nucti();		/* CTI secular	*/
extern Table	*get_brnch(),		/* file access	*/
		*get_ecd();		/* FITS access	*/

extern Rwork	rmg_works;		/* work struct	*/

extern char	*string_fix(),		/* fix str i/o  */
		*string_cat();		/* ddir / file  */


/*
 *  cfitsio file structures
 */

extern fitsfile  *f_eunit, *f_iunit, *f_ounit;

/* passed to getpha.f for fitsio, not used otherwise */
#define F_LUNIT  39

/*
 *  Pointers to the calibration database Points follow.
 */
#define sunimret p_0001
extern Plist sunimret, terminus;
#define p_0050 terminus

#ifdef SISRMG_C
/*
 *  Disclaimers, etc.
 */
static char *disclaimer[] = {
	" ",
	"THE SIS RESPONSE CALIBRATION PROGRAM IS STILL IN PROGRESS.",
	"THESE MATRICES REFLECT OUR CURRENT UNDERSTANDING OF THE SIS",
	"RESPONSE, BUT THERE ARE SOME KNOWN DEFICIENCIES.",
	"For more information, on the use of sisrmg, or the current",
	"calibration status, or to report problems with these matrices",
	"contact:       Geoffrey B. Crew, MIT/CSR, gbc@space.mit.edu",
	"               http://space.mit.edu/~gbc",
	"the ASCA GOF:  ascahelp@athena.gsfc.nasa.gov",
	"               http://legacy.gsfc.nasa.gov/docs/asca/cal_probs.html",
	" ",
  "" }; static char *rel_note[] = {
	"This version resolves *all* known keyword incompatibilities with",
	"other FTOOLS tasks.  If you have previously fudged *any* keywords",
	"in the PHA file to compensate, incorrect results will follow.",
	" ",
  "" };
/*
 *  GET_PHA magic
 */
# define  fi_lun ( 1 - 1 )
# define  fi_inst ( 2 - 1 )
# define  fi_chip ( 3 - 1 )
# define  fi_rawx ( 4 - 1 )
# define  fi_rawy ( 5 - 1 )
# define  fi_dmode ( 6 - 1 )
# define  fi_cmode ( 7 - 1 )
# define  fi_event_th ( 8 - 1 )
# define  fi_split ( 9 - 1 )
# define  fi_gmask ( 10 - 1 )
# define  fi_tr_grade ( 11 - 1 )
# define  fi_zerodef ( 12 - 1 )
# define  fi_pi ( 13 - 1 )
# define  fi_chan1 ( 14 - 1 )
# define  fi_n_chan ( 15 - 1 )
# define  fi_bright ( 16 - 1 )
# define  fi_chip_in ( 17 - 1 )
# define  fi_rddcv ( 18 - 1 )
# define  fi_ascatime ( 1 - 1 )

#endif SISRMG_C
