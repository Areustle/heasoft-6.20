/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/sp_0031.c,v 3.13 2001/12/06 21:18:33 irby Exp $   */
/*                   */
/*
 filename:      sp_0031.c
 purpose:       Calibration Point 0031
 author:        gbc@space.mit.edu
 date:		Tue Apr 29 13:27:05 EDT 1997
 modified:      jeff.guerber@gsfc.nasa.gov, Dec. 1997: loader(): only try ascii
                ecd's if not using caldb.
 */
#include "defs.h"

/*
 *  The following functions are accessed via tables
 *  which are defined at the end of this file.  The
 *  only global symbol defined herein is Plist p_*.
 */

static void	fixit(), xfixit();
static double	last_en = -1.0, last_qe;
static Pfunc	part_qe;


/*
 *  For each grade, we swap in a set of Tables which
 *  are interpolated to obtain the gaussian parameters.
 *
 *  qe_cache gets set to qe???? defined at end of file.
 */
#define SetGrade(N,T,D)	static Table	* T , * D ;			\
			static Rincr N (e, g)				\
				double  e; Gcomp   *g;			\
			{						\
				if (e != last_en) (*qe_cache)(e, g);	\
				branch = T;				\
				diplod = D;				\
			}
static Table	*branch, *diplod;
static Rincr	(*qe_cache)();
SetGrade(gr_0, br_0, dp_0)
SetGrade(gr_1, br_1, dp_1)
SetGrade(gr_2, br_2, dp_2)
SetGrade(gr_3, br_3, dp_3)
SetGrade(gr_4, br_4, dp_4)
SetGrade(gr_5, br_5, dp_5)
SetGrade(gr_6, br_6, dp_6)

/*
 *  The Rincr functions are called for each energy
 *  for which the model flux will be evaluated; they
 *  should call their Gcomp functions for each energy
 *  in order to get the appropriate parameter values.
 *
 *  There are two flavors of such functions; the first
 *  provides the energies for the RMF EBOUNDS extension.
 *
 *  Here we want to capture energies that lie near the peaks.
 */
/* ARGSUSED */
static Rincr
ebnds(e, g)
	double	e;
	Gcomp	*g;
{
	register Real	*p, max = 0.0;
	static int	cnt = 0;
	int		m = -1;

	for (p = rmg_works.pharr + rmg_works.phmax; p > rmg_works.pharr; )
		if (*--p > max) {
			m = p - rmg_works.pharr;
			max = *p;
		}
	if (m < 0) return;

	/* max == rmg_works.pharr[m] becomes == 0.0 at peak */
	max = (rmg_works.pharr[m-1] - rmg_works.pharr[m+1]) /
		(rmg_works.pharr[m-1] + rmg_works.pharr[m+1] - 2*max);
	if (max < 0.0) max = -max;

	/* if (max < (Real)0.5 / rmg_works.rebin) { */
	if (max < (Real)0.05) {
		rmg_works.ebnds[m] = (Real)e;
		if (rmg_works.chatr & C_EBD) {
			ERRCHK(1, "P[%d] ",m,0);
			ERRCHK(1, "= %g",(Real)e,0);
			ERRCHK(1, "%c", (++cnt % 4 == 0) ? '\n' : '\t', 0);
		}
	}
}

/*
 *  The low energy tail is applied as a final feature.  The shape
 *  is determined by the lookup routine'' LK_LETAIL, but it should
 *  be renormalized by the total efficiency at this point.  Also, it
 *  should be truncated when its value becomes comparable to the other
 *  accumulated response features.  The relative quantum efficiency
 *  has accumulated in 0.0 < part_qe < 1.0
 */
static double	(*lowe_tail)();
static Real	fgain, noisf;
/* ARGSUSED */
static Rincr
ltail(e, g)
	double  e;
	Gcomp   *g;
{
	static Real	tailf[4096];
	static int	count = 0;
	register Real	enf = (Real)e;
	register int	k;

	part_qe *= (last_qe / M_IS2PI);		/* i.e. t_qe	*/

	if ((lowe_tail(tailf, enf, fgain, noisf) != 0.0) ||
		(part_qe == 0.0)) return;	/* not likely	*/

	for (k = rmg_works.phmin; k < rmg_works.phmax; k++) {
		enf = tailf[k] * part_qe;	/* renormalize	*/
		if (enf == 0.0) break;
		rmg_works.pharr[k] += enf;	/* increment it	*/
	}
	if (rmg_works.chatr & C_CAL && (count++ % 8) == 0) {
		ERRCHK(rmg_works.chatr & C_LOP|C_EBD , "\n", 0, 0);
		ERRCHK(1, "LowE tail < %d ADU ", k, 0);
		ERRCHK(1, "@ %.3lf keV\n", e, 0);
	}
}

/*
 *  Include the High Energy PH tail thingy.
 *  This is a modified version of diplodicus.c to do it as
 *  a one-shot calculation.
 */
static double	dipars[14];		/* many parameters */
static Rincr
diplo(e, g)
	double	e;
	Gcomp	*g;
{
	register Real	*rmv;
	register int	pha;
	int		phmin, phmax, phx;
	double		tmp;

	if ((tmp = PAR(g,0,e)) <= 0.0) return;

	pha = (int)dipars[12];
	phmin = rmg_works.phmin - (int)dipars[0];
	phmax = rmg_works.phmax - (int)dipars[0];
	if (phmin < pha) phmin = pha;

	rmv = rmg_works.pharr + (int)dipars[0] + phmin;

	phx = (int)dipars[1];
	if (phx > phmax) phx = phmax;
	for (pha = phmin; pha < phx; pha++) {
		tmp =  (dipars[8] -
			dipars[9] * pow((double)(-pha), dipars[2]));
		*rmv++ += (Real)(dipars[6]*exp(tmp));
	}
	phx = (int)dipars[3];
	if (phx > phmax) phx = phmax;
	for ( ; pha < phx; pha++) {
		tmp = - (double)(pha * pha) * dipars[7];
		*rmv++ += (Real)(dipars[6]*exp(tmp));
	}
	for ( ; pha < phmax; pha++) {
		tmp =  (dipars[10] -
			dipars[11] * pow((double)(pha), dipars[4]));
		*rmv++ += (Real)(dipars[6]*exp(tmp));
	}
}

/*
 *  Enforce the event threshold.
 */
static int	event_thresh = 0;
/* ARGSUSED */
static Rincr
evthr(e, g)
	double  e;
	Gcomp   *g;
{
	register int	pha;

	for (pha = rmg_works.phmin; pha < event_thresh; pha++)
		rmg_works.pharr[pha] = 0.0;
}

/*
 *  Miscellaneous diagnostics.
 */
/* ARGSUSED */
static int diparmerrs = 0;
static Rincr
diags(e, g)
	double  e;
	Gcomp   *g;
{
	ERRCHK((rmg_works.chatr & C_LOP) && diparmerrs,
		"%d diplodicus (HEtail) violations\n", diparmerrs, 0);
	diparmerrs = 0;
}

/*
 *  The second flavor computes a response feature at
 *  all pulse heights for a particular input energy.
 */
/* ARGSUSED */
static double	hold_cen, hold_wid;
static Rincr
gausa(e, g)
	double	e;
	Gcomp	*g;
{
	register Real	*rmv;
	register int	pha;
	int		phmin, phmax;
	double		nor, z;

	if ((nor = PAR(g,0,e)) <= 0.0) return;
	if ((z = log(nor/RM_THRES)) < 0.0) return;

	phmax = hold_wid * sqrt(z);	/* * 1.42 ~ sqrt(2) */
	phmin = hold_cen - phmax;
	phmax += hold_cen;

	if (phmin < rmg_works.phmin) phmin = rmg_works.phmin;
	if (phmax > rmg_works.phmax) phmax = rmg_works.phmax;

	rmv = rmg_works.pharr + phmin;
	for (pha = phmin; pha <= phmax; pha++) {
		z = (pha - hold_cen) / hold_wid;
		z = nor * exp(- z * z * 0.5);
		*rmv++ += (Real)z;
	}
}

/*
 *  The Pfunc functions will be called once to perform
 *  any initializations necessary for optimal response
 *  on future calls; thereafter, they should be called
 *  as functions of energy only (p == NULL), and should
 *  provide the value of the relevant parameter.
 */
/* ARGSUSED */
static Pfunc
thresh(e, p)
	double  e;
	Point   *p;
{
	if (p) event_thresh = (int)p->evth;
	return(0.0);	/* ignored */
}

#define	SetParam(N,I)	static Pfunc N (e, p)				\
				double  e; Point   *p;			\
			{						\
				register Table	*t = branch + I;	\
				register Pfunc	v;			\
									\
				if (p)	return(0.0);	/* init */	\
				/* setup hold_cen, hold_wid */		\
				v = t->f(e, t);		t++;		\
				if (v == 0.0) return(0.0);		\
				/* incr partial qe for tail */		\
				/* but only for Primary rsp */		\
				if (I < 6 || I > 18) part_qe += v;	\
				hold_cen = t->f(e, t);	t++;		\
				hold_wid = t->f(e, t);	t++;		\
				/* and return normalization */		\
				return(v * last_qe / hold_wid);		\
			}
SetParam(aprime, 1)
SetParam(bprime, 4)
SetParam(aescap, 7)
SetParam(bescap,10)
SetParam(afluor,13)
SetParam(bfluor,16)
SetParam(aextra,19)
SetParam(bextra,22)

/*
 *  Precompute all the diplodicus parameters.
 */
static Pfunc
diparm(e, p)
	double  e;
	Point   *p;
{
	register Table	*t = diplod + 1;
	register int	i;

	if (p)	return(0.0);    /* init */
	if (!diplod) return(dipars[6] = 0.0);

	for (i = 0; i < 7; i++, t++) dipars[i] = t->f(e, t);
	/* Check--didn't find par2=0 or par4=0 in any files--so: */
	if (0.0 < dipars[1] || 0.0 > dipars[3] ||
	    dipars[2] == 0.0 || dipars[4] == 0.0) {
		dipars[6] = 0.0; ++diparmerrs;
		ERRCHK(1, "Dipars interpolation error.\n", 0, 0);
	}

	if (dipars[6] <= 0.0) return(dipars[6] = 0.0);

	/* additional dipars[] */
	dipars[7] = 1.0 / (2.0 * dipars[5] * dipars[5]);
	dipars[8] = /* n1 - */
	  (dipars[1]*dipars[1])/(dipars[5]*dipars[5])*(1.0/dipars[2]-0.5);
	dipars[9] = /* n2 - */
	  pow(- dipars[1],2.0-dipars[2])/(dipars[2]*dipars[5]*dipars[5]);
	dipars[10] = /* n1 + */
	  (dipars[3]*dipars[3])/(dipars[5]*dipars[5])*(1.0/dipars[4]-0.5);
	dipars[11] = /* n2 + */
	  pow(  dipars[3],2.0-dipars[4])/(dipars[4]*dipars[5]*dipars[5]);

	/* derived pha limits:  dipars[0] + dipars[12/13] */
	dipars[12] = (dipars[6] < RM_THRES) ? dipars[1] : - exp(
	  log((log(dipars[6]/RM_THRES) + dipars[8]) / dipars[9]) / dipars[2]
	);
	dipars[13] = (dipars[6] < RM_THRES) ? dipars[3] : exp(
	  log((log(dipars[6]/RM_THRES) + dipars[10]) / dipars[11]) / dipars[4]
	);
	if (dipars[12] > dipars[1]) {
		ERRCHK(1, "Dipars argument 12 error.\n",0,0);
		dipars[12] = dipars[1]; ++diparmerrs;
	}
	if (dipars[13] < dipars[3]) {
		ERRCHK(1, "Dipars argument 13 error.\n",0,0);
		dipars[13] = dipars[3]; ++diparmerrs;
	}

	return(dipars[6]);
}

/*
 *  Loader routines to load calibration files...
 *  p is the target (supplied by input)
 *  r is our local calibration response point.
 */
#define CT_C(T,I)	if (T) do {/* code to correct center for CTI */	\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--)					\
				t->y[i] = (*phx)(t->y[i],x,y);		\
			} while(0);

#define CT_W(T,C,I)	if (T) do {/* fix width relative to center */	\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--)	{				\
				c = (T+C)->y[i];			\
				t->y[i] = (*phx)(c+t->y[i],x,y)		\
					- (*phx)(c,        x,y);	\
			    }						\
			} while(0);

#define CTIB(B)		if (B && rmg_works.chatr & C_CAL) ERRCHK(1,	   \
			"\tE.g. PrimeC @ %le",(B+2)->y[3*((B+2)->l)/4],0); \
			CT_W(B, 2, 3);CT_C(B, 2);CT_W(B, 5, 6);CT_C(B, 5); \
			CT_W(B, 8, 9);CT_C(B, 8);CT_W(B,11,12);CT_C(B,11); \
			CT_W(B,14,15);CT_C(B,14);CT_W(B,17,18);CT_C(B,17); \
			CT_W(B,20,21);CT_C(B,20);CT_W(B,23,24);CT_C(B,23); \
			if (B && rmg_works.chatr & C_CAL) ERRCHK(1,	   \
			" -> %le\n", (B+2)->y[3*((B+2)->l)/4], 0);


#define CTID(D)		CT_W(D,1,2);CT_W(D,1,4);CT_W(D,1,6);CT_C(D,1);

/*
 *  Load the CTI information; want the ph2pX transformation function.
 *  This routine may complain, but failure just means no correction is done.
 */
/* ARGSUSED */
static int
ctiloader(p, r)
	Point	*p, *r;
{
	double		(*phx)() = 0, c, x = p->xcen, y = p->ycen;

	get_ph2pi(rmg_works.ctp, p->epch, 9.0e+6, p->detr, p->chip, &phx);
	ERRCHK(!phx, "No CTI info available; use XSPEC gain command!\n",
		0, return(0));

	/* correct gaussian positions */
	CTIB(br_0);	CTIB(br_1);	CTIB(br_2);	CTIB(br_3);
	CTIB(br_4);	CTIB(br_5);	CTIB(br_6);

	/* correct diplodicus positions */
	CTID(dp_0);	CTID(dp_1);	CTID(dp_2);	CTID(dp_3);
	CTID(dp_4);	CTID(dp_5);	CTID(dp_6);

	/*
	 *  These weren't in the 0.8 response model, as there wasn't
	 *  much gain change in that era, but if anybody tries this
	 *  on recent data, not doing this will be a bug.
	 */
	/* correct low energy tail model */
	ERRCHK(rmg_works.chatr & C_CAL, "LowE tail gain %g -> ", fgain, 0);
	fgain *= 1000.0 / (*phx)(1000.0, x, y);
	ERRCHK(rmg_works.chatr & C_CAL, "%g eV/ADU\n", fgain, 0);

	r->epch = p->epch;	/* UNNECESSARY? */

	return(0);
}

#define LDIP(B,M,I)	if (M & rmg_works.gmask) {			\
				*(c=strchr(rmg_works.f2p, '?')) = '0'+I;\
				ERRCHK(!(B = get_brnch(rmg_works.f2p)),	\
				"Bad dip. on Grade %d\n",I,return(1));	\
				*c = '?';				\
			} else						\
				B = 0
/* ARGSUSED */
static int
f2_loader(p, r)
	Point	*p, *r;
{
	static char	f2[MAX_FNAME*2];
	register char	*c;

	if (!rmg_works.f2[0] || rmg_works.f2[0] == ' ') {
		(void)sprintf(rmg_works.f2,
			"s%1dc%1dp%2de%1d/diplo.s%1d.c%1d.g?.ecd",
			p->detr, p->chip,
			(int)p->spth, (p->echo == 0.0) ? 0 : 1,
			p->detr, p->chip);
	}
	rmg_works.f2p = string_cat(f2, rmg_works.f2, MAX_FNAME-1);

	LDIP(dp_0,(MSK_FTBT0|MSK_FAST0),0);
	LDIP(dp_1,(MSK_FTBT1)          ,1);
	LDIP(dp_2,(MSK_FTBT2|MSK_FAST0),2);
	LDIP(dp_3,(MSK_FTBT3)          ,3);
	LDIP(dp_4,(MSK_FTBT4)          ,4);
	LDIP(dp_5,(MSK_FTBT5)          ,5);
	LDIP(dp_6,(MSK_FTBT6)          ,6);

	return(0);
}

#define LDBR(B,M,I)	if (M & rmg_works.gmask) {			\
				*(c=strchr(rmg_works.f1p, '?')) = '0'+I;\
				ERRCHK(!(B = get_brnch(rmg_works.f1p)),	\
				"Bad gau. on Grade %d\n",I,return(1));	\
				*c = '?';				\
			} else						\
				B = 0
/* ARGSUSED */
static int
f1_loader(p, r)
	Point	*p, *r;
{
	static char	f1[MAX_FNAME*2];
	register char	*c;

	if (!rmg_works.f1[0] || rmg_works.f1[0] == ' ') {
		(void)sprintf(rmg_works.f1,
			"s%1dc%1dp%2de%1d/gau.s%1d.c%1d.g?.ecd",
			p->detr, p->chip,
			(int)p->spth, (p->echo == 0.0) ? 0 : 1,
			p->detr, p->chip);
	}
	rmg_works.f1p = string_cat(f1, rmg_works.f1, MAX_FNAME-1);

	LDBR(br_0,(MSK_FTBT0|MSK_FAST0),0);
	LDBR(br_1,(MSK_FTBT1)          ,1);
	LDBR(br_2,(MSK_FTBT2|MSK_FAST0),2);
	LDBR(br_3,(MSK_FTBT3)          ,3);
	LDBR(br_4,(MSK_FTBT4)          ,4);
	LDBR(br_5,(MSK_FTBT5)          ,5);
	LDBR(br_6,(MSK_FTBT6)          ,6);

	return(0);
}

#define LECD(B,C,M,I)	if (M & rmg_works.gmask) {			\
				xt[4] = 'G';	xt[5] = '0'+I;		\
				ERRCHK(!(B=get_ecd(rmg_works.gdp,xt)),	\
				"Missing Grade %d gau.\n",I,return(1));	\
				xt[4] = 'D';	xt[5] = '0'+I;		\
				ERRCHK(!(C=get_ecd(rmg_works.gdp,xt)),	\
				"Missing Grade %d dip.\n",I,return(1));	\
			} else						\
				B = C = 0
/* ARGSUSED */
static int
ecdloader(p, r)
	Point	*p, *r;
{
	static char	xt[9] = "ECD_tgEe";

	xt[7] = (p->echo == 0.0) ? '0' : '1';

	LECD(br_0,dp_0,(MSK_FTBT0|MSK_FAST0),0);
	LECD(br_1,dp_1,(MSK_FTBT1)          ,1);
	LECD(br_2,dp_2,(MSK_FTBT2|MSK_FAST0),2);
	LECD(br_3,dp_3,(MSK_FTBT3)          ,3);
	LECD(br_4,dp_4,(MSK_FTBT4)          ,4);
	LECD(br_5,dp_5,(MSK_FTBT5)          ,5);
	LECD(br_6,dp_6,(MSK_FTBT6)          ,6);

	return(0);
}

/*
 *  Main entry to loading externals.
 */
/* ARGSUSED */
static int
loader(p, r, l)
	Point	*p, *r;
	double	(**l)();
{
	fixit(p, r, l);		/* cache stuff */

	/*	if (ecdloader(p, r) && (f1_loader(p, r) || f2_loader(p, r)))
	 *	     return(1);
	 */

	/* if ecdloader fails and not using caldb, try the ascii files */
	if (ecdloader(p,r) != 0) {
	  ERRCHK(1,"Unable to read Fits ECD from %s\n", rmg_works.gdp, 0);
	  if (strcmp(rmg_works.gd,"CALDB")==0) return(1);

	  ERRCHK(1,"Trying ASCII ECD files...\n",0,0);
	  ERRCHK( (f1_loader(p, r) || f2_loader(p, r)),
		  "Unable to read ASCII ECD\n", 0, return(1));
	}

	if (ctiloader(p, r)) return(1);

	/*
	 *  These weren't in v0.8, but should be harmless to propagate.
	 */
	r->epch = p->epch;	/* UNNECESSARY? */
	r->xcen = p->xcen;	/* UNNECESSARY? */
	r->ycen = p->ycen;	/* UNNECESSARY? */
	r->xwid = p->xwid;	/* ignored */	/* UNNECESSARY? */
	r->ywid = p->ywid;	/* ignored */	/* UNNECESSARY? */

	return(0);
}

/*
 *  The following static structures configure the response
 *  about the data point in SIS configuration space.  This
 *  is mostly boiler-plate:
 *
 *  	Gcomp -- one for each feature, ebound mapping
 *	Gcptr -- null terminated lists of features
 *	Range -- features for each grade for an energy range
 *	Point -- locates the response domain
 */

static Gcomp	fg0 = { gr_0, 0, 0, 0, 0, 0, 0 },
		fg1 = { gr_1, 0, 0, 0, 0, 0, 0 },
		fg2 = { gr_2, 0, 0, 0, 0, 0, 0 },
		fg3 = { gr_3, 0, 0, 0, 0, 0, 0 },
		fg4 = { gr_4, 0, 0, 0, 0, 0, 0 },
		fg5 = { gr_5, 0, 0, 0, 0, 0, 0 },
		fg6 = { gr_6, 0, 0, 0, 0, 0, 0 },
		feb = { ebnds, 0, 0, 0, 0, 0, 0 },
		fet = { ltail, 0, 0, 0, 0, 0, 0 },
		fez = { evthr, thresh, 0, 0, 0, 0, 0 },
		fdg = { diags, 0, 0, 0, 0, 0, 0 },
		fp1 = { gausa, aprime, 0, 0, 0, 0, 0 },
		fp2 = { gausa, bprime, 0, 0, 0, 0, 0 },
		fe1 = { gausa, aescap, 0, 0, 0, 0, 0 },
		fe2 = { gausa, bescap, 0, 0, 0, 0, 0 },
		ff1 = { gausa, afluor, 0, 0, 0, 0, 0 },
		ff2 = { gausa, bfluor, 0, 0, 0, 0, 0 },
		fx1 = { gausa, aextra, 0, 0, 0, 0, 0 },
		fx2 = { gausa, bextra, 0, 0, 0, 0, 0 },
		fdp = { diplo, diparm, 0, 0, 0, 0, 0 };
static Gcptr	grlb[] = { &feb, &fet, &fez, &fdg, 0 },
		grl0[] = { &fg0, &fp1, &fp2, &fdp, 0 },
		grh0[] = { &fg0, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl1[] = { &fg1, &fp1, &fp2, &fdp, 0 },
		grh1[] = { &fg1, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl2[] = { &fg2, &fp1, &fp2, &fdp, 0 },
		grh2[] = { &fg2, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl3[] = { &fg3, &fp1, &fp2, &fdp, 0 },
		grh3[] = { &fg3, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl4[] = { &fg4, &fp1, &fp2, &fdp, 0 },
		grh4[] = { &fg4, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl5[] = { &fg5, &fp1, &fp2, &fdp, 0 },
		grh5[] = { &fg5, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grl6[] = { &fg6, &fp1, &fp2, &fdp, 0 },
		grh6[] = { &fg6, &fp1, &fp2,
			   &fe1, &fe2, &ff1, &ff2, &fx1, &fx2, &fdp, 0 },
		grlf[] = { &fg0, &fp1, &fp2, &fdp,
			   &fg2, &fp1, &fp2, &fdp, 0 },
		grhf[] = { &fg0, &fp1, &fp2, &fe1, &fe2, &ff1, &ff2,
				 &fx1, &fx2, &fdp,
			   &fg2, &fp1, &fp2, &fe1, &fe2, &ff1, &ff2,
				 &fx1, &fx2, &fdp, 0 };
static Range	res0 = { EMIN, EEDGE,
			 { grlb,
			   grl0, grl1, grl2, grl3, grl4, grl5, grl6, 0,
			   grlb,
			   grlf, 0 } },
		res1 = { EEDGE, EMAX,
			 { grlb,
			   grh0, grh1, grh2, grh3, grh4, grh5, grh6, 0,
			   grlb,
			   grhf, 0 } };

/*
 *  Miscellaneous fixes, p is target response point, l is lookup
 *  table, and r points to one of the data0031 response points.
 */
static void
fixit(p, r, l)
	Point	*p, *r;
	double	(**l)();
{
	/* These are things that don't get fixed elsewhere */
	/* Nota Bene: metric() currently may do this anyway */
	r->vers = 0.8;	/* UNNECESSARY? */
	r->evth = p->evth;	/* UNNECESSARY? */
	r->spth = p->spth;	/* UNNECESSARY? */
	r->echo = p->echo;	/* UNNECESSARY? */

	lowe_tail = l[LK_LETAIL2];	 /* attach tail hook */

	/* patch in tail gains */
	xfixit(r);
}

/*
 *  Local definitions of internal parameters, hidden
 *  parameter routines and convenient macro expansions.
 */

/*
 *  Complex Slab + Channel Stops Model
 *    Cf. arasmus Xspec SIS qe model:
 grep 'temp =' *f | sed 's/energy\*\*\([-0-9.-E()]*\)/pow((E), \1)/' | sed 's/energy\*\*\([-0-9.E()]*\)/pow((E),\1)/'
 */

#define Si_lo(P,E)	(-(P)/\
	(0.2352 * pow((E), 1.22176) * (1. + 10.3584 * pow((E),1.6264))))
#define Si_hi(P,E)	(-(P)/\
	(0.1038 * pow((E), 3.0688)  * (1. + 1.7359  * pow((E),(-1.2153)))))

#define SiO2_lo(P,E)	(-(P)/\
	(0.698319 * pow((E), 2.24294) * (1. + 4.541946 * pow((E),2.5228E-5))))
#define SiO2_me(P,E)	(-(P)/\
	(0.91088 * pow((E), 3.02935) * (1. + 0.4724 * pow((E),(-1.215895)))))
#define SiO2_hi(P,E)	(-(P)/\
	(0.16533 * pow((E), 3.1152)  * (1. + 1.7406  * pow((E),(-1.1601)))))

#define Si3N4_lo(P,E)	(-(P)/\
	(3.5621 * pow((E), 2.30224) * (1. - 0.1332 * pow((E),(-4.9E-5)))))
#define Si3N4_me(P,E)	(-(P)/\
	(0.3380 * pow((E), 3.5212) * (1. + 2.3309 * pow((E),(-1.1949)))))
#define Si3N4_hi(P,E)	(-(P)/\
	(0.1226 * pow((E), 3.12336)  * (1. + 1.79513  * pow((E),(-1.1022)))))

#define Al_lo(P,E)	(-(P)/\
	(3.06778 * pow((E), 2.7571) * (1. + 0.01449 * pow((E),(-2.3435)))))
#define Al_hi(P,E)	(-(P)/\
	(0.1203 * pow((E), 3.0615)  * (1. + 1.4325  * pow((E),(-1.23)))))

#define Lexan_lo(P,E)	(-(P)/\
	(7.4408 * pow((E), 1.6112) * (1. + 11.498 * pow((E),(1.3583)))))
#define Lexan_me(P,E)	(-(P)/\
	(0.6698 * pow((E), 1.9402) * (1. + 5.595 * pow((E),(0.717666)))))
#define Lexan_hi(P,E)	(-(P)/\
	(1.0094 * pow((E), 3.4177)  * (1. + 2.474  * pow((E),(-1.0692)))))

#define SIS_QE(Si,Ox,Ni,Al,Lx,E)					\
	exp( Si(P2,E) + Ox(P3,E) + Ni(P4,E) + Al(P5,E) + Lx(P6,E) )	\
	* ( 1.0 - exp( Si(P7,E) ) )					\
	* ( 1.0 - P69 * ( 1.0 - exp( Ox(P8,E) + Si(P10,E) ) ) )

#define Func(F,S,O,N,A,L)	static Pfunc F (e,p)			\
					double e; Point *p;		\
				{					\
					if (p)	return(0.0);		\
					else	return(			\
						  SIS_QE(S,O,N,A,L,e)	\
						);			\
				}

static char	name0031[] = "SIS0C0 Response Matrix v0.8";
static char	desc0031[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 0.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.5706	eV/ADU\n\
					.9782	e-/ADU\n\
		Readout noise:		5.43	ADU\n\
					5.3116	e-\n\
		Depletion depth:	32.9508	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0031 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 0, 1, &data0031, name0031, desc0031,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0031 = 3.5706 , noisf0031 = 5.3116 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.3246 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.6417 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0837 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0708 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1816 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 32.9508 )	/* eff. depletion layer thick.	*/
#define P69	( 1.5882 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.3777 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.6088 )	/* eff. chan. stop th. of SiO2	*/

Func(s00031,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10031,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20031,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30031,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40031,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50031,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0031(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00031(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10031(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20031(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30031(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40031(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50031(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0032;
Plist	p_0031 = { &p_0032, &data0031 };

static char	name0032[] = "SIS0C1 Response Matrix v0.8";
static char	desc0032[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 1.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.5827	eV/ADU\n\
					.9815	e-/ADU\n\
		Readout noise:		5.30	ADU\n\
					5.2019	e-\n\
		Depletion depth:	31.4705	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0032 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 1, 1, &data0032, name0032, desc0032,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0032 = 3.5827 , noisf0032 = 5.2019 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.2885 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.6462 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0724 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.1004 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1788 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 31.4705 )	/* eff. depletion layer thick.	*/
#define P69	( 1.5479 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.3885 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.7083 )	/* eff. chan. stop th. of SiO2	*/

Func(s00032,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10032,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20032,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30032,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40032,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50032,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0032(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00032(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10032(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20032(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30032(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40032(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50032(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0033;
Plist	p_0032 = { &p_0033, &data0032 };

static char	name0033[] = "SIS0C2 Response Matrix v0.8";
static char	desc0033[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 2.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.3086	eV/ADU\n\
					.9064	e-/ADU\n\
		Readout noise:		6.00	ADU\n\
					5.4384	e-\n\
		Depletion depth:	27.8898	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0033 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 2, 1, &data0033, name0033, desc0033,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0033 = 3.3086 , noisf0033 = 5.4384 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.3614 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.5809 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0737 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0699 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1817 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 27.8898 )	/* eff. depletion layer thick.	*/
#define P69	( 1.4954 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.5532 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.8112 )	/* eff. chan. stop th. of SiO2	*/

Func(s00033,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10033,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20033,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30033,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40033,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50033,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0033(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00033(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10033(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20033(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30033(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40033(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50033(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0034;
Plist	p_0033 = { &p_0034, &data0033 };

static char	name0034[] = "SIS0C3 Response Matrix v0.8";
static char	desc0034[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 3.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.4728	eV/ADU\n\
					.9514	e-/ADU\n\
		Readout noise:		6.34	ADU\n\
					6.0318	e-\n\
		Depletion depth:	29.4673	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0034 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 3, 1, &data0034, name0034, desc0034,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0034 = 3.4728 , noisf0034 = 6.0318 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.2910 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.5749 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.1026 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0812 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1797 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 29.4673 )	/* eff. depletion layer thick.	*/
#define P69	( 1.4090 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.5064 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.9839 )	/* eff. chan. stop th. of SiO2	*/

Func(s00034,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10034,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20034,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30034,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40034,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50034,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0034(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00034(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10034(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20034(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30034(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40034(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50034(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0035;
Plist	p_0034 = { &p_0035, &data0034 };

static char	name0035[] = "SIS1C0 Response Matrix v0.8";
static char	desc0035[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 0.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.1958	eV/ADU\n\
					.8755	e-/ADU\n\
		Readout noise:		4.62	ADU\n\
					4.0448	e-\n\
		Depletion depth:	31.4722	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0035 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 0, 1, &data0035, name0035, desc0035,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0035 = 3.1958 , noisf0035 = 4.0448 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.4010 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.5422 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0836 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0463 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.2048 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 31.4722 )	/* eff. depletion layer thick.	*/
#define P69	( 1.4357 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.4789 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.6698 )	/* eff. chan. stop th. of SiO2	*/

Func(s00035,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10035,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20035,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30035,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40035,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50035,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0035(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00035(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10035(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20035(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30035(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40035(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50035(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0036;
Plist	p_0035 = { &p_0036, &data0035 };

static char	name0036[] = "SIS1C1 Response Matrix v0.8";
static char	desc0036[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 1.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.2734	eV/ADU\n\
					.8968	e-/ADU\n\
		Readout noise:		4.60	ADU\n\
					4.1252	e-\n\
		Depletion depth:	30.5289	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0036 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 1, 1, &data0036, name0036, desc0036,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0036 = 3.2734 , noisf0036 = 4.1252 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.4220 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.5998 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0574 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0476 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1936 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 30.5289 )	/* eff. depletion layer thick.	*/
#define P69	( 1.5686 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.4786 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.6558 )	/* eff. chan. stop th. of SiO2	*/

Func(s00036,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10036,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20036,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30036,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40036,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50036,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0036(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00036(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10036(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20036(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30036(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40036(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50036(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0037;
Plist	p_0036 = { &p_0037, &data0036 };

static char	name0037[] = "SIS1C2 Response Matrix v0.8";
static char	desc0037[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 2.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.5217	eV/ADU\n\
					.9648	e-/ADU\n\
		Readout noise:		6.53	ADU\n\
					6.3001	e-\n\
		Depletion depth:	34.0066	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0037 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 2, 1, &data0037, name0037, desc0037,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0037 = 3.5217 , noisf0037 = 6.3001 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.3616 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.6087 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0826 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0607 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1844 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 34.0066 )	/* eff. depletion layer thick.	*/
#define P69	( 1.5912 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.4009 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.5183 )	/* eff. chan. stop th. of SiO2	*/

Func(s00037,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10037,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20037,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30037,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40037,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50037,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0037(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00037(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10037(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20037(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30037(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40037(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50037(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0038;
Plist	p_0037 = { &p_0038, &data0037 };

static char	name0038[] = "SIS1C3 Response Matrix v0.8";
static char	desc0038[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 3.\n\
	The low-energy tail to the response is approximately modeled\n\
		Nominal gain:		3.2502	eV/ADU\n\
					.8904	e-/ADU\n\
		Readout noise:		5.26	ADU\n\
					4.6835	e-\n\
		Depletion depth:	32.2095	microns\n\n\
		Epoch:			9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0038 = { 0.8, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 3, 1, &data0038, name0038, desc0038,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0038 = 3.2502 , noisf0038 = 4.6835 ;

/* HEAD_QE_CACHE_DEFINITION */

#define P2	( 0.3556 )	/* eff. dead layer th. of Si	*/
#define P3	( 0.5812 )	/* eff. dead layer th. of SiO2	*/
#define P4	( 0.0851 )	/* eff. dead layer th. of Si3N4	*/
#define P5	( 0.0820 )	/* eff. dead layer th. of Al	*/
#define P6	( -0.1943 )	/* eff. dead layer th. of Lexan	*/
#define P7	( 32.2095 )	/* eff. depletion layer thick.	*/
#define P69	( 1.4863 / 27. )	/* chan. stop wdth / pixel size	*/
#define P10	( -2.5917 )	/* eff. chan. stop th. of Si	*/
#define P8	( 4.8459 )	/* eff. chan. stop th. of SiO2	*/

Func(s00038,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10038,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20038,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30038,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40038,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50038,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0038(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00038(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10038(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20038(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30038(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40038(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50038(e, (Point *)0);

	last_qe = t_qe * M_IS2PI;
	part_qe = 0.0;
	last_en = e;
}

#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P69
#undef P10
#undef P8

/* TAIL_QE_CACHE_DEFINITION */

/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0039;
Plist	p_0038 = { &p_0039, &data0038 };

/*
 *  Connect up gain for tail models
 */
static void
xfixit(r)
	Point	*r;
{
	       if (r == &data0031) {
		qe_cache = qe0031; fgain = fgain0031; noisf = noisf0031;
	} else if (r == &data0032) {
		qe_cache = qe0032; fgain = fgain0032; noisf = noisf0032;
	} else if (r == &data0033) {
		qe_cache = qe0033; fgain = fgain0033; noisf = noisf0033;
	} else if (r == &data0034) {
		qe_cache = qe0034; fgain = fgain0034; noisf = noisf0034;
	} else if (r == &data0035) {
		qe_cache = qe0035; fgain = fgain0035; noisf = noisf0035;
	} else if (r == &data0036) {
		qe_cache = qe0036; fgain = fgain0036; noisf = noisf0036;
	} else if (r == &data0037) {
		qe_cache = qe0037; fgain = fgain0037; noisf = noisf0037;
	} else if (r == &data0038) {
		qe_cache = qe0038; fgain = fgain0038; noisf = noisf0038; }
}

