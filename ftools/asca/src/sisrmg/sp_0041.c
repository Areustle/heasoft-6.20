/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/sp_0041.c,v 3.14 2001/12/06 21:18:33 irby Exp $   */
/*                   */
/*
 filename:      sp_0041.c
 purpose:       Calibration Point 0041
 author:        gbc@space.mit.edu
 date:		Tue Apr 29 13:27:27 EDT 1997
 modified:      jeff.guerber@gsfc.nasa.gov, Dec. 1997: loader(): only try ascii
                ecd's if not using caldb.  br_loader: replaced bzero w/memset.
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
static Point	*cal_pt;


/*
 *  For each grade, we swap in a set of Tables which
 *  are interpolated to obtain the gaussian parameters.
 *
 *  qe_cache gets set to qe???? defined at end of file.
 */
#define SetGrade(N,T,D,F,I) 						\
			  static Table	* T , * D ;			\
			  static Rincr N (e, g)				\
				double  e; Gcomp   *g;			\
			  {						\
				if (e != last_en) (*qe_cache)(e, g);	\
				branch = T;				\
				diplod = D;				\
				rdd_func = F;				\
				gr_evt_thresh =				\
					(int)cal_pt->evth + I *		\
					(int)cal_pt->spth;		\
				if (gr_pi) gr_evt_thresh = (int)	\
					gr_pi((double)gr_evt_thresh,	\
						gr_xe, gr_ye);		\
				if (lt_gr_evth < 0) /* first come */	\
					lt_gr_evth = gr_evt_thresh;	\
			  }
static Table	*branch, *diplod;
static Rincr	(*qe_cache)();
static double	(*rdd_func)();
static int	gr_evt_thresh, lt_gr_evth = -1;
static double	(*gr_pi)() = 0, gr_xe, gr_ye;
static double	rdd_frac, rdd_xone, nuc_fano = 0.0;

static double	rdd_1(), rdd_2();
static int	rddunload();

SetGrade(gr_0, br_0, dp_0, rdd_1, 0)
SetGrade(gr_1, br_1, dp_1, rdd_1, 0)
SetGrade(gr_2, br_2, dp_2, rdd_2, 1)
SetGrade(gr_3, br_3, dp_3, rdd_2, 1)
SetGrade(gr_4, br_4, dp_4, rdd_2, 1)
SetGrade(gr_5, br_5, dp_5, rdd_2, 1)
SetGrade(gr_6, br_6, dp_6, rdd_2, 2)	/* well, should be rdd_3 */

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

	/* Only interested in a peak, not an edge */
	if (rmg_works.pharr[m-1] == 0.0 ||
	    rmg_works.pharr[m+1] == 0.0) return;
	/* max == rmg_works.pharr[m] becomes == 0.0 at peak */
	max = (rmg_works.pharr[m-1] - rmg_works.pharr[m+1]) /
		(rmg_works.pharr[m-1] + rmg_works.pharr[m+1] - 2*max);
	if (max < 0.0) max = -max;

	/* if (max < (Real)0.5 / rmg_works.rebin) { */
	if (max < (Real)0.05) {
		rmg_works.ebnds[m] = (Real)e;
		if (rmg_works.chatr & C_EBD) {
			ERRCHK(1, "P[%d] ",m,0);
			ERRCHK(1, "= %.4g",(Real)e,0);
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

	/* round the corner for aesthetics */
	for (k = 7; k < rmg_works.phmax && tailf[k] > 0; k++) ;
	tailf[k - 1] *= (64. - 49.)/64.;
	tailf[k - 2] *= (64. - 36.)/64.;
	tailf[k - 3] *= (64. - 25.)/64.;
	tailf[k - 4] *= (64. - 16.)/64.;
	tailf[k - 5] *= (64. -  9.)/64.;
	tailf[k - 6] *= (64. -  4.)/64.;
	tailf[k - 7] *= (64. -  1.)/64.;

	k = (lt_gr_evth > rmg_works.phmin)
		? lt_gr_evth : rmg_works.phmin;
	for ( ; k < rmg_works.phmax; k++) {
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
	phmin = (gr_evt_thresh > rmg_works.phmin)
		?  gr_evt_thresh  - (int)dipars[0]
		: rmg_works.phmin - (int)dipars[0];
	if (phmin < pha) phmin = pha;

	pha = (int)dipars[13];
	phmax = rmg_works.phmax - (int)dipars[0];
	if (phmax > pha) phmax = pha;

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

	if (phmin <  gr_evt_thresh ) phmin = gr_evt_thresh;

	rmv = rmg_works.pharr + phmin;
	for (pha = phmin; pha <= phmax; pha++) {
		z = (pha - hold_cen) / hold_wid;
		z = nor * exp(- z * z * 0.5);
		*rmv++ += (Real)z;
	}
}

/*
 *  The RDD variant of gausa.  Uses rdd_func which is rdd_1 or rdd_2
 *  to compute the modified gaussian response feature.
 */
/* ARGSUSED */
static Rincr
rddis(e, g)
	double	e;
	Gcomp	*g;
{
	register Real	*rmv;
	register int	pha;
	int		phmin;
	/* int		phmax; */
	double		nor, z, zm;

	if ((nor = PAR(g,0,e)) <= 0.0) return;
	if ((z = log(nor/RM_THRES)) < 0.0) return;
	nor *= hold_wid / M_IS2PI;	/* was optimized for gausa */

	phmin = hold_cen - hold_wid * sqrt(z);
	if (phmin < rmg_works.phmin) phmin = rmg_works.phmin;
	/* phmax = rmg_works.phmax; */

	if (phmin <  gr_evt_thresh ) phmin = gr_evt_thresh;

	rmv = rmg_works.pharr + phmin;
	z = zm = nor * (*rdd_func)(phmin);
	for (pha = phmin; pha <= rmg_works.phmax && z >= zm; pha++) {
		z = nor * (*rdd_func)(pha);
		*rmv++ += (Real)z;
	}
}

/* one-pixel rdd function */
static double
rdd_1(pha)
	int	pha;
{
	double	a, g;
	extern double	rdd_G(), rdd_P();

	a = (1.0 - rdd_frac) * rdd_G((double)pha,hold_cen,hold_wid);
	g = rdd_frac * rdd_P((double)pha,hold_cen,hold_wid,rdd_xone);
	g += a;
	return(g);
}

/* two-pixel rdd function */
static double
rdd_2(pha)
	int	pha;
{
	double	a, b, c, g;
	extern double	rdd_G(), rdd_P();

	a = hold_wid / rdd_xone;
	b = ((double)pha - hold_cen) / hold_wid;
	c = ((1.0 - rdd_frac)*(1.0 - rdd_frac) + rdd_frac*rdd_frac*a*a) *
		rdd_G((double)pha,hold_cen,hold_wid);
	g = (a*rdd_frac*rdd_frac*(b-a)+2.0*rdd_frac*(1.0 - rdd_frac)) *
		rdd_P((double)pha,hold_cen,hold_wid,rdd_xone);
	g += c;
	return(g);
}

/*
 *  The Pfunc functions will be called once to perform
 *  any initializations necessary for optimal response
 *  on future calls; thereafter, they should be called
 *  as functions of energy only (p == NULL), and should
 *  provide the value of the relevant parameter.
 */
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
				if (nuc_fano != 0.0) {			\
					hold_wid *= hold_wid;		\
					hold_wid += e * nuc_fano;	\
					hold_wid = sqrt(hold_wid);	\
				}					\
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
	int		x = rmg_works.chatr & C_LOP;

	if (p)	return(0.0);    /* init */
	if (!diplod) return(dipars[6] = 0.0);

	for (i = 0; i < 7; i++, t++) dipars[i] = t->f(e, t);
	/* Check--didn't find par2=0 or par4=0 in any files--so: */
	if (0.0 < dipars[1] || 0.0 > dipars[3] ||
	    dipars[2] == 0.0 || dipars[4] == 0.0) {
		dipars[6] = 0.0; ++diparmerrs;
		ERRCHK(x, "Dipars interpolation error.\n", 0, 0);
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
		ERRCHK(x, "(Dipars Argument %g error:  ",
			(log(dipars[6]/RM_THRES) + dipars[8]) / dipars[9], 0);
		ERRCHK(x, "Dipars[12] = %g ->", dipars[12], 0);
		ERRCHK(x, " %g)\n", dipars[1], 0);
		dipars[12] = dipars[1]; ++diparmerrs;
	}
	if (dipars[13] < dipars[3]) {
		ERRCHK(x, "(Dipars Argument %g error:  ",
			(log(dipars[6]/RM_THRES) + dipars[10]) / dipars[11],0);
		ERRCHK(x, "Dipars[13] = %g ->", dipars[13], 0);
		ERRCHK(x, " %g)\n", dipars[3], 0);
		dipars[13] = dipars[3]; ++diparmerrs;
	}

	return(dipars[6]);
}

/*
 *  Loader routines to load calibration files...
 *  p is the target (supplied by input)
 *  r is our local calibration response point.
 *
 *  some values of r get reset to target values so that FITS header
 *  keyword values correctly describe the matrix produced.
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
			"\tE.g. GC @ %.1lf",(B+2)->y[3*((B+2)->l)/4],0); \
			CT_W(B, 2, 3);CT_C(B, 2);CT_W(B, 5, 6);CT_C(B, 5); \
			CT_W(B, 8, 9);CT_C(B, 8);CT_W(B,11,12);CT_C(B,11); \
			CT_W(B,14,15);CT_C(B,14);CT_W(B,17,18);CT_C(B,17); \
			CT_W(B,20,21);CT_C(B,20);CT_W(B,23,24);CT_C(B,23); \
			if (B && rmg_works.chatr & C_CAL) ERRCHK(1,	   \
			" -> %.1lf\n", (B+2)->y[3*((B+2)->l)/4], 0);

#define CTID(D)		CT_W(D,1,2);CT_W(D,1,4);CT_W(D,1,6);CT_C(D,1);

#define NUCR(B)		if (B && rmg_works.chatr & C_CAL) do {	\
			    double sg;	/* report nucti */	\
			    ERRCHK(1,"\tE.g. PrimeW(%.3lf)",	\
				(B+3)->x[3*((B+3)->l)/4],0);	\
			    ERRCHK(1," of %.1lf ADU",		\
				sg=(B+3)->y[3*((B+3)->l)/4],0);	\
			    ERRCHK(1," -> %.1lf ADU\n",		\
				sqrt((B+3)->y[3*((B+3)->l)/4]*	\
				nuc_fano + sg*sg),0);		\
			} while (0);

/*
 *  Load the CTI information; want the ph2pX transformation function.
 *  This routine may complain, but failure just means no correction is done.
 */
/* ARGSUSED */
static int
ctiloader(p, r)
	Point	*p, *r;
{
	double	(*phx)() = 0, c, x = p->xcen, y = p->ycen;

	get_ph2pi(rmg_works.ctp, p->epch, 9.0e+6, p->detr, p->chip, &phx);
	ERRCHK(!phx, "No CTI info in %s; use XSPEC gain command!\n",
		rmg_works.ctp, return(0));

	/* correct gaussian positions */
	CTIB(br_0);	CTIB(br_1);	CTIB(br_2);	CTIB(br_3);
	CTIB(br_4);	CTIB(br_5);	CTIB(br_6);

	/* correct diplodicus positions */
	CTID(dp_0);	CTID(dp_1);	CTID(dp_2);	CTID(dp_3);
	CTID(dp_4);	CTID(dp_5);	CTID(dp_6);

	/* correct low energy tail model */
	ERRCHK(rmg_works.chatr & C_CAL, "LowE tail gain %g -> ", fgain, 0);
	fgain *= 1000.0 / (*phx)(1000.0, x, y);
	/* fgain is eV/ADU, approximate to epoch */
	ERRCHK(rmg_works.chatr & C_CAL, "%g eV/ADU\n", fgain, 0);

	/* cache stuff for event threshold in PI mode */
	if (rmg_works.sispi) {
		gr_pi = phx; gr_xe = x; gr_ye = y;
		ERRCHK(rmg_works.chatr & C_CAL, "Event Threshold %d",
			cal_pt->evth, 0);
		ERRCHK(rmg_works.chatr & C_CAL, " -> %d for PI Matrix\n",
			(int)gr_pi((double)cal_pt->evth, gr_xe, gr_ye), 0);
	}

	/* get non-uniform CTI while we're here */
	if (r->vers >= 0.95) {
		Real	fan0, fan1;

		get_nucti(rmg_works.ctp, 9.0e+6,  p->detr, &fan0);
		get_nucti(rmg_works.ctp, p->epch, p->detr, &fan1);
		ERRCHK(fan0 == 0.0 && fan1 == 0.0,
			"Nonuniform CTI data not in %s\n",
				rmg_works.ctp, return(0));
		if (fan1 > fan0)	/* e(keV)*SnSG -> ADU^2 */
			nuc_fano = 1000.0 * (fan1-fan0) / ( fgain*fgain );
		NUCR(br_0); NUCR(br_1); NUCR(br_2); NUCR(br_3);
		NUCR(br_4); NUCR(br_5); NUCR(br_6);
	}

	return(0);
}

/*
 *  Load echo information.  If this is echo-corrupted data, we will
 *  need to adjust feature positions to compensate for additional
 *  echo shift since time 9.0e+6 secs, which is implicit in .ecd files.
 *
 *  Assuming the bias errors are small, the gaussian centers that were
 *  positioned at 1+echo need to be divided by that, then multiplied by
 *  the new echo factor.  (For grades 5 & 6 we really should consider
 *  the relative occurance of echo affected events:  1/2 for now.)
 */
#define EKC(T,I,R)	if (T) do {/* code to echo shift center */	\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--)		t->y[i] *= R;		\
			} while(0);
#define EKCR(T,R)	do {						   \
			  EKC(T, 2,R);EKC(T, 5,R);EKC(T, 8,R);EKC(T,11,R); \
			  EKC(T,14,R);EKC(T,17,R);EKC(T,20,R);EKC(T,23,R); \
			} while(0);
static int
ekoloader(p, r)
	Point	*p, *r;
{
	Real	echo_ecd = 0.0, echo_tgt = 0.0;
	double	ratio;

	if (p->echo == 0.0) return(0);

	/* either of these should work, but latter guarantees continuity */
	/* echo_ecd = (p->detr) ? 0.0087 : 0.014; */
	get_echos(rmg_works.ehp, 9.0e+6,  p->detr, &echo_ecd);

	get_echos(rmg_works.ehp, p->epch, p->detr, &echo_tgt);
	if (echo_tgt == 0 || echo_ecd == 0) {
		ERRCHK(1, "Unable to read echo history %s\n",
			rmg_works.ehp, 0);
		ERRCHK(p->echo < .005 || p->echo > .05,
			"Echo %lg out of bounds, no correction.\n",
			p->echo, return(0));
		echo_tgt = p->echo;
		echo_ecd = (p->detr) ? 0.0087 : 0.014;
	}

	ratio = (1.0 + echo_tgt) / (1.0 + echo_ecd);
	EKCR(br_4,ratio);
	EKCR(br_6,ratio);	/* kludge */
	EKC(dp_4,1,ratio);
	EKC(dp_6,1,ratio);	/* kludge */
	ratio = (1.0 + 0.5*echo_tgt) / (1.0 + 0.5*echo_ecd);
	EKCR(br_5,ratio);	/* kludge */
	EKC(dp_5,1,ratio);	/* kludge */

	r->echo = p->echo = (double)echo_tgt;
	return(0);
}

/*
 *  Make rdd transformations of gaussian parameters:
 *
 (was)	p->dark != 0 : faintdfe not applied:  p->dark = ?  q0 == qmean.
 (was)	p->dark == 0 : faintdfe was applied:  p->dark = 0, q0 == qpeak.
 *
 (now)  zerodef == 0 : old faintdfe applied:  q0 == qpeak
 (now)  zerodef == 1 : new faintdfe applied:  q0 == - frac * xone
 (now)  zerodef == 2 : bright processing:     q0 == qmean
 (now)	zerodef == -1: who knows...:          q0 == p->dark
 *
 *  This is also a convenient place to apply a light leak:
 *
 *	p->leak == average light leak present, in ADU
 (was)		results in 1) downshift of line energy due to onboard CPU
 (now)			   2) possion process: variance/pixel += leak.
 */
#define RDQ(T,I,N)	if (T) do {/* code to do qzero correction */	\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--)		t->y[i] += N*qzero;	\
			} while(0);
#define RDS(T,I,N)	if (T) do {/* code to do sigma corretion */	\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--) {				\
				ss = t->y[i];				\
				t->y[i] = sqrt(ss*ss+(double)N*dsig2);	\
			    }						\
			} while(0);
#define RDDC(T,N)	do {						   \
			  RDQ(T, 2,N);RDQ(T, 5,N);RDQ(T, 8,N);RDQ(T,11,N); \
			  RDS(T, 3,N);RDS(T, 6,N);RDS(T, 9,N);RDS(T,12,N); \
			  RDQ(T,14,N);RDQ(T,17,N);RDQ(T,20,N);RDQ(T,23,N); \
			  RDS(T,15,N);RDS(T,18,N);RDS(T,21,N);RDS(T,24,N); \
			} while(0);
/* ARGSUSED */
static int
rddloader(p, r)
	Point	*p, *r;
{
	Real	frac, xone, dsig2 = 0.0, qpeak, qmean, qzero, ss;

	/* Bypass this if FAST mode */
	ERRCHK(p->mode == 0,
		"No RDD model applied to FAST mode data.\n",0,return(0));

	/* Bypass this setup if RDD is already corrected */
	if (rmg_works.rddcv > 0) {
		ERRCHK(1, "RDD Corrected Version %d\n", rmg_works.rddcv,
			return(rddunload()));
	}

	get_rddis(rmg_works.rdp, p->epch, 9.0e+6, p->detr, p->mode,
		&frac, &xone, &dsig2, &qpeak, &qmean);
	if (dsig2 < 0.0) {
		ERRCHK(1, "Unable to read RDD history %s: using defaults\n",
			rmg_works.rdp, 0);
		get_rddis((char *)0, p->epch, 9.0e+6, p->detr, p->mode,
			&frac, &xone, &dsig2, &qpeak, &qmean);
		ERRCHK(dsig2 < 0.0, "Defaults didn't work--no rdd!\n",
			0, return(1));
	}

	/* cache values for future use */
	rdd_frac = (double)frac;
	rdd_xone = (double)xone;

	/* true light leak adjustments ?huh? */
	if (p->leak != 0.0) dsig2 += p->leak;

	/* corrections for to gaussian centers and widths */
	switch (rmg_works.dfezd) {
	case 0:  /* old faintdfe */	qzero = qpeak;		break;
	case 1:  /* new faintdfe */	qzero = -frac*xone;	break;
	case 2:  /* bright mimic */	qzero = qmean;		break;
	default: /* do not know  */	qzero = p->dark;	break;
	}
	RDDC(br_0,1.0);
	RDDC(br_1,1.0);
	RDDC(br_2,2.0);
	RDDC(br_3,2.0);
	RDDC(br_4,2.0);
	RDDC(br_5,2.0);
	RDDC(br_6,3.0);

	r->mode = p->mode;	/* UNNECESSARY? */
	r->leak = p->leak;	/* UNNECESSARY? */
	r->dark = qzero;

	if (rmg_works.chatr & C_CAL) {
		ERRCHK(1, "  RDD:  frac  = %.3le\t", frac, 0);
		ERRCHK(1,         "xone  = %.3le\t", xone, 0);
		ERRCHK(1,         "dsig2 = %.3le\n", dsig2, 0);
		ERRCHK(1, "  RDD:  qzero = %.3le\t", qzero, 0);
		ERRCHK(1,         "dark  = %.3le\t", r->dark,  0);
		ERRCHK(1,         "leak  = %.3le\n", r->leak,  0);
	}
	return(0);
}

/*
 *  Adjust total branching ratios, a multistep process:
 *
 *	Step 1: compute total branching into each grade for
 *		gaussian features only (a warranted approximation).
 *	Step 2: look for a suitable expected global branching ratio file
 *	Step 3: for nonzero values of the latter, compute relative ratio
 *	Step 4: fold that back onto normalization of all response features.
 *	Step 5: a diagnostic report.
 *
 *  Current code assumes all gaussian files have a common energy grid.
 */
#define BRD(T,I,G)	if (T) do {					\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--) g_branch[G][i] += t->y[i];	\
			    rant = t;					\
			} while(0);

#define BRA(T,G)	BRD(T, 1,G);BRD(T, 4,G);BRD(T, 7,G);BRD(T,10,G); \
			BRD(T,13,G);BRD(T,16,G);BRD(T,19,G);BRD(T,22,G);

#define BRU(T,I,G)	if (T) do {					\
			    register Table	*t = T+I;		\
			    register int	i = t->l + 1;		\
			    while (i--) t->y[i] *= g_branch[G][i];	\
			} while(0);

#define BRR(T,G)	BRU(T, 1,G);BRU(T, 4,G);BRU(T, 7,G);BRU(T,10,G); \
			BRU(T,13,G);BRU(T,16,G);BRU(T,19,G);BRU(T,22,G);

#define ME	80
static double	g_branch[8][ME];

#ifdef SISDEBUG
/*
 *  Dump out a diagnostic file whether we need it or not.
 */
static void
dmp_brnch(name, rant)
	char	*name;
	Table	*rant;
{
	FILE	*fp;
	int	i, j;

	fp = fopen(name, "w");
	if (fp) for (i = 0; i <= rant->l; i++) {
		fprintf(fp, "%7.3lf", rant->x[i]);
		for (j = 0; j < 8; j++)
			fprintf(fp, "%9.6lf", g_branch[j][i]);
		fputs("\n", fp);
	}
	fclose(fp);
}
#endif SISDEBUG

/* ARGSUSED */
static int
br_loader(p, r)
	Point	*p, *r;
{
	static char	f3[MAX_FNAME*2];
	Table	*rant, *rave, *t;
	double	e, gb;
	int	ii, jj;

	if (!rmg_works.f3[0] || rmg_works.f3[0] == ' ') return(0);

	/* Step 1 */
	BRA(br_6,6); BRA(br_5,5); BRA(br_4,4);
	BRA(br_3,3); BRA(br_2,2); BRA(br_1,1);
	BRA(br_0,0);
#ifdef SISDEBUG
	dmp_brnch("g_branch.ecd", rant);
#endif SISDEBUG

	/* Step 2 */
	rmg_works.f3p = string_cat(f3, rmg_works.f3, MAX_FNAME-1);
	ERRCHK(!(rave = get_brnch(rmg_works.f3p)),
		"(A file %s would only used to adjust normalizations.)\n",
		rmg_works.f3p, return(0));
	/* This could otherwise do more hunting around... */

	/* Step 3 */
	for (ii = 0; ii <= rant->l; ii++)
	    for (e = rant->x[ii], jj = 0; jj < 8; jj++) {
		if (g_branch[jj][ii] == 0.0) continue;
		t = rave + 1 + jj;	/* table for grade jj */
		gb = t->f(e, t);	/* branching val at e */
		g_branch[jj][ii] = (gb > 0.0 && gb <= 1.0)
			? gb / g_branch[jj][ii]
			: 1.0;
	    }
#ifdef SISDEBUG
	dmp_brnch("g_branch.ratio", rant);
#endif SISDEBUG

	/* Step 4 */
	BRR(br_6,6); BRR(br_5,5); BRR(br_4,4);
	BRR(br_3,3); BRR(br_2,2); BRR(br_1,1);
	BRR(br_0,0);
	BRU(dp_6,7,6); BRU(dp_5,7,5); BRU(dp_4,7,4);
	BRU(dp_3,7,3); BRU(dp_2,7,2); BRU(dp_1,7,1);
	BRU(dp_0,7,0);

	/* Step 5 */
	/* bzero((char *)g_branch[0], 8 * ME * sizeof(double)); */
	memset((char *)g_branch[0], 0, 8 * ME * sizeof(double));
	BRA(br_6,6); BRA(br_5,5); BRA(br_4,4);
	BRA(br_3,3); BRA(br_2,2); BRA(br_1,1);
	BRA(br_0,0);
#ifdef SISDEBUG
	dmp_brnch("g_branch.rev", rant);
#endif SISDEBUG

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
	if (ekoloader(p, r)) return(1);
	if (rddloader(p, r)) return(1);
	if (br_loader(p, r)) return(1);

	/* changes so that output FITS commentary is correct */
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
		fdg = { diags, 0, 0, 0, 0, 0, 0 },
		fp1 = { rddis, aprime, 0, 0, 0, 0, 0 },
		fp2 = { gausa, bprime, 0, 0, 0, 0, 0 },
		Fp1 = { gausa, aprime, 0, 0, 0, 0, 0 },
		fe1 = { rddis, aescap, 0, 0, 0, 0, 0 },
		fe2 = { gausa, bescap, 0, 0, 0, 0, 0 },
		Fe1 = { gausa, aescap, 0, 0, 0, 0, 0 },
		ff1 = { rddis, afluor, 0, 0, 0, 0, 0 },
		ff2 = { gausa, bfluor, 0, 0, 0, 0, 0 },
		Ff1 = { gausa, afluor, 0, 0, 0, 0, 0 },
		fx1 = { rddis, aextra, 0, 0, 0, 0, 0 },
		fx2 = { gausa, bextra, 0, 0, 0, 0, 0 },
		Fx1 = { gausa, aextra, 0, 0, 0, 0, 0 },
		fdp = { diplo, diparm, 0, 0, 0, 0, 0 };
static Gcptr	grlb[] = { &feb, &fet, &fdg, 0 },
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
		grlf[] = { &fg0, &Fp1, &fp2, &fdp,
			   &fg2, &Fp1, &fp2, &fdp, 0 },
		grhf[] = { &fg0, &Fp1, &fp2, &Fe1, &fe2, &Ff1, &ff2,
				 &Fx1, &fx2, &fdp,
			   &fg2, &Fp1, &fp2, &Fe1, &fe2, &Ff1, &ff2,
				 &Fx1, &fx2, &fdp, 0 };
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
 *  Revert to Gaussian features if RDD effects have been corrected.
 *  Later versions will have more work to do here.  (E.g. replace
 *  rddis() with some other function.)
 */
static int
rddunload()
{
	fp1.f = gausa;
	fe1.f = gausa;
	ff1.f = gausa;
	fx1.f = gausa;
	return(0);
}

/*
 *  Miscellaneous fixes, p is target response point, l is lookup
 *  table, and r points to one of the data0041 response points.
 */
static void
fixit(p, r, l)
	Point	*p, *r;
	double	(**l)();
{
	/* cache response point for later use */
	cal_pt = r;

	/* These are things that don't get fixed elsewhere */
	/* Nota Bene: metric() currently may do this anyway */
	if (p->vers < 0.95) r->vers = 0.9;
	else			r->vers = 1.1;

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

static char	name0041[] = "SIS0C0 Response Matrix v0.9/1.1";
static char	desc0041[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 0.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.5706	eV/ADU\n\
					.9782	e-/ADU\n\
		Readout noise:		5.43	ADU\n\
					5.3116	e-\n\
		Depletion depth:	32.9508	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0041 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 0, 1, &data0041, name0041, desc0041,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0041 = 3.5706 , noisf0041 = 5.3116 ;

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

Func(s00041,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10041,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20041,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30041,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40041,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50041,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0041(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00041(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10041(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20041(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30041(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40041(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50041(e, (Point *)0);

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
extern Plist p_0042;
Plist	p_0041 = { &p_0042, &data0041 };

static char	name0042[] = "SIS0C1 Response Matrix v0.9/1.1";
static char	desc0042[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 1.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.5827	eV/ADU\n\
					.9815	e-/ADU\n\
		Readout noise:		5.30	ADU\n\
					5.2019	e-\n\
		Depletion depth:	31.4705	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0042 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 1, 1, &data0042, name0042, desc0042,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0042 = 3.5827 , noisf0042 = 5.2019 ;

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

Func(s00042,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10042,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20042,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30042,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40042,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50042,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0042(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00042(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10042(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20042(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30042(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40042(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50042(e, (Point *)0);

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
extern Plist p_0043;
Plist	p_0042 = { &p_0043, &data0042 };

static char	name0043[] = "SIS0C2 Response Matrix v0.9/1.1";
static char	desc0043[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 2.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.3086	eV/ADU\n\
					.9064	e-/ADU\n\
		Readout noise:		6.00	ADU\n\
					5.4384	e-\n\
		Depletion depth:	27.8898	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0043 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 2, 1, &data0043, name0043, desc0043,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0043 = 3.3086 , noisf0043 = 5.4384 ;

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

Func(s00043,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10043,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20043,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30043,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40043,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50043,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0043(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00043(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10043(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20043(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30043(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40043(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50043(e, (Point *)0);

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
extern Plist p_0044;
Plist	p_0043 = { &p_0044, &data0043 };

static char	name0044[] = "SIS0C3 Response Matrix v0.9/1.1";
static char	desc0044[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 0 chip 3.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.4728	eV/ADU\n\
					.9514	e-/ADU\n\
		Readout noise:		6.34	ADU\n\
					6.0318	e-\n\
		Depletion depth:	29.4673	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0044 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 207.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 0, 3, 1, &data0044, name0044, desc0044,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0044 = 3.4728 , noisf0044 = 6.0318 ;

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

Func(s00044,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10044,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20044,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30044,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40044,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50044,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0044(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00044(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10044(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20044(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30044(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40044(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50044(e, (Point *)0);

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
extern Plist p_0045;
Plist	p_0044 = { &p_0045, &data0044 };

static char	name0045[] = "SIS1C0 Response Matrix v0.9/1.1";
static char	desc0045[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 0.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.1958	eV/ADU\n\
					.8755	e-/ADU\n\
		Readout noise:		4.62	ADU\n\
					4.0448	e-\n\
		Depletion depth:	31.4722	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0045 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 0, 1, &data0045, name0045, desc0045,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0045 = 3.1958 , noisf0045 = 4.0448 ;

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

Func(s00045,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10045,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20045,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30045,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40045,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50045,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0045(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00045(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10045(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20045(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30045(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40045(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50045(e, (Point *)0);

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
extern Plist p_0046;
Plist	p_0045 = { &p_0046, &data0045 };

static char	name0046[] = "SIS1C1 Response Matrix v0.9/1.1";
static char	desc0046[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 1.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.2734	eV/ADU\n\
					.8968	e-/ADU\n\
		Readout noise:		4.60	ADU\n\
					4.1252	e-\n\
		Depletion depth:	30.5289	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0046 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 1, 1, &data0046, name0046, desc0046,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0046 = 3.2734 , noisf0046 = 4.1252 ;

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

Func(s00046,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10046,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20046,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30046,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40046,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50046,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0046(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00046(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10046(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20046(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30046(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40046(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50046(e, (Point *)0);

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
extern Plist p_0047;
Plist	p_0046 = { &p_0047, &data0046 };

static char	name0047[] = "SIS1C2 Response Matrix v0.9/1.1";
static char	desc0047[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 2.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.5217	eV/ADU\n\
					.9648	e-/ADU\n\
		Readout noise:		6.53	ADU\n\
					6.3001	e-\n\
		Depletion depth:	34.0066	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0047 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 2, 1, &data0047, name0047, desc0047,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0047 = 3.5217 , noisf0047 = 6.3001 ;

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

Func(s00047,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10047,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20047,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30047,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40047,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50047,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0047(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00047(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10047(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20047(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30047(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40047(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50047(e, (Point *)0);

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
extern Plist p_0048;
Plist	p_0047 = { &p_0048, &data0047 };

static char	name0048[] = "SIS1C3 Response Matrix v0.9/1.1";
static char	desc0048[MAX_MESS] = "\
	Detector response is based on ISAS QE model (May 30, 1994)\n\
	with simulated branching ratios for SIS 1 chip 3.\n\
	The low-ph tail to the response is approximately modeled.\n\
	Response features are approximately corrected for RDD;\n\
	Corrections to the grade branching ratios may be applied.\n\
\n\
	v0.9 includes CTI gain corrections.\n\
	v1.1 includes nonuniform CTI effects.\n\
\n\
		Nominal gain:		3.2502	eV/ADU\n\
					.8904	e-/ADU\n\
		Readout noise:		5.26	ADU\n\
					4.6835	e-\n\
		Depletion depth:	32.2095	microns\n\n\
		ECD Epoch:		9.0e+6	ASCA s\n\
\n\
	WARNING:  these matrices have not been fully tested for\n\
	all possible parameter regimes.  Use with all due caution.\n\
\n\
";
static Point	data0048 = { 0.95, 9.0e+6, 100.0, 40.0,
			 215.5, 420.0, 211.5, 422.0,
			 211.15, 1.0, 6.0, -6.0,
			 0.0, 0.0, 0.0,
			 1, 3, 1, &data0048, name0048, desc0048,
			 { &res0, &res1, 0, 0, 0, 0 },
			 0, loader };
static Real	fgain0048 = 3.2502 , noisf0048 = 4.6835 ;

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

Func(s00048,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_lo) /*          e < 0.2838 */
Func(s10048,Si_lo,SiO2_lo,Si3N4_lo,Al_lo,Lexan_me) /* 0.2838 < e < 0.4000 */
Func(s20048,Si_lo,SiO2_lo,Si3N4_me,Al_lo,Lexan_me) /* 0.4000 < e < 0.5317 */
Func(s30048,Si_lo,SiO2_me,Si3N4_me,Al_lo,Lexan_hi) /* 0.5317 < e < 1.5599 */
Func(s40048,Si_lo,SiO2_me,Si3N4_me,Al_hi,Lexan_hi) /* 1.5599 < e < 1.8400 */
Func(s50048,Si_hi,SiO2_hi,Si3N4_hi,Al_hi,Lexan_hi) /* 1.8400 < e          */

/*
 *  This "feature" calculates a basic quantum efficiency
 *  from which the normalizations of each feature are
 *  derived via branching ratios.  It is intended to be
 *  called once per energy, with the values stored in:
 */
/* ARGSUSED */
static Rincr
qe0048(e, g)
	double  e;
	Gcomp   *g;
{
	register Pfunc	t_qe;

	t_qe = 0.0;
	if (    !rmg_works.sisde     ) t_qe = 1.0;			else
	if (              e <= 0.2838) t_qe = s00048(e, (Point *)0);	else
	if (0.2838 < e && e <= 0.4000) t_qe = s10048(e, (Point *)0);	else
	if (0.4000 < e && e <= 0.5317) t_qe = s20048(e, (Point *)0);	else
	if (0.5317 < e && e <= 1.5599) t_qe = s30048(e, (Point *)0);	else
	if (1.5599 < e && e <= 1.8400) t_qe = s40048(e, (Point *)0);	else
	if (1.8400 < e               ) t_qe = s50048(e, (Point *)0);

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
extern Plist p_0049;
Plist	p_0048 = { &p_0049, &data0048 };

/*
 *  Connect up gain for tail models
 */
static void
xfixit(r)
	Point	*r;
{
	       if (r == &data0041) {
		qe_cache = qe0041; fgain = fgain0041; noisf = noisf0041;
	} else if (r == &data0042) {
		qe_cache = qe0042; fgain = fgain0042; noisf = noisf0042;
	} else if (r == &data0043) {
		qe_cache = qe0043; fgain = fgain0043; noisf = noisf0043;
	} else if (r == &data0044) {
		qe_cache = qe0044; fgain = fgain0044; noisf = noisf0044;
	} else if (r == &data0045) {
		qe_cache = qe0045; fgain = fgain0045; noisf = noisf0045;
	} else if (r == &data0046) {
		qe_cache = qe0046; fgain = fgain0046; noisf = noisf0046;
	} else if (r == &data0047) {
		qe_cache = qe0047; fgain = fgain0047; noisf = noisf0047;
	} else if (r == &data0048) {
		qe_cache = qe0048; fgain = fgain0048; noisf = noisf0048; }
}

