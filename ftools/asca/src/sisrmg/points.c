
/*
 filename:	points.c
 purpose:	simple package to manage calibration points
 author:	Geoffrey B. Crew
 date:		April 1994
 updated:	February 1996
 modified:      Jeff Guerber, GSFC/HSTX, Apr 1996. Convert to cfitsio.
 */

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/points.c,v 3.10 1998/01/14 20:10:17 guerber Exp $";

#ifdef SISDATA
  /* just what we need for get_brnch(), get_ecd() for sis2ecd.c */
# include <stdio.h>
# include "cfitsio.h"
# define ERRCHK(C,M,V,A)  do if(C){             \
		(void)fprintf(stderr,(M),(V));  \
		A;                              \
	} while (0)
struct table {
	int     k,              /* previous evaluation  */
		l;              /* last lookup index    */
	double  *x,             /* array of abscissa    */
		*y,             /* array of ordinates   */
		(*f)();         /* evaluation function  */
};
typedef struct table Table;
# define MAX_MESS	1600    /* max message size     */
# define C_PNT		0xffff	/* whatever... */
struct rwork {
	int	chatr;		/* level of chattiness  */
} rmg_works = { C_PNT };
# define LK_LINLIN	0
static double	lins_lini();
static double	(*lookup[])() = { lins_lini };
extern double	strtod();


#else
  /* keep most of the file */
# include "defs.h"

Plist		terminus = { 0, 0 };

static Point	*ww, nada, *none = &nada;

static double	metric(), lins_lini(), bins_lini(),
		lowe_tai1(), lowe_tai2(), diplodicus();
static Point	*pick();

static double	(*lookup[])() = { 0,
				  lins_lini, bins_lini,
				  lowe_tai1, lowe_tai2,
				/*  diplodicus, */
				  0 };

/*
 *  Find a point in the SIS calibration space
 *  which is nearest to target as determined
 *  by the metric based on the provided weight.
 */
void
get_point(target, weight, result)
	Point	*target, *weight, **result;
{
	register Plist	*tryme = &sunimret;
	register Point	*monte;
	double		best = FAR_AWAY, this;

	if (*result = pick(target)) {		/* human picks it */
		if (*result == none) *result = 0;
		return;
	}

	ww = weight;			/* attach weights */

	while(monte = tryme->p) {	/* point to test  */
		ERRCHK(monte != monte->me,
			"Misconfigured response Point %p",
			monte, continue);
		this = metric(target, tryme->p);
		if (this < best) {	/* save it */
			best = this;
			*result = tryme->p;
		}
		tryme = tryme->n;
	}
	ERRCHK(rmg_works.chatr & C_PNT,"Selected %s:\n", (*result)->id, 0);
	ERRCHK(rmg_works.chatr & C_PNT,"\n%s\n", (*result)->ego, 0);
	ERRCHK((*result)->load && (*result)->load(target, *result, lookup),
		"Selected response invalid!\n",0,*result=0);
}

/*
 *  A metric function for the calibration space.  The revised
 *  definitions of WSQ and ICP allow the RQT values to propagate
 *  into the RMF primary header if the WGT values are nonnegative.
 */
#define WSQC(T) (double)((ww->T)*(a->T - b->T)*(a->T - b->T))
#define ICPC(T) (double)(ww->T)*(double)(a->T != b->T)
#define WSQ(T)	WSQC(T);if(b->vers			\
			&& rmg_works.chatr&C_RQT	\
			&& (ww->T)>0.0)		b->T=a->T
#define ICP(T)	ICPC(T);if(b->vers			\
			&& rmg_works.chatr&C_RQT	\
			&& (ww->T)>0  )		b->T=a->T
static double
metric(a, b)
	Point	*a, *b;
{
	static int	num = 1;
	double		sum = 0.0;

	sum += WSQ(vers);
	sum += WSQ(epch);
	sum += WSQ(evth);
	sum += WSQ(spth);
	sum += WSQ(xcen);
	sum += WSQ(xwid);
	sum += WSQ(ycen);
	sum += WSQ(ywid);
	sum += WSQ(temp);
	sum += WSQ(evpf);
	sum += WSQ(imhi);
	sum += WSQ(imlo);

	sum += WSQ(echo);
	sum += WSQ(leak);
	sum += WSQ(dark);

	sum += ICP(detr);
	sum += ICP(chip);
	sum += ICP(mode);

	if (rmg_works.chatr & C_PNT) {
		ERRCHK(rmg_works.chatr&C_RQT, " Update: ",0,0);
		ERRCHK(1, "(Model %02d) ",num++,0);
		ERRCHK(1, "Metric = %.1lg", sum, 0);
		ERRCHK(1, " on %s", b->id, 0);
		ERRCHK(1, " (%p)\n", b, 0);
	}
	return(sum);
}
#undef WSQ
#undef ICP

/*
 *  Human is allowed to select a model here,
 *  bypassing the weighted sum algorithm.  The
 *  hook will be an RMG input file name of:
 *
 *	model?		for a list
 *	model#		to select model number #
 */
static Point *
pick(target)
	Point	*target;
{
	static Point	*result = 0;
	register Plist	*tryme = &sunimret;
	register int	n;

	if (strncmp(rmg_works.inp, "MODEL", 5) &&
	    strncmp(rmg_works.inp, "Model", 5) &&
	    strncmp(rmg_works.inp, "model", 5)) return(result);

	if (n = atoi(rmg_works.inp + 5)) {	/* cough up number n */
		while (--n && tryme->p) tryme = tryme->n;
		result = tryme->p;
		if (result) {
			ERRCHK(rmg_works.chatr & C_PNT,
				"Selected %s\n", result->id, 0);
			ERRCHK(rmg_works.chatr & C_PNT,"%s\n", result->ego, 0);
			ERRCHK( result->load &&
				result->load(target, result, lookup),
				"Selected response invalid!\n",0,return(none));

			return(result);
		} else {
			ERRCHK(1,"Model number out of bounds!\n", 0,0);
		}
	}

	ERRCHK(1,"Available models are as follows:\n",0,0);
	ERRCHK(1,"(Select with infile=Model#)\n\n",0,0);
	for (n = 1; tryme->p; n++, tryme = tryme->n) {
		ERRCHK(n,"Model %02d == ",n,0);
		ERRCHK(n,"%s\n",tryme->p->id,0);
		ERRCHK(rmg_works.inp[0] == 'M', "%s\n",tryme->p->ego,0);
	}

	return(none);
}
#endif /* SISDATA */

/*
 *  Lookup table routine; linear search, linear interpolate.
 */
static double
lins_lini(x, t)
	double	x;
	Table	*t;
{
	register int	i = t->k, j;
	double		v = t->x[i];

	while (v > x && i > 0) v = t->x[--i];

	v = t->x[j = i + 1];
	while (v < x && j < t->l) v = t->x[++j];

	t->k = i = j - 1;

	v = (x - t->x[i]) / (t->x[j] - t->x[i]);
	return(t->y[i] + v * (t->y[j] - t->y[i]));
}

#ifndef SISDATA
/*
 *  Lookup table routine; binary search, linear interpolate.
 */
static double
bins_lini(x, t)
	double	x;
	Table	*t;
{
	register int	i, j;
	double		v = t->x[t->k];

	if (v > x) { i = 0;	j = t->k;	if (i==j) j++;	} else
	if (v < x) { i = t->k;	j = t->l;	if (i==j) i--;	} else
		   { return(t->y[t->k]); 			}

	while (j - i - 1) {	/* j > i */
		v = t->x[t->k = (i + j) / 2];
		if (v > x) { j = t->k;		 } else
		if (v < x) { i = t->k;		 } else
			   { return(t->y[t->k]); }
	}			/* j = i + 1 */

	v = (x - t->x[i]) / (t->x[j] - t->x[i]);
	return(t->y[i] + v * (t->y[j] - t->y[i]));
}
#endif /* SISDATA */

/*
 *  Read in a branching ratio (or other data) table of the form:
 *
 *	E a1 c1 w1 a2 c2 w2 ...
 *
 *  returning a pointer to the first (a1).  The
 *  tables are initialized for use.  If the file
 *  is unreadable, unuseable, ..., return 0.
 *
 *  remember, the values might be string-valued (=0).
 *
 *  This should eventually be ported to FITS.
 */
static char	buf[MAX_MESS];		/* for scanning	*/
#define MG	40			/* MAX grades	*/
#define MC	25			/* MAX columns	*/
#define ME	80			/* MAX energies	*/
static Table	storage[MG][MC];	/* malloc later */
static double	numbers[MG][MC][ME];	/* 	"	*/
static int	invoked = 0;		/* bookkeepping	*/
Table *
get_brnch(f)
	char	*f;
{
	FILE		*fp;
	char		*l, *n = 0;
	int		c, e = 0;
	Table		*t;

	fp = fopen(f, "r");
	ERRCHK(!fp, "File %s not found.\n", f, return((Table *)0));

 	/* read through branching file, one line at a time */
	while ((l = fgets(buf, MAX_MESS, fp)) && e < ME) {
		if (*l == '!') continue;	/* skip a comment */
		for (c = 0; c < MC; c++, l = n)
			numbers[invoked][c][e] = (*l) ? strtod(l, &n) : 0.0;
		e++;
	}
	if (rmg_works.chatr & C_PNT) {
		ERRCHK(1,"\t%s",         f,       0);
		ERRCHK(1," @ %d",        invoked, 0);
		ERRCHK(1," (%d rows)\n", e,       0);
	}
	e--;
	fclose(fp);

	for (c = 1, t = storage[invoked] + 1; c < MC; c++, t++) {
		t->k = 0;
		t->l = e;
		t->x = numbers[invoked][0];
		t->y = numbers[invoked][c];
		t->f = lookup[LK_LINLIN];
	}

	return(storage[invoked++]);
}

/*
 *  Find named extension.  Cf find_ext.
 *  Number of rows is returned; <0 on error.
 */
static int
ext_by_name(file, name)
	char	*file, *name;
{
	static char	val[FITS_CLEN_KEYVAL], com[FITS_CLEN_COMMENT];
	int		ext, blk = 0, hdu = 0, stat = 0;
	long            rows = -1;

	ffopen( &f_eunit, file, 0, &stat );
	ERRCHK(stat!=0, "Unable to open %s.\n", file, return( -1 ));

	for (ext = 1; !stat; ext++, stat = 0) {
                ffmahd( f_eunit, ext, &hdu, &stat );
		if (stat) break;		/* no more exts */
                ffgkys( f_eunit, "EXTNAME ", val, com, &stat );
		if (!stat && !strncmp(name, val, strlen(name))) {
                        ffgkyj( f_eunit, "NAXIS2  ", &rows, com, &stat );
			if (stat) break;	/* found, !rows */
			return(rows);		/* found & rows */
		}
	}
	rows = -1;	/* file did not open */
	return( (int)rows );
}

/* Cf. storage,numbers,invoked above */
static Table	ecd_stor[MG][MC];	/* malloc later */
static double	ecd_nums[MG][MC][ME];	/* 	"	*/
static int	ecd_used = 0;		/* bookkeepping	*/

/*
 *  Return branching Table from named file+ext.
 */
Table *
get_ecd(file, ext)
	char	*file, *ext;
{
	static char	com[FITS_CLEN_COMMENT];
	int		rows, stat = 0, a = 0;
	long            cols = 0;
	register int	c;
	register Table	*t;

	rows = ext_by_name(file, ext);
	ERRCHK(rows<=0,"Ext %s empty or missing.\n", ext, goto die);
	if (rows > ME) rows = ME;
	ffgkyj( f_eunit, "TFIELDS ", &cols, com, &stat );
	if (cols > MC) cols = MC;

	for (c = 0; !stat && c < cols; c++)
	    ffgcvd( f_eunit, 1+c, 1l, 1l, (long)rows, 0.0,
		    ecd_nums[ecd_used][c], &a, &stat );

	ERRCHK(stat,"FITSIO error on %s\n", ext, goto die);
	while (c < MC)	/* anal */
		memset((char *)ecd_nums[ecd_used][c++],0,ME*sizeof(double));

	if (rmg_works.chatr & C_PNT) {
		ERRCHK(1,"\t%s",     file,     0);
		ERRCHK(1,"+%s",      ext,      0);
		ERRCHK(1," @ %d",    ecd_used, 0);
		ERRCHK(1," (%d r x", rows,     0);
		ERRCHK(1," %d c)\n", (int)cols,     0);
	}
	rows--;
	for (c = 1, t = ecd_stor[ecd_used] + 1; c < MC; c++, t++) {
		t->k = 0;
		t->l = rows;
		t->x = ecd_nums[ecd_used][0];
		t->y = ecd_nums[ecd_used][c];
		t->f = lookup[LK_LINLIN];
	}

	ffclos( f_eunit, &stat );
	return(ecd_stor[ecd_used++]);

  die:	ffclos( f_eunit, &stat );
	return((Table *)0);
}

#ifndef SISDATA
/*
 *  Support for the Low Energy Tail included from le_xxxx.c files
 */
#define SCALE    le_scal
#define GAIN     le_gain
#define FULLSIZE le_full
#define NROW	 100
#define NCOL	 100

static void	matrix_row(), e_matrix_row();
static float	le_gain;
static int	le_scal, le_full;

/*
 *  Definitions of lowe_xxxx() functions.
extern short lowe_matri1[], lowe_matri2[];
#include "le_0001.c"
 */
#include "le_0002.c"

/*
 *  Primary feature matrix component
 */
static void
matrix_row(array,en,g,mtrx)
     float *array,en,g;short *mtrx;
{
  short coln,rown;
  int offset,i,j;
  float colwt,rowwt,jacobian,temp;
  static float temparray[NCOL];
  /* figure out which row of matrix corresponds to this energy */
  temp = en*1000.0/GAIN*NROW/(1.0*FULLSIZE) - 0.5;
  rown = (temp<0.0)?0:(temp>(NROW-2.))?NROW-2:(short)floor(temp);
  rowwt=rown+1-temp;
  offset=rown*NCOL;
  /* load temparray with weighted average of two rows */
  for(i=0;i<NCOL;i++)
    temparray[i]=rowwt*(mtrx[offset+i])+(1.0-rowwt)*(mtrx[offset+NCOL+i]);
  /* now to scale the energy and redistribute flexibly */
  jacobian=g*NCOL/(1000.0*en*SCALE); /* jacobian */
  for(j=0;j<FULLSIZE;j++) {
    temp=j*g*NCOL/(1000.0*en) - 0.5;
    coln=(temp<0.0)?0:(temp>(NCOL-2.))?NCOL-2:(short)floor(temp);
    colwt=coln+1-temp;
    if (temp>=NCOL)
      array[j]=0.0;	/* PH * GAIN > Energy */
    else
      array[j]=
        jacobian*(colwt*temparray[coln]+(1.0-colwt)*temparray[coln+1]);
  }
}

/* DIPLODICUS.C		--	simple, yet general function used
 *	                       	to fit some of the spectral response
 *				features.
 * author: arasmus
 * date  : 1-Apr-94
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <math.h>
 * NOTE: it is expected that par[1] < par[0] < par[3].
 * diplodicus will complain, crash and burn if this is not so.
 *
 * Checks for this are now made in the Pfunc routine.
 *
 * Code revised and moved to individual calibration code modules
 *
#define sgn(argument)	(((argument)>0.0) ? 1 : -1)

static double
diplodicus(x,par)
	double 	x,	/* argument */ /*
	par[];		/* parameter array 0..6 (7 parameters) */ /*
{
  double n1,n2,t;

  switch (sgn(x-(par[0]+par[1]))+sgn(x-(par[0]+par[3])))
    {
    case -2:
      if (par[2] == 0.0) {
	x=par[1];goto simple_gaussian;
      }
      n1=(par[1]*par[1])/(par[5]*par[5])*(1.0/par[2]-0.5);
      n2=pow((-1.0*par[1]),2.0-par[2])/(par[2]*par[5]*par[5]);
      t=n1-n2*pow((par[0]-x),par[2]);
      break;
    case 0:
    simple_gaussian:
      t = -(x-par[0])*(x-par[0])/(2.0*par[5]*par[5]);
      break;
    case 2:
      if (par[4] == 0.0) {
	x=par[3];goto simple_gaussian;
      }
      n1=(par[3]*par[3])/(par[5]*par[5])*(1.0/par[4]-0.5);
      n2=pow((par[3]),2.0-par[4])/(par[4]*par[5]*par[5]);
      t=n1-n2*pow((x-par[0]),par[4]);
      break;
    default:
      break;
    }
  return((double)par[6]*exp(t));
}
 */
#endif /* SISDATA */

#if defined ( sun ) && defined ( SISDEBUG )
int
matherr(x)
	struct exception *x;
{
	switch (x->type) {
	case DOMAIN:
		fprintf(stderr, "DOMAIN in %s--", x->name);
		break;
	default:
		break;
	}
	return(0);
}

#endif /* SISDEBUG */
/*
 *  End of points.c
 */
