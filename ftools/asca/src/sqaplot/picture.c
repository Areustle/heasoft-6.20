/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/picture.c,v 3.7 2007/08/01 16:41:51 irby Exp $   */
/*                   */

/*
 filename:	picture.c
 purpose:	produce the summary plots of science file data
 author:	Geoffrey B. Crew
 date:		May 1993
 */
#include "defs.h"
#include "compat.h"

static Real	zero = 0.0,	*zer = &zero,
		wune = 1.0,	*one = &wune,
		varx = 0.0,	*vrx = &varx,
		vary = 0.0,	*vry = &vary,
		varz = 0.0,	*vrz = &varz;
static int	fals = 0,	*sqafls = &fals;
/*static int	true = 0,	*tru = &true; */
static char	buf[MAX_FNAME], bug[MAX_FNAME];

static void	spread(), semi_log(), pg_param(), poisson(),
		contours(), chip_origin(), time_line(), overalls();
static Real	*transform();

static void	plot_image(), plot_spect(), plot_light(), plot_stuff();

/*
 *  Entry to the plot pages.
 */
void
do_picture()
{
	pg_param(-2);			/* notice color, char size,... */

	PGPAGE(); plot_image(); overalls();
	PGPAGE(); plot_spect();
	PGPAGE(); plot_light();
	PGPAGE(); plot_stuff(); PGIDEN();
}

/*
 *  Plot the quick image.
 */
static void
plot_image()
{
	register int	x, y;
	int		c;
	Chips		*cp;
	static Real	cn[NCONTOURS];
	static int	ione = 1;
	static int	idim = 106, jdim = 105;
	static Real	a[4][105][106], amax, *amx = &amax;

	PGVSTAND();
	if (sqwork.rd) {
		*vrx = -475;	*vry = 475;
	} else {
		*vrx = 165;	*vry = 1115;
	}
	PGWNAD(vrx, vry, vrx, vry);

	/*
	 *  Cycle through chips, taking care over:
	 *	C <--> F array storage order
	 *	x == RAWX - 5; y == RAWY
	 *	(1,1) <= (x,y) <= (420,422) == (NCOLS,NROWS)
         *	(1,1) <= (j,i) <= (105,106) == (NCOLS,NROWS)/4
	 *	idim <-> y, jdim <-> x
	 *	4/1 compression to is to save on PostScript
	 */
	for (c = 0, cp = sqwork.cs; c < 4; c++, cp++)
		for (x = 0; x < 420; x++)
			for (y = 0; y < 422; y++)
				a[c][x/4][y/4] += cp->xy[x+6][y+1];
        contours(a, amx, cn);

	for (c = 0; c < 4; c++) {
		PGGRAY(&a[c][0][0], &idim, &jdim,
			&ione, &idim, &ione, &jdim,
			amx, zer, transform(c));
		if (sqwork.nc > 0) {
			for (x = 0, y = 1; x < sqwork.sm; x++, y *= 2) {
				spread(a[c], y, 0);
				spread(a[c], 0, y);
			}
			pg_param(5);
			PGCONT(&a[c][0][0], &idim, &jdim,
				&ione, &idim, &ione, &jdim,
				cn, &sqwork.nc, transform(c));
			pg_param(-1);
		}
		chip_origin(c);
	}

	PGBOX("bcnst", zer, sqafls, "bcnst", zer, sqafls);

	(void)sprintf(buf, "SIS%d %dCCD-%s Mode Image", sqwork.hd.sisn,
		sqwork.hd.nccd, sqwork.hd.datamode + 1);
	if (sqwork.rd)
		PGLAB("RAWX (pixels)", "RAWY (pixels)", buf);
	else
		PGLAB("DETX (pixels)", "DETY (pixels)", buf);
}

/*
 *  Overall plot title, etc.
 */
static void
overalls()
{
	pg_param(-8);
	*vrx = 1.5;	*vry =  1.20;	*vrz = 0.5;
	(void)sprintf(buf, "Sqaplot v. %.2f", sqwork.version);
	PGMTXT("t", vrx, vry, vrz, buf);

	pg_param(-16);
	*vrx = 2.5;	*vry =  1.40;	*vrz = 0.5;
	PGMTXT("t", vrx, vry, vrz, sqwork.in);
	*vrx = 2.5;	*vry = -0.35;	*vrz = 0.0;
	(void)sprintf(buf, "%s@%sbit-rate",
		sqwork.hd.object+1, sqwork.hd.bit_rate);
	PGMTXT("t", vrx, vry, vrz, buf);

	/* capture flag set in read_primary or read_header */
	if (sqwork.hd.ra_nom[0] && sqwork.hd.dec_nom[0]) {
		*vrx = 2.5;	*vry =  3.15;	*vrz = 1.0;
		(void)sprintf(buf, "RA: %.2f DEC: %.2f",
			atof(sqwork.hd.ra_nom), atof(sqwork.hd.dec_nom));
		PGMTXT("t", vrx, vry, vrz, buf);
	}
	pg_param(-3);
}

/*
 *  Find the maximum of the smushed chips, and pick contours.
 */
static void
contours(a, m, cv)
	Real	a[4][105][106], *m, *cv;
{
	register int	x, y;
        int		c, b;
	Real		mx, bx;

        (void)sprintf(buf, "\n** Image maximum set at %.2f counts\n",
		sqwork.max); sqecho(buf);

	*m = sqwork.max * sqwork.rg;
	if (sqwork.nc == 0) return;

	/*
	 *  Ok, the human wants to see the wiggly lines:
	 *  Let's take a stab at making the picture interesting.
	 */
	for (c = 0, b = 0, mx = sqwork.max; sqwork.sm && c < 4; c++)
		for (x = 0; x < 105; x++)
			for (y = 0; y < 106; y++) {
				if (a[c][x][y] > mx) mx = a[c][x][y];
				b += a[c][x][y];
			}

	bx = (Real)b / (Real)(105*106);
	(void)sprintf(buf, "   Fuzzy maximum at %.2f\n", mx);	sqecho(buf);
	(void)sprintf(buf, "   Average brightness %.2f\n", bx);	sqecho(buf);
	sqecho("   Contours at: ");

	for (x = 0; x < sqwork.nc; x++) {
		cv[x] = bx + (mx-bx) *
			((Real)x + 1.0) / ((Real)sqwork.nc + 1.0);
		(void)sprintf(buf, " %.2f", cv[x]); sqecho(buf);
	}
	sqecho("\n");
}

/*
 *  Provide the PGPLOT transformation from array to world coordinates.
 *  We handle the SIS0 v SIS1 chip orientation by the big switch.
 */
static Real *
transform(c)
	int	c;
{
	static Real	t[6];
	Real		tmpa, tmpb, tmpc;

	switch ((c + 2*sqwork.hd.sisn) % 4) {
	case 0:	t[0] =  -11.5 + 1.5; t[1] =  0.0; t[2] = -4.0;
		t[3] =  424.5 + 1.5; t[4] = -4.0; t[5] =  0.0;
		break;
	case 1:	t[0] =  432.5 + 1.5; t[1] =  0.0; t[2] = -4.0;
		t[3] =  424.5 + 1.5; t[4] = -4.0; t[5] =  0.0;
		break;
	case 2:	t[0] =   11.5 - 1.5; t[1] =  0.0; t[2] =  4.0;
		t[3] = -424.5 - 1.5; t[4] =  4.0; t[5] =  0.0;
		break;
	case 3:	t[0] = -432.5 - 1.5; t[1] =  0.0; t[2] =  4.0;
		t[3] = -424.5 - 1.5; t[4] =  4.0; t[5] =  0.0;
		break;
	}
	if (sqwork.rd) return(t);

	/* handle the case of detector coordinate plotting */
	tmpa =   t[0];	tmpb =   t[1];	tmpc =   t[2];
	t[0] = - t[3];	t[1] = - t[4];	t[2] = - t[5];
	t[3] = - tmpa;	t[4] = - tmpb;	t[5] = - tmpc;

	t[0] += 640.;	t[3] += 640.;

	return(t);
}

/*
 *  Provide little chip origin markers.
 */
static void
chip_origin(c)
	int	c;
{
	Real		*t = transform(c), tck = 8.0;
	int		rdt = (sqwork.rd) ? 0 : 4,
			ick = (sqwork.rd) ? 0 : 3;
	register int	i, j;
	static char	top[] = "t", bot[] = "b", *whr, lab[] = "C4:";

	pg_param(c);
	*vrx = t[0] + tck * t[2+ick];	*vry = t[3] + 0.0 * t[4-ick];
	PGMOVE(vrx, vry);
	*vrx = t[0] + 0.0 * t[2+ick];	*vry = t[3] + 0.0 * t[4-ick];
	PGDRAW(vrx, vry);
	*vrx = t[0] + 0.0 * t[2+ick];	*vry = t[3] + tck * t[4-ick];
	PGDRAW(vrx, vry);
	pg_param(-1);

	switch (((c + 2*sqwork.hd.sisn) % 4) + rdt) {
		case 0: case 4: whr = top; *vry = -0.15; *vrz = 1.0; break;
		case 1: case 7: whr = top; *vry =  1.10; *vrz = 0.0; break;
		case 2: case 6: whr = bot; *vry =  1.10; *vrz = 0.0; break;
		case 3: case 5: whr = bot; *vry = -0.15; *vrz = 1.0; break;
	}
	lab[1] = '0' + c;		*vrx = -1.5;
	PGMTXT(whr, vrx, vry, vrz, lab);

	/* indicate total good counts */
	*vrx -= 1.0;
	(void)sprintf(buf, "%d cts", sqwork.cs[c].events);
	PGMTXT(whr, vrx, vry, vrz, buf);

	/* determine chip exposure */
	*vrx -= 1.0;
	for (j = 0, i = 0; j < TIME_BINS; j++)
		if (sqwork.cs[c].gt[j]) i++;
	(void)sprintf(buf, "%g s", (double)i * sqwork.hd.tdelta);
	PGMTXT(whr, vrx, vry, vrz, buf);
}

/*
 *  Smear out the distribution for better contouring.
 */
static void
spread(a, i, j)
	Real	a[105][106];
	int	i, j;
{
	register int	x, y;

	if (i && !j) {
		for (x = 0, j = 105 - i; x < j; x++)
			for (y = 0; y < 106; y++)
				a[x][y] += a[x+i][y];
	} else if (j && !i) {
		for (x = 0, i = 106 - j; x < 105; x++)
			for (y = 0; y < i; y++)
				a[x][y] += a[x][y+j];
        }
}

/*
 *  Plot the spectra.
 */
static void
plot_spect()
{
	Real		hist[4][ADU_BINS], adus[ADU_BINS],
			am, mx, mn, zr;
	register int	i, j;
	int		ihm;
	Chips		*cp;

	PGVSTAND();
	/*
	 *  Transformation to plotting world.
	 */
	am = (Real)(4096 / ADU_BINS);
	for (i = 1, adus[0] = 0.0; i < ADU_BINS; i++)
		adus[i] = adus[i-1] + am;
	if (!sqwork.rd) for (i = 0; i < ADU_BINS; i++)
		adus[i] *= sqwork.gn;
	mx = mn = zr =  0.0;
	semi_log(adus, &mx, &mn, &zr, ADU_BINS);
	mx = mn = zr = -0.5;
	for (i = 0, cp = sqwork.cs; i < 4; i++, cp++) {
		for (j = 0; j < ADU_BINS; j++)
			hist[i][j] = cp->ph[j];
		semi_log(&hist[i][0], &mx, &mn, &zr, ADU_BINS);
	}

	mx += 0.3;
	mn += 0.3;
	ihm = ADU_BINS-12;
	PGWINDOW(adus+12, adus+ADU_BINS-1, &mn, &mx);
	PGBOX("bclnst", zer, sqafls, "bclnst", zer, sqafls);
	if (sqwork.rd) {
		(void)sprintf(buf, "Counts/%d ADU Bin",
			(4096 / ADU_BINS));
		PGLAB("Binned PH (ADU)", buf, "PHA Spectrum");
	} else {
		(void)sprintf(buf, "Counts/%.1lf eV Bin",
			(4096000. / ADU_BINS) * sqwork.gn);
		PGLAB("PI Energy (keV)", buf, "PI Spectrum");
	}
	/*
	 *  Do the plots.
	 */
	for (i = 0; i < 4; i++) {
		pg_param(i);
		PGBIN(&ihm, adus, hist[i], sqafls);
	}
	pg_param(-1);
}

/*
 *  Plot the light curve.
 */
static void
plot_light()
{
	Real		hist[TIME_BINS], time[TIME_BINS], tm, hm;
	Real		gtis[TIME_BINS];
	register int	i, j;
	int		ihm;
	Chips		*cp;

	PGVSTAND();
	/*
	 *  Transformation to plotting world.
	 */
	tm = sqwork.hd.tdelta;
	for (i = 1, time[0] = 0.0; i < sqwork.hd.tbins; i++)
		time[i] = time[i-1] + tm;
	/* tm = time[--i]; */
	tm = sqwork.hd.tstp - sqwork.hd.tstrt;
	for (i = 0, cp = sqwork.cs, ihm = 0; i < 4; i++, cp++)
		for (j = 0; j < sqwork.hd.tbins; j++)
			if (cp->lc[j] > ihm) ihm = cp->lc[j];
	hm = (Real)ihm * 1.10;
	PGWINDOW(zer, &tm, zer, &hm);
	PGBOX("bcnst", zer, sqafls, "bcnst", zer, sqafls);
	(void)sprintf(buf, "Seconds from %.3f (ASCA Time)", sqwork.hd.tstrt);
	(void)sprintf(bug, "Counts/Chip/%.0f-sec Bin", sqwork.hd.tdelta);
	PGLAB(buf, bug, "Light Curve");
	*vrx = 2.0;
	(void)sprintf(buf, "%s %s", sqwork.hd.date_obs, sqwork.hd.time_obs);
	PGMTXT("t", vrx, zer, zer, buf);
	(void)sprintf(buf, "%s %s", sqwork.hd.date_end, sqwork.hd.time_end);
	PGMTXT("t", vrx, one, one, buf);
	/*
	 *  Do the plots.
	 */
	for (i = 0, cp = sqwork.cs, ihm = sqwork.hd.tbins; i < 4; i++, cp++) {
		pg_param(i);
		for (j = 0; j < sqwork.hd.tbins; j++)
			hist[j] = (Real)cp->lc[j];
		for (j = 0; j < sqwork.hd.tbins; j++)
			gtis[j] = hm * ((cp->gt[j]) ? 0.98 - i*.01 : 2.0 );
		PGBIN(&ihm, time, hist, sqafls);
		PGBIN(&ihm, time, gtis, sqafls);
	}
	pg_param(-1);

	time_line(sqwork.hd.tdelta * sqwork.hd.tbins);
}

/*
 *  Stick on a Day, Hour or Minute timeline based on interval in seconds.
 */
static void
time_line(d)
	double	d;
{
	Real	start, stop;
	char	*label;

	if (d < 300.0) {			/* < 5 min  */
		start  = fmod(sqwork.hd.tstrt, 60.0);
		stop   = start + d;
		label  = "secs: ";
	} else if ((d /= 60.0) < 120.0) {	/* < 2 hour */
		start  = fmod(sqwork.hd.tstrt, 3600.0)/60.0;
		stop   = start + d;
		label  = "mins: ";
	} else if ((d /= 60.0) < 48.0) {	/* < 2 days */
		start  = fmod(sqwork.hd.tstrt, 86400.0)/3600.0;
		stop   = start + d;
		label  = "hrs: ";
	} else {				/* >= 1 day */
		start  = fmod(sqwork.hd.tstrt, 86400.0)/86400.0;
		stop   = start + d/24.0;
		label  = "days: ";
	}
	PGWINDOW(&start, &stop, zer, one);
	PGBOX("cimt", zer, sqafls, "", zer, sqafls);
	*vrx = 0.8;	*vry = -0.02;
	PGMTXT("t", vrx, vry, one, label);
}

/*
 *  Plot the flicker diagnostics.
 */
static void
plot_stuff()
{
	Real		hist[4][MAX_FRAMES], fram[MAX_FRAMES],
			pois[4][MAX_FRAMES], pfrm[MAX_FRAMES],
			fm, mx, mn, zr;
	register int	i, j;
	int		ihm;
	Chips		*cp;

	PGVSTAND();
	/*
	 *  Transformation to plotting world.
	 */
	for (i = 1, fram[0] = 1.0, pfrm[0] = 1.5; i < MAX_FRAMES; i++) {
		fram[i] = fram[i-1] + 1.0;
		pfrm[i] = pfrm[i-1] + 1.0;
	}
	mx = mn = zr =  0.0;
	semi_log(fram, &mx, &mn, &zr, MAX_FRAMES);
	semi_log(pfrm, &mx, &mn, &zr, MAX_FRAMES);
	mx = mn = zr = -0.5;
	for (i = 0, cp = sqwork.cs, ihm = 2; i < 4; i++, cp++) {
		if (cp->fq[1] == 0) continue;
		for (j = ihm; cp->fq[j] > 0 && j < MAX_FRAMES; j++);
		ihm = j;
		for (j = 1; j < MAX_FRAMES; j++)
			hist[i][j] = cp->fq[j];
		poisson(cp->fq, pois[i]);
		semi_log(&hist[i][1], &mx, &mn, &zr, MAX_FRAMES);
	}
	fm = (Real)log10((double)ihm);

	mx += 0.3;
	PGWINDOW(zer, &fm, zer, &mx);
	PGBOX("bclnst", zer, sqafls, "bclnst", zer, sqafls);
	PGLAB("Number of Frames on", "Number of Pixels",
		"Flickering Pixel Distribution");
	/*
	 *  Do the plots.
	 */
	pg_param(-11);
	*vrx = -1.8;	*vry = 0.85;
	PGMTXT("t", vrx, vry, one,
		"thresh (events+others:tlmsat|flicks) Chip #: ");
	for (i = 0, cp = sqwork.cs; i < 4; i++, cp++) {
		pg_param(-1);			/* reset line style */
		/* line labels */
		*vrx = (i + 1.90) * (-2.0);
		*vry = 0.85;
		(void)sprintf(buf, "%d (%d+%d:%d|%d) Chip %d: ", cp->knk,
			cp->events, cp->grades, cp->satevs, cp->flixes, i);
		PGMTXT("t", vrx, vry, one, buf);
		PGQPOS(vrx, vry);
		*vrx *= 1.02;
		PGMOVE(vrx, vry);
		*vrx *= 1.10;
		pg_param(i);			/* set line style */
		PGDRAW(vrx, vry);

		/* do the histogram */
		if (cp->fq[1] == 0) continue;	/* nothing to plot */
		PGBIN(&ihm, fram, &hist[i][1], sqafls);

		/* threshold cutoffs */
		PGLINE(&cp->knk, pfrm, &pois[i][1]);
		if (!cp->fq[cp->knk]) continue;
		*vrx = (Real)log10((double)cp->knk);
		*vry = (Real)log10((double)cp->fq[cp->knk]);
		PGMOVE(vrx, vry);
		PGDRAW(vrx, zer);
	}
	pg_param(-3);
}

/*
 *  Generate a plotting signature for the poisson cleaning.
 *  Logic is similar to threshold_o() of science.c.
 */
static void
poisson(fq, h)
	int	*fq;
	Real	*h;
{
	int	n;
	double	mean;
	
	*fq  = NROWS * NCOLS;
	h[0] = fq[0] - fq[1];		/* H(0) */
	h[1] = fq[1] - fq[2];		/* H(1) */
	mean = (Real)h[1] / (Real)h[0];
	n    = 1;

	while (h[n] > 1.0 && n++ < MAX_FRAMES) {
		h[n] = (double)h[n-1] * mean / (Real)n;	/* H(n) */
	}
	while (n-- > 0)
		h[n] += h[n+1];
	while (h[++n] >= 1.0)
		h[n] = log10((double)h[n]);
	while (n < MAX_FRAMES)
		h[n++] = -2.0;	/* i.e., small */
}

/*
 *  Convert an array to logarithmic values for plotting.
 *  In the process, provide the maximum and minumum and
 *  substitute the supplied zero value for nulls.
 */
static void
semi_log(a, mx, mn, zr, n)
	Real	*a, *mx, *mn, *zr;
	int	n;
{
	register int	i;

	/*
	 *  Find first useful value...
	 */
	for (i = 0; i < n; i++)
		if (a[i] <= 0.0) {
			a[i] = *zr;
		} else {
			a[i] = log10((double)a[i]);
			if (*mx == *zr || a[i] > *mx) *mx = a[i];
			if (*mn == *zr || a[i] < *mn) *mn = a[i];
			break;
		}
	/*
	 *  ...then continue with the rest.
	 */
	for (++i ; i < n; i++) {
		a[i] = (a[i] > 0.0) ? log10((double)a[i]) : *zr;
		if (a[i] > *mx) *mx = a[i];
		if (a[i] < *mn) *mn = a[i];
	}
}

/*
 *  Line style games; i >=  0 does the set;
 *                    i == -1 resets to default
 *		      i == -2 does a setup.
 */
static void
pg_param(i)
	int	i;
{
	static int	ls, color = 0;
	static Real	dfont = 1.3, sfont;

	if (i >= 0) {
		if (color) {				/* color */
			ls = i+2;
			PGSCI(&ls);
		} else {				/* b & w */
			if (i > 3) i = 0;		/* extra */
			ls = (i%2)*3+1;
			PGSLS(&ls);
			ls = (i/2)+1;
			PGSLW(&ls);
		}
	} else if (i == -2) {				/* setup */
		if ((sqwork.dv[0] == '/' &&
		     sqwork.dv[1] == 'x' &&
		     sqwork.dv[2] == 'w') || 
		    (sqwork.dv[0] == '/' &&
		     sqwork.dv[1] == 'X' &&
		     sqwork.dv[2] == 'W')) color = 1;
		PGSCH(&dfont);
	} else if (i == -1) {				/* lines */
		ls = 1;
		PGSLS(&ls);
		PGSLW(&ls);
		PGSCI(&ls);
	} else if (i == -3) {				/* csize */
		PGSCH(&dfont);
	} else {
		sfont = (Real)i / -10.0;		/* kluge */
		PGSCH(&sfont);
	}
}
