/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/rmebxt.c,v 3.9 1997/04/02 00:24:32 guerber Exp $   */
/*                   */

/*
 filename:	rmebxt.c
 purpose:	generate the EBOUNDS extension to RMF
 author:	Geoffrey B. Crew
 date:		July 1993
 updated:	February 1996
                Apr. 1996. Jeff Guerber GSFC/HSTX. Convert to new cfitsio.
		Nov. 1996. Jeff Guerber GSFC/HSTX. TLMIN, TLMAX for CHANNELS
 */
#include "defs.h"

static void	dump_eheader(), dump_ebounds(), massage(),
		compress(), chatter(), linear(), smooth(),
		smoot7(), smoot9(), avelin(), avline();

static Real	yp = 0.0, yo = 0.0;
static int	yc = 0;

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/rmebxt.c,v 3.9 1997/04/02 00:24:32 guerber Exp $";

/*
 *  Cleanup the EBOUNDS FITS extension, and output it.
 *  By this point the ebounds array has been populated
 *  with a set of energy values at corresponding pulse
 *  height channels.  If these aren't contiguous, use
 *  linear {inter|extra}polation to fill out the array.
 */
void
rmf_ebnds(cp, status)
	Point	*cp;
	int	*status;
{
	Real	*a, *b, *c, *h = rmg_works.ebnds,
		*hx = rmg_works.ebnds + MAX_EBNDS;

	dump_eheader(cp, status);

	/*
	 *  Find the initial reference points a & b.
	 *  (We force the initial slope to be positive)
	 */
	for (a = rmg_works.ebnds; *a == 0.0; a++);
	for (b = a + 1; *b < *a && b < hx; b++);
	avelin(a, b);

	/*
	 *  Do the {inter|extra}polation.
	 */
	for ( ; h < hx; h++) {
		if (h == b) {		/* advance a & b */
			for (c = b + 1; *c == 0.0 && c < hx; c++);
			if (c < hx) {	/* advance a & b */
				a = b;
				b = c;
				avelin(a, b);
		 	}
		} else if (h != a) {
			/* a & b are fixed points of h(a,b) */
			*h = *a * ( (Real)(b - h)/(Real)(b - a) )
			   + *b * ( (Real)(h - a)/(Real)(b - a) );
			/* purge negative energies */
			if (*h < 0.0) *h = 0.0;
		}
	}

	/*
	 *  Clean it up.
	 */
	massage(rmg_works.ebnds, hx-1);

	dump_ebounds(status);
}

/*
 *  Compute the average slope and offset
 */
static void
avelin(a, b)
	Real	*a, *b;
{
	Real	s;

	if (a == b) return;

	s = (*b - *a) / ( b - a);
	yo += (*a - s * (a - rmg_works.ebnds));
	yp += s;
	yc++;
}

/*
 *  Dump out the EBOUNDS FITS extension header to the RMF file.
 REVISIT
 */
static void
dump_eheader(cp, status)
	Point	*cp;
	int	*status;
{
	static char	*ttype[] = { "CHANNEL ", "E_MIN   ", "E_MAX   " },
			*tform[] = { "I       ", "E       ", "E       " },
			*tunit[] = { "channel ", "keV     ", "keV     " };
	static char	key[FITS_CLEN_KEYNAME],
			vals[FITS_CLEN_KEYVAL],
			com[FITS_CLEN_COMMENT];

	/* move to, and initiate extension header */
	ffcrhd(f_ounit, status);
	ffphbn(f_ounit, (long)rmg_works.pcbin, 3, ttype, tform, tunit,
	       "EBOUNDS", 0l, status);

	/* any additional keywords with FCPKYx */

	/* If CHANNEL moves from column 1, change these to match */
	ffpkyj(f_ounit, "TLMIN1", (long)rmg_works.first,
	       "First legal channel number", status);
	ffpkyj(f_ounit, "TLMAX1", (long)(rmg_works.first+rmg_works.pcbin-1),
	       "Highest legal channel number", status);

	ffpkys(f_ounit, "TELESCOP", "ASCA", "mission/satellite name", status);

	strncpy(vals, "SIS     ", 8);
	vals[3] = '0' + cp->detr;
	ffpkys(f_ounit, "INSTRUME", vals, "instrument/detector", status);

	ffpkys(f_ounit, "FILTER", "NONE", "filter information", status);

	ffpkys(f_ounit, "RMFVERSN", "1992a",
	       "OGIP classification of FITS format style", status);

	ffpkys(f_ounit, "CHANTYPE", ( (rmg_works.sispi) ? "PI" : "PHA" ),
	       "Type of channels (PHA, PI etc)", status);

	ffpkyj(f_ounit, "DETCHANS", (long)rmg_works.pcbin,
	       "Total number of detector PHA channels", status);

	ffpkyj(f_ounit, "SMOOTHED", (long)rmg_works.gains,
	       "0 = raw, 1-12 = smooth, -1 = ep-lin, -2 = mean-lin", status);

	cal_stuff(2, 0, 0, (Point *)0);
}

/*
 *  Round out the rough edges of the EBOUNDS data depending
 *  on the value of the switch rmg_works.gains.
 */
static void
massage(h, hx)
	Real	*h, *hx;
{
	switch (rmg_works.gains) {
		case 12:	smoot9(h, hx);
		case 11:	smoot9(h, hx);
		case 10:	smoot9(h, hx);
		case  9:	smoot9(h, hx);
		case  8:	smoot9(h, hx);
		case  7:	smoot9(h, hx);
		case  6:	smoot9(h, hx);
		case  5:	smoot9(h, hx);
		case  4:	smoot9(h, hx);
		case  3:	smoot7(h, hx);
		case  2:	smoot7(h, hx);
		case  1:	smooth(h, hx);
		case  0:	break;
		case -1:	linear(h, hx);
				break;
		default:	avline(h, hx);
				break;
	}
}

/*
 *  Smooth the Ebounds data.  Since we grabbed energy on an integer
 *  grid, there's probably a bit of noise that ought to be wiped.
 *  Here we do a double Simpson integral smoothing.  Stability?
 */
static void
smooth(h, hx)
	Real	*h, *hx;
{
	h++;
	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];

	for (h++, hx -= 2; h < hx; h++)
		*h = 0.083333333333333 * (h[-2] + h[2]) +
		     0.333333333333333 * (h[-1] + h[1]) +
		     0.166666666666666 * (    h[0]    );

	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];
}

/*
 *  Smooth the Ebounds data.  Since we grabbed energy on an integer
 *  grid, there's probably a bit of noise that ought to be wiped.
 *  This is higher order.
 */
static void
smoot7(h, hx)
	Real	*h, *hx;
{
	h++;
	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];
	h++;
	*h = 0.083333333333333 * (h[-2] + h[2]) +
	     0.333333333333333 * (h[-1] + h[1]) +
	     0.166666666666666 * (    h[0]    );

	for (h++, hx -= 3; h < hx; h++)
		*h = 0.0625 * (h[-3] + h[3]) +
		     0.1875 * (h[-2] + h[2]) +
		     0.1875 * (h[-1] + h[1]) +
		     0.1250 * (    h[0]    );

	*h = 0.083333333333333 * (h[-2] + h[2]) +
	     0.333333333333333 * (h[-1] + h[1]) +
	     0.166666666666666 * (    h[0]    );
	h++;
	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];
}

/*
 *  Smooth the Ebounds data.  Since we grabbed energy on an integer
 *  grid, there's probably a bit of noise that ought to be wiped.
 *  This is higher order still.  Dubious?
 */
static void
smoot9(h, hx)
	Real	*h, *hx;
{
	h++;
	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];
	h++;
	*h = 0.083333333333333 * (h[-2] + h[2]) +
	     0.333333333333333 * (h[-1] + h[1]) +
	     0.166666666666666 * (    h[0]    );
	h++;
	*h = 0.0625 * (h[-3] + h[3]) +
	     0.1875 * (h[-2] + h[2]) +
	     0.1875 * (h[-1] + h[1]) +
	     0.1250 * (    h[0]    );

	for (h++, hx -= 4; h < hx; h++)
		*h = 0.04166666666666 * (h[-4] + h[4]) +
		     0.16666666666666 * (h[-3] + h[3]) +
		     0.08333333333333 * (h[-2] + h[2]) +
		     0.16666666666666 * (h[-1] + h[1]) +
		     0.08333333333333 * (    h[0]    );

	*h = 0.0625 * (h[-3] + h[3]) +
	     0.1875 * (h[-2] + h[2]) +
	     0.1875 * (h[-1] + h[1]) +
	     0.1250 * (    h[0]    );
	h++;
	*h = 0.083333333333333 * (h[-2] + h[2]) +
	     0.333333333333333 * (h[-1] + h[1]) +
	     0.166666666666666 * (    h[0]    );
	h++;
	*h = 0.25 * (h[-1] + h[1]) + 0.5 * h[0];
}

/*
 *  Force a nominal linear gain solution for the EBOUNDS extension.
 */
static void
linear(h, hx)
	Real	*h, *hx;
{
	register Real	delta;

	delta = (*hx - *h) / (Real)(hx - h);
	while (--hx > h) *hx = *h + delta * (hx - h);
}

/*
 *  Build linear solution from the average of the samples
 */
static void
avline(h, hx)
	Real	*h, *hx;
{
	register int	x;

	if (yc == 0) {
		yp = 3.65;
		yo = 0.00;
	} else {
		yp /= yc;
		yo /= yc;
	}
	if (rmg_works.chatr & C_EBD) {
		ERRCHK(1, "E[PH] = %g", yo, 0);
		ERRCHK(1, " + %g * PH keV", yp, 0);
		ERRCHK(1, "\t(%d samples)\n", yc, 0);
	}
	for (x = 0; h + x <= hx; x++) {
		h[x] = yo + yp * x;
		if (h[x] < 0.0) h[x] = 0.0;
	}
}

/*
 *  Dump out the EBOUNDS FITS extension data to the RMF file.
 */
static void
dump_ebounds(status)
	int	*status;
{
	register int	i;
	short		hh[MAX_PBINS];

	for (i = 0; i < MAX_PBINS; i++)
		hh[i] = (short)(i + rmg_works.first);

	compress(rmg_works.ebnds, rmg_works.ecnds);

	ffpcli( f_ounit, 1, 1l, 1l, (long)rmg_works.pcbin, hh, status );
	ffpcle( f_ounit, 2, 1l, 1l, (long)rmg_works.pcbin,
	       rmg_works.ecnds+0, status );
	ffpcle( f_ounit, 3, 1l, 1l, (long)rmg_works.pcbin,
	       rmg_works.ecnds+1, status );

	if (rmg_works.chatr & C_EBD) chatter(hh);
}

/*
 *  Diagnostic output
 */
static void
chatter(hh)
	short	*hh;
{
	register Real	*eb = rmg_works.ecnds;
	register int	pb  = rmg_works.pcbin;

	ERRCHK(1, "\n", 0, 0);
	ERRCHK(1, "EBOUNDS: From PH = %hd\t", hh[0], 0);
	ERRCHK(1,  "E_MIN = %.3e  ", eb[0], 0);
	ERRCHK(1,  "E_MAX = %.3e\n", eb[1], 0);
	ERRCHK(1, "EBOUNDS:  via PH = %hd\t", hh[pb/4 - 1], 0);
	ERRCHK(1,  "E_MIN = %.3e  ", eb[pb/4 - 1], 0);
	ERRCHK(1,  "E_MAX = %.3e\n", eb[pb/4], 0);
	ERRCHK(1, "EBOUNDS:  via PH = %hd\t", hh[pb/2 - 1], 0);
	ERRCHK(1,  "E_MIN = %.3e  ", eb[pb/2 - 1], 0);
	ERRCHK(1,  "E_MAX = %.3e\n", eb[pb/2], 0);
	ERRCHK(1, "EBOUNDS: thru PH = %hd\t", hh[pb - 1], 0);
	ERRCHK(1,  "E_MIN = %.3e  ", eb[pb - 1], 0);
	ERRCHK(1,  "E_MAX = %.3e\n", eb[pb], 0);
}

/*
 *  If we are in Fast/Bright mode, the PH values are compressed
 *  using the simple tri-linear map.  We must convert accordingly.
 *  and copy the source ebnds values to the destination cbnds.
 *
 *  In addition, if the channels are to be rebinned, we do that too.
 */
static void
compress(eb, ec)
	Real	*eb, *ec;
{
	register int	s = rmg_works.rebin;

	while (eb <  rmg_works.ebnds + 1024) {
		*ec++ = *eb;	eb += s;
	}
	if (rmg_works.gmask & MSK_BRITE) s *= 2;
	while (eb <  rmg_works.ebnds + 2048) {
		*ec++ = *eb;	eb += s;
	}
	if (rmg_works.gmask & MSK_BRITE) s *= 2;
	while (eb <= rmg_works.ebnds + 4096) {
		*ec++ = *eb;	eb += s;
	}
}

/*
 *  End of rmebxt.c
 */
