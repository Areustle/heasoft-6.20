/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/rmloop.c,v 3.8 1997/01/15 02:30:42 guerber Exp $   */
/*                   */

/*
 filename:	rmloop.c
 purpose:	inner loop which builds RM from features
 author:	Geoffrey B. Crew
 date:		November 1992
 updated:	February 1996
 modified:      Jeff Guerber, GSFC/HSTX, Apr. 1996. Convert to cfitsio.
                Jeff Guerber, GSFC/HSTX, Nov. 1996. TLMIN, TLMAX for F_CHAN
 */
#include "defs.h"

static int	heap_size;
static Real	rm_max = 0.0;

static void	rmf_loop(), compress(), chatter(),
		dump_rheader(), dump_rphrow(),
		dump_cleanup(), dump_rimage();
static Gcptr	*setup();

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/rmloop.c,v 3.8 1997/01/15 02:30:42 guerber Exp $";

/*
 *  Use the functional representation of the response
 *  matrix embedded in the provided point to generate
 *  the individual elements of the response matrix.
 *
 *  For debugging and presentation purposes, an extra
 *  pass may be taken to output the RM as the IMAGE.
 */
void
rmf_array(cp, status)
	Point	*cp;
	int	*status;
{
	/*
	 *  Work on the RM for image output.
	 */
	if (rmg_works.chatr & C_PRI) {
		ERRCHK(1, "RM Primary: %d bytes\n",
			sizeof(short) * rmg_works.e_bin * rmg_works.pcbin,
			rmf_loop(cp, status, dump_rimage));
		ERRCHK(1, "RM Maximum:  %.3e\n", rm_max, 0);
		ERRCHK(1, "RM LO_THRES: %.3e\n", LO_THRES, 0);
		ERRCHK(1, "RM RM_THRES: %.3e\n", RM_THRES, 0);
		ERRCHK(1, "ARF EA_MAX:  %.3e\n", rmg_works.eamax, 0);
	}
	/*
	 *  Work on the RM for extension output.
	 */
	dump_rheader(cp, status);
	heap_size = 0;			/* count heap bytes */
	rmf_loop(cp, status, dump_rphrow);
	dump_cleanup(status);
	ERRCHK(rmg_works.chatr & C_LOP, "RM Heapsize: %d\n", heap_size, 0);
}

/*
 *  Generate & output the RM data.
 *
 *  This version utilizes at most a single response range
 *  in order to compute matrix elements.  Future versions
 *  should use some form of a weighted sum when elo-ehi
 *  cross from one response range to the next.
 */
static void
rmf_loop(cp, status, output)
	Point	*cp;
	int	*status;
	void	(*output)();
{
	int	i;
	Real	*elo = rmg_works.evals,
		*ehi = rmg_works.evals + 1, emn;
	double	ene;
	Range	**rp = cp->rp,
		*lp = *rp - 1;		/* impossible last *rp */
	Gcptr	*gl, *gp;

	for (i = 0; i < rmg_works.e_bin; i++, elo++, ehi++) {
		emn = (*elo + *ehi) * 0.5;
		ene = (double)emn;
		memset((char *)rmg_works.pharr, 0, sizeof(Real) * MAX_PBINS);
		/*
		 *  If required, shift to appropriate
		 *  energy Range and perform the setup.
		 */
		while (*rp && emn > (*rp)->eupp) rp++;
		if (*rp != lp && (lp = *rp))
			gl = setup(lp->gc, ene, cp);
		/*
		 *  If in range, loop over response features.
		 */
		if (lp && emn > lp->elow)
			for (gp = gl; *gp; gp++)
				(*gp)->f(ene, *gp);
		/*
		 *  Output the response for this row.
		 */
		(*output)(i, elo, ehi, status);
	}
}

/*
 *  Loop over the range table of Gcomps, accumulating
 *  features appropriate to the grade mask and doing
 *  the initialization of their parameter functions.
 */
static Gcptr *
setup(gc, e, cp)
	Gcptr	**gc;
	double	e;
	Point	*cp;
{
	static Gcptr	list[MAX_FEATS];
	Gcptr		*gp, *gn, *ebnd = 0;
	int		i, m;
	Pfunp		*p;

	ERRCHK(rmg_works.chatr & C_LOP, "RM Setup @ E = %lg\n", e, 0);
	/*
	 *  Build component list based on grade mask.
	 *  Ensure Ebnds routines are processed last.
	 */
	for (i = 0, m = 1, gp = list; i < NUM_TYPES; gc++, i++, m <<= 1) {
		if (m & rmg_works.gmask && *gc) {
			if (m & MSK_EBNDS) ebnd = *gc;
			else for (gn = *gc; *gn; )
				*gp++ = *gn++;
		}
	}
	ERRCHK(!ebnd,"No Ebounds info in %#x!\n",rmg_works.gmask,return(0));
	while (*ebnd) *gp++ = *ebnd++;		/* add ebnds gcomps */
	*gp = (Gcptr)0;		/* NULL terminate list */
	/*
	 *  Setup each component with calibration info.
	 */
	for (gp = list; *gp; gp++)
		for (p = (*gp)->p; *p; p++)
			(void)(**p)(e, cp);

	return(list);
}

/*
 *  Dump out one row of RM into the FITS primary array.
 *  Presumably one should do the write with ftppre(),
 *  but in the current implementation, this is just a
 *  repackaging of ftpcle().  Cut out the middle man!
 *
 *  ARGSUSED
 */
static void
dump_rimage(ebin, elo, ehi, status)
	int	ebin, *status;
	Real	*elo, *ehi;
{
	static long	felm = 1;
	Real		*r, *rx;

	compress(rmg_works.pharr, rmg_works.pcarr);
	/*
	 *  Look for a maximum in each row for FITS image scaling.
	 */
	rx = rmg_works.pcarr + rmg_works.pcbin;
	for (r = rmg_works.pcarr; r < rx; r++)
		if (*r > rm_max) rm_max = *r;

	if (rmg_works.chatr & C_LOP) {
		ERRCHK(ebin == 0,
			"RM First  image row @ fel = %d\n", felm, 0);
		ERRCHK(ebin == (rmg_works.e_bin - 1)/2,
			"RM Middle image row @ fel = %d\n", felm, 0);
		ERRCHK(ebin == rmg_works.e_bin - 1,
			"RM Last   image row @ fel = %d\n", felm, 0);
	}

	ffppre( f_ounit, 0l, felm, (long)rmg_works.pcbin, rmg_works.pcarr,
		status);

	ERRCHK(*status, "FITS error %d", *status, 0);
	ERRCHK(*status, " at row %d\n", ebin, *status=0);
	felm += rmg_works.pcbin;
}

/*
 *  Dump out the MATRIX FITS extension header to the RMF file.
 REVISIT -- might not need all the strcpys.
 */
static void
dump_rheader(cp, status)
	Point	*cp;
	int	*status;
{
	static char	*ttype[] = { "ENERG_LO", "ENERG_HI", "N_GRP   ",
				     "F_CHAN  ", "N_CHAN  ", "MATRIX  " },
			*tform[] = { "E       ", "E       ", "I       ",
				     "PI      ", "PI      ", "PE      " },
			*tunit[] = { "keV     ", "keV     ", "",
				     "", "", "" };
	static char	key[FITS_CLEN_KEYNAME],
			vals[FITS_CLEN_KEYVAL],
			com[FITS_CLEN_COMMENT];
	static Real	rval;

	/* move to, and initiate extension header */
	ffcrhd( f_ounit, status );
	ffphbn( f_ounit, (long)rmg_works.e_bin, 6, ttype, tform, tunit,
		"MATRIX", 0l, status );

	/* any additional keywords with FCPKYx */

	/* If F_CHAN moves from column 4, change these to match */
	ffpkyj( f_ounit, "TLMIN4", (long)rmg_works.first,
	       "First legal channel number", status );
	ffpkyj( f_ounit, "TLMAX4", (long)(rmg_works.first+rmg_works.pcbin-1),
	       "Highest legal channel number", status );

	ffpkys( f_ounit, "TELESCOP", "ASCA", "mission/satellite name", status);

	strncpy(vals, "SIS     ", 8);
	vals[3] = '0' + cp->detr;
	ffpkys( f_ounit, "INSTRUME", vals, "instrument/detector", status );

	ffpkys( f_ounit, "FILTER", "NONE", "filter information", status );

	ffpkys( f_ounit, "RMFVERSN", "1992a",
		"OGIP classification of FITS format style", status );

	ffpkys( f_ounit, "CHANTYPE", ( (rmg_works.sispi) ? "PI" : "PHA" ),
		"Type of channels (PHA, PI etc)", status );

	ffpkyj( f_ounit, "DETCHANS", (long)rmg_works.pcbin,
		"Total number of detector PHA channels", status );

	rval = LO_THRES;
	if (rmg_works.chatr & C_ARF) rval *= rmg_works.eamax;
	ffpkye( f_ounit, "LO_THRES", rval, 2,
		"Lower probability density threshold for matrix", status );

	cal_stuff(1, 0, 0, (Point *)0);
	/* ftbdef is deprecated in fitsio, dropped from cfitsio */
	/*	FCBDEF(F_OUNIT, 6, tform, 0, rmg_works.e_bin, status);*/
}

/*
 *  Dump out one row of MATRIX FITS extension data to the RMF file.
 */
static void
dump_rphrow(ebin, elo, ehi, status)
	int	ebin, *status;
	Real	*elo, *ehi;
{
	static short	fchan[MAX_NGRPS], nchan[MAX_NGRPS];
	Real		*r, *rx, *rm, rsum = 0.0, rmax = 0.0;
	static Real	rlst = 0.0;
	register int	nsig, osig = 0;
	short		*fc = fchan, *nc = nchan, ngrp = 0, *ng = &ngrp, nx = 0;
	int		row = ebin + 1, numg = 0, numm = 0;

	/*
	 *  Compress the row.
	 */
	compress(rmg_works.pharr, rmg_works.pcarr);
	/*
	 *  Determine matrix data for this row.
	 *  PH channel names may affect fchan values.
	 */
	rx = rmg_works.pcarr + rmg_works.pcbin;
	for (rm = r = rmg_works.pcarr; r < rx; r++) {
		nsig = (*r > LO_THRES) ? 1 : 0;
		if (nsig) {			/* significant */
			if (!osig) {		/* grab addres */
				*fc++ = r - rmg_works.pcarr
					  + rmg_works.first;
				*nc = 0;
				ngrp++;
			}
			*rm++ = *r;		/* mv value  */
			rsum += *r;		/* total rsp */
			if (*r > rmax) {	/* is max */
				rmax = *r;
				nx = *nc;
			}
			*nc += 1;
		} else {			/* ignorable */
			if (osig) {
				nc++;
			}
		}
		osig = nsig;
	}
	numg = ngrp;				/* ng alias    */
	numm = rm - rmg_works.pcarr;		/* sum nchan   */
	heap_size += 4 * (numg + numm);		/* 2ng+2ng+4nr */
	/*
	 *  Factor in the ARF EA at the current energy if required.
	 */
	if (rmg_works.chatr & C_ARF)
		for (r = rmg_works.pcarr; r < rm; r++)
			*r *= rmg_works.earea[ebin];
	/*
	 *  Output the data to the FITS file.
	 *  Obviously this needs to be made more efficient.
	 */
	ffpcle(f_ounit, 1, (long)row, 1l, 1l, elo, status);
	ffpcle(f_ounit, 2, (long)row, 1l, 1l, ehi, status);
	ffpcli(f_ounit, 3, (long)row, 1l, 1l, ng,  status);
	ffpcli(f_ounit, 4, (long)row, 1l, (long)numg, fchan, status);
	ffpcli(f_ounit, 5, (long)row, 1l, (long)numg, nchan, status);
	ffpcle(f_ounit, 6, (long)row, 1l, (long)numm, rmg_works.pcarr, status);
	/*
	 *  Diagnostic output on the total response.
	 */
	rlst -= rsum;
	if ((rmg_works.chatr & C_LOP) && (rlst > 0.05 || rlst < -0.05))
		chatter(row, elo, &rsum, numm, ng, fchan, nchan, nx);
	rlst = rsum;
}

/*
 *  Diagnostic comments on response output when there is a significant
 *  change in the total response, e.g., at edges.
 */
static void
chatter(erow, elo, rsm, nmr, ng, fc, nc, nx)
	int	erow, nmr;
	Real	*elo, *rsm;
	short	*ng, *fc, *nc, nx;
{
	Real	*r, *rx;
	int	n;

	ERRCHK(1, " Row % 4d ",  erow, 0);
	ERRCHK(1, "El = %.2e ",  *elo, 0);
	ERRCHK(1, "QE = %.2e\t", *rsm, 0);
	ERRCHK(rmg_works.chatr & C_ARF,
		  "EA = %.2e\t", rmg_works.earea[erow - 1], 0);
	ERRCHK(1, "% 2d grps ",  *ng, 0);
	ERRCHK(1, "% 4d elms\n", nmr, 0);

	if (*ng > 1) for (n = 0; n < *ng; n++) {
		rx = rmg_works.pcarr + fc[n] + nc[n]/2 + 3;
		for (r = rx - 6; r < rx; r++) {
			ERRCHK(1, "  rm[% 4d]",  r - rmg_works.pcarr,  0);
			ERRCHK(1, " = %.3e", *(r - rmg_works.first), 0);
			ERRCHK(1, "%c", (rx - r - 1)%3 ? '\t' : '\n', 0);
		}
	} else {
		rx = rmg_works.pcarr + fc[1] + nx + 3;
		for (r = rx - 3; r < rx; r++) {
			ERRCHK(1, "  rm[% 4d]",  r - rmg_works.pcarr,  0);
			ERRCHK(1, " = %.3e", *(r - rmg_works.first), 0);
			ERRCHK(1, "%c", (rx - r - 1)%3 ? '\t' : '\n', 0);
		}
	}
}

/*
 *  Cleanup for the FITS variable arrays.  It needs to know how
 *  big is the current data unit, so that the next extension does
 *  not overwrite it.  The correct way to do this is unknown.
 REVISIT
 */
static void
dump_cleanup(status)
	int	*status;
{
	static int	nax1 = 0, nax2 = 0;
	static char	key[FITS_CLEN_KEYNAME],
			com[FITS_CLEN_COMMENT];

	ffmkyj( f_ounit, "PCOUNT", (long)heap_size,
		"Number of bytes acumulated in heap", status );

	ffgkey(f_ounit, "NAXIS1  ", key, com, status); nax1 = atoi(key);
	ffgkey(f_ounit, "NAXIS2  ", key, com, status); nax2 = atoi(key);

	/*
	 *  The total size of this extension is believed to be the
	 *  size of the bintable plus the storage all our pointers
	 *  point at.  There should be a better way....
	 */
	heap_size += nax1 * nax2;
	/*      dropped from cfitsio, but program seems to work anyway*/
	/*	FCDDEF(F_OUNIT, heap_size, status);*/
}

/*
 *  Compress the PH array from ph location to pc location.
 *  There are two compression effects here:  (1) the tri-linear
 *  fast/bright ph scale, and (2) the additional rebinning.
 */
static void
compress(ph, pc)
	Real	*ph, *pc;
{
	register int	i, j = rmg_works.rebin - 1;

	while (ph < rmg_works.pharr + 1024) {
		for (i = 0, *pc = *ph++; i < j; i++)
			*pc += *ph++;
		pc++;
	}
	if (rmg_works.gmask & MSK_BRITE)
		j = 2 * rmg_works.rebin - 1;
	while (ph < rmg_works.pharr + 2048) {
		for (i = 0, *pc = *ph++; i < j; i++)
			*pc += *ph++;
		pc++;
	}
	if (rmg_works.gmask & MSK_BRITE)
		j = 4 * rmg_works.rebin - 1;
	while (ph < rmg_works.pharr + 4096) {
		for (i = 0, *pc = *ph++; i < j; i++)
			*pc += *ph++;
		pc++;
	}
}

/*
 *  End of rmloop.c
 */
