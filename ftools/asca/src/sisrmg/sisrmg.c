/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/sisrmg.c,v 3.17 2000/01/28 18:17:24 ngan Exp $   */
/*		*/
/*
 header:	$Header: /headas/headas/ftools/asca/src/sisrmg/sisrmg.c,v 3.17 2000/01/28 18:17:24 ngan Exp $
 filename:	sisrmg.c
 purpose:	main entry to the SIS response matrix generator
 author:	Geoffrey B. Crew
 date:		February 1993
 updated:	February 1996
                Mar 1996 Jeff Guerber, GSFC 664/HSTX. Use sprintf in
		motherhood(), initialize rmg_works.f0p,f1p,f2p,f3p
		Apr 1996 Jeff Guerber. Convert to new native-C fitsio
                Jul 1996 JRG: merged GBC's May 1996 changes
                Dec 1997 JRG: Added collie() (called from get_problem()) to
                access Calibration Database.
                Jan 2000 NG: Fixed bug of clobber parameter.
 */
#define SISRMG_C
#include "defs.h"

#define RqtChk(M,V)	do {	/* replace bad values only */	\
				if (rqt.M == bpt.M) rqt.M = V;	\
			} while (0)
#define DF_EVTH	100
#define DF_SPTH	40
#define DF_XCEN 215.5
#define DF_XWID 420.0
#define DF_YCEN 211.5
#define DF_YWID 422.0

static Point	rqt = {	/* defaults / bad values */
			VERSION, 0.0, 0, 0,
			0., 0., 0., 0.,
			211.2, 1.0, 6.0, -6.0,
			0.0, 0.0, 0.0,
			2, 4, 3,
			/* null self    */
			/* null name    */
			/* null ranges  */
		}, *rqtp = &rqt,
#define WZ 1e-20
		wgt = {	/* REVISIT */
			9000., WZ, 1, 1,
			WZ, WZ, WZ, WZ,
			0., 0., 0., 0.,	/* ignored */
			WZ, WZ, WZ,
			800, 400, 1,
			/* null self	*/
			/* null name	*/
			/* null ranges	*/
		},
		bpt = { /* copies of (bad) parameter values; matched in */
			/* sisrmg.param, get_def_params(), unpack_pha() */
			/* must be different from rqt to override PHA	*/
			VERSION, 0.0, 0, 0,
			0., 0., 0., 0.,
			0., 0., 0., 0.,
			1.0, 100.0, 100.0,
			2, 4, 3,
			/* null self    */
			/* null name    */
			/* null ranges  */
		};
#undef WZ

static void	get_problem(), get_thought(), get_from_pha(),
		get_def_params(), unpack_pha(),
		get_required(), get_request(), get_arfinfo(),
		ph_space(), gr_check(), dm_check(), do_report(),
		rmf_primary(), rmf_verbiage(), rmf_header(), rmf_finish(),
		motherhood(), set_eamax(), do_chatr(), to_string(),
		barf(), do_terse();
static int	parse_mask(), prep_arf(), fake_arf(),
		rmf_open(), side_step();
static char*    collie();

#undef FFCPARS
static void	FFCPARS();

static int	params_gmask, params_dmode;

Rwork		rmg_works;	/* cached for the matrix creation */

fitsfile    *f_iunit = 0, *f_ounit = 0, *f_eunit = 0;

static char *rcsid = "$Header: /headas/headas/ftools/asca/src/sisrmg/sisrmg.c,v 3.17 2000/01/28 18:17:24 ngan Exp $";

/*
 *  Generate the response matrix, passing info between components.
 */
#ifdef SISDEBUG
int
SISRMG()
#else
int
sisrmg_main()
#endif /* SISDEBUG */
{
	int	status = 0;
	Point	*caldata;
	char	**c;

	/*
	 *  Masthead.
	 */
	ERRCHK(1,"Sisrmg Version %g",VERSION,0);
	ERRCHK(1,", Configured %s\n\n",DATE,0);
	for (c = rel_note; **c != '\0'; c++) ERRCHK(1,"  %s\n",*c,0);
	/*
	 *  Find out what sort of response is required
	 *  and look for suitable algorithms to apply.
	 */
	get_problem(&status);
	ERRCHK(status,"Ill-posed problem %d\n",status,return(1));
	get_point(rqtp, &wgt, &caldata);
	ERRCHK(!caldata,"No calibration data available\n",0,return(2));
	cal_stuff(0, 'P', 0, rqtp);
	/*
	 *  Provide some positive feedback to the human.
	 */
	if (rmg_works.chatr & C_HLP) do_report("Ready for RMF:\n");
	else                         do_terse();
	ERRCHK(1,"Please stand by...\n",0,0);
	/*
	 *  Hand off to routines to build the response
	 *  and write out the various FITS extensions.
	 */
	rmf_primary(caldata, &status);	/* create Fits Primary	*/
	ERRCHK(status,"Problem %d with RMF primary.\n",status,return(5));
	rmf_array(caldata, &status);	/* do the RMF extention	*/
	ERRCHK(status,"Problem %d with MATRIX ext.\n",status,return(6));
	rmf_ebnds(caldata, &status);	/* & EBOUNDS extension	*/
	ERRCHK(status,"Problem %d with EBOUNDS ext.\n",status,return(7));
	rmf_finish(&status);		/* and wrap it all up	*/
	ERRCHK(status,"Problem %d with RMF cleanup.\n",status,return(8));
	/*
	 *  Success.
	 */
	ERRCHK(1,"...Done.\n",0,0);
	return(0);
}

/*
 *  Interface to all Q&A with user on problem definition.
 *  Here we ensure that the Rwork structure is fully set.
 *
 *  In the post v0.9 i/o paradigm, marching orders are acquired
 *  from the infile:
 *
 *	1) if it doesn't exist, take everything from parameter I/F
 *	2) if it exists, use PHA first, then goto parameter I/F
 *	3) if it exists, but isn't a PHA file, use RQ* keywords
 */
static void
get_problem(status)
	int	*status;
{
	static char	dd[MAX_FNAME*2], eh[MAX_FNAME*2],
			rd[MAX_FNAME*2], ct[MAX_FNAME*2],
			gd[MAX_FNAME*2],
			er[FITS_CLEN_ERRMSG];

	get_required(status);
	ERRCHK(*status,"Problem %d on required arguments\n",*status,return);

	get_from_pha(status);
	if (*status) {
		*status = 0;
		ERRCHK(1, "Infile %s is incomplete...\n",rmg_works.inp, 0);
		rmg_works.gmask = params_gmask;
		if (params_dmode == DM_BRIGHT && rqt.echo == 0.0)
			rqt.echo = 0.01;	/* force echo lookup */
		get_request(status);
		ERRCHK(*status,"...nor a useful FITS file (FITSIO err %d).\n",
			*status,0);
		ffgerr( *status, er );
		ERRCHK(*status,"FITSIO:  %s\n",er,return);
		ERRCHK(1,"...but was scanned for RQ* keywords.\n", 0, 0);
	}
	rmg_works.ddp = string_fix(dd, rmg_works.dd, MAX_FNAME-1);

	/* rmg_works.gdp = string_cat(gd, rmg_works.gd, MAX_FNAME-1); */
	/* (could be better integrated with get_thought) */
	RqtChk(spth, DF_SPTH); /*from get_thought, so can find ECD in caldb*/
	rmg_works.gdp = collie(gd, rmg_works.gd, MAX_FNAME-1, "ECD", status);

	/* rmg_works.ctp = string_cat(ct, rmg_works.ct, MAX_FNAME-1); */
	rmg_works.ctp = collie(ct,rmg_works.ct,MAX_FNAME-1,"GAIN HIST",status);

	/* rmg_works.ehp = string_cat(eh, rmg_works.eh, MAX_FNAME-1); */
	rmg_works.ehp = collie(eh,rmg_works.eh,MAX_FNAME-1,"ECHO_HIST",status);

	/* rmg_works.rdp = string_cat(rd, rmg_works.rd, MAX_FNAME-1); */
	rmg_works.rdp = collie(rd,rmg_works.rd,MAX_FNAME-1,"RDD_HIST",status);
	ERRCHK(*status,"Problem locating calibration file\n",0,return);

	get_arfinfo(status);
	ERRCHK(*status,"Problem with ARF %s\n",rmg_works.arp,return);

	get_thought(status);
	ERRCHK(*status,"I'm lost[%d]--are you?\n",*status,return);
}

/*
 *  Read the required arguments, and set up some defaults.
 */
static void
get_required(status)
	int	*status;
{
	static char	in[MAX_FNAME], ar[MAX_FNAME], rm[MAX_FNAME];
	static Real	ph[MAX_PBINS], pc[MAX_PBINS],
			ea[MAX_EBINS], ev[MAX_EVALS],
			eb[MAX_EBNDS], ec[MAX_EBNDS];
	/*
	 *  Connect trivial storage.
	 */
	rmg_works.evals = ev;	rmg_works.earea = ea;
	rmg_works.pharr = ph;	rmg_works.pcarr = pc;
	rmg_works.ebnds = eb;	rmg_works.ecnds = ec;
	/*
	 *  Default values that *must* be overridden.
	 */
	rmg_works.phmin = MAX_PBINS;
	rmg_works.phmax = -1;
	rmg_works.e_min = EMAX;
	rmg_works.e_max = EMIN;
	rmg_works.e_bin = 0;
	rmg_works.gmask = 0;
	rmg_works.chatr = C_NUL;
	rmg_works.rebin = 0;
	/*
	 * VMS sprintf can't deal with null char*, can't hurt on Unix.
	 */
	rmg_works.f0p = "(none)";
	rmg_works.f1p = "(none)";
	rmg_works.f2p = "(none)";
	rmg_works.f3p = "(none)";
	/*
	 *  Ok; read the required arguments.
	 */
	Uclgst("infile", rmg_works.in, status);
	Uclgst("arfile", rmg_works.ar, status);
	Uclgst("rmfile", rmg_works.rm, status);
	/*
	 *  Truncated strings for error messsages:
	 */
	rmg_works.inp = string_fix(in, rmg_works.in, MAX_FNAME-1);
	rmg_works.arp = string_fix(ar, rmg_works.ar, MAX_FNAME-1);
	rmg_works.rmp = string_fix(rm, rmg_works.rm, MAX_FNAME-1);
}

/*
 *  Treat the infile as a PHA file and see if that works.
 *  As a side effect, read all the hidden parameters.
 REVISIT
 */
static void
get_from_pha(status)
	int	*status;
{
	static int	ints[22];
	static double	dbls[2];
	static char	mesg[FITS_CLEN_COMMENT], er[FITS_CLEN_ERRMSG];

	/* load defaults */
	get_def_params(status);
	ERRCHK(*status,"Problem %d on hidden parameters\n",*status,return);

	/* now pack get_pha() input */
	/* Note that get_pha seems to be self-contained in its use of       */
	/* (fortran) fitsio; F_LUNIT is only used here, as its unit number. */
	ints[fi_lun] = F_LUNIT;
	ints[fi_chip_in] = (rqt.chip > -2 && rqt.chip < 4) ? rqt.chip : -1;

	get_pha(rmg_works.in, ints, dbls, mesg, status);
	ffgerr( *status, er );
	ERRCHK(*status, "FITSIO: %s\n", er, *status=1);
	/* mesg must be set to ' ' if pha is ok */
	ERRCHK(*mesg && *mesg != ' ',"GET_PHA: %s\n",mesg,*status=1;return);
	unpack_pha(ints, dbls);
}

/*
 *  Load the parameter file defaults:
 *	directly into rmg_works if we don't need to remember what
 *	the parameter choices were
 *	into params (for later insertion into rmg_works) if we
 *	might need to think about which to trust
 *
 REVISIT:	more thinking here or elsewhere?
 *
 *  status carries serious errors back up.
 */
#define UclgsD(N,P,S)	do {	/* read Real, store Double */	\
				static Real     temp;		\
				Uclgsr(N, &temp, S);		\
				*(P) = (double)temp;		\
			} while (0)
#define ParAss(M,V)	do {	/* if not bad, assign it */	\
				if (V != bpt.M)	rqt.M = V;	\
			} while (0)
static void
get_def_params(status)
	int	*status;
{
	static Real	rval;
	static int	ival;

	/* infile,arfile,rmfile				in get_required */
	UclgsD("vers",   &rqt.vers,     status);
	Uclgst("datadir", rmg_works.dd, status);
	Uclgst("ecdata",  rmg_works.gd, status);
	Uclgst("phtopi",  rmg_works.ct, status);
	Uclgst("echosh",  rmg_works.eh, status);
	Uclgst("rddhis",  rmg_works.rd, status);
	/* phmin,phmax					in ph_space */
	Uclgsb("sisde",  &rmg_works.sisde, status);
	Uclgsb("sispi",  &rmg_works.sispi, status);
	Uclgsi("gains",  &rmg_works.gains, status);

	/* emin,emax,ebin				in prep_arf */
	Uclgsi("first",  &rmg_works.first, status);

	UclgsD("epch",  &rval, status);		ParAss(epch, rval);
	Uclgsi("cmode", &ival, status);		ParAss(mode, ival);
	Uclgsi("detr",  &ival, status);		ParAss(detr, ival);
	Uclgsi("chip",  &ival, status);		ParAss(chip, ival);
	UclgsD("xcen",  &rval, status);		ParAss(xcen, rval);
	UclgsD("xwid",  &rval, status);		ParAss(xwid, rval);
	UclgsD("ycen",  &rval, status);		ParAss(ycen, rval);
	UclgsD("ywid",  &rval, status);		ParAss(ywid, rval);
	Uclgsi("evth",  &ival, status);		ParAss(evth, ival);
	Uclgsi("spth",  &ival, status);		ParAss(spth, ival);
	/* gmask,grades,rebin				in gr_check */
	dm_check(&params_dmode, status);
	gr_check(&params_gmask, status);
	Uclgsi("zerodef",&rmg_works.dfezd, status);
	Uclgsi("rddcorv",&rmg_works.rddcv, status);

	UclgsD("echo",  &rval, status);		ParAss(echo, rval);
	UclgsD("dark",  &rval, status);		ParAss(dark, rval);
	UclgsD("leak",  &rval, status);		ParAss(leak, rval);

	/* clobber					in rmf_open */
	do_chatr(status);
	ERRCHK(*status, "Using Version %g parameter file?\n", VERSION, 0);
}
#undef UclgsD
#undef ParAss

/*
 *  Unpack get_pha() output into rqt and rmg_works,
 *  zapping garbage returns and making no-brainer decisions.
 */
static void
unpack_pha(ints, dbls)
	int	*ints;
	double	*dbls;
{
	register int	gm = 0, reb;
	int		par_ebnds = 0;	/* bad */

	RqtChk(detr, ints[fi_inst]);
	RqtChk(chip, ints[fi_chip]);
	RqtChk(xcen, ints[fi_rawx]);
	RqtChk(ycen, ints[fi_rawy]);

	RqtChk(mode, ints[fi_cmode]);
	if ((params_dmode || (params_dmode = ints[fi_dmode]))
		&& (params_dmode == DM_FAST)) rqt.mode = 0;

	RqtChk(epch, dbls[fi_ascatime]);
	RqtChk(evth, ints[fi_event_th]);
	RqtChk(spth, ints[fi_split]);

	rmg_works.sispi = ints[fi_pi];
	ERRCHK(ints[fi_chan1] < 1, "Setting 1st PHA channel to 0\n",0,
		rmg_works.first = 0);

	/* REVISIT:  force sensible grades coming from PHA file */
	/* This can be overridden using the parameter interface */
	/* Also hack on params_gmask if rebin/dmode unspecified */
	if (params_dmode == DM_FAST) {	/* FAST grades */
		gm = ints[fi_gmask] << 10;
		gm &= MSK_FAST0;
		/* if (!MSK_OK(params_gmask)) par_ebnds = MSK_BRITE; */
	} else {			/* FAINT/BRIGHT grades */
		gm = ints[fi_gmask] << 1;
		gm &= MSK_FTBT0|MSK_FTBT2|MSK_FTBT3|MSK_FTBT4|MSK_FTBT6;
	}
	if (ints[fi_bright]) {
		if (!MSK_OK(params_gmask)) par_ebnds = MSK_BRITE;
		gm |= MSK_BRITE;
		reb = 2048 / ints[fi_n_chan];
	} else {
		if (!MSK_OK(params_gmask)) par_ebnds = MSK_FAINT;
		gm |= MSK_FAINT;
		reb = 4096 / ints[fi_n_chan];
	}
	while (reb >>= 1) gm += MSK_RBIN1;
	if (!MSK_OK(params_gmask)) {
		params_gmask |= par_ebnds;
		params_gmask |= (gm & MSK_REBIN);
	}

	/* PHA rules, unless parameter input is really ok */
	if (MSK_OK(params_gmask) && (params_gmask&MSK_GRADE)) {
		rmg_works.gmask = params_gmask;
		ERRCHK(rmg_works.chatr & C_HLP,
			"%s PHA grades bypassed.\n", (ints[fi_tr_grade])
			? "Trusty" : "Dubious",0);
	} else {
		rmg_works.gmask = gm;
	}

	/* Echo correct/corrupt */
	rmg_works.dfezd = ints[fi_zerodef];
	if ((params_dmode == DM_BRIGHT && rqt.echo == 0.0) ||
	    (rmg_works.dfezd == DFE_BRIGHT))
		rqt.echo = 0.01;	/* i.e. nonzero forces lookup */
	if (rmg_works.dfezd == DFE_NEW || rmg_works.dfezd == DFE_OLD)
		rqt.echo = 0.0;		/* i.e. assume echo corrected */

	/* RDD Correction override */
	if (rmg_works.rddcv < 0) rmg_works.rddcv = ints[fi_rddcv];
	/* otherwise it was set from the parameter interface */

	/* REVISIT: This is a blizzard of possibilities */
}

/*
 *  Look over input acquired so far and decide if we can proceed.
 *  If not, query the human for a bit more information....
 *
 *  The exact precedence of the different ways of specifying EMIN/EMAX
 *  and CHATR needs to be redressed.  However, for the sake of compilation:
 *  A serious issue here is the ultimate configuration of the HOST interface
 *  regarding whether it uses /dev/tty or stdin....stay tuned.
 */
static void
get_thought(status)
	int	*status;
{
	register char	*x, *y;

	/*
	 *  Check on the response mask.
	 */
	ERRCHK(!MSK_OK(rmg_works.gmask),"Grade Mask ill: %04x\n",
		rmg_works.gmask,*status=1111;return);
	if (rmg_works.chatr & C_HLP) {
#define GRADE_HELP "\n\
Warning:  Grade %d response selected! You should probably\n\
          override this with grades and rebin parameters.\n\n"
		ERRCHK(rmg_works.gmask & MSK_FTBT1, GRADE_HELP, 1, 0);
		ERRCHK(rmg_works.gmask & MSK_FTBT5, GRADE_HELP, 5, 0);
		ERRCHK(rmg_works.gmask & MSK_FTBT6, GRADE_HELP, 6, 0);
		ERRCHK(rmg_works.gmask & MSK_FTBT7, GRADE_HELP, 7, 0);
	}
	ERRCHK( (rmg_works.chatr & C_HLP) && ( ! MXOR(
		 (rmg_works.gmask&(MSK_FAST0)),
		 (rmg_works.gmask&(MSK_FTBT0|MSK_FTBT2|MSK_FTBT3|MSK_FTBT4))
		) ), "Implausible mix of grades %04x--expect problems...\n",
		rmg_works.gmask, 0);

	/*
	 *  Setup ph & pc space to account for the different modes.
	 */
	ph_space(status);

	/*
	 *  A few last things....
	 */
	if (rqt.temp < 0) rqt.temp += 273.15;	/* C -> K */
	/* SIS0 = sensor5, SIS1 = sensor3, SIS2 = sensor2, SIS4 = sensor4 */
	/* Yah, right: from now on, only SIS0 and SIS1 */
	ERRCHK((rqt.detr < 0 || rqt.detr > 1),
		"SIS%d not supported.\n", rqt.detr, *status=11111;return);
	ERRCHK((rqt.chip < 0 || rqt.chip > 3),
		"Chip%d not supported.\n",rqt.chip, *status=11112;return);
	ERRCHK((rqt.mode < 0 || rqt.mode == 3 || rqt.mode > 4),
		"%dCCD mode is not supported.\n",rqt.mode,
			*status=11113;return);
	/*
	 *  If these haven't been set yet...
	 */
	RqtChk(evth, DF_EVTH); RqtChk(spth, DF_SPTH);
	RqtChk(xcen, DF_XCEN); RqtChk(xwid, DF_XWID);
	RqtChk(ycen, DF_YCEN); RqtChk(ywid, DF_YWID);

	/*
	 *  Ensure correct numbers in ECD filename: .*sisNcMpSP_.*
	 *                                            0  3 5 78
	 *  Only splits of 20 and 40 are supported.
	 */
	for (x = rmg_works.gdp; *x; x++)
		if (x[0] == 's' && x[1] == 'i' && x[2] == 's' &&
		    x[4] == 'c' && x[6] == 'p' && x[9] == '_') break;
	if (x[0] == 's') {
		x[3] = '0'+rqt.detr;
		x[5] = '0'+rqt.chip;
		if (rqt.spth > 30.) { x[7] = '4'; x[8] = '0'; }
		else                { x[7] = '2'; x[8] = '0'; }
		for (y = rmg_works.gd; y < rmg_works.gd+MAX_FNAME; y++)
			if (y[0] == 's' && y[1] == 'i' && y[2] == 's' &&
			    y[4] == 'c' && y[6] == 'p' && y[9] == '_') break;
		if (y[0] == 's') {
			y[3] = x[3];	y[5] = x[5];
			y[7] = x[7];	y[8] = x[8];
		}
	} else {
		ERRCHK(1,"Nonstandard ecdata file name %s\n",
			rmg_works.gdp,/*not_fatal*/0);
	}

	if (rmg_works.chatr & C_INP) do_report("Ready for Points:\n");
}

/*
 *  Dump our brains.
 REVISIT
 */
static void
do_report(label)
	char *label;
{
	ERRCHK(1, label, 0, 0);
	/* more stuff */
	ERRCHK(1, " vers = %.3lf\t", rqt.vers, 0);
	ERRCHK(1, " epch = %.1le\t", rqt.epch, 0);
	ERRCHK(1, " evth = %.0lf\t", rqt.evth, 0);
	ERRCHK(1, " spth = %.0lf\n", rqt.spth, 0);
	ERRCHK(1, " xcen = %.1lf\t", rqt.xcen, 0);
	ERRCHK(1, " ycen = %.1lf\t", rqt.ycen, 0);
	ERRCHK(1, " xwid = %.1lf\t", rqt.xwid, 0);
	ERRCHK(1, " ywid = %.1lf\n", rqt.ywid, 0);
	ERRCHK(1, " echo = %.3lf\t", rqt.echo, 0);
	ERRCHK(1, " leak = %.3lf\t", rqt.leak, 0);
	ERRCHK(1, " dark = %.3lf\n", rqt.dark, 0);
	ERRCHK(1, " detr = %d\t",    rqt.detr, 0);
	ERRCHK(1, " chip = %d\t",    rqt.chip, 0);
	ERRCHK(1, " mode = %d\n",    rqt.mode, 0);
	/* more stuff */
	ERRCHK(1, "E_min = %.3lf\t", rmg_works.e_min,  0);
	ERRCHK(1, "E_max = %.3lf\t", rmg_works.e_max,  0);
	ERRCHK(1, "E_bin = %d\n",    rmg_works.e_bin,  0);
	/* more stuff */
	ERRCHK(1, "Pcmin = %4d\t",   rmg_works.pcmin,  0);
	ERRCHK(1, "Pcmax = %4d\t",   rmg_works.pcmax,  0);
	ERRCHK(1, "Pcbin = %4d\n",   rmg_works.pcbin,  0);
	ERRCHK(1, "Rebin = %4d\t",   rmg_works.rebin,  0);
	ERRCHK(1, "First = %4d\t",   rmg_works.first,  0);
	ERRCHK(1, "Gains = %4d\n",   rmg_works.gains,  0);
	/* more stuff */
	ERRCHK(1, "GMask = %#x\t",   rmg_works.gmask,  0);
	ERRCHK(1, "Pgmsk = %#x\t",   params_gmask,     0);
	ERRCHK(1, "Chatr = %#x\n",   rmg_works.chatr,  0);
	/* more stuff */
	ERRCHK(1, "SISde = %s\t",    (rmg_works.sisde)?"Yes":"No",  0);
	ERRCHK(1, "SISpi = %s\n",    (rmg_works.sispi)?"Yes":"No",  0);
	ERRCHK(1, "RDDcv = %4d\t",   rmg_works.rddcv,  0);
	ERRCHK(1, "DFEzd = %s\n",    (rmg_works.dfezd==DFE_OLD) ? "Old" :
				     (rmg_works.dfezd==DFE_NEW) ? "New" :
				     (rmg_works.dfezd==DFE_BRIGHT) ? "Bright" :
				     "Unknown",  0);
	/* more stuff */
	ERRCHK(1, "input = %s\n",    rmg_works.inp, 0);
	ERRCHK(1, "  arf = %s\n",    rmg_works.arp, 0);
	ERRCHK(1, "  rmf = %s\n",    rmg_works.rmp, 0);
	ERRCHK(1, " data = %s\n",    rmg_works.ddp, 0);
	ERRCHK(1, " aarf = %s\n",    rmg_works.f0p, 0);
	ERRCHK(1, "  gau = %s\n",    rmg_works.f1p, 0);
	ERRCHK(1, "diplo = %s\n",    rmg_works.f2p, 0);
	ERRCHK(1, "  gbr = %s\n",    rmg_works.f3p, 0);
	ERRCHK(1, "  ecd = %s\n",    rmg_works.gdp, 0);
	ERRCHK(1, "  cti = %s\n",    rmg_works.ctp, 0);
	ERRCHK(1, " echo = %s\n",    rmg_works.ehp, 0);
	ERRCHK(1, "  rdd = %s\n",    rmg_works.rdp, 0);
	/* more stuff */
	ERRCHK(1, "dmode = %s\n\n",
		(params_dmode == DM_BRIGHT2) ? "Bright2" :
		(params_dmode == DM_BRIGHT) ? "Bright" :
		(params_dmode == DM_FAST) ? "Fast" : "Unknown", 0);
}

/*
 *  Brief description of what is about to happen.
 */
static void
do_terse()
{
	static char descrip[120];

	(void)sprintf(descrip, "Making v%.1lf %dx%d S%dC%d %s %s RMF\n",
		rqt.vers, rmg_works.e_bin, rmg_works.pcbin,
		(int)rqt.detr, (int)rqt.chip,
		(params_dmode == DM_BRIGHT2) ? "Bright2" :
		 (params_dmode == DM_BRIGHT) ? "Bright" :
		 (params_dmode == DM_FAST) ? "Fast" : "Unknown",
		 (rmg_works.sispi)?"PI":"PHA");
	ERRCHK(1,descrip,0,0);

	/*
	 * Original, "ERRCHK(1,"Calibration data from %s\n",rmg_works.ddp,0);"
	 * is misleading for CALDB case.  This is too, for ASCII fallback.
	 */
	ERRCHK(1,"Calibration data files:\n",0,0);
	ERRCHK(1, "  ecd = %s\n",    rmg_works.gdp, 0);
	ERRCHK(1, "  cti = %s\n",    rmg_works.ctp, 0);
	ERRCHK(1, " echo = %s\n",    rmg_works.ehp, 0);
	ERRCHK(1, "  rdd = %s\n",    rmg_works.rdp, 0);
}

/*
 *  The chatr parameter can either be a hex mask, or else some wordage.
 */
static void
do_chatr(status)
	int	*status;
{
	static char	bf[MAX_FNAME];

	Uclgst("chatr", bf, status);
	rmg_works.chatr = parse_mask(bf, C_NUL);
	if (rmg_works.chatr == C_NUL)
		switch(*bf) {
		case 'n':
		case 'N':
			rmg_works.chatr = C_NONE;
			break;
		case 'y':
		case 'Y':
			rmg_works.chatr = C_SOME;
			break;
		default:
			rmg_works.chatr = C_LOTS;
			break;
		}
}

/*
 *  Get & Check the Grade mask.
 *  Only one energy-ph mapping to a customer.
 *  Emit some words to the wise.
	REVISIT
	ERRCHK(rmg_works.gmask & MSK_FTBT1,
		"Warning: You should not use Grade 1 data\n",0,0);
	ERRCHK(rmg_works.gmask & MSK_FTBT5,
		"Warning: Grade 5 response is approximate\n",0,0);
	ERRCHK(rmg_works.gmask & MSK_FTBT6,
		"Warning: Grade 6 response is approximate\n",0,0);
	ERRCHK(rmg_works.gmask & MSK_FTBT7,
		"Warning: You should not use Grade 7 data\n",0,0);
 */
static void
gr_check(gmask, status)
	int	*gmask, *status;
{
	static char	buffer[MAX_FNAME];
	register char	*b;

	/* try gmask parameter first */
	Uclgst("gmask", buffer, status);
	*gmask = parse_mask(buffer, MSK_EBNDS);
	if (MSK_OK(*gmask)) return;

	Uclgst("grades", buffer, status);
	if (!strcmp(buffer, "UNKNOWN")) return;
	*gmask = 0;
	if (params_dmode == DM_FAST) {
		for (b = buffer; *b; b++)
			*gmask |= MSK_FAST0 << *b - '0';
	} else {
		for (b = buffer; *b; b++)
			*gmask |= MSK_FTBT0 << *b - '0';
	}

	Uclgst("rebin", buffer, status);
	if (!strcmp(buffer, "1"))   *gmask |= MSK_FAINT;	else
	if (!strcmp(buffer, "1b"))  *gmask |= MSK_BRITE;	else
	if (!strcmp(buffer, "2"))   *gmask |= MSK_FAINT|0x1000;	else
	if (!strcmp(buffer, "2b"))  *gmask |= MSK_BRITE|0x1000;	else
	if (!strcmp(buffer, "4"))   *gmask |= MSK_FAINT|0x2000;	else
	if (!strcmp(buffer, "4b"))  *gmask |= MSK_BRITE|0x2000;	else
	if (!strcmp(buffer, "8"))   *gmask |= MSK_FAINT|0x3000;	else
	if (!strcmp(buffer, "8b"))  *gmask |= MSK_BRITE|0x3000;	else
	if (!strcmp(buffer, "16"))  *gmask |= MSK_FAINT|0x4000;	else
	if (!strcmp(buffer, "16b")) *gmask |= MSK_BRITE|0x4000;
	if (MSK_OK(*gmask)) return;

	/*
	 * bad news answer
	 * actually, at this point, gmask is incomplete, so it should barf.
	*gmask = MSK_EBNDS;
	 */
}

/*
 *  Grab user supplied datamode for later reference.
 */
static void
dm_check(dmode, status)
	int	*dmode, *status;
{
	static char	buffer[MAX_FNAME];

	Uclgst("dmode", buffer, status);
	if (!strcmp(buffer, "BRIGHT2"))	*dmode = DM_BRIGHT2;	else
	if (!strcmp(buffer, "BRIGHT"))	*dmode = DM_BRIGHT;	else
	if (!strcmp(buffer, "FAST"))	*dmode = DM_FAST;	else
					*dmode = 0;
}

/*
 *  Parse the grade mask and chatter masks to allow the user some
 *  flexibility on input.  For the present, insist on 0x%x input.
 */
static int
parse_mask(s, x)
	char	*s;
	int	x;
{
	static int	hmask;

	if (s[0] == '\'') s++;		/* for 'strings' */

	if (s[0] == '0' && s[1] == 'x') {
		/* get mask based on 0x.... input */
		(void)sscanf(s, "0x%x", &hmask);
		return(hmask);
	} else if (s[0] == '0' && s[1] == 'X') {
		/* get mask based on 0X.... input */
		(void)sscanf(s, "0X%x", &hmask);
		return(hmask);
	}
	return(x);
}

/*
 *  Figure out how the PH output space is to be configured.
 */
static void
ph_space(status)
	int	*status;
{
	/*
	 *  Get the rebin factor correct; MSK_RB is log2(rebin).
	 */
	rmg_works.rebin = (1 << MSK_RB(rmg_works.gmask));
	ERRCHK(rmg_works.chatr & C_INP,
		"Rebin = %d\n", rmg_works.rebin, 0);
	/*
	 *  Make sure PH request is in bounds.
	 *  Catch lack of file input via ``bad values''
	 */
	if (rmg_works.phmin == MAX_PBINS)
		Uclgsi("phmin", &rmg_works.phmin, status);
	if (rmg_works.phmax == -1)
		Uclgsi("phmax", &rmg_works.phmax, status);
	if (rmg_works.phmin < 0)
		rmg_works.phmin = 0;
	if (rmg_works.phmax > rmg_works.phmin + MAX_PBINS - 1)
		rmg_works.phmax = rmg_works.phmin + MAX_PBINS - 1;
	/*
	 *  Work out the compressed PH scale.
	 */
	if (rmg_works.gmask & MSK_BRITE) {
		rmg_works.pcmin = PC(rmg_works.phmin) / rmg_works.rebin;
		rmg_works.pcmax = PC(rmg_works.phmax) / rmg_works.rebin;
		rmg_works.phmin = PH(rmg_works.pcmin * rmg_works.rebin
				+ rmg_works.rebin - 1);
		rmg_works.phmax = PH(rmg_works.pcmax * rmg_works.rebin
				+ rmg_works.rebin - 1);
		if (rmg_works.phmax >= 2048) rmg_works.phmax += 3; else
		if (rmg_works.phmax >= 1024) rmg_works.phmax += 1;
	} else {
		rmg_works.pcmin = rmg_works.phmin / rmg_works.rebin;
		rmg_works.pcmax = rmg_works.phmax / rmg_works.rebin;
		rmg_works.phmin = rmg_works.pcmin * rmg_works.rebin;
		rmg_works.phmax = rmg_works.pcmax * rmg_works.rebin
				+ rmg_works.rebin - 1;
	}
	rmg_works.phbin = rmg_works.phmax - rmg_works.phmin + 1;
	rmg_works.pcbin = rmg_works.pcmax - rmg_works.pcmin + 1;
	/*
	 *  Diagnostic output.
	 REVISIT
	 */
	if (rmg_works.chatr & C_INP) {
		ERRCHK(1,"Phmax = %d\t",rmg_works.phmax,0);
		ERRCHK(1,"Phmin = %d\t",rmg_works.phmin,0);
		ERRCHK(1,"Phbin = %d\n",rmg_works.phbin,0);
		ERRCHK(1,"Pcmax = %d\t",rmg_works.pcmax,0);
		ERRCHK(1,"Pcmin = %d\t",rmg_works.pcmin,0);
		ERRCHK(1,"Pcbin = %d\n",rmg_works.pcbin,0);
	}
}

/*
 *  Direct connect to a model.
 *  Probably not too useful.
 */
static int
side_step(rmfi)
	char	*rmfi;
{
	if (!strncmp(rmfi, "NONE", 4) ||
	    !strncmp(rmfi, "None", 4) ||
	    !strncmp(rmfi, "none", 4)) {
		return(1);
	}
	if (!strncmp(rmfi, "MODEL", 5) ||
	    !strncmp(rmfi, "Model", 5) ||
	    !strncmp(rmfi, "model", 5)) {
		if (rmfi[5] != 's' && rmfi[5] != 'S') {
			rqt.detr = (atoi(rmfi+5) % 10) / 4;
			rqt.chip = (atoi(rmfi+5) % 10) % 4;
		} else {
			rqt.detr = rmfi[6] - '0';
			rqt.chip = rmfi[8] - '0';
			(void)sprintf(rmfi, "Model%d",
				SISsCHIPc + 4 * rqt.detr + rqt.chip);
		}
		return(1);
	}
	return(0);
}

/*
 *  Query the input file for the requested point configuration.
 *  All items are optional.
 */
#define G_S(S,K,C,A)	do {						\
				ffgkey( f_iunit, K, val, com, status ); \
				if (!*status) (A)(S.C,val);		\
				if (*status) *status = 0;		\
			} while(0)
#define G_K(S,K,C,A)	do {						\
				ffgkey( f_iunit, K, val, com, status ); \
				if (!*status) S.C =  (A)(val);		\
				if (*status) *status = 0;		\
			} while(0)
#define GK(S,K,C,A,B) do {						\
				ffgkey( f_iunit, K, val, com, status ); \
				if (!*status) S.C =  (A)(val,B);	\
				if (*status) *status = 0;		\
			} while(0)
static void
get_request(status)
	int	*status;
{
	static char	val[FITS_CLEN_KEYVAL],
			com[FITS_CLEN_COMMENT],
			nam[FITS_CLEN_KEYVAL];
	static int	ext, blk, hdu;
	extern int	atoi();
	extern double	atof();

	if (side_step(rmg_works.inp)) return;

	FFCPARS(rmg_works.in, nam, &ext, status);
	if (++ext<=0) ext = 1;	/* default is primary */

	ffopen( &f_iunit, nam, 0, status );

	ERRCHK(*status, "Problem opening RMG input %s.\n",
		rmg_works.inp, return);

	ffmahd( f_iunit, ext, &hdu, status );

	ERRCHK(*status, "Problem shifting to ext=%d", ext, return);

	ERRCHK(1, "Reading RQ* keywords of %s",rmg_works.inp, 0);
	ERRCHK(1, "[%d]\n", ext-1, 0);
	/*
	 *  Grab whatever is available.
	 */
	G_K(rqt,"rqvers  ",vers,atof);
	G_K(rqt,"rqepch  ",epch,atof);
	G_K(rqt,"rqevth  ",evth,atof);
	G_K(rqt,"rqspth  ",spth,atof);
	G_K(rqt,"rqxcen  ",xcen,atof);
	G_K(rqt,"rqxwid  ",xwid,atof);
	G_K(rqt,"rqycen  ",ycen,atof);
	G_K(rqt,"rqywid  ",ywid,atof);
	G_K(rqt,"rqtemp  ",temp,atof);
	G_K(rqt,"rqevpf  ",evpf,atof);
	G_K(rqt,"rqimhi  ",imhi,atof);
	G_K(rqt,"rqimlo  ",imlo,atof);

	G_K(rqt,"rqecho  ",echo,atof);
	G_K(rqt,"rqleak  ",leak,atof);
	G_K(rqt,"rqdark  ",dark,atof);

	G_K(rqt,"rqdetr  ",detr,atoi);
	G_K(rqt,"rqchip  ",chip,atoi);
	G_K(rqt,"rqmode  ",mode,atoi);

	/*
	 *  Last word over parameter data.
	 */
	G_K(rmg_works, "rqpmin  ", phmin, atoi);
	G_K(rmg_works, "rqpmax  ", phmax, atoi);
	GK (rmg_works, "rqmask  ", gmask, parse_mask, MSK_FAINT&MSK_BRITE);
	GK (rmg_works, "rqchtr  ", chatr, parse_mask, C_NUL);
	G_K(rmg_works, "rqebin  ", e_bin, atoi);
	G_K(rmg_works, "rqemin  ", e_min, atof);
	G_K(rmg_works, "rqemax  ", e_max, atof);

	G_K(rmg_works, "rqfrst  ", first, atoi);
	G_K(rmg_works, "rqgans  ", gains, atoi);
	G_K(rmg_works, "rqdeff  ", sisde, atoi);
	G_K(rmg_works, "rqdopi  ", sispi, atoi);
	G_K(rmg_works, "rqrdcv  ", rddcv, atoi);
	G_K(rmg_works, "rqdfez  ", dfezd, atoi);

	G_S(rmg_works, "rqdir0  ", dd,    to_string);

	G_S(rmg_works, "rqfil0  ", f0,    to_string);
	G_S(rmg_works, "rqfil1  ", f1,    to_string);
	G_S(rmg_works, "rqfil2  ", f2,    to_string);
	G_S(rmg_works, "rqfil3  ", f3,    to_string);

	G_S(rmg_works, "rqgecd  ", gd,    to_string);
	G_S(rmg_works, "rqphpi  ", ct,    to_string);
	G_S(rmg_works, "rqehis  ", eh,    to_string);
	G_S(rmg_works, "rqrddh  ", rd,    to_string);

	ffclos( f_iunit, status );
}

/*
 *  This routine opens the ARF and populates the
 *  effective area and energy space information.
 REVISIT:
	should check column names, etc.
	was that what the CHK_STR macro was for?
 */
static void
get_arfinfo(status)
	int	*status;
{
	static char	val[FITS_CLEN_KEYVAL],
			com[FITS_CLEN_COMMENT],
			nam[FITS_CLEN_KEYVAL],
			elo[FITS_CLEN_KEYNAME] = "ENERG_LO",
			ehi[FITS_CLEN_KEYNAME] = "ENERG_HI",
			spc[FITS_CLEN_KEYNAME] = "SPECRESP";
	static int	ext, blk, hdu, col, zero;
	extern int	atoi();

	if (!strncmp("NONE",rmg_works.ar,4) ||
	    !strncmp("None",rmg_works.ar,4) ||
	    !strncmp("none",rmg_works.ar,4)) { barf(status);return; }

	FFCPARS(rmg_works.ar, nam, &ext, status);
	if (++ext<=0) ext = 2;	/* default is 1st bintable */

	ffopen( &f_iunit, nam, 0, status );
	ffmahd( f_iunit, ext, &hdu, status );

	ERRCHK(*status,"Improvising ARF from parameters...\n",0,
		barf(status);return
	);

	/* does it have to be an exact match? */
	ffgcno(f_iunit, FALSE, elo, &col, status);
	ffgcno(f_iunit, FALSE, ehi, &col, status);
	ffgcno(f_iunit, FALSE, spc, &col, status);

	G_K(rmg_works, "naxis2  ", e_bin, atoi);

	ERRCHK(*status,"Status = %d on ARF?\n",*status,return);
	ERRCHK(rmg_works.e_bin > MAX_EBINS,"ebin was %d; resetting\n",
		rmg_works.e_bin,rmg_works.e_bin = MAX_EBINS);

	ffgcve(f_iunit, 1, 1l, 1l, (long)rmg_works.e_bin,   /* ENERG_LO */
		0., rmg_works.evals, &zero, status);
	ffgcve(f_iunit, 2, 1l, 1l, rmg_works.e_bin,        /* ENERG_HI */
		0., rmg_works.evals+1, &zero, status);
	ffgcve(f_iunit, 3, 1l, 1l, rmg_works.e_bin,     /* SPECRESP */
		0., rmg_works.earea, &zero, status);

	ERRCHK(*status,"Status = %d on Bintable?\n",*status,return);

	ffclos(f_iunit, status);

	rmg_works.e_min = rmg_works.evals[0];
	rmg_works.e_max = rmg_works.evals[rmg_works.e_bin];

	set_eamax();
}

/*
 *  Get the maximum of the arf effective area.  A useful diagnostic.
 */
static void
set_eamax()
{
	register int	i;

	for (i = 0, rmg_works.eamax = 0.0; i < rmg_works.e_bin; i++)
		if (rmg_works.earea[i] > rmg_works.eamax)
			rmg_works.eamax = rmg_works.earea[i];
	ERRCHK(rmg_works.chatr & C_ARF, "Maximum effective area %g\n",
		rmg_works.eamax, 0);
}

/*
 *  Try to build the ARF from the parameter/RQ values.
 */
static void
barf(status)
	int	*status;
{
	ERRCHK(prep_arf(status),
		"Emin/Emax/Ebin values incorrect--no ARF!\n",0,return);
	ERRCHK(fake_arf(status),
		"Tried Ascii ARF and failed!\n",0,return);
	cal_stuff(0, 'A', (rmg_works.eamax != 1.0), (Point *)0);
}

/*
 *  Figure out the right way to fake the ARF.
 */
static int
prep_arf(status)
	int	*status;
{
	if (rmg_works.e_bin == 0 || rmg_works.e_bin > MAX_EBINS
		|| rmg_works.e_min >= rmg_works.e_max) {
		Uclgsr("emin",  &rmg_works.e_min, status);
		Uclgsr("emax",  &rmg_works.e_max, status);
		Uclgsi("ebin",  &rmg_works.e_bin, status);
		if (rmg_works.e_bin >  MAX_EBINS)
			rmg_works.e_bin = MAX_EBINS;
		if (rmg_works.e_bin < -MAX_EBINS)
			rmg_works.e_bin = -MAX_EBINS;
	}
	if (rmg_works.f0[0])
		rmg_works.e_bin = 0;	/* File ARF */
	else if (rmg_works.e_bin == 0 || rmg_works.e_bin > MAX_EBINS
			|| rmg_works.e_min >= rmg_works.e_max)
		return(1);

	return(0);
}

/*
 *  This constructs a fake ARF from the ebin/emin/emax data,
 *  or else by reading an effective area file of the form
 *  energy, area, transmission, or else by substituting unit
 *  unit effective area; evals are det'd by rmg_works.e_bin.
 */
static int
fake_arf(status)
	int	*status;
{
	double		de;
	register int	i, im1;

	rmg_works.eamax = 1.0;

	if (rmg_works.e_bin > 0) {
		de = (rmg_works.e_max - rmg_works.e_min) / rmg_works.e_bin;

		rmg_works.evals[0] = rmg_works.e_min;
		for (i = 1, im1 = 0; i < rmg_works.e_bin; i++, im1++) {
			rmg_works.evals[ i ] = rmg_works.evals[im1] + de;
			rmg_works.earea[im1] = 1.0;
		}
		rmg_works.evals[ i ] = rmg_works.e_max;
		rmg_works.earea[im1] = 1.0;
	} else if (rmg_works.e_bin < 0) {
		rmg_works.e_bin = - rmg_works.e_bin;
		de = pow(rmg_works.e_max/rmg_works.e_min,1.0/rmg_works.e_bin);

		rmg_works.evals[0] = rmg_works.e_min;
		for (i = 1, im1 = 0; i < rmg_works.e_bin; i++, im1++) {
			rmg_works.evals[ i ] = rmg_works.evals[im1] * de;
			rmg_works.earea[im1] = 1.0;
		}
		rmg_works.evals[ i ] = rmg_works.e_max;
		rmg_works.earea[im1] = 1.0;
	} else {
		FILE		*fp;
		Real		trans;
		static char	f0[MAX_FNAME*2];

		ERRCHK((!rmg_works.f0[0] || rmg_works.f0[0] == ' '),
			"Ebins = 0 but no file '%s'\n",rmg_works.f0,
			return(*status = 10008));
		rmg_works.f0p = string_cat(f0, rmg_works.f0, MAX_FNAME-1);
		fp = fopen(rmg_works.f0p, "r");
		ERRCHK(!fp, "File %s not found.\n",
			rmg_works.f0p, return(*status = 10008));

		for (i = 0; 3 == fscanf(fp,"%g %g %g", &rmg_works.evals[i],
			&rmg_works.earea[i], &trans); i++) {
			rmg_works.earea[i] *= trans;
			if (rmg_works.evals[i] < rmg_works.e_min) {
				i--;
				continue;
			}
			if (rmg_works.earea[i] > rmg_works.eamax)
				rmg_works.eamax = rmg_works.earea[i];
			if (rmg_works.evals[i] > rmg_works.e_max)
				break;
		}
		rmg_works.e_bin = --i;
		rmg_works.e_min = rmg_works.evals[0];
		rmg_works.e_max = rmg_works.evals[i];

		fclose(fp);
		return(*status = 0);
	}

	set_eamax();			/* get value for diagnostic */

	if (rmg_works.chatr & C_XRT) {
		ERRCHK(1,"XEA(%.3e)",rmg_works.evals[0],0);
		ERRCHK(1," = %.3e\n",rmg_works.earea[0],0);
		ERRCHK(1,"XEA(%.3e)",rmg_works.evals[rmg_works.e_bin/4],0);
		ERRCHK(1," = %.3e\n",rmg_works.earea[rmg_works.e_bin/4],0);
		ERRCHK(1,"XEA(%.3e)",rmg_works.evals[rmg_works.e_bin/2],0);
		ERRCHK(1," = %.3e\n",rmg_works.earea[rmg_works.e_bin/2],0);
		ERRCHK(1,"XEA(%.3e)",rmg_works.evals[rmg_works.e_bin-1],0);
		ERRCHK(1," = %.3e\n",rmg_works.earea[rmg_works.e_bin-1],0);
		ERRCHK(1,"XEA( MAXIMUM ) = %.3e\n",rmg_works.eamax,0);
	}
	return(*status = 0);
}

/*
 *  Create the RMF output file with trivial primary header
 *  and NULL primary array.  Extensions created elsewhere.
 */
static void
rmf_primary(cpp, status)
	Point	*cpp;
	int	*status;
{
	static int	nax = 0;
	static long     nxs[2];
	static double	bscl, bzer;

	if (rmf_open(status)) return;
	/*
	 *  Normally no primary array data is written, but
	 *  if the RM is to be also placed in primary array,
	 *  we need to flesh out the header with more info.
	 */
	if (rmg_works.chatr & C_PRI)	{
		nax = 2;
		nxs[0] = rmg_works.pcbin;
		nxs[1] = rmg_works.e_bin;
	}
	ffphpr(f_ounit, TRUE, 16, nax, nxs, 0l, 1l, TRUE, status);
	/*
	 *  Standard disclaimer.
	 */
	motherhood(status);
	/*
	 *  Dump out the calibration point info.
	 */
	if (rmg_works.chatr & C_KEY) rmf_verbiage(cpp, status);

	/*
	 *  Setup automatic scaling if RM in primary.
	 */
	if (rmg_works.chatr & C_PRI)	{
		ffpcom(f_ounit,"Primary array is a response matrix image",
			status);
		ffpcom(f_ounit,"which is included here as debugging aid.",
			status);
		bscl = RM_MAX * rmg_works.rebin / (2.0 * B_RANGE);
		if (rmg_works.chatr & C_ARF) bscl *= rmg_works.eamax;
		ffpkyd(f_ounit, "BSCALE  ", bscl, 5,
			"REAL = TAPE*BSCALE + BZERO", status);
		bzer = bscl * B_RANGE;
		ffpkyd(f_ounit, "BZERO   ", bzer, 5,
			"Matrix values are positive", status);
		ffpscl(f_ounit, bscl, bzer, status);
		ffpnul(f_ounit, 0l, status);
	}
}

/*
 *  Open the RM file, clobbering if possible.
 *  Nonzero return will be fatal.
 */
static int
rmf_open(status)
	int	*status;
{
	static char	er[FITS_CLEN_ERRMSG];
	int		doit = 0;

	/* Open the output file with ReadWrite access */
	ffinit( &f_ounit, rmg_works.rm, status);
	if (!*status) return(0);	/* happy, happy */
	ffgerr( *status, er);

	*status = 0;			/* for Uclgsb */
	Uclgsb("clobber", &doit, status);
	ERRCHK(*status, "Unable to get clobber mode.\n",0,return(1112));
	ERRCHK(!doit,"FITSIO: %s && clobber == no.\n",er,return(1113));

        *status = 0;
        ffclos(f_ounit, status);
        remove(rmg_works.rm);
	/* DEL_FIL(rmg_works.rm, status); */	/* does delete the file */
	/* ERRCHK(*status, "Delete [err = %d] failed.\n", *status,return(1114));           */
       

	/* Try again to open the output file ReadWrite */
	*status = 0;
	ffinit( &f_ounit, rmg_works.rm, status);
	ffgerr(*status, er);
	ERRCHK(*status, "FITSIO: %s && no other options.\n",er,return(1115));
	return(*status);
}

/*
 *  Provide the standard data disclaimer.
 */
static void
motherhood(status)
	int     *status;
{
	static char	rec[FITS_CLEN_CARD];
	char		**c;

	for (c = disclaimer; **c != '\0'; c++) {
	  ffpcom (f_ounit, *c, status );
	}
}

/*
 REVISIT -- might not need all the strncpys....
 */
#define PUT_E(P,K,M,C)	do {						\
				ffpkye(f_ounit,K,(float)(P)->M,5,C,status);\
			} while(0)
#define PUT_J(P,K,M,C)	do {						\
				ffpkyj(f_ounit,K,(long)(P)->M,C,status);\
			} while(0)
#define PUT_S(P,K,M,C)	do {						\
				(void)sprintf(sv.c,"0x%0.8x",(P)->M);	\
				ffpkys(f_ounit,K,sv.f,C,status);	\
			} while(0)
#define PUTLS(P,K,M,C)	do {						\
				if (*(P)->M == '\0' || *(P)->M == ' ')	\
					break;				\
				ffpkys(f_ounit,K,(P)->M,C,status);	\
			} while(0)
#define COM(C)		do {						\
				ffpcom(f_ounit,C,status);		\
			} while(0)
#define C2M(P,C)	do {						\
				(void)sprintf(rec,"%9.9s%-.71s",P,C);	\
				ffpcom(f_ounit,rec,status);		\
			} while(0)

/*
 *  Provide a complete description of response parameters.
 *  Called only when C_KEY is set...
 */
static void
rmf_verbiage(cpp, status)
	Point	*cpp;
	int	*status;
{
	static char	key[FITS_CLEN_KEYNAME],
			com[FITS_CLEN_COMMENT],
			rec[FITS_CLEN_CARD];
	static union	{ char c[FITS_CLEN_CHECKSUM];
			  char f[FITS_FLEN_CHECKSUM]; } sv;
	static int	ival;
	static Real	rval;

	COM("The ReQuest keywords may be used to exactly specify   ");
	COM("the matrix generated by SISRMG.  See help for details.");
	COM("Arguments and parameters supplied to sisrmg:       ");
	C2M( "model: ", cpp->id);
	C2M("infile: ", rmg_works.inp);
	C2M("arfile: ", rmg_works.arp);
	C2M("rmfile: ", rmg_works.rmp);

	rmf_header(cpp, status);

	PUT_J(&rmg_works, "rqpmin  ", phmin,
		"minimum intrinsic PH [ADU] in the response     ");
	PUT_J(&rmg_works, "rqpmax  ", phmax,
		"maximum intrinsic PH [ADU] in the response     ");
	PUT_S(&rmg_works, "rqmask  ", gmask,
		"mask of response features to include           ");
	PUT_S(&rmg_works, "rqchtr  ", chatr,
		"level of chattiness during execution           ");
	PUT_J(&rmg_works, "rqebin  ", e_bin,
		"number of energy bins in response domain       ");
	PUT_E(&rmg_works, "rqemin  ", e_min,
		"minimum input energy for response domain       ");
	PUT_E(&rmg_works, "rqemax  ", e_max,
		"maximum input energy for response domain       ");
	PUT_J(&rmg_works, "rqfrst  ", first,
		"conventional first output PH channel (0|1)     ");
	PUT_J(&rmg_works, "rqgans  ", gains,
		"EBOUNDS: <0=linear,0=p.w.linear,>0=smoothed    ");
	PUT_J(&rmg_works, "rqdeff  ", sisde,
		"1=Include/0=Exclude detector efficiency        ");
	PUT_J(&rmg_works, "rqdopi  ", sispi,
		"1=PI channel matrix 0=PH channel matrix        ");
	PUT_J(&rmg_works, "rqrdcv  ", rddcv,
		"Version of RDD correction s/w applied to data  ");
	PUT_J(&rmg_works, "rqdfez  ", dfezd,
		"FaintDFE style 0,1,2 == Old,New,Bright         ");
	PUTLS(&rmg_works, "rqfil0  ", f0,
		"Zeroth file -- arbitrary energy scale          ");
	PUTLS(&rmg_works, "rqfil1  ", f1,
		"1st file  -- branching ratio files             ");
	PUTLS(&rmg_works, "rqfil2  ", f2,
		"2nd file  -- high energy tail files            ");
	PUTLS(&rmg_works, "rqfil3  ", f3,
		"3rd file  -- global branching ratio file       ");
	PUTLS(&rmg_works, "rqdir0  ", dd,
		"Root directory of SIS calibration data files   ");
	PUTLS(&rmg_works, "rqgecd  ", gd,
		"ECD data file                                  ");
	PUTLS(&rmg_works, "rqphpi  ", ct,
		"PH to PI transformation calibration file       ");
	PUTLS(&rmg_works, "rqehis  ", eh,
		"Echo secular history file                      ");
	PUTLS(&rmg_works, "rqrddh  ", rd,
		"Residual Dark Distribution history file        ");
}

/*
 *  Dump out the point information to the RMF primary header.
 */
static void
rmf_header(cpp, status)
	Point	*cpp;
	int     *status;
{
	static char	key[FITS_CLEN_KEYNAME], com[FITS_CLEN_COMMENT];
	static int	ival;
	static Real	rval;

	PUT_E(cpp, "rqvers  ", vers,
		"version of calibration database                ");
	PUT_E(cpp, "rqepch  ", epch,
		"instrument epoch:  ASCA seconds                ");
	PUT_E(cpp, "rqevth  ", evth,
		"SIS event threshold                            ");
	PUT_E(cpp, "rqspth  ", spth,
		"SIS split threshold                            ");
	PUT_E(cpp, "rqxcen  ", xcen,
		"center column of the region of interest        ");
	PUT_E(cpp, "rqxwid  ", xwid,
		"width (in columns) about this center           ");
	PUT_E(cpp, "rqycen  ", ycen,
		"center CCD row of the region of interest       ");
	PUT_E(cpp, "rqywid  ", ywid,
		"width (in rows) about this center              ");
	PUT_E(cpp, "rqtemp  ", temp,
		"focal plane temperature (K - ignored)          ");
	PUT_E(cpp, "rqevpf  ", evpf,
		"events per frame (ignored)                     ");
	PUT_E(cpp, "rqimhi  ", imhi,
		"image clock hi (ignored)                       ");
	PUT_E(cpp, "rqimlo  ", imlo,
		"image clock lo (ignored)                       ");

	PUT_E(cpp, "rqecho  ", echo,
		"echo corruption of data                        ");
	PUT_E(cpp, "rqleak  ", leak,
		"average light leak in ADU                      ");
	PUT_E(cpp, "rqdark  ", dark,
		"nominal dark frame error in ADU                ");

	PUT_J(cpp, "rqdetr  ", detr,
		"detector # == 0 (SIS0), 1 (SIS1)               ");
	PUT_J(cpp, "rqchip  ", chip,
		"chip #     == 0, 1, 2, 3                       ");
	PUT_J(cpp, "rqmode  ", mode,
		"number of chips in use == 1, 2, 4              ");
}
#undef PUT_J
#undef PUT_E
#undef PUT_S
#undef COM
#undef C2M

/*
 *  Close the RMF file, and say goodbye to the human.
 REVISIT
 */
static void
rmf_finish(status)
	int	*status;
{
  ffclos(f_ounit, status);
}

#ifndef SISDEBUG
/*
 *  Line buffer the monolog.
 *  Not used in SISDEBUG mode 'cause we just spray stderr.
 */
void
errchk(m)
	char	*m;
{
	static char	mess[4*MAX_MESS+1], *p = mess;

	strcpy(p, m);
	p += strlen(m);

	if (strchr(m, '\n')) {	/* time to dump message */
		p[-1] = '\0';	/* since FC_ECHO has \n  */
		FC_ECHO(mess);
		memset(p = mess, 0, sizeof(mess));
	}
}
#endif  SISDEBUG

/*
 *  Take the string (src) presented by XPI/FITSIO, clean it up, and
 *  make a C usable copy (dst); it returns its first argument.
 *
 *  This may become unnecessary with cfortran.h?
 */
char *
string_fix(dst, src, len)
	char	*dst, *src;
	int	len;
{
	register char	*s;

	/* copy from src to dst */
	s = strchr(strncpy(dst, src, len), ' ');

	/* if ' ' was found, make that the end of the string */
	if (s) s[0] = '\0';

	return(dst);
}

/*
 *  Calls string_fix to clean up the string,
 *  and prepends the data directory file name.
 *
 *  Returns pointer to the entire string.
 */
char *
string_cat(dst, src, len)
	char	*dst, *src;
	int	len;
{
	(void)strncpy(dst, rmg_works.ddp, MAX_FNAME-1);
	(void)string_fix(dst + strlen(dst), src, len);
	return(dst);
}


/*
 *  Remove the quotes on FITS-returned strings.
 *  Doesn't handle multiline string values:  probably
 *  there is now a FITSIO call to handle this....
 */
static void
to_string(s, f)
	char	*s, *f;
{
	register char	*sx = s + MAX_FNAME;

	if (f[0] == '\'') f++;

	while (*f != '\'' && *f != ' ' && *f != '\0' && *f != '\\')
		*s++ = *f++;
	if (*f == '\\') s = sx - MAX_FNAME;

	while (s < sx) *s++ = ' ';
}

/*
 *  HACK ALERT
 *  this could be replaced by a cfortran call to fcpars in library/misc.for
 REVISIT
 */
static void
FFCPARS(f, n, e, status)
	char	*f, *n;
	int	*e, *status;
{
	while (!(*f == '+' || *f == '[' || *f == ' ' || *f == '\0'))
		*n++ = *f++;	/* copy file name */
	switch (*f) {
	case '[':
	case '+':	*e = atoi(f+1);		break;
	default :	*e = -99;		break;
	}
	*status = 0;
}

/*
 *  Replacement for missing library routine(s):
 *  Should migrate all the bzero calls to memset().
 */
/*#if defined(vms) || defined(solaris) || defined(IRAFONLY)
 * int
 * bzero(b, len)
 *        char    *b;
 *        int     len;
 *{
 *        register char   *z = b + len;
 *
 *        while (b < z) *b++ = 0;
 *        return(0);
 *}
 *
 *#endif
 */
/*
 *  HACK ALERT
 */


/*
 * collie -- Locate calves (ie CALibration Files), in CALDB or
 *    ftools/refdata/sisdata/ (or whatever rmg_works.dd points to)
 *    Jeff Guerber, HSTX/GSFC, Dec. 1997.
 */

static char *
collie( file, in, len, ccnm1, status )

     char *file;   /* returned file name */
     char *in;     /* input filename, or string "CALDB" */
     char *ccnm1;  /* codename: ECD, GAIN HIST, etc, exactly as in caldb */
     int  len;     /* for string_fix */
     int  *status; /* 0 if successful */

{
  /* gtcalf args... strings must be actual arrays for cfortran */
  char  scope[] = "ASCA",
    instr[] = "SIS ",
    det[] = "    ",    /*CCD0 etc, or `-'*/
    ccnm2[15],
    dash[] = "-",
    online[21],
    expr[100];
  int  gtchat, maxret=1, extno, nret, nfound, gtstatus;

  if (strcmp(in, "CALDB") == 0) {   /* should really be case-insens */

    /*
     * CALDB: call gtcalf to get the pathname of the file.  Currently we
     * don't use the extension information gtcalf returns.
     */

    strncpy(ccnm2,ccnm1,15); ccnm2[14] = '\0';    /* input might be a const */
    instr[3] = '0' + rqt.detr;                    /* SIS0 or SIS1 */
    strcpy(det,"-");
    strcpy(expr,"-");

    /* there are different ECD files for each sis/chip/split: */
    /* note: expr cannot contain any spaces, or gtcalf won't parse it right */
    if (strcmp(ccnm1, "ECD") == 0) {
      strcpy(det,"CCD "); det[3] = '0' + rqt.chip;
      sprintf(expr, "SPLIT.EQ.%d", ((rqt.spth > 30.) ? 40 : 20) );
    }

    /* gtcalf chatter: Major errors at 1, other errors at 5, reassurances
     * at 20-25 (some 10 or 15).  Printed if gtchat >= these levels.
     */
    gtchat = 9;
    if (rmg_works.chatr & C_CDB) gtchat = 30;

    extno = 0;  nret = 0;  nfound = 0;  gtstatus = 0;

    Gtcalf(gtchat, scope, instr, det, dash, ccnm2, dash, dash, dash, dash,
	   expr, maxret, file, &extno, online, &nret, &nfound, &gtstatus);

    ERRCHK(nfound<=0, "%s file not located in CALDB\n", ccnm1, *status=2);
    ERRCHK(gtstatus, "CALDB/GTCALF error %d\n", gtstatus, 0);
    ERRCHK(gtstatus, "     while looking for the %s calibration file\n",
	   ccnm1, *status=1);
    return(file);

  } else {

    /*
     * NOT CALDB, assume file is in refdata/sisdata/ (or wherever
     * rmg_works.dd points); the original behavior
     */

    return( string_cat(file, in, len) );
  }

}

/*
 *  End of sisrmg.c
 */
