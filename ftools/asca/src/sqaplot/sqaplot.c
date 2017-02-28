/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/sqaplot.c,v 3.8 1997/10/27 22:49:39 peachey Exp $   */
/*                   */

/*
 filename:	sqaplot.c
 purpose:	main entry to the SIS response matrix generator
 author:	Geoffrey B. Crew
 date:		May 1993
 */
/******************************************************************************
Taskname:    sqaplot

Filename:    sqaplot.c

Description: main entry to the SIS response matrix generator

Author/Date: Geoffrey B. Crew, 5/93

Modification History:
    James Peachey, HSTX, 7/96 - changed to use native cfitsio library

Notes:
    Draft 1:
        - Replace macro syntax FFTXXXX with cfitsio ffxxxx
	- Replace macros FCECHO and FCPARS with the c_fcecho and c_fcpars
        - Add a global fitsfile* fun_cfits to replace fun, the LUN of infile
        - For I/O of the sqwork structure, use new structure element fitsfile
          *en_cfits instead of element int en (LUN)

Functions implemented in this file:
    int SQAPLT(); main control routine
    static int get_problem(int *status); 
    static void flix_thresh(char *b);
    static void make_name(char *f, char *n, char *e);
    void sqecho(char *m);
    void errck2(char *m);
    static int cleanup(int *status, int doplot);
    int bzero(char *p, unsigned int n);

******************************************************************************/

#include "defs.h"
#include "compat.h"

static int	get_problem(), cleanup();
static void	flix_thresh(), make_name();

Swork		sqwork;
Chips		chip_stats[4];

fitsfile	*fun_cfits = NULL;

/*
 *  Main control routine for the SIS Quick Analyis PLOT package.
 */
int
SQAPLT()
{
	int	status = 0, doplot;

	/*
	 *  Find out what and where...
	 */
	doplot = get_problem(&status);
	ERRCK2(status,"Problem with parameters %d\n",status,return(1));
	/*
	 *  Interpret the science data.
	 */
	get_science(&status);
	ERRCK2(status,"Problem with science file %d\n",status,return(2));
	/*
	 *  Hand off to do the various plots.
	 */
	if (doplot) do_picture();

	return(cleanup(&status, doplot));
}

/*
 *  Interface to all Q&A with user on problem definition.
 */
static int
get_problem(status)
	int	*status;
{
	static char	in[MAX_FNAME], dv[MAX_FNAME], ou[MAX_FNAME],
			ev[MAX_FNAME], es[MAX_FNAME], ct[MAX_FNAME],
			bf[MAX_FNAME], nam[MAX_FNAME], fx[MAX_FNAME];
	static int	blk, zero = 0, two = 2, doplot = 1,
			funit = F_IUNIT, *fun = &funit;

	/*
	 *  Trivial initializations.
	 */
	sqwork.version = VERSION; 
	sqwork.frfvers = FRFVERS;
	(void)strcpy(sqwork.date, SQADATE);
	sqwork.cs = chip_stats;
	/*
	 *  Get CL marching orders.
	 */
	FUCLGST("infile",     sqwork.in, status);
	FUCLGST("device",     sqwork.dv, status);
	FUCLGST("outfile",    sqwork.ou, status);
	FUCLGST("eventfile",  sqwork.ev, status);
	FUCLGST("flicker",    fx,        status);
	FUCLGSR("range",     &sqwork.rg, status);
	FUCLGSI("contours",  &sqwork.nc, status);
	FUCLGSI("smooth",    &sqwork.sm, status);
	FUCLGSI("split",     &sqwork.st, status);
	FUCLGST("echos",      bf,        status);
	FUCLGSI("style",     &sqwork.sy, status);
	FUCLGSI("maxgrade",  &sqwork.gm, status);
	FUCLGSR("pichip",    &sqwork.pi, status);
	FUCLGSB("telemetry", &sqwork.ts, status);
	FUCLGSI("maxcounts", &sqwork.mx, status);
	FUCLGSB("rawdisplay",&sqwork.rd, status);
	FUCLGST("echohist",   sqwork.es, status);
	FUCLGST("phtopi",     sqwork.ct, status);
        /*
         *  Truncated strings for error messsages; trivial checking.
         */
	make_name(sqwork.in, sqwork.ou, "out");
	make_name(sqwork.in, sqwork.ev, "evt");
        strchr(strncpy(sqwork.inp = in, sqwork.in, MAX_FNAME), ' ')[0] = '\0';
        strchr(strncpy(sqwork.dvp = dv, sqwork.dv, MAX_FNAME), ' ')[0] = '\0';
        strchr(strncpy(sqwork.oup = ou, sqwork.ou, MAX_FNAME), ' ')[0] = '\0';
        strchr(strncpy(sqwork.evp = ev, sqwork.ev, MAX_FNAME), ' ')[0] = '\0';
        strchr(strncpy(sqwork.esp = es, sqwork.es, MAX_FNAME), ' ')[0] = '\0';
        strchr(strncpy(sqwork.ctp = ct, sqwork.ct, MAX_FNAME), ' ')[0] = '\0';
	ERRCK2(in[0] == '@',"Cannot process %s, try a single file.\n",
		in, *status=10000);
	ERRCK2(dv[0] != '/',"Device name %s looks invalid.\n",
		dv, *status=10001);
	ERRCK2(*status,"Problem %d accessing CL\n",*status,return(doplot));
	(void)sscanf(bf, "%g,%g", sqwork.ek, sqwork.ek+1);
	/*
	 *  Setup for output file.
	 */
	if (strcmp(sqwork.oup, "STDOUT")) {
		if (strcmp(sqwork.oup, "NONE"     ) &&
		    strcmp(sqwork.oup, " "        ) &&
		    strcmp(sqwork.oup, "/dev/null"))
			sqwork.fd = open(sqwork.oup,
				O_WRONLY | O_CREAT | O_TRUNC, 0666);
		else
			sqwork.fd = -1;
	} else {
		sqwork.fd = 0;
	}
	/*
	 *  Setup for events file.
	 */
	if (strcmp(sqwork.evp, "NONE"     ) &&
	    strcmp(sqwork.oup, " "        ) &&
	    strcmp(sqwork.oup, "/dev/null"))
		sqwork.fe = F_OUNIT;
	else
		sqwork.fe = 0;
	/*
	 *  Start writing...
	 */
	(void)sprintf(bf, "** SQAPLOT ver %3.1f (%s) **\n",
		sqwork.version, sqwork.date);		sqecho(bf);
	(void)sprintf(bf, "This version is consistent with FRFread %5.3f\n",
		sqwork.frfvers);			sqecho(bf);
	(void)sprintf(bf, "SIS Science File Name:\n");	sqecho(bf);
	(void)sprintf(bf, "%s\n", sqwork.inp);		sqecho(bf);
	/*
	 *  Be anal about zeroing statistics.
	 */
	(void)bzero((char *)sqwork.cs, 4 * sizeof(Chips));
	flix_thresh(fx);
	/*
	 *  Start on the plot unless it should be skipped.
	 */
	if (!strncmp("/null", sqwork.dv, 5) || !strncmp("/NULL", sqwork.dv, 5))
		doplot = 0;
	if (doplot) PGBEGIN(&zero, sqwork.dv, &two, &two);
	/*
	 *  Open the Science file, crude FCPARS behavior for now.
	 */
/*	FFCPARS(sqwork.in, nam, &sqwork.ex, status);*/
	c_fcpars(sqwork.in, nam, &sqwork.ex, status);
	ERRCK2(++sqwork.ex<=0,"Using first bintable of %s by default.\n",
		sqwork.inp, sqwork.ex=2);
/*	FFTOPEN(fun, nam, &zero, &blk, status);*/
	ffopen(&fun_cfits, nam, READONLY, status);
	ERRCK2(*status, "Unable to open Science File %s\n", sqwork.inp, 0);
	/*
	 *  Open the Events file.
	 */
/*	if (sqwork.fe)
		FFTINIT(&sqwork.fe, sqwork.ev, &blk, status);*/
	if (sqwork.fe)
		ffinit(&sqwork.fe_cfits, sqwork.ev, status);
	ERRCK2(*status, "Unable to create Events file %s\n", sqwork.evp, 0);

	return(doplot);
}

/*
 *  Parse the flicker threshold input information.
 */
static void
flix_thresh(b)
	char	*b;
{
	register int	c;
	register char	*be = b + MAX_FNAME - 1;

	for (c = 0, *be = '\0'; c < 4 && b < be; c++) {
		while (!isdigit(*b)) b++;
		sqwork.cs[c].ask = atoi(b);
		while ( isdigit(*b)) b++;
	}
}

/*
 *  Generate DEFAULT output file names using the input
 *  science file name:  if name is of the form [^.]*.fits,
 *  then convert it to something of the form [^.]*.ext.
 *
 *  These are fortran ' ' delimited strings.
 */
static void
make_name(f, n, e)
	char	*f, *n, *e;
{
	register char	*name = n, *fits = f;

	if (strncmp(n, "DEFAULT", 7)) return;

	while (*f != ' ' && *f) *n++ = *f++;	/* copy fname */
	f -= 5;	n -= 4;

	if (!strncmp(f++, ".fits", 5)) {	/* append ext */
		while (*e) *n++ = *e++;
		while (n - name < MAX_FNAME) *n++ = ' ';
	} else {
		ERRCK2(1,"Infile %s doesn't end in .fits; no DEFAULT name.\n",
			fits, strcpy(name, "NONE"));
	}
}

/*
 *  Output routine for all seasons.
 */
void
sqecho(m)
	char	*m;
{
	static char	mess[800], *p = mess;

	if (sqwork.fd > 0) {		/* write it to file	*/
		write(sqwork.fd, m, strlen(m));
	} else if (sqwork.fd == 0 ) {	/* buffer it for FCECHO	*/
		(void)strcpy(p, m);
		p += strlen(m);

		if (strchr(m, '\n')) {	/* time to dump message */
			p[-1] = '\0';	/* since FCECHO has \n  */
/*			FCECHO(mess);*/
			c_fcecho(mess);
			p = mess;
		}
	}				/* else skip it */
}

#ifndef SISDEBUG
void
errck2(m)
	char	*m;
{
	static char	mess[800], *p = mess;

	(void)strcpy(p, m);
	p += strlen(m);

	if (strchr(m, '\n')) {	/* time to dump message */
		p[-1] = '\0';	/* since FCECHO has \n  */
/*		FCECHO(mess);*/
		c_fcecho(mess);
		p = mess;
	}
}
#endif  SISDEBUG

static int
cleanup(status, doplot)
	int	*status, doplot;
{
	static int	funit = F_IUNIT, *fun = &funit;

/*	FFTCLOS(fun, status);				/* close infile	*/
	ffclos(fun_cfits, status);			/* close infile	*/
	fun_cfits = NULL;
	if (sqwork.fd) close(sqwork.fd);		/* close output	*/
	if (doplot) PGEND();				/* close device	*/
/*	if (sqwork.fe) FFTCLOS(&sqwork.fe, status);	/* close output	*/
	if (sqwork.fe_cfits){
		ffclos(sqwork.fe_cfits, status);		/* close output	*/
		sqwork.fe_cfits = NULL;
	}
	return(*status);
}

#if defined(vax) || defined(solaris) && !defined(__hpux)
/*
 *  Insert primitive bzero() routine
 */
int
bzero(p, n)
	char		*p;
	unsigned int	n;
{
	while (n--) *p++ = 0;
}
#endif vax
