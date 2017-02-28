/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/extras.c,v 3.7 1997/10/27 22:49:35 peachey Exp $   */
/*                   */

/*
 filename:	extras.c
 purpose:	do some additional science processing.
 author:	Geoffrey B. Crew
 date:		July 1993
 */
#include "defs.h"
#include "compat.h"

#ifdef SISEXTRA

#define BR_BINS	128
#define PHB(P)  ( ( ( (sqwork.hd.phabins == 2048) ? PH(P) : (P) ) \
							* br_bins) / 4096 )
#define CP_WIDTH 200

static int	rv_or_ka = 0, do_hista = 1, gr_shifts = 0, ev_which = 3,
		do_branch = 1, br_bins = BR_BINS, br_thre = 100;
static int	hista[4][2*CP_WIDTH], branch[4][8][BR_BINS];

# ifdef SISDEBUG
extern void	exit();

/*
 *  Parse additional arguments outside of HOST environment.
 *  On return should leave something host can deal with.
 */
void
parse_extras(ac, av)
	int	*ac;
	char	***av;
{
	char	*me = **av, **argv = *av;
	int	ok = 1;

	while (ok && --*ac && **++argv == '-')
		switch ((*argv)[1]) {
		case 'o':
			sqwork.op |= 1;
			break;
		case 'w':
			ev_which = atoi(*++argv);	--*ac;
			if (ev_which > 3 || ev_which < 0) ev_which = 0;
			if (ev_which > 0) sqwork.op = 0;
			break;
		case 'n':
			do_branch |= 1;
			br_bins = atoi(*++argv);	--*ac;
			if (br_bins > BR_BINS) br_bins = BR_BINS;
			break;
		case 't':
			do_branch |= 1;
			br_thre = atoi(*++argv);	--*ac;
			break;
		case 'b':
			do_branch |= 1;
			break;
		case 'h':
			do_hista |= 1;
			break;
		case 'r':
			rv_or_ka |= 1;
			break;
		case 'k':
			rv_or_ka |= 2;
			break;
		case '-':
			ok = 0;
			break;
		default:
			ERRCK2(1, "Valid hidden options are:\n", 0, 0);
			ERRCK2(1, "\t-o\t# one pass only\n", 0, 0);
			ERRCK2(1, "\t-w nn\t# for collection point:\n", 0, 0);
			ERRCK2(1, "\t\t# 0 first pass\n", 0, 0);
			ERRCK2(1, "\t\t# 1 all events\n", 0, 0);
			ERRCK2(1, "\t\t# 2 non-flicker events\n", 0, 0);
			ERRCK2(1, "\t\t# 3 good events only\n", 0, 0);
			ERRCK2(1, "\t-b\t# for branching ratios\n", 0, 0);
			ERRCK2(1, "\t-t nn\t# branching threshold (%d)\n",
				br_thre, 0);
			ERRCK2(1, "\t-n nn\t# number branching bins (%d)\n",
				br_bins, 0);
			ERRCK2(1, "\t-h\t# for neighor histograms\n", 0, 0);
			ERRCK2(1, "\t-r\t# for RV grading method\n", 0, 0);
			ERRCK2(1, "\t-k\t# for KA grading method\n", 0, 0);
			ERRCK2(1, "\t--\t# mandatory terminator\n", 0, 0);
			exit(1);
		}

	*argv = me;
	*av = argv;
}

/*
 *  Substitute for grading routine.  This is a cheap hook into
 *  lots of stuff, but it comes at the event grading point.
 */
void
ex_grading(ev)
	Event   *ev;
{
	short		sum, typ, abv, p[9];
	register int	j;

	switch (rv_or_ka) {
	case 0:
	case 1:
		do_grading(ev);				/* RV-grading */
		break;
	case 2:
		ka_grading(ev->s, &sum, &typ, &abv);	/* KA-grading */
		ev->p = sum; ev->g = typ;
		break;
	case 3:
		for (j = 0; j < 9; j++) p[j] = ev->s[j];
		do_grading(ev);				/* RV-grading */
		ka_grading(p, &sum, &typ, &abv);	/* KA-grading */
		if (sum != ev->p || typ != ev->g)
			gr_shifts++;
		break;
	}
}

# endif SISDEBUG

/*
 *  Accumulate branching ratios and corner pixels among real events.
 *  The flag ev_which indicates the collection point.
 */
void
ex_events(ev, wh)
	Event   *ev;
	int	wh;
{
	register int	j, k;

	if (wh != ev_which)
		return;

	if (do_hista) {
		/* j = sqwork.st; */
		/* if (j > 100) j = 100; */
		j = CP_WIDTH;
		if ((k = ev->s[1]) < j && k >= -j) hista[ev->c][k+CP_WIDTH]++;
		if ((k = ev->s[3]) < j && k >= -j) hista[ev->c][k+CP_WIDTH]++;
		if ((k = ev->s[6]) < j && k >= -j) hista[ev->c][k+CP_WIDTH]++;
		if ((k = ev->s[8]) < j && k >= -j) hista[ev->c][k+CP_WIDTH]++;
	}
	if (do_branch)
		branch[ev->c][ev->g][PHB(ev->p)]++;
}

/*
 *  Dump out products
 */
int
dump_extras()
{
	register int	j, k;
	int		l, evi;
	double		evs;
	static char	buf[MAX_FNAME];

	sqecho("\n** Extra Processing Output Follows **\n");

	if (rv_or_ka == 3) {
		(void)sprintf(buf, "%d grade/ph shifts\n", gr_shifts/2);
		sqecho(buf);
	}
	
	if (do_hista) {
		sqecho("\n** Corner Pixels Head **\n");
		/* k = -sqwork.st; */
		/* if (k > -40) k = -40; */
		k = -CP_WIDTH;
		/* for (j = k+100; k < sqwork.st; j++, k++) { */
		for (j = k+CP_WIDTH; k < CP_WIDTH; j++, k++) {
			(void)sprintf(buf, "%d\t%d\t%d\t%d\t%d\n", k,
				hista[0][j]/2, hista[1][j]/2,
				hista[2][j]/2, hista[3][j]/2);
			sqecho(buf);
		}
		sqecho("\n** Corner Pixels Tail **\n");
	}

	for (l = 0; do_branch && l < 4; l++) {
		(void)sprintf(buf,
		"\n** Branching Ratios: %d-ADU bins with >%d counts **\n",
			4096/br_bins, br_thre); sqecho(buf);
		(void)sprintf(buf, "\n** Branching Ratios Chip %d %s **\n",
			l, "Head"); sqecho(buf);

		for (j = 0, k = 0; j < br_bins; j++, k += 4096 / br_bins) {
			evi = branch[l][0][j] + branch[l][1][j]
			    + branch[l][2][j] + branch[l][3][j]
			    + branch[l][4][j] + branch[l][5][j]
			    + branch[l][6][j];
			if (evi < br_thre) continue;
			evs = (double)evi;
			(void)sprintf(buf,
	"%d\t%d\t%.5lf\t%.5lf\t%.5lf\t%.5lf\t%.5lf\t%.5lf\t%.5lf\n",
				k, (int)evs, branch[l][0][j]/evs,
				branch[l][1][j]/evs, branch[l][2][j]/evs,
				branch[l][3][j]/evs, branch[l][4][j]/evs,
				branch[l][5][j]/evs, branch[l][6][j]/evs);
			sqecho(buf);
		}
		(void)sprintf(buf, "** Branching Ratios Chip %d %s **\n",
			l, "Tail"); sqecho(buf);
	}
}

#endif SISEXTRA
