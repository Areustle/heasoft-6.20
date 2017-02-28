/*
 SimASTE_XRSRMFsim.c
   SimaSTE module : XRS simulation with RMF

  1998-02-25	version 1.00	Y.ISHISAKI

  1998-04-14	version 1.01	Y.ISHISAKI
	bug fix in SimASTE_XRSRMFsim_com (com -> key)
	SimASTE_DRNDTS -> aste_drndts

  1998-09-10	version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	add Be filter selection for FW

  1999-01-22	version 1.20	Y.ISHISAKI
	add event flag (HiRes, MidRes, LowRes, Secondary)
	put SimASTE:SKYX/Y/ROLL
	bug fix in show_parameter() for com.pxl_sel

  1999-02-09	version 1.21	Y.ISHISAKI
	check if (total == 0) in pixel statistics
	print all statistics

  2003-09-13	version 1.30	Y.ISHISAKI
	use latest aste_coord in astetool-1.24

  2003-11-07	version 1.40	Y.ISHISAKI
	BnkPut PI:TLMIN/TLMAX, PHA:TLMIN/TLMAX

  2003-11-10	version 1.50	Y.ISHISAKI
	read fw_file & gv_file

  2004-07-06	version 1.60	Y.ISHISAKI
	quick fix for CAL pixel (No.3) & non-connected pixel (No.2)
	count statistics on H, H+Mp, Ms, Lp+Ls
	print statistics with 2D-array map & pixel order

  2006-04-09 version 1.7	Y.ISHISAKI
	increase rmffile[256] -> [1024], trans_file[256] -> [1024]

  2006-07-24 version 2.0	Y.ISHISAKI
	use anl_msg_xxx() functions

  2006-08-02 version 2.1	Y.ISHISAKI
	add aberration parameter

  2006-08-05 version 2.2	Y.ISHISAKI
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	new BNK, SimASTE:XRS:RMF_FILE:PTR,
	new BNK, SimASTE:XRS:FW_FILE:PTR, SimASTE:XRS:GV_FILE:PTR

  2006-08-22 version 2.3	Y.ISHISAKI
	check HrndmRMF_init() error status in _init()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "com.h"
#include "cli.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "SimASTE.h"

#define TRANSMISSION_INDEX_SEARCH	/* fast index search for transmission */

static char pname[] = "SimASTE_XRSRMFsim";
char SimASTE_XRSRMFsim_version[] = "version 2.3";

/* XRSRMFsim parameters */
static struct {
	int mode;
	int xrs_efficiency;
	int xrs_pixel_select;
	char rmffile[PIL_LINESIZE];
	struct HrndmRMF *hmf;
	/* char filter_name[FLEN_VALUE]; */
	int cdpClockHz, pulseLength, shortPulseLength, pulseOverLength;
	double dtLowMidThres, dtMidHiThres, dtSecondaryThres;
	struct transmission {
		char trans_file[PIL_LINESIZE];
		int nrow;
		double *energy, *trans;
#ifdef TRANSMISSION_INDEX_SEARCH
		struct trans_index {
			double norm, offs;
			int nbody;		/* in fact, sizeof(body)-1 */
			int *body;		/* 0-nbody */
		} index;
#endif
	} fw, gv;
	int aberration;
	int mjdrefi;
	double mjdreff;
	int (*prev_write_history)(fitsfile *);
} com = {
	-1,						/* mode (DEFAULT) */
	1,						/* eff (YES) */
	1,						/* spec (YES) */
	"astroe_xrs.rmf",		/* rmffile */
	NULL,					/* hmf */
	/* "none", */			/* filter_name */
	12288, 1757, 446, 0,	/* cdpClockHz, pulseLength,
							   shortPulseLength, pulseOverLength */
	0.0, 0.0, 0.0,			/* dtLowMidThres, dtMidHiThres, dtSecondaryThres */
	{
		"none",				/* fw.trans_file */
		0,					/* fw.nrow */
		NULL, NULL			/* fw.energy, fw.trans */
#ifdef TRANSMISSION_INDEX_SEARCH
		, {
			0.0, 0.0,		/* trans.index.norm, trans.index.offs */
			10000, NULL		/* trans.index.nbody, trans.index.body */
		}
#endif
	}, {
		"none",				/* gv.trans_file */
		0,					/* gv.nrow */
		NULL, NULL			/* gv.energy, gv.trans */
#ifdef TRANSMISSION_INDEX_SEARCH
		, {
			0.0, 0.0,		/* trans.index.norm, trans.index.offs */
			10000, NULL		/* trans.index.nbody, trans.index.body */
		}
#endif
	},
	ANL_YES,				/* aberration */
	0,						/* mjdrefi */
	0.0,					/* mjdreff */
	NULL					/* prev_write_history */
};

static struct {
	long total, H, H_Mp, Ms, Lp_Ls;
} statistics[32], statistics_all;

static int
read_trans_file(struct transmission *p)
{
	int status, hdutype;
	fitsfile *fp;

	status = 0;
	fits_open_file(&fp, p->trans_file, READONLY, &status);
	if ( status ) {
		anl_msg_error("\
%s: XRS FW/GV transmission '%s' open failed\n", pname, p->trans_file);
		return -1;
	}

	for (;;) {
#ifdef TRANSMISSION_INDEX_SEARCH
		int i, j, k, nbody, *body;
		double offs, norm;
#endif
		int ne, col_energy, col_trans, anynul;
		char comment[80];
		int exact = 1;

		status = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &status);
		if ( status ) {
			fits_close_file(fp, &status);
			anl_msg_error("\
%s: no transmission data in '%s'\n", pname, p->trans_file);
			return -1;
		}
		fits_read_key(fp, TINT, "NAXIS2", &ne, comment, &status);
		ffgcno(fp, exact, "Energy", &col_energy, &status);
		ffgcno(fp, exact, "Transmission", &col_trans, &status);
		p->energy = malloc(2 * sizeof(double) * ne
#ifdef TRANSMISSION_INDEX_SEARCH
				+ sizeof(*p->index.body) * (p->index.nbody + 1)
#endif
						   );
		p->trans = p->energy + ne;
		if ( NULL == p->energy ) {
			anl_msg_error("\
%s: Energy/Transmission malloc failed for '%s'\n", pname, p->trans_file);
			return -1;
		}
		ffgcvd(fp, col_energy, 1, 1, ne, 0.0, p->energy, &anynul, &status);
		ffgcvd(fp, col_trans,  1, 1, ne, 0.0, p->trans,  &anynul, &status);
		fits_close_file(fp, &status);
		if ( status ) {
			anl_msg_error("\
%s: Energy/Transmission column read error for '%s'\n", pname, p->trans_file);
			return -1;
		}

#ifdef TRANSMISSION_INDEX_SEARCH
		nbody = p->index.nbody;
		body = p->index.body = (int *)( &p->trans[ne] );
		offs = p->index.offs = p->energy[0];
		norm = p->index.norm = p->energy[ne-1];
		for (i = j = 0; i < ne - 1; i++) {
			k = nbody * ( p->energy[i] - offs ) / norm;
			while ( j <= k ) body[j++] = i;
		}
		while ( j < nbody+1 ) body[j++] = i;
#endif

		return ne;
	}
}

static double
transmission_at(double energy, struct transmission *p)
{
	double trans, x0, x1, y0, y1;

#ifdef TRANSMISSION_INDEX_SEARCH
	int ipos, ie, ne;
	struct trans_index *idx = &p->index;

	ipos = idx->nbody * ( energy - idx->offs ) / idx->norm;
	if ( ipos < 0 || idx->nbody <= ipos ) return 0.0;	/* out of bounds */
	ne = p->nrow - 1;

	for (ie = idx->body[ipos]; ie < ne; ie++) {
		if ( energy < p->energy[ie] ) break;
	}

	if ( ie <= 0 ) return 0.0;

	x0 = p->energy[ie-1];
	x1 = p->energy[ie];
	y0 = p->trans[ie-1];
	y1 = p->trans[ie];

	trans = (y0 * (x1 - energy) + y1 * (energy - x0)) / (x1 - x0);
/*
	if ( energy < x0 || x1 < energy ) {
		printf("transmission_at: index search failed !!!\n");
	}
	printf("\
ie=%d, ipos=%d, nsearch=%d, x0=%f, energy=%f, x1=%f\n",
		   ie, ipos, ie - idx->body[ipos], x0, energy, x1);
*/

#else

	int i, n;

	if ( energy < p->energy[0] ) {
		return 0.0;
	}

	trans = 0.0;
	n = p->nrow - 1;
	for (i = 0; i < n; i++) {
		if ( energy <= p->energy[i+1] ) {
			x0 = p->energy[i];
			x1 = p->energy[i+1];
			y0 = p->trans[i];
			y1 = p->trans[i+1];
			trans = (y0 * (x1 - energy) + y1 * (energy - x0)) / (x1 - x0);
			break;
		}
	}

#endif

	return trans;
}

static int
write_history(fitsfile *fp)
{
	int istat;
	char history[PIL_LINESIZE + FLEN_VALUE];

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	if ( com.aberration ) {
		sprintf(history, "\
  aberration=yes  mjdrefi=%d  mjdreff=%.17f", com.mjdrefi, com.mjdreff);
	} else {
		sprintf(history, "\
  aberration=no");
	}
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  xrs_rmffile='%s'", com.rmffile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  fw_file='%s'", com.fw.trans_file);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  gv_file='%s'", com.gv.trans_file);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  xrs_efficiency=%s  xrs_pixel_select=%s",
		com.xrs_efficiency  ? "yes" : "no",
		com.xrs_pixel_select ? "yes" : "no");
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static void
MSG(char *format, ...)
{
	FILE *fp = stdout;
	va_list args;
	va_start(args, format);
	if ( '!' == *format ) {
		vfprintf(fp, format+1, args);
	} else {
		vfprintf(fp, format, args);
		fputc('\n', fp);
	}
	va_end(args);
	if ( isatty(fileno(fp)) ) fflush(fp);
}

static void
show_yes_no(char *msg, int sw)
{
	MSG("%4s%-20s%s", "", msg, sw ? "YES" : "NO");
}

static void
show_parameter(char *title)
{
	char *p;

	MSG("");
	MSG(title, pname);
	MSG("");

	switch ( com.mode ) {
	case SimASTE_Mode_Discard: p = "Discard_Mode"; break;
	case SimASTE_Mode_Weight: p = "Weighting_Mode"; break;
	default: p = "Default";
	}

	MSG("%4s%-20s'%s'", "", "XRS_RMFFILE", com.rmffile);
	MSG("%4s%-20s'%s'", "", "FW_FILE", com.fw.trans_file);
	MSG("%4s%-20s'%s'", "", "GV_FILE", com.gv.trans_file);
	MSG("%4s%-20s%s", "", "SIMULATION_MODE", p);
	show_yes_no("ABERRATION", com.aberration);
	show_yes_no("XRS_EFFICIENCY", com.xrs_efficiency);
	show_yes_no("XRS_PIXEL_SELECT", com.xrs_pixel_select);
}

static void
show_statistics(char *title)
{
	int i;
	long total;
	double frac;
	char percent_H[16], percent_H_Mp[16], percent_Ms[16], percent_Lp_Ls[16];

	MSG("");
	MSG(title, pname);
	MSG("");

	MSG("\
  DETY\n\
   ^\n\
   |\n\
   +---+---+---+---+---+---+\n\
   |   | 13| 15|  6|  4|   |\n\
   +---+---+---+---+---+---+\n\
   | 11| 12| 14|  5|   |   |\n\
   +---+---+---+---+---+---+\n\
   |  9| 10|  8|  7|  1|  0|\n\
   +---+---+---+---+---+---+\n\
   | 16| 17| 23| 24| 26| 25|\n\
   +---+---+---+---+---+---+\n\
   | 18| 19| 21| 30| 28| 27|\n\
   +---+---+---+---+---+---+\n\
   |   | 20| 22| 31| 29|   |\n\
   +---+---+---+---+---+---+--> DETX\n\
\n\
==============================================================================\n\
Pixel    Total        H         (H + Mp)             Ms        (Lp + Ls)\n\
------------------------------------------------------------------------------\
");
/*
  0   10000000 10000000(99.0%) 10000000(99.0%) 10000000(99.0%) 10000000(99.0%)
  1   10000000 10000000(99.0%) 10000000(99.0%) 10000000(99.0%) 10000000(99.0%)

 all  10000000 10000000(99.0%) 10000000(99.0%) 10000000(99.0%) 10000000(99.0%)
*/
	for (i = 0; i < 32; i++) {
		int pixel = i;

		total = statistics[pixel].total;
		frac = (0 == total) ? 0.0 : (100.0/total);
		sprintf(percent_H,    "%f", frac*statistics[pixel].H);
		sprintf(percent_H_Mp, "%f", frac*statistics[pixel].H_Mp);
		sprintf(percent_Ms,   "%f", frac*statistics[pixel].Ms);
		sprintf(percent_Lp_Ls,"%f", frac*statistics[pixel].Lp_Ls);
		MSG(" %2d  %9ld%9ld(%4.4s%%)%9d(%4.4s%%)%9d(%4.4s%%)%9d(%4.4s%%)",
			pixel,
			statistics[pixel].total,
			statistics[pixel].H, percent_H,
			statistics[pixel].H_Mp, percent_H_Mp,
			statistics[pixel].Ms, percent_Ms,
			statistics[pixel].Lp_Ls, percent_Lp_Ls
			);
		statistics_all.total += statistics[pixel].total;
		statistics_all.H += statistics[pixel].H;
		statistics_all.H_Mp += statistics[pixel].H_Mp;
		statistics_all.Ms += statistics[pixel].Ms;
		statistics_all.Lp_Ls += statistics[pixel].Lp_Ls;
	}

	total = statistics_all.total;
	frac = (0 == total) ? 0.0 : (100.0/total);
	sprintf(percent_H,    "%f", frac*statistics_all.H);
	sprintf(percent_H_Mp, "%f", frac*statistics_all.H_Mp);
	sprintf(percent_Ms,   "%f", frac*statistics_all.Ms);
	sprintf(percent_Lp_Ls,"%f", frac*statistics_all.Lp_Ls);
	MSG("\
------------------------------------------------------------------------------\n\
 all%10ld%9ld(%4.4s%%)%9d(%4.4s%%)%9d(%4.4s%%)%9d(%4.4s%%)\n\
------------------------------------------------------------------------------\n\
",
		statistics_all.total,
		statistics_all.H, percent_H,
		statistics_all.H_Mp, percent_H_Mp,
		statistics_all.Ms, percent_Ms,
		statistics_all.Lp_Ls, percent_Lp_Ls
		);
}

void
SimASTE_XRSRMFsim_startup(int *status)
{
	com.mjdrefi = aste_mjdrefi();
	com.mjdreff = aste_mjdreff();
}

void
SimASTE_XRSRMFsim_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"RMF_FILE",
		"SIMULATION_MODE",
		"ABERRATION",
		"EFFICIENCY",
		"PIXEL_SELECT",
		"FW_FILE",
		"GV_FILE",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"RMF utilized for XRS simulation",
		"Set Simulation Mode (Default/Discard/Weight)",
		"Correct for aberration or not",
		"Multiply XRS effciency or not",
		"Discard events which is fallen outside of pixels",
		"Filter Wheel transmission file",
		"Gate Valve transmission file",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	char *k;

	if ( *status ) {	/* ftools */
		if ( PILGetBool (k="aberration", &com.aberration) ||
			 PILGetFname(k="xrs_rmffile", com.rmffile) ||
			 PILGetFname(k="fw_file", com.fw.trans_file) ||
			 PILGetFname(k="gv_file", com.gv.trans_file) ||
			 PILGetBool (k="xrs_efficiency", &com.xrs_efficiency) ||
			 PILGetBool (k="xrs_pixel_select", &com.xrs_pixel_select) ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("RMF_FILE", key) ) {
			CLtxtrd(key, com.rmffile, sizeof(com.rmffile));
		} else if ( 0 == strcmp("SIMULATION_MODE", key) ) {
			static char *keytbl[] = {
				"Discard_Mode", "Weighting_Mode", "Default"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static char key[32] = "Default";
			int it;

			CLkeyrd(-1, "SimASTE_XRSRMFsim Simulation Mode",
					key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("Discard_Mode", key) ) {
				com.mode = SimASTE_Mode_Discard;
			} else if ( 0 == strcmp("Weighting_Mode", key) ) {
				com.mode = SimASTE_Mode_Weight;
			} else if ( 0 == strcmp("Default", key) ) {
				com.mode = -1;
			}
		} else if ( 0 == strcmp("ABERRATION", key) ) {
			CLlogrd("Correct for aberration", &com.aberration);
		} else if ( 0 == strcmp("EFFICIENCY", key) ) {
			CLlogrd("Multiply XRS efficiency", &com.xrs_efficiency);
		} else if ( 0 == strcmp("PIXEL_SELECT", key) ) {
			CLlogrd("Discard events fallen outside of pixels",
				&com.xrs_pixel_select);
		} else if ( 0 == strcmp("FW_FILE", key) ) {
			CLtxtrd(key, com.fw.trans_file, sizeof(com.fw.trans_file));
		} else if ( 0 == strcmp("GV_FILE", key) ) {
			CLtxtrd(key, com.gv.trans_file, sizeof(com.gv.trans_file));
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_XRSRMFsim_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;

	char *filename;
	int ival, used;

	EvsDef("SimASTE_XRSRMFsim:BEGIN");
	EvsDef("SimASTE_XRSRMFsim:ENTRY");
	EvsDef("SimASTE_XRSRMFsim:OK");

	BnkDef("XRSsim:EVENT_FLAGS", 8);	  /* tentative event flag */
	BnkDef("XRSsim:PREV_EVENT_FLAGS", 8); /* real event flag for prev event */
	BnkDef("SimASTE:XRS:RMF_FILE:PTR", sizeof(char *));
	BnkDef("SimASTE:XRS:FW_FILE:PTR", sizeof(char *));
	BnkDef("SimASTE:XRS:GV_FILE:PTR", sizeof(char *));

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	com.dtLowMidThres = (double)com.shortPulseLength / com.cdpClockHz;
	com.dtMidHiThres = (double)com.pulseLength / com.cdpClockHz;
	com.dtSecondaryThres = (double)(com.pulseLength + com.pulseOverLength)
															/ com.cdpClockHz;

	show_parameter("%s:  *** show parameter ***");
	MSG("");

	if ( com.mode < 0 ) {
		int size;
		BnkfGetM("SimASTE:MODE", sizeof(com.mode), &size, &com.mode);
	}

	if ( 0 != CLstricmp("NONE", com.fw.trans_file) ) {
		anl_msg_info("\
%s: reading FW file '%s'\n", pname, com.fw.trans_file);
		com.fw.nrow = read_trans_file(&com.fw);
		if ( com.fw.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	if ( 0 != CLstricmp("NONE", com.gv.trans_file) ) {
		anl_msg_info("\
%s: reading GV file '%s'\n", pname, com.gv.trans_file);
		com.gv.nrow = read_trans_file(&com.gv);
		if ( com.gv.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	anl_msg_info("\
%s: reading RMF file ... ", pname);
	com.hmf = HrndmRMF_init(com.rmffile);
	if ( NULL == com.hmf ) {
		anl_msg_error("\
%s: HrndmRMF_init() failed\n", pname);
		*status = ANL_QUIT;
		return;
	}
	anl_msg_info("\
finished\n");

	ival = com.hmf->ebounds.channel[0];
	BnkPut("SimASTE:PHA:TLMIN", sizeof(ival), &ival);
	BnkPut("SimASTE:PI:TLMIN", sizeof(ival), &ival);
	ival = com.hmf->ebounds.channel[0] + com.hmf->detchans - 1;
	BnkPut("SimASTE:PHA:TLMAX", sizeof(ival), &ival);
	BnkPut("SimASTE:PI:TLMAX", sizeof(ival), &ival);

	filename = com.rmffile;
	BnkPut("SimASTE:XRS:RMF_FILE:PTR", sizeof(filename), &filename);
	filename = com.fw.trans_file;
	BnkPut("SimASTE:XRS:FW_FILE:PTR", sizeof(filename), &filename);
	filename = com.gv.trans_file;
	BnkPut("SimASTE:XRS:GV_FILE:PTR", sizeof(filename), &filename);

	*status = ANL_OK;
}

void
SimASTE_XRSRMFsim_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRSRMFsim_bgnrun(int *status)
{
	EvsSet("SimASTE_XRSRMFsim:BEGIN");
	*status = ANL_OK;
}

static char *
judgeEventFlags(double tb, double ta, char *event_flags)
{
	char *p = event_flags;

	if ( tb < com.dtLowMidThres || ta < com.dtLowMidThres ) {
		*p++ = 'L';		/* lowRes */
	} else if ( tb < com.dtMidHiThres || ta < com.dtMidHiThres ) {
		*p++ = 'M';		/* midRes */
	}
	if ( tb < com.dtSecondaryThres ) {
		*p++ = 'S';		/* secondary */
	}
	if ( p == event_flags ) {
		*p++ = 'H';		/* hiRes */
	}
	*p = '\0';

	return event_flags;
}

void
SimASTE_XRSRMFsim_ana(int nevent, int eventid, int *status)
{
	static int set_tentative_event[32];
	static double t0[32] = {	/* arrival time of previous event */
	/* initialize with reasonably long time ago */
		-1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10,
		-1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10,
		-1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10,
		-1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10, -1e10
	};
	static double tb[32] = {	/* time interval from previous event */
		1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
		1.0, 1.0, 1.0, 1.0,	1.0, 1.0, 1.0, 1.0,
		1.0, 1.0, 1.0, 1.0,	1.0, 1.0, 1.0, 1.0,
		1.0, 1.0, 1.0, 1.0,	1.0, 1.0, 1.0, 1.0
	};
	static double ta[32];		/* time interval to next event */
	static char tentative_event_flags[32][8];

	int size, pixel;
	double energy, photon_time, detx, dety, skyx, skyy, roll, ra, de;
	double pi, eff, weight;
	int pi_ch, detx_ch, dety_ch, skyx_ch, skyy_ch;
	AtEulerAng euler;
	SKYREF skyref, tmp_skyref;
	char event_flags[8];	/* MLS */
	TELDEF *teldef;

	EvsfSetM("SimASTE_XRSRMFsim:ENTRY");
	*status = ANL_OK;

	BnkfGetM("SimASTE:TELDEF", sizeof(teldef), &size, &teldef);

/* get photon information */
	BnkfGetM("SimASTE:PHOTON_ENERGY", sizeof(energy), &size, &energy);
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &size, &photon_time);
	BnkfGetM("SimASTE:XRSX", sizeof(detx), &size, &detx);
	BnkfGetM("SimASTE:XRSY", sizeof(dety), &size, &dety);
	BnkfGetM("SimASTE:EULER", sizeof(euler), &size, &euler);
	BnkfGetM("SimASTE:SKYREF", sizeof(skyref), &size, &skyref);

/* determine hitted pixel */
	xrs_det2pixel(teldef, detx, dety, &pixel);
/* quick fix for CAL pixel (No.3) & non-connected pixel (No.2) */
	if ( 2 == pixel || 3 == pixel ) pixel = -1;

	if ( com.xrs_pixel_select && pixel < 0 ) {
		*status = ANL_SKIP;
		return;
	}

/* set DETX/Y, X/Y */
	detx_ch = (int)(detx + 10000.5) - 10000;
	dety_ch = (int)(dety + 10000.5) - 10000;

	if ( com.aberration ) {
		aste_det2ecs(teldef, &euler, detx, dety, &ra, &de);
		aste_cor_aberration(photon_time, com.mjdrefi, com.mjdreff, &ra, &de);
		aste_ecs2sky(teldef, &skyref, ra, de, &skyx, &skyy);
	} else {
		aste_det2sky(teldef, &euler, &skyref, detx, dety, &skyx, &skyy);
	}
	skyx_ch = (int)(skyx + 10000.5) - 10000;
	skyy_ch = (int)(skyy + 10000.5) - 10000;
	aste_euler2skyref(teldef, &euler, &tmp_skyref);
	roll = tmp_skyref.roll;

/* randomize PI */
	HrndmRMF(energy, aste_drndts(), com.hmf, &pi, &eff);
	pi_ch = (int)pi + com.hmf->ebounds.channel[0];

/* consider XRS efficiency */
	if ( com.xrs_efficiency ) {
		struct transmission *p;

		p = &com.fw;
		if ( p->nrow ) {
			eff *= transmission_at(energy, p);
		}

		p = &com.gv;
		if ( p->nrow ) {
			eff *= transmission_at(energy, p);
		}

		if ( SimASTE_Mode_Discard == com.mode ) {
			if ( eff < aste_drndts() ) {
				*status = ANL_SKIP;
				return;
			}
		} else {
			BnkfGetM("SimASTE:WEIGHT", sizeof(weight), &size, &weight);
			weight *= eff;
			BnkfPutM("SimASTE:WEIGHT", sizeof(weight), &weight);
		}
	}

/* set event flags */
	if ( 0 <= pixel && pixel < 32 ) {
		char *fs;

		ta[pixel] = photon_time - t0[pixel];
		judgeEventFlags(tb[pixel], ta[pixel], event_flags);
		BnkfPutM("XRSsim:PREV_EVENT_FLAGS", strlen(event_flags), event_flags);

		if ( set_tentative_event[pixel] ) {

			fs = tentative_event_flags[pixel];
			if ( 'H' == fs[0] ) {
				statistics[pixel].H--;			/* H */
				statistics[pixel].H_Mp--;
			} else if ( 'M' == fs[0] ) {
				if ( 'S' != fs[1] ) {
					statistics[pixel].H_Mp--;	/* Mp */
				} else {
					statistics[pixel].Ms--;		/* Ms */
				}
			} else {
				statistics[pixel].Lp_Ls--;		/* Lp or Ls */
			}

			fs = event_flags;
			if ( 'H' == fs[0] ) {
				statistics[pixel].H++;			/* H */
				statistics[pixel].H_Mp++;
			} else if ( 'M' == fs[0] ) {
				if ( 'S' != fs[1] ) {
					statistics[pixel].H_Mp++;	/* Mp */
				} else {
					statistics[pixel].Ms++;		/* Ms */
				}
			} else {
				statistics[pixel].Lp_Ls++;		/* Lp or Ls */
			}

		}

		tb[pixel] = ta[pixel];
		judgeEventFlags(tb[pixel], 1.0, event_flags);
			/* assume next event 1-sec after */
		BnkfPutM("XRSsim:EVENT_FLAGS", strlen(event_flags), event_flags);
		t0[pixel] = photon_time;	/* remember previous event arrival time */
		strcpy(tentative_event_flags[pixel], event_flags);
		set_tentative_event[pixel] = 1;

		fs = event_flags;
		if ( 'H' == fs[0] ) {
			statistics[pixel].H++;			/* H */
			statistics[pixel].H_Mp++;
		} else if ( 'M' == fs[0] ) {
			if ( 'S' != fs[1] ) {
				statistics[pixel].H_Mp++;	/* Mp */
			} else {
				statistics[pixel].Ms++;		/* Ms */
			}
		} else {
			statistics[pixel].Lp_Ls++;		/* Lp or Ls */
		}

		statistics[pixel].total++;
	} else {
		BnkfPutM("XRSsim:PREV_EVENT_FLAGS", 0, "");
		BnkfPutM("XRSsim:EVENT_FLAGS", 0, "");
	}

/* put event information */
	BnkfPutM("SimASTE:TIME", sizeof(photon_time), &photon_time);
	BnkfPutM("SimASTE:PI", sizeof(pi_ch), &pi_ch);
	BnkfPutM("SimASTE:PHA", sizeof(pi_ch), &pi_ch);
	BnkfPutM("SimASTE:PIXEL", sizeof(pixel), &pixel);
	BnkfPutM("SimASTE:DETX", sizeof(detx_ch), &detx_ch);
	BnkfPutM("SimASTE:DETY", sizeof(dety_ch), &dety_ch);
	BnkfPutM("SimASTE:SKYX", sizeof(skyx_ch), &skyx_ch);
	BnkfPutM("SimASTE:SKYY", sizeof(skyy_ch), &skyy_ch);
	BnkfPutM("SimASTE:ROLL", sizeof(roll), &roll);

	EvsfSetM("SimASTE_XRSRMFsim:OK");
}

void
SimASTE_XRSRMFsim_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRSRMFsim_exit(int *status)
{
	show_statistics("%s: *** pixel statistics ***");

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
