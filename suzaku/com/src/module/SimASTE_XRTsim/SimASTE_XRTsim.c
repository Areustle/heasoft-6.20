/*
 SimASTE_XRTsim.c
   SimASTE module : XRT Ray-tracing calling Fink's library

  1998/02/25  version 1.00  Y.UEDA, Y.ISHISAKI
	coded first

  1998/04/14  version 1.01  Y.ISHISAKI
	SimASTE_DRNDTS -> aste_drndts

  1998/09/10  version 1.10    Y.ISHISAKI
	work with ftools parameter interface
	add area_scale parameter
	remove minAngle, maxAngle, minRadius, maxRadius

  1999/03/10  version 1.20    Y.UEDA
	new functions in xrrt-2.1a (e.g. ASTRO-E scattering function) are available
	(parameter file is changed accordingly)

  2003/09/11  version 1.30    Y.ISHISAKI
	use xrrt-test4.1.1
	read thermal shield transmission from file

  2005/12/09  version 1.40    Y.ISHISAKI
	change column name: "Energy" -> "ENERGY", "Transmission" -> "TRANSMIS"

  2005/12/20  version 1.50    Y.ISHISAKI
	modified for xrrt-6.2
	added new parameter, scatterindexfile
	EVS 'SimASTE:XRT:ABS_BY_THERMAL_SHIELD' -> 'SimASTE:XRT:ABS_BY_THRMSHIELD'

  2005/12/24  version 1.60    Y.ISHISAKI
	fix EVS names, CANNOT_LEAVE_MIRROR, HITS_TOP_OF_COLLI, CANNOT_LEAVE_COLLI,
		ABS_ON_OUTER_COLLI, ABS_ON_INNER_COLLI, ABS_BY_SHIELD
	print precollimatorfile in _init()

  2006/03/06  version 1.70	Y.ISHISAKI
	add com.quadrant[], quadrant parameter

  2006-04-09 version 1.8	Y.ISHISAKI
	ANL_SKIP if 0.0 == eff, in SimASTE_XRTsim_ana()

  2006-07-24 version 2.0	Y.ISHISAKI
	support for CALDB
	BnkGet SimASTE:TELESCOP:PTR, SimASTE:INSTRUME:PTR for CALDB
	remove/rename several parameter names,
	removed:
		precollimatorfile, precollimatorreffile, atomfile, etc.
	renamed:
		xrtfile							-> mirrorfile
		thickness						-> foilthickness
		shadows							-> obstruct
		precollimator					-> pcol
		reffile							-> reflectfile
		scatterindexfile				-> backproffile
		missalignmentmode				-> missalignmode
		precollimatorthickness			-> pcolthicknes
		precollimatormissalignmentmode	-> pcolmissalignmode
		precollimatormissalignment		-> pcolmissalignment

  2006-08-05 version 2.2	Y.ISHISAKI
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	BnkPut SimASTE:SHIELD_TRANSMIS:FUNC in _init()
	BnkPut SimASTE:SHIELD_TRANSMIS in _ana()
	rename com.scattermode -> com.scatter_mode
	static declaration of show_parameter()

  2006-10-31 version 2.3	Y.ISHISAKI
	quit on error in _com(), _init()

  2008-03-03 version 3.0	Y.ISHISAKI
	changes for xrrt-6.5.0,
	remove scatter, misalignment, and ASCA/SUZAKU scatter parameters
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_caldb.h"
#include "xrrtexternal.hh"
#include "pil.h"
#include "fitsio.h"
#include "SimASTE.h"

#define TRANSMISSION_INDEX_SEARCH	/* fast index search for transmission */

char pname[] = "SimASTE_XRTsim";
char SimASTE_XRTsim_version[] = "version 3.0";

/* extracted from xrrtphoton.hh in xrrt-test4.1.0 */
enum PhotonStatusCodes {
	PHOTON_CONTINUES,
	PHOTON_HITS_OBSTRUCTION,
	PHOTON_HITS_OUTER_HOUSING,
	PHOTON_HITS_INNER_HOUSING,
	PHOTON_HITS_TOP_OF_MIRROR,
	PHOTON_ABSORBED_ON_OUTER_MIRROR,
	PHOTON_ABSORBED_ON_INNER_MIRROR,
	PHOTON_REVERSES_Z_DIRECTION,
	PHOTON_CAN_NOT_LEAVE_MIRROR,
	PHOTON_HIT_FOCAL_PLANE,
/*
	// Add the case of collimator using for ASTRO-E-II
	// (added by HIDEYUKI MORI)
*/
	PHOTON_HITS_TOP_OF_COLLIMATOR,
	PHOTON_CAN_NOT_LEAVE_COLLIMATOR,
	PHOTON_ABSORBED_ON_OUTER_COLLIMATOR,
	PHOTON_ABSORBED_ON_INNER_COLLIMATOR,
	ERROR
};

/* extracted from xrrtscatter.hh in xrrt-test4.1.0 */
enum ScatterType {
	NO_SCATTER=0,
	ASCA_SCATTER=1,
	ASTROE_SCATTER=2
};

/* followings are examples of input parameters for SimASTE_XRTsim */
static struct {
	int simulation_mode;		/* -1: follow SimASTEroot setting */

	char *telescop;
	char *instrume;
	int xrt_id;

/* xrrtloadmirror() parameters */
	char *mirrorfile, o_mirrorfile[PIL_LINESIZE];
	char mirror[32], obstruct[32], quadrant[32], pcol[32];
	char *reflectfile, o_reflectfile[PIL_LINESIZE];
	char *backproffile, o_backproffile[PIL_LINESIZE];

/* consider thermal shield or not */
	char *shieldfile, o_shieldfile[PIL_LINESIZE];
	struct {
		int nrow;
		double *en, *va;
#ifdef TRANSMISSION_INDEX_SEARCH
		struct trans_index {
			double norm, offs;
			int nbody;		/* in fact, sizeof(body)-1 */
			int *body;		/* 0-nbody */
		} index;
#endif
	} trans;

	int (*prev_write_history)(fitsfile *);

} com;

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

	sprintf(history, "\
  xrt_id='%s'", aste_instrume(com.xrt_id));
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  mirrorfile='%s'%s", com.mirrorfile,
		(com.mirrorfile == com.o_mirrorfile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  mirror='%s'  obstruct='%s'  quadrant='%s'  pcol='%s'",
		com.mirror, com.obstruct, com.quadrant, com.pcol);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  reflectfile='%s'%s", com.reflectfile,
		(com.reflectfile == com.o_reflectfile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  backproffile='%s'%s", com.backproffile,
		(com.backproffile == com.o_backproffile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  shieldfile='%s'%s",
		(NULL == com.shieldfile) ? com.o_shieldfile : com.shieldfile,
		(NULL == com.shieldfile ||
		 com.shieldfile == com.o_shieldfile ) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static void
show_parameter(char *title)
{
#define MSG	anl_msg_always
	MSG("\n");
	MSG(title, pname);
	MSG("\n\n");

	MSG("%4s%-24s%s\n", "", "SIMULATION_MODE",
		(com.simulation_mode < 0) ? "Follow SimASTE_Root" :
		(SimASTE_Mode_Discard == com.simulation_mode) ? "Discard" :
		(SimASTE_Mode_Weight == com.simulation_mode) ? "Weight" :
		"unkown");

	MSG("%4s%-24s'%s'\n", "", "XRT_ID", aste_instrume(com.xrt_id));

	MSG("%4s%-24s'%s'%s\n", "", "MIRRORFILE", com.mirrorfile,
		(com.mirrorfile == com.o_mirrorfile) ? "" : " (CALDB)");
	MSG("%4s%-24s'%s'\n", "", "  MIRROR", com.mirror);
	MSG("%4s%-24s'%s'\n", "", "  OBSTRUCT", com.obstruct);
	MSG("%4s%-24s'%s'\n", "", "  QUADRANT", com.quadrant);
	MSG("%4s%-24s'%s'\n", "", "  PCOL", com.pcol);
	MSG("%4s%-24s'%s'%s\n", "", "REFLECTFILE", com.reflectfile,
		(com.reflectfile == com.o_reflectfile) ? "" : " (CALDB)");
	MSG("%4s%-24s'%s'%s\n", "", "BACKPROFFILE", com.backproffile,
		(com.backproffile == com.o_backproffile) ? "" : " (CALDB)");
	MSG("%4s%-24s'%s'%s\n", "", "SHIELDFILE",
		(NULL == com.shieldfile) ? com.o_shieldfile : com.shieldfile,
		(NULL == com.shieldfile ||
		 com.shieldfile == com.o_shieldfile ) ? "" : " (CALDB)");
#undef MSG
}

static int
read_trans_file(char *shieldfile)
{
	int hdutype;
	fitsfile *fp;

	int istat = 0;

	fits_open_file(&fp, shieldfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: XRT thermal shield transmission '%s' open failed\n", pname, shieldfile);
		return -1;
	}

	for (;;) {
#ifdef TRANSMISSION_INDEX_SEARCH
		int i, j, k, nbody, *body;
		double offs, norm;
#endif
		int ne, col_energy, col_trans, anynul;

		istat = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
		if ( istat ) {
			fits_close_file(fp, &istat);
			anl_msg_error("\
%s: no transmission data in '%s'\n", pname, shieldfile);
			return -1;
		}
		fits_read_key(fp, TINT, "NAXIS2", &ne, NULL, &istat);
		fits_get_colnum(fp, CASEINSEN, "ENERGY", &col_energy, &istat);
		fits_get_colnum(fp, CASEINSEN, "TRANSMIS", &col_trans, &istat);
		if ( istat ) {
			istat = 0;
			fits_get_colnum(fp, CASEINSEN, "TRANSMISSION", &col_trans, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_get_colunm('TRANSMIS') failed (%d)\n", pname, istat);
				return istat;
			}
		}
		com.trans.en = malloc(2 * sizeof(double) * ne
#ifdef TRANSMISSION_INDEX_SEARCH
				+ sizeof(*com.trans.index.body) * (com.trans.index.nbody + 1)
#endif
							  );
		com.trans.va = &com.trans.en[ne];
		if ( NULL == com.trans.en ) {
			anl_msg_error("\
%s: Energy/Transmission malloc failed for '%s'\n", pname, shieldfile);
			return -1;
		}
		ffgcvd(fp, col_energy, 1, 1, ne, 0.0, com.trans.en, &anynul, &istat);
		ffgcvd(fp, col_trans,  1, 1, ne, 0.0, com.trans.va, &anynul, &istat);
		fits_close_file(fp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: Energy/Transmission column read error for '%s'\n", pname, shieldfile);
			return -1;
		}

#ifdef TRANSMISSION_INDEX_SEARCH
		nbody = com.trans.index.nbody;
		body = com.trans.index.body = (int *)( &com.trans.va[ne] );
		offs = com.trans.index.offs = com.trans.en[0];
		norm = com.trans.index.norm = com.trans.en[ne-1];
		for (i = j = 0; i < ne - 1; i++) {
			k = nbody * ( com.trans.en[i] - offs ) / norm;
			while ( j <= k ) body[j++] = i;
		}
		while ( j < nbody+1 ) body[j++] = i;
#endif

		return ne;
	}
}

static double
transmission_at(double energy)
{
	double trans, x0, x1, y0, y1;

#ifdef TRANSMISSION_INDEX_SEARCH
	int ipos, ie, ne;
	struct trans_index *idx = &com.trans.index;

	ipos = idx->nbody * ( energy - idx->offs ) / idx->norm;
	if ( ipos < 0 || idx->nbody <= ipos ) return 0.0;	/* out of bounds */
	ne = com.trans.nrow - 1;

	for (ie = idx->body[ipos]; ie < ne; ie++) {
		if ( energy < com.trans.en[ie] ) break;
	}

	if ( ie <= 0 ) return 0.0;

	x0 = com.trans.en[ie-1];
	x1 = com.trans.en[ie];
	y0 = com.trans.va[ie-1];
	y1 = com.trans.va[ie];

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

	if ( energy < com.trans.en[0] ) {
		return 0.0;
	}

	trans = 0.0;
	n = com.trans.nrow - 1;
	for (i = 0; i < n; i++) {
		if ( energy <= com.trans.en[i+1] ) {
			x0 = com.trans.en[i];
			x1 = com.trans.en[i+1];
			y0 = com.trans.va[i];
			y1 = com.trans.va[i+1];
			trans = (y0 * (x1 - energy) + y1 * (energy - x0)) / (x1 - x0);
			break;
		}
	}

#endif

	return trans;
}

void
SimASTE_XRTsim_startup(int *status)
{
	com.simulation_mode = -1;
	com.instrume = "XRS";
	com.xrt_id = ASTE_XRTS_ID;
	com.mirrorfile = strcpy(com.o_mirrorfile, "CALDB");
	strcpy(com.mirror, "mirror");
	strcpy(com.obstruct, "obstruct");
	strcpy(com.quadrant, "quadrant");
	strcpy(com.pcol, "pcol");
	com.reflectfile = strcpy(com.o_reflectfile, "CALDB");
	com.backproffile = strcpy(com.o_backproffile, "CALDB");
/* for transmission search */
	com.shieldfile = strcpy(com.o_shieldfile, "CALDB");
	com.trans.nrow = 0;
	com.trans.en = com.trans.va = NULL;
#ifdef TRANSMISSION_INDEX_SEARCH
	com.trans.index.norm = 0.0;
	com.trans.index.offs = 0.0;
	com.trans.index.nbody = 10000;
	com.trans.index.body = NULL;
#endif
}

void
SimASTE_XRTsim_com(int *status)
{
	char *k;

	if ( *status ) {			/* ftools */

		if ( PILGetFname (k="mirrorfile", com.o_mirrorfile) ||
			 PILGetFname (k="reflectfile", com.o_reflectfile) ||
			 PILGetFname (k="backproffile", com.o_backproffile) ||
			 PILGetFname (k="shieldfile", com.o_shieldfile) ||
			 PILGetString(k="mirror", com.mirror) ||
			 PILGetString(k="obstruct", com.obstruct) ||
			 PILGetString(k="quadrant", com.quadrant) ||
			 PILGetString(k="pcol", com.pcol) ||
			 0 ) {
			goto pil_error;
		}

	}

	*status = ANL_OK;
	return;

 pil_error:
	anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XRTsim_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;

	static struct CALDB_INDEX {
		int allow_none;
		char *instrume;
		char *codename;
		char *origname;
		char **realname_ptr;
	} caldb_index[] = {
		{ 0, NULL, "GEOMETRY", com.o_mirrorfile, &com.mirrorfile},
		{ 0, "XRT","REFLECTIVITY", com.o_reflectfile, &com.reflectfile},
		{ 0, "XRT","BACKPROF", com.o_backproffile, &com.backproffile},
		{ 1, "XRT","FTRANS", com.o_shieldfile, &com.shieldfile},
		{ 0, NULL, NULL, NULL, NULL }
	};

	int i, used, sensor;
	CALDB_INFO caldb;
	double (*func2)(double energy);

	int raytrace_status = 0;

	EvsDef("SimASTE_XRTsim:BEGIN");
	EvsDef("SimASTE_XRTsim:ENTRY");
	EvsDef("SimASTE_XRTsim:OK");

	EvsDef("SimASTE:XRT:ABS_BY_SHIELD");
	EvsDef("SimASTE:XRT:CONTINUES");
	EvsDef("SimASTE:XRT:HITS_OBSTRUCTION");
	EvsDef("SimASTE:XRT:HITS_OUTER_HOUSING");
	EvsDef("SimASTE:XRT:HITS_INNER_HOUSING");
	EvsDef("SimASTE:XRT:HITS_TOP_OF_MIRROR");
	EvsDef("SimASTE:XRT:ABS_ON_OUTER_MIRROR");
	EvsDef("SimASTE:XRT:ABS_ON_INNER_MIRROR");
	EvsDef("SimASTE:XRT:REVERSES_Z_DIRECTION");
	EvsDef("SimASTE:XRT:CANNOT_LEAVE_MIRROR");
	EvsDef("SimASTE:XRT:HIT_FOCAL_PLANE");
	EvsDef("SimASTE:XRT:HITS_TOP_OF_COLLI");
	EvsDef("SimASTE:XRT:CANNOT_LEAVE_COLLI");
	EvsDef("SimASTE:XRT:ABS_ON_OUTER_COLLI");
	EvsDef("SimASTE:XRT:ABS_ON_INNER_COLLI");
	EvsDef("SimASTE:XRT:ERROR");

	BnkGet("SimASTE:TELESCOP:PTR", sizeof(com.telescop), &used, &com.telescop);
	BnkGet("SimASTE:INSTRUME:PTR", sizeof(com.instrume), &used, &com.instrume);
	BnkGet("SimASTE:SENSOR", sizeof(sensor), &used, &sensor);
	if ( ASTE_XRS_ID == sensor ) {
		com.xrt_id = ASTE_XRTS_ID;
	} else if ( ASTE_XIS0_ID <= sensor && sensor <= ASTE_XIS3_ID ) {
		com.xrt_id = ASTE_XRT0_ID + sensor - ASTE_XIS0_ID;
	} else if ( ASTE_XRT0_ID <= sensor && sensor <= ASTE_XRTS_ID ) {
		com.xrt_id = sensor;
	} else {
		anl_msg_error("\
%s: invalid instrume='%s'\n", pname, com.instrume);
		goto quit;
	}

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	for (i = 0; NULL != caldb_index[i].realname_ptr; i++) {
		int ifound;
		struct CALDB_INDEX *p = &caldb_index[i];

		aste_caldb_init(&caldb);
		if ( p->allow_none && 0 == CLstricmp("NONE", p->origname) ) {
			*p->realname_ptr = NULL;
		} else if ( 0 == CLstricmp("CALDB", p->origname) ) {
			caldb.telescop = com.telescop;
			if ( NULL == p->instrume ) {
				caldb.instrume = aste_instrume(com.xrt_id);
			} else {
				caldb.instrume = p->instrume;
			}
			caldb.codename = p->codename;
			aste_caldb_get(&caldb);
			if ( 0 != caldb.status || 0 == caldb.nfound ) {
				anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
					pname, caldb.codename, caldb.status);
				goto quit;
			}
			for (ifound = 1; ifound < caldb.nfound; ifound++) {
				if ( 0 != strcmp(caldb.fnames[0], caldb.fnames[ifound]) ) {
					anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
						pname, caldb.nfound, caldb.codename);
					break;
				}
			}
			*p->realname_ptr = caldb.filename;
		}
	}

	show_parameter("%s:  *** show parameter ***");

	if ( com.simulation_mode < 0 ) {
		BnkGet("SimASTE:MODE",
			   sizeof(com.simulation_mode), &used, &com.simulation_mode);
	}

	anl_msg_info("\n\
%s: reading mirror & reflection file ...\n", pname);

	xrrtloadmirror(com.mirrorfile, com.mirror, com.obstruct, com.quadrant,
				   com.pcol, com.reflectfile, com.backproffile,
				   &raytrace_status);

	if ( raytrace_status ) {
		anl_msg_error("\
%s: xrrtloadmirror: status = %d\n", pname, raytrace_status);
		goto quit;
	}

	func2 = NULL;
	if ( NULL != com.shieldfile ) {
		anl_msg_info("\
Reading '%s' ...\n", com.shieldfile);
		com.trans.nrow = read_trans_file(com.shieldfile);
		if ( com.trans.nrow < 0 ) {
			goto quit;
		}
		func2 = transmission_at;
	}
	BnkPut("SimASTE:XRT:SHIELD_TRANSMIS:FUNC", sizeof(func2), &func2);

	anl_msg_info("\
finished\n");

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XRTsim_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRTsim_bgnrun(int *status)
{
	EvsSet("SimASTE_XRTsim:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_XRTsim_ana(int nevent, int eventid, int *status)
{
	int sensor, used, return_code, raytrace_status;
	double energy, eff, weight, radius, angle, unitradial, unitphi, unitz;
	double xrtin_x,  xrtin_y,  xrtin_theta,  xrtin_phi;
	double xrtout_x, xrtout_y, xrtout_theta, xrtout_phi;

	EvsfSetM("SimASTE_XRTsim:ENTRY");

	BnkfGetM("SimASTE:SENSOR", sizeof(sensor), &used, &sensor);
	BnkfGetM("SimASTE:PHOTON_ENERGY", sizeof(energy), &used, &energy);
	BnkfGetM("SimASTE:XRTINX", sizeof(xrtin_x), &used, &xrtin_x);
	BnkfGetM("SimASTE:XRTINY", sizeof(xrtin_y), &used, &xrtin_y);
	BnkfGetM("SimASTE:XRTINtheta", sizeof(xrtin_theta), &used, &xrtin_theta);
	BnkfGetM("SimASTE:XRTINphi", sizeof(xrtin_phi), &used, &xrtin_phi);

	eff = 1.0;

	if ( NULL != com.shieldfile ) {
		eff = transmission_at(energy);
	}

	if ( SimASTE_Mode_Discard == com.simulation_mode ) {
		if ( eff < aste_drndts() ) {
			EvsfSetM("SimASTE:XRT:ABS_BY_SHIELD");
			*status = ANL_SKIP;
			return;
		}
	} else {
		if ( 0.0 == eff ) {
			EvsfSetM("SimASTE:XRT:ABS_BY_SHIELD");
			*status = ANL_SKIP;
			return;
		}
		BnkfGetM("SimASTE:WEIGHT", sizeof(weight), &used, &weight);
		weight *= eff;
		BnkfPutM("SimASTE:WEIGHT", sizeof(weight), &weight);
		BnkfPutM("SimASTE:XRT:SHIELD_TRANSMIS", sizeof(eff), &eff);
	}

	radius = sqrt(xrtin_x*xrtin_x + xrtin_y*xrtin_y);
	angle = atan2(xrtin_y, xrtin_x) * RAD2DEG;

	xrtin_theta *= ARCMIN2RAD;
	unitradial = sin(xrtin_theta);
	unitphi = xrtin_phi * DEG2RAD;
	unitz = - cos(xrtin_theta);

/* run ray-tracing */
	raytrace_status = 0;
	xrrtracephoton(&energy, &radius, &angle, &unitradial, &unitphi, &unitz,
				   &xrtout_x, &xrtout_y, &return_code, &raytrace_status);
	xrtout_theta = 0.0;			/* dummy value */
	xrtout_phi = 0.0;			/* dummy value */

	if ( raytrace_status ) {
		anl_msg_error("\
%s: Error in Ray Tracing (status=%d)\n", pname, raytrace_status);
		*status = ANL_SKIP;
		return;
	}

	switch ( return_code ) {
	case PHOTON_CONTINUES:
		EvsfSetM("SimASTE:XRT:CONTINUES");
		break;
	case PHOTON_HITS_OBSTRUCTION:
		EvsfSetM("SimASTE:XRT:HITS_OBSTRUCTION");
		break;
	case PHOTON_HITS_OUTER_HOUSING:
		EvsfSetM("SimASTE:XRT:HITS_OUTER_HOUSING");
		break;
	case PHOTON_HITS_INNER_HOUSING:
		EvsfSetM("SimASTE:XRT:HITS_INNER_HOUSING");
		break;
	case PHOTON_HITS_TOP_OF_MIRROR:
		EvsfSetM("SimASTE:XRT:HITS_TOP_OF_MIRROR");
		break;
	case PHOTON_ABSORBED_ON_OUTER_MIRROR:
		EvsfSetM("SimASTE:XRT:ABS_ON_OUTER_MIRROR");
		break;
	case PHOTON_ABSORBED_ON_INNER_MIRROR:
		EvsfSetM("SimASTE:XRT:ABS_ON_INNER_MIRROR");
		break;
	case PHOTON_REVERSES_Z_DIRECTION:
		EvsfSetM("SimASTE:XRT:REVERSES_Z_DIRECTION");
		break;
	case PHOTON_CAN_NOT_LEAVE_MIRROR:
		EvsfSetM("SimASTE:XRT:CANNOT_LEAVE_MIRROR");
		break;
	case PHOTON_HIT_FOCAL_PLANE:
		EvsfSetM("SimASTE:XRT:HIT_FOCAL_PLANE");
		break;
	case PHOTON_HITS_TOP_OF_COLLIMATOR:
		EvsfSetM("SimASTE:XRT:HITS_TOP_OF_COLLI");
		break;
	case PHOTON_CAN_NOT_LEAVE_COLLIMATOR:
		EvsfSetM("SimASTE:XRT:CANNOT_LEAVE_COLLI");
		break;
	case PHOTON_ABSORBED_ON_OUTER_COLLIMATOR:
		EvsfSetM("SimASTE:XRT:ABS_ON_OUTER_COLLI");
		break;
	case PHOTON_ABSORBED_ON_INNER_COLLIMATOR:
		EvsfSetM("SimASTE:XRT:ABS_ON_INNER_COLLI");
		break;
	case ERROR:
		EvsfSetM("SimASTE:XRT:ERROR");
		break;
	default:
		;
	}

	if ( PHOTON_HIT_FOCAL_PLANE != return_code ) {
		*status = ANL_SKIP;
		return;
	}

/* put data to BNK */
	BnkfPutM("SimASTE:XRTOUTX", sizeof(xrtout_x), &xrtout_x);
	BnkfPutM("SimASTE:XRTOUTY", sizeof(xrtout_y), &xrtout_y);
	BnkfPutM("SimASTE:XRTOUTtheta", sizeof(xrtout_theta), &xrtout_theta);
	BnkfPutM("SimASTE:XRTOUTphi", sizeof(xrtout_phi), &xrtout_phi);

	EvsfSetM("SimASTE_XRTsim:OK");

	*status = ANL_OK;
}


void
SimASTE_XRTsim_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRTsim_exit(int *status)
{
	*status = ANL_OK;
}
