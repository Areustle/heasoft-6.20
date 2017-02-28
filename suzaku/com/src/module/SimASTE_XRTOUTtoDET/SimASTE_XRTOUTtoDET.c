/*
 SimASTE_XRTOUTtoDET.c
   SimASTE module : convert XRTOUTX/Y -> DETX/Y

  1998/02/24	version 1.00	Y.ISHISAKI
	coded first

  1998/09/10	version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	fill BNK of "SimASTE:FOCX", "SimASTE:FOCY"

  2003-09-13	version 1.20	Y.ISHISAKI
	use latest aste_coord in astetool-1.24

  2006-04-09 version 1.3	Y.ISHISAKI
	change version number only

  2006-08-07 version 2.2	Y.ISHISAKI
	BnkGet com.teldef in _init()
	BnkGet/Put only required
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "atFunctions.h"
#include "aste_coord.h"

static char pname[] = "SimASTE_XRTOUTtoDET";
char SimASTE_XRTOUTtoDET_version[] = "version 2.2";

static struct {
	TELDEF *teldef;
} com;

static void
show_parameter(char *title)
{
	;
}

void
SimASTE_XRTOUTtoDET_startup(int *status)
{
	;
}

void
SimASTE_XRTOUTtoDET_init(int *status)
{
	int used;

	EvsDef("SimASTE_XRTOUTtoDET:BEGIN");
	EvsDef("SimASTE_XRTOUTtoDET:ENTRY");
	EvsDef("SimASTE_XRTOUTtoDET:OK");

	BnkfGetM("SimASTE:TELDEF", sizeof(com.teldef), &used, &com.teldef);

	show_parameter("%s:  *** show parameter ***");

	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	if ( *status ) {	/* ftools */
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
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_bgnrun(int *status)
{
	EvsSet("SimASTE_XRTOUTtoDET:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_ana(int nevent, int eventid, int *status)
{
	int used;
	double xrtx_mm, xrty_mm;
/*	double theta, phi;*/
	double detx_ch, dety_ch, focx_ch, focy_ch;

	EvsfSetM("SimASTE_XRTOUTtoDET:ENTRY");

	BnkfGetM("SimASTE:XRTOUTX", sizeof(xrtx_mm), &used, &xrtx_mm);
	BnkfGetM("SimASTE:XRTOUTY", sizeof(xrty_mm), &used, &xrty_mm);
/*	BnkfGetM("SimASTE:XRTOUTtheta", sizeof(theta), &used, &theta);
	BnkfGetM("SimASTE:XRTOUTphi", sizeof(phi), &used, &phi);*/

	aste_xrt2det(com.teldef, xrtx_mm, xrty_mm, &detx_ch, &dety_ch);
	BnkfPutM("SimASTE:XISX", sizeof(detx_ch), &detx_ch);
	BnkfPutM("SimASTE:XISY", sizeof(dety_ch), &dety_ch);
	BnkfPutM("SimASTE:XRSX", sizeof(detx_ch), &detx_ch);
	BnkfPutM("SimASTE:XRSY", sizeof(dety_ch), &dety_ch);

	aste_det2foc(com.teldef, detx_ch, dety_ch, &focx_ch, &focy_ch);
	BnkfPutM("SimASTE:FOCX", sizeof(focx_ch), &focx_ch);
	BnkfPutM("SimASTE:FOCY", sizeof(focy_ch), &focy_ch);

	EvsfSetM("SimASTE_XRTOUTtoDET:OK");

	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XRTOUTtoDET_exit(int *status)
{
	*status = ANL_OK;
}
