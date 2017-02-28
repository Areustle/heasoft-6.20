/*
 *  HXDrndInit
 *       version 0.0.0          99/8/10    C.Tanihata
 *       version 0.0.4          99/12/24   Y.Terada
 *         change param name for the third release of hxd-ftools.
 *       version 0.1.0          03/07/23   Y Terada, H Takahashi, M Suzuki
 *         for HEADAS.
 *       version 0.2.0          05/02/24   Y Terada
 *         change keyword name in PIL.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "aste_rand.h"
#include "fitsio.h"
#include "cfortran.h"
#include "HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#define DEBUG 1

char HXDrndInit_version[] = "version 0.2.0";
static char pname[] = "HXDrndInit";

static int irseed=7;
static int skip=0;

void
HXDrndInit_startup(int *status){ *status = ANL_OK; }

void
HXDrndInit_com(int *status) {
    
    if ( *status ) { /* ftools */
	
        *status = 0;
	/*
	UCLGSI("rnd_seed", irseed, *status);
	UCLGSI("rnd_skip", skip, *status);
	*/

	*status = PILGetInt("rand_seed", &irseed);
        if ( *status ) {
	  fprintf(stderr, "%s: PILGetInt rnd_seed error (%d)\n", 
		  pname, *status);
            *status = ANL_QUIT;
            exit(-1);
        }
        
	*status = PILGetInt("rand_skip", &skip);
        if ( *status ) {
	  fprintf(stderr, "%s: PILGetInt rnd_skip error (%d)\n", 
		  pname, *status);
            *status = ANL_QUIT;
            exit(-1);
        }
        
        *status = ANL_OK;
        return;
    }
    /* ANL */
    
    CLintrd("Random number seed", &irseed);
    CLintrd("Random number skip", &skip);
    
    *status = ANL_OK;
}

void
HXDrndInit_init(int *status) {
    
    aste_rndtsini(irseed);
    aste_drndtsn_skip(skip); 
    
    *status = ANL_OK;
    
}

void
HXDrndInit_his(int *status){ *status = ANL_OK; }

void
HXDrndInit_bgnrun(int *status){ *status = ANL_OK; }

void
HXDrndInit_ana(int nevent, int eventid, int *status){
    
    *status = ANL_OK;
    return;
    
}

void
HXDrndInit_endrun(int *status){ *status = ANL_OK; }

void
HXDrndInit_exit(int *status){ *status = ANL_OK; }



/* ======================= End of HXDrndInit.c =========================== */
