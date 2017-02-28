/**************************************
  XISpreparePHASCORR

  verseion 0.0	2005/09/17 created by hayasida
    Module to define BNK"PHASCORR" and copy data from "PHAS"
  version 0.1	2005/09/20         by nakajima
    correct the size of PHASCORR in BNKPUT
  version 0.2	2006/08/25         Y.ISHISAKI
    remove unused "cfortran.h", "headas.h"
    use anl_msg_**() functions for messages
  version 3.0	2007/04/22         Y.ISHISAKI
    change definition of XIS:PHASCORR as int -> double
    BnkDef "XIS:PHA:DOUBLE" in _ini()
    add rand_seed, rand_skip parameters
  version 3.1	2007/05/03         Y.ISHISAKI
    BnkDef "XIS:PHANOCTI:DOUBLE", "XIS:PHASNOCTI:DOUBLE" in _ini()
    BnkDef "XIS:PHAOCTI", if not defined in _ini()
  version 3.2	2007/05/07         Y.ISHISAKI
    fill PHAS[0] to "XIS:PHA:DOUBLE", "XIS:PHANOCTI:DOUBLE" in timing mode
    BnkGet "XIS:EDITMODE" in _ini()
**************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "com.h"
#include "cli.h"
#include "pil.h"
#include "fitsio.h"
#include "aste_rand.h"
#include "xisTelemFormat.h"

static char pname[] = "XISpreparePHASCORR";
char XISpreparePHASCORR_version[] = "version 3.2";

static struct {
  int flag_rand_phas0;
  int rand_seed;
  double rand_skip;
} com;

static int editmode;
static int num_pixel;

static void
showParam(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "FLAG_RAND_PHAS0", com.flag_rand_phas0 ? "YES" : "NO");
  printf("%20s   %d\n", "RAND_SEED", com.rand_seed);
  printf("%20s   %.0f\n", "RAND_SKIP", com.rand_skip);
}

void
XISpreparePHASCORR_startup(int *status)
{
  *status = ANL_OK;
}

void
XISpreparePHASCORR_com(int *status)
{
  static char *keytbl[] = {
    "FLAG_RAND_PHAS0",
    "RAND_SEED",
    "RAND_SKIP",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "flag to randomize PHAS[0]",
    "random number seed",
    "random number skip count",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetBool(k="flag_rand_phas0", &com.flag_rand_phas0) ||
PILGetInt (k="rand_seed", &com.rand_seed) ||
PILGetReal(k="rand_skip", &com.rand_skip) ||
	0 ) {
      anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
    return;
  }

  for(;;) {
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if ( 0 == strcmp ("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp("FLAG_RAND_PHAS0", key) ) {
      CLlogrd(key, &com.flag_rand_phas0);
    } else if ( 0 == strcmp("RAND_SEED", key) ) {
      CLintrd(key, &com.rand_seed);
    } else if ( 0 == strcmp("RAND_SKIP", key) ) {
      CLfdprd(key, &com.rand_skip);
    } else if ( 0 == strcmp("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XISpreparePHASCORR_init(int *status)
{
  int used;

  showParam();

  EvsDef("XISpreparePHASCORR:BEGIN");
  EvsDef("XISpreparePHASCORR:ENTRY");
  EvsDef("XISpreparePHASCORR:OK");

  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &editmode);

  switch ( editmode ) {
  case XISedit5x5:
    num_pixel = XISoneEventPixelTotNo5x5;
    break;
  case XISedit3x3:
    num_pixel = XISoneEventPixelTotNo3x3;
    break;
  case XISedit2x2:
    num_pixel = XISoneEventPixelTotNo2x2;
    break;
  case XISeditTiming:
    num_pixel = 1;
    break;
  default:
    num_pixel = 0;
  }

  if ( 0 < num_pixel ) {
    if ( ANL_OK != BnkIsDef("XIS:PHANOCTI") ) {
      BnkDef("XIS:PHANOCTI", sizeof(int));
    }
    BnkDef("XIS:PHASNOCTI", sizeof(double) * num_pixel);
    BnkDef("XIS:PHANOCTI:DOUBLE", sizeof(double));
    BnkDef("XIS:PHASCORR", sizeof(double) * num_pixel);
    BnkDef("XIS:PHA:DOUBLE", sizeof(double));
  }

  aste_rndtsini(com.rand_seed);
  aste_drndtsn_skipd(com.rand_skip);

  *status = ANL_OK;
}

void
XISpreparePHASCORR_his(int *status)
{
  *status = ANL_OK;
}

void
XISpreparePHASCORR_bgnrun(int *status)
{
  EvsfSetM("XISpreparePHASCORR:BEGIN");

  *status = ANL_OK;
}

void
XISpreparePHASCORR_ana(int nevent, int eventid, int *status)
{
  int i, used;
  int phas[XISoneEventPixelTotNo5x5];
  double phascorr[XISoneEventPixelTotNo5x5];

  EvsfSetM("XISpreparePHASCORR:ENTRY");

  switch ( editmode ) {
  case XISedit5x5:
  case XISedit3x3:
  case XISedit2x2:
  case XISeditTiming:
    break;
  default:
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:PHAS", sizeof(int) * num_pixel, &used, phas);
  for (i = 0; i < num_pixel; i++) {
    phascorr[i] = phas[i];
  }
  if ( com.flag_rand_phas0 ) {
    phascorr[0] += aste_drndts() - 0.5;
  }
  BnkfPutM("XIS:PHASNOCTI", sizeof(double) * num_pixel, phascorr);
  BnkfPutM("XIS:PHASCORR", sizeof(double) * num_pixel, phascorr);

  if ( XISeditTiming == editmode ) {
    BnkfPutM("XIS:PHA:DOUBLE", sizeof(double), phascorr);
    BnkfPutM("XIS:PHANOCTI:DOUBLE", sizeof(double), phascorr);
  }

  EvsfSetM("XISpreparePHASCORR:OK");
  *status = ANL_OK;
}

void
XISpreparePHASCORR_endrun(int *status)
{
  fitsfile *fp;			/* for output evt fits */
  char history[2*PIL_LINESIZE];
  int used = 0, istat = 0;

                          /*************************************************/
                          /* Write a HISTORY keyword into output fits file */
                          /*************************************************/
  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) ) {
    *status = ANL_OK;
    return;
  }

  sprintf(history, "\
  flag_rand_phas0=%s  rand_seed=%d  rand_skip=%.0f",
	com.flag_rand_phas0 ? "yes" : "no", com.rand_seed, com.rand_skip);
  fits_write_history(fp, history, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XISpreparePHASCORR_exit(int *status)
{
  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
