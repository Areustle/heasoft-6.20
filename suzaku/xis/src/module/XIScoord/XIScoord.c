/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:05 1999 by E. Miyata*/
/* xis_module/XIScoord/0.0_fast/XIScoord_v0.0.c: modified by M.Ozaki on Fri Aug 13 21:04:46 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/**************************************

  XIScoord v2.34

  19990810	Emi Miyata
	fill ACT/DET/FOC/SKY column
	BNKget XIS:**
	BNKput XIS:**

  ver 0.1    1999.10.20 Emi Miyata
	include fast version
	support FOCX/Y, SKYX/Y ... but failed

  ver 0.2    1999.11.03 Emi Miyata
	add aste_coord.h

  ver 0.3    1999.12.27 Emi Miyata
	modify to use astetool ver1.20

  ver 0.4    1999.12.30 Emi Miyata
	modify parameter file

  ver 0.5    1999.12.30 Emi Miyata
	use attitude information based on xrscoord
	add history keywords

  ver 0.51   2000.01.13 Emi Miyata
	replace teldef -> teldef_file in CLtxtrd

  ver 0.52   2000.01.22 Emi Miyata
	fix attitude filename

  ver 0.6   2000.02.03 Emi Miyata
	support window mode

  ver 0.8   2003.07.03 Hironori Matsumoto
	solve the problem of sprintf by using malloc

  ver 1.0   2004.03.03 Hiroshi Murakami
	add random number (-0.5 -- 0.5) before coordinate conversion

  ver 1.1   2005.05.23 Atsushi Senda
	put TCRVL, TCRPX, and TCDLT to the correct values
	fix the unit of the header comments;
	    (mm) for DET, and (deg) for FOC, SKY

  ver 1.2   2005.05.30 Aya Bamba
	add editmode information
	for hot pixel detection tools with PPUX/Y

  ver 1.3   2005.05.30 Aya Bamba
	add teldef="none" support

  ver 1.4   2005.06.15 Atsushi Senda
	* changes for compatibility with a new module "aeaspect"
        - delete pointing=ATT option
        - configure not to modify RA_NOM, DEC_NOM values
        - calculate OPTIC10, OPTIC11 from RA_PNT, DEC_PNT values
	* add editmode branch not to convert DET -> FOC -> SKY
          in case darkinit, darkupdate, frame, and darkframe mode

  ver 1.5   2005.07.04 Aya Bamba
	change the parameter names according to aeaspect, aecoordcalc
	- teldef_xis? -> xis?_teldef
	- ea_phi -> ea1, ea_theta -> ea2, ea_psi -> ea3

  ver 1.6   2005.07.22 Aya Bamba
	add the coordinate calculation for p-sum mode
	(rawy, acty, dety, focy, skyy = blank)

  ver 1.7   2005.07.23 Aya Bamba
	change the Bnk names accordinga to the change of
	XISreadEvent 1.4 -> 1.5

  ver 1.71   2005.07.29 Aya Bamba
	fix some bugs regarding to BNK names

  ver 1.72   2005.08.10 Aya Bamba
	fix some bugs on TELDEF files

  ver 2.0    2005.09.17 Aya Bamba
	get window option information from frame header

  ver 2.1    2005.10.27 Aya Bamba
	calculate OPTIC10, OPTIC11 from RA_NOM, DEC_NOM values

  ver 2.2    2005.10.31 Aya Bamba
	change default attitude file name

  ver 2.3    2005.11.01 Aya Bamba
	get mean euler angles from event fits header

  ver 2.31   2005.11.01 Aya Bamba
	get mean euler angles from event fits header

  ver 2.32   2005.11.02 Aya Bamba
	write history of aberration correct
	write history of xiscoord version and time
	calculate mjd using aefits_mission2mjd_tt (quit to use leapsec file)

  ver 2.33   2005.11.04 Aya Bamba
	change some sentences in HISTORY
	fix non-ANSI style declaration of variables (by M.Ozaki)

  ver 2.34   2006.08.22 Y.ISHISAKI
	use anl_msg_**() functions for messages
	remove idetx/y = (int)(detx/y + 10000.5) - 10000;
	use floor() instead of (int)(** + 10000.5) - 10000 for ifocx/y, iskyx/y
	use aste_cor_aberration() for the aberration correction
	use aste_att_ea() instead of obsolete aste_att_euler()

  ver 2.4  2006.10.31 Y.ISHISAKI
	change parameter name, teldef_xis[0-3] -> teldef
	support CALDB
	attitude=DEFAULT is obsolete
	check PIL error in _com()

  ver 2.5  2006.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits

  ver 2.6  2007.05.07 Y.ISHISAKI
	fill blank on coordinates conversion error

  ver 2.7  2007.05.14 Y.ISHISAKI
	use fits_update_key() instead of fits_modify_key()
	BnkPut "XIS:DETX", "XIS:DETY" when XISeditDarkInit, XISeditDarkUpdate
	do nothing when XISeditFrame, XISeditDarkFrame
	set ACTY=511 in timing mode

  ver 2.8  2008.07.31 Y.ISHISAKI
	initialize rawx = rawy = com.blank for DarkInit/Update in _ana()

  ver 2.9  2008.10.17 Y.ISHISAKI
	use fits_update_key() instead of fits_modify_key() in modkeys()

*****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "pil.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_att.h"
#include "aste_time.h"
#include "aste_rand.h"
#include "aste_caldb.h"
#include "xisTelemFormat.h"
#include "xisEventFitsUtil.h"
#include "aeFitsHeaderUtil.h"

/*#define _PRINT_OUT_*/

static char pname[] = "XIScoord";
char XIScoord_version[] = "version 2.9";

/* attitude file */
enum pointing_type {
/* pointing=ATT の廃止 */
/* POINTING_ATT, */
  POINTING_KEY,
  POINTING_USER
};

enum attitude_type {
/* attitude=DEFAULT の廃止 */
/*  ATTITUDE_DEFAULT,*/
  ATTITUDE_EULER,
  ATTITUDE_FILE
};

static struct {
  TELDEF *teldef;
  ATTFILE *attfile;
  char *teldef_file, o_teldef_file[PIL_LINESIZE];
  char pointing[PIL_LINESIZE];
  char attitude_file[PIL_LINESIZE];
  enum attitude_type attitude_type;
  enum pointing_type pointing_type;
  int aberration;
  SKYREF skyref;
  double ea1, ea2, ea3;
  AtEulerAng ea;
  int rand_seed;
  double rand_skip;
  int blank;			/* value of NULL to use */
} com;

static int editmode;		/* edit mode number; see xisTelemFormat.h */
static int winopt;		/* window option mode */
static int win_st;		/* window start address */
static int mjdrefi;		/*  MJD reference day */
static double mjdreff;		/* MJD reference (fraction of day) */

static int
modkeys(fitsfile *fp, char *name, int col,
	int tlmin, int tlmax,
	double tcrpx, double tcrvl, double tcdlt, double optic
)
{
    char key[16], comment[80];
    int istat = 0;

    sprintf(key, "TLMIN%d", col);
    sprintf(comment, "minimum legal value for %s", name);
    fits_update_key_lng(fp, key, tlmin, comment, &istat);
    anl_msg_info("  %-8s = %20d / %s\n", key, tlmin, comment);

    sprintf(key, "TLMAX%d", col);
    sprintf(comment, "maximum legal value for %s", name);
    fits_update_key_lng(fp, key, tlmax, comment, &istat);
    anl_msg_info("  %-8s = %20d / %s\n", key, tlmax, comment);

    sprintf(key, "TCRPX%d", col);
    sprintf(comment, "%s reference pixel", name);
    fits_update_key_fixdbl(fp, key, tcrpx, 1, comment, &istat);
    anl_msg_info("  %-8s = %20.1f / %s\n", key, tcrpx, comment);

    /* XIScoord-1.1
       In case DETX/Y,
       an unit of TCRVL (reference pixel value) should be (mm),
       and an unit of TCDLT (pixel scale) should be (mm/pixel). */
    if ( 0 == strcmp("DETX", name) || 0 == strcmp("DETY", name) ) {
	sprintf(key, "TCRVL%d", col);
	sprintf(comment, "%s reference pixel value (mm)", name);
	fits_update_key_fixdbl(fp, key, tcrvl, 5, comment, &istat);
	anl_msg_info("  %-8s = %20.1f / %s\n", key, tcrvl, comment);

	sprintf(key, "TCDLT%d", col);
	sprintf(comment, "%s pixel scale (mm/pixel)", name);
	fits_update_key_fixdbl(fp, key, tcdlt, 7, comment, &istat);
	anl_msg_info("  %-8s = %20.3f / %s\n", key, tcdlt, comment);
    } else {
	sprintf(key, "TCRVL%d", col);
	sprintf(comment, "%s reference pixel value (deg)", name);
	fits_update_key_fixdbl(fp, key, tcrvl, 5, comment, &istat);
	anl_msg_info("  %-8s = %20.5f / %s\n", key, tcrvl, comment);

	sprintf(key, "TCDLT%d", col);
	sprintf(comment, "%s pixel scale (deg/pixel)", name);
	fits_update_key_fixdbl(fp, key, tcdlt, 7, comment, &istat);
	anl_msg_info("  %-8s = %20.7f / %s\n", key, tcdlt, comment);
    }
    sprintf(key, "OPTIC%d", col);
    sprintf(comment, "%s of the optical axis (pixel)", name);
    fits_update_key_fixdbl(fp, key, optic, 2, comment, &istat);
    anl_msg_info("  %-8s = %20.2f / %s\n", key, optic, comment);

    if ( istat ) {
	anl_msg_error("\
%s: updating keyword for %s failed (ignored)\n", pname, name);
	return istat;
    }

    return 0;
}

static void
show_parameter(void)
{
  printf("\n");
  printf("%s: *** show parameter ***\n", pname);
  printf("\n");
  printf("%20s   '%s'%s\n", "TELDEF", com.teldef_file,
    (com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
  if ( ATTITUDE_EULER == com.attitude_type ) {
    printf("%20s   EULER\n", "ATTITUDE");
    printf("%20s   (%.4f, %.4f, %.4f)\n", "EULER", com.ea1, com.ea2, com.ea3);
  } else {
    printf("%20s   '%s'\n", "ATTITUDE", com.attitude_file);
  }
  printf("%20s   (%.4f, %.4f, %.1f)\n", "SKYREF",
	 com.skyref.alpha, com.skyref.delta, com.skyref.roll);
  printf("%20s   %s\n", "ABERRATION", com.aberration ? "YES" : "NO");
  printf("%20s   %d\n", "RAND_SEED", com.rand_seed);
  printf("%20s   %.0f\n", "RAND_SKIP", com.rand_skip);
  printf("%20s   %d\n", "BLANK", com.blank); /* Values of NULL to use */
}

void
XIScoord_startup(int *status)
{
  com.teldef_file = com.o_teldef_file;
  com.attitude_type = ATTITUDE_FILE;
  com.pointing_type = POINTING_USER;
  com.rand_seed = 7;
  com.rand_skip = 0.0;

  *status = ANL_OK;
}

void
XIScoord_com(int *status)
{
  /* H. M. for random number */
  /* #define NVAL    13 */
  /* #define NVAL    15 */
#define NVAL       14
  static char *names[NVAL] = {
    "TELDEF",
    "ATTITUDE",
    "POINTING",
    "EULER",
    "SKYREF",
    "ABERRATION",
    "RAND_SEED",
    "RAND_SKIP",
    "BLANK",
    "SHOW",
    "EXIT"
  };
  static char *help[NVAL] = {
/*    "hk file list in ascii",*/
    "teldef file name",
    "attitude file name",
    "pointing type, KEY/USER",
    "euler angles in degree",
    "sky reference position",
    "correct aberration",
    "random number seed",
    "random number skip count",
    "Value of NULL to use",
    "Show current setting",
    "Exit from this menu"
  };
  int nreply = 1;
  int answer[2];
  char *k;

  /* パラメータファイルから値を読み込む */
  if ( *status ) {                        /* ftools */
    *status = 0;

    if ( PILGetFname(k="teldef", com.o_teldef_file) ||
	 PILGetFname(k="attitude", com.attitude_file) ) {
      goto pil_error;
    }
    if ( 0 == CLstricmp("EULER", com.attitude_file) ) {
      com.attitude_type = ATTITUDE_EULER;
      if ( PILGetReal(k="ea1", &com.ea1) ||
	   PILGetReal(k="ea2", &com.ea2) ||
	   PILGetReal(k="ea3", &com.ea3) ) {
	goto pil_error;
      }
      com.ea.phi = com.ea1 * DEG2RAD;
      com.ea.theta = com.ea2 * DEG2RAD;
      com.ea.psi = com.ea3 * DEG2RAD;
    } else {
      com.attitude_type = ATTITUDE_FILE;
    }
    if ( PILGetString(k="pointing", com.pointing) ) {
      goto pil_error;
    }
    if ( 0 == CLstricmp("KEY", com.pointing) ) {
      com.pointing_type = POINTING_KEY;
    } else if ( 0 == CLstricmp("USER", com.pointing) ) {
      com.pointing_type = POINTING_USER;
      if ( PILGetReal(k="ref_alpha", &com.skyref.alpha) ||
	   PILGetReal(k="ref_delta", &com.skyref.delta) ||
	   PILGetReal(k="ref_roll", &com.skyref.roll) ) {
	goto pil_error;
      }
    } else {
      anl_msg_error("\
%s: unknown pointing=%s\n", pname, com.pointing);
      goto quit;
    }
    if ( PILGetBool(k="aberration", &com.aberration) ||
	 PILGetInt (k="rand_seed", &com.rand_seed) ||
	 PILGetReal(k="rand_skip", &com.rand_skip) ||
	 PILGetInt (k="blank", &com.blank) ) {
      goto pil_error;
    }

    *status = ANL_OK;
    return;

 pil_error:
    anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
 quit:
    *status = ANL_QUIT;
    return;
  }

  for (;;) {
    char *p;
    CMinquir(pname, NVAL, names, help, nreply, answer);
    p = names[answer[1]-1];
    if ( 0 == strcmp("Show", p) ) {
      show_parameter();
    } else if ( 0 == strcmp("TELDEF", p) ) {
      CLtxtrd(p, com.teldef_file, sizeof(com.teldef_file));
    } else if ( 0 == strcmp("ATTITUDE", p) ) {
      CLtxtrd(p, com.attitude_file, sizeof(com.attitude_file));
      if ( 0 == CLstricmp("EULER", com.attitude_file) ) {
	com.attitude_type = ATTITUDE_EULER;
      } else {
	com.attitude_type = ATTITUDE_FILE;
      }
    } else if ( 0 == strcmp("POINTING", p) ) {
      CLtxtrd(p, com.pointing, sizeof(com.pointing));
      if ( 0 == CLstricmp("KEY", com.pointing) ) {
	com.pointing_type = POINTING_KEY;
      } else if ( CLstricmp("USER", com.pointing) ) {
	com.pointing_type = POINTING_USER;
      } else {
	anl_msg_warning("\
%s: WARNING: unknown pointing=%s\n", pname, com.pointing);
      }
    } else if ( 0 == strcmp("EULER", p) ) {
      CLfdprd("EULER  phi  (deg)", &com.ea1);
      CLfdprd("EULER theta (deg)", &com.ea2);
      CLfdprd("EULER  psi  (deg)", &com.ea3);
      com.ea.phi = com.ea1 * DEG2RAD;
      com.ea.theta = com.ea2 * DEG2RAD;
      com.ea.psi = com.ea3 * DEG2RAD;
    } else if ( 0 == strcmp("SKYREF", p) ) {
      CLfdprd("SKYREF alpha (deg)", &com.skyref.alpha);
      CLfdprd("SKYREF delta (deg)", &com.skyref.delta);
      CLfdprd("SKYREF roll  (deg)", &com.skyref.roll);
    } else if ( 0 == strcmp("ABERRATION", p) ) {
      CLlogrd(p, &com.aberration);
    } else if ( 0 == strcmp("RAND_SEED", p) ) {
      CLintrd(p, &com.rand_seed);
    } else if ( 0 == strcmp("RAND_SKIP", p) ) {
      CLfdprd(p, &com.rand_skip);
    } else if ( 0 == strcmp("Exit", p) ) {
      break;
    }
  }

#undef NVAL
  *status = ANL_OK;
}

void
XIScoord_init(int *status)
{
  int size;
  char *telescop, o_telescop[FLEN_VALUE];
  char *instrume, o_instrume[FLEN_VALUE];

  CALDB_INFO caldb;

  EvsDef("XIScoord:BEGIN");
  EvsDef("XIScoord:ENTRY");
  EvsDef("XIScoord:OK");

  BnkGet("XIS:TELESCOP", sizeof(o_telescop), &size, o_telescop);
  BnkGet("XIS:INSTRUME", sizeof(o_instrume), &size, o_instrume);
  telescop = o_telescop + 1;	/* remove beginning "'" */
  instrume = o_instrume + 1;	/* remove beginning "'" */

  if ( 0 == CLstricmp("CALDB", com.o_teldef_file) ) {
    aste_caldb_init(&caldb);
    caldb.telescop = telescop;
    caldb.instrume = instrume;
    caldb.codename = "TELDEF";
    aste_caldb_get(&caldb);
    if ( 0 != caldb.status || 0 == caldb.nfound ) {
      anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
		    pname, caldb.codename, caldb.status);
      goto quit;
    } else if ( 1 != caldb.nfound ) {
      anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
		      pname, caldb.nfound, caldb.codename);
    }
    com.teldef_file = caldb.filename;
  }

  com.teldef = aste_coord_init(NULL, instrume, com.teldef_file);
  if ( NULL == com.teldef ) {
    anl_msg_error("\
%s: aste_coord_init('%s') failed\n", pname, com.teldef_file);
    goto quit;
  }

  if ( ATTITUDE_FILE == com.attitude_type ) {
    com.attfile = aste_att_init(com.attitude_file);
    if ( NULL == com.attfile ) {
      anl_msg_error("\
%s: could not find attfile '%s'\n", pname, com.attitude_file);
      goto quit;
    }

  } else {
    com.attfile = NULL;
  }

  /* pointing=ATT の廃止 */
  if ( POINTING_KEY == com.pointing_type ) {
    BnkGet("XIS:RA_NOM", sizeof(com.skyref.alpha), &size, &com.skyref.alpha);
    BnkGet("XIS:DEC_NOM", sizeof(com.skyref.delta), &size, &com.skyref.delta);
    com.skyref.roll = 0.0;
  }

  aste_rndtsini(com.rand_seed);
  aste_drndtsn_skipd(com.rand_skip);

  show_parameter();

  /* warning when teldef file = "none" */
  if ( 0 == CLstricmp("none", com.teldef_file) ) {
      anl_msg_warning("\
%s: WARNING: built-in teldef file is assumed\n", pname);
   }

  /* show mode information */

  BnkGet("XIS:EDITMODE", sizeof(int), &size, &editmode);
  BnkGet("XIS:WINOPT", sizeof(int), &size, &winopt);
  BnkGet("XIS:WIN_ST", sizeof(int), &size, &win_st);
  BnkGet("XIS:MJDREFI", sizeof(int), &size, &mjdrefi);
  BnkGet("XIS:MJDREFF", sizeof(double), &size, &mjdreff);

  anl_msg_info("\
EDITMODE = %d\n\
WINOPT   = %d\n\
WIN_ST   = %d\n", editmode, winopt, win_st);

  *status = ANL_OK;
  return;

quit:
  *status = ANL_QUIT;
  return;
}

void
XIScoord_his(int *status)
{
  *status = ANL_OK;
}

void
XIScoord_bgnrun(int *status)
{
  char *k;
  fitsfile *fp;
  AtEulerAng ea;
  char buf[PIL_LINESIZE+80];
  int col, tlmin, tlmax;
  double alpha, delta, tcrpx, tcrvl, tcdlt, optic, optx_ch, opty_ch;

  TELDEF_ASTROE *p = com.teldef->mission.aste;
  int istat = 0;
  int used = 0;

  EvsfSetM("XIScoord:BEGIN");

  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) ) {
    *status = ANL_OK;
    return;
  }

  sprintf(buf, "  teldef='%s'%s", com.teldef_file,
	  (com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
  fits_write_history(fp, buf, &istat);
  if ( ATTITUDE_EULER == com.attitude_type ) {
    sprintf(buf, "  attitude=EULER  ea=(%.4f, %.4f, %.4f)",
	    com.ea1, com.ea2, com.ea3);
  } else {
    sprintf(buf, "  attitude='%s'", com.attitude_file);
  }
  fits_write_history(fp, buf, &istat);
  sprintf(buf, "  pointing=%s  skyref=(%.4f, %.4f, %.1f)",
	  com.pointing, com.skyref.alpha, com.skyref.delta, com.skyref.roll);
  fits_write_history(fp, buf, &istat);
  sprintf(buf, "  aberration=%s  rand_seed=%d  rand_skip=%.0f  blank=%d",
	  com.aberration ? "yes" : "no",
	  com.rand_seed, com.rand_skip, com.blank);
  fits_write_history(fp, buf, &istat);

  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    goto quit;
  }

  if (
      fits_read_key_dbl(fp, k="MEAN_EA1", &ea.phi, NULL, &istat) ||
      fits_read_key_dbl(fp, k="MEAN_EA2", &ea.theta, NULL, &istat) ||
      fits_read_key_dbl(fp, k="MEAN_EA3", &ea.psi, NULL, &istat) ||
      0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  ea.phi *= DEG2RAD;
  ea.theta *= DEG2RAD;
  ea.psi *= DEG2RAD;

  anl_msg_info("\n\
%s: updating header keywords ...\n\n", pname);

  aste_det2ecs(com.teldef, &ea, p->det.xcen, p->det.ycen, &alpha, &delta);
  if ( fits_get_colnum(fp, CASESEN, "DETX", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->det.xpix1;
    tlmax = p->det.xpix1 + p->det.xsiz - 1;
    tcrpx = p->det.xcen;
    tcrvl = 0.0;
    tcdlt = p->det.xscl;
    optic = p->optaxisx;
    if ( modkeys(fp, "DETX", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  if ( fits_get_colnum(fp, CASESEN, "DETY", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->det.ypix1;
    tlmax = p->det.ypix1 + p->det.ysiz - 1;
    tcrpx = p->det.ycen;
    tcrvl = 0.0;
    tcdlt = p->det.yscl;
    optic = p->optaxisy;
    if ( modkeys(fp, "DETY", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  aste_foc2ecs(com.teldef, &ea, p->foc.xcen, p->foc.ycen, &alpha, &delta);
  aste_det2foc(com.teldef, p->optaxisx, p->optaxisy, &optx_ch, &opty_ch);
  if ( fits_get_colnum(fp, CASESEN, "FOCX", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->foc.xpix1;
    tlmax = p->foc.xpix1 + p->foc.xsiz - 1;
    tcrpx = p->foc.xcen;
    tcrvl = 0.0;
    tcdlt = - (p->foc.xscl / p->focallen) * RAD2DEG;
    optic = optx_ch;
    if ( modkeys(fp, "FOCX", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  if ( fits_get_colnum(fp, CASESEN, "FOCY", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->foc.ypix1;
    tlmax = p->foc.ypix1 + p->foc.ysiz - 1;
    tcrpx = p->foc.ycen;
    tcrvl = 0.0;
    tcdlt = (p->foc.yscl / p->focallen) * RAD2DEG;
    optic = opty_ch;
    if ( modkeys(fp, "FOCY", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  aste_det2sky(com.teldef, &ea, &com.skyref, p->optaxisx, p->optaxisy, &optx_ch, &opty_ch);
  if ( fits_get_colnum(fp, CASESEN, "X", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->sky.xpix1;
    tlmax = p->sky.xpix1 + p->sky.xsiz - 1;
    tcrpx = p->sky.xcen;
    tcrvl = com.skyref.alpha;
    tcdlt = - (p->sky.xscl / p->focallen) * RAD2DEG;
    optic = optx_ch;
    if ( modkeys(fp, "X", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  if ( fits_get_colnum(fp, CASESEN, "Y", &col, &istat) ) {
    istat = 0;			/* ignore it */
  } else {
    tlmin = p->sky.ypix1;
    tlmax = p->sky.ypix1 + p->sky.ysiz - 1;
    tcrpx = p->sky.ycen;
    tcrvl = com.skyref.delta;
    tcdlt = (p->sky.yscl / p->focallen) * RAD2DEG;
    optic = opty_ch;
    if ( modkeys(fp, "Y", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic) ) {
      goto quit;
    }
  }
  anl_msg_info("\n");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XIScoord_ana(int nevent, int eventid, int *status)
{
  int size, istat;
  int segment, rawx, rawy;
  int actx, acty, idetx, idety, ifocx, ifocy, iskyx, iskyy;
  int ppux, ppuy;
  double aetime, detx, dety, focx, focy, skyx, skyy;
  AtEulerAng ea;
  double alpha, delta;

  EvsfSetM("XIScoord:ENTRY");

  actx = acty = idetx = idety = ifocx = ifocy = iskyx = iskyy = com.blank;

  /* bank get */
  BnkfGetM("XIS:TIME", sizeof(double), &size, &aetime);
  BnkfGetM("XIS:SEGMENT", sizeof(int), &size, &segment);

  switch ( editmode ) {

  case XISeditDarkInit:
  case XISeditDarkUpdate:

    BnkfGetM("XIS:PPUX", sizeof(int), &size, &ppux);
    BnkfGetM("XIS:PPUY", sizeof(int), &size, &ppuy);

/* ppu2raw */
    rawx = rawy = com.blank;
    if ( xis_ppu2raw(com.teldef, ppux, ppuy, &rawx, &rawy) ) {
      anl_msg_error("\
%s: ERROR in aste_ppu2raw() at t=%.1f\n", pname, aetime);
    }
/* raw2act */
    else if ( xis_raw2act(com.teldef, segment, rawx, rawy, winopt, win_st,
			  &actx, &acty) ) {
      anl_msg_error("\
%s: ERROR in aste_raw2act() at t=%.1f\n", pname, aetime);
    }
/* act2det */
    else if ( xis_act2det(com.teldef, actx, acty, &idetx, &idety) ) {
      anl_msg_error("\
%s: ERROR in aste_act2det() at t=%.1f\n", pname, aetime);
    }

    BnkfPutM("XIS:RAWX", sizeof(int), &rawx);
    BnkfPutM("XIS:RAWY", sizeof(int), &rawy);
    BnkfPutM("XIS:ACTX", sizeof(int), &actx);
    BnkfPutM("XIS:ACTY", sizeof(int), &acty);
    BnkfPutM("XIS:DETX", sizeof(int), &idetx);
    BnkfPutM("XIS:DETY", sizeof(int), &idety);
    goto end;

  case XISeditFrame:
  case XISeditDarkFrame:
    goto end;

  default:
    ;

  }

  BnkfGetM("XIS:RAWX", sizeof(int), &size, &rawx);
  BnkfGetM("XIS:RAWY", sizeof(int), &size, &rawy);

/* When the timing mode, y axes have no meaning */
  if ( XISeditTiming == editmode ) {
    rawy = (XISactiveSegmentVsize - 1) / 2;	/* force ACTY=511 */
  }

/* raw2act */
  if ( xis_raw2act(com.teldef,segment,rawx,rawy,winopt,win_st,&actx,&acty) ) {
    anl_msg_error("\
%s: ERROR in aste_raw2act() at t=%.1f\n", pname, aetime);
    goto skip;
  }

  /* act2det */
  if ( xis_act2det(com.teldef, actx, acty, &idetx, &idety) ) {
    anl_msg_error("\
%s: ERROR in aste_act2det() at t=%.1f\n", pname, aetime);
    goto skip;
  }

  /* H. M. for random number */
  /* ここでdetx, detyにrandom number をたしてしまう */
  detx = (double)idetx + aste_drndts() - 0.5;
  dety = (double)idety + aste_drndts() - 0.5;

  /* det2foc */
  if ( aste_det2foc (com.teldef, detx, dety, &focx, &focy) ) {
    anl_msg_error("\
%s: ERROR in aste_det2foc() at t=%.1f\n", pname, aetime);
    goto skip;
  }

  /* attitude */
  if ( ATTITUDE_EULER == com.attitude_type ) {
    ea = com.ea;
  } else {
    istat = aste_att_ea(com.attfile, aetime, &ea);
    if ( istat ) {
      anl_msg_error("\
%s: ERROR: t=%.1f out of range of attitude\n", pname, aetime);
      goto skip;
    }
  }

  aste_foc2ecs(com.teldef, &ea, focx, focy, &alpha, &delta);
  if ( com.aberration ) {
    aste_cor_aberration(aetime, mjdrefi, mjdreff, &alpha, &delta);
  }
  aste_ecs2sky(com.teldef, &com.skyref, alpha, delta, &skyx, &skyy);

/* FOCX, FOCY is short integer '1I' in XIS event file,
   however it should be within reasonable value if normal teldef is used */
  ifocx = (int)floor(focx + 0.5);
  ifocy = (int)floor(focy + 0.5);

/* X, Y is short integer '1I' in XIS event file,
   so range check is needed to avoid over/underflow */
  if ( skyx + 0.5 <= -32768.0 ) {
    iskyx = -32768;
  } else if ( 32767 <= skyx + 0.5 ) {
    iskyx = 32767;
  } else {
    iskyx = (int)floor(skyx + 0.5);
  }
  if ( skyy + 0.5 <= -32768.0 ) {
    iskyy = -32768;
  } else if ( 32767 <= skyy + 0.5 ) {
    iskyy = 32767;
  } else {
    iskyy = (int)floor(skyy + 0.5);
  }

 skip:

/* bank put */
  BnkfPutM("XIS:ACTX", sizeof(int), &actx);
  BnkfPutM("XIS:ACTY", sizeof(int), &acty);
  BnkfPutM("XIS:DETX", sizeof(int), &idetx);
  BnkfPutM("XIS:DETY", sizeof(int), &idety);
  BnkfPutM("XIS:FOCX", sizeof(int), &ifocx);
  BnkfPutM("XIS:FOCY", sizeof(int), &ifocy);
  BnkfPutM("XIS:X",    sizeof(int), &iskyx);
  BnkfPutM("XIS:Y",    sizeof(int), &iskyy);

 end:

  EvsfSetM("XIScoord:OK");
  *status = ANL_OK;
}

void
XIScoord_endrun(int *status)
{
  *status = ANL_OK;
}

void
XIScoord_exit(int *status)
{
  aste_att_close(com.attfile);

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
