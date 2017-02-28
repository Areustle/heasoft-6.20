#ifndef RWGPS_H
#define RWGPS_H 1

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/sgp.h,v $
 * $Revision: 1.1 $
 * $Date: 2002/02/20 14:39:17 $
 *
 * $Log: sgp.h,v $
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.2  2002/01/28 19:21:32  rwiegand
 * Define some SGP modelling macros in terms of others
 *
 * Revision 1.1  2001/11/01 14:44:47  rwiegand
 * Initial revision
 *
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "tle.h"


/* Common arguments between deep-space functions */
typedef struct
{
  /* Used by dpinit part of sgpdeep() */
  double eosq;
  double sinio;
  double cosio;
  double betao;
  double aodp;
  double theta2;
  double sing;
  double cosg;
  double betao2;
  double xmdot;
  double omgdot;
  double xnodot;
  double xnodp;

  /* Used by dpsec and dpper parts of Deep() */
  double xll;
  double omgadf;
  double xnode;
  double em;
  double xinc;
  double xn;
  double t;

  /* Used by thetg and Deep() */
  double ds50;
} sgpdeep_t;


/* Table of constant values */
#define sgp_pi          3.14159265358979323
#define sgp_twopi       (2. * sgp_pi)
#define sgp_pio2        (sgp_pi / 2.)

#define sgp_ae          1.0
#define sgp_tothrd      (2./3)

/* earth equatorial radius - kolometers (WGS '72) */
#define sgp_xkmper      6378.135
/* earth flattening (WGS '72) */
#define sgp_f           (1. / 298.26)
/* earth gravitational constant (WGS '72) */
#define sgp_ge          398600.8
/* J2 harmonic (WGS '72) */
#define sgp_xj2         1.0826158e-3
/* J3 harmonic (WGS '72) */
#define sgp_xj3         -2.53881e-6
/* J4 harmonic (WGS '72) */
#define sgp_xj4         -1.65597e-6

#define sgp_ck2         (sgp_xj2 / 2.)
#define sgp_ck4         (-3. * sgp_xj4 / 8.)
#define sgp_qo          (sgp_ae * (1. + 120. /sgp_xkmper))
#define sgp_so          78.
#define sgp_s           (sgp_ae * (1. + sgp_so / sgp_xkmper))
#define sgp_e6a         1e-6

#define sgp_de2ra       (sgp_pi / 180.)
#define sgp_ra2de       (180. / sgp_pi)

#define sgp_x3pio2      4.71238898
#define sgp_xke         7.43669161e-2
#define sgp_xmnpda      1440.
#define sgp_secday      86400.

#define sgp_omega_E     1.0027379
#define sgp_omega_ER    6.3003881
#define sgp_zns         1.19459e-5
#define sgp_c1ss        2.9864797e-6
#define sgp_zes         0.01675
#define sgp_znl         1.5835218e-4
#define sgp_c1l         4.7968065e-7
#define sgp_zel         0.05490
#define sgp_zcosis      0.91744867
#define sgp_zsinis      0.39785416
#define sgp_zsings     -0.98088458
#define sgp_zcosgs      0.1945905
#define sgp_zcoshs      1.
#define sgp_zsinhs      0.
#define sgp_q22         1.7891679e-6
#define sgp_q31         2.1460748e-6
#define sgp_q33         2.2123015e-7
#define sgp_g22         5.7686396
#define sgp_g32         0.95240898
#define sgp_g44         1.8014998
#define sgp_g52         1.0508330
#define sgp_g54         4.4108898
#define sgp_root22      1.7891679e-6
#define sgp_root32      3.7393792e-7
#define sgp_root44      7.3636953e-9
#define sgp_root52      1.1428639e-7
#define sgp_root54      2.1765803e-9
#define sgp_thdt        4.3752691e-3
#define sgp_rho         1.5696615e-1


/* sgp.c */
void sgp_sgp (double tsince, const tlesgp_t *tle, int *pflags,
              vector_t *pos, vector_t *vel);
void sgp_sgp4 (double tsince, const tlesgp_t *tle, int *pflags,
              vector_t *pos, vector_t *vel);
void sgp_sgp8 (double tsince, const tlesgp_t *tle, int *pflags,
              vector_t *pos, vector_t *vel);
void sgp_sdp4 (double tsince, const tlesgp_t *tle, int *pflags,
              vector_t *pos, vector_t *vel);
void sgp_sdp8 (double tsince, const tlesgp_t *tle, int *pflags,
              vector_t *pos, vector_t *vel);
void sgp_deep (int ientry, const tlesgp_t *tle, int *pflags,
              sgpdeep_t *deeparg);
double sgp_thetag (double epoch, sgpdeep_t *deeparg);
double sgp_actan (double sinx, double cosx);
double sgp_fmod2p (double x);
double sgp_modulus (double arg1, double arg2);


#endif

