/*
 * $Source: /headas/headas/attitude/tasks/prefilter/sgp.c,v $
 * $Revision: 1.2 $
 * $Date: 2002/01/28 16:05:05 $
 *
 * $Log: sgp.c,v $
 * Revision 1.2  2002/01/28 16:05:05  rwiegand
 * Renamed variable for sgp_deep.  Added whitespace
 *
 * Revision 1.1  2001/11/01 14:45:03  rwiegand
 * Initial revision
 *
 */

/*
 * norad.c  v. 0.1beta 03/17/2001
 * 
 * C source code derived mostly from NORADS's spacetrack report #3
 * FORTRAN routines and Dr. T. Kelso's Pascal ports of same. This
 * file contains SGP, SGP4, SGP8, SDP4 and SDP8 satellite ephemeris
 * routines for the prediction of a satellites position and velocity
 * from its two-line element (tle) set of orbital parameters.
 */

#include "sgp.h"


/* TLE flag definitions */
#define ALL_FLAGS              -1
#define  SGP_INITIALIZED_FLAG  0x0001
#define SGP4_INITIALIZED_FLAG  0x0002
#define SDP4_INITIALIZED_FLAG  0x0004
#define SGP8_INITIALIZED_FLAG  0x0008
#define SDP8_INITIALIZED_FLAG  0x0010
#define SIMPLE_FLAG            0x0020
#define RESONANCE_FLAG         0x0400
#define SYNCHRONOUS_FLAG       0x0800

#define sgp_dpinit      1 /* Deep-space initialization code */
#define sgp_dpsec       2 /* Deep-space secular code        */
#define sgp_dpper       3 /* Deep-space periodic code       */


static int isFlagClear  (int *pflags, int flag);
static int isFlagSet    (int *pflags, int flag);
static void clearFlag   (int *pflags, int flag);
static void setFlag     (int *pflags, int flag);


static double Julian_Date_of_Year (double year);



/* SGP */
void sgp_sgp (double tsince, const tlesgp_t *tle, int *pflags,
         vector_t *pos, vector_t *vel)
{
  static double
    ao, qo, xlo, d1o, d2o, d3o, d4o, omgdt, xnodot, c5, c6;


  double
    temp, rdot, cosu, sinu, cos2u, po2no, sin2u, a, e,
    p, rr, u, ecose, esine, omgas, cosik, cosio, xinck,
    sinik, sinio, a1, c1, c2, c3, c4, d1, axnsl, aynsl,
    sinuk, rvdot, cosuk,dd1, dd2, coseo1, sineo1, pl,
    rk, po, uk, xl, su, ux, uy, uz, vx, vy, vz, pl2,
    xnodek, cosnok, xnodes, el2, eo1, r1, sinnok,
    xls, xmx, xmy, tem2, tem5;

  int i;

  if (isFlagClear(pflags, SGP_INITIALIZED_FLAG))
    {
      setFlag(pflags, SGP_INITIALIZED_FLAG);

      /* Initialization */
      c1 = sgp_ck2 * 1.5;
      c2 = sgp_ck2 / 4.;
      c3 = sgp_ck2 / 2.;
      r1 = sgp_ae;
      c4 = sgp_xj3 * (r1 * (r1 * r1)) / (sgp_ck2 * 4.);
      cosio = cos(tle->xincl);
      sinio = sin(tle->xincl);
      dd1 = (sgp_xke / tle->xno);
      dd2 = sgp_tothrd;
      a1 = pow(dd1, dd2);
      dd1 = (1. - tle->eo * tle->eo);
      d1 = c1 / a1 / a1 * (cosio * 3. *cosio - 1.) / pow(dd1, 1.5);
      ao = a1 * (1. - d1 * (1. / 3.) - d1 * d1
		- d1 * 1.654320987654321 * d1 * d1);
      po = ao * (1. - tle->eo * tle->eo);
      qo = ao * (1. - tle->eo);
      xlo = tle->xmo + tle->omegao + tle->xnodeo;
      d1o = c3 * sinio * sinio;
      d2o = c2 * (cosio * 7. * cosio - 1.); 
      d3o = c1 * cosio;
      d4o = d3o * sinio;
      po2no = tle->xno / (po * po);
      omgdt = c1 * po2no * (cosio * 5. * cosio - 1.);
      xnodot = d3o * -2. * po2no;
      c5 = c4 * .5 * sinio * (cosio * 5. + 3.) / (cosio + 1.);
      c6 = c4 * sinio;
    } /* End of SGP() initialization */

  /* Update for secular gravity and atmospheric drag */
  a = tle->xno + (tle->xndt2o * 2. + tle->xndd6o * 3. * tsince) * tsince;
  dd1 = tle->xno / a;
  dd2 = sgp_tothrd;
  a = ao * pow(dd1, dd2);
  e = sgp_e6a;
  if (a > qo) e = 1. - qo / a;
  p = a * (1. - e * e);
  xnodes = tle->xnodeo + xnodot * tsince;
  omgas = tle->omegao + omgdt * tsince;
  r1 = xlo + (tle->xno + omgdt + xnodot
		+ (tle->xndt2o + tle->xndd6o * tsince) * tsince) * tsince;
  xls = sgp_fmod2p(r1);

  /* Long period periodics */
  axnsl = e * cos(omgas);
  aynsl = e * sin(omgas) - c6 / p;
  r1 = xls - c5 / p * axnsl;
  xl = sgp_fmod2p(r1);

  /* Solve Kepler's equation */
  r1 = xl - xnodes;
  u = sgp_fmod2p(r1);
  eo1 = u;
  tem5 = 1.;

  i = 0;
  do
    {
      sineo1 = sin(eo1);
      coseo1 = cos(eo1);
      if (fabs(tem5) < sgp_e6a) break;
      tem5 = 1. - coseo1 * axnsl - sineo1 * aynsl;
      tem5 = (u - aynsl * coseo1 + axnsl * sineo1 - eo1) / tem5;
      tem2 = fabs(tem5);
      if (tem2 > 1.) tem5 = tem2 / tem5;
      eo1 += tem5;
    }
  while (i++ < 10);

  /* Short period preliminary quantities */
  ecose = axnsl * coseo1 + aynsl * sineo1;
  esine = axnsl * sineo1 - aynsl * coseo1;
  el2 = axnsl * axnsl + aynsl * aynsl;
  pl = a * (1. - el2);
  pl2 = pl * pl;
  rr = a * (1. - ecose);
  rdot = sgp_xke * sqrt(a) / rr * esine;
  rvdot = sgp_xke * sqrt(pl) / rr;
  temp = esine / (sqrt(1. - el2) + 1.);
  sinu = a / rr * (sineo1 - aynsl - axnsl * temp);
  cosu = a / rr * (coseo1 - axnsl + aynsl * temp);
  su = sgp_actan(sinu, cosu);

  /* Update for short periodics */
  sin2u = (cosu + cosu) * sinu;
  cos2u = 1. - 2. * sinu * sinu;
  rk = rr + d1o / pl * cos2u;
  uk = su - d2o / pl2 * sin2u;
  xnodek = xnodes + d3o * sin2u / pl2;
  xinck = tle->xincl + d4o / pl2 * cos2u;

  /* Orientation vectors */
  sinuk = sin(uk);
  cosuk = cos(uk);
  sinnok = sin(xnodek);
  cosnok = cos(xnodek);
  sinik = sin(xinck);
  cosik = cos(xinck);
  xmx = -sinnok * cosik;
  xmy = cosnok * cosik;
  ux = xmx * sinuk + cosnok * cosuk;
  uy = xmy * sinuk + sinnok * cosuk;
  uz = sinik * sinuk;
  vx = xmx * cosuk - cosnok * sinuk;
  vy = xmy * cosuk - sinnok * sinuk;
  vz = sinik * cosuk;

  /* Position and velocity */
  pos->scale = sgp_xkmper;
  pos->x = rk * ux;
  pos->y = rk * uy;
  pos->z = rk * uz;

  vel->scale = sgp_xkmper / 60.;
  vel->x = rdot * ux;
  vel->y = rdot * uy;
  vel->z = rdot * uz;
  vel->x = rvdot * vx + vel->x;
  vel->y = rvdot * vy + vel->y;
  vel->z = rvdot * vz + vel->z;

} /* sgp_sgp */

/*------------------------------------------------------------------*/

/* SGP4 */
void sgp_sgp4 (double tsince, const tlesgp_t *tle, int *pflags,
          vector_t *pos, vector_t *vel)
{
  static double
    aodp, aycof, c1, c4, c5, cosio, d2, d3, d4, delmo, eta, omgcof,
    omgdot, qoms2t, sinio, xnodp, sinmo, t2cof, t3cof, t4cof, t5cof,
    x1mth2, x3thm1, x7thm1, xmcof, xmdot, xnodcf, xnodot, xlcof;
 
  double
    cosuk, sinuk, rfdotk, vx, vy, vz, ux, uy, uz, xmy, xmx,
    cosnok, sinnok, cosik, sinik, rdotk, xinck, xnodek, uk,
    rk, cos2u, sin2u, u, sinu, cosu, betal, rfdot, rdot, r, pl,
    elsq, esine, ecose, epw, cosepw, x1m5th, xhdot1, tfour,
    sinepw, capu, ayn, xlt, aynl, xll, axn, xn, beta, xl, e, a,
    tcube, delm, delomg, templ, tempe, tempa, xnode, tsq, xmp,
    omega, xnoddf, omgadf, xmdf, a1, a3ovk2, ao, betao, betao2,
    c1sq, c2, c3, coef, coef1, del1, delo, eeta, eosq, etasq,
    perige, pinvsq, psisq, qoms24, s4, temp, temp1, temp2,
    temp3, temp4, temp5, temp6, theta2, theta4, tsi;

  int i;  

  /* Initialization */
  if (isFlagClear(pflags, SGP4_INITIALIZED_FLAG))
    {
      setFlag(pflags, SGP4_INITIALIZED_FLAG);

      /* Recover original mean motion (xnodp) and   */
      /* semimajor axis (aodp) from input elements. */
      a1 = pow(sgp_xke / tle->xno, sgp_tothrd);
      cosio = cos(tle->xincl);
      theta2 = cosio * cosio;
      x3thm1 = 3. * theta2 - 1.;
      eosq = tle->eo * tle->eo;
      betao2 = 1. - eosq;
      betao = sqrt(betao2);
      del1 = 1.5 * sgp_ck2 * x3thm1 / (a1 * a1 * betao * betao2);
      ao = a1 * (1. - del1 * (0.5 * sgp_tothrd
              + del1 * (1. + 134. / 81. * del1)));
      delo = 1.5 * sgp_ck2 * x3thm1 / (ao * ao * betao * betao2);
      xnodp = tle->xno / (1. + delo);
      aodp = ao / (1. - delo);

      /* For perigee less than 220 kilometers, the "simple" flag is set */
      /* and the equations are truncated to linear variation in sqrt a  */
      /* and quadratic variation in mean anomaly.  Also, the c3 term,   */
      /* the delta omega term, and the delta m term are dropped.        */
      if ((aodp * (1. - tle->eo) / sgp_ae) < (220. / sgp_xkmper + sgp_ae))
	setFlag(pflags, SIMPLE_FLAG);
      else
	clearFlag(pflags, SIMPLE_FLAG);

      /* For perigee below 156 km, the       */ 
      /* values of s and qoms2t are altered. */
      s4 = sgp_s;
      qoms2t = pow(sgp_qo - sgp_s, 4.);
      qoms24 = qoms2t;
      perige = (aodp * (1. - tle->eo) - sgp_ae) * sgp_xkmper;
      if (perige < 156.)
	{
       	  if(perige <= 98.)
	    s4 = 20.;
          else
	    s4 = perige - 78.;
	  qoms24 = pow((120. - s4) * sgp_ae / sgp_xkmper, 4.);
	  s4 = s4 / sgp_xkmper + sgp_ae;
	}

      pinvsq = 1. / (aodp * aodp * betao2 * betao2);
      tsi = 1. / (aodp - s4);
      eta = aodp * tle->eo * tsi;
      etasq = eta * eta;
      eeta = tle->eo * eta;
      psisq = fabs(1. - etasq);
      coef = qoms24 * pow(tsi, 4.);
      coef1 = coef / pow(psisq, 3.5);
      c2 = coef1 * xnodp * (aodp * (1. + 1.5 * etasq + eeta * (4. + etasq))
           + 0.75 * sgp_ck2 * tsi / psisq * x3thm1
               * (8. + 3. * etasq * (8. + etasq)));
      c1 = tle->bstar * c2;
      sinio = sin(tle->xincl);
      a3ovk2 = -sgp_xj3 / sgp_ck2 * pow(sgp_ae, 3.);
      c3 = coef * tsi * a3ovk2 * xnodp * sgp_ae * sinio / tle->eo;
      x1mth2 = 1. - theta2;
      c4 = 2. * xnodp * coef1 * aodp * betao2 *
           (eta * (2 + 0.5 * etasq) + tle->eo * (0.5 + 2. * etasq)
                - 2. * sgp_ck2 * tsi / (aodp * psisq)
                    * (-3 * x3thm1 * (1 - 2 * eeta + etasq * (1.5 - .5 * eeta))
                        + 0.75 * x1mth2 * (2 * etasq
                            - eeta * (1 + etasq)) * cos(2 * tle->omegao)));
      c5 = 2. * coef1 * aodp * betao2
              * (1. + 2.75 * (etasq + eeta) + eeta * etasq);
      theta4 = theta2 * theta2;
      temp1 = 3 * sgp_ck2 * pinvsq * xnodp;
      temp2 = temp1 * sgp_ck2 * pinvsq;
      temp3 = 1.25 * sgp_ck4 * pinvsq * pinvsq * xnodp;
      xmdot = xnodp + 0.5 * temp1 * betao * x3thm1
              + 0.0625 * temp2 * betao * (13. - 78. * theta2 + 137. * theta4);
      x1m5th =  1. - 5. * theta2;
      omgdot = -0.5 * temp1 * x1m5th
               + 0.0625 * temp2 * (7. - 114. * theta2 + 395. * theta4)
               + temp3 * (3. - 36. * theta2 + 49. * theta4);
      xhdot1 = -temp1 * cosio;
      xnodot = xhdot1 + (0.5 * temp2 * (4 - 19 * theta2)
                   + 2 * temp3 * (3 - 7 * theta2)) * cosio;
      omgcof = tle->bstar * c3 * cos(tle->omegao);
      xmcof = -sgp_tothrd * coef * tle->bstar *sgp_ae / eeta;
      xnodcf = 3.5 * betao2 * xhdot1 * c1;
      t2cof = 1.5 * c1;
      xlcof = 0.125 * a3ovk2 * sinio * (3 + 5 * cosio) / (1 + cosio);
      aycof = 0.25 * a3ovk2 * sinio;
      delmo = pow(1 + eta * cos(tle->xmo), 3.);
      sinmo = sin(tle->xmo);
      x7thm1 = 7 * theta2 - 1;

      if (isFlagClear(pflags, SIMPLE_FLAG))
	{
	  c1sq = c1 * c1;
	  d2 = 4 * aodp * tsi * c1sq;
	  temp = d2 * tsi * c1 / 3;
	  d3 = (17 * aodp + s4) * temp;
	  d4 = 0.5 * temp * aodp * tsi * (221 * aodp + 31 * s4) * c1;
	  t3cof = d2 + 2 * c1sq;
	  t4cof = 0.25 * (3 * d3 + c1 * (12 * d2 + 10 * c1sq));
	  t5cof = 0.2 * (3 * d4 + 12 * c1 * d3 + 6 * d2 * d2
                  + 15 * c1sq * (2 * d2 + c1sq));
	} /* End of if (isFlagClear(pflags, SIMPLE_FLAG)) */
    } /* End of SGP4() initialization */

  /* Update for secular gravity and atmospheric drag. */
  xmdf = tle->xmo + xmdot * tsince;
  omgadf = tle->omegao + omgdot * tsince;
  xnoddf = tle->xnodeo + xnodot * tsince;
  omega = omgadf;
  xmp = xmdf;
  tsq = tsince * tsince;
  xnode = xnoddf + xnodcf * tsq;
  tempa = 1 - c1 * tsince;
  tempe = tle->bstar * c4 * tsince;
  templ = t2cof * tsq;
  if (isFlagClear(pflags, SIMPLE_FLAG))
    {
      delomg = omgcof * tsince;
      delm = xmcof * (pow(1. + eta * cos(xmdf), 3.) - delmo);
      temp = delomg + delm;
      xmp = xmdf + temp;
      omega = omgadf - temp;
      tcube = tsq * tsince;
      tfour = tsince * tcube;
      tempa = tempa - d2 * tsq - d3 * tcube - d4 * tfour;
      tempe = tempe + tle->bstar * c5 * (sin(xmp) - sinmo);
      templ = templ + t3cof * tcube + tfour * (t4cof + tsince * t5cof);
    } /* End of if (isFlagClear(pflags, SIMPLE_FLAG)) */

  a = aodp * pow(tempa, 2.);
  e = tle->eo - tempe;
  xl = xmp + omega + xnode + xnodp * templ;
  beta = sqrt(1 - e * e);
  xn = sgp_xke / pow(a, 1.5);

  /* Long period periodics */
  axn = e * cos(omega);
  temp = 1 / (a * beta * beta);
  xll = temp * xlcof * axn;
  aynl = temp * aycof;
  xlt = xl + xll;
  ayn = e * sin(omega) + aynl;

  /* Solve Kepler's' Equation */
  capu = sgp_fmod2p(xlt - xnode);
  temp2 = capu;

  for (i = 0; i < 10; ++i)
    {
      sinepw = sin(temp2);
      cosepw = cos(temp2);
      temp3 = axn * sinepw;
      temp4 = ayn * cosepw;
      temp5 = axn * cosepw;
      temp6 = ayn * sinepw;
      epw = (capu - temp4 + temp3 - temp2) / (1. - temp5 - temp6) + temp2;
      if (fabs(epw - temp2) <= sgp_e6a)
        break;
      temp2 = epw;
    }

  /* Short period preliminary quantities */
  ecose = temp5 + temp6;
  esine = temp3 - temp4;
  elsq = axn * axn + ayn * ayn;
  temp = 1 - elsq;
  pl = a * temp;
  r = a * (1 - ecose);
  temp1 = 1 / r;
  rdot = sgp_xke * sqrt(a) * esine * temp1;
  rfdot = sgp_xke * sqrt(pl) * temp1;
  temp2 = a * temp1;
  betal = sqrt(temp);
  temp3 = 1 / (1 + betal);
  cosu = temp2 * (cosepw - axn + ayn * esine * temp3);
  sinu = temp2 * (sinepw - ayn - axn * esine * temp3);
  u = sgp_actan(sinu, cosu);
  sin2u = 2 * sinu * cosu;
  cos2u = 2 * cosu * cosu - 1;
  temp = 1 / pl;
  temp1 = sgp_ck2 * temp;
  temp2 = temp1 * temp;

  /* Update for short periodics */
  rk = r * (1 - 1.5 * temp2 * betal * x3thm1) + 0.5 * temp1 * x1mth2 * cos2u;
  uk = u - 0.25 * temp2 * x7thm1 * sin2u;
  xnodek = xnode + 1.5 * temp2 * cosio * sin2u;
  xinck = tle->xincl + 1.5 * temp2 * cosio * sinio * cos2u;
  rdotk = rdot - xn * temp1 * x1mth2 * sin2u;
  rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + 1.5 * x3thm1);

  /* Orientation vectors */
  sinuk = sin(uk);
  cosuk = cos(uk);
  sinik = sin(xinck);
  cosik = cos(xinck);
  sinnok = sin(xnodek);
  cosnok = cos(xnodek);
  xmx = -sinnok * cosik;
  xmy = cosnok * cosik;
  ux = xmx * sinuk + cosnok * cosuk;
  uy = xmy * sinuk + sinnok * cosuk;
  uz = sinik * sinuk;
  vx = xmx * cosuk - cosnok * sinuk;
  vy = xmy * cosuk - sinnok * sinuk;
  vz = sinik * cosuk;

  /* Position and velocity */
  pos->scale = sgp_xkmper;
  pos->x = rk * ux;
  pos->y = rk * uy;
  pos->z = rk * uz;

  vel->scale = sgp_xkmper / 60.;
  vel->x = rdotk * ux + rfdotk * vx;
  vel->y = rdotk * uy + rfdotk * vy;
  vel->z = rdotk * uz + rfdotk * vz;

} /*SGP4*/

/*------------------------------------------------------------------*/

/* SGP8 */
void sgp_sgp8 (double tsince, const tlesgp_t *tle, int *pflags,
         vector_t *pos, vector_t *vel)
{
  static double
    cosi, theta2, tthmun, xnodp, sini, sinio2, cosio2, unm5th,
    unmth2, a3cof, xmdt1, xgdt1, xhdt1, xlldot, xnodot, xndt,
    pp, gamma, xnd, qq, ed, ovgpp, edot, omgdt, qoms2t;

  double cape, beta, eeta, csfg, delo, aodp, diwc, cose, cosg,
    sine, snfg, sing, etdt, xmam, axnm, temp, aynm, eosq, aovr,
    rdot, d1ddt, cos2g, sin2g, temp1, r1, rr2, psim2, b, c0dtc0,
    c1dtc1, rr, cs2f2g, betao, eddot, dd1, dd2, ecosf, sn2f2g,
    pom2, xlamb, etddt, a1, b1, b2, d1, d2, d3, d5, d4, b3, c0,
    c1, c4, c5, xndtn, d6, d7, d8, c8, c9, d9, alpha2, xnddt,
    beta2m, z1, betao2, sinos, cosos, g1, g2, z7, g3, g4, g5,
    y4, y5, theta4, rvdot, pardt1, pardt2, pardt4, sni2du,
    d10, d20, d11, d12, d13, d14, d15, d16, ao, d25, d17, d18,
    d19, d23, em, am, fm, pm, g10, po, aldtal, rm, g13, g14, xn,
    dr, di, cslamb, snlamb, ux, vx, uy, vy, uz, vz, omgasm,
    tmnddt, xnodes, tsddts, psdtps, zc2, zc5, xntrdt,
    tsdtts, eta, csf, snf, tsi, del1, eta2, d1dt, d2dt,
    d3dt, d4dt, d5dt, c4dt, c5dt;

  int i;

  if (isFlagClear(pflags, SGP8_INITIALIZED_FLAG))
    {
      setFlag(pflags, SGP8_INITIALIZED_FLAG);

      /* Recover original mean motion (xnodp) and semimajor   */
      /* axis (aodp) from input elements. Calculate ballistic */
      /* coefficient (b term) from input b* drag term         */
      dd1 = (sgp_xke / tle->xno);
      dd2 = sgp_tothrd;
      a1 = pow(dd1, dd2);
      cosi = cos(tle->xincl);
      theta2 = cosi * cosi;
      tthmun = theta2 * 3. - 1.;
      eosq = tle->eo * tle->eo;
      betao2 = 1. - eosq;
      betao = sqrt(betao2);
      del1 = sgp_ck2 * 1.5 * tthmun / (a1 * a1 * betao * betao2);
      ao = a1 * (1. - del1 * (sgp_tothrd * .5 +
	   del1 * (del1 * 1.654320987654321 + 1.)));
      delo = sgp_ck2 * 1.5 * tthmun / (ao * ao * betao * betao2);
      aodp = ao / (1. - delo);
      xnodp = tle->xno / (delo + 1.);
      b = tle->bstar * 2. / sgp_rho;

      /* Initialization */
      po = aodp * betao2;
      pom2 = 1. / (po * po);
      sini = sin(tle->xincl);
      sing = sin(tle->omegao);
      cosg = cos(tle->omegao);
      temp = tle->xincl * .5;
      sinio2 = sin(temp);
      cosio2 = cos(temp);
      r1 = theta2;
      theta4 = r1 * r1;
      unm5th = 1. - theta2 * 5.;
      unmth2 = 1. - theta2;
      r1 = sgp_ae;
      a3cof = -sgp_xj3 / sgp_ck2 * (r1 * (r1 * r1));
      pardt1 = sgp_ck2 * 3. * pom2 * xnodp;
      pardt2 = pardt1 * sgp_ck2 * pom2;
      pardt4 = sgp_ck4 * 1.25 * pom2 * pom2 * xnodp;
      xmdt1 = pardt1 * .5 * betao * tthmun;
      xgdt1 = pardt1 * - .5 * unm5th;
      xhdt1 = -pardt1 * cosi;
      xlldot = xnodp + xmdt1 + pardt2 * .0625 * betao *
	       (13. - theta2 * 78. + theta4 * 137.);
      omgdt = xgdt1 + pardt2 * .0625 * (7. - theta2 * 114. + theta4 * 395.)
		+ pardt4 * (3. - theta2 * 36. + theta4 * 49.);
      xnodot = xhdt1 + (pardt2 * .5 * (4. - theta2 * 19.) +
	       pardt4 * 2. * (3. - theta2 * 7.)) * cosi;
      tsi = 1. / (po - sgp_s);
      eta = tle->eo * sgp_s * tsi;
      r1 = eta;
      eta2 = r1 * r1;
      psim2 = (r1 = 1. / (1. - eta2), fabs(r1));
      alpha2 = eosq + 1.;
      eeta = tle->eo * eta;
      r1 = cosg;
      cos2g = r1 * r1 * 2. - 1.;
      d5 = tsi * psim2;
      d1 = d5 / po;
      d2 = eta2 * (eta2 * 4.5 + 36.) + 12.;
      d3 = eta2 * (eta2 * 2.5 + 15.);
      d4 = eta * (eta2 * 3.75 + 5.);
      b1 = sgp_ck2 * tthmun;
      b2 = -sgp_ck2 * unmth2;
      b3 = a3cof * sini;
      r1 = tsi, r1 *= r1;
      dd1 = psim2;
      qoms2t = pow(sgp_qo - sgp_s, 4);
      c0 = b * .5 * sgp_rho * qoms2t * xnodp * aodp * (r1 * r1) *
	pow(dd1, 3.5) / sqrt(alpha2);
      r1 = alpha2;
      c1 = xnodp * 1.5 * (r1 * r1) * c0;
      c4 = d1 * d3 * b2;
      c5 = d5 * d4 * b3;
      xndt = c1 * (eta2 * (eosq * 34. + 3.) + 2. + eeta * 5. * (eta2 + 4.) 
	     + eosq * 8.5 + d1 * d2 * b1 + c4 * cos2g + c5 * sing);
      xndtn = xndt / xnodp;

      /* If drag is very small, the isimp flag is set and the */
      /* equations are truncated to linear variation in mean  */
      /* motion and quadratic variation in mean anomaly       */
      r1 = xndtn * sgp_xmnpda;
      if (fabs(r1) > .00216)
	{
	  clearFlag(pflags, SIMPLE_FLAG);
	  d6 = eta * (eta2 * 22.5 + 30.);
	  d7 = eta * (eta2 * 12.5 + 5.);
	  d8 = eta2 * (eta2 + 6.75) + 1.;
	  c8 = d1 * d7 * b2;
	  c9 = d5 * d8 * b3;
	  edot = -c0 * (eta * (eta2 + 4. + eosq * (eta2 * 7. + 15.5))
		+ tle->eo * (eta2 * 15. + 5.) + d1 * d6 * b1
		+ c8 * cos2g + c9 * sing);
	  d20 = sgp_tothrd * .5 * xndtn;
	  aldtal = tle->eo * edot / alpha2;
	  tsdtts = aodp * 2. * tsi * (d20 * betao2 + tle->eo * edot);
	  etdt = (edot + tle->eo * tsdtts) * tsi * sgp_s;
	  psdtps = -eta * etdt * psim2;
	  sin2g = sing * 2. * cosg;
	  c0dtc0 = d20 + tsdtts * 4. - aldtal - psdtps * 7.;
	  c1dtc1 = xndtn + aldtal * 4. + c0dtc0;
	  d9 = eta * (eosq * 68. + 6.) + tle->eo * (eta2 * 15. + 20.);
	  d10 = eta * 5. * (eta2 + 4.) + tle->eo * (eta2 * 68. + 17.);
	  d11 = eta * (eta2 * 18. + 72.);
	  d12 = eta * (eta2 * 10. + 30.);
	  d13 = eta2 * 11.25 + 5.;
	  d14 = tsdtts - psdtps * 2.;
	  d15 = (d20 + tle->eo * edot / betao2) * 2.;
	  d1dt = d1 * (d14 + d15);
	  d2dt = etdt * d11;
	  d3dt = etdt * d12;
	  d4dt = etdt * d13;
	  d5dt = d5 * d14;
	  c4dt = b2 * (d1dt * d3 + d1 * d3dt);
	  c5dt = b3 * (d5dt * d4 + d5 * d4dt);
	  d16 = d9 * etdt + d10 * edot + b1 * (d1dt * d2 + d1 * d2dt)
		+ c4dt * cos2g + c5dt * sing
		+ xgdt1 * (c5 * cosg - c4 * 2. * sin2g);
	  xnddt = c1dtc1 * xndt + c1 * d16;
	  eddot = c0dtc0 * edot - c0 * ((eta2 * 3. + 4. + eeta * 30.
		+ eosq * (eta2 * 21. + 15.5)) * etdt + (eta2 * 15. + 5.
		+ eeta * (eta2 * 14. + 31.)) * edot + b1 * (d1dt * d6 + d1 * etdt *
	         (eta2 * 67.5 + 30.)) + b2 * (d1dt * d7 + d1 * etdt *
		 (eta2 * 37.5 + 5.)) * cos2g + b3 * (d5dt * d8 + d5 * etdt * eta *
		 (eta2 * 4. + 13.5)) * sing + xgdt1 * (c9 * cosg - c8 * 2. * sin2g));
	  r1 = edot;
	  d25 = r1 * r1;
	  r1 = xndtn;
	  d17 = xnddt / xnodp - r1 * r1;
 	  tsddts = tsdtts * 2. * (tsdtts - d20) + aodp * tsi *
	           (sgp_tothrd * betao2 * d17 - d20 * 4. * tle->eo * edot +
	           (d25 + tle->eo * eddot) *2.);
	  etddt = (eddot + edot * 2. * tsdtts) * tsi * sgp_s + tsddts * eta;
	  r1 = tsdtts;
	  d18 = tsddts - r1 * r1;
	  r1 = psdtps;
	  rr2 = psdtps;
	  d19 = -(r1 * r1) / eta2 - eta * etddt * psim2 - rr2 * rr2;
	  d23 = etdt * etdt;
	  d1ddt = d1dt * (d14 + d15) + d1 * (d18 - d19 * 2. + sgp_tothrd * d17 +
		  (alpha2 * d25 / betao2 + tle->eo * eddot) * 2. / betao2);
	  r1  = aldtal;
	  xntrdt = xndt*(sgp_tothrd*2.*d17+(d25+tle->eo*eddot)*3./
	     	   alpha2-r1*r1*6.+d18*4.-d19*7.)+
                   c1dtc1*xnddt+c1*(c1dtc1*d16+d9*etddt+d10*
		   eddot+d23*(eeta*30.+6.+eosq*68.)+etdt*edot*
		   (eta2*30.+40.+eeta*272.)+d25*(eta2*68.+17.)+
		   b1*(d1ddt*d2+d1dt*2.*d2dt+d1*(etddt*d11+d23*
		   (eta2*54.+72.)))+b2*(d1ddt*d3+d1dt*2.*d3dt+d1
		   *(etddt*d12+d23*(eta2*30.+30.)))*cos2g+b3*
		   ((d5dt*d14+d5*(d18-d19*2.))*d4+d4dt*2.*d5dt+
		   d5*(etddt*d13+eta*22.5*d23))*sing+xgdt1*((d20*
		   7.+tle->eo*4.*edot/betao2)*(c5*cosg-c4*2.*
		   sin2g)+(c5dt*2.*cosg-c4dt*4.*sin2g-xgdt1*
		   (c5*sing+c4*4.*cos2g))));
	  tmnddt = xnddt*1e9;  
	  r1 = tmnddt;
	  temp = r1*r1-xndt*1e18*xntrdt;
	  r1 = tmnddt;
	  pp = (temp+r1*r1)/temp;
	  gamma = -xntrdt/(xnddt*(pp-2.));
	  xnd = xndt/(pp*gamma);
	  qq = 1.-eddot/(edot*gamma);
	  ed = edot/(qq*gamma);
	  ovgpp = 1./(gamma*(pp+1.));
	}
      else 
	{
	  setFlag(pflags, SIMPLE_FLAG);
	  edot = -sgp_tothrd*xndtn*(1.-tle->eo);
	}  /* End of if (fabs(r1) > .00216) */
    } /* End of SGP8() initialization */

  /* Update for secular gravity and atmospheric drag */
  r1 = tle->xmo+xlldot*tsince;
  xmam = sgp_fmod2p(r1);
  omgasm = tle->omegao+omgdt*tsince;
  xnodes = tle->xnodeo+xnodot*tsince;
  if(isFlagClear(pflags, SIMPLE_FLAG))
    {
      temp = 1.-gamma*tsince;
      dd1 = temp;
      dd2 = pp;
      temp1 = pow(dd1, dd2);
      xn = xnodp+xnd*(1.-temp1);
      dd1 = temp;
      dd2 = qq;
      em = tle->eo+ed*(1.-pow(dd1, dd2));
      z1 = xnd*(tsince+ovgpp*(temp*temp1-1.));
    }
  else
    {
      xn = xnodp+xndt*tsince;
      em = tle->eo+edot*tsince;
      z1 = xndt*.5*tsince*tsince;
    }  /* if(isFlagClear(pflags, SIMPLE_FLAG)) */

  z7 = sgp_tothrd*3.5*z1/xnodp;
  r1 = xmam+z1+z7*xmdt1;
  xmam = sgp_fmod2p(r1);
  omgasm += z7*xgdt1;
  xnodes += z7*xhdt1;

  /* Solve Kepler's equation */
  zc2 = xmam+em*sin(xmam)*(em*cos(xmam)+1.);

  i = 0;
  do
    {
      sine = sin(zc2);
      cose = cos(zc2);
      zc5 = 1./(1.-em*cose);
      cape = (xmam+em*sine-zc2)*zc5+zc2;
      r1 = cape-zc2;
      if(fabs(r1) <= sgp_e6a) break;
      zc2 = cape;
    }
  while(i++ < 10 );

  /* Short period preliminary quantities */
  dd1 = (sgp_xke/xn);
  dd2 = sgp_tothrd;
  am = pow(dd1, dd2);
  beta2m = 1.-em*em;
  sinos = sin(omgasm);
  cosos = cos(omgasm);
  axnm = em*cosos;
  aynm = em*sinos;
  pm = am*beta2m;
  g1 = 1./pm;
  g2 = sgp_ck2*.5*g1;
  g3 = g2*g1;
  beta = sqrt(beta2m);
  g4 = a3cof*.25*sini;
  g5 = a3cof*.25*g1;
  snf = beta*sine*zc5;
  csf = (cose-em)*zc5;
  fm = sgp_actan(snf, csf);
  snfg = snf*cosos+csf*sinos;
  csfg = csf*cosos-snf*sinos;
  sn2f2g = snfg*2.*csfg;  
  r1 = csfg;
  cs2f2g = r1*r1*2.-1.;
  ecosf = em*csf;
  g10 = fm-xmam+em*snf;
  rm = pm/(ecosf+1.);
  aovr = am/rm;
  g13 = xn*aovr;
  g14 = -g13*aovr;
  dr = g2*(unmth2*cs2f2g-tthmun*3.)-g4*snfg;
  diwc = g3*3.*sini*cs2f2g-g5*aynm;
  di = diwc*cosi;

  /* Update for short period periodics */
  sni2du = sinio2*(g3*((1.-theta2*7.)*.5*sn2f2g-unm5th*3.*g10)-
	   g5*sini*csfg*(ecosf+2.))-g5*.5f*theta2*axnm/cosio2;
  xlamb = fm+omgasm+xnodes+g3*((cosi*6.+1.-theta2*7.)*
	  .5*sn2f2g-(unm5th+cosi*2.)*3.*g10)+g5*sini*
          (cosi*axnm/(cosi+1.)-(ecosf+2.)*csfg);
  y4 = sinio2*snfg+csfg*sni2du+snfg*.5*cosio2*di;
  y5 = sinio2*csfg-snfg*sni2du+csfg*.5*cosio2*di;
  rr = rm+dr;
  rdot = xn*am*em*snf/beta+g14*(g2*2.*unmth2*sn2f2g+g4*csfg);
  r1 = am;
  rvdot = xn*(r1*r1)*beta/rm+g14 *
          dr+am*g13*sini*diwc;

  /* Orientation vectors */
  snlamb = sin(xlamb);
  cslamb = cos(xlamb);
  temp = (y5*snlamb-y4*cslamb)*2.;
  ux = y4*temp+cslamb;
  vx = y5*temp-snlamb;
  temp = (y5*cslamb+y4*snlamb)*2.;
  uy = -y4*temp+snlamb;
  vy = -y5*temp+cslamb;
  temp = sqrt(1.-y4*y4-y5*y5)*2.;
  uz = y4*temp;
  vz = y5*temp;

  /* Position and velocity */
	pos->scale = sgp_xkmper;
  pos->x = rr*ux;
  pos->y = rr*uy;
  pos->z = rr*uz;

	vel->scale = sgp_xkmper / 60.;
  vel->x = rdot*ux+rvdot*vx;
  vel->y = rdot*uy+rvdot*vy;
  vel->z = rdot*uz+rvdot*vz;

} /* SGP8 */

/*------------------------------------------------------------------*/

/* SDP4 */
void sgp_sdp4 (double tsince, const tlesgp_t *tle, int *pflags,
         vector_t *pos, vector_t *vel)
{
  int i;

  static double
    qoms2t,x3thm1,c1,x1mth2,c4,xnodcf,t2cof,xlcof,aycof,x7thm1;

  double
    a,axn,ayn,aynl,beta,betal,capu,cos2u,cosepw,cosik,
    cosnok,cosu,cosuk,ecose,elsq,epw,esine,pl,theta4,
    rdot,rdotk,rfdot,rfdotk,rk,sin2u,sinepw,sinik,
    sinnok,sinu,sinuk,tempe,templ,tsq,u,uk,ux,uy,uz,
    vx,vy,vz,xinck,xl,xlt,xmam,xmdf,xmx,xmy,xnoddf,
    xnodek,xll,a1,a3ovk2,ao,c2,coef,coef1,x1m5th,
    xhdot1,del1,r,delo,eeta,eta,etasq,perige,
    psisq,tsi,qoms24,s4,pinvsq,temp,tempa,temp1,
    temp2,temp3,temp4,temp5,temp6;

  static sgpdeep_t deeparg;

  /* Initialization */
  if (isFlagClear(pflags, SDP4_INITIALIZED_FLAG))
    {
printf("initalizing sdp4\n");
      setFlag(pflags, SDP4_INITIALIZED_FLAG);

      /* Recover original mean motion (xnodp) and   */
      /* semimajor axis (aodp) from input elements. */
      a1 = pow(sgp_xke / tle->xno, sgp_tothrd);
      deeparg.cosio = cos(tle->xincl);
      deeparg.theta2 = deeparg.cosio * deeparg.cosio;
      x3thm1 = 3. * deeparg.theta2 - 1.;
      deeparg.eosq = tle->eo * tle->eo;
      deeparg.betao2 = 1. - deeparg.eosq;
      deeparg.betao = sqrt(deeparg.betao2);
      del1 = 1.5 * sgp_ck2 * x3thm1 /
                (a1 * a1 * deeparg.betao * deeparg.betao2);
      ao = a1 * (1. - del1 * (0.5 * sgp_tothrd +
                      del1 * (1. + 134. / 81. * del1)));
      delo = 1.5 * sgp_ck2 * x3thm1
                / (ao * ao * deeparg.betao * deeparg.betao2);
      deeparg.xnodp = tle->xno / (1. + delo);
      deeparg.aodp = ao / (1. - delo);

      /* For perigee below 156 km, the values */
      /* of s and qoms2t are altered.         */
      s4 = sgp_s;
      qoms2t = pow(sgp_qo - sgp_s, 4.);
      qoms24 = qoms2t;
      perige = (deeparg.aodp * (1 - tle->eo) - sgp_ae) * sgp_xkmper;

      if (perige < 156.)
	{
       	  if (perige <= 98.)
	    s4 = 20.;
          else
	    s4 = perige - 78.;
	  qoms24 = pow((120. - s4) * sgp_ae / sgp_xkmper, 4.);
	  s4 = s4 / sgp_xkmper + sgp_ae;
	}

      pinvsq = 1. / (deeparg.aodp * deeparg.aodp *
               deeparg.betao2 * deeparg.betao2);
      deeparg.sing = sin(tle->omegao);
      deeparg.cosg = cos(tle->omegao);
      tsi = 1. / (deeparg.aodp - s4);
      eta = deeparg.aodp * tle->eo * tsi;
      etasq = eta * eta;
      eeta = tle->eo * eta;
      psisq = fabs(1. - etasq);
      coef = qoms24 * pow(tsi, 4.);
      coef1 = coef / pow(psisq, 3.5);
      c2 = coef1 * deeparg.xnodp *
               (deeparg.aodp * (1. + 1.5 * etasq + eeta * (4. + etasq))
               + 0.75 * sgp_ck2 * tsi / psisq * x3thm1 *
                   (8. + 3. * etasq * (8. + etasq)));
      c1 = tle->bstar * c2;
      deeparg.sinio = sin(tle->xincl);
      a3ovk2 = -sgp_xj3 / sgp_ck2 * pow(sgp_ae, 3.);
      x1mth2 = 1. - deeparg.theta2;

      c4 = 2. * deeparg.xnodp * coef1 * deeparg.aodp * deeparg.betao2 *
           (eta * (2. + 0.5 * etasq) + tle->eo * (0.5 + 2. * etasq)
               - 2. * sgp_ck2 * tsi / (deeparg.aodp * psisq)
               * (-3. * x3thm1 * (1. - 2. * eeta + etasq * (1.5 - 0.5 * eeta))
                   + 0.75 * x1mth2 * (2. * etasq - eeta * (1. + etasq)) *
                       cos(2. * tle->omegao)));
      theta4 = deeparg.theta2 * deeparg.theta2;
      temp1 = 3. * sgp_ck2 * pinvsq * deeparg.xnodp;
      temp2 = temp1 * sgp_ck2 * pinvsq;
      temp3 = 1.25 * sgp_ck4 * pinvsq * pinvsq * deeparg.xnodp;
      deeparg.xmdot = deeparg.xnodp + 0.5 * temp1 * deeparg.betao *
	               x3thm1 + 0.0625 * temp2 * deeparg.betao *
                       (13. - 78. * deeparg.theta2 + 137. * theta4);
      x1m5th = 1. - 5. * deeparg.theta2;
      deeparg.omgdot = -0.5 * temp1 * x1m5th + 0.0625 * temp2 *
                        (7. - 114. * deeparg.theta2 + 395. * theta4) +
	                temp3 * (3. - 36. * deeparg.theta2 + 49. * theta4);
      xhdot1 = -temp1 * deeparg.cosio;
      deeparg.xnodot = xhdot1 + (0.5 * temp2 * (4. - 19. * deeparg.theta2) +
		2. * temp3 * (3. - 7. * deeparg.theta2)) * deeparg.cosio;
      xnodcf = 3.5 * deeparg.betao2 * xhdot1 * c1;
      t2cof = 1.5 * c1;
      xlcof = 0.125 * a3ovk2 * deeparg.sinio * (3. + 5. * deeparg.cosio) /
              (1. + deeparg.cosio);
      aycof = 0.25 * a3ovk2 * deeparg.sinio;
      x7thm1 = 7. * deeparg.theta2 - 1.;

      /* initialize sgp_deep() */
      sgp_deep(sgp_dpinit, tle, pflags, &deeparg);
    } /* End of sgp_sdp4() initialization */

  /* Update for secular gravity and atmospheric drag */
  xmdf = tle->xmo + deeparg.xmdot * tsince;
  deeparg.omgadf = tle->omegao + deeparg.omgdot * tsince;
  xnoddf = tle->xnodeo + deeparg.xnodot * tsince;
  tsq = tsince * tsince;
  deeparg.xnode = xnoddf + xnodcf * tsq;
  tempa = 1. - c1 * tsince;
  tempe = tle->bstar * c4 * tsince;
  templ = t2cof * tsq;
  deeparg.xn = deeparg.xnodp;

  /* Update for deep-space secular effects */
  deeparg.xll = xmdf;
  deeparg.t = tsince;

  sgp_deep(sgp_dpsec, tle, pflags, &deeparg);

  xmdf = deeparg.xll;
  a = pow(sgp_xke / deeparg.xn, sgp_tothrd) * tempa * tempa;
/* REW NORAD's version uses another variable e here */
  deeparg.em = deeparg.em - tempe;
  xmam = xmdf + deeparg.xnodp * templ;

  /* Update for deep-space periodic effects */
  deeparg.xll = xmam;

  sgp_deep(sgp_dpper, tle, pflags, &deeparg);

  xmam = deeparg.xll;
  xl = xmam + deeparg.omgadf + deeparg.xnode;
  beta = sqrt(1. - deeparg.em * deeparg.em);
  deeparg.xn = sgp_xke / pow(a, 1.5);

  /* Long period periodics */
  axn = deeparg.em * cos(deeparg.omgadf);
  temp = 1. / (a * beta * beta);
  xll = temp * xlcof * axn;
  aynl = temp * aycof;
  xlt = xl + xll;
  ayn = deeparg.em * sin(deeparg.omgadf) + aynl;

  /* Solve Kepler's Equation */
  capu = sgp_fmod2p(xlt - deeparg.xnode);
  temp2 = capu;

  for (i = 0; i < 10; ++i)
    {
      sinepw = sin(temp2);
      cosepw = cos(temp2);
      temp3 = axn * sinepw;
      temp4 = ayn * cosepw;
      temp5 = axn * cosepw;
      temp6 = ayn * sinepw;
      epw = (capu - temp4 + temp3 - temp2) / (1. - temp5 - temp6) + temp2;
      if (fabs(epw - temp2) <= sgp_e6a)
        break;
      temp2 = epw;
    }

  /* Short period preliminary quantities */
  ecose = temp5 + temp6;
  esine = temp3 - temp4;
  elsq = axn * axn + ayn * ayn;
  temp = 1. - elsq;
  pl = a * temp;
  r = a * (1. - ecose);
  temp1 = 1. / r;
  rdot = sgp_xke * sqrt(a) * esine * temp1;
  rfdot = sgp_xke * sqrt(pl) * temp1;
  temp2 = a * temp1;
  betal = sqrt(temp);
  temp3 = 1. / (1. + betal);
  cosu = temp2 * (cosepw - axn + ayn * esine * temp3);
  sinu = temp2 * (sinepw - ayn - axn * esine * temp3);
  u = sgp_actan(sinu, cosu);
  sin2u = 2. * sinu * cosu;
  cos2u = 2. * cosu * cosu - 1.;
  temp = 1. / pl;
  temp1 = sgp_ck2 * temp;
  temp2 = temp1 * temp;

  /* Update for short periodics */
  rk = r * (1. - 1.5 * temp2 * betal * x3thm1) + 0.5 * temp1 * x1mth2 * cos2u;
  uk = u - 0.25 * temp2 * x7thm1 * sin2u;
  xnodek = deeparg.xnode + 1.5 * temp2 * deeparg.cosio * sin2u;
  xinck = deeparg.xinc + 1.5 * temp2 * deeparg.cosio * deeparg.sinio * cos2u;
  rdotk = rdot - deeparg.xn * temp1 * x1mth2 * sin2u;
  rfdotk = rfdot + deeparg.xn * temp1 * (x1mth2 * cos2u + 1.5 * x3thm1);

  /* Orientation vectors */
  sinuk = sin(uk);
  cosuk = cos(uk);
  sinik = sin(xinck);
  cosik = cos(xinck);
  sinnok = sin(xnodek);
  cosnok = cos(xnodek);
  xmx = -sinnok * cosik;
  xmy = cosnok * cosik;
  ux = xmx * sinuk + cosnok * cosuk;
  uy = xmy * sinuk + sinnok * cosuk;
  uz = sinik * sinuk;
  vx = xmx * cosuk - cosnok * sinuk;
  vy = xmy * cosuk - sinnok * sinuk;
  vz = sinik * cosuk;

  /* Position and velocity */
  pos->scale = sgp_xkmper;
  pos->x = rk * ux;
  pos->y = rk * uy;
  pos->z = rk * uz;

  vel->scale = sgp_xkmper / 60.;
  vel->x = rdotk * ux + rfdotk * vx;
  vel->y = rdotk * uy + rfdotk * vy;
  vel->z = rdotk * uz + rfdotk * vz;

} /* sgp_sdp4 */

/*------------------------------------------------------------------*/

/* SDP8 */
void sgp_sdp8 (double tsince, const tlesgp_t *tle, int *pflags,
         vector_t *pos, vector_t *vel)
{
  static double
    qoms2t, tthmun, sinio2, cosio2, unm5th, unmth2,
    a3cof, xmdt1, xgdt1, xhdt1, xndt, edot;

  double cape, beta, eeta, csfg, delo, diwc, cose,
    sine, snfg, xmam, axnm, temp, aynm, aovr, dd1, dd2, tsi,
    rdot, cos2g, sini2, psim2, b, rr, cs2f2g, ecosf, sn2f2g,
    xlamb, a1, b1, b2, d1, d2, d3, d5, d4, b3, c0, c1, c4, c5,
    xndtn, sinos, cosos, g1, g2, g3, r1, g4, alpha2, g5, rvdot,
    beta2m, z1, y4, y5, z7, theta4, pardt1, pardt2, pardt4,
    sni2du, g10, g13, g14, am, ao, fm, dr, di, pm, po, rm, pom2,
    cslamb, xmamdf, snlamb, ux, vx, uy, vy, uz, vz, zc2, zc5,
    eta, csf, snf, del1, eta2;

  int i;

  static sgpdeep_t deeparg;

  if (isFlagClear(pflags, SDP8_INITIALIZED_FLAG))
    {
      setFlag(pflags, SDP8_INITIALIZED_FLAG);

      /* Recover original mean motion (xnodp) and semimajor axis (aodp) */
      /* from input elements. Calculate ballistic coefficient */
      /* (b term) from input b* drag term */
      dd1 = (sgp_xke / tle->xno);
      dd2 = sgp_tothrd;
      a1 = pow(dd1, dd2);
      deeparg.cosio = cos(tle->xincl);
      deeparg.theta2 = deeparg.cosio * deeparg.cosio;
      tthmun = deeparg.theta2 * 3. - 1.;
      deeparg.eosq = tle->eo * tle->eo;
      deeparg.betao2 = 1. - deeparg.eosq;
      deeparg.betao = sqrt(deeparg.betao2);
      del1 = sgp_ck2 * 1.5 * tthmun
		/ (a1 * a1 * deeparg.betao * deeparg.betao2);
      ao = a1 * (1. - del1 * (sgp_tothrd * .5
		+ del1 * (del1 * 1.654320987654321 + 1.)));
      delo = sgp_ck2 * 1.5 * tthmun
		/ (ao * ao * deeparg.betao * deeparg.betao2);
      deeparg.aodp = ao / (1. - delo);
      deeparg.xnodp = tle->xno / (delo + 1.);
      b = tle->bstar * 2. / sgp_rho;

      /* Initialization */
      po = deeparg.aodp * deeparg.betao2;
      pom2 = 1. / (po * po);
      deeparg.sinio = sin(tle->xincl);
      deeparg.sing = sin(tle->omegao);
      deeparg.cosg = cos(tle->omegao);
      temp = tle->xincl * .5;
      sinio2 = sin(temp);
      cosio2 = cos(temp);
      r1 = deeparg.theta2;
      theta4 = r1 * r1;
      unm5th = 1. - deeparg.theta2 * 5.;
      unmth2 = 1. - deeparg.theta2;
      r1 = sgp_ae;
      a3cof = -sgp_xj3 / sgp_ck2 * (r1 * r1 * r1);
      pardt1 = sgp_ck2 * 3. * pom2 * deeparg.xnodp;
      pardt2 = pardt1 * sgp_ck2 * pom2;
      pardt4 = sgp_ck4 * 1.25 * pom2 * pom2 * deeparg.xnodp;
      xmdt1 = pardt1 * .5 * deeparg.betao * tthmun;
      xgdt1 = pardt1 * -.5 * unm5th;
      xhdt1 = -pardt1 * deeparg.cosio;
      deeparg.xmdot = deeparg.xnodp + xmdt1 + pardt2 * .0625 * deeparg.betao
			* (13. - deeparg.theta2 * 78. + theta4 * 137.);
      deeparg.omgdot = xgdt1
		+ pardt2 * .0625 * (7. - deeparg.theta2 * 114. + theta4 * 395.)
		+ pardt4 * (3. - deeparg.theta2 * 36. + theta4 * 49.);
      deeparg.xnodot = xhdt1 + (pardt2 * .5 * (4. - deeparg.theta2 * 19.)
		+ pardt4 * 2. * (3. - deeparg.theta2 * 7.)) * deeparg.cosio;
      tsi = 1. / (po - sgp_s);
      eta = tle->eo * sgp_s * tsi;
      r1 = eta;
      eta2 = r1 * r1;
      psim2 = (r1 = 1. / (1. - eta2), fabs(r1));
      alpha2 = deeparg.eosq + 1.;
      eeta = tle->eo * eta;
      r1 = deeparg.cosg;
      cos2g = r1 * r1 * 2. - 1.;
      d5 = tsi * psim2;
      d1 = d5 / po;
      d2 = eta2 * (eta2 * 4.5 + 36.) + 12.;
      d3 = eta2 * (eta2 * 2.5 + 15.);
      d4 = eta * (eta2 * 3.75 + 5.);
      b1 = sgp_ck2 * tthmun;
      b2 = -sgp_ck2 * unmth2;
      b3 = a3cof * deeparg.sinio;
      r1 = tsi, r1 *= r1;
      dd1 = psim2;
      qoms2t = pow(sgp_qo - sgp_s, 4);
      c0 = b * .5 * sgp_rho * qoms2t * deeparg.xnodp * deeparg.aodp
		 * (r1 * r1) * pow(dd1, 3.5) / sqrt(alpha2);
      r1 = alpha2;
      c1 = deeparg.xnodp * 1.5 * (r1 * r1) * c0;
      c4 = d1 * d3 * b2;
      c5 = d5 * d4 * b3;
      xndt = c1 * (eta2 * (deeparg.eosq * 34. + 3.) + 2.
		+ eeta * 5. * (eta2 + 4.) + deeparg.eosq * 8.5
		+ d1 * d2 * b1 + c4 * cos2g + c5 * deeparg.sing);
      xndtn = xndt / deeparg.xnodp;
      edot = -sgp_tothrd * xndtn * (1. - tle->eo);

      /* initialize sgp_deep() */
      sgp_deep(sgp_dpinit, tle, pflags, &deeparg);

    } /* End of sgp_sdp8() initialization */

  /* Update for secular gravity and atmospheric drag */
  z1 = xndt * .5 * tsince * tsince;
  z7 = sgp_tothrd * 3.5 * z1 / deeparg.xnodp;
  xmamdf = tle->xmo + deeparg.xmdot * tsince;
  deeparg.omgadf = tle->omegao + deeparg.omgdot * tsince + z7 * xgdt1;
  deeparg.xnode = tle->xnodeo + deeparg.xnodot * tsince + z7 * xhdt1;
  deeparg.xn = deeparg.xnodp;

  /* Update for deep-space secular effects */
  deeparg.xll = xmamdf;
  deeparg.t = tsince;
  sgp_deep(sgp_dpsec, tle, pflags, &deeparg);
  xmamdf = deeparg.xll;
  deeparg.xn += xndt * tsince;
  deeparg.em += edot * tsince;
  xmam = xmamdf + z1 + z7 * xmdt1;

  /* Update for deep-space periodic effects */
  deeparg.xll = xmam;
  sgp_deep(sgp_dpper, tle, pflags, &deeparg);
  xmam = deeparg.xll;
  xmam = sgp_fmod2p(xmam);

  /* Solve Kepler's equation */
  zc2 = xmam + deeparg.em * sin(xmam) * (deeparg.em * cos(xmam) + 1.);

  i = 0;
  do
    {
      sine = sin(zc2);
      cose = cos(zc2);
      zc5 = 1. / (1. - deeparg.em * cose);
      cape = (xmam + deeparg.em * sine - zc2) * zc5 + zc2;
      r1 = cape - zc2;
      if (fabs(r1) <= sgp_e6a) break;
      zc2 = cape;
    }
  while(i++ < 10);

  /* Short period preliminary quantities */
  dd1 = (sgp_xke / deeparg.xn);
  dd2 = sgp_tothrd;
  am = pow(dd1, dd2);
  beta2m = 1.f - deeparg.em * deeparg.em;
  sinos = sin(deeparg.omgadf);
  cosos = cos(deeparg.omgadf);
  axnm = deeparg.em * cosos;
  aynm = deeparg.em * sinos;
  pm = am * beta2m;
  g1 = 1. / pm;
  g2 = sgp_ck2 * .5 * g1;
  g3 = g2 * g1;
  beta = sqrt(beta2m);
  g4 = a3cof * .25 * deeparg.sinio;
  g5 = a3cof * .25 * g1;
  snf = beta * sine * zc5;
  csf = (cose - deeparg.em) * zc5;
  fm = sgp_actan(snf, csf);
  snfg = snf * cosos + csf * sinos;
  csfg = csf * cosos - snf * sinos;
  sn2f2g = snfg * 2. * csfg;
  r1 = csfg;
  cs2f2g = r1 * r1 * 2. - 1.;
  ecosf = deeparg.em * csf;
  g10 = fm - xmam + deeparg.em * snf;
  rm = pm / (ecosf + 1.);
  aovr = am / rm;
  g13 = deeparg.xn * aovr;
  g14 = -g13 * aovr;
  dr = g2 * (unmth2 * cs2f2g - tthmun * 3.) - g4 * snfg;
  diwc = g3 * 3. * deeparg.sinio * cs2f2g - g5 * aynm;
  di = diwc * deeparg.cosio;
  sini2 = sin(deeparg.xinc * .5);

  /* Update for short period periodics */
  sni2du = sinio2
	* (g3 * ((1. - deeparg.theta2 * 7.) * .5 * sn2f2g - unm5th * 3. * g10)
		- g5 * deeparg.sinio * csfg * (ecosf + 2.))
	- g5 * .5 * deeparg.theta2 * axnm / cosio2;
  xlamb = fm + deeparg.omgadf + deeparg.xnode
	+ g3 * ((deeparg.cosio * 6. + 1. - deeparg.theta2 * 7.) * .5 * sn2f2g
			- (unm5th + deeparg.cosio * 2.) * 3. * g10)
	+ g5 * deeparg.sinio * (deeparg.cosio * axnm / (deeparg.cosio + 1.)
				- (ecosf + 2.) * csfg);
  y4 = sini2 * snfg + csfg * sni2du + snfg * .5 * cosio2 * di;
  y5 = sini2 * csfg - snfg * sni2du + csfg * .5 * cosio2 * di;
  rr = rm + dr;
  rdot = deeparg.xn * am * deeparg.em * snf / beta
	+ g14 * (g2 * 2. * unmth2 * sn2f2g + g4 * csfg);
  r1 = am;
  rvdot = deeparg.xn * (r1 * r1) * beta / rm + g14 * dr
	+ am * g13 * deeparg.sinio * diwc;

  /* Orientation vectors */
  snlamb = sin(xlamb);
  cslamb = cos(xlamb);
  temp = (y5 * snlamb - y4 * cslamb) * 2.;
  ux = y4 * temp + cslamb;
  vx = y5 * temp - snlamb;
  temp = (y5 * cslamb + y4 * snlamb) * 2.;
  uy = -y4 * temp + snlamb;
  vy = -y5 * temp + cslamb;
  temp = sqrt(1. - y4 * y4 - y5 * y5) * 2.;
  uz = y4 * temp;
  vz = y5 * temp;

  /* Position and velocity */
  pos->scale = sgp_xkmper;
  pos->x = rr * ux;
  pos->y = rr * uy;
  pos->z = rr * uz;

  vel->scale = sgp_xkmper / 60.;
  vel->x = rdot * ux + rvdot * vx;
  vel->y = rdot * uy + rvdot * vy;
  vel->z = rdot * uz + rvdot * vz;

} /* sgp_sdp8 */

/*------------------------------------------------------------------*/

/* DEEP */
void
sgp_deep(int ientry, const tlesgp_t *tle, int *pflags,
         sgpdeep_t *deeparg)
{
  static double
    thgr, xnq, xqncl, omegaq, zmol, zmos, savtsn, ee2, e3, xi2,
    xl2, xl3, xl4, xgh2, xgh3, xgh4, xh2, xh3, sse, ssi, ssg, xi3,
    se2, si2, sl2, sgh2, sh2, se3, si3, sl3, sgh3, sh3, sl4, sgh4,
    ssl, ssh, d3210, d3222, d4410, d4422, d5220, d5232, d5421,
    d5433, del1, del2, del3, fasx2, fasx4, fasx6, xlamo, xfact,
    xni, atime, stepp, stepn, step2, preep, pl, sghs, xli,
    d2201, d2211, sghl, sh1, pinc, pe, shs, zsingl, zcosgl,
    zsinhl, zcoshl, zsinil, zcosil;

  double
    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, ainv2, alfdp, aqnv,
    sgh, sini2, sinis, sinok, sh, si, sil, day, betdp, dalf,
    bfact, c, cc, cosis, cosok, cosq, ctem, f322, zx, zy,
    dbet, dls, eoc, eq, f2, f220, f221, f3, f311, f321, xnoh,
    f330, f441, f442, f522, f523, f542, f543, g200, g201,
    g211, pgh, ph, s1, s2, s3, s4, s5, s6, s7, se, sel, ses, xls,
    g300, g310, g322, g410, g422, g520, g521, g532, g533, gam,
    sinq, sinzf, sis, sl, sll, sls, stem, temp, temp1, x1, x2,
    x2li, x2omi, x3, x4, x5, x6, x7, x8, xl, xldot, xmao, xnddt,
    xndot, xno2, xnodce, xnoi, xomi, xpidot, z1, z11, z12, z13,
    z2, z21, z22, z23, z3, z31, z32, z33, ze, zf, zm, zmo, zn,
    zsing, zsinh, zsini, zcosg, zcosh, zcosi, delt=0, ft=0;


  switch (ientry)
    {
    case sgp_dpinit : { /* Entrance for deep space initialization */
      int lunardone = 0;

      thgr = sgp_thetag(tle->epoch, deeparg);
      eq = tle->eo;
      xnq = deeparg->xnodp;
      aqnv = 1. / deeparg->aodp;
      xqncl = tle->xincl;
      xmao = tle->xmo;
      xpidot = deeparg->omgdot + deeparg->xnodot;
      sinq = sin(tle->xnodeo);
      cosq = cos(tle->xnodeo);
      omegaq = tle->omegao;

      /* Initialize lunar solar terms */
      day = deeparg->ds50 + 18261.5;  /*Days since 1900 Jan 0.5*/
      if (day != preep)
	{
	  preep = day;
	  xnodce = 4.5236020 - 9.2422029E-4 * day;
	  stem = sin(xnodce);
	  ctem = cos(xnodce);
	  zcosil = 0.91375164 - 0.03568096 * ctem;
	  zsinil = sqrt(1 - zcosil * zcosil);
	  zsinhl = 0.089683511 * stem / zsinil;
	  zcoshl = sqrt(1 - zsinhl * zsinhl);
	  c = 4.7199672 + 0.22997150 * day;
	  gam = 5.8351514 + 0.0019443680 * day;
	  zmol = sgp_fmod2p(c - gam);
	  zx = 0.39785416 * stem / zsinil;
	  zy = zcoshl * ctem + 0.91744867 * zsinhl * stem;
	  zx = sgp_actan(zx, zy);
	  zx = gam + zx - xnodce;
	  zcosgl = cos(zx);
	  zsingl = sin(zx);
	  zmos = 6.2565837 + 0.017201977 * day;
	  zmos = sgp_fmod2p(zmos);
	} /* End if (day != preep) */

      /* Do solar terms */
      savtsn = 1e20;
      zcosg = sgp_zcosgs;
      zsing = sgp_zsings;
      zcosi = sgp_zcosis;
      zsini = sgp_zsinis;
      zcosh = cosq;
      zsinh = sinq;
      cc = sgp_c1ss;
      zn = sgp_zns;
      ze = sgp_zes;
      zmo = zmos;
      xnoi = 1. / xnq;

      /* Loop breaks when Solar terms are done a second */
      /* time, after Lunar terms are initialized        */
      for (;;) 
	{
	  /* Solar terms done again after Lunar terms are done */
	  a1 = zcosg * zcosh + zsing * zcosi * zsinh;
	  a3 = -zsing * zcosh + zcosg * zcosi * zsinh;
	  a7 = -zcosg * zsinh + zsing * zcosi * zcosh;
	  a8 = zsing * zsini;
	  a9 = zsing * zsinh + zcosg * zcosi * zcosh;
	  a10 = zcosg * zsini;
	  a2 = deeparg->cosio * a7 + deeparg->sinio * a8;
	  a4 = deeparg->cosio * a9 + deeparg->sinio * a10;
	  a5 = -deeparg->sinio * a7 + deeparg->cosio * a8;
	  a6 = -deeparg->sinio * a9 + deeparg->cosio * a10;
	  x1 = a1 * deeparg->cosg + a2 * deeparg->sing;
	  x2 = a3 * deeparg->cosg + a4 * deeparg->sing;
	  x3 = -a1 * deeparg->sing + a2 * deeparg->cosg;
	  x4 = -a3 * deeparg->sing + a4 * deeparg->cosg;
	  x5 = a5 * deeparg->sing;
	  x6 = a6 * deeparg->sing;
	  x7 = a5 * deeparg->cosg;
	  x8 = a6 * deeparg->cosg;
	  z31 = 12 * x1 * x1 - 3 * x3 * x3;
	  z32 = 24 * x1 * x2 - 6 * x3 * x4;
	  z33 = 12 * x2 * x2 - 3 * x4 * x4;
	  z1 = 3 * (a1 * a1 + a2 * a2) + z31 * deeparg->eosq;
	  z2 = 6 * (a1 * a3 + a2 * a4) + z32 * deeparg->eosq;
	  z3 = 3 * (a3 * a3 + a4 * a4) + z33 * deeparg->eosq;
	  z11 = -6 * a1 * a5 + deeparg->eosq * (-24 * x1 * x7 - 6 * x3 * x5);
	  z12 = -6 * (a1 * a6 + a3 * a5) + deeparg->eosq *
                (-24 * (x2 * x7 + x1 * x8) - 6 * (x3 * x6 + x4 * x5));
	  z13 = -6 * a3 * a6 + deeparg->eosq * (-24 * x2 * x8 - 6 * x4 * x6);
	  z21 = 6 * a2 * a5 + deeparg->eosq * (24 * x1 * x5 - 6 * x3 * x7);
	  z22 = 6 * (a4 * a5 + a2 * a6) + deeparg->eosq *
                (24 * (x2 * x5 + x1 * x6) - 6 * (x4 * x7 + x3 * x8));
	  z23 = 6 * a4 * a6 + deeparg->eosq * (24 * x2 * x6 - 6 * x4 * x8);
	  z1 = z1 + z1 + deeparg->betao2 * z31;
	  z2 = z2 + z2 + deeparg->betao2 * z32;
	  z3 = z3 + z3 + deeparg->betao2 * z33;
	  s3 = cc * xnoi;
	  s2 = -0.5 * s3 / deeparg->betao;
	  s4 = s3 * deeparg->betao;
	  s1 = -15 * eq * s4;
	  s5 = x1 * x3 + x2 * x4;
	  s6 = x2 * x3 + x1 * x4;
	  s7 = x2 * x4 - x1 * x3;
	  se = s1 * zn * s5;
	  si = s2 * zn * (z11 + z13);
	  sl = -zn * s3 * (z1 + z3 - 14 - 6 * deeparg->eosq);
	  sgh = s4 * zn * (z31 + z33 - 6);
	  sh = -zn * s2 * (z21 + z23);
	  if (xqncl < 5.2359877E-2)
            sh = 0;
	  ee2 = 2 * s1 * s6;
	  e3 = 2 * s1 * s7;
	  xi2 = 2 * s2 * z12;
	  xi3 = 2 * s2 * (z13 - z11);
	  xl2 = -2 * s3 * z2;
	  xl3 = -2 * s3 * (z3 - z1);
	  xl4 = -2 * s3 * (-21 - 9 * deeparg->eosq) * ze;
	  xgh2 = 2 * s4 * z32;
	  xgh3 = 2 * s4 * (z33 - z31);
	  xgh4 = -18 * s4 * ze;
	  xh2 = -2 * s2 * z22;
	  xh3 = -2 * s2 * (z23 - z21);

	  if (lunardone)
            break;

	  /* Do lunar terms */
	  sse = se;
	  ssi = si;
	  ssl = sl;
	  ssh = sh / deeparg->sinio;
	  ssg = sgh - deeparg->cosio * ssh;
	  se2 = ee2;
	  si2 = xi2;
	  sl2 = xl2;
	  sgh2 = xgh2;
	  sh2 = xh2;
	  se3 = e3;
	  si3 = xi3;
	  sl3 = xl3;
	  sgh3 = xgh3;
	  sh3 = xh3;
	  sl4 = xl4;
	  sgh4 = xgh4;
	  zcosg = zcosgl;
	  zsing = zsingl;
	  zcosi = zcosil;
	  zsini = zsinil;
	  zcosh = zcoshl * cosq + zsinhl * sinq;
	  zsinh = sinq * zcoshl - cosq * zsinhl;
	  zn = sgp_znl;
	  cc = sgp_c1l;
	  ze = sgp_zel;
	  zmo = zmol;

          lunardone = 1;
	} /* End of for (;;) */

      sse = sse + se;
      ssi = ssi + si;
      ssl = ssl + sl;
      ssg = ssg + sgh - deeparg->cosio / deeparg->sinio * sh;
      ssh = ssh + sh / deeparg->sinio;

      /* Geopotential resonance initialization for 12 hour orbits */
      clearFlag(pflags, RESONANCE_FLAG);
      clearFlag(pflags, SYNCHRONOUS_FLAG);

      if ( !((xnq < 0.0052359877) && (xnq > 0.0034906585)) )
	{
	  if ( (xnq < 0.00826) || (xnq > 0.00924) )
            return;
	  if (eq < 0.5)
            return;
	  setFlag(pflags, RESONANCE_FLAG);
	  eoc = eq * deeparg->eosq;
	  g201 = -0.306 - (eq - 0.64) * 0.440;
	  if (eq <= 0.65)
	    {
	      g211 = 3.616 - 13.247 * eq + 16.290 * deeparg->eosq;
	      g310 = -19.302 + 117.390 * eq
                      - 228.419 * deeparg->eosq + 156.591 * eoc;
	      g322 = -18.9068 + 109.7927 * eq
                      - 214.6334 * deeparg->eosq + 146.5816 * eoc;
	      g410 = -41.122 + 242.694 * eq
                      -471.094 * deeparg->eosq + 313.953 * eoc;
	      g422 = -146.407 + 841.880 * eq
                      -1629.014 * deeparg->eosq + 1083.435 * eoc;
	      g520 = -532.114 + 3017.977 * eq
                      - 5740 * deeparg->eosq + 3708.276 * eoc;
	    }
	  else
	    {
	      g211 = -72.099 + 331.819 * eq
                      - 508.738 * deeparg->eosq + 266.724 * eoc;
	      g310 = -346.844 + 1582.851 * eq
                      - 2415.925 * deeparg->eosq + 1246.113 * eoc;
	      g322 = -342.585 + 1554.908 * eq
                      - 2366.899 * deeparg->eosq + 1215.972 * eoc;
	      g410 = -1052.797 + 4758.686 * eq
                      - 7193.992 * deeparg->eosq + 3651.957 * eoc;
	      g422 = -3581.69 + 16178.11 * eq
                      - 24462.77 * deeparg->eosq + 12422.52 * eoc;
	      if (eq <= 0.715)
		g520 = 1464.74 - 4664.75 * eq + 3763.64 * deeparg->eosq;
	      else
		g520 = -5149.66 + 29936.92 * eq
                        - 54087.36 * deeparg->eosq + 31324.56 * eoc;
	    } /* End if (eq <= 0.65) */

	  if (eq < 0.7)
	    {
	      g533 = -919.2277 + 4988.61 * eq
                      - 9064.77 * deeparg->eosq + 5542.21 * eoc;
	      g521 = -822.71072 + 4568.6173 * eq
                      - 8491.4146 * deeparg->eosq + 5337.524 * eoc;
	      g532 = -853.666 + 4690.25 * eq
                      - 8624.77 * deeparg->eosq + 5341.4 * eoc;
	    }
	  else
	    {
	      g533 = -37995.78 + 161616.52 * eq
                      - 229838.2 * deeparg->eosq + 109377.94 * eoc;
	      g521 = -51752.104 + 218913.95 * eq
                      - 309468.16 * deeparg->eosq + 146349.42 * eoc;
	      g532 = -40023.88 + 170470.89 * eq
                      - 242699.48 * deeparg->eosq + 115605.82 * eoc;
	    } /* End if (eq <= 0.7) */

	  sini2 = deeparg->sinio * deeparg->sinio;
	  f220 = 0.75 * (1 + 2 * deeparg->cosio + deeparg->theta2);
	  f221 = 1.5 * sini2;
	  f321 = 1.875 * deeparg->sinio
                   * (1 - 2 * deeparg->cosio - 3 * deeparg->theta2);
	  f322 = -1.875 * deeparg->sinio
                   * (1 + 2 * deeparg->cosio - 3 * deeparg->theta2);
	  f441 = 35 * sini2 * f220;
	  f442 = 39.3750 * sini2 * sini2;
	  f522 = 9.84375 * deeparg->sinio
                   * (sini2 * (1 - 2 * deeparg->cosio - 5 * deeparg->theta2)
                      + 0.33333333 * (-2 + 4 * deeparg->cosio
                                      + 6 * deeparg->theta2));
	  f523 = deeparg->sinio * (4.92187512 * sini2
                        * (-2 - 4 * deeparg->cosio + 10 * deeparg->theta2)
                   + 6.56250012 * (1 + 2*deeparg->cosio - 3*deeparg->theta2));
	  f542 = 29.53125 * deeparg->sinio
                 * (2 - 8 * deeparg->cosio
                    + deeparg->theta2 *
		        (-12 + 8 * deeparg->cosio + 10 * deeparg->theta2));
	  f543 = 29.53125 * deeparg->sinio
                 * (-2 - 8 * deeparg->cosio + deeparg->theta2
                    * (12 + 8 * deeparg->cosio - 10 * deeparg->theta2));
	  xno2 = xnq * xnq;
	  ainv2 = aqnv * aqnv;
	  temp1 = 3 * xno2 * ainv2;
	  temp = temp1 * sgp_root22;
	  d2201 = temp * f220 * g201;
	  d2211 = temp * f221 * g211;
	  temp1 = temp1 * aqnv;
	  temp = temp1 * sgp_root32;
	  d3210 = temp * f321 * g310;
	  d3222 = temp * f322 * g322;
	  temp1 = temp1 * aqnv;
	  temp = 2 * temp1 * sgp_root44;
	  d4410 = temp * f441 * g410;
	  d4422 = temp * f442 * g422;
	  temp1 = temp1 * aqnv;
	  temp = temp1 * sgp_root52;
	  d5220 = temp * f522 * g520;
	  d5232 = temp * f523 * g532;
	  temp = 2 * temp1 * sgp_root54;
	  d5421 = temp * f542 * g521;
	  d5433 = temp * f543 * g533;
	  xlamo = xmao + tle->xnodeo + tle->xnodeo - thgr - thgr;
	  bfact = deeparg->xmdot + deeparg->xnodot
                  + deeparg->xnodot - sgp_thdt - sgp_thdt;
	  bfact = bfact + ssl + ssh + ssh;
	} /* if ( !(xnq < 0.0052359877) && (xnq > 0.0034906585) ) */
      else
	{
	  setFlag(pflags, RESONANCE_FLAG);
	  setFlag(pflags, SYNCHRONOUS_FLAG);

	  /* Synchronous resonance terms initialization */
	  g200 = 1 + deeparg->eosq * (-2.5 + 0.8125 * deeparg->eosq);
	  g310 = 1 + 2 * deeparg->eosq;
	  g300 = 1 + deeparg->eosq * (-6 + 6.60937 * deeparg->eosq);
	  f220 = 0.75 * (1 + deeparg->cosio) * (1 + deeparg->cosio);
	  f311 = 0.9375 * deeparg->sinio * deeparg->sinio
                 * (1 + 3 * deeparg->cosio) - 0.75 * (1 + deeparg->cosio);
	  f330 = 1 + deeparg->cosio;
	  f330 = 1.875 * f330 * f330 * f330;
	  del1 = 3 * xnq * xnq * aqnv * aqnv;
	  del2 = 2 * del1 * f220 * g200 * sgp_q22;
	  del3 = 3 * del1 * f330 * g300 * sgp_q33 * aqnv;
	  del1 = del1 * f311 * g310 * sgp_q31 * aqnv;
	  fasx2 = 0.13130908;
	  fasx4 = 2.8843198;
	  fasx6 = 0.37448087;
	  xlamo = xmao + tle->xnodeo + tle->omegao - thgr;
	  bfact = deeparg->xmdot + xpidot - sgp_thdt;
	  bfact = bfact + ssl + ssg + ssh;
	} /* End if ( !(xnq < 0.0052359877) && (xnq > 0.0034906585) ) */

      xfact = bfact - xnq;

      /* Initialize integrator */
      xli = xlamo;
      xni = xnq;
      atime = 0;
      stepp = 720;
      stepn = -720;
      step2 = 259200;
      /* End case sgp_dpinit: */
    }

    case sgp_dpsec: {
      int epochRestartFlag = 0;
      int doLoopFlag = 0;
      /* Entrance for deep space secular effects */
      deeparg->xll = deeparg->xll + ssl * deeparg->t;
      deeparg->omgadf = deeparg->omgadf + ssg * deeparg->t;
      deeparg->xnode = deeparg->xnode + ssh * deeparg->t;
      deeparg->em = tle->eo + sse * deeparg->t;
      deeparg->xinc = tle->xincl + ssi * deeparg->t;
      if (deeparg->xinc < 0)
	{
	  deeparg->xinc = -deeparg->xinc;
	  deeparg->xnode = deeparg->xnode + sgp_pi;
	  deeparg->omgadf = deeparg->omgadf - sgp_pi;
	}

      if ( isFlagClear(pflags, RESONANCE_FLAG) )
        return;

      do
/* 100 */
	{
          if ( (atime == 0) ||
	     ((deeparg->t >= 0) && (atime < 0)) || 
	     ((deeparg->t < 0) && (atime >= 0)) )
	    {
/* 170 */     /* Epoch restart */
	      if (deeparg->t >= 0)
		delt = stepp;
	      else
		delt = stepn;

	      atime = 0;
	      xni = xnq;
	      xli = xlamo;
	    }
	  else
	    {	  
              if ( fabs(deeparg->t) >= fabs(atime) )
		{
/* 120 */
		  if ( deeparg->t > 0 )
		    delt = stepp;
		  else
		    delt = stepn;
		}
              else
                {
                  if (deeparg->t >= 0)
                    delt = stepn;
                  else
                    delt = stepp;
                  epochRestartFlag = 1;
                }
	    }

          do 
	    {
              if (!epochRestartFlag)
                {
/* 125 */
	          if ( fabs(deeparg->t - atime) >= stepp )
		    {
                      doLoopFlag = 1;
		    }
	          else
		    {
		      ft = deeparg->t - atime;
                      doLoopFlag = 0;
		    }
                }
    
	      /* Dot terms calculated */
              if (isFlagSet(pflags, SYNCHRONOUS_FLAG))
		{
		  xndot = del1 * sin(xli - fasx2) + del2 * sin(2*(xli - fasx4))
		          + del3 * sin(3 * (xli - fasx6));
		  xnddt = del1 * cos(xli - fasx2)
                          + 2 * del2 * cos(2 * (xli - fasx4))
                          + 3 * del3 * cos(3 * (xli - fasx6));
		}
	      else
		{
		  xomi = omegaq + deeparg->omgdot * atime;
		  x2omi = xomi + xomi;
		  x2li = xli + xli;
		  xndot = d2201 * sin(x2omi + xli - sgp_g22)
		          + d2211 * sin(xli - sgp_g22)
		          + d3210 * sin(xomi + xli - sgp_g32)
		          + d3222 * sin(-xomi + xli - sgp_g32)
		          + d4410 * sin(x2omi + x2li - sgp_g44)
		          + d4422 * sin(x2li - sgp_g44)
		          + d5220 * sin(xomi + xli - sgp_g52)
		          + d5232 * sin(-xomi + xli - sgp_g52)
		          + d5421 * sin(xomi + x2li - sgp_g54)
		          + d5433 * sin(-xomi + x2li - sgp_g54);
		  xnddt = d2201 * cos(x2omi + xli - sgp_g22)
		          + d2211 * cos(xli - sgp_g22)
		          + d3210 * cos(xomi + xli - sgp_g32)
		          + d3222 * cos(-xomi + xli - sgp_g32)
		          + d5220 * cos(xomi + xli - sgp_g52)
		          + d5232 * cos(-xomi + xli - sgp_g52)
		          + 2 * (d4410 * cos(x2omi + x2li - sgp_g44)
		              + d4422 * cos(x2li - sgp_g44)
		              + d5421 * cos(xomi + x2li - sgp_g54)
		              + d5433 * cos(-xomi + x2li - sgp_g54));
		} /* End of if (isFlagSet(pflags, SYNCHRONOUS_FLAG)) */

	      xldot = xni + xfact;
	      xnddt = xnddt * xldot;

	      if (doLoopFlag)
		{
		  xli = xli + xldot * delt + xndot * step2;
		  xni = xni + xndot * delt + xnddt * step2;
		  atime = atime + delt;
		}
	    }
	  while (doLoopFlag && !epochRestartFlag);
	}
      while (doLoopFlag && epochRestartFlag);

/* 140 */
      deeparg->xn = xni + xndot * ft + xnddt * ft * ft * 0.5;
      xl = xli + xldot * ft + xndot * ft * ft * 0.5;
      temp = -deeparg->xnode + thgr + deeparg->t * sgp_thdt;

      if (isFlagClear(pflags, SYNCHRONOUS_FLAG))
	deeparg->xll = xl + temp + temp;
      else
	deeparg->xll = xl - deeparg->omgadf + temp;

      /*End case sgp_dpsec: */
      break;
    }

    case sgp_dpper: /* Entrance for lunar-solar periodics */
      sinis = sin(deeparg->xinc);
      cosis = cos(deeparg->xinc);
      if (fabs(savtsn - deeparg->t) >= 30)
	{
	  savtsn = deeparg->t;
	  zm = zmos + sgp_zns * deeparg->t;
	  zf = zm + 2 * sgp_zes * sin(zm);
	  sinzf = sin(zf);
	  f2 = 0.5 * sinzf * sinzf - 0.25;
	  f3 = -0.5 *sinzf * cos(zf);
	  ses = se2 * f2 + se3 * f3;
	  sis = si2 * f2 + si3 * f3;
	  sls = sl2 * f2 + sl3 * f3 + sl4 * sinzf;
	  sghs = sgh2 * f2 + sgh3 * f3 + sgh4 * sinzf;
	  shs = sh2 * f2 + sh3 * f3;
	  zm = zmol + sgp_znl * deeparg->t;
	  zf = zm + 2 * sgp_zel * sin(zm);
	  sinzf = sin(zf);
	  f2 = 0.5 * sinzf * sinzf - 0.25;
	  f3 = -0.5 * sinzf * cos(zf);
	  sel = ee2 * f2 + e3 * f3;
	  sil = xi2 * f2 + xi3 * f3;
	  sll = xl2 * f2 + xl3 * f3 + xl4 * sinzf;
	  sghl = xgh2 * f2 + xgh3 * f3 + xgh4 * sinzf;
	  sh1 = xh2 * f2 + xh3 * f3;
	  pe = ses + sel;
	  pinc = sis + sil;
	  pl = sls + sll;
	}

      pgh = sghs + sghl;
      ph = shs + sh1;
      deeparg->xinc = deeparg->xinc + pinc;
      deeparg->em = deeparg->em + pe;

      if (xqncl >= 0.2)
	{
	  /* Apply periodics directly */
	  ph = ph / deeparg->sinio;
	  pgh = pgh - deeparg->cosio * ph;
	  deeparg->omgadf = deeparg->omgadf + pgh;
	  deeparg->xnode = deeparg->xnode + ph;
	  deeparg->xll = deeparg->xll + pl;
	}
      else
        {
	  /* Apply periodics with Lyddane modification */
	  sinok = sin(deeparg->xnode);
	  cosok = cos(deeparg->xnode);
	  alfdp = sinis * sinok;
	  betdp = sinis * cosok;
	  dalf = ph * cosok + pinc * cosis * sinok;
	  dbet = -ph * sinok + pinc * cosis * cosok;
	  alfdp = alfdp + dalf;
	  betdp = betdp + dbet;
	  /* deeparg->xnode = sgp_fmod2p(deeparg->xnode); */
	  xls = deeparg->xll + deeparg->omgadf + cosis * deeparg->xnode;
	  dls = pl + pgh - pinc * deeparg->xnode * sinis;
	  xls = xls + dls;
	  xnoh = deeparg->xnode;
	  deeparg->xnode = sgp_actan(alfdp, betdp);

#ifdef RCM
          /* This is a patch to Lyddane modification */
          /* suggested by Rob Matson. */
	  if (fabs(xnoh - deeparg->xnode) > sgp_pi)
	    {
	      if (deeparg->xnode < xnoh)
		deeparg->xnode += sgp_twopi;
	      else
		deeparg->xnode -= sgp_twopi;
	    }
#endif

	  deeparg->xll = deeparg->xll + pl;
	  deeparg->omgadf = xls - deeparg->xll
                              - cos(deeparg->xinc) * deeparg->xnode;
	} /* End case sgp_dpper: */
        break;

    } /* End switch(ientry) */

} /* End of sgp_deep() */


/*------------------------------------------------------------------*/

/* THETAG */
double sgp_thetag (double epoch, sgpdeep_t *deeparg)
{
  /* Reference:  The 1992 Astronomical Almanac, page B6. */

  double year,day,UT,jd,TU,GMST,ThetaG;

/* Modification to support Y2K */
/* Valid 1957 through 2056     */
  day = modf(epoch * 1E-3, &year) * 1E3;
  if (year < 57)
    year += 2000;
  else
    year += 1900;
  /* End modification */

  UT   = modf(day, &day);
  jd   = Julian_Date_of_Year(year) + day;
  TU   = (jd - 2451545.0) / 36525;
  GMST = 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
  GMST = sgp_modulus(GMST + sgp_secday * sgp_omega_E * UT, sgp_secday);
  ThetaG = sgp_twopi * GMST / sgp_secday;
  deeparg->ds50 = jd - 2433281.5 + UT;
  ThetaG = sgp_fmod2p(6.3003880987 * deeparg->ds50 + 1.72944494);

  return ThetaG;
} /*Function thetag*/

/*------------------------------------------------------------------*/

double Julian_Date_of_Year(double year)
{
  /* Astronomical Formulae for Calculators, Jean Meeus, */
  /* pages 23-25. Calculate Julian Date of 0.0 Jan year */
  long A,B,i;
  double jdoy;

  year = year - 1;
  i = year / 100;
  A = i;
  i = A / 4;
  B = 2 - A + i;
  i = 365.25 * year;
  i += 30.6001 * 14;
  jdoy = i + 1720994.5 + B;

  return jdoy;
}  /*Function Julian_Date_of_Year*/

/*------------------------------------------------------------------*/

/* ACTAN */
double sgp_actan (double sinx, double cosx)
{
  if (cosx == 0)
    {
      if (sinx > 0)
	return sgp_pio2;
      else
	return sgp_x3pio2;
    }
  else
    {
      if (cosx > 0)
	{
	  if (sinx > 0)
	    return atan(sinx / cosx);
	  else
	    return sgp_twopi + atan(sinx / cosx);
	}
      else
	return sgp_pi + atan(sinx / cosx);
    }

} /*Function actan*/

/*------------------------------------------------------------------*/

/* FMOD2P */
double sgp_fmod2p (double x)
{
  int i;
  double ret_val;

  ret_val = x;
  i = ret_val / sgp_twopi;
  ret_val -= i * sgp_twopi;
  if (ret_val < 0)
    ret_val += sgp_twopi;

  return ret_val;
} /* fmod2p */

/*------------------------------------------------------------------*/

double sgp_modulus (double arg1, double arg2)
{
  int i;
  double ret_val;

  ret_val = arg1;
  i = ret_val / arg2;
  ret_val -= i * arg2;
  if (ret_val < 0)
    ret_val += arg2;

  return ret_val;
} /* sgp_modulus */

/*------------------------------------------------------------------*/

/* Functions for testing and setting/clearing flags */

int
isFlagSet(int *pflags, int flag)
{
  return (*pflags & flag);
}

int
isFlagClear(int *pflags, int flag)
{
  return (~*pflags & flag);
}

void
setFlag(int *pflags, int flag)
{
  *pflags |= flag;
}

void
clearFlag(int *pflags, int flag)
{
  *pflags &= ~flag;
}

