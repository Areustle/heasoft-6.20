/* slalib.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b7 = 6.283185307179586476925287;
static doublereal c_b10 = 6.283185307179586476925286766559;
static doublereal c_b13 = 1950.;
static doublereal c_b21 = 0.;

/*<       include 'addet.f' >*/
/*<       include 'dcc2s.f' >*/
/* Subroutine */ int sla_addet__(doublereal *rm, doublereal *dm, doublereal *
	eq, doublereal *rc, doublereal *dc)
{
    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *);
    doublereal a[3];
    integer i;
    doublereal v[3];
    extern /* Subroutine */ int sla_etrms__(doublereal *, doublereal *);
    extern doublereal sla_dranrm__(doublereal *);

/* + */
/*     - - - - - - */
/*      A D D E T */
/*     - - - - - - */

/*  Add the E-terms (elliptic component of annual aberration) */
/*  to a pre IAU 1976 mean place to conform to the old */
/*  catalogue convention (double precision) */

/*  Given: */
/*     RM,DM     dp     RA,Dec (radians) without E-terms */
/*     EQ        dp     Besselian epoch of mean equator and equinox */

/*  Returned: */
/*     RC,DC     dp     RA,Dec (radians) with E-terms included */

/*  Called: */
/*     sla_ETRMS, sla_DCS2C, sla_DCC2S, sla_DRANRM, sla_DRANGE */

/*  Explanation: */
/*     Most star positions from pre-1984 optical catalogues (or */
/*     derived from astrometry using such stars) embody the */
/*     E-terms.  If it is necessary to convert a formal mean */
/*     place (for example a pulsar timing position) to one */
/*     consistent with such a star catalogue, then the RA,Dec */
/*     should be adjusted using this routine. */

/*  Reference: */
/*     Explanatory Supplement to the Astronomical Ephemeris, */
/*     section 2D, page 48. */

/*  P.T.Wallace   Starlink   July 1986 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION RM,DM,EQ,RC,DC >*/
/*<       DOUBLE PRECISION sla_DRANRM >*/
/*<       DOUBLE PRECISION A(3),V(3) >*/
/*<       INTEGER I >*/
/*  E-terms vector */
/*<       CALL sla_ETRMS(EQ,A) >*/
    sla_etrms__(eq, a);
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(RM,DM,V) >*/
    sla_dcs2c__(rm, dm, v);
/*  Include the E-terms */
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          V(I)=V(I)+A(I) >*/
	v[i - 1] += a[i - 1];
/*<       END DO >*/
    }
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V,RC,DC) >*/
    sla_dcc2s__(v, rc, dc);
/*  Bring RA into conventional range */
/*<       RC=sla_DRANRM(RC) >*/
    *rc = sla_dranrm__(rc);
/*<       END >*/
    return 0;
} /* sla_addet__ */

/*<       include 'dcs2c.f' >*/
/* Subroutine */ int sla_dcc2s__(doublereal *v, doublereal *a, doublereal *b)
{
    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal r, x, y, z;

/* + */
/*     - - - - - - */
/*      D C C 2 S */
/*     - - - - - - */

/*  Direction cosines to spherical coordinates (double precision) */

/*  Given: */
/*     V     d(3)   x,y,z vector */

/*  Returned: */
/*     A,B   d      spherical coordinates in radians */

/*  The spherical coordinates are longitude (+ve anticlockwise */
/*  looking from the +ve latitude pole) and latitude.  The */
/*  Cartesian coordinates are right handed, with the x axis */
/*  at zero longitude and latitude, and the z axis at the */
/*  +ve latitude pole. */

/*  If V is null, zero A and B are returned. */
/*  At either pole, zero A is returned. */

/*  P.T.Wallace   Starlink   July 1989 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION V(3),A,B >*/
/*<       DOUBLE PRECISION X,Y,Z,R >*/
/*<       X = V(1) >*/
    /* Parameter adjustments */
    --v;

    /* Function Body */
    x = v[1];
/*<       Y = V(2) >*/
    y = v[2];
/*<       Z = V(3) >*/
    z = v[3];
/*<       R = SQRT(X*X+Y*Y) >*/
    r = sqrt(x * x + y * y);
/*<       IF (R.EQ.0D0) THEN >*/
    if (r == 0.) {
/*<          A = 0D0 >*/
	*a = 0.;
/*<       ELSE >*/
    } else {
/*<          A = ATAN2(Y,X) >*/
	*a = atan2(y, x);
/*<       END IF >*/
    }
/*<       IF (Z.EQ.0D0) THEN >*/
    if (z == 0.) {
/*<          B = 0D0 >*/
	*b = 0.;
/*<       ELSE >*/
    } else {
/*<          B = ATAN2(Z,R) >*/
	*b = atan2(z, r);
/*<       END IF >*/
    }
/*<       END >*/
    return 0;
} /* sla_dcc2s__ */

/*<       include 'dimxv.f' >*/
/* Subroutine */ int sla_dcs2c__(doublereal *a, doublereal *b, doublereal *v)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal cosb;

/* + */
/*     - - - - - - */
/*      D C S 2 C */
/*     - - - - - - */

/*  Spherical coordinates to direction cosines (double precision) */

/*  Given: */
/*     A,B       dp      spherical coordinates in radians */
/*                        (RA,Dec), (Long,Lat) etc */

/*  Returned: */
/*     V         dp(3)   x,y,z unit vector */

/*  The spherical coordinates are longitude (+ve anticlockwise */
/*  looking from the +ve latitude pole) and latitude.  The */
/*  Cartesian coordinates are right handed, with the x axis */
/*  at zero longitude and latitude, and the z axis at the */
/*  +ve latitude pole. */

/*  P.T.Wallace   Starlink   October 1984 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION A,B,V(3) >*/
/*<       DOUBLE PRECISION COSB >*/
/*<       COSB=COS(B) >*/
    /* Parameter adjustments */
    --v;

    /* Function Body */
    cosb = cos(*b);
/*<       V(1)=COS(A)*COSB >*/
    v[1] = cos(*a) * cosb;
/*<       V(2)=SIN(A)*COSB >*/
    v[2] = sin(*a) * cosb;
/*<       V(3)=SIN(B) >*/
    v[3] = sin(*b);
/*<       END >*/
    return 0;
} /* sla_dcs2c__ */

/*<       include 'dmxv.f' >*/
/* Subroutine */ int sla_dimxv__(doublereal *dm, doublereal *va, doublereal *
	vb)
{
    integer i, j;
    doublereal w, vw[3];

/* + */
/*     - - - - - - */
/*      D I M X V */
/*     - - - - - - */

/*  Performs the 3-D backward unitary transformation: */

/*     vector VB = (inverse of matrix DM) * vector VA */

/*  (double precision) */

/*  (n.b.  the matrix must be unitary, as this routine assumes that */
/*   the inverse and transpose are identical) */

/*  Given: */
/*     DM       dp(3,3)    matrix */
/*     VA       dp(3)      vector */

/*  Returned: */
/*     VB       dp(3)      result vector */

/*  P.T.Wallace   Starlink   March 1986 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DM(3,3),VA(3),VB(3) >*/
/*<       INTEGER I,J >*/
/*<       DOUBLE PRECISION W,VW(3) >*/
/*  Inverse of matrix DM * vector VA -> vector VW */
/*<       DO J=1,3 >*/
    /* Parameter adjustments */
    --vb;
    --va;
    dm -= 4;

    /* Function Body */
    for (j = 1; j <= 3; ++j) {
/*<          W=0D0 >*/
	w = 0.;
/*<          DO I=1,3 >*/
	for (i = 1; i <= 3; ++i) {
/*<             W=W+DM(I,J)*VA(I) >*/
	    w += dm[i + j * 3] * va[i];
/*<          END DO >*/
	}
/*<          VW(J)=W >*/
	vw[j - 1] = w;
/*<       END DO >*/
    }
/*  Vector VW -> vector VB */
/*<       DO J=1,3 >*/
    for (j = 1; j <= 3; ++j) {
/*<          VB(J)=VW(J) >*/
	vb[j] = vw[j - 1];
/*<       END DO >*/
    }
/*<       END >*/
    return 0;
} /* sla_dimxv__ */

/*<       include 'drange.f' >*/
/* Subroutine */ int sla_dmxv__(doublereal *dm, doublereal *va, doublereal *
	vb)
{
    integer i, j;
    doublereal w, vw[3];

/* + */
/*     - - - - - */
/*      D M X V */
/*     - - - - - */

/*  Performs the 3-D forward unitary transformation: */

/*     vector VB = matrix DM * vector VA */

/*  (double precision) */

/*  Given: */
/*     DM       dp(3,3)    matrix */
/*     VA       dp(3)      vector */

/*  Returned: */
/*     VB       dp(3)      result vector */

/*  P.T.Wallace   Starlink   March 1986 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DM(3,3),VA(3),VB(3) >*/
/*<       INTEGER I,J >*/
/*<       DOUBLE PRECISION W,VW(3) >*/
/*  Matrix DM * vector VA -> vector VW */
/*<       DO J=1,3 >*/
    /* Parameter adjustments */
    --vb;
    --va;
    dm -= 4;

    /* Function Body */
    for (j = 1; j <= 3; ++j) {
/*<          W=0D0 >*/
	w = 0.;
/*<          DO I=1,3 >*/
	for (i = 1; i <= 3; ++i) {
/*<             W=W+DM(J,I)*VA(I) >*/
	    w += dm[j + i * 3] * va[i];
/*<          END DO >*/
	}
/*<          VW(J)=W >*/
	vw[j - 1] = w;
/*<       END DO >*/
    }
/*  Vector VW -> vector VB */
/*<       DO J=1,3 >*/
    for (j = 1; j <= 3; ++j) {
/*<          VB(J)=VW(J) >*/
	vb[j] = vw[j - 1];
/*<       END DO >*/
    }
/*<       END >*/
    return 0;
} /* sla_dmxv__ */

/*<       include 'dranrm.f' >*/
doublereal sla_drange__(doublereal *angle)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *), d_sign(doublereal *, doublereal 
	    *);

/* + */
/*     - - - - - - - */
/*      D R A N G E */
/*     - - - - - - - */

/*  Normalize angle into range +/- pi  (double precision) */

/*  Given: */
/*     ANGLE     dp      the angle in radians */

/*  The result (double precision) is ANGLE expressed in the range +/- pi. 
*/

/*  P.T.Wallace   Starlink   23 November 1995 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION ANGLE >*/
/*<       DOUBLE PRECISION DPI,D2PI >*/
/*<       PARAMETER (DPI=3.141592653589793238462643D0) >*/
/*<       PARAMETER (D2PI=6.283185307179586476925287D0) >*/
/*<       sla_DRANGE=MOD(ANGLE,D2PI) >*/
    ret_val = d_mod(angle, &c_b7);
/* cc      IF (ABS(sla_DRANGE).GE.DPI) */
/*<        >*/
    if (ret_val >= 3.141592653589793238462643 || -ret_val >= 
	    3.141592653589793238462643) {
	ret_val -= d_sign(&c_b7, angle);
    }
/*<       END >*/
    return ret_val;
} /* sla_drange__ */

/*<       include 'dvdv.f' >*/
doublereal sla_dranrm__(doublereal *angle)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *);

/* + */
/*     - - - - - - - */
/*      D R A N R M */
/*     - - - - - - - */

/*  Normalize angle into range 0-2 pi  (double precision) */

/*  Given: */
/*     ANGLE     dp      the angle in radians */

/*  The result is ANGLE expressed in the range 0-2 pi (double */
/*  precision). */

/*  P.T.Wallace   Starlink   23 November 1995 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION ANGLE >*/
/*<       DOUBLE PRECISION D2PI >*/
/*<       PARAMETER (D2PI=6.283185307179586476925286766559D0) >*/
/*<       sla_DRANRM=MOD(ANGLE,D2PI) >*/
    ret_val = d_mod(angle, &c_b10);
/*<       IF (sla_DRANRM.LT.0D0) sla_DRANRM=sla_DRANRM+D2PI >*/
    if (ret_val < 0.) {
	ret_val += 6.283185307179586476925286766559;
    }
/*<       END >*/
    return ret_val;
} /* sla_dranrm__ */

/*<       include 'eg50.f' >*/
doublereal sla_dvdv__(doublereal *va, doublereal *vb)
{
    /* System generated locals */
    doublereal ret_val;

/* + */
/*     - - - - - */
/*      D V D V */
/*     - - - - - */

/*  Scalar product of two 3-vectors  (double precision) */

/*  Given: */
/*      VA      dp(3)     first vector */
/*      VB      dp(3)     second vector */

/*  The result is the scalar product VA.VB (double precision) */

/*  P.T.Wallace   Starlink   November 1984 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION VA(3),VB(3) >*/
/*<       sla_DVDV=VA(1)*VB(1)+VA(2)*VB(2)+VA(3)*VB(3) >*/
    /* Parameter adjustments */
    --vb;
    --va;

    /* Function Body */
    ret_val = va[1] * vb[1] + va[2] * vb[2] + va[3] * vb[3];
/*<       END >*/
    return ret_val;
} /* sla_dvdv__ */

/*<       include 'epb2d.f' >*/
/* Subroutine */ int sla_eg50__(doublereal *dr, doublereal *dd, doublereal *
	dl, doublereal *db)
{
    /* Initialized data */

    static doublereal rmat[9]	/* was [3][3] */ = { -.066988739415,
	    .492728466075,-.867600811151,-.872755765852,-.45034695802,
	    -.188374601723,-.483538914632,.744584633283,.460199784784 };

    extern /* Subroutine */ int sla_dmxv__(doublereal *, doublereal *, 
	    doublereal *), sla_dcs2c__(doublereal *, doublereal *, doublereal 
	    *), sla_dcc2s__(doublereal *, doublereal *, doublereal *);
    doublereal d, r;
    extern /* Subroutine */ int sla_subet__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal v1[3], v2[3];
    extern doublereal sla_drange__(doublereal *), sla_dranrm__(doublereal *);

/* + */
/*     - - - - - */
/*      E G 5 0 */
/*     - - - - - */

/*  Transformation from B1950.0 'FK4' equatorial coordinates to */
/*  IAU 1958 galactic coordinates (double precision) */

/*  Given: */
/*     DR,DD       dp       B1950.0 'FK4' RA,Dec */

/*  Returned: */
/*     DL,DB       dp       galactic longitude and latitude L2,B2 */

/*  (all arguments are radians) */

/*  Called: */
/*     sla_DCS2C, sla_DMXV, sla_DCC2S, sla_SUBET, sla_DRANRM, sla_DRANGE 
*/

/*  Note: */
/*     The equatorial coordinates are B1950.0 'FK4'.  Use the */
/*     routine sla_EQGAL if conversion from J2000.0 coordinates */
/*     is required. */

/*  Reference: */
/*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960) */

/*  P.T.Wallace   Starlink   5 September 1993 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DR,DD,DL,DB >*/
/*<       DOUBLE PRECISION sla_DRANRM,sla_DRANGE >*/
/*<       DOUBLE PRECISION V1(3),V2(3),R,D >*/

/*  L2,B2 system of galactic coordinates */

/*  P = 192.25       RA of galactic north pole (mean B1950.0) */
/*  Q =  62.6        inclination of galactic to mean B1950.0 equator */
/*  R =  33          longitude of ascending node */

/*  P,Q,R are degrees */


/*  Equatorial to galactic rotation matrix */

/*  The Euler angles are P, Q, 90-R, about the z then y then */
/*  z axes. */

/*         +CP.CQ.SR-SP.CR     +SP.CQ.SR+CP.CR     -SQ.SR */

/*         -CP.CQ.CR-SP.SR     -SP.CQ.CR+CP.SR     +SQ.CR */

/*         +CP.SQ              +SP.SQ              +CQ */

/*<       DOUBLE PRECISION RMAT(3,3) >*/
/*<        >*/
/*  Remove E-terms */
/*<       CALL sla_SUBET(DR,DD,1950D0,R,D) >*/
    sla_subet__(dr, dd, &c_b13, &r, &d);
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(R,D,V1) >*/
    sla_dcs2c__(&r, &d, v1);
/*  Rotate to galactic */
/*<       CALL sla_DMXV(RMAT,V1,V2) >*/
    sla_dmxv__(rmat, v1, v2);
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V2,DL,DB) >*/
    sla_dcc2s__(v2, dl, db);
/*  Express angles in conventional ranges */
/*<       DL=sla_DRANRM(DL) >*/
    *dl = sla_dranrm__(dl);
/*<       DB=sla_DRANGE(DB) >*/
    *db = sla_drange__(db);
/*<       END >*/
    return 0;
} /* sla_eg50__ */

/*<       include 'epj.f' >*/
doublereal sla_epb2d__(doublereal *epb)
{
    /* System generated locals */
    doublereal ret_val;

/* + */
/*     - - - - - - */
/*      E P B 2 D */
/*     - - - - - - */

/*  Conversion of Besselian Epoch to Modified Julian Date */
/*  (double precision) */

/*  Given: */
/*     EPB      dp       Besselian Epoch */

/*  The result is the Modified Julian Date (JD - 2400000.5). */

/*  Reference: */
/*     Lieske,J.H., 1979. Astron.Astrophys.,73,282. */

/*  P.T.Wallace   Starlink   February 1984 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION EPB >*/
/*<       sla_EPB2D = 15019.81352D0 + (EPB-1900D0)*365.242198781D0 >*/
    ret_val = (*epb - 1900.) * 365.242198781 + 15019.81352;
/*<       END >*/
    return ret_val;
} /* sla_epb2d__ */

/*<       include 'eqgal.f' >*/
doublereal sla_epj__(doublereal *date)
{
    /* System generated locals */
    doublereal ret_val;

/* + */
/*     - - - - */
/*      E P J */
/*     - - - - */

/*  Conversion of Modified Julian Date to Julian Epoch (double precision) 
*/

/*  Given: */
/*     DATE     dp       Modified Julian Date (JD - 2400000.5) */

/*  The result is the Julian Epoch. */

/*  Reference: */
/*     Lieske,J.H., 1979. Astron.Astrophys.,73,282. */

/*  P.T.Wallace   Starlink   February 1984 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DATE >*/
/*<       sla_EPJ = 2000D0 + (DATE-51544.5D0)/365.25D0 >*/
    ret_val = (*date - 51544.5) / 365.25 + 2e3;
/*<       END >*/
    return ret_val;
} /* sla_epj__ */

/*<       include 'etrms.f' >*/
/* Subroutine */ int sla_eqgal__(doublereal *dr, doublereal *dd, doublereal *
	dl, doublereal *db)
{
    /* Initialized data */

    static doublereal rmat[9]	/* was [3][3] */ = { -.054875539726,
	    .494109453312,-.867666135858,-.87343710801,-.444829589425,
	    -.198076386122,-.483834985808,.74698225181,.455983795705 };

    extern /* Subroutine */ int sla_dmxv__(doublereal *, doublereal *, 
	    doublereal *), sla_dcs2c__(doublereal *, doublereal *, doublereal 
	    *), sla_dcc2s__(doublereal *, doublereal *, doublereal *);
    doublereal v1[3], v2[3];
    extern doublereal sla_drange__(doublereal *), sla_dranrm__(doublereal *);

/* + */
/*     - - - - - - */
/*      E Q G A L */
/*     - - - - - - */

/*  Transformation from J2000.0 equatorial coordinates to */
/*  IAU 1958 galactic coordinates (double precision) */

/*  Given: */
/*     DR,DD       dp       J2000.0 RA,Dec */

/*  Returned: */
/*     DL,DB       dp       galactic longitude and latitude L2,B2 */

/*  (all arguments are radians) */

/*  Called: */
/*     sla_DCS2C, sla_DMXV, sla_DCC2S, sla_DRANRM, sla_DRANGE */

/*  Note: */
/*     The equatorial coordinates are J2000.0.  Use the routine */
/*     sla_EG50 if conversion from B1950.0 'FK4' coordinates is */
/*     required. */

/*  Reference: */
/*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960) */

/*  P.T.Wallace   Starlink   November 1988 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DR,DD,DL,DB >*/
/*<       DOUBLE PRECISION sla_DRANRM,sla_DRANGE >*/
/*<       DOUBLE PRECISION V1(3),V2(3) >*/

/*  L2,B2 system of galactic coordinates */

/*  P = 192.25       RA of galactic north pole (mean B1950.0) */
/*  Q =  62.6        inclination of galactic to mean B1950.0 equator */
/*  R =  33          longitude of ascending node */

/*  P,Q,R are degrees */

/*  Equatorial to galactic rotation matrix (J2000.0), obtained by */
/*  applying the standard FK4 to FK5 transformation, for inertially */
/*  zero proper motion, to the columns of the B1950 equatorial to */
/*  galactic rotation matrix: */

/*<       DOUBLE PRECISION RMAT(3,3) >*/
/*<        >*/
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(DR,DD,V1) >*/
    sla_dcs2c__(dr, dd, v1);
/*  Equatorial to galactic */
/*<       CALL sla_DMXV(RMAT,V1,V2) >*/
    sla_dmxv__(rmat, v1, v2);
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V2,DL,DB) >*/
    sla_dcc2s__(v2, dl, db);
/*  Express in conventional ranges */
/*<       DL=sla_DRANRM(DL) >*/
    *dl = sla_dranrm__(dl);
/*<       DB=sla_DRANGE(DB) >*/
    *db = sla_drange__(db);
/*<       END >*/
    return 0;
} /* sla_eqgal__ */

/*<       include 'fk45z.f' >*/
/* Subroutine */ int sla_etrms__(doublereal *ep, doublereal *ev)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal e, p, t, e0, ek, cp;

/* + */
/*     - - - - - - */
/*      E T R M S */
/*     - - - - - - */

/*  Compute the E-terms (elliptic component of annual aberration) */
/*  vector (double precision) */

/*  Given: */
/*     EP      dp      Besselian epoch */

/*  Returned: */
/*     EV      dp(3)   E-terms as (dx,dy,dz) */

/*  Note the use of the J2000 aberration constant (20.49552 arcsec). */
/*  This is a reflection of the fact that the E-terms embodied in */
/*  existing star catalogues were computed from a variety of */
/*  aberration constants.  Rather than adopting one of the old */
/*  constants the latest value is used here. */

/*  References: */
/*     1  Smith, C.A. et al., 1989.  Astr.J. 97, 265. */
/*     2  Yallop, B.D. et al., 1989.  Astr.J. 97, 274. */

/*  P.T.Wallace   Starlink   23 August 1996 */

/*  Copyright (C) 1996 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION EP,EV(3) >*/
/*  Arcseconds to radians */
/*<       DOUBLE PRECISION AS2R >*/
/*<       PARAMETER (AS2R=0.484813681109535994D-5) >*/
/*<       DOUBLE PRECISION T,E,E0,P,EK,CP >*/
/*  Julian centuries since B1950 */
/*<       T=(EP-1950D0)*1.00002135903D-2 >*/
    /* Parameter adjustments */
    --ev;

    /* Function Body */
    t = (*ep - 1950.) * .0100002135903;
/*  Eccentricity */
/*<       E=0.01673011D0-(0.00004193D0+0.000000126D0*T)*T >*/
    e = .01673011 - (t * 1.26e-7 + 4.193e-5) * t;
/*  Mean obliquity */
/*<       E0=(84404.836D0-(46.8495D0+(0.00319D0+0.00181D0*T)*T)*T)*AS2R >*/
    e0 = (84404.836 - ((t * .00181 + .00319) * t + 46.8495) * t) * 
	    4.84813681109535994e-6;
/*  Mean longitude of perihelion */
/*<       P=(1015489.951D0+(6190.67D0+(1.65D0+0.012D0*T)*T)*T)*AS2R >*/
    p = (((t * .012 + 1.65) * t + 6190.67) * t + 1015489.951) * 
	    4.84813681109535994e-6;
/*  E-terms */
/*<       EK=E*20.49552D0*AS2R >*/
    ek = e * 20.49552 * 4.84813681109535994e-6;
/*<       CP=COS(P) >*/
    cp = cos(p);
/*<       EV(1)= EK*SIN(P) >*/
    ev[1] = ek * sin(p);
/*<       EV(2)=-EK*CP*COS(E0) >*/
    ev[2] = -ek * cp * cos(e0);
/*<       EV(3)=-EK*CP*SIN(E0) >*/
    ev[3] = -ek * cp * sin(e0);
/*<       END >*/
    return 0;
} /* sla_etrms__ */

/*<       include 'fk524.f' >*/
/* Subroutine */ int sla_fk45z__(doublereal *r1950, doublereal *d1950, 
	doublereal *bepoch, doublereal *r2000, doublereal *d2000)
{
    /* Initialized data */

    static doublereal a[3] = { -1.62557e-6,-3.1919e-7,-1.3843e-7 };
    static doublereal ad[3] = { .001245,-.00158,-6.59e-4 };
    static doublereal em[18]	/* was [6][3] */ = { .9999256782,.011182061,
	    .0048579479,-5.51e-4,.238514,-.435623,-.0111820611,.9999374784,
	    -2.71474e-5,-.238565,-.002667,.012254,-.0048579477,-2.71765e-5,
	    .9999881997,.435739,-.008541,.002117 };

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern doublereal sla_epb2d__(doublereal *);
    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *);
    integer i, j;
    doublereal w, a1[3], r0[3], v1[3], v2[6];
    extern doublereal sla_dranrm__(doublereal *), sla_epj__(doublereal *);

/* + */
/*     - - - - - - */
/*      F K 4 5 Z */
/*     - - - - - - */

/*  Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero */
/*  proper motion in an inertial frame (double precision) */

/*  This routine converts stars from the old, Bessel-Newcomb, FK4 */
/*  system to the new, IAU 1976, FK5, Fricke system, in such a */
/*  way that the FK5 proper motion is zero.  Because such a star */
/*  has, in general, a non-zero proper motion in the FK4 system, */
/*  the routine requires the epoch at which the position in the */
/*  FK4 system was determined. */

/*  The method is from Appendix 2 of Ref 1, but using the constants */
/*  of Ref 4. */

/*  Given: */
/*     R1950,D1950     dp    B1950.0 FK4 RA,Dec at epoch (rad) */
/*     BEPOCH          dp    Besselian epoch (e.g. 1979.3D0) */

/*  Returned: */
/*     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad) */

/*  Notes: */

/*  1)  The epoch BEPOCH is strictly speaking Besselian, but */
/*      if a Julian epoch is supplied the result will be */
/*      affected only to a negligible extent. */

/*  2)  Conversion from Besselian epoch 1950.0 to Julian epoch */
/*      2000.0 only is provided for.  Conversions involving other */
/*      epochs will require use of the appropriate precession, */
/*      proper motion, and E-terms routines before and/or */
/*      after FK45Z is called. */

/*  3)  In the FK4 catalogue the proper motions of stars within */
/*      10 degrees of the poles do not embody the differential */
/*      E-term effect and should, strictly speaking, be handled */
/*      in a different manner from stars outside these regions. */
/*      However, given the general lack of homogeneity of the star */
/*      data available for routine astrometry, the difficulties of */
/*      handling positions that may have been determined from */
/*      astrometric fields spanning the polar and non-polar regions, */
/*      the likelihood that the differential E-terms effect was not */
/*      taken into account when allowing for proper motion in past */
/*      astrometry, and the undesirability of a discontinuity in */
/*      the algorithm, the decision has been made in this routine to */
/*      include the effect of differential E-terms on the proper */
/*      motions for all stars, whether polar or not.  At epoch 2000, */
/*      and measuring on the sky rather than in terms of dRA, the */
/*      errors resulting from this simplification are less than */
/*      1 milliarcsecond in position and 1 milliarcsecond per */
/*      century in proper motion. */

/*  References: */

/*     1  Aoki,S., et al, 1983.  Astron.Astrophys., 128, 263. */

/*     2  Smith, C.A. et al, 1989.  "The transformation of astrometric */
/*        catalog systems to the equinox J2000.0".  Astron.J. 97, 265. */

/*     3  Yallop, B.D. et al, 1989.  "Transformation of mean star places 
*/
/*        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space". */
/*        Astron.J. 97, 274. */

/*     4  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to */
/*        the Astronomical Almanac", ISBN 0-935702-68-7. */

/*  Called:  sla_DCS2C, sla_EPJ, sla_EPB2D, sla_DCC2S, sla_DRANRM */

/*  P.T.Wallace   Starlink   5 November 1996 */

/*  Copyright (C) 1996 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION R1950,D1950,BEPOCH,R2000,D2000 >*/
/*<       DOUBLE PRECISION D2PI >*/
/*<       PARAMETER (D2PI=6.283185307179586476925287D0) >*/
/*<       DOUBLE PRECISION W >*/
/*<       INTEGER I,J >*/
/*  Position and position+velocity vectors */
/*<       DOUBLE PRECISION R0(3),A1(3),V1(3),V2(6) >*/
/*  Radians per year to arcsec per century */
/*<       DOUBLE PRECISION PMF >*/
/*<       PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI) >*/
/*  Functions */
/*<       DOUBLE PRECISION sla_EPJ,sla_EPB2D,sla_DRANRM >*/

/*  CANONICAL CONSTANTS  (see references) */

/*  Vectors A and Adot, and matrix M (only half of which is needed here) 
*/
/*<       DOUBLE PRECISION A(3),AD(3),EM(6,3) >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(R1950,D1950,R0) >*/
    sla_dcs2c__(r1950, d1950, r0);
/*  Adjust vector A to give zero proper motion in FK5 */
/*<       W=(BEPOCH-1950D0)/PMF >*/
    w = (*bepoch - 1950.) / 20626480.624709636;
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          A1(I)=A(I)+W*AD(I) >*/
	a1[i - 1] = a[i - 1] + w * ad[i - 1];
/*<       END DO >*/
    }
/*  Remove e-terms */
/*<       W=R0(1)*A1(1)+R0(2)*A1(2)+R0(3)*A1(3) >*/
    w = r0[0] * a1[0] + r0[1] * a1[1] + r0[2] * a1[2];
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          V1(I)=R0(I)-A1(I)+W*R0(I) >*/
	v1[i - 1] = r0[i - 1] - a1[i - 1] + w * r0[i - 1];
/*<       END DO >*/
    }
/*  Convert position vector to Fricke system */
/*<       DO I=1,6 >*/
    for (i = 1; i <= 6; ++i) {
/*<          W=0D0 >*/
	w = 0.;
/*<          DO J=1,3 >*/
	for (j = 1; j <= 3; ++j) {
/*<             W=W+EM(I,J)*V1(J) >*/
	    w += em[i + j * 6 - 7] * v1[j - 1];
/*<          END DO >*/
	}
/*<          V2(I)=W >*/
	v2[i - 1] = w;
/*<       END DO >*/
    }
/*  Allow for fictitious proper motion in FK4 */
/*<       W=(sla_EPJ(sla_EPB2D(BEPOCH))-2000D0)/PMF >*/
    d__1 = sla_epb2d__(bepoch);
    w = (sla_epj__(&d__1) - 2e3) / 20626480.624709636;
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          V2(I)=V2(I)+W*V2(I+3) >*/
	v2[i - 1] += w * v2[i + 2];
/*<       END DO >*/
    }
/*  Revert to spherical coordinates */
/*<       CALL sla_DCC2S(V2,W,D2000) >*/
    sla_dcc2s__(v2, &w, d2000);
/*<       R2000=sla_DRANRM(W) >*/
    *r2000 = sla_dranrm__(&w);
/*<       END >*/
    return 0;
} /* sla_fk45z__ */

/*<       include 'fk54z.f' >*/
/* Subroutine */ int sla_fk524__(doublereal *r2000, doublereal *d2000, 
	doublereal *dr2000, doublereal *dd2000, doublereal *p2000, doublereal 
	*v2000, doublereal *r1950, doublereal *d1950, doublereal *dr1950, 
	doublereal *dd1950, doublereal *p1950, doublereal *v1950)
{
    /* Initialized data */

    static doublereal a[6] = { -1.62557e-6,-3.1919e-7,-1.3843e-7,.001245,
	    -.00158,-6.59e-4 };
    static doublereal emi[36]	/* was [6][6] */ = { .9999256795,-.0111814828,
	    -.004859004,-5.51e-4,-.23856,.43573,.0111814828,.9999374849,
	    -2.71557e-5,.238509,-.002667,-.008541,.0048590039,-2.71771e-5,
	    .9999881946,-.435614,.012254,.002117,-2.4238984e-6,2.710544e-8,
	    1.177742e-8,.99990432,-.01118145,-.00485852,-2.710544e-8,
	    -2.42392702e-6,6.585e-11,.01118145,.99991613,-2.716e-5,
	    -1.177742e-8,6.585e-11,-2.42404995e-6,.00485852,-2.717e-5,
	    .99996684 };

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), sqrt(doublereal), atan2(
	    doublereal, doublereal);

    /* Local variables */
    doublereal rxyz, d;
    integer i, j;
    doublereal r, w, x, y, z, v1[6], v2[6], rxysq, cd, cr, sd, ud, wd, xd, yd,
	     zd, sr, ur, px, rv, rxy;

/* + */
/*     - - - - - - */
/*      F K 5 2 4 */
/*     - - - - - - */

/*  Convert J2000.0 FK5 star data to B1950.0 FK4 (double precision) */

/*  This routine converts stars from the new, IAU 1976, FK5, Fricke */
/*  system, to the old, Bessel-Newcomb, FK4 system.  The precepts */
/*  of Smith et al (Ref 1) are followed, using the implementation */
/*  by Yallop et al (Ref 2) of a matrix method due to Standish. */
/*  Kinoshita's development of Andoyer's post-Newcomb precession is */
/*  used.  The numerical constants from Seidelmann et al (Ref 3) are */
/*  used canonically. */

/*  Given:  (all J2000.0,FK5) */
/*     R2000,D2000     dp    J2000.0 RA,Dec (rad) */
/*     DR2000,DD2000   dp    J2000.0 proper motions (rad/Jul.yr) */
/*     P2000           dp    parallax (arcsec) */
/*     V2000           dp    radial velocity (km/s, +ve = moving away) */

/*  Returned:  (all B1950.0,FK4) */
/*     R1950,D1950     dp    B1950.0 RA,Dec (rad) */
/*     DR1950,DD1950   dp    B1950.0 proper motions (rad/trop.yr) */
/*     P1950           dp    parallax (arcsec) */
/*     V1950           dp    radial velocity (km/s, +ve = moving away) */

/*  Notes: */

/*  1)  The proper motions in RA are dRA/dt rather than */
/*      cos(Dec)*dRA/dt, and are per year rather than per century. */

/*  2)  Note that conversion from Julian epoch 2000.0 to Besselian */
/*      epoch 1950.0 only is provided for.  Conversions involving */
/*      other epochs will require use of the appropriate precession, */
/*      proper motion, and E-terms routines before and/or after */
/*      FK524 is called. */

/*  3)  In the FK4 catalogue the proper motions of stars within */
/*      10 degrees of the poles do not embody the differential */
/*      E-term effect and should, strictly speaking, be handled */
/*      in a different manner from stars outside these regions. */
/*      However, given the general lack of homogeneity of the star */
/*      data available for routine astrometry, the difficulties of */
/*      handling positions that may have been determined from */
/*      astrometric fields spanning the polar and non-polar regions, */
/*      the likelihood that the differential E-terms effect was not */
/*      taken into account when allowing for proper motion in past */
/*      astrometry, and the undesirability of a discontinuity in */
/*      the algorithm, the decision has been made in this routine to */
/*      include the effect of differential E-terms on the proper */
/*      motions for all stars, whether polar or not.  At epoch 2000, */
/*      and measuring on the sky rather than in terms of dRA, the */
/*      errors resulting from this simplification are less than */
/*      1 milliarcsecond in position and 1 milliarcsecond per */
/*      century in proper motion. */

/*  References: */

/*     1  Smith, C.A. et al, 1989.  "The transformation of astrometric */
/*        catalog systems to the equinox J2000.0".  Astron.J. 97, 265. */

/*     2  Yallop, B.D. et al, 1989.  "Transformation of mean star places 
*/
/*        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space". */
/*        Astron.J. 97, 274. */

/*     3  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to */
/*        the Astronomical Almanac", ISBN 0-935702-68-7. */

/*  P.T.Wallace   Starlink   19 December 1993 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<        >*/
/*  Miscellaneous */
/*<       DOUBLE PRECISION R,D,UR,UD,PX,RV >*/
/*<       DOUBLE PRECISION SR,CR,SD,CD,X,Y,Z,W >*/
/*<       DOUBLE PRECISION V1(6),V2(6) >*/
/*<       DOUBLE PRECISION XD,YD,ZD >*/
/*<       DOUBLE PRECISION RXYZ,WD,RXYSQ,RXY >*/
/*<       INTEGER I,J >*/
/*  2Pi */
/*<       DOUBLE PRECISION D2PI >*/
/*<       PARAMETER (D2PI=6.283185307179586476925287D0) >*/
/*  Radians per year to arcsec per century */
/*<       DOUBLE PRECISION PMF >*/
/*<       PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI) >*/
/*  Small number to avoid arithmetic problems */
/*<       DOUBLE PRECISION TINY >*/
/*<       PARAMETER (TINY=1D-30) >*/

/*  CANONICAL CONSTANTS  (see references) */

/*  Km per sec to AU per tropical century */
/*  = 86400 * 36524.2198782 / 149597870 */
/*<       DOUBLE PRECISION VF >*/
/*<       PARAMETER (VF=21.095D0) >*/
/*  Constant vector and matrix (by columns) */
/*<       DOUBLE PRECISION A(6),EMI(6,6) >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*<        >*/
/*  Pick up J2000 data (units radians and arcsec/JC) */
/*<       R=R2000 >*/
    r = *r2000;
/*<       D=D2000 >*/
    d = *d2000;
/*<       UR=DR2000*PMF >*/
    ur = *dr2000 * 20626480.624709636;
/*<       UD=DD2000*PMF >*/
    ud = *dd2000 * 20626480.624709636;
/*<       PX=P2000 >*/
    px = *p2000;
/*<       RV=V2000 >*/
    rv = *v2000;
/*  Spherical to Cartesian */
/*<       SR=SIN(R) >*/
    sr = sin(r);
/*<       CR=COS(R) >*/
    cr = cos(r);
/*<       SD=SIN(D) >*/
    sd = sin(d);
/*<       CD=COS(D) >*/
    cd = cos(d);
/*<       X=CR*CD >*/
    x = cr * cd;
/*<       Y=SR*CD >*/
    y = sr * cd;
/*<       Z=   SD >*/
    z = sd;
/*<       W=VF*RV*PX >*/
    w = rv * 21.095 * px;
/*<       V1(1)=X >*/
    v1[0] = x;
/*<       V1(2)=Y >*/
    v1[1] = y;
/*<       V1(3)=Z >*/
    v1[2] = z;
/*<       V1(4)=-UR*Y-CR*SD*UD+W*X >*/
    v1[3] = -ur * y - cr * sd * ud + w * x;
/*<       V1(5)= UR*X-SR*SD*UD+W*Y >*/
    v1[4] = ur * x - sr * sd * ud + w * y;
/*<       V1(6)=         CD*UD+W*Z >*/
    v1[5] = cd * ud + w * z;
/*  Convert position+velocity vector to BN system */
/*<       DO I=1,6 >*/
    for (i = 1; i <= 6; ++i) {
/*<          W=0D0 >*/
	w = 0.;
/*<          DO J=1,6 >*/
	for (j = 1; j <= 6; ++j) {
/*<             W=W+EMI(I,J)*V1(J) >*/
	    w += emi[i + j * 6 - 7] * v1[j - 1];
/*<          END DO >*/
	}
/*<          V2(I)=W >*/
	v2[i - 1] = w;
/*<       END DO >*/
    }
/*  Position vector components and magnitude */
/*<       X=V2(1) >*/
    x = v2[0];
/*<       Y=V2(2) >*/
    y = v2[1];
/*<       Z=V2(3) >*/
    z = v2[2];
/*<       RXYZ=SQRT(X*X+Y*Y+Z*Z) >*/
    rxyz = sqrt(x * x + y * y + z * z);
/*  Apply E-terms to position */
/*<       W=X*A(1)+Y*A(2)+Z*A(3) >*/
    w = x * a[0] + y * a[1] + z * a[2];
/*<       X=X+A(1)*RXYZ-W*X >*/
    x = x + a[0] * rxyz - w * x;
/*<       Y=Y+A(2)*RXYZ-W*Y >*/
    y = y + a[1] * rxyz - w * y;
/*<       Z=Z+A(3)*RXYZ-W*Z >*/
    z = z + a[2] * rxyz - w * z;
/*  Recompute magnitude */
/*<       RXYZ=SQRT(X*X+Y*Y+Z*Z) >*/
    rxyz = sqrt(x * x + y * y + z * z);
/*  Apply E-terms to both position and velocity */
/*<       X=V2(1) >*/
    x = v2[0];
/*<       Y=V2(2) >*/
    y = v2[1];
/*<       Z=V2(3) >*/
    z = v2[2];
/*<       W=X*A(1)+Y*A(2)+Z*A(3) >*/
    w = x * a[0] + y * a[1] + z * a[2];
/*<       WD=X*A(4)+Y*A(5)+Z*A(6) >*/
    wd = x * a[3] + y * a[4] + z * a[5];
/*<       X=X+A(1)*RXYZ-W*X >*/
    x = x + a[0] * rxyz - w * x;
/*<       Y=Y+A(2)*RXYZ-W*Y >*/
    y = y + a[1] * rxyz - w * y;
/*<       Z=Z+A(3)*RXYZ-W*Z >*/
    z = z + a[2] * rxyz - w * z;
/*<       XD=V2(4)+A(4)*RXYZ-WD*X >*/
    xd = v2[3] + a[3] * rxyz - wd * x;
/*<       YD=V2(5)+A(5)*RXYZ-WD*Y >*/
    yd = v2[4] + a[4] * rxyz - wd * y;
/*<       ZD=V2(6)+A(6)*RXYZ-WD*Z >*/
    zd = v2[5] + a[5] * rxyz - wd * z;
/*  Convert to spherical */
/*<       RXYSQ=X*X+Y*Y >*/
    rxysq = x * x + y * y;
/*<       RXY=SQRT(RXYSQ) >*/
    rxy = sqrt(rxysq);
/*<       IF (X.EQ.0D0.AND.Y.EQ.0D0) THEN >*/
    if (x == 0. && y == 0.) {
/*<          R=0D0 >*/
	r = 0.;
/*<       ELSE >*/
    } else {
/*<          R=ATAN2(Y,X) >*/
	r = atan2(y, x);
/*<          IF (R.LT.0.0D0) R=R+D2PI >*/
	if (r < 0.) {
	    r += 6.283185307179586476925287;
	}
/*<       END IF >*/
    }
/*<       D=ATAN2(Z,RXY) >*/
    d = atan2(z, rxy);
/*<       IF (RXY.GT.TINY) THEN >*/
    if (rxy > 1e-30) {
/*<          UR=(X*YD-Y*XD)/RXYSQ >*/
	ur = (x * yd - y * xd) / rxysq;
/*<          UD=(ZD*RXYSQ-Z*(X*XD+Y*YD))/((RXYSQ+Z*Z)*RXY) >*/
	ud = (zd * rxysq - z * (x * xd + y * yd)) / ((rxysq + z * z) * rxy);
/*<       END IF >*/
    }
/*  Radial velocity and parallax */
/*<       IF (PX.GT.TINY) THEN >*/
    if (px > 1e-30) {
/*<          RV=(X*XD+Y*YD+Z*ZD)/(PX*VF*RXYZ) >*/
	rv = (x * xd + y * yd + z * zd) / (px * 21.095 * rxyz);
/*<          PX=PX/RXYZ >*/
	px /= rxyz;
/*<       END IF >*/
    }
/*  Return results */
/*<       R1950=R >*/
    *r1950 = r;
/*<       D1950=D >*/
    *d1950 = d;
/*<       DR1950=UR/PMF >*/
    *dr1950 = ur / 20626480.624709636;
/*<       DD1950=UD/PMF >*/
    *dd1950 = ud / 20626480.624709636;
/*<       P1950=PX >*/
    *p1950 = px;
/*<       V1950=RV >*/
    *v1950 = rv;
/*<       END >*/
    return 0;
} /* sla_fk524__ */

/*<       include 'galeq.f' >*/
/* Subroutine */ int sla_fk54z__(doublereal *r2000, doublereal *d2000, 
	doublereal *bepoch, doublereal *r1950, doublereal *d1950, doublereal *
	dr1950, doublereal *dd1950)
{
    doublereal d, r, px, rv;
    extern /* Subroutine */ int sla_pm__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), 
	    sla_fk524__(doublereal *, doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);

/* + */
/*     - - - - - - */
/*      F K 5 4 Z */
/*     - - - - - - */

/*  Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming */
/*  zero proper motion and parallax (double precision) */

/*  This routine converts star positions from the new, IAU 1976, */
/*  FK5, Fricke system to the old, Bessel-Newcomb, FK4 system. */

/*  Given: */
/*     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad) */
/*     BEPOCH          dp    Besselian epoch (e.g. 1950D0) */

/*  Returned: */
/*     R1950,D1950     dp    B1950.0 FK4 RA,Dec (rad) at epoch BEPOCH */
/*     DR1950,DD1950   dp    B1950.0 FK4 proper motions (rad/trop.yr) */

/*  Notes: */

/*  1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt. */

/*  2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0 */
/*      only is provided for.  Conversions involving other epochs will */
/*      require use of the appropriate precession routines before and */
/*      after this routine is called. */

/*  3)  Unlike in the sla_FK524 routine, the FK5 proper motions, the */
/*      parallax and the radial velocity are presumed zero. */

/*  4)  It is the intention that FK5 should be a close approximation */
/*      to an inertial frame, so that distant objects have zero proper */
/*      motion;  such objects have (in general) non-zero proper motion */
/*      in FK4, and this routine returns those fictitious proper */
/*      motions. */

/*  5)  The position returned by this routine is in the B1950 */
/*      reference frame but at Besselian epoch BEPOCH.  For */
/*      comparison with catalogues the BEPOCH argument will */
/*      frequently be 1950D0. */

/*  Called:  sla_FK524, sla_PM */

/*  P.T.Wallace   Starlink   10 April 1990 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<        >*/
/*<       DOUBLE PRECISION R,D,PX,RV >*/
/*  FK5 equinox J2000 (any epoch) to FK4 equinox B1950 epoch B1950 */
/*<        >*/
    sla_fk524__(r2000, d2000, &c_b21, &c_b21, &c_b21, &c_b21, &r, &d, dr1950, 
	    dd1950, &px, &rv);
/*  Fictitious proper motion to epoch BEPOCH */
/*<        >*/
    sla_pm__(&r, &d, dr1950, dd1950, &c_b21, &c_b21, &c_b13, bepoch, r1950, 
	    d1950);
/*<       END >*/
    return 0;
} /* sla_fk54z__ */

/*<       include 'ge50.f' >*/
/* Subroutine */ int sla_galeq__(doublereal *dl, doublereal *db, doublereal *
	dr, doublereal *dd)
{
    /* Initialized data */

    static doublereal rmat[9]	/* was [3][3] */ = { -.054875539726,
	    .494109453312,-.867666135858,-.87343710801,-.444829589425,
	    -.198076386122,-.483834985808,.74698225181,.455983795705 };

    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *), sla_dimxv__(doublereal *, doublereal *, doublereal *);
    doublereal v1[3], v2[3];
    extern doublereal sla_drange__(doublereal *), sla_dranrm__(doublereal *);

/* + */
/*     - - - - - - */
/*      G A L E Q */
/*     - - - - - - */

/*  Transformation from IAU 1958 galactic coordinates to */
/*  J2000.0 equatorial coordinates (double precision) */

/*  Given: */
/*     DL,DB       dp       galactic longitude and latitude L2,B2 */

/*  Returned: */
/*     DR,DD       dp       J2000.0 RA,Dec */

/*  (all arguments are radians) */

/*  Called: */
/*     sla_DCS2C, sla_DIMXV, sla_DCC2S, sla_DRANRM, sla_DRANGE */

/*  Note: */
/*     The equatorial coordinates are J2000.0.  Use the routine */
/*     sla_GE50 if conversion to B1950.0 'FK4' coordinates is */
/*     required. */

/*  Reference: */
/*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960) */

/*  P.T.Wallace   Starlink   November 1988 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DL,DB,DR,DD >*/
/*<       DOUBLE PRECISION sla_DRANRM,sla_DRANGE >*/
/*<       DOUBLE PRECISION V1(3),V2(3) >*/

/*  L2,B2 system of galactic coordinates */

/*  P = 192.25       RA of galactic north pole (mean B1950.0) */
/*  Q =  62.6        inclination of galactic to mean B1950.0 equator */
/*  R =  33          longitude of ascending node */

/*  P,Q,R are degrees */

/*  Equatorial to galactic rotation matrix (J2000.0), obtained by */
/*  applying the standard FK4 to FK5 transformation, for inertially */
/*  zero proper motion, to the columns of the B1950 equatorial to */
/*  galactic rotation matrix: */

/*<       DOUBLE PRECISION RMAT(3,3) >*/
/*<        >*/
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(DL,DB,V1) >*/
    sla_dcs2c__(dl, db, v1);
/*  Galactic to equatorial */
/*<       CALL sla_DIMXV(RMAT,V1,V2) >*/
    sla_dimxv__(rmat, v1, v2);
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V2,DR,DD) >*/
    sla_dcc2s__(v2, dr, dd);
/*  Express in conventional ranges */
/*<       DR=sla_DRANRM(DR) >*/
    *dr = sla_dranrm__(dr);
/*<       DD=sla_DRANGE(DD) >*/
    *dd = sla_drange__(dd);
/*<       END >*/
    return 0;
} /* sla_galeq__ */

/*<       include 'pm.f' >*/
/* Subroutine */ int sla_ge50__(doublereal *dl, doublereal *db, doublereal *
	dr, doublereal *dd)
{
    /* Initialized data */

    static doublereal rmat[9]	/* was [3][3] */ = { -.066988739415,
	    .492728466075,-.867600811151,-.872755765852,-.45034695802,
	    -.188374601723,-.483538914632,.744584633283,.460199784784 };

    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *);
    doublereal d, r;
    extern /* Subroutine */ int sla_addet__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), sla_dimxv__(doublereal 
	    *, doublereal *, doublereal *);
    doublereal v1[3], v2[3], de, re;
    extern doublereal sla_drange__(doublereal *), sla_dranrm__(doublereal *);

/* + */
/*     - - - - - */
/*      G E 5 0 */
/*     - - - - - */

/*  Transformation from IAU 1958 galactic coordinates to */
/*  B1950.0 'FK4' equatorial coordinates (double precision) */

/*  Given: */
/*     DL,DB       dp       galactic longitude and latitude L2,B2 */

/*  Returned: */
/*     DR,DD       dp       B1950.0 'FK4' RA,Dec */

/*  (all arguments are radians) */

/*  Called: */
/*     sla_DCS2C, sla_DIMXV, sla_DCC2S, sla_ADDET, sla_DRANRM, sla_DRANGE 
*/

/*  Note: */
/*     The equatorial coordinates are B1950.0 'FK4'.  Use the */
/*     routine sla_GALEQ if conversion to J2000.0 coordinates */
/*     is required. */

/*  Reference: */
/*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960) */

/*  P.T.Wallace   Starlink   5 September 1993 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION DL,DB,DR,DD >*/
/*<       DOUBLE PRECISION sla_DRANRM,sla_DRANGE >*/
/*<       DOUBLE PRECISION V1(3),V2(3),R,D,RE,DE >*/

/*  L2,B2 system of galactic coordinates */

/*  P = 192.25       RA of galactic north pole (mean B1950.0) */
/*  Q =  62.6        inclination of galactic to mean B1950.0 equator */
/*  R =  33          longitude of ascending node */

/*  P,Q,R are degrees */


/*  Equatorial to galactic rotation matrix */

/*  The Euler angles are P, Q, 90-R, about the z then y then */
/*  z axes. */

/*         +CP.CQ.SR-SP.CR     +SP.CQ.SR+CP.CR     -SQ.SR */

/*         -CP.CQ.CR-SP.SR     -SP.CQ.CR+CP.SR     +SQ.CR */

/*         +CP.SQ              +SP.SQ              +CQ */

/*<       DOUBLE PRECISION RMAT(3,3) >*/
/*<        >*/
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(DL,DB,V1) >*/
    sla_dcs2c__(dl, db, v1);
/*  Rotate to mean B1950.0 */
/*<       CALL sla_DIMXV(RMAT,V1,V2) >*/
    sla_dimxv__(rmat, v1, v2);
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V2,R,D) >*/
    sla_dcc2s__(v2, &r, &d);
/*  Introduce E-terms */
/*<       CALL sla_ADDET(R,D,1950D0,RE,DE) >*/
    sla_addet__(&r, &d, &c_b13, &re, &de);
/*  Express in conventional ranges */
/*<       DR=sla_DRANRM(RE) >*/
    *dr = sla_dranrm__(&re);
/*<       DD=sla_DRANGE(DE) >*/
    *dd = sla_drange__(&de);
/*<       END >*/
    return 0;
} /* sla_ge50__ */

/*<       include 'subet.f' >*/
/* Subroutine */ int sla_pm__(doublereal *r0, doublereal *d0, doublereal *pr, 
	doublereal *pd, doublereal *px, doublereal *rv, doublereal *ep0, 
	doublereal *ep1, doublereal *r1, doublereal *d1)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *);
    integer i;
    doublereal p[3], t, w, em[3];
    extern doublereal sla_dranrm__(doublereal *);

/* + */
/*     - - - */
/*      P M */
/*     - - - */

/*  Apply corrections for proper motion to a star RA,Dec */
/*  (double precision) */

/*  References: */
/*     1984 Astronomical Almanac, pp B39-B41. */
/*     (also Lederle & Schwan, Astron. Astrophys. 134, */
/*      1-6, 1984) */

/*  Given: */
/*     R0,D0    dp     RA,Dec at epoch EP0 (rad) */
/*     PR,PD    dp     proper motions:  RA,Dec changes per year of epoch 
*/
/*     PX       dp     parallax (arcsec) */
/*     RV       dp     radial velocity (km/sec, +ve if receding) */
/*     EP0      dp     start epoch in years (e.g. Julian epoch) */
/*     EP1      dp     end epoch in years (same system as EP0) */

/*  Returned: */
/*     R1,D1    dp     RA,Dec at epoch EP1 (rad) */

/*  Called: */
/*     sla_DCS2C       spherical to Cartesian */
/*     sla_DCC2S       Cartesian to spherical */
/*     sla_DRANRM      normalize angle 0-2Pi */

/*  Note: */
/*     The proper motions in RA are dRA/dt rather than */
/*     cos(Dec)*dRA/dt, and are in the same coordinate */
/*     system as R0,D0. */

/*  P.T.Wallace   Starlink   23 August 1996 */

/*  Copyright (C) 1996 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION R0,D0,PR,PD,PX,RV,EP0,EP1,R1,D1 >*/
/*  Km/s to AU/year multiplied by arc seconds to radians */
/*<       DOUBLE PRECISION VFR >*/
/*<       PARAMETER (VFR=0.21094502D0*0.484813681109535994D-5) >*/
/*<       INTEGER I >*/
/*<       DOUBLE PRECISION sla_DRANRM >*/
/*<       DOUBLE PRECISION W,EM(3),T,P(3) >*/
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(R0,D0,P) >*/
    sla_dcs2c__(r0, d0, p);
/*  Space motion (radians per year) */
/*<       W=VFR*RV*PX >*/
    w = *rv * 1.0226903165792469e-6 * *px;
/*<       EM(1)=-PR*P(2)-PD*COS(R0)*SIN(D0)+W*P(1) >*/
    em[0] = -(*pr) * p[1] - *pd * cos(*r0) * sin(*d0) + w * p[0];
/*<       EM(2)= PR*P(1)-PD*SIN(R0)*SIN(D0)+W*P(2) >*/
    em[1] = *pr * p[0] - *pd * sin(*r0) * sin(*d0) + w * p[1];
/*<       EM(3)=         PD*COS(D0)        +W*P(3) >*/
    em[2] = *pd * cos(*d0) + w * p[2];
/*  Apply the motion */
/*<       T=EP1-EP0 >*/
    t = *ep1 - *ep0;
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          P(I)=P(I)+T*EM(I) >*/
	p[i - 1] += t * em[i - 1];
/*<       END DO >*/
    }
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(P,R1,D1) >*/
    sla_dcc2s__(p, r1, d1);
/*<       R1=sla_DRANRM(R1) >*/
    *r1 = sla_dranrm__(r1);
/*<       END >*/
    return 0;
} /* sla_pm__ */

/* Subroutine */ int sla_subet__(doublereal *rc, doublereal *dc, doublereal *
	eq, doublereal *rm, doublereal *dm)
{
    extern doublereal sla_dvdv__(doublereal *, doublereal *);
    extern /* Subroutine */ int sla_dcs2c__(doublereal *, doublereal *, 
	    doublereal *), sla_dcc2s__(doublereal *, doublereal *, doublereal 
	    *);
    doublereal a[3], f;
    integer i;
    doublereal v[3];
    extern /* Subroutine */ int sla_etrms__(doublereal *, doublereal *);
    extern doublereal sla_dranrm__(doublereal *);

/* + */
/*     - - - - - - */
/*      S U B E T */
/*     - - - - - - */

/*  Remove the E-terms (elliptic component of annual aberration) */
/*  from a pre IAU 1976 catalogue RA,Dec to give a mean place */
/*  (double precision) */

/*  Given: */
/*     RC,DC     dp     RA,Dec (radians) with E-terms included */
/*     EQ        dp     Besselian epoch of mean equator and equinox */

/*  Returned: */
/*     RM,DM     dp     RA,Dec (radians) without E-terms */

/*  Called: */
/*     sla_ETRMS, sla_DCS2C, sla_,DVDV, sla_DCC2S, sla_DRANRM */

/*  Explanation: */
/*     Most star positions from pre-1984 optical catalogues (or */
/*     derived from astrometry using such stars) embody the */
/*     E-terms.  This routine converts such a position to a */
/*     formal mean place (allowing, for example, comparison with a */
/*     pulsar timing position). */

/*  Reference: */
/*     Explanatory Supplement to the Astronomical Ephemeris, */
/*     section 2D, page 48. */

/*  P.T.Wallace   Starlink   10 May 1990 */

/*  Copyright (C) 1995 Rutherford Appleton Laboratory */
/* - */
/*<       IMPLICIT NONE >*/
/*<       DOUBLE PRECISION RC,DC,EQ,RM,DM >*/
/*<       DOUBLE PRECISION sla_DRANRM,sla_DVDV >*/
/*<       DOUBLE PRECISION A(3),V(3),F >*/
/*<       INTEGER I >*/
/*  E-terms */
/*<       CALL sla_ETRMS(EQ,A) >*/
    sla_etrms__(eq, a);
/*  Spherical to Cartesian */
/*<       CALL sla_DCS2C(RC,DC,V) >*/
    sla_dcs2c__(rc, dc, v);
/*  Include the E-terms */
/*<       F=1D0+sla_DVDV(V,A) >*/
    f = sla_dvdv__(v, a) + 1.;
/*<       DO I=1,3 >*/
    for (i = 1; i <= 3; ++i) {
/*<          V(I)=F*V(I)-A(I) >*/
	v[i - 1] = f * v[i - 1] - a[i - 1];
/*<       END DO >*/
    }
/*  Cartesian to spherical */
/*<       CALL sla_DCC2S(V,RM,DM) >*/
    sla_dcc2s__(v, rm, dm);
/*  Bring RA into conventional range */
/*<       RM=sla_DRANRM(RM) >*/
    *rm = sla_dranrm__(rm);
/*<       END >*/
    return 0;
} /* sla_subet__ */

