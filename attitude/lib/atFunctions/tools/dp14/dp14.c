#include "atFunctions.h"
#include "atError.h"
#include <stdio.h>
#include <math.h>


int main()
{
    /* Initialized data */

    static double mjd50 = 33281.923;
    static AtTime times = { 88,1,1,0,0,0,0.};
    					/* DEC cc does not understand "0.f" */
    static int maxlin = 48;
    static char statn[1*6+1] = "UOIQSJ";
    static char cbrzl[1*2+1] = " ?";
    static char sunshn[1*2+1] = "* ";
    static AtPolarVect trkStation[6]={
	{0., 131.079*DEG2RAD, 31.25*DEG2RAD},{0., 128.*DEG2RAD, 26.5*DEG2RAD},
	{0., 80.2*DEG2RAD, 13.7*DEG2RAD}, {0., 282.*DEG2RAD, 0.*DEG2RAD},
	{0., 290.*DEG2RAD, -33.*DEG2RAD}, {0., 31.*DEG2RAD, -29.5*DEG2RAD},};
    
    int newpag_(AtPolarVect pvSun, AtPolarVect pvZ, int page);
    
    /* Local variables */
    int page, line, brzl, i, j, eflag, occlt, nstat, nt=60;
    double latt, elst, elvy, azst, heigh, disst, el, dt=120., mjd;
    float rig;
    static char aquis[4] = "   ";
    char path[11];
    char ess;
    char ctime[25];
    
    static AtPolarVect y50P={1.,30.,60.};
    AtPolarVect gSatP, vpTrk, pvSun, pvField, pvEarth;
    AtPolarVect pvSat;		/* DEBUG */
    AtVect vSat, vSat50, gSat, vField, vField50, vSun, y50, vTrk[2], vEarth;
    AtRotMat precRM, trkRM[2];
    static char orbitFile[128]="../data/orbit.data";
    static char rigFile[128]="../data/rigidity.data";


/* SIMULATION OF SATELLITE ORBITAL MOTION. (LP DUMP OUT) */
/*  INPUT DATA. */
/*   ORBITAL ELEMENT:   SIRIUS FILE. */
/*   CUT OFF RIGIDITY:  FT12F001. */
/*   Y,M,D,H,M,S,INTVAL(SEC),N,Y-ALP,Y-DEL :   FT05F001 */


    scanf("%d %d %d %d %d %d",
	&times.yr, &times.mo, &times.dy, &times.hr, &times.mn, &times.sc);
    scanf("%lf %d %lf %lf", &dt, &nt, &y50P.lon, &y50P.lat);
    scanf("%s", orbitFile);
    scanf("%s", rigFile);

    y50P.lon *= DEG2RAD;
    y50P.lat *= DEG2RAD;
    atPolToVect(&y50P, y50);
    for (i=0; i<2; ++i) {
	atAzElSet(&trkStation[i], vTrk[i], trkRM[i]);
    }
	    
    dt = dt / 3600. / 24.;
    atMJulian(&times, &mjd);
    atSetElement(orbitFile, mjd, 0);
    atGeomagSet(mjd, 8);
    atRigSet(rigFile);
    atPrecessRM(mjd, mjd50, precRM);
    page = 0;
    line = maxlin + 1;
    
    for (j = 0; j < nt; ++j) {
	atPathNum(mjd, path);
	atSatPos(mjd, vSat);
	atRotVect(precRM, vSat, vSat50);
	atInvVect(vSat50, vEarth);
	atVectToPol(vEarth, &pvEarth);
	atGeodetic(mjd, vSat, gSat);
	atVectToPol(gSat, &gSatP);
	atEllipsoid(&gSatP, &latt, &heigh);
	atGeomag(&gSatP, vSat, vField);
	atRotVect(precRM, vField, vField50);
	atVectToPol(vField50, &pvField);
	atSun(mjd, vSun);
	atVectToPol(vSun, &pvSun);
	atEarthOccult(vSat50, vSun, vSun, &occlt, &el);
	atBrazil(gSatP.lon, latt, &brzl);
	atRigidity(&gSatP, &rig);
	atEarthOccult(vSat50, y50, vSun, &eflag, &elvy);
	if (eflag == 2) {
	    ess = '*';
	} else {
	    ess = ' ';
	}
	nstat = 0;
	azst = -1.;
	elst = -1.;
	disst = -1.;
	for (i=0; i<2; ++i) {
	    atAzEl(gSat, vTrk[i], trkRM[i], &vpTrk);
	    el = vpTrk.lat * RAD2DEG;
	    if (el > 0.) {
		if (el > 3.) {
		    aquis[i] = statn[i];
		} else {
		    aquis[i] = '+';
		}
		++nstat;
		if (i == 0 || nstat == 1) {
		    azst = vpTrk.lon * RAD2DEG;
		    elst = el;
		    disst = vpTrk.r;
		}
	    } else {
		aquis[i] = ' ';
	    }
	}
	++line;
	if (line > maxlin) {
	    ++page;
	    line = 0;
	    newpag_(pvSun, y50P, page);
	}
	atMJDate(mjd, &times);
	atCTime(&times, ctime);
	printf("%s %.14s ", path, &ctime[3]);
	printf("%8.2lf%7.2lf", gSatP.r, heigh);
	printf("%7.2lf%7.2lf", gSatP.lon * RAD2DEG, latt * RAD2DEG);
	printf(" %s%7.2lf%7.2lf%7.1lf", aquis, azst, elst, disst);
	printf("%7.2lf%7.2lf", pvSun.lon * RAD2DEG, pvSun.lat * RAD2DEG);
	printf("%c", sunshn[occlt]);
	printf("%7.2lf%7.2lf", pvField.lon * RAD2DEG, pvField.lat * RAD2DEG );
	printf("%7.2lf%7.2lf", pvEarth.lon * RAD2DEG, pvEarth.lat * RAD2DEG );
	printf("%5.1f%c%6.1lf%c\n", rig, cbrzl[brzl], elvy* RAD2DEG, ess);

	mjd += dt;
    }
    return 0;
} /* MAIN__ */


int newpag_(AtPolarVect pvSun, AtPolarVect pvZ, int page)
{
    
    /* Local variables */
    char ctime[25];
    
    printf("POSITION AND ATITUDE OF SATELLITE FOR ORBITAL ELEMENT AT ");
    atCTime(&(atElement.itz), ctime);
    printf("%17s\n", ctime);
    
    printf("SEMI-AXIS = %9.3lf KM, ", atElement.semiax);
    printf("ECCENTRICITY = %9.6lf, ", atElement.eccent);
    printf("INCLINATION= %8.4lf DEGREE, ", RAD2DEG * atElement.aincln);
    printf("PERIGEE= %9.4lf KM, ", atElement.perige);
    printf("APOGEE= %9.4lf KM\n", atElement.apoge);
    
    printf("PERIGEE ANGLE = %9.4lf DEGREE,", RAD2DEG * atElement.smaome);
    printf("ASCENDINGNODE AT %9.4lf DEGREE,", RAD2DEG * atElement.ragome);
    printf("MEAN ANOMALY = %9.4lf DEGREE\n", RAD2DEG * atElement.omean0);
    
    printf("ASC. NODE VAR. = %9.4lf DEG./DAY,", RAD2DEG * atElement.ragdot);
    printf("PERIGEE ANGLE VAR. = %9.4lf DEG./DAY,", RAD2DEG*atElement.smodot);
    printf("MEAN MOTION = %9.4lf DEG./MIN.\n", RAD2DEG*atElement.znbar);
    
    printf("VAR. OF SEMI-AXIS = %9.5lf KM/DAY, ", atElement.adot);
    printf("VARIATION OF MEAN MOTION = %12.5le DEGREE/MIN/DAY\n",
					    RAD2DEG * atElement.znbadt);
    
    printf("SOLAR POSITION:  RIGHT ASCENSION = %8.3lf ", RAD2DEG*pvSun.lon);
    printf("DEG.,  DECLINATION = %8.3lf ", RAD2DEG*pvSun.lat);
    printf("DEG.,  DISTANCE = %8.5lf AU  (1950.0 EQUINOX)\n", RAD2DEG*pvSun.r);
    
    printf("Z-AXIS DIRECTION:  RIGHT ASCENSION = %8.3lf ", RAD2DEG*pvZ.lon);
    printf("DEG.,  DECLINATION = %8.3lf DEG.\n\n\n", RAD2DEG*pvZ.lat);
    
    printf("               UT           SATELLITE  POSITION              ");
    printf("ACQUISITION             SUN          ");
    printf("GEO.MAG.F.   EARTH CENTER   RIG   YELV\n");
    
    printf("   PATH   MN DY HR MN SC RAD.DIS HEIGHT  LONG.  LATT.  ");
    printf("   AZIM.   ELV.  RANGE   ALPHA  DELTA    ALPHA  DELTA");
    printf("   ALPHA  DELTA");
    printf("%87.0x ---------- 1950.0 EQUINOX -----------\n",0);
    return 0;
} 
