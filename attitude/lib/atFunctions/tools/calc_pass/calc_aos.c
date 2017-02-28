#include <stdio.h>
#include <math.h>
#include "cli.h"
#include "atFunctions.h"
#include "atError.h"

static char pname[] = "calc_aos";

#define MIN_OPERATION 5.0
#define MIN_OPERATION 5.0
#define HORIZON 0.0
#define ABOVE_HORIZON 1
#define BELOW_HORIZON 0
#define CTIMELENGTH 25
#define PASSLENGTH 11

extern void push_args(int argc, char **argv);

int
main(int argc, char **argv)
{
/* Initialized data */
	static AtPolarVect trkStation={
		0.349, 131.0792*DEG2RAD, 31.25*DEG2RAD
	};
	static char orbitFile[128]= "ae2_orbit.dat";
	static AtTimeD time_start = { 2005,7,1,0,0,0,0.};
	static AtTimeD time_stop  = { 2005,7,2,0,0,0,0.};

/* Local variables */
	char aostime[CTIMELENGTH], lostime[CTIMELENGTH];
	int j, kchk = 0, nt, el_flag=BELOW_HORIZON, prev_el_flag=BELOW_HORIZON;
	int op=0;
	double  dt=10 , mjd, mjd_stop;
	double latt,  heigh ;
	double az,el,dist, max_elev=0.0 ;
	char path[PASSLENGTH];
	int istat;

	AtTimeD times ;
	AtPolarVect gSatP, vpTrk;
	AtVect vSat, gSat, vTrk ;
	AtRotMat trkRM;

	push_args(argc, argv);

	CLintrdL("Input start year ", &(time_start.yr), 2005,2020);
	CLintrdL("Input start month", &(time_start.mo), 1,12);
	CLintrdL("Input start day  ", &(time_start.dy), 1,31);
	CLintrdL("Input start hour ", &(time_start.hr), 0,23);
	CLintrdL("Input stop  year ", &(time_stop.yr), 2005,2020);
	CLintrdL("Input stop  month", &(time_stop.mo), 1,12);
	CLintrdL("Input stop  day  ", &(time_stop.dy), 1,31);
	CLintrdL("Input stop  hour ", &(time_stop.hr), 0,23);
	CLfdprd("Input interval(sec)", &dt) ;
	CLtxtrd("Orbit text file", orbitFile, sizeof(orbitFile));
/* set USC */
	atAzElSet(&trkStation, vTrk, trkRM);

	dt = dt / 3600. / 24.;
	atMJulianD(&time_start, &mjd);
	atMJulianD(&time_stop, &mjd_stop);
	nt=(int)((mjd_stop-mjd)/dt) ;
	istat = atSetElement(orbitFile, mjd, kchk);
	if ( istat ) {
		fprintf(stderr, "\
%s: atSetElement('%s') failed\n", pname, orbitFile);
		return 1;
	}

	for (j = 0; j < nt; ++j) {
		atSetElement(orbitFile, mjd, kchk);
		atSatPos(mjd, vSat);
		atGeodetic(mjd, vSat, gSat);
		atVectToPol(gSat, &gSatP);
		atEllipsoid(&gSatP, &latt, &heigh);
		atMJDateD(mjd, &times);
		/* Satellite position */
		/* gSatP.r	radius from the Earth center */
		/* heigh	  height from the Earth */
		/* gSatP.lon*RAD2DEG longtitude */
		/* latt*RAD2DEG	  lattitude */

		atAzEl(gSat, vTrk, trkRM, &vpTrk);
		el = vpTrk.lat * RAD2DEG; /*Aximuth*/
		az = vpTrk.lon * RAD2DEG; /*Elevation*/
		dist = vpTrk.r;			/*Distance*/
		if (el > HORIZON){
			el_flag=ABOVE_HORIZON ;
			if(prev_el_flag==BELOW_HORIZON){
				atPathNumUSC(mjd, path);
				atCTimeD(&times, aostime);
				max_elev=0.0;
				printf("%s %.14s ", path, &aostime[3]);
				printf("%7.2lf%7.1lf ", az, dist);
			} else{
				if(el > max_elev){ max_elev=el; }
			}
		}
		else {
			el_flag=BELOW_HORIZON ;
			if(prev_el_flag==ABOVE_HORIZON){
				if(max_elev>MIN_OPERATION){ op=1;}
				else{ op=0 ; }
				atCTimeD(&times, lostime);
				printf("%.14s ",&lostime[3]);
				printf("%7.2lf %7.1lf  %7.2lf \n", az, dist, max_elev);
			}
		}
		prev_el_flag = el_flag ;
		mjd += dt;
	}
	return 0;
} /* MAIN__ */
