#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <atFunctions.h>

int
main(int argc, char **argv)
{
	double mjd0, mjd1;
	AtEulerAng ea;
	mjd0 = atof(argv[1]);
	mjd1 = atof(argv[2]);
	atPrecessEuler(mjd0, mjd1, &ea);
	while ( ea.phi < 0 ) ea.phi += 2*M_PI;
	while ( ea.theta < 0 ) ea.theta += 2*M_PI;
	while ( ea.psi < 0 ) ea.psi += 2*M_PI;
	printf("phi = %f, theta = %f, psi = %f\n",
		   ea.phi*RAD2DEG, ea.theta*RAD2DEG, ea.psi*RAD2DEG);
	if ( argc > 2 ) {
		AtPolarVect pv0, pv1, pv2;
		AtVect v0, v1, v2;
		pv0.lon = atof(argv[3]) * DEG2RAD;
		pv0.lat = atof(argv[4]) * DEG2RAD;
		pv0.r = 1.0;
		atPolToVect(&pv0, v0);
		atPrecession(mjd0, v0, mjd1, v1);
		atVectToPol(v1, &pv1);
		atPrecession(mjd1, v1, mjd0, v2);
		atVectToPol(v2, &pv2);
		printf("(%f,%f) -> (%f,%f) -> (%f,%f)\n",
			   pv0.lon*RAD2DEG, pv0.lat*RAD2DEG,
			   pv1.lon*RAD2DEG, pv1.lat*RAD2DEG,
			   pv2.lon*RAD2DEG, pv2.lat*RAD2DEG
			   );
	}
	return 0;
}
