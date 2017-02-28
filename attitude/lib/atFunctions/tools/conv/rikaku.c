/*
  rikaku.c   By  C. Otani
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "atFunctions.h"

static char pname[] = "rikaku";

void
usage(void)
{
	printf("\
usage: %s RA1 DEC1 RA2 DEC2\n", pname);
	return;
}

int
main(int argc, char **argv)
{
	AtPolarVect   pvTgt1, pvTgt2;
	AtVect        vTgt1, vTgt2;
	double        angle;

	if ( 5 != argc ) {
		usage();
		return 1;
	}

	pvTgt1.r = pvTgt2.r = 1.0;
	pvTgt1.lon = atParseRAToRadian(argv[1]);
	pvTgt1.lat = atParseDecToRadian(argv[2]);
	pvTgt2.lon = atParseRAToRadian(argv[3]);
	pvTgt2.lat = atParseDecToRadian(argv[4]);
	atPolToVect(&pvTgt1,vTgt1);
	atPolToVect(&pvTgt2,vTgt2);

	atAngDistance(vTgt1,vTgt2,&angle);
	printf("%lf\n", angle*RAD2DEG);

	return 0;
}
