#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_att.h"

static char pname[] = "test-aste_att";

int
test_att_euler(double t)
{
	int istat;
	AtEulerAng ea;

	istat = aste_att_euler(t, &ea);
	printf("t=%.3lf -> istat=%d, EULER=(%.3lf, %.3lf, %.3lf)\n",
		   t, istat, RAD2DEG*ea.phi, RAD2DEG*ea.theta, RAD2DEG*ea.psi);

	return 0;
}

int
main(int argc, char **argv)
{
	ATTFILE *fp;
	double t;

	char *fn = argv[1];

	if ( argc < 2 ) {
		printf("\
usage: %s attitude-file\n", pname);
		return 1;
	}

	fp = aste_att_init(fn);
	if ( NULL == fp ) {
		printf("\
%s: aste_att_init('%s') failed\n", pname, fn);
		return 1;
	}

	printf("\
TELESCOP='%s'\n\
filename='%s'\n\
nrows=%ld, type=%d\n\
tstart=%.3lf, tstop=%.3lf, duration=%.3lf\n\
min_extrapolated=%.3lf, max_extrapolated=%.3lf\n",
		   fp->telescop, fp->filename, fp->nrows, fp->type,
		   fp->tstart, fp->tstop, fp->duration,
		   fp->min_extrapolated, fp->max_extrapolated);

	test_att_euler(fp->tstart);
	test_att_euler(fp->tstop);
	test_att_euler(fp->min_extrapolated);
	test_att_euler(fp->max_extrapolated);
	test_att_euler((fp->tstart + fp->tstop)/2);

	printf("\n");
	for (t = fp->tstart; t <= fp->tstop + 1; t += fp->duration/10.0) {
		test_att_euler(t);
	}

	aste_att_close(fp);

	return 0;
}
