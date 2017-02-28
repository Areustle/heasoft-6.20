#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "atFunctions.h"

int
main(int argc, char **argv)
{
	double r2000, d2000;
	double r1950, d1950;

	switch(argc){
	case 3:
		sscanf( argv[1], "%lf", &r2000 );
		sscanf( argv[2], "%lf", &d2000 );
		atJ2000toB1950(r2000, d2000, &r1950, &d1950);
		printf( "%9.5f %9.5f\n", r1950, d1950);
		break;

	case 1:
		while( !feof(stdin) ){
			char buffer[256], *p;
			int read;

			p    = fgets( buffer,255,stdin );                   if( p==NULL ) break;
			read = sscanf( buffer, "%lf %lf", &r2000, &d2000 ); if( read< 2 ) break;
			atJ2000toB1950(r2000, d2000, &r1950, &d1950);
			printf( "%9.5f %9.5f\n", r1950, d1950);
		}
		break;

	default:
		printf("usage: j2000_b1950 ra dec\n");
		exit(1);
	}

	return 0;
}
