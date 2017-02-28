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
		sscanf( argv[1], "%lf", &r1950 );
		sscanf( argv[2], "%lf", &d1950 );
		atB1950toJ2000( r1950, d1950, &r2000, &d2000 );
		printf( "%9.5f %9.5f\n", r2000, d2000);
		break;

	case 1:
		while( !feof(stdin) ){
			char buffer[256], *p;
			int read;

			p    = fgets( buffer,255,stdin );                   if( p==NULL ) break;
			read = sscanf( buffer, "%lf %lf", &r1950, &d1950 ); if( read< 2 ) break;
			atB1950toJ2000( r1950, d1950, &r2000, &d2000 );
			printf( "%9.5f %9.5f\n", r2000, d2000);
		}
		break;

	default:
		printf("usage: b1950_j2000 ra dec\n");
		exit(1);
	}

	return 0;
}
