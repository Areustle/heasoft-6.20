#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "atFunctions.h"

int
main(int argc, char **argv)
{
	double dr, dd;
	double dl, db;

	switch(argc){
	case 3:
		sscanf( argv[1], "%lf", &dl );
		sscanf( argv[2], "%lf", &db );
		atGaltoB1950( dl, db, &dr, &dd );
		printf( "%9.5f %9.5f\n", dr, dd);
		break;

	case 1:
		while( !feof(stdin) ){
			char buffer[256], *p;
			int read;

			p    = fgets( buffer,255,stdin );                   if( p==NULL ) break;
			read = sscanf( buffer, "%lf %lf", &dl, &db );       if( read< 2 ) break;
			atGaltoB1950( dl, db, &dr, &dd );
			printf( "%9.5f %9.5f\n", dr, dd);
		}
		break;

	default:
		printf("usage: gal_b1950 lii bii\n");
		exit(1);
	}

	return 0;
}
