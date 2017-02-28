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
		sscanf( argv[1], "%lf", &dr );
		sscanf( argv[2], "%lf", &dd );
		atJ2000toGal( dr, dd, &dl, &db );
		printf( "%9.5f %9.5f\n", dl, db);
		break;

	case 1:
		while( !feof(stdin) ){
			char buffer[256], *p;
			int read;

			p    = fgets( buffer,255,stdin );                   if( p==NULL ) break;
			read = sscanf( buffer, "%lf %lf", &dr, &dd );       if( read< 2 ) break;
			atJ2000toGal( dr, dd, &dl, &db );
			printf( "%9.5f %9.5f\n", dl, db);
		}
		break;

	default:
		printf("usage: j2000_gal ra dec\n");
		exit(1);
	}

	return 0;
}
