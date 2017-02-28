/*
C File: comtestc.c
C Description: Test program for COM
*/

#include <stdio.h>
#include <stdlib.h>
#include "com.h"

int
main(void)
{
	static int nval = 5;
	static int ivalue[5] = { 1, 2, 3, 4, 5 };
	static float rvalue[5] = { 1.0, 2.0, 3.0, 4.0, 5.0 };
	static double dvalue[5] = { 1.0, 2.0, 3.0, 4.0, 5.0 };
	static char *help[5] = {
		"korewa ichi", "ni", "san", "yon", "go desuyo"
	};
	char *names[5] = {
		"ONE", "TWO", "THREE", "FOUR", "FIVE"
	};
	int lans[6];

	CMinicom("COM");

	for (;;) {
		int i;

		CMmodval("*** MODVAL (I) ***", nval, names, help, "I", ivalue);
		for (i = 0; i < 5; i++) {
			printf("Value[%d] = %d\n", i, ivalue[i]);
		}
		CMmodval("*** MODVAL (R) ***", nval, names, help, "R", rvalue);
		for (i = 0; i < 5; i++) {
			printf("Value[%d] = %f\n", i, rvalue[i]);
		}
		CMmodval("*** MODVAL (D) ***", nval, names, help, "D", dvalue);
		for (i = 0; i < 5; i++) {
			printf("Value[%d] = %f\n", i, dvalue[i]);
		}

		CMchval("*** Chval (R) ***", nval, names, help, "R", rvalue);

		CMinquir("*** INQUIR ***", nval, names, help, 1, lans);

		for (i = 0; i < 0; i++) {
			printf("LANS[%d] = %d\n", i, lans[i]);
		}
		if ( 5 == lans[1] ) {
			return 0;
		}
		CMshowit("*** SHOWIT ***", nval, names, help);
		CMinquir("*** INQUIR ***", nval, names, help, 2, lans);
		for (i = 0; i < 0; i++) {
			printf("LANS[%d] = %d\n", i, lans[i]);
		}
		if ( 5 == lans[1] ) {
			return 0;
		}
		CMshowit("*** SHOWIT ***", nval, names, help);
	}
}
