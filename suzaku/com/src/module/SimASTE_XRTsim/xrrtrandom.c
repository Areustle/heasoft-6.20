/****************************************************
// xrrtrandom.c
//
//	Replacement of xrrtrandom.cc by Richard L Fink
//
//	2003/09/14	Y.ISHISAKI
//		Changed to use astetool random number routines
****************************************************/

#include "xrrt_types.hh"
#include "aste_rand.h"

/*
// Global random function for all of XRRT
*/
double
xrrtrandom(void)
{
	double r = aste_drndts();
	return r;
}

double
xrrtgaussrandom(void)
{
	double r = aste_drndtsg();
	return r;
}

void
xrrtrandomseed(int seed)
{
	/* just returns */
}
