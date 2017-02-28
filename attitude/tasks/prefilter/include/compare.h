#ifndef PREFILTER_COMPARE_H
#define PREFILTER_COMPARE_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/compare.h,v $
 * $Revision: 1.1 $
 * $Date: 2002/12/06 20:14:21 $
 *
 *
 * $Log: compare.h,v $
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include "prefilter.h"

typedef struct
{
	char * name;
	int index;
	double value;
} CompareColumn;


/*
 * Compare.pcolumn[] index and default column name (in comment).
 * Since this was initially implemented to compare against XTE's
 * xtefilt, the default column names are the XTE mnemonics.
 */
enum
{
	COMPARE_TIME,  /* TIME */

	COMPARE_PX,    /* ACSSCPOSX */
	COMPARE_PY,    /* ACSSCPOSY */
	COMPARE_PZ,    /* ACSSCPOSZ */

	COMPARE_VX,    /* ACSSCVELX */
	COMPARE_VY,    /* ACSSCVELY */
	COMPARE_VZ,    /* ACSSCVELZ */

	COMPARE_Q1,    /* ACSESTQ1 */
	COMPARE_Q2,    /* ACSESTQ2 */
	COMPARE_Q3,    /* ACSESTQ3 */
	COMPARE_Q4,    /* ACSESTQ4 */

	COMPARE_COLUMNS
};


typedef struct
{
	fitsfile * fptr;
	int row;
	int last;

	CompareColumn time;          /* seconds elapsed since mission epoch */

	CompareColumn px;
	CompareColumn py;
	CompareColumn pz;

	CompareColumn vx;
	CompareColumn vy;
	CompareColumn vz;

	CompareColumn q1;
	CompareColumn q2;
	CompareColumn q3;
	CompareColumn q4;

	CompareColumn * pcolumn[COMPARE_COLUMNS];
		/* initialized to point to time, px, ... for iteration */

#if 0
	AtRotMat rm;          /* conversion of xyz pos/vel to ECI */
#endif

} Compare;


int initialize_compare (Arguments * args);
Iterator iterate_context_compare;

#endif
