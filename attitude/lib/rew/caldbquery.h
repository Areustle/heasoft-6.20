/*
 * $Source: /headas/headas/attitude/lib/rew/caldbquery.h,v $
 * $Revision: 1.3 $
 * $Date: 2005/09/26 21:08:20 $
 *
 * $Log: caldbquery.h,v $
 * Revision 1.3  2005/09/26 21:08:20  rwiegand
 * Change expression member to array (was external pointer).
 *
 * Revision 1.2  2005/09/15 15:22:11  rwiegand
 * Added function to set query qualifiers.
 *
 * Revision 1.1  2005/05/20 18:18:06  rwiegand
 * Added functions from UVOT library for interacting with CALDB.
 *
 * Revision 1.2  2004/12/10 14:48:39  rwiegand
 * Added standard header.
 *
 */

#ifndef CALDBQUERY_H
#define CALDBQUERY_H


#include "fitsio.h"



#define QUERY_MAXPATH 1024


enum
{
	QUERY_NO_RESULTS = 1,
	QUERY_CODE_LIMIT
};

enum
{
	QUERY_DETECTOR = 1,
	QUERY_DATETIME = 2,

	QUERY_FILTER = 16,

	QUERY_TERMINATOR
};



typedef struct
{
	int flags;

	char codename[32];

	char mission[32];
	char instrument[32];
	char detector[32];
	char filter[32];

	char startdate[32], starttime[32];
	char stopdate[32], stoptime[32];

	char expression[256];

	const char * infile;
	const char * qualifiers;

} CALDBQuery;


typedef struct
{
	char * path;
	int extension;
	int online;
	char space[QUERY_MAXPATH];

} CALDBEntry;


typedef struct
{
	/* input */
	int maxcount;
	int maxpath;
	int offline;	/* if non-zero, include results which are not online */

	/* output */
	int count;		/* number of CALDBEntrys */
	CALDBEntry * entries;
	int found;		/* number of CALDB matches */
} CALDBResults;



int init_caldb_query (CALDBQuery * query, fitsfile * fptr);
int exec_caldb_query (CALDBQuery * query, CALDBResults * results);

void free_caldb_results (CALDBResults * results);

int set_caldb_query_qualifiers (CALDBQuery * query, const char * qualifiers);
int simple_caldb_query (CALDBQuery * query, fitsfile * fptr, char * result);


#endif
