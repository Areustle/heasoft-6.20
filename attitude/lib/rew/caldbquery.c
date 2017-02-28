/*
 * $Source: /headas/headas/attitude/lib/rew/caldbquery.c,v $
 * $Revision: 1.11 $
 * $Date: 2007/01/18 16:05:48 $
 *
 * $Log: caldbquery.c,v $
 * Revision 1.11  2007/01/18 16:05:48  rwiegand
 * Last fix broke YYYY-MM-DDTHH:MM:SS without subseconds .
 *
 * Revision 1.10  2006/11/21 19:15:42  rwiegand
 * Accept DATE-OBS/END in YYYY-MM-DDTHH:MM:SS(.sss) format.
 *
 * Revision 1.9  2006/11/14 18:59:48  rwiegand
 * When grabbing keywords from HDU, if FILTER was missing, DATE* would not
 * be queried.
 *
 * Revision 1.8  2006/09/28 18:33:11  rwiegand
 * Retrieve time values for query from keywords by default.
 *
 * Revision 1.7  2005/09/26 21:08:20  rwiegand
 * Change expression member to array (was external pointer).
 *
 * Revision 1.6  2005/09/15 18:17:20  rwiegand
 * Report query parameters.
 *
 * Revision 1.5  2005/09/15 15:22:11  rwiegand
 * Added function to set query qualifiers.
 *
 * Revision 1.4  2005/07/18 12:35:59  rwiegand
 * Off by one error finding end of string.
 *
 * Revision 1.3  2005/05/20 20:30:42  rwiegand
 * Had variables for number of files returned and available in CALDB reversed.
 * Reinstated some query control.
 *
 * Revision 1.2  2005/05/20 18:51:31  rwiegand
 * Noticed that the filter field was not being initialized.
 *
 * Revision 1.1  2005/05/20 18:18:06  rwiegand
 * Added functions from UVOT library for interacting with CALDB.
 *
 * Revision 1.1  2004/10/12 22:46:03  rwiegand
 * Library for interfacing with CALDB.
 *
 */

#include <stdlib.h>
#include <string.h>

#include "fitsio.h"
#include "hdcal.h"
#include "caldbquery.h"
#include "report.h"


static char NULL_EXPRESSION[] = "-";



static void
store_datetime (int status, char * datetime, char * date, char * time)
{
	if (status)
		return;

	if (strlen(datetime) == 10 || strlen(datetime) >= 19) {
		strncpy(date, datetime, 10);
		date[10] = 0;
	}
	else
		report_warning("ignoring invalid date string '%s'\n", datetime);

	if (strlen(datetime) >= 19) {
		strncpy(time, datetime + 11, 9);
		time[9] = 0;
	}
}



static void store_string (char * dest, const char * src, int length)
{
	while (*src == ' ' && length > 0)
		++src, --length;

	while (length >=0 && src[length-1] == ' ')
		--length;

	strncpy(dest, src, length);
	dest[length] = 0;
}


#define END_OF_QUALIFIERS -1
#define BAD_QUALIFIER -2


static int next_qualifier (const char * s, int p0, char * q, char * v)
{
	const char * pend; /* end of this term */
	const char * pequal; /* separates name from value of this term */

	{
		int length = strlen(s);
		if (p0 > length - 2)
			return END_OF_QUALIFIERS;
	}

	while (s[p0] && s[p0] == ' ')
		++p0;

	pend = index(s + p0, ',');
	if (!pend) {
		int length = strlen(s + p0);
		pend = s + p0 + length;
	}

	pequal = index(s + p0, '=');
	if (!pequal) {
		printf("next_qualifier: missing =\n\t'%s'\n", s + p0);
		return BAD_QUALIFIER;
	}

	store_string(q, s + p0, pequal - (s + p0));
	store_string(v, pequal +1, pend - pequal - 1);

	return pend - s + 1;
}


int init_caldb_query (CALDBQuery * query, fitsfile * fptr)
{
	int status = 0;
	char comment[FLEN_COMMENT];
	char datetime[32];

#if 0
	query->mission[0] = 0;
	query->instrument[0] = 0;
	query->detector[0] = 0;
	query->filter[0] = 0;
	query->startdate[0] = 0;
	query->starttime[0] = 0;
	query->stopdate[0] = 0;
	query->stoptime[0] = 0;
#endif

	if (query->qualifiers) {
		int p = 0;
		char qualifier[256];
		char value[256];
		while ((p = next_qualifier(query->qualifiers, p, qualifier, value)) > 0) {
#if 0
			report_verbose("qualifier '%s' value '%s' [p=%d]\n", qualifier, value, p);
#endif
			if (!strcasecmp(qualifier, "MISSION"))
				strcpy(query->mission, value);
			else if (!strcasecmp(qualifier, "INSTRUMENT"))
				strcpy(query->instrument, value);
			else if (!strcasecmp(qualifier, "DETECTOR"))
				strcpy(query->detector, value);
			else if (!strcasecmp(qualifier, "FILTER"))
				strcpy(query->filter, value);
			else if (!strcasecmp(qualifier, "EXPRESSION"))
				strcpy(query->expression, value);
			else if (!strcasecmp(qualifier, "STARTDATE"))
				strcpy(query->startdate, value);
			else if (!strcasecmp(qualifier, "STARTTIME"))
				strcpy(query->starttime, value);
			else if (!strcasecmp(qualifier, "STOPDATE"))
				strcpy(query->stopdate, value);
			else if (!strcasecmp(qualifier, "STOPTIME"))
				strcpy(query->stoptime, value);
			else if (!strcasecmp(qualifier, "DATE")) {
				strcpy(query->startdate, value);
				strcpy(query->stopdate, value);
			}
			else if (!strcasecmp(qualifier, "TIME")) {
				strcpy(query->starttime, value);
				strcpy(query->stoptime, value);
			}
			else
				report_warning("init_caldb_query(codename=%s) ignoring unknown qualifier %s=%s\n",
							query->codename, qualifier, value);

		}
		if (p == BAD_QUALIFIER)
			report_warning("init_caldb_query(codename=%s) bad qualifiers\n\t'%s'",
							query->codename, query->qualifiers);

	}

	if (!query->mission[0])
		fits_read_key_str(fptr, "TELESCOP", query->mission, comment, &status);
	if (!query->instrument[0])
		fits_read_key_str(fptr, "INSTRUME", query->instrument, comment, &status);
	if (!query->expression[0])
		strcpy(query->expression, NULL_EXPRESSION);


	if (query->flags) {
		/* caller is controlling which subfields are queried */
		if (query->detector[0])
			  ;
		else if (query->flags & QUERY_DETECTOR)
			fits_read_key_str(fptr, "DETNAM", query->detector, comment, &status);
		else
			strcpy(query->detector, "-");

		if (query->filter[0])
				;
		else if (query->flags & QUERY_FILTER)
			fits_read_key_str(fptr, "FILTER", query->filter, comment, &status);
		else
			strcpy(query->filter, "-");

		if (query->startdate[0])
			;
		else if (query->flags & QUERY_DATETIME) {
			fits_read_key_str(fptr, "DATE-OBS", datetime, comment, &status);
			store_datetime(status, datetime, query->startdate, query->starttime);
			fits_read_key_str(fptr, "DATE-END", datetime, comment, &status);
			store_datetime(status, datetime, query->stopdate, query->stoptime);
		}
	}
	else {
		/* by default try to read any not already set */
		int tmp;

		fits_write_errmark();

		tmp = 0;
		if (!query->detector[0])
			fits_read_key_str(fptr, "DETNAM", query->detector, comment, &tmp);
		if (tmp)
			strcpy(query->detector, "-");

		tmp = 0;
		if (!query->filter[0])
			fits_read_key_str(fptr, "FILTER", query->filter, comment, &tmp);
		if (tmp)
			strcpy(query->filter, "-");

		tmp = 0;
		if (!query->startdate[0]) {
			fits_read_key_str(fptr, "DATE-OBS", datetime, comment, &tmp);
			store_datetime(tmp, datetime, query->startdate, query->starttime);
			fits_read_key_str(fptr, "DATE-END", datetime, comment, &tmp);
			store_datetime(tmp, datetime, query->stopdate, query->stoptime);
		}

		fits_clear_errmark();
	}

	if (!query->stopdate[0] && query->startdate[0])
		strcpy(query->stopdate, query->startdate);
	if (!query->stoptime[0] && query->starttime[0])
		strcpy(query->stoptime, query->starttime);

	if (!query->startdate[0])
		strcpy(query->startdate, "now");
	if (!query->starttime[0])
		strcpy(query->starttime, "00:00:00");
	if (!query->stopdate[0])
		strcpy(query->stopdate, "now");
	if (!query->stoptime[0])
		strcpy(query->stoptime, "00:00:00");

	return status;
}


void report_caldb_query (const char * tag, const CALDBQuery * query)
{
	report_status("%s\n\tCODENAME => %s"
			"\n\tMISSION => %s"
			"\n\tINSTRUMENT => %s"
			"\n\tDETECTOR => %s"
			"\n\tFILTER => %s"
			"\n\tSTARTDATE => %s"
			"\n\tSTARTTIME => %s"
			"\n\tSTOPDATE => %s"
			"\n\tSTOPTIME => %s"
			"\n\tEXPRESSION => %s"
			"\n",
			tag,
			query->codename,
			query->mission,
			query->instrument,
			query->detector,
			query->filter,
			query->startdate,
			query->starttime,
			query->stopdate,
			query->stoptime,
			query->expression
			);
}


int exec_caldb_query (CALDBQuery * query, CALDBResults * results)
{
	int status = 0;
	char ** filevec = 0;
	char ** onlinevec = 0;
	long * extvec = 0;
	int i, eop, gotten;


	if (!results->maxpath)
		results->maxpath = QUERY_MAXPATH;

#define SIZE_ONLINE 64

	/* NB: one big chunk is allocated for the filevec,onlinevec pointers
	 * and their pointees
	 */
	filevec = malloc(results->maxcount * (sizeof(char*) + results->maxpath));
	onlinevec = malloc(results->maxcount * (sizeof(char*) + SIZE_ONLINE));
	extvec = malloc(results->maxcount * sizeof(long));

	if (!filevec || !onlinevec || !extvec) {
		status = MEMORY_ALLOCATION;
		goto cleanup;
	}

	eop = results->maxcount * sizeof(char *);

   	for (i = 0; i < results->maxcount; ++i) {
		filevec[i] = ((char*) filevec) + eop + i * results->maxpath;
		onlinevec[i] = ((char*) onlinevec) + eop + i * SIZE_ONLINE;
	}

	report_caldb_query("querying CALDB with", query);

	HDgtcalf(query->mission,
			query->instrument,
			query->detector,
			query->filter,
			query->codename,
			query->startdate, query->starttime,
			query->stopdate, query->stoptime,
			query->expression,

			results->maxcount,
			results->maxpath,
			filevec,
			extvec,
			onlinevec,
			&gotten,
			&results->found,
			&status);


	results->count = 0;

	if (status)
		report_warning("exec_caldb_query(codename=%s) HDgtcalf=%d\n",
				query->codename, status);

	else if (gotten == 0) {
		status = QUERY_NO_RESULTS;
		report_warning("exec_caldb_query(codename=%s) no results\n",
				query->codename);
	}

	else	{
		/* go ahead and allocate space for all of 'em even though some
		 * may be filtered (if offline)
		 */
		if (!results->entries)
			results->entries = calloc(gotten, sizeof(CALDBEntry));

		for (i = 0; i < gotten; ++i) {

			int online = !strcmp(onlinevec[i], "ONLINE");
			if (results->offline || online) {
				CALDBEntry * entry = &results->entries[i];
				int length = strlen(filevec[i]);
				if (length < QUERY_MAXPATH)
					entry->path = entry->space;
				else
					entry->path = malloc(length + 1);
				strcpy(entry->path, filevec[i]);
				entry->online = online;
				entry->extension = extvec[i];
				++results->count;
			}
		}

		report_verbose("CALDB query %s results [%d/%d]\n",
				query->codename, results->count, gotten);
		for (i = 0; i < results->count; ++i)
			report_verbose("\t%s\n", results->entries[i].path);
	}


cleanup:
	if (filevec)
		free(filevec);
	if (onlinevec)
		free(onlinevec);
	if (extvec)
		free(extvec);
	/* user has to call free_caldb_results */

	return status;
}



void free_caldb_results (CALDBResults * results)
{
	int i;

	for (i = 0; i < results->count; ++i) {
		CALDBEntry * entry = &results->entries[i];
		if (entry->path && entry->path != entry->space)
			free(entry->path);
	}

	free(results->entries);
}


int simple_caldb_query (CALDBQuery * query, fitsfile * fptr, char * path)
{
	int code = 0;
	CALDBResults results = { 0 };
	CALDBEntry entries[1];
	int needclose = 0;

	results.entries = entries;
	results.maxcount = 1;

	if (!code && !fptr && query->infile) {
		fits_open_file(&fptr, query->infile, READONLY, &code);
		if (code)
			report_warning("simple_caldb_query(codename=%s)"
					" unable to open %s [%d]\n",
					query->codename, query->infile, code);
		else
			needclose = 1;
	}

	if (!code)
		code = init_caldb_query(query, fptr);

	if (!code)
		code = exec_caldb_query(query, &results);

	if (!code && results.count > 0) {
		strcpy(path, entries[0].path);
		report_status("selected %s %s\n", query->codename, path);
	}

	if (code)
		report_warning("simple_caldb_query(codename=%s) code %d\n",
				query->codename, code);

	if (needclose) {
		int status = 0;
		fits_close_file(fptr, &status);
	}

	return code;
}


int set_caldb_query_qualifiers (CALDBQuery * query, const char * qualifiers)
{
	int code = 0;

	if (!strncasecmp(qualifiers, "CALDB", 5)) {
		if (qualifiers[5] == ':')
			query->qualifiers = qualifiers + 6;
		else if (qualifiers[5])
			report_warning("ignoring CALDB query qualifier string %s\n", qualifiers);
	}
	else
		query->qualifiers = qualifiers;

	return code;
}


