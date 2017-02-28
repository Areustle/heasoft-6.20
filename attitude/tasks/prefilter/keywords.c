/*
 * $Source: /headas/headas/attitude/tasks/prefilter/keywords.c,v $
 * $Revision: 1.3 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: keywords.c,v $
 * Revision 1.3  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.2  2004/05/11 18:45:48  rwiegand
 * Indicate interval between records in DELTAT keyword.  Indicate that
 * POSITION/VELOCITY vectors hold X,Y,Z.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include <stdio.h>

#include "derive.h"
#include "convert.h"
#include "headas.h"       /* for get_toolversion */
#include "misstime.h"


#define KEYWORD_SPACE 64

int update_keywords (const Arguments * args, Filter * filter)
{
	Keyword keywords[KEYWORD_SPACE];
	KeywordBuffer buffer = { 0 };

	/* initialize buffer */ 
	buffer.keywords = keywords;
	buffer.allocated = KEYWORD_SPACE;
	buffer.timestamp = 1;
	buffer.checksums = 1;

	/* prepare time span keywords */
	if (!args->code && !filter_code(filter))
		{
			AtTime t;

			char dateobs[32];
			char timeobs[32];
			char dateend[32];
			char timeend[32];

			met_to_AtTime(args, args->start, &t);
			sprintf(dateobs, "%d-%02d-%02d", t.yr, t.mo, t.dy);
			add_keyword(&buffer, TSTRING, "DATE-OBS", dateobs,
					"Start date for data");

			sprintf(timeobs, "%02d:%02d:%02d", t.hr, t.mn, t.sc);
			add_keyword(&buffer, TSTRING, "TIME-OBS", timeobs,
					"Start time for data");

			met_to_AtTime(args, args->end, &t);
			sprintf(dateend, "%d-%02d-%02d", t.yr, t.mo, t.dy);
			add_keyword(&buffer, TSTRING, "DATE-END", dateend,
					"End date for data");

			sprintf(timeend, "%02d:%02d:%02d", t.hr, t.mn, t.sc);
			add_keyword(&buffer, TSTRING, "TIME-END", timeend,
					"End time for data");

			add_keyword(&buffer, TDOUBLE, "TSTART", &args->start,
					"As in the TIME column");

			add_keyword(&buffer, TDOUBLE, "TSTOP", &args->end,
					"As in the TIME column");

			add_keyword(&buffer, TDOUBLE, "DELTAT", &args->interval,
					"Interval between records [s]");
		}

	/* prepare pointing keywords */
	if (!args->code && !filter_code(filter))
		{
			add_keyword(&buffer, TDOUBLE, "RA_NOM", &args->nominalRightAscension,
					"Nominal right ascension (degrees)");
			add_keyword(&buffer, TDOUBLE, "DEC_NOM", &args->nominalDeclination,
					"Nominal declination (degrees)");
		}

	/* prepare time system keywords from input FITS file */
	if (!args->code && !filter_code(filter))
		{
			struct KeywordData
			{
				char TELESCOP[FLEN_VALUE];
				char TIMESYS[FLEN_VALUE];
				double TIMEZERO;
				char TIMEUNIT[FLEN_VALUE];
				double MJDREF;
				long MJDREFI;
				double MJDREFF;
				double EQUINOX;
				char RADECSYS[FLEN_VALUE];
			} data;

#define KEY_INFO(type, name) \
	{ type, #name, offsetof(struct KeywordData, name) }

			struct KeyInfo
			{
				int type;
				char * name;
				size_t offset;
			} keyInfo[] =
				{
					KEY_INFO(TSTRING,  TELESCOP),
					KEY_INFO(TSTRING,  TIMESYS),
					KEY_INFO(TDOUBLE,  TIMEZERO),
					KEY_INFO(TSTRING,  TIMEUNIT),
					KEY_INFO(TDOUBLE,  MJDREF),
					KEY_INFO(TLONG,    MJDREFI),
					KEY_INFO(TDOUBLE,  MJDREFF),
					KEY_INFO(TDOUBLE,  EQUINOX),
					KEY_INFO(TSTRING,  RADECSYS),
				};

			int i;
			char comment[FLEN_COMMENT];
			fitsfile * source = 0;

			if (args->orbitMode == ORBIT_COMPARE)
				source = args->compare->fptr;
			else
				source = args->attfile->fp;

			for (i = 0; i < sizeof(keyInfo) / sizeof(keyInfo[0]); ++i)
				{
					int status = 0;
					struct KeyInfo * info = &keyInfo[i];
					void * address;
					address = ((char *) &data) + info->offset;
					fits_read_key(source, info->type, info->name,
							address, comment, &status);
					/* if keyword is present in input, include in output */
					if (!status)
						add_keyword(&buffer, info->type, info->name, address, comment);
#ifdef DEBUG
					else
						module_format(__func__, REPORT_VERBOSE,
								"input does not have keyword %s\n",
								info->name);
#endif
				}
		}

	/* prepare tool keywords */
	if (!args->code && !filter_code(filter))
		{
			char version[256];
			get_toolversion(version);
			add_keyword(&buffer, TSTRING, "CREATOR", version,
					"file creator");
#if 0
			add_keyword(&buffer, TSTRING, "DATE", timestamp,
					"file creation date");
#endif
			add_keyword(&buffer, TSTRING, "ORIGIN", args->origin,
					"file creation location");
		}

	/* write keywords to primary and secondary headers */
	if (!args->code && !filter_code(filter))
		{
			buffer.hdu = 1;
			put_keywords(filter, &buffer);
		}

	if (!args->code && !filter_code(filter))
		{
			buffer.hdu = 2;
			put_keywords(filter, &buffer);
		}

	release_keyword_buffer(&buffer);

	return filter_code(filter);
}


