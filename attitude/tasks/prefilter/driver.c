/*
 * $Source: /headas/headas/attitude/tasks/prefilter/driver.c,v $
 * $Revision: 1.20 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: driver.c,v $
 * Revision 1.20  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.19  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.18  2004/03/02 18:26:49  rwiegand
 * Remove obsolete parameters [pointaxis and bodybore].
 *
 * Revision 1.17  2004/02/02 16:00:02  rwiegand
 * Updated version string.
 *
 * Revision 1.16  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.15  2003/06/23 22:57:37  rwiegand
 * Added iteration code that indicates that current iteration should be skipped.
 *
 * Revision 1.14  2003/02/03 15:24:06  rwiegand
 * Fixed orbit mode string constants to match parameter file.  Reworked loading
 * of text TLEs so individual invalid records cause warnings instead of errors.
 * Indicate which record(s) in TLE file are invalid.
 *
 * Revision 1.13  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.12  2003/01/09 21:25:40  rwiegand
 * Put errors on stderr instead of stdout.  Fixed conversion of 2 digit to 4
 * digit years (thanks Ed).  More informative error message when satellite
 * is below minimum altitude.
 *
 * Revision 1.11  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "prefilter.h"
#include "derive.h"
#include "report.h"
#include "pill.h"
#include "pil.h"           /* PIL_LINESIZE */

#include "headas.h"

#define TOOLSUB prefilter
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


#if 0
static int STDERR_CHAT_LEVEL = 1;


static int report_prefilter_messages (report_t * info)
{
	int chat = 0;
	char buffer[256];

	/* map report code to headas chat code */
	switch (info->code)
		{
			case REPORT_NULL:
			case REPORT_SILENT:
				chat = -1;
				break;

			case REPORT_ERROR:
			case REPORT_EXCEPTION:
				chat = 1;
				break;

			case REPORT_WARNING:
			case REPORT_MONITOR:
				chat = 2;
				break;

			case REPORT_STATUS:
				chat = 3;
				break;

			case REPORT_VERBOSE:
				chat = 4;
				break;

			case REPORT_DEBUG:
				chat = 5;
				break;

			default:
				chat = 2;
				break;
		}

	if (chat >= 0)
	{
		const char * code;
		const char * module;

		extern const char * report_code_string(int);
		code = report_code_string(info->code);
		module = info->module ? info->module : "";

		if (chat <= STDERR_CHAT_LEVEL)
			HD_fprintf(hd_err, "%s: %s: %s", code, module, info->string);

		headas_chat(chat, "%s: %s: %s", code, module, info->string);
	}

	while (fits_read_errmsg(buffer))
		HD_fprintf(hd_err, "fitsio: %s\n", buffer);

	return 0;
}
#endif


int prefilter ()
{
	Arguments args = { 0 };

	char outname[PIL_LINESIZE];
	char columns[PIL_LINESIZE];
	char orbmode[PIL_LINESIZE];
	char orbname[PIL_LINESIZE];
	char attname[PIL_LINESIZE];
	char alignfile[PIL_LINESIZE];
	char leapname[PIL_LINESIZE];
	char rigname[PIL_LINESIZE];
	char missepoch[FLEN_VALUE];
	char timeadj[FLEN_VALUE];
	char origin[FLEN_VALUE];
	char compcolumns[PIL_LINESIZE];

	int pill = 0;

	add_report_function(&report_headas);

	set_toolversion("PREFILTER v3.3");

	if (!pill)
		pill = PILLGetFname("outname", outname);

	if (!pill)
		pill = PILLGetString("columns", columns);

	if (!pill)
		pill = PILLGetString("orbmode", orbmode);

	if (!pill)
		pill = PILLGetFname("orbname", orbname);

	if (!pill)
		pill = PILLGetFname("attname", attname);

	if (!pill)
		pill = PILLGetFname("alignfile", alignfile);

	if (!pill)
		pill = PILLGetFname("leapname", leapname);

	if (!pill)
		pill = PILLGetFname("rigname", rigname);

	if (!pill)
		pill = PILLGetReal("start", &args.start);

	if (!pill)
		pill = PILLGetReal("end", &args.end);

	if (!pill)
		pill = PILLGetReal("interval", &args.interval);

	if (!pill)
		pill = PILLGetReal("ranom", &args.nominalRightAscension);

	if (!pill)
		pill = PILLGetReal("decnom", &args.nominalDeclination);

	if (!pill)
		pill = PILLGetReal("attextrap", &args.attitudeExtrapolation);

	if (!pill)
		pill = PILLGetString("missepoch", missepoch);

	if (!pill)
		pill = PILLGetString("timeadj", timeadj);

	if (!pill)
		pill = PILLGetString("origin", origin);

	if (!pill)
		pill = PILLGetString("compcols", compcolumns);

	if (!pill)
		pill = PILLGetBool("compapplyquat", &args.compareApplyQuaternion);

	if (!pill)
		pill = PILLGetBool("history", &args.history);

	args.outname = outname;
	args.columns = columns;
	args.orbname = orbname;
	args.attname = attname;
	args.alignfile = alignfile;
	args.leapname = leapname;
	args.rigname = rigname;

	args.missepoch = missepoch;
	args.timeadj = timeadj;
	args.origin = origin;
	args.compareColumns = compcolumns;

	if (!strcasecmp(orbmode, "TLE_FITS"))
		args.orbitMode = ORBIT_TLE_FITS;
	else if (!strcasecmp(orbmode, "TLE_TEXT2"))
		args.orbitMode = ORBIT_TLE_TEXT2;
	else if (!strcasecmp(orbmode, "TLE_TEXT3"))
		args.orbitMode = ORBIT_TLE_TEXT3;
	else if (!strcasecmp(orbmode, "atSetElement"))
		args.orbitMode = ORBIT_atSetElement;
	else if (!strcasecmp(orbmode, "atSetElement2"))
		args.orbitMode = ORBIT_atSetElement2;
	else if (!strcasecmp(orbmode, "compare"))
		args.orbitMode = ORBIT_COMPARE;
	else
		args.orbitMode = ORBIT_TLE_FITS;

	args.clobber = headas_clobpar;
	args.verbose = (headas_chatpar > 4) ? 1 : 0;

	args.code = pill;

	if (!args.code)
		prefilter_dispatch(&args);

	remove_report_function(&report_headas);

	return args.code;
}

