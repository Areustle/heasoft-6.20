/*
 * $Source: /headas/headas/attitude/tasks/expomap/expomap.c,v $
 * $Revision: 1.8 $
 * $Date: 2004/06/23 19:29:18 $
 *
 *
 * $Log: expomap.c,v $
 * Revision 1.8  2004/06/23 19:29:18  rwiegand
 * Use a real value for NULL pixels instead of NaN.
 *
 * Revision 1.7  2003/09/04 17:36:07  rwiegand
 * Allow creating exposure maps in detector coordinates.
 *
 * Revision 1.6  2003/08/26 14:49:48  rwiegand
 * Changed input angles to be in arcseconds.
 *
 * Revision 1.5  2003/07/28 21:13:20  rwiegand
 * Added history and checksum flags.
 *
 * Revision 1.4  2003/07/18 21:24:27  rwiegand
 * Transform from SKY_FROM DET level to SKY instead of DET level 0.  Ripped out
 * a bunch of printf statements.
 *
 * Revision 1.3  2003/07/18 21:14:38  rwiegand
 * Pass in binning parameters instead of trying to determine from WCS keywords
 * and TELDEF information.
 *
 * Revision 1.2  2003/07/18 20:11:21  rwiegand
 * Relocated to headas/attitude package.  Use coord/image.c support for
 * windowing/binning based on WCS keywords.
 *
 * Revision 1.1  2003/05/14 18:14:44  rwiegand
 * Tool for creating exposure maps from instrument map, good time intervals,
 * and attitude information.
 *
 * Revision 1.2  2003/05/14 13:35:29  rwiegand
 * Transfer keywords from input to output.
 *
 * Revision 1.1  2003/05/12 14:16:33  rwiegand
 * Generic exposure map generator.
 * Given an instrument map, good time interval(s) and attitude information,
 * creates an exposure map.
 *
 * Revision 1.1  2003/05/02 18:24:17  rwiegand
 * Exposure map generator
 *
 */


#include <stdio.h>	/* pil.h uses but does not include */
#include <math.h>
#include <string.h>

#include "pil.h"
#include "expomap.h"
#include "expio.h"
#include "report.h"

#define TOOLSUB expomap
#include "headas_main.c"


typedef struct
{
	int chatter;
	int clobber;

	char exptype[PIL_LINESIZE];
	int is_det;

	char gtiname[PIL_LINESIZE];
	char attname[PIL_LINESIZE];
	char teldef[PIL_LINESIZE];
	char badpixname[PIL_LINESIZE];
	char instname[PIL_LINESIZE];

	char outname[PIL_LINESIZE];

	double attdelta;
	double cx; /* RA or DETX of center of exposure map */
	double cy; /* DEC or DETY ... */

	double inst_binx;
	double inst_biny;
	double exp_binx;
	double exp_biny;

	int aberration;

	int width;
	int height;

} Parameters;



int
get_parameters (Parameters * p)
{
	int code = 0;
	int pill = 0;

	if (!pill)
		{
			pill = PILGetString("exptype", p->exptype);
			p->is_det = strcmp(p->exptype, "SKY");
		}

	if (!pill)
		pill = PILGetFname("gtiname", p->gtiname);

	if (!pill)
		pill = PILGetFname("attname", p->attname);

	if (!pill)
		pill = PILGetFname("instname", p->instname);

	if (!pill)
		pill = PILGetFname("teldef", p->teldef);

	if (!pill)
		pill = PILGetFname("outname", p->outname);

	if (!pill)
		pill = PILGetReal("attdelta", &p->attdelta);

	if (p->is_det)
		{
			if (!pill)
				pill = PILGetReal("cx", &p->cx);

			if (!pill)
				pill = PILGetReal("cy", &p->cy);
		}
	else
		{
			if (!pill)
				pill = PILGetReal("ra", &p->cx);

			if (!pill)
				pill = PILGetReal("dec", &p->cy);
		}

	if (!pill)
		pill = PILGetInt("width", &p->width);

	if (!pill)
		pill = PILGetInt("height", &p->height);

	if (!pill)
		pill = PILGetReal("inst_binx", &p->inst_binx);

	if (!pill)
		pill = PILGetReal("inst_biny", &p->inst_biny);

	if (!pill)
		pill = PILGetReal("exp_binx", &p->exp_binx);

	if (!pill)
		pill = PILGetReal("exp_biny", &p->exp_biny);

	if (!pill)
		pill = PILGetBool("aberration", &p->aberration);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;

	if (pill) {
		code = EXPOMAP_SETUP_ERROR;
		report_error("unable to load parameters\n");
	}
	else
		report_verbose("parameters loaded\n");

	return code;
}


static double
degrees_to_radians (double degrees)
{
	double radians = M_PI * degrees / 180;
	return radians;
}


static double
arcsecs_to_radians (double arcsecs)
{
	double radians = degrees_to_radians(arcsecs / 3600);
	return radians;
}


int
run_parameters (Parameters * p)
{
	int code = 0;

	ExposureMapper mapper = { 0 };
	FImage exposure = { 0 };
	FImage instrument = { 0 };
	FITSHeader header = { 0 };

	ImageWCS instwcs = { 0 };
	ImageWCS expwcs = { 0 };

	mapper.history = 1;
	mapper.is_det = p->is_det;

	instrument.null = -1;
	instrument.binx = p->inst_binx;
	instrument.biny = p->inst_biny;

	exposure.null = -1;
	exposure.binx = p->exp_binx;
	exposure.biny = p->exp_biny;

	mapper.aberration = p->aberration;
	mapper.exposure = &exposure;
	mapper.instrument = &instrument;
	mapper.header = &header;

	instrument.wcs = &instwcs;
	exposure.wcs = &expwcs;

	if (p->clobber)
		headas_clobberfile(p->outname);

	if (!code)
		code = load_good_time_intervals(&mapper, p->gtiname);

	if (!code)
		code = load_instrument_map(&mapper, p->instname);

	if (!code)
		code = load_telescope_definition(&mapper, p->teldef);

	if (!code)
		code = initialize_attitude(&mapper, p->attname,
				arcsecs_to_radians(p->attdelta));

	if (!code)
		code = initialize_exposure_map(&mapper,
				p->cx, p->cy, p->width, p->height);

	if (!code)
		{
			if (p->is_det)
				code = create_det_exposure_map(&mapper);
			else
				code = create_sky_exposure_map(&mapper);
		}

	if (!code)
		code = write_exposure_map(&mapper, p->outname);

	if (!code)
		code = release_mapper(&mapper);

	return code;
}


int
expomap ()
{
	int code = 0;
	Parameters p = { 0 };

	set_toolversion("0.4");

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}


