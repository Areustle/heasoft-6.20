/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotimgrism/rectext.c,v $
 * $Revision: 1.10 $
 * $Date: 2011/09/12 16:53:12 $
 *
 * $Log: rectext.c,v $
 * Revision 1.10  2011/09/12 16:53:12  rwiegand
 * Updated version number.  Updated default overlap for backward compatibility.
 *
 * Revision 1.9  2011/09/12 16:43:11  rwiegand
 * Added parameters that allow user to define overlap required for a non-null
 * output pixel and the value to use for null output pixels.
 *
 * Revision 1.8  2005/07/18 12:47:54  rwiegand
 * Null terminate FITS key class list.
 *
 * Revision 1.7  2004/10/13 22:30:06  rwiegand
 * Updated to propagate all usable keywords from input to output.
 *
 * Revision 1.6  2004/09/17 18:47:12  rwiegand
 * Initialize output image pixels and null value.
 *
 * Revision 1.5  2003/11/24 22:32:48  rwiegand
 * Pay attention to history parameter.  Polygon intersection support moved
 * to library.
 *
 * Revision 1.4  2003/06/27 20:33:00  rwiegand
 * Recombined extracted region and wave scale into a single output file.
 * Modified to include 0th order in extracted region.  Updated wavelength
 * equation constants.  Updated unit test to run with UVOT test data.
 *
 * Revision 1.3  2003/05/14 18:05:16  rwiegand
 * Use librew instead of local implementation.
 *
 * Revision 1.2	2003/05/14 13:33:26	rwiegand
 * Implemented flux calibration.	Added unit test and help file.
 *
 * Revision 1.1	2003/04/07 17:04:30	rwiegand
 * Checkpoint for ugrism.	Testing with realistic data necessary.
 *
 */

#include <math.h>

#include "headas_utils.h"
#include "pil.h"
#include "extract.h"
#include "report.h"

/*
 * #include "headas.h"
 */

#define TOOLSUB rectext
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


enum
{
	RECTEXTRACT_SUCCESS,
	INITIALIZATION_ERROR,
	OUTPUT_ERROR,
	FINALIZATION_ERROR,
	RECTEXTRACT_DUMMY
};


typedef struct
{
	int dummy;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	double angle;
	int width;
	int height;
	double x0;
	double y0;

	double null;
	double overlap;

	int clobber;
	int history;
	int chatter;

} Parameters;


int get_parameters (Parameters * p);
int fits_extraction (Parameters * p);


int
rectext ()
{
	int code = 0;
	Parameters p = { 0 };

	set_toolversion("0.4");

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = fits_extraction(&p);

	return code;
}


int
get_parameters (Parameters * p)
{
	int code = 0;
	int pill = 0;

	if (!pill)
		pill = PILGetFname("infile", p->infile);

	if (!pill)
		pill = PILGetFname("outfile", p->outfile);

	if (!pill)
		pill = PILGetInt("width", &p->width);

	if (!pill)
		pill = PILGetInt("height", &p->height);

	if (!pill)
		pill = PILGetReal("angle", &p->angle);

	if (!pill)
		pill = PILGetReal("x0", &p->x0);

	if (!pill)
		pill = PILGetReal("y0", &p->y0);

	if (!pill)
		pill = PILGetReal("null", &p->null);

	if (!pill)
		pill = PILGetReal("overlap", &p->overlap);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	if (pill) {
		code = INITIALIZATION_ERROR;
		report_error("unable to load parameters\n");
	}
	else
		report_verbose("parameters loaded\n");

	return code;
}


int
fits_extraction (Parameters * p)
{
	int code = 0;

	Extraction e = { 0 };
	DImage from = { 0 };
	DImage to = { 0 };
	FITSHeader header = { 0 };

	to.null = -1;

	if (p->clobber)
		headas_clobberfile(p->outfile);

	if (!code)
		code = dimage_allocate(&to, p->width, p->height);

	if (!code)
		code = dimage_set_constant(&to, 0);

	if (!code)
		{
			ImageIO io = { 0 };
			int types[] = { TYP_HDUID_KEY, TYP_COMM_KEY, TYP_CONT_KEY,
					                    TYP_USER_KEY, TYP_REFSYS_KEY, 0 };
			io.header = &header;
			header.accept = accept_keycard_types;
			header.user = types;
			/*
			 * this could be optimized to only read the required
			 * subset of the input image
			 */
			code = dimage_read(&from, p->infile, &io);
		}

	if (!code)
		{
			/*
			 * perform extraction 
			 */
			e.from = &from;
			e.to = &to;

			e.x0 = p->x0;
			e.y0 = p->y0;
			e.angle = atan2(1, 1) * p->angle / 45;

			e.null = p->null;
			e.overlap = p->overlap;

			code = extract_rectangle(&e);
		}

	if (!code)
		{
			ImageIO io = { 0 };
			/*
			 * this could be optimized to only read the required
			 * subset of the input image
			 */
			io.header = &header;
			io.history = p->history;
			io.checksum = 1;
			header.accept = 0;
			code = dimage_write(&to, p->outfile, &io);
		}

	{
		report_verbose("cleaning up\n");
		dimage_release(&from);
		dimage_release(&to);
	}

	return code;
}


