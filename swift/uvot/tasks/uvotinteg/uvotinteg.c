/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotinteg/uvotinteg.c,v $
 * $Revision: 1.11 $
 * $Date: 2009/07/24 18:25:06 $
 *
 *
 * $Log: uvotinteg.c,v $
 * Revision 1.11  2009/07/24 18:25:06  rwiegand
 * Added CLIP operation which performs sigma clipping of values in the region
 * then reports the clipped mean.
 *
 * Revision 1.10  2009/03/31 20:41:46  rwiegand
 * Need to reset insideCount for each operation.
 *
 * Revision 1.9  2009/03/31 15:02:17  rwiegand
 * Implemented FOV operation.
 *
 * Revision 1.8  2008/10/17 15:20:06  rwiegand
 * Use fits_read_ascii_region file to avoid warnings.
 *
 * Revision 1.7  2008/09/23 15:16:15  irby
 * Expand fffrgn to fits_free_region to conform with new region.h (cfitsio 3.11).
 *
 * Revision 1.6  2008/09/23 13:26:36  rwiegand
 * Location of xmin, xmax, ymin, ymax changed.
 *
 * Revision 1.5  2008/03/28 15:27:12  rwiegand
 * Cannot load sigma clipping parameters yet.
 *
 * Revision 1.4  2008/03/26 19:54:25  rwiegand
 * Started implementing MEAN operation and sigma clipping, but not ready
 * for public use.
 *
 * Revision 1.3  2008/03/07 22:16:15  rwiegand
 * Handle null pixels.  Better WCS support.
 *
 * Revision 1.2  2006/10/31 19:14:46  rwiegand
 * Report result using monitor type.
 *
 * Revision 1.1  2006/10/30 22:48:01  rwiegand
 * New tool uvotinteg for finding area, sum, centroid for image/region.
 *
 */

#include <stdio.h>	/* pil.h uses but does not include */
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "genimage.h"
#include "region.h" /* CFITSIO region interface */
#include "clip.h"


#define TOOLSUB uvotinteg
#define VERSION 1.3
#include "headas_main.c"


#define _STRINGIFY0(x) # x
#define _STRINGIFY(x) _STRINGIFY0(x)

#define MAX_OPERATIONS 16


enum
{
	TASK_OK,
	TASK_SETUP_ERROR,
	TASK_INPUT_ERROR,
	TASK_OUTPUT_ERROR,
/*
	TASK_FITS_ERROR,
	TASK_CALDB_ERROR,
*/
	TASK_ENUM_COUNT
};


enum
{
	OP_NONE,
	OP_AREA,
	OP_SUM,
	OP_CENTROID,
	OP_MEAN,
	OP_FILE,
	OP_FOV,			/* is the region fully within the FOV?
						region => inside AND
						[value != NULL => value > 0] */
	OP_CLIP,		/* determine nsigma clipped mean */

	OP_ENUM_COUNT
};


enum
{
	SYS_NONE,
	SYS_DEFAULT,
	SYS_PIXEL,
	SYS_WORLD,
	SYS_VALUE,
	SYS_OTHER,
	SYS_ENUM_COUNT
};


enum
{
	DEBUG_SUBPIXELS = 1,
	DEBUG_INTEGRATION = 2,
	DEBUG_TERMINATOR
};


enum
{
	CONTROL_NONE,
	CONTROL_BASIC,
	CONTROL_SCRIPT,
	CONTROL_ENUM_COUNT
};


typedef struct
{
	int chatter;
	int history;

	char infile[PIL_LINESIZE];
	char regfile[PIL_LINESIZE];

	int opcount;
	int oplist[MAX_OPERATIONS];
	const char * opfile;

	int subpixels;

	char format[256];
	char sysname[64];
	int system;

	int includeNullArea;

	int clip;
	double nsigma;

	int control;

} Parameters;



typedef struct
{
	int xmin;
	int xmax;
	int ymin;
	int ymax;
} Bounds;


typedef struct Task Task;

struct Task
{
	const Parameters * par;

	DImage * image;

	SAORegion * region;
	WCSdata * wcs;
	ImageWCSa * wcsx;
	ImageWCSa * wcsy;
	Bounds * bounds;
	SigmaClip * clip;

	/* iteration shared */
	int insideFlag;
	int insideCount;
	double pixelValue;
	double axisScale;
	double areaScale;

	int nullFlag;		/* null pixel */
	int outsideFlag;	/* pixel outside image */
	int nullPixels;
	int goodPixels;

	int (*integrator) (Task *, double, double);

	/* for OP_AREA */
	/* use areaScale */
	double areaSum;

	/* for OP_SUM */
	double sum;

	/* for OP_CENTROID */
	/* reuse OP_SUM:sum */
	double xSum;
	double ySum;

	/* for OP_MEAN */
	/* reuse OP_SUM:sum */
	/* use goodPixels */

	/* for OP_FOV */
	int fovPixels;

	int system;
	char format[256];
	char result[256+64];
	int formatInteger;

};



#if 0

static double imax (double u, double v)
{
	if (u > v)
		return (int) u;
	if (v > u)
		return (int) v;
	return u;
}


static double imin (double u, double v)
{
	if (u > v)
		return (int) v;
	if (v > u)
		return (int) u;
	return u;
}

#endif



static int find_system (const char * sysname)
{
	int system = SYS_NONE;

	if (!strcasecmp(sysname, "IMAGE"))
		system = SYS_PIXEL;

	else if (!strcasecmp(sysname, "WORLD"))
		system = SYS_WORLD;

	else if (!strcasecmp(sysname, "WCS"))
		system = SYS_WORLD;

	else if (!strcasecmp(sysname, "DEFAULT"))
		system = SYS_DEFAULT;

	/* note this last one tests for mismatch NONE  */
	else if (strcasecmp(sysname, "NONE"))
		system = SYS_OTHER;

	return system;
}


static int get_operation (const char * s)
{
	int op = OP_NONE;

	if (!strcasecmp(s, "AREA"))
		op = OP_AREA;
	else if (!strcasecmp(s, "SUM"))
		op = OP_SUM;
	else if (!strcasecmp(s, "CENTROID"))
		op = OP_CENTROID;
	else if (!strcasecmp(s, "MEAN"))
		op = OP_MEAN;
	else if (!strcasecmp(s, "FOV"))
		op = OP_FOV;
	else if (!strcasecmp(s, "CLIP"))
		op = OP_CLIP;

	return op;
}



typedef struct
{
	int error;
	const char * start;
	const char * current;
} ParseState;

static void
store_operation (Parameters * par, ParseState * state)
{
	char buffer[64];
	int length = state->current - state->start;
	int op = OP_NONE;

	if (length > sizeof(buffer) - 2)
		{
			/* bogus operation string */
			state->error = 1;
			report_error("bogus operation length %d\n", length);
		}

	if (!state->error)
		{
			strncpy(buffer, state->start, length);
			buffer[length] = 0;
			op = get_operation(buffer);
			if (op == OP_NONE)
				{
					state->error = 1;
					report_error("invalid op '%s'\n", buffer);
				}
		}

	if (!state->error)
		{
			if (par->opcount == MAX_OPERATIONS)
				{
					state->error = 1;
					report_error("too many operations\n");
				}
			else
				{
					par->oplist[par->opcount++] = op;
					if (op == OP_CLIP)
						par->clip = 1;
				}
		}

	state->start = 0;
}


static int
parse_operation_list (Parameters * par, const char * s)
{
	int op = OP_NONE;
	ParseState state = { 0 };

	state.current = s;

	while (*state.current && !state.error)
		{
			char c = *state.current;

			if (isspace(c))
				{
					if (state.start)
						store_operation(par, &state);
				}
			else if (c == ',')
				{
					if (state.start)
						store_operation(par, &state);
				}
			else if (isalpha(c))
				{
					if (!state.start)
						state.start = state.current;
				}
			else
				{
					state.error = 1;
					report_error("invalid operation character '%c'\n", c);
				}

			++state.current;
		}

	if (state.start)
		store_operation(par, &state);

	if (!state.error && par->opcount > 0)
		op = par->oplist[0];

	return op;
}


static int
get_parameters (Parameters * par)
{
	int code = 0;
	int pill = 0;
	char operation[PIL_LINESIZE];
	char control[PIL_LINESIZE];

	if (!pill)
		pill = PILGetFname("infile", par->infile);

	if (!pill)
		pill = PILGetFname("regfile", par->regfile);

	if (!pill)
		pill = PILGetString("operation", operation);
	if (!pill)
		{
			report_verbose("operation is '%s'\n", operation);

#if 0
			if (operation[0] == '@')
				{
					par->opcount = 1;
					par->oplist[0] = OP_FILE;
					par->opfile = &operation[1];
				}
			else
#endif
			if (parse_operation_list(par, operation) == OP_NONE)
				{
					pill = PIL_BAD_ARG;
					report_error("invalid operation '%s'\n", operation);
				}
		}

	if (!pill)
		pill = PILGetInt("subpixels", &par->subpixels);

	if (!pill)
		pill = PILGetString("format", par->format);

	if (!pill)
		pill = PILGetString("sysname", par->sysname);
	if (!pill)
		{
			par->system = find_system(par->sysname);
			report_verbose("sysname is '%s'\n", par->sysname);
		}

	if (!pill)
		pill = PILGetBool("nullarea", &par->includeNullArea);

	if (!pill)
		pill = PILGetString("control", control);
	if (!pill)
		{
			report_verbose("control is '%s'\n", control);

			if (!strcasecmp(control, "BASIC"))
				par->control = CONTROL_BASIC;

			else if (!strcasecmp(operation, "SCRIPT"))
				par->control = CONTROL_SCRIPT;

			else {
				pill = PIL_BAD_ARG;
				report_error("unexpected control '%s'\n", control);
			}
		}

	if (!pill)
		pill = PILGetReal("nsigma", &par->nsigma);

	par->chatter = headas_chatpar;
	get_history(&par->history);

	if (pill)
		{
			code = TASK_SETUP_ERROR;
			report_error("unable to load parameters\n");
		}
	else
		report_verbose("parameters loaded\n");

	return code;
}



static int
load_region (Task * task, const char * path)
{
	int code = 0;
	int status = 0;
	if (fits_read_rgnfile(path, task->wcs, &task->region, &status))
		{
			code = TASK_INPUT_ERROR;
			report_error("unable to load region from %s [%d]\n",
						path, status);
		}
		else
			report_verbose("loaded region %s\n", path);

	if (!code)
		report_verbose("region has %d shape(s)\n", task->region->nShapes);

	if (!code && task->region->nShapes < 1)
		{
			code = TASK_INPUT_ERROR;
			report_error("region does not include any shapes\n");
		}

	if (!code && !task->region->Shapes[0].sign)
		{
			code = TASK_INPUT_ERROR;
			report_error("region begins with an exclude region [unsupported]\n");
		}

	return code;
}


static int
store_wcsa (const FITSHeader * header, ImageWCSa * wcsa, int axis)
{
	int code = 0;
	if (image_get_wcsa(header, wcsa, axis, 0))
		{
			code = TASK_INPUT_ERROR;
			report_error("unable to determine WCS%d info\n", axis);
		}
	return code;
}


static int
load_image (Task * task, const char * path)
{
	int code = 0;
	FITSHeader header = { 0 };
	ImageIO io = { 0 };
	ImageWCS wcs = { 0 };
	DImage * image = task->image;

	image->wcs = &wcs;
	io.header = &header;

	code = dimage_read(image, path, &io);

	if (!code)
		{
			/* FITS/image pixels counts from 1 */
			image->x0 = 1;
			image->y0 = 1;
		}

	if (!code)
		code = store_wcsa(&header, task->wcsx, 1);

	if (!code)
		code = store_wcsa(&header, task->wcsy, 2);

	if (!code)
		{
			/* transfer WCS information to WCSdata */
			WCSdata * fwcs = task->wcs;

			fwcs->xrefpix = wcs.xpix;
			fwcs->yrefpix = wcs.ypix;
			fwcs->xrefval = wcs.xval;
			fwcs->yrefval = wcs.yval;
			fwcs->xinc = wcs.xinc;
			fwcs->yinc = wcs.yinc;
			fwcs->rot = wcs.rot;
        
			strncpy(fwcs->type, wcs.type, sizeof(wcs.type));

			fwcs->exists = 1;

			report_verbose("loaded %s [width=%d, height=%d]\n",
						path, image->width, image->height);
		}

	return code;
}


static int
test_position (Task * task, double x, double y)
{
	int inside = 0;
	inside = fits_in_region(x, y, task->region);
	task->insideFlag = inside;

	if (inside)
		++task->insideCount;

	return inside;
}


static int
assimilate_subpixel (Task * task, double x, double y)
{
	int code = 0;
	int inside;
	double delta = task->image->null;

	inside = test_position(task, x, y);
	if (inside)
		{
			if (!task->nullFlag || task->par->includeNullArea)
				task->areaSum += task->areaScale;

			if (task->nullFlag)
				++task->nullPixels;
			else
				{
					++task->goodPixels;

					delta = task->pixelValue * task->areaScale;
					task->sum += delta;
					task->xSum += delta * x;
					task->ySum += delta * y;

					if (task->clip)
						add_clip_data(task->clip, task->pixelValue);
				}

			if (!task->outsideFlag && task->pixelValue != 0)
				++task->fovPixels;
		}

	if (debug_test(DEBUG_INTEGRATION))
		report_verbose("x=%.3f, y=%.3f, null=%d, delta=%.3f\n",
					x, y, task->pixelValue, task->nullFlag, delta);

	return code;
}


extern void xim_bbox_region ();

static int
find_region_bounds (Task * task, Bounds * bounds)
{
	/* worse case for now; could optimize based on image and region */
	int code = 0;
	int status = 0;
	double xmin, xmax, ymin, ymax;

	xim_bbox_region(task->region, &xmin, &xmax, &ymin, &ymax, &status);
	if (status)
		{
			code = TASK_INPUT_ERROR;
			report_warning("unable to find region bounds [%d]\n", status);
		}
	else
		{
			report_verbose("find_region_bounds[real] "
						"xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f\n",
						xmin, xmax, ymin, ymax);

			bounds->xmin = (int) floor(xmin+0.5);
			bounds->xmax = (int) floor(xmax+0.5);
			bounds->ymin = (int) floor(ymin+0.5);
			bounds->ymax = (int) floor(ymax+0.5);

			report_verbose("find_region_bounds[int] "
						"xmin=%d, xmax=%d, ymin=%d, ymax=%d\n",
						bounds->xmin, bounds->xmax, bounds->ymin, bounds->ymax);

			task->bounds = bounds;
		}

	return code;
}


static int
iterate_subpixels (Task * task, int i, int j)
{
	int code = 0;
	int ii, jj;
	double x, y;
	DImage * pi;
	const Parameters * par = task->par;

	pi = task->image;
	task->outsideFlag = 0;

	if (i < pi->x0 || i > pi->width || j < pi->y0 || j > pi->height)
		{
			task->pixelValue = pi->null;
			task->outsideFlag = 1;
		}
	else
		task->pixelValue = dimage_get_absolute(task->image, i, j);

	task->nullFlag = task->pixelValue == task->image->null;

	for (ii = 0; ii < par->subpixels; ++ii)
		for (jj = 0; jj < par->subpixels; ++jj)
			{
				x = i - 0.5 + task->axisScale * (ii + 0.5);
				y = j - 0.5 + task->axisScale * (jj + 0.5);

				assimilate_subpixel(task, x, y);

				if (debug_test(DEBUG_SUBPIXELS))
					report_verbose("i=%d+%d, j=%d+%d, x=%.3f, y=%.3f\n",
								i, ii, j, jj, x, y);
			}

	return code;
}


static int is_pixel_unit (const char * s)
{
	if (!strcasecmp(s, "pixel"))
		return 1;
	if (!strcasecmp(s, "pixels"))
		return 1;
	if (!strcasecmp(s, "pix"))
		return 1;

	/* string consisting of blanks is pixels, all others not */
	while (*s && *s == ' ')
		++s;
	if (*s)
		return 0;

	return 1;
}


static void modulate_format (Task * task, int operation)
{
	char buffer[80];
	const char * xunit;
	const char * yunit;
	switch (task->system)
		{
			case SYS_PIXEL :
				strcat(task->format, " [pixel]");
				break;

			case SYS_WORLD :
				xunit = task->wcsx->cunit;
				yunit = task->wcsy->cunit;
				if (operation == OP_AREA) 
					{
						if (is_pixel_unit(xunit) && is_pixel_unit(yunit))
							strcpy(buffer, " [pixel]");
						else if (!strcmp(xunit, yunit))
							sprintf(buffer, " [%s^2]", xunit);
						else
							sprintf(buffer, " [%s*%s]", xunit, yunit);
					}
				else if (operation == OP_CENTROID)
					{
						if (is_pixel_unit(xunit) && is_pixel_unit(yunit))
							strcpy(buffer, " [pixel]");
						else if (!strcmp(xunit, yunit))
							sprintf(buffer, " [%s]", xunit);
						else
							sprintf(buffer, " [%s,%s]", xunit, yunit);
					}
				else if (operation == OP_FOV)
					report_warning("ignoring system WORLD for operation FOV\n");
				else
					sprintf(buffer, " [value]");
				strcat(task->format, buffer);
				break;

			case SYS_VALUE :
				/* TODO: could use BUNIT */
				strcat(task->format, " [value]");
				break;

			case SYS_OTHER :
				sprintf(buffer, " [%s]", task->par->sysname);
				strcat(task->format, buffer);
				break;

			case SYS_NONE  :
				break;

			default:
				report_warning(
						"modulate_format: ignoring unexpected system %d\n",
						task->system);
		}
}


static int
initialize_result (Task * task, int operation)
{
	int code = 0;
	const Parameters * par = task->par;

	task->formatInteger = 0;

	if (par->system == SYS_DEFAULT)
		switch (operation)
			{
				case OP_AREA:
					task->system = SYS_PIXEL;
					break;
				case OP_SUM:
					task->system = SYS_VALUE;
					break;
				case OP_CENTROID:
					task->system = SYS_WORLD;
					break;
				case OP_MEAN:
					task->system = SYS_VALUE;
					break;
				case OP_FOV:
					task->system = SYS_VALUE;
					break;
				case OP_CLIP:
					task->system = SYS_VALUE;
					break;
				default:
					report_warning(
							"initialize_result: unexpected operation %d\n",
							operation);
			}
	else
		task->system = par->system;

	if (!strcasecmp(par->format, "DEFAULT"))
		{
			switch (operation)
				{
					case OP_AREA:
						strcpy(task->format, "area: %.3f");
						break;

					case OP_SUM:
						strcpy(task->format, "sum: %.3f");
						break;

					case OP_CENTROID:
						if (task->system == SYS_WORLD)
							strcpy(task->format, "centroid: %.6f,%.6f");
						else
							strcpy(task->format, "centroid: %.3f,%.3f");
						break;

					case OP_MEAN:
						strcpy(task->format, "mean: %.3f");
						break;

					case OP_FOV:
						strcpy(task->format, "fov: %.3f");
						break;

					case OP_CLIP:
						strcpy(task->format, "clipped: %.3f");
						break;

					default:
						code = TASK_INPUT_ERROR;
						break;
				}

		}
	else
		{
			int formatFields = 0;
			int active = 0;
			char c;
			const char * p = par->format;
			int opFields = operation == OP_CENTROID ? 2 : 1;

			while ((c = *p++) != 0)
				{
					if (active && c != '%')
						{
							++formatFields;
							active = 0;
							if (c == 'd' || c == 'i' || c == 'o' || c == 'u' || c == 'x')
								task->formatInteger = 1;
						}
					if (!active && c == '%') { active = 1; }
				}

			/* check number of fields in user format */
			if (formatFields != opFields)
				report_warning("expecting %d fields in format but %s has %d\n",
							opFields, par->format, formatFields);

			/* go ahead and try to use it */
			strcpy(task->format, par->format);

		}

	modulate_format(task, operation);

	return code;
}


static void
report_result (Task * task)
{
	report_code(REPORT_MONITOR, "%s\n", task->result);

#if 0
	if (task->par->reportNulls)
#endif
		report_code(REPORT_MONITOR, "nulls: %d\n", task->nullPixels);
}


static int
final_result (Task * task)
{
	int code = 0;
	int pill;

	pill = PILPutString("result", task->result);
	if (pill)
		{
			code = TASK_OUTPUT_ERROR;
			report_error("unable to save result [%d]\n", code);
		}
	pill = PILPutInt("outnull", task->nullPixels);
	if (pill)
		{
			code = TASK_OUTPUT_ERROR;
			report_error("unable to save outnull [%d]\n", code);
		}

	return code;
}


static int
initialize_integration (Task * task)
{
	int code = 0;
	const Parameters * par = task->par;

	/* initialization */

	/* TODO: should be factored by operation */
	task->axisScale = 1. / par->subpixels;
	task->areaScale = 1. / par->subpixels / par->subpixels;

	task->areaSum = 0;
	task->sum = 0;
	task->xSum = 0;
	task->ySum = 0;
	task->insideCount = 0;
	task->nullPixels = 0;
	task->goodPixels = 0;
	task->fovPixels = 0;

	if (task->clip)
		{
			Bounds * b = task->bounds;
			int n = (b->xmax - b->xmin + 1) * (b->ymax - b->ymin + 1)
					* par->subpixels * par->subpixels;
			report_verbose("allocating %d elements for clipping\n", n);
			task->clip->data = calloc(n, sizeof(double));
		}

	return code;
}


/* this could be smarter and only call iterate_subpixels if subpixels > 1 */
/* this iteration could also use gen image iteration... */
static int
iterate_pixels (Task * task)
{
	int code = 0;
	int i, j;
	const Bounds * bounds = task->bounds;

	for (i = bounds->xmin; !code && i <= bounds->xmax; ++i)
		for (j = bounds->ymin; !code && j <= bounds->ymax; ++j)
			code = iterate_subpixels(task, i, j);

	return code;
}



static int
operation_area (Task * task)
{
	int code = 0;
	double area;

	if (!code)
		{
			area = task->areaSum;

			if (task->system == SYS_WORLD)
				area *= task->wcsx->cdelt * task->wcsy->cdelt;

			sprintf(task->result, task->format, area);
			report_status("operation_area: areaScale=%.3f inside=%d, area=%.3f\n",
						task->areaScale, task->insideCount, task->areaSum);
		}
	

	return code;
}


static int
operation_sum (Task * task)
{
	int code = 0;
	double sum;

	if (!code)
		{
			sum = task->sum;
			sprintf(task->result, task->format, sum);
			report_status("operation_sum: inside=%d, sum=%.3f\n",
						task->insideCount, task->sum);
		}

	return code;
}


static int
operation_centroid (Task * task)
{
	int code = 0;

	report_verbose("operation_centroid: "
				"inside=%d, sum=%.3f, xSum=%.3f, ySum=%.3f\n",
				task->insideCount, task->sum, task->xSum, task->ySum);

	if (!code)
		{
			double x, y;
			x = task->xSum / task->sum;
			y = task->ySum / task->sum;

			report_status("operation_centroid [image]: x=%.3f, y=%.3f\n", x, y);

			if (task->system == SYS_WORLD)
				{
					/* convert x,y to world */
					double tmpx, tmpy;
					int status = 0;
					WCSdata * wcs = task->wcs;
#if 0
					int ffwldp(double xpix, double ypix, double xref, double yref,
									   double xrefpix, double yrefpix, double xinc, double yinc,
								     double rot, char *type, double *xpos, double *ypos,
										 int *status);
#endif
					fits_pix_to_world(x, y, wcs->xrefval, wcs->yrefval,
													wcs->xrefpix, wcs->yrefpix, wcs->xinc, wcs->yinc,
													wcs->rot, wcs->type, &tmpx, &tmpy, &status);
					x = tmpx;
					y = tmpy;
					report_status("operation_centroid [world]: x=%.3f, y=%.3f\n", x, y);
				}

			sprintf(task->result, task->format, x, y);
		}

	return code;
}


static int
operation_mean (Task * task)
{
	int code = 0;

	if (!code)
		{
			double mean;
			mean = task->sum / task->insideCount;
			sprintf(task->result, task->format, mean);
			report_status("operation_mean: inside=%d, sum=%.3f\n",
						task->insideCount, task->sum);
		}

	return code;
}


static int
operation_fov (Task * task)
{
	int code = 0;

	if (!code)
		{
			int validFOV;
			double fractionFOV;

			validFOV = task->insideCount == task->fovPixels;

			fractionFOV = task->insideCount
					? ((double) task->fovPixels) / task->insideCount : 0;

			if (task->formatInteger)
				sprintf(task->result, task->format, validFOV);
			else
				sprintf(task->result, task->format, fractionFOV);
			report_status("operation_fov: inside=%d, fov=%d, null=%d\n",
						task->insideCount, task->fovPixels, task->nullPixels);
		}

	return code;
}


static int
operation_clip (Task * task)
{
	int code = 0;

	if (!code)
		{
			SigmaClip * clip = task->clip;
			code = sigma_clip(clip);
			sprintf(task->result, task->format, clip->mean);
			report_status("operation_clip: inside=%d, null=%d, niter=%d, mean=%.3f, sigma=%.3f\n",
						task->insideCount, task->nullPixels, clip->niter, clip->mean, clip->sigma);
		}

	return code;
}


static int
iterate_operations (Task * task)
{
	int code = 0;
	int i, operation;
	const Parameters * par = task->par;

	code = initialize_integration(task);

	if (!code)
		code = iterate_pixels(task);

	for (i = 0; !code && i < par->opcount; ++i)
		{
			operation = par->oplist[i];

			initialize_result(task, operation);

			if (operation == OP_AREA)
				code = operation_area(task);

			else if (operation == OP_SUM)
				code = operation_sum(task);

			else if (operation == OP_CENTROID)
				code = operation_centroid(task);

			else if (operation == OP_MEAN)
				code = operation_mean(task);

			else if (operation == OP_FOV)
				code = operation_fov(task);

			else if (operation == OP_CLIP)
				code = operation_clip(task);

			else
				{
					code = TASK_INPUT_ERROR;
					report_warning("unhandled operation %d\n", operation);
				}

			if (!code)
				report_result(task);
		}

	return code;
}


static int
iterate_regions (Task * task)
{
	int code = 0;
	char ** reglist;
	Bounds bounds = { 0 };
	int i;
	int count = 0;

	if (!code)
		{
			int fieldsep = ',';
			int trim = 1;
			int skipempty = 1;
			int guardparen = 0;
			int status = 0;
			reglist = expand_item_list((char *) task->par->regfile,
					&count, fieldsep, trim, skipempty, guardparen, &status);
			if (status || !reglist)
				{
					code = TASK_INPUT_ERROR;
					report_error("unable to expand regfile [%d]\n", status);
				}
		}

	for (i = 0; !code && i < count; ++i)
		{
			report_verbose("processing region %s\n", reglist[i]);

			code = load_region(task, reglist[i]);

			if (!code)
				code = find_region_bounds(task, &bounds);

			if (!code)
				code = iterate_operations(task);

			if (task->region)
				{
					fits_free_region(task->region);
					task->region = 0;
				}
		}

	return code;
}


static int
run_parameters (const Parameters * par)
{
	int code = 0;
	Task task = { 0 };
	DImage image = { 0 };
	WCSdata wcs = { 0 };
	ImageWCSa wcsx = { 0 };
	ImageWCSa wcsy = { 0 };
	SigmaClip clip = { 0 };

	/* point task at stack variables */
	task.par = par;
	task.image = &image;
	task.wcs = &wcs;
	task.wcsx = &wcsx;
	task.wcsy = &wcsy;

	image.null = -999;
	image.warnings = 10;

	if (par->clip)
		{
			clip.nsigma = par->nsigma;
			task.clip = &clip;
		}

	if (!code)
		code = load_image(&task, par->infile);

	if (!code)
		code = iterate_regions(&task);

	if (!code)
		code = final_result(&task);

#ifdef DEALLOCATE_MEMORY
	dimage_release(&image);
#endif

	return code;
}


int
TOOLSUB ()
{
	int code = 0;

	Parameters p = { 0 };

	set_toolname(_STRINGIFY(TOOLSUB));
	set_toolversion(_STRINGIFY(VERSION));

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}

