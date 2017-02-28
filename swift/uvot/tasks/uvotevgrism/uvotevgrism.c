/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotevgrism/uvotevgrism.c,v $
 * $Revision: 1.13 $
 * $Date: 2005/09/26 21:17:18 $
 *
 *
 * $Log: uvotevgrism.c,v $
 * Revision 1.13  2005/09/26 21:17:18  rwiegand
 * Updated use of CALDBQuery expression.
 *
 * Revision 1.12  2005/09/12 14:25:00  rwiegand
 * Allow user to provide qualifiers for CALDB queries.  Added filter and
 * wheelpos expression to TELDEF CALDB query.
 *
 * Revision 1.11  2005/05/24 20:53:21  rwiegand
 * Moved CALDB interface from UVOT specific to attitude library.
 *
 * Revision 1.10  2005/05/20 20:29:01  rwiegand
 * Updated calibration file format.  Record file(s) retrieved from CALDB
 * in output.
 *
 * Revision 1.9  2004/10/14 17:36:48  rwiegand
 * Updated UV grism FILTER value.  Write parameter history and update checksums.
 *
 * Revision 1.8  2004/10/05 15:24:25  rwiegand
 * Updated format of grism calibration file to accommodate grism equation
 * and zeroth order offset variation across detector.
 *
 * Revision 1.7  2004/05/12 21:50:31  rwiegand
 * Was failing to apply [nm to A] conversion to grating equation coefficients.
 *
 * Revision 1.6  2004/05/12 20:41:51  rwiegand
 * Updated the grating equation calibration file format.  Intra-tool parameter
 * consistency effort.
 *
 * Revision 1.5  2003/12/03 14:49:00  rwiegand
 * Updated event table extension name to 'EVENTS'.
 *
 * Revision 1.4  2003/11/24 22:18:15  rwiegand
 * Use new HEAdas function for writing history keywords.
 *
 * Revision 1.3  2003/07/11 19:02:22  rwiegand
 * When loading grating equation from calibration file, was failing to save
 * coefficients.  Refactoring.
 *
 * Revision 1.2  2003/07/07 15:57:17  rwiegand
 * Load grating equation constants from calibration file.
 *
 * Revision 1.1  2003/06/24 19:19:16  rwiegand
 * UVOT event grism tool.  Determines which events are in the extraction region
 * and calculates the corresponding wavelength.
 *
 */


#include <stdio.h>							/* pil.h uses but does not include */
#include <string.h>
#include <math.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "keyutil.h"
#include "coordfits.h"
#include "caldbquery.h"


#define TOOLSUB uvotevgrism
#include "headas_main.c"


/* file related parameter names */
#define PAR_INFILE      "infile"
#define PAR_OUTFILE     "outfile"
#define PAR_WAVEFILE    "wavefile"
#define PAR_TELDEF      "teldeffile"
#define PAR_ATTFILE     "attfile"


/* FILTER keyword constants */
#define FILTER_UVGRISM   "UGRISM"
#define FILTER_VGRISM    "VGRISM"


enum
{
	GRISM_OK,
	GRISM_SETUP_ERROR,
	GRISM_INPUT_ERROR,
	GRISM_OUTPUT_ERROR,
	GRISM_FITS_ERROR,
	GRISM_DUMMY
};


typedef struct
{
	double angle;	/* angle from +X axis to dispersion axis
		              (0th -> 1st) [degrees]*/
	double width;	 /* width of accepted region [pixels] */
	double lambdamin;	/* wavelength boundaries */
	double lambdamax;

	double dmin; /* pixdist corresponding to lambdamin */
	double dmax;

} GrismParameters;


typedef struct
{
	int chatter;
	int clobber;
	int history;
	int checksum;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	char gratecal[PIL_LINESIZE];
	char teldef[PIL_LINESIZE];
	char attfile[PIL_LINESIZE];

	double ra;
	double dec;

	GrismParameters * uvgrism;
	GrismParameters * vgrism;

	double attmargin; /* time limit on attitude extrapolation */

} ToolParameters;


typedef struct
{
	double time;
	double detx;
	double dety;
	double lambda;

} Event;


typedef struct
{
	double x;
	double y;
} Vector;


typedef struct
{
	double xpix;
	double ypix;
	double xval;
	double yval;
	double xinc;
	double yinc;
	double rot;

	char type[FLEN_VALUE];

} ImageWCS;


typedef struct
{
	GrismParameters * grism;

	fitsfile * iptr;
	fitsfile * mptr;
	fitsfile * optr;

	TELDEF * teldef;
	ATTFILE * attfile;
	double attmargin;

	double ranom; /* nominal pointing during observation */
	double decnom;

	double skyx; /* sky pixel coordinates of zeroth order */
	double skyy;

	double tstart;
	double tstop;
	char filter[32];
	int wheelpos;

	char * precal; /* grating equation calibration extension prefix */
	double distance;

	int polydegree;
	double polycoeff[20];

	/* amount zeroth order is displaced from nominal position */
	double deltax;
	double deltay;

	/* time and detector position of zeroth order */
	double lastTime;
	double zerox;
	double zeroy;

	QUAT * quaternion;
	XFORM2D * sky2det;

	Vector dispersion_unit;
	Vector normal_unit;

	ImageWCS skyWCS;

	double grismeq_wave_scale;

} EventGrism;



static double
vector_dot (const Vector * u, const Vector * v)
{
	double dot = u->x * v->x + u->y * v->y;
	return dot;
}


int
get_parameters (ToolParameters * p)
{
	int code = 0;
	int pill = 0;
	GrismParameters * grism = 0;

	if (!pill)
		pill = PILGetFname(PAR_OUTFILE, p->outfile);

	if (!pill)
		pill = PILGetFname(PAR_INFILE, p->infile);

	if (!pill)
		pill = PILGetFname(PAR_WAVEFILE, p->gratecal);

	if (!pill)
		pill = PILGetFname(PAR_TELDEF, p->teldef);

	if (!pill)
		pill = PILGetFname(PAR_ATTFILE, p->attfile);

	if (!pill)
		pill = PILGetReal("ra", &p->ra);

	if (!pill)
		pill = PILGetReal("dec", &p->dec);

	/* grab UV grism parameters */
	grism = p->uvgrism;
	if (!pill)
		pill = PILGetReal("uvang", &grism->angle);

	if (!pill)
		pill = PILGetReal("uvwid", &grism->width);

	if (!pill)
		pill = PILGetReal("uvmin", &grism->lambdamin);

	if (!pill)
		pill = PILGetReal("uvmax", &grism->lambdamax);

	/* grab V grism parameters */
	grism = p->vgrism;
	if (!pill)
		pill = PILGetReal("vang", &grism->angle);

	if (!pill)
		pill = PILGetReal("vwid", &grism->width);

	if (!pill)
		pill = PILGetReal("vmin", &grism->lambdamin);

	if (!pill)
		pill = PILGetReal("vmax", &grism->lambdamax);

	if (!pill)
		pill = PILGetReal("attlim", &p->attmargin);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	if (pill)
		{
			code = GRISM_SETUP_ERROR;
			report_error("unable to load parameters\n");
		}
	else
		report_verbose("parameters loaded\n");

	return code;
}


int
update_zeroth_order_position (EventGrism * tool, Event * event)
{
	int code = 0;

	if (event->time == tool->lastTime)
		return code;

	tool->lastTime = event->time;

	if (!isInExtrapolatedAttFile(tool->attfile, event->time))
		{
			code = GRISM_INPUT_ERROR;
			report_error("unable to determine quaternion at %.3f\n", event->time);
		}
	else
		{
			double x, y;
			double v, vhat[3];
			COORDDEF * det = tool->teldef->det[0];

			/* determine velocity */
			v = 0;

			findQuatInAttFile(tool->attfile, tool->quaternion, event->time);

			/* prime teldef->det2sky (this x and y are ignored) */
			convertDetectorToSkyUsingTeldef(tool->teldef, &x, &y,
											det->center_x, det->center_y, tool->quaternion, v, vhat);

			invertXform2d(tool->sky2det, tool->teldef->det2sky);

			applyXform2dToContinuousCoords(tool->sky2det,
											&x, &y,
											tool->skyx, tool->skyy);

			tool->zerox = x + tool->deltax;
			tool->zeroy = y + tool->deltay;
		}

	return code;
}


/* 
 * Since lambda = poly(offset) is nearly linear, treat it as such.
 *
 * Note: this is only used for placing spatial boundaries on the events which
 * are processed
 */
static double
lambda_to_offset (EventGrism * tool, double lambda)
{
	double * coeff = tool->polycoeff;
	double offset = (lambda - coeff[0]) / coeff[1];
	return offset;
}


static double
offset_to_lambda (EventGrism * tool, double offset)
{
	int i;
	double offupi = offset;
	double lambda = tool->polycoeff[0];
	for (i = 1; i <= tool->polydegree; ++i)
		{
			lambda += tool->polycoeff[i] * offupi;
			offupi *= offset;
		}

	return lambda;
}


/*
 * Construct a vector, v, from the zeroth order position to this event.
 * Let u be the unit vector along the dispersion axis.
 * Let n be a unit normal to u.
 * Determine the projection of v onto u and call it a.
 * Determine the projection of v onto n and call it b.
 *
 * Then the event is of interest if
 *    abs(b) < width / 2
 * and
 *    dmin <= a <= dmax
 */
int
determine_wavelength (EventGrism * tool, Event * event)
{
	int code = 0;

	Vector v;
	double a, b;
	GrismParameters * grism = tool->grism;

	event->lambda = 0;
	v.x = event->detx - tool->zerox;
	v.y = event->dety - tool->zeroy;

	a = vector_dot(&v, &tool->dispersion_unit);
	b = vector_dot(&v, &tool->normal_unit);

	if ( (fabs(b) < grism->width / 2)
			&& (grism->dmin <= a)
			&& (a <= grism->dmax) )
		event->lambda = offset_to_lambda(tool, a);

	return code;
}



enum
{
	EVENT_TIME,
	EVENT_DETX,
	EVENT_DETY,
	EVENT_LAMBDA,
	EVENT_COLUMNS
};


int
iterate_events_aux (long totaln, long offset, long firstn,
								long nvalues, int narrays, iteratorCol * icols,
								void * user)
{
	int code = 0;

	int i;
	EventGrism * tool = (EventGrism *) user;

	double * ptime;
	double * pdetx;
	double * pdety;
	double * plambda;

	report_verbose("iterate_events_aux(totaln %ld, offset %ld, firstn %ld, nvalues %ld, narrays %d, ...)\n", totaln, offset, firstn, nvalues, narrays);
	ptime = (double *) fits_iter_get_array(icols + EVENT_TIME);
	pdetx = (double *) fits_iter_get_array(icols + EVENT_DETX);
	pdety = (double *) fits_iter_get_array(icols + EVENT_DETY);
	plambda = (double *) fits_iter_get_array(icols + EVENT_LAMBDA);

	/* 0th value indicates null */
	plambda[0] = 0;

	for (i = 1; !code && i <= nvalues; ++i)
		{
			Event event;

			event.time = ptime[i];
			event.detx = pdetx[i];
			event.dety = pdety[i];

			code = update_zeroth_order_position(tool, &event);

			if (!code)
				code = determine_wavelength(tool, &event);

			plambda[i] = event.lambda;
		}

	return code;
}


static void
initialize_iterator_column (iteratorCol * icol, char * name, fitsfile * fptr,
							int datatype, int coltype, int * status)
{
	int tmp = fits_iter_set_by_name(icol, fptr, name, datatype, coltype);

	if (tmp)
		{
			if (!*status)
				*status = tmp;
			report_error("unable to initialize iterator column '%s' [%d]\n",
					name, status);
		}
}



int
iterate_events (EventGrism * tool)
{
	int code = 0;
	int status = 0;

	iteratorCol icols[EVENT_COLUMNS] = { { 0 } };

	initialize_iterator_column(&icols[EVENT_TIME], "TIME",
									tool->mptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[EVENT_DETX], "DETX",
									tool->mptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[EVENT_DETY], "DETY",
									tool->mptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[EVENT_LAMBDA], "WAVELENGTH",
									tool->mptr, TDOUBLE, OutputCol, &status);

	if (status)
		{
			code = status;
			report_error("unable to initialize event iteration [%d]\n", status);
		}

	fits_iterate_data(EVENT_COLUMNS, icols, 0, 0, iterate_events_aux, tool, &status);
	if (status)
		{
			code = status;
			report_error("event iteration failed [%d]\n", status);
		}

	return code;
}


static double
degrees_to_radians (double degrees)
{
	double radians = M_PI * degrees / 180;
	return radians;
}


static int
load_wcs_aux (fitsfile * fptr, const char * colname,
			double * pix, double * val, double * inc, char * typ)
{
	/* find TCTYPn matching type
	 * get associated TCRPXn, TCRVLn, TCDLTn
	 */
	int done = 0;
	int col = 0;

	char tcrpxn[16];
	char tcrvln[16];
	char tcdltn[16];
	char tctypn[16];

	while (!done)
		{
			int status = 0;
			char ttype[16];
			char value[FLEN_VALUE];

			++col;
			sprintf(ttype, "TTYPE%d", col);

			fits_read_key(fptr, TSTRING, ttype, value, 0, &status);
			if (status)
				{
					done = -1;
					report_error("trouble getting %s [%d]\n", ttype, status);
				}
			else if (!strcmp(value, colname))
				{
					done = 1;
					sprintf(tcrpxn, "TCRPX%d", col);
					sprintf(tcrvln, "TCRVL%d", col);
					sprintf(tcdltn, "TCDLT%d", col);
					sprintf(tctypn, "TCTYP%d", col);
				}
		}

	if (done > 0)
		{
			int status = 0;

			fits_read_key(fptr, TDOUBLE, tcrpxn, pix, 0, &status);
			fits_read_key(fptr, TDOUBLE, tcrvln, val, 0, &status);
			fits_read_key(fptr, TDOUBLE, tcdltn, inc, 0, &status);
			fits_read_key(fptr, TSTRING, tctypn, typ, 0, &status);

			if (status)
				{
					done = -1;
					report_error("unable to read TCRPXn, TCRVLn, ... [%d]\n", status);
				}
		}

	/* indicate error if done < 0 */
	return done < 0;
}



static int
load_wcs (fitsfile * fptr, ImageWCS * wcs, char * xname, char * yname)
{
	int code = 0;
	char xtype[16];
	char ytype[16];

	wcs->rot = 0;

	if (!code)
		code = load_wcs_aux(fptr, xname, &wcs->xpix, &wcs->xval,
										&wcs->xinc, xtype);

	if (!code)
		code = load_wcs_aux(fptr, yname, &wcs->ypix, &wcs->yval,
										&wcs->yinc, ytype);

	if (!code)
		{
			/* given x and y TCTYPn ABCD-XYZ, make sure the -XYZ match  */
			if (strcmp(xtype + 4, ytype + 4))
				{
					code = 1;
					report_error("mismatch between sky X [%s] and Y [%s] projections\n",
													xtype, ytype);
				}
			else
				strcpy(wcs->type, xtype + 4);
		}

	return code;
}


enum
{
	GRISMEQ_CLOCKING,
	GRISMEQ_DETX,
	GRISMEQ_DETY,
	GRISMEQ_COEFF,
	GRISMEQ_COLUMNS
};


int
iterate_grating_equation (long totaln, long offset, long firstn, long nvalues,
            int narrays, iteratorCol *icols, void * user)
{
	int status = 0;
	EventGrism * tool = (EventGrism *) user;
	long i;
	int * clocking;
	float * detx;
	float * dety;
	double * coeff;

	clocking = (int *)    fits_iter_get_array(icols + GRISMEQ_CLOCKING);
	detx     = (float *)  fits_iter_get_array(icols + GRISMEQ_DETX);
	dety     = (float *)  fits_iter_get_array(icols + GRISMEQ_DETY);
	coeff    = (double *) fits_iter_get_array(icols + GRISMEQ_COEFF);

	if (offset == 0)
		{
			tool->polydegree = icols[GRISMEQ_COEFF].repeat;
			report_verbose("grating equation COEFF repeat is %d\n", tool->polydegree);
		}

	for (i = 1; i <= nvalues; ++i)
		{
			double scale = 1;
			double distance;

			if (clocking[i] != tool->wheelpos)
				continue;

			/* determine distance of zeroth order from this entry */
			distance = hypot(tool->zerox - detx[i], tool->zeroy - dety[i]);

			if (tool->distance < 0 || distance < tool->distance)
				{
					int e, o;
					tool->distance = distance;
					o = (i - 1) * tool->polydegree + 1;
					for (e = 0; e < tool->polydegree; ++e)
						tool->polycoeff[e] = coeff[o + e] * scale;
				}

		}

	return status;
}


static int
determine_grating_equation (EventGrism * tool, fitsfile * fptr)
{
	int status = 0;
	iteratorCol icols[GRISMEQ_COLUMNS] = { { 0 } };
	char calext[32];

	sprintf(calext, "%sGRISMEQ", tool->precal);
	if (!status)
		if (fits_movnam_hdu(fptr, BINARY_TBL, calext, 0, &status))
			report_error("unable to move to %s extension of cal file [%d]\n",
										calext, status);

	if (!status) {
		char wav_unit[FLEN_VALUE];
		if (fits_read_key_str(fptr, "WAV_UNIT", wav_unit, 0, &status))
			report_error("unable to read WAV_UNIT keyword from [%s] [%d]\n",
						calext, status);
		else {
			if (!strcasecmp(wav_unit, "A")
					|| !strcasecmp(wav_unit, "angstrom")
					|| !strcasecmp(wav_unit, "angstroms"))
				tool->grismeq_wave_scale = 1;
			else if (!strcasecmp(wav_unit, "nm"))
				tool->grismeq_wave_scale = 10;
			else 
				{
					status = KEY_OUT_BOUNDS;
					report_error("unknown wavelength unit '%s' in [%s]\n",
												wav_unit, calext);
				}
		}
	}

	if (!status) {
		char dsp_unit[FLEN_VALUE];

		if (fits_read_key_str(fptr, "DSP_UNIT", dsp_unit, 0, &status))
			report_error("unable to read DSP_UNIT keyword from [%s] [%d]\n",
						calext, status);
		else {

			if (strcasecmp(dsp_unit, "pixel")
					&& strcasecmp(dsp_unit, "pixels"))
				{
					status = KEY_OUT_BOUNDS;
					report_warning("unknown DSP_UNIT '%s' in [%s]\n",
							dsp_unit, calext);
				}
		}
	}

	if (!status) {
		initialize_iterator_column(&icols[GRISMEQ_CLOCKING], "CLOCKING", fptr,
				TINT, InputCol, &status);
		initialize_iterator_column(&icols[GRISMEQ_DETX], "DETX", fptr,
				TFLOAT, InputCol, &status);
		initialize_iterator_column(&icols[GRISMEQ_DETY], "DETY", fptr,
				TFLOAT, InputCol, &status);
		initialize_iterator_column(&icols[GRISMEQ_COEFF], "COEFF", fptr,
				TDOUBLE, InputCol, &status);
	}

	if (status)
		report_error("unable to initialize grating calibration iteration\n");

	tool->distance = -1;
	report_status("zeroth order at DET %.3f, %.3f\n", tool->zerox, tool->zeroy);

	if (!status)
		status = fits_iterate_data(GRISMEQ_COLUMNS, icols,
							0 /* offset */, 0 /* default rows per iteration */,
							&iterate_grating_equation, tool, &status);

	if (tool->distance < 0)
		status = 1;

	if (status)
		report_error("unable to determine grating equation\n");
	else
		{
			int i;
			char buffer[1024];
			buffer[0] = 0;
			for (i = 1; i <= tool->polydegree; ++i)
				if (tool->polycoeff[i])
					{
						char tmp[64];
						sprintf(tmp, "\t\t%+.3f * offset ^ %d\n", tool->polycoeff[i], i);
						strcat(buffer, tmp);
					}
			report_status("grating_equation:\n\tlambda[A] = %.3f\n%s",
					tool->polycoeff[0], buffer);
		}

	return status;
}


enum
{
	ZEROTH_OFFSET_CLOCKING,
	ZEROTH_OFFSET_DETX,
	ZEROTH_OFFSET_DETY,
	ZEROTH_OFFSET_DELTAX,
	ZEROTH_OFFSET_DELTAY,
	ZEROTH_OFFSET_COLUMNS
};


int
iterate_zeroth_offset (long totaln, long offset, long firstn, long nvalues,
            int narrays, iteratorCol *icols, void * user)
{
	int status = 0;
	EventGrism * tool = (EventGrism *) user;
	long i;
	int * clocking;
	float * detx;
	float * dety;
	float * deltax;
	float * deltay;

	clocking = (int *)    fits_iter_get_array(icols + ZEROTH_OFFSET_CLOCKING);
	detx     = (float *)  fits_iter_get_array(icols + ZEROTH_OFFSET_DETX);
	dety     = (float *)  fits_iter_get_array(icols + ZEROTH_OFFSET_DETY);
	deltax   = (float *)  fits_iter_get_array(icols + ZEROTH_OFFSET_DELTAX);
	deltay   = (float *)  fits_iter_get_array(icols + ZEROTH_OFFSET_DELTAY);


	for (i = 1; i <= nvalues; ++i)
		{
			double distance;

			if (clocking[i] != tool->wheelpos)
				continue;

			/* determine distance of zeroth order from this entry */
			distance = hypot(tool->zerox - detx[i], tool->zeroy - dety[i]);

			/* update to bilinear interpolation? */
			if (tool->distance < 0 || distance < tool->distance)
				{
					tool->distance = distance;
					tool->deltax = deltax[i];
					tool->deltay = deltay[i];
				}

		}

	return status;
}


static int
determine_zeroth_order_offset (EventGrism * tool, fitsfile * fptr)
{
	int status = 0;
	iteratorCol icols[ZEROTH_OFFSET_COLUMNS] = { { 0 } };
	char calext[16];

	sprintf(calext, "%sTRANSLATION", tool->precal);
	if (!status)
		if (fits_movnam_hdu(fptr, BINARY_TBL, calext, 0, &status))
			report_error("unable to move to %s extension of grism calibration [%d]\n",
							calext, status);

	initialize_iterator_column(&icols[ZEROTH_OFFSET_CLOCKING], "CLOCKING", fptr,
				TINT, InputCol, &status);
	initialize_iterator_column(&icols[ZEROTH_OFFSET_DETX], "DETX", fptr,
				TFLOAT, InputCol, &status);
	initialize_iterator_column(&icols[ZEROTH_OFFSET_DETY], "DETY", fptr,
				TFLOAT, InputCol, &status);
	initialize_iterator_column(&icols[ZEROTH_OFFSET_DELTAX], "DELTAX", fptr,
				TFLOAT, InputCol, &status);
	initialize_iterator_column(&icols[ZEROTH_OFFSET_DELTAY], "DELTAY", fptr,
				TFLOAT, InputCol, &status);

	if (status)
		report_error("unable to initialize grism offset iteration\n");

	tool->distance = -1;

	if (!status)
		status = fits_iterate_data(ZEROTH_OFFSET_COLUMNS, icols,
							0 /* offset */, 0 /* default rows per iteration */,
							&iterate_zeroth_offset, tool, &status);

	if (tool->distance < 0)
		status = 1;

	if (status)
		report_error("unable to determine zeroth order displacement\n");
	else
		report_status("zeroth order displacement %.3f, %.3f\n",
					tool->deltax, tool->deltay);

	return status;
}


static int
load_grism_calibration (EventGrism * tool, const char * path)
{
	int status = 0;
	fitsfile * fptr = 0;

	fits_open_file(&fptr, path, READONLY, &status);
	if (status)
		report_error("unable to open %s [%d]\n", path, status);

	if (!status)
		status = determine_grating_equation(tool, fptr);

	if (!status)
		status = determine_zeroth_order_offset(tool, fptr);

	if (fptr)
		{
			int tmp = 0;
			fits_close_file(fptr, &tmp);
		}

	return status;
}


int
run_parameters (ToolParameters * p)
{
	int code = 0;
	int status = 0;

	EventGrism tool = { 0 };

	if (!code)
		{
			tool.quaternion = allocateQuat();
			tool.sky2det = allocateXform2d();
		}

	if (p->clobber)
		headas_clobberfile(p->outfile);

	if (!code)
		{
			/* open input */
			fits_open_file(&tool.iptr, p->infile, READONLY, &status);
			if (status)
				{
					code = status;
					report_error("unable to open %s [%d]\n", p->infile, status);
				}
		}

	if (!code)
		{
			/* move to EVENTS extension */

			fits_movnam_hdu(tool.iptr, ANY_HDU, "EVENTS", 0, &status);
			if (status)
				{
					code = status;
					report_error("unable to move to EVENTS extension [%d]\n", status);
				}
		}

	if (!code)
		{
			/* create memory copy of EVENTS extension */
			char memname[] = "mem://uvotevgrism";

			fits_create_file(&tool.mptr, memname, &status);
			if (status)
				{
					code = status;
					report_error("unable to create %s [%d]\n", memname, status);
				}
		}

	if (!code)
		{
			fits_copy_hdu(tool.iptr, tool.mptr, 0, &status);
			if (status)
				{
					code = status;
					report_error("unable to copy EVENTS extension [%d]\n", status);
				}
		}

	if (!code)
		{
			/* add WAVELENGTH column */
			int numcol = 0;
			fits_get_num_cols(tool.mptr, &numcol, &status);
			if (status)
				{
					code = status;
					report_error("unable to determine number of columns [%d]\n", status);
				}

			fits_insert_col(tool.mptr, numcol + 1, "WAVELENGTH", "1E", &status);
			if (status)
				{
					code = status;
					report_error("unable to add WAVELENGTH column [%d]\n", status);
				}
		}

	if (!code)
		{
			/* open output */
			/* not written to for a long time, but ensures we can */
			fits_create_file(&tool.optr, p->outfile, &status);
			if (status)
				{
					code = status;
					report_error("unable to create %s [%d]\n", p->outfile, status);
				}
		}

	if (!code)
		{
			/* determine which grism */
			char value[FLEN_VALUE];
			char comment[FLEN_COMMENT];
			long tmp;

			fits_read_key_str(tool.mptr, "FILTER", value, comment, &status);
			fits_read_key_lng(tool.mptr, "WHEELPOS", &tmp, comment, &status);
			if (status)
				{
					code = status;
					report_error("unable to read FILTER/WHEELPOS[%d]\n", status);
				}
			else if (!strcmp(value, FILTER_UVGRISM))
				{
					tool.grism = p->uvgrism;
					tool.precal = "U";
				}
			else if (!strcmp(value, FILTER_VGRISM))
				{
					tool.grism = p->vgrism;
					tool.precal = "V";
				}
			else
				{
					code = GRISM_INPUT_ERROR;
					report_error("unexpected FILTER keyword value '%s'\n", value);
				}

			if (!code)
				{
					strcpy(tool.filter, value);
					tool.wheelpos = (int) tmp;
				}
		}

	if (!code)
		{
			char comment[FLEN_COMMENT];

			fits_read_key_dbl(tool.mptr, "TSTART", &tool.tstart, comment, &status);
			fits_read_key_dbl(tool.mptr, "TSTOP", &tool.tstop, comment, &status);
			fits_read_key_dbl(tool.mptr, "RA_NOM", &tool.ranom, comment, &status);
			fits_read_key_dbl(tool.mptr, "DEC_NOM", &tool.decnom, comment, &status);
			if (status)
				{
					code = status;
					report_error("unable to read input keywords [%d]\n", status);
				}
		}

	if (!code)
		code = load_wcs(tool.iptr, &tool.skyWCS, "X", "Y");

	if (!code)
		{
			/* load TELDEF */
			char path[QUERY_MAXPATH];
			if (!strncasecmp(p->teldef, "CALDB", 5))
				{
					CALDBQuery query = { 0 };
					strcpy(query.codename, "TELDEF");
					sprintf(query.expression, "FILTER.eq.%s.and.WHEELPOS.eq.%d",
							tool.filter, tool.wheelpos);
					set_caldb_query_qualifiers(&query, p->teldef);
					code = simple_caldb_query(&query, tool.iptr, path);
					if (!code)
						HDpar_note(PAR_TELDEF, path);
				}
			else
				strcpy(path, p->teldef);

			if (!code)
				tool.teldef = readTelDef(path);

			if (!tool.teldef)
				{
					code = GRISM_INPUT_ERROR;
					report_error("unable to load telescope definition %s\n", p->teldef);
				}
			else
				{
					ImageWCS * wcs = &tool.skyWCS;

					fits_world_to_pix(p->ra, p->dec, wcs->xval, wcs->yval,
							wcs->xpix, wcs->ypix, wcs->xinc, wcs->yinc,
							wcs->rot, wcs->type, &tool.skyx, &tool.skyy, &status);

					if (status)
						{
							code = GRISM_INPUT_ERROR;
							report_error("unable to convert RA, DEC to sky X, Y\n");
						}
					else
						report_status("RA %.3f, DEC %.3f => skyx %.3f, skyy %.3f\n",
														p->ra, p->dec, tool.skyx, tool.skyy);

					setSkyCoordCenterInTeldef(tool.teldef, tool.ranom, tool.decnom);
				}
		}

	if (!code)
		{
			/* initialize attitude */
			tool.attfile = openAttFile(p->attfile);
			if (!tool.attfile)
				{
					code = GRISM_INPUT_ERROR;
					report_error("unable to load attitude file %s\n", p->attfile);
				}
			else
				resetAttFileExtrapolationLimits(tool.attfile, p->attmargin);
		}

	if (!code)
		{
			Event event = { 0 };
			event.time = (tool.tstart + tool.tstop) / 2;
			update_zeroth_order_position(&tool, &event);
		}

	if (!code)
		{
			char path[QUERY_MAXPATH];
			if (!strncasecmp(p->gratecal, "CALDB", 5))
				{
					CALDBQuery query = { 0 };
					strcpy(query.codename, "GRISMEQ");
					set_caldb_query_qualifiers(&query, p->gratecal);
					code = simple_caldb_query(&query, tool.iptr, path);
					if (!code)
						HDpar_note(PAR_WAVEFILE, path);
				}
			else
				strcpy(path, p->gratecal);

			if (!code)
				code = load_grism_calibration(&tool, path);
		}

	if (!code)
		{
			/* calculate derived grism parameters */
			GrismParameters * grism = tool.grism;

			Vector * u = &tool.dispersion_unit;
			Vector * n = &tool.normal_unit;

			grism->dmin = lambda_to_offset(&tool, grism->lambdamin);
			grism->dmax = lambda_to_offset(&tool, grism->lambdamax);

			u->x = cos(degrees_to_radians(grism->angle));
			u->y = sin(degrees_to_radians(grism->angle));

			n->x = -u->y;
			n->y = u->x;
		}

	if (!code)
		code = iterate_events(&tool);

	if (!code)
		{
			/* filter out useless events */
			long rows;
			fits_get_num_rows(tool.mptr, &rows, &status);
			report_status("initial event count %ld\n", rows);

			fits_select_rows(tool.mptr, tool.mptr, "WAVELENGTH.ne.0", &status);
			if (status)
				{
					code = status;
					report_error("unable to select extracted events [%d]\n", status);
				}

			fits_get_num_rows(tool.mptr, &rows, &status);
			report_status("filtered event count %ld\n", rows);
		}

	if (!code)
		{
			/* copy primary array */
			fits_movabs_hdu(tool.iptr, 1, 0, &status);
			fits_copy_hdu(tool.iptr, tool.optr, 0, &status);
			if (status)
				{
					code = status;
					report_error("unable to copy primary array [%d]\n", status);
				}
		}

	if (!code)
		{
			/* copy updated events table */
#define EVENT_HDU 2
			fits_movabs_hdu(tool.mptr, EVENT_HDU, 0, &status);
			fits_copy_hdu(tool.mptr, tool.optr, 0, &status);
			if (status)
				{
					code = status;
					report_error("unable to copy filtered event table [%d]\n", status);
				}

			/* write history, checksum */
			if (p->history)
				HDpar_stamp(tool.optr, EVENT_HDU, &status);
			if (p->checksum)
				fits_write_chksum(tool.optr, &status);
			if (status)
				{
					code = status;
					report_error("unable to write history / checksum [%d]\n", status);
				}
		}

	if (!code)
		{
			/* copy remaining extensions (GTInn and WINDOW) */
			int i;
			int hdus = 0;
			fits_get_num_hdus(tool.iptr, &hdus, &status);
			for (i = EVENT_HDU + 1; i <= hdus; ++i)
				{
					fits_movabs_hdu(tool.iptr, i, 0, &status);
					fits_copy_hdu(tool.iptr, tool.optr, 0, &status);
				}
			if (status)
				{
					code = status;
					report_error("unable to copy remaining extensions [%d]\n", status);
				}
		}

	if (tool.iptr)
		{
			/* close input */
			int tmp = 0;
			fits_close_file(tool.iptr, &tmp);
		}

	if (tool.optr)
		{
			int tmp = 0;

			/* bookkeep */
			if (p->history)
				HDpar_stamp(tool.optr, 2, &tmp);
			fits_write_chksum(tool.optr, &tmp);
			if (tmp)
				{
					code = tmp;
					report_error("unable to update history/checksums [%d]\n", tmp);
				}

			/* close output */
			tmp = 0;
			if (code)
				fits_delete_file(tool.optr, &tmp);
			else
				fits_close_file(tool.optr, &tmp);
		}

	/* cleanup */
	if (tool.teldef)
		destroyTelDef(tool.teldef);

	if (tool.attfile)
		closeAttFile(tool.attfile);

	if (tool.sky2det)
		destroyXform2d(tool.sky2det);

	if (tool.quaternion)
		destroyQuat(tool.quaternion);

	return code;
}


int
uvotevgrism ()
{
	int code = 0;

	ToolParameters p = { 0 };
	GrismParameters uvpar = { 0 };
	GrismParameters vpar = { 0 };
	p.uvgrism = &uvpar;
	p.vgrism = &vpar;

	set_toolversion("0.2");

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}

