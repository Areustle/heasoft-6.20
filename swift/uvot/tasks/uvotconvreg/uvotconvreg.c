/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotconvreg/uvotconvreg.c,v $
 * $Revision: 1.2 $
 * $Date: 2010/07/26 15:00:16 $
 *
 * $Log: uvotconvreg.c,v $
 * Revision 1.2  2010/07/26 15:00:16  rwiegand
 * (Re?)-fixing EXTVER value.
 *
 * Revision 1.1  2010/06/29 19:40:59  irby
 * Relocating uvotconvreg from uvot/tasks/uvotsource/uvotconvreg to
 * uvot/tasks/uvotconvreg.  To view prior history, use cvs log in
 * uvot/tasks/uvotsource/uvotconvreg/.
 *
 * Revision 1.2  2010/06/29 19:12:23  rwiegand
 * Enhanced to support writing FITS region tables.
 *
 * Revision 1.1  2008/12/16 22:37:47  rwiegand
 * Updated Makefile to build uvotconvreg.
 *
 */


#include <math.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "genimage.h"
#include "region.h" /* CFITSIO region interface */
#include "uvottool.h"


#define TOOLSUB uvotconvreg
#define VERSION 0.2
#include "headas_main.c"


typedef enum
{
	SHAPE_POINT,
	SHAPE_CIRCLE,
	SHAPE_ELLIPSE,
	SHAPE_ANNULUS,
	SHAPE_ELLIPTANNULUS,
	SHAPE_BOX,
	SHAPE_ROTBOX,
	SHAPE_BOXANNULUS,
	SHAPE_RECTANGLE,
	SHAPE_ROTRECTANGLE,
	SHAPE_POLYGON,
	SHAPE_PIE,
	SHAPE_SECTOR,
	SHAPE_DIAMOND,
	SHAPE_RHOMBUS,
	SHAPE_ROTDIAMOND,
	SHAPE_ROTRHOMBUS,
	SHAPE_PANDA,
	SHAPE_EPANDA,
	SHAPE_BPANDA
} WhichShape;


enum
{
	CONSTANT_MAX_POINTS = 1000,
	CONSTANT_MAX_PARAMS = 11,
	CONSTANT_DUMMY
};


typedef struct
{
	int chatter;
	int clobber;
	int history;

	char infile[PIL_LINESIZE];
	char wcsfile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	int tofits;
	int altwcs;

} Parameters;


typedef struct
{
	int order;
	double **p;

} SimplePolynomial;


typedef struct
{
	const Parameters * par;

	WCSdata * wcs;
	SAORegion * region;

	ImageWCSa * wcs1;
	ImageWCSa * wcs2;
	SimplePolynomial * sipap;
	SimplePolynomial * sipbp;

} Task;


typedef struct
{
	WhichShape id;
	shapeType type;
	const char *name;
	int points;

} ShapeClass;


typedef struct
{
	long component;

	ShapeClass *klazz;

	int sign;
	int points;
	double point[CONSTANT_MAX_POINTS];

} ShapeObject;


static int
dump_saoregion (SAORegion *reg)
{

	int i, j;

	if (!reg)
	{
		return (0);
	}

	/* write out the information we have accumulated */

	printf("Number of regions = %d\n", reg->nShapes);
	for (i = 0; i < reg->nShapes; i++)
	{
		RgnShape *shape = &reg->Shapes[i];
		printf("sign = %d, shape = %d, comp = %d\n",
		        shape->sign, shape->shape, shape->comp);
		if (shape->shape == poly_rgn)
		{
			printf("nPts = %d\n", shape->param.poly.nPts);
			for (j = 0; j < shape->param.poly.nPts; j++)
			{
				printf("%f ", shape->param.poly.Pts[j]);
			}
			printf("\n");
		}
		else
		{
			for (j = 0; j < CONSTANT_MAX_PARAMS; j++)
			{
				printf("%f ", shape->param.gen.p[j]);
			}
			printf("\n");
			printf("%f %f %f %f\n", shape->param.gen.sinT,
			        shape->param.gen.cosT, shape->param.gen.a,
			        shape->param.gen.b);
		}
		printf("%f %f %f %f\n", shape->xmin, shape->xmax, shape->ymin, shape->ymax);
	}

	return (0);
}


static void
convert_shape (Task *task, RgnShape *rgnShape, ShapeObject *object)
{
	int i, j;

	ShapeClass spec[] =
	{
		{ SHAPE_POINT,        point_rgn,          "POINT",	        2 },
		{ SHAPE_CIRCLE,       circle_rgn,         "CIRCLE",	        3 },
		{ SHAPE_ELLIPSE,      ellipse_rgn,        "ELLIPSE",	    5 },
		{ SHAPE_ANNULUS,      annulus_rgn,        "ANNULUS",	    4 },
		{ SHAPE_ELLIPTANNULUS,elliptannulus_rgn,  "ELLIPTANNULUS",  8 },
		{ SHAPE_BOX,          box_rgn,            "BOX", 		    5 },
		{ SHAPE_ROTBOX,       box_rgn,            "ROTBOX",	        5 },
		{ SHAPE_BOXANNULUS,   boxannulus_rgn,     "BOXANNULUS",     8 },
		{ SHAPE_RECTANGLE,    rectangle_rgn,      "RECTANGLE",      5 },
		{ SHAPE_ROTRECTANGLE, rectangle_rgn,      "ROTRECTANGLE",   5 },
		{ SHAPE_POLYGON,      poly_rgn,           "POLYGON",        0 },
		{ SHAPE_PIE,          sector_rgn,         "PIE",            4 },
		{ SHAPE_SECTOR,       sector_rgn,         "SECTOR",         4 },
		{ SHAPE_DIAMOND,      diamond_rgn,        "DIAMOND",        5 },
		{ SHAPE_RHOMBUS,      diamond_rgn,        "RHOMBUS",        5 },
		{ SHAPE_ROTDIAMOND,   diamond_rgn,        "ROTDIAMOND",     5 },
		{ SHAPE_ROTRHOMBUS,   diamond_rgn,        "ROTRHOMBUS",     5 },
		{ SHAPE_PANDA,        panda_rgn,          "PANDA",          8 },
		{ SHAPE_EPANDA,       epanda_rgn,         "EPANDA",         11 },
		{ SHAPE_BPANDA,       bpanda_rgn,         "BPANDA",         11 }
	};

	int nShapes = sizeof(spec) / sizeof(spec[0]);

	object->sign = rgnShape->sign;
	object->component = rgnShape->comp;

	for (i = 0; i < CONSTANT_MAX_POINTS; ++i)
		object->point[i] = 0;

	for (i = 0; i < nShapes; i++)
	{
		if (rgnShape->shape == spec[i].type)
		{
			object->klazz = &spec[i];
			if (rgnShape->shape == poly_rgn)
			{
				object->points = rgnShape->param.poly.nPts;
				if (object->points > CONSTANT_MAX_POINTS)
				{
					report_warning("truncating polygon from %d to %d points\n",
							object->points, CONSTANT_MAX_POINTS);
					object->points = CONSTANT_MAX_POINTS;
				}
				for (j = 0; j < object->points; ++j)
					object->point[j] = rgnShape->param.poly.Pts[j];
			}
			else
			{
				object->points = object->klazz->points;
				for (j = 0; j < object->points; ++j)
					object->point[j] = rgnShape->param.gen.p[j];
			}
		}
	}

}


static void
get_shape (Task *task, int i, ShapeObject *object)
{
	convert_shape(task, &task->region->Shapes[i], object);
}


static int
write_fits_region_aux (Task *task, fitsfile *fptr, int *pstatus)
{
	char *ttype[] = { "X", "Y", "SHAPE", "R", "ROTANG", "COMPONENT" };
	char *tform[] = { "1D", "1D", "16A", "1D", "1D", "1I" };
	char *tunit[] = { "pixel", "pixel", " ", "pixel", "degree", " " };

	char tformxy[16], tformvec[16], tformrot[16], buffshape[32];
	char ctype1[16], ctype2[16];

	WCSdata *wcs;

	int nFields = sizeof(ttype) / sizeof(ttype[0]);

	int i, j, npos, nshapes;
	int nvec, nrot;
	int shape;
	ShapeObject object = { 0 };
	int prec = 15;
	double *points;
	char *strdata[1];
	double zerod[1] = { 0 };


	nshapes = task->region->nShapes;

/*
c First loop through the regions finding the sizes of the arrays required.
*/
	npos = 1;
	nvec = 1;
	nrot = 1;

	for (i = 0; i < nshapes; ++i)
	{
		get_shape(task, i, &object);

		shape = object.klazz->id;

		if (shape == SHAPE_POLYGON)
			npos = object.points / 2;
	
		if (shape == SHAPE_BOX || shape == SHAPE_DIAMOND
				|| shape == SHAPE_ROTBOX || shape == SHAPE_ELLIPSE
				|| shape == SHAPE_ANNULUS || shape == SHAPE_SECTOR
				|| shape == SHAPE_PANDA)
			nvec = 2;

		if (shape == SHAPE_BOXANNULUS || shape == SHAPE_ELLIPTANNULUS
				|| shape == SHAPE_EPANDA || shape == SHAPE_BPANDA)
			nvec = 4;

		if (shape == SHAPE_BOXANNULUS || shape == SHAPE_ELLIPTANNULUS
				|| shape == SHAPE_SECTOR || shape == SHAPE_PANDA)
			nrot = 2;

		if (shape == SHAPE_EPANDA || shape == SHAPE_BPANDA)
			nrot = 3;
	}

	sprintf(tformxy, "%dD", npos);
	tform[0] = &tformxy[0];
	tform[1] = &tformxy[0];

	sprintf(tformvec, "%dD", nvec);
	sprintf(tformrot, "%dD", nrot);

	tform[3] = tformvec;
	tform[4] = tformrot;

/* create the new HDU */

	fits_create_tbl(fptr, BINARY_TBL, 0, nFields,
			ttype, tform, tunit,
			"REGION", pstatus);

	fits_update_key_lng(fptr, "EXTVER", 1, "", pstatus);

	fits_update_key_lng(fptr, "EXTLEVEL", 1,
			"Level i DB hierarchy: Data table", pstatus);

	fits_update_key_str(fptr, "HDUNAME", "REGION",
			"ASCDM block name", pstatus);

	fits_update_key_str(fptr, "HDUCLASS", "ASC",
			"Region extension", pstatus);

	fits_update_key_str(fptr, "HDUCLAS1", "REGION",
			"Region extension", pstatus);

	fits_update_key_str(fptr, "HDUCLAS2", "STANDARD",
			"Region extension", pstatus);

	fits_update_key_str(fptr, "HDUVERS", "1.0.0", "", pstatus);

	fits_update_key_str(fptr, "HDUDOC",
			"ASC-FITS-REGION-1.0: McDowell, Rots: "
					"FITS REGION Binary Table Design",
			"", pstatus);

	fits_update_key_str(fptr, "CONTENT", "REGION",
			"CXC Content Key", pstatus);

	if (*pstatus)
	{
		report_error("failed to write the standard keywords [%d]\n", *pstatus);
		return 0;
	}


/* write the data model (not implemented) */
	/* if (task->par->dmFlag) { } */

/* write the WCS keywords
	The original code noted that the region positions and sizes will be
	written in unbinned units.
*/

	fits_update_key_str(fptr, "RADECSYS", "ICRS", "", pstatus);
	fits_update_key_dbl(fptr, "EQUINOX", 2000, prec, "", pstatus);

	wcs = &task->region->wcs;
	sprintf(ctype1, "RA--%s", wcs->type);
	sprintf(ctype2, "DEC-%s", wcs->type);
	fits_update_key_str(fptr, "TCTYP1", ctype1, "", pstatus);
	fits_update_key_dbl(fptr, "TCRPX1", wcs->xrefpix, prec,
			"X axis reference pixel", pstatus);
	fits_update_key_dbl(fptr, "TCRVL1", wcs->xrefval, prec,
			"Coord of X ref pixel", pstatus);
	fits_update_key_dbl(fptr, "TCDLT1", wcs->xinc, prec,
			"X axis increment", pstatus);

	fits_update_key_str(fptr, "TCTYPE2", ctype2, "", pstatus);
	fits_update_key_dbl(fptr, "TCRPX2", wcs->yrefpix, prec,
			"Y axis reference pixel", pstatus);
	fits_update_key_dbl(fptr, "TCRVL2", wcs->yrefval, prec,
			"Coord of Y ref pixel", pstatus);
	fits_update_key_dbl(fptr, "TCDLT2", wcs->yinc, prec,
			"Y axis increment", pstatus);

/* loop over shapes writing the output arrays */

	for (i = 1; i <= nshapes; ++i)
	{

/* initialize the output arrays to zero */

		for (j = 1; j <= npos; ++j)
		{
			fits_write_col_dbl(fptr, 1, i, j, 1, zerod, pstatus);
			fits_write_col_dbl(fptr, 2, i, j, 1, zerod, pstatus);
		}

		for (j = 1; j <= nvec; ++j)
			fits_write_col_dbl(fptr, 4, i, j, 1, zerod, pstatus);

		for (j = 1; j <= nrot; ++j)
			fits_write_col_dbl(fptr, 5, i, j, 1, zerod, pstatus);

		get_shape(task, i-1, &object);

		shape = object.klazz->id;

		if (object.sign == 0)
			sprintf(buffshape, "!%s", object.klazz->name);
		else
			strcpy(buffshape, object.klazz->name);

		points = &object.point[0];

		if (shape != SHAPE_POLYGON)
		{
			fits_write_col_dbl(fptr, 1, i, 1, 1, &points[0], pstatus);
			fits_write_col_dbl(fptr, 2, i, 1, 1, &points[1], pstatus);
		}
		else
		{
			for (j = 1; j <= object.points / 2; ++j)
			{
				fits_write_col_dbl(fptr, 1, i, j, 1, &points[2*j-1], pstatus);
				fits_write_col_dbl(fptr, 2, i, j, 1, &points[2*j], pstatus);
			}
		}

		strdata[0] = buffshape;
		fits_write_col_str(fptr, 3, i, 1, 1, &strdata[0], pstatus);

		if (shape == SHAPE_PANDA)
		{
			fits_write_col_dbl(fptr, 4, i, 1, nvec, &points[5], pstatus);
			fits_write_col_dbl(fptr, 5, i, 1, nrot, &points[2], pstatus);
		}
		else if (shape == SHAPE_EPANDA || shape == SHAPE_BPANDA)
		{
			fits_write_col_dbl(fptr, 4, i, 1, nvec, &points[5], pstatus);
			fits_write_col_dbl(fptr, 5, i, 1, nrot-1, &points[2], pstatus);
			fits_write_col_dbl(fptr, 5, i, 3, 1, &points[10], pstatus);
		}
		else if (shape != SHAPE_POLYGON)
		{
			fits_write_col_dbl(fptr, 4, i, 1, nvec, &points[2], pstatus);

			if (shape != SHAPE_CIRCLE && shape != SHAPE_ANNULUS
					&& shape != SHAPE_POINT)
			{
				fits_write_col_dbl(fptr, 5, i, 1, nrot, &points[2+nvec], pstatus);
			}
		}

		fits_write_col_lng(fptr, 6, i, 1, 1, &object.component, pstatus);

		if (*pstatus)
			report_warning("failed to write region %d\n", i);

	}

	return *pstatus;
}


static int write_fits_region (Task *task, const char *outfile)
{
	int code = 0;
	int status = 0;
	fitsfile *fptr = 0;

	if (!status)
	{
		fits_create_file(&fptr, outfile, &status);
		if (status)
			report_error("unable to create %s [%d]\n", outfile, status);
	}

	if (!status)
	{
		write_fits_region_aux(task, fptr, &status);
		if (status)
			report_error("unable to write %s [%d]\n", outfile, status);
	}

	if (!status)
		HDpar_stamp(fptr, 2, &status);

	if (!status)
	{
		fits_close_file(fptr, &status);
		if (status)
			report_error("unable to close %s [%d]\n", outfile, status);
	}

	if (status)
		code = TASK_OUTPUT_ERROR;

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

	if (!code && !task->par->tofits && !task->region->Shapes[0].sign)
		{
			code = TASK_INPUT_ERROR;
			report_error("region begins with an exclude region [unsupported]\n");
		}

	return code;
}


static int
store_wcsa (const FITSHeader * header, ImageWCSa * wcsa, int axis, int altwcs)
{
	int code = 0;
	char suffix = 0;

	if (altwcs > 0)
		suffix = altwcs - 1 + 'A';

	if (image_get_wcsa(header, wcsa, axis, suffix))
		{
			code = TASK_INPUT_ERROR;
			report_error("unable to determine WCS%d%c info\n", axis, suffix);
		}

	return code;
}


static double
toDegrees (double radians)
{
	double degrees;
	degrees = radians * 180 / myPI;
	return degrees;
}


static int
store_wcssip (const FITSHeader * header, const char * tag,
		SimplePolynomial * sip)
{
	int code = 0;
	int i, j;
	char buffer[64];
	double zero = 0;

	sprintf(buffer, "%s_ORDER", tag);

	if (!code)
		code = get_header_key_integer(header, buffer, &sip->order, 0);

	if (!code)
		report_verbose("SIP %s order is %d\n", tag, sip->order);

	if (!code) {
		sip->p = calloc(sip->order+1, sizeof(double*));
		for (i = 0; i <= sip->order; ++i) {
			sip->p[i] = calloc(sip->order+1, sizeof(double));
		}
	}

	if (!code) {
		for (i = 0; i <= sip->order; ++i)
			for (j = 0; j <= sip->order; ++j) {
				sprintf(buffer, "%s_%d_%d", tag, i, j);
				get_header_key_double(header, buffer, &sip->p[i][j], &zero);
			}
	}

	return code;
}


static int
load_wcs (Task * task)
{
	int code = 0;
	FITSHeader header = { 0 };
	ImageWCSa * wcs1;
	ImageWCSa * wcs2;
	double pcij[4] = { 0 };

	wcs1 = task->wcs1;
	wcs2 = task->wcs2;
	wcs1->pcij = pcij;

	if (!code)
		code = fetch_header_records_path(&header, task->par->wcsfile);

	if (!code)
		code = store_wcsa(&header, wcs1, 1, task->par->altwcs);

	if (!code)
		code = store_wcsa(&header, wcs2, 2, task->par->altwcs);

	if (!code)
		{
			/* transfer WCS information to WCSdata */
			WCSdata * fwcs = task->wcs;

			fwcs->xrefpix = wcs1->crpix;
			fwcs->yrefpix = wcs2->crpix;
			fwcs->xrefval = wcs1->crval;
			fwcs->yrefval = wcs2->crval;
			fwcs->xinc = wcs1->cdelt;
			fwcs->yinc = wcs2->cdelt;
			fwcs->rot = toDegrees(atan2(wcs1->pcij[1], wcs1->pcij[0]));

			strncpy(fwcs->type, &wcs1->ctype[4], 4);

			fwcs->exists = 1;
		}

	if (!code && !strcmp(&wcs1->ctype[8], "-SIP"))
		code = store_wcssip(&header, "AP", task->sipap);

	if (!code && !strcmp(&wcs1->ctype[8], "-SIP"))
		code = store_wcssip(&header, "BP", task->sipbp);

	return code;
}


static double
fix_angle (double angle)
{
	if (angle >= 360)
		angle -= 360;
	else if (angle < 0)
		angle += 360;
	return angle;
}


static int
write_ascii_region (Task * task, const char * path)
{
	int code = 0;
	int i;
	RgnShape * shape;
	FILE * fp = 0;
	const char * incexc;

	fp = fopen(path, "w");
	if (!fp) {
		report_error("write_ascii_region: unable to create %s [%d]\n",
				path, errno);
		return TASK_OUTPUT_ERROR;
	}

	fprintf(fp, "image\n");

	for (i = 0; i < task->region->nShapes; ++i) {
		shape = task->region->Shapes + i;

		incexc = shape->sign ? "" : "-";

		switch (shape->shape) {
			case point_rgn:
				fprintf(fp, "%spoint(%f, %f)\n",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1]
						);
				break;

			case line_rgn:
				fprintf(fp, "%sline(%f, %f, %f, %f)\n",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				break;

			case circle_rgn:
				fprintf(fp, "%scircle(%f, %f, %f)\n",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2]
						);
				break;

			case annulus_rgn:
				fprintf(fp, "%sannulus(%f, %f, %f, %f)\n",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				break;

			case ellipse_rgn:
				fprintf(fp, "%sellipse(%f, %f, %f, %f",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				if (shape->param.gen.p[4] != 0)
					fprintf(fp, ", %f)\n", fix_angle(shape->param.gen.p[4]));
				else
					fprintf(fp, ")\n");
				break;

			case elliptannulus_rgn:
				code = TASK_OUTPUT_ERROR;
				report_error("elliptannulus_rgn: not supported\n");
				break;

			case box_rgn:
				fprintf(fp, "%sbox(%f, %f, %f, %f",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				if (shape->param.gen.p[4] != 0)
					fprintf(fp, ", %f)\n", fix_angle(shape->param.gen.p[4]));
				else
					fprintf(fp, ")\n");
				break;

			case rectangle_rgn:
				fprintf(fp, "%srectangle(%f, %f, %f, %f",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				if (shape->param.gen.p[4] != 0)
					fprintf(fp, ", %f)\n", fix_angle(shape->param.gen.p[4]));
				else
					fprintf(fp, ")\n");
				break;

			case diamond_rgn:
				fprintf(fp, "%sdiamond(%f, %f, %f, %f",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				if (shape->param.gen.p[4] != 0)
					fprintf(fp, ", %f)\n", fix_angle(shape->param.gen.p[4]));
				else
					fprintf(fp, ")\n");
				break;

			case sector_rgn:
				fprintf(fp, "%ssector(%f, %f, %f, %f)\n",
						incexc,
						shape->param.gen.p[0],
						shape->param.gen.p[1],
						shape->param.gen.p[2],
						shape->param.gen.p[3]
						);
				break;

			case poly_rgn:
				code = TASK_OUTPUT_ERROR;
				report_error("poly_rgn: not supported\n");
				break;

			default:
				code = TASK_OUTPUT_ERROR;
				report_error("unexpected region type %d\n", shape->shape);
				break;
		}
	}

	if (!code)
		report_status("wrote linear region %s\n", path);

	fclose(fp);

	return code;
}


static double
sip_pij (SimplePolynomial * sip, int i, int j)
{
	double pij = 0;
	if (i >= 0 && i <= sip->order && j >= 0 && j <= sip->order)
		pij = sip->p[i][j];
	return pij;
}


static int IMAX (int a, int b)
{
	if (a > b)
		return a;
	return b;
}


static int
sip_position (Task * task, double * px, double * py)
{
	int code = 0;
	int i, j, order;
	double dx, dy, dxhat, dyhat, dxidyj, pij;

	order = IMAX(task->sipap->order, task->sipbp->order);

	dx = *px - task->wcs1->crpix;
	dy = *py - task->wcs2->crpix;
	dxhat = dx;
	dyhat = dy;

	for (i = 0; i <= order; ++i) {
		for (j = 0; j <= order; ++j) {
			if ((i + j > 0) && (i + j <= order)) {
				dxidyj = pow(dx, i) * pow(dy, j);

				pij = sip_pij(task->sipap, i, j);
				if (pij != 0)
					dxhat += pij * dxidyj;

				pij = sip_pij(task->sipbp, i, j);
				if (pij != 0)
					dyhat += pij * dxidyj;
			}
		}
	}

	*px = dxhat + task->wcs1->crpix;
	*py = dyhat + task->wcs2->crpix;

	return code;
}


static int
sip_shape (Task * task, RgnShape * shape)
{
	int code = 0;

	/* the first two parameters are always a position */
	if (!code)
		code = sip_position(task,
				&shape->param.gen.p[0],
				&shape->param.gen.p[1]);

	/* for line_rgn and poly_rgn, subsequent parameter(s) are also positions */
	if (!code && shape->shape == line_rgn)
		code = sip_position(task,
				&shape->param.gen.p[2],
				&shape->param.gen.p[3]);

	return code;
}


static int
apply_sip (Task * task)
{
	int code = 0;
	int i;

	if (task->sipap->order == 0 && task->sipbp->order == 0)
		return code;

	report_verbose("apply_sip: AP order=%d, BP order=%d\n",
			task->sipap->order, task->sipbp->order);

	for (i = 0; !code && i < task->region->nShapes; ++i)
		code = sip_shape(task, task->region->Shapes + i);

	return code;
}


static int
run_parameters (Parameters * p)
{
	int code = 0;
	Task task = { 0 };
	WCSdata wcs = { 0 };
	SimplePolynomial sipap = { 0 };
	SimplePolynomial sipbp = { 0 };
	ImageWCSa wcs1 = { 0 };
	ImageWCSa wcs2 = { 0 };

	task.par = p;
	task.wcs = &wcs;
	task.wcs1 = &wcs1;
	task.wcs2 = &wcs2;
	task.sipap = &sipap;
	task.sipbp = &sipbp;

	if (!code)
		code = load_wcs(&task);

	if (!code)
		code = load_region(&task, p->infile);

	if (!code)
		code = apply_sip(&task);

	if (!code)
		headas_clobberfile(p->outfile);

	if (!code)
		{
			if (p->tofits)
				code = write_fits_region(&task, p->outfile);
			else
				code = write_ascii_region(&task, p->outfile);
		}

	return code;
}


static int
wcsAlternate (const char * s)
{
	int alt = -1;

	if (strlen(s) != 1)
		;
	else if (s[0] == '-')
		alt = 0;
	else if (islower(s[0]))
		alt = s[0] - 'a' + 1;
	else if (isupper(s[0]))
		alt = s[0] - 'A' + 1;

	if (alt < 0)
		report_error("invalid WCS system identifier '%s'\n", s);

	return alt;
}


static int
get_parameters (Parameters * par)
{
	int code = 0;
	int pill = 0;
	char outtype[PIL_LINESIZE];
	char altwcs[PIL_LINESIZE];

	if (!pill)
		pill = PILGetFname("infile", par->infile);

	if (!pill)
		pill = PILGetFname("wcsfile", par->wcsfile);

	if (!pill)
		pill = PILGetFname("outfile", par->outfile);

	if (!pill)
		pill = PILGetString("outtype", outtype);
	if (!pill)
		{
			if (!strcasecmp(outtype, "FITS"))
				par->tofits = 1;
		}

	if (!pill)
		pill = PILGetString("wcs", altwcs);
	if (!pill)
		{
			par->altwcs = wcsAlternate(altwcs);
			if (par->altwcs < 0)
				pill = -1;
		}

	par->chatter = headas_chatpar;
	par->clobber = headas_clobpar;

	if (pill)
		{
			code = TASK_SETUP_ERROR;
			report_error("unable to load parameters [%d]\n", pill);
		}
	else
		report_verbose("parameters loaded\n");

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

