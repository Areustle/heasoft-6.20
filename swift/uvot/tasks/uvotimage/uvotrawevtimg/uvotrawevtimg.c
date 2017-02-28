/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotimage/uvotrawevtimg/uvotrawevtimg.c,v $
 * $Revision
 * $Date: 2007/12/03 22:20:57 $
 *
 * $Log: uvotrawevtimg.c,v $
 * Revision 1.3  2007/12/03 22:20:57  rwiegand
 * Corrected code for randomizing event positions within pixels to call
 * HDmtDrand() [was calling HDmtRand()].
 *
 * Revision 1.2  2007/11/15 16:38:04  rwiegand
 * Implemented randomization of event position within pixel.
 *
 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "report.h"
#include "headas.h"
#include "headas_rand.h"
#include "pil.h"
#include "fitsio.h"

#include "keyutil.h"
#include "teldef.h"
#include "attfile.h"
#include "genimage.h"
#include "quat.h"


#define TOOLSUB rawevtimg
#include "headas_main.c"



typedef struct
{
	int dummy;

	char eventfile[PIL_LINESIZE];
	char attfile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	char teldef[PIL_LINESIZE];
	double trel, t1, t2;

	int x0, y0, dx, dy;

	int randomize;

	int chatter;
	int clobber;
	int history;

} Parameters;



typedef struct
{
	const Parameters * par;

	TELDEF * teldef;
	ATTFILE * attfile;
	QUAT * q;
	XFORM2D * sky2det;

	double ranom, decnom;

	int detx0, dety0;

	fitsfile * fptr;

	IImage * image;

} Task;


enum
{
	COL_TIME,
	COL_RAWX,
	COL_RAWY,
	COL_COUNT
};


static int
get_parameters (Parameters * p)
{
	int code = 0;

	if (!code)
		code = PILGetFname("eventfile", p->eventfile);

	if (!code)
		code = PILGetFname("attfile", p->attfile);

	if (!code)
		code = PILGetFname("outfile", p->outfile);

	if (!code)
		code = PILGetFname("teldeffile", p->teldef);

	if (!code)
		code = PILGetInt("x0", &p->x0);

	if (!code)
		code = PILGetInt("y0", &p->y0);

	if (!code)
		code = PILGetInt("dx", &p->dx);

	if (!code)
		code = PILGetInt("dy", &p->dy);

	if (!code)
		code = PILGetReal("trel", &p->trel);

	if (!code)
		code = PILGetReal("t1", &p->t1);

	if (!code)
		code = PILGetReal("t2", &p->t2);

	if (!code)
		code = PILGetInt("randomize", &p->randomize);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	return code;
}


static int
iterate_events_aux (long totaln, long offset, long firstn,
					long nrows, int narrays, iteratorCol * icols,
					void * user)
{
	int code = 0;
	Task * task = (Task *) user;
	double lastTime;

	int corrx, corry, z;

	long i;

	double * ptime;  
	long * prawx;
	long * prawy;

	double detx, dety;
	double skyx, skyy;
	double randomx, randomy;
	double v, vhat[3];
	v = 0.;

	randomx = 0.;
	randomy = 0.;

	report_verbose("iterate_events_aux(totaln %ld, offset %ld, firstn %ld, nrows %ld, narrays %d, ...)\n", totaln, offset, firstn, nrows, narrays);
	ptime = (double *) fits_iter_get_array(icols + COL_TIME);
	prawx = (long *) fits_iter_get_array(icols + COL_RAWX);
	prawy = (long *) fits_iter_get_array(icols + COL_RAWY);

	lastTime = -1;

	for (i = 1; i <= nrows; ++i, lastTime = ptime[i]) {

		if (ptime[i] != lastTime) {
			findQuatInAttFile(task->attfile, task->q, ptime[i]);
			convertDetectorToSkyUsingTeldef(task->teldef, &skyx, &skyy,
					task->detx0, task->dety0, task->q, v, vhat);
		}
		else
			repeatDetectorToSkyTeldefConversion(task->teldef, &skyx, &skyy,
					task->detx0, task->dety0);

		applyXform2dToContinuousCoords(task->sky2det, &detx, &dety, skyx, skyy);

		if (task->par->randomize) {
			randomx = HDmtDrand() - 0.5;
			randomy = HDmtDrand() - 0.5;
		}

		corrx = prawx[i] + floor(detx - task->detx0 + 0.5 + randomx);
		corry = prawy[i] + floor(dety - task->dety0 + 0.5 + randomy);

		z = iimage_get_absolute(task->image, corrx, corry);
		iimage_set_absolute(task->image, corrx, corry, z+1);
	}

	return code;
}


static void
initialize_iterator_column (iteratorCol * icol, char * name, fitsfile * fptr,
							int datatype, int coltype, int * status)
{
	int tmp = fits_iter_set_by_name(icol, fptr, name, datatype, coltype);

	if (tmp) {
		if (!*status)
			*status = tmp;
		report_error("unable to initialize iterator column '%s' [%d]\n",
				name, status);
	}
}


static int
iterate_events (Task * task)
{
	int status = 0;
	double v, vhat[3];
	double skyx0, skyy0;
	iteratorCol icols[COL_COUNT] = { { 0 } };

	v = 0.;
	task->detx0 = task->teldef->det[0]->center_x;
	task->dety0 = task->teldef->det[0]->center_y;

	setSkyCoordCenterInTeldef(task->teldef, task->ranom, task->decnom);

	task->q = allocateQuat();

	findQuatInAttFile(task->attfile, task->q, task->par->trel);
	convertDetectorToSkyUsingTeldef(task->teldef, &skyx0, &skyy0,
			task->detx0, task->dety0, task->q, v, vhat);

	task->sky2det = allocateXform2d();
	invertXform2d(task->sky2det, task->teldef->det2sky);

	initialize_iterator_column(&icols[COL_TIME], "TIME",
			task->fptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[COL_RAWX], "RAWX",
			task->fptr, TLONG, InputCol, &status);
	initialize_iterator_column(&icols[COL_RAWY], "RAWY",
			task->fptr, TLONG, InputCol, &status);

	if (status)
		report_error("unable to initialize event iteration [%d]\n", status);

	fits_iterate_data(COL_COUNT, icols, 0, 0,
			iterate_events_aux, task, &status);

	if (status)
		report_error("event iteration failed [%d]\n", status);

	if (task->q)
		destroyQuat(task->q);

	return status;
}


static int run_parameters (const Parameters * p)
{
	int code = 0;
	Task task = { 0 };
	FITSHeader header = { 0 };
	IImage image = { 0 };

	task.par = p;
	task.image = &image;
	header.accept = accept_keycard_types;

	if (!code)
		headas_clobberfile((char *) p->outfile);

	HDmtInit((unsigned long) p->randomize);

	if (!code) {
		code = iimage_allocate(&image, p->dx, p->dy);
		if (code)
			report_error("unable to allocate image width=%d, height=%d\n",
					p->dx, p->dy);
		else
			iimage_set_constant(&image, 0);
		image.x0 = p->x0;
		image.y0 = p->y0;
		image.null = -1;
	}

	if (!code) {
		task.teldef = readTelDef((char *) p->teldef);
		if (!task.teldef) {
			code = READ_ERROR;
			report_error("unable to load TELDEF %s\n", p->teldef);
		}
	}

	if (!code) {
		task.attfile = openAttFile((char *) p->attfile);
		if (!task.attfile) {
			code = READ_ERROR;
			report_error("unable to open attfile %s\n", p->attfile);
		}
	}

	if (!code) {
		int status = 0;
		fits_open_file(&task.fptr, p->eventfile, READONLY, &status);
		if (status) {
			code = READ_ERROR;
			report_error("unable to open event file [%d]\n", status);
		}
	}

	if (!code) {
		fits_movnam_hdu(task.fptr, ANY_HDU, "EVENTS", 0, &code);
		if (code)
			report_error("unable to move to EVENTS extension [%d]\n", code);
	}

	if (!code) {
		int types[] = { TYP_REFSYS_KEY, TYP_USER_KEY, 0 };
		header.user = types;
		code = fetch_header_records_fits(&header, task.fptr);
		if (code)
			report_error("unable to load event file header from %s [%d]\n",
					p->eventfile, code);
	}

	if (!code) {

		if (get_header_key_double(&header, "RA_PNT", &task.ranom, 0)
				|| get_header_key_double(&header, "DEC_PNT", &task.decnom, 0)) {
			code = READ_ERROR;
			report_error("input missing RA_PNT/DEC_PNT keyword\n");
		}
	}

	if (!code)
		code = iterate_events(&task);

	if (!code) {
		ImageIO io = { 0 };
		io.header = &header;
		header.accept = 0;
		code = iimage_write(task.image, p->outfile, &io);
	}

	if (task.teldef)
		destroyTelDef(task.teldef);

	if (task.attfile)
		closeAttFile(task.attfile);

	HDmtFree();

	return code;
}


int TOOLSUB ()
{
	int code = 0;
	Parameters par = { 0 };

	set_toolname("uvotrawevtimg");
	set_toolversion("1.0");

	add_report_function(&report_stdout);

	if (!code)
		code = get_parameters(&par);

	if (!code)
		code = run_parameters(&par);

	remove_report_function(&report_stdout);

	return code;
}


