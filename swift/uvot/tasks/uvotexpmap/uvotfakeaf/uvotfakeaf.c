/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/uvotfakeaf/uvotfakeaf.c,v $
 * $Revision
 * $Date: 2007/10/10 21:24:16 $
 *
 * $Date: 2007/10/10 21:24:16 $
 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "report.h"
#include "headas.h"
#include "pil.h"
#include "fitsio.h"

#include "keyutil.h"
#include "teldef.h"
#include "attfile.h"
#include "att_iterator.h"
#include "quat.h"


#define TOOLSUB uvotfakeaf
#include "headas_main.c"



typedef struct
{
	int dummy;

	char qualfile[PIL_LINESIZE];
	char attfile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	char teldef[PIL_LINESIZE];
	double trel, t1, t2;
	double attdelta;
	double frametime;

	int chatter;
	int clobber;
	int history;
} Parameters;



typedef struct
{
	const Parameters * par;

	TELDEF * teldef;
	ATTFILE * attfile;
	ATTITERATOR * iterator;
	QUAT * q;

	double ranom, decnom;
	fitsfile * fptr;

	int count;
	int space;
	double * time;
	int * dx;
	int * dy;
	int * nframes;
} Task;



static int get_parameters (Parameters * p)
{
	int code = 0;

	if (!code)
		code = PILGetFname("badpixfile", p->qualfile);

	if (!code)
		code = PILGetFname("attfile", p->attfile);

	if (!code)
		code = PILGetFname("outfile", p->outfile);

	if (!code)
		code = PILGetFname("teldeffile", p->teldef);

	if (!code)
		code = PILGetReal("trel", &p->trel);

	if (!code)
		code = PILGetReal("t1", &p->t1);

	if (!code)
		code = PILGetReal("t2", &p->t2);

	if (!code)
		code = PILGetReal("attdelta", &p->attdelta);

	if (!code)
		code = PILGetReal("frametime", &p->frametime);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	return code;
}



static int
write_output (Task * task, FITSHeader * header)
{
	int status = 0;
	int code = 0;

	enum { COL_NULL, COL_TIME, COL_N_FRAMES, COL_D_RAWX, COL_D_RAWY };
	char * outfile = (char *) task->par->outfile;
	char * ttype[] = { "TIME", "N_FRAMES", "D_RAWX", "D_RAWY" };
	char * tform[] = { "D", "J", "J", "J" };
	char * tunit[] = { "s", "", "pixel", "pixel" };
	int N_COLUMNS = sizeof(ttype) / sizeof(ttype[0]);
	char extname[80];

	fits_create_file(&task->fptr, outfile, &status);
	if (status)
		report_error("unable to create %s [%d]\n", outfile, status);
	else
		report_status("writing %s\n", outfile);

	if (!status && fits_create_img(task->fptr, BYTE_IMG, 0, 0, &status))
		report_error("unable to create empty primary image [%d]\n", status);

#if 0
	if (!status && (code = update_header_records_fits(header, task->fptr)))
		report_warning("unable to write primary header [%d]\n", code);
#endif

	if (!status && fits_create_tbl(task->fptr, BINARY_TBL,
			task->count, N_COLUMNS, ttype, tform, tunit, "AF", &status))
		report_error("unable to create aspect following table [%d]\n", status);

	if (!status && fits_write_col_dbl(task->fptr, COL_TIME,
			1, 1, task->count, task->time, &status))
		report_error("unable to write aspect following TIME [%d]\n", status);

	if (!status && fits_write_col_int(task->fptr, COL_N_FRAMES,
			1, 1, task->count, task->nframes, &status))
		report_error("unable to write aspect following N_FRAMES [%d]\n", status);

	if (!status && fits_write_col_int(task->fptr, COL_D_RAWX,
			1, 1, task->count, task->dx, &status))
		report_error("unable to write aspect following D_RAWX [%d]\n", status);

	if (!status && fits_write_col_int(task->fptr, COL_D_RAWY,
			1, 1, task->count, task->dy, &status))
		report_error("unable to write aspect following D_RAWY [%d]\n", status);

	/* change EXTNAME from ffEXPID[IE] to AFEXPID[IE] */
	if (!status && (code = get_header_key_string(header, "EXTNAME", extname, 0)))
		report_warning("unable to get EXTNAME [%d]\n", code);
	else {
		extname[0] = 'A';
		extname[1] = 'F';
		set_header_key_string(header, "EXTNAME", extname);
		report_verbose("updating EXTNAME to %s\n", extname);
	}

	if (!status && (code = update_header_records_fits(header, task->fptr)))
		report_warning("unable to write table header [%d]\n", code);

	if (!status && fits_close_file(task->fptr, &status))
		report_error("unable to close aspect following file [%d]\n", status);

	return status;
}


static int
store_aspect_following_record (Task * task, double t,
		double dx, double dy, double * dt)
{
	int code = 0;
	int idx, idy, nframes;

	idx = floor(dx + 0.5);
	idy = floor(dy + 0.5);
	nframes = floor(*dt / task->par->frametime + 0.5);

	if (task->count == task->space) {
		task->space += 1000;
		task->time = realloc(task->time, task->space * sizeof(double));
		task->dx = realloc(task->dx, task->space * sizeof(int));
		task->dy = realloc(task->dy, task->space * sizeof(int));
		task->nframes = realloc(task->nframes, task->space * sizeof(int));
	}

	task->time[task->count] = t;
	task->dx[task->count] = idx;
	task->dy[task->count] = idy;
	task->nframes[task->count] = nframes;
	++task->count;

	*dt -= nframes * task->par->frametime;

	if (task->par->chatter > 4)
		report_verbose("AF: dx=%d dy=%d frames=%d\n", idx, idy, nframes);

	return code;
}


static int
iterate_sky_range (Task * task)
{
	int code = 0;
	double v, vhat[3];
	double skyx, skyy, skyx0, skyy0;
	double detx, dety, detx0, dety0;
	double dt, sumdt, t;
	double dx, dy, dx0, dy0, prevdx, prevdy;
	XFORM2D * sky2det;

	v = 0.;
	detx0 = task->teldef->det[0]->center_x;
	dety0 = task->teldef->det[0]->center_y;

	sumdt = 0;
	dx0 = 0;
	dy0 = 0;

	findQuatInAttFile(task->attfile, task->q, task->par->trel);
	convertDetectorToSkyUsingTeldef(task->teldef, &skyx0, &skyy0,
			detx0, dety0, task->q, v, vhat);

	sky2det = allocateXform2d();
	invertXform2d(sky2det, task->teldef->det2sky);

	setAttIteratorInterval(task->iterator, task->par->t1, task->par->t2);
	t = task->par->t1;

	for ( ; moreAttIteratorSteps(task->iterator);
			nextAttIteratorStep(task->iterator)) {

		dt = getAttIteratorValues(task->iterator, task->q);

		convertDetectorToSkyUsingTeldef(task->teldef, &skyx, &skyy,
				detx0, dety0, task->q, v, vhat);

		applyXform2dToContinuousCoords(sky2det, &detx, &dety, skyx, skyy);
		dx = detx - detx0;
		dy = dety - dety0;
		if (fabs(dx - dx0) + fabs(dy - dy0) > 0.5) {
			/* store an AF record */
			if (sumdt > 0)
				store_aspect_following_record(task, t, prevdx, prevdy, &sumdt);
			/* start collecting data at the new position */
			dx0 = dx;
			dy0 = dy;
			sumdt += dt;
		}
		else { /* include this step with the previous */
			sumdt += dt;
		}
		prevdx = dx;
		prevdy = dy;
		t += dt;
	}

	if (sumdt > 0)
		store_aspect_following_record(task, t, prevdx, prevdy, &sumdt);

	return code;
}


int
run_sky_range (Task * task)
{
	int code = 0;

	task->q = allocateQuat();

	if (!code) {
		setSkyCoordCenterInTeldef(task->teldef, task->ranom, task->decnom);
	}

	if (!code) {
		task->attfile = openAttFile((char *) task->par->attfile);
		if (!task->attfile) {
			code = READ_ERROR;
			report_error("unable to open ATTFILE %s\n", task->par->attfile);
		}
	}

	if (!code) {
		double radians = (task->par->attdelta / 3600.) * (3.14159 / 180);
		task->iterator = allocateAttIterator(task->attfile, radians);
		if (!task->iterator) {
			code = MEMORY_ALLOCATION;
			report_error("unable to create ATTITERATOR\n");
		}
	}

	if (!code)
		code = iterate_sky_range(task);

	if (task->q)
		destroyQuat(task->q);

	if (task->teldef)
		destroyTelDef(task->teldef);

	if (task->attfile)
		closeAttFile(task->attfile);

	if (task->iterator)
		destroyAttIterator(task->iterator);

	return code;
}


static int run_parameters (const Parameters * p)
{
	int code = 0;
	Task task = { 0 };
	FITSHeader header = { 0 };

	task.par = p;
	header.accept = accept_nonstructural_records;

	if (!code)
		headas_clobberfile((char *) p->outfile);

	if (!code) {
		task.teldef = readTelDef((char *) p->teldef);
		if (!task.teldef) {
			code = READ_ERROR;
			report_error("unable to load TELDEF %s\n", p->teldef);
		}
	}

	if (!code) {
		code = fetch_header_records_path(&header, p->qualfile);
		if (code) {
			code = READ_ERROR;
			report_error("unable to load quality header from %s [%d]\n",
					p->qualfile, code);
		}
	}

	if (!code) {

		if (get_header_key_double(&header, "RA_PNT", &task.ranom, 0)
				|| get_header_key_double(&header, "DEC_PNT", &task.decnom, 0)) {
			code = 1;
			report_error("input missing RA_PNT/DEC_PNT keyword\n");
		}
	}

	if (!code) {
		task.count = 0;
		task.space = 1000;
		task.time = malloc(task.space * sizeof(double));
		task.dx = malloc(task.space * sizeof(int));
		task.dy = malloc(task.space * sizeof(int));
		task.nframes = malloc(task.space * sizeof(int));
	}

	if (!code)
		code = run_sky_range(&task);

	if (!code)
		code = write_output(&task, &header);

	return code;
}


int TOOLSUB ()
{
	int code = 0;
	Parameters par = { 0 };

	set_toolname("uvotatt2af");
	set_toolversion("1.0");

	add_report_function(&report_stdout);

	if (!code)
		code = get_parameters(&par);

	if (!code)
		code = run_parameters(&par);

	remove_report_function(&report_stdout);

	return code;
}


