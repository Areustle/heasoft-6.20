
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "instmap.h"
#include "genimage.h"
#include "report.h"
#include "headas.h"
#include "pil.h"
#include "fitsio.h"
#include "comboxform.h"

#include "teldef.h"
#include "attfile.h"
#include "att_iterator.h"
#include "quat.h"


#define TOOLSUB uvotinstmap
#include "headas_main.c"



enum
{
	MAP_CONST,
	MAP_MASK,
	MAP_SHIFTADD,
	NMETHODS
};


struct Parameters
{
	int dummy;

	char infile[PIL_LINESIZE];
	char qualfile[PIL_LINESIZE];
	char transform[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	int method;
	int nparts;     /* how many parts to divide each side into */
	int npixels;    /* how many pixels per part */
	int masksize;
	int masktrim;

	char teldef[PIL_LINESIZE];
	char attfile[PIL_LINESIZE];

	double ranom, decnom;

	double trel, t1, t2;
	double attdelta;

	int chatter;
	int clobber;
	int history;

};



typedef struct
{
	TELDEF * teldef;
	ATTFILE * attfile;
	ATTITERATOR * iterator;
	QUAT * q;

	int dxmin, dxmax;
	int dymin, dymax;

	double ranom, decnom;

	double scale;

} SkyRangeTask;



static int get_parameters (Parameters * p)
{
	int code = 0;
	char method[PIL_LINESIZE];

	if (!code)
		code = PILGetFname("infile", p->infile);

	if (!code)
		code = PILGetFname("badpixfile", p->qualfile);

	if (!code)
		code = PILGetFname("transform", p->transform);

	if (!code)
		code = PILGetFname("outfile", p->outfile);

	if (!code)
		code = PILGetString("method", method);

	if (!code) {
		if (!strcasecmp(method, "CONST"))
			p->method = MAP_CONST;
		else if (!strcasecmp(method, "MASK"))
			p->method = MAP_MASK;
		else {
			code = 1;
			report_error("invalid method '%s'\n", method);
		}
	}

	if (!code)
		code = PILGetReal("ra", &p->ranom);

	if (!code)
		code = PILGetReal("dec", &p->decnom);

	if (p->method == MAP_MASK) {
		if (!code)
			code = PILGetFname("teldef", p->teldef);

		if (!code)
			code = PILGetFname("attfile", p->attfile);

		if (!code)
			code = PILGetReal("trel", &p->trel);

		if (!code)
			code = PILGetReal("t1", &p->t1);

		if (!code)
			code = PILGetReal("t2", &p->t2);

		if (!code)
			code = PILGetReal("attdelta", &p->attdelta);

		if (!code)
			code = PILGetInt("masksize", &p->masksize);

		if (!code)
			code = PILGetInt("masktrim", &p->masktrim);
	}

	if (!code)
		code = PILGetInt("npixels", &p->npixels);

	if (!code)
		code = PILGetInt("nparts", &p->nparts);

	if (!code)
		code = PILGetBool("clobber", &p->clobber);

	p->chatter = headas_chatpar;
	get_history(&p->history);

	return code;
}



int setup_output (Task * task, char * path, FITSHeader * header)
{
	fitsfile * fptr = 0;
	int status = 0;
	int bitpix, naxis;
	long naxes[2];

	if (fits_open_file(&fptr, path, READONLY, &status))
		report_error("unable to open %s [%d]\n", status);

	else if (fits_get_img_param(fptr, 2, &bitpix, &naxis, naxes, &status))
		report_error("unable to get %s image info [%d]\n", status);

	else if (naxis != 2)
		report_error("input %s is not a 2d image\n");

	else {
		int width = (int) naxes[0];
		int height = (int) naxes[1];

		report_status("input image is %d by %d\n", width, height);

		status = fimage_allocate(task->map, width, height);
		fimage_set_constant(task->map, 0);

		if (task->par->method == MAP_MASK) {
			status = simage_allocate(task->mask, width, height);
			simage_set_constant(task->mask, 0);
		}
	}

	if (!status) {
		header->accept = accept_nonstructural_records;
		status = fetch_header_records_fits(header, fptr);
		header->accept = 0;
	}

	if (fptr) {
		int tmp = 0;
		fits_close_file(fptr, &tmp);
	}

	return status;
}


int load_quality_map (SImage * quality, const char * path)
{
	int code = 0;
	ImageIO io = { 0 };

	quality->null = -1;

	if (!code)
		code = simage_read(quality, path, &io);

	if (code)
		report_error("unable to load quality file '%s' [%d]\n", path, code);

	return code;
}



int load_transform (Task * task, char * path)
{
	int code = 0;

	task->xform = readComboXform(path);

	if (!task->xform) {
		code = 1;
		report_error("unable to load transform from '%s'\n", path);
	}
	else {
	/*************************************************************************
	* from the user's point of view, the transforms follow the FITS convention
	* of having the center of the first pixels be (1,1). However internally
	* we put the center of the first pixel at (0,0). This is C after all.
	* so we need to modify the transform to follow the internal convention
	*************************************************************************/
		XFORM2D * temp = allocateXform2d();

		setXform2dToTranslation(temp, 1.0, 1.0); /* from C to FITS */
		applyXform2dBeforeComboXform(temp, task->xform);

		setXform2dToTranslation(temp, -1.0, -1.0); /* from FITS back to C */
		applyXform2dAfterComboXform(task->xform, temp);
		destroyXform2d(temp);
	}

	return code;
}


int
find_sky_range (SkyRangeTask * task, const Parameters * par)
{
	int code = 0;
	double v, vhat[3];
	double skyx, skyy, skyx0, skyy0;
	double detx, dety;
	double dx, dy;
	double dxmin, dxmax, dymin, dymax;

	v = 0.;
	detx = task->teldef->det[0]->center_x;
	dety = task->teldef->det[0]->center_y;
	dxmin = 0.;
	dxmax = 0.;
	dymin = 0.;
	dymax = 0.;

	findQuatInAttFile(task->attfile, task->q, par->trel);
	convertDetectorToSkyUsingTeldef(task->teldef, &skyx0, &skyy0,
			detx, dety, task->q, v, vhat);

	setAttIteratorInterval(task->iterator, par->t1, par->t2);
	setAttFileInterpolation(task->iterator->att, ATTFILE_LINEAR_NEARBY);
	requireTimeNearAttFileRecord(task->iterator->att, 32.0);

	for ( ; moreAttIteratorSteps(task->iterator);
			nextAttIteratorStep(task->iterator)) {

		getAttIteratorValues(task->iterator, task->q);

		convertDetectorToSkyUsingTeldef(task->teldef, &skyx, &skyy,
				detx, dety, task->q, v, vhat);

		dx = skyx - skyx0;
		dy = skyy - skyy0;

		if (dx < dxmin) dxmin = dx;
		if (dx > dxmax) dxmax = dx;
		if (dy < dymin) dymin = dy;
		if (dy > dymax) dymax = dy;
	}

	report_status("range: dxmin=%.3f dxmax=%.3f dymin=%.3f dymax=%.3f\n",
			dxmin, dxmax, dymin, dymax);

	task->dxmin = (int) floor(dxmin - 0.5);
	task->dxmax = (int) ceil(dxmax + 0.5);
	task->dymin = (int) floor(dymin - 0.5);
	task->dymax = (int) ceil(dymax + 0.5);
	report_status("range: dxmin=%d dxmax=%d dymin=%d dymax=%d\n",
			task->dxmin, task->dxmax, task->dymin, task->dymax);

	return code;
}


int
run_sky_range (SkyRangeTask * task, Parameters * par)
{
	int code = 0;

	task->q = allocateQuat();

	if (!code) {
		task->teldef = readTelDef(par->teldef);
		if (!task->teldef) {
			code = READ_ERROR;
			report_error("unable to load TELDEF %s\n", par->teldef);
		}
	}

	if (!code) {
		task->scale = task->teldef->det[0]->scale_x;
		setSkyCoordCenterInTeldef(task->teldef, task->ranom, task->decnom);
	}

	if (!code) {
		task->attfile = openAttFile(par->attfile);
		if (!task->attfile) {
			code = READ_ERROR;
			report_error("unable to open ATTFILE %s\n", par->attfile);
		}
	}

	if (!code) {
		double radians = (par->attdelta / 3600.) * (3.14159 / 180);
		task->iterator = allocateAttIterator(task->attfile, radians);
		if (!task->iterator) {
			code = MEMORY_ALLOCATION;
			report_error("unable to create ATTITERATOR\n");
		}
	}

	if (!code) {
		if (par->masktrim < 0)
			code = find_sky_range(task, par);
		else {
			task->dxmin = task->dymin = -par->masktrim;
			task->dxmax = task->dymax = par->masktrim;
		}
	}

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



static int run_parameters (Parameters * p)
{
	int code = 0;
	Task task = { 0 };

	FImage map = { 0 };
	SImage mask = { 0 };
	SImage quality = { 0 };
	ImageIO io = { 0 };
	FITSHeader header = { 0 };

	task.par = p;
	io.header = &header;

	task.map = &map;
	task.quality = &quality;

	if (p->method == MAP_MASK) {
		mask.null = -999;
		task.mask = &mask;
	}

	if (!code)
		code = setup_output(&task, p->infile, &header);

	if (!code)
		code = get_header_key_double(&header, "EXPOSURE", &task.exposure, 0);

	if (!code && p->method == MAP_MASK) {
		SkyRangeTask subtask = { 0 };
		double cdelt1d;
		int keyerr;

		subtask.ranom = p->ranom;
		subtask.decnom = p->decnom;

		if (!code) {
			code = run_sky_range(&subtask, p);
			task.dxmin = subtask.dxmin;
			task.dxmax = subtask.dxmax;
			task.dymin = subtask.dymin;
			task.dymax = subtask.dymax;
		}

		header.warnings = 10;

		keyerr = get_header_key_double(&header, "CRPIX1D", &task.crpix1d, 0);

		keyerr |= get_header_key_double(&header, "CRPIX2D", &task.crpix2d, 0);

		cdelt1d = 0.009075;
		get_header_key_double(&header, "CDELT1D", &cdelt1d, 0);

		task.maskRadius = keyerr ? -1 : p->masksize * subtask.scale / cdelt1d;
		if (task.maskRadius > 0)
			report_verbose("internal mask radius %.1f\n", task.maskRadius);
		else
			report_verbose("will not apply mask radius\n");
	}

	if (!code)
		code = load_quality_map(&quality, p->qualfile);

	if (!code) {
		if (p->npixels < 1)
			task.nx = task.ny = p->nparts;
		else {
			task.nx = 1 + (quality.width - 1) / p->npixels;
			task.ny = 1 + (quality.height - 1) / p->npixels;
		}
		if (task.nx < 1 || task.ny < 1) {
			code = READ_ERROR;
			report_error("invalid input nparts/npixels\n");
		}
	}

	if (!code)
		code = load_transform(&task, p->transform);

	if (!code)
		code = solve(&task);

	if (!code) {
		if (p->method == MAP_MASK)
			code = simage_write(&mask, p->outfile, &io);
		else
			code = fimage_write(&map, p->outfile, &io);
	}

	fimage_release(&map);
	simage_release(&mask);
	simage_release(&quality);

	return code;
}




int uvotinstmap ()
{
	int code = 0;
	Parameters par = { 0 };

	set_toolname("uvotinstmap");
	set_toolversion("2.0");

	add_report_function(&report_stdout);

	if (!code)
		code = get_parameters(&par);

	if (!code)
		code = run_parameters(&par);

	remove_report_function(&report_stdout);

	return code;
}


