/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotflagqual/uvotflagqual.c,v $
 * $Revision: 1.8 $
 * $Date: 2010/06/21 20:27:13 $
 *
 * $Log: uvotflagqual.c,v $
 * Revision 1.8  2010/06/21 20:27:13  rwiegand
 * Updated with new version from Vladimir: Source detection algorithm is improved
 * (splitting overlapping sources).
 *
 * Revision 1.7  2010/04/28 21:51:06  rwiegand
 * Vladimir provided a new version which operates on each extension and updates
 * quality flagging (halo rings, filter dependence).
 *
 * Revision 1.6 2010/04/20 18:40:00 yershov:
 * The task is reformatted to make it working with multiple-extension files;
 * The name of the input parameter "rawfile" is changed to "infile";
 * The polynomial coefficients for calculating the positions of halo-ring
 * scattered-light features are introduced for all of the UVOT filters.
 +
 + Revision 1.5 2010/03/21 yershov: (flagqual2.c) isolated single pixels
 + are removed from the source map used for source detection, which is
 + needed for dealing with short-exposures made with the UVW2-filter.
 +
 + Revision 1.4  2009/12/21 yershov:
 + The task is now detecting bright sources by itself and does not require
 + the input source list. So, the corresponding input parameter is removed.
 +
 * Revision 1.3  2008/06/12 18:09:22  wiegand
 * Added assorted parameters to assist in testing.  Use CXX, CYY, CXY ellipse
 * for extended sources.  Avoid copying structural keywords to output.  Process
 * raw image with floating point.  Ensure that input image is RAW.  Write
 * quality flag definitions to output as COMMENTs.  Write flag summary.
 *
 * Revision 1.2  2008/06/11 14:29:00  wiegand
 * Implemented quality flag image.
 *
 * Revision 1.1  2008/06/10 20:24:03  wiegand
 * Initial revision
 *
 */

#include <math.h>
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "fitsio.h"

#include "report.h"
#include "uvottool.h"
#include "uvotfile.h"
#include "uvotquality.h"
#include "uvotimage.h"
#include "caldbquery.h"
#include "keyutil.h"
#include "flagqual.h"


#define TOOLSUB uvotflagqual
#define VERSION 2.1
#define CREATOR _STRINGIFY(TOOLSUB) " " _STRINGIFY(VERSION)
#define ORIGIN "GSFC"
#include "headas_main.c"

#define PAR_INFILE       "infile"

#define PAR_BADPIXFILE   "badpixfile"
#define PAR_OUTFILE      "outfile"
#define PAR_SMAPBASE     "smapbase"
#define PAR_QMAPBASE     "qmapbase"

#define PAR_PSF_FWHM     "psffwhm"
#define PAR_APERTURE     "aperture"
#define PAR_SOURCE_TYPE_SCALE  "pntext"
#define PAR_ELLIPSE_SCALE  "extscale"
#define PAR_FLAG_DETECTIONS    "flagdet"


typedef IImage RawImage;


struct Parameters
{
	int chatter;
	int clobber;
	int history;
	char infile[PIL_LINESIZE];
	char badpixfile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];
	char smapfile[PIL_LINESIZE];
	char qmapfile[PIL_LINESIZE];

	double psf_fwhm_arcsec;
	double aperture_arcsec;

	double pointExtendedScale;
	double ellipseScale;

	int flagDetections;
};


int applyBadPixList(Task * task)
{
	int code = 0;
	int count, i, j;

	ImageBadPixel * bpImage;

	/*BadPixelList bpRaw = { 0 };
	 task->badPixList = &bpRaw;*/

	/*code = load_bad_pixel_list(&bpRaw, path);*/

	if (!code) {
		/* prepare flattened list */

		count = 0;
		for (i = 0; i < task->badPixList->count; ++i) {
			count += task->badPixList->yLength[i];
		}
		report_verbose("bad pixel count=%d \n", count);

		if (task->badPixel == NULL) {
			task->badPixel = calloc(count, sizeof(ImageBadPixel));
		}
		else {
			task->badPixel = realloc(task->badPixel, count
					* sizeof(ImageBadPixel));
		}

		task->nBadPixels = 0;
		for (i = 0; i < task->badPixList->count; ++i) {
			for (j = 0; j < task->badPixList->yLength[i]; ++j) {
				bpImage = &task->badPixel[task->nBadPixels++];
				convert_raw_to_image(task, task->badPixList->x[i],
						task->badPixList->yTop[i] - j, &bpImage->xImage,
						&bpImage->yImage);
			}
		}
	}
	else
		report_error("unable to load bad pixel list\n");
	return code;
} /* applyBadPixList */


#define toRadians(degrees) (degrees*M_PI/180.0)

/* see SExtractor v 2.5 manual, section 9.1.6 */

/*
static void initialize_ellipse (Source * s, double sizeScale)
{
	double aa, bb, costheta, sintheta;
	Ellipse * e =  &s->ellipse_imagePixels;

	aa = s->majorAxis_imagePixels * s->majorAxis_imagePixels;
	bb = s->minorAxis_imagePixels * s->minorAxis_imagePixels;

	costheta = cos(toRadians(s->thetaAxis_degrees));
	sintheta = sin(toRadians(s->thetaAxis_degrees));

	e->xbar = s->xImage;
	e->ybar = s->yImage;
	e->cxx = costheta * costheta / aa + sintheta * sintheta / bb;
	e->cyy = sintheta * sintheta / aa + costheta * costheta / bb;
	e->cxy = 2 * costheta * sintheta * (1 / aa - 1 / bb);
	e->rr = 3 * 3 * sizeScale * sizeScale;
}
*/


int determineConstants(Task * task)
{
	int code = 0;
	long binx;
	Constants * k = (Constants *) task->constants;

	if (task->par->psf_fwhm_arcsec > 0)
		k->psf_fwhm_arcsec = task->par->psf_fwhm_arcsec;
	else
		k->psf_fwhm_arcsec = 2;

	if (task->par->aperture_arcsec > 0)
		k->aperture_arcsec = task->par->aperture_arcsec;
	else
		k->aperture_arcsec = 5;
	
	/*binx = task->binx;*/
	binx=1;

	/*fits_read_key_lng(task->fptr, "BINX", &binx, 0, pstatus);*/
	if (!code) {
		k->arcsec_per_imagePixel = 0.5 * binx;
		k->psf_fwhm_imagePixels = k->psf_fwhm_arcsec / k->arcsec_per_imagePixel;
		k->aperture_imagePixels = k->aperture_arcsec / k->arcsec_per_imagePixel;
		report_verbose("binning = %d\n", (int) binx);
		report_verbose("pixel = %.2f [arcsec]\n", k->arcsec_per_imagePixel);
		report_verbose("aperture = %.2f [arcsec]\n", k->aperture_arcsec);
		report_verbose("aperture = %.2f [pixel]\n", k->aperture_imagePixels);
		report_verbose("psf fwhm = %.2f [arcsec]\n", k->psf_fwhm_arcsec);
		report_verbose("psf fwhm = %.2f [pixel]\n", k->psf_fwhm_imagePixels);
	}
	else {
		report_error("unable to determine constants [%d]\n", code);
	}
	return code;
} /* determine constants*/


static int getFilterCode(const char *s)
{
	FilterCode filter = FILTER_UNKNOWN;

	if (strcmp(s, "U") == 0)
		filter = FILTER_U;
	else if (strcmp(s, "B") == 0)
		filter = FILTER_B;
	else if (strcmp(s, "V") == 0)
		filter = FILTER_V;
	else if (strcmp(s, "WHITE") == 0)
		filter = FILTER_WHITE;
	else if (strcmp(s, "UVW1") == 0)
		filter = FILTER_UVW1;
	else if (strcmp(s, "UVW2") == 0)
		filter = FILTER_UVW2;
	else if (strcmp(s, "UVM2") == 0)
		filter = FILTER_UVM2;

	return filter;
}


static int
writeFimage (const char *path, FImage * image)
{
	int code = 0;
	ImageIO io = { 0 };
	code = fimage_write(image, path, &io);
	return code;
} /* writeFimage */


int create_one_quality_map(Task * task)
{
	int code = 0;
	int status = 0;

	RawImage raw = { 0 };
	/*QualityImage quality = { 0 };*/
	FImage extendedImage = { 0 };
	ImageIO io = { 0 };

	FITSHeader header = { 0 };
	RawWindow window = { 0 };

	FImage rawImage = { 0 };
	IImage flagImage = { 0 }; /* This is for the main quality map */

	/*----------------*/
	FImage smapImage = { 0 };
	FImage vstreakImage = { 0 };
	IImage qmapImage = { 0 }; /* This is for the additional (strong) quality map */
	/*----------------*/

	int hdu = 0;
	double met = 1e20;
	char extname[FLEN_VALUE];
	char keyspace[FLEN_KEYWORD];
	int scheme = 0;
	double overflow;
	double underflow;

	
	task->rawImage = &rawImage;
	rawImage.null = -1;
	flagImage.null = -1;

	/*------------------*/
	smapImage.null = -1;
	vstreakImage.null = -1;
	qmapImage.null = -1;
	extendedImage.null = -1;
	/*------------------*/

	report_verbose("reading the hdu keywords  ... \n");
	if (!code) {
	  /*report_verbose("----------------------------\n");*/
		fits_get_hdu_num(task->fptr, &hdu);
		report_verbose(" hdu number = %d \n", hdu);
	        /*report_verbose("----------------------------\n");*/
		if (hdu > 1) {
			int tmp = 0;
			fits_write_errmark();
			fits_read_key_str(task->fptr, "EXTNAME", extname, 0, &tmp);
			if (tmp) {
				tmp = 0;
				fits_read_key_str(task->fptr, "HDUNAME", extname, 0, &tmp);
			}
			if (tmp)
				strcpy(extname, "UNKNOWN");
			fits_clear_errmark();
		}
		else
			strcpy(extname, "PRIMARY");
		report_status("building %s quality map\n", extname);
	}
	if (!code) {
		char ctype[FLEN_VALUE];
		fits_read_key_str(task->fptr, "CTYPE1", ctype, NULL, &status);
		if (status)
			report_warning("unable to read CTYPE1 keyword\n");
		else if (!strcasecmp(ctype, "RA---TAN")) {
			window.ignoreWCS = 1;
			window.writeWCS = 1;
			report_verbose("CTYPE indicates SKY image, will write RAW WCS\n");
		}
		status = 0;
	}

	/* Read the FILTER keyword */
	task->filter = FILTER_UNKNOWN;
	strcpy(task->filterString, "UNKNOWN");
	if (!code) {
		fits_read_key_str(task->fptr, "FILTER", keyspace, NULL, &status);
		if (status)
			report_warning("unable to read FILTER keyword\n");
		else {
			task->filter = getFilterCode(keyspace);
			strcpy(task->filterString, keyspace);
			report_verbose("FILTER=%s [%d]\n", keyspace, task->filter);
			status = 0;
		}
	}

	/* Read the EXPOSURE keyword */
	if (!code) {
		fits_read_key_dbl(task->fptr, "EXPOSURE", &task->exposure, NULL,
				&status);
		if (status)
			report_warning("unable to read EXPOSURE keyword\n");
		else {
			report_verbose("EXPOSURE=%.3f\n", task->exposure);
			status = 0;
		}
	}

	if (!code) {
		code = load_raw_window(task->fptr, &window);
		if (code) {
			code = TASK_INPUT_ERROR;
			report_error("unable to load %s raw window\n", extname);
		}
		else {
			raw.x0 = window.x0;
			raw.y0 = window.y0;

			task->x0 = window.x0;
			task->y0 = window.y0;
			task->dx = window.dx;
			task->dy = window.dy;
			task->binx = window.binx;
			task->biny = window.biny;

			rawImage.x0 = window.x0;
			rawImage.y0 = window.y0;

			report_verbose("raw window is loaded x0=%d y0=%d dx=%d dy=%d \n", 
					window.x0, window.y0, window.dx, window.dy);
		}
	}

	if (!code) {
		report_verbose("applying the bad pixels list ... \n");
		code = applyBadPixList(task);
	}

	if (!code) {

		if (window.ignoreWCS) {
			/* working from a non-RAW image, just make a flat image */
			int width = window.dx / window.binx;
			int height = window.dy / window.biny;
			report_verbose("window width= %d, height= %d \n", width, height);

			code = iimage_allocate(&raw, width, height);
			iimage_set_constant(&raw, 0);

			/* Allocate also the floating point image */
			if (!code) {
				code = fimage_allocate(&rawImage, width, height);
				fimage_set_constant(&rawImage, 0.0);
				report_verbose("allocated memory for the raw image \n");
			}
		}
		else {
			raw.null = -1;
			code = iimage_read_chdu(&raw, task->fptr, &io);

			/* Read also the floating point image */
			if (!code) {
				rawImage.null = -1;
				code = fimage_read_chdu(&rawImage, task->fptr, &io);
				report_verbose("raw image is loaded \n");
			}
		}
		if (code) {

			code = TASK_INPUT_ERROR;
			report_error("unable to initialize %s RAW image\n", extname);
		}
	}

	if (!code) {
		if (window.ignoreWCS) {

			int types[] = { TYP_HDUID_KEY, TYP_COMM_KEY, TYP_CONT_KEY,
					TYP_USER_KEY, TYP_REFSYS_KEY, 0 };

			header.user = types;
			header.accept = accept_keycard_types;
			fetch_header_records_fits(&header, task->fptr);
		}
		else {

			header.accept = accept_nonstructural_records;
			code = fetch_header_records_fits(&header, task->fptr);
		}

		if (code) {

			code = TASK_INPUT_ERROR;
			report_error("unable to load %s header\n", extname);
		}
		header.accept = 0;
		header.user = 0;
	}
	/*
	 * grab the special keywords (these are in io.header)
	 */
	if (!code && !window.ignoreWCS) {

		fits_read_key_log(task->fptr, "CMPCNTMN", &scheme, NULL, &status);
		fits_read_key_dbl(task->fptr, "CMPOTHRS", &overflow, NULL, &status);
		fits_read_key_dbl(task->fptr, "CMPUTHRS", &underflow, NULL, &status);
		if (status) {
			scheme = 0;
			report_warning("unable to read %s compression keywords\n"
				"\t=> assuming no compression damage\n", extname);
		}
		status = 0;
	}

	if (!code) {

		met = 1e12;
		fits_read_key_dbl(task->fptr, "TSTOP", &met, NULL, &status);
		if (status)
			report_warning("unable to read TSTOP keyword\n"
				"\t=> using TSTOP %e\n", met);
		status = 0;
	}

	/*
	if (!code) {

		quality.null = -1;
		quality.x0 = raw.x0;
		quality.y0 = raw.y0;
		code = simage_allocate(&quality, raw.width, raw.height);
		if (code) {
			code = TASK_INPUT_ERROR;
			report_error("unable to allocate quality map\n");
		}
		else {
			report_verbose("setting the quality image to zero \n");
			simage_set_constant(&quality, 0);
		}
	}
	*/

	if (!code) {
		report_verbose("allocating space for the extended source image \n");

		extendedImage.x0 = raw.x0;
		extendedImage.y0 = raw.y0;
		extendedImage.binx = window.binx;
		extendedImage.biny = window.biny;

		task->extendedImage = &extendedImage;

		code = fimage_allocate(&extendedImage, raw.width, raw.height);
		if (code) {
			code = TASK_INPUT_ERROR;
			report_error("unable to allocate extended source image \n");
		}
		else {
			report_verbose("setting the extended source  image to zero \n");
			fimage_set_constant(&extendedImage, 0);
		}
	}

	/*********************************************
	 *  Allocate the quality map image (flagImage)
	 *********************************************/
	if (!code) {
		if (strcasecmp(task->par->outfile, "NONE")) {
			task->flagImage = &flagImage;
			code = iimage_allocate(&flagImage, rawImage.width, rawImage.height);

			if (code)
				report_error("unable to allocate flag image [%d]\n", code);
			else {
				report_verbose("flag image is allocated\n");
			}
		}
		else
			report_verbose("no flag image requested\n");
	}
	/*printf("L513 code=%d  hdu=%d \n", code, hdu); */
	if (!code && task->flagImage){
		code = iimage_set_constant(&flagImage, 0);
		report_verbose("flag image is set to zero \n");
	}


	/*------------------------------------------*/
	/*
	 printf("L687 dumping the image .......\n");
	 {
	 code = writeFimage("raw_image2map.fits", &rawImage);
	 printf("L696 ......... rawImage .\n");
	 }
	*/

	/*---------------------------------------------------------------*/
	/* The code below is to allocate space for the readour-streak image,
	   and also for detecting bright sources
	   in order to get a their coordinates and use them  for 
	   flagging purposes */
	/*---------------------------------------------------------------*/
	report_verbose("analysing the input image  ... \n");

	
	if (!code) {
		report_verbose("allocating space for the vertical readout streak image \n");
		task->vstreakImage = &vstreakImage;

		/* Create an image for storing the highlighted readout streaks  */
		code = fimage_allocate(&vstreakImage, rawImage.width, rawImage.height);
		if (code)
			report_error("Unable to allocate the vertical streak image [%d]\n",
					code);
		else {
			report_verbose("allocated vertical streak image\n");
		}
	}

	if (!code && task->vstreakImage) {
		report_verbose("setting the vertical streak image to zero \n");
		code = fimage_set_constant(&vstreakImage, 0);
	}


	/***********************************************************/
	/* Allocate smapImage for storing the map of bright sources*/
	/***********************************************************/

	if (!code) {
		report_verbose("allocating the source map image \n");
		task->smapImage = &smapImage;

		/* Create an image for storing the bright-sources map */
		code = fimage_allocate(&smapImage, rawImage.width, rawImage.height);

		if (code)
			report_error("Unable to allocate source map image; code=[%d]\n",
					code);
		else {
			report_verbose("allocated source map image\n");
		}
	}

	if (!code && task->smapImage) {
		report_verbose("setting source map image to zero \n");
		code = fimage_set_constant(&smapImage, 0);
	}

	/**********************************************************************/
	/* Allocate qmapImage for the additional quality map (strong features)*/
	/**********************************************************************/

	if (!code) {
		report_verbose("allocating the quality map image (for strong scattered-light features) \n");
		task->qmapImage = &qmapImage;

		/* Create an image for storing the additional quality  map */
		code = iimage_allocate(&qmapImage, rawImage.width, rawImage.height);

		if (code)
			report_error("Unable to allocate the additional quality  map image; code=[%d]\n",
					code);
		else {
			report_verbose("allocated additional quality map image\n");
		}
	}

	if (!code && task->qmapImage) {
		report_verbose("setting additional quality map image to zero \n");
		code = iimage_set_constant(&qmapImage, 0);
	}


	/**************************************************************************/
	/* The function checkBrightSources will create the list of bright sources */
	/***************************************************************************/
	if (!code) {
		report_verbose("checking for the presence of bright sources ... \n");
		code = checkBrightSources(task, hdu);
	}



	if (!code)
		code = flagAllQuality(task);

	report_verbose("finished creating the quality flag map \n");



	/***********************************************************************************/
	/*  If the output of the source map image was requested then append it to the file */
	/***********************************************************************************/
	if (!code && task->smapImage && hdu > 0
			&& strcasecmp(task->par->smapfile, "NONE")) {
	 
	  report_verbose("output of the source map was requested \n"); 
	  io.header = &header;
	  io.checksum = 1;

	  report_verbose("appending the source map to the smap file \n");
	  code = fimage_append(&smapImage, task->smapfptr, &io);

	  if (code)
	    report_error("unable to write source map extension\n");
	}


	/**********************************/
	/* Saving the quality map image   */
	/**********************************/
	if (!code) {
		io.header = &header;
		io.checksum = 1;

		report_verbose("appending the flag map to the output file \n");
		code = iimage_append(task->flagImage, task->ofptr, &io);

		if (code)
			report_error("unable to write quality map extension\n");
	}


	if (!code) {
		code = save_raw_window(task->ofptr, &window);
		report_verbose("saving the main quality map window \n");
	}
	if (!code && (code = fits_write_chksum(task->ofptr, &status) != 0))
		report_warning("unable to write checksum for HDU %s\n", extname);


	/*****************************************************************************************/
	/*  If the output of the additional quality map was requested then append it to the file */
	/*****************************************************************************************/
	if (!code && task->qmapImage && hdu > 0
			&& strcasecmp(task->par->qmapfile, "NONE")) {
	 
	  report_verbose("output of the source map was requested \n"); 
	  io.header = &header;
	  io.checksum = 1;

	  report_verbose("appending the additional quality map to the qmap-2 file \n");
	  code = iimage_append(&qmapImage, task->qmapfptr, &io);

	  if (code)
	    report_error("unable to write additional quality map extension\n");
	}


	report_verbose("releasing the memory \n");
	iimage_release(&raw);
	/*simage_release(&quality);*/
	fimage_release(&extendedImage);

	fimage_release(&smapImage);
	iimage_release(&qmapImage);
	iimage_release(&flagImage);
	fimage_release(&vstreakImage);

	return code;
} /* create_one_quality_map */


int create_quality_maps(Task * task, FITSHeader * header)
{
	int code = 0;
	int status = 0;
	ImageIO io = { 0 };
	/*const char * path = task->par->infile;*/
	char buffer[FLEN_CARD];
	fitsfile * fptr = 0;
	FileSpec spec = { 0 };

	BadPixelList bpRaw = { 0 };
	int smapFlag=0;
	int qmapFlag=0;

	task->badPixList = &bpRaw;

	io.header = header;
	header->accept = accept_nonstructural_records;

	sprintf(buffer, "%s", task->par->infile);

	if (fits_open_file(&fptr, buffer, READONLY, &status)) {
		code = TASK_INPUT_ERROR;
		report_error("unable to open %s [%d]\n", buffer, status);
	}
	else {
		task->fptr = fptr;
		report_verbose("opened %s\n", buffer);
	}

	/*.................. */
	if (!code) {

		if (file_parse_path(task->par->infile, &spec)) {

			code = TASK_INPUT_ERROR;
			report_error("invalid input '%s'\n", task->par->infile);
		}
		else {
			if (spec.hdu1) {
				if (file_resolve_hdu(&spec, task->fptr)) {
					code = TASK_INPUT_ERROR;
					report_error("invalid input HDU '%s'\n", task->par->infile);
				}
				else {
					report_verbose("user specified extension '%s' => %d\n",
							spec.ext, spec.hdu1);
				}
			}
		}
	}

	/*
	 * Determine some instrument constants
	 */
	if (!code) {
		report_verbose("determining some instrument constants\n");
		code = determineConstants(task);
	}

	/*
	 * Create the output file
	 */
	if (!code) {
		char * out = (char *) task->par->outfile;
		report_verbose("the output file will be: %s\n", out);
		if (fits_create_file(&task->ofptr, out, &status)) {
			code = TASK_OUTPUT_ERROR;
			report_error("unable to create %s [%d]\n", out, status);
		}
		else {
			report_verbose("created %s\n", out);
		}
	}

	/* Source map file */
	smapFlag=0;
	/*printf("L701 smapfile= %s \n", task->par->smapfile);*/
	/* Check whether the output of the source map image is requested */
	if (!code && strcasecmp(task->par->smapfile, "NONE")){
	  char * smapout = (char *) task->par->smapfile;
	  report_verbose("the output of the source map image is requested \n");
          report_verbose("the output smap file will be: %s\n", smapout);
	  if (fits_create_file(&task->smapfptr, smapout, &status)) {
	    code = TASK_OUTPUT_ERROR;
	    report_error("unable to create %s [%d]\n", smapout, status);
	  }
	  else {
	    smapFlag=1;
	    report_verbose("created %s\n", smapout);
	  }
	  /*printf("--------------------------------------------------------\n");*/

	} 

	/* Additional quality map file */
	qmapFlag=0;
	/*report_verbose("additional quality map file= %s \n", task->par->qmapfile);*/
	/* Check whether the output of the additional quality map image is requested */
	if (!code && strcasecmp(task->par->qmapfile, "NONE")){
	  char * qmapout = (char *) task->par->qmapfile;
	  report_verbose("the output of the additional quality map image is requested \n");
          report_verbose("the output of the additional (strong) quality map file will be: %s\n", qmapout);
	  if (fits_create_file(&task->qmapfptr, qmapout, &status)) {
	    code = TASK_OUTPUT_ERROR;
	    report_error("unable to create %s [%d]\n", qmapout, status);
	  }
	  else {
	    qmapFlag=1;
	    report_verbose("created %s\n", qmapout);
	  }
	  /*printf("--------------------------------------------------------\n");*/

	} 


	/*
	 * load the bad pixel list
	 */

	if (!code) {
		char path[PIL_LINESIZE];

		if (!strncasecmp(task->par->badpixfile, "CALDB", 5)) {

			CALDBQuery query = { 0 };
			strcpy(query.codename, "BADPIX");

			set_caldb_query_qualifiers(&query, task->par->badpixfile);

			code = simple_caldb_query(&query, task->fptr, path);

			if (!code)
				HDpar_note(PAR_BADPIXFILE, path);
		}
		else {
			strcpy(path, task->par->badpixfile);
		}
		if (!code) {
			report_verbose("loading the bad pixel list  \n");
			code = load_bad_pixel_list(&bpRaw, path);
			report_verbose("bad pixel list is loaded  \n");
		}
	}

	/*
	 * output the primary header
	 */
	if (!code) {
		file_create_primary(task->fptr, task->ofptr, &spec);

		/*
		 * modify DATE and CREATOR keywords in the primary header
		 */
		fits_write_date(task->ofptr, &status);
		fits_update_key(task->ofptr, TSTRING, "CREATOR", CREATOR,
				"Software that created this file", &status);
		fits_update_key(task->ofptr, TSTRING, "ORIGIN", ORIGIN,
				"Processing Site", &status);

		if (task->par->history)
			HDpar_stamp(task->ofptr, 1, &status);
		if (status) {
			code = TASK_OUTPUT_ERROR;
			report_error("unable to write history\n");
		}
		if (fits_write_chksum(task->ofptr, &status)) {
			code = TASK_OUTPUT_ERROR;
			report_error("unable to write checksums\n");
		}
	}
	/*
	 * If the source map output is requested, make its primary header
	 */
	if (smapFlag){
	  if (!code){
	    file_create_primary(task->fptr, task->smapfptr, &spec);
	    fits_write_date(task->smapfptr, &status);
	    fits_update_key(task->smapfptr, TSTRING, "CREATOR", CREATOR,
			    "Software that created this file", &status);
	    fits_update_key(task->smapfptr, TSTRING, "ORIGIN", ORIGIN,
			    "Processing Site", &status);
	    if (task->par->history)
	      HDpar_stamp(task->smapfptr, 1, &status);
	    if (status) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to write history\n");
	    }
	    if (fits_write_chksum(task->smapfptr, &status)) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to write checksums\n");
	    }
	  }
	}


	/*
	 * If the additional quality map output is requested, make its primary header
	 */
	if (qmapFlag){
	  if (!code){
	    file_create_primary(task->fptr, task->qmapfptr, &spec);
	    fits_write_date(task->qmapfptr, &status);
	    fits_update_key(task->qmapfptr, TSTRING, "CREATOR", CREATOR,
			    "Software that created this file", &status);
	    fits_update_key(task->qmapfptr, TSTRING, "ORIGIN", ORIGIN,
			    "Processing Site", &status);
	    if (task->par->history)
	      HDpar_stamp(task->qmapfptr, 1, &status);
	    if (status) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to write history\n");
	    }
	    if (fits_write_chksum(task->qmapfptr, &status)) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to write checksums\n");
	    }
	  }
	}

	if (!code) {
		if (spec.hdu1) {
			/*
			 * just process the specified HDU
			 */
		        
			code = create_one_quality_map(task);
		}
		else {

			/*
			 * process all of the extensions
			 */

			int i;
			int hdus = 0;

			fits_get_num_hdus(task->fptr, &hdus, &status);
			report_verbose("processing all extensions; hdus=%d \n", hdus);

			if (hdus == 1)
				report_warning("no extensions to process\n");

			for (i = 1; !code && i < hdus; ++i) {
				int hdu = i + 1;
				int hduType;

				if (fits_movabs_hdu(task->fptr, hdu, &hduType, &status)) {
					code = TASK_FITS_ERROR;
					report_error("unable to move to extension %d [%d]\n", hdu,
							status);
				}
				else if (hduType != IMAGE_HDU) {
					code = TASK_INPUT_ERROR;
					report_error("input extension %d is not an image\n", hdu);
				}
				else {
					report_verbose("creating a quality map \n");
					code = create_one_quality_map(task);
				}
			}
		}
	}

	if (task->fptr) {
		status = 0;
		if (fits_close_file(task->fptr, &status)) {
			code = TASK_INPUT_ERROR;
			report_error("unable to close input [%d]\n", status);
		}
	}

	if (task->ofptr) {
		status = 0;
		if (fits_close_file(task->ofptr, &status)) {
			code = TASK_OUTPUT_ERROR;
			report_error("unable to close output [%d]\n", status);
		}
	}

	if (smapFlag){
	  if (task->smapfptr) {
	    status = 0;
	    if (fits_close_file(task->smapfptr, &status)) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to close output [%d]\n", status);
	    }
	  }
	}

	if (qmapFlag){
	  if (task->qmapfptr) {
	    status = 0;
	    if (fits_close_file(task->qmapfptr, &status)) {
	      code = TASK_OUTPUT_ERROR;
	      report_error("unable to close output [%d]\n", status);
	    }
	  }
	}


	return code;
}


#if 0
static int
writeQualityMap (Task * task, FITSHeader * header)
{
	int code = 0;
	int i;
	char comment[FLEN_VALUE];
	ImageIO io = { 0 };

	io.header = header;
	add_header_comment(header, "QUALITY FLAGS");

	for (i = 0; i < FLAG_TERMINATOR; ++i) {
		sprintf(comment, "%4d -> %s", (1 << i), flagString(i));
		add_header_comment(header, comment);
	}

	code = iimage_write(task->flagImage, task->par->outfile, &io);

	return code;
} /* writeQualityMAp */
#endif


static void writeFlagSummary(Task * task)
{
	int i;
	const char * columns;
	const char * format;
	const char * flag;
	int nSources, nPixels;

	report_status("processing summary:\n");
	if (task->flagImage) {
		columns = "  sources    pixels   flag\n";
		format = "   %5d   %8d  %-s\n";
	}
	else {
		columns = "  sources  flag\n";
		format = "   %5d   %-s\n";
	}

	report_status(columns);

	for (i = 0; i < FLAG_TERMINATOR; ++i) {
		flag = flagString(i);
		nSources = task->flaggedSources[i];
		nPixels = task->flaggedPixels[i];

		if (task->flagImage)
			report_status(format, nSources, nPixels, flag);
		else
			report_status(format, nSources, flag);
	}
} /* writeFrlagSummary */


static int run_parameters(Parameters * p)
{
	int code = 0;
	static Task task = { 0 };

	Constants constants = { 0 };

	FITSHeader rawHeader = { 0 };

	task.par = p;
	task.constants = &constants;

	task.flagDetections = p->flagDetections;

	if (getenv("REPORT_DUPLICATES"))
		task.reportDuplicates = 1;

	if (!code)
		code = create_quality_maps(&task, &rawHeader);
	report_verbose("finished creating quality maps \n");

	if (!code)
		writeFlagSummary(&task);

	return code;
} /* run_parameters */


static int get_parameters(Parameters * p)
{
	int code = 0;
	int status = 0;

	if (!status)
		status = PILGetFname(PAR_INFILE, p->infile);

	if (!status)
		status = PILGetFname(PAR_BADPIXFILE, p->badpixfile);

	if (!status)
		status = PILGetFname(PAR_OUTFILE, p->outfile);

	if (!status)
		status = PILGetFname(PAR_SMAPBASE, p->smapfile);

	if (!status)
		status = PILGetFname(PAR_QMAPBASE, p->qmapfile);

	if (!status)
		status = PILGetReal(PAR_PSF_FWHM, &p->psf_fwhm_arcsec);

	if (!status)
		status = PILGetReal(PAR_APERTURE, &p->aperture_arcsec);

	if (!status)
		status = PILGetReal(PAR_SOURCE_TYPE_SCALE, &p->pointExtendedScale);

	if (!status)
		status = PILGetReal(PAR_ELLIPSE_SCALE, &p->ellipseScale);

	if (!status)
		status = PILGetBool(PAR_FLAG_DETECTIONS, &p->flagDetections);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	if (status) {
		code = TASK_SETUP_ERROR;
		report_error("unable to load parameters\n");
	}
	else {
	  report_verbose("------------------------------------\n");
	  report_verbose("   uvotflagqual - %3.1f \n", VERSION);
	  report_verbose("------------------------------------\n");
		report_verbose("parameters loaded\n");
	}

	return code;
} /* get_parameters */


int TOOLSUB()
{
	int code = 0;

	Parameters p = { 0 };

	set_toolname(_STRINGIFY(TOOLSUB));
	set_toolversion(_STRINGIFY(VERSION));

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		headas_clobberfile(p.outfile);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}

