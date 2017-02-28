/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotimsum/uvotimsum1.c,v $
 * $Revision: 1.17 $
 * $Date: 2012/03/23 15:36:06 $
 *
 *		Sum the image extensions of an file
 *
 * $Log: uvotimsum1.c,v $
 * Revision 1.17  2012/03/23 15:36:06  rwiegand
 * Clarified error message written when input image is not oriented North up.
 *
 * Revision 1.16  2009/11/05 21:45:48  rwiegand
 * The previous commit deleted the statement to read the exclude parameter
 * (thanks for catching that, Alex!).
 *
 * Revision 1.15  2009/10/26 17:13:23  rwiegand
 * Support summing LSS maps.
 *
 * Revision 1.14  2009/10/05 14:06:21  rwiegand
 * Support infile being a comma-delimited list of files or @filename.  Propagate
 * keywords which are consistent across input HDUs to the output.
 *
 * Revision 1.13  2008/10/16 18:30:00  rwiegand
 * Write keycards with more significant digits.
 *
 * Revision 1.12  2008/07/02 15:53:18  rwiegand
 * Support summing flag images.
 *
 * Revision 1.11  2006/01/19 14:57:02  rwiegand
 * Extended the exclude parameter to support excluding HDUs based on the
 * presence/value of the ASPCORR keyword.
 *
 * Revision 1.10  2005/07/18 12:47:37  rwiegand
 * Null terminate FITS key class list.
 *
 * Revision 1.9  2005/03/21 14:23:27  rwiegand
 * Include RADECSYS and EQUINOX keywords in output.
 *
 * Revision 1.8  2005/02/17 14:56:00  rwiegand
 * Pass general keywords from input primary to output.
 *
 * Revision 1.7  2004/12/29 21:19:39  rwiegand
 * Implemented exclude parameter.
 *
 * Revision 1.6  2004/12/29 20:18:39  rwiegand
 * Specialized summing of exposure maps.
 *
 * Revision 1.5  2004/12/23 19:43:24  rwiegand
 * Added parameter to indicate whether summing exposure maps or images.
 * Also added (but did not implement) parameter for excluding extensions
 * from sum.
 *
 * Revision 1.4  2004/12/10 01:19:38  rwiegand
 * Corrected documentation of pixsize units.  Was cheating too much on
 * gnomonic projection.
 *
 * Revision 1.3  2004/10/21 13:13:22  rwiegand
 * Argh:  CUNITn array overflowing.
 *
 * Revision 1.2  2004/09/27 21:42:22  rwiegand
 * Needed to switch to higher precision for converting between world and pixel
 * coordinates.  Removed unnecessary WCS keywords.
 *
 * Revision 1.1  2004/09/23 19:44:07  rwiegand
 * Relocated uvotimsum script and added tool that adds correctly oriented
 * images quickly.
 *
 */


#include <stdio.h>	/* pil.h uses but does not include */
#include <string.h>
#include <math.h>

#include "headas.h"
#include "pil.h"
#include "genimage.h"
#include "report.h"
#include "keyutil.h"
#include "overlap.h"
#include "uvottool.h"


#define TOOLSUB uvotimsum1
#define TOOLVER 1.1
#include "headas_main.c"


enum
{
	INSPECT_OK,
	INSPECT_ERROR,
	INSPECT_ASSIMILATE,
	INSPECT_IGNORE
};


typedef double real;

typedef float PixType;
typedef FImage SkyImage;

typedef int FlagType;
typedef IImage FlagImage;


typedef struct
{
	int chatter;
	int clobber;
	int history;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	double ra, dec;      /* center of output image [deg] */
	double pixsize;      /* output pixel size (at center) [arcsec] */
	int width, height;   /* dimensions of output */

	char datatype[PIL_LINESIZE];
	int expmap;
	int qualflag;
	int lssmap;

	char exclude[PIL_LINESIZE];

} Parameters;



typedef struct
{
	double sinra;
	double cosra;
	double sindec;
	double cosdec;
} Gnomonic;


typedef struct
{
	int bitpix, naxis;
	long naxes[2];

	double crval1, crval2, crpix1, crpix2, cdelt1, cdelt2, crota;
	char ctype[8];

	char ctype1[FLEN_VALUE], ctype2[FLEN_VALUE];
	char cunit1[FLEN_VALUE], cunit2[FLEN_VALUE];
	char radecsys[FLEN_VALUE];

	double exposure;
	double equinox;

	void * projection;

} ImageInfo;


typedef struct
{
	int i0, i1, j0, j1;
	real u1, u2, v1, v2;
	real area;

	int width;
	int height;

} IterationLimits;


typedef struct
{
	const Parameters * par;

	fitsfile * fptr;

	SkyImage * sum;
	SkyImage * weightSum;
	FlagImage * quality;
	ImageInfo * inInfo;
	ImageInfo * outInfo;

	double scale;

} Summer;




#ifdef STSDAS

      REAL FUNCTION OVER(I,J,XMIN,XMAX,YMIN,YMAX)
C
C OVER - calculate overlap between an arbitrary rectangle, aligned
C        with the axes, and a pixel.
C
C This is a simplified version of the BOXER code.
C
C Richard Hook, 6th May 1998
C
      IMPLICIT NONE

      INTEGER I,J
      REAL XMIN,XMAX,YMIN,YMAX

      REAL DX,DY

      DX=MIN(XMAX,REAL(I)+0.5)-MAX(XMIN,REAL(I)-0.5)
      DY=MIN(YMAX,REAL(J)+0.5)-MAX(YMIN,REAL(J)-0.5)

      IF(DX.GT.0 .AND. DY.GT.0) THEN
         OVER=DX*DY
      ELSE
         OVER=0.0
      ENDIF

      RETURN
      END

#endif /* STSDAS */



#define U_MAX(x,y) (x > y ? x : y)
#define U_MIN(x,y) (x < y ? x : y)

real rect_overlap (int i, int j,
		real xmin, real xmax, real ymin, real ymax)
{
	real dx, dy;

	dx = U_MIN(xmax, i + 0.5) - U_MAX(xmin, i - 0.5);
	dy = U_MIN(ymax, j + 0.5) - U_MAX(ymin, j - 0.5);

	if (dx > 0 && dy > 0)
		return dx * dy;

	return 0;
}


#if 0
static void
u_pix_to_world (ImageInfo * wcs, real xpix, real ypix,
			real * xworld, real * yworld)
{
	/* +1 to account switch from 0 based to 1 based for FITS */
	*xworld = (wcs->crpix1 - (xpix + 1)) * wcs->cdelt1 + wcs->crval1;
	*yworld = (wcs->crpix2 - (ypix + 1)) * wcs->cdelt2 + wcs->crval2;
}


static void
u_world_to_pix (ImageInfo * wcs, real xworld, real yworld,
			real * xpix, real * ypix)
{
	/* 1 to account switch from 0 based to 1 based for FITS */
	*xpix = wcs->crpix1 - 1 - (xworld - wcs->crval1) / wcs->cdelt1;
	*ypix = wcs->crpix2 - 1 - (yworld - wcs->crval2) / wcs->cdelt2;
}
#endif


static void
g_pix_to_world (ImageInfo * wcs, real xpix, real ypix,
			real * xworld, real * yworld)
{
	double dx, dy, L, M, x, y, z;

	Gnomonic * p = (Gnomonic *) wcs->projection;

	/* +1 to account switch from 0 based to 1 based for FITS */
	dx = (xpix + 1 - wcs->crpix1) * wcs->cdelt1;
	dy = (ypix + 1 - wcs->crpix2) * wcs->cdelt2;

	L = dx * M_PI / 180;
	M = dy * M_PI / 180;

	x = p->cosdec * p->cosra - L * p->sinra - M * p->cosra * p->sindec;
	y = p->cosdec * p->sinra + L * p->cosra - M * p->sinra * p->sindec;
	z = p->sindec + M * p->cosdec;

	*xworld = atan2(y, x) * 180 / M_PI;
	*yworld = atan(z / hypot(x, y)) * 180 / M_PI;
}


static void
g_world_to_pix (ImageInfo * wcs, real xworld, real yworld,
			real * xpix, real * ypix)
{
	double dx, dy, ra, dec, ra0, coss, sins, sint, L, M;
	Gnomonic * p = (Gnomonic *) wcs->projection;

	dx = xworld - wcs->crval1;
	dy = yworld - wcs->crval2;

	ra = xworld * M_PI / 180;
	dec = yworld * M_PI / 180;
	ra0 = wcs->crval1 * M_PI / 180;

	coss = cos(dec);
	sins = sin(dec);

	L = sin(ra - ra0) * coss;
	sint = sins * p->sindec + coss * p->cosdec * cos(ra - ra0);

	if (p->cosdec < 0.001)
		{
			M = (coss * cos(ra - ra0)) / (sins * p->sindec);
			M = (-M + p->cosdec * (1 + M * M)) / p->sindec;
		}
	else
		{
			M = (sins / sint - p->sindec) / p->cosdec;
		}

	if (fabs(p->sinra) < 0.3)
		{
			L = coss * sin(ra) / sint - p->cosdec * p->sinra
					+ M * p->sinra * p->sindec;
			L /= p->cosra;
		}
	else
		{
			L = coss * cos(ra) / sint - p->cosdec * p->cosra
					+ M * p->cosra * p->sindec;
			L /= -p->sinra;
		}

	dx = L * 180 / M_PI;
	dy = M * 180 / M_PI;

#if 0
	no rotation allowed!
	tmp = dx * cosr + dy * sinr;
	dy = dy * cosr - dx * sinr;
	dx = tmp;
#endif

	/* -1 to account for switch from FITS to 0 based */
	*xpix = dx / wcs->cdelt1 + wcs->crpix1 - 1;
	*ypix = dy / wcs->cdelt2 + wcs->crpix2 - 1;
}


#define U_ORDER(a,b) if (a>b) { real tmp = a; a = b; b = tmp; }


static void
find_iteration_limits (Summer * summer, int x, int y, IterationLimits * limits)
{
	real r1, r2, s1, s2;
	real u1, u2, v1, v2;

	/* this conversion could be optimized to 
			u = u0 + x * du
			v = v0 + y * dv
	*/

	/* determine the world coordinates of this pixel */
	g_pix_to_world(summer->inInfo, x - 0.5, y - 0.5, &r1, &s1);
	g_pix_to_world(summer->inInfo, x + 0.5, y + 0.5, &r2, &s2);

	/* determine the output pixel coordinates of this pixel */
	g_world_to_pix(summer->outInfo, r1, s1, &u1, &v1);
	g_world_to_pix(summer->outInfo, r2, s2, &u2, &v2);

	/* orient rectangle */
	U_ORDER(u1, u2);
	U_ORDER(v1, v2);
	limits->area = (u2 - u1) * (v2 - v1);

	/* distribute value */
	limits->i0 = (int) floor(u1 + 0.5);
	limits->i1 = (int) ceil(u2 - 0.5 + 1);
	limits->j0 = (int) floor(v1 + 0.5);
	limits->j1 = (int) ceil(v2 - 0.5 + 1);

	if (limits->i0 < 0)
		limits->i0 = 0;
	if (limits->i1 > limits->width)
		limits->i1 = limits->width;

	if (limits->j0 < 0)
		limits->j0 = 0;
	if (limits->j1 > limits->height)
		limits->j1 = limits->height;

	limits->u1 = u1;
	limits->u2 = u2;
	limits->v1 = v1;
	limits->v2 = v2;
}


static int
assimilate_flags (FlagImage * image, IIState * state)
{
	int code = 0;
	int x, y;
	FlagType z;
	Summer * summer = (Summer *) state->user;
	FlagImage * outImage = summer->quality;
	IterationLimits limits = { 0 };

	x = state->x;
	y = state->y;

	limits.width = outImage->width;
	limits.height = outImage->height;

	z = iimage_get_relative(image, x, y);

	if (z == image->null)
		; /* ignore null pixel */
	else if (z == 0)
		; /* ignore zero pixel */
	else
		{
			int i, j;

			find_iteration_limits(summer, x, y, &limits);

			for (i = limits.i0; i < limits.i1; ++i)
				for (j = limits.j0; j < limits.j1; ++j)
					{
						real overlap = rect_overlap(i, j,
								limits.u1, limits.u2,
								limits.v1, limits.v2);

						if (overlap > 0)
							{
								FlagType oz;
								oz = iimage_get_relative(outImage, i, j);
								if (oz != outImage->null)
									iimage_set_relative(outImage, i, j, oz | z);
							}
					}
		}

	return code;
}


int processFlagImage (Summer * summer, ImageInfo * info)
{
	int code = 0;
	FlagImage image = { 0 };

	if (!code)
		{
			ImageIO io = { 0 };
			code = iimage_read_chdu(&image, summer->fptr, &io);
		}

	if (!code)
		{
			IIState state = { 0 };
			summer->inInfo = info;
			state.user = summer;
			image.warnings = 10;
			code = iimage_iterate(&image, &assimilate_flags, &state);
		}

	iimage_release(&image);

	return code;
}


static int
assimilate_pixel (SkyImage * image, IIState * state)
{
	int code = 0;
	int x, y;
	PixType z;
	Summer * summer = (Summer *) state->user;
	SkyImage * outImage = summer->sum;
	SkyImage * weightImage = summer->weightSum;
	double scale = summer->scale;
	IterationLimits limits = { 0 };

	x = state->x;
	y = state->y;

	limits.width = outImage->width;
	limits.height = outImage->height;

	z = fimage_get_relative(image, x, y);

	if (z == image->null)
		; /* ignore null pixel */
	else if (z == 0)
		; /* ignore zero pixel */
	else
		{
			int i, j;

			find_iteration_limits(summer, x, y, &limits);

			for (i = limits.i0; i < limits.i1; ++i)
				for (j = limits.j0; j < limits.j1; ++j)
					{
						real overlap = rect_overlap(i, j,
										limits.u1, limits.u2,
										limits.v1, limits.v2);

						if (overlap > 0)
							{
								PixType oz;
								PixType delta;
								if (weightImage)
									delta = (PixType) (z * overlap * scale);
								else
									delta = (PixType) (z * overlap * scale / limits.area);

								oz = fimage_get_relative(outImage, i, j);
								if (oz != outImage->null)
									fimage_set_relative(outImage, i, j, oz + delta);

								if (weightImage)
									{
										PixType delta = (PixType) (overlap * scale);
										oz = fimage_get_relative(weightImage, i, j);
										fimage_set_relative(weightImage, i, j, oz + delta);
									}
							}
					}
		}

	return code;
}


int processRealImage (Summer * summer, ImageInfo * info)
{
	int code = 0;
	SkyImage image = { 0 };

	summer->inInfo = info;

	if (!code)
		{
			ImageIO io = { 0 };
			code = fimage_read_chdu(&image, summer->fptr, &io);
		}

	if (code)
		; /* toast */
	else if (summer->par->expmap)
		{
			ImageInfo * oi = summer->outInfo;
			summer->scale =
						(info->cdelt1 * info->cdelt1 + info->cdelt2 * info->cdelt2)
								/ (oi->cdelt1 * oi->cdelt1 + oi->cdelt2 * oi->cdelt2);
			report_status("scaling by %.3f\n", summer->scale);
		}
	else if (summer->par->lssmap)
		summer->scale = summer->inInfo->exposure;
	else
		summer->scale = 1;

	if (!code)
		{
			IIState state = { 0 };
			state.user = summer;
			image.warnings = 10;
			code = fimage_iterate(&image, &assimilate_pixel, &state);
		}

	fimage_release(&image);

	return code;
}


int assimilate_hdu (Summer * summer, int hdu0)
{
	int code = 0;
	int status = 0;
	SkyImage image = { 0 };
	ImageInfo info = { 0 };
	Gnomonic gnomonic;
	image.null = -1;

	if (fits_get_img_param(summer->fptr, 2,
					&info.bitpix, &info.naxis, info.naxes, &status))
		report_error("unable to get HDU %d+1 image [%d]\n", hdu0, status);

	else if (info.naxis != 2)
		return 0; /* skip non-2d image */

	else if (fits_read_img_coord(summer->fptr, &info.crval1, &info.crval2,
					&info.crpix1, &info.crpix2, &info.cdelt1, &info.cdelt2,
					&info.crota, info.ctype, &status))
		report_error("unable to get WCS info [%d]\n", status);

	if (status)
		return TASK_INPUT_ERROR;

	report_status("assimilating HDU %d+1\n", hdu0);

	if (!code)
		{
			gnomonic.sinra = sin(info.crval1 * M_PI / 180);
			gnomonic.sindec = sin(info.crval2 * M_PI / 180);
			gnomonic.cosra = cos(info.crval1 * M_PI / 180);
			gnomonic.cosdec = cos(info.crval2 * M_PI / 180);
			info.projection = &gnomonic;
		}

	if (!code)
		{
			/* validate image info */
			/* could be a little more lenient */
			ImageInfo * oi = summer->outInfo;

			if (strcmp(info.ctype, "-TAN"))
				{
					code = TASK_INPUT_ERROR;
					report_error("supports tangent projections, got '%s'\n", info.ctype);
				}
			else
				{
					strcpy(oi->ctype, "-TAN");
					strcpy(oi->ctype1, "RA---TAN");
					strcpy(oi->ctype2, "DEC--TAN");
				}

			/* could fall back on Quad overlap */
			if (fabs(info.crota * info.naxes[0]) > 1e-3
						|| fabs(info.crota * info.naxes[1]) > 1e-3)
				{
					code = TASK_INPUT_ERROR;
					report_error("supports North up images, but +Y axis is %e [deg] off North\n", info.crota);
				}
		}

	if (!code)
		{
			fits_read_key_str(summer->fptr, "CUNIT1", info.cunit1, 0, &status);
			fits_read_key_str(summer->fptr, "CUNIT2", info.cunit2, 0, &status);
			if (status)
				{
					code = TASK_INPUT_ERROR;
					report_error("missing CUNITn keyword(s)\n");
				}
			else if (strcmp(info.cunit1, info.cunit2))
				{
					code = TASK_INPUT_ERROR;
					report_error("HDU %d+1 CUNITn mismatch [%s/%s]\n",
							hdu0, info.cunit1, info.cunit2);
				}
			else
				{
					ImageInfo * oi = summer->outInfo;
					if (!oi->cunit1[0])
						{
							strcpy(oi->cunit1, info.cunit1);
							strcpy(oi->cunit2, info.cunit2);
							report_status("selected output units [%s]\n", oi->cunit1);
						}
					else if (strcmp(oi->cunit1, info.cunit1))
						{
							code = TASK_INPUT_ERROR;
							report_error("HDU %d+1 units [%s] do not match\n",
									hdu0, info.cunit1);
						}
					else
						; /* repeat of same CUNITn */
				}
		}

	if (!code)
		{
			int tmp = 0;
			fits_write_errmark();
			fits_read_key_dbl(summer->fptr, "EXPOSURE", &info.exposure, 0, &tmp);
			fits_read_key_str(summer->fptr, "RADECSYS", info.radecsys, 0, &tmp);
			fits_read_key_dbl(summer->fptr, "EQUINOX", &info.equinox, 0, &tmp);
			fits_clear_errmark();
			if (tmp)
					report_warning("missing EXPOSURE/RADECSYS/EQUINOX keyword(s)\n");
			else
				{
					ImageInfo * oi = summer->outInfo;
					if (!oi->radecsys[0])
						{
							strcpy(oi->radecsys, info.radecsys);
							oi->equinox = info.equinox;
							report_status("set output system %s/%e\n",
														oi->radecsys, oi->equinox);
						}
					else if (strcmp(oi->radecsys, info.radecsys))
						{
							code = TASK_INPUT_ERROR;
							report_error("HDU %d+1 RADECSYS [%s] does not match\n",
									hdu0, info.radecsys);
						}
					else if (oi->equinox != info.equinox)
						{
							code = TASK_INPUT_ERROR;
							report_error("HDU %d+1 EQUINOX [%e] does not match\n",
									hdu0, info.equinox);
						}
					else
						; /* repeat of same RADECSYS/EQUINOX */
				}
		}

	if (!code)
		{
			if (summer->par->qualflag)
				code = processFlagImage(summer, &info);
			else
				code = processRealImage(summer, &info);
		}

	return code;
}


static int divide_sum_by_weights (Summer * summer)
{
	int code = 0;
	int x, y;
	PixType z, w, zhat;
	SkyImage *sumImage = summer->sum;
	SkyImage *weightImage = summer->weightSum;

	if (!code)
		{
			for (y = 0; y < sumImage->height; ++y)
				for (x = 0; x < sumImage->width; ++x)
					{
						z = fimage_get_relative(sumImage, x, y);
						w = fimage_get_relative(weightImage, x, y);

						if (w > 0)
							zhat = z / w;
						else
							zhat = 0;

						fimage_set_relative(summer->sum, x, y, zhat);
					}
		}

	return code;
}


int
exclude_hdu (const Parameters * par, const char * hdu, int * status)
{
	char buffer[PIL_LINESIZE];
	char * start;
	const char * token;

	if (!strcasecmp(par->exclude, "-"))
		return 0;

	strcpy(buffer, par->exclude);

	start = buffer;
	while ((token = strtok(start, ",")) != NULL)
		{
			start = 0;
			if (!strcasecmp(token, hdu))
				return 1;
		}

	return 0;
}


int
inspect_hdu (Summer * summer, int hdu0)
{
	int code = INSPECT_OK;
	int status = 0;
	int type;
	char extname[FLEN_VALUE];

	sprintf(extname, "%d", hdu0);
	
	if (fits_movabs_hdu(summer->fptr, hdu0 + 1, &type, &status))
		{
			code = INSPECT_ERROR;
			report_error("unable to move to HDU %d+1 [%d]\n", hdu0, status);
		}

	else if (exclude_hdu(summer->par, extname, &code))
		code = INSPECT_IGNORE;

	else
		{
			fits_write_errmark();
			extname[0] = 0;
			fits_read_key_str(summer->fptr, "EXTNAME", extname, 0, &status);
			if (status)
				status = 0;
			else if (exclude_hdu(summer->par, extname, &code))
				code = INSPECT_IGNORE;
			fits_clear_errmark();
		}


	if (code == INSPECT_IGNORE)
		report_status("HDU [%s] is on the exclude list\n", extname);

	else if (code)
		;

	else if (type != IMAGE_HDU)
		{
			code = INSPECT_IGNORE;
			report_status("HDU %d+1 is not an image\n", hdu0);
		}

	else
		code = INSPECT_ASSIMILATE;


	return code;
}


int
run_parameters (Parameters * p)
{
	int code = 0;

	Summer summer = { 0 };
	SkyImage sum = { 0 };
	SkyImage weightSum = { 0 };
	FlagImage quality = { 0 };
	ImageInfo outInfo = { 0 };
	FITSHeader header = { 0 };
	Gnomonic gnomonic;
	int status = 0;
	int hdus;

	summer.par = p;

	sum.null = -1;
	sum.warnings = 10;
	quality.null = -1;
	quality.warnings = 10;

	if (!code && p->clobber)
		code = headas_clobberfile(p->outfile);

	if (!code && fits_open_file(&summer.fptr, p->infile, READONLY, &status))
		{
			report_error("unable to open %s [%d]\n", p->infile, status);
			code = TASK_INPUT_ERROR;
		}

	if (!code && fits_get_num_hdus(summer.fptr, &hdus, &status))
		{
			report_error("unable to get HDU count [%d]\n", status);
			code = TASK_INPUT_ERROR;
		}

	if (!code)
		{
			if (p->qualflag)
				code = iimage_allocate(&quality, p->width, p->height);
			else
				code = fimage_allocate(&sum, p->width, p->height);
		}

	if (!code)
		{
			if (p->qualflag)
				code = iimage_set_constant(&quality, 0);
			else
				code = fimage_set_constant(&sum, 0);
		}

	if (!code)
		{
			outInfo.naxis = 2;
			outInfo.bitpix = -32;
			outInfo.naxes[0] = p->width;
			outInfo.naxes[1] = p->height;

			outInfo.crval1 = p->ra;
			outInfo.crval2 = p->dec;
			outInfo.crpix1 = (p->width + 1.0) / 2;
			outInfo.crpix2 = (p->height + 1.0) / 2;
			outInfo.cdelt1 = -p->pixsize;
			outInfo.cdelt2 = p->pixsize;
			outInfo.crota = 0;

			gnomonic.sinra = sin(p->ra * M_PI / 180);
			gnomonic.sindec = sin(p->dec * M_PI / 180);
			gnomonic.cosra = cos(p->ra * M_PI / 180);
			gnomonic.cosdec = cos(p->dec * M_PI / 180);
			outInfo.projection = &gnomonic;

			summer.sum = &sum;
			summer.quality = &quality;
			summer.outInfo = &outInfo;
		}

	if (!code && p->lssmap)
		{
			summer.weightSum = &weightSum;
			code = fimage_allocate(&weightSum, p->width, p->height);
			if (!code)
				code = fimage_set_constant(&weightSum, 0);
		}

	if (!code)
		{
			int hdu;
			for (hdu = 0; !code && hdu < hdus; ++hdu)
				{
					int c = inspect_hdu(&summer, hdu);
					if (c == INSPECT_ASSIMILATE)
						code = assimilate_hdu(&summer, hdu);
					else if (c == INSPECT_IGNORE)
						;
					else
						code = TASK_INPUT_ERROR;
				}
		}

	if (!code && p->lssmap)
		code = divide_sum_by_weights(&summer);

	if (!code)
		{
			ImageIO io = { 0 };
			header.decimals = 12;

			set_header_key_string(&header, "CTYPE1", outInfo.ctype1);
			set_header_key_string(&header, "CTYPE2", outInfo.ctype2);
			set_header_key_double(&header, "CRVAL1", outInfo.crval1);
			set_header_key_double(&header, "CRVAL2", outInfo.crval2);
			set_header_key_double(&header, "CRPIX1", outInfo.crpix1);
			set_header_key_double(&header, "CRPIX2", outInfo.crpix2);
			set_header_key_double(&header, "CDELT1", outInfo.cdelt1);
			set_header_key_double(&header, "CDELT2", outInfo.cdelt2);
			set_header_key_string(&header, "CUNIT1", outInfo.cunit1);
			set_header_key_string(&header, "CUNIT2", outInfo.cunit2);

			if (!outInfo.radecsys[0])
				strcpy(outInfo.radecsys, "FK5");
			if (!outInfo.equinox)
				outInfo.equinox = 2000;
			set_header_key_string(&header, "RADECSYS", outInfo.radecsys);
			set_header_key_double(&header, "EQUINOX", outInfo.equinox);

			io.header = &header;

			if (p->history)
				io.history = 1;
			io.checksum = 1;
			if (p->qualflag)
				code = iimage_write(&quality, p->outfile, &io);
			else
				code = fimage_write(&sum, p->outfile, &io);
		}

	fimage_release(&sum);
	iimage_release(&quality);
	release_header(&header);

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
		pill = PILGetReal("ra", &p->ra);

	if (!pill)
		pill = PILGetReal("dec", &p->dec);

	if (!pill)
		pill = PILGetInt("width", &p->width);

	if (!pill)
		pill = PILGetInt("height", &p->height);

	if (!pill)
		pill = PILGetReal("pixsize", &p->pixsize);

	if (!pill)
		pill = PILGetString("exclude", p->exclude);

	if (!pill)
		{
			pill = PILGetString("datatype", p->datatype);
			if (pill)
				; /* trouble */
			else if (!strcasecmp(p->datatype, "EXPMAP"))
				p->expmap = 1;
			else if (!strcasecmp(p->datatype, "FLAG"))
				p->qualflag = 1;
			else if (!strcasecmp(p->datatype, "LSSMAP"))
				p->lssmap = 1;
		}

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	if (pill)
		{
			code = TASK_SETUP_ERROR;
			report_error("unable to load parameters\n");
		}
	else
		report_status("parameters loaded\n");

	return code;
}



int
uvotimsum1 ()
{
	int code = 0;
	Parameters p = { 0 };

	set_toolname(_STRINGIFY(TOOLSUB));
	set_toolversion(_STRINGIFY(TOOLVER));

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}


