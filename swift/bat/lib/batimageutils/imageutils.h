/* $Id: imageutils.h,v 1.25 2006/05/16 19:01:10 craigm Exp $ */

/*
 * Basic Image manipulate routines
 *   * allocation
 *   * I/O to FITS file
 *   * FFT and convolution
 *   * rescale and rebalance
 *
 * C. Markwardt
 * 
 */


/* The following set of defines can select float vs double precision */
#ifndef ISFLOAT
#define ISFLOAT 1
#endif

#if ISFLOAT
#define FLOAT float
#define TFLTTYPE TFLOAT
#define FLT_IMG FLOAT_IMG
#else
#define FLOAT double
#define TFLTTYPE TDOUBLE
#define FLT_IMG FLOAT_IMG  /* Output image is still float */
#endif


/* Codes for image_stripbits */
#define STRIP_VAR     1
#define STRIP_REL     2

/* -------------------------------------------------------------------- */
/* Abstract data type which holds an image, metadata, and various
   auxiliary data required for FFT analysis */
struct image_struct {
  int naxis;    /* Number of axes */
  long axes[2]; /* Dimensionsi of image */
  int bitpix;   /* FITS bitpix value */
  int flags;    /* Various bitflags to be OR'd
		    1 = image_init allocated storage for image data
		    2 = image is in Fourier domain (default is spatial domain)
		*/
  FLOAT *data;  /* Pointer to raw image data */
  FLOAT **datap;/* Pointer to array of pointers, so array can be accessed as
		   image->datap[y][x]; */

  FLOAT *t, *w; /* Auxiliary data required for Ooura's FFT */
  int nip, *ip;
  char units[80]; /* Units of image (FITS standard string) */
  FLOAT nullval;
  double tstart, tstop, telapse, ontime, exposure; /* Time keywords */
};

#define IMGFLAG_MALLOCED	1
#define IMGFLAG_FOURIER		2

/* For image interpolation */
struct interp_struct {
  long int ncoord;
  int method;
  int flags;
  double param;
  long int nsamp, ngood;
  long int *ind, *igood;
  FLOAT *coef;
  long int minind, maxind;
};

#define INTERP_NEARN     1
#define INTERP_BILINEAR  2
#define INTERP_BICUBICB  3
#define INTERP_BICUBICH  4

/* Distortion map structure */
struct distortmap_struct {
  int naxis;
  long naxes[10];
  char ctype1[80], ctype2[80];
  double crpix1, cdelt1, crval1;
  double crpix2, cdelt2, crval2;
  FLOAT nulval;
  FLOAT *dimx, *dimy;
  FLOAT data[1];
};


/* -------------------------------------------------------------------- */
/* Routine which rescales an array to a different sampling */
extern int rescalearr(FLOAT *arr1, int nx1, int ny1, int nstride1, int nrow1,
	       FLOAT *arr2,                   int nstride2, int nrow2,
	       FLOAT xscale, FLOAT yscale, 
	       FLOAT xphase, FLOAT yphase);

void image_free(struct image_struct *image);
struct image_struct * image_init(int nx, int ny, FLOAT *data);
struct image_struct *image_rescale(struct image_struct *image, 
				   FLOAT xscale, FLOAT yscale,
				   FLOAT xphase, FLOAT yphase, 
				   int ncols, int nrows);
void image_cmult(struct image_struct *c,
		 struct image_struct *a,
		 struct image_struct *b);
void image_balance(struct image_struct *image, struct image_struct *mask);
void image_fft(struct image_struct *image);
unsigned int image_next2pown(unsigned int n);
struct image_struct *image_convolve(struct image_struct *a, 
				    struct image_struct *b, 
				    struct image_struct **fb);

/* From detmask.c */
void image_mkdetgaps(struct image_struct *image);
void image_mkdetgaps_val(struct image_struct *image, float val);
struct image_struct *image_mkdetmask(struct image_struct *base, 
				     int nx, int ny);


extern int is_units_rate(char *unitstr);

/* Following prototypes are only declared if CFITSIO has been defined */
#ifdef CFITSIO_VERSION
extern struct image_struct * image_read(fitsfile *inptr, int *status);
extern struct image_struct * image_read_i(fitsfile *inptr, int imgnum, 
					  char *colname, 
					  FLOAT *nullval, int *anynull, 
					  int *status);
extern int image_nimages(fitsfile *inptr, int *hdutype0, int *status);
extern int image_copykeys(fitsfile *outptr, fitsfile *headfile, int *status);
extern int image_copykeyclasses(fitsfile *outptr, fitsfile *headfile, 
			 int class, int copycomments, int *status);
extern int image_write(fitsfile *outptr, struct image_struct *image, 
		       int *status);
extern int image_append(fitsfile **fileptr, char *filename, int append, 
			int *status);
extern int write_wcsaxis(fitsfile *imgfile, int axis, char *suffix, 
		  char *wcsname, char *wcstype, 
		  char *ctype, double crpix, double cdelt, double crval, 
		  char *cunit, int *status);
extern int read_wcsaxis(fitsfile *imgfile, int axis, char *suffix, 
		 char *wcsname, char *wcstype, 
		 char *ctype, double *crpix, double *cdelt, double *crval, 
		 int *status);

extern int image_writeinfo(fitsfile *outptr, struct image_struct *image, int *status);
extern int image_copyinfo(struct image_struct *result, struct image_struct *a);
extern struct image_struct *image_copy(struct image_struct *source);

/* corrections.c */
extern int image_stripbits(int keepbits,
			   struct image_struct *outimg,
			   struct image_struct *varimg,
			   int striptype, 
			   FLOAT nullval);


/* imageinterp.c */
extern
int imageinterp_init(long int nx, long int ny,
		     double pixcrd[][2], long int ncoord,
		     int method, int flags, double param,
		     struct interp_struct *interp);

extern
int imageinterp_apply(FLOAT *from, FLOAT *to, FLOAT nulval,
		      struct interp_struct *interp);

extern
int imageinterp_free(struct interp_struct *interp);

/* distortmap.c */

extern
int read_distortmap(char *filename, struct distortmap_struct **d, int *status);

extern
int distortmap_coco1(struct distortmap_struct *d,
		     double imx_in,  double imy_in,
		     double *imx_out, double *imy_out,
		     int direction);
/* Values of direction */
#define APP_TO_TRUE 1          /*     apparent to true */
#define TRUE_TO_APP 2          /*     true to apparent */


#endif

/* -------------------------------------------------------------------- */

#define foreach_pixel(image, i, expr) \
  { int i, _npix=(image->axes[0]*image->axes[1]); \
    for (i=0; i<_npix; i++) {expr;} \
  }

