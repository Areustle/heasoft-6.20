/* 
 * batcelldetect
 * Definitions and declarations
 * 
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: batcelldetect.h,v 1.66 2010/06/22 22:40:52 craigm Exp $
 */

/* NEW WCS Library */
#include <wcslib/wcs.h>
#include <wcslib/wcshdr.h>

/* Geometries for the background window */
#define CIRCLE 1
#define SQUARE 2
#define SMOOTH_CIRCLE 3

/* PSF Shape types */
#define GAUSSIAN 1
#define PYRAMID  2
#define TRUNCONE 3

/* Image statistics */
#define STATGAUSS 1
#define STATPOISS 2

/* Constraint flags */
#define FIX_IMX   1
#define FIX_IMY   2
#define FIX_FLUX  4
#define FIX_WIDTH 8

/* Detection methods */
#define METH_CELL 1
#define METH_PSF  2
#define METH_MULT 4

/* Ways to calculate output position when outvect=YES */
#define POS_FIRST    1
#define POS_FIXFIRST 2
#define POS_LAST     3
#define POS_AVG      4
#define POS_MAX_SNR  5

/* Maximum number of pixels in a detected source */
#define MAXSRCPIX 200

/* Errors from source detection */
#define ERR_MAXSRCPIX   -1 /* Source exceeded MAXSRCPIX pixels */
#define ERR_NOPIX       -2 /* No pixels detected for source */
#define ERR_NULLBORDER  -3 /* Source lies on border of image */
#define ERR_ERRTOOBIG   -4 /* Specified position error is too big for PSF fit*/

#define DTOR (M_PI / 180.0)  /* Degrees to radians */

/* Maximum number of *APP correction keywords to read */
#define MAX_APP_KEYS 32

/* Definitions of image types */
#define FLUXMAP       0
#define PCODEMAP_FILE 1
#define PCODEMAP_APPEND 2
#define PCODEMAP_LAST 3
#define BKGVARMAP     4
#define STDDEVMAP     5
#define SIGNIFMAP     6
#define BKGMAP        7

/* Structure used for arbitrary keyword values */
struct keyword_struct {
  char name[FLEN_CARD];
  char comment[FLEN_CARD];
  char dtype;
  union val_union {
    char str[FLEN_CARD];
    int bool;
    long int intv;
    double floatv;
  } val;
};

struct parm_struct 
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];    /* Input image file name */
  char outfile[PIL_PATH_MAX];   /* Output source list name */
  char pcodefile[PIL_PATH_MAX]; /* Partial coding map */
  double pcodethresh;           /* Partial coding threshold */
  double bkgpcodethresh;        /* Background partial coding threshold */
  char signifmap[PIL_PATH_MAX]; /* (optional) output significance map */
  char bkgmap[PIL_PATH_MAX];    /* (optional) output background map */
  char bkgvarmap[PIL_PATH_MAX]; /* (optional) output background variance map */
  char incatalog[PIL_PATH_MAX]; /* (optional) input catalog to check against */
  char regionfile[PIL_PATH_MAX];/* (optional) output region file */
  char distfile[PIL_PATH_MAX];  /* (optional) distortion map file */
  int bkgwindowtype;            /* Background window type (circle or square) */
  int bkgwindowrad;             /* Background window radius (pixel) */
  int srcwindowrad;             /* Source window radius (pixel) */
  int npixthresh;               /* Minimum number of pixels */
  int niter;                    /* Number of iterations to compute */
  int pospeaks;                 /* Search for positive peaks only */
  double snrthresh;             /* Minimum signal to noise threshold */

  double tstart, tstop;         /* Start / stop time from image file */
  double exposure;              /* Exposure time from image file */
  double oversampx, oversampy;  /* OVERSAMPX/Y from image file */
  int nbatdets;                 /* NBATDETS from image file */
  fitsfile *outcat;             /* Output catalog */
  double batz;                  /* BAT_Z of source */
  int nadjpix;                  /* Number of adjacent pixels required */
  char newsrcname[FLEN_CARD];   /* New source name (or C format string) */
  int newsrcind;                /* New source index (or -1 for RA/DEC format)*/
  struct keyword_struct appkeys[MAX_APP_KEYS]; /* Application-specific keywords */
  int nappkeys;                 /* Number of *APP keywords in input */

  int srcdetect;                /* Detect new sources? 1=yes,0=no*/
  int srcfit;                   /* Fit source position/intensity? 1=yes,0=no */
  int fitpos;                   /* Fit positions of known sources? 1=yes 0=no */
  int bkgfit;                   /* Fit background of each source? 1=yes 0=no */

  double posfitwindow;          /* Position fit window for all pos fits (deg) */
  double possyserr;             /* Position systematic error for all pos fits (deg)*/
  int outvector;                /* Output vector fluxes? (=1) or one per row? (=0)*/
  int carryover;                /* Carry over detected sources in one image to next? */
  char rowstr[1024];            /* Row selection expression */
  int nonullborder;             /* Remove sources on the borders? 1=yes, 0=no */
  int psfshape;                 /* PSF shape; one of GAUSSIAN or PYRAMID */
  char **keepkeywords;          /* List of keyword patterns to keep */
  int nkeepkeywords;            /* Number of keyword patterns in list */
  char ra_colname[FLEN_CARD];   /* RA column name (could be GLON too) */
  char dec_colname[FLEN_CARD];  /* Dec column name (could be GLON too) */
  char **hduclasses;            /* HDUCLASn filters */
  int nhduclasses;              /* Number of HDUCLASn filters */

  double psffwhm;               /* Starting PSF full-width at half-max (deg) */
  double psftopwidth;           /* Starting PSF top width (deg) for pyramid */
  int fit_fwhm, fit_topwidth;   /* Fit FWHM or top width? 1=yes, 0=no */
  char hduclas2[FLEN_CARD];     /* HDUCLAS2 keyword value */
  int keepbits;                 /* Number of bits to keep in output image pixel values */
  char psfdebugfile[PIL_PATH_MAX]; /* Debug output file for PSF fitting */
  int inbkgmap_zero;            /* Set background map to zero? (1=YES; 0=NO) */
  char inbkgmap[PIL_PATH_MAX];  /* Input background map (or NONE or ZERO) */
  char inbkgvarmap[PIL_PATH_MAX];  /* Input background variance map (or NONE) */
  char sortcolumns[PIL_PATH_MAX];/* Sort column expression (or NONE) */
  int statistics;               /* Image statistics: STATGAUSS or STATPOISS */
  int keep_bad_sources;         /* Keep source entries where detect_status != 0? 1=yes; 0=no */
  int pt_correl;                /* Point-to-point correlation type (0=none; 1=PSF; 2=GAUSSIAN) */
  double pt_correl_parms[10];   /* Point-to-point correlation parameters */
  double psf_chitol;            /* PSF fit chi-square convergence criterium */
  double psf_partol;            /* PSF fit parameter convergence criterium */
  int posfluxfit;               /* PSF fit - force positive flux peaks? (0=any; 1=pos-only) */
  int vectpos;               /* Vector position method? POS_* above */
};

#define SRCNAME_LEN 30
#define RA_COLNAME "RA_OBJ"
#define DEC_COLNAME "DEC_OBJ"
#define ERR_RAD_COLNAME "ERR_RAD"
#define NAME_COLNAME "NAME"
#define SOURCEID_COLNAME "CATNUM"

/* Record for detection of sources */
struct source_struct {
  char name[SRCNAME_LEN];/* Name of source */
  int sourceid;          /* Source catalog ID number */
  int status;            /* Source status (see ERR_*) defines above 0=OK */
  int npix;              /* Number of source pixels */
  int precat;            /* Is source pre-cataloged? 1=yes 0=no */
  int constraintflags;   /* Flags for constraining fit parameters:
			    1 - fix RA
			    2 - fix DEC
			    3 - fix FLUX
			    4 - fix SIGMA 
			 */

  /* Following quantities are computed at the peak flux pixel, i.e.,
     no subpixelization occurs */
  /*                         During flooddetect() -->  After src_summarize() */
  /* Pixel coords:            C 0-based             --> FITS 1-based   */
  int xpeak, ypeak;       /* Peak flux position in unit pixels */

  double peakflux;        /* Flux at peak pixel location */
  double peakflux_err;    /* Error in peakflux */
  double centflux;        /* Pixel value containing the PSF centroid */
  double bestflux;        /* Best flux; either peakflux, or PSF peak if calculated */
  double bestflux_err;    /* Error in bestflux */

  double bkgflux, bkgvar; /* Background and its variation at peak position */
  double ebkgflux;        /* Error in background flux */
  double nbkgpix;         /* Number of background pixels used to compute bkg */

  double bkgflux_fit, bkgflux_cell; /* Background values derived from cell & fit methods */

  /* Following quantities are centroided, i.e., the mean centroid is
     computed by performing a sum over pixel position, weighted by the
     background subtracted flux. */
  /* During the flooddetect() algorithm, these quantities are the
     running totals of the summation.  After the src_summarize()
     function has been applied, the mean values are computed. */
  /*                         During flooddetect() -->  After src_summarize() */
  /* Pixel coords:            C 0-based             --> FITS 1-based   */
  double xsum, ysum;       /* Weighted sum of X & Y --> centroid X & Y */
  double xsum2, ysum2;     /* Weighted sum of X & Y --> std dev  X & Y */
  double sumflux;          /* Sum of pixel fluxes   --> mean source flux */
  double sum2flux;         /* Sum of squared fluxes --> spatial std dev flux*/

  /* NOTE: during accumulation, pixel positions are 0-based.  After
     summarize(), pixel coordinates are 1-based, in order to be
     compatible with FITS */

  /* The "orig" indicates the position of the source in world
     coordinates as originally read from the input catalog.  It is
     assumed that the input catalog coordinates are "true" (corrected)
     celestial coordinates, with any BAT distortion removed. 

     The "fitted" indicates the uncorrected "work in progress"
     position within batcelldetect, "apparent" (uncorrected).  These
     quantities never reach the outside world.

     The "corr" indicates the position in world coordinates, after
     fitting, and after correcting to the "true" position again.
  */

#define NULL_POS -1e37
  double imx, imy;         /* Peak centroid in tangent coord (fitted)*/
  double imx_orig,imy_orig;/* Peak centroid in tangent coord (orig catalog)*/
  double imx_corr,imy_corr;/* Peak centroid in tangent coord (corrected)*/
  double imx_err, imy_err; /*   Error in peak centroid */
  double wimx, wimy;       /* Peak width in tangent plane coordinates */
  double wimx_err, wimy_err; /* Error in widths */
  double ra_orig, dec_orig;/* Peak centroid in sky coordinates (orig catalog)*/
  double ra_corr, dec_corr;/* Peak centroid in sky coordinates (corrected) */
  double ra_err, dec_err;  /*   Error in sky coordinates */
  double err_rad;          /* Error radius in degrees (0=fixed; -1=default) */
  double theta, phi;       /* Flight software theta and phi (deg) */
  double grmclon, grmclat; /* GRMC longitude and latitude (deg) */
  int method;              /* Detection method (bitwise OR of):
			      1 - cell detection
			      2 - PSF fit */
  double chi2;             /* Chi-square value */
  double dof;              /* Degrees of freedom (may be fractional) */
  double pcode;            /* Partial coding value from pcodefile image */
  double snr;              /* Signal to noise ratio */
  double centsnr;          /* Signal to noise calculated from centflux */

  double ra_per_imx,       /* Change in RA per unit IMX */
         dec_per_imx,      /*   "    "  DEC "   "   IMX */
         ra_per_imy,       /*   "    "  RA  "   "   IMY */
         dec_per_imy;      /*   "    "  DEC "   "   IMy */

  int psfcluster;          /* Set by psffit(); which PSF cluster it appears in */

  double contamflux;       /* Flux due to other sources at pos'n of this source */

  double wt_sum,  wt_sum_x,  wt_sum_y;  /* Weighted position */
  double max_snr, max_snr_x, max_snr_y; /* Max-SNR position */
  double first_x, first_y;              /* First-measured position */
  int old_status;                       /* Saved status, in-case of vector fail*/
};

struct wcs_coord {
  double crval1, cdelt1, crpix1;
  double crval2, cdelt2, crpix2, crota2;
  char ctype1[FLEN_CARD];
  char ctype2[FLEN_CARD];
  char coordtype[5];
};


struct detect_struct {
  struct image_struct *image;          /* Raw data: sky image */
  struct image_struct *mask, *bkgmask; /* Quality maks: source & bkg */
  struct image_struct *pcode;          /* Partial coding map */
  struct image_struct *npix;
  struct image_struct *bkgmap, *bkgvarmap;
  struct image_struct *snrmap;
  struct image_struct *found;
  struct source_struct *sources;
  int npixfound;             /* Number of detected pixels */
  int nsrcfound;             /* Number of detected sources */
  int nsrcmax;               /* Number of source entries allocated */
  int has_cel_coords;        /* Image has celestial coordinates? 1=yes 0=no */
  int has_tan_coords;        /* Image has tangent plane coordinates? 1=yes 0=no */
  int nimages;               /* Number of input images */
  int ngoodimages;           /* Number of selected images */
  int imgnum;                /* Current image number; 1 <= imgnum <= nimages */
  int nadjpix;               /* Number of required adjacent pixels */
  long int currow;           /* Current row number in output table */
  int nonullborder;          /* Remove sources on the borders? 1=yes, 0=no */
  int nwcs;                  /* Number of WCS coordinate systems */
  struct wcsprm *wcs;        /* WCSLIB coordinate parameters */
  int altwcs[27];            /* Pointers to alternate WCS structures */
  struct distortmap_struct *distmap; /* Distortion map structure (or 0 for none) */
  int dist_corr;             /* Correct for distortion? 1=yes; 0=no */
};
  

/* External function prototype declarations */

/* fileio.c */
extern int copycat(struct parm_struct *parms, struct detect_struct *detect);
extern
int imgout(struct parm_struct *parms, 
	   char *outfilename, int imgtype, int *ihdu,
	   struct image_struct *image, fitsfile *infileptr, 
	   int append, int *status);
extern int catout(struct parm_struct *parms, struct detect_struct *detect,
		  struct source_struct *sources, int nsources, 
		  fitsfile *infileptr);
extern int enlargecat(struct parm_struct *parms, struct detect_struct *detect, 
		      int nsources, int carryover);
extern
int print_source_table(char *sort_expr, 
		       struct detect_struct *detect,
		       struct source_struct *sources, int nsources,
		       int *status);


/* pixdetect.c */
extern int pixdetect(struct parm_struct *parms, struct detect_struct *detect);
extern int blanksources(struct parm_struct *parms, 
			struct detect_struct *detect);

/* psffit.c */
extern int psffit(struct parm_struct *parms, struct detect_struct *detect);

/* srcdetect.c */
extern struct source_struct *addsource(struct source_struct *sources, 
				       int *nsrc, int *nmax);
extern int delsource(struct source_struct *sources, int i,
		     int *nsrc, int *nmax);
extern struct source_struct *flooddetect(struct detect_struct *detect,
					 int srcradius);

/* wcs.c - alternate coordinate conversion routines */
extern
int coco(struct wcsprm *wcs, int altwcs[27], int from, int to, 
	 double in1, double in2, double *out1, double *out2,
	 int *status);


/* sort.c */
extern 
int parse_sort_expression(char *expr, 
			  struct source_struct *sources, int nsources,
			  int **result,
			  int *status);


/* mcholdc.c */
extern
int mcholdc(int n, double *a, double *p);
extern
int mcholsol(int n, double *a, double *p, 
	     double *b, double *x);


/* misc.c */
extern int est_cent_flux(struct source_struct *sources,
		  int nsources,
		  struct image_struct *detect_image,
		  struct image_struct *detect_bkgmap, 
		  struct image_struct *detect_bkgvarmap,
		  FLOAT nullval);
extern int est_vect_pos(struct source_struct *sources,
		 int nsources,
		 int parms_snrthresh,
		 int parms_vectpos,
		 struct wcsprm *wcs, int altwcs[27]);
extern int pix_to_img(struct source_struct *sources,
	       int nsources,
	       struct wcsprm *wcs, int altwcs[27],
	       int *status);
extern int name_new_sources(struct source_struct *sources,
		     int nsources,
		     char *parms_newsrcname,
		     int parms_newsrcind);
extern int img_to_sky(struct source_struct *sources, int nsources, 
		      struct detect_struct *detect,
		      struct image_struct *pcode,
		      double possyserr, int *status);
extern int read_app_keywords(fitsfile *file, 
		      char **inclist, int nlist,
		      struct keyword_struct *keys, 
		      int nmax, int *status);
extern int chat_app_keywords(struct keyword_struct *appkeys,
		      int nappkeys);
extern int img_hduclass_keep(int imgindex, fitsfile *imgfile,
			     char **hduclasses, int nhduclasses);
extern int read_distortion_map(fitsfile *imgfile,
			char *parms_distfile,
			struct distortmap_struct **detect_distmap);

extern int warn_low_snr(struct source_struct *sources,
		 int nsources);

extern int compare_img_systems(fitsfile *imgfile, int imgnum,
			int parms_outvector, char *parms_hduclas2,

			struct wcs_coord *tanxy, struct wcs_coord *defcoor,
			double *exposure, double *tstart, double *tstop,
			double *batz, double *oversampx, double *oversampy,
			int *nbatdets,

			struct wcs_coord *otanxy, struct wcs_coord *odefcoor,
			double *oexposure, double *otstart, double *otstop,
			double *obatz, double *ooversampx, double *ooversampy,
			int *onbatdets,

			struct wcsprm **detect_wcs, int detect_altwcs[27],
			int *detect_nwcs,
			struct distortmap_struct **detect_distmap,
			int *detect_dist_corr,
			char *parms_distfile,

			int *detect_has_cel_coords, int *detect_has_tan_coords,
			char *parms_ra_colname, char *parms_dec_colname);


/* init.c */
extern int batcelldetect_getpar(struct parm_struct *parms);
extern void banner(struct parm_struct *parms);
extern void summary(struct parm_struct *parms);

