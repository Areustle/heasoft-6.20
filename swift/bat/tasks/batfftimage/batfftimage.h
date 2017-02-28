/* 
 * Task to reconstruct sky image from BAT detector plane image
 *    - data definitions, constants and function declarations
 *
 *   C. Markwardt
 *   Dec 2003
 *
 * $Id: batfftimage.h,v 1.19 2006/04/06 21:49:45 craigm Exp $
 */

/* ----------------------------------------- Structure definitions */

/* Definition of main parameter interface structure */
struct parm_struct 
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];   /* Input detector plane image file name */
  char attitude[PIL_PATH_MAX]; /* Attitude file (or NONE) */
  char aperture[PIL_PATH_MAX]; /* Aperture file */
  char bkgfile[PIL_PATH_MAX];  /* Name of background file */
  char outfile[PIL_PATH_MAX];  /* Output sky image file name */
  char detmask[PIL_PATH_MAX];  /* Detector image mask file name */
  char teldef[PIL_PATH_MAX];   /* Telescope definition file */
  char bkgvarmap[PIL_PATH_MAX];/* Output variance map (or NONE or APPEND) */
  char signifmap[PIL_PATH_MAX];/* Output significance map (or NONE or APPEND) */
  char rowstr[PIL_LINESIZE];   /* Row selection string */
  char countscol[PIL_LINESIZE];/* Counts column */
  double maskoff[3];           /* Mask offset BAT_X,Y,Z */
  double srcpos[3];            /* Position of source in BAT_Z */
  double origin[3];            /* Origin for computing angles */
  int oversampx, oversampy;    /* Oversampling factor in X/Y */
  int rebalance, pcodemap, append;
  double tstart, tstop;        /* [s] Input image start/stop time (MET) */
  double time;                 /* [s] Attitude interpolation time (MET) */
  double exposure;             /* [s] Input image exposure time */
  int goodval;                 /* Good detmask value */

  char corrections[PIL_LINESIZE]; /* Any corrections to apply */
  int cautocollim;             /* Correct for autocollimation effect? 1=Y 0=N*/
  int pcodecorr;               /* Correct for partial coding? 1=Y 0=N */
  int handedness;              /* Handedness of image: 1=EAST=LEFT; 0=EAST=RIGHT (on a NORTH=UP image) */
  int cflatfield;              /* Correct for flat fielding? 1=Y 0=N */
  int ccosine;                 /* Correct for cosine effect only? 1=Y 0=N */
  double pcodethresh;          /* Partial coding threshold */
  int cmaskwt;                 /* Correct for mask weight technique? 1=Y 0=N */
  int cndet;                   /* Normalize by number of enabled dets?1=Y 0=N*/
  double maskwtswgain;         /* Mask weighting software gain factor;
			          maskwtnorm should be multiplied by (1+maskwtswgain) */
  int copyall;                 /* Copy all extra HDUs to output? 1=Y 0=N */
  int bkgvartype;              /* Bkg variance image type (see below) */
  int keepbits;                /* Number of bits to keep in output image pixel values */
};

#define HANDEDNESS_LEFT  1
#define HANDEDNESS_RIGHT 0

/* WCS coordinates for output image */
struct wcscoords_struct {
  double ra, dec;                    /* RA/DEC of BAT boresite */
  double pc1_1, pc1_2, pc2_1, pc2_2; /* Rotation matrix of RA/DEC axes
                                        w.r.t. IMX/Y axes */
  double rota;                       /* WCS CROTA2 keyword */
};

/* WCS summary information */
struct wcs_coord {
  double crval1, cdelt1, crpix1;
  double crval2, cdelt2, crpix2, crota2;
  char ctype1[FLEN_CARD];
  char ctype2[FLEN_CARD];
  char coordtype[5];
  double rescale_factor[2];
};

/* Tranformations between pixel and physical spaces */
/* From pixel to physical dimensions */
#define xform1_ph(xform,pix) ((xform).crval1 + (xform).cdelt1*(pix - (xform).crpix1))
#define xform2_ph(xform,pix) ((xform).crval2 + (xform).cdelt2*(pix - (xform).crpix2))
/* From physical dimensions to pixels*/
#define xform1_px(xform,phy) ((phy - (xform).crval1)/(xform).cdelt1 + (xform).crpix1)
#define xform2_px(xform,phy) ((phy - (xform).crval2)/(xform).cdelt2 + (xform).crpix2)


/* ----------------------------------------- Constants */
#define RTOD (180.0/M_PI)
#define FLUXMAP       0
#define PCODEMAP_FILE 1
#define PCODEMAP_APPEND 2
#define PCODEMAP_LAST 3
#define BKGVARMAP     4
#define STDDEVMAP     5
#define SIGNIFMAP     6
#define BKGMAP        7



/* ----------------------------------------- External functions */
/* wcs.c */
extern int write_imgkeys(fitsfile *imgfile, long naxes[2], 
		  struct parm_struct *parms,
		  struct image_struct *focal, struct image_struct *aperture,
		  struct batmaskplane_struct *mask, 
		  struct batdetplane_struct *detplane,
			 int *status);

int compute_crpixdelt(long naxes[2], 
		      struct parm_struct *parms,
		      struct image_struct *focal, struct image_struct *aperture,
		      struct batmaskplane_struct *mask, 
		      struct batdetplane_struct *detplane,
		      int handedness,
		      struct wcs_coord *wcs);


/* fileio.c */
extern struct image_struct *getimage(fitsfile *file, int imgnum, int hdutype,
				     char *countscol,
				     FLOAT *nullval, int *anynull, 
				     double *tstart, double *tstop, 
				     double *exposure,
				     int *status);


extern int locate_caldb_files(struct parm_struct *parms, fitsfile **dpifile);

extern int read_qmap(struct parm_struct *parms, struct image_struct **detmask);

extern struct image_struct *read_focal(fitsfile *dpifile, int imgnum, 
				fitsfile *bkgfile, int nbkgimages,
				int hdutype, int bkg_hdutype,
				char *countscol,
				struct parm_struct *parms,
				struct image_struct **pvardat,
			       int *anynull, int *status);

extern int image_out(fitsfile *outfile, fitsfile *dpifile,
	      struct image_struct *outimg, 
	      struct parm_struct *parms, int imgtype,
	      int imgnum, double mwfcorr,
	      struct image_struct *focal,
	      struct image_struct *aperture,
	      struct batmaskplane_struct *mask,
	      struct batdetplane_struct *detplane,
	     struct wcs_coord *wcs, FLOAT nullval, int ndet, int *ihdu);


/* corrections.c */
extern int image_stripbits(int keepbits,
			   struct image_struct *outimg,
			   struct image_struct *varimg,
			   int striptype, 
			   FLOAT nullval);

extern int image_finalize(struct parm_struct *parms,
		   struct image_struct *outimg, 
		   struct image_struct *pcodeimg,
		   int imgtype,
		   struct image_struct *focal,
		   struct image_struct *aperture,
		   struct batmaskplane_struct *mask,
		   struct batdetplane_struct *detplane,
		   struct wcs_coord *wcs, FLOAT nullval, int ndet,
 		  double *pmwfcorr);



