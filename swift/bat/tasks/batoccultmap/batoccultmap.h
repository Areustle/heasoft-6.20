/* 
 * batoccultmap
 * Definitions and declarations
 * 
 * Piotr Banat
 * Modified by Jay Cummings
 * Modified by Craig Markwardt
 *
 * $Id: batoccultmap.h,v 1.4 2007/03/22 16:04:19 craigm Exp $
 */

/* NEW WCS Library */
#include <wcslib/wcs.h>
#include <wcslib/wcshdr.h>

/* Distances */
/* Not used: see the parameter file !! */
/* #define earth_radius 6378.9 */
/* #define moon_radius 1738.0 */
/* #define sun_radius 695000.0 */
/* #define moon_dist 349521.1 */
/* #define sun_dist 148000000.0 */

#define rad2deg 0.01745329251994
#define deg2rad 57.29577951308232


/* Maximum number of *APP correction keywords to read */
#define MAX_APP_KEYS 32

struct parm_struct 
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];   /* Input image file name */
  char outfile[PIL_PATH_MAX];  /* Output  */
  double rearth;               /* [km] Earth radius */
  double atmdpth;              /* [km] depth of atmosphere (affects how big the Earth is) */
  double rmoon;                /* [arcmin] Moon radius */
  double rsun;                 /* [arcmin] Sun radius */
  double timesegerr;           /* ?? */
  char method[PIL_PATH_MAX];   /* position or time */
  char saofile[PIL_PATH_MAX]; /* attitude-orbit filter */
  char occultation[PIL_PATH_MAX];/* ?? */
  char gtifile[PIL_PATH_MAX];   /* GTI file name (or INFILE) */
  char constraints[PIL_PATH_MAX];    /* Constraints to operate on */
  int do_earth, do_moon, do_sun; /* Compute earth, moon, sun? (1=yes) */
  char multfiles[PIL_PATH_MAX];/* Files to multiply correction by */
  char divfiles[PIL_PATH_MAX]; /* Files to divide correction by */
  int alg;                     /* Algorithm to use? 1=image; 2=contour */
};

#define SRCNAME_LEN 20
#define ERR_RAD_COLNAME "ERR_RAD"
#define NAME_COLNAME "NAME"
#define SOURCEID_COLNAME "CATNUM"

#define GS_COLNAME "START"
#define GE_COLNAME "STOP"
#define RA_COLNAME "RA"
#define DEC_COLNAME "DEC"
#define TIME_COLNAME "TIME"
#define ALT_COLNAME "SAT_ALT"
#define ROLL_COLNAME "ROLL"
#define RA_MOON_COLNAME "MOON_RA"
#define DEC_MOON_COLNAME "MOON_DEC"
#define RA_SUN_COLNAME "SUN_RA"
#define DEC_SUN_COLNAME "SUN_DEC"
#define RA_EARTH_COLNAME "EARTH_RA"
#define DEC_EARTH_COLNAME "EARTH_DEC"


struct wcs_coord {
  double crval1, cdelt1, crpix1;
  double crval2, cdelt2, crpix2, crota2;
  char ctype1[FLEN_CARD];
  char ctype2[FLEN_CARD];
  char coordtype[5];
};



struct img_wcs_struct {
  struct image_struct *image, *mask; /* Raw data: sky image and quality mask */
  struct wcs_coord tanxy;    /* Tangent plane coordinate descriptors */
  struct wcs_coord defcoor;  /* Default image coordinate descriptors */
};



/* External functions */

/* correct.c */
extern int imgcorrect(struct parm_struct *parms,
		      struct image_struct *sky,
		      FLOAT nullval,
		      char *creator);

/* contour.c */
extern int poscircle(double ra, double dec, double theta, int nsegs,
		     double *radeccirc);


extern void bresline(struct image_struct *image, FLOAT value,
		     int x0, int y0, int x1, int y1, 
		     long int *npix, long int *isum, long int *jsum);

extern int bordcalc(struct image_struct *image,
		    struct wcsprm *wcsdata,
		    int *nborderpix0, double **bradec, double **bijpix);


extern int contcalc(struct image_struct *result, FLOAT value,
		    struct wcsprm *wcsdata,
		    double ra_body, double dec_body, double radius, 
		    int nsegs,
		    int nborderpix, double *bradec, double *bijpix);


/* image.c */
extern int allpixconvert(int nonulls, int nx, int ny,
			 struct image_struct *image, FLOAT nullval,
			 double *img,
			 struct wcsprm *wcsdata,
			 double *sin_rapix, double *cos_rapix,
			 double *sin_decpix, double *cos_decpix);

	       
extern int doimagemap(int nonulls, int ntseg, double *img, 
	       double *sin_rapix, double *cos_rapix,
	       double *sin_decpix, double *cos_decpix,
	       double *dt, double *alt, 
	       double *earth_ra, double *earth_dec,
	       double *moon_ra, double *moon_dec,
	       double *sun_ra, double *sun_dec,
	       double earth_radius, double sun_radius, double moon_radius,
	       double atmosphere,
	       int do_earth, int do_moon, int do_sun
		      );

extern int docontourmap(int ntseg, struct image_struct *sky,
		 int nborderpix, double *bradec, double *bijpix,
		 struct wcsprm *wcsdata,
		 double *dt, double *alt, 
		 double *earth_ra, double *earth_dec,
		 double *moon_ra, double *moon_dec,
		 double *sun_ra, double *sun_dec,
		 double earth_radius, double sun_radius, double moon_radius,
		 double atmosphere,
		 int do_earth, int do_moon, int do_sun
		 );
