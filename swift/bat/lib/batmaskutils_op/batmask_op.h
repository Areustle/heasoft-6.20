/* Definition of terms:

   centpos - "Design center of mask" - the position in cm of the
             center of the BAT mask, as designed.

	     The design center refers to the corner which joins mask
	     cells XXXXXX, and along the Z direction is the top of
	     the lead tile

   meanpos - "Mean center of mask" - the position offset in cm of the
              actual center of the BAT mask with respect to the design
              center.  This is a small offset in case of a shifting
              during integration, launch or flight.


   psi - "Orientation of mask plane w.r.t design plane" - Euler angle
              rotations of mean mask array about the mean center, to
              reflect small tilts or misalignments as found in flight

              Euler angle rotations of angle psi(2) about +z, followed
	      by psi(1) about +y, followed by psi(0) about +x.

   awarp, bwarp, cwarp - "Coefficients of mask warp" - 
      ;; Warp of mask array with respect to the mean plane of the
      ;; array.  maskwarp(0) is the coefficient of the term quadratic
      ;; in XMASK, maskwarp(1) is the coefficient of the term
      ;; quadratic in YMASK, and maskwarp(2) is the cross-term

 * $Id: batmask_op.h,v 1.1 2003/10/28 19:14:08 krimm Exp $
 *
 * 02 Jan 2003 - Signature of maskwtimg() changed because of 2D array
 *               problems (CM)
 * 29 Jan 2003 - Change centpos definition to top of mask tiles

*/

struct batmaskplane_struct {
  double centpos[3];  /* Design center */
  double meanpos[3];  /* Mean center offset */

  double psi[3];      /* Orientation of mean mask plane about design plane */

  double awarp,bwarp,cwarp;     /* Mask warp coefficients */

  double cellsize[3]; /* Size of mask cell in cm */
  double cell0[2];    /* Position of 0,0 cell in aperature array */
  
  int ncells[2];     /* Number of cells in aperature array */
  int nfilled;       /* Number of filled aperature cells */
  int nopen;         /* Number of open aperture cells */
  int *aperture;     /* Aperture array */
};

/* Define whether the mask weighting is used for forward of backward
   projection.  This controls how the weighting corrections are
   applied.  With forward projection, one wants to enhance the
   weighting where the array counts should be high; for back
   projection, one wants to deweight the portions of the array which
   have high counts. */
#define RAYTRACE_FORW 1
#define RAYTRACE_BACK 0

struct batmaskcorrections_struct {
  double subpixsize;
  int ccosine;       /* Apply cos(theta) correction (1=yes; 0=no)*/
  int cside;         /* Compute side illumination XXX not implemented*/
  int copaque;       /* Assume mask elements are perfectly opaque 
			XXX do not use */
  double opaquethresh;/* Transmission below this threshold is considered opaque */
  int nmasklayers;   /* Number of mask sublayers - XXX not user-settable*/
  int rebalance;     /* Rebalance the image? (1=yes; 0=no) */
  int cpcode;        /* Apply partial coding correction (1=yes; 0=no)*/
  int crsquare;      /* Apply 1/r^2 correction (1=yes; 0=no) */
  int cflatfield;    /* Apply Tueller/Hullinger flatfield correction (1=yes;0=no)*/
  int cnbatdets;     /* Correct for number of detectors? (1=yes; 0=no)*/
  int direction;     /* Direction of projection (1=forward; 0=backward) */
  int cunbalanced;   /* Make unbalanced map? (wts from 0 to 1) (1=yes; 0=no) */
  int cinc_edge;      /* Include exposure of edges in weight (1=yes; 0=no) */
  double eff_edge;   /* Effective depth of edge; what part of 2mm edge to include (0.15 to 2.0) */
};

/* Pre-declare the batdetplane_struct so that the following does not
   cause a syntax error. */
struct batdetplane_struct;

struct blockdettop_struct {
       double centpos[3]; /* Design center */
       double meanpos[3]; /* Mean center offset */
       double cellsize[3];
       double cell0[3];
       int ncells[2];
};
			
extern int forwdettop(double srcpos[3],  struct blockdettop_struct *dettopplane,
                     double xmin, double xmax, double ymin, double ymax,
                     double zproj,
                     double *poly, int nmaxpoly,
                     struct batmaskcorrections_struct *corrs);


extern int maskwtimg(double srcpos[3], 
		   struct batmaskplane_struct *mask, 
		   struct batdetplane_struct *detplane,
		   struct batmaskcorrections_struct *corrs,
		   double *maskimg, int nimgx, int nimgy);

/* detmask.c */
extern void mkdetgaps_val(double *detimg, int nx, int ny, float val);
extern void mkdetgaps_intval(int *detimg, int nx, int ny, int val);
extern int *read_detmask(char *filename, long int axes[2], 
			 int goodval, int *status);

extern int forwmask(double srcpos[3], struct batmaskplane_struct *mask, 
		    double xmin, double xmax, double ymin, double ymax, 
		    double dz, double zdet, 
		    double *poly, double *weights, int nmaxpoly, 
		    struct batmaskcorrections_struct *corrs);

extern double maskwtcorrs(double detpos[3], double srcpos[3],
		   struct batmaskcorrections_struct *corrs);

int est_masklayers(double *srcpos, double *cellsize, 
		   struct batmaskcorrections_struct *corrs);

int bat_coordtype(char *coords, char coord_name[20]);
int bat_coordver(int coord_type, 
		 double srcpos[3], double lonlat[2], double *distance);

#ifdef TDOUBLE
int mask_image(fitsfile *maskfile, struct batmaskplane_struct *mask, 
	       int *status);

int mask_writekey(fitsfile *file, struct batmaskplane_struct *mask, 
		  int *status);
int mask_readkey(fitsfile *file, struct batmaskplane_struct *mask, 
		  int *status);
#endif

/* Coordinate types used in batmaskwtimg and batmaskwtevt */
#define BCCARTESIAN 1 /* Cartesian coordinate system: BAT_X/Y/Z (cm) */
#define BCUNIT      2 /* Cartesian unit vector: BAT_X/Y/Z */
#define BCFSWLONLAT 3 /* Flight software theta / phi (degree) */
#define BCGRMC      4 /* GRMC longitude and latitude (degree) */
#define BCSKY       5 /* Sky coordinates RA/DEC (degree) */
#define BCTANGENT   6 /* Tangent-plane coordinates IMX/IMY */
