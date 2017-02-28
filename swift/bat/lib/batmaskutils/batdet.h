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

 * $Id: batdet.h,v 1.12 2005/09/23 04:33:46 craigm Exp $
 *
 * 29 Jan 2003 - Change centpos definition to top plane of detector

*/

struct batdetplane_struct {
  double centpos[3]; /* Design center */
  double meanpos[3]; /* Mean center offset */
  
  double cellsize[3];
  double detsize[3];
  double cell0[3];
  int ncells[2];
};

struct batmaskcorrections_struct;  /* Forward declaration */

#define DETFLOAT float
#define DETMAX   1

#define BLOCKXCELLS 36
#define BLOCKYCELLS 88
#define DETPLANEXCELLS 286
#define DETPLANEYCELLS 173

extern int dettop_shad(double x[4], double y[4],
		DETFLOAT *detplane, int nx, int ny,
		double cellsize[2], double cell0[2]);

extern int detbresimg(double x[4], double y[4], 
		     DETFLOAT *detplane, int nx, int ny, 
		     double cellsize[2], double cell0[2],
		     DETFLOAT weight);

int fastcell(double x[4], double y[4], 
	     DETFLOAT *detimg, int nx, int ny, 
	     struct batdetplane_struct *detplane,
	     double cell0[2], DETFLOAT weight);

double maskaddsub(struct batdetplane_struct *detplane, 
		  struct batmaskcorrections_struct *corrs,
		  DETFLOAT *arr, int stride, double prevsum);

#ifdef CFITSIO_VERSION
int detplane_writekey(fitsfile *file, struct batdetplane_struct *detplane, 
		      int *status);
int detplane_readkey(fitsfile *file, struct batdetplane_struct *detplane, 
		     int *status);
#endif
