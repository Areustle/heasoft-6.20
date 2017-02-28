#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <headas_rand.h> /* Need to use Mersenne Twister (MT) algorithm */
#include "ast.h"

/*
 *  For info on AST, see
 *  http://www.starlink.rl.ac.uk/star/docs/sun211.htx/sun211.html
 */

/*
 *  Remap functionality split between remapprep.c and remapcore.c
 *   to allow outside use of remap functionality.  remapcore.c is
 *   is completely independent of ximage, requiring only AST
 *
 *  Outside users of this code need only call the remapf routine
 */

/*
 * Prototypes
 */
static HDmt_state* ran_state = 0; /* Global seed state used by Mersenne Twister functions */
double mindary(int size, double *ary);
double maxdary(int size, double *ary);
double getRandomDouble();         /* Function to generate a random number between 0 and 1 */
double getRandomScale();          /* Limits Random number from 0 to .999999 */
int moffset(int szx, int i, int j);
void boxer(int, int, float *, float *, float *);
void initializeRandom();         /* Setup the seed for the MT number generator */
void destroyRandom();            /* Reset the seed for clean use later */
float sgarea(float, float, float, float, int, int);
void cdisro(float *, float, int, int, float *);
void remapconserve(AstMapping *, int, int, float *, int, float, int, int, 
                   float *, int *);

/*
 * remapf
 * Perform remapping operation on floating point image grid
 *
 * NOTE: The in/outwcs framesets must be set such that the 
 *  current frame corresponds to the image grids you
 *  wish to transform between  (Generally this means that
 *  the CURRENT frame is set to the one with Domain=GRID)
 *  If the in and out image sizes differ a new frame may need
 *  to be created which accounts for the scaling difference
 *  (use astWinMap to create)
 */

void
remapf(int mode, double *params, AstFrameSet *inwcs, float *inary, 
       int inszx, int inszy, float nulval, AstFrameSet *outwcs, 
       float *outary, int outszx, int outszy, int *status)
{
/*
 *  I mode     Corresponds to AST__* code that astResample recognizes
 *               as its "interp" argument, negative values are used
 *               for custom modes (i.e. mode=-1 is conserve counts mode)
 *  I params   Parameters passed to interpolation mode (defined in 
 *               astResample docs, except custom mode.  mode=-1
 *               params[0] is a type parameter, a value of 1
 *               specifies that the map is to be treated as an integer
 *               map (i.e. whole counts must be distributed), otherwise
 *               the pixel value is split into fractional float values
 *  I inwcs    Coordinates associated with input map
 *  I inary    Input image map 
 *  I inszx/y  Size of input map
 *  I nulval   Value to use as null value in maps
 *  I outwcs   Coordinates remapping to
 *  O outary   Output image map
 *  I outszx/y Size of output map
 *  O status   Error flag (0=OK)
 */

   int insvbase, insvcurr, outsvbase, outsvcurr;
   AstMapping *cvt;
   void (* finterp) ();
   int interp, nbad, isintmap, i;
   int lbnd_in[2], ubnd_in[2], lbnd_out[2], ubnd_out[2], maxpix;
   double tol;

   *status = 0;
   if ( mode == 0 ) {
      *status = -1;
      return;
   }
   /* Save frameset configuration */
   insvbase = astGetI(inwcs, "Base");
   insvcurr = astGetI(inwcs, "Current");
   outsvbase = astGetI(outwcs, "Base");
   outsvcurr = astGetI(outwcs, "Current");

   astBegin;
   cvt = astConvert(inwcs, outwcs, "SKY,OGRID");
   if ( !cvt ) {
      *status = -1;
      astEnd;
      return;
   }

   if ( inszx == outszx && inszy == outszy &&
        astIsAUnitMap(astGetMapping(cvt, AST__BASE, AST__CURRENT)) ) { 

      /* UnitMap indicates that no remapping is necessary, do simple copy */

      for ( i = 0; i < inszx*inszy; i++ ) {
         outary[i] = inary[i];
      }

   } else if ( mode == -1 ) {  /* CONSERVE counts interpolation method */
      isintmap = 0;
      if ( floor(params[0]) == 1 ) isintmap = 1;
      remapconserve(cvt, inszx, inszy, inary, isintmap, nulval, 
                    outszx, outszy, outary, status);
      
   } else {

      finterp = NULL;
      interp = mode;
      lbnd_in[0] = 1;
      lbnd_in[1] = 1;
      ubnd_in[0] = inszx;
      ubnd_in[1] = inszy;
      lbnd_out[0] = 1;
      lbnd_out[1] = 1;
      ubnd_out[0] = outszx;
      ubnd_out[1] = outszy;
      tol = 0;
      tol = 1.e-5;
      maxpix = 64;  /* faster value? */
      maxpix = inszx*inszy; /* Large value prevents linear approximation */
      nbad = astResampleF(cvt, 2, lbnd_in, ubnd_in, inary, NULL, interp, 
                          finterp, params, AST__USEBAD, tol, maxpix, nulval, 
                          2, lbnd_out, ubnd_out, lbnd_out, ubnd_out, outary, 
                          NULL);
   }
   astEnd;

   /* Restore frameset configuration */
   astSetI(inwcs, "Base", insvbase);
   astSetI(inwcs, "Current", insvcurr);
   astSetI(outwcs, "Base", outsvbase);
   astSetI(outwcs, "Current", outsvcurr);
}

void remapconserve(AstMapping *cvt, int inszx, int inszy, float *inary, 
                   int isintmap, float nulval, int outszx, int outszy, 
                   float *outary, int *status) {
/*
 *  Perform remap while conserving counts
 */
   int i, j, k, np, imin, imax, jmin, jmax, ii, jj;
   double xin[4], yin[4], xout[4], yout[4], z;
   float totprob, effval, x[4], y[4];
   int maxbuf, *idist, *jdist, offset;
   float goodprob, badprob, *overprob, *npix;
   int count, l, m;

   for ( k = 0; k < outszx*outszy; k++ ) {
      outary[k] = nulval;
   }
/*
 *  Go through each pixel in the input map
 *  Note that in the input maps are expected to be Fortran-ordered
 */
   for ( i = 1; i <= inszx; i++ ) {
      for ( j = 1; j <= inszy; j++ ) {

         if ( inary[moffset(inszx,i,j)] == nulval ) continue;
/*  
 *  Calculate the coordinates of pixel corners in GRID coordinates
 */
         xin[0] = i - 0.5;
         yin[0] = j + 0.5;
         xin[1] = i + 0.5;
         yin[1] = j + 0.5;
         xin[2] = i + 0.5;
         yin[2] = j - 0.5;
         xin[3] = i - 0.5;
         yin[3] = j - 0.5;
         astTran2(cvt, 4, xin, yin, 1, xout, yout);
/*  
 *  Loop over output pixels which could be affected
 */
         np = 0;
         badprob = 0.;
         goodprob = 0.;
         imin = floor( mindary(4, xout) + 0.5 );
         imax = floor( maxdary(4, xout) + 0.5 );
         jmin = floor( mindary(4, yout) + 0.5 );
         jmax = floor( maxdary(4, yout) + 0.5 );

	/*
         *  Boxer requires a clockwise polygon
         */
	 count = 0;
	 for ( k = 0; k < 4; k++ ) {
	    l = (k + 1) % 4;
	    m = (k + 2) % 4;
	    z = ( xout[l] - xout[k] ) * ( yout[m] - yout[l]);
	    z -= ( yout[l] - yout[k] ) * ( xout[m] - xout[l]);
	    if ( z < 0 ) { 
	       count--;
	    } else if ( z > 0 ) { 
	       count++;
	    }
	 }

         for ( k = 0; k < 4; k++ ) { /* Copy to float for boxer */
            x[k] = xout[k];
            y[k] = yout[k];
         }
	 if ( count > 0 ) {  /* Swap second point with last point */
	    x[1] = xout[3];  /* If counterclockwise */
	    y[1] = yout[3];
	    x[3] = xout[1];
	    y[3] = yout[1];
	 } 
/*
 *  Allocate space for arrays which track pixel distribution
 */ 
         maxbuf = ((imax-imin+1)*(jmax-jmin+1)+1);
         idist = (int *) calloc (maxbuf,sizeof(int));
         jdist = (int *) calloc (maxbuf,sizeof(int));
         overprob = (float *) calloc (maxbuf,sizeof(float));
         npix = (float *) calloc (maxbuf,sizeof(float));

         if ( !idist || !jdist || !overprob || !npix ) {
            fprintf(stderr, "remapconserve: Buffer allocation failed\n");
            *status = -1;
            return;
         }
         
         for ( ii = imin; ii <= imax; ii++ ) {
            for ( jj = jmin; jj <= jmax; jj++ ) {
/*  
 *  Call boxer to calculate overlap
 *  Keep track of values that fall outside image
 */
               np = np + 1;
               boxer(ii, jj, x, y, &overprob[np-1]);
               if ( ii < 1 || ii > outszx || jj < 1 || jj > outszy ) {
                  badprob += overprob[np-1];
                  np--;
               } else if ( overprob[np-1] > 0.0 ) {
                  goodprob += overprob[np-1];
                  idist[np-1] = ii;
                  jdist[np-1] = jj;
               } else {
                  np--;
               }
            }
         }
/*
 *  Distribute counts into affected pixels
 */
         if ( np > 0 ) {

            totprob = goodprob + badprob;

            /* Account for edge pixels by renormalizing */
            effval = (goodprob/totprob)*inary[moffset(inszx,i,j)];
            if ( isintmap ) effval = floor(effval);
            for ( k = 0; k < np; k++ ) {
               overprob[k] = overprob[k]/goodprob;
            }
            /* Initialize MT random number generator */
            initializeRandom();
            /* Run cdisro */
            cdisro(overprob, effval, np, isintmap, npix);
            /* Reset seed for MT random number generator */
            destroyRandom();
            for ( k = 0; k < np; k++ ) {
               ii = idist[k];
               jj = jdist[k];
               offset = moffset(outszx,ii,jj);
               if ( outary[offset] == nulval ) {
                  outary[offset] = npix[k];
               } else {
                  outary[offset] += npix[k];
               }
            }
         }
         free(idist);
         free(jdist);
         free(overprob);
         free(npix);
      }
   }
}

/*
 *  From HST drizzle code:
 *  http://www.stsci.edu/instruments/wfpc2/Wfpc2_driz/wfpc2_driz.html
 *
 *  BOXER -- compute area of box overlap (originally in Fortran)
 *
 *  Calculate the area common to input clockwise polygon x(n), y(n)
 *  with square (is-0.5, js-0.5) to (is+0.5, js+0.5)
 *  Note: convention is for integer value to be at pixel center
 *
 *  (Original boxer last modified 24-APR-1996 Richard Hook ECF)
 */

void boxer(int is, int js, float *x, float *y, float *darea) {
/*
 *  Local variables
 */
   float px[4], py[4], sum;
   int i;
/*
 *  Set up coords relative to unit square at origin
 */
   for ( i = 0; i < 4; i++ ) {
      px[i] = x[i] - (is-0.5);
      py[i] = y[i] - (js-0.5);
   }
/*
 *  For each line in the quadrilateral, calculate the area common to the
 *  unit square (allow negative area for subsequent 'vector' addition
 *  of subarea)
 */
   sum = 0.0;
   for ( i = 0; i < 3; i++ ) {
      sum = sum + sgarea(px[i], py[i], px[i+1], py[i+1], is, js);
   }
   sum = sum + sgarea(px[3], py[3], px[0], py[0], is, js);
   *darea = sum;
}

/*
 *  Used by BOXER
 *  Calculate area under a line segment within unit square at origin
 */
float sgarea(float x1, float y1, float x2, float y2, int is, int js) {
/*
 *  Local variables
 */
   float m, c, dx;
   float xlo, xhi, ylo, yhi, xtop;
   int negdx;

   dx = x2 - x1;
/*
 *  Trap vertical line
 */
   if ( dx == 0.0 ) return 0.0;
/*
 *  Order the two input points in x
 */
   if ( x1 < x2 ) {
      xlo = x1;
      xhi = x2;
   } else {
      xlo = x2;
      xhi = x1;
   }
/*
 *  And determine the bounds ignoring y for now
 */
   if ( xlo >= 1.0 ) return 0.0;
   if ( xhi <= 0.0 ) return 0.0;
   xlo = xlo > 0.0 ? xlo : 0.0;
   xhi = xhi < 1.0 ? xhi : 1.0;
/*
 *  Now look at y, basic info about the line y = mx + c
 */
   negdx = 0;
   if ( dx < 0.0 ) negdx = 1;
   m = (y2 - y1) / dx;
   c = y1 - m * x1;
   ylo = m * xlo + c;
   yhi = m * xhi + c;
/*
 *  Trap segment entirely below axis
 */
   if ( ylo <= 0.0 && yhi <= 0.0 ) return 0.0;
/*
 *  Adjust bounds if segment crosses axis
 *  (to exclude anything below axis)
 */
   if ( ylo < 0.0 ) {
      ylo = 0.0;
      xlo = -c/m;
   }
   if ( yhi < 0.0 ) {
      yhi = 0.0;
      xhi = -c/m;
   }
/*
 *  There are four possibilities: both y below 1, both y above 1
 *  and one of each
 */
   if ( ylo >= 1.0 && yhi >= 1.0 ) {
      /* Line segment is entirely above square */
      if ( negdx ) return (xlo - xhi);
      return (xhi - xlo);
   }
   if ( ylo <= 1.0 && yhi <= 1.0 ) {
      /* Segment entirely within square */
      if ( negdx ) return (0.5 * (xlo-xhi) * (yhi+ylo));
      return (0.5 * (xhi-xlo) * (yhi+ylo));
   }
/*
 *  Otherwise it must cross the top of the square
 */
   xtop = (1.0 - c)/m;
/*
 *  Check for a problem - note that this now has a small error
 *  margin included to avoid errors coming from numerical inaccuracy
 */
   if ( xtop < (xlo - 1.0e-5) || xtop > (xhi + 1.0e-5) ) {
      fprintf(stderr,  "Warning: box overlap calculation problem %d %d\n", is, js);
   }

   if ( ylo < 1.0 ) {
      if ( negdx ) return -(0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop);
      return (0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop);
   }
   if ( negdx ) return -(0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo);
   return (0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo);
}

void cdisro(float *pixpro, float pixval, int np, int isintmap, 
            float *npix) {
/*
 *  C version of DISRO
 *  Divide value up evenly for real case.  For integer,
 *  use probability for low counts
 *  Note: C version has no argument for the seed value
 *        A routine used to call disro should run srand with
 *        an appropriate seed value once before all the cdisro calls
 *
 *  I pixpro   (r)  probability that a photon will go in pixel i
 *  I pixval   (r)  pixel value to be distributed
 *  I np       (i)  # of pixels where photons have to be distributed
 *  I isintmap (l)  If true, integer map, else real map
 *  O npix     (r)  # of photons in pixel i
 */
   float sum, rrr;
   int i, j, n, npmu, isnegative, done;
 
   if ( !isintmap ) {
      sum = 0.;
      for ( i = 0; i < np-1; i++ ) {
         if ( isintmap ) {
            npix[i] = floor(pixval*pixpro[i] + 0.5);
         } else {
            npix[i] = pixval*pixpro[i];
         }
         sum += npix[i];
      }
      npix[np-1] = pixval - sum;
      return;
   }

   npix[0] = 0.;
   npmu = np - 1;
   for ( j = 0; j < np; j++ ) {
      npix[j] = 0.;
   }

   if ( pixval == 0. ) return; /* no need to do any calcs if zero */

   isnegative = 0;
   if ( pixval < 0. ) isnegative = 1;
   n = floor(fabs(pixval) + 0.5);

   if ( np == 1 ) { /* no need to divide among 1 pixel */
      if ( isnegative ) {
         npix[0] = -n; 
      } else {
         npix[0] = n; 
      }
      return;
   }

   sum = 0.;
   for ( i = 0; i < np; i++ ) {
      sum = sum + pixpro[i];
   }
   pixpro[0] = pixpro[0]/sum;
   for ( i = 1; i < np; i++ ) {
      pixpro[i] = pixpro[i-1] + pixpro[i]/sum;
   }
   for ( i = 1; i <= n; i++ ) {
      rrr = getRandomScale();
      j = -1;
      done = 0;
      while (!done) {
         j++;
         if ( j > np ) {
            fprintf(stderr, "cdisro: unexpected condition j>np %d>%d, r=%14.17f\n", j, np, rrr);
            break;
         }
         if ( rrr < pixpro[j] ) {
            npix[j]++;
            done = 1;
         }
      } 
   }
   if ( isnegative ) {
      for ( i = 0; i < np; i++ ) {
         npix[i] = -npix[i];
      }
   }
}

double maxdary(int size, double *ary) {
/*
 *  Find max in double array
 */
   int i;
   double maxval;
   
   maxval = ary[0];
   for ( i = 1; i < size; i++ ) {
      if ( ary[i] > maxval ) maxval = ary[i];
   }
   return maxval;
}

double mindary(int size, double *ary) {
/*
 *  Find min in double array
 */
   int i;
   double minval;
   
   minval = ary[0];
   for ( i = 1; i < size; i++ ) {
      if ( ary[i] < minval ) minval = ary[i];
   }
   return minval;
}
/*
 *  Get offset for array which represents 2d map
 *  Uses Fortran ordering
 */
int moffset(int szx, int i, int j) {
   return ((j-1)*szx + i - 1);
}

void initializeRandom(const unsigned long int seedIn)
{
  struct timeval tt;
  unsigned long int seed = 0; 
  
  if (0 == seedIn)
  {
    gettimeofday(&tt, NULL);
    seed = tt.tv_sec + tt.tv_usec;
  }
  else 
  {
    seed = seedIn;
  }
  ran_state = HDmt_srand(seed);
}

double getRandomDouble(void)
{
  return HDmt_drand(ran_state);
}

double getRandomScale(void)
{
  return 0.999999 * getRandomDouble();
}

void destroyRandom(void)
{ 
  HDmt_destroy_state(ran_state);
}

