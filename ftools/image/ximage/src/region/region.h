/*
 *  Modified version of regoin code from CFITSIO
 *  uses AST for coordinate conversions by way of wcscnv routines
 */

#include "ast.h"

/***************************************************************/
/*                   REGION STUFF                              */
/***************************************************************/

#define myPI  3.1415926535897932385

typedef enum {
   point_rgn,
   line_rgn,
   circle_rgn,
   annulus_rgn,
   ellipse_rgn,
   elliptannulus_rgn,
   box_rgn,
   rectangle_rgn,
   diamond_rgn,
   sector_rgn,
   poly_rgn
} shapeType;

typedef enum { pixel_fmt, degree_fmt, hhmmss_fmt } coordFmt;
   
typedef struct {
   char      sign;        /*  Include or exclude?        */
   shapeType shape;       /*  Shape of this region       */

   union {                /*  Parameters - In pixels     */

      /****   Generic Shape Data   ****/

      struct {
	 double p[8];        /*  Region parameters       */
	 double sinT, cosT;  /*  For rotated shapes      */
	 double a, b;        /*  Extra scratch area      */
      } gen;

      /****      Polygon Data      ****/

      struct {
         int    nPts;        /*  Number of Polygon pts   */
         double *Pts;        /*  Polygon points          */
	 double xmin,xmax;   /*  Polygon bounding box    */
	 double ymin,ymax;
      } poly;

   } param;

} RgnShape;

typedef struct {
   int exists;
   int cnvidx;
   double rot;
} RgnWCS;

typedef struct {
   int       nShapes;
   RgnShape  *Shapes;
   RgnWCS    *wcs;
} SAORegion;

#ifdef __cplusplus
extern "C" {
#endif

int  xffrrgn( const char *filename, RgnWCS *wcs, SAORegion **Rgn, int *status );
int  xfftrgn( double X, double Y, SAORegion *Rgn );
void xfffrgn( SAORegion *Rgn );

#ifdef __cplusplus
    }
#endif

#define xim_read_rgnfile xffrrgn
#define xim_in_region    xfftrgn
#define xim_free_region  xfffrgn

