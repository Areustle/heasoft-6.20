/***************************************************************/
/*                   REGION STUFF                              */
/***************************************************************/

#define myPI  3.1415926535897932385

typedef struct {
   int    exists;
   double xrefval, yrefval;
   double xrefpix, yrefpix;
   double xinc,    yinc;
   double rot;
   char   type[6];
} WCSdataX;

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
   int       nShapes;
   RgnShape  *Shapes;
   WCSdataX   wcs;
} SAORegion;

#ifdef __cplusplus
extern "C" {
#endif

int  fits_read_rgnfile( const char *filename, WCSdataX *wcs, SAORegion **Rgn, int *status );
int  fits_in_region( double X, double Y, SAORegion *Rgn );
void fits_free_region( SAORegion *Rgn );

#ifdef __cplusplus
    }
#endif

