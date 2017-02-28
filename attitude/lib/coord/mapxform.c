#include "mapxform.h"

/*
#define DEBUG
*/



/****************************************************************************
*****************************************************************************
* allocate memory for a MAPXFORM structure whose map has given dimensions
****************************************************************************/
MAPXFORM* allocateMapXform(int dimenx, int dimeny) {

MAPXFORM* map;
int i;

/*******************************
* allocate space for structure *
*******************************/
map=(MAPXFORM*)malloc(sizeof(MAPXFORM));

/*********************
* set map dimensions *
*********************/
map->dimenx=dimenx;
map->dimeny=dimeny;

/**************************
* allocate the map arrays *
**************************/
map->deltax    =(MAPXFORM_TYPE**)malloc(sizeof(MAPXFORM_TYPE*)*dimeny       );
map->deltax[0]=(MAPXFORM_TYPE* )malloc(sizeof(MAPXFORM_TYPE )*dimenx*dimeny);
for(i=1;i<dimeny;++i) {
    map->deltax[i] = map->deltax[i-1] + dimenx;
}

map->deltay    =(MAPXFORM_TYPE**)malloc(sizeof(MAPXFORM_TYPE*)*dimeny       );
map->deltay[0]=(MAPXFORM_TYPE* )malloc(sizeof(MAPXFORM_TYPE )*dimenx*dimeny);
for(i=1;i<dimeny;++i) {
    map->deltay[i] = map->deltay[i-1] + dimenx;
}

/************************************************
* initialize the transform to pixel coordinates 
* to the identity transform
************************************************/
map->to_pixels = allocateXform2d();
setXform2dToTranslation(map->to_pixels,0.0,0.0);

return(map);

} /* end of allocateMapXform function */

/****************************************************************************
*****************************************************************************
* allocate a new MAPXFORM structure with the same dimensions and
* axis scaling and offsets as an existing one
****************************************************************************/
MAPXFORM* allocateMapXformWithSameAxes(MAPXFORM* old) {

MAPXFORM* new;

new=allocateMapXform(old->dimenx,old->dimeny);

copyXform2d(new->to_pixels, old->to_pixels);


return(new);

} /* end of allocateMapXformWithSameAxies function */

/****************************************************************************
*****************************************************************************
* Create a new MAPXFORM which specified the same transform as the original
* but which occupies completely separate memory
****************************************************************************/
MAPXFORM* cloneMapXform(MAPXFORM* map) {

MAPXFORM* copy;
int size;

copy = allocateMapXformWithSameAxes(map);

size = sizeof(MAPXFORM_TYPE) * map->dimenx * map->dimeny;
memcpy(copy->deltax[0], map->deltax[0], size);
memcpy(copy->deltay[0], map->deltay[0], size);

return copy;

} /* end of cloneMapXform function */


/****************************************************************************
*****************************************************************************
* free memory for a MAPXFORM structure
****************************************************************************/
void destroyMapXform(MAPXFORM* map) {

free(map->deltax[0]);
free(map->deltay[0]);

free(map->deltax);
free(map->deltay);

free(map);

} /* end of destroyMapXform function */

/* Print a MAPXFORM structure to a stream. Since the data dimensions
   can be large, set max_rows = 0 to display all rows, or set max_rows
   >= 1 to display only a certain number of rows. Set max_columns
   analogously to display some or all columns. */
void printMapXform(MAPXFORM* map, FILE* stream, int max_columns, int max_rows)
{
  int m = 0;
  int i = 0;
  int j = 0;
  int i_max = map->dimenx;
  int j_max = map->dimeny;

  /* Print null warning if pointer is null. */

  if(map == NULL)
    {
      fprintf(stream, "  NULL MapXform\n");
      return;
    }

  /* Restrict the chunk of each 2D data array if the maxes
   * are set higher than 0 but smaller than the full dimensions. */

  if(max_columns > 0 && max_columns < map->dimenx)
    i_max = max_columns;
  else
    i_max = map->dimenx;

  if(max_rows > 0 && max_rows < map->dimeny)
    j_max = max_rows;
  else
    j_max = map->dimeny;

  /* Send structure contents to the stream. */
  
  fprintf(stream, "      MapXform structure contents:\n");
  fprintf(stream, "        dimenx: %d\n", map->dimenx);
  fprintf(stream, "        dimeny: %d\n", map->dimeny);
  fprintf(stream, "        to_pixels:\n");
  printXform2d(map->to_pixels, stream);
  
  for(m = 0; m < 2; m++) /* m = 0 for X map, m = 1 for Y map */
    {
      fprintf(stream, "        %s array:\n", (m == 0 ? "deltax" : "deltay"));
      if(i_max < map->dimenx)
	fprintf(stream, "          Not all columns are displayed.\n");    
      if(j_max < map->dimeny)
	fprintf(stream, "          Not all rows are displayed.\n");    

      fprintf(stream, "                  ");

      for(i = 0; i < i_max; i++)
	{
	  fprintf(stream, "     X%-4d", i);
	}
      fprintf(stream, "\n");

      for(j = j_max - 1; j >= 0; j--)
	{
	  fprintf(stream, "            Y%-4d ", j);
	  for(i = 0; i < i_max; i++)
	    {
	      fprintf(stream, " %9.4f", (m == 0 ? map->deltax[j][i] : map->deltay[j][i]));
	    }
	  fprintf(stream, "\n");
	}
      fprintf(stream, "\n");
    }
}


/***************************************************************************
****************************************************************************
* returns a pointer to the first pixel in the deltax array.
* this is useful for, say reading the array from a FITS file
***************************************************************************/
MAPXFORM_TYPE* getMapXformDeltaxBlock(MAPXFORM* map) {
    return(map->deltax[0]);
}



/***************************************************************************
****************************************************************************
* returns a pointer to the first pixel in the deltay array.
* this is useful for, say reading the array from a FITS file
***************************************************************************/
MAPXFORM_TYPE* getMapXformDeltayBlock(MAPXFORM* map) {
    return(map->deltay[0]);
}



/***************************************************************************
****************************************************************************
* set transformation from coordinate to be transformed to map pixel
* coordinates. "origin" is the position of the first map pixel in 
* coordinate space.
***************************************************************************/
void setMapXformAxes(MAPXFORM* map, double origin_x, double scale_x,
                                     double origin_y, double scale_y ) {

/*
map->origin_x = origin_x;
map->origin_y = origin_y;

map->scale_x = scale_x;
map->scale_y = scale_y;
*/

setXform2dToScaling(map->to_pixels, scale_x, scale_y, origin_x, origin_y);
applyTranslationToXform2d(map->to_pixels, -origin_x, -origin_y );

} /* end of setMapXformAxes function */


/***************************************************************************
****************************************************************************
* apply the map transform to (x0,y0) to produce (x1,y1).
* Returns 1 if the point was outside the map array boundaries and
* returns 0 otherwise. 
***************************************************************************/
int applyMapXform(MAPXFORM* map, double *x1, double *y1, 
                                 double  x0, double y0  ) {

double xpix, ypix;
int i,j;
double xhat, yhat;
int isOffArray=0;

MAPXFORM_TYPE **a;
double deltax, deltay;

#ifdef DEBUG
printf("applyMapXform: start map@%ld\n", (long)map);
#endif /* DEBUG */


/***********************************
* convert to map pixel coordinates *
***********************************/
/*
xpix=(x0 - map->origin_x) * map->scale_x;
ypix=(y0 - map->origin_y) * map->scale_y;
*/
applyXform2dToContinuousCoords(map->to_pixels, &xpix, &ypix, x0, y0);

i=(int)(xpix);
j=(int)(ypix);

#ifdef DEBUG
printf("applyMapXform: x0=%g y0=%g xpix=%g ypix=%g\n",x0,y0,xpix,ypix);

#endif /* DEBUG */

/***************************************************
* check if we are on the array - if not set the
* off array flag and adjust to the hearest border
***************************************************/
if(i<0) {
    i=0;
    isOffArray=1;
} 

if(i >= map->dimenx - 1) {
    i = map->dimenx - 2;
    isOffArray=1;
}

if(j<0) {
    j=0;
    isOffArray=1;
}

if(j >= map->dimeny - 1) {
    j = map->dimeny - 2;
    isOffArray=1;
}

/*******************************************
* get the coordinates within the map pixel *
*******************************************/
xhat=xpix-(double)i;
yhat=ypix-(double)j;

/*****************************************************
* bi-linear interpolate the deltax and deltay values *
*****************************************************/
a=map->deltax;
deltax = xhat*yhat*(a[j+1][i+1] - a[j+1][i] - a[j][i+1] + a[j][i])
        +     xhat*(a[j  ][i+1] - a[j  ][i] )
        +     yhat*(a[j+1][i  ] - a[j  ][i] )
        +           a[j  ][i  ];

a=map->deltay;
deltay = xhat*yhat*(a[j+1][i+1]-a[j+1][i]-a[j][i+1]+a[j][i])
        +     xhat*(a[j  ][i+1]-a[j  ][i])
        +     yhat*(a[j+1][i  ]-a[j  ][i])
        +           a[j  ][i  ];

#ifdef DEBUG
printf("applyMapXform: i=%d j=%d xhat=%g yhat=%g yhat-1=%g\n",
       i,j,xhat,yhat, yhat-1.0);



printf("applyMapXform: deltax[j+1][i]=%g deltax[j+1][i+1]=%g\n",
       map->deltax[j+1][i],map->deltax[j+1][i+1]);

printf("applyMapXform: deltax[j][i]=%g deltax[j][i+1]=%g\n",
       map->deltax[j][i],map->deltax[j][i+1]);

printf("applyMapXform: deltax=%g\n",deltax);


printf("applyMapXform: deltay=%g\n",deltay);


#endif /* DEBUG */


/*******************
* apply the shifts *
*******************/
*x1=x0+deltax;
*y1=y0+deltay;

return(isOffArray);

} /* end of applyMapXform */

/***************************************************************************
****************************************************************************
* Take a mapped transform "old" and apply a linear transform "trans"
* and produce a mapped transform "new", such that
* trans(old(x,y)) = new(trans(x,y)). In otherwords, "new" applies
* to the linear transformed space instead of to the pre-linear transform space.
* The transform is done in place - i.e. the original MAPXFORM is modified
******************************************************************************/
void applyXform2dToMapXform(MAPXFORM* map, XFORM2D* trans) {

XFORM2D* inverse;
XFORM2D* to_pixels;

int i,j;
double dx0, dy0;
double dx1, dy1;

/******************************
* invert the linear transform *
******************************/
inverse=allocateXform2d();
invertXform2d(inverse, trans);

/***************************************************************
* set the to_pixels transform in the new map so that
* the pixels in the new and old maps correspond. 
* that means we preceed the old to_pixels transform with the
* inverse of the linear transform
***************************************************************/
to_pixels = allocateXform2d();
combineXform2ds(to_pixels, inverse, map->to_pixels);

destroyXform2d(inverse);
destroyXform2d(map->to_pixels);
map->to_pixels=to_pixels;


/*******************************
* loop over all the map pixels *
*******************************/
for(j=0; j<map->dimeny; ++j) {
    for(i=0; i<map->dimenx; ++i) {

        dx0 = map->deltax[j][i];
        dy0 = map->deltay[j][i];
	
	applyXform2dToOffset(trans, &dx1, &dy1, dx0, dy0);
	
	map->deltax[j][i] = dx1;
	map->deltay[j][i] = dy1;
    }
} /* end of loop over pixels */

} /* end of applyXform2dToMapXform function */

/****************************************************************************
* Given a set of offsets at randomly sampled points, this function fills
* in the deltax and deltay arrays for a MAPXFORM so as to approximate as set
* of randomly spaced offsets. Inverse distance weighting is used to interpolate
* the unevenly spaced input data.
****************************************************************************/
void setMapXformFromListOfPoints(MAPXFORM* map, double* x, double* y,
                                           double* deltax, double* deltay,
                                           int npoints ) {
int point;
int i,j;
double minx, miny;
double maxx, maxy;

double x0, y0;
double dx, dy;
double dist2;
double weight;

double sumx, sumy;
double norm;

double scalex, scaley;

XFORM2D* to_coord;

#ifdef DEBUG
printf("setMapXformFromListOfPoints: start\n");
#endif
/*****************************************************
* first get the bounding box of the original points
* so that we can establish the to_pixels transform
*****************************************************/
minx=maxx=x[0];
miny=maxy=y[0];
for(point=1; point<npoints; ++point) {

    if(x[point] < minx) minx = x[point];
    if(x[point] > maxx) maxx = x[point];

    if(y[point] < miny) miny = y[point];
    if(y[point] > maxy) maxy = y[point];
}


scalex = (maxx - minx)/(double)(map->dimenx-1);
scaley = (maxy - miny)/(double)(map->dimeny-1);

setMapXformAxes(map, minx, 1./scalex, miny, 1./scaley );

#ifdef DEBUG
printf("minx=%g maxx=%g miny=%g maxy=%g\n", minx, maxx, miny, maxy);
#endif

/************************************************************
* now loop over all the pixels and interpolate to the grid. *
************************************************************/
to_coord = allocateXform2d();
invertXform2d(to_coord, map->to_pixels);

#ifdef DEBUG
applyXform2dToContinuousCoords(to_coord, &x0, &y0, 0.0, 0.0);
printf("bottom left corner %g %g\n", x0, y0);

applyXform2dToContinuousCoords(to_coord, &x0, &y0, (double)(map->dimenx-1),
                                                   (double)(map->dimeny-1) );
printf("top right corner %g %g\n", x0, y0);
#endif 

for(j=0; j<map->dimeny; ++j) {
    for(i=0; i<map->dimenx; ++i) {

        /******************************************************
	* find the location of this pixel in coordinate space *
	******************************************************/
        applyXform2dToContinuousCoords(to_coord, &x0, &y0, (double)i, (double)j);
/*
printf("x0=%g y0=%g\n", x0, y0);
*/
	/**********************************************
	* do inverse distance weighting interpolation *
	**********************************************/
	sumx=0.0;
	sumy=0.0;
	norm=0.0;
	for(point=0; point<npoints; ++point) {

	    /**********************************************
	    * calculate the distance to the current point *
	    **********************************************/
	    dx = x[point]-x0;
	    dy = y[point]-y0;
	    dist2=dx*dx+dy*dy;

	    if(dist2==0.0) {
	        /********************************
		* we're right on top of a point *
		********************************/
                sumx = deltax[point];
		sumy = deltay[point];
		norm=1.0;
		break;
	    }



	    /**********************
	    * accumulate the sums *
	    **********************/
	    weight=1./(dist2*dist2);
	    sumx += deltax[point]*weight;
	    sumy += deltay[point]*weight;
	    norm += weight;
/*
if(weight>1e-3)
printf("    point=%d x=%g y=%g dist2=%g weight=%g\n",
       point, x[point], y[point], dist2, weight);
*/
        } /* end of loop over points */
	
	/**********************************
	* calculate the new offset values *
	**********************************/
	map->deltax[j][i] = sumx/norm;
	map->deltay[j][i] = sumy/norm;


#ifdef DEBUG
printf("i=%d j=%d deltax=%g deltay=%g\n",
	      i,j,map->deltax[j][i], map->deltay[j][i]);
#endif

    
    }
} /* end of loop over pixels */

destroyXform2d(to_coord);

} /* end of setMapXformFromListOfPoints function */

/********************************************************************************
* Given a MAPXform set another MAPXFORm to be the inverse. Note that this
* inversion introduces innaccuracy from interpolation.
********************************************************************************/
void invertMapXform(MAPXFORM* inverse, MAPXFORM* map) {

double* x;
double* y;
double* deltax;
double* deltay;

int npoints;
int i,j;
int point;

double x0, y0;

XFORM2D* to_coord;

#ifdef DEBUG
printf("invertMapXform: start\n");
#endif

/**********************************
* allocate some temporary storage *
**********************************/
npoints = map->dimenx * map->dimeny;
x = (double*)malloc(sizeof(double)*npoints);
y = (double*)malloc(sizeof(double)*npoints);

deltax = (double*)malloc(sizeof(double)*npoints);
deltay = (double*)malloc(sizeof(double)*npoints);

/****************************************************
* get the transform to coordinate space from pixels *
****************************************************/
to_coord = allocateXform2d();
invertXform2d(to_coord, map->to_pixels);

/****************************************************************
* loop over all the pixels in the map and generate a list
* points from which we will interpolate the inverse transform.
****************************************************************/
point=0;
for(j=0; j<map->dimeny; ++j) {
    for(i=0; i<map->dimenx; ++i) {

        /******************************************************
	* find the location of this pixel in coordinate space *
	******************************************************/
        applyXform2dToContinuousCoords(to_coord, &x0, &y0, (double)i, (double)j);
	
	x[point] = x0 + map->deltax[j][i];
	y[point] = y0 + map->deltay[j][i];
    
	deltax[point] = - map->deltax[j][i];
	deltay[point] = - map->deltay[j][i];
	
/*
printf("point=%d deltax=%g deltay=%g\n", point, deltax[point], deltay[point]);
*/
        ++point;
    
    }
}

/****************************************
* now interpolate the inverse transform *
****************************************/
setMapXformFromListOfPoints(inverse, x,  y, deltax, deltay, npoints);

/**********
* cleanup *
**********/
destroyXform2d(to_coord);


} /* end of invertMapXform function */



