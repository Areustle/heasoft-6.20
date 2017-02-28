/* Interface routines to go from Fortran to the cfitsio region-handling routines */

#include <math.h>
#include "region.h"
#include "cfortran.h"

/* function definition */

int dump_saoregion(SAORegion *reg);
int select_region(double X, double Y, int ireg);
int get_region_bounding_box(int *bbox);
int select_region_setup(char *filename, int ireg, double refval[2], double refpix[2], double inc[2], double rot, char *type, double *area, int *bbox);
int read_dskey_region(char *filename, char *eventname, SAORegion **dsRgn, int *status);
int combine_all_regions(char *filename, char *eventname);
int merge_regions(SAORegion *reg1, SAORegion *reg2, SAORegion **retreg, int *status);
int copy_shape(RgnShape *inshape, RgnShape *outshape);
int equal_shape(RgnShape *shape1, RgnShape *shape2);
int number_shapes();
void get_shape_data(int iShape, int *sign, char *shape, int *comp, int *npoints, double *points);

/* cfortran definition for functions that have to be visible from Fortran */

FCALLSCFUN3(INT,select_region,SELECT_REGION,select_region,DOUBLE,DOUBLE,INT)
FCALLSCFUN9(INT,select_region_setup,SELECT_REGION_SETUP,select_region_setup,STRING,INT,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLE,STRING,PDOUBLE,INTV)
FCALLSCFUN2(INT,combine_all_regions,COMBINE_ALL_REGIONS,combine_all_regions,STRING,STRING)
FCALLSCFUN0(INT,number_shapes,NUMBER_SHAPES,number_shapes)
FCALLSCSUB6(get_shape_data,GET_SHAPE_DATA,get_shape_data,INT,PINT,PSTRING,PINT,PINT,DOUBLEV) 

/* Global variables */

#define MAXREG  10000
#define MAXCOLS 100
#define FLAG -999
#define NSHAPES 20
#define MAXPARS 11

SAORegion *aRgn;


/******************************************************************************/
int dump_saoregion(SAORegion *reg)
{

  int i, j;

  if (!reg) return(0);

  /* write out the information we have accumulated */

  printf("Number of regions = %d\n", reg->nShapes);
  for (i=0; i<reg->nShapes; i++) {
    printf("sign = %d, shape = %d, comp = %d\n", reg->Shapes[i].sign, reg->Shapes[i].shape, reg->Shapes[i].comp);
    if ( reg->Shapes[i].shape == poly_rgn ) {
      printf("nPts = %d\n", reg->Shapes[i].param.poly.nPts);
      for (j=0; j<reg->Shapes[i].param.poly.nPts; j++) printf("%f ", reg->Shapes[i].param.poly.Pts[j]);
      printf("\n");
    } else {
      for (j=0; j<11; j++) printf("%f ", reg->Shapes[i].param.gen.p[j]);
      printf("\n");
      printf("%f %f %f %f\n", reg->Shapes[i].param.gen.sinT, 
	     reg->Shapes[i].param.gen.cosT, reg->Shapes[i].param.gen.a, 
	     reg->Shapes[i].param.gen.b); 
    }
    printf("%f %f %f %f\n", reg->Shapes[i].xmin, reg->Shapes[i].xmax, reg->Shapes[i].ymin, reg->Shapes[i].ymax);
  }

  return(0);
}

/******************************************************************************/
int select_region(double X, double Y, int ireg)
{

  int in_region;

  in_region = fits_in_region(X, Y, aRgn);

  return(in_region);

}

/******************************************************************************/
int get_region_bounding_box(int *bbox)
{
  int i;
  double xlow, xhigh, ylow, yhigh;
  RgnShape *tmpShape;

  if (!aRgn) return(0);

  /* if the first region is an exclude then leave the bounding box values unchanged */
  if ( !aRgn->Shapes[0].sign ) return(0);

  /* set the bounding box to the part of the first shape which lies within the
     input bounding box */

  tmpShape = &aRgn->Shapes[0];

  xlow  = tmpShape->xmin;
  ylow  = tmpShape->ymin;
  xhigh = tmpShape->xmax;
  yhigh = tmpShape->ymax;
  if ( xlow < bbox[0] ) xlow = bbox[0];
  if ( ylow < bbox[1] ) ylow = bbox[1];
  if ( xhigh > bbox[2] ) xhigh = bbox[2];
  if ( yhigh > bbox[3] ) yhigh = bbox[3];

  /* loop over other shapes extending the bounding box if necessary */

  for (i=1; i<aRgn->nShapes; i++) {

    tmpShape = &aRgn->Shapes[i];

    if ( tmpShape->xmin <= tmpShape->xmax && tmpShape->ymin <= tmpShape->ymax &&
	 tmpShape->sign ) {

      if ( tmpShape->xmin < xlow ) xlow = tmpShape->xmin;
      if ( tmpShape->xmax > xhigh ) xhigh = tmpShape->xmax;
      if ( tmpShape->ymin < ylow ) ylow = tmpShape->ymin;
      if ( tmpShape->ymax > yhigh ) yhigh = tmpShape->ymax;

    }

  }

  bbox[0] = (int)(floor(xlow));
  bbox[1] = (int)(floor(ylow));
  bbox[2] = (int)(ceil(xhigh));
  bbox[3] = (int)(ceil(yhigh));

  return(0);

}

/******************************************************************************/
int select_region_setup(char *filename, int ireg, double refval[2], double refpix[2], double inc[2], double rot, char *type, double *area, int *bbox)
{
  /* set up the region structure */

  int status;
  int i, j;
  double x, y;
  WCSdata wcs;

  *area = 0.0;
  status = 0;

  wcs.exists = 0;
  if ( index(type,'-') != NULL ) {
    wcs.xrefval = refval[0];
    wcs.yrefval = refval[1];
    wcs.xrefpix = refpix[0];
    wcs.yrefpix = refpix[1];
    wcs.xinc = inc[0];
    wcs.yinc = inc[1];
    wcs.rot = rot;
    strcpy(wcs.type, type+4);
    wcs.exists = 1;
  }

  fits_read_rgnfile(filename, &wcs, &aRgn, &status);

  if ( status !=0 || aRgn == 0 ) {
    printf("Warning: No region information read from %s, status = %d\n", filename, status);
    return(1);
  }

  /* debug */
  /*    status = dump_saoregion(aRgn); */

  status = get_region_bounding_box(bbox);

  /* calculate the area included by the selection region */

  for (i=bbox[0];i<=bbox[2];i++) {
    for (j=bbox[1];j<=bbox[3];j++) {
      x = (double) i;
      y = (double) j;
      if ( fits_in_region(x, y, aRgn) ) {
	*area += 1.0;
      }
    }
  }

  return(0);
}

/******************************************************************************/
int read_dskey_region(char *filename, char *eventname, SAORegion **Rgn, int *status)
{
  /* read any region descriptors in the data subspace keywords */

  int nfound, key, i, icomp, done, exclude, nparam;
  fitsfile *fptr;
  SAORegion *dsRgn=NULL;
  RgnShape *newShape, *tmpShape;
  char *keyresult[MAXCOLS];
  char *instring;
  char *regstr=NULL, *curstr, *parstr;

  /* set up output region structure */

  dsRgn = (SAORegion *)malloc( sizeof(SAORegion) );
  if( ! dsRgn ) {
    printf("Couldn't allocate memory to hold DSS Region contents.");
    return(-1);
  }
  dsRgn->nShapes    =    0;
  dsRgn->Shapes     = NULL;

  /* open the file and move to the eventname extension */

  if ( ffopen(&fptr, filename, READONLY, status) ) {
    printf("Could not open event file : %s\n status = %d\n", filename, *status);
    return(*status);
  }

  if ( ffmnhd(fptr, BINARY_TBL, eventname, 1, status) ) {
    printf("Could not move to %s extension.\n status = %d\n", eventname, *status);
    goto error;
  }

  /* read the DSTYP# keywords */

  for (i=0; i<MAXCOLS; i++) keyresult[i] = (char *) malloc ( (FLEN_VALUE+1)*sizeof(char) );
  if ( ffgkns(fptr, "DSTYP", 1, MAXCOLS, keyresult, &nfound, status) ) {
    printf("Failed to read DSTYP keywords\n");
    goto error;
  }

  /* look for a keyword SKY(X,Y) */

  key = -1;
  for (i=0; i<nfound; i++) {
    if ( !strcasecmp(keyresult[i], "SKY(X,Y)") ) key = i;
  }
  if ( key == -1 ) goto error;

  /* read the appropriate DSVAL */

  ffgkns(fptr, "DSVAL", key, 1, keyresult, &nfound, status);
  if ( *status || nfound == 0 ) {
    printf("Failed to read DSVAL keyword\n");
    goto error;
  }

  instring = (char *) malloc ((strlen(keyresult[0])+1)*sizeof(char));
  strcpy(instring, keyresult[0]);

  /* convert the region string to blank de-limited by removing parentheses and commas
     and replace any ! by - */

  for (i=0; i<strlen(instring); i++) {
    if ( instring[i] == '(' || instring[i] == ')' || instring[i] == ',' ) instring[i] = ' ';
    if ( instring[i] == '!' ) instring[i] = '-';
  }

  /* loop through the string splitting out individual region specifications */

  icomp = 0;
  done = 0;
  while ( !done ) {

    for (i=0; i<(strlen(instring)-1); i++) {
      if ( instring[i] == '&' ) break;
      if ( instring[i] == '|') {
	icomp++;
	break;
      }
    }
    strncpy(regstr, instring, i);
    regstr[i] = '\0';
    instring += i;
    if ( i == (strlen(instring)-1) ) done = 1;
    while (*regstr == ' ') regstr++;

    /* get memory for this region */

    if( !(dsRgn->nShapes % 10) ) {
      if( dsRgn->Shapes )
	tmpShape = (RgnShape *) realloc( dsRgn->Shapes,
					(10+dsRgn->nShapes)
					* sizeof(RgnShape) );
      else
	tmpShape = (RgnShape *) malloc( 10 * sizeof(RgnShape) );
      if( tmpShape ) {
	dsRgn->Shapes = tmpShape;
      } else {
	printf( "Failed to allocate memory for Region data");
	*status = -1;
	goto error;
      }
    }

    newShape        = &dsRgn->Shapes[dsRgn->nShapes++];
    newShape->sign  = 1;
    newShape->shape = point_rgn;
    for (i=0; i<MAXPARS; i++) newShape->param.gen.p[i] = 0.0;
    newShape->param.gen.a = 0.0;
    newShape->param.gen.b = 0.0;
    newShape->param.gen.sinT = 0.0;
    newShape->param.gen.cosT = 0.0;

  /* check for special characters at the beginning of the region name */

    exclude = 0;
    if ( regstr[0] == '+' ) regstr++;
    if ( regstr[0] == '-' ) {
      exclude = 1;
      regstr++;
    }

  /* identify the region */

    if ( !strncasecmp (regstr, "circle", 6) ) {
      newShape->shape = circle_rgn;
      regstr += 6;
    } else if ( !strncasecmp (regstr, "annulus", 7) ) {
      newShape->shape = annulus_rgn;
      regstr += 7;
    } else if ( !strncasecmp (regstr, "ellipse", 7) ) {
      newShape->shape = ellipse_rgn;
      regstr += 7;
    } else if ( !strncasecmp (regstr, "elliptannulus", 13) ) {
      newShape->shape = elliptannulus_rgn;
      regstr += 13;
    } else if ( !strncasecmp (regstr, "box", 3) ) {
      newShape->shape = box_rgn;
      regstr += 3;
    } else if ( !strncasecmp (regstr, "rectangle", 9) ) {
      newShape->shape = rectangle_rgn;
      regstr += 9;
    } else if ( !strncasecmp (regstr, "diamond", 7) ) {
      newShape->shape = diamond_rgn;
      regstr += 7;
    } else if ( !strncasecmp (regstr, "sector", 6) ) {
      newShape->shape = sector_rgn;
      regstr += 6;
    } else if ( !strncasecmp (regstr, "point", 5) ) {
      newShape->shape = point_rgn;
      regstr += 5;
    } else if ( !strncasecmp (regstr, "line", 4) ) {
      newShape->shape = line_rgn;
      regstr += 4;
    } else if ( !strncasecmp (regstr, "polygon", 7) ) {
      newShape->shape = poly_rgn;
      regstr += 7;
    } else if ( !strncasecmp (regstr, "panda", 5) ) {
      newShape->shape = panda_rgn;
      regstr += 5;
    } else if ( !strncasecmp (regstr, "epanda", 6) ) {
      newShape->shape = epanda_rgn;
      regstr += 6;
    } else if ( !strncasecmp (regstr, "bpanda", 6) ) {
      newShape->shape = bpanda_rgn;
      regstr += 6;
    } else {
      printf("Unrecognized region found in the DSS keywords");
      *status = -1;
      goto error;
    }
    while ( *regstr == ' ' ) regstr++;
    
    /* if the shape is a polygon find the number of parameters and get the memory */

    if ( newShape->shape == poly_rgn ) {
      nparam = 0;
      curstr = regstr;
      while ( *curstr != '\0' ) {
	nparam++;
	while ( *curstr != ' ' ) curstr++;
	while ( *curstr == ' ' ) curstr++;
      }
      newShape->param.poly.nPts = nparam;
      newShape->param.poly.Pts = (double *) malloc (nparam*sizeof(double));
      if ( !newShape->param.poly.Pts ) {
	printf("Could not allocate memory for polygon parameters");
	*status = -1;
	goto error;
      }
    }
    
    /* save the parameter values */

    i = 0;
    while ( *regstr != '\0' ) {
      parstr = regstr;
      while ( *regstr != ' ' ) regstr++;
      *(regstr++) = '\0';
      while ( *regstr == ' ' ) regstr++;
      if ( newShape->shape == poly_rgn ) {
	newShape->param.poly.Pts[i] = atof(parstr);
      } else {
	newShape->param.gen.p[i] = atof(parstr);
      }
      i++;
    }

  /* end loop on regions */

  }

error:

  if( *status || dsRgn->nShapes == 0) {
    fits_free_region( dsRgn );
     *Rgn = NULL;
  } else {
     *Rgn = dsRgn;
  }

  ffclos(fptr, status);


  return (*status);

}

/******************************************************************************/
int combine_all_regions(char *filename, char *eventname)
{
  /* reads region data from any REGION extension or DSS keywords in the input */
  /* filename and combines them with the current region structure (aRgn)      */

  int status = 0;
  SAORegion *fitsRgn=NULL, *dsRgn=NULL, *combRgn=NULL, *finalRgn=NULL;

  WCSdata wcs;

  /* read any FITS region extension in the input file */

  wcs.exists = 0;
  fits_read_rgnfile(filename, &wcs, &fitsRgn, &status);

  /* and any region defined by DSS keywords in the eventname extension */

  status = 0;
  read_dskey_region(filename, eventname, &dsRgn, &status);

  /* combine these two regions */

  if ( fitsRgn && dsRgn ) {
    merge_regions(fitsRgn, dsRgn, &combRgn, &status);
  } else if ( fitsRgn && !dsRgn ) {
    combRgn = fitsRgn;
  } else if ( !fitsRgn && dsRgn ) {
    combRgn = dsRgn;
  }

  /* then combine with the current region structure */

  if ( combRgn ) {
    merge_regions(aRgn, combRgn, &finalRgn, &status);
    aRgn = finalRgn;
  }

  return (status);

}
/********************************************************************************/
int merge_regions(SAORegion *reg1, SAORegion *reg2, SAORegion **retreg, int *status)
{
  /* Merge reg1 and reg2 to give outreg */

  int ncomp1, ncomp2;
  int icompout, icomb, icomp1, icomp2, i, j;
  int isnew;
  SAORegion *outreg;

  /* Allocate memory for merged region structure */

  outreg = (SAORegion *) malloc (sizeof(SAORegion));
  if (!outreg) {
    printf("Could not allocate memory for combined region\n");
    *status = -1;
    return(*status);
  }

  ncomp1 = reg1->Shapes[reg1->nShapes-1].comp;
  ncomp2 = reg2->Shapes[reg2->nShapes-1].comp;

  /* perform an initial double loop over components to find the number of 
     shapes in the output region */

  outreg->nShapes = 0;

  for (icomp1=1; icomp1<=ncomp1; icomp1++) {
    for (icomp2=1; icomp2<=ncomp2; icomp2++) {

      /* loop through shapes in icomp1 */

      for (i=0; i<reg1->nShapes; i++) {
	if ( reg1->Shapes[i].comp == icomp1 ) outreg->nShapes++;
      }

      /* then shapes in icomp2 */

      for (i=0; i<reg2->nShapes; i++) {
	if ( reg2->Shapes[i].comp == icomp2 ) outreg->nShapes++;
      }

      /* end loop over components */

    }
  }

  /* allocate memory for the output shapes */

  outreg->Shapes = (RgnShape *)malloc(outreg->nShapes*sizeof(RgnShape));

  /* double loop over components creating output region */

  icompout = 0;
  icomb = -1;

  for (icomp1=1; icomp1<=ncomp1; icomp1++) {
    for (icomp2=1; icomp2<=ncomp2; icomp2++) {

      icompout++;

      /* loop through shapes in icomp1 copying to the output region any shapes
         which are unique in this output component */

      for (i=0; i<reg1->nShapes; i++) {
	if ( reg1->Shapes[i].comp == icomp1 ) {
	  isnew = 1;
	  for (j=0; j<=icomb;j++) {
	    if (equal_shape(&(reg1->Shapes[i]),&outreg->Shapes[j]) &&
		outreg->Shapes[j].comp == icompout) isnew = 0;
	  }
	  if (isnew) {
	    icomb++;
	    copy_shape(&(reg1->Shapes[i]), &(outreg->Shapes[icomb]));
	    outreg->Shapes[icomb].comp = icompout;
	  }
	}
      }

      /* then shapes in icomp2 */

      for (i=0; i<reg2->nShapes; i++) {
	if ( reg2->Shapes[i].comp == icomp2 ) {
	  isnew = 1;
	  for (j=0; j<=icomb;j++) {
	    if (equal_shape(&(reg2->Shapes[i]),&outreg->Shapes[j]) &&
		outreg->Shapes[j].comp == icompout) isnew = 0;
	  }
	  if (isnew) {
	    icomb++;
	    copy_shape(&(reg2->Shapes[i]), &(outreg->Shapes[icomb]));
	    outreg->Shapes[icomb].comp = icompout;
	  }
	}
      }

      /* end loop over components */

    }
  }

  /* reset total number of shapes - this will be <= the number used to allocate
     the original memory for this region */

  outreg->nShapes = icomb+1;

  *retreg = outreg;
      
  return(0);
}
/********************************************************************************/
int copy_shape(RgnShape *inshape, RgnShape *outshape)
{
  /* copy inshape into outshape */

  int i;

  outshape->sign = inshape->sign;
  outshape->shape = inshape->shape;
  outshape->comp = inshape->comp;

  outshape->xmin = inshape->xmin;  
  outshape->xmax = inshape->xmax;
  outshape->ymin = inshape->ymin;  
  outshape->ymax = inshape->ymax;

  if ( inshape->shape == poly_rgn ) {

    outshape->param.poly.nPts = inshape->param.poly.nPts;
    outshape->param.poly.Pts = (double *) malloc (outshape->param.poly.nPts*sizeof(double));
    for (i=0; i<outshape->param.poly.nPts; i++) outshape->param.poly.Pts[i] = inshape->param.poly.Pts[i];

  } else {

    for (i=0; i<MAXPARS; i++) outshape->param.gen.p[i] = inshape->param.gen.p[i];
    outshape->param.gen.sinT = inshape->param.gen.sinT;
    outshape->param.gen.cosT = inshape->param.gen.cosT;
    outshape->param.gen.a = inshape->param.gen.a;
    outshape->param.gen.b = inshape->param.gen.b;

  }

  return(0);
}
/********************************************************************************/
int equal_shape(RgnShape *shape1, RgnShape *shape2)
{
  /* return true if two shapes are equal */

  int i;
  int equal = 1;

  if (shape2->sign != shape1->sign) equal = 0;
  if (shape2->shape != shape1->shape) equal = 0;
  if (shape2->comp != shape1->comp) equal = 0;

  if (shape2->xmin != shape1->xmin) equal = 0;  
  if (shape2->xmax != shape1->xmax) equal = 0;
  if (shape2->ymin != shape1->ymin) equal = 0;
  if (shape2->ymax != shape1->ymax) equal = 0;

  if ( shape1->shape == poly_rgn ) {

    if (shape2->param.poly.nPts != shape1->param.poly.nPts) {
      equal = 0;
    } else {
      for (i=0; i<shape2->param.poly.nPts; i++) {
	if (shape2->param.poly.Pts[i] != shape1->param.poly.Pts[i]) equal = 0;
      }
    }

  } else {

    for (i=0; i<MAXPARS; i++) {
      if (shape2->param.gen.p[i] != shape1->param.gen.p[i]) equal = 0;
    }
    if (shape2->param.gen.sinT != shape1->param.gen.sinT) equal = 0;
    if (shape2->param.gen.cosT != shape1->param.gen.cosT) equal = 0;
    if (shape2->param.gen.a != shape1->param.gen.a) equal = 0;
    if (shape2->param.gen.b != shape1->param.gen.b) equal = 0;

  }

  return(equal);
}
/********************************************************************************/
int number_shapes()
{

  return(aRgn->nShapes);

}

/********************************************************************************/
/* return information for the ith shape (where i counts from 1)                 */
/********************************************************************************/
void get_shape_data(int iShape, int *sign, char *shape, int *comp, int *npoints, double *points)
{
  int i;
  RgnShape *tmpShape; 
  char shapename[NSHAPES][FLEN_VALUE] = {"POINT","CIRCLE","ELLIPSE","ANNULUS",
				    "ELLIPTANNULUS","BOX","ROTBOX","BOXANNULUS",
				    "RECTANGLE","ROTRECTANGLE","POLYGON","PIE",
				    "SECTOR","DIAMOND","RHOMBUS","ROTDIAMOND",
				    "ROTRHOMBUS","PANDA","EPANDA","BPANDA"};
  int shapetype[NSHAPES] = {point_rgn, circle_rgn, ellipse_rgn, annulus_rgn, 
		       elliptannulus_rgn, box_rgn, box_rgn, boxannulus_rgn, 
		       rectangle_rgn, rectangle_rgn, poly_rgn, sector_rgn, 
		       sector_rgn, diamond_rgn, diamond_rgn, diamond_rgn, 
		       diamond_rgn, panda_rgn, epanda_rgn, bpanda_rgn};
  int shapepoints[NSHAPES] = {2, 3, 5, 4, 8, 5, 5, 8, 5, 5, 0, 4, 4, 5, 5, 5, 5, 8, 11, 11};


  tmpShape = &(aRgn->Shapes[iShape-1]);

  *sign = tmpShape->sign;
  *comp = tmpShape->comp;
  for (i=0 ;i<NSHAPES; i++) {
    if ( tmpShape->shape == shapetype[i] ) {
      strcpy(shape, shapename[i]);
    }
  }

  if ( tmpShape->shape == poly_rgn ) {
    *npoints = tmpShape->param.poly.nPts;
    for (i=0; i<(*npoints); i++) points[i] = tmpShape->param.poly.Pts[i];
  } else {
    for (i=0 ;i<NSHAPES; i++) {
      if ( tmpShape->shape == shapetype[i] ) *npoints = shapepoints[i];
    }
    for (i=0; i<(*npoints); i++) points[i] = tmpShape->param.gen.p[i];
  }

}
