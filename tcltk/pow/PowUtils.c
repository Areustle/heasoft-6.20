#include <limits.h>
#include <fitsio2.h>
#include "pow.h"
#include "powRegion.h"

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif

#define NCOORD 1

static int Pt_in_Poly( double x, double y, int nPts, double *Pts );

int PowFindCurvesMinMax(const char *curves, char *axis, double *min, double *max,
			int filter)
  /* Sets min and max to the minimum and maximum values found in the 
     "axis" vector of each member of the list of curves.  Initial values 
     for min and max are whatever they are when the routine is called.  
     (i.e. if *min == 0  upon entry and there are no negative values in the 
     curves, *min will == 0 upon return). */
{
  PowCurve *current_curve;
  PowVector *current_vector;
  PowData *current_data;
  int curve_index,curveArgc;
  const char **curveArgv;
  double datum;
  int i;

  if(curves == NULL || strstr(curves,"NULL") != NULL ) return TCL_OK;
  if(Tcl_SplitList(interp,curves,&curveArgc,&curveArgv) != TCL_OK) {
    return TCL_ERROR;
  }
  for (curve_index = 0; curve_index < curveArgc;curve_index++) {
    current_curve = PowFindCurve(curveArgv[curve_index]);
    switch (*axis) {
    case 'X': current_vector = current_curve->x_vector;
      break;
    case 'Y': current_vector = current_curve->y_vector;
      break;
    case 'Z': current_vector = current_curve->z_vector;
      break;
    default:
      Tcl_SetResult( interp, "Axis must be X, Y, or Z.", TCL_VOLATILE );
      ckfree( (char *)curveArgv );
      return TCL_ERROR;
    }
    if(current_vector != NULL) {
      current_data = current_vector->dataptr;
      for (i = current_vector->offset ;
	   i < current_vector->offset + current_curve->length ;
	   i++) {
	datum = PowExtractDatum(current_data,i);
	if( filter && datum <= 0.0 ) /* Positive data only (for logs) */
	   datum = DBL_MAX;
	if (datum != DBL_MAX) {
	  *min = (datum < *min) ? datum : *min;
	  *max = (datum > *max) ? datum : *max;
	}
      }
    } else {
       *min = (         1.0          < *min) ?          1.0          : *min;
       *max = (current_curve->length > *max) ? current_curve->length : *max;
    }
  }
  ckfree((char *) curveArgv);
  return TCL_OK;
}

int PowFindCurvesBBox(char *graph,
		      char *curves, double *xleft, double *xright, 
		      double *ybot, double *ytop, WCSdata *BaseWCS)
{
   PowCurve *current_curve;
   PowVector *Xvec, *Yvec;
   int curve_index,curveArgc;
   const char **curveArgv;
   double xdatum, ydatum, xmin, xmax, ymin, ymax, tmp;
   double lxmin, lxmax, lymin, lymax;
   int i,j, logX, logY;
   const char *optVal;

   if(curves == NULL || strstr(curves,"NULL") != NULL ) return TCL_OK;

   if(Tcl_SplitList(interp,curves,&curveArgc,&curveArgv) != TCL_OK) {
      return TCL_ERROR;
   }

  /*  Search through curve list for bounding box information  */
  /*  Skip any curves which raise errors, but don't abort!    */

   for( curve_index = 0; curve_index < curveArgc; curve_index++ ) {

      current_curve = PowFindCurve(curveArgv[curve_index]);

      Xvec = current_curve->x_vector;
      Yvec = current_curve->y_vector;

      optVal = PowGetObjectOption( graph, curveArgv[curve_index],
				   "logX", "curve" );
      if( !optVal || Tcl_GetBoolean( interp, optVal, &logX )==TCL_ERROR ) {
	 logX = 0;
      }
      optVal = PowGetObjectOption( graph, curveArgv[curve_index],
				   "logY", "curve" );
      if( !optVal || Tcl_GetBoolean( interp, optVal, &logY )==TCL_ERROR ) {
	 logY = 0;
      }

      xmin = ymin = DBL_MAX;  xmax = ymax = - DBL_MAX;

      if( Xvec==NULL || Yvec==NULL || 
          (current_curve->WCS.type[0]=='\0'
           && current_curve->WCS.cdFrwd[0][1]==0.0 
           && current_curve->WCS.cdFrwd[1][0]==0.0 ) ) {

	 lxmin = lymin = DBL_MAX;  lxmax = lymax = - DBL_MAX;
	 PowFindCurvesMinMax( curveArgv[curve_index], "X", &lxmin, &lxmax,
			      logX );
	 PowFindCurvesMinMax( curveArgv[curve_index], "Y", &lymin, &lymax,
			      logY );

	 if( logX ) {
	    if( lxmin<=0.0 || lxmax<=0.0 ) {
	       return TCL_ERROR;
	    } else {
	       lxmin = log10(lxmin);
	       lxmax = log10(lxmax);
	    }
	 }
	 if( logY ) {
	    if( lymin<=0.0 || lymax<=0.0 ) {
	       return TCL_ERROR;
	    } else {
	       lymin = log10(lymin);
	       lymax = log10(lymax);
	    }
	 }
	 xmin = (lxmin < xmin) ? lxmin : xmin;
	 xmax = (lxmax > xmax) ? lxmax : xmax;
	 ymin = (lymin < ymin) ? lymin : ymin;
	 ymax = (lymax > ymax) ? lymax : ymax;

	 if( PowPixToPos( xmin, ymin, &current_curve->WCS, &xmin, &ymin ) )
            continue;
	 if( PowPixToPos( xmax, ymax, &current_curve->WCS, &xmax, &ymax ) )
            continue;

	 if( PowPosToPix( xmin, ymin, BaseWCS, &xmin, &ymin ) )
            continue;
	 if( PowPosToPix( xmax, ymax, BaseWCS, &xmax, &ymax ) )
            continue;

	 if( xmin>xmax ) { tmp=xmax; xmax=xmin; xmin=tmp; }
	 if( ymin>ymax ) { tmp=ymax; ymax=ymin; ymin=tmp; }

      } else {

	 for (i = Xvec->offset, j = Yvec->offset ;
	      i < Xvec->offset + current_curve->length ;
	      i++, j++) {
	    xdatum = PowExtractDatum(Xvec->dataptr,i);
	    ydatum = PowExtractDatum(Yvec->dataptr,j);
	    if( xdatum != DBL_MAX && ydatum != DBL_MAX ) {

	       if( PowPixToPos( xdatum, ydatum, &current_curve->WCS,
				&xdatum, &ydatum ) )
                  continue;
	       if( PowPosToPix( xdatum, ydatum, BaseWCS, &xdatum, &ydatum ) )
		  continue;
	       xmin = (xdatum < xmin) ? xdatum : xmin;
	       xmax = (xdatum > xmax) ? xdatum : xmax;
	       ymin = (ydatum < ymin) ? ydatum : ymin;
	       ymax = (ydatum > ymax) ? ydatum : ymax;
	    }
	 }
      }

      if( xmin < *xleft  ) *xleft  = xmin;
      if( ymin < *ybot   ) *ybot   = ymin;
      if( xmax > *xright ) *xright = xmax;
      if( ymax > *ytop   ) *ytop   = ymax;

   }

   ckfree((char *) curveArgv);
   return TCL_OK;
}

int PowFindImagesBBox(char *images, double *xleft, double *xright, 
		      double *ybot, double *ytop, WCSdata *BaseWCS)
{
  PowImage *current_image;
  int image_index,imageArgc;
  const char **imageArgv;
  double xorigin,xotherend,yorigin,yotherend,xcorner,ycorner;

  if(images == NULL || strstr(images,"NULL") != NULL) return TCL_OK;


  if(Tcl_SplitList(interp,images,&imageArgc,&imageArgv) != TCL_OK) {
    return TCL_ERROR;
  }

  /*  Search through image list for bounding box information  */
  /*  Skip any images which raise errors, but don't abort!    */

  for (image_index = 0; image_index < imageArgc; image_index++) {

     current_image = PowFindImage(imageArgv[image_index]);
    
     /*  Convert origin and otherend info into pixel coordinates  */

     if ( PowPosToPix( current_image->xorigin, current_image->yorigin,
		       BaseWCS, &xorigin, &yorigin ) )
        continue;
     if ( PowPosToPix( current_image->xotherend, current_image->yotherend,
		       BaseWCS, &xotherend, &yotherend ) )
        continue;

     /**************************************/
     /*  Test the images for consistency:  */
     /**************************************/

     if ( BaseWCS->type[0] && !current_image->WCS.type[0] )
        continue;

     /*  We are in pixel coordinates, so they should  */
     /*  ALWAYS go from left->right                   */

     if ( xorigin > xotherend || yorigin > yotherend )
        continue;

     /***************************************************/
     /*  Images must project to an unrotated rectangle  */
     /***************************************************/

     if( PowPixToPos( -0.5, current_image->height-0.5, &current_image->WCS,
                      &xcorner, &ycorner ) )
        continue;
     if( PowPosToPix( xcorner, ycorner, BaseWCS, &xcorner, &ycorner ) )
        continue;

     if( fabs( xcorner-xorigin ) > 1.0 || fabs( ycorner-yotherend ) > 1.0 ) {
        continue;
     }

     if( PowPixToPos( current_image->width-0.5, -0.5, &current_image->WCS,
                      &xcorner, &ycorner ) )
        continue;
     if( PowPosToPix( xcorner, ycorner, BaseWCS, &xcorner, &ycorner ) )
        continue;

     if( fabs( xcorner-xotherend ) > 1.0 || fabs( ycorner-yorigin ) > 1.0 ) {
        continue;
     }

     /**************************************/
     /*     End of consistency tests       */
     /**************************************/

     if ( xorigin   < *xleft  ) *xleft   = xorigin;
     if ( yorigin   < *ybot   ) *ybot    = yorigin;
     if ( xotherend > *xright ) *xright  = xotherend;
     if ( yotherend > *ytop   ) *ytop    = yotherend;

  }

  ckfree( (char *)imageArgv);
  return TCL_OK;
}

int PowFindGraphBBox( PowGraph *graph, char *images, char *curves,
		      double *xmin, double *xmax,
		      double *ymin, double *ymax )
{
   /* xmin, etc, are initially in graph's "pixel" coordinates */

   *xmin =   DBL_MAX;
   *xmax = - DBL_MAX;
   *ymin =   DBL_MAX;
   *ymax = - DBL_MAX;

   /*  Test any curves that are present  */

   if( curves != NULL && strstr(curves,"NULL") == NULL ) {

      if( PowFindCurvesBBox( graph->graph_name, curves,
                             xmin, xmax, ymin, ymax, &(graph->WCS) )
          == TCL_ERROR ) {
         return TCL_ERROR;
      }

   }
   
   /*  Test any images that are present  */

   if( images != NULL && strstr(images,"NULL") == NULL ) {

      if( PowFindImagesBBox( images, xmin, xmax, ymin, ymax, &(graph->WCS) )
          == TCL_ERROR ) {
         return TCL_ERROR;
      }

   } else if( *xmin!=DBL_MAX ) {

      double xdim, ydim;

      /*  Only plots in graph... expand by 10% for margins around points */

      xdim   = *xmax - *xmin;
      ydim   = *ymax - *ymin;
      *xmin -= 0.05*xdim;
      *ymin -= 0.05*ydim;
      *xmax += 0.05*xdim;
      *ymax += 0.05*ydim;
      
   }

   /*  Convert bounds back into graph coordinates   */

   if( *xmin!=DBL_MAX ) {
      PowPixToPos(*xmin, *ymin, &(graph->WCS), xmin, ymin );
      PowPixToPos(*xmax, *ymax, &(graph->WCS), xmax, ymax );
   } else {
      /*  Failed to find any valid bounding box.  Try just 1 pixel wide
          around reference pix  */
      PowPixToPos( graph->WCS.refPix[0]-1, graph->WCS.refPix[1]-1,
                   &(graph->WCS), xmin, ymin);
      PowPixToPos( graph->WCS.refPix[0]+1, graph->WCS.refPix[1]+1,
                   &(graph->WCS), xmax, ymax);
   }

   return TCL_OK;
}

int PowSortGraphMinMax( PowGraph *graph, double *xleft, double *xright,
			double *ybot, double *ytop, double *xdim, double *ydim)
{
  double tmp;
  int len;
  char *idxStr;
  const char *graphType;
  int zoomed;
  int xCount, yCount;

  len    = strlen(graph->graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "graphType", graph->graph_name);
  graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
  ckfree(idxStr);

  len    = strlen(graph->graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "zoomed", graph->graph_name);
  zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
  ckfree(idxStr);

  xCount = atoi(Tcl_GetVar2(interp,"xCount",graph->graph_name,TCL_GLOBAL_ONLY));
  yCount = atoi(Tcl_GetVar2(interp,"yCount",graph->graph_name,TCL_GLOBAL_ONLY));

  if ( PowPosToPix(*xleft,  *ybot, &graph->WCS, xleft,  ybot) )
     return TCL_ERROR;
  if ( PowPosToPix(*xright, *ytop, &graph->WCS, xright, ytop) )
     return TCL_ERROR;

  if ( zoomed == 0 && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
  } else {
  }

  if ( zoomed == 0 && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
  } else {
  }

  if ( *xleft>*xright ) { tmp=*xleft; *xleft=*xright; *xright=tmp; }
  if ( *ybot >*ytop )   { tmp=*ybot;  *ybot =*ytop;   *ytop  =tmp; }

  *xdim = *xright - *xleft;
  *ydim = *ytop   - *ybot;

/*  Convert bounds back into graph coordinates and return  */

  if ( PowPixToPos(*xleft,  *ybot, &graph->WCS, xleft,  ybot) )
     return TCL_ERROR;
  if ( PowPixToPos(*xright, *ytop, &graph->WCS, xright, ytop) )
     return TCL_ERROR;

  return TCL_OK;
}

PowCurve *
PowFindCurve(const char *curve_name) {
  Tcl_HashEntry *entry_ptr;
  PowCurve *curve_ptr;

  if(curve_name == NULL || strstr(curve_name,"NULL") != NULL) {
    return (PowCurve *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowCurveTable,curve_name);
  if (entry_ptr == NULL) {
    return (PowCurve *) NULL;
  }
  curve_ptr = (PowCurve *) Tcl_GetHashValue(entry_ptr);
  return curve_ptr;
}

PowImage *
PowFindImage(const char *image_name) {
  Tcl_HashEntry *entry_ptr;
  PowImage *image_ptr;

  if(image_name == NULL || strstr(image_name,"NULL") != NULL) {
    return (PowImage *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowImageTable,image_name);
  if (entry_ptr == NULL) {
    return (PowImage *) NULL;
  }
  image_ptr = (PowImage *) Tcl_GetHashValue(entry_ptr);
  return image_ptr;
}

#ifdef __WIN32__
__int64 PowExtractDatumLong(PowData *data, int element) {
  __int64 datum;
  return *((__int64 *) data->data_array + element); 
}
#else
long long PowExtractDatumLong(PowData *data, int element) {
  return *((long long *) data->data_array + element); 
}
#endif

double
PowExtractDatum(PowData *data, int element) {
  double datum;
  switch (data->data_type) {
  case BYTE_DATA : datum = (double) *((unsigned char *) data->data_array + element);
    if (datum == UCHAR_MAX) { datum = DBL_MAX;}
    break;
  case INT_DATA : datum = (double) *((int *) data->data_array + element);
    if (datum == INT_MAX) {datum = DBL_MAX;}
    break;
  case SHORTINT_DATA : datum = (double) *((short int *) data->data_array + element);
    if (datum == SHRT_MAX) {datum = DBL_MAX;}
    break;
  case REAL_DATA : datum = (double) *((float *) data->data_array + element);
    if (datum == FLT_MAX) {datum = DBL_MAX;}
    break;	
  case DOUBLE_DATA : datum =  *((double *) data->data_array + element);
    break;	
  case STRING_DATA : /*don't use PowExtractDatum on string data */
    datum = DBL_MAX;    
    break;
  case LONGLONG_DATA: datum = DBL_MAX;
    break;
  }
  return datum;
}


int 
PowPutDatum(PowData *data, double datum, int element) {
  switch (data->data_type) {
  case BYTE_DATA : *((unsigned char *) data->data_array + element) = 
		     (unsigned char) datum;
    break;
  case INT_DATA : *((int *) data->data_array + element) =
		    (int) datum;
    break;
  case SHORTINT_DATA : *((short int *) data->data_array + element) =
			 (int) datum;
    break;
  case REAL_DATA : *((float *) data->data_array + element) =
		    (float) datum;
    break;	
  case DOUBLE_DATA :  *((double *) data->data_array + element) =
		    (double) datum;
    break;	
  }
  return TCL_OK;
}



PowVector *
PowFindVector(char *vector_name) {
  Tcl_HashEntry *entry_ptr;
  PowVector *vector_ptr;

  if(vector_name == NULL || strstr(vector_name,"NULL") != NULL) {
    return (PowVector *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowVectorTable,vector_name);
  if (entry_ptr == NULL) {
    return (PowVector *) NULL;
  }
  vector_ptr = (PowVector *) Tcl_GetHashValue(entry_ptr);
  return vector_ptr;
}

PowData *
PowFindData(char *data_name) {
  Tcl_HashEntry *entry_ptr;
  PowData *data_ptr;

  if(data_name == NULL || strstr(data_name,"NULL") != NULL) {
    return (PowData *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowDataTable,data_name);
  if (entry_ptr == NULL) {
    return (PowData *) NULL;
  }
  data_ptr = (PowData *) Tcl_GetHashValue(entry_ptr);
  return data_ptr;
}

PowGraph *
PowFindGraph(char *graph_name) {
  Tcl_HashEntry *entry_ptr;
  PowGraph *graph_ptr;

  if(graph_name == NULL || strstr(graph_name,"NULL") != NULL) {
    return (PowGraph *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowGraphTable,graph_name);
  if (entry_ptr == NULL) {
    return (PowGraph *) NULL;
  }
  graph_ptr = (PowGraph *) Tcl_GetHashValue(entry_ptr);
  return graph_ptr;
}


const char *PowGetObjectOption(char *graph, const char *obj, char *option, char *objType)
{
   char *idxStr, gn[255];
   const char *res;
   int len;

   len = strlen(graph);

   if( len>5 && !strcmp(graph+len-5,"scope") ) {
      strncpy(gn,graph,len-5);
      gn[len-5]='\0';
   } else {
      strcpy(gn,graph);
   }

   len    = strlen(gn)+strlen(obj)+strlen(option)+10;
   idxStr = (char *) ckalloc( len*sizeof(char) );

   if( !strcmp(objType,"curve") ) {

      sprintf(idxStr,"%s%s,%s",option,obj,gn);
      res = Tcl_GetVar2(interp,"powCurveParam",idxStr,TCL_GLOBAL_ONLY);
      if( res==NULL ) {
	 sprintf(idxStr,"%s,powDef",option);
	 res = Tcl_GetVar2(interp,"powCurveParam",idxStr,TCL_GLOBAL_ONLY);
      }

   } else if( !strcmp(objType,"image") ) {

      sprintf(idxStr,"%s%s,%s",option,obj,gn);
      res = Tcl_GetVar2(interp,"powImageParam",idxStr,TCL_GLOBAL_ONLY);
      if( res==NULL ) {
	 sprintf(idxStr,"%s,powDef",option);
	 res = Tcl_GetVar2(interp,"powImageParam",idxStr,TCL_GLOBAL_ONLY);
      }

   } else if( !strcmp(objType,"graph") ) {

      sprintf(idxStr,"%s%s,%s",option,obj,gn);
      res = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
      if( res==NULL ) {
	 sprintf(idxStr,"%s,powDef",option);
	 res = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
      }

   }
   ckfree(idxStr);
   return res;
}

int PowPosToPix( double xpos, double  ypos, WCSdata *WCS,
		        double *xpix, double *ypix )
{
   char powFitsHeader[14]="powFitsHeader";
   char powFitsHeaderCnt[17]="powFitsHeaderCnt";
   int i, relax, HDRcnt, ctrl, nreject=0, nwcs=0;
   const char *HDRstring;

   /* input */
   int nelem;
   double pixcrd[NCOORD][4];

   /* output */
   double imgcrd[NCOORD][4], world[NCOORD][4];
   double phi[NCOORD], theta[NCOORD];
   int stat[NCOORD];
   int statFix[NWCSFIX];
   int coordSel;

   int useWCSInfo;
   const char *str = NULL;

   if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
      str = Tcl_GetVar2(interp,"useWCSInfo",WCS->graphName,TCL_GLOBAL_ONLY);
   } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
      str = Tcl_GetVar2(interp,"useWCSInfo",WCS->curveName,TCL_GLOBAL_ONLY);
   }
   /* str = Tcl_GetVar(interp,"useWCSInfo",TCL_GLOBAL_ONLY); */

   if ( str != (char *)NULL ) {
      useWCSInfo = atoi(str);
   } else {
      useWCSInfo = 0;
   }

   /* useWCSInfo = 1; */

   if ( useWCSInfo == 1 ) {

      /* using WCS information */
      char errormsg[512];
      Tcl_Obj *listObj;
      Tcl_Obj *wcsname[27];
      int status;

      if ( WCS->haveWCSinfo == 0 ) {
         /* no wcs info yet */
         if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
            HDRstring = Tcl_GetVar2(interp,powFitsHeader,WCS->graphName,TCL_GLOBAL_ONLY);
            HDRcnt = atoi(Tcl_GetVar2(interp,powFitsHeaderCnt,WCS->graphName,TCL_GLOBAL_ONLY));
         } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
            HDRstring = Tcl_GetVar2(interp,powFitsHeader,WCS->curveName,TCL_GLOBAL_ONLY);
            HDRcnt = atoi(Tcl_GetVar2(interp,powFitsHeaderCnt,WCS->curveName,TCL_GLOBAL_ONLY));
         } else {
            Tcl_SetResult(interp, "Can't construct WCS information." ,TCL_VOLATILE);
            Tcl_SetVar(interp,"powWCSTranslation", "1" ,TCL_GLOBAL_ONLY);
            return TCL_ERROR;
         }
         relax = WCSHDR_all;

         ctrl = 2;

         if (status = wcspih(HDRstring, HDRcnt, relax, ctrl, &nreject, &nwcs, &(WCS->wcs))) {
            sprintf (errormsg, "Couldn't construct WCS information: %s", WCSpih_Message[status]);
            Tcl_SetResult(interp, errormsg ,TCL_VOLATILE);
            Tcl_SetVar(interp,"powWCSTranslation", WCSpih_Message[status] ,TCL_GLOBAL_ONLY);
            return TCL_ERROR;
         }

         listObj = Tcl_NewObj();
         for (i=0; i<nwcs; i++) {
           wcsname[i] = Tcl_NewStringObj(WCS->wcs[i].alt,-1);
         }

         /* This is just a debug statement */
         /* powDebugDataPrint(HDRstring, HDRcnt, WCS, nwcs, WCS->graphName); */

         Tcl_ListObjAppendElement( interp, listObj, Tcl_NewIntObj( nwcs ) );
         Tcl_ListObjAppendElement( interp, listObj, Tcl_NewListObj(nwcs,wcsname) );
         if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
            Tcl_SetVar2Ex(interp,"powWCSList", WCS->graphName, listObj, TCL_GLOBAL_ONLY);
         } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
            Tcl_SetVar2Ex(interp,"powWCSList", WCS->curveName, listObj, TCL_GLOBAL_ONLY);
         }

         WCS->haveWCSinfo = 1;
      }

      if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->graphName,TCL_GLOBAL_ONLY));
      } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->curveName,TCL_GLOBAL_ONLY));
      }
      WCS->wcs[coordSel].crpix[0] = WCS->refPix[0];
      WCS->wcs[coordSel].crpix[1] = WCS->refPix[1];

/*
fprintf(stdout, "---PosToPix graphName: <%s>,  coordSel: <%d>\n", WCS->graphName, coordSel);
fflush(stdout);
*/
      nelem = 2;
      world[0][0] = xpos;
      world[0][1] = ypos;
      world[0][2] = 1.f;
      world[0][3] = 1.f;

      status = 0;
      status = wcsfix(7, 0, &(WCS->wcs[coordSel]), statFix);
      status = wcss2p(&(WCS->wcs[coordSel]), NCOORD, nelem, world, phi, theta, imgcrd, pixcrd, stat);

      if ( status ) {
         sprintf (errormsg, "Couldn't translate WCS coords to pixels: %s", WCStrans_Message[status]);
         Tcl_SetResult( interp, errormsg, TCL_VOLATILE );
         Tcl_SetVar(interp,"powWCSTranslation", WCStrans_Message[status] ,TCL_GLOBAL_ONLY);
         return TCL_ERROR;
      } else {

         *xpix = pixcrd[0][0];
         *ypix = pixcrd[0][1];

         Tcl_SetVar(interp,"powWCSTranslation","0",TCL_GLOBAL_ONLY);
      }
   } else {
      xpos -= WCS->refVal[0];
      ypos -= WCS->refVal[1];

      *xpix = WCS->cdRvrs[0][0] * xpos + WCS->cdRvrs[0][1] * ypos;
      *ypix = WCS->cdRvrs[1][0] * xpos + WCS->cdRvrs[1][1] * ypos;


      Tcl_SetVar(interp,"powWCSTranslation","0",TCL_GLOBAL_ONLY);
      *xpix += WCS->refPix[0];
      *ypix += WCS->refPix[1];
   }
   return TCL_OK;
}

int PowPixToPos ( double  xpix, double  ypix, WCSdata *WCS, double *xpos, double *ypos )
{
   char powFitsHeader[14]="powFitsHeader";
   char powFitsHeaderCnt[17]="powFitsHeaderCnt";
   int i, relax, HDRcnt, ctrl, nreject=0, nwcs=0;
   const char *HDRstring;

   /* input */
   int nelem;
   double pixcrd[NCOORD][4];

   /* output */
   double imgcrd[NCOORD][4], world[NCOORD][4];
   double phi[NCOORD], theta[NCOORD];
   int stat[NCOORD];
   int statFix[NWCSFIX];
   int coordSel;

   int useWCSInfo;
   const char *str = NULL;

   if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
      str = Tcl_GetVar2(interp,"useWCSInfo",WCS->graphName,TCL_GLOBAL_ONLY);
   } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
      str = Tcl_GetVar2(interp,"useWCSInfo",WCS->curveName,TCL_GLOBAL_ONLY);
   }

   if ( str != (char *)NULL ) {
      useWCSInfo = atoi(str);
   } else {
      useWCSInfo = 0;
   }

   /* useWCSInfo = 1; */
   if ( useWCSInfo == 1 ) {

      /* using WCS information */
      char errormsg[512];
      Tcl_Obj *listObj;
      Tcl_Obj *wcsname[27];
      int status;

      if ( WCS->haveWCSinfo == 0 ) {
         if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
            HDRstring = Tcl_GetVar2(interp,powFitsHeader,WCS->graphName,TCL_GLOBAL_ONLY);
            HDRcnt = atoi(Tcl_GetVar2(interp,powFitsHeaderCnt,WCS->graphName,TCL_GLOBAL_ONLY));
         } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
            HDRstring = Tcl_GetVar2(interp,powFitsHeader,WCS->curveName,TCL_GLOBAL_ONLY);
            HDRcnt = atoi(Tcl_GetVar2(interp,powFitsHeaderCnt,WCS->curveName,TCL_GLOBAL_ONLY));
         } else {
            Tcl_SetResult(interp, "Can't construct WCS information." ,TCL_VOLATILE);
            Tcl_SetVar(interp,"powWCSTranslation", "1" ,TCL_GLOBAL_ONLY);
            return TCL_ERROR;
         }

         relax = WCSHDR_all;

         ctrl = 2;

         if (status = wcspih(HDRstring, HDRcnt, relax, ctrl, &nreject, &nwcs, &(WCS->wcs))) {
            sprintf (errormsg, "Couldn't construct WCS information: %s", WCSpih_Message[status]);
            Tcl_SetResult(interp, errormsg ,TCL_VOLATILE);
            Tcl_SetVar(interp,"powWCSTranslation", WCSpih_Message[status] ,TCL_GLOBAL_ONLY);
            return TCL_ERROR;
         }

         listObj = Tcl_NewObj();
         for (i=0; i<nwcs; i++) {
             wcsname[i] = Tcl_NewStringObj(WCS->wcs[i].alt,-1);
         }

         /* This is just a debug statement */
         /* powDebugDataPrint(HDRstring, HDRcnt, WCS, nwcs, WCS->graphName); */

         Tcl_ListObjAppendElement( interp, listObj, Tcl_NewIntObj( nwcs ) );
         Tcl_ListObjAppendElement( interp, listObj, Tcl_NewListObj(nwcs,wcsname) );

         if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
            Tcl_SetVar2Ex(interp,"powWCSList", WCS->graphName, listObj, TCL_GLOBAL_ONLY);
         } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
            Tcl_SetVar2Ex(interp,"powWCSList", WCS->curveName, listObj, TCL_GLOBAL_ONLY);
         }
         WCS->haveWCSinfo = 1;
      }

      if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->graphName,TCL_GLOBAL_ONLY));
      } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->curveName,TCL_GLOBAL_ONLY));
      }

/*
fprintf(stdout, "PixToPos graphName: <%s> coordSel: <%d>\n", WCS->graphName, coordSel);
fprintf(stdout, "         WCS->refPix[0]: <%20.15f> WCS->refPix[1]: <%20.15f>\n", WCS->refPix[0], WCS->refPix[1]);
fflush(stdout);
*/

      WCS->wcs[coordSel].crpix[0] = WCS->refPix[0];
      WCS->wcs[coordSel].crpix[1] = WCS->refPix[1];

      /* using WCS information */
      nelem = 2;
      pixcrd[0][0] = xpix;
      pixcrd[0][1] = ypix;
      pixcrd[0][2] = 1.f;
      pixcrd[0][3] = 1.f;

      status = 0;
      status = wcsfix(7, 0, &(WCS->wcs[coordSel]), statFix);
      status = wcsp2s(&(WCS->wcs[coordSel]), NCOORD, nelem, pixcrd, imgcrd, phi, theta, world, stat);

      if ( status ) {
         sprintf (errormsg, "Couldn't translate pixels to WCS coords: %s", WCStrans_Message[status]);
         Tcl_SetResult( interp, errormsg, TCL_VOLATILE );
         Tcl_SetVar(interp,"powWCSTranslation", WCStrans_Message[status] ,TCL_GLOBAL_ONLY);
         return TCL_ERROR;
      }

      *xpos = world[0][0];
      *ypos = world[0][1];

      Tcl_SetVar(interp,"powWCSTranslation", "0" ,TCL_GLOBAL_ONLY);
   } else {
      xpix -= WCS->refPix[0];
      ypix -= WCS->refPix[1];

      *xpos = WCS->cdFrwd[0][0] * xpix + WCS->cdFrwd[0][1] * ypix;
      *ypos = WCS->cdFrwd[1][0] * xpix + WCS->cdFrwd[1][1] * ypix;

      *xpos += WCS->refVal[0];
      *ypos += WCS->refVal[1];
      Tcl_SetVar(interp,"powWCSTranslation", "0" ,TCL_GLOBAL_ONLY);
   }

   return TCL_OK;
}

/* Routine to determine whether a point is in the region */  
int PowIsInRegion( double* pos , double *parReg,
                   int nParReg, char *shape, int* status)
/* The pos and parReg list should be converted to pow pixel before passing
   into this routine */
{
    char tmp_shape[10];
    char *ptr;
    double x,y,k,b,x1,y1;
    int i;
    double *polygon_vertex;
    int result;

    *status = 0;
    ptr = tmp_shape;
    strcpy(tmp_shape,shape);
    while(*ptr !=  '\0') {
       *ptr = tolower(*ptr);
       ptr++;
    }

    if (!strcmp(tmp_shape,"point")) {
         if(nParReg != 2)  {
             *status = 1;
             return 0;
         }
        if (pos[0]==parReg[0] && pos[1] == parReg[1])
            return 1;
        else
            return 0;
    }   


    if (!strcmp(tmp_shape,"line")) {
        if(nParReg != 4)  {
            *status = 1;
            return 0;
        }
        x = parReg[2] >= parReg[0]?parReg[2]:parReg[0];
        if (pos[0] > x) return 0;
        x = parReg[2] <= parReg[0]?parReg[2]:parReg[0];
        if (pos[0] < x) return 0;
        y = parReg[3] >= parReg[1]?parReg[3]:parReg[1];
        if (pos[0] > x) return 0;
        y = parReg[3] <= parReg[1]?parReg[3]:parReg[1];
        if (pos[0] < x) return 0;
        if(parReg[2] !=  parReg[0]) {
            k = (parReg[3] - parReg[1])/(parReg[2] - parReg[0]);
            b =  parReg[1] - k * parReg[0];
            y = k*pos[0] + b;
            if ( pos[1] == y)
                return 1;
            else
                return 0;
        } else {
          if (pos[0] == parReg[0]) return 1;
          return 0;
        }
    }

    if (!strcmp(tmp_shape,"polygon")) {
         polygon_vertex = (double *)malloc((nParReg + 1) * sizeof (double));

         for (i=0; i < nParReg; i++) {
             polygon_vertex[i] = parReg[i];
         }

         result = Pt_in_Poly(pos[0], pos[1], nParReg, polygon_vertex);
         free (polygon_vertex);

         return result;
     }

    if (!strcmp(tmp_shape,"circle")) {
        if(nParReg != 3)  {
            *status = 1;
            return 0;
        }
        b = (pos[0]-parReg[0])*(pos[0]-parReg[0])+
              (pos[1]-parReg[1])*(pos[1]-parReg[1]);
        if (b <= parReg[2]*parReg[2]) return 1;
        return 0;
    }

   if (!strcmp(tmp_shape,"box")) {
        if(nParReg != 5)  {
            *status = 1;
            return 0;
        }
    /* The width and height of the box is in pixels! Ugly! */
        x = pos[0] - parReg[0];
        y = pos[1] - parReg[1];

        b = parReg[4]/180.0*3.1415926;
        x1 = x*cos(b) + y * sin(b);
        y1 = -x*sin(b) + y* cos(b);

        if ( x1 >= parReg[2]/-2.0 && x1 <= parReg[2]/2.0 &&
             y1 >= parReg[3]/-2.0 && y1 <= parReg[3]/2.0 )
           return 1;
        return 0;
     }

    if (!strcmp(tmp_shape,"ellipse")) {
        if(nParReg != 5)  {
            *status = 1;
            return 0;
        }

        x = pos[0] - parReg[0];
        y = pos[1] - parReg[1];

        b = parReg[4]/180.0*3.1415926;
        x1 = x*cos(b) + y * sin(b);
        y1 = -x*sin(b) + y* cos(b);

        k = x1*x1/parReg[2]/parReg[2] + y1*y1/parReg[3]/parReg[3];
        if (k <= 1.0) return 1;
        return 0;
    }
    *status = 3;
    return 0;
}

/* Caculate  statistics inside the region */
int PowCalRegion( PowImage* image_ptr, char *regionFile, int *rect, 
                  double *parReg, int nParReg, char *shape, 
                  char *sign, double* cent, double* cstd, double* flux,
                  double* npix, double* mean, double* dmean, int* status)
/* The pos and parReg list should be converted to sao pixel before passing
   into this routine */
{
      int i, j;
      double datum;
      double sx,sy;
      double sx1,sy1;
      double sx2,sy2;
      double sxx,syy;
      double flux2;
      int flag;
      int ix,iy;
      int regionInputFile;
      double pos[2];
      double absflux;
      int statusN = 0;       /* status must always be initialized = 0  */
      SAORegion *Rgn;

      int xmin,ymin,xmax,ymax, signflag;

      regionInputFile = 0;
      if (strcmp(regionFile,"NONE") != 0) {
         fits_read_rgnfile(regionFile, 0, &Rgn, &statusN);
         regionInputFile = 1;
         if ( statusN ) {
            Tcl_SetResult( interp, "Could not read region file.\n", TCL_VOLATILE );
	    return TCL_ERROR;
         }
      }

      *npix = 0;
      if(strchr(sign,'+')!= NULL) {
         signflag = 1;
         xmin = rect[0];
         ymin = rect[1];
         xmax = rect[2];
         ymax = rect[3];
      } else {
         signflag = 0;
         xmin = 1;
         ymin = 1;
         xmax = image_ptr->width;
         ymax = image_ptr->height;
      }

      if ( regionInputFile == 1 ) {
         signflag = 0;
         xmin = 1;
         ymin = 1;
         xmax = image_ptr->width;
         ymax = image_ptr->height;
      }

      *flux = 0.0;
      flux2 = 0.0;
      absflux = 0.0;
      sx = 0.0;
      sy = 0.0;
      sxx = 0.0;
      syy = 0.0;
      sx1 = 0.0;
      sy1 = 0.0;
      sx2 = 0.0;
      sy2 = 0.0;
      cent[0] = 0.0;
      cent[1] = 0.0;

     if ( regionInputFile == 1 ) {
        for (j = ymin; j <= ymax; j++) {
            pos[1] = (double)j;
            iy = (int)(pos[1] - 1 + image_ptr->yoffset);
            for (i = xmin; i <= xmax; i++) {
                pos[0] = (double)i  ;
                ix = (int)(pos[0] - 1 + image_ptr->xoffset);
                flag = fits_in_region(i, j, Rgn);
                *status = 0;
                if (flag) {
                   datum = PowExtractDatum(image_ptr->dataptr, iy * image_ptr->width + ix);
                   if ( datum==DBL_MAX ) { 
                      continue;
                   }

                   *flux += datum;
                   flux2 += datum*datum;
                   datum = datum >=0 ? datum : -datum;
                   absflux += datum;
                   sx += pos[0]*datum;
                   sy += pos[1]*datum;
                   sxx += pos[0]*pos[0]*datum;
                   syy += pos[1]*pos[1]*datum;
                   sx1 += pos[0];
                   sy1 += pos[1]; 
                   sx2 += pos[0]*pos[0];
                   sy2 += pos[1]*pos[1];
                   (*npix)++;
                }
            }
        }
     } else { 
        for (j = ymin; j < ymax; j++) {
            pos[1] = (double)j;
            iy = (int)(pos[1] - 1 + image_ptr->yoffset);
            if (iy  < 0 || iy >= image_ptr->height ) continue;
            for (i = xmin; i < xmax; i++) {
                pos[0] = (double)i  ;
                ix = (int)(pos[0] - 1 + image_ptr->xoffset);
                if (ix < 0 || ix >= image_ptr->width ) continue;
                if(*status) *status = 0;
                flag = PowIsInRegion(pos,parReg,nParReg,shape,status); 

                if (signflag && flag ) {  
                   datum = PowExtractDatum(image_ptr->dataptr, iy * image_ptr->width + ix);
                   if ( datum==DBL_MAX ) { 
                      continue;
                   }
                   *flux += datum;
	           flux2 += datum*datum;
                   datum = datum >=0 ? datum : -datum;
                   absflux += datum;
                   sx += pos[0]*datum;
                   sy += pos[1]*datum; 
                   sxx += pos[0]*pos[0]*datum;
                   syy += pos[1]*pos[1]*datum;
                   sx1 += pos[0];
                   sy1 += pos[1]; 
                   sx2 += pos[0]*pos[0];
                   sy2 += pos[1]*pos[1];
                   (*npix)++;
                }

                if (!signflag && !flag && *status == 0) { 
                   datum = PowExtractDatum(image_ptr->dataptr, ix * image_ptr->height + iy);
                   if ( datum==DBL_MAX ) { 
                      continue;
                   }
                   *flux += datum;
	           flux2 += datum*datum;
                   datum = datum >=0 ? datum : -datum;
                   absflux += datum;
                   sx += pos[0]*datum;
                   sy += pos[1]*datum;
                   sxx += pos[0]*pos[0]*datum;
                   syy += pos[1]*pos[1]*datum;
                   sx1 += pos[0];
                   sy1 += pos[1]; 
                   sx2 += pos[0]*pos[0];
                   sy2 += pos[1]*pos[1];
                   (*npix)++;
                }
            }
        }
     }

     if (*npix == 0 )  {
        *status = 1;
        return 1;
     } 

     if (absflux != 0.0) {
         cent[0] = sx/(absflux);
         cent[1] = sy/(absflux);

         cstd[0] = sqrt(fabs(sxx/(absflux) - cent[0]*cent[0]));
         cstd[1] = sqrt(fabs(syy/(absflux) - cent[1]*cent[1])); 
     } else {
         cent[0] = sx1/(*npix);
         cent[1] = sy1/(*npix);
         cstd[0] = sqrt(fabs(sx2 - *npix*cent[0]*cent[0])/sqrt((*npix)));
         cstd[1] = sqrt(fabs(sy2 - *npix*cent[1]*cent[1])/sqrt((*npix))); 
     }
     *mean = *flux/(*npix);	
     if ( *npix==1 )
        *dmean = 0.0;
     else
        *dmean = sqrt(flux2-*npix*(*mean)*(*mean))/sqrt((*npix-1)*(*npix));
     return 0;
}     

/*---------------------------------------------------------------------------*/
static int Pt_in_Poly( double x,
                       double y,
                       int nPts,
                       double *Pts )
/*  Internal routine for testing whether the coordinate x,y is within the    */
/*  polygon region traced out by the array Pts.                              */
/*---------------------------------------------------------------------------*/
{
   int i, j, flag=0;
   double prevX, prevY;
   double nextX, nextY;
   double dx, dy, Dy;

   nextX = Pts[nPts-2];
   nextY = Pts[nPts-1];

   for( i=0; i<nPts; i+=2 ) {
      prevX = nextX;
      prevY = nextY;

      nextX = Pts[i];
      nextY = Pts[i+1];

      if( (y>prevY && y>=nextY) || (y<prevY && y<=nextY)
          || (x>prevX && x>=nextX) )
         continue;
      
      /* Check to see if x,y lies right on the segment */

      if( x>=prevX || x>nextX ) {
         dy = y - prevY;
         Dy = nextY - prevY;

         if( fabs(Dy)<1e-10 ) {
            if( fabs(dy)<1e-10 )
               return( 1 );
            else
               continue;
         }

         dx = prevX + ( (nextX-prevX)/(Dy) ) * dy - x;
         if( dx < -1e-10 )
            continue;
         if( dx <  1e-10 )
            return( 1 );
      }

      /* There is an intersection! Make sure it isn't a V point.  */

      if( y != prevY ) {
         flag = 1 - flag;
      } else {
         j = i+1;  /* Point to Y component */
         do {
            if( j>1 )
               j -= 2;
            else
               j = nPts-1;
         } while( y == Pts[j] );

         if( (nextY-y)*(y-Pts[j]) > 0 )
            flag = 1-flag;
      }

   }
   return( flag );
}

void powDebugDataPrint (char *header,
                        int headerCnt,
                        WCSdata *WCS,
                        int nwcs,
                        char *graphName)
{
   int k = 0;
   int i;
   char currentStr[81];
   char *ptr;
   
   ptr = header;

   fprintf(stdout, "graphName: <%s>, headerCnt: <%d>\n", graphName, headerCnt);
   fflush(stdout); 
   for (i= 0; i< strlen(header); i+=80 ) {
       memset(currentStr, '\0', 81);
       strncpy(currentStr, ptr, 80);
       fprintf(stdout, "<%s>\n", currentStr);
       fflush(stdout); 
       k++; 
       ptr += 80;
   }

   fprintf(stdout, "final count: <%d>\n\nnumber of wcs: <%d>\nwcsname: ", k, nwcs );
   fprintf(stdout, "wcsname: ");
   fflush(stdout); 

   for (i= 0; i< nwcs; i++ ) {
       fprintf(stdout, "<%s>", WCS->wcs[i].alt);
   }

   fprintf(stdout, "\n");
   fflush(stdout); 
}
