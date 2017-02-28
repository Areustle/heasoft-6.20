#include <fitsio2.h>
#include "pow.h"

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif

#ifdef macintosh
#include <MacMemory.h>
#endif

typedef struct {
   double x,y;
} Point;

extern int Pow_Allocated;
extern PictColorTable *PowColorTable;

int PowCleanUp(ClientData clientData, Tcl_Interp *interp, 
	       int argc, char *argv[]) {
#if !(defined(__WIN32__) || defined(macintosh))
    /*
    Tcl_HashEntry *entry_ptr;
    Tcl_HashSearch search;
    PowData *data_instance;
    */
    unsigned long *pixels;
    int i,j;
#endif

/*free the data arrays that belong to POW */
/* Actually, we're going to comment this out for now.  With the hide/show
   graph capability, we never really dispose of a graph until the
   parent exits. So we'd better not trash the data */
/*
  for (entry_ptr = Tcl_FirstHashEntry(&PowDataTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    data_instance = (PowData *)Tcl_GetHashValue(entry_ptr);
    if (data_instance->copy ) {
      ckfree(data_instance->data_array);
    }
  }
  */


#if !(defined(__WIN32__) || defined(macintosh))

  if (Pow_Allocated != 0) {
/*free the colorcells we snarfed up */
    pixels = (unsigned long *)ckalloc(PowColorTable->ncolors*
				      sizeof(unsigned long));
    if( pixels == NULL )
      return 0;
    
    for(j=PowColorTable->lut_start,i=0;i<PowColorTable->ncolors;i++,j++)
      pixels[i] = j;
    
    /* free colors */
    XFreeColors(PowColorTable->display,PowColorTable->colormap,
		pixels,PowColorTable->ncolors,0);
    ckfree((void*)pixels);
    
    Pow_Allocated = 0;
  }
#endif /*__WIN32__ || macintosh */

    return TCL_OK;
}

int PowListGraphs(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  Tcl_HashEntry *entry_ptr;
  Tcl_HashSearch search;
  if (argc == 2) {
    if (Tcl_FindHashEntry(&PowGraphTable, argv[1]) != NULL) {
      Tcl_SetResult(interp,"1",TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp,"0",TCL_VOLATILE);
    }
    return TCL_OK;
  }
  for (entry_ptr = Tcl_FirstHashEntry(&PowGraphTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    Tcl_AppendElement(interp,
		      Tcl_GetHashKey(&PowGraphTable, entry_ptr));
  }
  return TCL_OK;
}

int PowListCurves(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  Tcl_HashEntry *entry_ptr;
  Tcl_HashSearch search;
  if (argc == 2) {
    if (Tcl_FindHashEntry(&PowCurveTable, argv[1]) != NULL) {
      Tcl_SetResult(interp,"1",TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp,"0",TCL_VOLATILE);
    }
    return TCL_OK;
  }
  for (entry_ptr = Tcl_FirstHashEntry(&PowCurveTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    Tcl_AppendElement(interp,
		      Tcl_GetHashKey(&PowCurveTable, entry_ptr));
  }
  return TCL_OK;
}

int PowListImages(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  Tcl_HashEntry *entry_ptr;
  Tcl_HashSearch search;
  if (argc == 2) {
    if (Tcl_FindHashEntry(&PowImageTable, argv[1]) != NULL) {
      Tcl_SetResult(interp,"1",TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp,"0",TCL_VOLATILE);
    }
    return TCL_OK;
  }
  for (entry_ptr = Tcl_FirstHashEntry(&PowImageTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    Tcl_AppendElement(interp,
		      Tcl_GetHashKey(&PowImageTable, entry_ptr));
  }
  return TCL_OK;
}
		      
int PowListVectors(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  Tcl_HashEntry *entry_ptr;
  Tcl_HashSearch search;
  if (argc == 2) {
    if (Tcl_FindHashEntry(&PowVectorTable, argv[1]) != NULL) {
      Tcl_SetResult(interp,"1",TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp,"0",TCL_VOLATILE);
    }
    return TCL_OK;
  }
  for (entry_ptr = Tcl_FirstHashEntry(&PowVectorTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    Tcl_AppendElement(interp,
		      Tcl_GetHashKey(&PowVectorTable, entry_ptr));
  }
  return TCL_OK;
}

int PowListData(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  Tcl_HashEntry *entry_ptr;
  Tcl_HashSearch search;
  if (argc == 2) {
    if (Tcl_FindHashEntry(&PowDataTable, argv[1]) != NULL) {
      Tcl_SetResult(interp,"1",TCL_VOLATILE);
    } else {
      Tcl_SetResult(interp,"0",TCL_VOLATILE);
    }
    return TCL_OK;
  }
  for (entry_ptr = Tcl_FirstHashEntry(&PowDataTable,&search);
       entry_ptr != NULL;
       entry_ptr = Tcl_NextHashEntry(&search)) {
    Tcl_AppendElement(interp,
		      Tcl_GetHashKey(&PowDataTable, entry_ptr));
  }
  return TCL_OK;
}


int PowProcessCurve(ClientData clientData, Tcl_Interp *interp, 
		    int argc, char *argv[]) {
  /* calculate the list of points for powPlotCurve */
  const char **bboxptr;
  const char **rbboxptr;
  double x0 , x1, y0, y1;
  double rx0 , rx1, ry0, ry1, ry, rx;
  double t_rx0 , t_rx1, t_ry0, t_ry1; /* to avoid point lossage due to roundoff
                                         errors on points that are near axes */
  int n, i, xoff, yoff, xeoff, yeoff;
  PowCurve *curve_ptr;
  PowVector *x_vector, *y_vector, *x_error, *y_error;
  PowData *x_vect, *y_vect, *x_err, *y_err;
  double xmagstep, ymagstep, xinc, yinc, oldx, oldy;
  char PlotCommand[1024];
  double x,y,xe,ye;
  int p1,p2,p3;
  int q1,q2,q3;
  int dflag,pflag,lflag;
  int lasti;

  lasti = 0;
  
  x_vect = NULL;
  y_vect = NULL;
  x_err = NULL;
  y_err = NULL;
  x_vector = NULL;
  y_vector = NULL;
  x_error = NULL;
  y_error = NULL;
  
  if(argc < 10 ) {
    Tcl_SetResult(interp, "usage: powProcessCurve curve bbox rbbox xinc yinc magstep tags <Line|Points> canvas", TCL_VOLATILE);
    return TCL_ERROR;
  }

  curve_ptr = PowFindCurve(argv[1]);
  if ((curve_ptr->x_vector)!= NULL ) { x_vect = (curve_ptr->x_vector)->dataptr;}
  if ((curve_ptr->x_error)!= NULL) {x_err = (curve_ptr->x_error)->dataptr;}
  if ((curve_ptr->y_vector)!= NULL){ y_vect = (curve_ptr->y_vector)->dataptr;}
  if ((curve_ptr->y_error)!= NULL) {y_err = (curve_ptr->y_error)->dataptr;}
  
  xoff = 0;
  yoff = 0;
  xeoff = 0;
  yeoff = 0;
  if (x_vector != NULL) xoff = x_vector->offset;
  if (y_vector != NULL) yoff = y_vector->offset;
  if (x_error != NULL) xeoff = x_error->offset;
  if (y_error != NULL) yeoff = y_error->offset;
  
  Tcl_SplitList(interp,argv[2],&n,&bboxptr);
  if(n != 4) {
    Tcl_SetResult(interp, "bbox malformed", TCL_VOLATILE);
    return TCL_ERROR;
  }
  /* (x0,y0) -- lower left */
  /* (x1,y1) -- upper right */
  
  Tcl_GetDouble(interp,bboxptr[0],&x0);
  Tcl_GetDouble(interp,bboxptr[3],&y0);
  Tcl_GetDouble(interp,bboxptr[2],&x1);
  Tcl_GetDouble(interp,bboxptr[1],&y1);
  /*  ckfree((void *)bboxptr);*/
  
  Tcl_SplitList(interp,argv[3],&n,&rbboxptr);
  if(n != 4) {
    Tcl_SetResult(interp, "rbbox malformed", TCL_VOLATILE);
    return TCL_ERROR;
  }
  /* (x0,y0) -- lower left */
  /* (x1,y1) -- upper right */
  
  Tcl_GetDouble(interp,rbboxptr[0],&rx0);
  Tcl_GetDouble(interp,rbboxptr[3],&ry0);
  Tcl_GetDouble(interp,rbboxptr[2],&rx1);
  Tcl_GetDouble(interp,rbboxptr[1],&ry1);
  /*ckfree(rbboxptr);*/
  Tcl_GetDouble(interp,argv[4],&xinc);
  Tcl_GetDouble(interp,argv[5],&yinc);
  Tcl_GetDouble(interp,argv[6],&xmagstep);
  ymagstep = xmagstep;
  if (rx0 <= rx1) {
    t_rx0 = rx0 - 3.0 * xinc;
    t_rx1 = rx1 + 3.0 * xinc;
  } else {
    t_rx1 = rx1 - 3.0 * xinc;
    t_rx0 = rx0 + 3.0 * xinc;
  }
  if (ry0 <= ry1) {
    t_ry0 = ry0 - 3.0 * yinc;
    t_ry1 = ry1 + 3.0 * yinc;
  } else {
    t_ry1 = ry1 - 3.0 * yinc;
    t_ry0 = ry0 + 3.0 * yinc;
  }
  
  pflag = 0;
  lflag = 0;
  if (strstr(argv[8],"Points")) { pflag = 1;}
  if (strstr(argv[8],"Line")) { lflag = 1;}  
  if((x_err == NULL && y_err == NULL) ||  lflag ) {
    /* just draw a bunch of line segments */
    dflag = 0;
    if (x_vect != NULL) {
      rx = PowExtractDatum(x_vect,xoff);
    } else {
      rx = 1.0;
    }
    if (y_vect != NULL) {
      ry = PowExtractDatum(y_vect,yoff);
    } else {
      ry = 1.0;
    }
    if (rx != DBL_MAX) {
      oldx = (rx - rx0)*xmagstep/xinc + x0;
    } else {
      oldx = DBL_MAX;
    }
    if (ry != DBL_MAX) {
      oldy = y0 - (ry - ry0)*ymagstep/yinc;
    } else {
      oldy = DBL_MAX;
    }
    for (i=0;i<curve_ptr->length;i++) {
      rx++;
      ry++;
      if (x_vect != NULL) {
	rx = PowExtractDatum(x_vect,i+xoff);
      } 
      if ((rx >= t_rx0 && rx <= t_rx1) || (rx >= t_rx1 && rx <= t_rx0) ) {
	x = (rx - rx0)*xmagstep/xinc + x0;
	if (y_vect != NULL) {
	  ry = PowExtractDatum(y_vect,i+yoff);
	}
	if ((ry >= t_ry0 && ry <= t_ry1) || (ry >= t_ry1 && ry <= t_ry0) ) {
	  y = y0 - (ry - ry0)*ymagstep/yinc;
	  if (pflag) {
	    /* if drawing points, make a little 7 pixel cross */
	    p1 = (int)(x + 3);
	    p2 = (int)(x - 3);
	    p3 = (int)(x);
	    q1 = (int)(y + 3);
	    q2 = (int)(y - 3);
	    q3 = (int)(y);
	  sprintf(PlotCommand," %d %d %d %d ",p2,q3,p1,q3);
	  Tcl_VarEval(interp,argv[9]," create line ",PlotCommand,"-tags {",argv[7],"} ", (char *) NULL);
	  sprintf(PlotCommand," %d %d %d %d ",p3,q2,p3,q1);
	  Tcl_VarEval(interp,argv[9]," create line ",PlotCommand,"-tags {",argv[7],"} ", (char *) NULL);
	  }	    
	  if (dflag && lflag) {
	    /* don't draw lines from points off the graph */
	    sprintf(PlotCommand," %.0f %.0f %.0f %.0f ",oldx,oldy,x,y);
	    Tcl_VarEval(interp,argv[9]," create line ",PlotCommand,"-tags {",argv[7],"}", (char *) NULL);
	  }
	  dflag = 1;
	  oldx = x;
	  oldy = y;
	  /*update every 100 points */
	  if (i - lasti > 100) {
	    Tcl_Eval(interp,"update idletasks");
	    lasti = i;
	  }
	} else {
	  /* don't draw lines from points off the graph */
	  dflag = 0;
	}
      }
    }
  } else { 
    /* plot points */
    rx = 0.0;
    ry = 0.0;
    for (i=0;i<curve_ptr->length;i++) {
      rx++;
      ry++;
      if (x_vect != NULL) {
	rx = PowExtractDatum(x_vect,i+xoff);
      }
      if ((rx >= t_rx0 && rx <= t_rx1) || (rx >= t_rx1 && rx <= t_rx0) ) {
	x = (rx-rx0)*xmagstep/xinc + x0;
	if (x_err != NULL) {
	  xe = PowExtractDatum(x_err,i+xeoff)*xmagstep/xinc ;
	} else {
	  xe = 0.0;
	}
	if (y_vect != NULL) {
	  ry = PowExtractDatum(y_vect,i+yoff);
	}
	if ((ry >= t_ry0 && ry <= t_ry1) || (ry >= t_ry1 && ry <= t_ry0) ) {
	  y = y0 - (ry - ry0)*ymagstep/yinc;
	  if (y_err != NULL) {
	    ye = PowExtractDatum(y_err,i+yeoff)*ymagstep/yinc ;
	  } else {
	    ye = 0.0;
	  }
	  p1 = (int)(x + xe);
	  p2 = (int)(x - xe);
	  p3 = (int)(x);
	  q1 = (int)(y + ye);
	  q2 = (int)(y - ye);
	  q3 = (int)(y);
	  sprintf(PlotCommand," %d %d %d %d ",p2,q3,p1,q3);
	  Tcl_VarEval(interp,argv[9]," create line ",PlotCommand,"-tags {",argv[7],"} ", (char *) NULL);
	  sprintf(PlotCommand," %d %d %d %d ",p3,q2,p3,q1);
	  Tcl_VarEval(interp,argv[9]," create line ",PlotCommand,"-tags {",argv[7],"} ", (char *) NULL);
	  /*update every 100 points */
	  if (i - lasti > 100) {
	    Tcl_Eval(interp,"update idletasks");
	    lasti = i;
	  }
	}
      }
    }
  }
  return TCL_OK;
}



  
int PowSetGraphMagstep(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  int status = TCL_OK;
  PowGraph *graph_ptr;
  if(argc != 4) {
    Tcl_SetResult(interp, "usage: powSetGraphMagstep graphname newxmagstep newymagstep", TCL_VOLATILE);
    return TCL_ERROR;
  }
  graph_ptr = PowFindGraph(argv[1]);
  if (graph_ptr == (PowGraph *) NULL) {
    Tcl_SetResult(interp, "Couldn't find graph.", TCL_VOLATILE);
    return TCL_ERROR;
  }
  status = Tcl_GetDouble(interp,argv[2],&(graph_ptr->xmagstep));
  if( status==TCL_OK )
     status = Tcl_GetDouble(interp,argv[3],&(graph_ptr->ymagstep));
  return status;
}
  

int PowGetImageOrigin(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  PowImage *image_ptr;
  char longStr[1024];
  if(argc != 3) {
    Tcl_SetResult(interp, "wrong # args", TCL_VOLATILE);
    return TCL_ERROR;
  }
  image_ptr = PowFindImage(argv[1]);
  if (image_ptr == (PowImage *) NULL) {
    Tcl_SetResult(interp, "Couldn't find image.", TCL_VOLATILE);
    return TCL_ERROR;
  }
  switch (*argv[2]) {
  case 'X':  sprintf(longStr,"%le", image_ptr->xorigin);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  case 'Y':  sprintf(longStr,"%le", image_ptr->yorigin);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  default: Tcl_SetResult(interp, "No such image axis (must be X or Y)", TCL_VOLATILE);
    return TCL_ERROR;
  }
  return TCL_OK;
}


int PowGetImageOtherend(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  PowImage *image_ptr;
  char longStr[1024];
  if(argc != 3) {
    Tcl_SetResult(interp, "wrong # args", TCL_VOLATILE);
    return TCL_ERROR;
  }
  image_ptr = PowFindImage(argv[1]);
  if (image_ptr == (PowImage *) NULL) {
    Tcl_SetResult(interp, "Couldn't find image.", TCL_VOLATILE);
    return TCL_ERROR;
  }
  switch (*argv[2]) {
  case 'X':  sprintf(longStr,"%le", image_ptr->xotherend);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  case 'Y':  sprintf(longStr,"%le", image_ptr->yotherend);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  default: Tcl_SetResult(interp, "No such image axis (must be X or Y)", TCL_VOLATILE);
    return TCL_ERROR;
  }
  return TCL_OK;
}
		       


int PowGetImageUnits(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  PowImage *image_ptr;
  char longStr[1024];
  if(argc != 3) {
    Tcl_SetResult(interp, "wrong # args", TCL_VOLATILE);
    return TCL_ERROR;
  }
  image_ptr = PowFindImage(argv[1]);
  if (image_ptr == (PowImage *) NULL) {
    Tcl_SetResult(interp, "Couldn't find image.", TCL_VOLATILE);
    return TCL_ERROR;
  }
  switch (*argv[2]) {
  case 'X':  sprintf(longStr,"%s", image_ptr->xunits);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  case 'Y':  sprintf(longStr,"%s", image_ptr->yunits);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  case 'Z':  sprintf(longStr,"%s", image_ptr->zunits);
             Tcl_SetResult(interp, longStr, TCL_OK);
    break;
  default: Tcl_SetResult(interp, "No such image axis (must be X or Y or Z)", TCL_VOLATILE);
    return TCL_ERROR;
  }
  return TCL_OK;
}



int PowGetImageZ( ClientData clientData, Tcl_Interp *interp, 
                  int argc, Tcl_Obj *const argv[] )
{
  char *imgName;
  PowImage *image_ptr;
  int x,y;
  double datum;
#ifdef __WIN32__
  __int64 datumL;
#else
  long long datumL;
#endif
  Tcl_Obj *res;
  char longStr[1024];

  if(argc != 4) {
    Tcl_SetResult( interp, "usage: powGetImageZ image X Y", TCL_VOLATILE );
    return TCL_ERROR;
  }

  imgName   = Tcl_GetStringFromObj( argv[1], NULL );
  image_ptr = PowFindImage( imgName );
  if (image_ptr == (PowImage *) NULL) {
    Tcl_AppendResult(interp, "Couldn't find image: ",imgName, NULL);
    return TCL_ERROR;
  }
  
  Tcl_GetIntFromObj(interp,argv[2],&x);
  Tcl_GetIntFromObj(interp,argv[3],&y);

  datum = PowExtractDatum(image_ptr->dataptr,y * image_ptr->width + x);
  
  if ( image_ptr->dataptr->data_type == STRING_DATA ) {
     if ( datum >= DBL_MAX ) {
        datumL = PowExtractDatumLong(image_ptr->dataptr,y * image_ptr->width + x);
 
        if (datumL >= LONGLONG_MAX || datumL <= -1 * LONGLONG_MAX) {
           res = Tcl_NewStringObj( "NULL", -1 );
        } else {

#ifdef __WIN32__
           sprintf(longStr, "%I64d", datumL);
#else
           sprintf(longStr, "%lld", datumL);
#endif
           res = Tcl_NewStringObj( longStr, -1 );
        }
     } else {
        res = Tcl_NewDoubleObj( datum );
     }
  } else {
     if (datum >= DBL_MAX || datum <= -1 * DBL_MAX) {
        res = Tcl_NewStringObj( "NULL", -1 );
     } else {
        res = Tcl_NewDoubleObj( datum );
     }
  }
  Tcl_SetObjResult( interp, res );

  return TCL_OK;
}


int PowTestMacMemory( ClientData clientData, Tcl_Interp *interp, 
                      int argc, Tcl_Obj *const argv[] )
{
  int enoughMemory = 1;
#if !(defined(__WIN32__) || defined(macintosh))
  int npixels;
  long dmy, appMem, appBlock, tmpBlock, tmpMem;
#endif

#ifdef macintosh
  /*  This routine makes Mac-specific tests for the amount of memory left  */
  if(argc != 2) {
    Tcl_SetResult( interp, "usage: powTestMacMemory npixels", TCL_VOLATILE );
    return TCL_ERROR;
  }

  Tcl_GetIntFromObj(interp,argv[1],&npixels);

  PurgeSpace( &appMem, &appBlock );
  tmpBlock = TempMaxMem(&dmy);
  dmy      = MaxBlockSys();
  if( dmy>tmpBlock ) tmpBlock=dmy;
  tmpMem   = FreeMemSys() + TempFreeMem();

  dmy = tmpMem + tmpBlock;
  if (          appMem + tmpMem < 12*npixels + 2048000  
       ||  ( tmpBlock < 6*npixels && appBlock < 6*npixels ) ) {
      enoughMemory = 0;
  }
  
#endif
  
  Tcl_SetObjResult( interp, Tcl_NewIntObj( enoughMemory ) );
  return TCL_OK;
}


int PowPutZoomedBlock(ClientData clientData, Tcl_Interp *interp, 
		      int argc, char *argv[]) {
  /* calls the VISU routine Tk_PictPutZoomedBlock or
     Tk_PhotoPutZoomedBlock*/
  /* this is a low level routine */
  /* don't call it yourself, unless you're rewriting POW */

  /* usage: powPutZoomedBlock imageName graphName x y width height zoomX zoomY 
            */
  /* Note, the x and y refer to the source, not the target block */
  char imageName[1024] = "",graphName[1024] = "",dispImageName[1024] = "";
  int xpix, ypix, width,height;
  double x,y;
  double zoomX, zoomY, Xoff, Yoff;
  Tk_PhotoHandle photo_handle, photo_disphandle;
  Tk_PhotoImageBlock photo_block;
  int pseudoImages;
  PowImage *image_instance;
#if !(defined(__WIN32__) || defined(macintosh))
  Tk_PictHandle pict_handle, pict_disphandle;
  Tk_PictImageBlock pict_block;
#endif

  Tcl_GetInt(interp,Tcl_GetVar(interp,"powPseudoImages",TCL_GLOBAL_ONLY),
	    &pseudoImages);


  if (argc != 9) {
    Tcl_SetResult(interp, "usage: powPutZoomedBlock imageName graphName x y width height zoomX zoomY\nYou probably shouldn't be seeing this.", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  strcpy(imageName,argv[1]);
  strcpy(graphName,argv[2]);
  Tcl_GetDouble(interp,argv[3],&x);
  Tcl_GetDouble(interp,argv[4],&y);
  Tcl_GetInt(interp,argv[5],&width);
  Tcl_GetInt(interp,argv[6],&height);
  Tcl_GetDouble(interp,argv[7],&zoomX);
  Tcl_GetDouble(interp,argv[8],&zoomY);

  if( pseudoImages ) {
#if !(defined(__WIN32__) || defined(macintosh))
    /* use Pict widget (Visu) */
    if ((pict_handle = Tk_FindPict(imageName)) == NULL) {
      Tcl_AppendResult(interp, "image \"", imageName, "\" doesn't",
		       " exist or is not a Pict image", (char *) NULL);
      return TCL_ERROR;
    }

    xpix = (int)(x+0.5);
    ypix = (int)(y+0.5);
    Xoff = (xpix-x+0.5) * zoomX;
    Yoff = (ypix-y+0.5) * zoomY;

    Tk_PictGetImage(pict_handle,&pict_block);

    pict_block.pixelPtr = pict_block.pixelPtr 
      + ypix * pict_block.pitch * pict_block.pixelSize
       + xpix * pict_block.pixelSize;

    strcat(dispImageName,imageName);
    strcat(dispImageName,"disp");
    strcat(dispImageName,graphName);
    
    if ((pict_disphandle = Tk_FindPict(dispImageName)) == NULL) {
      Tcl_AppendResult(interp, "image \"", imageName, "\" doesn't",
		       "have a displayed instance on graph \"", graphName,
		       "\"", (char *) NULL);
      return TCL_ERROR;
    }
    
    Tk_PictPutScaledBlock(pict_disphandle,&pict_block,0,0,width,height,
			  zoomX, zoomY, Xoff, Yoff);
    return TCL_OK;
#else
    Tcl_AppendResult(interp,"You should not see this",NULL);
    return TCL_ERROR;
#endif /*__WIN32__ || macintosh*/
  } else {
    /* use Photo widget */
    if ((photo_handle = Tk_FindPhoto(interp,imageName)) == NULL) {
      Tcl_AppendResult(interp, "image \"", imageName, "\" doesn't exist",
		       (char *) NULL);
      return TCL_ERROR;
    }


    Tk_PhotoGetImage(photo_handle,&photo_block);


    strcat(dispImageName,imageName);
    strcat(dispImageName,"disp");
    strcat(dispImageName,graphName);
    
    if ((photo_disphandle = Tk_FindPhoto(interp,dispImageName)) == NULL) {
      Tcl_AppendResult(interp, "image \"", imageName, "\" doesn't",
		       "have a displayed instance on graph \"", graphName,
		       "\"", (char *) NULL);
      return TCL_ERROR;
    }
    
    image_instance = PowFindImage(imageName);

    xpix = (int)(x+0.5);
    Xoff = (xpix-x+0.5) * zoomX;

    y   += (height-1)/zoomY;
    ypix = (int)(y+0.5);
    Yoff = ( (y+0.5) - ypix ) * zoomY + 1.0;
    if( Yoff>zoomY ) Yoff = zoomY;

    photo_block.pixelPtr = photo_block.pixelPtr 
      + (image_instance->height - 1 - ypix) * photo_block.pitch 
      + xpix * photo_block.pixelSize; 

    Pow_PhotoPutScaledBlock(photo_disphandle,&photo_block,0,0,width,height,
			   zoomX, zoomY, Xoff, Yoff );
    return TCL_OK;
  }  
}


int PowDestroyData_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powDestroyData data_name*/
  int status=0;

  if (argc != 2) {
    Tcl_SetResult(interp,"usage: powDestroyData data_name",TCL_VOLATILE);
    return TCL_ERROR;
  }


  PowDestroyData(argv[1],&status);
  
  if (status != 0) {
    /* Result already set to error message by PowDestroyData*/
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCloneData(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /*usage: powCloneData new_data_name old_data_name ?offset? ?length? ?copy?*/

  int data_type,length,status = 0;
  void *databuff;
  int copy, offset;
  PowData *old_data;
  
  if(argc < 3 || argc > 6) {
    Tcl_SetResult(interp,
    "usage: powCloneData new_data_name old_data_name ?offset? ?length? ?copy?",
		  TCL_VOLATILE);
    return TCL_ERROR;
  }

  old_data = PowFindData(argv[2]);

  if (old_data == NULL) {
    Tcl_SetResult(interp,"Couldn't find data: ",TCL_VOLATILE);
    Tcl_AppendResult(interp,argv[2],(char *)NULL);
    return TCL_ERROR;
  }


  if (argc > 3) {
    Tcl_GetInt(interp,argv[3],&offset);
  } else {
    offset = 0;
  } 

  if (argc <= 4 || strstr(argv[4],"NULL") != NULL ) {
    length = old_data->length;
  } else {
    Tcl_GetInt(interp,argv[4],&length);
  }
  
  /* don't allow the new data object to run off the end of the old */
  if (length + offset > old_data->length) {
    length = old_data->length - offset;
  }

  if (argc > 5) {
    copy = Tcl_GetInt(interp,argv[5],&copy);
  } else {
    copy = 0;
  }

  if (copy < 0 && offset != 0) {
    Tcl_SetResult(interp,"Can't clone data with nonzero offset!",TCL_VOLATILE);
    return TCL_ERROR;
  }


  data_type = old_data->data_type;

  databuff = (void *)((char *)old_data->data_array + offset * pixelSizes[data_type]);

  PowCreateData(argv[1],databuff,&data_type,&length,&copy,&status);

  if(status != 0) {
    Tcl_SetResult(interp,"powCloneData failed",TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}


int PowCreateDataFlip_Tcl(ClientData clientData, Tcl_Interp *interp,
                          int argc, char *argv[]) {
  /* this is the TCL wrapper for the PowCreateDataFlip routine.  */
  /* the pointer to the data should be converted using sprintf(PTRFORMAT,pointer) */
    
  int status = 0;
  char *direction;
  int height, width;
    
  if(argc < 3) {
    Tcl_SetResult(interp,"usage: powCreateDataFlip data_name direction height width",TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  direction = argv[2];
  Tcl_GetInt(interp,argv[3],&height);
  Tcl_GetInt(interp,argv[4],&width);

  PowCreateDataFlip(argv[1], direction, &height, &width, &status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't flip data.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}


int PowCreateCurveFlip_Tcl(ClientData clientData, Tcl_Interp *interp,
                          int argc, char *argv[]) {
  /* this is the TCL wrapper for the PowCreateCurveFlip routine.  */
  /* the pointer to the data should be converted using sprintf(PTRFORMAT,pointer) */
    
  PowGraph *graph;
  int status = 0;
  char *direction;
  char *graphName;
  const char *canvas;
  double x, y;
  Point bbox_ll, bbox_ur;
  int i;
  char cmdLine[1024];
  const char **list;
    
  if(argc < 3) {
    Tcl_SetResult(interp,"usage: powCreateCurveFlip data_name canvas direction",TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  graphName = argv[1];
  canvas    = argv[2];
  direction = argv[3];

  graph = PowFindGraph(graphName);

  sprintf(cmdLine, "%s coords %sbox", canvas, graphName );
  if ( Tcl_Eval(interp,cmdLine)!=TCL_OK ) {
       Tcl_SetResult(interp,"Couldn't get bounding box", TCL_VOLATILE);
      return TCL_ERROR;
  }
  strncpy(cmdLine,Tcl_GetStringResult(interp),256);
  Tcl_SplitList(interp,cmdLine,&i,&list);
  Tcl_GetDouble(interp,list[0],&(bbox_ll.x));
  Tcl_GetDouble(interp,list[1],&(bbox_ur.y));
  Tcl_GetDouble(interp,list[2],&(bbox_ur.x));
  Tcl_GetDouble(interp,list[3],&(bbox_ll.y));

  ckfree((char *) list);

  PowPixToPos(bbox_ll.x, bbox_ll.y, &graph->WCS, &x, &y);

  PowCreateCurveFlip(graphName, direction, &status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't flip Curve data.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PowCreateData_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateData data_name data_pointer data_type length ?copy?  */
  /* this is the TCL wrapper for the PowCreateData routine.  */
  /* the pointer to the data should be converted using sprintf(PTRFORMAT,pointer) */
  
  int data_type,length,status = 0;
  void *databuff;
  int copy;

  
  if(argc < 5) {
    Tcl_SetResult(interp,"usage: powCreateData data_name data_pointer data_type length ?copy?",TCL_VOLATILE);
    return TCL_ERROR;
  }

  if(strstr(argv[3],"BYTE") != NULL){
    data_type = BYTE_DATA;
  } else if (strstr(argv[3],"SHORTINT") != NULL) {
    data_type = SHORTINT_DATA ;
  } else if (strstr(argv[3],"INT") != NULL) {
    data_type = INT_DATA ;
  } else if (strstr(argv[3],"REAL") != NULL) {
    data_type = REAL_DATA ;
  } else if (strstr(argv[3],"FLOAT") != NULL) {
    data_type = REAL_DATA ;
  } else if (strstr(argv[3],"DOUBLE") != NULL) {
    data_type = DOUBLE_DATA;
  } else {
    Tcl_GetInt(interp,argv[3],&data_type);
  }



  Tcl_GetInt(interp,argv[4],&length);
  if (sscanf(argv[2],PTRFORMAT,&databuff) != 1) {
    Tcl_SetResult(interp, "Couldn't parse data address into an integer", TCL_VOLATILE);
    return TCL_ERROR;
  }
  if (argc == 6) {
    Tcl_GetInt(interp,argv[5],&copy);
  } else {
    copy = 0;
  }
  
  PowCreateData(argv[1],databuff,&data_type,&length,&copy,&status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create data.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PowFindData_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /*This is a wrapper on PowFindData to see if image data exists */
  PowData *powdatabuff;

  if (argc != 2) {
     Tcl_SetResult(interp,"usage: powFindData data_name",TCL_VOLATILE);
     return TCL_ERROR;
  }

  powdatabuff = PowFindData(argv[1]);

  if (powdatabuff == (PowData *)NULL ) {
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PowRegisterData_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /*This is for people who create their PowData objects outside of pow */
  /*(e.g. users of the LHEA orbit libraries) but then want to import them */
  /*into POW. */
  PowData *powdatabuff;
  int status=0;

  if(argc != 2) {
        Tcl_SetResult(interp,"usage: powRegisterData PowData_pointer",TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (sscanf(argv[1],PTRFORMAT,&powdatabuff) != 1) {
    Tcl_SetResult(interp, "Couldn't parse powdata address into an integer", TCL_VOLATILE);
    return TCL_ERROR;
  }

  PowRegisterData(powdatabuff,&status);

  if (status != 0) {
    Tcl_SetResult(interp,"Couldn't register powdata.",TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}


int PowDestroyImage_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powDestroyImage image_name*/
  int status=0;

  if (argc != 2) {
    Tcl_SetResult(interp,"usage: powDestroyImage image_name",TCL_VOLATILE);
    return TCL_ERROR;
  }


  PowDestroyImage(argv[1],&status);
  
  if (status != 0) {
    /* Result already set to error message by PowDestroyImage*/
    return TCL_ERROR;
  }
  return TCL_OK;
}


int PowCreateImage_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateImage image_name data_name xoffset yoffset\  */
  /*        width height xorigin xinc yorigin yinc xunits yunits zunits */
  /* this is the TCL wrapper for the PowCreateImage routine.  */
  
  int xoffset,yoffset,width,height,status=0;
  double xorigin,xinc,yorigin,yinc;


  if(argc != 14) {
    Tcl_SetResult(interp, "usage: powCreateImage image_name data_name xoffset yoffset\\\n        width height xorigin xinc yorigin yinc xunits yunits zunits", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_GetInt(interp,argv[3],&xoffset);
  Tcl_GetInt(interp,argv[4],&yoffset);
  Tcl_GetInt(interp,argv[5],&width);
  Tcl_GetInt(interp,argv[6],&height);
  Tcl_GetDouble(interp,argv[7],&xorigin);
  Tcl_GetDouble(interp,argv[8],&xinc);
  Tcl_GetDouble(interp,argv[9],&yorigin);
  Tcl_GetDouble(interp,argv[10],&yinc);
 
/* fprintf(stdout, "PowCommands before calling PowCreateImage\n"); */
/* fprintf(stdout, "xoffset: %f\n", xoffset);*/
/* fprintf(stdout, "yoffset: %f\n", yoffset);*/
/* fprintf(stdout, "width: %f\n", width);*/
/* fprintf(stdout, "height: %f\n", height);*/
/* fprintf(stdout, "xorigin: %f\n", xorigin);*/
/* fprintf(stdout, "yorigin: %f\n", yorigin);*/
/* fprintf(stdout, "xinc: %f\n", xinc);*/
/* fprintf(stdout, "yinc: %f\n", yinc);*/
/* fprintf(stdout, "argv[11]: %s\n", argv[11]);*/
/* fprintf(stdout, "argv[12]: %s\n", argv[12]);*/
/* fprintf(stdout, "argv[13]: %s\n", argv[13]);*/
/* fprintf(stdout, "status: %d\n", status);*/

  PowCreateImage(argv[1],argv[2],&xoffset,&yoffset,&width,&height,
		 &xorigin,&xinc,&yorigin,&yinc,argv[11],argv[12],
		 argv[13],&status);

/* fprintf(stdout, "xoffset: %f\n", xoffset);*/
/* fprintf(stdout, "yoffset: %f\n", yoffset);*/
/* fprintf(stdout, "width: %f\n", width);*/
/* fprintf(stdout, "height: %f\n", height);*/
/* fprintf(stdout, "xorigin: %f\n", xorigin);*/
/* fprintf(stdout, "yorigin: %f\n", yorigin);*/
/* fprintf(stdout, "xinc: %f\n", xinc);*/
/* fprintf(stdout, "yinc: %f\n", yinc);*/
/* fprintf(stdout, "argv[11]: %s\n", argv[11]);*/
/* fprintf(stdout, "argv[12]: %s\n", argv[12]);*/
/* fprintf(stdout, "argv[13]: %s\n", argv[13]);*/
/* fprintf(stdout, "status: %d\n", status);*/

/* fprintf(stdout, "done calling PowCommands\n"); */
  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create image.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}


int PowDestroyVector_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powDestroyVector vector_name*/
  int status=0;

  if (argc != 2) {
    Tcl_SetResult(interp,"usage: powDestroyVector vector_name",TCL_VOLATILE);
    return TCL_ERROR;
  }


  PowDestroyVector(argv[1],&status);
  
  if (status != 0) {
    /* Result already set to error message by PowDestroyVector*/
    return TCL_ERROR;
  }
  return TCL_OK;
}


int PowCreateVector_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateVector vector_name data_name offset length\  */
  /*        units */
  /* this is the TCL wrapper for the PowCreateVector routine.  */
  
  int offset,status=0;
  int *length;


  if(argc != 6) {
    Tcl_SetResult(interp, "usage: powCreateVector vector_name data_name offset length units", TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_GetInt(interp,argv[3],&offset);
  if ( strstr(argv[4],"NULL") == NULL ) {
    length = (int *) ckalloc(sizeof(int));
    Tcl_GetInt(interp,argv[4],length);
  } else {
    length = NULL;
  }

  
  PowCreateVector(argv[1],argv[2],&offset,length,argv[5],&status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create vector.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PowDestroyCurve_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powDestroyCurve curve_name*/
  int status=0;

  if (argc != 2) {
    Tcl_SetResult(interp,"usage: powDestroyCurve curve_name",TCL_VOLATILE);
    return TCL_ERROR;
  }


  PowDestroyCurve(argv[1],&status);
  
  if (status != 0) {
    /* Result already set to error message by PowDestroyCurve*/
    return TCL_ERROR;
  }
  return TCL_OK;
}



int PowCreateCurve_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateCurve curve_name x_vector x_error y_vector y_error\  */
  /*        <z_vector z_error> */
  /* this is the TCL wrapper for the PowCreateVector routine.  */
  
  char *z_vector, *z_error;
  int status=0;


  if(argc < 6 || argc == 7) {
    Tcl_SetResult(interp, "usage: powCreateCurve curve_name x_vector x_error y_vector y_error <z_vector z_error>", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  if(argc == 6) {
    z_vector = ckalloc(sizeof("NULL"));
    strcpy(z_vector,"NULL");
    z_error = ckalloc(sizeof("NULL"));
    strcpy(z_error,"NULL");
  } else {
    z_vector = argv[6];
    z_error = argv[7];
  }


  PowCreateCurve(argv[1],argv[2],argv[3],argv[4],argv[5],z_vector,z_error,&status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create curve.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}


int PowCreateVectorEN_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateVectorEN vector_name data_name length start
  increment units */
  /* this is the TCL wrapper for the PowCreateVector routine.  */
  
  int length,status=0;
  double start,increment;
  PowData *data_instance;
  char ptrString[40];


  if(argc != 7) {
    Tcl_SetResult(interp, "usage: powCreateVectorEN vector_name data_name length start increment units", TCL_VOLATILE);
    return TCL_ERROR;
  }


  Tcl_GetInt(interp,argv[3],&length);
  Tcl_GetDouble(interp,argv[4],&start);
  Tcl_GetDouble(interp,argv[5],&increment);

  
  PowCreateVectorEN(argv[1],argv[2],&length,&start,&increment,argv[6],&status);


  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create vector.", TCL_VOLATILE);
    return TCL_ERROR;
  }


  data_instance = PowFindData(argv[1]);

  /*Return the string representation of the PowData pointer */
  sprintf(ptrString,PTRFORMAT,data_instance);
  Tcl_SetResult(interp,ptrString,TCL_VOLATILE);


  return TCL_OK;
}

int PowDataPtr_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, Tcl_Obj *const argv[]) {
   /* usage: powDataPtr data_name */
  
  PowData *data_instance;
  char ptrString[40];

  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powDataPtr data_name", TCL_VOLATILE);
    return TCL_ERROR;
  }

  data_instance = PowFindData( Tcl_GetStringFromObj( argv[1], NULL ) );

  /*Return the string representation of the PowData pointer */
  sprintf(ptrString,PTRFORMAT,data_instance);
  Tcl_SetResult(interp,ptrString,TCL_VOLATILE);

  return TCL_OK;
}

int PowCreateHisto_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateHisto histo_name x_vector y_vector */
  /* this is the TCL wrapper for the PowCreateHisto routine.  */
  
  int status=0;


  if(argc != 4) {
    Tcl_SetResult(interp, "usage: powCreateHisto histo_name x_vector y_vector", TCL_VOLATILE);
    return TCL_ERROR;
  }
  

  PowCreateHisto(argv[1],argv[2],argv[3],&status);

  if(status != 0) {
    Tcl_SetResult(interp, "Couldn't create histo.", TCL_VOLATILE);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int PowDestroyGraph_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powDestroyGraph graph_name*/
  int status=0;

  if (argc != 2) {
    Tcl_SetResult(interp,"usage: powDestroyGraph graph_name",TCL_VOLATILE);
    return TCL_ERROR;
  }


  PowDestroyGraph(argv[1],&status);
  
  if (status != 0) {
    /* Result already set to error message by PowDestroyGraph*/
    return TCL_ERROR;
  }
  return TCL_OK;
}


int PowCreateGraph_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powCreateGraph graph_name curves images xunits yunits xlabel\  */
  /*        ylabel xdimdisp ydimdisp <xmin ymin xmax ymax> */
  /* this is the TCL wrapper for the PowCreateImage routine.  */
  
  int xdimdisp,ydimdisp,*pxdimdisp,*pydimdisp,status=0;
  double rxdimdisp,rydimdisp;
  double xmin,xmax,ymin,ymax;
  double *pxmin,*pxmax,*pymin,*pymax;
  char *whichPowCanvas;
  Tcl_DString errMsg;

  pxmin = &xmin;
  pxmax = &xmax;
  pymin = &ymin;
  pymax = &ymax;

  pxdimdisp = &xdimdisp;
  pydimdisp = &ydimdisp;

  if(argc < 8) {
    Tcl_SetResult(interp, "usage: powCreateGraph graph_name curves images xunits yunits xlabel\\\n ylabel ?xdimdisp ydimdisp xmin ymin xmax ymax? ", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if( argc > 8 && strstr(argv[8],"NULL") == NULL ) {
     if( Tcl_GetDouble(interp,argv[8],&rxdimdisp)!=TCL_OK ) {
        pxdimdisp = NULL;
     } else {
        xdimdisp = (int) ceil(rxdimdisp);
     }
  } else {
     pxdimdisp = NULL;
  }

  if( argc > 9 && strstr(argv[9],"NULL") == NULL ) {
     if( Tcl_GetDouble(interp,argv[9],&rydimdisp)!=TCL_OK ) {
        pydimdisp = NULL;
     } else {
        ydimdisp = (int) ceil(rydimdisp);
     }
  } else {
     pydimdisp = NULL;
  }

  if (argc >= 11 && (strstr(argv[10],"NULL") == NULL)) {
    Tcl_GetDouble(interp,argv[10],pxmin);
  } else {
    pxmin = NULL;
  }

  if (argc >= 12 && (strstr(argv[11], "NULL") == NULL)) {
    Tcl_GetDouble(interp,argv[11],pymin);
  } else {
    pymin = NULL;
  }

  if (argc >= 13 && (strstr(argv[12], "NULL") == NULL)) {
    Tcl_GetDouble(interp,argv[12],pxmax);
  } else {
    pxmax = NULL;
  }


  if (argc >= 14 && (strstr(argv[13],"NULL") == NULL)) {
    Tcl_GetDouble(interp,argv[13],pymax);
  } else {
    pymax = NULL;
  }


  /* set global whichPowCanvas variable (only used internally to implement
    "scope" window, *not* for public usage at this time) */
  if (argc >=15 && (strstr(argv[14],"NULL") == NULL)) {
    whichPowCanvas=(char *) ckalloc((strlen(argv[14])+1)*sizeof(char));
    strcpy(whichPowCanvas,argv[14]);
  } else {
    whichPowCanvas=(char *) ckalloc(sizeof(".pow.pow"));
    strcpy(whichPowCanvas,".pow.pow");
  }
  

  /* This can take a while, so set POW's cursor to a watch */
  Tcl_GlobalEval(interp, "powSetCursor watch");

  PowCreateGraph_internal( argv[1], argv[2], argv[3], argv[4],
			   argv[5], argv[6], argv[7],
			   pxdimdisp, pydimdisp, pxmin, pymin, pxmax, pymax,
			   whichPowCanvas, &status);
  if( status ) {
     Tcl_DStringInit(&errMsg);
     Tcl_DStringGetResult(interp, &errMsg);
  }

  Tcl_GlobalEval(interp, "powSetCursor reset");

  ckfree(whichPowCanvas);

  if( status ) {
     Tcl_DStringAppend( &errMsg, "\nCouldn't create graph", -1 );
     Tcl_DStringResult( interp, &errMsg );
     return TCL_ERROR;
  }

  return TCL_OK;
}

int PowTestImage(ClientData clientData, Tcl_Interp *interp,
		 int argc, Tcl_Obj *const argv[] )
{
   /*******************************************************/
   /*  Test new image with old graph for WCS consistency  */
   /*******************************************************/

   PowGraph *graph;
   PowImage *image;
   double xorigin, yorigin, xotherend, yotherend, xcorner, ycorner;

   if( argc != 3 ) {
      Tcl_SetResult( interp, "Usage: powTestImage gn image", TCL_VOLATILE );
      return TCL_ERROR;
   }

   graph = PowFindGraph( Tcl_GetStringFromObj( argv[1], NULL ) );
   image = PowFindImage( Tcl_GetStringFromObj( argv[2], NULL ) );

   if( graph==NULL || image==NULL )
      return TCL_ERROR;

   /*************************************************************/
   /*  Convert origin and otherend info into pixel coordinates  */
   /*************************************************************/

   if( PowPosToPix( image->xorigin, image->yorigin,
		    &graph->WCS, &xorigin, &yorigin ) )
      return TCL_ERROR;
   if( PowPosToPix( image->xotherend, image->yotherend,
		    &graph->WCS, &xotherend, &yotherend ) )
      return TCL_ERROR;
   
   if( (graph->WCS.type[0]!='\0') != (image->WCS.type[0]!='\0') ) {
      Tcl_SetResult(interp,"WCS state of graph and image differ",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   /*************************************************/
   /*  We are in pixel coordinates, so they should  */
   /*  ALWAYS go from left->right                   */
   /*************************************************/

   if( xorigin > xotherend || yorigin > yotherend ) {
/*
fprintf(stdout, "-------> xorigin: <%20.15f>, xotherend: <%20.15f>\n", xorigin, xotherend);
fprintf(stdout, "-------> yorigin: <%20.15f>, yotherend: <%20.15f>\n", yorigin, yotherend);
fflush(stdout);
*/
      Tcl_SetResult(interp,"New image does not point in same direction",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   /***************************************************/
   /*  Images must project to an unrotated rectangle  */
   /***************************************************/

   if( PowPixToPos( -0.5, image->height-0.5, &image->WCS, &xcorner, &ycorner ) )
      return TCL_ERROR;
   if( PowPosToPix( xcorner, ycorner,        &graph->WCS, &xcorner, &ycorner ) )
      return TCL_ERROR;

   if( fabs( xcorner-xorigin ) > 1.0 || fabs( ycorner-yotherend ) > 1.0 ) {
      Tcl_SetResult(interp, "Graph and image have different rotation angles.",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   if( PowPixToPos( image->width-0.5, -0.5, &image->WCS, &xcorner, &ycorner ) )
      return TCL_ERROR;
   if( PowPosToPix( xcorner, ycorner,       &graph->WCS, &xcorner, &ycorner ) )
      return TCL_ERROR;

   if( fabs( xcorner-xotherend ) > 1.0 || fabs( ycorner-yorigin ) > 1.0 ) {
      Tcl_SetResult(interp, "Graph and image have different rotation angles.",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   return TCL_OK;
}

int PowFetchCurveInfoHash(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powFetchVectorHash curvename */
  /* used by the vector edit window to fetch component vector names */
  PowCurve *curve_ptr;
  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powFetchVectorHash curvename", TCL_VOLATILE);
    return TCL_ERROR;
  }

  curve_ptr = PowFindCurve(argv[1]);
  if (curve_ptr == (PowCurve *) NULL) {
    Tcl_SetResult(interp,"Couldn't find curve.",TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  Tcl_SetResult(interp,"X",TCL_VOLATILE);
  if (curve_ptr->x_vector) {
    Tcl_AppendResult(interp," ",
		     (curve_ptr->x_vector)->vector_name,(char *)NULL);
  } else {
    Tcl_AppendResult(interp," NULL",(char *)NULL);
  }
  if (curve_ptr->x_error) {
    Tcl_AppendResult(interp," XE ",
		     (curve_ptr->x_error)->vector_name,(char *)NULL);
  } else {
    Tcl_AppendResult(interp," XE NULL",(char *)NULL);
  }
  if (curve_ptr->y_vector) {
    Tcl_AppendResult(interp," Y ",
		     (curve_ptr->y_vector)->vector_name,(char *)NULL);
  } else {
    Tcl_AppendResult(interp," Y NULL",(char *)NULL);
  }
  if (curve_ptr->y_error) {
    Tcl_AppendResult(interp," YE ",
		     (curve_ptr->y_error)->vector_name,(char *)NULL);
  } else {
    Tcl_AppendResult(interp," YE NULL",(char *)NULL);
  }


  return TCL_OK;
}
  
int PowFetchDataLength(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powFetchDataLength data_name */
  /* used by the data edit window to fetch data structure fields */
  PowData *data_ptr;
  char length_str[22];
  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powFetchDataLength dataname", TCL_VOLATILE);
    return TCL_ERROR;
  }

  data_ptr = PowFindData(argv[1]);
  if (data_ptr == (PowData *) NULL) {
    Tcl_SetResult(interp,"Couldn't find data: ",TCL_VOLATILE);
    Tcl_AppendResult(interp,argv[1],(char *)NULL);
    return TCL_ERROR;
  }

  sprintf(length_str,"%d",data_ptr->length);
  Tcl_SetResult(interp,length_str,TCL_VOLATILE);

  return TCL_OK;
}

int PowExprDataInfo(ClientData clientData, Tcl_Interp *interp,
		    int argc, Tcl_Obj *const argv[] ) {
   /* usage: NONE!  This is a callback function to be used in    */
   /* conjunction with the fitsTcl vector expression calculator. */
   /* It locates the powData object being referenced and returns */
   /* A pointer to the data, the datatype, and the data length.  */

  PowData *data_ptr;
  Tcl_Obj *res[4];
  char ptrStr[16];

  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powExprDataInfo dataname", TCL_VOLATILE);
    return TCL_ERROR;
  }

  data_ptr = PowFindData( Tcl_GetStringFromObj(argv[1],NULL) );
  if (data_ptr == (PowData *) NULL) {
    Tcl_SetResult(interp,"Couldn't find data.",TCL_VOLATILE);
    return TCL_ERROR;
  }
  sprintf(ptrStr, PTRFORMAT, data_ptr->data_array);

  res[0] = Tcl_NewStringObj( "-ptr", -1 );
  res[1] = Tcl_NewStringObj( ptrStr, -1 );
  res[2] = Tcl_NewIntObj( data_ptr->data_type );
  res[3] = Tcl_NewIntObj( data_ptr->length    );

  Tcl_SetObjResult(interp, Tcl_NewListObj(4, res) );

  return TCL_OK;
}


int PowFetchVectorInfoHash(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powFetchVectorInfoHash vector_name */
  /* used by the vector edit window to fetch vector structure fields */
  PowVector *vector_ptr;
  char length_str[22];
  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powFetchVectorInfoHash vectorname", TCL_VOLATILE);
    return TCL_ERROR;
  }

  vector_ptr = PowFindVector(argv[1]);
  if (vector_ptr == (PowVector *) NULL) {
    Tcl_SetResult(interp,"Couldn't find vector.",TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult(interp,"data",TCL_VOLATILE);
  Tcl_AppendResult(interp," ",
		     (vector_ptr->dataptr)->data_name,(char *)NULL);
  sprintf(length_str,"%d",vector_ptr->length);
  Tcl_AppendResult(interp," length ",
		     length_str,(char *)NULL);
  Tcl_AppendResult(interp," units ",
		     vector_ptr->units,(char *)NULL);

  return TCL_OK;
 
}


int PowFetchImageInfoHash(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powFetchImageInfoHash image_name */
  /* used by the image edit window to fetch image structure fields */
  PowImage *image_ptr;
  char tmp_str[22];
  if(argc != 2) {
    Tcl_SetResult(interp, "usage: powFetchImageInfoHash imagename", TCL_VOLATILE);
    return TCL_ERROR;
  }

  image_ptr = PowFindImage(argv[1]);
  if (image_ptr == (PowImage *) NULL) {
    Tcl_SetResult(interp,"Couldn't find image.",TCL_VOLATILE);
    return TCL_ERROR;
  }

  Tcl_SetResult(interp,"data",TCL_VOLATILE);
  Tcl_AppendResult(interp," ",
		     (image_ptr->dataptr)->data_name,(char *)NULL);
  sprintf(tmp_str,"%d",image_ptr->width);
  Tcl_AppendResult(interp," width ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%d",image_ptr->height);
  Tcl_AppendResult(interp," height ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%lg",image_ptr->xorigin+0.5*image_ptr->xinc);
  Tcl_AppendResult(interp," xorigin ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%lg",image_ptr->yorigin+0.5*image_ptr->yinc);
  Tcl_AppendResult(interp," yorigin ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%lg",image_ptr->xinc);
  Tcl_AppendResult(interp," xinc ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%lg",image_ptr->yinc);
  Tcl_AppendResult(interp," yinc ",
		     tmp_str,(char *)NULL);
  Tcl_AppendResult(interp," xunits ",
		     image_ptr->xunits,(char *)NULL);
  Tcl_AppendResult(interp," yunits ",
		     image_ptr->yunits,(char *)NULL);
  if (strcmp(image_ptr->zunits, "") != 0 ) {
     Tcl_AppendResult(interp," zunits ",
		     image_ptr->zunits,(char *)NULL);
  }

  sprintf(tmp_str,"%lg",image_ptr->xotherend+0.5*image_ptr->xinc);
  Tcl_AppendResult(interp," xotherend ",
		     tmp_str,(char *)NULL);
  sprintf(tmp_str,"%lg",image_ptr->yotherend+0.5*image_ptr->yinc);
  Tcl_AppendResult(interp," yotherend ",
		     tmp_str,(char *)NULL);

  return TCL_OK;
 
}

int PowFindCurvesMinMax_Tcl(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char *argv[]) {
  /* usage: powFindCurveMinMax curves axis */
  /* this is the TCL wrapper for the PowFindCurvesMinMax routine.  */
  double min, max;
  char outstring[1024];

  if(argc != 3) {
    Tcl_SetResult(interp, "usage: powFindCurveMinMax curves axis", TCL_VOLATILE);
    return TCL_ERROR;
  }

  min = 6.66e100;
  max = -6.66e100;

  PowFindCurvesMinMax(argv[1], argv[2], &min, &max, 0);
  
  sprintf(outstring,"%g",min);

  Tcl_AppendElement(interp,outstring);
  
  sprintf(outstring,"%g",max);

  Tcl_AppendElement(interp,outstring);
  return TCL_OK;

}
  
int PowCreateDataFromChannel(ClientData clientData, Tcl_Interp *interp,
                             int argc, Tcl_Obj *const argv[] )
{
  /* usage: powCDFC channel data_name bitpix byteOrder */
  Tcl_Channel channel;
  char buffer[1024];
  char *data, *cName, *dName, *bin, *bout;
  int done=0, copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder, bytesRead, len=0;

  if( argc!=5 ) {
    Tcl_SetResult(interp, "usage: powCreateDataFromChannel chanName "
                  "data_name bitpix byteOrder", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  cName = Tcl_GetStringFromObj( argv[1], NULL );
  dName = Tcl_GetStringFromObj( argv[2], NULL );
  Tcl_GetIntFromObj( interp, argv[3], &bitpix );
  Tcl_GetIntFromObj( interp, argv[4], &byteOrder );
  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  channel = Tcl_GetChannel( interp, cName, NULL );
  if( channel==NULL ) {
    Tcl_AppendResult(interp, "Unable to find channel ", cName, NULL);
    return TCL_ERROR;
  }


  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  while( !done ) {
    bytesRead = Tcl_Read( channel, buffer, 1024 );
  
    if( len==0 ) {
      len = bytesRead;
      data = (char *) ckalloc( bytesRead * sizeof(char) );
    } else if( bytesRead>0 ) {
      len += bytesRead;
      data = (char *) ckrealloc( data, len * sizeof(char) );
      if( bytesRead<1024 ) done=1;
    } else if( bytesRead==0 ) {
      done=1;
    } else {
      /* ERROR */
      if( len>0 ) ckfree( data );
      Tcl_AppendResult(interp, "Error reading channel", NULL);
      return TCL_ERROR;
    }
      
    if( byteOrder>0 || datasize==1 ) {
/* fprintf(stdout, "memcpy\n"); */
      memcpy( data+len-bytesRead, buffer, bytesRead );
    } else {
/* fprintf(stdout, "else\n"); */
      bin  = buffer;
      bout = data+len-bytesRead;
      nPts = bytesRead/datasize;
      for( i=0; i<nPts; i++ ) {
        for( j=0; j<datasize; j++ )
          bout[datasize-j-1] = *bin++;
        bout += datasize;
      }
    }

  } /* end while() */

  nPts = len/datasize;

/* fprintf(stdout, "len = %ld\n", len); */
/* fprintf(stdout, "nPts = %ld\n", nPts); */

  PowCreateData(dName,data,&bitpix,&nPts,&copy,&status);
  if( status ) {
    Tcl_AppendResult(interp, "Error creating data ", dName, NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCreateDataFromBuffer(ClientData clientData, Tcl_Interp *interp,
                             int argc, Tcl_Obj *const argv[] )
{
  /* usage: powCDFC channel data_name bitpix byteOrder */
  char *data, *dName, *bin, *bout;
  int copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder, bytesRead, len=0;

  if( argc!=6 ) {
    Tcl_SetResult(interp, "usage: powCreateDataFromBuffer bufferName length "
                  "data_name bitpix byteOrder", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  dName = Tcl_GetStringFromObj( argv[3], NULL );
  Tcl_GetIntFromObj( interp, argv[4], &bitpix );
  Tcl_GetIntFromObj( interp, argv[5], &byteOrder );

  if ( bitpix<0 || bitpix>4 ) 
  {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  switch (bitpix) 
  {
       case 0: datasize = 1; break;
       case 1: datasize = 2; break;
       case 2: datasize = 4; break;
       case 3: datasize = 4; break;
       case 4: datasize = 8; break;
  }

  Tcl_GetIntFromObj(interp, argv[2], &bytesRead);
  len = bytesRead;

/* fprintf(stdout, "PowCreateDataFromBuffer: size of data= %ld\n", len); */ 
/* fprintf(stdout, "PowCreateDataFromBuffer: byteOrder = %d\n", byteOrder); */
/* fprintf(stdout, "PowCreateDataFromBuffer: datasize = %d\n", datasize); */

  data = (char *) ckalloc( bytesRead * sizeof(char));

  if ( byteOrder>0 || datasize==1 ) 
  {
/* fprintf(stdout, "PowCreateDataFromBuffer: memcpy\n"); */
     memcpy( data, Tcl_GetByteArrayFromObj(argv[1], NULL) , bytesRead );
  } 
  else 
  {
/* fprintf(stdout, "PowCreateDataFromBuffer: else\n"); */
      bin = Tcl_GetByteArrayFromObj(argv[1], NULL);
      bout = data;
      nPts = bytesRead/datasize;

      for ( i=0; i<nPts; i++ ) 
      {
          for ( j=0; j<datasize; j++ )
              bout[datasize-j-1] = *bin++;
          bout += datasize;
      }
  } 

  nPts = bytesRead/datasize;

/* fprintf(stdout, "PowCreateDataFromBuffer: nPts = %ld\n", nPts); */

  PowCreateData(dName,data,&bitpix,&nPts,&copy,&status);
  /* ckfree(data);  */

  if( status ) 
  {
    Tcl_AppendResult(interp, "Error creating data ", dName, NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCreateDataFromPtr(ClientData clientData, Tcl_Interp *interp,
                         int argc, Tcl_Obj *const argv[] )
{
  Tcl_Obj **dList;
  char *data, *dPtr, *dName, *bin, *bout;
  int copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder;

  if( argc!=4 ) {
    Tcl_SetResult(interp, "usage: powCreateDataFromPtr dataPtr "
                  "data_name byteOrder", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  /*  Parse dataPtr of form [address bitpix [naxes]]  */
  if( Tcl_ListObjGetElements( interp, argv[1], &i, &dList ) != TCL_OK
      || i != 3 ) {
     Tcl_AppendResult(interp, 
                      "Bad dataList parameter: address dataType naxes",
                      TCL_VOLATILE);
     return TCL_ERROR;
  }
  dPtr   = Tcl_GetStringFromObj( dList[0], NULL );
  if (sscanf(dPtr,PTRFORMAT,&data) != 1) {
    Tcl_SetResult(interp,"Couldn't parse data address into an integer",
                  TCL_VOLATILE);
    return TCL_ERROR;
  }
  Tcl_GetIntFromObj( interp, dList[1], &bitpix );

  /*  Read NAXES list and calculate data length  */

  if( Tcl_ListObjGetElements( interp, dList[2], &i, &dList ) != TCL_OK ) {
     Tcl_AppendResult(interp, "Bad naxes parameter", TCL_VOLATILE);
     return TCL_ERROR;
  }
  for( nPts=1; i; ) {
     Tcl_GetIntFromObj( interp, dList[--i], &j );
     nPts *= j;
  }

  dName  = Tcl_GetStringFromObj( argv[2], NULL );
  Tcl_GetIntFromObj( interp, argv[3], &byteOrder );
  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  if( byteOrder>0 || datasize==1 ) {
     copy = 1;
  } else {
     bin  = (char *)data;
     bout = (char *)ckalloc( nPts * datasize * sizeof(char) );
     for( i=0; i<nPts; i++ ) {
        for( j=0; j<datasize; j++ )
           bout[datasize-j-1] = *bin++;
        bout += datasize;
     }
     copy = -1;  /*  Have POW take ownership of new data array  */
     data = bout;
  }

  PowCreateData(dName,data,&bitpix,&nPts,&copy,&status);
  if( status ) {
    Tcl_AppendResult(interp, "Error creating data ", dName, NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCreateDataFromList(ClientData clientData, Tcl_Interp *interp, 
			  int argc, char *argv[]) {
  /* usage: powCreateDataFromList data_name list_o_data*/
  int largc;
  const char **largv, **counter;
  double *data, *datacounter;
  int i,j;
  int status = 0;
  PowData *data_instance;
  char ptrString[40];
  int string_flag = 0;

  if (argc < 3 || argc > 4) {
    Tcl_SetResult(interp, "usage: powCreateDataFromList data_name list_o_data ?stringflag?", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if (Tcl_SplitList(interp,argv[2],&largc,&largv) != TCL_OK) {
    Tcl_SetResult(interp,"Couldn't split input data list",TCL_VOLATILE);
    return TCL_ERROR;
  }


  if (argc == 4) {
    if (Tcl_GetBoolean(interp,argv[3],&string_flag) != TCL_OK) {
      Tcl_SetResult(interp,"Couldn't convert stringflag to boolean",
		    TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  if (string_flag) {
    i=0;
    j=STRING_DATA;
    
    PowCreateData(argv[1],(void *)largv,&j,&largc,&i,&status);

  } else {
    data = (double *)ckalloc(largc * sizeof(double));
    
    counter = largv;
    datacounter = data;
  
    for (i=0;i<largc;i++) {
      Tcl_GetDouble(interp,*counter,datacounter);
      counter++;
      datacounter++;
    }
  
  /*  ckfree((char *) largv);*/

    i=0;
    j=DOUBLE_DATA;

    PowCreateData(argv[1],data,&j,&largc,&i, &status);
  }

  if ( status != 0) {
    Tcl_SetResult(interp,"Couldn't create data",TCL_VOLATILE); 
    return TCL_ERROR;
  }

  data_instance = PowFindData(argv[1]);

  /*Since this data was made by us, we'll mark it as a POW copy so that
    PowDestroyData will free it.*/
  data_instance->copy = 1;
  

  /*Return the string representation of the PowData pointer */
  sprintf(ptrString,PTRFORMAT,data_instance);
  Tcl_SetResult(interp,ptrString,TCL_VOLATILE);
  

  return TCL_OK;
}
  
int PowCreateStrFromPtr(ClientData clientData, Tcl_Interp *interp,
                        int argc, Tcl_Obj *const argv[] )
{
  Tcl_Obj **dList;
  char *data, *dPtr;
  int i, j;
  int nPts, datasize, bitpix;

  if( argc!=4 ) {
    Tcl_SetResult(interp, "usage: powCreateStrFromPtr "
                  "address bitpix naxes", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  dPtr   = Tcl_GetStringFromObj( argv[1], NULL );
  if (sscanf(dPtr,PTRFORMAT,&data) != 1) {
    Tcl_SetResult(interp,"Couldn't parse data address into an integer",
                  TCL_VOLATILE);
    return TCL_ERROR;
  }
  Tcl_GetIntFromObj( interp, argv[2], &bitpix );

  /*  Read NAXES list and calculate data length  */

  if( Tcl_ListObjGetElements( interp, argv[3], &i, &dList ) != TCL_OK ) {
     Tcl_AppendResult(interp, "Bad naxes parameter", TCL_VOLATILE);
     return TCL_ERROR;
  }
  for( nPts=1; i; ) {
     Tcl_GetIntFromObj( interp, dList[--i], &j );
     nPts *= j;
  }


  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  Tcl_SetObjResult( interp, Tcl_NewStringObj( data, nPts * datasize ) );
  return TCL_OK;
}

int PowGraphToCanvas( ClientData clientData, Tcl_Interp *interp, 
		      int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   Tcl_Obj *list, *elem[4];
   double x, y, xorig, yorig;
   double xorig_curr, yorig_curr;
   char *canvas=".pow.pow";
   char *graphName;
   int len;
   char *idxStr;
   const char *graphType;
   int zoomed;
   int xCount, yCount;
   int graph_is_scope;

   if( argc < 4 || argc > 5 ) {
      Tcl_SetResult(interp,"usage: powGraphToCanvas graph x y ?canvas?",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }
   graphName = Tcl_GetStringFromObj( argv[1], NULL );
   graph = PowFindGraph( graphName );
   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Graph ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }
   Tcl_GetDoubleFromObj(interp, argv[2], &x);
   Tcl_GetDoubleFromObj(interp, argv[3], &y);
   if( argc==5 )
      canvas = Tcl_GetStringFromObj( argv[4], NULL );

   Tcl_VarEval(interp, canvas, " coords ",graphName, "box", NULL);

   list = Tcl_GetObjResult(interp);
   Tcl_ListObjIndex(interp, list, 0, &elem[0]);
   Tcl_ListObjIndex(interp, list, 1, &elem[1]);
   Tcl_ListObjIndex(interp, list, 2, &elem[2]);
   Tcl_ListObjIndex(interp, list, 3, &elem[3]);
   if( elem[0]==NULL || elem[3]==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Unable to find bbox for ", graphName,
		       " in ",canvas, NULL);
      return TCL_ERROR;
   }

   Tcl_GetDoubleFromObj(interp, elem[0], &xorig);
   Tcl_GetDoubleFromObj(interp, elem[3], &yorig);

   len    = strlen(graphName)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "graphType", graphName);
   graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
   ckfree(idxStr);

   len    = strlen(graphName)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "zoomed", graphName);
   zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
   ckfree(idxStr);

   xCount = atoi(Tcl_GetVar2(interp,"xCount",graphName,TCL_GLOBAL_ONLY));
   yCount = atoi(Tcl_GetVar2(interp,"yCount",graphName,TCL_GLOBAL_ONLY));

   /* Chai 06/29/2007:
      We are not actually fliping the coordinates on the canvas. If tk allows this, then there is
      no need to do the following. What the logic below is to trick pow to think that the point on
      the canvas has been flipped. The xCount and yCount indicate if the graph has been flipped
      before. So if X has been previously flipped, the next flipping occurs on Y, the logic inside
      ..Count % 2 will make sure the information on previous flip still exists. */

/*
fprintf(stdout, "powGraphToCanvas xorig, yorig (%20.15f, %20.15f)\n", xorig, yorig);
fflush(stdout);
*/
   if (strcmp(graphType, "binary") == 0 && xCount % 2 != 0) {
      Tcl_GetDoubleFromObj(interp, elem[2], &xorig_curr);
   }

   if (strcmp(graphType, "binary") == 0 && yCount % 2 != 0) {
      Tcl_GetDoubleFromObj(interp, elem[1], &yorig_curr);
   }

   if( PowPosToPix( x, y, &graph->WCS, &x, &y ) != TCL_OK )
      return TCL_ERROR;

   idxStr = strstr(graphName, "scope");
   graph_is_scope = 0;
   if (idxStr != (char *)NULL) {
      graph_is_scope = 1;
   }

/*
fprintf(stdout, "graphName: <%s>, graph_is_scope: <%d>\n", graphName, graph_is_scope);
fflush(stdout);
*/
   if ( strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      if ( graph->WCS.type[0] != '\0' ) {
         /* previous flip */
         x = xorig + x * graph->xmagstep;
      } else {
         x = xorig + (-1.0 * x) * graph->xmagstep;
      }
   } else {
      x = xorig + x * graph->xmagstep;
   }

   if ( strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      /* previous flip */
      if ( graph->WCS.type[0] != '\0' ) {
         y = yorig - y * graph->ymagstep;
      } else {
         y = yorig - (-1.0 * y) * graph->ymagstep;
      }
   } else {
      y = yorig - y * graph->ymagstep;
   }

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

/*
fprintf(stdout, "                  return x, y (%20.15f, %20.15f)\n", x, y);
fflush(stdout);
*/
   return TCL_OK;
}

int PowCanvasToGraph( ClientData clientData, Tcl_Interp *interp, 
		      int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   Tcl_Obj *list, *elem[4];
   double x, y, xorig, yorig;
   double xorig_curr, yorig_curr;
   char *canvas=".pow.pow";
   char *graphName;
   int len;
   char *idxStr;
   const char *graphType;
   int zoomed;
   int xCount, yCount;
   int graph_is_scope;

   if( argc < 4 || argc > 5 ) {
      Tcl_SetResult(interp,"usage: powCanvasToGraph graph x y {canvas}",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   graphName = Tcl_GetStringFromObj( argv[1], NULL );
   graph = PowFindGraph( graphName );
   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Graph ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }
   Tcl_GetDoubleFromObj(interp, argv[2], &x);
   Tcl_GetDoubleFromObj(interp, argv[3], &y);
   if( argc==5 )
      canvas = Tcl_GetStringFromObj( argv[4], NULL );

   Tcl_VarEval(interp, canvas, " coords ",graphName, "box", NULL);

   list = Tcl_GetObjResult(interp);
   Tcl_ListObjIndex(interp, list, 0, &elem[0]);
   Tcl_ListObjIndex(interp, list, 1, &elem[1]);
   Tcl_ListObjIndex(interp, list, 2, &elem[2]);
   Tcl_ListObjIndex(interp, list, 3, &elem[3]);

   Tcl_GetDoubleFromObj(interp, elem[0], &xorig);
   Tcl_GetDoubleFromObj(interp, elem[3], &yorig);

   len    = strlen(graphName)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "graphType", graphName);
   graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
   ckfree(idxStr);

   len    = strlen(graphName)+15;
   idxStr = (char *) ckalloc( len*sizeof(char) );
   sprintf(idxStr, "%s,%s", "zoomed", graphName);
   zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
   ckfree(idxStr);

   xCount = atoi(Tcl_GetVar2(interp,"xCount",graphName,TCL_GLOBAL_ONLY));
   yCount = atoi(Tcl_GetVar2(interp,"yCount",graphName,TCL_GLOBAL_ONLY));

   if ( strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      Tcl_GetDoubleFromObj(interp, elem[2], &xorig_curr);
   }

   if ( strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      Tcl_GetDoubleFromObj(interp, elem[1], &yorig_curr);
   }

   idxStr = strstr(graphName, "scope");
   graph_is_scope = 0;
   if (idxStr != (char *)NULL) {
      graph_is_scope = 1;
   }

   /* Chai 06/29/2007:
      We are not actually fliping the coordinates on the canvas. If tk allows this, then there is
      no need to do the following. What the logic below is to trick pow to think that the point on
      the canvas has been flipped. The xCount and yCount indicate if the graph has been flipped
      before. So if X has been previously flipped, the next flipping occurs on Y, the logic inside
      ..Count % 2 will make sure the information on previous flip still exists. */

   if ( strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
      /* previous flip */
      if ( graph->WCS.type[0] != '\0' ) {
         x = (x - xorig) / graph->xmagstep;
      } else {
         x = (xorig - x) / graph->xmagstep;
      }
   } else {
      x = (x - xorig) / graph->xmagstep;
   }

   if ( strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
      /* previous flip */
      if ( graph->WCS.type[0] != '\0' ) {
         y = (yorig - y) / graph->ymagstep;
      } else {
         y = (y - yorig) / graph->ymagstep;
      }
   } else {
      y = (yorig - y) / graph->ymagstep;
   }

   if( PowPixToPos( x, y, &graph->WCS, &x, &y ) != TCL_OK )
      return TCL_ERROR;

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

   return TCL_OK;
}

int PowResetWcsStructure ( ClientData clientData, Tcl_Interp *interp, 
		           int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   PowGraph *scopegraph;
   PowImage *image;
   PowCurve *curve;
   double refpix_1, refpix_2, xoff, yoff;
   char *graphName, *operation, *direction;
   int wcsStatus;
   const char *WCSstring;
   char powWCS[7]="powWCS";
   char curveName[512];
   char scopeName[1024];
   int coordSel;

   if( argc != 5 && argc != 6 ) {
      Tcl_SetResult(interp,"usage: powResetWcsStructure <-g/-r graph refpix1/xoff refpix2/yoff> or <-d direction refpix1 refpix2>", TCL_VOLATILE);
      return TCL_ERROR;
   }

   operation = Tcl_GetStringFromObj( argv[1], NULL );
   graphName = Tcl_GetStringFromObj( argv[2], NULL );
   sprintf(scopeName, "%sscope", graphName);

   graph = PowFindGraph( graphName );
   scopegraph = PowFindGraph( scopeName );

   if( scopegraph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "graph Object ",scopeName," does not exist", NULL);
      return TCL_ERROR;
   }

   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "graph Object ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }
   Tcl_GetDoubleFromObj(interp, argv[3], &refpix_1);
   Tcl_GetDoubleFromObj(interp, argv[4], &refpix_2);

   if (!strcmp(operation, "-g")) {
      graph->WCS.haveWCSinfo = 0;

      PowPosToPix( graph->xleft, graph->ybot, &graph->WCS, &xoff, &yoff );
   
      graph->WCS.refPix[0] = refpix_1 + xoff;
      graph->WCS.refPix[1] = refpix_2 + yoff;
      graph->xoff = xoff;
      graph->yoff = yoff;
 
   } else if (!strcmp(operation, "-r")) {
      /* user refpix_1 and 2 for storage only, purpose is to reset xoff and yoff back to 0.0 */
      graph->xoff = 0.0;
      graph->yoff = 0.0;
   } else if (!strcmp(operation, "-d")) {
      if (graph->WCS.haveWCSinfo == 0) return TCL_OK;
      image = PowFindImage ( graphName );
      if( image == NULL ) {
         curve = PowFindCurve( graphName );
         if ( curve == NULL ) {
            sprintf(curveName, "c1_%s", graphName);
            curve = PowFindCurve( curveName );
            if ( curve == NULL ) {
               Tcl_ResetResult(interp);
               Tcl_AppendResult(interp, "Curve ", curveName," does not exist", NULL);
               return TCL_ERROR;
            }
         }
      }
      direction = Tcl_GetStringFromObj( argv[3], NULL );
      Tcl_GetDoubleFromObj(interp, argv[4], &refpix_1);
      Tcl_GetDoubleFromObj(interp, argv[5], &refpix_2);

      coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",graphName,TCL_GLOBAL_ONLY));

      if (!strcmp(direction, "X")) {
         graph->WCS.wcs[coordSel].cdelt[0] *= -1.0f;
         scopegraph->WCS.wcs[coordSel].cdelt[0] *= -1.0f;
      } else if (!strcmp(direction, "Y")) {
         graph->WCS.wcs[coordSel].cdelt[1] *= -1.0f;
         scopegraph->WCS.wcs[coordSel].cdelt[1] *= -1.0f;
      } else if (!strcmp(direction, "B") || !strcmp(direction, "U")) {
         graph->WCS.wcs[coordSel].cdelt[0] *= -1.0f;
         graph->WCS.wcs[coordSel].cdelt[1] *= -1.0f;
         scopegraph->WCS.wcs[coordSel].cdelt[0] *= -1.0f;
         scopegraph->WCS.wcs[coordSel].cdelt[1] *= -1.0f;
      }
      graph->WCS.haveWCSinfo = 1;

      graph->WCS.refPix[0] = refpix_1 + graph->xoff;
      graph->WCS.refPix[1] = refpix_2 + graph->yoff;

/*
fprintf(stdout, "X graph->xleft (%20.15f), graph->xright (%20.15f)\n", graph->xleft, graph->xright);
fprintf(stdout, "X graph->ybot  (%20.15f), graph->ytop   (%20.15f)\n", graph->ybot, graph->ytop);
fflush(stdout);
*/

      wcsStatus = TCL_ERROR;

      if ( image != NULL ) {
         image->WCS.refPix[0] = graph->WCS.refPix[0];
         image->WCS.refPix[1] = graph->WCS.refPix[1];
         image->WCS.wcs[coordSel].cdelt[0] = graph->WCS.wcs[coordSel].cdelt[0];
         image->WCS.wcs[coordSel].cdelt[1] = graph->WCS.wcs[coordSel].cdelt[1];
         WCSstring = Tcl_GetVar2(interp,powWCS,image->WCS.graphName,TCL_GLOBAL_ONLY);
         if( (WCSstring != NULL) && strcmp(WCSstring,"") ) {
            wcsStatus = Tcl_VarEval(interp, "powWCSInitImage ", image->WCS.graphName, " ",
                                    WCSstring, (char *) NULL);
         }
         FillinWCSStructure (&image->WCS);
         refpix_1 = image->WCS.refPix[0];
         refpix_2 = image->WCS.refPix[1];
      } else {
         curve->WCS.refPix[0] = graph->WCS.refPix[0];
         curve->WCS.refPix[1] = graph->WCS.refPix[1];
         curve->WCS.wcs[coordSel].cdelt[0] = graph->WCS.wcs[coordSel].cdelt[0];
         curve->WCS.wcs[coordSel].cdelt[1] = graph->WCS.wcs[coordSel].cdelt[1];
         WCSstring = Tcl_GetVar2(interp,powWCS,curve->WCS.curveName,TCL_GLOBAL_ONLY);
         if( (WCSstring != NULL) && strcmp(WCSstring,"") != 0 ) {
            wcsStatus = Tcl_VarEval(interp, "powWCSInitCurve ", curve->WCS.curveName, " ",
                                    WCSstring, (char *) NULL);
         }
         FillinWCSStructure (&curve->WCS);
         refpix_1 = curve->WCS.refPix[0];
         refpix_2 = curve->WCS.refPix[1];
      }

   }

   graph->WCS.refPix[0] = refpix_1;
   graph->WCS.refPix[1] = refpix_2;

   strcpy(graph->WCS.graphName, graphName);
   strcpy(graph->WCS.curveName, "\0");
   FillinWCSStructure (&graph->WCS);

   strcpy(scopegraph->WCS.graphName, graphName);
   FillinWCSStructure (&scopegraph->WCS);
   return TCL_OK;
}

int PowPixelToGraph( ClientData clientData, Tcl_Interp *interp, 
                     int argc, Tcl_Obj *const argv[])
{
   PowImage *image;
   PowCurve *curve;
   Tcl_Obj *list, *elem[2];
   double x, y;
   char *objName;

   if( argc != 4 ) {
      Tcl_SetResult(interp,"usage: powPixelToGraph image|curve x y}",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }
   objName = Tcl_GetStringFromObj( argv[1], NULL );
   Tcl_GetDoubleFromObj(interp,    argv[2], &x);
   Tcl_GetDoubleFromObj(interp,    argv[3], &y);

   image = PowFindImage( objName );
   if( image ) {
      if( PowPixToPos( x, y, &image->WCS, &x, &y ) != TCL_OK )
         return TCL_ERROR;
   } else {
      curve = PowFindCurve( objName );
      if( curve ) {
         if( PowPixToPos( x, y, &curve->WCS, &x, &y ) != TCL_OK )
            return TCL_ERROR;
      } else {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "Object ",objName," does not exist", NULL);
        return TCL_ERROR;
      }
   }

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

   return TCL_OK;
}

int PowGraphToPixel( ClientData clientData, Tcl_Interp *interp, 
                     int argc, Tcl_Obj *const argv[])
{
   PowImage *image;
   PowCurve *curve;
   Tcl_Obj *list, *elem[2];
   double x, y;
   char *objName;

   if( argc != 4 ) {
      Tcl_SetResult(interp,"usage: powGraphToPixel image|curve x y",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }
   objName = Tcl_GetStringFromObj( argv[1], NULL );
   Tcl_GetDoubleFromObj(interp,    argv[2], &x);
   Tcl_GetDoubleFromObj(interp,    argv[3], &y);

   image = PowFindImage( objName );
   if( image ) {
      if( PowPosToPix( x, y, &image->WCS, &x, &y ) != TCL_OK )
        return TCL_ERROR;
   } else {
      curve = PowFindCurve( objName );
      if( curve ) {
         if( PowPosToPix( x, y, &curve->WCS, &x, &y ) != TCL_OK )
            return TCL_ERROR;
      } else {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "Object ",objName," does not exist", NULL);
        return TCL_ERROR;
      }
   }

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

   return TCL_OK;
}


int PowPixelVToGraphV( ClientData clientData, Tcl_Interp *interp, 
                       int argc, Tcl_Obj *const argv[])
{
   PowImage *image;
   PowCurve *curve;
   WCSdata *WCS;
   Tcl_Obj *list, *elem[2];
   double x, y, xorig, yorig;
   char *objName;

   if( argc != 4 ) {
      Tcl_SetResult(interp,"usage: powPixelVToGraphV image|curve dx dy}",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }
   objName = Tcl_GetStringFromObj( argv[1], NULL );
   Tcl_GetDoubleFromObj(interp,    argv[2], &xorig);
   Tcl_GetDoubleFromObj(interp,    argv[3], &yorig);

   image = PowFindImage( objName );
   if( image ) {
     WCS = &image->WCS;
   } else {
      curve = PowFindCurve( objName );
      if( curve ) {
        WCS = &curve->WCS;
      } else {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "Object ",objName," does not exist", NULL);
        return TCL_ERROR;
      }
   }
   x = WCS->cdFrwd[0][0] * xorig + WCS->cdFrwd[0][1] * yorig;
   y = WCS->cdFrwd[1][0] * xorig + WCS->cdFrwd[1][1] * yorig;

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

   return TCL_OK;
}

int PowGraphVToPixelV( ClientData clientData, Tcl_Interp *interp, 
                       int argc, Tcl_Obj *const argv[])
{
   PowImage *image;
   PowCurve *curve;
   WCSdata  *WCS;
   Tcl_Obj *list, *elem[2];
   double x, y, xorig, yorig;
   char *objName;

   if( argc != 4 ) {
      Tcl_SetResult(interp,"usage: powGraphVToPixelV image|curve dx dy",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }
   objName = Tcl_GetStringFromObj( argv[1], NULL );
   Tcl_GetDoubleFromObj(interp,    argv[2], &xorig);
   Tcl_GetDoubleFromObj(interp,    argv[3], &yorig);

   image = PowFindImage( objName );
   if( image ) {
     WCS = &image->WCS;
   } else {
      curve = PowFindCurve( objName );
      if( curve ) {
        WCS = &curve->WCS;
      } else {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "Object ",objName," does not exist", NULL);
        return TCL_ERROR;
      }
   }
   x = WCS->cdRvrs[0][0] * xorig + WCS->cdRvrs[0][1] * yorig;
   y = WCS->cdRvrs[1][0] * xorig + WCS->cdRvrs[1][1] * yorig;

   elem[0] = Tcl_NewDoubleObj(x);
   elem[1] = Tcl_NewDoubleObj(y);
   list = Tcl_NewListObj(2,elem);
   Tcl_SetObjResult(interp, list);

   return TCL_OK;
}


int PowGetImageClipbox( ClientData clientData, Tcl_Interp *interp, 
			int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   PowImage *image;
   Tcl_Obj *list, *clip[6];
   char *graphName, *imageName;
   double xorigin, yorigin, xother, yother, gWidth, gHeight;
   double xscale, yscale, xleft, ybot;
   /* FILE *fp; */

   if( argc < 3 || argc > 4  ) {
      Tcl_SetResult(interp,"usage: powGetImageClipbox graph image ?canvas?",
		    TCL_VOLATILE);
      return TCL_ERROR;
   }

   graphName = Tcl_GetStringFromObj( argv[1], NULL );
   graph = PowFindGraph( graphName );
   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Graph ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }
    
   imageName = Tcl_GetStringFromObj( argv[2], NULL );
   image = PowFindImage( imageName );
   if( image==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Image ",imageName," does not exist", NULL);
      return TCL_ERROR;
   }
    
/* fprintf(fp, "PowCommands: image->xorigin:  %f\n", image->xorigin); */
/* fprintf(fp, "PowCommands: image->yorigin:  %f\n", image->yorigin); */
/* fprintf(fp, "PowCommands: image->xotherend: %f\n", image->xotherend); */
/* fprintf(fp, "PowCommands: image->yotherend: %f\n", image->yotherend); */

   PowPosToPix( image->xorigin, image->yorigin, &graph->WCS, 
                                                          &xorigin, &yorigin );
   PowPosToPix( image->xotherend, image->yotherend, &graph->WCS,
		&xother, &yother  );
   PowPosToPix( graph->xright, graph->ytop,   &graph->WCS, &gWidth, &gHeight );

   if( xother<=0.0 || yother<=0.0 || xorigin>=gWidth || yorigin>=gHeight ) {
      Tcl_SetResult(interp,"clipped", TCL_VOLATILE);
      return TCL_OK;
   }

/* fprintf(fp, "PowCommands: xother: %f\n", xother); */
/* fprintf(fp, "PowCommands: yother: %f\n", yother); */
/* fprintf(fp, "PowCommands: xorigin: %f\n", xorigin); */
/* fprintf(fp, "PowCommands: yorigin: %f\n", yorigin); */
/* fprintf(fp, "PowCommands: image->width:  %f\n", image->width); */
/* fprintf(fp, "PowCommands: image->height: %f\n", image->height); */

   xscale = image->width  / (xother - xorigin);
   yscale = image->height / (yother - yorigin);

/* fprintf(fp, "PowCommands: xscale: %f\n", xscale); */
/* fprintf(fp, "PowCommands: yscale: %f\n", yscale); */

   if( xorigin < 0.0 ) {
      xleft   = - xorigin * xscale;
      xorigin = 0.0;
   } else
      xleft   = 0.0;

   if( yorigin < 0.0 ) {
      ybot    = - yorigin * yscale;
      yorigin = 0.0;
   } else
      ybot    = 0.0;

/* fprintf(fp, "PowCommands: xleft: %f\n", xleft); */
/* fprintf(fp, "PowCommands: ybot: %f\n", ybot); */

   if( xother > gWidth  ) xother = image->width - (xother-gWidth) * xscale;
   else                   xother = image->width;

   if( yother > gHeight ) yother = image->height - (yother-gHeight) * yscale;
   else                   yother = image->height;

/* fprintf(fp, "PowCommands: xother: %f\n", xother); */
/* fprintf(fp, "PowCommands: yother: %f\n", yother); */
/* fprintf(fp, "***************************\n"); */

   PowPixToPos( xorigin, yorigin, &graph->WCS, &xorigin, &yorigin );

   clip[0] = Tcl_NewDoubleObj(xorigin    );
   clip[1] = Tcl_NewDoubleObj(yorigin    );
   clip[2] = Tcl_NewDoubleObj(xleft  -0.5);
   clip[3] = Tcl_NewDoubleObj(ybot   -0.5);
   clip[4] = Tcl_NewDoubleObj(xother -0.5);
   clip[5] = Tcl_NewDoubleObj(yother -0.5);
   list = Tcl_NewListObj(6,clip);
   Tcl_SetObjResult(interp, list);

/* fclose(fp); */

   return TCL_OK;
}


int PowWCSexists( ClientData clientData, Tcl_Interp *interp, 
                     int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   Tcl_Obj *result;
   char *graphName;

   if( argc != 2 ) {
      Tcl_SetResult(interp,"usage: powWCSexists graph", TCL_VOLATILE);
      return TCL_ERROR;
   }
   graphName = Tcl_GetStringFromObj( argv[1], NULL );
   graph = PowFindGraph( graphName );
   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Graph ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }

   result = Tcl_NewBooleanObj( graph->WCS.type[0]!='\0' );
   Tcl_SetObjResult(interp, result);

   return TCL_OK;
}

int PowWCSisSwapped( ClientData clientData, Tcl_Interp *interp, 
                     int argc, Tcl_Obj *const argv[])
{
   PowGraph *graph;
   Tcl_Obj *result;
   char *graphName;

   if( argc != 2 ) {
      Tcl_SetResult(interp,"usage: powWCSisSwapped graph", TCL_VOLATILE);
      return TCL_ERROR;
   }
   graphName = Tcl_GetStringFromObj( argv[1], NULL );
   graph = PowFindGraph( graphName );
   if( graph==NULL ) {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "Graph ",graphName," does not exist", NULL);
      return TCL_ERROR;
   }
   
   /* for the unknown WCS type, always set the swap to zero */
   if (graph->WCS.type[0]=='\0') graph->WCS.RaDecSwap = 0; 
   result = Tcl_NewLongObj( graph->WCS.RaDecSwap );
   Tcl_SetObjResult(interp, result);

   return TCL_OK;
}

/* Get the centroid and flux */
int PowGetRegionStatistics( ClientData clientData, Tcl_Interp *interp, 
                  int argc, char **argv )
{
  char *imgName;
  PowImage *image_ptr;

  char *descr;
  char *cPar;
  char *shape;
  char *sign;
  char *regionFile;
  double *dataPar;
  const char **argvPtr;

  int i, numPars;
  int plen;

  double cent[2], cstd[2],flux, npix;
  double mean,dmean;
  double a,b,c,d;
  char results[512];
  int ix,iy;

  char *ptr;
  int rect[4];
  int status;
  int good;

  if(argc != 6) {
    Tcl_SetResult(
    interp, "usage: powGetGetRegionStatistics image regionFile/NONE descr shape sign",
        TCL_VOLATILE );
    good = 0;
    return TCL_ERROR;
  }

  imgName   = argv[1];
  image_ptr = PowFindImage( imgName );
  if (image_ptr == (PowImage *) NULL) {
    Tcl_AppendResult(interp, "Couldn't find image: ",imgName, NULL);
    good = 0;
    return TCL_ERROR;
  }

  regionFile = argv[2];
  descr      = argv[3];
  shape      = argv[4];
  sign       = argv[5];

  plen = strlen(descr);

  /* parse the parameters */
  cPar = (char *)malloc ((plen+1)*sizeof(char));
  strcpy(cPar,descr);
  ptr = cPar;
  while ( *ptr!='\0') {
     if( *ptr == ','|| *ptr == '(' || *ptr == ')' ) *ptr = ' ';
     ptr++;
  }
  if(Tcl_SplitList(interp, cPar, &numPars, &argvPtr) != TCL_OK ) {
    good = 0;
    return TCL_ERROR;
  }
  dataPar = (double*) malloc(numPars*sizeof(double));
  for (i=0; i<numPars; i++) {
      Tcl_GetDouble(interp, argvPtr[i],&dataPar[i]);
  }
  cent[0] = 0;
  cent[1] = 0;
  flux = 0;
  if (!strcmp(shape,"Point")) {
      ix = (int)(dataPar[0]+0.5 - 1);
      iy = (int)(dataPar[1]+0.5 - 1);
      flux = PowExtractDatum(image_ptr->dataptr,
         iy * image_ptr->width + ix);
      cent[0] = dataPar[0];
      cent[1] = dataPar[1];
      good = 1;
  }
  if (!strcmp(shape,"Line")) {
    good = 0;
  }

  /* Find the binding box for circle, box, ellipse, and polygon */
  if (!strcmp(shape,"Circle")) {
      rect[0] = (int)(dataPar[0] - dataPar[2] + 0.5 -1) ;
      rect[1] = (int)(dataPar[1] - dataPar[2] + 0.5 -1) ;
      rect[2] = (int)(dataPar[0] + dataPar[2] + 0.5 +1) ;
      rect[3] = (int)(dataPar[1] + dataPar[2] + 0.5 +1) ;
  }

  if (!strcmp(shape,"Box")) {
      a = sqrt(dataPar[2]*dataPar[2]+dataPar[3]*dataPar[3])/2.0;
      rect[0] = (int)(dataPar[0] - a + 0.5 -1) ;
      rect[1] = (int)(dataPar[1] - a + 0.5 -1) ;
      rect[2] = (int)(dataPar[0] + a + 0.5 +1) ;
      rect[3] = (int)(dataPar[1] + a + 0.5 +1) ;
  }
  if (!strcmp(shape,"Ellipse")) {
      a = sqrt(dataPar[2]*dataPar[2]+dataPar[3]*dataPar[3]);
      rect[0] = (int)(dataPar[0] - a + 0.5 -1) ;
      rect[1] = (int)(dataPar[1] - a + 0.5 -1) ;
      rect[2] = (int)(dataPar[0] + a + 0.5 +1) ;
      rect[3] = (int)(dataPar[1] + a + 0.5 +1) ;
  }

  if (!strcmp(shape,"Polygon")) {
      a = dataPar[0];
      b = dataPar[1];
      c = dataPar[0];
      d = dataPar[1];
      for (i = 0; i < numPars/2; i++) {
        a = a > dataPar[2*i] ? dataPar[2*i] : a;
        b = b > dataPar[2*i+1] ? dataPar[2*i+1] : b;
        c = c < dataPar[2*i] ? dataPar[2*i] : c;
        d = d < dataPar[2*i+1] ? dataPar[2*i+1] : d;
      }
      rect[0] = (int)(a + 0.5 -1) ;
      rect[1] = (int)(b + 0.5 -1) ;
      rect[2] = (int)(c + 0.5 +1) ;
      rect[3] = (int)(d + 0.5 +1) ;
  }
  status = 0;
  PowCalRegion(image_ptr, regionFile, rect,dataPar, numPars, shape,sign,
               cent, cstd, &flux, &npix, &mean, &dmean, &status);
  if(status == 0)
    good = 1;
  else
    good = 0;

  sprintf(results,"%d %g %g %g %g %g %g %g %g",good,cent[0],cent[1],cstd[0],
        cstd[1], flux, npix, mean, dmean);
  Tcl_SetResult( interp, results,TCL_VOLATILE );

  free(dataPar);
  free(cPar);

  return TCL_OK;
}

/*----------------------------------------------------------------------
*
* Pow_PhotoPutScaledBlock (formerly Tk_PhotoPutScaledBlock) --
*
*	This procedure is called to put image data into a photo image,
*	with possible zooming of the pixels.
*
* Results:
*	None.
*
* Side effects:
*	The image data is stored.  The image may be expanded.
*	The Tk image code is informed that the image has changed.
*
*----------------------------------------------------------------------*/

void
Pow_PhotoPutScaledBlock(handle, blockPtr, x, y, width, height, zoomX, 
			zoomY, Xoff, Yoff)
Tk_PhotoHandle handle;		/* Opaque handle for the photo image
				 * to be updated. */
register Tk_PhotoImageBlock *blockPtr;
				/* Pointer to a structure describing the
				 * pixel data to be copied into the image. */
int x, y;			/* Coordinates of the top-left pixel to
				 * be updated in the image. */
int width, height;		/* Dimensions of the area of the image
				 * to be updated. */
double zoomX, zoomY;      	/* Zoom factors for the X and Y axes. */
double Xoff, Yoff;              /* Offset into initial pixel data */
{
     int greenOffset, blueOffset, alphaOffset;
     int wCopy, hCopy;
     unsigned char *srcPtr, *srcLinePtr;
     unsigned char *destPtr, *destLinePtr;
     int pitch;
     double xRepeat, yRepeat;
     int blockXSkip, blockYSkip;
     Tk_PhotoImageBlock destBlockPtr;

     if( (zoomX <= 0.0) || (zoomY <= 0.0) || (width <= 0) || (height <= 0)
             || (x < 0) || (y < 0) )
         return;
     if( (zoomX == 1.0) && (zoomY == 1.0) ) {
         Tk_PhotoPutBlock(interp, handle, blockPtr, x, y, width, height, TK_PHOTO_COMPOSITE_SET);
         return;
     }

     Tk_PhotoExpand(interp, handle, x+width, y+height);
     Tk_PhotoGetImage(handle, &destBlockPtr);

     /*
      * If this image block could have different red, green and blue
      * components, mark it as a color image.
      */

     greenOffset = blockPtr->offset[1] - blockPtr->offset[0];
     blueOffset = blockPtr->offset[2] - blockPtr->offset[0];
     alphaOffset = blockPtr->offset[3];
     if ((alphaOffset >= blockPtr->pixelSize) || (alphaOffset < 0)) {
         alphaOffset = 0;
     } else {
         alphaOffset -= blockPtr->offset[0];
     }

     /*
      * Copy the data into the destination's 32-bit/pixel array.
      */

     destLinePtr = destBlockPtr.pixelPtr + (y * destBlockPtr.width + x) * 4;
     pitch       = destBlockPtr.width * 4;

     srcLinePtr = blockPtr->pixelPtr + blockPtr->offset[0];
     blockXSkip = blockPtr->pixelSize;
     blockYSkip = blockPtr->pitch;

     yRepeat = Yoff;
     for (hCopy=height; hCopy > 0; hCopy--) {
         destPtr = destLinePtr;
         srcPtr  = srcLinePtr;
         xRepeat = Xoff;
         for (wCopy=width; wCopy > 0; wCopy--) {
             if (!destPtr[3]) {
                 destPtr[0] = destPtr[1] = destPtr[2] = 0xd9;
             }
             if (!alphaOffset || (srcPtr[alphaOffset] == 255)) {
                 *destPtr++ = srcPtr[0];
                 *destPtr++ = srcPtr[greenOffset];
                 *destPtr++ = srcPtr[blueOffset];
                 *destPtr++ = 255;
             } else {
                 if (srcPtr[alphaOffset]) {
                     destPtr[0] += (srcPtr[0] - destPtr[0])
                     * srcPtr[alphaOffset] / 255;
                     destPtr[1] += (srcPtr[greenOffset] - destPtr[1])
                         * srcPtr[alphaOffset] / 255;
                     destPtr[2] += (srcPtr[blueOffset] - destPtr[2])
                         * srcPtr[alphaOffset] / 255;
                     destPtr[3] += (255 - destPtr[3])
                         * srcPtr[alphaOffset] / 255;
                 }
                 destPtr+=4;
             }
             xRepeat--;
             while( xRepeat <= 0.0 ) {
                 srcPtr  += blockXSkip;
                 xRepeat += zoomX;
             }
         }
         destLinePtr += pitch;
         yRepeat--;
         while( yRepeat <= 0.0 ) {
             srcLinePtr += blockYSkip;
             yRepeat    += zoomY;
         }
     }

     Tk_PhotoPutBlock(interp, handle, &destBlockPtr, x, y, width, height, TK_PHOTO_COMPOSITE_SET);
}
