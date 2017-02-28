/* 
 * powCanvCurve.c -- derived from 
 * tkCanvLine.c --
 *
 *	This file implements powCurve items for canvas widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tkInt.h"
#include "tkPort.h"
#include "tkCanvas.h"

#include "pow.h"

void outDebugStr(char *title, char *str);

/*
 * Information used for parsing configuration specs.  If you change any
 * of the default strings, be sure to change the corresponding default
 * values in CreatePowCurve.
 */

static Tk_CustomOption stateOption = {
    (Tk_OptionParseProc *) TkStateParseProc,
    TkStatePrintProc, (ClientData) 2
};
static Tk_CustomOption tagsOption = {
    (Tk_OptionParseProc *) Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc, (ClientData) NULL
};
static Tk_CustomOption dashOption = {
    (Tk_OptionParseProc *) TkCanvasDashParseProc,
    TkCanvasDashPrintProc, (ClientData) NULL
};
static Tk_CustomOption offsetOption = {
    (Tk_OptionParseProc *) TkOffsetParseProc,
    TkOffsetPrintProc,
    (ClientData) (TK_OFFSET_RELATIVE|TK_OFFSET_INDEX)
};
static Tk_CustomOption pixelOption = {
    (Tk_OptionParseProc *) TkPixelParseProc,
    TkPixelPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {
  {TK_CONFIG_CUSTOM, "-activedash", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.activeDash),
   TK_CONFIG_NULL_OK, &dashOption},
  {TK_CONFIG_COLOR, "-activefill", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.activeColor),
   TK_CONFIG_NULL_OK},
  {TK_CONFIG_BITMAP, "-activestipple", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.activeStipple),
   TK_CONFIG_NULL_OK},
  {TK_CONFIG_CUSTOM, "-activewidth", (char *) NULL, (char *) NULL,
   "0.0", Tk_Offset(PowCurveItem, lOutline.activeWidth),
   TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
  {TK_CONFIG_CAP_STYLE, "-capstyle", (char *) NULL, (char *) NULL,
   "butt", Tk_Offset(PowCurveItem, capStyle), TK_CONFIG_DONT_SET_DEFAULT},
  {TK_CONFIG_STRING, "-pointtype", (char *) NULL, (char *) NULL,
   "Cross", Tk_Offset(PowCurveItem, pointType), 0},
  {TK_CONFIG_PIXELS, "-pointsize", (char *) NULL, (char *) NULL,
   "3", Tk_Offset(PowCurveItem, pointSize), 0},
  {TK_CONFIG_BOOLEAN, "-pointdisplay", (char *) NULL, (char *) NULL,
   "1", Tk_Offset(PowCurveItem, pointDisp), 0},
  {TK_CONFIG_BOOLEAN, "-pointfill", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, pointFill), 0},
  {TK_CONFIG_BOOLEAN, "-pointerror", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, pointError), 0},
  {TK_CONFIG_BOOLEAN, "-linedisplay", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, lineDisp), 0},
  {TK_CONFIG_BOOLEAN, "-stairstep", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, stairStep), 0},
  {TK_CONFIG_BOOLEAN, "-boxfill", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, boxFill), 0},
  {TK_CONFIG_BOOLEAN, "-curvetopoint", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, curveToPoint), 0},
  {TK_CONFIG_BOOLEAN, "-hidden", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, hidden), 0},
  {TK_CONFIG_COLOR, "-lfill", (char *) NULL, (char *) NULL,
   "Black", Tk_Offset(PowCurveItem, lOutline.color), TK_CONFIG_NULL_OK},
  {TK_CONFIG_COLOR, "-pfill", (char *) NULL, (char *) NULL,
   "Black", Tk_Offset(PowCurveItem, pOutline.color), TK_CONFIG_NULL_OK},
  {TK_CONFIG_BOOLEAN, "-logx", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, logX), 0},
  {TK_CONFIG_BOOLEAN, "-logy", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, logY), 0},
  {TK_CONFIG_INT, "-LOD", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, LOD), 0},
  {TK_CONFIG_CUSTOM, "-dash", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.dash),
   TK_CONFIG_NULL_OK, &dashOption},
  {TK_CONFIG_PIXELS, "-dashoffset", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(PowCurveItem, lOutline.offset),
   TK_CONFIG_DONT_SET_DEFAULT},
  {TK_CONFIG_CUSTOM, "-disableddash", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.disabledDash),
   TK_CONFIG_NULL_OK, &dashOption},
  {TK_CONFIG_COLOR, "-disabledfill", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.disabledColor),
   TK_CONFIG_NULL_OK},
  {TK_CONFIG_BITMAP, "-disabledstipple", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.disabledStipple),
   TK_CONFIG_NULL_OK},
  {TK_CONFIG_CUSTOM, "-disabledwidth", (char *) NULL, (char *) NULL,
   "0.0", Tk_Offset(PowCurveItem, lOutline.disabledWidth),
   TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
  {TK_CONFIG_JOIN_STYLE, "-joinstyle", (char *) NULL, (char *) NULL,    
   "round", Tk_Offset(PowCurveItem, joinStyle), TK_CONFIG_DONT_SET_DEFAULT},
  {TK_CONFIG_CUSTOM, "-offset", (char *) NULL, (char *) NULL,
   "0,0", Tk_Offset(PowCurveItem, lOutline.tsoffset),
   TK_CONFIG_DONT_SET_DEFAULT, &offsetOption},
  {TK_CONFIG_CUSTOM, "-state", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK,
   &stateOption},
  {TK_CONFIG_BITMAP, "-stipple", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(PowCurveItem, lOutline.stipple),
   TK_CONFIG_NULL_OK},
  {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
   (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
  {TK_CONFIG_CUSTOM, "-width", (char *) NULL, (char *) NULL,
   "1.0", Tk_Offset(PowCurveItem, lOutline.width),
   TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
  {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
   (char *) NULL, 0, 0}
};

/*
 * The structures below defines the powCurve item type by means
 * of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkPowCurveType = {
    "powCurve",				/* name */
    sizeof(PowCurveItem),		/* itemSize */
    CreatePowCurve,			/* createProc */
    configSpecs,			/* configSpecs */
    ConfigurePowCurve,			/* configureProc */
    PowCurveCoords,			/* coordProc */
    DeletePowCurve,			/* deleteProc */
    DisplayPowCurve,			/* displayProc */
    TK_CONFIG_OBJS,			/* flags */
    PowCurveToPoint,			/* pointProc */
    PowCurveToArea,			/* areaProc */
    PowCurveToPostscript,		/* postscriptProc */
    ScalePowCurve,			/* scaleProc */
    TranslatePowCurve,			/* translateProc */
    GetPowCurveIndex,			/* indexProc */
    (Tk_ItemCursorProc *) NULL,		/* icursorProc */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    PowCurveInsert,			/* insertProc */
    PowCurveDeleteCoords,		/* dTextProc */
    (Tk_ItemType *) NULL		/* nextPtr */
};


/*
 *--------------------------------------------------------------
 *
 * CreatePowCurve --
 *
 *	This procedure is invoked to create a new powCurve item in
 *	a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	the interp's result;  in this case itemPtr is left uninitialized,
 *	so it can be safely freed by the caller.
 *
 * Side effects:
 *	A new powCurve item is created.
 *
 *--------------------------------------------------------------
 */

 int
CreatePowCurve(interp, canvas, itemPtr, objc, objv)
    Tcl_Interp *interp;			/* Interpreter for error reporting. */
    Tk_Canvas canvas;			/* Canvas to hold new item. */
    Tk_Item *itemPtr;			/* Record to hold new item;  header
					 * has been initialized by caller. */
    int objc;				/* Number of arguments in objv. */
    Tcl_Obj *CONST objv[];		/* Arguments describing powCurve. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    char *crv, *grph;

    /*
     * Carry out initialization that is needed to set defaults and to
     * allow proper cleanup after errors during the the remainder of
     * this procedure.
     */

    Tk_CreateOutline(&(powCurvePtr->lOutline));
    Tk_CreateOutline(&(powCurvePtr->pOutline));
    powCurvePtr->canvas       = canvas;
    powCurvePtr->pointType    = NULL;
    powCurvePtr->curveToPoint = 0;


    powCurvePtr->capStyle = CapButt;
    powCurvePtr->joinStyle = JoinRound;

    if (objc < 2) {
       Tcl_SetResult(interp, "Usage: canvas create powCurve curve_name graph_name ?options?", TCL_STATIC);
      goto error;
    }

    crv  = Tcl_GetStringFromObj( objv[0], NULL );
    grph = Tcl_GetStringFromObj( objv[1], NULL );
    if( (powCurvePtr->curveObjectPtr = PowFindCurve(crv)) == NULL ) {
      Tcl_SetResult(interp,"Couldn't find curve: ", TCL_STATIC );
      Tcl_AppendResult(interp,crv,(char*)NULL);
      goto error;
    }

    if( (powCurvePtr->graphObjectPtr = PowFindGraph(grph)) == NULL ) {
      Tcl_SetResult(interp,"Couldn't find graph: ", TCL_STATIC );
      Tcl_AppendResult(interp,grph,(char*)NULL);
      goto error;
    }

    powCurvePtr->pCoordPtr = NULL;
    powCurvePtr->lCoordPtr = NULL;
    
    if (ConfigurePowCurve(interp, canvas, itemPtr, objc-2, objv+2, 0) != TCL_OK) {
	goto error;
    }

    if (PowCurveCoords(interp, canvas, itemPtr, objc, objv) == TCL_OK) {
	return TCL_OK;
    }


    error:
    DeletePowCurve(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * PowCurveCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on powCurves.  See the user documentation for details
 *	on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

 int
PowCurveCoords(interp, canvas, itemPtr, objc, objv)
   /* This routine calculates a list of canvas coordinates for
    * the points in the PowCurve object, these are then used to
    * display/scale/etc the item. */

    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item whose coordinates are to be
					 * read or modified. */
    int objc;				/* Number of coordinates supplied in
					 * argv. */
    Tcl_Obj *CONST objv[];		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    double *pCoordPtr, *lCoordPtr;
    /*From the old PowProcessCurve command */
    char **bboxptr;
    double x0 , x1, y0, y1;
    double rx0 , rx1, ry0, ry1, ry, rx;
    int n, xoff, yoff, xeoff, yeoff;
    PowCurve *curve_ptr;
    PowVector *x_vector, *y_vector, *x_error, *y_error, *z_vector;
    PowData *x_vect, *y_vect, *x_err, *y_err, *z_vect;
    PowGraph *graph;
    double xmagstep, ymagstep, oldx, oldy, modx, mody;
    double x,y,xe[5],ye[5],xp,yp,histX,histY,pX[12],pY[12];

    int len;
    char *idxStr;
    const char *graphType;
    int zoomed;
    int xCount, yCount;
    int coordSel;
    double WCScdeltX, WCScdeltY;
    double p1;
    double q1;
    int nPts;
    int dflag=0,pflag=0,lflag=0,eflag,pType=0,pSize=0,logX=0,logY=0;
    int lasti;
    char *graphName;
    char *tagstring;
    Tcl_FreeProc *freeProcPtr;
    int pts_per_pt, allocPts, usedPts;
    int i,seg;
    int LOD=0;
    double nsum, rx_LOD, rxsum, rysum, rx2sum, ry2sum, sigmax, sigmay;
    double LODthresh=0.0, rxm, rym;
    int LOD_summing, LOD_summed=0;
    char xstring[30]="";
    char ystring[30]="";
    static struct {
       char *Name;
       int nLines;
       struct {
	  double x, y;
       } lines[16];
    } pointShapes[] = {
       { "Cross",   5, {          /*  These 99s should be DBL_MAX, but  */
                                  /*  Borland C++ (Windoze) complains   */
                         { 1,  0}, {-1,  0}, {99.0, 99.0},
			 { 0,  1}, { 0, -1}
                       }
       },
       { "Diamond", 5, {
                         { 1,  0}, { 0,  1},
			 {-1,  0}, { 0, -1},
                         { 1,  0}
                       }
       },
       { "Box", 5, {
                         { 1,  1}, {-1,  1},
                         {-1, -1}, { 1, -1},
                         { 1,  1}
                       }
       },
       { "Octagon", 9, {
                         { 1.000,  0.000}, { 0.707,  0.707},
                         { 0.000,  1.000}, {-0.707,  0.707},
                         {-1.000,  0.000}, {-0.707, -0.707},
                         { 0.000, -1.000}, { 0.707, -0.707},
                         { 1.000,  0.000}
                       }
       },
       { "Triangle", 4, {
                         { 1,  1}, { 0, -1},
			 {-1,  1}, { 1,  1},
                       }
       },
       { "Inv. Triangle", 4, {
                         { 1, -1}, { 0,  1},
			 {-1, -1}, { 1, -1},
                       }
       },
       { "Dot", 1, {
                         { 0.0, 0.0}
                       }
       },
       { "", 0 }
    };

  /* objc should be 0*/

  lasti = 0;

  x_vect = NULL;
  y_vect = NULL;
  z_vect = NULL;
  x_err = NULL;
  y_err = NULL;
  x_vector = NULL;
  y_vector = NULL;
  x_error = NULL;
  y_error = NULL;
  z_vector = NULL;

  tagstring = Tk_CanvasTagsPrintProc((ClientData) NULL, 
				     Tk_CanvasTkwin(canvas),
				     (char *)itemPtr,
				     0,
				     &freeProcPtr);


  curve_ptr = powCurvePtr->curveObjectPtr;
  if ((curve_ptr->x_vector)!= NULL ) {
    x_vect = (curve_ptr->x_vector)->dataptr;
    x_vector = curve_ptr->x_vector;
  }
  if ((curve_ptr->x_error)!= NULL) {
    x_err = (curve_ptr->x_error)->dataptr;
    x_error = curve_ptr->x_error;
  }
  if ((curve_ptr->y_vector)!= NULL){ 
    y_vect = (curve_ptr->y_vector)->dataptr;
    y_vector = curve_ptr->y_vector;
  }
  if ((curve_ptr->y_error)!= NULL) {
    y_err = (curve_ptr->y_error)->dataptr;
    y_error = curve_ptr->y_error;
  }

  if ((curve_ptr->z_vector)!= NULL){ 
    z_vect = (curve_ptr->z_vector)->dataptr;
    z_vector = curve_ptr->z_vector;
  }

  
  xoff = 0;
  yoff = 0;
  xeoff = 0;
  yeoff = 0;
  if (x_vector != NULL) xoff = x_vector->offset;
  if (y_vector != NULL) yoff = y_vector->offset;
  if (x_error != NULL) xeoff = x_error->offset;
  if (y_error != NULL) yeoff = y_error->offset;


  /* get the canvas coordinates for the axes box */

  Tcl_VarEval( interp, Tk_PathName(Tk_CanvasTkwin(canvas)),
	      " coords ", powCurvePtr->graphObjectPtr->graph_name,"box",
	      (char *) NULL);
  Tcl_SplitList(interp,Tcl_GetStringResult(interp),&n,&bboxptr);

  /* (x0,y0) -- lower left */
  /* (x1,y1) -- upper right */
  
  Tcl_GetDouble(interp,bboxptr[0],&x0);
  Tcl_GetDouble(interp,bboxptr[3],&y0);
  Tcl_GetDouble(interp,bboxptr[2],&x1);
  Tcl_GetDouble(interp,bboxptr[1],&y1);

  len    = strlen(powCurvePtr->graphObjectPtr->graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "graphType", powCurvePtr->graphObjectPtr->graph_name);
  graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
  ckfree(idxStr);

  len    = strlen(powCurvePtr->graphObjectPtr->graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "zoomed", powCurvePtr->graphObjectPtr->graph_name);
  zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
  ckfree(idxStr);

  xCount = atoi(Tcl_GetVar2(interp,"xCount",powCurvePtr->graphObjectPtr->graph_name,TCL_GLOBAL_ONLY));
  yCount = atoi(Tcl_GetVar2(interp,"yCount",powCurvePtr->graphObjectPtr->graph_name,TCL_GLOBAL_ONLY));

  graph = powCurvePtr->graphObjectPtr;
  graphName = powCurvePtr->graphObjectPtr->graph_name;
  strcpy(curve_ptr->WCS.graphName, graphName);

  coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",graphName,TCL_GLOBAL_ONLY));

  /* Chai 06/29/2007:
     We are not actually fliping the coordinates on the canvas. If tk allows this, then there is
     no need to do the following. What the logic below is to trick pow to think that the point on
     the canvas has been flipped. The xCount and yCount indicate if the graph has been flipped
     before. So if X has been previously flipped, the next flipping occurs on Y, the logic inside
     ..Count % 2 will make sure the information on previous flip still exists. */
 
  /* Chai 06/29/2007:
     At this point, graph already know if it has been flipped. xleft/xright ybot/ytop value(s) has already
     been swapped and x0, y0, x1, y1 have all been taken care of. */
  rx0 = graph->xleft;
  ry0 = graph->ybot;
  rx1 = graph->xright;
  ry1 = graph->ytop;

  ckfree((void *)bboxptr);

  xmagstep = graph->xmagstep;
  ymagstep = graph->ymagstep;

  logX  = powCurvePtr->logX;
  logY  = powCurvePtr->logY;

  LOD   = powCurvePtr->LOD;

  lflag = powCurvePtr->lineDisp;
  pflag = powCurvePtr->pointDisp;
  eflag = (x_err || y_err);

  /*  Identify the point shape, if points plotted  */

  pType = 0;
  if( pflag ) {
     while( pointShapes[pType].Name[0] ) {
	if (!strcmp(powCurvePtr->pointType,pointShapes[pType].Name))
	   break;
	pType++;
     }
     if( !pointShapes[pType].Name[0] ) pType = 0;

     pSize = powCurvePtr->pointSize;
     if( powCurvePtr->pointError && eflag ) {
	eflag = pSize = 0;  /*  Don't draw errorbars... just the points  */
     }
  }

  /* Allocate enough space to hold every point although some may not
     be visible on the graph */

  if( pflag || eflag || LOD) {

     pts_per_pt = 0;
     if( pflag )
	pts_per_pt += pointShapes[pType].nLines + 1;

     if( eflag || LOD) {
	if( graph->WCS.type[0] )
	   pts_per_pt += 8;
	else
	   pts_per_pt += 6;
     }

     allocPts = curve_ptr->length * pts_per_pt * 2;
     pCoordPtr = (double *) ckalloc(sizeof(double) * allocPts);
     if( !pCoordPtr ) {
	Tcl_SetResult(interp, "Unable to allocate memory for curve",
		      TCL_VOLATILE);
	return TCL_ERROR;
     }
  } else {

     pCoordPtr = (double *) NULL;

  }     
  powCurvePtr->pCoordPtr = pCoordPtr;

  /*  Be more conservative in allocating memory for line coords.           */
  /*  Can range from 0 to 2 x/y pairs/pt, depending on clipping frequency. */
  /*  Assume an average of 1.5 pairs per point, then realloc as needed.    */
  /*  Another factor of 2 will enter if drawing stair step style (histo).  */

  if( lflag ) {
     allocPts = curve_ptr->length * 3 + 30;
     if( powCurvePtr->stairStep ) {
	allocPts += allocPts;
	if( powCurvePtr->boxFill )
	   allocPts += allocPts;
     }
     lCoordPtr = (double *) ckalloc(sizeof(double) * allocPts);
     if( !lCoordPtr ) {
	Tcl_SetResult(interp, "Unable to allocate memory for curve",
		      TCL_VOLATILE);
	return TCL_ERROR;
     }
  } else {
     lCoordPtr = NULL;
  }
  powCurvePtr->lCoordPtr = lCoordPtr;

  /*Keep track of wether we're really doing LOD summing during any given */
  /*iteration so we can ignore LOD processing if we aren't */
    
  LOD_summing = 0;

  /*This should make sure the first point always gets plotted */
  rx_LOD = DBL_MAX;

  nsum = rxsum = rx2sum = rysum = ry2sum = 0;
  dflag = 0;
  oldx = oldy = pX[0] = pY[0] =  DBL_MAX;

  /* if Level of Detail averaging is chosen, set the threshold for binning */
  if (LOD) {
    LODthresh = fabs(rx1 - rx0)/ (double) LOD;
  }

  if ( graph->WCS.type[0] != '\0' && strcmp(graphType, "binary") == 0 &&
       (xCount % 2 != 0 || yCount % 2 != 0) ) {
     curve_ptr->WCS.haveWCSinfo = 0;
     graph->WCS.haveWCSinfo = 0;
  }

  for (i=0;i<curve_ptr->length;i++) {
     if (x_vect != NULL) {
	rx = PowExtractDatum(x_vect,i+xoff);
     } else
	rx = i + 1;
     if (y_vect != NULL) {
	ry = PowExtractDatum(y_vect,i+yoff);
     } else
	ry = i + 1;

     if (LOD) {
       if  (rx < rx0 || rx > rx1 || 
	    ry < ry0 || ry > ry1  /*ignore points off graph*/) {  
	 /* LOD may be accumulating, but this point is NULL or off the graph */
	 /* so don't do anything with it*/
	 continue;
       }
       if (fabs(rx - rx_LOD) < LODthresh) {
	 /* we haven't reached the edge of the LOD bin yet, keep acumulating */
	 rxsum += rx;
	 rysum += ry;
	 rx2sum += rx*rx;
	 ry2sum += ry*ry;
	 nsum++;
	 LOD_summing = 1;
	 LOD_summed = 0;
	 continue;
       } else if (LOD_summing) {
	 /* we've reached current LOD, calculate mean value and continue */
	 rxsum += rx;
	 rysum += ry;
	 rx2sum += rx*rx;
	 ry2sum += ry*ry;
	 nsum++;
	 rx_LOD = rx;
	 rxm = rxsum/nsum;
	 rym = rysum/nsum;
	 sigmax = sqrt(rx2sum/nsum - rxm*rxm);
	 sigmay = sqrt(ry2sum/nsum - rym*rym);
	 rxsum = 0;
	 rx2sum = 0;
	 rysum = 0;
	 ry2sum = 0;
	 nsum = 0;
	 rx = rxm;
	 ry = rym;
	 LOD_summing = 0;
	 LOD_summed = 1;
       } else {
	 /* just a normal point and we haven't done any LOD. Reset "mark" */
	 /* for LOD averaging */
	 rx_LOD = rx;
	 LOD_summed = 0;
       }
     }
   
     if( logX && rx<=0.0 )
	rx = DBL_MAX;
     if( logY && ry<=0.0 )
	ry = DBL_MAX;

     if( rx==DBL_MAX || ry==DBL_MAX ) {
	x = y = DBL_MAX;
     } else {

        /*   Shouldn't need to do this
	if( curve_ptr->WCS.type[0] ) {
	   rx--; ry--;
	}
        */

        if ( graph->WCS.type[0] != '\0' && strcmp(graphType, "binary") == 0 && 
             (xCount % 2 != 0 || yCount % 2 != 0) ) {
           if (xCount % 2 != 0) {
              WCScdeltX = graph->WCS.wcs[coordSel].cdelt[0];
              graph->WCS.wcs[coordSel].cdelt[0] *= -1.0f;
           }

           if ( yCount % 2 != 0) {
              WCScdeltY = graph->WCS.wcs[coordSel].cdelt[1];
              graph->WCS.wcs[coordSel].cdelt[1] *= -1.0f;
           }
        }

	if( PowPixToPos( (logX ? log10(rx): rx), (logY ? log10(ry): ry),
                         &curve_ptr->WCS, &x, &y) != TCL_OK ) return TCL_ERROR;
	if( PowPosToPix( x,  y, &graph->WCS,     &x, &y) != TCL_OK ) return TCL_ERROR;

        if ( graph->WCS.type[0] != '\0' && strcmp(graphType, "binary") == 0 &&
             (xCount % 2 != 0 || yCount % 2 != 0) ) {
           if (xCount % 2 != 0) {
              graph->WCS.wcs[coordSel].cdelt[0] = WCScdeltX;
           }
           if ( yCount % 2 != 0) {
              graph->WCS.wcs[coordSel].cdelt[1] = WCScdeltY;
           }
        }

        if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
           /* at this point, in graph and curve WCS, the right x is the original left x. */
	   x = x0 + (-1.0 * x) * xmagstep;
        } else {
	   x = x0 + x * xmagstep;
        }

        if ( graph->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
	   y = y0 - (-1.0 * y) * ymagstep;
        } else {
	   y = y0 - y * ymagstep;
        }

     }

     /***************************************************************/
     /*       If we have string data for "z" create the canvas text */
     /***************************************************************/

     if (z_vect != NULL &&  z_vect->data_type == STRING_DATA && 
	 (x >= x0 && x <= x1) && (y <= y0 && y >= y1) ) {

       sprintf(xstring," %lf ",x);
       sprintf(ystring," %lf ",y);
					  
       Tcl_VarEval( interp, Tk_PathName(Tk_CanvasTkwin(canvas)),
		    " create text ",xstring,ystring," -text ",
		    ((char **)(z_vect->data_array))[i],
		    " -tags {",tagstring,"}",
		    (char *) NULL);
     }

     /*************************************************************/
     /*       DRAW THE POINT AND ERRORBARS FOR THIS POINT         */
     /*************************************************************/

  
     

     if (pflag && (x >= x0 && x <= x1) && (y <= y0 && y >= y1) ) {
	   
	/*  Load xe/ye[0] with real error value... fill in [1-4] with
	    vector offset in each of 4 directions                      */
	if( x_err || y_err || LOD_summed) {
	   if (x_err != NULL) {
	      *xe = PowExtractDatum(x_err,i+xeoff);
	   } else {
	      *xe = 0.0;
	   }
	   if (LOD_summed) {
	     *xe = sigmax;
	   }
	   if (y_err != NULL) {
	      *ye = PowExtractDatum(y_err,i+yeoff);
	   } else {
	      *ye = 0.0;
	   }
	   if (LOD_summed) {
	     *ye = sigmay;
	   }
	   if( *xe || *ye ) {
	      if( graph->WCS.type[0] ) {
		 /*  In non rectilinear coordinates... must do all 4 sides  */
		 PowPixToPos( rx+xe[0], ry      , &curve_ptr->WCS, xe+1, ye+1 );
		 PowPixToPos( rx      , ry+ye[0], &curve_ptr->WCS, xe+2, ye+2 );
		 PowPixToPos( rx-xe[0], ry      , &curve_ptr->WCS, xe+3, ye+3 );
		 PowPixToPos( rx      , ry-ye[0], &curve_ptr->WCS, xe+4, ye+4 );

		 for( seg=1; seg<5; seg++ ) {
		    PowPosToPix(xe[seg], ye[seg], &graph->WCS, xe+seg, ye+seg );
		    xe[seg] = x0 + xe[seg] * xmagstep - x;
		    ye[seg] = y0 - ye[seg] * ymagstep - y;
		 }
	      } else if( logX || logY ) {

#define LOG10(x) ( (x)>0.0 ? log10(x) : -300 )

		 PowPixToPos( (logX ? LOG10(rx+xe[0]): rx+xe[0]),
			      (logY ? LOG10(ry+ye[0]): ry+ye[0]),
			      &curve_ptr->WCS, xe+1, ye+2 );
		 PowPixToPos( (logX ? LOG10(rx-xe[0]): rx-xe[0]),
			      (logY ? LOG10(ry-ye[0]): ry-ye[0]),
			      &curve_ptr->WCS, xe+3, ye+4 );
		 PowPosToPix(xe[1], ye[2], &graph->WCS, xe+1, ye+2 );
		 PowPosToPix(xe[3], ye[4], &graph->WCS, xe+3, ye+4 );
		 xe[1] = x0 + xe[1] * xmagstep - x;
		 ye[2] = y0 - ye[2] * ymagstep - y;
		 xe[3] = x0 + xe[3] * xmagstep - x;
		 ye[4] = y0 - ye[4] * ymagstep - y;
		 ye[1] = xe[2] = ye[3] = xe[4] = 0.0;
	      } else {
		 /*  In rectilinear coords... do one corner and copy  */
		 PowPixToPos( rx+xe[0], ry+ye[0], &curve_ptr->WCS, xe+1, ye+1 );
		 PowPosToPix(    xe[1],    ye[1], &graph->WCS,     xe+1, ye+1 );
		 xe[3] = - (xe[1] = x0 + xe[1] * xmagstep - x);
		 ye[4] = - (ye[2] = y0 - ye[1] * ymagstep - y);
		 ye[1] = xe[2] = ye[3] = xe[4] = 0.0;
	      }
	   }
	}

#define CLIP(x,min,max) ( (x)<(min) ? (min) : ( (x)>(max) ? (max) : (x) ) )

	if( eflag || LOD_summed ) {
	   /* Draw error bars */
	   if( graph->WCS.type[0] ) {
	      if( *xe ) {
		 *(pCoordPtr++) = CLIP(x + xe[1],x0,x1);
		 *(pCoordPtr++) = CLIP(y + ye[1],y1,y0);
		 *(pCoordPtr++) = x;
		 *(pCoordPtr++) = y;
		 *(pCoordPtr++) = CLIP(x + xe[3],x0,x1);
		 *(pCoordPtr++) = CLIP(y + ye[3],y1,y0);
		 *(pCoordPtr++) = DBL_MAX;
		 *(pCoordPtr++) = DBL_MAX;
	      }
	      if( *ye ) {
		 *(pCoordPtr++) = CLIP(x + xe[2],x0,x1);
		 *(pCoordPtr++) = CLIP(y + ye[2],y1,y0);
		 *(pCoordPtr++) = x;
		 *(pCoordPtr++) = y;
		 *(pCoordPtr++) = CLIP(x + xe[4],x0,x1);
		 *(pCoordPtr++) = CLIP(y + ye[4],y1,y0);
		 *(pCoordPtr++) = DBL_MAX;
		 *(pCoordPtr++) = DBL_MAX;
	      }
	   } else {
	      if( *xe ) {
		 *(pCoordPtr++) = CLIP(x + xe[1],x0,x1);
		 *(pCoordPtr++) = y;
		 *(pCoordPtr++) = CLIP(x + xe[3],x0,x1);
		 *(pCoordPtr++) = y;
		 *(pCoordPtr++) = DBL_MAX;
		 *(pCoordPtr++) = DBL_MAX;
	      }
	      if( *ye ) {
		 *(pCoordPtr++) = x;
		 *(pCoordPtr++) = CLIP(y + ye[2],y1,y0);
		 *(pCoordPtr++) = x;
		 *(pCoordPtr++) = CLIP(y + ye[4],y1,y0);
		 *(pCoordPtr++) = DBL_MAX;
		 *(pCoordPtr++) = DBL_MAX;
	      }
	   }
	}
	   
	/*  Draw the shape  */

	if( powCurvePtr->pointError && pSize==0 ) {
	   xp = 0.5*(xe[1]-xe[3]);
	   yp = 0.5*(ye[2]-ye[4]);
	} else {
	   xp = yp = pSize;
	}
	for( seg=0; seg< pointShapes[pType].nLines; seg++ ) {
	   if( pointShapes[pType].lines[seg].x==99.0 ) {
	      *(pCoordPtr++) = DBL_MAX;
	      *(pCoordPtr++) = DBL_MAX;
	   } else {
	      p1 = x + xp * pointShapes[pType].lines[seg].x;
	      q1 = y + yp * pointShapes[pType].lines[seg].y;
	      *(pCoordPtr++) = CLIP(p1,x0,x1);
	      *(pCoordPtr++) = CLIP(q1,y1,y0);
	   }
	}	   
	*(pCoordPtr++) = DBL_MAX;
	*(pCoordPtr++) = DBL_MAX;
     }	    

     /*************************************************************/
     /*       DRAW THE LINE AND ERRORBARS FOR THIS POINT         */
     /*************************************************************/

     if( lflag ) {

	/*  Check lCoordPtr length  */

	usedPts = (lCoordPtr - powCurvePtr->lCoordPtr);
	if( allocPts - usedPts < 30 ) {
	   printf( "Must realloc lCoordPtr: used=%d  alloc=%d\n",
		   usedPts, allocPts );
	   allocPts += (curve_ptr->length>>2) + 30;
	   lCoordPtr = (double *)ckrealloc( (char*)powCurvePtr->lCoordPtr,
                                            sizeof(double) * allocPts );
	   if( lCoordPtr ) {
	      powCurvePtr->lCoordPtr = lCoordPtr;
	      lCoordPtr += usedPts;
	   } else {
	      /*  Memory error!!!  */
	      printf("Couldn't allocate enough memory for PowCurve coords\n");
	      ckfree( (char*)powCurvePtr->lCoordPtr );
	      powCurvePtr->lCoordPtr = NULL;
	      powCurvePtr->numLines = 0;
	      return TCL_ERROR;
	   }
	}

	if( powCurvePtr->stairStep ) {

	   if( powCurvePtr->boxFill ) {

	      if( pX[0]==DBL_MAX || pY[0]==DBL_MAX ) {
		 nPts = 0;  /*  Setup to draw inital box on next pass  */
		 PowPixToPos(   0.0,   0.0, &curve_ptr->WCS, pX+1, pY+1   );
		 PowPosToPix( pX[1], pY[1], &graph->WCS,     pX+1, &histY );
		 histY = y0 - histY * ymagstep;
		 if( histY>y0 ) histY=y0;
		 if( histY<y1 ) histY=y1;
	      } else {
		 if( nPts==0 ) {  /*  Initial box needs to be drawn  */
		    histX = 0.5*(pX[0]+x);
		    pX[1] = pX[0] - histX + pX[0];
		 } else {
		    pX[1] = histX;
		    if( x==DBL_MAX || y==DBL_MAX ) {
		       histX = pX[0]+pX[0]-histX;
		    } else {
		       histX = 0.5*(pX[0]+x);
		    }
		 }
		 pX[1] = ( pX[1]<x0 ? x0 : (pX[1]>x1 ? x1 : pX[1]) );
		 pY[1] = histY;
		 pX[2] = pX[1];
		 pY[2] = ( pY[0]>y0 ? y0 : (pY[0]<y1 ? y1 : pY[0]) );
		 pX[3] = ( histX<x0 ? x0 : (histX>x1 ? x1 : histX) );
		 pY[3] = pY[2];
		 pX[4] = pX[3];
		 pY[4] = histY;
		 pX[5] = DBL_MAX;
		 pY[5] = DBL_MAX;
		 if( pX[1]!=pX[3] && pY[1]!=pY[2] ) {
		    for( nPts=1; nPts<=5; nPts++ ) {
		       *(lCoordPtr++) = pX[nPts];
		       *(lCoordPtr++) = pY[nPts];
		    }
		 }

		 if( i==curve_ptr->length-1 && x!=DBL_MAX ) {
		    histX  = x + x - histX;
		    pX[1] = pX[4];
		    pY[1] = pY[4];
		    pX[2] = pX[1];
		    pY[2] = ( y>y0 ? y0 : (y<y1 ? y1 : y) );
		    pX[3] = ( histX<x0 ? x0 : (histX>x1 ? x1 : histX) );
		    pY[3] = pY[2];
                    pX[4] = pX[3];
                    pY[4] = histY;
		    pX[5] = DBL_MAX;
		    pY[5] = DBL_MAX;
		    if( pX[1]!=pX[3] && pY[1]!=pY[2] ) {
		       for( nPts=1; nPts<=5; nPts++ ) {
			  *(lCoordPtr++) = pX[nPts];
			  *(lCoordPtr++) = pY[nPts];
		       }
		    }
		 }
		 nPts = -1;
	      }

	   } else if( pX[0]==DBL_MAX || pY[0]==DBL_MAX ) {
	      nPts = 0;  /*  Setup to draw inital box on next pass  */
	      PowPixToPos(   0.0,   0.0, &curve_ptr->WCS, pX+1, pY+1   );
	      PowPosToPix( pX[1], pY[1], &graph->WCS,     pX+1, &histY );
	      histY = y0 - histY * ymagstep;
	   } else if( x==DBL_MAX || y==DBL_MAX ) {
	      nPts = 3;  /*  This point terminates histogram. Close box.  */
	      pX[1] = pX[0]+pX[0]-histX;
	      pY[1] = pY[0];
	      pX[2] = pX[1];
	      pY[2] = histY;
	      pX[3] = DBL_MAX;
	      pY[3] = DBL_MAX;
	   } else {
	      histX = 0.5*(pX[0]+x);
	      if( nPts==0 ) {  /*  Initial box needs to be drawn  */
		 nPts  = 2;
		 pX[1] = pX[0] - histX + pX[0];
		 pY[1] = histY;
		 pX[2] = pX[1];
		 pY[2] = pY[0];
	      } else
		 nPts = 0;
	      nPts++;
	      pX[nPts] = histX;
	      pY[nPts] = pY[0];
	      nPts++;
	      pX[nPts] = histX;
	      pY[nPts] = y;
	      if( i==curve_ptr->length-1 && x!=DBL_MAX ) {
		 histX = x + x - histX;
		 nPts++;
		 pX[nPts] = histX;
		 pY[nPts] = y;
		 nPts++;
		 pX[nPts] = histX;
		 pY[nPts] = histY;
	      }
	   }

	} else {
	   nPts = 1;
	   pX[1] = x;
	   pY[1] = y;
	}

	pX[0] = x;
	pY[0] = y;

	for( n=1; n<=nPts; n++ ) {
	   x = pX[n];
	   y = pY[n];

	   if( x==DBL_MAX || y==DBL_MAX ) {

	      if( dflag ) {
		 /*  Terminate line segment.  Go to next point  */

		 dflag = 0;
		 if( powCurvePtr->stairStep && oldx!=DBL_MAX ) {
		    *(lCoordPtr++) = oldx;
		    *(lCoordPtr++) = oldy;
		 }
		 *(lCoordPtr++) = DBL_MAX;
		 *(lCoordPtr++) = DBL_MAX;
	      }

	   } else if( (x >= x0 && x <= x1) && (y <= y0 && y >= y1) ) {
	
	      if( !dflag && oldx!=DBL_MAX) {
		 /*  Last point off graph... Find entrance point  */
	      
		 if( oldx<x0 ) {
		    oldy += (y-oldy)/(x-oldx)*(x0-oldx);
		    oldx = x0;
		 } else if( oldx>x1 ) {
		    oldy += (y-oldy)/(x-oldx)*(x1-oldx);
		    oldx = x1;
		 }
	      
		 if( oldy>y0 ) {
		    oldx += (x-oldx)/(y-oldy)*(y0-oldy);
		    oldy = y0;
		 } else if( oldy<y1 ) {
		    oldx += (x-oldx)/(y-oldy)*(y1-oldy);
		    oldy = y1;
		 }
		 *(lCoordPtr++) = oldx;
		 *(lCoordPtr++) = oldy;
		 *(lCoordPtr++) = x;
		 *(lCoordPtr++) = y;

	      } else {
		 /*  Both points on graph... connect the dots  */

		 *(lCoordPtr++) = x;
		 *(lCoordPtr++) = y;
	      }
	      dflag = 1;
	   } else if( dflag ) {
	      /*  Last point in bbox... Find exit point  */

	      modx = x;  mody = y;
	      if( modx<x0 ) {
		 mody += (mody-oldy)/(modx-oldx)*(x0-modx);
		 modx = x0;
	      } else if( modx>x1 ) {
		 mody += (mody-oldy)/(modx-oldx)*(x1-modx);
		 modx = x1;
	      }
	
	      if( mody>y0 ) {
		 modx += (modx-oldx)/(mody-oldy)*(y0-mody);
		 mody = y0;
	      } else if( mody<y1 ) {
		 modx += (modx-oldx)/(mody-oldy)*(y1-mody);
		 mody = y1;
	      }
	
	      *(lCoordPtr++) = modx;
	      *(lCoordPtr++) = mody;

	      *(lCoordPtr++) = DBL_MAX;
	      *(lCoordPtr++) = DBL_MAX;
	      dflag = 0;
	
	   } else if( oldx!=DBL_MAX ) {
	      /*  This and previous point off graph...
                          ... Does line intersect box?  */
	
	      if( !( (oldx<x0 && x<x0) || (oldx>x1 && x>x1)
		     || (oldy<y1 && y<y1) || (oldy>y0 && y>y0) ) ) {

		 if( oldx<x0 ) {
		    oldy += (y-oldy)/(x-oldx)*(x0-oldx);
		    oldx = x0;
		 } else if( oldx>x1 ) {
		    oldy += (y-oldy)/(x-oldx)*(x1-oldx);
		    oldx = x1;
		 }
		 
		 if( oldy>y0 ) {
		    oldx += (x-oldx)/(y-oldy)*(y0-oldy);
		    oldy = y0;
		 } else if( oldy<y1 ) {
		    oldx += (x-oldx)/(y-oldy)*(y1-oldy);
		    oldy = y1;
		 }
	   
		 if( (oldx>=x0 && oldx<=x1) && (oldy<=y0 && oldy>=y1) ) {
		    modx = x;  mody = y;
		    if( modx<x0 ) {
		       mody += (mody-oldy)/(modx-oldx)*(x0-modx);
		       modx = x0;
		    } else if( modx>x1 ) {
		       mody += (mody-oldy)/(modx-oldx)*(x1-modx);
		       modx = x1;
		    }
	      
		    if( mody>y0 ) {
		       modx += (modx-oldx)/(mody-oldy)*(y0-mody);
		       mody = y0;
		    } else if( mody<y1 ) {
		       modx += (modx-oldx)/(mody-oldy)*(y1-mody);
		       mody = y1;
		    }
	      
		    *(lCoordPtr++) = oldx;
		    *(lCoordPtr++) = oldy;
		    *(lCoordPtr++) = modx;
		    *(lCoordPtr++) = mody;
		    *(lCoordPtr++) = DBL_MAX;
		    *(lCoordPtr++) = DBL_MAX;
	
		 }
	   
	      }

	      dflag = 0;
	
	   }
     
	   oldx = x;
	   oldy = y;

	}  /*  End of for loop over connect-the-dots  */

     } /*  End of if lflag  */

  } /*  End of loop of # of points  */

  powCurvePtr->numPoints = (int)((pCoordPtr -  powCurvePtr->pCoordPtr)/2.0);  
  if( powCurvePtr->numPoints==0 ) {
     ckfree( (char*)powCurvePtr->pCoordPtr );
     powCurvePtr->pCoordPtr=NULL;
  }

  powCurvePtr->numLines = (int)((lCoordPtr -  powCurvePtr->lCoordPtr)/2.0);  
  if( powCurvePtr->numLines==0 ) {
     ckfree( (char*)powCurvePtr->lCoordPtr );
     powCurvePtr->lCoordPtr=NULL;
  }

  ComputePowCurveBbox(canvas,powCurvePtr);

  return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ConfigurePowCurve --
 *
 *	This procedure is invoked to configure various aspects
 *	of a powCurve item such as its background color.
 *
 * Results:
 *	A standard Tcl result code.  If an error occurs, then
 *	an error message is left in the interp's result.
 *
 * Side effects:
 *	Configuration information, such as colors and stipple
 *	patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

 int
ConfigurePowCurve(interp, canvas, itemPtr, objc, objv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* PowCurve item to reconfigure. */
    int objc;			/* Number of elements in argv.  */
    Tcl_Obj *CONST objv[];	/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    XGCValues gcValues;
    GC newGC;
    unsigned long mask;
    Tk_Window tkwin;
    Tk_State state;
    XColor *color;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc, (const char**)objv,
	    (char *) powCurvePtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
	return TCL_ERROR;
    }
    newGC = powCurvePtr->pOutline.gc;
    color = powCurvePtr->pOutline.color;
    powCurvePtr->pOutline = powCurvePtr->lOutline;
    powCurvePtr->pOutline.gc = newGC;
    powCurvePtr->pOutline.dash.number=0;
    powCurvePtr->pOutline.width=1;
    powCurvePtr->pOutline.color=color;

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    if (powCurvePtr->lOutline.activeWidth > powCurvePtr->lOutline.width ||
	    powCurvePtr->lOutline.activeDash.number > 0 ||
	    powCurvePtr->lOutline.activeColor != NULL ||
	    powCurvePtr->lOutline.activeStipple != None) {
	itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
    } else {
	itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
    }
    mask = Tk_ConfigOutlineGC(&gcValues, canvas, itemPtr,
			      &(powCurvePtr->lOutline));
    if (mask) {
      gcValues.cap_style = powCurvePtr->capStyle;
      mask |= GCCapStyle;
      gcValues.join_style = powCurvePtr->joinStyle;
      mask |= GCJoinStyle;
      newGC = Tk_GetGC(tkwin, mask, &gcValues);
      gcValues.line_width = 0;
    } else {
      newGC = None;
    }
    if (powCurvePtr->lOutline.gc != None) {
	Tk_FreeGC(Tk_Display(tkwin), powCurvePtr->lOutline.gc);
    }
    powCurvePtr->lOutline.gc = newGC;

    mask = Tk_ConfigOutlineGC(&gcValues, canvas, itemPtr,
			      &(powCurvePtr->pOutline));
    if (mask) {
      gcValues.cap_style = powCurvePtr->capStyle;
      mask |= GCCapStyle;
      gcValues.join_style = powCurvePtr->joinStyle;
      mask |= GCJoinStyle;
      newGC = Tk_GetGC(tkwin, mask, &gcValues);
      gcValues.line_width = 0;
    } else {
      newGC = None;
    }
    if (powCurvePtr->pOutline.gc != None) {
	Tk_FreeGC(Tk_Display(tkwin), powCurvePtr->pOutline.gc);
    }
    powCurvePtr->pOutline.gc = newGC;

    if ((state==TK_STATE_HIDDEN)) {
	ComputePowCurveBbox(canvas, powCurvePtr);
	return TCL_OK;
    }

    ComputePowCurveBbox(canvas, powCurvePtr);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DeletePowCurve --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a powCurve item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

 void
DeletePowCurve(canvas, itemPtr, display)
    Tk_Canvas canvas;			/* Info about overall canvas widget. */
    Tk_Item *itemPtr;			/* Item that is being deleted. */
    Display *display;			/* Display containing window for
					 * canvas. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;

    Tk_DeleteOutline(display, &(powCurvePtr->lOutline));
    if (powCurvePtr->pOutline.gc != None) {
	Tk_FreeGC(display, powCurvePtr->pOutline.gc);
    }
    if( powCurvePtr->pCoordPtr )
       ckfree( (char*)powCurvePtr->pCoordPtr );
    if( powCurvePtr->lCoordPtr )
       ckfree( (char*)powCurvePtr->lCoordPtr );
}

/*
 *--------------------------------------------------------------
 *
 * ComputePowCurveBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a powCurve.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The fields x1, y1, x2, and y2 are updated in the header
 *	for itemPtr.
 *
 *--------------------------------------------------------------
 */

 void
ComputePowCurveBbox(canvas, powCurvePtr)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    PowCurveItem *powCurvePtr;			/* Item whose bbox is to be
					 * recomputed. */
{
    double *coordPtr;
    int i, intWidth;
    double width;
    Tk_State state = powCurvePtr->header.state;
    Tk_TSOffset *tsoffset;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    if (state==TK_STATE_HIDDEN
	|| !(powCurvePtr->pCoordPtr || powCurvePtr->lCoordPtr) ) {
	powCurvePtr->header.x1 = -1;
	powCurvePtr->header.x2 = -1;
	powCurvePtr->header.y1 = -1;
	powCurvePtr->header.y2 = -1;
	return;
    }

    width = powCurvePtr->lOutline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == (Tk_Item *)powCurvePtr) {
	if (powCurvePtr->lOutline.activeWidth>width) {
	    width = powCurvePtr->lOutline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (powCurvePtr->lOutline.disabledWidth>0) {
	    width = powCurvePtr->lOutline.disabledWidth;
	}
    }

    if( powCurvePtr->pCoordPtr ) {
       powCurvePtr->header.x1 = powCurvePtr->header.x2 =
	  (int) powCurvePtr->pCoordPtr[0];
       powCurvePtr->header.y1 = powCurvePtr->header.y2 =
	  (int) powCurvePtr->pCoordPtr[1];
    } else {
       powCurvePtr->header.x1 = powCurvePtr->header.x2 =
	  (int) powCurvePtr->lCoordPtr[0];
       powCurvePtr->header.y1 = powCurvePtr->header.y2 =
	  (int) powCurvePtr->lCoordPtr[1];
    }

    /*
     * Compute the bounding box of all the points in the powCurve,
     * then expand in all directions by the powCurve's width to take
     * care of butting or rounded corners and projecting or
     * rounded caps.  This expansion is an overestimate (worst-case
     * is square root of two over two) but it's simple.  Don't do
     * anything special for curves.  This causes an additional
     * overestimate in the bounding box, but is faster.
     */

    for (i = 0, coordPtr = powCurvePtr->pCoordPtr; 
	 i < powCurvePtr->numPoints;
	 i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX )
	  TkIncludePoint((Tk_Item *) powCurvePtr, coordPtr);
    }
    for (i = 0, coordPtr = powCurvePtr->lCoordPtr; 
	 i < powCurvePtr->numLines;
	 i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX )
	  TkIncludePoint((Tk_Item *) powCurvePtr, coordPtr);
    }

    width = powCurvePtr->lOutline.width;
    if (width < 1.0) {
	width = 1.0;
    }

    tsoffset = &powCurvePtr->lOutline.tsoffset;
    if (tsoffset->flags & TK_OFFSET_INDEX) {
	double *coordPtr;
	if( powCurvePtr->pCoordPtr ) {
	   coordPtr = powCurvePtr->pCoordPtr
	      + (tsoffset->flags & ~TK_OFFSET_INDEX);
	   if (tsoffset->flags <= 0) {
	      coordPtr = powCurvePtr->pCoordPtr;
	   }
	   if (tsoffset->flags > (powCurvePtr->numPoints * 2)) {
	      coordPtr = powCurvePtr->pCoordPtr + (powCurvePtr->numPoints * 2);
	   }
	} else {
	   coordPtr = powCurvePtr->lCoordPtr
	      + (tsoffset->flags & ~TK_OFFSET_INDEX);
	   if (tsoffset->flags <= 0) {
	      coordPtr = powCurvePtr->lCoordPtr;
	   }
	   if (tsoffset->flags > (powCurvePtr->numLines * 2)) {
	      coordPtr = powCurvePtr->lCoordPtr + (powCurvePtr->numLines * 2);
	   }
	}
	tsoffset->xoffset = (int)(coordPtr[0]);
	tsoffset->yoffset = (int)(coordPtr[1]);
    } else {
	if (tsoffset->flags & TK_OFFSET_LEFT) {
	    tsoffset->xoffset = powCurvePtr->header.x1;
	} else if (tsoffset->flags & TK_OFFSET_CENTER) {
	    tsoffset->xoffset = (powCurvePtr->header.x1 + powCurvePtr->header.x2)/2;
	} else if (tsoffset->flags & TK_OFFSET_RIGHT) {
	    tsoffset->xoffset = powCurvePtr->header.x2;
	}
	if (tsoffset->flags & TK_OFFSET_TOP) {
	    tsoffset->yoffset = powCurvePtr->header.y1;
	} else if (tsoffset->flags & TK_OFFSET_MIDDLE) {
	    tsoffset->yoffset = (powCurvePtr->header.y1 + powCurvePtr->header.y2)/2;
	} else if (tsoffset->flags & TK_OFFSET_BOTTOM) {
	    tsoffset->yoffset = powCurvePtr->header.y2;
	}
    }

    intWidth = (int) (width + 0.5);
    powCurvePtr->header.x1 -= intWidth;
    powCurvePtr->header.x2 += intWidth;
    powCurvePtr->header.y1 -= intWidth;
    powCurvePtr->header.y2 += intWidth;

    if (powCurvePtr->curveObjectPtr->length==1) {
	return;
    }

    /*
     * Add one more pixel of fudge factor just to be safe (e.g.
     * X may round differently than we do).
     */

    powCurvePtr->header.x1 -= 1;
    powCurvePtr->header.x2 += 1;
    powCurvePtr->header.y1 -= 1;
    powCurvePtr->header.y2 += 1;
}

/*
 *--------------------------------------------------------------
 *
 * DisplayPowCurve --
 *
 *	This procedure is invoked to draw a powCurve item in a given
 *	drawable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	ItemPtr is drawn in drawable using the transformation
 *	information in canvas.
 *
 *--------------------------------------------------------------
 */

 void
DisplayPowCurve(canvas, itemPtr, display, drawable, x_reg, y_reg,
		width_reg, height_reg)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    Tk_Item *itemPtr;			/* Item to be displayed. */
    Display *display;			/* Display on which to draw item. */
    Drawable drawable;			/* Pixmap or window in which to draw
					 * item. */
    int x_reg, y_reg, width_reg, height_reg ;/* Describes region of canvas that
					 * must be redisplayed (not used). */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    XPoint staticPoints[100];
    XPoint *pointPtr;
    XPoint *linePtr;
    XRectangle tmpRect;
    double *coordPtr, *dPtr;
    int i, j, numPoints, numLines;
    struct {
       double x1,y1,x2,y2;
    } clipbox, bbox;
    int clipped;
    Tk_State state = itemPtr->state;
    Pixmap stipple = powCurvePtr->lOutline.stipple;

    if( powCurvePtr->hidden
	&& (powCurvePtr->numPoints + powCurvePtr->numLines)>10000 ) return;

    if (drawable == None) {
      return;
    }


    if (powCurvePtr->lOutline.gc==None) {
	return;
    }

    /*
    printf("Draw...%s %3d %3d %3d %3d  :  %6d %6d\n",
	   powCurvePtr->graphObjectPtr->graph_name,
	   x_reg, y_reg, width_reg, height_reg,
	   powCurvePtr->numPoints, powCurvePtr->numLines);
	   */

    clipbox.x1 = x_reg - 1; /* include a +-1 pixel border */
    clipbox.y1 = y_reg - 1;
    clipbox.x2 = x_reg + width_reg + 1;
    clipbox.y2 = y_reg + height_reg + 1;

    tmpRect.width = tmpRect.height = 1;

    if(state == TK_STATE_NULL) {
	    state = ((TkCanvas *)canvas)->canvas_state;
    }
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (powCurvePtr->lOutline.activeStipple!=None) {
	    stipple = powCurvePtr->lOutline.activeStipple;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (powCurvePtr->lOutline.disabledStipple!=None) {
	    stipple = powCurvePtr->lOutline.disabledStipple;
	}
    }
    /*
     * Build up an array of points in screen coordinates.  Use a
     * static array unless the powCurve has an enormous number of points;
     * in this case, dynamically allocate an array.  For smoothed powCurves,
     * generate the curve points on each redisplay.
     */

    /*  Use staticPoints for drawing the Points & Errorbars since each  */
    /*  instance will be very short. But might allocate for lines       */

    pointPtr = staticPoints;

    numLines = powCurvePtr->numLines;
    if (numLines <= 100) {
	linePtr = staticPoints;
    } else {
	linePtr = (XPoint *) ckalloc((unsigned) (numLines * sizeof(XPoint)));
    }

    /*
     * Display powCurve.
     * If we're stippling, then modify the stipple offset
     * in the GC.  Be sure to reset the offset when done, since the
     * GC is supposed to be read-only.
     */

    /*  Do Points/Errorbars first  */

    Tk_ChangeOutlineGC(canvas, itemPtr, &(powCurvePtr->pOutline));

    clipped = 1;
    numPoints = 0;
    for (i = 0, coordPtr = powCurvePtr->pCoordPtr; 
	 i < powCurvePtr->numPoints;
	 i++, coordPtr += 2) {
       if( *coordPtr != DBL_MAX ) {

	  /* Test if this point is inside clipbox */
	  if( clipped ) {

	     if( coordPtr[0]>=clipbox.x1 && coordPtr[0]<=clipbox.x2
		 && coordPtr[1]>=clipbox.y1 && coordPtr[1]<=clipbox.y2 ) {
		clipped = 0;
	     } else if( numPoints==0 ) {
		bbox.x1 = bbox.x2 = coordPtr[0];
		bbox.y1 = bbox.y2 = coordPtr[1];
	     } else {
		if( bbox.x1 > coordPtr[0] ) bbox.x1 = coordPtr[0];
		else if( bbox.x2 < coordPtr[0] ) bbox.x2 = coordPtr[0];
		if( bbox.y1 > coordPtr[1] ) bbox.y1 = coordPtr[1];
		else if( bbox.y2 < coordPtr[1] ) bbox.y2 = coordPtr[1];
	     }
	  }

	  numPoints++;
       }
       if( numPoints && (*coordPtr==DBL_MAX || i==powCurvePtr->numPoints-1) ) {

	  if( clipped ) {
	     /*  None of points inside clipbox.  Does bbox intersect clip?  */
	     if( !(bbox.x2 < clipbox.x1 || bbox.x1 > clipbox.x2
		   || bbox.y2 < clipbox.y1 || bbox.y1 > clipbox.y2) ) {
		clipped = 0;
	     }
	  }

	  if( !clipped ) {

	     dPtr = coordPtr - (numPoints+numPoints);
	     if( *coordPtr!=DBL_MAX ) dPtr+=2;
	     for( j=0; j < numPoints; j++, dPtr+=2 )
		Tk_CanvasDrawableCoords( canvas, dPtr[0], dPtr[1],
					 &pointPtr[j].x,
					 &pointPtr[j].y );

	     if( numPoints>2 && powCurvePtr->pointFill )
		XFillPolygon(display, drawable, (powCurvePtr->pOutline).gc,
			     pointPtr, numPoints, Convex, CoordModeOrigin);
	     else if( numPoints>1 )
		XDrawLines(display, drawable, (powCurvePtr->pOutline).gc,
			   pointPtr, numPoints, CoordModeOrigin);
	     else {
		tmpRect.x = pointPtr->x;    tmpRect.y = pointPtr->y;
		XFillRectangles( display, drawable, (powCurvePtr->pOutline).gc,
				 &tmpRect, 1 );
	     }
	  }
	  numPoints = 0;
	  clipped = 1;
       }
    }

    Tk_ResetOutlineGC(canvas, itemPtr, &(powCurvePtr->pOutline));

    /*  Now do the lines  */

    Tk_ChangeOutlineGC(canvas, itemPtr, &(powCurvePtr->lOutline));

    clipped = 1;
    numPoints = 0;
    for (i = 0, coordPtr = powCurvePtr->lCoordPtr; 
	 i < powCurvePtr->numLines;
	 i++, coordPtr += 2) {
       if( *coordPtr != DBL_MAX ) {

	  /* Test if this point is inside clipbox */
	  if( clipped ) {

	     if( coordPtr[0]>=clipbox.x1 && coordPtr[0]<=clipbox.x2
		 && coordPtr[1]>=clipbox.y1 && coordPtr[1]<=clipbox.y2 ) {
		clipped = 0;
	     } else if( numPoints==0 ) {
		bbox.x1 = bbox.x2 = coordPtr[0];
		bbox.y1 = bbox.y2 = coordPtr[1];
	     } else {
		if( bbox.x1 > coordPtr[0] ) bbox.x1 = coordPtr[0];
		else if( bbox.x2 < coordPtr[0] ) bbox.x2 = coordPtr[0];
		if( bbox.y1 > coordPtr[1] ) bbox.y1 = coordPtr[1];
		else if( bbox.y2 < coordPtr[1] ) bbox.y2 = coordPtr[1];
	     }
	  }

	  numPoints++;
       }
       if( numPoints && (*coordPtr==DBL_MAX || i==powCurvePtr->numLines-1) ) {

	  if( clipped ) {
	     /*  None of points inside clipbox.  Does bbox intersect clip?  */
	     if( !(bbox.x2 < clipbox.x1 || bbox.x1 > clipbox.x2
		   || bbox.y2 < clipbox.y1 || bbox.y1 > clipbox.y2) ) {
		clipped = 0;
	     }
	  }

	  if( !clipped ) {

	     dPtr = coordPtr - (numPoints+numPoints);
	     if( *coordPtr!=DBL_MAX ) dPtr+=2;
	     for( j=0; j < numPoints; j++, dPtr+=2 )
		Tk_CanvasDrawableCoords( canvas, dPtr[0], dPtr[1],
					 &linePtr[j].x,
					 &linePtr[j].y );

	     if( powCurvePtr->stairStep && powCurvePtr->boxFill ) {
		if( linePtr[0].x<linePtr[2].x ) {
		   tmpRect.x     = linePtr[0].x;
		   tmpRect.width = linePtr[2].x - tmpRect.x;
		} else {
		   tmpRect.x     = linePtr[2].x;
		   tmpRect.width = linePtr[0].x - tmpRect.x;
		}
		if( linePtr[0].y<linePtr[2].y ) {
		   tmpRect.y      = linePtr[0].y;
		   tmpRect.height = linePtr[2].y - tmpRect.y;
		} else {
		   tmpRect.y      = linePtr[2].y;
		   tmpRect.height = linePtr[0].y - tmpRect.y;
		}
		XFillRectangles( display, drawable, (powCurvePtr->lOutline).gc,
				 &tmpRect, 1 );
	     } else {
		XDrawLines(display, drawable, (powCurvePtr->lOutline).gc,
			   linePtr, numPoints, CoordModeOrigin);
	     }

	  }
	  numPoints = 0;
	  clipped = 1;
       }
    }

    Tk_ResetOutlineGC(canvas, itemPtr, &(powCurvePtr->lOutline));

    if( linePtr!=staticPoints ) ckfree( (char*)linePtr );
}


/*
 *--------------------------------------------------------------
 *
 * PowCurveInsert --
 *
 *	Insert coords into a powCurve item at a given index.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The coords in the given item is modified.
 *
 *--------------------------------------------------------------
 */

 void
PowCurveInsert(canvas, itemPtr, beforeThis, string)
    Tk_Canvas canvas;		/* Canvas containing text item. */
    Tk_Item *itemPtr;		/* PowCurve item to be modified. */
    int beforeThis;		/* Index before which new coordinates
				 * are to be inserted. */
    char *string;		/* New coordinates to be inserted. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    int length, argc, i;
    const char **argv = (const char **) NULL;
    double *new, *coordPtr;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    if(!string || !*string) {
	return;
    }
    if ((Tcl_SplitList(((TkCanvas *)canvas)->interp, string, &argc, &argv) != TCL_OK)
	    || argv == NULL || !argc || argc&1) {
	Tcl_ResetResult(((TkCanvas *)canvas)->interp);
	if (argv != NULL) {
	    ckfree((char *) argv);
	}
	return;
    }
    length = 2*powCurvePtr->numPoints;
    if (beforeThis < 0) {
	beforeThis = 0;
    }
    if (beforeThis > length) {
	beforeThis = length;
    }
    new = (double *) ckalloc((unsigned)(sizeof(double) * (length + argc)));
    for(i=0; i<beforeThis; i++) {
	new[i] = powCurvePtr->pCoordPtr[i];
    }
    for(i=0; i<argc; i++) {
	if (Tcl_GetDouble(((TkCanvas *)canvas)->interp,argv[i],
		new+(i+beforeThis))!=TCL_OK) {
	    Tcl_ResetResult(((TkCanvas *)canvas)->interp);
	    ckfree((char *) new);
	    ckfree((char *) argv);
	    return;
	}
    }

    for(i=beforeThis; i<length; i++) {
	new[i+argc] = powCurvePtr->pCoordPtr[i];
    }
    if(powCurvePtr->pCoordPtr) ckfree((char *)powCurvePtr->pCoordPtr);
    ckfree((char *) argv);
    powCurvePtr->pCoordPtr = new;
    powCurvePtr->curveObjectPtr->length = (length + argc)/2;

    if ((length>3) && (state != TK_STATE_HIDDEN)) {
	/*
	 * This is some optimizing code that will result that only the part
	 * of the polygon that changed (and the objects that are overlapping
	 * with that part) need to be redrawn. A special flag is set that
	 * instructs the general canvas code not to redraw the whole
	 * object. If this flag is not set, the canvas will do the redrawing,
	 * otherwise I have to do it here.
	 */
	itemPtr->redraw_flags |= TK_ITEM_DONT_REDRAW;

	if (beforeThis>0) {beforeThis -= 2; argc+=2; }
	if ((beforeThis+argc)<length) argc+=2;
	itemPtr->x1 = itemPtr->x2 = (int)(powCurvePtr->pCoordPtr[beforeThis]);
	itemPtr->y1 = itemPtr->y2 = (int)(powCurvePtr->pCoordPtr[beforeThis+1]);

	coordPtr = powCurvePtr->pCoordPtr+beforeThis+2;
	for(i=2; i<argc; i+=2) {
	    TkIncludePoint(itemPtr, coordPtr);
		coordPtr+=2;
	}
    }


    if(itemPtr->redraw_flags & TK_ITEM_DONT_REDRAW) {
	double width;
	int intWidth;
	width = powCurvePtr->lOutline.width;
	if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
		if (powCurvePtr->lOutline.activeWidth>width) {
		    width = powCurvePtr->lOutline.activeWidth;
		}
	} else if (state==TK_STATE_DISABLED) {
		if (powCurvePtr->lOutline.disabledWidth>0) {
		    width = powCurvePtr->lOutline.disabledWidth;
		}
	}
	intWidth = (int) (width + 0.5);
	if (intWidth < 1) {
	    intWidth = 1;
	}
	itemPtr->x1 -= intWidth; itemPtr->y1 -= intWidth;
	itemPtr->x2 += intWidth; itemPtr->y2 += intWidth;
	Tk_CanvasEventuallyRedraw(canvas, itemPtr->x1, itemPtr->y1,
		itemPtr->x2, itemPtr->y2);
    }

    ComputePowCurveBbox(canvas, powCurvePtr);
}

/*
 *--------------------------------------------------------------
 *
 * PowCurveDeleteCoords --
 *
 *	Delete one or more coordinates from a powCurve item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Characters between "first" and "last", inclusive, get
 *	deleted from itemPtr.
 *
 *--------------------------------------------------------------
 */

 void
PowCurveDeleteCoords(canvas, itemPtr, first, last)
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* Item in which to delete characters. */
    int first;			/* Index of first character to delete. */
    int last;			/* Index of last character to delete. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    int count, i, first1, last1;
    int length = 2*powCurvePtr->numPoints;
    double *coordPtr;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    first &= -2;
    last &= -2;

    if (first < 0) {
	first = 0;
    }
    if (last >= length) {
	last = length-2;
    }
    if (first > last) {
	return;
    }

    first1 = first; last1 = last;
    if(first1>0) first1 -= 2;
    if(last1<length-2) last1 += 2;

    if((first1<2) && (last1 >= length-2)) {
	/*
	 * This is some optimizing code that will result that only the part
	 * of the powCurve that changed (and the objects that are overlapping
	 * with that part) need to be redrawn. A special flag is set that
	 * instructs the general canvas code not to redraw the whole
	 * object. If this flag is set, the redrawing has to be done here,
	 * otherwise the general Canvas code will take care of it.
	 */

	itemPtr->redraw_flags |= TK_ITEM_DONT_REDRAW;
	itemPtr->x1 = itemPtr->x2 = (int)(powCurvePtr->pCoordPtr[first1]);
	itemPtr->y1 = itemPtr->y2 = (int)(powCurvePtr->pCoordPtr[first1+1]);
	coordPtr = powCurvePtr->pCoordPtr+first1+2;
	for(i=first1+2; i<=last1; i+=2) {
	    TkIncludePoint(itemPtr, coordPtr);
		coordPtr+=2;
	}
    }

    count = last + 2 - first;
    for(i=last+2; i<length; i++) {
	powCurvePtr->pCoordPtr[i-count] = powCurvePtr->pCoordPtr[i];
    }
    powCurvePtr->curveObjectPtr->length -= count/2;

    if(itemPtr->redraw_flags & TK_ITEM_DONT_REDRAW) {
	double width;
	int intWidth;
	width = powCurvePtr->lOutline.width;
	if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
		if (powCurvePtr->lOutline.activeWidth>width) {
		    width = powCurvePtr->lOutline.activeWidth;
		}
	} else if (state==TK_STATE_DISABLED) {
		if (powCurvePtr->lOutline.disabledWidth>0) {
		    width = powCurvePtr->lOutline.disabledWidth;
		}
	}
	intWidth = (int) (width + 0.5);
	if (intWidth < 1) {
	    intWidth = 1;
	}
	itemPtr->x1 -= intWidth; itemPtr->y1 -= intWidth;
	itemPtr->x2 += intWidth; itemPtr->y2 += intWidth;
	Tk_CanvasEventuallyRedraw(canvas, itemPtr->x1, itemPtr->y1,
		itemPtr->x2, itemPtr->y2);
    }
    ComputePowCurveBbox(canvas, powCurvePtr);
}

/*
 *--------------------------------------------------------------
 *
 * PowCurveToPoint --
 *
 *	Computes the distance from a given point to a given
 *	powCurve, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are pointPtr[0] and pointPtr[1] is inside the powCurve.  If the
 *	point isn't inside the powCurve then the return value is the
 *	distance from the point to the powCurve.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
 double
PowCurveToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *pointPtr;		/* Pointer to x and y coordinates. */
{
    double width;
    Tk_State state = itemPtr->state;
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
	double *powCurveLines;
    double bestDist;
    int numLines;

    bestDist = 1.0e36;

    if (!powCurvePtr->curveToPoint) return bestDist;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    width = powCurvePtr->lOutline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (powCurvePtr->lOutline.activeWidth>width) {
	    width = powCurvePtr->lOutline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (powCurvePtr->lOutline.disabledWidth>0) {
	    width = powCurvePtr->lOutline.disabledWidth;
	}
    }

    numLines = powCurvePtr->numLines;
    powCurveLines = powCurvePtr->lCoordPtr;

    if (!numLines || itemPtr->state==TK_STATE_HIDDEN) {
	return bestDist;
    } else if (numLines == 1) {
	bestDist = hypot(powCurveLines[0] - pointPtr[0],
			 powCurveLines[1] - pointPtr[1]) - width/2.0;
	if (bestDist < 0) bestDist = 0;
	return bestDist;
    }

    /*  Deleted a lot of code not needed by PowCanvCurve... for now  */

    return bestDist;
}

/*
 *--------------------------------------------------------------
 *
 * PowCurveToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given rectangular area.
 *
 * Results:
 *	-1 is returned if the item is entirely outside the
 *	area, 0 if it overlaps, and 1 if it is entirely
 *	inside the given area.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

	/* ARGSUSED */
 int
PowCurveToArea(canvas, itemPtr, rectPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against powCurve. */
    double *rectPtr;
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    int result;
    double radius, width;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }
    width = powCurvePtr->lOutline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (powCurvePtr->lOutline.activeWidth>width) {
	    width = powCurvePtr->lOutline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (powCurvePtr->lOutline.disabledWidth>0) {
	    width = powCurvePtr->lOutline.disabledWidth;
	}
    }

    radius = (width+1.0)/2.0;

    /*  Delete some code not needed by PowCanvCurve... for now  */
    result = -1;

    return result;
}

/*
 *--------------------------------------------------------------
 *
 * ScalePowCurve --
 *
 *	This procedure is invoked to rescale a powCurve item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The powCurve referred to by itemPtr is rescaled so that the
 *	following transformation is applied to all point
 *	coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

 void
ScalePowCurve(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;			/* Canvas containing powCurve. */
    Tk_Item *itemPtr;			/* PowCurve to be scaled. */
    double originX, originY;		/* Origin about which to scale rect. */
    double scaleX;			/* Amount to scale in X direction. */
    double scaleY;			/* Amount to scale in Y direction. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    double *coordPtr;
    int i;

    for (i = 0, coordPtr = powCurvePtr->pCoordPtr; i < powCurvePtr->numPoints;
	    i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX ) {
	  coordPtr[0] = originX + scaleX*(*coordPtr - originX);
	  coordPtr[1] = originY + scaleY*(coordPtr[1] - originY);
       }
    }

    for (i = 0, coordPtr = powCurvePtr->lCoordPtr; i < powCurvePtr->numLines;
	    i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX ) {
	  coordPtr[0] = originX + scaleX*(*coordPtr - originX);
	  coordPtr[1] = originY + scaleY*(coordPtr[1] - originY);
       }
    }

    ComputePowCurveBbox(canvas, powCurvePtr);
}

/*
 *--------------------------------------------------------------
 *
 * GetPowCurveIndex --
 *
 *	Parse an index into a powCurve item and return either its value
 *	or an error.
 *
 * Results:
 *	A standard Tcl result.  If all went well, then *indexPtr is
 *	filled in with the index (into itemPtr) corresponding to
 *	string.  Otherwise an error message is left in
 *	the interp's result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

 int
GetPowCurveIndex(interp, canvas, itemPtr, string, indexPtr)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item for which the index is being
				 * specified. */
    char *string;		/* Specification of a particular coord
				 * in itemPtr's powCurve. */
    int *indexPtr;		/* Where to store converted index. */
{
    /* Function not supported */

    /*
     * Some of the paths here leave messages in the interp's result,
     * so we have to clear it out before storing our own message.
     */
    
    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
    Tcl_AppendResult(interp, "bad index \"", string, "\"",
		     (char *) NULL);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * TranslatePowCurve --
 *
 *	This procedure is called to move a powCurve by a given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the powCurve is offset by (xDelta, yDelta), and
 *	the bounding box is updated in the generic part of the item
 *	structure.
 *
 *--------------------------------------------------------------
 */

 void
TranslatePowCurve(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item that is being moved. */
    double deltaX, deltaY;		/* Amount by which item is to be
					 * moved. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    double *coordPtr;
    int i;

    for (i = 0, coordPtr = powCurvePtr->pCoordPtr;
	 i < powCurvePtr->numPoints;
	 i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX ) {
	  coordPtr[0] += deltaX;
	  coordPtr[1] += deltaY;
       }
    }

    for (i = 0, coordPtr = powCurvePtr->lCoordPtr;
	 i < powCurvePtr->numLines;
	 i++, coordPtr += 2) {
       if( *coordPtr!=DBL_MAX ) {
	  coordPtr[0] += deltaX;
	  coordPtr[1] += deltaY;
       }
    }

    ComputePowCurveBbox(canvas, powCurvePtr);
}


/*
 *--------------------------------------------------------------
 *
 * PowCurveToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	powCurve items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in the interp's result, replacing whatever used
 *	to be there.  If no error occurs, then Postscript for the
 *	item is appended to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

 int
PowCurveToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Leave Postscript or error message
					 * here. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
    PowCurveItem *powCurvePtr = (PowCurveItem *) itemPtr;
    char buffer[200];
    char *style;

    double width;
    XColor *lcolor, *pcolor;
    Pixmap stipple;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    width = powCurvePtr->lOutline.width;
    lcolor = powCurvePtr->lOutline.color;
    pcolor = powCurvePtr->pOutline.color;
    stipple = powCurvePtr->lOutline.stipple;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (powCurvePtr->lOutline.activeWidth>width) {
	    width = powCurvePtr->lOutline.activeWidth;
	}
	if (powCurvePtr->lOutline.activeColor!=NULL) {
	    lcolor = powCurvePtr->lOutline.activeColor;
	    pcolor = powCurvePtr->pOutline.activeColor;
	}
	if (powCurvePtr->lOutline.activeStipple!=None) {
	    stipple = powCurvePtr->lOutline.activeStipple;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (powCurvePtr->lOutline.disabledWidth>0) {
	    width = powCurvePtr->lOutline.disabledWidth;
	}
	if (powCurvePtr->lOutline.disabledColor!=NULL) {
	    lcolor = powCurvePtr->lOutline.disabledColor;
	    pcolor = powCurvePtr->pOutline.disabledColor;
	}
	if (powCurvePtr->lOutline.disabledStipple!=None) {
	    stipple = powCurvePtr->lOutline.disabledStipple;
	}
    }

    if (lcolor == NULL
	|| ( 
	    (powCurvePtr->numPoints<1 || powCurvePtr->pCoordPtr==NULL)
	    && (powCurvePtr->numLines<1 || powCurvePtr->lCoordPtr==NULL)
           ) ) {
	return TCL_OK;
    }

    if (powCurvePtr->numPoints==1) {
	sprintf(buffer, "%.15g %.15g translate %.15g %.15g",
		powCurvePtr->pCoordPtr[0],
		Tk_CanvasPsY(canvas, powCurvePtr->pCoordPtr[1]),
		width/2.0, width/2.0);
	Tcl_AppendResult(interp, "matrix currentmatrix\n",buffer,
			 " scale 1 0 moveto 0 0 1 0 360 arc\nsetmatrix\n",
			 (char *) NULL);
	if (Tk_CanvasPsColor(interp, canvas, pcolor)
		!= TCL_OK) {
	    return TCL_ERROR;
	}
	if (stipple != None) {
	    Tcl_AppendResult(interp, "clip ", (char *) NULL);
	    if (Tk_CanvasPsStipple(interp, canvas, stipple) != TCL_OK) {
		return TCL_ERROR;
	    }
	} else {
	    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
	}
    }

    if (powCurvePtr->numLines==1) {
	sprintf(buffer, "%.15g %.15g translate %.15g %.15g",
		powCurvePtr->lCoordPtr[0],
		Tk_CanvasPsY(canvas, powCurvePtr->lCoordPtr[1]),
		width/2.0, width/2.0);
	Tcl_AppendResult(interp, "matrix currentmatrix\n",buffer,
			 " scale 1 0 moveto 0 0 1 0 360 arc\nsetmatrix\n",
			 (char *) NULL);
	if (Tk_CanvasPsColor(interp, canvas, lcolor)
		!= TCL_OK) {
	    return TCL_ERROR;
	}
	if (stipple != None) {
	    Tcl_AppendResult(interp, "clip ", (char *) NULL);
	    if (Tk_CanvasPsStipple(interp, canvas, stipple) != TCL_OK) {
		return TCL_ERROR;
	    }
	} else {
	    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
	}
    }
    if( powCurvePtr->numPoints<=1 && powCurvePtr->numLines<=1 )
       return TCL_OK;

    /*
     * Set powCurve-drawing parameters
     */

    style = "0 setlinecap\n";
    if (powCurvePtr->capStyle == CapRound) {
	style = "1 setlinecap\n";
    } else if (powCurvePtr->capStyle == CapProjecting) {
	style = "2 setlinecap\n";
    }
    Tcl_AppendResult(interp, style, (char *) NULL);
    style = "0 setlinejoin\n";
    if (powCurvePtr->joinStyle == JoinRound) {
	style = "1 setlinejoin\n";
    } else if (powCurvePtr->joinStyle == JoinBevel) {
	style = "2 setlinejoin\n";
    }
    Tcl_AppendResult(interp, style, (char *) NULL);

    /*
     * Generate a path for the powCurve's center-line (do this differently
     * for straight powCurves and smoothed powCurves).
     */

    {
       TkCanvas *cnvs = (TkCanvas*)canvas;
       double *coordPtr;
       char buffer[200];
       int nPts, i;

       if (Tk_CanvasPsColor(interp, canvas, pcolor) != TCL_OK) {
          return TCL_ERROR;
       }
       coordPtr = powCurvePtr->pCoordPtr;
       nPts = 0;
       for ( i=0, coordPtr=powCurvePtr->pCoordPtr;
	     i < powCurvePtr->numPoints;
	     i++, coordPtr += 2) {

	  if( *coordPtr!=DBL_MAX ) {
	     if( nPts ) {
		sprintf(buffer, "%.15g %.15g lineto\n", coordPtr[0],
			Tk_PostscriptY(coordPtr[1], cnvs->psInfo));
	     } else {
		sprintf(buffer, "%.15g %.15g moveto\n", coordPtr[0],
			Tk_PostscriptY(coordPtr[1], cnvs->psInfo));
	     }
	     Tcl_AppendResult(interp, buffer, (char *) NULL);
	     nPts++;
	  }
	  if( nPts && (*coordPtr==DBL_MAX || i == powCurvePtr->numPoints-1) ) {

	     /*
	      * Stroke out the point/error bar.
	      */

	     if( nPts>2 && powCurvePtr->pointFill ) {
		
		if (stipple != None) {
		   Tcl_AppendResult(interp, "clip ", (char *) NULL);
		   if (Tk_CanvasPsStipple(interp, canvas, stipple) != TCL_OK) {
		      return TCL_ERROR;
		   }
		} else {
		   Tcl_AppendResult(interp, "fill\n", (char *) NULL);
		}

	     } else if ( nPts==1 ) {
		if( *coordPtr!=DBL_MAX ) nPts=0;
		sprintf(buffer, "%.15g %.15g 0.5 0.0 360.0 arc fill\n",
			coordPtr[-2*nPts],
			Tk_PostscriptY(coordPtr[-2*nPts+1], cnvs->psInfo));
		Tcl_AppendResult(interp, buffer, (char *) NULL);
	     } else if (Tk_CanvasPsOutline( canvas, itemPtr,
				     &(powCurvePtr->pOutline) ) != TCL_OK) {
		return TCL_ERROR;
	     }
	     nPts = 0;
	  }
       }


       if (Tk_CanvasPsColor(interp, canvas, lcolor) != TCL_OK) {
          return TCL_ERROR;
       }
       coordPtr = powCurvePtr->lCoordPtr;
       nPts = 0;
       for ( i=0, coordPtr=powCurvePtr->lCoordPtr;
	     i < powCurvePtr->numLines;
	     i++, coordPtr += 2) {

	  if( *coordPtr!=DBL_MAX ) {
	     if( nPts ) {
		sprintf(buffer, "%.15g %.15g lineto\n", coordPtr[0],
			Tk_PostscriptY(coordPtr[1], cnvs->psInfo));
	     } else {
		sprintf(buffer, "%.15g %.15g moveto\n", coordPtr[0],
			Tk_PostscriptY(coordPtr[1], cnvs->psInfo));
	     }
	     Tcl_AppendResult(interp, buffer, (char *) NULL);
	     nPts++;
	  }
	  if( nPts && (*coordPtr==DBL_MAX || i == powCurvePtr->numLines-1) ) {

	     /*
	      * Stroke out the powCurve.
	      */

	     if ( powCurvePtr->stairStep && powCurvePtr->boxFill ) {
		   Tcl_AppendResult(interp, "fill\n", (char *) NULL);
	     } else {
		if (Tk_CanvasPsOutline(canvas, itemPtr,
				       &(powCurvePtr->lOutline)) != TCL_OK) {
		   return TCL_ERROR;
		}
	     }
	     nPts = 0;
	  }
       }


    }

    return TCL_OK;
}

void outDebugStr(char *title, char *str) {

    char *p;
    char currstr[90];

    p = str;
    
    fprintf(stdout, "Title: <%s>\n", title);
    fflush(stdout); 

    while (1) {
      if ( strlen(p) <= 0 ) break;
      strncpy(currstr, p, 80);
      currstr[80] = '\0';
      fprintf(stdout, "currstr: <%s>\n", currstr);
      fflush(stdout); 
      p += 80;
    }
}
