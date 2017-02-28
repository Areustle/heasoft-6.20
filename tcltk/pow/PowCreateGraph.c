#include "pow.h"

void PowCreateGraph(char *graph_name, char *curves, char *images,
		    char *xunits, char *yunits, char *xlabel, char *ylabel,
		    int *xdimdisp, int *ydimdisp, double *xmin_in, 
		    double *ymin_in, double *xmax_in, double *ymax_in, 
		    int *status) {
char whichPowCanvas[9]=".pow.pow";

PowCreateGraph_internal(graph_name, curves, images, xunits, yunits,
			xlabel, ylabel, xdimdisp, ydimdisp, xmin_in, 
			ymin_in, xmax_in, ymax_in, whichPowCanvas, 
			status) ;

}




void PowCreateGraph_internal(char *graph_name, char *curves, char *images,
		    char *xunits, char *yunits, char *xlabel, char *ylabel,
		    int *xdimdisp, int *ydimdisp, double *xmin_in, 
		    double *ymin_in, double *xmax_in, double *ymax_in, 
		    char *whichPowCanvas, int *status) {
  /* xdimdisp and ydimdisp are the *displayed* size of the new graph it will be
     zoomed or shrunk by an integral or 1/integral factor to come as
     close as possible to filling this requested space. If xdimdisp and 
     ydimdisp are both 0, the graph will appear at Magstep 1 */

  PowGraph *graph_instance;
  Tcl_HashEntry *entry_ptr;
  int new = 0;
  double xmin,xmax,ymin,ymax,xdim,ydim,tmp;
  int xmargin,ymargin;
  double xoff, yoff;
  char bbox[128];
  char extraparams[256], *pPtr;
  char *str_ptr;
  char *aspect="no";
  int in_limits;
  int x_points_right,y_points_up;
  int len;
  int zoomed;
  char *idxStr;
  const char *graphType;
  int xCount, yCount;
  const char *WCSvalue;
  char errormsg[512];

  in_limits = 1;

  entry_ptr = Tcl_CreateHashEntry(&PowGraphTable, graph_name, &new);

  if ( new ) {
    graph_instance = (PowGraph *) ckalloc(sizeof(PowGraph));
    if(graph_instance == NULL) {
      *status = TCL_ERROR;
      Tcl_SetResult( interp, "Couldn't ckalloc graph structure space",
                     TCL_VOLATILE );
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }

    Tcl_SetHashValue( entry_ptr, graph_instance);

    /*  Copy graph_name into graph's structure  */

    str_ptr = ckalloc(strlen(graph_name)+1);
    strcpy(str_ptr,graph_name);
    graph_instance->graph_name = str_ptr;

  } else {

#ifdef DEBUG  
    printf("Reusing graph name: %s\n",graph_name);
#endif

    graph_instance = (PowGraph *) Tcl_GetHashValue( entry_ptr );

    /*  Free up old string pointers  */

    ckfree(graph_instance->xunits);
    ckfree(graph_instance->yunits);
    ckfree(graph_instance->xlabel);
    ckfree(graph_instance->ylabel);

  }

  if (xmin_in != NULL && xmax_in != NULL && *xmin_in > *xmax_in) {
     x_points_right = 0;
  } else {
     x_points_right = 1;
  }

  if (ymin_in != NULL && ymax_in != NULL && *ymin_in > *ymax_in) {
     y_points_up = 0;
  } else {
     y_points_up = 1;
  }

  graph_instance->WCS.haveWCSinfo = 0;

  PowWCSInitGraph( graph_instance, curves, images,
		   x_points_right, y_points_up);

/*
  FillinWCSStructure ( &graph_instance->WCS );
  image_instance = PowFindImage(images);
  FillinWCSStructure ( &image_instance->WCS );
*/

  /*  Do we need to keep a fixed Aspect ratio?  */

  if( graph_instance->WCS.type[0]
      || ( images != NULL && strstr(images,"NULL") == NULL ) )
      aspect = "yes";


  /*
   *   If any of the min/max values are not specified, search
   *   the graph's contents for its bounding box.
   */

  if( xmin_in==NULL || xmax_in==NULL || ymin_in==NULL || ymax_in==NULL ) {

     if( PowFindGraphBBox( graph_instance, images, curves,
                           &xmin, &xmax, &ymin, &ymax       ) != TCL_OK ) {
        *status = TCL_ERROR;
        Tcl_AppendResult( interp, "\nError locating curves' bounding boxes",
                          NULL );
        ckfree( (char *)graph_instance->graph_name );
        ckfree( (char *)graph_instance );
        Tcl_DeleteHashEntry(entry_ptr);
        return;
     }
  }

  /*  Now apply supplied bounding box values  */

  if (xmin_in != NULL) xmin = *xmin_in;
  if (xmax_in != NULL) xmax = *xmax_in;
  if (ymin_in != NULL) ymin = *ymin_in;
  if (ymax_in != NULL) ymax = *ymax_in;

  if (xmin == xmax) {
    if(xmin == 0) {
      xmax = 1;
    } else {
      xmin *= 0.9;
      xmax *= 1.1;
    }
  }

  if (ymin == ymax) {
    if (ymin == 0) {
      ymax = 1;
    } else {
      ymin *= 0.9;
      ymax *= 1.1;
    }
  }

  len    = strlen(graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "graphType", graph_name);
  graphType = Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY);
  ckfree(idxStr);

  len    = strlen(graph_name)+15;
  idxStr = (char *) ckalloc( len*sizeof(char) );
  sprintf(idxStr, "%s,%s", "zoomed", graph_name);
  zoomed = atoi(Tcl_GetVar2(interp,"powPlotParam",idxStr,TCL_GLOBAL_ONLY));
  ckfree(idxStr);

  xCount = atoi(Tcl_GetVar2(interp,"xCount",graph_name,TCL_GLOBAL_ONLY));
  yCount = atoi(Tcl_GetVar2(interp,"yCount",graph_name,TCL_GLOBAL_ONLY));

  if ( graph_instance->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
     graph_instance->WCS.cdFrwd[0][0] = 1.0;
  } 
  if ( graph_instance->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
     graph_instance->WCS.cdFrwd[1][1] = 1.0;
  }

  if( PowSortGraphMinMax(graph_instance,&xmin,&xmax,&ymin,&ymax,&xdim,&ydim) ) {
     /*  Bounding box is invalid.  Force default bounding box.  */

     PowFindGraphBBox( graph_instance, images, curves, &xmin, &xmax, &ymin, &ymax );
     PowSortGraphMinMax(graph_instance,&xmin,&xmax,&ymin,&ymax,&xdim,&ydim);
  }

  WCSvalue = Tcl_GetVar(interp,"powWCSTranslation",TCL_GLOBAL_ONLY);

  if (WCSvalue[0] != '0') {
    sprintf(errormsg, "\nError translating WCS information. error:<%s>.", WCSvalue);
    *status = TCL_ERROR;
    Tcl_AppendResult( interp, errormsg, NULL );
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  /* Chai 06/29/2007:
      We are not actually fliping the coordinates on the canvas. If tk allows this, then there is
      no need to do the following. What the logic below is to trick pow to think that the point on
      the canvas has been flipped. The xCount and yCount indicate if the graph has been flipped
      before. So if X has been previously flipped, the next flipping occurs on Y, the logic inside
      ..Count % 2 will make sure the information on previous flip still maintained. */

  if ( graph_instance->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && xCount % 2 != 0 ) {
     /* previous flip */
     tmp = xmin;
     xmin = xmax;
     xmax = tmp;
  }

  if ( graph_instance->WCS.type[0] == '\0' && strcmp(graphType, "binary") == 0 && yCount % 2 != 0 ) {
     /* previous flip */
     tmp = ymin;
     ymin = ymax;
     ymax = tmp;
  }

  graph_instance->xleft  = xmin;
  graph_instance->xright = xmax;
  graph_instance->ybot   = ymin;
  graph_instance->ytop   = ymax;

  PowPosToPix( xmin, ymin, &graph_instance->WCS, &xoff, &yoff );
  graph_instance->WCS.refPix[0] -= xoff;
  graph_instance->WCS.refPix[1] -= yoff;

  graph_instance->xoff -= xoff;
  graph_instance->yoff -= yoff;

  str_ptr = ckalloc(strlen(xunits)+1);
  strncpy(str_ptr,xunits,strlen(xunits)+1);
  graph_instance->xunits = str_ptr;
  str_ptr = ckalloc(strlen(yunits)+1);
  strncpy(str_ptr,yunits,strlen(yunits)+1);
  graph_instance->yunits = str_ptr;
  str_ptr = ckalloc(strlen(xlabel)+1);
  strncpy(str_ptr,xlabel,strlen(xlabel)+1);
  graph_instance->xlabel = str_ptr;
  str_ptr = ckalloc(strlen(ylabel)+1);
  strncpy(str_ptr,ylabel,strlen(ylabel)+1);
  graph_instance->ylabel = str_ptr;

  
  sprintf(bbox," %#.17g %#.17g %#.17g %#.17g", graph_instance->xleft, 
	  graph_instance->xright, graph_instance->ybot, graph_instance->ytop);

  if( xdimdisp && *xdimdisp<=0 ) *xdimdisp = (int)xdim;
  if( ydimdisp && *ydimdisp<=0 ) *ydimdisp = (int)ydim;

  xmargin = 80;
  ymargin = 60;

  sprintf(extraparams," %#.17g %#.17g ", xdim, ydim );

  /*  Handle possible NULL value of dimdisp's  */

  pPtr = extraparams + strlen(extraparams);
  if( xdimdisp )
     sprintf(pPtr, "%d ", *xdimdisp);
  else
     sprintf(pPtr, "NULL ");
  pPtr += strlen( pPtr );

  if( ydimdisp )
     sprintf(pPtr, "%d ", *ydimdisp);
  else
     sprintf(pPtr, "NULL ");
  pPtr += strlen( pPtr );

  sprintf(pPtr, "%s %d %d ",aspect, xmargin, ymargin);

  if ( Tcl_VarEval(interp, "powInitGraph ", graph_name, bbox," {",
		   xunits,"} {", yunits,"} {",xlabel,"} {",ylabel,"} ",
		   whichPowCanvas, extraparams, (char *) NULL) == TCL_ERROR) {
    *status = TCL_ERROR;
    Tcl_AppendResult( interp, "\nError initializing graph.", NULL );
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  };

  if( images==NULL ) images="NULL";
  if( curves==NULL ) curves="NULL";
  if ( Tcl_VarEval(interp, "powBuildGraph ", graph_name,
		   " [list ", images," ] ", " [list ", curves," ] ",
		   whichPowCanvas, (char *) NULL)
       == TCL_ERROR) {
     *status =  TCL_ERROR;
     Tcl_AppendResult( interp, "\nError building graph.", NULL );
     Tcl_DeleteHashEntry(entry_ptr);
     return;
  }

  if ( !strcmp( whichPowCanvas, ".pow.pow" ) ) {
     if ( Tcl_VarEval(interp, "powSelectGraph ", graph_name, (char *) NULL)
	  == TCL_ERROR) {
	*status =  TCL_ERROR;
        Tcl_AppendResult( interp, "\nError selecting graph.", NULL );
	Tcl_DeleteHashEntry(entry_ptr);
	return;
     }
  }
}



void PowDestroyGraph(char *graph_name, int *status) {
  Tcl_HashEntry *entry_ptr;
  char errormsg[1024];
  PowGraph *graph_ptr;
  
  entry_ptr = Tcl_FindHashEntry(&PowGraphTable,graph_name);
  
  if (entry_ptr == NULL) {
    *status = TCL_ERROR;
    sprintf(errormsg,"Can't find POWGraph Object %s to destroy",graph_name);
    Tcl_SetResult(interp,errormsg,TCL_VOLATILE);
    return;
  }

  Tcl_VarEval(interp,"powUnmapGraph ",graph_name,(char *)NULL);
  Tcl_VarEval(interp,"powFreeGraph ", graph_name,(char *)NULL);

  graph_ptr = (PowGraph *)Tcl_GetHashValue(entry_ptr);

  /*Delete the entry from the master POWData Hash*/
  Tcl_DeleteHashEntry(entry_ptr);
  
  /*free the PowGraph memory itself and the string holding the name and labels,
     although this is small change*/
  ckfree(graph_ptr->graph_name);
  ckfree(graph_ptr->xunits);
  ckfree(graph_ptr->yunits);
  ckfree(graph_ptr->xlabel);
  ckfree(graph_ptr->ylabel);
  ckfree((char*)graph_ptr);

  return;
}
