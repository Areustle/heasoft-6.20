#include <math.h>
#include <string.h>
#include <stdio.h>
#include "pow.h"

#define DEG2RAD 1.745329252e-2

#define NUM_WCS_TYPES 27
static char wcsProjections[NUM_WCS_TYPES][5] =
      {"-AZP", "-SZP", "-TAN", "-STG", "-SIN", "-ARC", "-ZPN", "-ZEA", "-AIR", "-CYP",
       "-CEA", "-CAR", "-MER", "-COP", "-COE", "-COD", "-COO", "-SFL", "-PAR", "-MOL",
       "-AIT", "-BON", "-PCO", "-TSC", "-CSC", "-QSC", "-HPX"};

void PowInitWCS( WCSdata *WCS, int n )
{
   int row, col;

   WCS->RaDecSwap = 0;
   WCS->nAxis     = n;
   for( row=0; row<n; row++ ) {
      WCS->refVal[row] = 0.0;
      WCS->refPix[row] = 0.0;
      for( col=0; col<n; col++ ) {
         WCS->cdFrwd[row][col] = (row==col?1:0);
         WCS->cdRvrs[row][col] = (row==col?1:0);
      }
   }
   memset (WCS->type, '\0', 6);
   memset (WCS->graphName, '\0', 1024);
   memset (WCS->curveName, '\0', 1024);
   wcsini (1, n, WCS->graphName);
   WCS->haveWCSinfo = 0;
}

int FillinWCSStructure ( WCSdata *WCS )
{
   char powFitsHeader[14]="powFitsHeader";
   char powFitsHeaderCnt[17]="powFitsHeaderCnt";
   int i, relax, HDRcnt, ctrl, nreject=0, nwcs=0;
   const char *HDRstring;
   int status;
   int coordSel;
   Tcl_Obj *listObj;
   Tcl_Obj *wcsname[27];

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
      char errormsg[512];
      sprintf(errormsg, "Can't construct WCS information: %s", WCSpih_Message[status]);
      Tcl_SetResult(interp, errormsg ,TCL_VOLATILE);
      Tcl_SetVar(interp,"powWCSTranslation", WCSpih_Message[status] ,TCL_GLOBAL_ONLY);
      return TCL_ERROR;
   }

   listObj = Tcl_NewObj();
   for (i=0; i<nwcs; i++) {
       wcsname[i] = Tcl_NewStringObj(WCS->wcs[i].alt,-1);
   }

   Tcl_ListObjAppendElement( interp, listObj, Tcl_NewIntObj( nwcs ) );
   Tcl_ListObjAppendElement( interp, listObj, Tcl_NewListObj(nwcs,wcsname) );

   if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
      Tcl_SetVar2Ex(interp,"powWCSList", WCS->graphName, listObj, TCL_GLOBAL_ONLY);
   } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
      Tcl_SetVar2Ex(interp,"powWCSList", WCS->curveName, listObj, TCL_GLOBAL_ONLY);
   }

   if (nwcs > 0 ) {
      if ( WCS->graphName[0] != '\0' && strcmp(WCS->graphName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->graphName,TCL_GLOBAL_ONLY));
      } else if ( WCS->curveName[0] != '\0' && strcmp(WCS->curveName, "NULL") != 0 ) {
         coordSel = atoi(Tcl_GetVar2(interp,"powWCSName",WCS->curveName,TCL_GLOBAL_ONLY));
      }

      WCS->wcs[coordSel].crpix[0] = WCS->refPix[0];
      WCS->wcs[coordSel].crpix[1] = WCS->refPix[1];
   }

   WCS->haveWCSinfo = 1;
   return TCL_OK;
}

int PowParseWCS( Tcl_Interp *interp, WCSdata *WCS,
                 int argc, Tcl_Obj *const argv[] )
{
   /* Known coordinate types (from worldpos below) */
   double xinc,yinc,rot;
   double refVal[MAX_WCS_DIMS],refPix[MAX_WCS_DIMS];
   double cdFrwd[MAX_WCS_DIMS][MAX_WCS_DIMS],cdRvrs[MAX_WCS_DIMS][MAX_WCS_DIMS];
   double norm;
   Tcl_Obj **listElems;
   int row, col, nElem, nDims, dim;
   char *type;
   int swap=0;

   PowInitWCS( WCS, MAX_WCS_DIMS );

   if( argc>7 ) {
      Tcl_GetDoubleFromObj(interp,argv[0],refVal+0);
      Tcl_GetDoubleFromObj(interp,argv[1],refVal+1);
      Tcl_GetDoubleFromObj(interp,argv[2],refPix+0);
      Tcl_GetDoubleFromObj(interp,argv[3],refPix+1);
      Tcl_GetDoubleFromObj(interp,argv[4],&xinc);
      Tcl_GetDoubleFromObj(interp,argv[5],&yinc);
      Tcl_GetDoubleFromObj(interp,argv[6],&rot);
      type = Tcl_GetStringFromObj(argv[7],NULL);

      if( argc>8 ) Tcl_GetBooleanFromObj(interp,argv[8],&swap);

      cdFrwd[0][0] =  xinc * cos( rot * DEG2RAD );
      cdFrwd[0][1] = -yinc * sin( rot * DEG2RAD );
      cdFrwd[1][0] =  xinc * sin( rot * DEG2RAD );
      cdFrwd[1][1] =  yinc * cos( rot * DEG2RAD );
      nDims = 2;

   } else {

      nDims = 1;

      Tcl_ListObjGetElements(interp,argv[0],&nElem,&listElems);
      if( nElem>MAX_WCS_DIMS ) nElem = MAX_WCS_DIMS;
      if( nDims < nElem ) nDims = nElem;
      for( row=0; row<nElem; row++ )
         Tcl_GetDoubleFromObj(interp, listElems[row], refVal+row);

      Tcl_ListObjGetElements(interp,argv[1],&nElem,&listElems);
      if( nElem>MAX_WCS_DIMS ) nElem = MAX_WCS_DIMS;
      if( nDims < nElem ) nDims = nElem;
      for( row=0; row<nElem; row++ )
         Tcl_GetDoubleFromObj(interp, listElems[row], refPix+row);

      Tcl_ListObjGetElements(interp,argv[2],&nElem,&listElems);
      dim = (int)(sqrt( (double)nElem )+0.5);
      nElem = ( dim>MAX_WCS_DIMS ? MAX_WCS_DIMS : dim );
      if( nDims < nElem ) nDims = nElem;
      for( row=0; row<nElem; row++ )
         for( col=0; col<nElem; col++ )
            Tcl_GetDoubleFromObj(interp, listElems[row*dim+col],
                                 &cdFrwd[row][col]);

      /*  Check if RA/Dec axes are swapped  */
      Tcl_ListObjGetElements(interp,argv[3],&nElem,&listElems);
      if( nElem ) {
         type = Tcl_GetStringFromObj(listElems[0],NULL);
         if( type[0] && (!strcmp(type, "DEC") || !strcmp(type+1, "LAT")) )
            swap = 1;
      }

      Tcl_ListObjGetElements(interp,argv[4],&nElem,&listElems);
      type = Tcl_GetStringFromObj(listElems[0],NULL);

   }

   if( swap ) {        /* Transform WCS data so that it gives RA/Dec */
      norm      = refVal[0];
      refVal[0] = refVal[1];
      refVal[1] = norm;
      for( col=0; col<nDims; col++ ) {
         norm           = cdFrwd[0][col];
         cdFrwd[0][col] = cdFrwd[1][col];
         cdFrwd[1][col] = norm;
      }
   }

   /*
    *  Test for unknown projection type.
    */

   if (strcmp(type, "none") == 0) {
      type[0] = '\0';
   } else if( nDims != 2 ) { /*  Only 2D projections allowed  */
      type[0] = '\0';
   } 

   /*
    *  Calculate the inverse transform
    */

   if( nDims == 1 ) {
      cdRvrs[0][0] = 1.0/cdFrwd[0][0];
   } else if( nDims==2 ) {
      norm = cdFrwd[0][0]*cdFrwd[1][1] - cdFrwd[0][1]*cdFrwd[1][0];
      cdRvrs[0][0] =   cdFrwd[1][1] / norm;
      cdRvrs[0][1] = - cdFrwd[0][1] / norm;
      cdRvrs[1][0] = - cdFrwd[1][0] / norm;
      cdRvrs[1][1] =   cdFrwd[0][0] / norm;
   } else {
      return TCL_ERROR;
   }
   
   /*
    *  Copy data into image's WCS structure
    */

   WCS->RaDecSwap = swap;
   WCS->nAxis     = nDims;
   if( *type && refVal[0]<0.0 ) refVal[0] += 360.0;
   for( row=0; row<nDims; row++ ) {
      WCS->refVal[row] = refVal[row];
      WCS->refPix[row] = refPix[row];
      for( col=0; col<nDims; col++ ) {
         WCS->cdFrwd[row][col] = cdFrwd[row][col];
         WCS->cdRvrs[row][col] = cdRvrs[row][col];
      }
   }

   if ( *type ) {
      strcpy(WCS->type,type);
   }

   /* PowDumpWCSstructure(WCS); */
   return TCL_OK;
}

void PowDumpWCSstructure ( WCSdata *WCS ) {
   fprintf(stdout, "**********************************\n");
   fprintf(stdout, "WCS->graphName  : <%s>\n", WCS->graphName);
   fprintf(stdout, "WCS->curveName  : <%s>\n", WCS->curveName);
   fprintf(stdout, "WCS->type       : <%s>\n", WCS->type);
   fprintf(stdout, "WCS->RaDecSwap  : <%d>\n", WCS->RaDecSwap);
   fprintf(stdout, "WCS->nAxis      : <%d>\n", WCS->nAxis);
   fprintf(stdout, "WCS->refVal[0]  : <%20.15f>\n", WCS->refVal[0]);
   fprintf(stdout, "WCS->refVal[1]  : <%20.15f>\n", WCS->refVal[1]);
   fprintf(stdout, "WCS->refPix[0]  : <%20.15f>\n", WCS->refPix[0]);
   fprintf(stdout, "WCS->refPix[1]  : <%20.15f>\n", WCS->refPix[1]);
   fprintf(stdout, "WCS->cdFrwd[0]  : <%20.15f,%20.15f>\n", WCS->cdFrwd[0][0], WCS->cdFrwd[0][1]);
   fprintf(stdout, "WCS->cdFrwd[1]  : <%20.15f,%20.15f>\n", WCS->cdFrwd[1][0], WCS->cdFrwd[1][1]);
   fprintf(stdout, "WCS->cdRvrs[0]  : <%20.15f,%20.15f>\n", WCS->cdRvrs[0][0], WCS->cdRvrs[0][1]);
   fprintf(stdout, "WCS->cdRvrs[1]  : <%20.15f,%20.15f>\n", WCS->cdRvrs[1][0], WCS->cdRvrs[1][1]);
   fprintf(stdout, "WCS->rot        : <%20.15f>\n", WCS->rot);
   fprintf(stdout, "WCS->haveWCSinfo: <%d>\n", WCS->haveWCSinfo);
   fprintf(stdout, "**********************************\n");
   fflush(stdout);
}

int PowWCSInitImage( ClientData clientData, Tcl_Interp *interp, 
                     int argc, Tcl_Obj *const argv[] )
{
   /* Fills in the origin, increment, and otherend fields in the 
      specified image using the WCS info provided.  This is mainly
      used by powCreateImage and by the callback proc for the trace
      on powWCS */
   double xpos,ypos;
   char *imageName;
   PowImage *image_ptr;
   int n;
  
   if( argc < 6 || argc > 11 ) {
      Tcl_SetResult( interp,
                     "usage: powWCSInitImage image xref yref xrefpix "
                     "yrefpix xinc yinc rot type ?swap?\n"
                     "   or: powWCSInitImage image {refVal} {refPix} "
                     "{matrix} {type} {proj}",
                     TCL_VOLATILE );
      return TCL_ERROR;
   }

   imageName = Tcl_GetStringFromObj(argv[1],NULL);
   image_ptr = PowFindImage( imageName );
   if (image_ptr == (PowImage *) NULL) {
      Tcl_SetResult( interp, "Couldn't find image.", TCL_VOLATILE );
      return TCL_ERROR;
   }

   PowParseWCS(interp, &image_ptr->WCS, argc-2, argv+2);

   /* add image name to WCS structure */
   strcpy (image_ptr->WCS.graphName, imageName);
   image_ptr->WCS.haveWCSinfo = 0;

   for( n=0; n<image_ptr->WCS.nAxis; ) {
      image_ptr->WCS.refPix[n++]--; /*  Makes pixels zero-indexed  */
   }

   if( !image_ptr->WCS.type[0]) {
     /* Tcl_SetVar2(interp,"powWCS",imageName,"",TCL_GLOBAL_ONLY); */
   }

   /*
   image_ptr->WCS.xref      = (swap?yref:xref);
   image_ptr->WCS.yref      = (swap?xref:yref);
   image_ptr->WCS.xrefpix   = xrefpix - 1; 
   image_ptr->WCS.yrefpix   = yrefpix - 1; 
   image_ptr->WCS.xinc      = xinc;
   image_ptr->WCS.yinc      = (swap?-yinc:yinc);
   image_ptr->WCS.rot       = (swap?90-rot:rot);
   */

   if( PowPixToPos( -0.5, -0.5, &image_ptr->WCS, &xpos, &ypos ) ) {
      Tcl_SetResult( interp,
                     "Couldn't translate pixels to WCS coords for image "
                     "initialization", TCL_VOLATILE );
      return TCL_ERROR;
   }
    
   image_ptr->xorigin = xpos;
   image_ptr->yorigin = ypos;

   if( PowPixToPos( image_ptr->width-0.5, image_ptr->height-0.5,
                    &image_ptr->WCS, &xpos, &ypos )  ) {
      Tcl_SetResult( interp,
                     "Couldn't translate pixels to WCS coords for "
                     "image initialization", TCL_VOLATILE );
      return TCL_ERROR;
   }

   image_ptr->xotherend = xpos;
   image_ptr->yotherend = ypos;

   image_ptr->xinc = ( xpos - image_ptr->xorigin ) / image_ptr->width;
   image_ptr->yinc = ( ypos - image_ptr->yorigin ) / image_ptr->height;

   return TCL_OK;
}


int PowWCSInitCurve(ClientData clientData, Tcl_Interp *interp, 
		    int argc, Tcl_Obj *const argv[])
{
   /* Fills in the WCS structure if info exists.  This is mainly
      used by powCreateCurve and by the callback proc for the trace
      on powWCS */
   PowCurve *curve_ptr;
   char *curveName;
   int str_len;
   char *p;
  
   if( argc < 7 || argc > 11 ) {
      Tcl_SetResult( interp,
                     "usage: powWCSInitCurve curve xref yref xrefpix "
                     "yrefpix xinc yinc rot type ?swap?\n"
                     "   or: powWCSInitCurve curve {refVal} {refPix} "
                     "{matrix} {type} {proj}",
                     TCL_VOLATILE );
      return TCL_ERROR;
   }

   curveName = Tcl_GetStringFromObj( argv[1], NULL );
   curve_ptr = PowFindCurve( curveName );
   if (curve_ptr == (PowCurve *) NULL) {
      Tcl_SetResult( interp, "Couldn't find curve.", TCL_VOLATILE );
      return TCL_ERROR;
   }

   PowParseWCS( interp, &curve_ptr->WCS, argc-2, argv+2 );

   /* add curve name to WCS structure */
   strcpy (curve_ptr->WCS.curveName, curveName);

   p = strstr(curveName, "_contour");
   if ( p != (char *)NULL ) {
      /* input is contour curve, grab its graph handler */
      str_len = strlen(curve_ptr->WCS.curveName) - strlen(p);
      strncpy(curve_ptr->WCS.graphName, curve_ptr->WCS.curveName, str_len);
      curve_ptr->WCS.graphName[str_len] = '\0';
   }

   if ( curve_ptr->WCS.type[0] == '\0' ) {
      /* for some reason, this has to be done for Windows. */
/*
      curve_ptr->WCS.refPix[0] = 0.0;
      curve_ptr->WCS.refPix[1] = 0.0;
*/
   }

   FillinWCSStructure(&curve_ptr->WCS);

   if ( curve_ptr->WCS.type[0] == '\0' ) {
      const char *WCSstring;

      WCSstring = Tcl_GetVar2(interp, "powWCS", curveName,TCL_GLOBAL_ONLY);
      /* Tcl_SetVar2(interp,"powWCS", curveName, "", TCL_GLOBAL_ONLY); */
   }
   return TCL_OK;
}

int PowWCSInitGraph( PowGraph *graph, char *curves, char *images,
		     int x_points_right, int y_points_up)
{
   PowCurve *current_curve;
   PowImage *current_image;
   int index,Argc;
   const char **Argv;
   char *p;

   graph->WCS.type[0] = '\0';
   graph->xoff = 0.0;
   graph->yoff = 0.0;
   if(images != NULL && strstr(images,"NULL") == NULL ) {

      if(Tcl_SplitList(interp,images,&Argc,&Argv) != TCL_OK) {
	 return TCL_ERROR;
      }
      for( index=0; index<Argc; index++ ) {
	 current_image = PowFindImage( Argv[index] );
	 if( current_image->WCS.type[0] ) {
	    graph->WCS = current_image->WCS;
	    ckfree( (char *)Argv );
	    return TCL_OK;
	 }
      }

      /*  Failed to find a WCS image.  Grab first image's WCS structure
          anyway... It could still contain linear scaling.              */

      graph->WCS = PowFindImage( Argv[0] )->WCS;
      /* wcsini (1, 2, graph->WCS.wcs); */
      p = strstr (images, "imgobj_");
      if ( p != (char *)NULL ) {
         p += strlen("imgobj_");
         strcpy(graph->WCS.graphName, p);
      } else {
         strcpy(graph->WCS.graphName, images);
      }
      strcpy(graph->WCS.curveName, "\0");
      ckfree( (char *)Argv );
      return TCL_OK;
   }

   if(curves != NULL && strstr(curves,"NULL") == NULL ) {

      if(Tcl_SplitList(interp,curves,&Argc,&Argv) != TCL_OK) {
	 return TCL_ERROR;
      }
      for( index=0; index<Argc; index++ ) {
	 current_curve = PowFindCurve( Argv[index] );
	 if( current_curve->WCS.type[0] ) {
	    graph->WCS = current_curve->WCS;
            strcpy(graph->WCS.graphName, "\0");
            strcpy(graph->WCS.curveName, curves);
	    ckfree( (char *)Argv );
	    return TCL_OK;
	 }
      }
      ckfree( (char *)Argv );
   }

   PowInitWCS( &graph->WCS, 2 );
   if( !x_points_right ) {
      graph->WCS.cdFrwd[0][0] = -1.0;
   }
   if( !y_points_up ) {
      graph->WCS.cdFrwd[1][1] = -1.0;
   }

   return TCL_OK;
}

int PowXYPx(ClientData clientData, Tcl_Interp *interp, 
            int argc, Tcl_Obj *const argv[])
{
   /* Calls the pow_xypx WCS routine, returns list of 2 image pixels*/
   double xpix,ypix,xpos,ypos;
   Tcl_Obj *res[2];
   WCSdata WCS;
  
   if(argc < 11 ) {
     Tcl_SetResult( interp,
                    "usage: powXYPx xpos ypos xref yref xrefpix yrefpix "
                    "xinc yinc rot type", TCL_VOLATILE );
     return TCL_ERROR;
   }

   Tcl_GetDoubleFromObj(interp,argv[1],&xpos);
   Tcl_GetDoubleFromObj(interp,argv[2],&ypos);

   PowParseWCS( interp, &WCS, argc-3, argv+3 );

   if( PowPosToPix(xpos,ypos,&WCS,&xpix,&ypix) != 0 ) {
     Tcl_SetResult( interp, "Couldn't translate WCS coords to pixels",
                    TCL_VOLATILE );
     return TCL_ERROR;
   }

   res[0] = Tcl_NewDoubleObj( xpix );
   res[1] = Tcl_NewDoubleObj( ypix );

   Tcl_SetObjResult(interp, Tcl_NewListObj(2, res) );

   return TCL_OK;
}


int PowWorldPos(ClientData clientData, Tcl_Interp *interp, 
                int argc, Tcl_Obj *const argv[])
{
   /* Calls the pow_worldpos WCS routine, returns list of 2 graph coords*/
   double xpix,ypix,xpos,ypos;
   Tcl_Obj *res[2];
   WCSdata WCS;
  
   if(argc < 11 ) {
     Tcl_SetResult( interp,
                    "usage: powWorldPos xpix ypix xref yref xrefpix "
                    "yrefpix xinc yinc rot type", TCL_VOLATILE );
     return TCL_ERROR;
   }

   Tcl_GetDoubleFromObj(interp,argv[1],&xpix);
   Tcl_GetDoubleFromObj(interp,argv[2],&ypix);

   PowParseWCS( interp, &WCS, argc-3, argv+3 );

   if( PowPixToPos(xpix,ypix,&WCS,&xpos,&ypos) != 0 ) {
     Tcl_SetResult( interp, "Couldn't translate pixels to WCS coords",
                    TCL_VOLATILE );
     return TCL_ERROR;
   }

   res[0] = Tcl_NewDoubleObj( xpos );
   res[1] = Tcl_NewDoubleObj( ypos );

   Tcl_SetObjResult(interp, Tcl_NewListObj(2, res) );

   return TCL_OK;
}

/*--------------------------------------------------------------------------*/
int pow_worldpos(double xpix, double ypix,
                 double refVal[], double refPix[],
                 double matrix[][MAX_WCS_DIMS],
                 char *type, double *xpos, double *ypos)

/* PDW 03/00: Add -CAR projection support                                  */
/* PDW 02/00: Change interface to use more general matrix/vector notation  */
/* LEB 11/97: change the name of the routine from 'ffwldp' to 'pow_worldpos' 
              rexmoved 'status' argument, convert to (0,0) based images to
              match POW convention. */
/* WDP 1/97: change the name of the routine from 'worldpos' to 'ffwldp' */

/*  worldpos.c -- WCS Algorithms from Classic AIPS.
    Copyright (C) 1994
    Associated Universities, Inc. Washington DC, USA.
   
    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.
   
    This library is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.
   
    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
   
    Correspondence concerning AIPS should be addressed as follows:
           Internet email: aipsmail@nrao.edu
           Postal address: AIPS Group
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA

                 -=-=-=-=-=-=-

    These two ANSI C functions, worldpos() and xypix(), perform
    forward and reverse WCS computations for 8 types of projective
    geometries ("-SIN", "-TAN", "-ARC", "-NCP", "-GLS", "-MER", "-AIT"
    and "-STG"):

        worldpos() converts from pixel location to RA,Dec 
        xypix()    converts from RA,Dec         to pixel location   

    where "(RA,Dec)" are more generically (long,lat). These functions
    are based on the WCS implementation of Classic AIPS, an
    implementation which has been in production use for more than ten
    years. See the two memos by Eric Greisen

        ftp://fits.cv.nrao.edu/fits/documents/wcs/aips27.ps.Z
	ftp://fits.cv.nrao.edu/fits/documents/wcs/aips46.ps.Z

    for descriptions of the 8 projective geometries and the
    algorithms.  Footnotes in these two documents describe the
    differences between these algorithms and the 1993-94 WCS draft
    proposal (see URL below). In particular, these algorithms support
    ordinary field rotation, but not skew geometries (CD or PC matrix
    cases). Also, the MER and AIT algorithms work correctly only for
    CRVALi=(0,0). Users should note that GLS projections with yref!=0
    will behave differently in this code than in the draft WCS
    proposal.  The NCP projection is now obsolete (it is a special
    case of SIN).  WCS syntax and semantics for various advanced
    features is discussed in the draft WCS proposal by Greisen and
    Calabretta at:
    
        ftp://fits.cv.nrao.edu/fits/documents/wcs/wcs.all.ps.Z
    
                -=-=-=-

    The original version of this code was Emailed to D.Wells on
    Friday, 23 September by Bill Cotton <bcotton@gorilla.cv.nrao.edu>,
    who described it as a "..more or less.. exact translation from the
    AIPSish..". Changes were made by Don Wells <dwells@nrao.edu>
    during the period October 11-13, 1994:
    1) added GNU license and header comments
    2) added testpos.c program to perform extensive circularity tests
    3) changed float-->double to get more than 7 significant figures
    4) testpos.c circularity test failed on MER and AIT. B.Cotton
       found that "..there were a couple of lines of code [in] the wrong
       place as a result of merging several Fortran routines." 
    5) testpos.c found 0h wraparound in xypix() and worldpos().
    6) E.Greisen recommended removal of various redundant if-statements,
       and addition of a 360d difference test to MER case of worldpos(). 
*/

/*-----------------------------------------------------------------------*/
/* routine to determine accurate position for pixel coordinates          */
/* returns 0 if successful otherwise:                                    */
/* 1 = angle too large for projection;                                   */
/* (WDP 1/97: changed the return value to 501 instead of 1)              */
/* does: -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT projections            */
/* anything else is linear                                               */
/* Input:                                                                */
/*   f   xpix    x pixel number  (RA or long without rotation)           */
/*   f   ypiy    y pixel number  (dec or lat without rotation)           */
/*   d   xref    x reference coordinate value (deg)                      */
/*   d   yref    y reference coordinate value (deg)                      */
/*   f   xrefpix x reference pixel                                       */
/*   f   yrefpix y reference pixel                                       */
/*   f   xinc    x coordinate increment (deg)                            */
/*   f   yinc    y coordinate increment (deg)                            */
/*   f   rot     rotation (deg)  (from N through E)                      */
/*   c  *type    projection type code e.g. "-SIN";                       */
/* Output:                                                               */
/*   d   *xpos   x (RA) coordinate (deg)                                 */
/*   d   *ypos   y (dec) coordinate (deg)                                */
/*-----------------------------------------------------------------------*/
{
   double dx, dy, dz, x, y, z;
   double sins, coss, dect, rat, dt, l, m, mg, da, dd, cos0, sin0;
   double dec0, ra0;
   double geo1, geo2, geo3;
   double pi    = 3.14159265358979323846;
   double cond2r= pi/180.0;
   double deps = 1.0e-5;
   int itype;

   /*  Apply Transform Matrix  */

   dx = matrix[0][0] * (xpix-refPix[0]) + matrix[0][1] * (ypix-refPix[1]);
   dy = matrix[1][0] * (xpix-refPix[0]) + matrix[1][1] * (ypix-refPix[1]);

   /*  find type  */
   /* WDP 1/97: removed support for default type for better error checking */
   for( itype=0; itype<NUM_WCS_TYPES; itype++ )
      if( !strncmp(type, wcsProjections[itype], 4) ) break;

   /* convert to radians  */
   ra0  = refVal[0] * cond2r;
   dec0 = refVal[1] * cond2r;
   l    =  dx  * cond2r;
   m    =  dy  * cond2r;
   sins = l*l + m*m;
   cos0 = cos(dec0);
   sin0 = sin(dec0);

   /* process by case  */
   switch (itype) {
    case 0:   /* -SIN sin*/ 
      if (sins>1.0) return( 501);
      coss = sqrt (1.0 - sins);
      dt = sin0 * coss + cos0 * m;
      if ((dt>1.0) || (dt<-1.0)) return(501);
      dect = asin(dt);
      rat = cos0 * coss - sin0 * m;
      if ((rat==0.0) && (l==0.0)) return(501);
      rat = atan2(l, rat) + ra0;
      break;
    case 1:   /* -TAN tan */
      x = cos0*cos(ra0) - l*sin(ra0) - m*cos(ra0)*sin0;
      y = cos0*sin(ra0) + l*cos(ra0) - m*sin(ra0)*sin0;
      z = sin0                       + m*         cos0;
      rat  = atan2( y, x );
      dect = atan( z / sqrt(x*x+y*y) );
      break;
    case 2:   /* -ARC Arc*/
      if (sins>=pi*pi) return(501);
      sins = sqrt(sins);
      coss = cos(sins);
      if (sins!=0.0) sins = sin(sins) / sins;
      else
	sins = 1.0;
      dt = m * cos0 * sins + sin0 * coss;
      if ((dt>1.0) || (dt<-1.0)) return(501);
      dect = asin(dt);
      da = coss - dt * sin0;
      dt = l * sins * cos0;
      if ((da==0.0) && (dt==0.0)) return(501);
      rat = ra0 + atan2(dt, da);
      break;
    case 3:   /* -NCP North celestial pole*/
      dect = cos0 - m * sin0;
      if (dect==0.0) return(501);
      rat = ra0 + atan2(l, dect);
      dt = cos(rat-ra0);
      if (dt==0.0) return(501);
      dect = dect / dt;
      if ((dect>1.0) || (dect<-1.0)) return(501);
      dect = acos(dect);
      if (dec0<0.0) dect = -dect;
      break;
    case 4:   /* -GLS global sinusoid */
      dect = dec0 + m;
      if (fabs(dect)>pi/2.0) return(501);
      coss = cos(dect);
      if (fabs(l)>pi*coss) return(501);
      rat = ra0;
      if (coss>deps) rat = rat + l / coss;
      break;
    case 5:   /* -MER mercator */
      /*  dt = yinc * cosr + xinc * sinr;  */
      /* Calculate the declination change for a (1,1) offset from refpix */
      dt = matrix[1][0] + matrix[1][1];
      if (dt==0.0) dt = 1.0;
      dy = (refVal[1]*0.5 + 45.0) * cond2r;
      dx = dy + dt / 2.0 * cond2r;
      dy = log (tan(dy));
      dx = log (tan(dx));
      geo2 = dt * cond2r / (dx - dy);
      geo3 = geo2 * dy;
      geo1 = cos(refVal[1]*cond2r);
      if (geo1<=0.0) geo1 = 1.0;
      rat = l / geo1 + ra0;
      if (fabs(rat - ra0) > pi+pi) return(501);
      dt = 0.0;
      if (geo2!=0.0) dt = (m + geo3) / geo2;
      dt = exp (dt);
      dect = 2.0 * atan(dt) - pi / 2.0;
      break;
    case 6:   /* -AIT Aitoff */
      /*  dt = yinc * cosr + xinc * sinr;  */
      /* Calculate the declination change for a (1,1) offset from refpix */
      dt = matrix[1][0] + matrix[1][1];
      if (dt==0.0) dt = 1.0;
      dt = dt * cond2r;
      dy = dec0;
      dx = sin(dy+dt)/sqrt((1.0+cos(dy+dt))/2.0) -
	  sin(dy)/sqrt((1.0+cos(dy))/2.0);
      if (dx==0.0) dx = 1.0;
      geo2 = dt / dx;
      /* Calculate the right ascension change for a (1,1) offset from refpix */
      /* dt = xinc*cosr - yinc* sinr; */
      dt = matrix[0][0] + matrix[0][1];
      if (dt==0.0) dt = 1.0;
      dt = dt * cond2r;
      dx = 2.0 * cos(dy) * sin(dt/2.0);
      if (dx==0.0) dx = 1.0;
      geo1 = dt * sqrt((1.0+cos(dy)*cos(dt/2.0))/2.0) / dx;
      geo3 = geo2 * sin(dy) / sqrt((1.0+cos(dy))/2.0);
      rat = ra0;
      dect = dec0;
      if ((l==0.0) && (m==0.0)) break;
      dz = 4.0 - l*l/(4.0*geo1*geo1) - ((m+geo3)/geo2)*((m+geo3)/geo2);
      if ((dz>4.0) || (dz<2.0)) return(501);
      dz = 0.5 * sqrt (dz);
      dd = (m+geo3) * dz / geo2;
      if (fabs(dd)>1.0) return(501);
      dd = asin(dd);
      if (fabs(cos(dd))<deps) return(501);
      da = l * dz / (2.0 * geo1 * cos(dd));
      if (fabs(da)>1.0) return(501);
      da = asin(da);
      rat = ra0 + 2.0 * da;
      dect = dd;
      break;
    case 7:   /* -STG Sterographic*/
      dz = (4.0 - sins) / (4.0 + sins);
      if (fabs(dz)>1.0) return(501);
      dect = dz * sin0 + m * cos0 * (1.0+dz) / 2.0;
      if (fabs(dect)>1.0) return(501);
      dect = asin(dect);
      rat = cos(dect);
      if (fabs(rat)<deps) return(501);
      rat = l * (1.0+dz) / (2.0 * rat);
      if (fabs(rat)>1.0) return(501);
      rat = asin(rat);
      mg = 1.0 + sin(dect) * sin0 + cos(dect) * cos0 * cos(rat);
      if (fabs(mg)<deps) return(501);
      mg = 2.0 * (sin(dect) * cos0 - cos(dect) * sin0 * cos(rat)) / mg;
      if (fabs(mg-m)>deps) rat = pi - rat;
      rat = ra0 + rat;
      break;
   case 8:    /*  -CAR Cartesian  */
      rat  = ra0  + l;
      dect = dec0 + m;
      /*  Should do some sperical wrapping, but can't get it to work, yet
      if( dect >  0.5*pi ) { dect =  pi - dect; rat += pi; }
      if( dect < -0.5*pi ) { dect = -pi - dect; rat += pi; }
      */ 
      break;

    default:
      /* fall through to here on error */
      return(504);
   }

   /*  return ra in range  */
   /*  Oh let's not.  LEB */
   /*
     if (rat-ra0> pi) rat -= pi + pi;
     if (rat-ra0<-pi) rat += pi + pi;
     if (rat < 0.0)   rat += pi + pi; // added by DCW 10/12/94
   */

   /*  correct units back to degrees  */
   *xpos  = rat  / cond2r;
   *ypos  = dect / cond2r;

   /*  Do bounds check in degree space since values are exact  */
   if     ( *xpos <    0.0 )
      *xpos += 360.0;
   else if( *xpos >= 360.0 )
      *xpos -= 360.0;

   return 0;
}  /* End of worldpos */


/*--------------------------------------------------------------------------*/
int pow_xypx(double xpos, double ypos,
             double refVal[], double refPix[],
             double matrixF[][MAX_WCS_DIMS],
             double matrixR[][MAX_WCS_DIMS],
             char *type, double *xpix, double *ypix)
/* PDW 03/00: Add -CAR projection support                                  */
/* PDW 02/00: Change interface to use more general matrix/vector notation  */
/* LEB 11/97: change the name of the routine from 'ffxypx' to 'pow_xypx'
   removed 'status' argument, convert to (0,0) based images to match POW 
   convention. */
/* WDP  1/97: changed name of routine from xypix to ffxypx    */
/*-----------------------------------------------------------------------*/
/* routine to determine accurate pixel coordinates for an RA and Dec     */
/* returns 0 if successful otherwise:                                    */
/* 1 = angle too large for projection;                                   */
/* 2 = bad values                                                        */
/* WDP 1/97: changed the return values to 501 and 502 instead of 1 and 2 */
/* does: -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT projections            */
/* anything else is linear                                               */
/* Input:                                                                */
/*   d   xpos    x (RA) coordinate (deg)                                 */
/*   d   ypos    y (dec) coordinate (deg)                                */
/*   d   xref    x reference coordinate value (deg)                      */
/*   d   yref    y reference coordinate value (deg)                      */
/*   f   xrefpix x reference pixel                                       */
/*   f   yrefpix y reference pixel                                       */
/*   f   xinc    x coordinate increment (deg)                            */
/*   f   yinc    y coordinate increment (deg)                            */
/*   f   rot     rotation (deg)  (from N through E)                      */
/*   c  *type    projection type code e.g. "-SIN";                       */
/* Output:                                                               */
/*   f  *xpix    x pixel number  (RA or long without rotation)           */
/*   f  *ypiy    y pixel number  (dec or lat without rotation)           */
/*-----------------------------------------------------------------------*/
{
   double dx, dy, ra0, dec0, ra, dec, coss, sins, dt, da, dd, sint;
   double cos0, sin0, dRA;
   double l, m, geo1, geo2, geo3;
   double deps=1.0e-5;
   double pi    = 3.14159265358979323846;
   double cond2r= pi/180.0;
   int itype;

   /*  find type  */
   /* WDP 1/97: removed support for default type for better error checking */
   for( itype=0; itype<NUM_WCS_TYPES; itype++ )
      if (!strncmp(type, wcsProjections[itype], 4)) break;

   if ( xpos < 0.0 )
      xpos += 360.0;

   /* Non linear position */
   ra0  = refVal[0] * cond2r;
   dec0 = refVal[1] * cond2r;
   ra   = xpos * cond2r;
   dec  = ypos * cond2r;

   dRA  = ra-ra0;
   if(      dRA >   pi ) dRA -= pi + pi;
   else if( dRA <= -pi ) dRA += pi + pi;

   /* compute direction cosine */
   coss = cos(dec);
   sins = sin(dec);
   cos0 = cos(dec0);
   sin0 = sin(dec0);
   l = sin(dRA) * coss;
   sint = sins * sin0 + coss * cos0 * cos(dRA);

   /* process by case  */
   switch (itype) {
    case 0:   /* -SIN sin*/ 
         if (sint<0.0) return(501);
         m = sins * cos(dec0) - coss * sin(dec0) * cos(dRA);
      break;
    case 1:   /* -TAN tan */
         if (sint<=0.0) return(501);
         if( cos0<0.001 ) {
            /* Do a first order expansion around pole */
            m = (coss * cos(dRA)) / (sins * sin0);
            m = (-m + cos0 * (1.0 + m*m)) / sin0;
         } else {
            m = ( sins/sint - sin0 ) / cos0;
         }
	 if( fabs(sin(ra0)) < 0.3 ) {
	    l  = coss*sin(ra)/sint - cos0*sin(ra0) + m*sin(ra0)*sin0;
	    l /= cos(ra0);
	 } else {
	    l  = coss*cos(ra)/sint - cos0*cos(ra0) + m*cos(ra0)*sin0;
	    l /= -sin(ra0);
	 }
      break;
    case 2:   /* -ARC Arc*/
         m = sins * sin(dec0) + coss * cos(dec0) * cos(dRA);
         if (m<-1.0) m = -1.0;
         if (m>1.0) m = 1.0;
         m = acos(m);
         if (m!=0) 
            m = m / sin(m);
         else
            m = 1.0;
         l = l * m;
         m = (sins * cos(dec0) - coss * sin(dec0) * cos(dRA)) * m;
      break;
    case 3:   /* -NCP North celestial pole*/
         if (dec0==0.0) 
	     return(501);  /* can't stand the equator */
         else
	   m = (cos(dec0) - coss * cos(dRA)) / sin(dec0);
      break;
    case 4:   /* -GLS global sinusoid */
         if (fabs(dec) >pi*0.5) return(501);
         if (fabs(dec0)>pi*0.5) return(501);
         m = dec - dec0;
         l = dRA * coss;
      break;
    case 5:   /* -MER mercator */
         /*  dt = yinc * cosr + xinc * sinr;  */
         /* Calculate the declination change for a (1,1) offset from refpix */
         dt = matrixF[1][0] + matrixF[1][1];
         if (dt==0.0) dt = 1.0;
         dy = (dec0 + 90.0 * cond2r) * 0.5;
         dx = dy + (dt * 0.5) * cond2r;
         dy = log (tan(dy));
         dx = log (tan(dx));
         geo2 = dt * cond2r / (dx - dy);
         geo3 = geo2 * dy;
         l  = dRA * cos(dec0);
         dt = dec * 0.5 + pi * 0.25;
         dt = tan(dt);
         if (dt<deps) return(502);
         m = geo2 * log (dt) - geo3;
         break;
    case 6:   /* -AIT Aitoff */
         da = 0.5 * dRA;
         if (fabs(dRA)>pi) return(501);
         /* Calculate the declination change for a (1,1) offset from refpix */
         /*  dt = yinc * cosr + xinc * sinr;  */
         dt = matrixF[1][0] + matrixF[1][1];
         if (dt==0.0) dt = 1.0;
         dt = dt * cond2r;
         dy = dec0;
         dx = sin(dy+dt)/sqrt((1.0+cos(dy+dt))/2.0) -
             sin(dy)/sqrt((1.0+cos(dy))/2.0);
         if (dx==0.0) dx = 1.0;
         geo2 = dt / dx;
         /* Calculate the RA change for a (1,1) offset from refpix */
         /* dt = xinc*cosr - yinc* sinr; */
         dt = matrixF[0][0] + matrixF[0][1];
         if (dt==0.0) dt = 1.0;
         dt = dt * cond2r;
         dx = 2.0 * cos(dy) * sin(dt/2.0);
         if (dx==0.0) dx = 1.0;
         geo1 = dt * sqrt((1.0+cos(dy)*cos(dt/2.0))/2.0) / dx;
         geo3 = geo2 * sin(dy) / sqrt((1.0+cos(dy))/2.0);
         dt = sqrt ((1.0 + cos(dec) * cos(da))/2.0);
         if (fabs(dt)<deps) return(503);
         l = 2.0 * geo1 * cos(dec) * sin(da) / dt;
         m = geo2 * sin(dec) / dt - geo3;
      break;
    case 7:   /* -STG Sterographic*/
         if (fabs(dec)>0.5*pi) return(501);
         dd = 1.0 + sins * sin(dec0) + coss * cos(dec0) * cos(dRA);
         if (fabs(dd)<deps) return(501);
         dd = 2.0 / dd;
         l = l * dd;
         m = dd * (sins * cos(dec0) - coss * sin(dec0) * cos(dRA));
      break;
   case 8:    /* -CAR Cartesian  */
      /* l = ra - ra0; */
      l = dRA;
      m = dec - dec0;
      break;

    default:
      /* fall through to here on error */
      return(504);

   }  /* end of itype switch */

   /*   back to degrees  */
   dx = l / cond2r;
   dy = m / cond2r;

   /*  Apply Transform Matrix  */

   *xpix = matrixR[0][0] * dx + matrixR[0][1] * dy + refPix[0];
   *ypix = matrixR[1][0] * dx + matrixR[1][1] * dy + refPix[1];

   return 0;
}  /* end xypix */
