#include <string.h>
#include <ctype.h>
#include "cfortran.h"
#include "ast.h"
#include "../include/maxvals.h"
#include "../include/null.h"
#include "../include/xcommon.h"
#include "../include/wcsmanager.h"

/*
 *  Remap functionality split between remapprep.c and remapcore.c
 *   to allow outside use of remap functionality.  remapcore.c is
 *   is completely independent of ximage, requiring only AST
 */
void remapf(int mode, double *params, AstFrameSet *inwcs, float *inary, 
            int inszx, int inszy, float nulval, AstFrameSet *outwcs, 
            float *outary, int outszx, int outszy, int *status);

/* Prototype (implemented below) */
int remapmode(char *methstr, int numparm, double *params);

/*
 * remapprep
 * Prepare for remapping operation on image grid
 *
 * methstr   (c) Interpolation method (see remapmode fcn)
 * numparm   (i) Input number of interpolation parameters
 * params    (d) Interpolation parameters
 * inwcsid   (c) Input WCS id
 * inary     (f) Input image map
 * inszx/y   (i) Size of input image map
 * outwcsid  (c) Output WCS id (Corresponding wcs will be updated)
 * origszx/y (i) Original size of image the coordinates of which
 *               are being used to do the remap
 * outary    (f) Ouput image map
 * outszx/y  (i) Size of output image map
 * status    (i) Error flag (0=OK)
 */
void
remapprep(char *methstr, int numparm, double *params,
          char *inwcsid, float *inary, int inszx, int inszy,
          char *outwcsid, int origszx, int origszy, float *outary,
          int outszx, int outszy, int *status)
{
   AstFrameSet *inwcs, *outwcs;
   int mode, ingrid, outgrid;
   double ina[2], inb[2], outa[2], outb[2];
   int insvframe, outsvframe;

   *status = 0;
   
   mode = remapmode(methstr, numparm, params);
   if ( !mode ) {
      cxwrite("ERROR: invalid remap mode", 5);
      *status = -1;
      return;
   }
   inwcs = getwcs(inwcsid);
   if ( !inwcs ) {
      cxwrite("ERROR: remap input wcsid is empty", 5);
      *status = -1;
      return;
   }
   outwcs = getmapwcs(outwcsid);
   if ( !outwcs ) {
      cxwrite("ERROR: remap output wcsid is empty", 5);
      *status = -1;
      return;
   }
   astBegin;
   ingrid = getframeidx(inwcs, "GRID");
   insvframe = astGetI(inwcs, "Current");
   astSetI(inwcs, "Current", ingrid);
   outgrid = getframeidx(outwcs, "GRID");
   outsvframe = astGetI(outwcs, "Current");
   astSetI(outwcs, "Current", outgrid);
   if ( origszx && origszy ) {
      ina[0] = 1;
      ina[1] = 1;
      outa[0] = 1;
      outa[1] = 1;
      inb[0] = origszx;
      inb[1] = origszy;
      outb[0] = outszx;
      outb[1] = outszy;
      astRemapFrame(outwcs, outgrid, 
                  astWinMap(2, ina, inb, outa, outb, ""));
   }
   remapf(mode, params, inwcs, inary, inszx, inszy, RNULL(),
          outwcs, outary, outszx, outszy, status);
   astSetI(inwcs, "Current", insvframe);
   astSetI(outwcs, "Current", outsvframe);
   astEnd;
}
FCALLSCSUB14(remapprep,REMAPPREP,remapprep,STRING,INT,DOUBLEV,STRING,FLOATV,INT,INT,STRING,INT,INT,FLOATV,INT,INT,PINT)

/*
 * remapmode
 * Given a string, describing the resample method return an integer
 *  mode to give as an argument to remapprep
 *
 *  Positive value indicates a AST standard resample method
 *  Negative value indicates a user-defined routine (AST__UINTERP +
 *   function name)
 *  Zero value indicates invalid mode string
 *
 *  {return value} (i) Integer interpolation mode
 *  methstr        (c) Interpolation method (NEAREST, LINEAR, SINC, SINCSINC, 
 *                     SINCCOS, SINCGAUSS, BLOCKAVE, CONSERVE)
 *  numparm        (i) Input number of interpolation parameters (0=set defaults)
 *  params         (d) Interpolation parameters (modified if numparm=0)
 */
int remapmode(char *methstr, int numparm, double *params) {

   char *buff;
   int i;
   buff = stralloc(methstr);
   for ( i = 0; i < strlen(methstr); i++ ) {
      methstr[i] = toupper(methstr[i]);
   }

/*
 *  Initialize parameters to zero if not set
 */
   if ( !numparm ) {
      params[0] = 0.;
      params[1] = 0.;
      params[2] = 0.;
      params[3] = 0.;
   }
   
   if ( strcmp(buff, "NEAREST") == 0 ) return AST__NEAREST;
   if ( strcmp(buff, "LINEAR") == 0 ) return AST__LINEAR;
   if ( strcmp(buff, "SINC") == 0 ) return AST__SINC;
   if ( strcmp(buff, "SINCSINC") == 0 ) {
      if ( !numparm ) {
         params[0] = 2;  /* Lanczos kernel */
         params[1] = 2;
	 numparm = 2;
      }
      return AST__SINCSINC;
   }
   if ( strcmp(buff, "SINCCOS") == 0 ) return AST__SINCCOS;
   if ( strcmp(buff, "SINCGAUSS") == 0 ) return AST__SINCGAUSS;
   if ( strcmp(buff, "BLOCKAVE") == 0 ) {
      if ( !numparm ) {
         cxwrite(" Defaulting to PARAMS=1 for BLOCKAVE method", 15);
         numparm = 1;
         params[0] = 1.0;
      }
      return AST__BLOCKAVE;
   }
   if ( strcmp(buff, "CONSERVE") == 0 ) return -1;
   cxwrite("Available remap interpolation modes: NEAREST, LINEAR, SINC, SINCSINC, SINCCOS, SINCGAUSS, BLOCKAVE, CONSERVE", 10);
   return 0;
}
FCALLSCFUN3(INT,remapmode,REMAPMODE,remapmode,STRING,INT,DOUBLEV)

