#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <float.h>

#include <xpa.h>

typedef enum {
   return_device_name = 1,
   return_dimensions = 2,
   return_resolution = 3,
   return_device_info = 4,
   return_default_file = 5,
   return_default_size = 6,
   return_defaults = 7,
   select_device = 8,
   open_workstation = 9,
   close_workstation = 10,
   begin_picture = 11,
   draw_line = 12,
   draw_dot = 13,
   end_picture = 14,
   select_color_index = 15,
   flush_buffer = 16,
   read_cursor = 17,
   erase_alpha_screen = 18,
   set_line_style = 19,
   polygon_fill = 20,
   set_color_representation = 21,
   set_line_width = 22,
   escape = 23,
   rectangle_fill = 24,
   set_fill_patt = 25,
   line_of_pixels = 26,
   scaling_info = 27,
   draw_marker = 28,
   query_color_representation = 29,
   scroll_rectangle = 30
} pgplot_operation ;

static char color[30];
static FILE *fp;

static char *availDashStyles[] = {
   "{ }", "10", "{15 10 4 10}", "{4 4}", "{15 10 4 10 4 10 4 10}"
};


typedef struct {
   int  resolution;
   int  linewidth;
   int  lineStyle;
   int  coloridx;
   int  red[256],green[256],blue[256];
   int  width;
   int  height;
   int  originx;
   int  originy;
   int  graphNum;
   int  freshSlate;
   float x0, y0, mx, my;
   char tag[40];
} DrawState;

typedef struct {
   int   connect;
   int   nPts;
   int   arraySize;
   float *x;
   float *y;
} Curve;

typedef struct {
   int   crvCnt;
   Curve dots;
   Curve lines;
} ComplexCurve;

typedef struct {
   int imgCnt;
   int pos;
   int  width, height;
   float clipRect[4];
   float matrix[6];
   unsigned char *data;
   unsigned char minVal;
   unsigned char maxVal;
} PGImage;

void appendLine2Crv ( ComplexCurve *crv, float pts[] );
void appendPt2Crv   ( Curve        *crv, float pts[] );
void scalePt        (                    float pts[], DrawState *state );
void startCurveObj  ( ComplexCurve *crv, DrawState *state );
void sendCurveObj   ( Curve        *crv, char *token, DrawState *state );
void startImageObj  ( PGImage      *img, float rbuf[],DrawState *state );
void sendImageObj   ( PGImage      *img, DrawState *state );

void fixScaleAndBounds( DrawState *state );
void formColorStr   ( char *clr, DrawState *state );

int  openPowConnection  ( void );
void closePowConnection ( void );
int  sendSetCmdToPow( char *cmd, char *buf, int buflen );
int  sendGetCmdToPow( char *cmd, char **rtnBuf, int *bufLen);
int  postProcessCall( int got, char **names, char **messages );

/*
 * Allow PWDRIV to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call pwdriv() or pwdriv_().
 */
#ifdef PG_PPU
#define PWDRIV pwdriv_
#else
#define PWDRIV pwdriv
#endif

void PWDRIV( int   *ifunc,
             float rbuf[],
             int   *nbuf,
             char  *chr,
             int   *lchr,
             int   len)
{
   static log = 0;
   static prevFunc = -1;
   static hasInited=0;
   int i, isNewFunc;
   char tmpStr;
   char colorStr[18];
   char powCmd[1024];
   char tclCmd[1024];

   static DrawState    theState;
   static ComplexCurve theCurve;
   static PGImage      theImage;

   int  rgbColors[5][3] = {
      {   0,   0,   0 },
      {   0,   0,   0 },
      { 255,   0,   0 },
      {   0, 255,   0 },
      {   0,   0, 255 }
   };

   /*  This is for logging long operations  */
   isNewFunc = ( *ifunc != prevFunc );
   prevFunc  = *ifunc;

   if( !hasInited ) { 
      hasInited = 1;
      theState.linewidth  =   1;
      theState.lineStyle  =   0;
      theState.coloridx   =   1;
      theState.resolution =  72;
      theState.width      =  -1;
      theState.height     =  -1;
      theState.originx    =   0;
      theState.originy    =   0;
      theState.graphNum   =   0;
      theState.x0         = 0.0;
      theState.y0         = 0.0;
      theState.mx         = 1.0;
      theState.my         = 1.0;
      for( i=0; i<5; i++ ) {
         theState.red[i]   = rgbColors[i][0];
         theState.green[i] = rgbColors[i][1];
         theState.blue[i]  = rgbColors[i][2];
      }
      for( i=5; i<256; i++ ) {  /* Create greyscale */
         theState.red[i]   = i;
         theState.green[i] = i;
         theState.blue[i]  = i;
      }
   }

   /*
    *    Branch on the specified PGPLOT opcode.
    */

   switch( (pgplot_operation)*ifunc ) {

   case return_device_name:
      strncpy( chr, "POW (a tk plotting device)", len );
      for(i=strlen(chr); i < len; i++)
         chr[i] = ' ';
      *lchr = strlen(chr);
      break;

   case return_dimensions:
      rbuf[0] =    0.0;
      rbuf[1] =   -1.0;  /*  Max plot width    */
      rbuf[2] =    0.0;
      rbuf[3] =   -1.0;  /*  Max plot height   */
      rbuf[4] =    0.0;
      rbuf[5] =  255.0;  /*  Number of colors-1*/
      *nbuf = 6;
      break;

   case return_resolution:
      rbuf[0] = theState.resolution;
      rbuf[1] = theState.resolution;
      rbuf[2] =  1.0;		/* Device coordinates per pixel */
      *nbuf = 3;
      break;

   case return_device_info:
      chr[0] = 'I'; /* Interactive or Hardware device */
      chr[1] = 'X'; /* Cursor is available */
      chr[2] = 'D'; /* No dashed lines */
      chr[3] = 'N'; /* Area fill available */
      chr[4] = 'T'; /* Thick lines */
      chr[5] = 'N'; /* Rectangle fill available */
      chr[6] = 'Q'; /* Line of pixels available */
      chr[7] = 'N'; /* Don't prompt on PGEND */
      chr[8] = 'Y'; /* Can return color representation */
      chr[9] = 'N'; /* Not used */
      chr[10]= 'N'; /* Area-scroll available */
      *lchr = 11;
      break;

   case return_default_file:
      chr[0] = '\0';  /* Default name is "" */
      *lchr = 0;
      break;

   case return_default_size:
      if( theState.width<0 ) {
         int bufLen;
         char *rtnVals;

         sendGetCmdToPow( "graph -name default xdimdisp", &rtnVals, &bufLen );
         theState.width  = atoi( rtnVals );
         sendGetCmdToPow( "graph -name default ydimdisp", &rtnVals, &bufLen );
         theState.height = atoi( rtnVals );
      }
      rbuf[0] =   0.0;
      rbuf[1] = theState.width;
      rbuf[2] =   0.0;
      rbuf[3] = theState.height;
      *nbuf = 4;
      break;

   case return_defaults:
      rbuf[0] = 1.0;  /* Scale factor for obsolete fonts */
      *nbuf = 1;
      break;

   case draw_line:
      if( theState.freshSlate ) {
         fixScaleAndBounds( &theState );
      }

      scalePt( rbuf+0, &theState );
      scalePt( rbuf+2, &theState );
      appendLine2Crv( &theCurve, rbuf );
      break;

   case draw_dot:
      if( theState.freshSlate ) {
         fixScaleAndBounds( &theState );
      }

      scalePt( rbuf+0, &theState );
      appendPt2Crv( &theCurve.dots, rbuf );
      break;

   case scaling_info:
      if( theState.freshSlate ) {
         /*  Only change scaling if we haven't drawn something already  */
         theState.x0 = rbuf[0];
         theState.mx = rbuf[1];
         theState.y0 = rbuf[2];
         theState.my = rbuf[3];
      }
      break;

   case rectangle_fill:
      break;

   case select_device:
      theState.graphNum = rbuf[1];
      break;

   case open_workstation:
      rbuf[0] = 1.0; /* Number used to select this device */
      rbuf[1] = 1.0; /* Error flag: 1.0=success           */
      *nbuf = 2;

      if( openPowConnection() ) {
         rbuf[1] = 0.0; /* Error */
         return;
      }
      break;

   case close_workstation:
      sendSetCmdToPow(NULL,NULL,-2);
      closePowConnection();
      break;

   case begin_picture:
      /*  Start a new graph of given width/height */
      theState.width  = rbuf[0];
      theState.height = rbuf[1];
      theState.freshSlate = 1;
      /* theState.graphNum++; */

      sprintf( powCmd, "graph -name pgPlotGraph%d GridLines No "
               "xLabelTicks {No No No No} yLabelTicks {No No No No} "
               "xTickLength {0 0 0 0} yTickLength {0 0 0 0} "
               "xlabel {} ylabel {} titleString {}",
               theState.graphNum );
      sendSetCmdToPow( powCmd, NULL, 0 );

      sprintf( powCmd, "create graph pgPlotGraph%d NULL NULL %d %d",
               theState.graphNum, theState.width, theState.height );
      sendSetCmdToPow( powCmd, NULL, 0 );
      sendSetCmdToPow( "scope 0", NULL, 0 );

      startCurveObj( &theCurve, &theState );
      break;

   case end_picture:
      /* Pan Chai - reset all necessary parameters */
      theState.width      =  -1;
      theState.height     =  -1;
      theState.freshSlate = 1;
      theState.x0         = 0.0;
      theState.y0         = 0.0;
      theState.mx         = 1.0;
      theState.my         = 1.0;
      break;

   case flush_buffer:
      startCurveObj( &theCurve, &theState );
      /*  Images are handled en masse, so one is never waiting to be flushed  */
      sendSetCmdToPow(NULL,NULL,-1);
      break;

   case erase_alpha_screen:
      /* Pan Chai - for some reason, this statement is needed for ximage to come back */
      fflush(stdout);
      break;

   case select_color_index:
      startCurveObj( &theCurve, &theState );
      if( rbuf[0]>0.5 ) {
         /*  Selection of background/erase color not yet allowed  */
         theState.coloridx = (int)(rbuf[0] + 0.5);
      }
      break;

   case query_color_representation:
      i = (int)(rbuf[0]+0.5);
      rbuf[1] = theState.red  [ i ] / 255.0;
      rbuf[2] = theState.green[ i ] / 255.0;
      rbuf[3] = theState.blue [ i ] / 255.0;
      break;

   case set_color_representation:
      i = (int)(rbuf[0]+0.5);
      theState.red  [ i ] = (int)(rbuf[1]*255.9);
      theState.green[ i ] = (int)(rbuf[2]*255.9);
      theState.blue [ i ] = (int)(rbuf[3]*255.9);
      break;

   case set_line_width:
      /*
       * The line width is provided in multiples of 0.005 inches.
       */
      startCurveObj( &theCurve, &theState );
      theState.linewidth = (int)(rbuf[0]*0.005 * theState.resolution + 0.5);
      if( theState.linewidth < 1 ) theState.linewidth = 1;
      break;

   case set_line_style:
      if( theState.lineStyle != (int)(rbuf[0]-0.5) ) {
         startCurveObj( &theCurve, &theState );
         theState.lineStyle = (int)(rbuf[0]-0.5);
      }
      break;

   case line_of_pixels:

      if( theState.freshSlate ) {
        
         if ( rbuf[0] == 0.0 ) 
         {
            /* Pan Chai - reset if a new image arrived */
            theState.freshSlate = 0;
            theState.x0         = 0.0;
            theState.y0         = 0.0;
            theState.mx         = 1.0;
            theState.my         = 1.0;
         }

         fixScaleAndBounds( &theState );
      }

      if( rbuf[0] == 0.0 ) {
         /*  New Image  */
         startImageObj( &theImage, rbuf, &theState );

      } else if ( rbuf[0] > 0.0 ) {
         /*  More Data  */
         unsigned char cVal;
         int nPix = (int)(rbuf[0]+0.5);
         for( i=0; i<nPix; ) {
            cVal = (unsigned char)(rbuf[++i]+0.5);
            theImage.data[ theImage.pos++ ] = cVal;
            if( cVal > theImage.maxVal ) theImage.maxVal = cVal;
            if( cVal < theImage.minVal ) theImage.minVal = cVal;
         }
      } else {
         /*  Done  */
         sendImageObj( &theImage, &theState );
      }

      break;

   case read_cursor:
      if( 1 ) {
         int bufLen;
         char *rtnVals, *endPtr;

         sendGetCmdToPow( "cursor", &rtnVals, &bufLen );
         rbuf[0] = strtod( rtnVals, &endPtr );
         rbuf[1] = strtod( endPtr, &endPtr  );
         i       = strtol( endPtr, &endPtr, 10 );
         if( i < 0 ) {
           /*  Key pressed  */
           chr[0] = -i;
         } else {
           /*  Button pressed... map to a letter  */
           chr[0]  = ( i==1 ? 'A' : (i==2 ? 'D' : 'X') );
         }
         free( rtnVals );

         /*  Unscale graph values returned by POW  */
         rbuf[0] = ( rbuf[0] * theState.mx ) + theState.x0;
         rbuf[1] = ( rbuf[1] * theState.my ) + theState.y0;
      }
      break;

   case polygon_fill:
   case scroll_rectangle:
   case draw_marker:
   case escape:           /* PGPLOT: Avoid use     */
   case set_fill_patt:    /* PGPLOT: Unimplemented */
   default:
      fprintf(stderr, "/POW: Unexpected opcode=%d in driver.\n", *ifunc);
      *nbuf = -1;
      break;
   };
   return;
}


void formColorStr( char *clr, DrawState *state )
{
   int i;

   i = state->coloridx;
   sprintf(clr,"\"#%02X%02X%02X\"",
           state->red[i], state->green[i], state->blue[i]);
}


/************************************************************************
 *
 *    Manage Curve Objects
 *
 *      typedef struct {
 *           int   connect;
 *           int   nPts;
 *           int   arraySize;
 *           float *x;
 *           float *y;
 *      } Curve;
 *      typdef struct {
 *           int   crvCnt;
 *           Curve dots;
 *           Curve lines;
 *      } ComplexCurve;
 *
 ************************************************************************/

#define ARRAY_STEP 1024

void startCurveObj  ( ComplexCurve *crvs, DrawState *state )
{
   static int initFlag = 1;
   char token[16];

   if( initFlag ) {
      crvs->crvCnt         = 0;
      crvs->dots.connect   = 0;
      crvs->lines.connect  = 1;
      crvs->dots.nPts      = crvs->lines.nPts      = 0;
      crvs->dots.arraySize = crvs->lines.arraySize = ARRAY_STEP;
      crvs->dots.x         = (float*) malloc( ARRAY_STEP * sizeof(float) );
      crvs->dots.y         = (float*) malloc( ARRAY_STEP * sizeof(float) );
      crvs->lines.x        = (float*) malloc( ARRAY_STEP * sizeof(float) );
      crvs->lines.y        = (float*) malloc( ARRAY_STEP * sizeof(float) );
      initFlag       = 0;
   } else if( crvs->dots.nPts || crvs->lines.nPts ) {
      if( crvs->dots.nPts ) {
         sprintf(token,"%d_pnts", crvs->crvCnt);
         sendCurveObj( &(crvs->dots), token, state );
      }
      if( crvs->lines.nPts ) {
         sprintf(token,"%d_line", crvs->crvCnt);
         sendCurveObj( &(crvs->lines), token, state );
      }
      crvs->crvCnt++;
   }
}

void scalePt( float pts[], DrawState *state )
{
   pts[0] = ( pts[0] - state->x0 ) / state->mx;
   pts[1] = ( pts[1] - state->y0 ) / state->my;
}


void appendLine2Crv ( ComplexCurve *crvs, float pts[] )
{
   float *lastX, *lastY;
   float nullPt[2];

   if( pts[0]==pts[2] && pts[1]==pts[3] ) {
      /*
       *  For some reason, PGPLOT is passing the coordinates of data
       *  points twice.  So, check that this point is indeed different
       *  from the preceding one... if it exists.
       */
      if( crvs->dots.nPts ) {
         lastX = crvs->dots.x + crvs->dots.nPts - 1;
         lastY = crvs->dots.y + crvs->dots.nPts - 1;
         if( *lastX != pts[0] || *lastY != pts[1] ) {
            appendPt2Crv( &(crvs->dots), pts );
         }
      } else {
         appendPt2Crv( &(crvs->dots), pts );
      }
   } else {
      if( crvs->lines.nPts ) {
         /*  Check if this segment is detached from previous point  */
         lastX = crvs->lines.x + crvs->lines.nPts - 1;
         lastY = crvs->lines.y + crvs->lines.nPts - 1;
         if( *lastX != pts[0] || *lastY != pts[1] ) {
            nullPt[0] = nullPt[1] = FLT_MAX;
            appendPt2Crv( &(crvs->lines), nullPt );
            appendPt2Crv( &(crvs->lines), pts );
         }
      } else {
         appendPt2Crv( &(crvs->lines), pts );
      }
      appendPt2Crv( &(crvs->lines), pts+2 );
   }
}

void appendPt2Crv   ( Curve *aCrv, float pts[] )
{
   int idx;
   
   if( aCrv->nPts >= aCrv->arraySize ) {
      aCrv->arraySize += ARRAY_STEP;
      aCrv->x = (float*) realloc( aCrv->x, aCrv->arraySize * sizeof(float) );
      aCrv->y = (float*) realloc( aCrv->y, aCrv->arraySize * sizeof(float) );
   }
   idx = aCrv->nPts++;
   aCrv->x[ idx ] = pts[0];
   aCrv->y[ idx ] = pts[1];
}

void sendCurveObj   ( Curve *crv, char *token, DrawState *state )
{
   char tmpStr[80];
   char powCmd[255];
   int pntFlag = 0;

   /*   Identify endianness of machine... POW may need to swap bytes   */
#if defined(__alpha) || defined(__i386) || defined(__i386__) || defined(__i486__) || defined(__i586__)
   char *endian = "littleEndian";
#else                                                                           
   char *endian = "bigEndian";
#endif

   if (strstr(token, "_pnts") != NULL)
   {
      pntFlag = 1;   
   }
   
   if( !crv->nPts ) return;

   /* Send X data */

   sprintf( powCmd, "array pgCurveXData%s -32 %s", token, endian );
   sendSetCmdToPow( powCmd, (char*)crv->x, (crv->nPts * sizeof(float)) );


   /* Send Y data */

   sprintf( powCmd, "array pgCurveYData%s -32 %s", token, endian );
   sendSetCmdToPow( powCmd, (char*)crv->y, (crv->nPts * sizeof(float)) );


   /* Build X Y curve and add it to graph */

   sprintf( powCmd, "create curve pgCurve%s pgCurveXData%s pgCurveYData%s",
            token, token, token );
   sendSetCmdToPow( powCmd, NULL, 0 );
   sprintf( powCmd, "add curve pgCurve%s", token );
   sendSetCmdToPow( powCmd, NULL, 0 );
   sprintf( powCmd, "select curve pgCurve%s", token );
   sendSetCmdToPow( powCmd, NULL, 0 );
   

   /* Now set the curve's options */

   formColorStr( tmpStr, state );
   sprintf( powCmd,
            "curve pDisp %s pColor %s lDisp %s lWidth %d lStyle %s lColor %s",
            /* pDisp  */  (pntFlag == 1 ? "No" : (crv->connect ? "No" : "Yes")), 
            /* pColor */  tmpStr,
            /* lDisp  */  (crv->connect ? "Yes" : "No"),
            /* lWidth */  state->linewidth,
            /* lStyle */  availDashStyles[state->lineStyle],
            /* lColor */  tmpStr );
   sendSetCmdToPow( powCmd, NULL, 0 );


   /*  Flush commands to POW  */

   sendSetCmdToPow( NULL, NULL, -1 );
   crv->nPts = 0;
}

void startImageObj ( PGImage *img, float rbuf[], DrawState *state )
{
   static int initFlag = 1;
   char token[16];
   int i;
   float norm;

   if( initFlag ) {
      img->imgCnt = 0;
      initFlag    = 0;
   } else {
      img->imgCnt++;
   }

   img->width  = (int)(rbuf[1]+0.5);
   img->height = (int)(rbuf[2]+0.5);

   for( i=0; i<4; i++ ) {
      img->clipRect[i] = rbuf[3+i];
      /*  printf("%8.3f  ", rbuf[3+i]);  */
   }

   /* Pan Chai - for readability in ximage windows */
   printf("\n");

   for( i=0; i<6; i++ ) {
      img->matrix[i] = rbuf[7+i];
      /*  printf("%8.3f  ", rbuf[7+i]);  */
   }
   /*  Invert the matrix  */
   norm = rbuf[7+0]*rbuf[7+3] - rbuf[7+1]*rbuf[7+2];
   img->matrix[0] =   rbuf[7+3] / norm;
   img->matrix[1] = - rbuf[7+1] / norm;
   img->matrix[2] = - rbuf[7+2] / norm;
   img->matrix[3] =   rbuf[7+0] / norm;

   scalePt( rbuf+11, state );
   img->matrix[4] = -rbuf[7+4]*img->matrix[0];
   img->matrix[5] = -rbuf[7+5]*img->matrix[3];

   /* Pan Chai - for readability in ximage windows */
   printf("\n");

   img->data = (unsigned char *)malloc( img->width * img->height );
   img->pos  = 0;
   img->minVal = 255;
   img->maxVal =   0;
}


void sendImageObj   ( PGImage *img, DrawState *state )
{
   char token[80];
   char powCmd[255];
   char *cmapStr, *cmapPtr;
   int i;

   sprintf( token, "%d", img->imgCnt );

   /* Send pix data */

   sprintf( powCmd, "array pgImageData%s 8", token );
   sendSetCmdToPow( powCmd, (char*)img->data, (img->height * img->width) );

   /* Build image and add it to graph */

   sprintf( powCmd, "create image pgImage%s pgImageData%s %d %d",
            token, token, img->width, img->height );
   sendSetCmdToPow( powCmd, NULL, 0 );

   sprintf( powCmd,
            "wcs pgImage%s {{%f %f} {0.5 0.5} {%f %f %f %f} {X Y} none}",
            token,
            img->matrix[4], img->matrix[5],  /*  RefVal  */
            img->matrix[0], img->matrix[1],  /*  Matrix  */
            img->matrix[2], img->matrix[3]   /*  Matrix  */
            );
   sendSetCmdToPow( powCmd, NULL, 0 );

   sprintf( powCmd, "add image pgImage%s", token );
   sendSetCmdToPow( powCmd, NULL, 0 );

   sprintf( powCmd, "select image pgImage%s", token );
   sendSetCmdToPow( powCmd, NULL, 0 );

   /*  Allocate enough space to hold all the RGB colors  */
   cmapStr = malloc( 6 * 256 * 3 + 50 );
   cmapPtr = cmapStr;
   
   sprintf( cmapPtr, "colormap add pgCmap%s", token );
   cmapPtr += strlen(cmapPtr);
   for( i=img->minVal; i<=img->maxVal; i++ ) {
      sprintf( cmapPtr, " %3d %3d %3d",
               state->red[i], state->green[i], state->blue[i] );
      cmapPtr += strlen(cmapPtr);
   }
   sendSetCmdToPow( cmapStr, NULL, 0 );
   free( cmapStr );

   sprintf( powCmd, "colormap -current scale linear %d %d",
            img->minVal, img->maxVal );
   sendSetCmdToPow( powCmd, NULL, 0 );
   sprintf( powCmd, "colormap -current pgCmap%s", token );
   sendSetCmdToPow( powCmd, NULL, 0 );

   sprintf( powCmd, "graph FixedAspect Yes" );
   sendSetCmdToPow( powCmd, NULL, 0 );

   /*  Flush commands to POW  */

   sendSetCmdToPow( NULL, NULL, -1 );
   free( img->data );
}

void fixScaleAndBounds( DrawState *state )
{
   /*  First data drawn... freeze the scale and set graph bounds  */

   char powCmd[255];
   float bounds[4];

   bounds[0] = 0.0;
   bounds[1] = 0.0;
   bounds[2] = state->width;
   bounds[3] = state->height;
   scalePt( bounds+0, state );
   scalePt( bounds+2, state );

   sprintf( powCmd, "bounds %f %f %f %f",
            bounds[0], bounds[1], bounds[2], bounds[3] );
   sendSetCmdToPow( powCmd, NULL, 0 );
   state->freshSlate = 0;
}

/************************************************************************
 *
 *   POW C socket/XPA driver
 *
 ***********************************************************************/

#define NXPA 1

static XPA   powConnection = NULL;
static char *powAddress    = NULL;

int openPowConnection()
{
   extern XPA powConnection;
   char *rtnBuf;
   int bufLen;

   if( !powAddress ) {
      powAddress = getenv( "POW_DISPLAY" );
      if( !powAddress || strlen(powAddress)==0 ) {
         /*  POW_DISPLAY not defined, must depend on name server  */
         powAddress = "pow";
      }
   }

   powConnection = XPAOpen( NULL );
   if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
      system("POWplot &");
      sleep(3);
      if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
         sleep(3);
         if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
            return 1;
         }
      }
   }
   free( rtnBuf );
   return 0;
}

void closePowConnection()
{
   extern XPA powConnection;

   XPAClose( powConnection );
   powConnection = NULL;
}

int sendGetCmdToPow( char *cmd, char **rtnBuf, int *bufLen)
{
   int  got, stat=0;
   char *bufs[NXPA];
   char *names[NXPA];
   char *messages[NXPA];
   int  lens[NXPA];
   extern XPA powConnection;
   extern char *powAddress;
   
   /*  Flush 'sendSet' command cache  */
   stat = sendSetCmdToPow( NULL, NULL, -1 );
   if( !stat ) {
      got = XPAGet(powConnection, powAddress, cmd, "",
                   bufs, lens, names, messages, NXPA);
      if( got==0 )
         stat = -1;
      else
         stat = postProcessCall(got, names, messages);
      *rtnBuf = bufs[0];
      *bufLen = lens[0];
   }
   return stat;
}

int sendSetCmdToPow( char *cmd, char *buf, int buflen )
{
   int  got, len, stat=0;
   char *names[NXPA];
   char *messages[NXPA];
   extern XPA  powConnection;
   extern char *powAddress;

   static char *cache     = NULL;
   static int   cachePos  = 0;
   static int   cacheSize = 0;

   if( buflen==0 ) {
      /*  Just cache command  */
      len = strlen(cmd);
      if( cachePos + len + 2 > cacheSize ) {
         cacheSize += 4096 + len;
         if( cache ) {
            cache = (char*) realloc( cache, sizeof(char) * cacheSize );
         } else {
            cache = (char*) malloc( sizeof(char) * cacheSize );
         }
      }
      cache[cachePos++] = '\n';
      strcpy( cache+cachePos, cmd );
      cachePos += len;
      if( cachePos>60000 ) {
         /*  Flush the cache  */
         stat = sendSetCmdToPow( NULL, NULL, -1 );
      }         
   } else if ( buflen==-1 ) {
      /*  Flush the cache  */
      if( cachePos ) {
         /*  printf("Flushing cache at %d bytes\n", cachePos);  */
         len      = cachePos;
         cachePos = 0;
         stat = sendSetCmdToPow( "tcl", cache, len );
      }
   } else if ( buflen==-2 ) {
      /*  Free the cache  */
      free(cache);
      cache = NULL;
      cachePos = cacheSize = 0;
   } else {
      if( cachePos ) {
         /*  Flush cache first  */
         stat = sendSetCmdToPow( NULL, NULL, -1 );
      }
      if( !stat ) {
         got = XPASet(powConnection, powAddress, cmd, "", buf, buflen,
                      names, messages, NXPA);
         if( got==0 )
            stat = -1;
         else
            stat = postProcessCall(got, names, messages);
      }
   }
   return stat;
}

int postProcessCall( int got, char **names, char **messages )
{
   int i, status=0;

   for(i=0; i<got; i++){
      if( messages[i] != NULL ) {
         /* error processing */
         fprintf(stderr, "ERROR: %s (%s)\n", messages[i], names[i]);
         status = 1;
      }
      if( names[i]    ) free(names[i]);
      if( messages[i] ) free(messages[i]);
   }
   return status;
}
