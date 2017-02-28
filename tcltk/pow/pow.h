#define POW

#ifndef _POW_H
#define _POW_H

#include "tkpict.h"
#include "tk.h"
#include <wcslib/wcs.h>
#include <wcslib/wcshdr.h>
#include <wcslib/wcsfix.h>

#define BYTE_DATA 0 /* unsigned char */
#define SHORTINT_DATA 1
#define INT_DATA 2
#define REAL_DATA 3
#define DOUBLE_DATA 4  
#define STRING_DATA 5
#define LONGLONG_DATA 6

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif

/*  Sun4s do not support %p, so switch to %lx  */

#ifdef HEX_PTRFORMAT
#define PTRFORMAT "%lx"
#else
#define PTRFORMAT "%p"
#endif

extern int pixelSizes[6];

extern char *WCSpih_Message[];
extern char *WCStrans_Message[];

/* Typedef for a PowData structure.  This is the main way of getting
   data into TCL */

typedef struct PowData {
  char *data_name;  /* The identifier for this data known to TCL and the
                       calling program.  Also the hash key.                 */
  void *data_array; /* The array full of data.                              */
  int  data_type;  /* The actual type of the data Byte-0,2 Bytes-1,4 Bytes-2,
		      4 Bytes Real- 3, 8 Bytes Real- 4 (not fully 
		      supported for images), String - 5 (currently supported
                      as the "z-vector" for a curve only.   */
  int copy;    /*if non-zero, indicates that the data pointer "belongs" to
                 POW (i.e. the data was copied at creation time) and may
                 thus be 'ckfree'd */
  int length;       /* The number of elements in the array.                 */
}  PowData;


#define MAX_WCS_DIMS 2
typedef struct WCSdata {
   char graphName[1024];
   char curveName[1024];
   char type[6];

   int RaDecSwap;
   int nAxis;
   double refVal[MAX_WCS_DIMS],
          refPix[MAX_WCS_DIMS],
          cdFrwd[MAX_WCS_DIMS][MAX_WCS_DIMS],
          cdRvrs[MAX_WCS_DIMS][MAX_WCS_DIMS],
          rot;

   int haveWCSinfo;
   struct wcsprm *wcs;
}  WCSdata;
   

typedef struct PowImage {  /* this associates "physics" with a 2d data array */
  char *image_name;  /* The identifier for this image known to TCL and the
			calling program.  Also the hash key. Also the image name
			for VISU/pict   */
  PowData *dataptr; /* The data array you want for this image */
  /*  Tk_PictHandle *pict_handle; The pict image handle */
  void *image_handle; /* This way we can toss around Photo *or* Pict images */
  int xoffset;
  int yoffset; /* The number of pixels in width and height you want to count
                  before the image actually starts */
  int width;  /* The width of the image in pixels */
  int height; /* The height of the image in pixels */
  double xorigin;
  double xinc;
  double xotherend; /*The real coordinates of the upper right pixel.  
                      Primarily for WCS usage, but handy to have in general */
  double yorigin;
  double yinc;  /* The origin values (at first used pixel) for x and y and the increment per pixel  */ 
  double yotherend;
  char *xunits; 
  char *yunits; /* Units strings for the min, and inc values */
  char *zunits;
  WCSdata WCS;
}  PowImage;


typedef struct PowVector { /*This associates "physics" with a 1d data array*/
  char *vector_name;  /* The identifier for this vector known to TCL and the
			 calling program.  Also the hash key.  */
  PowData *dataptr; /* The data array for the vector */
  int offset; /* The number of data you want to skip before the vector actually starts */
  int length; /* The length of the vector (number of elements) */
  char *units; /* A units string */
}  PowVector;

typedef struct PowCurve {
  char *curve_name; /* The identifier for this vector known to TCL and the
		       calling program.  Also the hash key.  */
  int length;  /* number of elements in the curve:  == length of first 
                  non-null vector (in sequence x, y, z) */
  PowVector *x_vector; 
  PowVector *x_error; /* This  may be NULL */
  PowVector *y_vector;
  PowVector *y_error; /* This  may be NULL */
  PowVector *z_vector; /* This  may be NULL */
  PowVector *z_error; /* This  may be NULL */
  WCSdata   WCS;
} PowCurve;



typedef struct PowGraph {
  char *graph_name;
  double xleft;
  double xright;
  double ybot;
  double ytop;

  double xmagstep;  /* This is the displayed size factor wrt the delta X */
  double ymagstep;  /* and delta Y of the original PowCreateGraph call   */
  
  double xoff;
  double yoff;

  char *xunits;
  char *yunits;
  char *xlabel;
  char *ylabel;
  WCSdata WCS;      /* The master WCS data for graph */
} PowGraph;

  
extern int Private_Colormap;
extern int slice_nb;
extern int nb_slices;
extern int Pow_Done;
extern int tty;

/* 12-23-03 BD change to permit pow use in C++ programs on linux. <unistd.h> defines
   isatty with an exception specification, and this definition causes compilation
   failure in any file that includes <unistd.h> as well as pow.h */
#ifndef __cplusplus
        extern int isatty _ANSI_ARGS_((int fd));
#else
        #include <unistd.h>
#endif



extern Tcl_Interp *interp;		/* Interpreter for application. */
extern Tk_Window mainWindow;	/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */


extern Tcl_HashTable PowDataTable;
extern Tcl_HashTable PowImageTable;
extern Tcl_HashTable PowVectorTable;
extern Tcl_HashTable PowCurveTable;
extern Tcl_HashTable PowGraphTable;


void PowInit(char *, char *, int *); /*call this from a main program before doing anything */
int Pow_Init(Tcl_Interp *); /*call this from a tkAppInit before doing anything */
int Pow_InitExec(Tcl_Interp *); 
int Pow_CreateCommands(Tcl_Interp *); 
int Pow_CreateCommands(Tcl_Interp *); 
void PowCreateData(char *, void *, int *, int *, int *, int *);
void PowRegisterData(PowData *,int *);
void PowDestroyData(char *, int *);

void Pow_PhotoPutScaledBlock(Tk_PhotoHandle, register Tk_PhotoImageBlock *, int, int, int, int, 
			     double, double, double, double);

void PowCreateImage(char *, char *, int *, int *, int *, int *, 
		    double *, double *, double *, double *,
		    char *, char *, char *, int *);
void PowDestroyImage(char *image_name, int *status);

void PowCreateVector(char *, char *, int *, int *, char *, int *);
void PowCreateVectorEN(char *, char *, int *, double *, 
			     double *, char *, int *);
void PowDestroyVector(char *vector_name, int *status);

void PowCreateCurve(char *, char *, char *, char *, char *, char *, 
		    char *, int *); 
void PowDestroyCurve(char *curve_name, int *status);

void PowCreateHisto(char *, char *, char *, int *);

void PowCreateGraph(char *, char *, char *, char *, char *, char *, 
		    char *, int *, int *, double *, double *, double *,
                    double *, int *);
void PowCreateGraph_internal(char *, char *, char *, char *, char *, char *, 
		    char *, int *, int *, double *, double *, double *,
                    double *, char *, int *);
void PowDestroyGraph(char *graph_name, int *status);

void PowCreateDataFlip(char *, char *, int *, int *, int *);
int PowCreateDataFlip_Tcl(ClientData, Tcl_Interp *, int, char **);

void PowCreateCurveFlip(char *, char *, int *);
int PowCreateCurveFlip_Tcl(ClientData, Tcl_Interp *, int, char **);

void PowInitWCS( WCSdata *WCS, int n );
int FillinWCSStructure (WCSdata *WCS);
void PowDumpWCSstructure ( WCSdata *WCS );
int  PowParseWCS( Tcl_Interp *interp, WCSdata *WCS, int argc, Tcl_Obj *const argv[] );
int  PowWCSInitGraph(PowGraph *, char *, char *, int, int);
void PowDitherToPhoto(PowImage *,Tk_PhotoImageBlock *, double, double);
void PowHandleEvents();
void PowWishHandleEvents();
void Pow_PhotoPutScaledBlock _ANSI_ARGS_((
         Tk_PhotoHandle handle, Tk_PhotoImageBlock * blockPtr,
         int x, int y, int width, int height,
         double zoomX, double zoomY, double Xoff, double Yoff));

int PowFindCurvesMinMax(const char *, char *, double *, double *, int);
int PowFindCurvesValue (char *, char *, int, double *); 
int PowFindCurvesBBox(char *, char *, double *, double *,
		      double *, double *, WCSdata *);
int PowFindImagesBBox(char *,double *,double *,double *,double *, WCSdata *);
int PowFindGraphBBox (PowGraph *, char *, char *, double *, double *,
                      double *, double *);
int PowPosToPix(double, double, WCSdata *, double *, double *);
int PowPixToPos(double, double, WCSdata *, double *, double *);
int PowSortGraphMinMax(PowGraph *, double *, double *,double *, double *,
		       double *, double *);

void powDebugDataPrint (char *, int, WCSdata *, int, char *);

const char *PowGetObjectOption(char *gn, const char *obj, char *option, char *objType);

#ifdef __WIN32__
  __int64 PowExtractDatumLong(PowData *, int);
#else
  long long PowExtractDatumLong(PowData *, int);
#endif

double PowExtractDatum(PowData *, int);
int PowPutDatum(PowData *,double,int);
PowCurve * PowFindCurve(const char *);
PowVector * PowFindVector(char *);
PowImage * PowFindImage(const char *);
PowGraph * PowFindGraph(char *);
PowData * PowFindData(char *);
int PowIsInRegion( double* , double *, int , char *, int* );
int PowCalRegion( PowImage* , char *, int *, double *, int , char *,
                  char*,  double* , double*, double*, double *, double*, 
		  double*, int* );


/* New Tcl Commands */

int PowGetRegionMean ( ClientData, Tcl_Interp, int, char **);
int PowWCSInitImage(ClientData , Tcl_Interp *, int , Tcl_Obj *const []);
int PowWCSInitCurve(ClientData , Tcl_Interp *, int , Tcl_Obj *const []);
int PowWCSexists(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowWCSisSwapped(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowGetImageOrigin(ClientData , Tcl_Interp *, int , char **);
int PowGetImageOtherend(ClientData , Tcl_Interp *, int , char **);
int PowGetImageUnits(ClientData , Tcl_Interp *, int , char **);
int PowCreateData_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCloneData(ClientData, Tcl_Interp *, int, char **);
int PowFindData_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowRegisterData_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyData_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyImage_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyVector_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyCurve_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyGraph_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowDestroyImage_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateDataFromBuffer(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateDataFromChannel(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateDataFromPtr(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateStrFromPtr(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateDataFromList(ClientData, Tcl_Interp *, int, char **);
int PowCreateCurve_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateHisto_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateVector_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateVectorEN_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateImage_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowCreateGraph_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowFindCurvesMinMax_Tcl(ClientData, Tcl_Interp *, int, char **);
int PowFetchDataLength(ClientData, Tcl_Interp *, int, char **);
int PowExprDataInfo(ClientData, Tcl_Interp *, int, Tcl_Obj *const [] );
int PowDataPtr_Tcl(ClientData, Tcl_Interp *, int, Tcl_Obj *const [] );
int PowFetchCurveInfoHash(ClientData, Tcl_Interp *, int, char **);
int PowFetchVectorInfoHash(ClientData, Tcl_Interp *, int, char **);
int PowFetchImageInfoHash(ClientData, Tcl_Interp *, int, char **);
int PowSetGraphMagstep(ClientData, Tcl_Interp *, int, char **);
int PowProcessCurve(ClientData, Tcl_Interp *, int, char **);
int PowListGraphs(ClientData, Tcl_Interp *, int, char **);
int PowListCurves(ClientData, Tcl_Interp *, int, char **);
int PowListImages(ClientData, Tcl_Interp *, int, char **);
int PowListVectors(ClientData, Tcl_Interp *, int, char **);
int PowListData(ClientData, Tcl_Interp *, int, char **);
int PowCleanUp(ClientData, Tcl_Interp *, int, char **);
int PowSetupColormap(ClientData, Tcl_Interp *, int, char **);
int PowSetupPhotoImages(ClientData, Tcl_Interp *, int, char **);
int PowTestColormap(ClientData, Tcl_Interp *, int, char **);
int PowPhotoCmapStretch(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowPhotoColorTable(ClientData, Tcl_Interp *, int, char **);
int PowTestMacMemory(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowPutZoomedBlock(ClientData, Tcl_Interp *, int, char **);
int PowReditherPhotoBlock(ClientData, Tcl_Interp *, int, char **);
int PowImageScale(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowGetHisto(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowGetImageZ(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowWorldPos(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowXYPx(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);

int PowGraphToCanvas(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCanvasToGraph(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowGraphToPixel(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowPixelToGraph(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowResetWcsStructure(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);

int PowGraphVToPixelV(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowPixelVToGraphV(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowGetImageClipbox(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowDrawGridLines(ClientData, Tcl_Interp *, int, char **);
int PowCreateContour(ClientData, Tcl_Interp *, int, char **);
int PowGetTics(ClientData, Tcl_Interp *, int, char **);
int PowTestImage(ClientData, Tcl_Interp *, int, Tcl_Obj *const[]);
int PowGetRegionStatistics( ClientData, Tcl_Interp *, int , char ** );

int pow_worldpos(double, double,
                 double [], double [], double [][MAX_WCS_DIMS], char *,
                 double *, double *);
int pow_xypx(double, double,
             double [], double [],
             double [][MAX_WCS_DIMS], double [][MAX_WCS_DIMS], char *,
             double *, double *);





/*see the sample tkAppInit.c in the POW source directory for an
  example of how to set up a user function (readpha) to allow passing data
  objects into POW from C */






/*
 * The structure below defines the record for each powcurve item.
 */


typedef struct PowCurveItem  {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Outline lOutline;	/* Outline structure for lines  */
    Tk_Outline pOutline;	/* Outline structure for points */
    Tk_Canvas canvas;		/* Canvas containing item.  Needed for
				 * parsing arrow shapes. */
    PowCurve *curveObjectPtr;   /* Pointer to the PowCurve object that this
				 * item instantiates. */
    PowGraph *graphObjectPtr;   /* Pointer to the PowCurve object that this
				 * item instantiates. */

    double *pCoordPtr;		/* Pointer to malloc-ed array containing
				 * x- and y- canvas coords of all points
                                 * and errorbars in curve.
				 * X-coords are even-valued indices, y-coords
				 * are corresponding odd-valued indices. */
    int numPoints;             /* The number of points in the actual set of
                                 * XDrawn lines 
                                 * == (length of pCoordPtr array)/2 */

    double *lCoordPtr;		/* Same but tracing out line */
    int numLines;               /* == (length of lCoordPtr array)/2 */

    char *pointType;		/* "Cross", shape to draw at point. */
    int   pointError;           /* Draw point the size of errorbars? */
    int   pointSize;		/* Size of point (absent error bars) in pxls. */
    int   pointDisp;            /* Whether to display points or not */
    int   pointFill;            /* Whether to fill points or draw outlines */
    int   boxFill;              /* Whether to fill histogram box outlines */
    int   lineDisp;             /* Whether to display lines or not  */
    int   stairStep;            /* Draw lines in stairstep fashion? */
    int   logX;                 /* Take log of X data? */
    int   logY;                 /* Take log of Y data? */
    int   LOD;                 /* Maximum # of points plotted when  */
                                /* Level Of Detail averaging is enabled */
                                /* Use 0 (default) to disable LOD and plot*/
                                /* all points */
    int curveToPoint;		/* flag to disable point to curve processing */
    int capStyle;		/* Cap style for powCurve. */
    int joinStyle;		/* Join style for powCurve. */
    int hidden;                 /* Hide curve... don't draw */
} PowCurveItem;


/* Definitions */

extern Tk_ItemType tkPowCurveType;


/*
 * Prototypes for powCanvCurve procedures defined in this file:
 */

 void		ComputePowCurveBbox _ANSI_ARGS_((Tk_Canvas canvas,
			    PowCurveItem *powCurvePtr));
 int		ConfigurePowCurve _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int objc,
			    Tcl_Obj *CONST objv[], int flags));
 int		CreatePowCurve _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, struct Tk_Item *itemPtr,
			    int objc, Tcl_Obj *CONST objv[]));
 void		DeletePowCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display));
 void		DisplayPowCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display, Drawable dst,
			    int x, int y, int width, int height));
 int		GetPowCurveIndex _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr,
			    char *indexString, int *indexPtr));
 int		PowCurveCoords _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr,
			    int objc, Tcl_Obj *CONST objv[]));
 void		PowCurveDeleteCoords _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, int first, int last));
 void		PowCurveInsert _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, int beforeThis, char *string));
 int		PowCurveToArea _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *rectPtr));
 double		PowCurveToPoint _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *coordPtr));
 int		PowCurveToPostscript _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int prepass));
 void		ScalePowCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double originX, double originY,
			    double scaleX, double scaleY));
 void		TranslatePowCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double deltaX, double deltaY));


#endif /*  _POW_H   */
