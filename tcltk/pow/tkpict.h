/*
 * tkpict.h --
 *
 * A header file for Pict images 
 *
 * Copyright (c) 1995 The Regents of the University of California.
 * 
 * Author: Pierre-Louis Bossart
 * Date: November 17, 1995
 *
 * Derived from tkImgPhoto.c in the tk4.0b2 distribution 
 * copyrighted as follows:
 *
 *    Copyright (c) 1994 The Australian National University.
 *    Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 *    Author: Paul Mackerras (paulus@cs.anu.edu.au),
 *	      Department of Computer Science,
 *	      Australian National University.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
 
#ifndef _TKPICT_H
#define _TKPICT_H

#include "tcl.h"
#include "tk.h"



#include <stdio.h>
#include <ctype.h>
#ifndef MAC_TCL
#   include <fcntl.h>
#endif
#   include <limits.h>
#include <math.h>
#   include <stdlib.h>
#include <string.h>

#if !(defined(__WIN32__) || defined(macintosh))
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#   include <unistd.h>
#endif

# include <X11/Xlib.h>
# include <X11/cursorfont.h>
# include <X11/keysym.h>
# include <X11/Xatom.h>
# include <X11/Xutil.h>


#ifndef NBBY
#   define NBBY 8
#endif

/*
 * Supply macros for seek offsets, if they're not already provided by
 * an include file.
 */

#ifndef SEEK_SET
#   define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#   define SEEK_CUR 1
#endif

#ifndef SEEK_END
#   define SEEK_END 2
#endif

#define UCHAR(c) ((unsigned char) (c))

/* absolute value of a */
#define ABS(a)		(((a)<0) ? -(a) : (a))

/* take binary sign of a, either -1, or 1 if >= 0 */
#define SGN(a)		(((a)<0) ? -1 : 1)

#define BYTE 0
#define WORD 1
#define LWORD 2
#define REAL 3
#define DOUBLE 4
#define LONGLONG 5

#define MAXTABLES 16
#define MAXPLANES 8
#define MAX_COLORS 256
#define MAX_CLUT_LEN 52
#define MAXWINDOWS 50
#define MAXLUTVALUE 255
#define MAX_LOOKUP 4096
#define S1 2.
#define S2 7.

extern int byteLookup[MAX_LOOKUP];

#define READ_SHARED_COLORMAP 0
#define DEFAULT_SCREEN_COLORMAP 1
#define DEFAULT_PRIVATE_COLORMAP 2
#define NEW_PRIVATE_COLORMAP 3
#define POW_COLORMAP 4

#undef MIN
#define MIN(a, b)	((a) < (b)? (a): (b))
#undef MAX
#define MAX(a, b)	((a) > (b)? (a): (b))

#define GRAY(r,g,b) ( ((int)(r)*11 + (int)(g)*16 + (int)(b)*5) >> 5)
#define NO_COPY 0
#define COPY 1
/*
 * The maximum number of pixels to transmit to the server in a
 * single XPutImage call.
 */

#define MAX_PIXELS 65536

typedef void *Tk_PictHandle;

/*
 * An unsigned 32-bit integral type, used for pixel values.
 * We use int rather than long here to accommodate those systems
 * where longs are 64 bits.
 */

typedef unsigned int pixel;


typedef struct PictColorTable {
  Display *display;		/* Display for windows using this instance. */
  Colormap colormap;		/* The image may only be used in windows with
				 * this particular colormap. */
  char colormap_level;          /* indicates what type of colormap is used */
  int ncolors;
  int lut_start;
  char atom;
  int refCount;		        /* Number of instances using this structure. */

  pixel redValues[MAX_COLORS];  /* Maps 8-bit values of red intensity
				 * to a pixel value or index in pixelMap. */
  int red[MAX_COLORS];
  int green[MAX_COLORS];
  int blue[MAX_COLORS];
  int intensity_lut[MAX_COLORS];
  int red_lut[MAX_COLORS];
  int green_lut[MAX_COLORS];
  int blue_lut[MAX_COLORS];

} PictColorTable;


#if !(defined(__WIN32__) || defined(macintosh))


extern Tk_ImageType                tkPictImageType;

/*
 * Definition of the data associated with each Pict image master.
 */

typedef struct PictMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter associated with the
				 * application using this image. */
    Tcl_Command imageCmd;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    int	flags;			/* Sundry flags, defined below. */
    int	width, height;		/* Dimensions of image. */
    int userWidth, userHeight;	/* User-declared image dimensions. */
    char *fileString;		/* Name of file to read into image. */
    char *dataString;		/* String value to use as contents of image. */
    char *format;		/* User-specified format of data in image
				 * file or string value. */
    char *data;                 /* original data read from file */
    int datatype;               /* datatype */
    int datasize;               /* datasize */
    int skip;                   /* size of header file in bytes */
    float pixel_x;              /* size of pixel in x direction */
    float pixel_y;              /* size of pixel in y direction */
    double user_dispmax;        /* max of data set by user */
    double user_dispmin;        /* min of data set by user */
    double dispmax;             /* max of data */
    double dispmin;             /* min of data */
    unsigned char *bytedata;	/* Local storage for 24-bit image. */
    Region validRegion;		/* X region indicating which parts of
				 * the image have valid image data. */
    struct PictInstance *instancePtr;
				/* First in the list of instances
				 * associated with this master. */
} PictMaster;

/*
 * Bit definitions for the flags field of a PictMaster.
 * COLOR_IMAGE:			1 means that the image has different color
 *				components.
 * IMAGE_CHANGED:		1 means that the instances of this image
 *				need to be redithered.
 */

#define COLOR_IMAGE		1
#define IMAGE_CHANGED		2


/*
 * The following data structure represents all of the instances of
 * a Pict image in windows on a given screen that are using the
 * same colormap.
 */
typedef struct PictInstance {
    Tk_Window  tkwin;           /* Pointer to window */
    PictMaster *masterPtr;	/* Pointer to master for image. */
    Display *display;		/* Display for windows using this instance. */
    Colormap colormap;		/* The image may only be used in windows with
				 * this particular colormap. */
    XVisualInfo visualInfo;	/* Information about the visual that these
				 * windows are using. */
    char colormap_level;        /* indicates what type of colormap is used */
    char has_overlay;           /* indicates whether instances has overlays */   
    PictColorTable *colorTable; /* Pointer to color table */
    struct PictInstance *nextPtr;
				/* Pointer to the next instance in the list
				 * of instances associated with this master. */
    int refCount;		/* Number of instances using this structure. */
    Pixmap pixels;		/* X pixmap containing dithered image. */
    int width, height;		/* Dimensions of the pixmap. */
    XImage *imagePtr;		/* Image structure for converted pixels. */
    GC gc;			/* Graphics context for writing images
				 * to the pixmap. */
    GC overlay_gc;              /* Graphics context for writing overlays
				 * to the pixmap. */
    int setgc;                  /* GXcopy,GXand,GXor,GXXor */
} PictInstance;

/*
 * The following data structure is used to return information
 * from ParseSubcommandOptions:
 */

struct SubcommandOptions {
    int options;		/* Individual bits indicate which
				 * options were specified - see below. */
    char *name;			/* Name specified without an option. */
    int fromX, fromY;		/* Values specified for -from option. */
    int fromX2, fromY2;		/* Second coordinate pair for -from option. */
    int toX, toY;		/* Values specified for -to option. */
    int toX2, toY2;		/* Second coordinate pair for -to option. */
    int zoomX, zoomY;		/* Values specified for -zoom option. */
    int subsampleX, subsampleY;	/* Values specified for -subsample option. */
    char *format;		/* Value specified for -format option. */
};

/*
 * Bit definitions for use with ParseSubcommandOptions:
 * Each bit is set in the allowedOptions parameter on a call to
 * ParseSubcommandOptions if that option is allowed for the current
 * Pict image subcommand.  On return, the bit is set in the options
 * field of the SubcommandOptions structure if that option was specified.
 *
 * OPT_FORMAT:			Set if -format option allowed/specified.
 * OPT_FROM:			Set if -from option allowed/specified.
 * OPT_SHRINK:			Set if -shrink option allowed/specified.
 * OPT_SUBSAMPLE:		Set if -subsample option allowed/spec'd.
 * OPT_TO:			Set if -to option allowed/specified.
 * OPT_ZOOM:			Set if -zoom option allowed/specified.
 */

#define OPT_FORMAT	1
#define OPT_FROM	2
#define OPT_SHRINK	4
#define OPT_SUBSAMPLE	8
#define OPT_TO		0x10
#define OPT_ZOOM	0x20

/*
 * Default configuration
 */
#define DEF_Pict_HEIGHT	"0"
#define DEF_Pict_PALETTE	""
#define DEF_Pict_WIDTH		"0"



/*
 * The following structure describes a block of pixels in memory:
 */

typedef struct Tk_PictImageBlock {
    unsigned char *pixelPtr;	/* Pointer to the first pixel. */
    int		width;		/* Width of block, in pixels. */
    int		height;		/* Height of block, in pixels. */
    int		pitch;		/* Address difference between corresponding
				 * pixels in successive lines. */
    int		pixelSize;	/* Address difference between successive
				 * pixels in the same line. */
    char        datatype;
    char        copy;            /* flag requiring copy or not */
    int         skip;            /* number of bytes to skip from header */ 
    float       pixel_x;         /* size of pixel in x direction */
    float       pixel_y;         /* size of pixel in y direction */
} Tk_PictImageBlock;



struct Tk_PictImageFormat {
    char *name;			/* Name of image file format */
    Tk_ImageFileMatchProc *fileMatchProc;
				/* Procedure to call to determine whether
				 * an image file matches this format. */
    Tk_ImageStringMatchProc *stringMatchProc;
				/* Procedure to call to determine whether
				 * the data in a string matches this format. */
    Tk_ImageFileReadProc *fileReadProc;
				/* Procedure to call to read data from
				 * an image file into a Pict image. */
    Tk_ImageStringReadProc *stringReadProc;
				/* Procedure to call to read data from
				 * a string into a Pict image. */
    Tk_ImageFileWriteProc *fileWriteProc;
				/* Procedure to call to write data from
				 * a Pict image to a file. */
    Tk_ImageStringWriteProc *stringWriteProc;
				/* Procedure to call to obtain a string
				 * representation of the data in a Pict
				 * image.*/
    struct Tk_PictImageFormat *nextPtr;
				/* Next in list of all Pict image formats
				 * currently known.  Filled in by Tk, not
				 * by image format handler. */
};

typedef struct Tk_PictImageFormat Tk_PictImageFormat;
extern void Tk_CreatePictImageFormat( Tk_PictImageFormat *formatPtr);
extern Tk_PictHandle Tk_FindPict(char *imageName);

extern void Tk_PictPutBlock( Tk_PictHandle handle, 
			    register Tk_PictImageBlock *block, 
			    int x, int y, int width, int height);
extern void Tk_PictPutZoomedBlock( Tk_PictHandle handle,
				  register Tk_PictImageBlock *blockPtr,
				  int x, int y, int width, int height,
				  int zoomX, int zoomY,
				  int subsampleX, int subsampleY);
extern void Tk_PictPutScaledBlock( Tk_PictHandle handle,
				  register Tk_PictImageBlock *blockPtr,
				  int x, int y, int width, int height,
				  double zoomX, double zoomY,
				  double Xoff, double Yoff);
extern void Tk_PictExpand(Tk_PictHandle handle,int width,int height);
extern void Tk_PictBlank( Tk_PictHandle handle);
extern void Tk_PictGetSize(Tk_PictHandle handle,int *width,int *height);
extern void Tk_PictSetSize(Tk_PictHandle handle,int width,int height);
extern int  Tk_PictGetImage(Tk_PictHandle handle,Tk_PictImageBlock *blockPtr);


extern void		DisposeInstance _ANSI_ARGS_((ClientData clientData));
extern void		Dither _ANSI_ARGS_((PictMaster *masterPtr,
			    int x, int y, int width, int height));
extern void		DitherInstance _ANSI_ARGS_((PictInstance *instancePtr,
			    int x, int y, int width, int height));

/* forward definitions */
extern int has_plb_segment;

#endif /*__WIN32__ || macintosh*/

extern int Private_Colormap;
extern int slice_nb;
extern int nb_slices;
extern XColor	 lut_colorcell_defs[MAX_COLORS];

extern int Visu_Init( Tcl_Interp *interp);
    
extern void linear_lut(int *lut,Display *disp,Colormap cmap,
		       
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);

extern void non_linear_lut(int *lut,int lut_size,int *x_lut,
			   int *y_lut,int nbpts,
			   Display *disp,Colormap cmap,
			   int ncolors,int lut_start,char overlay,
			   int *red,int *green,int *blue,
			   int *intensity_lut,int *red_lut,int *green_lut,
			   int *blue_lut);
extern void gray(Display *disp,Colormap cmap,
		 int ncolors,int lut_start,char overlay,
		 int *red,int *green,int *blue,
		 int *intensity_lut,int *red_lut,int *green_lut,
		 int *blue_lut);
extern void blkbdy(Display *disp,Colormap cmap,
		   int ncolors,int lut_start,char overlay, 
		   int *red,int *green,int *blue,
		   int *intensity_lut,int *red_lut,int *green_lut,
		   int *blue_lut);
extern void hot(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut);
extern void cold(Display *disp,Colormap cmap,
		 int ncolors,int lut_start,char overlay,
		 int *red,int *green,int *blue,
		 int *intensity_lut,int *red_lut,int *green_lut,
		 int *blue_lut);
extern void hls(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut);
extern void rgb(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut);
extern void spectrum(Display *disp,Colormap cmap,
		     int ncolors,int lut_start,char overlay,
		     int *red,int *green,int *blue,
		     int *intensity_lut,int *red_lut,int *green_lut,
		     int *blue_lut);
extern void invert_cmap(Display *disp,Colormap cmap,
		   int ncolors,int lut_start,char overlay,
		   int *red,int *green,int *blue,
		   int *intensity_lut,int *red_lut,int *green_lut,
		   int *blue_lut);
extern void randwalk_spectrum(Display *disp,Colormap cmap,
			      int ncolors,int lut_start,char overlay,
			      int *red,int *green,int *blue,
			      int *intensity_lut,int *red_lut,int *green_lut,
			      int *blue_lut);
extern void bowlerhat(Display *disp,Colormap cmap,
		      int ncolors,int lut_start,char overlay,
		      int *red,int *green,int *blue,
		      int *intensity_lut,int *red_lut,int *green_lut,
		      int *blue_lut);
extern void tophat(Display *disp,Colormap cmap,
		   int ncolors,int lut_start,char overlay,
		   int *red,int *green,int *blue,
		   int *intensity_lut,int *red_lut,int *green_lut,
		   int *blue_lut);
extern void hatgray(Display *disp,Colormap cmap,
		    int ncolors,int lut_start,char overlay,
		    int *red,int *green,int *blue,
		    int *intensity_lut,int *red_lut,int *green_lut,
		    int *blue_lut);
extern void hatct(Display *disp,Colormap cmap,
		  int ncolors,int lut_start,char overlay,
		  int *red,int *green,int *blue,
		  int *intensity_lut,int *red_lut,int *green_lut,
		  int *blue_lut);
extern void lut_thres(Display *disp,Colormap cmap,
		      int ncolors,int lut_start,char overlay,
		      int loval,int hival,
		      int *red,int *green,int *blue,
		      int *intensity_lut,int *red_lut,int *green_lut,
		      int *blue_lut);
extern void gray_ramp2(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void gray_ramp4(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void gray_step4(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void gray_step8(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void bgr_step  (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void bgr_ramp  (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void bgr_step2 (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void bgr_ramp2 (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void rygcbm_ramp(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void rygcbm_step(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);

extern void spectrum2 (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void inv_spec  (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void color1_lut (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void color2_lut (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);
extern void color3_lut (Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut);

extern int  customCmap(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut,
                       Tcl_Interp *interp, Tcl_Obj *lutObj);

extern void lut_ramp(int *lut,int begin,float beginv,int end,float endv);
extern void set_hls(int *red,int *green,int *blue);
extern void convert_HLS_rgb(float H,float L,float S,int *r,int *g,int *b);

extern int init_colors(Display *disp,Colormap *colormap,XVisualInfo *vis_info,
		       char *colormap_level,
		       int *numcolors,int *start_lut,char *atom,Tk_Window tkwin);

extern int convert_block_to_byte(void *in, unsigned char *out, int npts,
				 int in_type,double *dispmin, double *dispmax);

extern int convert_block_to_histo( void *in, int npts, int in_type,
                                   double *dispmin, double *dispmax,
                                   unsigned int *histo );

extern void equalize_histo( void *in_data, int data_type, unsigned int totalPix,
                            double *min, double *max );
extern void build_lookup( int *x_lut, int *y_lut, int nbpts );

extern void deinit_disp(Display *disp);
extern XVisualInfo *get_visual(Display *disp);

extern void put_lut(Display *disp,Colormap cmap,
		    int ncolors,int lut_start,char overlay,
		    int *red,int *green,int *blue,
		    int *intensity_lut,int *red_lut,int *green_lut,
		    int *blue_lut);

extern int AllocateColorTable(PictColorTable **colorTable,Display *disp, 
			      Colormap cmap,int colormap_level,int ncolors,
			      int lut_start,char atom);
extern int DisposeColorTable(PictColorTable *colorTable);

extern int GetColorTable(Tk_Window tkwin,
			 PictColorTable **colorTable,
			 XVisualInfo **visInfoPtr);


#endif  /*  _TKPICT_H  */
