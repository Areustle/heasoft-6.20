/*
 * tkImgPict.c --
 *
 * A source file for Pict images 
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

#include "tkpict.h"
/* if PLB_SEGMENT is defined, then include the plb_segment include file */
#ifdef PLB_SEGMENT
#include "plb_segment.h"
int has_plb_segment = 1;
#else 
int has_plb_segment = 0;
#endif

/*
 * Hash table used to provide access to Pict images from C code.
 */
static Tcl_HashTable imgPictHash;
static int imgPictHashInitialized;	/* set when Tcl_InitHashTable done */

static int		ImgPictCreate _ANSI_ARGS_((Tcl_Interp *interp,
			    char *name, int objc, Tcl_Obj *CONST objv[],
			    Tk_ImageType *typePtr, Tk_ImageMaster master,
			    ClientData *clientDataPtr));
static ClientData	ImgPictGet _ANSI_ARGS_((Tk_Window tkwin,
			    ClientData clientData));
static void		ImgPictDisplay _ANSI_ARGS_((ClientData clientData,
			    Display *display, Drawable drawable,
			    int imageX, int imageY, int width, int height,
			    int drawableX, int drawableY));
static void		ImgPictFree _ANSI_ARGS_((ClientData clientData,
			    Display *display));
static void		ImgPictDelete _ANSI_ARGS_((ClientData clientData));

Tk_ImageType tkPictImageType = {
    "pict",			/* name */
    ImgPictCreate,		/* createProc */
    ImgPictGet,		        /* getProc */
    ImgPictDisplay,		/* displayProc */
    ImgPictFree,		/* freeProc */
    ImgPictDelete,		/* deleteProc */
    NULL	/* nextPtr */
};

/*
 * List of option names.  The order here must match the order of
 * declarations of the OPT_* constants above.
 */ 

static char *optionNames[] = {
    "-format",
    "-from",
    "-shrink",
    "-subsample",
    "-to",
    "-zoom",
    (char *) NULL
};


/*
 * Information used for parsing configuration specifications:
 */
static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_STRING, "-data", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(PictMaster, dataString), TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-format", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(PictMaster, format), TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-file", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(PictMaster, fileString), TK_CONFIG_NULL_OK},
    {TK_CONFIG_INT, "-height", (char *) NULL, (char *) NULL,
	 DEF_Pict_HEIGHT, Tk_Offset(PictMaster, userHeight), 0},
    {TK_CONFIG_INT, "-width", (char *) NULL, (char *) NULL,
	 DEF_Pict_WIDTH, Tk_Offset(PictMaster, userWidth), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	 (char *) NULL, 0, 0}
};



/*
 * Pointer to the first in the list of known Pict image formats.
 */

static Tk_PictImageFormat *formatList = NULL;

/*
 * Forward declarations
 */

static int		ImgPictCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, const char **argv));
static int		ParseSubcommandOptions _ANSI_ARGS_((
			    struct SubcommandOptions *optPtr,
			    Tcl_Interp *interp, int allowedOptions,
			    int *indexPtr, int argc, const char **argv));
static void		ImgPictCmdDeletedProc _ANSI_ARGS_((
			    ClientData clientData));
static int		ImgPictConfigureMaster _ANSI_ARGS_((
			    Tcl_Interp *interp, PictMaster *masterPtr,
			    int argc, const char **argv, int flags));
static void		ImgPictConfigureInstance _ANSI_ARGS_((
			    PictInstance *instancePtr));
static void		ImgPictSetSize _ANSI_ARGS_((PictMaster *masterPtr,
			    int width, int height));
static void		ImgPictInstanceSetSize _ANSI_ARGS_((
			    PictInstance *instancePtr));
static int              ImgPictCopy(Tcl_Interp *interp,
				    PictMaster *masterPtr,
				    int argc,
				    const char **argv);
static int              ImgPictSnap2Photo(Tcl_Interp *interp,
					  PictMaster *masterPtr,
					  int argc,
					  const char **argv);
static int              ImgPictSnap2Pict(Tcl_Interp *interp,
					 PictMaster *masterPtr,
					 int argc,
					 const char **argv);
static int		MatchFileFormat _ANSI_ARGS_((Tcl_Interp *interp,
			    Tcl_Channel f, char *fileName, char *formatString,
			    Tk_PictImageFormat **imageFormatPtr,
			    int *widthPtr, int *heightPtr));
static int		MatchStringFormat _ANSI_ARGS_((Tcl_Interp *interp,
			    char *string, char *formatString,
			    Tk_PictImageFormat **imageFormatPtr,
			    int *widthPtr, int *heightPtr));

int convert_block_to_byte(void *in, unsigned char *out, int npts,
			  int in_type,double *dispmin, double *dispmax);
static int make_colorbar(Tk_PictHandle handle,
			 int width,int height);
static void normalize_data(PictMaster *masterPtr);

static void get_line_pixels(char *string,unsigned char *img,
			    int nr,int nc,int x1,int y1,int x2,int y2,
			    double min, double max);

static int ChangeColorTable(PictMaster *masterPtr);
#ifdef PLB_SEGMENT
static int ImgPictClip(Tcl_Interp *interp,
		       PictMaster *masterPtr,
		       int argc,
		       char **argv);
static int ImgPictThreshold(Tcl_Interp *interp,
			    PictMaster *masterPtr,
			    int argc,
			    char **argv);
static int ImgPictSmooth(Tcl_Interp *interp,
			    PictMaster *masterPtr,
			    int argc,
			    char **argv);
static int ImgPictGradient(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv);
static int ImgPictLaplacian(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv);
static int ImgPictZeroCrng(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv);
static int ImgPictErosion(Tcl_Interp *interp,
			  PictMaster *masterPtr,
			  int argc,
			  char **argv);
static int ImgPictDilation(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv);
static int ImgPictCloseHoles(Tcl_Interp *interp,
			     PictMaster *masterPtr,
			     int argc,
			     char **argv);
static int ImgPictGetHoles(Tcl_Interp *interp,
			     PictMaster *masterPtr,
			     int argc,
			     char **argv);
static int ImgPictDistanceTransform(Tcl_Interp *interp,
				    PictMaster *masterPtr,
				    int argc,
				    char **argv);
static int ImgPictLabel(Tcl_Interp *interp,
			PictMaster *masterPtr,
			int argc,
			char **argv);

#endif


/*
 *----------------------------------------------------------------------
 *
 * Tk_CreatePictImageFormat --
 *
 *	This procedure is invoked by an image file handler to register
 *	a new Pict image format and the procedures that handle the
 *	new format.  The procedure is typically invoked during
 *	Tcl_AppInit.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The new image file format is entered into a table used in the
 *	Pict image "read" and "write" subcommands.
 *
 *----------------------------------------------------------------------
 */

void
Tk_CreatePictImageFormat(formatPtr)
    Tk_PictImageFormat *formatPtr;
				/* Structure describing the format.  All of
				 * the fields except "nextPtr" must be filled
				 * in by caller.  Must not have been passed
				 * to Tk_CreatePictImageFormat previously. */
{
    Tk_PictImageFormat *copyPtr;

#ifdef DEBUG
    printf("Tk_CreatePictImageFormat\n");
#endif

    copyPtr = (Tk_PictImageFormat *) ckalloc(sizeof(Tk_PictImageFormat));
    if(copyPtr == NULL ) {
      (void)fprintf(stderr,"Tk_CreatePictImageFormat: Could not allocate memory\n");
      return;
    }
    *copyPtr = *formatPtr;
    copyPtr->name = (char *) ckalloc((unsigned) (strlen(formatPtr->name) + 1));
    if(copyPtr->name == NULL ) {
      (void)fprintf(stderr,"Tk_CreatePictImageFormat: Could not allocate memory\n");
      return;
    }
    strcpy(copyPtr->name, formatPtr->name);
    copyPtr->nextPtr = formatList;
    formatList = copyPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictCreate --
 *
 *	This procedure is called by the Tk image code to create
 *	a new Pict image.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The data structure for a new Pict image is allocated and
 *	initialized.
 *
 *----------------------------------------------------------------------
 */
static
int ImgPictCreate(interp, name, objc, objv, typePtr, master, clientDataPtr)
    Tcl_Interp *interp;		/* Interpreter for application containing
				 * image. */
    char *name;			/* Name to use for image. */
    int objc;			/* Number of arguments. */
    Tcl_Obj *CONST objv[];	/* Argument strings for options (doesn't
				 * include image name or type). */
    Tk_ImageType *typePtr;	/* Pointer to our type record (not used). */
    Tk_ImageMaster master;	/* Token for image, to be used by us in
				 * later callbacks. */
    ClientData *clientDataPtr;	/* Store manager's token for image here;
				 * it will be returned in later callbacks. */
{
    PictMaster *masterPtr;
    Tcl_HashEntry *entry;
    int isNew;
    int j, argc;
    const char **argv;

#ifdef DEBUG
    printf("ImgPictCreate\n");
#endif

    /*  Convert Tcl_Objs to char *  */
    argc = objc;
    argv = (const char **) ckalloc( argc * sizeof(char *) );
    for( j=0; j<objc; j++ ) {
       argv[j] = Tcl_GetStringFromObj( objv[j], NULL );
    }

    /*
     * Allocate and initialize the Pict image master record.
     */

    masterPtr = (PictMaster *) ckalloc(sizeof(PictMaster));
    if(masterPtr == NULL ) {
      (void)fprintf(stderr,"ImgPictCreate: Could not allocate memory\n");
      ckfree( (char*) argv );
      return 0;
    }
    memset((void *) masterPtr, 0, sizeof(PictMaster));
    masterPtr->tkMaster = master;
    masterPtr->interp = interp;
    masterPtr->imageCmd = Tcl_CreateCommand(interp, name, ImgPictCmd,
	    (ClientData) masterPtr, ImgPictCmdDeletedProc);
    masterPtr->data = NULL;
    masterPtr->bytedata = NULL;
    masterPtr->instancePtr = NULL;
    masterPtr->validRegion = XCreateRegion();
    masterPtr->dispmax = 0.0;
    masterPtr->dispmin = 0.0;
    masterPtr->user_dispmax = 0.0;
    masterPtr->user_dispmin = 0.0;
    masterPtr->pixel_x = 1.0;
    masterPtr->pixel_y = 1.0;

    /*
     * Process configuration options given in the image create command.
     */

    if (ImgPictConfigureMaster(interp, masterPtr, argc, argv, 0) != TCL_OK) {
	ImgPictDelete((ClientData) masterPtr);
        ckfree( (char*) argv );
	return TCL_ERROR;
    }

    /*
     * Enter this Pict image in the hash table.
     */

    if (!imgPictHashInitialized) {
	Tcl_InitHashTable(&imgPictHash, TCL_STRING_KEYS);
	imgPictHashInitialized = 1;
    }
    entry = Tcl_CreateHashEntry(&imgPictHash, name, &isNew);
    Tcl_SetHashValue(entry, masterPtr);

    *clientDataPtr = (ClientData) masterPtr;

    ckfree( (char*) argv );
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictCmd --
 *
 *	This procedure is invoked to process the Tcl command that
 *	corresponds to a Pict image.  See the user documentation
 *	for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

static int
ImgPictCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about Pict master. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    const char **argv;		/* Argument strings. */
{
    PictMaster *masterPtr = (PictMaster *) clientData;
    int c, x, y;
    Tk_PictImageBlock block;
    char string[10000];
    Tk_PictHandle srcHandle;
    size_t length;    
    int pix_int;
    float pix_float;
    double pix_double;
    short *shortPtr;
    int *intPtr;
    float *floatPtr;
    double *doublePtr;
    int i;

#ifdef DEBUG
    printf("ImgPictCmd\n");
#endif

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'b') && (strncmp(argv[1], "blank", length) == 0)) {
      /*
       * Pict blank command - just call Tk_PictBlank.
       */
      
      if (argc == 2) {
	Tk_PictBlank(masterPtr);
      } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " blank\"", (char *) NULL);
	return TCL_ERROR;
      }
      return TCL_OK;
    }
    else if (c == 'c') {
      
      if( (strncmp(argv[1], "colorbar", length) == 0)) {
	make_colorbar((Tk_PictHandle)masterPtr,
		      masterPtr->width,masterPtr->height);
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "cget", length) == 0)) {
	if (argc != 3) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"",
			   argv[0], " cget option\"",
			   (char *) NULL);
	  return TCL_ERROR;
	}
	return Tk_ConfigureValue(interp, Tk_MainWindow(interp), configSpecs,
				 (char *) masterPtr, argv[2], 0);
      }
      else if( (strncmp(argv[1], "configure", length) == 0)) {
	/*
	 * Pict configure command - handle this in the standard way.
	 */
	
	if (argc == 2) {
	  return Tk_ConfigureInfo(interp, Tk_MainWindow(interp),
				  configSpecs, (char *) masterPtr, 
				  (char *) NULL, 0);
	}
	if (argc == 3) {
	  return Tk_ConfigureInfo(interp, Tk_MainWindow(interp),
				  configSpecs, (char *) masterPtr, argv[2], 0);
	}
	return ImgPictConfigureMaster(interp, masterPtr, argc-2, argv+2,
				      TK_CONFIG_ARGV_ONLY);
      } 
      else if( (strncmp(argv[1], "cmap_level", length) == 0)) {
	PictInstance *instancePtr;
	int cmap_level;
	
	if (argc != 2 && argc != 3) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " cmap_level [0|1|2|3] \"", 
			   (char *) NULL);
	  return TCL_ERROR;
	}

	if( (instancePtr=masterPtr->instancePtr) == NULL ) {
	  Tcl_AppendResult(interp, "No instance and no colors allocated yet", 
			   (char *) NULL);
	  return TCL_ERROR;
	}
	
	if (argc == 2) {
	  sprintf(string,"%d",instancePtr->colormap_level);
	  Tcl_AppendResult(interp,string,(char *) NULL);
	} else {
	  
	  if(Tcl_GetInt(interp, argv[2], &cmap_level) != TCL_OK) {
	    Tcl_AppendResult(interp, "Wrong arguments, should be: \"", argv[0],
			     " cmap_level [0|1|2|3]\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	  
	  if(cmap_level != READ_SHARED_COLORMAP &&
	     cmap_level != DEFAULT_SCREEN_COLORMAP &&
	     cmap_level != DEFAULT_PRIVATE_COLORMAP &&
	     cmap_level != NEW_PRIVATE_COLORMAP ) {
	    Tcl_AppendResult(interp, "Wrong arguments, should be: \"", argv[0],
			   " cmap_level  [0|1|2|3]\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	  
	  Private_Colormap = cmap_level;
	  
	  if( ChangeColorTable((PictMaster *)masterPtr) == 0 ) {     
	    Tcl_AppendResult(interp, "Could not change colormap level for active window", 
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	}
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "cmap_stretch", length) == 0)) {
	PictInstance *instancePtr;
	PictColorTable *colorTable;
	int cwid,clen;
	int x_lut[MAX_CLUT_LEN];
	int y_lut[MAX_CLUT_LEN];
	int i,j;
	int lut_size, nElem;
        const char **lutElem;
	int *p_lut;

	if( (instancePtr=masterPtr->instancePtr) == NULL )
	  return TCL_OK;

	  if (argc != 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " cmap_stretch [intensity|red|green|blue] cwid clen {x1 y1 x2 y2 ...}\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	  else {
	    colorTable = instancePtr->colorTable;
	    length =strlen(argv[2]);
	    if( (strncmp(argv[2], "intensity", length) == 0)) {
	      p_lut = colorTable->intensity_lut;
	      lut_size = colorTable->ncolors;
	    } else if( (strncmp(argv[2], "red", length) == 0)) {
	      p_lut = colorTable->red_lut;
	      lut_size = MAX_COLORS;
	    } else if( (strncmp(argv[2], "green", length) == 0)) {
	      p_lut = colorTable->green_lut;
	      lut_size = MAX_COLORS;
	    } else if( (strncmp(argv[2], "blue", length) == 0)) {
	      p_lut = colorTable->blue_lut;
	      lut_size = MAX_COLORS;
	    } else {
	      Tcl_AppendResult(interp, "bad lookup table : should be \"", argv[0],
			       " cmap_stretch [intensity|red|green|blue] cwid clen x1 y1 x2 y2 ... \"", 
			       (char *) NULL);
	      return TCL_ERROR;
	    }
	    
	    if((Tcl_GetInt(interp, argv[3], &cwid) != TCL_OK) ||
	       (Tcl_GetInt(interp, argv[4], &clen) != TCL_OK) ) {
	      Tcl_AppendResult(interp, "bad lookup table : should be \"", argv[0],
			       " cmap_stretch [intensity|red|green|blue] cwid clen {x1 y1 x2 y2 ... }\"", 
			       (char *) NULL);
	      return TCL_ERROR;
	    }

            if( Tcl_SplitList( interp, argv[5], &nElem, &lutElem )
                != TCL_OK ) {
               Tcl_AppendResult(interp, "Error reading LUT",
                                (char*)NULL);
               return TCL_ERROR;
            }

            if( nElem<4 || nElem&0x1 ) {
               Tcl_SetResult(interp,"LUT must have an even number of elements >= 4",
                             TCL_VOLATILE);
               ckfree( (char*)lutElem );
               return TCL_ERROR;
            }


	    i = 0;
	    j = 0;
	    while(i<nElem) {
	      if((Tcl_GetInt(interp, lutElem[i++], &x_lut[j]) != TCL_OK) ||
		 (Tcl_GetInt(interp, lutElem[i++], &y_lut[j]) != TCL_OK) ) {
		Tcl_AppendResult(interp, "bad lookup table : should be \"", argv[0],
				 " cmap_stretch [intensity|red|green|blue] cwid clen {x1 y1 x2 y2 ...} \"", 
				 (char *) NULL);
                ckfree( (char*)lutElem );
		return TCL_ERROR;
	      }
	      j++;
	    }
            ckfree( (char*)lutElem );
	    
	    for(i=0;i<j;i++) {
	      x_lut[i] = floor((double)x_lut[i]/(double)cwid * lut_size); 
	      y_lut[i] = floor((double)y_lut[i]/(double)clen * lut_size);
	    }
      
	    non_linear_lut(p_lut,lut_size,
			   x_lut,y_lut,j,
			   instancePtr->display,
			   instancePtr->colormap,
			   colorTable->ncolors,
			   colorTable->lut_start,
			   instancePtr->has_overlay,
			   colorTable->red,colorTable->green,colorTable->blue,
			   colorTable->intensity_lut,
			   colorTable->red_lut,colorTable->green_lut,
			   colorTable->blue_lut); 
	  
	    return TCL_OK;
	  }
      }
      else if (strncmp(argv[1], "cmap_threshold", length) == 0) {
	double loval,hival;
	PictInstance *instancePtr;
	PictColorTable *colorTable;
	
	if (argc != 4) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " cmap_threshold x y\"", (char *) NULL);
	  return TCL_ERROR;
	}
	if ((Tcl_GetDouble(interp, argv[2], &loval) != TCL_OK)
	    || (Tcl_GetDouble(interp, argv[3], &hival) != TCL_OK)) {
	  return TCL_ERROR;
	}
	
	if( masterPtr->instancePtr == NULL ) 
	  return TCL_OK;

	instancePtr = masterPtr->instancePtr;
	colorTable = instancePtr->colorTable;
	
	loval = (double)(loval - masterPtr->dispmin) *
	  (double)(colorTable->ncolors) /
	    (double)(masterPtr->dispmax - masterPtr->dispmin);
	hival = (hival - masterPtr->dispmin) *
	  (double)(colorTable->ncolors) /
	    (double)(masterPtr->dispmax - masterPtr->dispmin);
	
	lut_thres(instancePtr->display,
		  instancePtr->colormap,
		  colorTable->ncolors,
		  colorTable->lut_start,
		  instancePtr->has_overlay,
		  floor(loval),ceil(hival),
		  colorTable->red,colorTable->green,colorTable->blue,
		  colorTable->intensity_lut,
		  colorTable->red_lut,colorTable->green_lut,
		  colorTable->blue_lut); 
	return TCL_OK;
      }
      else if ( (strncmp(argv[1], "colormap", length) == 0)) {
	/*
	 * Pict colormap command - first parse and check parameters.
	 */
	PictInstance *instancePtr;
	PictColorTable *colorTable;
	void (*f)(Display *display,Colormap cmap,
		 int ncolors,int lut_start,char overlay,
		 int *red,int *green,int *blue,
		 int *intensity_lut,int *red_lut,int *green_lut,
		 int *blue_lut);

	if (argc != 3) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " colormap cmap\"", (char *) NULL);
	  return TCL_ERROR;
	}
	instancePtr = masterPtr->instancePtr;
	
	if( instancePtr == NULL )
	  return TCL_OK;

	colorTable = instancePtr->colorTable;

	if(strcmp(argv[2], "gray") == 0)
	   f = gray;
	else if(strcmp(argv[2], "blkbdy") == 0)
	   f = blkbdy;
	else if(strcmp(argv[2], "hot") == 0)
	   f = hot;
	else if(strcmp(argv[2], "cold") == 0)
	   f= cold;
	else if(strcmp(argv[2], "hls") == 0)
	   f = hls;
	else if(strcmp(argv[2], "rgb") == 0)
	   f = rgb;
	else if(strcmp(argv[2], "invert") == 0)
	   f = invert_cmap;
	else if(strcmp(argv[2], "random") == 0)
	   f = randwalk_spectrum;
	else if(strcmp(argv[2], "bowlerhat") == 0)
	   f = bowlerhat;
	else if(strcmp(argv[2], "tophat") == 0)
	   f = tophat;
	else if(strcmp(argv[2], "hatgray") == 0)
	   f = hatgray;
	else if(strcmp(argv[2], "hatct") == 0)
	   f = hatct;
	else if(strcmp(argv[2], "gray-ramp2") == 0)
	   f = gray_ramp2;
	else if(strcmp(argv[2], "gray-ramp4") == 0)
	   f = gray_ramp4;
	else if(strcmp(argv[2], "gray-step4") == 0)
	   f = gray_step4;
	else if(strcmp(argv[2], "gray-step8") == 0)
	   f = gray_step8;
	else if(strcmp(argv[2], "bgr-step") == 0)
	   f = bgr_step;
	else if(strcmp(argv[2], "bgr-ramp") == 0)
	   f = bgr_ramp;
	else if(strcmp(argv[2], "bgr-step2") == 0)
	   f = bgr_step2;
	else if(strcmp(argv[2], "bgr-ramp2") == 0)
	   f = bgr_ramp2;
	else if(strcmp(argv[2], "rygcbm-ramp") == 0)
	   f = rygcbm_ramp;
	else if(strcmp(argv[2], "rygcbm-step") == 0)
	   f = rygcbm_step;
	else if(strcmp(argv[2], "spectrum") == 0)
	   f = spectrum2;
	else if(strcmp(argv[2], "inv_spec") == 0)
	   f = inv_spec;
	else if(strcmp(argv[2], "color1") == 0)
	   f = color1_lut;
	else if(strcmp(argv[2], "color2") == 0)
	   f = color2_lut;
	else if(strcmp(argv[2], "color3") == 0)
	   f = color3_lut;
	else {
           /*  Try to find a user-specified cmap...   */

           char scrtch[200];
           Tcl_Obj *lut;

           sprintf(scrtch,"cmapLUT_%s,powDef",argv[2]);
           lut = Tcl_ObjGetVar2(interp,
                                Tcl_NewStringObj("powImageParam",-1),
                                Tcl_NewStringObj(scrtch,-1),
                                TCL_GLOBAL_ONLY);
           if( lut==NULL ) {
              sprintf(scrtch,"Unable to locate LUT for %s\n",argv[2]);
              Tcl_SetResult(interp,scrtch,TCL_VOLATILE);
              return TCL_ERROR;
           }

           return
              customCmap(instancePtr->display,
                         instancePtr->colormap,
                         colorTable->ncolors,
                         colorTable->lut_start,
                         instancePtr->has_overlay,
                         colorTable->red,colorTable->green,colorTable->blue,
                         colorTable->intensity_lut,
                         colorTable->red_lut,colorTable->green_lut,
                         colorTable->blue_lut,
                         interp, lut);
        }
	
	(*f)(instancePtr->display,
	  instancePtr->colormap,
	  colorTable->ncolors,
	  colorTable->lut_start,
	  instancePtr->has_overlay,
	  colorTable->red,colorTable->green,colorTable->blue,
	  colorTable->intensity_lut,
	  colorTable->red_lut,colorTable->green_lut,colorTable->blue_lut);
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "copy", length) == 0)) {
	
	return ImgPictCopy(interp,masterPtr,argc,argv);
       
      }
#ifdef PLB_SEGMENT  
      else if ( (strncmp(argv[1], "clip", length) == 0)) {
	return ImgPictClip(interp,masterPtr,argc,argv);
      }
      else if ( (strncmp(argv[1], "close_holes", length) == 0)) {
	return ImgPictCloseHoles(interp,masterPtr,argc,argv);
      } 
      else if (strncmp(argv[1], "convert", length) == 0) {
	PictMaster   *srcMasterPtr;
	void *buff;
	int npts;
	int datatype,datasize;

	if(argc!=5) {
	  Tcl_AppendResult(interp, "wrong # args: should be  \"", argv[0],
			   " convert srcImg  -type [byte|short|int|float] \"", 
			   (char *) NULL);
	  return TCL_ERROR;
	}

	length = strlen(argv[3]);
	if (strncmp(argv[3], "-type", length) != 0) {
	  Tcl_AppendResult(interp, "wrong # args: should be  \"", argv[0],
			   " convert srcImg -type [byte|short|int|float]\"", 
			   (char *) NULL);
	  return TCL_ERROR;
	}
	length = strlen(argv[4]);
	if ((strncmp(argv[4], "byte", length) != 0) &&
	    (strncmp(argv[4], "short", length) != 0) &&
	    (strncmp(argv[4], "int", length) != 0) &&
	    (strncmp(argv[4], "float", length) != 0))
	  {
	    Tcl_AppendResult(interp, "bad type : should be  \"", argv[0],
			     " convert srcImg -type [byte|short|int|float]\"",
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	
	
	if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
	  Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
			   " exist or is not a Pict image", (char *) NULL);
	  return TCL_ERROR;
	}
	
	srcMasterPtr = (PictMaster *)srcHandle;
	
	/* set new type */
	if (strcmp(argv[4],"byte") == 0) {
	  datatype = BYTE;
	  datasize = sizeof(unsigned char);
	}
	else if(strcmp(argv[4],"short") == 0) {
	  datatype = WORD;
	  datasize = sizeof(short); 
	}
	else if(strcmp(argv[4],"int") == 0) {
	  datatype = LWORD;
	  datasize = sizeof(int);
	}
	else if(strcmp(argv[4],"float") == 0) {
	  datatype = REAL;
	  datasize = sizeof(float);
	} 
	else if(strcmp(argv[4],"double") == 0) {
	  datatype = DOUBLE;
	  datasize = sizeof(double);
	}
	else {
	  Tcl_AppendResult(interp, argv[0], " convert : ",
			   "unknown data type", (char *) NULL);
	  return TCL_ERROR;
	}

	/* allocate memory */
	npts = (srcMasterPtr->width)*(srcMasterPtr->height);
	buff = (void*)ckalloc(npts*datasize);
	if( buff == NULL ) {
	  Tcl_AppendResult(interp, "Cannot allocate memory for conversion",
			   (char*)NULL);
	  return TCL_ERROR;
	}
	/* convert data */
	lconvert_types(npts,(void*)srcMasterPtr->data,
		       srcMasterPtr->datatype,
		       (void*)buff,datatype);

	/* set image size */
	Tk_PictExpand(masterPtr,srcMasterPtr->width,srcMasterPtr->height);

       	/* initialize block */
	block.pixelPtr = (unsigned char*)buff;
	block.width = srcMasterPtr->width;
	block.height = srcMasterPtr->height;
	block.pitch = block.width;
	block.pixelSize = datasize;
	block.datatype =  datatype;
	block.pixel_x = srcMasterPtr->pixel_x;
	block.pixel_y = srcMasterPtr->pixel_y;
	block.copy = NO_COPY;
	block.skip = 0;
	
	Tk_PictPutBlock(masterPtr,&block,0,0,
			masterPtr->width,masterPtr->height); 

	return TCL_OK;
      }
#endif
    }
#ifdef PLB_SEGMENT  
    else if(c=='d') {
      if ( (strncmp(argv[1], "dilation", length) == 0)) {
	return ImgPictDilation(interp,masterPtr,argc,argv);
      }
      else if(strncmp(argv[1], "dt", length) == 0) {
	return ImgPictDistanceTransform(interp,masterPtr,argc,argv);
      }
    }
    else if(c=='e') {
      if ( (strncmp(argv[1], "erosion", length) == 0)) {
	return ImgPictErosion(interp,masterPtr,argc,argv);
      }
    }
#endif
    else if (c == 'g') {
      
      if( (strncmp(argv[1], "get", length) == 0)) {
	/*
	 * Pict get command - first parse and check parameters.
	 */
	
	if (argc != 4) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " get x y\"", (char *) NULL);
	  return TCL_ERROR;
	}
	if ((Tcl_GetInt(interp, argv[2], &x) != TCL_OK)
	    || (Tcl_GetInt(interp, argv[3], &y) != TCL_OK)) { 
	  return TCL_ERROR;
	}
	if ((x < 0) || (x >= masterPtr->width)
	    || (y < 0) || (y >= masterPtr->height)) {
	  Tcl_AppendResult(interp, argv[0], " get: ",
			   "coordinates out of range", (char *) NULL);
	  return TCL_ERROR;
	}
	
	/*
	 * Extract the value of the desired pixel and format it as a string.
	 */
	
	switch(masterPtr->datatype)
	  {
	  case BYTE:
	    pix_int = (masterPtr->bytedata)[y * masterPtr->width + x];
	    sprintf(string, "%d", pix_int);
	    break;
	  case WORD:
	    shortPtr = (short*)masterPtr->data;
	    pix_int = shortPtr[y * masterPtr->width + x];
	    sprintf(string, "%d", pix_int);
	    break;
	  case LWORD:
	    intPtr = (int*)masterPtr->data;
	    pix_int = intPtr[y * masterPtr->width + x];
	    sprintf(string, "%d", pix_int);
	    break;
	  case REAL:
	    floatPtr = (float*)masterPtr->data;
	    pix_float = floatPtr[y* masterPtr->width + x];
	    sprintf(string, "%.5g", pix_float);
	    break;
	  case DOUBLE:
	    doublePtr = (double*)masterPtr->data;
	    pix_double = doublePtr[y* masterPtr->width + x];
	    sprintf(string, "%.5g", pix_double);
	    break;
	  default:
	    Tcl_AppendResult(interp, argv[0], " get: ",
		    "unknown data type", (char *) NULL);
	    return TCL_ERROR;
	  }
	Tcl_SetResult(interp,string,TCL_VOLATILE);
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "getline", length) == 0)) {
	int x0,y0,x1,y1;

	/*
	 * Pict get command - first parse and check parameters.
	 */
	
	if (argc != 6) {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " getline x0 y0 x1 y1\"", (char *) NULL);
	  return TCL_ERROR;
	}
	
	if((Tcl_GetInt(interp, argv[2], &x0) != TCL_OK) ||
	   (Tcl_GetInt(interp, argv[3], &y0) != TCL_OK) ||
	   (Tcl_GetInt(interp, argv[4], &x1) != TCL_OK) ||
	   (Tcl_GetInt(interp, argv[5], &y1) != TCL_OK))
	  {
	    Tcl_AppendResult(interp, argv[0], " getline: ",
			     "parameters out of range", (char *) NULL);
	    return TCL_ERROR;
	  }
	
	sprintf(string,"");
	get_line_pixels(string,masterPtr->bytedata,
			masterPtr->height,
			masterPtr->width,
			x0,y0,x1,y1,
			masterPtr->dispmin,
			masterPtr->dispmax);

	Tcl_AppendResult(interp, string, (char *) NULL); 
	return TCL_OK;
      }  
      else if( (strncmp(argv[1], "getmin", length) == 0)) {
	sprintf(string,"%.5g",masterPtr->dispmin);
	Tcl_AppendResult(interp, string, (char *) NULL); 
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "getmax", length) == 0)) {
	sprintf(string,"%.5g",masterPtr->dispmax);
	Tcl_AppendResult(interp, string, (char *) NULL); 
	return TCL_OK;
      }
#ifdef PLB_SEGMENT
      else if( (strncmp(argv[1], "get_holes", length) == 0)) {
	return ImgPictGetHoles(interp,masterPtr,argc,argv); 
      }
      else if( (strncmp(argv[1], "gradient", length) == 0)) {
	return ImgPictGradient(interp,masterPtr,argc,argv);
      }
#endif
    }
    else if(c== 'h') {
      if( strncmp(argv[1], "histogram", length) == 0 ) {
	int hist[256];
	char string1[256];
	int size;
	unsigned char *ptr;
	
	size = (masterPtr->width)*(masterPtr->height);
	
	/* clear histogram */
	for (i = 0; i < 256; i++) 
	  hist[i] = 0;

	/* fill histogram */
	ptr = masterPtr->bytedata;
	for (i = 0;i < size; i++)
	  hist[(*ptr++)]++;
	
	/* format output string */
	sprintf(string,"");
	for(i=0;i<256;i++) {
	  sprintf(string1,"%g ",((double)i/(double)(MAX_COLORS-1.0)*
				 (masterPtr->dispmax-masterPtr->dispmin))
		  +masterPtr->dispmin); 
	  strcat(string,string1);
	  sprintf(string1,"%d ",hist[i]);
	  strcat(string,string1);
	}
	Tcl_AppendResult(interp, string, (char *) NULL); 
	return TCL_OK;
      }
    }
#ifdef PLB_SEGMENT
    else if( c=='l') {
      if(strncmp(argv[1], "label", length) == 0) {
	return ImgPictLabel(interp,masterPtr,argc,argv);
      } else if(strncmp(argv[1], "laplacian", length) == 0) {
	return ImgPictLaplacian(interp,masterPtr,argc,argv);
      }
    }
#endif

    else if ((c == 'r') && (strncmp(argv[1], "range", length) == 0)) {
      double dispmax;
      double dispmin;
      PictInstance *instancePtr;

      if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " range x y\"", (char *) NULL);
	return TCL_ERROR;
      }
      if((Tcl_GetDouble(interp, argv[2], &dispmin) != TCL_OK) 
	 || (Tcl_GetDouble(interp, argv[3], &dispmax) != TCL_OK))
	return TCL_ERROR;
      masterPtr->user_dispmax = dispmax;
      masterPtr->user_dispmin = dispmin;

      normalize_data(masterPtr);
      
      /*
       * Update each instance.
       */
      for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	  instancePtr = instancePtr->nextPtr)
	DitherInstance(instancePtr, 0, 0,
		       instancePtr->width,
		       instancePtr->height);

      /*
       * Tell the core image code that this image has changed.
       */

      Tk_ImageChanged(masterPtr->tkMaster,0,0,
		      masterPtr-> width, 
		      masterPtr->height, 
		      masterPtr->width,
		      masterPtr->height);
      return TCL_OK;
    }
    else if (c == 'r') {
      
      if( (strncmp(argv[1], "redither", length) == 0)) {
	
	if (argc == 2) {
	  
	  PictInstance *instancePtr;
	  
	  for (instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	       instancePtr = instancePtr->nextPtr) {
	    DitherInstance(instancePtr, 0, 0, 
			   instancePtr->width,instancePtr->height);
	  }
	  i++;
	  Tk_ImageChanged(masterPtr->tkMaster, 0, 0,
			  masterPtr->width, masterPtr->height,
			  masterPtr->width, masterPtr->height);
	} else {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " redither\"", (char *) NULL);
	  return TCL_ERROR;
	} 
	return TCL_OK;
      } 
      else if( (strncmp(argv[1], "rxsize", length) == 0)) {
	if (argc == 2) {
	  sprintf(string,"%g",masterPtr->pixel_x);
		  
	  Tcl_AppendResult(interp, string, (char *) NULL);
	} else {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " rxsize\"", (char *) NULL);
	  return TCL_ERROR;
	}
	return TCL_OK;
      }
      else if( (strncmp(argv[1], "rysize", length) == 0)) {
	if (argc == 2) {
	  sprintf(string,"%g",masterPtr->pixel_y);
		  
	  Tcl_AppendResult(interp, string, (char *) NULL);
	} else {
	  Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			   " rysize\"", (char *) NULL);
	  return TCL_ERROR;
	}
	return TCL_OK;
      }
    }
    else if (c == 's') {

      if( (strncmp(argv[1], "snap2photo", length) == 0)) {
	return ImgPictSnap2Photo(interp,masterPtr,argc,argv);
      }
      else 
	if( (strncmp(argv[1], "snap2pict", length) == 0)) {
	  return ImgPictSnap2Pict(interp,masterPtr,argc,argv);
	}  
#ifdef PLB_SEGMENT
      else if( (strncmp(argv[1], "smooth", length) == 0)) {
	return ImgPictSmooth(interp,masterPtr,argc,argv);
      }
      else if( (strncmp(argv[1], "snake", length) == 0)) {
	return ImgPictSnakeCmd(masterPtr,interp,argc,argv);
      }
#endif
    }
    else if ( c == 't') {
#ifdef PLB_SEGMENT
      if (strncmp(argv[1], "threshold", length) == 0) {
	return ImgPictThreshold(interp,masterPtr,argc,argv);
      } /* end threshold */
      else
#endif 
      if (strncmp(argv[1], "thres_isodata", length) == 0) {
	int hist[256];
	int size;
	unsigned char *ptr;
	double t1,t2,g1,g2;
	int i,jt,it;
	int n = 256;
	double scale;
	
	if( argc != 2 ) {
	  Tcl_AppendResult(interp," wrong # of arguments: should be ",argv[0],
			   " thres_isodata ",(char *) NULL); 
	  return TCL_ERROR;
	}
	size = (masterPtr->width)*(masterPtr->height);
	
	/* clear histogram */
	for (i = 0; i < n; i++) 
	  hist[i] = 0;
	
	/* fill histogram */
	ptr = masterPtr->bytedata;
	for (i = 0;i < size; i++)
	  hist[(*ptr++)]++;
	
	/* find threshold width isodata algorithm */
	jt = -1;
	do
	  {
	    it = jt;
	    for (t1 = g1 = 0.0, i = 0; i < it; i++)
	      {
		t1 += hist[i];
		g1 += hist[i] * ((double)i);
	      }
	    g1 = (t1 == 0.0 ? 0.0 : g1/t1);
	    for (t2 = g2 = 0.0; i < n; i++)
	      {
		t2 += hist[i];
		g2 += hist[i] * ((double)i);
	      }
	    g2 = (t2 == 0.0 ? (it-1) : g2/t2);
	    jt = (jt == -1 ? g2 + 1 : (g1 + g2)/2 + 1);
	  } while (jt != it);
	
	scale = (masterPtr->dispmax-masterPtr->dispmin)/255.0;
	
	if( masterPtr->datatype != REAL && masterPtr->datatype != DOUBLE) 
	  sprintf(string,"%d",(int)((double)jt*scale + masterPtr->dispmin));
	else 
	  sprintf(string,"%g",(double)jt*scale + masterPtr->dispmin);
	
	Tcl_AppendResult(interp, string, (char *) NULL); 
	return TCL_OK;
      } /* end thres_isodata */
      else if( strncmp(argv[1],"type",length) == 0) {
	if(argc!=2) {
	  Tcl_AppendResult(interp," Wrong number of arguments, should be ",
			   argv[0]," type",(char*)NULL);
	  return TCL_ERROR;
	}
	switch(masterPtr->datatype)
	  {
	  case BYTE:
	    Tcl_SetResult(interp,"byte",TCL_VOLATILE);
	    break;
	  case WORD:
	    Tcl_SetResult(interp,"short",TCL_VOLATILE);
	    break;
	  case LWORD:
	    Tcl_SetResult(interp,"int",TCL_VOLATILE);
	    break;
	  case REAL:
	     Tcl_SetResult(interp,"float",TCL_VOLATILE);
	    break;
	  case DOUBLE:
	     Tcl_SetResult(interp,"double",TCL_VOLATILE);
	    break;
	  default:
	    Tcl_AppendResult(interp, argv[0], " type: ",
		    "unknown data type", (char *) NULL);
	    return TCL_ERROR;
	  }
	return TCL_OK;
      }
    } /* endif c == 't' */
    else if (c == 'w') { 

    }
#ifdef PLB_SEGMENT
    else if( c=='z') {
      if(strncmp(argv[1], "zero_crossings", length) == 0) {
	return ImgPictZeroCrng(interp,masterPtr,argc,argv);
      }
    } 
#endif
  
    /* If this point is reached, issue error message and list of commands */
      
#ifdef PLB_SEGMENT 
      
    Tcl_AppendResult(interp, "bad option \"", argv[1],
		     "\": must be blank, configure, cget, cmap_stretch,",
		     "cmap_threshold, colorbar, colormap, copy, clip, convert,"
		     "erosion, dt, dilation, get,getline, getmin, getmax,"
		     "gradient, histogram, label, laplacian, overlay,"
		     "range,read, rdbinary, redither, snap2photo, snap2pict",
		     "smooth, threshold, type, write, wrbinary, zero_crng",
		     (char *) NULL); 


#else 
    Tcl_AppendResult(interp, "bad option \"", argv[1], 
		     "\": must be blank, configure, cget, cmap_stretch,",
		     "cmap_threshold, colorbar, colormap, copy, get,",
		     "getline, getmin, getmax, histogram, overlay, range,",
		     "read, rdbinary, redither, snap2photo, snap2pict, type, write,wrbinary", 
		     (char *) NULL); 
#endif 
    
    return TCL_ERROR; 
    
}

/*
 *----------------------------------------------------------------------
 *
 * ParseSubcommandOptions --
 *
 *	This procedure is invoked to process one of the options
 *	which may be specified for the Pict image subcommands,
 *	namely, -from, -to, -zoom, -subsample, -format, and -shrink.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Fields in *optPtr get filled in.
 *
 *----------------------------------------------------------------------
 */

static int
ParseSubcommandOptions(optPtr, interp, allowedOptions, optIndexPtr, argc, argv)
    struct SubcommandOptions *optPtr;
				/* Information about the options specified
				 * and the values given is returned here. */
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    int allowedOptions;		/* Indicates which options are valid for
				 * the current command. */
    int *optIndexPtr;		/* Points to a variable containing the
				 * current index in argv; this variable is
				 * updated by this procedure. */
    int argc;			/* Number of arguments in argv[]. */
    const char **argv;		/* Arguments to be parsed. */
{
    int index, c, bit, currentBit;
    size_t length;
    char *option, **listPtr;
    int values[4];
    int numValues, maxValues, argIndex;

#ifdef DEBUG
    printf("ParseSubcommandOptions\n");
#endif

    for (index = *optIndexPtr; index < argc; *optIndexPtr = ++index) {
	/*
	 * We can have one value specified without an option;
	 * it goes into optPtr->name.
	 */

	option = argv[index];
	if (option[0] != '-') {
	    if (optPtr->name == NULL) {
		optPtr->name = option;
		continue;
	    }
	    break;
	}

	/*
	 * Work out which option this is.
	 */

	length = strlen(option);
	c = option[0];
	bit = 0;
	currentBit = 1;
	for (listPtr = optionNames; *listPtr != NULL; ++listPtr) {
	    if ((c == *listPtr[0])
		    && (strncmp(option, *listPtr, length) == 0)) {
		if (bit != 0) {
		    bit = 0;	/* An ambiguous option. */
		    break;
		}
		bit = currentBit;
	    }
	    currentBit <<= 1;
	}

	/*
	 * If this option is not recognized and allowed, put
	 * an error message in the interpreter and return.
	 */

	if ((allowedOptions & bit) == 0) {
	    Tcl_AppendResult(interp, "unrecognized option \"", argv[index],
		    "\": must be ", (char *)NULL);
	    bit = 1;
	    for (listPtr = optionNames; *listPtr != NULL; ++listPtr) {
		if ((allowedOptions & bit) != 0) {
		    if ((allowedOptions & (bit - 1)) != 0) {
			Tcl_AppendResult(interp, ", ", (char *) NULL);
			if ((allowedOptions & ~((bit << 1) - 1)) == 0) {
			    Tcl_AppendResult(interp, "or ", (char *) NULL);
			}
		    }
		    Tcl_AppendResult(interp, *listPtr, (char *) NULL);
		}
		bit <<= 1;
	    }
	    return TCL_ERROR;
	}

	/*
	 * For the -from, -to, -zoom and -subsample options,
	 * parse the values given.  Report an error if too few
	 * or too many values are given.
	 */

	if ((bit != OPT_SHRINK) && (bit != OPT_FORMAT)) {
	    maxValues = ((bit == OPT_FROM) || (bit == OPT_TO))? 4: 2;
	    argIndex = index + 1;
	    for (numValues = 0; numValues < maxValues; ++numValues) {
		if ((argIndex < argc) && (isdigit(UCHAR(argv[argIndex][0]))
			|| ((argv[argIndex][0] == '-')
			&& (isdigit(UCHAR(argv[argIndex][1])))))) {
		    if (Tcl_GetInt(interp, argv[argIndex], &values[numValues])
			    != TCL_OK) {
			return TCL_ERROR;
		    }
		} else {
		    break;
		}
		++argIndex;
	    }

	    if (numValues == 0) {
		Tcl_AppendResult(interp, "the \"", argv[index], "\" option ",
			 "requires one ", maxValues == 2? "or two": "to four",
			 " integer values", (char *) NULL);
		return TCL_ERROR;
	    }
	    *optIndexPtr = (index += numValues);

	    /*
	     * Y values default to the corresponding X value if not specified.
	     */

	    if (numValues == 1) {
		values[1] = values[0];
	    }
	    if (numValues == 3) {
		values[3] = values[2];
	    }

	    /*
	     * Check the values given and put them in the appropriate
	     * field of the SubcommandOptions structure.
	     */

	    switch (bit) {
		case OPT_FROM:
		    if ((values[0] < 0) || (values[1] < 0) || ((numValues > 2)
			    && ((values[2] < 0) || (values[3] < 0)))) {
			Tcl_AppendResult(interp, "value(s) for the -from",
				" option must be non-negative", (char *) NULL);
			return TCL_ERROR;
		    }
		    if (numValues <= 2) {
			optPtr->fromX = values[0];
			optPtr->fromY = values[1];
			optPtr->fromX2 = -1;
			optPtr->fromY2 = -1;
		    } else {
			optPtr->fromX = MIN(values[0], values[2]);
			optPtr->fromY = MIN(values[1], values[3]);
			optPtr->fromX2 = MAX(values[0], values[2]);
			optPtr->fromY2 = MAX(values[1], values[3]);
		    }
		    break;
		case OPT_SUBSAMPLE:
		    optPtr->subsampleX = values[0];
		    optPtr->subsampleY = values[1];
		    break;
		case OPT_TO:
		    if ((values[0] < 0) || (values[1] < 0) || ((numValues > 2)
			    && ((values[2] < 0) || (values[3] < 0)))) {
			Tcl_AppendResult(interp, "value(s) for the -to",
				" option must be non-negative", (char *) NULL);
			return TCL_ERROR;
		    }
		    if (numValues <= 2) {
			optPtr->toX = values[0];
			optPtr->toY = values[1];
			optPtr->toX2 = -1;
			optPtr->toY2 = -1;
		    } else {
			optPtr->toX = MIN(values[0], values[2]);
			optPtr->toY = MIN(values[1], values[3]);
			optPtr->toX2 = MAX(values[0], values[2]);
			optPtr->toY2 = MAX(values[1], values[3]);
		    }
		    break;
		case OPT_ZOOM:
		    if ((values[0] <= 0) || (values[1] <= 0)) {
			Tcl_AppendResult(interp, "value(s) for the -zoom",
				" option must be positive", (char *) NULL);
			return TCL_ERROR;
		    }
		    optPtr->zoomX = values[0];
		    optPtr->zoomY = values[1];
		    break;
	    }
	} else if (bit == OPT_FORMAT) {
	    /*
	     * The -format option takes a single string value.
	     */

	    if (index + 1 < argc) {
		*optIndexPtr = ++index;
		optPtr->format = argv[index];
	    } else {
		Tcl_AppendResult(interp, "the \"-format\" option ",
			"requires a value", (char *) NULL);
		return TCL_ERROR;
	    }
	}

	/*
	 * Remember that we saw this option.
	 */

	optPtr->options |= bit;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictConfigureMaster --
 *
 *	This procedure is called when a Pict image is created or
 *	reconfigured.  It processes configuration options and resets
 *	any instances of the image.
 *
 * Results:
 *	A standard Tcl return value.  If TCL_ERROR is returned then
 *	an error message is left in masterPtr->interp->result.
 *
 * Side effects:
 *	Existing instances of the image will be redisplayed to match
 *	the new configuration options.
 *
 *----------------------------------------------------------------------
 */

static int
ImgPictConfigureMaster(interp, masterPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    PictMaster *masterPtr;	/* Pointer to data structure describing
				 * overall Pict image to (re)configure. */
    int argc;			/* Number of entries in argv. */
    const char **argv;		/* Pairs of configuration options for image. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget,
				 * such as TK_CONFIG_ARGV_ONLY. */
{
    PictInstance *instancePtr;
    char *oldFileString, *oldDataString, *realFileName;
    int result;
    Tcl_Channel f;
    Tk_PictImageFormat *imageFormat;
    int imageWidth, imageHeight;
    Tcl_DString buffer;

#ifdef DEBUG
    printf("ImgPictConfigureMaster\n");
#endif

    /*
     * Save the current values for fileString and dataString, so we
     * can tell if the user specifies them anew.
     */

    oldFileString = masterPtr->fileString;
    oldDataString = (oldFileString == NULL)? masterPtr->dataString: NULL;

    /*
     * Process the configuration options specified.
     */

    if (Tk_ConfigureWidget(interp, Tk_MainWindow(interp), configSpecs,
	    argc, argv, (char *) masterPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Regard the empty string for -file, -data or -format as the null
     * value.
     */

    if ((masterPtr->fileString != NULL) && (masterPtr->fileString[0] == 0)) {
	ckfree(masterPtr->fileString);
	masterPtr->fileString = NULL;
    }
    if ((masterPtr->dataString != NULL) && (masterPtr->dataString[0] == 0)) {
	ckfree(masterPtr->dataString);
	masterPtr->dataString = NULL;
    }
    if ((masterPtr->format != NULL) && (masterPtr->format[0] == 0)) {
	ckfree(masterPtr->format);
	masterPtr->format = NULL;
    }

    /*
     * Set the image to the user-requested size, if any,
     * and make sure storage is correctly allocated for this image.
     */

    ImgPictSetSize(masterPtr, masterPtr->width, masterPtr->height);

    /*
     * Read in the image from the file or string if the user has
     * specified the -file or -data option.
     */

    if ((masterPtr->fileString != NULL)
	    && (masterPtr->fileString != oldFileString)) {

	realFileName = Tcl_TildeSubst(interp, masterPtr->fileString, &buffer);
	if (realFileName == NULL) { 
	  Tcl_AppendResult(interp, "No filename specified",(char*)NULL);
	  return TCL_ERROR;
	}
        f = Tcl_OpenFileChannel(interp, realFileName, "r", 0);
	Tcl_DStringFree(&buffer);
	if (f == NULL) {
	    Tcl_AppendResult(interp, "couldn't read image file \"",
		    masterPtr->fileString, "\": ", Tcl_PosixError(interp),
		    (char *) NULL);
	    return TCL_ERROR;
	}
	if (MatchFileFormat(interp, f, masterPtr->fileString,
		masterPtr->format, &imageFormat, &imageWidth,
		&imageHeight) != TCL_OK) {
	    Tcl_Close(interp, f);
	    return TCL_ERROR;
	}
	ImgPictSetSize(masterPtr, imageWidth, imageHeight);
	result = (*imageFormat->fileReadProc)(interp, f, masterPtr->fileString,
		Tcl_NewStringObj( masterPtr->format, -1 ),
                (Tk_PictHandle) masterPtr, 0, 0,
		imageWidth, imageHeight, 0, 0);

	Tcl_Close(interp,f);
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}

	masterPtr->flags |= IMAGE_CHANGED;
    }

    if ((masterPtr->fileString == NULL) && (masterPtr->dataString != NULL)
	    && (masterPtr->dataString != oldDataString)) {

	if (MatchStringFormat(interp, masterPtr->dataString, 
		masterPtr->format, &imageFormat, &imageWidth,
		&imageHeight) != TCL_OK) {
	    return TCL_ERROR;
	}
	ImgPictSetSize(masterPtr, imageWidth, imageHeight);
	if ((*imageFormat->stringReadProc)(interp,
                Tcl_NewStringObj( masterPtr->dataString, -1 ),
		Tcl_NewStringObj( masterPtr->format, -1 ),
                (Tk_PictHandle) masterPtr,
		0, 0, imageWidth, imageHeight, 0, 0) != TCL_OK) {
	    return TCL_ERROR;
	}

	masterPtr->flags |= IMAGE_CHANGED;
    }

    /*
     * Cycle through all of the instances of this image, regenerating
     * the information for each instance.  Then force the image to be
     * redisplayed everywhere that it is used.
     */

    for (instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	    instancePtr = instancePtr->nextPtr) {
	ImgPictConfigureInstance(instancePtr);
    }

    /*
     * Inform the generic image code that the image
     * has (potentially) changed.
     */

    Tk_ImageChanged(masterPtr->tkMaster, 0, 0, masterPtr->width,
	    masterPtr->height, masterPtr->width, masterPtr->height);
    masterPtr->flags &= ~IMAGE_CHANGED;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictConfigureInstance --
 *
 *	This procedure is called to create displaying information for
 *	a Pict image instance based on the configuration information
 *	in the master.  It is invoked both when new instances are
 *	created and when the master is reconfigured.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Generates errors via Tk_BackgroundError if there are problems
 *	in setting up the instance.
 *
 *----------------------------------------------------------------------
 */

static void
ImgPictConfigureInstance(instancePtr)
    PictInstance *instancePtr;	/* Instance to reconfigure. */
{
    PictMaster *masterPtr = instancePtr->masterPtr;
    Display *disp;
    XImage *imagePtr;
    int bitsPerPixel;
    XRectangle validBox;
    int new_image = 0;

#ifdef DEBUG
    printf("ImgPictConfigureInstance\n");
#endif

    /*
     * Create a new XImage structure for sending data to
     * the X server, if necessary.
     */
    disp = instancePtr->display;
    bitsPerPixel = instancePtr->visualInfo.depth;
    
    if ((instancePtr->imagePtr == NULL)
	|| (instancePtr->imagePtr->bits_per_pixel != bitsPerPixel)) {
      new_image = 1;
      if (instancePtr->imagePtr != NULL) {
	XFree((char *) instancePtr->imagePtr);
      }
      imagePtr = XCreateImage(disp,
			      instancePtr->visualInfo.visual,
			      (unsigned) bitsPerPixel,
			      (bitsPerPixel > 1? ZPixmap: XYBitmap), 
			      0, (char *) NULL,
			      1, 1, 32, 0);
      instancePtr->imagePtr = imagePtr;

      /*
       * Determine the endianness of this machine.
       * We create images using the local host's endianness, rather
       * than the endianness of the server; otherwise we would have
       * to byte-swap any 16 or 32 bit values that we store in the
       * image in those situations where the server's endianness
       * is different from ours.
       */
      
      if (imagePtr != NULL) {
	union {
	  int i;
	  char c[sizeof(int)];
	} kludge;
	
	imagePtr->bitmap_unit = sizeof(pixel) * NBBY;
	kludge.i = 0;
	kludge.c[0] = 1;
	imagePtr->byte_order = (kludge.i == 1) ? LSBFirst : MSBFirst;
	_XInitImageFuncPtrs(imagePtr);
      }
    }
  

    /*
     * If the user has specified a width and/or height for the master
     * which is different from our current width/height, set the size
     * to the values specified by the user.  If we have no pixmap, we
     * do this also, since it has the side effect of allocating a
     * pixmap for us.
     */

    if ((instancePtr->pixels == None)
	|| (instancePtr->width != masterPtr->width)
	|| (instancePtr->height != masterPtr->height)) {
      ImgPictInstanceSetSize(instancePtr);
    }

    /*
     * Redither this instance if necessary.
     */
    
    if ((masterPtr->flags & IMAGE_CHANGED)
	|| (new_image == 1)) {
      XClipBox(masterPtr->validRegion, &validBox);
      if ((validBox.width > 0) && (validBox.height > 0)) {
	DitherInstance(instancePtr, validBox.x, validBox.y,
		       validBox.width, validBox.height);
      }
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictGet --
 *
 *	This procedure is called for each use of a Pict image in a
 *	widget.
 *
 * Results:
 *	The return value is a token for the instance, which is passed
 *	back to us in calls to ImgPictDisplay and ImgPictFree.
 *
 * Side effects:
 *	A data structure is set up for the instance (or, an existing
 *	instance is re-used for the new one).
 *
 *----------------------------------------------------------------------
 */

static ClientData ImgPictGet(tkwin, masterData)
    Tk_Window tkwin;		/* Window in which the instance will be
				 * used. */
    ClientData masterData;	/* Pointer to our master structure for the
				 * image. */
{
  PictMaster *masterPtr = (PictMaster *) masterData;
  PictInstance *instancePtr;
  PictColorTable *colorTable;
  XVisualInfo *visInfoPtr;
  XRectangle validBox;
  XColor *white, *black;
  XGCValues gcValues;
  
#ifdef DEBUG
  printf("ImgPictGet\n");
#endif

  if( GetColorTable(tkwin,(PictColorTable **)&colorTable,(XVisualInfo **)&visInfoPtr) == 0 ) {
    fprintf(stderr," GetColorTable failed \n");
    return 0;
  }

  /*
   * Make a new instance of the image.
   */
  instancePtr = (PictInstance *) ckalloc(sizeof(PictInstance));
  if(instancePtr == NULL) {
    (void)fprintf(stderr,"ImgPictGet: Could not allocate memory\n");
    return 0;
  }

  instancePtr->tkwin = tkwin;
  instancePtr->masterPtr = masterPtr;
  instancePtr->display = colorTable->display;
  instancePtr->colormap = colorTable->colormap;
  instancePtr->colormap_level = colorTable->colormap_level;
  instancePtr->has_overlay = 0;
  instancePtr->refCount = 1;
  instancePtr->pixels = None;
  instancePtr->width = 0;
  instancePtr->height = 0;
  instancePtr->imagePtr = 0;
  instancePtr->colorTable = colorTable;
  instancePtr->nextPtr = masterPtr->instancePtr;
  masterPtr->instancePtr = instancePtr;
  instancePtr->visualInfo = *visInfoPtr;
  XFree(visInfoPtr);
 
  /*
   * Make a GC with background = black and foreground = white.
   */

  white = Tk_GetColor(masterPtr->interp, tkwin, "white");
  black = Tk_GetColor(masterPtr->interp, tkwin, "black");
  gcValues.foreground = (white != NULL)? white->pixel:
  WhitePixelOfScreen(Tk_Screen(tkwin));
  gcValues.background = (black != NULL)? black->pixel:
  BlackPixelOfScreen(Tk_Screen(tkwin));
  gcValues.graphics_exposures = False;
  instancePtr->gc = Tk_GetGC(tkwin,
			     GCForeground|GCBackground|GCGraphicsExposures, &gcValues);
  instancePtr->setgc = GXcopy;
  /* Set configuration options and finish the initialization 
     of the instance. */
  ImgPictConfigureInstance(instancePtr);
  
  /* If this is the first instance, must set the size of the image. */
  if (instancePtr->nextPtr == NULL) {
    Tk_ImageChanged(masterPtr->tkMaster, 0, 0, 0, 0,
		    masterPtr->width, masterPtr->height);
  }
  
  /* If we have no pixmap, we do this also, since it has the side
     effect of allocating a pixmap for us.  */
  if (instancePtr->pixels == None) {
    XClipBox(masterPtr->validRegion, &validBox);
    if ((validBox.width > 0) && (validBox.height > 0)) {
      DitherInstance(instancePtr, validBox.x, validBox.y, validBox.width,
		     validBox.height);
    }
  }

  return (ClientData) instancePtr;
} /* end ImgPictGet */


/*
 *----------------------------------------------------------------------
 *
 * ImgPictDisplay --
 *
 *	This procedure is invoked to draw a Pict image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A portion of the image gets rendered in a pixmap or window.
 *
 *----------------------------------------------------------------------
 */
static
void ImgPictDisplay(clientData, display, drawable, imageX, imageY, width,
	height, drawableX, drawableY)
    ClientData clientData;	/* Pointer to PictInstance structure for
				 * for instance to be displayed. */
    Display *display;		/* Display on which to draw image. */
    Drawable drawable;		/* Pixmap or window in which to draw image. */
    int imageX, imageY;		/* Upper-left corner of region within image
				 * to draw. */
    int width, height;		/* Dimensions of region within image to draw. */
    int drawableX, drawableY;	/* Coordinates within drawable that
				 * correspond to imageX and imageY. */
{
    PictInstance *instancePtr = (PictInstance *) clientData;

#ifdef DEBUG
    printf("ImgPictDisplay\n");
#endif

    /*
     * If there's no pixmap, it means that an error occurred
     * while creating the image instance so it can't be displayed.
     */

    if (instancePtr->pixels == None) {
	return;
    }

    /*
     * masterPtr->region describes which parts of the image contain
     * valid data.  We set this region as the clip mask for the gc,
     * setting its origin appropriately, and use it when drawing the
     * image.
     */

    XSetRegion(display, instancePtr->gc, instancePtr->masterPtr->validRegion);
    XSetClipOrigin(display, instancePtr->gc, drawableX - imageX,
	    drawableY - imageY);
    XCopyArea(display, instancePtr->pixels, drawable, instancePtr->gc,
	    imageX, imageY, (unsigned) width, (unsigned) height,
	    drawableX, drawableY);
    XSetClipMask(display, instancePtr->gc, None);
    XSetClipOrigin(display, instancePtr->gc, 0, 0);
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictFree --
 *
 *	This procedure is called when a widget ceases to use a
 *	particular instance of an image.  We don't actually get
 *	rid of the instance until later because we may be about
 *	to get this instance again.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Internal data structures get cleaned up, later.
 *
 *----------------------------------------------------------------------
 */
static
void ImgPictFree(clientData, display)
    ClientData clientData;	/* Pointer to PictInstance structure for
				 * for instance to be displayed. */
    Display *display;		/* Display containing window that used image. */
{
    PictInstance *instancePtr = (PictInstance *) clientData;

#ifdef DEBUG
    printf("ImgPictFree\n");
#endif

    instancePtr->refCount -= 1;
    if (instancePtr->refCount > 0) {
	return;
    }

    /* There are no more uses of the image within this widget.
       free the instance structure. */
    DisposeInstance((ClientData) instancePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictDelete --
 *
 *	This procedure is called by the image code to delete the
 *	master structure for an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with the image get freed.
 *
 *----------------------------------------------------------------------
 */
static
void ImgPictDelete(masterData)
    ClientData masterData;	/* Pointer to PictMaster structure for
				 * image.  Must not have any more instances. */
{
    PictMaster *masterPtr = (PictMaster *) masterData;
    PictInstance *instancePtr;

#ifdef DEBUG
    printf("ImgPictDelete\n");
#endif

    while ((instancePtr = masterPtr->instancePtr) != NULL) {
	if (instancePtr->refCount > 0) {
	    panic("tried to delete Pict image when instances still exist");
	}
#if (TK_MINOR_VERSION == 0)
	Tk_CancelIdleCall(DisposeInstance, (ClientData) instancePtr);
#else
	Tcl_CancelIdleCall(DisposeInstance, (ClientData) instancePtr);
#endif
	DisposeInstance((ClientData) instancePtr);
    }
    masterPtr->tkMaster = NULL;
    if (masterPtr->imageCmd != NULL) {
	Tcl_DeleteCommand(masterPtr->interp,
		Tcl_GetCommandName(masterPtr->interp, masterPtr->imageCmd));
    }
    if (masterPtr->data != NULL) {
      if( (char*)(masterPtr->data) == (char*)(masterPtr->bytedata) ) {
	ckfree((char *) masterPtr->data);
	masterPtr->data = NULL;
	masterPtr->bytedata = NULL;
      } else {
	ckfree((char *) masterPtr->data);
	masterPtr->data = NULL;
      }
    }
    if (masterPtr->bytedata != NULL) {
      ckfree((char *) masterPtr->bytedata);
    }

    if (masterPtr->validRegion != NULL) {
	XDestroyRegion(masterPtr->validRegion);
    }
    Tk_FreeOptions(configSpecs, (char *) masterPtr, (Display *) NULL, 0);
    ckfree((char *) masterPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictCmdDeletedProc --
 *
 *	This procedure is invoked when the image command for an image
 *	is deleted.  It deletes the image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image is deleted.
 *
 *----------------------------------------------------------------------
 */

static void
ImgPictCmdDeletedProc(clientData)
    ClientData clientData;	/* Pointer to PictMaster structure for
				 * image. */
{
    PictMaster *masterPtr = (PictMaster *) clientData;

#ifdef DEBUG
    printf("ImgPictCmdDeletedProc\n");
#endif

    masterPtr->imageCmd = NULL;
    if (masterPtr->tkMaster != NULL) {
	Tk_DeleteImage(masterPtr->interp, Tk_NameOfImage(masterPtr->tkMaster));
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictSetSize --
 *
 *	This procedure reallocates the image storage and instance
 *	pixmaps for a Pict image, as necessary, to change the
 *	image's size to `width' x `height' pixels.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Storage gets reallocated, for the master and all its instances.
 *
 *----------------------------------------------------------------------
 */

static void
ImgPictSetSize(masterPtr, width, height)
    PictMaster *masterPtr;
    int width, height;
{
  char *newData;
  int h, offset, pitch;
  char *srcPtr, *destPtr;
  XRectangle validBox, clipBox;
  Region clipRegion;
  PictInstance *instancePtr;

#ifdef DEBUG
    printf("ImgPictSetSize\n");
#endif

    if (masterPtr->userWidth > 0) {
	width = masterPtr->userWidth;
    }
    if (masterPtr->userHeight > 0) {
	height = masterPtr->userHeight;
    }

    /*
     * We have to trim the valid region if it is currently
     * larger than the new image size.
     */

    XClipBox(masterPtr->validRegion, &validBox);
    if ((validBox.x + validBox.width > (unsigned) width)
	    || (validBox.y + validBox.height > (unsigned) height)) {
	clipBox.x = 0;
	clipBox.y = 0;
	clipBox.width = width;
	clipBox.height = height;
	clipRegion = XCreateRegion();
	XUnionRectWithRegion(&clipBox, clipRegion, clipRegion);
	XIntersectRegion(masterPtr->validRegion, clipRegion,
		masterPtr->validRegion);
	XDestroyRegion(clipRegion);
	XClipBox(masterPtr->validRegion, &validBox);
    }

    if ((width != masterPtr->width) || (height != masterPtr->height)) {
      if( masterPtr->data == NULL ) {
	masterPtr->width = width;
	masterPtr->height = height;
      }
      else {
	
	/*
	 * Reallocate storage for the byte image and copy
	 * over valid regions.
	 */
	
	pitch = width;
	newData = (char *) ckalloc((unsigned) (height * pitch*masterPtr->datasize));
	if(newData == NULL) {
	  (void)fprintf(stderr,"ImgPictSetSize: Could not allocate memory\n");
	  return;
	}
	/*
	 * Zero the new array.  The dithering code shouldn't read the
	 * areas outside validBox, but they might be copied to another
	 * Pict image or written to a file.
	 */
	
	if ((masterPtr->data != NULL)
	    && ((width == masterPtr->width) || (width == validBox.width))) {
	  if (validBox.y > 0) {
	    memset((VOID *) newData, 0, (size_t) (validBox.y * pitch*masterPtr->datasize));
	  }
	  h = validBox.y + validBox.height;
	  if (h < height) {
	    memset((VOID *) (newData + h * pitch), 0,
			(size_t) ((height - h) * pitch));
	  }
	} else {
	  memset((VOID *) newData, 0, (size_t) (height * pitch*masterPtr->datasize));
	}
	
	if (masterPtr->data != NULL) {
	  
	  /*
	   * Copy the common area over to the new array array and
	   * free the old array.
	   */
	  
	    if (width == masterPtr->width) {
	      
		/*
		 * The region to be copied is contiguous.
		 */
	      
		offset = validBox.y * pitch;
		memcpy((VOID *) (newData + offset),
		       (VOID *) (masterPtr->data + offset),
		       (size_t) (validBox.height * pitch*masterPtr->datasize));
		
	    } else if ((validBox.width > 0) && (validBox.height > 0)) {
	      
		/*
		 * Area to be copied is not contiguous - copy line by line.
		 */

	      destPtr = newData + (validBox.y * width + validBox.x)*masterPtr->datasize;
	      srcPtr = masterPtr->data + (validBox.y * masterPtr->width
					  + validBox.x)*masterPtr->datasize;
	      for (h = validBox.height; h > 0; h--) {
		memcpy((VOID *) destPtr, (VOID *) srcPtr,
			    (size_t) (validBox.width*masterPtr->datasize));
		destPtr += width*masterPtr->datasize ;
		srcPtr += masterPtr->width*masterPtr->datasize;
	      }
	    }
	    if (masterPtr->data != NULL) {
	      if ((char*)(masterPtr->data) == (char*)(masterPtr->bytedata)) {
		free((void*)masterPtr->data);
		masterPtr->data = NULL;
		masterPtr->bytedata = NULL;
	      } else {
		free((void*)masterPtr->data);
		masterPtr->data = NULL;
		free((void*)masterPtr->bytedata);
		masterPtr->bytedata = NULL;
	      }
	    }
	  }
	masterPtr->data = newData;
	masterPtr->width = width;
	masterPtr->height = height;
	normalize_data(masterPtr);
      } 
    }

  /*
   * Now adjust the sizes of the pixmaps for all of the instances.
   */
  
  for (instancePtr = masterPtr->instancePtr; instancePtr != NULL;
       instancePtr = instancePtr->nextPtr) {
    ImgPictInstanceSetSize(instancePtr);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictInstanceSetSize --
 *
 * 	This procedure reallocates the instance pixmap and dithering
 *	error array for a Pict instance, as necessary, to change the
 *	image's size to `width' x `height' pixels.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Storage gets reallocated, here and in the X server.
 *
 *----------------------------------------------------------------------
 */

static void
ImgPictInstanceSetSize(instancePtr)
    PictInstance *instancePtr;		/* Instance whose size is to be
					 * changed. */
{
    PictMaster *masterPtr;
    XRectangle validBox;
    Pixmap newPixmap;

#ifdef DEBUG
    printf("ImgPictInstanceSetSize\n");
#endif

    masterPtr = instancePtr->masterPtr;
    XClipBox(masterPtr->validRegion, &validBox);

    if ((instancePtr->width != masterPtr->width)
	    || (instancePtr->height != masterPtr->height)
	    || (instancePtr->pixels == None)) {
	newPixmap = Tk_GetPixmap(instancePtr->display,
		RootWindow(instancePtr->display,
		    instancePtr->visualInfo.screen),
	(masterPtr->width > 0) ? masterPtr->width: 1,
		(masterPtr->height > 0) ? masterPtr->height: 1,
		instancePtr->visualInfo.depth);

	if (instancePtr->pixels != None) {
	    /*
	     * Copy any common pixels from the old pixmap and free it.
	     */
	    XCopyArea(instancePtr->display, instancePtr->pixels, newPixmap,
		    instancePtr->gc, validBox.x, validBox.y,
		    validBox.width, validBox.height, validBox.x, validBox.y);
	    Tk_FreePixmap(instancePtr->display, instancePtr->pixels);
	}
	instancePtr->pixels = newPixmap;
    }

    instancePtr->width = masterPtr->width;
    instancePtr->height = masterPtr->height;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgPictCopy
 *
 * 	This procedure copies data from one image to another
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      The contents of the image is replaced by what's been read.
 *
 *----------------------------------------------------------------------
 */

static int ImgPictCopy(Tcl_Interp *interp,
		       PictMaster *masterPtr,
		       int argc,
		       const char **argv)
{
  int index;
  int width, height;
  struct SubcommandOptions options;
  Tk_PictImageBlock block;
  Tk_PictHandle srcHandle;

  /*
   * Pict copy command - first parse options.
   */
  
  index = 2;
  memset((VOID *) &options, 0, sizeof(options));
  options.zoomX = options.zoomY = 1;
  options.subsampleX = options.subsampleY = 1;
  options.name = NULL;
  if (ParseSubcommandOptions(&options, interp,
			     OPT_FROM | OPT_TO | OPT_ZOOM | 
			     OPT_SUBSAMPLE | OPT_SHRINK,
			     &index, argc, argv) != TCL_OK) {
    return TCL_ERROR;
  }
  if (options.name == NULL || index < argc) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " copy source-image ?-from x1 y1 x2 y2?",
		     " ?-to x1 y1 x2 y2? ?-zoom x y? ?-subsample x y?",
		     "\"", (char *) NULL);
    return TCL_ERROR;
  }
  
  /*
   * Look for the source image and get a pointer to its image data.
   * Check the values given for the -from option.
   */
  
  if ((srcHandle = Tk_FindPict(options.name)) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  Tk_PictGetImage(srcHandle, &block);
  if ((options.fromX2 > block.width) || (options.fromY2 > block.height)
      || (options.fromX2 > block.width)
      || (options.fromY2 > block.height)) {
       Tcl_AppendResult(interp, "coordinates for -from option extend ",
			"outside source image", (char *) NULL);
       return TCL_ERROR;
     }
  
  /*
   * Fill in default values for unspecified parameters.
   */
  
  if (((options.options & OPT_FROM) == 0) || (options.fromX2 < 0)) {
    options.fromX2 = block.width;
    options.fromY2 = block.height;
  }
  if (((options.options & OPT_TO) == 0) || (options.toX2 < 0)) {
    width = options.fromX2 - options.fromX;
    if (options.subsampleX > 0) {
      width = (width + options.subsampleX - 1) / options.subsampleX;
    } else if (options.subsampleX == 0) {
      width = 0;
    } else {
      width = (width - options.subsampleX - 1) / -options.subsampleX;
    }
    options.toX2 = options.toX + width * options.zoomX;
    
    height = options.fromY2 - options.fromY;
    if (options.subsampleY > 0) {
      height = (height + options.subsampleY - 1)
	/ options.subsampleY;
    } else if (options.subsampleY == 0) {
      height = 0;
    } else {
      height = (height - options.subsampleY - 1)
	/ -options.subsampleY;
    }
    options.toY2 = options.toY + height * options.zoomY;
  }
  
  /*
   * Set the destination image size if the -shrink option was specified.
   */
  
  if (options.options & OPT_SHRINK) {
    ImgPictSetSize(masterPtr, options.toX2, options.toY2);
  }
  
  /*
   * Copy the image data over using Tk_PictPutZoomedBlock.
   */
	
  block.pixelPtr += (options.fromX + options.fromY * block.pitch) 
    * block.pixelSize;
  block.width = options.fromX2 - options.fromX;
  block.height = options.fromY2 - options.fromY;
  Tk_PictPutZoomedBlock((Tk_PictHandle) masterPtr, &block,
			options.toX, options.toY, 
			options.toX2 - options.toX,
			options.toY2 - options.toY, 
			options.zoomX, options.zoomY,
			options.subsampleX, options.subsampleY);
  return TCL_OK;
  
} /* end ImgPictCopy */

/*
 *----------------------------------------------------------------------
 *
 * ImgPictSnap2Photo --
 *
 *	This procedure is used for snapshots of a pict image. The result
 *      is stored as a photo image.
 *     
 * Results:
 *	None.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

int ImgPictSnap2Photo(Tcl_Interp *interp,
		      PictMaster *masterPtr,
		      int argc,
		      const char **argv)
{
  void *destHandle;
  int i,j,col;
  Tk_PhotoImageBlock photoblock;
  PictInstance *instancePtr=masterPtr->instancePtr;
  PictColorTable *colorTable;
  int *red,*green,*blue;
  int *intensity_lut,*red_lut,*green_lut,*blue_lut;
  
  if ((destHandle = Tk_FindPhoto(interp,argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Photo image", (char *) NULL);
    return TCL_ERROR;
  }
	
  /* set size */
  Tk_PhotoExpand(interp, destHandle,masterPtr->width,masterPtr->height);

  /* copy data from pict image to photo image. If no instance
     exists, no colors have been allocated, so we copy the
     original master data. Otherwise, we copy them using the
     color lookup tables in a 24-bit image. */
  
  if( masterPtr->instancePtr == NULL ) {
    photoblock.pixelSize = 1;
    photoblock.offset[0] = 0;
    photoblock.offset[1] = 0;
    photoblock.offset[2] = 0;
    
    photoblock.width = masterPtr->width;
    photoblock.height = masterPtr->height;
    
    photoblock.pitch = photoblock.pixelSize * masterPtr->width;
    
    /* allocate storage */
    photoblock.pixelPtr = (unsigned char*)ckalloc((size_t)
						 (masterPtr->width)*
						 (masterPtr->height));
    if ( photoblock.pixelPtr == NULL) {
      Tcl_AppendResult(interp, "Cannot allocate memory in snap2photo ",
		       (char*)NULL);
      return TCL_ERROR;
    }
    memcpy((void*)(photoblock.pixelPtr),
	   (void*)(masterPtr->bytedata),
	   (size_t)(masterPtr->width)*(masterPtr->height));
  }
  else { /* produce a 24-bit image */
       
    photoblock.pixelSize = 3;
    photoblock.offset[0] = 0;
    photoblock.offset[1] = 1;
    photoblock.offset[2] = 2;
    
    photoblock.width = masterPtr->width;
    photoblock.height = masterPtr->height;
    
    photoblock.pitch = photoblock.pixelSize * (masterPtr->width);
    
    /* allocate storage */
    photoblock.pixelPtr = (unsigned char*)ckalloc((size_t)3*
						 (masterPtr->width)*
						 (masterPtr->height));
    if ( photoblock.pixelPtr == NULL) {
      Tcl_AppendResult(interp, "Cannot allocate memory in snap2photo ",
		       (char*)NULL);
      return TCL_ERROR;
    }
    /* get the colors */
    colorTable = instancePtr->colorTable;
    red = colorTable->red;     
    red_lut = colorTable->red_lut;
    green = colorTable->green; 
    green_lut = colorTable->green_lut;
    blue = colorTable->blue;   
    blue_lut = colorTable->blue_lut;
    intensity_lut = colorTable->intensity_lut;
    
    if( instancePtr->has_overlay == 0 ) {
      for(i=0,j=0;i<(masterPtr->width)*(masterPtr->height);i++) {
	/* map from [0-MAX_COLORS-1] to [0-(colorTable->ncolors-1)] */
	col = (double)(masterPtr->bytedata[i])*
	  (double)((colorTable->ncolors-1))/
	    (double)(MAX_COLORS-1);
	
	/* fill pixels of 24-bit image */
	photoblock.pixelPtr[j++] = red_lut[red[intensity_lut[col]]];
	photoblock.pixelPtr[j++] = green_lut[green[intensity_lut[col]]];
	photoblock.pixelPtr[j++] = blue_lut[blue[intensity_lut[col]]];
      }
    } else { /* instead of using a XGetImage call, we re-calculate the color
		values. tedious but no need for a server call */
      
      Tcl_AppendResult(interp, "Overlays not supported. How did you get here?",(char *) NULL);
	return TCL_ERROR;
    } /* end else if overlays */
  }
  
  /* put block in photo image */
  Tk_PhotoPutBlock(interp, destHandle,&photoblock,0,0,masterPtr->width,masterPtr->height, TK_PHOTO_COMPOSITE_SET);
  
  /* free photo block */
  ckfree((void*)photoblock.pixelPtr);
  return TCL_OK;
} /* end ImgPictSnap2Photo */

/*
 *----------------------------------------------------------------------
 *
 * ImgPictSnap2Pict --
 *
 *	This procedure is used for snapshots of a pict image. The result
 *      is stored as a pict image after NTSC conversion.
 *     
 * Results:
 *	None.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

int ImgPictSnap2Pict(Tcl_Interp *interp,
		      PictMaster *masterPtr,
		      int argc,
		      const char **argv)
{
  PictMaster *destmasterPtr;
  Tk_PictHandle destHandle;
  Tk_PictImageBlock *tmp_block;
  int i,j,col;

  /* Basically the same thing as snap2photo, plus the conversion
     to gray, but we can use Tk_PictExpand and Tk_PictPutBlock */
  
  if ((destHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  
  destmasterPtr = (PictMaster*)destHandle;
  
  /* allocate storage */
  tmp_block = (Tk_PictImageBlock*)ckalloc(sizeof(Tk_PictImageBlock));
  if ( tmp_block == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in snap2pict ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  tmp_block->pixelPtr = (unsigned char*)ckalloc((size_t)
					       (masterPtr->width)*
					       (masterPtr->height));
  tmp_block->width = masterPtr->width;
  tmp_block->height = masterPtr->height;
  
  tmp_block->datatype = BYTE;
  tmp_block->pixelSize = sizeof(unsigned char);
  tmp_block->pitch = tmp_block->pixelSize * tmp_block->width;
  tmp_block->copy = NO_COPY; 
  tmp_block->skip = 0;
  tmp_block->pixel_x = masterPtr->pixel_x;
  tmp_block->pixel_y = masterPtr->pixel_y;
  
  /* copy data from pict image to pict image. If no instance
     exists, no colors have been allocated, so we copy the
     original master byte data. Otherwise, we copy them using the
     color lookup tables */
  if( masterPtr->instancePtr == NULL ) {
    (void)memcpy((char*)(tmp_block->pixelPtr),
		 (char*)(masterPtr->bytedata),
		 (masterPtr->width)*(masterPtr->height));
  }
  else {
    PictInstance *instancePtr=masterPtr->instancePtr;
    PictColorTable *colorTable;
    int *red,*green,*blue;
    int *intensity_lut,*red_lut,*green_lut,*blue_lut;
    
    colorTable = instancePtr->colorTable;
    red = colorTable->red;     
    red_lut = colorTable->red_lut;
    green = colorTable->green; 
    green_lut = colorTable->green_lut;
    blue = colorTable->blue;   
    blue_lut = colorTable->blue_lut;
    intensity_lut = colorTable->intensity_lut;
    

    if( instancePtr->has_overlay == 0 ) {
      for(i=0,j=0;i<masterPtr->width*masterPtr->height;i++) {
	/* map from [0-MAX_COLORS-1] to [0-(colorTable->ncolors-1)] */
	col = (double)(masterPtr->bytedata[i])*
	  (double)(colorTable->ncolors-1)/
	    (double)(MAX_COLORS-1);
	
	tmp_block->pixelPtr[i] =  GRAY((red_lut[red[intensity_lut[col]]]),
				       (green_lut[green[intensity_lut[col]]]),
				       (blue_lut[blue[intensity_lut[col]]]));
      }
    } else {
	    
      
      Tcl_AppendResult(interp, "Overlays not supported. How did you get here?",(char *) NULL);
      return TCL_ERROR;
    } /* end else if overlays */
  }
  
  Tk_PictExpand(destHandle,masterPtr->width,masterPtr->height);
  Tk_PictPutBlock(destHandle,tmp_block,0,0,masterPtr->width,masterPtr->height); 
  Tk_ImageChanged(destmasterPtr->tkMaster,0,0,
		  destmasterPtr-> width, 
		  destmasterPtr->height, 
		  destmasterPtr->width,
		  destmasterPtr->height);
  ckfree((void*)tmp_block);
  return TCL_OK;
} /* end ImgPictSnap2Pict */

/*
 *----------------------------------------------------------------------
 *
 * DisposeInstance --
 *
 *	This procedure is called to finally free up an instance
 *	of a Pict image which is no longer required.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The instance data structure and the resources it references
 *	are freed.
 *
 *----------------------------------------------------------------------
 */

void
DisposeInstance(clientData)
    ClientData clientData;	/* Pointer to the instance whose resources
				 * are to be released. */
{
    PictInstance *instancePtr = (PictInstance *) clientData;
    PictInstance *prevPtr;

#ifdef DEBUG
    printf("DisposeInstance\n");
#endif
    if(instancePtr->has_overlay) {
      XFreeGC(instancePtr->display,instancePtr->overlay_gc);
      instancePtr->has_overlay = False;
    }

    if (instancePtr->pixels != None) {
	Tk_FreePixmap(instancePtr->display, instancePtr->pixels);
    }
    if (instancePtr->gc != None) {
	Tk_FreeGC(instancePtr->display, instancePtr->gc);
    }
    if (instancePtr->imagePtr != NULL) {
	XFree((char *) instancePtr->imagePtr);
    }
    
    instancePtr->colorTable->refCount --;
    if( instancePtr->colorTable->refCount == 0 ) {
      (void)DisposeColorTable(instancePtr->colorTable);
      instancePtr->colorTable = NULL;
    }

    if (instancePtr->masterPtr->instancePtr == instancePtr) {
	instancePtr->masterPtr->instancePtr = instancePtr->nextPtr;
    } else {
	for (prevPtr = instancePtr->masterPtr->instancePtr;
		prevPtr->nextPtr != instancePtr; prevPtr = prevPtr->nextPtr) {
	    /* Empty loop body */
	}
	prevPtr->nextPtr = instancePtr->nextPtr;
    }
    ckfree((char *) instancePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * MatchFileFormat --
 *
 *	This procedure is called to find a Pict image file format
 *	handler which can parse the image data in the given file.
 *	If a user-specified format string is provided, only handlers
 *	whose names match a prefix of the format string are tried.
 *
 * Results:
 *	A standard TCL return value.  If the return value is TCL_OK, a
 *	pointer to the image format record is returned in
 *	*imageFormatPtr, and the width and height of the image are
 *	returned in *widthPtr and *heightPtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
MatchFileFormat(interp, f, fileName, formatString, imageFormatPtr,
	widthPtr, heightPtr)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    Tcl_Channel f;   	        /* The image file, open for reading. */
    char *fileName;		/* The name of the image file. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PictImageFormat **imageFormatPtr;
				/* A pointer to the Pict image format
				 * record is returned here. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here. */
{
    int matched;
    Tk_PictImageFormat *formatPtr;

#ifdef DEBUG
    printf("MatchFileFormat\n");
#endif

    /*
     * Scan through the table of file format handlers to find
     * one which can handle the image.
     */

    matched = 0;
    for (formatPtr = formatList; formatPtr != NULL;
	 formatPtr = formatPtr->nextPtr) {
	if ((formatString != NULL)
		&& (strncasecmp(formatString, formatPtr->name,
		strlen(formatPtr->name)) != 0)) {
	    continue;
	}
	matched = 1;
	if (formatPtr->fileMatchProc != NULL) {
	    Tcl_Seek(f, 0L, SEEK_SET);
	    if ((*formatPtr->fileMatchProc)(f, fileName,
                    Tcl_NewStringObj( formatString, -1 ),
		    widthPtr, heightPtr, interp)) {
		if (*widthPtr < 1) {
		    *widthPtr = 1;
		}
		if (*heightPtr < 1) {
		    *heightPtr = 1;
		}
		break;
	    }
	}
    }

    if (formatPtr == NULL) {
	if ((formatString != NULL) && !matched) {
	    Tcl_AppendResult(interp, "image file format \"", formatString,
		    "\" is unknown", (char *) NULL);
	} else {
	    Tcl_AppendResult(interp,
		    "couldn't recognize data in image file \"",
		    fileName, "\"", (char *) NULL);
	}
	return TCL_ERROR;
    }

    *imageFormatPtr = formatPtr;
    Tcl_Seek(f, 0L, SEEK_SET);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MatchStringFormat --
 *
 *	This procedure is called to find a Pict image file format
 *	handler which can parse the image data in the given string.
 *	If a user-specified format string is provided, only handlers
 *	whose names match a prefix of the format string are tried.
 *
 * Results:
 *	A standard TCL return value.  If the return value is TCL_OK, a
 *	pointer to the image format record is returned in
 *	*imageFormatPtr, and the width and height of the image are
 *	returned in *widthPtr and *heightPtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
MatchStringFormat(interp, string, formatString, imageFormatPtr,
	widthPtr, heightPtr)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    char *string;		/* String containing the image data. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PictImageFormat **imageFormatPtr;
				/* A pointer to the Pict image format
				 * record is returned here. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here. */
{
    int matched;
    Tk_PictImageFormat *formatPtr;

#ifdef DEBUG
    printf("MatchStringFormat\n");
#endif

    /*
     * Scan through the table of file format handlers to find
     * one which can handle the image.
     */
    matched = 0;
    for (formatPtr = formatList; formatPtr != NULL;
	    formatPtr = formatPtr->nextPtr) {
	if ((formatString != NULL) && (strncasecmp(formatString,
		formatPtr->name, strlen(formatPtr->name)) != 0)) {
	    continue;
	}
	matched = 1;
	if ((formatPtr->stringMatchProc != NULL)
		&& (*formatPtr->stringMatchProc)(
                    Tcl_NewStringObj( string, -1 ),
                    Tcl_NewStringObj( formatString, -1 ),
                    widthPtr, heightPtr, interp)) {
	    break;
	}
    }

    if (formatPtr == NULL) {
	if ((formatString != NULL) && !matched) {
	    Tcl_AppendResult(interp, "image file format \"", formatString,
		    "\" is unknown", (char *) NULL);
	} else {
	    Tcl_AppendResult(interp, "no format found to parse",
		    " image data string", (char *) NULL);
	}
	return TCL_ERROR;
    }

    *imageFormatPtr = formatPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_FindPict --
 *
 *	This procedure is called to get an opaque handle (actually a
 *	PictMaster *) for a given image, which can be used in
 *	subsequent calls to Tk_PictPutBlock, etc.  The `name'
 *	parameter is the name of the image.
 *
 * Results:
 *	The handle for the Pict image, or NULL if there is no
 *	Pict image with the name given.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tk_PictHandle
Tk_FindPict(imageName)
    char *imageName;		/* Name of the desired Pict image. */
{
    Tcl_HashEntry *entry;

#ifdef DEBUG
    printf("Tk_FindPict\n");
#endif

    if (!imgPictHashInitialized) {
	return NULL;
    }
    entry = Tcl_FindHashEntry(&imgPictHash, imageName);
    if (entry == NULL) {
	return NULL;
    }
    return (Tk_PictHandle) Tcl_GetHashValue(entry);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictPutBlock --
 *
 *	This procedure is called to put image data into a Pict image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image data is stored.  The image may be expanded.
 *	The Tk image code is informed that the image has changed.
 *
 *---------------------------------------------------------------------- */

void
Tk_PictPutBlock(handle, blockPtr, x, y, width, height)
    Tk_PictHandle handle;	/* Opaque handle for the Pict image
				 * to be updated. */
    register Tk_PictImageBlock *blockPtr;
				/* Pointer to a structure describing the
				 * pixel data to be copied into the image. */
    int x, y;			/* Coordinates of the top-left pixel to
				 * be updated in the image. */
    int width, height;		/* Dimensions of the area of the image
				 * to be updated. */
{
    register PictMaster *masterPtr;
    PictInstance *instancePtr;
    int xEnd, yEnd;    
    XRectangle rect;
  
    int i,j;

#ifdef DEBUG
    printf("Tk_PictPutBlock\n");
#endif

    masterPtr = (PictMaster *) handle;

    if ((masterPtr->userWidth != 0) && ((x + width) > masterPtr->userWidth)) {
	width = masterPtr->userWidth - x;
    }
    if ((masterPtr->userHeight != 0)
	    && ((y + height) > masterPtr->userHeight)) {
	height = masterPtr->userHeight - y;
    }
    if ((width <= 0) || (height <= 0))
	return;
   
    xEnd = x + width;
    yEnd = y + height;

    if ((xEnd > masterPtr->width) || (yEnd > masterPtr->height)) {
	ImgPictSetSize(masterPtr, MAX(xEnd, masterPtr->width),
		MAX(yEnd, masterPtr->height));
    }
   
    if((x!= 0) || 
       (y!= 0) || 
       (masterPtr->width != blockPtr->width) ||
       (masterPtr->height != blockPtr->height)) {
#ifdef DEBUG
      printf(" needs copy \n");
#endif
      blockPtr->copy = COPY;
      /*      printf("Warning : the physical dimensions of the block being read will not be saved \n"); */
    }

   
   
    if( blockPtr->copy == COPY ) 
      {

	if( masterPtr->data == NULL ) {
#ifdef DEBUG
	  printf("needs allocation \n");
#endif
	  masterPtr->datatype = blockPtr->datatype; 
	  masterPtr->datasize = blockPtr->pixelSize; 

	  masterPtr->data = (char*)ckalloc((size_t)masterPtr->datasize*
					  masterPtr->width*
					  masterPtr->height);
	  if( masterPtr->data == NULL ) {
	    (void)fprintf(stderr,"Could not allocate memory \n");
	    return;
	  }
	} 
	else {
	  if (masterPtr->datatype != blockPtr->datatype ) {
	    (void)fprintf(stderr,"Type mismatch \n");
	    return;
	  }
	}
	if(masterPtr->width == blockPtr->width 
	   && masterPtr->height == blockPtr->height)
	  masterPtr->skip = blockPtr->skip;

	if( blockPtr->datatype == BYTE ) {
	  for(i=0;i<width;i++)
	    for(j=0;j<height;j++)
	      masterPtr->data[i+x+(j+y)*masterPtr->width] =  
		blockPtr->pixelPtr[i+j*blockPtr->pitch];
	}
	else if( blockPtr->datatype == WORD ) {
	  short *srcPtr = (short*)blockPtr->pixelPtr;
	  short *destPtr = (short*)masterPtr->data;
	  for(i=0;i<width;i++)
	    for(j=0;j<height;j++)
	      destPtr[i+x+(j+y)*masterPtr->width] = 
		srcPtr[i+j*blockPtr->pitch];
	}
	else if( blockPtr->datatype == LWORD ) {
	  int *srcPtr = (int*)blockPtr->pixelPtr;
	  int *destPtr = (int*)masterPtr->data;
	  for(i=0;i<width;i++)
	    for(j=0;j<height;j++)
	      destPtr[i+x+(j+y)*masterPtr->width] = 
		srcPtr[i+j*blockPtr->pitch];
	}
	else if( blockPtr->datatype == REAL ) {
	  float *srcPtr = (float*)blockPtr->pixelPtr;
	  float *destPtr = (float*)masterPtr->data;
	  for(i=0;i<width;i++)
	    for(j=0;j<height;j++)
	      destPtr[i+x+(j+y)*masterPtr->width] = 
		srcPtr[i+j*blockPtr->pitch];
	}
	else if( blockPtr->datatype == DOUBLE ) {
	  double *srcPtr = (double*)blockPtr->pixelPtr;
	  double *destPtr = (double*)masterPtr->data;
	  for(i=0;i<width;i++)
	    for(j=0;j<height;j++)
	      destPtr[i+x+(j+y)*masterPtr->width] = 
		srcPtr[i+j*blockPtr->pitch];
	}
      }
    else {
      
      if( masterPtr->bytedata != NULL ) {
	if( (char*)masterPtr->bytedata == (char*)masterPtr->data ) {
	  ckfree((void*)masterPtr->bytedata);
	  masterPtr->bytedata = NULL;
	  masterPtr->data = NULL;
	} else {
	  ckfree((void*)masterPtr->bytedata);
	  masterPtr->bytedata = NULL;
	}
      }
      if( masterPtr->data != NULL ) {
	free((void*)masterPtr->data);
	masterPtr->data = NULL;
      }
      masterPtr->datatype = blockPtr->datatype; 
      masterPtr->datasize = blockPtr->pixelSize; 
      masterPtr->skip = blockPtr->skip;
      /* save physical dimensions */
      masterPtr->pixel_x = blockPtr->pixel_x;
      masterPtr->pixel_y = blockPtr->pixel_y;
      /* Put the data into our local data array */
      masterPtr->data = (char*)blockPtr->pixelPtr;
    }
     
    normalize_data(masterPtr);
    blockPtr->pixelPtr = NULL;

    /*
     * Add this new block to the region which specifies which data is valid.
     */

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;
    XUnionRectWithRegion(&rect, masterPtr->validRegion,
	    masterPtr->validRegion);

    /*
     * Update each instance.
     */
    for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	instancePtr = instancePtr->nextPtr)
      DitherInstance(instancePtr, x, y, width, height);
   
    /*
     * Tell the core image code that this image has changed.
     */

    Tk_ImageChanged(masterPtr->tkMaster, x, y, width, height, masterPtr->width,
	    masterPtr->height);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictPutZoomedBlock --
 *
 *	This procedure is called to put image data into a Pict image,
 *	with possible subsampling and/or zooming of the pixels.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image data is stored.  The image may be expanded.
 *	The Tk image code is informed that the image has changed.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictPutZoomedBlock(handle, blockPtr, x, y, width, height, zoomX, zoomY,
	subsampleX, subsampleY)
    Tk_PictHandle handle;	/* Opaque handle for the Pict image
				 * to be updated. */
    register Tk_PictImageBlock *blockPtr;
				/* Pointer to a structure describing the
				 * pixel data to be copied into the image. */
    int x, y;			/* Coordinates of the top-left pixel to
				 * be updated in the image. */
    int width, height;		/* Dimensions of the area of the image
				 * to be updated. */
    int zoomX, zoomY;		/* Zoom factors for the X and Y axes. */
    int subsampleX, subsampleY;	/* Subsampling factors for the X and Y axes. */
{
    register PictMaster *masterPtr;
    PictInstance *instancePtr;
    int xEnd, yEnd;
    int wLeft, hLeft;
    int wCopy, hCopy;
    int blockWid, blockHt;
    unsigned char *srcPtr, *srcLinePtr, *srcOrigPtr;
    unsigned char *destPtr, *destLinePtr;
    int pitch;
    int xRepeat, yRepeat;
    int blockXSkip, blockYSkip;
    XRectangle rect;
    register int il;

#ifdef DEBUG
    printf("Tk_PictPutZoomedBlock\n");
#endif

    if ((zoomX == 1) && (zoomY == 1) && (subsampleX == 1)
	    && (subsampleY == 1)) {
	Tk_PictPutBlock(handle, blockPtr, x, y, width, height);
	return;
    }

    masterPtr = (PictMaster *) handle;

    if ((zoomX <= 0) || (zoomY <= 0))
	return;
    if ((masterPtr->userWidth != 0) && ((x + width) > masterPtr->userWidth)) {
	width = masterPtr->userWidth - x;
    }
    if ((masterPtr->userHeight != 0)
	    && ((y + height) > masterPtr->userHeight)) {
	height = masterPtr->userHeight - y;
    }
    if ((width <= 0) || (height <= 0))
	return;

    xEnd = x + width;
    yEnd = y + height;
    if ((xEnd > masterPtr->width) || (yEnd > masterPtr->height)) {
	ImgPictSetSize(masterPtr, MAX(xEnd, masterPtr->width),
		MAX(yEnd, masterPtr->height));
    }

  
    if( masterPtr->data == NULL ) {
#ifdef DEBUG
      printf("needs allocation \n");
#endif

      masterPtr->datatype = blockPtr->datatype;
      masterPtr->datasize = blockPtr->pixelSize;
      masterPtr->pixel_x  = blockPtr->pixel_x;
      masterPtr->pixel_y  = blockPtr->pixel_y;

      masterPtr->data = (char*)ckalloc((size_t)masterPtr->datasize*
				      masterPtr->width*
				      masterPtr->height);
      if( masterPtr->data == NULL ) {
	(void)fprintf(stderr,"Could not allocate memory \n");
	return;
      }


    } else {
      if (masterPtr->datatype != blockPtr->datatype ) {
	(void)fprintf(stderr,"Type mismatch \n");
	return;
      }
      if ((masterPtr->pixel_x  != blockPtr->pixel_x) ||
	  (masterPtr->pixel_y  != blockPtr->pixel_y) ) {
	printf("Warning : the physical dimensions of the block being read will not be saved \n");
      }
    }

    /*
     * Work out what area the pixel data in the block expands to after
     * subsampling and zooming.
     */

    blockXSkip = subsampleX * blockPtr->pixelSize;
    blockYSkip = subsampleY * blockPtr->pitch * blockPtr->pixelSize;
    if (subsampleX > 0)
	blockWid = ((blockPtr->width + subsampleX - 1) / subsampleX) * zoomX;
    else if (subsampleX == 0)
	blockWid = width;
    else
	blockWid = ((blockPtr->width - subsampleX - 1) / -subsampleX) * zoomX;
    if (subsampleY > 0)
	blockHt = ((blockPtr->height + subsampleY - 1) / subsampleY) * zoomY;
    else if (subsampleY == 0)
	blockHt = height;
    else
	blockHt = ((blockPtr->height - subsampleY - 1) / -subsampleY) * zoomY;

         
    destLinePtr = (unsigned char*)(masterPtr->data + 
      (y * masterPtr->width + x)*masterPtr->datasize);
    srcOrigPtr = blockPtr->pixelPtr;
    if (subsampleX < 0) {
      srcOrigPtr += (blockPtr->width - 1) * blockPtr->pixelSize;
    }
    if (subsampleY < 0) {
      srcOrigPtr += (blockPtr->height - 1) * 
	blockPtr->pitch * blockPtr->pixelSize; 
    }
    
    pitch = masterPtr->width*masterPtr->datasize;
    for (hLeft = height; hLeft > 0; ) {
      hCopy = MIN(hLeft, blockHt);
      hLeft -= hCopy;
      yRepeat = zoomY;
      srcLinePtr = srcOrigPtr;
      for (; hCopy > 0; --hCopy) {
	destPtr = destLinePtr;
	for (wLeft = width; wLeft > 0;) {
	  wCopy = MIN(wLeft, blockWid);
	  wLeft -= wCopy;
	  srcPtr = srcLinePtr;
	  for (; wCopy > 0; wCopy -= zoomX) {
	    for (xRepeat = MIN(wCopy, zoomX); xRepeat > 0; xRepeat--) {
	      for(il=0;il<masterPtr->datasize;il++)
		*destPtr++ = srcPtr[il];
	    }
	    srcPtr += blockXSkip;
	  }
	}
	destLinePtr += pitch;
	yRepeat--;
	if (yRepeat <= 0) {
	  srcLinePtr += blockYSkip;
	  yRepeat = zoomY;
	}
      }
    }
       
    normalize_data(masterPtr);
    blockPtr->pixelPtr = NULL;

    /*
     * Add this new block to the region that specifies which data is valid.
     */

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;
    XUnionRectWithRegion(&rect, masterPtr->validRegion,
	    masterPtr->validRegion);

    /*
     * Update each instance.
     */

    for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	instancePtr = instancePtr->nextPtr)
      DitherInstance(instancePtr, x, y, width, height);

    /*
     * Tell the core image code that this image has changed.
     */

    Tk_ImageChanged(masterPtr->tkMaster, x, y, width, height, masterPtr->width,
	    masterPtr->height);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictBlank --
 *
 *	This procedure is called to clear an entire Pict image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The valid region for the image is set to the null region.
 *	The generic image code is notified that the image has changed.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictBlank(handle)
    Tk_PictHandle handle;	/* Handle for the image to be blanked. */
{
    PictMaster *masterPtr;
   
#ifdef DEBUG
    printf("Tk_PictBlank\n");
#endif

    masterPtr = (PictMaster *) handle;
   
    /*
     * The image has valid data nowhere.
     */

    if (masterPtr->validRegion != NULL) {
	XDestroyRegion(masterPtr->validRegion);
    }
    masterPtr->validRegion = XCreateRegion();

    /*
     * Clear out the data storage array.
     */

    memset((VOID *) masterPtr->data, 0,
	    (size_t) (masterPtr->width * masterPtr->height * masterPtr->datasize));

    /*
     * Tell the core image code that this image has changed.
     */
  
    Tk_ImageChanged(masterPtr->tkMaster, 0, 0, masterPtr->width,
	    masterPtr->height, masterPtr->width, masterPtr->height);

}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictExpand --
 *
 *	This procedure is called to request that a Pict image be
 *	expanded if necessary to be at least `width' pixels wide and
 *	`height' pixels high.  If the user has declared a definite
 *	image size (using the -width and -height configuration
 *	options) then this call has no effect.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The size of the Pict image may change; if so the generic
 *	image code is informed.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictExpand(handle, width, height)
    Tk_PictHandle handle;	/* Handle for the image to be expanded. */
    int width, height;		/* Desired minimum dimensions of the image. */
{
    PictMaster *masterPtr;

#ifdef DEBUG
    printf("Tk_PictExpand\n");
#endif

    masterPtr = (PictMaster *) handle;

    if (width <= masterPtr->width) {
	width = masterPtr->width;
    }
    if (height <= masterPtr->height) {
	height = masterPtr->height;
    } 

    if ((width != masterPtr->width) || (height != masterPtr->height)) {
	ImgPictSetSize(masterPtr, MAX(width, masterPtr->width),
		MAX(height, masterPtr->height));
	Tk_ImageChanged(masterPtr->tkMaster, 0, 0, 0, 0, masterPtr->width,
		masterPtr->height);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictGetSize --
 *
 *	This procedure is called to obtain the current size of a Pict
 *	image.
 *
 * Results:
 *	The image's width and height are returned in *widthp
 *	and *heightp.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictGetSize(handle, widthPtr, heightPtr)
    Tk_PictHandle handle;	/* Handle for the image whose dimensions
				 * are requested. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are returned
				 * here. */
{
    PictMaster *masterPtr;

#ifdef DEBUG
    printf("Tk_PictGetSize\n");
#endif

    masterPtr = (PictMaster *) handle;
    *widthPtr = masterPtr->width;
    *heightPtr = masterPtr->height;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictSetSize --
 *
 *	This procedure is called to set size of a Pict image.
 *	This call is equivalent to using the -width and -height
 *	configuration options.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The size of the image may change; if so the generic
 *	image code is informed.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictSetSize(handle, width, height)
    Tk_PictHandle handle;	/* Handle for the image whose size is to
				 * be set. */
    int width, height;		/* New dimensions for the image. */
{
    PictMaster *masterPtr;

#ifdef DEBUG
    printf("Tk_PictSetSize\n");
#endif

    masterPtr = (PictMaster *) handle;

    masterPtr->userWidth = width;
    masterPtr->userHeight = height;
    ImgPictSetSize(masterPtr, ((width > 0) ? width: masterPtr->width),
	    ((height > 0) ? height: masterPtr->height));
    Tk_ImageChanged(masterPtr->tkMaster, 0, 0, 0, 0,
	    masterPtr->width, masterPtr->height);
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictGetImage --
 *
 *	This procedure is called to obtain image data from a Pict
 *	image.  This procedure fills in the Tk_PictImageBlock structure
 *	pointed to by `blockPtr' with details of the address and
 *	layout of the image data in memory.
 *
 * Results:
 *	TRUE (1) indicating that image data is available,
 *	for backwards compatibility with the old Pict widget.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tk_PictGetImage(handle, blockPtr)
    Tk_PictHandle handle;	/* Handle for the Pict image from which
				 * image data is desired. */
    Tk_PictImageBlock *blockPtr;
				/* Information about the address and layout
				 * of the image data is returned here. */
{
    PictMaster *masterPtr;

#ifdef DEBUG
    printf("Tk_PictGetImage\n");
#endif

    masterPtr = (PictMaster *) handle;
    blockPtr->pixelPtr = (unsigned char*)masterPtr->data;
    blockPtr->width = masterPtr->width;
    blockPtr->height = masterPtr->height;
    blockPtr->pitch = masterPtr->width;
    blockPtr->pixelSize = masterPtr->datasize;;
    blockPtr->datatype = masterPtr->datatype;
    blockPtr->copy = COPY;
    blockPtr->skip = masterPtr->skip;
    blockPtr->pixel_x = masterPtr->pixel_x;
    blockPtr->pixel_y = masterPtr->pixel_y;
    return 1;
}


static int make_colorbar(Tk_PictHandle handle,
			 int width, int height)
{
  Tk_PictImageBlock block;
  int i,j;
  unsigned char *pixelPtr;
  int nBytes;
 
#ifdef DEBUG
    printf("make_colorbar \n");
#endif

  block.datatype = BYTE;
  block.pixelSize = sizeof(unsigned char);
  block.width = width;
  block.height = height;
  block.pitch = block.pixelSize * width;
  
  nBytes = width * height * block.pixelSize;
  pixelPtr = (unsigned char *) ckalloc((unsigned) nBytes);
  if ( pixelPtr == NULL )
    return 0;

  for(j=0;j<height;j++)
    for(i=0;i<width;i++)
      pixelPtr[i+j*width] = i*255/width;

  block.pixelPtr = pixelPtr;

  Tk_PictPutBlock(handle, &block,0,0, width, height);
  return 1;
} /* end make_colorbar */


static void normalize_data(PictMaster *masterPtr)
{
  unsigned char *out;

#ifdef DEBUG
  printf("normalize_data\n");
#endif
  if( masterPtr->bytedata != NULL ) {
    ckfree((void*)masterPtr->bytedata);
  }
  out=(unsigned char*)ckalloc((size_t)(masterPtr->width*masterPtr->height*sizeof(unsigned char)));
  if( out == NULL ) {
    (void)fprintf(stderr,"Could not allocate memory \n");
    return;
  }
  
  masterPtr->bytedata = out;
  if( masterPtr->user_dispmin != 0.0 || masterPtr->user_dispmax != 0.0 ) {
    masterPtr->dispmin = masterPtr->user_dispmin;
    masterPtr->dispmax = masterPtr->user_dispmax;
  } else {
    masterPtr->dispmin = 0.0;
    masterPtr->dispmax = 0.0;
  }
  convert_block_to_byte((void*)masterPtr->data,
			(unsigned char*)masterPtr->bytedata,
			(masterPtr->width)*(masterPtr->height),
			masterPtr->datatype,
			&(masterPtr->dispmin),
			&(masterPtr->dispmax)); 
} /* end normalize_data */


/* extract the pixel values along an arbitrary line. No interpolation, 
uses the Bresenham algorithm instead */

static void get_line_pixels(char *string,
			    unsigned char *img,
			    int nr,int nc,
			    int x1,int y1,
			    int x2,int y2,
			    double min, double max)
{
  int d, x, y, ax, ay, sx, sy, dx, dy;
  int pix_int;
  int i = 0;
  char string1[10000];

  dx = x2-x1;  ax = ABS(dx)<<1;  sx = SGN(dx);
  dy = y2-y1;  ay = ABS(dy)<<1;  sy = SGN(dy);
	
  x = x1;
  y = y1;

  

  if (ax>ay) {		/* x dominant */
    d = ay-(ax>>1);
    for (;;) {
      /* add index x+y*nc to list */
      if ((y<0) || (y>= nr) || (x<0) || (x>nc))
	pix_int = 0;
      else 
	pix_int = img[y*nc + x];
      sprintf(string1,"%d ",i++); 
      strcat(string,string1);
      sprintf(string1,"%g ",((double)pix_int/(double)(MAX_COLORS-1.0)*(max-min))+min);
      strcat(string,string1);
      if (x==x2) return;
      if (d>=0) {
	y += sy;
	d -= ax;
      }
      x += sx;
      d += ay;
    }
  }
  else {	        /* y dominant */
    d = ax-(ay>>1);
    for (;;) {
      /* add index x+y*nc to list */
      if ((y<0) || (y>= nr) || (x<0) || (x>nc))
	pix_int = 0;
      else 
	pix_int = img[y*nc + x];
      sprintf(string1,"%d ",i++); 
      strcat(string,string1);
      sprintf(string1,"%g ",((double)pix_int/(double)(MAX_COLORS-1.0)*(max-min))+min);
      strcat(string,string1);
      if (y==y2) return;
      if (d>=0) {
	x += sx;
	d -= ay;
      }
      y += sy;
      d += ax;
     }
  }
} /* end get_line_pixels */

static int ChangeColorTable(PictMaster *masterPtr)    
{
  PictInstance *instancePtr = masterPtr->instancePtr;
  PictColorTable *colorTable;
  PictColorTable *old_colorTable;
  XVisualInfo *visInfoPtr;
  XRectangle validBox;
  
#ifdef DEBUG
  printf("ChangeColorTable \n");
#endif

  old_colorTable = instancePtr->colorTable;
  old_colorTable->refCount--;
  if ( old_colorTable->refCount == 0 ) {
    DisposeColorTable((PictColorTable *)old_colorTable);
  }
  
  if( GetColorTable(instancePtr->tkwin,(PictColorTable **)&colorTable,
		    (XVisualInfo **)&visInfoPtr) == 0 ) {
    fprintf(stderr," GetColorTable failed \n");
    return 0;
  }

  instancePtr->display = colorTable->display;
  instancePtr->colormap = colorTable->colormap;
  instancePtr->colormap_level = colorTable->colormap_level;
  instancePtr->has_overlay = 0;
  instancePtr->colorTable = colorTable;
  instancePtr->visualInfo = *visInfoPtr;
  XFree(visInfoPtr);
 
  XClipBox(masterPtr->validRegion, &validBox);
  if ((validBox.width > 0) && (validBox.height > 0)) {
    DitherInstance(instancePtr, validBox.x, validBox.y, validBox.width,
		   validBox.height);
  }

  Tk_ImageChanged(masterPtr->tkMaster, 0, 0,  
		  masterPtr->width, masterPtr->height,
		  masterPtr->width, masterPtr->height);
  return 1;

} /* end ChangeColorTable */


#ifdef PLB_SEGMENT
static int ImgPictClip(Tcl_Interp *interp,
		       PictMaster *masterPtr,
		       int argc,
		       char **argv)
{ 
  PictInstance *instancePtr;
  int loval,hival;
  double dloval,dhival;
  int sim;
  
  if (argc < 3 || argc > 4) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " clip  loval [hival]\"", (char *) NULL);
    return TCL_ERROR;
  }
  
  sim = masterPtr->width*masterPtr->height;
  
  if(masterPtr->datatype == BYTE) {
    if(argc==3) {
      if(Tcl_GetInt(interp, argv[2], &loval) != TCL_OK)
	return TCL_ERROR;
      if(loval<0 || loval > 255) {
	Tcl_AppendResult(interp, argv[0], "clip: ",
			 "parameters should be in range [0-255]", 
			 (char *) NULL);
	return TCL_ERROR;
      }
      lclip_above_ubyte((unsigned char*)masterPtr->data,sim,
			loval);
    }
    else {
      if((Tcl_GetInt(interp, argv[2], &loval) != TCL_OK) || 
	 (Tcl_GetInt(interp, argv[3], &hival) != TCL_OK)) 
	return TCL_ERROR;
      
      if(loval<0 || loval > 255 || hival<0 || hival>255) {
	Tcl_AppendResult(interp, argv[0], "clip: ",
			 "parameters should be in range [0-255]", 
			 (char *) NULL);
	return TCL_ERROR;
      }
      lclip_ubyte((unsigned char*)masterPtr->data,sim,loval,hival);
    }
  } else if(masterPtr->datatype == WORD ) {
    if(argc==3) {
      if(Tcl_GetInt(interp, argv[2], &loval) != TCL_OK)
	return TCL_ERROR;
      if(loval<-32767 || loval > 32767 ) {
	Tcl_AppendResult(interp, argv[0], "clip: ",
			 "parameters should be in range [-32768,32768]", 
			 (char *) NULL);
	return TCL_ERROR;
      }
      lclip_above_short((short*)masterPtr->data,sim,loval);
    }
    else {
      if((Tcl_GetInt(interp, argv[2], &loval) != TCL_OK) || 
	 (Tcl_GetInt(interp, argv[3], &hival) != TCL_OK)) 
	return TCL_ERROR;
      
      if((loval<-32767) || (loval > 32767) || 
	 (hival<-32767) || (hival>32767)) {
	Tcl_AppendResult(interp, argv[0], "clip: ",
			 "parameters should be in range [-32768,32768]",
			 (char *) NULL);
	return TCL_ERROR;
      }
      lclip_short((short*)masterPtr->data,sim,loval,hival);
    }
  } else if(masterPtr->datatype == LWORD ) {
    if(argc==3) {
      if(Tcl_GetInt(interp, argv[2], &loval) != TCL_OK)
	return TCL_ERROR;
      
      lclip_above_int((int*)masterPtr->data,sim,loval);
    }
    else {
      if((Tcl_GetInt(interp, argv[2], &loval) != TCL_OK) || 
	 (Tcl_GetInt(interp, argv[3], &hival) != TCL_OK)) 
	return TCL_ERROR;
      
      lclip_int((int*)masterPtr->data,sim,loval,hival);
    }
  } else if(masterPtr->datatype == REAL ) {
    /*this should be duplicated for DOUBLEs, but since I don't have
      the plb segment library, I've no idea how.  LEB */
    if(argc==3) {
      if(Tcl_GetDouble(interp, argv[2], &dloval) != TCL_OK)
	return TCL_ERROR;
      
      lclip_above_float((float*)masterPtr->data,sim,dloval);
    }
    else {
      if((Tcl_GetDouble(interp, argv[2], &dloval) != TCL_OK) || 
	 (Tcl_GetDouble(interp, argv[3], &dhival) != TCL_OK)) 
	return TCL_ERROR;
      
      lclip_float((float*)masterPtr->data,sim,
		  (float)dloval,(float)dhival);
    }
  }
  
  normalize_data(masterPtr);
  
  /*
   * Update each instance.
   */
  for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
      instancePtr = instancePtr->nextPtr)
    DitherInstance(instancePtr, 0, 0, 
		   masterPtr->width, masterPtr->height);
  
  /*
   * Tell the core image code that this image has changed.
   */
  
  Tk_ImageChanged(masterPtr->tkMaster, 0, 0, 
		  masterPtr->width,
		  masterPtr->height,
		  masterPtr->width,
		  masterPtr->height);
  return TCL_OK;
} /* end ImgPictClip */

static int ImgPictThreshold(Tcl_Interp *interp,
			    PictMaster *masterPtr,
			    int argc,
			    char **argv)
{
  PictInstance *instancePtr;
  int loval,hival,inval,outval;
  int rdval;
  double dloval,dhival,dinval,doutval;
  int sim;
  int parsed1 = 1;
  char *arg;
  int argstart;
  int has_lo;
  int has_hi;
  
  if( argc == 2 ) {
    Tcl_AppendResult(interp,"Usage ",argv[0]," threshold -loval <loval> [-hival <hival> -inval <inval> -outval <outval>] \n",(char*)NULL);
    return TCL_ERROR;
  }
  
  has_lo = has_hi = 0;
  argstart = 2;
  
  if ( masterPtr->datatype == BYTE ) {
    inval = 255;
    outval = 0;
  } else if ( masterPtr->datatype == WORD || 
	     masterPtr->datatype == LWORD  ) {
    inval = 1;
    outval = 0;
  } else if (masterPtr->datatype == REAL ) {
    /*this should be duplicated for DOUBLEs, but since I don't have
      the plb segment library, I've no idea how.  LEB */
    dinval = 1.0;
    doutval = 0.0;
  }
  
  if ( masterPtr->datatype != REAL ) {
    /*this should be duplicated for DOUBLEs, but since I don't have
      the plb segment library, I've no idea how.  LEB */
    while( (argstart<argc) && (arg = argv[argstart]) != NULL && parsed1)
      {
	parsed1 = 0;
	
	if (strcmp(arg,"-loval") == 0)
	  {
	    has_lo = parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetInt(interp, argv[argstart], &loval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    rdval = loval;
	    argstart++;
	  }
	else if (strcmp(arg,"-hival") == 0)
	  {
	    has_hi = parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetInt(interp, argv[argstart], &hival) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    } 
	    rdval = hival;
	    argstart++;
	  }
	else if (strcmp(arg,"-inval") == 0)
	  {
	    parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetInt(interp, argv[argstart], &inval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    rdval = inval;
	    argstart++;
	  }
	else if (strcmp(arg,"-outval") == 0)
	  {
	    parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetInt(interp, argv[argstart], &outval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg," option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    rdval = outval;
	    argstart++;
	  }
	else {
	  Tcl_AppendResult(interp,"Usage ",argv[0]," threshold -loval <loval> [-hival <hival> -inval <inval> -outval <outval>] \n",(char*)NULL);
	  return TCL_ERROR;
	}
	
	if(masterPtr->datatype == BYTE) {
	  if((rdval<0 || rdval > 255)) {
	    Tcl_AppendResult(interp, argv[0], "threshold: ",
			     "parameters should be in range [0-255]", 
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	} else if(masterPtr->datatype == WORD) {
	  if((loval<-32767) || (loval > 32767)) {
	    Tcl_AppendResult(interp, argv[0], "threshold: ",
			     "parameters should be in range  [-32768,32768]",
			     (char *) NULL);
	    return TCL_ERROR;
	  }
	}
      }
  } else { /* process floating-points */
    while ((arg = argv[argstart]) != NULL && parsed1)
      {
	parsed1 = 0;
	
	if (strcmp(arg,"-loval") == 0)
	  {
	    has_lo = parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetDouble(interp, argv[argstart], &dloval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    argstart++;
	  }
	else if (strcmp(arg,"-hival") == 0)
	  {
	    has_hi = parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetDouble(interp, argv[argstart], &dhival) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg," option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    argstart++;
	  }
	else if (strcmp(arg,"-inval") == 0)
	  {
	    parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetDouble(interp, argv[argstart], &dinval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    argstart++;
	  }
	else if (strcmp(arg,"-outval") == 0)
	  {
	    parsed1 = 1;
	    ++argstart;
	    if( argv[argstart] != NULL ) {
	      if( (Tcl_GetDouble(interp, argv[argstart], &doutval) != TCL_OK)) {
		Tcl_AppendResult(interp," could not read value for ",arg,
				 " option",(char*)NULL);
		return TCL_ERROR;
	      }
	    }
	    argstart++;
	  }
	else {
	  Tcl_AppendResult(interp,"Usage ",argv[0]," threshold -loval <loval> [-hival <hival> -in <inval> -out <outval>] \n",(char*)NULL);
	  return TCL_ERROR;
	}
      }
  }
  
  if( !has_lo) {
    Tcl_AppendResult(interp,"Usage ",argv[0]," threshold -loval <loval> [-hival <hival> -inval <inval> -outval <outval>] \n",(char*)NULL);
    return TCL_ERROR;
  }
  
  
  sim = masterPtr->width*masterPtr->height;
  
  if(masterPtr->datatype == BYTE) {
    if( !has_hi )
      lthres_above_ubyte((unsigned char*)masterPtr->data,
			 (unsigned char*)masterPtr->data,
			 sim,
			 loval,inval,outval);
    else
      lthres_between_ubyte((unsigned char*)masterPtr->data,
			   (unsigned char*)masterPtr->data,
			   sim,
			   loval,hival,inval,outval);
  } else if(masterPtr->datatype == WORD ) {
    if( !has_hi )
      lthres_above_short((short*)masterPtr->data,
			 (short*)masterPtr->data,
			 sim,loval,inval,outval);
    else
      lthres_between_short((short*)masterPtr->data,
			   (short*)masterPtr->data,
			   sim,loval,hival,inval,outval);
  } else if(masterPtr->datatype == LWORD ) {
    if( !has_hi )
      lthres_above_int((int*)masterPtr->data,
		       (int*)masterPtr->data,
		       sim,loval,inval,outval);
    else 
      lthres_between_int((int*)masterPtr->data,
			 (int*)masterPtr->data,
			 sim,loval,hival,inval,outval);
  } else if(masterPtr->datatype == REAL ) {
    /*this should be duplicated for DOUBLEs, but since I don't have
      the plb segment library, I've no idea how.  LEB */

    if( !has_hi )
      lthres_above_float((float*)masterPtr->data,
			 (float*)masterPtr->data,
			 sim,(float)dloval,(float)dinval,(float)doutval);
    else
      lthres_between_float((float*)masterPtr->data,
			   (float*)masterPtr->data,
			   sim,
			   (float)dloval,(float)dhival,
			   (float)dinval,(float)doutval);
  }
  
  normalize_data(masterPtr);
  
  /*
   * Update each instance.
   */
  for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
      instancePtr = instancePtr->nextPtr)
    DitherInstance(instancePtr, 0, 0, 
		   masterPtr->width, masterPtr->height);
  
  /*
   * Tell the core image code that this image has changed.
   */
  
  Tk_ImageChanged(masterPtr->tkMaster, 0, 0, 
		  masterPtr->width,
		  masterPtr->height,
		  masterPtr->width,
		  masterPtr->height);
  return TCL_OK;
} /* end ImgPictThreshold */


static int ImgPictSmooth(Tcl_Interp *interp,
			 PictMaster *masterPtr,
			 int argc,
			 char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  float *temp_img;
  float *buffer;
  int nr,nc;
  int npts;
  float *binom_filter;
  int filter_order=7;
  int len;
  
  if(argc != 3 &&  argc != 5) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " smooth <srcImg> [-order <order>]",
		     (char *) NULL);
    return TCL_ERROR;
  }
  if( argc==5 ) {
    len = strlen(argv[3]);
    if (strncmp(argv[3],"-order",len) != 0 ||
	(Tcl_GetInt(interp, argv[4], &filter_order) != TCL_OK )) {
      Tcl_AppendResult(interp," wrong # of arguments, should be ", argv[0], 
		       "smooth <srcImg> [-order <order>]",
		       (char *) NULL);
      return TCL_ERROR;
    }
    if (filter_order<1 || filter_order> 40) {
      Tcl_AppendResult(interp,"The order of the low-pass filter is too big. The maximum acceptable value is 40",
		       (char *) NULL);
      return TCL_ERROR;
    }
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  temp_img = (float*)ckalloc(npts*sizeof(float));
  if ( temp_img == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in smooth ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  /* convert to float and allocate memory */
  buffer = (float*)ckalloc(npts*sizeof(float));
  if ( buffer == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in smooth ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory for filter */
  binom_filter = (float*)ckalloc(filter_order*sizeof(float));
  if ( binom_filter == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in smooth ",
		     (char*)NULL);
    return TCL_ERROR;
  }
 
  lconvert_types(npts,
		 (void*)srcMasterPtr->data,
		 srcMasterPtr->datatype,
		 (void*)buffer,
		 REAL);
  
  lbinom_float_1D(binom_filter,filter_order);
  lconvolve2D_float((char*)buffer,(char*)binom_filter,(char*)temp_img,
		    nr,nc,filter_order,1);
  lconvolve2D_float((char*)temp_img,(char*)binom_filter,(char*)buffer,
		    nr,nc,1,filter_order);
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(float);
  block.datatype = REAL;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)buffer;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  /* free memory */
  ckfree((void*)temp_img);
  ckfree((void*)binom_filter);
  
  return TCL_OK;
} /* end ImgPictSmooth */	

static int ImgPictGradient(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr; 
  Tk_PictImageBlock block;
  float *g_img;
  float *buffer;
  int npts;
  int nr,nc;
  int len;
  float *binom_filter;
  int filter_order = 1;
  
  if(argc != 3 &&  argc != 5) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " gradient <srcImg> [-order <order>]",
		     (char *) NULL);
    return TCL_ERROR;
  }
  if( argc==5 ) {
    len = strlen(argv[3]);
    if (strncmp(argv[3],"-order",len) != 0 ||
	(Tcl_GetInt(interp, argv[4], &filter_order) != TCL_OK )) {
      Tcl_AppendResult(interp," wrong # of arguments, should be ", argv[0], 
		       "gradient <srcImg> [-order <order>]",
		       (char *) NULL);
      return TCL_ERROR;
    }
    if (filter_order<1 || filter_order> 40) {
      Tcl_AppendResult(interp,"The order of the low-pass filter is too big. The maximum acceptable value is 40",
		       (char *) NULL);
      return TCL_ERROR;
    }
  }
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  g_img = (float*)ckalloc(npts*sizeof(float));
  if ( g_img == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in gradient ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  /* convert to float if necessary, otherwise copy */
  buffer = (float*)ckalloc(npts*sizeof(float));
  if ( buffer == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in gradient ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  lconvert_types(npts,
		 (void*)srcMasterPtr->data,
		 srcMasterPtr->datatype,
		 (void*)buffer,
		 REAL);
  if( filter_order > 1 ) {
    /* allocate memory for filter */
    binom_filter = (float*)ckalloc(filter_order*sizeof(float));
    if ( binom_filter == NULL) {
      Tcl_AppendResult(interp, "Cannot allocate memory in gradient ",
		       (char*)NULL);
      return TCL_ERROR;
    }
    lbinom_float_1D(binom_filter,filter_order);
    lconvolve2D_float((char*)buffer,(char*)binom_filter,(char*)g_img,
		      nr,nc,filter_order,1);
	  lconvolve2D_float((char*)g_img,(char*)binom_filter,(char*)buffer,
			    nr,nc,1,filter_order);
  }
  
  lgradient2D_float((float*)buffer,(float*)g_img,nr,nc);
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(float);
  block.datatype = REAL;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  block.pixelPtr = (unsigned char*)g_img;
  
  /* set the dimensions to force reallocation of memory */
  masterPtr->width = 0;
  masterPtr->height = 0;
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  /* free memory */
  ckfree((void*)buffer);
  if( filter_order > 1 ) 
    ckfree((void*)binom_filter);
  
  return TCL_OK;
} /* end ImgPictGradient */

static int ImgPictLaplacian(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv)
{
Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr; 
  Tk_PictImageBlock block;
  float *g_img;
  float *buffer;
  int npts;
  int nr,nc;
  int len;
  
  if(argc != 3) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " laplacian <srcImg> ",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  g_img = (float*)ckalloc(npts*sizeof(float));
  if ( g_img == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in laplacian ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  /* convert to float if necessary, otherwise copy */
  buffer = (float*)ckalloc(npts*sizeof(float));
  if ( buffer == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in laplacian ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  lconvert_types(npts,
		 (void*)srcMasterPtr->data,
		 srcMasterPtr->datatype,
		 (void*)buffer,
		 REAL);

  llaplacian2D_float((float*)buffer,(float*)g_img,nr,nc);
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(float);
  block.datatype = REAL;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  block.pixelPtr = (unsigned char*)g_img;
  
  /* set the dimensions to force reallocation of memory */
  masterPtr->width = 0;
  masterPtr->height = 0;
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  /* free memory */
  ckfree((void*)buffer);
  
  return TCL_OK;
} /* end ImgPictLaplacian */

static int ImgPictZeroCrng(Tcl_Interp *interp,
			   PictMaster *masterPtr,
			   int argc,
			   char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr; 
  Tk_PictImageBlock block;
  unsigned char *result;
  float *buffer;
  int npts;
  int nr,nc;
  int i,j;
  unsigned char temp;
  
  if(argc != 3) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " zero_crossings <srcImg> ",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  if( srcMasterPtr->datatype != REAL ) {
    Tcl_AppendResult(interp, "Can only compute zero-crossings for floating-point images", (char *) NULL);
    return TCL_ERROR;
  }

  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  result = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( result == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in zero_crossings ",
		     (char*)NULL);
    return TCL_ERROR;
  } 
  memset((void*)result,0,npts*sizeof(unsigned char));

  buffer = (float*)(srcMasterPtr->data);

  /* detect zero_crossings */
  for(i=1;i<(nc-1);i++)
    for(j=1;j<(nr-1);j++)
      {
	temp = 0;
	if( buffer[j*nc+i] < 0.0 )
	  {
	    if(((float)(buffer[(j+1)*nc +i]) > 0.) ||   
	       ((float)(buffer[j*nc +i+1]) > 0.)  )
	      
	      temp = 255;
	  }  
	if( buffer[j*nc+i] >= 0.0 )
	  {
	    if(((float)(buffer[(j+1)*nc +i]) <= 0.) ||   
	       ((float)(buffer[j*nc +i+1]) <= 0.)  )
	      
	      temp = 255;
	  }                
	result[j*nc + i ] = temp;
      }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(unsigned char);
  block.datatype = BYTE;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  block.pixelPtr = (unsigned char*)result;
  
  /* set the dimensions to force reallocation of memory */
  masterPtr->width = 0;
  masterPtr->height = 0;
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  return TCL_OK;
} /* end ImgPictZeroCrng */

static int ImgPictErosion(Tcl_Interp *interp,
			  PictMaster *masterPtr,
			  int argc,
			  char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *result;
  int nr,nc;
  int x,y;
  int npts;

  if(argc != 5) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " erosion <srcImg> x y",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
     (Tcl_GetInt(interp, argv[4], &y) != TCL_OK)) {
    Tcl_AppendResult(interp," wrong arguments, should be ", argv[0], 
		     " erosion <srcImg> x y",
		      (char *) NULL);
    return TCL_ERROR;
  }      
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
   
  /* allocate memory for result */
  result = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( result == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in erosion ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  memcpy((void*)result,(void*)(srcMasterPtr->bytedata),
	 (size_t)(npts*sizeof(unsigned char)));
 
  if( !lseparable_erosion_3D(result,nr,nc,1,
			     x,y,0) ) {
    Tcl_AppendResult(interp, "lerosion_3D failed",
		     (char*)NULL);
    ckfree((void*)result);
    return TCL_ERROR;
  }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(unsigned char);
  block.datatype = BYTE;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)result;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  return TCL_OK;
} /* end ImgPictErosion*/	

static int ImgPictDilation(Tcl_Interp *interp,
			  PictMaster *masterPtr,
			  int argc,
			  char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *result;
  int nr,nc;
  int x,y;
  int npts;

  if(argc != 5) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " dilation <srcImg> x y",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
     (Tcl_GetInt(interp, argv[4], &y) != TCL_OK)) {
    Tcl_AppendResult(interp," wrong arguments, should be ", argv[0], 
		     " dilation <srcImg> x y",
		     (char *) NULL);
    return TCL_ERROR;
  }      
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
   
  /* allocate memory for result */
  result = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( result == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in dilation ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  memcpy((void*)result,(void*)(srcMasterPtr->bytedata),
	 (size_t)(npts*sizeof(unsigned char)));
 
  if( !lseparable_dilation_3D(result,nr,nc,1,
			     x,y,0) ) {
    Tcl_AppendResult(interp, "ldilation_3D failed",
		     (char*)NULL);
    ckfree((void*)result);
    return TCL_ERROR;
  }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(unsigned char);
  block.datatype = BYTE;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)result;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  return TCL_OK;
} /* end ImgPictDilation*/	

static int ImgPictCloseHoles(Tcl_Interp *interp,
			     PictMaster *masterPtr,
			     int argc,
			     char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *temp_img;
  unsigned char *marker;
  int nr,nc;
  int npts;
 
  if(argc != 3) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " close_holes <srcImg> ",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
   
  /* allocate memory for marker */
  marker = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( marker == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in close_holes ",
		     (char*)NULL);
    return TCL_ERROR;
  }
  
  temp_img = (unsigned char *)(srcMasterPtr->bytedata);
  
  if( !lclose_holes_3D(temp_img,marker,255,nr,nc,1,0) ) {
    Tcl_AppendResult(interp, "lclose_holes_3D failed",
		     (char*)NULL);
    ckfree((void*)marker);
    return TCL_ERROR;
  }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(unsigned char);
  block.datatype = BYTE;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)marker;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  return TCL_OK;
} /* end ImgPictCloseHoles */	

static int ImgPictGetHoles(Tcl_Interp *interp,
			     PictMaster *masterPtr,
			     int argc,
			     char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *temp_img;
  unsigned char *marker;
  int nr,nc;
  int npts;
  int i;

  if(argc != 3) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " get_holes <srcImg> ",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  /* allocate memory for marker */
  marker = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( marker == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in close_holes ",
		     (char*)NULL);
    return TCL_ERROR;
  }

  temp_img = (unsigned char*)(srcMasterPtr->bytedata);

  if( !lclose_holes_3D(temp_img,marker,255,nr,nc,1,0) ) {
    Tcl_AppendResult(interp, "lclose_holes_3D failed",
		     (char*)NULL);
    ckfree((void*)marker);
    return TCL_ERROR;
  }
  
  /* now get the holes by substraction */
  for(i=0;i<npts;i++) {
    marker[i] -= temp_img[i];
  }
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(unsigned char);
  block.datatype = BYTE;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)marker;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
 
  return TCL_OK;
} /* end ImgPictGetHoles */



static int ImgPictDistanceTransform(Tcl_Interp *interp,
				    PictMaster *masterPtr,
				    int argc,
				    char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *temp_img;
  short  *result;
  int nr,nc;
  int npts;
  int dist;

  if(argc != 4) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " dt <srcImg> [34|5711]",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }
  
  if((Tcl_GetInt(interp, argv[3], &dist) != TCL_OK) ||
     (dist != 34 && dist != 5711)) {
    Tcl_AppendResult(interp," wrong arguments, should be ", argv[0], 
		     " dt <srcImg> [34|5711]",
		     (char *) NULL);
    return TCL_ERROR;
  }      

  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  /* allocate memory for output */
  result = (short*)ckalloc(npts*sizeof(short));
  if ( result == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in DistanceTransform",
		     (char*)NULL);
    return TCL_ERROR;
  }

  temp_img = (unsigned char*)(srcMasterPtr->bytedata);

  if( !ldt_2D(temp_img,
	      result,nr,nc,
	      1,  /* one slice */
	      dist, /* type of distance */
	      1)) { /* force normalization */
    Tcl_AppendResult(interp, "ldt_2D failed",
		     (char*)NULL);
    ckfree((void*)result);
    return TCL_ERROR;
  }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(short);
  block.datatype = WORD;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)result;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
 
  return TCL_OK;
} /* end ImgPictDistanceTransform*/

static int ImgPictLabel(Tcl_Interp *interp,
			PictMaster *masterPtr,
			int argc,
			char **argv)
{
  Tk_PictHandle srcHandle;
  PictMaster *srcMasterPtr;
  Tk_PictImageBlock block;
  unsigned char *temp_img;
  short  *result;
  int nr,nc;
  int npts;
 
  char *arg;
  int argstart;
  int parsed1;
  int hival = 1;
  int loval = 1;
  int has_lo = 0;
  int su = 0;
  int ma =0 ;
  int stretch = 0;
  int minlab = 1;
  int verbose = 0;

  if(argc < 3) {
    Tcl_AppendResult(interp,"wrong # of arguments, should be ", argv[0], 
		     " label <srcImg> [-hival <hival> -loval <loval> -su <surface size> -ma <max surfaces> -minlab <minlab> -v]",
		     (char *) NULL);
    return TCL_ERROR;
  }
  
  if ((srcHandle = Tk_FindPict(argv[2])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " exist or is not a Pict image", (char *) NULL);
    return TCL_ERROR;
  }
  srcMasterPtr = (PictMaster *)srcHandle;
  
  if( srcMasterPtr->data == NULL ) {
    Tcl_AppendResult(interp, "image \"", argv[2], "\" doesn't",
		     " contain any data", (char *) NULL);
    return TCL_ERROR;
  }

  argstart = 3;
  while ( (argstart<argc) && ((arg = argv[argstart]) != NULL) && parsed1)
    {
      parsed1 = 0;
      
      if (strcmp(arg,"-loval") == 0)
	{
	  has_lo = parsed1 = 1;
	  ++argstart;
	  if( argv[argstart] != NULL ) {
	    if( (Tcl_GetInt(interp, argv[argstart], &loval) != TCL_OK)) {
	      Tcl_AppendResult(interp," could not read value for ",arg,
			       " option",(char*)NULL);
	      return TCL_ERROR;
	    }
	  }
	  argstart++;
	}
      else if (strcmp(arg,"-hival") == 0)
	{
	  parsed1 = 1;
	  ++argstart;
	  if( argv[argstart] != NULL ) {
	    if( (Tcl_GetInt(interp, argv[argstart], &hival) != TCL_OK)) {
	      Tcl_AppendResult(interp," could not read value for ",arg,
			       " option",(char*)NULL);
	      return TCL_ERROR;
	    }
	  }
	  argstart++;
	}
      else if (strcmp(arg,"-su") == 0)
	{
	  parsed1 = 1;
	  ++argstart;
	  if( argv[argstart] != NULL ) {
	    if( (Tcl_GetInt(interp, argv[argstart], &su) != TCL_OK)) {
	      Tcl_AppendResult(interp," could not read value for ",arg,
			       " option",(char*)NULL);
	      return TCL_ERROR;
	    }
	  }
	  argstart++;
	}
      else if (strcmp(arg,"-ma") == 0)
	{
	  parsed1 = 1;
	  ++argstart;
	  if( argv[argstart] != NULL ) {
	    if( (Tcl_GetInt(interp, argv[argstart], &ma) != TCL_OK)) {
	      Tcl_AppendResult(interp," could not read value for ",arg,
			       " option",(char*)NULL);
	      return TCL_ERROR;
	    }
	  }
	  argstart++;
	}
      else if (strcmp(arg,"-minlab") == 0)
	{
	  parsed1 = 1;
	  ++argstart;
	  if( argv[argstart] != NULL ) {
	    if( (Tcl_GetInt(interp, argv[argstart], &minlab) != TCL_OK)) {
	      Tcl_AppendResult(interp," could not read value for ",arg,
			       " option",(char*)NULL);
	      return TCL_ERROR;
	    }
	  }
	  argstart++;
	}
      else if (strcmp(arg,"-v") == 0)
	{
	  parsed1 = 1;
	  verbose = 1;
	  ++argstart;
	}
    }
  
  if( has_lo == 0) 
    loval = hival;

  if( hival > 255 || loval > 255 || minlab > 255) {
    Tcl_AppendResult(interp," hival, loval and minlab values should be lower than 255",(char*)NULL);
    return TCL_ERROR;
  }
  if( minlab > 1 )
    stretch = 1;

  /* allocate memory */
  nr = srcMasterPtr->height;
  nc = srcMasterPtr->width;
  npts = nr*nc;
  
  /* allocate memory for input */
  temp_img = (unsigned char*)ckalloc(npts*sizeof(unsigned char));
  if ( temp_img == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in label",
		     (char*)NULL);
    return TCL_ERROR;
  }
  memcpy((void*)temp_img,(void*)(srcMasterPtr->bytedata),npts);

  /* allocate memory for output */
  result = (short*)ckalloc(npts*sizeof(short));
  if ( result == NULL) {
    Tcl_AppendResult(interp, "Cannot allocate memory in label",
		     (char*)NULL);
    ckfree((void*)temp_img);
    return TCL_ERROR;
  }

  if( lflabel_2D_short(temp_img,
		       result,
		       nr,nc,
		       su,   
		       ma,   
		       (unsigned char)hival,  
		       (unsigned char)loval,   
		       (unsigned char)stretch,    
		       (unsigned char)minlab,
		       (char)verbose) < 0) { 
    Tcl_AppendResult(interp, "lflabel_2D_short failed",
		     (char*)NULL);
    ckfree((void*)result);
    ckfree((void*)temp_img);
    return TCL_ERROR;
  }
  
  block.width = nc;
  block.height = nr;
  block.pitch = nc;
  block.pixelSize = sizeof(short);
  block.datatype = WORD;
  block.copy = NO_COPY;
  block.skip = 0;
  block.pixel_x = srcMasterPtr->pixel_x;
  block.pixel_y = srcMasterPtr->pixel_y;
  
  block.pixelPtr = (unsigned char*)result;
  
  /* set image size */
  Tk_PictExpand(masterPtr,nc,nr);
  
  Tk_PictPutBlock(masterPtr,&block,0,0,nc,nr);
  
  /* free memory */
  ckfree((void*)temp_img);

  return TCL_OK;
} /* end ImgPictLabel */
#endif

/*
 *----------------------------------------------------------------------
 *
 * Tk_PictPutScaledBlock --
 *
 *	This procedure is called to put image data into a Pict image,
 *	with possible zooming of the pixels.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image data is stored.  The image may be expanded.
 *	The Tk image code is informed that the image has changed.
 *
 *----------------------------------------------------------------------
 */

void
Tk_PictPutScaledBlock(handle, blockPtr, x, y, width, height, zoomX, zoomY,
		      Xoff, Yoff)
    Tk_PictHandle handle;	/* Opaque handle for the Pict image
				 * to be updated. */
    register Tk_PictImageBlock *blockPtr;
				/* Pointer to a structure describing the
				 * pixel data to be copied into the image. */
    int x, y;			/* Coordinates of the top-left pixel to
				 * be updated in the image. */
    int width, height;		/* Dimensions of the area of the image
				 * to be updated. */
    double zoomX, zoomY;	/* Zoom factors for the X and Y axes. */
    double Xoff, Yoff;          /* Offset into initial pixel data */
{
    register PictMaster *masterPtr;
    PictInstance *instancePtr;
    int xEnd, yEnd;
    int wCopy, hCopy;
    unsigned char *srcPtr, *srcLinePtr;
    unsigned char *destPtr, *destLinePtr;
    int pitch;
    double xRepeat, yRepeat;
    int blockXSkip, blockYSkip;
    XRectangle rect;
    register int il;

#ifdef DEBUG
    printf("PowPictPutZoomedBlock\n");
#endif

    if ((zoomX == 1.01) && (zoomY == 1.0)) {
	Tk_PictPutBlock(handle, blockPtr, x, y, width, height);
	return;
    }

    masterPtr = (PictMaster *) handle;

    if ((zoomX <= 0.0) || (zoomY <= 0.0))
	return;
    if ((masterPtr->userWidth != 0) && ((x + width) > masterPtr->userWidth)) {
	width = masterPtr->userWidth - x;
    }
    if ((masterPtr->userHeight != 0)
	    && ((y + height) > masterPtr->userHeight)) {
	height = masterPtr->userHeight - y;
    }
    if ((width <= 0) || (height <= 0))
	return;

    xEnd = x + width;
    yEnd = y + height;
    if ((xEnd > masterPtr->width) || (yEnd > masterPtr->height)) {
	ImgPictSetSize(masterPtr, MAX(xEnd, masterPtr->width),
		MAX(yEnd, masterPtr->height));
    }

  
    if( masterPtr->data == NULL ) {
#ifdef DEBUG
      printf("needs allocation \n");
#endif

      masterPtr->datatype = blockPtr->datatype;
      masterPtr->datasize = blockPtr->pixelSize;
      masterPtr->pixel_x  = blockPtr->pixel_x;
      masterPtr->pixel_y  = blockPtr->pixel_y;

      masterPtr->data = (char*)ckalloc((size_t)masterPtr->datasize*
				      masterPtr->width*
				      masterPtr->height);
      if( masterPtr->data == NULL ) {
	(void)fprintf(stderr,"Could not allocate memory \n");
	return;
      }


    } else {
      if (masterPtr->datatype != blockPtr->datatype ) {
	(void)fprintf(stderr,"Type mismatch \n");
	return;
      }
      if ((masterPtr->pixel_x  != blockPtr->pixel_x) ||
	  (masterPtr->pixel_y  != blockPtr->pixel_y) ) {
	printf("Warning : the physical dimensions of the block being read will not be saved \n");
      }
    }

    destLinePtr = (unsigned char*)(masterPtr->data + 
      (y * masterPtr->width + x)*masterPtr->datasize);
    pitch = masterPtr->width*masterPtr->datasize;

    srcLinePtr = blockPtr->pixelPtr;
    blockXSkip = blockPtr->pixelSize;
    blockYSkip = blockPtr->pitch * blockPtr->pixelSize;
    
    yRepeat = Yoff;
    for (hCopy=height; hCopy > 0; hCopy--) {
       destPtr = destLinePtr;
       srcPtr  = srcLinePtr;
       xRepeat = Xoff;
       for (wCopy=width; wCopy > 0; wCopy--) {
	  for(il=0;il<masterPtr->datasize;il++)
	     *destPtr++ = srcPtr[il];
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
       
    normalize_data(masterPtr);
    blockPtr->pixelPtr = NULL;

    /*
     * Add this new block to the region that specifies which data is valid.
     */

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;
    XUnionRectWithRegion(&rect, masterPtr->validRegion,
	    masterPtr->validRegion);

    /*
     * Update each instance.
     */

    for(instancePtr = masterPtr->instancePtr; instancePtr != NULL;
	instancePtr = instancePtr->nextPtr)
      DitherInstance(instancePtr, x, y, width, height);

    /*
     * Tell the core image code that this image has changed.
     */

    Tk_ImageChanged(masterPtr->tkMaster, x, y, width, height, masterPtr->width,
	    masterPtr->height);
}
