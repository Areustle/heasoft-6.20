/*
 * shared_colors.c  --
 *
 * A source file for Pict images 
 *
 * Copyright (c) 1995 The Regents of the University of California.
 *
 * Author: Pierre-Louis Bossart
 * Date: November 17, 1995
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tkpict.h"

extern Tcl_Interp *interp;

/* definition of variables for the entire module */

static struct XColorSharedStruct { /* Structure for posting color lut info
				    for other clients to share */
   Colormap        cmap;      /* The colormap ID */
   unsigned char   lutStart;  /* The first available LUT value */
   unsigned char   nPC;       /* The number of pseudocolor values used */
};

static Window root_window;

/* definition of global variables */
extern XColor	 lut_colorcell_defs[MAX_COLORS];

/* forward declaration of static functions */
static void show_installed_colormaps(Display *disp); 
static void print_visual_class (Visual *visual);
static void writeSharedColorAtom (Display *disp,Colormap cmap,
				  int lutStart,int nPC);
static Status readSharedColorAtom (Display *disp,Colormap *cmap,
				   int *lutStart,int *nPC);     
static void destroySharedColorAtom(Display *disp);  
static XVisualInfo* pick_visual( XVisualInfo* vis_list,int depth,int num_visuals);


int init_colors(Display *disp,Colormap *colormap,XVisualInfo *vis_info,
		char *colormap_level,
		int *numcolors,int *start_lut,char *atom, Tk_Window tkwin)
{
  int                  depth;
  unsigned long	       *plane_masks;
  unsigned long	       *pixels;
  int                  i;               /* Local looping variables */
  int                  lutStartIndex;      /* Index in pixels for lut_start */
  int                  lutEnd;             /* Last value we will use in LUT */
  Bool                 tfGotColors;        /* True if we got colors from X */
  Colormap             cmap;
  Status	       status;
  unsigned long        blackPix, whitePix; /* Pixel value for black, white */
  int                  screenIndex;   /* Index of screen (0, 1, etc.) */
  int                  lut_start;
  int                  ncolors;
  XColor               *colors;
  int                  colormap_size;
  int                  color_def;


#ifdef DEBUG
  printf("init_colors\n");
#endif

  screenIndex = DefaultScreen(disp);  
  root_window = RootWindow(disp, screenIndex);
  
  plane_masks = (unsigned long *) ckalloc(MAXPLANES*sizeof(unsigned long));
  pixels = (unsigned long *) ckalloc(MAX_COLORS*sizeof(unsigned long));
  if (plane_masks == NULL || pixels == NULL) {
    (void)fprintf(stderr,
		  "\n Unable to allocate storage for init colors\n");
    return 0;
  }

  /* Determine pixel values for black, white in default colormap */
  blackPix = BlackPixel (disp, screenIndex);
  whitePix = WhitePixel (disp, screenIndex);
  depth = vis_info->depth;


  switch( *colormap_level ) {
  case READ_SHARED_COLORMAP:
    /* Get LUT start and length from other client */  
    status = readSharedColorAtom (disp,&cmap, &lut_start, &ncolors);
    
    if (status != Success) 
      return 0;
    else break; /* we still need to set the argument pointers and free the
		   memory allocated */
  case DEFAULT_SCREEN_COLORMAP:
    cmap = DefaultColormap(disp, screenIndex); 
    break;
  case DEFAULT_PRIVATE_COLORMAP:
  case NEW_PRIVATE_COLORMAP: 
    /* Allocate our own colormap */
    /* get 40 colors from default colormap to avoid flashing */
    colormap_size = DisplayCells(disp,screenIndex);
    colors = (XColor*)ckalloc(colormap_size*sizeof(XColor));

    for(i=0;i<colormap_size;i++) {
      pixels[i] = colors[i].pixel = i;
      colors[i].flags = DoRed | DoGreen | DoBlue;
    }

    XQueryColors(disp,DefaultColormap(disp, screenIndex),
		 colors,colormap_size);
    
    cmap = XCreateColormap (disp, root_window, vis_info->visual, AllocNone); 
    /* AllocAll doesn't work quite correctly for X11/NeWS 1.0 */
     
    if (!cmap) {
      printf ("ERROR in init_colors: XCreateColormap returned %x\n", 
	      (unsigned int)cmap);
      return 0;
    }

    colormap_size = 40;
    XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,colormap_size); 
    XStoreColors(disp,cmap,colors,colormap_size);
 
    ckfree((void*)colors);
    break;
  case POW_COLORMAP: 

    cmap = Tk_GetColormap(interp, tkwin, ".pow");

    break;
  }  /* end switch statement */
  
  if( *colormap_level != READ_SHARED_COLORMAP ) {
    /* Try to get a set of contiguous color table entries for our use */
    /* Decrement by 10 cells until we succeed */
  
    if( depth >= 8 ) {   
      tfGotColors = False;           /* Will be set True when we succeed */
      ncolors = 212;
      
      while (ncolors > 10) {
	status = XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,ncolors);
	if (status != 0) {
	  tfGotColors = True;   /* Success.  Break out of the while loop */
	  break;
	}
	ncolors -= 10;      /* Failure.  Decrement request and try again */
      } /* End of while loop */
    }
    else {
      (void)fprintf(stderr,
	      "\nERROR in init_colors: Not enough graphics planes, depth is %d\n",depth) ;
      exit(0);
    }
  }
  
  if( *colormap_level != READ_SHARED_COLORMAP ) {
    if(tfGotColors == False) {/* Case 1: Got no colors */
      ckfree((void*)plane_masks);  plane_masks = NULL;
      ckfree((void*)pixels);       pixels = NULL;
      return 0;
    }
    
    else if((depth == 8) && (ncolors < 50)) {  /* Case 2: not enough colors */
      
      /* Free the colors we got */      
      XFreeColors (disp, cmap, pixels, ncolors, 0);
      ckfree((void*)plane_masks);  plane_masks = NULL;
      ckfree((void*)pixels);       pixels = NULL;
      return 0;
    }
    else	{         /* Case 3: We got enough colors */
      
      /* We got colors, but they may not be contiguous (SGI, for example...).
       * Find the endpoints of the set of contiguous colors ending at the top
       * value of those allocated, and use that segment.
       * Free up the rest of the allocated colors, if any.
       */
      lutStartIndex = ncolors - 1;
      
      for (i=ncolors-1; i>0; i--) {
	if (pixels[i-1] != (pixels[i] - 1)) break;
	lutStartIndex = i-1;
      }
      
      lut_start = pixels[lutStartIndex]; 
      lutEnd = pixels[ncolors-1];
      ncolors = lutEnd - lut_start + 1;  
      
#ifdef DEBUG
      printf("lut start = %d nb colors %d \n",lut_start,ncolors);
#endif

      /* Free the short, unused block of colors */
      if (lutStartIndex != 0) {   
	XFreeColors (disp, cmap, pixels, lutStartIndex, 0);
      } 
      
      /* Send default lut data to server for other clients to use */
      if( *colormap_level == DEFAULT_SCREEN_COLORMAP ) {
	writeSharedColorAtom (disp,cmap, lut_start, ncolors);
	*atom = 1;
	/* We wrote color atom, and should delete
	   it when the colortable is disposed */
      }
    }    /* End of third case */    
  }
  for (color_def=lut_start; color_def<lut_start+ncolors;
       color_def++) {
    lut_colorcell_defs[color_def].pixel = color_def;
    lut_colorcell_defs[color_def].flags = (DoRed | DoGreen | DoBlue);
  }
  
  *colormap = cmap;
  *start_lut = lut_start;
  *numcolors = ncolors;
    
  ckfree((void*)plane_masks);  plane_masks = NULL;
  ckfree((void*)pixels);       pixels = NULL;
  
  return 1;
} /* End of init_colors */

void deinit_disp(Display *disp)
{
  /* This routine deinitializes the display.
    If this copy of the program wrote the X server information about
    the shared colormap, it destroys that information so that the
    colortool and other clients will be aware that no colors are
    reserved for them.
    */

  /* Destroy X server info re shared colormap */
#ifdef DEBUG
  printf(" deinit_disp \n");
  printf("destroying coloratom \n");
#endif

  destroySharedColorAtom(disp);
  XFlush(disp);
} /* end deinit_disp */

static XVisualInfo* pick_visual( XVisualInfo* vis_list,int depth,int num_visuals)
{
  int i=0;
  XVisualInfo *vis_found;

#ifdef DEBUG
  printf("pick_visual\n");
#endif

  for(i=0,vis_found=vis_list;i<num_visuals;i++,vis_found++) {
    if( vis_found->depth >= depth )
      return(vis_found);
  }
  return(NULL);
} /* end pick_visual */


XVisualInfo *get_visual(Display *disp) 
     /* Pointer to list of matching visuals */
{
  XVisualInfo vTemplate;   /* Template of the visual we want */
  XVisualInfo *visualList; /* Pointer to list of matching visuals */
  int visualsMatched;      /* Number of visuals matched */
  int screenIndex;
  int depth;
  XVisualInfo *vis_found,*vis_found1;

#ifdef DEBUG
  printf("get_visual\n");
#endif

  screenIndex = DefaultScreen(disp);       /* Screen index */
  depth = 8; /* need at least 8 planes */
  root_window	= RootWindow(disp, screenIndex);
 
  /* Set up the template for getting visual info.  Select some or
     all of these depending on the VisualMask. */
  
  vTemplate.screen = screenIndex;  
  vTemplate.class = PseudoColor;  
  
  /* Find all PseudoColor visuals */
  visualList = XGetVisualInfo (disp,
			       VisualScreenMask  | VisualClassMask,
			       &vTemplate,
			       &visualsMatched);

  if (visualList == NULL) {
    /*  This is recoverable, so don't print a message to stderr
    (void)fprintf (stderr,"get_visual:  No PseudoColor visuals found .  \n");
    */
    return NULL;
    /* exit (0);*/
  }
  if( (vis_found=pick_visual((XVisualInfo *)visualList,depth,visualsMatched)) == NULL) {
    /*  This is recoverable, so don't print a message to stderr
    (void)fprintf (stderr,"get_visual:  No PseudoColor visuals with depth at least 8 found .  \n");
    */
    return NULL;
    /*exit (0);*/
  }

  vTemplate.screen = vis_found->screen;
  vTemplate.class = vis_found->class;
  vTemplate.depth = vis_found->depth;
  vis_found1 = XGetVisualInfo (disp,
			       VisualScreenMask  | VisualClassMask | VisualDepthMask,
			       &vTemplate,
			       &visualsMatched);
  XFree(visualList);
  if( vis_found1 == NULL) {
    /*  This is recoverable, so don't print a message to stderr
    (void)fprintf (stderr,"get_visual:  No PseudoColor visuals with depth at least 8 found .  \n");
    */
    return NULL;
    /*exit (0);*/
  }
  return(vis_found1);        

} /* End of "get_visual" */


static void show_installed_colormaps(Display *disp)
{
  Colormap *colorMapList;  /* Pointer to list of installed colormaps */
  int i;                   /* Local looping variable */
  int num;                 /* Number of returned colormaps */

#ifdef DEBUG 
  printf ("show_installed_colormaps\n");
#endif

  XFlush(disp) ;
  colorMapList = XListInstalledColormaps (disp, root_window, &num);
  XFlush(disp) ;
  printf ("Number of installed colormaps in root_window (%x) =%d\n",
	  (unsigned int)root_window, num);
  for (i=0; i<num; i++) {
    printf("colorMapList[%d]=%x\n", i,(unsigned int)( colorMapList[i]));
  }
  XFree (colorMapList);    /* Free the list of colormaps */
  return;
}




/* Print the type of a visual to stdout */
static void print_visual_class (Visual *visual)
{
  switch (visual->class) {
  case PseudoColor:
    printf ("Visual class is PseudoColor\n");
    break;
  case StaticColor:
    printf ("Visual class is StaticColor\n");
    break;
  case DirectColor:
    printf ("Visual class is DirectColor\n");
    break;
  case TrueColor:
    printf ("Visual class is TrueColor\n");
    break;
  case GrayScale:
    printf ("Visual class is GrayScale\n");
    break;
  case StaticGray:
    printf ("Visual class is StaticGray\n");
    break;
  default:
    printf ("Visual class is not known\n");
    break;
  }
  return;
}/* end of "print_visual_class" */



/* Write the shared color atom to give info about the shared colorlut */
void writeSharedColorAtom (Display *disp,Colormap cmap,int lutStart,int nPC)
{
   Atom       colormapAtom;  /* Atom for specifying colormap info to server */
   struct XColorSharedStruct XColorShared;

   XColorShared.cmap = cmap;
   XColorShared.lutStart = lutStart;
   XColorShared.nPC = nPC;
 
#ifdef DEBUG
   printf("writeSharedColorAtom\n");
#endif

   /* Post the colormap in atom VIEW_COLORMAP */
   /* Create the atom */
   colormapAtom = XInternAtom (disp, "VIEW_COLORMAP", False);

   if (colormapAtom == None) {
#ifdef DEBUG
     (void)fprintf(stderr,
	     "ERROR in writeSharedColorAtom: XInternAtom returned None (%d)\n",
	     (int)colormapAtom);
#endif
     return; 
   }

   /* Store XColorShared in the atom */
   XChangeProperty (disp, root_window, colormapAtom, XA_STRING, 8,
		    PropModeReplace, (unsigned char*)&XColorShared, sizeof(XColorShared));
   return;
}
/* End of "writeSharedColorAtom" */



/* Read the shared color atom to get information about the shared colorlut */
Status readSharedColorAtom (Display *disp,
			    Colormap *cmap, /* The colormap ID */
			    int *lutStart, /* The first available LUT value */
			    int *nPC)      /* The number of pseudocolor values used */
{
   Atom       colormapAtom;  /* Atom for specifying colormap info to server */
   int status;
   Atom actualType;
   int actualFormat;
   unsigned long nitems;
   unsigned long bytesAfter;
   struct XColorSharedStruct *theColorShared=NULL;

#ifdef DEBUG
   printf("readSharedColorAtom\n");
#endif

   /* Create the atom */
   colormapAtom = XInternAtom (disp, "VIEW_COLORMAP", True);

   if (colormapAtom == None) {
#ifdef DEBUG
     printf("ERROR in readSharedColorAtom: XInternAtom returned None (%d)\n",
	    (unsigned int)colormapAtom);
#endif
     cmap = NULL;
     *lutStart = *nPC = 0;
     return BadAtom;
   }

   status = XGetWindowProperty (disp, root_window, colormapAtom,
				0L, 1000L, False, AnyPropertyType,
				&actualType, &actualFormat, &nitems,
				&bytesAfter,(unsigned char **)&theColorShared);
   
   if ((status == Success) && (theColorShared != NULL)) {
     *cmap = theColorShared->cmap;
     *lutStart = theColorShared->lutStart;
     *nPC = theColorShared->nPC;
     
     XFree (theColorShared); 
     theColorShared=NULL;
     return status;
   }
   else {
      
     switch (status) {
     case Success:
       if (actualType == None) {
	 /* printf ("actualType=None\n"); */
	 return BadAtom;
       }
       break;
     case BadAtom :
       printf("bad atom\n");
       break;
     case BadMatch :
       printf("bad match\n");
       break;
     case BadValue :
       printf("bad value\n");
       break;
     case BadWindow :
       printf("bad window\n");
       break;
     default:
       printf("bad other\n");
       break;
     }              /* End of switch (status) */
	 
     printf("ERROR in readSharedColorAtom: XGetWindowProperty returned %d\n",
	    status);
     cmap = NULL;
     *lutStart = *nPC = 0;
     return status;
   }
 }
/* End of readSharedColorAtom */


/* Destroy the shared colormap atom, in preparation to exit the program */
void destroySharedColorAtom(Display *disp) 
{
  Atom       colormapAtom;  /* Atom for specifying colormap info to server */

#ifdef DEBUG
  printf("destroySharedColorAtom\n");
#endif

  /* Create the atom */
  colormapAtom = XInternAtom (disp, "VIEW_COLORMAP", True);

  if (colormapAtom == None) {
    printf("ERROR in destroySharedColorAtom: XInternAtom returned None (%d)\n",
	   (int)colormapAtom);
  }

  /* Destroy the property, in preparation to exit */
  XDeleteProperty (disp, root_window, colormapAtom);
  return;
}
/* End of "destroySharedColorAtom" */
