/*
 * colors.c --
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


extern XColor	 lut_colorcell_defs[256];
extern int ncolors;
extern int lut_start;

static int Default_Shared_Allocated  = 0;
static int Default_Screen_Allocated  = 0;
static int Default_Private_Allocated = 0;
int Pow_Allocated = 0;

static PictColorTable *DefaultSharedColorTable;
static PictColorTable *DefaultScreenColorTable;
static PictColorTable *DefaultPrivateColorTable;
extern PictColorTable *PowColorTable;

/*
 *----------------------------------------------------------------------
 *
 * DitherInstance --
 *
 *	This procedure is called to update an area of an instance's
 *	pixmap by dithering the corresponding area of the master.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The instance's pixmap gets updated.
 *
 *----------------------------------------------------------------------
 */


void
DitherInstance(instancePtr, xStart, yStart, width, height)
    PictInstance *instancePtr;	/* The instance to be updated. */
    int xStart, yStart;		/* Coordinates of the top-left pixel in the
				 * block to be dithered. */
    int width, height;		/* Dimensions of the block to be dithered. */
{
    PictMaster *masterPtr;
    PictColorTable *colorTable;
    XImage *imagePtr;
    int nLines;
    int i, c, x, y;
    int xEnd, yEnd;
    int bitsPerPixel, bytesPerLine, lineLength;
    unsigned char *srcLinePtr, *srcPtr;
    unsigned char *destBytePtr, *dstLinePtr;
    pixel *destLongPtr;
   
 
#ifdef DEBUG
    printf("DitherInstance\n");
#endif

    masterPtr = instancePtr->masterPtr;
    colorTable = instancePtr->colorTable;

    /*
     * First work out how many lines to do at a time,
     * then how many bytes we'll need for pixel storage,
     * and allocate it.
     */

    nLines = (MAX_PIXELS + width - 1) / width;
    if (nLines < 1) {
	nLines = 1;
    }
    if (nLines > height ) {
	nLines = height;
    }

    imagePtr = instancePtr->imagePtr;
    if (imagePtr == NULL) {
	return;			/* we must be really tight on memory */
      }
    bitsPerPixel = imagePtr->bits_per_pixel;
    bytesPerLine = ((bitsPerPixel * width + 31) >> 3) & ~3;
    imagePtr->width = width;
    imagePtr->height = nLines;
    imagePtr->bytes_per_line = bytesPerLine;
    imagePtr->data = (char *) ckalloc((unsigned) (imagePtr->bytes_per_line * nLines));
    if( imagePtr->data == NULL ) {
      (void)fprintf(stderr,"DitherInstance: ckalloc failed \n");
      return;
    }

    lineLength = masterPtr->width;
    srcLinePtr = masterPtr->bytedata + (yStart + height - 1) * lineLength + xStart;
    xEnd = xStart + width;

    
    if (bitsPerPixel > 1) {
      /*
     * Loop over the image, doing at most nLines lines before
     * updating the screen image.
     */

      for (; height > 0; height -= nLines) {
	if (nLines > height) {
	  nLines = height;
	}
	dstLinePtr = (unsigned char *) imagePtr->data;
	yEnd = yStart + nLines;
	for (y = yStart; y < yEnd; ++y) {
	  srcPtr = srcLinePtr;
	  destBytePtr = dstLinePtr;
	  destLongPtr = (pixel *) dstLinePtr;
	  	   
	  for (x = xStart; x < xEnd; ++x) {
	    c = srcPtr[0];
	    srcPtr += 1;
	    if (c < 0) {
	      c = 0;
	    } else if (c > 255) {
	      c = 255;
	    }
		
	    i = colorTable->redValues[c];
	    switch (bitsPerPixel) {
	    case NBBY:
	      *destBytePtr++ = i;
	      break;
	    case NBBY * sizeof(pixel):
	      *destLongPtr++ = i;
	      break;
	    default:
	      XPutPixel(imagePtr, x - xStart, y - yStart,
			(unsigned) i);
	    }
	  }
	
	  srcLinePtr -= lineLength;
	  dstLinePtr += bytesPerLine;
      
	}
	/* Update the pixmap for this instance with the block of
	   pixels that we have just computed. */
	XPutImage(instancePtr->display, instancePtr->pixels,
		  instancePtr->gc, imagePtr, 0, 0, xStart, yStart,
		  (unsigned) width, (unsigned) nLines);
	yStart = yEnd;
      }
    }
    ckfree(imagePtr->data);
    imagePtr->data = NULL;
  
} /* end DitherInstance */








int GetColorTable(Tk_Window tkwin,
		  PictColorTable **colorTable,
		  XVisualInfo **visInfoPtr)
{ 
  Display *disp;
  Colormap cmap;
  
  int ncolors;
  int lut_start;
  char colormap_level;
  char atom = 0;
  static int gave_message=0;

  disp = Tk_Display(tkwin);
  (*visInfoPtr) = get_visual(disp);
  if ((*visInfoPtr) == NULL) {
    (void)fprintf (stderr,"GetColorTable:  No PseudoColor visuals found .  \n");
    exit (0);
  }
  colormap_level = Private_Colormap;
  
  /* first make sure the default Display is PseudoColor */
  if( ((*visInfoPtr)->visual != DefaultVisual(disp,DefaultScreen(disp))) 
     && (colormap_level < DEFAULT_PRIVATE_COLORMAP) ) {
    (void)fprintf (stderr,
		   "ERROR: Default Display is not PseudoColor \n Allocating a Shareable Private Colormap instead \n");
    colormap_level = DEFAULT_PRIVATE_COLORMAP;
  }
  
  /* shared colormap */
  if(colormap_level == READ_SHARED_COLORMAP) {
    if( Default_Shared_Allocated == 0 ) {
      if( !init_colors(disp,&cmap,(*visInfoPtr),
		       &colormap_level,&ncolors,&lut_start,&atom,tkwin) ) {
	(void)fprintf (stderr,
		       "ERROR: no shared colormap exists.\n");
	(void)fprintf (stderr,
		       "Using the default colormap instead \n");
	colormap_level = DEFAULT_SCREEN_COLORMAP;
      }
      else { 
	if( !AllocateColorTable((PictColorTable **)&DefaultSharedColorTable,
				disp,cmap,colormap_level,ncolors,lut_start,atom) )
	  return 0;
	else {
	  Default_Shared_Allocated = 1;
	  *colorTable = DefaultSharedColorTable;
	}
      }
    }
    else { /* use existing color table */
      DefaultSharedColorTable->refCount ++;
      *colorTable = DefaultSharedColorTable;
    }
  } /* end shared colormap*/

  /* default screen colormap */
  if(colormap_level == DEFAULT_SCREEN_COLORMAP) {
    if( Default_Screen_Allocated == 0 ) {
      if( !init_colors(disp,&cmap,(*visInfoPtr),
		       &colormap_level,&ncolors,&lut_start,&atom,tkwin) ) {
	/* from deway garett: eliminate multiple messages on stderr:*/
	if (!gave_message) {
	  gave_message=1;
	  (void)fprintf(stderr,"ERROR: not enough colors in screen Default Colormap\n");
	  (void)fprintf(stderr,"Creating a default private colormap instead \n");
	}
	colormap_level = DEFAULT_PRIVATE_COLORMAP;
      }
      else {
	if( !AllocateColorTable((PictColorTable **)&DefaultScreenColorTable,
				disp,cmap,colormap_level,ncolors,lut_start,atom) )
	  return 0;
	else {
	  Default_Screen_Allocated = 1;
	  *colorTable = DefaultScreenColorTable;
	}
      }
    }
    else { /* use existing color table */
      DefaultScreenColorTable->refCount += 1;
      *colorTable = DefaultScreenColorTable;
    }
  } /* end default screen colormap */

  /* shared private colormap */
  if(colormap_level == DEFAULT_PRIVATE_COLORMAP) {
    if( Default_Private_Allocated == 0 ) {
      if( !init_colors(disp,&cmap,(*visInfoPtr),
		       &colormap_level,&ncolors,&lut_start,&atom,tkwin) ) {
	(void)fprintf(stderr,"init_colors failed \n");
	return 0;
      }
      else {
	if( !AllocateColorTable((PictColorTable **)&DefaultPrivateColorTable,
				disp,cmap,colormap_level,ncolors,lut_start,atom) )
	  return 0;
	else { 
	  Default_Private_Allocated = 1;
	  *colorTable = DefaultPrivateColorTable;
	}
      }
    }
    else { /* use existing color table */
      DefaultPrivateColorTable->refCount += 1;
      *colorTable = DefaultPrivateColorTable;
    }
  } /* end shared private colormap */

  /* private colormap */
  if(colormap_level == NEW_PRIVATE_COLORMAP) {
    if( !init_colors(disp,&cmap,(*visInfoPtr),
		     &colormap_level,&ncolors,&lut_start,&atom,tkwin) ) {
      (void)fprintf(stderr,"init_colors failed \n");
      return 0;
    } 
    else {
      if( !AllocateColorTable((PictColorTable **)colorTable,
			      disp,cmap,colormap_level,ncolors,lut_start,atom) )
	return 0;
    }
  } /* end private colormap */

  /* POW colormap */
  if(colormap_level == POW_COLORMAP) {
    if( Pow_Allocated == 0 ) {
      if( !init_colors(disp,&cmap,(*visInfoPtr),
		       &colormap_level,&ncolors,&lut_start,&atom,tkwin) ) {
	(void)fprintf(stderr,"init_colors failed \n");
	return 0;
      } 
      else {
	if( !AllocateColorTable((PictColorTable **)&PowColorTable,
				disp,cmap,colormap_level,ncolors,lut_start,atom) )
	  return 0;
	else { 
	  Pow_Allocated = 1;
	  *colorTable = PowColorTable;
	}
      }
    } else {
      /* use existing color table */
      PowColorTable->refCount ++;
      *colorTable = PowColorTable;
    }
  } /* end POW colormap */

  /* change default back to screen colormap. This is useless in visu,
     since the scripts always check for the colormap level of the
     current image. This might be useful in the future though */
  /*  Private_Colormap = DEFAULT_SCREEN_COLORMAP;*/
    
  /* set window attributes */
  if( Tk_SetWindowVisual(tkwin, (*visInfoPtr)->visual,(*visInfoPtr)->depth,
			 (*colorTable)->colormap) == 0 ){

    /* window already exists. See if the new colormap can be installed */
    /*    if(((*visInfoPtr)->visual == (((Tk_FakeWin *)(tkwin)))->visual) &&
       ((*visInfoPtr)->depth  == (((Tk_FakeWin *)(tkwin)))->depth)) { */
    /*Let's try not doing any testing, HPUX barfs if we do*/

      Tk_SetWindowColormap(tkwin,(*colorTable)->colormap); 

      /*    }
    else {
      (void)fprintf(stderr,"Window already exists with Visual class %d and depth %d. Change the Tk script to create image widget with PseudoColor 8 visual\n",
                    ((Tk_FakeWin *)(tkwin))->visual->class,
		    ((Tk_FakeWin *)(tkwin))->depth); 
      return 0;
    } */
  } else {
    /* The visual was set, but the colormap needs to be installed. The
       window managers should do it but for now let's do it ourselves */
    /* Tk_SetWindowColormap(tkwin,(*colorTable)->colormap); */
  }
  return 1;
}



int DisposeColorTable(PictColorTable *colorTable)
{
  unsigned long *pixels;
  int i,j;
  /* if still instances using colorTable  *or* if using POW Colormap, don't
     do this.  pict images in POW use the POW windows colormap.  Once
     allocated, the POW colorTable should remain (as far as VISU is 
     concerned).  The Pow colorTable is handled in PowCleanUp if POW is
     exitted. */
  if(colorTable->refCount != 0 ||  colorTable->colormap_level == POW_COLORMAP)
    return 0;

  /* destroy atom */
  if(colorTable->atom == 1) 
    deinit_disp(colorTable->display);

  if(colorTable->colormap_level > READ_SHARED_COLORMAP) {

    pixels = (unsigned long *)ckalloc(colorTable->ncolors*
				     sizeof(unsigned long));
    if( pixels == NULL )
      return 0;

    /* create pixel array */
    for(j=colorTable->lut_start,i=0;i<colorTable->ncolors;i++,j++)
      pixels[i] = j;

    /* free colors */
    XFreeColors(colorTable->display,colorTable->colormap,
		pixels,colorTable->ncolors,0);
    ckfree((void*)pixels);

    /* free colormap */
    XFreeColormap(colorTable->display,
		  colorTable->colormap);
  }

  if( colorTable->colormap_level ==  READ_SHARED_COLORMAP )
    Default_Shared_Allocated = 0;
  if( colorTable->colormap_level == DEFAULT_SCREEN_COLORMAP)
    Default_Screen_Allocated = 0;
  else if( colorTable->colormap_level == DEFAULT_PRIVATE_COLORMAP )
    Default_Private_Allocated = 0;

  ckfree((void*)colorTable);
  colorTable = NULL;

  return 1;
} /* end DisposeColorTable */

 
  
     
