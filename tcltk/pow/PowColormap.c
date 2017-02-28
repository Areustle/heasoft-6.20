#include "pow.h"

extern XColor	 lut_colorcell_defs[256];
extern PictColorTable *PowColorTable;

int PowSetupColormap(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[] ) {
  /* This routine creates a toplevel window with a colormap for that
     window which has the specified number of cells free in addition to those
     needed by POW.  If force_cmap is non-zero:

     0 - Default behavior.  Choose the "best" pseudocolor map with enough
         colorcells available.

     1 - Force a new private pseudocolor colormap.

     2 - Force truecolor mode.  Note, the *images* are truecolor, the visual
         associated with the POW colormap may be anything,
	  "toplevel -visual best" is used to choose the visual.
   
     3 - Force screen default colormap (WARNING: this may crash application
         if the default colormap is pseudocolor, or not if you're lucky, or 
         VISU may barf at image startup, you getting the idea that I don't
         recommend this option; if it's truecolor, this should work as well 
         as option 2 does. )

     Returns toplevel path name.
     All of an application's windows should then use the colormap of this
     toplevel. */
#if !(defined(__WIN32__) || defined(macintosh))
  Tk_Window dotwin;	
  Tk_Window tkwin;
  Screen *screen;
  Display *disp;
  Colormap cmap;
  int i;
  XColor *colors;
  int colormap_size;
  int screenIndex;
  unsigned long *pixels;
  unsigned long *plane_masks;
  XVisualInfo *visual_info;
  int ncolors;
  Status status;
  Bool tfGotColors;        /* True if we got colors from X */
  int powCells;
#endif
  char *toplevel;
  char *options;
  int free_cells;
  int force_cmap; 

  if (argc == 2 && (!strcmp(argv[1],"none") || !strcmp(argv[1],"NULL"))) {
    /* do nothing */
    return TCL_OK;
  }



  if (argc > 5 || argc < 3 ) {
    Tcl_SetResult(interp, "usage: powSetupColormap toplevel_name free_cells ?force_cmap? ?options_list?", TCL_VOLATILE);
    return TCL_ERROR;
  }


  toplevel = ckalloc(strlen(argv[1])+5);
  strcpy(toplevel,argv[1]);

  

  Tcl_GetInt(interp,argv[2],&free_cells);

  if (argc >= 4) {
    Tcl_GetInt(interp,argv[3],&force_cmap);
  } else {
    force_cmap = 0;
  }



  if (argc == 5) {
    options = ckalloc(strlen(argv[4])+1);
    strcpy(options,argv[4]);
  } else {
    options = ckalloc(1);
    *options = '\0';
  }
#if defined(__WIN32__) || defined(macintosh)
  /*In WIN32/mac, just create the toplevel. Pseudocolor mode isn't supported
    in WIN32/mac because the necessary Xlib calls are missing.*/
  Tcl_SetVar(interp,"powPseudoImages","0",TCL_GLOBAL_ONLY); 
  return Tcl_VarEval(interp, "toplevel ",toplevel,
                     " -visual default ",options,(char *)NULL);
/*
  return Tcl_VarEval(interp, "toplevel ",toplevel,
                     " -visual best ",options,(char *)NULL);
*/
#else

  if (force_cmap == 2) {
    Tcl_SetVar(interp,"powPseudoImages","0",TCL_GLOBAL_ONLY); 
    return Tcl_VarEval(interp, "toplevel ",toplevel,
                       " -visual default ",options,(char *)NULL);
/*
    return Tcl_VarEval(interp, "toplevel ",toplevel,
                       " -visual best ",options,(char *)NULL);
*/
  }

  /* find out some things about the screen*/

  dotwin = Tk_NameToWindow(interp,".",Tk_MainWindow(interp));
  screen=Tk_Screen(dotwin);
  disp = Tk_Display(dotwin);
  screenIndex=DefaultScreen(disp);

  if (force_cmap == 3) {
    /*Force default visual. */

    return Tcl_VarEval(interp, "toplevel ",toplevel," -visual default ", 
		       options,(char *)NULL);

    /* We want to know if the default visual is pseudocolor. 
       Finding this out in Xlib is stupidly complicated, so we'll
       let Tk do it for us */
    Tcl_Eval(interp,"string match \"pseudocolor\" [winfo screenvisual .]");
    if (Tcl_GetStringResult(interp) != "1") {
      Tcl_SetVar(interp,"powPseudoImages","0",TCL_GLOBAL_ONLY); 
    }
    return TCL_OK;
  }
  
  visual_info = get_visual(disp);  

  if (visual_info == NULL) {
    /* Is this really an error???
    (void)fprintf (stderr,"powSetupColormap: toplevel created with non-pseudocolor visual. \n");
    */
    Tcl_SetVar(interp,"powPseudoImages","0",TCL_GLOBAL_ONLY); 
    return Tcl_VarEval(interp, "toplevel ",toplevel," -visual default ", 
		       options,(char *)NULL);
/*
    return Tcl_VarEval(interp, "toplevel ",toplevel," -visual best ", 
		       options,(char *)NULL);
*/
  }


  plane_masks = (unsigned long *) ckalloc(MAXPLANES*sizeof(unsigned long));
  pixels = (unsigned long *) ckalloc(MAX_COLORS*sizeof(unsigned long));
  if (plane_masks == NULL || pixels == NULL) {
    (void)fprintf(stderr,
		  "\n Unable to allocate storage for PowSetupColormap\n");
    return TCL_ERROR;
  }


  tfGotColors = False;           /* Will be set True when we succeed */

  if (force_cmap != 1) {
  
    /* Try default Colormap first */
    cmap = DefaultColormap(disp, screenIndex); 
    
    ncolors = 212; /* why not 256, I don't know, but since we have to
		      match whatever VISU does and I don't want to
		      mess with that.... */
    
    while (ncolors > 10) {
      status = XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,ncolors);
      if (status != 0) {
	tfGotColors = True;   /* Success.  Break out of the while loop */
	break;
	
      }
      ncolors -= 10;      /* Failure.  Decrement request and try again */
    } /* End of while loop */
  }
    
 Tcl_GetInt(interp,Tcl_GetVar(interp,"powMinColorcells",TCL_GLOBAL_ONLY),
	    &powCells);

  if(force_cmap != 1  && ncolors >= powCells + free_cells) {  /* enough colors , use default map */
    /* First free the colors we got */      
    XFreeColors (disp, cmap, pixels, ncolors, 0);
    ckfree((void*)plane_masks);  plane_masks = NULL;
    ckfree((void*)pixels);       pixels = NULL;
    return Tcl_VarEval(interp, "toplevel ",toplevel,
		       options,(char *)NULL);
  } else {         /* not enough colors */
    
    /* Free the colors we did get */
    if(tfGotColors == True) {
      XFreeColors (disp, cmap, pixels, ncolors, 0);
    }
    
    
    tkwin = Tk_CreateWindowFromPath(interp,dotwin,".powCmap",NULL);
    

    if (tkwin == NULL) {
      (void)fprintf(stderr,
		    "\n Couldn't create dummy window for PowSetupColormap\n");
      return TCL_ERROR;
    }

    /*  This prevents a seg-fault from occuring at a 'winfo class' command  */
    Tk_SetClass( tkwin, "PowCmapDmy" );
    
    
    colormap_size = DisplayCells(disp,screenIndex);
    colors = (XColor*)ckalloc(colormap_size*sizeof(XColor));
    
    for(i=0;i<colormap_size;i++) {
      pixels[i] = colors[i].pixel = i;
      colors[i].flags = DoRed | DoGreen | DoBlue;
    }
    
    XQueryColors(disp,DefaultColormap(disp, screenIndex),
		 colors,colormap_size);
    
    
    cmap = XCreateColormap (disp, RootWindow(disp,screenIndex),
			    visual_info->visual , AllocNone); 
    
    if (!cmap) {
      printf ("ERROR in PowSetupColormap: XCreateColormap returned %x\n", 
	      (unsigned int)cmap);
      return TCL_ERROR;
    }
    
    
    /* 
       Not too sure of the purpose of all this... but it seems to be
       reserving part of the windows private colormap.  Visu later grabs
       the necessary colors for the colormaps.  So, for greater color
       range, need to reserve as little as possible here.  That is what
       free_cells should be doing. (PDW 10/23/00)
    */

    /*  colormap_size = 212 - powCells - free_cells;  */

    colormap_size = free_cells;
    XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,colormap_size); 
    XStoreColors(disp,cmap,colors,colormap_size);
    

    ckfree((void*)plane_masks);  plane_masks = NULL;
    ckfree((void*)pixels);       pixels = NULL;
    ckfree((void*)colors);

    Tk_SetWindowColormap(tkwin,cmap);
  
    return Tcl_VarEval(interp, "toplevel ",toplevel,
		       " -colormap .powCmap ",options,(char *)NULL);
  }
#endif /* __WIN32__ || macintosh */
}





int PowTestColormap(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[] ) {
  /* This routine returns the number of free colors in the colormap of
     the specified window */
#if defined(__WIN32__) || defined(macintosh)
  /* This routine does nothing in WIN32 or MacOS */
  return TCL_OK;
#else
  Tk_Window tkwin;
  Display *disp;
  Colormap cmap;
  unsigned long *pixels;
  unsigned long *plane_masks;
  char *window;
  int ncolors;
  Status status;
  Bool tfGotColors;        /* True if we got colors from X */

  if (argc != 2 ) {
    Tcl_SetResult(interp, "usage: powTestColormap window", TCL_VOLATILE);
    return TCL_ERROR;
  }


  window = ckalloc(strlen(argv[1])+1);
  strcpy(window,argv[1]);


  plane_masks = (unsigned long *) ckalloc(MAXPLANES*sizeof(unsigned long));
  pixels = (unsigned long *) ckalloc(MAX_COLORS*sizeof(unsigned long));
  if (plane_masks == NULL || pixels == NULL) {
    (void)fprintf(stderr,
		  "\n Unable to allocate storage for PowTestColormap\n");
    return TCL_ERROR;
  }


  /* find out some things about the screen*/

  tkwin = Tk_NameToWindow(interp,window,Tk_MainWindow(interp));
  disp = Tk_Display(tkwin);

  tfGotColors = False;
  
  cmap = Tk_Colormap(tkwin);
  
  ncolors = 256;


  while (ncolors > 0) {
    status = XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,ncolors);
    if (status != 0) {
      tfGotColors = True;   /* Success.  Break out of the while loop */
      break;

    }
    ncolors -= 1;      /* Failure.  Decrement request and try again */
  } /* End of while loop */
  
  
    
  /* First free the colors we got */      
  if (tfGotColors == True) {
    XFreeColors (disp, cmap, pixels, ncolors, 0);
  }
  ckfree((void*)plane_masks);  plane_masks = NULL;
  ckfree((void*)pixels);       pixels = NULL;
  sprintf(Tcl_GetStringResult(interp),"%i",ncolors);
  return TCL_OK;
#endif /* __WIN32__ || macintosh */
}


int PowSetupPhotoImages(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[] ) {
  int ncolors=256;
  int lut_start=0;
  int color_def;

  /*We're going to use the Colortable routines in VISU to setup our colortables
    and fake out the X stuff */

  for (color_def=lut_start; color_def<lut_start+ncolors;
       color_def++) {
    lut_colorcell_defs[color_def].pixel = color_def;
    lut_colorcell_defs[color_def].flags = (DoRed | DoGreen | DoBlue);
  }


  AllocateColorTable((PictColorTable **)&PowColorTable,(Display*)NULL,
		     (Colormap)0L,POW_COLORMAP,
		     ncolors,lut_start,0);
  return TCL_OK;
}
  


void PowDitherToPhoto(PowImage *powImage, Tk_PhotoImageBlock *photoBlock,
		      double disp_min, double disp_max) 
{
  /* Dithers the powImage data to a Tk_PhotoBlock.  Calls 
     convert_block_to_byte and uses lut_colorcell_defs global. */
  unsigned char *dithered_data_array;
  unsigned char *photo_image_array;
  unsigned char *inptr;
  unsigned char *outptr;
  void *in_data;
  int width,height;
  int data_type;
  int col,i,j;

  width = powImage->width;
  height = powImage->height;

  in_data = (powImage->dataptr)->data_array;

  data_type = (powImage->dataptr)->data_type;

  dithered_data_array = (unsigned char *)ckalloc(sizeof(char)*width*height);

  convert_block_to_byte(in_data,dithered_data_array,width*height,data_type,
			&disp_min, &disp_max);

  photo_image_array = (unsigned char *)ckalloc(3*sizeof(char)*width*height);

  inptr = dithered_data_array;
  outptr = photo_image_array;

  for (i=0; i<height; i++) {
    inptr = dithered_data_array + i*width;
    outptr = photo_image_array + (height - i - 1) * width * 3;
    for ( j = 0; j < width; j++) {
      col = *inptr++;
      *outptr++ = lut_colorcell_defs[col].red   >> 8;
      *outptr++ = lut_colorcell_defs[col].green >> 8;
      *outptr++ = lut_colorcell_defs[col].blue  >> 8;
    }
  }


  ckfree(dithered_data_array);
  
  photoBlock->pixelPtr = photo_image_array;

  return;
    
}

int PowGetHisto(ClientData clientData, Tcl_Interp *interp, 
                int argc, Tcl_Obj *const argv[])
{
   /* usage: powGetHisto imageName */
  
   PowImage *powImage;
   void *in_data;
   int data_type;
   int i;
   int histo1[MAX_LOOKUP], histo2[256], totalPix, level;
   double min, max;
   Tcl_Obj *list, *val;

   if(argc != 4) {
      Tcl_SetResult(interp, "usage: powGetHisto image min max",
                    TCL_VOLATILE);
      return TCL_ERROR;
   }

   powImage = PowFindImage( Tcl_GetStringFromObj(argv[1], NULL) );
   if( !powImage ) {
      Tcl_AppendResult( interp, "Unable to find image ",
                        Tcl_GetStringFromObj(argv[1],NULL), (char*)NULL );
      return TCL_ERROR;
   }
   Tcl_GetDoubleFromObj(interp, argv[2], &min);
   Tcl_GetDoubleFromObj(interp, argv[3], &max);

   totalPix  = powImage->width * powImage->height;
   in_data   = (powImage->dataptr)->data_array;
   data_type = (powImage->dataptr)->data_type;

   /*  Calculate histogram  */

   convert_block_to_histo(in_data, totalPix, data_type, &min, &max, (unsigned int *) histo1);
   for( i=0; i<256; i++ ) histo2[i]=0;
   for( i=0; i<MAX_LOOKUP; i++ ) {
      histo2[ i * 256 / MAX_LOOKUP ] += histo1[i];
   }

   list = Tcl_NewListObj( 0, NULL );
   for( level=0; level<256; level++ ) {
      val = Tcl_NewIntObj( histo2[level] );
      Tcl_ListObjAppendElement( interp, list, val );
   }

   Tcl_SetObjResult( interp, list );
   return TCL_OK;
}

int PowPhotoColorTable (ClientData clientData, Tcl_Interp *interp,
			int argc, char *argv[])
{
  void (*f)(Display *display,Colormap cmap,
	    int ncolors,int lut_start,char overlay,
	    int *red,int *green,int *blue,
	    int *intensity_lut,int *red_lut,int *green_lut,
	    int *blue_lut);

  if (argc != 2) {
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     argv[0], " cmap\"", (char *) NULL);
    return TCL_ERROR;
  }

  if(strcmp(argv[1], "gray") == 0)
    f = gray;
  else if(strcmp(argv[1], "blkbdy") == 0)
    f = blkbdy;
  else if(strcmp(argv[1], "hot") == 0)
    f = hot;
  else if(strcmp(argv[1], "cold") == 0)
    f= cold;
  else if(strcmp(argv[1], "hls") == 0)
    f = hls;
  else if(strcmp(argv[1], "rgb") == 0)
    f = rgb;
  else if(strcmp(argv[1], "invert") == 0)
    f = invert_cmap;
  else if(strcmp(argv[1], "random") == 0)
    f = randwalk_spectrum;
  else if(strcmp(argv[1], "bowlerhat") == 0)
    f = bowlerhat;
  else if(strcmp(argv[1], "tophat") == 0)
    f = tophat;
  else if(strcmp(argv[1], "hatgray") == 0)
    f = hatgray;
  else if(strcmp(argv[1], "hatct") == 0)
    f = hatct;
  else if(strcmp(argv[1], "gray-ramp2") == 0)
    f = gray_ramp2;
  else if(strcmp(argv[1], "gray-ramp4") == 0)
    f = gray_ramp4;
  else if(strcmp(argv[1], "gray-step4") == 0)
    f = gray_step4;
  else if(strcmp(argv[1], "gray-step8") == 0)
    f = gray_step8;
  else if(strcmp(argv[1], "bgr-step") == 0)
    f = bgr_step;
  else if(strcmp(argv[1], "bgr-ramp") == 0)
    f = bgr_ramp;
  else if(strcmp(argv[1], "bgr-step2") == 0)
    f = bgr_step2;
  else if(strcmp(argv[1], "bgr-ramp2") == 0)
    f = bgr_ramp2;
  else if(strcmp(argv[1], "rygcbm-ramp") == 0)
    f = rygcbm_ramp;
  else if(strcmp(argv[1], "rygcbm-step") == 0)
    f = rygcbm_step;
  else if(strcmp(argv[1], "spectrum") == 0)
    f = spectrum2;
  else if(strcmp(argv[1], "inv_spec") == 0)
    f = inv_spec;
  else if(strcmp(argv[1], "color1") == 0)
    f = color1_lut;
  else if(strcmp(argv[1], "color2") == 0)
    f = color2_lut;
  else if(strcmp(argv[1], "color3") == 0)
    f = color3_lut;

  else {
     /*  Try to find a user-specified cmap...   */

     char scrtch[200];
     Tcl_Obj *lut;

     sprintf(scrtch,"cmapLUT_%s,powDef",argv[1]);
     lut = Tcl_ObjGetVar2(interp,
			  Tcl_NewStringObj("powImageParam",-1),
			  Tcl_NewStringObj(scrtch,-1),
			  TCL_GLOBAL_ONLY);
     if( lut==NULL ) {
	sprintf(scrtch,"Unable to locate LUT for %s\n",argv[1]);
	Tcl_SetResult(interp,scrtch,TCL_VOLATILE);
	return TCL_ERROR;
     }

     return
        customCmap((Display*)NULL,
                   (Colormap)0L,
                   PowColorTable->ncolors,
                   PowColorTable->lut_start,
                   False,
                   PowColorTable->red,PowColorTable->green,PowColorTable->blue,
                   PowColorTable->intensity_lut,
                   PowColorTable->red_lut,PowColorTable->green_lut,
                   PowColorTable->blue_lut,
                   interp, lut);
  }
  
  (*f)((Display*)NULL,
       (Colormap)0L,
       PowColorTable->ncolors,
       PowColorTable->lut_start,
       False,
       PowColorTable->red,PowColorTable->green,PowColorTable->blue,
       PowColorTable->intensity_lut,
       PowColorTable->red_lut,PowColorTable->green_lut,
       PowColorTable->blue_lut);
  return TCL_OK;
}




int PowPhotoCmapStretch( ClientData clientData, Tcl_Interp *interp,
                         int argc, Tcl_Obj *const argv[] )
{
  int cwid,clen;
  int x_lut[MAX_CLUT_LEN];
  int y_lut[MAX_CLUT_LEN];
  int i,j;
  int lut_size, nElem;
  int *p_lut;
  Tcl_Obj **lutElem;
  
  if( argc != 4 ) {
    Tcl_AppendResult(interp, "wrong # args: should be \"",
                     Tcl_GetStringFromObj(argv[0],NULL),
                     " cwid clen {x1 y1 x2 y2 ... }\"", (char *)NULL);
    return TCL_ERROR;
  }

  p_lut    = PowColorTable->intensity_lut;
  lut_size = PowColorTable->ncolors;

  if( Tcl_GetIntFromObj(interp, argv[1], &cwid) != TCL_OK ||
      Tcl_GetIntFromObj(interp, argv[2], &clen) != TCL_OK ) {
    Tcl_AppendResult(interp, "bad lookup table : should be \"",
                     Tcl_GetStringFromObj(argv[0],NULL),
                     " cwid clen {x1 y1 x2 y2 ... }\"", 
                     (char *) NULL);
    return TCL_ERROR;
  }

  if( Tcl_ListObjGetElements( interp, argv[3], &nElem, &lutElem ) != TCL_OK ) {
    Tcl_SetResult(interp,"Error reading LUT", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if( nElem&0x1 ) {
    Tcl_SetResult(interp,"LUT must have an even number of elements",
                  TCL_VOLATILE);
    return TCL_ERROR;
  }

  i = 0;
  j = 0;
  while( i<nElem ) {
    if( Tcl_GetIntFromObj(interp, lutElem[i++], &x_lut[j]) != TCL_OK ||
        Tcl_GetIntFromObj(interp, lutElem[i++], &y_lut[j]) != TCL_OK ) {
      Tcl_AppendResult(interp, "bad lookup table : should be \"", argv[0],
                       " cwid clen x1 y1 x2 y2 ... \"", 
                       (char *) NULL);
      return TCL_ERROR;
    }
    j++;
  }
    
  for(i=0;i<j;i++) {
    x_lut[i] = (int)(floor((double)x_lut[i]/(double)cwid * lut_size)); 
    y_lut[i] = (int)(floor((double)y_lut[i]/(double)clen * lut_size));
  }
    
  non_linear_lut(p_lut,lut_size,
                 x_lut,y_lut,j,
                 (Display*)NULL,
                 (Colormap)0L,
                 PowColorTable->ncolors,
                 PowColorTable->lut_start,
                 False,
                 PowColorTable->red,PowColorTable->green,PowColorTable->blue,
                 PowColorTable->intensity_lut,
                 PowColorTable->red_lut,PowColorTable->green_lut,
                 PowColorTable->blue_lut); 
  
  return TCL_OK;
}


int PowImageScale( ClientData clientData, Tcl_Interp *interp,
                    int argc, Tcl_Obj *const argv[] )
{
  int x_lut[MAX_CLUT_LEN];
  int y_lut[MAX_CLUT_LEN];
  int i,j;
  int nElem;
  Tcl_Obj **lutElem, *values[2], *minmax;
  char *type;
  double scale, min, max;
  PowImage *powImage;
  void *in_data;
  unsigned int totalPix;
  int data_type;
  extern double lastLookupMin, lastLookupMax;

  if( argc < 2 ) {
    Tcl_AppendResult(interp, "wrong # args: should be \"",
                     Tcl_GetStringFromObj(argv[0],NULL),
                     " lut ?options ..?\"", (char *)NULL);
    return TCL_ERROR;
  }

  /*  Reading a string LUT descriptor  */

  type = Tcl_GetStringFromObj( argv[1], NULL );

  if( !strcmp(type, "linear") ) {

     for( i=0; i<MAX_LOOKUP; i++ )
        byteLookup[i] = i * 256 / MAX_LOOKUP;
     lastLookupMin = lastLookupMax = 0.0;

  } else if( !strcmp(type, "sqrt") ) {

     scale = 256.0 / sqrt( 256.0 );
     for( i=0; i<MAX_LOOKUP; i++ ) {
        byteLookup[i] = (int)(scale * sqrt( (double)i * 256 / MAX_LOOKUP ));
     }
     lastLookupMin = lastLookupMax = 0.0;

  } else if( !strcmp(type, "log") ) {

     scale = 256.0 / log10( 256.0+1.0 );
     for( i=0; i<MAX_LOOKUP; i++ ) {
        byteLookup[i] = (int)(scale * log10( (double)i * 256 / MAX_LOOKUP + 1.0 ));
     }
     lastLookupMin = lastLookupMax = 0.0;

  } else if( !strcmp(type, "histo") ) {
     
     if( argc != 5 ) {
        Tcl_SetResult(interp, "histo LUT requires extra parameters "
                      "'img min max'", TCL_VOLATILE);
        return TCL_ERROR;
     }

     powImage = PowFindImage( Tcl_GetStringFromObj(argv[2], NULL) );
     if( powImage==NULL ) {
        Tcl_AppendResult( interp, "Unable to find image ",
                          Tcl_GetStringFromObj(argv[2], NULL), (char*)NULL );
        return TCL_ERROR;
     }
     if( Tcl_GetDoubleFromObj(interp, argv[3], &min) != TCL_OK ||
         Tcl_GetDoubleFromObj(interp, argv[4], &max) != TCL_OK ) {
        Tcl_AppendResult( interp, "Error reading min/max parameters",
                          (char*)NULL );
        return TCL_ERROR;
     }

     totalPix  = (powImage->width) * (powImage->height);
     in_data   = (powImage->dataptr)->data_array;
     data_type = (powImage->dataptr)->data_type;

     equalize_histo( in_data, data_type, totalPix, &min, &max );

     lastLookupMin = min;
     lastLookupMax = max;

     values[0] = Tcl_NewDoubleObj( min );
     values[1] = Tcl_NewDoubleObj( max );
     minmax    = Tcl_NewListObj( 2, values );
     Tcl_SetObjResult( interp, minmax );

  } else if( !strcmp(type, "model") ) {

     /*  Reading a LUT array  */

     if( Tcl_ListObjGetElements( interp, argv[2], &nElem, &lutElem )
         != TCL_OK ) {
        Tcl_AppendResult(interp, "Error reading LUT", (char*)NULL);
        return TCL_ERROR;
     }

     if( nElem<4 || nElem&0x1 ) {
        Tcl_SetResult(interp,"LUT must have an even number of elements >= 4",
                      TCL_VOLATILE);
        return TCL_ERROR;
     }

     i = 0;
     j = 0;
     while( i<nElem ) {
        if( Tcl_GetIntFromObj(interp, lutElem[i++], &x_lut[j]) != TCL_OK ||
            Tcl_GetIntFromObj(interp, lutElem[i++], &y_lut[j]) != TCL_OK ) {
           Tcl_AppendResult(interp, "bad lookup table : should be \"", argv[0],
                            " cwid clen x1 y1 x2 y2 ... \"", 
                            (char *) NULL);
           return TCL_ERROR;
        }
        j++;
     }
    
     for( i=0; i<j; i++ ) {

        if( x_lut[i]<0 )
           x_lut[i] = 0;
        else if( x_lut[i]>=MAX_LOOKUP )
           x_lut[i] = MAX_LOOKUP-1;

        if( y_lut[i]<0 )
           y_lut[i] = 0;
        else if( y_lut[i]>255 )
           y_lut[i] = 255;
     }
    
     build_lookup( x_lut, y_lut, j );
     lastLookupMin = lastLookupMax = 0.0;

  } else {

     Tcl_SetResult(interp,"Unrecognized LUT type", TCL_VOLATILE);
     return TCL_ERROR;

  }

  return TCL_OK;
}


int PowReditherPhotoBlock (ClientData clientData, Tcl_Interp *interp, int argc, char *argv[] ) {
  PowImage *image_instance;
  Tk_PhotoHandle photo_handle;
  Tk_PhotoImageBlock photo_block;
  double min, max;

  if (argc != 4) {
    Tcl_SetResult(interp, "usage: powReditherPhotoBlock imageName min max", TCL_VOLATILE);
    return TCL_ERROR;
  }

  if ((photo_handle = Tk_FindPhoto(interp,argv[1])) == NULL) {
    Tcl_AppendResult(interp, "image \"", argv[1], "\" doesn't exist",
		     (char *) NULL);
    return TCL_ERROR;
  }

  Tcl_GetDouble(interp, argv[2], &min);
  Tcl_GetDouble(interp, argv[3], &max);

  /*  Tk_PhotoGetImage(photo_handle, &photo_block); */

  image_instance = PowFindImage(argv[1]);

  PowDitherToPhoto(image_instance, &photo_block, min, max);

  photo_block.pixelSize = 3;
  photo_block.width = image_instance->width;
  photo_block.height = image_instance->height;
  photo_block.pitch = image_instance->width * 3;
  photo_block.offset[0] = 0;
  photo_block.offset[1] = 1;
  photo_block.offset[2] = 2;
  
  
  Tk_PhotoPutBlock(interp, photo_handle,&photo_block,0,0,image_instance->width,image_instance->height, TK_PHOTO_COMPOSITE_SET);
  
  ckfree(photo_block.pixelPtr);
  return TCL_OK;
}
