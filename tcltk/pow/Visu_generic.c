#include "tkpict.h"

extern Tcl_Interp *interp;

PictColorTable *PowColorTable;
XColor lut_colorcell_defs[MAX_COLORS];
int byteLookup[MAX_LOOKUP];
double lastLookupMin=0.0, lastLookupMax=0.0;

static int convert_block( void *in, int npts, int in_type,
                          double *dispmin, double *dispmax,
                          unsigned char *out, unsigned int *histo );

/***************************************************/
/*  Wrappers to convert_block which will either    */
/*  convert the image data to bytes (translated    */
/*  using byteLookup[]) or calculate a histogram   */
/*  of the image data (of length MAX_LOOKUP).      */
/***************************************************/

int convert_block_to_byte( void *in, unsigned char *out, int npts,
                           int in_type, double *dispmin, double *dispmax )
{
   if( (lastLookupMin==0.0 && lastLookupMax==0.0)
       || (*dispmin==0.0 && *dispmax==0.0) ) {
      return convert_block( in, npts, in_type, dispmin, dispmax, out, NULL );
   } else {
      return convert_block( in, npts, in_type,
                            &lastLookupMin, &lastLookupMax, out, NULL );
   }
}

int convert_block_to_histo( void *in, int npts, int in_type,
                            double *dispmin, double *dispmax,
                            unsigned int *histo )
{
  return convert_block( in, npts, in_type, dispmin, dispmax, NULL, histo );
}


/***************************************************/
/*    Internal routine called by above wrappers    */
/***************************************************/

static int convert_block( void *in, int npts, int in_type,
                          double *dispmin, double *dispmax,
                          unsigned char *out, unsigned int *histo )
{
  register int i;
  double scale;
  unsigned char *ptr_out; 
  double holder, maxLookup;

  if( histo )
    for( i=0; i<MAX_LOOKUP; i++ ) histo[i] = 0;

  maxLookup = MAX_LOOKUP - 1.0;
  ptr_out = (unsigned char*)out;
  switch (in_type)
    { 
    case BYTE:
      {
        unsigned char *ptr = (unsigned char *)in;
	unsigned char min, max;
	
	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
           ptr = (unsigned char *)in;
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    case WORD:
      {
        short *ptr = (short *)in;
	short min, max;
		
	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
           ptr = (short *)in;
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    case LWORD:
      {
        int *ptr = (int *)in;
	int min, max;

	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
           ptr = (int *)in;
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    case REAL:
      {
        float *ptr = (float *)in;
	float min, max;

	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
           ptr = (float *)in;
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    case DOUBLE:
      {
        double *ptr = (double *)in;
	double min, max;

	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
           ptr = (double*)in;
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    case LONGLONG:
      {
#ifdef __WIN32__
        __int64 *ptr = (__int64*)in;
	__int64 min, max;
#else
        long long *ptr = (long long*)in;
	long long min, max;
#endif

	if( (*dispmin == 0.0) && (*dispmax == 0.0) ) {
           min = max = *ptr++;
           for (i=1;i<npts;++i,++ptr) {
              if(      *ptr > max ) max = *ptr;
              else if( *ptr < min ) min = *ptr;
           }
           *dispmin = min;
           *dispmax = max;
#ifdef __WIN32__
           ptr = (__int64*)in;
#else
           ptr = (long long*)in;
#endif
        }
	if( (*dispmax-*dispmin) == 0.0 ) {
          if( histo )
            histo[ MAX_LOOKUP>>1 - 1 ] = npts;
          else if( out )
            for( i=npts; i--; ) *ptr_out++ = 127;
	} else {
	  scale = maxLookup / (*dispmax - *dispmin);
	  for( i=npts; i--; ) {
	    holder = scale * (*ptr++ - *dispmin);
            if( holder > maxLookup )
              holder = maxLookup;
            else if( holder < 0.0 )
              holder = 0.0;
            if( histo )
              histo[ (int)holder ]++;
            else if( out )
              *ptr_out++ = byteLookup[ (int)holder ];
	  }
	}
      }
      break;

    default:
       (void)fprintf(stderr,"Unknown data type %d\n",in_type);
       return(0);
    }

  return(1);

} /* end convert_block */


void equalize_histo( void *in_data, int data_type, unsigned int totalPix,
                     double *min, double *max )
{
   unsigned int histo[MAX_LOOKUP], excess;
   int color, level, done;
   double bin, pixbin, lvl;
   int bottBin, topBin;

   /*  Calculate histogram  */

   done = 0;
   do {

      convert_block_to_histo(in_data, totalPix, data_type, min, max, histo);

      pixbin = (double)(totalPix - (totalPix>1?1:0)) / 256.0;
      if( pixbin>1.0 ) {
         
         /*  Reduce the excessively abundant levels to a single binwidth  */

         pixbin *= 3.0;
         excess = 0;
         for( level=0; level<MAX_LOOKUP; level++ )
            if( histo[level]>pixbin ) {
               excess += (unsigned int)((histo[level]-pixbin-0.5));
               histo[level] = (unsigned int)(pixbin+0.5);
            }
         pixbin = (double)(totalPix - (totalPix>1?1:0) - excess) / 256.0;
         if( pixbin < 1.0 ) pixbin=1.0;
      }

      /*  Create new color table  */

      bin = 0.0;
      for( level=color=0; level<MAX_LOOKUP-1 && color<255; level++) {
         byteLookup[level] = color;
         bin += histo[level];
         while( bin>=pixbin && color<255 ) {
            bin -= pixbin;
            color++;
         }
      }
      while( level<MAX_LOOKUP ) byteLookup[level++] = 255;

      /* Calculate the binwidth of the 5 and 250 color mark */

      bottBin = topBin = -1;
      for( level=1; level<MAX_LOOKUP; level++ ) {
         if( bottBin < 0 && byteLookup[level] > 5   )
            bottBin = level-1;
         if( topBin  < 0 && byteLookup[level] > 250 )
            topBin = level;
      }
      if( topBin - bottBin < MAX_LOOKUP>>3 ) {
         if( bottBin > 0            ) bottBin--;
         if( topBin  < MAX_LOOKUP-1 ) topBin++;
         lvl = (*max-*min)/(MAX_LOOKUP-1.0);
         if( topBin-bottBin>2  ||  lvl>1e-6*fabs(*min) ) {
           *max = topBin  * lvl + *min;
           *min = bottBin * lvl + *min;
         } else
           done = 1;
      } else {
         done = 1;
      }

   } while( ! done );

}


void build_lookup( int *x_lut, int *y_lut, int nbpts )
{
   int j;
   double slope;
   int curr_pt;
   
   curr_pt =   0;
   slope   = 0.0;
   
   for( j=0; j<x_lut[0]; j++ )
      byteLookup[j] = y_lut[0];
   
   for( j=x_lut[0]; j<x_lut[nbpts-1]; j++ ) {
      
      if( j < x_lut[curr_pt] ) {
         
         byteLookup[j] = (unsigned int)((j-x_lut[curr_pt])*slope + y_lut[curr_pt]);
         if( byteLookup[j] < 0 )
            byteLookup[j] = 0;
         else if( byteLookup[j] > 255 ) 
            byteLookup[j] = 255;
         
      } else {
         
         byteLookup[j] = y_lut[curr_pt];
         if( byteLookup[j] < 0 )
            byteLookup[j] = 0;
         else if( byteLookup[j] > 255 )
            byteLookup[j] = 255;
         
         /* remove vertical ramps */
         while( curr_pt<nbpts-1 && (x_lut[curr_pt+1]-x_lut[curr_pt])==0 )
            curr_pt++;	
         /* compute new slope */
         if( curr_pt< nbpts-1 ) {
            curr_pt++;
            slope = (double)(y_lut[curr_pt]-y_lut[curr_pt-1])/
               (double)(x_lut[curr_pt]-x_lut[curr_pt-1]);
         }
         
      }
      
   }
   
   for(j=x_lut[nbpts-1];j<MAX_LOOKUP;j++) {
      byteLookup[j] = 255;
   }
   
}

/********************/
/*  Other routines  */
/********************/

/* Copy new lut into cmap */
/* Copy ncolors colors from argument arrays to color cells, and
   load into cmap to change the colors on the X display */

void put_lut(Display *disp,Colormap cmap,
	     int ncolors,int lut_start,char overlay,
	     int *red,int *green,int *blue,
	     int *intensity_lut,int *red_lut,int *green_lut,int *blue_lut)
{
   int 	i,j;
   int index;
   int background_index;
   int mod_lstart;
#if !(defined(__WIN32__) || defined(macintosh))
   int pseudoImages;
#endif

#ifdef DEBUG
   printf("put_lut\n");
#endif

   if(overlay == False) {
     for (i=lut_start,j=0; j<ncolors; i++,j++) {
       index = intensity_lut[j];
       lut_colorcell_defs[i].pixel = i;
       lut_colorcell_defs[i].red   =  (unsigned short) (red_lut[red[index]]<<8) ;
       lut_colorcell_defs[i].green =  (unsigned short) (green_lut[green[index]]<<8) ;
       lut_colorcell_defs[i].blue  =  (unsigned short) (blue_lut[blue[index]]<<8) ;
       lut_colorcell_defs[i].flags = DoRed | DoBlue | DoGreen;
     }
   } else {
     mod_lstart = lut_start%2; /* helpful if we decide one day that lut_start should
				  be odd and not even as it is hard-coded now */
     for (i=lut_start,j=0; j<ncolors; i++,j++) {
       index = intensity_lut[j];
      
       if( i%2==mod_lstart) {
	 lut_colorcell_defs[i].red   = (unsigned short) (red_lut[red[index]]<<8) ;
	 lut_colorcell_defs[i].green = (unsigned short) (green_lut[green[index]]<<8) ;
	 lut_colorcell_defs[i].blue  = (unsigned short) (blue_lut[blue[index]]<<8) ;
       } else {
	 lut_colorcell_defs[i].red   = (unsigned short) 0xfffffff;
	 if( index < 50 )
	   background_index = ncolors-1-50;
	 else background_index = ncolors-1-index;
	 lut_colorcell_defs[i].green = (unsigned short) (green_lut[green[background_index]]<<8) ;
	 lut_colorcell_defs[i].blue  = (unsigned short) (blue_lut[blue[background_index]]<<8) ;
       }
       lut_colorcell_defs[i].flags = DoRed | DoBlue | DoGreen;
     }
   }

#if !(defined(__WIN32__) || defined(macintosh))
   Tcl_GetInt(interp,Tcl_GetVar(interp,"powPseudoImages",TCL_GLOBAL_ONLY),
	    &pseudoImages);
   if(pseudoImages) {
     XStoreColors(disp,cmap,&lut_colorcell_defs[lut_start],ncolors) ;
     XFlush(disp) ;
   }
#endif

} /* end put_lut */

int AllocateColorTable(PictColorTable **colorTable,Display *disp, 
		       Colormap cmap,int colormap_level,int ncolors,
		       int lut_start,char atom)
{
  int color_nb;
  int i,r;

  /* Allocate memory */
  (*colorTable) = (PictColorTable*)ckalloc(sizeof(PictColorTable));
  if((*colorTable) == NULL) {
    (void)fprintf(stderr,"ImgPictGet: Could not allocate memory\n");
    return 0;
  }

  /* store values */
  (*colorTable)->display = disp;
  (*colorTable)->colormap = cmap;
  (*colorTable)->colormap_level = colormap_level;
  (*colorTable)->ncolors = ncolors;
  (*colorTable)->lut_start = lut_start;
  (*colorTable)->atom =  atom;
  (*colorTable)->refCount = 1;
	 
  color_nb = ncolors-1;
	  
  for( i=0; i<MAX_LOOKUP; i++ )
    byteLookup[ i ] = i * 256 / MAX_LOOKUP;

  /* load linear intensity lookup table */
  for (i=0; i<ncolors; i++) {
    (*colorTable)->intensity_lut[i] = i;
  }
  /* load linear red green blue lookup tables */
  for (i=0; i<MAX_COLORS; i++) {
    (*colorTable)->red_lut[i] = i;
    (*colorTable)->green_lut[i] = i;
    (*colorTable)->blue_lut[i] = i;
  }
  for (i = 0; i < MAX_COLORS; ++i) {
    r = i*color_nb/(MAX_COLORS-1)+lut_start;
    (*colorTable)->redValues[i] = lut_colorcell_defs[r].pixel;
  }
 
  /* Load the gray color pattern */
  gray(disp,cmap,ncolors,lut_start,False,
       (*colorTable)->red,
       (*colorTable)->green,
       (*colorTable)->blue, 
       (*colorTable)->intensity_lut, 
       (*colorTable)->red_lut,  
       (*colorTable)->green_lut, 
       (*colorTable)->blue_lut);

  return 1;
} /* end AllocateColorTable */
