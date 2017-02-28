/*
 * lut.c --
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

static int random_walk(int *color);


void linear_lut(int *lut, Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut)
{
  int i,j;

  for (i=lut_start,j=0; j<ncolors; i++,j++) {
    lut[j] = j;
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,
	  red,green,blue,intensity_lut,red_lut,green_lut,blue_lut);
}

void non_linear_lut(int *lut, int lut_size, int *x_lut, int *y_lut, int nbpts,
		    Display *disp,Colormap cmap,
		    int ncolors,int lut_start,char overlay,
		    int *red,int *green,int *blue,
		    int *intensity_lut,int *red_lut,int *green_lut,
		    int *blue_lut)
{
  int j;
  double slope;
  int curr_pt;

  curr_pt = 0;
  slope = 0;

  for( j=0; j<x_lut[0]; j++ )
    lut[j] = y_lut[0];

  for( j=x_lut[0]; j<x_lut[nbpts-1]; j++ ) {

    if( j < x_lut[curr_pt] ) {

      lut[j] = (int)((j-x_lut[curr_pt])*slope + y_lut[curr_pt]);
      if( lut[j] < 0 )
	lut[j] = 0;
      else if( lut[j] >= lut_size ) 
	lut[j] = lut_size-1;

    } else {

      lut[j] = y_lut[curr_pt];
      if( lut[j] < 0 )
	lut[j] = 0;
      else if( lut[j] >= lut_size )
	lut[j] = lut_size-1;
     
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

  for(j=x_lut[nbpts-1];j<lut_size;j++) {
    lut[j] = lut_size-1;
  }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
}

void spectrum(Display *disp,Colormap cmap,
	      int ncolors,int lut_start,char overlay,  
	      int *red,int *green,int *blue,
	      int *intensity_lut,int *red_lut,int *green_lut,
	      int *blue_lut)
{
  int 		i;
  double 	pi_over_4, f1, f2, aa, bb, wavelength, s, delta;
  
  /* Compute some necessary values */
  pi_over_4 = atan(1.0);	/* for later */
  aa = (2. * S1 - S2) / (S1 * S2);
  bb =      (S2 - S1) / (S1 * S2);
  delta = 1.0 / (ncolors - 1.0);
  
  /* Go thru each color of the spectrum and load with the proper values */
  for (i=0; i<ncolors; i++) {

    /* Compute the distance along a contour in RGB space */
    wavelength = i * delta;
    s = wavelength / (aa * wavelength + bb);
    
    if (s <= 0.0) {		/* Black floor */
      red[i] = 0;
      green[i] = 0;
      blue[i] = 0;
      
    } 
    else if (s <= 1.0) {	/* Black to red */
      red[i] = (int)(s * MAXLUTVALUE);
      green[i] = 0;
      blue[i] = 0;
    } 
    else if (s <= 2.0) {	/* Red to yellow */
      red[i] = MAXLUTVALUE ;
      green[i] = (int)((s - 1.0) * MAXLUTVALUE);
      blue[i] = 0;
    } 
    else if (s <= 3.0) {	/* Yellow to green */
      red[i] = (int)(MAXLUTVALUE  - ((s - 2.0) * MAXLUTVALUE));
      green[i] = MAXLUTVALUE ;
      blue[i] = 0;
    } 
    else if (s <= 4.0) {	/* Green to cyan */
      f1 = (s - 3.0) * pi_over_4;
      red[i] = 0;
      green[i] = (int)(cos(f1) * MAXLUTVALUE);
      blue[i] = (int)(sin(f1) * MAXLUTVALUE);
    } 
    else if (s <= 5.0) {	/* Cyan to blue */
      f1 = (s - 3.0) * pi_over_4;	/* Yes, s-3, not s-4 here */
      red[i] = 0;
      green[i] = (int)(cos(f1) * MAXLUTVALUE);
      blue[i] = (int)(sin(f1) * MAXLUTVALUE);
    } 
    else if (s <= 6.0) {	/* Blue to magenta */
      f1 = (s - 5.0) * pi_over_4;
      red[i] = (int)(sin(f1) * MAXLUTVALUE);
      green[i] = 0;
      blue[i] = (int)(cos(f1) * MAXLUTVALUE);
    } 
    else if (s <= 7.0) {	/* Magenta to white */
      f1 = s - 6.0;
      f2 = (f1 + (1.0-f1)/sqrt(2.0));
      red[i] = (int)(f2 * MAXLUTVALUE);
      green[i] = (int)(f1 * MAXLUTVALUE);
      blue[i] = (int)(f2 * MAXLUTVALUE);
    } 
    else {			/* Saturate to white */
      red[i] = MAXLUTVALUE ;
      green[i] = MAXLUTVALUE ;
      blue[i] = MAXLUTVALUE ;
    }

  } /* end for each color in spectrum */

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);

} /* end spectrum */


void rgb(Display *disp,Colormap cmap,
	 int ncolors,int lut_start,char overlay, 
	 int *red,int *green,int *blue,
	 int *intensity_lut,int *red_lut,int *green_lut,
	 int *blue_lut)
{
  float step, c;
  int i;

  c = 0;
  step = (float)((ncolors - 1) / 3.0);
  
  for(i=0; i<ncolors; i++){
    if ( c < ncolors )
      blue[i] = (int)c;
    else {
      c = 0;
      blue[i] = (int)c;
    }
    c += step;
  }
  
  step = (float)((ncolors - 1) / 7.0);
  c = 0 ;
  for(i=0; i<ncolors; i++){
    if ( c < ncolors )
      green[i] = (int)c;
    else {
      c = 0;
      green[i] = (int)c;
    }
    c += step;
  }
  c=0;
  for(i=0; i<ncolors; i++){
    if ( c < ncolors )
      red[i] = (int)c;
    else {
      c = 0;
      red[i] = (int)c;
    }
    c += step;
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end rgb */


void gray(Display *disp,Colormap cmap,
	  int ncolors,int lut_start,char overlay, 
	  int *red,int *green,int *blue,
	  int *intensity_lut,int *red_lut,int *green_lut,
	  int *blue_lut)
{
  lut_ramp(red,0,0.0,ncolors-1,1.0) ;
  lut_ramp(green,0,0.0,ncolors-1,1.0) ;
  lut_ramp(blue,0,0.0,ncolors-1,1.0) ;
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
 
} /* end gray */


void hot(Display *disp,Colormap cmap,
	 int ncolors,int lut_start,char overlay,
	 int *red,int *green,int *blue,
	 int *intensity_lut,int *red_lut,int *green_lut,
	 int *blue_lut)
     /* this table is currently very similar to ct */
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;

  lut_ramp(red,   (int)(  0*mult), 0.0, (int)(120*mult), 1.0) ;
  lut_ramp(red,   (int)(120*mult), 1.0, (int)(255*mult), 1.0) ;
  lut_ramp(green, (int)(  0*mult), 0.0, (int)( 85*mult), 0.0) ;
  lut_ramp(green, (int)( 85*mult), 0.0, (int)(255*mult), 1.0) ;
  lut_ramp(blue,  (int)(  0*mult), 0.0, (int)(170*mult), 0.0) ;
  lut_ramp(blue,  (int)(170*mult), 0.0, (int)(255*mult), 1.0) ;
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
}/* end hot */


void cold(Display *disp,Colormap cmap,
	  int ncolors,int lut_start,char overlay,
	  int *red,int *green,int *blue,
	  int *intensity_lut,int *red_lut,int *green_lut,
	  int *blue_lut)
     /* this table is currently ct + invert + flip	*/
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;

  lut_ramp(red,   (int)(  0*mult), 0.0, (int)( 75*mult), 0.0) ;
  lut_ramp(red,   (int)( 75*mult), (float)0.0, (int)(195*mult),(float)0.1) ;
  lut_ramp(red,   (int)(195*mult), (float)0.1, (int)(255*mult),(float)1.0) ;
  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 55*mult),(float)0.0) ;
  lut_ramp(green, (int)( 55*mult), (float)0.0, (int)(245*mult),(float)1.0) ;
  lut_ramp(green, (int)(245*mult), (float)1.0, (int)(255*mult),(float)1.0) ;
  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)(135*mult),(float)1.0) ;
  lut_ramp(blue,  (int)(135*mult), (float)1.0, (int)(255*mult),(float)1.0) ; 
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end cold */

int customCmap(Display *disp,Colormap cmap,
                int ncolors,int lut_start,char overlay,
                int *red,int *green,int *blue,
                int *intensity_lut,int *red_lut,int *green_lut,
                int *blue_lut,
                Tcl_Interp *interp, Tcl_Obj *lutObj)
{
   int nElem, i, j;
   Tcl_Obj **lutElem;
   float mult;

   if( Tcl_ListObjGetElements( interp, lutObj, &nElem, &lutElem )
       != TCL_OK ) {
      Tcl_SetResult(interp,"Error reading LUT", TCL_VOLATILE);
      return TCL_ERROR;
   }

   if( !nElem%3 ) {
      Tcl_SetResult(interp,"LUT must have multiple-of-3 elements",
                    TCL_VOLATILE);
      return TCL_ERROR;
   }

   mult = (float)(nElem/3-1) / (float)(ncolors-1) ;
   for( j=0; j<ncolors; j++ ) {
      i = 3 * (int)(j*mult+0.5);
      if( Tcl_GetIntFromObj(interp, lutElem[i++], &red[j]  ) != TCL_OK ||
          Tcl_GetIntFromObj(interp, lutElem[i++], &green[j]) != TCL_OK ||
          Tcl_GetIntFromObj(interp, lutElem[i++], &blue[j] ) != TCL_OK ) {
         Tcl_SetResult(interp,"Invalid custom lut",TCL_VOLATILE);
         return TCL_ERROR;
      }
   }

   put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
           intensity_lut,red_lut,green_lut,blue_lut);

   return TCL_OK;
}

void hls(Display *disp,Colormap cmap,
	 int ncolors,int lut_start,char overlay,
	 int *red,int *green,int *blue,
	 int *intensity_lut,int *red_lut,int *green_lut,
	 int *blue_lut)
{
  set_hls(red,green,blue) ;
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end hls */



void blkbdy(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)( 60*mult), (float)0.9) ;
  lut_ramp(red,   (int)( 60*mult), (float)0.9, (int)(180*mult), (float)1.0) ;
  lut_ramp(red,   (int)(180*mult), (float)1.0, (int)(255*mult), (float)1.0) ;
  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 10*mult), (float)0.0) ;
  lut_ramp(green, (int)( 10*mult), (float)0.0, (int)(200*mult), (float)1.0) ;
  lut_ramp(green, (int)(200*mult), (float)1.0, (int)(255*mult), (float)1.0) ;
  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)(120*mult), (float)0.0) ; 
  lut_ramp(blue,  (int)(120*mult), (float)0.0, (int)(255*mult), (float)1.0) ;
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end blkbdy */

void invert_cmap(Display *disp,Colormap cmap,
	    int ncolors,int lut_start,char overlay,
	    int *red,int *green,int *blue,
	    int *intensity_lut,int *red_lut,int *green_lut,
	    int *blue_lut)
{
  int nred[MAX_COLORS], ngreen[MAX_COLORS], nblue[MAX_COLORS] ;
  int i, start ;

  start = ncolors - 1 ;
  
  for (i=0; i<ncolors; i++) {
    nred[i] = red[start-i] ;
    ngreen[i] = green[start-i] ;
    nblue[i] = blue[start-i] ;
  }
  for (i=0; i<ncolors; i++) {
    red[i] = nred[i] ;
    green[i] = ngreen[i] ;
    blue[i] = nblue[i] ;
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
 
} /* end invert_cmap */

void bowlerhat(Display *disp,Colormap cmap,
	       int ncolors,int lut_start,char overlay,
	       int *red,int *green,int *blue,
	       int *intensity_lut,int *red_lut,int *green_lut,
	       int *blue_lut)
{
  int halfway ;

  halfway = ncolors/2 - 1 ;
  
  /* Up ramp */

  lut_ramp(red,   0, (float)0.0, halfway, (float)1.0) ;
  lut_ramp(green, 0, (float)0.0, halfway, (float)1.0) ;
  lut_ramp(blue,  0, (float)0.0, halfway, (float)1.0) ;
  
  /* Down ramp */
  
  lut_ramp(red,   halfway+1, (float)1.0, ncolors-1, (float)0.0) ;
  lut_ramp(green, halfway+1, (float)1.0, ncolors-1, (float)0.0) ;
  lut_ramp(blue,  halfway+1, (float)1.0, ncolors-1, (float)0.0) ;
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end bowlerhat */

void tophat(Display *disp,Colormap cmap,
	    int ncolors,int lut_start,char overlay, 
	    int *red,int *green,int *blue,
	    int *intensity_lut,int *red_lut,int *green_lut,
	    int *blue_lut)
{
  int third, twothird ;
  int j ;
  
  third = ncolors/3  ;
  twothird = 2*third ;
  
  for (j = 0; j < third; j++) {
    red[j] = (int)0.0 ;
    green[j] = (int)0.0 ;
    blue[j] = (int)0.0 ;
  }

  for (j = third; j < twothird; j++) {
    red[j] = MAXLUTVALUE ;
    green[j] = MAXLUTVALUE ;
    blue[j] = MAXLUTVALUE ;
  }
  
  for (j = twothird; j < ncolors; j++) {
    red[j] = (int)0.0 ;
    green[j] = (int)0.0 ;
    blue[j] = (int)0.0 ;
  } 
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end tophat */

void randwalk_spectrum(Display *disp,Colormap cmap,
		       int ncolors,int lut_start,char overlay,
		       int *red,int *green,int *blue,
		       int *intensity_lut,int *red_lut,int *green_lut,
		       int *blue_lut)
{
   int j;
   int rval, gval, bval;

   /* Set up default values for random walk function (from Bob Sherwood). */
   /* Start with red; this could be any starting color  */
   
   rval = (MAXLUTVALUE) ;
   gval = 0;
   bval = 0;

   for (j=0; j<ncolors; j++) {
     red[j] = random_walk(&rval);
     green[j] = random_walk(&gval);
     blue[j] = random_walk(&bval);
   }
   put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	   intensity_lut,red_lut,green_lut,blue_lut);
} /* end randwalk_spectrum */


int random_walk(int *color)
{
   /* register long idelta; */
  register int idelta;
  int mask = 0x1f ;

   /*   NOTE: This algorithm adds or subtracts AT MOST the low order
    *   5 bits to the previous value.  To change this, change the
    *   mask.  To make it change more rapidly, use 0x3f
    *   or 0x7f.  To change more slowly, use 0x0f or 0x07.  */

   /* idelta = random();	*/	/* Get an RV */
  idelta = rand();	
  if (idelta & 0x80) {		/* Use eighth bit as a sign */
    *color = abs(*color - (idelta & mask));
  } else {
    *color = abs(*color + (idelta & mask));
  }
  
  if (*color > 255) {	/* Reflect off 255 */
    *color = (2*255) - *color;
  }
   
  return( *color & 0xff );
} /* end random_walk */

void lut_ramp(int *lut,int begin,float beginv,int end,float endv)
{
  int intensity, i ;
  float increment, value ;
  
  if ((begin < 0) || (begin > 255)) 
    return;
  if ((end < 0) || (end > 255))
    return;    

  if ((beginv < 0.0) || (beginv > 1.0)) 
    return;
  if ((endv < 0.0) || (endv > 1.0)) 
    return;

  if (end == begin) {
    intensity = (int)(beginv * 255 + 0.5);
    lut[begin] = intensity ;
    return;
  }
  
  increment = (endv - beginv) / (end - begin) ;
  value = beginv ;
  for (i=begin; i<= end; i++) {
    intensity = (int)(value * 255 + 0.5);
    lut[i] = intensity ;
    value += increment ;
  }
} /* end lut_ramp */

void set_hls(int *red,int *green,int *blue)
{
  float H,L,S ;
  int r,g,b ;
  int n ;
  

  /* set background blue ... n=0 */
  
  H = 0 ;  L = 0.5 ;  S = 0.5 ;
  convert_HLS_rgb(H,L,S,&r,&g,&b) ;
  red[0] = r ;  green[0] = g ;  blue[0] = b ;
  

  /* set red, orange, yellow, and green ranges */

  for (n=1; n<=255; n++) {
    if (n < 64) {
      H = 105.0 ;
      L = (float)(0.3 + 0.00968 * (n - 1));
      S = (float)(0.4 + 0.00645 * (n - 1));
    }
    else if (n < 128) {
      H = 155.0 ;
      L = (float)(0.3 + 0.00952 * (n - 64));
      S = (float)(0.4 + 0.00635 * (n - 64));
    }
    else if (n < 192) {
      H = 190.0 ;
      L = (float)(0.3 + 0.00968 * (n - 128));
      S = (float)(0.4 + 0.00645 * (n - 128)) ;
    }
    else {
      H = 240.0 ;
      L = (float)(0.3 + 0.00968 * (n - 192));
      S = (float)(0.4 + 0.00645 * (n - 192));
    }
    convert_HLS_rgb(H,L,S,&r,&g,&b) ;
    red[n] = r ;  green[n] = g ;  blue[n] = b ;
  }
} /* end set_hls */


void convert_HLS_rgb(float H,float L,float S,int *r,int *g,int *b)
{
  float R,G,B ;
  float M,m ;
  

  /* Setup equations */
  
  if (L <= 0.5)
    M = L * (1 + S) ;
  else M = L + S - L*S ;
  m = 2*L - M ;
  
  /* Calculate R */
  
  if (H < 60)
    R = (float)(m + (M - m) * (H/60.0));
  else if (H < 180)
    R = M ;
  else if (H < 240)
    R = m + (M - m) * ((240 - H)/60) ;
  else R = m ;
  
  
  /* calculate G */
  
  if (H < 120)
        G = m ;
  else if (H < 180)
    G = m + (M - m) * ((H - 120)/60) ;
  else if (H < 300)
    G = M ;
  else 
    G = m + (M - m) * ((360 - H)/60) ;

  /* calculate B */
  
  if (H < 60)
    B = M ;
  else if (H < 120)
    B = m + (M - m) * ((120 - H)/60) ;
  else if (H < 240)
    B = m ;
  else if (H < 300)
    B = m + (M - m) * (( H - 240)/60) ;
  else 
    B = M ;


  /* scale R,G,B to 0-255 */
  
  *r = (int)(255 * R);
  *g = (int)(255 * G);
  *b = (int)(255 * B);

} /* end convert_HLS_rgb */

void hatgray(Display *disp,Colormap cmap,
	     int ncolors,int lut_start,char overlay, 
	     int *red,int *green,int *blue,
	     int *intensity_lut,int *red_lut,int *green_lut,
	     int *blue_lut)
{
  int tred[256],tgreen[256],tblue[256];
  int i,j;

  /* compute the gray color map */
  
  lut_ramp(red,0,(float)0.0,ncolors-1,(float)1.0) ;
  lut_ramp(green,0,(float)0.0,ncolors-1,(float)1.0) ;
  lut_ramp(blue,0,(float)0.0,ncolors-1,(float)1.0) ;

  /* Up ramp */
  i = 0;
  for (j=1; j<ncolors-1; j+=2) {
    tred[i] = red[j];
    tgreen[i] = green[j];
    tblue[i] = blue[j];
    i++;
  }
  
  /* Down ramp */
  for (j=ncolors-1; j>0; j-=2) {
    tred[i] = red[j];
    tgreen[i] = green[j];
    tblue[i] = blue[j];
    i++;
  }
  for(i=0;i<ncolors;i++) {
    red[i] = tred[i];
    green[i] = tgreen[i];
    blue[i] = tblue[i];
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end hatgray */
 
void hatct(Display *disp,Colormap cmap,
	   int ncolors,int lut_start,char overlay,
	   int *red,int *green,int *blue,
	   int *intensity_lut,int *red_lut,int *green_lut,
	   int *blue_lut)
{
  int tred[256],tgreen[256],tblue[256];
  int i,j;
  float mult = (float)((ncolors-1) / 255.0);

  /* compute the ct color map */
  lut_ramp(red, 0, (float)0.0, (int)(60*mult), (float)0.9) ;
  lut_ramp(red, (int)(60*mult), (float)0.9, (int)(180*mult), (float)1.0) ;
  lut_ramp(red, (int)(180*mult), (float)1.0, (int)(255*mult), (float)1.0) ;
  lut_ramp(green, 0, (float)0.0, (int)(10*mult), (float)0.0) ;
  lut_ramp(green, (int)(10*mult), (float)0.0, (int)(200*mult), (float)1.0) ;
  lut_ramp(green, (int)(200*mult), (float)1.0, (int)(255*mult), (float)1.0) ;
  lut_ramp(blue,  0, (float)0.0, (int)(120*mult), (float)0.0) ; 
  lut_ramp(blue, (int)(120*mult), (float)0.0, (int)(255*mult), (float)1.0) ;
  
  /* Up ramp */
  i = 0;
  for (j=1; j<ncolors-1; j+=2) {
    tred[i] = red[j];
    tgreen[i] = green[j];
    tblue[i] = blue[j];
    i++;
  }

  /* Down ramp */
  for (j=ncolors-1; j>0; j-=2) {
    tred[i] = red[j];
    tgreen[i] = green[j];
    tblue[i] = blue[j];
    i++;
  } 
  for(i=0;i<ncolors;i++) {
    red[i] = tred[i];
    green[i] = tgreen[i];
    blue[i] = tblue[i];
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end hat_ct */
 



void lut_thres(Display *disp,Colormap cmap,
	       int ncolors,int lut_start,char overlay,int loval,int hival,
	       int *red,int *green,int *blue,
	       int *intensity_lut,int *red_lut,int *green_lut,
	       int *blue_lut)
{
  int i;
 
  if( loval>= hival )
    return;
  if(loval < 0) 
    loval = 0;
  if(hival>(MAX_COLORS-1))
    hival = (MAX_COLORS-1);

  for (i=0; i<loval; i++) {
   red[i]=blue[i]=green[i]=0;
  }
  for (i=loval; i<=hival; i++) {
    red[i]=blue[i]=green[i]=MAX_COLORS-1;
  }
  for (i=hival+1; i<ncolors; i++) {
    red[i]=blue[i]=green[i]=0;
  }
  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut);
} /* end lut_thres */



/*********************************/
/*   New Colormaps for POW       */
/*********************************/

void bgr_ramp(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)(170*mult), (float)0.0);
  lut_ramp(red,   (int)(170*mult), (float)0.0, (int)(255*mult), (float)1.0);

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 85*mult), (float)0.0) ;
  lut_ramp(green, (int)( 85*mult), (float)0.0, (int)(170*mult), (float)1.0) ;
  lut_ramp(green, (int)(170*mult), (float)0.0, (int)(255*mult), (float)0.0) ;

  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)( 85*mult), (float)1.0) ;
  lut_ramp(blue,  (int)( 85*mult), (float)0.0, (int)(255*mult), (float)0.0) ;

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end bgr_ramp */


void bgr_ramp2(Display *disp,Colormap cmap,
	       int ncolors,int lut_start,char overlay,
	       int *red,int *green,int *blue,
	       int *intensity_lut,int *red_lut,int *green_lut,
	       int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)( 85*mult), (float)0.0);
  lut_ramp(red,   (int)( 86*mult), (float)0.0, (int)(127*mult), (float)1.0);
  lut_ramp(red,   (int)(128*mult), (float)0.0, (int)(213*mult), (float)0.0);
  lut_ramp(red,   (int)(214*mult), (float)0.0, (int)(255*mult), (float)1.0);

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0);
  lut_ramp(green, (int)( 43*mult), (float)0.0, (int)( 85*mult), (float)1.0);
  lut_ramp(green, (int)( 86*mult), (float)0.0, (int)(170*mult), (float)0.0);
  lut_ramp(green, (int)(171*mult), (float)0.0, (int)(213*mult), (float)1.0);
  lut_ramp(green, (int)(214*mult), (float)0.0, (int)(255*mult), (float)0.0);

  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)1.0);
  lut_ramp(blue,  (int)( 43*mult), (float)0.0, (int)(127*mult), (float)0.0);
  lut_ramp(blue,  (int)(128*mult), (float)0.0, (int)(170*mult), (float)1.0);
  lut_ramp(blue,  (int)(171*mult), (float)0.0, (int)(255*mult), (float)0.0);

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end bgr_ramp2 */


void bgr_step(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)(170*mult), (float)0.0);
  lut_ramp(red,   (int)(171*mult), (float)1.0, (int)(255*mult), (float)1.0);

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 85*mult), (float)0.0);
  lut_ramp(green, (int)( 86*mult), (float)1.0, (int)(170*mult), (float)1.0);
  lut_ramp(green, (int)(171*mult), (float)0.0, (int)(255*mult), (float)0.0);

  lut_ramp(blue,  (int)(  0*mult), (float)1.0, (int)( 85*mult), (float)1.0);
  lut_ramp(blue,  (int)( 86*mult), (float)0.0, (int)(255*mult), (float)0.0);

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end bgr_step */


void bgr_step2(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)( 85*mult), (float)0.0);
  lut_ramp(red,   (int)( 86*mult), (float)1.0, (int)(127*mult), (float)1.0);
  lut_ramp(red,   (int)(128*mult), (float)0.0, (int)(213*mult), (float)0.0);
  lut_ramp(red,   (int)(214*mult), (float)1.0, (int)(255*mult), (float)1.0);

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0);
  lut_ramp(green, (int)( 43*mult), (float)1.0, (int)( 85*mult), (float)1.0);
  lut_ramp(green, (int)( 86*mult), (float)0.0, (int)(170*mult), (float)0.0);
  lut_ramp(green, (int)(171*mult), (float)1.0, (int)(213*mult), (float)1.0);
  lut_ramp(green, (int)(214*mult), (float)0.0, (int)(255*mult), (float)0.0);

  lut_ramp(blue,  (int)(  0*mult), (float)1.0, (int)( 42*mult), (float)1.0);
  lut_ramp(blue,  (int)( 43*mult), (float)0.0, (int)(127*mult), (float)0.0);
  lut_ramp(blue,  (int)(128*mult), (float)1.0, (int)(170*mult), (float)1.0);
  lut_ramp(blue,  (int)(171*mult), (float)0.0, (int)(255*mult), (float)0.0);

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end bgr_step2 */


void rygcbm_ramp(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)1.0) ;
  lut_ramp(red,   (int)( 43*mult), (float)0.0, (int)( 85*mult), (float)1.0) ;
  lut_ramp(red,   (int)( 86*mult), (float)0.0, (int)(127*mult), (float)0.0) ;
  lut_ramp(red,   (int)(128*mult), (float)0.0, (int)(170*mult), (float)0.0) ;
  lut_ramp(red,   (int)(171*mult), (float)0.0, (int)(213*mult), (float)0.0) ;
  lut_ramp(red,   (int)(214*mult), (float)0.0, (int)(255*mult), (float)1.0) ;

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0) ;
  lut_ramp(green, (int)( 43*mult), (float)0.0, (int)( 85*mult), (float)1.0) ;
  lut_ramp(green, (int)( 86*mult), (float)0.0, (int)(127*mult), (float)1.0) ;
  lut_ramp(green, (int)(128*mult), (float)0.0, (int)(170*mult), (float)1.0) ;
  lut_ramp(green, (int)(171*mult), (float)0.0, (int)(213*mult), (float)0.0) ;
  lut_ramp(green, (int)(214*mult), (float)0.0, (int)(255*mult), (float)0.0) ;

  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0) ;
  lut_ramp(blue,  (int)( 43*mult), (float)0.0, (int)( 85*mult), (float)0.0) ;
  lut_ramp(blue,  (int)( 86*mult), (float)0.0, (int)(127*mult), (float)0.0) ;
  lut_ramp(blue,  (int)(128*mult), (float)0.0, (int)(170*mult), (float)1.0) ;
  lut_ramp(blue,  (int)(171*mult), (float)0.0, (int)(213*mult), (float)1.0) ;
  lut_ramp(blue,  (int)(214*mult), (float)0.0, (int)(255*mult), (float)1.0) ;

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end rygcbm_ramp */


void rygcbm_step(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  
  lut_ramp(red,   (int)(  0*mult), (float)1.0, (int)( 42*mult), (float)1.0) ;
  lut_ramp(red,   (int)( 43*mult), (float)1.0, (int)( 85*mult), (float)1.0) ;
  lut_ramp(red,   (int)( 86*mult), (float)0.0, (int)(127*mult), (float)0.0) ;
  lut_ramp(red,   (int)(128*mult), (float)0.0, (int)(170*mult), (float)0.0) ;
  lut_ramp(red,   (int)(171*mult), (float)0.0, (int)(213*mult), (float)0.0) ;
  lut_ramp(red,   (int)(214*mult), (float)1.0, (int)(255*mult), (float)1.0) ;

  lut_ramp(green, (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0) ;
  lut_ramp(green, (int)( 43*mult), (float)1.0, (int)( 85*mult), (float)1.0) ;
  lut_ramp(green, (int)( 86*mult), (float)1.0, (int)(127*mult), (float)1.0) ;
  lut_ramp(green, (int)(128*mult), (float)1.0, (int)(170*mult), (float)1.0) ;
  lut_ramp(green, (int)(171*mult), (float)0.0, (int)(213*mult), (float)0.0) ;
  lut_ramp(green, (int)(214*mult), (float)0.0, (int)(255*mult), (float)0.0) ;

  lut_ramp(blue,  (int)(  0*mult), (float)0.0, (int)( 42*mult), (float)0.0) ;
  lut_ramp(blue,  (int)( 43*mult), (float)0.0, (int)( 85*mult), (float)0.0) ;
  lut_ramp(blue,  (int)( 86*mult), (float)0.0, (int)(127*mult), (float)0.0) ;
  lut_ramp(blue,  (int)(128*mult), (float)1.0, (int)(170*mult), (float)1.0) ;
  lut_ramp(blue,  (int)(171*mult), (float)1.0, (int)(213*mult), (float)1.0) ;
  lut_ramp(blue,  (int)(214*mult), (float)1.0, (int)(255*mult), (float)1.0) ;

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end rygcbm_step */


void gray_ramp2(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  int i;
  
  lut_ramp(red, (int)(  0*mult), (float)0.0, (int)(127*mult), (float)1.0) ;
  lut_ramp(red, (int)(128*mult), (float)0.0, (int)(255*mult), (float)1.0) ;

  for(i=0;i<ncolors;i++)
     green[i] = blue[i] = red[i];

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end gray_ramp2 */


void gray_ramp4(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  int i;
  
  lut_ramp(red,   (int)(  0*mult), (float)0.0, (int)( 63*mult), (float)1.0) ;
  lut_ramp(red,   (int)( 64*mult), (float)0.0, (int)(127*mult), (float)1.0) ;
  lut_ramp(red,   (int)(128*mult), (float)0.0, (int)(191*mult), (float)1.0) ;
  lut_ramp(red,   (int)(192*mult), (float)0.0, (int)(255*mult), (float)1.0) ;

  for(i=0;i<ncolors;i++)
     green[i] = blue[i] = red[i];

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end gray_ramp4 */


void gray_step4(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  int i;
  
  lut_ramp(red, (int)(  0*mult), (float)0.00, (int)( 63*mult), (float)0.00) ;
  lut_ramp(red, (int)( 64*mult), (float)0.33, (int)(127*mult), (float)0.33) ;
  lut_ramp(red, (int)(128*mult), (float)0.67, (int)(191*mult), (float)0.67) ;
  lut_ramp(red, (int)(192*mult), (float)1.00, (int)(255*mult), (float)1.00) ;

  for(i=0;i<ncolors;i++)
     green[i] = blue[i] = red[i];

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end gray_step4 */


void gray_step8(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
  float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
  int i;
  
  lut_ramp(red, (int)(  0*mult), (float)0.000, (int)( 31*mult), (float)0.000) ;
  lut_ramp(red, (int)( 32*mult), (float)0.143, (int)( 63*mult), (float)0.143) ;
  lut_ramp(red, (int)( 64*mult), (float)0.286, (int)( 95*mult), (float)0.286) ;
  lut_ramp(red, (int)( 96*mult), (float)0.429, (int)(127*mult), (float)0.429) ;
  lut_ramp(red, (int)(128*mult), (float)0.571, (int)(159*mult), (float)0.571) ;
  lut_ramp(red, (int)(160*mult), (float)0.714, (int)(191*mult), (float)0.714) ;
  lut_ramp(red, (int)(192*mult), (float)0.857, (int)(223*mult), (float)0.857) ;
  lut_ramp(red, (int)(224*mult), (float)1.000, (int)(255*mult), (float)1.000) ;

  for(i=0;i<ncolors;i++)
     green[i] = blue[i] = red[i];

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end gray_step8 */


void spectrum2(Display *disp,Colormap cmap,
	int ncolors,int lut_start,char overlay,
	int *red,int *green,int *blue,
	int *intensity_lut,int *red_lut,int *green_lut,
	int *blue_lut)
{
   int N=12;
   static double r[]={0,1,1,1,1,.75,0,0,0,0,.5,1};

   static double g[]={0,0,0.5,0.75,1,1,1,1,0.7,0,0.5,1};
   
   static double b[]={0,0,0,0,0,0,0,1,1,1,1,1};
   
   float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
   int i,start,end;
  
   for(i=1;i<N;i++) {
      start = (int)((255*(i-1)/(N-1))*mult);
      end   = (int)((255*( i )/(N-1))*mult);
      lut_ramp(red,   start, (float)r[i-1], end, (float)r[i]);
      lut_ramp(green, start, (float)g[i-1], end, (float)g[i]);
      lut_ramp(blue,  start, (float)b[i-1], end, (float)b[i]);
   }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end spectrum2 */


void inv_spec(Display *disp,Colormap cmap,
	      int ncolors,int lut_start,char overlay,
	      int *red,int *green,int *blue,
	      int *intensity_lut,int *red_lut,int *green_lut,
	      int *blue_lut)
{
   int N=11;
   static struct {
      double r,g,b;
   } clrs[] = {
      { 0., 0., 0. },
      { 0., 0., 1. },
      { 0., .7, 1. },
      { 0., 1., 1. },
      { 0., 1., 0. },
      {.75, 1., 0. },
      { 1., 1., 0. },
      { 1., .75, .0 },
      { 1., .5, 0. },
      { 1., 0., 0. },
      { 1., 1., 1. }
   };

   float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
   int i,start,end;
  
   for(i=1;i<N;i++) {
      start = (int)((255*(i-1)/(N-1))*mult);
      end   = (int)((255*( i )/(N-1))*mult);
      lut_ramp(red,   start, (float)clrs[i-1].r, end, (float)clrs[i].r);
      lut_ramp(green, start, (float)clrs[i-1].g, end, (float)clrs[i].g);
      lut_ramp(blue,  start, (float)clrs[i-1].b, end, (float)clrs[i].b);
   }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end inv_spec */


void color1_lut(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut)
{
   int N=5;
   static double r[]={ 0.0, 0.9, 1.0, 0.0, 1.0 };
   static double g[]={ 0.0, 0.0, 1.0, 0.8, 1.0 };
   static double b[]={ 0.0, 0.8, 0.0, 1.0, 1.0 };
   
   float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
   int i,start,end;
  
   for(i=1;i<N;i++) {
      start = (int)((255*(i-1)/(N-1))*mult);
      end   = (int)((255*( i )/(N-1))*mult);
      lut_ramp(red,   start, (float)r[i-1], end, (float)r[i]);
      lut_ramp(green, start, (float)g[i-1], end, (float)g[i]);
      lut_ramp(blue,  start, (float)b[i-1], end, (float)b[i]);
   }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end color1 */


void color2_lut(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut)
{
   int N=5;
   static double r[]={ 0.0, 0.9, 0.0, 1.0, 1.0 };
   static double g[]={ 0.0, 0.0, 1.0, 1.0, 1.0 };
   static double b[]={ 0.0, 1.0, 1.0, 0.0, 1.0 };
   
   float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
   int i,start,end;
  
   for(i=1;i<N;i++) {
      start = (int)((255*(i-1)/(N-1))*mult);
      end   = (int)((255*( i )/(N-1))*mult);
      lut_ramp(red,   start, (float)r[i-1], end, (float)r[i]);
      lut_ramp(green, start, (float)g[i-1], end, (float)g[i]);
      lut_ramp(blue,  start, (float)b[i-1], end, (float)b[i]);
   }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end color2 */


void color3_lut(Display *disp,Colormap cmap,
		int ncolors,int lut_start,char overlay,
		int *red,int *green,int *blue,
		int *intensity_lut,int *red_lut,int *green_lut,
		int *blue_lut)
{
   int N=5;
   static double r[]={ 0.0, 0.0, 0.0, 1.0, 1.0 };
   static double g[]={ 0.0, 0.0, 1.0, 1.0, 1.0 };
   static double b[]={ 0.0, 1.0, 1.0, 0.0, 1.0 };
   
   float mult = (float)(ncolors-1) / (float)(MAXLUTVALUE) ;
   int i,start,end;
  
   for(i=1;i<N;i++) {
      start = (int)((255*(i-1)/(N-1))*mult);
      end   = (int)((255*( i )/(N-1))*mult);
      lut_ramp(red,   start, (float)r[i-1], end, (float)r[i]);
      lut_ramp(green, start, (float)g[i-1], end, (float)g[i]);
      lut_ramp(blue,  start, (float)b[i-1], end, (float)b[i]);
   }

  put_lut(disp,cmap,ncolors,lut_start,overlay,red,green,blue,
	  intensity_lut,red_lut,green_lut,blue_lut); 
} /* end color3 */
