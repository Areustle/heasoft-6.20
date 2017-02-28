#include <stdio.h>
#include <string.h>
#include <math.h>
#include "pil.h"
#include "headas.h"        
                                                                                
int printsub(const char *);
                                                                                
#define TOOLSUB varmosaic
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

#define VARMOSAIC_VERSION "varmosaic  ver 2.0 (2005-03-07)"
#define  MAXFILE 10000      /* Maximum number of input files */
#define  MAXNAMELENGTH 256  /* Maximum character length of file names */
#define  BufLen_2      MAXNAMELENGTH /* Required for pfile.h */
#define  MAX_E_RANGES 20    /* Maximum number of energy ranges for the input images */

/* Carry out quick mosaic of integral images taking account of variance 
   2004-11-10 version 1.0 Ken Ebisawa 
   2004-11-11 version 1.1 Ken Ebisawa 
   In ISDC Barn for INTEGRAL Quick Look Analysis, somehow fits_open_image does not work.
   If there is a blank line in the input file, it was considered
   to be a file with null name.

   2005-03-07 version 2.0. First release an ftool from GSFC
*/


/* Function prototypes */
int varmosaic(void);
int write_wcs(fitsfile *outfptr,double xrval,double yrval,double xrpix,double yrpix,double xinc,double yinc,double rot, int *status);

int varmosaic(void){
  int status = 0, i, j, nfile, hdutype,jj, l, m;
  fitsfile *infptr,*outfptr;
  FILE *fp;
  char filelist[MAXNAMELENGTH];
  char inimages[MAXFILE][MAXNAMELENGTH], tmp[MAXNAMELENGTH], coordtype[10];
  double xrefval[MAXFILE], yrefval[MAXFILE], xrefpix, yrefpix, xinc, yinc, rot;
  double pointx[MAXFILE],pointy[MAXFILE],pointz[MAXFILE];
  double avex=0.0,avey=0.0,avez=0.0,deg2rad=1.745329252E-2;
  char instrume[20], comment[MAXNAMELENGTH],outfile[MAXNAMELENGTH];
  char text[9999]; /* Output text messages */
  
  double aveRA, aveDEC, xrefpixout, yrefpixout;   /* WCS information of the output file. Note, xinc and yinc
						     are the same as those of input files  */
  int imagebin;    /* Output image bins (X and Y same)  */
  int num_E_range=0; /* Number of energy ranges in the input images/output image */
  
  int clobber; /* clobber the output image or not */
  int exist;  /*output file alreayd exists or not*/
  int pixdivide; /* input image pixel is divided into pixdivide**2 subpixels */

  long inimsize; /* Input imagesize. Readfrom NAXIS1 */

  double dist=0.0, maxdist=-999.0,imagesize=0;

  struct ScWimage{ /* Defined for each energy range               */
    int    intHDU; /* HDU of the intensity map                    */
    int    varHDU; /* HDU of the variance map                     */
    double E_min;  /* E_min of the energy range                   */
    double E_max;  /* E_max of the energy range                   */
  } ScW[MAX_E_RANGES];

  static char taskname[80] = "varmosaic";
  static char version[8] = "2.0";
  /* Register taskname and version. */
  set_toolname(taskname);
  set_toolversion(version);

  /* Read parameters from the paramete file */
   status=PILGetString("filelist", filelist);

  /* Read ascii image list*/
  if((fp = fopen(filelist,"r"))==NULL){
    sprintf(text,"##### Input file \"%s\" NOT opened.\n",filelist);
    HD_printf(text);
    return status;
  }else{
    sprintf(text,"##### Input file \"%s\" opened\n",filelist);
    HD_printf(text);
    i = 0;
    while(fgets(tmp, MAXNAMELENGTH,fp)!=NULL&&sscanf(tmp,"%s",inimages[i])>0){
      i++;
    }
    fclose(fp);
  }
  nfile = i;
  sprintf(text,"##### Total number of input files: %d\n", nfile);  
  HD_printf(text);

  /* Read other parameters */
  status=PILGetString("outimage", outfile);
  status=PILGetInt("pixdivide", &pixdivide);
  sprintf(text,"##### Input pixel is divided into %d x %d sub-pixels\n", pixdivide, pixdivide);  
  HD_printf(text);

  status=PILGetBool("clobber",&clobber);

  {FILE *openfile; /* Check if the output file exists or not */
  openfile = fopen(outfile,"r");
  if(openfile != NULL) { 
    fclose(openfile);
    exist = 1;
  }else    {
    exist = 0;
  }}

  if(exist){
    if(clobber){
      sprintf(text,"##### Output file exists and will be overwritten since clobber=yes");
      headas_clobberfile(outfile);
    }
    else{
      sprintf(text,"##### Output file exists and will NOT be overwritten since clobber=no");
      fprintf(stderr,text);
      return status;
    }
  }

  status=PILGetInt("outimagesize",&imagebin);
  if(imagebin > 0){
    /* Output image size and reference points read from the parameter file */
      sprintf(text,"##### Output image size is given in the parameter file as %d\n", imagebin);
      HD_printf(text);
      status=PILGetReal("ra_ref",&aveRA);
      HD_printf(text);
      status=PILGetReal("dec_ref",&aveDEC);
      sprintf(text,"##### Reference point RA, DEC = %10.3f %10.3f\n", aveRA, aveDEC);
      HD_printf(text);
  }

  
  HD_printf("");

  /* Read the input image FITS file one by one, determine the average of the pointings, which 
     will be the center of the mosaic image.  Also, we deterime the mosaic image extent,
     to determine the size of the mosaic image.  Note, we use the fixed  imagebin size, 
     same as the ScW level images. Fom the first input file, we get the number of energy bands. 
     Assumption is that all the input files have the same number of energy bands */
  
  for(i=0;i<nfile;i++){ /* loop DDD for each input file*/
    /*workaround in Barn. Somehow fits_open_image detect the empty primary
      fits_open_image(&infptr, inimages[i], 0, &status);
      {int hdunum;
      fits_get_hdu_num(infptr, &hdunum);
      }
    */
    fits_open_data(&infptr, inimages[i], 0, &status);
    fits_movabs_hdu(infptr, 3, &hdutype, &status);

    fits_read_key_str(infptr, "INSTRUME", instrume,comment,&status);
    fits_read_img_coord(infptr, &xrefval[i], &yrefval[i], &xrefpix, &yrefpix, &xinc,  &yinc,
			&rot, coordtype, &status);
    sprintf(text,"Input File[%d]: %s\n", i+1,inimages[i]);
    HD_printf(text);
    sprintf(text,"Pointing direction: (%7.3f,%7.3f)\n", xrefval[i], yrefval[i]);
    HD_printf(text);
    pointx[i]=cos(xrefval[i]*deg2rad)*cos(yrefval[i]*deg2rad);
    pointy[i]=sin(xrefval[i]*deg2rad)*cos(yrefval[i]*deg2rad);
    pointz[i]=sin(yrefval[i]*deg2rad);
    avex= avex + pointx[i];
    avey= avey + pointy[i];
    avez= avez + pointz[i];
    
    if(i==0){/* block CCC */
      /* Look into the first image file structure to know the energy bands*/
      {int hdunum,ii=0;
      char imatype[20];
      double E_min, E_max;
      fits_get_num_hdus(infptr, &hdunum, &status);
      sprintf(text,"Looking into the structure of the first image. Number of HDU = %d\n", hdunum);
      HD_printf(text);
      for(j=1;j<=hdunum;j++){
	fits_movabs_hdu(infptr, j, &hdutype, &status);
	fits_read_key_lng(infptr, "NAXIS1",   &inimsize, comment, &status);
	fits_read_key_str(infptr, "IMATYPE", imatype,comment, &status);
	fits_read_key_dbl(infptr, "E_MIN",   &E_min, comment, &status);
	fits_read_key_dbl(infptr, "E_MAX",   &E_max, comment, &status);
	if(strstr(imatype,"INTENSITY") != NULL){
	  ScW[ii].intHDU = j;
	  ScW[ii].E_min = E_min;
	  ScW[ii].E_max = E_max;
	}else if(strstr(imatype,"VARIANCE") != NULL) {
	  ScW[ii].varHDU = j;
	  ii++;
	}
	if(status !=0){
	  status = 0;
	}else{
	  sprintf(text,"HDU = %2d IMATYPE = %12s E_min = %7.2f keV E_max = %7.2f keV\n", j, imatype, E_min, E_max);
	  HD_printf(text);
}
      }
      num_E_range = ii;
      sprintf(text,"Number of energy ranges = %d\n", num_E_range);
      HD_printf(text);
      for(ii=0;ii<num_E_range;ii++){
	sprintf(text,"Energy range:%d Intensity HDU:%d Variance HDU:%d E_min %4.2f keV E_max %4.2f keV\n", ii+1, ScW[ii].intHDU,ScW[ii].varHDU,ScW[ii].E_min,ScW[ii].E_max);
	HD_printf(text);
      }
      HD_printf("");
      }
    }/*End CCC for the first input file */
      fits_close_file(infptr, &status);
  } /* End DDD for each input file */
  

  if(imagebin <=0){ /* block LLL when imagebin in the parameter file <=0,
		       Determine the average poiting position and output image size from the input files*/
    { 
      double length=0.0;
      length = sqrt(avex*avex+avey*avey+avez*avez);
      avex=avex/length;
      avey=avey/length;
      avez=avez/length;
    }
    
    aveRA = atan2(avey,avex)/deg2rad;
    if(aveRA<0){aveRA = aveRA + 360.0;}
    aveDEC= asin(avez)/deg2rad;
    
    sprintf(text,"Average Pointing direction is calculated as: (%7.3f,%7.3f)\n", aveRA, aveDEC);
    HD_printf(text);
    
    /* Determine the maximum distance between input image centers and the pointing average. 
       Create the output image mosaic file having the same number of energy bands as
       input imagess,  write necessary keywords to the output file, in particular WCS keywords.*/
    
    for(i=0;i<nfile;i++){
      dist = sqrt((avex-pointx[i])*(avex-pointx[i])+(avey-pointy[i])*(avey-pointy[i])+(avez-pointz[i])*(avez-pointz[i]));
      if(dist > maxdist){maxdist = dist;}
    }
    maxdist = 2.0*asin(maxdist/2.0)/deg2rad;
    sprintf(text,"\nMaximum distance of the mosaic center and each ScW pointing :%8.3f degree\n", maxdist);
    HD_printf(text);
    /* ISGRI FOV = 30.6 deg x 30.6 deg which is covered with a circle with 21.6 deg radius.
       JEMX  FOV = 6.6 deg radius */
    if(strchr(instrume,'J')!=NULL){imagesize = 2.0*(maxdist +  6.6);}
    if(strchr(instrume,'I')!=NULL){imagesize = 2.0*(maxdist + 21.6);}
    imagebin =  floor(imagesize/yinc)+1;
    sprintf(text,"We define the output image size as %9.3f deg.\n", imagesize);
    HD_printf(text);
    sprintf(text,"Imagebinsize = %9.4f deg (same as input), thus output image size = %d pixels\n", yinc,imagebin);
    HD_printf(text);
  } /* end of block LLL */
  
  {/* start block FFF */
    long naxes[2];
    char extname[20];
    
    fits_create_file(&outfptr, outfile, &status);
    naxes[0] = imagebin;naxes[1]=imagebin;
    xrefpixout = (1.0+ imagebin)/2.0;
    yrefpixout = (1.0+ imagebin)/2.0;
    for(jj=1;jj<=num_E_range*3+1;jj++){ /* start loop EEE for each output HDU*/
      fits_create_img(outfptr, FLOAT_IMG, 2, naxes, &status);
      fits_write_key_str(outfptr, "INSTRUME", instrume, "Instrument name", &status);
      fits_write_key_str(outfptr, "CREATOR", VARMOSAIC_VERSION, "Program to create this file", &status);
      if(jj==num_E_range*3+1){
	fits_write_key_str(outfptr, "IMATYPE", "EXPOSURE", "Type of image", &status);
	fits_write_key_str(outfptr, "EXTNAME", "EXPOSURE", "Extension name", &status);
	fits_write_key_str(outfptr, "BUNIT", "sec", "Unit for pixel values", &status);
      }else{
	fits_write_key_dbl(outfptr,"E_MIN",ScW[(jj-1)/3].E_min,-4,"Lower energy boundary [keV]",&status);
	fits_write_key_dbl(outfptr,"E_MAX",ScW[(jj-1)/3].E_max,-4,"Upper energy boundary [keV]",&status);
	if(jj%3==1){
	  fits_write_key_str(outfptr, "IMATYPE", "SIGNIFICANCE", "Type of image", &status);
	  sprintf(extname,"SIGNIFICANCE%-d",(jj-1)/3+1);
	  if(jj==1){
	    fits_write_key_str(outfptr, "HDUNAME", extname, "Extension name", &status);
	  }else{
	    fits_write_key_str(outfptr, "EXTNAME", extname, "Extension name", &status);
	  }
	  fits_write_key_str(outfptr, "BUNIT", "no unit", "Unit for pixel values", &status);
	}else if(jj%3==2){
	  fits_write_key_str(outfptr, "IMATYPE", "INTENSITY", "Type of image", &status);
	  sprintf(extname,"INTENSITY%-d",(jj-1)/3+1);
	  fits_write_key_str(outfptr, "EXTNAME", extname, "Extension name", &status);
	  fits_write_key_str(outfptr, "BUNIT", "count/s", "Unit for pixel values", &status);
	}else if(jj%3==0){
	  fits_write_key_str(outfptr, "IMATYPE", "VARIANCE", "Type of image", &status);
	  sprintf(extname,"VARIANCE%-d",(jj-1)/3+1);
	  fits_write_key_str(outfptr, "EXTNAME", extname, "Extension name", &status);
	  fits_write_key_str(outfptr, "BUNIT", "(count/s)**2", "Unit for pixel values", &status);
	}
      }
      write_wcs(outfptr,aveRA,aveDEC,xrefpixout,yrefpixout,xinc,yinc,0.0E0,&status);
      sprintf(comment, "Number of subpixel divisions: %2dx%2d",pixdivide,pixdivide);
      fits_write_comment(outfptr, comment, &status);
      sprintf(comment, "Total number of input files: %5d",nfile);
      fits_write_comment(outfptr, comment, &status);
      for(i=0;i<nfile;i++){
	sprintf(comment, "%4d: %s", i+1,inimages[i]);
	fits_write_comment(outfptr, comment, &status);
      }
    }/* end of EEE for each output HDU*/
    fits_close_file(outfptr, &status);    
  }/* end of block FFF. End creating the empty output FITS file. Necessary extensions and keywords are
      written to the output file.*/
  
  /* Allocate input and output image buffer.  We need output images, the number of which is
     3 times (flux, variance, significance) the number of energy ranges plus 1 (exposure) */
  {/* start block ZZZ*/
    int anynul,ii,kk,ienergy;
    float *in_flx_image[MAX_E_RANGES], *in_var_image[MAX_E_RANGES];
    float *flx_image[MAX_E_RANGES], *var_image[MAX_E_RANGES], *sig_image[MAX_E_RANGES], *exp_image;
    double ScWxrefval,ScWyrefval,ScWxrefpix,ScWyrefpix,ScWrot,xpos,ypos;
    double XpixOut,YpixOut;
    float flux, variance, exposure;
    float sub_exposure, sub_variance;    
    int intXpixOut,intYpixOut;

    for(ienergy=0;ienergy<num_E_range;ienergy++){
      in_flx_image[ienergy]=malloc(inimsize*inimsize*sizeof(float));
      in_var_image[ienergy]=malloc(inimsize*inimsize*sizeof(float));    
      flx_image[ienergy]=malloc(imagebin*imagebin*sizeof(float));
      sig_image[ienergy]=malloc(imagebin*imagebin*sizeof(float));
      var_image[ienergy]=malloc(imagebin*imagebin*sizeof(float));
    }
    exp_image=malloc(imagebin*imagebin*sizeof(float));
    
    /* initialize the output images */
    for(i=0;i<imagebin;i++){
      for(j=0;j<imagebin;j++){
	for(ienergy=0;ienergy<num_E_range;ienergy++){
	  flx_image[ienergy][i+imagebin*j]=0.0;
	  sig_image[ienergy][i+imagebin*j]=0.0;
	  var_image[ienergy][i+imagebin*j]=0.0;
	} 
      }
    }
    
    /* Open the input images one by one, read soft flux, soft variance, hard flux and
       hard variance images.  Add them properly taking account of variance weight,
       and write into the output file.
       Note, we do not divide pixels to save time; this causes quantization error
       below pixel level.*/
    for(i=0;i<nfile;i++){/* start loop AA for each input file*/
      fits_open_image(&infptr, inimages[i], 0, &status);
      for(ienergy=1;ienergy<=num_E_range;ienergy++){/* Read all the flux and variance maps in the input file */
	fits_movabs_hdu(infptr,ScW[ienergy-1].intHDU, &hdutype, &status); /* flux HDU */
	fits_read_img_coord(infptr, &ScWxrefval, &ScWyrefval, &ScWxrefpix, &ScWyrefpix,&xinc,&yinc,
			    &ScWrot, coordtype, &status);
	fits_read_key_flt(infptr, "EXPOSURE",   &exposure, comment, &status);
	if(strchr(instrume,'I')&&exposure<=0.0){
	  /* Occationally, ISGRI has a bug that EXPOSURE = 0.0 */
	  fits_read_key_flt(infptr, "TELAPSE",   &exposure, comment, &status);
	}
	fits_read_2d_flt(infptr, 0L, 0, inimsize, inimsize, inimsize, in_flx_image[ienergy-1], &anynul, &status);
	fits_movabs_hdu(infptr,ScW[ienergy-1].varHDU, &hdutype, &status); /* variance HDU */
	fits_read_2d_flt(infptr, 0L, 0, inimsize, inimsize, inimsize, in_var_image[ienergy-1], &anynul, &status);
      }
      fits_close_file(infptr, &status);
      sprintf(text,"Reading image:%5d %s\n", i+1, inimages[i]);
      HD_printf(text);
      for(ii=1;ii<=inimsize;ii++){for(jj=1;jj<=inimsize;jj++){/*start loop YYY for input image pixels */
	  /* Now we are dividing this input image pixel into sub-pixels.*/
	for(l=1; l<=pixdivide; l++){for(m=1; m<=pixdivide; m++){ /* Start loop CCC repeat for sub-pixels */
	  for(ienergy=1;ienergy<=num_E_range;ienergy++){/*start loop XXX for energy bands */
	    /* Read all the flux and variance maps in the input file 
	       Here, ii and jj are the X and Y pixel number of the input images,
	       starting from 1, not zero.
	    */
	    flux     = in_flx_image[ienergy-1][(ii-1)+(jj-1)*inimsize];
	    variance = in_var_image[ienergy-1][(ii-1)+(jj-1)*inimsize];
	    if(flux==flux&&variance==variance&&variance>0.0){ /* not NULL */
	      {
		double subpixelX, subpixelY, subpixelsize;
		subpixelsize = 1.0/ (double) pixdivide;
		subpixelX = (double) ii - 0.5 + (l-0.5) * subpixelsize;
		subpixelY = (double) jj - 0.5 + (m-0.5) * subpixelsize;
		fits_pix_to_world(subpixelX,subpixelY,ScWxrefval,ScWyrefval,ScWxrefpix,ScWyrefpix,xinc,yinc,ScWrot,coordtype,&xpos,&ypos,
				  &status);
	      }
	      
	      fits_world_to_pix(xpos,ypos,aveRA,aveDEC,xrefpixout,yrefpixout,xinc,yinc,0.0E0,coordtype,&XpixOut,&YpixOut,&status);
	      if(XpixOut>=0.5&&XpixOut<=imagebin+0.5&&YpixOut>=0.5&&YpixOut<=imagebin+0.5){
		intXpixOut=floor(XpixOut+0.5);
		intYpixOut=floor(YpixOut+0.5);
		/* kk is the suffix for the output image pixels */
		kk = intXpixOut-1 +(intYpixOut-1)*imagebin;
		/* Each sub-pixel carries the associated values,
		   sub_exposure=exposure/sub_pixel_numbers, sub_pixel_flux=flux, and sub_variance=sub_pixel_numbers*variance.*/
		sub_exposure = exposure/ (float)(pow(pixdivide,2));
		sub_variance = variance*(float) pow(pixdivide,2);
		if(ienergy==1){exp_image[kk]=exp_image[kk]+sub_exposure;}
		if(var_image[ienergy-1][kk]>0.0){
		  flx_image[ienergy-1][kk]=flx_image[ienergy-1][kk]/var_image[ienergy-1][kk]+ flux/sub_variance;
		  {float tmp;
		  tmp = 1.0/var_image[ienergy-1][kk]+1.0/sub_variance;
		  flx_image[ienergy-1][kk]=flx_image[ienergy-1][kk]/tmp;
		  var_image[ienergy-1][kk]=1.0/tmp;
		  }
		}else{
		  flx_image[ienergy-1][kk]=flux;
		  var_image[ienergy-1][kk]=sub_variance;
		}
		sig_image[ienergy-1][kk]=flx_image[ienergy-1][kk]/sqrt(var_image[ienergy-1][kk]);
	      }
	    }
	  } /* end XXX, loop for energy bin for each pixel. Note, energy loop is inside pixel loop, assuming all the enegy bands have the same WCS information */
	}}/* end CCC repeating for the number of sub-pixels */
      }}  /* end YYY, loop for each pixel of an input image*/
    }     /* end AA, loop for input images */
    
    /* Now we are going to write the output image */
    fits_open_image(&outfptr, outfile, 1, &status);
    for(ienergy=1;ienergy<=num_E_range;ienergy++){ /* loop for enery ranges */
      fits_movabs_hdu(outfptr,3*(ienergy-1)+1,&hdutype, &status); /*significane */
      fits_write_2d_flt(outfptr, 0L, (long) imagebin, (long) imagebin,(long)imagebin,sig_image[ienergy-1],&status);
      fits_movabs_hdu(outfptr,3*(ienergy-1)+2,&hdutype, &status); /*flux        */
      fits_write_2d_flt(outfptr, 0L, (long) imagebin, (long) imagebin,(long)imagebin,flx_image[ienergy-1],&status);
      fits_movabs_hdu(outfptr,3*(ienergy-1)+3,&hdutype, &status); /*variance    */
      fits_write_2d_flt(outfptr, 0L, (long) imagebin, (long) imagebin,(long)imagebin,var_image[ienergy-1],&status);
    }
    fits_movabs_hdu(outfptr,3*num_E_range+1,&hdutype, &status); /*Exposure    */
    fits_write_2d_flt(outfptr, 0L, (long) imagebin, (long) imagebin,(long)imagebin,exp_image,&status);
    fits_close_file(outfptr, &status);
    
    /*free memory buffer*/
    for(i=0;i<num_E_range;i++){
      free(in_flx_image[i]);
      free(in_var_image[i]);
      free(flx_image[i]);
      free(sig_image[i]);
      free(var_image[i]);
    }
    free(exp_image);
  }/*end of block ZZZ finish writing images*/
  
  /* Close the output file. */
  if(status == 0){
    sprintf(text,"varmosaic ends status = %d\n", status);
    HD_printf(text);
  }else{
    fprintf(stderr,"varmosaic ends with error.\n");
    sprintf(text,"status = %d\n", status);
    HD_printf(text);
  }
  return status;
}

int write_wcs(fitsfile *outfptr,double xrval,double yrval,double xrpix,double yrpix,double xinc,double yinc,double rot, int *status){
  fits_write_key_str(outfptr,"RADECSYS","FK5","Reference frame",status);
  fits_write_key_str(outfptr,"CTYPE1","RA---TAN","Projection method", status);
  fits_write_key_str(outfptr,"CTYPE2","DEC--TAN","Projection method", status);
  fits_write_key_dbl(outfptr,"EQUINOX",2000.0E0,-15,"Coordinate system equinox",status);
  fits_write_key_dbl(outfptr,"CRVAL1",xrval,-15,"RA at reference point", status);
  fits_write_key_dbl(outfptr,"CRVAL2",yrval,-15,"DEC at reference point",status);  
  fits_write_key_dbl(outfptr,"CRPIX1",xrpix,-15,"X at reference point",status);  
  fits_write_key_dbl(outfptr,"CRPIX2",yrpix,-15,"Y at reference point", status);  
  fits_write_key_dbl(outfptr,"CDELT1",xinc, -15,"X increment (deg)",status);  
  fits_write_key_dbl(outfptr,"CDELT2",yinc, -15,"Y increment (deg)",status);  
  fits_write_key_dbl(outfptr,"CROTA2",rot, -15,"Rotation",status);  
}
