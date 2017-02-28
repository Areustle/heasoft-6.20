/*  batwarpimg - Tool for applying small image adjustments
 *
 * $Id: batwarpimg.c,v 1.5 2009/08/21 18:27:41 craigm Exp $
 */

#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batwarpimg.h"
#include "bat_gswdev.h"

/* Main driver program */

#define TOOLSUB batwarpimg
static char taskname[] = "batwarpimg";
static char taskver[]  = "1.2";

/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

int batwarpimg_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char coordstr[FLEN_CARD] = "";
  
  parms->infile[0] = 0; /* Input image*/
  parms->outfile[0] = 0;/* Output image*/
  parms->distfile[0] = 0; /* Distortion map file */
  
  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");
  
  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("distfile", parms->distfile)))
    fprintf(stderr, "Error reading the 'distfile' parameter.\n");

  else if ((status = PILGetString("outcoord", coordstr)))
    fprintf(stderr, "Error reading the 'coordstr' parameter.\n");

  else if ((status = PILGetString("rows", parms->rowstr)))
    fprintf(stderr, "Error reading the 'rows' parameter.\n");

  else if ((status = PILGetBool("copyall", &parms->copyall)))
    fprintf(stderr, "Error reading the 'copyall' parameter.\n");

  if (status) return status;

  if (strcasecmp(coordstr,"TRUE") == 0) {
    parms->direction = APP_TO_TRUE;
  } else if (strcasecmp(coordstr,"APPARENT") == 0) {
    parms->direction = TRUE_TO_APP;
  } else {
    fprintf(stderr, "ERROR: outcoord must be one of TRUE or APPARENT\n");
    return -1;
  }
  
  return status;
  
}

void banner(struct parm_struct *parms)
     
{
  
  headas_chat(2, "***********************************************\n");
  headas_chat(1, "#        %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "     Input Image: %s\n", parms->infile);
  headas_chat(2, "    Output Image: %s\n", parms->outfile);
  headas_chat(2, "  Distortion Map: %s\n", parms->distfile);
  headas_chat(2, "   Output Coords: %s\n", (parms->direction==TRUE_TO_APP)?"APPARENT":"TRUE");
  headas_chat(2, "------------------------------------------\n");
}

void summary(struct parm_struct *parms)
{
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "... DONE \n");
}  


int read_map(fitsfile **fits, char *filename, int imgplane,
	     int imgnum, char *colname, FLOAT *nullval, int *anynull,
	     struct image_struct **img, 
	     struct image_info *info, int *status)
{
  fitsfile *infile = 0;
  struct image_struct *img1;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;

  if (fits && *fits) {
    infile = *fits;
  } else {
    if (fits_open_image(&infile, filename, READONLY, status)) {
      fprintf(stderr, "ERROR: could not open %s for reading\n", filename);
      return *status;
    }
  }
  
  img1 = image_read_i(infile, imgnum, colname, nullval, anynull, status);
  if (imgplane > 1) {
    long int fpixel[] = {1,1,1};
    fpixel[2] = imgplane;
    fits_read_pix(infile, TFLTTYPE, fpixel, img1->axes[0]*img1->axes[1], 
		  nullval, img1->data, anynull, status);
  }
  if (*status) {
    fprintf(stderr, "ERROR: could not read image from %s\n", filename);
    return (*status);
  }
  read_wcsaxis(infile, 1, "T", 0, 0, info->ctype1, 
	       &info->crpix1,&info->cdelt1, &info->crval1,status);
  read_wcsaxis(infile, 2, "T", 0, 0, info->ctype2, 
	       &info->crpix2,&info->cdelt2, &info->crval2,status);
  if (*status) {
    fprintf(stderr, "ERROR: the input image must have BAT tangent plane coordinate\n");
    fprintf(stderr, "       system (WCS alternate coordinate 'T'), but it did not\n");
    fprintf(stderr, "   (file: %s)\n", filename);
    return *status;
  }

  if (fits && infile) {
    *fits = infile;
  } else {
    int safestatus = 0;
    if (infile) fits_close_file(infile, &safestatus);
  }
  if (img && img1) {
    *img = img1;
  }

  return *status;
}

/* ----------------------------------------------------------------- */


int batwarpimg_work(struct parm_struct *parms)
{
  fitsfile *infile = 0, *outfile = 0;
  int status = 0;
  long int i, j, k;
  long int nx = 0, ny = 0, onx = 0, ony = 0;
  int direction, doall;
  double signx, signy;
  int compute;

  int nimages = 0, hdutype, distapp, imgnum;
  struct image_struct *orig = 0;
  struct image_info info, oinfo;
  struct image_struct *output = 0;

  struct image_struct *xwarpo = 0, *ywarpo = 0;
  struct image_struct *xwarpi = 0, *ywarpi = 0;
  struct image_info xwi, ywi;
  FLOAT nullval = -1e37;
  int anynull = 0;

  struct interp_struct interp;
  double *indices = 0;

  /* Row selections */
  int maxrowranges = 512, numrowranges = 0;
  long int rowrangemin[512], rowrangemax[512];

  int irowstart;
  int first = 1;

  memset(&interp, 0, sizeof(struct interp_struct));

  banner(parms);
  direction = parms->direction;



  /* Need to open the input image file before the loop starts */
  headas_chat(5, "  ..reading input image..\n");
  if (fits_open_image(&infile, parms->infile, READONLY, &status)) {
    fprintf(stderr, "ERROR: could not open %s for reading\n", parms->infile);
    return status;
  }

  /* Locate the distortion map via CALDB if requested */
  if (strcasecmp(parms->distfile, "CALDB") == 0) {
    struct caldbparms_struct caldb;
    char *expr = "-";
    char *codenam = "DET_POSCOR";
    char *pfile = parms->distfile;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;
    
    batkw_to_caldb_parms(infile, &caldb, 1, &status);

    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the BAT teldef file in CALDB\n");
      return status;
    }
  }	       


  nimages = image_nimages(infile, &hdutype, &status);
  headas_chat(5, "    (%d images present)\n", nimages);

  headas_chat(5, "...parsing 'rows'...\n");
  if (fits_parse_range(parms->rowstr, nimages, maxrowranges,
		       &numrowranges, rowrangemin, rowrangemax, &status)) {
    fprintf(stderr, "ERROR: could not parse 'rows' parameter ('%s')\n",
	    parms->rowstr);
    return status;
  }

  /* Check to see if the extension number was explicitly given or not */
  doall = 1;
  if (strcmp(parms->rowstr, "-") == 0) {
    int hdunum;
    fits_get_hdu_num(infile, &hdunum);
    if (hdunum > 1) {
      doall = 0;   /* a specific extension was given */
    } else {
      if (strstr(parms->infile, "+0") || strstr(parms->infile, "[0]") )
	doall = 0;  /* the primary array was specifically given */
    }
  }
  headas_chat(5, "  ..doall=%d..\n", doall);

  if (!doall) {
    /* Do only a single image */
    irowstart = -1;
    nimages = -1;
    numrowranges = 1;
    rowrangemin[0] = -1;
    rowrangemax[0] = -1;
  } else {
    /* Do all requested images */
    irowstart = 1;
  }

  for(imgnum=irowstart; imgnum<=nimages; imgnum++) {
    /* ============== Read input image */

    /* Check to see if this image is in the requested row ranges */
    for (j=0; j<numrowranges; j++) {
      if ((imgnum >= rowrangemin[j]) && (imgnum <= rowrangemax[j])) break;
    }
    /* This image was not found */
    if (j == numrowranges) continue;

    headas_chat(5, "  ..reading input image %d..\n", imgnum);
    if (orig) { image_free(orig); orig = 0; }
    if (read_map(&infile, parms->infile, 1, imgnum, 0, &nullval, &anynull,
		 &orig, &info, &status)) {
      return status;
    }

    /* Check for the BDISTAPP keyword, which signals this image may
       have already been processed */
    distapp = 1;
    fits_write_errmark();
    fits_read_key(infile, TLOGICAL, "BDISTAPP", &distapp, 0, &status);
    fits_clear_errmark();
    /* "not found" is equivalent to "not corrected" */
    if ( ((distapp == 0 || status) && direction == TRUE_TO_APP) ||
	 (distapp && status == 0 && direction == APP_TO_TRUE) ){
      fprintf(stderr, 
	      "WARNING: image %s has already been converted.\n"
	      "         Skipping this image.\n", parms->infile);
      continue;
    }
    status = 0;
	      
    nx = orig->axes[0]; ny = orig->axes[1];
    headas_chat(5, "    (%dx%d)\n", nx, ny);
    
    /* ================ Read warp maps if this is the first time */
    if (xwarpo == 0) {
      headas_chat(5, "  ..reading warp maps..\n");
      if (read_map(0, parms->distfile, 1, -1, 0, &nullval, &anynull,
		   &xwarpo, &xwi, &status)) {
	return status;
      }
      headas_chat(5, "    (X map %dx%d)\n", xwarpo->axes[0], xwarpo->axes[1]);
    }
    if (ywarpo == 0) {
      if (read_map(0, parms->distfile, 2, -1, 0, &nullval, &anynull,
		   &ywarpo, &ywi, &status)) {
	return status;
      }
      headas_chat(5, "    (Y map %dx%d)\n", ywarpo->axes[0], ywarpo->axes[1]);
    }

    /* Check to see if we need to recompute the interpolation values */
    if ((first == 0) &&
	(nx == onx && ny == ony) &&
	(info.crpix1 == oinfo.crpix1) && (info.crpix2 == oinfo.crpix2) &&
	(info.crval1 == oinfo.crval1) && (info.crval2 == oinfo.crval2) &&
	(info.cdelt1 == oinfo.cdelt1) && (info.cdelt2 == oinfo.cdelt2)) {
      /* All values the same: no need to recompute */
      compute = 0;
    } else {
      compute = 1;

      /* If the size of the image has changed then we need to also
	 delete the interpolation index array */
      if ((first == 0) && ((nx != onx) || (ny != ony))) { 
	if (indices) free(indices);
	indices = 0;
	imageinterp_free(&interp);
      }
    }
    
    /* ================ Sample warp maps up to full map resolution */
    if (compute) {
      if (!indices) indices = (double *)malloc(nx*ny*2*sizeof(double));
      if (!indices) { 
	fprintf(stderr, "ERROR: could not allocation internal memory\n");
	return MEMORY_ALLOCATION;
      }
      
      headas_chat(5, "  ..up-sampling warp maps to full size..\n");
      headas_chat(5, "  ..X map..\n");
      for (k=0,j=0; j<ny; j++) {
	for (i=0; i<nx; i++, k+=2) {
	  indices[k]   = (((i+1.0)-info.crpix1)*info.cdelt1+info.crval1-xwi.crval1)/xwi.cdelt1 + xwi.crpix1;
	  indices[k+1] = (((j+1.0)-info.crpix2)*info.cdelt2+info.crval2-xwi.crval2)/xwi.cdelt2 + xwi.crpix2;
	}
      }
      imageinterp_init(xwarpo->axes[0], xwarpo->axes[1], indices, nx*ny,
		       INTERP_NEARN, 0, 0.0, &interp);
      
      if (xwarpi) { image_free(xwarpi); xwarpi = 0; }
      xwarpi = image_init(nx, ny, 0);
      xwarpi->nullval = nullval;
      imageinterp_apply(xwarpo->data, xwarpi->data, nullval, &interp);
      
      headas_chat(5, "  ..Y map..\n");
      /* Only compute if the coordinate systems are different */
      if ((xwarpo->axes[0] == ywarpo->axes[0]) && 
	  (xwarpo->axes[1] == ywarpo->axes[1]) && 
	  (ywi.crpix1 == xwi.crpix1) && (ywi.crpix2 == xwi.crpix2) &&
	  (ywi.crval1 == xwi.crval1) && (ywi.crval2 == xwi.crval2) &&
	  (ywi.cdelt1 == xwi.cdelt1) && (ywi.cdelt2 == xwi.cdelt2)) {
	headas_chat(5, "    (same as X map)\n");
      } else {
	imageinterp_free(&interp);
	for (k=0,j=0; j<ny; j++) {
	  for (i=0; i<nx; i++, k+=2) {
	    indices[k]   = (((i+1.0)-info.crpix1)*info.cdelt1+info.crval1-ywi.crval1)/ywi.cdelt1 + ywi.crpix1;
	    indices[k+1] = (((j+1.0)-info.crpix2)*info.cdelt2+info.crval2-ywi.crval2)/ywi.cdelt2 + ywi.crpix2;
	  }
	}
	imageinterp_init(ywarpo->axes[0], ywarpo->axes[1], indices, nx*ny,
			 INTERP_NEARN, 0, 0.0, &interp);
      }
      
      if (ywarpi) { image_free(ywarpi); ywarpi = 0; }
      ywarpi = image_init(nx, ny, 0);
      ywarpi->nullval = nullval;
      imageinterp_apply(ywarpo->data, ywarpi->data, nullval, &interp);
      imageinterp_free(&interp);
      
      /* ================ Make new indices */
      headas_chat(5, "  ..making master image interpolation indices..\n");
      /* NOTE:  
       *   Since we are warping the coordinate *grid* instead of 
       *     source centroids, the sign is negative.
       */
      signx = -1.0/info.cdelt1;
      signy = -1.0/info.cdelt2;
      if (direction != APP_TO_TRUE) { 
	signx = -signx;
	signy = -signy;
      }
      for (k=0,j=0; j<ny; j++) {
	for (i=0; i<nx; i++, k+=2) {
	  /* dx,dy contain the apparent-actual */
	  double dx = xwarpi->datap[j][i];
	  double dy = ywarpi->datap[j][i];
	  
	  indices[k]   = i+1.0+signx*dx;
	  indices[k+1] = j+1.0+signy*dy;
#if 0
	  if (k % 20000 == 0) {
	    headas_chat(6, "   %8d (i,d)=(%4d,%3d) (IMX,Y)=(%9.3f,%9.3f) (DIMX,DIMY)=(%12f,%12f)\n",
			k, i, j, 
			(i+1.0-info.crpix1)*info.cdelt1+info.crval1,
			(j+1.0-info.crpix2)*info.cdelt2+info.crval2,
			dx, dy);
	  }
#endif
	}
      }

      /* ================ Interpolate to new image */
      headas_chat(5, "  ..initialize interpolation..\n");
      imageinterp_init(nx, ny, indices, nx*ny,
		       INTERP_BILINEAR, 0, 0.0, &interp);

    }  /* End of interpolation preparation */

    if (output) { image_free(output); output = 0; }
    output = image_init(nx, ny, 0);
    output->nullval = nullval;
    headas_chat(5, "  ..apply interpolation..\n");
    imageinterp_apply(orig->data, output->data, nullval, &interp);
    
    /* ------------------------------- */
    /* Write output image */
    headas_chat(5, "  ..write output..\n");
    if (first) {
      headas_clobberfile(parms->outfile);
      if (fits_create_file(&outfile, parms->outfile, &status)) {
	fprintf(stderr, "ERROR: could not create %s\n", parms->outfile);
	return status;
      }
      /* Copy any preceding extensions */
      if (parms->copyall) {
	fits_copy_file(infile, outfile, 1, 0, 0, &status);
      }
    }

    image_write(outfile, output, &status);
    image_copykeyclasses(outfile, infile, TYP_SCAL_KEY, 0, &status);
    distapp = (direction == APP_TO_TRUE) ? 1 : 0;
    fits_update_key(outfile, TLOGICAL, "BDISTAPP", &distapp, 
		    " BAT image corrected for distortions?", &status);
    fits_set_hdustruc(outfile, &status);
    
    /* ------------------------------- */
    /* Append history keywords */
    if (status == 0) {
      status = HDpar_stamp(outfile, 0, &status);
      
      if (status != 0) {
	fprintf(stderr, "ERROR: could not write history to %s\n", parms->outfile);
      }
      
    }

    first = 0;
    /* Save these successful image values to compare with the next */
    oinfo = info;  onx = nx; ony = ny;
  } /* End of main FOR loop */
    
  /* Copy any trailing extensions if requested */
  if (infile && outfile && parms->copyall) {
    int hdunum = 0;
    fits_get_hdu_num(infile, &hdunum);
    headas_chat(5, "  ..copying all extensions (from %d)..\n", hdunum);
    fits_copy_file(infile, outfile, 0, 0, 1, &status);
  }

  /* ------------------------------- */
  /* Close files and free memory */
  if (indices) free(indices);
  if (xwarpo) { image_free(xwarpo); }
  if (ywarpo) { image_free(ywarpo); }
  if (xwarpi) { image_free(xwarpi); }
  if (ywarpi) { image_free(ywarpi); }
  if (orig) { image_free(orig); }
  if (output) { image_free(output); }
  imageinterp_free(&interp);

  if (infile) {
    int safestatus = 0;
    fits_close_file(infile, &safestatus);
    infile = 0;
  }
  
  if (outfile) {
    int safestatus = 0;
    fits_close_file(outfile, &safestatus);
    outfile = 0;
  }

  summary(parms);
  return status;
}


int batwarpimg(void)
{
  int status = 0;
  
  struct parm_struct parms;
  
  /* Register taskname and version. */
  
  set_toolname(taskname);
  set_toolversion(taskver);
  
  if ((status = batwarpimg_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }
  
  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];
  
  return batwarpimg_work(&parms);
  
}
