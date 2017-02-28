#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"

#include "coordfits.h"
#include "batfftimage.h"

/* 
 * Task to reconstruct sky image from BAT detector plane image
 *   - routines for file I/O
 *
 * $Id: fileio.c,v 1.9 2010/12/16 06:26:44 craigm Exp $
 *
 * 27 Mar 2005 - split from batfftimage.c
 * 
 */

/* ============================================================= */

/* ------------------------------------------------------------------------- */
/*
 * getimage - retrieve image and image parameters
 *
 * fitsfile *file - pointer to open FITS file
 * int imgnum - image number 
 * int hdutype - type of FITS HDU (IMAGE_HDU or BINTABLE_HDU)
 * char *countscol - for image tables, the column name (or NULL for default)
 * FLOAT *nullval - value to substitute for null pixels
 * int *anynul - upon return, *anynul is 0/1 if nulls are not/are present
 * double *tstart, *tstop - upon return, *tstart/tstop are the image 
 *                          start/stop times
 * double *exposure - upon return, *exposure is the image exposure time
 * int *status - upon return, *status is the FITS status code
 *
 * RETURNS: pointer to image structure.  User is responsible to free
 * this structure when done.
 *
 */
struct image_struct *getimage(fitsfile *file, int imgnum, int hdutype,
			      char *countscol,
			      FLOAT *nullval, int *anynull, 
			      double *tstart, double *tstop, 
			      double *exposure,
			      int *status)
{
  struct image_struct *image = 0;
  int safestatus = 0;

  if (status == 0) return 0;
  if (*status) return 0;

  image = image_read_i(file, imgnum, countscol, 
		       nullval, anynull, status);
  if (*status) return 0;

  *exposure = 0;
  *tstart = 0;
  *tstop = 0;

  if (hdutype == IMAGE_HDU) {
    fits_write_errmark();
    safestatus = 0;  /* Fail gracefully */
    fits_read_key(file, TDOUBLE, "EXPOSURE", exposure, 0, &safestatus);
    if (safestatus) {
      safestatus = 0;
      fits_read_key(file, TDOUBLE, "ONTIME", exposure, 0, &safestatus);
    }

    safestatus = 0;
    fits_read_key(file, TDOUBLE, "TSTART", tstart, 0, &safestatus);
    safestatus = 0;
    fits_read_key(file, TDOUBLE, "TSTOP", tstop, 0, &safestatus);
    safestatus = 0;
    
    if ((*exposure) == 0) {
      *exposure = *tstop - *tstart;
    }
    
    fits_clear_errmark();
  } else {
    int colnum = 0;

    fits_write_errmark();
    safestatus = 0;  /* Fail gracefully */
    fits_get_colnum(file, CASEINSEN, "TIME", &colnum, &safestatus);
    fits_read_col(file, TDOUBLE, colnum, imgnum, 1, 1, 0, tstart, 0, &safestatus);

    safestatus = 0;  /* Fail gracefully */
    fits_get_colnum(file, CASEINSEN, "TIME_STOP", &colnum, &safestatus);
    fits_read_col(file, TDOUBLE, colnum, imgnum, 1, 1, 0, tstop, 0, &safestatus);
    safestatus = 0;
    fits_get_colnum(file, CASEINSEN, "EXPOSURE", &colnum, &safestatus);
    if (safestatus) {
      safestatus = 0;
      fits_get_colnum(file, CASEINSEN, "ONTIME", &colnum, &safestatus);
    }
    fits_clear_errmark();

    if (safestatus == 0) {
      fits_read_col(file, TDOUBLE, colnum, imgnum, 1, 1, 0, exposure, 0, status);
    } else {
      *exposure = (*tstop) - (*tstart);
    }

  }
  headas_chat(5,"   (exposure=%f)\n", *exposure);

  return image;
}

/* ------------------------------------------------------------------------- */

/* 
 * locate_caldb_files - find CALDB files (aperture & TELDEF)
 *
 * struct parm_struct *parms - pointer to parameter structure
 *                     parms->infile - name of input detector image file
 *                     parms->aperture - name of aperture, or "CALDB"
 *                     parms->teldef - name of TELDEF file, or "CALDB"
 * fitsfile **dpifile - pointer to FITS file pointer.  Upon return,
 *                      the detector image file may have been opened.  The
 *                      caller is responsible for closing.
 *
 * RETURNS: FITS error status code
 *
 */
int locate_caldb_files(struct parm_struct *parms, fitsfile **dpifile)
{
  struct caldbparms_struct caldb;
  int status = 0;

  if (parms == 0 || dpifile == 0) return NULL_INPUT_PTR;

  /* Open the detector image now.  This is needed to read the time
     keywords, which are needed to refer to CALDB */
  fits_open_file(dpifile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s (CALDB)\n", parms->infile);
    return status;
  }
  batkw_to_caldb_parms(*dpifile, &caldb, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
	    parms->infile);
    return status;
  }

  /* Locate the aperture file */
  if (strncasecmp(parms->aperture, "CALDB", 5) == 0) {
    char expr[80];
    char *codenam = "CODED_MASK";
    char *pfile = parms->aperture;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;

    if (strcasecmp(parms->aperture, "CALDB") == 0) {
      headas_chat(1, "NOTE: Using the 'FLUX' aperture type\n");
      strcpy(expr, "APERTYPE.eq.\"FLUX\"");
    } else if (strncasecmp(parms->aperture, "CALDB:",6) == 0) {
      sprintf(expr, "APERTYPE.eq.\"%s\"", parms->aperture+6);
    } else {
      fprintf(stderr, "ERROR: aperture must be either CALDB or CALDB:apertype\n");
      return -1;
    }
      
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the aperture file in CALDB\n");
      return status;
    }
  }	       
	
  /* Locate the TELDEF file */
  if (parms->attitude[0] && (strcasecmp(parms->teldef, "CALDB") == 0)) {
    char *expr = "-";
    char *codenam = "TELDEF";
    char *pfile = parms->teldef;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;
    
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the BAT teldef file in CALDB\n");
      return status;
    }
  }	       

  return status;
}

/* ------------------------------------------------------------------------- */

/* 
 * read_qmap - read quality map
 *
 * struct parm_struct *parms - pointer to parameter structure
 *                     parms->detmask - name of quality map file
 *                     parms->goodval - value indicating "good" quality
 *
 * struct image_struct **detmask - upon return (*detmask) points to an
 *                     image structure containing the quality map
 *
 * RETURNS: FITS status code 
 */

int read_qmap(struct parm_struct *parms, struct image_struct **detmask)
{
  fitsfile *imgfile = 0;
  int status = 0, safestatus = 0;

  if (parms == 0 || detmask == 0) return NULL_INPUT_PTR;

  headas_chat(5, "...reading detector mask image...\n");
  fits_open_image(&imgfile, parms->detmask, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->detmask);
    return status;
  }
  
  (*detmask) = image_read(imgfile, &status);
  
  /* Read GOODVAL keyword.  If the keyword is not found, then assume
     that 1=good, 0=bad.  If the keyword is found, then use that
     value for good, and anything else for bad. */
  safestatus = 0;
  fits_write_errmark();
  fits_read_key(imgfile, TINT, "GOODVAL", &parms->goodval, 0, &safestatus);
  fits_clear_errmark();
  if (safestatus) parms->goodval = 0;
  
  safestatus = 0;
  fits_close_file(imgfile, &safestatus);
  if (status) {
    fprintf(stderr, "ERROR: could not read detector mask data from %s\n", 
	    parms->detmask);
    return status;
  }
  
  if (parms->goodval != 1) {
    /* Convert from 0 == good, else bad representation, to
       1 == good, else bad representation */
    
    headas_chat(5, "...converting mask to 1=good, 0=bad...\n");
    foreach_pixel((*detmask), i, 
    {(*detmask)->data[i] = ((*detmask)->data[i] == parms->goodval);});
  }
  
  /* Apply the detector gaps just to be sure */
  headas_chat(5, "...applying detector gaps to detector mask image...\n");
  image_mkdetgaps(*detmask);

  return status;
}

/* ------------------------------------------------------------------------- */

/*
 * read_focal - read focal plane map (and background
 *
 * fitsfile *dpifile - already-open FITS file containing DPI
 * int imgnum - image number to read (starting with 1)
 * fitsfile *bkgfile - already-open FITS file containing background image
 *                     (or NULL if no background is present)
 * int nbkgimages - total number of background images
 *                  (if nbkgimages > 1 then the bkg image number is
 *                  selected by imgnum)
 * int hdutype - type of FITS HDU (IMAGE_HDU or BINTABLE_HDU)
 * int bkg_hdutype - type of background FITS HDU (IMAGE_HDU or BINTABLE_HDU)
 * char *countscol - table column to read image data (or NULL for default)
 * struct parm_struct *parms - pointer to parameter structure
 *                     parms->infile - name of input image file
 *                     parms->bkgfile - name of input background image file
 *                     parms->tstart - upon return, start MET of image
 *                     parms->tstop - upon return, stop MET of image
 *                     parms->exposure - upon return, exposure of image [s]
 * int *anynull - upon return, (*anynull) is true if the image contains nulls
 * int *status - upon return, CFITSIO status variable
 *
 * RETURNS: pointer to image_struct structure, which contains the
 * requested single image, background subtracted if requested.  The
 * user is responsible for freeing the image with image_free().
 *
 * Modified by GKS 7 feb 05
 * Now also returns an image to be used for variance calculation
 * Returned as an argument vardat  -  not neat, but avoids changing structure definition
 * If the real image is in units of counts and not background subtracted, 
 * then the vardat data array is simply a copy of the counts array
 * If background has been subtracted, then in vardat it is ADDED
 * If working in rates, or if the background is not for the same time, then a factor 
 * is applied in vardat so that vardat should contain the variance if Poisson appplies
 *
 */
struct image_struct *read_focal(fitsfile *dpifile, int imgnum, 
				fitsfile *bkgfile, int nbkgimages,
				int hdutype, int bkg_hdutype,
				char *countscol,
				struct parm_struct *parms,
				struct image_struct **pvardat,
				int *anynull, int *status)
{
  struct image_struct *focal = 0;
  FLOAT nullval = 1e38;
  struct image_struct *vardat = 0;
  char hduclas2[FLEN_CARD];
  int safestatus = 0;

  if ((dpifile == 0) || (parms == 0) || (status == 0)) return 0;
  if (*status) return 0;
  
  /* ------ Read the input image */
  *anynull = 0;  /* Default is no null values */
  focal = getimage(dpifile, imgnum, hdutype, countscol,
		   &nullval, anynull, 
		   &(parms->tstart), &(parms->tstop),
		   &(parms->exposure), status);
  if ((focal == 0) || (*status != 0)) {
    fprintf(stderr, "ERROR: could not read input image %s [%d]\n", 
	    parms->infile, imgnum);
    if (*status == 0) *status = -1;
    return 0;
  }
  
  /* Create focal variance image */
  safestatus = 0;
  fits_write_errmark();
  fits_read_key(dpifile, TSTRING, "HDUCLAS2", hduclas2, 0, &safestatus);
  fits_clear_errmark();
  if (safestatus == 0 && !strcasecmp(hduclas2, "RESIDUAL")) {
    vardat = 0;
    fprintf(stderr, "WARNING: Input image is a residual map.\n");
    fprintf(stderr, "         Cannot compute a valid variance map.\n");
  } else {
    vardat = image_copy(focal);
    if (vardat == 0) {
      fprintf(stderr, "ERROR: could not create focal variance map\n");
      *status = MEMORY_ALLOCATION;
      return 0;
    }
  }
  if ( vardat && is_units_rate(focal->units) && (parms->exposure > 0)) {
    foreach_pixel(vardat, i, { vardat->data[i] /= (parms->exposure);});
  }


  /* ------ Read the background image */
  if (bkgfile) {
    int bkg_anynull = 0; /* Default is no null values */
    double bkg_exposure = 0, bkg_tstart = 0, bkg_tstop = 0;
    struct image_struct *bkg_image = 0;
    int bkg_imgnum = imgnum;
    double bkgfact = 1;
    double bkgfact_v = 1;        /* ditto for vardat */
    int nbkgnulls = 0;
    
    if (nbkgimages == 1) bkg_imgnum = 1;

    headas_chat(5, "...reading background image %d...\n", bkg_imgnum);
    bkg_image = getimage(bkgfile, bkg_imgnum, bkg_hdutype, countscol,
			 &nullval, &bkg_anynull, 
			 &bkg_tstart, &bkg_tstop,
			 &bkg_exposure, status);
    if ((bkg_image == 0) || (*status != 0)) {
      fprintf(stderr, "ERROR: could not read background image %s [%d]\n", 
	      parms->bkgfile, bkg_imgnum);
      if (*status == 0) *status = -1;
      if (focal) image_free(focal);
      return 0;
    }
    /* Check image dimensions */
    if ( ((bkg_image->axes[0] != focal->axes[0]) || 
	  (bkg_image->axes[1] != focal->axes[1])) ) {
      headas_chat(5,"   (focal [%dx%d]  background [%dx%d])\n",
		  focal->axes[0], focal->axes[1],
		  bkg_image->axes[0], bkg_image->axes[1]);
      fprintf(stderr, 
	      "ERROR: Background image must have same dimensions as detector image\n");
      if (bkg_image) image_free(bkg_image);  bkg_image = 0;
      if (focal)     image_free(focal);      focal = 0;
      *status = BAD_DIMEN;
      return 0;
    }
      
    if (!is_units_rate(bkg_image->units) && (bkg_exposure > 0)) {
      /* If background is in counts, convert to rate */
      bkgfact *= 1.0 / (bkg_exposure);
    }
    if ( is_units_rate(bkg_image->units) && (bkg_exposure > 0)) {
      /* If background is in rate, get factor to correct variance
	 gks */
      bkgfact_v *= 1.0 / (bkg_exposure);
    }

    if (!is_units_rate(focal->units) && (parms->exposure > 0)) {
      /* If focal image is in counts, convert background rate to counts */
      bkgfact *= (parms->exposure);
    }
    bkgfact_v *= bkgfact * bkgfact ;    /*  what we do to data, we do
					    to variance in spades
					    gks */

    /* except that if bkg is from batclean, we can assume the variance
       contribution is negligable gks */

    safestatus = 0;
    fits_write_errmark();
    fits_read_key(bkgfile, TSTRING, "HDUCLAS2", hduclas2, 0, &safestatus);
    fits_clear_errmark();
    if (safestatus == 0 && !strcasecmp(hduclas2, "PREDICTED")) {
      bkgfact_v = 0.0;
      headas_chat(5,"...ignoring background variance because it is modeled...\n");
    }

    headas_chat(5,"   (background factor=%f)\n", bkgfact);
    headas_chat(5,"   (fg exposure=%f     bg exposure=%f )\n", parms->exposure, bkg_exposure );
    headas_chat(5,"   (background factor for variance=%f)\n", bkgfact_v);

    
    /* Perform the background subtraction */
    headas_chat(5,"...performing background subtraction...\n");
    if (bkg_anynull) {
      /* If there are background nulls, transfer the nulls to the focal map */
      int i;
      for (i = 0; i<(bkg_image->axes[0]*bkg_image->axes[1]); i++) {
	if (bkg_image->data[i] == nullval) {
	  focal->data[i] = nullval; 
	  nbkgnulls++;
	} else {
	  focal->data[i]  -= (bkgfact*bkg_image->data[i]);
	  if (vardat) {
	    vardat->data[i] += (bkgfact_v*bkg_image->data[i]);
	  }
	}
      }
    } else {
      foreach_pixel(focal, i, {focal->data[i] -= (bkgfact*bkg_image->data[i]);});
      if (vardat) {
	foreach_pixel(vardat, i, {vardat->data[i] += (bkgfact_v*bkg_image->data[i]);});
      }
    }
    headas_chat(5, "  (found %d nulls in background image)\n", nbkgnulls);
    
    image_free(bkg_image); bkg_image = 0;
  }

  *pvardat = 0;
  if (vardat) {
    *pvardat = vardat;
  }

  return focal;
}

int image_out(fitsfile *outfile, fitsfile *dpifile,
	      struct image_struct *outimg, 
	      struct parm_struct *parms, int imgtype,
	      int imgnum, double mwfcorr,
	      struct image_struct *focal,
	      struct image_struct *aperture,
	      struct batmaskplane_struct *mask,
	      struct batdetplane_struct *detplane,
	      struct wcs_coord *wcs, FLOAT nullval, int ndet,
	      int *ihdu)

{
  int status = 0;
  char extname[FLEN_CARD];
  char *p;
  int pcodecorr;
  int pcodemap = imgtype;
  int is_pcodemap = ((pcodemap == PCODEMAP_FILE) || (pcodemap == PCODEMAP_APPEND) ||
		     (pcodemap == PCODEMAP_LAST));
  int hduclas3_erase = 0;
  /* FITS standard used to forbid EXTNAME in the primary HDU but now
     allows it (version 3.0; released 2008) */
  char *extkey = "EXTNAME";
  int bdistapp = 0;

  pcodecorr = parms->pcodecorr;
  if (is_pcodemap) pcodecorr = 0;  /* We don't correct the correction! */

  image_write(outfile, outimg, &status);
  if (pcodemap == PCODEMAP_LAST) {
    /* Option '3' indicates a final partial coding map, always the first */
    strcpy(extname, "BAT_PCODE_1");
  } else if (is_pcodemap) {
    sprintf(extname, "BAT_PCODE_%d", imgnum);
  } else if (imgtype == BKGVARMAP) {
    sprintf(extname, "BAT_VARMAP_%d", imgnum);
  } else if (imgtype == SIGNIFMAP) {
    sprintf(extname, "BAT_SIGNIF_%d", imgnum);
  } else {
    sprintf(extname, "BAT_IMAGE_%d", imgnum);
  }
  *ihdu = *ihdu + 1;

  /* Write a unique extension name */
  fits_update_key(outfile, TSTRING, extkey, extname, 
		  "Name of extension", &status);

  fits_set_hdustruc(outfile, &status);

  /* -------------- */
  /* Write keywords */
  if (dpifile) image_copykeys(outfile, dpifile, &status);
  image_writeinfo(outfile, outimg, &status);  /* Write time keywords */

  headas_chat(5, "...writing coordinate keywords... (status=%d)\n", status);
  write_imgkeys(outfile, outimg->axes, parms, 
		focal, aperture, mask, detplane, &status);
  headas_chat(5, "...writing mask orientation keywords... (status=%d)\n", 
	      status);
  mask_writekey(outfile, mask, &status);
  headas_chat(5, "...writing detector orientation keywords... (status=%d)\n",
	      status);
  detplane_writekey(outfile, detplane, &status);
  /* If the source is not at infinity, then write the source position 
     in BAT_Z to the image */

  /* -------------- */
  /* Write OGIP HDUCLASn keywords */
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP",
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "IMAGE", 
		  "Contains image data", &status);
  if (is_pcodemap) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "VIGNETTING",
		    "Contains partial coding map", &status);
    fits_update_key(outfile, TSTRING, "IMATYPE", "EXPOSURE",
		    "Contains partial coding map", &status);
    hduclas3_erase = 1;
  } else if (imgtype == FLUXMAP) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "NET",
		    "Contains net flux map", &status);
    fits_update_key(outfile, TSTRING, "IMATYPE", "INTENSITY",
		    "Contains net flux map", &status);
    hduclas3_erase = 1;
  } else if (imgtype == BKGVARMAP && parms->bkgvartype == STDDEVMAP) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "BKG_STDDEV",
		    "Contains std. deviation map", &status);
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "PREDICTED",
		    "Predicted standard deviation", &status);
    fits_update_key(outfile, TSTRING, "IMATYPE", "ERROR",
		    "Contains std. deviation map", &status);
    hduclas3_erase = 0;
  } else if (imgtype == BKGVARMAP) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "BKG_VARIANCE",
		    "Contains variance map", &status);
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "PREDICTED",
		    "Theoretical variance", &status);
    fits_update_key(outfile, TSTRING, "IMATYPE", "VARIANCE",
		    "Contains variance map", &status);
    hduclas3_erase = 0;
  } else if (imgtype == SIGNIFMAP) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "SIGNIFICANCE",
		    "Contains significance map", &status);
    fits_update_key(outfile, TSTRING, "IMATYPE", "SIGNIFICANCE",
		    "Contains significance map", &status);
    hduclas3_erase = 1;
  }

  if (hduclas3_erase) {
    /* Erase any stray HDUCLAS3 keywords which leaked through */
    int mystatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "HDUCLAS3", &mystatus);
    fits_clear_errmark();
  }


  /* -------------- */
  /* Special BAT correction keywords */
  if (parms->srcpos[2] > 0) {
    fits_update_key(outfile, TDOUBLE, "BAT_ZOBJ", &parms->srcpos[2], 
		    "[cm] Position of source in BAT_Z", &status);
  }
  fits_update_key(outfile, TLOGICAL, "ACOLAPP", &(parms->cautocollim), 
		  "Was autocollimation correction applied?", &status);
  fits_update_key(outfile, TLOGICAL, "PCODEAPP", &pcodecorr,
		  "Partial coding correction applied?", &status);
  fits_update_key(outfile, TLOGICAL, "FFAPP", &(parms->cflatfield),
		  "Projection correction applied?", &status);
  if (parms->ccosine) 
    fits_update_key(outfile, TLOGICAL, "COSAPP", &(parms->ccosine),
		    "Cosine correction applied?", &status);
  fits_update_key(outfile, TINT, "NBATDETS", &ndet, 
		  "Number of enabled detectors", &status);
  fits_update_key(outfile, TINT, "NGOODPIX", &ndet,
		  "Number of enabled detectors", &status);
  fits_update_key(outfile, TLOGICAL, "NGPIXAPP", &(parms->cndet),
		  "Normalized by number of detectors?", &status);
  if (parms->bkgfile[0]) {
    int true = 1;
    fits_update_key(outfile, TLOGICAL, "BACKAPP", &true,
		    "Was background subtracted?", &status);
  }
  /* Indicate whether distortion correction has been applied */
  /*   1 = image is in "true" coordinates */
  /*   0 = image is in "apparent" coordinates */
  bdistapp = 0;  /* ALWAYS FALSE - this task always produces an apparent image */
  fits_update_key(outfile, TLOGICAL, "BDISTAPP", &bdistapp, 
		  "BAT image corrected for distortions?", &status);

  if ( is_pcodemap == 0 ) {
    fits_update_key(outfile, TLOGICAL, "MSKWTAPP", &(parms->cmaskwt),
		    "Correction for mask weight technique applied?", &status);
    fits_update_key(outfile, TSTRING, "FLUXMETH", "WEIGHTED", 
		    "Flux extraction method", &status);
  } else {
    /* Partial coding map -- remove the units if they are present */
    int mystatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "BUNIT", &mystatus); mystatus = 0;
    fits_delete_key(outfile, "BUNITS", &mystatus); mystatus = 0;
    fits_clear_errmark();
  }


  /* -------------- */
  /* Squared units for the true variance map */
  if (imgtype == BKGVARMAP && parms->bkgvartype == BKGVARMAP) {
    int mystatus = 0;
    char bunit[FLEN_CARD], bunit_sq[FLEN_CARD];
    fits_write_errmark();
    fits_read_key(outfile, TSTRING, "BUNIT", bunit, 0, &mystatus);

    if (mystatus == 0) {
      sprintf(bunit_sq, "(%s)**2", bunit);
      fits_update_key(outfile, TSTRING, "BUNIT", bunit_sq, 0, &mystatus);
    }
    fits_clear_errmark();
  }

  fits_update_key(outfile, TDOUBLE, "MSKWTSQF", &mwfcorr, 
		  "Half-variance of mask weights", &status);

  /* Output teldef and aperture file names as keywords */
  /* Remove path components */
  p = rindex(parms->aperture,'/');
  if (p == 0) p = parms->aperture; else p++;
  fits_update_key(outfile, TSTRING, "APERTURE", p, 
		  "BAT aperture file name", &status);
  
  p = rindex(parms->teldef,'/');
  if (p == 0) p = parms->teldef; else p++;
  fits_update_key(outfile, TSTRING, "BTELDEF", p, 
		  "BAT teldef file name", &status);

  /* -------------- */
  /* Append history keywords */
  if (status == 0) {
    status = HDpar_stamp(outfile, 0, &status);

    if (status != 0) {
      fprintf(stderr, "ERROR: could not write history to %s\n", parms->outfile);
    }

  }
  fits_set_hdustruc(outfile, &status);

  return status;
}
