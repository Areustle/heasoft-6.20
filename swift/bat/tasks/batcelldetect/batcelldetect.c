#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "mpfit.h"
#include "bat_gswdev.h"

#include "batcelldetect.h"

static char taskname[] = "batcelldetect";
static char taskver[]  = "1.85";

/* =================================================================== */
/* 
 * batcelldetect
 * Cell Detection for BAT Images
 *
 * Main driver program
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: batcelldetect.c,v 1.151 2011/04/29 17:15:44 craigm Exp $
 */

/* Main driver program */

#define TOOLSUB batcelldetect
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* =================================================================== */
int batcelldetect_work(struct parm_struct *parms)
{
  struct image_struct *mask = 0, *bkgmask = 0, *pcode = 0;
  fitsfile *imgfile = 0, *pcodefile = 0;
  fitsfile *bkgfile = 0, *bkgvarfile = 0;
  int ihdu_signifmap = 0, ihdu_bkgmap = 0, ihdu_bkgvarmap = 0;
  int nx, ny;
  int status = 0;
  int i, j, imgindex;
  int auxappend = 0;  /* Append auxiliary files? */

  /* Image coordinate systems */
  struct wcs_coord otanxy, odefcoor;
  struct wcs_coord tanxy, defcoor;

  /* Pixel detection information */
  struct detect_struct detect;

  /* Saved image parameters */
  double oexposure, otstart, otstop, obatz;
  double ooversampx, ooversampy;
  int onbatdets;

  /* Source detection information */
  struct source_struct *sources = 0;
  int nsources = 0;

  /* Row selections */
  int maxrowranges = 512, numrowranges = 0;
  long int rowrangemin[512], rowrangemax[512];
  int nbkgimages = 0, nbkgvarimages = 0;
  int bkgindex = 1, bkgvarindex = 1;

  /* Zero the detection structure pointers */
  memset(&detect, 0, sizeof(detect));
  memset(&otanxy, 0, sizeof(otanxy));
  memset(&odefcoor, 0, sizeof(odefcoor));
  memset(&tanxy, 0, sizeof(tanxy));
  memset(&defcoor, 0, sizeof(defcoor));
  detect.nadjpix = parms->nadjpix;
  detect.nonullborder = parms->nonullborder;
  detect.currow = 1;

  banner(parms);

  /* Open the input image and get the basic layout of the images */
  fits_open_image(&imgfile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
    return status;
  }
  detect.nimages = image_nimages(imgfile, 0, &status);
  if ((detect.nimages == 0) || (status != 0)) {
    int safestatus = 0;
    fprintf(stderr, "ERROR: could not determine number of images in %s\n", parms->infile);
    fits_close_file(imgfile, &safestatus);
    
    return status;
  }

  /* -------------- */
  /* Parse the 'rows' string */
  headas_chat(5, "...parsing 'rows' (='%s')...\n", parms->rowstr);
  if (fits_parse_range(parms->rowstr, detect.nimages, maxrowranges,
		       &numrowranges, rowrangemin, rowrangemax, &status)) {
    fprintf(stderr, "ERROR: could not parse 'rows' parameter ('%s')\n",
	    parms->rowstr);
    return status;
  }
  headas_chat(5, "   (found %d ranges, starting at %ld, ending at %ld)\n",
	      numrowranges, rowrangemin[0], rowrangemax[numrowranges-1]);

  /* Count the number of good images using the row filtering expression */
  detect.ngoodimages = 0;
  for (i=1; i<=detect.nimages; i++) {
    /* Check to see if this image is in the requested row ranges */
    for (j=0; j<numrowranges; j++) {
      if ((i >= rowrangemin[j]) && (i <= rowrangemax[j])) break;
    }
    /* This image was not found */
    if (j == numrowranges) continue;
    detect.ngoodimages ++;
  }

  headas_chat(2, "    Found %d Images (and selected %d images)\n", 
	      detect.nimages, detect.ngoodimages);
  if (detect.ngoodimages == 0) {
    fprintf(stderr, "WARNING: no images were found or selected. Exiting.\n");
    return 0;
  }

  if (parms->inbkgmap[0]) {
    fits_open_image(&bkgfile, parms->inbkgmap, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->inbkgmap);
      return status;
    }
    nbkgimages = image_nimages(bkgfile, 0, &status);
    headas_chat(5, "  ... background file %s (%d images)\n",
		parms->inbkgmap, nbkgimages);
    if ((nbkgimages < detect.ngoodimages) || (status != 0)) {
      fprintf(stderr, "ERROR: could not determine number of images in %s\n", 
	      parms->inbkgmap);
      return status;
    }
  }

  if (parms->inbkgvarmap[0]) {
    fits_open_image(&bkgvarfile, parms->inbkgvarmap, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->inbkgvarmap);
      return status;
    }
    nbkgvarimages = image_nimages(bkgvarfile, 0, &status);
    headas_chat(5, "  ... background file %s (%d images)\n",
		parms->inbkgvarmap, nbkgvarimages);
    if ((nbkgvarimages < detect.ngoodimages) || (status != 0)) {
      fprintf(stderr, "ERROR: could not determine number of images in %s\n", 
	      parms->inbkgvarmap);
      return status;
    }
  }




  /* ===================== MAIN LOOP */
  /* Loop through each image in the input */
  detect.imgnum = 0;
  for (imgindex=1; imgindex<= detect.nimages; imgindex++) {
    FLOAT nullval = -1e38;
    int anynull = 0, anynull_bkgmap = 0, anynull_bkgvarmap = 0;

    /* Check to see if this image is in the requested row ranges */
    for (j=0; j<numrowranges; j++) {
      if ((imgindex >= rowrangemin[j]) && 
	  (imgindex <= rowrangemax[j])) break;
    }
    /* This image was not found */
    if (j == numrowranges) {
      headas_chat(2, "     Skipping Image: %d\n", imgindex);
      continue;
    }
    
    /* Read the image */
    if (detect.image) image_free(detect.image);
    detect.image = image_read_i(imgfile, imgindex, 0, 
				&nullval, &anynull, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not read image data from %s\n", 
	      parms->infile);
      return status;
    }

    if (! img_hduclass_keep(imgindex, imgfile, 
			    parms->hduclasses, parms->nhduclasses)) {
      /* Not keeping based on HDUCLAS* filtering, so go to next image */
      continue;
    }

    if (bkgfile) {
      detect.bkgmap = image_read_i(bkgfile, bkgindex++, 0, 
				   &nullval, &anynull_bkgmap, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not read image data from %s\n", 
		parms->inbkgmap);
	return status;
      }
    } else if (parms->inbkgmap_zero) {
      detect.bkgmap = image_init(detect.image->axes[0], detect.image->axes[1], 0);
    }

    if (bkgvarfile) {
      detect.bkgvarmap = image_read_i(bkgvarfile, bkgvarindex++, 0, 
				      &nullval, &anynull_bkgvarmap, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not read image data from %s\n", 
		parms->inbkgvarmap);
	return status;
      }
    }

    headas_chat(2, "    Analyzing Image: %d\n", imgindex);
    detect.imgnum ++;  /* Increment the output image counter */

    if ((status = read_distortion_map(imgfile, parms->distfile, 
				      &(detect.distmap)))) {
      return status;
    }

    /* Save the old values, because we want to compare the WCS
       keywords afterwards. */
    status = compare_img_systems(imgfile, detect.imgnum,
				 parms->outvector, parms->hduclas2,
				 &tanxy, &defcoor, 
				 &(parms->exposure), &(parms->tstart),
				 &(parms->tstop), &(parms->batz), 
				 &(parms->oversampx), &(parms->oversampy),
				 &(parms->nbatdets),
				 &otanxy, &odefcoor, 
				 &oexposure, &otstart, &otstop,
				 &obatz, &ooversampx, &ooversampy,
				 &onbatdets,
				 &(detect.wcs), detect.altwcs, &detect.nwcs,
				 &(detect.distmap), &(detect.dist_corr),
				 parms->distfile,
				 &(detect.has_cel_coords),
				 &(detect.has_tan_coords),
				 parms->ra_colname, parms->dec_colname);
    if (status) return status;

    /* Read any *APP keywords */
    headas_chat(5, "...reading *APP correction keywords...\n");
    parms->nappkeys = 0;
    if (parms->nkeepkeywords > 0 && parms->keepkeywords) {
      parms->nappkeys = read_app_keywords(imgfile, 
					  parms->keepkeywords,
					  parms->nkeepkeywords,
					  parms->appkeys,
					  MAX_APP_KEYS, &status);
    }
    if (parms->nappkeys < 0) {
      fprintf(stderr, "ERROR: while reading *APP keys from input\n");
      return status;
    }
    chat_app_keywords(parms->appkeys, parms->nappkeys);
    
    nx = detect.image->axes[0];
    ny = detect.image->axes[1];
    
    /* Read mask file, or make a default mask 
     * XXX - double-check the WCS keywords against the main image */
    if ((detect.pcode == 0) && (parms->pcodefile[0])) {
      fits_open_image(&pcodefile, parms->pcodefile, READONLY, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not open %s\n", parms->pcodefile);
	return status;
      }
      pcode = image_read(pcodefile, &status);
      fits_close_file(pcodefile, &status);
      if (status || (pcode == 0)) {
	fprintf(stderr, "ERROR: could not read image data from %s\n", 
		parms->pcodefile);
	return status;
      }
      if ((nx != pcode->axes[0]) || (ny != pcode->axes[1])) {
	fprintf(stderr, "ERROR: dimensions of image and partial coding map must match\n");
	headas_chat(5,  "       ... image dimensions (%d,%d) ...\n", nx,ny);
	headas_chat(5,  "       ... mask  dimensions (%d,%d) ...\n", 
		    pcode->axes[0], pcode->axes[1]);
	return BAD_DIMEN;
      }
      
      detect.pcode = pcode;
    }
	

    /* Make an image quality map */
    if ((detect.mask == 0) || (detect.bkgmask == 0)) {
      if (detect.mask) image_free(detect.mask);
      detect.mask = 0;
      if (detect.bkgmask) image_free(detect.bkgmask);
      detect.bkgmask = 0;
      
      if (detect.pcode) {       
	/* Truncate the partial coding map below the source threshold */
	mask = image_init(nx, ny, 0);
	foreach_pixel(pcode, i, 
	  {if (pcode->data[i] < parms->pcodethresh) mask->data[i] = 0; else mask->data[i] = 1;});

	/* ... and now the same for the background threshold */
	bkgmask = image_init(nx, ny, 0);
	foreach_pixel(pcode, i, 
	  {if (pcode->data[i] < parms->bkgpcodethresh) bkgmask->data[i] = 0; else bkgmask->data[i] = 1;});
      } else {
	mask = image_init(nx, ny, 0);  /* Mask for disabling source pixels */
	foreach_pixel(mask, i, {mask->data[i] = 1;});
	bkgmask = image_init(nx, ny, 0);  /* Mask for disabling background pixels */
	foreach_pixel(bkgmask, i, {bkgmask->data[i] = 1;});
      }

      detect.mask = mask;
      detect.bkgmask = bkgmask;

      /* Mask out any undefined pixels in the input image */
      if (anynull) {
	foreach_pixel(detect.image, i, 
         {if (detect.image->data[i] == nullval) { 
            detect.image->data[i] = 0; mask->data[i] = 0; bkgmask->data[i] = 0;} });
      }

      if (anynull_bkgmap) {
	foreach_pixel(detect.bkgmap, i, 
         {if (detect.bkgmap->data[i] == nullval) { 
            detect.image->data[i] = 0; mask->data[i] = 0; bkgmask->data[i] = 0;} });
      }

      if (anynull_bkgvarmap) {
	foreach_pixel(detect.bkgvarmap, i, 
         {if (detect.bkgvarmap->data[i] == nullval) { 
	    detect.image->data[i] = 0; mask->data[i] = 0; bkgmask->data[i] = 0;} });
      }



    }

    
    /* Check to see if there is an input catalog, and if so, copy it
       over to the output before proceeding.  Sources which are in the
       field of view should be assigned a source table entry, and pixels
       around the known sources are disabled for pixel detection. */
    
    detect.npixfound = 0;

    /* Initialize the output catalog, either for the first time, or by
       adding new rows for the later images. */
    if (detect.imgnum == 1) {
      /* Initial catalog creation */
      copycat(parms, &detect);
    } else if (parms->outvector == 0) {
      /* Copy the previous catalog into the new catalog 
         (NOTE: nsources is from previous image/iteration) */
      enlargecat(parms, &detect, nsources, parms->carryover);
      nsources = detect.nsrcfound;  /* may have changed */
    }
    
    /* Perform pixel detection algorithm */
    detect.npixfound = 0;
    if (parms->niter > 0) pixdetect(parms, &detect);
    
    /* Write significance map if requested (IMAGE detect.snrmap) */
    if (detect.snrmap) {
      if (parms->signifmap[0]) {
	/* Copy units of input image */
	strcpy(detect.snrmap->units, detect.image->units);
	imgout(parms, parms->signifmap, SIGNIFMAP, &ihdu_signifmap,
	       detect.snrmap, imgfile, auxappend, &status);
	if (status == 0) {
	  headas_chat(2, "  Significance map written to %s\n", parms->signifmap);
	}
	status = 0;
      }
      image_free(detect.snrmap); detect.snrmap = 0;
    }
    
    /* =================== */
    /* Detect sources */
    if (parms->srcdetect && parms->niter > 0) {
      flooddetect(&detect, parms->srcwindowrad);
    }
    sources = detect.sources;
    nsources = detect.nsrcfound;
    headas_chat(5,"  ...Flood detection algorithm found %d sources...\n",
		nsources);

    /* Now select a healthy number of pixels around all sources.  The
       newly detected sources, if they are near detection threshold
       may not have many selected pixels.  This operation puts all
       sources on equal footing. */
    if (parms->srcdetect && parms->niter > 0) {
      int npixfound = detect.npixfound;
      headas_chat(5,"  ...Masking all detected sources...\n");
      blanksources(parms, &detect);
      headas_chat(5,"     (found %d additional pixels)\n", 
		  detect.npixfound - npixfound);
    }

    /* Convert pixel coordinates to image coordinates */
    pix_to_img(sources, nsources, detect.wcs, detect.altwcs, &status);

    /* =================== */
    /* Perform PSF fitting */
    if (parms->srcfit) psffit(parms, &detect);

    /* Compute pixel value at center position */
    est_cent_flux(sources, nsources, detect.image, 
		  detect.bkgmap, detect.bkgvarmap, nullval);

    /* Do averaging of source position if we are fitting it in
       each input image */
    if (parms->outvector) {
      est_vect_pos(sources, nsources, parms->snrthresh, parms->vectpos,
		   detect.wcs, detect.altwcs);
      /* Note that pixel coords may be adjusted but .imx/y are unchanged.
	 This is OK since img_to_sky() below starts with pixel coords,
	 and recomputes .imx/y from scratch. */
    }

    /* Warn if snrthresh was set too low */
    warn_low_snr(sources, nsources);

    headas_chat(2, "\n");
    headas_chat(2, "    Detected %d sources\n", nsources);
    headas_chat(2, "\n");


    /* Convert to sky coordinates - native coords are PIXEL */
    img_to_sky(sources, nsources, 
	       &detect, detect.pcode, 
	       parms->possyserr, &status);

    /* Name any unnamed sources */
    parms->newsrcind = name_new_sources(sources, nsources, 
					parms->newsrcname, 
					parms->newsrcind);
    if (status) {
      fprintf(stderr, "ERROR: WCS coordinate conversion failed\n");
      return status;
    }

    
    /* Make output -- both FITS table ... */
    if ((status = catout(parms, &detect, sources, nsources, imgfile))) {
      return status;
    }
    /* ... and ASCII console printout */
    print_source_table(parms->sortcolumns, &detect, sources, nsources, &status);
    status = 0;

    /* Bookkeeping: free data (IMAGE detect.npix) */
    if (detect.npix) {
      image_free(detect.npix);
      detect.npix = 0;
    }
    image_free(detect.found); /* (IMAGE detect.found) */
    detect.found = 0;  
    
    /* Write background map (IMAGE detect.bkgmap) */
    if (detect.bkgmap) {
      if (parms->bkgmap[0]) {
	/* Copy units of input image */
	strcpy(detect.bkgmap->units, detect.image->units);
	imgout(parms, parms->bkgmap, BKGMAP, &ihdu_bkgmap,
	       detect.bkgmap, imgfile, auxappend, &status);
	if (status == 0) {
	  headas_chat(2, "  Background map written to %s\n", parms->bkgmap);
	}
	status = 0;
      }
      image_free(detect.bkgmap); 
      detect.bkgmap = 0;
    }
    
    /* Write background variation map (IMAGE detect.bkgvarmap) */
    if (detect.bkgvarmap) {
      if (parms->bkgvarmap[0]) {
	/* Copy units of input image */
	strcpy(detect.bkgvarmap->units, detect.image->units);
	imgout(parms, parms->bkgvarmap, STDDEVMAP, &ihdu_bkgvarmap,
	       detect.bkgvarmap, imgfile, auxappend, &status);
	if (status == 0) {
	  headas_chat(2, "  Background variance map written to %s\n", 
		      parms->bkgvarmap);
	}
	status = 0;
      }
      image_free(detect.bkgvarmap); 
      detect.bkgvarmap = 0;
    }

    /* Remove the mask array if it has been corrupted by disabled
       pixels for this particular image */
    if (detect.mask) {
      image_free(detect.mask);
      detect.mask = 0;
      image_free(detect.bkgmask);
      detect.bkgmask = 0;
    }

    if (detect.image) image_free(detect.image);
    detect.image = 0;


    /* Clear source detection errors associated with a given analysis
       run.  This is to prevent an earlier image from screwing up a
       later image analysis. But keep ERR_NULLBORDER because that is
       a geometric status that cannot change at later times, plus
       it can be set on input if the source is noticed to be touching
       the edge of the pcode field of view when the catalog is read.
    */
    for (i=0; i<nsources; i++) {
      if (sources[i].status < 0 &&
	  sources[i].status != ERR_NULLBORDER) {
	sources[i].status = 0;
      }
    }
    
    if (detect.wcs) wcsvfree(&(detect.nwcs), &(detect.wcs));
    detect.wcs = 0;
    detect.nwcs = 0;
    
    auxappend += 1;  /* Append any more auxiliary images */
  } /* End of loop through input images */
  
  if (detect.wcs) wcsvfree(&(detect.nwcs), &(detect.wcs));
  detect.wcs = 0;
  detect.nwcs = 0;

  /* Close input image */
  fits_close_file(imgfile, &status);
  imgfile = 0;
  status = 0;

  /* Close background files if open */
  if (bkgfile) fits_close_file(bkgfile, &status);
  bkgfile = 0;
  status = 0;
  if (bkgvarfile) fits_close_file(bkgvarfile, &status);
  bkgvarfile = 0;
  status = 0;

  /* Close output catalog */
  if (parms->outcat) {
    int mystatus = 0;
    /* Filter output catalog to keep only DETECT_STATUS == 0 */
    if (!parms->keep_bad_sources) {
      fits_set_hdustruc(parms->outcat, &status);
      fits_select_rows(parms->outcat, parms->outcat, 
		       "DETECT_STATUS == 0",
		       &status);
    }
    status = HDpar_stamp(parms->outcat, 0, &status);
    fits_close_file(parms->outcat, &mystatus);
    parms->outcat = 0;
  }

  /* Free other memory */
  if (detect.pcode) image_free(detect.pcode);
  detect.pcode = 0;
  if (detect.mask) image_free(detect.mask);
  detect.mask = 0;
  if (detect.bkgmask) image_free(detect.bkgmask);
  detect.bkgmask = 0;
  if (detect.sources) free(detect.sources);
  detect.sources = 0;
  if (detect.distmap) free(detect.distmap);
  detect.distmap = 0;

  summary(parms);
  return 0;
}

/* =================================================================== */
int batcelldetect(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batcelldetect_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batcelldetect_work(&parms);

}
