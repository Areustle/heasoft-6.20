#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"
#include "bat_gswdev.h"
#include "pil.h"
#include "headas.h"

/* $Id: bathotpix.c,v 1.25 2009/08/21 18:27:40 craigm Exp $ */

static char taskname[] = "bathotpix";
static char taskver[]  = "1.10";


/*
  Outline 
  - hot pixel clean
     o median
     o plus minus 98%
     o guard bands 20% + 10
     o compute average, excluding 0
     o If average exceeds 16, exclude 0, else include 0 
*/

#define TOOLSUB bathotpix
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

struct parm_struct 
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];    /* Input image file name */
  char outfile[PIL_PATH_MAX];   /* Output image mask */
  char detmask[PIL_PATH_MAX];   /* Mask to apply to input image */
  int applygaps;                /* Whether to zero BAT detector gaps */
  double keepfract;             /* Fraction of detectors to keep */
  double guardfract;            /* Fractional size of guard band,
                                   relative to median value */
  double guardval;              /* Additional additive guard band */
  double zerothresh;            /* Threshold above which zero cts is "cold" */
  int mergemask;                /* Whether to AND detmask with result */

  int goodval;                  /* Value to use for good pixels */
  int badval;                   /* Value to use for bad pixels */
  int hotval;                   /* Value to use for hot pixels */
  int coldval;                  /* Value to use for cold pixels */
  int row;                      /* Input image to process */
  int chatter;                  /* Chatter level 0=silent, 5=debug */
};

int bathotpix_getpar(struct parm_struct *parms) 
{
  int status = 0;

  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->detmask[0] = 0;
  parms->applygaps = 1;
  parms->keepfract = 0.98;
  parms->guardfract = 0.25;
  parms->guardval = 5;
  parms->zerothresh = 20;
  parms->mergemask = 1;
  parms->goodval = 0;
  parms->badval = 1;
  parms->hotval = 1;
  parms->coldval = 1;
  parms->row = -1;

  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("detmask", parms->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");

  else if ((status = PILGetInt("row", &parms->row)))
    fprintf(stderr, "Error reading the 'row' parameter.\n");

  else if ((status = PILGetBool("applygaps", &parms->applygaps)))
    fprintf(stderr, "Error reading the 'applygaps' parameter.\n");

  else if ((status = PILGetBool("mergemask", &parms->mergemask)))
    fprintf(stderr, "Error reading the 'mergemask' parameter.\n");

  else if ((status = PILGetReal("keepfract", &parms->keepfract)))
    fprintf(stderr, "Error reading the 'keepfract' parameter.\n");

  else if ((status = PILGetReal("guardfract", &parms->guardfract)))
    fprintf(stderr, "Error reading the 'guardfract' parameter.\n");

  else if ((status = PILGetReal("guardval", &parms->guardval)))
    fprintf(stderr, "Error reading the 'guardval' parameter.\n");

  else if ((status = PILGetReal("zerothresh", &parms->zerothresh)))
    fprintf(stderr, "Error reading the 'zerothresh' parameter.\n");

  else if ((status = PILGetInt("goodval", &parms->goodval)))
    fprintf(stderr, "Error reading the 'goodval' parameter.\n");
  else if ((status = PILGetInt("badval", &parms->badval)))
    fprintf(stderr, "Error reading the 'badval' parameter.\n");
  else if ((status = PILGetInt("hotval", &parms->hotval)))
    fprintf(stderr, "Error reading the 'hotval' parameter.\n");
  else if ((status = PILGetInt("coldval", &parms->coldval)))
    fprintf(stderr, "Error reading the 'coldval' parameter.\n");
  else if ((status = PILGetInt("chatter", &parms->chatter))) 
    fprintf(stderr, "Error reading the 'chatter' parameter.\n");

  headas_chat(5,"   (goodval=%d badval=%d hotval=%d coldval=%d)\n",
	      parms->goodval, parms->badval, parms->hotval, parms->coldval);
  if (status) return status;

  /* Default value of parms->detmask file name */
  if (strcasecmp(parms->detmask, "none") == 0) {
    parms->detmask[0] = 0;
  }

  if ((parms->row < 1) && (parms->row != -1)) {
    fprintf(stderr, "ERROR: row number must be >= 1 or -1 \n");
    return -1;
  }

  return status;
}

void banner(struct parm_struct *parms)
{
  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "     Input Image: %s\n", parms->infile);
  headas_chat(2, "          Number: %d\n", parms->row);
  headas_chat(2, "------------------------------------------\n");
}

void summary(struct parm_struct *parms)
{
  headas_chat(2, "------------------------------------------\n");
}  


int bathotpix_work(struct parm_struct *parms)
{
  struct image_struct *focal = 0;                  /* Focal plane image */
  struct image_struct *detmask = 0, *detmask0 = 0; /* Input mask */
  struct image_struct *outmask = 0;                /* Output mask */
  int nbins;       /* Number of bins in histogram */
  int *histo = 0;  /* Histogram */
  int i, j;
  int xcum1, xcumn;
  double minind, maxind, medind;
  double vminval, vmaxval, vmedval;
  int detidmap[173][286];
  unsigned short int detids[256];
  short int blocks[256], dms[256], dets[256], rows[256], cols[256];
  fitsfile *infile, *maskfile, *outfile;
  int status = 0, safestatus = 0;
  int clipped = 0;
  char bunit[FLEN_CARD] = {0};
  double exposure = 1.0;
  double tstart = 0.0;

  int bdetx[32768], bdety[32768];
  int bdetid[32768];
  int bquality[32768];
  int bcounts[32768];
  int nbad = 0;
  int ngoodpix, nhotpix, ncoldpix, ndispix;
  
  banner(parms);

  /* Make a detector ID lookup map */
  headas_chat(5, "...Making detid lookup table...\n");
  for (i=0; i<32768; i+=256) {
    for (j=0; j<256; j++) 
      detids[j] = i+j;
    batidconvert(256, detids, blocks, dms, dets, rows, cols);
    for (j=0; j<256; j++) 
      detidmap[rows[j]][cols[j]] = detids[j];
  }

  /* Read input image */
  headas_chat(5,"...Reading focal plane map...\n");
  fits_open_image(&infile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
    return status;
  }
  focal = image_read_i(infile, parms->row, 0, 0, 0, &status);
  
  /* All done reading image, still need some keywords */
  if (focal) {
    headas_chat(5, "    (%dx%d image)\n", focal->axes[0], focal->axes[1]);
  }
  if (status) {
    int mystatus = 0;
    fits_close_file(infile, &mystatus);
    fprintf(stderr, "ERROR: could not read image data from %s\n", 
	    parms->infile);
    return status;
  }

  fits_write_errmark();
  fits_read_key(infile, TSTRING, "BUNIT", bunit, 0, &status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    bunit[0] = 0;
    headas_chat(5,"...NOTE: BUNIT not found, assuming 'count' image...\n");
  }
  headas_chat(5,"   (BUNIT=%s)\n",bunit);

  fits_write_errmark();
  fits_read_key(infile, TDOUBLE, "EXPOSURE", &exposure, 0, &status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    exposure = 1.0;
    if (is_units_rate(bunit)) {
      fprintf(stderr, "WARNING: EXPOSURE not found in a rate image, assuming 1 second exposure\n");
    }
  }
  if (exposure <= 0) {
    fprintf(stderr, "WARNING: Nonsensical EXPOSURE (=%f), re-setting to 1 sec\n", exposure);
    exposure = 1.0;
  }
  headas_chat(5,"  (EXPOSURE=%f)\n", exposure);
    

  fits_write_errmark();
  fits_read_key(infile, TDOUBLE, "TSTART", &tstart, 0, &status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    tstart = 0;
  }
  headas_chat(5,"  (TSTART=%f)\n", tstart);


  /* Done reading all input stuff */
  fits_close_file(infile, &status);

  /* Convert from rate to counts */
  if (is_units_rate(bunit)) {
    headas_chat(5,"...Converting rate image to counts image...\n");
    foreach_pixel(focal, i, 
        {focal->data[i] *= exposure; });
  }


  /* Read detector mask file */
  if (parms->detmask[0] != 0) {
    headas_chat(5, "...reading detector mask image...\n");
    fits_open_image(&maskfile, parms->detmask, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->detmask);
      return status;
    }

    detmask0 = image_read(maskfile, &status);
    detmask  = image_read(maskfile, &status);

    /* Read GOODVAL keyword.  If the keyword is not found, then assume
       that 1=good, 0=bad.  If the keyword is found, then use that
       value for good, and anything else for bad. */
    safestatus = 0;
    fits_read_key(maskfile, TINT, "GOODVAL", &parms->goodval, 0, &safestatus);
    if (safestatus) parms->goodval = 0;

    fits_close_file(maskfile, &status);
    headas_chat(5, "    (%dx%d image)\n", detmask->axes[0], detmask->axes[1]);
    if (status) {
      fprintf(stderr, "ERROR: could not read detector mask data from %s\n", 
	      parms->detmask);
      return status;
    }

    headas_chat(5, "...checking detector mask image...\n");
    if (focal && ((detmask->axes[0] != focal->axes[0]) || 
		  (detmask->axes[0] != focal->axes[0])) ) {
      fprintf(stderr, 
	 "ERROR: Detector mask must have same dimensions as detector image\n");
      return BAD_DIMEN;
    }

    if (parms->goodval != 1) {
      /* Convert from 0 == good, else bad representation, to
	 1 == good, else bad representation */
      
      headas_chat(5, "...converting mask to 1=good, 0=bad...\n");
      foreach_pixel(detmask, i, 
        {detmask->data[i] = (detmask->data[i] == parms->goodval);});
    }

  } else {
    headas_chat(5, "...creating default detector mask image...\n");
    detmask0 = 0;
    detmask = image_init(focal->axes[0], focal->axes[1], 0);
    foreach_pixel(detmask, i, {detmask->data[i] = 1;});
  }

  /* Error checking on the good/bad/hot/cold pixel values */
  if (parms->goodval == 1) {
    if (parms->hotval != 0) {
      headas_chat(1,"NOTE: Since GOODVAL is 1, forcing HOTVAL to 0\n");
      parms->hotval = 0;
    }
    if (parms->coldval != 0) {
      headas_chat(1,"NOTE: Since GOODVAL is 1, forcing COLDVAL to 0\n");
      parms->coldval = 0;
    }
    if (parms->badval != 0) {
      headas_chat(1,"NOTE: Since GOODVAL is 1, forcing BADVAL to 0\n");
      parms->badval = 0;
    }
  }
  if ((parms->goodval == parms->hotval) || (parms->goodval == parms->coldval)){
    fprintf(stderr, "ERROR: GOODVAL cannot be equal to HOTVAL or COLDVAL\n");
    return -1;
  }


  /* Apply the detector gaps just to be sure */
  headas_chat(5, "...applying detector gaps to detector mask image...\n");
  if (parms->applygaps) image_mkdetgaps(detmask);

  /* Make new image for output mask */
  outmask = image_init(detmask->axes[0], detmask->axes[1], 0);
  if (outmask == 0) {
    fprintf(stderr, "ERROR: could not allocate output image\n");
    return (MEMORY_ALLOCATION);
  }

  /* Apply input mask to output mask */
  if (parms->mergemask) {
    if (detmask0) {
      foreach_pixel(outmask, i, {outmask->data[i] = detmask0->data[i];});
    } else {
      foreach_pixel(outmask, i, {outmask->data[i] = parms->goodval;});
    }

    /* ... and again cancel the detector gaps */
    headas_chat(5, "  ...filling gaps with badval = %d...\n", parms->badval);
    if (parms->applygaps) image_mkdetgaps_val(outmask, parms->badval);
  }

  /* Find maximum of the un-masked pixels */
  nbins = 0;
  foreach_pixel(focal, i,
    {if ((detmask->data[i] != 0) && (focal->data[i] > nbins)) 
      nbins = focal->data[i];});

  if (nbins == 0) {
    fprintf(stderr, "WARNING: image contains all zeroes\n");
    goto GRACEFUL_FINISH;
  }

  if (nbins > 1024*1024) {
    nbins = 1024*1024;
    clipped = 1;
  }

  /* With fewer than 5 counts in any one pixel, there are no
     discernable hot pixels.  */
  if (nbins < 5) { 
    headas_chat(1, "NOTE: Dynamic range is small - accepting all pixels\n");
    goto GRACEFUL_FINISH;
  }

  /* Allocate and zero the histogram */
  histo = (int *) malloc(nbins*sizeof(int));
  if (histo == 0) {
    fprintf(stderr, "ERROR: could not allocate memory for histogram\n");
    return MEMORY_ALLOCATION;
  }
  for (i=0; i<nbins; i++) histo[i] = 0;

  /* Populate histogram */  
  foreach_pixel(focal, i, 
    {if ((detmask->data[i] != 0) && 
	 (focal->data[i] >= 0) && (focal->data[i] < nbins))
      histo[(int)focal->data[i]] ++;});

  /* Form cumulative sum, ignoring the zero-bin */
  if (nbins > 2)
    for (i=2; i<nbins; i++) histo[i] += histo[i-1];

  /* Result is:
     histo[0] is number of zeroes
     histo[i], i>0, is cumulative histogram, number of pixels w/ value >= i
  */

  xcum1 = 0;
  xcumn = histo[nbins-1];

  /* Find X percent region in histogram, then add guard bands.  These
     are the desired numbers of cumulative detectors... */
  minind = xcum1 + (xcumn-xcum1)*(1.0-parms->keepfract)/2;  /* low */
  maxind = xcumn - (xcumn-xcum1)*(1.0-parms->keepfract)/2;  /* high */
  medind = xcum1 + (xcumn-xcum1)*0.5;                       /* median */
  headas_chat(3, "               X range: %d - %d\n",xcum1, xcumn);
  headas_chat(3, "               Y range: %f - %f\n",minind, maxind);
  headas_chat(3, "              Y median: %f\n", medind);

  /* ... which we now need to search through the histogram to find */
  for (i=1; i<nbins-1; i++) 
    if (histo[i] >= minind) break;  /* First the low end... */
  vminval = i;

  for (i=nbins-1; i>=0; i--)
    if (histo[i] < maxind) break;   /* ...then the high end */
  vmaxval = i;

  for (i=1; i<nbins-1; i++) 
    if (histo[i] >= medind) break;  /* and finally the median */
  vmedval = i;


  /* Trap error conditions where the bounds cross each other.  In that
     case I think that there is no harm in taking the full range. */
  if ((vminval == nbins) || (vmaxval < 0) || (vminval >= vmaxval)) {
    vminval = 1;
    vmaxval = nbins-1;
  }

  if ((vmedval < vminval) || (vmedval > vmaxval)) {
    vmedval = 0.5*(vminval + vmaxval);
  }

  headas_chat(2, "   Image maximum value: %d %s\n", nbins,
	      clipped ? "(clipped)" : "");
  headas_chat(2, "      Number of zeroes: %d\n", histo[0]);
  headas_chat(2, "        Selected range: %f - %f (without guard)\n",
	      vminval, vmaxval);


  /* Add guard bands */
  vminval = vminval - parms->guardfract*vminval - parms->guardval;
  vmaxval = vmaxval + parms->guardfract*vmaxval + parms->guardval;
  if (vminval < 1) vminval = 1;  /* Exclude zero */
  /* Reset lower threshold to zero if the total counts is low enough */
  if (vmedval < parms->zerothresh) vminval = 0;

  headas_chat(2, "        Selected range: %f - %f (with guard)\n",
	      vminval, vmaxval);
  headas_chat(2, "    Approximate median: %f\n", vmedval);

  /* Scan the image for out-of-bounds values, mark as zero */
  headas_chat(2, "\n   Report of HOT/COLD pixels (note row & col start at 0)\n");
  headas_chat(2, "   -------------------------\n");
  headas_chat(2, "%8s  %5s  H/C %12s    %5s  %2s %2s %2s %3s\n",
	      "Col.", "Row", "Pix Value", "DETID", "Bl", "DM", "S", "Det");

  /* Initialize statistics counters */
  nhotpix = ncoldpix = ndispix = ngoodpix = 0;

  for(i=0; i<(outmask->axes[0]*outmask->axes[1]); i++) {

    if (detmask->data[i] == 0) {
      ndispix ++;
    }

    /* Check for "good" detector which is out of bounds */
    /* Note: "good" detmask value is 1 here */
    if (detmask->data[i] != 0) {
      /* Enabled detector */

      if ((focal->data[i] < vminval) || (focal->data[i] > vmaxval)) {
	/* Enabled and out of bounds */
	int lo = (focal->data[i]<vminval);  /* lo == 1  if pixel is cold */
	int row = i/focal->axes[0], col = i%focal->axes[0];
	outmask->data[i] = lo?(parms->coldval):(parms->hotval);
	
	if (lo) {
	  ncoldpix ++;
	} else {
	  nhotpix  ++;
	}
	
	if ((focal->axes[0] == 286) && (focal->axes[1] == 173)) {
	  int block, dm, side, det, detid;
	  detid = detidmap[row][col];
	  det = detid % 128;
	  side = (detid/128) % 2;
	  dm = (detid/256) % 8;
	  block = detid/2048;
	  headas_chat(lo?3:2, "%8d  %5d   %c  %12.1f    %5d  %2d %2d %2d %3d\n", 
		      col, row, lo?'C':'H', focal->data[i],
		      detid, block, dm, side, det);
	  
	  /* Store accounting data */
	  bdetid[nbad] = detid;
	  bdetx[nbad] = col;
	  bdety[nbad] = row;
	  bquality[nbad] = lo?1:2;        /* COLD == 1; HOT == 2 */
	  bcounts[nbad] = focal->data[i];
	  nbad ++;
	  
	} else {
	  headas_chat(lo?3:2, "%8d  %5d   %c  %12.1f\n", col,
		      row, lo?'C':'H', focal->data[i]);
	}
      } else {

	/* Enabled and in-bounds */
	ngoodpix ++;
      }
	     
    }
  }
  if (parms->chatter <= 2) {
    headas_chat(2, "     Note: Cold pixels do not show up at chatter<=2\n");
  }

 GRACEFUL_FINISH:
  /* Write output image */
  headas_chat(5, "...writing output image...\n");
  headas_clobberfile(parms->outfile);
  image_append(&outfile, parms->outfile, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s for output\n",
	    parms->outfile);
    return status;
  }
  outmask->nullval = -1e37;
  image_write(outfile, outmask, &status);
  fits_set_hdustruc(outfile, &status);
  fits_open_image(&infile, parms->infile, READONLY, &status);
  image_copykeys(outfile, infile, &status);
  image_writeinfo(outfile, outmask, &status);  /* Write time keywords */
  fits_update_key(outfile, TINT, "GOODVAL", &parms->goodval, 0, &status);
  fits_update_key(outfile, TINT, "NGOODPIX", &ngoodpix, " Number of in-bounds pixels", &status);
  fits_update_key(outfile, TINT, "NHOTPIX", &nhotpix, " Number of hot pixels", &status);
  fits_update_key(outfile, TINT, "NCOLDPIX", &ncoldpix, " Number of cold pixels", &status);
  fits_update_key(outfile, TINT, "NDISPIX", &ndispix, " Number of disabled pixels", &status);

  status = HDpar_stamp(outfile, 0, &status);
  fits_set_hdustruc(outfile, &status);

  /* Create bad detector list */
  if ((status == 0) && (nbad > 0)) { 
    char *ttype[] = {"TIME", "EXPOSURE", "DET_ID", "DETX", "DETY", "QUALITY", "COUNTS"};
    char *tform[] = {"1D",   "1D",       "1J",     "1I",   "1I",   "1I",      "1J"};
    char *tunit[] = {"s",    "s",        "",       "pixel","pixel","",        "count"};
    char *comments[] = {"Time of flagged data",
			"Exposure of flagged data",
			"Flagged detector ID",
			"Detector X position",
			"Detector Y position",
			"Quality indicator code",
			"Counts in flagged detector"};
    int ncols = sizeof(ttype)/sizeof(ttype[0]);
    char ttypen[FLEN_CARD];
    
    fits_create_tbl(outfile, BINARY_TBL, nbad, ncols, 
		    ttype, tform, tunit, "BADPIX", &status);
    for (i=0; i<ncols; i++) {
      if (comments && comments[i] && comments[i][0]) {
	fits_make_keyn("TTYPE", i+1, ttypen, &status);
	fits_modify_comment(outfile, ttypen, comments[i], &status);
      }
    }
    fits_write_comment(outfile, "  ========================================================", &status);
    fits_update_key(outfile, TSTRING, "CONTENT", "BAT derived bad pixel list", 0, &status);
    fits_write_comment(outfile, "  QUALITY == 1 Indicates a cold or undersensitive detector", &status);
    fits_write_comment(outfile, "  QUALITY == 2 Indicates a hot detector", &status);
    fits_write_comment(outfile, "  QUALITY == 3 Indicates a disabled detector", &status);
    fits_write_comment(outfile, "  ========================================================", &status);
    image_copykeys(outfile,infile,&status);
    fits_update_key(outfile, TINT, "NGOODPIX", &ngoodpix, " Number of in-bounds pixels", &status);
    fits_update_key(outfile, TINT, "NHOTPIX", &nhotpix, " Number of hot pixels", &status);
    fits_update_key(outfile, TINT, "NCOLDPIX", &ncoldpix, " Number of cold pixels", &status);
    fits_update_key(outfile, TINT, "NDISPIX", &ndispix, " Number of disabled pixels", &status);
    fits_set_hdustruc(outfile, &status);

    if (status) {
      fprintf(stderr, "ERROR: could not create BADPIX extension\n");
    } else {
      for (i=1; i<=nbad; i++) {
	fits_write_col(outfile, TDOUBLE, 1, i, 1, 1, &tstart,   &status);
	fits_write_col(outfile, TDOUBLE, 2, i, 1, 1, &exposure, &status);
      }
      fits_write_col(outfile, TINT, 3, 1, 1, nbad, bdetid,   &status);
      fits_write_col(outfile, TINT, 4, 1, 1, nbad, bdetx,    &status);
      fits_write_col(outfile, TINT, 5, 1, 1, nbad, bdety,    &status);
      fits_write_col(outfile, TINT, 6, 1, 1, nbad, bquality, &status);
      fits_write_col(outfile, TINT, 7, 1, 1, nbad, bcounts,  &status);
    }
  }


  fits_close_file(infile, &status);
  fits_close_file(outfile, &status);
  if (status == 0)
    headas_chat(1, "  Mask image written to %s\n", parms->outfile);

  /* Free data */
  image_free(detmask);
  image_free(outmask);
  image_free(focal);
  if (detmask0) image_free(detmask0);
  if (histo) free(histo);

  summary(parms);
  return 0;
}

int bathotpix(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = bathotpix_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return bathotpix_work(&parms);

}
