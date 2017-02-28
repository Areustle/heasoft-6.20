#include <string.h>
#include <fitsio.h>
#include <pil.h>
#include <math.h>
#include "headas.h"
#include "headas_utils.h"
#include "imageutils.h"
#include "batcelldetect.h"
#include "bat_gswdev.h"

/* 
 * misc - miscellaneous functions for batcelldetect 
 *
 */

/* 
 * est_cent_flux - estimate central rate/counts and snr
 * 
 * sources, nsources - source catalog 
 *   input fields: .xsum, .ysum
 *   output fields: .centflux, .centsnr
 * detect_image - flux map
 * detect_bkgvarmap - flux noise map
 * 
 * RETURNS: 0
 */
int est_cent_flux(struct source_struct *sources,
		  int nsources,
		  struct image_struct *detect_image,
		  struct image_struct *detect_bkgmap, 
		  struct image_struct *detect_bkgvarmap, 
		  FLOAT nullval)
{
  int i;

  /* Compute pixel value at center position */
  for (i=0; i<nsources; i++) {
    int ixpix, iypix;
    sources[i].centflux = nullval;
    sources[i].centsnr  = nullval;
    
    /* xsum/ysum are FITS 1-based indices */
    /* ixpix/iypix are C 0-based indices */
    ixpix = rint(sources[i].xsum - 1);
    iypix = rint(sources[i].ysum - 1);
    if ((ixpix >= 0) && (ixpix < detect_image->axes[0]) &&
	(iypix >= 0) && (iypix < detect_image->axes[1]) &&
	detect_bkgvarmap &&
	detect_bkgvarmap->datap[iypix][ixpix] > 0) {

      /* Background-subtracted central pixel rate, and signal to noise */
      sources[i].centflux = (detect_image->datap[iypix][ixpix] -
			     sources[i].bkgflux);
      sources[i].centsnr  = (sources[i].centflux / 
			     detect_bkgvarmap->datap[iypix][ixpix]);
    }
  }

  return 0;
}


/* 
 * est_vect_pos - when outvect==T, estimate position of fitted sources
 * 
 * sources, nsources - source catalog 
 *   input fields: .xsum, .ysum, .constraintflags
 *   output fields: .wt_sum, .wt_sum_{x,y}, .max_snr, .max_snr_{x,y}
 *                  .first_{x,y} 
 *                  .xsum, .ysum - possibly overwritten by new position
 *                  .constraintflags  (add METH_MULT)
 * parms_snrthresh - signal to noise threshold
 * parms_vectpos - user-choice of how position should be calculated
 * wcs, altwcs - WCS coordinate parameters
 * 
 * IMPORTANT: xsum/ysum and imx/imy may be out of sync after this call
 *   --> xsum/ysum have the more definitive values.
 *
 * RETURNS: 0
 */
int est_vect_pos(struct source_struct *sources,
		 int nsources,
		 int parms_snrthresh,
		 int parms_vectpos,
		 struct wcsprm *wcs, int altwcs[27])

{
  int i;
  double snr, wti;

  for (i=0; i<nsources; i++) {
    snr = sources[i].snr;
    
    /* If this is a source where we fitted position ... */
    if (((sources[i].constraintflags & FIX_IMX) == 0 ||
	 (sources[i].constraintflags & FIX_IMY) == 0)) {

      headas_chat(5, 
	  "  ... vect src #%d: (XPIX,YPIX) = (%f,%f) snr=%f, status=%d\n",
		  i,
		  sources[i].xsum, sources[i].ysum, 
		  sources[i].snr,
		  sources[i].status);
      /* ... and it was significant */
      if (snr > parms_snrthresh) {

	wti = snr * snr;

	/* Save a copy of the status */
	if (sources[i].status >= 0 && sources[i].old_status <= 0) {
	  sources[i].old_status = sources[i].status;
	}

	sources[i].wt_sum   += wti;
	sources[i].wt_sum_x += sources[i].xsum * wti;
	sources[i].wt_sum_y += sources[i].ysum * wti;
	
	if (snr > sources[i].max_snr) {
	  sources[i].max_snr     = snr;
	  sources[i].max_snr_x = sources[i].xsum;
	  sources[i].max_snr_y = sources[i].ysum;
	}
	
	if (sources[i].first_x == NULL_POS) {
	  sources[i].first_x = sources[i].xsum;
	  sources[i].first_y = sources[i].ysum;
	}
      }

      /* Recover some sanity for the .status field, so that
	 a source is not deleted at the output stage.
       */
      if (snr < parms_snrthresh && 
	  sources[i].old_status >= 0 &&
	  sources[i].status < 0) {
	headas_chat(5,"     (status %d -> %d)\n", 
		    sources[i].status, sources[i].old_status);
	sources[i].status = sources[i].old_status;
      }
      
      /* Now store the output position we will report in the catalog */
      switch (parms_vectpos) {
      case POS_FIXFIRST:
	/* Fix position on subsequent fits */
	sources[i].constraintflags |= (FIX_IMX + FIX_IMY);
	/* ... fall through ... */
      case POS_FIRST:
	/* Use first measured position */
	sources[i].xsum = sources[i].first_x;
	sources[i].xsum = sources[i].first_y;
	sources[i].method |= METH_MULT;
	headas_chat(5,"     (POS_FIRST: (XPIX,YPIX) = (%f,%f) status=%d)\n",
		    sources[i].xsum, sources[i].ysum, sources[i].status);
	break;
	
      case POS_LAST:
	/* Use last measured position */
	/* do nothing, most recent position already stored */
	headas_chat(5,"     (POS_LAST: (XPIX,YPIX) = (%f,%f) status=%d)\n",
		    sources[i].xsum, sources[i].ysum, sources[i].status);
	break;
	
      case POS_AVG:
	/* Use SNR-weighted average position */
	if (sources[i].wt_sum > 0) {
	  sources[i].xsum = sources[i].wt_sum_x / sources[i].wt_sum;
	  sources[i].ysum = sources[i].wt_sum_y / sources[i].wt_sum;
	  sources[i].method |= METH_MULT;
	  headas_chat(5,"     (POS_AVG: (XPIX,YPIX) = (%f,%f) status=%d)\n",
		      sources[i].xsum, sources[i].ysum, sources[i].status);
	}
	break;
	
      case POS_MAX_SNR: 
	/* Use position for maximum SNR */
	if (sources[i].max_snr > 0) {
	  sources[i].xsum = sources[i].max_snr_x;
	  sources[i].ysum = sources[i].max_snr_y;
	  sources[i].method |= METH_MULT;
	  headas_chat(5,"     (POS_MAX_SNR: (XPIX,YPIX) = (%f,%f) status=%d)\n",
		      sources[i].xsum, sources[i].ysum, sources[i].status);
	}
	break;
      }
    }
    
  } /* loop over sources */
  
  return 0;
}

/* 
 * pix_to_img - compute image coordinates from pixel coordinates 
 * 
 * sources, nsources - source catalog 
 *   input fields: .xsum, .ysum
 *   output fields: .imx, .imy, .wimx, .wimy
 * wcs, altwcs - WCS coordinate parameters
 * *status - pointer to status variable
 * 
 * RETURNS: 0
 */
int pix_to_img(struct source_struct *sources,
	       int nsources,
	       struct wcsprm *wcs, int altwcs[27],
	       int *status)
{
  int i;

  for (i=0; i<nsources; i++) {
    if (sources[i].status == 0 || sources[i].status == ERR_NULLBORDER) {
      
      coco(wcs, altwcs, '.', '+', 
	   sources[i].xsum, sources[i].ysum,
	   &(sources[i].imx), &(sources[i].imy), status);

      /* Compute the width in tangent plane coordinates */
      coco(wcs, altwcs, '.', '+', 
	   sources[i].xsum+sources[i].xsum2, 
	   sources[i].ysum+sources[i].ysum2,
	   &(sources[i].wimx), &(sources[i].wimy), status);

      sources[i].wimx = fabs(sources[i].wimx-sources[i].imx);
      sources[i].wimy = fabs(sources[i].wimy-sources[i].imy);
    }
    headas_chat(5,"    %d  (xpix,ypix)=(%f,%f)  (imx,imy)=(%f,%f) status=%d\n",
		i, 
		sources[i].xsum, sources[i].ysum, 
		sources[i].imx, sources[i].imy, 
		sources[i].status);
  }

  return 0;
}

/* 
 * img_to_sky() - convert pixel coordinates to image/sky, also partial coding
 * 
 * struct source_struct *sources - array of detected sources
 *   input fields: .xsum, .ysum - pixel coordinates
 *   output fields: .imx, .imy  - image coordinates
 *                  .imx_corr, .imy_corr - corrected image coords
 *                  .ra_corr, .dec_corr - corrected sky coords
 * int nsources - size of sources array
 * struct image_struct *pcode - pointer to partial coding map, or 0 for none
 * int *status - CFITSIO status code
 *
 * RETURNS: CFITSIO status code
 *
 */
int img_to_sky(struct source_struct *sources, int nsources, 
	       struct detect_struct *detect,
	       struct image_struct *pcode,
	       double possyserr, int *status)
{
  /* Image coordinate systems */
  int i;

  double poserr;
  double imx, imy, imr;
  double dx = 1e-6;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;

  /* Convert to sky coordinates */
  for (i=0; i<nsources; i++) {

    /* Convert from pixel to tangent plane */
    coco(detect->wcs, detect->altwcs, '.', '+', 
	 sources[i].xsum, sources[i].ysum,
	 &(sources[i].imx), &(sources[i].imy), status);

    /* Correct from "apparent" tangent plane to "true" tangent plane */
    if (detect->distmap && detect->dist_corr) {
      distortmap_coco1(detect->distmap, 
		       sources[i].imx, sources[i].imy,
		       &sources[i].imx_corr, &sources[i].imy_corr,
		       APP_TO_TRUE);
    } else {
      sources[i].imx_corr = sources[i].imx;
      sources[i].imy_corr = sources[i].imy;
    }

    if (detect->has_cel_coords) {
      double ra1, dec1;

      /* Convert from corrected tangent plane coords to corrected sky */
      coco(detect->wcs, detect->altwcs, '+', '@', 
	   sources[i].imx_corr, sources[i].imy_corr,
	   &(sources[i].ra_corr), &(sources[i].dec_corr), status);

      while (sources[i].ra_corr < 0)    { sources[i].ra_corr += 360; }
      while (sources[i].ra_corr >= 360) { sources[i].ra_corr -= 360; }

      /* Compute small offset in IMX to make transformation matrix */
      coco(detect->wcs, detect->altwcs, '+', '@',
	   sources[i].imx_corr+dx, sources[i].imy_corr, 
	   &ra1, &dec1, status);

      sources[i].ra_per_imx  = (ra1  - sources[i].ra_corr ) / dx;
      sources[i].dec_per_imx = (dec1 - sources[i].dec_corr) / dx;

      /* ... and same for the IMY step */
      coco(detect->wcs, detect->altwcs, '+', '@',
	   sources[i].imx_corr, sources[i].imy_corr+dx, 
	   &ra1, &dec1, status);

      while (ra1 < 0)    { ra1 += 360.0; }
      while (ra1 >= 360) { ra1 -= 360.0; }
      sources[i].ra_per_imy  = (ra1  - sources[i].ra_corr ) / dx;
      sources[i].dec_per_imy = (dec1 - sources[i].dec_corr) / dx;

      /* Position error */
      poserr = sources[i].imx_err;
      if (sources[i].imy_err > poserr) poserr = sources[i].imy_err;
      poserr = sqrt(poserr*poserr + possyserr*possyserr);

      sources[i].ra_err  = atan(poserr) / DTOR * cos(sources[i].dec_corr*DTOR);
      sources[i].dec_err = atan(poserr) / DTOR;
      sources[i].err_rad = atan(poserr) / DTOR;
    }
    
    imx = sources[i].imx_corr; imy = sources[i].imy_corr;
    imr = sqrt(imx*imx + imy*imy);
    sources[i].theta = atan(imr) / DTOR;
    sources[i].phi   = atan2(-imy, imx) / DTOR;
    sources[i].grmclon = atan(-imy) / DTOR;
    sources[i].grmclat = atan(imx*cos(sources[i].grmclon*DTOR)) / DTOR;
    sources[i].pcode   = 0.0;
    
    /* Pull out the partial coding value at this position */
    if (pcode) {
      int ixpix, iypix;
      /* Convert FITS 1-based indices to C 0-based indices */
      ixpix = rint(sources[i].xsum-1); iypix = rint(sources[i].ysum-1);
      
      if ((ixpix >= 0) && (ixpix < pcode->axes[0]) &&
	  (iypix >= 0) && (iypix < pcode->axes[1])) {
	sources[i].pcode = pcode->datap[iypix][ixpix];
      }
    }
    
    headas_chat(5,"XPIX=%f YPIX=%f   IMX=%f IMY=%f   RA=%f DEC=%f PRECAT=%d PCODE=%f STATUS=%d\n",
		sources[i].xsum, sources[i].ysum,
		sources[i].imx_corr, sources[i].imy_corr,
		sources[i].ra_corr, sources[i].dec_corr, sources[i].precat,
		sources[i].pcode, sources[i].status);
  }

  return *status;
}


/* 
 * name_new_sources - construct standard name for newly detected sources
 * 
 * sources, nsources - source catalog 
 *   input fields: .name, .ra_corr, .dec_corr
 *   output fields: .name
 * parms_newsrcname - sprintf-style format string for output name
 * parms_newsrcind - starting source index number
 *
 * RETURNS: adjusted starting source index number 
 *
 */
int name_new_sources(struct source_struct *sources,
		     int nsources,
		     char *parms_newsrcname,
		     int parms_newsrcind)
{
  int i;
  char name[1024];

  for (i=0; i<nsources; i++) {
    if (sources[i].name[0] == 0) {
      if (parms_newsrcind >= 0) {
	/* Integer index format */
	sprintf(name, parms_newsrcname, parms_newsrcind);
	parms_newsrcind ++;
      } else {
	sprintf(name, parms_newsrcname, 
		sources[i].ra_corr, sources[i].dec_corr);
      }
      
      strncpy(sources[i].name, name, SRCNAME_LEN-1);
      sources[i].name[SRCNAME_LEN-1] = 0;
    }
  }

  return parms_newsrcind;
}


/* =================================================================== */
/* Read any keywords containing *APP (corrections) */
int read_app_keywords(fitsfile *file, 
		      char **inclist, int nlist,
		      struct keyword_struct *keys, 
		      int nmax, int *status)
{
  char card[FLEN_CARD];
  int ncards = 0;
  int i, nfound = 0;
  int keylength = 0;
  char value[FLEN_CARD];

  if (status == 0) return -1;
  if (*status) return -1;
  if ((file == 0) || (keys == 0)) {
    *status = NULL_INPUT_PTR;
    return -1;
  }

  /* Reset header pointer to beginning of file */
  if (fits_get_hdrspace(file, &ncards, 0, status)) return -1;
  if (ncards <= 0) return 0;
  if (fits_read_record(file, 0, card, status)) return -1;

  while (1) {
    keylength = 0;
    if (fits_find_nextkey(file, inclist, nlist, 0, 0, card, status)) break;
    if (fits_get_keyname(card, keys[nfound].name, &keylength, status)) break;
    keys[nfound].name[keylength] = 0; /* Null terminate */
    nfound ++;
    if (nfound >= nmax) break;
  }

  if (*status == KEY_NO_EXIST) *status = 0;
  if (*status) return -1;

  for (i=0; i<nfound; i++) {
    /* Initialize the keyword info (but not .name) */
    keys[i].dtype = ' ';
    keys[i].comment[0] = 0;
    memset(&(keys[i].val), 0, sizeof(keys[i].val));

    fits_read_keyword(file, keys[i].name, value, 0, status);
    fits_get_keytype(value, &(keys[i].dtype), status);

    if (*status == 0) {
      switch (keys[i].dtype) {
      case 'C': 
	fits_read_key(file, TSTRING, keys[i].name, keys[i].val.str,
		      keys[i].comment, status); 
	break;
      case 'L': 
	fits_read_key(file, TLOGICAL, keys[i].name, &(keys[i].val.bool), 
		      keys[i].comment, status);
	break;
      case 'I': 
	fits_read_key(file, TLONG, keys[i].name, &(keys[i].val.intv), 
		      keys[i].comment, status);
	break;
      case 'F': 
	fits_read_key(file, TDOUBLE, keys[i].name, &(keys[i].val.floatv), 
		      keys[i].comment, status);
	break;
      default:
	keys[i].dtype = ' ';
	break;
      }
    } else {
      keys[i].dtype = ' ';
    }
  }

  if (*status) return -1;
  return nfound;
}

/*
 * chat_app_keywords - print out appkeys for debugging
 * 
 * appkeys - array of application-defined keywords
 * nappkeys - number of elements in appkeys
 *
 * RETURNS: 0
 */
int chat_app_keywords(struct keyword_struct *appkeys,
		      int nappkeys)
{
  int i;

  headas_chat(5, "  (found %d *APP keywords)\n", nappkeys);
  for (i=0; i<nappkeys; i++) {
    switch(appkeys[i].dtype) {
    case 'C': 
      headas_chat(5, "  (%s=%s)\n", appkeys[i].name, 
		  appkeys[i].val.str);
      break;
    case 'L': 
      headas_chat(5, "  (%s=%d)\n", appkeys[i].name, 
		  appkeys[i].val.bool);
      break;
    case 'I': 
      headas_chat(5, "  (%s=%ld)\n", appkeys[i].name, 
		  appkeys[i].val.intv);
      break;
    case 'F': 
      headas_chat(5, "  (%s=%f)\n", appkeys[i].name, 
		  appkeys[i].val.floatv);
      break;
    }
  }
  
  return 0;
}

/*
 * img_hduclass_filter - decided whether to accept this HDU based on HDUCLAS* keywords
 * 
 * imgfile - input image file, cued to the desired HDU
 * hduclasses - list of HDUCLAS* values that are acceptable
 * nhduclasses - number of elements in hduclasses list
 *
 * RETURNS: 0 - reject this HDU
 *          1 - accept this HDU
 */
int img_hduclass_keep(int imgindex, fitsfile *imgfile,
		      char **hduclasses, int nhduclasses)
{
  char *hduclasskeys[] = {"HDUCLASS","HDUCLAS1","HDUCLAS2","HDUCLAS3"};
  int nkeys = 4;
  int pass, fail, mustpass;
  char strvalue[FLEN_CARD];
  int j, k;

  /* If no HDUCLAS keywords were specified, then always keep */
  if (nhduclasses == 0) return 1;  

  /* Perform filtering on the HDUCLASS keywords */
  mustpass = 0; pass = 0; fail = 0;
  for (k=0; k<nkeys; k++) {
    int safestatus = 0;

    fits_write_errmark();
    fits_read_key(imgfile, TSTRING, hduclasskeys[k], strvalue, 0, &safestatus);
    fits_clear_errmark();
    if (safestatus == 0) {
      headas_chat(5, "   * %s='%s'\n", hduclasskeys[k], strvalue);
    }

    for(j=0; j<nhduclasses; j++) {
	  
      if (hduclasses[j][0] == '-') {
	if (strcasecmp(strvalue,hduclasses[j]+1) == 0) {
	  /* Exclusionary filter */
	  headas_chat(5,"      (filter %s - failed)\n",hduclasses[j]);
	  fail = 1;
	  break;
	} else {
	  headas_chat(5,"      (filter %s - passed)\n",hduclasses[j]);
	}
      } else {
	/* Inclusionary filter (must pass) */
	mustpass = 1;
	if (strcasecmp(strvalue,hduclasses[j]) == 0) {
	  pass = 1;
	  headas_chat(5,"      (filter %s - passed)\n",hduclasses[j]);
	} else {
	  headas_chat(5,"      (filter %s - failed)\n",hduclasses[j]);
	}
      }
    } /* for j */
    if (fail) break;
  }

  if (fail || (pass == 0 && mustpass)) {
    if (fail && (k < nkeys)) {
      headas_chat(2, "     Skipping Image: %d (user requested to skip %s='%s')\n", 
		  imgindex, hduclasskeys[k], strvalue);
    } else {
      headas_chat(2, "     Skipping Image: %d (missing required HDUCLASn keyword(s)\n",
		  imgindex);
    }
    return 0; /* Skip this image */
  }

  /* Default pass */
  return 1;

}
			

/*
 * read_distortion_map - read BAT distortion map, optionally referring to CALDB
 * 
 * imgfile - open image extension used for CALDB date query
 * parms_distfile - name of distortion map file name, or "CALDB"
 *    if "CALDB", then upon return, parms_distfile will be re-written with
 *    CALDB query result.
 * distmap - upon return, *distmap has pointer to new distortion map
 * 
 * RETURNS: status value (0=OK)
 */
int read_distortion_map(fitsfile *imgfile,
			char *parms_distfile,
			struct distortmap_struct **detect_distmap)
{			
  int status = 0;

  /* Read the distortion map if requested - check for CALDB */
  if (strcasecmp(parms_distfile, "CALDB") == 0) {
    struct caldbparms_struct caldb;
    char *expr = "-";
    char *codenam = "DET_POSCOR";
    char *pfile = parms_distfile;  /* Overwrite parms->distfile */
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;
      
    batkw_to_caldb_parms(imgfile, &caldb, 1, &status);
    
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate the BAT distortion map in CALDB\n");
      return status;
    }
  }
  /* NOTE: now parms_distfile has file name */

  /* Read this file in */
  if (parms_distfile[0] && *detect_distmap == 0) {
    if (read_distortmap(parms_distfile, detect_distmap, &status)) {
      fprintf(stderr, "ERROR: could not read distortion map %s\n",
	      parms_distfile);
      return status;
    }
  }

  return status;
}


int compare_img_systems(fitsfile *imgfile, int imgnum,
			int parms_outvector, char *parms_hduclas2,

			struct wcs_coord *tanxy, struct wcs_coord *defcoor,
			double *exposure, double *tstart, double *tstop,
			double *batz, double *oversampx, double *oversampy,
			int *nbatdets,

			struct wcs_coord *otanxy, struct wcs_coord *odefcoor,
			double *oexposure, double *otstart, double *otstop,
			double *obatz, double *ooversampx, double *ooversampy,
			int *onbatdets,

			struct wcsprm **detect_wcs, int detect_altwcs[27],
			int *detect_nwcs,
			struct distortmap_struct **detect_distmap,
			int *detect_dist_corr,
			char *parms_distfile,

			int *detect_has_cel_coords, int *detect_has_tan_coords,
			char *parms_ra_colname, char *parms_dec_colname)

{
  int status = 0;
  char *header = 0;
  int nkeys = 0, nreject = 0;
  int distapp = 0;
  int iwcs;
  
  /* I: imgfile, detect.imgnum, parms->outvector */
  /* I/O: distmap, distfile, dist_corr */
  /* I/O: tanxy, defcoor, parms->{exposure,tstart,tstop,batz,oversampx,y,nbatdets} */
  /*      and 'o' versions of same */
  /*   O: detect.wcs, detect.nwcs, detect.altwcs, detect.has_{cel,tan}_coords */
  /*   O: parms.hduclas2 */
  *otanxy = *tanxy;
  *odefcoor = *defcoor;
  *oexposure = *exposure;
  *otstart = *tstart;
  *otstop = *tstop;
  *obatz = *batz;
  *ooversampx = *oversampx;
  *ooversampy = *oversampy;
  *onbatdets = *nbatdets;
  
  /* Read these, only to do a comparison with the previous values.
     The tanxy and defcoor structures are not used for coordinate
     conversion */
  read_wcsaxis(imgfile, 1, " ", 0, 0, defcoor->ctype1, 
	       &(defcoor->crpix1), &(defcoor->cdelt1), 
	       &(defcoor->crval1), &status);
  read_wcsaxis(imgfile, 2, " ", 0, 0, defcoor->ctype2, 
	       &(defcoor->crpix2), &(defcoor->cdelt2), 
	       &(defcoor->crval2),&status);
  if (status) {
    fprintf(stderr, "ERROR: could not read the primary WCS keywords\n");
    return status;
  }
  
  fits_write_errmark();
  read_wcsaxis(imgfile, 1, "T", 0, 0, tanxy->ctype1, 
	       &(tanxy->crpix1), &(tanxy->cdelt1), &(tanxy->crval1),&status);
  read_wcsaxis(imgfile, 2, "T", 0, 0, tanxy->ctype2, 
	       &(tanxy->crpix2), &(tanxy->cdelt2), &(tanxy->crval2),&status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    headas_chat(1,"NOTE: No tangent plane coordinates were detected.\n");
    headas_chat(1,"      Using the intermediate coordinates of the image.\n");
    
    if (*detect_distmap) {
      headas_chat(1,"WARNING: without a tangent plane coordinate system, the\n");
      headas_chat(1,"   distortion map cannot be used.  Disabling distortion map.\n");
      
      /* De-allocate storage */
      free(*detect_distmap);
      *detect_distmap = 0;
      /* Make sure we don't try again */
      parms_distfile[0] = 0;
      *detect_dist_corr = 0;
    }
  }
  
  /* Read image coordinates using WCSLIB */
  *detect_wcs = 0;         /* WCSLIB coordinate structures */
  *detect_nwcs = 0;
  
  fits_hdr2str(imgfile, 1, 0, 0, &header, &nkeys, &status);
  if (status) return status;
  
  status = wcspih(header, nkeys, 1, 0, &nreject, 
		  detect_nwcs, detect_wcs);
  free(header);
  
  if (status == 0) {
    status = wcsidx(*detect_nwcs, detect_wcs, &(detect_altwcs[0]));
    for (iwcs=0; iwcs<27; iwcs++) if (detect_altwcs[iwcs] >= 0) {
      int j = detect_altwcs[iwcs];
      if (status == 0) status = wcsset(&( (*detect_wcs)[j] ));
      headas_chat(5,"  ..found WCS '%c' coord sys..\n", 'A'+iwcs-1);
    }
  }
  if (status) {
    fprintf(stderr, "ERROR: WCS keyword ingest failed.\n");
    return status;
  }
  
  /* Special secret code indicates whether the column is celestial or not */
  *detect_has_cel_coords = (((*detect_wcs)[0].types[0] / 1000) % 10) == 2;
  /* Determine if the image has tangent plane coordinates:
   *    * either a non-celestial primary coordinate system or
   *    * an alternate 'T' tangent plane coordinate system 
   */
  *detect_has_tan_coords = ((detect_altwcs)['T'-'@'] >= 0) || (! *detect_has_cel_coords);
  
  /* Make appropriate column names */
  if (*detect_has_cel_coords) {
    sprintf(parms_ra_colname,  "%s_OBJ", (*detect_wcs)[0].lngtyp);
    sprintf(parms_dec_colname, "%s_OBJ", (*detect_wcs)[0].lattyp);
  } else {
    sprintf(parms_ra_colname,  "%s_OBJ", (*detect_wcs)[0].ctype[0]);
    sprintf(parms_dec_colname, "%s_OBJ", (*detect_wcs)[0].ctype[1]);
  }	
  
  fits_read_key(imgfile, TDOUBLE, "EXPOSURE", exposure, 0, &status);
  if (status) {
    fprintf(stderr, "WARNING: no exposure-related keywords found in input\n");
    fprintf(stderr, "         (assuming 1 second exposure)\n");
    status = 0;
    *exposure = 1;
  }
  
  fits_read_key(imgfile, TDOUBLE, "TSTART", tstart, 0, &status);
  fits_read_key(imgfile, TDOUBLE, "TSTOP", tstop, 0, &status);
  if (status) {
    fprintf(stderr, "WARNING: no time-related keywords found in input\n");
    fprintf(stderr, "         (assuming time 0 to %f)\n", *exposure);
    status = 0;
    *tstart = 0;
    *tstop = *exposure;
  }
  
  /* Read BAT_Z keyword if present */
  fits_read_key(imgfile, TDOUBLE, "BAT_ZOBJ", batz, 0, &status);
  if (status) {
    status = 0;
    *batz = 0;  /* Default is 0 */
  }
  
  /* Read other data from input file */
  fits_read_key(imgfile, TDOUBLE, "OVERSMPX", oversampx, 0, &status);
  fits_read_key(imgfile, TDOUBLE, "OVERSMPY", oversampy, 0, &status);
  if (status) {
    status = 0;
    *oversampx = 1.0;
    *oversampy = 1.0;
  }
  fits_read_key(imgfile, TINT, "NBATDETS", nbatdets, 0, &status);
  if (status) {
    status = 0;
    *nbatdets = 1;
  }  
  
  parms_hduclas2[0] = 0;
  fits_read_key(imgfile, TSTRING, "HDUCLAS2", parms_hduclas2, 0, &status);
  if (status) {
    status = 0;
    parms_hduclas2[0] = 0;
  }
  
  *detect_dist_corr = 1; /* By default, correct for distortion */
  fits_read_key(imgfile, TLOGICAL, "DISTAPP", &distapp, 0, &status);
  if (status == 0 && distapp == 1) { *detect_dist_corr = 0; }
  status = 0;
  
  /* We need to be sure that the WCS keywords for all files match if
     we are doing vector output. */
  if ((imgnum > 1) && (parms_outvector) && 
      ((memcmp(tanxy,otanxy,sizeof(*otanxy))) || 
       (memcmp(defcoor,odefcoor,sizeof(*odefcoor))) ||
       (*exposure != *oexposure) || (*tstart != *otstart) ||
       (*tstop != *otstop) || (*batz != *obatz) ||
       (*oversampx != *ooversampx) || (*oversampy != *ooversampy) ||
       (*nbatdets != *onbatdets)) ) {
    fprintf(stderr, "=======================================================\n");
    fprintf(stderr, "ERROR: to use vectorflux=yes, the image coordinates, exposure,\n");
    fprintf(stderr, "       TSTART/TSTOP, BAT_Z, OVERSAMPX/Y and NBATDETS must match\n");
    fprintf(stderr, "       exactly.  The provided images did not match.\n");
    fprintf(stderr, "       The output catalog will likely be meaningless.\n");
    fprintf(stderr, "=======================================================\n");
  }
  
  return status;
}


/* 
 * warn_low_snr - warn for conditions suggesting snrthresh setting was too low
 *
 * sources, nsources - source catalog 
 *   input fields: .status
 *
 * RETURNS: 1 if SNR too-low was found, 0 if OK
 *
 */
int warn_low_snr(struct source_struct *sources,
		 int nsources)
{
  int i;
  int snr_toolow = 0;

  /* Some error checking */
  for (i=0; i<nsources; i++) {
    if (sources[i].status == ERR_MAXSRCPIX) {
      snr_toolow = 1;
    }
  }
  if (snr_toolow) {
    fprintf(stderr, 
	    "ERROR: at least one source contained more than %d pixels.\n",
	    MAXSRCPIX);
    fprintf(stderr, 
	    "       This usually indicates a snrthresh which is too low.\n");
  }
  
  return snr_toolow;
}

