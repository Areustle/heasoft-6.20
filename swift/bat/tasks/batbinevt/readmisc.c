#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "headas_gti.h"
#include "bat_gswdev.h"

#include "batbinevt.h"

/* 
 * Routines for reading miscellaneous formats
 *   - read_detmask() - read detector quality map
 *   - read_data_gtibins() - read GTI time bins
 *   - read_maskwt() - read mask weighting map
 *   - read_ebounds() - read EBOUNDS extension
 *   - check_intype() - determine input file type
 *   - read_tlimits() - scan inputs for time limits and GTIs
 *
 *  02 Dec 2003 - split out from batbinevt.c
 *
 * $Id: readmisc.c,v 1.26 2010/06/11 19:00:32 craigm Exp $
 * C. Markwardt */


/* ----------------------------------------------------------------- */
/* Read detector mask file */
int *read_detmaskdata(fitsfile *fptr, long naxes[2], 
		      char *colname, int rownum, 
		      void *nullval,
		      int *status)
{
  int nhdu, curhdu, hdutype, bitpix, naxis;
  int typecode;
  long int nimages, repeat, width;
  long int fpixel[2] = {1, 1};
  int colnum, ncols;
  int *image;
  int j;

  if (*status) return 0;

  *status = 0;
  fits_get_num_hdus(fptr, &nhdu, status);
  fits_get_hdu_num(fptr, &curhdu);
  fits_get_hdu_type(fptr, &hdutype, status);
  if ((nhdu <= 0) || (curhdu < 0) || (*status != 0)) {
    if (*status == 0) *status = BAD_HDU_NUM;
    return 0;
  }

  if (hdutype == IMAGE_HDU) {

    fits_get_img_param(fptr, 2, &bitpix, &naxis, naxes, status);
    if ((naxis < 2) || (naxes[0] == 0) || (naxes[1] == 0) || (*status)) {
      if (*status == 0) *status = BAD_NAXIS;
      return 0;
    }
  } else {

    colnum = 0;
    if (colname) {
      /* We know the column name, so find the column number */
      fits_get_colnum(fptr, CASEINSEN, colname, &colnum, status);
    } else {

      /* DEFAULT: Column name is unknown, so we take the first
	 column which is a two dimensional array */
      fits_get_num_cols(fptr, &ncols, status);
      if (*status) return 0;
      
      for (j=1; j<=ncols; j++) {
	fits_read_tdim(fptr, j, 2, &naxis, naxes, status);
	if ((*status) || (naxis < 2) || (naxes[0] == 0) || (naxes[1] == 0)) 
	  continue;
	colnum = j;
      }
    }
    if (colnum == 0) {
      fprintf(stderr, "ERROR: Could not find 2-d image column\n");
      *status = COL_NOT_FOUND;
      return 0;
    }
    
    fits_get_num_rows(fptr, &nimages, status);
    fits_read_tdim(fptr, colnum, 2, &naxis, naxes, status);
    fits_get_coltype(fptr, colnum, &typecode, &repeat, &width, status);

    if (*status) return 0;
    if ((rownum <= 0) || (rownum > nimages)) {
      fprintf(stderr, "ERROR: requested image number %d does not exist\n", 
	      rownum);
      return 0;
    }
  }

  image = (int *) malloc (sizeof(int) * naxes[0] * naxes[1]);
  if (image == 0) {
    *status = MEMORY_ALLOCATION;
    return 0;
  }

  if (hdutype == IMAGE_HDU) {
    fits_read_pix(fptr, TINT, fpixel, naxes[0]*naxes[1], 0, 
		  image, 0, status);
  } else {
    fits_read_col(fptr, TINT, colnum, rownum, 1, naxes[0]*naxes[1], 
		  nullval, image, 0, status);
  }

  if (*status) {
    free(image);
    return 0;
  }

  return image;
}

/* ----------------------------------------------------------------- */
/* Read detector quality map from file */
int *read_detmask(char *filename, long axes[2], int goodval, 
		  char *colname, int rownum, int *status)
{
  int i;
  int *image = 0;
  fitsfile *fptr = 0;
  int safestatus = 0;
  int nullval = 9999999;
  
  if (status == 0) return 0;
  if (*status != 0) return 0;
  if (filename == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }

  fits_open_data(&fptr, filename, READONLY, status);
  if (*status) return 0;

  image = read_detmaskdata(fptr, axes, colname, rownum, &nullval, status);

  safestatus = 0;
  fits_write_errmark();
  fits_read_key(fptr, TINT, "GOODVAL", &goodval, 0, &safestatus);
  fits_clear_errmark();

  safestatus = 0;
  fits_close_file(fptr, &safestatus);
  if (*status) return 0;
  if (image == 0) {
    *status = MEMORY_ALLOCATION;
    return 0;
  }

  for (i=0; i< (axes[0]*axes[1]); i++) {
    image[i] = (image[i] == goodval);
  }

  return image;

}

/* ----------------------------------------------------------------- */
/* Read tbins from input data array */
int read_data_gtibins(fitsfile *infile, 
		      struct gti_struct *gti, 
		      double **exposure,
		      int *status)
{
  long int nrows = 0;
  int timecol, expocol, i;
  double timepixr, timedeli;
  double *expo = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;
  if ((infile == 0) || (gti == 0))
    return (*status = NULL_INPUT_PTR);

  headas_chat(5, "   (reading time rows from input)\n");
  HDgti_init(gti);

  /* 
     START = TIME - TIMEPIXR*TIMEDEL or
     START = TIME - TIMEPIXR*EXPOSURE

     STOP = TIME_STOP or
     STOP = TIME + (1-TIMEPIXR)*TIMEDEL or
     STOP = TIME + (1-TIMEPIXR)*EXPOSURE
  */

  fits_get_num_rows(infile, &nrows, status);
  if (*status) {
    fprintf(stderr, "ERROR: could not read number of rows from infile\n");
    return *status;
  }

  HDgti_grow(gti, nrows, status);
  if (*status) {
    fprintf(stderr, "ERROR: could not enlarge user GTI for 'infile' binning\n");
    return *status;
  }

  if (nrows == 0) return *status;

  expo = (double *) malloc( sizeof(double) * nrows );
  if (expo == 0) {
    fprintf(stderr, "ERROR: could not allocate memory for exposure list\n");
    return (*status = MEMORY_ALLOCATION);
  }

  fits_get_colnum(infile, CASEINSEN, "TIME", &timecol, status);
  if (*status) {
    fprintf(stderr, "ERROR: could not find TIME column for 'infile' binning\n");
    return *status;
  }
  fits_read_col(infile, TDOUBLE, timecol, 1, 1, nrows, 0, 
		gti->start, 0, status);
  if (*status) {
    fprintf(stderr, "ERROR: could not read TIME column\n");
    return *status;
  }

  /* Default is that TIME refers to center of bin */
  timepixr = 0.5;
  fits_read_key(infile, TDOUBLE, "TIMEPIXR", &timepixr, 0, status);
  *status = 0;

  fits_get_colnum(infile, CASEINSEN, "TIME_STOP", &expocol, status);
  if (*status == 0) {
    /* TIME_STOP column was found, use it directly */
    headas_chat(5,"   (found TIME_STOP)\n");
    fits_read_col(infile, TDOUBLE, expocol, 1, 1, nrows, 0, 
		  gti->stop, 0, status);
  } else {

    *status = 0;

    /* TIME_STOP was not found, so compute it based on TIMEDEL or
       EXPOSURE */

    /* First check if there is a TIMEDEL or EXPOSURE column */
    fits_get_colnum(infile, CASEINSEN, "TIMEDEL", &expocol, status);
    if (*status) {
      *status = 0;
      fits_get_colnum(infile, CASEINSEN, "EXPOSURE", &expocol, status);
    }

    if (*status) {
      /* Column TIMEDEL or EXPOSURE was not found, check for keyword */
      *status = 0;

      fits_read_key(infile, TDOUBLE, "TIMEDEL", &timedeli, 0, status);
      if (*status) {
	*status = 0;
	fits_read_key(infile, TDOUBLE, "EXPOSURE", &timedeli, 0, status);
      }
      if (*status) {
	fprintf(stderr, 
         "ERROR: could not find TIME_STOP, TIMEDEL, or EXPOSURE\n"
	 "       values from input file for 'infile' time binning.\n");
	return *status;
      }

      headas_chat(5,"   (found TIMEDEL/EXPOSURE keyword)\n");
      /* Compute the START and STOP time based on scalar TIMEDEL */
      for(i=0; i<nrows; i++) {
	gti->start[i] -= timedeli*timepixr;
	gti->stop[i]   = gti->start[i] + timedeli;
      }

    } else {
      /* Column form of TIMEDEL or EXPOSURE were found */

      headas_chat(5,"   (found TIMEDEL/EXPOSURE column)\n");
      /* NOTE: gti->stop[i] initially contains the exposure */
      fits_read_col(infile, TDOUBLE, expocol, 1, 1, nrows, 0, 
		    gti->stop, 0, status);
      for(i=0; i<nrows; i++) {
	gti->start[i] -= gti->stop[i]*timepixr;
	gti->stop[i]   = gti->start[i] + gti->stop[i];
      }
      /* NOTE: now gti->stop[i] contains the stop time */
    }
  }

  /* Read exposure values */
  fits_write_errmark();
  if (fits_get_colnum(infile, CASEINSEN, "EXPOSURE", &expocol, status)) {
    *status = 0;
    if (fits_get_colnum(infile, CASEINSEN, "ONTIME", &expocol, status)) {
      *status = 0;
      if (fits_get_colnum(infile, CASEINSEN, "TELAPSE", &expocol, status)) {
	*status = 0;
	if (fits_get_colnum(infile, CASEINSEN, "TIMEDEL", &expocol, status)) {
	  *status = 0;
	  expocol = -1;
	}
      }
    }
  }	
  fits_clear_errmark();
    
  if (expocol >= 1) {
    fits_read_col(infile, TDOUBLE, expocol, 1, 1, nrows, 0, 
		  expo, 0, status);
  } else {
    for(i=0; i<nrows; i++) expo[i] = gti->stop[i] - gti->start[i];
  }

  if (exposure) { *exposure = expo; }
  gti->ngti = nrows;
  return *status;
}

/* ----------------------------------------------------------------- */
/* Read any keywords containing *APP (corrections) */
int read_app_keywords(fitsfile *file, 
		      char keynames[][FLEN_CARD], 
		      char keycomms[][FLEN_CARD], 
		      int *keyvalues, 
		      int nmax, int *status)
{
  char card[FLEN_CARD];
  char *inclist[] = {"*APP"};
  int ncards = 0;
  int i, nfound = 0;
  int keylength = 0;

  if (status == 0) return -1;
  if (*status) return -1;
  if ((file == 0) || (keynames == 0) || (keyvalues == 0) ||
      (keycomms == 0)) {
    *status = NULL_INPUT_PTR;
    return -1;
  }

  /* Reset header pointer to beginning of file */
  if (fits_get_hdrspace(file, &ncards, 0, status)) return -1;
  if (ncards <= 0) return 0;
  if (fits_read_record(file, 0, card, status)) return -1;

  while (1) {
    keylength = 0;
    if (fits_find_nextkey(file, inclist, 1, 0, 0, card, status)) break;
    if (fits_get_keyname(card, keynames[nfound], &keylength, status)) break;
    keynames[nfound][keylength] = 0; /* Null terminate */
    nfound ++;
    if (nfound >= nmax) break;
  }

  if (*status == KEY_NO_EXIST) *status = 0;
  if (*status) return -1;

  for (i=0; i<nfound; i++) {
    fits_read_key(file, TLOGICAL, keynames[i], &(keyvalues[i]), 
		  keycomms[i], status);
  }

  if (*status) return -1;
  return nfound;
}


/* ----------------------------------------------------------------- */
/* Read mask weight map file */
double *read_maskwt(char *filename, long axes[2],
		    struct parm_struct *parms, int *status)
{
  int naxis, bitpix;
  double *image = 0;
  fitsfile *fptr = 0;
  long int fpixel[] = {1,1};
  int safestatus = 0;
  int i;
  
  if (status == 0) return 0;
  if (*status != 0) return 0;
  if (filename == 0 || parms == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }

  fits_open_image(&fptr, filename, READONLY, status);
  if (*status) return 0;

  fits_get_img_param(fptr, 2, &bitpix, &naxis, axes, status);
  if (*status) goto FAILED;

  image = (double *) malloc (sizeof(double) * axes[0] * axes[1]);
  if (image == 0) {
    *status = MEMORY_ALLOCATION;
    goto FAILED;
  }

  fits_read_pix(fptr, TDOUBLE, fpixel, axes[0]*axes[1], 0, 
		image, 0, status);
  if (*status) goto FAILED;

  parms->nappkeys = read_app_keywords(fptr, 
				      parms->appkeynames, 
				      parms->appkeycomms, 
				      parms->appkeyvals, 
				      MAX_APP_KEYS, status);
  if (parms->nappkeys < 0) {
    fprintf(stderr, "ERROR: while reading *APP keys from input\n");
    goto FAILED;
  }

  headas_chat(5, "  (found %d *APP keywords)\n", parms->nappkeys);
  for (i=0; i<parms->nappkeys; i++) {
    headas_chat(5, "  (%s=%d)\n", parms->appkeynames[i], parms->appkeyvals[i]);
  }

  fits_close_file(fptr, status);
  return image;


 FAILED:
  /* Free the image data if it was allocated */
  if (image) free(image);
  image = 0;

  /* Close the file if it was open */
  safestatus = 0;
  if (fptr) fits_close_file(fptr, &safestatus);
  fptr = 0;

  return 0;
}

/* ----------------------------------------------------------------- */
/* Check type of input file; result is placed in parms->intype */
int check_intype(fitsfile *infile, 
		 int i,
		 struct parm_struct *parms,
		 int *status)
{
  long int nebins;
  float emin[1024], emax[1024];
  int dphcol;
  int naxis;
  long int naxes[3];
  int j;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((infile == 0) || (parms == 0)) return (*status = NULL_INPUT_PTR);
  
  /* FIRST CASE: we don't know yet whether we are reading EVENTS or
     BAT_DPH files */
  if (parms->intype == 0) {
    /* ======================================================= */

    fits_movnam_hdu(infile, BINARY_TBL, "EVENTS", 0, status);
    if (*status == 0) {
      /* That worked, we will use events all the time */
      parms->intype = EVENTS;

      goto CHECK_EVT_QUANTIZATION;

    } else {
      /* That didn't work, try BAT_DPH */
      *status = 0;
      fits_movnam_hdu(infile, BINARY_TBL, "BAT_DPH", 0, status);
      if (*status) {
	fprintf(stderr, "ERROR: The input file must contain an EVENTS or BAT_DPH extension\n");
	return *status;
      }
      parms->intype = DPH;

      fits_get_colnum(infile, CASEINSEN, parms->dphcolumn, &dphcol, status);
      fits_read_tdim(infile, dphcol, 3, &(parms->ndphaxes), 
		     parms->dphaxes, status);
      if (*status) {
	fprintf(stderr, "ERROR: could not read dimensions for %s in %s\n",
		parms->dphcolumn, parms->infiles[i]);
	return *status;
      }
      /* If we are reading a 2D DPI, then rescale to 3 dims */
      if (parms->ndphaxes == 2) {
	parms->dphaxes[2] = parms->dphaxes[1];
	parms->dphaxes[1] = parms->dphaxes[0];
	parms->dphaxes[0] = 1;
	parms->ndphaxes   = 3;
      }

      /* Read HDUCLAS2, which contains the class of data, measured
	 vs. predicted */
      fits_read_key(infile, TSTRING, "HDUCLAS2", parms->hduclas2, 0, status);
      headas_chat(5, "   (HDUCLAS2 = '%s')\n", parms->hduclas2);
      if (*status) {
	fprintf(stderr, 
		"WARNING: could not find HDUCLAS2 keyword in %s\n"
		"         assuming that it contains measured counts\n",
		parms->infiles[i]);
	strcpy(parms->hduclas2, "TOTAL");
	*status = 0;
      }

      /* Read energy bins */
      read_ebounds(infile, &(parms->ndphbins), 1024, 
		   parms->dphemin, parms->dphemax, status);
      if (*status || (parms->ndphbins <= 0)) {
	fprintf(stderr, "ERROR: Could not read BAT_DPH EBOUNDS extension\n");
	fprintf(stderr, "       of %s\n", parms->infiles[i]);
	if (*status == 0) *status = BAD_DIMEN;
	return *status;
      }
      headas_chat(5, "   (found %d energy bins from %f to %f keV)\n",
		  parms->ndphbins, 
		  parms->dphemin[0], parms->dphemax[parms->ndphbins-1]);

      

    }
  } else if (parms->intype == EVENTS) {

    /* ======================================================= */
    /* SECOND CASE: We know we are reading EVENTS files */

    int picol;
    int safestatus = 0;

    fits_movnam_hdu(infile, BINARY_TBL, "EVENTS", 0, status);
    if (*status) {
      fprintf(stderr, "ERROR: You may not mix EVENTS and BAT_DPH files\n");
      fprintf(stderr, "  First file was events file: %s\n", parms->infiles[0]);
      fprintf(stderr, "  This file was not: %s\n", parms->infiles[i]);
      return *status;
    }

  CHECK_EVT_QUANTIZATION:
    /* Check for the column of interest... */
    fits_write_errmark();
    safestatus = 0;
    fits_get_colnum(infile, CASEINSEN,parms->coltype,&picol,&safestatus);
    if (safestatus == 0) {
      char keyname[FLEN_CARD];
      float keyvalue = 0.0;

      /* ... and if present, check for the TSCALn keyword... */
      fits_make_keyn("TSCAL", picol, keyname, &safestatus);
      fits_read_key(infile, TFLOAT, keyname, &keyvalue, 0, &safestatus);
      if ((safestatus == 0) && (keyvalue != 1.0) && (keyvalue != 0)) {
	headas_chat(5, " ...found %s = %f...\n", keyname, keyvalue);
	if (parms->ebinquant != 0) {

	  /* A quantization has been found in previous files.  Check
             that this file matches. */
	  if (fabs(parms->ebinquant-keyvalue) > 0.01) {

	    fprintf(stderr, "ERROR: the input file %s columns have different energy quantizations\n", parms->coltype);
	    fprintf(stderr, "       and should not be binned together.\n");
	    fits_clear_errmark();
	    return (*status = ZERO_SCALE);
	  }
	} else {
	  /* This is the first quantization to be found.  Use it as
             the default. */
	  parms->ebinquant = keyvalue;
	}
      }
    }
    fits_clear_errmark();


  } else if (parms->intype == DPH) {
    char hduclas2[FLEN_CARD];
    
    /* ======================================================= */
    /* THIRD CASE: We know we are reading BAT_DPH files */

    fits_movnam_hdu(infile, BINARY_TBL, "BAT_DPH", 0, status);
    if (*status) {
      fprintf(stderr, "ERROR: You may not mix EVENTS and BAT_DPH files\n");
      fprintf(stderr, "  First file was DPH file: %s\n", parms->infiles[0]);
      fprintf(stderr, "  This file was not: %s\n", parms->infiles[i]);
      return *status;
    }

    /* Check first that the spatial dimensions agree */
    fits_get_colnum(infile, CASEINSEN, parms->dphcolumn, &dphcol, status);
    fits_read_tdim(infile, dphcol, 3, &naxis, naxes, status);
    if (*status) {
      fprintf(stderr, "ERROR: could not read dimensions for %s in %s\n",
	      parms->dphcolumn, parms->infiles[i]);
      return *status;
    }
    
    if (naxis != parms->ndphaxes) {
    SPATIAL_NOAGREE:
      fprintf(stderr, "ERROR: BAT_DPH spatial dimensions must agree\n");
      return (*status = -1);
    }
    for (j=0; j<naxis; j++) {
      if (naxes[j] != parms->dphaxes[j]) goto SPATIAL_NOAGREE;
    }

    /* Those agree, are they also of the same class of map?  For
       example measured vs. predicted counts? */
    fits_read_key(infile, TSTRING, "HDUCLAS2", hduclas2, 0, status);
    if (*status) {
	fprintf(stderr, 
		"WARNING: could not find HDUCLAS2 keyword in %s\n"
		"         assuming that it contains measured counts\n",
		parms->infiles[i]);
	strcpy(hduclas2, "TOTAL");
	*status = 0;
    }
    /* Strip off any trailing blank characters */
    for (j=0; hduclas2[j]; j++) 
      if (hduclas2[j] == ' ') hduclas2[j] = '\0';
    for (j=0; parms->hduclas2[j]; j++) 
      if (parms->hduclas2[j] == ' ') parms->hduclas2[j] = '\0';

    if (strcasecmp(parms->hduclas2, hduclas2) != 0) {
      fprintf(stderr,
	      "ERROR: HDUCLAS2 keyword of %s does not match the first input file\n"
	      "       batbinevt must only be used with files of the same type.\n",
	      parms->infiles[i]);
      return (*status = -1);
    }

    /* Ahh, the spatial dimensions do agree! Good.  Now check energy */
    read_ebounds(infile, &nebins, 1024, emin, emax, status);
    if (*status) {
      fprintf(stderr, "ERROR: Could not read BAT_DPH EBOUNDS extension\n");
      fprintf(stderr, "       of %s\n", parms->infiles[i]);
      return *status;
    }
    headas_chat(5, "   (found %d energy bins from %f to %f keV)\n",
		nebins, emin[0], emax[nebins-1]);

    if (nebins != parms->ndphbins) {
      fprintf(stderr, "ERROR: number of energy bins in input files does not agree\n");
      return (*status = -1);
    }

    for (j = 0; j<nebins; j++) {
      if ((fabs(emin[j] - parms->dphemin[j]) > 0.01) ||
	  (fabs(emax[j] - parms->dphemax[j]) > 0.01)) {
	fprintf(stderr, "ERROR: energy bins of input files do not agree (bin %d)\n",j);
	return (*status = -1);
      }
    }

    
    /* Finally! So energy and spatial dimensions agree, and the class
       agrees, so we can proceed */

  } else {

    /* ======================================================= */
    fprintf(stderr, "ERROR: invalid input type %d\n", parms->intype);
    return (*status = -1);
  }

  return (*status);
}


/* Small utility routine to accumulate GTIs in-place */
struct gti_struct *gti_accum(int method, 
			     struct gti_struct *orig,
			     struct gti_struct *new, 
			     int *status)
{
  struct gti_struct tempgti;
  double orig_mjdref;
  HDgti_init(&tempgti);

  if (status == 0 || *status != 0) return 0;

  orig_mjdref = orig->mjdref;
  if (orig_mjdref == 0) orig_mjdref = new->mjdref;

  if (HDgti_merge(method, &tempgti, orig, new, status)) return 0;
  HDgti_free(orig);
  *orig = tempgti;
  orig->mjdref = orig_mjdref;
  HDgti_init(&tempgti);

  return orig;
}

/* ----------------------------------------------------------------- */
/* Read time limits from input file TSTART/TSTOP keywords */
int read_tlimits(struct parm_struct *parms)
{
  int status = 0;
  int i, j;
  fitsfile *infile;
  struct gti_struct *gtis;
  struct gti_struct fugti; /* File bins */
  struct gti_struct fumaster;
  char *curext = "";         /* Data extension: "EVENTS" or "BAT_DPH" */
  double expo = 0;
  double *dph_expo = 0;       /* exposure of each DPH in one file */
  double *dph_expomaster = 0; /* exposure of each DPH */
  int    *dph_fileindex = 0;  /* which file each DPH came from */
  int fudged_gti = 0;

  headas_chat(5, "  ...determining tlimits...\n");
  if ((parms->ninfiles <= 0) || (parms == 0)) return NULL_INPUT_PTR;
  gtis = parms->gtis;
  HDgti_init(&fumaster);

  for (i=0; i<parms->ninfiles; i++) {
    parms->ftstart[i] = T_INDEF;
    parms->ftstop[i]  = T_INDEF;
    fudged_gti = 0;

    infile = 0;
    fits_open_file(&infile, parms->infiles[i],READONLY,&status); 
    if (status) {
      fprintf(stderr, "Unable to open %s for read access\n", 
	      parms->infiles[i]);
      goto CLEANUP;
    }

    check_intype(infile, i, parms, &status);
    if (status) {
      fprintf(stderr, "ERROR: Determining file type and/or energy binning\n");
      goto CLEANUP;
    }
    switch(parms->intype) {
    case EVENTS: curext = "EVENTS";  break;
    case DPH:    curext = "BAT_DPH"; break;
    }
    parms->dataext = curext;

    if ((parms->tbinmethod == INFBINS) && (parms->intype != DPH)) {
      fprintf(stderr, "ERROR: 'infile' binning must only be used for binned input data\n");
      status = -1;
      goto CLEANUP;
    }

    /* First attempt: Read GTI extension in file.  File is already
       open, so pass it ... */
    HDgti_init(&gtis[i]);
    HDgti_read(0, &gtis[i], 0, 0, 0, 0, &infile, &status);
    if (status == 0) 
      headas_chat(5, "  ...found GTI extension (%d entries)...\n", gtis[i].ngti);

    if ((status == 0) && (gtis[i].ngti > 0)) {
      
      /* Compute min/max file time from GTI */
      parms->ftstart[i] = gtis[i].start[0];
      parms->ftstop[i]  = gtis[i].stop[0];
      for (j=1; j<gtis[i].ngti; j++) {
	if (gtis[i].start[j] < parms->ftstart[i]) {
	  parms->ftstart[i] = gtis[i].start[j];
	}
	if (gtis[i].stop[j] > parms->ftstop[i]) {
	  parms->ftstop[i] = gtis[i].stop[j];
	}
      }

    } else {
	  
      fprintf(stderr, 
	      "WARNING: input file %s\n"
	      "         did not contain a GTI extension.\n",
	      parms->infiles[i]);
	      
      headas_chat(5, "  ...reverting to TSTART/TSTOP...\n");
      status = 0;

      /* Couldn't find GTI, so open input events file */
      
      /* Move to the first extension containing BAT events.  */
      fits_movnam_hdu(infile, BINARY_TBL, curext, 0, &status);
      if (status != 0) {
	fprintf(stderr, 
		"ERROR: input file does not contain an EVENTS extension\n");
	goto CLEANUP;
      }

      parms->ftstart[i] = HDget_frac_time(infile, "TSTART", 0, 0, &status);
      if (status) parms->ftstart[i] = T_INDEF;
      status = 0;
      parms->ftstop[i] = HDget_frac_time(infile, "TSTOP", 0, 0, &status);
      if (status) parms->ftstop[i] = T_INDEF;
      status = 0;

      /* If we can't read the time range at all, then bail */
      if ((parms->ftstart[i] == T_INDEF) || (parms->ftstop[i] == T_INDEF)) {
	fprintf(stderr, "ERROR: could not determine time span of %s\n",
		parms->infiles[i]);
	goto CLEANUP;
      }

      /* Create a simplified GTI with only the TSTART/TSTOP range */
      if (HDgti_grow(&gtis[i], 1, &status)) {
	fprintf(stderr, "ERROR: could not allocate basic GTI\n");
	goto CLEANUP;
      }
      gtis[i].start[0] = parms->ftstart[i];
      gtis[i].stop[0] = parms->ftstop[i];
      gtis[i].ngti = 1;
      fudged_gti = 1;

      if (parms->intype == EVENTS) {
	/* Note: DPHs may be recovered below, so the warning doesn't
	   apply to them here */
	fprintf(stderr, 
		"WARNING: could not find a GTI extension in %s\n"
		"         (reverting to TSTART/TSTOP values)\n",
		parms->infiles[i]);
      }
    }

    if (parms->intype == DPH) {
      /* For DPH files, read input file times, and convert to a GTI */
      int j, k;

      fits_movnam_hdu(infile, BINARY_TBL, curext, 0, &status);
      HDgti_init(&fugti);
      read_data_gtibins(infile, &fugti, &dph_expo, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not read DPH file time binning\n");
	goto CLEANUP;
      }

      /* If we fudged the GTI above using TSTART/TSTOP, we now have a
	 better GTI to use. */
      if (fudged_gti) {
	if (gtis[i].start[0] != fugti.start[0] ||
	    gtis[i].stop[0]  != fugti.stop[fugti.ngti-1]) {
	  fprintf(stderr, 
		  "WARNING: DPH times and TSTART/TSTOP values do not agree\n"
		  "         in file %s\n"
		  "         (using DPH times and exposures)\n",
		  parms->infiles[i]);
	  headas_chat(5, 
		      "       DPH (start,stop) = (%f,%f)\n"
		      "       (TSTART,TSTOP)   = (%f,%f)\n",
		      fugti.start[0], fugti.stop[fugti.ngti-1],
		      gtis[i].start[0], gtis[i].stop[0]);
	}

	HDgti_grow(&gtis[i], fugti.ngti, &status);
	HDgti_copy(&gtis[i], &fugti, &status);

	if (status) {
	  fprintf(stderr, "ERROR: could not create file GTI\n");
	  goto CLEANUP;
	}
	
	fudged_gti = 0;
      }

      /* Merge fugti (this file) with fumaster (total) */
      if (HDgti_grow(&fumaster, fumaster.ngti+fugti.ngti, &status)) {
	fprintf(stderr, "ERROR: could not grow 'infile' GTI\n");
	goto CLEANUP;
      }
      if (dph_expomaster == 0) {
	dph_expomaster = (double *)malloc(sizeof(double)*fugti.ngti);
	dph_fileindex  = (int *)malloc(sizeof(int)*fugti.ngti);
      } else {
	dph_expomaster = (double *)realloc(dph_expomaster,
					   sizeof(double)*(fugti.ngti+fumaster.ngti));
	dph_fileindex  = (int *)realloc(dph_fileindex,
					sizeof(int)*(fugti.ngti+fumaster.ngti));
      }
      if (dph_expomaster == 0 || dph_fileindex == 0) {
	fprintf(stderr, "ERROR: could not (re)allocate memory for master exposure list\n");
	goto CLEANUP;
      }

      /* Append this file's intervals to the master list */
      for (k=fumaster.ngti, j=0; j<fugti.ngti; k++, j++) {
	fumaster.start[k] = fugti.start[j];
	fumaster.stop[k]  = fugti.stop[j];
	dph_expomaster[k] = dph_expo[j];
	dph_fileindex[k]  = i;    /* Record which file this DPH is from */
	fumaster.ngti ++;
      }

      headas_chat(5, "   (file infbins=%d, cumulative=%d)\n", fugti.ngti, fumaster.ngti);

      HDgti_free(&fugti);
    }

    if (gtis[i].mjdref == 0) {
      int safestatus = 0;
      gtis[i].mjdref = HDget_frac_time(infile, "MJDREF", 0, 0, &safestatus);
    }

    /* Close file, we are done with it */
    fits_close_file(infile, &status);
    infile = 0;

    headas_chat(5, "  - File %s (MJDREF=%f) : GTI :\n", 
		parms->infiles[i], gtis[i].mjdref);
    for (j=0; j<gtis[i].ngti; j++) {
      headas_chat(5, "    %3d. (%f,%f)\n", j+1, 
		  gtis[i].start[j], gtis[i].stop[j]);
    }
    headas_chat(5, "         (%f,%f) = (START,STOP)\n",
		parms->ftstart[i], parms->ftstop[i]);

    /* Merge this file's GTI with the master GTI, using "OR" or UNION  */
    if (gti_accum(GTI_OR, &parms->gtimaster, &gtis[i], &status) == 0) {
      fprintf(stderr, "ERROR: could not merge GTIs\n");
      goto CLEANUP;
    }

  }

  /* No data were found */
  if (parms->gtimaster.ngti == 0) {
    headas_chat(1, "WARNING: no good times were found in input events\n");
    goto CLEANUP;
  }

  headas_chat(5, "  - MASTER : GTI :\n");
  for (j=0; j<parms->gtimaster.ngti; j++) {
    headas_chat(5, "    %3d. (%f,%f)\n", j+1, 
		parms->gtimaster.start[j], parms->gtimaster.stop[j]);
  }

  /* Initialize the user gtifile */
  headas_chat(5,"   (before user GTI selection, ngti=%d)\n", 
	      parms->gtimaster.ngti);
  HDgti_init(&parms->ugti);

  /* Read user gti */
  if (parms->gtifile[0]) {

    if (parms->tbinmethod != MATCHBINS) {
      /* User gti is commonly a standard GTI file ... */
      HDgti_read(parms->gtifile, &parms->ugti, 0, 0, 0, 0, 0, &status);
    } else {
      /* ... but it can be a light curve that we want to match the
	 binning of */
      fitsfile *lcfile = 0;
      int safestatus = 0;
      fits_open_data(&lcfile, parms->gtifile, READONLY, &status);
      if (status == 0) {
	read_data_gtibins(lcfile, &parms->ugti, 0, &status);
	fits_close_file(lcfile, &safestatus);
      }
    }


    if (status == 0) {

      headas_chat(5, "   (read user gti, ngti=%d)\n", 
		  parms->ugti.ngti);
      /* If the user GTI file was provided, then merge it with the
         "master" GTI, so that we have a better accounting */
      if (gti_accum(GTI_AND, &parms->gtimaster, &parms->ugti,&status) == 0) {
	fprintf(stderr, "ERROR: could not merge master GTI and gtifile\n");
	goto CLEANUP;
      }

      if (parms->tbinmethod != MATCHBINS) {
	for (i=0; i<parms->ugti.ngti; i++) {
	  expo = HDgti_exp(parms->ugti.start[i], parms->ugti.stop[i],
			   &(parms->gtimaster), &status);
	  if (expo == 0) { 
	    parms->ugti.start[i] = parms->ugti.stop[i] = 0;
	  }
	}
	
	/* Compress the user gti, excluding those intervals which have
	   absolutely no overlap with the master GTI */
	for (i=0, j=0; i<parms->ugti.ngti; i++) {
	  if (parms->ugti.stop[i] > parms->ugti.start[i]) {
	    parms->ugti.start[j] = parms->ugti.start[i];
	    parms->ugti.stop[j] = parms->ugti.stop[i];
	    j++;
	  }
	}
	parms->ugti.ngti = j;
	headas_chat(5, "   (after compression, user gti, ngti=%d)\n", 
		    parms->ugti.ngti);
      }
    } else {
      fprintf(stderr, "ERROR: could not open GTI file %s\n",
	      parms->gtifile);
      goto CLEANUP;
    }
  }
  headas_chat(5,"   (after user GTI selection, ngti=%d)\n", 
	      parms->gtimaster.ngti);

  /* No data were found */
  if (parms->gtimaster.ngti == 0) {
    headas_chat(1, "WARNING: no good times were found after intersection with gtifile\n");
    goto CLEANUP;
  }


  /* Scan through file times and find min/max */
  parms->ftmin = parms->ftstart[0];
  parms->ftmax = parms->ftstop[0];
  for (i=1; i<parms->ninfiles; i++) {
    if (parms->ftstart[i] < parms->ftmin) parms->ftmin = parms->ftstart[i];
    if (parms->ftstop[i]  > parms->ftmax) parms->ftmax = parms->ftstop[i];
  }

  /* If user gave default values for start/stop time, fill those in */
  if (parms->tstart == T_INDEF) {
    parms->tstart = parms->ftmin;
  }
  if (parms->tstop == T_INDEF) {
    parms->tstop = parms->ftmax;
  }

  /* Apply user-selected time limits as a GTI */
  headas_chat(5,"   (before user time selection, ngti=%d)\n", 
	      parms->gtimaster.ngti);
  if ((parms->tstart != parms->ftmin) || (parms->tstop != parms->ftmax)) {
    struct gti_struct tlimits;  /* Note: statically allocated */
    tlimits.timezero = parms->gtimaster.timezero;
    tlimits.ngti = 1;
    tlimits.start = &parms->tstart;
    tlimits.stop = &parms->tstop;
    if (gti_accum(GTI_AND, &parms->gtimaster, &tlimits,&status) == 0) {
      fprintf(stderr, "ERROR: could not merge master GTI and tstart/stop\n");
      goto CLEANUP;
    }
  }

  /* Sort the master list of DPH times in ascending time order */
  headas_chat(5,"   (before partial overlap, ngti=%d)\n", 
	      parms->gtimaster.ngti);
  if (parms->intype == DPH && fumaster.ngti > 0) {
    int j, k;
    struct gti_struct dph_gti;

    /* Make sure the DPH GTI has at least four entries (may not use it
       though) */
    HDgti_init(&dph_gti);
    HDgti_grow(&dph_gti, 4, &status);
    if (status) { 
      fprintf(stderr, "ERROR: could not create DPH GTI\n");
      goto CLEANUP;
    }

    for(j=0; j<fumaster.ngti; j++) {
      for(k=j; k<fumaster.ngti; k++) {
	if (fumaster.start[k] < fumaster.start[j]) {
	  double start, stop, expo;
	  int ind;

	  start = fumaster.start[k]; stop = fumaster.stop[k];
	  expo = dph_expomaster[k];
	  ind  = dph_fileindex[k];

	  fumaster.start[k] = fumaster.start[j]; 
	  fumaster.stop[k] = fumaster.stop[j];
	  dph_expomaster[k] = dph_expomaster[j];
	  dph_fileindex[k] = dph_fileindex[j];

	  fumaster.start[j] = start;
	  fumaster.stop[j] = stop;
	  dph_expomaster[j] = expo;
	  dph_fileindex[j] = ind;
	}
      }
    }

    /* Now check for partial overlapping DPHs and remove them */
    for(j=0; j<fumaster.ngti && parms->gtimaster.ngti > 0; j++) {
      /* compute partial overlap with master GTI */
      expo = HDgti_exp(fumaster.start[j], fumaster.stop[j],
		       &(parms->gtimaster), &status);

      /* Check if that overlap is too small */
      if ( status ||
	   (expo < dph_expomaster[j]*parms->minfracoverlap) ||
	   (expo < parms->mintimeoverlap) ||
	   ((dph_expomaster[j] - expo) > parms->maxtimenonoverlap) ) {

	/* Create a bad-time interval to eliminate this DPH */
	dph_gti.ngti = 2;
	dph_gti.start[0] = -1e307;           dph_gti.stop[0] = fumaster.start[j];
	dph_gti.start[1] = fumaster.stop[j]; dph_gti.stop[1] = 1e307;
	gti_accum(GTI_AND, &parms->gtimaster, &dph_gti, &status);
	status = 0;

	/* Remove this DPH from the list of good ones */
	dph_expomaster[j] = -dph_expomaster[j];

      } else if ( (status == 0) && (dph_expomaster[j]-expo) > 1e-6) {

	/* Here is a tricky situation: if the overlap was big enough
	   to satisfy the constraints, then we will accept the entire
	   DPH.  However, the user-GTI and the actual good time of the
	   DPH may not match.  Thus, we must update the user-gti with
	   the actual good time stored in the DPH, since we can't
	   accept a fraction of the DPH. 

	   This is a bit of a torturous procedure.  First we must find
	   out what the GTI for this DPH *only* is.  Then we must OR
	   that DPH-GTI  with the user-gti. */

	/* DPH-only GTI */
	dph_gti.ngti = 1;
	dph_gti.start[0] = fumaster.start[j];
	dph_gti.stop[0]  = fumaster.stop[j];
	  
	/* AND with file-GTI */
	gti_accum(GTI_AND, &dph_gti, &gtis[dph_fileindex[j]], &status);

	/* OR with user-gti */
	gti_accum(GTI_OR, &parms->gtimaster, &dph_gti, &status);
	
	status = 0;
      }
    }

    /* Compress out the "bad" DPHs from the data interval list, since
       these may become the "user" GTI. */
    for (i=0, j=0; i<fumaster.ngti; i++) {
      if (dph_expomaster[i] > 0) {
	fumaster.start[j] = fumaster.start[i];
	fumaster.stop[j] = fumaster.stop[i];
	dph_expomaster[j] = dph_expomaster[i];
	dph_fileindex[j] = dph_fileindex[i];
	j++;
      }
    }
    fumaster.ngti = j;

    /* Merge this file's DPH data intervals with the master GTI; note
       that this is different than the code above which checks each
       DPH interval for partial overlap.  This particular merge is
       required to make sure that no master GTIs exist *outside* the
       DPH data intervals.  This prevents a silent failure of
       batbinevt which produces zero counts. */
    if (gti_accum(GTI_AND, &parms->gtimaster, &fumaster, &status) == 0) {
      fprintf(stderr, "ERROR: could not merge data interval GTI\n");
      goto CLEANUP;
    }


    HDgti_free(&dph_gti);
  }
  headas_chat(5,"   (after partial overlap, ngti=%d ndph=%d)\n", 
	      parms->gtimaster.ngti, fumaster.ngti);

  /* No data was found */
  if (parms->gtimaster.ngti == 0) {
    headas_chat(1, "WARNING: no good times were found after intersection with tstart/tstop\n");
    goto CLEANUP;
  }

  /* Now read the 'infile' GTI into the user gti.  At this point, the
     'gtifile' GTI has already been merged into the master GTI, so it
     is no longer needed.  However, the 'infile' GTI is. */
  if (parms->tbinmethod == INFBINS) {
    HDgti_free(&parms->ugti);

    parms->ugti = fumaster;
  }
  HDgti_init(&fumaster);

  /* Find final min/max times .... and place in parms->tmin/max */
  parms->tmin = parms->gtimaster.start[0];
  parms->tmax = parms->gtimaster.stop[0];
  for (i=1; i<parms->gtimaster.ngti; i++) {
    if (parms->gtimaster.start[i] < parms->tmin) {
      parms->tmin = parms->gtimaster.start[i];
    }
    if (parms->gtimaster.stop[i] > parms->tmax) {
      parms->tmax = parms->gtimaster.stop[i];
    }
  }

  headas_chat(5, "  - FINAL MASTER : GTI :\n");
  for (j=0; j<parms->gtimaster.ngti; j++) {
    headas_chat(5, "    %3d. (%f,%f)\n", j+1, 
		parms->gtimaster.start[j], parms->gtimaster.stop[j]);
  }

  headas_chat(5, "     (%f,%f)=(tstart,tstop)\n", 
	      parms->tmin, parms->tmax);
  return status;

 CLEANUP:
  if (infile) {
    int mystatus = 0;
    fits_close_file(infile, &mystatus);
    infile = 0;
  }
  if (parms->ftstart) free(parms->ftstart);
  if (parms->ftstop)  free(parms->ftstop);
  if (dph_expomaster) free(dph_expomaster);
  if (dph_fileindex)  free(dph_fileindex);
  parms->ftstart = 0;
  parms->ftstop = 0;

  return status;
}
