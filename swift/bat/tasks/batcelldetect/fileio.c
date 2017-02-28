#include <fitsio.h>
#include <math.h>
#include <string.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batcelldetect.h"
#include "mpfit.h"

/* 
 * batcelldetect
 * File input output routines
 * 
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: fileio.c,v 1.100 2011/04/29 17:15:44 craigm Exp $
 */

struct col_struct { char name[FLEN_CARD]; int status; };

int check_add_col(fitsfile *outfile, char *ttype, char *tform, 
		  char *tunit, void *tnull, char *tdisp, int veclen, 
		  int *colnum, char *comment, char **extracomments,
		  struct col_struct *colnames,
		  int *status);

/* Copy the input catalog to the output, keep only those sources which
   are in the field of view of the image, and make a new list of
   sources. */
int copycat(struct parm_struct *parms, struct detect_struct *detect)
{
  fitsfile *incat = 0, *outcat = 0;
  int status = 0, mystatus = 0;
  int racol = 0, deccol = 0, namecol = 0, idcol = 0, eradcol = 0;
  int typecode = 0;
  long int repeat = 0, width = 0;
  int i, j, k;
  long int nrows, ndel, *rowflag = 0;
  int nxpix, nypix;               /* Number of pixels in image */
  double xpix0, ypix0, ra0, dec0; /* Center of image */
  double xpix1, ypix1, ra1, dec1; /* Point source of interest; FITS 1-based indices */
  int ixpix1, iypix1;             /*                           C 0-based indices */
  double imx1, imy1;
  double *ra = 0, *dec = 0;
  double costh;
  char buffer[FLEN_CARD], *pbuffer;
  char *ra_colname = 0, *dec_colname = 0;

  if ((parms == 0) || (detect == 0)) {
    return NULL_INPUT_PTR;
  }

  if (parms->incatalog[0] == 0) {
    return 0;
  }

  /* If the image does not have an RA/DEC coordinate system, then do
     not bother copying the old catalog, since it will be useless. */
  if (! detect->has_cel_coords) {
    headas_chat(1, "NOTE: input image does not have celestial coordinate\n");
    headas_chat(1, "      system.  Ignoring input catalog\n");
    return 0;
  }

  /* Prepare to use the RA/DEC coordinate system */
  ra_colname = parms->ra_colname; dec_colname = parms->dec_colname;

  /* Open the input file for reading */
  headas_chat(5, "   (input catalog %s)\n", parms->incatalog);
  fits_open_data(&incat, parms->incatalog, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: Unable to open %s for read access\n", 
	    parms->incatalog);
    return status;
  }

  /* Make sure that the input catalog has RA and DEC columns,
     otherwise it is pointless */
  fits_get_colnum(incat, CASEINSEN, ra_colname,  &racol,  &status);
  fits_get_colnum(incat, CASEINSEN, dec_colname, &deccol, &status);
  fits_get_colnum(incat, CASEINSEN, NAME_COLNAME, &namecol, &status);
  headas_chat(5, "   (racol=%d deccol=%d namecol=%d   status=%d)\n",
	      racol, deccol, namecol, status);

  if (status) {
    headas_chat(1, "WARNING: input catalog is required to have the\n");
    headas_chat(1, "         %s, %s and %s columns but did not.\n",
		ra_colname, dec_colname, NAME_COLNAME);
    headas_chat(1, "         Ignoring input catalog.\n");
    status = 0;
    goto CLEANUP;
  }

  fits_write_errmark();
  fits_get_colnum(incat, CASEINSEN, SOURCEID_COLNAME, &idcol, &status);
  fits_clear_errmark();
  if (status) {
    headas_chat(5, "  (input catalog did not have CATNUM column)\n");
    status = 0;
    idcol = 0;
  }

  /* Create the output file, or if it is already open, then use that */
  if (parms->outcat) { 
    headas_chat(5, "   (output catalog already open)\n");
    outcat = parms->outcat;
  } else {
    headas_chat(5, "   (creating output catalog)\n");
    headas_clobberfile(parms->outfile);
    fits_create_file(&outcat, parms->outfile, &status);

    if (status) {
      fprintf(stderr, "ERROR: could not open %s for writing\n", 
	      parms->outfile);
      goto CLEANUP;
    }

  }

  /* Copy input catalog to output */
  headas_chat(5, "   (copying input -> output catalog)\n");
  fits_copy_file(incat, outcat, 1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not copy catalog to %s\n",
	      parms->outfile);
    goto CLEANUP;
  }

  detect->currow = 1;  /* Reset current output row to 1 */

  fits_close_file(incat, &status);
  incat = 0;
  status = 0;

  /* Now work only with the output catalog */
  fits_movabs_hdu(outcat, 2, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not move to table extension in catalog\n");
    goto CLEANUP;
  }

  /* Get table information, and especially the RA/Dec/Name column #s */
  fits_get_num_rows(outcat, &nrows, &status);
  fits_get_colnum(outcat, CASEINSEN, ra_colname,  &racol, &status);
  fits_get_colnum(outcat, CASEINSEN, dec_colname, &deccol, &status);
  fits_get_colnum(outcat, CASEINSEN, NAME_COLNAME, &namecol, &status);
  if (status || (nrows <= 0)) {
    fprintf(stderr,"ERROR: could not read table information from %s\n",
	    parms->outfile);
    goto CLEANUP;
  }

  /* The CATNUM and ERR_RAD columns are optional */
  fits_write_errmark();
  fits_get_colnum(outcat, CASEINSEN, SOURCEID_COLNAME, &idcol, &status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    idcol = 0;
  }

  fits_write_errmark();
  fits_get_colnum(outcat, CASEINSEN, ERR_RAD_COLNAME,  &eradcol, &status);
  fits_clear_errmark();
  if (status) {
    status = 0;
    eradcol = 0;
  }

  /* Check that the NAME column is of the proper dimensions */
  repeat = 0;  typecode = 0;  width = 0;
  fits_get_coltype(outcat, namecol, &typecode, &repeat, &width, &status);
  headas_chat(5, "   (%s column typecode=%d repeat=%d width=%d)\n",
	      SOURCEID_COLNAME, typecode, repeat, width);
  if (status || (typecode != TSTRING) || (repeat == 0)) {
    fprintf(stderr,"ERROR: %s column has wrong type and/or length\n",
	    NAME_COLNAME);
    goto CLEANUP;
  }
  if ((typecode == TSTRING) && (repeat < SRCNAME_LEN)) {
    fits_modify_vector_len(outcat, namecol, SRCNAME_LEN, &status);
    if (status) {
      fprintf(stderr,"ERROR: could not enlarge %s column to expected size\n",
	      NAME_COLNAME);
      goto CLEANUP;
    }
  }

  /* Allocate memory for the RA and DEC columns, also for the flag
     which tells us whether a source is in the field of view or not */

  headas_chat(5, "   (allocating memory %d rows)\n", nrows);
  ra = (double *) malloc(sizeof(double)*nrows);
  dec = (double *) malloc(sizeof(double)*nrows);
  rowflag = (long int *) malloc(sizeof(long int)*nrows);
  if ((ra == 0) || (dec == 0) || (rowflag == 0)) {
    fprintf(stderr, "ERROR: could not allocate memory for %s/%s columns\n",
	    ra_colname, dec_colname);
    status = MEMORY_ALLOCATION;
    goto CLEANUP;
  }
  for (i=0; i<nrows; i++) {
    rowflag[i] = 0;
  }


  /* Read RA/DEC data */
  fits_read_col(outcat, TDOUBLE, racol, 1, 1, nrows, 0, ra, 0, &status);
  fits_read_col(outcat, TDOUBLE, deccol, 1, 1, nrows, 0, dec, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read %s/%s columns\n", 
	    ra_colname, dec_colname);
    goto CLEANUP;
  }

  /* Determine the center of the image */
  nxpix = detect->image->axes[0];
  nypix = detect->image->axes[1];
  xpix0 = nxpix / 2.0 + 1;
  ypix0 = nypix / 2.0 + 1;

  /* Convert this to an RA/DEC pair at the center of the image */
  coco(detect->wcs, detect->altwcs, '.', '@', xpix0, ypix0, &ra0, &dec0, &status);

  while (ra0 < 0)    { ra0 += 360; }
  while (ra0 >= 360) { ra0 -= 360; }
  headas_chat(5, "  (image center pix (x,y)=(%f,%f)  (ra,dec)=(%f,%f)\n",
	      xpix0, ypix0, ra0, dec0);
  
  /* Now scan through each RA/DEC pair, and see if it falls within the
     field of view. */

  j = 0;
  for (i=0; i<nrows; i++) {

    ra1 = ra[i];
    dec1 = dec[i];

    /* Normalize the RA value */
    while (ra1 < 0)    { ra1 += 360.0; }
    while (ra1 >= 360) { ra1 -= 360.0; }

    /* Compute the cosine of the angle between the pointing direction
       and the source.  Since I don't trust the FITSIO coordinate
       conversion when the source is on the opposite side of the
       instrument, I do a preliminary screening. */
    costh = (sin(dec1*DTOR)*sin(dec0*DTOR) + 
	     cos(dec1*DTOR)*cos(dec0*DTOR)*cos((ra1-ra0)*DTOR));
    if (costh < 0.1 ) {
      headas_chat(5,"  ... source %d  (ra,dec)=(%f,%f)                     wrong hemisphere\n",
		  i+1, ra1, dec1);
      continue;
    }

    fits_write_errmark();
    /* xpix1/ypix1 are FITS 1-based indices */
    coco(detect->wcs, detect->altwcs, '@', '.', ra1, dec1, &xpix1, &ypix1, &status);
    fits_clear_errmark();
    /* ixpix1/iypix1 are C 0-based indices */
    ixpix1 = rint(xpix1 - 1);
    iypix1 = rint(ypix1 - 1);

    if (status) {
      headas_chat(5,"  ... source %d  (ra,dec)=(%f,%f)                     coord xform failed\n",
		  i+1, ra1, dec1);
      status = 0;
      continue;
    }

    /* NOTE: pixels are in FITS pixel indexing convention */
    /* Check if the pixel values are out of bounds */
    if ((ixpix1 < 0) || (iypix1 < 0) || (ixpix1 >= nxpix) || (iypix1 >= nypix)) {
      headas_chat(5,"  ... source %d  (ra,dec)=(%f,%f)   pix (x,y)=(%f,%f) not in image\n",
		  i+1, ra1, dec1, xpix1, ypix1);
      continue;
    }

    headas_chat(5,"  ... source %d  (ra,dec)=(%f,%f)   pix (x,y)=(%f,%f) pcode=%f\n",
		i+1, ra1, dec1, xpix1, ypix1, 
		detect->pcode ?(detect->pcode->datap[iypix1][ixpix1]):0);
    /* Finally, check the partial coding mask to see if we are in the
       desired field of view.  If not, then skip this entry.  If there
       is no partial coding map, but the source is within the pixel
       boundaries of the image, then allow the source. 

       This code checks to see if any portion of the source is
       included within the partial coded region.  It tries all nine
       combinations of X +/- R and Y +/- R, where R is the source
       radius.

       Partially detected sources will be handled separately in the
       "null border" stage.
    */
    if (detect->pcode) {
      int found = 0;
      FLOAT **p = detect->pcode->datap;
      double pc = parms->pcodethresh;
      int r = parms->srcwindowrad;
      int ii, jj;

      for (ii=ixpix1-r; ii<=ixpix1+r; ii+=r) {
	for (jj=iypix1-r; jj<=iypix1+r; jj+=r) {
	  if (ii >= 0 && ii < nxpix && jj >= 0 && jj < nypix && 
	      p[jj][ii] >= pc) found = 1;
	  if (found) break;
	}
	if (found) break;
      }

      /* No part of the source touches the partially coded region;
	 skip this source */
      if (!found) continue;
    }

    /* This source passed all our tests!  We should keep it. */
    rowflag[i] = 1;

    detect->sources = addsource(detect->sources, &detect->nsrcfound, 
				&detect->nsrcmax);

    /* Convert "true" celestial to "true" tangent plane coordinates */
    coco(detect->wcs, detect->altwcs, '@', '+', ra1, dec1, 
	 &imx1, &imy1, &status);

    detect->sources[j].precat = 1;  /* Source is previously cataloged */
    detect->sources[j].status = 0;
    detect->sources[j].constraintflags = (FIX_IMX + FIX_IMY); /* Fix RA & DEC */
    detect->sources[j].xsum = xpix1;      /* FITS 1-based indices */
    detect->sources[j].ysum = ypix1;
    detect->sources[j].ra_orig   = ra1;
    detect->sources[j].dec_orig  = dec1;
    detect->sources[j].imx_orig  = imx1;
    detect->sources[j].imy_orig  = imy1;
    detect->sources[j].peakflux = 0.0;
    detect->sources[j].bestflux = 0.0;
    detect->sources[j].centflux = NULL_POS;
    detect->sources[j].xpeak = 0;  /* FITS 1-based indices */
    detect->sources[j].ypeak = 0;
    detect->sources[j].pcode = 0;
    detect->sources[j].err_rad = 0.0;
    detect->sources[j].contamflux = NULL_POS;
    if (detect->pcode) {
      detect->sources[j].pcode = detect->pcode->datap[iypix1][ixpix1];
      
      if (detect->sources[j].pcode < parms->pcodethresh) {
	/* Special case: source centroid is outside the partial coding
	   threshold, but some part of the source region is inside.
	   In that case, we mark the source as "bad" status (with the
	   null border flag).  Thus, the source will be in the
	   catalog, so it will prevent a bright source from being
	   re-discovered.  On the other hand, since the status is
	   marked as bad, the source will not have a PSF fit, and will
	   be removed from the output catalog by default.
	*/
	detect->sources[j].status = ERR_NULLBORDER;
      }
    }

    /* Distortion correction and conversion to pixels */
    {
      /* Make conversion from "true" to "apparent" tangent plane coords */
      if (detect->distmap && detect->dist_corr) {
	distortmap_coco1(detect->distmap, 
			 detect->sources[j].imx_orig, detect->sources[j].imy_orig, 
			 &detect->sources[j].imx, &detect->sources[j].imy,
			 TRUE_TO_APP);
      } else {
	detect->sources[j].imx = detect->sources[j].imx_orig; /* Dummy */
	detect->sources[j].imy = detect->sources[j].imy_orig; /* Dummy */
      }

      /* Make conversion from "apparent" tangent plane coords to pixels */
      coco(detect->wcs, detect->altwcs, '+', '.', 
	   detect->sources[j].imx, detect->sources[j].imy,
	   &(detect->sources[j].xsum), &(detect->sources[j].ysum), &status);
    }

    /* Read the name of the source - avoid buffer overflow */
    for (k=0; k<FLEN_CARD; k++) buffer[k] = ' ';
    pbuffer = &(buffer[0]);
    fits_read_col(outcat, TSTRING, namecol, i+1, 1, 1, 0, &pbuffer, 0, &status);
    if (status) {
      detect->sources[j].name[0] = 0;
      fprintf(stderr, "WARNING: could not read catalog name row %d\n", i+1);
    }

    /* Copy from buffer into place */
    buffer[FLEN_CARD-1] = 0;
    for (k=0; k<SRCNAME_LEN; k++) detect->sources[j].name[k] = ' ';
    strncpy(detect->sources[j].name, buffer, 
	    (SRCNAME_LEN < FLEN_CARD) ? SRCNAME_LEN : FLEN_CARD);
    for (k=SRCNAME_LEN-1; k>=0; k--) {
      if (detect->sources[j].name[k] == ' ') detect->sources[j].name[k] = 0;
      else break;
    }
    
    /* Read source ID */
    if (idcol > 0) {
      fits_read_col(outcat, TINT, idcol, i+1, 1, 1, 0, 
		    &(detect->sources[j].sourceid), 0, &status);
    } else {
      /* No source ID column */
      detect->sources[j].sourceid = -1;
    }

    /* Read source error radius */
    if (eradcol > 0) {
      double nullval = -1.0;
      fits_read_col(outcat, TDOUBLE, eradcol, i+1, 1, 1, &nullval, 
		    &(detect->sources[j].err_rad), 0, &status);
    }
    
    /* If the source position is uncertain, and the user requests,
       then remove the position constraint flags */
    if ( (detect->sources[j].err_rad > 0 && parms->fitpos) ||
	 (parms->posfitwindow > 0) ) {
      detect->sources[j].constraintflags &= ~(FIX_IMX + FIX_IMY);
    }


    j ++;
  }

  /* Go through and figure out the list of rows to delete, and at the
   * same time, add the source to the internal source list. */
  j = 0;
  for (i=0; i<nrows; i++) {
    if (rowflag[i] == 0) rowflag[j++] = i+1;
  }
  ndel = j;

  /* ... and delete them */
  if (ndel > 0) {
    fits_delete_rowlist(outcat, rowflag, ndel, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not remove irrelevant sources from catalog\n");
      goto CLEANUP;
    }
  }

 CLEANUP:
  mystatus = 0;
  if (incat) fits_close_file(incat, &mystatus);
  mystatus = 0;
  if (status) {
    /* Only close output catalog if there was an error.  If everything
       is cool, then we can keep the file open, and it will be used
       later, then closed. */
    if (outcat) fits_close_file(outcat, &mystatus);
    parms->outcat = 0;
  } else {
    parms->outcat = outcat;
  }

  if (ra) free(ra);
  if (dec) free(dec);
  if (rowflag) free(rowflag);
  
  return status;
}

/* 
 * enlargecat 
 * 
 * Replicate the rows of the catalog into a new set of rows.  If
 * carryover=no, then enlargecat will delete the those rows in the new
 * section of the catalog which correspond to newly detected sources.
 * The internal source catalog is also scanned for such sources, which
 * are deleted.  Finally, the conversion from "true" position to
 * "apparent" position is performed.
 *
 */
int enlargecat(struct parm_struct *parms, struct detect_struct *detect, 
	       int nsources, int carryover)
{
  /* Image coordinate systems */
  int i;

  long nrows, naxis1, startrow;
  int status = 0;
  fitsfile *outcat = parms->outcat;
  unsigned char *buffer = 0;
  struct source_struct *sources;

  if (!parms->outcat) { return 0; }
  if (detect->currow <= 1) { return 0; }  /* No rows yet */
  if (nsources < 1) { return 0; }         /* No sources to copy */

  /* Find out how big the table is.  We are going to enlarge it in a
     second */
  fits_read_key(outcat, TLONG, "NAXIS1", &naxis1, 0, &status);
  if (status) return status;

  nrows = nsources;
  startrow = detect->currow - nsources;
  if (startrow < 1) { return BAD_ROW_NUM; }

  /* Buffer for transferring table data */
  buffer = (unsigned char *) malloc( sizeof(unsigned char)*nrows*naxis1 );
  if (buffer == 0) return MEMORY_ALLOCATION;

  /* Read the last NSOURCES rows from the table and copy them to the
     end of the table by appending.  This will be the new table we
     write to for the next iteration. */
  fits_read_tblbytes(outcat, startrow, 1, nrows*naxis1, buffer, &status);
  fits_write_tblbytes(outcat, detect->currow, 1, nrows*naxis1, buffer, 
		      &status);
  fits_set_hdustruc(outcat, &status);  /* Reset internal CFITSIO structures */
  free(buffer);

  /* If we are not carying over the previous newly detected sources, then
     we need to filter them out now. */
  if (carryover == 0) {
    char expr[1024];
    sprintf(expr, "(#ROW < %ld) || ((DETECT_METHOD %% %d) == 0)", 
	    detect->currow, METH_PSF);
    fits_select_rows(outcat, outcat, expr, &status);
    
    i = 0;
    while (i < detect->nsrcfound) {
      if (detect->sources[i].precat == 0) {
	delsource(detect->sources, i, &(detect->nsrcfound), &(detect->nsrcmax));
      } else {
	i++;
      }
    }
    nsources = detect->nsrcfound;
  }


  if (detect->has_cel_coords) {
    double ra1, dec1, imx1, imy1;

    /* Convert celestial coordinates to pixel coordinates */

    /* Recompute image positions of the sources */
    sources = detect->sources;
    for (i=0; i<nsources; i++) {
      if (carryover || sources[i].ra_orig == NULL_POS) {
	/* We do not have an original RA/Dec, so use the detected one */
	ra1 = sources[i].ra_corr;
	dec1 = sources[i].dec_corr;
      } else {
	/* We have an original RA/Dec from the input catalog */
	ra1 = sources[i].ra_orig;
	dec1 = sources[i].dec_orig;
      }

      /* Convert from "true" RA/DEC to "true" IMX/IMY */
      coco(detect->wcs, detect->altwcs, '@', '+', ra1, dec1, 
	   &imx1, &imy1, &status);

      /* Make conversion from "true" to "apparent" tangent plane coords */
      if (detect->distmap && detect->dist_corr) {
	distortmap_coco1(detect->distmap, imx1, imy1, 
			 &detect->sources[i].imx, &detect->sources[i].imy,
			 TRUE_TO_APP);
      } else {
	detect->sources[i].imx = imx1; /* Dummy */
	detect->sources[i].imy = imy1; /* Dummy */
      }


      /* Make conversion from "apparent" tangent plane coords to pixels */
      coco(detect->wcs, detect->altwcs, '+', '.', 
	   detect->sources[i].imx, detect->sources[i].imy,
	   &(detect->sources[i].xsum), &(detect->sources[i].ysum), &status);
      sources[i].xpeak = 0;
      sources[i].ypeak = 0;
    }
  }

  return status;
}

/* Output image to disk */
int imgout(struct parm_struct *parms, 
	   char *outfilename, int imgtype, int *ihdu,
	   struct image_struct *image, fitsfile *infileptr, 
	   int append,
	   int *status)
{
  char creator[FLEN_CARD], extname[FLEN_CARD] = "";
  fitsfile *imgfile;
  int hduclas3_erase = 0;
  FLOAT nullval = -1e38;
  char *comment = "Image";
  /* FITS standard used to forbid EXTNAME in the primary HDU but now
     allows it (version 3.0; released 2008) */
  char *extkey  = "EXTNAME";

  if (status == 0) return NULL_INPUT_PTR;;
  if ((*status) != 0) return (*status);
  if (outfilename == 0) return (*status = FILE_NOT_CREATED);

  switch (imgtype) {
  case SIGNIFMAP: comment = "Significance Map"; break;
  case BKGMAP:    comment = "Mean sky background map"; break;
  case STDDEVMAP: comment = "Sky background standard deviation map"; break;
  default:        comment = "Image"; break;
  }

  /* CREATOR keyword */
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  *ihdu = *ihdu + 1;

  if (!append) {
    headas_clobberfile(outfilename);
    fits_create_file(&imgfile, outfilename, status);
    if (*status != 0) {
      fprintf(stderr, "ERROR: could not create output file %s\n",
	      outfilename);
      return (*status);
    }
  } else {
    fits_open_file(&imgfile, outfilename, READWRITE, status);
    if (*status != 0) {
      fprintf(stderr, "ERROR: could not open output file %s\n",
	      outfilename);
      return (*status);
    }
  }

  /* Strip user-specified number of bits for known file types */
  if (imgtype == BKGMAP || imgtype == STDDEVMAP) {
    image_stripbits(parms->keepbits, image, 0, STRIP_REL, nullval);
  } else if (imgtype == SIGNIFMAP) {
    image_stripbits(parms->keepbits, image, 0, STRIP_VAR, nullval);
  }

  /* Write image data */
  image->nullval = nullval;
  image_write(imgfile, image, status);

  /* Copy keywords from input image */
  image_copykeyclasses(imgfile, infileptr, TYP_WCS_KEY, 0, status);

  fits_update_key(imgfile, TSTRING, "HDUCLASS", "OGIP",
		  "Conforms to OGIP/GSFC standards", status);
  fits_update_key(imgfile, TSTRING, "HDUCLAS1", "IMAGE", 
		  "Contains image data", status);
  if (imgtype == SIGNIFMAP) {
    fits_update_key(imgfile, TSTRING, "HDUCLAS2", "SIGNIFICANCE",
		    "Contains significance map", status);
    fits_update_key(imgfile, TSTRING, "IMATYPE", "SIGNIFICANCE",
		    "Contains significance map", status);
    sprintf(extname, "BAT_SIGNIF_%d", append);
    hduclas3_erase = 1;
  } else if (imgtype == BKGMAP) {
    fits_update_key(imgfile, TSTRING, "HDUCLAS2", "BKG",
		    "Contains mean sky background map", status);
    fits_update_key(imgfile, TSTRING, "IMATYPE", "BACKGROUND",
		    "Contains mean sky background map", status);
    sprintf(extname, "BAT_BKG_%d", append);
    hduclas3_erase = 1;
  } else if (imgtype == STDDEVMAP) {
    fits_update_key(imgfile, TSTRING, "HDUCLAS2", "BKG_STDDEV",
		    "Contains std. deviation map", status);
    fits_update_key(imgfile, TSTRING, "HDUCLAS3", "MEASURED",
		    "Measured standard deviation", status);
    fits_update_key(imgfile, TSTRING, "IMATYPE", "ERROR",
		    "Contains std. deviation map", status);
    sprintf(extname, "BAT_VARMAP_%d", append);
    hduclas3_erase = 0;
  }
  if (extname[0]) {
    fits_update_key(imgfile, TSTRING, extkey, extname, 
		    "Name of extension", status);
  }

  if (hduclas3_erase) {
    /* Erase any stray HDUCLAS3 keywords which leaked through */
    int mystatus = 0;
    fits_write_errmark();
    fits_delete_key(imgfile, "HDUCLAS3", &mystatus);
    fits_clear_errmark();
  }


  /* Write comment and other keywords */
  if (comment && comment[0])
    fits_write_comment(imgfile, comment, status);
  fits_update_key(imgfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", status);

  /* Write time keywords */
  image_writeinfo(imgfile, image, status);

  *status = HDpar_stamp(imgfile, 0, status);

  fits_close_file(imgfile, status);
  if (*status) {
    fprintf(stderr, "WARNING: could not write %s\n", outfilename);
  }
  
  return (*status);
}

/* 
 * check_add_col - check if column is present; if not, then add it
 *
 * This routine checks for the presence of the requested column in the
 * file.  If it exists, then the properties of the column are checked
 * to be sure that they agree with the requested properties.  
 *
 * If the requested vector size is different than the actual vector
 * size, then it is changed to match the requested.
 *
 * If the column does not exist, then it is added to the table.
 *
 * fitsfile *outfile - pointer to FITS file open for writing
 * char *ttype - name of column to be added / checked for
 * char *tform - the TFORM for the column
 * char *tunit - the TUNIT for the column
 * void *tnull - pointer to TNULL value
 * char *tdisp - TDISP value
 * int veclen - the requested vector size of the array
 * int *column - upon output, *colnum contains the name of the column
 * char *comment - TTYPE comment value
 * char **extracomments - null-terminated array of string pointers with extra
 *     COMMENT strings associated with this column
 * struct col_struct *colnames - array of existing column names, 
 *     colnames[i].name - column name
 *     colnames[i].status - 0=no entry, 1=entry, -1=end-of-list
 *    Upon return, if ttype matches an entry in colnames, that entry
 *    status will be changed to 0.
 * int *status - upon output, CFITSIO status value
 *
 * RETURNS: CFITSIO status value
 * 
 */
int check_add_col(fitsfile *outfile, char *ttype, char *tform, 
		  char *tunit, void *tnull, char *tdisp, int veclen, int *colnum, 
		  char *comment, char **extracomments, 
		  struct col_struct *colnames,
		  int *status)
{
  int ncol = 0;
  int typecode;
  char typechar, typechar_desired;
  long repeat, width, nrows;
  char keyname[FLEN_CARD];
  char keyval[FLEN_CARD];
  double keyval_data[4];
  void *keyvalx = &(keyval_data[0]);

  if (veclen == 0) veclen = 1;
  if (status == 0) return NULL_INPUT_PTR;;
  if ((*status) != 0) return (*status);
  if ((outfile == 0) || (ttype == 0) || (colnum == 0)) {
    return (*status = NULL_INPUT_PTR);
  }

  *colnum = 0;
  fits_write_errmark();
  fits_get_colnum(outfile, CASEINSEN, ttype, colnum, status);
  fits_clear_errmark();
  /* headas_chat(5, "  ... column %s colnum=%d status=%d...\n", 
     ttype, *colnum, *status); */

  if (((*status == 0) || (*status == COL_NOT_UNIQUE)) && (*colnum >= 1)) {

    /* ================================     Column exists, check type */
    *status = 0;
    /* headas_chat(5, "  ... %s/%s found (validating) ...\n", ttype, tform); */
    sprintf(keyname, "TFORM%d", *colnum);
    fits_get_coltype(outfile, *colnum, &typecode, &repeat, &width, status);
    fits_read_key(outfile, TSTRING, keyname, keyval, 0, status);
    /* headas_chat(5, "    (TFORM existing=%s requested=%s)\n",
       keyval, tform); */
    typechar = typechar_desired = 0;
    sscanf(tform, "%*d%c", &typechar_desired);
    if (*status == 0) sscanf(tform, "%*d%c", &typechar);

    if ((*status != 0) || (typechar_desired != typechar)) {
      fprintf(stderr, 
	   "ERROR: %s requested and existing columns have different types\n",
	      ttype);
      if (*status == 0) *status = -1;
      return *status;
    }

    /* If vector sizes don't agree, then shrink or grow it */
    if (((typecode != TSTRING) && (veclen != repeat)) ||
	((typecode == TSTRING) && (veclen >  repeat))) {
      fits_modify_vector_len(outfile, *colnum, veclen, status);
      if (*status) {
	fprintf(stderr, "ERROR: %s column dimensions could not be changed\n",
		ttype);
	return *status;
      }
    }

    /* Check that the TUNITs agree */
    if (tunit) {
      sprintf(keyname, "TUNIT%d", *colnum);
      fits_write_errmark();
      fits_read_key(outfile, TSTRING, keyname, keyval, 0, status);
      fits_clear_errmark();
      /* headas_chat(5, "    (TUNIT existing=%s requested=%s)\n",
	 keyval, tunit); */
      if ((*status == 0) && (strcmp(keyval,tunit) != 0)) {
	fprintf(stderr, 
	     "ERROR: requested and existing columns have different TUNITs\n");
 	return (*status = -1);
       } else if (*status != 0) {
 	*status = 0;
 	fprintf(stderr, 
 		"WARNING: column %s does not have a TUNIT value\n"
		"NOTE:    batcelldetect will add a TUNIT value\n", ttype);
 	fits_update_key(outfile, TSTRING, keyname, tunit, 
 			"physical unit of field", status);
      }
    }

    /* Check that the TNULL agrees */
    if (tnull) {
      sprintf(keyname, "TNULL%d", *colnum);
      
      fits_write_errmark();
      fits_read_key(outfile, typecode, keyname, keyvalx, 0, status);
      fits_clear_errmark();

      /* Don't care if the TNULLs don't match, as long as one exists */
      if (*status != 0) {
 	*status = 0;
	fprintf(stderr, 
 		"WARNING: column %s does not have a TNULL value\n"
 		"NOTE:    batcelldetect will add a TNULL value\n", ttype);
 	fits_update_key(outfile, typecode, keyname, tnull, 
 			"data null value", status);
      }	
    }

    /* Add TDISP if one does not already exist */
    if (tdisp) {
      sprintf(keyname, "TDISP%d", *colnum);

      fits_write_errmark();
      fits_read_key(outfile, typecode, keyname, keyval, 0, status);
      fits_clear_errmark();

      /* Not found: add it */
      if (*status) {
	*status = 0;
	fits_update_key(outfile, TSTRING, keyname, tdisp, 
			"Column display format", status);
	if (*status != 0) {
	  fprintf(stderr, 
		  "ERROR: %s keyword could not be updated\n", keyname);
	  return *status;
	}
      }
    }


  } else {


    /* ================================     Column does not exist, create it */

    headas_chat(5, "  ... creating %s/%s ...\n", ttype, tform);
    *status = 0;
    fits_get_num_cols(outfile, &ncol, status);
    if (*status) {
      fprintf(stderr, 
	      "ERROR: could not determine number of columns\n");
      return *status;
    }

    /* Add the column, double check the table sizes */
    *colnum = ncol + 1;
    fits_insert_col(outfile, *colnum, ttype, tform, status);
    fits_get_num_rows(outfile, &nrows, status);
    fits_get_coltype(outfile, *colnum, &typecode, &repeat, &width, status);
    headas_chat(5, "    (nrows=%d column=%d tform=%s typecode=%d repeat=%ld width=%ld)\n",
		nrows, *colnum, tform, typecode, repeat, width);
    if (*status) {
      fprintf(stderr, 
	      "ERROR: could not create column %s\n", ttype);
      return *status;
    }

    /* Update the TTYPE comment */
    if (comment) {
      sprintf(keyname, "TTYPE%d", *colnum);
      fits_update_key(outfile, TSTRING, keyname, ttype, comment, status);
      if (*status) {
	fprintf(stderr, 
		"ERROR: could not write TTYPE comment for column %s\n", ttype);
	return *status;
      }
    }

    /* Add the TUNITs */
    if (tunit) {
      headas_chat(5, "    (tunit=%s)\n", tunit);
      sprintf(keyname, "TUNIT%d", *colnum);
      fits_update_key(outfile, TSTRING, keyname, tunit, 
		      "physical unit of field", status);
      if (*status) {
	fprintf(stderr, 
		"ERROR: could not write TUNIT for column %s\n", ttype);
	return *status;
      }
    }

    /* Add the TNULL */
    if (tnull) {
      sprintf(keyname, "TNULL%d", *colnum);
      fits_update_key(outfile, typecode, keyname, tnull, 
		      "data null value", status);
      if (*status) {
	fprintf(stderr, 
		"ERROR: could not write TNULL for column %s\n", ttype);
	return *status;
      }
    }

    /* Add TDISP */
    if (tdisp) {
      sprintf(keyname, "TDISP%d", *colnum);
      fits_update_key(outfile, TSTRING, keyname, tdisp, 
		      "Column display format", status);
      if (*status) {
	fprintf(stderr, 
		"ERROR: could not write TDISP for column %s\n", ttype);
	return *status;
      }
    }

    if (extracomments) {
      char **p;
      for (p=extracomments; *p; p++) fits_write_comment(outfile, *p, status);
      if (*status) {
	fprintf(stderr, 
		"ERROR: could not write COMMENTS for column %s\n", ttype);
	return *status;
      }
    }
    
    /* NOTE: this call is needed because the structure keywords have
       been changed after data has been written to the
       table. fits_set_hdustruc() will resync CFITSIO's internal
       structures to those in the header. 

       It may appear inefficient to do this call at the end of each
       column, but it is necessary since the following
       fits_write_col_null() can fail if it doesn't recognize that the
       TNULL value has changed.
    */
    fits_set_hdustruc(outfile, status);

    if (nrows > 0) {
      if (typecode == TSTRING) repeat /= width;
      headas_chat(5, "    (writing nulls %dx%d)\n", repeat, nrows);
      fits_write_col_null(outfile, *colnum, 1, 1, repeat*nrows, status);
    }

    if (*status) {
      fprintf(stderr, 
	      "ERROR: could not write keywords for column %s\n", ttype);
      return *status;
    }      
  }

  if (*status == 0) {
    int i;
    /* Find and blank out the column if we found it */
    for (i=0; colnames[i].status >= 0; i++) {
      if (strcasecmp(ttype, colnames[i].name) == 0) {
	colnames[i].status = 0;
      }
    }
  }

  return *status;
}

/* 
 * fits_write_nullrows1 - write TNULLs to all columns in one or more rows
 *
 * fitsfile *fptr - pointer to FITS HDU opened for read/write
 * long int firstrow - first table row to set to null. (firstrow >= 1)
 * long int nrows - total number or rows to set to null. (nrows >= 1)
 * int *status - upon return, *status contains CFITSIO status code
 *
 * RETURNS: CFITSIO status code
 */
int fits_write_nullrows1(fitsfile *fptr, long int firstrow, long int nrows, 
			int *status)
{
  long int ntotrows;
  int ncols, i;
  int typecode = 0;
  long int repeat = 0, width = 0;
  char card[FLEN_CARD+1];
  char keyname[FLEN_CARD];
  int nullstatus;

  if (status == 0) return NULL_INPUT_PTR;
  if (fptr == 0) return (*status = NULL_INPUT_PTR);
  if (*status != 0) return *status;

  if ((firstrow <= 0) || (nrows <= 0)) return (*status = BAD_ROW_NUM);

  fits_get_num_rows(fptr, &ntotrows, status);
  fits_get_num_cols(fptr, &ncols, status);
  if (*status) return *status;

  /* Loop through each column and write nulls */
  for (i=1; i <= ncols; i++) {
    repeat = 0;  typecode = 0;  width = 0;
    fits_get_coltype(fptr, i, &typecode, &repeat, &width, status);
    if (*status) break;


    /* NOTE: data of TSTRING type must not write the total repeat
       count, since the repeat count is the *character* count, not the
       nstring count.  Divide by string width to get number of
       strings. */
    
    if (typecode == TSTRING) repeat /= width;

    /* Check to see that a TNULLn keyword exists */
    nullstatus = 0;
    fits_write_errmark();
    fits_make_keyn("TNULL", i, keyname, &nullstatus);
    fits_read_card(fptr, keyname, card, &nullstatus);
    fits_clear_errmark();

    /* Write NULLs (if the keyword exists) */
    if (nullstatus == 0) {
      fits_write_col_null(fptr, i, firstrow, 1, repeat*nrows, status);
    }

    if (*status) break;
  }
    
  return *status;
}


/* outreg() - output a region file suitable for FV or DS9 with source pos/names
 *
 * char *regionfile - name of output region file
 * char *taskname - name of task
 * char *taskver - task version designator 
 * char *infile - name of input file
 * struct source_struct *sources - array of detected sources
 * int nsources - number of sources in array
 * int *status - upon return, CFITSIO status value
 *
 * RETURNS: CFITSIO status value
 *
 */
int outreg(char *regionfile, char *taskname, char *taskver, char *infile,
	   struct source_struct *sources, int nsources, 
	   int has_cel_coords, char *ctype1, char *ctype2, int *status)
{
  FILE *rfile;
  char datestr[FLEN_CARD] = {0};
  int timeref, i;
  int overwrite = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;

  /* Make a region file if requested */
  if (regionfile == 0) return (*status = NULL_INPUT_PTR);
  while (regionfile[0] == '!') {
    overwrite = 1;
    regionfile ++;
  }
  if (regionfile[0] == 0) return 0;

  headas_clobberfile(regionfile);
  headas_chat(5, "  ...writing region file to %s...\n", regionfile);
  if (!overwrite) {
    rfile = fopen(regionfile, "r");
    if (rfile != 0) {
      fclose(rfile);
      fprintf(stderr, "ERROR: the output region file already exists, did you want to clobber it?\n");
      return (*status = FILE_NOT_OPENED);
    }
  }

  rfile = fopen(regionfile, "w");

  if (rfile == 0) {
    fprintf(stderr, "ERROR: could not open region file %s\n",
	    regionfile);
    return (*status = FILE_NOT_OPENED);
  }

  /* Should print any comments?  Something like this:
     # batcelldetect vX.X  2003 May 26 14:58:21
     but are comments allowed?
  */
  fits_get_system_time(datestr, &timeref, status);
  if ((*status == 0) && (datestr[0])) {
    fprintf(rfile, "# %s v%s  %s %s\n",
	    taskname, taskver, datestr, 
	    timeref ? "(local)" : "(UTC)");
  }
  fprintf(rfile, "# filename: %s\n", infile);
  fprintf(rfile, "global wcs=wcs\n");
  if (has_cel_coords) {
    if (!strncmp(ctype1,"RA",2)   && !strncmp(ctype2,"DEC",3))  
      fprintf(rfile, "fk5\n");
    else if (!strncmp(ctype1,"GLON",4) && !strncmp(ctype2,"GLAT",4)) 
      fprintf(rfile, "galactic\n");
    else 
      fprintf(rfile, "linear\n");
  }
  *status = 0;
  for (i=0; i<nsources; i++) {
    double x, y, r;
    char *color;
    if (sources[i].status < 0) continue;

    if (has_cel_coords) {
      x = sources[i].ra_corr;
      y = sources[i].dec_corr;
      r = 2.0*sources[i].wimx / DTOR;
    } else {
      x = sources[i].imx_corr;
      y = sources[i].imy_corr;
      r = 2.0*sources[i].wimx;
    }

    if (sources[i].snr > 3) {
      color = "red";
    } else {
      color = "blue";
    }
      
    if (sources[i].name[0]) {
      fprintf(rfile, "circle(%fd,%fd,%fd) # text={%s} color=%s select=0 edit=0 move=0\n",
	      x, y, r, sources[i].name, color);
    } else {
      fprintf(rfile, "circle(%fd,%fd,%fd) # color=%s select=0 edit=0 move=0\n",
	      x, y, r, color);
    }	
    
  }
  
  fclose(rfile);
  headas_chat(5, "    (done)\n");

  return *status;
}



/* 
 * catout() - main output routine to write catalog file
 *
 * If the catalog file is open already, then it is used (see
 * parms->outcat).  If the catalog file does not exist, then one is
 * created.  If it exists, it may be modified to include new columns,
 * or modify the dimensions of existing columns.
 *
 * The output catalog is left open at the end.  The caller is
 * responsible for closing the file when finished.
 *
 * struct parm_struct *parms - contains task parameters
 *                             reads: regionfile, taskver, taskname, infile
 *                                    outfile, tstart, tstop, exposure
 *                                    srcwindowrad, bkgwindowrad, nbatdets,
 *                                    batz
 *                             reads/writes: outcat
 * struct detect_struct *detect - source detection results
 *                                reads: nimages, imgnum, wcs, pcode
 * struct source_struct *sources - array of detected sources, to be written
 * int nsources - number of values in sources
 * fitsfile *infileptr - pointer to open input image (keywords are copied)
 *           
 * RETURNS: CFITSIO status
 *   */
int catout(struct parm_struct *parms, struct detect_struct *detect,
	   struct source_struct *sources, int nsources, 
	   fitsfile *infileptr)
{
  int status = 0;
  int i, j;
  fitsfile *outfile;
  int ncols = 0;
  long int currow = 0, nrows = 0;
  double chi2_nu;
  char units[FLEN_CARD] = "count";

  int timecol, tstopcol, expocol, imxcol, imycol, imxwcol, imywcol;
  int ratecol, bkgcol, bkgvarcol, bkgfitcol, bkgcellcol;
  int racol, deccol, npixscol, npixbcol;
  int imxerrcol, imyerrcol, imxpixcol, imypixcol, eradcol, centcol;
  int raerrcol, decerrcol, thetacol, phicol, grmcloncol, grmclatcol;
  int imxwerrcol, imywerrcol, rateerrcol, bkgerrcol;
  int methcol, chi2col, chi2ncol, dofcol, snrcol, ndetcol, batzcol;
  int statuscol, srcradcol, bkgradcol, constcol, pcodecol, namecol, idcol;
  /* int peakcol, peakxcol, peakycol, peaksnrcol, bestsnrcol; */
  /* double peaksnr = 0.0; */
  int centsnrcol;
  int ra_imx_col, dec_imx_col, ra_imy_col, dec_imy_col;
  int contamcol;
  int appcol[MAX_APP_KEYS];

  char tform_name[100];
  char nD[100], nJ[100];    /* TFORM strings for %dD and %dJ */
  long int null_long = -1000;
  long int null_catnum = -1;
  double double_nulval = NULL_POS;
  struct col_struct *colnames = 0;

  /* Current image status */
  int nimages = detect->ngoodimages;  /* Number of images */
  int imgnum;                         /* Current image number (if outvector) */

  char *ra_colname = 0, *dec_colname = 0;

  /* Size of TFORM column for source name */
  sprintf(tform_name, "%dA", SRCNAME_LEN);
  ra_colname = parms->ra_colname;
  dec_colname = parms->dec_colname;

  /* Make a region file if requested */
  if (parms->regionfile[0]) {
    outreg(parms->regionfile, parms->taskname, parms->taskver, parms->infile,
	   sources, nsources, detect->has_cel_coords,
	   detect->wcs[0].ctype[0], detect->wcs[0].ctype[1], &status);
    if (status) {
      fprintf(stderr, "ERROR: output to region file failed\n");
      return status;
    }
  }


  /* 
     attempt to open existing output table.
       - if exists, then open read/write
       - otherwise, create
       - bomb if failure
     for each quantity, check
       - does column exist?
       - if no, then create column
       - write data

     options for
       - modify/append lines
         (reason, first time around, will be modifying existing
         catalog; on second passes, will be doing a 5-min survey pass,
	 so each detection will be a new entry)
  */

  if (parms->outcat) { 
    headas_chat(5, "   (outcat - output catalog already open)\n");
    outfile = parms->outcat;
  } else {

    headas_clobberfile(parms->outfile);
    fits_open_data(&outfile, parms->outfile, READWRITE, &status);

    if (status) {
      char creator[FLEN_CARD];
      char *ttype[] = { "TIME",   "TIME_STOP", NAME_COLNAME,"IMX",    "IMY"};
      char *tform[] = { "1D",     "1D",        "20A",       "1D",     "1D"};
      char *tunit[] = { "s",      "s",            "",       "",       ""};
      char *comments[]={"Image start time",
			"Image stop time",
			"Object Name",
			"IMX Tangent plane coordinate",
			"IMY Tangent plane coordinate"};
      char *extname = "BAT_CATALOG";
      char ttypen[FLEN_CARD];
      
      headas_chat(5, "   (creating output catalog)\n");
      /* CREATOR keyword */
      sprintf(creator, "%s %s", parms->taskname, parms->taskver);
      tform[2] = tform_name;
      status = 0;
      ncols = sizeof(ttype)/sizeof(ttype[0]);
      
      headas_chat(5, "  ... output file was not found (creating it) ...\n");
      headas_clobberfile(parms->outfile);
      fits_create_file(&outfile, parms->outfile, &status);
      if (status) {
	fprintf(stderr, "ERROR: Could not create %s\n", parms->outfile);
	return status;
      }
      fits_create_tbl(outfile, BINARY_TBL, 0, ncols, 
		      ttype, tform, tunit, extname, &status);
      if (status) {
	fprintf(stderr, "ERROR: Could not create source table in %s\n", 
		parms->outfile);
	return status;
      }
      for (i=0; i<ncols; i++) {
	if (comments && comments[i] && comments[i][0]) {
	  fits_make_keyn("TTYPE", i+1, ttypen, &status);
	  fits_modify_comment(outfile, ttypen, comments[i], &status);
	}
      }

      headas_chat(5, "  ... copying keywords ...\n");
      image_copykeys(outfile, infileptr, &status);
      fits_update_key(outfile, TSTRING, "CREATOR", creator,
		      " Program that created this FITS file", &status);
      if (status) {
	fprintf(stderr, "ERROR: Could not copy keywords to %s\n", 
		parms->outfile);
	return status;
      }
      
    }

    /* Establish the current row number, i.e. the row we will start
       writing the next entry to. */
    fits_get_num_rows(outfile, &(detect->currow), &status);
    detect->currow ++;

  }


  if (parms->newsrcind >= 0) {
    fits_update_key(outfile, TINT, "NEWSRCIN", &(parms->newsrcind),
		    "Current new source index number", &status);
  }

  fits_get_num_rows(outfile, &nrows, &status);
  fits_get_num_cols(outfile, &ncols, &status);
  currow = detect->currow;

  /* 
     Construct a list of columns already existing in the input file.
     We will flag any columns that batcelldetect will *not* write to,
     and notify the user that these columns are not being updated.
     The point is to try to avoid the situation where somebody tries
     to interpret a column as batcelldetect-output, when it is really
     not. 

  */
  colnames = (struct col_struct *) malloc(sizeof(struct col_struct)*(ncols+1));
  if (colnames == 0) { 
    status = MEMORY_ALLOCATION;
    return status;
  }
  for (i=0; i<ncols; i++) { colnames[i].status = 0; }
  colnames[ncols].status = -1;  /* Termination */
  /* Make a list of columns, so we account for columns we *don't* change */
  for (i=0; i<ncols && (status == 0 || status == COL_NOT_UNIQUE); i++) {
    int dummy;
    fits_get_colname(outfile, 0, "*", colnames[i].name, &dummy, &status);
    if (status == 0 || status == COL_NOT_UNIQUE) {
      colnames[i].status = 1;
      colnames[i].name[FLEN_CARD-1] = 0; /* for safety */
    }
  }
  /* Expect these errors from fits_get_colname */
  if ((status == COL_NOT_FOUND)||(status == COL_NOT_UNIQUE)) status = 0;

  /* Pre-fill TFORM strings for double and integer values */
  if (parms->outvector) {
    /* Vector output */
    sprintf(nD, "%dD", nimages);
    sprintf(nJ, "%dI", nimages);
    imgnum = detect->imgnum;
  } else {
    /* Non-vector output */
    strcpy(nD, "1D");
    strcpy(nJ, "1I");
    imgnum = 1;
    nimages = 1;
  }


  /* Add or check for desired columns */
  check_add_col(outfile, "TIME", "1D", "s", 0, "F17.6", 0, &timecol, 
		"Start time of detection", 0, colnames, &status);
  check_add_col(outfile, "TIME_STOP", "1D", "s", 0, "F17.6", 0, &tstopcol, 
		"Stop time of detection", 0, colnames, &status);
  check_add_col(outfile, "EXPOSURE", "1D", "s", 0, "F11.3", 0, &expocol, 
		"Image exposure", 0, colnames, &status);
  check_add_col(outfile, SOURCEID_COLNAME, "1J", 0, &null_catnum, 0, 0, &idcol,
		"Source ID number", 0, colnames, &status);
  check_add_col(outfile, NAME_COLNAME, tform_name, 0, 0, 0, SRCNAME_LEN, &namecol,
		"Source ID name", 0, colnames, &status);
  check_add_col(outfile, "IMX",  "1D",  0,  0, "F10.5", 0, &imxcol,  
		"IMX Tangent plane coordinate", 0, colnames, &status);
  check_add_col(outfile, "IMY",  "1D",  0,  0, "F10.5", 0, &imycol,  
		"IMY Tangent plane coordinate", 0, colnames, &status);
  check_add_col(outfile, "IMX_ERR",  "1D",  0,  0, "F8.5", 0, &imxerrcol,  
		"IMX Tangent plane error", 0, colnames, &status);
  check_add_col(outfile, "IMY_ERR",  "1D",  0,  0, "F8.5", 0, &imyerrcol,  
		"IMY Tangent plane error", 0, colnames, &status);
  check_add_col(outfile, "IMXPIX",  "1D", "pixel",  0, "F10.2", 0, &imxpixcol,  
		"Best X Pixel position", 0, colnames, &status);
  check_add_col(outfile, "IMYPIX",  "1D",  "pixel",  0, "F10.2", 0, &imypixcol,  
		"Best Y Pixel position", 0, colnames, &status);
  if (detect->has_cel_coords) {
    char colname[FLEN_CARD];
    check_add_col(outfile, ra_colname,   "1D",  "deg",  0, "F10.4", 0, &racol,  
		  "Source Longitude", 0, colnames, &status);
    check_add_col(outfile, dec_colname,  "1D",  "deg",  0, "F10.4", 0, &deccol,  
		  "Source Latitude", 0, colnames, &status);
    sprintf(colname, "%s_ERR", ra_colname);
    check_add_col(outfile, colname,   "1D",  "deg",  0, "F8.4", 0, &raerrcol, 
		  "Estimated Longitude Error",  0, colnames, &status);
    sprintf(colname, "%s_ERR", dec_colname);
    check_add_col(outfile, colname,  "1D",  "deg",  0, "F8.4", 0, &decerrcol,
		  "Estimated Latitude Error",  0, colnames, &status);
  }
  check_add_col(outfile, "ERR_RAD",  "1D",  "deg",  0, "F8.4", 0, &eradcol,  
		"Estimated position error (1 sigma)", 0, colnames, &status);
  check_add_col(outfile, "IMX_FWHM",  "1D",  0,  0, "F8.5", 0, &imxwcol,  
		"IMX PSF full width at half-max", 0, colnames, &status);
  check_add_col(outfile, "IMY_FWHM",  "1D",  0,  0, "F8.5", 0, &imywcol,
		"IMY PSF full width at half-max", 0, colnames, &status);
  check_add_col(outfile, "IMX_FHWM_ERR",  "1D",  0,  0, "F8.5", 0, &imxwerrcol,  
		"IMX PSF width error", 0, colnames, &status);
  check_add_col(outfile, "IMY_FHWM_ERR",  "1D",  0,  0, "F8.5", 0, &imywerrcol,
		"IMY PSF width error", 0, colnames, &status);
  check_add_col(outfile, "THETA",   "1D",  "deg",  0, "F10.4", 0, &thetacol,
		"BAT FSW co-latitude", 0, colnames, &status);
  check_add_col(outfile, "PHI",     "1D",  "deg",  0, "F10.4", 0, &phicol,
		"BAT FSW longitude", 0, colnames, &status);
  check_add_col(outfile, "GRMCLON", "1D",  "deg",  0, "F10.4", 0, &grmcloncol, 
		"BAT GRMC longitude", 0, colnames, &status);
  check_add_col(outfile, "GRMCLAT", "1D",  "deg",  0, "F10.4", 0, &grmclatcol,
		"BAT GRMC latitude", 0, colnames, &status);

  /* Write RATE column if input image is in rate units */
  if (detect->image->units[0]) strcpy(units, detect->image->units);

  if (is_units_rate(units)) {
    check_add_col(outfile, "RATE", nD, units, 0, "F17.7", nimages, &ratecol,
		  "Fitted source rate", 0, colnames, &status);
    check_add_col(outfile, "RATE_ERR", nD, units, 0, "F13.7", nimages, &rateerrcol, 
		  "Source rate error", 0, colnames, &status);
    check_add_col(outfile, "CENT_RATE", nD, units, 0, "F17.7", nimages, &centcol,
		  "Central pixel value", 0, colnames, &status);
    check_add_col(outfile, "CONTAM_RATE", nD, units, 0, "F17.7", nimages, &contamcol,
		  "Src Contam. rate", 0, colnames, &status);
#ifdef PEAK_STATS
    check_add_col(outfile, "PEAK_RATE", nD, units, 0, "F17.7", nimages, &peakcol,
		  "Peak pixel value", 0, colnames, &status);
#endif
  } else {
    check_add_col(outfile, "COUNTS", nD, units, 0, "F17.7", nimages, &ratecol, 
		  "Fitted source counts", 0, colnames, &status);
    check_add_col(outfile, "COUNTS_ERR", nD, units, 0, "F13.7", nimages, &rateerrcol, 
		  "Source counts error", 0, colnames, &status);
    check_add_col(outfile, "CENT_COUNTS", nD, units, 0, "F17.7", nimages, &centcol,
		  "Central pixel value", 0, colnames, &status);
    check_add_col(outfile, "CONTAM_COUNTS", nD, units, 0, "F17.7", nimages, &contamcol,
		  "Src Contam. counts", 0, colnames, &status);
#ifdef PEAK_STATS
    check_add_col(outfile, "PEAK_COUNTS", nD, units, 0, "F17.7", nimages, &peakcol,
		  "Peak pixel value", 0, colnames, &status);
#endif
  }
  check_add_col(outfile, "BKG", nD, units, 0, "F17.7", nimages, &bkgcol, 
		"Image local background", 0, colnames, &status);
  check_add_col(outfile, "BKG_ERR", nD, units, 0, "F13.7", nimages, &bkgerrcol, 
		"Image local bkg error", 0, colnames, &status);
  check_add_col(outfile, "BKG_VAR", nD, units, 0, "F13.7", nimages, &bkgvarcol, 
		"Image local variance", 0, colnames, &status);
  check_add_col(outfile, "BKG_CELL", nD, units, 0, "F17.7", nimages, &bkgcellcol, 
		"Image background, cell detect", 0, colnames, &status);
  check_add_col(outfile, "BKG_FIT", nD, units, 0, "F17.7", nimages, &bkgfitcol, 
		"Image background, PSF fit", 0, colnames, &status);

  check_add_col(outfile, "SNR", nD, 0, 0, "F10.3", nimages, &snrcol, 
		"Signal to noise ratio of RATE/COUNTS", 0, colnames, &status);
  check_add_col(outfile, "CENT_SNR", nD, 0, 0, "F10.3", nimages, &centsnrcol, 
		"Signal to noise ratio of CENT_RATE", 0, colnames, &status);
#ifdef PEAK_STATS
  check_add_col(outfile, "PEAK_SNR", nD, 0, 0, "F10.3", nimages, &peaksnrcol, 
		"Signal to noise ratio of PEAK_RATE", 0, colnames, &status);

  check_add_col(outfile, "BEST_SNR", nD, 0, 0, "F10.3", nimages, &bestsnrcol, 
		"Best signal to noise ratio estimate", 0, colnames, &status);
  check_add_col(outfile, "PEAK_IMXPIX",  "1I",  "pixel",  0, 0, 0, &peakxcol,  
		"Peak pixel X position", 0, colnames, &status);
  check_add_col(outfile, "PEAK_IMYPIX",  "1I",  "pixel",  0, 0, 0, &peakycol,  
		"Peak pixel Y position", 0, colnames, &status);
#endif

  check_add_col(outfile, "NPIXSOU", "1J", "pixel", &null_long, 0, 0, &npixscol, 
		"Source filter pixels", 0, colnames, &status);
  check_add_col(outfile, "NPIXBKG", "1J", "pixel", &null_long, 0, 0, &npixbcol, 
		"Background filter pixels", 0, colnames, &status);
  { 
    char *extracomments[] = { "DETECT_METHOD (sum of): 1=Cell detection; 2=PSF fit; 4=vector-image pos'n",
			      0  };
    check_add_col(outfile, "DETECT_METHOD", "1J", 0, &null_long, "I3", 0, &methcol, 
		  "Source detection method code", extracomments, colnames, &status);
  }
  check_add_col(outfile, "CHI2", nD, 0, 0, "F12.3", nimages, &chi2col, 
		"Fitted chi-square value", 0, colnames, &status);
  check_add_col(outfile, "DOF",  nD, 0, 0, "F10.1", nimages, &dofcol, 
		"Fit degrees of freedom", 0, colnames, &status);
  check_add_col(outfile, "CHI2_NU", nD, 0, 0, "F10.3", nimages, &chi2ncol, 
		"Normalized chi-square value", 0, colnames, &status);
  {
    char *extracomments[] = { 
      "DETECT_STATUS: 0=OK, -1=MAXSRCPIX, -2=NOPIX, -3=NULLBORDER",
      "DETECT_STATUS: -4=ERRTOOBIG, -5=PSFFAILED", 0 };
    check_add_col(outfile, "DETECT_STATUS","1J", 0, &null_long, "I3", 0, &statuscol, 
		  "Detection status code", extracomments, colnames, &status);
  }
  {
    char *extracomments[] = { 
      "CONSTRAINT_FLAG: (sum of): 1=IMX fixed; 2=IMY fixed;",
      "CONSTRAINT_FLAG: 4=COUNTS/RATE fixed; 8=PSF width fixed;", 0 };
    check_add_col(outfile, "CONSTRAINT_FLAG", "1J", 0, &null_long, "I3", 0, &constcol,
		  "PSF fit constraint flags", extracomments, colnames, &status);
  }
  check_add_col(outfile, "SRC_WINDOW_RAD","1J", "pixel", &null_long, 0, 0, &srcradcol, 
		"Source filter radius", 0, colnames, &status);
  check_add_col(outfile, "BKG_WINDOW_RAD","1J", "pixel", &null_long, 0, 0, &bkgradcol, 
		"Background filter radius", 0, colnames, &status);
  check_add_col(outfile, "NGOODPIX","1J", 0, &null_long, 0, 0, &ndetcol,
		"Number of enabled detectors", 0, colnames, &status);
  check_add_col(outfile, "BAT_ZOBJ","1D", 0, 0, "F10.3", 0, &batzcol,
		"Source height", 0, colnames, &status);
  check_add_col(outfile, "PCODEFR","1D", 0, 0, "F9.5", 0, &pcodecol,
		"Partial coding fraction", 0, colnames, &status);

  /* Image plane coordinate transformation matrix */
  if (detect->has_cel_coords) {
    char colname[FLEN_CARD];
    char comment[FLEN_CARD];
    sprintf(colname, "%s_PER_IMX", detect->wcs[0].lngtyp);
    sprintf(comment, "Change in %s(deg) per unit IMX", detect->wcs[0].lngtyp);
    check_add_col(outfile, colname, "1D", "deg", 0, "F13.5", 0, &ra_imx_col, 
		  comment, 0, colnames, &status);
    sprintf(colname, "%s_PER_IMX", detect->wcs[0].lattyp);  
    sprintf(comment, "Change in %s(deg) per unit IMX", detect->wcs[0].lattyp);
    check_add_col(outfile, colname, "1D", "deg", 0, "F13.5", 0, &dec_imx_col, 
		  comment, 0, colnames, &status);
    sprintf(colname, "%s_PER_IMY", detect->wcs[0].lngtyp);
    sprintf(comment, "Change in %s(deg) per unit IMY", detect->wcs[0].lngtyp);
    check_add_col(outfile, colname, "1D", "deg", 0, "F13.5", 0, &ra_imy_col, 
		  comment, 0, colnames, &status);
    sprintf(colname, "%s_PER_IMY", detect->wcs[0].lattyp);  
    sprintf(comment, "Change in %s(deg) per unit IMY", detect->wcs[0].lattyp);  
    check_add_col(outfile, colname, "1D", "deg", 0, "F13.5", 0, &dec_imy_col, 
		  comment, 0, colnames, &status);
  }

  for (i=0; i<parms->nappkeys; i++) {
    char comm[1024];
    char *tform = 0;
    void *tnull = 0;
    char tform_char[80];
    int veclen = 1;

    if (parms->appkeys[i].dtype == 'C') {
      int len = strlen(parms->appkeys[i].val.str);
      sprintf(tform_char, "%dA", len);
      veclen = len;
    }

    switch(parms->appkeys[i].dtype) {
    case 'C': tform = tform_char; tnull = 0; break;
    case 'L': tform = "1J"; tnull = &null_long; break;
    case 'I': tform = "1J"; tnull = &null_long; break;
    case 'F': tform = "1D"; tnull = 0; break;
    }      

    if (tform) {
      if (parms->appkeys[i].comment[0]) {
	strcpy(comm, parms->appkeys[i].comment);
      } else {
	strcpy(comm, parms->appkeys[i].name);
	strcat(comm, " keyword value");
      }
      check_add_col(outfile, parms->appkeys[i].name, tform, 0, 
		    tnull, 0, veclen, &(appcol[i]), comm, 0, colnames, &status);
    }
  }

  if (status) {
    fprintf(stderr, "ERROR: Could not add columns to output catalog\n");
    return status;
  }

  /* Warn if some columns are being copied, but will not be modified */
  if (colnames) { 
    int first_warn = 0;
    for(i=0; i<ncols; i++) {
      if (colnames[i].status != 0) {
	if (! first_warn) {
	  printf("NOTE: Existing columns listed below were copied unchanged from the input\n");
	  printf("  ");
	  first_warn = 1;
	}
	printf("%s ", colnames[i].name);
      }
    }
    if (first_warn) {
      printf("\n");
    }

    free(colnames); colnames = 0;
  }
    

  headas_chat(5, "   (output catalog rows=%d columns=%d currow=%d)\n",
	      nrows, ncols, currow);
  for (i=0; i<nsources; i++, currow++) {
    char *sptr = 0;

    /* Time related columns */
    fits_write_col(outfile, TDOUBLE, timecol, currow, 1, 1, &(parms->tstart),
		   &status);
    fits_write_col(outfile, TDOUBLE, tstopcol, currow, 1, 1, &(parms->tstop),
		   &status);
    fits_write_col(outfile, TDOUBLE, expocol, currow, 1, 1, &(parms->exposure),
		   &status);
    /* Write catalog ID, or null value if the catalog number is not given */
    if (sources[i].sourceid == null_catnum) {
      fits_write_col_null(outfile, idcol, currow, 1, 1, &status);
    } else {
      fits_write_col(outfile, TINT, idcol, currow, 1, 1, &(sources[i].sourceid),
		     &status);
    }

    /* Position related columns */
    fits_write_col(outfile, TDOUBLE, imxcol, currow, 1, 1, &(sources[i].imx_corr),
		   &status);
    fits_write_col(outfile, TDOUBLE, imycol, currow, 1, 1, &(sources[i].imy_corr),
		   &status);
    fits_write_col(outfile, TDOUBLE, imxerrcol, currow, 1, 1, 
		   &(sources[i].imx_err), &status);
    fits_write_col(outfile, TDOUBLE, imyerrcol, currow, 1, 1, 
		   &(sources[i].imy_err), &status);
    fits_write_col(outfile, TDOUBLE, imxwcol, currow, 1, 1,&(sources[i].wimx),
		   &status);
    fits_write_col(outfile, TDOUBLE, imywcol, currow, 1, 1,&(sources[i].wimy),
		   &status);
    fits_write_col(outfile, TDOUBLE, imxwerrcol, currow, 1, 1,
		   &(sources[i].wimx_err), &status);
    fits_write_col(outfile, TDOUBLE, imywerrcol, currow, 1, 1,
		   &(sources[i].wimy_err), &status);
    fits_write_col(outfile, TDOUBLE, imxpixcol, currow, 1, 1, 
		   &(sources[i].xsum), &status);
    fits_write_col(outfile, TDOUBLE, imypixcol, currow, 1, 1, 
		   &(sources[i].ysum), &status);

#ifdef PEAK_STATS
    fits_write_col(outfile, TINT, peakxcol, currow, 1, 1, 
		   &(sources[i].xpeak), &status);
    fits_write_col(outfile, TINT, peakycol, currow, 1, 1, 
		   &(sources[i].ypeak), &status);
#endif

    sptr = &(sources[i].name[0]);
    fits_write_col(outfile, TSTRING, namecol, currow, 1, 1, &sptr, &status);
    if (detect->has_cel_coords) {
      fits_write_col(outfile, TDOUBLE, racol, currow, 1, 1, &(sources[i].ra_corr),
		     &status);
      fits_write_col(outfile, TDOUBLE, deccol, currow, 1, 1, &(sources[i].dec_corr),
		     &status);
      fits_write_col(outfile, TDOUBLE, raerrcol, currow, 1, 1, 
		     &(sources[i].ra_err), &status);
      fits_write_col(outfile, TDOUBLE, decerrcol, currow, 1, 1, 
		     &(sources[i].dec_err), &status);
    }
      
    fits_write_col(outfile, TDOUBLE, eradcol, currow, 1, 1, 
		   &(sources[i].err_rad), &status);

    /* Auxiliary coordinates */
    fits_write_col(outfile, TDOUBLE, thetacol, currow, 1, 1, 
		   &(sources[i].theta), &status);
    fits_write_col(outfile, TDOUBLE, phicol, currow, 1, 1, 
		   &(sources[i].phi), &status);
    fits_write_col(outfile, TDOUBLE, grmcloncol, currow, 1, 1, 
		   &(sources[i].grmclon), &status);
    fits_write_col(outfile, TDOUBLE, grmclatcol, currow, 1, 1, 
		   &(sources[i].grmclat), &status);


    /* Flux related columns */
    fits_write_col(outfile, TDOUBLE, ratecol, currow, imgnum, 1,
		   &(sources[i].bestflux), &status);
    fits_write_col(outfile, TDOUBLE, rateerrcol, currow, imgnum, 1,
		   &(sources[i].bestflux_err), &status);
    fits_write_col(outfile, TDOUBLE, centcol, currow, imgnum, 1,
		   &(sources[i].centflux), &status);
    fits_write_colnull(outfile, TDOUBLE, contamcol, currow, imgnum, 1,
		   &(sources[i].contamflux), &double_nulval, &status);
#ifdef PEAK_STATS
    fits_write_col(outfile, TDOUBLE, peakcol, currow, imgnum, 1,
		   &(sources[i].peakflux), &status);
#endif

    /* Background/noise related columns */
    fits_write_col(outfile, TDOUBLE, bkgcol, currow, imgnum, 1,
		   &(sources[i].bkgflux), &status);
    fits_write_col(outfile, TDOUBLE, bkgerrcol, currow, imgnum, 1,
		   &(sources[i].ebkgflux), &status);
    fits_write_col(outfile, TDOUBLE, bkgvarcol, currow, imgnum, 1,
		   &(sources[i].bkgvar), &status);
    fits_write_colnull(outfile, TDOUBLE, bkgcellcol, currow, imgnum, 1,
		   &(sources[i].bkgflux_cell), &double_nulval, &status);
    fits_write_colnull(outfile, TDOUBLE, bkgfitcol, currow, imgnum, 1,
		   &(sources[i].bkgflux_fit), &double_nulval, &status);

    /* Signal to noise columns */
    fits_write_col(outfile, TDOUBLE, snrcol, currow, imgnum, 1,
		   &(sources[i].snr), &status);
    fits_write_col(outfile, TDOUBLE, centsnrcol, currow, imgnum, 1,
		   &(sources[i].centsnr), &status);
#ifdef PEAK_STATS
    peaksnr = 0.0;
    if (sources[i].bkgvar > 0) { peaksnr = sources[i].peakflux / sources[i].bkgvar; }
    fits_write_col(outfile, TDOUBLE, peaksnrcol, currow, imgnum, 1,
		   &(peaksnr), &status);
#endif

    /* Current best estimate of signal to noise is CENT_RATE */
#ifdef PEAK_STATS
    fits_write_col(outfile, TDOUBLE, bestsnrcol, currow, imgnum, 1,
		   &(sources[i].centsnr), &status);
#endif

    /* Status related keywords */
    fits_write_col(outfile, TINT, npixscol, currow, 1, 1,
		   &(sources[i].npix), &status);
    fits_write_col(outfile, TDOUBLE, npixbcol, currow, 1, 1,
		   &(sources[i].nbkgpix), &status);
    fits_write_col(outfile, TINT, methcol, currow, 1, 1,
		   &(sources[i].method), &status);
    fits_write_col(outfile, TDOUBLE, chi2col, currow, imgnum, 1,
		   &(sources[i].chi2), &status);
    fits_write_col(outfile, TDOUBLE, dofcol, currow, imgnum, 1,
		   &(sources[i].dof), &status);
    chi2_nu = 0;
    if (sources[i].dof > 0) chi2_nu = sources[i].chi2 / sources[i].dof;

    fits_write_col(outfile, TDOUBLE, chi2ncol, currow, imgnum, 1, 
		   &chi2_nu, &status);
    fits_write_col(outfile, TINT, statuscol, currow, 1, 1,
		   &(sources[i].status), &status);
    fits_write_col(outfile, TINT, constcol, currow, 1, 1, 
		   &(sources[i].constraintflags), &status);
    fits_write_col(outfile, TINT, srcradcol, currow, 1, 1,
		   &(parms->srcwindowrad), &status);
    fits_write_col(outfile, TINT, bkgradcol, currow, 1, 1,
		   &(parms->bkgwindowrad), &status);
    fits_write_col(outfile, TINT, ndetcol, currow, 1, 1,
		   &(parms->nbatdets), &status);
    fits_write_col(outfile, TDOUBLE, batzcol, currow, 1, 1, 
		   &(parms->batz), &status);
    fits_write_col(outfile, TDOUBLE, pcodecol, currow, 1, 1, 
		   &(sources[i].pcode), &status);

    if (detect->has_cel_coords) {
      /* Image transformation columns */
      fits_write_col(outfile, TDOUBLE, ra_imx_col, currow, 1, 1, 
		     &(sources[i].ra_per_imx), &status);
      fits_write_col(outfile, TDOUBLE, dec_imx_col, currow, 1, 1, 
		     &(sources[i].dec_per_imx), &status);
      fits_write_col(outfile, TDOUBLE, ra_imy_col, currow, 1, 1, 
		     &(sources[i].ra_per_imy), &status);
      fits_write_col(outfile, TDOUBLE, dec_imy_col, currow, 1, 1, 
		     &(sources[i].dec_per_imy), &status);
    }

    for (j=0; j<parms->nappkeys; j++) {
      char *strptr = parms->appkeys[j].val.str;
      switch (parms->appkeys[j].dtype) {
      case 'C': 
	fits_write_col(outfile, TSTRING, appcol[j], currow, 1, 1, 
		       &strptr, &status);
	break;
      case 'L': 
	fits_write_col(outfile, TINT, appcol[j], currow, 1, 1, 
		       &(parms->appkeys[j].val.bool), &status);
	break;
      case 'I': 
	fits_write_col(outfile, TLONG, appcol[j], currow, 1, 1, 
		       &(parms->appkeys[j].val.intv), &status);
	break;
      case 'F': 
	fits_write_col(outfile, TDOUBLE, appcol[j], currow, 1, 1, 
		       &(parms->appkeys[j].val.floatv), &status);
	break;
      }
    }
      
    if (status) {
      fprintf(stderr, "ERROR: could not write data to source catalog\n");
      return status;
    }
  }

  /* Save output file pointer in outfile.  User is responsible for
     closing the file */
  parms->outcat = outfile;

  /* Save current row number since we may come back and append more
     rows to the end of the catalog table */
  if (parms->outvector == 0) {
    detect->currow = currow;
  }

  /* Make sure the table structure is sane, so that other CFITSIO
     calls don't go wacko!!! */
  fits_set_hdustruc(outfile, &status);
  fits_get_num_rows(outfile, &nrows, &status);
  fits_get_num_cols(outfile, &ncols, &status);
  headas_chat(5, "   (output catalog rows=%d columns=%d currow=%d)\n",
	      nrows, ncols, currow);

  if (colnames) { free(colnames); }
  return status;
}

int print_source_table(char *sort_expr, 
		       struct detect_struct *detect,
		       struct source_struct *sources, int nsources,
		       int *status)
{
  int *ps = 0;
  int i, ii;

  if (*status) return (*status);
  parse_sort_expression(sort_expr, sources, nsources, &ps, status);
  if (*status) return (*status);


  /* Write some output to the console */
  if (detect->has_cel_coords) {
    char racent[256], deccent[256];
    sprintf(racent,"%scent", detect->wcs[0].lngtyp);
    sprintf(deccent,"%scent", detect->wcs[0].lattyp);
    headas_chat(1, "# %8s %8s %6s  %5s %6s  %11s %7s  %s\n",
		racent, deccent, "POSerr", "Theta", "Phi",
		"Peak Cts", "SNR", "Name");
    for (ii=0; ii<nsources; ii++) {
      i = ps[ii];
      if (sources[i].status < 0) continue;
      headas_chat(1, "  %8.4f %+8.4f %6.4f  %5.1f %6.1f  %11.4f %7.1f  %s\n",
		  sources[i].ra_corr, sources[i].dec_corr, sources[i].err_rad,
		  sources[i].theta, sources[i].phi,
		  sources[i].bestflux, sources[i].snr,
		  sources[i].name);
    }
  } else {
    headas_chat(1, "# %8s %8s   %8s %8s  %11s %9s %7s\n",
		"IMXcent", "IMYcent", "IMXwid", "IMYwid", 
		"Peak Cts", "Bkg Var", "SNR");
    for (ii=0; ii<nsources; ii++) {
      i = ps[ii];
      if (sources[i].status < 0) continue;
      headas_chat(1, "  %8.5f %8.5f   %8.5f %8.5f  %11.4f %9.4f %7.1f\n",
		  sources[i].imx_corr, sources[i].imy_corr,
		  sources[i].wimx, sources[i].wimy,
		  sources[i].bestflux, sources[i].bkgvar, sources[i].snr);
    }
  }

  free(ps);
  return 0;
}

