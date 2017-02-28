/* wcs.c - World Coordinate System calculator functions for ftimgcalc */
#include <fitsio.h>
#include <ctype.h>
#include <string.h>

#include "ftimgcalc.h"

#define BUFFER_SIZE 30000

/* 
 * Initialize the WCS names for a given coordinate 
 *
 * struct wcsprm *wcs - WCSLIB parameters for this coordinate system
 * char *colname - name of variable used to refer to this coord 
 * struct coord_struct *coord - output structure, initialized w/ WCS data
 * 
 * RETURNS: 0
 */

int fill_wcs_names(struct wcsprm *wcs, char *colname,
		   struct coord_struct *coord)
{
  int i;
  char ctype[FLEN_CARD], *s;

  for (i=0; i<wcs->naxis; i++) {
    /* Copy coordinate name, but blank out the projection type */
    strcpy(ctype, wcs->ctype[i]);

    /* Find the first non-identifier character (essentially the '-'
       character in 'RA--TAN').  Insert a null at that point so now
       'ctype' contains only the CTYPE name portion, and no spaces or
       projection types. */
    for (s=ctype; *s && isident(*s); s++);
    *s = '\0';

    coord[i].alt[0] = wcs->alt[0];
    coord[i].alt[1] = '\0';  /* Null terminator */

    strcpy(coord[i].ctype, ctype);                   /* CTYPEia */
    strcpy(coord[i].cunit, wcs->cunit[i]);           /* CUNITia */
    strcpy(coord[i].vname, colname);                 /* NAME */
    if (ctype[0] != '\0') {
      /* These only apply if there is a CTYPE name present. */
      sprintf(coord[i].name, "%s.%s", colname, ctype); /* NAME.CTYPE */
      if (wcs->alt[0] != ' ') {                        /* NAME.CTYPE.X (alt coord) */
	sprintf(coord[i].altname, "%s.%s.%c", colname, ctype, wcs->alt[0]);
	sprintf(coord[i].newname, "%s_%s_%c", colname, ctype, wcs->alt[0]);
      } else {
	coord[i].altname[0] = '\0';                    /* NAME.CTYPE */
	sprintf(coord[i].newname, "%s_%s", colname, ctype);
      }
    } else {
      /* No CTYPE name available, just use empty string */
      coord[i].name[0]    = '\0';
      coord[i].altname[0] = '\0';
      coord[i].newname[0] = '\0';
    }
    sprintf(coord[i].pixname, "%s.P%d", colname, i+1);   /* NAME.Pi */
    sprintf(coord[i].newpixname, "%s_P%d", colname, i+1);/*  = NAME_Pi */
    sprintf(coord[i].intname, "%s.X%d", colname, i+1);   /* NAME.Xi */
    sprintf(coord[i].newintname, "%s_X%d", colname, i+1);/*  = NAME_Xi */
    if (i == 0) {
      sprintf(coord[i].natname, "%s.THETA", colname);    /* NAME.THETA */
      sprintf(coord[i].newnatname, "%s_THETA", colname); /*  = NAME_THETA */
    } else if (i == 1) {
      sprintf(coord[i].natname, "%s.PHI", colname);      /* NAME.PHI */
      sprintf(coord[i].newnatname, "%s_PHI", colname);   /*  = NAME_PHI */
    } else {
      coord[i].natname[0] = '\0';
      coord[i].newnatname[0] = '\0';
    }
    
    /* Reset the counters so we default do not create any of the
       coordinate columns */
    coord[i].do_pix = coord[i].do_int = coord[i].do_nat = coord[i].do_cel = 0;
  }

  return 0;
}


/*
 * Search for WCS-like names in the input expression, and mark those
 * WCS axes as processable. 
 *
 * char *expr - expression to be searched
 * struct coord_struct *coord - coordinates to be searched for
 * int naxis - number of coordinates
 *
 * RETURNS: total number of coordinates found
 */
int flag_wcs_names(char *expr, struct coord_struct *coord, int naxis)
{
  int i;
  int ntotal = 0;

  for (i=0; i<naxis; i++) {

    /* NOTE: the "alt" expression must come first since the
       coord[i].name is a substring of coord[i].altname */
    if (coord[i].altname[0] && find_var_expr(expr, coord[i].altname)) {
      coord[i].do_cel = 1;
      ntotal ++;
    } 
    if (coord[i].name[0] && find_var_expr(expr, coord[i].name)) {
      coord[i].do_cel = 1;
      ntotal ++;
    } 
    if (coord[i].pixname[0] && find_var_expr(expr, coord[i].pixname)) {
      coord[i].do_pix = 1;
      ntotal ++;
    } 
    if (coord[i].intname[0] && find_var_expr(expr, coord[i].intname)) {
      coord[i].do_int = 1;
      ntotal ++;
    } 
    if (coord[i].natname[0] && find_var_expr(expr, coord[i].natname)) {
      coord[i].do_nat = 1;      
      ntotal ++;
    }
  }

  return ntotal;
}


/* 
 * Create new image cell, with proper coordinate systems
 *
 * fitsfile *outptr - pointer to existing output table
 * int oldcolnum - column number of base column
 * char *colname - name of new coordinate column
 * char *tform_code - TFORM type for this table cell 
 * int naxis - number of image axes
 * long naxes[naxis] - image dimensions
 * int *colnum - upon return, the new column number
 * char *units - units for this column, or zero 
 * int *status - upon return, status code of routine
 *
 * RETURNS: status code
 */
int make_cell(fitsfile *outptr, int oldcolnum, 
	      char *colname, char tform_code,
	      int naxis, long naxes[], int *colnum,
	      char *units, 
	      int *status)
{
  char tform[FLEN_CARD], keyname[FLEN_CARD];
  long nelt = 1;
  int ncols;
  int i;
  int newcolnum;

  if (*status) return (*status);
  *colnum = 0;
  
  for (i=0; i<naxis; i++) nelt *= naxes[i];
  sprintf(tform, "%ld%c", nelt, tform_code);

  fits_get_num_cols(outptr, &ncols, status);
  newcolnum = ncols+1;
  fits_insert_col(outptr, newcolnum, colname, tform, status);
  fits_write_tdim(outptr, newcolnum, naxis, naxes, status);
  if (units) {
    fits_make_keyn("TUNIT", newcolnum, keyname, status);
    fits_update_key(outptr, TSTRING, keyname, units, 
		    "physical unit of image", status);
  }
  
  /* Copy the cell-image keywords */
  copy_wcs_imcolumn2imcolumn(outptr, outptr, oldcolnum, newcolnum, status);

  if (*status == 0) *colnum = newcolnum;
  return (*status);
}

/* 
 * Copy a buffer of values to the output image cell.  If the table has
 * multiple rows, then the data is copied to the cell in each row.
 *
 * fitsfile *outptr - output table
 * int datatype - cell datatype (must be TDOUBLE)
 * int colnum - column number of table cell
 * long firstelem - starting element number in cell
 * long nelements - number of elements in buffer
 * int nrows - number of rows to write, starting at 1
 * double *array - buffer of at least nelements values
 * int slice - which slice to write 0=axis1; 1=axis2; etc
 * int naxis - number of images axes (slice < naxis)
 * int *status - status code
 *
 * RETURNS: status code
 */
int xfer_cell_data(fitsfile *outptr, int datatype, int colnum,
		   long firstelem, long nelements, int nrows, 
		   double *array, int slice, int naxis, int *status)
{
  double buffer[BUFFER_SIZE];
  int j = slice;
  int i;

  if (*status) return (*status);

  for (i=0; i<nelements; i++) {
    buffer[i] = array[j];
    j += naxis;
  }

  for (i=1; i<= nrows; i++) {
    fits_write_col(outptr, datatype, colnum, i, firstelem, nelements, buffer, status);
  }
  
  return (*status);
}


/* 
 * Create WCS cells in the output table, based on the input table 
 *
 * fitsfile *inptr - input image
 * fitsfile *outptr - output table, new colums will be created
 * int colnum - column number of the base column
 * struct coord_struct *coord - coordinates for this column
 * struct wcsprm *wcs - WCS coordinate parameters
 * int *status - status code
 *
 * RETURNS: status code
 */
int make_wcs_cells(fitsfile *inptr, fitsfile *outptr, int colnum,
		   struct coord_struct *coord, 
		   struct wcsprm *wcs,
		   int *status)
{
  int naxis = wcs->naxis, bitpix;
  long nelt, i;
  long nrows;
  long nbuff = BUFFER_SIZE;
  double *buffer = 0;
  double *pixcrd, *world, *imgcrd, *theta, *phi;
  int *statp;
#define MAXDIM 10
  long naxes[MAXDIM];
  long ipix[] = {1,1,1,1,1,1,1,1,1,1};  /* Pixel coordinate counters */
  int do_pix = 0, do_int = 0, do_nat = 0, do_cel = 0;
  long istart, ioff = 0, joff = 0, ndone;
  int jdim;

  if (*status) return (*status);
  fits_get_img_param(inptr, MAXDIM, &bitpix, &naxis, naxes, status);
  fits_get_num_rows(outptr, &nrows, status);
  if (*status) return (*status);

  /* double((pixcrd + world + imgcrd)*naxis + theta + phi) + int(statp) */
  buffer = (double *) malloc(sizeof(double)*nbuff*(naxis*3+2) +
			     sizeof(int)*nbuff);
  if (buffer == 0) { return (*status = MEMORY_ALLOCATION); }
  pixcrd = buffer;
  imgcrd = pixcrd + (nbuff*naxis);
  world  = imgcrd + (nbuff*naxis);
  theta  = world  + (nbuff*naxis);
  phi    = theta  + (nbuff);
  statp  = (int *)(phi + nbuff);

  /* ===== */
  /* First make the output columns */
  nelt = 1;
  for(i=0; i<naxis; i++) {

    nelt *= naxes[i];

    /* Pixel coordinates */
    if (coord[i].do_pix) {
      make_cell(outptr, colnum, coord[i].newpixname, 'D', 
		naxis, naxes, &(coord[i].pix_col), "pix", status);
      copy_wcs_imcolumn2pixlist(outptr, outptr, 
				colnum, coord[i].pix_col, i+1,
				status);
      do_pix = 1;
    }

    /* Intermediate coordinates - scaled but not projected */
    if (coord[i].do_int) {
      make_cell(outptr, colnum, coord[i].newintname, 'D', 
		naxis, naxes, &(coord[i].int_col), coord[i].cunit, status);
      do_int = 1;
    }

    /* Natural coordinates - THETA and PHI */
    if (coord[i].do_nat) {
      make_cell(outptr, colnum, coord[i].newnatname, 'D', 
		naxis, naxes, &(coord[i].nat_col), "deg", status);
      do_nat = 1;
    }
    
    /* Celestial coordinates */
    if (coord[i].do_cel) {
      make_cell(outptr, colnum, coord[i].newname, 'D', 
		naxis, naxes, &(coord[i].cel_col), coord[i].cunit, status);
      do_cel = 1;
    }
  }

  if (*status) return (*status);

  /* ===== */
  /* Now create the coordinate columns */
  i = 0;       /* Counts total number of pixels processed */
  ioff = 0;    /* Number of pixels completed in the current iteration */
  istart = 0;  /* Pixel position in the output array */
  joff = 0;    /* Position in the pixcrd array */

  for (i=0; i<nelt; i++) {

    /* Add new pixel coordinates for this pixel position */
    for (jdim=0; jdim<naxis; jdim++) {
      pixcrd[joff++] = ipix[jdim];
    }

    /* If we have filled the buffer, or finished completely... */
    ioff ++;
    if (ioff == nbuff || i == (nelt-1)) {
      ndone = ioff;

      /* Pixel coordinates */
      if (do_pix) {
	for (jdim=0; jdim<naxis; jdim++) if (coord[jdim].do_pix) {
	  xfer_cell_data(outptr, TDOUBLE, coord[jdim].pix_col, istart+1, ndone, nrows,
			 pixcrd, jdim, naxis, status);
	}
      }

      /* Now check for WCS (native, intermediate and celestial) coordinates */
      if (do_cel || do_nat || do_int) {

	/* Convert from pixel to celestial+image+natural coordinates */
	*status = wcsp2s(wcs, ndone, naxis, pixcrd,
			imgcrd, phi, theta, world, statp);

	/* Celestial */
	for (jdim=0; jdim<naxis; jdim++) if (coord[jdim].do_cel) {
	  xfer_cell_data(outptr, TDOUBLE, coord[jdim].cel_col, istart+1, ndone, nrows,
			 world, jdim, naxis, status);
	}
	
	/* Intermediate */
	for (jdim=0; jdim<naxis; jdim++) if (coord[jdim].do_int) {
	  xfer_cell_data(outptr, TDOUBLE, coord[jdim].int_col, istart+1, ndone, nrows,
			 imgcrd, jdim, naxis, status);
	}
	
	/* Theta/phi (native) */
	for (jdim=0; jdim<2; jdim++) if (coord[jdim].do_nat) {
	  xfer_cell_data(outptr, TDOUBLE, coord[jdim].nat_col, istart+1, ndone, nrows,
			 (jdim == 0)?(theta):(phi), 0, 1, status);
	}
      }

      istart += ioff;
      ioff = 0;
      joff = 0;
    }
    
    /* Increment the pixel counters.  Start with the most rapidly
       varying dimension, and check for rollover at each dimension. */
    ipix[0]++;
    if (ipix[0] == naxes[0]+1) {
      jdim = 0;
      while (jdim < naxis && ipix[jdim] == naxes[jdim]+1) {
	ipix[jdim] = 1;    /* FITS convention */
	jdim++;
	if (jdim < naxis) ipix[jdim] ++;
      }
    }

  } /* Main pixel-driven loop */

  free(buffer);
  return (*status);
}


/* 
 * Main driver routine.  Creates new columns in the output table
 * corresponding to the WCS coordinate columns in the input image.
 * The expression is re-written to refer to the new columns.
 *
 * fitsfile *inptr - input image
 * fitsfile *outptr - output table, new columns will be created
 * char *expr - expression to be parsed for WCS names (will be altered)
 * char *varname - variable name to check for
 * int *status - status code
 *
 * RETURNS: status code
 */
int make_wcs_images(fitsfile *inptr, fitsfile *outptr, 
		    char *expr,
		    char *varname, int *status)
{
  struct coord_struct coord[10];
  char *header;
  int nkeys, nreject, nwcs;
  struct wcsprm *wcs = 0;
  struct wcsprm wcs_dummy;
  int i;
  int colnum;
  int naxis;

  if (*status) return (*status);

  /* Read NAXIS and be sure it is positive */
  fits_get_img_dim(inptr, &naxis, status);
  if (*status) return (*status);
  if (naxis <= 0) {
    *status = BAD_DIMEN;
    fits_write_errmsg("Image must have at least one dimension");
    return *status;
  }

  /* Extract FITS header */
  fits_hdr2str(inptr, 1, 0, 0, &header, &nkeys, status);
  if (*status) return (*status);

  /* If there was an "error" reading the WCS keywords, we assume there
     are no WCS keywords. */
  *status = wcspih(header, nkeys, 1, 0, &nreject, &nwcs, &wcs);
  if (*status) {
    free(header);
    return (*status = 0);
  }

  /* 
     In the past, WCSLIB did not allocate any WCS structures if there
     are no WCS keywords in the input file at all, even though a
     linear transformation would just work fine.  This should be fixed
     with WCSLIB 4.3.3.  However, just in case it does not always
     work, we allocate a dummy WCS structure which has enough
     information to keep the rest of the computations working, at
     least for pixel coordinates.
  */
  if (nwcs == 0) {
    wcs = &wcs_dummy;
    nwcs = 1;
    wcs->flag = -1;   /* First initialization */
    if (wcsini(1, naxis, wcs)) {
      return (*status = MEMORY_ALLOCATION);
    }
  }

  fits_get_colnum(outptr, CASEINSEN, varname, &colnum, status);
  if (*status) return (*status);

  /* Convert header to WCS stuff - loop through each coordinate
     system */
  for (i=0; i<nwcs; i++) {
    fill_wcs_names(&(wcs[i]), varname, coord);
    flag_wcs_names(expr, coord, naxis);
    make_wcs_cells(inptr, outptr, colnum, coord, &(wcs[i]), status);
    if (*status) break;

    rewrite_expr(expr, coord, naxis);
  }


  /* De-allocate memory */
  if (wcs == &wcs_dummy) {
    /* We were using the dummy automatic variable, just deallocate the
       internal contents of the structure */
    wcsfree(wcs);
  } else if (wcs) {
    /* We were using the list of WCS structures allocated by wcspih(),
       in which case we want to deallocate everything explicitly */
    wcsvfree(&nwcs, &wcs);
  }
  nwcs = 0; 
  wcs = 0;
  free(header);

  return (*status);
}
