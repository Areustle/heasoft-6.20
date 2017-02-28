#include <fitsio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include "imageutils.h"
#include "batfftsg.h"

/*
 * Basic Image manipulate routines
 *   * allocation
 *   * I/O to FITS file
 *   * rescale and rebalance
 *   * write WCS-related keywords
 *
 * C. Markwardt
 *
 * 16 Dec 2002 - image_write() Add extra keyword space for images (CM)
 * 17 Jan 2003 - remove image_fft() to its own file, imagefft.c (CM)
 * 
 */

/* $Id: imageutils.c,v 1.35 2006/04/05 16:59:35 craigm Exp $ */

/* 
 * Free all data associated with an Image structure.
 *
 * struct image_struct *image - image to be freed
 */
void image_free(struct image_struct *image)
{
  if (image == 0) return;
  if (image->t) free(image->t);
  if (image->w) free(image->w);
  if (image->ip) free(image->ip);
  if (image->datap) free(image->datap);
  if (image->flags & IMGFLAG_MALLOCED) free(image->data);
  free(image);
}

/* 
 * Allocate an image structure 
 * 
 * int nx - number of image columns
 * int ny - number of image rows
 *
 * FLOAT *data - pre-allocated image data values, or 0 if image_init()
 *               is to allocate the memory.  If data is passed, then
 *               the user is responsible for free-ing it.
 * RETURNS: Newly allocated image.  User responsible to free image.
 *          Returns 0 upon failure.  The image values are zeroed.
 */
struct image_struct * image_init(int nx, int ny, FLOAT *data)
{
  int i;
  struct image_struct *image;
  int allocated = 0;

  if ((nx <= 0) || (ny <= 0)) { 
    return 0;
  }

  /* Allocate and initialize raw storage if needed */
  if (data == 0) {
    data = (FLOAT *) malloc(sizeof(FLOAT)*nx*ny);
    if (data == 0) {
      fprintf(stderr, "ERROR: unable to allocate memory for image array\n");
      return 0;
    }
    for (i=0; i<(nx*ny); i++) {
      data[i] = 0;
    }
    allocated = 1;
  }

  /* Alocate image_struct */
  image = (struct image_struct *) malloc(sizeof(struct image_struct));
  if (image == 0) {
    if (allocated) free(data);
    fprintf(stderr, "ERROR: unable to allocate image structure\n");
    return 0;
  }

  /* Initialize structure */
  image->naxis = 2;
  image->bitpix = 0;
  image->axes[0] = nx;
  image->axes[1] = ny;
  image->data = data;
  image->t = 0;
  image->w = 0;
  image->datap = 0;
  image->nip = 0;
  image->ip = 0;
  image->flags = (allocated ? IMGFLAG_MALLOCED : 0);
  image->units[0] = 0;  /* Null units string by default */
  image->nullval = 0;
  image->tstart = -1e307;
  image->tstop = -1e307;
  image->exposure = -1e307;
  image->telapse = -1e307;
  image->ontime = -1e307;

  /* Auxiliary arrays */
  image->nip = 2 + floor(sqrt(nx+ny)) + 1;
  image->t = (FLOAT *) malloc(sizeof(FLOAT)*8*ny);
  image->w = (FLOAT *) malloc(sizeof(FLOAT)*(nx+ny));
  image->ip = (int *) malloc(sizeof(int)*image->nip);
  image->ip[0] = 0;  /* Signals FFT routine to initialize */

  /* Pointer array */
  image->datap = (FLOAT **) malloc(sizeof(FLOAT *)*ny);
  if ((image->t == 0) || (image->w == 0) || (image->ip == 0) || 
      (image->datap == 0)) {
    image_free(image);
    fprintf(stderr, "ERROR: Could not allocate memory for image data\n");
    return 0;
  }
  for (i=0; i<ny; i++) {
    image->datap[i] = image->data + i*nx;
  }

  return image;
}

/* 
 * Read image from FITS file (current image extension)
 *
 * fitsfile *inptr - FITS file to read image data from (must be an
 *                   image extension), already opened
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: image read from FITS file.  User responsible to free image.
 *          Returns 0 if image was not readable.
 */
struct image_struct * image_read(fitsfile *inptr, int *status)
{
  return image_read_i(inptr, -1, 0, 0, 0, status);
}

/* 
 * Read ith image from FITS file
 *
 * fitsfile *inptr - FITS file to read image data from (may contain either
 *                   image data or table data)
 * int imgnum - image number to read (from 1 to image_nimages())
 *              (or -1 to indicate the current extension)
 * char *colname - if inptr is a table, the column name to read from.
 *                 if colname is 0, then the first 2D column is read.
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: image read from FITS file.  User responsible to free image.
 *          Returns 0 if image was not readable.
 */
struct image_struct * image_read_i(fitsfile *inptr, int imgnum, 
				   char *colname, 
				   FLOAT *nullval, int *anynull,
				   int *status)
{
  struct image_struct *image = 0;
  int i, j;
  long int fpixel[] = {1,1,1,1,1,1,1,1,1,1};
  int nhdu = -1;
  int hdutype, colnum, ncols;
  int naxis, typecode, bitpix;
  long int nimages, repeat, width, naxes[2];
  int found_image_hdu = 0;
  int curhdu, istart, hdugoal;
  int imgnum0;

  if (status == 0) return 0;
  if (inptr == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }

  imgnum0 = imgnum;

  *status = 0;
  fits_get_num_hdus(inptr, &nhdu, status);
  fits_get_hdu_num(inptr, &curhdu);
  if ((nhdu <= 0) || (curhdu < 0) || (*status != 0)) {
    if (*status == 0) *status = BAD_HDU_NUM;
    return 0;
  }

  if (imgnum == -1) { 
    istart = curhdu;
    hdugoal = curhdu;
  } else {
    istart = 1;
    hdugoal = imgnum;
  }

  nimages = 0;
  for (i=istart; i<= nhdu; i++) {
    fits_movabs_hdu(inptr, i, 0, status);
    fits_get_hdu_type(inptr, &hdutype, status);
    if (*status) return 0;
    
    if (hdutype == IMAGE_HDU) {

      /* =================================================================== */
      /* It's an image, so count it once */
      fits_get_img_param(inptr, 2, &bitpix, &naxis, naxes, status);
      if (*status) continue;
      if ((naxis < 2) || (naxes[0] == 0) || (naxes[1] == 0)) continue;

      nimages ++;
      found_image_hdu = 1;
      if ((nimages == hdugoal) || (imgnum0 == -1)) {
	/* This is the image we want!! */

	image = image_init(naxes[0], naxes[1], 0);
	if (image == 0) {
	  *status = MEMORY_ALLOCATION;
	  return 0;
	}
	image->bitpix = bitpix;
	fits_read_pix(inptr, TFLTTYPE, fpixel, naxes[0]*naxes[1], nullval, 
		      image->data, anynull, status);
	if (nullval) { image->nullval = *nullval; }
	if (*status) {
	  image_free(image);
	  image = 0;
	}

	/* Read BUNIT keyword, get units.  If the keyword does not
           exist, then store the empty string. */
	{
	  int mystatus = 0;
	  image->units[0] = 0;
	  fits_write_errmark();
	  fits_read_key(inptr, TSTRING, "BUNIT", image->units, 0, &mystatus);
	  fits_clear_errmark();
	}

	/* Read time and exposure keywords */
	{
	  int status1 = 0, status2 = 0, status3 = 0, status4 = 0, status5 = 0;
	  fits_write_errmark();
	  image->tstart = -1e307;
	  image->tstop = -1e307;
	  image->exposure = -1e307;
	  image->telapse = -1e307;
	  image->ontime = -1e307;

	  fits_read_key(inptr, TDOUBLE, "TSTART", &(image->tstart), 0, &status1);
	  fits_read_key(inptr, TDOUBLE, "TSTOP", &(image->tstop), 0, &status2);
	  fits_read_key(inptr, TDOUBLE, "EXPOSURE", &(image->exposure), 0, &status3);
	  fits_read_key(inptr, TDOUBLE, "TELAPSE", &(image->telapse), 0, &status4);
	  fits_read_key(inptr, TDOUBLE, "ONTIME", &(image->ontime), 0, &status5);
	  fits_clear_errmark();

	  /* XXX - remember this code appears twice ! */
	  if (status4 && !status1 && !status2) {
	    image->telapse = image->tstop - image->tstart;
	    status4 = 0;
	  }
	  if (status5 && !status4) {
	    image->ontime = image->telapse;
	    status5 = 0;
	  }
	  if (status3 && !status5) {
	    image->exposure = image->ontime;
	    status3 = 0;
	  }
	  if (status5 && !status3) {
	    image->ontime = image->exposure;
	    status5 = 0;
	  }
	  if (status4 && !status5) {
	    image->telapse = image->ontime;
	    status4 = 0;
	  }
	  if (status2 && !status1 && !status3 && (image->telapse > 0)) {
	    image->tstop = image->tstart + image->telapse;
	  }
	  
	}

	/* Break out of the loop and return the image */
	break;

      }
    } else if (found_image_hdu == 0) {

      /* =================================================================== */
      /* This is a binary table, so we will stop right here, read the
         required row, and return that */

      /* This is if we came in with imgnum == -1 */
      if (imgnum < 1) { imgnum = 1; }

      colnum = 0;
      if (colname) {
	/* We know the column name, so find the column number */
	fits_get_colnum(inptr, CASEINSEN, colname, &colnum, status);
      } else {

	/* DEFAULT: Column name is unknown, so we take the first
           column which is a two dimensional array */
	fits_get_num_cols(inptr, &ncols, status);
	if (*status) return 0;

	for (j=1; j<=ncols; j++) {
	  fits_read_tdim(inptr, j, 2, &naxis, naxes, status);
	  if ((*status) || (naxis < 2) || (naxes[0] == 0) || (naxes[1] == 0)) 
	    continue;
	  colnum = j;
	}
      }
      if (colnum == 0) {
	fprintf(stderr, "ERROR: Could not find 2-d image column\n");
	return 0;
      }

      /* Find out the number of rows, columns in table, and in image */
      fits_get_num_rows(inptr, &nimages, status);
      fits_read_tdim(inptr, colnum, 2, &naxis, naxes, status);
      fits_get_coltype(inptr, colnum, &typecode, &repeat, &width, status);
      if (*status) return 0;
      if ((imgnum <= 0) || (imgnum > nimages)) {
	fprintf(stderr, "ERROR: requested image number %d does not exist\n", 
		imgnum);
	return 0;
      }

      image = image_init(naxes[0], naxes[1], 0);
      if (image == 0) {
	*status = MEMORY_ALLOCATION;
	return 0;
      }
      image->bitpix = typecode;
      
      fits_read_col(inptr, TFLTTYPE, colnum, imgnum, 1, naxes[0]*naxes[1], 
		    nullval, image->data, anynull, status);
      if (nullval) { image->nullval = *nullval; }
      if (*status) {
	image_free(image);
	image = 0;
      }

      {
	char keyname[FLEN_CARD];
	int mystatus = 0;

	/* Read TUNITn keyword, get units */
	fits_write_errmark();
	image->units[0] = 0;
	fits_make_keyn("TUNIT", colnum, keyname, &mystatus);
	fits_read_key(inptr, TSTRING, keyname, image->units, 0, &mystatus);
	fits_clear_errmark();
      }


      /* Read time and exposure keywords */
      {
	char keyname[FLEN_CARD];
	int mystatus = 0, status1 = 0, status2 = 0, status3 = 0, 
	  status4 = 0, status5 = 0;
	double tnullval = -1e307;

	image->tstart = -1e307;
	image->tstop = -1e307;
	image->exposure = -1e307;
	image->telapse = -1e307;
	image->ontime = -1e307;
	  
	fits_write_errmark();
	if (fits_get_colnum(inptr, CASEINSEN, "TIME", &colnum, &status1) == 0) {
	  fits_read_col(inptr, TDOUBLE, colnum, imgnum, 1, 1, &tnullval,
			&(image->tstart), 0, &status1);
	}
	if (fits_get_colnum(inptr, CASEINSEN, "TIME_STOP", &colnum, &status2) == 0) {
	  fits_read_col(inptr, TDOUBLE, colnum, imgnum, 1, 1, &tnullval,
			&(image->tstop), 0, &status2);
	}
	if (fits_get_colnum(inptr, CASEINSEN, "EXPOSURE", &colnum, &status3) == 0) {
	  fits_read_col(inptr, TDOUBLE, colnum, imgnum, 1, 1, &tnullval,
			&(image->exposure), 0, &status3);
	}
	if (fits_get_colnum(inptr, CASEINSEN, "TELAPSE", &colnum, &status4) == 0) {
	  fits_read_col(inptr, TDOUBLE, colnum, imgnum, 1, 1, &tnullval,
			&(image->telapse), 0, &status4);
	}
	if (fits_get_colnum(inptr, CASEINSEN, "ONTIME", &colnum, &status5) == 0) {
	  fits_read_col(inptr, TDOUBLE, colnum, imgnum, 1, 1, &tnullval,
			&(image->ontime), 0, &status5);
	}

	  /* XXX - remember this code appears twice ! */
	  if (status4 && !status1 && !status2) {
	    image->telapse = image->tstop - image->tstart;
	    status4 = 0;
	  }
	  if (status5 && !status4) {
	    image->ontime = image->telapse;
	    status5 = 0;
	  }
	  if (status3 && !status5) {
	    image->exposure = image->ontime;
	    status3 = 0;
	  }
	  if (status5 && !status3) {
	    image->ontime = image->exposure;
	    status5 = 0;
	  }
	  if (status4 && !status5) {
	    image->telapse = image->ontime;
	    status4 = 0;
	  }
	  if (status2 && !status1 && !status3 && (image->telapse > 0)) {
	    image->tstop = image->tstart + image->telapse;
	  }

	image->units[0] = 0;
	fits_make_keyn("TUNIT", colnum, keyname, &mystatus);
	fits_read_key(inptr, TSTRING, keyname, image->units, 0, &mystatus);
	fits_clear_errmark();
      }


      break;
    }
			  
  }

  return image;
}

/* 
 * Compute number of images in input FITS file
 *
 * The file is considered to either contain multiple 2D image
 * extensions, in which case, the number of image extensions is
 * returned; or the file may contain a FITS binary table with a 2D
 * image column, in which case the number of rows is returned.  The
 * first extension to be found which matches determines which type of
 * file it is.  It is not supported to mix the two types.
 *
 * File pointer may be positioned at any extension, but the count
 * starts at the primary extension no matter what.  Upon successful
 * return, the file pointer should still be positioned at the original
 * extension.
 *
 * fitsfile *inptr - FITS file to count images from.  
 *
 * int *hdutype0 - upon return, *hdutype0 contains the type of the
 *                 extension which matched, either BINARY_TBL or
 *                 IMAGE_HDU.  If hdutype0 is 0, then it is ignored.
 *
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: number of successfully matched images in the file.
 */
int image_nimages(fitsfile *inptr, int *hdutype0, int *status)
{
  int i;
  int hdutype, naxis, bitpix;
  long int nimages, naxes[2];
  int savehdu = -1, nhdu = -1, firsthdu = -1;
  int found_image_hdu = 0;

  if (status == 0) return 0;
  if (inptr == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }

  *status = 0;
  fits_get_num_hdus(inptr, &nhdu, status);
  fits_get_hdu_num(inptr, &savehdu);
  if ((savehdu < 0) || (nhdu <= 0) || (*status != 0)) {
    if (*status == 0) *status = BAD_HDU_NUM;
    return 0;
  }

  nimages = 0;
  for (i=1; i<= nhdu; i++) {
    fits_movabs_hdu(inptr, i, 0, status);
    fits_get_hdu_type(inptr, &hdutype, status);
    if (*status) return 0;
    
    if (hdutype == IMAGE_HDU) {
      /* It's an image, so count it once */
      fits_get_img_param(inptr, 2, &bitpix, &naxis, naxes, status);
      if (*status) continue;
      if ((naxis < 2) || (naxes[0] == 0) || (naxes[1] == 0)) continue;

      if (hdutype0) *hdutype0 = hdutype;
      firsthdu = i;
      found_image_hdu = 1;
      nimages ++;
    } else if (found_image_hdu == 0) {
      /* This is a binary table, so we will stop right here, find the
         number of rows, and return that. */
      nimages = 0;
      firsthdu = i;
      if (hdutype0) *hdutype0 = hdutype;
      fits_get_num_rows(inptr, &nimages, status);
      break;
    }
			  
  }

  /* Return to starting HDU number */
  fits_movabs_hdu(inptr, savehdu, 0, status);
  return nimages;
}

/* 
 * Copy all the user keywords (not the structural keywords)
 *
 * fitsfile *outptr - FITS file pointer, opened for writing
 * fitsfile *headfile - pointer to open FITS file from which keys are taken
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: CFITSIO status variable
 */
int image_copykeys(fitsfile *outptr, fitsfile *headfile, int *status)
{
  return image_copykeyclasses(outptr, headfile, TYP_REFSYS_KEY, 0, status);
}

/* 
 * Copy all the requested level of keywords
 *
 * fitsfile *outptr - FITS file pointer, opened for writing
 * fitsfile *headfile - pointer to open FITS file from which keys are taken
 * int class - keywords with class value >= class are copied
 *             (see documentation for fits_get_keyclass)
 * int copycomments - copy comments?  1=yes, 0=no
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: CFITSIO status variable
 */
int image_copykeyclasses(fitsfile *outptr, fitsfile *inptr, 
			 int class, int copycomments, int *status)
{
  int i, nkeys, copy;
  char card[FLEN_CARD];
  
  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (inptr == 0) return (*status = NULL_INPUT_PTR);

  fits_get_hdrspace(inptr, &nkeys, NULL, status);
  if (*status) return *status;
  
  for (i = 1; i <= nkeys; i++) {
    fits_read_record(inptr, i, card, status);
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if (fits_get_keyclass(card) >= class) {
      copy = 1;
      if ((strncmp(card, "COMMENT ", 7) == 0) && (copycomments == 0)) copy=0;
      if (copy) fits_write_record(outptr, card, status);
    }
    
  }
  fits_set_hdustruc(outptr, status);

  *status = 0;
  return *status;
}

/* 
 * Write image to FITS file
 *
 * fitsfile *outptr - FITS file pointer, opened for writing
 * struct image_struct *image - image to be written
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: CFITSIO status variable
 */
int image_write(fitsfile *outptr, struct image_struct *image, int *status)
{
  long int fpixel[] = {1,1,1,1,1,1,1,1,1,1};

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((outptr == 0) || (image == 0)) return NULL_INPUT_PTR;

  fits_create_img(outptr, FLT_IMG, 2, image->axes, status);
  /* Add some extra space for the keywords that are to come */
  fits_set_hdrsize(outptr, 65, status);
  if (*status) return (*status);

  fits_write_pixnull(outptr, TFLTTYPE, fpixel, image->axes[0]*image->axes[1], 
		     image->data, &(image->nullval), status);

  
  return (*status);
}

/*
 * Either create new file or append to existing file
 *
 * fitsfile **fileptr - pointer to FITS pointer; upon return,
 *                     (*fileptr) is the CFITSIO file pointer for the
 *                     output file.
 * char *filename - name of file to be created or appended to.
 * int append - flag: 1=append; 0=create
 * int *status - pointer to CFITSIO status variable
 * 
 * RETURNS: CFITSIO status variable
 */
int image_append(fitsfile **fileptr, char *filename, int append, 
		 int *status)
{
  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((fileptr == 0) || (filename == 0) || (filename[0] == 0)) 
    return (*status = NULL_INPUT_PTR);

  if (append) {
    fits_open_file(fileptr, filename, READWRITE, status);
    if (*status == 0) return (*status);
  }

  fits_create_file(fileptr, filename, status);
  if (*status) {
    fprintf(stderr, "ERROR: could not create %s\n", filename);
  }
  return (*status);
}


/* 
 * Rescale image, conserving flux
 *
 * Input and output images may contain more storage columns and rows
 * than the actual image occupies (i.e., the image may be zero-padded.
 *
 * struct image_struct *image - image to be rescaled
 * FLOAT xscale, yscale - rescaling factors in X and Y; output image
 *                        will have x/yscale as many pixels in each
 *                        direction.
 * FLOAT xphase, yphase - shift values in X and Y, in units of output pixels.
 * int ncols, nrows - number of storage rows and columns of output image;
 *                    may be larger than (xphase*image->axes[0]) and
 *                    (yphase*image->axes[1]).
 *
 * RETURNS: rescaled image.  User is responsible to free image.
 */
struct image_struct *image_rescale(struct image_struct *image, 
				   FLOAT xscale, FLOAT yscale,
				   FLOAT xphase, FLOAT yphase, 
				   int ncols, int nrows)
{
  struct image_struct *rimage = 0;
  if (image == 0) return 0;
  
  rimage = image_init(ncols, nrows, 0);
  if (rimage == 0) return 0;
  
  rescalearr(image->data, 
	     image->axes[0], image->axes[1],
	     image->axes[0], image->axes[1],
	     rimage->data, ncols, nrows, xscale, yscale, xphase, yphase);

  return rimage;
}

/* 
 * Compute complex multiplication C = A * B
 * Uses complex 2D arrangement of Ooura FFT library
 *
 * All images should be in Fourier domain.
 *
 * struct image_struct *c - destination image, must be initialized by user.
 * struct image_struct *a, *b - source images
 */
void image_cmult(struct image_struct *c,
		 struct image_struct *a,
		 struct image_struct *b)
{
  int i, j, nx, ny;
  FLOAT **ad, **bd, **cd;
  FLOAT *ua, *ub, *uc;
  FLOAT ar, ai, br, bi;

  if ((a == 0) || (b == 0) || (c == 0)) return;

  nx = a->axes[0]; ny = a->axes[1];
  ad = a->datap; bd = b->datap; cd = c->datap;

  /* Compute the multiplication, using Ooura's layout */

  /* The first set of values are the Nyquist frequencies, and are
     hence all real */
  cd[0][0]    = ad[0][0] * bd[0][0];
  cd[0][1]    = ad[0][1] * bd[0][1];
  cd[ny/2][0] = ad[ny/2][0] * bd[ny/2][0];
  cd[ny/2][1] = ad[ny/2][1] * bd[ny/2][1];

  /* Next set of values is the bulk of the array, and alternates
     real,complex */
  for (j=0; j<ny; j++) {
    ua = ad[j]; ub = bd[j]; uc = cd[j];
    for (i=2; i<nx-1; i+=2) {
      ar=ua[i]; ai=ua[i+1]; br=ub[i]; bi=ub[i+1];
      uc[i]   = ar*br + ai*bi;
      uc[i+1] = ai*br - ar*bi;
    }
  }

  /* First half-column is the zero-frequency components along each
     direction */
  for (j=1; j<ny/2; j++) {
    ar=ad[j][0]; ai=ad[j][1]; br = bd[j][0]; bi = bd[j][1];
    cd[j][0] = ar*br + ai*bi;
    cd[j][1] = ai*br - ar*bi;
  }

  /* Second half-column is the Nyquist frequency components for each
     direction.  Be careful, the data is stored in the opposite format
     from everything else, namely (-Im, Re) instead of (Re, Im). */
  for (j=ny-1; j>ny/2; j--) {
    ar=ad[j][1]; ai=-ad[j][0]; br=bd[j][1]; bi=-bd[j][0];
    cd[j][1] =   ar*br + ai*bi;
    cd[j][0] = -(ai*br - ar*bi);
  }

  if ((a->flags & IMGFLAG_FOURIER) || (b->flags & IMGFLAG_FOURIER)) {
    c->flags |= IMGFLAG_FOURIER;    /* Add Fourier flag */
  } else {
    c->flags &= (~IMGFLAG_FOURIER); /* Remove Fourier flag */
  }

  return;
}

/* 
 * Balance the image so that the sum is zero
 *
 * Image is balanced destructively.
 *
 * struct image_struct *image - image to be balanced
 * struct image_struct *mask - mask: 0=exclude from sum; 1=include
 */
void image_balance(struct image_struct *image,
		   struct image_struct *mask)
{
  int i, j;
  FLOAT total = 0;
  int npt = 0;

  /* Sum only non-zero bins */
  for (j=0; j<image->axes[1]; j++) {
    for (i=0; i<image->axes[0]; i++) {
      if (mask->datap[j][i] != 0) {
	total += image->datap[j][i];
	npt++;
      }
    }
  }
  total = total / npt;
  for (j=0; j<image->axes[1]; j++) {
    for (i=0; i<image->axes[0]; i++) {
      if (mask->datap[j][i] != 0) image->datap[j][i] -= total;
    }
  }
}


/* 
 * Return next highest power of 2^n
 *
 * unsigned int value - value to be analyzed
 *
 * RETURNS: the smallest value of (2^n) such that (value < 2^n)
 */
unsigned int image_next2pown(unsigned int value)
{
  unsigned int i;
  if (value <= 2) { return 2; }

  for (i=4; i>0; i*=2) {
    if (value < i) return i;
  }
  return 0;
}

/* 
 * Convolve image a with kernel b (or optionally fb)
 *
 * This routine has an optimization if the same kernel is to be
 * repeatedly applied.  If fb is a valid pointer, then upon return,
 * (*fb) contains the Fourier transform of b.  In subsequent calls,
 * image_convolve will use that value of (*fb) instead of b to speed
 * the computation.  User is responsible for freeing *fb when the
 * computations are finished.
 *
 * struct image_struct *a - image to be convolved;
 * struct image_struct *b - kernel that image is to be convolved with;
 *                          b is ignored if (*fb) is valid;
 * struct image_struct *fb - pointer to Fourier-transformed kernel;
 *                           fb == 0 means don't save it;
 *                           (*fb) == 0 means image_convolve should compute it
 *                              and save it in *fb;
 *                           (*fb) != 0 means image_convolve should use 
 *                              precomputed transformed kernel.
 * RETURNS: convolved image.  User is responsible to free image.
 */
struct image_struct *image_convolve(struct image_struct *a, 
				    struct image_struct *b, 
				    struct image_struct **fb)
{
  struct image_struct *Fa, *Fb, *result;
  int nx, ny;
  int xo, yo;
  int i, j;

  nx = image_next2pown(a->axes[0] + b->axes[0]);
  ny = image_next2pown(a->axes[1] + b->axes[1]);
  xo = b->axes[0]/2;
  yo = b->axes[1]/2;

  /* Create padded image */
  Fa = image_rescale(a, 1.0, 1.0, xo, yo, nx, ny);
  image_fft(Fa);
    
  if (fb && *fb) {
    /* Reuse an old kernel */
    Fb = *fb;
  } else {
    /* Create padded kernel */
    Fb = image_init(nx, ny, 0);
    for (j=0; j<b->axes[1]; j++) {
      /* Following code does three things:
	 1: offset by (xo,yo)
	 2: wraparound of kernel
	 3: mirror reversal of kernel in x and y */
      int jj = (j-yo+ny)%ny;
      int b0 = b->axes[0]-1, j0 = b0-j;
      for (i=0; i<b->axes[0]; i++) {
	Fb->datap[jj][(i-xo+nx)%nx] = b->datap[j0][b0-i];
      }
    }
    image_fft(Fb);
    if (fb) *fb = Fb;
  }
  
  /* Compute convolution, result goes in Fa */
  image_cmult(Fa, Fa, Fb);
  image_fft(Fa);
  if (fb == 0) image_free(Fb);

  result = image_init(a->axes[0], a->axes[1], 0);
  for (j=0; j<a->axes[1]; j++) {
    for (i=0; i<a->axes[0]; i++) {
      result->datap[j][i] = Fa->datap[j+yo][i+xo];
    }
  }
  image_free(Fa);

  return result;
}

/*
 * image_copy() - copy image to another
 *
 * struct image_struct *src - source 
 *
 *
 */
struct image_struct *image_copy(struct image_struct *source)
{
  struct image_struct *dest = 0;

  if (source == 0) return 0;
  if (source->axes[0] == 0 || source->axes[1] == 0) return 0;

  dest = image_init(source->axes[0], source->axes[1], 0);
  if (dest == 0) return 0;

  foreach_pixel(dest, i, { dest->data[i] = source->data[i]; });
  image_copyinfo(dest, source);

  return dest;
}


/*
 * image_copyinfo() - copy image metadata to another structure 
 *
 * struct image_struct *result - destination structure
 * struct image_struct *a - source structure
 *
 * RETURNS: -1 upon failure, 0 upon success
 *
 */
int image_copyinfo(struct image_struct *result, struct image_struct *a)
{
  if ((a == 0) || ( result == 0)) return -1;

  if (a->units[0]) { strcpy(result->units,a->units); }
  result->nullval = a->nullval;
  result->tstart = a->tstart;
  result->tstop  = a->tstop;
  result->exposure = a->exposure;
  result->telapse = a->telapse;
  result->ontime = a->ontime;
  return 0;
}

/*
 * image_writeinfo() - write image metadata to FITS keywords
 *
 * fitsfile *outptr - FITS file pointer, opened for writing
 * struct image_struct *image - source structure
 * int *status - pointer to CFITSIO status variable
 *
 * RETURNS: CFITSIO status variable
 *
 */
int image_writeinfo(fitsfile *outptr, struct image_struct *image, int *status)
{
  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((outptr == 0) || (image == 0)) return NULL_INPUT_PTR;

  /* Write image units to standard keyword, *IF* the units are
     defined.  For an empty string, nothing is written */
  if (image->units[0] != 0) {
    fits_update_key(outptr, TSTRING, "BUNIT", image->units,
		    "physical unit of image", status);
  }

  /* Write time and exposure keywords */
  if (image->tstart != (-1e307)) {
    fits_update_key(outptr, TDOUBLE, "TSTART", &(image->tstart), 
		    "start time of image", status);
  }
  if (image->tstop != (-1e307)) {
    fits_update_key(outptr, TDOUBLE, "TSTOP", &(image->tstop), 
		    "stop time of image", status);
  }
  if (image->exposure > 0) {
    fits_update_key(outptr, TDOUBLE, "EXPOSURE", &(image->exposure), 
		    "exposure of image", status);
  }
  if (image->telapse > 0) {
    fits_update_key(outptr, TDOUBLE, "TELAPSE", &(image->telapse),
		    "elapsed time of image (= TSTOP-TSTART)", status);
  }
  if (image->ontime > 0) {
    fits_update_key(outptr, TDOUBLE, "ONTIME", &(image->ontime), 
		    "on-time of image", status);
  }

  return *status;
}

/* 
 * Write the WCS keywords for one image axis
 *
 * fitsfile *imgfile - FITS file to modify
 * int axis - axis number to modify (1 or 2)
 * char *suffix - alternate WCS coordinate system suffix, or "" for default
 * char *wcsname - name of coordinate system, or "" for none
 * char *wcstype - name of coordinate axis, or "" for none
 *                 (not a WCS standard, but Chandra uses it)
 * char *ctype - axis name ("" means do not write)
 * double crpix - reference pixel in image (1.0 is center of 1st pixel)
 * double cdelt - grid spacing of image at reference pixel position
 * double crval - value of coordinate at reference pixel position
 * char *cunit - axis units ("" means do not write)
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status variable
 */
int write_wcsaxis(fitsfile *imgfile, int axis, char *suffix, 
		  char *wcsname, char *wcstype, 
		  char *ctype, double crpix, double cdelt, double crval, 
		  char *cunit, int *status)
{
  char key[20];

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (imgfile == 0) return (*status = NULL_INPUT_PTR);

  if (wcsname && wcsname[0]) {
    sprintf(key, "WCSNAME%s", suffix);
    fits_update_key(imgfile, TSTRING, key, wcsname,
		   "Coordinate system name", status);
  }
  if (wcstype && wcstype[0]) {
    sprintf(key, "WCSTY%d%s", axis, suffix);
    fits_update_key(imgfile, TSTRING, key, wcstype,
		   "Coordinate system axis", status);
  }
  sprintf(key, "CTYPE%d%s", axis, suffix);
  fits_update_key(imgfile, TSTRING, key, ctype,
		 "Name of coordinate", status);
  if (cunit && cunit[0]) {
    sprintf(key, "CUNIT%d%s", axis, suffix);
    fits_update_key(imgfile, TSTRING, key, cunit,
		    "Units of coordinate axis", status);
  }
  sprintf(key, "CRPIX%d%s", axis, suffix);
  fits_update_key(imgfile, TDOUBLE, key, &crpix,
		 "Reference pixel position", status);
  sprintf(key, "CDELT%d%s", axis, suffix);
  fits_update_key(imgfile, TDOUBLE, key, &cdelt,
		 "Pixel spacing in physical units", status);
  sprintf(key, "CRVAL%d%s", axis, suffix);
  fits_update_key(imgfile, TDOUBLE, key, &crval,
		 "Coordinate value at reference pixel position", status);

  return (*status);
}

/* 
 * Read the WCS keywords for one image axis
 *
 * fitsfile *imgfile - FITS file to read from
 * int axis - axis number to read (1 or 2)
 * char *suffix - alternate WCS coordinate system suffix, or "" for default
 * char *wcsname - upon output, name of coordinate system, or "" for none
 * char *wcstype - upon output, name of coordinate axis, or "" for none
 *                 (not a WCS standard, but Chandra uses it)
 * char *ctype - upon output, axis name
 * double *crpix - upon output, reference pixel in image
 * double *cdelt - upon output, grid spacing of image at ref. pixel position
 * double *crval - upon output, value of coordinate at reference pixel position
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status variable
 */
int read_wcsaxis(fitsfile *imgfile, int axis, char *suffix, 
		 char *wcsname, char *wcstype, 
		 char *ctype, double *crpix, double *cdelt, double *crval, 
		 int *status)
{
  char key[20];

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (imgfile == 0) return (*status = NULL_INPUT_PTR);

  if (wcsname) {
    wcsname[0] = 0;
    sprintf(key, "WCSNAME%s", suffix);
    fits_read_key(imgfile, TSTRING, key, wcsname, NULL, status);
    *status = 0;  /* Fall through if keyword not there */
  }
  if (wcstype) {
    wcstype[0] = 0;
    sprintf(key, "WCSTY%d%s", axis, suffix);
    fits_read_key(imgfile, TSTRING, key, wcstype, NULL, status);
    *status = 0;  /* Fall through if keyword not there */
  }
  sprintf(key, "CTYPE%d%s", axis, suffix);
  if (ctype) fits_read_key(imgfile, TSTRING, key, ctype, NULL, status);
  sprintf(key, "CRPIX%d%s", axis, suffix);
  if (crpix) fits_read_key(imgfile, TDOUBLE, key, crpix, NULL, status);
  sprintf(key, "CDELT%d%s", axis, suffix);
  if (cdelt) fits_read_key(imgfile, TDOUBLE, key, cdelt, NULL, status);
  sprintf(key, "CRVAL%d%s", axis, suffix);
  if (crval) fits_read_key(imgfile, TDOUBLE, key, crval, NULL, status);

  return (*status);
}

/* Determine whether this is a 'rate' unit string or not */
int is_units_rate(char *unitstr)
{
  char compstr[100];
  char *p, *q;
  
  /* Bounds checking */
  if (unitstr == 0) return 0;
  if (unitstr[0] == 0) return 0;
  if (strlen(unitstr) > (100-1)) return 0;

  /* Compress all the spaces out */
  for(p=unitstr, q=compstr; *p; p++) {
    if (! isspace(*p)) *q++ = tolower(*p);
  }
  *q = 0;

  /* Possible units: count/s or count s**(-1) */
  if (strcmp(compstr, "count/s") == 0) return 1;
  if (strcmp(compstr, "counts**(-1)") == 0) return 1;
  
  return 0;
}
