#include <stdio.h>
#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"

#include "ftimgcalc.h"

/* Main driver program

  ftimgcalc - create a new FITS image from computations on one or
              more input images

  ftimgcalc performs general image computations on a set of input
  images.  Multiple images can be combined using any of the mathematical
  operators and functions supported by the CFITSIO calculator.

  This task is similar to the ftpixcalc task, but has some additional
  capabilities, most notably, the ability to perform calculations
  based on world coordinates (e.g., the RA and DEC value of the
  pixels), or ASCII region files, and the ability to process multiple
  FITS extensions in sequence.  In cases where both ftpixcalc and
  ftimgcalc can perform the same task, ftpixcalc will usually require
  less computer memory and, in principle, may run faster.

*/

#define TOOLSUB ftimgcalc
static char taskname[80] = "ftimgcalc";
static char taskver[8] = "2.1";

/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure used to map between variable name and FITS file name */
struct image_name_struct { 
  char colname[FLEN_CARD];     /* Variable name used in expression */
  char filename[PIL_PATH_MAX]; /* FITS image file name */
};

struct parm_struct {
  char *taskname;              /* Task name */
  char *taskver;               /* Task version */
  char infiles[PIL_LINESIZE];  /* Input file name string */
  char outfile[PIL_PATH_MAX];  /* Output file name string */
  char diagfile[PIL_PATH_MAX]; /* Diagnostic file name string */
  char expr[MAX_EXPR_LEN];     /* Expression to compute */
  char wcscoordimage[PIL_LINESIZE];/* Image to retrieve WCS coordinates from */
  char resultcol[FLEN_CARD];   /* Name of output result column/extension */
  char tform[FLEN_CARD];       /* TFORM of output */
  char bunit[FLEN_CARD];       /* BUNIT of output */
  char otherext[FLEN_CARD];    /* Which input image to draw other extensions */
  int nvectimages;             /* Number of vector images */
  int nimages;                 /* Number of images specified on cmd line */
  int replicate;               /* Whether to replicate images to nvectimages */
  struct image_name_struct images[26]; /* Variable <-> File mapping */
  int chatter;                 /* Verbosity level (1-5) */
  int firstimagenum;           /* First image in a-z scale specified */
};

/*
 * ftimgcalc_getimages - prompt for each input image 
 *
 */
int ftimgcalc_getimages(struct parm_struct *parms, int reprompt)
{
  int i;
  char parname[10];
  char filename[PIL_PATH_MAX], *p;
  int status = 0;

  parms->firstimagenum = -1;

  /* Loop through each image position */
  for (i=0; i<26; i++) {
    parms->images[i].filename[0] = '\0';
    sprintf(parms->images[i].colname, "%c", i+'A');
    sprintf(parname, "%c", i+'a');

    /* This is where some magic happens.  Reprompt means, go to the
       command line even if the value is in the parameter file.  This
       makes it possible to prompt for images at the console if the
       command line is empty. */
    if (reprompt) PILSetReprompt(parname, 1);
    status = PILGetString(parname, filename);
    if (status) return status;
      
    if (reprompt) {
      /* End the loop when we reach the first NONE value */
      if (strcasecmp(filename, "NONE") == 0) break;
    } else {
      if (strcasecmp(filename, "NONE") == 0) continue;
    }
      
    parms->nimages ++;
    if (parms->firstimagenum < 0) parms->firstimagenum = i;

    /* Parse either 'VAR=filename' or simply 'filename' */
    for (p=filename; *p && *p != '=' && *p != '['; p++);
    if (*p == '=') {
      /* 'VAR=filename' */
      strcpy(parms->images[i].filename, p+1);     /* filename */
      *p = 0;
      strcpy(parms->images[i].colname, filename); /* VAR */
    } else {
      /* 'filename' */
      strcpy(parms->images[i].filename, filename); /* filename */
    }

    /* Check for invalid variable names */
    headas_chat(5, "  -- col name %s = %s\n", 
		parname, parms->images[i].colname);
    if ((strcasecmp(parms->images[i].colname,"t") == 0) ||
	(strcasecmp(parms->images[i].colname,"f") == 0) ) {
      fprintf(stderr, 
	      "ERROR: you cannot specify a variable name as 'T' or 'F'.\n"
	      "       Either rename the variable or do not use 'T' or 'F'.\n");
      return -1;
    }
  }

  return status;
}

int ftimgcalc_getpar(struct parm_struct *parms) 
{
  int status = 0;
  int tform_n = 0;

  char *file_expr = NULL;
  
  parms->infiles[0] = 0;
  parms->outfile[0] = 0;
  parms->expr[0] = 0;
  parms->tform[0] = 0;
  parms->nimages = 0;
  parms->nvectimages = 1;
  
  if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("diagfile", parms->diagfile))) 
    fprintf(stderr, "Error reading the 'diagfile' parameter.\n");

  else if ((status = PILGetString("expr", parms->expr))) 
    fprintf(stderr, "Error reading the 'expr' parameter.\n");

  else if ((status = PILGetInt("nvectimages", &(parms->nvectimages) )))
    fprintf(stderr, "Error reading the 'nvectimages' parameter.\n");

  else if ((status = PILGetBool("replicate", &(parms->replicate) )))
    fprintf(stderr, "Error reading the 'replicate' parameter.\n");

  else if ((status = PILGetString("bitpix", parms->tform))) 
    fprintf(stderr, "Error reading the 'bitpix' parameter.\n");

  else if ((status = PILGetString("bunit", parms->bunit))) 
    fprintf(stderr, "Error reading the 'bunit' parameter.\n");

  else if ((status = PILGetString("wcsimage", parms->wcscoordimage))) 
    fprintf(stderr, "Error reading the 'wcsimage' parameter.\n");

  else if ((status = PILGetString("otherext", parms->otherext))) 
    fprintf(stderr, "Error reading the 'otherext' parameter.\n");

  else if ((status = PILGetString("resultname", parms->resultcol))) 
    fprintf(stderr, "Error reading the 'resultname' parameter.\n");
  
  else if ((status = PILGetInt("chatter", &(parms->chatter) )))
    fprintf(stderr, "Error reading the 'chatter' parameter.\n");

  if (status) return status;

  status = ftimgcalc_getimages(parms, 0);

  /* If no parameters were specified on the command line as hidden
     parameters, then reprompt for each one until we reach a "NONE"
     value. */

  if (status == 0 && parms->nimages == 0) {
    status = ftimgcalc_getimages(parms, 1);
  }

  /* Handle default values */
  if (strcasecmp(parms->outfile,"NONE") == 0) {
    parms->outfile[0] = 0;
  }

  /* Handle default values */
  if (strcasecmp(parms->diagfile,"NONE") == 0) {
    parms->diagfile[0] = 0;
  }

  if (strcasecmp(parms->bunit,"NONE") == 0) {
    parms->bunit[0] = 0;
  }

  if (strcasecmp(parms->tform,"INDEF") == 0) {
    parms->tform[0] = 0;
  } else {
    if (sscanf(parms->tform, "%d", &tform_n) > 0) {
      /* Check for an integer number */
      switch (tform_n) {
      case   0: parms->tform[0] = 0;   break;
      case   8: parms->tform[0] = 'B'; break;
      case  16: parms->tform[0] = 'I'; break;
      case  32: parms->tform[0] = 'J'; break;
      case  64: parms->tform[0] = 'K'; break;
      case -32: parms->tform[0] = 'E'; break;
      case -64: parms->tform[0] = 'D'; break;
      default: 
	fprintf(stderr, "ERROR: 'bitpix=%d' is an invalid integer",
		tform_n);
	return -1;
      }
      parms->tform[1] = 0;  /* New null termination */
    }
  }    
   
  if (strcasecmp(parms->otherext,"NONE") == 0) {
    parms->otherext[0] = 0;
  }

  if (strcasecmp(parms->wcscoordimage,"NONE") == 0) {
    parms->wcscoordimage[0] = 0;
  }

  /* Default WCS image, if the input is INDEF */
  if (strcasecmp(parms->wcscoordimage,"INDEF") == 0 && 
      parms->firstimagenum >= 0) {
    strcpy(parms->wcscoordimage, 
	   parms->images[parms->firstimagenum].colname);
    headas_chat(5, "  ... wcsimage = %s ...\n", parms->wcscoordimage);
  }

  /* If expression is a file name (begins with '@'), process it through
   * ffimport_file (CFITSIO). */
  if( parms->expr[0] == '@' ) {
      ffimport_file( parms->expr+1, &file_expr, &status );
      strcpy(parms->expr,file_expr);
      if( file_expr ) free( file_expr );
  }
  
  return status;
}

/* ----------------------------------------------------------------- */


int ftimgcalc_work(struct parm_struct *parms)
{
  int nimages = 0, nvectimages = 0;
  struct image_name_struct *images = 0;
  fitsfile *internal = 0, *outfile = 0, *otherfile = 0;
  /* Tracking variables for each input file */
  fitsfile *inptr[26];
  int firsthdu[26], hdutype[26], hdunum[26];
  char expr[MAX_EXPR_LEN];
  int status = 0;
  int i, row;
  int outcolnum = 0;
  int copykeyflag = 1; /* Copy keys from input image to table cell */

  char *extname = "INTERNAL_IMAGES";
  char *tform = 0;
  int wcscoordimagenum = -1;
  int ncols;
  int unitscol = -1;
  int firsttype = 0;
  
  nvectimages = parms->nvectimages;
  images = parms->images;
  /* Initialize input tracking variables */
  for (i=0; i<26; i++) {
    inptr[i] = 0;
    firsthdu[i] = hdutype[i] = hdunum[i] = -1;
  }

  /* ==================== */
  /* Parse the image list */
  nimages = parms->nimages;
  if (nimages == 0) {
    fprintf(stderr, "ERROR: no images were specified on the command line\n");
    return -1;
  }

  /* ============================================ */
  /* MAIN LOOP */
  /* Loop through each row in the input */
  for (row=1; row <= nvectimages; row++) {
    headas_chat(5, "...row %d...\n", row);
    /* Make a temporary copy of parms->expr */
    strcpy(expr, parms->expr);
    
    /* ==================== */
    /* Create the internal file to be used for the calculation */
    fits_create_file(&internal, "mem://internal", &status);
    fits_create_tbl(internal, BINARY_TBL, 0, 0, 0, 0, 0, extname, &status);
    if (status > 0) {
      fprintf(stderr, "ERROR: could not create internal data structures\n");
      return status;
    }
    
    /* ==================== */
    /* Loop through each input image and read it into the table */
    headas_chat(5,"  ...reading images...\n");
    copykeyflag = 1; /* Copy all keywords from first image file */
    for (i=0; i<26; i++) {
      
      if (images[i].filename[0] == 0) continue;
      headas_chat(5,"  ...image %s=\"%s\"...\n", 
		  images[i].colname, images[i].filename);

      /* Open input file for this column (if not already open) */
      if (check_open_file(&(inptr[i]), images[i].filename,
			  &(firsthdu[i]), &(hdutype[i]), &status)) {
	fprintf(stderr, "ERROR: could not open %s\n",
		images[i].filename);
	return status;
      }

      /* Read data type of image, if not already determined */
      if (!firsttype) {
	fits_get_img_equivtype(inptr[i], &firsttype, &status);
      }

      /* Increment to the "next" extension */
      if (increment_image_ext(inptr[i], 
			      row, parms->replicate, 
			      &(firsthdu[i]), &(hdutype[i]), &(hdunum[i]), 
			      &status)) {
	fprintf(stderr, 
		"ERROR: could not find %d images in %s\n"
		"       and 'replicate' was not set\n",
		parms->nvectimages, images[i].filename);
	return status;
      }
      headas_chat(5, "  ...extension number %d...\n", hdunum[i]);

      /* Copy the image HDU to a table cell */
      if (fits_copy_image2cell(inptr[i], internal, images[i].colname, 1, 
			       copykeyflag, &status)) {
	fprintf(stderr, "ERROR: could not read image from %s into row %d\n",
		images[i].filename, row);
	return status;
      }
      copykeyflag = 2; /* Copy WCS keywords ONLY, after the first cell */


      /* Record column-number related things.  Because the WCS
	 calculation below could violate the
	 image-number-equals-column-number assumption, we directly
	 retrieve the number of columns here.  */
      
      fits_get_num_cols(internal, &ncols, &status);

      /* First time around, check if this is the "units" column, etc. 
         If the function returns "true" then this is the "otherext"
	 file from which other extensions will be taken.
      */
      if (row == 1 && 
	  update_image_params(images[i].colname, i, ncols, 
			      parms->wcscoordimage,
			      parms->bunit,
			      parms->otherext,
			      &wcscoordimagenum, &unitscol)) {
	otherfile = inptr[i];
      }

      headas_chat(5,"    (starting WCS calc)\n");
      make_wcs_images(inptr[i], internal, 
		      expr, images[i].colname, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not create WCS coordinate columns\n");
	return status;
      }
      /* Note: this expression occurs *afterward* because it may be
	 modified by make_wcs_images */
      headas_chat(5,"    (expr=\"%s\")\n", expr);

    }
    headas_chat(5,"  ...row %d: done reading images...\n", row);
    /* End of image-reading loop for this row */

    /* Sanity checking on wcs, unit, and otherext parameters */
    if (row == 1 && 
	(status = check_image_params(parms->wcscoordimage,
				     parms->bunit,
				     parms->otherext,
				     wcscoordimagenum, unitscol, otherfile))) {
      return status;
    }

    /* If output data type is not specified, guess the optimal type */
    if (!parms->tform[0]) {
      int tform = choose_tform(internal, expr, firsttype);
      if (tform < 0) { return BAD_TFORM; }
      parms->tform[0] = tform;
      parms->tform[1] = 0;

      headas_chat(5, "  ...first input image has type code %d\n",firsttype);
    }   

    tform = parms->tform;
    headas_chat(5, "  ...creating output image of type %s\n",tform);

    /* Replace #ROW with the actual row number.  Since we are only
       working on one image "row" at a time, the special notation #ROW
       no longer actually means "row number" since it will always read
       a value of 1.  We have to rewrite the expression to compensate. */
    {
      char tmpstr[MAX_EXPR_LEN];  /* Temp string */
      char rowstr[14];            /* Row number 14 = "(14)" */
      int n = 0;

      sprintf(rowstr, "(%d)", row);
      n = repstr(tmpstr, expr, "#ROW", rowstr);
      if (n > 0) { 
	strcpy(expr, tmpstr);
	headas_chat(5, "  (found '#ROW' in expression, replaced with '%s')\n",
		    rowstr);
	headas_chat(5,"    (expr=\"%s\")\n", expr);
      }
    }

    headas_chat(5, "  ...calculating...\n");
    /* ==================== */
    /* Make the computation !! */
    if (fits_calculator(internal, expr, internal, 
			parms->resultcol, tform, &status) ||
	fits_get_colnum(internal, CASEINSEN, parms->resultcol, 
			&outcolnum, &status)) {
      fprintf(stderr, 
	      "ERROR: calculation of expression failed:\n"
	      "         %s\n", expr);
      
      return status;
    }
    headas_chat(5, "  ...done calculating...\n");
    
    
    /* Re-write the WCS keywords */
    if (wcscoordimagenum >= 1) {
      headas_chat(5, "  ...applying WCS coordinates...\n");
      copy_wcs_imcolumn2imcolumn(internal, internal, 
				 wcscoordimagenum, outcolnum, &status);
      if (status) {
	fprintf(stderr, "ERROR: keyword translation for new image failed\n");
	return status;
      }
    }

    /* ==================== */
    if (parms->bunit[0]) {
      status = apply_units_keyword(internal, parms->bunit, unitscol, outcolnum);
      if (status) return status;
    }
      
    /* ==================== */
    /* Write diagnostic file if requested (this is just the internal
       FITS table) */
    if (parms->diagfile[0]) {
      fitsfile *diagfile = 0;
      int mystatus = 0;
      
      headas_chat(5, "  ...writing diagnostic file...\n");
      if (row == 1) {
	/* First pass: create */
	headas_clobberfile(parms->diagfile);
	fits_create_file(&diagfile, parms->diagfile, &status);
	fits_copy_file(internal, diagfile, 1, 1, 1, &status);
      } else {
	/* Later passes: append */
	fits_open_table(&diagfile, parms->diagfile, READWRITE, &status);
	fits_copy_hdu(internal, diagfile, 0, &status);
      }
      fits_close_file(diagfile, &mystatus);
      if (status) {
	fprintf(stderr, 
		"ERROR: failed to write diagnostic file\n"
		"         %s\n", parms->diagfile);
	return status;
      }
    }
    

    if (row == 1 && parms->outfile[0]) {
      headas_chat(5, "  ...creating output image...\n");
      headas_clobberfile(parms->outfile);
      fits_create_file(&outfile, parms->outfile, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not create %s\n", parms->outfile);
	return status;
      }
    }

    status = print_col_value(internal, outcolnum, /* row = */ 1, 
			     parms->chatter, outfile, 
			     parms->resultcol, parms->bunit);
    if (status) return status;
    
    headas_chat(5,"  ... output row %d...\n", row);

    /* Write the data to the output file if requested */
    if (outfile) {
      char outextname[FLEN_CARD];

      if (fits_copy_cell2image(internal, outfile, parms->resultcol, 
			       /* row = */ 1, &status)) {
	fprintf(stderr, "ERROR: could not write image to output (row %d)\n",
		row);
	return status;
      }
      /* Output extension keyword is HDUNAME (primary) or EXTNAME (all
	 other HDUS) */
      sprintf(outextname, "%s_%d", parms->resultcol, row);
      if (row == 1) {
	fits_update_key(outfile, TSTRING, "HDUNAME", 
			outextname, 0, &status);
      } else {
	fits_update_key(outfile, TSTRING, "EXTNAME",
			outextname, 0, &status);
      }

      status = HDpar_stamp(outfile, 0, &status);
    }

    /* Close internal memory file. We will create a new one if necessary */
    {
      int mystatus = 0;
      fits_close_file(internal, &mystatus);
      internal = 0;
    }
    if (status) break;
  }
  headas_chat(5, "  ...end loop over rows...\n");

  /* Copy any extra extensions */
  if (outfile && otherfile) {
    fits_copy_file(otherfile, outfile, 0, 0, 1, &status); otherfile = 0;
    /* Note that otherfile will be closed along with all input files
       in the next cleanup section */
  }

  /* ==================== */
  /* Close the files safely */
  {
    int mystatus;

    mystatus = 0;
    if (outfile) fits_close_file(outfile, &mystatus); outfile = 0;
    mystatus = 0;
    if (internal) fits_close_file(internal, &mystatus); internal = 0;
    internal = 0;
    for (i = 0; i<26; i++ ) if (inptr[i]) {
      mystatus = 0;
      fits_close_file(inptr[i], &mystatus); inptr[i] = 0;
    }
  }
  
  return 0;
}


int ftimgcalc(void)
{
  int status = 0;
  
  struct parm_struct parms;
  
  /* Register taskname and version. */
  
  set_toolname(taskname);
  set_toolversion(taskver);
  
  if ((status = ftimgcalc_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }
  
  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];
  
  return ftimgcalc_work(&parms);
  
}

