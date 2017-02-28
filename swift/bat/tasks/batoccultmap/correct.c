/*
 * correct.c
 *
 * corrections applied to images
 *
 * $Id: correct.c,v 1.5 2007/04/03 19:05:01 craigm Exp $
 *
 */

#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"

#include "batoccultmap.h"

/* ----------------------------------------------------------------- */
/* Apply exposure correction to images */

int imgcorrect(struct parm_struct *parms,
	       struct image_struct *sky,
	       FLOAT nullval,
	       char *creator)
	       
{
  int op;         /* Operation 1=MULT; 2=DIV */
  int nitems = 0; /* Number of files to operate on */
  char *flist;    /* Original file list */
  char **filelist;/* Parsed file list */
  struct image_struct *im = 0;  /* Image to be corrected */
  FLOAT imnullval;/* Null value for corrected image */
  int occapp = 0; /* Reading OCCAPP keyword from input */
  int i, j;
  fitsfile *imgfile = 0;
  int status = 0;
  int anynull = 0;
  long fpixel[2] = {1, 1};  /* For use with fits_write_pixnull() */
  
  /* Automatically correct any requested images */
  for (j=0; j<2; j++) {
    nitems = 0;
    im = 0;
    occapp = 0;

    /* Pull out the multiply, then divide lists */
    filelist = 0;
    if (j == 0) {  /* MULTIPLY */
      flist = parms->multfiles;
      op = 1;
    } else {       /* DIVIDE  */
      flist = parms->divfiles;
      op = 2;
    }

    /* No files, skip this one */
    if (flist[0] == 0) continue;

    /* Parse the file list, or read an @-batch file */
    filelist = expand_item_list(flist, &nitems, ',',
				1, 1, 1, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not parse '%s'\n", flist);
      return status;
    }

    /* Loop through the files in the list */
    for (i=0; i<nitems; i++) {
      imnullval = 1e38;	

      /* Open and read the image */
      headas_chat(2, "Correcting %s (%s)...\n",
		  filelist[i], (op == 1)?("MULTIPLY"):("DIVIDE"));
      fits_open_image(&imgfile, filelist[i], READWRITE, &status);
      im = image_read_i(imgfile,-1,0,&imnullval, &anynull, &status);
      if (status != 0 || im == 0) {
	fprintf(stderr, "ERROR: could not read image '%s'\n",
		filelist[i]);
	if (status == 0) { status = MEMORY_ALLOCATION; }
	return status;
      }

      fits_write_errmark();
      fits_read_key(imgfile, TLOGICAL, "OCCAPP", &occapp, 0, &status);
      fits_clear_errmark();
      if (status) {
	status = 0;
	occapp = 0;
      }
      if (occapp) {
	fprintf(stderr, "ERROR: image %s has already been corrected\n",
		filelist[i]);
	return -1;
      }
      
      /* Check for compatibility;
	 XXX really we should check for WCS matches too */
      if ((im->axes[0] != sky->axes[0]) || (im->axes[1] != sky->axes[1])) {
	fprintf(stderr, "ERROR: image '%s' does not match input image\n",
		filelist[i]);
	status = BAD_NAXIS;
	return status;
      }
      
      /* Apply the correction... */
      if (op == 1) {    /* ... MULTIPLY ... */
	foreach_pixel(im, i, { 
	  if ((sky->data[i] == nullval) || (sky->data[i] == 0) ||
	      (im->data[i] == imnullval)) { 
	    im->data[i] = imnullval; 
	  } else {
	    im->data[i] *= sky->data[i]; }});
      } else {          /* ... or DIVIDE */
	foreach_pixel(im, i, { 
	  if ((sky->data[i] == nullval) || (sky->data[i] == 0) ||
	      (im->data[i] == imnullval)) { 
	    im->data[i] = imnullval; 
	  } else {
	    im->data[i] /= sky->data[i]; }});
      }

      /* Write the image */
      im->nullval = imnullval;
      fits_write_pixnull(imgfile, TFLTTYPE, fpixel, im->axes[0]*im->axes[1], 
			 im->data, &(im->nullval), &status);

      /* Update the image keywords */
      fits_write_history(imgfile, "Image was corrected for occultation", &status);
      occapp = 1;
      fits_update_key(imgfile, TLOGICAL, "OCCAPP", &occapp, 
		      "Occultation correction has been applied", &status);
      if (op == 1) {
	fits_update_key(imgfile, TSTRING, "OCCTYPE", "MULTIPLY",
			"Original data were MULTIPLIED by correction", &status);
      } else {
	fits_update_key(imgfile, TSTRING, "OCCTYPE", "DIVIDE",
			"Original data were DIVIDED by correction", &status);
      }
      fits_update_key(imgfile, TSTRING, "OCCLEVS", parms->occultation, 
		      "Apply fractional or mask (ANY) correction?", &status);
      fits_update_key(imgfile, TSTRING, "OCCFILE", parms->outfile,
		      "Occultation correction filename", &status);
      fits_update_key(imgfile, TSTRING, "OCCVER", creator,
		      "Occultation correction version", &status);
      status = HDpar_stamp(imgfile, 0, &status);
      fits_set_hdustruc(imgfile, &status);
      /* Add the parameter HISTORY for good measure */
      fits_close_file(imgfile, &status);
      imgfile = 0;

      /* Release resources */
      image_free(im); 
      im = 0;
      if (status) {
	fprintf(stderr, "ERROR: failed to write modified image '%s'\n",
		filelist[i]);
	return status;
      }
      
    } /* End of loop through corrected images */
    
    free(filelist);
    filelist = 0;
  } /* End of loop through mult/div */

  return status;
}
