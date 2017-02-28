/*
 * $Source: /headas/headas/heatools/ftpixcalc/ftpixcalc.c,v $
 * $Revision: 1.6 $
 * $Date: 2008/05/09 19:05:27 $
 *
 * $Log: ftpixcalc.c,v $
 * Revision 1.6  2008/05/09 19:05:27  irby
 * Call HDfile_system_check with mode "e" instead of 0.  Was exiting
 * with an error (for either value of clobber) when the output file
 * existed.
 *
 * Revision 1.5  2006/09/06 17:26:35  pence
 * Changes:  incremented version to 1.0
 *           fixed bug so it doesn't clobber the input file unless clobber=y
 *           added warnings about using the f and t parameters (confused with
 *              TRUE/FALSE)
 *           implemented the HISTORY parameter functionality
 *           fixed the BITPIX default value in the .par file (0 instead of "0")
 *
 * Revision 1.4  2006/05/18 18:16:58  pence
 * changed fits_open_file to fits_open_image in ftpixcalc.c so that it will
 * automatically move to the first non-null image extension in the input file(s).
 *
 * Revision 1.3  2006/05/17 21:21:37  pence
 * Made major changes to the ftpixcalc user interface to make it more
 * similar to the ftimgcalc parameter interface.  Added more examples
 * to the help file.
 *
 * Revision 1.2  2006/03/14 19:13:49  irby
 * Update TOOLSUB to reflect the new name 'ftpixcalc'.
 *
 * Revision 1.1  2006/03/09 18:16:37  irby
 * New tool submitted by Bob Wiegand:
 * ftpixcalc will read multiple input FITS files and evaluate a fairly
 * arbitrary expression to derive the output pixel values.
 *
 */

#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "fitsio.h"

#define TOOLSUB ftpixcalc
#include "headas_main.c"

#define VERSION 1.0

struct Parameters {
	int chatter;
	int clobber;

	char infile[PIL_LINESIZE];
	char tag[FLEN_FILENAME];
	char expression[PIL_LINESIZE];
	char bunit[FLEN_CARD];

	char outfile[FLEN_FILENAME];
	int bitpix;
	int nimages;
};

static int parse_filter_parameters(struct Parameters *p, PixelFilter *filter,
				int *status)
{
	int items;
	int trim = 1;
	int skip = 1;
	int guard = 1;
	int delimiter = ',';

	if (*status)
		return *status;

	/* determine number of images in infile */
	filter->path = expand_item_list(p->infile, &items,
					delimiter, trim, skip, guard, status);
	filter->count = items;

	filter->tag = expand_item_list(p->tag, &items,
					delimiter, trim, skip, guard, status);

	if (filter->count > items) {
		*status = TOO_MANY_FILES;
		headas_chat(0, "not enough tags [%d] for images [%d]\n",
						items, filter->count);
	}

	return *status;
}


static int initialize_file_handles (struct Parameters * p, PixelFilter * filter,
				int * status)
{
	int i;

	if (*status)
		return *status;

	fits_create_file(&filter->ofptr, p->outfile, status);
	if (*status)
		headas_chat(0, "unable to create %s [%d]\n", p->outfile, *status);

	filter->ifptr = calloc(filter->count, sizeof(fitsfile *));

	for (i = 0; !*status && i < filter->count; ++i) {
		char * path = filter->path[i];
		fits_open_image(&filter->ifptr[i], path, READONLY, status);
		if (*status)
			headas_chat(0, "unable to open %s [%d]\n", path, *status);
		/* require HDU to be an image? */
		/* how does the parser know which input to take keywords from? */
	}

	return *status;
}



static int release_filter (PixelFilter * filter, int * status)
{
	
	/* only bother to close output file */
	if (!*status) {
	        /* write optional history keywords */
		HDpar_stamp(filter->ofptr, 0, status);
		fits_close_file(filter->ofptr, status);
	}
	
	if (filter->tag)
		free(filter->tag);

	if (filter->path)
		free(filter->path);

	return 0;
}


static int run_parameters (struct Parameters * p, int * status)
{
	PixelFilter filter = { 0 };

	if  (*status) return *status;

	if (HDfile_system_check(p->outfile, "e")) {
			if (!p->clobber) {
				*status = FILE_NOT_CREATED;
				headas_chat(0, "%s exists and clobber not set\n", p->outfile);
				return *status;
			}
			else
				headas_clobberfile(p->outfile);
	}

	if (!*status)
		parse_filter_parameters(p, &filter, status);

	if (!*status)
		initialize_file_handles(p, &filter, status);

	filter.bitpix = p->bitpix;
	filter.expression = p->expression;

	if (!*status)
		fits_pixel_filter(&filter, status);
	
	if (!*status) {
	    if (p->bunit[0])
	       fits_update_key_str(filter.ofptr, "BUNIT", p->bunit,
	            "physical unit of image",  status);
	}

	release_filter(&filter, status);

	if (*status)
		remove(p->outfile);
	
	return *status;
}

static int ftpixcalc_getimages(struct Parameters * parms, int reprompt)
{
  int i;
  char parname[10];
  char filename[PIL_PATH_MAX], *p;
  int status = 0;


  parms->infile[0] = '\0';
  parms->tag[0] = '\0';
  
  for (i=0; i<26; i++) {
    sprintf(parname, "%c", i+'a');
      
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
    
    for (p=filename; *p && *p != '=' && *p != '['; p++);
    if (*p == '=') {
      strcat(parms->infile, p+1);
      strcat(parms->infile, ",");
      *p = 0;
      strcat(parms->tag, filename);
      strcat(parms->tag, ",");
    } else {
      strcat(parms->infile, filename);
      strcat(parms->infile, ",");
      strcat(parms->tag, parname);
      strcat(parms->tag, ",");
      
      /* error check for use of 'f' or 't' parameters names, which are
         reserved for Boolean True or False by the parser */
      if (*parname == 'f' || *parname == 't') {
	fprintf(stderr, "Error: the 'f' and 't' filename parameters can only be used with a\n");
	fprintf(stderr, " mnemonic name (e.g., f='fimage = myfile.fits'), because these 2 letters are\n");
	fprintf(stderr, " interpreted as the Boolean TRUE or FALSE by the expression parser.\n");
	status = PARSE_SYNTAX_ERR;
	return status;
      }
    }
  }

  /* remove the trailing comma from each string */
  i = strlen(parms->infile);
  if (i > 0)
     (parms->infile)[i-1] = '\0';

  i = strlen(parms->tag);
  if (i > 0)
     (parms->tag)[i-1] = '\0';
  
  return status;
}



static int get_parameters (struct Parameters * p, int * status)
{
        if ((*status = PILGetFname("outfile", p->outfile)))
          fprintf(stderr, "Error reading the 'outfile' parameter.\n");

        else if ((*status = PILGetString("expr", p->expression))) 
          fprintf(stderr, "Error reading the 'expr' parameter.\n");

        else if ((*status = PILGetInt("bitpix", &p->bitpix))) 
          fprintf(stderr, "Error reading the 'bitpix' parameter.\n");

        else if ((*status = PILGetString("bunit", p->bunit))) 
          fprintf(stderr, "Error reading the 'bunit' parameter.\n");

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;

        if (*status) return *status;

        *status = ftpixcalc_getimages(p, 0);

        /* If no parameters were specified on the command line as hidden
          parameters, then reprompt for each one until we reach a "NONE"
          value. */

        if (*status == 0 && p->nimages == 0) {
          *status = ftpixcalc_getimages(p, 1);
        }

        /* Handle default values */
        if (strcasecmp(p->bunit,"NONE") == 0) {
          p->bunit[0] = 0;
        }

        return *status;
}


int TOOLSUB ()
{
	int status = 0;
	struct Parameters parameters = { 0 };

	if (get_parameters(&parameters, &status))
		headas_chat(0, "error loading parameters\n");

	else if (run_parameters(&parameters, &status))
		headas_chat(0, "error running parameters\n");



	return status;
}


