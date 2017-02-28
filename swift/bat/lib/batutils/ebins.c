#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "headas.h"
#include "headas_utils.h"
#include "bat_gswdev.h"

/*
 * ebins.c - BAT ground software
 * C. B. Markwardt
 * 
 * Code to read energy bin edges from a file, either as
 * a FITS EBOUNDS extension or as an ASCII comma delimited list.
 * 
 * The main routine is read_ebins(), which either calls 
 *   read_ebounds() - if the input is a FITS file
 *   parse_franges() - if the input is a list or an @textfile.dat
 *
 */ 

/* ----------------------------------------------------------------- */
/* Parse a single energy range.  Can't use the CFITSIO routine because
   the input is float, not int */
int parse_frange(char *crange, float *fmin, float *fmax)
{
  char dummy[100];
  *fmin = 0.0;
  *fmax = 0.0;

  if ( (sscanf(crange, "%f-%f", fmin, fmax)) == 2 ) {
    return 3;
  } else if ( (sscanf(crange, "%f%[-]", fmin, dummy)) == 2 ) {
    return 1;    /* Minimum only was found */
  } else if ( (sscanf(crange, "-%f", fmax)) == 1 ) {
    return 2;    /* Maximum only was found */
  } else if ( (crange[0] == '-') && 
	      ((crange[1] == 0) || isspace(crange[1]) || (crange[1] == ','))){
    return 0;    /* Only a dash was found (followed by null, space or comma) */
  }

  return RANGE_PARSE_ERROR;
}

/* ----------------------------------------------------------------- */
/* Parse a group of energy ranges, separated by commas.  Intermixed
   white space is ignored. */
int parse_franges(char *crange, float *fmin, float *fmax, 
		  int nranges, float defmin, float defmax)
{
  char *ptr;
  int flag, curbin = 0;

  if (nranges <= 0) return 0;
  if ((fmin == 0) || (fmax == 0) || (crange == 0)) return 0;

  for (curbin=0, ptr=crange; (*ptr) && (curbin < nranges); curbin++) {
    /* Parse single range */
    flag = parse_frange(ptr, fmin+curbin, fmax+curbin);

    /* Neither part of range parse was successful - return error */
    if (flag == (RANGE_PARSE_ERROR)) return 0;

    /* Minimum only was found */
    if ((flag == 1) || (flag == 0)) fmax[curbin] = defmax;
    /* Maximum only was found */
    if ((flag == 2) || (flag == 0)) fmin[curbin] = defmin;

    /* Scan over the comma */
    while (*ptr && (*ptr != ',')) ptr++;
    if (*ptr) ptr++;
    /* Skip any white space (including carriage returns, etc) */
    while (*ptr && isspace(*ptr)) ptr++;
  }

  /* Overflow - return error */
  if (curbin == nranges) return 0;

  return curbin;
}

/* ----------------------------------------------------------------- */
/* Read energy bins from input.  Input can be one of three things:
     0. Check for CALDB
     1. explicitly listed in the parameter
     2. FITS EBOUNDS extension with energy bounds
     3. ASCII file with energy bounds
*/
int read_ebins(char *ebinlist, float *fmin, float *fmax, int nmaxbins,
	       char *coltype)
{
  int i;
  long int nbins, nchars;
  char efilelist[16384];
  char *method = "unknown";
  FILE *afile;
  fitsfile *ebinfile;
  int status;

  nbins = 0;
  if ((ebinlist == 0) || (fmin == 0) || (fmax == 0) || (nmaxbins <= 0))
    return 0;

  /* First attempt - read energy bins from parameter itself */
  headas_chat(5, "Attempting direct parse of energy bins\n");
  method = "direct parse";
  nbins = parse_franges(ebinlist, fmin, fmax, nmaxbins, 0.0, 65536.0);
  if (nbins > 0) goto DONE;

  /* Second attempt - read energy bins from FITS EBOUNDS extension */
  headas_chat(5, "Attempting read of FITS energy bins\n");
  status = 0;
  fits_open_file(&ebinfile, ebinlist, READONLY, &status);
  /* Move to EBOUNDS extension */
  if (status == 0) {

    method = "FITS file";
    read_ebounds(ebinfile, &nbins, nmaxbins, fmin, fmax, &status);
    
    if (status != 0) {
      fprintf(stderr, "ERROR: could not read energy bins from %s\n", ebinlist);
      status = 0;
      nbins = 0;
    }

    /* If we are using a response matrix as input, then the input column
       name for energy should be called ENERGY.  */
    if (strcasecmp(coltype,"ENERGY") != 0) {
      fprintf(stderr, 
	      "\n"
	      "WARNING: You have asked for ENERGY bins from file:\n"
	      "    %s\n"
	      "  but the input 'energy' column name is named '%s' and not\n"
	      "  'ENERGY'.  This is a mismatch that will likely cause \n"
	      "  unexpected results.\n"
	      "\n",
	      ebinlist, coltype);
    }


    fits_close_file(ebinfile, &status);
    goto DONE;
  }

  /* Third attempt - read energy bins from ASCII file */
  headas_chat(5, "Attempting read of ASCII energy bins file\n");
  afile = fopen(ebinlist, "r");
  if (afile) {
    method = "ASCII file parse";
    nchars = fread(efilelist, sizeof(char), 16384, afile);
    fclose(afile);

    headas_chat(5, "   (read %d chars)\n", nchars);
    if (nchars > 0) {
      nbins = parse_franges(efilelist, fmin, fmax, nmaxbins, 0.0, 65536.0);
      if (nbins > 0) goto DONE;
    }
    fprintf(stderr, "ERROR: could not read ASCII energy bins from %s\n",
	    ebinlist);
  }

  fprintf(stderr, "ERROR: could not parse energy ranges or energy bin file\n");
  nbins = 0;

 DONE:
  if (nbins > 0) {
    headas_chat(5, "  ...found %d energy ranges by %s\n", nbins, method);
    for (i=0; i<nbins; i++) 
      headas_chat(5, "      %f    %f\n", fmin[i], fmax[i]);
  }
  return nbins;
}

/* ----------------------------------------------------------------- */
/* Read EBOUNDS extension */
int read_ebounds(fitsfile *ebinfile, long int *nbins, int nmaxbins,
		 float *fmin, float *fmax, int *status) 
{
  int fmincol, fmaxcol;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;
  if ((ebinfile == 0) || (nbins == 0) || (fmin == 0) || (fmax == 0))
    return (*status = NULL_INPUT_PTR);

  /* Move to EBOUNDS extension */
  fits_movnam_hdu(ebinfile, BINARY_TBL, "EBOUNDS", 0, status);
  if (*status) return (*status);
  
  fits_get_num_rows(ebinfile, nbins, status);
  if ((*status != 0) || (*nbins == 0) || (*nbins >= nmaxbins)) {
    fprintf(stderr, "ERROR: number of ebounds rows is too large\n");
    if (*status == 0) *status = -1;
    return (*status);
  }
  
  fits_get_colnum(ebinfile, CASEINSEN, "E_MIN", &fmincol, status);
  fits_get_colnum(ebinfile, CASEINSEN, "E_MAX", &fmaxcol, status);
  headas_chat(5, "   (reading from columns %d and %d)\n", fmincol, fmaxcol);
  fits_read_col(ebinfile, TFLOAT, fmincol, 1, 1, *nbins, 0, fmin, 0, status);
  fits_read_col(ebinfile, TFLOAT, fmaxcol, 1, 1, *nbins, 0, fmax, 0, status);

  return *status;
}
