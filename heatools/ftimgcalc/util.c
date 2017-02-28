#include <fitsio2.h>
#include <string.h>
#include <ctype.h>
#include "headas.h"

#include "ftimgcalc.h"

/* 
 * Copy column-WCS keywords from one column to another
 */
int copy_wcs_imcolumn2imcolumn(fitsfile *inptr, fitsfile *outptr,
			       int fromcol, int tocol,
			       int *status) 
{
  /* Table-to-table keyword translation table  
     (used to rename the column numbers) */
  /*                        INPUT      OUTPUT  */
  /*                       01234567   01234567 */
  char *patterns[][2] = {{"iCTYPn",  "iCTYPn"},  /* Coordinate labels */
			 {"iCTYna",  "iCTYna"},
			 {"iCUNIn",  "iCUNIn"},  /* Coordinate units */
			 {"iCUNna",  "iCUNna"},
			 {"iCRVLn",  "iCRVLn"},  /* WCS keywords */
			 {"iCRVna",  "iCRVna"},
			 {"iCDLTn",  "iCDLTn"},
			 {"iCDEna",  "iCDEna"},
			 {"iCRPXn",  "iCRPXn"},
			 {"iCRPna",  "iCRPna"},
			 {"ijPCna",  "ijPCna"},
			 {"ijCDna",  "ijCDna"},
			 {"iVn_ma",  "iVn_ma"},
			 {"iSn_ma",  "iSn_ma"},
			 {"iCRDna",  "iCRDna"},
			 {"iCSYna",  "iCSYna"},
			 {"iCROTn",  "iCROTn"},
			 {"iCNAna",  "iCNAna"},
			 {"WCAXna",  "WCAXna"},
			 {"WCSNna",  "WCSNna"},
			 {"LONPna",  "LONPna"}, /* Specialty WCS keywords */
			 {"LATPna",  "LATPna"}, 
			 {"EQUIna",  "EQUIna"},
			 {"MJDOBn",  "MJDOBn"},
			 {"MJDAn",   "MJDAn" },
			 {"RADEna",  "RADEna"},
			 {"DAVGn",   "DAVGn" },
			 {"*",       "-"     }}; /* Do not recopy everything */
  int npat = sizeof(patterns)/sizeof(patterns[0][0])/2;

  fits_translate_keywords(inptr, outptr, 9, patterns, npat,
			  fromcol, tocol-fromcol, 0, status);
  return (*status);
}


/* 
 * Convert image column WCS keywords to pixel list keywords
 */
int copy_wcs_imcolumn2pixlist(fitsfile *inptr, fitsfile *outptr,
			      int fromcol, int tocol, int axis,
			      int *status) 
{
  /* Table-to-table keyword translation table  
     (used to rename the column numbers) */
  /*                        INPUT      OUTPUT  */
  /*                       01234567   01234567 */
  char *patterns[][2] = {{"iCTYPn",  "TCTYPn"},  /* Coordinate labels */
			 {"iCTYna",  "TCTYna"},
			 {"iCUNIn",  "TCUNIn"},  /* Coordinate units */
			 {"iCUNna",  "TCUNna"},
			 {"iCRVLn",  "TCRVLn"},  /* WCS keywords */
			 {"iCRVna",  "TCRVna"},
			 {"iCDLTn",  "TCDLTn"},
			 {"iCDEna",  "TCDEna"},
			 {"iCRPXn",  "TCRPXn"},
			 {"iCRPna",  "TCRPna"},
			 {"ijPCna",  "TPn_ja"},
			 {"ijCDna",  "TCn_ja"},
			 {"iVn_ma",  "TVn_ma"},
			 {"iSn_ma",  "TSn_ma"},
			 {"iCRDna",  "TCRDna"},
			 {"iCSYna",  "TCSYna"},
			 {"iCROTn",  "TCROTn"},
			 {"WCAXna",  "-"     },
			 {"WCSNna",  "TWCSna"},
			 {"LONPna",  "LONPna"}, /* Specialty WCS keywords */
			 {"LATPna",  "LATPna"}, 
			 {"EQUIna",  "EQUIna"},
			 {"MJDOBn",  "MJDOBn"},
			 {"MJDAn",   "MJDAn" },
			 {"RADEna",  "RADEna"},
			 {"DAVGn",   "DAVGn" },
			 {"*",       "-"     }}; /* Do not recopy everything */
  char subpat[100][FLEN_CARD];
  int i, j;
  int npat = sizeof(patterns)/sizeof(patterns[0][0])/2;

  if (axis > 0) {
    j = 0;
    for (i=0; i<npat; i++) {
      if (patterns[i][0][0] == 'i') {
	strcpy(subpat[j], patterns[i][0]);
	subpat[j][0] = axis + '0';
	patterns[i][0] = subpat[j];
	j++;
      }
    }
  }

  fits_translate_keywords(inptr, outptr, 9, patterns, npat,
			  fromcol, tocol-fromcol, 0, status);
  return (*status);
}

/*
 * Copy the non-WCS keywords from input to output 
 * 
 * Output keywords are written with fits_update_key()
 */
int copy_nonwcs_img2img(fitsfile *inptr, fitsfile *outptr, int *status)
{
    int nrec, nkeys, nmore;
    char rec[FLEN_CARD];
    int i = 0, j = 0, n = 0, m = 0;
    int pat_num = 0;
    char outrec[FLEN_CARD];
    int firstkey = 1;
    int npat = 0;
    /* This set of patterns will remove all FITS and WCS-related keywords,
       but copy everything else */
    char *patterns[][2] = {
			   {"BSCALE",  "-"  },  /* Standard FITS keywords */
			   {"BZERO",   "-"  },
			   {"BUNIT",   "-"  },
			   {"BLANK",   "-"  },
			   {"DATAMIN", "-"  },
			   {"DATAMAX", "-"  },
			   {"CTYPEi",  "-"  },  /* Coordinate labels */
			   {"CTYPEia", "-"  },
			   {"CUNITi",  "-"  },  /* Coordinate units */
			   {"CUNITia", "-"  },
			   {"CRVALi",  "-"  },  /* WCS keywords */
			   {"CRVALia", "-"  },
			   {"CDELTi",  "-"  },
			   {"CDELTia", "-"  },
			   {"CRPIXj",  "-"  },
			   {"CRPIXja", "-"  },
			   {"PCi_ja",  "-"  },
			   {"CDi_ja",  "-"  },
			   {"PVi_ma",  "-"  },
			   {"PSi_ma",  "-"  },
			   {"WCSAXESa","-"  },
			   {"WCSNAMEa","-"  },
			   {"CRDERia", "-"  },
			   {"CSYERia", "-"  },
			   {"CROTAi",  "-"  },

			   {"LONPOLEa","-"  },
			   {"LATPOLEa","-"  },
			   {"EQUINOXa","-"  },
			   {"MJD-OBS", "-"  },
			   {"MJD-AVG", "-"  },
			   {"RADESYSa","-"  },
			   {"CNAMEia", "-"  },
			   {"DATE-AVG","-"  },

			   {"NAXISi",  "-"  },  /* Remove structural keywords*/
                           {"SIMPLE",   "-" },
			   {"XTENSION", "-" },
			   {"BITPIX",   "-" },
			   {"NAXIS",    "-" },
			   {"PCOUNT",  "-"  },
			   {"GCOUNT",  "-"  },
			   {"EXTEND",  "-"  },
			   {"EXTNAME", "-"  },
			   {"EXTVER",  "-"  },
			   {"EXTLEVEL","-"  },
			   {"CHECKSUM","-"  },
			   {"DATASUM", "-"  },
			   {"*",       "+"  }}; /* copy all other keywords */

    npat = sizeof(patterns)/sizeof(patterns[0][0])/2;

    if (*status > 0)
        return(*status);

    ffghsp(inptr, &nkeys, &nmore, status);  /* get number of keywords */

    for (nrec = firstkey; nrec <= nkeys; nrec++) {
      outrec[0] = '\0';

      ffgrec(inptr, nrec, rec, status);

      fits_translate_keyword(rec, outrec, patterns, npat, 
			     0, 0, 0, 
			     &pat_num, &i, &j, &m, &n, status);
      
      if (outrec[0]) {
	char keyname[10];
	int k;

	strncpy(keyname, outrec, 8);
	/* Remove any trailing spaces */
	for (k=9; k>0 && (keyname[k] == ' ' || keyname[k] == 0); k--) {
	  keyname[k] = 0;
	}

	fits_update_card(outptr, keyname, outrec, status);
      }

      rec[8] = 0; outrec[8] = 0;

      if (*status) break;
    }	

    return(*status);
}

/*
 * check_open_file - open a file if necessary, otherwise pass thru
 * 
 * fitsfile **fptr - pointer to (fitfile*) handle
 *                   (*fptr) == 0: file should be opened
 *                   (*fptr) != 0: pass through
 * char *filename - name of file to open, if needed
 * int *firsthdu - upon return, contains HDU number of opened file
 * int *hdutype - upon return, contains HDU-type of opened extension
 * int *status - CFITSIO return code
 * 
 * RETURNS: CFITSIO return code
 */
int check_open_file(fitsfile **fptr, char *filename, 
		    int *firsthdu, int *hdutype, 
		    int *status)
{
  if (*status) return *status;
  if (fptr == 0) { 
    return (*status == NULL_INPUT_PTR);
  }

  /* If FITS pointer is null, then attempt to open the file */
  if (*fptr == 0) {
    if (fits_open_image(fptr, filename, READONLY, status)) {
      return *status;
    }

    fits_get_hdu_num(*fptr, firsthdu);
    fits_get_hdu_type(*fptr, hdutype, status);
  }

  return *status;
}

/* 
 * increment_image_ext - move to "next" image extension in input file
 * 
 * fitsfile *fptr - FITS file to operate on
 *                  if (row == 1) then do not advance
 *                  otherwise, advance to next image HDU
 * int row - desired output "row" number
 * int replicate - if true, then replicate images if input is too small
 * int *firsthdu - points to first HDU number
 * int *hdutype - upon return contains the HDU-type of opened extension
 * int *status - CFITSIO return code
 * 
 * RETURN: CFITSIO return code
 */
int increment_image_ext(fitsfile *fptr, 
			int row, int replicate, 
			int *firsthdu, int *hdutype, int *hdunum,
			int *status)
{
  if (*status) return *status;
  *hdunum = -1;

  if (row > 1) {
    /* We are already positioned correctly the first time */
    fits_movrel_hdu(fptr, +1, hdutype, status);
  }

  /* Note that the first time around we should have succeeded!!! */
  while (*hdutype != IMAGE_HDU && *status == 0) {
    fits_movrel_hdu(fptr, +1, hdutype, status);
  }

  /* We failed to find an image! Either we bomb out, or we recycle
     back to the first image, depending on the 'replicate'
     parameter */
  if (*status) {
    if (!replicate) { return *status; }

    *status = 0;
    /* Replicate was set, so we recycle back to the starting point */
    fits_movabs_hdu(fptr, *firsthdu, hdutype, status);
  }

  fits_get_hdu_num(fptr, hdunum);
  if (*hdunum == -1) { return *status = BAD_HDU_NUM; }
  
  return *status;
}

/* 
 * update_image_params - update image flags based on input
 *
 * char *colname - current column name
 * int icol - current input image number
 * int ncols - total number of columns in output table
 * char *parms_wcscoordimage - requested WCS image name/number
 * char *parms_bunit - requested units image name/number
 * char *parms_otherext - requested image name/number for "other" extensions
 * int *wcscoordimagenum - if this is requested WCS image name/number, then
 *         *wcscoordimagenum is set to ncols
 * int *unitscol - if this is requested units image name/number, then
 *         *unitscol is set to ncols
 * 
 * RETURN: 1 if this is the "otherext" image, otherwise 0
 */
int update_image_params(char *colname, int icol, int ncols,
			char *parms_wcscoordimage,
			char *parms_bunit,
			char *parms_otherext,
			int *wcscoordimagenum,
			int *unitscol)
{
  int otherfile = 0;
  
  if (strcasecmp(parms_wcscoordimage, colname) == 0) {
    *wcscoordimagenum = ncols;
    headas_chat(5, "  ... wcsimage = %s , col = %d...\n", 
		parms_wcscoordimage, *wcscoordimagenum);
  }
  if ((parms_wcscoordimage[0] == ':' || parms_wcscoordimage[0] == '+') &&
      ((strcasecmp(parms_wcscoordimage+1,colname) == 0) ||
       (isdigit(parms_wcscoordimage[1]) && 
	atoi(parms_wcscoordimage+1) == icol)) ) {
    *wcscoordimagenum = ncols;
    headas_chat(5, "  ... wcsimage = %s , col = %d...\n", 
		parms_wcscoordimage, *wcscoordimagenum);
  }
  if ((parms_bunit[0] == ':' || parms_bunit[0] == '+') && 
      ((strcasecmp(parms_bunit+1,colname) == 0) ||
       (isdigit(parms_bunit[1]) && atoi(parms_bunit+1) == icol)) ) {
    *unitscol = ncols;
    headas_chat(5, "  ... bunit = %s , col = %d...\n", 
		parms_bunit, *unitscol);
  }
  if ((parms_otherext[0] == ':' || parms_otherext[0] == '+') && 
      ((strcasecmp(parms_otherext+1,colname) == 0) ||
       (isdigit(parms_otherext[1]) && atoi(parms_otherext+1) == icol)) ) {
    otherfile = 1;
    headas_chat(5, "  ... otherfile = %s , col = %d...\n", 
		parms_otherext, icol);
  }
  if (strcasecmp(parms_otherext,colname) == 0) {
    otherfile = 1;
    headas_chat(5, "  ... otherfile = %s , col = %d...\n", 
		parms_otherext, icol);
  }

  return otherfile;
}

/* 
 * check_image_params - sanity check of image-related parameters
 *
 * char *parms_wcscoordimage - requested WCS image name/number
 * char *parms_bunit - requested units image name/number
 * char *parms_otherext - requested image name/number for "other" extensions
 * int wcscoordimagenum - requested WCS column number (or -1)
 * int unitscol - requested units column number (or -1)
 * fitsfile *otherfile - requested "otherfile" (or 0)
 * 
 * RETURN: CFITSIO return code (or 0 upon all sanity checks passing)
 */
int check_image_params(char *parms_wcscoordimage,
		       char *parms_bunit,
		       char *parms_otherext,
		       int wcscoordimagenum,
		       int unitscol,
		       fitsfile *otherfile)
{
  int err = COL_NOT_FOUND;

  /* Sanity checking on wcs, unit, and otherext parameters */
  if ((parms_bunit[0] == ':' || parms_bunit[0] == '+') && unitscol < 0) {
    fprintf(stderr, 
	    "ERROR: the requested units image '%s' was not\n"
	    "       specified on the command line\n", parms_bunit);
    return COL_NOT_FOUND;
  }
  if ((parms_otherext[0]) && otherfile == 0) {
    fprintf(stderr, 
	    "ERROR: the requested image '%s' for other extensions was not\n"
	    "       specified on the command line\n", parms_otherext);
    return err;
  }
  if ((parms_wcscoordimage[0]) && wcscoordimagenum < 0) {
    fprintf(stderr, 
	    "ERROR: the requested WCS image '%s' was not\n"
	    "       specified on the command line\n", 
	    parms_wcscoordimage);
    return err;
  }

  return 0;
}

/* 
 * choose_tform - choose appropriate TFORM for output based on
 *      requested TFORM and/or input image types.
 * 
 * fitsfile *fptr - table that calculation will occur on
 * char *expr - expression to be run on table
 * int firsttype - CFITSIO data type of first opened image
 * 
 * RETURN: optimal TFORM value, or -1 if failure
 */
int choose_tform(fitsfile *fptr, char *expr, int firsttype)
{
  int status = 0;
  int optimaltype, rnaxis;
  long int rnelem, rnaxes[9];
  int tform = -1;

  /* test the expression to get the natural datatype of the result */
  /* this will usually either be 41 (long) or 82 (double)          */
  fits_test_expr(fptr, expr, 9, &optimaltype, &rnelem,
		 &rnaxis, rnaxes, &status);
  
  if (optimaltype == TDOUBLE) {
    /* only write a double output image if the first input image
       is  a double or a longlong int, otherwise write a float image */
    switch (firsttype) {
    case  BYTE_IMG:     tform = 'E'; break;
    case  SBYTE_IMG:    tform = 'E'; break;
    case  SHORT_IMG:    tform = 'E'; break;
    case  USHORT_IMG:   tform = 'E'; break;
    case  LONG_IMG:     tform = 'E'; break;
    case  ULONG_IMG:    tform = 'E'; break;
    case  LONGLONG_IMG: tform = 'D'; break;
    case  FLOAT_IMG:    tform = 'E'; break;
    case  DOUBLE_IMG:   tform = 'D'; break;
    default: 
      fprintf(stderr, "ERROR: 'image datatype=%d' is an invalid integer",
	      firsttype);
      return -1;
    }
    
  } else {
    /* write an integer output image, with same bitpix  as first image */
    switch (firsttype) {
    case  BYTE_IMG:     tform = 'B'; break;
    case  SBYTE_IMG:    tform = 'S'; break;
    case  SHORT_IMG:    tform = 'I'; break;
    case  USHORT_IMG:   tform = 'U'; break;
    case  LONG_IMG:     tform = 'J'; break;
    case  ULONG_IMG:    tform = 'V'; break;
    case  LONGLONG_IMG: tform = 'K'; break;
    case  FLOAT_IMG:    tform = 'E'; break;
    case  DOUBLE_IMG:   tform = 'D'; break;
    default: 
      fprintf(stderr, "ERROR: 'image datatype=%d' is an invalid integer",
	      firsttype);
      return -1;
    }
  }   
  
  return tform;
}
  
/* 
 * apply_units_keyword - set BUNIT keyword of output table
 * 
 * fitsfile *fptr - output FITS table file
 * char *parms_bunit - requested BUNIT value
 *                     (NOTE: may be changed!)
 * int unitscol - requested input units column (or 0 if parms_bunit is correct)
 * int outcolumn - output FITS column number to change
 * 
 * RETURN: CFITSIO return code
 */
int apply_units_keyword(fitsfile *fptr, 
			char *parms_bunit, int unitscol,
			int outcolnum)
{
  int status = 0;
  char inkeyname[FLEN_CARD];
  char outkeyname[FLEN_CARD];
  char comment[FLEN_CARD] = "physical unit of image";
  
      
  headas_chat(5, "  ...applying units...\n");
  fits_make_keyn("TUNIT", outcolnum, outkeyname, &status);
  if (unitscol >= 1) {
    char bunit[FLEN_CARD];
    fits_make_keyn("TUNIT", unitscol, inkeyname, &status);
    fits_read_key(fptr, TSTRING, inkeyname, bunit,
		  comment, &status);
    if (status) {
      fprintf(stderr, 
	      "ERROR: requested image '%s' does not have a BUNIT keyword\n",
	      parms_bunit);
      return status;
    }
    strcpy(parms_bunit, bunit);
  }
      
  headas_chat(5, "    [%s] /%s\n", parms_bunit, comment);
  fits_update_key(fptr, TSTRING, outkeyname, parms_bunit, 
		  comment, &status);

  return status;
}

/* 
 * print_col_value - print out scalar result value
 * 
 * fitsfile *fptr - FITS table file to read
 * int colnum - FITS table column number
 * int row - FITS table row number
 * int chatter - chattiness level (>=2 enables console output)
 * fitsfile *outfile - output file pointer 
 *      (note: this is only checked for being set or not)
 * char *resultcol - name of output result column
 * char *bunit - name of units
 * int *status - CFITSIO return code
 * 
 * RETURN: CFITSIO return code
 */
int print_col_value(fitsfile *fptr, int colnum, int row,
		    int chatter, fitsfile *outfile,
		    char *resultcol, char *bunit)
{
  int status = 0;
  int typecode, anynul;
  long repeat, width;
  long int longnul = 0, longval;
  double dblnul = 0.0, dblval;
  
  /* Write the data to the console if desired (chatter >= 2) */
  fits_get_coltype(fptr, colnum, &typecode, &repeat,
		     &width, &status);
  if (status == 0 && repeat == 1 && chatter >= 2) {
    if (typecode == TFLOAT || typecode == TDOUBLE) {
      fits_read_col(fptr, TDOUBLE, colnum, row, 1, 1,
		    &dblnul, &dblval, &anynul, &status);
      if (status == 0 && anynul == 0 && bunit[0]) {
	headas_chat(2, "%s(%d)=%24.17g [%s]\n",
		    resultcol,row,dblval, bunit);
      } else if (status == 0 && anynul == 0) {
	headas_chat(2, "%s(%d)=%24.17g\n",resultcol,row,dblval);
      } else if (status == 0 && anynul == 1) {
	headas_chat(2, "%s(%d)=#NULL\n",resultcol,row);
      }
    } else {
      fits_read_col(fptr, TLONG, colnum, row, 1, 1,
		    &longnul, &longval, &anynul, &status);
      if (status == 0 && anynul == 0 && bunit[0]) {
	headas_chat(2, "%s(%d)=%d [%s]\n",
		    resultcol,row,longval,bunit);
      } else if (status == 0 && anynul == 0) {
	headas_chat(2, "%s(%d)=%d\n", resultcol,row,longval);
      } else if (status == 0 && anynul == 1) {
	headas_chat(2, "%s(%d)=#NULL\n",resultcol,row);
      }
    }
  }
  
  if (row == 1 && outfile == 0 && (repeat != 1 || chatter < 2)) {
    if (repeat != 1) {
      fprintf(stderr, "WARNING: outfile was set to NONE but there were too\n");
      fprintf(stderr, "         pixels to print at the console (npix = %ld).\n",
	      repeat);
    }
    if (chatter < 2) {
      fprintf(stderr, "WARNING: outfile was set to NONE but chatter must be\n");
      fprintf(stderr, "         set to 2 or higher to get console output (chatter=%d)\n",
	      chatter);
    }
    status = -1;
  }

  return status;
}
