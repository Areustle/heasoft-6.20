/* ftimgcalc.h - header definitions for ftimgcalc */

/* Global includes for WCS libraries */
/* NEW WCS Library */
#include <wcslib/wcs.h>
#include <wcslib/wcshdr.h>

/* 
 * This structure represents one coordinate of one coordinate system
 * It contains the names of the coordinates themselvees, plus all 
 * the variable names that can be used in an expression.
 *
 */
struct coord_struct {
  char ctype[FLEN_CARD];    /* "RA" - coordinate axis label from CTYPEi */
  char cunit[FLEN_CARD];    /* "deg" - units of axis i */
  char vname[FLEN_CARD];    /* "V1" - variable name in expression */
  char name[FLEN_CARD];     /* "V1.RA" - expression name */
  char altname[FLEN_CARD];  /* "V1.RA.X" - alternate coordinate expression name */
  char pixname[FLEN_CARD];  /* "V1.P1" - pixel expression name */
  char intname[FLEN_CARD];  /* "V1.X1" - intermediate coordinate expression name */
  char natname[FLEN_CARD];  /* "V1.THETA" - natural coordinate expression name */

  char newname[FLEN_CARD];    /* "V1_RA_X" - replacement expression name (alt required) */
  char newpixname[FLEN_CARD]; /* "V1_P1" - replacement pixel expression name */
  char newintname[FLEN_CARD]; /* "V1_X1" - replacment intermediate coord name */
  char newnatname[FLEN_CARD]; /* "V1_THETA" - replacement natural coord name */
  char alt[2];                /* "X" - name of alternate coordinate system */

  int do_pix, pix_col;        /* Do this coordinate? / column number for it */
  int do_int, int_col;        /* Do this coordinate? / column number for it */
  int do_nat, nat_col;        /* Do this coordinate? / column number for it */
  int do_cel, cel_col;        /* Do this coordinate? / column number for it */
};


#define MAX_EXPR_LEN 1024

/* Macro: is char c part of an identifier token? */
#define isident(c) (isalnum(c) || (c == '_'))

/* fits_cell.c */
extern int fits_copy_image_cell1(fitsfile *fptr, fitsfile *newptr, char *colname,
				 long rownum, int *status);
extern int fits_copy_cell_image1(fitsfile *fptr, fitsfile *newptr, char *colname,
				 long rownum, int *status);


/* fits_translate_keyword.c */
extern int fits_translate_keyword(char *inrec, char *outrec, char *patterns[][2], 
			   int npat, int n_value, int n_offset, int n_range,
			   int *pat_num, int *i, int *j, int *m, int *n,
			   int *status);
extern int fits_translate_keywords(fitsfile *infptr, fitsfile *outfptr, int firstkey,
			    char *patterns[][2], int npat, int n_value, int n_offset,
			    int n_range, int *status);

/* wcs.c */
extern int make_wcs_images(fitsfile *inptr, fitsfile *outptr, 
			   char *expr, char *varname, int *status);

/* util.c */
extern
int copy_wcs_imcolumn2imcolumn(fitsfile *inptr, fitsfile *outptr,
			       int fromcol, int tocol,
			       int *status);
extern
int copy_wcs_imcolumn2pixlist(fitsfile *inptr, fitsfile *outptr,
			      int fromcol, int tocol, int axis,
			      int *status);
extern
int copy_nonwcs_img2img(fitsfile *inptr, fitsfile *outptr, int *status);

extern
int check_open_file(fitsfile **fptr, char *filename, 
		    int *firsthdu, int *hdutype, 
		    int *status);
extern
int increment_image_ext(fitsfile *fptr, 
			int row, int replicate, 
			int *firsthdu, int *hdutype, int *hdunum,
			int *status);
extern
int update_image_params(char *colname, int icol, int ncols,
			char *parms_wcscoordimage,
			char *parms_bunit,
			char *parms_otherext,
			int *wcscoordimagenum,
			int *unitscol);
extern
int check_image_params(char *parms_wcscoordimage,
		       char *parms_bunit,
		       char *parms_otherext,
		       int wcscoordimagenum,
		       int unitscol,
		       fitsfile *otherfile);
extern
int choose_tform(fitsfile *fptr, char *expr, int firsttype);
extern
int apply_units_keyword(fitsfile *fptr, 
			char *parms_bunit, int unitscol,
			int outcolnum);
extern
int print_col_value(fitsfile *fptr, int colnum, int row,
		    int chatter, fitsfile *outfile,
		    char *resultcol, char *bunit);

/* parse.c */
extern
int repstr(char *result, char *haystack, char *needle, char *newneedle);

extern
char *find_var_expr(char *expr, char *subexpr);

extern
int rewrite_expr(char *expr, struct coord_struct *coord, int naxis);
