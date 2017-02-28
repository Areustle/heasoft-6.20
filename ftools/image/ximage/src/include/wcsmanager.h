/*
 *  Manage and track wcs info in the form of AST "objects"
 *  Referenced by string WCS# in tcl and fortran
 *
 *  ast.h must be included before this file
 */

#include "wcsmath.h"

/*
 *  Manage reference count of a particular wcsid
 */
void wcsgetref(char *wcsid, int *refcnt);
void wcsincref(char *wcsid);
void wcsdecref(char *wcsid);
/*
 *  Get AstFrameSet for a particular wcsid
 */
AstFrameSet *getwcs(char *wcsid);
/*
 *  Get AstFrameSet for a particular id
 *  If 'wcs#' given assume wcsid, otherwise assume mapid,
 *  retrieve wcsid from mapid and use that to get wcs data structure
 */
AstFrameSet *getmapwcs(char *id);
/*
 *  Print status of all wcsid's
 */
void prallwcs();
/*
 *  Wcsid "construction" routines
 */
void genimgwcs(char *filename, int extnum, int wcslen, char *wcsid, int *status);
void genckeywcs(int *naxes, char *ctype1, char *ctype2, char *cunit1, 
                char *cunit2, double *crval, double *crpix, double *cdelt,
                double crota2, int equinox, int wcslen, char *wcsid, 
		int *status);
void genhdrwcs (char *mapid, int wcslen, char *wcsid, int *status);
/*
 *  Get frame index which corresponds to frameid string
 *  Returns 0 if not found
 */
int getframeidx(AstFrameSet *wcsinfo, char *frameid);
/*
 *  Return unit of frame
 */
const char *getframeunit(AstFrame *frame);
/*
 *  Setup frameset Base/Current frames based on input domains
 */
int setupframeset(AstFrameSet *wcsinfo, char *baseframeid, char *curframeid);
/*
 *  Use frameset to convert from indomain frame to outdomain frame
 */
void wcscnvbeg(char *wcsid, char *inframeid, char *outframeid, 
               int *wcscnvidx, int *status);
void wcscnvd(int wcscnvidx, double in1, double in2, 
                            double *out1, double *out2);
void wcscnvend(int wcscnvidx);
/*
 *  Convert between image (GRID) and detector (PIXEL) coordinates
 */
void wcsimgpix(char *wcsid, double *ximg, double *yimg, double *xpix,
               double *ypix, int forward, int *status);
/*
 *  Assign coordinate keys in internal header associated with mapid
 *    to values stored in wcs frameset
 */
void wcstohdr(char *wcsid, char *mapid, int *status);

/*
** mimic the cfitsio routine ffgiwcs
*/
void getwcscrota( AstFitsChan *fitschan, int lataxis,
                  double* crota2, double* cd1_1, double* cd2_2 );
    
/*
 *  Define detector coordinate frame in existing WCS frameset
 *    Domain = PIXEL
 */
void wcssetdet(char *wcsid, int deqc, double *imgref, double *detref,
               double *begzm, int *status);
/*
 *  Get info on the current frame of a particular wcsid
 */
void wcsfrminfo(char *wcsid, char *system, char *projection, char *xlab,
                char *ylab, char *unit, double *equinox);
