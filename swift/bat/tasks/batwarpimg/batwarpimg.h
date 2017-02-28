/* 
 * batwarpimg
 * Definitions and declarations
 * 
 * Craig Markwardt
 *
 * $Id: batwarpimg.h,v 1.2 2006/04/04 19:37:03 craigm Exp $
 */

struct parm_struct 
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];   /* Input image file name */
  char outfile[PIL_PATH_MAX];  /* Output  */
  char distfile[PIL_PATH_MAX]; /* Distortion map file name */
  int direction;               /* Transformation direction */
#define APP_TO_TRUE 1          /*     apparent to true */
#define TRUE_TO_APP 2          /*     true to apparent */
  char rowstr[PIL_PATH_MAX];   /* Images to process */
  int copyall;                 /* Copy all extensions? */
};

struct image_info {
  double crval1, cdelt1, crpix1;
  double crval2, cdelt2, crpix2, crota2;
  char ctype1[FLEN_CARD];
  char ctype2[FLEN_CARD];
  char coordtype[5];
};

