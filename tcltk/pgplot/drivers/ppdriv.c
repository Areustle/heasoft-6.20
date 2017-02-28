
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#ifndef convex
#include <string.h>
#endif
#include <math.h>

/*
 * The following macro must enclose all function prototype arguments.
 * This allows pre-ANSI compilers to compile this code, by discarding
 * the prototype arguments if __STDC__ is not set.
 */
#ifdef __STDC__
#define ARGS(args) args
#else
#define ARGS(args) ()
#endif


/* Fortran routines */
#ifdef PG_PPU
#define PPDRIV ppdriv_
#define GRCFIL grcfil_
#define GROFIL grofil_
#define GRCTOI grctoi_
#define GRGENV grgenv_
#define GRWFCH grwfch_
#define GRWFIL grwfil_
#define GRFAO  grfao_
#define GRTRIM grtrim_
#define GRUSER gruser_
#define GRDATE grdate_
#else
#define PPDRIV ppdriv
#define GRCFIL grcfil
#define GROFIL grofil
#define GRCTOI grctoi
#define GRGENV grgenv
#define GRWFCH grwfch
#define GRWFIL grwfil
#define GRFAO  grfao
#define GRTRIM grtrim
#define GRUSER gruser
#define GRDATE grdate
#endif

extern int GRCFIL ARGS((int *));
extern int GROFIL ARGS((char *, int));
extern int GRCTOI ARGS((char *, int *, int));
extern int GRGENV ARGS((char *, char *, int *, int, int));
extern int GRWFCH ARGS((int *, char *, int));
extern int GRWFIL ARGS((int *, int *, char *, int));
extern int GRFAO ARGS((char *, int *, char *, int *, int *, int *, int *, int, int));
extern int GRTRIM ARGS((char *, int));
extern int GRTRIM ARGS((char *, int));
extern int GRUSER ARGS((char *, int *, int));
extern int GRDATE(char *, int *, int);

/* C Routines in this file */
static int grpp02 ARGS((int, int, int, int, int, int *));
static int grpp06 ARGS((int, int, int, int *));
static int grpp10 ARGS((char *, int, char *));
static int pp_nint ARGS((float));


/* have to define our own nint function */

#ifdef __STDC__
static int pp_nint(float x)
#else
static int pp_nint(x)
     float x;
#endif
{
  return (int) (x >= 0.0 ? (x + 0.5) : (x - 0.5));
}

/* *PPDRIV -- PGPLOT PPM driver */
/* + */
#ifdef __STDC__
int PPDRIV(int *ifunc, float *rbuf, int *nbuf, char *chr, int *lchr, int *mode, int chr_len)
#else
int PPDRIV(ifunc, rbuf, nbuf, chr, lchr, mode, chr_len)
     int *ifunc; float *rbuf; int *nbuf; char *chr; int *lchr; int *mode; int chr_len;
#endif
{
    /* Initialized data */

    static int cdeflt[48]	/* was [3][16] */ = { 0,0,0,255,255,255,255,0,
	    0,0,255,0,0,0,255,0,255,255,255,0,255,255,255,0,255,128,0,128,255,
	    0,0,255,128,0,128,255,128,0,255,255,0,128,85,85,85,170,170,170 };
    static int state = 0;

    /* Local variables */
    static int i, l, ll, bx, by, ix0, iy0, ix1, iy1;
    static int icol, rcol, gcol, bcol, cval;
    static char msg[128];
    static int unit;
    static int npict, userh;
    static char instr[80];
    static int userw, ctable[768]	/* was [3][256] */;
    static char filenm[128];
    static int *pixmap;


/* PGPLOT driver for Portable Pixel Map (PPM) files with 'true color'   */
/* capability.                                                          */
/*                                                                      */
/* Supported device: PPM (P6) file format                               */
/*                                                                      */
/* Device type codes: /PPM or /VPPM                                     */
/*                                                                      */
/* Default device name: pgplot.ppm.                                     */
/*                                                                      */
/* If you have more than one image to plot (i.e. use PGPAGE) with this  */
/* device, subsequent pages will be named: pgplot2.ppm, pgplot3.ppm,    */
/* etc, disrespective of the device name you specified.                 */
/* You can however bypass this by specifying a device name including a  */
/* number sign (#), which will henceforth be replaced by the pagenumber.*/
/* Example: page#.ppm will produce files page1.ppm, page2.ppm, etc.     */
/*                                                                      */
/* Default view surface dimensions are:                                 */
/* - PPM  : 850 x 680 pixels                                            */
/* - VPPM : 680 x 850 pixels                                            */
/* Default width and height can be overriden by specifying environment  */
/* variables                                                            */
/* PGPLOT_PPM_WIDTH  (default 850)                                      */
/* PGPLOT_PPM_HEIGHT (default 680)                                      */
/* The nominal scale is 85 pixels per inch.                             */
/*                                                                      */
/* Color capability:                                                    */
/*   Indices 0 to 255 are supported.                                    */
/*   Default colors for indices 0 to 15 are implemented.                */
/*   Color representation can be changed with PGSCR; color changes      */
/*   affect subsequently drawn pixels only, not previously drawn        */
/*   pixels. Thus the image is not limited to 256 different colors.     */
/*                                                                      */
/* Obtaining hardcopy: Use a PPM viewer or converter.                   */
/*=                                                                     */
/*  9-Aug-1993 - Created by Remko Scharroo                              */
/*  6-Jul-1994 - Adapted to new PGPLOT version 4.9h                     */
/*  4-Aug-1994 - Use FASTIO.                                            */
/*  9-Aug-1994 - New scheme for line plotting                           */
/* 16-Aug-1994 - Provide multi-image plotting.                          */
/* 16-Nov-1994 - Revised (T. Pearson).                                  */
/* 28-Dec-1995 - Prevent concurrent access [TJP].                       */
/* 29-Apr-1996 - Use GRCTOI to decode environment variables [TJP].      */
/*  7-Jul-2006 - Implementation in C (K. Arnaud)                        */
/*----------------------------------------------------------------------*/

   switch (*ifunc) {

/* --- IFUNC = 1, Return device name ------------------------------------- */
    case 1:
      {
        char *dev_name;
        switch(*mode) {  /* Locate the name used to select the given mode */
        case 1: default:
	  dev_name = "PPM   (PPM file, landscape orientation)";
	  break;
        case 2:
	  dev_name = "VPPM  (PPM file, portrait orientation)";
	  break;
        };
        strncpy(chr, dev_name, chr_len);
        *lchr = strlen(dev_name);
        for(i = *lchr; i < chr_len; i++) chr[i] = ' ';
      };
      break;

/* --- IFUNC = 2, Return physical min and max for plot device, and range */
/*               of color indices --------------------------------------- */

    case 2:
      rbuf[0] = 0.0;
      rbuf[1] = -1.0;
      rbuf[2] = 0.0;
      rbuf[3] = -1.0;
      rbuf[4] = 0.0;
      rbuf[5] = 255.0;
      *nbuf = 6;
      break;

/* --- IFUNC = 3, Return device resolution ------------------------------- */

    case 3:  
      rbuf[0] = 85.0;
      rbuf[1] = 85.0;
      rbuf[2] = 1.0;
      *nbuf = 3;
      break;

/* --- IFUNC = 4, Return misc device info -------------------------------- */
/*    (This device is Hardcopy, supports rectangle fill, pixel */
/*     primitives, and query color rep.) */

    case 4:
      strncpy(chr, "HNNNNRPNYN", 10);
      *lchr = 10;
      break;

/* --- IFUNC = 5, Return default file name ------------------------------- */

    case 5:
      strncpy(chr, "pgplot.ppm", 10);
      *lchr = 10;
      break;

/* --- IFUNC = 6, Return default physical size of plot ------------------- */

    case 6:
      rbuf[0] = 0.0;
      rbuf[1] = (float) (bx - 1);
      rbuf[2] = 0.0;
      rbuf[3] = (float) (by - 1);
      *nbuf = 4;
      break;

/* --- IFUNC = 7, Return misc defaults ----------------------------------- */

    case 7:
      rbuf[0] = 1.0;
      *nbuf = 1;
      break;

/* --- IFUNC = 8, Select plot -------------------------------------------- */

    case 8:
      break;

/* --- IFUNC = 9, Open workstation --------------------------------------- */

    case 9:

/*     -- check for concurrent access */
      if (state == 1) {
	fprintf(stderr, "PGPLOT, a PGPLOT PPM file is already open\n");
	rbuf[0] = 0.0;
	rbuf[1] = 0.0;
	return 0;
      }
/*     -- dimensions of plot buffer */
      userw = 0;
      userh = 0;
      GRGENV("PPM_WIDTH", instr, &l, 9, 80);
      ll = 1;
      if (l > 0) {
	userw = GRCTOI(instr, &ll, l);
      }
      GRGENV("PPM_HEIGHT", instr, &l, 10, 80);
      ll = 1;
      if (l > 0) {
	userh = GRCTOI(instr, &ll, l);
      }
      if (*mode == 1) {
/*     -- Landscape */
	bx = 850;
	if (userw >= 8) {
	    bx = userw;
	}
	by = 680;
	if (userh >= 8) {
	    by = userh;
	}
      } else {
/*     -- Portrait */
	bx = 680;
	if (userh >= 8) {
	    bx = userh;
	}
	by = 850;
	if (userw >= 8) {
	  by = userw;
	}
      }
      npict = 1;
/*     -- Initialize color table */
      for (i = 0; i <= 15; ++i) {
	ctable[i * 3] = cdeflt[i * 3];
	ctable[i * 3 + 1] = cdeflt[i * 3 + 1];
	ctable[i * 3 + 2] = cdeflt[i * 3 + 2];
      }
      for (i = 16; i <= 255; ++i) {
	ctable[i * 3] = 128;
	ctable[i * 3 + 1] = 128;
	ctable[i * 3 + 2] = 128;
      }

      strncpy(filenm, chr, 128);
      grpp10(filenm, npict, msg);
      unit = GROFIL(msg, 128);

      rbuf[0] = (float) unit;
      if (unit < 0) {
	fprintf(stderr, "PGPLOT, Cannot open output file for PPM plot\n");
	rbuf[1] = 0.0;
      } else {
	rbuf[1] = 1.0;
	state = 1;
      }
      break;

/* --- IFUNC=10, Close workstation --------------------------------------- */

    case 10:
      state = 0;
      break;

/* --- IFUNC=11, Begin picture ------------------------------------------- */

    case 11:
      bx = pp_nint(rbuf[0]) + 1;     
      by = pp_nint(rbuf[1]) + 1;
      pixmap = (int *) malloc(sizeof(int)*bx*by);
      if ( pixmap == NULL ) {
	fprintf(stderr, "PGPLOT, Failed to allocate plot buffer.\n");
	bx = 0;
	by = 0;
      }
/*     -- initialize to zero (background color) */
      if (pixmap != NULL) {
	for (i=0; i<bx*by; i++) pixmap[i] = 0;
      }
      if (npict > 1) {
	grpp10(filenm, npict, msg);
	unit = GROFIL(msg, 128);
	if (unit < 0) {
	    fprintf(stderr, "PGPLOT, Cannot open output file for PPM plot.\n");
	}
      }
      break;

/* --- IFUNC=12, Draw line ----------------------------------------------- */

    case 12:
      {
	float d;
	int is, ix, iy, idx, idy;

        ix0 = pp_nint(rbuf[0]) + 1;
        ix1 = pp_nint(rbuf[2]) + 1;
        iy0 = by - pp_nint(rbuf[1]);
        iy1 = by - pp_nint(rbuf[3]);

        idx = ix1 - ix0;
        idy = iy1 - iy0;

        if (idx == 0 && idy == 0) {
          grpp02(ix0, iy0, cval, bx, by, pixmap);
        } else if ( abs(idy) > abs(idx) ) {
	  d = idx / (float) idy;
	  is = (idy < 0) ? -1 : 1;
	  for (iy = iy0; is < 0 ? iy >= iy1 : iy <= iy1; iy += is) {
	    ix = pp_nint(ix0 + (iy - iy0) * d);
            grpp02(ix, iy, cval, bx, by, pixmap);
	  }
        } else {
	  d = idy / (float) idx;
	  is = (idx < 0) ? -1 : 1;
	  for (ix = ix0; is < 0 ? ix >= ix1 : ix <= ix1; ix += is) {
	    iy = pp_nint(iy0 + (ix - ix0) * d);
            grpp02(ix, iy, cval, bx, by, pixmap);
	  }
        }

        break;
      }

/* --- IFUNC=13, Draw dot ------------------------------------------------ */

    case 13:
      ix0 = pp_nint(rbuf[0]) + 1;
      iy0 = by - pp_nint(rbuf[1]);
      grpp02(ix0, iy0, cval, bx, by, pixmap);
      break;

/* --- IFUNC=14, End picture --------------------------------------------- */

    case 14:
      {
        if (unit >= 0) {
	  grpp06(unit, bx, by, pixmap);
	  GRCFIL(&unit);
        }
        ++npict;
        free(pixmap);
        break;
      }

/* --- IFUNC=15, Select color index -------------------------------------- */

    case 15:
      icol = pp_nint(rbuf[0]);
      if (icol < 0 || icol > 255 ) icol = 1;
      rcol = ctable[3*icol];
      gcol = ctable[3*icol+1];
      bcol = ctable[3*icol+2];
      cval = rcol + 256*(gcol + 256*bcol);
      break;

/* --- IFUNC=16, Flush buffer. ------------------------------------------- */
/*    (Not used.) */

    case 16:
      break;

/* --- IFUNC=17, Read cursor. -------------------------------------------- */
/*    (Not implemented: should not be called) */

    case 17:
      break;

/* --- IFUNC=18, Erase alpha screen. ------------------------------------- */
/*    (Not implemented: no alpha screen) */

    case 18:
      break;

/* --- IFUNC=19, Set line style. ----------------------------------------- */
/*    (Not implemented: should not be called) */

    case 19:
      break;

/* --- IFUNC=20, Polygon fill. ------------------------------------------- */
/*    (Not implemented: should not be called) */

    case 20:
      break;

/* --- IFUNC=21, Set color representation. ------------------------------- */

    case 21:
      i = pp_nint(rbuf[0]);
      if (i >= 0 && i <= 255) {
        ctable[i * 3] = pp_nint(rbuf[1] * 255);
        ctable[i * 3 + 1] = pp_nint(rbuf[2] * 255);
        ctable[i * 3 + 2] = pp_nint(rbuf[3] * 255);
      }
      break;

/* --- IFUNC=22, Set line width. ----------------------------------------- */
/*    (Not implemented: should not be called) */

    case 22:
      break;

/* --- IFUNC=23, Escape -------------------------------------------------- */
/*    (Not implemented: ignored) */

    case 23:
      break;

/* --- IFUNC=24, Rectangle fill ------------------------------------------ */

    case 24:
      {
	int ix, iy;
        ix0 = pp_nint(rbuf[0]) + 1;
        ix1 = pp_nint(rbuf[2]) + 1;
        iy1 = by - pp_nint(rbuf[1]);
        iy0 = by - pp_nint(rbuf[3]);
        for (iy = iy0; iy <= iy1; ++iy) {
	  for (ix = ix0; ix <= ix1; ++ix) {
            grpp02(ix, iy, cval, bx, by, pixmap);
	  }
        }
        break;
      }

/* --- IFUNC=25, Not implemented ----------------------------------------- */

    case 25:
      break;

/* --- IFUNC=26, Line of pixels ------------------------------------------ */

    case 26:
      {
        int ix, iy, n, ic, r, g, b;
	ix = pp_nint(rbuf[0])+1;
	iy = by - pp_nint(rbuf[1]);
	for (n = 2; n < *nbuf; ++n) {
	  ic = rbuf[n];
          r = ctable[3*ic];
          g = ctable[3*ic+1];
          b = ctable[3*ic+2];
	  ic = r + 256*(g + 256*b);
          grpp02(ix+n-2, iy, ic, bx, by, pixmap);
	}
        break;
      }

/* --- IFUNC=27, Not implemented ----------------------------------------- */

    case 27:
      break;

/* --- IFUNC=28, Not implemented ----------------------------------------- */

    case 28:
      break;

/* --- IFUNC=29, Query color representation. ----------------------------- */

    case 29:
      i = rbuf[0];
      rbuf[1] = ctable[i * 3] / 255.f;
      rbuf[2] = ctable[i * 3 + 1] / 255.f;
      rbuf[3] = ctable[i * 3 + 2] / 255.f;
      *nbuf = 4;
      break;

/* --- IFUNC=?, ---------------------------------------------------------- */

    default:
      fprintf(stderr, "PGPLOT, Ignoring unimplemented opcode=%d in GIF device driver.\n", *ifunc);
      *nbuf = -1;
      break;
   };

   return 0;

/* ----------------------------------------------------------------------- */
} /* ppdriv_ */


/* *GRPP02 -- PGPLOT PPM driver, load a value into the pixmap array */
/* + */
#ifdef __STDC__
int grpp02(int ix, int iy, int cval, int bx, int by, int *pixmap)
#else
int grpp02(ix, iy, cval, bx, by, pixmap)
     int ix; int iy; int cval; int bx; int by; int *pixmap;
#endif
{


    /* Local variables */
    static int ipos;

/* Load the value ICOL into PIXMAP(IX,IY) */

/* Arguments: */
/*  IX, IY          (input): Coordinates */
/*  CVAL            (input): Color index */
/*  BX, BY          (input): Size of PIXMAP */
/*  PIXMAP   (input/output): The image data buffer. */
/* ----------------------------------------------------------------------- */

    ipos = ix-1 + (iy-1)*bx;
    if ( ipos < 0 || ipos >= bx*by ) {
      fprintf(stderr, "PGPLOT, attempt to write outside PIXMAP array bounds, ix = %d, iy = %d\n", ix, iy);
    } else {
      if ( pixmap != NULL ) pixmap[ipos] = cval;
    }

    return 0;

} /* grpp02 */

#ifdef __STDC__
int grpp06(int unit, int bx, int by, int *pixmap)
#else
int grpp06(unit, bx, by, pixmap)
     int unit; int bx; int by; int *pixmap;
#endif
{

    /* Local variables */
    static int i, n, ier, ibuf, l1, l2, bad;
    static char buf[498];
    static char head[128], user[20], today[20];

/* Write PPM image to UNIT. */

/* Arguments: */
/* UNIT   (input): Output unit */
/* BX,BY  (input): `Screen' size */
/* PIXMAP (input): Image data */
/* -- */
/* 10-Aug-1993 - Created by Remko Scharroo */
/* 16-Nov-1994 - Rewritten by T. Pearson */
/* ----------------------------------------------------------------------- */

/* Write the header (magic number = P6) */

    GRUSER(user, &l1, 20);
    strncat(user, " ", 1);
    l1++;
    GRDATE(today, &l2, 20);

    strcpy(head, "P6 # PGPLOT PPM image ");
    strncat(head, user, l1);
    strncat(head, today, l2);
    sprintf(head+22+l1+l2, "\n%5d %5d\n%3d\n", bx, by, 255);

    n = 22 + l1 + l2 + 17;
    ier = GRWFCH(&unit, head, n);
    if ( ier != n ) {
      fprintf(stderr, "PGPLOT, failed to write PPM header\n");
    }

/* Write the pixel data as R, G, B components */

    n = bx * by;
    ibuf = 0;
    bad = 0;

    for (i=0; i<n; i++) {

      buf[ibuf]   = (char) pixmap[i] % 256;
      buf[ibuf+1] = (char) (pixmap[i] / 256 % 256);
      buf[ibuf+2] = (char) (pixmap[i] / 65536);
      ibuf += 3;

      if (ibuf >= 498) {
	ier = GRWFCH(&unit, buf, 498);
	if (ier != ibuf) bad = 1;
	ibuf = 0;
      }
    }

    if (ibuf > 0) {
	ier = GRWFCH(&unit, buf, ibuf);
	if (ier != ibuf) bad = 1;
	ibuf = 0;
    }
    if (bad == 1) fprintf(stderr, "PGPLOT, failed writing PPM data\n");

    return 0;

} /* grpp06 */

/* *GRPP10 -- Replace # in filename by picture number */

#ifdef __STDC__
int grpp10(char *name1, int np, char *name2)
#else
int grpp10(name1, np, name2)
     char *name1; int np; char *name2;
#endif
{
    /* Local variables */
    static int l, ln, name1_len, nd, tm;
    static char tmp[128];
    static int c0 = 0;

/* get the number of digits for this page */
    nd = 0;
    tm = np;
    while ( tm > 0 ) { nd++; tm /= 10; }

    name1_len = strlen(name1);
    name1_len = name1_len > 127 ? 127 : name1_len;
    ln = GRTRIM(name1, name1_len);
    if ( index(name1, '#') != NULL && ( ln + nd ) < 128 ) {
/*        -- if the supplied name contains a #-character, replace */
/*           it with the page number */
        GRFAO(name1, &l, tmp, &np, &c0, &c0, &c0, name1_len, 128);
    } else if (np == 1) {
/*        -- if this is the first page, use the supplied name */
        strncpy(name2, name1, 128);
        return 0;
    } else if (ln + nd + 1 < 128) {
/*        -- append an underscore and the page number to the supplied */
/*           name */
        strcpy(name1 + ln, "_#");
        GRFAO(name1, &l, tmp, &np, &c0, &c0, &c0, name1_len, 128);
    } else {
/*        -- last resort: invent a new name */
        for ( l = 0; l < 128; l++ ) {
           tmp[l] = '\0';
        }
        GRFAO("pgplot#.ppm", &l, tmp, &np, &c0, &c0, &c0, 11, 128);
    }
/* Writing concatenation */

    fprintf(stdout, "PGPLOT, Writing new PPM image as: %s\n", tmp);
    strncpy(name2, tmp, 128);
    return 0;
} /* grpp10 */

