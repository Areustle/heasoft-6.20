
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
#define WDDRIV wddriv_
#define GRCFIL grcfil_
#define GROFIL grofil_
#define GRCTOI grctoi_
#define GRGENV grgenv_
#define GRWFIL grwfil_
#define GRFAO  grfao_
#define GRTRIM grtrim_
#else
#define WDDRIV wddriv
#define GRCFIL grcfil
#define GROFIL grofil
#define GRCTOI grctoi
#define GRGENV grgenv
#define GRWFIL grwfil
#define GRFAO  grfao
#define GRTRIM grtrim
#endif

extern int GRCFIL ARGS((int *));
extern int GROFIL ARGS((char *, int));
extern int GRCTOI ARGS((char *, int *, int));
extern int GRGENV ARGS((char *, char *, int *, int, int));
extern int GRWFIL ARGS((int *, int *, char *, int));
extern int GRFAO ARGS((char *, int *, char *, int *, int *, int *, int *, int, int));
extern int GRTRIM ARGS((char *, int));

/* C Routines in this file */
static int grwd02 ARGS((int, int, int, int, int, char *));
static int grwd06 ARGS((int, int, int, int *, char *));
static int grwd07 ARGS((int, char *));
static int grwd10 ARGS((char *, int, char *));
static int wd_nint ARGS((float));

/* have to define our own nint function */

#ifdef __STDC__
static int wd_nint(float x)
#else
static int wd_nint(x)
     float x;
#endif
{
  return (int) (x >= 0.0 ? (x + 0.5) : (x - 0.5));
}


/* Global variable for number of colors used */
static int maxidx;


/* *WDDRIV -- PGPLOT XWD drivers */
/* + */
#ifdef __STDC__
int WDDRIV(int *ifunc, float *rbuf, int *nbuf, char *chr, int *lchr, int *mode, int chr_len)
#else
int WDDRIV(ifunc, rbuf, nbuf, chr, lchr, mode, chr_len)
     int *ifunc; float *rbuf; int *nbuf; char *chr; int *lchr; int *mode; int chr_len;
#endif
{
    /* Initialized data */

    static int cdeflt[48]	/* was [3][16] */ = { 0,0,0,255,255,255,255,0,
	    0,0,255,0,0,0,255,0,255,255,255,0,255,255,255,0,255,128,0,128,255,
	    0,0,255,128,0,128,255,128,0,255,255,0,128,85,85,85,170,170,170 };
    static int state = 0;

    /* Local variables */
    static int i, l, icol, ll, bx, by, ix0, iy0, ix1, iy1;
    static char msg[128];
    static int unit;
    static int npict, userh;
    static char instr[80];
    static int userw, ctable[768]	/* was [3][256] */;
    static char filenm[128];
    static char *pixmap;

/*-----------------------------------------------------------------------*/
/* PGPLOT driver for X Window Dump (XWD) files.                          */
/*                                                                       */
/* Supported device: XWD format                                          */
/*                                                                       */
/* Device type codes: /WD or /VWD                                        */
/*                                                                       */
/* Default device name: pgplot.xwd.                                      */
/*                                                                       */
/* If you have more than one image to plot (i.e. use PGPAGE) with this   */
/* device, subsequent pages will be named: pgplot2.xwd, pgplot3.xwd,     */
/* etc, disrespective of the device name you specified.                  */
/* You can however bypass this by specifying a device name including a   */
/* number sign (#), which will henceforth be replaced by the pagenumber. */
/* Example: page#.xwd will produce files page1.xwd, page2.xwd, ...,      */
/* page234.xwd, etc.                                                     */
/*                                                                       */
/* Default view surface dimensions are:                                  */
/* - WD   : 850 x 680 pixels (translates to 10.0 x  8.0 inch).           */
/* - VWD  : 680 x 850 pixels (translates to  8.0 x 10.0 inch).           */
/* with an assumed scale of 85 pixels/inch.                              */
/* Default width and height can be overridden by specifying environment  */
/* variables                                                             */
/* PGPLOT_WD_WIDTH  (default 850)                                        */
/* PGPLOT_WD_HEIGHT (default 680)                                        */
/*                                                                       */
/* Color capability:                                                     */
/* Indices 0 to 255 are supported. Each of these indices can be assigned */
/* one color. Default colors for indices 0 to 15 are implemented.        */
/*                                                                       */
/* Obtaining hardcopy: Use an XWD viewer (xwud) or converter.            */
/*=                                                                      */
/* 23-Jan-1995 - Steal GIDRIV.F code and bash appropriately [SCA].       */
/* 28-Dec-1995 - Prevent concurrent access [TJP].                        */
/* 29-Apr-1996 - Use GRCTOI to decode environment variables [TJP].       */
/*  8-Jul-2006 - Implementation in C (K .Arnaud)                         */
/*-----------------------------------------------------------------------*/

   switch (*ifunc) {

/* --- IFUNC = 1, Return device name ------------------------------------- */
    case 1:
      {
        char *dev_name;
        switch(*mode) {  /* Locate the name used to select the given mode */
        case 1: default:
	  dev_name = "WD   (XWD file, landscape orientation)";
	  break;
        case 2:
	  dev_name = "VWD  (XWD file, portrait orientation)";
	  break;
        };
        strncpy(chr, dev_name, chr_len);
        *lchr = strlen(dev_name);
        for(i = *lchr; i < chr_len; i++) chr[i] = ' ';
      };
      break;

/* --- IFUNC = 2, Return physical min and max for plot device, and range */
/*               of color indices --------------------------------------- */
/*     (Maximum size is set by XWD format to 2**16 - 1 pixels) */
    case 2:
      rbuf[0] = 0.0;
      rbuf[1] = 65535.0;
      rbuf[2] = 0.0;
      rbuf[3] = 65535.0;
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
      strncpy(chr, "pgplot.xwd", 10);
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
	fprintf(stderr, "PGPLOT, a PGPLOT GIF file is already open\n");
	rbuf[0] = 0.0;
	rbuf[1] = 0.0;
	return 0;
      }
/*     -- dimensions of plot buffer */
      userw = 0;
      userh = 0;
      GRGENV("WD_WIDTH", instr, &l, 9, 80);
      ll = 1;
      if (l > 0) {
	userw = GRCTOI(instr, &ll, l);
      }
      GRGENV("WD_HEIGHT", instr, &l, 10, 80);
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
      maxidx = 0;
/*     -- Initialize color table */
      for (i = 0; i <= 15; ++i) {
	ctable[i * 3] = cdeflt[i * 3];
	ctable[i * 3 + 1] = cdeflt[i * 3 + 1];
	ctable[i * 3 + 2] = cdeflt[i * 3 + 2];
/* L95: */
      }
      for (i = 16; i <= 255; ++i) {
	ctable[i * 3] = 128;
	ctable[i * 3 + 1] = 128;
	ctable[i * 3 + 2] = 128;
/* L96: */
      }

      strncpy(filenm, chr, 128);
      grwd10(filenm, npict, msg);
      unit = GROFIL(msg, 128);

      rbuf[0] = (float) unit;
      if (unit < 0) {
	fprintf(stderr, "PGPLOT, Cannot open output file for WD plot\n");
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
      bx = wd_nint(rbuf[0]) + 1;     
      by = wd_nint(rbuf[1]) + 1;
      pixmap = (char *) malloc(sizeof(char)*bx*by);
      if ( pixmap == NULL ) {
	fprintf(stderr, "PGPLOT, Failed to allocate plot buffer.\n");
	bx = 0;
	by = 0;
      }
/*     -- initialize to zero (background color) */
      if (pixmap != NULL) {
	for (i=0; i<bx*by; i++) pixmap[i] = (char) 0;
      }
      if (npict > 1) {
	grwd10(filenm, npict, msg);
	unit = GROFIL(msg, 128);
	if (unit < 0) {
	    fprintf(stderr, "PGPLOT, Cannot open output file for GIF plot.\n");
	}
      }
      break;

/* --- IFUNC=12, Draw line ----------------------------------------------- */

    case 12:
      {
	float d;
	int is, ix, iy, idx, idy;

        ix0 = wd_nint(rbuf[0]) + 1;
        ix1 = wd_nint(rbuf[2]) + 1;
        iy0 = by - wd_nint(rbuf[1]);
        iy1 = by - wd_nint(rbuf[3]);

        idx = ix1 - ix0;
        idy = iy1 - iy0;

        if (idx == 0 && idy == 0) {
          grwd02(ix0, iy0, icol, bx, by, pixmap);
        } else if ( abs(idy) > abs(idx) ) {
	  d = idx / (float) idy;
	  is = (idy < 0) ? -1 : 1;
	  for (iy = iy0; is < 0 ? iy >= iy1 : iy <= iy1; iy += is) {
	    ix = wd_nint(ix0 + (iy - iy0) * d);
            grwd02(ix, iy, icol, bx, by, pixmap);
	  }
        } else {
	  d = idy / (float) idx;
	  is = (idx < 0) ? -1 : 1;
	  for (ix = ix0; is < 0 ? ix >= ix1 : ix <= ix1; ix += is) {
	    iy = wd_nint(iy0 + (ix - ix0) * d);
            grwd02(ix, iy, icol, bx, by, pixmap);
	  }
        }

        break;
      }

/* --- IFUNC=13, Draw dot ------------------------------------------------ */

    case 13:
      ix0 = wd_nint(rbuf[0]) + 1;
      iy0 = by - wd_nint(rbuf[1]);
      grwd02(ix0, iy0, icol, bx, by, pixmap);
      break;

/* --- IFUNC=14, End picture --------------------------------------------- */

    case 14:
      {
        if (unit >= 0) {
	  grwd06(unit, bx, by, ctable, pixmap);
	  GRCFIL(&unit);
        }
        ++npict;
        free(pixmap);
        break;
      }

/* --- IFUNC=15, Select color index -------------------------------------- */

    case 15:
      icol = rbuf[0];
      maxidx = (maxidx > icol) ? maxidx : icol;
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
      i = rbuf[0];
      ctable[i * 3] = wd_nint(rbuf[1] * 255);
      ctable[i * 3 + 1] = wd_nint(rbuf[2] * 255);
      ctable[i * 3 + 2] = wd_nint(rbuf[3] * 255);
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
        ix0 = wd_nint(rbuf[0]) + 1;
        ix1 = wd_nint(rbuf[2]) + 1;
        iy1 = by - wd_nint(rbuf[1]);
        iy0 = by - wd_nint(rbuf[3]);
        for (iy = iy0; iy <= iy1; ++iy) {
	  for (ix = ix0; ix <= ix1; ++ix) {
            grwd02(ix, iy, icol, bx, by, pixmap);
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
        int ix, iy, n, ic;
	ix = wd_nint(rbuf[0])+1;
	iy = by - wd_nint(rbuf[1]);
	for (n = 2; n < *nbuf; ++n) {
	  ic = rbuf[n];
	  maxidx = (maxidx > ic) ? maxidx : ic;
	  if ( ic > 127 ) ic -= 256;
          grwd02(ix+n-2, iy, ic, bx, by, pixmap);
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
} /* gidriv_ */


/* *GRWD02 -- PGPLOT XWD driver, load a value into the pixmap array */
/* + */
#ifdef __STDC__
int grwd02(int ix, int iy, int icol, int bx, int by, char *pixmap)
#else
int grwd02(ix, iy, icol, bx, by, pixmap)
     int ix; int iy; int icol; int bx; int by; char *pixmap;
#endif
{


    /* Local variables */
    static int ipos, ic;
    static char val[1];


/* Load the value ICOL into PIXMAP(IX,IY) */

/* Arguments: */
/*  IX, IY          (input): Coordinates */
/*  ICOL            (input): Color index */
/*  BX, BY          (input): Size of PIXMAP */
/*  PIXMAP   (input/output): The image data buffer. */
/* ----------------------------------------------------------------------- */

    ic = (icol > 127) ? icol - 256 : icol;
    *(unsigned char*)val = (char) ic;

    ipos = ix-1 + (iy-1)*bx;
    if ( ipos < 0 || ipos >= bx*by ) {
      fprintf(stderr, "PGPLOT, attempt to write outside PIXMAP array bounds, ix = %d, iy = %d\n", ix, iy);
    } else {
      if ( pixmap != NULL ) *(unsigned char *)&pixmap[ipos] = *(unsigned char*)val;
    }

    return 0;

} /* grwd02 */

#ifdef __STDC__
int grwd06(int unit, int bx, int by, int *ctable, char *pixmap)
#else
int grwd06(unit, bx, by, ctable, pixmap)
     int unit; int bx; int by; int *ctable; char *pixmap;
#endif
{

    /* Local variables */
    static int i, j, ier, ic;
    static int c107 = 107;
    static int c12 = 12;
    static char head[107], color[12];

/* Write XWD image to UNIT. */

/* Arguments: */
/* UNIT   (input): Output unit */
/* BX,BY  (input): `Screen' size */
/* CTABLE  (input): Color map */
/* PIXMAP (input): Image data */
/* -- */
/* 23-Jan-1995 - New routine [SCA] */
/* ----------------------------------------------------------------------- */

/* Initialize the color and head arrays */

    for (i=0; i<12; i++) strncpy(color+i, "\0", 1);
    strncpy(color+10, "\a", 1);

    for (i=0; i<107; i++) strncpy(head+i, "\0", 1);

    strncpy(head+3, "k", 1);
    strncpy(head+7, "\a", 1);
    strncpy(head+11, "\2", 1);
    strncpy(head+15, "\b", 1);
    strncpy(head+31, "\1", 1);
    strncpy(head+35, "\b", 1);
    strncpy(head+39, "\1", 1);
    strncpy(head+43, "\b", 1);
    strncpy(head+47, "\b", 1);
    strncpy(head+55, "\3", 1);
    strncpy(head+71, "\b", 1);
    strncpy(head+74, "\1", 1);
    strncpy(head+100, "PGPLOT", 6);

/*     DATA COLOR /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0/ */
/*     DATA  HEAD / 0,   0,   0, 107,       0,   0,   0,   7, */
/*    1             0,   0,   0,   2,       0,   0,   0,   8, */
/*    2             0,   0,   0,   0,       0,   0,   0,   0, */
/*    3             0,   0,   0,   0,       0,   0,   0,   1, */
/*    4             0,   0,   0,   8,       0,   0,   0,   1, */
/*    5             0,   0,   0,   8,       0,   0,   0,   8, */
/*    6             0,   0,   0,   0,       0,   0,   0,   3, */
/*    7             0,   0,   0,   0,       0,   0,   0,   0, */
/*    8             0,   0,   0,   0,       0,   0,   0,   8, */
/*    9             0,   0,   1,   0,       0,   0,   0,   0, */
/*    A             0,   0,   0,   0,       0,   0,   0,   0, */
/*    B             0,   0,   0,   0,       0,   0,   0,   0, */
/*    C             0,   0,   0,   0,      80,  71,  80,  76, */
/*    D            79,  84,   0/ */

/* Write image width into Header. */

    grwd07(bx, head + 18);
    grwd07(bx, head + 50);
    grwd07(bx, head + 82);

/* Write image height into Header. */

    grwd07(by, head + 22);
    grwd07(by, head + 86);

/* Write number of colors into Header. */

    grwd07(maxidx+1, head + 78);

/* Write Header. */

    ier = GRWFIL(&unit, &c107, head, 1);
    if (ier != 107) fprintf(stderr, "Error writing XWD header");

/* Write out the color table. */

    for (j = 0; j <= maxidx; ++j) {
      grwd07(j, color + 2);
      for (i = 1; i <= 3; ++i) {
	ic = ctable[i-1+j*3];
	if ( ic > 127 ) ic -= 256;
	*(unsigned char *)&color[(i*2) + 2] = (char) ic;
	*(unsigned char *)&color[(i*2) + 3] = (char) ic;
      }
      ier = GRWFIL(&unit, &c12, color, 1);
    }

/* Write out the bitmap. */

    i = bx * by;
    ier = GRWFIL(&unit, &i, pixmap, 1);

    return 0;

} /* grwd06 */

/* *GRWD07 -- Store unsigned 16-bit integer in host independent format */
/* + */
#ifdef __STDC__
int grwd07(int i, char *arr)
#else
int grwd07(i, arr)
     int i; char *arr;
#endif
{
    static int tmp;

    /* Function Body */
    tmp = i / 256 % 256;
    if (tmp > 127) {
	*(unsigned char *)&arr[0] = (char) (tmp - 256);
    } else {
	*(unsigned char *)&arr[0] = (char) tmp;
    }
    tmp = i % 256;
    if (tmp > 127) {
	*(unsigned char *)&arr[1] = (char) (tmp - 256);
    } else {
	*(unsigned char *)&arr[1] = (char) tmp;
    }
    return 0;
} /* grwd07 */

/* *GRWD10 -- Replace # in filename by picture number */

#ifdef __STDC__
int grwd10(char *name1, int np, char *name2)
#else
int grwd10(name1, np, name2)
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
        GRFAO("pgplot#.xwd", &l, tmp, &np, &c0, &c0, &c0, 11, 128);
    }
/* Writing concatenation */

    fprintf(stdout, "PGPLOT, Writing new XWD image as: %s\n", tmp);
    strncpy(name2, tmp, 128);
    return 0;
} /* grwd10 */

