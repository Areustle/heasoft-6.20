
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
#define GIDRIV gidriv_
#define GRCFIL grcfil_
#define GROFIL grofil_
#define GRCTOI grctoi_
#define GRGENV grgenv_
#define GRWFCH grwfch_
#define GRWFIL grwfil_
#define GRFAO  grfao_
#define GRTRIM grtrim_
#else
#define GIDRIV gidriv
#define GRCFIL grcfil
#define GROFIL grofil
#define GRCTOI grctoi
#define GRGENV grgenv
#define GRWFCH grwfch
#define GRWFIL grwfil
#define GRFAO  grfao
#define GRTRIM grtrim
#endif

extern int GRCFIL ARGS((int *));
extern int GROFIL ARGS((char *, int));
extern int GRCTOI ARGS((char *, int *, int));
extern int GRGENV ARGS((char *, char *, int *, int, int));
extern int GRWFCH ARGS((int *, char *, int));
extern int GRWFIL ARGS((int *, int *, char *, int));
extern int GRFAO ARGS((char *, int *, char *, int *, int *, int *, int *, int, int));
extern int GRTRIM ARGS((char *, int));

/* C Routines in this file */
static int grgi02 ARGS((int, int, int, int, int, unsigned char *));
static int grgi06 ARGS((int, int, int, int *, unsigned char *));
static int grgi07 ARGS((int, int));
static int grgi08 ARGS((int, int));
static int grgi10 ARGS((char *, int, char *));
static int gi_nint ARGS((float));

/* have to define our own nint function */

#ifdef __STDC__
static int gi_nint(float x)
#else
static int gi_nint(x)
     float x;
#endif
{
  return (int) (x >= 0.0 ? (x + 0.5) : (x - 0.5));
}


/* Global variables for GIF output block */

static int bmax, bmult, brest, bout;
char   blkout[255];

/* Global variable for number of colors used */
static int maxidx;


/* *GIDRIV -- PGPLOT GIF drivers */
/* + */
#ifdef __STDC__
int GIDRIV(int *ifunc, float *rbuf, int *nbuf, char *chr, int *lchr, int *mode, int chr_len)
#else
int GIDRIV(ifunc, rbuf, nbuf, chr, lchr, mode, chr_len)
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
    static unsigned char *pixmap;


/* PGPLOT driver for Graphics Interchange Format (GIF) files. */

/* *********************************************************************** */
/*                           CAUTION                                    * */
/*                                                                      * */
/* The GIF specification incorporates the Lempel-Zev-Welch (LZW)        * */
/* compression technology which is the subject of a patent awarded to   * */
/* Unisys. Use of this technology, and in particular creation of GIF    * */
/* format files using this PGPLOT device driver, may require a license  * */
/* from Unisys.                                                         * */
/* *********************************************************************** */

/* Supported device: GIF87a file format */

/* Device type codes: /GIF or /VGIF */

/* Default device name: pgplot.gif. */

/* If you have more than one image to plot (i.e. use PGPAGE) with this */
/* device, subsequent pages will be named: pgplot2.gif, pgplot3.gif, */
/* etc, disrespective of the device name you specified. */
/* You can however bypass this by specifying a device name including a */
/* number sign (#), which will henceforth be replaced by the pagenumber. */
/* Example: page#.gif will produce files page1.gif, page2.gif, ..., */
/* page234.gif, etc. */

/* Default view surface dimensions are: */
/* - GIF  : 850 x 680 pixels (translates to 10.0 x  8.0 inch). */
/* - VGIF : 680 x 850 pixels (translates to  8.0 x 10.0 inch). */
/* with an assumed scale of 85 pixels/inch. */
/* Default width and height can be overridden by specifying environment */
/* variables */
/* PGPLOT_GIF_WIDTH  (default 850) */
/* PGPLOT_GIF_HEIGHT (default 680) */

/* Color capability: */
/* Indices 0 to 255 are supported. Each of these indices can be assigned */
/* one color. Default colors for indices 0 to 15 are implemented. */

/* Obtaining hardcopy: Use a GIF viewer or converter. */
/* = */
/*  1-Aug-1994 - Created by Remko Scharroo */
/*  9-Aug-1994 - New scheme for line plotting */
/* 16-Aug-1994 - Provide multi-image plotting. */
/*  8-Sep-1994 - Add opcode 29 [TJP]. */
/*  5-Nov-1994 - Adjust size of bitmap if necessary [TJP]. */
/* 18-Jan-1995 - Attempt to prevent integer overflow on systems where */
/*               BYTE is signed [TJP]. */
/* 28-Dec-1995 - prevent concurrent access [TJP]. */
/* 29-Apr-1996 - use GRCTOI to decode environment variables [TJP]. */
/*  7-Jul-2006 - implementation in C (K. Arnaud)                           */
/* ----------------------------------------------------------------------- */

   switch (*ifunc) {

/* --- IFUNC = 1, Return device name ------------------------------------- */
    case 1:
      {
        char *dev_name;
        switch(*mode) {  /* Locate the name used to select the given mode */
        case 1: default:
	  dev_name = "GIF   (GIF file, landscape orientation)";
	  break;
        case 2:
	  dev_name = "VGIF  (GIF file, portrait orientation)";
	  break;
        };
        strncpy(chr, dev_name, chr_len);
        *lchr = strlen(dev_name);
        for(i = *lchr; i < chr_len; i++) chr[i] = ' ';
      };
      break;

/* --- IFUNC = 2, Return physical min and max for plot device, and range */
/*               of color indices --------------------------------------- */
/*     (Maximum size is set by GIF format to 2**16 pixels) */

    case 2:
      rbuf[0] = 0.0;
      rbuf[1] = 65536.0;
      rbuf[2] = 0.0;
      rbuf[3] = 65536.0;
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
      strncpy(chr, "pgplot.gif", 10);
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
      GRGENV("GIF_WIDTH", instr, &l, 9, 80);
      ll = 1;
      if (l > 0) {
	userw = GRCTOI(instr, &ll, l);
      }
      GRGENV("GIF_HEIGHT", instr, &l, 10, 80);
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
      grgi10(filenm, npict, msg);
      unit = GROFIL(msg, 128);

      rbuf[0] = (float) unit;
      if (unit < 0) {
	fprintf(stderr, "PGPLOT, Cannot open output file for GIF plot\n");
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
      bx = gi_nint(rbuf[0]) + 1;     
      by = gi_nint(rbuf[1]) + 1;
      pixmap = (unsigned char *) malloc(sizeof(unsigned char)*bx*by);
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
	grgi10(filenm, npict, msg);
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

        ix0 = gi_nint(rbuf[0]) + 1;
        ix1 = gi_nint(rbuf[2]) + 1;
        iy0 = by - gi_nint(rbuf[1]);
        iy1 = by - gi_nint(rbuf[3]);

        idx = ix1 - ix0;
        idy = iy1 - iy0;

        if (idx == 0 && idy == 0) {
          grgi02(ix0, iy0, icol, bx, by, pixmap);
        } else if ( abs(idy) > abs(idx) ) {
	  d = idx / (float) idy;
	  is = (idy < 0) ? -1 : 1;
	  for (iy = iy0; is < 0 ? iy >= iy1 : iy <= iy1; iy += is) {
	    ix = gi_nint(ix0 + (iy - iy0) * d);
            grgi02(ix, iy, icol, bx, by, pixmap);
	  }
        } else {
	  d = idy / (float) idx;
	  is = (idx < 0) ? -1 : 1;
	  for (ix = ix0; is < 0 ? ix >= ix1 : ix <= ix1; ix += is) {
	    iy = gi_nint(iy0 + (ix - ix0) * d);
            grgi02(ix, iy, icol, bx, by, pixmap);
	  }
        }

        break;
      }

/* --- IFUNC=13, Draw dot ------------------------------------------------ */

    case 13:
      ix0 = gi_nint(rbuf[0]) + 1;
      iy0 = by - gi_nint(rbuf[1]);
      grgi02(ix0, iy0, icol, bx, by, pixmap);
      break;

/* --- IFUNC=14, End picture --------------------------------------------- */

    case 14:
      {
        if (unit >= 0) {
	  grgi06(unit, bx, by, ctable, pixmap);
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
      ctable[i * 3] = gi_nint(rbuf[1] * 255);
      ctable[i * 3 + 1] = gi_nint(rbuf[2] * 255);
      ctable[i * 3 + 2] = gi_nint(rbuf[3] * 255);
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
        ix0 = gi_nint(rbuf[0]) + 1;
        ix1 = gi_nint(rbuf[2]) + 1;
        iy1 = by - gi_nint(rbuf[1]);
        iy0 = by - gi_nint(rbuf[3]);
        for (iy = iy0; iy <= iy1; ++iy) {
	  for (ix = ix0; ix <= ix1; ++ix) {
            grgi02(ix, iy, icol, bx, by, pixmap);
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
	ix = gi_nint(rbuf[0])+1;
	iy = by - gi_nint(rbuf[1]);
	for (n = 2; n < *nbuf; ++n) {
	  ic = rbuf[n];
	  maxidx = (maxidx > ic) ? maxidx : ic;
	  if ( ic > 127 ) ic -= 256;
          grgi02(ix+n-2, iy, ic, bx, by, pixmap);
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


/* *GRGI02 -- PGPLOT GIF driver, load a value into the pixmap array */
/* + */
#ifdef __STDC__
int grgi02(int ix, int iy, int icol, int bx, int by, unsigned char *pixmap)
#else
int grgi02(ix, iy, icol, bx, by, pixmap)
     int ix; int iy; int icol; int bx; int by; unsigned char *pixmap;
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

} /* grgi02 */

#ifdef __STDC__
int grgi06(int unit, int bx, int by, int *ctable, unsigned char *pixmap)
#else
int grgi06(unit, bx, by, ctable, pixmap)
     int unit; int bx; int by; int *ctable; unsigned char *pixmap;
#endif
{

    /* Local variables */
    static int i, i1, j, k, in, eoi, pre, ext;
    static int oldpre;
    static int pixel, total;
    static char gif1[7], gif2[7], gif3[3], gif4[10];
    static char ch[1];
    static int bits, clear, table;
    static short *code;
    static int c1 = 1;


/* Write GIF image to UNIT. */

/* Arguments: */
/* UNIT   (input): Output unit */
/* BX,BY  (input): `Screen' size */
/* CTABLE  (input): Color map */
/* PIXMAP (input): Image data */
/* -- */
/* 16-Nov-94: fixed bug (BYTE is signed) */
/* ----------------------------------------------------------------------- */

    /* Function Body */
    bits = 1;
L10:
    if (maxidx < (int)pow((double)2.0, (double)bits)) {
	goto L20;
    }
    ++bits;
    goto L10;
L20:

/* Write Header. */

    strcpy(gif1, "GIF87a");
    i = GRWFCH(&unit, gif1, 6);
    if (i != 6) {
	fprintf(stderr, "PGPLOT, Error writing GIF header\n");
    }

/* Write Logical Screen Descriptor (screen width, screen height, */
/* color data, background color index [0], pixel aspect ratio [0]). */

    *(unsigned char *)&gif2[0] = (char) (bx % 256);
    *(unsigned char *)&gif2[1] = (char) (bx / 256 % 256);
    *(unsigned char *)&gif2[2] = (char) (by % 256);
    *(unsigned char *)&gif2[3] = (char) (by / 256 % 256);
    *(unsigned char *)&gif2[4] = (char) ((bits - 1) * 17 + 128);
    *(unsigned char *)&gif2[5] = '\0';
    *(unsigned char *)&gif2[6] = '\0';
    i = GRWFCH(&unit, gif2, 7);

/* Write Global Color Table. */

    i1 = (int)pow((double)2.0, (double)bits) - 1;
    for (j = 0; j <= i1; ++j) {
	*(unsigned char *)&gif3[0] = (char) ctable[j * 3];
	*(unsigned char *)&gif3[1] = (char) ctable[j * 3 + 1];
	*(unsigned char *)&gif3[2] = (char) ctable[j * 3 + 2];
	i = GRWFCH(&unit, gif3, 3);
/* L30: */
    }

    pixel = (bits > 2) ? bits : 2;

/* Write Image Descriptor. */

    *(unsigned char *)&gif4[0] = ',';
    *(unsigned char *)&gif4[1] = '\0';
    *(unsigned char *)&gif4[2] = '\0';
    *(unsigned char *)&gif4[3] = '\0';
    *(unsigned char *)&gif4[4] = '\0';
    *(unsigned char *)&gif4[5] = gif2[0];
    *(unsigned char *)&gif4[6] = gif2[1];
    *(unsigned char *)&gif4[7] = gif2[2];
    *(unsigned char *)&gif4[8] = gif2[3];
    *(unsigned char *)&gif4[9] = '\0';
    i = GRWFCH(&unit, gif4, 10);

/* Write Table Based Image Data, in sub-blocks of up to 255 bytes. */

    *(unsigned char *)&ch[0] = pixel;
    i = GRWFCH(&unit, ch, 1);

/* LZW-compression; initialize counters; define clear code and EOI code. */
/* Start packing variable-size codes into 8-bit bytes. */
/* Push a clear code first. */
/* `Read' first character. */

    code = (short *) malloc(sizeof(short)*4098*256);
    if ( code == NULL ) fprintf(stderr, "PGPLOT, Failed to allocate code array.\n");
    for (k = 0; k < 4098*256; ++k) code[k] = 0;

    clear = pow(2, pixel);
    eoi = clear + 1;
    brest = 0;
    bout = 0;
    bmult = 1;
    bmax = clear << 1;
    grgi07(unit, clear);
    in = 0;
    total = bx * by;
    pre = *(unsigned char *)&pixmap[in];
    if (pre < 0) {
	pre += 256;
    }

/* Start new data stream at line 310: */
/* 2**n-1  (n+1)-bit codes */
/* 2*2**n  (n+2)-bit codes */
/* 4*2**n  (n+3)-bit codes */
/*    .         .      . */
/*   1024     11-bit codes */
/*   2048     12-bit codes (incl. one clear code) */

L310:
    table = eoi;
    bmax = clear << 1;

/* `Read' next character; check if combination prefix&extension occurred earlier */

L320:
    if (in > total) {
	goto L350;
    }
    ++in;
    ext = *(unsigned char *)&pixmap[in];
    if (ext < 0) {
	ext += 256;
    }
    oldpre = pre;
    pre = code[pre + ext * 4098];
    if (pre > 0) {
	goto L320;
    }

/* If no earlier occurrence add combination to table */

    ++table;
    grgi07(unit, oldpre);
    code[oldpre + ext * 4098] = (short) table;
    pre = ext;
    if (table == bmax) {
	bmax <<= 1;
    }
    if (table < 4095) {
	goto L320;
    }
    grgi07(unit, clear);
    for (k = 0; k < 4098*256; ++k) code[k] = 0;
    goto L310;

/* Last character */

L350:
    grgi07(unit, pre);
    grgi07(unit, eoi);

    if (bmult > 1) grgi08(unit, brest);

    if (bout > 0) {
	if (bout > 127) {
	    *(unsigned char *)&blkout[0] = (char) (bout - 256);
	} else {
	    *(unsigned char *)&blkout[0] = (char) bout;
	}
	i1 = bout + 1;
	i = GRWFIL(&unit, &i1, blkout, 1);
	bout = 0;
    }
    *(unsigned char *)&blkout[0] = '\0';
    i = GRWFIL(&unit, &c1, blkout, 1);

/* Write GIF Trailer. */

    i = GRWFCH(&unit, ";", 1);

    free(code);

    return 0;
} /* grgi06 */

/* *GRGI07 -- Compile GIF output code */

#ifdef __STDC__
int grgi07(int unit, int incode)
#else
int grgi07(unit, incode)
     int unit; int incode;
#endif
{

    brest += bmult * incode;
    bmult *= bmax;

L10:
    if (bmult < 256) {
	return 0;
    }
    grgi08(unit, brest);
    brest /= 256;
    bmult /= 256;
    goto L10;

} /* grgi07 */

/* *GRGI08 -- Compile and write GIF output buffer */

#ifdef __STDC__
int grgi08(int unit, int incode)
#else
int grgi08(unit, incode)
     int unit; int incode;
#endif
{
    static int i, j;
    static int c255 = 255;

    ++bout;
    j = incode % 256;
    if (j > 127) j += -256;

    *(unsigned char *)&blkout[bout] = (char) j;
    if (bout < 254) return 0;

    *(unsigned char *)&blkout[0] = 254;
    i = GRWFIL(&unit, &c255, blkout, 1);
    bout = 0;
    return 0;
} /* grgi08 */


/* *GRGI10 -- Replace # in filename by picture number */

#ifdef __STDC__
int grgi10(char *name1, int np, char *name2)
#else
int grgi10(name1, np, name2)
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
        GRFAO("pgplot#.gif", &l, tmp, &np, &c0, &c0, &c0, 11, 128);
    }
/* Writing concatenation */

    fprintf(stdout, "PGPLOT, Writing new GIF image as: %s\n", tmp);
    strncpy(name2, tmp, 128);
    return 0;
} /* grgi10 */

