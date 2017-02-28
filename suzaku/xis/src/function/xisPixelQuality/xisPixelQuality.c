/*************************************
  xisPixelQuality
     Calculate Pixel Quality for each event

  ver 2.0    2007.05.05 Y.ISHISAKI
	xisPixelQualityInit(), xisPixelQuality() from XISputPixelQuality.c
	add 2PIX_FROM_WINBOUNDARY, 1PIX_FROM_WINBOUNDARY, WINBOUNDARY
	check active CCD size, and set BIT_OUTSIDE_AREADISCRI if outside
	check SCI rows in xisPixelQuality()

  ver 2.1    2007.05.14 Y.ISHISAKI
	change bit order for BIT_SCI_2nd_TRAILING_ROW

  ver 2.2    2007.05.27 Y.ISHISAKI
  	change safe_bit_level = SCI_2nd_TRAILING_ROW in xisPixqStatInit()
	accept badcol_filename, calmask_filename = "none" in xisPixqStatInit()
	add decimal values for comment in xisPixqStatInit()
	add xisPixqAddBadcolCache(), xisPixqReadHotpixFiles(), xis_raw2act()

  ver 2.3    2007.05.28 Y.ISHISAKI
	add xisPixqReadBadcolFile(), xisPixqReadCalmaskFile()
	check INSTRUME keyword in read_hotpixfile()
*************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "cli.h"
#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"

static char pname[] = "xisPixelQuality-2.3";

#define DEFAULT_START	(0.0)
#define DEFAULT_STOP	(1.0e20)

#define TESTBIT(status, bit)	(0 != (status & (1 << bit)))
#define SETBIT(status, bit)	status |= (1 << bit)

int
xisPixqReadBadcolFile(PIXQ_INFO *p, char *badcol_filename)
{
  int i, a, hdutype, typecode;
  char *k, file_instrume[FLEN_VALUE];

  long irow, num_badcol, repeat, width;
  int col_start, col_stop, col_actx, col_acty1, col_acty2, col_bccode;
  unsigned char bccode_byte[4];
  BADCOL_INFO *bp;

  fitsfile *fp = NULL;
  int istat = 0;

  if ( NULL == badcol_filename ||
       '\0' == badcol_filename[0] ||
       0 == CLstricmp("none", badcol_filename) ) {
    return 0;
  }

  fits_open_file(&fp, badcol_filename, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, badcol_filename, istat);
    goto quit;
  }

  /* old badcolum CALDB has different EXTNAME, e.g., XIS1_BADCOLUMNS,
     so just move to 1st extension, here */
  fits_movabs_hdu(fp, 2, &hdutype, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_movabs_hdu(hdunum=2) failed (%d)\n", pname, istat);
    goto quit;
  }

  fits_get_num_rows(fp, &num_badcol, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    goto quit;
  }

  /* check sensor id */
  fits_read_key_str(fp, k="INSTRUME", file_instrume, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if ( 0 != strcmp(p->instrume, file_instrume) ) {
    anl_msg_error("\
%s: INSTRUME keyword (%s) is inconsistent with CALDB (%s)\n",
	pname, p->instrume, file_instrume);
    istat = -1;
    goto quit;
  }

  p->badcol_list = malloc(num_badcol * sizeof(*p->badcol_list));
  if ( NULL == p->badcol_list ) {
    anl_msg_error("\
%s: malloc() failed for badcol_list (num_badcol=%ld)\n", pname, num_badcol);
    istat = -1;
    goto quit;
  }

  if (
fits_get_colnum(fp, CASESEN, k="START", &col_start, &istat) ||
fits_get_colnum(fp, CASESEN, k="STOP", &col_stop, &istat) ||
       0 ) {
    anl_msg_warning("\
%s: no START or STOP columns (%d), ignore it\n", pname, istat);
    istat = 0;
    col_start = col_stop = -1;
  }

  if (
fits_get_colnum(fp, CASESEN, k="ACTX", &col_actx, &istat) ||
fits_get_colnum(fp, CASESEN, k="ACTY1", &col_acty1, &istat) ||
fits_get_colnum(fp, CASESEN, k="ACTY2", &col_acty2, &istat) ||
fits_get_colnum(fp, CASESEN, k="BCCODE", &col_bccode, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_get_coltype(fp, col_bccode, &typecode, &repeat, &width, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_coltype('BCCODE') failed (%d)\n", pname, istat);
    goto quit;
  }
  if ( TBIT != typecode && TINT32BIT != typecode ) {
    anl_msg_error("\
%s: invalid typecode=%d for BCCODE\n", pname, typecode);
    istat = -1;
    goto quit;
  }

  bp = p->badcol_list;
  for (irow = 1; irow <= num_badcol; irow++, bp++) {
    bp->start = DEFAULT_START;
    bp->stop = DEFAULT_STOP;
    if ( 0 < col_start && 0 < col_stop ) {
      if (
fits_read_col_dbl(fp, i=col_start, irow, 1, 1, 0, &bp->start, &a, &istat) ||
fits_read_col_dbl(fp, i=col_stop,  irow, 1, 1, 0, &bp->stop,  &a, &istat) ||
	   0 ) {
	anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
	goto quit;
      }
    }
    if (
fits_read_col_sht(fp, i=col_actx,  irow, 1, 1, 0, &bp->actx,  &a, &istat) ||
fits_read_col_sht(fp, i=col_acty1, irow, 1, 1, 0, &bp->acty1, &a, &istat) ||
fits_read_col_sht(fp, i=col_acty2, irow, 1, 1, 0, &bp->acty2, &a, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      goto quit;
    }
    if ( TBIT == typecode ) {
fits_read_col_byt(fp, i=col_bccode, irow, 1, 4, 0, bccode_byte, &a, &istat);
      bp->bccode = (((bccode_byte[0] << 8) | bccode_byte[1]) << 16) |
      		    ((bccode_byte[2] << 8) | bccode_byte[3]);
    } else {
fits_read_col_uint(fp, i=col_bccode, irow, 1, 1, 0, &bp->bccode, &a, &istat);
    }
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      goto quit;
    }
  }

  fits_close_file(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  fp = NULL;
  p->num_badcol = num_badcol;

  return 0;

 quit:
  if ( NULL != p->badcol_list ) {
    free(p->badcol_list);
  }
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

int
xisPixqReadCalmaskFile(PIXQ_INFO *p, char *calmask_filename)
{
  char *k, file_instrume[FLEN_VALUE];
  int bitpix, naxis, a, ms;
  long tpix, fpix[2], naxes[2];

  fitsfile *fp = NULL;
  int istat = 0;

  if ( NULL == calmask_filename ||
       '\0' == calmask_filename[0] ||
       0 == CLstricmp("none", calmask_filename) ) {
    return 0;
  }

  p->calmask = malloc(ms = sizeof(*p->calmask));
  if ( NULL == p->calmask ) {
    anl_msg_error("\
%s: malloc() failed for calmask (size=%d)\n", pname, ms);
    istat = -1;
    goto quit;
  }

  fits_open_file(&fp, calmask_filename, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, calmask_filename, istat);
    goto quit;
  }

  /* check sensor id */
  fits_read_key_str(fp, k="INSTRUME", file_instrume, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if ( 0 != strcmp(p->instrume, file_instrume) ) {
    anl_msg_error("\
%s: INSTRUME keyword (%s) is inconsistent with CALDB (%s).\n",
	pname, p->instrume, file_instrume);
    goto quit;
  }

  naxes[0] = naxes[1] = 0;
  fits_get_img_param(fp, 2, &bitpix, &naxis, naxes, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_get_img_param() failed (%d)\n", pname, istat);
    goto quit;
  }

  if ( naxis != 2 ||
       naxes[0] != XISactiveFrameHsize ||
       naxes[1] != XISactiveFrameVsize ) {
    anl_msg_error("\
%s: calmask file is strange (naxis=%d, naxes=%ldx%ld)\n",
	pname, naxis, naxes[0], naxes[1]);
    goto quit;
  }

  fpix[0] = fpix[1] = 1;
  tpix = naxes[0] * naxes[1];

  fits_read_pix(fp, TBYTE, fpix, tpix, NULL, p->calmask->image[0], &a, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_pix_sht() failed (%d)\n", pname, istat);
    goto quit;
  }

  fits_close_file(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  return 0;

 quit:
  if ( NULL != p->calmask ) {
    free(p->calmask);
  }
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

int
xisPixelQualityInit(char *instrume, PIXQ_INFO *p)
{
  int istat;

  strncpy(p->instrume, instrume, sizeof(p->instrume));
  p->instrume[sizeof(p->instrume)-1] = '\0';
  p->num_badcol = 0;
  p->num_hotpixfile = 0;
  p->badcol_list = NULL;
  p->hotpixfile_list = NULL;
  p->badcol_cache = NULL;
  p->calmask = NULL;
  p->bscale = 1.0;
  p->actexpo = NULL;

  /***********************************************/
  /* Set Bad Column Information from a fits file */
  /***********************************************/

  istat = xisPixqReadBadcolFile(p, p->badcol_filename);
  if ( istat ) {
    goto quit;
  }

  /*******************************************/
  /* Calibration Source Mask from CALDB file */
  /*******************************************/

  istat = xisPixqReadCalmaskFile(p, p->calmask_filename);
  if ( istat ) {
    goto quit;
  }

  /********************************************/
  /* HOTPIXELS in DarkInit & DarkUpdate Files */
  /********************************************/

  istat = xisPixqReadHotpixFiles(p, p->hotpix_filenames);
  if ( istat ) {
    goto quit;
  }

  return 0;

 quit:
  return istat;
}

static int
xis_actx2rawx(int actx)
{
  static int coe_x_a[4] = {
    0, 511, 512, 1023
  };
  static int coe_x_b[4] = {
    1, -1, 1, -1
  };

  int seg, rawx;

  actx &= 1023;
  seg = actx / 256;
  rawx = coe_x_b[seg] * (actx - coe_x_a[seg]);

  return rawx;
}

static int
xis_raw2act(int iseg, int rawx, int rawy, int *actx, int *acty)
{
  if ( rawx < -2 || XISactiveSegmentHsize + 2 <= rawx ) {
    anl_msg_error("\
%s: illegal RAWX value -- %d\n", pname, rawx);
    return -1;
  }

  if ( rawy < 0 || XISactiveFrameVsize <= rawy ) {
    anl_msg_error("\
%s: illegal RAWY value -- %d\n", pname, rawy);
    return -1;
  }

  switch (iseg) {
  case 0:
    *actx = rawx;
    break;
  case 1:
    *actx = 2*XISactiveSegmentHsize - rawx - 1;
    break;
  case 2:
    *actx = 2*XISactiveSegmentHsize + rawx;
    break;
  case 3:
    *actx = 4*XISactiveSegmentHsize - rawx - 1;
    break;
  default:
    anl_msg_error("\
%s: illegal segment ID -- %d\n", pname, iseg);
    return -1;
  }

  if ( *actx < 0 || XISactiveFrameHsize <= *actx ) {
    anl_msg_error("\
%s: illegal ACTX value after conversion -- %d\n", pname, *actx);
    return -1;
  }

  *acty = rawy;

  return 0;
}

unsigned
xisPixelQuality(PIXQ_INFO *p, double aetime, int actx, int acty, int rawy)
{
  static unsigned char insidecalmask = 0;
  unsigned int pixel_quality;
  BADCOL_INFO *bp;

  int nrow, ap4n, ap256n;
  int period, rawx;
  short *sciy, *ap4y, *ap256y;

  int i, bactx, bacty1, bacty2, actx_mod_256;
  unsigned int bccode;
  int num_badcol = p->num_badcol;

  int win_st = p->sci.win_st;
  int win_siz = p->sci.win_siz;
  int win_ed = win_st + win_siz;

  pixel_quality = 0;

/* check active CCD size */

  if ( win_st < 0 || win_siz <= 0 || XISactiveFrameVsize < win_siz ) {
    anl_msg_error("\
%s: strange window parameter: WIN_ST=%d, WIN_SIZ=%d\n",
	pname, win_st, win_siz);
    win_st = 0;
    win_siz = XISactiveFrameVsize;
  }

  if ( actx < 0 || XISactiveFrameHsize <= actx ||
       acty < win_st || win_ed <= acty ) {
    SETBIT(pixel_quality, BIT_OUTSIDE_AREADISCRI);
    return pixel_quality;
  }

/* check boundary */

  actx_mod_256 = actx % XISactiveSegmentHsize;
  if ( 0 == actx_mod_256 || XISactiveSegmentHsize - 1 == actx_mod_256 ||
       0 == acty || XISactiveSegmentVsize - 1 == acty ) {
    /* segment boundary pixels */
    SETBIT(pixel_quality, BIT_SEGBOUNDARY);
  }
  if ( 1 == actx_mod_256 || XISactiveSegmentHsize - 2 == actx_mod_256 ||
       1 == acty || XISactiveSegmentVsize - 2 == acty ) {
    /* 1 pix from segment boundary pixels */
    SETBIT(pixel_quality, BIT_1PIX_FROM_SEGBOUNDARY);
  }
  if ( 2 == actx_mod_256 || XISactiveSegmentHsize - 3 == actx_mod_256 ||
       2 == acty || XISactiveSegmentVsize - 3 == acty ) {
    /* 2 pix from segment boundary pixels */
    SETBIT(pixel_quality, BIT_2PIX_FROM_SEGBOUNDARY);
  }

  if ( 0 == actx || XISactiveFrameHsize - 1 == actx ||
       win_st == acty || win_ed - 1 == acty ) {
    /* frame/window boundary pixels */
    SETBIT(pixel_quality, BIT_WINBOUNDARY);
  }
  if ( 1 == actx || XISactiveFrameHsize - 2 == actx ||
       1 + win_st == acty || win_ed - 2 == acty ) {
    /* 1 pix from frame/window boundary pixels */
    SETBIT(pixel_quality, BIT_1PIX_FROM_WINBOUNDARY);
  }
  if ( 2 == actx || XISactiveFrameHsize - 3 == actx ||
       2 + win_st == acty || win_ed - 3 == acty ) {
    /* 2 pix from frame/window boundary pixels */
    SETBIT(pixel_quality, BIT_2PIX_FROM_WINBOUNDARY);
  }

/* bad column information */

  bp = p->badcol_list;
  if ( NULL == bp ) {
    goto skip_badcol;
  }

  if ( NULL != p->badcol_cache ) {
    pixel_quality |= p->badcol_cache[acty*XISactiveFrameHsize + actx];
  }

  num_badcol = p->num_badcol;
  for (i = 0; i < num_badcol; i++, bp++) {

    if ( aetime < bp->start || bp->stop < aetime ) {
      continue;
    }

    bccode = bp->bccode;
    bactx  = bp->actx;
    bacty1 = bp->acty1;
    bacty2 = bp->acty2;

    if ( actx == bactx ) {
      if ( acty < bacty1 ) {
	/* preceding pixels */
	SETBIT(pixel_quality, BIT_PRECEDING);
      } else if ( acty <= bacty2 ) {
	/* bad CTE columns, hot  pixels, flickering pixels */
	pixel_quality |= bccode;
      }
      if ( acty == bacty1 - 1 || acty == bacty2 + 1 ) {
	goto next_to_badpix;
      } else if ( acty == bacty1 - 2 || acty == bacty2 + 2 ) {
	goto next_next_to_badpix;
      }
    } else if ( actx == bactx - 1 || actx == bactx + 1 ) {
      if ( acty < bacty1 + 1 ) {
	/* 1 pixel apart from the preceding pixels  */
	SETBIT(pixel_quality, BIT_1PIX_FROM_PRECEDING);
      }
      if ( bacty1 - 1 <= acty && acty <= bacty2 + 1 ) {
	/* 1 pixel apart from the bad pixels  */
    next_to_badpix:
	if ( TESTBIT(bccode, BIT_BADCTE) ) {
	  SETBIT(pixel_quality, BIT_1PIX_FROM_BADCTE);
	}
	if ( TESTBIT(bccode, BIT_HOT) ) {
	  SETBIT(pixel_quality, BIT_1PIX_FROM_HOT);
	}
	if ( TESTBIT(bccode, BIT_FLICKERING) ) {
	  SETBIT(pixel_quality, BIT_1PIX_FROM_FLICKERING);
	}
      }
      if ( acty == bacty1 - 2 || acty == bacty2 + 2 ) {
	goto next_next_to_badpix;
      }
    } else if ( actx == bactx - 2 || actx == bactx + 2 ) {
      if ( acty < bacty1 + 2 ) {
	/* 2 pixels apart from the preceding pixels  */
	SETBIT(pixel_quality, BIT_2PIX_FROM_PRECEDING);
      }
      if ( bacty1 - 2 <= acty && acty <= bacty2 + 2 ) {
	/* 2 pixels apart from the bad pixels  */
  next_next_to_badpix:
	if ( TESTBIT(bccode, BIT_BADCTE) ) {
	  SETBIT(pixel_quality, BIT_2PIX_FROM_BADCTE);
	}
	if ( TESTBIT(bccode, BIT_HOT) ) {
	  SETBIT(pixel_quality, BIT_2PIX_FROM_HOT);
	}
	if ( TESTBIT(bccode, BIT_FLICKERING) ) {
	  SETBIT(pixel_quality, BIT_2PIX_FROM_FLICKERING);
	}
      }
    }

  } /* end of for */

 skip_badcol:

/* check calmask information */

  if ( NULL != p->calmask && insidecalmask == p->calmask->image[acty][actx] ) {
    SETBIT(pixel_quality, BIT_CALMASK);
  }

/* check SCI rows */

  period = p->sci.period_rawy;
  if ( 0 < period ) {
    nrow = p->sci.nrow;
    sciy = p->sci.rawy;

    for (i = 0; i < nrow; i++) {
      switch ( rawy - sciy[i] ) {
      case 0:
	SETBIT(pixel_quality, BIT_SCI_ROW);
	goto skip_sci_row;
      case 1:
	SETBIT(pixel_quality, BIT_SCI_TRAILING_ROW);
	goto skip_sci_row;
      case 2:
	SETBIT(pixel_quality, BIT_SCI_2nd_TRAILING_ROW);
	goto skip_sci_row;
      case 3:
	SETBIT(pixel_quality, BIT_SCI_3rd_TRAILING_ROW);
	goto skip_sci_row;
      case -1:
	SETBIT(pixel_quality, BIT_SCI_PRECEDING_ROW);
	goto skip_sci_row;
      case -2:
	SETBIT(pixel_quality, BIT_SCI_2nd_PRECEDING_ROW);
	goto skip_sci_row;
      default:
	;
      }
    }

    /* special treatment for hidden ACTY */
    switch ( acty - p->sci.hidden_acty ) {
    case 0:
      SETBIT(pixel_quality, BIT_SCI_ROW);
      goto skip_sci_row;
    case 1:
      SETBIT(pixel_quality, BIT_SCI_TRAILING_ROW);
      goto skip_sci_row;
    case 2:
      SETBIT(pixel_quality, BIT_SCI_2nd_TRAILING_ROW);
      goto skip_sci_row;
    case 3:
      SETBIT(pixel_quality, BIT_SCI_3rd_TRAILING_ROW);
      goto skip_sci_row;
    case -1:
      SETBIT(pixel_quality, BIT_SCI_PRECEDING_ROW);
      goto skip_sci_row;
    case -2:
      SETBIT(pixel_quality, BIT_SCI_2nd_PRECEDING_ROW);
      goto skip_sci_row;
    default:
      ;
    }

 skip_sci_row:

    if ( TESTBIT(pixel_quality, BIT_SCI_ROW) ) {
      ap4n = p->sci.ap4n;
      ap4y = p->sci.ap4y;
      rawx = xis_actx2rawx(actx);
      if ( rawx < 4 ) {
	for (i = 0; i < ap4n; i++) {
	  if ( rawy == ap4y[i] ) {
	    SETBIT(pixel_quality, BIT_SCI_AP_ROW);
	    goto skip_sci_ap;
	  }
	}
      }
      ap256n = p->sci.ap256n;
      ap256y = p->sci.ap256y;
      for (i = 0; i < ap256n; i++) {
	if ( rawy == ap256y[i] ) {
	  SETBIT(pixel_quality, BIT_SCI_AP_ROW);
	  goto skip_sci_ap;
	}
      }
    }
  }

 skip_sci_ap:

  return pixel_quality;
}

int
xisPixqStatInit(PIXQ_STAT *p)
{
  static char *comment[] = {
    "B0           1 ***RESERVED***",
    "B1           2 AREADISCRI_EDGE",
    "B2           4 2PIX_FROM_SEGBOUNDARY",
    "B3           8 2PIX_FROM_PRECEDING",
    "B4          16 2PIX_FROM_BADCTE",
    "B5          32 2PIX_FROM_HOT",
    "B6          64 2PIX_FROM_FLICKERING",
    "B7         128 2PIX_FROM_WINBOUNDARY",
    "B8         256 1PIX_FROM_SEGBOUNDARY",
    "B9         512 SCI_3rd_TRAILING_ROW",
    "B10       1024 1PIX_FROM_PRECEDING",
    "B11       2048 PRECEDING",
    "B12       4096 1PIX_FROM_BADCTE",
    "B13       8192 1PIX_FROM_HOT",
    "B14      16384 1PIX_FROM_FLICKERING",
    "B15      32768 SCI_2nd_PRECEDING_ROW",
    "B16      65536 CALMASK",
    "B17     131072 SEGBOUNDARY",
    "B18     262144 SCI_2nd_TRAILING_ROW",
    "B19     524288 1PIX_FROM_WINBOUNDARY",
    "B20    1048576 BADCTE",
    "B21    2097152 HOT",
    "B22    4194304 FLICKERING",
    "B23    8388608 WINBOUNDARY",
    "B24   16777216 OUTSIDE_AREADISCRI",
    "B25   33554432 OTHER_BAD",
    "B26   67108864 ***RESERVED***",
    "B27  134217728 ***RESERVED***",
    "B28  268435456 SCI_PRECEDING_ROW",
    "B29  536870912 SCI_TRAILING_ROW",
    "B30 1073741824 SCI_AP_ROW",
    "B31 2147483648 SCI_ROW"
  };

  int i;

  p->flag_sel = 0;
  p->pixq_min = 0;
  p->pixq_max = 0;
  p->pixq_and = 0;
  p->pixq_eql = 0;

  p->clean_zero = p->safe = p->select = p->total = 0;
  p->safe_bit_level = BIT_SCI_2nd_TRAILING_ROW;
  p->bits_reserved = BITS_RESERVED;
  for (i = 0; i < NBITS_BCCODE; i++) {
    p->bits[i] = 0;
    p->cm[i] = comment[i];
  }

  return 0;
}

int
xisPixqStatAdd(PIXQ_STAT *p, unsigned pixel_quality)
{
  int i;

  p->total++;

  if ( 0 == pixel_quality ) {
    p->clean_zero++;
  } else {
    for (i = 0; i < NBITS_BCCODE; i++) {
      if ( TESTBIT(pixel_quality, i) ) {
	p->bits[i]++;
      }
    }
  }

  if ( pixel_quality < (1<< (p->safe_bit_level+1)) ) {
    p->safe++;
  }

  if ( p->pixq_min <= pixel_quality && pixel_quality <= p->pixq_max &&
       (pixel_quality & p->pixq_and) == p->pixq_eql ) {
    p->select++;
  }

  return 0;
}

static int
xisPixqStatWriteSub(PIXQ_STAT *p,
	int (*outfunc)(void *fp, char *msg, int *istat), void *fp)
{
  int i;
  char *tbar, *hbar, *labels, *sbar, safebits[8], *totbits, line[80];
  char safe_label[40], select_label[40];
  unsigned int pixel_quality;
  double safe_num, select_num;
  int flag_use;
  int num_use = 0;
  unsigned long sum = p->clean_zero;
  double tot = p->total / 100.0;
  int istat = 0;

  if ( 0.0 == tot ) tot = 1.0;

  if ( p->flag_sel ) {
    tbar = "\
=================================================================";
    labels = "\
 Bit    Decimal Name                            Count    Frac  S";
    hbar = "\
-----------------------------------------------------------------";
    sbar = "  -";
    sprintf(safebits, " %2d", p->safe_bit_level + 1);
    totbits = " 32";
  } else {
    tbar = "\
==============================================================";
    labels = "\
 Bit    Decimal Name                            Count    Frac";
    hbar = "\
--------------------------------------------------------------";
    sbar = "";
    safebits[0] = '\0';
    totbits = "";
  }

  (*outfunc)(fp, tbar, &istat);
  (*outfunc)(fp, labels, &istat);
  (*outfunc)(fp, hbar, &istat);

  safe_num = select_num = 0.0;
  for (i = 0; i < NBITS_BCCODE; i++) {
    pixel_quality = (1 << i);
    if ( i <= p->safe_bit_level ) {
      safe_num += pixel_quality;
    }
    flag_use = 0;
    if ( p->pixq_min <= pixel_quality && pixel_quality <= p->pixq_max &&
	 (pixel_quality & p->pixq_and) == p->pixq_eql ) {
      num_use++;
      flag_use = 1;
      select_num += pixel_quality;
    }
    sum += p->bits[i];
    sprintf(line, "\
 %-40s%12lu  %6.2f%s",
	p->cm[i], p->bits[i], p->bits[i]/tot,
	p->flag_sel ? ( flag_use ? "  o" : "  x" ) : "");
    (*outfunc)(fp, line, &istat);
  }
  sprintf(line, "\
 %-40s%12lu  %6.2f%s", "###          0 CLEAN_ZERO",
	p->clean_zero, p->clean_zero/tot, sbar);
  (*outfunc)(fp, line, &istat);
  (*outfunc)(fp, hbar, &istat);
  sprintf(line, "\
 %-40s%12lu  %6.2f%s", "+++ 4294967295 SUM", sum, sum/tot, totbits);
  (*outfunc)(fp, line, &istat);
  sprintf(safe_label, "::: %10.0f SAFE(B0-%d)", safe_num, p->safe_bit_level);
  sprintf(line, "\
 %-40s%12lu  %6.2f%s", safe_label, p->safe, p->safe/tot, safebits);
  (*outfunc)(fp, line, &istat);
  if ( p->flag_sel ) {
    sprintf(select_label, "::: %10.0f SELECT", select_num);
    sprintf(line, "\
 %-40s%12lu  %6.2f %2d", select_label, p->select, p->select/tot, num_use);
    (*outfunc)(fp, line, &istat);
  }
  sprintf(line, "\
 %-40s%12lu  %6.2f%s", ">>> 4294967295 TOTAL", p->total, 100.0, totbits);
  (*outfunc)(fp, line, &istat);
  (*outfunc)(fp, hbar, &istat);
  (*outfunc)(fp, "", &istat);

  return istat;
}

static int
writefp(void *fp, char *msg, int *istat)
{
  fprintf((FILE*)fp, "%s\n", msg);
  return 0;
}

int
xisPixqStatWrite(PIXQ_STAT *p, FILE *fp)
{
  return xisPixqStatWriteSub(p, writefp, fp);
}

static int
writefits(void *fp, char *msg, int *istat)
{
  if ( '\0' == *msg ) {
    fits_write_history((fitsfile*)fp, " ", istat);
  } else {
    fits_write_history((fitsfile*)fp, msg, istat);
  }
  return *istat;
}

int
xisPixqStatWriteFits(PIXQ_STAT *p, fitsfile *fp)
{
  return xisPixqStatWriteSub(p, writefits, fp);
}

static int
check_delimiters(int c)
{
  switch ( c ) {
  case '\0':
  case ';':
  case ',':
  case ' ':
  case '\t':
  case '\r':
  case '\n':
    return ANL_TRUE;
  default:
    ;
  }
  return ANL_FALSE;
}

static int
split_path(char *filelist, int n, char *outbuf, int outbuf_size)
{
  int i;
  char *p = filelist;

  for (i = 0; i < n; i++) {
    for (;;) {
      if ( check_delimiters(*p) ) {
	break;
      }
      p++;
    }
    if ( '\0' == *p ) {
      break;
    }
    p++;			/* skip delimeter */
  }

  if ( '\0' == *p ) {
    if ( NULL != outbuf && 0 < outbuf_size ) {
      outbuf[0] = '\0';
    }
    return -1;			/* n-th file not found */
  }

  if ( NULL == outbuf && outbuf_size <= 0 ) {
    return 0;			/* successfull split, but no output */
  }

  for (i = 0; i < outbuf_size; i++) {
    if ( check_delimiters(*p) ) {
      break;
    }
    outbuf[i] = *p;
    p++;
  }

  if ( i < outbuf_size ) {
    outbuf[i] = '\0';
    return 0;			/* successfull split */
  }

  outbuf[outbuf_size - 1] = '\0';
  anl_msg_warning("\
%s: WARNING: too long file name, truncated to '%s'\n", pname, outbuf);

  return 0;
}

static int
check_badcol_dupli(long num_badcol, BADCOL_INFO *bp, int actx, int acty)
{
  long i;

  for (i = 0; i < num_badcol; i++) {
    if ( bp->start <= DEFAULT_START && DEFAULT_STOP <= bp->stop ) {
      if ( actx == bp->actx && bp->acty1 <= acty && acty <= bp->acty2 ) {
	if ( TESTBIT(bp->bccode, BIT_HOT) ) {
	  return 1;		/* already in badcol_list */
	}
      }
    }
    bp++;
  }

  return 0;		/* no duplication in badcol_list */
}

static int
read_hotpixfile(PIXQ_INFO *p, HOTPIXFILE_INFO *hp)
{
  static int hdutype = BINARY_TBL;
  static char extname[] = "HOTPIXELS";
  static int extver = 0;	/* ignore EXTVER */

  char *k, file_instrume[FLEN_VALUE];
  BADCOL_INFO *badcol_list;
  long irow, num_tmp, num_hotpix, num_added, num_dupli, num_badcol;
  int icol, col_seg, col_rawx, col_rawy, anul;
  int seg, rawx, rawy, actx, acty;

  fitsfile *fp = NULL;
  int istat = 0;

  anl_msg_info("\
Reading HOTPIXELS in '%s' ...\n", hp->filename);

  fits_open_file(&fp, k=hp->filename, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  fits_movnam_hdu(fp, hdutype, k=extname, extver, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  fits_read_key_str(fp, k="INSTRUME", file_instrume, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if ( 0 != strcmp(p->instrume, file_instrume) ) {
    anl_msg_error("\
%s: INSTRUME keyword (%s) is inconsistent (%s)\n",
	pname, p->instrume, file_instrume);
    istat = -1;
    goto quit;
  }

  fits_get_num_rows(fp, &num_hotpix, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    goto quit;
  }

  if ( 0 == num_hotpix ) {
    anl_msg_error("\
%s: WARNING: no hot pixels in %s extension,\n\
    probably telemetry band was not sufficient\n", pname, extname);
    goto skip;
  }

  num_added = 0;
  num_dupli = 0;
  num_badcol = p->num_badcol;
  num_tmp = num_badcol + num_hotpix;
  badcol_list = realloc(p->badcol_list, sizeof(*badcol_list) * num_tmp);
  if ( NULL == badcol_list ) {
    anl_msg_error("\
%s: badcol_list realloc(num=%ld) failed\n", pname, num_tmp);
    /* previous badcol_list is unchanged */
    goto quit;
  }

  if (
fits_get_colnum(fp, CASESEN, k="SEGMENT", &col_seg, &istat) ||
fits_get_colnum(fp, CASESEN, k="RAWX", &col_rawx, &istat) ||
fits_get_colnum(fp, CASESEN, k="RAWY", &col_rawy, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  for (irow = 1; irow <= num_hotpix; irow++) {
    if (
fits_read_col_int(fp, icol=col_seg,  irow, 1, 1, 0, &seg,  &anul, &istat) ||
fits_read_col_int(fp, icol=col_rawx, irow, 1, 1, 0, &rawx, &anul, &istat) ||
fits_read_col_int(fp, icol=col_rawy, irow, 1, 1, 0, &rawy, &anul, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, icol, irow, istat);
      goto quit;
    }

    istat = xis_raw2act(seg, rawx, rawy, &actx, &acty);
    if ( istat ) {
      goto quit;
    }

    if ( check_badcol_dupli(num_badcol, badcol_list, actx, acty) ) {
      num_dupli++;
      continue;
    }

    badcol_list[num_badcol].start = DEFAULT_START;
    badcol_list[num_badcol].stop  = DEFAULT_STOP;
    badcol_list[num_badcol].actx  = actx;
    badcol_list[num_badcol].acty1 = acty;
    badcol_list[num_badcol].acty2 = acty;
    badcol_list[num_badcol].bccode = (1 << BIT_HOT);

    num_badcol++;
    num_added++;
  }

  p->num_badcol  = num_badcol;
  p->badcol_list = realloc(badcol_list, sizeof(*badcol_list) * num_badcol);
  if ( NULL == p->badcol_list ) {
    p->badcol_list = badcol_list;	/* badcol_list is kept unchanged */
  }

  hp->num_hotpix = num_hotpix;
  hp->num_added  = num_added;
  hp->num_dupli  = num_dupli;

  anl_msg_info("\
  num_hotpix=%ld  num_added=%ld  num_dupli=%ld\n",
	num_hotpix, num_added, num_dupli);

 skip:

  fits_close_file(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }
  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

int
xisPixqAddBadcolCache(PIXQ_INFO *p)
{
  long num_badcol;
  unsigned int bccode, next_to_bad, next_next_to_bad, *cache;
  int ms, i, ix, iy, ibadcol, actx, acty1, acty2;
  BADCOL_INFO *bp;

  anl_msg_info("\
Adding badcol_cache ...\n");

  ms = sizeof(*cache) * XISactiveFrameVsize * XISactiveFrameHsize;
  cache = malloc(ms);
  if ( NULL == cache ) {
    return -1;
  }
  memset(cache, 0, ms);

  bp = p->badcol_list;
  num_badcol = p->num_badcol;
  ibadcol = 0;
  while ( ibadcol < num_badcol ) {
    if ( DEFAULT_START < bp->start || bp->stop < DEFAULT_STOP ) {
      ibadcol++;
      bp++;
      continue;
    }

    actx  = bp->actx;
    acty1 = bp->acty1;
    acty2 = bp->acty2;
    bccode = bp->bccode;
    num_badcol--;
    for (i = ibadcol; i < num_badcol; i++) {
      p->badcol_list[i] = p->badcol_list[i+1];
    }

    next_to_bad = next_next_to_bad = 0;
    if ( TESTBIT(bccode, BIT_BADCTE) ) {
      SETBIT(next_to_bad, BIT_1PIX_FROM_BADCTE);
      SETBIT(next_next_to_bad, BIT_2PIX_FROM_BADCTE);
    }
    if ( TESTBIT(bccode, BIT_HOT) ) {
      SETBIT(next_to_bad, BIT_1PIX_FROM_HOT);
      SETBIT(next_next_to_bad, BIT_2PIX_FROM_HOT);
    }
    if ( TESTBIT(bccode, BIT_FLICKERING) ) {
      SETBIT(next_to_bad, BIT_1PIX_FROM_FLICKERING);
      SETBIT(next_next_to_bad, BIT_2PIX_FROM_FLICKERING);
    }

    ix = actx;
    for (iy = acty1; iy <= acty2; iy++) {
      cache[iy*XISactiveFrameHsize+ix] |= bccode;
    }
    for (iy = 0; iy < acty1; iy++) {
      SETBIT(cache[iy*XISactiveFrameHsize+ix], BIT_PRECEDING);
    }
    if ( 0 <= (iy = acty1 - 1) ) {
      cache[iy*XISactiveFrameHsize+ix] |= next_to_bad;
      if ( 0 <= (iy = acty1 - 2) ) {
	cache[iy*XISactiveFrameHsize+ix] |= next_next_to_bad;
      }
    }
    if ( (iy = acty2 + 1) < XISactiveFrameVsize ) {
      cache[iy*XISactiveFrameHsize+ix] |= next_to_bad;
      if ( (iy = acty2 + 2) < XISactiveFrameVsize ) {
	cache[iy*XISactiveFrameHsize+ix] |= next_next_to_bad;
      }
    }

    for (ix = actx - 1; ix <= actx + 1; ix += 2) {
      if ( 0 <= ix && ix < XISactiveFrameHsize ) {
	for (iy = 0; iy < acty1 + 1; iy++) {
	  SETBIT(cache[iy*XISactiveFrameHsize+ix], BIT_1PIX_FROM_PRECEDING);
	}
	for (iy = acty1 - 1; iy <= acty2 + 1; iy++) {
	  if ( 0 <= iy && iy < XISactiveFrameVsize ) {
	    cache[iy*XISactiveFrameHsize+ix] |= next_to_bad;
	  }
	}
	if ( 0 <= (iy = acty1 - 2) ) {
	  cache[iy*XISactiveFrameHsize+ix] |= next_next_to_bad;
	}
	if ( (iy = acty2 + 2) < XISactiveFrameVsize ) {
	  cache[iy*XISactiveFrameHsize+ix] |= next_next_to_bad;
	}
      }
    }

    for (ix = actx - 2; ix <= actx + 2; ix += 4) {
      if ( 0 <= ix && ix < XISactiveFrameHsize ) {
	for (iy = 0; iy < acty1 + 2; iy++) {
	  if ( iy < XISactiveFrameVsize ) {
	    SETBIT(cache[iy*XISactiveFrameHsize+ix], BIT_2PIX_FROM_PRECEDING);
	  }
	}
	for (iy = acty1 - 2; iy <= acty2 + 2; iy++) {
	  if ( 0 <= iy && iy < XISactiveFrameVsize ) {
	    cache[iy*XISactiveFrameHsize+ix] |= next_next_to_bad;
	  }
	}
      }
    }

  }

  anl_msg_info("\
  num_badcol = %ld -> %ld\n", p->num_badcol, num_badcol);

  bp = realloc(p->badcol_list, num_badcol * sizeof(*bp));
  if ( NULL != bp ) {
    p->badcol_list = bp;	/* if NULL, p->badcol_list is kept unchanged */
  }

  p->num_badcol = num_badcol;
  p->badcol_cache = cache;

  return 0;
}

int
xisPixqReadHotpixFiles(PIXQ_INFO *p, char *hotpix_filenames)
{
  long len, num_hotpixfile;
  int i, istat, outbuf_size;
  HOTPIXFILE_INFO *hp;
  char *filelist;
  char *outbuf = NULL;
  FILE *fp = NULL;

  filelist = hotpix_filenames;
  if ( NULL == filelist ||
       '\0' == filelist[0] ||
       0 == CLstricmp("none", filelist) ||
       0 ) {
    return 0;			/* nothing to do, just return */
  }

  if ( '@' == filelist[0] ) {
    fp = fopen(filelist+1, "r");
    if ( NULL == fp ) {
      anl_msg_error("\
%s: indirect hot pixel file list '%s' not found\n", pname, filelist);
      istat = -1;
      goto quit;
    }
    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    if ( -1 == len ) {
      anl_msg_error("\
%s: indirect hot pixel file list '%s' ftell() error\n", pname, filelist);
      istat = -1;
      goto quit;
    }
    if ( 0 == len ) {
      fclose(fp);
      return 0;			/* file is empty, just return */
    }
    rewind(fp);			/* rwind() returns no value */
    filelist = malloc(len + 1);	/* add +1 for last '\0' */
    if ( NULL == filelist ) {
      anl_msg_error("\
%s: malloc(size=%ld) failed for '%s'\n", pname, len+1, filelist);
      istat = -1;
      goto quit;
    }
    if ( len != fread(filelist, 1, len, fp) ) {
      anl_msg_error("\
%s: fread(len=%ld) failed for '%s'\n", pname, len, filelist);
      istat = -1;
      goto quit;
    }
    filelist[len] = '\0';	/* terminate filelist with '\0' */
    fclose(fp);
    fp = NULL;
  }

  outbuf_size = strlen(filelist) + 1;
  outbuf = malloc(outbuf_size);
  if ( NULL == outbuf ) {
    anl_msg_error("\
%s: malloc(size=%d) failed for outbuf\n", pname, outbuf_size);
    istat = -1;
    goto quit;
  }

  num_hotpixfile = 0;
  for (i = 0; 0 == split_path(filelist, i, outbuf, outbuf_size); i++) {
    if ( '\0' == outbuf[0] ) {
      continue;
    }
    num_hotpixfile++;
  }

  p->hotpixfile_list = malloc(sizeof(*p->hotpixfile_list) * num_hotpixfile);
  if ( NULL == p->hotpixfile_list ) {
    anl_msg_error("\
%s: hotpixfile_list malloc(num=%ld) failed", pname, num_hotpixfile);
    istat = -1;
    goto quit;
  }

  p->num_hotpixfile = 0;
  for (i = 0; 0 == split_path(filelist, i, outbuf, outbuf_size); i++) {
    if ( '\0' == outbuf[0] ) {
      continue;
    }
    if ( num_hotpixfile <= p->num_hotpixfile ) {
      /* this should not happen, but check for safety */
      anl_msg_error("\
%s: something is wroing in splitting filelist\n", pname);
      istat = -1;
      goto quit;
    }
    hp = &p->hotpixfile_list[p->num_hotpixfile];
    p->num_hotpixfile++;
    hp->filename = strdup(outbuf);
    if ( NULL == hp->filename ) {
      anl_msg_error("\
%s: strdup('%s') failed\n", pname, outbuf);
      istat = -1;
      goto quit;
    }
    istat = read_hotpixfile(p, hp);
    if ( istat ) {
      goto quit;
    }
  }
  if ( p->num_hotpixfile != num_hotpixfile ) {
    /* this should not happen, but check for safety */
    anl_msg_error("\
%s: something is wroing in splitting filelist\n", pname);
    istat = -1;
    goto quit;
  }

  xisPixqAddBadcolCache(p);
  anl_msg_info("\n");

  return 0;

 quit:

  if ( NULL != p->hotpixfile_list ) {
    for (i = 0; i < p->num_hotpixfile; i++) {
      if ( NULL != p->hotpixfile_list[i].filename ) {
	free(p->hotpixfile_list[i].filename);
      }
    }
    free(p->hotpixfile_list);
    p->num_hotpixfile = 0;
    p->hotpixfile_list = NULL;
  }
  if ( NULL != outbuf ) {
    free(outbuf);
  }
  if ( NULL != filelist && filelist != hotpix_filenames ) {
    free(filelist);
  }
  if ( NULL != fp ) {
    fclose(fp);
  }
  return istat;
}

int
xisPixqExpMapGenACT(PIXQ_INFO *p, PIXQ_STAT *s, double obstime)
{
  int iwin, num_win, win_st, win_en, win_siz;
  int ms, ipos, rawy, actx, acty;
  unsigned int pixel_quality, pixq_min, pixq_max, pixq_and, pixq_eql;
  int istat;

  pixq_min = s->pixq_min;
  pixq_max = s->pixq_max;
  pixq_and = s->pixq_and;
  pixq_eql = s->pixq_eql;

/* allocate memory for ACT exposure image */
  if ( NULL != p->actexpo ) {
    free(p->actexpo);
    p->actexpo = NULL;
  }
  ms = sizeof(*p->actexpo) * XISactiveFrameVsize * XISactiveFrameHsize;
  p->actexpo = malloc(ms);
  if ( NULL == p->actexpo ) {
    anl_msg_error("\
%s: actexpo malloc(size=%d) failed\n", pname, ms);
    istat = -1;
    goto quit;
  }
  memset(p->actexpo, 0, ms);

/* check window option */
  switch ( p->sci.winopt ) {
  case XISwindowOff:
    num_win = 1;
    break;
  case XISwindow4:
    num_win = 4;
    break;
  case XISwindow8:
    num_win = 8;
    break;
  case XISwindow16:
    num_win = 16;
    break;
  default:
    anl_msg_error("\
%s: unknown WIN_OPT=%d\n", pname, p->sci.winopt);
    istat = -1;
    goto quit;
  }

  win_st = p->sci.win_st;
  win_en = p->sci.win_st + p->sci.win_siz;
  win_siz = p->sci.win_siz;

/* calculate DET exposure image */
  for (iwin = 0; iwin < num_win; iwin++) {
    for (acty = win_st; acty < win_en; acty++) {
      rawy = iwin * win_siz + acty - win_st;
      for (actx = 0; actx < XISactiveFrameHsize; actx++) {
	pixel_quality = xisPixelQuality(p, obstime, actx, acty, rawy);
	if ( pixq_min <= pixel_quality && pixel_quality <= pixq_max &&
	     (pixel_quality & pixq_and) == pixq_eql ) {
	  p->actexpo[acty*XISactiveFrameHsize + actx] += 1;
	}
	xisPixqStatAdd(s, pixel_quality);
      }
    }
  }

  xisPixqStatWrite(s, stdout);

/* check whether BSCALE is needed or not, when window option */
  p->bscale = 1.0;
  if ( 1 < num_win ) {
    for (ipos = 0; ipos < XISactiveFrameVsize*XISactiveFrameHsize; ipos++) {
      if ( 0 != p->actexpo[ipos] % num_win ) {
	p->bscale = 1.0 / num_win;		/* BSCALE is needed */
	break;
      }
    }
    if ( 1.0 == p->bscale ) {
      for (ipos = 0; ipos < XISactiveFrameVsize*XISactiveFrameHsize; ipos++) {
	p->actexpo[ipos] /= num_win;
      }
    }
  }

  return 0;

 quit:
  if ( NULL != p->actexpo ) {
    free(p->actexpo);
    p->actexpo = NULL;
  }
  return istat;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
