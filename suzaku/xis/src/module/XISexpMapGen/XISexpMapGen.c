/*
    XISexpMapGen.c

    2007/05/27 Y.ISHISAKI	version 1.0

    2007/05/28 Y.ISHISAKI	version 1.1
	write HDUCLASS, HDUCLAS1, HDUCLAS2, HDUDOC, CREATOR keywords
	call fits_write_date(), fits_write_chksum() in write_history()
	use xisPixqExpMapGenACT() in _init()
	call write_dmkeys() in _init()
	use aste_caldb_find() instead of xisrsp_get_caldb_file()

    2007/05/28 Y.ISHISAKI	version 1.2
	add parameter, aberration

    2015/01/07 Y.ISHISAKI	version 1.3
	use p->tbuf instead of tbuf after realloc in get_next_exptime_events()

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_caldb.h"
#include "aste_att.h"
#include "aeFitsHeaderUtil.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"

/* convert ACT -> DET -> SKY for each pixel, and very very slow.
   Difference is less than 1 pixel order */
/*#define DENSE_SKY_MAPPING*/
#ifdef DENSE_SKY_MAPPING
/* strict treatment of aberration, as conducted in xiscoord, but
   takes abount twice time. Difference is less than 1 pixel order */
/*#define STRICT_ABERRATION*/
#endif

static char pname[] = "xisexpmapgen";
char XISexpMapGen_version[] = "version 1.3";

typedef struct {
  double winexp;
  double t;
  int flag_end;
  long irow;
  long nrows;
  int col_start;
  int col_stop;
  int idiv;
  int ndiv;
  double t0;
  double *tbuf;
} EXPOSURE_INFO;

typedef struct {
  double t;
  AtEulerAng ea;
  int detx;
  int dety;
  int nexp;
} EULER_INFO;

static struct {
  /* XISexpMapGen */
  char outfile[PIL_LINESIZE];
  char phafile[PIL_LINESIZE];
  char attitude[PIL_LINESIZE];
  char gtifile[PIL_LINESIZE];
  char hotpixfiles[PIL_LINESIZE];
  char *teldeffile, o_teldeffile[PIL_LINESIZE];
  char *badcolumfile, o_badcolumfile[PIL_LINESIZE];
  char *calmaskfile, o_calmaskfile[PIL_LINESIZE];
  int enable_pixq;
  unsigned int pixq_min;
  unsigned int pixq_max;
  unsigned int pixq_and;
  unsigned int pixq_eql;
  int aberration;
  int clobber;
} com;

static void
show_parameter(void)
{
  printf("\n");
  printf("%s: *** show parameter ***\n", pname);
  printf("\n");
  printf("%20s   '%s'\n", "OUTFILE", com.outfile);
  printf("%20s   '%s'\n", "PHAFILE", com.phafile);
  printf("%20s   '%s'\n", "ATTITUDE", com.attitude);
  printf("%20s   '%s'\n", "GTIFILE", com.gtifile);
  printf("%20s   '%s'%s\n", "TELDEF", com.teldeffile,
	 ( com.teldeffile == com.o_teldeffile ) ? "" : " (CALDB)");
  printf("%20s   %s\n", "ENABLE_PIXQ", com.enable_pixq ? "YES" : "NO");
  if ( com.enable_pixq ) {
    printf("%20s   '%s'\n", "HOTPIXFILES", com.hotpixfiles);
    printf("%20s   '%s'%s\n", "BADCOLUMFILE", com.badcolumfile,
	 ( com.badcolumfile == com.o_badcolumfile ) ? "" : " (CALDB)");
    printf("%20s   '%s'%s\n", "CALMASKFILE", com.calmaskfile,
	 ( com.calmaskfile == com.o_calmaskfile ) ? "" : " (CALDB)");
    printf("%20s   %u (0x%08x)\n", "PIXQ_MIN", com.pixq_min, com.pixq_min);
    printf("%20s   %u (0x%08x)\n", "PIXQ_MAX", com.pixq_max, com.pixq_max);
    printf("%20s   %u (0x%08x)\n", "PIXQ_AND", com.pixq_and, com.pixq_and);
    printf("%20s   %u (0x%08x)\n", "PIXQ_EQL", com.pixq_eql, com.pixq_eql);
  }
  printf("%20s   %s\n", "ABERRATION", com.aberration ? "YES" : "NO");
  printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
  printf("\n");
}

static int
get_next_exptime_gti(fitsfile *fp, EXPOSURE_INFO *p)
{
  char *k;
  int anul;
  double start, stop, span;

  int istat = 0;

  p->flag_end = 0;

  if ( 0 == p->irow ) {
    fits_get_num_rows(fp, &p->nrows, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
      goto quit;
    }
    if ( p->nrows < 1 ) {
      anl_msg_error("\
%s: empty GTI extension\n", pname);
      istat = -1;
      goto quit;
    }
    if (
fits_get_colnum(fp, CASESEN, k="START", &p->col_start, &istat) ||
fits_get_colnum(fp, CASESEN, k="STOP", &p->col_stop, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
    p->idiv = 0;
    anl_msg_info("\
%s: extracting time from GTI: ngti=%ld winexp=%.1f\n",
	pname, p->nrows,  p->winexp);
  }

  if ( 0 == p->idiv ) {
    p->irow++;
    if ( p->nrows < p->irow ) {
      p->flag_end = 1;
      return 0;
    }
fits_read_col_dbl(fp, p->col_start, p->irow, 1, 1, 0.0, &start, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('START') failed (%d)\n", pname, istat);
      goto quit;
    }
fits_read_col_dbl(fp, p->col_stop, p->irow, 1, 1, 0.0, &stop, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('STOP') failed (%d)\n", pname, istat);
      goto quit;
    }
    span = stop - start;
    p->ndiv = (int)( (span + p->winexp - 0.1) / p->winexp );
    if ( p->ndiv <= 0 ) {
      anl_msg_warning("\
%s: WARNING: strange GTI at irow=%ld, %.0f - %.0f\n",
	pname, p->irow, start, stop);
      p->t = (start + stop) / 2;
      return 0;
    }
    p->t0 = start - (p->winexp * p->ndiv - span - p->winexp) / 2;
    anl_msg_debug("%.3f - %.3f span=%.3f t0=%.3f ndiv=%d\n",
	start, stop, span, p->t0, p->ndiv);
  }

  p->t = p->t0 + p->idiv * p->winexp;
  p->idiv++;
  if ( p->ndiv <= p->idiv ) {
    p->idiv = 0;	/* goto next line */
  }

  return 0;

 quit:
  return istat;
}

static int
compare_double(const void *v1, const void *v2)
{
  if ( *(double *)v1 < *(double *)v2 ) {
    return -1;
  } else if ( *(double *)v1 > *(double *)v2 ) {
    return +1;
  }
  return 0;
}

static int
get_next_exptime_events(fitsfile *fp, EXPOSURE_INFO *p)
{
  char *k;
  int i, n, nalloc, col_time, anul;
  long irow, nrows;
  double t, *tbuf;

  int istat = 0;

  if ( 0 == p->irow ) {
    fits_get_num_rows(fp, &nrows, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
      goto quit;
    }
    if ( nrows < 1 ) {
      anl_msg_error("\
%s: empty EVENTS extension\n", pname);
      istat = -1;
      goto quit;
    }
    fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
    nalloc = 0;
    tbuf = NULL;
    n = 0;
    for (irow = 1; irow <= nrows; irow++) {
      fits_read_col_dbl(fp, col_time, irow, 1, 1, 0.0, &t, &anul, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_read_col('TIME') failed (%d)\n", pname, istat);
	goto quit;
      }
      for (i = n - 1; 0 <= i && t != tbuf[i]; i--) {
	;
      }
      if ( i < 0 ) {	/* not found */
	if ( nalloc <= n ) {
	  nalloc += 10000;
	  tbuf = realloc(tbuf, sizeof(*tbuf) * nalloc);
	  if ( NULL == tbuf ) {
	    anl_msg_error("\
%s: tbuf realloc(size=%d) failed\n", pname, nalloc);
	    istat = -1;
	    goto quit;
	  }
	}
	tbuf[n] = t;
	n++;
      }
    }
    p->tbuf = realloc(tbuf, sizeof(*tbuf) * n);
    if ( NULL == p->tbuf ) {
      p->tbuf = tbuf;	/* tbuf is kept unchanged */
    }
    qsort(p->tbuf, n, sizeof(*tbuf), compare_double);
    p->nrows = n;
    anl_msg_info("\
%s: extracting time from EVENTS: n=%ld nexp=%d winexp=%.1f\n",
	pname, nrows, n, p->winexp);
  }

  if ( p->irow < p->nrows ) {
    p->flag_end = 0;
    p->t = p->tbuf[p->irow];
    p->irow++;
  } else {
    p->flag_end = 1;
    free(p->tbuf);
  }

  return 0;

 quit:
  return istat;
}

static int
compare_eulinfo(const void *v1, const void *v2)
{
#define detx1 (((EULER_INFO *)v1)->detx)
#define detx2 (((EULER_INFO *)v2)->detx)
#define dety1 (((EULER_INFO *)v1)->dety)
#define dety2 (((EULER_INFO *)v2)->dety)
  if ( detx1 < detx2 ) {
    return -1;
  } else if ( detx1 > detx2 ) {
    return +1;
  } else if ( dety1 < dety2 ) {
    return -1;
  } else if ( dety1 > dety2 ) {
    return +1;
  }
  return 0;
#undef detx1
#undef detx2
#undef dety1
#undef dety2
}

static void
EulerToVect(AtEulerAng *ea, AtVect v)
{
	double sin_t = sin(ea->theta);

	v[0] = sin_t * cos(ea->phi);
	v[1] = sin_t * sin(ea->phi);
	v[2] = cos(ea->theta);
}

static int
VectToEuler(AtVect v, AtEulerAng *ea)
{
#define x (v[0])
#define y (v[1])
#define z (v[2])

	double r2, x2y2, rxy;

	x2y2 = x*x + y*y;
	r2 = x2y2 + z*z;
	if ( 0.0 == r2 ) {
		return -1;
	}

	if ( 0.0 == x2y2 ) {
		ea->phi = 0.0;
	} else {
		ea->phi = atan2(y, x);	/* -PI <= atan2 <= PI */
		if ( ea->phi < 0.0 ) {
			ea->phi += TWO_PI;
		}
	}

	rxy = sqrt(x2y2);
	ea->theta = atan2(rxy, z);	/* 0 <= theta, because 0 <= rxy */

#undef x
#undef y
#undef z

	return 0;
}

static int
shrink_eulinfo(EULER_INFO *eulinfo, int n)
{
  int i, istat, nexp;
  double ax, x, ay, y, at;
  AtVect av_vec, vec;
  AtEulerAng *ea, av_ea;

  if ( 1 == n ) {
    return 0;		/* no need to shrink */
  }

  nexp = 0;
  ax = ay = at = 0.0;
  av_vec[0] = av_vec[1] = av_vec[2] = 0.0;
  for (i = 0; i < n; i++) {
    nexp += eulinfo[i].nexp;
    ea = &eulinfo[i].ea;
    EulerToVect(ea, vec);
    av_vec[0] += vec[0];
    av_vec[1] += vec[1];
    av_vec[2] += vec[2];
    x = cos(ea->psi);
    y = sin(ea->psi);
    ax += x;
    ay += y;
    at += eulinfo[i].t;
  }

  istat = VectToEuler(av_vec, &av_ea);
  if ( istat ) {
    av_ea = eulinfo->ea;
  }

  av_ea.psi = atan2(ay, ax);		/* -PI <= atan2 <= PI */
  if ( av_ea.psi < 0.0 ) {
    av_ea.psi += TWO_PI;
  }

  eulinfo->nexp = nexp;
  eulinfo->ea = av_ea;
  eulinfo->t = at / nexp;

  return 0;
}

static int
calc_skyexpo(TELDEF *teldef, ATTFILE *attfile, fitsfile *gti_fp,
	     SKYREF *skyref, PIXQ_INFO *pixq_ip,
	     double exposure, int mjdrefi, double mjdreff, double *map)
{
  int num_win, iexp, nexp, ieuler, neuler, nalloc;
  int i, n, ix, iy, ipos, idetx, idety, iactx, iacty;
  EXPOSURE_INFO expinfo;
  int (*get_next_exptime)(fitsfile *fp, EXPOSURE_INFO *p);
  AtEulerAng ea;
  EULER_INFO *eulinfo, *ep;
  double t, detx, dety, skyx, skyy, alpha, delta, eulexp, factor;
  unsigned char aexp, *actexpo, *winmap, *cormap;
#ifndef STRICT_ABERRATION
  SKYREF cor_ref;
#endif

  TELDEF_ASTROE *aste = teldef->mission.aste;
  int sky_xpix1 = aste->sky.xpix1;
  int sky_ypix1 = aste->sky.ypix1;
  int nx = aste->sky.xsiz;
  int ny = aste->sky.ysiz;
  int nxny = nx * ny;
  double *expmap = map;
  int istat = 0;

  winmap = malloc( 2 * sizeof(*winmap) * nxny );
  if ( NULL == winmap ) {
    anl_msg_error("\
%s: winmap malloc(size=%d) failed\n", pname, nxny);
    istat = -1;
    goto quit;
  }
  cormap = &winmap[nxny];

  switch (pixq_ip->sci.winopt) {
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
%s: illegal WINOPT=%d\n", pname, pixq_ip->sci.winopt);
    istat = -1;
    goto quit;
  }
  expinfo.winexp = 8.0 / num_win;
  expinfo.irow = 0;

  get_next_exptime = get_next_exptime_events;
  fits_movnam_hdu(gti_fp, BINARY_TBL, "EVENTS", 0, &istat);
  if ( istat ) {
    istat = 0;	/* ignore error */
    get_next_exptime = get_next_exptime_gti;
    fits_movnam_hdu(gti_fp, BINARY_TBL, "GTI", 0, &istat);
    if ( istat ) {
      istat = 0;	/* ignore error */
      fits_movnam_hdu(gti_fp, BINARY_TBL, "STDGTI", 0, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: no EVENTS/GTI/STDGTI extension is found (%d)\n", pname, istat);
	goto quit;
      }
    }
  }

  nexp = 0;
  nalloc = 0;
  eulinfo = NULL;
  for (;;) {
    istat = (*get_next_exptime)(gti_fp, &expinfo);
    if ( istat ) goto quit;
    if ( expinfo.flag_end ) {
      break;
    }
    istat = aste_att_ea(attfile, expinfo.t, &ea);
    if ( istat ) {
      anl_msg_error("\
%s: aste_att_ea(t=%.0f) failed (%d)\n", pname, expinfo.t, istat);
      goto quit;
    }
    if ( nalloc <= nexp ) {
      nalloc += 10000;
      eulinfo = realloc(eulinfo, sizeof(*eulinfo) * nalloc);;
      if ( NULL == eulinfo ) {
	anl_msg_error("\
%s: eulinfo realloc(size=%d) failed\n", pname, nalloc);
	istat = -1;
	goto quit;
      }
    }
    alpha = skyref->alpha;
    delta = skyref->delta;
    if ( com.aberration ) {
      aste_inv_aberration(expinfo.t, mjdrefi, mjdreff, &alpha, &delta);
    }
    aste_ecs2det(teldef, &ea, alpha, delta, &detx, &dety);
    eulinfo[nexp].t = expinfo.t;
    eulinfo[nexp].ea = ea;
    eulinfo[nexp].detx = (int)floor(detx);
    eulinfo[nexp].dety = (int)floor(dety);
    eulinfo[nexp].nexp = 1;
    nexp++;
  }

  qsort(eulinfo, nexp, sizeof(*eulinfo), compare_eulinfo);
  iexp = neuler = 0;
  while ( iexp < nexp ) {
    ep = &eulinfo[iexp];
    idetx = ep->detx;
    idety = ep->dety;
    for (i = iexp; i < nexp; i++) {
      if ( idetx != eulinfo[i].detx || idety != eulinfo[i].dety ) {
	break;
      }
    anl_msg_debug("\
  %4d: %d %d: (%.6f %.6f %.6f) t=%.1f\n",
	i+1, eulinfo[i].detx, eulinfo[i].dety,
	eulinfo[i].ea.phi*RAD2DEG, eulinfo[i].ea.theta*RAD2DEG,
	eulinfo[i].ea.psi*RAD2DEG, eulinfo[i].t);
    }
    n = i - iexp;
    shrink_eulinfo(ep, n);
    if ( neuler < iexp ) {
      eulinfo[neuler] = *ep;
    }
    iexp += n;
    neuler++;
    anl_msg_debug("\
%04d: n=%d: %d %d: (%.6f %.6f %.6f) t=%.1f\n",
	neuler, ep->nexp, ep->detx, ep->dety,
	ep->ea.phi*RAD2DEG, ep->ea.theta*RAD2DEG, ep->ea.psi*RAD2DEG, ep->t);
  }

  anl_msg_info("\
%s: nexp=%d shrink to neuler=%d\n", pname, nexp, neuler);

  for (ipos = 0; ipos < nxny; ipos++) {
    expmap[ipos] = 0.0;
  }

  for (ieuler = 0; ieuler < neuler; ieuler++) {
/*  for (ieuler = 0; ieuler < neuler/10+1; ieuler++) {*/
    ep = &eulinfo[ieuler];
    ea = ep->ea;
    t  = ep->t;
#ifndef STRICT_ABERRATION
    cor_ref = *skyref;
    if ( com.aberration ) {
      aste_inv_aberration(t, mjdrefi, mjdreff, &cor_ref.alpha, &cor_ref.delta);
    }
#endif
    for (ipos = 0; ipos < nxny; ipos++) {
      winmap[ipos] = cormap[ipos] = 0;
    }
    anl_msg_info("\
%4d: n=%-6d [%d %d] (%.6f %.6f %.6f) t=%.1f\n",
	ieuler+1, ep->nexp, ep->detx, ep->dety,
	ea.phi*RAD2DEG, ea.theta*RAD2DEG, ea.psi*RAD2DEG, t);
    actexpo = pixq_ip->actexpo;

#ifndef DENSE_SKY_MAPPING

    {
      static struct {
	int x, y;
      } act[4] = {
	{ 0, 0 },
	{ XISactiveFrameHsize - 1, 0 },
	{ 0, XISactiveFrameVsize - 1 },
	{ XISactiveFrameHsize - 1, XISactiveFrameVsize - 1 }
      };
      struct { double x, y; } sky[4];
      double x, x0, x10, x20, x3210;
      double y, y0, y10, y20, y3210;

      for (i = 0; i < 4; i++) {
	xis_act2det(teldef, act[i].x, act[i].y, &idetx, &idety);
	aste_det2ecs(teldef, &ea, idetx, idety, &alpha, &delta);
	if ( com.aberration ) {
	  aste_cor_aberration(t, mjdrefi, mjdreff, &alpha, &delta);
	}
	aste_ecs2sky(teldef, skyref, alpha, delta, &sky[i].x, &sky[i].y);
      }

      x0 = sky[0].x;
      x10 = sky[1].x - x0;
      x20 = sky[2].x - x0;
      x3210 = sky[3].x - sky[2].x - sky[1].x + x0;

      y0 = sky[0].y;
      y10 = sky[1].y - y0;
      y20 = sky[2].y - y0;
      y3210 = sky[3].y - sky[2].y - sky[1].y + y0;

      for (iacty = 0; iacty < XISactiveFrameVsize; iacty++) {
	for (iactx = 0; iactx < XISactiveFrameHsize; iactx++) {
	  aexp = (NULL != actexpo) ? *actexpo++ : 1;

#define calc_winmap()	do {\
  skyx = x0 + x*x10 + y*x20 + x*y*x3210;\
  skyy = y0 + x*y10 + y*y20 + x*y*y3210;\
  ix = (int)(skyx + 0.5) - sky_xpix1;\
  if ( 0 <= ix && ix < nx ) {\
    iy = (int)(skyy + 0.5) - sky_ypix1;\
    if ( 0 <= iy && iy < ny ) {\
      ipos = iy * nx + ix;\
      winmap[ipos] += aexp;\
      cormap[ipos] += 1;\
    }\
  }\
} while (0)

          x = (iactx - 0.25) / (XISactiveFrameHsize - 1);
	  y = (iacty - 0.25) / (XISactiveFrameVsize - 1);
          calc_winmap();
          x = (iactx + 0.25) / (XISactiveFrameHsize - 1);
          calc_winmap();
	  y = (iacty + 0.25) / (XISactiveFrameVsize - 1);
          calc_winmap();
          x = (iactx - 0.25) / (XISactiveFrameHsize - 1);
	}
      }
    }

#else	/* DENSE_SKY_MAPPING */

    for (iacty = 0; iacty < XISactiveFrameVsize; iacty++) {
      for (iactx = 0; iactx < XISactiveFrameHsize; iactx++) {
	aexp = (NULL != actexpo) ? *actexpo++ : 1;
	xis_act2det(teldef, iactx, iacty, &idetx, &idety);

#ifndef STRICT_ABERRATION
#define calc_winmap()	do {\
  aste_det2sky(teldef, &ea, &cor_ref, detx, dety, &skyx, &skyy);\
  ix = (int)(skyx + 0.5) - sky_xpix1;\
  if ( 0 <= ix && ix < nx ) {\
    iy = (int)(skyy + 0.5) - sky_ypix1;\
    if ( 0 <= iy && iy < ny ) {\
      ipos = iy * nx + ix;\
      winmap[ipos] += aexp;\
      cormap[ipos] += 1;\
    }\
  }\
} while (0)
#else	/* STRICT_ABERRATION */
#define calc_winmap()	do {\
  aste_det2ecs(teldef, &ea, detx, dety, &alpha, &delta);\
  if ( com.aberration ) {\
    aste_cor_aberration(t, mjdrefi, mjdreff, &alpha, &delta);\
  }\
  aste_ecs2sky(teldef, skyref, alpha, delta, &skyx, &skyy);\
  ix = (int)(skyx + 0.5) - sky_xpix1;\
  if ( 0 <= ix && ix < nx ) {\
    iy = (int)(skyy + 0.5) - sky_ypix1;\
    if ( 0 <= iy && iy < ny ) {\
      ipos = iy * nx + ix;\
      winmap[ipos] += aexp;\
      cormap[ipos] += 1;\
    }\
  }\
} while (0)
#endif	/* STRICT_ABERRATION */

	detx = idetx - 0.25;
	dety = idety - 0.25;
	calc_winmap();		/* [idetx-0.25, idety-0.25] */
	detx = idetx + 0.25;
	calc_winmap();		/* [idetx+0.25, idety-0.25] */
	dety = idety + 0.25;
	calc_winmap();		/* [idetx+0.25, idety+0.25] */
	detx = idetx - 0.25;
	calc_winmap();		/* [idetx-0.25, idety+0.25] */
      }
    }

#endif	/* DENSE_SKY_MAPPING */

#undef calc_winmap

    eulexp = ep->nexp;
    for (ipos = 0; ipos < nxny; ipos++) {
      if ( 0 < cormap[ipos] ) {
	expmap[ipos] += eulexp * winmap[ipos] / cormap[ipos];
      }
    }
  }

  factor = pixq_ip->bscale * exposure / nexp;
  anl_msg_debug("%s: exposure=%.3f factor=%.3f\n", pname, exposure, factor);
  for (ipos = 0; ipos < nxny; ipos++) {
    expmap[ipos] *= factor;
  }

  free(eulinfo);
  free(winmap);

  return 0;

 quit:
  if ( NULL != winmap ) {
    free(winmap);
  }
  return istat;
}

static int
copy_keys(fitsfile *ifp, fitsfile *ofp)
{
  static char *keylist[] = {
    "TELESCOP", "INSTRUME", "OBS_MODE", "DATAMODE", "XIS-AEID", "EDITMODE",
    "CLK_MODE", "CODE_ID",  "WINOPT",   "WIN_ST",   "WIN_SIZ",  "SNAPTI*",
    "DELAY*",   "PSUM_L",   "CI",       "BINNING",  "SRAM_VER", "SCIPERIY",
    "SCISTATY", "SCISPEED", "SCIN",     "SCIY*",    "AP4N",     "AP4Y*",
    "AP256N",   "AP256Y*",  "OBS_ID",   "OBSERVER", "OBJECT",   "OBS_REM",
    "NOM_PNT",  "RA_OBJ",   "DEC_OBJ",  "RA_PNT",   "DEC_PNT",  "RA_NOM",
    "DEC_NOM",  "PA_NOM",   "MEAN_EA1", "MEAN_EA2", "MEAN_EA3", "RADECSYS",
    "EQUINOX",  "DATE-OBS", "DATE-END", "TSTART",   "TSTOP",    "TELAPSE",
    "ONTIME",   "LIVETIME", "EXPOSURE", "TIMESYS",  "MJDREFI",  "MJDREFF",
    "TIMEREF",  "TIMEUNIT", "TASSIGN",  "CLOCKAPP", "TIMEDEL",  "TIMEPIXR",
    "TIERRELA", "TIERABSO", "TLM_FILE", "TIM_FILE", "ATT_FILE", "ORB_FILE",
    "LEAPFILE", "TELDEF",   "ORIGIN",   "MJD-OBS",  "TIMEZERO", "SEQPNUM",
    "PROCVER",  "MK1STVER", "SOFTVER",  "CALDBVER",
    NULL
  };

  char **kp, keyname[9], card[FLEN_CARD];
  int i, len, keynum, keyexist, morekeys;
  int istat = 0;

  fits_get_hdrspace(ifp, &keyexist, &morekeys, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_get_hdrspace() failed (%d)\n", pname, istat);
    return istat;
  }

  for (keynum = 1; keynum <= keyexist; keynum++) {
    fits_read_record(ifp, keynum, card, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_record(keynum=%d) failed (%d)\n", pname, keynum, istat);
      return istat;
    }
    for (kp = keylist; NULL != *kp; kp++) {
      strcpy(keyname, *kp);
      len = strlen(keyname);
      if ( '*' == keyname[len-1] ) {
	len--;
      } else {
	for (i = len; i < 8; i++) {
	  keyname[i] = ' ';
	}
	keyname[8] = '\0';
	len = 8;
      }
      if ( 0 == strncmp(keyname, card, len) ) {
	break;
      }
    }
    if ( NULL != *kp ) {	/* keyword found in the keylist[] */
      fits_write_record(ofp, card, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_write_record('%s') failed (%d)\n", pname, *kp, istat);
	return istat;
      }
    }
  }

  return 0;
}

static int
write_dmkeys_det(fitsfile *fp, TELDEF *teldef)
{
  char *k, mform1[FLEN_VALUE];
  double optic1, optic2;

  TELDEF_ASTROE *aste = teldef->mission.aste;
  int istat = 0;

  sprintf(mform1, "%s,%s", aste->det.xcol, aste->det.ycol);
  aste_xrt2det(teldef, 0.0, 0.0, &optic1, &optic2);

  if (
fits_write_key_str(fp, k="CONTENT", "IMAGE",
	"image file", &istat) ||
fits_write_key_str(fp, k="HDUNAME", "IMAGE",
	"FITS Header Data Unit Name", &istat) ||
fits_write_key_lng(fp, k="IMGBIN", 1,
	"Image binning factor", &istat) ||
fits_write_key_str(fp, k="MTYPE1", aste->det.name,
	"DM Keyword: Descriptor name.", &istat) ||
fits_write_key_str(fp, k="MFORM1", mform1,
	"DM Keyword: Descriptor value.", &istat) ||
fits_write_key_str(fp, k="CTYPE1", aste->det.xcol,
	"X axis coordinate system", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX1", aste->det.xcen, 1,
	"X axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL1", 0.0, 1,
	"X coordinate of reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT1", aste->det.xscl, 3,
	"X axis increment", &istat) ||
fits_write_key_str(fp, k="CUNIT1", aste->det.xunit,
	"unit of X axis", &istat) ||
fits_write_key_str(fp, k="CTYPE2", aste->det.ycol,
	"Y axis coordinate system", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX2", aste->det.ycen, 1,
	"Y axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL2", 0.0, 1,
	"Y coordinate of reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT2", aste->det.yscl, 3,
	"Y axis increment", &istat) ||
fits_write_key_str(fp, k="CUNIT2", aste->det.yunit,
	"unit of Y axis", &istat) ||
fits_write_key_fixdbl(fp, k="CROTA2", 0.0, 1,
	"rotation angle (deg)", &istat) ||

fits_write_key_fixdbl(fp, k="OPTIC1", optic1, 2,
	"X optical axis", &istat) ||
fits_write_key_fixdbl(fp, k="OPTIC2", optic2, 2,
	"Y optical axis", &istat) ||

fits_write_key_str(fp, k="WCSNAMEP", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="WCSTY1P", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="CTYPE1P", aste->det.xcol,
	"Source of X-axis", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX1P", 1.0, 1,
	"X axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL1P", 1.0, 1,
	"coord of X ref pixel in original image", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT1P", 1.0, 1,
	"X axis increment", &istat) ||
fits_write_key_str(fp, k="WCSTY2P", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="CTYPE2P", aste->det.ycol,
	"Source of Y-axis", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX2P", 1.0, 1,
	"Y axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL2P", 1.0, 1,
	"coord of Y ref pixel in original image", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT2P", 1.0, 1,
	"Y axis increment", &istat) ||

fits_write_key_str(fp, k="HDUCLASS", "OGIP",
	"format conforms to OGIP/GSFC conventions", &istat) ||
fits_write_key_str(fp, k="HDUCLAS1", "IMAGE",
	"2-D image array", &istat) ||
fits_write_key_str(fp, "HDUCLAS2", "DETMAP",
	"map of detector focal plan", &istat) ||
fits_write_key_str(fp, k="HDUDOC", "OGIP/96-001",
	"OGIP memo describing format", &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  return 0;
}

static int
write_dmkeys_sky(fitsfile *fp, TELDEF *teldef, SKYREF *skyref)
{
  char *k, mform1[FLEN_VALUE];
  double cdelt1, cdelt2;
/*  double optic1, optic2;*/

  TELDEF_ASTROE *aste = teldef->mission.aste;
  int istat = 0;

  sprintf(mform1, "%s,%s", aste->sky.xcol, aste->sky.ycol);
  cdelt1 = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
  cdelt2 = + (aste->sky.yscl / aste->focallen) * RAD2DEG;

  if (
fits_write_key_str(fp, k="CONTENT", "IMAGE",
	"image file", &istat) ||
fits_write_key_str(fp, k="HDUNAME", "IMAGE",
	"FITS Header Data Unit Name", &istat) ||
fits_write_key_lng(fp, k="IMGBIN", 1,
	"Image binning factor", &istat) ||
fits_write_key_str(fp, k="MTYPE1", aste->sky.name,
	"DM Keyword: Descriptor name.", &istat) ||
fits_write_key_str(fp, k="MFORM1", mform1,
	"DM Keyword: Descriptor value.", &istat) ||
fits_write_key_str(fp, k="CTYPE1", "RA---TAN",
	"X axis coordinate system", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX1", aste->sky.xcen, 1,
	"X axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL1", skyref->alpha, 4,
	"X coordinate of reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT1", cdelt1, 7,
	"X axis increment", &istat) ||
/*fits_write_key_str(fp, k="CUNIT1", aste->sky.xunit,
	"unit of X axis", &istat) ||*/
fits_write_key_str(fp, k="CTYPE2", "DEC--TAN",
	"Y axis coordinate system", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX2", aste->sky.ycen, 1,
	"Y axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL2", skyref->delta, 4,
	"Y coordinate of reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT2", cdelt2, 7,
	"Y axis increment", &istat) ||
/*fits_write_key_str(fp, k="CUNIT2", aste->sky.yunit,
	"unit of Y axis", &istat) ||*/
fits_write_key_fixdbl(fp, k="CROTA2", skyref->roll, 1,
	"rotation angle (deg)", &istat) ||
/*
fits_write_key_fixdbl(fp, k="OPTIC1", optic1, 2,
	"X optical axis", &istat) ||
fits_write_key_fixdbl(fp, k="OPTIC2", optic2, 2,
	"Y optical axis", &istat) ||
*/
fits_write_key_str(fp, k="WCSNAMEP", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="WCSTY1P", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="CTYPE1P", aste->det.xcol,
	"Source of X-axis", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX1P", 1.0, 1,
	"X axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL1P", 1.0, 1,
	"coord of X ref pixel in original image", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT1P", 1.0, 1,
	"X axis increment", &istat) ||
fits_write_key_str(fp, k="WCSTY2P", "PHYSICAL",
	"", &istat) ||
fits_write_key_str(fp, k="CTYPE2P", aste->det.ycol,
	"Source of Y-axis", &istat) ||
fits_write_key_fixdbl(fp, k="CRPIX2P", 1.0, 1,
	"Y axis reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="CRVAL2P", 1.0, 1,
	"coord of Y ref pixel in original image", &istat) ||
fits_write_key_fixdbl(fp, k="CDELT2P", 1.0, 1,
	"Y axis increment", &istat) ||

fits_write_key_str(fp, k="HDUCLASS", "OGIP",
	"format conforms to OGIP/GSFC conventions", &istat) ||
fits_write_key_str(fp, k="HDUCLAS1", "IMAGE",
	"2-D image array", &istat) ||
fits_write_key_str(fp, "HDUCLAS2", "EXPOSURE",
	"map of detector focal plan", &istat) ||
fits_write_key_str(fp, k="HDUDOC", "OGIP/96-001",
	"OGIP memo describing format", &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  return 0;
}

static int
write_history(fitsfile *fp, PIXQ_INFO *p, PIXQ_STAT *statistics)
{
  HOTPIXFILE_INFO *hp;
  char *k, *task_name, *task_version, creator[FLEN_VALUE], history[1024];
  int i;

  int istat = 0;

  task_name = anl_task_name();
  task_version = anl_task_version();

  sprintf(creator, "%s version %s", task_name, task_version);
  if (
fits_write_key_str(fp, k="CREATOR", creator,
	"software that created this file", &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }
  fits_write_date(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_date() failed (%d)\n", pname, istat);
    return istat;
  }
  fits_write_chksum(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    return istat;
  }

  istat = aefits_write_name_vers(fp, task_name, task_version);
  if ( istat ) {
    return istat;
  }
  sprintf(history, "\
  outfile='%s'", com.outfile);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  phafile='%s'", com.phafile);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  attitude='%s'", com.attitude);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  gtifile='%s'", com.gtifile);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  teldef='%s'%s", com.teldeffile,
	  (com.teldeffile == com.o_teldeffile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  enable_pixq=%s  aberration=%s  clobber=%s",
	com.enable_pixq ? "yes" : "no",
	com.aberration ? "yes" : "no",
	com.clobber ? "yes" : "no");
  fits_write_history(fp, history, &istat);
  if ( com.enable_pixq ) {
    sprintf(history, "\
  hotpixfiles='%s'", com.hotpixfiles);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < p->num_hotpixfile; i++) {
      hp = &p->hotpixfile_list[i];
      sprintf(history, "\
    %d: '%s'", i+1, hp->filename);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
       num_hotpix=%ld  num_added=%ld  num_dupli=%ld",
	hp->num_hotpix, hp->num_added, hp->num_dupli);
      fits_write_history(fp, history, &istat);
    }
    sprintf(history, "\
  badcolumfile='%s'%s", com.badcolumfile,
	  (com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  calmaskfile='%s'%s", com.calmaskfile,
	  (com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  pixq_min=%u (0x%08x)  pixq_max=%u (0x%08x)",
	com.pixq_min, com.pixq_min, com.pixq_max, com.pixq_max);
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  pixq_and=%u (0x%08x)  pixq_eql=%u (0x%08x)",
	com.pixq_and, com.pixq_and, com.pixq_eql, com.pixq_eql);
    fits_write_history(fp, history, &istat);
  }

  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    return istat;
  }

  if ( com.enable_pixq ) {
    istat = xisPixqStatWriteFits(statistics, fp);
  }

  return istat;
}

static int
write_image_hdr(fitsfile *fp, int typecode, double bscale, int nx, int ny)
{
  static int naxis = 2;

  long naxes[2];
  char *k;
  int bitpix;
  int istat = 0;

  naxes[0] = nx;
  naxes[1] = ny;

  switch (typecode) {
  case TBYTE:
    bitpix = BYTE_IMG;
    break;
  case TSHORT:
    bitpix = SHORT_IMG;
    break;
  case TUSHORT:
    bitpix = USHORT_IMG;
    break;
  case TLONG:
    bitpix = LONG_IMG;
    break;
  case TULONG:
    bitpix = ULONG_IMG;
    break;
  case TFLOAT:
    bitpix = FLOAT_IMG;
    break;
  case TDOUBLE:
    bitpix = DOUBLE_IMG;
    break;
  default:
    anl_msg_error("\
%s: unknown typecode=%d\n", pname, typecode);
    return -1;
  }

  fits_create_img(fp, bitpix, naxis, naxes, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_img() failed (%d)\n", pname, istat);
    return istat;
  }

  if ( 1.0 != bscale ) {
    fits_write_key_fixdbl(fp, k="BSCALE", bscale, 4, "scaling factor", &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
      return istat;
    }
    fits_set_bscale(fp, 1.0, 0.0, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_set_bscale() failed (%d)\n", pname, istat);
      return istat;
    }
  }

  return istat;
}

void
XISexpMapGen_startup(int *status)
{
  com.teldeffile = com.o_teldeffile;
  com.badcolumfile = com.o_badcolumfile;
  com.calmaskfile = com.o_calmaskfile;

  *status = ANL_OK;
}

void
XISexpMapGen_com(int *status)
{
#define NVAL	17
  static char *names[NVAL] = {
    "SHOW_PARAMETER",
    "OUTFILE",
    "PHAFILE",
    "GTIFILE",
    "ATTITUDE",
    "TELDEF",
    "ENABLE_PIXQ",
    "HOTPIXFILES",
    "BADCOLUMFILE",
    "CALMASKFILE",
    "PIXQ_MIN",
    "PIXQ_MAX",
    "PIXQ_AND",
    "PIXQ_EQL",
    "ABERRATION",
    "CLOBBER",
    "EXIT"
  };
  static char *help[NVAL] = {
    "show current setting",
    "output exposure map file name",
    "input PHA file name",
    "input GTI file name",
    "input attitude file name",
    "telescope definition file in CALDB",
    "flag to enable pixel quality selection",
    "input hot pixel file(s)",
    "badcolumn file in CALDB",
    "calmask file in CALDB",
    "minimum value of allowed pixel quality",
    "maximum value of allowed pixel quality",
    "bit AND mask for pixel quality",
    "allowed pixel quality after bit AND mask",
    "correct aberration",
    "overwrite output file if exists",
    "exit from this menu"
  };
  char *k;
  int answer[2];
  int nreply = 1;

  unsigned int uint_min = 0;
  unsigned int uint_max = 4294967295u;
  double pixq_min = 0.0;
  double pixq_max = 524287.0;
  double pixq_and = 0.0;
  double pixq_eql = 0.0;

  if ( *status ) {		/* ftools */
    *status = ANL_QUIT;
    if (
PILGetFname(k="outfile", com.outfile) ||
PILGetFname(k="phafile", com.phafile) ||
PILGetFname(k="attitude", com.attitude) ||
PILGetFname(k="gtifile", com.gtifile) ||
PILGetFname(k="teldef", com.o_teldeffile) ||
PILGetBool (k="enable_pixq", &com.enable_pixq) ||
PILGetBool (k="aberration", &com.aberration) ||
PILGetBool (k="clobber", &com.clobber) ||
	0 ) {
      goto pil_error;
    }
    if ( com.enable_pixq ) {
      if (
PILGetFname(k="hotpixfiles", com.hotpixfiles) ||
PILGetFname(k="badcolumfile", com.o_badcolumfile) ||
PILGetFname(k="calmaskfile", com.o_calmaskfile) ||
PILGetReal (k="pixq_min", &pixq_min) ||
PILGetReal (k="pixq_max", &pixq_max) ||
PILGetReal (k="pixq_and", &pixq_and) ||
PILGetReal (k="pixq_eql", &pixq_eql) ||
	   0 ) {
    pil_error:
	anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
	*status = ANL_QUIT;
	return;
      }
    }
    goto skip;
  }

  for (;;) {
    CMinquir(pname, NVAL, names, help, nreply, answer);
    k = names[answer[1]-1];
    if ( 0 == strcmp("SHOW_PARAMETER", k) ) {
      show_parameter();
    } else if ( 0 == strcmp("OUTFILE", k) ) {
      CLtxtrd(k, com.outfile, sizeof(com.outfile));
    } else if ( 0 == strcmp("PHAFILE", k) ) {
      CLtxtrd(k, com.phafile, sizeof(com.phafile));
    } else if ( 0 == strcmp("GTIFILE", k) ) {
      CLtxtrd(k, com.gtifile, sizeof(com.gtifile));
    } else if ( 0 == strcmp("ATTITUDE", k) ) {
      CLtxtrd(k, com.attitude, sizeof(com.attitude));
    } else if ( 0 == strcmp("TELDEF", k) ) {
      CLtxtrd(k, com.o_teldeffile, sizeof(com.o_teldeffile));
    } else if ( 0 == strcmp("ENABLE_PIXQ", k) ) {
      CLlogrd(k, &com.enable_pixq);
    } else if ( 0 == strcmp("HOTPIXFILES", k) ) {
      CLtxtrd(k, com.hotpixfiles, sizeof(com.hotpixfiles));
    } else if ( 0 == strcmp("BADCOLUMFILE", k) ) {
      CLtxtrd(k, com.o_badcolumfile, sizeof(com.o_badcolumfile));
    } else if ( 0 == strcmp("CALMASKFILE", k) ) {
      CLtxtrd(k, com.o_calmaskfile, sizeof(com.o_calmaskfile));
    } else if ( 0 == strcmp("PIXQ_MIN", k) ) {
      CLfdprdL(k, &pixq_min, (double)uint_min, (double)uint_max);
    } else if ( 0 == strcmp("PIXQ_MAX", k) ) {
      CLfdprdL(k, &pixq_max, (double)uint_min, (double)uint_max);
    } else if ( 0 == strcmp("PIXQ_AND", k) ) {
      CLfdprdL(k, &pixq_and, (double)uint_min, (double)uint_max);
    } else if ( 0 == strcmp("PIXQ_EQL", k) ) {
      CLfdprdL(k, &pixq_eql, (double)uint_min, (double)uint_max);
    } else if ( 0 == strcmp("ABERRATION", k) ) {
      CLlogrd(k, &com.aberration);
    } else if ( 0 == strcmp("CLOBBER", k) ) {
      CLlogrd(k, &com.clobber);
    } else if ( 0 == strcmp("EXIT", k) ) {
      break;
    }
  }
#undef NVAL

 skip:

  if ( pixq_min < uint_min || uint_max < pixq_min ||
       pixq_max < uint_min || uint_max < pixq_max ||
       pixq_and < uint_min || uint_max < pixq_and ||
       pixq_eql < uint_min || uint_max < pixq_eql ||
       0 ) {
    anl_msg_error("\
%s: 'pixq_***' parameter out of range\n", pname);
    *status = ANL_QUIT;
    return;
  }

  com.pixq_min = (unsigned int)pixq_min;
  com.pixq_max = (unsigned int)pixq_max;
  com.pixq_and = (unsigned int)pixq_and;
  com.pixq_eql = (unsigned int)pixq_eql;

  *status = ANL_OK;
}

void
XISexpMapGen_init(int *status)
{
  PIXQ_INFO pixq;
  PIXQ_STAT statistics;
  int ipos, actx, acty, detx, dety, map_size;
  int det_xsiz, det_ysiz, sky_xsiz, sky_ysiz, mjdrefi;
  double tstart, tstop, exposure, obstime, ra_nom, dec_nom, mjdreff;
  char *k, instrume[FLEN_VALUE];
  TELDEF *teldef;
  TELDEF_ASTROE *aste;
  SKYREF skyref;
  long felem, nelem;
  unsigned char detexpo, detoneline[XISactiveFrameHsize];

  double *map = NULL;
  fitsfile *ifp = NULL;
  fitsfile *ofp = NULL;
  fitsfile *gti_fp = NULL;
  ATTFILE *attfile = NULL;
  int istat = 0;

/* open input pha file to read keywords for CALDB */
  if (
fits_open_file(&ifp, k=com.phafile, READONLY, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if (
fits_read_key_str(ifp, k="INSTRUME", instrume, NULL, &istat) ||
fits_read_key_dbl(ifp, k="TSTART", &tstart, NULL, &istat) ||
fits_read_key_dbl(ifp, k="TSTOP", &tstop, NULL, &istat) ||
fits_read_key_dbl(ifp, k="EXPOSURE", &exposure, NULL, &istat) ||
fits_read_key(ifp, TINT, k="MJDREFI", &mjdrefi, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MJDREFF", &mjdreff, NULL, &istat) ||
fits_read_key_dbl(ifp, k="RA_NOM", &ra_nom, NULL, &istat) ||
fits_read_key_dbl(ifp, k="DEC_NOM", &dec_nom, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  obstime = (tstart + tstop) / 2;

/* find CALDB files */
  com.teldeffile = aste_caldb_find(instrume, "TELDEF", com.o_teldeffile);
  if ( NULL == com.teldeffile ) goto quit;
  if ( com.enable_pixq ) {
    com.badcolumfile = aste_caldb_find(instrume, "BADPIX", com.o_badcolumfile);
    if ( NULL == com.badcolumfile ) goto quit;
    com.calmaskfile = aste_caldb_find(instrume, "CALMASK", com.o_calmaskfile);
    if ( NULL == com.calmaskfile ) goto quit;
  }

  show_parameter();

/* initialize aste_coord */
  teldef = aste_coord_init(NULL, instrume, com.teldeffile);
  if ( NULL == teldef ) {
    anl_msg_error("\
%s: aste_coord_init('%s') failed\n", pname, com.teldeffile);
    goto quit;
  }
  aste = teldef->mission.aste;
  det_xsiz = aste->det.xsiz;
  det_ysiz = aste->det.ysiz;
  sky_xsiz = aste->sky.xsiz;
  sky_ysiz = aste->sky.ysiz;

/* open attitude file */
  if ( 0 == CLstricmp("none", com.attitude) ) {
    attfile = NULL;
  } else {
    attfile = aste_att_init(com.attitude);
    if ( NULL == attfile ) {
      anl_msg_error("\
%s: could not find attfile '%s'\n", pname, com.attitude);
      goto quit;
    }
    if ( 0 == CLstricmp("none", com.gtifile) ) {
      fits_reopen_file(ifp, &gti_fp, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
	goto quit;
      }
    } else {
      fits_open_file(&gti_fp, k=com.gtifile, READONLY, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
	goto quit;
      }
    }
    map_size = sizeof(*map) * sky_xsiz * sky_ysiz;
    map = malloc(map_size);
    if ( NULL == map ) {
      anl_msg_error("\
%s: exposure map malloc(wize=%d) failed\n", pname, map_size);
      goto quit;
    }
  }

/* create output exposure file */
  if ( com.clobber ) {
    unlink(com.outfile);
  }
  if (
fits_create_file(&ofp, k=com.outfile, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

/* check 'enable_pixq' parameter */
  if ( 0 == com.enable_pixq ) {
    pixq.badcol_list = NULL;
    pixq.hotpixfile_list = NULL;
    pixq.badcol_cache = NULL;
    pixq.calmask = NULL;
    pixq.actexpo = NULL;
    pixq.bscale = 1.0;
    goto skip_pixq;
  }

/* initialize pixel quality function */
  pixq.hotpix_filenames = com.hotpixfiles;
  pixq.badcol_filename  = com.badcolumfile;
  pixq.calmask_filename = com.calmaskfile;
  istat = xisSciReadKeys(ifp, &pixq.sci);
  if ( istat ) {
    goto quit;
  }
  istat = xisPixelQualityInit(instrume, &pixq);
  if ( istat ) {
    goto quit;
  }

/* initialize pixel quality statistics */
  xisPixqStatInit(&statistics);
  statistics.flag_sel = 1;
  statistics.pixq_min = com.pixq_min;
  statistics.pixq_max = com.pixq_max;
  statistics.pixq_and = com.pixq_and;
  statistics.pixq_eql = com.pixq_eql;

/* generate exposure map in ACT coordinates */
  istat = xisPixqExpMapGenACT(&pixq, &statistics, obstime);
  if ( istat ) {
    goto quit;
  }

 skip_pixq:

/* write DET exposure image */
  istat = write_image_hdr(ofp, TBYTE, pixq.bscale, det_xsiz, det_ysiz);
  if ( istat ) {
    goto quit;
  }
  istat = copy_keys(ifp, ofp);
  if ( istat ) {
    goto quit;
  }
  istat = write_dmkeys_det(ofp, teldef);
  if ( istat ) {
    goto quit;
  }
  istat = write_history(ofp, &pixq, &statistics);
  if ( istat ) {
    goto quit;
  }
  ipos = 0;
  detexpo = 1;
  felem = 1L;
  nelem = det_xsiz;
  for (dety = 1; dety <= det_ysiz; dety++) {
    for (detx = 1; detx <= det_xsiz; detx++) {
      if ( NULL != pixq.actexpo ) {
	xis_det2act(teldef, detx, dety, &actx, &acty);
	ipos = acty * XISactiveFrameHsize + actx;
	detexpo = pixq.actexpo[ipos];
      }
      detoneline[detx-1] = detexpo;
    }
    fits_write_img(ofp, TBYTE, felem, nelem, detoneline, &istat);
    felem += nelem;
  }
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_img() failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    goto quit;
  }

/* calculate sky exposure map */
  if ( NULL == attfile ) {
    goto skip_skyexpo;
  }
  skyref.alpha = ra_nom;
  skyref.delta = dec_nom;
  skyref.roll  = 0.0;
  istat = calc_skyexpo(teldef, attfile, gti_fp, &skyref, &pixq,
		exposure, mjdrefi, mjdreff, map);
  if ( istat ) goto quit;

/* write sky exposure image */
  istat = write_image_hdr(ofp, TFLOAT, 1.0, sky_xsiz, sky_ysiz);
  if ( istat ) {
    goto quit;
  }
  fits_write_key_str(ofp, k="EXTNAME", "EXPOSURE",
	"name of this image extension", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  istat = copy_keys(ifp, ofp);
  if ( istat ) {
    goto quit;
  }
 istat = write_dmkeys_sky(ofp, teldef, &skyref);
  if ( istat ) {
    goto quit;
  }
  istat = write_history(ofp, &pixq, &statistics);
  if ( istat ) {
    goto quit;
  }
  nelem = sky_xsiz * sky_ysiz;
  fits_write_img(ofp, TDOUBLE, 1, nelem, map, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_img() failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_write_chksum(ofp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
    goto quit;
  }

/* free map */
  if ( NULL != map ) {
    free(map);
  }

/* close input GTI file */
  if ( NULL != gti_fp ) {
    fits_close_file(gti_fp, &istat);
    gti_fp = NULL;
    if ( istat ) {
      anl_msg_error("\
%s: fits_close_file() for GTI failed (%d)\n", pname, istat);
      goto quit;
    }
  }

/* close attitude */
  if ( NULL != attfile ) {
    aste_att_close(attfile);
  }

 skip_skyexpo:

/* free actexpo */
  if ( NULL != pixq.actexpo ) {
    free(pixq.actexpo);
  }

/* free badcol_cache */
  if ( NULL != pixq.badcol_cache ) {
    free(pixq.badcol_cache);
  }

/* close input pha file */
  fits_close_file(ifp, &istat);
  ifp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.phafile, istat);
    goto quit;
  }

/* close output exposure file */
  fits_close_file(ofp, &istat);
  ofp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.outfile, istat);
    goto quit;
  }

  *status = ANL_OK;
  return;

 quit:
  if ( NULL != map ) {
    free(map);
  }
  if ( NULL != attfile ) {
    aste_att_close(attfile);
  }
  if ( NULL != gti_fp ) {
    int istat2 = 0;
    fits_close_file(gti_fp, &istat2);	/* ignore error */
  }
  if ( NULL != ifp ) {
    int istat2 = 0;
    fits_close_file(ifp, &istat2);	/* ignore error */
  }
  if ( NULL != ofp ) {
    int istat2 = 0;
    fits_delete_file(ofp, &istat2);	/* ignore error */
  }

  *status = ANL_QUIT;
  return;
}

void
XISexpMapGen_his(int *status)
{
  *status = ANL_OK;
}

void
XISexpMapGen_bgnrun(int *status)
{
  *status = ANL_OK;
}

void
XISexpMapGen_ana(int *nevent, int *eventid, int *status)
{
  *status = ANL_QUIT;
}

void
XISexpMapGen_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISexpMapGen_exit(int *status)
{
  anl_msg_info("\
%s: created '%s'\n", pname, com.outfile);

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
