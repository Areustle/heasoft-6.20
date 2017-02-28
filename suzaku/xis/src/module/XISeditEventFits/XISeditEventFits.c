/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:06 1999 by E. Miyata*/
/* xis_module/XISeditEventFits/0.3_fast/XISeditEventFits_v0.3.c: modified by M.Ozaki on Fri Aug 13 21:04:46 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/*************************************

  XISeditEventFits
	1999/07/17 created by miyata

	Ftools にむけて XIS 用の eventFits を加工して最後に
	event fits を吐き出すようにする。
	方針としては、
	EXPOSURE, LOST_AREA, GTI はすべてコピー。
	FRAME, EVENT のみ編集が必要。

	良く考えると、これまでと違って、単一sensor,clock,edit
	なんですんごい楽。すべての extension が同時に開ける。

    version 0.0 1999.07.17		Emi Miyata
	とりあえず、単に書き出せるようになったよバージョン

    version 0.2 1999.07.25		Emi Miyata
	modify frame bank name

    version 0.3 1999.07.29		Emi Miyata
	modify treatment of 'frame last event'

    version 0.5 1999.12.30		Emi Miyata
	bnkdef/bnkput fits pointer of output file

    version 0.7 2005.07.21		Hiroshi Nakajima
	eliminate some sources of compile warnings

    version 0.8 2005.09.13		Kiyoshi Hayashida
        frame extension and exposure extension are simply copied.

    version 0.9 2005.09.16		Kiyoshi Hayashida
        any order of extension_id is allowed.
    version 1.0 2005.10.03		Kiyoshi Hayashida
        compatible with hotpixel extension
    version 1.1 2005.12.05		Hiroshi Nakajima
        transparent when the edit mode is Frame, DarkInit,
        DarkUpdate, and DarkFrame mode
    version 1.2 2005.12.09		Hiroshi Nakajima
	Do not get Bnk XIS:EXPNAMENOS when DarfFrame mode
    version 1.3 2006.08.24		Y.ISHISAKI
	remove unused "cfortran.h", "aetimeUtil.h", "headas.h"
    version 1.31 2006.09.11  L. Angelini for XIS team
        Change STDGTI reference to GTI
    version 1.4 2007.01.29  Y.ISHISAKI
        check both GTI & STDGTI in copyExtensionFile()
	malloc eventExt in copyExtensionFile()
	pass extension_num to fitsFileMerge() in _exit()
	BnkGet XIS:OUTFITS:FILENAME:PTR in _exit()
    version 1.5 2007.01.31  Y.ISHISAKI
	call aefits_write_module_history() in _init()
	call fits_write_date(), fits_write_chksum() in _exit()
    version 1.6 2007.04.31  Y.ISHISAKI
	remove unused include files, "xisread.h", "XISeditEventFits.h"
	remove useless check of "XIS:skip event" in _ana
    version 2.0 2007.05.06  Y.ISHISAKI
	always BnkDef "XIS:OUTFITS:EXPOSURES:PTR" for primary use
	BnkPut NULL fitsfile pointer when extensions do not exist
	call fits_set_hdrsize() after fits_copy_header()
	BnkDef/Put "XIS:OUTFITS:NUM_EXT", "XIS:OUTFITS:FITS_EXT:PTR" in _ini()
    version 2.1 2007.05.14  Y.ISHISAKI
	treat HOSPIXELS extension as EVENTS when DarkInit, DarkUpdate
*************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "pil.h"
#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisFitsHeaderUtil.h"
#include "xisEventFitsUtil.h"
#include "xisEditEventFitsUtil.h"
#include "aeFitsHeaderUtil.h"

/* #define _PRINT_OUT_ */

/* function information */
static char pname[] = "XISeditEventFits";
char XISeditEventFits_version[] = "version 2.1";

/* XIS DE Edit Mode */
static int editmode;

/* fits information */
static char *infile;
static char *outfile;
static FITS_EXT_STRUCT *fitsExt;

/* fits column information */
static COLUMN_INF *event_column_inf;
static int event_column_num;
static int event_value_max_size;

static long event_num;

/* extension id's are defined by looking at the input fits file */
static int extension_num;
static int frame_ext_id, exposure_ext_id, event_ext_id;
static int lost_event_ext_id, gti_ext_id;

/* char *getExtensionName (int extnum) in xisEventFitsUtil.c is not used */
static char *
getExtensionName_local(fitsfile *fp, int extnum)
{
  static char extname[FLEN_VALUE];
  char *k;
  int istat = 0;

  if ( extnum == 0 ) {
    return "PRIMARY";
  }

  fits_read_key_str(fp, k="EXTNAME", extname, NULL, &istat);
  if ( istat ) {
    anl_msg_warning("\
%s: WARNING: fits_read_key_str('%s') not found for extnum=%d (%d)\n",
	pname, k, extnum, istat);
    return "";
  }

  return extname;
}

static void
getTempEditFileName(fitsfile *fp, int extnum, char *filename)
{
  sprintf(filename, "%s-xis_%s.evt", tempnam("./", NULL),
	  getExtensionName_local(fp, extnum));
}

static int
copyExtensionFile(fitsfile *infitsd)
{
  static int morekeys = 80;	/* 適当 */

  int extnum, hdutype, extver;
  int istat = 0;

/* check the total number of extensions */
fits_get_num_hdus(infitsd, &extension_num, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_get_num_hdus() failed (%d)\n", pname, istat);
    return istat;
  }

  fitsExt = malloc(extension_num * sizeof(*fitsExt));
  if ( NULL == fitsExt ) {
    anl_msg_error("\
%s: fitsExt malloc(extension_num=%d) failed\n", pname, extension_num);
    return -1;
  }

/* check extension name and its id */
  hdutype = BINARY_TBL;
  extver = 0;		/* ignore EXTVER */

fits_movnam_hdu(infitsd, hdutype, FRAME_EXTENSION_NAME, extver, &istat);
  if ( istat ) {
    istat = 0;		/* ignore error */
    frame_ext_id = -1;
  } else {
fits_get_hdu_num(infitsd, &extnum);
    frame_ext_id = extnum - 1;
  }

fits_movnam_hdu(infitsd, hdutype, EXPOSURE_EXTENSION_NAME, extver, &istat);
  if ( istat ) {
    istat = 0;		/* ignore error */
    exposure_ext_id = -1;
fits_movnam_hdu(infitsd, hdutype, EXPOSURE_NOS_EXTENSION_NAME, extver, &istat);
    if ( istat ) {
      istat = 0;	/* ignore error */
      exposure_ext_id = -1;
    } else {
fits_get_hdu_num(infitsd, &extnum);
      exposure_ext_id = extnum - 1;
    }
  } else {
fits_get_hdu_num(infitsd, &extnum);
    exposure_ext_id = extnum - 1;
  }

  switch ( editmode ) {
  case XISedit5x5:
  case XISedit3x3:
  case XISedit2x2:
  case XISeditTiming:
fits_movnam_hdu(infitsd, hdutype, EVENT_EXTENSION_NAME, extver, &istat);
    if ( istat ) {
      istat = 0;	/* ignore error */
      event_ext_id = -1;
    } else {
fits_get_hdu_num(infitsd, &extnum);
      event_ext_id = extnum - 1;
    }
    break;
  case XISeditDarkInit:
  case XISeditDarkUpdate:
    /* treat HOSPIXELS extension as EVENTS */
fits_movnam_hdu(infitsd, hdutype, HOTPIXEL_EXTENSION_NAME, extver, &istat);
    if ( istat ) {
      istat = 0;	/* ignore error */
      event_ext_id = -1;
    } else {
fits_get_hdu_num(infitsd, &extnum);
      event_ext_id = extnum - 1;
    }
    break;
  default:
    event_ext_id = -1;
  }

fits_movnam_hdu(infitsd, hdutype, LOST_EVENT_EXTENSION_NAME, extver, &istat);
  if ( istat ) {
    istat = 0;		/* ignore error */
    lost_event_ext_id = -1;
  } else {
fits_get_hdu_num(infitsd, &extnum);
    lost_event_ext_id = extnum - 1;
  }

  if ( XISeditDarkFrame != editmode ) {
fits_movnam_hdu(infitsd, hdutype, GTI_EXTENSION_NAME, extver, &istat);
    if ( istat ) {
      istat = 0;	/* ignore error */
fits_movnam_hdu(infitsd, hdutype, STDGTI_EXTENSION_NAME, extver, &istat);
    }
    if ( istat ) {
      istat = 0;	/* ignore error */
      gti_ext_id = extnum -1;
    } else {
fits_get_hdu_num(infitsd, &extnum);
      gti_ext_id = extnum - 1;
    }
  }

#ifdef _PRINT_OUT_
  fprintf(stderr,"%d %d %d %d %d %d\n",
	  frame_ext_id, exposure_ext_id,
	 event_ext_id, lost_event_ext_id, gti_ext_id,
         exposure_nos_ext_id);
#endif

  for (extnum = 0; extnum < extension_num; extnum++) {
/* move infitsd to each extension id */
fits_movabs_hdu(infitsd, extnum+1, &hdutype, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_movabs_hdu(hdunum=%d) failed (%d)\n", pname, extnum+1, istat);
      return istat;
    }

/* get temporary file for each extension */
    getTempEditFileName(infitsd, extnum, fitsExt[extnum].filename);

/* open each extension file */
fits_create_file(&fitsExt[extnum].fitsd, fitsExt[extnum].filename, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_create_file() failed for extnum=%d (%d)\n", pname, extnum, istat);
      return istat;
    }

    if ( extnum != PRIMARY_HDU_ID ) {
fits_create_hdu(fitsExt[extnum].fitsd, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_create_hdu() failed for extnum=%d (%d)\n", pname, extnum, istat);
	return istat;
      }
    }

/* copy HDU */
    if ( event_ext_id == extnum ) {
fits_copy_header(infitsd, fitsExt[extnum].fitsd, &istat);
fits_set_hdrsize(fitsExt[extnum].fitsd, morekeys, &istat);
      /* fits_set_hdrsize() must be immediately after fits_copy_header() */
    } else {
fits_copy_hdu(infitsd, fitsExt[extnum].fitsd, morekeys, &istat);
    }

    if ( istat ) {
      anl_msg_error("\
%s: fits_copy_hdu(hdunum=%d) failed (%d)\n", pname, extnum+1, istat);
      return istat;
    }
  }

  return 0;
}

void
XISeditEventFits_startup(int *status)
{
  *status = ANL_OK;
}

void
XISeditEventFits_com(int *status)
{
  *status = ANL_OK;
}

void
XISeditEventFits_init(int *status)
{
  int used, flag_expname_nos;
  char history[2*PIL_LINESIZE];
  fitsfile *_infitsd, *infitsd, *fp;

  int istat = 0;

  EvsDef("XISeditEventFits:BEGIN");
  EvsDef("XISeditEventFits:ENTRY");
  EvsDef("XISeditEventFits:OK");

  /* Get Event mode */
  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &editmode);

  /* open input event fits file */
  BnkfGetM("XIS:FITS:PTR", sizeof(_infitsd), &used, &_infitsd);
  fits_reopen_file(_infitsd, &infitsd, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  /* create output event fits file */
  istat = copyExtensionFile(infitsd);
  if ( istat ) {
    goto quit;
  }

/* close input FITS file */
  fits_close_file(infitsd, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  /* bankget column info */
  BnkGet("XIS:EVENTS:LIST:PTR", sizeof(char *), &used, &event_column_inf);
  BnkGet("XIS:EVENTS:LIST:NUM", sizeof(int), &used, &event_column_num);
  BnkGet("XIS:EVENTS:LIST:MAXSIZE", sizeof(int), &used, &event_value_max_size);
  BnkGet("XIS:EXPNAMENOS", sizeof(int), &used, &flag_expname_nos);

  /* bnkdef/put output file pointer */
  BnkDef("XIS:OUTFITS:NUM_EXT", sizeof(int));
  BnkPut("XIS:OUTFITS:NUM_EXT", sizeof(int), &extension_num);
  BnkDef("XIS:OUTFITS:FITS_EXT:PTR", sizeof(fitsExt));
  BnkPut("XIS:OUTFITS:FITS_EXT:PTR", sizeof(fitsExt), &fitsExt);

  fp = ( 0 < frame_ext_id ) ? fitsExt[frame_ext_id].fitsd : NULL;
  BnkDef("XIS:OUTFITS:FRAMES:PTR", sizeof(fp));
  BnkPut("XIS:OUTFITS:FRAMES:PTR", sizeof(fp), &fp);

  fp = ( 0 < exposure_ext_id ) ? fitsExt[exposure_ext_id].fitsd : NULL;
  BnkDef("XIS:OUTFITS:EXPOSURES:PTR", sizeof(fp));
  BnkPut("XIS:OUTFITS:EXPOSURES:PTR", sizeof(fp), &fp);
  if ( flag_expname_nos ) {
    BnkDef("XIS:OUTFITS:EXPOSURE:PTR", sizeof(fp));
    BnkPut("XIS:OUTFITS:EXPOSURE:PTR", sizeof(fp), &fp);
  }

  fp = ( 0 < event_ext_id ) ? fitsExt[event_ext_id].fitsd : NULL;
  BnkDef("XIS:OUTFITS:EVENTS:PTR", sizeof(fp));
  BnkPut("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &fp);

  fp = ( 0 < lost_event_ext_id ) ? fitsExt[lost_event_ext_id].fitsd : NULL;
  BnkDef("XIS:OUTFITS:LOSTAREAS:PTR", sizeof(fp));
  BnkPut("XIS:OUTFITS:LOSTAREAS:PTR", sizeof(fp), &fp);

  fp = ( 0 < gti_ext_id ) ? fitsExt[gti_ext_id].fitsd : NULL;
  BnkDef("XIS:OUTFITS:GTI:PTR", sizeof(fp));
  BnkPut("XIS:OUTFITS:GTI:PTR", sizeof(fp), &fp);

  if ( 0 < event_ext_id ) {
    istat = aefits_write_module_history(fitsExt[event_ext_id].fitsd, pname);
    if ( istat ) {
      goto quit;
    }

    BnkGet("XIS:FILENAME:PTR", sizeof(infile), &used, &infile);
    BnkGet("XIS:OUTFITS:FILENAME:PTR", sizeof(outfile), &used, &outfile);

    sprintf(history, "  infile='%s'", infile);
    fits_write_history(fitsExt[event_ext_id].fitsd, history, &istat);
    sprintf(history, "  outfile='%s'", outfile);
    fits_write_history(fitsExt[event_ext_id].fitsd, history, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
      goto quit;
    }
  }

  event_num = 1L;

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISeditEventFits_his(int *status)
{
  *status = ANL_OK;
}

void
XISeditEventFits_bgnrun(int *status)
{
  EvsfSetM("XISeditEventFits:BEGIN");
  *status = ANL_OK;
}

void
XISeditEventFits_ana(int nevent, int eventid, int *status)
{
  EvsfSetM ("XISeditEventFits:ENTRY");

  if ( 0 < event_ext_id ) {
  /* read event inf. from bank and write them to event fits */
    if ( bank2fits(fitsExt[event_ext_id].fitsd,
		   event_column_inf, event_column_num, event_num++,
		   event_value_max_size) != ANL_TRUE ) {
      anl_msg_error("\
%s: cannot bank2fits here\n", pname);
      *status = ANL_ERROR;
      return;
    }
  }

  EvsfSetM ("XISeditEventFits:OK");
  *status = ANL_OK;
}

void
XISeditEventFits_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISeditEventFits_exit(int *status)
{
  int istat = 0;

  if ( 0 < event_ext_id ) {
    fits_write_date(fitsExt[event_ext_id].fitsd, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_date() failed for extnum=%d\n", pname, event_ext_id);
      goto quit;
    }
    fits_write_chksum(fitsExt[event_ext_id].fitsd, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_write_chksum() failed for extnum=%d\n", pname, event_ext_id);
      goto quit;
    }
  }

  /* merge all extension */
  if ( fitsFileMerge(extension_num, fitsExt, outfile) != ANL_TRUE ) {
    goto quit;
  }

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
