/****************************************************************
  replace #include <> -> #include "" for local include files
  Wed Nov  3 22:49:55 1999 by E. Miyata

ver 3.6 2004.03.14 by H. Matsumoto
   AtTime --> AtTimeD
   aste2attime --> aste2attimeD

ver 3.7 2004.04.02 by H. Matsumoto
   delete unused extensions

ver 3.8 2005.6.7 H. Matsumoto
   BNK AEpacketRPTread:FILE_NAME の廃止と
   BNK ASTE:RPT:IFILE_NAME:PTR の導入に対応。
   getBnkForFrame に stime_of_exptime を追加。


ver 3.9 2005/6/11 H. Matsumoto
   BNK XIS:MODINF_EXPTIME に対応
   BNK XIS:MODINF_EXPTIME_AETIME に対応

ver 4.0 2005/6/14 Hironori Matsumoto
   構造体 XISmodeInf のフィールド readOutTime を exptime に変更
   構造体 XIS_STD_KEYS のフィールド名変更に対応

ver 4.1 2005/9/2 Hironori Matsumoto
   aetimeUtil.h のありかの変更に対応
   xisEventFitsUtil.h のありかをこのディレクトリに変更
   getEventFileName を
      RPT が ae20041215_1055_1932.rpt なら ae20041215_1055_1932_xis0_5x5.tff
             ae070412150_1.rpt        なら ae070412150xi0_1_5x5.tff
     というように変更
     frame mode の時、ファイル番号は 01, 02, ... となるようにする。

ver 4.2 2005/10/28 Hironori Matsumoto
   fitsFileMergeCloseDelete のマイナーチェンジ

ver 4.3 2005/10/31 Y.ISHISAKI
   #include "cfortran.h" を削除
   free allocated memory by tempnam() in getTempEventFileName()
   call aefits_write_module_history() in fitsFileMergeCloseDelete()

ver 4.4 2005/11/01 Y.ISHISAKI
   increase morekeys 2 -> 15 in fitsFileMergeCloseDelete()

ver 4.5 2005/11/05 M.Ozaki
   Checking the return value of rename(2) system call and printing
   error message in case of error.
   KNOWN BUG: rename fails when the temporaly file and the product file
   are in different device.  In such a case, the file must be copied
   from the source to the destination. (FIXME!!)

ver 4.6 2005/12/05 H.Nakajima
   Change the criteria in fitsFileMerge

ver 4.7 2006/08/24 Y.ISHISAKI
   basename() -> aefits_basename()
   remove extern int errno; in fitsFileMergeCloseDelete(), which is in errno.h
   use fits_write_key_fixdbl() in addMinMax()
   remove unused getCharID()
   BnkGet -> BnkfGetM

ver 4.8 2006/09/14 Hironori Matsumoto
   GTI extension name を STDGTI から GTI に変更。

ver 4.9 2007/01/30 Y.ISHISAKI
   add num_ext argument to fitsFileMerge()

ver 5.0 2007/02/14 Y.ISHISAKI
   bug fix fitsFileMerge(), hduid < num_ext

ver 5.1 2007/05/14 Y.ISHISAKI
   HOTPIXEL_EXTENSION_NAME, EXPOSURE_NOS_EXTENSION_NAME を追加。
   call fits_write_date(), fits_write_chksum() in fitsFileMerge()

*****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aetimeUtil.h"
#include "aeFitsHeaderUtil.h"
#include "xisEventFitsUtil.h"
#include "xisutility.h"
#include "xisNamingFunc.h"
#include "xisFitsHeaderUtil.h"

#if defined(__XISQL__) || defined(__XISREAD__)
#else
#include "xisDESim.h"
#endif

/* #define pname           "xisEventFitsUtil" */
static char pname[]="xisEventFitsUtil";
#define _FILENAME_MAX   1023
#define   SET_FRAME_TIME_FLAG 1
/* #define _PRINT_OUT_*/


/*
 * fitsclose
 *	fits file を close する
 */
int
fitsclose(fitsfile **fitsd)
{
  int fitsStatus=0;
  FITS_GET_MEMORY;
  FITS_CHECK_ERROR(fits_close_file (*fitsd, &fitsStatus));
  *fitsd = (fitsfile *)NULL;
  FITS_CHECK_ERROR(fitsStatus);
  return ANL_TRUE;
}


/*
 *  getBnkForPrimaryHeader
 *	primary header の bnkget を一手に引き受ける
 */
int
getBnkForPrimary(XIS_STD_KEYS *inf)
{
  int size=0;
#if defined(__XISQL__) || defined(__XISREAD__)
  BnkfGetM("XIS:FRAME_ST_AETIME",  sizeof(double), &size, &(inf->tstart));
  BnkfGetM("XIS:FRAME_END_AETIME", sizeof(double), &size, &(inf->tstop));
  /*  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &size, &(inf->tstart));
      printf ("%s: packet aetime = %.2f\n", pname, inf->tstart);
      inf->tstop = inf->tstart;*/
  strcpy (inf->tlm_file, "XISQL");
#else
  XISsimSetup  *simSetup;
  XISdeopeStatus *desimStatus;
  BnkfGetM("XIS:SIM_SETUP:PTR", sizeof(XISsimSetup *),
	 &size, &simSetup);
  BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
	 &size, &desimStatus);
  inf->tstart=desimStatus->tstart;
  inf->tstop=desimStatus->tstop;
  strcpy (inf->tlm_file, simSetup->fits_fname);
#endif

  return ANL_TRUE;
}

/*
 *  getTlmFileName
 */
int
getTlmFileName(XIS_STD_KEYS *inf)
{
#if defined(__XISQL__) || defined(__XISREAD__)
  int index;
  if ( ANL_OK == BnkKey("ASTE:RPT:IFILE_NAME:PTR", &index) ) {
    int used;
    char *rptname;
    BnkfGetM("ASTE:RPT:IFILE_NAME:PTR", sizeof(rptname), &used, &rptname);
    strcpy(inf->tlm_file, aefits_basename(rptname));
  } else
    strcpy (inf->tlm_file, "XISQL");
#else
  int size;
  XISsimSetup  *simSetup;
  BnkfGetM("XIS:SIM_SETUP:PTR", sizeof(XISsimSetup *), &size, &simSetup);
  strcpy (inf->tlm_file, simSetup->fits_fname);
#endif
  return ANL_TRUE;
}

/*
 *  setFileTstart
 *	set tstart for file
 */
int
setFileTstart(fileTtime *ftime)
{
  int size=0;
#if defined(__XISQL__) || defined(__XISREAD__)
  BnkfGetM("XIS:FRAME_ST_AETIME",  sizeof(double), &size, &(ftime->tstart));
  /*  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &size, &(ftime->tstart));*/
#else
  XISdeopeStatus *desimStatus;
  if (ftime->tstart > -1.1e99) return ANL_TRUE;
  BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
	 &size, &desimStatus);
  ftime->tstart=desimStatus->tstart;
#endif
  return ANL_TRUE;
}

/*
 *  setFileTstop
 *	set tstop for file
 */
int
setFileTstop(fileTtime *ftime)
{
  int size=0;
#if defined(__XISQL__) || defined(__XISREAD__)
  BnkfGetM("XIS:FRAME_END_AETIME",  sizeof(double), &size, &(ftime->tstop));
  /*  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &size, &(ftime->tstop));*/
#else
  XISdeopeStatus *desimStatus;
  BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
	 &size, &desimStatus);
  ftime->tstop=desimStatus->tstop;
#endif
  return ANL_TRUE;
}

/*
 *  getTstartTstop
 *	get tstart & tstop
 */
int
getFRAMETstartTstop(fileTtime *ftime)
{
  int size=0;
#if defined(__XISQL__) || defined(__XISREAD__)
  BnkfGetM("XIS:FRAME_ST_AETIME",  sizeof(double), &size, &(ftime->tstart));
  BnkfGetM("XIS:FRAME_END_AETIME", sizeof(double), &size, &(ftime->tstop));
  /*  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &size, &(ftime->tstart));
      ftime->tstop = ftime->tstart + FRAME_EXPOSURE_TIME/2;
      ftime->tstart -= FRAME_EXPOSURE_TIME/2;*/
#else
  XISdeopeStatus *desimStatus;
  BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
	 &size, &desimStatus);
  ftime->tstart=desimStatus->tstart;
  ftime->tstop=desimStatus->tstop;
#endif
  return ANL_TRUE;

}

int
setTstartTstop(XIS_STD_KEYS *stdkeys)
{
  int size=0;

#if defined(__XISQL__) || defined(__XISREAD__)
  double sttime, endtime;
  BnkfGetM("XIS:FRAME_ST_AETIME",  sizeof(double), &size, &(sttime));
  BnkfGetM("XIS:FRAME_END_AETIME",  sizeof(double), &size, &(endtime));
  /*  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &size, &aetime);*/
  if ((int) stdkeys->tstart) {
    if (stdkeys->tstart > sttime) stdkeys->tstart = sttime;
    if (stdkeys->tstop  < endtime) stdkeys->tstop = endtime;
  } else {
    stdkeys->tstart = sttime;
    stdkeys->tstop  = endtime;
  }
#else
  XISdeopeStatus *desimStatus;
  BnkfGetM("XIS:DEOPE_STATUS:PTR",sizeof(XISdeopeStatus *), &size, &desimStatus);

  /* not sure how this desimStatus works ? */
  if ((int) stdkeys->tstart) {
    if (stdkeys->tstart > desimStatus->tstart) stdkeys->tstart = desimStatus->tstart;
    if (stdkeys->tstop < desimStatus->tstop) stdkeys->tstop = desimStatus->tstop;
  } else {
    stdkeys->tstart = desimStatus->tstart;
    stdkeys->tstop = desimStatus->tstop;
  }

#endif
  return ANL_TRUE;
}

static int
getExpinfMajor(XISexpInfMajor *inf)
{
  int size;
  BnkfGetM("XIS:AREA_DISCRI_ENA",    sizeof(inf->areaDiscriEna),
         &size, inf->areaDiscriEna);
  BnkfGetM("XIS:GRADE_DISCRI_ENA",   sizeof(inf->gradeDiscriEna),
         &size, &inf->gradeDiscriEna);
  BnkfGetM("XIS:EVENT_BUF_ID",       sizeof(inf->eventBufferID),
         &size, inf->eventBufferID);
  BnkfGetM("XIS:PIXEL_BUF_ID",           sizeof(inf->pixelBufferID),
         &size, inf->pixelBufferID);
  BnkfGetM("XIS:HOT_P_OVF",           sizeof(inf->hotPOvf),
         &size, inf->hotPOvf);
  BnkfGetM("XIS:PPU_TIME_OUT",           sizeof(inf->timeOut),
         &size, inf->timeOut);
  BnkfGetM("XIS:EVENT_RAM_FULL",           sizeof(inf->evramFull),
         &size, inf->evramFull);
  BnkfGetM("XIS:OTHER_ERR",           sizeof(inf->otherErr),
         &size, inf->otherErr);
  BnkfGetM("XIS:A_CHK_ERR",           sizeof(inf->aChkErr),
         &size, inf->aChkErr);
  BnkfGetM("XIS:F_SYNC_ERR",           sizeof(inf->fSyncErr),
         &size, inf->fSyncErr);
  BnkfGetM("XIS:L_SYNC_ERR",           sizeof(inf->lSyncErr),
         &size, inf->lSyncErr);
  BnkfGetM("XIS:PCODE_PARITY_ERR",           sizeof(inf->pixCpErr),
         &size, inf->pixCpErr);
  BnkfGetM("XIS:INSIDE_EV_TH_PIXEL", sizeof(inf->insideEvThPixel),
         &size, inf->insideEvThPixel);
  BnkfGetM("XIS:EVENT_TOT_NO",       sizeof(inf->eventTotNo),
         &size, inf->eventTotNo);
  BnkfGetM("XIS:ABOVE_EV_TH_UPPER",  sizeof(inf->aboveEvThUpper),
         &size, inf->aboveEvThUpper);
  BnkfGetM("XIS:BIAS",               sizeof(inf->bias),
         &size, inf->bias);
  BnkfGetM("XIS:LAST_LINE",               sizeof(inf->lastLine),
         &size, inf->lastLine);
  BnkfGetM("XIS:LIGHT_LEAK",         sizeof(inf->lightLeak),
         &size, inf->lightLeak);
  return ANL_TRUE;
}

static int
getExpinfTiming(XISexpInfMajor *inf)
{
  int size;
  BnkfGetM("XIS:AREA_DISCRI_ENA",    sizeof(inf->areaDiscriEna),
         &size, inf->areaDiscriEna);
  BnkfGetM("XIS:GRADE_DISCRI_ENA",   sizeof(inf->gradeDiscriEna),
         &size, &inf->gradeDiscriEna);
  BnkfGetM("XIS:EVENT_BUF_ID",       sizeof(inf->eventBufferID),
         &size, inf->eventBufferID);
  BnkfGetM("XIS:PIXEL_BUF_ID",           sizeof(inf->pixelBufferID),
         &size, inf->pixelBufferID);
  BnkfGetM("XIS:HOT_P_OVF",           sizeof(inf->hotPOvf),
         &size, inf->hotPOvf);
  BnkfGetM("XIS:PPU_TIME_OUT",           sizeof(inf->timeOut),
         &size, inf->timeOut);
  BnkfGetM("XIS:EVENT_RAM_FULL",           sizeof(inf->evramFull),
         &size, inf->evramFull);
  BnkfGetM("XIS:OTHER_ERR",           sizeof(inf->otherErr),
         &size, inf->otherErr);
  BnkfGetM("XIS:A_CHK_ERR",           sizeof(inf->aChkErr),
         &size, inf->aChkErr);
  BnkfGetM("XIS:F_SYNC_ERR",           sizeof(inf->fSyncErr),
         &size, inf->fSyncErr);
  BnkfGetM("XIS:L_SYNC_ERR",           sizeof(inf->lSyncErr),
         &size, inf->lSyncErr);
  BnkfGetM("XIS:PCODE_PARITY_ERR",           sizeof(inf->pixCpErr),
         &size, inf->pixCpErr);
  BnkfGetM("XIS:INSIDE_EV_TH_PIXEL", sizeof(inf->insideEvThPixel),
         &size, inf->insideEvThPixel);
  BnkfGetM("XIS:EVENT_TOT_NO",       sizeof(inf->eventTotNo),
         &size, inf->eventTotNo);
  BnkfGetM("XIS:ABOVE_EV_TH_UPPER",  sizeof(inf->aboveEvThUpper),
         &size, inf->aboveEvThUpper);
  BnkfGetM("XIS:BIAS",               sizeof(inf->bias),
         &size, inf->bias);
  BnkfGetM("XIS:LAST_LINE",               sizeof(inf->lastLine),
         &size, inf->lastLine);
  return ANL_TRUE;
}

static int
getExpinfFrame(XISexpInfMajor *inf)
{
  int size;
  BnkfGetM("XIS:AREA_DISCRI_ENA",    sizeof(inf->areaDiscriEna),
         &size, inf->areaDiscriEna);
  BnkfGetM("XIS:GRADE_DISCRI_ENA",   sizeof(inf->gradeDiscriEna),
         &size, &inf->gradeDiscriEna);
  BnkfGetM("XIS:EVENT_BUF_ID",       sizeof(inf->eventBufferID),
         &size, inf->eventBufferID);
  BnkfGetM("XIS:PIXEL_BUF_ID",           sizeof(inf->pixelBufferID),
         &size, inf->pixelBufferID);
  BnkfGetM("XIS:HOT_P_OVF",           sizeof(inf->hotPOvf),
         &size, inf->hotPOvf);
  BnkfGetM("XIS:PPU_TIME_OUT",           sizeof(inf->timeOut),
         &size, inf->timeOut);
  BnkfGetM("XIS:EVENT_RAM_FULL",           sizeof(inf->evramFull),
         &size, inf->evramFull);
  BnkfGetM("XIS:OTHER_ERR",           sizeof(inf->otherErr),
         &size, inf->otherErr);
  BnkfGetM("XIS:A_CHK_ERR",           sizeof(inf->aChkErr),
         &size, inf->aChkErr);
  BnkfGetM("XIS:F_SYNC_ERR",           sizeof(inf->fSyncErr),
         &size, inf->fSyncErr);
  BnkfGetM("XIS:L_SYNC_ERR",           sizeof(inf->lSyncErr),
         &size, inf->lSyncErr);
  BnkfGetM("XIS:PCODE_PARITY_ERR",           sizeof(inf->pixCpErr),
         &size, inf->pixCpErr);
  return ANL_TRUE;
}

static int
getExpinfDark(XISexpInfDark *inf)
{
  int size;
  BnkfGetM("XIS:AREA_DISCRI_ENA",    sizeof(inf->areaDiscriEna),
         &size, inf->areaDiscriEna);
  BnkfGetM("XIS:GRADE_DISCRI_ENA",   sizeof(inf->gradeDiscriEna),
         &size, &inf->gradeDiscriEna);
  BnkfGetM("XIS:EVENT_BUF_ID",       sizeof(inf->eventBufferID),
         &size, inf->eventBufferID);
  BnkfGetM("XIS:PIXEL_BUF_ID",           sizeof(inf->pixelBufferID),
         &size, inf->pixelBufferID);
  BnkfGetM("XIS:HOT_P_OVF",           sizeof(inf->hotPOvf),
         &size, inf->hotPOvf);
  BnkfGetM("XIS:PPU_TIME_OUT",           sizeof(inf->timeOut),
         &size, inf->timeOut);
  BnkfGetM("XIS:EVENT_RAM_FULL",           sizeof(inf->evramFull),
         &size, inf->evramFull);
  BnkfGetM("XIS:OTHER_ERR",           sizeof(inf->otherErr),
         &size, inf->otherErr);
  BnkfGetM("XIS:A_CHK_ERR",           sizeof(inf->aChkErr),
         &size, inf->aChkErr);
  BnkfGetM("XIS:F_SYNC_ERR",           sizeof(inf->fSyncErr),
         &size, inf->fSyncErr);
  BnkfGetM("XIS:L_SYNC_ERR",           sizeof(inf->lSyncErr),
         &size, inf->lSyncErr);
  BnkfGetM("XIS:PCODE_PARITY_ERR",           sizeof(inf->pixCpErr),
         &size, inf->pixCpErr);
  /* #if defined(__XISQL__) || defined(__XISREAD__)
     #else
     BnkfGetM("XIS:DARK_UPDATE_SEQ_NUM", sizeof(inf->updateSeqNo),
     #endif
     removed by K.Hayashida 99/10/27 */
  BnkfGetM("XIS:DARK_UPDATE_SEQ_NO", sizeof(inf->updateSeqNo),
         &size, inf->updateSeqNo);
  BnkfGetM("XIS:DARK_REJECT_PIX",    sizeof(inf->rejectPixel),
         &size, inf->rejectPixel);
  BnkfGetM("XIS:DARK_REPLACE_PIX",   sizeof(inf->replacePixel),
         &size, inf->replacePixel);
  BnkfGetM("XIS:DARK_HOTPIX",        sizeof(inf->hotPixel),
         &size, inf->hotPixel);
  return ANL_TRUE;
}

/*
 *  getBnkForFrame
 *	フレーム情報の BnkfGetM を一手に引き受ける
 */
int
getBnkForFrame(XIS_FRAME_INF *xisinf)
{
  int size=0;
#if defined(__XISQL__) || defined(__XISREAD__)
  BnkfGetM("XIS:LINE_SEQ_NO", sizeof(xisinf->modeinf.lineSeqNo),
         &size, &xisinf->modeinf.lineSeqNo);
  BnkfGetM("XIS:FRAME_ST_AETIME",    sizeof(xisinf->framesttime),
         &size, &xisinf->framesttime);
  BnkfGetM("XIS:FRAME_END_AETIME",    sizeof(xisinf->frameendtime),
         &size, &xisinf->frameendtime);
#else
  XISdeopeStatus *desimStatus;
  BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
         &size, &desimStatus);
  xisinf->framesttime  = desimStatus->tstart;
  xisinf->frameendtime = desimStatus->tstop;
  xisinf->modeinf.lineSeqNo = desimStatus->lineNum;
#endif
  /* get expinf for all edit mode */
  BnkfGetM("XIS:EDIT_MODE",          sizeof(xisinf->modeinf.editMode),
         &size, &xisinf->modeinf.editMode);
  BnkfGetM("XIS:CLOCK_MODE",         sizeof(xisinf->modeinf.clockMode),
         &size, &xisinf->modeinf.clockMode);
  BnkfGetM("XIS:WINDOW_OPTION",      sizeof(xisinf->modeinf.windowOption),
         &size, &xisinf->modeinf.windowOption);
  BnkfGetM("XIS:LINE_SEQ_NO",      sizeof(xisinf->modeinf.lineSeqNo),
         &size, &xisinf->modeinf.lineSeqNo);
  BnkfGetM("XIS:PIXBUF_FLIPTIME",      sizeof(xisinf->modeinf.pixBufFlipTime),
         &size, &xisinf->modeinf.pixBufFlipTime);

  BnkfGetM("XIS:MODINF_EXPTIME",    sizeof(xisinf->modeinf.exptime),
         &size, &xisinf->modeinf.exptime);

  BnkfGetM("XIS:STIME_OF_CCSDSPACKET",    sizeof(xisinf->stime_of_ccsdspacket),
         &size, &xisinf->stime_of_ccsdspacket);

  BnkfGetM("XIS:MODINF_EXPTIME_AETIME", sizeof(xisinf->modeinf.exptime_aetime),
         &size, &xisinf->modeinf.exptime_aetime);

  /* get misc */
  xisinf->aeDate = (short int) aetime2aedate (xisinf->modeinf.exptime_aetime);

  /* get modeinf for each edit mode */
  switch (xisinf->modeinf.editMode) {
  case XISedit5x5:
  case XISedit3x3:
  case XISedit2x2:
    getExpinfMajor (&(xisinf->expinf));
    break;
  case XISeditTiming:
    getExpinfTiming (&(xisinf->expinf));
    break;
  case XISeditFrame:
    getExpinfFrame (&(xisinf->expinf));
    break;
  case XISeditDarkUpdate:
  case XISeditDarkInit:
    getExpinfDark (&(xisinf->expinfDark));
    break;
  default:
    break;
  }
  return ANL_TRUE;
}

int
getWindowNum(int window_option)
{
  switch (window_option) {
  case XISwindow4:
    return 4;
  case XISwindow8:
    return 8;
  case XISwindow16:
    return 16;
  case XISwindowOff:
    return 1;
  default:
    fprintf (stderr, "%s: unknown window option=%d\n", pname, window_option);
    return -1;
  }
}

/*
 *  getBnkForExposure
 *	exposure の bnkget を一手に引き受ける
 */
int
getBnkForExposure(XIS_FRAME_INF xis_frame_inf,
		  XIS_EXPOSURE_INF *xis_exposure_inf)
{
  int total_num, rawy;
#if defined(__XISQL__) || defined(__XISREAD__)
  xisFrameTimes ft;
  double expsttime;
#endif

  total_num = getWindowNum(xis_frame_inf.modeinf.windowOption);
  rawy = XISactiveSegmentVsize / total_num * xis_exposure_inf->expSeqNo
    + XISactiveSegmentVsize / total_num / 2;

#if defined(__XISQL__) || defined(__XISREAD__)
  ft.framesttime  = xis_frame_inf.framesttime;
  ft.frameendtime = xis_frame_inf.frameendtime;
  /*  ft.first_frame_flag = SET_FRAME_TIME_FLAG;*/
  xisSetPramInfo(xis_frame_inf.modeinf.clockMode,
		 (int)NULL,	/* burst mode exposure time included in HK data */
		 &ft);
  xisGetExpStTime (xis_frame_inf.modeinf.clockMode,
		   xis_frame_inf.modeinf.windowOption,
		   rawy,
		   &ft,
		   &expsttime,
		   &xis_exposure_inf->expTime);
  xis_exposure_inf->expCentTime
    = xisGetExpCentTime(expsttime,xis_exposure_inf->expTime);
#ifdef _PRINT_OUT_
  printf ("seq=%d y=%d framest=%.2f expsttime=%.2f exptime=%.2f\n",
	  xis_exposure_inf->expSeqNo, rawy, xis_frame_inf.framesttime, expsttime,
	  xis_exposure_inf->expCentTime);
#endif
#else
  {
    int size;
    XISdeopeStatus *desimStatus;
    BnkfGetM("XIS:DEOPE_STATUS:PTR", sizeof(XISdeopeStatus *),
	   &size, &desimStatus);
    xis_exposure_inf->expCentTime = (desimStatus->tstart + desimStatus->tstop) / 2.0;
    xis_exposure_inf->expTime = desimStatus->tstop - desimStatus->tstart;
  }
#endif
  return ANL_TRUE;
}

/*
 * getExtensionName
 *	extension の数字をもらって名前を返す
 */
char *
getExtensionName(int extnum)
{
  switch (extnum) {
  case PRIMARY_HDU_ID:
    return "PRIMARY";
  case FRAME_EXTENSION_ID:
    return FRAME_EXTENSION_NAME;
  case EXPOSURE_EXTENSION_ID:
    return EXPOSURE_EXTENSION_NAME;
  case EVENT_EXTENSION_ID:
    return EVENT_EXTENSION_NAME;
  case LOST_EVENT_EXTENSION_ID:
    return LOST_EVENT_EXTENSION_NAME;
  case GTI_EXTENSION_ID:
    return GTI_EXTENSION_NAME;
  }
  return NULL;
}


/*
 * get_rpt_name
 *	bnk get rpt filename
 *	remove suffix
 */
int
get_rpt_name(char *rpt)
{
  char ss[_FILENAME_MAX], *s;
  int size;
  char *rptname;

  BnkfGetM ("ASTE:RPT:IFILE_NAME:PTR", sizeof(rptname), &size, &rptname);
  strcpy(ss, rptname);
  ss[strlen(ss)-4] = '\0';	/* "---.rpt" の "." をとってしまう */
  s = strrchr (ss, '/');
  if (s == NULL)
    strcpy (rpt, ss);
  else
    strcpy (rpt, ++s);

  return ANL_TRUE;
}



/**********************************************************************
 * getEventFileName
 *	時間、センサー、モードをつかって正式なイベントファイル名を返す

 2005/9/3  Hironori Matsumoto
      RPT が ae20041215_1055_1932.rpt なら ae20041215_1055_1932_xis0_5x5.tff
             ae070412150_1.rpt        なら ae070412150xi0_1_5x5.tff
      とする。
      Frame mode の時の番号付けを 000, 001, ... から 01, 02,... へ
*************************************************************************/

#ifdef _HPK_
int
getEventFileName(int sensor, int mode, fileTtime ftime, char *filename)
{
  sprintf (filename, "HPK%d_%s.fff",
	   sensor, getEditModeName(mode));
  return ANL_TRUE;
}
#else
int
getEventFileName(int sensor, XIS_FRAME_INF frame_inf, fileTtime ftime,
		 char *rpt_name, char *filename)
{
#if defined(__XISQL__) || defined(__XISREAD__)

  int i; /* ループ用 */
  int num_us ; /* rpt_name に含まれる "_" の数 */
  int rpt_name_length; /* rpt_name の字数 */

  /* rpt 名のタイプ。
     type = 0; ae20041215_1055_1932.rpt
     type = 1; ae070412150_1.rpt */
  int type;

  /* rpt_name には RPT 名から ".rpt" を引いたものが入る。例 ae070504070_0 */
  char corename[FILENAME_MAX]; /* "_" より前の部分。上の例では "ae070504070" */
  char rpt_num[FILENAME_MAX];  /* "_" より後の部分。上の例では "0" */
  char *moji; /* "_" の位置を探し出すため */

  /* darkfarme mode の seqence number
   static にして、再初期化されないようにする。*/
  static int darkframe_seq = 0;

  /* rpt_name が ae20041215_1055_1932 タイプか、ae070412150_1 タイプかを判定。
   rpt_name に含まれる "_" の数から判断 */
  num_us = 0;
  rpt_name_length = strlen(rpt_name);
  for (i=0; i<rpt_name_length; i++) {
    if (rpt_name[i] == '_') {
      num_us++;
    }
  }
  if (num_us == 1) {
    type = 1;
  }
  else {
    type = 0;
  }


  /* type 0 なら ae20041215_1055_1932_xis0
     type 1 なら ae070412150xi0_1
     という文字列を作り、filename に格納 */

  switch (type) {

  /* type 0 の場合: すなわち ae20041215_1055_1932.rpt タイプ */
  case 0:

    sprintf(filename, "%s_xis%01d", rpt_name, sensor);

    break;

  /* type 1 の場合: すなわち ae070412150_1.rpt タイプ */
  case 1:
  default:

    /* rpt の一番右側の "_" を探し出す */
    moji = strrchr(rpt_name, '_');

    /* corename を作る */
    if (moji == NULL) {
      strcpy(corename, rpt_name);
    }
    else {
      strncpy(corename, rpt_name, (moji-rpt_name));
      *(corename + (moji - rpt_name)) = '\0'; /* strncpy はヌル文字を加えてくれないことに注意 */
    }

    /*rpt_num を作る */
    if (moji == NULL) {
      strcpy(rpt_num, "NULL");
    }
    else {
      strcpy(rpt_num, moji+1);
    }

    /* filename に {corename}xi[0-4]_{rpt_num} といれる */
    sprintf (filename, "%sxi%01d_%s", corename, sensor, rpt_num);

    break;
  }

  /* 5x5 とか 3x3 とか frame とか darkframe という文字をくっつける */
    sprintf(filename, "%s_%s", filename, getEditModeName(frame_inf.modeinf.editMode));

  /* darkframe mode 以外で P-sum, Burst のときは、
     psum とか burst という文字を加える */
  if ((frame_inf.modeinf.editMode != XISeditDarkFrame) && (frame_inf.modeinf.clockMode != XISclockNormal)) {
    sprintf (filename, "%s_%s", filename,
	     getClockModeName (frame_inf.modeinf.clockMode));
  }

  /* frame mode のときは、seqence 番号をつける */
  if (frame_inf.modeinf.editMode == XISeditFrame) {
    sprintf (filename, "%s%02d", filename, frame_inf.seq_num + 1);
  }

  /* darkframe mode のときは、sequence 番号を frame_inf.seq_num
     を利用しないで、darkframe_seq を用いてつける。
     理由は frame_inf.seq_num が、darkeframe と darkframe_burst や
     darkframe_psum を区別しているかもしれないから。*/
  if (frame_inf.modeinf.editMode == XISeditDarkFrame) {
    darkframe_seq ++;
    sprintf (filename, "%s%02d", filename, darkframe_seq);
  }


  strcat (filename, ".tff");

#elif defined(__XISREAD__)
  sprintf (filename, "%s_xis%d_%s", rpt_name,
	   sensor, getEditModeName(frame_inf.modeinf.editMode));
  if (frame_inf.modeinf.clockMode != XISclockNormal)
    sprintf (filename, "%s_%s", filename,
	     getClockModeName (frame_inf.modeinf.clockMode));
  if (frame_inf.modeinf.editMode == XISeditFrame
      || frame_inf.modeinf.editMode == XISeditDarkFrame)
    sprintf (filename, "%s%03d", filename, frame_inf.seq_num);

  strcat (filename, "__.fff");
#else
  AtTimeD atstart, atstop;
  aste2attimeD (ftime.tstart, &atstart);
  aste2attimeD (ftime.tstop, &atstop);
  sprintf (filename, "%4d%02d%02d_%02d%02d-%02d%02d_xis%d_%s.fff",
	   atstart.yr, atstart.mo,
           atstart.dy, atstart.hr, atstart.mn,
           atstop.hr, atstop.mn,
	   sensor, getEditModeName(frame_inf.modeinf.editMode));
#endif
  return ANL_TRUE;
}
#endif

/*
 * getTempEventFileName
 *	センサー、モード、extension num をつかってテンポラリーファイル名を返す
 */
int
getTempEventFileName(int sensor, XIS_FRAME_INF xisFrameInf,
		     int extnum, char *filename)
{
  char *p;
  p = tempnam("./", NULL);
  sprintf (filename, "%s-xis%d%s_%s_%s.evt", (NULL == p) ? "file" : p,
	   sensor, getEditModeName(xisFrameInf.modeinf.editMode),
	   getClockModeName(xisFrameInf.modeinf.clockMode),
	   getExtensionName(extnum));
  if ( NULL != p ) free(p);
  return ANL_TRUE;
}

int
readOutTime2readOutTimeH(unsigned int readOutTime)
{
  int readOutTimeH;
  readOutTimeH = (int) ( readOutTime >> 16 );
  return readOutTimeH;
}

int
readOutTime2readOutTimeL(unsigned int readOutTime)
{
  int readOutTimeL;
  readOutTimeL = (int) ( readOutTime & 0xFFFF );
  return readOutTimeL;
}

/*
 * addMinMax
 *	TLMIN/TLMAX を追加する
 */
int
addMinMax(FITS_EXT_STRUCT ext_struct)
{
  int fitsStatus=0, inum, splitNum;
  int vectorNum, hdutype;
  char form, tlmin[10], tlmax[10];

  FITS_GET_MEMORY;

  if (ext_struct.fitsd == (fitsfile *)NULL) {
    if (fits_open_file (&ext_struct.fitsd, ext_struct.filename, READWRITE, &fitsStatus)) {
      fprintf(stderr, "%s: Error opening %s (status=%d)\n",
	      pname,ext_struct.filename,fitsStatus);
      fflush(stderr);
      return ANL_FALSE;
    }
  }

  if (fits_movabs_hdu(ext_struct.fitsd, 2, &hdutype, &fitsStatus)) {
    fprintf(stderr, "%s: Error in fits_movabs_hdu in %s (status=%d)\n",
	    pname,ext_struct.filename,fitsStatus);
    fflush(stderr);
    return ANL_FALSE;
  }

  for (inum=0; inum<ext_struct.tfields; inum++) {
    if (ext_struct.tlmin[inum] < _TL_IGNORE
	&& ext_struct.tlmax[inum] < _TL_IGNORE)
      continue;

    splitNum = sscanf (ext_struct.tform[inum], "%d%c", &vectorNum, &form);
    if (splitNum != 2) {
      fprintf (stderr, "%s: Unknown tform=%s\n",pname,ext_struct.ttype[inum]);
      return ANL_FALSE;
    }
    sprintf (tlmin, "TLMIN%d", inum+1);
    sprintf (tlmax, "TLMAX%d", inum+1);

    switch (form) {
    case 'X':
    case 'B':
      break;
    default:
      fits_write_key_fixdbl(ext_struct.fitsd, tlmin, ext_struct.tlmin[inum], 0,
			"Minimum range", &fitsStatus);
      fits_write_key_fixdbl(ext_struct.fitsd, tlmax, ext_struct.tlmax[inum], 0,
			"Maximum range", &fitsStatus);
    }
  }

  if (fitsclose (&ext_struct.fitsd) != ANL_TRUE) return ANL_FALSE;
  FITS_CHECK_ERROR(fitsStatus);
  return ANL_TRUE;
}

/*
 * createExtension
 *	extension を一つ持った fitsfile を作る
 *	イメージ部分は空とする
 */
int
createExtension(FITS_EXT_STRUCT *ext_struct, XIS_STD_KEYS stdkeys)
{
  int fitsStatus=0;
  FITS_GET_MEMORY;

  /* Make the file */
  fits_create_file(&(ext_struct->fitsd), ext_struct->filename, &fitsStatus);
  if (fitsStatus != 0) {
    fitsStatus=0;
    remove (ext_struct->filename);
    FITS_CHECK_ERROR(fits_create_file(&(ext_struct->fitsd),
                                      ext_struct->filename, &fitsStatus));
  }

  /* Make the primary array */ {
    int bitpix = 8;
    int naxis = 0;
    long naxes[] = {0};
    fits_create_img(ext_struct->fitsd, bitpix, naxis, naxes, &fitsStatus);
  }

  /* Make the 1-st extension */
  FITS_CHECK_ERROR(fits_create_hdu(ext_struct->fitsd, &fitsStatus));

  /* write the required header parameters for the bin table */
  FITS_CHECK_ERROR(fits_write_btblhdr(ext_struct->fitsd, 0,
				      ext_struct->tfields, ext_struct->ttype,
				      ext_struct->tform, ext_struct->tunit,
				      ext_struct->extname, 0, &fitsStatus));

  if (writeXISStdKeys (ext_struct, stdkeys, 0, &fitsStatus) != ANL_TRUE) {
    fprintf(stderr,"%s: writeEvtStdKeys() failed (status=%d)\n",pname,fitsStatus);
    fflush(stderr);
    return ANL_FALSE;
  }

  /* close fits file due to limit the file number=20 */
  if (fitsclose (&ext_struct->fitsd) != ANL_TRUE) return ANL_FALSE;

  return ANL_TRUE;
}

int
createExtensionOpen(FITS_EXT_STRUCT *ext_struct)
{
  int fitsStatus=0;
  FITS_GET_MEMORY;

  /* Make the file */
  fits_create_file(&(ext_struct->fitsd), ext_struct->filename, &fitsStatus);
  if (fitsStatus != 0) {
    fitsStatus=0;
    remove (ext_struct->filename);
    FITS_CHECK_ERROR(fits_create_file(&(ext_struct->fitsd),
                                      ext_struct->filename, &fitsStatus));
  }

  /* Make the primary array */ {
    int bitpix = 8;
    int naxis = 0;
    long naxes[] = {0};
    fits_create_img(ext_struct->fitsd, bitpix, naxis, naxes, &fitsStatus);
  }

  /* Make the 1-st extension */
  FITS_CHECK_ERROR(fits_create_hdu(ext_struct->fitsd, &fitsStatus));

  return ANL_TRUE;
}

int
createPrimary(FITS_EXT_STRUCT *ext_struct)
{
  int fitsStatus=0;
  FITS_GET_MEMORY;

  /* Make the file */
  fits_create_file(&(ext_struct->fitsd), ext_struct->filename, &fitsStatus);
  if (fitsStatus != 0) {
    fitsStatus=0;
    remove (ext_struct->filename);
    FITS_CHECK_ERROR(fits_create_file(&(ext_struct->fitsd),
                                      ext_struct->filename, &fitsStatus));
  }

  return ANL_TRUE;
}

/*
 * fitsFileMergeCloseDelete
 *	複数のファイルに分かれた extension を primary extension
 *	に結合する。出力ファイル名引数で与える。
 */
int
fitsFileMergeCloseDelete(FITS_EXT_STRUCT ext[], char newname[])
{
  int hdutype=0, morekeys=15;
  int fitsStatus=0, hduid=FRAME_EXTENSION_ID;

  /* for deleting unused extensions */
  char editmode_value[FLEN_CARD];
  char editmode_comment[FLEN_CARD];

  FITS_GET_MEMORY;


  /* in the case of event number = 0 */
  if (ext[FRAME_EXTENSION_ID].rowNumber <= 0) {
    for (hduid=0; hduid<EXTENSION_NUM; hduid++) {

      /* delete file if exists */
      if (ext[hduid].fitsd != (fitsfile *)NULL) {
	FITS_CHECK_ERROR(fits_delete_file (ext[hduid].fitsd, &fitsStatus));
      } else {
	remove (ext[hduid].filename);
      }
    }
  } else {

    /* open primary unit file */
    if (ext[PRIMARY_HDU_ID].fitsd == (fitsfile *)NULL)
      fits_open_file (&(ext[PRIMARY_HDU_ID].fitsd),
		      ext[PRIMARY_HDU_ID].filename, READWRITE, &fitsStatus);
    else
      fits_flush_file (ext[PRIMARY_HDU_ID].fitsd, &fitsStatus);

    /* open extension files if need */
    for (hduid=FRAME_EXTENSION_ID; hduid<=GTI_EXTENSION_ID; hduid++) {
      if (ext[hduid].fitsd == (fitsfile *)NULL) {
	fits_open_file (&ext[hduid].fitsd, ext[hduid].filename,
			READWRITE, &fitsStatus);
	fits_movabs_hdu (ext[hduid].fitsd, 2, &hdutype, &fitsStatus);
      } else {
	fits_flush_file (ext[hduid].fitsd, &fitsStatus);
      }
    }

    /* write checksum and date of primary header*/
    if (fits_write_date(ext[PRIMARY_HDU_ID].fitsd, &fitsStatus)){
      fprintf(stderr, "%s:  fits_write_date failed (%d)\n", pname, fitsStatus);
      fitsStatus = 0;
    }
    if (fits_write_chksum(ext[PRIMARY_HDU_ID].fitsd, &fitsStatus)) {
      fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, fitsStatus);
      fitsStatus = 0;
    }

    /* append all extension into primary header */
    for (hduid=FRAME_EXTENSION_ID; hduid<=GTI_EXTENSION_ID; hduid++) {
      fits_create_hdu (ext[PRIMARY_HDU_ID].fitsd, &fitsStatus);

      FITS_CHECK_ERROR(fits_copy_hdu (ext[hduid].fitsd,
				      ext[PRIMARY_HDU_ID].fitsd,
				      morekeys, &fitsStatus));
      FITS_CHECK_ERROR(fits_write_date(ext[PRIMARY_HDU_ID].fitsd, &fitsStatus));
      if (fits_write_chksum(ext[PRIMARY_HDU_ID].fitsd, &fitsStatus)) {
	fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, fitsStatus);
	fitsStatus = 0;
      }

      FITS_CHECK_ERROR( aefits_write_module_history(ext[PRIMARY_HDU_ID].fitsd, pname) );
    }

    /* remove unused extensions */
    fits_read_key(ext[PRIMARY_HDU_ID].fitsd, TSTRING, "EDITMODE",
		  &editmode_value, editmode_comment, &fitsStatus);
    printf("fits_read_key: EDIT MODE %s\n", editmode_value);

    /* if the file is darkframe, delete EXPOSURE, EVENTS, STDGTI */

    if (strcmp(editmode_value, "darkframe")==0) {
      fits_movnam_hdu(ext[PRIMARY_HDU_ID].fitsd, ANY_HDU, EXPOSURE_EXTENSION_NAME, 0, &fitsStatus);
      fits_delete_hdu(ext[PRIMARY_HDU_ID].fitsd, &hdutype, &fitsStatus);
      fits_movnam_hdu(ext[PRIMARY_HDU_ID].fitsd, ANY_HDU, EVENT_EXTENSION_NAME, 0, &fitsStatus);
      fits_delete_hdu(ext[PRIMARY_HDU_ID].fitsd, &hdutype, &fitsStatus);
      fits_movnam_hdu(ext[PRIMARY_HDU_ID].fitsd, ANY_HDU, GTI_EXTENSION_NAME, 0, &fitsStatus);
      fits_delete_hdu(ext[PRIMARY_HDU_ID].fitsd, &hdutype, &fitsStatus);
    }

    /* if the file is frame, delete EVENTS */

    if (strcmp(editmode_value, "frame")==0) {
      fits_movnam_hdu(ext[PRIMARY_HDU_ID].fitsd, ANY_HDU, "EVENTS", 0, &fitsStatus);
      fits_delete_hdu(ext[PRIMARY_HDU_ID].fitsd, &hdutype, &fitsStatus);
    }

    /* close file */
    fitsclose (&ext[PRIMARY_HDU_ID].fitsd);

    /* rename file name */
    printf ("%s: rename %s -> %s",pname,ext[PRIMARY_HDU_ID].filename,newname);
    if ( rename(ext[PRIMARY_HDU_ID].filename, newname) == 0 ){
      printf ("\n");  /* success */
    } else {
      printf (": FAILED: %s\nThe file '%s' remains existing as is.\n",
	strerror(errno), ext[PRIMARY_HDU_ID].filename);
    }

    for (hduid=1; hduid<EXTENSION_NUM; hduid++) {
      if (hduid != PRIMARY_HDU_ID){  /* just a sanity check */
        fits_delete_file (ext[hduid].fitsd, &fitsStatus);
      }
    }

  }

  return ANL_TRUE;
}

int
fitsFileMerge(int num_ext, FITS_EXT_STRUCT ext[], char newname[])
{
  int morekeys=0;
  int fitsStatus=0, hduid;

  FITS_GET_MEMORY;

  fits_write_date(ext[0].fitsd, &fitsStatus);
  fits_write_chksum(ext[0].fitsd, &fitsStatus);
  FITS_CHECK_ERROR( fitsStatus );

  /* append all extension into primary header */
  for (hduid=1; hduid < num_ext; hduid++) {
    if ( NULL != ext[hduid].fitsd ) {
      fits_flush_file(ext[hduid].fitsd, &fitsStatus);
      fits_create_hdu(ext[0].fitsd, &fitsStatus);
      fits_copy_hdu(ext[hduid].fitsd, ext[0].fitsd, morekeys, &fitsStatus);
      fits_write_date(ext[0].fitsd, &fitsStatus);
      fits_write_chksum(ext[0].fitsd, &fitsStatus);
      FITS_CHECK_ERROR( fitsStatus );
    }
  }

  /* close file */
  fitsclose(&ext[0].fitsd);
  /* rename file name */
  printf("%s: rename %s -> %s", pname, ext[0].filename, newname);
  if ( rename(ext[0].filename, newname) == 0 ) {
    printf("\n");  /* success */
  } else {
    printf(": FAILED: %s\nThe file '%s' remains existing as is.\n",
	strerror(errno), ext[0].filename);
  }

  for (hduid=1; hduid < num_ext; hduid++) {
    fits_delete_file(ext[hduid].fitsd, &fitsStatus);
    FITS_CHECK_ERROR( fitsStatus );
  }

  return ANL_TRUE;
}

/*
 * updateRowNumber
 *	fitsd の first extension に NEVENTS の数字を書き込む
 */
int
updateRowNumber(FITS_EXT_STRUCT *ext, int hdunum)
{
  int hdutype=0;
  int fitsStatus=0;
  char comment[] = "&";
  FITS_GET_MEMORY;

  if (ext->fitsd == (fitsfile *)NULL)
    FITS_CHECK_ERROR(fits_open_file (&ext->fitsd, ext->filename, READWRITE, &fitsStatus));

  if (hdunum != 0)
    FITS_CHECK_ERROR(fits_movabs_hdu(ext->fitsd, hdunum, &hdutype, &fitsStatus));

  FITS_CHECK_ERROR(fits_update_key_lng(ext->fitsd, "NAXIS2", ext->rowNumber, comment, &fitsStatus));

  if (hdunum == 1)
    FITS_CHECK_ERROR(fits_update_key_lng(ext->fitsd, "NEVENTS", ext->rowNumber, "Number of events", &fitsStatus));

  if (fitsclose (&ext->fitsd) != ANL_TRUE) return ANL_FALSE;

  return ANL_TRUE;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
