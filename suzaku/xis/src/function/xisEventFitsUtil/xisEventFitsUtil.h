/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:55 1999 by E. Miyata*/

/**************************************************************

  ver 3.8 2005/06/06 Hironori Matsumoto
	XIS_FRAME_INF に stime_of_exptime のフィールドを追加

  ver 3.9 2005/06/14 Hironori Matsumoto
	XIS_FRAME_INF の stime_of_exptime を stime_of_ccsdspacketに変更

  ver.4.1 2005/9/4
	EXPOSURE_EXTENSION_NAME を "EXPOSURES" に変更。

  ver 4.7 2006/08/24 Y.ISHISAKI

  ver 4.8 2006/09/14 Hironori Matsumoto
	GTI extension name を STDGTI から GTI に変更。

  ver 5.1 2007/05/14 Y.ISHISAKI
	HOTPIXEL_EXTENSION_NAME, EXPOSURE_NOS_EXTENSION_NAME を追加。
****************************************************************/


#ifndef _XIS_EVENT_FITS_
#define _XIS_EVENT_FITS_

/* XISmkEventFits ver0.9 まで */
/* #define XIS_FITS_VERSION		"0.2"*/
/* XISmkEventFits ver1.0 まで */
/* #define XIS_FITS_VERSION		"0.3"*/
/* ver0.4: fix exposure extension, fix tlmin/tlmax */
/* #define XIS_FITS_VERSION		"0.4"*/
/* ver1: 1999-04-30 version */
/* #define XIS_FITS_VERSION		"1"*/
/* ver2: 1999-08-24 version (include Ping-san's comments) */
#define XIS_FITS_VERSION		"2"

#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisEventLostList.h"
#include "xisFitsHeaderUtil.h"

/* frame duration time: 8 sec */
#define FRAME_EXPOSURE_TIME             8

#define EXTENSION_NUM			6
#define PRIMARY_HDU_ID			0
#define FRAME_EXTENSION_ID		1
#define EXPOSURE_EXTENSION_ID		2
#define EVENT_EXTENSION_ID		3
#define LOST_EVENT_EXTENSION_ID		4
#define GTI_EXTENSION_ID		5

#define FRAME_EXTENSION_NAME		"FRAMES"
#define EXPOSURE_EXTENSION_NAME		"EXPOSURES"
#define EVENT_EXTENSION_NAME		"EVENTS"
#define LOST_EVENT_EXTENSION_NAME	"LOSTAREAS"
#define GTI_EXTENSION_NAME		"GTI"
#define STDGTI_EXTENSION_NAME		"STDGTI"	/* obsolete */
#define HOTPIXEL_EXTENSION_NAME  	"HOTPIXELS"
/* for backward compatibility to the file with EXPOSURE extension */
#define EXPOSURE_NOS_EXTENSION_NAME	"EXPOSURE"

/* if tlmin and tlmax below TL_IGNORE, it will not included in keywords */
#define	TL_IGNORE			-2.0e-10
#define	_TL_IGNORE			-1.0e-10

#define	DOUBLE_DECIMAL			14


typedef struct {
  XISmodeInf      modeinf;	/* in xisTelemFormat.h */
  XISexpInfMajor  expinf;	/* Major / Timing / Frame; in xisTelemFormat.h */
  XISexpInfDark   expinfDark;	/* DarkInit / DarkUpdate; in xisTelemFormat.h */
  short int    aeDate;		/* calculated */
  double framesttime;		/* calculated based on FJT function */
  double frameendtime;		/* calculated based on FJT function */
  int seq_num;			/* sequence number for frame mode */

  /* modeinf.readOutTime はテレメトリーの MODEINF EXPTIME のことだが、
     これを aetime に変換する時には、その CCSDSパケットについていた S_TIME
     が必要。*/
  double stime_of_ccsdspacket;

} XIS_FRAME_INF;

typedef struct {
  int expSeqNo;
  double expCentTime;
  double expTime;
} XIS_EXPOSURE_INF;

typedef struct {
  double tstart;		/* tstart for event fits file */
  double tstop;			/* tstop for event fits file */
} fileTtime;

typedef struct {
  double start;
  double stop;
  double ontime;
} GTI_STRUCT;

#define	GTI_UNDEF			(-1.0e30)
#define	GTI_CHECK			(-1.0e29)

/********************** FUNCTIONS **********************/
int fitsclose(fitsfile **fitsd);
int fitsFileMergeCloseDelete(FITS_EXT_STRUCT ext[], char newname[]);
int fitsFileMerge(int num_ext, FITS_EXT_STRUCT ext[], char newname[]);
int createPrimary(FITS_EXT_STRUCT *ext_struct);
int createExtension(FITS_EXT_STRUCT *ext_struct, XIS_STD_KEYS);
int createExtensionOpen(FITS_EXT_STRUCT *ext_struct);
int getEventFileName(int sensor, XIS_FRAME_INF inf, fileTtime ftime,
		     char *rpt_name, char *filename);
int getTempEventFileName(int sensor, XIS_FRAME_INF xisFrameInf,
			 int extnum, char *filename);
int get_rpt_name(char *s);
int getBnkForPrimary(XIS_STD_KEYS *);
int getBnkForFrame(XIS_FRAME_INF *);
int  getEditModeNum(char *name);
char *getEditModeName(int mode);
int getBnkForExposure(XIS_FRAME_INF , XIS_EXPOSURE_INF *);
int getWindowNum(int window_option);
int updateRowNumber(FITS_EXT_STRUCT *ext, int);
int getTlmFileName(XIS_STD_KEYS *inf);
int getFRAMETstartTstop(fileTtime *);
int setFileTstart(fileTtime *);
int setFileTstop(fileTtime *);
int setTstartTstop(XIS_STD_KEYS *);
int readOutTime2readOutTimeH(unsigned int readOutTime);
int readOutTime2readOutTimeL(unsigned int readOutTime);
int addMinMax (FITS_EXT_STRUCT ext_struct);

/***********************************************************/
/*		FITSIO 関連				   */
/***********************************************************/
#define FITS_GET_MEMORY	char errorLog[FLEN_ERRMSG];
#define FITS_CHECK_ERROR_ANL(x)     if( (x) > 0 ) {\
						     fits_get_errstatus (fitsStatus, errorLog);\
						     fprintf(stderr, "%s: Status code is %d\nERROR::%s\n", \
							     pname, fitsStatus, errorLog);\
						     *status=ANL_QUIT;\
						     fflush(stderr);\
						     return;\
						 }
#define FITS_CHECK_ERROR(x)     if( (x) > 0 ) {\
						 fits_get_errstatus (fitsStatus, errorLog);\
						 fprintf(stderr, "%s: Status code is %d\nERROR::%s\n", \
							 pname, fitsStatus, errorLog);\
						 fflush(stderr);\
						 exit (x);\
					     }

#define MEMORY_CHECK(x)	if ((x)==NULL) {\
					  fprintf (stderr, "%s: memory allocation error\n", pname); \
					  fflush(stderr);\
					  return ANL_FALSE;}

#endif
