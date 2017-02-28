/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:55 1999 by E. Miyata*/

/**********************************************************
  ver 0.0 2004/2/5 Hironori Matsumoto
     change XIS_STD_KEYS for the new keywords of TIMESYSTEM 
  
  ver 0.1 2005/6/8 Hironori Matsumoto
     XIS_STD_KEYS の mjdreff, mjdrefi のタイプを修正

  ver 0.2 2005/6/22 Hironori Matsumoto
     XIS_STD_KEYS に
        ra_obj, dec_obj, dec_pnt, pa_nom, 
	mean_ea1, mean_ea2, mean_ea3,
	tim_file, att_file, orb_file, teldef 
     を追加。   
     XIS_STD_KEYS の各フィールドにコメントのフィールドを用意。

  ver 2.5 2005/10/28 Hironori Matsumoto
     int delete_TSCAL_keys(FITS_EXT_STRUCT *ext) 
     を定義

     STDKEY を少し変更

  ver2.9 2007/04/23 Hironori Matsumoto
     STDKEYS に NOM_PNT を追加
  ************************************************************/

#ifndef _XIS_FITS_HEADER_UTIL_H_
#define _XIS_FITS_HEADER_UTIL_H_

#include <stdio.h>
#include "xisTelemFormat.h"

#define MAX_COL_NUM	256

typedef struct {
  fitsfile *fitsd;
  int   sensor;                 /* sensor ID */
  char  filename[FILENAME_MAX]; /* temporary FITS file name*/
  int   tbltype;                /* table type (ASCII or BINARY */
  int   naxis2;                 /* total event number */
  int   tfields;                /* number of table */
  char  *tform[MAX_COL_NUM];    /* format of each column in table */
  char  *tunit[MAX_COL_NUM];    /* unit of each column in table */
  char  *ttype[MAX_COL_NUM];    /* title of each column in table */
  double tlmin[MAX_COL_NUM];    /* minimum value of parameter */
  double tlmax[MAX_COL_NUM];    /* maximum value of parameter */
  char  *extname;               /* name of extension */
  char  *instrument;            /* instrument */
  int   rowNumber;              /* current event number */
} FITS_EXT_STRUCT;



typedef struct {
  /* TELESCOP */
  char *telescop;
  char *telescop_comment;
  /* INSTRUME */
  char *instrume;               
  char *instrume_comment;
  /* OBS_MODE */
  char *obs_mode;
  char *obs_mode_comment;
  /* DATAMODE */
  char *datamode;
  char *datamode_comment;
  /* XIS-AEID */
  char *xis_aeid;
  char *xis_aeid_comment;               
  /* EDITMODE */
  char *editmode;
  char *editmode_comment;
  /* CLK_MODE */
  char *clk_mode;
  char *clk_mode_comment;
  /* OBSID */
  char *obsid;
  char *obsid_comment;
  /* OBSERVER */
  char *observer;
  char *observer_comment;
  /* OBJECT */
  char *object;
  char *object_comment;
  /* OBS_REM */
  char *obsrem;
  char *obsrem_comment;
  /* RA_OBJ */
  double ra_obj;
  char *ra_obj_comment;
  /* DEC_OBJ */
  double dec_obj;
  char *dec_obj_comment;
  /* RA_PNT */
  double ra_pnt;
  char *ra_pnt_comment;
  /* DEC_PNT */
  double dec_pnt;
  char *dec_pnt_comment;
  /* RA_NOM */
  double ra_nom;
  char *ra_nom_comment;
  /* DEC_NOM */
  double dec_nom;
  char *dec_nom_comment;
  /* PA_NOM */
  double pa_nom;
  char *pa_nom_comment;
  /* MEAN_EA1 */
  double mean_ea1;
  char *mean_ea1_comment;
  /* MEAN_EA2 */
  double mean_ea2;
  char *mean_ea2_comment;
  /* MEAN_EA3 */
  double mean_ea3;
  char *mean_ea3_comment;
  /* RADECSYS */
  char *radecsys;
  char *radecsys_comment;
  /* EQUINOX */
  int equinox;
  char *equinox_comment;
  /* NOM_PNT */
  char *nom_pnt;
  char *nom_pnt_comment;
  /* DATE-OBS */
  char *date_obs;
  char *date_obs_comment;
  /* TIME-OBS */
  char *time_obs;
  char *time_obs_comment;
  /* DATE-END */
  char *date_end;
  char *date_end_comment;
  /* TIME-END */
  char *time_end;
  char *time_end_comment;
  /* TSTART */
  double tstart;
  char *tstart_comment;
  /* TSTOP */
  double tstop;
  char *tstop_comment;
  /* TELAPSE */
  double telapse;
  char *telapse_comment;
  /* ONTIME */
  double ontime; 
  char *ontime_comment; 
  /* TIMESYS */
  char *timesys;
  char *timesys_comment;
  /* MJDREFI; New for Astro-E2 */
  int mjdrefi;
  char *mjdrefi_comment;
  /* MJDREFF */
  double mjdreff;
  char *mjdreff_comment;
  /* TIMEREF */
  char *timeref;
  char *timeref_comment;
  /* TIMEUNIT */
  char *timeunit;
  char *timeunit_comment;
  /* TASSIGN */
  char *tassign;
  char *tassign_comment;
  /* CLOCKAPP */
  int  clockapp;
  char *clockapp_comment;
  /* TIMEDEL */
  double timedel;
  char *timedel_comment;
  /* TIMEPIXR */
  double timepixr;
  char *timepixr_comment;
  /* TIERRELA */
  double tierrela;
  char *tierrela_comment;
  /* TIERABSO */
  double tierabso;
  char *tierabso_comment;
  /* HDUCLASS */
  char *hduclass;
  char *hduclass_comment;
  char *hduclas1;
  char *hduclas1_comment;
  char *hduclas2;
  char *hduclas2_comment;
  /* TLM_FILE */
  char *tlm_file;
  char *tlm_file_comment;
  /* TIM_FILE */
  char *tim_file;
  char *tim_file_comment;
  /* ATT_FILE */
  char *att_file;
  char *att_file_comment;
  /* ORB_FILE */
  char *orb_file;
  char *orb_file_comment;
  /* LEAP_FILE */
  char *leap_file;
  char *leap_file_comment;
  /* TELDEF */
  char *teldef;
  char *teldef_comment;
  /* CREATOR */
  char *creator;
  char *creator_comment;
  /* ORIGIN */
  char *origin;
  char *origin_comment;

  float mjdref;			/* obsolate, but remains for backword compatibility */


  /* 以下は今後廃止。しかし、compatibility のために残すことにする。*/
  char *telescope;              /* TELESCOP */
  char *instrument;
  char  tlmfile[FILENAME_MAX];
  int nevents;			/* 使用されていないようだが？ */


} XIS_STD_KEYS;

#endif


#ifndef _XIS_FITS_HEADER_UTIL_FUNC_
#define _XIS_FITS_HEADER_UTIL_FUNC_

void setDefaultKeywordValues(XIS_STD_KEYS *);
int writeXISStdKeys (FITS_EXT_STRUCT *, XIS_STD_KEYS, int hdunum, int *status);
int updateStdTimeKeys (FITS_EXT_STRUCT *, XIS_STD_KEYS, int hdunum, int *status);
int updateHduclas(FITS_EXT_STRUCT *, int hdunum, int extID, int *status);
int write_DM_coor_keys(FITS_EXT_STRUCT *, int hdunum, int coor_flag, int *status);
int write_DM_GTI_keys(FITS_EXT_STRUCT *, int hdunum, int *status);
int updateDatamodeKey(FITS_EXT_STRUCT *, int hdunum, char *, int *status);
int delete_TSCAL_keys(FITS_EXT_STRUCT *);

#endif

