/************************************************************************/
/*  ascatime
/*    [English translation] of the original Japanese comments added 
/*    by Ken Ebisawa on 1999-11-05
/*    
/*    AtTimeとミッションタイムを変換するルーチン
/*    [Routine to convert between the AtTime and Mission time]
/*					by R.Fujimoto
/*
/*
/*  93/03/06 V1.0	閏秒を考慮するようにした [take into account of leap seconds]
/*     03/16 V1.1	データをメモリに保存するようにした [store data in memory]
/*     03/28 V1.2       閏秒が正しく変換されるようにした [correct leap second conversion]
/*     06/10 V1.3       閏秒のテーブルを環境変数で指定 [leap second table is specfied by
/*                      an environmental variable]
/*     06/12 V1.4       leapflagをNOに初期化した [leapflag is initialized as NO]
/*
/*  95/10/14 V2.0	bug fixならびにmjd2asca、asca2mjdという関数を追加。
/*                      [bug fixed, mjd2asca, asca2mjd added]
/*     10/15 V2.1	minor bug fix
/*     10/16 V2.3	reformAtTimeの引数に閏秒を加える。 [leap second is added to the
/*                      arguments of reformAtTime]
/*			asca2attimeで閏秒が入った瞬間かどうかの判定を改良。
/*                      [in asca2attitime, algorithm of judgement of the the moment 
/*                       when a leap second is inserted is improved]
/*     10/16 V2.4	asca2mjdtmpを新設。asca2attimeとasca2mjdを修正。
/*			閏秒前後でmjdが連続になるように定義。
/*                      [asca2mjdtmp added. asca2attime, asca2mjd modified.
/*                       mjd is so defined as to be continuous before and after
/*                       the leap seconds]
/*  98/07/22 v2.5       1) switched to leapsec.fits from leapsec.dat
/*                         This is now similar to ascatime.c in mkfilter2,
/*                         but it has two more functions.
/*                      2) No longer using ENV variable to access leapsec file.
/*                         As a result, a few functions were removed.
/*  99/10/21 v2.6       Changed the attime0 from 93 to 1993.
/*                      
/****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <atFunctions.h>
#include "fitsio.h"

#define EPSILON_T	0.00005

#define YES 1
#define NO 0
#define MAXDAT 30

static AtTime attime0 = {1993, 1, 1, 0, 0, 0, 0.0};
static double mjd0;
static AtTime leaptime[MAXDAT];
static double mjd_leap[MAXDAT];
static double step[MAXDAT];
static int ndata = 0;
static int readDataFlag = NO;

/**************************************************************************
 * this variable is used only if the calling function failed to 
 * initialize the leapsec table first using the leapsec.fits file
 * specified in parameter file;
 * If this variable is used, warning msg will be sent to stderr
 **************************************************************************/
#define LEAPTABLE "/opt/local/ftools/ftools.4.1/SunOS_5.4_sparc/refdata/leapsec.fits"


/************************************************************************/
/*  round
/*    小数点以下を四捨五入する関数 [round decimals]
/*
/*  95/10/14
/*  ・static double型に変更
/************************************************************************/
double round(double x)
{
  double n;

  n = (double)(int)x;
  if (x > 0.0) {
    if ((x - n) < 0.5)
      return n;
    else
      return (n + 1.0);
  } else {
    if ((n - x) < 0.5)
      return n;
    else
      return (n - 1.0);
  }
}


/************************************************************************/
/*  reformAtTime
/*    繰り上がりを考慮してAtTimeを整形する関数 
/*    [reform AtTime taking into account of raising decimals]
/*    Input&Output:
/*      *attime: 整形したい時刻 [time to be reformed]
/*    Input:
/*      extrasec: 繰り上がりが60秒より長い場合にはその数字を与える。
/*                60秒で繰り上がる場合には0.0を渡すこと。ただし、現状では
/*                整数部分しか見ていない。
/*                [When the amount of raising is longer than 60 sec,
/*                 give that number.  If being raised at 60 sec, give 0.0.
/*                 However, only the integer part is checed currently.]
/*
/*  95/10/14
/*  ・attime->mn = month;となっていたバグをfix。
/*    [attime->mn = month; bug fix]
/*  ・static void型に変更。[convert to static void]
/*  ・名称をreformからreformAtTimeに変更。 [rename from reform to reformt AtTime]
/*
/*  95/10/16
/*  ・引数にextrasecを追加。 [add extrasec to the argument]
/************************************************************************/
static void reformAtTime(AtTime *attime, double extrasec)
{
  int year, month, day, hour, minute, sec;
  int fullsec;
  float msec;
  double round(double);

  year = attime->yr;
  month = attime->mo;
  day = attime->dy;
  hour = attime->hr;
  minute = attime->mn;
  sec = attime->sc;


  /* ミリ秒を10倍して小数点以下を四捨五入、これが10000を越える場合は
     繰り上がり計算を行なう 
     [multiply miliseconds by 10,  and round the decimals. If this execced
     100000, calculate to raise.]*/
  msec = (round((double)(attime->ms)*10.0));
  if (msec >= 10000.0) {
    msec -= 10000.0;
    msec /= 10.0;
    sec++;
  } else {
    msec /= 10.0;
  }

  /* 閏秒が挿入されている場合、60秒ではなく60+extrasec秒で繰り上がる 
     [When leapseconds are inserted, raise at 60+extrasec, not 60 sec.]*/
  fullsec = 60 + (int)extrasec;
  sec += (int)extrasec;
  if (sec >= fullsec) {
    sec -= fullsec;
    minute += 1;
  }

  if (minute >= 60) {
    minute -= 60;
    hour++;
  }

  if (hour >= 24) {
    hour -= 24;
    day++;
  }

  if (day > 28) {
    if ((day == 29) && (month == 2) && (year%4 != 0)) {
      day = 1;
      month = 3;
    } else if ((day == 30) && (month == 2) && (year%4 == 0)) {
      day = 1;
      month = 3;
    } else if ((day == 31) && ((month == 4) || (month == 6) || (month == 9) || (month == 11))) {
      day = 1;
      month++;
    } else if (day == 32) {
      day = 1;
      month++;
    }
  }

  if (month == 13) {
    month = 1;
    year++;
  }

  attime->yr = year;
  attime->mo = month;
  attime->dy = day;
  attime->hr = hour;
  attime->mn = minute;
  attime->sc = sec;
  attime->ms = msec;
}


/************************************************************************
 * Using leapsec.fits in FTOOLS' refdate area instead of leadsec.dat
 * The starting leapsec is 7/1, 1993, leapsec before that will be ignored
 * 
 ************************************************************************/
void readLeapTable(char *leaptable)
{

  fitsfile *fp;
  int i, index, hdutype, status=0, anynul=0;
  long NAXIS2;
  char comm[73];

  /* initialize mjd0 from attime0 */
  atMJulian(&attime0, &mjd0);

  if ( fits_open_file(&fp, leaptable, READONLY, &status) ) {
    c_fcecho("Error openning leapsec table (%d): %s\n",status,leaptable);
    exit (1);
  }

  if (fits_movabs_hdu(fp, 2, &hdutype, &status))
    fits_report_error(stderr, status);

  if (fits_read_key_lng(fp, "NAXIS2", &NAXIS2, comm, &status))
    fits_report_error(stderr,status);

  /***************************************************************** 
   * READ MJD and determine when mjd > mjd0
   ******************************************************************/
  /* now read mjd time from col 3 in leapsec.fits */
  if (fits_read_col_dbl(fp,3,1,1,NAXIS2,0.0,mjd_leap,&anynul,&status))
    fits_report_error(stderr,status);

  /* find the starting rows of 1/7/93 (july 1 1993) */
  for (i=0; i<NAXIS2; i++) {
    if (mjd_leap[i] >= mjd0) break;
  }
  
  index = i+1;
  
  /* re-arrange mjd_leap[] */
  for (i=index-1;i<NAXIS2;i++)
    mjd_leap[i-index+1]=mjd_leap[i];

  /* read column LEAPSECS (step):  dbl*/
  if (fits_read_col_dbl(fp,5,index,1,NAXIS2-index+1,0.0,step,&anynul,&status))
    fits_report_error(stderr,status);

  if (fits_close_file (fp, &status)) {
    c_fcecho("Warning: error closing file (status=%d):%s\n",status,leaptable);
  }


  /* now convert the data to leaptime, and step[] */
  ndata=0;
  for (i=index-1;i<NAXIS2;i++){
    mjd_leap[i-index+1]=mjd_leap[i];
    atMJDate(mjd_leap[i],&leaptime[i-index+1]);
    ndata++;
  }

  readDataFlag = YES;

  c_fcecho("leapsec FITS data was successfully read\n");

  /*
  c_fcecho("DEBUG: ndata= %d\n",ndata);
  for (i=0;i<ndata;i++) {
    c_fcecho("%d %d %d %d %d %d %f %f %lf\n",leaptime[i].yr,leaptime[i].mo,
	   leaptime[i].dy,leaptime[i].hr,leaptime[i].mn,
	   leaptime[i].sc,leaptime[i].ms,step[i],mjd_leap[i]);
  }
  */

}


/************************************************************************/
/*  attime2asca
/*    日時（AtTime型）をミッションタイムに変換する [convert date and time
/*                   (AtTime type) to the mission time.]
/*
/*  95/10/14  
/*  ・基準時のmjd0と、閏秒が挿入された時のmjd_leapをreadLeapTableで計算する
/*    ようにした。
/*    [Standard mjd0 and the mjd_leap when leap seconds are inserted are
/*    calculated by readLeapTable.]
/************************************************************************/
double attime2asca(AtTime attime)
{
  double mjd;
  double ascatime;
  int i;

  if (readDataFlag == NO) {
    c_fcerr("Warning: using %s\n",LEAPTABLE);
    readLeapTable(LEAPTABLE);
  }

  /* 日時をmjdに変換する [convert date and time to MJD]*/
  atMJulian(&attime, &mjd);

  /* ミッションタイムを計算する [calculate the mission time]*/
  ascatime = mjd*86400.0 - mjd0*86400.0;

  for (i = 0; i < ndata; i++) {
    /* 問題の日時が閏秒を挿入した後なら、その時間を考慮する 
       The date and time considering is after insertion of the leapseconds,
       take into account that time*/
    if (mjd >= mjd_leap[i]) {
      ascatime += step[i];
      if ((mjd < mjd_leap[i] + step[i]/86400.0) && ((attime.mo != leaptime[i].mo) || (attime.dy != leaptime[i].dy) || (attime.hr != leaptime[i].hr) || (attime.mn != leaptime[i].mn)))
	ascatime -= step[i];

    } else
      break;
  }

  return (ascatime);
}  


/************************************************************************/
/*  asca2mjdtmp
/*    ミッションタイムを日時に変換するための基本的なルーチン。
/*     [basic routine to convert mission time to date and time]
/*    Input:
/*      ascatime: ミッションタイム [mission time]
/*    Output:
/*      *mjd: テンポラリmjd [temporary mjd]
/*      *leapflag: 閏秒が挿入された瞬間か否かを示すフラグ [flag to tell if this is
/*       the memoent leapsecond is inserted]
/*      *currentleap: 挿入された閏秒 [inserted leap seconds]
/*      *mjd_leap: 閏秒が挿入された時点のmjd。 [mjd when leap seconds are inserted]
/*
/*  95/10/16 
/*  ・EPSILON_Tを導入し、閏秒が挿入されたその瞬間の判定方法を改善した。
/*    [Introduce EPSILON_T, improve the algorithm to judge the mmoment when
/*     a leap second is introduced.]
/*  ・asca2attimeの前半部分を独立させた。 [the first half of asca2atteime is made
/*    independent.]
/************************************************************************/
static void 
asca2mjdtmp(double ascatime, double *mjd, int *leapflag, double *currentleap, 
	    double *mjdLeap)
{
  double asca_leap;
  double totalleap = 0;
  FILE *fp;
  double round(double);
  int i;

  /* ポインタ変数の初期化 [initialize pointer variable]*/
  *currentleap = 0.0;
  *leapflag = NO;

  /* 閏秒のテーブルを開く [open the leapsecond table]*/
  if (readDataFlag == NO) {
    c_fcerr("Warning: using %s\n",LEAPTABLE);
    readLeapTable(LEAPTABLE);
  }

  /* ミッションタイムに相当するmjdを算出する [calcluate mjd corresponding to
     the mission time]]*/
  *mjd = mjd0 + ascatime/86400.0;

  for (i = 0; i < ndata; i++) {

    /* 閏秒が挿入された日時に相当するミッションタイムを算出する 
       [calculate the mission time corresponding to the date ane time when
       leap seconds are inserted]*/
    asca_leap = attime2asca(leaptime[i]);

    if (ascatime >= asca_leap) {
      /* 閏秒が挿入されている場合 [in the case leapseconds are inserted] */
      totalleap += step[i];
    } else if (ascatime > asca_leap - step[i] - EPSILON_T) {
      /* 閏秒が挿入されたその瞬間で、対応するmjdが存在しない場合 
         [in the case leap seconds are inserted and the corresponding mjd does
          not exist]r*/
      *leapflag = YES;
      totalleap += step[i];
      *mjdLeap = mjd0 + (asca_leap - totalleap)/86400.0;
      *currentleap = step[i];
      break;
    } else {
      /* 閏秒が挿入されていない場合 [when leap seconds are not inserted]*/
      break;
    }
  }

  /* 挿入された閏秒の分だけミッションタイムから差っ引く 
   [subtract the inserted leap seconds from the mission time]*/
  ascatime -= totalleap;
  *mjd = mjd0 + ascatime/86400.0;

}


/************************************************************************/
/*  asca2attime
/*    ミッションタイムを日時に変換する。[convert mission time to date and time]
/*    現在、秒単位の閏秒にしか対応していません。[only can handle leap seconds
/*    in the unit of second]
/*
/*  95/10/14 
/*  ・mjd0を外部変数とし、readLeapTableで読ませるようにした。[mjd0 is an external
/*    variable, and read from readLeapTable]
/*  95/10/16
/*  ・閏秒が入った時の繰り上げ処理をreformAtTimeで行なうようにした。
/*    [reformAtTime takes care of raising decimals when leap seconds are inserted]
/*  ・前半部分をasca2mjdtmpという関数として独立させた。
/*    [The first half is made independent as asca2mjdtmp]
/************************************************************************/
asca2attime(double ascatime, AtTime *attime)
{
  double mjd, currentleap = 0.0;
  double dummy;
  int leapflag = NO;


  /* ミッションタイムからMJDと（閏秒が挿入された瞬間ならば）その閏秒を計算 */
  /* calcllated MJD from mission time. At the moment leapsecons are
   inserted, calculate that leap seconds */
  asca2mjdtmp(ascatime, &mjd, &leapflag, &currentleap, &dummy);

  /* MJDをAtTimeに変換 [convert MJD to AtTime ]*/
  atMJDate(mjd, attime);

  /* 繰り上がりを考慮してAtTimeを整形する [reform AtTime taking account of
   raising decimals] */
  reformAtTime(attime, currentleap);
}


/************************************************************************/
/*  mjd2asca
/*    mjdをascatimeに変換する。[convert mjd to ascatime]
/************************************************************************/
double
mjd2asca(double mjd)
{
  AtTime attime;

  atMJDate(mjd, &attime);
  return (attime2asca(attime));
}


/************************************************************************/
/*  asca2mjd
/*    ascatimeをmjdに変換する。[covert ascatime to mjd]
/*
/*  95/10/16
/*  ・asca2attimeを経由しないようにした。 [not through asca2atttime]
/*  ・閏秒の時にも連続になるようにした。  [continuous even at leap seconds]
/*  1998-07-22: changed mjd_leap to mjdLeap to avoid confusion with mjd_leap[]
/************************************************************************/
double 
asca2mjd(double ascatime)
{
  double mjd, mjdLeap, dummy;
  int leapflag = NO;

  /* ミッションタイムからMJDを計算する [calculate MJD from mission time]*/
  asca2mjdtmp(ascatime, &mjd, &leapflag, &dummy, &mjdLeap);

  /* 閏秒が挿入されている最中ならば、対応するMJDがないので、挿入された時の
     MJDを返す [if this is during insertion of leap seconds, there will be no
     corresponding MJD, so MJD at the insertion is returned*/
  if (leapflag == YES)
    return (mjdLeap);
  else 
    return (mjd);
}
