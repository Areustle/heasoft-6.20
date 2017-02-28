/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:52 1999 by E. Miyata*/
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include "atFunctions.h"
#include "aste_time.h"

#ifndef AE_EPOCH		/* 2000/1/1 0:0:0 の、1970/1/1 0:0:0 からの */
#define AE_EPOCH 946684800	/* 経過秒数 (うるう秒含まず)		    */
#endif

/*****************************************************************
  timeval をもらって aetime返す関数
******************************************************************/
double timeval2aetime (struct timeval t);

/*****************************************************************
  time_t をもらって aetime返す関数
******************************************************************/
double time2aetime (time_t t);

/*****************************************************************
  time_t をもらって localtime の aetime 返す関数
  つまり、localtime で 1993/01/01 からの経過時刻、と言う意味
******************************************************************/
double time2aetimeLocal (time_t t);

/*****************************************************************
  time_t をもらって日付を返す関数群
     UT で返すのは time2utCtime
     local 時間 で返すのは time2localCtime
******************************************************************/
char *time2utCtime    (time_t t, char *c);
char *time2localCtime (time_t t, char *c);

/*****************************************************************
  timeval をもらって日付を返す関数群
    UT で返すのは timeval2utCtime
    local 時間で返すのは timeval2localCtime
******************************************************************/
char *timeval2utCtime    (struct timeval t, char *c);
char *timeval2localCtime (struct timeval t, char *c);

/*****************************************************************
  aetime をもらって日付を返す関数群
    UT で返すのは aetime2utCtime
    local 時間で返すのは aetime2localCtime
******************************************************************/
char *aetime2utCtime    (double t, char *c);
char *aetime2localCtime (double t, char *c);

int  aetime2aedate (double aetime);
