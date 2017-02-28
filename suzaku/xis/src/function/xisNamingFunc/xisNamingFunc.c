/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:56 1999 by E. Miyata*/
/**********************************
  xisFunction
  v0.0 1999/03/27  D. Akutsu
  v0.1 1999/05/13  E. Miyata
    replace top3 wth 2x2
  v0.2 2005/05/27  A. Bamba
    strstr function -> strncasecmp function in getEditModeNum
  v0.3 2005/10/31  Y. ISHISAKI
    return '\0' on error in getSegmentName()
**********************************/

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include "xisTelemFormat.h"
#include "xisNamingFunc.h"


/*
 * edit mode の名前から数字に変換する
 */

int  getEditModeNum (char *name) {
  if (strncasecmp (name+1, "5x5", strlen("5x5"))  == 0) {
    return XISedit5x5;
  } else if (strncasecmp (name+1, "3x3", strlen("3x3")) == 0) {
    return XISedit3x3;
  } else if (strncasecmp (name+1, "2x2", strlen("2x2")) == 0) {
    return XISedit2x2;
  } else if (strncasecmp (name+1, "Timing", strlen("Timing")) == 0) {
    return XISeditTiming;
  } else if (strncasecmp (name+1, "DarkInit", strlen("DarkInit")) == 0) {
    return XISeditDarkInit;
  } else if (strncasecmp (name+1, "DarkUpdate", strlen("DarkUpdate")) == 0) {
    return XISeditDarkUpdate;
  } else if (strncasecmp (name+1, "Frame", strlen("Frame")) == 0) {
    return XISeditFrame;
  } else if (strncasecmp (name+1, "DarkFrame",strlen("DarkFrame")) == 0) {
    return XISeditDarkFrame;
  } else if (strncasecmp (name+1, "StandBy",strlen("StandBy")) == 0) {
    return XISeditStandBy;
  } else {
    fprintf (stderr, "getEditModeNum: cannot resolve edit mode:%s\n", name);
    return -1;
  }

}

/*
 * edit mode の数字から名前に変換する
 */
char *getEditModeName (int mode) {
  switch (mode) {
  case XISedit5x5:
    return "5x5";
  case XISedit3x3:
    return "3x3";
  case XISedit2x2:
    return "2x2";
  case XISeditTiming:
    return "timing";
  case XISeditDarkInit:
    return "darkinit";
  case XISeditDarkUpdate:
    return "darkupdate";
  case XISeditFrame:
    return "frame";
  case XISeditDarkFrame:
    return "darkframe";
  case XISeditStandBy:
    return "standby";
  }
  fprintf (stderr,"getClocModeName: cannot resolve cloc mode num:%d\n",mode);
  return NULL;
}


/*
 * clock mode の名前から数字に変換する
 */
int getClockModeNum (char *name) {

  if (strstr (name, "normal") != NULL) {
    return XISclockNormal;
  } else if (strstr (name, "psum") != NULL) {
    return XISclockPsum;
  } else if (strstr (name, "burst") != NULL) {
    return  XISclockBurst;
  } else{
    fprintf (stderr,"getClocModeNum: cannot resolve cloc mode name name:%s\n",name);
    return -1;
  }
}


/*
 * clock mode の数字から名前に変換する
 */
char *getClockModeName (int clock) {
  switch (clock) {
  case XISclockNormal:
    return "normal";
  case XISclockPsum:
    return "psum";
  case XISclockBurst:
    return "burst";
  }
  fprintf (stderr,"getClocModeNum: cannot resolve cloc mode num:%d\n",clock);
  return NULL;
}


/*
 *segment を数字からアルファベットに変換する
 */
char getSegmentName (int segment) {
  char name='a';

  if(0<=segment&&segment<=25){
    name=name+segment;
    return name;
  }
  fprintf (stderr,"getSegmentName: cannot resolve segment num:%d\n",segment);
  return '\0';
}


/*
 *segment をアルファベットから数字に変換する
 */
char getSegmentNum (char name) {

  char segment;

  if('a'<=name&&name<='z'){
    segment=name-'a';
    return segment;
  }
  fprintf (stderr,"getSegmentNum: cannot resolve segment name:%c\n",name);
  return -1;
}
