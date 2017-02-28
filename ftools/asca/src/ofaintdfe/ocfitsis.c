/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/ocfitsis.c,v 3.14 2002/04/05 15:55:00 irby Exp $   */
/*                   */
/* FITS accessing routines for sisftool by K.Mitsuda */
/* Modifed to suit ftools environment (Srilal) */
/* Modified by Jeff Guerber, Sept 17, 1996.  Initialize tscal, tzero, timedel
   to work around a cfortran.h problem on Alphas (SIGFPE at FCGBCL).  Fixed
   cfFillStr0, and changed to fill with blanks, also for cfortran.  */
/* MJT - 29Sept96 - initialization of "extension" needed for Sun (ANSI) cc */
/* Jeff Guerber, Feb. 24 1997. cfOpenFits: Changed exact to 0 so FCGCNO calls
   will be case-insensitive.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include <math.h> */
#include "ocfitsis5_41.h"
#include "cfitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "pfile.h"
#include "pctype.h"
#include "xpi.h"
#ifdef vms
#define RETURN 1
#endif

/*  #define DEBUG TRUE */

char datafile[FILENMLEN]; /* fits file name */
SISMODE datamode;
BITRATE bitrate;
int sensorid;
double timedel;
SISEVENT event;
struct column_s column;

static char infile[FILENMLEN];
static int unit=1;
static int nrecords;

void cfFillStr0(str, len)
     char *str;
     int len;
{
  int i;

  for (i=0; i<len-1; i++){    /* memset() probably better */
    str[i] = ' ';
  }
  str[len-1] = '\0';
}


void cfTermStr(str, len)
     char *str;
     int len;
{
  len--;
  *(str+len) = '\0';
  while(len){
    len--;
    if(*(str+len) == ' ') {
      *(str+len) ='\0';
    }else{
      return;
    }
  }
}

int cfGetSysParms()
{
  int status=0;
  int BufLen_2 = FILENMLEN-1;

  cfFillStr0(datafile, sizeof datafile);

  cuclgst("infile", datafile, &status);
  if(status) return(status);
  cfTermStr(datafile, sizeof datafile);


#ifdef DEBUG
  printf("%d %s\n", status, datafile);
#endif
  return(0);
}

int cfOpenFits()
{
  int rwmode = 0;
  long exact = 0;
  int block, rowlen, tfield, vardat, extension, hdtype;
  int maxcol=MAXCOL;
  char ttype[MAXCOL][CFSTRREC];
  char tform[MAXCOL][CFSTRREC];
  char tunit[MAXCOL][CFSTRREC];
/*rmj*/
  char temp1[CFSTRREC];
  char temp2[CFSTRREC];
/*rmj*/
  int  tbcol[MAXCOL];
  char extnam[CFSTRREC];
  double tscal = 1., tzero = 0.;
  int tnull;
/*  char tdisp;*/
  char tdisp[CFSTRREC];
  char filename[FILENMLEN];
  char datamodestr[CFSTRREC];
  char comment[CFSTRREC];
  char instrum[CFSTRREC];
  char bitratestr[CFSTRREC];
  int status=0;
  int BufLen_2 = FILENMLEN-1;

  extension=0;

/* parse datafile which can be with extension number with [ ]
   to file path name (=filename) and extention number */

  cfFillStr0(filename, sizeof filename);  /*initialize output memeory area*/
  cfTermStr(datafile, sizeof datafile);   /*truncate extra spaces*/

#ifdef DEBUG
  fprintf(stderr," Before Fcpars: \n");
  fprintf(stderr,"datafile='%s', filename='%s', extension='%d',status='%d\n",\
  datafile,filename,extension, status);
#endif

  Fcpars(datafile, filename, &extension, &status);
  if(status) {
    fprintf(stderr, "error in Fcpars\n");
    return(status);
  }

#ifdef DEBUG
  fprintf(stderr," After Fcpars: \n");
  fprintf(stderr,"datafile='%s', filename='%s', extension='%d',status='%d\n",\
  datafile,filename,extension, status);
#endif

  cfTermStr(filename, sizeof filename);  /*truncate extra spaces */

#ifdef DEBUG
  fprintf(stderr," After cfTermStr: \n");
  fprintf(stderr,"datafile='%s', filename='%s', extension='%d',status='%d\n",\
  datafile,filename,extension, status);
#endif

/* open fits file */
  FCOPEN(unit, filename, rwmode, &block, &status);
  if(status) {
    fprintf(stderr, "error in FCOPEN\n");
    return(status);
  }

/* move to the extention*/
  if(extension<=0) {
/* 29 Oct, 1993
    fprintf(stderr, "No extension in FITS file\n");
    return(999);
*/
    extension = 1;
  }

  FCMRHD(unit, extension, &hdtype, &status);
  if(status){
    fprintf(stderr, "error in FTMRHD\n");
    return(status);
  }
#ifdef DEBUG
  printf("FCMRHD: extension=%d, hdtype= %d\n", extension, hdtype);
#endif

/* get the common information in the header */
  switch( hdtype ) {
  case 1:
    FCGHTB(unit, maxcol, &rowlen, &nrecords, &tfield, ttype[0],
	   tbcol, tform[0], tunit[0], extnam, &status);
    break;
  case 2:
    FCGHBN(unit, maxcol,  &nrecords, &tfield, ttype[0],
	   tform[0], tunit[0], extnam, &vardat, &status);
    break;
  default:
    fprintf(stderr,"hdtype %d not supported.",hdtype);
    return(998);
  }
  if(status){
    fprintf(stderr, "error in FTGHTB/FTGHBN\n");
    return(status);
  }

/* read some header records */
/*  instrument */
  cfFillStr0(instrum, sizeof instrum);
  FCGKYS(unit, "INSTRUME", instrum, comment, &status);
  if(status){
    fprintf(stderr, "error in FTGKYS\n");
    return(status);
  }
  cfTermStr(instrum, sizeof instrum);

  if( sscanf(instrum,"SIS%d", &sensorid) == 0 ) {
    fprintf(stderr, "Instrument is not SIS.\n");
    return(997);
  }

/*  SIS data mode */
  cfFillStr0(datamodestr, sizeof datamodestr);
  FCGKYS(unit, "DATAMODE", datamodestr, comment, &status);
  if(status){
    fprintf(stderr, "error in FTGKYS\n");
    return(status);
  }
  cfTermStr(datamodestr, sizeof datamodestr);
#ifdef DEBUG
  fprintf(stderr,"instrument, datamodestr, bitratestr=%s, %s, %s\n",instrum, datamodestr, bitratestr);
#endif

  if( strcasecmp( datamodestr, "FAINT" ) ==0 ) {
    datamode=FAINT;
  }else if( strcasecmp( datamodestr, "BRIGHT" ) ==0 ){
    datamode=BRIGHT;
  }else if( strcasecmp( datamodestr, "FAST" ) ==0 ){
    datamode=FAST;
  }else{
    datamode=NONSIS;
    fprintf(stderr,"data mode is not SIS observation mode\n");
    return(996);
  }


/*  bit rate */
  cfFillStr0(bitratestr, sizeof bitratestr);
  FCGKYS(unit, "BIT_RATE", bitratestr, comment, &status);
  if(status){
    fprintf(stderr, "error in FTGKYS\n");
    return(status);
  }
  cfTermStr(bitratestr, sizeof bitratestr);

#ifdef DEBUG
  fprintf(stderr,"instrument, datamodestr, bitratestr=%s, %s, %s\n",instrum, datamodestr, bitratestr);
#endif

  if( strcasecmp( bitratestr, "HIGH" ) ==0 ) {
    bitrate=HIGH;
  }else if( strcasecmp( bitratestr, "MEDIUM" ) ==0 ) {
    bitrate=MED;
  }else if( strcasecmp( bitratestr, "LOW" ) ==0 ) {
    bitrate=LOW;
  }else{
    datamode=NORATE;
    fprintf(stderr,"bit rate is funny\n");
    return(995);
  }

/*  Exposure : 4, 8 or 16 sec. read from TIMEDEL  (Srilal) */
  timedel = 0.;
  FCGKYD(unit, "TIMEDEL", &timedel, comment, &status);
  if(status){
    fprintf(stderr, "Error in reading TIMEDEL\n");
    return(status);
  }

/* get the column numbers */
/* initialize */
  column.time.colnum=column.ccdid.colnum=column.rawx.colnum=column.rawy.colnum=0;
  column.detx.colnum=column.dety.colnum=column.x.colnum=column.y.colnum=0;
  column.phas.colnum=column.pha.colnum=column.pi.colnum=column.grade.colnum=0;
  column.pi.colnum=0;
/*   time */
  FCGCNO(unit, exact, "TIME", &(column.time.colnum), &status);


  if(status){
    fprintf(stderr, "column not found (time)\n");
  }else {
    cfFillStr0(column.time.type, sizeof column.time.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.time.colnum, temp1, temp2, column.time.type,
	   &column.time.repeat, &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.time.colnum, ttype[0], tunit[0], column.time.type,
	   &column.time.repeat, &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.time.type, sizeof column.time.type);
    if(status){
      fprintf(stderr, "column data type not found (time)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.time.colnum, type, repeat =%d, %s, %d\n",
	  column.time.colnum, column.time.type, column.time.repeat);
#endif


/*   ccdid */
  status=0;
  FCGCNO(unit, exact, "CCDID", &(column.ccdid.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (ccdid)\n");
  }else {
    cfFillStr0(column.ccdid.type, sizeof column.ccdid.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.ccdid.colnum, temp1, temp2, column.ccdid.type,
	   &(column.ccdid.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.ccdid.colnum, ttype[0], tunit[0], column.ccdid.type,
	   &(column.ccdid.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.ccdid.type, sizeof column.ccdid.type);
    if(status){
      fprintf(stderr, "column data type not found (ccdid)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.ccdid.colnum, type, repeat =%d, %s, %d\n",
	  column.ccdid.colnum, column.ccdid.type, column.ccdid.repeat);
#endif

/*   rawx */
  status=0;
  FCGCNO(unit, exact, "RAWX", &(column.rawx.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (rawx)\n");
  }else {
    cfFillStr0(column.rawx.type, sizeof column.rawx.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.rawx.colnum, temp1, temp2, column.rawx.type,
	   &(column.rawx.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.rawx.colnum, ttype[0], tunit[0], column.rawx.type,
	   &(column.rawx.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.rawx.type, sizeof column.rawx.type);
    if(status){
      fprintf(stderr, "column data type not found (rawx)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.rawx.colnum, type, repeat =%d, %s, %d\n",
	  column.rawx.colnum, column.rawx.type, column.rawx.repeat);
#endif

/*   rawy */
  FCGCNO(unit, exact, "RAWY", &(column.rawy.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (rawy)\n");
  }else {
    cfFillStr0(column.rawy.type, sizeof column.rawy.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.rawy.colnum, temp1, temp2, column.rawy.type,
	   &(column.rawy.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.rawy.colnum, ttype[0], tunit[0], column.rawy.type,
	   &(column.rawy.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.rawy.type, sizeof column.rawy.type);
    if(status){
      fprintf(stderr, "column data type not found (rawy)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.rawy.colnum, type, repeat =%d, %s, %d\n",
	  column.rawy.colnum, column.rawy.type, column.rawy.repeat);
#endif

/*   detx */
  status=0;
  FCGCNO(unit, exact, "DETX", &(column.detx.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (detx)\n");
  }else {
    cfFillStr0(column.detx.type, sizeof column.detx.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.detx.colnum, temp1, temp2, column.detx.type,
	  &(column.detx.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.detx.colnum, ttype[0], tunit[0], column.detx.type,
	   &(column.detx.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.detx.type, sizeof column.detx.type);
    if(status){
      fprintf(stderr, "column data type not found (detx)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.detx.colnum, type, repeat =%d, %s, %d\n",
	  column.detx.colnum, column.detx.type, column.detx.repeat);
#endif

/*   dety */
  status=0;
  FCGCNO(unit, exact, "DETY", &(column.dety.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (dety)\n");
  }else {
    cfFillStr0(column.dety.type, sizeof column.dety.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.dety.colnum, temp1, temp2, column.dety.type,
	   &(column.dety.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.dety.colnum, ttype[0], tunit[0], column.dety.type,
	   &(column.dety.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.dety.type, sizeof column.dety.type);
    if(status){
      fprintf(stderr, "column data type not found (dety)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.dety.colnum, type, repeat =%d, %s, %d\n",
	  column.dety.colnum, column.dety.type, column.dety.repeat);
#endif

/*   x */
  status=0;
  FCGCNO(unit, exact, "X", &(column.x.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (x)\n");
  }else {
    cfFillStr0(column.x.type, sizeof column.x.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.x.colnum, temp1, temp2, column.x.type,
	   &(column.x.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.x.colnum, ttype[0], tunit[0], column.x.type,
	   &(column.x.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.x.type, sizeof column.x.type);
    if(status){
      fprintf(stderr, "column data type not found (x)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.x.colnum, type, repeat =%d, %s, %d\n",
	  column.x.colnum, column.x.type, column.x.repeat);
#endif

/*   y */
  status=0;
  FCGCNO(unit, exact, "Y", &(column.y.colnum), &status);
  if(status){
    fprintf(stderr, "column not found (y)\n");
  }else {
    cfFillStr0(column.y.type, sizeof column.y.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.y.colnum, temp1, temp2, column.y.type,
	   &(column.y.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.y.colnum, ttype[0], tunit[0], column.y.type,
	   &(column.y.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.y.type, sizeof column.y.type);
    if(status){
      fprintf(stderr, "column data type not found (y)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.y.colnum, type, repeat =%d, %s, %d\n",
	  column.y.colnum, column.y.type, column.y.repeat);
#endif

/*   pi */
  status=0;
  FCGCNO(unit, exact, "PI", &(column.pi.colnum), &status);
  if(status){
/* column pi may not exist */
/*    fprintf(stderr, "column not found (pi)\n"); */
  }else {
    cfFillStr0(column.pi.type, sizeof column.pi.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.pi.colnum, temp1, temp2, column.pi.type,
	   &(column.pi.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.pi.colnum, ttype[0], tunit[0], column.pi.type,
	   &(column.pi.repeat), &tscal, &tzero, &tnull, tdisp, &status);
*/
    cfTermStr(column.pi.type, sizeof column.pi.type);
    if(status){
      fprintf(stderr, "column data type not found (pi)\n");
      return(990);
    }
  }
#ifdef DEBUG
  fprintf(stderr,"column.pi.colnum, type, repeat =%d, %s, %d\n",
	  column.pi.colnum, column.pi.type, column.pi.repeat);
#endif

  switch(datamode){
  case FAINT:
/*   phas */
    status=0;
    FCGCNO(unit, exact, "PHAS", &(column.phas.colnum), &status);
    if(status){
      fprintf(stderr, "column not found (phas)\n");
    }else {
      cfFillStr0(column.phas.type, sizeof column.phas.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.phas.colnum, temp1, temp2, column.phas.type,
	   &(column.phas.repeat), &tscal, &tzero, &tnull, tdisp, &status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*      FCGBCL(unit, column.phas.colnum, ttype[0], tunit[0], column.phas.type,
	     &(column.phas.repeat), &tscal, &tzero,&tnull,tdisp,&status);
*/
      cfTermStr(column.phas.type, sizeof column.phas.type);
      if(status){
	fprintf(stderr, "column data type not found (phas)\n");
	return(990);
      }
   }
#ifdef DEBUG
    fprintf(stderr,"column.phas.colnum, type, repeat =%d, %s, %d\n",
	    column.phas.colnum, column.phas.type, column.phas.repeat);
#endif

    break;
  case BRIGHT:
  case FAST:
/*   pha */
    status=0;
    FCGCNO(unit, exact, "PHA", &(column.pha.colnum), &status);
    if(status){
      fprintf(stderr, "column not found (pha)\n");
    }else {
      cfFillStr0(column.pha.type, sizeof column.pha.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.pha.colnum, temp1, temp2, column.pha.type,
	  &(column.pha.repeat), &tscal, &tzero,&tnull,tdisp,&status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*    FCGBCL(unit, column.pha.colnum, ttype[0], tunit[0], column.pha.type,
	     &(column.pha.repeat), &tscal, &tzero,&tnull,tdisp,&status);
*/
      cfTermStr(column.pha.type, sizeof column.pha.type);
      if(status){
	fprintf(stderr, "column data type not found (pha)\n");
	return(990);
      }
    }
#ifdef DEBUG
    fprintf(stderr,"column.pha.colnum, type, repeat =%d, %s, %d\n",
	    column.pha.colnum, column.pha.type, column.pha.repeat);
#endif

/*   grade */
    status=0;
    FCGCNO(unit, exact, "GRADE", &(column.grade.colnum), &status);
    if(status){
      fprintf(stderr, "column not found (grade)\n");
    }else {
      cfFillStr0(column.grade.type, sizeof column.grade.type);
/*rmj*/
  cfFillStr0(temp1, sizeof temp1);
  cfFillStr0(temp1, sizeof temp2);
    FCGBCL(unit, column.grade.colnum, temp1, temp2, column.grade.type,
	     &(column.grade.repeat), &tscal, &tzero,&tnull,tdisp,&status);
  strcpy (ttype[0], temp1);
  strcpy (tunit[0], temp2);
/*rmj*/
/*      FCGBCL(unit, column.grade.colnum, ttype[0], tunit[0], column.grade.type,
	    &(column.grade.repeat), &tscal, &tzero,&tnull,tdisp,&status);
*/
      cfTermStr(column.grade.type, sizeof column.grade.type);
      if(status){
	fprintf(stderr, "column data type not found (grade)\n");
	return(990);
      }
    }
#ifdef DEBUG
    fprintf(stderr,"column.grade.colnum, type, repeat =%d, %s, %d\n",
	    column.grade.colnum, column.grade.type, column.grade.repeat);
#endif

    break;
  default:
    fprintf(stderr,"mode number is funny.\n");
    return(994);
  }


  return(0);
}

int cfCloseFits()
{
  int status;

  FCCLOS(unit,&status);

  return(status);
}


void cfGetColumn(recnum, colinf, value)
     int recnum;
     COLTYPE colinf;
     void *value;
{
  static int felm=1, nelm=1, nullval=0, status;
  static  int anyf;


  if( colinf.colnum ){
    status=0;
    switch (colinf.type[0]) {
    case 'D':


      FCGCVD(unit, colinf.colnum, recnum,
	     felm, colinf.repeat, nullval,
	     value, &anyf, &status);
      break;
    case 'I':
      FCGCVI(unit, colinf.colnum, recnum,
	     felm, colinf.repeat, nullval,
	     value, &anyf, &status);
      break;
    case 'J':
      FCGCVJ(unit, colinf.colnum, recnum,
	     felm, colinf.repeat, nullval,
	     value, &anyf, &status);
      break;
    case 'B':
      FCGCVB(unit, colinf.colnum, recnum,
	     felm, colinf.repeat, nullval,
	     value, &anyf, &status);
      break;
    default:
      ffatal("col type, %s, is not supported.\n",colinf.type);
    }
  }
}


int cfGetSisEvent(recnum, event)
     int recnum;
     SISEVENT *event;
{

/* check record number*/
  if(recnum<1 || recnum >nrecords) return(1);

/* read data */
/*  time */

  if( column.time.colnum ){
    cfGetColumn(recnum, column.time, &(event->time) );
#ifdef DEBUG
    fprintf(stdout,"time= %f\n", event->time);
#endif
  }else{
    event->time = -0.1;
  }
/*  ccdid */
  if( column.ccdid.colnum ){
    cfGetColumn(recnum, column.ccdid, &(event->ccdid) );
#ifdef DEBUG
    fprintf(stdout,"ccdid= %d\n", event->ccdid);
#endif
  }else{
    event->ccdid = -1;
  }
/*  rawx */
  if( column.rawx.colnum ){
    cfGetColumn(recnum, column.rawx, &(event->rawx) );
#ifdef DEBUG
    fprintf(stdout,"rawx= %d\n", event->rawx);
#endif
  }else{
    event->rawx = -1;
  }
/*  rawy */
  if( column.rawy.colnum ){
    cfGetColumn(recnum, column.rawy, &(event->rawy) );
#ifdef DEBUG
    fprintf(stdout,"rawy= %d\n", event->rawy);
#endif
  }else{
    event->rawy = -1;
  }
/*  detx */
  if( column.detx.colnum ){
    cfGetColumn(recnum, column.detx, &(event->detx) );
#ifdef DEBUG
    fprintf(stdout,"detx= %d\n", event->detx);
#endif
  }else{
    event->detx = -1;
  }
/*  dety */
  if( column.dety.colnum ){
    cfGetColumn(recnum, column.dety, &(event->dety) );
#ifdef DEBUG
    fprintf(stdout,"dety= %d\n", event->dety);
#endif
  }else{
    event->dety = -1;
  }
/*  x */
  if( column.x.colnum ){
    cfGetColumn(recnum, column.x, &(event->x) );
#ifdef DEBUG
    fprintf(stdout,"x= %d\n", event->x);
#endif
  }else{
    event->x = -1;
  }
/*  y */
  if( column.y.colnum ){
    cfGetColumn(recnum, column.y, &(event->y) );
#ifdef DEBUG
    fprintf(stdout,"y= %d\n", event->y);
#endif
  }else{
    event->y = -1;
  }
/*  phas */
  if( column.phas.colnum ){
#ifdef DEBUG
    int j;
#endif
    cfGetColumn(recnum, column.phas, (event->phas) );
#ifdef DEBUG
    fprintf(stdout,"phas=");
    for(j=0; j<9; j++)
      fprintf(stdout," %d", event->phas[j]);
    fprintf(stdout,"\n");
#endif
  }else{
    event->phas[0] = -1;
  }
/*  pha */

column.pha.colnum = 0;
  if( column.pha.colnum ){
    cfGetColumn(recnum, column.pha, &(event->pha) );
#ifdef DEBUG
    fprintf(stdout,"pha= %d\n", event->pha);
#endif
  }else{
    event->pha = -1;
  }
/*  pi */
/*  grade */
  if( column.grade.colnum ){
    cfGetColumn(recnum, column.grade, &(event->grade) );
#ifdef DEBUG
    fprintf(stdout,"grade= %d\n", event->grade);
#endif
  }else{
    event->grade = -1;
  }
  return(0);
}


int cfSisEventProc(event)
  SISEVENT event;
{
  int recnum;

  for(recnum=1; recnum<=nrecords; recnum++){
    if(cfGetSisEvent(recnum, &event)){
      fprintf(stderr, "Error: in cfGetSisEvent\n");
      return(1);
    }
    if(anUserProcEvent(event)){
      fprintf(stderr, "Error: in anUserProcEvent\n");
      return(1);
    }
  }

  return(0);
}
