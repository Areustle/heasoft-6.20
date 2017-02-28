/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/ofaintdfe.c,v 3.11 2001/10/15 19:08:59 zpan Exp $   */
/*                   */
/**************************************************************/
/* SELECTOR TASK: 						*/
/*      faintdfe						*/

/* FILE:							*/
/*      faintdfe.f						*/

/* DESCRIPTION:							*/
/*       Calculate the dark frame error for SIS FAINT data	*/
/*       based on a time-dependent template.			*/
/*	 (This is the new version of faintdfe, originally 	*/
/*	 developed by Chiko Otani (RIKEN) as animaldfe5.41.c)	*/

/* MODIFICATION HISTORY:					*/
/*       12/18/95 (Srilal - NASA/GSFC)  			*/
/*                Modified to suit to the ftools environment	*/
/*  9/18/1997 (Jeff Guerber - GSFC/HSTX)   In anUserProcEvent,  */
/*     removed "zero-levels..." warning (req. by Ed Pier)       */
/**************************************************************/
/*                                                            */
/*  << animaldfe5.41.c >>                                     */
/*                                                            */
/*               example userfunc.c          by K.Mitsuda     */
/*   1993.10.30  modified to faintdfe.c      by C.Otani       */
/*   1993.11. 8  modified to animaldfe.c     by C.Otani       */
/*   1995. 3. 1  modified to animaldfe5.31.c by C.Otani       */
/*               use template                                 */
/*   1995. 3.16  change the frequency to make template        */
/*               ASCA time (int --> double)                   */
/*   1995. 4. 7  modified to animaldfe5.32.c by C.Otani       */
/*               added "-l", "-o1", "-o2", & "-o4" options    */
/*   1995. 6. 1  modified to animaldfe5.33.c by C.Otani       */
/*               bug in getTemplate is fixed                  */
/*               added "-z" option                            */
/*   1995. 8. 7  modified in order to avoid to be trapped by  */
/*               a side peak in the peak search               */
/*   1995. 8.10  modified in order to set the zero-level to   */
/*               that in bright mode                          */
/*   1995. 8.22  erfcc.c is used in spite of erf              */
/*   1995. 8.25  modified to ver. 5.41                        */
/*               Table parameters are changd by A.Yamashita   */
/*   1995. 9. 2  three -z alternatives                        */
/*   1995. 9. 7  To introduce the modedist[3] to determine    */
/*               the number of readout chips                  */
/*                                                            */
/**************************************************************/


/****************/
/*   includes   */
/****************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ocfitsis5_41.h"


/*******************/
/*   Definitions   */
/*******************/

#define MAX_A  40
#define MAX_B  40
#define MAX_C  ( MAX_A+MAX_B )
#define COUNT_MIN 10
#define EXP_MIN    5

typedef struct{
  double av[2*(MAX_A+MAX_B)+1];
  double nd[2*(MAX_A+MAX_B)+1];
}   CCR;


typedef struct{
  double ave;
  double sig;
  double n;
}   AVE_SIG;



/***********************/
/*   Local variables   */
/***********************/
int BufLen_2 = FILENMLEN-1;
int split;
double binsec;
double start;
double start0;
int zerodef;
char outfile[FILENMLEN];
char tblfile[FILENMLEN];

static double dst0[3][2*MAX_A+1];
static double dst1[4][2*MAX_B+1];
static double brtzero[2][3];
static double zl[4], zl0[4];
static int npixel[4];
static int flag;
static FILE *fp1;


static int expflag;
static int exposure;
static int LaunchFlag=0;
static int ZeroLevelFlag;

/*************************************************/
/*   Parameters to calculate the template file   */
/*     [i][j]: i=0 SIS-0                         */
/*             i=1 SIS-1                         */
/*             j=0 1CCD mode                     */
/*             j=1 2CCD mode                     */
/*             j=2 4CCD mode                     */
/*************************************************/
/*   Base time   */
static double ASCA_t0;

/*   f: Fraction   */
static double tf0[2][3];
static double Tf[2][3];

/*   sig: Sigma   */
static double ss0[2][3];
static double ss1[2][3];

/*   Q: Exponential scale   */
static double QQ0[2][3];
static double QQ1[2][3];

/*   q: centroid   */
static double qq0[2][3];
static double qq1[2][3];

/*   others   */
static double q0[2][3];
static double ffff[2][3];
static double Q[2][3];
static double sig[2][3];
static int modedist[3];

/*  The following are read from cfOpenFits */
extern int sensorid;
extern double timedel;

#include "oerfcc.c"
#include "oreadTempPara.c"
#include "ogetTemplate.c"
#include "otrapzd.c"
#include "oqromb.c"
#include "onew_zero.c"
#include "opolint.c"
/* #include "nrutil.c" */




/**************************************************/
/*   upper: Function to convert character upper   */
/* name changed to avoid conflict with "aspect"   */
/**************************************************/
char *uppr(str,  n)
char *str;
int n;
{
  int  i;
  for (i=0;i<n;i++)   *(str+i) = (char)toupper((unsigned char)(*(str+i)));
  return str;
}


/****************************************************/
/*   ccr: Function to calculate Cross-Correlation   */
/****************************************************/
int ccr(  dst_a,  dst_b,  ccrpeak )
double dst_a[];
double dst_b[];
double *ccrpeak;
{
  AVE_SIG  b;
  CCR      ccr0;
  int      i, j, n;
  double   fact;
  double   max_c, max_i;
  double   a1, a2, a3;
  int      x1, x2, x3;
  double   y1, y2, y3;


/*   Calculate Averages and Measurement Errors   */
  b.ave = b.sig = b.n = 0.0;
  for (j=-MAX_B;j<=MAX_B;j++){
    b.ave += dst_b[j+MAX_B];
    b.sig += ( dst_b[j+MAX_B]*dst_b[j+MAX_B] );
    b.n   += 1.0;
  }
  b.ave /= b.n;
  b.sig /= b.n;
  b.sig = sqrt((b.sig-b.ave*b.ave)*(b.n/(b.n-1.0)));


/*   Normalized by b.sig   */
  for (j=-MAX_B;j<=MAX_B;j++){
    dst_b[j+MAX_B] = ( dst_b[j+MAX_B]-b.ave )/b.sig;
  }


/*   initialize CCR  */
  for (i=-MAX_C;i<=MAX_C;i++){
    ccr0.av[i+MAX_C] = ccr0.nd[i+MAX_C] = 0.0;
  }


/*   get CCR   */
  for (i=-MAX_A;i<=MAX_A;i++){
    for (j=-MAX_B;j<=MAX_B;j++){
      ccr0.av[j-i+MAX_A+MAX_B] += (dst_a[i+MAX_A]*dst_b[j+MAX_B]);
      ccr0.nd[j-i+MAX_A+MAX_B] += 1.0;
    }
  }
  for (i=-MAX_C;i<=MAX_C;i++){
    if ( ccr0.nd[i+MAX_C] != 0.0 )   ccr0.av[i+MAX_C] /= ccr0.nd[i+MAX_C];
  }

/*   search peak   */
  max_c = -1.0;
  max_i = -(double)MAX_C;
  n = 0;
  for (i=-MAX_C/2;i<=MAX_C/2;i++){
    if      ( max_c <  ccr0.av[i+MAX_C] && ccr0.av[i+MAX_C] > 0.0 ){
      max_c = ccr0.av[i+MAX_C];
      max_i = (double)i;
      n = 1;
    }
    else if ( max_c == ccr0.av[i+MAX_C] && ccr0.av[i+MAX_C] > 0.0 ){
      max_i = ( max_i*(double)n+(double)i )/(double)(n+1);
      n++;
    }
  }

/*   quadratic fit   */
  x2 = (int)(max_i+(double)MAX_C+0.5)-MAX_C; /*   »Í¼Î¸ÞÆþ   */
  x1 = x2-1;
  x3 = x2+1;
  y1 = ccr0.av[x1+MAX_C];
  y2 = ccr0.av[x2+MAX_C];
  y3 = ccr0.av[x3+MAX_C];
  a1 = ( y1+y3 )*0.5 - y2;
  a2 = ( y2-y1 )-a1*(double)(x1+x2);
  a3 = y2 - a1*pow((double)x2,2.0) - a2*(double)x2;
  (*ccrpeak) = -0.5*a2/a1;

  return 0;
}



/****************************************/
/*   anGetUserParms: Initial settings   */
/****************************************/
int anGetUserParms()
{
  int BufLen_2 = FILENMLEN-1;
  int status = 0;
  int i, j;
  char file[FILENMLEN];

/*   initialization   */
  for (i=0;i<3;i++){
    for (j=0;j<=2*MAX_A;dst0[i][j++]=0.0);
  }
  for (i=0;i<2;i++){
    for (j=0;j<3;brtzero[i][j++]=0.0);
    for (j=0;j<3;q0[i][j++]     =0.0);
    for (j=0;j<3;ffff[i][j++]   =0.0);
    for (j=0;j<3;Q[i][j++]      =0.0);
  }
  for(i=0;i<4;i++){
    for(j=0;j<=2*MAX_B;dst1[i][j++]=0.0);
    npixel[i]=0;
    zl[i]=zl0[i]=0.0;
  }
/*  tim =0.0;*/
  flag=0;
  expflag=0;
  modedist[0] = modedist[1] = modedist[2] = 0;

/*   Read table filename   */
    cuclgst("tblfile", tblfile, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get tblfile parameter \n");
     exit(1);
     }

/*   Get PHD Table   */
  if ( readTempPara(tblfile) != 0 )   return status;

     /*   Read Split Threshold   */
     cuclgsi("split", &split, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get SPLIT parameter \n");
     exit(1);
     }

/*   Read Bin Width in second   */
     cuclgsd("binsec", &binsec, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get BINSEC parameter \n");
     exit(1);
     }

/*   Read Start in second   */
     cuclgsd("start", &start, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get START parameter \n");
     exit(1);
     }

/*   Read ZeroLevelFlag    */
     cuclgsi("zerodef", &zerodef, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get ZERODEF parameter \n");
     exit(1);
     }

     ZeroLevelFlag = zerodef;
/* Use the same variable name as in the original animaldfe */


/*   Read output filename   */
    cuclgst("outfile", outfile, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get OUTFILE parameter \n");
     exit(1);
     }

/*   set out file   */
  sprintf(file,"%s",outfile);
  if ( !strcmp(uppr(file,strlen(file)),"STDOUT") )   fp1=stdout;
  else if ( ( fp1 = fopen(outfile,"w") ) == NULL ){
    fprintf(stderr,"File Open Error !! (%s)\n",outfile);
    return 1;
  }
  printf("");
  return 0;
}



/*****************************************/
/*   anUserProcEvent: Event treatments   */
/*****************************************/
int anUserProcEvent(event)
SISEVENT event;
{
  int       value, grade;
  short int *handle[9];
  int       sumph, type, above; /* dummy */
  int       id, i, j;
  int       sum;
  int       ccdmode=0, chip;
     event.sensor = sensorid;
     event.exp =  (int) timedel;

  if ( expflag == 0 ){  /* This is done only once for the first event */
    start = event.time;
    getTemplate5_41(start,event.sensor,dst0);

    /*   The case in which "-br"("-z 2") is specified   */
    if ( ZeroLevelFlag == 2 ){
      for (ccdmode=0;ccdmode<=2;ccdmode++){
	  brtzero[event.sensor][ccdmode] = new_zero(event.sensor,ccdmode);
	  /*   The same values are used for all chips in the same sensor   */
      }
      /* fprintf(stderr,"animaldfe5.41: zero-levels are set to ones expected in bright mode.\n"); */
    }

    expflag++;
  }


/*   calculations   */
  if ( event.time > start + binsec*4.0 ||
      ( event.time > start + binsec &&
       modedist[0]+modedist[1]+modedist[2]>=EXP_MIN ) ){
    if ( npixel[0] >= COUNT_MIN || npixel[1] >= COUNT_MIN ||
	 npixel[2] >= COUNT_MIN || npixel[3] >= COUNT_MIN ){
      if ( modedist[0]>=modedist[1] && modedist[0]>=modedist[2] )
	ccdmode = 0;
      else if ( modedist[1]>=modedist[2] )
	ccdmode = 1;
      else
	ccdmode = 2;
    }
    for (i=0;i<4;i++){
      if ( npixel[i] > COUNT_MIN ){
	ccr(dst0[ccdmode],dst1[i],&zl[i]);
	switch ( ZeroLevelFlag ){
	case 1:
	  zl[i] += ( q0[event.sensor][ccdmode] +
		    ffff[event.sensor][ccdmode]*Q[event.sensor][ccdmode] );
	  break;
	case 2:
	  zl[i] += brtzero[event.sensor][ccdmode];
	  break;
	case 0: default:
	  break;
	}
      }
      else {
	zl[i] = zl0[i];
      }
    }
    if ( zl[0]!=zl0[0] || zl[1]!=zl0[1] || zl[2]!=zl0[2] || zl[3]!=zl0[3] || flag==1 ){
      fprintf(fp1,"%.4lf",start);
      for (i=0;i<4;i++)   fprintf(fp1,"\t%.2lf",zl[i]);
      fprintf(fp1,"\n");
      flag++;
    }
    for (i=0;i<4;i++){
      for(j=0;j<=2*MAX_B;dst1[i][j++]=0);
      npixel[i]=0;
      zl0[i] = zl[i];
    }
    start = start0;
    modedist[0] = modedist[1] = modedist[2] = 0;
  }


/*   event treatments   */
  id = event.ccdid;
  if ( event.exp == 4 || event.exp == 8 || event.exp == 16 ){
    exposure = event.exp;
  }
  for (i=0;i<9;i++)   handle[i] = &event.phas[i];
  if ( ( grade=onormclassify_event(handle,split,&sumph,&type,&above) )==0 ||
      grade==2 || grade==3 || grade==4 ){
    if ( (value=event.phas[1])>=-MAX_B && value<=MAX_B ){
      dst1[id][value+MAX_B]++;
      npixel[id]++;
    }
    if ( (value=event.phas[3])>=-MAX_B && value<=MAX_B ){
      dst1[id][value+MAX_B]++;
      npixel[id]++;
    }
    if ( (value=event.phas[6])>=-MAX_B && value<=MAX_B ){
      dst1[id][value+MAX_B]++;
      npixel[id]++;
    }
    if ( (value=event.phas[8])>=-MAX_B && value<=MAX_B ){
      dst1[id][value+MAX_B]++;
      npixel[id]++;
    }
  }
  if ( event.time > start0 ){
    if      ( event.exp ==  4 )   modedist[0]++;
    else if ( event.exp ==  8 )   modedist[1]++;
    else if ( event.exp == 16 )   modedist[2]++;
    else {
      modedist[0]++;
      fprintf(stderr,"animaldfe5.41 (Warning): Invalid exposure (%d) is detected (%lf).\n",event.exp,event.time);
    }
  }
  start0 = event.time;

  return 0;
}



/******************************************/
/*   anUserTerminate: Final processings   */
/******************************************/
int anUserTerminate( sensor)
int sensor;
{
  int i, j, sum, ccdmode=0;


  if ( npixel[0] >= COUNT_MIN || npixel[1] >= COUNT_MIN ||
      npixel[2] >= COUNT_MIN || npixel[3] >= COUNT_MIN ){
    if ( modedist[0]>=modedist[1] && modedist[0]>=modedist[2] )
      ccdmode = 0;
    else if ( modedist[1]>=modedist[2] )
      ccdmode = 1;
    else
      ccdmode = 2;
/*
    fprintf(stderr,"modedist = (%d,%d,%d), ccdmode=%d\n",
	    modedist[0],modedist[1],modedist[2],ccdmode);
*/
  }
  for (i=0;i<4;i++){
    if ( npixel[i] > COUNT_MIN ){
      ccr(dst0[ccdmode],dst1[i],&zl[i]);
      switch ( ZeroLevelFlag ){
      case 1:
	zl[i] += ( q0[sensor][ccdmode] +
		  ffff[sensor][ccdmode]*Q[sensor][ccdmode] );
	break;
      case 2:
	zl[i] += brtzero[sensor][ccdmode];
	break;
      case 0: default:
	break;
      }
    }
    else {
      zl[i] = zl0[i];
    }
  }
  if ( zl[0]!=zl0[0] || zl[1]!=zl0[1] || zl[2]!=zl0[2] || zl[3]!=zl0[3] ||
      flag==1 ){
    fprintf(fp1,"%.4lf",start);
    for (i=0;i<4;i++)   fprintf(fp1,"\t%.2lf",zl[i]);
    fprintf(fp1,"\n");
  }

/* Write zerodef value to the output dfe file) */
   fprintf(fp1,"! ZERODEF=%d\n",zerodef);

  if ( fp1 != stdout )   fclose(fp1);

  return 0;
}

