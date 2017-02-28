/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/ocfitsis5_41.h,v 3.7 2002/01/30 22:08:03 irby Exp $   */
/*                   */
/* header file for cfitsis  by K.Mitsuda */
/*  Modified to suit animaldfe (Srilal)*/

/* defines and global variables for cfitsis libray*/

#define FILENMLEN 250
#define DATATYPELEN 16
#define CFSTRREC  80
#define MAXCOL 1  /* maximum num of table columns */
#define MAXELM 64   /* maximum num of elm of a table column */
/* 
#define MAXCOL 512 
*/ 
typedef enum {FAINT, BRIGHT, FAST, NONSIS} SISMODE;
typedef enum {HIGH, MED, LOW, NORATE} BITRATE;

typedef struct{
  double time;
  short int sensor;
  short int ccdid;
  short int rawx;
  short int rawy;
  short int detx;
  short int dety;
  short int x;
  short int y;
  short int phas[9];
  short int pha;
  short int exp;
  short int pi;
  short int grade;
} SISEVENT;

typedef struct {
  int colnum;
  int  repeat;
  char type[DATATYPELEN];
} COLTYPE;

/*global variables*/

extern char datafile[FILENMLEN]; /* fits file name */
extern SISMODE datamode;
extern BITRATE bitrate;
extern int sensorid;
extern double timedel;
extern SISEVENT event;
struct column_s {
  COLTYPE time;
  COLTYPE ccdid;
  COLTYPE rawx;
  COLTYPE rawy;
  COLTYPE detx;
  COLTYPE dety;
  COLTYPE x;
  COLTYPE y;
  COLTYPE phas;    /*faint mode only*/
  COLTYPE pha;    /*bright&fast mode only*/
  COLTYPE pi;     /*this is not supported yet*/
  COLTYPE grade;  /*bright&fast mode only*/
  COLTYPE colinf;  
};
extern struct column_s column;


/* function prototypes */
/* cfitsis.c */
void cfFillStr0();

void cfTermStr();

int cfGetSysParms();

int cfOpenFits();

int cfCloseFits();

void cfGetColumn();

int cfGetSisEvent();

int cfSisEventProc();

/* userfunc.c */
int anGetUserParms();

int anUserProcEvent();

int anUserTerminate();
