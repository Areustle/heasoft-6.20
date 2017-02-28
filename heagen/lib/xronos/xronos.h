/*
 *  Xronos 
 */
#ifndef XRONOS_H
#define XRONOS_H

#define XRONOS5_EMULATION 1

#include "headas.h"
#include "headas_error.h"

/*
 * Xronos-wide constants
 */

/* Double comparison */
#define EPSILON 1e-7

/* Chat levels */
#define MUTE 0
#define TERSE 1
#define NORMAL 2
#define VERBOSE 4

#define MAXLEN_ERR 256

/* Boolean data type */

typedef int bool;
#define TRUE 1
#define FALSE 0

/*
 * Types of data
 */
#define EVENT_DATA 1
#define RATE_DATA 2
#define GTI_DATA 4
/*
 * Time systems
 */
#define TIMESYS_REF 0
#define TIMESYS_MJD 1
#define TIMESYS_JD  2
#define TIMESYS_TJD 3
#define TIMESYS_NONE -1
/*
 * Time units
 */
#define TIMEUNIT_SEC 1
#define TIMEUNIT_DAY 2
#define TIMEUNIT_MILSEC 3
#define TIMEUNIT_MICSEC 4
#define TIMEUNIT_HOUR 5
#define TIMEUNIT_YEAR 6
#define TIMEUNIT_NONE -1
/*
 * Binning modes
 */
#define BINMODE_LINEAR 1
#define BINMODE_LOG 2
#define BINMODE_COUNTS 3
#define BINMODE_BAYES 4

/* Data structures */

/* Task */
typedef struct XRTask {
   char *name;       /* e.g. "lcurve" */
   char *version;    /* e.g. "1.0" */
   char *code;       /* e.g. "lc" */

   /* progtype replaced by booleans */
   bool time_type;   /* true for all but efold, efsearch */
   bool fft_type;    /* true for autocor, crosscor, powspec */
   bool search_type; /* true for efsearch */
   bool fold_type;   /* true for efold, efsearch */

   /* whether to ask for certain parameters */
   bool use_outfile;     /* true for all but listdata, lcstats */
   bool use_gapfill;     /* true for all but listdata */
   bool use_errorbars;   /* true for all but lcurve, lcstats, efsearch */
   bool use_flatexpo;    /* true for efold, efsearch */
   bool use_nintfm;      /* true for tasks where intvs can be averaged
                          * (i.e. timeskew,powspec,autocor,crosscor,efold) */
   bool use_rebin;       /* true for tasks where results can be rebinned 
                          * (i.e. timeskew,powspec,autocor) */

   bool strict_newbin;   /* newbin must be integer multiple of min
                          * true for autocor, crosscor, timeskew */
   int nbdf;             /* default newbins per interval */

} XRTask;

/* Param */

typedef struct XROptBool {  /* Used in XRFile below */
   bool set;    /* Whether option is set */
   bool value;  /* Option value */
} XROptBool;

typedef struct XROptInt {  /* Used in XRFile below */
   bool set;    /* Whether option is set */
   int value;   /* Option value */
} XROptInt;

typedef struct XROptDbl {  /* Used in XRFile below */
   bool set;     /* Whether option is set */
   double value; /* Option value */
} XROptDbl;

typedef struct XRFile {  /* Used in XRSeries below */

   int id;     /* File id (>=1) */
   char *name; /* Location of file */

   /* Rate or Event Data */
   int ext;     /* Relevant extension */
   int type;    /* Type of data in file (EVENT_DATA, RATE_DATA) */

   long nrows;  /* Number of rows */
   long frow;   /* First row */
   long lrow;   /* Last row */

   /* Descriptive keywords */
   char *object;    /* OBJECT keyword value */
   char *extname;   /* EXTNAME keyword value */
   char *rastr;     /* RA or RA_OBJ or RA_SRC or RA_NOM keyword value */
   char *decstr;    /* DEC or DEC_OBJ or DEC_SRC or DEC_NOM keyword value */
   char *telescop;  /* TELESCOP keyword value */
   char *instrume;  /* INSTRUME keyword value */
   char *detname;   /* DETNAME keyword value */
   char *filter;    /* FILTER keyword value */

   /*  Column index */
   int col_time;    /* Time column */
   int col_rate;    /* Rate column */
   int col_err;     /* Error column */
   int col_deadc;   /* Dead time column */
   int col_timedel; /* Integration time column */
   int col_fracexp; /* Fractional exposure column */
   int col_pha;     /* PHA column */

   /*  Time properties */
   int timesys;            /* TIMESYS flag */
   int timeunit_header;    /* TIMEUNIT flag of header */
   int timeunit_column;    /* TIMEUNIT flag of column */
   double dtzero;          /* TIMEZERO [day] */
   double dtint;           /* Integration time [sec] (e.g. TIMEDEL) */
   double dtoffset;        /* Used to convert to TJD [day] */
   double dtsta;           /* Start time [day] */
   double dtsto;           /* Stop time [day] */
   double dfoffset;        /* Time centering offset 0.5-TIMEPIXR */

   /* Corrections */
   bool clockapp;   /* Whether clock correction is applied */
   bool vignapp;    /* Whether vignetting correction is applied */
   double vignet;   /* Value of vignetting correction */
   bool deadapp;    /* Whether deadtime correction is applied */
   double deadc;    /* Value of deadtime correction */
   double deaderr;  /* Value of deadtime correction error */
   bool backapp;    /* Whether background correction is applied */
   double backv;    /* Value of background correction */

   /* GTI Data */
   int gtiext;      /* GTI extension (0=not found) */
   char *gtiname;   /* GTI extension name */
   long ngtis;      /* Number of GTIs */
   int col_gtista;  /* GTI start column */
   int col_gtisto;  /* GTI stop  column */
   int gtiunit;     /* Unit of GTI pairs */
   double gtizero;  /* GTI TIMEZERO [day] */
   double gtista;   /* Overall GTI start time [day] */
   double gtisto;   /* Overall GTI stop time [day] */

   /* Options */
   XROptBool BT; /* BT  - Apply Barycentric Time correction (rate buff) */
   XROptInt  FE; /* FEn - Start loading counts at energy channel n in 
                  *       the FITS y-vector */
   XROptInt  FR; /* FRn - Start reading infile from record n (First Rec)(rb)
                  *                            (from row n for FITS) */
   XROptInt  LR; /* LRn - Stop reading infile at record n (Last Rec)(rb)
                  *                            (at row n for FITS) */
   XROptBool DN; /* DN  - If true, do NOT do following: 
                  *       (ME rate buff) apply Dead Time correction to counts 
                  *       and variance (to variance only if it is already 
                  *       applied to the counts) (for ME only) (this if all 
                  *       the relevant info is in the header otherwise see 'DV')
                  *       (option 'MA' can be used to apply a dead time
                  *       correction factor to the counts) */
   XROptInt  LE; /* LEn - Stop loading counts at energy channel n in
                  *       the FITS y-vector. */
   XROptInt  VX; /* VXn - Use vector n as X-axis (time) (for qdp or FITS)
                  *       (a neg. value means stepped X-axis according to
                  *       file header line "!!H") */
   XROptInt  VY; /* VYn - Use vector n as Y-axis (for qdp and FITS infiles) */
   XROptInt  VC; /* VCn - Use column n for Energy Channel in FITS infiles */
   XROptInt  VE; /* VEn - use vector n as Exposure-axis (for qdp) or as 
                  *       Dead Time (FITS). If event file, extension n
                  *       is GTI. If n=0, turn off GTIs (FITS) */
   XROptInt  VS; /* VSn - use vector n as Y-error-axis (for qdp and FITS) */
   XROptInt  RT; /* RTn - use extension n for the rate table. (where the first 
                  *       extension is n=1 and primary array is irrelevant) */
   XROptBool OF; /* OF  - set the doffset=0 no MJDREF */

   XROptDbl  MU; /* MUx - multiply data and errors by x (MUltiply) */
   XROptDbl  MD; /* MDx - multiply data by x (Multiply Data) */
   XROptDbl  ME; /* MEx - multiply errors by x (Multiply Errors) */
   XROptDbl  MA; /* MAx - as MU but divide exposure by x (Multiply All)
                  *       (used to have possib. to reconstruct photon stat) */
   XROptDbl  DI; /* DIx - divide data and errors by x (DIvide) */
   XROptDbl  DD; /* DDx - divide data by x (Divide Data) */
   XROptDbl  DE; /* DEx - divide errors by x (Divide Errors) */
   XROptDbl  DA; /* DAx - as DI but multiply exposure by x (Divide All)
                  *       (used to have possib. to reconstruct photon stat) */
   XROptDbl  AA; /* AAx - add data and errors with x (Add All) */
   XROptDbl  AD; /* ADx - add data with x (Add Data) */
   XROptDbl  AE; /* AEx - add errors with x (Add Errors) */
   XROptDbl  SA; /* SAx - subtract data and errors with x (Subtract All) */
   XROptDbl  SD; /* SDx - subtract data with x (Subtract Data) */
   XROptDbl  SE; /* SEx - subtract errors with x (Subtract Errors) */
   XROptDbl  QA; /* QAx - add to data the square of data multiplied by x
                  *       and sum to errors the product of data and error
                  *       multiplied by x. This is used to multiply data
                  *       and errors by a linear interpolation constant
                  *       y'=y(a+by)=ya+byaya/aa=ya+cyaya with c=b/aa
                  *       sy'=sy(a+by)=sya+byasya/aa=sya+cyasya
                  *       (sy = error). This can be done by using options:
                  *       MUa QAc (where a and c are those used above). */
   XROptDbl  QD; /* QDx - as with QA above but for data only */
   XROptDbl  QE; /* QEx - as with QA above but for errors only */
   XROptDbl  ST; /* STx - shift times by x days (Shift Times) */
   XROptDbl  SS; /* SSx - shift times by x seconds (Shift Seconds) */
   XROptDbl  DV; /* DVx - apply Dead Time correction to variance by 
                  *       providing avg. count rate BEFORE dead time 
                  *       correction x, or AFTER dead time correction 
                  *       -x (i.e. negative) (for ME rate buff) */
} XRFile;

typedef struct XRSeries {  /* Used in XRParam below */

   int id;         /* Series id (>=1) */
   int type;       /* Type of data in series (EVENT_DATE, RATE_DATA) */
   int mxfiles;    /* Maximum number of files (allocated) */
   int nfiles;     /* Number of files (in use) */
   XRFile **files; /* Files array */

   /*  Time properties */
   double dtrange;         /* Time range [day] */
   double dtsta;           /* Start time [day] */
   double dtsto;           /* Stop time [day] */
   double dtint;           /* Largest integration time among files [sec] */

   /* Intensity Windows (original bins) */
   int mxorifwi;     /* maximum number of intensity windows (original bins) */
   int norifwi;      /* number of intensity windows (original bins) */
   double *orifwia;  /* start of intensity windows (original bins) */
   double *orifwio;  /* stop of intensity windows (original bins) */

   /* Intensity Windows (newbins) */
   int mxnewfwi;     /* maximum number of intensity windows (newbins) */
   int nnewfwi;      /* number of intensity windows (newbins) */
   double *newfwia;  /* start of intensity windows (newbins) */
   double *newfwio;  /* stop of intensity windows (newbins) */

   /* Intensity Windows (intervals) */
   int mxintfwi;     /* maximum number of intensity windows (intervals) */
   int nintfwi;      /* number of intensity windows (intervals) */
   double *intfwia;  /* start of intensity windows (intervals) */
   double *intfwio;  /* stop of intensity windows (intervals) */

   /* Exposure Window (will there ever be more than one?) */

   /* (original bins) */
   double oriewia;
   double oriewio;

   /* (newbins) */
   double newewia;
   double newewio;

   /* (intervals) */
   double intewia;
   double intewio;

} XRSeries;

typedef struct XRTimeBreak {  /* Used in XRParam below */

   double time;    /* TJD */

   /* Binning properties */
   int binmode;    /* Binning mode */
   double dtnb;    /* Duration of newbin [sec] */
   int logbase;    /* Base of logarithmic binning */

   /*  binmode == BINMODE_COUNTS */
   double cntbin;  /* Counts per bin */
   double gapintv; /* Length of absent data to consider a gap (sec) */

   /* Next element in linked list */
   struct XRTimeBreak *next;
   
} XRTimeBreak;

typedef struct XRParam {
 /* These are transparently taken care of by headas 
   int tchat;
   int lchat;
   char *logname;
   bool clobber;
  */
   int gapfill;
   bool forcestart;
   int errorbars;
   bool exposure;
   int normalization;
   bool simultaneous;
   double spwinbefore;
   double spwinafter;
   double rescale;
   double offset;
   bool fast;
   bool flatexpo;

   /* Series */
   int nser;
   int mxser;
   XRSeries **series;
   double dtrange;    /* Overall range [days] */
   double dtsta;      /* Overall start time [days] */
   double dtsto;      /* Overall stop time [days] */
   double dtint;      /* Overall largest integration time */
   
   /* Time windows */
   int mxtwin;   /* maximum number of time windows (currently allocated) */
   int ntwin;    /* number of time windows defined */
   double *twia; /* start */
   double *twio; /* stop */

   /* Phase windows */
   int mxpwin;    
   int npwin;
   double *pwia;
   double *pwio;

   /* Binning properties */
   int binmode;    /* Binning mode */
   double dtnb;    /* Duration of newbin [sec] */
   int logbase;    /* Base of logarithmic binning */
   int nbint;      /* Newbins per interval */

   /*  binmode == BINMODE_COUNTS */
   double cntbin;  /* Counts per bin */
   double gapintv; /* Length of absent data to consider a gap (sec) */

   /* Time break, binning changes at defined time */
   XRTimeBreak *timebreak;

   /*  binmode == BINMODE_BAYES */
   int evtcell;    /* Events per cell */
   int binblock;   /* Bins per Bayesian block */
   double logncp;  /* Log of number of changepoints (complexity param) */

   /*  Calculated properties */
   int nintv;      /* Number of expected intervals */
   int nintfm;     /* Number of intervals per frame */
   int nframes;    /* Max number of frames expected (excluding frame 0) */

   /* Output */
   double rebin;   /* rebinning constant (>1 linear, <1 log) */

   char *outfileroot; 
   char *outfile;

} XRParam;

/* XRFrame */

typedef struct XRSeriesData { /* Used in XRInterval below */

   long mxbint;    /* maximum bins per interval (allocated) */
   long nbint;     /* number of bins per interval */
   float *y;       /* counts/sec */
   float *sy;      /* error on counts/sec */
   float *expcor;  /* exposure correction */
   double *expo;   /* size of bin [sec] */
   double *time;   /* time of bin [day] */

   /* statistics on interval (intsta, rntsta, dntsta)
    * given descriptive names here */

   /*  Time properties */
   double dtrange;         /* Time range [day] */
   double dtsta;           /* Start time [day] */
   double dtsto;           /* Stop time [day] */
   double dtint;           /* Largest integration time among files [sec] */

   /*  Exposure Status */
   long iexpo;             /* Index of expo bin to be calculated */

} XRSeriesData;

typedef struct XRInterval { /* Used in XRFrame below */

   int id;         /* Interval id (>=1) */

   int nser;       /* Number of series */
   int mxser;      /* Maximum number of series allocated */
   XRSeriesData **series;   /* Series array */

   double dtint;   /* Binning time of infiles (longest if variable) */
   double dtsta;   /* Start time of first infile */
   double dtsto;   /* Stop time of last infile */

   int nbins;      /* Running total of number of input bins/events */
   int ngtis;      /* Running total of number of input gtis */
} XRInterval;

typedef struct XRFrame {

   int curintv;  /* Current interval */
   int nintv;    /* Total number of intervals */
   int mxintv;   /* Maximum number of intervals allocated */

   XRInterval **intvs;  /* Interval array */
   XRInterval *avgintv; /* Average interval */

   /* Frame statistics */

} XRFrame;

/* XRFITSBuffer */

typedef struct XRFITSBuffer {

   int type;          /* Type of data expected */
   int nfiles;        /* Number of files (in use) */
   XRFile **files;    /* Files array */

   /* State of buffer */
   int ifile;
   XRFile *file;
   fitsfile *fptr; 
   long irow;
   bool end;

   /* Buffer data */
   /* caching not implemented */

} XRFITSBuffer;

/* XRDataPoint */

typedef struct XRDataPoint {

   bool   valid; /* whether point is valid */
   bool   null;  /* whether point is null */
   bool   event; /* whether point is an event */

   double y;      /* cts/sec (1 for event) */
   double sy;     /* error on cts/sec (0 for event) */
   double expo;   /* Bin exposure (0 for event) */
   double expcor; /* Corrected bin exposure (0 for event) */
   double time;   /* time of bin center */

} XRDataPoint;


/* XRExpoPoint */

typedef struct XRExpoPoint {

   bool   valid;     /* Whether point is valid */

   double timesta;   /* Start time of interval */
   double duration;  /* Duration of interval */
   double fracexp;   /* Fractional exposure */

} XRExpoPoint;

/* XRBinState */

typedef struct XRBinState {

   int mode;       /* Binning mode */
   int index;      /* Bin index */
   int npnts;      /* Number of points added to bin */
   double sum;     /* Count sum */
   double err;     /* Error on counts */
   double expo;    /* Bin exposure */
   double expcor;  /* Bin corrected exposure */
   double timesta; /* Bin start time [day] */
   double timesto; /* Bin stop time [day] */

   /* Linear/log binning */
   double dtnb;    /* Duration of newbin */
   int logbase;    /* Base of logarithmic binning */

   /* Binning by counts */
   int cntbin;     /* Counts per bin */
   double gapintv; /* Length of missing data to consider gap (sec) */

   /* Time break */
   XRTimeBreak *timebreak;

} XRBinState;

/* Prototypes for libxronos */
/* util */
int xrinit(XRTask *task, int *status);
int xrdectun(char *unitstr);
int xrcnvtun(int inunit, double intime, int outunit, double *outtime,
             int *status);
int xrdaytostr(double day, int decimals, char *outstr, int *status);
char *stralloc(const char *temp_str);
char *strcatalloc (const char *str1, const char *str2);
/* par */
int xrgetparams(XRTask *task, XRParam *param, int *status);
int xrgetnser(XRTask *task, XRParam *param, int *status);
int xrgetfiles(XRTask *task, XRParam *param, int *status);
int xrparseopt(char *filestr, XRFile *file, int *status);
int xrskimfiles(XRTask *task, XRParam *param, int *status);
int xrsummfile(XRFile *file, int *status);
int xrupdtser(XRFile *file, XRSeries *series, int *status);
int xrgetwin(XRTask *task, XRParam *param, int *status);
int xrgetbinmode(XRTask *task, XRParam *param, int *status);
int xrparsebinmode(char *binmodestr, int *binmode, int *status);
int xrgettime(XRTask *task, XRParam *param, int *status);
XRTimeBreak *xrnewtimebreak(XRParam *param, double time, int *status);
int xrgettimebreak(XRTask *task, XRParam *param, int *status);
int xrgetnbin(XRTask *task, XRParam *param, int *status);
int xrgindefdbl(char *parname, double defval, double *value);
int xrgindefint(char *parname, int defval, int *value);
int xrgetfout(XRTask *task, XRParam *param, int *status);
/* fits */
int xrftgext(fitsfile *fptr, XRFile *file, int *status);
int xrftgcol(fitsfile *fptr, XRFile *file, int *status);
int xrftgtky(fitsfile *fptr, XRFile *file, int *status);
int xrftgtunit(fitsfile *fptr, int timesys, int column, 
               int *timeunit_header, int *timeunit_column, int *status);
int xrftgtgti(fitsfile *fptr, XRFile *file, int *status);
int xrftgtcor(fitsfile *fptr, XRFile *file, int *status);
int xrftgdky(fitsfile *fptr, char *keynami, char *keynamf, char *keynams,
             double *dval, int *status);
int xrftgdes(fitsfile *fptr, XRFile *file, int *status);
int xrftbufinit(XRFITSBuffer *ftbuff, int type, int nfiles, XRFile **files, 
                int *status);
int xrftbufgetpnt(XRFITSBuffer *ftbuff, void *pnt, int *status);
int xrftbufclean(XRFITSBuffer *ftbuff, int *status);
int xrftdatapnt(XRFITSBuffer *ftbuff, XRDataPoint *pnt, int *status);
int xrftexpopnt(XRFITSBuffer *ftbuff, XRExpoPoint *pnt, int *status);
int xrwroutf(XRTask *task, XRParam *param, XRFrame *frame, int *status);
/* interval */
int xrsetupfrm(XRTask *task, XRParam *param, XRFrame *frame, int *status);
int xrgetintv(XRTask *task, XRParam *param, XRFrame *frame, int *status);
int xrbininit(XRBinState *bin, XRParam *param, int *status);
int xrbinpnt(XRBinState *bin, XRDataPoint *datapnt, XRExpoPoint *expopnt,
             double *cursta, double *cursto, bool *leftover, 
             XRSeriesData *series, int *status);
int xrmergebin(XRBinState *bin, XRSeriesData *series, int *status);
int xraddnewbin(XRBinState *bin, XRSeriesData *series, int *status);
int xrallocser(int index, XRSeriesData *series, int *status);
int xrfreeser(XRSeriesData *series, int *status);
int xrupdtexpo(XRExpoPoint *pnt, XRExpoPoint *lastpnt, int *ibin,
               double *curtime, XRSeriesData *series, int *status);
#endif
