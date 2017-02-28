/* ----------------------------------------------------------------- */
/* Constants used in this program */ 
#define UNIFORM  2  /* Uniform time bins */
#define USERBINS 0  /* User-defined time bins via GTI */
#define CONSTSNR 3  /* Constant signal to noise */
#define BAYESIAN 1  /* Bayesian (not used) */
#define INFBINS  4  /* Use time bins from infile */
#define MATCHBINS 5 /* Use time bins from light curve */
#define MINSNR   6  /* Minimum signal to noise */

#define T_INDEF     -1e307
#define NULL_COUNTS -1e307

/* XXX Error in XSPEC doesn't allow us to set the errors to zero */
#define XSPEC_ERROR_FUDGE_FACTOR 0.0001


/* File data types */
#define LC  1      /* Light curve */
#define PHA 2      /* Pulse height spectra - Automatic */
#define PHA1 3     /* Pulse height spectra - Type I */
#define PHA2 4     /* Pulse height spectra - Type II */
#define DPI 5      /* Detector image */
#define DPITAB 6   /* Table of detector images */
#define DPH 7      /* Table of detector histograms - multi-slice */
#define EVENTS 10  /* INPUT: EVENTS */

#define type_is_image(type) (((type)==DPI)||((type)==DPITAB)||((type)==DPH))
#define type_is_spect(type) (((type)==PHA)||((type)==PHA1)||((type)==PHA2))

/* Maximum number of *APP correction keywords to read */
#define MAX_APP_KEYS 32

/* ----------------------------------------------------------------- */
/* Definition of structures */
struct parm_struct  /* Tool parameters */
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];   /* Input file string, or "@" file name */
  int  ninfiles;               /* Number of input files */
  char **infiles;              /* Input files */
  char outfile[PIL_PATH_MAX];  /* Output file name */
  char gtifile[PIL_PATH_MAX];  /* Good time interval file name */
  char detmask[PIL_PATH_MAX];  /* Detector quality mask file name */
  char ebins[PIL_PATH_MAX];    /* Energy binning string or file name */
  char coltype[PIL_PATH_MAX];  /* Column name with energy */
  char maskwt[PIL_PATH_MAX];   /* Mask weight map (if not in event data) */
  char wtstr[PIL_LINESIZE];    /* Is the data to be weighted or not? (str) */
  char unstr[PIL_LINESIZE];    /* Output units string */
  int weighted;                /* Is the data to be weighted or not? (int) */
  double snr;                  /* Signal to noise threshold */
  int outtype;                 /* Output type, LC PHA DPH DPI DPITAB */
  int intype;                  /* Input type, EVENTS */
  int tbinmethod;              /* Time binning algorithm */
  int buffersize;              /* Input event buffer size */
  double tstart, tstop;        /* Requested start and stop time */
  double tmin, tmax;           /* Actual start and stop time */
  double *ftstart, *ftstop;    /* File TSTART/TSTOP keywords */
  struct gti_struct *gtis;     /* Array of GTIs, one for each file */
  struct gti_struct gtimaster; /* Master GTI, all input files merged */
  struct gti_struct ugti;      /* User requested GTI */
  double ftmin, ftmax;         /* File min/max event times */
  double tbinsize;             /* Requested time bin size (0=adaptive) */
  double minfracexp;           /* Minimum fractional time bin exposure */
  int nimgx, nimgy;            /* Image X and Y size */
  char timecolumn[PIL_LINESIZE];/* Name of TIME column */
  char dphcolumn[PIL_LINESIZE]; /* Name of DPH column */
  char detxcolumn[PIL_LINESIZE];/* Name of DETX column */
  char detycolumn[PIL_LINESIZE];/* Name of DETY column */
  char maskwtcolumn[PIL_LINESIZE];/* Name of MASK_WEIGHT column */
  int rate;                    /* Output rate?  1=yes 0=no */
  char *dataext;               /* Name of data extension: EVENTS or BAT_DPH */

  long int ndphbins;           /* Number of energy bins in DPH */
  float dphemin[1024];         /* Energy bins of DPH: lower edge */
  float dphemax[1024];         /* Energy bins of DPH: upper edge */
  int ndphaxes;                /* Number of spatial dimensions in DPH */
  long int dphaxes[3];         /* Spatial dimensions of DPH */
  
  float ebinquant;             /* [keV] Energy bin quantization */
  float default_ebinquant;     /* [keV] Default energy bin quantization */

  int appkeyvals[MAX_APP_KEYS]; /* *APP keyword values ... */
  char appkeynames[MAX_APP_KEYS][FLEN_CARD];  /* ... and names ... */
  char appkeycomms[MAX_APP_KEYS][FLEN_CARD];  /* ... and comments */
  int nappkeys;                 /* Number of *APP keywords in input */
  int delzeroes;               /* Delete time bins containing zero? 1=Y 0=N */
  char hduclas2[FLEN_CARD];    /* HDUCLAS2 value */
  double minfracoverlap;       /* Minimum DPH fractional time overlap */
  double mintimeoverlap;       /* [sec] Minimum DPH time overlap */
  double maxtimenonoverlap;    /* [sec] Maximum DPH non-overlap time */
  double timepixr;             /* Time bin reference point 0=begin 0.5=cent -1=auto */
};

/*
  int nebins;          number of energy bins 
  int ntbins;          number of time bins 
  double *exposures;   [ntbins] Exposure of each time bin 
  double tstart;       Start time of data in file 
  double tstop;        Stop time of data in file 
  double *times;       [ntbins] Start times of each time bin 
  double *tends;       [ntbins] Stop times of each time bin 
  double *weights;     [nebins*ntbins] Rate in each energy & time bin 
  double *weights2;    [nebins*ntbins] Error in rate of weights[] 
  double *counts;      [ntbins] Total counts per time bin (unweighted) 
*/

struct spectrum_struct {    /* Binned spectrum */
  double tstart, tstop, tbinsize;      /* Start/stop time, time binsize */
  int nebins, nsplits;                 /* Number of energy/other bins */
  int nbins;                           /* Total number of bins per time */
  int ntbins, ntmax;                   /* Number of used/allocated time bins */
  double totcounts, badcounts;          /* Total in/out-of-band counts */ 
  double snr, snr2;                    /* S/N ratio and (S/N)^2 */
  double *times, *tends, *exposures;   /* Time start/stop/exposure arrays */
  double *weights, *weights2;          /* sum(weights) and sum(weights^2) */
                                       /* ---> rates and uncertainty */
  long int *counts;                    /* Counts per time bin */
  double *totweights, *totweights2;    /* Total weights per time bin */
  int lastseg;                         /* Last GTI segment encountered */
  double lasttime;                     /* Last time value encountered */
  int accum_wt2;               /* Flag: accumulate weight2 array? 1=YES; 0=NO */
};


struct evtfile_struct {    /* Information about input events file */
  char *infile;                /* Input file name */      
  fitsfile *inevt;             /* Input file CFITSIO pointer */
  long int nrows, ndone;       /* Number of rows, and rows finished */
  int timecol, picol, wtcol;   /* Column numbers: TIME, PI, MASK_WEIGHT */
  int xcol, ycol;              /* Column numbers: DETX, DETY */
  int blankweights, blankpi;   /* Some error checking */
  double timedel;              /* TIMEDEL keyword value */
  double dtcent;               /* Time value to add to TIME to center the bin */
  int *detmask;                /* Pointer to detector quality map */
  double *maskwt;              /* Pointer to mask weight map */
};

struct dphfile_struct {    /* Information about input DPH file */
  char *infile;                /* Input file name */      
  fitsfile *indph;             /* Input file CFITSIO pointer */
  long int nrows, ndone;       /* Number of rows, and rows finished */
  int timecol, dphcol;         /* Column numbers: TIME, DPH_COUNTS */
  int expocol;                 /* Column numbers; EXPOSURE */
  int tstopcol, telapsecol;    /* Column numbers; TELAPSE, TIME_STOP (-1 if not found)*/
  int *detmask;                /* Pointer to detector quality map */
  double *maskwt;              /* Pointer to mask weight map */
  float *data;                 /* Temporary data storage pointer */
  int ntotbins;                /* Number of total bins per row */
  int curbin;                  /* Current working bin number within data[] */
};


/* Binning routines in binning.c */
extern int uniform_init(struct spectrum_struct *spect, 
			struct parm_struct *parms);
extern int uniform_bin(struct spectrum_struct *spect, 
		double *tev, double *tend, int *ebin, float *weights, int n,
		int *segs, int *detx, int *dety, int *split, float *ncounts);

extern int userbin_init(struct spectrum_struct *spect, 
			struct parm_struct *parms);
extern int userbin_bin(struct spectrum_struct *spect, 
		double *tev, double *tend, int *ebin, float *weights, int n,
		int *segs, int *detx, int *dety, int *split, float *ncounts,
		struct gti_struct *ugti);

extern int constsnr_init(struct spectrum_struct *spect, 
			 struct parm_struct *parms);
extern int constsnr_bin(struct spectrum_struct *spect, 
		 double *tev, double *tend, int *ebin, float *weights, int n,
		 int *segs, int *detx, int *dety, int *split, float *ncounts,
		 int mode);


/* Spectrum manipulation routines from spect.c */
extern int free_spect(struct spectrum_struct *spect);
extern int init_spect(struct spectrum_struct *spect);
extern int expand_spect(struct spectrum_struct *spect, int newtbins);

/* File input/output routines for FITS */
extern int writelc(fitsfile *infile, fitsfile *outfile, 
		   struct parm_struct *parms, 
	    struct spectrum_struct *spect,
	    int nebins, float *emin, float *emax);
extern int writepha1(fitsfile *infile, fitsfile *outfile, 
		    struct parm_struct *parms, 
	     struct spectrum_struct *spect);
extern int writepha2(fitsfile *infile, fitsfile *outfile, 
		    struct parm_struct *parms, 
	     struct spectrum_struct *spect);
extern int writedph(fitsfile *infile, fitsfile *outfile, 
		    struct parm_struct *parms, 
	     struct spectrum_struct *spect,
	     int nebins, float *emin, float *emax);
extern int writedpi(fitsfile *infile, fitsfile *outfile, 
		    struct parm_struct *parms, 
	     struct spectrum_struct *spect,
	     int nebins, float *emin, float *emax);
extern int writeebounds(fitsfile *infile, fitsfile *outfile, 
		 struct parm_struct *parms, 
		 struct spectrum_struct *spect, 
		 int nebins, float *emin, float *emax);
extern int writegti(fitsfile *infile, fitsfile *outfile, 
	     struct parm_struct *parms);

/* events.c */
extern int events_open(struct parm_struct *parms,
		struct evtfile_struct *evtfile,
		int *status);
extern int events_read(struct evtfile_struct *evtfile,
		struct parm_struct *parms,
		int nebintab, int *ebintab,
		double *tev, float *pi, float *weights,
		int *ebin, int *segs, int *split, int *detx, int *dety,
		int *nread, int *remaining, int *status);
extern int events_close(struct evtfile_struct *evtfile,
		 struct parm_struct *parms,
		 int *status);
extern int events_mkebintab(struct parm_struct *parms,
			    int nbins, float *emin, float *emax, 
			    int nebintab, int *ebintab);
extern int events_readebins(fitsfile *infile, char *coltype,
		     float *emin, float *emax, int nbins);

/* dphread.c */
extern int dph_open(struct parm_struct *parms,
	     struct dphfile_struct *dphfile,
	     int *status);
extern int dph_read(struct dphfile_struct *dphfile,
		struct parm_struct *parms,
		int nebintab, int *ebintab,
		double *tev, double *tend, float *pi, float *weights,
		int *ebin, int *segs, int *split, 
		int *detx, int *dety, float *ncounts,
		int *nread, int *remaining, int *status);
extern int dph_close(struct dphfile_struct *dphfile,
		 struct parm_struct *parms,
		 int *status);
extern int dph_mkebintab(struct parm_struct *parms,
		  int nbins, float *emin, float *emax, 
		  int nebintab, int *ebintab);

/* readmisc.c */
int *read_detmask(char *filename, long axes[2], int goodval, 
		  char *colname, int rownum, int *status);
extern int read_data_gtibins(fitsfile *infile, struct gti_struct *gti, 
			     double **exposure, int *status);
extern double *read_maskwt(char *filename, long axes[2], 
			   struct parm_struct *parms, int *status);
extern int check_intype(fitsfile *infile, int i, struct parm_struct *parms,
		 int *status);
extern int read_tlimits(struct parm_struct *parms);
