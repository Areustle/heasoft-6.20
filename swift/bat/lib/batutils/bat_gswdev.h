/* Include file for all BAT GSW Development
   HAK 10-Sep-2002 */
/* Modified HAK 16-Sep-2002  Added structure for energy gain conversion */
/* Modified CM 02 Jan 2003   Protected expressions with ()'s; docs;
                             copied into batmaskutils library */
/* Modified CM 15 Jan 2003   Add prototype for batidconvert(); */
/* Modified CM 16 Jan 2003   Correct error in prototype */
/* Modified HAK 20 Aug 2003  Added prototype for svd.  Cleaned up a bit and 
                             added comments.  Copied into batutils library */

/* General definitions */
#define SANDWICH_ROWS 8   /* Rows per sandwich */
#define SANDWICH_COLS 16  /* Cols per sandwich */
#define GAP_ROWS   3   /* Inter-sandwich gap in rows */
#define GAP_COLS   2   /* Inter-sandwich gap in columns */
#define DAP_ROWS   173 /* Rows in full array */
#define DAP_COLS   286 /* Columns in full array */
#define DAP_CELLS (DAP_ROWS*DAP_COLS)
#define DAP_PIXELS (DAP_CELLS)

#define NUM_BLOCKS      16   /* Number of blocks in DAP array */
#define NUM_SANDWICHES  16   /* Number of sandwiches in block */
#define NUM_DETECTORS   128  /* Number of detectors in sandwich */
#define NUM_ELEMENTS    (NUM_BLOCKS*NUM_SANDWICHES*NUM_DETECTORS)
#define NUM_PERBLOCK    (NUM_SANDWICHES*NUM_DETECTORS)

/* #define ONE             1 */
/* #define TOP_CORNER      120  */
/* Lower left corner detector of Sandwich for Blocks 0-7 */
/* #define BOTTOM_CORNER   0 */
/* Lower left corner detector of Sandwich for Blocks 8-15 */
/* #define MAX_EBINS       80 */

/* Methods used to perform gain/offset correction */
#define INDEF_METH    0    /* User-requested default method */
#define LIN_METH      1    /* Linear correction, same as flight method */
#define QUAD_METH     2    /* Quadratic residual method */
#define CUBIC_METH    3    /* Cubic residual method */
#define FIXEDDAC_METH 4    /* Cubic residual method with fixed DAC intercept points */
#define DIRECT_METH   5    /* ??? */

/* Structure definitions */

typedef struct {

  /* Pulser to PHA residuals, either based on quadratic or cubic fit */
  /* From swbquadres* calibration file, columns GAIN, GAIN2, GAIN3, OFFSET */
  /* These values are used differently depending on the */
  /* Old names resid_gain1, resid_gain2, resid_cubic, resid_offset */
   float gpulresid1[DAP_CELLS]; /* Column GAIN  - Linear gain (Volt/ADU) */
   float gpulresid2[DAP_CELLS]; /* Column GAIN2 - Quadratic gain (Volt/ADU^2) */
   float gpulresid3[DAP_CELLS]; /* Column GAIN3 - Cubic gain (Volt/ADU^3) */
   float gpulresid0[DAP_CELLS]; /* Column OFFSET - Offset (Volt) */

   /* "Nominal" pulser to PHA linear relationship */
   /* From swbquadres* calibration file, columns LIN_GAIN and LIN_OFFSET */
   /* Old names lin_gain, lin_offset */
   float gpul_nom_gain[DAP_CELLS];   /* Column LIN_GAIN - (Volt/ADU) */
   float gpul_nom_offset[DAP_CELLS]; /* Column LIN_OFFSET - (Volt) */
  
   /* Flight derived total gain and total offset   */
   /* From flight-telemetered gain/offset map, columns GAIN and OFFSET */
   /* Old names flight_gain, flight_offset */
   float ftotgain[DAP_CELLS];    /* Column GAIN - Gain (keV/ADU) */
   float ftotoffset[DAP_CELLS];  /* Column OFFSET - Offset (ADU) */

   /* Flight-used pulser-to-energy conversion factors, "gain" and "offset" */
   /* From swbpulseflt* calibration file, columns GAIN and OFFSET */
   /* Energy [keV] = fpulseTokeV * (Volts - fpulse0keV) */
   /* Old names flp2e_gain, flp2e_offset */
   float fpulseTokeV[DAP_CELLS]; /* Column GAIN - energy scale (keV/volt) */
   float fpulse0keV[DAP_CELLS];  /* Column OFFSET - energy offset (keV) */

   /* Ground-derived pulser-to-energy conversion factors, "gain" and "offset" */
   /* Same conversion formula as above */
   /* From swbpulsecal* calibration file, columns GAIN and OFFSET */
   /* Old names p2e_gain, p2e_offset */
   float gpulseTokeV[DAP_CELLS]; /* Column GAIN - energy scale (keV/volt) */
   float gpulse0keV[DAP_CELLS];  /* Column OFFSET - energy offset (keV) */
   
   /* Value of DAC in lower of the 2-point "offset" calibration used
      to produce the flight_gain/flight_offset map onboard */
   /* From swbquadres* file, keyword name OFFPULSE */
   int DAClow;    /* Keyword OFFPULSE - units of (DACU) */

   /* Flag for good gain values for a particular detector */
   /*    0 = good */
   /* Old name flag */
   int quality_flag[DAP_CELLS];                

  /* Geographic position look-up index list.  Given detector ID d, 
       array[pos_lookup[d]] gives the value of array[] for that detector,
     where array[] is any of the arrays listed above */
   int pos_lookup[NUM_ELEMENTS];

   /* Flight assigned gain / offset index number for accounting purposes */
   /* From the flight-telemetered gain/offset map */
   int gainindex;         /* Number of flight gain calibration */
   int offsetindex;       /* Number of flight offset calibration */

   /* Method used for energy calculation, see *_METH defines above,
      for possible values. */
   int gain_meth;

   int flight_nulval;   /* Null value used in flight_gain/offset[]. */
} bat_ecal_data;



/* Function prototypes */

void batidconvert(int bufsize, unsigned short int *detid, short int *block,
		  short int *dm, short int *det,
		  short int *row, short int *col);

int svd(double *, int, int, double *, double *, double *, 
        double, int, double *);

/* ------------------------------------------------------ */
/* From batcaldb2.c */

/* Structure which defines CALDB-applicable values that can be found
   in FITS headers */
struct caldbparms_struct {
  char telescop[80];   /* TELESCOP keyword - name of mission */
  char instrume[80];   /* INSTRUME keyword - name of instrument */
  char detnam[80];     /* DETNAM keyword - name of detector within instrument */
  char filter[80];     /* FILTER keyword - name of filter in instrument */
  char strtdate[80];   /* Start date - YYYY-MM-DD */
  char strttime[80];   /* Start time - hh:mm:ss.s */
  char stopdate[80];   /* Stop  date - YYYY-MM-DD */
  char stoptime[80];   /* Stop  time - hh:mm:ss.s */
};

#ifdef CFITSIO_VERSION
/* Protect against fitsio.h not included */
extern int batkw_to_caldb_parms(fitsfile *fitsptr, 
				struct caldbparms_struct *caldb,
				int init, int *systatus);
#endif

extern int bat_caldb_search(struct caldbparms_struct *caldb, char *codenam, 
			    char *expr, int maxret, int filenamesize, 
			    char **filenam,
			    long *extno, char **online, 
			    int *nret, int *nfound, int *status);


/* ------------------------------------------------------ */
/* from ebins.c */
extern int parse_frange(char *crange, float *fmin, float *fmax);
extern int parse_franges(char *crange, float *fmin, float *fmax, 
			 int nranges, float defmin, float defmax);
extern int read_ebins(char *ebinlist, float *fmin, float *fmax, int nmaxbins,
		      char *coltype);

#ifdef CFITSIO_VERSION
/* Protect against fitsio.h not included */
extern int read_ebounds(fitsfile *ebinfile, long int *nbins, int nmaxbins,
			float *fmin, float *fmax, int *status);
#endif

/* ------------------------------------------------------ */

int bat_getcaldbfn(const char *,const char *,
    struct caldbparms_struct *caldbtime, char *,long *,int *,int *);


/* From bat_read_cal_coeff.c */
extern int bat_read_cal_coeff(double time,  
			      char *gofile, 
			      char *resfile, int resext,
			      char *pulfile, int pulext,
			      char *fltpulfile, int fltpulext,
			      int *calmode,
			      bat_ecal_data *caldata);

extern int bat_caldata_adjust(bat_ecal_data *caldata, int client);


/* From econv.c */
extern int bat_pha_to_energy(float *pha, float *energy, int n,    
		      int method,
		      float ftotgain,    float ftotoffset, 
		      float fpulseTokeV, float fpulse0keV,
		      float gpulseTokeV, float gpulse0keV,
		      float DAClow,
		      float gpulresid0, float gpulresid1,
		      float gpulresid2, float gpulresid3,
		      float gpul_nom_offset, float gpul_nom_gain, 
		      int which_scale,
		      int *status);

extern int bat_flight_energy_to_pha(float *flight_energy, float *pha,
			     int n,
			     float ftotgain, float ftotoffset,
			     int quantize);
