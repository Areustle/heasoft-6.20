/* batgse2dph.h */

/* Constants */
#define NGSEBINS 4096

/* Structure definitions */
typedef struct {
  char infile[PIL_LINESIZE];
  char outfile[PIL_LINESIZE];
  char calfile[PIL_LINESIZE];
  char residfile[PIL_LINESIZE];
  char pulserfile[PIL_LINESIZE];
  char binfile[PIL_LINESIZE];
  char calmode[10];
  int rebin;
  double deadpercount;
  int deadapp;
} batgse2dph_parms;

typedef struct {
  int channel[NGSEBINS];
  float e_min[NGSEBINS];
  float e_max[NGSEBINS];
  float e_cent[NGSEBINS];
  float hist[NGSEBINS];
  int size;
} spectrum;

/* Function Prototypes */

/* Functions found in batgse2dph.c */
int check_binedges(spectrum *spec);
int get_ebins(batgse2dph_parms *parms, spectrum *newspec);
char **read_infile(char *infile, int *file_count);
int make_gse_energy (GAIN_STRUCTURE *gain_offset, int quadmode, int detid,
    spectrum *gsespec);
int batgse2dph_getpar (batgse2dph_parms *parms);
int write_dph(fitsfile *infits, fitsfile *outfits, spectrum *newspec,
    double exposure, char *gainmeth, char *creator);
int write_keywords(fitsfile *infits, fitsfile *outfits, double exposure,
    char *gainmeth, char *creator);

/* Functions found in read_ebins.c */
int read_ebounds(fitsfile *ebinfile, long int *nbins, int nmaxbins,
    float *fmin, float *fmax, int *status);
int parse_frange(char *crange, float *fmin, float *fmax);
int read_ebins(char *ebinlist, float *e_min, float *e_max, int n_bins);
int parse_franges(char *crange, float *fmin, float *fmax,
    int nranges, float defmin, float defmax);

/* Functions found in binning.c */
int rebin_spectrum(spectrum *gsespec, spectrum *newspec);
int bin_spectrum(spectrum *gsespec, spectrum *newspec);
