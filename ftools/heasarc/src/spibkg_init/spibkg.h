/*
 *    variable definitions & parameter for spi background 
 *    preprocessor calculation
 */

#define       N_max_chans       2048     /* maximum number of detector channels   */
#define       N_max_spec        500000   /* maximum number of spectra to consider */
#define       N_templates       4
#define       arf_frac_thresh   0.15     /* maximum arf value (in terms of rank order) 
					    to use in forming background template */
#define       epsilon           0.000001 /* used to avoid div-by-zero's           */


void writebintable( );
void copyhdu( );
void selectrows( );
void readheader();
void readimage();
void readtable();
void printerror( int status);

void get_arf_dat(char *, float[], int *, int *, int); 
void sum_arfs(int, int, float[], float[]);
void find_min_idx(float *, float *, int, float[], int[]);
void get_shad_dets( );

void get_spec_dat( char *, float[], float[], int *, int *);
int  get_bkg_params(char *, char *, char *, char *, char *, char *, char *, 
		    char*, int, int *, float[], int *, char *);
void get_spiback_dat( char *, float[], float[], float[]);

void wrt_bkg_model (char *, float[], float[], float[], int[], int *, int *, int);
void wrt_main_hdr ( char *, char *, char *, char*, char *, char *, char *,
		    char *, char *, char *, char *);
