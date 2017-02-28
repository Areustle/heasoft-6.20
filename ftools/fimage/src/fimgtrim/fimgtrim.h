/* fimgtrim constants */

#define FLEN_BUFFER   257
#define MAXRANGES   15
#define ERROR_STATUS 1

void fimgtrim();

void fimgtrimgp(char *infile, char *outfile, int *lopres, int *uppres,
                float *thresh_lo, float *thresh_up, float *const_lo,
                float *const_up, char *type, int *clobber, int *status);
void fimgtrimdo(char *infile, char *outfile, int *lopres, int *uppres,
                 float *thresh_lo, float *thresh_up, float *const_lo,
                 float *const_up, char *type, int *clobber, int *status);
 
