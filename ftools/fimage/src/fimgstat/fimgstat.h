/* fimgtrim constants */

#define FLEN_BUFFER   257

void fimgstat();

void fimgstatgp(char *infile, char *outfile, int *lopres, int *uppres,
                double *thresh_lo, double *thresh_up,
                int *clobber, int *status);
void fimgstatdo(char *infile, char *outfile, int *lopres, int *uppres,
                 double *thresh_lo, double *thresh_up,
                 int *clobber, int *status);
   
