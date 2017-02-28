/* correctrdd constants */

#define FLEN_BUFFER   257
 
#define RDDX 426
#define RDDY 423

void correctrdd();

void correctrdd_par(char *infile, char *outfile, int *clobber, int *status);

void correctrdd_prob(float prob[][9]);

void correctrdd_do(char *infile, char *outfile, char *version, float prob[][9],
                   int *clobber,int *status);

void expand(int *, int);

void reduce(int *, int);

