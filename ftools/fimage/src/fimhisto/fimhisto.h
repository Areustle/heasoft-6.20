/* fimhisto constants */

#define FLEN_BUFFER   257
#define BytesAtaTime  100000

/* fimhisto constants */

void fimhisto();

void fimhistoGetPar(char *infile, char *outfile, char *version, 
		    int *lopres, int *uppres, float *xmin, float *xmax, 
                    float *binsize, int *nbins, int *clobber, int *status);

void fimhistoDo(char *infile, char *outfile, char *version, 
		int lopres, int uppres, float *xmin, float *xmax, 
                float *binsize, int *nbins, int clobber, int *status);

