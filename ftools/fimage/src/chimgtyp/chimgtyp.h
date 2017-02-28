/* chimgtyp constants */

#define FLEN_BUFFER   257
#define BytesAtaTime 100000

/* chimgtyp constants */

void chimgtyp();

void chimgtypGetPar(char *infile, char *outfile, char *datatype, 
		    float *Fnull, int *Inull, int *copyall, 
		    int *clobber, int *status);
void chimgtypDo(char *infile, char *outfile, char *datatype, 
		float *Fnull, int *Inull, int *copyall, 
		int *clobber, int *status);

void chimgtypNewFile(fitsfile **infp1, char *filename, int simple,
		     int bitpix, int naxis, long *naxes, long pcount,
		     long gcount, int extend, int *status);

