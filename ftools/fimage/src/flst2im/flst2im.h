/* flst2im constants */

#define FLEN_BUFFER   257

void flst2im();


void flst2imgp(char *infile, char *outfile,
               char *xcol, char *ycol, char *value, int *xrangepresent,
               int *yrangepresent, int *numranges, float *xrange1,
               float *xrange2, float *yrange1, float *yrange2,
               int *rowpresent, int *FirstRow, int *LastRow,
               int *xbinpresent, int *ybinpresent,
               int *nxbin, int *nybin, int *bitpix, int *clobber, int *status);

void flst2imdo(char *infile, char *outfile,
               char *xcol, char *ycol, char *value, int *xrangepresent,
               int *yrangepresent, int *numranges, float *xrange1,
               float *xrange2, float *yrange1, float *yrange2,
               int *rowpresent, int *FirstRow, int *LastRow,
               int *xbinpresent, int *ybinpresent,
               int *nxbin, int *nybin, int bitpix, int clobber, int *status);

