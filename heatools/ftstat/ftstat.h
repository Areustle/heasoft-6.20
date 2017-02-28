typedef struct 
{
char name[70];
char unit[70];
int dtype;
long repeat;
long width;
long total;
long good;
long null;
long clipped;
double sum;
double sumsq;
double mean;
double median;
double min;
double max;
double std;
double *dvalue;
char *nulflag; 
char converge;
double cx;
double cy;
double dx;
double dy;
long xmin;
long ymin;
long xmax;
long ymax;
int wcflg;
double rcx;
double rcy;
double rdx;
double rdy;
double rxmin;
double rymin;
double rxmax;
double rymax;
int pass;
double mode;
int modes;
int mode_depth;
} Column;

typedef struct 
{
char extname[70];
int hdutype;
int ncols;
long nrows;
int naxis;
long naxes[4];
Column* column;
} hduInfo;
