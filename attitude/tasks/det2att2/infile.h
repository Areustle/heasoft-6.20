#include <stdio.h>

#include "fitsio.h"

typedef struct  {

fitsfile* fp;
int col_x;
int col_y;
int col_time;
int col_cos_angle;
int col_sin_angle;
  int use_angle;

long nrows;
long row;


} INFILE;

/****************************************************************************
*
****************************************************************************/
INFILE* openInfile(char* name);

/***************************************************************************
*
***************************************************************************/
int readInfileValues(INFILE* infile,
                     double* time, double* detx, double* dety,
		     double* sin_angle, double* cos_angle);
                     
                     
                     
/***************************************************************************
*
***************************************************************************/
void closeInfile(INFILE* infile);

