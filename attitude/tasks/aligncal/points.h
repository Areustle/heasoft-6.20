#ifndef POINTS_INCLUDED

#include <fitsio.h>


typedef struct  {

    int npoints;
    double* x0;
    double* y0;
    double* x1;
    double* y1;
    double* wgt;

} POINTS;



/****************************************************************************
*
****************************************************************************/

POINTS* readPoints(char* filename,   char* extname,
                   char* x0_colname, char* y0_colname,
                   char* x1_colname, char* y1_colname,
                   char* wgt_colname                  );
                   
#define POINTS_INCLUDED
#endif /* POINTS_INCLUDED */
