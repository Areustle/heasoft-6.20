#ifndef METHODS_INCLUDED



#include "fitsio.h"
#include "param.h"

/********************************************************************************
*
********************************************************************************/
void transform_by_events(fitsfile* fpin, fitsfile* fpout, PARAM* param );

/********************************************************************************
*
********************************************************************************/
void transform_by_intensity(fitsfile* fpin, fitsfile* fpout, PARAM* param);

void transform_by_center(fitsfile* fpin, fitsfile* fpout, PARAM* param);
void transform_by_area(fitsfile* fpin, fitsfile* fpout, PARAM* param);


/********************************************************************************
*
********************************************************************************/
void transform_bbox(double* xmin, double* ymin,
                    double* xmax, double* ymax,
                    fitsfile* fpin, PARAM* param);
                    
/************************************************************************
*
************************************************************************/
void report_bbox(double xmin, double ymin, double xmax, double ymax);
                    
#define METHODS_INCLUDED
#endif /* METHODS_INCLUDED */
