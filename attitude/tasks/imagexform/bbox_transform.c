#include <math.h>

#include "headas.h"
#include "methods.h"
#include "param.h"
#include "att_fatal.h"
#include "param_wrappers.h"



static
void transform_point(PARAM* param, double*  to_x, double*  to_y,
                                   double from_x, double from_y) {

    /**************************************
    * do the linear part of the transform *
    **************************************/
    applyXform2dToContinuousCoords(param->trans, to_x, to_y, from_x, from_y);

	if(param->nonlinear) {
	    /********************************
	    * apply a non-linear correction *
	    ********************************/
	    double tmp_x, tmp_y;

	    applyMapXform(param->nonlinear, &tmp_x, &tmp_y, *to_x, *to_y);
        *to_x = tmp_x;
        *to_y = tmp_y;
    }
} /* end of transform_point method */



/***************************************************************************
* Calculates the bounding box which contains the input image in the
* output image pixel coordinates
***************************************************************************/
void transform_bbox(double* xmin, double* ymin,
                    double* xmax, double* ymax,
                    fitsfile* fpin, PARAM* param) {

    long dimenx, dimeny;
    double *x, *y;
    double x4[4], y4[4];

    int i;
    int npoints;
    int status=0;


    /*****************************************
    * Read the dimensions of the input image *
    *****************************************/
    fits_read_key_lng(fpin, "NAXIS1", &dimenx, NULL, &status);
    fits_read_key_lng(fpin, "NAXIS2", &dimeny, NULL, &status);
    if(status) {
        fprintf(stderr, "Error while reading NAXIS1 from %s\n", param->infile);
        fits_report_error(stderr,status);
        att_fatal(status);
    }

    /*********************************************************
    * If the transform is linear we just need the corners.
    * Otherwise we will walk over each pixel on the boundary.
    ********************************************************/
    if (param->nonlinear) {
        int p = 0;
        npoints = 2 * (dimenx + dimeny);
        x = (double*) malloc(npoints * sizeof(double));
        y = (double*) malloc(npoints * sizeof(double));

        for (i = 0; i <= dimenx; ++i) {

            x[p] = i-0.5;       y[p] = -0.5;          ++p;

            x[p] = i-0.5;       y[p] = dimeny-0.5;    ++p;
        }

        for (i = 1; i < dimeny; ++i) {

            x[p] = -0.5;          y[p] = i-0.5;    ++p;

            x[p] = dimenx-0.5;    y[p] = i-0.5;    ++p;
        }
    }
    else {
        npoints = 4;
        x = x4;
        y = y4;

        x[0] = -0.5;           y[0] = -0.5;
        x[1] = -0.5;           y[1] = dimeny - 0.5;
        x[2] = dimenx - 0.5;   y[2] = -0.5;
        x[3] = dimenx - 0.5;   y[3] = dimeny - 0.5;
    }

    for (i = 0; i < npoints; ++i) {

        double xhat, yhat;

        transform_point(param, &xhat, &yhat, x[i], y[i]);
        
        headas_chat(5, "(%g, %g) -> (%g, %g)\n", x[i], y[i], xhat, yhat);

        /* update limits */
        if (!i) {
            *xmin = *xmax = xhat;
            *ymin = *ymax = yhat;
        }
        else {
            if(xhat < *xmin) *xmin = xhat;
            if(xhat > *xmax) *xmax = xhat;
            if(yhat < *ymin) *ymin = yhat;
            if(yhat > *ymax) *ymax = yhat;
        }
    }

    if (param->nonlinear) {
        free(x);
        free(y);
    }

} /* end of transform bbox fuction */

/************************************************************************
*
************************************************************************/
void report_bbox (double xmin, double ymin, double xmax, double ymax) {

    int dimenx, dimeny;

    /************************************
    * report the bounding box to stdout *
    ************************************/
    headas_chat(1, "Bounding box %g %g %g %g\n", xmin, ymin, xmax, ymax);
    headas_chat(1, "relative to the center of the first pixel.\n");

    /*******************************************************
    * calculate the dimensions of the output image
    *******************************************************/
    dimenx = ceil(xmax - xmin);
    dimeny = ceil(ymax - ymin);

    /***************************************
    * write the bbox values to the parfile *
    ***************************************/
    write_double_param("to_offx", xmin);
    write_double_param("to_offy", ymin);
    write_int_param("to_sizex", dimenx);
    write_int_param("to_sizey", dimeny);
}

