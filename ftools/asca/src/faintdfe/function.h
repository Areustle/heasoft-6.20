/**************************************************************************
***************************************************************************
* find the zero point such that the mean q calculated between 
* DARK_LV_LO and DARK_LV_UP is zero. This mimics the onboard 
* dark frame adjustments. This is an iterative process.
* q0 is the initial guess and 
* toll is the accuracy tollerance.
* This function is needed to calculate the zerodef=2 zero level.
**************************************************************************/
double onboard_zero(double func(double q), double q0, double toll );

/*****************************************************************************
******************************************************************************
* normalize a distribution
*****************************************************************************/
void normalize_distribution(double* dist, int n);


/****************************************************************************
*****************************************************************************
* Calculate the cross correlation function between two distributions
* note that ccr must have dimension= n1+n2-1
****************************************************************************/
void cross_correlate(double* ccr, double* dist1, int n1, 
                                  double* dist2, int n2);

/****************************************************************************
*****************************************************************************
* find the peak of a distribution - this is done by first finding the
* largest array value and then finding a maximum of the quadratic
* fit to the three points surrounding this point.
* Note: if largest array value is in the first or last position in the array
* the interpolation will read outside the array. This is OK though since the
* intent is that this function will only be passed the center part of
* a larger array.
****************************************************************************/
double find_peak(double* dist, int n);

